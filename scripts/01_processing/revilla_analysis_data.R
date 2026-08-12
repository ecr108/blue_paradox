################################################################################
# Revillagigedo Blue Paradox Data
################################################################################
#
# Emily Rodriguez
# ecr108@miami.edu
#
# Data for analysis of blue paradox in Revillagigedo
#
################################################################################

# SET UP #######################################################################

## Load packages ---------------------------------------------------------------

library(tidyverse)
library(sf)
library(lubridate)
library(mapview)

## Load data -------------------------------------------------------------------

# Spatial data
revilla <- st_read("data/raw/mpa/revillagigedo_park.gpkg")
mexico_eez <- st_read("data/raw/eez/mexico_eez.gpkg")

# Tuna data
tuna <- readRDS("data/raw/rfmo/allrfmo_month_1deg_purseseine.rds")

# PROCESSING ###################################################################

# Rasterize tuna data ----------------------------------------------------------
make_cell <- function(lon, lat) {
  st_polygon(list(rbind(
    c(lon - 0.5, lat - 0.5),
    c(lon + 0.5, lat - 0.5),
    c(lon + 0.5, lat + 0.5),
    c(lon - 0.5, lat + 0.5),
    c(lon - 0.5, lat - 0.5)
  )))
}

tuna_polys <- tuna |>
  mutate(
    cell = purrr::map2(lon, lat, make_cell)
  ) |>
  mutate(
    cell = st_sfc(cell, crs = 4326)
  ) |>
  st_as_sf(sf_column_name = "cell")

# Remove border cells ----------------------------------------------------------

# Identify cells that intersect revilla
tuna_polys$touches_revilla <- st_intersects(
  tuna_polys,
  revilla,
  sparse = FALSE
)[, 1]

# Compute centroids
centroids <- st_centroid(tuna_polys)

# Identify cells whose centroid is inside revilla
tuna_polys$centroid_inside_revilla <- st_within(
  centroids,
  revilla,
  sparse = FALSE
)[, 1]

# Remove border cells
# Rule is set to keep cells whose centroid is inside revilla
tuna_polys_rm <- tuna_polys |>
  filter(!(touches_revilla & !centroid_inside_revilla))


# Define temporal boundaries ---------------------------------------------------
start_pre      <- as.Date("2015-11-24")  # Missing
announcement   <- as.Date("2016-11-24")  # Missing
implementation <- as.Date("2017-11-24")  # Implementation date
end_post       <- as.Date("2018-11-24")  # 1 year after implementation

# There is a duplicate cell in the Mexico EEZ ----------------------------------
mexico_eez <- st_make_valid(mexico_eez)

# Define spatial areas ---------------------------------------------------------
revilla_area <- revilla |>
  st_transform(6933) |>
  st_area() |>
  as.numeric() / 1e6 # convert m2 -> km2

control_area <- mexico_eez |>
  st_transform(6933) |>
  st_area() |>
  as.numeric() / 1e6 # convert m2 -> km2

# Filter -----------------------------------------------------------------------
tuna_region <- tuna_polys_rm |>
  mutate(
    in_revilla = as.integer(st_intersects(cell, revilla, sparse = FALSE)),
    in_eez = as.integer(st_intersects(cell, mexico_eez, sparse = FALSE)),

    # Treatment = inside PIPA
    # Control = inside Line Islands and Gilbert EEZ
    treatment = case_when(
      in_revilla == 1 ~ "treatment",
      in_eez == 1 ~ "control",
      TRUE ~ "other"
    ),
    date = as.Date(paste(year, month, "01", sep = "-")) # this adds 01 as day to date in set, not sure if this would be correct approach
  ) |>

  # Add column of region area
  mutate(
    region_area = case_when(
      treatment == "treatment" ~ revilla_area,
      treatment == "control"   ~ control_area
    )
  ) |>

  # Filter for time
  filter(
    treatment %in% c("treatment", "control"),
    date >= start_pre,
    date <= end_post
  ) |>

  # Add tags to time periods
  mutate(
    period = case_when(
      date < announcement ~ "pre",
      date >= announcement & date < implementation ~ "anticipation",
      date >= implementation ~ "post"
    )
  ) |>

  # Add CPUE day and set
  mutate(
    cpue_day = ifelse(effort_day > 0, catch_tot / effort_day, NA),
    cpue_set = ifelse(effort_set > 0, catch_tot / effort_set, NA)
  ) |>
  arrange(treatment, date)

# VISUALIZE ####################################################################

# Map RFMO data, PIPA, and EEZ
mapview(tuna_region, zcol = "treatment") +
  mapview(revilla, col.regions = "red", alpha.regions = 0.3) +
  mapview(mexico_eez, col.regions = "blue", alpha.regions = 0.2)

# EXPORT #######################################################################
saveRDS(tuna_region, "data/processed/blue_paradox_revilla.rds")
saveRDS(tuna_polys, "data/processed/unfiltered_blue_paradox_revilla.rds")
