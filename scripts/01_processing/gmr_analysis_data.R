################################################################################
# GMR Blue Paradox Data
################################################################################
#
# Emily Rodriguez
# ecr108@miami.edu
#
# Data for analysis of blue paradox in GMR.
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
gmr <- st_read("data/raw/mpa/galapagos_world_heritage.gpkg")
e_main_eez <- st_read("data/raw/eez/ecuador_eez_main.gpkg")
e_gal_eez <- st_read("data/raw/eez/ecuador_eez_galapagos.gpkg")

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

# Identify cells that intersect GMR
tuna_polys$touches_gmr <- st_intersects(
  tuna_polys,
  gmr,
  sparse = FALSE
)[, 1]

# Compute centroids
centroids <- st_centroid(tuna_polys)

# Identify cells whose centroid is inside GMR
tuna_polys$centroid_inside_gmr <- st_within(
  centroids,
  gmr,
  sparse = FALSE
)[, 1]

# Remove border cells
tuna_polys_rm <- tuna_polys |>
  filter(!(touches_gmr & !centroid_inside_gmr))

# Define temporal boundaries ---------------------------------------------------
# (need to be updated correctly)

start_pre      <- as.Date("1997-01-01")  # 1 year before 1998 announcement
announcement   <- as.Date("1998-01-01")  # Announcement and implementation of GMR
implementation <- as.Date("2002-01-01")  # Implementation of enforcement date
end_post       <- as.Date("2003-01-01")  # 1 year after implementation

# Define spatial areas ---------------------------------------------------------
gmr_area <- gmr |>
  st_transform(6933) |>
  st_area() |>
  as.numeric() / 1e6 # convert m2 -> km2

control_area <- e_main_eez |>
  st_transform(6933) |>
  st_area() |>
  as.numeric() / 1e6 # convert m2 -> km2


# Filter -----------------------------------------------------------------------
tuna_region <- tuna_polys_rm |>
  mutate(
    in_gmr = as.integer(st_intersects(cell, gmr, sparse = FALSE)),
    in_eez = as.integer(st_intersects(cell, e_main_eez, sparse = FALSE)),

    # Treatment = inside PIPA
    # Control = inside Line Islands and Gilbert EEZ
    treatment = case_when(
      in_gmr == 1 ~ "treatment",
      in_eez == 1 ~ "control",
      TRUE ~ "other"
    ),
    date = as.Date(paste(year, month, "01", sep = "-")) # this adds 01 as day to date in set, not sure if this would be correct approach
  ) |>

  # Add column of region area
  mutate(
    region_area = case_when(
      treatment == "treatment" ~ gmr_area,
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
  mapview(gmr, col.regions = "red", alpha.regions = 0.3) +
  mapview(e_main_eez, col.regions = "blue", alpha.regions = 0.2)

# EXPORT #######################################################################
saveRDS(tuna_region, "data/processed/blue_paradox_gmr.rds")
saveRDS(tuna_polys, "data/processed/unfiltered_blue_paradox_gmr.rds")
