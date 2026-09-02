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
# All eez within 200 nm of the mpa boundary

# Define buffer  ---------------------------------------------------------------

# 200 nm buffer in meters
buffer_200nm <- 200 * 1852

revilla_proj <- revilla |>
  st_make_valid() |>
  st_transform(6933)

revilla_buffer <- revilla_proj |>
  st_buffer(dist = buffer_200nm)

# Define control region --------------------------------------------------------

# Make Mexico EEZ valid and project it for spatial operations
mexico_eez_proj <- mexico_eez |>
  st_make_valid() |>
  st_transform(6933)

# Control region = Mexico EEZ within 200 nm of Revillagigedo

control_region_m <- mexico_eez_proj |>
  st_intersection(revilla_buffer)

# Transform control region back to tuna-cell CRS
control_region <- control_region_m |>
  st_transform(st_crs(tuna_polys))


# Define treatment cells -------------------------------------------------------

# Treatment cells
#  - intersect revilla
#  - have centroid inside revilla

tuna_polys$touches_revilla <- st_intersects(
  tuna_polys,
  revilla,
  sparse = FALSE
)[, 1]

centroids <- st_centroid(tuna_polys)

tuna_polys$centroid_inside_revilla <- st_within(
  centroids,
  revilla,
  sparse = FALSE
)[, 1]

# Define control cells rules ---------------------------------------------------

# Control cells
#  - intersect the control region buffer
#  - have their centroid inside the region

tuna_polys$touches_control <- st_intersects(
  tuna_polys,
  control_region,
  sparse = FALSE
)[, 1]

tuna_polys$centroid_inside_control <- st_within(
  centroids,
  control_region,
  sparse = FALSE
)[, 1]

# Remove cells that do not follow the above definitions ------------------------

# (keeps everything except border cells whose centroid falls outside the area they touch)
tuna_polys_rm <- tuna_polys |>
  filter(
    !(touches_revilla & !centroid_inside_revilla) &
      !(touches_control & !centroid_inside_control)
  )

# Calculate treatment and control areas
treatment_area <- revilla_proj |>
  st_area() |>
  sum() |>
  as.numeric() / 1e6 # km2

control_area <- control_region |>
  st_transform(6933) |>
  st_area() |>
  sum() |>
  as.numeric() / 1e6 # km2

# There is a duplicate cell in the Mexico EEZ ----------------------------------
mexico_eez <- st_make_valid(mexico_eez)

# Define temporal boundaries ---------------------------------------------------
start_pre      <- as.Date("2016-09-17")  # One year before announcement
announcement   <- as.Date("2017-10-05")  # Date of announcement
implementation <- as.Date("2017-11-27")  # Implementation date (from MPA database)
end_post       <- as.Date("2018-11-27")  # 1 year after implementation

# Filter -----------------------------------------------------------------------
tuna_region <- tuna_polys_rm |>
  mutate(
    in_revilla = as.integer(st_intersects(cell, revilla, sparse = FALSE)),
    in_control = as.integer(st_intersects(cell, control_region, sparse = FALSE)),

    # Treatment = inside Revilla
    # Control = inside Mexico EEZ
    treatment = case_when(
      in_revilla == 1 ~ "treatment",
      in_control == 1 ~ "control",
      TRUE ~ "other"
    ),
    date = as.Date(paste(year, month, "01", sep = "-")) # this adds 01 as day to date in set, not sure if this would be correct approach
  ) |>

  # Add column of region area
  mutate(
    region_area = case_when(
      treatment == "treatment" ~ treatment_area,
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
# Border cells here are not removed, becasue they do not chane the
# the final data
mapview(tuna_region, zcol = "treatment") +
  mapview(revilla, col.regions = "red", alpha.regions = 0.3) +
  mapview(mexico_eez, col.regions = "blue", alpha.regions = 0.2)

# EXPORT #######################################################################
saveRDS(tuna_region, "data/processed/blue_paradox_revilla.rds")
saveRDS(tuna_polys, "data/processed/unfiltered_blue_paradox_revilla.rds")
