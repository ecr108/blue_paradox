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
palau <- st_read("data/raw/eez/palau_eez.gpkg")
pnms <- st_read("data/raw/mpa/pnms.gpkg")

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

# Define temporal boundaries ---------------------------------------------------

start_pre      <- as.Date("1999-10-22")  # 1 year before 2015 announcement
announcement   <- as.Date("2015-10-22")  # Announcement of Palau (per MPA dataset)
implementation <- as.Date("2020-01-01")  # Implementation of Palau
end_post       <- as.Date("2021-01-01")  # 1 year after implementation

# Define spatial areas ---------------------------------------------------------
pnms_area <- pnms |>
  st_transform(6933) |>
  st_area() |>
  sum() |>
  as.numeric() / 1e6 # m2 -> km2

palau_area <- palau |>
  st_transform(6933) |>
  st_area() |>
  sum() |>
  as.numeric() / 1e6 # m2 -> km2


# Filter -----------------------------------------------------------------------
tuna_region <- tuna_polys |>
  mutate(
    in_pnms = as.integer(st_intersects(cell, pnms, sparse = FALSE)),
    in_palau = as.integer(st_intersects(cell, palau, sparse = FALSE)),

    # Treatment = inside PNMS
    # Control = inside Palau EEZ
    treatment = case_when(
      in_pnms == 1 ~ "treatment",
      in_palau == 1 ~ "control",
      TRUE ~ "other"
    ),
    date = as.Date(paste(year, month, "01", sep = "-")) # this adds 01 as day to date in set, not sure if this would be correct approach
  ) |>

  # Add column of region area
  mutate(
    region_area = case_when(
      treatment == "treatment" ~ pnms_area,
      treatment == "control"   ~ palau_area
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

# Tests ########################################################################
# Rough spatial window around Palau
palau_bbox <- st_bbox(palau) |>
  st_as_sfc() |>
  st_buffer(2)

tuna_map <- tuna_polys |>
  filter(st_intersects(cell, palau_bbox, sparse = FALSE)[, 1])

mapview(tuna_map, alpha.regions = 0.2) +
  mapview(pnms, col.regions = "blue") +
  mapview(palau, col.regions = "red")

# Without removing dates;
tuna_region2 <- tuna_polys |>
  mutate(
    in_pnms = as.integer(st_intersects(cell, pnms, sparse = FALSE)),
    in_palau = as.integer(st_intersects(cell, palau, sparse = FALSE)),

    # Treatment = inside PNMS
    # Control = inside Palau EEZ
    treatment = case_when(
      in_pnms == 1 ~ "treatment",
      in_palau == 1 ~ "control",
      TRUE ~ "other"
    ),
    date = as.Date(paste(year, month, "01", sep = "-")) # this adds 01 as day to date in set, not sure if this would be correct approach
  ) |>

  # Add column of region area
  mutate(
    region_area = case_when(
      treatment == "treatment" ~ pnms_area,
      treatment == "control"   ~ palau_area
    )
  ) |>

  # Filter for time
  filter(
    treatment %in% c("treatment", "control"))|>

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

# Map RFMO data, PIPA, and EEZ
mapview(tuna_region2, zcol = "treatment") +
  mapview(pnms, col.regions = "red", alpha.regions = 0.3) +
  mapview(palau, col.regions = "blue", alpha.regions = 0.2)

# VISUALIZE ####################################################################

# Map
mapview(tuna_region, zcol = "treatment") +
  mapview(pnms, col.regions = "red", alpha.regions = 0.3) +
  mapview(palau, col.regions = "blue", alpha.regions = 0.2)

# EXPORT #######################################################################
saveRDS(tuna_region, "data/processed/blue_paradox_palau.rds")
