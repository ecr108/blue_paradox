################################################################################
# Recreate PIPA analysis data
################################################################################
#
# Emily Rodriguez
# ecr108@miami.edu
#
# Recreation of blue paradox analysis in PIPA region using tuna RFMO catch and
# effort data at the 1 x 1 degree, monthly resolution.
#
################################################################################

# SET UP #######################################################################

## Load packages ---------------------------------------------------------------

library(tidyverse)
library(sf)
library(mapview)

## Load data -------------------------------------------------------------------

# Spatial data
pipa <- st_read("data/raw/mpa/pipa.gpkg")
k_eez <- st_read("data/raw/eez/kiribati_eez.gpkg")
line_eez <- st_read("data/raw/eez/kiribati_eez_line.gpkg")
phoenix_eez <- st_read("data/raw/eez/kiribati_eez_phoenix.gpkg")
gilbert_eez <- st_read("data/raw/eez/kiribati_eez_gilbert.gpkg")

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
start_pre      <- as.Date("2012-09-01")  # 1 year before 2013 announcement
announcement   <- as.Date("2013-09-01")  # Announcement online defined by PIPA paper
implementation <- as.Date("2015-01-01")  # Implementation date
end_post       <- as.Date("2016-01-01")  # 1 year after implementation

# Define spatial areas ---------------------------------------------------------
pipa_area <- pipa |>
  st_area() |>
  as.numeric() / 1e6 # convert m2 -> km2

control_area <- st_union(
  line_eez,
  gilbert_eez
) |>
  st_area() |>
  as.numeric() / 1e6

# Filter -----------------------------------------------------------------------
tuna_region <- tuna_polys |>
  mutate(
    in_pipa = as.integer(st_intersects(cell, pipa, sparse = FALSE)),
    in_line = as.integer(st_intersects(cell, line_eez, sparse = FALSE)),
    in_gilbert = as.integer(st_intersects(cell, gilbert_eez, sparse = FALSE)),

    # Treatment = inside PIPA
    # Control = inside Line Islands and Gilbert EEZ
    treatment = case_when(
      in_pipa == 1 ~ "treatment",
      in_line == 1 | in_gilbert == 1 ~ "control",
      TRUE ~ "other"
    ),
    date = as.Date(paste(year, month, "01", sep = "-")) # this adds 01 as day to date in set, not sure if this would be correct approach
  ) |>

  # Add column of region area
  mutate(
    region_area = case_when(
      treatment == "treatment" ~ pipa_area,
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

# EXPORT #######################################################################

saveRDS(tuna_region, "data/processed/blue_paradox_pipa.rds")
