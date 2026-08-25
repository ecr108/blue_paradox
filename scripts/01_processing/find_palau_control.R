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
fsm <- st_read("data/raw/eez/fsm_eez.gpkg")

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


# Define FSM control area within 200 nm of Palau -------------------------------

# 200 nautical miles in meters
nm200 <- 200 * 1852

# Use projection for distance calculations around Palau
# EPSG:32654 = UTM zone 54N (covers Palau/FSM)
palau_utm <- palau |>
  st_make_valid() |>
  st_transform(32654)

fsm_utm <- fsm |>
  st_make_valid() |>
  st_transform(32654)

# 200 nm buffer around Palau, restricted to FSM EEZ
palau_200nm_fsm <- palau_utm |>
  st_union() |>
  st_buffer(dist = nm200) |>
  st_intersection(st_union(fsm_utm)) |>
  st_transform(4326)


# Define temporal boundaries ---------------------------------------------------

start_pre      <- as.Date("2014-10-22")  # 1 year before 2015 announcement
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

fsm_area <- fsm |>
  st_transform(6933) |>
  st_area() |>
  sum() |>
  as.numeric() / 1e6 # m2 -> km2


# Filter -----------------------------------------------------------------------
tuna_region <- tuna_polys |>
  mutate(
    in_pnms = as.integer(st_intersects(cell, pnms, sparse = FALSE)),
    in_palau = as.integer(st_intersects(cell, palau, sparse = FALSE)),
    in_fsm_200nm = as.integer(st_intersects(cell, palau_200nm_fsm, sparse = FALSE)),

    # Treatment = inside PNMS
    # Control = inside Palau EEZ
    treatment = case_when(
      in_pnms == 1 ~ "treatment",
      in_fsm_200nm == 1 ~ "control",
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

# Map
mapview(tuna_region, zcol = "treatment") +
  mapview(pnms, col.regions = "red", alpha.regions = 0.3) +
  mapview(palau, col.regions = "green", alpha.regions = 0.3) +
  mapview(fsm, col.regions = "blue", alpha.regions = 0.2)
