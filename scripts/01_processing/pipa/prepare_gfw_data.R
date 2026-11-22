################################################################################
# Compare GFW and RFMO Data
################################################################################
#
#
#
################################################################################

# SET UP #######################################################################

## Load packages ---------------------------------------------------------------
library(gfwr)
library(tidyverse)
library(sf)
library(lubridate)

## Load data -------------------------------------------------------------------
k_eez <- st_read("data/raw/eez/kiribati_eez.gpkg")

# Define temporal boundaries ---------------------------------------------------
start_pre      <- as.Date("2012-09-01")  # 1 year before 2013 announcement
announcement   <- as.Date("2013-09-01")  # Announcement online defined by PIPA paper
implementation <- as.Date("2015-01-01")  # Implementation date
end_post       <- as.Date("2016-01-01")  # 1 year after implementation

# PROCESSING ###################################################################

## Obtain unique fishing vessel ID by gear type --------------------------------

# September 2012-August 2013
gfw_daily_2012_2013 <- gfw_ais_fishing_hours(
  spatial_resolution = "HIGH", # 0.01 degree
  temporal_resolution = "DAILY",
  group_by = "VESSEL_ID",
  filter_by = "geartype = 'tuna_purse_seines'",
  start_date = "2012-09-01",
  end_date = "2013-09-01",
  region_source = "USER_SHAPEFILE",
  region = k_eez
)

# September 2013-August 2014
gfw_daily_2013_2014 <- gfw_ais_fishing_hours(
  spatial_resolution = "HIGH",
  temporal_resolution = "DAILY",
  group_by = "VESSEL_ID",
  filter_by = "geartype = 'tuna_purse_seines'",
  start_date = "2013-09-01",
  end_date = "2014-09-01",
  region_source = "USER_SHAPEFILE",
  region = k_eez
)

# September-December 2014
gfw_daily_2014 <- gfw_ais_fishing_hours(
  spatial_resolution = "HIGH",
  temporal_resolution = "DAILY",
  group_by = "VESSEL_ID",
  filter_by = "geartype = 'tuna_purse_seines'",
  start_date = "2014-09-01",
  end_date = "2015-02-01",
  region_source = "USER_SHAPEFILE",
  region = k_eez
)

# Aggregate to 1x1 degree cells and prepare for RFMO comparison
gfw_kiribati_1deg_daily <- bind_rows(
  gfw_daily_2012_2013,
  gfw_daily_2013_2014,
  gfw_daily_2014
) |>
  filter(`Apparent Fishing Hours` > 0) |>
  mutate(
    date = ymd(`Time Range`),
    year = year(date),
    month = month(date),
    day = day(date),

    # 1 degree grid layout
    lon_min = floor(Lon),
    lat_min = floor(Lat),

    vessel_id = `Vessel ID`
  ) |>
  group_by(lon_min, lat_min, year, month, vessel_id) |>

  # Get distinct fishing days for vessels
  summarise(
    effort_day = n_distinct(date),
    .groups = "drop"
  ) |>

  # Sum all unique fishing days across vessels per month
  group_by(lon_min, lat_min, year, month) |>
  summarise(
    effort_day = sum(effort_day),
    .groups = "drop"
  ) |>
  mutate(
    # Create cell centers
    lon = lon_min + 0.5,
    lat = lat_min + 0.5,

    # Recreate monthly date after aggregation
    date = as.Date(paste(year, month, "01", sep = "-"))
  ) |>
  filter(
    date >= start_pre,
    date < implementation
  ) |>
  mutate(
    period = case_when(
      date < announcement ~ "pre",
      date >= announcement &
        date < implementation ~ "anticipation",
      date >= implementation ~ "post"
    ),

    id = paste(lat, lon, sep = "_")
  ) |>
  select(lon, lat, date, year, month, effort_day, period) |>
  arrange(date)


# EXPORT #######################################################################
saveRDS(gfw_kiribati_1deg_daily, "data/processed/kiribati_eez_points_daily_ps_gfw.rds")


## Obtain GFW Fishing data by hours for tuna purse seine -----------------------
# September 2012-August 2013
gfw_2012_2013 <- gfw_ais_fishing_hours(
  spatial_resolution = "HIGH", # 0.01 degree
  temporal_resolution = "MONTHLY",
  group_by = "GEARTYPE",
  start_date = "2012-09-01",
  end_date = "2013-09-01",
  region_source = "USER_SHAPEFILE",
  region = k_eez
)

# September 2013-August 2014
gfw_2013_2014 <- gfw_ais_fishing_hours(
  spatial_resolution = "HIGH",
  temporal_resolution = "MONTHLY",
  group_by = "GEARTYPE",
  start_date = "2013-09-01",
  end_date = "2014-09-01",
  region_source = "USER_SHAPEFILE",
  region = k_eez
)

# September-December 2014
gfw_2014 <- gfw_ais_fishing_hours(
  spatial_resolution = "HIGH",
  temporal_resolution = "MONTHLY",
  group_by = "GEARTYPE",
  start_date = "2014-09-01",
  end_date = "2015-02-01",
  region_source = "USER_SHAPEFILE",
  region = k_eez
)

gfw_kiribati_1deg_hours <- bind_rows(
  gfw_2012_2013,
  gfw_2013_2014,
  gfw_2014
) |>
  filter(
    geartype == "tuna_purse_seines"
  ) |>
  mutate(
    date = ym(`Time Range`),

    year = year(date),
    month = month(date),

    # 1 deg grid layout
    lon_min = floor(Lon),
    lat_min = floor(Lat)
  ) |>
  group_by(
    lon_min,
    lat_min,
    year,
    month
  ) |>
  summarise(
    fishing_hours = sum(`Apparent Fishing Hours`, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    lon = lon_min + 0.5,
    lat = lat_min + 0.5,

    date = make_date(year, month, 1)
  ) |>
  filter(
    date >= start_pre,
    date < implementation
  ) |>
  mutate(
    period = case_when(
      date < announcement ~ "pre",
      date >= announcement &
        date < implementation ~ "anticipation",
      TRUE ~ "post"
    )
  ) |>
  select(lon, lat, date, year, month, period, fishing_hours)

# EXPORT #######################################################################
saveRDS(gfw_kiribati_1deg_hours, "data/processed/kiribati_eez_points_hours_gfw.rds")

