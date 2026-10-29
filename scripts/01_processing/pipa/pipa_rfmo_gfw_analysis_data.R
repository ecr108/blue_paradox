# SET UP #######################################################################

## Load packages ---------------------------------------------------------------

pacman::p_load(
  tidyverse,
  exactextractr,
  terra,
  sf,
  mapview
)

## Load data -------------------------------------------------------------------

# Spatial data
k_eez <- st_read("data/raw/eez/kiribati_eez.gpkg")

# Tuna data
tuna <- readRDS("data/raw/rfmo/allrfmo_month_1deg_purseseine.rds")

# Define temporal boundaries ---------------------------------------------------
start_pre      <- as.Date("2012-09-01")  # 1 year before 2013 announcement
announcement   <- as.Date("2013-09-01")  # Announcement online defined by PIPA paper
implementation <- as.Date("2015-01-01")  # Implementation date
end_post       <- as.Date("2016-01-01")  # 1 year after implementation

# Define spatial area ----------------------------------------------------------

kiribati_area <- st_area(k_eez) |>
  as.numeric() / 1e6  # convert m2 -> km2

# Create reference raster ------------------------------------------------------

ref_rast <- rast(
  xmin = -180, xmax = 180,
  ymin = -90, ymax = 90,
  res = 1,
  crs = "EPSG:4326"
)

# PROCESSING ###################################################################

# Convert tuna data to spatial points ------------------------------------------

tuna_sf <- tuna |>
  st_as_sf(
    coords = c("lon", "lat"),
    crs = "EPSG:4326",
    remove = FALSE
  )

# Rasterize --------------------------------------------------------------------

tuna_rast <- rasterize(tuna_sf, ref_rast)

# Identify tuna cells within Kiribati EEZ ---------------------------------------

kiribati_cells <- exact_extract(
  tuna_rast,
  k_eez,
  include_xy = TRUE
) |>
  bind_rows() |>
  as.data.frame() |>
  rename(lon = x, lat = y) |>
  filter(coverage_fraction > 0) |>
  select(lon, lat, coverage_fraction) |>
  distinct()

# Join EEZ cells back to tuna data ---------------------------------------------

tuna_region <- tuna |>
  inner_join(kiribati_cells, by = join_by(lon, lat)) |>
  mutate(
    date = as.Date(paste(year, month, "01", sep = "-"))
  ) |>
  filter(
    date >= start_pre,
    date <= implementation # changing date here
  ) |>
  mutate(
    period = case_when(
      date < announcement ~ "pre",
      date >= announcement & date < implementation ~ "anticipation",
      date >= implementation ~ "post"
    ),
    region_area = kiribati_area,
    id = paste(lat, lon, sep = "_"),
    cpue_day = ifelse(effort_day > 0, catch_tot / effort_day, NA),
    cpue_set = ifelse(effort_set > 0, catch_tot / effort_set, NA)
  ) |>
  arrange(date)

# EXPORT #######################################################################

saveRDS(tuna_region, "data/processed/kiribati_eez_points_rfmo.rds")
