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

pacman::p_load(
  tidyverse,
  exactextractr,
  terra,
  tidyterra,
  sf,
  mapview
)

## Load data -------------------------------------------------------------------

# Spatial data
revilla <- st_read("data/raw/mpa/revillagigedo_park.gpkg")
mexico_eez <- st_read("data/raw/eez/mexico_eez.gpkg") |>
  st_make_valid()

# Tuna data
tuna <- readRDS("data/raw/rfmo/allrfmo_month_1deg_purseseine.rds")

# PROCESSING ###################################################################

# Define temporal boundaries ---------------------------------------------------
start_pre      <- as.Date("2016-09-17")  # One year before announcement
announcement   <- as.Date("2017-10-05")  # Date of announcement
implementation <- as.Date("2017-11-27")  # Implementation date (from MPA database)
end_post       <- as.Date("2018-11-27")  # 1 year after implementation

# Create buffer ----------------------------------------------------------------
nm200 <- 200 * 1852

revilla_buffer <- revilla |>
  st_buffer(dist = nm200)

# Create control region: Mexican EEZ excluding 200 nm around Revillagigedo
control_region <- mexico_eez |>
  st_difference(revilla_buffer) |>
  st_make_valid()

# Define spatial areas ---------------------------------------------------------
treatment_area <- revilla |>
  st_area() |>
  as.numeric() / 1e6 # km2

control_area <- control_region |>
  st_area() |>
  as.numeric() / 1e6 # km2

# Create reference raster
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

# Rasterize
tuna_rast <- rasterize(tuna_sf, ref_rast)

# Identify tuna cells within Revilla -------------------------------------------

treated_cells <- exact_extract(tuna_rast, revilla, include_xy = TRUE) |>
  bind_rows() |>
  as.data.frame() |>
  rename(lon = x, lat = y) |>
  filter(coverage_fraction > 0) |>
  select(lon, lat) |>
  distinct() |>
  mutate(treatment = 1)


# Identify tuna cells within control region ------------------------------------

control_cells <- exact_extract(tuna_rast, control_region, include_xy = TRUE) |>
  bind_rows() |>
  as.data.frame() |>
  rename(lon = x, lat = y) |>
  filter(coverage_fraction > 0) |>
  select(lon, lat) |>
  distinct() |>
  mutate(treatment = 0)

# Combine treatment and control cells -----------------------------------------

cell_categories <- bind_rows(
  treated_cells,
  control_cells
)

# Join raster cell categories back to tuna data

tuna_region <- tuna |>
  inner_join(cell_categories, by = join_by(lon, lat)) |>
  mutate(
    group = ifelse(treatment == 1, "Treatment", "Control"),
    date = as.Date(paste(year, month, "01", sep = "-"))) |>
  mutate(
    region_area = case_when(
      treatment == 1 ~ treatment_area,
      treatment == 0 ~ control_area
    )
  ) |>
  filter(
    date >= start_pre,
    date <= end_post
  ) |>
  mutate(
    period = case_when(
      date < announcement ~ "pre",
      date >= announcement & date < implementation ~ "anticipation",
      date >= implementation ~ "post"
    )
  ) |>
  mutate(
    id = paste(lat, lon, sep = "_")
  ) |>
  mutate(
    cpue_day = ifelse(effort_day > 0, catch_tot / effort_day, NA),
    cpue_set = ifelse(effort_set > 0, catch_tot / effort_set, NA)
  ) |>
  arrange(treatment, date)

# EXPORT #######################################################################
saveRDS(tuna_region, "data/processed/revilla_rasterized.rds")
