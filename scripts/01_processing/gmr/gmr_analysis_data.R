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

pacman::p_load(
  tidyverse,
  exactextractr,
  terra,
  sf,
  mapview
)
## Load data -------------------------------------------------------------------

# Spatial data
gmr <- st_read("data/raw/mpa/galapagos_world_heritage.gpkg")
e_main_eez <- st_read("data/raw/eez/ecuador_eez_main.gpkg")
e_gal_eez <- st_read("data/raw/eez/ecuador_eez_galapagos.gpkg")

# Tuna data
tuna <- readRDS("data/raw/rfmo/allrfmo_month_1deg_purseseine.rds")

# Define temporal boundaries ---------------------------------------------------

start_pre      <- as.Date("1997-01-01")  # 1 year before 1998 announcement
announcement   <- as.Date("1998-03-18")  # Announcement and implementation of GMR
implementation <- as.Date("2002-01-01")  # Implementation of enforcement date (need to find month)
end_post       <- as.Date("2003-01-01")  # 1 year after implementation

# Define spatial areas ---------------------------------------------------------
gmr_area <- gmr |>
  st_area() |>
  as.numeric() / 1e6 # convert m2 -> km2

control_area <- e_main_eez |>
  st_area() |>
  as.numeric() / 1e6 # convert m2 -> km2

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

# Identify tuna cells entirely within GMR ------------------------------------

treated_cells <- exact_extract(tuna_rast, gmr, include_xy = TRUE) |>
  bind_rows() |>
  as.data.frame() |>
  rename(lon = x, lat = y) |>
  filter(coverage_fraction > 0) |>
  select(lon, lat) |>
  distinct() |>
  mutate(treatment = 1)

# Identify tuna cells entirely within control region --------------------------

control_cells <- exact_extract(tuna_rast, e_main_eez, include_xy = TRUE) |>
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

tuna_region <- tuna |>
  inner_join(
    cell_categories,
    by = join_by(lon, lat)
  ) |>
  mutate(
    group = ifelse(treatment == 1, "Treatment", "Control"),
    date = as.Date(paste(year, month, "01", sep = "-"))) |>
  mutate(
    region_area = case_when(
      treatment == 1 ~ gmr_area,
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

# Check it worked

tuna_region_sf <- tuna_region |>
  st_as_sf(
    coords = c("lon", "lat"),
    crs = 4326,
    remove = FALSE
  )

# EXPORT #######################################################################

saveRDS(tuna_region, "data/processed/gmr_rasterized.rds")

