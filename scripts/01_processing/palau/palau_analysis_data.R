################################################################################
# PNMS analysis data
################################################################################
#
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
palau <- st_read("data/raw/eez/palau_eez.gpkg")
pnms <- st_read("data/raw/mpa/pnms_2015.gpkg") # using original shape
fsm <- st_read("data/raw/eez/fsm_eez.gpkg")

# Tuna data
tuna <- readRDS("data/raw/rfmo/allrfmo_month_1deg_purseseine.rds")

# Define temporal boundaries ---------------------------------------------------

start_pre      <- as.Date("2014-10-22")  # 1 year before 2015 announcement
announcement   <- as.Date("2015-10-22")  # Announcement of Palau (per MPA dataset)
implementation <- as.Date("2020-01-01")  # Implementation of Palau
end_post       <- as.Date("2021-01-01")  # 1 year after implementation

# Define FSM control area within 200 nm of Palau -------------------------------

# Create buffer
nm200 <- 200 * 1852

# 200 nm buffer around Palau, restricted to FSM EEZ
control_region <- palau |>
  st_union() |>
  st_buffer(dist = nm200) |>
  st_intersection(st_union(fsm))


# Define spatial areas ---------------------------------------------------------
treatment_area <- pnms |>
  st_area() |>
  as.numeric() / 1e6 #km2

control_area <- control_region |>
  st_area() |>
  as.numeric() / 1e6 #km2

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

# Rasterize
tuna_rast <- rasterize(tuna_sf, ref_rast)

# Identify tuna cells within Revilla -------------------------------------------

treated_cells <- exact_extract(tuna_rast, pnms, include_xy = TRUE) |>
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

cell_categories_clean <- cell_categories |>
  arrange(desc(treatment)) |>  # prioritize cells that overlap both being kept for the treatment region
  distinct(lon, lat, .keep_all = T)

# Join raster cell categories back to tuna data

tuna_region <- tuna |>
  inner_join(cell_categories_clean, by = join_by(lon, lat)) |>
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

# Convert tuna_region to sf
tuna_region_sf <- tuna_region |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326)


# Map RFMO data, PIPA, and EEZ
mapview(tuna_region_sf, zcol = "treatment") +
  mapview(pnms, col.regions = "red", alpha.regions = 0.3) +
  mapview(fsm, col.regions = "blue", alpha.regions = 0.2) +
  mapview(palau, col.region = "yellow", alpha.regions = 0.2)


# EXPORT #######################################################################
saveRDS(tuna_region, "data/processed/palau_rasterized.rds")
