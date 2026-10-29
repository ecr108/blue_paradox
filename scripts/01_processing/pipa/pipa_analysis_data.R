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

pacman::p_load(
  tidyverse,
  exactextractr,
  terra,
  sf,
  mapview
)

## Load data -------------------------------------------------------------------

# Spatial data
pipa <- st_read("data/raw/mpa/pipa.gpkg")
k_eez <- st_read("data/raw/eez/kiribati_eez.gpkg")
line_eez <- st_read("data/raw/eez/kiribati_eez_line.gpkg")
phoenix_eez <- st_read("data/raw/eez/kiribati_eez_phoenix.gpkg")
gilbert_eez <- st_read("data/raw/eez/kiribati_eez_gilbert.gpkg")

# Tuna data
tuna <- readRDS("data/raw/rfmo/allrfmo_month_1deg_purseseine.rds")

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

# Identify tuna cells within PIPA ----------------------------------------------

treated_cells <- exact_extract(tuna_rast, pipa, include_xy = TRUE) |>
  bind_rows() |>
  as.data.frame() |>
  rename(lon = x, lat = y) |>
  filter(coverage_fraction > 0) |>
  select(lon, lat, coverage_fraction) |>
  distinct() |>
  mutate(treatment = 1)

# Identify tuna cells within control region ------------------------------------
control_area_rast <- st_union(line_eez, gilbert_eez)

control_cells <- exact_extract(tuna_rast, control_area_rast, include_xy = TRUE) |>
  bind_rows() |>
  as.data.frame() |>
  rename(lon = x, lat = y) |>
  filter(coverage_fraction > 0) |>
  select(lon, lat, coverage_fraction) |>
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
      treatment == 1 ~ pipa_area,
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

saveRDS(tuna_region, "data/processed/pipa_rasterized.rds")
