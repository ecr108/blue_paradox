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

# PROCESSING ###################################################################

# Convert tuna data to spatial points ------------------------------------------

tuna_sf <- tuna |>
  st_as_sf(
    coords = c("lon", "lat"),
    crs = "EPSG:4326",
    remove = FALSE
  )

# Identify tuna cells entirely within PIPA ------------------------------------

treated_cells <- tuna_sf |>
  st_filter(
    pipa,
    .predicate = st_intersects
  ) |>
  st_drop_geometry() |>
  select(lon, lat) |>
  distinct() |>
  mutate(treatment = 1)

# Identify tuna cells entirely within control region --------------------------
control_area_sf <- st_union(
  st_transform(line_eez, st_crs(tuna_sf)),
  st_transform(gilbert_eez, st_crs(tuna_sf))
)


control_cells <- tuna_sf |>
  st_filter(
    control_area_sf,
    .predicate = st_intersects
  ) |>
  st_drop_geometry() |>
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
    group = ifelse(treatment == 1, "Treatment", "Control")
  ) |>
  mutate(
    date = as.Date(
      paste(year, month, "01", sep = "-")
    )
  ) |>
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

# Check it worked

tuna_region_sf <- tuna_region |>
  st_as_sf(
    coords = c("lon", "lat"),
    crs = 4326,
    remove = FALSE
  )

# Map RFMO data, PIPA, and EEZ
mapview(tuna_region_sf, zcol = "treatment") +
  mapview(pipa, col.regions = "red", alpha.regions = 0.3) +
  mapview(k_eez, col.regions = "blue", alpha.regions = 0.2)

# EXPORT #######################################################################

saveRDS(tuna_region, "data/processed/pipa_rastered.rds")
