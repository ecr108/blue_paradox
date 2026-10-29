# SET UP #######################################################################

## Load packages ---------------------------------------------------------------
pacman::p_load(
  tidyverse,
  exactextractr,
  terra,
  sf
)

## Load data -------------------------------------------------------------------
rfmo <- readRDS("data/processed/kiribati_eez_points_rfmo.rds")
gfw <- readRDS("data/processed/kiribati_eez_points_gfw.rds")
pipa <- st_read("data/raw/mpa/pipa.gpkg")

# Define temporal boundaries ---------------------------------------------------
start_pre      <- as.Date("2012-09-01")  # 1 year before 2013 announcement
announcement   <- as.Date("2013-09-01")  # Announcement online defined by PIPA paper
implementation <- as.Date("2015-01-01")  # Implementation date

# PROCESSING ###################################################################


# Join only matching cells and months ------------------------------------------
grid_compare <- inner_join(
  rfmo |>
    select(lon, lat, year, month, effort_day),
  gfw |>
    rename(gfw_effort = effort_day),
  by = c("lon", "lat", "year", "month")
) |>
  rename(
    rfmo_effort = effort_day
  )

# Monthly totals using only overlapping cells ----------------------------------
monthly_compare <- grid_compare |>
  group_by(year, month) |>
  summarise(
    rfmo_effort = sum(rfmo_effort, na.rm = TRUE),
    gfw_effort  = sum(gfw_effort, na.rm = TRUE),
    n_cells = n(),
    .groups = "drop"
  ) |>
  mutate(
    date = make_date(year, month, 1)
  )

# Long format for plotting -----------------------------------------------------
plot_df <- monthly_compare |>
  pivot_longer(
    c(rfmo_effort, gfw_effort),
    names_to = "source",
    values_to = "effort"
  ) |>
  mutate(
    source = recode(
      source,
      rfmo_effort = "rfmo",
      gfw_effort  = "gfw"
    )
  )

# Plot -------------------------------------------------------------------------
ggplot(plot_df,
       aes(date, effort, color = source)) +
  geom_line(linewidth = 1.1) +
  geom_vline(
    xintercept = announcement,
    linetype = "dashed"
  ) +
  labs(
    x = NULL,
    y = "Fishing effort (days)",
    color = NULL,
    title = "Monthly fishing effort in overlapping 1 degree cells (all Kiribati)",
    subtitle = "Uses GFW data divided by 8 for daily effort"
  ) +
  theme_bw()


# Compare data only in PIPA  ###################################################

# Convert GFW cells to sf points
gfw_sf <- gfw |>
  st_as_sf(
    coords = c("lon", "lat"),
    crs = "EPSG:4326",
    remove = FALSE
  )

# Raster template
ref_rast <- rast(
  xmin = -180, xmax = 180,
  ymin = -90, ymax = 90,
  res = 1,
  crs = "EPSG:4326"
)

# Rasterize cells
gfw_rast <- rasterize(gfw_sf, ref_rast)

# Identify cells overlapping PIPA
pipa_cells <- exact_extract(
  gfw_rast,
  pipa,
  include_xy = TRUE
) |>
  bind_rows() |>
  as.data.frame() |>
  rename(
    lon = x,
    lat = y
  ) |>
  filter(
    coverage_fraction > 0
  ) |>
  select(
    lon,
    lat,
    coverage_fraction
  ) |>
  distinct()

# Crop both datasets to PIPA
rfmo_pipa <- rfmo |>
  inner_join(
    pipa_cells,
    by = c("lon", "lat")
  )

gfw_pipa <- gfw |>
  inner_join(
    pipa_cells,
    by = c("lon", "lat")
  )

# Join only matching cells and months ------------------------------------------

grid_compare_pipa <- inner_join(
  rfmo_pipa |>
    select(lon, lat, year, month, effort_day),
  gfw_pipa |>
    rename(gfw_effort = effort_day),
  by = c("lon", "lat", "year", "month")
) |>
  rename(
    rfmo_effort = effort_day
  )

# Monthly totals using only overlapping cells ----------------------------------

monthly_compare_pipa <- grid_compare_pipa |>
  group_by(year, month) |>
  summarise(
    rfmo_effort = sum(rfmo_effort, na.rm = TRUE),
    gfw_effort  = sum(gfw_effort, na.rm = TRUE),
    n_cells     = n(),
    .groups = "drop"
  ) |>
  mutate(
    date = make_date(year, month, 1)
  )

# Long format for plotting -----------------------------------------------------

plot_df_pipa <- monthly_compare_pipa |>
  pivot_longer(
    c(rfmo_effort, gfw_effort),
    names_to = "source",
    values_to = "effort"
  ) |>
  mutate(
    source = recode(
      source,
      rfmo_effort = "rfmo",
      gfw_effort  = "gfw"
    )
  )

# Plot -------------------------------------------------------------------------

ggplot(
  plot_df_pipa,
  aes(date, effort, color = source)
) +
  geom_line(linewidth = 1.1) +
  geom_vline(
    xintercept = announcement,
    linetype = "dashed"
  ) +
  labs(
    x = NULL,
    y = "Fishing effort (days)",
    color = NULL,
    title = "Monthly fishing effort in PIPA",
    subtitle = "Uses GFW data divided by 8 for daily effort"
  ) +
  theme_bw()


# Compare using n_distinct(days) ###############################################

## Load data -------------------------------------------------------------------
gfw_daily <- readRDS("data/processed/kiribati_eez_points_daily_gfw.rds")

# PROCESSING ###################################################################

# ISSUE: Does not filter for purse seine
# Compare data only in PIPA  ###################################################

# Convert GFW cells to sf points
gfw_sf <- gfw_daily |>
  st_as_sf(
    coords = c("lon", "lat"),
    crs = "EPSG:4326",
    remove = FALSE
  )

# Raster template
ref_rast <- rast(
  xmin = -180, xmax = 180,
  ymin = -90, ymax = 90,
  res = 1,
  crs = "EPSG:4326"
)

# Rasterize cells
gfw_rast <- rasterize(gfw_sf, ref_rast)

# Identify cells overlapping PIPA
pipa_cells <- exact_extract(
  gfw_rast,
  pipa,
  include_xy = TRUE
) |>
  bind_rows() |>
  as.data.frame() |>
  rename(lon = x, lat = y) |>
  filter(coverage_fraction > 0) |>
  select(lon, lat, coverage_fraction) |>
  distinct()

# Crop both datasets to PIPA
rfmo_pipa <- rfmo |>
  inner_join(
    pipa_cells,
    by = c("lon", "lat")
  )

gfw_pipa <- gfw_daily |>
  inner_join(
    pipa_cells,
    by = c("lon", "lat")
  )

# Join only matching cells and months ------------------------------------------

grid_compare_pipa <- inner_join(
  rfmo_pipa |>
    select(lon, lat, year, month, effort_day),
  gfw_pipa |>
    rename(gfw_effort = effort_day),
  by = c("lon", "lat", "year", "month")
) |>
  rename(
    rfmo_effort = effort_day
  )

# Monthly totals using only overlapping cells ----------------------------------

monthly_compare_pipa <- grid_compare_pipa |>
  group_by(year, month) |>
  summarise(
    rfmo_effort = sum(rfmo_effort, na.rm = TRUE),
    gfw_effort  = sum(gfw_effort, na.rm = TRUE),
    n_cells     = n(),
    .groups = "drop"
  ) |>
  mutate(
    date = make_date(year, month, 1)
  )

# Long format for plotting -----------------------------------------------------

plot_df_pipa <- monthly_compare_pipa |>
  pivot_longer(
    c(rfmo_effort, gfw_effort),
    names_to = "source",
    values_to = "effort"
  ) |>
  mutate(
    source = recode(
      source,
      rfmo_effort = "rfmo",
      gfw_effort  = "gfw"
    )
  )

# Plot -------------------------------------------------------------------------

ggplot(
  plot_df_pipa,
  aes(date, effort, color = source)
) +
  geom_line(linewidth = 1.1) +
  geom_vline(
    xintercept = announcement,
    linetype = "dashed"
  ) +
  labs(
    x = NULL,
    y = "Fishing effort (days)",
    color = NULL,
    title = "Monthly fishing effort in PIPA",
    subtitle = "Uses n_distinct(days) for GFW"
  ) +
  theme_bw()

# Comparing hours to effort_days ###############################################
## Load data -------------------------------------------------------------------
gfw_hours <- readRDS("data/processed/kiribati_eez_points_hours_gfw.rds")


# Join data
rfmo_gfw_hours <- inner_join(
  rfmo |>
    select(lon, lat, year, month, effort_day),
  gfw_hours,
  by = c("lon", "lat", "year", "month"))

ggplot(
  rfmo_gfw_hours,
  aes(
    x = effort_day,
    y = fishing_hours
  )
) +
  geom_point(
    alpha = 0.3
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "red"
  ) +
  labs(
    x = "RFMO effort (days)",
    y = "GFW fishing effort (hours)",
    title = "RFMO vs GFW effort in matched 1 degree cells",
    subtitle = "Each point is a cell-month observation"
  ) +
  theme_bw()
