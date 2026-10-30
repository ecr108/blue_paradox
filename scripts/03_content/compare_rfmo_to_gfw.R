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
gfw_daily <- readRDS("data/processed/kiribati_eez_points_daily_ps_gfw.rds")
gfw_hours <- readRDS("data/processed/kiribati_eez_points_hours_gfw.rds")
pipa <- st_read("data/raw/mpa/pipa.gpkg")

# Define temporal boundaries ---------------------------------------------------
start_pre      <- as.Date("2012-09-01")  # 1 year before 2013 announcement
announcement   <- as.Date("2013-09-01")  # Announcement online defined by PIPA paper
implementation <- as.Date("2015-01-01")  # Implementation date

# PROCESSING ###################################################################

# Plot rfmo effort days vs gfw hours ###########################################
# Identify PIPA cells ----------------------------------------------------------

gfw_sf <- gfw_hours |>
  st_as_sf(
    coords = c("lon", "lat"),
    crs = "EPSG:4326",
    remove = FALSE
  )

ref_rast <- rast(
  xmin = -180, xmax = 180,
  ymin = -90, ymax = 90,
  res = 1,
  crs = "EPSG:4326"
)

gfw_rast <- rasterize(gfw_sf, ref_rast)

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
  filter(coverage_fraction > 0) |>
  select(lon, lat) |>
  distinct()

# Crop datasets to PIPA --------------------------------------------------------

rfmo_pipa <- rfmo |>
  inner_join(
    pipa_cells,
    by = c("lon", "lat")
  )

gfw_pipa <- gfw_hours |>
  inner_join(
    pipa_cells,
    by = c("lon", "lat")
  )

# Aggregate to monthly regional totals -----------------------------------------

rfmo_monthly <- rfmo_pipa |>
  group_by(year, month) |>
  summarise(
    effort_day = sum(effort_day, na.rm = TRUE),
    .groups = "drop"
  )

gfw_monthly <- gfw_pipa |>
  group_by(year, month) |>
  summarise(
    fishing_hours = sum(fishing_hours, na.rm = TRUE),
    .groups = "drop"
  )

# Join by month and year only --------------------------------------------------

rfmo_gfw_monthly <- inner_join(
  rfmo_monthly,
  gfw_monthly,
  by = c("year", "month")
)

# Plot -------------------------------------------------------------------------
# Each point is a month of total effort within PIPA
# RFMO vs GFW effort in PIPA
days_v_hours <- ggplot(
  rfmo_gfw_monthly,
  aes(
    x = effort_day,
    y = fishing_hours
  )
) +
  geom_point(
    size = 3,
    alpha = 0.6,
    size = 3) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "#D55E00"
  ) +
  labs(
    x = "RFMO effort (days)",
    y = "GFW fishing effort (hours)",
  ) +
  theme_classic()

# Plot GFW vs RFMO data using n_distinct(days) #################################
## Identify GFW grid cells overlapping PIPA ------------------------------------

gfw_sf <- gfw_daily |>
  st_as_sf(
    coords = c("lon", "lat"),
    crs = "EPSG:4326",
    remove = FALSE
  )

ref_rast <- rast(
  xmin = -180, xmax = 180,
  ymin = -90, ymax = 90,
  res = 1,
  crs = "EPSG:4326"
)

gfw_rast <- rasterize(gfw_sf, ref_rast)

pipa_cells <- exact_extract(
  gfw_rast,
  pipa,
  include_xy = TRUE
) |>
  bind_rows() |>
  as.data.frame() |>
  rename(lon = x, lat = y) |>
  filter(coverage_fraction > 0) |>
  select(lon, lat) |>
  distinct()


## Crop both datasets to PIPA --------------------------------------------------

rfmo_pipa <- rfmo |>
  inner_join(pipa_cells, by = c("lon", "lat"))

gfw_pipa <- gfw_daily |>
  inner_join(pipa_cells, by = c("lon", "lat"))


## Aggregate each dataset to monthly regional totals ---------------------------

rfmo_monthly <- rfmo_pipa |>
  group_by(year, month) |>
  summarise(
    rfmo_effort = sum(effort_day, na.rm = TRUE),
    .groups = "drop"
  )

gfw_monthly <- gfw_pipa |>
  group_by(year, month) |>
  summarise(
    gfw_effort = sum(effort_day, na.rm = TRUE),
    .groups = "drop"
  )


## Join by month and year only -------------------------------------------------

monthly_compare_pipa <- inner_join(
  rfmo_monthly,
  gfw_monthly,
  by = c("year", "month")
) |>
  mutate(
    date = make_date(year, month, 1)
  ) |>
  arrange(date)


## Long format for plotting ----------------------------------------------------

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
      gfw_effort = "gfw"
    )
  )

## Plot ------------------------------------------------------------------------
# Monthly fishing effort in PIPA using n_distinct days vs RFMO effort days
days <- ggplot(
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
  ) +
  scale_color_manual(
    values = c(
      "rfmo" = "#0072B2",
      "gfw" = "#D55E00"
    ),
    labels = c(
      "rfmo" = "RFMO",
      "gfw" = "GFW"
    )
  ) +
  theme_classic()

# EXPORT #######################################################################

ggsave("results/figures/pipa_original/gfw_rfmo_days_v_hours.png",
       plot = days_v_hours,
       width = 8,
       height = 5,
       dpi = 300)

ggsave("results/figures/pipa_original/gfw_rfmo_days.png",
       plot = days,
       width = 8,
       height = 5,
       dpi = 300)

