# SET UP #######################################################################

## Load packages ---------------------------------------------------------------

pacman::p_load(
  tidyverse,
  sf,
  mapview,
  lwgeom,
  viridis,
  rnaturalearth,
  patchwork
)

## Load data -------------------------------------------------------------------

# Filtered RFMO Data
revilla_data <- readRDS("data/processed/revilla_rasterized.rds")

# Spatial data
revilla <- st_read("data/raw/mpa/revillagigedo_park.gpkg")
mexico_eez <- st_read("data/raw/eez/mexico_eez.gpkg") |>
  st_make_valid()
world <- ne_countries(scale = "medium", returnclass = "sf")
world_eez <- st_read("data/raw/eez/world_eez.gpkg") |>
  filter(pol_type == "200NM") # only main EEZ, no intersecting boundaries or treaty agreements


# Create buffer ----------------------------------------------------------------
nm200 <- 200 * 1852

revilla_buffer <- revilla |>
  st_buffer(dist = nm200)

# Create control region: Mexican EEZ excluding 200 nm around Revillagigedo
control_region <- mexico_eez |>
  st_difference(revilla_buffer) |>
  st_make_valid()

# Define temporal boundaries ---------------------------------------------------
start_pre      <- as.Date("2016-09-17")  # One year before announcement
announcement   <- as.Date("2017-09-17")  # Date that they published the MPA should be made
implementation <- as.Date("2017-11-27")  # Implementation date (from MPA database)
end_post       <- as.Date("2018-11-27")  # 1 year after implementation

# Transform data
revilla_sf <- revilla_data |>
  distinct(lon, lat, treatment) |>   # one point per cell
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

revilla_map <- ggplot() +
  geom_sf(data = world_eez, fill = NA) +
  geom_sf(data = world) +
  geom_sf(data = mexico_eez) +
  geom_sf(data = control_region, fill = "#0072B2", alpha = 0.3) +
  geom_sf(data = revilla, fill = "#D55E00", alpha = 0.3) +
  geom_sf(data = revilla_sf, aes(color = factor(treatment)), size = 2) +
  coord_sf(
    crs = "EPSG:6365",
    default_crs = "EPSG:4326",
    xlim = c(-125, -90),
    ylim = c(10, 35)
  ) +
  scale_color_manual(
    values = c("0" = "#0072B2", "1" = "#D55E00"),
    labels = c("Control", "Treatment"),
    name = "Region"
  ) +

  theme_bw() +
  labs(
    x = "Longitude",
    y = "Latitude"
  ) +
  theme(
    panel.grid.major = element_blank(),
    panel.background = element_rect(
      fill = "aliceblue",
      color = NA),
  )

# Make time series plots #######################################################

# Time series standardizing for area -------------------------------------------

# Change catch_tot to kg
revilla_data_kg <- revilla_data |>
  mutate(catch_tot = catch_tot * 1000) |>
  filter(date >= start_pre, date <= implementation)

# Plot catch time series
p_catch_a <- revilla_data_kg |>
  group_by(date, treatment) |>
  summarise(
    catch_tot = sum(catch_tot, na.rm = TRUE) / (unique(region_area)/ 1e4) # per 10000km2
  ) |>
  ggplot(aes(x = date, y = catch_tot, color = factor(treatment))) +
  geom_line() +
  geom_point(aes(color = factor(treatment))) +
  geom_vline(xintercept = (announcement), linetype = "dashed") +
  labs(y = "Catch (kg per 10,000 km²)", x = "Date", color = "Region") +
  scale_color_manual(
    values = c(
      "0" = "#0072B2",
      "1" = "#D55E00"
    ),
    labels = c(
      "0" = "Control",
      "1" = "Treatment"
    )
  ) +
  theme_classic()

# Plot effort (sets) time series
p_effort_set_a <- revilla_data_kg |>
  group_by(date, treatment) |>
  summarise(effort_set = sum(effort_set, na.rm = TRUE)/(unique(region_area)/ 1e4)) |>
  ggplot(aes(x = date, y = effort_set, color = factor(treatment))) +
  geom_line() +
  geom_point(aes(color = factor(treatment))) +
  geom_vline(xintercept = (announcement), linetype = "dashed") +
  labs(y = "Effort (sets per 10,000 km²)", x = "Date", color = "Region") +
  scale_color_manual(
    values = c(
      "0" = "#0072B2",
      "1" = "#D55E00"
    ),
    labels = c(
      "0" = "Control",
      "1" = "Treatment"
    )
  ) +
  theme_classic()


# Set up CPUE data

region_df <- revilla_data_kg |>
  group_by(treatment, date, region_area) |>
  summarise(
    catch_sum = sum(catch_tot, na.rm = TRUE),
    effort_day_sum = sum(effort_day, na.rm = TRUE),
    effort_set_sum = sum(effort_set, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    cpue_day = catch_sum / effort_day_sum,
    cpue_set = catch_sum / effort_set_sum
  )

cpue_ts <- region_df |>
  group_by(date, treatment) |>
  summarise(
    cpue_day = mean(cpue_day, na.rm = TRUE),
    cpue_set = mean(cpue_set, na.rm = TRUE),
    .groups = "drop"
  )

# CPUE sets
p_cpue_set_a <- ggplot(cpue_ts, aes(x = date, y = cpue_set,
                                    color = factor(treatment))) +
  geom_line() +
  geom_point(aes(color = factor(treatment))) +
  geom_vline(xintercept = (announcement), linetype = "dashed") +
  labs(y = "CPUE (kg/sets)", x = "Date", color = "Region") +
  scale_color_manual(
    values = c(
      "0" = "#0072B2",
      "1" = "#D55E00"
    ),
    labels = c(
      "0" = "Control",
      "1" = "Treatment"
    )
  ) +
  theme_classic()

# Create a second zoomed in study region with control colors ###################
revilla_map_mini <- ggplot() +
  geom_sf(data = world_eez, fill = NA) +
  geom_sf(data = world) +
  geom_sf(data = mexico_eez) +
  geom_sf(data = revilla, fill = "#D55E00", alpha = 0.5) +
  geom_sf(data = control_region, fill = "#0072B2", alpha = 0.5) +
  coord_sf(
    crs = "EPSG:6365",
    default_crs = "EPSG:4326",
    xlim = c(-125, -90),
    ylim = c(10, 35)
  ) +
  scale_color_manual(
    values = c("0" = "#0072B2", "1" = "#D55E00"),
    labels = c("Control", "Treatment"),
    name = "Region"
  ) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(
      fill = "aliceblue",
      color = NA),
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.8
    )
  )

# Combine Figures ##############################################################

# Area figures
figure <- p_effort_set_a +
  p_catch_a + p_cpue_set_a + revilla_map_mini + patchwork::guide_area() +
  plot_layout(
    design = "
    ABF
    CD#
    ",
    widths = c(1, 1),
    heights = c(1, 1, 1),
    guides = "collect"
  ) +
  plot_annotation(
    tag_levels = "A"
  ) &
  theme(
    legend.position = "right",
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15)
  )

# EXPORT #######################################################################
ggsave("results/figures/revilla/revilla_map.png",
       plot = revilla_map,
       width = 8,
       height = 5,
       dpi = 300)

ggsave("results/figures/revilla/catch_timeseries_area_standardized.png",
       plot = p_catch_a,
       width = 8,
       height = 5,
       dpi = 300)

ggsave("results/figures/revilla/effort_set_timeseries_area_standardized.png",
       plot = p_effort_set_a,
       width = 8,
       height = 5,
       dpi = 300)

ggsave("results/figures/revilla/cpue_set_timeseries_area_standardized.png",
       plot = p_cpue_set_a,
       width = 8,
       height = 5,
       dpi = 300)

ggsave("results/figures/revilla/BACI_timeseries_panels_region.png",
       figure,
       width = 15,
       height = 8,
       dpi = 600)

