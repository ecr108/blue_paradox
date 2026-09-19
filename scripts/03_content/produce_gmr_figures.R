################################################################################
# Visualize GMR  Analysis
################################################################################
#
# Emily Rodriguez
# ecr108@miami.edu
#
# This script visualizes results from the GMR blue paradox analysis.
#
################################################################################

# SET UP #######################################################################

## Load packages ---------------------------------------------------------------

pacman::p_load(
  tidyverse,
  sf,
  mapview,
  lwgeom,
  viridis,
  rnaturalearth,
  rnaturalearthdata,
  patchwork
)

## Load data -------------------------------------------------------------------

# Filtered RFMO Data
gmr_data <- readRDS("data/processed/gmr_rastered.rds")

# Spatial data
gmr <- st_read("data/raw/mpa/galapagos_world_heritage.gpkg")
e_main_eez <- st_read("data/raw/eez/ecuador_eez_main.gpkg")
e_gal_eez <- st_read("data/raw/eez/ecuador_eez_galapagos.gpkg")
world <- ne_countries(scale = "medium", returnclass = "sf")
world_eez <- st_read("data/raw/eez/world_eez.gpkg")

# Define temporal boundaries ---------------------------------------------------

start_pre      <- as.Date("1997-01-01")  # 1 year before 1998 announcement
announcement   <- as.Date("1998-03-18")  # Announcement and implementation of GMR
implementation <- as.Date("2002-01-01")  # Implementation of enforcement date (need to find month)
end_post       <- as.Date("2003-01-01")  # 1 year after implementation

# Make study region map ########################################################

# Build a study area mask
#study_area <- st_union(e_gal_eez, e_main_eez)

# Crop world map and eez
#world_crop <- st_intersects(world, study_area)
#world_eez_crop <- st_intersects(world_eez, study_area)

library(sf)

# Define crop area
crop_bbox <- st_bbox(
  c(
    xmin = -95.0,
    xmax = -79.0,
    ymin = -5.2,
    ymax = 5.2
  ),
  crs = st_crs(4326)
)

# Crop shapefiles
world_crop <- st_crop(world, crop_bbox)
world_eez_crop <- st_crop(world_eez, crop_bbox)




# Transform data
gmr_sf <- gmr_data |>
  distinct(lon, lat, treatment) |>   # one point per cell
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

gmr_map <- ggplot() +
  #geom_sf(data = world_crop, fill = "grey90", color = "grey70") +
  #geom_sf(data = world_eez_crop, fill = NA, color = "grey50") +
  geom_sf(data = world_eez, alpha = 0.3) +
  geom_sf(data = world) +
  geom_sf(data = e_gal_eez) +
  geom_sf(data = gmr, fill = "#D55E00", alpha = 0.3) +
  geom_sf(data = e_main_eez, fill = "#0072B2", alpha = 0.3) +

  geom_sf(data = gmr_sf, aes(color = factor(treatment)), size = 2) +
  coord_sf(crs = "EPSG:32715",
           xlim = c(-95.0, -79.0),  # Longitude degrees (West)
           ylim = c(-5.2, 5.2),     # Latitude degrees (South/North)
           default_crs = "EPSG:4326"
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
gmr_data_kg <- gmr_data |>
  mutate(catch_tot = catch_tot * 1000) |>
  filter(date >= start_pre, date <= implementation)

# Plot catch time series
p_catch_a <- gmr_data_kg |>
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
p_effort_set_a <- gmr_data_kg |>
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

region_df <- gmr_data_kg |>
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
gmr_map_mini <- ggplot() +
  geom_sf(data = e_gal_eez) +
  geom_sf(data = gmr, fill = "#D55E00", alpha = 0.5) +
  geom_sf(data = e_main_eez, fill = "#0072B2", alpha = 0.5) +
  #coord_sf(crs = "EPSG:8859") +
  scale_color_manual(
    values = c("0" = "#0072B2", "1" = "#D55E00"),
    labels = c("Control", "Treatment"),
    name = "Region"
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank()
  ) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major = element_blank()
  )

# Combine Figures ##############################################################

# Area figures
figure <- p_effort_set_a +
  p_catch_a + p_cpue_set_a + revilla_map_mini +
  plot_layout(
    design = "
    AB
    CD
    EF
    ",
    widths = c(1, 1),
    heights = c(1, 1, 1),
    guides = "collect"
  ) +
  plot_annotation(
    tag_levels = "A"
  ) &
  theme(
    legend.position = "bottom",
    legend.justification = "left",
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15)
  )

figure

# EXPORT #######################################################################
ggsave("results/figures/gmr/gmr_map.png",
       plot = gmr_map,
       width = 8,
       height = 5,
       dpi = 300)

ggsave("results/figures/gmr/catch_timeseries_area_standardized.png",
       plot = p_catch_a,
       width = 8,
       height = 5,
       dpi = 300)

ggsave("results/figures/gmr/effort_set_timeseries_area_standardized.png",
       plot = p_effort_set_a,
       width = 8,
       height = 5,
       dpi = 300)

ggsave("results/figures/gmr/cpue_set_timeseries_area_standardized.png",
       plot = p_cpue_set_a,
       width = 8,
       height = 5,
       dpi = 300)

ggsave("results/figures/gmr/BACI_timeseries_panels_region.png",
      figure,
      width = 15,
      height = 8,
      dpi = 600)

