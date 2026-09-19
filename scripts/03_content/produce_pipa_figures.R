################################################################################
# Visualize PIPA Analysis
################################################################################
#
# Emily Rodriguez
# ecr108@miami.edu
#
# This script visualizes results from the PIPA blue paradox analysis.
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
  terra,
  ggspatial,
  rnaturalearth,
  rnaturalearthdata,
  patchwork,
  mregions2
)

## Load data -------------------------------------------------------------------

# Filtered RFMO Data
pipa_data <- readRDS("data/processed/pipa_rastered.rds")

# EEZs and MPA
pipa <- st_read("data/raw/mpa/pipa.gpkg")
k_eez <- st_read("data/raw/eez/kiribati_eez.gpkg")
line_eez <- st_read("data/raw/eez/kiribati_eez_line.gpkg")
phoenix_eez <- st_read("data/raw/eez/kiribati_eez_phoenix.gpkg")
gilbert_eez <- st_read("data/raw/eez/kiribati_eez_gilbert.gpkg")

world_eez <- st_read("data/raw/eez/world_eez.gpkg")

# Define temporal boundaries ---------------------------------------------------
start_pre      <- as.Date("2012-09-01")  # 1 year before 2013 announcement
announcement   <- as.Date("2013-09-01")  # Announcement online defined by PIPA paper
implementation <- as.Date("2015-01-01")  # Implementation date
end_post       <- as.Date("2016-01-01")  # 1 year after implementation

# VISUALIZE ####################################################################

# Map RFMO data, PIPA, and EEZ
mapview(pipa_data, zcol = "treatment") +
  mapview(pipa, col.regions = "red", alpha.regions = 0.3) +
  mapview(k_eez, col.regions = "blue", alpha.regions = 0.2)


# Create area of interest plot -------------------------------------------------

## World map
world <- ne_countries(scale = "medium", returnclass = "sf")

# Create area of interest box

aoi_box <- st_as_sfc(  # convert geometry into sf object
  st_bbox(             # creates box
    c(
      xmin = 178,      # defined in lat/lon
      xmax = 220,
      ymin = -13,
      ymax = 7
    ),
    crs = 4326
  )
) |>
  st_transform("EPSG:8859")

# Create global map
global_map <- ggplot() +

  geom_sf(
    data = world,
    fill = "grey85",
    color = "grey50",
    linewidth = 0.15
  ) +

  # Add global EEZ boundaries
  geom_sf(
    data = world_eez,
    fill = NA,
    color = "grey70",
    linewidth = 0.15               # Need to find out why this layer adds lines
  ) +

  geom_sf(
    data = aoi_box,
    fill = NA,
    color = "blue",
    linewidth = 1.2
  ) +

  coord_sf(                  # limits of world map
    crs = "EPSG:8859",
    xlim = c(-3.5e6, 9.5e6),
    ylim = c(-5e6, 3e6),
    datum = NA
  ) +

  annotation_scale(          # scale bar move to right
    location = "br",
    width_hint = 0.25
  ) +

  #annotation_north_arrow(
   # location = "tl",
  #  which_north = "true",
  #  style = north_arrow_fancy_orienteering
  # ) +

  #theme_void() +

  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    ),
    panel.grid.major = element_blank(
    ),
    panel.background = element_rect(
      fill = "aliceblue",
      color = NA
    ),
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.8
    )
  )

# Create zoomed in study region

zoom_map <- ggplot() +

  geom_sf(
    data = gilbert_eez,
    fill = NA,
    color = "grey50",
    linewidth = 0.5
  ) +

  geom_sf(
    data = phoenix_eez,
    fill = NA,
    color = "grey50",
    linewidth = 0.5
  ) +

  geom_sf(
    data = line_eez,
    fill = NA,
    color = "grey50",
    linewidth = 0.5
  ) +

  geom_sf(
    data = pipa,
    fill = NA,
    color = "#D55E00",
    linewidth = 1.4
  ) +

  coord_sf(crs = "EPSG:8859") +

  scale_x_continuous(breaks = c(-190, -180, -170, -160, -150)) +

  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    ),
    panel.grid.major = element_blank(
    ),
    panel.background = element_rect(
      fill = "aliceblue",
      color = NA),
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.8
    )
  )

# Combine figures
# Remove titles

study_area_map <- (global_map + zoom_map) +
  plot_annotation(tag_levels = "A")

study_area_map


# Create fishing effort plot ---------------------------------------------------

# Aggregate and fix meridian issue for RFMO data
tuna_tiles <- pipa_data |>
  filter(year >= 2010, year <= 2015) |>
  group_by(lon, lat) |>
  summarize(effort = sum(effort_day, na.rm = TRUE)) |>
  st_as_sf(coords = c("lon", "lat"), crs = "EPSG:4326")

# Plot fishing effort (days) over study region
p_map_pipa <- ggplot() +

  geom_sf(
    data = tuna_tiles,
    aes(fill = log(effort)),
    color = "transparent"
  ) +

  scale_fill_viridis(
    option = "G",
    na.value = "transparent",
    end = 0.95,
    alpha = 0.9
  ) +

  # Place islands on top
  geom_sf(data = line_eez,
          fill = NA, color = "grey60",
          linewidth = 0.4) +

  geom_sf(data = gilbert_eez,
          fill = NA,
          color = "grey60",
          linewidth = 0.4) +

  geom_sf(data = phoenix_eez,
          fill = NA,
          color = "grey60",
          linewidth = 0.4) +

  geom_sf(
    data = pipa,
    aes(color = "PIPA"),
    fill = NA,
    linewidth = 1.5,
    show.legend = TRUE
  ) +

  coord_sf(expand = FALSE, crs = "EPSG:8859") +

  scale_color_manual(name = NULL, values = c("PIPA" = "#d73027")) +

  scale_x_continuous(
    breaks = seq(150, 210, by = 5),
    labels = function(x) paste0(x, "°")
  ) +

  theme_bw(base_size = 13) +

  theme(
    legend.position = "bottom",
    panel.background = element_rect(fill = "#eaf2f8", color = NA)
  ) +

  # Add north arrow
  annotation_north_arrow(
    location = "bl",
    height = unit(0.8, "cm"),
    width  = unit(0.8, "cm"),
    style = north_arrow_fancy_orienteering()
  ) +

  # Add scale bar
  annotation_scale(location = "bl",
                   pad_x = unit(1, "cm"),
                   pad_y = unit(0.5, "cm")) +
  # Headings
  labs(fill = "Fishing effort\n(log-days)",
       x = "Longitude",
       y = "Latitude")

# Create plot of fishing points ------------------------------------------------
pipa_sf <- pipa_data |>
  distinct(lon, lat, treatment) |>   # one point per cell
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

pipa_map <- ggplot() +
  geom_sf(data = k_eez) +
  geom_sf(data = pipa, fill = "#D55E00", alpha = 0.3) +
  geom_sf(data = line_eez, fill = "#0072B2", alpha = 0.3) +
  geom_sf(data = gilbert_eez, fill = "#0072B2", alpha = 0.3) +
  geom_sf(data = pipa_sf, aes(color = factor(treatment)), size = 2) +
  coord_sf(crs = "EPSG:8859") +
  scale_color_manual(
    values = c("0" = "#0072B2", "1" = "#D55E00"),
    labels = c("Control", "Treatment"),
    name = "Region"
  ) +
  scale_x_continuous(
    breaks = seq(150, 210, by = 5),
    labels = function(x) paste0(x, "°")
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

# Time series standardizing for area -------------------------------------------

# Change catch_tot to kg
pipa_data_kg <- pipa_data |>
  mutate(catch_tot = catch_tot * 1000) |>
  filter(date >= start_pre, date <= implementation)

# Plot catch time series
p_catch_a <- pipa_data_kg |>
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

# Plot effort (days) time series
p_effort_day_a <- pipa_data_kg |>
  group_by(date, treatment) |>
  summarise(effort_day = sum(effort_day, na.rm = TRUE)/(unique(region_area)/ 1e4)) |>
  ggplot(aes(x = date, y = effort_day, color = factor(treatment))) +
  geom_line() +
  geom_point(aes(color = factor(treatment))) +
  geom_vline(xintercept = (announcement), linetype = "dashed") +
  labs(y = "Effort (days per 10,000 km²)", x = "Date", color = "Region") +
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
p_effort_set_a <- pipa_data_kg |>
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

region_df <- pipa_data_kg |>
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

# CPUE days
p_cpue_day_a <- ggplot(cpue_ts, aes(x = date, y = cpue_day, color = factor(treatment))) +
  geom_line() +
  geom_point(aes(color = factor(treatment))) +
  geom_vline(xintercept = (announcement), linetype = "dashed") +
  labs(y = "CPUE (kg/day)", x = "Date", color = "Region") +
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

# Create zoomed in study region

zoom_map2 <- ggplot() +

  geom_sf(
    data = gilbert_eez,
    fill = NA,
    color = "#0072B2",
    linewidth = 0.5
  ) +

  geom_sf(
    data = phoenix_eez,
    fill = NA,
    color = "grey50",
    linewidth = 0.5
  ) +

  geom_sf(
    data = line_eez,
    fill = NA,
    color = "#0072B2",
    linewidth = 0.5
  ) +

  geom_sf(
    data = pipa,
    fill = NA,
    color = "#D55E00",
    linewidth = 1.4
  ) +

  coord_sf(crs = "EPSG:8859") +

  scale_x_continuous(breaks = c(-190, -180, -170, -160, -150)) +

  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    ),
    panel.grid.major = element_blank(
    ),
    panel.background = element_rect(
      fill = "aliceblue",
      color = NA),
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.8
    )
  )


pipa_map_mini <- ggplot() +
  geom_sf(data = k_eez) +
  geom_sf(data = pipa, fill = "#D55E00", alpha = 0.5) +
  geom_sf(data = line_eez, fill = "#0072B2", alpha = 0.5) +
  geom_sf(data = gilbert_eez, fill = "#0072B2", alpha = 0.5) +
  coord_sf(crs = "EPSG:8859") +
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
figure <- p_effort_day_a + p_effort_set_a +
  p_catch_a + p_cpue_day_a + p_cpue_set_a + pipa_map_mini +
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

ggsave("results/figures/pipa_original/map_studyarea.png",
       plot = study_area_map,
       width = 8,
       height = 6,
       dpi = 300)

ggsave("results/figures/pipa_original/map_pipa.png",
       plot = p_map_pipa,
       width = 8,
       height = 6,
       dpi = 300)

ggsave("results/figures/pipa_original/catch_timeseries_area_standardized.png",
       plot = p_catch_a,
       width = 8,
       height = 5,
       dpi = 300)

ggsave("results/figures/pipa_original/effort_day_timeseries_area_standardized.png",
       plot = p_effort_day_a,
       width = 8,
       height = 5,
       dpi = 300)

ggsave("results/figures/pipa_original/effort_set_timeseries_area_standardized.png",
       plot = p_effort_set_a,
       width = 8,
       height = 5,
       dpi = 300)

ggsave("results/figures/pipa_original/cpue_day_timeseries_area_standardized.png",
       plot = p_cpue_day_a,
       width = 8,
       height = 5,
       dpi = 300)

ggsave("results/figures/pipa_original/cpue_set_timeseries_area_standardized.png",
       plot = p_cpue_set_a,
       width = 8,
       height = 5,
       dpi = 300)

ggsave("results/figures/pipa_original/BACI_timeseries_panels_region.png",
       figure,
       width = 15,
       height = 10,
       dpi = 600)

