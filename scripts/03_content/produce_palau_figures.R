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
  patchwork
)

## Load data -------------------------------------------------------------------

# Filtered RFMO Data
palau_data <- readRDS("data/processed/palau_rasterized.rds")

# Spatial data
pnms <- st_read("data/raw/mpa/pnms_2015.gpkg")
palau <- st_read("data/raw/eez/palau_eez.gpkg")
fsm <- st_read("data/raw/eez/fsm_eez.gpkg")
world <- ne_countries(scale = "medium", returnclass = "sf")
world_eez <- st_read("data/raw/eez/world_eez.gpkg") |>
  filter(pol_type == "200NM") # only main EEZ, no intersecting boundaries or treaty agreements

# Define FSM control area within 200 nm of Palau -------------------------------

# Create buffer
nm200 <- 200 * 1852

# 200 nm buffer around Palau, restricted to FSM EEZ
control_region <- palau |>
  st_union() |>
  st_buffer(dist = nm200) |>
  st_intersection(st_union(fsm))


# Define temporal boundaries ---------------------------------------------------
start_pre      <- as.Date("2014-10-22")  # 1 year before 2015 announcement
announcement   <- as.Date("2015-10-22")  # Announcement of Palau (per MPA dataset)
implementation <- as.Date("2020-01-01")  # Implementation of Palau
end_post       <- as.Date("2021-01-01")  # 1 year after implementation

# Transform data
palau_sf <- palau_data |>
  distinct(lon, lat, treatment) |>   # one point per cell
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

palau_map <- ggplot() +
  geom_sf(data = world_eez, fill = NA) +
  geom_sf(data = world) +
  geom_sf(data = palau) +
  geom_sf(data = fsm) +
  geom_sf(data = control_region, fill = "#0072B2", alpha = 0.3) +
  geom_sf(data = pnms, fill = "#D55E00", alpha = 0.3) +
  geom_sf(data = palau_sf, aes(color = factor(treatment)), size = 3) +
  scale_color_manual(
    values = c("0" = "#0072B2", "1" = "#D55E00"),
    labels = c("Control", "Treatment"),
    name = "Region"
  ) +
  coord_sf(
    xlim = c(125, 150),
    ylim = c(-2, 15),
    expand = FALSE
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
palau_data_kg <- palau_data |>
  mutate(catch_tot = catch_tot * 1000) |>
  filter(date >= start_pre, date <= implementation)

# Plot catch time series
p_catch_a <- palau_data_kg |>
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
p_effort_day_a <- palau_data_kg |>
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
p_effort_set_a <- palau_data_kg |>
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

region_df <- palau_data_kg |>
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
palau_map_mini <- ggplot() +
  geom_sf(data = world_eez, fill = NA) +
  geom_sf(data = world) +
  geom_sf(data = palau) +
  geom_sf(data = fsm) +
  geom_sf(data = pnms, fill = "#D55E00", alpha = 0.5) +
  geom_sf(data = control_region, fill = "#0072B2", alpha = 0.5) +
  scale_color_manual(
    values = c("0" = "#0072B2", "1" = "#D55E00"),
    labels = c("Control", "Treatment"),
    name = "Region"
  ) +
  coord_sf(
    xlim = c(125, 150),
    ylim = c(-2, 15),
    expand = FALSE
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank()
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
figure <- p_effort_day_a + p_effort_set_a +
  p_catch_a + p_cpue_day_a + p_cpue_set_a + palau_map_mini +
  patchwork::guide_area() +
  plot_layout(
    design = "
    ABG
    CD#
    EF#
    ",
    widths = c(1, 1),
    heights = c(1, 1, 1.5),
    guides = "collect"
  ) +
  plot_annotation(
    tag_levels = "A"
  ) &
  theme(
    legend.position = "right",
    #legend.justification = "left",
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15)
  )

figure

# EXPORT #######################################################################
ggsave("results/figures/palau/palau_map.png",
       plot = palau_map,
       width = 8,
       height = 5,
       dpi = 300)

ggsave("results/figures/palau/catch_timeseries_area_standardized.png",
       plot = p_catch_a,
       width = 8,
       height = 5,
       dpi = 300)

ggsave("results/figures/palau/effort_day_timeseries_area_standardized.png",
       plot = p_effort_day_a,
       width = 8,
       height = 5,
       dpi = 300)

ggsave("results/figures/palau/effort_set_timeseries_area_standardized.png",
       plot = p_effort_set_a,
       width = 8,
       height = 5,
       dpi = 300)

ggsave("results/figures/palau/cpue_day_timeseries_area_standardized.png",
       plot = p_cpue_day_a,
       width = 8,
       height = 5,
       dpi = 300)

ggsave("results/figures/palau/cpue_set_timeseries_area_standardized.png",
       plot = p_cpue_set_a,
       width = 8,
       height = 5,
       dpi = 300)

ggsave("results/figures/palau/BACI_timeseries_panels_region.png",
       figure,
       width = 15,
       height = 8,
       dpi = 600)

