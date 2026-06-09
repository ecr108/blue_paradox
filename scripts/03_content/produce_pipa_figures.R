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

library(tidyverse)
library(sf)
library(mapview)
library(lwgeom)
library(patchwork)
library(viridis)

## Load data -------------------------------------------------------------------

# Filtered RFMO Data
pipa_data <- readRDS("data/processed/blue_paradox_pipa.rds")

# EEZs and MPA
pipa <- st_read("data/raw/mpa/pipa.gpkg")
k_eez <- st_read("data/raw/eez/kiribati_eez.gpkg")
line_eez <- st_read("data/raw/eez/kiribati_eez_line.gpkg")
phoenix_eez <- st_read("data/raw/eez/kiribati_eez_phoenix.gpkg")
gilbert_eez <- st_read("data/raw/eez/kiribati_eez_gilbert.gpkg")

# Define temporal boundaries
start_pre      <- as.Date("2012-09-01")  # 1 year before 2013 announcement
announcement   <- as.Date("2013-09-01")  # Announcement online defined by PIPA paper
implementation <- as.Date("2015-01-01")  # Implementation date
end_post       <- as.Date("2016-01-01")  # 1 year after implementation

# VISUALIZE ####################################################################

# Map RFMO data, PIPA, and EEZ
mapview(pipa_data, zcol = "treatment") +
  mapview(pipa, col.regions = "red", alpha.regions = 0.3) +
  mapview(k_eez, col.regions = "blue", alpha.regions = 0.2)


# Aggregate and fix meridian issue for RFMO data
tuna_tiles <- pipa_data |>
  mutate(lon_plot = if_else(lon < 0, lon + 360, lon)) |>
  group_by(lon_plot, lat) |>
  summarise(
    effort_day_sum = sum(effort_day, na.rm = TRUE),
    .groups = "drop"
  )

# Fix meridian issue for spatial objects
pipa_360     <- st_shift_longitude(pipa)
line_360     <- st_shift_longitude(line_eez)
gilbert_360  <- st_shift_longitude(gilbert_eez)
phoenix_360  <- st_shift_longitude(phoenix_eez)
kiribati_360 <- st_shift_longitude(k_eez)

# Plot fishing effort (days) over study region
p_map_pipa <- ggplot() +
  geom_tile(
    data = tuna_tiles,
    aes(x = lon_plot, y = lat, fill = effort_day_sum),
    width = 1, height = 1
  ) +
  scale_fill_viridis(
    name = "Fishing effort (days)",
    option = "G",
    trans = "sqrt",
    na.value = "transparent",
    end = 0.95,
    alpha = 0.9
  ) +

  #geom_sf(data = kiribati_360, fill = NA, color = "grey40", linewidth = 0.3) +
  geom_sf(data = line_360,     fill = NA, color = "grey60", linewidth = 0.4) +
  geom_sf(data = gilbert_360,  fill = NA, color = "grey60", linewidth = 0.4) +
  geom_sf(data = phoenix_360,  fill = NA, color = "grey60", linewidth = 0.4) +
  geom_sf(data = pipa_360,     fill = NA, color = "red",    linewidth = 1.5) +

  coord_sf(expand = FALSE, datum = NA) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 9),
    legend.position = "right"
  )

# Time series without standardizing for area -----------------------------------

# Plot catch time series
p_catch <- ggplot(pipa_data, aes(x = date, y = catch_tot, color = treatment)) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  geom_vline(xintercept = (announcement), linetype = "dashed") +
  geom_vline(xintercept = (implementation),
             linetype = "dashed", color = "red") +
  labs(y = "Catch (tons)", x = "Date", color = "Region") +
  theme_classic()


# Plot effort (days) time series
p_effort_day <- ggplot(pipa_data, aes(x = date, y = effort_day, color = treatment)) +
  stat_summary(fun = sum, geom = "line") +
  geom_vline(xintercept = (announcement), linetype = "dashed") +
  geom_vline(xintercept = (implementation),
             linetype = "dashed", color = "red") +
  labs(y = "Effort (days)", x = "Date", color = "Region") +
  theme_classic()


# Plot effort (sets) time series
p_effort_set <- ggplot(pipa_data, aes(x = date, y = effort_set, color = treatment)) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  geom_vline(xintercept = (announcement), linetype = "dashed") +
  geom_vline(xintercept = (implementation),
             linetype = "dashed", color = "red") +
  labs(y = "Effort (sets)", x = "Date", color = "Region") +
  theme_classic()

# Plot CPUE (tons per day) time series
p_cpue_day <-  ggplot(pipa_data, aes(x = date, y = cpue_day, color = treatment)) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  geom_vline(xintercept = (announcement), linetype = "dashed") +
  geom_vline(xintercept = (implementation),
             linetype = "dashed", color = "red") +
  labs(y = "CPUE (tons/day)", x = "Date", color = "Region") +
  theme_classic()

# Plot CPUE (tons per set) time series
p_cpue_set <- ggplot(pipa_data, aes(x = date, y = cpue_set, color = treatment)) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  geom_vline(xintercept = (announcement), linetype = "dashed") +
  geom_vline(xintercept = (implementation),
             linetype = "dashed", color = "red") +
  labs(y = "CPUE (tons/set)", x = "Date", color = "Region") +
  theme_classic()


# Time series standardizing for area -------------------------------------------

# Plot catch time series
p_catch_a <- pipa_data |>
  group_by(date, treatment) |>
  summarize(catch_tot = sum(catch_tot, na.rm = TRUE)/unique(region_area)) |>
  ggplot(aes(x = date, y = catch_tot, color = treatment, group = treatment)) +
  geom_line() +
  geom_vline(xintercept = (announcement), linetype = "dashed") +
  geom_vline(xintercept = (implementation),
             linetype = "dashed", color = "red") +
  labs(y = "Catch (mt)", x = "Date", color = "Region") +
  theme_minimal()


# Plot effort (days) time series
p_effort_day_a <- pipa_data |>
  group_by(date, treatment) |>
  summarize(effort_day = sum(effort_day, na.rm = TRUE)/unique(region_area)) |>
  ggplot(aes(x = date, y = effort_day, color = treatment, group = treatment)) +
  geom_line() +
  geom_vline(xintercept = (announcement), linetype = "dashed") +
  geom_vline(xintercept = (implementation),
             linetype = "dashed", color = "red") +
  labs(y = "Days", x = "Date", color = "Region") +
  theme_minimal()

# Plot effort (sets) time series
p_effort_set_a <- pipa_data |>
  group_by(date, treatment) |>
  summarize(effort_set = sum(effort_set, na.rm = TRUE)/unique(region_area)) |>
  ggplot(aes(x = date, y = effort_set, color = treatment, group = treatment)) +
  geom_line() +
  geom_vline(xintercept = (announcement), linetype = "dashed") +
  geom_vline(xintercept = (implementation),
             linetype = "dashed", color = "red") +
  labs(y = "Sets", x = "Date", color = "Region") +
  theme_minimal()

# EXPORT #######################################################################

ggsave("results/figures/pipa_original/p_map_pipa.png",
       plot = p_map_pipa,
       width = 8,
       height = 6,
       dpi = 300)

ggsave("results/figures/pipa_original/catch_timeseries.png",
       plot = p_catch,
       width = 8,
       height = 5,
       dpi = 300)

ggsave("results/figures/pipa_original/effort_day_timeseries.png",
       plot = p_effort_day,
       width = 8,
       height = 5,
       dpi = 300)

ggsave("results/figures/pipa_original/effort_set_timeseries.png",
       plot = p_effort_set,
       width = 8,
       height = 5,
       dpi = 300)

ggsave("results/figures/pipa_original/cpue_day_timeseries.png",
       plot = p_cpue_day,
       width = 8,
       height = 5,
       dpi = 300)

ggsave("results/figures/pipa_original/cpue_set_timeseries.png",
       plot = p_cpue_set,
       width = 8,
       height = 5,
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
