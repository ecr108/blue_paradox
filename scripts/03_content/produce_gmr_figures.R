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
gmr_data <- readRDS("data/processed/blue_paradox_gmr.rds")

# Spatial data
gmr <- st_read("data/raw/mpa/galapagos_world_heritage.gpkg")
e_main_eez <- st_read("data/raw/eez/ecuador_eez_main.gpkg")
e_gal_eez <- st_read("data/raw/eez/ecuador_eez_galapagos.gpkg")
world <- ne_countries(scale = "medium", returnclass = "sf")

# Define temporal boundaries ---------------------------------------------------

start_pre      <- as.Date("1997-01-01")  # 1 year before 1998 announcement
announcement   <- as.Date("1998-03-18")  # Announcement and implementation of GMR
implementation <- as.Date("2002-01-01")  # Implementation of enforcement date (need to find month)
end_post       <- as.Date("2003-01-01")  # 1 year after implementation

# Make study region map ########################################################
study_region <- ggplot() +

  geom_sf(
    data = e_main_eez,
    fill = NA,
    color = "#0072B2",
    linewidth = 0.5
  ) +

  geom_sf(
    data = e_gal_eez,
    fill = NA,
    color = "grey50",
    linewidth = 0.5
  ) +

  geom_sf(
    data = gmr,
    fill = NA,
    color = "#D55E00",
    linewidth = 1.4
  ) +

  geom_sf(
    data = world,
    fill = "grey85",
    color = "grey50",
    linewidth = 0.15
  ) +

  coord_sf(crs = "EPSG:32715",
           xlim = c(-95.0, -79.0),  # Longitude degrees (West)
           ylim = c(-5.2, 5.2),     # Latitude degrees (South/North)
           default_crs = "EPSG:4326"
  ) +

  scale_x_continuous(breaks = seq(-95, -80, by = 5)) +

  scale_y_continuous(breaks = seq(-5, 5, by = 5)) +


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
study_region

# Map RFMO data, GMR, and EEZ
mapview(gmr_data, zcol = "treatment") +
  mapview(gmr, col.regions = "red", alpha.regions = 0.3) +
  mapview(e_gal_eez, col.regions = "green", alpha.regions = 0.2) +
  mapview(e_main_eez, col.regions = "blue", alpha.regions = 0.2)


# Create fishing effort plot ---------------------------------------------------

# Aggregate and fix meridian issue for RFMO data
tuna_tiles <- gmr_data |>
  filter(year >= 1997, year <= 2003) |>
  group_by(lon, lat) |>
  summarize(effort = sum(effort_set, na.rm = TRUE)) |>
  st_as_sf(coords = c("lon", "lat"), crs = "EPSG:32715")

# Plot fishing effort (days) over study region
p_map_gmr <- ggplot() +

  geom_sf(
    data = world,
    fill = "grey85",
    color = "grey50",
    linewidth = 0.15
  ) +

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
  geom_sf(data = e_main_eez,
          fill = NA, color = "grey60",
          linewidth = 0.4) +

  geom_sf(data = e_gal_eez,
          fill = NA,
          color = "grey60",
          linewidth = 0.4) +

  geom_sf(
    data = gmr,
    aes(color = "GMR"),
    fill = NA,
    linewidth = 1.5,
    show.legend = TRUE
  ) +

  coord_sf(crs = "EPSG:32715",
           xlim = c(-95.0, -79.0),  # Longitude degrees (West)
           ylim = c(-5.2, 5.2),     # Latitude degrees (South/North)
           default_crs = "EPSG:4326"
  ) +

  scale_x_continuous(breaks = seq(-95, -80, by = 5)) +

  scale_y_continuous(breaks = seq(-5, 5, by = 5)) +


  scale_color_manual(name = NULL, values = c("GMR" = "#d73027")) +

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


# Make time series plots #######################################################
# Time series without standardizing for area -----------------------------------

# Plot catch time series
p_catch <- ggplot(gmr_data, aes(x = date, y = catch_tot, color = treatment)) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(fun.data = mean_se, geom = "pointrange", width = 0.2) +
  geom_vline(xintercept = (announcement), linetype = "dashed") +
  geom_vline(xintercept = (implementation),
             linetype = "dashed") +
  labs(
    y = "Catch (tons)",
    x = "Date",
    color = "Region") +
  scale_color_manual(
    values = c(
      "control" = "#0072B2",
      "treatment" = "#D55E00"
    ),
    labels = c(
      "control" = "Control",
      "treatment" = "Treatment"
    )
  ) +
  theme_classic()


# Plot effort (sets) time series
p_effort_set <- ggplot(gmr_data, aes(x = date, y = effort_set,
                                      color = treatment)) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(fun.data = mean_se, geom = "pointrange", width = 0.2) +
  geom_vline(xintercept = (announcement), linetype = "dashed") +
  geom_vline(xintercept = (implementation),
             linetype = "dashed") +
  labs(#title = "Mean fishing effort (sets) over time",
    y = "Effort (sets)", x = "Date", color = "Region") +
  scale_color_manual(
    values = c(
      "control" = "#0072B2",
      "treatment" = "#D55E00"
    ),
    labels = c(
      "control" = "Control",
      "treatment" = "Treatment"
    )
  ) +
  theme_classic()


# Plot CPUE (tons per set) time series
p_cpue_set <- ggplot(gmr_data, aes(x = date, y = cpue_set, color = treatment)) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(fun.data = mean_se, geom = "pointrange", width = 0.2) +
  geom_vline(xintercept = (announcement), linetype = "dashed") +
  geom_vline(xintercept = (implementation),
             linetype = "dashed") +
  labs(#title = "Mean CPUE (tons per set) over time",
    y = "CPUE (tons/set)", x = "Date", color = "Region") +
  scale_color_manual(
    values = c(
      "control" = "#0072B2",
      "treatment" = "#D55E00"
    ),
    labels = c(
      "control" = "Control",
      "treatment" = "Treatment"
    )
  ) +
  theme_classic()


# Time series standardizing for area -------------------------------------------

# Change catch_tot to kg
gmr_data_kg <- gmr_data |>
  mutate(catch_tot = catch_tot * 1000)

# Plot catch time series
p_catch_a <- gmr_data_kg |>
  group_by(date, treatment) |>
  summarise(
    catch_tot = sum(catch_tot, na.rm = TRUE) / unique(region_area)
  ) |>
  ggplot(aes(x = date, y = catch_tot, color = treatment)) +
  geom_line() +
  geom_vline(xintercept = (announcement), linetype = "dashed") +
  geom_vline(xintercept = (implementation),
             linetype = "dashed") +
  labs(#title = "Region-level catch standardized by area over time",
    y = "Catch (kg)", x = "Date", color = "Region") +
  scale_color_manual(
    values = c(
      "control" = "#0072B2",
      "treatment" = "#D55E00"
    ),
    labels = c(
      "control" = "Control",
      "treatment" = "Treatment"
    )
  ) +
  theme_classic()

# Redo with points to match previous
p_catch_a <- gmr_data_kg |>
  group_by(date, treatment) |>
  summarise(
    catch_tot = sum(catch_tot, na.rm = TRUE) / unique(region_area)
  ) |>
  ggplot(aes(date, catch_tot, color = treatment)) +
  geom_line() +
  geom_point(size = 2) +
  geom_vline(xintercept = announcement, linetype = "dashed") +
  geom_vline(xintercept = implementation, linetype = "dashed") +
  labs(
    #title = "Region-level catch standardized by area over time",
    y = "Catch (kg)",
    x = "Date",
    color = "Region"
  ) +
  scale_color_manual(
    values = c(
      "control" = "#0072B2",
      "treatment" = "#D55E00"
    ),
    labels = c(
      "control" = "Control",
      "treatment" = "Treatment"
    )
  ) +
  theme_classic()

# Plot effort (sets) time series
p_effort_set_a <- gmr_data_kg |>
  group_by(date, treatment) |>
  summarise(effort_set = sum(effort_set, na.rm = TRUE)/unique(region_area)) |>
  ggplot(aes(x = date, y = effort_set, color = treatment)) +
  geom_line() +
  geom_point(size = 2) +
  geom_vline(xintercept = (announcement), linetype = "dashed") +
  geom_vline(xintercept = (implementation),
             linetype = "dashed") +
  labs(#title = "Region-level effort (sets) standardized by area over time",
    y = "Sets", x = "Date", color = "Region") +
  scale_color_manual(
    values = c(
      "control" = "#0072B2",
      "treatment" = "#D55E00"
    ),
    labels = c(
      "control" = "Control",
      "treatment" = "Treatment"
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
p_cpue_set_a <- ggplot(cpue_ts, aes(x = date, y = cpue_set, color = treatment)) +
  geom_line() +
  geom_point(size = 2) +
  geom_vline(xintercept = (announcement), linetype = "dashed") +
  geom_vline(xintercept = (implementation),
             linetype = "dashed") +
  labs(#title = "Region-level CPUE (set) standardized by area over time",
    y = "CPUE (kg/sets)", x = "Date", color = "Region") +
  scale_color_manual(
    values = c(
      "control" = "#0072B2",
      "treatment" = "#D55E00"
    ),
    labels = c(
      "control" = "Control",
      "treatment" = "Treatment"
    )
  ) +
  theme_classic()

# Combine Figures ##############################################################


# Pixel
figure1 <- p_effort_set + p_catch + p_cpue_set + study_region +
  plot_layout(
    design = "
    AB
    CD
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

# Area figures
figure2 <- p_effort_set_a +
  p_catch_a + p_cpue_set_a + study_region +
  plot_layout(
    design = "
    AB
    CD
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

# EXPORT #######################################################################

ggsave("results/figures/gmr/catch_timeseries.png",
       plot = p_catch,
       width = 8,
       height = 5,
       dpi = 300)

ggsave("results/figures/gmr/effort_set_timeseries.png",
       plot = p_effort_set,
       width = 8,
       height = 5,
       dpi = 300)

ggsave("results/figures/gmr/cpue_set_timeseries.png",
       plot = p_cpue_set,
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

ggsave("results/figures/gmr/BACI_timeseries_panels.png",
      figure1,
      width = 15,
      height = 8,
      dpi = 600)

ggsave("results/figures/gmr/BACI_timeseries_panels_region.png",
      figure2,
      width = 15,
      height = 8,
      dpi = 600)

