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
revilla_data <- readRDS("data/processed/blue_paradox_revilla.rds")

# Spatial data
revilla <- st_read("data/raw/mpa/revillagigedo_park.gpkg")
mexico_eez <- st_read("data/raw/eez/mexico_eez.gpkg")

# Define temporal boundaries ---------------------------------------------------
start_pre      <- as.Date("2016-09-17")  # One year before announcement
announcement   <- as.Date("2017-09-17")  # Date that they published the MPA should be made
implementation <- as.Date("2017-11-27")  # Implementation date (from MPA database)
end_post       <- as.Date("2018-11-27")  # 1 year after implementation

# Time series without standardizing for area -----------------------------------

# Plot catch time series
p_catch <- ggplot(revilla_data, aes(x = date, y = catch_tot, color = treatment)) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(fun.data = mean_se, geom = "pointrange", width = 0.2) +
  geom_vline(xintercept = (announcement), linetype = "dashed") +
  geom_vline(xintercept = (implementation),
             linetype = "dashed") +
  labs(
    y = "Catch (tons)",
    x = "Date",
    color = "Region") +
  scale_color_discrete(
    labels = c("control" = "Control",
               "treatment" = "Treatment")
  ) +
  theme_classic()


# Plot effort (sets) time series
p_effort_set <- ggplot(revilla_data, aes(x = date, y = effort_set,
                                     color = treatment)) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(fun.data = mean_se, geom = "pointrange", width = 0.2) +
  geom_vline(xintercept = (announcement), linetype = "dashed") +
  geom_vline(xintercept = (implementation),
             linetype = "dashed") +
  labs(#title = "Mean fishing effort (sets) over time",
    y = "Effort (sets)", x = "Date", color = "Region") +
  scale_color_discrete(
    labels = c("control" = "Control",
               "treatment" = "Treatment")
  ) +
  theme_classic()


# Plot CPUE (tons per set) time series
p_cpue_set <- ggplot(revilla_data, aes(x = date, y = cpue_set, color = treatment)) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(fun.data = mean_se, geom = "pointrange", width = 0.2) +
  geom_vline(xintercept = (announcement), linetype = "dashed") +
  geom_vline(xintercept = (implementation),
             linetype = "dashed") +
  labs(#title = "Mean CPUE (tons per set) over time",
    y = "CPUE (tons/set)", x = "Date", color = "Region") +
  scale_color_discrete(
    labels = c("control" = "Control",
               "treatment" = "Treatment")
  ) +
  theme_classic()


# Time series standardizing for area -------------------------------------------

# Change catch_tot to kg
revilla_data_kg <- revilla_data |>
  mutate(catch_tot = catch_tot * 1000)

# Plot catch time series
p_catch_a <- revilla_data_kg |>
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
  scale_color_discrete(
    labels = c("control" = "Control",
               "treatment" = "Treatment")
  ) +
  theme_classic()

# Redo with points to match previous
p_catch_a <- revilla_data_kg |>
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
  scale_color_discrete(
    labels = c(control = "Control",
               treatment = "Treatment")
  ) +
  theme_classic()

# Plot effort (sets) time series
p_effort_set_a <- revilla_data_kg |>
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
  scale_color_discrete(
    labels = c("control" = "Control",
               "treatment" = "Treatment")
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
p_cpue_set_a <- ggplot(cpue_ts, aes(x = date, y = cpue_set, color = treatment)) +
  geom_line() +
  geom_point(size = 2) +
  geom_vline(xintercept = (announcement), linetype = "dashed") +
  geom_vline(xintercept = (implementation),
             linetype = "dashed") +
  labs(#title = "Region-level CPUE (set) standardized by area over time",
    y = "CPUE (kg/sets)", x = "Date", color = "Region") +
  scale_color_discrete(
    labels = c("control" = "Control",
               "treatment" = "Treatment")
  ) +
  theme_classic()

# Combine Figures ##############################################################


# Pixel
figure1 <- p_effort_set + p_catch + p_cpue_set +
  plot_layout(
    design = "
    AB
    C#
    "
  ) +
  plot_annotation(
    tag_levels = "A"
  )

# Area figures
figure2 <- p_effort_set_a +
  p_catch_a + p_cpue_set_a +
  plot_layout(
    design = "
    AB
    C#
    "
  ) +
  plot_annotation(
    tag_levels = "A"
  )

# EXPORT #######################################################################

ggsave("results/figures/revilla/catch_timeseries.png",
       plot = p_catch,
       width = 8,
       height = 5,
       dpi = 300)

ggsave("results/figures/revilla/effort_set_timeseries.png",
       plot = p_effort_set,
       width = 8,
       height = 5,
       dpi = 300)

ggsave("results/figures/revilla/cpue_set_timeseries.png",
       plot = p_cpue_set,
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

ggsave("results/figures/revilla/BACI_timeseries_panels.png",
       figure1,
       width = 15,
       height = 8,
       dpi = 600)

ggsave("results/figures/revilla/BACI_timeseries_panels_region.png",
       figure2,
       width = 15,
       height = 8,
       dpi = 600)

