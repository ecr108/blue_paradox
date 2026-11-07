## Load packages ---------------------------------------------------------------
pacman::p_load(
  tidyverse,
  here,
  fixest,
  rms,
  sf,
  lwgeom,
  viridis,
  rnaturalearth,
  rnaturalearthdata,
  patchwork
)

## Load data -------------------------------------------------------------------

# Filtered RFMO Data
pipa_data <- readRDS("data/processed/pipa_rasterized.rds")

# EEZs and MPA
pipa <- st_read("data/raw/mpa/pipa.gpkg")
k_eez <- st_read("data/raw/eez/kiribati_eez.gpkg")
line_eez <- st_read("data/raw/eez/kiribati_eez_line.gpkg")
phoenix_eez <- st_read("data/raw/eez/kiribati_eez_phoenix.gpkg")
gilbert_eez <- st_read("data/raw/eez/kiribati_eez_gilbert.gpkg")
world <- ne_countries(scale = "medium", returnclass = "sf")
world_eez <- st_read("data/raw/eez/world_eez.gpkg") |>
  filter(pol_type == "200NM") # only main EEZ, no intersecting boundaries or treaty agreements


## Prepare data ----------------------------------------------------------------

# Set temporal boundaries
start_pre      <- as.Date("2012-09-01")  # 1 year before 2013 announcement
announcement   <- as.Date("2013-09-01")  # Announcement online defined by PIPA paper
implementation <- as.Date("2015-01-01")  # Implementation date

# Create zoomed in study region ################################################

pipa_map_mini <- ggplot() +
  geom_sf(data = world_eez, fill = NA) +
  geom_sf(data = world) +
  geom_sf(data = k_eez) +
  geom_sf(data = pipa, aes(fill = "#D55E00"), alpha = 0.5) +
  geom_sf(data = line_eez, fill = "#0072B2", alpha = 0.5) +
  geom_sf(data = gilbert_eez, fill = "#0072B2", alpha = 0.5) +
  coord_sf(
    crs = "EPSG:8859",
    default_crs = "EPSG:4326",
    xlim = c(-195, -145),
    ylim = c(-15, 10)
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

# With legend for dif figure
pipa_map_mini_2 <- ggplot() +
  geom_sf(data = world_eez, fill = NA) +
  geom_sf(data = world) +
  geom_sf(data = k_eez) +
  geom_sf(data = pipa, aes(fill = "Treatment"), alpha = 0.5) +
  geom_sf(data = line_eez, aes(fill = "Control"), alpha = 0.5) +
  geom_sf(data = gilbert_eez, aes(fill = "Control"), alpha = 0.5) +
  coord_sf(
    crs = "EPSG:8859",
    default_crs = "EPSG:4326",
    xlim = c(-195, -145),
    ylim = c(-15, 10)
  ) +
  scale_fill_manual(
    values = c(
      "Control" = "#0072B2",
      "Treatment" = "#D55E00"
    ),
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
      color = NA
    ),
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.8
    )
  )
# PROCESSING ###################################################################

## Prepare data ----------------------------------------------------------------
# Change catch_tot to kg
pipa_data_kg <- pipa_data |>
  mutate(catch_tot = catch_tot * 1000)


# Create region-level data
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

region_std_df <- region_df |>
  mutate(
    catch_per_km2 = catch_sum / (region_area / 1e4), # catch per 10,000 km2
    effort_day_per_km2 = effort_day_sum / (region_area / 1e4),
    effort_set_per_km2 = effort_set_sum / (region_area / 1e4)
  )

# Create estimation panel at the region level
region_panel <- region_std_df |>
  st_drop_geometry() |>
  filter(date < implementation) |>
  mutate(post = 1 * (date >= announcement & date < implementation)
  )


spline_data <- region_panel |>
  mutate(
    event = interval(announcement, date) %/% months(1),
    treatment = treatment == 1
    ) |>
  complete(
    nesting(date, event),
    treatment,
    fill = list(
      effort_day_per_km2 = 0,
      effort_set_per_km2 = 0,
      catch_per_km2 = 0,
      cpue_set = NA_real_,
      cpue_day = NA_real_)) |>
  mutate(post = if_else(date >= announcement, 1L, 0L))

# Fit spline for each outcome of interest --------------------------------------
spline_days   <- feols(effort_day_per_km2 ~ treatment * rcs(event),
                     data = spline_data,
                     vcov = "NW",
                     panel.id = ~treatment + date)
spline_sets   <- feols(effort_set_per_km2 ~ treatment * rcs(event),
                     data = spline_data,
                     vcov = "NW",
                     panel.id = ~treatment + date)
spline_catch  <- feols(catch_per_km2 ~ treatment * rcs(event),
                      data = spline_data,
                      vcov = "NW",
                      panel.id = ~treatment + date)
spline_cpue_d <- feols(cpue_day ~ treatment * rcs(event),
                       data = spline_data,
                       vcov = "NW",
                       panel.id = ~treatment + date)
spline_cpue_s <- feols(cpue_set ~ treatment * rcs(event),
                       data = spline_data,
                       vcov = "NW",
                       panel.id = ~treatment + date)

# Get predictions
predictions_days   <- spline_data |>
  cbind(predict(spline_days, newdata = spline_data, se.fit = T))
predictions_sets   <- spline_data |>
  cbind(predict(spline_sets, newdata = spline_data, se.fit = T))
predictions_catch  <- spline_data |>
  cbind(predict(spline_catch, newdata = spline_data, se.fit = T))
predictions_cpue_d <- spline_data |>
  cbind(predict(spline_cpue_d, newdata = spline_data, se.fit = T))
predictions_cpue_s <- spline_data |>
  cbind(predict(spline_cpue_s, newdata = spline_data, se.fit = T))


# Create plotting function
plot_spline <- function(df, observed_var, y_label) {

  ggplot(df, aes(x = date, y = fit, color = treatment, fill = treatment)) +
    geom_ribbon(aes(ymin = fit - se.fit, ymax = fit + se.fit),
                alpha = 0.2, color = NA) +
    geom_line(linewidth = 1) +
    geom_point(aes(y = .data[[observed_var]]), alpha = 0.6) +
    geom_vline(xintercept = announcement, linetype = "dashed") +
    geom_hline(yintercept = 0) +
    labs(
      x = "Date",
      y = y_label,
      color = "Region",
      fill = "Region"
    ) +
    scale_color_manual(
      values = c("FALSE" = "#0072B2", "TRUE" = "#D55E00"),
      labels = c("FALSE" = "Control", "TRUE" = "Treatment")
    ) +
    scale_fill_manual(
      values = c("FALSE" = "#0072B2", "TRUE" = "#D55E00"),
      labels = c("FALSE" = "Control", "TRUE" = "Treatment")
    ) +
    theme_classic()
}

# Plot data
p_effort_day_a <- plot_spline(
  predictions_days,
  "effort_day_per_km2",
  "Effort (days per 10,000 km²)"
)

p_effort_set_a <- plot_spline(
  predictions_sets,
  "effort_set_per_km2",
  "Effort (sets per 10,000 km²)"
)

p_catch_a <- plot_spline(
  predictions_catch,
  "catch_per_km2",
  "Catch (kg per 10,000 km²)"
)

p_cpue_day_a <- plot_spline(
  predictions_cpue_d,
  "cpue_day",
  "CPUE (kg/day)"
)

p_cpue_set_a <- plot_spline(
  predictions_cpue_s,
  "cpue_set",
  "CPUE (kg/set)"
)

# Combine Figures ##############################################################

# Area figures
figure <- p_effort_day_a + p_effort_set_a +
  p_cpue_day_a + p_cpue_set_a + p_catch_a  + pipa_map_mini +
  patchwork::guide_area() +
  plot_layout(
    design = "
    ABG
    CD#
    EF#
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
    #legend.justification = "left",
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15)
  )

ggsave("results/figures/pipa_original/cubic_timeseries_panels_region.png",
       figure,
       width = 15,
       height = 10,
       dpi = 600)

# DIFFERENCE FIGURES ###########################################################

# Calculate day-by-day difference then fit another spline to this difference

# Effort days
diff_data_days <- predictions_days |>
  select(date, event, treatment, fit, se.fit) |>
  pivot_wider(
    names_from = treatment,
    values_from = c(fit, se.fit)
  ) |>
  magrittr::set_colnames(
    c("date", "event", "control", "treated", "se_control", "se_treated")
  ) |>
  mutate(
    change = treated - control,
    se_change = sqrt(se_treated^2 - se_control^2)
  )

# Effort sets
diff_data_sets <- predictions_sets |>
  select(date, event, treatment, fit, se.fit) |>
  pivot_wider(
    names_from = treatment,
    values_from = c(fit, se.fit)
  ) |>
  magrittr::set_colnames(
    c("date", "event", "control", "treated", "se_control", "se_treated")
  ) |>
  mutate(
    change = treated - control,
    se_change = sqrt(se_treated^2 - se_control^2)
  )

# Catch
diff_data_catch <- predictions_catch |>
  select(date, event, treatment, fit, se.fit) |>
  pivot_wider(
    names_from = treatment,
    values_from = c(fit, se.fit)
  ) |>
  magrittr::set_colnames(
    c("date", "event", "control", "treated", "se_control", "se_treated")
  ) |>
  mutate(
    change = treated - control,
    se_change = sqrt(se_treated^2 - se_control^2)
  )

# CPUE days
diff_data_cpue_d <- predictions_cpue_d |>
  select(date, event, treatment, fit, se.fit) |>
  pivot_wider(
    names_from = treatment,
    values_from = c(fit, se.fit)
  ) |>
  magrittr::set_colnames(
    c("date", "event", "control", "treated", "se_control", "se_treated")
  ) |>
  mutate(
    change = treated - control,
    se_change = sqrt(se_treated^2 - se_control^2)
  )

# CPUE sets
diff_data_cpue_s <- predictions_cpue_s |>
  select(date, event, treatment, fit, se.fit) |>
  pivot_wider(
    names_from = treatment,
    values_from = c(fit, se.fit)
  ) |>
  magrittr::set_colnames(
    c("date", "event", "control", "treated", "se_control", "se_treated")
  ) |>
  mutate(
    change = treated - control,
    se_change = sqrt(se_treated^2 - se_control^2)
  )

# Create plotting function -----------------------------------------------------

# Create plotting function for treatment-control differences
plot_difference <- function(df, y_label) {

  ggplot(df, aes(x = date, y = change)) +
    geom_ribbon(
      aes(
        ymin = change - se_change,
        ymax = change + se_change
      ),
      fill = "grey60",
      alpha = 0.2,
      color = NA
    ) +
    geom_line(color = "black", linewidth = 1) +
    geom_vline(xintercept = announcement, linetype = "dashed") +
    geom_hline(yintercept = 0, color = "grey60", linewidth = 0.4) +
    labs(
      x = "Date",
      y = y_label,
      title = ""
    ) +
    theme_classic()
}

# Generate figures -------------------------------------------------------------
# Effort days
p_dif_effort_days <- plot_difference(
  diff_data_days,
  "Treatment - Control (days per 10,000 km²)"
)

# Effort sets
p_dif_effort_sets <- plot_difference(
  diff_data_sets,
  "Treatment - Control (sets per 10,000 km²)"
)

# Catch
p_dif_catch <- plot_difference(
  diff_data_catch,
  "Treatment - Control (kg per 10,000 km²)"
)

# CPUE days
p_dif_cpue_d <- plot_difference(
  diff_data_cpue_d,
  "Treatment - Control (kg/day)"
)

# CPUE sets
p_dif_cpue_s <- plot_difference(
  diff_data_cpue_s,
  "Treatment - Control (kg/set)"
)

# Combine Figures ##############################################################

figure2 <- p_dif_effort_days + p_dif_effort_sets +
  p_dif_cpue_d + p_dif_cpue_s + p_dif_catch + pipa_map_mini_2 +
  patchwork::guide_area() +
  plot_layout(
    design = "
    ABG
    CD#
    EF#
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
    #legend.justification = "left",
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15)
  )

ggsave("results/figures/pipa_original/differences.png",
       figure2,
       width = 15,
       height = 10,
       dpi = 600)

# EXPORT INDIVIDUAL ############################################################

# Effort days with map

effort_map <- p_effort_day_a + pipa_map_mini +
  patchwork::guide_area() +
  plot_layout(
    design = "A/B",
    guides = "collect",
    axes = "collect"
  ) +
  plot_annotation(
    tag_levels = "A"
  ) &
  theme(
    legend.position = "right",
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15),
    plot.margin = margin(0, 0, 0, 0, unit = "pt")
  )

ggsave("results/figures/pipa_original/cubic_timeseries_effort_map.png",
       plot = effort_map,
       width = 8,
       height = 3.5,
       dpi = 300)


effort_map <- p_effort_day_a / pipa_map_mini +
  plot_layout(
    ncol = 1,
    heights = c(1, 1),
    guides = "collect",
    axes = "collect"
  ) +
  plot_annotation(
    tag_levels = "A"
  ) &
  theme(
    legend.position = "right",
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15),
    plot.margin = margin(0, 0, 0, 0, unit = "pt")
  )

ggsave(
  "results/figures/pipa_original/cubic_timeseries_effort_map.png",
  plot = effort_map,
  width = 8,
  height = 6,
  dpi = 300
)

# Effort days alone

ggsave("results/figures/pipa_original/cubic_timeseries_effort.png",
       plot = p_effort_day_a,
       width = 8,
       height = 5,
       dpi = 300)

# CPUE days alone
ggsave("results/figures/pipa_original/cubic_timeseries_cpue_day.png",
       plot = p_cpue_day_a,
       width = 8,
       height = 5,
       dpi = 300)
