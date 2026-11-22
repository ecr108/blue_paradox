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
revilla_data <- readRDS("data/processed/revilla_rasterized.rds")

# Spatial data
revilla <- st_read("data/raw/mpa/revillagigedo_park.gpkg")
mexico_eez <- st_read("data/raw/eez/mexico_eez.gpkg") |>
  st_make_valid()
world <- ne_countries(scale = "medium", returnclass = "sf")
world_eez <- st_read("data/raw/eez/world_eez.gpkg") |>
  filter(pol_type == "200NM") # only main EEZ, no intersecting boundaries or treaty agreements

## Prepare data ----------------------------------------------------------------

# Set temporal boundaries
start_pre      <- as.Date("2016-09-17")  # One year before announcement
announcement   <- as.Date("2017-09-17")  # Date that they published the MPA should be made
implementation <- as.Date("2017-11-27")  # Implementation date (from MPA database)

# Create buffer ----------------------------------------------------------------
nm200 <- 200 * 1852

revilla_buffer <- revilla |>
  st_buffer(dist = nm200)

# Create control region: Mexican EEZ excluding 200 nm around Revillagigedo
control_region <- mexico_eez |>
  st_difference(revilla_buffer) |>
  st_make_valid()

# Create zoomed in study region ################################################

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


# With legend for dif figure
revilla_map_mini_2 <- ggplot() +
  geom_sf(data = world_eez, fill = NA) +
  geom_sf(data = world) +
  geom_sf(data = revilla, aes(fill = "Treatment"), alpha = 0.5) +
  geom_sf(data = control_region, aes(fill = "Control"), alpha = 0.5) +
  coord_sf(
    crs = "EPSG:6365",
    default_crs = "EPSG:4326",
    xlim = c(-125, -90),
    ylim = c(10, 35)
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
revilla_data_kg <- revilla_data |>
  mutate(catch_tot = catch_tot * 1000)


# Create region-level data
region_df <- revilla_data_kg |>
  group_by(treatment, date, region_area) |>
  summarise(
    catch_sum = sum(catch_tot, na.rm = TRUE),
    effort_set_sum = sum(effort_set, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    cpue_set = catch_sum / effort_set_sum
  )

region_std_df <- region_df |>
  mutate(
    catch_per_km2 = catch_sum / (region_area / 1e4), # catch per 10,000 km2
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
      effort_set_per_km2 = 0,
      catch_per_km2 = 0,
      cpue_set = NA_real_)) |>
  mutate(post = if_else(date >= announcement, 1L, 0L))

# Fit spline for each outcome of interest --------------------------------------
spline_sets   <- feols(effort_set_per_km2 ~ treatment * rcs(event),
                       data = spline_data,
                       vcov = "NW",
                       panel.id = ~treatment + date)
spline_catch  <- feols(catch_per_km2 ~ treatment * rcs(event),
                       data = spline_data,
                       vcov = "NW",
                       panel.id = ~treatment + date)
spline_cpue_s <- feols(cpue_set ~ treatment * rcs(event),
                       data = spline_data,
                       vcov = "NW",
                       panel.id = ~treatment + date)

# Get predictions
predictions_sets   <- spline_data |>
  cbind(predict(spline_sets, newdata = spline_data, se.fit = T))

predictions_catch  <- spline_data |>
  cbind(predict(spline_catch, newdata = spline_data, se.fit = T))

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

p_cpue_set_a <- plot_spline(
  predictions_cpue_s,
  "cpue_set",
  "CPUE (kg/set)"
)

# Combine Figures ##############################################################

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

ggsave("results/figures/revilla/cubic_timeseries_panels_region.png",
       figure,
       width = 15,
       height = 10,
       dpi = 600)

# DIFFERENCE FIGURES ###########################################################

# Calculate day-by-day difference then fit another spline to this difference

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

# Effort sets
p_dif_effort_sets <- plot_difference(
  diff_data_sets,
  "Effort (sets per 10,000 km²)"
)

# Catch
p_dif_catch <- plot_difference(
  diff_data_catch,
  "Catch (kg per 10,000 km²)"
)

# CPUE sets
p_dif_cpue_s <- plot_difference(
  diff_data_cpue_s,
  "CPUE (kg/set)"
)

# Combine Figures ##############################################################

figure2 <- p_dif_effort_sets +
  p_dif_catch + p_dif_cpue_s + revilla_map_mini_2 +
  patchwork::guide_area() +
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
    #legend.justification = "left",
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15)
  )

ggsave("results/figures/revilla/differences.png",
       figure2,
       width = 15,
       height = 10,
       dpi = 600)

