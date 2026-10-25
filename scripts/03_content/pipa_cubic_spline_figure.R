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



# Create a second zoomed in study region with control colors ###################

# Create zoomed in study region

pipa_map_mini <- ggplot() +
  geom_sf(data = world_eez, fill = NA) +
  geom_sf(data = world) +
  geom_sf(data = k_eez) +
  geom_sf(data = pipa, fill = "#D55E00", alpha = 0.5) +
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

# PROCESSING ###################################################################

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


my_data <- region_panel |>
  mutate(event = interval(announcement, date) %/% months(1),
         treatment = treatment == 1) |>
  complete(nesting(date, event), treatment, fill = list(effort_day_per_km2 = 0,
                                                        effort_set_per_km2 = 0,
                                                        catch_per_km2 = 0,
                                                        cpue_set = 0,
                                                        cpue_day = 0))

# Fit spline for each region ###################################################
# Days
spline_days <- feols(effort_day_per_km2 ~ treatment * rcs(event),
                data = my_data)

predictions_days <- my_data |>
  cbind(predict(spline_days, se.fit = T))

# Sets
spline_sets <- feols(effort_set_per_km2 ~ treatment * rcs(event),
                     data = my_data)

predictions_sets <- my_data |>
  cbind(predict(spline_sets, se.fit = T))

# Catch
spline_catch <- feols(catch_per_km2 ~ treatment * rcs(event),
                      data = my_data)

predictions_catch <- my_data |>
  cbind(predict(spline_catch, se.fit = T))

# CPUE days
spline_cpue_d <- feols(cpue_day ~ treatment * rcs(event),
                       data = my_data)

predictions_cpue_d <- my_data |>
  cbind(predict(spline_cpue_d, se.fit = T))

# CPUE sets
spline_cpue_s <- feols(cpue_set ~ treatment * rcs(event),
                       data = my_data)

predictions_cpue_s <- my_data |>
  cbind(predict(spline_cpue_s, se.fit = T))


# plotted from model (example)
ggplot(data = predictions_days,
       aes(x = date, y = fit, color = treatment, fill = treatment)) +
  geom_ribbon(aes(ymin = fit - se.fit, ymax = fit + se.fit), alpha = 0.5) +
  geom_line(linewidth = 1) +
  geom_point(aes(y = effort_day_per_km2)) +
  geom_vline(xintercept = c(announcement, implementation)) +
  geom_hline(yintercept = 0)

# Plot #########################################################################

# Effort Days
p_effort_day_a <- ggplot(data = predictions_days,
  aes(x = date,
    y = fit,
    color = treatment,
    fill = treatment
  )
) +
  geom_ribbon(
    aes(ymin = fit - se.fit, ymax = fit + se.fit),
    alpha = 0.2,
    color = NA
  ) +
  geom_line(linewidth = 1) +
  geom_point(
    aes(y = effort_day_per_km2),
    alpha = 0.6
  ) +
  geom_vline(
    xintercept = announcement,
    linetype = "dashed"
  ) +
  geom_hline(yintercept = 0) +
  labs(
    y = "Effort (days per 10,000 km²)",
    x = "Date",
    color = "Region",
    fill = "Region"
  ) +
  scale_color_manual(
    values = c(
      "FALSE" = "#0072B2",
      "TRUE" = "#D55E00"
    ),
    labels = c(
      "FALSE" = "Control",
      "TRUE" = "Treatment"
    )
  ) +
  scale_fill_manual(
    values = c(
      "FALSE" = "#0072B2",
      "TRUE" = "#D55E00"
    ),
    labels = c(
      "FALSE" = "Control",
      "TRUE" = "Treatment"
    )
  ) +
  theme_classic()

# Sets
p_effort_set_a <- ggplot(
  data = predictions_sets,
  aes(
    x = date,
    y = fit,
    color = treatment,
    fill = treatment
  )
) +
  geom_ribbon(
    aes(ymin = fit - se.fit, ymax = fit + se.fit),
    alpha = 0.2,
    color = NA
  ) +
  geom_line(linewidth = 1) +
  geom_point(
    aes(y = effort_set_per_km2),
    alpha = 0.6
  ) +
  geom_vline(
    xintercept = announcement,
    linetype = "dashed"
  ) +
  geom_hline(yintercept = 0) +
  labs(
    y = "Effort (sets per 10,000 km²)",
    x = "Date",
    color = "Region",
    fill = "Region"
  ) +
  scale_color_manual(
    values = c(
      "FALSE" = "#0072B2",
      "TRUE" = "#D55E00"
    ),
    labels = c(
      "FALSE" = "Control",
      "TRUE" = "Treatment"
    )
  ) +
  scale_fill_manual(
    values = c(
      "FALSE" = "#0072B2",
      "TRUE" = "#D55E00"
    ),
    labels = c(
      "FALSE" = "Control",
      "TRUE" = "Treatment"
    )
  ) +
  theme_classic()


# Catch

p_catch_a <- ggplot(
  data = predictions_catch,
  aes(
    x = date,
    y = fit,
    color = treatment,
    fill = treatment
  )
) +
  geom_ribbon(
    aes(ymin = fit - se.fit, ymax = fit + se.fit),
    alpha = 0.2,
    color = NA
  ) +
  geom_line(linewidth = 1) +
  geom_point(
    aes(y = catch_per_km2),
    alpha = 0.6
  ) +
  geom_vline(
    xintercept = announcement,
    linetype = "dashed"
  ) +
  geom_hline(yintercept = 0) +
  labs(
    y = "Catch (kg per 10,000 km²)",
    x = "Date",
    color = "Region",
    fill = "Region"
  ) +
  scale_color_manual(
    values = c(
      "FALSE" = "#0072B2",
      "TRUE" = "#D55E00"
    ),
    labels = c(
      "FALSE" = "Control",
      "TRUE" = "Treatment"
    )
  ) +
  scale_fill_manual(
    values = c(
      "FALSE" = "#0072B2",
      "TRUE" = "#D55E00"
    ),
    labels = c(
      "FALSE" = "Control",
      "TRUE" = "Treatment"
    )
  ) +
  theme_classic()

# CPUE Days

p_cpue_day_a <- ggplot(
  data = predictions_cpue_d,
  aes(
    x = date,
    y = fit,
    color = treatment,
    fill = treatment
  )
) +
  geom_ribbon(
    aes(ymin = fit - se.fit, ymax = fit + se.fit),
    alpha = 0.2,
    color = NA
  ) +
  geom_line(linewidth = 1) +
  geom_point(
    aes(y = cpue_day),
    alpha = 0.6
  ) +
  geom_vline(
    xintercept = announcement,
    linetype = "dashed"
  ) +
  geom_hline(yintercept = 0) +
  labs(
    y = "CPUE (kg/day)",
    x = "Date",
    color = "Region",
    fill = "Region"
  ) +
  scale_color_manual(
    values = c(
      "FALSE" = "#0072B2",
      "TRUE" = "#D55E00"
    ),
    labels = c(
      "FALSE" = "Control",
      "TRUE" = "Treatment"
    )
  ) +
  scale_fill_manual(
    values = c(
      "FALSE" = "#0072B2",
      "TRUE" = "#D55E00"
    ),
    labels = c(
      "FALSE" = "Control",
      "TRUE" = "Treatment"
    )
  ) +
  theme_classic()


# CPUE sets

p_cpue_set_a <- ggplot(
  data = predictions_cpue_s,
  aes(
    x = date,
    y = fit,
    color = treatment,
    fill = treatment
  )
) +
  geom_ribbon(
    aes(ymin = fit - se.fit, ymax = fit + se.fit),
    alpha = 0.2,
    color = NA
  ) +
  geom_line(linewidth = 1) +
  geom_point(
    aes(y = cpue_set),
    alpha = 0.6
  ) +
  geom_vline(
    xintercept = announcement,
    linetype = "dashed"
  ) +
  geom_hline(yintercept = 0) +
  labs(
    y = "CPUE (kg/set)",
    x = "Date",
    color = "Region",
    fill = "Region"
  ) +
  scale_color_manual(
    values = c(
      "FALSE" = "#0072B2",
      "TRUE" = "#D55E00"
    ),
    labels = c(
      "FALSE" = "Control",
      "TRUE" = "Treatment"
    )
  ) +
  scale_fill_manual(
    values = c(
      "FALSE" = "#0072B2",
      "TRUE" = "#D55E00"
    ),
    labels = c(
      "FALSE" = "Control",
      "TRUE" = "Treatment"
    )
  ) +
  theme_classic()

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






# uses gemo smooth from data
ggplot(data = my_data,
       aes(x = date, y = effort_day_per_km2, color = treatment, fill = treatment)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ rcs(x)) +
  geom_point(aes(y = effort_day_per_km2)) +
  geom_vline(xintercept = c(announcement, implementation)) +
  geom_hline(yintercept = 0)



# Differences ##################################################################

# Calculate day-by-day difference and then fit another spline to this difference

# Example
diff_data_days <- predictions_days |>
  select(date, event, treatment, fit) |>
  pivot_wider(names_from = treatment, values_from = fit) |>
  magrittr::set_colnames(c("date", "event", "control", "treated")) |>
  mutate(change = treated-control)

ggplot(data = diff_data_days,
       aes(x = date, y = change)) +
  geom_line(aes(y = change)) +
  geom_line(linewidth = 2) +
  geom_vline(xintercept = c(announcement, implementation)) +
  geom_hline(yintercept = 0)


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
    se_change = sqrt(se_control^2 + se_treated^2) # test to calculate SE
  )

# Effort sets
diff_data_sets <- predictions_sets |>
  select(date, event, treatment, fit) |>
  pivot_wider(
    names_from = treatment,
    values_from = fit
  ) |>
  magrittr::set_colnames(
    c("date", "event", "control", "treated")
  ) |>
  mutate(
    change = treated - control
  )

# Catch
diff_data_catch <- predictions_catch |>
  select(date, event, treatment, fit) |>
  pivot_wider(
    names_from = treatment,
    values_from = fit
  ) |>
  magrittr::set_colnames(
    c("date", "event", "control", "treated")
  ) |>
  mutate(
    change = treated - control
  )

# CPUE days
diff_data_cpue_d <- predictions_cpue_d |>
  select(date, event, treatment, fit) |>
  pivot_wider(
    names_from = treatment,
    values_from = fit
  ) |>
  magrittr::set_colnames(
    c("date", "event", "control", "treated")
  ) |>
  mutate(
    change = treated - control
  )

# CPUE sets
diff_data_cpue_s <- predictions_cpue_s |>
  select(date, event, treatment, fit) |>
  pivot_wider(
    names_from = treatment,
    values_from = fit
  ) |>
  magrittr::set_colnames(
    c("date", "event", "control", "treated")
  ) |>
  mutate(
    change = treated - control
  )

# Plot treatment-control difference

# Effort days
p_dif_effort_days <- ggplot(
  data = diff_data_days,
  aes(x = date, y = change)
) +
  geom_ribbon(
    aes(
      ymin = change - se_change, # test to add SE
      ymax = change + se_change
    ),
    fill = "grey60",
    alpha = 0.2,
    color = NA
  ) +
  geom_line(
    color = "black",
    linewidth = 1
  ) +
  geom_vline(
    xintercept = announcement,
    linetype = "dashed"
  ) +
  geom_hline(
    yintercept = 0,
    color = "grey60",
    linewidth = 0.4
  ) +
  labs(
    x = "Date",
    y = "Treatment - Control",
    title = ""
  ) +
  theme_classic()



# Effort sets
p_dif_effort_sets <- ggplot(
  data = diff_data_sets,
  aes(x = date, y = change)
) +
  geom_line(
    color = "black",
    linewidth = 1
  ) +
  geom_vline(
    xintercept = announcement,
    linetype = "dashed"
  ) +
  geom_hline(
    yintercept = 0,
    color = "grey60",
    linewidth = 0.4
  ) +
  labs(
    x = "Date",
    y = "Treatment - Control",
    title = ""
  ) +
  theme_classic()


# Catch
p_dif_catch <- ggplot(
  data = diff_data_catch,
  aes(x = date, y = change)
) +
  geom_line(
    color = "black",
    linewidth = 1
  ) +
  geom_vline(
    xintercept = announcement,
    linetype = "dashed"
  ) +
  geom_hline(
    yintercept = 0,
    color = "grey60",
    linewidth = 0.4
  ) +
  labs(
    x = "Date",
    y = "Treatment - Control",
    title = ""
  ) +
  theme_classic()


# CPUE days
p_dif_cpue_d <- ggplot(
  data = diff_data_cpue_d,
  aes(x = date, y = change)
) +
  geom_line(
    color = "black",
    linewidth = 1
  ) +
  geom_vline(
    xintercept = announcement,
    linetype = "dashed"
  ) +
  geom_hline(
    yintercept = 0,
    color = "grey60",
    linewidth = 0.4
  ) +
  labs(
    x = "Date",
    y = "Treatment - Control",
    title = ""
  ) +
  theme_classic()

# CPUE sets
p_dif_cpue_s <- ggplot(
  data = diff_data_cpue_s,
  aes(x = date, y = change)
) +
  geom_line(
    color = "black",
    linewidth = 1
  ) +
  geom_vline(
    xintercept = announcement,
    linetype = "dashed"
  ) +
  geom_hline(
    yintercept = 0,
    color = "grey60",
    linewidth = 0.4
  ) +
  labs(
    x = "Date",
    y = "Treatment - Control",
    title = ""
  ) +
  theme_classic()


# Combine Figures ##############################################################

# Area figures
figure2 <- p_dif_effort_days + p_dif_effort_sets +
  p_dif_cpue_d + p_dif_cpue_s + p_dif_catch + pipa_map_mini +
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
