################################################################################
# Find Revilla Control Region
################################################################################

# Load packages ----------------------------------------------------------------
library(sf)
library(dplyr)
library(ggplot2)
library(mapview)

# Load data --------------------------------------------------------------------

tuna_polys <- readRDS("data/processed/unfiltered_blue_paradox_revilla.rds")

# Spatial data
revilla <- st_read("data/raw/mpa/revillagigedo_park.gpkg")
mexico_eez <- st_read("data/raw/eez/mexico_eez.gpkg")

# Check data for Revilla announcement/anticipation period ##########################

# OPTION 1 #####################################################################
# All eez within 200 nm of the mpa boundary

# Define buffer  ---------------------------------------------------------------

# 200 nm buffer in meters
buffer_200nm <- 200 * 1852

revilla_proj <- revilla |>
  st_make_valid() |>
  st_transform(6933)

revilla_buffer <- revilla_proj |>
  st_buffer(dist = buffer_200nm)

# Define control region --------------------------------------------------------

# Make Mexico EEZ valid and project it for spatial operations
mexico_eez_proj <- mexico_eez |>
  st_make_valid() |>
  st_transform(6933)

# Control region = Mexico EEZ within 200 nm of Revillagigedo

control_region_m <- mexico_eez_proj |>
  st_intersection(revilla_buffer)

# Transform control region back to tuna-cell CRS
control_region <- control_region_m |>
  st_transform(st_crs(tuna_polys))


# Define treatment cells -------------------------------------------------------

# Treatment cells
#  - intersect revilla
#  - have centroid inside revilla

tuna_polys$touches_revilla <- st_intersects(
  tuna_polys,
  revilla,
  sparse = FALSE
)[, 1]

centroids <- st_centroid(tuna_polys)

tuna_polys$centroid_inside_revilla <- st_within(
  centroids,
  revilla,
  sparse = FALSE
)[, 1]

# Define control cells rules ---------------------------------------------------

# Control cells
#  - intersect the control region buffer
#  - have their centroid inside the region

tuna_polys$touches_control <- st_intersects(
  tuna_polys,
  control_region,
  sparse = FALSE
)[, 1]

tuna_polys$centroid_inside_control <- st_within(
  centroids,
  control_region,
  sparse = FALSE
)[, 1]

# Remove cells that do not follow the above definitions ------------------------

# (keeps everything except border cells whose centroid falls outside the area they touch)
tuna_polys_rm <- tuna_polys |>
  filter(
    !(touches_revilla & !centroid_inside_revilla) &
      !(touches_control & !centroid_inside_control)
  )

# Calculate treatment and control areas
treatment_area <- revilla_proj |>
  st_area() |>
  sum() |>
  as.numeric() / 1e6 # km2

control_area <- control_region |>
  st_transform(6933) |>
  st_area() |>
  sum() |>
  as.numeric() / 1e6 # km2

# There is a duplicate cell in the Mexico EEZ ----------------------------------
mexico_eez <- st_make_valid(mexico_eez)

# Filter -----------------------------------------------------------------------
tuna_region <- tuna_polys_rm |>
  mutate(
    in_revilla = as.integer(st_intersects(cell, revilla, sparse = FALSE)),
    in_control = as.integer(st_intersects(cell, control_region, sparse = FALSE)),

    # Treatment = inside Revilla
    # Control = inside Mexico EEZ
    treatment = case_when(
      in_revilla == 1 ~ "treatment",
      in_control == 1 ~ "control",
      TRUE ~ "other"
    ),
    date = as.Date(paste(year, month, "01", sep = "-")) # this adds 01 as day to date in set, not sure if this would be correct approach
  ) |>

  # Add column of region area
  mutate(
    region_area = case_when(
      treatment == "treatment" ~ treatment_area,
      treatment == "control"   ~ control_area
    )
  ) |>
  # Filter for revilla time range
  filter(year >= 2015, year <= 2020,
         treatment %in% c("treatment", "control")) |>
  # Standardize
  mutate(
    effort_density = effort_set / region_area,
    catch_density  = catch_tot / region_area
  )


tuna_region_2 <- tuna_polys_rm |>
  filter(
    !(lon == -109.5 & lat == 17.5),
    !(lon == -109.5 & lat == 18.5),
    !(lon == -109.5 & lat == 19.5),
    !(lon == -109.5 & lat == 20.5)
  ) |>
  mutate(
    in_revilla = as.integer(st_intersects(cell, revilla, sparse = FALSE)),
    in_control = as.integer(st_intersects(cell, control_region, sparse = FALSE)),

    # Treatment = inside Revilla
    # Control = inside Mexico EEZ
    treatment = case_when(
      in_revilla == 1 ~ "treatment",
      in_control == 1 ~ "control",
      TRUE ~ "other"
    ),
    date = as.Date(paste(year, month, "01", sep = "-")) # this adds 01 as day to date in set, not sure if this would be correct approach
  ) |>

  # Add column of region area
  mutate(
    region_area = case_when(
      treatment == "treatment" ~ treatment_area,
      treatment == "control"   ~ control_area
    )
  ) |>
  # Filter for revilla time range
  filter(year >= 2015, year <= 2020,
         treatment %in% c("treatment", "control")) |>
  # Standardize
  mutate(
    effort_density = effort_set / region_area,
    catch_density  = catch_tot / region_area
  )


# COMPARE

effort_comparison <- tuna_region |>
  st_drop_geometry() |>
  group_by(date) |>
  summarise(
    effort_v1 = sum(effort_set, na.rm = TRUE),
    .groups = "drop"
  ) |>
  left_join(
    tuna_region_2 |>
      st_drop_geometry() |>
      group_by(date) |>
      summarise(
        effort_v2 = sum(effort_set, na.rm = TRUE),
        .groups = "drop"
      ),
    by = "date"
  ) |>
  mutate(
    difference = effort_v1 - effort_v2,
  )

effort_comparison

effort_comparison |>
  summarise(
    total_effort_v1 = sum(effort_v1, na.rm = TRUE),
    total_effort_v2 = sum(effort_v2, na.rm = TRUE),
    total_removed = sum(difference, na.rm = TRUE)
  )

# Keep version one, plots do not change when removing cells

# PLOTS ########################################################################

# Mapview
mapview(tuna_region, zcol = "treatment") +
  mapview(revilla, col.regions = "red", alpha.regions = 0.3) +
  mapview(mexico_eez, col.regions = "blue", alpha.regions = 0.2)


mapview(tuna_region_2, zcol = "treatment") +
  mapview(revilla, col.regions = "red", alpha.regions = 0.3) +
  mapview(mexico_eez, col.regions = "blue", alpha.regions = 0.2)

# Aggregate by month/region
tuna_monthly <- tuna_region |>
  group_by(date, treatment) |>
  summarise(
    effort_density = sum(effort_density, na.rm = TRUE),
    catch_density  = sum(catch_density, na.rm = TRUE),
    cpue_set = if_else(
      sum(effort_set, na.rm = TRUE) > 0,
      sum(catch_tot, na.rm = TRUE) / sum(effort_set, na.rm = TRUE),
      NA_real_
    ),
    .groups = "drop"
  )

# Plot effort density

ggplot(tuna_monthly, aes(x = date, y = effort_density,
                         color = treatment)) +
  geom_line(linewidth = 0.7) +
  geom_vline(xintercept = as.Date("2017-11-24"),
             linetype = "dashed", color = "black") +
  geom_vline(xintercept = as.Date("2018-11-24"),
             linetype = "dotted", color = "black") +
  annotate("text", x = as.Date("2017-11-24"), y = max(tuna_monthly$effort_density, na.rm = TRUE),
           label = "MPA declared (2017)", hjust = -0.1, vjust = -0.5, size = 3) +
  annotate("text", x = as.Date("2018-11-24"), y = max(tuna_monthly$effort_density, na.rm = TRUE),
           label = "One year post implementation", hjust = -0.1, vjust = -0.5, size = 3) +
  labs(
    title = "Fishing Effort Density (sets)",
    caption = "Mexican EEZ 200 km buffer",
    y = "Effort density (sets per km²)",
    x = "Date",
    color = "Region"
  ) +
  theme_minimal(base_size = 14)


# Aggregate by month/region
tuna_monthly_2 <- tuna_region |>
  group_by(date, treatment) |>
  summarise(
    effort_density = sum(effort_density, na.rm = TRUE),
    catch_density  = sum(catch_density, na.rm = TRUE),
    cpue_set = if_else(
      sum(effort_set, na.rm = TRUE) > 0,
      sum(catch_tot, na.rm = TRUE) / sum(effort_set, na.rm = TRUE),
      NA_real_
    ),
    .groups = "drop"
  )

# Plot effort density

ggplot(tuna_monthly_2, aes(x = date, y = effort_density,
                         color = treatment)) +
  geom_line(linewidth = 0.7) +
  geom_vline(xintercept = as.Date("2017-11-24"),
             linetype = "dashed", color = "black") +
  geom_vline(xintercept = as.Date("2018-11-24"),
             linetype = "dotted", color = "black") +
  annotate("text", x = as.Date("2017-11-24"), y = max(tuna_monthly$effort_density, na.rm = TRUE),
           label = "MPA declared (2017)", hjust = -0.1, vjust = -0.5, size = 3) +
  annotate("text", x = as.Date("2018-11-24"), y = max(tuna_monthly$effort_density, na.rm = TRUE),
           label = "One year post implementation", hjust = -0.1, vjust = -0.5, size = 3) +
  labs(
    title = "Fishing Effort Density (sets)",
    caption = "Mexican EEZ 200 km buffer, border cells removed",
    y = "Effort density (sets per km²)",
    x = "Date",
    color = "Region"
  ) +
  theme_minimal(base_size = 14)

# Plot catch

ggplot(tuna_monthly, aes(x = date, y = catch_density,
                         color = treatment)) +
  geom_line(linewidth = 0.7) +
  geom_vline(xintercept = as.Date("2017-11-24"),
             linetype = "dashed", color = "black") +
  geom_vline(xintercept = as.Date("2018-11-24"),
             linetype = "dotted", color = "black") +
  annotate("text", x = as.Date("2017-11-24"), y = max(tuna_monthly$catch_density, na.rm = TRUE),
           label = "MPA declared (1998)", hjust = -0.1, vjust = -0.5, size = 3) +
  annotate("text", x = as.Date("2018-11-24"), y = max(tuna_monthly$catch_density, na.rm = TRUE),
           label = "Enforcement begins (2002)", hjust = -0.1, vjust = -0.5, size = 3) +
  labs(
    title = "Fishing Catch Density (Treatment vs Control), 1992–2008",
    y = "Catch density (mt)",
    x = "Date",
    color = "Region"
  ) +
  theme_minimal(base_size = 14)

# Plot CPUE

ggplot(tuna_monthly, aes(x = date, y = cpue_set,
                         color = treatment)) +
  geom_line(linewidth = 0.7) +
  geom_vline(xintercept = as.Date("2017-11-24"),
             linetype = "dashed", color = "black") +
  geom_vline(xintercept = as.Date("2018-11-24"),
             linetype = "dotted", color = "black") +
  annotate("text", x = as.Date("2017-11-24"), y = max(tuna_monthly$cpue_set, na.rm = TRUE),
           label = "MPA declared (1998)", hjust = -0.1, vjust = -0.5, size = 3) +
  annotate("text", x = as.Date("2018-11-24"), y = max(tuna_monthly$cpue_set, na.rm = TRUE),
           label = "Enforcement begins (2002)", hjust = -0.1, vjust = -0.5, size = 3) +
  labs(
    title = "CPUE, sets (Treatment vs Control), 1992–2008",
    y = "CPUE (catch / effort)",
    x = "Date",
    color = "Region"
  ) +
  theme_minimal(base_size = 14)
