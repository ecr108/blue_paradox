
################################################################################
# Find GMR Control Region
################################################################################

# Load packages ----------------------------------------------------------------
library(tidyverse)
library(sf)
library(viridis)
library(mapview)

# Load data --------------------------------------------------------------------

tuna_polys <- readRDS("data/processed/unfiltered_blue_paradox_gmr.rds")

# Spatial data
gmr <- st_read("data/raw/mpa/galapagos_world_heritage.gpkg")
e_main_eez <- st_read("data/raw/eez/ecuador_eez_main.gpkg")
e_gal_eez <- st_read("data/raw/eez/ecuador_eez_galapagos.gpkg")

# Check data for GMR announcement/anticipation period ##########################

# Define spatial areas ---------------------------------------------------------
gmr_area <- gmr |>
  st_transform(6933) |>
  st_area() |>
  as.numeric() / 1e6 # convert m2 -> km2

control_area <- e_main_eez |>
  st_transform(6933) |>
  st_area() |>
  as.numeric() / 1e6 # convert m2 -> km2

# Remove border cells ----------------------------------------------------------

# Identify cells that intersect GMR
tuna_polys$touches_gmr <- st_intersects(
  tuna_polys,
  gmr,
  sparse = FALSE
)[, 1]

# Compute centroids
centroids <- st_centroid(tuna_polys)

# Identify cells whose centroid is inside GMR
tuna_polys$centroid_inside_gmr <- st_within(
  centroids,
  gmr,
  sparse = FALSE
)[, 1]

# Remove border cells (this keeps some along the border that slightly crossover)
tuna_polys_rm <- tuna_polys |>
  filter(!(touches_gmr & !centroid_inside_gmr))

# OR ###########################################################################

# Keep only within GMR
inside <- st_within(tuna_polys, gmr, sparse = FALSE)[, 1]

tuna_polys_rm_all <- tuna_polys |>
  filter(inside | !st_intersects(tuna_polys, gmr, sparse = FALSE)[, 1])


# Filter -----------------------------------------------------------------------
tuna_region <- tuna_polys_rm_all |>
  mutate(
    in_gmr = as.integer(st_intersects(cell, gmr, sparse = FALSE)),
    in_eez = as.integer(st_intersects(cell, e_main_eez, sparse = FALSE)),

    # Treatment = inside PIPA
    # Control = inside Line Islands and Gilbert EEZ
    treatment = case_when(
      in_gmr == 1 ~ "treatment",
      in_eez == 1 ~ "control",
      TRUE ~ "other"
    ),
    date = as.Date(paste(year, month, "01", sep = "-")) # this adds 01 as day to date in set, not sure if this would be correct approach
  ) |>

  # Add column of region area
  mutate(
    region_area = case_when(
      treatment == "treatment" ~ gmr_area,
      treatment == "control"   ~ control_area
    )
  ) |>
  # Filter for GMR time range
  filter(year >= 1992, year <= 2008,
         treatment %in% c("treatment", "control")) |>
  # Standardize
  mutate(
    effort_density = effort_set / region_area,
    catch_density  = catch_tot / region_area
  )

mapview(tuna_region, zcol = "treatment") +
  mapview(gmr, col.regions = "red", alpha.regions = 0.3) +
  mapview(e_main_eez, col.regions = "blue", alpha.regions = 0.2)

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
  geom_vline(xintercept = as.Date("1998-03-01"),
             linetype = "dashed", color = "black") +
  geom_vline(xintercept = as.Date("2002-01-01"),
             linetype = "dotted", color = "black") +
  annotate("text", x = as.Date("1998-03-01"), y = max(tuna_monthly$effort_density, na.rm = TRUE),
           label = "MPA declared (1998)", hjust = -0.1, vjust = -0.5, size = 3) +
  annotate("text", x = as.Date("2002-01-01"), y = max(tuna_monthly$effort_density, na.rm = TRUE),
           label = "Enforcement begins (2002)", hjust = -0.1, vjust = -0.5, size = 3) +
  labs(
    title = "Fishing Effort Density (sets) (Treatment vs Control), 1992–2008",
    y = "Effort density (sets per km²)",
    x = "Date",
    color = "Region"
  ) +
  theme_minimal(base_size = 14)

# Plot catch

ggplot(tuna_monthly, aes(x = date, y = catch_density,
                         color = treatment)) +
  geom_line(linewidth = 0.7) +
  geom_vline(xintercept = as.Date("1998-03-01"),
             linetype = "dashed", color = "black") +
  geom_vline(xintercept = as.Date("2002-01-01"),
             linetype = "dotted", color = "black") +
  annotate("text", x = as.Date("1998-03-01"), y = max(tuna_monthly$catch_density, na.rm = TRUE),
           label = "MPA declared (1998)", hjust = -0.1, vjust = -0.5, size = 3) +
  annotate("text", x = as.Date("2002-01-01"), y = max(tuna_monthly$catch_density, na.rm = TRUE),
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
  geom_vline(xintercept = as.Date("1998-03-01"),
             linetype = "dashed", color = "black") +
  geom_vline(xintercept = as.Date("2002-01-01"),
             linetype = "dotted", color = "black") +
  annotate("text", x = as.Date("1998-03-01"), y = max(tuna_monthly$cpue_set, na.rm = TRUE),
           label = "MPA declared (1998)", hjust = -0.1, vjust = -0.5, size = 3) +
  annotate("text", x = as.Date("2002-01-01"), y = max(tuna_monthly$cpue_set, na.rm = TRUE),
           label = "Enforcement begins (2002)", hjust = -0.1, vjust = -0.5, size = 3) +
  labs(
    title = "CPUE, sets (Treatment vs Control), 1992–2008",
    y = "CPUE (catch / effort)",
    x = "Date",
    color = "Region"
  ) +
  theme_minimal(base_size = 14)


# Plot fishing effort leading up to implementation #############################

# Define pre-period

start_pre <- as.Date("1993-01-01")
end_pre   <- as.Date("1997-12-31")


# Create dates

tuna_pre <- tuna_polys |>
  mutate(
    date = as.Date(paste(year, month, "01", sep = "-"))
  ) |>
  filter(
    date >= start_pre,
    date <= end_pre
  )


# Create box around GMR region

box <- st_bbox(c(
  xmin = -110,
  xmax = -70,
  ymin = -20,
  ymax = 10
), crs = st_crs(4326))

box_sf <- st_as_sfc(box)

# Keep only cells inside the box
tuna_pre_box <- st_intersection(tuna_pre, box_sf)


# Aggregate fishing effort by cell

effort_map <- tuna_pre_box |>
  group_by(lon, lat, cell) |>
  summarise(
    total_effort = sum(effort_set, na.rm = TRUE),
    mean_effort  = mean(effort_set, na.rm = TRUE),
    .groups = "drop"
  )


# Plot total fishing effort

ggplot() +
  geom_sf(
    data = effort_map,
    aes(fill = total_effort),
    color = NA
  ) +
  geom_sf(
    data = e_main_eez,
    fill = NA,
    color = "black",
    linewidth = 0.4
  ) +
  geom_sf(
    data = gmr,
    fill = NA,
    color = "red",
    linewidth = 0.8
  ) +
  geom_sf(
    data = e_gal_eez,
    fill = NA,
    color = "white",
    linewidth = 0.8
  ) +
  scale_fill_viridis_c(
    trans = "sqrt",
    na.value = "white"
  ) +
  coord_sf(
    xlim = c(-110, -70),
    ylim = c(-20, 10),
    expand = FALSE
  ) +
  labs(
    title = "Total Purse Seine Fishing Effort (1993–1997)",
    fill = "Total (sets)"
  ) +
  theme_minimal(base_size = 13)
