################################################################################
# Recreate PIPA analysis - filtered
################################################################################
#
# Emily Rodriguez
# ecr108@miami.edu
#
# Recreation of blue paradox analysis in PIPA region using tuna RFMO catch and
# effort data at the 1 x 1 degree, monthly resolution. This analysis crops catch
# and effort cells to be only within PIPA region. When excluding border cells,
# data is too limited for analysis.
#
################################################################################
# SET UP #######################################################################
# Load packages ---------------------------------------------------------------

library(tidyverse)
library(sf)
library(mapview)

################################################################################
# Recreate with PIPA Analysis: Sample 1: leaves some cells on border
################################################################################

## Load data -------------------------------------------------------------------

# Spatial data
pipa <- st_read("data/raw/mpas/pipa.gpkg")
k_eez <- st_read("data/raw/eez/kiribati_eez.gpkg")
line_eez <- st_read("data/raw/eez/kiribati_eez_line.gpkg")
phoenix_eez <- st_read("data/raw/eez/kiribati_eez_phoenix.gpkg")
gilbert_eez <- st_read("data/raw/eez/kiribati_eez_gilbert.gpkg")

# Tuna data
tuna <- readRDS("data/raw/rfmo/allrfmo_month_1deg_purseseine.rds")


# PROCESSING ###################################################################

## Create 1°×1° grid cells around each tuna point ------------------------------
# Build function to create a square polygon around lat/lon points
make_cell <- function(lon, lat) {
  st_polygon(list(rbind(
    c(lon - 0.5, lat - 0.5),
    c(lon + 0.5, lat - 0.5),
    c(lon + 0.5, lat + 0.5),
    c(lon - 0.5, lat + 0.5),
    c(lon - 0.5, lat - 0.5)
  )))
}

tuna_polys <- tuna |>
  # Apply make_cell() to each lat/lon pair
  mutate(cell = purrr::map2(lon, lat, make_cell)) |>
  # Turn into sf geometry column with correct coordinate system
  mutate(cell = st_sfc(cell, crs = 4326)) |>
  # Convert to sef object
  st_as_sf(sf_column_name = "cell")


# Remove border cells ----------------------------------------------------------

## Identify cells that touch PIPA
tuna_polys$touches_pipa <- st_intersects(
  tuna_polys,
  pipa,
  sparse = FALSE
)[,1]

# Define center point of each grid cell
centroids <- st_centroid(tuna_polys)

# Identify cells whose centroid is inside PIPA
tuna_polys$centroid_inside_pipa <- st_within(
  centroids,
  pipa,
  sparse = FALSE
)[,1]

# Remove cells on border (touches or overlaps PIPA, but centroid outside):
tuna_clean <- tuna_polys |>
  filter(!(touches_pipa == TRUE & centroid_inside_pipa == FALSE))

# Define temporal boundaries ---------------------------------------------------

start_pre      <- as.Date("2012-09-01")
announcement   <- as.Date("2013-09-01")
implementation <- as.Date("2015-01-01")
end_post       <- as.Date("2016-01-01")

# Define spatial areas ---------------------------------------------------------

pipa_area <- pipa |>
  st_transform(6933) |>  # Use equal area projection
  st_area() |>
  as.numeric() / 1e6     # Convert from m to km squared

control_area <- st_union(
  st_transform(line_eez, 6933),
  st_transform(gilbert_eez, 6933)
) |>
  st_area() |>
  as.numeric() / 1e6


# Filter data ##################################################################

tuna_region_1 <- tuna_clean |>

  # Assign each grid cell to a region
  mutate(
    in_pipa    = as.integer(st_intersects(cell, pipa, sparse = FALSE)),
    in_line    = as.integer(st_intersects(cell, line_eez, sparse = FALSE)),
    in_gilbert = as.integer(st_intersects(cell, gilbert_eez, sparse = FALSE)),

    # Treatment = inside PIPA
    # Control = inside Line Islands and Gilbert EEZ
    treatment = case_when(
      in_pipa == 1 ~ "treatment",
      in_line == 1 | in_gilbert == 1 ~ "control",
      TRUE ~ "other"
    ),

    # Create date variable
    date = as.Date(paste(year, month, "01", sep = "-"))
  ) |>

  # Assign the correct area to each group
  mutate(
    region_area = case_when(
      treatment == "treatment" ~ pipa_area,
      treatment == "control"   ~ control_area
    )
  ) |>

  # Remove points outside of study region and outside study time frame
  filter(
    treatment %in% c("treatment", "control"),
    date >= start_pre,
    date <= end_post
  ) |>

  # Split data into time periods
  mutate(
    period = case_when(
      date < announcement ~ "pre",
      date >= announcement & date < implementation ~ "anticipation",
      date >= implementation ~ "post"
    ),

    # Compute CPUE
    cpue_day = ifelse(effort_day > 0, catch_tot / effort_day, NA),
    cpue_set = ifelse(effort_set > 0, catch_tot / effort_set, NA)
  ) |>

  # Arrange data by treatment and date
  arrange(treatment, date)


# VISUALIZE ####################################################################

mapview(tuna_region_1, zcol = "treatment") +
  mapview(pipa, col.regions = "red", alpha.regions = 0.3) +
  mapview(k_eez, col.regions = "blue", alpha.regions = 0.2)

# Plot catch time series
ggplot(tuna_region_1, aes(x = date, y = catch_tot, color = treatment)) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  geom_vline(xintercept = (announcement), linetype = "dashed") +
  geom_vline(xintercept = (implementation),
             linetype = "dashed", color = "red") +
  labs(y = "Catch (tons)", x = "Date") +
  theme_minimal()


# Plot effort (days) time series
ggplot(tuna_region_1, aes(x = date, y = effort_day, color = treatment)) +
  stat_summary(fun = mean,geom = "line") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  geom_vline(xintercept = (announcement), linetype = "dashed") +
  geom_vline(xintercept = (implementation),
             linetype = "dashed", color = "red") +
  labs(y = "Effort (days)", x = "Date") +
  theme_minimal()


# Plot effort (sets) time series
ggplot(tuna_region_1, aes(x = date, y = effort_set, color = treatment)) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  geom_vline(xintercept = (announcement), linetype = "dashed") +
  geom_vline(xintercept = (implementation),
             linetype = "dashed", color = "red") +
  labs(y = "Effort (sets)", x = "Date") +
  theme_minimal()

# Plot CPUE (tons per day) time series
ggplot(tuna_region_1, aes(x = date, y = cpue_day, color = treatment)) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  geom_vline(xintercept = (announcement), linetype = "dashed") +
  geom_vline(xintercept = (implementation),
             linetype = "dashed", color = "red") +
  labs(y = "CPUE (tons/day)", x = "Date") +
  theme_minimal()

# Plot CPUE (tons per set) time series
ggplot(tuna_region_1, aes(x = date, y = cpue_set, color = treatment)) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  geom_vline(xintercept = (announcement), linetype = "dashed") +
  geom_vline(xintercept = (implementation),
             linetype = "dashed", color = "red") +
  labs(y = "CPUE (tons/set)", x = "Date") +
  theme_minimal()


################################################################################
#
# Trial 2: Crop any cell outside (one straggler)
#
################################################################################


# Build 1 degree cells----------------------------------------------------------
# Build function to create a square polygon around lat/lon points
make_cell <- function(lon, lat) {
  st_polygon(list(rbind(
    c(lon - 0.5, lat - 0.5),
    c(lon + 0.5, lat - 0.5),
    c(lon + 0.5, lat + 0.5),
    c(lon - 0.5, lat + 0.5),
    c(lon - 0.5, lat - 0.5)
  )))
}

tuna_polys <- tuna |>
  # Apply make_cell() to each lat/lon pair
  mutate(cell = map2(lon, lat, make_cell)) |>
  # Turn into sf geometry column with correct coordinate system
  mutate(cell = st_sfc(cell, crs = 4326)) |>
  # Convert into sf object
  st_as_sf(sf_column_name = "cell")


# Remove border cells in PIPA --------------------------------------------------

# Cells that touch PIPA
tuna_polys$touches_pipa <- st_intersects(
  tuna_polys,
  pipa,
  sparse = FALSE
)[,1]

# Cells whose centroid is inside PIPA
# Find centroid
centroids <- st_centroid(tuna_polys)

tuna_polys$centroid_inside_pipa <- st_within(
  centroids,
  pipa,
  sparse = FALSE
)[,1]

# Remove border cells
tuna_clean <- tuna_polys |>
  filter(!(touches_pipa == TRUE & centroid_inside_pipa == FALSE))


################################################################################
# Remove overlap cells
################################################################################

# Rebuild geometry column
tuna_clean <- tuna_clean |>
  mutate(cell = st_geometry(cell)) |>
  st_as_sf(sf_column_name = "cell")

# Transform to equal-area CRS
tuna_area   <- tuna_clean |>
  st_transform(6933) |>
  st_make_valid()
pipa_area <- pipa |>
  st_transform(6933) |>
  st_make_valid()
phoenix_area <- phoenix_eez |>
  st_transform(6933) |>
  st_make_valid()

# Compute Phoenix EEZ outside PIPA (unprotected area)
phoenix_outside <- st_difference(phoenix_area, pipa_area) |>
  st_make_valid()

# Change into single shape
phoenix_outside_geom <- st_union(phoenix_outside)

# Identify tuna cells that intersect Phoenix outside region
idx <- st_intersects(tuna_area, phoenix_outside_geom, sparse = FALSE)[,1]

# Compute overlap area only for each intersecting cell
tuna_area$phoenix_overlap_area <- 0 # no overlap default

tuna_area$phoenix_overlap_area[idx] <- as.numeric(
  st_area(
    st_intersection(tuna_area[idx, ], phoenix_outside_geom)
  )
)

# Keep only cells with no overlap into Phoenix outside region
tuna_clean2 <- tuna_area |>
  filter(phoenix_overlap_area == 0) |> # Can change to partial overlap if needed
  st_transform(4326)


# Define temporal boundaries ---------------------------------------------------

start_pre      <- as.Date("2012-09-01")
announcement   <- as.Date("2013-09-01")
implementation <- as.Date("2015-01-01")
end_post       <- as.Date("2016-01-01")


# Define spatial areas ---------------------------------------------------------

pipa_area <- pipa |>
  st_transform(6933) |>
  st_area() |>
  as.numeric() / 1e6

control_area <- st_union(
  st_transform(line_eez, 6933),
  st_transform(gilbert_eez, 6933)
) |>
  st_area() |>
  as.numeric() / 1e6


# Filter data ###################################################################

tuna_region_2 <- tuna_clean2 |>

  # Assign each grid cell to a region
  mutate(
    in_pipa    = as.integer(st_intersects(cell, pipa,        sparse = FALSE)),
    in_line    = as.integer(st_intersects(cell, line_eez,    sparse = FALSE)),
    in_gilbert = as.integer(st_intersects(cell, gilbert_eez, sparse = FALSE)),

    # Treatment = inside PIPA
    # Control = inside Line Islands and Gilbert EEZ
    treatment = case_when(
      in_pipa == 1 ~ "treatment",
      in_line == 1 | in_gilbert == 1 ~ "control",
      TRUE ~ "other"
    ),

    # Create date variable
    date = as.Date(paste(year, month, "01", sep = "-"))
  ) |>

  # Assign correct area to each group
  mutate(
    region_area = case_when(
      treatment == "treatment" ~ pipa_area,
      treatment == "control"   ~ control_area
    )
  ) |>

  # Filter points within time frame
  filter(
    treatment %in% c("treatment", "control"),
    date >= start_pre,
    date <= end_post
  ) |>

  # Add time period tags
  mutate(
    period = case_when(
      date < announcement ~ "pre",
      date >= announcement & date < implementation ~ "anticipation",
      date >= implementation ~ "post"
    ),

    # Calculate CPUE
    cpue_day = ifelse(effort_day > 0, catch_tot / effort_day, NA),
    cpue_set = ifelse(effort_set > 0, catch_tot / effort_set, NA)
  ) |>

  # Arrange by treatment and date
  arrange(treatment, date)

# VISUALIZE ####################################################################

mapview(tuna_region_2, zcol = "treatment") +
  mapview(pipa, col.regions = "red", alpha.regions = 0.3) +
  mapview(k_eez, col.regions = "blue", alpha.regions = 0.2)


# Plot catch time series
ggplot(tuna_region_2, aes(x = date, y = catch_tot, color = treatment)) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  geom_vline(xintercept = (announcement), linetype = "dashed") +
  geom_vline(xintercept = (implementation),
             linetype = "dashed", color = "red") +
  labs(y = "Catch (tons)", x = "Date") +
  theme_minimal()


# Plot effort (days) time series
ggplot(tuna_region_2, aes(x = date, y = effort_day, color = treatment)) +
  stat_summary(fun = mean,geom = "line") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  geom_vline(xintercept = (announcement), linetype = "dashed") +
  geom_vline(xintercept = (implementation),
             linetype = "dashed", color = "red") +
  labs(y = "Effort (days)", x = "Date") +
  theme_minimal()


# Plot effort (sets) time series
ggplot(tuna_region_2, aes(x = date, y = effort_set, color = treatment)) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  geom_vline(xintercept = (announcement), linetype = "dashed") +
  geom_vline(xintercept = (implementation),
             linetype = "dashed", color = "red") +
  labs(y = "Effort (sets)", x = "Date") +
  theme_minimal()

# Plot CPUE (tons per day) time series
ggplot(tuna_region_2, aes(x = date, y = cpue_day, color = treatment)) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  geom_vline(xintercept = (announcement), linetype = "dashed") +
  geom_vline(xintercept = (implementation),
             linetype = "dashed", color = "red") +
  labs(y = "CPUE (tons/day)", x = "Date") +
  theme_minimal()

# Plot CPUE (tons per set) time series
ggplot(tuna_region_2, aes(x = date, y = cpue_set, color = treatment)) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  geom_vline(xintercept = (announcement), linetype = "dashed") +
  geom_vline(xintercept = (implementation),
             linetype = "dashed", color = "red") +
  labs(y = "CPUE (tons/set)", x = "Date") +
  theme_minimal()


# Test Analysis ################################################################

library(fixest)

# Fully cropped
feols(catch_tot ~ i(date, treatment, "2013-09-01") | year ^ month + lat ^ lon,
      data = tuna_region_1 |>
        mutate(
          treatment = 1 * (treatment == "treatment")
        )) |>
  iplot()

# partially cropped
feols(catch_tot ~ i(date, treatment, "2013-09-01") | year ^ month + lat ^ lon,
      data = tuna_region_1 |>
        mutate(
          treatment = 1 * (treatment == "treatment")
        )) |>
  iplot()

# change date 2014 fully cropped
feols(catch_tot ~ i(date, treatment, "2014-01-01") | year ^ month + lat ^ lon,
      data = tuna_region_1 |>
        mutate(
          treatment = 1 * (treatment == "treatment")
        )) |>
  iplot()


# change date 2015 (closed) fully cropped
feols(catch_tot ~ i(date, treatment, "2015-01-01") | year ^ month + lat ^ lon,
      data = tuna_region_1 |>
        mutate(
          treatment = 1 * (treatment == "treatment")
        )) |>
  iplot()
