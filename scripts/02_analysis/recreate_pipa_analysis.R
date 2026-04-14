################################################################################
# Recreate PIPA analysis
################################################################################
#
# Your Name Here
# Your email here
# date
#
# Description
#
################################################################################

# SET UP #######################################################################

## Load packages ---------------------------------------------------------------

library(tidyverse)
library(sf)
library(mapview)

## Load data -------------------------------------------------------------------
# set up connection to GitHub tuna repo?

# Load spatial data
pipa <- st_read("data/raw/mpas/pipa.gpkg")
k_eez <- st_read("data/raw/eez/kiribati_eez.gpkg")
line_eez <- st_read("data/raw/eez/kiribati_eez_line.gpkg")
phoenix_eez <- st_read("data/raw/eez/kiribati_eez_phoenix.gpkg")
gilbert_eez <- st_read("data/raw/eez/kiribati_eez_gilbert.gpkg")

# Load tuna data
## need to crop tuna data within region
tuna <- readRDS("data/raw/rfmo/allrfmo_month_1deg_purseseine.rds") |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) # check, is geometry needed?


# PROCESSING ###################################################################

## Set up data -----------------------------------------------------------------

# Define control and treatment region
## need to consider rule for cells being both inside and outside region

# Temporal boundaries
start_pre      <- as.Date("2012-09-01")  # 1 year before 2013 announcement
announcement   <- as.Date("2013-09-01")
implementation <- as.Date("2015-01-01")
end_post       <- as.Date("2016-01-01")  # 1 year after implementation

# Filter
tuna_region <- tuna |>
  mutate(
    in_pipa = as.integer(st_intersects(geometry, pipa, sparse = FALSE)),
    in_line = as.integer(st_intersects(geometry, line_eez, sparse = FALSE)),
    in_gilbert = as.integer(st_intersects(geometry, gilbert_eez, sparse = FALSE)),

    # Treatment = inside PIPA
    # Control = inside Line Islands and Gilbert EEZ
    treatment = case_when(
      in_pipa == 1 ~ "treatment",
      in_line == 1 | in_gilbert == 1 ~ "control",
      TRUE ~ "other"
    ),
    date = as.Date(paste(year, month, "01", sep = "-")) # this adds 01 as day to date in set, not sure if this would be correct approach
  ) |>
  filter(
    treatment %in% c("treatment", "control"),
    date >= start_pre,
    date <= end_post
  ) |>
  # add tags to time periods
  mutate(
    period = case_when(
      date < announcement ~ "pre",
      date >= announcement & date < implementation ~ "anticipation",
      date >= implementation ~ "post"
    )
  )


# VISUALIZE ####################################################################
mapview(k_eez) +
  mapview(pipa)


mapview(tuna_region, zcol = "treatment") +
  mapview(pipa, col.regions = "red", alpha.regions = 0.3) +
  mapview(k_eez, col.regions = "blue", alpha.regions = 0.2)

mapview(k_eez)

## Another step ----------------------------------------------------------------

# ANALYSIS #####################################################################

## Almost last step ------------------------------------------------------------


# EXPORT #######################################################################

## The final step --------------------------------------------------------------

