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

pipa <- st_read("data/raw/mpas/pipa.gpkg")  # do I need to set coordinates?

k_eez <- st_read("data/raw/eez/kiribati_eez.gpkg")

line_eez <- st_read("data/raw/eez/kiribati_eez_line.gpkg")

phoenix_eez <- st_read("data/raw/eez/kiribati_eez_phoenix.gpkg")

gilbert_eez <- st_read("data/raw/eez/kiribati_eez_gilbert.gpkg")

# need to crop tuna data within region
tuna <- readRDS("data/raw/rfmo/allrfmo_month_1deg_purseseine.rds") |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) |> # check, is geometry needed?
  mutate(
    treatment = as.integer(
      st_intersects(geometry, pipa, sparse = FALSE)
    )
  )

# PROCESSING ###################################################################

## Some step -------------------------------------------------------------------
mapview(line_eez) +
  mapview(pipa)


# VISUALIZE ####################################################################

## Another step ----------------------------------------------------------------

# ANALYSIS #####################################################################

## Almost last step ------------------------------------------------------------


# EXPORT #######################################################################

## The final step --------------------------------------------------------------

