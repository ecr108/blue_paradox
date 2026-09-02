################################################################################
# Retrieve MPA sf
################################################################################
#
# Your Name Here
# Your email here
# date
#
# This script details how to obtain and save MPA shape files from the R package
# mregions2
#
################################################################################

library(mregions2)
library(sf)
library(tidyverse)

# Search
revilla_search <- gaz_search("Revillagigedo")

# Extract ID
revilla_id <- revilla_search |>
  filter(MRGID == 64215) |>
  pull(MRGID)

# Build sf object
revilla_sf <- st_sf(
  revilla_search |> filter(MRGID == revilla_id),
  geometry = gaz_geometry(revilla_id)
)


# Save
st_write(revilla_sf, "data/raw/mpa/revillagigedo_park.gpkg", delete_dsn = TRUE)

#########################
# Mexico EEZ
#########################

# Search
mexico_search <- gaz_search("Mexican Exclusive Economic Zone")

# Mainland EEZ
mexico_id <- 8429

# Build sf
mexico_sf <- st_sf(
  mexico_search |> filter(MRGID == mexico_id),
  geometry = gaz_geometry(mexico_id)
)

st_write(mexico_sf, "data/raw/eez/mexico_eez.gpkg", delete_dsn = TRUE)

