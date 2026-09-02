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
gala_search <- gaz_search("Galápagos Islands")

# Extract ID
gala_id <- gala_search |>
  filter(MRGID == 26845) |>
  pull(MRGID)

# Build sf object
gala_sf <- st_sf(
  gala_search |> filter(MRGID == gala_id),
  geometry = gaz_geometry(gala_id)
)

# Save
st_write(gala_sf, "data/raw/mpa/galapagos_world_heritage.gpkg", delete_dsn = TRUE)

#########################
# Ecuador EEZ
#########################

# Search
ecuador_search <- gaz_search("Ecuadorian Exclusive Economic Zone")

# Mainland EEZ
ecuador_id <- 8431

# Build sf
ecuador_sf <- st_sf(
  ecuador_search |> filter(MRGID == ecuador_id),
  geometry = gaz_geometry(ecuador_id)
)

st_write(ecuador_sf, "data/raw/eez/ecuador_eez_main.gpkg", delete_dsn = TRUE)

# Galapagos EEZ
gala_eez_id <- 8403

# Build sf
gala_eez_sf <- st_sf(
  ecuador_search |> filter(MRGID == gala_eez_id),
  geometry = gaz_geometry(gala_eez_id)
)

# Save
st_write(gala_eez_sf, "data/raw/eez/ecuador_eez_galapagos.gpkg", delete_dsn = TRUE)
