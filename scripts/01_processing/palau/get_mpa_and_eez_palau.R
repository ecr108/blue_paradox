################################################################################
# Retrieve MPA sf
################################################################################
#
# This script details how to obtain and save MPA shape files from the R package
# mregions2
#
################################################################################

library(mregions2)
library(sf)
library(tidyverse)

# Search
palau_search <- gaz_search("PNMS")

# Extract ID
palau_id <- palau_search |>
  filter(MRGID == 8315) |>
  pull(MRGID)

# Build sf object
palau_sf <- st_sf(
  palau_search |> filter(MRGID == palau_id),
  geometry = gaz_geometry(palau_id)
)

# Save
st_write(palau_sf, "data/raw/eez/palau_eez.gpkg", delete_dsn = TRUE)

# Palau National Marine Sanctuary (2020 Updated) -------------------------------

pnms <- st_read("data/raw/mpa/palau/PNMS.shp")


st_write(pnms, "data/raw/mpa/pnms_2020.gpkg")

# PNMS original 2015 -----------------------------------------------------------
pnms_og <- st_read("data/raw/mpa/palau/palau_2015/WDPA_WDOECM_Sep2026_Public_555622118_shp-polygons.shp")

st_write(pnms_og, "data/raw/mpa/pnms_2015.gpkg")

# Control region option 2: FSM -------------------------------------------------

# Search
fsm_search <- gaz_search("Micronesia")

fsm_id <- fsm_search |>
  filter(MRGID == 8316) |>
  pull(MRGID)

fsm_sf <- st_sf(
  fsm_search |> filter(MRGID == fsm_id),
  geometry = gaz_geometry(fsm_id)
)

# Save
st_write(fsm_sf, "data/raw/eez/fsm_eez.gpkg", delete_dsn = TRUE)


#
