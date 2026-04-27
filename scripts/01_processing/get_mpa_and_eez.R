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
pipa_search <- gaz_search("Phoenix Islands Protected Area")

# Extract ID
pipa_id <- pipa_search |>
  filter(grepl("Phoenix Islands Protected Area", preferredGazetteerName)) %>%
  pull(MRGID)


# Build sf object (attributes + geometry)
pipa_sf <- st_sf(
  pipa_search |>
    filter(MRGID == pipa_id),
  geometry = gaz_geometry(pipa_id)
)

# Save
st_write(pipa_sf, "data/raw/mpas/pipa.gpkg", delete_dsn = TRUE)

# Possibly: get attributes only
write.csv(
  st_drop_geometry(pipa_sf),
  "data/derived/mpas/pipa_attributes.csv",
  row.names = FALSE
)

############################
# Get Kiribati EEZ: TEST
############################

# Search
kiribati_search <- gaz_search("Kiribati Exclusive Economic Zone")

# Extract IDs (3 EEZ parts)
kiribati_ids <- kiribati_search |>
  filter(grepl("Kiribati Exclusive Economic Zone", preferredGazetteerName)) |>
  pull(MRGID)

# Build sf object (attributes + geometry)
kiribati_sf <- st_sf(
  kiribati_search |>
    filter(MRGID %in% kiribati_ids),
  geometry = gaz_geometry(kiribati_ids)
)

# Combine into single EEZ polygon
kiribati_sf <- kiribati_sf |>
  summarise(geometry = st_union(geometry))

# Save spatial file
st_write(kiribati_sf, "data/raw/eez/kiribati_eez.gpkg", delete_dsn = TRUE)

# Possibly: get attributes only
write.csv(
  st_drop_geometry(kiribati_sf),
  "data/raw/eez/kiribati_eez_attributes.csv",
  row.names = FALSE
)

# To obtain separately:#########################################################

# Keep only EEZ entries
kiribati_eez_parts <- kiribati_search |>
  filter(placeType == "EEZ") |>
  select(MRGID, preferredGazetteerName)

# Get geometries
kiribati_eez_parts <- st_sf(
  kiribati_eez_parts,
  geometry = gaz_geometry(kiribati_eez_parts$MRGID)
)

# Gilbert Islands
st_write(
  kiribati_eez_parts |> filter(MRGID == 8488),
  "data/raw/eez/kiribati_eez_gilbert.gpkg",
  delete_dsn = TRUE
)

# Phoenix Islands
st_write(
  kiribati_eez_parts |> filter(MRGID == 8450),
  "data/raw/eez/kiribati_eez_phoenix.gpkg",
  delete_dsn = TRUE
)

# Line Islands
st_write(
  kiribati_eez_parts |> filter(MRGID == 8441),
  "data/raw/eez/kiribati_eez_line.gpkg",
  delete_dsn = TRUE
)
