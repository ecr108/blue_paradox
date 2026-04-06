################################################################################
# Retrieve MPA sf
################################################################################
#
# Your Name Here
# Your email here
# date
#
# This script details how to obtain and save MPA shape fiels from the R package
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
