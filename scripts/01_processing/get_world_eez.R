################################################################################
# Get world EEZ
################################################################################
#
#
################################################################################
# SET UP #######################################################################

## Load packages ---------------------------------------------------------------

library(sf)
library(mregions2)

# PROCESSING ###################################################################

# Get EEZ
world_eez <- mrp_get("eez")

# Break polygons at the antimeridian
world_eez <- st_break_antimeridian(
  world_eez,
  lon_0 = 150
)

# Make geometries valid
world_eez <- st_make_valid(world_eez)

# Simplify global EEZ
world_eez <- st_simplify(
  world_eez,
  dTolerance = 0.1,
  preserveTopology = TRUE
)

# Save processed version
st_write(
  world_eez,
  "data/raw/eez/world_eez.gpkg",
  delete_dsn = TRUE
)
