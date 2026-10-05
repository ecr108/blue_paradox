################################################################################
# Create presentation figures
################################################################################
#
################################################################################

# SET UP #######################################################################

## Load packages ---------------------------------------------------------------

pacman::p_load(
  tidyverse,
  sf,
  mapview,
  lwgeom,
  viridis,
  terra,
  ggspatial,
  rnaturalearth,
  rnaturalearthdata,
  patchwork,
  mregions2
)

## Load data -------------------------------------------------------------------

# MPAs and world data
world <- ne_countries(scale = "medium", returnclass = "sf")
world_eez <- st_read("data/raw/eez/world_eez.gpkg") |>
  filter(pol_type == "200NM") # only main EEZ, no intersecting boundaries or treaty agreements


# Create world map of mpas
global_map <- ggplot() +

  geom_sf(
    data = world,
    fill = "grey85",
    color = "grey50",
    linewidth = 0.15
  ) +

  geom_sf(
    data = world_eez,
    fill = NA,
    linewidth = 0.15
  ) +
  theme_void() +
  labs(
    title = "World MPA Coverage"
  )

global_map



ggsave("results/figures/presentation/world_eez_map.png",
       plot = global_map,
       width = 8,
       height = 3,
       dpi = 300)
