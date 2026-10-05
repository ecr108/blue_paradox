################################################################################
# Model Revillagigedo
################################################################################
#
# Emily Rodriguez
# ecr108@miami.edu
#
# This script models the cleaned data for the Revillagigedo blue paradox analysis.
#
################################################################################

# SET UP #######################################################################

## Load packages ---------------------------------------------------------------

library(tidyverse)
library(fixest)
library(modelsummary)
library(sf)
library(gt)

## Load data -------------------------------------------------------------------

# Filtered RFMO Data
revilla_data <- readRDS("data/processed/revilla_rasterized.rds")

## Prepare data ----------------------------------------------------------------

# Set temporal boundaries

start_pre      <- as.Date("2016-09-17")  # One year before announcement
announcement   <- as.Date("2017-10-05")  # Date of announcement
implementation <- as.Date("2017-11-27")  # Implementation date (from MPA database)

# Test Analysis ################################################################

# Unit of observation is the region --------------------------------------------

# Change catch_tot to kg
revilla_data_kg <- revilla_data |>
  mutate(catch_tot = catch_tot * 1000)


# Create region-level data
region_df <- revilla_data_kg |>
  group_by(treatment, date, region_area) |>
  summarise(
    catch_sum = sum(catch_tot, na.rm = TRUE),
    effort_set_sum = sum(effort_set, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    cpue_set = catch_sum / effort_set_sum
  )

region_std_df <- region_df |>
  mutate(
    catch_per_km2 = catch_sum / (region_area / 1e4), # catch per 10,000 km2
    effort_set_per_km2 = effort_set_sum / (region_area / 1e4)
  )

# Create estimation panel at the region level
region_panel <- region_std_df |>
  st_drop_geometry() |>
  filter(date < implementation) |>
  mutate(post      = 1 * (date >= announcement & date < implementation))


# BACI models

m_region_log <- feols(
  c(
    log(effort_set_per_km2),
    log(catch_per_km2),
    log(cpue_set)
  ) ~ treatment * post,
  data = region_panel
)

saveRDS(m_region_log, "data/output/revilla/m_region_log.rds")
