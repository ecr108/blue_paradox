################################################################################
# Model Palau Analysis
################################################################################
#
# Emily Rodriguez
# ecr108@miami.edu
#
# This script models the cleaned data for the Palau blue paradox analysis.
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
palau_data <- readRDS("data/processed/palau_rasterized.rds")

## Prepare data ----------------------------------------------------------------

# Set temporal boundaries
start_pre      <- as.Date("2014-10-22")  # 1 year before 2015 announcement
announcement   <- as.Date("2015-10-22")  # Announcement of Palau (per MPA dataset)
implementation <- as.Date("2020-01-01")  # Implementation of Palau

# Test Analysis ################################################################

# Unit of observation is the region --------------------------------------------

# Change catch_tot to kg
palau_data_kg <- palau_data |>
  mutate(catch_tot = catch_tot * 1000)


# Create region-level data
region_df <- palau_data_kg |>
  group_by(treatment, date, region_area) |>
  summarise(
    catch_sum = sum(catch_tot, na.rm = TRUE),
    effort_day_sum = sum(effort_day, na.rm = TRUE),
    effort_set_sum = sum(effort_set, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    cpue_day = catch_sum / effort_day_sum,
    cpue_set = catch_sum / effort_set_sum
  )

region_std_df <- region_df |>
  mutate(
    catch_per_km2 = catch_sum / (region_area / 1e4), # catch per 10,000 km2
    effort_day_per_km2 = effort_day_sum / (region_area / 1e4),
    effort_set_per_km2 = effort_set_sum / (region_area / 1e4)
  )

# Create estimation panel at the region level
region_panel <- region_std_df |>
  st_drop_geometry() |>
  filter(date < implementation) |>
  mutate(post = 1 * (date >= announcement & date < implementation)
  )

# BACI models

m_region_log <- feols(
  c(log(effort_day_per_km2),
    log(effort_set_per_km2),
    log(catch_per_km2),
    log(cpue_day),
    log(cpue_set)
  ) ~ treatment * post,
  data = region_panel
)

saveRDS(m_region_log, "data/output/palau/m_region_log.rds")
