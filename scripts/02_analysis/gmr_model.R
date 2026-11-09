################################################################################
# Model GMR Analysis
################################################################################
#
# Emily Rodriguez
# ecr108@miami.edu
#
# This script models the cleaned data for the GMR blue paradox analysis.
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
gmr_data <- readRDS("data/processed/gmr_rasterized.rds")

## Prepare data ----------------------------------------------------------------

# Set temporal boundaries

start_pre      <- as.Date("1997-03-18")  # 1 year before 1998 announcement
announcement   <- as.Date("1998-03-18")  # Announcement and implementation of GMR
implementation <- as.Date("2002-01-01")  # Implementation of enforcement date

# Test Analysis ################################################################

# Unit of observation is the region --------------------------------------------

# Change catch_tot to kg
gmr_data_kg <- gmr_data |>
  mutate(catch_tot = catch_tot * 1000)


# Create region-level data
region_df <- gmr_data_kg |>
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
  data = region_panel,
  vcov = "NW",
  panel.id = ~treatment + date
)

saveRDS(m_region_log, "data/output/gmr/m_region_log.rds")

