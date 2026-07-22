################################################################################
# Model PIPA Analysis
################################################################################
#
# Emily Rodriguez
# ecr108@miami.edu
#
# This script models the cleaned data for the PIPA blue paradox analysis.
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
pipa_data <- readRDS("data/processed/blue_paradox_pipa.rds")

## Prepare data ----------------------------------------------------------------

# Set temporal boundaries
start_pre      <- as.Date("2012-09-01")  # 1 year before 2013 announcement
announcement   <- as.Date("2013-09-01")  # Announcement online defined by PIPA paper
implementation <- as.Date("2015-01-01")  # Implementation date

# Test Analysis ################################################################

# Unit of observation is pixel by month ----------------------------------------

# Create estimation panel
estimation_panel <- pipa_data |>
  st_drop_geometry() |>
  filter(date < implementation) |>
  mutate(treatment = 1 * (treatment == "treatment"),  # treatment indicator
         post      = 1 * (period == "anticipation"))  # post indicator

# BACI models

m_pixel <- feols(
  c(
    effort_day,
    effort_set,
    catch_tot,
    cpue_day,
    cpue_set
  ) ~ treatment * post,
  data = estimation_panel,
  vcov = vcov_conley()
)

m_pixel_list <- as.list(m_pixel)

names(m_pixel_list) <- c(
  "Effort (days)",
  "Effort (sets)",
  "Total Catch",
  "CPUE (days)",
  "CPUE (sets)"
)

# Get model summary
modelsummary(
  m_pixel_list,
  stars = TRUE,
  title = "Pixel-level BACI models of effort, catch, and CPUE",
  notes = "BACI regression estimates at the pixel‑month level. The interaction
  term estiamtes the change in treated pixels relative to the control pixels
  during the anticiaption period. Conley standard errors are used.",
  output = "results/tab/pixel_model_summary.html"
)

# Repeat with log wrap

m_pixel_log <- feols(
  c(
    log(effort_day),
    log(effort_set),
    log(catch_tot),
    log(cpue_day),
    log(cpue_set)
  ) ~ treatment * post,
  data = estimation_panel,
  vcov = vcov_conley()
)

m_pixel_log_list <- as.list(m_pixel_log)

names(m_pixel_log_list) <- c(
  "log Effort (days) / km²",
  "log Effort (sets) / km²",
  "log Total Catch / km²",
  "CPUE (days)",
  "CPUE (sets)"
)

modelsummary(
  m_pixel_log_list,
  stars = TRUE,
  coef_map = c(
    "treatment" = "Treatment region",               # Sample labeling
    "post" = "Anticipation period",
    "treatment:post" = "BACI effect (Treatment × Anticipation)"
  ),
  title = "Log-linear pixel-level BACI models of effort, catch, and CPUE",
  note = "Log-linear BACI regression estimates at the pixel-month level.
  The interaction term estimates the percentage change in treated pixels
  relative to control pixels during the anticipation period. Conley standard
  errors are used.",
  output = "results/tab/log_pixel_model_summary.html"
)

# Unit of observation is the region --------------------------------------------

# Change catch_tot to kg
pipa_data_kg <- pipa_data |>
  mutate(catch_tot = catch_tot * 1000)


# Create region-level data
region_df <- pipa_data_kg |>
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
  mutate(
    treatment = 1 * (treatment == "treatment"),
    post      = 1 * (date >= announcement & date < implementation) # see if this can be fixed to match first panel
  )


# BACI models

m_region <- feols(
  c(
    effort_day_per_km2,
    effort_set_per_km2,
    catch_per_km2,
    cpue_day,
    cpue_set
  ) ~ treatment * post,
  data = region_panel
)

m_region_list <- as.list(m_region)

names(m_region_list) <- c(
  "Effort (days) / km²",
  "Effort (sets) / km²",
  "Total Catch / km²",
  "CPUE (days)",
  "CPUE (sets)"
)

modelsummary(
  m_region_list,
  stars = TRUE,
  title = "Region-level BACI models of effort, catch, and CPUE",
  note = "BACI regression estimates at the region-level. The interaction
  term estiamtes the change in treatment region relative to the control region
  during the anticiaption period.",
  output = "results/tab/region_model_summary.html"
)


# Repeat with log wrap
m_region_log <- feols(
  c(
    log(effort_day_per_km2),
    log(effort_set_per_km2),
    log(catch_per_km2),
    log(cpue_day),
    log(cpue_set)
  ) ~ treatment * post,
  data = region_panel
)

m_region_log_list <- as.list(m_region_log)

names(m_region_log_list) <- c(
  "log Effort (days) / km²",
  "log Effort (sets) / km²",
  "log Total Catch / km²",
  "CPUE (days)",
  "CPUE (sets)"
)

modelsummary(
  m_region_log_list,
  stars = TRUE,
  coef_map = c(
    "treatment" = "Treatment region",               # Sample labeling
    "post" = "Anticipation period",
    "treatment:post" = "BACI effect (Treatment × Anticipation)"
  ),
  title = "Log-linear region-level BACI models of effort, catch, and CPUE",
  note = "Log-linear BACI regression estimates at the region-level.
  The interaction term estimates the percentage change in the treatment region
  relative to control region during the anticipation period.",
  output = "results/tab/log_region_model_summary.html"
)

