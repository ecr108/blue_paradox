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
gmr_data <- readRDS("data/processed/blue_paradox_gmr.rds")

## Prepare data ----------------------------------------------------------------

# Set temporal boundaries

start_pre      <- as.Date("1997-01-01")  # 1 year before 1998 announcement
announcement   <- as.Date("1998-03-18")  # Announcement and implementation of GMR
implementation <- as.Date("2002-01-01")  # Implementation of enforcement date (need to find month)
end_post       <- as.Date("2003-01-01")  # 1 year after implementation

# Test Analysis ################################################################

# Unit of observation is pixel by month ----------------------------------------

# Create estimation panel
estimation_panel <- gmr_data |>
  st_drop_geometry() |>
  filter(date < implementation) |>
  mutate(treatment = 1 * (treatment == "treatment"),  # treatment indicator
         post      = 1 * (period == "anticipation"))  # post indicator

# BACI models

m_pixel <- feols(
  c(
    effort_set,
    catch_tot,
    cpue_set
  ) ~ treatment * post,
  data = estimation_panel,
  vcov = vcov_conley()
)

m_pixel_list <- as.list(m_pixel)

names(m_pixel_list) <- c(
  "Effort (sets)",
  "Total Catch (mt)",
  "CPUE (mt/sets)"
)

# Get model summary
modelsummary(
  m_pixel_list,
  stars = TRUE,
  title = "Pixel-level BACI models of effort, catch, and CPUE",
  notes = "BACI regression estimates at the pixel‑month level. The interaction
  term estimates the change in treated pixels relative to the control pixels
  during the anticiaption period. Conley standard errors are used.",
  output = "results/tab/gmr/pixel_model_summary.html"
)

# Repeat with log wrap

m_pixel_log <- feols(
  c(
    log(effort_set),
    log(catch_tot),
    log(cpue_set)
  ) ~ treatment * post,
  data = estimation_panel,
  vcov = vcov_conley()
)

m_pixel_log_list <- as.list(m_pixel_log)

names(m_pixel_log_list) <- c(
  "log Effort (sets)",
  "log Total Catch (mt)",
  "CPUE (mt/sets)"
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
  output = "results/tab/gmr/log_pixel_model_summary.html"
)

saveRDS(m_pixel_log, "data/output/gmr/m_pixel_log.rds")

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
  mutate(
    treatment = 1 * (treatment == "treatment"),
    post      = 1 * (date >= announcement & date < implementation) # see if this can be fixed to match first panel
  )


# BACI models

m_region <- feols(
  c(
    effort_set_per_km2,
    catch_per_km2,
    cpue_set
  ) ~ treatment * post,
  data = region_panel
)

m_region_list <- as.list(m_region)

names(m_region_list) <- c(
  "Effort (sets) / km²",
  "Total Catch (kg) / km²",
  "CPUE (kg/sets)"
)

modelsummary(
  m_region_list,
  stars = TRUE,
  title = "Region-level BACI models of effort, catch, and CPUE",
  note = "BACI regression estimates at the region-level. The interaction
  term estiamtes the change in treatment region relative to the control region
  during the anticiaption period.",
  output = "results/tab/gmr/region_model_summary.html"
)


# Repeat with log wrap
m_region_log <- feols(
  c(
    log(effort_set_per_km2),
    log(catch_per_km2),
    log(cpue_set)
  ) ~ treatment * post,
  data = region_panel
)

m_region_log_list <- as.list(m_region_log)

names(m_region_log_list) <- c(
  "log Effort (sets) / km²",
  "log Total Catch (kg) / km²",
  "CPUE (kg/sets)"
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
  output = "results/tab/gmr/log_region_model_summary.html"
)

saveRDS(m_region_log, "data/output/gmr/m_region_log.rds")
