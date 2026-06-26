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

## Effort days
m_effort_day <- feols(
  effort_day ~ treatment * post,
  data = estimation_panel,
  vcov = vcov_conley()
)

## Effort sets
m_effort_set <- feols(
  effort_set ~ treatment * post,
  data = estimation_panel,
  vcov = vcov_conley()
)

## Total catch
m_catch_tot <- feols(
  catch_tot ~ treatment * post,
  data = estimation_panel,
  vcov = vcov_conley()
)

## CPUE (days)
m_cpue_day <- feols(
  cpue_day ~ treatment * post,
  data = estimation_panel,
  vcov = vcov_conley()
)

## CPUE (sets)
m_cpue_set <- feols(
  cpue_set ~ treatment * post,
  data = estimation_panel,
  vcov = vcov_conley()
)

# Model Summaries
tab <- modelsummary(
  list(
    "Effort (days)" = m_effort_day,
    "Effort (sets)" = m_effort_set,
    "Catch total"   = m_catch_tot,
    "CPUE (day)"    = m_cpue_day,
    "CPUE (set)"    = m_cpue_set
  ),
  title = "Unit of observation is a 1 degree grid cell",
  gof_omit = "IC|RMSE",
  stars = TRUE,
  output = "gt"
)

gtsave(tab, "results/tab/pixel_model_summaries.png")

modelsummary(
  list(
    "Effort (days)" = m_effort_day,
    "Effort (sets)" = m_effort_set,
    "Catch total"   = m_catch_tot,
    "CPUE (day)"    = m_cpue_day,
    "CPUE (set)"    = m_cpue_set
  ),
  stars = TRUE,
  output = "html"
)

# Redo models in one set

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

modelsummary(
  m_pixel_list,
  stars = TRUE,
  output = "html"
)

# Repeat model with a log wrap

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
  "log Effort (days)",
  "log Effort (sets)",
  "log Total Catch",
  "log CPUE (days)",
  "log CPUE (sets)"
)

modelsummary(
  m_pixel_log_list,
  stars = TRUE,
  output = "html"
)

# Example from meeting:
# m1 <- feols(c(effort_day, effort_set) ~ treatment * post,
            # data = estimation_panel, vcov = vcov_conley())

# modelsummary(m1, output = "markdown")

# feols(c(effort_day) ~ treatment * post,
     #  data = estimation_panel)


# Unit of observation is the region --------------------------------------------

# Create region-level data
region_df <- pipa_data |>
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

## Effort (days per km²)
m_region_effort_day <- feols(
  log(effort_day_per_km2) ~ treatment * post,
  data = region_panel
)

# Effort (sets per km²)
m_region_effort_set <- feols(
  effort_set_per_km2 ~ treatment * post,
  data = region_panel
)

# Catch (per km²)
m_region_catch <- feols(
  catch_per_km2 ~ treatment * post,
  data = region_panel
)

# CPUE (day)
m_region_cpue_day <- feols(
  cpue_day ~ treatment * post,
  data = region_panel
)

# CPUE (set)
m_region_cpue_set <- feols(
  cpue_set ~ treatment * post,
  data = region_panel
)

tab2 <- modelsummary(
  list(
    "Effort day / km²" = m_region_effort_day,
    "Effort set / km²" = m_region_effort_set,
    "Catch / km²"      = m_region_catch,
    "CPUE day"         = m_region_cpue_day,
    "CPUE set"         = m_region_cpue_set
  ),
  output = "gt"
)

gtsave(tab2, "results/tab/region_model_summaries.png")

modelsummary(
  list(
    "Effort day / km²" = m_region_effort_day,
    "Effort set / km²" = m_region_effort_set,
    "Catch / km²"      = m_region_catch,
    "CPUE day"         = m_region_cpue_day,
    "CPUE set"         = m_region_cpue_set
  ),
  stars = TRUE,
  output = "html"
)

# Redo models in one set

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
  title = "Region level model comparisons",
  output = "html"
)


# Redo with log wrap
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
  output = "html"
)


# Old --------------------------------------------------------------------------

m_event <- feols(catch_tot ~ i(date, treatment, "2013-09-01") | year ^ month + lat ^ lon,
                 data = tuna_region |>
                   mutate(
                     treatment = 1 * (treatment == "treatment")
                   )) |>
  iplot()

iplot(m_event)

summary(m_event)
m_event$gg


summary(m_event)

feols(catch_tot ~ treatment * rcs(as.numeric(date - as.Date("2013-09-01"))),
      data = tuna_region |>
        mutate(
          treatment = 1 * (treatment == "treatment")
        )) |>
  predict()
