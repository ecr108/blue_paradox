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
modelsummary(
  list(
    "Effort (days)" = m_effort_day,
    "Effort (sets)" = m_effort_set,
    "Catch total"   = m_catch_tot,
    "CPUE (day)"    = m_cpue_day,
    "CPUE (set)"    = m_cpue_set
  ),
  output = "markdown"
)


# Example from meeting:
# m1 <- feols(c(effort_day, effort_set) ~ treatment * post,
            # data = estimation_panel, vcov = vcov_conley())

# modelsummary(m1, output = "markdown")

# feols(c(effort_day) ~ treatment * post,
     #  data = estimation_panel)


# Unit of observation is the region --------------------------------------------

# Create region-level data
# should this be used for models above??
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
    catch_per_km2 = catch_sum / region_area,
    effort_day_per_km2 = effort_day_sum / region_area,
    effort_set_per_km2 = effort_set_sum / region_area
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
  effort_day_per_km2 ~ treatment * post,
  data = region_panel,
  vcov = vcov_NW(unit = ~ treatment, time = ~ date) # not sure if this is correct approach
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

modelsummary(
  list(
    "Effort day / km²" = m_region_effort_day,
    "Effort set / km²" = m_region_effort_set,
    "Catch / km²"      = m_region_catch,
    "CPUE day"         = m_region_cpue_day,
    "CPUE set"         = m_region_cpue_set
  ),
  output = "markdown"
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
