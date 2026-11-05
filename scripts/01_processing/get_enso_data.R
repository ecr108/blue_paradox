################################################################################
# title
################################################################################
#
# Description
# Go here: https://psl.noaa.gov/data/climateindices/list/
# Find:
# Niño 3.4*    East Central Tropical Pacific SST (5N-5S)(170-120W): From CPC
# CPC uses the NOAA ERSST V5 anomalies. Now uses https://www.cpc.ncep.noaa.gov/data/indices/ersst5.nino.mth.91-20.ascii. Mean values also available.
# Click on "Mean Values", which akes you here: https://psl.noaa.gov/data/correlation/nina4.data
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  tidyverse
)

# Set temporal boundaries
start_pre      <- as.Date("2012-09-01")  # 1 year before 2013 announcement
announcement   <- as.Date("2013-09-01")  # Announcement online defined by PIPA paper
implementation <- as.Date("2015-01-01")  # Implementation date


# Load data --------------------------------------------------------------------
nino_raw <- read_table("https://psl.noaa.gov/data/correlation/nina34.data",
                      skip = 3,
                      col_names = F)

## PROCESSING ##################################################################

# Get annual means -------------------------------------------------------------
nino <- nino_raw |>
  magrittr::set_colnames(c("year", 1:12)) |>
  mutate(across(everything(), as.numeric)) |>
  filter(!is.na(year)) |>
  filter(year >= 2012, year <= 2015) |>
  pivot_longer(cols = c(2:13),
               names_to = "month",
               values_to = "nino34") |>
  mutate(
    month = as.integer(month),
    date = as.Date(sprintf("%04d-%02d-01", year, month))
  ) |>
  select(date, nino34) |>
  filter(
    date >= as.Date("2012-09-01"),
    date < implementation
  )

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(object = nino,
        file = here("data", "raw", "annual_nino34.rds"))
