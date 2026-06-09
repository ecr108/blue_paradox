# Blue Paradox
Assessing preemptive fishing responses to marine protected area announcements using RFMO catch and effort data.

## Overview


## Research question
Do announcements of MPAs lead to increased fishing effort or catch in the designated area prior to implementation?


## Repository Structure

- `data/`:
    - `data/raw`: 
        - `raw/eez` : contains `.gkpg` files of EEZs from the `mregions2` package
        - `raw/mpa` : contains `.gkpg` files of MPAs from the `mregions2` package
        - `raw/rfmo`: contains harmonized datasets from the [`tuna_data`](https://github.com/jcvdav/tuna_data) repo
        
    - `data/processed`: contains `.rds` file of processed data to recreate PIPA analysis
    - `data/output`: 
- `scripts/`:
    - `scripts/01_processing`: contains scripts to prepare MPA, EEZ, and RFMO data
    - `scripts/02_analysis`: contains script to test for PIPA blue paradox
    - `scripts/03_content`: contains script that produces figures for the PIPA analysis
- `results/`:
    - `results/figures`:
      - `pipa_original`: contains time series plots from recreated PIPA analysis
    - `results/img`:
    - `results/tab`:

## Background

## Study areas

## Data sources

## Repository structure

## References
