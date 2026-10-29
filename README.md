# Testing the Blue Paradox in the Phoenix Islands Protected Area using RFMO catch and effort data
## Overview

This repository contains the data processing, models, and outputs used in the paper 
"Testing the Blue Paradox in the Phoenix Islands Protected Area using RFMO catch and effort data".
The analysis reassesses the original blue paradox analysis in the Phoenix Islands Protected Area (McDermott et al., 2019) and extends the
evaluation to three additional MPAs: the Galápagos Marine Reserve (GMR), Revillagigedo National Park (RNP), 
and the Palau National Marine Sanctuary (PNMS).

## Repository Structure

- `data/`:
    - `data/raw`: 
        - `raw/eez` : contains `.gkpg` files of EEZs
        - `raw/mpa` : contains `.gkpg` files of MPAs
        - `raw/rfmo`: contains harmonized dataset from the [`tuna_data`](https://github.com/jcvdav/tuna_data) repo
    - `data/processed`: contains `.rds` files of filtered data to test blue paradox
    - `data/output`: contains model outputs of blue paradox analyses
- `docs/`:
    - contains paper output of study and references
- `scripts/`:
    - `scripts/01_processing`: contains scripts to prepare MPA, EEZ, and RFMO data
    - `scripts/02_analysis`: contains scripts to test MPAs for blue paradox
    - `scripts/03_content`: contains scripts to produce time series and maps for each analysis
- `results/`:
    - `results/figures`: contains time series from blue paradox analyses

## Key Findings

### PIPA
![Temporal trends in our five outcomes of interest for control and treatment regions in the Phoenix Islands Protected Area (PIPA) at the region-month level. Panels show monthly trajectories for (A) effort (days/10,000 km$^2$), (B) effort (sets/10,000 km$^2$), (C) total catch (kg/10,000 km$^2$), (D) CPUE (kg/day), and (E) CPUE (kg/sets) from 2013-2016. The vertical line indicates the announcement of PIPA (2013-09-01). The final data point is on the date of the MPA implementation (2015-01-01). Panel F highlights the related control and treatment region.](results/figures/pipa_original/BACI_timeseries_panels_region.png){#fig-pipa-region}

### GMR
![Temporal trends in our three outcomes of interest for control and treatment regions in the Galápagos Marine Reserve (GMR) at the region-month level. Panels show monthly trajectories for (A) effort (sets/10,000 km$^2$), (B) total catch (kg/10,000 km$^2$), (C) CPUE (kg/set) from 1998-2002. The vertical line indicates the implementation of GMR (1998-03-18). The final data point is on the estimated date of enforcement within GMR (2002-01-01). Panel D highlights the related control and treatment region.](results/figures/gmr/BACI_timeseries_panels_region.png){#fig-gmr-region}

## Data sources

Emily Rodriguez & Juan Carlos Villaseñor-Derbez. (2026). RFMO Tuna Catch and Effort Data (Version v0.0.0) [Dataset]. Zenodo. https://doi.org/10.5281/zenodo.21494710

## References

McDermott, Grant R., Kyle C. Meng, Gavin G. McDonald, and Christopher J. Costello. 2019.
“The Blue Paradox: Preemptive Overfishing in Marine Reserves.” Proceedings of the
National Academy of Sciences 116 (12): 5319–25.
https://doi.org/10.1073/pnas.1802862115
