# The Blue Paradox
Assessing preemptive fishing responses to marine protected area announcements using RFMO catch and effort data.

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
    - `results/img`:
    - `results/tab`: contains model summary tables

## Key Findings

### PIPA
![Temporal trends in fishing effort, catch, and CPUE for control and treatment regions at the pixel-month level in PIPA. Panels show monthly trajectories for (A) effort (days), (B) effort (sets), (C) total catch (mt), (D) CPUE (mt/day), and (E) CPUE (mt/set) from 2013-2016. The first vertical marker indicates the announcement date of PIPA, while the second vertical marker notes the implementation of PIPA.](results/figures/pipa_original/BACI_timeseries_panels.png)

### GMR
![Temporal trends in fishing effort, catch, and CPUE for control and treatment regions at the pixel-month level in GMR. Panels show monthly trajectories for (A) effort (sets), (B) total catch (mt), (C) CPUE (mt/set) from 2013-2016. The first vertical marker indicates the implementation of GMR, while the second vertical marker notes the implementation of enforcement within GMR.](results/figures/gmr/BACI_timeseries_panels.png)

## Data sources

Emily Rodriguez & Juan Carlos Villaseñor-Derbez. (2026). RFMO Tuna Catch and Effort Data (Version v0.0.0) [Dataset]. Zenodo. https://doi.org/10.5281/zenodo.21494710

## References

McDermott, Grant R., Kyle C. Meng, Gavin G. McDonald, and Christopher J. Costello. 2019.
“The Blue Paradox: Preemptive Overfishing in Marine Reserves.” Proceedings of the
National Academy of Sciences 116 (12): 5319–25.
https://doi.org/10.1073/pnas.1802862115
