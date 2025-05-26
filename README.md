# DLNM-GLMM-health

## Overview

This repository contains the complete workflow for analyzing the effects of climatic variables and air pollution on hospital discharges due to respiratory diseases in Costa Rica. The study employs Distributed Lag Nonlinear Models (DLNM) integrated with Generalized Linear Mixed Models (GLMM) to evaluate exposure-lag-response associations across climatic regions and subregions.

## Project Structure
```
DLNM-GLMM-health/
├── Data/
│   ├── Shapefiles/          # Geographic data (e.g., IMN climate regions)
│   └── Tables/              # Raw input tables, including hospitalization records
│
├── Results/
│   ├── Plots/
│       ├── discharges/               # Visualizations of hospitalization patterns
│       ├── exploratory_variable/     # EDA of temperature, humidity, AOD, etc.
│       └── relative_risk/            # Predicted RR surfaces by variable and region
│
├── Script/                 # All R scripts for data extraction, processing, modeling
├── DLNM-GLMM-health.Rproj  # RStudio project file
├── Cod_import              # Codified import procedures
├── .gitignore              # Standard git exclusions
├── .Rhistory               # R history file
└── README.md               # Project description
```


## Repository

GitHub: https://github.com/Emanuelle-Parra/DLNM-GLMM-health.git

## Data Sources

**Hospital discharges:** Weekly aggregated counts for ICD-10 J00–J99 respiratory codes, provided by the Costa Rican Social Security Fund (CCSS).

**Climate regions/subregions:** Defined by the National Meteorological Institute (IMN) of Costa Rica.

**Temperature:** CHIRTS database from UCSB. (https://www.chc.ucsb.edu/data/chirtsmonthly)

**Precipitation:** CHIRPS database from UCSB. (https://developers.google.com/earth-engine/datasets/catalog/UCSB-CHG_CHIRPS_DAILY?hl=es-419)

**Relative humidity:** MERRA-2 reanalysis by NASA. (https://disc.gsfc.nasa.gov/datasets/M2T3NVCLD_5.12.4/summary)

**Aerosol Optical Depth (AOD):** MOD08_D3 from MODIS (NASA Terra/Aqua). (https://ladsweb.modaps.eosdis.nasa.gov/missions-and-measurements/products/MOD08_D3#product-information)


## Software and Environment

R version 4.4.3 (2025-02-28 ucrt) - "Trophy Case"

### Key R Packages Used

Data manipulation and transformation:

dplyr, tidyr, tidyverse, stringr, readxl, rlist, reshape2, reshape, lubridate

Visualization and graphics:

ggplot2, ggseas, scales, viridis, corrplot, GGally, ggmap

Statistical analysis and modeling:

dlnm, glmmTMB, gamlss, lme4, mgcv, MASS, forecast, car, splines, tseries, urca

Information metrics and evaluation:

energy, infotheo, Metrics

Spatial data processing:

sf, sp, geodata, raster, terra, maps, mapproj, tidync, RNetCDF, ncdf4

Table creation:

kableExtra, DT

## How to Reproduce the Analysis

Clone the repository:

git clone https://github.com/Emanuelle-Parra/DLNM-GLMM-health.git

Open DLNM-GLMM-health.Rproj in RStudio.

Run the scripts in Script/ sequentially:

Scripts 01–07: Data extraction and preprocessing

Script 10: Exploratory analysis

Scripts 11–31: Model fitting for discharges and relative risk per region

Visualize some outputs in Results/Plots/


## Contact

If you have any questions or suggestions, feel free to reach out via email at emanuelle.parra@ucr.ac.cr.

