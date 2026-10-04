# CA_diversion_drought

Code and derived data for Chung et al., "Forest thinning and wildfire do not increase surface-water diversions in California", *Nature Communications* (in revision).

Conditional permutation importance (CPI), streamflow analyses, and AutoML projections of hydropower and consumptive surface-water diversions in California's HUC8 watersheds, water years 2011–2024, with projections to 2070.

Version 1.2.1 adds this README and the license. Code and data are identical to version 1.2.0 (DOI 10.5281/zenodo.23129939), which reproduces the revised manuscript. Version 1.1.0 (DOI 10.5281/zenodo.20724105) holds the code submitted in June 2026.

## Changes from version 1.1.0

- CMIP6 air temperature in the projection input converted from K to °C (`Input_csv/Simulation/CA_wtr_HUC8_all_ssp370_CMIP6.csv.gz`).
- Both AutoML scripts check the units of every projection input before training.
- HUC8 table `Input_csv/CA_wtr_HUC8_all_040226.csv` added. Both AutoML scripts read it.

## Repository layout

```
RF_CPI/      random forests, CPI, and partial dependence (four watershed groups)
NLDI/        streamflow analyses in NLDI gauge catchments
AutoML/      H2O AutoML training and CMIP5 and CMIP6 projections
Input_csv/   HUC8 input tables and projection inputs
```

Figure scripts are not included.

## Analyses

| Folder | Scripts | Output |
|---|---|---|
| `RF_CPI/<group>/` | `1_cforest_*` | Conditional-inference random forest (`party::cforest`, mtry = 6, ntree = 1,000), saved as `.RData` |
| | `2_permimp_*` | CPI (`permimp`, threshold 0.80), per tree and mean |
| | `3_pardep_*` | Partial dependence for each predictor (`edarf`) |
| `NLDI/` | `1_3a_wtr_div_nldi_spatial.R` | Thinning, wildfire, and diversion records intersected with NLDI catchments. Station lists |
| | `1_3b_wtr_div_nldi_analysis.R` | Monthly streamflow, Q/PPT anomaly, D/Q ratio, and water-balance ET against OpenET |
| `AutoML/` | `4_wtr_h2o_pw_2070_frst_160617.R` | Hydropower model, validation statistics, CMIP5 and CMIP6 predictions |
| | `4_wtr_h2o_con_2070_frst_160617.R` | Consumptive model (log1p target), validation statistics, CMIP5 and CMIP6 predictions |

File-name codes:

| Code | Meaning |
|---|---|
| `frst`, `nfrst` | Forested (20% forest cover or more) and non-forested HUC8 watersheds |
| `dr`, `ndr` | Drought years (WY2012–2015, WY2020–2022) and non-drought years |
| `pw`, `con` | Hydropower and consumptive diversion |
| `nprj` | Sensitivity analysis excluding HUC8s with SWP or CVP infrastructure (forested groups only) |

AutoML holds out WY2014, WY2017, and WY2019 as the test set and trains up to 50 models (deep learning excluded, seed 160617).

## Input_csv/

| File | Contents |
|---|---|
| `CA_wtr_HUC8_all_var_month_040726.csv` | Monthly HUC8 model table, WY2011–2024: two diversion targets and 12 predictors |
| `CA_wtr_HUC8_all_040226.csv` | Monthly HUC8 source table, 2010–2024, with HUC8 area |
| `Simulation/CA_wtr_HUC8_all_<scenario>_<GCM>.csv.gz` | CMIP5 projection inputs, 4 scenarios × 4 GCMs, 2010–2070 |
| `Simulation/CA_wtr_HUC8_all_ssp370_CMIP6.csv.gz` | CMIP6 projection input, SSP3-7.0, 8 GCMs, WY2015–2070 |

| Variable | Description | Unit |
|---|---|---|
| `Power_diverted`, `consumtive_diverted` | Hydropower and consumptive diversion | acre-feet |
| `mng_medhigh_10yr_pct` | Medium- and high-intensity thinning, 10-year cumulative | % of watershed area |
| `BurnSev34_10yr_pct` | Medium- and high-severity wildfire, 10-year cumulative | % of watershed area |
| `et_mean`, `prcp_sum` | Evapotranspiration, precipitation | mm month-1 |
| `swe_mean`, `inflow_wtr_mm` | Snow water equivalent, net inflow | mm |
| `tmean` | Air temperature | °C |
| `sum_cap_af` | Reservoir capacity | acre-feet |
| `elevation` | Elevation | m |
| `pop_den` | Population density | persons km-2 |
| `weighted_median_income` | Median income | US $ |
| `project` | SWP or CVP infrastructure | 0, 1 |

## System requirements

R 4.5.2 and H2O 3.44.0.3 for the AutoML runs. [fill: R version for RF_CPI and NLDI, if different]

Packages: `party`, `permimp`, `edarf`, `h2o`, `sf`, `dplyr`, `tidyverse`, `tidyr`, `tibble`, `readr`, `stringr`, `lubridate`, `reshape2`, `Rmisc`, `zoo`, `RcppRoll`. [fill: versions from sessionInfo()]

Tested on Linux (SLURM cluster) and Windows. The AutoML scripts start H2O with 32 GB of memory. No non-standard hardware is required.

## Installation

```r
install.packages(c("party", "permimp", "h2o", "sf", "tidyverse", "reshape2",
                   "Rmisc", "zoo", "RcppRoll", "devtools"))
devtools::install_github("zmjones/edarf", subdir = "pkg")
```

Installation takes [fill: minutes] on a desktop computer.

## Running

1. Set `input.dir` at the top of each script (the directory variables in the `NLDI/` scripts).
2. Copy `Input_csv/*.csv` to `<input.dir>/input/`.
3. Decompress `Input_csv/Simulation/*.csv.gz` into `<input.dir>/input/projection/`.
4. Create `<input.dir>/output/varimp/<group>/` and `<input.dir>/output/pardep/<group>/`, where `<group>` is `frstdr`, `frstndr`, `nfrstdr`, or `nfrstndr`.
5. In each `RF_CPI/<group>/` folder, run `1_cforest_*`, then `2_permimp_*`, then `3_pardep_*`. The partial-dependence scripts take the predictor index as an argument (SLURM array 1–12, or 1–11 for `nprj`).
6. Run the two `AutoML/` scripts. Each writes to `<input.dir>/output/prediction/`.

Demo: `RF_CPI/nonforest_20pct_drought/1_cforest_wtr_pw_11_24_non0_nfrstdr.R` followed by `2_permimp_wtr_pw_11_24_non0_nfrstdr.R` runs on the included input table and writes the CPI values to `output/varimp/nfrstdr/`. Run time: [fill].

Full run times: [fill: one cforest and permimp job; one AutoML script].

## Input data

The `NLDI/` scripts read source datasets that are publicly available and are not redistributed. Sources, periods, and resolutions are listed in Supplementary Table 1 of the paper.

## License

MIT. See `LICENSE`.

## Contact

Min Gon Chung, Cooperative Institute for Research in Environmental Sciences, University of Colorado Boulder.
