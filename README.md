# CA_diversion_drought

Code and derived data for Chung et al., "Forest thinning and wildfire do not increase surface-water diversions in California", *Nature Communications* (in revision).

Conditional permutation importance (CPI), streamflow analyses, and AutoML projections of hydropower and consumptive surface-water diversions across California's HUC8 watersheds, water years 2011–2024, with projections to 2070.

## Versions

| Version | DOI | Contents |
|---|---|---|
| 1.2.1 | This release | README and licence added. Code and data are those of version 1.2.0 |
| 1.2.0 | 10.5281/zenodo.23129939 | CMIP6 air temperature in °C, revised AutoML scripts, HUC8 table `CA_wtr_HUC8_all_040226.csv` added |
| 1.1.0 | 10.5281/zenodo.20724105 | First revision. CMIP6 air temperature not converted from K |

## Repository layout

```
RF_CPI/      random forests, CPI, and partial dependence (36 scripts)
NLDI/        streamflow-to-precipitation (Q/PPT) and diversion-to-streamflow (D/Q) analyses
AutoML/      H2O AutoML training and CMIP5 and CMIP6 projections
Input_csv/   HUC8 input tables and projection inputs
```

| Folder | Manuscript items |
|---|---|
| `RF_CPI/` | Figs 6 and 7, Supplementary Figs 3–5 |
| `NLDI/` | Figs 4 and 5, Supplementary Fig. 10 |
| `AutoML/` | Fig. 8, Supplementary Figs 6 and 11 |

Figure scripts are not included.

Script names in `RF_CPI/` combine three labels.

| Label | Meaning |
|---|---|
| `con`, `pw` | Consumptive diversion, hydropower diversion |
| `frstdr`, `frstndr`, `nfrstdr`, `nfrstndr` | Forested or non-forested watersheds, drought or non-drought years |
| `_nprj` | Sensitivity analysis without SWP/CVP watersheds (forested models only) |

## 1. System requirements

Tested on Rocky Linux 8.10 (Alpine cluster, University of Colorado Boulder) with R 4.5.2. H2O needs Java (OpenJDK 25.0.1 used). H2O 3.46.0.7 gave identical models. No non-standard hardware is required. The AutoML scripts start H2O with 32 GB of memory.

| Folder | Packages (version) |
|---|---|
| `RF_CPI/` | `party` (1.3.21), `permimp` (1.1.0.9000), `edarf` (1.1.1), `dplyr` (1.2.1), `tidyverse` (2.0.0), `tibble` (3.3.1), `reshape2` (1.4.5), `Rmisc` (1.5.1) |
| `AutoML/` | `h2o` (3.44.0.3), `dplyr` (1.2.1), `tibble` (3.3.1), `lubridate` (1.9.5) |
| `NLDI/` | `sf` (1.1.2), `dplyr` (1.2.1), `tidyr` (1.3.2), `readr` (2.2.0), `stringr` (1.6.0), `lubridate` (1.9.5), `reshape2` (1.4.5), `zoo` (1.8.15), `RcppRoll` (0.3.2) |

## 2. Installation guide

```r
install.packages(c("party", "dplyr", "tidyverse", "tibble", "reshape2", "Rmisc",
                   "h2o", "lubridate", "sf", "tidyr", "readr", "stringr", "zoo",
                   "RcppRoll", "devtools"))
devtools::install_github("ddebeer/permimp")
devtools::install_github("zmjones/edarf", subdir = "pkg")
```

Typical install time: about 4 minutes.

## 3. Demo

The input tables in `Input_csv/` are the full analysis tables, so the demo is the hydropower AutoML run of the manuscript.

1. Create a working directory with this layout.

```
<root>/input/                 the two CSV files of Input_csv/
<root>/input/projection/      the files of Input_csv/Simulation/, uncompressed
```

2. Set `input.dir` at the top of the script to `<root>/`.

3. Run the script.

```
Rscript AutoML/4_wtr_h2o_pw_2070_frst_160617.R
```

Expected output in `<root>/output/prediction/2021/`:

- Five CSV files with observed and predicted diversions and the performance of the model on the training and test sets (`h2o_pw_*_160617_frst.csv`).
- The saved leader model in `models/`, a gradient-boosting machine.
- Predictions in `cmip5/` (32 files, 4 scenarios × 4 GCMs) and `cmip6/` (16 files, 8 GCMs).

Expected run time: 3 min 35 s on 24 cores. The script starts H2O with 32 GB of memory (`max_mem_size`), which can be lowered on a desktop computer.

## 4. Instructions for use

**RF_CPI.** Each of the four folders holds one watershed group and drought condition. Run step 1 (`cforest`, mtry = 6, ntree = 1,000), step 2 (`permimp`, conditional, threshold 0.80), and step 3 (`partial_dependence`) for `con` and `pw`. Step 3 takes the predictor index (1–12) as its argument and ran as a SLURM array job. Create `output/varimp/<label>/` and `output/pardep/<label>/` before running. These steps are compute-intensive: one partial-dependence task took 1.6–2.7 hours on 16 cores with 60 GB of memory.

**AutoML.** `4_wtr_h2o_pw_2070_frst_160617.R` (hydropower) and `4_wtr_h2o_con_2070_frst_160617.R` (consumptive) train on water years 2011–2024, hold out water years 2014, 2017, and 2019, and project to 2070. Each script checks the units of every projection file before training. Outputs go to `<root>/output/prediction/<run.folder>/`: performance tables, the saved leader model, and predictions in `cmip5/` and `cmip6/`. Settings: 50 models, seed 160617, deep learning excluded. The consumptive run took 4 min 5 s on 24 cores.

**NLDI.** Run `1_3a_wtr_div_nldi_spatial.R`, then `1_3b_wtr_div_nldi_analysis.R`. Both need the raw inputs (USGS streamflow, NLDI catchments, eWRIMS diversion points, FACTS and CAL FIRE thinning records, burn severity, OpenET, and DAYMET). Set the directory variables at the top of each script.

## Input data

Raw input datasets are publicly available and are not redistributed. Sources, periods, and resolutions are listed in Supplementary Table 1 of the manuscript.

| File | Contents |
|---|---|
| `CA_wtr_HUC8_all_var_month_040726.csv` | Monthly diversions and the 12 predictors for 140 HUC8s, water years 2011–2024. Input to `RF_CPI/` and `AutoML/` |
| `CA_wtr_HUC8_all_040226.csv` | Monthly HUC8 table for 2010–2024 with watershed area. Read by the AutoML scripts |
| `Simulation/CA_wtr_HUC8_all_<scenario>_<GCM>.csv.gz` | CMIP5 projection inputs to 2070, 4 scenarios × 4 GCMs |
| `Simulation/CA_wtr_HUC8_all_ssp370_CMIP6.csv.gz` | CMIP6 projection inputs for water years 2015–2070, SSP3-7.0, 8 GCMs |

Diversions (`Power_diverted`, `consumtive_diverted`) and reservoir capacity are in acre-feet. Precipitation and ET are in mm per month, SWE and inflow in mm, air temperature in °C, population density in persons per km², and thinning and wildfire extent in % of watershed area.

## License

MIT. See `LICENSE`.

## Contact

Min Gon Chung, Cooperative Institute for Research in Environmental Sciences, University of Colorado Boulder.
