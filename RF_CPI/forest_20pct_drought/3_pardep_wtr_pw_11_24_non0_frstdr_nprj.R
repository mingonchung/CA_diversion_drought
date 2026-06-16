# CPI Sensitivity Step 3: partial dependence — Forest watersheds, Drought years, Hydropower
# Run as SLURM array job: --array=1-11 (project excluded: constant after nprj filter)
# https://www.r-bloggers.com/2018/06/be-aware-of-bias-in-rf-variable-importance-metrics/

rm(list = ls())
# devtools::install_github("zmjones/edarf", subdir = "pkg")

library(tidyverse)
library(party)
library(edarf)

## SLURM array index selects which predictor to process
num <- commandArgs(trailingOnly = TRUE)
num <- as.numeric(num)

input.dir <- "/projects/mich9173/CA_wtr_div/"
#input.dir <- "~/data-store/home/mgchung/CA_wtr_div/"
#input.dir <- "E:/CA_data_analysis/new_analysis_040325/"

setwd(paste0(input.dir, "input"))
load(paste0("rf_pw_all_huc8_non0_6_1000_pct_frstdr_11_24_nprj.RData"))

print("load RData done")

## Focal predictors only (indices 1-11).
## project excluded: identically 0 after nprj filter — not in formula, not plotted.
## year_wtr and month are lag/control terms, not produced.
name.var <- c("mng_medhigh_10yr_pct", "BurnSev34_10yr_pct",
              "et_mean", "tmean", "prcp_sum", "swe_mean",
              "inflow_wtr_mm", "sum_cap_af", "elevation",
              "pop_den", "weighted_median_income")

for (i in num) {
  var.name <- name.var[i]

  pd.pw <- partial_dependence(rf.pw, vars = var.name)

  print(paste0(var.name, ", partial dependence done"))

  setwd(paste0(input.dir, "output/pardep/frstdr/"))

  write.csv(pd.pw,
            paste0("pardep_rf_pw_", var.name, "_huc8_6_1000_non0_pct_frstdr_11_24_nprj.csv"),
            row.names = FALSE)

  gc()
  rm(pd.pw)
}

print("Write.csv done")
