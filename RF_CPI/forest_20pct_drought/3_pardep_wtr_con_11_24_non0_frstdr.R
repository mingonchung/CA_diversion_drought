# CPI Step 3: partial dependence — Forest watersheds, Drought years, Consumptive (consumtive_diverted)
# Run as SLURM array job: --array=1-12 (focal predictors only; year_wtr/month excluded)
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
load(paste0("rf_con_all_huc8_non0_6_1000_pct_frstdr_11_24.RData"))

print("load RData done")

## Focal predictors only (indices 1-12).
## year_wtr (13) and month (14) are lag/control terms; partial dependence
## plots for these are not scientifically interpretable and are not produced.
name.var <- c("mng_medhigh_10yr_pct", "BurnSev34_10yr_pct",
              "et_mean", "tmean", "prcp_sum", "swe_mean",
              "inflow_wtr_mm", "sum_cap_af", "elevation",
              "pop_den", "weighted_median_income", "project")

for (i in num) {
  var.name <- name.var[i]

  pd.con <- partial_dependence(rf.con, vars = var.name)

  print(paste0(var.name, ", partial dependence done"))

  setwd(paste0(input.dir, "output/pardep/frstdr/"))

  write.csv(pd.con,
            paste0("pardep_rf_con_", var.name, "_huc8_6_1000_non0_pct_frstdr_11_24.csv"),
            row.names = FALSE)

  gc()
  rm(pd.con)
}

print("Write.csv done")
