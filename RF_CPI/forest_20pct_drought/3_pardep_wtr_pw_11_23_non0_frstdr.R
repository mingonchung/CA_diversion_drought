#https://www.r-bloggers.com/2018/06/be-aware-of-bias-in-rf-variable-importance-metrics/

rm(list=ls())
#devtools::install_github("zmjones/edarf", subdir = "pkg")

library(tidyverse)
library(party)
library(edarf)

### select variables
num <- commandArgs(trailingOnly = TRUE)
num <- as.numeric(num)

# load Rdata, cforest
input.dir <- "/projects/mich9173/CA_wtr_div/"
#input.dir <- "~/data-store/home/mgchung/CA_wtr_div/"
#input.dir <- "E:/CA_data_analysis/new_analysis_040325/"

setwd(paste0(input.dir,"input"))
load(paste0("rf_pw_all_huc8_non0_4_1000_pct_frstdr.RData"))

print("load Rdata done")

## for loop
name.var <- c("mng_medhigh_10yr_pct", "BurnSev4_10yr_pct", "et_mean", "tmean", "prcp_sum", "swe_mean", "inflow_wtr_mm", 
              "sum_cap_af", "elevation", "pop_den", "weighted_median_income", "project", "year_wtr", "month")  

for (i in num) {
 var.name <- name.var[i] 

  ## get partial dependence
  pd.pw <- partial_dependence(rf.pw, vars = var.name)

  print(paste0(var.name, ", Partial dependence done"))

  # write.csv
  setwd(paste0(input.dir,"output/pardep/frstdr/"))

  write.csv(pd.pw, paste0("pardep_rf_pw_",var.name,"_huc8_4_1000_non0_pct_frstdr.csv"), row.names=F)

  gc()
  rm(pd.pw)

}

print("Write.csv done")
