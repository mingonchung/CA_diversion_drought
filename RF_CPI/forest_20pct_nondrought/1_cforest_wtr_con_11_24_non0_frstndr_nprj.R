# CPI Sensitivity: cforest — Forest watersheds, Non-drought years, Consumptive
# Excludes SWP/CVP-dominated HUC8s (project == 1) to test robustness of main CPI results.
# Addresses R2-1 (inter-HUC8 transfers). Companion to main analysis (1_cforest_..._frstndr_11_24.R).

rm(list = ls())

library(dplyr)
library(party)

input.dir <- "/projects/mich9173/CA_wtr_div/"
#input.dir <- "~/data-store/home/mgchung/CA_wtr_div/"
#input.dir <- "E:/CA_data_analysis/new_analysis_040325/"

setwd(paste0(input.dir, "input"))
wtr.data <- read.csv("CA_wtr_HUC8_all_var_month_040726.csv",
                     header = TRUE, stringsAsFactors = FALSE)

############################################
## Step 1: identify qualifying HUC8s (cumulative >= 1000 AF over WY2011-2024)
qualifying_huc8 <- wtr.data %>%
  group_by(huc8) %>%
  summarize(total_div = sum(consumtive_diverted, na.rm = TRUE), .groups = "drop") %>%
  filter(total_div >= 1000) %>%
  pull(huc8)

wtr.data.sub <- wtr.data %>% filter(huc8 %in% qualifying_huc8)

## Step 2: retain forested HUC8s (>= 20% forest cover)
wtr.data.sub <- wtr.data.sub %>%
  group_by(huc8, year) %>%
  filter(mean(Forest_pct, na.rm = TRUE) >= 20) %>%
  ungroup()

## Step 3: non-drought years only (WY2011, 2016-2019, 2023-2024)
wtr.data.sub <- wtr.data.sub %>%
  filter(year_wtr %in% c(2011, 2016, 2017, 2018, 2019, 2023, 2024))

## Step 4: exclude Colorado River basin HUC8s (prefix 150)
wtr.data.sub <- wtr.data.sub %>%
  filter(!(huc8 %in% c(15030101, 15030104, 15030107)))

## Step 5 (sensitivity): exclude SWP/CVP-dominated HUC8s (project == 1)
wtr.data.sub <- wtr.data.sub %>% filter(project == 0)

cat("Rows in sensitivity dataset:", nrow(wtr.data.sub), "\n")
cat("Unique HUC8s:", n_distinct(wtr.data.sub$huc8), "\n")

############################################
## Random forest — cforest (unbiased conditional inference)
set.seed(160617)

mtry.num  <- 6
ntree.num <- 1000

rf.con <- cforest(
  consumtive_diverted ~ mng_medhigh_10yr_pct + BurnSev34_10yr_pct +
    et_mean + tmean + prcp_sum + swe_mean + inflow_wtr_mm +
    sum_cap_af + elevation + pop_den + weighted_median_income +
    year_wtr + month,          # project excluded: constant (== 0) after Step 5
  data    = wtr.data.sub,
  control = cforest_unbiased(mtry = mtry.num, ntree = ntree.num)
)

## Save RData
setwd(paste0(input.dir, "input"))
save(rf.con, file = paste0("rf_con_all_huc8_non0_", mtry.num, "_", ntree.num,
                           "_pct_frstndr_11_24_nprj.RData"))

print("con Forest-non-drought, sensitivity (no SWP/CVP), cforest done")
