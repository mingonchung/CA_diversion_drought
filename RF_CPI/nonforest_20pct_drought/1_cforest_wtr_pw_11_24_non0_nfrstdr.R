# CPI Step 1: cforest — Non-forest watersheds, Drought years, Hydropower (Power_diverted)
# Reference: Hothorn et al. (2006); Debeer & Strobl (2020)
# https://www.r-bloggers.com/2018/06/be-aware-of-bias-in-rf-variable-importance-metrics/

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
## Step 1: identify qualifying HUC8s
## Retain only HUC8s that cumulatively diverted >= 1000 AF (Power_diverted)
## over the full study period (WY2011-2024), consistent with manuscript Methods.
qualifying_huc8 <- wtr.data %>%
  group_by(huc8) %>%
  summarize(total_div = sum(Power_diverted, na.rm = TRUE), .groups = "drop") %>%
  filter(total_div >= 1000) %>%
  pull(huc8)

wtr.data.sub <- wtr.data %>% filter(huc8 %in% qualifying_huc8)

## Step 2: retain non-forested HUC8s (< 20% forest cover)
wtr.data.sub <- wtr.data.sub %>%
  group_by(huc8, year) %>%
  filter(mean(Forest_pct, na.rm = TRUE) < 20) %>%
  ungroup()

## Step 3: drought years only (WY2012-2015, 2020-2022)
wtr.data.sub <- wtr.data.sub %>%
  filter(year_wtr %in% c(2012, 2013, 2014, 2015, 2020, 2021, 2022))

## Step 4: exclude Colorado River basin HUC8s (prefix 150)
wtr.data.sub <- wtr.data.sub %>%
  filter(!(huc8 %in% c(15030101, 15030104, 15030107)))

cat("Rows in model dataset:", nrow(wtr.data.sub), "\n")
cat("Unique HUC8s:", n_distinct(wtr.data.sub$huc8), "\n")

############################################
## Random forest — cforest (unbiased conditional inference)
set.seed(160617)

mtry.num  <- 6
ntree.num <- 1000

rf.pw <- cforest(
  Power_diverted ~ mng_medhigh_10yr_pct + BurnSev34_10yr_pct +
    et_mean + tmean + prcp_sum + swe_mean + inflow_wtr_mm +
    sum_cap_af + elevation + pop_den + weighted_median_income +
    project + year_wtr + month,
  data    = wtr.data.sub,
  control = cforest_unbiased(mtry = mtry.num, ntree = ntree.num)
)

## Save RData
setwd(paste0(input.dir, "input"))
save(rf.pw, file = paste0("rf_pw_all_huc8_non0_", mtry.num, "_", ntree.num,
                          "_pct_nfrstdr_11_24.RData"))

print("pw Non-forest-drought, cforest done")
