#https://www.r-bloggers.com/2018/06/be-aware-of-bias-in-rf-variable-importance-metrics/

rm(list=ls())
#devtools::install_github("zmjones/edarf", subdir = "pkg")

library(dplyr)
library(party)
#library(edarf)

input.dir <- "/projects/mich9173/CA_wtr_div/"
#input.dir <- "~/data-store/home/mgchung/CA_wtr_div/"
#input.dir <- "E:/CA_data_analysis/new_analysis_040325/"

setwd(paste0(input.dir,"input"))
wtr.data <- read.csv("CA_wtr_HUC8_all_var_month_040925.csv", header=T, stringsAsFactors=FALSE)

############################################
## delete watersheds, all zeros over time
wtr.data.sub.non0 <- wtr.data %>% group_by(huc8, year) %>% filter(sum(consumtive_diverted) >= 1000)
#wtr.data.sub3.non0 <- subset(wtr.data.sub2.non0, consumtive_diverted > 0)
wtr.data.sub.non0 <- wtr.data.sub.non0 %>% group_by(huc8, year) %>% filter(mean(Forest_pct) < 20)

# drought years only; 2012-2015 & 2020-2022
wtr.data.sub.non0 <- subset(wtr.data.sub.non0, year_wtr %in% c(2012, 2013, 2014, 2015, 2020, 2021, 2022))

# delete colorado river basins, start with 150;
wtr.data.sub2.non0 <- subset(wtr.data.sub.non0, !(huc8 %in% c(15030101, 15030104, 15030107)))
#max(wtr.data.sub2.non0$tmean)

###########
## random forest
set.seed(160617)

mtry.num <- 4
ntree.num <- 1000

rf.con <- cforest(consumtive_diverted ~ mng_medhigh_10yr_pct + BurnSev4_10yr_pct + et_mean + tmean + prcp_sum + swe_mean + inflow_wtr_mm + sum_cap_af + elevation + pop_den + weighted_median_income + project + year_wtr + month,
                 data = wtr.data.sub2.non0,
                 control = cforest_unbiased(mtry = mtry.num,
                                            ntree = ntree.num))
# save Rdata
setwd(paste0(input.dir,"input"))
save(rf.con, file = paste0("rf_con_all_huc8_non0_",mtry.num,"_",ntree.num,"_pct_nfrstdr.RData"))

print("con non-Forest-drought years, Cforest done")