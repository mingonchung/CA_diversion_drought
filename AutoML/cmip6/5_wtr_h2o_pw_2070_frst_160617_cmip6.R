rm(list=ls())

#https://docs.h2o.ai/h2o/latest-stable/h2o-docs/automl.html
## h2o + timetk, Time Series Machine Learning
# https://www.business-science.io/code-tools/2017/10/28/demo_week_h2o.html

# Load libraries
library(h2o)        # Awesome ML Library
library(timetk)     # Toolkit for working with time series in R
library(tidyquant)  # Loads tidyverse, financial pkgs, used to get data
library(dplyr)
library(tibble)
library(ggplot2)
library(lubridate)

# conda install r-lubridate r-tidyquant r-timetk
# module load mambaforge
# mamba install r-lubridate r-tidyquant r-timetk
####
#num <- commandArgs(trailingOnly = TRUE)
#num <- as.numeric(num)
num <- 160617

input.dir <- "/projects/mich9173/CA_wtr_div/"
#input.dir <- "E:/CA_data_analysis/new_analysis_040325/"

setwd(paste0(input.dir,"input"))
wtr.data <- read.csv(paste0("CA_wtr_HUC8_all_var_month_040925.csv"), header=T, stringsAsFactors=FALSE)
huc8.aqkm <- read.csv(paste0("CA_wtr_HUC8_all_040825.csv"), header=T, stringsAsFactors=FALSE)

## subset
# delete unecessary columns
# only diverted & year, month
wtr.data$yearmonth <- paste(wtr.data$year,wtr.data$month,sep="-")
wtr.data.sub <- wtr.data#[,c(-5)]
#names(wtr.data.sub)

# year, month, sum_dams to numeric
wtr.data.sub$date <- ym(wtr.data.sub$yearmonth)

## all time-related variables
# quater
wtr.data.sub$quater <- dplyr::recode(wtr.data.sub$month, `1` = 1, `2` = 1, `3` = 1, `4` = 2, `5` = 2, `6` = 2, `7` = 3, `8` = 3, `9` = 3, `10` = 4, `11` = 4, `12` = 4)

############################################
## delete watersheds, all zeros over time
wtr.data.sub.non0 <- wtr.data.sub %>% group_by(huc8, year) %>% filter(sum(Power_diverted) >= 1000)
wtr.data.sub.non0 <- subset(wtr.data.sub.non0, Power_diverted > 0)
wtr.data.sub2.non0 <- wtr.data.sub.non0 %>% group_by(huc8, year) %>% filter(mean(Forest_pct) >= 20)
wtr.data.sub2.non0$foldnumber <- wtr.data.sub2.non0$year_wtr-2010 #year_wtr

## add area_sqkm to pred_wtr
huc8.sel <- unique(wtr.data.sub2.non0[,c("huc8")])
huc8.area.sqkm <- as.data.frame(unique(huc8.aqkm[,c(1,36)]))
huc8.area.sqkm <- merge(huc8.sel, huc8.area.sqkm, by="huc8")

##!!!!!!!!!!!!!!
year.sel <- 2019
working.hr <- 0

# Split into training, validation and test sets
train_wtr <- wtr.data.sub2.non0 %>% filter(year_wtr < year.sel+1) #!!!!!!!!!!!!!!!!!!!!!!!!!!
#valid_wtr <- wtr.data.sub2.non0 %>% filter(year_wtr > year.sel)#year.sel
test_wtr  <- wtr.data.sub2.non0 %>% filter(year_wtr > year.sel)#year.sel

h2o.init(max_mem_size = "32G", nthreads = 1, port=61716)         # Fire up h2o #port=16618, max_mem_size='32G'
#h2o.no_progress() # Turn off progress bars

# Convert to H2OFrame objects
train_h2o <- as.h2o(train_wtr)
#valid_h2o <- as.h2o(valid_wtr)
test_h2o  <- as.h2o(test_wtr)

# Set names for h2o
y <- "Power_diverted"
x <- c("date", "year_wtr", "month", "quater", "mng_medhigh_10yr_pct", "BurnSev4_10yr_pct", "et_mean", "tmean", "prcp_sum", "swe_mean", 
       "inflow_wtr_mm", "sum_cap_af", "elevation", "pop_den", "weighted_median_income", "project")
# x = x: The names of our feature columns.
# y = y: The name of our target column.
# training_frame = train_h2o: Our training set consisting of data from 2010 to start of 2016.
# validation_frame = valid_h2o: Our validation set consisting of data in the year 2016. H2O uses this to ensure the model does not overfit the data.
# leaderboard_frame = test_h2o: The models get ranked based on MAE performance against this set.
# max_runtime_secs = 60: We supply this to speed up H2O's modeling. The algorithm has a large number of complex # models so we want to keep things moving at the expense of some accuracy.
# stopping_metric = "deviance": Use deviance as the stopping metric, which provides very good results for MAPE
# linear regression model used, but can use any model
set.seed(160617)
#rd.var <- sample.int(100000, 1000, replace=F)
#!!!!!!!!!!!!!!!!!!!!!!!!!!
automl.h2o.pw <- h2o.automl(
  x = x, 
  y = y, 
  training_frame = train_h2o,   
  #validation_frame = valid_h2o, 
  #leaderboard_frame = test_h2o,
  fold_column = "foldnumber",
  max_runtime_secs=3600*working.hr, # 0 (no limit) in HPC 
  seed = 160617) 

#summary(automl.h2o.pw) #conf.int=0.95

# Extract leader model
automl_leader <- automl.h2o.pw@leader
print(automl_leader) # n = nrow(automl_leader)

# variable importance
varimp.csv <- h2o.varimp(automl_leader)
print(varimp.csv)

# Get leaderboard with all possible columns
lb <- h2o.get_leaderboard(object = automl.h2o.pw, extra_columns = "ALL")
print(lb)

# Generate predictions on the test data
pred_h2o <- h2o.predict(automl_leader, newdata = test_h2o)
pred_h2o_train <- h2o.predict(automl_leader, newdata = train_h2o)
perf_test <- h2o.performance(automl_leader, newdata = test_h2o)
perf_train <- h2o.performance(automl_leader, newdata = train_h2o)

# Convert the performance metrics to a data frame
perf_summary <- data.frame(
  MSE = h2o.mse(perf_test),
  RMSE = h2o.rmse(perf_test),
  MAE = h2o.mae(perf_test),
  R2 = h2o.r2(perf_test),
  Mean_Residual_Deviance = perf_test@metrics$mean_residual_deviance,
  MSE_train = h2o.mse(perf_train),
  RMSE_train = h2o.rmse(perf_train),
  MAE_train = h2o.mae(perf_train),
  R2_train = h2o.r2(perf_train),
  Mean_Residual_Deviance = perf_train@metrics$mean_residual_deviance
)

# root mean squared error (RMSE) and mean absolute error (MAE)
print(perf_summary)

# Investigate test error
error_wtr <- wtr.data.sub2.non0[, c("huc8", "year", "month", "year_wtr", "Power_diverted")] %>%
  filter(year_wtr > year.sel) %>%
  tibble::add_column(pred = pred_h2o %>% as.tibble() %>% pull(predict)) %>%
  rename(actual = Power_diverted) %>%
  mutate(
    # Replace negative predicted values with 0; pmax returns the elementwise maximum.
    pred = pmax(pred, 0),
    error = actual - pred,
    # Avoid division by zero: if actual is 0 then set error_pct to NA (or 0, if that’s more appropriate for your analysis)
    error_pct = if_else(actual != 0, error / actual, NA))

print(error_wtr)

error_wtr_train <- wtr.data.sub2.non0[, c("huc8", "year", "month", "year_wtr", "Power_diverted")] %>%
  filter(year_wtr < year.sel+1) %>%
  tibble::add_column(pred = pred_h2o_train %>% as.tibble() %>% pull(predict)) %>%
  rename(actual = Power_diverted) %>%
  mutate(
    # Replace negative predicted values with 0; pmax returns the elementwise maximum.
    pred = pmax(pred, 0),
    error = actual - pred,
    # Avoid division by zero: if actual is 0 then set error_pct to NA (or 0, if that’s more appropriate for your analysis)
    error_pct = if_else(actual != 0, error / actual, NA))

error_wtr_sum <- error_wtr %>%
  summarise(
    me   = mean(error),
    rmse = mean(error^2)^0.5,
    mae  = mean(abs(error)),
    mape = mean(abs(error_pct)),
    mpe  = mean(error_pct)
  ) #%>% glimpse()

error_wtr_yr <- error_wtr %>%
  group_by(year, month, year_wtr) %>% 
  dplyr::summarize(n=n(),
                   actual_sum = sum(actual),
                   pred_sum = sum(pred))

error_wtr_yr_train <- error_wtr_train %>%
  group_by(year, month, year_wtr) %>% 
  dplyr::summarize(n=n(),
                   actual_sum = sum(actual),
                   pred_sum = sum(pred))

## write.csv
output.dir <- paste0(input.dir,"output/prediction/",year.sel,"_",working.hr,"hr/cmip6/")

if ( ! dir.exists(output.dir) ) {
  dir.create(output.dir, recursive = TRUE)
}
setwd(output.dir)

write.csv(perf_summary, paste0("h2o_pw_perf_test_huc8_",num,"_frst.csv"), row.names = FALSE)
write.csv(error_wtr, paste0("h2o_pw_all_predict_huc8_",num,"_frst.csv"), row.names=F)
write.csv(error_wtr_yr, paste0("h2o_pw_error_predict_huc8_",num,"_frst.csv"), row.names=F)
write.csv(error_wtr_yr_train, paste0("h2o_pw_error_predict_huc8_",num,"_frst_train.csv"), row.names=F)
write.csv(error_wtr_sum, paste0("h2o_pw_error_summary_huc8_",num,"_frst.csv"), row.names=F)


#############################
###### -2070 forecasts ######
setwd(paste0(input.dir,"input"))

wtr.pred <- read.csv("CA_wtr_HUC8_all_ssp370_CMIP6.csv", header=T, stringsAsFactors=FALSE)

wtr.pred$date <- as.Date(wtr.pred$date)
wtr.pred$quater <- dplyr::recode(wtr.pred$month, `1` = 1, `2` = 1, `3` = 1, `4` = 2, `5` = 2, `6` = 2, `7` = 3, `8` = 3, `9` = 3, `10` = 4, `11` = 4, `12` = 4)

### delete zero-watersheds
wtr.pred2 <- merge(wtr.pred, huc8.area.sqkm, by="huc8")
print(paste0("Selected HUC8 #: ", length(unique(wtr.pred2$huc8))))

GCM.list <- unique(wtr.pred$model)

for(a in 1:length(GCM.list)) {
    
    pred_wtr <-  wtr.pred2 %>% filter(year_wtr > 2014,
                                      model == GCM.list[a])
    simul_h2o <- as.h2o(pred_wtr)
    
    ## predict from 2021 to 2070
    simul_h2o_pred <- h2o.predict(automl_leader, newdata = simul_h2o)
    
    # Investigate test error
    simul_h2o_pred_list <- pred_wtr[,c(1:6)] %>%
      filter(year_wtr > 2014) %>% ##!!!!!!!!!!
      tibble::add_column(pred = simul_h2o_pred %>% as.tibble() %>% pull(predict)) #%>%
    #rename(actual = consumtive_diverted) %>%
    #mutate(
    #  error     = actual - pred,
    #  error_pct = error / actual
    #) 
    
    simul_h2o_pred_list_yr <- simul_h2o_pred_list %>%
      group_by(date) %>% 
      dplyr::summarize(n=n(),
                       #actual_sum = sum(actual),
                       pred_sum = sum(pred))
    
    ## write.csv
    #!!!!!!!!!!!!!!!!!!!!!!!!!!
    setwd(output.dir)

    write.csv(simul_h2o_pred_list, paste0("h2o_pw_pred_huc8_",GCM.list[[a]],"_",num,"_frst.csv"), row.names=F)
    write.csv(simul_h2o_pred_list_yr, paste0("h2o_pw_pred_huc8_yr_",GCM.list[[a]],"_",num,"_frst.csv"), row.names=F)

  }
