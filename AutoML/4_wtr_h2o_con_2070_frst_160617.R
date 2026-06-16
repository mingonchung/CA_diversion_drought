rm(list=ls())

# H2O AutoML - Consumptive diversion projection, forested CA HUC8s (prefix 18)
# Projects to 2070 under CMIP5 (4 GCMs x 4 scenarios) and CMIP6 (8 GCMs).
#
# Key changes vs. prior version:
#   - year_wtr REMOVED from predictors: tree models cannot extrapolate beyond
#     training range 2011-2024. Drought state encoded through physical variables.
#   - Climate-stratified holdout: WY2014 (peak drought), WY2017 (extreme wet),
#     WY2019 (normal/wet recovery). 3 of 14 years = 21% held out.
#   - log1p target transformation: consumptive diversion is severely zero-inflated
#     (curtailment months) and right-skewed. log1p is particularly critical here.
#   - Original-scale R2/RMSE/NSE/PBIAS reported after expm1() back-transform.

library(h2o)
library(timetk)
library(tidyquant)
library(dplyr)
library(tibble)
library(ggplot2)
library(lubridate)

num       <- 160617
input.dir <- "/projects/mich9173/CA_wtr_div/"

# -----------------------------------------------------------------------
# 1. Load observational data
# -----------------------------------------------------------------------
setwd(paste0(input.dir, "input"))
wtr.data  <- read.csv("CA_wtr_HUC8_all_var_month_040726.csv",
                      header = TRUE, stringsAsFactors = FALSE)
huc8.aqkm <- read.csv("CA_wtr_HUC8_all_040226.csv",
                      header = TRUE, stringsAsFactors = FALSE)

wtr.data$yearmonth <- paste(wtr.data$year, wtr.data$month, sep = "-")
wtr.data$date      <- ym(wtr.data$yearmonth)
wtr.data$quater    <- dplyr::recode(wtr.data$month,
                       `1`=1,`2`=1,`3`=1,`4`=2,`5`=2,`6`=2,
                       `7`=3,`8`=3,`9`=3,`10`=4,`11`=4,`12`=4)

# -----------------------------------------------------------------------
# 2. Eligibility: CA HUC8s only (prefix 18), cumulative >= 1000 AF, forested
# -----------------------------------------------------------------------
huc8_eligible_con <- wtr.data %>%
  group_by(huc8) %>%
  summarize(cum_div = sum(consumtive_diverted, na.rm = TRUE), .groups = "drop") %>%
  filter(cum_div >= 1000) %>%
  pull(huc8)

wtr.sub <- wtr.data %>%
  filter(huc8 %in% huc8_eligible_con,
         grepl("^18", as.character(huc8))) %>%
  group_by(huc8, year) %>%
  filter(mean(Forest_pct, na.rm = TRUE) >= 20) %>%
  ungroup()

cat(sprintf("Eligible forested CA HUC8s (consumptive): %d\n",
            length(unique(wtr.sub$huc8))))

# log1p-transform target (especially important for consumptive: zero-inflated)
wtr.sub$con_log    <- base::log1p(wtr.sub$consumtive_diverted)
wtr.sub$foldnumber <- wtr.sub$year_wtr - 2010L

# -----------------------------------------------------------------------
# 3. HUC8 area lookup
# -----------------------------------------------------------------------
area_col  <- "area_sqkm"
huc8.area <- as.data.frame(unique(huc8.aqkm[, c("huc8", area_col)]))
huc8.sel  <- data.frame(huc8 = unique(wtr.sub$huc8))
huc8.area <- merge(huc8.sel, huc8.area, by = "huc8")

# -----------------------------------------------------------------------
# 4. Climate-stratified holdout split
#    WY2014: peak first drought (critically dry)
#    WY2017: extreme wet break (record precipitation, atmospheric rivers)
#    WY2019: normal/wet recovery (pre-second drought baseline)
# -----------------------------------------------------------------------
test_years <- c(2014L, 2017L, 2019L)
train_wtr  <- wtr.sub %>% filter(!year_wtr %in% test_years)
test_wtr   <- wtr.sub %>% filter( year_wtr %in% test_years)

# THE FIX: Dynamically re-index foldnumber to be strictly contiguous (0-indexed)
# This prevents empty validation folds in H2O
train_wtr$foldnumber <- as.integer(as.factor(train_wtr$year_wtr)) - 1L

cat(sprintf("Train: %d years, %d rows | Test: %d years, %d rows\n",
            length(unique(train_wtr$year_wtr)), nrow(train_wtr),
            length(unique(test_wtr$year_wtr)),  nrow(test_wtr)))
# -----------------------------------------------------------------------
# 5. Start H2O
# -----------------------------------------------------------------------
h2o.init(max_mem_size = "32G", nthreads = -1, port = 61716)

train_h2o <- as.h2o(train_wtr)
test_h2o  <- as.h2o(test_wtr)

y <- "con_log"
x <- c("month", "quater",
       "mng_medhigh_10yr_pct", "BurnSev34_10yr_pct",
       "et_mean", "tmean", "prcp_sum", "swe_mean",
       "inflow_wtr_mm", "sum_cap_af", "elevation",
       "pop_den", "weighted_median_income", "project")

# -----------------------------------------------------------------------
# 6. AutoML training
# -----------------------------------------------------------------------
automl.h2o.con <- h2o.automl(
  x                = x,
  y                = y,
  training_frame   = train_h2o,
  fold_column      = "foldnumber",
  max_runtime_secs = 0,
  max_models       = 50,
  exclude_algos    = c("DeepLearning"),
  sort_metric      = "RMSE",
  seed             = 160617)

automl_leader <- automl.h2o.con@leader
print(automl_leader)

tryCatch(
  print(h2o.varimp(automl_leader)),
  error = function(e) cat("varimp unavailable for StackedEnsemble.\n"))

lb <- h2o.get_leaderboard(object = automl.h2o.con, extra_columns = "ALL")
print(lb)

# -----------------------------------------------------------------------
# 7. Performance evaluation (original-scale metrics via expm1 back-transform)
# -----------------------------------------------------------------------
pred_h2o       <- h2o.predict(automl_leader, newdata = test_h2o)
pred_h2o_train <- h2o.predict(automl_leader, newdata = train_h2o)
perf_test      <- h2o.performance(automl_leader, newdata = test_h2o)
perf_train     <- h2o.performance(automl_leader, newdata = train_h2o)

error_wtr <- wtr.sub[, c("huc8","year","month","year_wtr","consumtive_diverted")] %>%
  filter(year_wtr %in% test_years) %>%
  add_column(pred_log = as_tibble(pred_h2o)$predict) %>%
  mutate(pred      = base::expm1(pmax(pred_log, 0)),
         actual    = consumtive_diverted,
         error     = actual - pred,
         error_pct = if_else(actual != 0, error / actual, NA_real_)) %>%
  select(-consumtive_diverted, -pred_log)

error_wtr_train <- wtr.sub[, c("huc8","year","month","year_wtr","consumtive_diverted")] %>%
  filter(!year_wtr %in% test_years) %>%
  add_column(pred_log = as_tibble(pred_h2o_train)$predict) %>%
  mutate(pred      = base::expm1(pmax(pred_log, 0)),
         actual    = consumtive_diverted,
         error     = actual - pred,
         error_pct = if_else(actual != 0, error / actual, NA_real_)) %>%
  select(-consumtive_diverted, -pred_log)

calc_metrics <- function(df) {
  obs   <- df$actual;  prd <- df$pred
  ss_res <- sum((obs - prd)^2,             na.rm = TRUE)
  ss_tot <- sum((obs - mean(obs, na.rm=TRUE))^2, na.rm = TRUE)
  r2    <- 1 - ss_res / ss_tot
  rmse  <- sqrt(mean((obs - prd)^2,   na.rm = TRUE))
  mae   <- mean(abs(obs - prd),       na.rm = TRUE)
  pbias <- sum(obs - prd, na.rm=TRUE) / sum(obs, na.rm=TRUE) * 100
  data.frame(R2=round(r2,3), RMSE=round(rmse,1), MAE=round(mae,1),
             NSE=round(r2,3), PBIAS=round(pbias,1))
}

m_test  <- calc_metrics(error_wtr)
m_train <- calc_metrics(error_wtr_train)

perf_summary <- data.frame(
  R2_test      = m_test$R2,    RMSE_test  = m_test$RMSE,
  MAE_test     = m_test$MAE,   NSE_test   = m_test$NSE,
  PBIAS_test   = m_test$PBIAS,
  R2_train     = m_train$R2,   RMSE_train = m_train$RMSE,
  MAE_train    = m_train$MAE,  NSE_train  = m_train$NSE,
  PBIAS_train  = m_train$PBIAS,
  R2_test_log  = h2o.r2(perf_test),   RMSE_test_log  = h2o.rmse(perf_test),
  R2_train_log = h2o.r2(perf_train),  RMSE_train_log = h2o.rmse(perf_train),
  train_years  = paste(sort(unique(train_wtr$year_wtr)), collapse=","),
  test_years   = paste(sort(unique(test_wtr$year_wtr)),  collapse=","),
  n_train      = nrow(train_wtr),
  n_test       = nrow(test_wtr)
)
print(perf_summary)

error_wtr_sum <- error_wtr %>%
  summarise(me   = mean(error,          na.rm=TRUE),
            rmse = mean(error^2,        na.rm=TRUE)^0.5,
            mae  = mean(abs(error),     na.rm=TRUE),
            mape = mean(abs(error_pct), na.rm=TRUE),
            mpe  = mean(error_pct,      na.rm=TRUE))

error_wtr_yr <- error_wtr %>%
  group_by(year, month, year_wtr) %>%
  summarize(n=n(), actual_sum=sum(actual), pred_sum=sum(pred), .groups="drop")

error_wtr_yr_train <- error_wtr_train %>%
  group_by(year, month, year_wtr) %>%
  summarize(n=n(), actual_sum=sum(actual), pred_sum=sum(pred), .groups="drop")

# -----------------------------------------------------------------------
# 8. Write outputs and save model
# -----------------------------------------------------------------------
output.dir <- paste0(input.dir, "output/prediction/2021/")
if (!dir.exists(output.dir)) dir.create(output.dir, recursive = TRUE)
setwd(output.dir)

write.csv(perf_summary,       paste0("h2o_con_perf_huc8_",     num, "_frst.csv"), row.names=FALSE)
write.csv(error_wtr,          paste0("h2o_con_all_predict_",   num, "_frst.csv"), row.names=FALSE)
write.csv(error_wtr_yr,       paste0("h2o_con_error_yr_",      num, "_frst.csv"), row.names=FALSE)
write.csv(error_wtr_yr_train, paste0("h2o_con_error_yr_train_",num, "_frst.csv"), row.names=FALSE)
write.csv(error_wtr_sum,      paste0("h2o_con_error_summary_", num, "_frst.csv"), row.names=FALSE)

model_dir  <- paste0(output.dir, "models/")
if (!dir.exists(model_dir)) dir.create(model_dir, recursive = TRUE)
model_path <- h2o.saveModel(automl_leader, path=model_dir, force=TRUE)
cat("Leader model saved:", model_path, "\n")

# -----------------------------------------------------------------------
# 9. CMIP5 projections
# -----------------------------------------------------------------------
GCM.list  <- c("CanESM2","CNRM-CM5","HadGEM2-ES","MIROC5")
list.prj  <- c("ssp1_rcp45","ssp3_rcp85","ssp2_rcp85","ssp5_rcp85")
cmip5.dir <- paste0(output.dir, "cmip5/")
if (!dir.exists(cmip5.dir)) dir.create(cmip5.dir, recursive = TRUE)

for (a in seq_along(GCM.list)) {
  for (numi in seq_along(list.prj)) {
    setwd(paste0(input.dir, "input/projection/"))
    wtr.pred <- read.csv(
      paste0("CA_wtr_HUC8_all_", list.prj[numi], "_", GCM.list[a], ".csv"),
      header=TRUE, stringsAsFactors=FALSE)
    wtr.pred$date   <- as.Date(wtr.pred$date)
    wtr.pred$quater <- dplyr::recode(wtr.pred$month,
                        `1`=1,`2`=1,`3`=1,`4`=2,`5`=2,`6`=2,
                        `7`=3,`8`=3,`9`=3,`10`=4,`11`=4,`12`=4)
    wtr.pred2  <- merge(wtr.pred, huc8.area, by="huc8")
    pred_wtr   <- wtr.pred2 %>% filter(year_wtr > 2010)
    h2o_pred   <- as.h2o(pred_wtr) # Name the uploaded object
    simul_pred <- h2o.predict(automl_leader, newdata=h2o_pred)
    pred_list  <- pred_wtr[, 1:5] %>%
      add_column(pred = base::expm1(pmax(as_tibble(simul_pred)$predict, 0)))
    pred_list_yr <- pred_list %>%
      group_by(date) %>% summarize(n=n(), pred_sum=sum(pred), .groups="drop")
    setwd(cmip5.dir)
    write.csv(pred_list,
      paste0("h2o_con_pred_",    list.prj[numi],"_",GCM.list[a],"_",num,"_frst.csv"), row.names=FALSE)
    write.csv(pred_list_yr,
      paste0("h2o_con_pred_yr_", list.prj[numi],"_",GCM.list[a],"_",num,"_frst.csv"), row.names=FALSE)
    h2o.rm(h2o_pred)
    h2o.rm(simul_pred)
    cat(sprintf("CMIP5 %s %s done.\n", GCM.list[a], list.prj[numi]))
  }
}

# -----------------------------------------------------------------------
# 10. CMIP6 projections
# -----------------------------------------------------------------------
cmip6.dir <- paste0(output.dir, "cmip6/")
if (!dir.exists(cmip6.dir)) dir.create(cmip6.dir, recursive = TRUE)
setwd(paste0(input.dir, "input/projection/"))
wtr.pred6 <- read.csv("CA_wtr_HUC8_all_ssp370_CMIP6.csv", header=TRUE, stringsAsFactors=FALSE)
wtr.pred6$date   <- as.Date(wtr.pred6$date)
wtr.pred6$quater <- dplyr::recode(wtr.pred6$month,
                    `1`=1,`2`=1,`3`=1,`4`=2,`5`=2,`6`=2,
                    `7`=3,`8`=3,`9`=3,`10`=4,`11`=4,`12`=4)
wtr.pred6b <- merge(wtr.pred6, huc8.area, by="huc8")
GCM6.list  <- unique(wtr.pred6b$model)

for (a in seq_along(GCM6.list)) {
  pred_wtr   <- wtr.pred6b %>% filter(year_wtr > 2014, model==GCM6.list[a])
  h2o_pred   <- as.h2o(pred_wtr) # Name the uploaded object
  simul_pred <- h2o.predict(automl_leader, newdata=h2o_pred)
  pred_list  <- pred_wtr[, 1:6] %>%
    add_column(pred = base::expm1(pmax(as_tibble(simul_pred)$predict, 0)))
  pred_list_yr <- pred_list %>%
    group_by(date) %>% summarize(n=n(), pred_sum=sum(pred), .groups="drop")
  setwd(cmip6.dir)
  write.csv(pred_list,
    paste0("h2o_con_pred_",    GCM6.list[a],"_",num,"_frst.csv"), row.names=FALSE)
  write.csv(pred_list_yr,
    paste0("h2o_con_pred_yr_", GCM6.list[a],"_",num,"_frst.csv"), row.names=FALSE)
  h2o.rm(h2o_pred)
  h2o.rm(simul_pred)
  cat(sprintf("CMIP6 %s done.\n", GCM6.list[a]))
}

h2o.shutdown(prompt=FALSE)
cat("Done.\n")
