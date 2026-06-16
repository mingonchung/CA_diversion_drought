## Stage 3 — Script 2: Tabular analysis (REVISED v2)
## Changes from v1:
##   - Q/P anomaly: site-month demeaning (removes seasonality + between-site)
##   - Adds within-site thinning-QP slope for each station
##   - All other sections unchanged from v1
## Author: Min Gon Chung

rm(list = ls())

library(dplyr)
library(lubridate)
library(readr)
library(stringr)
library(zoo)
library(RcppRoll)
library(reshape2)
library(tidyr)

# ── Directories ───────────────────────────────────────────────────────────────
nwis_dir <- "E:/CA_data/NWIS/ST_daily"
gee_dir  <- "E:/CA_data/GEE/nldi"
out_dir  <- "E:/CA_data_analysis/new_analysis_040325/output/csv/nldi" #"E:/CA_data/NWIS/nldi"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ── Drought period definition (manuscript-aligned) ────────────────────────
assign_drought <- function(wy) {
  case_when(
    wy == 2011            ~ "Pre-drought",
    wy %in% 2012:2015     ~ "Drought1",
    wy %in% 2016:2019     ~ "PostDrought1",
    wy %in% 2020:2022     ~ "Drought2",
    wy %in% 2023:2024     ~ "PostDrought2",
    TRUE                  ~ NA_character_
  )
}

# ── Load outputs from Script 1 ─────────────────────────────────────────────
meta <- read_csv(file.path(nwis_dir, "station_metadata_all_CA.csv"),
                 col_types = cols(site_no = col_character()))

nldi_area <- read_csv(file.path(out_dir, "nldi_ca_coverage_pct.csv"),
                      col_types = cols(site_no = col_character())) %>%
  select(site_no, area_total_m2, area_total_km2)

stations_r2   <- read_csv(file.path(out_dir, "stations_r2_validation.csv"),
                          col_types = cols(site_no = col_character()))
stations_qp   <- read_csv(file.path(out_dir, "stations_qp_anomaly.csv"),
                          col_types = cols(site_no = col_character()))
stations_r4   <- read_csv(file.path(out_dir, "stations_r4_diversion_ratio.csv"),
                          col_types = cols(site_no = col_character()))

mng_nldi <- read_csv(file.path(out_dir, "thinning_nldi_intersect.csv"),
                     col_types = cols(site_no   = col_character(),
                                      yearmonth = col_character()))
fire_all <- read_csv(file.path(out_dir, "wildfire_nldi_intersect.csv"),
                     col_types = cols(site_no   = col_character(),
                                      yearmonth = col_character()))
wtr_nldi_wide <- read_csv(file.path(out_dir, "diversion_nldi_monthly_mm.csv"),
                          col_types = cols(site_no_nldi = col_character(),
                                           yearmonth    = col_character()))
met_nldi      <- read_csv(file.path(gee_dir, "OpenET_DayMet_nldi_2010_2024.csv.gz"),
                          col_types = cols(site_no = col_character()))

cat("Stations R2:", nrow(stations_r2),
    "| QP:", nrow(stations_qp),
    "| R4:", nrow(stations_r4), "\n")

# ═══════════════════════════════════════════════════════════════════════════
# ── STEP 3e: Daily → monthly streamflow (NLDI polygon area) ─────────────
# ═══════════════════════════════════════════════════════════════════════════

valid_codes  <- c("A", "A e", "A R", "P")
all_stations <- unique(c(stations_r2$site_no,
                         stations_qp$site_no,
                         stations_r4$site_no))

flow_raw <- read_csv(file.path(nwis_dir, "USGS_daily_streamflow_raw_2011_24.csv"),
                     col_types = cols(site_no = col_double(),
                                      Date     = col_character())) %>%
  mutate(
    Date       = as.Date(parse_date_time(Date, orders = c("Ymd", "mdY"), quiet = TRUE)),
    site_no    = formatC(site_no, width = 8, flag = "0", mode = "integer"),
    year       = year(Date),
    month      = month(Date),
    water_year = ifelse(month >= 10, year + 1, year)
  )

cat("Sample site_no after padding:", head(flow_raw$site_no, 3), "\n")
cat("Sample all_stations:          ", head(all_stations, 3), "\n")
cat("Overlap count:", sum(flow_raw$site_no %in% all_stations), "\n")

flow_filt <- flow_raw %>%
  filter(
    water_year >= 2011,
    water_year <= 2024,
    site_no    %in% all_stations,
    Flow_cd    %in% valid_codes,
    Flow       >= 0
  )

monthly_q <- flow_filt %>%
  group_by(site_no, water_year, year, month) %>%
  summarise(
    n_valid  = sum(!is.na(Flow)),
    mean_cfs = mean(Flow, na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  filter(n_valid >= 25) %>%
  left_join(nldi_area %>% select(site_no, area_total_m2),
            by = "site_no") %>%
  mutate(
    days_in_month = days_in_month(make_date(year, month, 1L)),
    vol_m3        = mean_cfs * 0.0283168 * as.numeric(days_in_month) * 86400,
    Q_mm          = vol_m3 / area_total_m2 * 1000,
    yearmonth     = str_c(year, month)
  )

write_csv(monthly_q, file.path(out_dir, "streamflow_monthly_mm.csv"))
cat("Monthly streamflow saved:", nrow(monthly_q),
    "rows |", n_distinct(monthly_q$site_no), "stations\n")

# ═══════════════════════════════════════════════════════════════════════════
# ── STEP 3f-part1: Rolling 10-year thinning and fire ─────────────────────
# ═══════════════════════════════════════════════════════════════════════════

mng_monthly <- mng_nldi %>%
  filter(intensity %in% c("High", "Medium")) %>%
  group_by(site_no, yearmonth, intensity) %>%
  summarise(area_km2 = sum(total_area_km2), .groups = "drop") %>%
  dcast(site_no + yearmonth ~ intensity,
        value.var = "area_km2", fill = 0) %>%
  rename(mng_high   = High,
         mng_medium = Medium)

fire_monthly <- fire_all %>%
  filter(BURNSEV4 > 0) %>%
  group_by(site_no, yearmonth, BURNSEV4) %>%
  summarise(burned_km2 = sum(total_burned_km2, na.rm = TRUE), .groups = "drop") %>%
  dcast(site_no + yearmonth ~ BURNSEV4,
        value.var = "burned_km2", fill = 0)

for (sev in 1:4) {
  col_old <- as.character(sev)
  col_new <- paste0("BurnSev", sev)
  if (col_old %in% names(fire_monthly)) {
    names(fire_monthly)[names(fire_monthly) == col_old] <- col_new
  } else {
    fire_monthly[[col_new]] <- 0
  }
}
cat("Fire monthly columns:", paste(names(fire_monthly), collapse = ", "), "\n")

spine <- expand.grid(
  site_no = all_stations,
  year    = 2000:2024,
  month   = 1:12,
  stringsAsFactors = FALSE
) %>%
  mutate(
    date      = zoo::as.yearmon(paste(year, month), "%Y %m"),
    yearmonth = str_c(year, month)
  )

spine_mng <- spine %>%
  left_join(mng_monthly,  by = c("site_no", "yearmonth")) %>%
  left_join(fire_monthly, by = c("site_no", "yearmonth")) %>%
  mutate(across(c(mng_high, mng_medium,
                  BurnSev1, BurnSev2, BurnSev3, BurnSev4),
                ~ replace_na(.x, 0))) %>%
  left_join(nldi_area %>% select(site_no, area_total_km2),
            by = "site_no") %>%
  arrange(site_no, date) %>%
  group_by(site_no) %>%
  mutate(
    mng_high_10yr   = roll_sum(mng_high,   n = 120, align = "right", fill = NA, na.rm = TRUE),
    mng_medium_10yr = roll_sum(mng_medium, n = 120, align = "right", fill = NA, na.rm = TRUE),
    BurnSev1_10yr   = roll_sum(BurnSev1,   n = 120, align = "right", fill = NA, na.rm = TRUE),
    BurnSev2_10yr   = roll_sum(BurnSev2,   n = 120, align = "right", fill = NA, na.rm = TRUE),
    BurnSev3_10yr   = roll_sum(BurnSev3,   n = 120, align = "right", fill = NA, na.rm = TRUE),
    BurnSev4_10yr   = roll_sum(BurnSev4,   n = 120, align = "right", fill = NA, na.rm = TRUE),
    mng_medhigh_10yr     = mng_high_10yr + mng_medium_10yr,
    mng_medhigh_10yr_pct = mng_medhigh_10yr / area_total_km2 * 100,
    BurnSev34_10yr       = BurnSev3_10yr + BurnSev4_10yr,
    BurnSev34_10yr_pct   = BurnSev34_10yr / area_total_km2 * 100,
    BurnSev4_10yr_pct    = BurnSev4_10yr  / area_total_km2 * 100,
    BurnSev1_10yr_pct    = BurnSev1_10yr  / area_total_km2 * 100,
    BurnSev2_10yr_pct    = BurnSev2_10yr  / area_total_km2 * 100,
    BurnSev3_10yr_pct    = BurnSev3_10yr  / area_total_km2 * 100
  ) %>%
  ungroup()

write_csv(
  spine_mng %>% filter(year >= 2011),
  file.path(out_dir, "thinning_fire_nldi_rolling10yr.csv")
)
cat("Rolling thinning+fire saved:",
    nrow(spine_mng %>% filter(year >= 2011)), "rows\n")

# ═══════════════════════════════════════════════════════════════════════════
# ── STEP 3f-part2: R2 water balance ──────────────────────────────────────
# ═══════════════════════════════════════════════════════════════════════════

met_sub <- met_nldi %>%
  mutate(
    yearmonth = str_c(YEAR, MONTH),
    site_no   = as.character(site_no)
  ) %>%
  select(site_no, YEAR, MONTH, yearmonth, prcp_sum, swe_mean, tmean, et_mean) %>%
  arrange(site_no, YEAR, MONTH) %>%
  group_by(site_no) %>%
  mutate(delta_swe = swe_mean - lag(swe_mean)) %>%
  ungroup()

disturbance_cols <- spine_mng %>%
  filter(year >= 2011) %>%
  select(site_no, year, month, yearmonth,
         mng_medhigh_10yr_pct,
         BurnSev34_10yr_pct,
         BurnSev4_10yr_pct)

# Monthly
wb_r2 <- monthly_q %>%
  filter(site_no %in% stations_r2$site_no) %>%
  left_join(met_sub, by = c("site_no", "yearmonth")) %>%
  mutate(
    ET_wb_monthly  = prcp_sum - Q_mm - delta_swe,
    ET_wb_simple   = prcp_sum - Q_mm,
    ET_residual    = ET_wb_monthly - et_mean,
    drought_period = assign_drought(water_year)
  ) %>%
  filter(!is.na(ET_wb_monthly), !is.na(et_mean)) %>%
  left_join(disturbance_cols, by = c("site_no", "year", "month", "yearmonth"))

write_csv(wb_r2, file.path(out_dir, "R2_water_balance_closure.csv"))
cat("R2 monthly water balance rows:", nrow(wb_r2),
    "| Stations:", n_distinct(wb_r2$site_no), "\n")

# Annual
# Set a realistic upper bound for ET in CA montane forests (e.g., 1100 mm/yr)
# Note: If you have actual PET/ETo data, replace the 1100 cap with your PET column.
MAX_ET_CAP <- 1100

wb_r2_annual <- monthly_q %>%
  filter(site_no %in% stations_r2$site_no) %>%
  left_join(met_sub %>% select(site_no, YEAR, MONTH, yearmonth,
                               prcp_sum, et_mean),
            by = c("site_no", "yearmonth")) %>%
  filter(!is.na(prcp_sum), !is.na(et_mean), !is.na(Q_mm)) %>%
  group_by(site_no, water_year) %>%
  mutate(n_months = n()) %>%
  filter(n_months >= 11) %>%
  summarise(
    P_ann      = sum(prcp_sum,  na.rm = TRUE),
    Q_ann      = sum(Q_mm,      na.rm = TRUE),
    ET_sat_ann = sum(et_mean,   na.rm = TRUE),
    n_months   = first(n_months),
    .groups    = "drop"
  ) %>%
  mutate(
    ET_wb_ann      = P_ann - Q_ann,
    ET_residual    = ET_wb_ann - ET_sat_ann,
    drought_period = assign_drought(water_year)
  ) %>%
  # ── NEW: STRICT PHYSICAL BOUNDS FILTER ──
  filter(
    ET_wb_ann > 0,              # Lower bound: Mass balance failure (precipitation undercatch)
    ET_wb_ann <= MAX_ET_CAP,    # Upper bound: Energy limit (cannot exceed PET proxy)
    ET_sat_ann > 0
  )

# Anomaly
# Recalculate anomalies ONLY on the physically realistic, filtered data
wb_r2_anomaly <- wb_r2_annual %>%
  group_by(site_no) %>%
  mutate(
    ET_wb_mean  = mean(ET_wb_ann,  na.rm = TRUE),
    ET_sat_mean = mean(ET_sat_ann, na.rm = TRUE),
    ET_wb_anom  = ET_wb_ann  - ET_wb_mean,
    ET_sat_anom = ET_sat_ann - ET_sat_mean
  ) %>%
  filter(n() >= 5) %>% # Require at least 5 valid years per station to calculate a meaningful anomaly
  ungroup()

write_csv(wb_r2_annual,  file.path(out_dir, "R2_water_balance_annual.csv"))
write_csv(wb_r2_anomaly, file.path(out_dir, "R2_water_balance_anomaly.csv"))
cat("R2 annual:", nrow(wb_r2_annual), "rows |",
    n_distinct(wb_r2_annual$site_no), "stations\n")
cat("R2 anomaly:", nrow(wb_r2_anomaly), "rows |",
    n_distinct(wb_r2_anomaly$site_no), "stations\n")

r2_ann_r <- cor(wb_r2_annual$ET_wb_ann, wb_r2_annual$ET_sat_ann)
cat("  Annual R²:", round(r2_ann_r^2, 3),
    "| Bias:", round(mean(wb_r2_annual$ET_wb_ann - wb_r2_annual$ET_sat_ann), 1), "mm/yr\n")
if (nrow(wb_r2_anomaly) > 10) {
  r2_anom_r <- cor(wb_r2_anomaly$ET_wb_anom, wb_r2_anomaly$ET_sat_anom)
  cat("  Anomaly R²:", round(r2_anom_r^2, 3), "\n")
}

# ═══════════════════════════════════════════════════════════════════════════
# ── Q/P anomaly — REFERENCE, SITE-MONTH DEMEANING ────────────────────────
# Removes between-site + seasonal confounds → pure inter-annual signal
# ═══════════════════════════════════════════════════════════════════════════

qp_base <- monthly_q %>%
  filter(site_no %in% stations_qp$site_no) %>%
  left_join(met_sub %>% select(site_no, YEAR, MONTH, prcp_sum),
            by = c("site_no", "year" = "YEAR", "month" = "MONTH")) %>%
  filter(prcp_sum > 5) %>%
  mutate(QP_ratio = Q_mm / prcp_sum) %>%
  # Site-month demeaning
  group_by(site_no, month) %>%
  mutate(
    QP_mean_site_month = mean(QP_ratio, na.rm = TRUE),
    QP_sd_site_month   = sd(QP_ratio,   na.rm = TRUE),
    QP_anomaly         = QP_ratio - QP_mean_site_month,
    QP_anomaly_std     = ifelse(QP_sd_site_month > 0,
                                (QP_ratio - QP_mean_site_month) / QP_sd_site_month,
                                NA_real_),
    n_site_month       = n()
  ) %>%
  ungroup() %>%
  filter(n_site_month >= 3) %>%
  mutate(drought_period = assign_drought(water_year)) %>%
  left_join(disturbance_cols, by = c("site_no", "year", "month", "yearmonth")) %>%
  mutate(high_fire_flag = BurnSev34_10yr_pct > 1)

write_csv(qp_base, file.path(out_dir, "QP_anomaly_monthly.csv"))
cat("Q/P anomaly saved:", nrow(qp_base), "rows |",
    n_distinct(qp_base$site_no), "stations (ref, site-month demeaned)\n")

# ═══════════════════════════════════════════════════════════════════════════
# ── Within-site thinning → Q/P slope ────────────────────────────────────
# For each station: QP_anomaly ~ thinning_pct over time
# ═══════════════════════════════════════════════════════════════════════════

qp_within <- qp_base %>%
  filter(
    !is.na(QP_anomaly),
    !is.na(mng_medhigh_10yr_pct),
    high_fire_flag == FALSE
  )

site_slopes <- qp_within %>%
  group_by(site_no) %>%
  filter(n() >= 20, sd(mng_medhigh_10yr_pct) > 0.01) %>%
  summarise(
    n_obs          = n(),
    thinning_sd    = sd(mng_medhigh_10yr_pct),
    thinning_range = max(mng_medhigh_10yr_pct) - min(mng_medhigh_10yr_pct),
    thinning_mean  = mean(mng_medhigh_10yr_pct),
    slope          = coef(lm(QP_anomaly ~ mng_medhigh_10yr_pct))[2],
    r_squared      = summary(lm(QP_anomaly ~ mng_medhigh_10yr_pct))$r.squared,
    p_value        = summary(lm(QP_anomaly ~ mng_medhigh_10yr_pct))$coefficients[2, 4],
    .groups        = "drop"
  )

write_csv(site_slopes, file.path(out_dir, "QP_within_site_thinning_slopes.csv"))
cat("Within-site slopes:", nrow(site_slopes), "stations\n")
cat("  Positive:", sum(site_slopes$slope > 0),
    "| Negative:", sum(site_slopes$slope < 0), "\n")
cat("  Significant (p<0.05):", sum(site_slopes$p_value < 0.05), "\n")
cat("  Median slope:", round(median(site_slopes$slope), 4), "\n")

# ═══════════════════════════════════════════════════════════════════════════
# ── Q/P anomaly — WILDFIRE ───────────────────────────────────────────────
# Parallel to thinning QP above.
# Purpose: test whether cumulative high-severity burn extent is associated
#          with elevated runoff efficiency at catchment scale.
# Key difference from thinning block: NO high_fire_flag exclusion —
#   fire is now the subject, not a confound to remove.
# BurnSev34_10yr_pct used as predictor, matching HUC8 CPI predictor definition.
# Same reference stations (stations_qp) and site-month demeaning as thinning.
# ═══════════════════════════════════════════════════════════════════════════

# ── Diagnostic: burn coverage across reference stations ──────────────────
fire_coverage <- qp_base %>%
  group_by(site_no) %>%
  summarise(
    n_months_total     = n(),
    n_months_burned    = sum(BurnSev34_10yr_pct > 0,  na.rm = TRUE),
    n_months_burned1pct = sum(BurnSev34_10yr_pct > 1, na.rm = TRUE),
    n_months_burned5pct = sum(BurnSev34_10yr_pct > 5, na.rm = TRUE),
    max_burn_pct       = max(BurnSev34_10yr_pct, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n── Wildfire QP coverage (reference stations) ──\n")
cat("Stations with any burn (BurnSev34 > 0):",
    sum(fire_coverage$n_months_burned > 0), "\n")
cat("Stations with >1% burn at some point:",
    sum(fire_coverage$max_burn_pct > 1), "\n")
cat("Stations with >5% burn at some point:",
    sum(fire_coverage$max_burn_pct > 5), "\n")
cat("Median max_burn_pct across all stations:",
    round(median(fire_coverage$max_burn_pct), 3), "\n")
cat("Distribution of max_burn_pct (quantiles):\n")
print(quantile(fire_coverage$max_burn_pct, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1.0),
               na.rm = TRUE))

# ── Build wildfire QP dataset ─────────────────────────────────────────────
# qp_base already has site-month demeaned QP_anomaly and BurnSev34_10yr_pct.
# Include all rows (drought and non-drought); no high_fire_flag filter applied.
qp_fire <- qp_base %>%
  filter(
    !is.na(QP_anomaly),
    !is.na(BurnSev34_10yr_pct),
    is.finite(QP_anomaly),
    !is.na(drought_period)
  )

cat("\nWildfire QP rows:", nrow(qp_fire),
    "| Stations:", n_distinct(qp_fire$site_no), "\n")
cat("Rows with BurnSev34 > 0:", sum(qp_fire$BurnSev34_10yr_pct > 0), "\n")
cat("Rows with BurnSev34 > 1:", sum(qp_fire$BurnSev34_10yr_pct > 1), "\n")

write_csv(qp_fire, file.path(out_dir, "QP_anomaly_wildfire_monthly.csv"))
cat("Wildfire QP anomaly saved:", nrow(qp_fire), "rows\n")

# ═══════════════════════════════════════════════════════════════════════════
# ── R4: D/Q ratio ────────────────────────────────────────────────────────
# ═══════════════════════════════════════════════════════════════════════════

consump_cols <- intersect(
  c("Domestic_diverted", "Irrigation_diverted", "Municipal_diverted"),
  names(wtr_nldi_wide)
)
hydro_cols <- intersect(c("Power_diverted"), names(wtr_nldi_wide))

cat("Consumptive columns found:", paste(consump_cols, collapse = ", "), "\n")
cat("Hydropower columns found:",  paste(hydro_cols,   collapse = ", "), "\n")

wtr_nldi_wide <- wtr_nldi_wide %>%
  mutate(
    consump_mm = rowSums(select(., all_of(consump_cols)), na.rm = TRUE),
    hydro_mm   = rowSums(select(., all_of(hydro_cols)),   na.rm = TRUE)
  )

dq_r4 <- monthly_q %>%
  filter(site_no %in% stations_r4$site_no) %>%
  left_join(
    wtr_nldi_wide %>%
      rename(site_no = site_no_nldi,
             year    = YEAR2,
             month   = month.num) %>%
      select(site_no, year, month, yearmonth, consump_mm, hydro_mm),
    by = c("site_no", "year", "month", "yearmonth")
  ) %>%
  mutate(
    consump_mm   = replace_na(consump_mm, 0),
    hydro_mm     = replace_na(hydro_mm,   0),
    total_div_mm = consump_mm + hydro_mm,
    DQ_consump   = pmin(consump_mm   / pmax(Q_mm, 0.001), 1),
    DQ_hydro     = pmin(hydro_mm     / pmax(Q_mm, 0.001), 1),
    DQ_total     = pmin(total_div_mm / pmax(Q_mm, 0.001), 1),
    drought_period = assign_drought(water_year)
  ) %>%
  filter(Q_mm > 0) %>%
  left_join(disturbance_cols, by = c("site_no", "year", "month", "yearmonth"))

write_csv(dq_r4, file.path(out_dir, "R4_diversion_streamflow_ratio.csv"))
cat("R4 D/Q ratio rows:", nrow(dq_r4),
    "| Stations:", n_distinct(dq_r4$site_no), "\n")

# ── Summary ───────────────────────────────────────────────────────────────
cat("\n=== Summary ===\n")
cat("R2 stations:", n_distinct(wb_r2$site_no), "\n")
cat("QP stations (ref, site-month demeaned):", n_distinct(qp_base$site_no), "\n")
cat("QP within-site slopes:", nrow(site_slopes), "\n")
cat("R4 stations:", n_distinct(dq_r4$site_no), "\n")
cat("Outputs saved to:", out_dir, "\n")
