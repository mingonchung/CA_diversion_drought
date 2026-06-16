## Stage 3 — Script 1: Spatial operations (REVISED)
## Changes from original:
##   - Exports area_total_m2 (NLDI polygon area) for consistent mm conversions
##   - Basin size filters: >= 50 km2 for R2/QP, 50-5000 km2 for R4
##   - Separate station list for QP anomaly (reference only, same as R2)
##   - eWRIMS vol_mm uses NLDI polygon area (not GAGES-II area_m2)
## Author: Min Gon Chung

rm(list = ls())

library(sf)
library(dplyr)
library(lubridate)
library(readr)
library(stringr)
library(reshape2)
library(tidyr)

sf_use_s2(FALSE)

# ── Directories ───────────────────────────────────────────────────────────────
nwis_dir     <- "/projects/mich9173/CA_wtr_div/NWIS"
gee_dir      <- "/projects/mich9173/CA_wtr_div/GEE"
nlcd_dir     <- "/projects/mich9173/CA_wtr_div/NLCD"
mgmt_dir     <- "/projects/mich9173/CA_wtr_div/Management"
nldi_dir     <- "/projects/mich9173/CA_wtr_div/NWIS"
wtr_dir      <- "/projects/mich9173/CA_wtr_div/input/point"
shp_dir      <- "/projects/mich9173/CA_wtr_div/Wildfire/USFS_RAVG/shp"
usfs_int_dir <- "/projects/mich9173/CA_wtr_div/Wildfire/USFS/shp_int"
ravg_int_dir <- "/projects/mich9173/CA_wtr_div/Wildfire/USFS_RAVG/tif/shp_re_int"
out_dir      <- "/projects/mich9173/CA_wtr_div/output/nldi"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ── Basin size thresholds (km2) ───────────────────────────────────────────
MIN_AREA_KM2 <- 50     # excludes noisy small basins
MAX_AREA_KM2 <- 5000   # excludes huge downstream basins (CA HUC8 avg ~2120 km2)

# ═══════════════════════════════════════════════════════════════════════════
# ── STEP 1: Load base datasets and establish projected CRS ───────────────
# ═══════════════════════════════════════════════════════════════════════════

ca_boundary <- st_read(
  file.path(shp_dir, "CA_State_TIGER2016_proj.shp"), quiet = TRUE
) %>% st_make_valid()

if (st_is_longlat(ca_boundary)) {
  stop("CA boundary is geographic CRS — needs projected version")
}
proj_crs <- st_crs(ca_boundary)
cat("Reference projected CRS:", proj_crs$input, "\n")

meta <- read_csv(file.path(nwis_dir, "station_metadata_all_CA.csv"),
                 col_types = cols(site_no = col_character()))

nldi_sf <- st_read(file.path(nldi_dir, "main_gauge_catchment_polygons.shp"),
                   quiet = TRUE)
cat("NLDI polygons raw:", nrow(nldi_sf), "\n")

nlcd_nldi <- read_csv(file.path(nlcd_dir, "nldi_NLCD_forest_pct_2010_2024.csv"),
                      col_types = cols(site_no = col_character()))

# ── Compute CA coverage and NLDI polygon area ────────────────────────────
nldi_proj <- st_transform(st_make_valid(nldi_sf), proj_crs) %>%
  mutate(area_total_m2 = as.numeric(st_area(.)))

nldi_ca_int <- st_intersection(nldi_proj, ca_boundary) %>%
  mutate(area_ca_m2 = as.numeric(st_area(.))) %>%
  st_drop_geometry() %>%
  select(site_no, area_ca_m2)

ca_coverage <- nldi_proj %>%
  st_drop_geometry() %>%
  select(site_no, area_total_m2) %>%
  left_join(nldi_ca_int, by = "site_no") %>%
  mutate(
    area_ca_m2     = replace_na(area_ca_m2, 0),
    pct_in_ca      = area_ca_m2 / area_total_m2 * 100,
    area_total_km2 = area_total_m2 / 1e6
  )

write_csv(ca_coverage, file.path(out_dir, "nldi_ca_coverage_pct.csv"))

# ═══════════════════════════════════════════════════════════════════════════
# ── STEP 2: CA coverage filter — 80% threshold ───────────────────────────
# ═══════════════════════════════════════════════════════════════════════════

sites_ca_80 <- ca_coverage %>% filter(pct_in_ca >= 80) %>% pull(site_no)

cat("NLDI polygons total:           ", nrow(nldi_sf), "\n")
cat("  >= 80% in CA (both R2/R4):  ", length(sites_ca_80), "\n")
cat("  < 80% in CA (excluded):     ", sum(ca_coverage$pct_in_ca < 80), "\n")

nldi_r2 <- nldi_proj %>% filter(site_no %in% sites_ca_80)
nldi_r4 <- nldi_proj %>% filter(site_no %in% sites_ca_80)

# ═══════════════════════════════════════════════════════════════════════════
# ── STEP 3b: Thinning intersection ───────────────────────────────────────
# ═══════════════════════════════════════════════════════════════════════════

process_facts <- function(mgmt, nldi, intensity_xwalk) {
  mgmt <- st_transform(st_make_valid(mgmt), st_crs(nldi))
  mgmt <- st_make_valid(st_buffer(mgmt, 0))
  nldi <- st_make_valid(st_buffer(nldi, 0))
  int  <- st_intersection(mgmt, nldi)
  int$DATE_COMPL <- as.Date(int$DATE_COMPL, format = "%Y-%m-%d")
  int  <- int %>%
    filter(year(DATE_COMPL) > 1999) %>%
    mutate(area_km2 = as.numeric(st_area(.)) / 1e6) %>%
    st_drop_geometry()
  int  <- inner_join(int, intensity_xwalk, by = c("TREATMENT_" = "disttype"))
  int %>%
    filter(intensity %in% c("High", "Medium")) %>%
    group_by(site_no, DATE_COMPL, intensity) %>%
    summarise(total_area_km2 = sum(area_km2, na.rm = TRUE), .groups = "drop")
}

process_calfire <- function(mgmt, nldi, intensity_xwalk, date_col, join_col) {
  mgmt <- st_transform(st_make_valid(mgmt), st_crs(nldi))
  mgmt <- st_make_valid(st_buffer(mgmt, 0))
  nldi <- st_make_valid(st_buffer(nldi, 0))
  int  <- st_intersection(mgmt, nldi)
  int[[date_col]] <- as.Date(int[[date_col]], format = "%Y-%m-%d")
  int  <- int %>%
    filter(year(.data[[date_col]]) > 1999) %>%
    mutate(area_km2 = as.numeric(st_area(.)) / 1e6,
           DATE     = .data[[date_col]]) %>%
    st_drop_geometry()
  int  <- inner_join(int, intensity_xwalk, by = setNames("disttype", join_col))
  int %>%
    filter(intensity %in% c("High", "Medium")) %>%
    group_by(site_no, DATE, intensity) %>%
    summarise(total_area_km2 = sum(area_km2, na.rm = TRUE), .groups = "drop")
}

facts_hz        <- st_read(file.path(mgmt_dir, "shp_CA/Actv_HazFuelTrt_PL_CA.shp"),  quiet = TRUE)
facts_th        <- st_read(file.path(mgmt_dir, "shp_CA/Actv_TimberHarvest_CA.shp"),   quiet = TRUE)
intensity_facts <- read.csv(file.path(mgmt_dir, "crosswalk/FACTS_list.csv"),
                            stringsAsFactors = FALSE)

facts_nldi <- bind_rows(
  process_facts(facts_hz, nldi_r2, intensity_facts) %>% mutate(source = "HazFuel"),
  process_facts(facts_th, nldi_r2, intensity_facts) %>% mutate(source = "Timber")
)

calfire_ntmp      <- st_read(file.path(mgmt_dir, "shp_CA/CAL_FIRE_Nonindustrial_Timber_Management_Plans_TA83.shp"), quiet = TRUE)
calfire_thp       <- st_read(file.path(mgmt_dir, "shp_CA/CAL_FIRE_Timber_Harvesting_Plans_TA83.shp"),              quiet = TRUE)
intensity_calfire <- read.csv(file.path(mgmt_dir, "crosswalk/CALFIRE_list.csv"),
                              stringsAsFactors = FALSE)

calfire_nldi <- bind_rows(
  process_calfire(calfire_ntmp, nldi_r2, intensity_calfire, "APPROVED",  "SILVI_1") %>% mutate(source = "NTMP"),
  process_calfire(calfire_thp,  nldi_r2, intensity_calfire, "COMPLETED", "SILVI_1") %>% mutate(source = "THP")
)

mng_nldi <- bind_rows(
  facts_nldi %>% rename(DATE = DATE_COMPL),
  calfire_nldi
) %>%
  mutate(
    year      = year(DATE),
    month     = month(DATE),
    yearmonth = str_c(year, month)
  )

write_csv(mng_nldi, file.path(out_dir, "thinning_nldi_intersect.csv"))
cat("Thinning intersection saved:", nrow(mng_nldi), "rows\n")

# ═══════════════════════════════════════════════════════════════════════════
# ── STEP 3b-2: Wildfire intersection ─────────────────────────────────────
# ═══════════════════════════════════════════════════════════════════════════

process_wildfire_year <- function(fire_sf, nldi, sev_col, year_val) {
  fire_sf <- st_transform(st_make_valid(fire_sf), st_crs(nldi))
  fire_sf <- st_collection_extract(fire_sf, "POLYGON")
  fire_sf <- st_make_valid(st_buffer(fire_sf, 0))
  nldi    <- st_make_valid(st_buffer(nldi, 0))
  int     <- st_intersection(fire_sf, nldi)
  if (nrow(int) == 0) return(NULL)
  int %>%
    mutate(
      area_km2  = as.numeric(st_area(.)) / 1e6,
      CONT_DATE = case_when(
        !is.na(as.Date(as.character(CONT_DATE), format = "%Y-%m-%d")) ~
          as.Date(as.character(CONT_DATE), format = "%Y-%m-%d"),
        !is.na(as.Date(as.character(CONT_DATE), format = "%Y/%m/%d")) ~
          as.Date(as.character(CONT_DATE), format = "%Y/%m/%d"),
        !is.na(as.Date(as.character(CONT_DATE), format = "%m/%d/%Y")) ~
          as.Date(as.character(CONT_DATE), format = "%m/%d/%Y"),
        TRUE ~ as.Date(NA)
      ),
      BURNSEV4  = case_when(
        .data[[sev_col]] == 1            ~ 0L,
        .data[[sev_col]] %in% c(2, 3)   ~ 1L,
        .data[[sev_col]] == 4            ~ 2L,
        .data[[sev_col]] == 5            ~ 3L,
        .data[[sev_col]] %in% c(6, 7)   ~ 4L,
        TRUE                             ~ NA_integer_
      )
    ) %>%
    filter(!is.na(BURNSEV4), !is.na(CONT_DATE)) %>%
    st_drop_geometry() %>%
    group_by(site_no, CONT_DATE, BURNSEV4) %>%
    summarise(total_burned_km2 = sum(area_km2, na.rm = TRUE), .groups = "drop") %>%
    mutate(year = year_val)
}

fire_usfs_list <- list()
for (yr in 2000:2011) {
  shp_path <- file.path(usfs_int_dir,
                        paste0("VegBurnSeverityBA_intersect_", yr, ".shp"))
  if (!file.exists(shp_path)) { message("USFS not found, skipping: ", yr); next }
  fire_sf <- st_read(shp_path, quiet = TRUE)
  result  <- process_wildfire_year(fire_sf, nldi_r2, "BURNSEV", yr)
  if (!is.null(result)) fire_usfs_list[[as.character(yr)]] <- result
  message("USFS wildfire processed: ", yr)
}
fire_usfs <- bind_rows(fire_usfs_list)
cat("USFS pre-2012 wildfire rows:", nrow(fire_usfs), "\n")

fire_ravg_list <- list()
for (yr in 2012:2024) {
  shp_path <- file.path(ravg_int_dir,
                        paste0("rav_", yr, "_ba7_CA_re_int.shp"))
  if (!file.exists(shp_path)) { message("RAVG not found, skipping: ", yr); next }
  fire_sf <- st_read(shp_path, quiet = TRUE)
  result  <- process_wildfire_year(fire_sf, nldi_r2, "gridcode", yr)
  if (!is.null(result)) fire_ravg_list[[as.character(yr)]] <- result
  message("RAVG wildfire processed: ", yr)
}
fire_ravg <- bind_rows(fire_ravg_list)
cat("RAVG 2012-2024 wildfire rows:", nrow(fire_ravg), "\n")

fire_all <- bind_rows(
  fire_usfs %>% filter(year < 2012),
  fire_ravg %>% filter(year >= 2012)
) %>%
  mutate(
    year      = year(CONT_DATE),
    month     = month(CONT_DATE),
    yearmonth = str_c(year, month)
  )

cat("Combined wildfire rows:", nrow(fire_all),
    "| Years:", min(fire_all$year), "-", max(fire_all$year), "\n")
write_csv(fire_all, file.path(out_dir, "wildfire_nldi_intersect.csv"))

# ═══════════════════════════════════════════════════════════════════════════
# ── STEP 3a: eWRIMS spatial join ─────────────────────────────────────────
# ═══════════════════════════════════════════════════════════════════════════
# ── STEP 3a: eWRIMS spatial join (REVISED) ───────────────────────────────
wtr_pt <- read_csv(file.path(wtr_dir, "CA_wtr_HUC8_2010_2024_point.csv"),
                   col_types = cols(.default = col_character())) %>%
  mutate(
    LONGITUDE = as.numeric(LONGITUDE),
    LATITUDE  = as.numeric(LATITUDE),
    acre.feet = as.numeric(acre.feet),
    YEAR2     = as.integer(YEAR2),
    month.num = as.integer(month.num)
  ) %>%
  filter(!is.na(LONGITUDE), !is.na(LATITUDE))

wtr_sf <- st_as_sf(wtr_pt, coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>%
  st_transform(proj_crs)

# Use NLDI polygon area for mm conversion
nldi_r4_valid <- st_make_valid(nldi_r4)

# 1:many join — each point duplicated for every NLDI polygon it falls within
wtr_nldi <- st_join(wtr_sf, nldi_r4_valid[, c("site_no", "area_total_m2")],
                    join = st_within)

cat("Columns after st_join:", paste(names(wtr_nldi), collapse = ", "), "\n")

# NO dedup — keep 1:many so each diversion counts in every containing catchment
# This is correct for cumulative D/Q: Q at gauge integrates all upstream diversions
wtr_nldi_clean <- wtr_nldi %>%
  st_drop_geometry() %>%
  filter(!is.na(site_no)) %>%
  rename(site_no_nldi = site_no)

cat("eWRIMS points total:             ", nrow(wtr_sf), "\n")
cat("Matched eWRIMS points:           ", n_distinct(wtr_nldi_clean$pt_id), "\n")
cat("eWRIMS-NLDI joins (1:many):      ", nrow(wtr_nldi_clean), "\n")
cat("Unique NLDI catchments matched:  ", n_distinct(wtr_nldi_clean$site_no_nldi), "\n")
cat("Expansion factor (joins/matched points): ",
    round(nrow(wtr_nldi_clean) / n_distinct(wtr_nldi_clean$pt_id), 2), "\n")

# Aggregate: sum all diversions falling within each catchment-month
wtr_nldi_ag <- wtr_nldi_clean %>%
  mutate(vol_m3 = acre.feet * 1233.48) %>%
  group_by(site_no_nldi, YEAR2, month.num, use_type, area_total_m2) %>%
  summarise(
    vol_acft = sum(acre.feet, na.rm = TRUE),
    vol_m3   = sum(vol_m3,    na.rm = TRUE),
    n_points = n(),
    .groups  = "drop"
  ) %>%
  mutate(
    vol_mm    = vol_m3 / area_total_m2 * 1000,
    yearmonth = str_c(YEAR2, month.num)
  )

wtr_nldi_wide <- dcast(
  wtr_nldi_ag,
  site_no_nldi + YEAR2 + month.num + yearmonth ~ use_type,
  value.var     = "vol_mm",
  fun.aggregate = sum
)

cat("Diversion columns:", paste(names(wtr_nldi_wide), collapse = ", "), "\n")
write_csv(wtr_nldi_wide, file.path(out_dir, "diversion_nldi_monthly_mm.csv"))
cat("Diversion aggregation saved:", nrow(wtr_nldi_wide), "rows\n")

# ═══════════════════════════════════════════════════════════════════════════
# ── STEP 3c: Forested catchment classification ────────────────────────────
# ═══════════════════════════════════════════════════════════════════════════

forest_flag <- nlcd_nldi %>%
  group_by(site_no) %>%
  summarise(
    mean_forest_pct = mean(forest_cover_pct, na.rm = TRUE),
    forested_yrs    = sum(forest_cover_pct >= 20, na.rm = TRUE),
    total_yrs       = n(),
    is_forested     = mean_forest_pct >= 20,
    .groups = "drop"
  )

cat("Forested catchments (mean >= 20%):", sum(forest_flag$is_forested), "\n")

# ═══════════════════════════════════════════════════════════════════════════
# ── STEP 3d: Station selection with basin size filters ───────────────────
# ═══════════════════════════════════════════════════════════════════════════

meta_full <- meta %>%
  mutate(site_no = as.character(site_no)) %>%
  left_join(forest_flag, by = "site_no") %>%
  left_join(ca_coverage, by = "site_no")

# R2: unregulated forested, >= 80% CA, >= 50 km2
stations_r2 <- meta_full %>%
  filter(
    is_forested    == TRUE,
    site_no        %in% sites_ca_80,
    area_total_km2 >= MIN_AREA_KM2,
    hcdn_2009 == 1 | (gagesii_class == "Ref" & ndams_upstream == 0)
  )
cat("R2 validation stations (>=", MIN_AREA_KM2, "km2):", nrow(stations_r2), "\n")

# QP anomaly: same reference stations as R2
stations_qp <- stations_r2
cat("QP anomaly stations (reference only):", nrow(stations_qp), "\n")

# R4: forested with diversions, >= 80% CA, 50-5000 km2
sites_with_diversion <- unique(wtr_nldi_wide$site_no_nldi)

stations_r4 <- meta_full %>%
  filter(
    is_forested    == TRUE,
    site_no        %in% sites_ca_80,
    site_no        %in% sites_with_diversion,
    area_total_km2 >= MIN_AREA_KM2,
    area_total_km2 <= MAX_AREA_KM2
  )
cat("R4 diversion-ratio stations (", MIN_AREA_KM2, "-",
    MAX_AREA_KM2, "km2):", nrow(stations_r4), "\n")

write_csv(meta_full,    file.path(out_dir, "station_metadata_nldi_classified.csv"))
write_csv(stations_r2,  file.path(out_dir, "stations_r2_validation.csv"))
write_csv(stations_qp,  file.path(out_dir, "stations_qp_anomaly.csv"))
write_csv(stations_r4,  file.path(out_dir, "stations_r4_diversion_ratio.csv"))

cat("\n=== Script 1 complete ===\n")
cat("Outputs saved to:", out_dir, "\n")
cat("Basin size filters: min =", MIN_AREA_KM2, "km2, max =", MAX_AREA_KM2, "km2\n")
cat("Area denominator: NLDI polygon area (area_total_m2)\n")
