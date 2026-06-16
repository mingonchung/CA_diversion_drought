# CPI Step 2: permimp — Non-forest watersheds, Drought years, Hydropower (Power_diverted)
# Reference: Debeer & Strobl (2020) BMC Bioinformatics 21:307
# https://www.r-bloggers.com/2018/06/be-aware-of-bias-in-rf-variable-importance-metrics/

rm(list = ls())

library(dplyr)
library(party)
library(permimp)
library(reshape2)
library(Rmisc)

#!!!! threshold: 0.80 per Debeer & Strobl (2020) recommendation
thrd      <- 0.80
thrd.name <- "080"

input.dir <- "/projects/mich9173/CA_wtr_div/"
#input.dir <- "~/data-store/home/mgchung/CA_wtr_div/"
#input.dir <- "E:/CA_data_analysis/new_analysis_040325/"

setwd(paste0(input.dir, "input"))
load(paste0("rf_pw_all_huc8_non0_6_1000_pct_nfrstdr_11_24.RData"))

set.seed(160617)

## Conditional Permutation Importance
imp.pw <- permimp(rf.pw,
                  conditional  = TRUE,
                  threshold    = thrd,
                  progressBar  = TRUE)

############################################
## Overall CPI values (mean across all trees, computed internally by permimp)
imp.pw.df    <- as.data.frame(imp.pw$values)
imp.pw.df.nm <- tibble::rownames_to_column(imp.pw.df, "Feature")
names(imp.pw.df.nm)[2] <- "value"

############################################
## Per-tree CPI values
## Zeros are retained: a zero in a given tree means the variable was not
## selected for splitting in that tree (structural zero, not missing data).
## Replacing zeros with NA would inflate mean importance and remove
## information about how consistently a variable contributes across the non-forest.
imp.pw.perdf <- as.data.frame(imp.pw$perTree)

## Melt to long format
imp.pw.perdf.m <- melt(imp.pw.perdf,
                       variable.name = "Feature",
                       value.name    = "value")

## Mean and 95% CI across trees (zeros retained)
imp.pw.perdf.m.ci <- imp.pw.perdf.m %>%
  group_by(Feature) %>%
  dplyr::summarize(
    avg          = mean(value, na.rm = TRUE),
    sd.avg       = sd(value, na.rm = TRUE),
    n.avg        = sum(!is.na(value)),
    se.avg       = sd.avg / sqrt(n.avg),
    avg.pkg      = CI(na.omit(value), ci = 0.95)[2],
    lower.ci.pkg = CI(na.omit(value), ci = 0.95)[3],
    upper.ci.pkg = CI(na.omit(value), ci = 0.95)[1],
    .groups = "drop"
  ) %>%
  mutate(
    lower.ci.avg = avg - qt(1 - (0.05 / 2), n.avg - 1) * se.avg,
    upper.ci.avg = avg + qt(1 - (0.05 / 2), n.avg - 1) * se.avg
  )

print("Varimp done")

############################################
## Write outputs
setwd(paste0(input.dir, "output/varimp/nfrstdr/"))

write.csv(imp.pw.perdf,
          paste0("permimp_cond_rf_pw_all_huc8_6_1000_thrld", thrd.name,
                 "_non0_raw_pct_nfrstdr_11_24.csv"),
          row.names = FALSE)

write.csv(imp.pw.perdf.m.ci,
          paste0("permimp_cond_rf_pw_all_huc8_6_1000_thrld", thrd.name,
                 "_non0_pct_nfrstdr_11_24.csv"),
          row.names = FALSE)

write.csv(imp.pw.df.nm,
          paste0("permimp_cond_avg_rf_pw_all_huc8_6_1000_thrld", thrd.name,
                 "_non0_pct_nfrstdr_11_24.csv"),
          row.names = FALSE)

print("Write.csv done")
