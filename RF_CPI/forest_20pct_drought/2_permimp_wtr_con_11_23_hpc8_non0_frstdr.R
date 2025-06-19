#https://www.r-bloggers.com/2018/06/be-aware-of-bias-in-rf-variable-importance-metrics/

rm(list=ls())

library(dplyr)
library(party)
library(permimp)
library(reshape2)
library(Rmisc)
#library(edarf)

#!!!! 0.80; 0.90; 0.95 
thrd <- 0.80
thrd.name <- 080

input.dir <- "/projects/mich9173/CA_wtr_div/"
#input.dir <- "~/data-store/home/mgchung/CA_wtr_div/"
#input.dir <- "E:/CA_data_analysis/new_analysis_040325/"

setwd(paste0(input.dir,"input"))
load(paste0("rf_con_all_huc8_non0_4_1000_pct_frstdr.RData")) # rf.con; rf.con.prj; rf.con.nprj

set.seed(160617)
# variable importance
# threshold should be larger than 0.8
imp.con <- permimp(rf.con, conditional = TRUE, 
                   threshold=thrd,
                   progressBar=TRUE)

## all values
# maybe wrong: all 0 included (not na.rm=TRUE)
imp.con.df <- as.data.frame(imp.con$values)
imp.con.df.nm <- tibble::rownames_to_column(imp.con.df, "Feature")
names(imp.con.df.nm)[2] <- "value"


## per-tree values
imp.con.perdf <- as.data.frame(imp.con$perTree)
imp.con.perdf[imp.con.perdf==0] <- NA

# melt
imp.con.perdf.m <- melt(imp.con.perdf, variable.name="Feature",
                       value.name = "value")

# mean & confidence interval
imp.con.perdf.m.ci <- imp.con.perdf.m %>%
  group_by(Feature) %>%
  dplyr::summarize(avg = mean(value, na.rm=TRUE), #
                   sd.avg = sd(value, na.rm=TRUE), #
                   n.avg = sum(!is.na(value)),
                   se.avg = sd.avg / sqrt(n.avg),
                   avg.pkg = CI(na.omit(value), ci=0.95)[2],
                   lower.ci.pkg = CI(na.omit(value), ci=0.95)[3],
                   upper.ci.pkg = CI(na.omit(value), ci=0.95)[1]) %>%
  mutate(
    lower.ci.avg = avg - qt(1 - (0.05 / 2), n.avg - 1) * se.avg,
    upper.ci.avg = avg + qt(1 - (0.05 / 2), n.avg - 1) * se.avg
  )

## plot
## barplot with visualization of the distribution: an
## interval between the .25 and .75 quantiles of the per 
## Tree values is added to the plot
# plot(imp.con, type = "bar", interval = "sd") #quantile

print("Varimp done")

# write.csv
#!!!! 0.80; 0.90; 0.95 
setwd(paste0(input.dir,"output/varimp/frstdr/"))

write.csv(imp.con.perdf, paste0("permimp_cond_rf_con_all_huc8_4_1000_thrld",thrd.name,"_non0_raw_pct_frstdr.csv"), row.names=F)

write.csv(imp.con.perdf.m.ci, paste0("permimp_cond_rf_con_all_huc8_4_1000_thrld",thrd.name,"_non0_pct_frstdr.csv"), row.names=F)

write.csv(imp.con.df.nm, paste0("permimp_cond_avg_rf_con_all_huc8_4_1000_thrld",thrd.name,"_non0_pct_frstdr.csv"), row.names=F)

print("Write.csv done")


