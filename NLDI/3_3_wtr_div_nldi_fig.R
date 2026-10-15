## 3_3_wtr_div_nldi_fig.R
## Figures from the NLDI outputs of 1_3b_wtr_div_nldi_analysis.R:
##   - Supplementary Fig. 10: annual OpenET vs water-balance ET (PPT - Q)
##   - Fig. 4: Q/PPT anomaly vs thinning and burn extent, drought and non-drought
##   - Fig. 5: D/Q ratio by period, station as the unit of analysis
##   - Source Data for Figs 4 and 5
## Editorial revision (Oct 2026, NCOMMS-25-101460B):
##   - Fig. 4: R2 and p-values computed on the plotted range (thinning <= 15%,
##     burn <= 30%), the same data used by the LOESS and linear fits.
##     Source Data export of plotted points.
##   - Fig. 5: station as the unit of analysis. Points show station-period
##     mean D/Q, bars the mean across stations, error bars the t-based 95% CI
##     (n = stations). Each panel uses stations with that diversion type.
##     Source Data export.
##   - Detaches Rmisc/plyr at start: plyr masks dplyr::summarise() and
##     dplyr::mutate() when 3_2 runs first in the same R session.
## Author: Min Gon Chung

rm(list = ls())

# Rmisc (used in 3_2) attaches plyr, whose summarise() and mutate() mask
# dplyr's. Detach both if this script runs in the same R session.
for (pkg in c("package:Rmisc", "package:plyr")) {
  if (pkg %in% search()) detach(pkg, character.only = TRUE)
}

library(dplyr)
library(lubridate)
library(readr)
library(stringr)
library(ggplot2)
library(tidyr)
library(scales)

out_dir <- "D:/CA_data_analysis/new_analysis_040325/output/csv/nldi"
fig_dir <- "D:/CA_data_analysis/new_analysis_040325/output/fig/nldi/"
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

# ── Load data ──────────────────────────────────────────────────────────────
wb_r2_annual  <- read_csv(file.path(out_dir, "R2_water_balance_annual.csv"),
                          col_types = cols(site_no = col_character()))
qp_base       <- read_csv(file.path(out_dir, "QP_anomaly_monthly.csv"),
                          col_types = cols(site_no = col_character(),
                                           yearmonth = col_character()))
dq_r4         <- read_csv(file.path(out_dir, "R4_diversion_streamflow_ratio.csv"),
                          col_types = cols(site_no = col_character(),
                                           yearmonth = col_character()))

# ── Drought period assignment (manuscript-aligned) ────────────────────────
assign_drought <- function(wy) {
  case_when(
    wy == 2011            ~ "Pre-drought",
    wy %in% 2012:2015     ~ "Drought 2012\u20132015",
    wy %in% 2016:2019     ~ "Post-drought 2016\u20132019",
    wy %in% 2020:2022     ~ "Drought 2020\u20132022",
    wy %in% 2023:2024     ~ "Post-drought 2023\u20132024",
    TRUE                  ~ NA_character_
  )
}

drought_levels <- c("Pre-drought",
                    "Drought 2012\u20132015",
                    "Post-drought 2016\u20132019",
                    "Drought 2020\u20132022",
                    "Post-drought 2023\u20132024")

drought_colors <- c(
  "Pre-drought"                      = "grey60",
  "Drought 2012\u20132015"           = "#f4a582",
  "Post-drought 2016\u20132019"      = "#92c5de",
  "Drought 2020\u20132022"           = "#d6604d",
  "Post-drought 2023\u20132024"      = "#4393c3"
)

# ═══════════════════════════════════════════════════════════════════════════
# ── Supplementary Fig. 10: Annual P-Q vs OpenET ───────────────────────────
# ═══════════════════════════════════════════════════════════════════════════

# 1. Ensure we use the shortened drought labels for consistency across all figures
drought_levels_short <- c("Pre-drought",
                          "Drought 2012-15",
                          "Post-drought 2016-19",
                          "Drought 2020-22",
                          "Post-drought 2023-24")

drought_colors_short <- c(
  "Pre-drought"          = "grey60",
  "Drought 2012-15"      = "#f4a582",
  "Post-drought 2016-19" = "#92c5de",
  "Drought 2020-22"      = "#d6604d",
  "Post-drought 2023-24" = "#4393c3"
)

assign_drought_short <- function(wy) {
  case_when(
    wy == 2011            ~ "Pre-drought",
    wy %in% 2012:2015     ~ "Drought 2012-15",
    wy %in% 2016:2019     ~ "Post-drought 2016-19",
    wy %in% 2020:2022     ~ "Drought 2020-22",
    wy %in% 2023:2024     ~ "Post-drought 2023-24",
    TRUE                  ~ NA_character_
  )
}

# 2. Prepare Data
wb_ann_plot <- wb_r2_annual %>%
  mutate(period = factor(assign_drought_short(water_year), levels = drought_levels_short)) %>%
  filter(!is.na(period))

# 3. Calculate Statistics (R2, p-value ONLY)
cor_test <- cor.test(wb_ann_plot$ET_wb_ann, wb_ann_plot$ET_sat_ann)
r2_ann   <- cor_test$estimate^2
p_val    <- cor_test$p.value

# Format the p-value string
p_str <- ifelse(p_val < 0.001, "italic(p) < 0.001",
                sprintf("italic(p) == '%.3f'", p_val))

# Create parseable expressions for ggplot annotation (italic scalars)
stat_label1 <- sprintf("italic(R)^2 == '%.2f'", r2_ann)


# 4. Build the Plot
fig1a <- ggplot(wb_ann_plot, aes(x = ET_wb_ann, y = ET_sat_ann)) +
  geom_point(aes(color = period), alpha = 0.6, size = 2.5, shape = 16) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "black", linewidth = 1) +
  scale_x_continuous(limits = c(0, 1250), expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 1250), expand = c(0,0)) +

  # Using LOESS to capture non-linear saturation limits of OpenET
  geom_smooth(method = "lm", color = "black", se = FALSE, linewidth = 1.5, alpha = 0.2, span = 0.8) +
  geom_smooth(method = "loess", color = "red", se = TRUE, linetype = "dashed", linewidth = 1.5, alpha = 0.2, span = 0.8) +

  # Add Statistical Annotations in the bottom-right corner
  # x = 1200 keeps it inside the 1250 limit; hjust = 1 right-aligns the text
  annotate("text", x = 1200, y = 150, label = stat_label1, parse = TRUE, hjust = 1, size = 7) +
  annotate("text", x = 1200, y = 50, label = p_str, parse = TRUE, hjust = 1, size = 7) +

  scale_color_manual(values = drought_colors_short, drop = FALSE) +
  coord_fixed(ratio = 1) +

  # --- REVISED LABELS ---
  labs(
    x = "Water-balance ET (PPT\u2212Q) (mm yr\u207b\u00b9)",
    y = "OpenET (mm yr\u207b\u00b9)"
  ) +
  #ggtitle("a") +
  theme_bw() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.line = element_line(colour = "black"),
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 22, margin = margin(t=10, r=10)),
    #plot.title = element_text(size = 24, face = "bold", hjust = 0.02, vjust = -6),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 20),
    legend.direction = "horizontal",
    legend.box.margin = margin(t = 20, b = 10),
    plot.margin = margin(t=10, r=15, b=10, l=10)
  ) +
  guides(color = guide_legend(nrow = 2, override.aes = list(size = 6, alpha = 1)))

# 5. Save the Figure
ggsave(file.path(fig_dir, "FigS6_OpenET_annual.svg"),
       fig1a, width = 9.5, height = 7.5, device = "svg")
# Vector PDF with embedded fonts for submission (en dashes render correctly)
ggsave(file.path(fig_dir, "FigS6_OpenET_annual.pdf"),
       fig1a, width = 9.5, height = 7.5, device = cairo_pdf)
# n for the Supp. Fig. 10 legend
cat("Supp. Fig. 10: n =", nrow(wb_ann_plot), "catchment-years from",
    dplyr::n_distinct(wb_ann_plot$site_no), "catchments\n")
cat("Supp. Fig. 10 saved\n")

# ═══════════════════════════════════════════════════════════════════════════
# ── Fig. 4 (4-Panel): Q/PPT anomaly vs Thinning & Wildfire by Drought Status
# ═══════════════════════════════════════════════════════════════════════════

# ── 1. Prepare Thinning Data ──
qp_plot <- qp_base %>%
  mutate(
    period = factor(assign_drought(water_year), levels = drought_levels),
    drought_binary = factor(
      case_when(
        period %in% c("Drought 2012\u20132015", "Drought 2020\u20132022") ~ "Drought Years",
        TRUE ~ "Non-Drought Years"
      ),
      levels = c("Non-Drought Years", "Drought Years")
    )
  ) %>%
  filter(
    !is.na(QP_anomaly),
    !is.na(mng_medhigh_10yr_pct),
    mng_medhigh_10yr_pct > 1,
    high_fire_flag == FALSE,
    !is.na(period),
    is.finite(QP_anomaly)
  )

# ── 2. Prepare Wildfire Data ──
qp_fire <- read_csv(file.path(out_dir, "QP_anomaly_wildfire_monthly.csv"),
                    col_types = cols(site_no = col_character(),
                                     yearmonth = col_character()))

qp_fire_plot <- qp_fire %>%
  mutate(
    period = factor(assign_drought(water_year), levels = drought_levels),
    drought_binary = factor(
      case_when(
        period %in% c("Drought 2012\u20132015", "Drought 2020\u20132022") ~ "Drought Years",
        TRUE ~ "Non-Drought Years"
      ),
      levels = c("Non-Drought Years", "Drought Years")
    )
  ) %>%
  filter(
    !is.na(period),
    is.finite(QP_anomaly),
    BurnSev34_10yr_pct > 1
  )

# ── 3. Define Specific Themes for Left and Right columns ──
# Left column keeps Y-axis numbers
theme_left <- theme_bw() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.line = element_line(colour = "black"),
    axis.text = element_text(size = 20),
    axis.title = element_blank(),
    plot.title = element_text(size = 24, face = "bold", hjust = 0.02, vjust = -6),
    legend.position = "none",
    plot.margin = margin(t=10, r=5, b=10, l=5)
  )

# Right column hides Y-axis numbers USING TRANSPARENCY to maintain equal panel widths
theme_right <- theme_bw() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20, color = "transparent"), # Secret to equal widths
    axis.ticks.y = element_line(color = "transparent"),           # Secret to equal widths
    axis.title = element_blank(),
    plot.title = element_text(size = 24, face = "bold", hjust = 0.02, vjust = -6),
    legend.position = "none",
    plot.margin = margin(t=10, r=15, b=10, l=5)
  )

# ── 4. Build the 4 Individual Panels (With Statistics) ──

# Helper function to calculate R2 and p-value for the linear fit
get_stats <- function(df, x_col, y_col) {
  ct <- cor.test(df[[x_col]], df[[y_col]], use = "complete.obs")
  r2 <- ct$estimate^2
  p_val <- ct$p.value
  # italic(R) and italic(p): scalar variables in italics (Nature style)
  p_str <- ifelse(p_val < 0.001, "italic(p) < 0.001",
                  sprintf("italic(p) == '%.3f'", p_val))
  r2_str <- if (r2 < 0.001) "italic(R)^2 < 0.001" else sprintf("italic(R)^2 == '%.3f'", r2)
  return(list(r2_str = r2_str, p_str = p_str))
}

# Plotted ranges of Fig. 4: thinning <= 15% and burn <= 30%.
# scale_x_continuous(limits) drops points beyond these values before
# geom_smooth fits the lines, so the subsets apply the same limits and
# get_stats() uses the same data as the plotted fits.
x_lim_thin <- 15
x_lim_fire <- 30

# Subsets for calculations
df_A <- qp_plot %>%
  filter(drought_binary == "Non-Drought Years", mng_medhigh_10yr_pct <= x_lim_thin)
df_B <- qp_plot %>%
  filter(drought_binary == "Drought Years", mng_medhigh_10yr_pct <= x_lim_thin)
df_C <- qp_fire_plot %>%
  filter(drought_binary == "Non-Drought Years", BurnSev34_10yr_pct <= x_lim_fire)
df_D <- qp_fire_plot %>%
  filter(drought_binary == "Drought Years", BurnSev34_10yr_pct <= x_lim_fire)

# Calculate stats
stats_A <- get_stats(df_A, "mng_medhigh_10yr_pct", "QP_anomaly")
stats_B <- get_stats(df_B, "mng_medhigh_10yr_pct", "QP_anomaly")
stats_C <- get_stats(df_C, "BurnSev34_10yr_pct", "QP_anomaly")
stats_D <- get_stats(df_D, "BurnSev34_10yr_pct", "QP_anomaly")

# Panel A: Thinning, Non-Drought
fig.A <- ggplot(df_A, aes(x = mng_medhigh_10yr_pct, y = QP_anomaly)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40", linewidth = 1) +
  geom_point(aes(color = period), alpha = 0.5, size = 2.5, shape = 16) +
  geom_smooth(method = "lm", color = "black", se = FALSE, linewidth = 1.5, alpha = 0.2, span = 0.8) +
  geom_smooth(method = "loess", color = "red", se = TRUE, linetype = "dashed", linewidth = 1.5, alpha = 0.2, span = 0.8) +
  annotate("text", x = 14.8, y = -0.75, label = stats_A$r2_str, parse = TRUE, hjust = 1, size = 6) +
  annotate("text", x = 14.8, y = -0.95, label = stats_A$p_str, parse = TRUE, hjust = 1, size = 6) +
  scale_color_manual(values = drought_colors) +
  scale_x_continuous(limits = c(0, 15), breaks = c(0, 5, 10, 15)) +
  coord_cartesian(ylim = c(-1, 1)) +
  ggtitle("a") + theme_left

# Panel B: Thinning, Drought
fig.B <- ggplot(df_B, aes(x = mng_medhigh_10yr_pct, y = QP_anomaly)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40", linewidth = 1) +
  geom_point(aes(color = period), alpha = 0.5, size = 2.5, shape = 16) +
  geom_smooth(method = "lm", color = "black", se = FALSE, linewidth = 1.5, alpha = 0.2, span = 0.8) +
  geom_smooth(method = "loess", color = "red", se = TRUE, linetype = "dashed", linewidth = 1.5, alpha = 0.2, span = 0.8) +
  annotate("text", x = 14.8, y = -0.75, label = stats_B$r2_str, parse = TRUE, hjust = 1, size = 6) +
  annotate("text", x = 14.8, y = -0.95, label = stats_B$p_str, parse = TRUE, hjust = 1, size = 6) +
  scale_color_manual(values = drought_colors) +
  scale_x_continuous(limits = c(0, 15), breaks = c(0, 5, 10, 15)) +
  coord_cartesian(ylim = c(-1, 1)) +
  ggtitle("b") + theme_right

# Panel C: Wildfire, Non-Drought
fig.C <- ggplot(df_C, aes(x = BurnSev34_10yr_pct, y = QP_anomaly)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40", linewidth = 1) +
  geom_point(aes(color = period), alpha = 0.5, size = 2.5, shape = 16) +
  geom_smooth(method = "lm", color = "black", se = FALSE, linewidth = 1.5, alpha = 0.2, span = 0.8) +
  geom_smooth(method = "loess", color = "red", se = TRUE, linetype = "dashed", linewidth = 1.5, alpha = 0.2, span = 0.8) +
  annotate("text", x = 29.5, y = -0.75, label = stats_C$r2_str, parse = TRUE, hjust = 1, size = 6) +
  annotate("text", x = 29.5, y = -0.95, label = stats_C$p_str, parse = TRUE, hjust = 1, size = 6) +
  scale_color_manual(values = drought_colors) +
  scale_x_continuous(limits = c(0, 30), breaks = c(0, 10, 20, 30)) +
  coord_cartesian(ylim = c(-1, 1)) +
  ggtitle("c") + theme_left

# Panel D: Wildfire, Drought
fig.D <- ggplot(df_D, aes(x = BurnSev34_10yr_pct, y = QP_anomaly)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40", linewidth = 1) +
  geom_point(aes(color = period), alpha = 0.5, size = 2.5, shape = 16) +
  geom_smooth(method = "lm", color = "black", se = FALSE, linewidth = 1.5, alpha = 0.2, span = 0.8) +
  geom_smooth(method = "loess", color = "red", se = TRUE, linetype = "dashed", linewidth = 1.5, alpha = 0.2, span = 0.8) +
  annotate("text", x = 29.5, y = -0.75, label = stats_D$r2_str, parse = TRUE, hjust = 1, size = 6) +
  annotate("text", x = 29.5, y = -0.95, label = stats_D$p_str, parse = TRUE, hjust = 1, size = 6) +
  scale_color_manual(values = drought_colors) +
  scale_x_continuous(limits = c(0, 30), breaks = c(0, 10, 20, 30)) +
  coord_cartesian(ylim = c(-1, 1)) +
  ggtitle("d") + theme_right


# ── 5. Create a Shared Global Legend ──
# Generate a dummy plot to extract a perfect, title-less legend
dummy_plot <- ggplot(qp_plot, aes(x=mng_medhigh_10yr_pct, y=QP_anomaly)) +
  geom_point(aes(color = period), size = 4) +
  # drop = FALSE forces all 5 drought periods to appear in the legend
  scale_color_manual(values = drought_colors, drop = FALSE) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 20),
    legend.direction = "horizontal",
    legend.box.margin = margin(t = 20, b = 20)
  ) +
  guides(color = guide_legend(nrow = 2, override.aes = list(size = 6, alpha=1)))

# FIX: Explicitly extract the bottom guide box for ggplot2 v3.5.0+ compatibility
shared_legend <- cowplot::get_plot_component(dummy_plot, "guide-box-bottom")

# Fallback just in case your package versions are slightly different
if (inherits(shared_legend, "NULL") || inherits(shared_legend, "zeroGrob")) {
  shared_legend <- cowplot::get_plot_component(dummy_plot, "guide-box")
}

# ── 6. Merge and Save the 4-Panel Figure ──

# A. Build Row 1 (Thinning) & Add its shared X-axis title
row1 <- cowplot::plot_grid(fig.A, fig.B, ncol = 2, align = "h", axis = "tb", rel_widths = c(1, 1))
x_title_thin <- cowplot::ggdraw() + cowplot::draw_label("Thinning area (%)", fontface = 'plain', size = 22)
row1_combined <- cowplot::plot_grid(row1, x_title_thin, ncol = 1, rel_heights = c(1, 0.08))

# B. Build Row 2 (Wildfire) & Add its shared X-axis title
row2 <- cowplot::plot_grid(fig.C, fig.D, ncol = 2, align = "h", axis = "tb", rel_widths = c(1, 1))
x_title_fire <- cowplot::ggdraw() + cowplot::draw_label("Burned area (%)", fontface = 'plain', size = 22)
row2_combined <- cowplot::plot_grid(row2, x_title_fire, ncol = 1, rel_heights = c(1, 0.08))

# C. Stack the two rows together
both_rows <- cowplot::plot_grid(row1_combined, row2_combined, ncol = 1, rel_heights = c(1, 1))

# D. Add the shared Y-axis title on the far left
y_title <- cowplot::ggdraw() + cowplot::draw_label("Q/PPT anomaly", fontface = 'plain', size = 22, angle = 90)
plots_with_y <- cowplot::plot_grid(y_title, both_rows, ncol = 2, rel_widths = c(0.04, 1))

# E. Combine the entire grid with the global legend at the bottom
# Increased relative height from 0.08 to 0.1 to ensure the legend appears
final_fig2 <- cowplot::plot_grid(plots_with_y, shared_legend, ncol = 1, rel_heights = c(1, 0.1))

ggsave(file.path(fig_dir, "Fig4_QP_anomaly_4panel.svg"),
       final_fig2, width = 11, height = 11, device = "svg")
ggsave(file.path(fig_dir, "Fig4_QP_anomaly_4panel.pdf"),
       final_fig2, width = 11, height = 11, device = cairo_pdf)
cat("Fig. 4 saved\n")


# ── 7. Extract LOESS Data from the 4 Panels ──
extract_loess <- function(plot_obj, facet_name, dist_type) {
  # The loess line is the 4th layer in our ggplot objects
  dat <- ggplot_build(plot_obj)$data[[4]]

  dat %>%
    # Select AND rename in one step to avoid masking errors
    dplyr::select(
      cumulative_pct = x,
      loess_mean = y,
      ci_lower = ymin,
      ci_upper = ymax,
      std_error = se
    ) %>%
    dplyr::mutate(
      facet_group = facet_name,
      disturbance_type = dist_type
    )
}

loess_A <- extract_loess(fig.A, "Non-Drought Years", "Thinning")
loess_B <- extract_loess(fig.B, "Drought Years", "Thinning")
loess_C <- extract_loess(fig.C, "Non-Drought Years", "Fire")
loess_D <- extract_loess(fig.D, "Drought Years", "Fire")

combined_loess_table <- bind_rows(loess_A, loess_B, loess_C, loess_D) %>%
  relocate(disturbance_type, facet_group)

write.csv(combined_loess_table,
          file.path(fig_dir, "Fig_R4_combined_loess_summary.csv"),
          row.names = FALSE)

cat("Combined Loess summary table (Thinning & Fire) saved successfully!\n")

# ── 8. Source Data: points plotted in Fig. 4 ──
bind_rows(
  df_A %>% mutate(panel = "a", extent_pct = mng_medhigh_10yr_pct),
  df_B %>% mutate(panel = "b", extent_pct = mng_medhigh_10yr_pct),
  df_C %>% mutate(panel = "c", extent_pct = BurnSev34_10yr_pct),
  df_D %>% mutate(panel = "d", extent_pct = BurnSev34_10yr_pct)
) %>%
  dplyr::select(panel, site_no, water_year, month, period, extent_pct, QP_anomaly) %>%
  write_csv(file.path(fig_dir, "SourceData_Fig4_points.csv"))

cat("Fig. 4 Source Data saved\n")

# ═══════════════════════════════════════════════════════════════════════════
# ── Fig. 5: D/Q ratio by period, station as the unit of analysis ──────────
# ═══════════════════════════════════════════════════════════════════════════
# Each point is one station's mean D/Q in a period. Bars show the mean across
# stations and error bars the t-based 95% CI (n = stations). A station enters
# a panel only if it recorded that diversion type in at least one month of
# WY2011–2024. Period labels, order and colours are those of Fig. 4.

set.seed(160617)  # reproducible jitter

# 1. Station-period means
dq_long <- dq_r4 %>%
  dplyr::mutate(period = factor(assign_drought(water_year), levels = drought_levels)) %>%
  dplyr::filter(!is.na(period), Q_mm > 0) %>%
  tidyr::pivot_longer(cols = c(DQ_consump, DQ_hydro),
                      names_to = "type", values_to = "DQ_ratio") %>%
  dplyr::filter(!is.na(DQ_ratio)) %>%
  dplyr::group_by(type, site_no) %>%
  dplyr::filter(any(DQ_ratio > 0)) %>%
  dplyr::ungroup()

dq_station <- dq_long %>%
  dplyr::group_by(type, period, site_no) %>%
  dplyr::summarise(DQ_ratio = mean(DQ_ratio), n_months = dplyr::n(),
                   .groups = "drop")

# 2. Mean across stations and t-based 95% CI
dq_summary <- dq_station %>%
  dplyr::group_by(type, period) %>%
  dplyr::summarise(
    n        = dplyr::n(),
    mean_DQ  = mean(DQ_ratio),
    se_DQ    = sd(DQ_ratio) / sqrt(n),
    ci_lower = mean_DQ - qt(0.975, n - 1) * se_DQ,
    ci_upper = mean_DQ + qt(0.975, n - 1) * se_DQ,
    .groups  = "drop"
  ) %>%
  # n per bar is given in the legend
  dplyr::mutate(bar_label = sprintf("%.3f", mean_DQ))

print(dq_summary)
cat("Stations per panel:\n")
print(dplyr::count(dplyr::distinct(dq_long, type, site_no), type))

# 3. Bar Themes
theme_left_bar <- theme_bw() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    axis.line    = element_line(colour = "black"),
    axis.text.y  = element_text(size = 20),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 22, margin = margin(t = 10)),
    plot.title   = element_text(size = 24, face = "bold", hjust = 0.02, vjust = -6),
    legend.position = "none",
    plot.margin  = margin(t = 10, r = 5, b = 10, l = 5)
  )

theme_right_bar <- theme_left_bar +
  theme(
    axis.text.y  = element_text(size = 20, color = "transparent"),
    axis.ticks.y = element_line(color = "transparent"),
    plot.margin  = margin(t = 10, r = 15, b = 10, l = 5)
  )

# 4. Build Individual Panels
make_dq_panel <- function(tp, x_lab, tag, theme_bar) {
  ggplot(dplyr::filter(dq_summary, type == tp),
         aes(x = period, y = mean_DQ, fill = period)) +
    geom_col(width = 0.7, color = "black", linewidth = 0.5, alpha = 0.6) +
    geom_jitter(data = dplyr::filter(dq_station, type == tp),
                aes(x = period, y = DQ_ratio), inherit.aes = FALSE,
                width = 0.2, height = 0, size = 1, alpha = 0.4,
                colour = "grey20", shape = 16) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                  width = 0.2, linewidth = 0.8) +
    # Mean above each error bar on a semi-transparent white box so the
    # station points behind it do not obscure the digits
    geom_label(aes(y = ci_upper, label = bar_label), vjust = -0.1, size = 5, lineheight = 0.9,
               fill = scales::alpha("white", 0.75), label.size = 0,
               label.padding = unit(0.1, "lines")) +
    scale_fill_manual(values = drought_colors) +
    # D/Q is capped at 1, so the axis ends at 1 (small top pad keeps
    # points at 1.0 inside the panel border)
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25),
                       expand = expansion(mult = c(0, 0.02))) +
    labs(x = x_lab) +
    ggtitle(tag) +
    theme_bar
}

fig.A <- make_dq_panel("DQ_consump", "Consumptive", "a", theme_left_bar)
fig.B <- make_dq_panel("DQ_hydro",   "Hydropower",  "b", theme_right_bar)

# 5. Create Shared Legend
dummy_bar <- ggplot(dq_summary, aes(x = period, y = mean_DQ, fill = period)) +
  geom_col() +
  scale_fill_manual(values = drought_colors, drop = FALSE) +
  theme_bw() +
  theme(
    legend.position   = "bottom",
    legend.title      = element_blank(),
    legend.text       = element_text(size = 20),
    legend.direction  = "horizontal",
    legend.box.margin = margin(t = 20, b = 20)
  ) +
  guides(fill = guide_legend(nrow = 2))

shared_legend_bar <- cowplot::get_plot_component(dummy_bar, "guide-box-bottom")
if (inherits(shared_legend_bar, "NULL") || inherits(shared_legend_bar, "zeroGrob")) {
  shared_legend_bar <- cowplot::get_plot_component(dummy_bar, "guide-box")
}

# 6. Merge and Save
bars_combined     <- cowplot::plot_grid(fig.A, fig.B, ncol = 2, align = "h",
                                        axis = "tb", rel_widths = c(1, 1))
y_title_bars      <- cowplot::ggdraw() +
  cowplot::draw_label("D/Q ratio", fontface = "plain", size = 22, angle = 90)
plots_with_y_bars <- cowplot::plot_grid(y_title_bars, bars_combined, ncol = 2,
                                        rel_widths = c(0.04, 1))
final_fig5        <- cowplot::plot_grid(plots_with_y_bars, shared_legend_bar,
                                        ncol = 1, rel_heights = c(1, 0.15))

ggsave(file.path(fig_dir, "Fig5_DQ_ratio_by_period.svg"),
       final_fig5, width = 11, height = 6.5, device = "svg")
ggsave(file.path(fig_dir, "Fig5_DQ_ratio_by_period.pdf"),
       final_fig5, width = 11, height = 6.5, device = cairo_pdf)

# 7. Source Data
write_csv(dq_station, file.path(fig_dir, "SourceData_Fig5_station_means.csv"))
write_csv(dplyr::select(dq_summary, -bar_label),
          file.path(fig_dir, "SourceData_Fig5_summary.csv"))
cat("Fig. 5 saved (station means, mean +/- 95% CI, n = stations)\n")

cat("\n=== All figures saved to:", fig_dir, "===\n")
cat("Final Output List:\n")
cat("Supp. Fig. 10: Annual P-Q vs OpenET (FigS6_OpenET_annual.svg/.pdf)\n")
cat("Fig. 4: Q/PPT anomaly 4-panel (Fig4_QP_anomaly_4panel.svg/.pdf)\n")
cat("Fig. 4: LOESS summary (Fig_R4_combined_loess_summary.csv)\n")
cat("Fig. 5: D/Q by period (Fig5_DQ_ratio_by_period.svg/.pdf)\n")
cat("Source Data: SourceData_Fig4_points.csv, SourceData_Fig5_*.csv\n")
