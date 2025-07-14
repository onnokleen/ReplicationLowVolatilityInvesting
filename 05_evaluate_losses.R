# ------------------------------------------------------------------------------
# Script: 05_evaluate_losses.R
# Purpose: 
#   - Evaluate and summarize forecast model performance using loss functions 
#     (Squared Error and QLIKE) for volatility forecasts.
#   - Generate LaTeX tables and figures for inclusion in research outputs.
#   - Compare model-based and combined forecasts across different parameterizations.
#
# Inputs:
#   - Model forecasts and realized volatility data (various .rds files)
#   - Helper functions and package loaders
#
# Outputs:
#   - LaTeX tables summarizing loss ratios and ranks (Tables/loss_tables_*.tex)
#   - Figures showing model ranks over time (Figures/loss_ranks_over_time*.tex)
#
# Author: Onno Kleen
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 1. Load required packages, helper functions, and data
#    - Loads model forecasts, realized volatility, and stock universe definitions.
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 2. Compute loss metrics (SE, QLIKE) for each model and stock
#    - Filters out certain models and time periods.
#    - Calculates per-stock and per-model loss ratios and ranks.
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 3. Generate time-series of model ranks for plotting
#    - Computes rolling QLIKE ranks for each model and stock.
#    - Prepares data for time-series plots of model performance.
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 4. Create and save figures
#    - Plots share of stocks where each model is top-4 or top-1 by QLIKE.
#    - Overlays S&P 500 realized volatility for context.
#    - Saves figures as LaTeX files for later compilation.
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 5. Summarize loss ratios for combined forecasts
#    - Defines helper functions to compute summary statistics for different 
#      forecast combination parameters (eta, delta).
#    - Prepares data for LaTeX tables.
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 6. Generate LaTeX tables summarizing model performance
#    - Writes formatted tables for both QLIKE and SE loss metrics.
#    - Organizes results by model class and combination parameters.
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# End of script
# ------------------------------------------------------------------------------

rm(list = ls())

source("_load_packages.R")
source("functions/helpers.R")

df_rv <-
  readRDS("data/df_har_end_2021_scaled_increasing_window_ss.rds")

df_forecast_raw <- 
  readRDS("portfolio_sorts/df_forecast_raw_rv_scaled_as_dependent_end_2021_increasing_window_scaling_ss_old_filter.rds") |>
  drop_na()

df_losses_raw <-
  df_rv %>%
  filter(is.finite(rv_sq_overnight)) %>%
  group_by(year_month, symbol) %>%
  summarise(rv_sq_overnight = mean(rv_sq_overnight, na.rm = TRUE) * 22) %>%
  right_join(df_forecast_raw) %>%
  filter(model != "oracle_rv_sq_overnight",
         model != "oracle_rv_sq_ret", model != "avg_forecast")  %>%
  filter(year_month >= "2005-01-01") %>%
  mutate(se = (forecast - rv_sq_overnight)^2,
         qlike = rv_sq_overnight / forecast - log(rv_sq_overnight / forecast) - 1) |> #,
  filter(year_month != "2022-01-01") 

# changing stock universe and changing number of forecasts-per stock for models vs. forecast combinations
stocks_to_evaluate <-
  readRDS(paste0("portfolio_sorts/df_portfolios_loss_eta_", 1, "_delta_", 48, "_80_500_old_filter_ss_scaling_increasing_window.rds")) |> 
  select(year_month, symbol) |>
  distinct()

gc()



df_loss_ratios_unrestricted_per_stock <-
  df_losses_raw %>%
  inner_join(stocks_to_evaluate) |>
  group_by(symbol, model) %>%
  filter(n() >= 24) |>
  summarise(se = mean(se),
            qlike = mean(qlike)) %>%
  ungroup()


df_loss_ratios_unrestricted_stock_eval <-
  df_loss_ratios_unrestricted_per_stock %>%
  left_join({df_loss_ratios_unrestricted_per_stock %>%
      filter(model == "last_year_sq_ret") %>%
      rename(se_b = se,
             qlike_b = qlike) %>%
      select(-model)}) %>%
  group_by(symbol) %>%
  mutate(se_rank = rank(se),
         qlike_rank = rank(qlike)) %>%
  group_by(model) %>%
  summarise(mean_se = mean(se /se_b),
            median_se = median(se /se_b),
            ind_se = mean((se / se_b) < 1),
            rank_se_1 = mean(se_rank <= 1),
            rank_se_4 = mean(se_rank <= 4),
            mean_qlike = mean(qlike / qlike_b),
            median_qlike = median(qlike / qlike_b),
            ind_qlike = mean((qlike / qlike_b) < 1),
            rank_qlike_1 = mean(qlike_rank <= 1),
            rank_qlike_4 = mean(qlike_rank <= 4)) %>%
  ungroup() |>
  mutate(model = factor(model, levels = level_models, labels = label_models)) %>%
  arrange(model)



### Ranks over time plots #########

df_avg_rank <-
  df_rv %>%
  filter(is.finite(rv_sq_overnight)) %>%
  group_by(year_month, symbol) %>%
  summarise(rv_sq_overnight = mean(rv_sq_overnight, na.rm = TRUE) * 22) %>%
  right_join(df_forecast_raw) %>%
  filter(model != "oracle_rv_sq_overnight",
         model != "oracle_rv_sq_ret", model != "avg_forecast")  %>%
  filter(year_month >= "2001-01-01") %>%
  mutate(se = (forecast - rv_sq_overnight)^2,
         qlike = rv_sq_overnight / forecast - log(rv_sq_overnight / forecast) - 1) |>
  group_by(symbol, model) |>
  mutate(qlike = rollmean(qlike, k = 48, fill = NA, align = "right")) |>
  group_by(year_month, symbol) |>
  mutate(qlike = rank(qlike)) 


tikz(file = "Figures/loss_ranks_over_time.tex", standAlone = TRUE, width = 8, height = 6)
df_avg_rank |>
  group_by(year_month, model) |>
  inner_join(stocks_to_evaluate) |>
  summarize(qlike_share = mean(qlike <= 4)) |>
  gather(summary, value, contains("qlike")) |>
  filter(model %in% c("last_month_rv", "capmgarch_vix", "har_vix", "panel_har", "panel_har_long_term", "log_har")) |>
  mutate(model = factor(model, level_models, label_models)) |> 
  filter(year_month >= "2005-01-01") |>
  ggplot() +
  geom_line(aes(x = year_month, y = value)) +
    facet_wrap(model ~., dir = "v") +
  ylab("Share QLIKE $Rk^i \\leq 4$") +
  scale_x_date(expand = c(0, 0), breaks = as.Date(c("2005-01-01", "2010-01-01",
                                                         "2015-01-01", "2020-01-01",
                                                         "2022-01-01")), labels = date_format("%Y"), limits = as.Date(c("2005-01-01", "2022-01-01")),
               minor_breaks = date_breaks("1 year")) +
  xlab("") +
  ylab("Share QLIKE $\\overline{Rk^j} \\leq 4$") +
  scale_fill_brewer(palette = "Set1") +
  scale_colour_brewer(palette = "Set1") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,1)) +
  theme_minimal() +
  theme(legend.position = "bottom",
    axis.ticks = element_line(colour = "black"),
        panel.spacing = unit(2, "lines"),
        plot.margin = margin(1,1,1,1, "lines"),
        legend.title = element_blank(),
        text = element_text(size = 15),
        axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()
system("pdflatex -output-directory Figures/ Figures/loss_ranks_over_time.tex")

df_vix_for_plotting <-
  alfred::get_fred_series("VIXCLS", "VIXCLS") |> 
  mutate(year_month = floor_date(date, "months")) |> 
  drop_na() |> group_by(year_month)

df_spx_monthly <- 
  readRDS("data/df_spx_daily_extended.rds") |>
  mutate(year_month = floor_date(date, "months")) |>
  filter(date >= "2005-01-01", date < "2022-01-01") |>
  mutate(spx_rv = spx_rv * mean(spx_ret_close_close^2) / mean(spx_rv)) |>
  group_by(year_month) |>
  summarize(spx_rv = sqrt(mean(spx_rv) * 252))

tikz(file = "Figures/loss_ranks_over_time_top_1.tex", standAlone = TRUE, width = 8, height = 6)
df_avg_rank |>
  group_by(year_month, model) |>
  inner_join(stocks_to_evaluate) |>
  summarize(qlike_share = mean(qlike == 1)) |>
  gather(summary, value, contains("qlike")) |>
  filter(model %in% c("last_year_sq_ret", "last_month_rv", "capmgarch_vix", "har_vix", "panel_har", "panel_har_long_term", "log_har")) |>
  mutate(model = factor(model, level_models, label_models)) |> 
  filter(year_month >= "2005-01-01") |>
  left_join(df_spx_monthly) |>
  ggplot() +
  geom_col(aes(x = year_month, y = value, fill = model, colour = model)) +
  geom_line(aes(x = year_month, y = (spx_rv) / 100 ), linewidth = 2) +
  scale_x_date(expand = c(0, 0), breaks = as.Date(c("2005-01-01", "2010-01-01",
                                                    "2015-01-01", "2020-01-01",
                                                    "2022-01-01")), labels = date_format("%Y"), limits = as.Date(c("2005-01-01", "2022-01-01")),
               minor_breaks = date_breaks("1 year")) +
  xlab("") +
  ylab("Share QLIKE $\\overline{Rk^j} = 1$") +
  scale_fill_brewer(palette = "Set1") +
  scale_colour_brewer(palette = "Set1") +
  scale_y_continuous(expand = c(0, 0),# limits = c(0,0.9),
                     sec.axis = sec_axis(~ (.) * 100, name = "$\\sqrt{\\text{S\\&P 500 RV}}$")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.ticks = element_line(colour = "black"),
        panel.spacing = unit(2, "lines"),
        plot.margin = margin(1,1,1,1, "lines"),
        legend.title = element_blank(),
        text = element_text(size = 15),
        axis.text.x = element_text(angle = 45, hjust = 1))

dev.off()
system("pdflatex -output-directory Figures/ Figures/loss_ranks_over_time_top_1.tex")

make_loss_ratios_combined <- function(x) {
  x %>%
    group_by(model, symbol) %>%
    summarise(se = mean(se),
              qlike = mean(qlike)) %>%
    inner_join({df_loss_ratios_unrestricted_per_stock %>%
        filter(model == "last_year_sq_ret") %>%
        rename(se_b = se,
               qlike_b = qlike) %>%
        select(-model)}) %>%
    group_by(model) %>%
    # drop_na() |>
    summarise(mean_se = mean(se /se_b),
              median_se = median(se /se_b),
              ind_se = mean((se / se_b) < 1),
              mean_qlike = mean(qlike / qlike_b),
              median_qlike = median(qlike / qlike_b),
              ind_qlike = mean((qlike / qlike_b) < 1), .groups = "drop") %>%
    filter(!(model %in% c("el_30", "el_20", "el_80", "el_20_80"))) %>%
    mutate(model = factor(model, levels = level_losses, labels = label_losses)) %>%
    arrange(model)
}


df_losses_base <-
  df_rv %>%
  filter(is.finite(rv_sq_overnight)) %>%
  group_by(year_month, symbol) %>%
  summarise(rv_sq_overnight = mean(rv_sq_overnight, na.rm = TRUE) * 22) %>%
  filter(year_month >= "2005-01-01") %>%
  ungroup()


make_losses <- function(x) {
  df_losses_base %>%
    right_join(x) %>%
    filter(year_month >= "2005-01-01") %>%
    filter(model != "oracle_rv_sq_overnight", 
           model != "oracle_rv_sq_ret", model != "avg_forecast") %>%
    mutate(se = (forecast - rv_sq_overnight)^2,
           qlike = rv_sq_overnight / forecast - log(rv_sq_overnight / forecast) - 1) %>%
    filter(year_month != "2022-01-01")
}

df_losses_raw_eta_0_delta_1 <-
  make_losses(readRDS(paste0("portfolio_sorts/df_portfolios_loss_eta_0_delta_1_80_500_old_filter_ss_scaling_increasing_window.rds")))
df_losses_raw_eta_0.5_delta_12 <-
  make_losses(readRDS(paste0("portfolio_sorts/df_portfolios_loss_eta_", 0.5, "_delta_", 12, "_80_500_old_filter_ss_scaling_increasing_window.rds")))
df_losses_raw_eta_1_delta_12 <-
  make_losses(readRDS(paste0("portfolio_sorts/df_portfolios_loss_eta_", 1, "_delta_", 12, "_80_500_old_filter_ss_scaling_increasing_window.rds")))
df_losses_raw_eta_Inf_delta_12 <-
  make_losses(readRDS(paste0("portfolio_sorts/df_portfolios_loss_eta_", Inf, "_delta_", 12, "_80_500_old_filter_ss_scaling_increasing_window.rds")))
df_losses_raw_eta_0.5_delta_48 <-
  make_losses(readRDS(paste0("portfolio_sorts/df_portfolios_loss_eta_", 0.5, "_delta_", 48, "_80_500_old_filter_ss_scaling_increasing_window.rds")))
df_losses_raw_eta_1_delta_48 <-
  make_losses(readRDS(paste0("portfolio_sorts/df_portfolios_loss_eta_", 1, "_delta_", 48, "_80_500_old_filter_ss_scaling_increasing_window.rds")))
df_losses_raw_eta_Inf_delta_48 <-
  make_losses(readRDS(paste0("portfolio_sorts/df_portfolios_loss_eta_", Inf, "_delta_", 48, "_80_500_old_filter_ss_scaling_increasing_window.rds")))


df_loss_ratios_unrestricted_eta_0_delta_1  <-
  make_loss_ratios_combined(df_losses_raw_eta_0_delta_1)

df_loss_ratios_unrestricted_eta_0.5_delta_12 <-
  make_loss_ratios_combined(df_losses_raw_eta_0.5_delta_12)
df_loss_ratios_unrestricted_eta_1_delta_12 <-
  make_loss_ratios_combined(df_losses_raw_eta_1_delta_12)
df_loss_ratios_unrestricted_eta_Inf_delta_12 <-
  make_loss_ratios_combined(df_losses_raw_eta_Inf_delta_12)

df_loss_ratios_unrestricted_eta_0.5_delta_48 <-
  make_loss_ratios_combined(df_losses_raw_eta_0.5_delta_48)
df_loss_ratios_unrestricted_eta_1_delta_48 <-
  make_loss_ratios_combined(df_losses_raw_eta_1_delta_48)
df_loss_ratios_unrestricted_eta_Inf_delta_48 <-
  make_loss_ratios_combined(df_losses_raw_eta_Inf_delta_48)

### QLIKE table ########

sink(file = paste0("Tables/loss_tables_2005_qlike_2025-02-23.tex"), append = FALSE, type = "output")
cat("\\begin{tabular}{llccccccccccccccccccccccccc} \n")
cat("\\toprule \n")
cat("Model & & $LR^j$ & Mean & Med & $Rk^j = 1$ & $Rk^j \\leq 4$ \\\\")
cat("\\midrule \n")
cat("\\multicolumn{4}{l}{\\textbf{Panel A: Model-based forecasts}} \\\\ ")
cat("\\midrule \n")
# cat("\n & \\\\ \n")
cat("\\multicolumn{2}{l}{")
cat(as.character(df_loss_ratios_unrestricted_stock_eval$model)[1])
cat("} & --- & --- & --- & ")
cat(formatC(unlist(df_loss_ratios_unrestricted_stock_eval[1,10:11]), digits = 2, format = "f"), sep = " & ")
cat("\\\\ \n")
for (ii in 2:nrow(df_loss_ratios_unrestricted_stock_eval)) {
  cat("\\multicolumn{2}{l}{")
  cat(as.character(df_loss_ratios_unrestricted_stock_eval$model)[ii])
  cat("} & ")
  cat(formatC(unlist(df_loss_ratios_unrestricted_stock_eval[ii, c("ind_qlike", "mean_qlike", "median_qlike", "rank_qlike_1", "rank_qlike_4")]), digits = 2, format = "f"), sep = " & ")
  cat("\\\\ \n")
  # cat(" & \\\\ \n")
  if (as.character(df_loss_ratios_unrestricted_stock_eval$model)[ii] %in% c("Log-HAR", "1m-$\\text{RV}^d$", "RM daily, 6 months", "Panel HAR-LR", "Panel MEM")) {
    cat("\\midrule \n")
  }
}
cat("\\midrule \n")
cat("\\multicolumn{7}{l}{\\textbf{Panel B: Combined forecasts---simple average ($\\eta = 0$)}} \\\\ ")
cat("\\midrule \n")
cat(" $\\eta = 0$")
cat(" & ")
cat(" & ")
cat(formatC(unlist(df_loss_ratios_unrestricted_eta_0_delta_1[1,c("ind_qlike", "mean_qlike", "median_qlike")]), digits = 2, format = "f"), sep = " & ")
cat(" & --- & --- ")
cat("\\\\ \n")
cat("\\midrule \n")
cat("\\multicolumn{4}{l}{\\textbf{Panel C: Combined forecasts $\\delta = 12$}} \\\\ ")
cat("\\midrule \n")
cat(" $\\eta = 1/2$ ")
cat(" & & ")
cat(formatC(unlist(df_loss_ratios_unrestricted_eta_0.5_delta_12[2, c("ind_qlike", "mean_qlike", "median_qlike")]), digits = 2, format = "f"), sep = " & ")
cat(" & --- & --- ")
cat("\\\\ \n")
cat(" $\\eta = 1$ ")
cat(" & & ")
cat(formatC(unlist(df_loss_ratios_unrestricted_eta_1_delta_12[2, c("ind_qlike", "mean_qlike", "median_qlike")]), digits = 2, format = "f"), sep = " & ")
cat(" & --- & --- ")
cat("\\\\ \n")
cat(" $\\eta = \\infty$")
cat(" & & ")
cat(formatC(unlist(df_loss_ratios_unrestricted_eta_Inf_delta_12[2, c("ind_qlike", "mean_qlike", "median_qlike")]), digits = 2, format = "f"), sep = " & ")
cat(" & --- & --- ")
cat("\\\\ \n")
cat("\\midrule \n")
cat("\\multicolumn{4}{l}{\\textbf{Panel D: Combined forecasts $\\delta = 48$}} \\\\ ")
cat("\\midrule \n")
cat(" $\\eta = 1/2$ ")
cat(" & & ")
cat(formatC(unlist(df_loss_ratios_unrestricted_eta_0.5_delta_48[2, c("ind_qlike", "mean_qlike", "median_qlike")]), digits = 2, format = "f"), sep = " & ")
cat(" & --- & --- ")
cat("\\\\ \n")
cat(" $\\eta = 1$ ")
cat(" & & ")
cat(formatC(unlist(df_loss_ratios_unrestricted_eta_1_delta_48[2, c("ind_qlike", "mean_qlike", "median_qlike")]), digits = 2, format = "f"), sep = " & ")
cat(" & --- & --- ")
cat("\\\\ \n")
cat(" $\\eta = \\infty$")
cat(" & & ")
cat(formatC(unlist(df_loss_ratios_unrestricted_eta_Inf_delta_48[2, c("ind_qlike", "mean_qlike", "median_qlike")]), digits = 2, format = "f"), sep = " & ")
cat(" & --- & --- ")
cat("\\\\ \n")
cat("\\bottomrule \n")
cat("\\end{tabular}")
sink(file = NULL)



sink(file = paste0("Tables/loss_tables_2005_se_2025-02-25.tex"), append = FALSE, type = "output")
cat("\\begin{tabular}{llccccccccccccccccccccccccc} \n")
cat("\\toprule \n")
cat("Model & & $LR^j$ & Mean & Med & $Rk^j = 1$ & $Rk^j \\leq 4$ \\\\")
cat("\\midrule \n")
cat("\\multicolumn{4}{l}{\\textbf{Panel A: Model-based forecasts}} \\\\ ")
cat("\\midrule \n")
cat("\\multicolumn{2}{l}{")
cat(as.character(df_loss_ratios_unrestricted_stock_eval$model)[1])
cat("} & --- & --- & --- & ")
cat(formatC(unlist(df_loss_ratios_unrestricted_stock_eval[1,10:11]), digits = 2, format = "f"), sep = " & ")
cat("\\\\ \n")
for (ii in 2:nrow(df_loss_ratios_unrestricted_stock_eval)) {
  cat("\\multicolumn{2}{l}{")
  cat(as.character(df_loss_ratios_unrestricted_stock_eval$model)[ii])
  cat("} & ")
  cat(formatC(unlist(df_loss_ratios_unrestricted_stock_eval[ii, c("ind_se", "mean_se", "median_se", "rank_se_1", "rank_se_4")]), digits = 2, format = "f"), sep = " & ")
  cat("\\\\ \n")
  # cat(" & \\\\ \n")
  if (as.character(df_loss_ratios_unrestricted_stock_eval$model)[ii] %in% c("Log-HAR", "1m-$\\text{RV}^d$", "RM daily, 6 months", "Panel HAR-LR", "Panel MEM")) {
    cat("\\midrule \n")
  }
}
cat("\\midrule \n")
cat("\\multicolumn{7}{l}{\\textbf{Panel B: Combined forecasts---simple average ($\\eta = 0$)}} \\\\ ")
cat("\\midrule \n")
cat(" $\\eta = 0$")
cat(" & ")
cat(" & ")
cat(formatC(unlist(df_loss_ratios_unrestricted_eta_0_delta_1[1,c("ind_se", "mean_se", "median_se")]), digits = 2, format = "f"), sep = " & ")
cat(" & --- & --- ")
cat("\\\\ \n")
cat("\\midrule \n")
cat("\\multicolumn{4}{l}{\\textbf{Panel C: Combined forecasts $\\delta = 12$}} \\\\ ")
cat("\\midrule \n")
cat(" $\\eta = 1/2$ ")
cat(" & & ")
cat(formatC(unlist(df_loss_ratios_unrestricted_eta_0.5_delta_12[2, c("ind_se", "mean_se", "median_se")]), digits = 2, format = "f"), sep = " & ")
cat(" & --- & --- ")
cat("\\\\ \n")
cat(" $\\eta = 1$ ")
cat(" & & ")
cat(formatC(unlist(df_loss_ratios_unrestricted_eta_1_delta_12[2, c("ind_se", "mean_se", "median_se")]), digits = 2, format = "f"), sep = " & ")
cat(" & --- & --- ")
cat("\\\\ \n")
cat(" $\\eta = \\infty$")
cat(" & & ")
cat(formatC(unlist(df_loss_ratios_unrestricted_eta_Inf_delta_12[2, c("ind_se", "mean_se", "median_se")]), digits = 2, format = "f"), sep = " & ")
cat(" & --- & --- ")
cat("\\\\ \n")
cat("\\midrule \n")
cat("\\multicolumn{4}{l}{\\textbf{Panel D: Combined forecasts $\\delta = 48$}} \\\\ ")
cat("\\midrule \n")
cat(" $\\eta = 1/2$ ")
cat(" & & ")
cat(formatC(unlist(df_loss_ratios_unrestricted_eta_0.5_delta_48[2, c("ind_se", "mean_se", "median_se")]), digits = 2, format = "f"), sep = " & ")
cat(" & --- & --- ")
cat("\\\\ \n")
cat(" $\\eta = 1$ ")
cat(" & & ")
cat(formatC(unlist(df_loss_ratios_unrestricted_eta_1_delta_48[2, c("ind_se", "mean_se", "median_se")]), digits = 2, format = "f"), sep = " & ")
cat(" & --- & --- ")
cat("\\\\ \n")
cat(" $\\eta = \\infty$")
cat(" & & ")
cat(formatC(unlist(df_loss_ratios_unrestricted_eta_Inf_delta_48[2, c("ind_se", "mean_se", "median_se")]), digits = 2, format = "f"), sep = " & ")
cat(" & --- & --- ")
cat("\\\\ \n")
cat("\\bottomrule \n")
cat("\\end{tabular}")
sink(file = NULL)



