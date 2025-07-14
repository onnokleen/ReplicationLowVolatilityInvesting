
# ------------------------------------------------------------------------------
# Script: 07_generate_returns_and_vol_summaries.R
# Purpose: 
#   - Compute and summarize returns and volatility statistics for benchmark and oracle portfolios.
#   - Evaluate portfolio performance, turnover, transaction costs, and related metrics.
#   - Generate summary statistics for use in tables and figures in research outputs.
#
# Inputs:
#   - Portfolio weights and returns (various .rds files)
#   - Realized volatility and Fama-French factor data
#   - Helper functions and robust variance test code
#
# Outputs:
#   - Summary statistics and performance metrics for portfolios
#   - Data for tables and figures (not directly saved here, but used downstream)
#
# Author: Onno Kleen
# ------------------------------------------------------------------------------

rm(list = ls())

source("functions/helpers.R")

df_fama_monthly <-
  readRDS("data/df_fama_french_monthly.rds") %>%
  select(year_month, mkt_rf, rf) %>%
  filter(year_month >= "1993-01-01" & year_month <= "2021-12-31")

df_rv <- readRDS("data/df_har_end_2021_scaled_increasing_window_ss.rds") %>%
  group_by(year_month, symbol) %>%
  summarise(rv_sq_overnight = mean(rv_sq_overnight, na.rm = TRUE) * 22) %>%
  ungroup()

# code for the robust variance test
# available from Michael Wolf—see ReadMe.txt in the functions folder for the license
load("functions/Var.RData") 

first_month <- "2005-01-01"

df_portfolios_non_smoothed <- readRDS("portfolio_sorts/df_low_vol_portfolios_rv_scaled_as_dependent_end_2021_increasing_window_scaling_ss_old_filter.rds")

df_oo_inv_sqrt_fcst_non_smothed <- 
  df_portfolios_non_smoothed %>% 
  filter(model == "oracle_rv_sq_overnight") %>%
  ungroup() %>%
  select(symbol, year_month, weight_inv_sqrt_fcst_oracle = weight_inv_sqrt_fcst) %>%
  right_join(df_portfolios_non_smoothed) %>%
  group_by(model, year_month)  %>%
  summarise(oracle_overlap = sum(pmin(weight_inv_sqrt_fcst, weight_inv_sqrt_fcst_oracle)),
            oracle_distance = sqrt(mean((weight_inv_sqrt_fcst - weight_inv_sqrt_fcst_oracle)^2)))

df_ret_benchmark_non_smoothed <-
  df_portfolios_non_smoothed  %>%
  filter(model == "last_year_sq_ret") %>%
  left_join(df_rv) %>%
  left_join(df_fama_monthly, by = "year_month") %>%
  group_by(model, year_month) %>%
  mutate(weight = weight_inv_sqrt_fcst) %>%
  summarise(mean_ret_test = sum(ret_crsp * weight),
            n = n(),
            mean_rv_sq_overnight = sum(rv_sq_overnight * weight),
            rf = unique(rf), .groups = "drop") %>%
  filter(year_month >= first_month) %>%
  ungroup() %>%
  left_join(readRDS("portfolio_sorts/turnover_df_low_vol_portfolios_rv_scaled_as_dependent_end_2021_increasing_window_scaling_ss_old_filter.rds")) %>%
  mutate(wealth = cumprod( 1 + mean_ret_test / 100),
         wealth_25 = cumprod(( 1 + mean_ret_test / 100) * (1 - ta_costs / 100))) %>%
  mutate(mean_ret = 100 * (wealth / lag(wealth, order_by = year_month) - 1)) %>%
  mutate(mean_ret_25 = 100 * (wealth_25 / lag(wealth_25, order_by = year_month) - 1)) %>%
  dplyr::mutate(mean_ret_test = if_else(is.na(mean_ret), 100 * (wealth - 1), mean_ret)) %>%
  dplyr::mutate(mean_ret_25_test = if_else(is.na(mean_ret_25), 100 * (wealth_25 - 1), mean_ret_25)) %>%
  dplyr::mutate(mean_ret_test = mean_ret_test - rf,
                mean_ret_25_test = mean_ret_25_test - rf) %>%
  group_by(model) %>%
  dplyr::mutate(mean_rv_sq_overnight_test =  mean_rv_sq_overnight,
         vol_benchmark = sd(mean_ret_test),
         vol_benchmark_25 = sd(mean_ret_25_test)) %>%
  ungroup() %>%
  select(year_month, mean_ret_test, mean_ret_25_test, mean_rv_sq_overnight_test, contains("vol_benchmark"))


df_ret_benchmark_oracle <-
  df_portfolios_non_smoothed %>%
  filter(model == "oracle_rv_sq_overnight") %>%
  left_join(df_rv, by = c("symbol", "year_month")) %>%
  left_join(df_fama_monthly, by = "year_month") %>%
  filter(year_month >= first_month) %>%
  group_by(model, year_month) %>%
  mutate(weight = weight_inv_sqrt_fcst) %>%
  group_by(model, year_month) %>%
  summarise(mean_ret_test_oracle = sum(ret_crsp * weight),
            mean_ret_test_oracle_nonweighted = mean(ret_crsp),
            avg_rv = mean(rv_sq_overnight),
            avg_rv_weighted = sum(rv_sq_overnight * weight),
            n = n(),
            rf = unique(rf), .groups = "drop") %>%
  ungroup() %>%
  left_join(readRDS("portfolio_sorts/turnover_df_low_vol_portfolios_rv_scaled_as_dependent_end_2021_increasing_window_scaling_ss_old_filter.rds")) %>%
  mutate(wealth = cumprod( 1 + mean_ret_test_oracle / 100),
         wealth_25 = cumprod(( 1 + mean_ret_test_oracle / 100) * (1 - ta_costs / 100))) %>%
  mutate(mean_ret = 100 * (wealth / lag(wealth, order_by = year_month) - 1)) %>%
  mutate(mean_ret_25 = 100 * (wealth_25 / lag(wealth_25, order_by = year_month) - 1)) %>%
  dplyr::mutate(mean_ret_test_oracle  = if_else(is.na(mean_ret), 100 * (wealth - 1), mean_ret)) %>%
  dplyr::mutate(mean_ret_25_test_oracle = if_else(is.na(mean_ret_25), 100 * (wealth_25 - 1), mean_ret_25)) %>%
  dplyr::mutate(mean_ret_test_oracle = mean_ret_test_oracle - rf,
         mean_ret_25_test_oracle = mean_ret_25_test_oracle - rf,
         turnover_oracle = turnover,
         ta_costs_oracle = ta_costs) %>%
  select(year_month, mean_ret_test_oracle, mean_ret_25_test_oracle, turnover_oracle, ta_costs_oracle)


make_summary <- function(df, path_transaction_costs) {
  
  df_oo <- 
    df_portfolios_non_smoothed %>% 
    filter(model == "oracle_rv_sq_overnight") %>%
    ungroup() %>%
    select(symbol, year_month, weight_inv_sqrt_fcst_oracle = weight_inv_sqrt_fcst) %>%
    right_join(df, by = c("symbol", "year_month"), multiple = "all") %>%
    filter(year_month >= first_month) %>%
    mutate(wad = 100 * abs(weight_inv_sqrt_fcst - weight_inv_sqrt_fcst_oracle)) %>%
    group_by(model, year_month)  %>%
    summarise(oracle_overlap = sum(pmin(weight_inv_sqrt_fcst, weight_inv_sqrt_fcst_oracle)),
              wad_min = min(wad),
              wad_20_percent = quantile(wad, 0.20),
              wad_mean = mean(wad),
              wad_sum = sum(wad),
              wad_median = median(wad),
              wad_80_percent = quantile(wad, 0.80),
              wad_max = max(wad),
              wad_sum_with_oracle = sum(wad * (weight_inv_sqrt_fcst_oracle != 0)),
              wad_sum_not_oracle = sum(wad * (weight_inv_sqrt_fcst_oracle == 0)),
              .groups = "drop")
  
  # browser()
  
  df %>%
    filter(model != "oracle_rv_sq_ret") %>%
    left_join(df_rv, by = c("year_month", "symbol")) %>%
    left_join(df_fama_monthly, by = "year_month") %>%
    filter(year_month >= first_month) %>%
    mutate(weight = weight_inv_sqrt_fcst) %>%
    group_by(model, year_month) %>%
    summarise(mean_ret = sum(ret_crsp * weight),
              max_weight = max(weight),
              mean_ret_mkt = unique(mkt_rf),
              mean_rv_sq_overnight = sum(rv_sq_overnight * weight),
              rf = unique(rf),
              n_stocks = n(), 
              positive_weights = mean(weight > 0),
              max_weight = max(weight),
              .groups = "drop") %>%
    ungroup() %>%
    mutate(mean_ret_backup = mean_ret) |>
    left_join(readRDS(path_transaction_costs), by = c("model", "year_month")) %>%
    left_join(df_oo, by = c("model", "year_month")) %>%
    mutate(turnover = if_else(is.na(turnover), 0, turnover)) %>%
    group_by(model) %>%
    mutate(wealth = cumprod( 1 + mean_ret / 100),
           wealth_25 = cumprod(( 1 + mean_ret / 100) * (1 - ta_costs / 100))) %>%
    mutate(mean_ret = 100 * (wealth / lag(wealth, order_by = year_month) - 1)) %>%
    mutate(mean_ret_25 = 100 * (wealth_25 / lag(wealth_25, order_by = year_month) - 1)) %>%
    mutate(mean_ret = if_else(is.na(mean_ret), 100 * (wealth - 1), mean_ret)) %>%
    mutate(mean_ret_25 = if_else(is.na(mean_ret_25), 100 * (wealth_25 - 1), mean_ret_25)) %>%
    mutate(mean_ret_25 = mean_ret_25 - rf) %>%
    ungroup() %>%
    # mutate(mean_ret_25 = mean_ret * (1 - ta_costs / 100) - rf) |>
    mutate(mean_ret = mean_ret - rf) %>%
    left_join(df_ret_benchmark_non_smoothed, by = "year_month") %>%
    left_join(df_ret_benchmark_oracle, by = "year_month") %>%
    group_by(model) |>
    mutate(return_win = 12 * (mean_ret / sd(mean_ret) * sd(mean_ret_test) - mean_ret_test),
           return_win_25 = 12 * (mean_ret_25 / sd(mean_ret_25) * sd(mean_ret_25_test) - mean_ret_25_test)) |>
    group_by(model) %>%
    summarise(arv = sqrt(mean(mean_rv_sq_overnight)) * sqrt(12),
              pvalue_arv = p_value_t_test(sqrt(mean_rv_sq_overnight) - sqrt(mean_rv_sq_overnight_test)),
              oo = mean(oracle_overlap) * 100,
              od = NA,
              to = mean(turnover, na.rm = TRUE),
              tracking_error = sd(mean_ret - mean_ret_test_oracle),
              tracking_error_25 = sd(mean_ret_25 - mean_ret_25_test_oracle),
              ret = mean(mean_ret) * 12,
              max_ret = max(mean_ret),
              pvalue_0 = p_value_t_test(mean_ret - mean_ret_test),
              sd = sd(mean_ret) * sqrt(12),
              pvalue_sd = ifelse(sum(mean_ret != mean_ret_test) == 0, NA, hac.inference.log.var(cbind(mean_ret, mean_ret_test))$p.Values),
              skew = moments::skewness(mean_ret), # note: some measures didn't make it in the final paper but we still include them for completeness
              sr = mean(mean_ret) / sd(mean_ret) * sqrt(12),
              pvalue_0_lw =  PeerPerformance::sharpeTesting(mean_ret, mean_ret_test, control = list(type = 1, hac = TRUE))$pval,
              fee_1 = utility_fee(mean_ret_test_oracle, mean_ret, 1) * 100 * 12,
              fee_5 = utility_fee(mean_ret_test_oracle, mean_ret, 5) * 100 * 12,
              fee_10 = utility_fee(mean_ret_test_oracle, mean_ret, 10) * 100 * 12,
              ret_25 = mean(mean_ret_25) * 12,
              pvalue_25 = p_value_t_test(mean_ret_25 - mean_ret_25_test),
              sd_25 = sd(mean_ret_25) * sqrt(12),
              pvalue_sd_25 = ifelse(sum(mean_ret_25 != mean_ret_25_test) == 0, NA, hac.inference.log.var(cbind(mean_ret_25, mean_ret_25_test))$p.Values),
              skew_25 = moments::skewness(mean_ret_25),
              sr_25 = mean(mean_ret_25) / sd(mean_ret_25) * sqrt(12),
              pvalue_0_lw_25 =  PeerPerformance::sharpeTesting(mean_ret_25, mean_ret_25_test, control = list(type = 1, hac = TRUE))$pval,
              fee_1_25 = utility_fee(mean_ret_25_test_oracle, mean_ret_25, 1) * 100 * 12,
              fee_5_25 = utility_fee(mean_ret_25_test_oracle, mean_ret_25, 5) * 100 * 12,
              fee_10_25 = utility_fee(mean_ret_25_test_oracle, mean_ret_25, 10) * 100 * 12,
              max_dd = PerformanceAnalytics::maxDrawdown((mean_ret) / 100),
              max_dd_25 = PerformanceAnalytics::maxDrawdown((mean_ret_25) / 100),
              positive_weight = mean(positive_weights),
              mean_max_weight = mean(max_weight),
              max_max_weight = max(max_weight),
              ta_costs = mean(ta_costs) * 100,
              wad_mean = mean(wad_sum),
              wad_max = max(wad_sum),
              wad_mean_with_oracle = mean(wad_sum_with_oracle),
              wad_mean_not_oracle = mean(wad_sum_not_oracle),
              min(n_stocks),
              p_value_return_win = p_value_t_test(return_win),
              return_win = mean(return_win),
              p_value_return_win_25 = p_value_t_test(return_win_25),
              return_win_25 = mean(return_win_25)
              ) %>% #,
    filter(!model %in% c("oracle_rv_sq_ret","oracle_rv_sq_overnight")) %>%
    mutate(model = factor(model, levels = c(level_models, level_losses), labels = c(label_models, label_losses))) %>%
    arrange(model)
}




df_summaries_unrestricted_non_smoothed <-
  make_summary(df_portfolios_non_smoothed, 
               "portfolio_sorts/turnover_df_low_vol_portfolios_rv_scaled_as_dependent_end_2021_increasing_window_scaling_ss_old_filter.rds") 

df_portfolio_summary_eta_0_delta_1  <-
  make_summary(readRDS(paste0("portfolio_sorts/df_portfolios_loss_eta_", 0, "_delta_", 1, "_80_500_old_filter_ss_scaling_increasing_window.rds")), 
               "portfolio_sorts/transaction_costs_loss_eta_0_delta_1_80_500_old_filter_ss_scaling_increasing_window.rds")

df_portfolio_summary_eta_0.5_delta_12  <-
  make_summary(readRDS(paste0("portfolio_sorts/df_portfolios_loss_eta_", 0.5, "_delta_", 12, "_80_500_old_filter_ss_scaling_increasing_window.rds")), 
               "portfolio_sorts/transaction_costs_loss_eta_0.5_delta_12_80_500_old_filter_ss_scaling_increasing_window.rds")
df_portfolio_summary_eta_1_delta_12  <-
  make_summary(readRDS(paste0("portfolio_sorts/df_portfolios_loss_eta_", 1, "_delta_", 12, "_80_500_old_filter_ss_scaling_increasing_window.rds")), 
               "portfolio_sorts/transaction_costs_loss_eta_1_delta_12_80_500_old_filter_ss_scaling_increasing_window.rds")
df_portfolio_summary_eta_Inf_delta_12  <-
  make_summary(readRDS(paste0("portfolio_sorts/df_portfolios_loss_eta_", Inf, "_delta_", 12, "_80_500_old_filter_ss_scaling_increasing_window.rds")), 
               "portfolio_sorts/transaction_costs_loss_eta_Inf_delta_48_80_500_old_filter_ss_scaling_increasing_window.rds")

df_portfolio_summary_eta_0.5_delta_48  <-
  make_summary(readRDS(paste0("portfolio_sorts/df_portfolios_loss_eta_", 0.5, "_delta_", 48, "_80_500_old_filter_ss_scaling_increasing_window.rds")), 
               "portfolio_sorts/transaction_costs_loss_eta_0.5_delta_48_80_500_old_filter_ss_scaling_increasing_window.rds")
df_portfolio_summary_eta_1_delta_48  <-
  make_summary(readRDS(paste0("portfolio_sorts/df_portfolios_loss_eta_", 1, "_delta_", 48, "_80_500_old_filter_ss_scaling_increasing_window.rds")), 
               "portfolio_sorts/transaction_costs_loss_eta_1_delta_48_80_500_old_filter_ss_scaling_increasing_window.rds")
df_portfolio_summary_eta_Inf_delta_48  <-
  make_summary(readRDS(paste0("portfolio_sorts/df_portfolios_loss_eta_", Inf, "_delta_", 48, "_80_500_old_filter_ss_scaling_increasing_window.rds")), 
               "portfolio_sorts/transaction_costs_loss_eta_Inf_delta_48_80_500_old_filter_ss_scaling_increasing_window.rds")


df_summary_oracle <-
  df_ret_benchmark_oracle %>%
  summarise(ret = mean(mean_ret_test_oracle) * 12,
            sd = sd(mean_ret_test_oracle) * sqrt(12),
            skew = moments::skewness(mean_ret_test_oracle),
            sr = ret / sd(mean_ret_test_oracle) / sqrt(12),
            ret_25 = mean(mean_ret_25_test_oracle) * 12,
            sd_25 = sd(mean_ret_25_test_oracle) * sqrt(12),
            skew_25 = moments::skewness(mean_ret_25_test_oracle),
            sr_25 = ret_25 / sd(mean_ret_25_test_oracle) / sqrt(12),
            turnover = mean(turnover_oracle),
            ta_costs = mean(ta_costs_oracle) * 100,
            max_dd = PerformanceAnalytics::maxDrawdown((mean_ret_test_oracle) / 100),
            max_dd_25 = PerformanceAnalytics::maxDrawdown((mean_ret_25_test_oracle) / 100))

paste_wad <- function(df) {
  cat(" & ")
  cat(formatC(df[1, c("wad_mean", "wad_mean_with_oracle", "wad_mean_not_oracle", "wad_max")] %>% unlist(),
              digits = 2, format = "f"), sep = " & ")
  cat(" & & ")
  cat(formatC(df[1,c("to", "ta_costs")] %>% unlist(), digits = 2, format = "f"), sep = " & ")
  cat("\\\\ \n")
  
}

sink(file = paste0("Tables/summary_table_portfolios_inv_sqrt_fcst_0_95_80_arvol_oo_to_revision_02_03_2025.tex"), append = FALSE, type = "output")
cat("\\begin{tabular}{lllccclccccccccccccc} \n")
cat("\\toprule \n")
cat(" & & \\multicolumn{4}{c}{WAD} & & \\multicolumn{1}{c}{TO} & \\multicolumn{1}{c}{TC}\\\\")
cat("\\cmidrule{3-6} \\cmidrule{8-9} \n")
cat(" & & Mean & In-Post-Hoc & Not-Post-hoc & Max & & --- & --- \\\\")
cat("\\midrule \n")
cat("\\multicolumn{2}{l}{Post-hoc} & --- & --- & --- & --- & & ")
cat(formatC(df_summary_oracle[1, c("turnover", "ta_costs")] %>% unlist(), digits = 2, format = "f"), sep = " & ")
cat(" \\\\ ")
cat("\\midrule")
cat("\\multicolumn{5}{l}{\\textbf{Panel A: Model-based portfolios}}\\\\ ")
cat("\\midrule \n")
for (ii in 1:nrow(df_summaries_unrestricted_non_smoothed)) {
  cat("\\multicolumn{2}{l}{")
  cat(paste(unlist(df_summaries_unrestricted_non_smoothed[ii,1])))
  cat("}")
  paste_wad(df_summaries_unrestricted_non_smoothed[ii,])
  if (as.character(df_summaries_unrestricted_non_smoothed$model)[ii] %in% c("1m-$\\text{RV}^d$", "Panel HAR-LR", "Panel MEM", "RM daily, 6 months", "Log-HAR")) {
    cat("\\midrule \n")
  }
}
cat("\\midrule \n")
cat("\\multicolumn{5}{l}{\\textbf{Panel B: Average forecast}} \\\\")
cat("\\midrule")
cat("$\\eta = 0$ & ")
paste_wad(df_portfolio_summary_eta_0_delta_1)
cat("\\midrule \n")
cat("\\multicolumn{5}{l}{\\textbf{Panel C: Loss-based portfolios $\\delta = 12$}} \\\\")
cat("\\midrule")
cat("$\\eta = 1/2$ & ")
paste_wad(df_portfolio_summary_eta_0.5_delta_12 |> filter(model == "QLIKE"))
cat("$\\eta = 1$ & ")
paste_wad(df_portfolio_summary_eta_1_delta_12 |> filter(model == "QLIKE"))
cat("$\\eta = \\infty$ & ")
paste_wad(df_portfolio_summary_eta_Inf_delta_12 |> filter(model == "QLIKE"))
cat("\\midrule")
cat("\\multicolumn{5}{l}{\\textbf{Panel D: Loss-based portfolios $\\delta = 48$}} \\\\")
cat("\\midrule")
cat("$\\eta = 1/2$ & ")
paste_wad(df_portfolio_summary_eta_0.5_delta_48 |> filter(model == "QLIKE"))
cat("$\\eta = 1$ & ")
paste_wad(df_portfolio_summary_eta_1_delta_48 |> filter(model == "QLIKE"))
cat("$\\eta = \\infty$ & ")
paste_wad(df_portfolio_summary_eta_Inf_delta_48 |> filter(model == "QLIKE"))
cat("\\bottomrule \n")
cat("\\end{tabular}")
sink(file = NULL)
sink()

paste_row <- function(df, loss = NULL, model = NULL, print.model = TRUE) {
  
  ii <- 1
  
  if (!is.null(loss)) {
    if (loss == "qlike") {
      ii <- 2
    } else {
      ii <- 1
    }
  }
  
  if (!is.null(model)) {
    df$model <- model
  }
  
  if (print.model) {
    # cat(" & ")
    cat(paste(unlist(df[ii,"model"])))
  }
  cat(" & ")
  cat(formatC(df[ii,c("ret")] %>% unlist(), digits = 2, format = "f"))
  cat(paste_stars(df[ii,c("pvalue_0")] %>% unlist()))
  cat(" & ")
  cat(formatC(df[ii,c("sd")] %>% unlist(), digits = 2, format = "f"))
  cat(paste_stars(df[ii,c("pvalue_sd")] %>% unlist()))
  cat(" & ")
  cat(formatC(df[ii,c("sr")] %>% unlist(), digits = 2, format = "f"), sep = " & ")
  cat(paste_stars(df[ii,c("pvalue_0_lw")] %>% unlist()))
  cat(" & ")
  cat(formatC(df[ii,c("return_win")] %>% unlist(), digits = 2, format = "f"))
  cat(paste_stars(df[ii,c("p_value_return_win")] %>% unlist()))
  cat(" & & ")
  cat(formatC(df[ii,"ret_25"] %>% unlist(), digits = 2, format = "f"))
  cat(paste_stars(df[ii, "pvalue_25"] %>% unlist()))
  cat(" & ")
  cat(formatC(df[ii,"sd_25"] %>% unlist(), digits = 2, format = "f"))
  cat(paste_stars(df[ii, "pvalue_sd_25"] %>% unlist()))
  cat(" & ")
  cat(formatC(df[ii,c("sr_25")] %>% unlist(), digits = 2, format = "f"), sep = " & ")
  cat(paste_stars(df[ii,"pvalue_0_lw_25"] %>% unlist()))
  cat(" & ")
  cat(formatC(df[ii,c("return_win_25")] %>% unlist(), digits = 2, format = "f"))
  cat(paste_stars(df[ii,c("p_value_return_win_25")] %>% unlist()))
  cat("\\\\ \n")
}

sink(file = paste0("Tables/summary_table_portfolios_inv_sqrt_fcst_0_80_returns_revision_2025-03-06.tex"), append = FALSE, type = "output")
cat("\\begin{tabular}{llllllllllllllllllllllllllllllllllllll} \n")
cat("\\toprule \n")
cat(" & \\multicolumn{4}{c}{Without TC} & & \\multicolumn{4}{c}{With TC} \\\\ \n")
cat(" \\cmidrule{2-5} \\cmidrule{7-10} \n")
cat(" & \\multicolumn{1}{c}{Ret} & Std & \\multicolumn{1}{c}{SR} & Ret-Win & &  \\multicolumn{1}{c}{Ret} & Std &  \\multicolumn{1}{c}{SR} & Ret-Win \\\\")
cat("\\midrule \n")
cat("\\multicolumn{1}{l}{Post-hoc} & ")
cat(formatC(df_summary_oracle[1,c(1:2, 4)] %>% unlist(), digits = 2, format = "f"), sep = " & ")
cat(" & --- & & ")
cat(formatC(df_summary_oracle[1,c(5:6, 8)] %>% unlist(), digits = 2, format = "f"), sep = " & ")
cat(" & --- ")
cat(" \\\\ ")
cat("\\midrule")
cat("\\multicolumn{8}{l}{\\textbf{Panel A: Portfolios based on individual forecasts}}\\\\ ")
cat("\\midrule \n")
for (ii in 1:dim(df_summaries_unrestricted_non_smoothed)[1]) {
  paste_row(df_summaries_unrestricted_non_smoothed[ii,], model = paste(unlist(df_summaries_unrestricted_non_smoothed[ii,"model"])))
  if (as.character(df_summaries_unrestricted_non_smoothed$model)[ii] %in% c("1m-$\\text{RV}^d$", "Log-HAR", "Panel HAR-LR", "Panel MEM", "RM daily, 6 months")) {
    cat("\\midrule \n")
  }
}
cat("\\midrule \n")
cat("\\multicolumn{8}{l}{\\textbf{Panel B: Portfolio based on averaged forecasts}} \\\\")
cat("\\midrule")
cat("$\\eta = 0$ ")
paste_row(df_portfolio_summary_eta_0_delta_1, print.model = FALSE)
cat("\\midrule \n")
cat("\\multicolumn{8}{l}{\\textbf{Panel C: Portfolios based on combined forecasts $\\delta = 12$}} \\\\")
cat("\\midrule")
paste_row(df_portfolio_summary_eta_0.5_delta_12, loss = "qlike", model = "$\\eta = 1/2$")
paste_row(df_portfolio_summary_eta_1_delta_12, loss = "qlike", model = "$\\eta = 1$")
paste_row(df_portfolio_summary_eta_Inf_delta_12, loss = "qlike", model = "$\\eta = \\infty$")
cat("\\midrule \n")
cat("\\multicolumn{8}{l}{\\textbf{Panel D: Portfolios based on combined forecasts $\\delta = 48$}} \\\\")
cat("\\midrule")
paste_row(df_portfolio_summary_eta_0.5_delta_48, loss = "qlike", model = "$\\eta = 1/2$")
paste_row(df_portfolio_summary_eta_1_delta_48, loss = "qlike", model = "$\\eta = 1$")
paste_row(df_portfolio_summary_eta_Inf_delta_48, loss = "qlike", model = "$\\eta = \\infty$")
cat("\\bottomrule \n")
cat("\\end{tabular}")
sink(file = NULL)

paste_row_fee <- function(df, loss = NULL, model = NULL, print.model = TRUE) {
  
  ii <- 1
  
  if (!is.null(loss)) {
    if (loss == "qlike") {
      ii <- 2
    } else {
      ii <- 1
    }
  }
  
  if (!is.null(model)) {
    df$model <- model
  }
  
  if (print.model) {
    # cat(" & ")
    cat(paste(unlist(df[ii,"model"])))
  }
  cat(" & ")
  cat(formatC(df[ii,c("tracking_error")] %>% unlist(), digits = 2, format = "f"), sep = " & ")
  cat(" & ")
  cat(formatC(df[ii,c("fee_1")] %>% unlist(), digits = 2, format = "f"), sep = " & ")
  cat(" & ")
  cat(formatC(df[ii,c("fee_10")] %>% unlist(), digits = 2, format = "f"), sep = " & ")
  cat(" & & ")
  cat(formatC(df[ii,c("tracking_error_25")] %>% unlist(), digits = 2, format = "f"), sep = " & ")
  cat(" & ")
  cat(formatC(df[ii,"fee_1_25"] %>% unlist(), digits = 2, format = "f"), sep = " & ")
  cat(" & ")
  cat(formatC(df[ii,"fee_10_25"] %>% unlist(), digits = 2, format = "f"), sep = " & ")
  cat("\\\\ \n")
}

sink(file = paste0("Tables/summary_table_portfolios_inv_sqrt_fcst_0_80_fees_2025-03-06.tex"), append = FALSE, type = "output")
cat("\\begin{tabular}{llllllllllllllllllllllllllllllllllllll} \n")
cat("\\toprule \n")
cat(" & \\multicolumn{3}{c}{Without TC} & & \\multicolumn{3}{c}{With TC} \\\\ \n")
cat(" \\cmidrule{2-4} \\cmidrule{6-8} \n")
cat(" & TE & $\\Delta_1$ & $\\Delta_{10}$ &  & TE & $\\Delta_1$ & $\\Delta_{10}$\\\\")
cat("\\midrule \n")
cat("\\multicolumn{7}{l}{\\textbf{Panel A: Portfolios based on individual forecasts}}\\\\ ")
cat("\\midrule \n")
for (ii in 1:dim(df_summaries_unrestricted_non_smoothed)[1]) {
  # cat("\\multicolumn{2}{l}{")
  # cat(paste(unlist(df_summaries_unrestricted_non_smoothed[ii,"model"])))
  # cat("}")
  paste_row_fee(df_summaries_unrestricted_non_smoothed[ii,], model = paste(unlist(df_summaries_unrestricted_non_smoothed[ii,"model"])))
  # cat("\\\\ \n")
  if (as.character(df_summaries_unrestricted_non_smoothed$model)[ii] %in% c("1m-$\\text{RV}^d$", "Log-HAR", "Panel HAR-LR", "Panel MEM", "RM daily, 6 months")) {
    cat("\\midrule \n")
  }
}
cat("\\midrule \n")
cat("\\multicolumn{7}{l}{\\textbf{Panel B: Portfolio based on averaged forecasts}} \\\\")
cat("\\midrule")
cat("$\\eta = 0$ ")
paste_row_fee(df_portfolio_summary_eta_0_delta_1, print.model = FALSE)
cat("\\midrule \n")
cat("\\multicolumn{7}{l}{\\textbf{Panel C: Portfolios based on combined forecasts $\\delta = 12$}} \\\\")
cat("\\midrule")
paste_row_fee(df_portfolio_summary_eta_0.5_delta_12, loss = "qlike", model = "$\\eta = 1/2$")
paste_row_fee(df_portfolio_summary_eta_1_delta_12, loss = "qlike", model = "$\\eta = 1$")
paste_row_fee(df_portfolio_summary_eta_Inf_delta_12, loss = "qlike", model = "$\\eta = \\infty$")
cat("\\midrule \n")
cat("\\multicolumn{7}{l}{\\textbf{Panel D: Portfolios based on combined forecasts $\\delta = 48$}} \\\\")
cat("\\midrule")
paste_row_fee(df_portfolio_summary_eta_0.5_delta_48, loss = "qlike", model = "$\\eta = 1/2$")
paste_row_fee(df_portfolio_summary_eta_1_delta_48, loss = "qlike", model = "$\\eta = 1$")
paste_row_fee(df_portfolio_summary_eta_Inf_delta_48, loss = "qlike", model = "$\\eta = \\infty$")
cat("\\bottomrule \n")
cat("\\end{tabular}")
sink(file = NULL)






