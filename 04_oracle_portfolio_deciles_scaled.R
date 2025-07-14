
# ------------------------------------------------------------------------------
# Script: 04_oracle_portfolio_deciles_scaled.R
# Purpose: 
#   - Generate and evaluate "oracle" and benchmark portfolios.
#   - Produce summary statistics and figures for portfolio performance.
#
# Inputs:
#   - Realized volatility, Fama-French factors, and benchmark index data
#   - Helper functions
#
# Outputs:
#   - Figures and LaTeX tables summarizing benchmark portfolios
#
# Author: Onno Kleen
# ------------------------------------------------------------------------------

# this file generates Figure 1, 

rm(list = ls())

source("functions/helpers.R")

df_fama_monthly <-
  readRDS("data/df_fama_french_monthly.rds") %>%
  select(year_month, mkt_rf, rf) %>%
  filter(year_month >= "1997-01-01" & year_month <= "2021-12-31")

df_har <- 
  readRDS("data/df_har_end_2021_scaled_increasing_window_ss.rds") %>%
  group_by(year_month, symbol) %>%
  summarise(rv_sq_overnight = mean(rv_sq_overnight, na.rm = TRUE) * 22) %>%
  ungroup()

df_spx_lv <-
  readxl::read_excel("Data/Low_vola_indices.xlsx") %>%
  mutate(date = as_date(Dates),
         year_month = floor_date(date, "months")) %>%
  group_by(year_month) %>%
  filter(date == max(date)) %>%
  ungroup() %>%
  rename(SPMVTR = `SP5LVTUT Index`) %>%
  select(year_month, SPMVTR) %>%
  mutate(SPMVTR = 100 * (SPMVTR - lag(SPMVTR)) / lag(SPMVTR))

df_market <-
  tidyquant::tq_get("^SP500TR", from = "2004-12-01") |>
  mutate(year_month = floor_date(as_date(date), "months")) |>
  group_by(year_month) |>
  filter(date == max(date)) |>
  ungroup() |>
  transmute(year_month,
            model = "S\\&P 500 TR",
            mean_ret = 100 * (adjusted - lag(adjusted)) / lag(adjusted))


sum_spx_low_vol <-
  df_spx_lv %>%
  filter(year_month >= "2005-01-01", year_month <= "2021-12-31") %>%
  left_join(df_fama_monthly) |>
  mutate(mean_ret = SPMVTR - rf) |>
  summarise(ret = mean(mean_ret) * 12,
            sd_ret = sd(mean_ret) * sqrt(12),
            sr = mean(mean_ret) / sd(mean_ret) * sqrt(12))

sum_market <-
  df_market %>%
  filter(year_month >= "2005-01-01", year_month <= "2021-12-31") %>%
  left_join(df_fama_monthly) |>
  mutate(mean_ret = mean_ret - rf) |>
  summarise(ret = mean(mean_ret) * 12,
            sd_ret = sd(mean_ret) * sqrt(12),
            sr = mean(mean_ret) / sd(mean_ret) * sqrt(12))

tikz(file = "Figures/cumprod_returns_low_vola_portfolios_rv_scaled_increasing.tex", standAlone = TRUE, width = 6.4, height = 2.8)
readRDS("portfolio_sorts/df_low_vol_portfolios_rv_scaled_as_dependent_end_2021_increasing_window_scaling_ss_old_filter.rds") %>% 
  filter(model %in% c("oracle_rv_sq_overnight", "last_year_sq_ret")) %>%
  filter(year_month >= "2005-01-01") %>%
  group_by(model, year_month) %>%
  mutate(weight = if_else(decile_id != "quintile_08_10", 1 / sqrt(forecast), 0)) %>%
  mutate(weight = regularize_weights(weight, 0.05)) %>%
  summarise(mean_ret = sum(weight * ret_crsp)) %>%
  ungroup() %>%
  bind_rows(
    df_market %>% 
      filter(year_month >= "2005-01-01")) %>%
  bind_rows(df_spx_lv %>% select(year_month, SPMVTR) %>% rename(mean_ret = SPMVTR) %>% mutate(model = "SPMVTR") %>% filter(year_month >= "2005-01-01")) %>%
  group_by(model) %>%
  arrange(year_month) %>%
  mutate(cumsum_ret = (cumprod(1 + mean_ret / 100) - 1) * 100) %>%
  ungroup() %>%
  mutate(model = factor(model, levels = c("S\\&P 500 TR", "oracle_rv_sq_overnight", "SPMVTR", "last_year_sq_ret"),
                        labels = c("S\\&P 500 TR", "Post-hoc", "SP5LVTUT", "12m-$\\text{RV}^d$"))) %>%
  ggplot() +
  geom_line(aes(x = year_month, y = cumsum_ret, colour = model, linetype = model), linewidth = 1.1) +
  scale_x_date(expand = c(0, 0), date_breaks = "1 years", labels = date_format("%Y"), limits = as.Date(c("2005-01-01", "2022-01-01"))) +
  xlab("") +
  ylab("") +
  scale_colour_brewer(palette = "Set1") +
  geom_hline(aes(yintercept = 0), colour = "gray") +
  scale_y_continuous(expand = c(0, 0), breaks = c(-100, 0, 200, 400, 600, 800),limits = c(-100,820)) +
  theme_minimal() +
  theme(legend.position = "right",
        legend.title = element_blank(),
        text = element_text(size = 15),
        axis.text.x = element_text(angle = 45, hjust = 1)) 
dev.off()
system("pdflatex -output-directory Figures/  Figures/cumprod_returns_low_vola_portfolios_rv_scaled_increasing.tex")

df_portfolios_80 <-
  readRDS("portfolio_sorts/df_low_vol_portfolios_rv_scaled_as_dependent_end_2021_increasing_window_scaling_ss_old_filter.rds")

df_oo_inv_sqrt_fcst_80 <- 
  df_portfolios_80 %>% 
  filter(model == "oracle_rv_sq_overnight") %>%
  select(symbol, year_month, weight_inv_sqrt_fcst_oracle = weight_inv_sqrt_fcst) %>%
  right_join(df_portfolios_80) %>%
  group_by(model, year_month) %>%
  summarise(oracle_overlap = sum(pmin(weight_inv_sqrt_fcst, weight_inv_sqrt_fcst_oracle)))

df_summaries_inv_sqrt_fcst_weighted_80 <-
  df_portfolios_80 %>%
  left_join(
    df_portfolios_80 %>%
      filter(model == "oracle_rv_sq_overnight") %>%
      select(year_month, symbol, weight_oracle = weight_inv_sqrt_fcst)) %>%
  left_join(df_har) %>%
  left_join(df_fama_monthly, by = "year_month") %>%
  filter(year_month >= "2005-01-01", year_month <= "2021-12-01") %>%
  group_by(model, year_month) %>%
  summarise(mean_ret = sum((ret_crsp) * weight_inv_sqrt_fcst),
            mean_rv_sq_overnight = sum(rv_sq_overnight * weight_inv_sqrt_fcst),
            oracle_distance = sqrt(sum((weight_inv_sqrt_fcst- weight_oracle)^2)),
            n_stocks = n(),
            rf = unique(rf)) %>%
  ungroup() %>%
  left_join(readRDS("portfolio_sorts/turnover_df_low_vol_portfolios_rv_scaled_as_dependent_end_2021_increasing_window_scaling_ss_old_filter.rds")) %>%
  left_join(df_oo_inv_sqrt_fcst_80) %>%
  group_by(model) %>%
  mutate(wealth_25 = cumprod(( 1 + mean_ret / 100) * (1 - 0.0025 * turnover / 100))) %>%
  mutate(mean_ret_25 = 100 * (wealth_25 / lag(wealth_25, order_by = year_month) - 1)) %>%
  mutate(mean_ret_25 = if_else(is.na(mean_ret_25), 100 * (wealth_25 - 1), mean_ret_25)) %>%
  summarise(ret = mean(mean_ret - rf) * 12,
            sd_ret = sd(mean_ret - rf) * sqrt(12),
            sr = mean(mean_ret - rf) / sd(mean_ret - rf) * sqrt(12),
            arv = sqrt(mean(mean_rv_sq_overnight)) * sqrt(12),
            od = mean(oracle_distance) * 100,
            to = mean(turnover, na.rm = TRUE),
            ret_25 = mean(mean_ret_25 - rf) * 12,
            sr_25 = mean(mean_ret_25 - rf) / sd(mean_ret - rf) * sqrt(12))


sink(file = paste0("Tables/summary_table_oracle_portfolio_increasing_scaling_ss.tex"), append = FALSE, type = "output")
cat("\\begin{tabular}{lccccccccccccccc} \n")
cat("\\toprule \n")
cat(" & Ret & Std & SR \\\\")
cat("\n \\midrule \n")
cat(" Post-hoc & ")
cat(formatC({df_summaries_inv_sqrt_fcst_weighted_80 %>% filter(model == "oracle_rv_sq_overnight")}[2:4] %>% unlist(), digits = 2, format = "f"), sep = " & ")
cat("\\\\ \n ")
# cat("\\\\  \n")
cat("\\midrule \n")
cat(" 12m-$\\text{RV}^d$ & ")
cat(formatC({df_summaries_inv_sqrt_fcst_weighted_80 %>% filter(model == "last_year_sq_ret")}[2:4] %>% unlist(), digits = 2, format = "f"), sep = " & ")
cat("\\\\  \n")
cat("\n \\midrule \n")
cat(" 12m-$\\text{RV}^d$ & ")
cat(formatC(unlist(sum_spx_low_vol), digits = 2, format = "f"), sep = " & ")
cat("\\\\  \n")
cat("\n \\midrule \n")
cat(" S\\&P 500 TR  & ")
cat(formatC(unlist(sum_market), digits = 2, format = "f"), sep = " & ")
# cat(" & --- & --- & --- & ---  ")
cat("\\\\")
cat("\\bottomrule \n")
cat("\\end{tabular}")
sink(file = NULL)

df_equal_weighted <-
  readRDS("portfolio_sorts/df_all_portfolios_rv_scaled_as_dependent_end_2021_increasing_window_scaling_ss_old_filter.rds") %>% 
  filter(model %in% c("oracle_rv_sq_overnight", "last_year_sq_ret")) %>%
  filter(year_month >= "2005-01-01") %>%
  left_join(df_fama_monthly, by = "year_month") |>
  group_by(model, decile_id, year_month) |> # the name decile_id is not the best as we are looking at quintiles
  summarize(return = mean(ret_crsp - rf))  %>%
  group_by(model, decile_id) |> # decile_id is just not the best name 
  summarise(mean_ret = mean(return) / sd(return) * sqrt(12)) |>
  ungroup() |>
  spread(decile_id, mean_ret)

sink(file = paste0("Tables/equal_weighted_sharpe_ratios.tex"), append = FALSE, type = "output")
cat("\\begin{tabular}{lccccccccccccccc} \n")
cat("\\toprule \n")
cat(" & Quintile & 1 & 2 & 3 & 4 & 5 \\\\")
cat("\n \\midrule \n")
cat("Trailing volatility sort & ")
cat(formatC(unlist(df_equal_weighted[1,2:6]), digits = 2, format = "f"), sep = " & ")
cat("\n \\\\ \\midrule")
cat("Post-hoc volatility sort & ")
cat(formatC(unlist(df_equal_weighted[2,2:6]), digits = 2, format = "f"), sep = " & ")
cat("\n \\\\")
cat("\\bottomrule \n")
cat("\\end{tabular}")
sink(file = NULL)

