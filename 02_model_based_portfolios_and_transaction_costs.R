
# ------------------------------------------------------------------------------
# Script: 02_model_based_portfolios_and_transaction_costs.R
# Purpose: 
#   - Construct model-based portfolios using volatility forecasts.
#   - Calculate and save portfolio returns and transaction costs.
#
# Inputs:
#   - Model forecasts and realized returns
#   - Transaction cost data
#   - Helper functions
#
# Outputs:
#   - Portfolio weights, returns, and transaction cost summaries as .rds files
#
# Author: Onno Kleen
# ------------------------------------------------------------------------------

rm(list = ls())

source("_load_packages.R")
source("functions/helpers.R")

df_returns_monthly <- 
  read_csv("data/crsp_monthly.csv") %>%
  distinct() %>% 
  mutate(PRC = abs(PRC),
         year_month = floor_date(date, "months"),
         DLRET = as.numeric(DLRET) * 100) %>%
  mutate(RET = as.numeric(RET) * 100) %>%
  mutate(RET = ifelse(is.na(DLRET) == FALSE, DLRET, RET)) %>%
  mutate(RET = ifelse(DLSTCD %in% c(500,520, 551:573,574, 580,584) & is.na(DLRET) == TRUE & year_month != "2024-12-01", -30, RET)) %>%
  mutate(RET = ifelse(!(DLSTCD %in% c(500,520, 551:573,574, 580,584)) & is.na(DLSTCD) == FALSE & is.na(DLRET) == TRUE & year_month != "2024-12-01", -100, RET)) %>%
  mutate(mktcap = SHROUT * abs(ALTPRC)) %>%
  select(PERMNO, year_month,RET, mktcap, PRC, ALTPRC) %>%
  rename(ret_crsp = RET,
         symbol = PERMNO) %>%
  group_by(symbol) %>% 
  mutate(lag_mktcap = lag(mktcap, order_by = year_month)) %>%
  filter(year_month >= "1993-01-01") %>%
  ungroup()


df_fama_monthly <-
  readRDS("data/df_fama_french_monthly.rds") %>%
  select(year_month, mkt_rf, rf) %>%
  filter(year_month >= "1993-01-01" & year_month <= "2022-01-31")

df_portfolios <-
  readRDS("portfolio_sorts/df_forecast_raw_rv_scaled_as_dependent_end_2021_increasing_window_scaling_ss_old_filter.rds") %>%
  left_join(df_returns_monthly %>% select(symbol, year_month, ret_crsp, lag_mktcap)) %>%
  filter(is.na(lag_mktcap) == FALSE) %>%
  group_by(model, year_month) %>% 
  filter(rank(lag_mktcap) >= n() - 499) %>%
  mutate(decile_id = identify_portfolio_quintile(forecast)) %>% # decile_id is not the best name
  ungroup()

saveRDS(df_portfolios, "portfolio_sorts/df_all_portfolios_rv_scaled_as_dependent_end_2021_increasing_window_scaling_ss_old_filter.rds")

df_portfolios_low <-
  df_portfolios %>%
  mutate(forecast = if_else(forecast == 0, 0.001, forecast)) |> # for one case in oracle_rv_sq_ret
  group_by(year_month, model) %>%
  mutate(weight_inv_sqrt_fcst =  if_else(decile_id != "quintile_08_10", 1 / sqrt(forecast), 0)) %>%
  mutate(weight_inv_sqrt_fcst = weight_inv_sqrt_fcst / sum(weight_inv_sqrt_fcst)) %>%
  mutate(weight_inv_sqrt_fcst = regularize_weights(weight_inv_sqrt_fcst, 0.05)) %>%
  ungroup()

saveRDS(df_portfolios_low, "portfolio_sorts/df_low_vol_portfolios_rv_scaled_as_dependent_end_2021_increasing_window_scaling_ss_old_filter.rds")

df_costs <- read_csv("data/Costs_filled.csv") %>%
  mutate(year_month = my(yearmonth)) %>%
  rename(symbol = permno,
         costs = c_new) %>%
  select(symbol, year_month, costs) %>%
  filter(year_month >= "1993-01-01") %>%
  distinct() %>%
  filter(!is.na(costs)) %>%
  group_by(symbol, year_month) %>%
  summarize(costs = mean(costs)) %>%
  ungroup()

df_avg_costs <- 
  df_costs %>%
  group_by(year_month) %>%
  summarize(avg_costs = mean(costs))

df_transactions_costs_inv_sqrt_fcst_weighted <- foreach (t = unique(df_portfolios_low$year_month)[-c(1:(12 * 4))], .combine = "bind_rows", .packages = c("dplyr", "foreach", "lubridate"), .multicombine = TRUE) %do% {
  print(t)
  
  
  df_portfolios_this_month <-
    df_portfolios_low %>%
    filter(year_month == t)
  
  df_portfolio_avg_returns <-
    df_portfolios_this_month %>%
    group_by(model) %>%
    summarise(mean_return = sum(ret_crsp * weight_inv_sqrt_fcst), .groups = "drop")
  
  df_portfolios_this_month <-
    df_portfolios_this_month %>% 
    split(.$model)
  
  df_portfolios_next_month <-
    df_portfolios_low %>%
    filter(year_month == t + months(1)) %>% 
    split(.$model)
  
  foreach (model_id = unique(df_portfolios_low$model), .combine = "bind_rows", .multicombine = TRUE) %do% {
    
    if (t < max(unique(df_portfolios_low$year_month))) {
      
      df_this_month <-
        df_portfolios_this_month[[model_id]] %>%
        transmute(symbol = symbol,
                  weight_this_month = weight_inv_sqrt_fcst,
                  ret_this_month = ret_crsp)
      
      df_next_month <-
        df_portfolios_next_month[[model_id]] %>%
        filter(model == model_id) %>%
        transmute(weight_next_month = weight_inv_sqrt_fcst,
                  symbol = symbol)
      
      res <-
        full_join(df_this_month, df_next_month, by = "symbol") %>%
        mutate(weight_this_month = ifelse(is.na(weight_this_month) == TRUE, 0, weight_this_month),
               ret_this_month = ifelse(is.na(ret_this_month) == TRUE, 0, ret_this_month),
               weight_next_month = ifelse(is.na(weight_next_month) == TRUE, 0, weight_next_month),
               mean_ret_this_month = dplyr::filter(df_portfolio_avg_returns, model == model_id)$mean_return) %>%
        mutate(weight_end_of_month = weight_this_month * (1 + ret_this_month / 100) / (1 + mean_ret_this_month / 100)) %>%
        mutate(turnover = abs(weight_next_month - weight_end_of_month)) %>%
        left_join(df_costs %>% filter(year_month == t), by = "symbol") %>%
        mutate(year_month = t) %>% # necessary to join transaction costs for new entries
        left_join(df_avg_costs, by = join_by(year_month)) %>%
        mutate(costs = if_else(is.na(costs), avg_costs, costs)) %>%
        summarise(ta_costs = sum(turnover * costs) * 100,
                  turnover = sum(turnover) * 100, 
                  .groups = "drop") %>%
        mutate(model = model_id,
               year_month = t)
    } else {
      res <-
        tibble(model = model_id,  year_month = t, turnover = 0, ta_costs = 0)
    }
    res
  }
}


saveRDS(df_transactions_costs_inv_sqrt_fcst_weighted, "portfolio_sorts/turnover_df_low_vol_portfolios_rv_scaled_as_dependent_end_2021_increasing_window_scaling_ss_old_filter.rds")





