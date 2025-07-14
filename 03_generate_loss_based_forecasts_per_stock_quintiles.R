
# ------------------------------------------------------------------------------
# Script: 03_generate_loss_based_forecasts_per_stock_quintiles.R
# Purpose: 
#   - Generate loss-based combined forecasts for each stock using different
#     loss functions and combination parameters.
#   - Construct portfolios based on these combined forecasts and evaluate
#     their performance and transaction costs.
#
# Inputs:
#   - Model forecasts, realized volatility, and return data
#   - Helper functions
#
# Outputs:
#   - Combined forecast portfolios and transaction cost summaries as .rds files
#
# Author: Onno Kleen
# ------------------------------------------------------------------------------

rm(list = ls())

source("_load_packages.R")
source("functions/helpers.R")

generate_loss_based_portfolios_per_stock <- function (eta, window.length.in.months) {
  
  df_portfolios_loss <- foreach (ii = seq.Date(ymd("2005-01-01"), ymd("2021-12-01"), "months"), .combine = "bind_rows",
                                 .multicombine = TRUE) %do% {
    
    print(ii)
    
    df_returns_month <-
      df_returns %>%
      filter(year_month == ii) %>%
      filter(!is.na(ret_crsp), !is.na(lag_mktcap)) %>%
      filter(rank(lag_mktcap) >= n() - 499)
      
    df_model_selected <-
      df_losses_raw %>%
      filter(symbol %in% df_returns_month$symbol) %>%
      select(year_month, model, symbol, se, qlike) %>%
      filter(year_month <= ii - months(1), 
             year_month >= ii - months(window.length.in.months)) %>%
      group_by(model, symbol) %>%
      summarise(across(c(se, qlike), ~ mean(.x, na.rm = TRUE), .names = "{.col}"), .groups = "drop") %>%
      gather(loss_function, mean_loss, se, qlike)
    
    
    if (is.infinite(eta)) {
      df_model_selected <- 
        df_model_selected %>%
        group_by(loss_function, symbol) %>%
        filter(mean_loss == min(mean_loss)) %>%
        ungroup() %>%
        select(loss_function,  model, symbol) %>%
        mutate(year_month = ii) %>%
        left_join(df_forecasts_for_models_select %>% filter(year_month == ii, symbol %in% unique(df_returns_month$symbol)), by = c("year_month", "model", "symbol")) %>%
        group_by(year_month, loss_function, symbol) %>%
        summarise(forecast = mean(forecast), .groups = "drop") %>% # if multiple models run into the boundary
        ungroup() %>%
        rename(model = loss_function)
    } else {
      df_model_selected <- 
        df_model_selected %>%
        group_by(loss_function, symbol) %>%
        mutate(forecast_weight = (1 / mean_loss)^eta / sum((1 / mean_loss)^eta)) %>%
        select(loss_function,  model, forecast_weight, symbol) %>%
        mutate(year_month = ii) %>%
        left_join(df_forecasts_for_models_select %>% filter(year_month == ii, symbol %in% unique(df_returns_month$symbol)), by = c("year_month", "model", "symbol")) %>%
        group_by(year_month, loss_function, symbol) %>%
        summarise(forecast = sum(forecast_weight * forecast), .groups = "drop") %>%
        ungroup() %>%
        rename(model = loss_function)
    }
    
    df_model_selected %>%
      group_by(model, year_month) %>%
      left_join(df_returns_month, by = c("year_month", "symbol")) %>%
      mutate(decile_id = identify_portfolio_quintile_fast(forecast)) %>%
      ungroup() %>%
      group_by(year_month, model) %>%
      mutate(weight_inv_sqrt_fcst =  if_else(decile_id != "quintile_08_10", 1 / sqrt(forecast), 0)) %>%
      mutate(weight_inv_sqrt_fcst = regularize_weights(weight_inv_sqrt_fcst, 0.05)) %>%
      ungroup()
  }  
    
  saveRDS(df_portfolios_loss,
          paste0("portfolio_sorts/df_portfolios_loss_eta_", eta, "_delta_", window.length.in.months, "_80_500_old_filter_ss_scaling_increasing_window.rds"))
  
}

df_returns <-
  readRDS("portfolio_sorts/df_all_portfolios_rv_scaled_as_dependent_end_2021_increasing_window_scaling_ss_old_filter.rds") %>% 
  filter(model == "oracle_rv_sq_overnight") %>% 
  select(year_month, symbol, ret_crsp, lag_mktcap)

df_forecast_raw <- readRDS("portfolio_sorts/df_forecast_raw_rv_scaled_as_dependent_end_2021_increasing_window_scaling_ss_old_filter.rds")

df_har <- readRDS("data/df_har_end_2021_scaled_increasing_window_ss.rds") 

df_losses_raw <-
  df_har %>%
  filter(is.finite(rv_sq_overnight)) %>%
  group_by(year_month, symbol) %>%
  summarise(rv_sq_overnight = mean(rv_sq_overnight, na.rm = TRUE) * 22) %>%
  right_join(df_forecast_raw) %>%
  filter(model != "oracle_rv_sq_overnight", 
         model != "oracle_rv_sq_ret", model != "avg_forecast")  %>%
  mutate(se = (forecast - rv_sq_overnight)^2,
         qlike = rv_sq_overnight / forecast - log(rv_sq_overnight / forecast) - 1)

rm(df_forecast_raw)
rm(df_har)

df_forecasts_for_models_select <-
  df_losses_raw %>%
  select(year_month, symbol, model, forecast)

gc()

generate_loss_based_portfolios_per_stock(0, 1)


foreach (kk = c(1/2, 1, Inf)) %do% {
  foreach (jj = c(12, 48)) %do% { # 1000 is for all-there-is forecast evaluation
    gc()
    generate_loss_based_portfolios_per_stock(kk, jj)
  }
} 

### Transaction costs non-smoothed #############

df_costs <- read_csv("data/Costs_filled.csv") %>%
  mutate(year_month = my(yearmonth)) %>%
  rename(symbol = permno,
         costs = c_new) %>%
  select(symbol, year_month, costs) %>%
  filter(year_month >= "1997-01-01") %>%
  distinct() %>%
  filter(!is.na(costs)) %>%
  group_by(symbol, year_month) %>%
  summarize(costs = mean(costs)) %>%
  ungroup()

df_avg_costs <- 
  df_costs %>%
  group_by(year_month) %>%
  summarize(avg_costs = mean(costs))

generate_loss_based_portfolios_to_non_smoothed <- function (eta, window.length.in.months) {
  
  df_portfolios_loss <- 
    readRDS(paste0("portfolio_sorts/df_portfolios_loss_eta_", eta, "_delta_", window.length.in.months, "_80_500_old_filter_ss_scaling_increasing_window.rds")) %>%
    select(-decile_id)
  
  
  df_transactions_costs_loss_sqrt_fcst_weighted <- foreach (t = unique(df_portfolios_loss$year_month), .combine = "bind_rows", .packages = c("dplyr", "foreach", "lubridate"), .multicombine = TRUE) %do% {
    print(t)
    
    df_portfolio_avg_returns <-
      df_portfolios_loss %>%
      filter(year_month == t) %>%
      group_by(model) %>%
      summarise(mean_return = sum(ret_crsp * weight_inv_sqrt_fcst), .groups = "drop") %>%
      ungroup()
    
    foreach (model_id = unique(df_portfolios_loss$model), .combine = "bind_rows", .multicombine = TRUE) %do% {
      
      if (t < max(unique(df_portfolios_loss$year_month))) {
        
        df_this_month <-
          df_portfolios_loss %>%
          filter(year_month == t, model == model_id) %>%
          mutate(weights = weight_inv_sqrt_fcst) %>%
          transmute(symbol = symbol,
                    weight_this_month = weights,
                    ret_this_month = ret_crsp)
        
        df_next_month <-
          df_portfolios_loss %>%
          filter(year_month == t + months(1) & model == model_id) %>%
          mutate(weights = weight_inv_sqrt_fcst) %>%
          transmute(weight_next_month = weights,
                    symbol = symbol)
        
        res <-
          full_join(df_this_month, df_next_month, by = "symbol") %>%
          mutate(weight_this_month = ifelse(is.na(weight_this_month), 0, weight_this_month),
                 ret_this_month = ifelse(is.na(ret_this_month), 0, ret_this_month),
                 weight_next_month = ifelse(is.na(weight_next_month), 0, weight_next_month),
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
  
  saveRDS(df_transactions_costs_loss_sqrt_fcst_weighted,
          paste0("portfolio_sorts/transaction_costs_loss_eta_", eta, "_delta_", window.length.in.months, "_80_500_old_filter_ss_scaling_increasing_window.rds"))
  
}

generate_loss_based_portfolios_to_non_smoothed(0, 1)
foreach (kk = c(1/2, 1, Inf)) %do% {
  foreach (jj = c(12, 48)) %do% {
    
    gc()
    generate_loss_based_portfolios_to_non_smoothed(kk, jj)
  }
} 

