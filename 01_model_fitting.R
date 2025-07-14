# ------------------------------------------------------------------------------
# Script: 01_model_fitting.R
# Purpose: 
#   - Fit various volatility forecasting models (HAR, GARCH, MEM, MIDAS, etc.)
#     to stock return data.
#   - Generate and save out-of-sample forecasts for each model and stock.
#
# Inputs:
#   - Stock return and realized volatility data
#   - Market capitalization and VIX data
#   - Helper functions and model fitting scripts
#
# Outputs:
#   - Model forecasts saved as .rds files in the models/ directory
#   - Combined forecast data for later portfolio construction and evaluation
#
# Author: Onno Kleen
# ------------------------------------------------------------------------------

rm(list = ls())

source("_load_packages.R")
source("functions/helpers.R")

df_vix <-
  read_csv("data/VIX_History.csv") %>%
  mutate(date = as.Date(DATE, "%m/%d/%Y")) %>%
  select(-DATE) %>%
  mutate(vix = CLOSE) %>%
  select(date, vix) %>%
  mutate(vix_sqrt_252 = vix/sqrt(252))

df_mktcap_filter <-
  read_csv("data/crsp_monthly.csv") %>%
  mutate(mktcap = SHROUT * abs(ALTPRC) / 1000) %>%
  filter(!is.na(mktcap)) %>%
  mutate(year_month = floor_date(date, "months")) %>%
  rename(symbol = PERMNO) %>%
  select(year_month, symbol, mktcap)

df_har <- readRDS("data/df_har_end_2021_scaled_increasing_window_ss.rds") |>
  filter(n_intraday >= 50)
months <- unique(df_har$year_month)


reestimate_har_models <- T
reestimate_garch_models <- T
reestimate_panelgarch_models <- T
reestimate_capmgarch_models <- T
reestimate_capmgarch_midas_models <- T
reestimate_memgarch_models <- T
reestimate_panelmemgarch_models <- T
reestimate_midas_models <- T
reestimate_panelmidas_models <- T

path_models <- "models/"
dir.create(path_models)


df_forecast_raw <- foreach (t = months[months >= "1997-01-01"], # our first start date for estimation is 1997-01-01
                            .packages = c("rugarch", "zoo", "dplyr", "tidyr", "lubridate", "purrr", "forecast", "mfGARCH"),
                            .combine = "bind_rows") %do% {
  print(t)
  
  df_portfolio <-
    df_har %>%
    filter(year_month <= t & year_month >= t - years(4)) %>%
    group_by(symbol) %>%
    mutate(n = n()) %>%
    filter(min(n_intraday) >= 5, !is.na(ret_close_close)) %>%
    filter(rv_sq_overnight != 0) %>%
    filter(!is.na(rv_sq_overnight)) |>
    ungroup() %>%
    filter(n >= 800) %>%
    select(-n)
  
  symbols_next_month <- # to exclude stocks that are known to be delisted
    df_portfolio %>%
    filter(year_month == t) %>%
    select(symbol) %>%
    unique() %>%
    unlist()
  
  symbols_price_filter <-
    df_portfolio %>%
    filter(year_month == t - months(1)) %>%
    filter(date == max(date)) %>%
    filter(close >= 1) %>%
    select(symbol) %>%
    unique() %>%
    unlist()

  symbols_mkt_filter <-
    df_mktcap_filter %>%
    filter(year_month == t - months(1),
           mktcap >= 1) %>%
    select(symbol) %>%
    unique() %>%
    unlist()
  
  df_portfolio <-
    df_portfolio %>%
    filter(symbol %in% symbols_next_month) %>%
    filter(symbol %in% symbols_price_filter) %>%
    filter(symbol %in% symbols_mkt_filter)
  
  symbols_mkt_filter_500 <- # only include 500 largest stocks for panel parameter estimations
    df_mktcap_filter %>%
    filter(year_month == t - months(1),
           symbol %in% df_portfolio$symbol) %>%
    filter(rank(mktcap) >= n() - 499) %>%
    select(symbol) %>%
    unique() %>%
    unlist()
  
  oracle_forecast_rv_sq_overnight <-
    df_portfolio %>%
    filter(year_month == t) %>%
    group_by(symbol) %>%
    summarise(forecast = mean(rv_sq_overnight, na.rm = TRUE) * 22, .groups = "drop") %>%
    mutate(model = "oracle_rv_sq_overnight")
  
  oracle_forecast_rv_sq_ret <-
    df_portfolio %>%
    filter(year_month == t) %>%
    group_by(symbol) %>%
    summarise(forecast = mean(ret_close_close^2, na.rm = TRUE) * 22, .groups = "drop") %>%
    mutate(model = "oracle_rv_sq_ret")
  
  if (reestimate_garch_models) {
    
    source("functions/fit_panelgarch.R")
    
    df_list_garch <-
      df_portfolio %>%
      select(date, year_month, symbol,ret_close_close) %>%
      filter(year_month < t) %>%
      split(.$symbol)

    gjrgarch_forecasts <-
      df_list_garch %>%
      lapply(FUN =
               function(x) fit_panelgarch(as.matrix(x %>% select(date, symbol,ret_close_close) %>% spread(symbol, ret_close_close) %>% select(-date), ncol = 1))) %>%
      bind_rows() %>%
      mutate(model = "gjrgarch")
    dir.create(paste0(path_models, "gjrgarch/"))
    saveRDS(gjrgarch_forecasts, paste0(path_models, "gjrgarch/", t, ".rds"))

  }

  gjrgarch_forecasts <- readRDS(paste0(path_models, "gjrgarch/", t, ".rds"))

  if (reestimate_memgarch_models) {

    source("functions/fit_panelgarch.R")
    df_list_memgarch <-
      df_portfolio %>%
      mutate(sqrt_rv_sq_overnight = sqrt(rv_sq_overnight)) %>%
      select(date, year_month, symbol,ret_close_close, sqrt_rv_sq_overnight) %>%
      filter(year_month < t) %>%
      split(.$symbol)

    memgarch_forecasts <-
      df_list_memgarch %>%
      mclapply(FUN =
               function(x) fit_panelgarch(as.matrix(x %>% select(date, symbol, sqrt_rv_sq_overnight) %>% 
                                                      spread(symbol, sqrt_rv_sq_overnight) %>% select(-date), ncol = 1), gamma = FALSE),
               mc.cores = 6) %>%
      bind_rows() %>%
      mutate(model = "memgarch")
    dir.create(paste0(path_models, "memgarch/"))
    saveRDS(memgarch_forecasts, paste0(path_models, "memgarch/", t, ".rds"))
  }
  memgarch_forecasts <- readRDS(paste0(path_models, "memgarch/", t, ".rds"))

  if (reestimate_panelgarch_models) {
    source("functions/fit_panelgarch.R")

    rdata <-
      df_portfolio %>%
      filter(year_month < t) %>%
      filter(symbol %in% symbols_mkt_filter_500) %>%
      select(date, symbol,ret_close_close) %>%
      spread(symbol, ret_close_close) %>%
      select(-date) %>%
      as.matrix()
    rdata_forecast <-
      df_portfolio %>%
      filter(year_month < t) %>%
      select(date, symbol,ret_close_close) %>%
      spread(symbol, ret_close_close) %>%
      select(-date) %>%
      as.matrix()

    dir.create(paste0(path_models, "panelgarch/"))
    saveRDS(fit_panelgarch(rdata, forecastdata = rdata_forecast) %>% mutate(model = "panelgarch"), paste0(path_models, "panelgarch/", t, ".rds"))

  }
  panelgarch_forecasts <- readRDS(paste0(path_models, "panelgarch/", t, ".rds"))

  if (reestimate_panelmemgarch_models) {

    source("functions/fit_panelgarch.R")
    rvdata <-
      df_portfolio %>%
      filter(year_month < t) %>%
      filter(symbol %in% symbols_mkt_filter_500) %>%
      mutate(sqrt_rv_sq_overnight = sqrt(rv_sq_overnight)) %>%
      select(date, symbol,sqrt_rv_sq_overnight) %>%
      spread(symbol, sqrt_rv_sq_overnight) %>%
      select(-date) %>%
      as.matrix()
    rvdata_forecast <-
      df_portfolio %>%
      filter(year_month < t) %>%
      mutate(sqrt_rv_sq_overnight = sqrt(rv_sq_overnight)) %>%
      select(date, symbol,sqrt_rv_sq_overnight) %>%
      spread(symbol, sqrt_rv_sq_overnight) %>%
      select(-date) %>%
      as.matrix()

    dir.create(paste0(path_models, "panelmemgarch/"))
    saveRDS(fit_panelgarch(rvdata, gamma = FALSE, forecastdata = rvdata_forecast) %>% mutate(model = "panelmemgarch"), paste0(path_models, "panelmemgarch/", t, ".rds"))
  }
  panelmemgarch_forecasts <- readRDS(paste0(path_models, "panelmemgarch/", t, ".rds"))

  if (reestimate_capmgarch_models) {

    source("functions/fit_capmgarch.R")
    df <-
      df_portfolio %>%
      select(date, year_month, symbol, ret_close_close) %>%
      filter(year_month < t)

    dir.create(paste0(path_models, "capmgarch/"))
    saveRDS(fit_capmgarch(df) %>% mutate(model = "capmgarch"), paste0(path_models, "capmgarch/", t, ".rds"))
  }
  capmgarch_forecasts <- readRDS(paste0(path_models, "capmgarch/", t, ".rds"))
  
  if (reestimate_capmgarch_midas_models) {

    source("functions/fit_capmgarch_midas.R")
    df <-
      df_portfolio %>%
      select(date, year_month, symbol,ret_close_close) %>%
      filter(year_month < t)

    dir.create(paste0(path_models, "capmgarch_midas/"))
    saveRDS(fit_capmgarch_midas(df), paste0(path_models, "capmgarch_midas/", t, ".rds"))
  }
  capmgarch_midas_forecasts <- readRDS(paste0(path_models, "capmgarch_midas/", t, ".rds"))

  if (reestimate_midas_models) {

    source("functions/fit_panelmidas.R")
    df_list_midas <- 
      df_portfolio %>%
      select(date, year_month, symbol,rv_sq_overnight) %>%
      filter(year_month < t) %>% # no data leakage due to how the function is written
      split(.$symbol)
    
    midas_forecasts <-
      df_list_midas %>%
      mclapply(FUN =
               function(x) fit_panelmidas(as.matrix(x %>% select(date, symbol,rv_sq_overnight) %>% 
                                                      spread(symbol, rv_sq_overnight) %>% select(-date),
                                                    ncol = 1), trace = TRUE),
               mc.cores = 6) %>%
      bind_rows() %>%
      mutate(model = "midas")
    dir.create(paste0(path_models, "midas/"))
    saveRDS(midas_forecasts, paste0(path_models, "midas/", t, ".rds"))

  }
  midas_forecasts <- readRDS(paste0(path_models, "midas/", t, ".rds"))

  if (reestimate_panelmidas_models) {

    source("functions/fit_panelmidas.R")
    rvdata <-
      df_portfolio %>%
      filter(date < t) %>%
      filter(symbol %in% symbols_mkt_filter_500) %>%
      select(date, symbol,rv_sq_overnight) %>%
      spread(symbol, rv_sq_overnight) %>%
      select(-date) %>%
      as.matrix()
    rvdata_forecast <-
      df_portfolio %>%
      filter(date < t) %>%
      select(date, symbol,rv_sq_overnight) %>%
      spread(symbol, rv_sq_overnight) %>%
      select(-date) %>%
      as.matrix()

    dir.create(paste0(path_models, "panelmidas/"))
    saveRDS(fit_panelmidas(rvdata = rvdata, forecastdata = rvdata_forecast) %>% mutate(model = "panelmidas"), 
            paste0(path_models, "panelmidas/", t, ".rds"))
  }
  
  panelmidas_forecasts <- readRDS(paste0(path_models, "panelmidas/", t, ".rds"))

  if (reestimate_har_models) {

    df_list_har_log <-
      df_portfolio %>%
      filter(year_month < t) %>%
      mutate(across(contains("sq_overnight"), log)) |>
      group_by(symbol) %>%
      fill(vix) %>%
      ungroup() %>%
      split(.$symbol)
    
    dir.create(paste0(path_models, "log_har/"))
    log_har <-
      df_list_har_log %>%
      lapply(FUN =
               function(x) {
                 list(lm = lm(data = x[1:(dim(x)[1]-22),], lead_rv_22_sq_overnight ~ rv_sq_overnight + rv_5_sq_overnight + rv_22_sq_overnight),
                      data = x)}) %>%
      lapply(FUN =
               function(x) {
                 list(model = "log_har",
                      forecast = exp(predict(x$lm, newdata = filter(x$data, date == max(date))) + 1/2 * var(x$lm$residuals)) * 22,
                      coefficients = x$lm$coefficients)})
    
    
    saveRDS(log_har, paste0(path_models, "log_har/", t, ".rds"))
    
    
    df_list_har <-
      df_portfolio %>%
      filter(year_month < t) %>%
      group_by(symbol) %>%
      fill(vix) %>%
      ungroup() %>%
      split(.$symbol)
    
    # # The x[...] statements are to counter look-ahead bias that would otherwise be introduced
    har <-
      df_list_har %>%
      lapply(FUN =
               function(x) {
                 list(lm = lm(data = x[1:(dim(x)[1]-22),], lead_rv_22_sq_overnight ~ rv_sq_overnight + rv_5_sq_overnight + rv_22_sq_overnight),
                      data = x)}) %>%
      lapply(FUN =
               function(x) {
                 list(model = "har",
                      forecast = predict(x$lm, newdata = filter(x$data, date == max(date))) * 22,
                      coefficients = x$lm$coefficients)})

    df_spx_forecasts <-
      readRDS("data/df_spx_daily_extended.rds") %>%
      mutate(year_month = floor_date(date, "months")) %>%
      filter(year_month < t, year_month >= t - years(4)) %>%
      mutate(rv_scaled = spx_rv * mean(spx_ret_close_close^2) / mean(spx_rv)) %>%
      mutate(spx_rv_sq_overnight = rv_scaled,
             spx_rv_sq_overnight_lead_22 = rollmean(lead(spx_rv_sq_overnight), k = 22, fill = NA, align = "left"),
             spx_rv_sq_overnight_5 = rollmean(spx_rv_sq_overnight, k = 5, fill = NA, align = "right"),
             spx_rv_sq_overnight_22 = rollmean(spx_rv_sq_overnight, k = 22, fill = NA, align = "right")) %>%
      list(lm = lm(data = .[1:(dim(.)[1]-22),], spx_rv_sq_overnight_lead_22 ~ spx_rv_sq_overnight + spx_rv_sq_overnight_5 +  spx_rv_sq_overnight_22),
           data = .)

    df_spx_forecasts <-
      tibble(date = df_spx_forecasts$data$date,
             forecast_spx = df_spx_forecasts$lm$coefficients[1] +
               df_spx_forecasts$lm$coefficients[2] * df_spx_forecasts$data$spx_rv_sq_overnight +
               df_spx_forecasts$lm$coefficients[3] * df_spx_forecasts$data$spx_rv_sq_overnight_5 +
               df_spx_forecasts$lm$coefficients[4] * df_spx_forecasts$data$spx_rv_sq_overnight_22)

    har_spx <-
      df_portfolio %>%
      filter(year_month < t) %>%
      left_join(df_spx_forecasts, by = "date") %>%
      split(.$symbol) %>%
      lapply(FUN =
               function(x) {
                 list(lm = lm(data = x[1:(dim(x)[1]-22),], lead_rv_22_sq_overnight ~ rv_sq_overnight + rv_5_sq_overnight + rv_22_sq_overnight + forecast_spx),
                      data = x)}) %>%
      lapply(FUN =
               function(x) {
                 list(model = "har_spx",
                      forecast = predict(x$lm, newdata = filter(x$data, date == max(date))) * 22,
                      coefficients = x$lm$coefficients)})

    har_long_term <-
      df_list_har %>%
      lapply(FUN =
               function(x) {
                 list(lm = lm(data = x[1:(dim(x)[1]-22),], lead_rv_22_sq_overnight ~ rv_sq_overnight + rv_5_sq_overnight + rv_22_sq_overnight + rv_66_sq_overnight + rv_132_sq_overnight),
                      data = x)}) %>%
      lapply(FUN =
               function(x) {
                 list(model = "har_long_term",
                      forecast = predict(x$lm, newdata = filter(x$data, date == max(date))) * 22,
                      coefficients = x$lm$coefficients)})

    har_long_term_spx <-
      df_portfolio %>%
      filter(year_month < t) %>%
      left_join(df_spx_forecasts, by = "date") %>%
      split(.$symbol) %>%
      lapply(FUN =
               function(x) {
                 list(lm = lm(data = x[1:(dim(x)[1]-22),], lead_rv_22_sq_overnight ~ rv_sq_overnight + rv_5_sq_overnight + rv_22_sq_overnight + rv_66_sq_overnight + rv_132_sq_overnight + forecast_spx),
                      data = x)}) %>%
      lapply(FUN =
               function(x) {
                 list(model = "har_long_term_spx",
                      forecast = predict(x$lm, newdata = filter(x$data, date == max(date))) * 22,
                      coefficients = x$lm$coefficients)})

    har_vix <-
      df_list_har %>%
      lapply(function(x) mutate(x, vix_sq = vix^2)) %>%
      lapply(FUN =
               function(x) {
                 list(lm = lm(data = x[1:(dim(x)[1]-22),], lead_rv_22_sq_overnight ~ rv_sq_overnight + rv_5_sq_overnight + rv_22_sq_overnight + vix_sq),
                      data = x)}) %>%
      lapply(FUN =
               function(x) {
                 list(model = "har_vix",
                      forecast = predict(x$lm, newdata = filter(x$data, date == max(date))) * 22,
                      coefficients = x$lm$coefficients)})

    har_vix_long_term <-
      df_list_har %>%
      lapply(function(x) mutate(x, vix_sq = vix^2)) %>%
      lapply(FUN =
               function(x) {
                 list(lm = lm(data = x[1:(dim(x)[1]-22),], lead_rv_22_sq_overnight ~ rv_sq_overnight + rv_5_sq_overnight + rv_22_sq_overnight + rv_66_sq_overnight + rv_132_sq_overnight + vix_sq),
                      data = x)}) %>%
      lapply(FUN =
               function(x) {
                 list(model = "har_vix_long_term",
                      forecast = predict(x$lm, newdata = filter(x$data, date == max(date))) * 22,
                      coefficients = x$lm$coefficients)})

    panel_har <-
      df_portfolio %>%
      filter(year_month < t) %>%
      group_by(symbol) %>%
      mutate(rv_lr = mean(rv_sq_overnight, na.rm = TRUE)) %>%
      mutate(lead_rv_22_sq_overnight_lr = lead_rv_22_sq_overnight - rv_lr,
             rv_sq_overnight_lr = rv_sq_overnight - rv_lr,
             rv_5_sq_overnight_lr = rv_5_sq_overnight - rv_lr,
             rv_22_sq_overnight_lr = rv_22_sq_overnight - rv_lr) %>%
      ungroup() %>%
      list(lm = lm(data = filter(.[1:(dim(.)[1]-22),], symbol %in% symbols_mkt_filter_500), lead_rv_22_sq_overnight_lr ~ 0 + rv_sq_overnight_lr + rv_5_sq_overnight_lr + rv_22_sq_overnight_lr),
           data = filter(., date == max(date)))

    panel_har_forecasts <-
      panel_har$data %>%
      transmute(model = "panel_har",
                symbol = symbol,
                forecast = (rv_lr + panel_har$lm$coefficients[1] * rv_sq_overnight_lr +
                              panel_har$lm$coefficients[2] * rv_5_sq_overnight_lr +
                              panel_har$lm$coefficients[3] * rv_22_sq_overnight_lr) * 22)

    panel_har_long_term <-
      df_portfolio %>%
      filter(year_month < t) %>%
      group_by(symbol) %>%
      mutate(rv_lr = mean(rv_sq_overnight, na.rm = TRUE)) %>%
      mutate(lead_rv_22_sq_overnight_lr = lead_rv_22_sq_overnight - rv_lr,
             rv_sq_overnight_lr = rv_sq_overnight - rv_lr,
             rv_5_sq_overnight_lr = rv_5_sq_overnight - rv_lr,
             rv_22_sq_overnight_lr = rv_22_sq_overnight - rv_lr,
             rv_66_sq_overnight_lr = rv_66_sq_overnight - rv_lr,
             rv_132_sq_overnight_lr = rv_132_sq_overnight - rv_lr) %>%
      ungroup() %>%
      list(lm = lm(data = filter(.[1:(dim(.)[1]-22),], symbol %in% symbols_mkt_filter_500), lead_rv_22_sq_overnight_lr ~ 0 + rv_sq_overnight_lr + rv_5_sq_overnight_lr + rv_22_sq_overnight_lr + rv_66_sq_overnight_lr + rv_132_sq_overnight_lr),
           data = filter(., date == max(date)))

    panel_har_long_term_forecasts <-
      panel_har_long_term$data %>%
      transmute(model = "panel_har_long_term",
                symbol = symbol,
                forecast = (rv_lr + panel_har_long_term$lm$coefficients[1] * rv_sq_overnight_lr +
                  panel_har_long_term$lm$coefficients[2] * rv_5_sq_overnight_lr +
                  panel_har_long_term$lm$coefficients[3] * rv_22_sq_overnight_lr +
                  panel_har_long_term$lm$coefficients[4] * rv_66_sq_overnight_lr +
                  panel_har_long_term$lm$coefficients[5] * rv_132_sq_overnight_lr) * 22)

    dir.create(paste0(path_models, "har/"))
    dir.create(paste0(path_models, "har_spx/"))
    dir.create(paste0(path_models, "har_long_term/"))
    dir.create(paste0(path_models, "har_long_term_spx/"))
    dir.create(paste0(path_models, "har_vix/"))
    dir.create(paste0(path_models, "har_vix_long_term/"))
    dir.create(paste0(path_models, "panel_har/"))
    dir.create(paste0(path_models, "panel_har_long_term/"))

    saveRDS(har, paste0(path_models, "har/", t, ".rds"))
    saveRDS(har_spx, paste0(path_models, "har_spx/", t, ".rds"))

    saveRDS(har_long_term, paste0(path_models, "har_long_term/", t, ".rds"))
    saveRDS(har_long_term_spx, paste0(path_models, "har_long_term_spx/", t, ".rds"))
    saveRDS(har_vix, paste0(path_models, "har_vix/", t, ".rds"))
    saveRDS(har_vix_long_term, paste0(path_models, "har_vix_long_term/", t, ".rds"))
    saveRDS(panel_har_forecasts, paste0(path_models, "panel_har/", t, ".rds"))
    saveRDS(panel_har_long_term_forecasts, paste0(path_models, "panel_har_long_term/", t, ".rds"))

  }


  har_forecasts <-
    list(readRDS(paste0(path_models, "har/", t, ".rds")),
         readRDS(paste0(path_models, "har_spx/", t, ".rds")),
         readRDS(paste0(path_models, "log_har/", t, ".rds")),
         readRDS(paste0(path_models, "har_long_term/", t, ".rds")),
         readRDS(paste0(path_models, "har_long_term_spx/", t, ".rds")),
         readRDS(paste0(path_models, "har_vix/", t, ".rds")),
         readRDS(paste0(path_models, "har_vix_long_term/", t, ".rds"))) %>%
    lapply(FUN = function(x) lapply(x, FUN = function(x) data.frame(model = x$model, forecast = x$forecast))) %>%
    lapply(FUN = function(x) bind_rows(x, .id = 'symbol')) %>%
    bind_rows() %>%
    ungroup() %>%
    mutate(symbol = as.numeric(symbol)) %>%
    bind_rows(readRDS(paste0(path_models, "panel_har/", t, ".rds")) %>% mutate(symbol)) %>%
    bind_rows(readRDS(paste0(path_models, "panel_har_long_term/", t, ".rds")))

  last_month_rv_forecast <-
    df_portfolio %>%
    filter(year_month == t - months(1)) %>%
    group_by(symbol) %>%
    summarise(forecast = mean(rv_sq_overnight, na.rm = TRUE) * 22, .groups = "drop") %>%
    mutate(model = "last_month_rv") %>%
    ungroup()
  
  last_month_sq_ret_forecast <-
    df_portfolio %>%
    filter(year_month == t - months(1)) %>%
    group_by(symbol) %>%
    summarise(forecast = mean(ret_close_close^2, na.rm = TRUE) * 22, .groups = "drop") %>%
    mutate(model = "last_month_sq_ret") %>%
    ungroup()
  
  last_six_months_sq_ret_forecast <-
    df_portfolio %>%
    filter(year_month < t & year_month >= t - months(6)) %>%
    group_by(symbol) %>%
    summarise(forecast = mean(ret_close_close^2, na.rm = TRUE) * 22, .groups = "drop") %>%
    mutate(model = "last_six_months_sq_ret") %>%
    ungroup()
  
  last_year_sq_ret_forecast <-
    df_portfolio %>%
    filter(year_month < t & year_month >= t - years(1)) %>%
    group_by(symbol) %>%
    summarise(forecast = mean(ret_close_close^2, na.rm = TRUE) * 22, .groups = "drop") %>%
    mutate(model = "last_year_sq_ret") %>%
    ungroup()
  
  last_four_years_sq_ret_forecast <-
    df_portfolio %>%
    filter(year_month < t & year_month >= t - years(4)) %>%
    group_by(symbol) %>%
    summarise(forecast = mean(ret_close_close^2, na.rm = TRUE) * 22, .groups = "drop") %>%
    mutate(model = "last_four_years_sq_ret") %>%
    ungroup()

  riskmetrics_monthly_twelve_month_forecast <-
    df_portfolio %>%
    filter(year_month < t & year_month >= t - years(1)) %>%
    group_by(symbol, year_month) %>%
    summarise(rv_monthly = mean(ret_close_close^2, na.rm = TRUE) * 22, .groups = "drop") %>%
    group_by(symbol) %>%
    summarise(forecast = sum(rv_monthly * rev(riskmetrics_weight(0.97, 12))), .groups = "drop") %>%
    mutate(model = "riskmetrics_monthly_12_months") %>%
    ungroup()

  riskmetrics_monthly_six_month_forecast <-
    df_portfolio %>%
    filter(year_month < t & year_month >= t - months(6)) %>%
    group_by(symbol, year_month) %>%
    summarise(rv_monthly = mean(ret_close_close^2, na.rm = TRUE) * 22, .groups = "drop") %>%
    group_by(symbol) %>%
    summarise(forecast = sum(rv_monthly * rev(riskmetrics_weight(0.97, 6))), .groups = "drop") %>%
    mutate(model = "riskmetrics_monthly_6_months") %>%
    ungroup()

  riskmetrics_daily_twelve_month_forecast <-
    df_portfolio %>%
    filter(year_month < t & year_month >= t - years(1)) %>%
    select(date, symbol, ret_close_close) %>%
    split(.$symbol) %>%
    lapply(., function(x) tibble(forecast = 22 * sum(x$ret_close_close^2 * rev(riskmetrics_weight(0.97, length(x$date)))))) %>%
    bind_rows(.id = 'symbol') %>%
    mutate(model = "riskmetrics_daily_12_months") %>%
    ungroup()

  riskmetrics_daily_six_month_forecast <-
    df_portfolio %>%
    filter(year_month < t & year_month >= t - months(6)) %>%
    select(date, symbol, ret_close_close) %>%
    split(.$symbol) %>%
    lapply(., function(x) tibble(forecast = 22 * sum(x$ret_close_close^2 * rev(riskmetrics_weight(0.97, length(x$date)))))) %>%
    bind_rows(.id = 'symbol') %>%
    mutate(model = "riskmetrics_daily_6_months") %>%
    ungroup()



  df_forecasts_models_without_oracle <-
    purrr::reduce(list(har_forecasts,
                gjrgarch_forecasts %>% mutate(symbol = as.numeric(symbol)),
                panelgarch_forecasts %>% mutate(symbol = as.numeric(symbol)),
                memgarch_forecasts %>% mutate(symbol = as.numeric(symbol)),
                panelmemgarch_forecasts %>% mutate(symbol = as.numeric(symbol)),
                capmgarch_forecasts %>% mutate(symbol = as.numeric(symbol)),
                capmgarch_midas_forecasts %>% mutate(symbol = as.numeric(symbol)),
                midas_forecasts %>% mutate(symbol = as.numeric(symbol)),
                panelmidas_forecasts %>% mutate(symbol = as.numeric(symbol)),
                riskmetrics_monthly_twelve_month_forecast,
                riskmetrics_monthly_six_month_forecast,
                riskmetrics_daily_twelve_month_forecast %>% mutate(symbol = as.numeric(symbol)),
                riskmetrics_daily_six_month_forecast %>% mutate(symbol = as.numeric(symbol))),
           full_join, by = c("symbol", "forecast", "model"))

  df_sanity <-
    df_portfolio %>%
    filter(year_month < t) %>%
    filter(is.finite(rv_sq_overnight)) %>%
    group_by(year_month,symbol) %>%
    summarise(rv_sq_overnight = mean(rv_sq_overnight, na.rm = TRUE) * 22, .groups = "drop") %>%
    ungroup() %>%
    group_by(symbol) %>%
    summarise(lower = 1/3 * quantile(rv_sq_overnight, 0.01),
              upper = 3 * quantile(rv_sq_overnight, 0.99),
              .groups = "drop") |>
    ungroup() |>
    left_join(last_month_rv_forecast |> select(-model, rw_forecast = forecast))
  
  bind_rows(list(df_forecasts_models_without_oracle,
                 last_month_sq_ret_forecast,
                 last_four_years_sq_ret_forecast,
                 last_year_sq_ret_forecast,
                 last_six_months_sq_ret_forecast,
                 last_month_rv_forecast,
                 oracle_forecast_rv_sq_ret,
                 oracle_forecast_rv_sq_overnight)) %>%
    mutate(year_month = t) %>%
    left_join(df_sanity) %>%
    mutate(forecast = if_else(forecast > upper & !(model %in% c("oracle_rv_sq_ret", "oracle_rv_sq_overnight")), upper, forecast),
           forecast = if_else(forecast < lower & !(model %in% c("oracle_rv_sq_ret", "oracle_rv_sq_overnight")), lower, forecast)) %>%
    select(-rw_forecast) |>
    select(-upper,-lower)
}

saveRDS(df_forecast_raw, "portfolio_sorts/df_forecast_raw_rv_scaled_as_dependent_end_2021_increasing_window_scaling_ss_old_filter.rds")



