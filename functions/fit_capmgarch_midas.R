
source("functions/fit_panelgarch.R")

df_vix <-
  read_csv("data/VIX_History.csv") %>%
  mutate(date = as.Date(DATE, "%m/%d/%Y")) %>%
  select(-DATE) %>%
  mutate(vix = CLOSE) %>%
  select(date, vix) %>%
  mutate(vix_sqrt_252 = vix/sqrt(252))


df_term_spread <-
  readxl::read_xls("data/allmonth.xls") %>%
  select(Date, Spread) %>%
  transmute(year_month = floor_date(as_date(Date), "months"),
            term_spread = Spread)

# rdata is matrix with returns, may include NAs, column names are stock names
fit_capmgarch_midas <- function(df, gamma = TRUE) {
  
  df_macro <- 
    alfred::get_alfred_series("HOUST", "hous", observation_start = "1986-12-01", 
                              observation_end = max(df$date), 
                              realtime_start = max(floor_date(df$date, "months")) - months(3),
                              realtime_end = max(df$date)) %>% 
    rename(year_month = date) %>%
    as_tibble() %>%
    filter(realtime_period == max(realtime_period)) %>%
    mutate(dhousing = log(hous) - log(lag(hous, order_by = year_month)))
  
  df_midas_housing <-
    readRDS("data/df_spx_daily_extended.rds") %>%
    filter(date >= "1987-01-01") %>%
    left_join(df_vix) %>%
    mutate(year_month = floor_date(date, "months")) %>%
    left_join(df_macro) %>%
    filter(date <= max(df$date)) %>%
    fill(dhousing) %>%
    filter(year_month >= floor_date(max(df$date), "months") - months(15 * 12 - 1)) %>%
    left_join(df_term_spread)
  
  df_midas_vix <- 
    df_midas_housing %>%
    filter(year_month >= floor_date(max(df$date), "months") - months(12 * 12 - 1))
  
  mfgarch_housing <- fit_mfgarch(distinct(select(df_midas_housing, date, spx_ret_close_close, vix_sqrt_252, dhousing, year_month)), 
              y = "spx_ret_close_close", x = "dhousing", weighting = "beta.unrestricted", K = 36, low.freq = "year_month", multi.start = FALSE)
  
  mfgarch_vix <- fit_mfgarch(select(df_midas_vix, date, spx_ret_close_close, vix_sqrt_252, dhousing, year_month) %>% filter(is.na(vix_sqrt_252) == FALSE), 
                      y = "spx_ret_close_close", x = "vix_sqrt_252", weighting = "beta.restricted", K = 3, low.freq = "date", multi.start = FALSE)
  
  mfgarch_term_spread <- fit_mfgarch(distinct(select(df_midas_housing, date, spx_ret_close_close, term_spread, year_month)), 
                                 y = "spx_ret_close_close", x = "term_spread", weighting = "beta.unrestricted", K = 36, low.freq = "year_month", multi.start = FALSE)
  
  df %>%
    select(date, symbol, ret_close_close) %>%
    left_join(select(readRDS("data/df_fama_french_daily.rds"), date, rf_log), by = "date") %>%
    left_join(readRDS("data/df_spx_daily_extended.rds"), by = "date") %>%
    split(.$symbol) %>%
    lapply(., function(x) filter(x, is.na(ret_close_close) == FALSE)) %>%
    lapply(., function(x) list(beta.reg = lm(data = x, (ret_close_close - rf_log) ~ (spx_ret_close_close - rf_log)))) %>%
    lapply(., function(x) tibble(model = c("capmgarch_housing", "capmgarch_term_spread", "capmgarch_vix"),
                                 forecast = c(x$beta.reg$coefficients[2]^2 * sum(predict(mfgarch_housing, n.ahead = 22)) + fit_panelgarch(cbind(spxres = x$beta.reg$residuals), gamma = FALSE)$forecast,
                                              x$beta.reg$coefficients[2]^2 * sum(predict(mfgarch_term_spread, n.ahead = 22)) + fit_panelgarch(cbind(spxres = x$beta.reg$residuals), gamma = FALSE)$forecast,
                                              x$beta.reg$coefficients[2]^2 * sum(predict(mfgarch_vix, n.ahead = 22)) + fit_panelgarch(cbind(spxres = x$beta.reg$residuals), gamma = FALSE)$forecast))) %>%
    bind_rows(.id = "symbol")
  
}
 