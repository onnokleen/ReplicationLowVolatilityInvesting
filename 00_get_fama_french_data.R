
# ------------------------------------------------------------------------------
# Script: 00_get_fama_french_data.R
# Purpose: 
#   - Download and process Fama-French factor data (daily and monthly).
#   - Prepare and save cleaned datasets for use in subsequent analysis.
#
# Inputs:
#   - Fama-French CSV files (downloaded from Ken French's data library)
#   - Momentum data (assumed to be pre-loaded or available in workspace)
#
# Outputs:
#   - Cleaned and processed Fama-French factor datasets as .rds files
#
# Author: Onno Kleen
# ------------------------------------------------------------------------------

library(foreach)
library(doParallel)
library(zoo)
library(dplyr)
library(readr)
library(tidyr)
library(broom)
library(purrr)
library(lubridate)

download.file("http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_5_Factors_2x3_daily_CSV.zip",
              "data/fama_french_2x3.csv.zip")

download.file("http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_5_Factors_2x3_CSV.zip",
              "data/fama_french_2x3_monthly.csv.zip")


df_fama_french_monthly <-
  read_csv("data/F-F_Research_Data_5_Factors_2x3.csv",
           skip = 4,
           col_names = c("date", "mkt_rf", "smb", "hml", "rnw", "cma", "rf")) %>%
  mutate(date = as_date(paste(substr(date, 1, 4), substr(date, 5, 6), "01", sep = "-"))) %>%
  rename(year_month = date) %>%
  left_join(df_momentum_monthly) %>%
  mutate(mkt_rf_log = 100 * log(1 + mkt_rf / 100),
         smb_log = 100 * log(1 + smb / 100),
         hml_log = 100 * log(1 + hml / 100),
         mom_log = 100 * log(1 + mom / 100),
         rnw_log = 100 * log(1 + rnw / 100),
         cma_log = 100 * log(1 + cma / 100),
         rf_log = 100 * log(1 + rf / 100)) %>%
  select(year_month, mkt_rf, smb, hml, mom, rnw, cma, rf, mkt_rf_log, smb_log, hml_log, mom_log, rnw_log, cma_log, rf_log) 

saveRDS(df_fama_french_monthly, "data/df_fama_french_monthly.rds")

df_fama_french_daily <-
  read_csv("data/fama_french_2x3.csv.zip",
           skip = 4,
           col_names = c("date", "mkt_rf", "smb", "hml", "rnw", "cma", "rf")) %>%
  mutate(date = as_date(paste(substr(date, 1, 4), substr(date, 5, 6), substr(date, 7, 8), sep = "-"))) %>%
  left_join(df_momentum_daily) %>%
  na.omit() %>%
  mutate(mkt_rf_log = 100 * log(1 + mkt_rf / 100),
         smb_log = 100 * log(1 + smb / 100),
         hml_log = 100 * log(1 + hml / 100),
         mom_log = 100 * log(1 + mom / 100),
         rnw_log = 100 * log(1 + rnw / 100),
         cma_log = 100 * log(1 + cma / 100),
         rf_log = 100 * log(1 + rf / 100)) %>%
  select(date, mkt_rf, smb, hml, mom, rnw, cma, rf, mkt_rf_log, smb_log, hml_log, mom_log, rnw_log, cma_log, rf_log)
saveRDS(df_fama_french_daily, "data/df_fama_french_daily.rds")
