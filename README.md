# Replication files for the paper "Volatility forecasting for low-volatility investing"
# Author: Onno Kleen
# 14.07.2025

### General remarks

The replication files are provided in a chronological order

-   00_get_fama_fernch_data.R 
    -   Downloads the Fama-French data from Keneth French's website and saves it as an RDS file.
-   01_model_fitting.R
    -   Fits all time series models and saves the forecasts as an RDS file.
-   03_generetae_loss_based_forecasts_per_stock_quintiles.R
    -   Generates the loss-based forecasts per stock quintiles and saves them as an RDS file.
-   04_oracle_portfolio_deciles_scaled.R
    -   Generates tables and figures around the post-hoc portfolio
-   05_evaluate_losses.R
    -   Evaluates the losses of the forecasts and generates tables and figures.
-   06_make_acf_figure.R
    -   Generates the figure with the ACF of the realized variances
-   07_generate_returns_and_vol_summaries.R
    -   Generates the portfolio summaries with table generation.

The scripts puts the figures of the paper into the folder 'Figures' and the 
tables into the folder 'Tables'. The folders 'models' and 'portfolio_sorts' are
used to store intermediate results. The folder functions contains additional subroutines.
The script '_load_packages.R' is a convenience script to load all packages needed for the analysis.

### Data sets needed

The different datasets needed can be found here:

-   data/allmonth.xls
    -   Needs a "Date" column and a "SREAD" column
    -   Can be downloaded here: <https://www.newyorkfed.org/medialibrary/media/research/capital_markets/allmonth.xls>
-   data/Low_vola_indices.xlsx (proprietary, downloaded via Bloomberg terminal)
    -   Needs a "Dates" column and a column "SP5LVTUT Index" which holds the values for the low SP5LVTUT volatility index.
-   data/crsp_monthly.csv
    -   A file with the CRSP monthly data. The columns requested from CRSP via WRDS should be the following: PERMNO, date, SHRCD, EXCHCD, SICCD, COMNAM, NAICS, PERMCO, DLAMT, DLSTCD, DLPRC, DLRET, PRC, RET, SHROUT, ALTPRC
-   data/Costs_filled.csv
    -   A file with costs per stock with the following column names: permno, year, kSample, c, firstdate, lastdate, yearmonth, match, c_match, c_new
-   data/df_har_end_2021_scaled_increasing_window_ss.rds (proprietary)
    -   A file with data underlying the estimation of the HAR models and also the other time series models. 
    -   The columns are as follows:
    -   date 
    -   symbol (should be the CRSP PERMNO) 
    -   n_intraday (number of intraday transactions)
    -   close (close price from CRSP)
    -   ret_close_close
    -   year_month (year-month indicator )
    -   rv_sq_overnight (daily realized variance (RV) scaled such that their scale encompasses the squared overnight return)
    -   lead_rv_22_sq_overnight (dependent variable for HAR models)
    -   rv_5_sq_overnight (average 5-day RV)
    -   rv_22_sq_overnight (average 22-day RV)
    -   rv_66_sq_overnight (average 66-day RV)
    -   rv_132_sq_overnight (average 132-day RV)
    -   vix (VIX index, can be obtained from Cboe website, see data/VIX_history.csv below)
    -   mkt_rf_log (market excess return, also in df_fama_french_daily.rds)
    -   rf_log (log risk free rate, also in df_fama_french_daily.rds)
-   data/df_fama_french_daily.rds
    -  Can be generated from the script 00_get_fama_french_data.R
-   data/df_fama_french_monthly.rds
    -  Can be generated from the script 00_get_fama_french_data.R
-   data/VIX_history.csv
    -  Can be downloaded from https://cdn.cboe.com/api/global/us_indices/daily_prices/VIX_History.csv

As the financial data is mainly proprietary, we include the monthly forecasts in the
"models" folder. The forecasts are generated from the script 01_model_fitting.R 

### Software 

> sessionInfo()
R version 4.4.1 (2024-06-14)
Platform: aarch64-apple-darwin20
Running under: macOS 15.5

Matrix products: default
BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib 
LAPACK: /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.0

Random number generation:
 RNG:     Mersenne-Twister 
 Normal:  Inversion 
 Sample:  Rounding 
 
locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

time zone: Europe/Amsterdam
tzcode source: internal

attached base packages:
[1] parallel  grid      stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] forecast_8.23.0   rugarch_1.5-3     tikzDevice_0.12.6 mfGARCH_0.2.1     purrr_1.0.4       broom_1.0.7       lmtest_0.9-40     sandwich_3.1-1    doParallel_1.0.17
[10] iterators_1.0.14  xtable_1.8-4      gridExtra_2.3     foreach_1.5.2     lubridate_1.9.3   zoo_1.8-12        ggthemes_5.1.0    ggplot2_3.5.1     tidyr_1.3.1      
[19] readr_2.1.5       scales_1.3.0      dplyr_1.1.4      

loaded via a namespace (and not attached):
 [1] tidyselect_1.2.1            filehash_2.4-6              timeDate_4041.110           farver_2.1.2                pracma_2.4.4                GeneralizedHyperbolic_0.8-6
 [7] digest_0.6.37               PeerPerformance_2.3.1       timechange_0.3.0            lifecycle_1.0.4             Rsolnp_1.16                 magrittr_2.0.3             
[13] compiler_4.4.1              rlang_1.1.5                 tools_4.4.1                 utf8_1.2.4                  data.table_1.17.0           labeling_0.4.3             
[19] bit_4.5.0                   mclust_6.1.1                curl_6.0.1                  RColorBrewer_1.1-3          TTR_0.24.4                  KernSmooth_2.23-24         
[25] withr_3.0.2                 numDeriv_2016.8-1.1         nnet_7.3-19                 xts_0.14.1                  colorspace_2.1-1            MASS_7.3-61                
[31] cli_3.6.4                   mvtnorm_1.3-2               crayon_1.5.3                PerformanceAnalytics_2.0.4  miscTools_0.6-28            generics_0.1.3             
[37] rstudioapi_0.17.1           httr_1.4.7                  tzdb_0.4.0                  readxl_1.4.3                alfred_0.2.1                stringr_1.5.1              
[43] urca_1.3-4                  cellranger_1.1.0            vctrs_0.6.5                 spd_2.0-1                   Matrix_1.7-1                jsonlite_1.8.9             
[49] hms_1.1.3                   tseries_0.10-58             bit64_4.5.2                 quantmod_0.4.26             glue_1.8.0                  nloptr_2.1.1               
[55] SkewHyperbolic_0.4-2        codetools_0.2-20            DistributionUtils_0.6-1     stringi_1.8.4               gtable_0.3.6                quadprog_1.5-8             
[61] munsell_0.5.1               tibble_3.2.1                pillar_1.10.1               truncnorm_1.0-9             R6_2.6.1                    maxLik_1.5-2.1             
[67] ks_1.14.3                   vroom_1.6.5                 lattice_0.22-6              moments_0.14.1              backports_1.5.0             fracdiff_1.5-3             
[73] Rcpp_1.0.13-1               nlme_3.1-166                pkgconfig_2.0.3  

### Hardware and running time

On a laptop with Apple Silicon M3 Pro, the code takes around 17 hours to run with only one core. 
The main bottleneck in terms of computing time is the model fitting step, which 
takes around 15 hours if it is not parallelized. 
Parallelization can be achieved via the `doParallel` package, which can be used in 
the outer foreach loop in 01_model_fitting.R