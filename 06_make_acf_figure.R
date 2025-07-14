# ------------------------------------------------------------------------------
# Script: 06_make_acf_figure.R
# Purpose: 
#   - Compute and visualize the autocorrelation function (ACF) for key time series 
#     related to volatility forecasting (e.g., realized volatility, forecast errors, etc.).
#   - Generate figures for inclusion in research outputs, illustrating temporal 
#     dependencies in the data.
#
# Inputs:
#   - Realized volatility and/or forecast data (likely from .rds or .csv files)
#   - Helper functions and package loaders
#
# Outputs:
#   - Figures (e.g., ACF plots) saved as files (e.g., in Figures/ directory)
#
# Author: Onno Kleen
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 1. Load required packages, helper functions, and data
#    - Loads time series data for which ACF will be computed.
# ------------------------------------------------------------------------------

rm(list = ls())

source("_load_packages.R")
source("functions/helpers.R")


df_rv <- readRDS("data/df_har_end_2021_scaled_increasing_window_ss.rds")  %>%
  group_by(year_month, symbol) %>%
  summarise(rv_sq_overnight = mean(rv_sq_overnight, na.rm = TRUE) * 22) %>%
  ungroup()

calculate_acf <- function(df) {
  tibble(lag = c(1:12),
         acf = sapply(c(1:12), FUN = function(i) 
           AER::ivreg(lead(rv, i) ~ rv | lag_1 + lag_2 + lag_3 + lag_4 + lag_5 + lag_6,
                      data = df)$coefficients[2]))
  
}

df_acf <-
  df_rv %>%
  filter(year_month >= "2001-01-01",
         symbol %in% 
           unique(readRDS("portfolio_sorts/df_low_vol_portfolios_rv_scaled_as_dependent_end_2021_increasing_window_scaling_ss_old_filter.rds")$symbol)) %>%
  group_by(symbol) %>%
  mutate(n= n()) %>%
  ungroup() %>%
  filter(n == max(n)) %>%
  rename(rv = rv_sq_overnight) %>%
  select(year_month, symbol, rv) %>%
  group_by(symbol) %>%
  mutate(lag_1 = lag(rv, 1) - mean(rv),
         lag_2 = lag(rv, 2) - mean(rv),
         lag_3 = lag(rv, 3) - mean(rv),
         lag_4 = lag(rv, 4) - mean(rv),
         lag_5 = lag(rv, 5) - mean(rv),
         lag_6 = lag(rv, 6) - mean(rv),
         lag_7 = lag(rv, 7) - mean(rv),
         lag_8 = lag(rv, 8) - mean(rv),
         lag_9 = lag(rv, 9) - mean(rv),
         lag_10 = lag(rv, 10) - mean(rv)) %>%
  nest() %>%
  mutate(acf = map(data, calculate_acf), .keep = "none") %>%
  unnest(acf) %>%
  mutate(permno = factor(symbol)) %>%
  ungroup()         


tikz(file = paste0("Figures/acf_ss.tex"), standAlone = TRUE, width = 6, height = 4)
df_acf %>%
  bind_rows(df_acf %>% select(symbol) %>% distinct() %>% mutate(lag = 0, acf = 1)) %>%
  group_by(lag) %>%
  ggdist::median_qi(acf, .width = c(.50, .80, .95)) %>%
  mutate(.width = case_when(.width == 0.95 ~ "95\\%",
                            .width == 0.8 ~ "80\\%",
                            .width == 0.5 ~ "50\\%",
                            TRUE ~ as.character(NA))) %>%
  ggplot() +
  geom_ribbon(data = . %>% filter(.width == "95\\%"), aes(x = lag, y = acf, ymin = .lower, ymax = .upper, fill = .width)) + #, colour = "#DEEBF7", fill = "#DEEBF7") +
  geom_ribbon(data = . %>% filter(.width == "80\\%"), aes(x = lag, y = acf, ymin = .lower, ymax = .upper, fill = .width)) + #, , colour = "#9ECAE1", fill = "#9ECAE1") +
  geom_ribbon(data = .  %>% filter(.width == "50\\%"), aes(x = lag, y = acf, ymin = .lower, ymax = .upper, fill = .width)) + #, , colour = "#3182BD", fill = "#3182BD")
  scale_fill_manual(values = c("50\\%" = "#3182BD", "80\\%" = "#9ECAE1",  "95\\%" = "#DEEBF7")) +
  theme_minimal() +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 12), breaks = c(1:12)) +
  coord_cartesian(ylim = c(0,1), expand = 0) +
  ylab("Autocorrelation") +
  xlab("Order of lag") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        strip.text.x = element_text(size = 14),
        plot.title = element_text(hjust = 0.5),
        strip.text = element_text(colour = 'black'),
        # legend.key.width = unit(1.5, 'cm'),
        panel.spacing = unit(2, "lines"),
        strip.background = element_rect(fill = "white"),
        text = element_text(size = 15))
dev.off()
system(paste0("pdflatex -output-directory Figures/ acf_ss.tex"), wait = TRUE)

system("rm Figures/*.aux")
system("rm Figures/*.log")
system("rm Figures/*.tex")
