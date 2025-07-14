# Share legend of multiple plots
grid_arrange_shared_legend <- function(..., nrow = 1, ncol = length(list(...)), position = c("bottom", "right"), which.legend = 1) {
  
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[which.legend]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position = "none"))
  gl <- c(gl, nrow = nrow, ncol = ncol)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  grid.newpage()
  grid.draw(combined)
  
}

identify_portfolio_quintile_fast <- function(x) {
  # Compute quantiles directly using quantile() instead of manual sorting/splitting
  quantiles <- quantile(x, probs = seq(0.2, 1, by = 0.2), na.rm = TRUE)
  
  # Define quintile labels
  quintile_labels <- c("quintile_00_02", "quintile_02_04", "quintile_04_06", 
                       "quintile_06_08", "quintile_08_10")
  
  # Use cut() for vectorized classification
  cut(x, breaks = c(-Inf, quantiles), labels = quintile_labels, include.lowest = TRUE, right = TRUE)
}

identify_portfolio_quintile <- function(x) {
  quantiles <- unlist(lapply(split(sort(x), cut(seq_along(sort(x)), 5, labels = FALSE)), max))
  
  ifelse(x <= quantiles[1], "quintile_00_02",
         ifelse(quantiles[1] < x & x <= quantiles[2], "quintile_02_04",
                ifelse(quantiles[2] < x & x <= quantiles[3], "quintile_04_06", 
                       ifelse(quantiles[3] < x & x <= quantiles[4], "quintile_06_08", "quintile_08_10"))))
}



regularize_weights <- function(weights, max.weight) {
  weights <- weights / sum(weights)
  
  while (any(weights > max.weight)) {
    weights[weights == max(weights)] <- max.weight
    weights[weights != max.weight] <- weights[weights != max.weight] / sum(weights[weights != max.weight]) * (1 - length(weights[weights == max.weight]) * max.weight)
  }
  weights
}

level_models <-
  c("last_year_sq_ret",
    "last_four_years_sq_ret", 
    "last_six_months_sq_ret",
    "last_month_sq_ret",
    "riskmetrics_monthly_12_months",
    "riskmetrics_monthly_6_months",
    "riskmetrics_daily_12_months",
    "riskmetrics_daily_6_months",
    "gjrgarch",
    "panelgarch",
    "capmgarch",
    "capmgarch_vix",
    "capmgarch_housing",
    "capmgarch_term_spread",
    "memgarch",
    "panelmemgarch",
    "last_month_rv",
    "har",
    "har_long_term",
    "har_spx",
    "har_long_term_spx",
    "har_vix",
    "har_vix_long_term",
    "panel_har",
    "panel_har_long_term",
    "log_har",
    "midas",
    "panelmidas")

label_models <-
  c("12m-$\\text{RV}^d$",
    "4y-$\\text{RV}^d$",
    "6m-$\\text{RV}^d$",
    "1m-$\\text{RV}^d$",
    "RM monthly, 12 months",
    "RM monthly, 6 months",
    "RM daily, 12 months",
    "RM daily, 6 months",
    "GJR-GARCH",
    "Panel GJR-GARCH",
    "Factor GARCH",
    "Factor GARCH-VIX",
    "Factor GARCH-$\\Delta$Hous",
    "Factor GARCH-TS",
    "MEM",
    "Panel MEM",
    "R-HAR",
    "HAR",
    "HAR-LR",
    "HAR-SPX",
    "HAR-SPX-LR",
    "HAR-VIX",
    "HAR-VIX-LR",
    "Panel HAR",
    "Panel HAR-LR",
    "Log-HAR",
    "MIDAS",
    "Panel MIDAS")



utility_fee <- function(x, y, gamma, sum = TRUE) {
  
  if (length(x) != length(y)) {
    stop("returns x and y have to be of same length")
  }
  
  # total number of months
  M <- length(x)
  
  X <- 1 + x / 100
  Y <- 1 + y / 100
  
  gamma_tilde <- gamma / (2 * (1 + gamma))
  
  if (sum == TRUE) {
    A <- M - 2 * gamma_tilde * sum(Y)
    B <- sum(X) - sum(Y) - gamma_tilde * sum(X^2) + gamma_tilde * sum(Y^2)# sum(x) - sum(y) + gamma_tilde * sum((1 + y)^2)
    
    p <- A / (gamma_tilde * M)
    q <- B / (gamma_tilde * M)
    
    abs(-p/2 + sqrt((p/2)^2 - q))
    
  } else {
    A <- M - 2 * gamma_tilde * Y
    B <- X - Y - gamma_tilde * X^2 + gamma_tilde * Y^2# sum(x) - sum(y) + gamma_tilde * sum((1 + y)^2)
    
    p <- A / (gamma_tilde * M)
    q <- B / (gamma_tilde * M)
    
    -p/2 + sqrt((p/2)^2 - q)
  }
  
}


label_losses <-
  c("SE", "QLIKE")
level_losses <-
  c("se", "qlike")

p_value_t_test <- function(x) {
  
  if (all(x == 0) || length(x) == 1) {
    NA
  } else {
    coeftest(lm(x ~ 1), vcov = NeweyWest(lm(x ~ 1), lag = 6))[1,4]
  }
}


riskmetrics_weight <- function(lambda, K) {
  if (lambda == 1) {
    rep(1/K, times = K)
  } else {
    unweighted <- sapply(c(0:(K-1)), function(k) (1-lambda) * lambda^k)
    unweighted / sum(unweighted)
  }
}

riskmetrics_weighted_timeseries <- function(returns, delta) {
  sum(returns * rev(riskmetrics_weight(delta, length(returns))))
}


paste_stars <- function(pvalue) {
  if (is.na(pvalue)) {
    return("")
  }
  if (pvalue < 0.01) {
    return("***")
  } 
  if (pvalue < 0.05) {
    return("**")
  } 
  if (pvalue < 0.1) {
    return("*")
  } 
  return("")
}
