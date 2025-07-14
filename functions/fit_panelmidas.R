


calculate_phi <- function(w1, w2, K) {
  
  calculate_phi_individual_weights <- function(j) {
    (j / (K + 1))^(w1 - 1) * (1 - j / (K + 1))^(w2 - 1)
  }
  
  weights <- sapply(c(1:K),
                    FUN = calculate_phi_individual_weights)
  weights <- weights/sum(weights)
  weights
}

fit_panelmidas <- function(rvdata, trace = FALSE, forecastdata = NULL) {
  
  left_lead <- 
    lapply(seq_len(ncol(rvdata)), function(i) rvdata[,i]) %>%
    lapply(., function(x) na.omit(x)) %>%
    lapply(., function(x) lead(data.table::frollapply(x - mean(x), n = 22, FUN = function(x) mean(x), fill = NA, align = "left")))
  
  # browser()
  
  lf <- function(p) {
    if (p[2] <= 1 | p[2] > 1500) { # p[2] > 1500 makes numerical problems in weighting scheme (and makes no sense)
      NA
    }
    se_loss <- 0
    estimated_weights <- rev(calculate_phi(1,p[2],132))
    
    for (jj in ncol(rvdata)) {
      
      rvstock <- na.omit(rvdata[,jj])
      
      right <- data.table::frollapply(rvstock - mean(rvstock), n = 132, FUN = function(x) sum(x * estimated_weights), fill = NA, align = "right")
      
      se_loss <-
        se_loss + sum((left_lead[[jj]] - p[1] * right)^2, na.rm = TRUE)
    }
    se_loss
  }
  
  p.e.nlminb <- try({optim(c(0.4,6), f = lf, hessian = FALSE, method = "L-BFGS-B", lower = c(-Inf, 1))})
  
  if (class(p.e.nlminb) == "try-error") {
    par <- c(0.4, 1) 
    print("here")
  } else {
    if (p.e.nlminb$value == 0) {
      p.e.nlminb <- try({optim(c(0.4,6), f = lf, hessian = FALSE)})
    }
    par <- p.e.nlminb$par
  }
  
  if (!is.null(forecastdata)) {
    rvdata <- forecastdata
  }
  
  forecasts <- rep(NA, times = ncol(rvdata)); 
  for (jj in 1:ncol(rvdata)) {
    forecasts[jj] <- 22 * (mean(rvdata[,jj], na.rm = TRUE) + par[1] * sum(rev(calculate_phi(1, par[2], 132)) * tail(na.omit(rvdata[,jj]) - mean(rvdata[,jj], na.rm = TRUE),132)))
  }
  
  tibble(symbol = colnames(rvdata),
         forecast = forecasts)
  
}

