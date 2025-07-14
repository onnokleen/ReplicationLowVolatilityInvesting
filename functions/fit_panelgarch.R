
Rcpp::sourceCpp("functions/calculate_g.cpp")

# rdata is matrix with returns, may include NAs, column names are stock names
fit_panelgarch <- function(rdata, gamma = TRUE, trace = FALSE, forecastdata = NULL) {
  
  llh_simple <- function(ret, alpha, beta, gamma) {
    omega <- (1 - alpha - beta - gamma / 2) * var(ret)
    
    if (omega < 0 || alpha < 0 || beta < 0 || alpha + beta + gamma / 2 >= 1 || gamma < -0.01) {
      return(NA)
    }
    
    g <- calculate_g(omega = omega, alpha = alpha, beta = beta, gamma = gamma,
                     returns = ret, g0 = var(ret))
    
    1/2 * log(2 * pi) + 1/2 * log(g) + 1/2 * ret^2/ g
  }
  
  forecast_garch <- function(omega, alpha, beta, gamma, g, ret, steps.ahead) {
    omega / (1 - alpha - gamma/2 - beta) + (alpha + beta + gamma/2)^(steps.ahead - 1) * (omega + (alpha + gamma/2 * as.numeric(ret < 0)) * ret^2 + beta * g - omega / (1 - alpha - gamma/2 - beta))
  }
  
  rdatalr <- colMeans(rdata)
  
  if (gamma == TRUE) {
    # Parameter estimation
    lf <- function(p) {
      llh_joint = 0
      for (jj in ncol(rdata)) {
        llh_joint <-
          llh_joint + 
          sum(llh_simple(ret = na.omit(rdata[,jj]) - mean(na.omit(rdata[,jj])),
                         alpha = p["alpha"],
                         beta = p["beta"],
                         gamma = p["gamma"]))
      }
      llh_joint
    }
    par.start <- c(alpha = 0.05, beta = 0.85, gamma = 0.04)
    ui.opt <- rbind(c(-1, -1, -1/2), c(1, 0, 0), c(0, 1, 0), c(0, 0, 1), c(0, -1, 0))
    ci.opt <- c(-0.99999, 0, 0,0,-0.99999)
  } else {
    # Parameter estimation
    lf <- function(p) {
      llh_joint = 0
      for (jj in ncol(rdata)) {
        llh_joint <-
          llh_joint + 
          sum(llh_simple(ret = na.omit(rdata[,jj]) - mean(na.omit(rdata[,jj])),
                         alpha = p["alpha"],
                         beta = p["beta"],
                         gamma = 0))
      }
      llh_joint
    }
    par.start <- c(alpha = 0.05, beta = 0.85)
    ui.opt <- rbind(c(-1, -1), c(1, 0), c(0, 1))
    ci.opt <- c(-0.99999, 0, 0)
  }
  
  
  p.e.nlminb <- constrOptim(theta = par.start,
                            f = lf,
                            grad = NULL, ui = ui.opt, ci = ci.opt, hessian = FALSE)
  
  p.e.nlminb2 <- try(optim(par = par.start, fn = lf, method = "BFGS"), silent = TRUE)
   
  if (class(p.e.nlminb2) != "try-error") {
    
    if (p.e.nlminb2$value < p.e.nlminb$value && sum(ui.opt %*% p.e.nlminb2$par - ci.opt >= 0) == 3) {
      par <- p.e.nlminb2$par
    } else {
      par <- p.e.nlminb$par
    }
  } else {
    par <- p.e.nlminb$par
  }
  
  if (trace == TRUE) {
    print(par)
  }
  
  if (!is.null(forecastdata)) {
    rdata <- forecastdata
  }
  
  forecasts <- rep(NA, times = ncol(rdata)); 
  for (jj in 1:ncol(rdata)) {
    forecasts[jj] <- 0
    
    for (kk in 1:22) {
      if (gamma == TRUE) {
        forecasts[jj] <- forecasts[jj] +
          forecast_garch(omega = (1 - par[1] - par[2] - par[3] / 2) * var(na.omit(rdata[,jj])), 
                         alpha = par[1], beta = par[2], gamma = par[3],
                         g = last(calculate_g(omega = (1 - par[1] - par[2] - par[3] / 2) * var(na.omit(rdata[,jj])), 
                                              alpha = par[1], beta = par[2], gamma = par[3],
                                              returns = na.omit(rdata[,jj]), g0 = var(na.omit(rdata[,jj])))),
                         ret = last(rdata[,jj]),
                         steps.ahead = kk)
      } else {
        forecasts[jj] <- forecasts[jj] +
          forecast_garch(omega = (1 - par[1] - par[2] - 0 / 2) * var(na.omit(rdata[,jj])), 
                         alpha = par[1], beta = par[2], gamma = 0,
                         g = last(calculate_g(omega = (1 - par[1] - par[2] - 0 / 2) * var(na.omit(rdata[,jj])), 
                                              alpha = par[1], beta = par[2], gamma = 0,
                                              returns = na.omit(rdata[,jj]), g0 = var(na.omit(rdata[,jj])))),
                         ret = last(rdata[,jj]),
                         steps.ahead = kk)
      }
      
    }
    
  }
  
  tibble(symbol = colnames(rdata),
         forecast = forecasts)
}
