
source("functions/fit_panelgarch.R")

# rdata is matrix with returns, may include NAs, column names are stock names
fit_capmgarch <- function(df, gamma = TRUE) {
  
  # df_spx <- readxl::read_xls("data/dailypricehistory_reduced_for_loading.xls") %>%
  #   transmute(date = as_date(date), sptr = SPTR) %>%
  #   mutate(sptr = (log(sptr) - log(lag(sptr, by = "date"))) * 100) %>%
  #   right_join(df, by = "date") %>%
  #   left_join(select(readRDS("data/df_fama_french_daily.rds"), date, rf_log), by = "date")
  
  # matrix_spx <- as.matrix(distinct(select(df_spx, date, sptr))$sptr)
  
  df_spx <- readRDS("data/df_spx_daily_extended.rds") %>%
    right_join(df, by = "date") %>%
    left_join(select(readRDS("data/df_fama_french_daily.rds"), date, rf_log), by = "date")
  
  matrix_spx <- as.matrix(na.omit(distinct(select(df_spx, date, spx_ret_close_close))$spx_ret_close_close))
  
  colnames(matrix_spx) <- "spx"
  
  spx_fcst <- fit_panelgarch(matrix_spx)$forecast
  
  df_spx %>%
    split(.$symbol) %>%
    lapply(., function(x) filter(x, is.na(ret_close_close) == FALSE)) %>%
    lapply(., function(x) list(beta.reg = lm(data = x, (ret_close_close - rf_log) ~ (spx_ret_close_close - rf_log)))) %>%
    lapply(., function(x) tibble(forecast = x$beta.reg$coefficients[2]^2 * spx_fcst + fit_panelgarch(cbind(spxres = x$beta.reg$residuals), gamma = FALSE)$forecast)) %>%
    bind_rows(.id = "symbol")
  # %>%
  #   right_join(df)
  
}
  # 
  # llh_simple <- function(ret, alpha, beta, gamma) {
  #   omega <- (1 - alpha - beta - gamma / 2) * var(ret)
  #   
  #   g <- calculate_g(omega = omega, alpha = alpha, beta = beta, gamma = gamma,
  #                    returns = ret, g0 = var(ret))
  #   1/2 * log(2 * pi) + 1/2 * log(g) + 1/2 * ret^2/ g
  # }
  # 
  # forecast_garch <- function(omega, alpha, beta, gamma, g, ret, steps.ahead) {
  #   omega / (1 - alpha - gamma/2 - beta) + (alpha + beta + gamma/2)^(steps.ahead - 1) * (omega + (alpha + gamma/2 * as.numeric(ret < 0)) * ret^2 + beta * g - omega / (1 - alpha - gamma/2 - beta))
  # }
  # 
  # rdatalr <- colMeans(rdata)
  # 
  # if (gamma == TRUE) {
  #   # Parameter estimation
  #   lf <- function(p) {
  #     llh_joint = 0
  #     for (jj in ncol(rdata)) {
  #       llh_joint <-
  #         llh_joint + 
  #         sum(llh_simple(ret = na.omit(rdata[,jj]) - mean(na.omit(rdata[,jj])),
  #                        alpha = p["alpha"],
  #                        beta = p["beta"],
  #                        gamma = p["gamma"]))
  #     }
  #     llh_joint
  #   }
  #   par.start <- c(alpha = 0.05, beta = 0.85, gamma = 0.04)
  #   ui.opt <- rbind(c(-1, -1, -1/2), c(1, 0, 0), c(0, 1, 0))
  #   ci.opt <- c(-0.99999, 0, 0)
  # } else {
  #   # Parameter estimation
  #   lf <- function(p) {
  #     llh_joint = 0
  #     for (jj in ncol(rdata)) {
  #       llh_joint <-
  #         llh_joint + 
  #         sum(llh_simple(ret = na.omit(rdata[,jj]) - mean(na.omit(rdata[,jj])),
  #                        alpha = p["alpha"],
  #                        beta = p["beta"],
  #                        gamma = 0))
  #     }
  #     llh_joint
  #   }
  #   par.start <- c(alpha = 0.05, beta = 0.85)
  #   ui.opt <- rbind(c(-1, -1), c(1, 0), c(0, 1))
  #   ci.opt <- c(-0.99999, 0, 0)
  # }
  # 
  # 
  # p.e.nlminb <- constrOptim(theta = par.start,
  #                           f = lf,
  #                           grad = NULL, ui = ui.opt, ci = ci.opt, hessian = FALSE)
  # 
  # p.e.nlminb2 <- try(optim(par = par.start, fn = lf, method = "BFGS"), silent = TRUE)
  #  
  # if (class(p.e.nlminb2) != "try-error") {
  #   
  #   if (p.e.nlminb2$value < p.e.nlminb$value && sum(ui.opt %*% p.e.nlminb2$par - ci.opt >= 0) == 3) {
  #     par <- p.e.nlminb2$par
  #   } else {
  #     par <- p.e.nlminb$par
  #   }
  # } else {
  #   par <- p.e.nlminb$par
  # }
  # 
  # print(par)
  # 
  # forecasts <- rep(NA, times = ncol(rdata)); 
  # for (jj in 1:ncol(rdata)) {
  #   forecasts[jj] <- 0
  #   
  #   for (kk in 1:22) {
  #     if (gamma == TRUE) {
  #       forecasts[jj] <- forecasts[jj] +
  #         forecast_garch(omega = (1 - par[1] - par[2] - par[3] / 2) * var(na.omit(rdata[,jj])), 
  #                        alpha = par[1], beta = par[2], gamma = par[3],
  #                        g = last(calculate_g(omega = (1 - par[1] - par[2] - par[3] / 2) * var(na.omit(rdata[,jj])), 
  #                                             alpha = par[1], beta = par[2], gamma = par[3],
  #                                             returns = na.omit(rdata[,jj]), g0 = var(na.omit(rdata[,jj])))),
  #                        ret = last(rdata[,jj]),
  #                        steps.ahead = kk)
  #     } else {
  #       forecasts[jj] <- forecasts[jj] +
  #         forecast_garch(omega = (1 - par[1] - par[2] - 0 / 2) * var(na.omit(rdata[,jj])), 
  #                        alpha = par[1], beta = par[2], gamma = 0,
  #                        g = last(calculate_g(omega = (1 - par[1] - par[2] - 0 / 2) * var(na.omit(rdata[,jj])), 
  #                                             alpha = par[1], beta = par[2], gamma = 0,
  #                                             returns = na.omit(rdata[,jj]), g0 = var(na.omit(rdata[,jj])))),
  #                        ret = last(rdata[,jj]),
  #                        steps.ahead = kk)
  #     }
  #     
  #   }
  #   
  # }
  # 
  # tibble(symbol = colnames(rdata),
  #        forecast = forecasts)
  # 
  # optim(par.start, lf, method = "BFGS")
  # 
  # if (multi.start == TRUE && gamma == TRUE) {
  #   p.e.nlminb.two <- try({
  #     suppressWarnings(optim(par = p.e.nlminb$par, fn = function (theta) {
  #       if( is.na(sum(lf(theta))) == TRUE) {
  #         NA
  #       } else {
  #         sum(lf(theta))
  #       }
  #     }, method = "BFGS"))}, silent = TRUE)
  #   
  #   if (class(p.e.nlminb.two) == "try-error") {
  #     print("Second-step BFGS optimization failed. Fallback: First-stage Nelder-Mead estimate.")
  #   } else {
  #     if (p.e.nlminb.two$value < p.e.nlminb$value) {
  #       p.e.nlminb <- p.e.nlminb.two
  #     }
  #   }
  # }
  # 
  # p.e.nlminb$value <- -p.e.nlminb$value
  # 
  # par <- p.e.nlminb$par
  # returns <- as.numeric(unlist(data[[y]]))
  # tau <- rep(exp(par["m"]), times = length(returns))
  # 
  # g <- c(rep(NA, times = sum(is.na((returns - par["mu"])/sqrt(tau)))),
  #        calculate_g(omega = 1 - par["alpha"] - par["beta"] -  par["gamma"]/2,
  #                    alpha = par["alpha"],
  #                    beta = par["beta"],
  #                    gamma = par["gamma"],
  #                    as.numeric(na.exclude((returns - par["mu"])/sqrt(tau))),
  #                    g0 = g_zero))
  # tau <- rep(exp(par["m"]), times = length(g))
  # 
  # if ((var.ratio.freq %in% c("date", "low.freq")) == FALSE) {
  #   df.fitted <- cbind(data[c("date", y, var.ratio.freq)], g = g, tau = tau)
  # } else {
  #   df.fitted <- cbind(data[c("date", y)], g = g, tau = tau)
  # }
  # df.fitted$residuals <- unlist((df.fitted[y] - par["mu"]) / sqrt(df.fitted$g * df.fitted$tau))
  # 
  # covariate <- unlist(unique(data[c(low.freq, x)])[x])
  # 
  # if (is.null(x.two) == FALSE) {
  #   covariate.two <- unlist(unique(data[c(low.freq.two, x.two)])[x.two])
  # }
  # 
  # # Output -----------------------------------------------------------------------------------------
  # output <-
  #   list(par = par,
  #        std.err = rob.std.err,
  #        broom.mgarch = data.frame(term = names(par),
  #                                  estimate = par,
  #                                  rob.std.err = rob.std.err,
  #                                  p.value = 2 * (1 - pnorm(unlist(abs(par/rob.std.err)))),
  #                                  opg.std.err = opg.std.err,
  #                                  opg.p.value = 2 * (1 - pnorm(unlist(abs(par/opg.std.err))))),
  #        tau = tau,
  #        g = g,
  #        df.fitted = df.fitted,
  #        K = K,
  #        weighting.scheme = weighting,
  #        llh = p.e.nlminb$value,
  #        bic = log(sum(!is.na(tau))) * length(par) - 2 * (p.e.nlminb$value),
  #        y = y,
  #        optim = p.e.nlminb)
  # 
  # # Add class mfGARCH for employing generic functions
  # class(output) <- "panelGARCH"
  # output
# }
