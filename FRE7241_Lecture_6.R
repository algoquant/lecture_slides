# Specify AR process parameters
n_rows <- 1e3
co_eff <- matrix(c(0.1, 0.39, 0.5)); n_coeff <- NROW(co_eff)
set.seed(1121); in_nov <- matrix(rnorm(n_rows))
# ari_ma <- filter(x=in_nov, filter=co_eff, method="recursive")
# Simulate AR process using HighFreq::sim_ar()
ari_ma <- HighFreq::sim_ar(coeff=co_eff, innov=in_nov)
# Fit AR model using ar.ols()
ar_fit <- ar.ols(ari_ma, order.max=n_coeff, aic=FALSE)
class(ar_fit)
is.list(ar_fit)
drop(ar_fit$ar); drop(co_eff)
# Define design matrix without intercept column
de_sign <- sapply(1:n_coeff, rutils::lag_it, in_put=ari_ma)
# Fit AR model using regression
design_inv <- MASS::ginv(de_sign)
coeff_fit <- drop(design_inv %*% ari_ma)
all.equal(drop(ar_fit$ar), coeff_fit, check.attributes=FALSE)
# Calculate the regression residuals
fit_ted <- drop(de_sign %*% coeff_fit)
residual_s <- drop(ari_ma - fit_ted)
# Variance of residuals
var_resid <- sum(residual_s^2)/(n_rows-NROW(coeff_fit))
# Design matrix squared
design_2 <- crossprod(de_sign)
# Calculate covariance matrix of AR coefficients
co_var <- var_resid*MASS::ginv(design_2)
coeff_fitd <- sqrt(diag(co_var))
# Calculate t-values of AR coefficients
coeff_tvals <- drop(coeff_fit)/coeff_fitd
# Fit AR(5) model into AR(3) process
de_sign <- sapply(1:5, rutils::lag_it, in_put=ari_ma)
design_inv <- MASS::ginv(de_sign)
coeff_fit <- drop(design_inv %*% ari_ma)
# Calculate t-values of AR(5) coefficients
residual_s <- drop(ari_ma - drop(de_sign %*% coeff_fit))
var_resid <- sum(residual_s^2)/(n_rows-NROW(coeff_fit))
co_var <- var_resid*MASS::ginv(crossprod(de_sign))
coeff_fitd <- sqrt(diag(co_var))
coeff_tvals <- drop(coeff_fit)/coeff_fitd
# Fit AR(5) model using arima()
arima_fit <- arima(ari_ma, order=c(5, 0, 0), include.mean=FALSE)
arima_fit$coef
# Fit AR(5) model using auto.arima()
library(forecast)  # Load forecast
arima_fit <- forecast::auto.arima(ari_ma, max.p=5, max.q=0, max.d=0)
# Fit AR(5) model into VTI returns
re_turns <- drop(zoo::coredata(na.omit(rutils::etfenv$re_turns$VTI)))
de_sign <- sapply(1:5, rutils::lag_it, in_put=re_turns)
design_inv <- MASS::ginv(de_sign)
coeff_fit <- drop(design_inv %*% re_turns)
# Calculate t-values of AR(5) coefficients
residual_s <- drop(re_turns - drop(de_sign %*% coeff_fit))
var_resid <- sum(residual_s^2)/(n_rows-NROW(coeff_fit))
co_var <- var_resid*MASS::ginv(crossprod(de_sign))
coeff_fitd <- sqrt(diag(co_var))
coeff_tvals <- drop(coeff_fit)/coeff_fitd
# Compute autocorrelation coefficients
ac_f <- acf(ari_ma, lag=10, plot=FALSE)
ac_f <- drop(ac_f$acf)
acf1 <- ac_f[-NROW(ac_f)]
# Define Yule-Walker matrix
yule_walker <- sapply(2:9, function(lagg) {
  c(acf1[lagg:1], acf1[2:(NROW(acf1)-lagg+1)])
})  # end sapply
yule_walker <- cbind(acf1, yule_walker, rev(acf1))
# Generalized inverse of Yule-Walker matrix
yule_walker_inv <- MASS::ginv(yule_walker)
# Solve Yule-Walker equations
coeff_yw <- drop(yule_walker_inv %*% ac_f[-1])
round(coeff_yw, 5)
coeff_fit
n_rows <- 1e2
co_eff <- c(0.1, 0.39, 0.5); n_coeff <- NROW(co_eff)
set.seed(1121); in_nov <- rnorm(n_rows)
# Simulate AR process using filter()
ari_ma <- filter(x=in_nov, filter=co_eff, method="recursive")
ari_ma <- as.numeric(ari_ma)
# Simulate AR process using C_rfilter()
arima_fast <- .Call(stats:::C_rfilter, in_nov, co_eff,
  double(n_rows + n_coeff))
all.equal(ari_ma, arima_fast[-(1:n_coeff)],
  check.attributes=FALSE)
# Forecast AR(3) process using loop in R
forecast_s <- numeric(NROW(ari_ma)+1)
forecast_s[1] <- 0
forecast_s[2] <- co_eff[1]*ari_ma[1]
forecast_s[3] <- co_eff[1]*ari_ma[2] + co_eff[2]*ari_ma[1]
for (it in 4:NROW(forecast_s)) {
  forecast_s[it] <- ari_ma[(it-1):(it-3)] %*% co_eff
}  # end for
# Plot with legend
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
plot(ari_ma, main="Forecasting Using AR(3) Model",
  xlab="", ylab="", type="l")
lines(forecast_s, col="orange", lwd=3)
legend(x="topright", legend=c("series", "forecasts"),
 col=c("black", "orange"), lty=1, lwd=6,
 cex=0.9, bg="white", bty="n")
# Forecast using filter()
filter_fast <- filter(x=ari_ma, sides=1,
  filter=co_eff, method="convolution")
filter_fast <- as.numeric(filter_fast)
# Compare excluding warmup period
all.equal(forecast_s[-(1:n_coeff)], filter_fast[-(1:(n_coeff-1))],
    check.attributes=FALSE)
# Filter using C_cfilter() compiled C++ function directly
filter_fast <- .Call(stats:::C_cfilter, ari_ma, filter=co_eff,
               sides=1, circular=FALSE)
# Compare excluding warmup period
all.equal(forecast_s[-(1:n_coeff)], filter_fast[-(1:(n_coeff-1))],
    check.attributes=FALSE)
# Filter using HighFreq::roll_conv() Rcpp function
filter_fast <- HighFreq::roll_conv(matrix(ari_ma), matrix(co_eff))
# Compare excluding warmup period
all.equal(forecast_s[-(1:n_coeff)], filter_fast[-(1:(n_coeff-1))],
    check.attributes=FALSE)
# Define predictor matrix for forecasting
predic_tor <- sapply(0:(n_coeff-1), function(lagg) {
  rutils::lag_it(ari_ma, lagg=lagg)
})  # end sapply
# Forecast using predictor matrix
filter_fast <- c(0, drop(predic_tor %*% co_eff))
# Compare with loop in R
all.equal(forecast_s, filter_fast, check.attributes=FALSE)
# Fit ARIMA model using arima()
arima_fit <- arima(ari_ma, order=c(3,0,0), include.mean=FALSE)
arima_fit$coef
co_eff
# One-step-ahead forecast using predict.Arima()
pre_dict <- predict(arima_fit, n.ahead=1)
# Or directly call predict.Arima()
# pre_dict <- predict.Arima(arima_fit, n.ahead=1)
# Inspect the prediction object
class(pre_dict)
names(pre_dict)
class(pre_dict$pred)
unlist(pre_dict)
# One-step-ahead forecast using matrix algebra
fore_cast <- drop(ari_ma[n_rows:(n_rows-2)] %*% arima_fit$coef)
# Compare one-step-ahead forecasts
all.equal(pre_dict$pred[[1]], fore_cast)
# Get information about predict.Arima()
?stats:::predict.Arima
# Calculate the in-sample forecasting residuals
residual_s <- (ari_ma - forecast_s[-NROW(forecast_s)])
# Compare residuals with innovations
all.equal(in_nov, residual_s, check.attributes=FALSE)
plot(residual_s, t="l", lwd=3, xlab="", ylab="",
     main="ARIMA Forecast Errors")
# Define AR process parameters
n_rows <- 1e3
co_eff <- c(0.5, 0.0, 0.0); n_coeff <- NROW(co_eff)
set.seed(1121); in_nov <- rnorm(n_rows)
# Simulate AR process using C_rfilter()
ari_ma <- .Call(stats:::C_rfilter, in_nov, co_eff,
  double(n_rows + n_coeff))[-(1:n_coeff)]
# Define order of the AR(n) forecasting model
or_der <- 5
# Define predictor matrix for forecasting
de_sign <- sapply(1:or_der, rutils::lag_it, in_put=ari_ma)
colnames(de_sign) <- paste0("pred_", 1:NCOL(de_sign))
# Add response equal to series
de_sign <- cbind(ari_ma, de_sign)
colnames(de_sign)[1] <- "response"
# Specify length of look-back interval
look_back <- 100
# Invert the predictor matrix
rang_e <- (n_rows-look_back):(n_rows-1)
design_inv <- MASS::ginv(de_sign[rang_e, -1])
# Calculate fitted coefficients
coeff_fit <- drop(design_inv %*% de_sign[rang_e, 1])
# Calculate forecast
drop(de_sign[n_rows, -1] %*% coeff_fit)
# Calculate a vector of daily VTI log returns
re_turns <- na.omit(rutils::etfenv$re_turns$VTI)
date_s <- index(re_turns)
re_turns <- as.numeric(re_turns)
n_rows <- NROW(re_turns)
# Define predictor as a rolling sum
n_agg <- 5
predic_tor <- rutils::roll_sum(re_turns, look_back=n_agg)
# Shift the res_ponse forward out-of-sample
res_ponse <- rutils::lag_it(predic_tor, lagg=(-n_agg))
# Define predictor matrix for forecasting
order_max <- 5
predic_tor <- sapply(1+n_agg*(0:order_max), rutils::lag_it,
               in_put=predic_tor)
predic_tor <- cbind(rep(1, n_rows), predic_tor)
# Define de_sign matrix
de_sign <- cbind(res_ponse, predic_tor)
# Perform rolling forecasting
look_back <- 100
forecast_s <- sapply((look_back+1):n_rows, function(end_p) {
  # Define rolling look-back range
  start_p <- max(1, end_p-look_back)
  # Or expanding look-back range
  # start_p <- 1
  rang_e <- start_p:(end_p-1)
  # Invert the predictor matrix
  design_inv <- MASS::ginv(de_sign[rang_e, -1])
  # Calculate fitted coefficients
  coeff_fit <- drop(design_inv %*% de_sign[rang_e, 1])
  # Calculate forecast
  drop(de_sign[end_p, -1] %*% coeff_fit)
})  # end sapply
# Add warmup period
forecast_s <- c(rep(0, look_back), forecast_s)
# Mean squared error
mean((re_turns - forecast_s)^2)
# Correlation
cor(forecast_s, re_turns)
# Plot forecasting series with legend
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0))
plot(re_turns[(n_rows-look_back):n_rows], col="blue",
     xlab="", ylab="", type="l", lwd=2,
     main="Rolling Forecasting Using AR Model")
lines(forecast_s[(n_rows-look_back):n_rows], col="red", lwd=2)
legend(x="top", legend=c("re_turns", "forecasts"),
 col=c("blue", "red"), lty=1, lwd=6,
 cex=0.9, bg="white", bty="n")
# Define backtesting function
sim_forecasts <- function(res_ponse, predic_tor=res_ponse, n_agg=5,
                or_der=5, lookback=100) {
  n_rows <- NROW(res_ponse)
  # Define predictor as a rolling sum
  predic_tor <- rutils::roll_sum(res_ponse, look_back=n_agg)
  # Shift the res_ponse forward out-of-sample
  res_ponse <- rutils::lag_it(predic_tor, lagg=(-n_agg))
  # Define predictor matrix for forecasting
  predic_tor <- sapply(1+n_agg*(0:or_der), rutils::lag_it,
                 in_put=predic_tor)
  predic_tor <- cbind(rep(1, n_rows), predic_tor)
  # Define de_sign matrix
  de_sign <- cbind(res_ponse, predic_tor)
  # Perform rolling forecasting
  forecast_s <- sapply((lookback+1):n_rows, function(end_p) {
    # Define rolling look-back range
    start_p <- max(1, end_p-lookback)
    # Or expanding look-back range
    # start_p <- 1
    rang_e <- start_p:(end_p-1)
    # Invert the predictor matrix
    design_inv <- MASS::ginv(de_sign[rang_e, -1])
    # Calculate fitted coefficients
    coeff_fit <- drop(design_inv %*% de_sign[rang_e, 1])
    # Calculate forecast
    drop(de_sign[end_p, -1] %*% coeff_fit)
  })  # end sapply
  # Add warmup period
  forecast_s <- c(rep(0, lookback), forecast_s)
  rutils::roll_sum(forecast_s, look_back=n_agg)
}  # end sim_forecasts
# Simulate the rolling autoregressive forecasts
forecast_s <- sim_forecasts(re_turns, or_der=5, look_back=100)
c(mse=mean((re_turns - forecast_s)^2), cor=cor(re_turns, forecast_s))
look_backs <- seq(20, 200, 20)
back_tests <- sapply(look_backs, sim_forecasts, res_ponse=res_ponse, predic_tor=res_ponse, n_agg=5, or_der=or_der)
colnames(back_tests) <- look_backs
mse_s <- apply(back_tests, 2, function(x) mean((re_turns - x)^2))
mean((back_tests[, 1] - re_turns)^2)
# Plot forecasting series with legend
plot(x=look_backs, y=mse_s,
  xlab="look-back", ylab="MSE", type="l", lwd=2,
  main="MSE of AR(5) Forecasting Model")
# Calculate a vector of daily VTI log returns
vt_i <- na.omit(rutils::etfenv$re_turns$VTI)
date_s <- index(vt_i)
vt_i <- as.numeric(vt_i)
n_rows <- NROW(vt_i)
# Define predictor matrix for forecasting
order_max <- 5
predic_tor <- sapply(1:order_max, rutils::lag_it, in_put=vt_i)
predic_tor <- cbind(rep(1, n_rows), predic_tor)
colnames(predic_tor) <- paste0("pred_", 1:NCOL(predic_tor))
res_ponse <- vt_i
# Calculate forecasts as function of the AR order
forecast_s <- lapply(2:NCOL(predic_tor), function(or_der) {
  # Calculate fitted coefficients
  in_verse <- MASS::ginv(predic_tor[, 1:or_der])
  co_eff <- drop(in_verse %*% res_ponse)
  # Calculate in-sample forecasts of vt_i
  drop(predic_tor[, 1:or_der] %*% co_eff)
})  # end lapply
names(forecast_s) <- paste0("p=", 2:NCOL(predic_tor))
# Calculate mean squared errors
ms_e <- sapply(forecast_s, function(x) {
  c(mse=mean((vt_i - x)^2), cor=cor(vt_i, x))
})  # end sapply
ms_e <- t(ms_e)
rownames(ms_e) <- names(forecast_s)
# Plot forecasting MSE
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
plot(x=2:NCOL(predic_tor), y=ms_e[, 1],
  xlab="AR(n) order", ylab="MSE", type="l", lwd=2,
  main="MSE of In-sample AR(n) Forecasting Model for VTI")
in_sample <- 1:(n_rows %/% 2)
out_sample <- (n_rows %/% 2 + 1):n_rows
# Calculate forecasts as function of the AR order
forecast_s <- lapply(2:NCOL(predic_tor), function(or_der) {
  # Calculate fitted coefficients
  in_verse <- MASS::ginv(predic_tor[in_sample, 1:or_der])
  co_eff <- drop(in_verse %*% res_ponse[in_sample])
  # Calculate out-of-sample forecasts of vt_i
  drop(predic_tor[out_sample, 1:or_der] %*% co_eff)
})  # end lapply
names(forecast_s) <- paste0("p=", 2:NCOL(predic_tor))
# Calculate mean squared errors
ms_e <- sapply(forecast_s, function(x) {
  c(mse=mean((vt_i[out_sample] - x)^2), cor=cor(vt_i[out_sample], x))
})  # end sapply
ms_e <- t(ms_e)
rownames(ms_e) <- names(forecast_s)
# Plot forecasting MSE
plot(x=2:NCOL(predic_tor), y=ms_e[, 1],
  xlab="AR(n) order", ylab="MSE", type="l", lwd=2,
  main="MSE of Out-of-sample AR(n) Forecasting Model for VTI")
# Calculate out-of-sample PnLs
pnl_s <- sapply(forecast_s, function(x) {
  cumsum(sign(x)*vt_i[out_sample])
})  # end sapply
colnames(pnl_s) <- names(forecast_s)
pnl_s <- xts::xts(pnl_s, date_s[out_sample])
# Plot dygraph of out-of-sample PnLs
color_s <- colorRampPalette(c("red", "blue"))(NCOL(pnl_s[, 1:4]))
col_names <- colnames(pnl_s[, 1:4])
dygraphs::dygraph(pnl_s[, 1:4],
  main="Autoregressive Strategies Performance With Different Order Parameters") %>%
  dyOptions(colors=color_s, strokeWidth=2) %>%
  dyLegend(width=500)
# Define predictor as a rolling mean
n_agg <- 5
predic_tor <- roll::roll_mean(vt_i, width=n_agg, min_obs=1)
res_ponse <- vt_i
# Define predictor matrix for forecasting
predic_tor <- sapply(1+n_agg*(0:order_max), rutils::lag_it,
               in_put=predic_tor)
predic_tor <- cbind(rep(1, n_rows), predic_tor)
# Calculate forecasts as function of the AR order
forecast_s <- lapply(2:NCOL(predic_tor), function(or_der) {
  in_verse <- MASS::ginv(predic_tor[in_sample, 1:or_der])
  co_eff <- drop(in_verse %*% res_ponse[in_sample])
  drop(predic_tor[out_sample, 1:or_der] %*% co_eff)
})  # end lapply
names(forecast_s) <- paste0("p=", 2:NCOL(predic_tor))
# Calculate out-of-sample PnLs
pnl_s <- sapply(forecast_s, function(x) {
  cumsum(sign(x)*vt_i[out_sample])
})  # end sapply
colnames(pnl_s) <- names(forecast_s)
pnl_s <- xts::xts(pnl_s, date_s[out_sample])
# Plot dygraph of out-of-sample PnLs
dygraphs::dygraph(pnl_s[, 1:4],
  main="Autoregressive Strategies Performance Using Rolling Average Predictor") %>%
  dyOptions(colors=color_s, strokeWidth=2) %>%
  dyLegend(width=500)
# Calculate out-of-sample PnLs
pnl_s <- sapply(forecast_s, function(x) {
  x <- roll::roll_mean(x, width=n_agg, min_obs=1)
  cumsum(sign(x)*vt_i[out_sample])
})  # end sapply
colnames(pnl_s) <- names(forecast_s)
pnl_s <- xts::xts(pnl_s, date_s[out_sample])
# Plot dygraph of out-of-sample PnLs
dygraphs::dygraph(pnl_s[, 1:4],
  main="Autoregressive Strategies Performance Using Rolling Average Forecasts") %>%
  dyOptions(colors=color_s, strokeWidth=2) %>%
  dyLegend(width=500)
# Calculate a vector of daily VTI log returns
vt_i <- na.omit(rutils::etfenv$re_turns$VTI)
date_s <- index(vt_i)
vt_i <- as.numeric(vt_i)
n_rows <- NROW(vt_i)
# Define predictor as a rolling mean
n_agg <- 5
predic_tor <- roll::roll_mean(vt_i, width=n_agg, min_obs=1)
# Shift the res_ponse forward out-of-sample
res_ponse <- vt_i
# Define predictor matrix for forecasting
order_max <- 5
predic_tor <- sapply(1+n_agg*(0:order_max), rutils::lag_it,
               in_put=predic_tor)
predic_tor <- cbind(rep(1, n_rows), predic_tor)
# Define de_sign matrix
de_sign <- cbind(res_ponse, predic_tor)
# Perform rolling forecasting
look_back <- 100
forecast_s <- sapply((look_back+1):n_rows, function(end_p) {
  # Define rolling look-back range
  start_p <- max(1, end_p-look_back)
  # Or expanding look-back range
  # start_p <- 1
  rang_e <- start_p:(end_p-1)
  # Invert the predictor matrix
  design_inv <- MASS::ginv(de_sign[rang_e, -1])
  # Calculate fitted coefficients
  co_eff <- drop(design_inv %*% de_sign[rang_e, 1])
  # Calculate forecast
  drop(de_sign[end_p, -1] %*% co_eff)
})  # end sapply
# Add warmup period
forecast_s <- c(rep(0, look_back), forecast_s)
# Mean squared error
mean((vt_i - forecast_s)^2)
# Correlation
cor(forecast_s, vt_i)
# Plot forecasting series with legend
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0))
plot(vt_i[(n_rows-look_back):n_rows], col="blue",
     xlab="", ylab="", type="l", lwd=2,
     main="Rolling Forecasting Using AR Model")
lines(forecast_s[(n_rows-look_back):n_rows], col="red", lwd=2)
legend(x="top", legend=c("VTI returns", "forecasts"),
 col=c("blue", "red"), lty=1, lwd=6,
 cex=0.9, bg="white", bty="n")
# Define backtesting function
sim_forecasts <- function(res_ponse, predic_tor=res_ponse, n_agg=5,
                or_der=5, look_back=100) {
  n_rows <- NROW(res_ponse)
  # Define predictor as a rolling mean
  predic_tor <- roll::roll_mean(vt_i, width=n_agg, min_obs=1)
  # Define predictor matrix for forecasting
  predic_tor <- sapply(1+n_agg*(0:or_der), rutils::lag_it,
                 in_put=predic_tor)
  predic_tor <- cbind(rep(1, n_rows), predic_tor)
  # Define de_sign matrix
  de_sign <- cbind(res_ponse, predic_tor)
  # Perform rolling forecasting
  forecast_s <- sapply((look_back+1):n_rows, function(end_p) {
    # Define rolling look-back range
    start_p <- max(1, end_p-look_back)
    # Or expanding look-back range
    # start_p <- 1
    rang_e <- start_p:(end_p-1)
    # Invert the predictor matrix
    design_inv <- MASS::ginv(de_sign[rang_e, -1])
    # Calculate fitted coefficients
    co_eff <- drop(design_inv %*% de_sign[rang_e, 1])
    # Calculate forecast
    drop(de_sign[end_p, -1] %*% co_eff)
  })  # end sapply
  # Add warmup period
  forecast_s <- c(rep(0, look_back), forecast_s)
  roll::roll_mean(forecast_s, width=n_agg, min_obs=1)
}  # end sim_forecasts
# Simulate the rolling autoregressive forecasts
forecast_s <- sim_forecasts(vt_i, or_der=5, look_back=100)
c(mse=mean((vt_i - forecast_s)^2), cor=cor(vt_i, forecast_s))
look_backs <- seq(20, 600, 40)
library(parallel)  # Load package parallel
# Calculate number of available cores
n_cores <- detectCores() - 1
# Initialize compute cluster under Windows
clus_ter <- makeCluster(n_cores)
# clusterExport(clus_ter, c("star_t", "bar_rier"))
# Perform parallel loop under Windows
forecast_s <- parLapply(clus_ter, look_backs, sim_forecasts, res_ponse=vt_i,
                  predic_tor=vt_i, n_agg=5, or_der=5)
# Perform parallel bootstrap under Mac-OSX or Linux
forecast_s <- mclapply(look_backs, sim_forecasts, res_ponse=vt_i,
  predic_tor=vt_i, n_agg=5, or_der=5, mc.cores=n_cores)
# Calculate mean squared errors
ms_e <- sapply(forecast_s, function(x) {
  c(mse=mean((vt_i - x)^2), cor=cor(vt_i, x))
})  # end sapply
ms_e <- t(ms_e)
rownames(ms_e) <- look_backs
# Select optimal look_back interval
look_back <- look_backs[which.min(ms_e[, 1])]
# Plot forecasting MSE
plot(x=look_backs, y=ms_e[, 1],
  xlab="look-back", ylab="MSE", type="l", lwd=2,
  main="MSE of AR Forecasting Model As Function of Look-back")
order_s <- 2:6
library(parallel)  # Load package parallel
# Calculate number of available cores
n_cores <- detectCores() - 1
# Initialize compute cluster under Windows
clus_ter <- makeCluster(n_cores)
# clusterExport(clus_ter, c("star_t", "bar_rier"))
# Perform parallel loop under Windows
forecast_s <- parLapply(clus_ter, order_s, sim_forecasts, res_ponse=vt_i,
                  predic_tor=vt_i, n_agg=5, look_back=look_back)
stopCluster(clus_ter)  # Stop R processes over cluster under Windows
# Perform parallel bootstrap under Mac-OSX or Linux
forecast_s <- mclapply(order_s, sim_forecasts, res_ponse=vt_i,
  predic_tor=vt_i, n_agg=5, look_back=look_back, mc.cores=n_cores)
# Calculate mean squared errors
ms_e <- sapply(forecast_s, function(x) {
  c(mse=mean((vt_i - x)^2), cor=cor(vt_i, x))
})  # end sapply
ms_e <- t(ms_e)
rownames(ms_e) <- order_s
# Select optimal order parameter
or_der <- order_s[which.min(ms_e[, 1])]
# Plot forecasting MSE
plot(x=order_s, y=ms_e[, 1],
  xlab="or_der", ylab="MSE", type="l", lwd=2,
  main="MSE of Forecasting Model As Function of AR Order")
# Simulate the rolling autoregressive forecasts
forecast_s <- sim_forecasts(vt_i, or_der=or_der, look_back=look_back)
# Calculate strategy PnLs
pnl_s <- sign(forecast_s)*vt_i
pnl_s <- cbind(vt_i, pnl_s, (vt_i+pnl_s)/2)
colnames(pnl_s) <- c("VTI", "AR_Strategy", "Combined")
cor(pnl_s)
# Annualized Sharpe ratios of VTI and AR strategy
pnl_s <- xts::xts(pnl_s, date_s)
sqrt(252)*sapply(pnl_s, function (x) mean(x)/sd(x))
# Plot the cumulative strategy PnLs
dygraphs::dygraph(cumsum(pnl_s), main="Rolling Autoregressive Strategy") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Load constant maturity Treasury rates
load(file="/Users/jerzy/Develop/lecture_slides/data/rates_data.RData")
# Combine rates into single xts series
rate_s <- do.call(cbind, as.list(rates_env))
# Sort the columns of rate_s according bond maturity
name_s <- colnames(rate_s)
name_s <- substr(name_s, start=4, stop=10)
name_s <- as.numeric(name_s)
indeks <- order(name_s)
rate_s <- rate_s[, indeks]
# Align rates dates with VTI prices
clos_e <- quantmod::Cl(rutils::etfenv$VTI)
colnames(clos_e) <- "VTI"
n_rows <- NROW(clos_e)
date_s <- zoo::index(clos_e)
rate_s <- na.omit(rate_s[date_s])
clos_e <- clos_e[zoo::index(rate_s)]
date_s <- zoo::index(clos_e)
# Calculate VTI returns and IR changes
re_turns <- rutils::diff_it(log(clos_e))
rates_diff <- rutils::diff_it(log(rate_s))
# Regress VTI returns versus the lagged rate differences
predic_tor <- rutils::lag_it(rates_diff)
mod_el <- lm(re_turns ~ predic_tor)
summary(mod_el)
# Regress VTI returns before and after 2012
summary(lm(re_turns["/2012"] ~ predic_tor["/2012"]))
summary(lm(re_turns["2012/"] ~ predic_tor["2012/"]))
# Calculate PCA of rates correlation matrix
ei_gen <- eigen(cor(rates_diff))
rates_pca <- -rates_diff %*% ei_gen$vectors
colnames(rates_pca) <- paste0("PC", 1:6)
# Define predictor as the YC PCAs
predic_tor <- rutils::lag_it(rates_pca)
mod_el <- lm(re_turns ~ predic_tor)
summary(mod_el)
# Plot YC steepener principal component with VTI
da_ta <- cbind(re_turns, rates_pca[, 2])
colnames(da_ta) <- c("VTI", "Steepener")
col_names <- colnames(da_ta)
dygraphs::dygraph(cumsum(da_ta), main="VTI and Yield Curve Steepener") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=1, col="blue") %>%
  dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=1, col="red")
# Define predictor with intercept term
predic_tor <- rutils::lag_it(rates_diff)
predic_tor <- cbind(rep(1, NROW(predic_tor)), predic_tor)
colnames(predic_tor)[1] <- "intercept"
# Calculate inverse of predictor
in_verse <- MASS::ginv(predic_tor)
# Calculate coefficients from response and inverse of predictor
res_ponse <- re_turns
co_eff <- drop(in_verse %*% res_ponse)
# Calculate forecasts in-sample
forecast_s <- (predic_tor %*% co_eff)
pnl_s <- forecast_s*re_turns
# Calculate in-sample factors
factor_s <- (predic_tor * co_eff)
apply(factor_s, 2, sd)
# Plot dygraph of in-sample IR strategy
weal_th <- cbind(re_turns, pnl_s)
colnames(weal_th) <- c("VTI", "Strategy")
col_names <- colnames(weal_th)
dygraphs::dygraph(cumsum(weal_th), main="Yield Curve Strategy In-sample") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=col_names[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Define in-sample and out-of-sample intervals
in_sample <- (date_s < as.Date("2020-01-01"))
out_sample <- (date_s >= as.Date("2020-01-01"))
# Calculate inverse of predictor in-sample
in_verse <- MASS::ginv(predic_tor[in_sample, ])
# Calculate coefficients in-sample
co_eff <- drop(in_verse %*% res_ponse[in_sample, ])
# Calculate forecasts and pnls out-of-sample
forecast_s <- (predic_tor[out_sample, ] %*% co_eff)
pnl_s <- forecast_s*re_turns[out_sample, ]
# Plot dygraph of out-of-sample IR PCA strategy
weal_th <- cbind(re_turns[out_sample, ], pnl_s)
colnames(weal_th) <- c("VTI", "Strategy")
col_names <- colnames(weal_th)
dygraphs::dygraph(cumsum(weal_th), main="Yield Curve Strategy Out-of-Sample") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=col_names[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Find optimal n_agg for predictor
n_aggs <- 5:100
tvalues <- sapply(n_aggs, function(n_agg) {
  predic_tor <- roll::roll_mean(rates_diff, width=n_agg, min_obs=1)
  predic_tor <- cbind(rep(1, NROW(predic_tor)), predic_tor)
  predic_tor <- rutils::lag_it(predic_tor)
  mod_el <- lm(res_ponse ~ predic_tor - 1)
  model_sum <- summary(mod_el)
  max(abs(model_sum$coefficients[, 3][-1]))
})  # end sapply
n_aggs[which.max(tvalues)]
plot(n_aggs, tvalues, t="l", col="blue", lwd=2)
# Calculate aggregated predictor
n_agg <- 53
predic_tor <- roll::roll_mean(rates_diff, width=n_agg, min_obs=1)
predic_tor <- rutils::lag_it(predic_tor)
predic_tor <- cbind(rep(1, NROW(predic_tor)), predic_tor)
mod_el <- lm(res_ponse ~ predic_tor - 1)
summary(mod_el)
# Calculate forecasts in-sample
in_verse <- MASS::ginv(predic_tor)
co_eff <- drop(in_verse %*% res_ponse)
forecast_s <- (predic_tor %*% co_eff)
pnl_s <- forecast_s*re_turns
# Plot dygraph of in-sample IR strategy
weal_th <- cbind(re_turns, pnl_s)
colnames(weal_th) <- c("VTI", "Strategy")
col_names <- colnames(weal_th)
dygraphs::dygraph(cumsum(weal_th), main="Aggregated YC Strategy In-sample") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=col_names[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Define in-sample and out-of-sample intervals
in_sample <- (date_s < as.Date("2020-01-01"))
out_sample <- (date_s >= as.Date("2020-01-01"))
# Calculate forecasts and pnls out-of-sample
in_verse <- MASS::ginv(predic_tor[in_sample, ])
co_eff <- drop(in_verse %*% res_ponse[in_sample, ])
forecast_s <- (predic_tor[out_sample, ] %*% co_eff)
pnl_s <- forecast_s*re_turns[out_sample, ]
# Plot dygraph of out-of-sample YC strategy
weal_th <- cbind(re_turns[out_sample, ], pnl_s)
colnames(weal_th) <- c("VTI", "Strategy")
col_names <- colnames(weal_th)
dygraphs::dygraph(cumsum(weal_th), main="Aggregated YC Strategy Out-of-Sample") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=col_names[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Define yearly dates
format(date_s[1], "%Y")
year_s <- paste0(seq(2001, 2022, 1), "-01-01")
year_s <- as.Date(year_s)
# Perform loop over yearly dates
pnl_s <- lapply(3:(NROW(year_s)-1), function(i) {
  # Define in-sample and out-of-sample intervals
  in_sample <- (date_s > year_s[i-2]) & (date_s < year_s[i])
  out_sample <- (date_s >= year_s[i]) & (date_s < year_s[i+1])
  # Calculate coefficients in-sample
  in_verse <- MASS::ginv(predic_tor[in_sample, ])
  # in_verse <- HighFreq::calc_inv(predic_tor[in_sample, ], eigen_max=3)
  co_eff <- drop(in_verse %*% res_ponse[in_sample, ])
  # Calculate forecasts and pnls out-of-sample
  forecast_s <- (predic_tor[out_sample, ] %*% co_eff)
  forecast_s*re_turns[out_sample, ]
})  # end lapply
pnl_s <- do.call(rbind, pnl_s)
# Plot dygraph of rolling yearly IR strategy
weal_th <- cbind(re_turns[zoo::index(pnl_s),], pnl_s)
colnames(weal_th) <- c("VTI", "Strategy")
col_names <- colnames(weal_th)
dygraphs::dygraph(cumsum(weal_th), main="Rolling Yearly Yield Curve Strategy") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=col_names[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Define monthly dates
format(date_s[1], "%m-%Y")
format(date_s[NROW(date_s)], "%m-%Y")
month_s <- seq.Date(from=as.Date("2001-05-01"), to=as.Date("2021-04-01"), by="month")
# Perform loop over monthly dates
pnl_s <- lapply(12:(NROW(month_s)-1), function(i) {
  # Define in-sample and out-of-sample intervals
  in_sample <- (date_s > month_s[i-11]) & (date_s < month_s[i])
  out_sample <- (date_s > month_s[i]) & (date_s < month_s[i+1])
  # Calculate forecasts and pnls out-of-sample
  in_verse <- MASS::ginv(predic_tor[in_sample, ])
  # in_verse <- HighFreq::calc_inv(predic_tor[in_sample, ], eigen_max=3)
  co_eff <- drop(in_verse %*% res_ponse[in_sample, ])
  forecast_s <- (predic_tor[out_sample, ] %*% co_eff)
  forecast_s*re_turns[out_sample, ]
})  # end lapply
pnl_s <- do.call(rbind, pnl_s)
# Plot dygraph of rolling monthly IR strategy
weal_th <- cbind(re_turns[zoo::index(pnl_s),], pnl_s)
colnames(weal_th) <- c("VTI", "Strategy")
col_names <- colnames(weal_th)
dygraphs::dygraph(cumsum(weal_th), main="Rolling Monthly Yield Curve Strategy") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=col_names[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Define weekly dates
week_s <- seq.Date(from=as.Date("2001-05-01"), to=as.Date("2021-04-01"), by="weeks")
# Perform loop over weekly dates
pnl_s <- lapply(51:(NROW(week_s)-1), function(i) {
  # Define in-sample and out-of-sample intervals
  in_sample <- (date_s > week_s[i-50]) & (date_s < week_s[i])
  out_sample <- (date_s > week_s[i]) & (date_s < week_s[i+1])
  # Calculate forecasts and pnls out-of-sample
  in_verse <- MASS::ginv(predic_tor[in_sample, ])
  # in_verse <- HighFreq::calc_inv(predic_tor[in_sample, ], eigen_max=3)
  co_eff <- drop(in_verse %*% res_ponse[in_sample, ])
  forecast_s <- (predic_tor[out_sample, ] %*% co_eff)
  forecast_s*re_turns[out_sample, ]
})  # end lapply
pnl_s <- do.call(rbind, pnl_s)
# Plot dygraph of rolling weekly IR strategy
weal_th <- cbind(re_turns[zoo::index(pnl_s),], pnl_s)
colnames(weal_th) <- c("VTI", "Strategy")
col_names <- colnames(weal_th)
dygraphs::dygraph(cumsum(weal_th), main="Rolling Weekly Yield Curve Strategy") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=col_names[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Extract ETF returns
sym_bols <- c("VTI", "IEF", "DBC")
re_turns <- rutils::etfenv$re_turns[, sym_bols]
re_turns <- na.omit(re_turns)
# Or, select rows with IEF data
# re_turns <- re_turns[index(rutils::etfenv$IEF)]
# Copy over NA values
# re_turns[1, is.na(re_turns[1, ])] <- 0
# re_turns <- zoo::na.locf(re_turns, na.rm=FALSE)
# Define end of month end points
end_p <- rutils::calc_endpoints(re_turns, inter_val="months")
end_p <- end_p[-1]
n_rows <- NROW(end_p)
date_s <- zoo::index(re_turns)[end_p]
# Start points equal end points lagged by 12-month look-back interval
look_back <- 12
start_p <- c(rep_len(1, look_back-1),
  end_p[1:(n_rows - look_back + 1)])
# Calculate matrix of look-back intervals
look_backs <- cbind(start_p, end_p)
colnames(look_backs) <- c("start", "end")
# Calculate matrix of look-forward intervals
look_fwds <- cbind(end_p + 1, rutils::lag_it(end_p, -1))
look_fwds[n_rows, ] <- end_p[n_rows]
colnames(look_fwds) <- c("start", "end")
# Inspect the intervals
head(cbind(look_backs, look_fwds))
tail(cbind(look_backs, look_fwds))
# Define performance function as Sharpe ratio
perform_ance <- function(re_turns) sum(re_turns)/sd(re_turns)
# Calculate past performance over look-back intervals
pas_t <- apply(look_backs, 1, function(ep) {
  sapply(re_turns[ep[1]:ep[2]], perform_ance)
})  # end sapply
pas_t <- t(pas_t)
pas_t[is.na(pas_t)] <- 0
# Weights are proportional to past performance
weight_s <- pas_t
# weight_s[weight_s < 0] <- 0
# Scale weight_s so sum of squares is equal to 1.
weight_s <- weight_s/sqrt(rowSums(weight_s^2))
# Or scale weight_s so sum is equal to 1
# weight_s <- weight_s/rowSums(weight_s)
# Set NA values to zero
weight_s[is.na(weight_s)] <- 0
sum(is.na(weight_s))
# Calculate future out-of-sample performance
fu_ture <- apply(look_fwds, 1, function(ep) {
  sapply(re_turns[ep[1]:ep[2]], sum)
})  # end sapply
fu_ture <- t(fu_ture)
fu_ture[is.na(fu_ture)] <- 0
tail(fu_ture)
# Calculate the momentum pnls
pnl_s <- rowSums(weight_s*fu_ture)
# Lag the future and momentum returns to proper dates
fu_ture <- rutils::lag_it(fu_ture)
pnl_s <- rutils::lag_it(pnl_s)
# The momentum strategy has low correlation to stocks
cor(pnl_s, fu_ture)
# Define all-weather benchmark
weights_aw <- c(0.30, 0.55, 0.15)
all_weather <- fu_ture %*% weights_aw
# Calculate the wealth of momentum returns
weal_th <- xts::xts(cbind(all_weather, pnl_s), order.by=date_s)
colnames(weal_th) <- c("All-Weather", "Momentum")
cor(weal_th)
# Plot dygraph of the momentum strategy returns
dygraphs::dygraph(cumsum(weal_th), main="Monthly Momentum Strategy vs All-Weather") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Define backtest functional
backtest_momentum <- function(returns,
                perform_ance=function(re_turns) (sum(re_turns)/sd(re_turns)),
                look_back=12, re_balance="months", bid_offer=0.001,
                end_p=rutils::calc_endpoints(re_turns, inter_val=re_balance)[-1],
                with_weights=FALSE, ...) {
  stopifnot("package:rutils" %in% search() || require("rutils", quietly=TRUE))
  # Define look-back and look-forward intervals
  n_rows <- NROW(end_p)
  start_p <- c(rep_len(1, look_back-1), end_p[1:(n_rows-look_back+1)])
  # Calculate look-back intervals
  look_backs <- cbind(start_p, end_p)
  # Calculate look-forward intervals
  look_fwds <- cbind(end_p + 1, rutils::lag_it(end_p, -1))
  look_fwds[n_rows, ] <- end_p[n_rows]
  # Calculate past performance over look-back intervals
  pas_t <- t(apply(look_backs, 1, function(ep) sapply(re_turns[ep[1]:ep[2]], perform_ance)))
  pas_t[is.na(pas_t)] <- 0
  # Calculate future performance
  fu_ture <- t(apply(look_fwds, 1, function(ep) sapply(re_turns[ep[1]:ep[2]], sum)))
  fu_ture[is.na(fu_ture)] <- 0
  # Scale weight_s so sum of squares is equal to 1
  weight_s <- pas_t
  weight_s <- weight_s/sqrt(rowSums(weight_s^2))
  weight_s[is.na(weight_s)] <- 0  # Set NA values to zero
  # Calculate momentum profits and losses
  pnl_s <- rowSums(weight_s*fu_ture)
  # Calculate transaction costs
  cost_s <- 0.5*bid_offer*cumprod(1 + pnl_s)*rowSums(abs(rutils::diff_it(weight_s)))
  pnl_s <- (pnl_s - cost_s)
  if (with_weights)
    rutils::lag_it(cbind(pnl_s, weight_s))
  else
    rutils::lag_it(pnl_s)
}  # end backtest_momentum
source("C:/Develop/lecture_slides/scripts/back_test.R")
look_backs <- seq(3, 15, by=1)
perform_ance <- function(re_turns) sum(re_turns)/sd(re_turns)
pro_file <- sapply(look_backs, function(look_back) {
  pnl_s <- backtest_momentum(returns=re_turns, end_p=end_p,
    look_back=look_back, perform_ance=perform_ance)
  sum(pnl_s)
})  # end sapply
# Plot momemntum PnLs
x11(width=6, height=5)
plot(x=look_backs, y=pro_file, t="l",
  main="Momemntum PnL as function of look_back",
  xlab="look_back (months)", ylab="pnl")
# Optimal look_back
look_back <- look_backs[which.max(pro_file)]
pnl_s <- backtest_momentum(returns=re_turns,
  look_back=look_back, end_p=end_p,
  perform_ance=perform_ance, with_weights=TRUE)
tail(pnl_s)
# Calculate the wealth of momentum returns
ret_mom <- pnl_s[, 1]
weal_th <- xts::xts(cbind(all_weather, ret_mom), order.by=date_s)
colnames(weal_th) <- c("All-Weather", "Momentum")
cor(weal_th)
# Plot dygraph of the momentum strategy returns
dygraphs::dygraph(cumsum(weal_th), main="Monthly Momentum Strategy vs All-Weather") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Or
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue")
quantmod::chart_Series(cumsum(weal_th), theme=plot_theme, lwd=2,
       name="Momentum PnL")
legend("topleft", legend=colnames(weal_th),
  inset=0.1, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")
# Plot the momentum portfolio weights
weight_s <- pnl_s[, -1]
vt_i <- log(quantmod::Cl(rutils::etfenv$VTI[date_s]))
colnames(vt_i) <- "VTI"
da_ta <- cbind(vt_i, weight_s)
da_ta <- na.omit(da_ta)
colnames(da_ta)[2:NCOL(pnl_s)] <- paste0(colnames(weight_s), "_weight")
zoo::plot.zoo(da_ta, xlab=NULL, main="Momentum Weights")
# Calculate ETF betas
betas_etf <- sapply(re_turns, function(x)
  cov(re_turns$VTI, x)/var(x))
# Momentum beta is equal weights times ETF betas
beta_s <- weight_s %*% betas_etf
beta_s <- xts::xts(beta_s, order.by=date_s)
colnames(beta_s) <- "momentum_beta"
da_ta <- cbind(beta_s, vt_i)
zoo::plot.zoo(da_ta,
  oma = c(3, 1, 3, 0), mar = c(0, 4, 0, 1),
  main="Momentum Beta & VTI Price", xlab="")
# Open x11 for plotting and set parameters to reduce whitespace around plot
x11(width=6, height=5)
par(mar=c(4, 4, 3, 1), oma=c(0, 0, 0, 0))
# Merton-Henriksson test
vt_i <- rutils::diff_it(vt_i)
de_sign <- cbind(VTI=vt_i, 0.5*(vt_i+abs(vt_i)), vt_i^2)
colnames(de_sign)[2:3] <- c("merton", "treynor")
mod_el <- lm(ret_mom ~ VTI + merton, data=de_sign); summary(mod_el)
# Treynor-Mazuy test
mod_el <- lm(ret_mom ~ VTI + treynor, data=de_sign); summary(mod_el)
# Plot residual scatterplot
plot.default(x=vt_i, y=ret_mom, xlab="VTI", ylab="momentum")
title(main="Treynor-Mazuy market timing test\n for Momentum vs VTI", line=0.5)
# Plot fitted (predicted) response values
points.default(x=vt_i, y=mod_el$fitted.values, pch=16, col="red")
residual_s <- mod_el$residuals
text(x=0.0, y=max(residual_s), paste("Treynor test t-value =", round(summary(mod_el)$coeff["treynor", "t value"], 2)))
# Standardize the returns
ret_mom_std <- (ret_mom-mean(ret_mom))/sd(ret_mom)
vt_i <- (vt_i-mean(vt_i))/sd(vt_i)
# Calculate skewness and kurtosis
apply(cbind(ret_mom_std, vt_i), 2, function(x)
  sapply(c(skew=3, kurt=4),
    function(e) sum(x^e)))/n_rows
# Plot histogram
hist(ret_mom_std, breaks=30,
  main="Momentum and VTI Return Distributions (standardized",
  xlim=c(-4, 4),
  xlab="", ylab="", freq=FALSE)
# Draw kernel density of histogram
lines(density(ret_mom_std), col='red', lwd=2)
lines(density(vt_i), col='blue', lwd=2)
# Add legend
legend("topright", inset=0.05, cex=0.8, title=NULL,
 leg=c("Momentum", "VTI"),
 lwd=6, bg="white", col=c("red", "blue"))
# Combine momentum strategy with all-weather
all_weather <- sd(ret_mom)*all_weather/sd(all_weather)
weal_th <- cbind(ret_mom, all_weather, 0.5*(ret_mom + all_weather))
colnames(weal_th) <- c("momentum", "all_weather", "combined")
# Calculate strategy annualized Sharpe ratios
apply(weal_th, MARGIN=2, function(x) {
  sqrt(12)*sum(x)/sd(x)/NROW(x)
})  # end apply
# Calculate strategy correlations
cor(weal_th)
# Calculate cumulative wealth
weal_th <- xts::xts(weal_th, date_s)
# Plot ETF momentum strategy combined with All-Weather
dygraphs::dygraph(cumsum(weal_th), main="ETF Momentum Strategy Combined with All-Weather") %>%
  dyOptions(colors=c("red", "blue", "green"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Or
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("green", "blue", "red")
quantmod::chart_Series(weal_th, theme=plot_theme,
       name="ETF Momentum Strategy Combined with All-Weather")
legend("topleft", legend=colnames(weal_th),
  inset=0.1, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")
# Calculate rolling variance
look_back <- 252
vari_ance <- roll::roll_var(re_turns, width=look_back, min_obs=1)
vari_ance[1, ] <- 1
# Calculate rolling Sharpe
pas_t <- roll::roll_mean(re_turns, width=look_back, min_obs=1)
weight_s <- pas_t/sqrt(vari_ance)
weight_s <- weight_s/sqrt(rowSums(weight_s^2))
weight_s <- rutils::lag_it(weight_s)
sum(is.na(weight_s))
# Calculate momentum profits and losses
pnl_s <- rowMeans(weight_s*re_turns)
# Calculate transaction costs
bid_offer <- 0.001
cost_s <- 0.5*bid_offer*rowSums(abs(rutils::diff_it(weight_s)))
pnl_s <- (pnl_s - cost_s)
# Define all-weather benchmark
weights_aw <- c(0.30, 0.55, 0.15)
all_weather <- re_turns %*% weights_aw
# Calculate the wealth of momentum returns
weal_th <- xts::xts(cbind(all_weather, pnl_s), order.by=index(re_turns))
colnames(weal_th) <- c("All-Weather", "Momentum")
cor(weal_th)
# Plot dygraph of the momentum strategy returns
dygraphs::dygraph(cumsum(weal_th)[date_s], main="Daily Momentum Strategy vs All-Weather") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Define backtest functional for daily momentum strategy
# If trend=(-1) then it backtests a mean reverting strategy
momentum_daily <- function(returns, look_back=252, bid_offer=0.001, trend=1, ...) {
  stopifnot("package:quantmod" %in% search() || require("quantmod", quietly=TRUE))
  # Calculate rolling variance
  vari_ance <- roll::roll_var(returns, width=look_back, min_obs=1)
  vari_ance[1, ] <- 1
  vari_ance[vari_ance <= 0] <- 1
# Calculate rolling Sharpe
  pas_t <- roll::roll_mean(returns, width=look_back, min_obs=1)
  weight_s <- pas_t/sqrt(vari_ance)
  weight_s <- weight_s/sqrt(rowSums(weight_s^2))
  weight_s <- rutils::lag_it(weight_s)
  # Calculate momentum profits and losses
  pnl_s <- trend*rowMeans(weight_s*returns)
  # Calculate transaction costs
  cost_s <- 0.5*bid_offer*rowSums(abs(rutils::diff_it(weight_s)))
  (pnl_s - cost_s)
}  # end momentum_daily
# Simulate a daily ETF momentum strategy
source("C:/Develop/lecture_slides/scripts/back_test.R")
weal_th <- momentum_daily(returns=re_turns, look_back=252,
  bid_offer=bid_offer)
# Perform sapply loop over look_backs
look_backs <- seq(50, 300, by=50)
weal_th <- sapply(look_backs, momentum_daily,
  returns=re_turns, bid_offer=bid_offer)
colnames(weal_th) <- paste0("look_back=", look_backs)
weal_th <- xts::xts(weal_th, index(re_turns))
tail(weal_th)
# Plot EWMA strategies with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(weal_th))
quantmod::chart_Series(cumsum(weal_th),
  theme=plot_theme, name="Cumulative Returns of Daily ETF Momentum Strategies")
legend("bottomleft", legend=colnames(weal_th),
  inset=0.02, bg="white", cex=0.7, lwd=rep(6, NCOL(re_turns)),
  col=plot_theme$col$line.col, bty="n")
# Load daily S&P500 percentage stock returns.
load(file="/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
# Overwrite NA values in returns_100
returns_100 <- returns_100["2000/"]
returns_100[1, is.na(returns_100[1, ])] <- 0
returns_100 <- zoo::na.locf(returns_100, na.rm=FALSE)
# Simulate a daily S&P500 momentum strategy.
# Perform sapply loop over look_backs
look_backs <- seq(50, 300, by=50)
weal_th <- sapply(look_backs, momentum_daily,
  returns=returns_100, bid_offer=0)
colnames(weal_th) <- paste0("look_back=", look_backs)
weal_th <- xts::xts(weal_th, index(returns_100))
# Plot daily S&P500 momentum strategies with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- colorRampPalette(c("blue", "red"))(NCOL(weal_th))
quantmod::chart_Series(cumsum(weal_th),
  theme=plot_theme, name="Cumulative Returns of S&P500 Momentum Strategies")
legend("bottomleft", legend=colnames(weal_th),
  inset=0.02, bg="white", cex=0.7, lwd=rep(6, NCOL(re_turns)),
  col=plot_theme$col$line.col, bty="n")
# Perform sapply loop over look_backs
look_backs <- seq(5, 50, by=5)
weal_th <- sapply(look_backs, momentum_daily,
  returns=returns_100, bid_offer=0, trend=(-1))
colnames(weal_th) <- paste0("look_back=", look_backs)
weal_th <- xts::xts(weal_th, index(returns_100))
# Plot EWMA strategies with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- colorRampPalette(c("blue", "red"))(NCOL(weal_th))
quantmod::chart_Series(cumsum(weal_th),
  theme=plot_theme, name="Cumulative Returns of S&P500 Mean Reverting Strategies")
legend("topleft", legend=colnames(weal_th),
  inset=0.05, bg="white", cex=0.7, lwd=rep(6, NCOL(re_turns)),
  col=plot_theme$col$line.col, bty="n")
library(rutils)
# Select all the ETF symbols except "VXX", "SVXY" and "MTUM"
sym_bols <- colnames(rutils::etfenv$re_turns)
sym_bols <- sym_bols[!(sym_bols %in% c("VXX", "SVXY", "MTUM"))]
# Extract columns of rutils::etfenv$re_turns and overwrite NA values
re_turns <- rutils::etfenv$re_turns[, sym_bols]
n_assets <- NCOL(re_turns)
# re_turns <- na.omit(re_turns)
re_turns[1, is.na(re_turns[1, ])] <- 0
re_turns <- zoo::na.locf(re_turns, na.rm=FALSE)
# Returns in excess of risk-free rate
risk_free <- 0.03/252
ex_cess <- (re_turns - risk_free)
# Maximum Sharpe weights in-sample interval
in_verse <- MASS::ginv(cov(re_turns["/2014"]))
weight_s <- in_verse %*% colMeans(ex_cess["/2014"])
weight_s <- drop(weight_s/sqrt(sum(weight_s^2)))
names(weight_s) <- colnames(re_turns)
# Plot portfolio weights
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
barplot(sort(weight_s), main="Maximum Sharpe Weights", cex.names=0.7)
# Calculate portfolio returns
rets_is <- re_turns["/2014"]
portf_is <- xts::xts(rets_is %*% weight_s, index(rets_is))
in_dex <- xts::xts(rowSums(rets_is)/sqrt(n_assets), index(rets_is))
portf_is <- portf_is*sd(in_dex)/sd(portf_is)
# Plot cumulative portfolio returns
weal_th <- cumsum(cbind(portf_is, in_dex))
colnames(weal_th) <- c("Optimal Portfolio", "Equal Weight Portfolio")
dygraphs::dygraph(weal_th, main="In-sample Optimal Portfolio Returns") %>%
  dyOptions(colors=c("red", "blue"), strokeWidth=2) %>%
  dyLegend(width=500)
# Out-of-sample portfolio returns
rets_os <- re_turns["2015/"]
portf_os <- xts::xts(rets_os %*% weight_s, index(rets_os))
in_dex <- xts::xts(rowSums(rets_os)/sqrt(n_assets), index(rets_os))
portf_os <- portf_os*sd(in_dex)/sd(portf_os)
# Plot cumulative portfolio returns
weal_th <- cumsum(cbind(portf_os, in_dex))
colnames(weal_th) <- c("Optimal Portfolio", "Equal Weight Portfolio")
dygraphs::dygraph(weal_th, main="Out-of-sample Optimal Portfolio Returns") %>%
  dyOptions(colors=c("red", "blue"), strokeWidth=2) %>%
  dyLegend(width=500)
load("C:/Develop/lecture_slides/data/sp500_returns.RData")
# Overwrite NA values in re_turns
re_turns <- re_turns["2000/"]
n_assets <- NCOL(re_turns)
re_turns[1, is.na(re_turns[1, ])] <- 0
re_turns <- zoo::na.locf(re_turns, na.rm=FALSE)
risk_free <- 0.03/252
ex_cess <- (re_turns - risk_free)
rets_is <- re_turns["/2010"]
rets_os <- re_turns["2011/"]
# Maximum Sharpe weights in-sample interval
cov_mat <- cov(rets_is)
in_verse <- MASS::ginv(cov_mat)
weight_s <- in_verse %*% colMeans(ex_cess["/2010"])
weight_s <- drop(weight_s/sqrt(sum(weight_s^2)))
names(weight_s) <- colnames(re_turns)
# Calculate portfolio returns
portf_is <- xts::xts(rets_is %*% weight_s, index(rets_is))
portf_os <- xts::xts(rets_os %*% weight_s, index(rets_os))
in_dex <- xts::xts(rowSums(re_turns)/sqrt(n_assets), index(re_turns))
# Plot cumulative portfolio returns
weal_th <- rbind(portf_is, portf_os)
weal_th <- weal_th*sd(in_dex)/sd(weal_th)
weal_th <- cumsum(cbind(weal_th, in_dex))
colnames(weal_th) <- c("Optimal Portfolio", "Equal Weight Portfolio")
dygraphs::dygraph(weal_th, main="Out-of-sample Optimal Portfolio Returns for Stocks") %>%
  dyOptions(colors=c("red", "blue"), strokeWidth=2) %>%
  dyEvent(index(last(rets_is[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=500)
# Create rectangular matrix with collinear columns
ran_dom <- matrix(rnorm(10*8), nc=10)
# Calculate covariance matrix
cov_mat <- cov(ran_dom)
# Calculate inverse of cov_mat - error
in_verse <- solve(cov_mat)
# Calculate regularized inverse of cov_mat
in_verse <- MASS::ginv(cov_mat)
# Verify inverse property of mat_rix
all.equal(cov_mat, cov_mat %*% in_verse %*% cov_mat)
# Perform eigen decomposition
ei_gen <- eigen(cov_mat)
eigen_vec <- ei_gen$vectors
eigen_val <- ei_gen$values
# Set tolerance for determining zero singular values
to_l <- sqrt(.Machine$double.eps)
# Calculate regularized inverse matrix
not_zero <- (eigen_val > (to_l * eigen_val[1]))
reg_inverse <- eigen_vec[, not_zero] %*%
  (t(eigen_vec[, not_zero]) / eigen_val[not_zero])
# Verify inverse property of mat_rix
all.equal(in_verse, reg_inverse)
# Calculate in-sample covariance matrix
cov_mat <- cov(rets_is)
ei_gen <- eigen(cov_mat)
eigen_vec <- ei_gen$vectors
eigen_val <- ei_gen$values
# Calculate regularized inverse of covariance matrix
max_eigen <- 21
in_verse <- eigen_vec[, 1:max_eigen] %*%
  (t(eigen_vec[, 1:max_eigen]) / ei_gen$values[1:max_eigen])
# Calculate portfolio weights
weight_s <- in_verse %*% colMeans(ex_cess["/2010"])
weight_s <- drop(weight_s/sqrt(sum(weight_s^2)))
names(weight_s) <- colnames(re_turns)
# Calculate portfolio returns
portf_is <- xts::xts(rets_is %*% weight_s, index(rets_is))
portf_os <- xts::xts(rets_os %*% weight_s, index(rets_os))
in_dex <- xts::xts(rowSums(re_turns)/sqrt(n_assets), index(re_turns))
# Plot cumulative portfolio returns
weal_th <- rbind(portf_is, portf_os)
weal_th <- weal_th*sd(in_dex)/sd(weal_th)
weal_th <- cumsum(cbind(weal_th, in_dex))
colnames(weal_th) <- c("Optimal Portfolio", "Equal Weight Portfolio")
dygraphs::dygraph(weal_th, main="Regularized Out-of-sample Optimal Portfolio Returns for Stocks") %>%
  dyOptions(colors=c("red", "blue"), strokeWidth=2) %>%
  dyEvent(index(last(rets_is[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=500)
# Shrink the in-sample returns to their mean
rets_mean <- colMeans(rets_is) - risk_free
al_pha <- 0.7
rets_mean <- (1 - al_pha)*rets_mean + al_pha*mean(rets_mean)
# Calculate portfolio weights
weight_s <- in_verse %*% rets_mean
weight_s <- drop(weight_s/sqrt(sum(weight_s^2)))
# Calculate portfolio returns
portf_is <- xts::xts(rets_is %*% weight_s, index(rets_is))
portf_os <- xts::xts(rets_os %*% weight_s, index(rets_os))
# Plot cumulative portfolio returns
weal_th <- rbind(portf_is, portf_os)
weal_th <- weal_th*sd(in_dex)/sd(weal_th)
weal_th <- cumsum(cbind(weal_th, in_dex))
colnames(weal_th) <- c("Optimal Portfolio", "Equal Weight Portfolio")
dygraphs::dygraph(weal_th, main="Out-of-sample Returns for Stocks With Regularization and Shrinkage") %>%
  dyOptions(colors=c("red", "blue"), strokeWidth=2) %>%
  dyEvent(index(last(rets_is[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=500)
library(RcppArmadillo)
# Source Rcpp functions from file
Rcpp::sourceCpp("C:/Develop/lecture_slides/scripts/calc_weights.cpp")
# Create random matrix of returns
mat_rix <- matrix(rnorm(300), nc=5)
# Regularized inverse of covariance matrix
max_eigen <- 4
ei_gen <- eigen(cov(mat_rix))
cov_inv <- ei_gen$vectors[, 1:max_eigen] %*%
  (t(ei_gen$vectors[, 1:max_eigen]) / ei_gen$values[1:max_eigen])
# Regularized inverse using RcppArmadillo
cov_inv_arma <- calc_inv(mat_rix, max_eigen)
all.equal(cov_inv, cov_inv_arma)
# Microbenchmark RcppArmadillo code
library(microbenchmark)
summary(microbenchmark(
  pure_r={
    ei_gen <- eigen(cov(mat_rix))
    ei_gen$vectors[, 1:max_eigen] %*%
(t(ei_gen$vectors[, 1:max_eigen]) / ei_gen$values[1:max_eigen])
  },
  r_cpp=calc_inv(mat_rix, max_eigen),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
# Calculate vector of monthly end points and start points
end_p <- rutils::calc_endpoints(re_turns, inter_val="months")
end_p <- end_p[end_p > 2*NCOL(re_turns)]
n_rows <- NROW(end_p)
look_back <- 24
start_p <- c(rep_len(0, look_back-1),
       end_p[1:(n_rows-look_back+1)])
# Perform loop over end points
rets_portf <- lapply(2:n_rows, function(i) {
    # Subset the ex_cess returns
    ex_cess <- ex_cess[start_p[i-1]:end_p[i-1], ]
    in_verse <- MASS::ginv(cov(ex_cess))
    # Calculate the maximum Sharpe ratio portfolio weights
    weight_s <- in_verse %*% colMeans(ex_cess)
    weight_s <- drop(weight_s/sqrt(sum(weight_s^2)))
    # Calculate the out-of-sample portfolio returns
    re_turns <- re_turns[(end_p[i-1]+1):end_p[i], ]
    xts::xts(re_turns %*% weight_s, index(re_turns))
})  # end lapply
rets_portf <- rutils::do_call(rbind, rets_portf)
# Plot cumulative strategy returns
in_dex <- xts::xts(rowSums(re_turns)/sqrt(n_assets), index(re_turns))
weal_th <- cumsum(na.omit(cbind(rets_portf, in_dex*sd(rets_portf)/sd(in_dex))))
colnames(weal_th) <- c("Rolling Portfolio Strategy", "Equal Weight Portfolio")
dygraphs::dygraph(weal_th, main="Rolling Portfolio Optimization Strategy") %>%
  dyOptions(colors=c("red", "blue"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
load("C:/Develop/lecture_slides/data/sp500_returns.RData")
# Overwrite NA values in re_turns
returns_100[1, is.na(returns_100[1, ])] <- 0
returns_100 <- zoo::na.locf(returns_100, na.rm=FALSE)
n_cols <- NCOL(returns_100) ; date_s <- index(returns_100)
# Define monthly end points
end_p <- rutils::calc_endpoints(returns_100, inter_val="months")
end_p <- end_p[end_p > (n_cols+1)]
n_rows <- NROW(end_p) ; look_back <- 12
start_p <- c(rep_len(0, look_back-1), end_p[1:(n_rows-look_back+1)])
end_p <- (end_p - 1)
start_p <- (start_p - 1)
start_p[start_p < 0] <- 0
al_pha <- 0.7 ; max_eigen <- 21
# Perform backtest in Rcpp
pnl_s <- HighFreq::back_test(typ_e="max_sharpe",
  ex_cess=returns_100, returns=returns_100,
  start_p=start_p, end_p=end_p,
  al_pha=al_pha, max_eigen=max_eigen)
# Calculate returns on equal weight portfolio
in_dex <- xts::xts(rowMeans(returns_100), index(returns_100))
# Plot cumulative strategy returns
weal_th <- cbind(pnl_s, in_dex, (pnl_s+in_dex)/2)
weal_th <- cumsum(na.omit(weal_th))
col_names <- c("Strategy", "Index", "Average")
colnames(weal_th) <- col_names
dygraphs::dygraph(weal_th[end_p], main="Rolling S&P500 Portfolio Optimization Strategy") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="red", strokeWidth=1) %>%
  dySeries(name=col_names[2], axis="y2", col="blue", strokeWidth=1) %>%
  dySeries(name=col_names[3], axis="y2", col="green", strokeWidth=2)
# Perform backtest over alphas
alpha_s <- seq(from=0.01, to=0.91, by=0.1)
pnl_s <- lapply(alpha_s, function(al_pha) {
  HighFreq::back_test(typ_e="max_sharpe",
  ex_cess=returns_100, returns=returns_100,
  start_p=start_p, end_p=end_p,
  al_pha=al_pha, max_eigen=max_eigen)
})  # end lapply
pro_file <- sapply(pnl_s, sum)
plot(x=alpha_s, y=pro_file, t="l", main="Strategy PnL as Function of Shrinkage Intensity Alpha",
  xlab="Shrinkage Intensity Alpha", ylab="pnl")
al_pha <- alpha_s[which.max(pro_file)]
pnl_s <- pnl_s[[which.max(pro_file)]]
# Perform backtest over max_eigens
max_eigens <- seq(from=3, to=40, by=2)
pnl_s <- lapply(max_eigens, function(max_eigen) {
  HighFreq::back_test(typ_e="max_sharpe",
    ex_cess=returns_100, returns=returns_100,
    start_p=start_p, end_p=end_p,
    al_pha=al_pha, max_eigen=max_eigen)
})  # end lapply
pro_file <- sapply(pnl_s, sum)
plot(x=max_eigens, y=pro_file, t="l", main="Strategy PnL as Function of Max_eigen",
  xlab="Max_eigen", ylab="pnl")
max_eigen <- max_eigens[which.max(pro_file)]
pnl_s <- pnl_s[[which.max(pro_file)]]
# Plot cumulative strategy returns
weal_th <- cbind(pnl_s, in_dex, (pnl_s+in_dex)/2)
weal_th <- cumsum(na.omit(weal_th))
col_names <- c("Strategy", "Index", "Average")
colnames(weal_th) <- col_names
dygraphs::dygraph(weal_th[end_p], main="Optimal Rolling S&P500 Portfolio Strategy") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="red", strokeWidth=1) %>%
  dySeries(name=col_names[2], axis="y2", col="blue", strokeWidth=1) %>%
  dySeries(name=col_names[3], axis="y2", col="green", strokeWidth=2)
# Perform backtest over look-backs
look_backs <- seq(from=3, to=24, by=1)
pnl_s <- lapply(look_backs, function(look_back) {
  start_p <- c(rep_len(0, look_back-1), end_p[1:(n_rows-look_back+1)])
  start_p <- (start_p - 1)
  start_p[start_p < 0] <- 0
  HighFreq::back_test(typ_e="max_sharpe",
    ex_cess=returns_100, returns=returns_100,
    start_p=start_p, end_p=end_p,
    al_pha=al_pha, max_eigen=max_eigen)
})  # end lapply
pro_file <- sapply(pnl_s, sum)
plot(x=look_backs, y=pro_file, t="l", main="Strategy PnL as Function of Look-back Interval",
  xlab="Look-back Interval", ylab="pnl")
look_back <- look_backs[which.max(pro_file)]
pnl_s <- pnl_s[[which.max(pro_file)]]
# Plot cumulative strategy returns
weal_th <- cbind(pnl_s, in_dex, (pnl_s+in_dex)/2)
weal_th <- cumsum(na.omit(weal_th))
col_names <- c("Strategy", "Index", "Average")
colnames(weal_th) <- col_names
dygraphs::dygraph(weal_th[end_p], main="Optimal Rolling S&P500 Portfolio Strategy") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="red", strokeWidth=1) %>%
  dySeries(name=col_names[2], axis="y2", col="blue", strokeWidth=1) %>%
  dySeries(name=col_names[3], axis="y2", col="green", strokeWidth=2)
