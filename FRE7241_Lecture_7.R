# Calculate a vector of daily VTI log returns
re_turns <- na.omit(rutils::etf_env$re_turns$VTI)
date_s <- index(re_turns)
re_turns <- as.numeric(re_turns)
n_rows <- NROW(re_turns)
# Define predictor matrix for forecasting
order_max <- 10
predic_tor <- sapply(1:order_max, rutils::lag_it, in_put=re_turns)
predic_tor <- cbind(rep(1, n_rows), predic_tor)
colnames(predic_tor) <- paste0("pred_", 1:NCOL(predic_tor))
res_ponse <- re_turns
# Calculate forecasts as function of the AR order
forecast_s <- lapply(2:NCOL(predic_tor), function(or_der) {
  # Calculate fitted coefficients
  in_verse <- MASS::ginv(predic_tor[, 1:or_der])
  coeff_fit <- drop(in_verse %*% res_ponse)
  # Calculate in-sample forecasts of re_turns
  drop(predic_tor[, 1:or_der] %*% coeff_fit)
})  # end lapply
names(forecast_s) <- paste0("p=", 2:NCOL(predic_tor))

# Calculate mean squared errors
ms_e <- sapply(forecast_s, function(x) {
  c(mse=mean((re_turns - x)^2), cor=cor(re_turns, x))
})  # end sapply
ms_e <- t(ms_e)
rownames(ms_e) <- names(forecast_s)
# Plot forecasting MSE
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
plot(x=2:NCOL(predic_tor), y=ms_e[, 1],
  xlab="AR(p) order", ylab="MSE", type="l", lwd=2,
  main="MSE of In-sample AR(p) Forecasting Model for VTI")

in_sample <- 1:(n_rows %/% 2)
out_sample <- (n_rows %/% 2 + 1):n_rows
# Calculate forecasts as function of the AR order
forecast_s <- lapply(2:NCOL(predic_tor), function(or_der) {
  # Calculate fitted coefficients
  in_verse <- MASS::ginv(predic_tor[in_sample, 1:or_der])
  coeff_fit <- drop(in_verse %*% res_ponse[in_sample])
  # Calculate out-of-sample forecasts of re_turns
  drop(predic_tor[out_sample, 1:or_der] %*% coeff_fit)
})  # end lapply
names(forecast_s) <- paste0("p=", 2:NCOL(predic_tor))

# Calculate mean squared errors
ms_e <- sapply(forecast_s, function(x) {
  c(mse=mean((re_turns[out_sample] - x)^2), cor=cor(re_turns[out_sample], x))
})  # end sapply
ms_e <- t(ms_e)
rownames(ms_e) <- names(forecast_s)
# Plot forecasting MSE
plot(x=2:NCOL(predic_tor), y=ms_e[, 1],
  xlab="AR(p) order", ylab="MSE", type="l", lwd=2,
  main="MSE of Out-of-sample AR(p) Forecasting Model for VTI")

# Calculate out-of-sample PnLs
pnl_s <- sapply(forecast_s, function(x) {
  cumsum(sign(x)*re_turns[out_sample])
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

# Define predictor as a rolling sum
n_agg <- 5
predic_tor <- rutils::roll_sum(re_turns, look_back=n_agg)
# Shift the res_ponse forward out-of-sample
res_ponse <- rutils::lag_it(predic_tor, lagg=(-n_agg))
# Define predictor matrix for forecasting
predic_tor <- sapply(1+n_agg*(0:order_max), rutils::lag_it,
               in_put=predic_tor)
predic_tor <- cbind(rep(1, n_rows), predic_tor)
# Calculate forecasts as function of the AR order
forecast_s <- lapply(2:NCOL(predic_tor), function(or_der) {
  in_verse <- MASS::ginv(predic_tor[in_sample, 1:or_der])
  coeff_fit <- drop(in_verse %*% res_ponse[in_sample])
  drop(predic_tor[out_sample, 1:or_der] %*% coeff_fit)
})  # end lapply
names(forecast_s) <- paste0("p=", 2:NCOL(predic_tor))

# Calculate out-of-sample PnLs
pnl_s <- sapply(forecast_s, function(x) {
  cumsum(sign(x)*re_turns[out_sample])
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
  x <- rutils::roll_sum(x, look_back=n_agg)
  cumsum(sign(x)*re_turns[out_sample])
})  # end sapply
colnames(pnl_s) <- names(forecast_s)
pnl_s <- xts::xts(pnl_s, date_s[out_sample])

# Plot dygraph of out-of-sample PnLs
dygraphs::dygraph(pnl_s[, 1:4],
  main="Autoregressive Strategies Performance Using Rolling Average Forecasts") %>%
  dyOptions(colors=color_s, strokeWidth=2) %>%
  dyLegend(width=500)

# Calculate a vector of daily VTI log returns
re_turns <- na.omit(rutils::etf_env$re_turns$VTI)
date_s <- index(re_turns)
re_turns <- as.numeric(re_turns)
# Define predictor as a rolling sum
predic_tor <- rutils::roll_sum(re_turns, look_back=n_agg)
# Shift the res_ponse forward out-of-sample
res_ponse <- rutils::lag_it(predic_tor, lagg=(-n_agg))
# Define predictor matrix for forecasting
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
plot(forecast_s[(n_rows-look_back):n_rows], col="red",
     xlab="", ylab="", type="l", lwd=2,
     main="Rolling Forecasting Using AR(5) Model")
lines(re_turns[(n_rows-look_back):n_rows], col="blue", lwd=2)
legend(x="top", legend=c("re_turns", "forecasts"),
 col=c("blue", "red"), lty=1, lwd=6,
 cex=0.9, bg="white", bty="n")

# Define backtesting function
sim_forecasts <- function(res_ponse, predic_tor=res_ponse, n_agg=5,
                or_der=5, look_back=100) {
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
  rutils::roll_sum(forecast_s, look_back=n_agg)
}  # end sim_forecasts
# Simulate the rolling autoregressive forecasts
forecast_s <- sim_forecasts(re_turns, or_der=5, look_back=100)
c(mse=mean((re_turns - forecast_s)^2), cor=cor(re_turns, forecast_s))

look_backs <- seq(20, 600, 40)
library(parallel)  # Load package parallel
# Calculate number of available cores
n_cores <- detectCores() - 1
# Initialize compute cluster under Windows
clus_ter <- makeCluster(n_cores)
# clusterExport(clus_ter, c("star_t", "bar_rier"))
# Perform parallel loop under Windows
forecast_s <- parLapply(clus_ter, look_backs, sim_forecasts, res_ponse=re_turns,
                  predic_tor=re_turns, n_agg=5, or_der=5)
# Perform parallel bootstrap under Mac-OSX or Linux
forecast_s <- mclapply(look_backs, sim_forecasts, res_ponse=re_turns,
  predic_tor=re_turns, n_agg=5, or_der=5, mc.cores=n_cores)

# Calculate mean squared errors
ms_e <- sapply(forecast_s, function(x) {
  c(mse=mean((re_turns - x)^2), cor=cor(re_turns, x))
})  # end sapply
ms_e <- t(ms_e)
rownames(ms_e) <- look_backs
# Select optimal look_back interval
look_back <- look_backs[which.min(ms_e[, 1])]
# Plot forecasting MSE
plot(x=look_backs, y=ms_e[, 1],
  xlab="look-back", ylab="MSE", type="l", lwd=2,
  main="MSE of AR(5) Forecasting Model for VTI")

order_s <- 2:6
library(parallel)  # Load package parallel
# Calculate number of available cores
n_cores <- detectCores() - 1
# Initialize compute cluster under Windows
clus_ter <- makeCluster(n_cores)
# clusterExport(clus_ter, c("star_t", "bar_rier"))
# Perform parallel loop under Windows
forecast_s <- parLapply(clus_ter, order_s, sim_forecasts, res_ponse=re_turns,
                  predic_tor=re_turns, n_agg=5, look_back=look_back)
stopCluster(clus_ter)  # Stop R processes over cluster under Windows
# Perform parallel bootstrap under Mac-OSX or Linux
forecast_s <- mclapply(order_s, sim_forecasts, res_ponse=re_turns,
  predic_tor=re_turns, n_agg=5, look_back=look_back, mc.cores=n_cores)
stopCluster(clus_ter)  # Stop R processes over cluster under Windows

# Calculate mean squared errors
ms_e <- sapply(forecast_s, function(x) {
  c(mse=mean((re_turns - x)^2), cor=cor(re_turns, x))
})  # end sapply
ms_e <- t(ms_e)
rownames(ms_e) <- order_s
# Select optimal order parameter
or_der <- order_s[which.min(ms_e)]
# Plot forecasting MSE
plot(x=order_s, y=ms_e[, 1],
  xlab="or_der", ylab="MSE", type="l", lwd=2,
  main="MSE of Forecasting Model As Function of AR Order")

# Simulate the rolling autoregressive forecasts
forecast_s <- sim_forecasts(re_turns, or_der=or_der, look_back=look_back)
# Calculate strategy PnLs
pnl_s <- sign(forecast_s)*re_turns
pnl_s <- cbind(re_turns, pnl_s, (re_turns+pnl_s)/2)
colnames(pnl_s) <- c("VTI", "AR_Strategy", "Combined")
cor(pnl_s)
# Annualized Sharpe ratios of VTI and AR strategy
sqrt(252)*apply(pnl_s, 2, function (x) mean(x)/sd(x))
pnl_s <- xts::xts(pnl_s, date_s)
pnl_s <- cumsum(pnl_s)

# Plot the cumulative strategy PnLs
dygraphs::dygraph(pnl_s, main="Rolling Autoregressive Strategy") %>%
  dyOptions(colors=c("blue","red","green"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)

# Extract ETF returns
sym_bols <- c("VTI", "IEF", "DBC")
re_turns <- rutils::etf_env$re_turns[, sym_bols]
# Select rows with IEF data
re_turns <- re_turns[index(rutils::etf_env$IEF)]
# Copy over NA values
re_turns[1, is.na(re_turns[1, ])] <- 0
re_turns <- zoo::na.locf(re_turns, na.rm=FALSE)

# Define end of month end points
end_p <- rutils::calc_endpoints(re_turns, inter_val="months")
n_rows <- NROW(end_p)
# start points equal end points lagged by 12-month look-back interval
look_back <- 12
start_p <- c(rep_len(0, look_back-1),
  end_p[1:(n_rows - look_back + 1)])
# Calculate matrix of look-back intervals
look_backs <- cbind(start_p, end_p)
# Calculate matrix of look-forward intervals
look_fwds <- cbind(end_p + 1, rutils::lag_it(end_p, -1))
look_fwds[n_rows, 1] <- end_p[n_rows]
# Inspect the intervals
tail(cbind(look_backs, look_fwds))

# Define performance function as Sharpe ratio
perform_ance <- function(re_turns) sum(re_turns)/sd(re_turns)
# Calculate past performance over look-back intervals
pas_t <- apply(look_backs, 1, function(ep) {
  sapply(re_turns[ep[1]:ep[2]], perform_ance)
})  # end sapply
pas_t <- t(pas_t)
pas_t[is.na(pas_t)] <- 0
# Calculate future performance
fu_ture <- apply(look_fwds, 1, function(ep) {
  sapply(re_turns[ep[1]:ep[2]], sum)
})  # end sapply
fu_ture <- t(fu_ture)
fu_ture[is.na(fu_ture)] <- 0
# Weights proportional to past performance
weight_s <- pas_t
# weight_s[weight_s < 0] <- 0
# Scale weight_s so sum is equal to 1
# weight_s <- weight_s/rowSums(weight_s)
# Scale weight_s so sum of squares is equal to 1
weight_s <- weight_s/sqrt(rowSums(weight_s^2))
# Set NA values to zero
weight_s[is.na(weight_s)] <- 0
sum(is.na(weight_s))

# Calculate momentum profits and losses (returns)
pnl_s <- rowSums(weight_s*fu_ture)
# Lag the momentum returns and weights
# to correspond with end of future interval
pnl_s <- rutils::lag_it(pnl_s)
weight_s <- rutils::lag_it(weight_s)
# bid_offer equal to 10 bps for liquid ETFs
bid_offer <- 0.001
# Calculate transaction costs
weal_th <- cumsum(pnl_s)
cost_s <- 0.5*bid_offer*weal_th*
  rowSums(abs(rutils::diff_it(weight_s)))
weal_th <- cumsum(pnl_s - cost_s)
date_s <- index(re_turns[end_p])
weal_th <- xts::xts(weal_th[-1], date_s)

# Define all-weather benchmark
weights_aw <- c(0.30, 0.55, 0.15)
ret_aw <- re_turns %*% weights_aw
wealth_aw <- cumsum(ret_aw)
wealth_aw <- xts::xts(wealth_aw[end_p], date_s)
# Plot the Momentum strategy and benchmark
da_ta <- cbind(weal_th, wealth_aw)
colnames(da_ta) <- c("Momentum Strategy", "Benchmark")
dygraphs::dygraph(da_ta, main="Momentum Strategy") %>%
  dyAxis("y", label="Benchmark", independentTicks=TRUE) %>%
  dyAxis("y2", label="Momentum Strategy", independentTicks=TRUE) %>%
  dySeries(name="Momentum Strategy", axis="y2", label="Momentum Strategy", strokeWidth=2, col="red") %>%
  dySeries(name="Benchmark", axis="y", label="Benchmark", strokeWidth=2, col="blue")

# Define backtest functional
backtest_momentum <- function(re_turns,
                perform_ance=function(re_turns) (sum(re_turns)/sd(re_turns)),
                look_back=12, re_balance="months", bid_offer=0.001,
                end_p=rutils::calc_endpoints(re_turns, inter_val=re_balance),
                with_weights=FALSE, ...) {
  stopifnot("package:rutils" %in% search() || require("rutils", quietly=TRUE))
  # Define look-back and look-forward intervals
  n_rows <- NROW(end_p)
  start_p <- c(rep_len(0, look_back-1), end_p[1:(n_rows-look_back+1)])
  # Calculate look-back intervals
  look_backs <- cbind(start_p, end_p)
  # Calculate look-forward intervals
  look_fwds <- cbind(end_p + 1, rutils::lag_it(end_p, -1))
  look_fwds[n_rows, 1] <- end_p[n_rows]
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
pro_files <- sapply(look_backs, function(look_back) {
  pnl_s <- backtest_momentum(re_turns=re_turns, end_p=end_p,
    look_back=look_back, perform_ance=perform_ance)
  last(cumsum(pnl_s))
})  # end sapply
x11(width=6, height=5)
plot(x=look_backs, y=pro_files, t="l",
  main="Strategy PnL as function of look_back",
  xlab="look_back (months)", ylab="pnl")

look_back <- look_backs[which.max(pro_files)]
pnl_s <- backtest_momentum(re_turns=re_turns,
  look_back=look_back, end_p=end_p,
  perform_ance=perform_ance, with_weights=TRUE)
tail(pnl_s)
ret_mom <- as.numeric(pnl_s[, 1])
weal_th <- cumsum(ret_mom)
da_ta <- cbind(weal_th[-1], wealth_aw, 0.5*(weal_th[-1] + wealth_aw))
colnames(da_ta) <- c("Momentum Strategy", "All_weather", "Combined")

# Plot the Momentum strategy and benchmark
dygraphs::dygraph(da_ta, main="Momentum Strategy") %>%
  dyAxis("y", label="All_weather", independentTicks=TRUE) %>%
  dyAxis("y2", label="Momentum Strategy", independentTicks=TRUE) %>%
  dySeries(name="Momentum Strategy", axis="y2", label="Momentum Strategy", strokeWidth=2, col="red") %>%
  dySeries(name="All_weather", axis="y", label="All_weather", strokeWidth=2, col="blue")
# Or
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue")
quantmod::chart_Series(da_ta, theme=plot_theme, lwd=2,
       name="Momentum PnL")
legend("topleft", legend=colnames(da_ta),
  inset=0.1, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")

# Plot the momentum portfolio weights
weight_s <- pnl_s[, -1]
vt_i <- rutils::etf_env$price_s$VTI[date_s]
da_ta <- cbind(vt_i, weight_s[-1, ])
da_ta <- na.omit(da_ta)
colnames(da_ta)[2:NCOL(pnl_s)] <- paste0(colnames(weight_s), "_weight")
zoo::plot.zoo(da_ta, xlab=NULL, main="Momentum Weights")

# Calculate ETF betas
betas_etf <- sapply(re_turns, function(x)
  cov(re_turns$VTI, x)/var(x))
# Betas equal weights times ETF betas
beta_s <- weight_s %*% betas_etf
beta_s <- xts(beta_s[-1], order.by=date_s)
colnames(beta_s) <- "momentum_beta"
da_ta <- cbind(beta_s, rutils::etf_env$VTI[date_s, 4])
zoo::plot.zoo(da_ta,
  oma = c(3, 1, 3, 0), mar = c(0, 4, 0, 1),
  main="betas & VTI", xlab="")

# Open x11 for plotting and set parameters to reduce whitespace around plot
x11(width=6, height=5)
par(mar=c(4, 4, 3, 1), oma=c(0, 0, 0, 0))
# Merton-Henriksson test
vt_i <- rutils::diff_it(vt_i)/rutils::lag_it(vt_i)
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
text(x=0.05, y=0.15, paste("Treynor test t-value =", round(summary(mod_el)$coefficients["treynor", "t value"], 2)))

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
ret_aw <- rutils::diff_it(wealth_aw)/rutils::lag_it(wealth_aw)
ret_aw <- sd(ret_mom)*ret_aw/sd(ret_aw)
da_ta <- cbind(ret_mom, ret_aw, 0.5*(ret_mom + ret_aw))
colnames(da_ta) <- c("momentum", "all_weather", "combined")
# Calculate strategy annualized Sharpe ratios
apply(da_ta, MARGIN=2, function(x) {
  sqrt(12)*sum(x)/sd(x)/NROW(x)
})  # end apply
# Calculate strategy correlations
cor(da_ta)
# Calculate cumulative wealth
weal_th <- apply(da_ta, MARGIN=2,
  function(x) {cumsum(x)}
)  # end apply
weal_th <- xts::xts(weal_th, date_s)

# Plot ETF momentum strategy combined with All-Weather
dygraphs::dygraph(weal_th, main="ETF Momentum Strategy Combined with All-Weather") %>%
  dyOptions(colors=c("green", "blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Or
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("green", "blue", "red")
quantmod::chart_Series(da_ta, theme=plot_theme,
       name="ETF Momentum Strategy Combined with All-Weather")
legend("topleft", legend=colnames(da_ta),
  inset=0.1, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")

# Calculate rolling variance
look_back <- 252
vari_ance <- roll::roll_var(re_turns, width=look_back)
vari_ance <- zoo::na.locf(vari_ance, na.rm=FALSE)
vari_ance[is.na(vari_ance)] <- 0
# Calculate rolling Sharpe
pas_t <- roll::roll_mean(re_turns, width=look_back)
weight_s <- pas_t/sqrt(vari_ance)
weight_s[vari_ance == 0] <- 0
weight_s[1:look_back, ] <- 1
weight_s <- weight_s/sqrt(rowSums(weight_s^2))
weight_s[is.na(weight_s)] <- 0
weight_s <- rutils::lag_it(weight_s)
sum(is.na(weight_s))
# Calculate momentum profits and losses
pnl_s <- rowMeans(weight_s*re_turns)

# Calculate transaction costs
bid_offer <- 0.001
cost_s <- 0.5*bid_offer*rowSums(abs(rutils::diff_it(weight_s)))
weal_th <- cumsum(pnl_s - cost_s)
weal_th <- xts(weal_th, order.by=index(re_turns))
# Plot momentum and VTI
wealth_aw <- cumsum(re_turns %*% weights_aw)
da_ta <- cbind(wealth_aw, weal_th)
colnames(da_ta) <- c("all_weather", "momentum")
col_names <- colnames(da_ta)
dygraphs::dygraph(da_ta, main="Momentum vs All-Weather") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="blue") %>%
  dySeries(name=col_names[2], axis="y2", col="red")

# Define backtest functional for daily momentum strategy
# If tre_nd=(-1) then it backtests a mean reverting strategy
momentum_daily <- function(re_turns, look_back=252, bid_offer=0.001, tre_nd=1, ...) {
  stopifnot("package:quantmod" %in% search() || require("quantmod", quietly=TRUE))
  # Calculate rolling variance
  vari_ance <- roll::roll_var(re_turns, width=look_back)
  vari_ance <- zoo::na.locf(vari_ance, na.rm=FALSE)
  # vari_ance[is.na(vari_ance)] <- 0
  vari_ance[vari_ance <= 0] <- 1
  # Calculate rolling Sharpe
  pas_t <- roll::roll_mean(re_turns, width=look_back)
  pas_t[1:look_back, ] <- 1
  weight_s <- pas_t/sqrt(vari_ance)
  # weight_s[vari_ance == 0] <- 0
  weight_s[1:look_back, ] <- 1
  weight_s <- weight_s/sqrt(rowSums(weight_s^2))
  weight_s[is.na(weight_s)] <- 0
  weight_s <- rutils::lag_it(weight_s)
  # Calculate momentum profits and losses
  pnl_s <- tre_nd*rowMeans(weight_s*re_turns)
  # Calculate transaction costs
  cost_s <- 0.5*bid_offer*rowSums(abs(rutils::diff_it(weight_s)))
  cumsum(pnl_s - cost_s)
}  # end momentum_daily

# Backtest a daily ETF momentum strategy
source("C:/Develop/lecture_slides/scripts/back_test.R")
weal_th <- momentum_daily(look_back=252,
  re_turns=re_turns, bid_offer=bid_offer)
# Perform sapply loop over look_backs
look_backs <- seq(50, 300, by=50)
weal_th <- sapply(look_backs, momentum_daily,
  re_turns=re_turns, bid_offer=bid_offer)
colnames(weal_th) <- paste0("look_back=", look_backs)
weal_th <- xts(weal_th, index(re_turns))
tail(weal_th)

# Plot EWMA strategies with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(weal_th))
quantmod::chart_Series(weal_th,
  theme=plot_theme, name="Cumulative Returns of Daily ETF Momentum Strategies")
legend("bottomleft", legend=colnames(weal_th),
  inset=0.02, bg="white", cex=0.7, lwd=rep(6, NCOL(re_turns)),
  col=plot_theme$col$line.col, bty="n")

# Backtest a daily S&P500 momentum strategy
source("C:/Develop/lecture_slides/scripts/back_test.R")
load("C:/Develop/lecture_slides/data/sp500_returns.RData")
# Perform sapply loop over look_backs
look_backs <- seq(50, 300, by=50)
weal_th <- sapply(look_backs, momentum_daily,
  re_turns=returns_100, bid_offer=0)
colnames(weal_th) <- paste0("look_back=", look_backs)
weal_th <- xts(weal_th, index(re_turns))

# Plot daily S&P500 momentum strategies with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(weal_th))
quantmod::chart_Series(weal_th,
  theme=plot_theme, name="Cumulative Returns of S&P500 Momentum Strategies")
legend("bottomleft", legend=colnames(weal_th),
  inset=0.02, bg="white", cex=0.7, lwd=rep(6, NCOL(re_turns)),
  col=plot_theme$col$line.col, bty="n")

# Perform sapply loop over look_backs
look_backs <- seq(5, 50, by=5)
weal_th <- sapply(look_backs, momentum_daily,
  re_turns=returns_100, bid_offer=0, tre_nd=(-1))
colnames(weal_th) <- paste0("look_back=", look_backs)
weal_th <- xts(weal_th, index(price_s))

# Plot EWMA strategies with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(weal_th))
quantmod::chart_Series(weal_th,
  theme=plot_theme, name="Cumulative Returns of S&P500 Mean Reverting Strategies")
legend("topleft", legend=colnames(weal_th),
  inset=0.05, bg="white", cex=0.7, lwd=rep(6, NCOL(re_turns)),
  col=plot_theme$col$line.col, bty="n")

# Linear constraint
weight_s <- weight_s/sum(weight_s)
# Quadratic constraint
weight_s <- weight_s/sqrt(sum(weight_s^2))
# Box constraints
weight_s[weight_s > 1] <- 1
weight_s[weight_s < 0] <- 0

library(quantmod)
library(Rglpk)
# Vector of symbol names
sym_bols <- c("VTI", "IEF", "DBC")
n_weights <- NROW(sym_bols)
# Calculate mean returns
re_turns <- rutils::etf_env$re_turns[, sym_bols]
re_turns <- zoo::na.locf(re_turns, na.rm=FALSE)
re_turns <- na.omit(re_turns)
mean_rets <- colMeans(re_turns)
# Specify weight constraints
constraint_s <- matrix(c(rep(1, n_weights), 1, 1, 0),
                 nc=n_weights, byrow=TRUE)
direction_s <- c("==", "<=")
rh_s <- c(1, 0)
# Specify weight bounds (-1, 1) (default is c(0, Inf))
bound_s <-
  list(lower=list(ind=1:n_weights, val=rep(-1, n_weights)),
 upper=list(ind=1:n_weights, val=rep(1, n_weights)))
# Perform optimization
op_tim <- Rglpk::Rglpk_solve_LP(
  obj=mean_rets,
  mat=constraint_s,
  dir=direction_s,
  rhs=rh_s,
  bounds=bound_s,
  max=TRUE)
unlist(op_tim[1:2])

# Calculate covariance matrix of returns and its inverse
cov_mat <- cov(re_turns)
cov_inv <- solve(a=cov_mat)
u_nit <- rep(1, NCOL(cov_mat))
# Minimum variance weights with constraint
# weight_s <- solve(a=cov_mat, b=u_nit)
weight_s <- cov_inv %*% u_nit
weight_s <- weight_s / drop(t(u_nit) %*% weight_s)
# Minimum variance
t(weight_s) %*% cov_mat %*% weight_s
1/(t(u_nit) %*% cov_inv %*% u_nit)

# Calculate vector of mean returns
mean_rets <- colMeans(re_turns)
# Specify the target return
tar_get <- 1.5*mean(re_turns)
# Products of inverse with mean returns and unit vector
f_mat <- matrix(c(
  t(u_nit) %*% cov_inv %*% u_nit,
  t(u_nit) %*% cov_inv %*% mean_rets,
  t(mean_rets) %*% cov_inv %*% u_nit,
  t(mean_rets) %*% cov_inv %*% mean_rets), nc=2)
# Solve for the Lagrange multipliers
mult_s <- solve(a=f_mat, b=c(2, 2*tar_get))
# Calculate weights
weight_s <- drop(0.5*cov_inv %*% cbind(u_nit, mean_rets) %*% mult_s)
# Calculate constraints
all.equal(1, sum(weight_s))
all.equal(tar_get, sum(mean_rets*weight_s))

# Calculate portfolio return and standard deviation
portf_rets <- drop(re_turns %*% weight_s)
c(return=mean(portf_rets), sd=sd(portf_rets))
all.equal(mean(portf_rets), tar_get)
# Calculate portfolio variance
uu <- c(1, tar_get)
f_inv <- solve(f_mat)
all.equal(var(portf_rets), drop(t(uu) %*% f_inv %*% uu))
# Calculate vertex of variance parabola
weight_s <- drop(cov_inv %*% u_nit /
  drop(t(u_nit) %*% cov_inv %*% u_nit))
portf_rets <- drop(re_turns %*% weight_s)
v_rets <-
  drop(t(u_nit) %*% cov_inv %*% mean_rets /
  t(u_nit) %*% cov_inv %*% u_nit)
all.equal(mean(portf_rets), v_rets)
var_min <-
  drop(1/t(u_nit) %*% cov_inv %*% u_nit)
all.equal(var(portf_rets), var_min)

# Calculate efficient frontier
target_s <- v_rets*(1+seq(from=-1, to=1, by=0.1))
eff_front <- sapply(target_s, function(tar_get) {
  uu <- c(1, tar_get)
  sqrt(drop(t(uu) %*% f_inv %*% uu))
})  # end sapply
# Plot efficient frontier
x11(width=6, height=5)
plot(x=eff_front, y=target_s, t="l", col="blue", lwd=2,
     main="Efficient Frontier and Minimum Variance Portfolio",
     xlab="standard deviation", ylab="return")
points(x=sqrt(var_min), y=v_rets, col="green", lwd=6)
text(x=sqrt(var_min), y=v_rets, labels="minimum \nvariance",
     pos=4, cex=0.8)

# Calculate portfolio standard deviation
std_dev <- sqrt(drop(t(uu) %*% f_inv %*% uu))
# Calculate the slope of the tangent line
slop_e <- (std_dev*det(f_mat))/(f_mat[1, 1]*tar_get-f_mat[1, 2])
# Calculate the risk-free rate as intercept of the tangent line
risk_free <- tar_get - slop_e*std_dev
# Calculate the risk-free rate from target return
risk_free <- (tar_get*f_mat[1, 2]-f_mat[2, 2]) /
  (tar_get*f_mat[1, 1]-f_mat[1, 2])

# Plot efficient frontier
plot(x=eff_front, y=target_s, t="l", col="blue", lwd=2,
     xlim=c(0.0, max(eff_front)),
     main="Efficient Frontier and Tangency Portfolio",
     xlab="standard deviation", ylab="return")
# Plot minimum variance
points(x=sqrt(var_min), y=v_rets, col="green", lwd=6)
text(x=sqrt(var_min), y=v_rets, labels="minimum \nvariance",
     pos=4, cex=0.8)
# Plot tangent point
points(x=std_dev, y=tar_get, col="red", lwd=6)
text(x=std_dev, y=tar_get, labels="tangency\nportfolio", pos=2, cex=0.8)
# Plot risk-free point
points(x=0, y=risk_free, col="red", lwd=6)
text(x=0, y=risk_free, labels="risk-free", pos=4, cex=0.8)
# Plot tangent line
abline(a=risk_free, b=slop_e, lwd=2, col="green")

# Calculate excess re_turns
risk_free <- 0.03/252
ex_cess <- re_turns - risk_free
# Calculate covariance and inverse matrix
cov_mat <- cov(re_turns)
u_nit <- rep(1, NCOL(cov_mat))
cov_inv <- solve(a=cov_mat)
# Calculate mean excess returns
ex_cess <- sapply(ex_cess, mean)
# Weights of maximum Sharpe portfolio
# weight_s <- solve(a=cov_mat, b=re_turns)
weight_s <- cov_inv %*% ex_cess
weight_s <- weight_s/drop(t(u_nit) %*% weight_s)
# Sharpe ratios
sqrt(252)*sum(weight_s * ex_cess) /
  sqrt(drop(weight_s %*% cov_mat %*% weight_s))
sapply(re_turns - risk_free,
  function(x) sqrt(252)*mean(x)/sd(x))
weights_maxsharpe <- weight_s

library(quantmod)
# Calculate minimum variance weights
weight_s <- cov_inv %*% u_nit
weights_minvar <-
  weight_s / drop(t(u_nit) %*% weight_s)
# Calculate optimal portfolio returns
optim_rets <- xts(
  x=cbind(exp(cumsum(re_turns %*% weights_maxsharpe)),
    exp(cumsum(re_turns %*% weights_minvar))),
  order.by=index(re_turns))
colnames(optim_rets) <- c("maxsharpe", "minvar")
# Plot optimal portfolio returns, with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "green")
x11(width=6, height=5)
chart_Series(optim_rets, theme=plot_theme,
  name="Maximum Sharpe and
  Minimum Variance portfolios")
legend("top", legend=colnames(optim_rets), cex=0.8,
 inset=0.1, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")

x11(wid_th <- 6, hei_ght <- 6)
# Calculate minimum variance weights
weight_s <- cov_inv %*% u_nit
weight_s <- weight_s / drop(t(u_nit) %*% weight_s)
# Minimum standard deviation and return
std_dev <- sqrt(252*drop(weight_s %*% cov_mat %*% weight_s))
min_ret <- 252*sum(weight_s * mean_rets)
# Calculate maximum Sharpe portfolios
risk_free <- (min_ret * seq(-10, 10, by=0.1)^3)/252
eff_front <- sapply(risk_free, function(risk_free) {
  weight_s <- cov_inv %*% (mean_rets - risk_free)
  weight_s <- weight_s/drop(t(u_nit) %*% weight_s)
  # Portfolio return and standard deviation
  c(return=252*sum(weight_s * mean_rets),
    stddev=sqrt(252*drop(weight_s %*% cov_mat %*% weight_s)))
})  # end sapply
eff_front <- cbind(252*risk_free, t(eff_front))
colnames(eff_front)[1] <- "risk-free"
eff_front <- eff_front[is.finite(eff_front[, "stddev"]), ]
eff_front <- eff_front[order(eff_front[, "return"]), ]
# Plot maximum Sharpe portfolios
plot(x=eff_front[, "stddev"],
     y=eff_front[, "return"], t="l",
     xlim=c(0.0*std_dev, 3.0*std_dev),
     ylim=c(0.0*min_ret, 2.0*min_ret),
     main="Efficient Frontier and Capital Market Line",
     xlab="standard deviation", ylab="return")
points(x=eff_front[, "stddev"], y=eff_front[, "return"],
 col="red", lwd=3)

# Plot minimum variance portfolio
points(x=std_dev, y=min_ret, col="green", lwd=6)
text(std_dev, min_ret, labels="minimum \nvariance",
     pos=4, cex=0.8)
# Draw Capital Market Line
sor_ted <- sort(eff_front[, 1])
risk_free <-
  sor_ted[findInterval(x=0.5*min_ret, vec=sor_ted)]
points(x=0, y=risk_free, col="blue", lwd=6)
text(x=0, y=risk_free, labels="risk-free",
     pos=4, cex=0.8)
in_dex <- match(risk_free, eff_front[, 1])
points(x=eff_front[in_dex, "stddev"],
 y=eff_front[in_dex, "return"],
 col="blue", lwd=6)
text(x=eff_front[in_dex, "stddev"],
     y=eff_front[in_dex, "return"],
     labels="market portfolio",
     pos=2, cex=0.8)
sharp_e <- (eff_front[in_dex, "return"]-risk_free)/
  eff_front[in_dex, "stddev"]
abline(a=risk_free, b=sharp_e, col="blue", lwd=2)
text(x=0.7*eff_front[in_dex, "stddev"],
     y=0.7*eff_front[in_dex, "return"]+0.01,
     labels="Capital Market Line", pos=2, cex=0.8,
     srt=45*atan(sharp_e*hei_ght/wid_th)/(0.25*pi))

# Calculate random portfolios
n_portf <- 1000
ret_sd <- sapply(1:n_portf, function(in_dex) {
  weight_s <- runif(n_weights-1, min=-0.25, max=1.0)
  weight_s <- c(weight_s, 1-sum(weight_s))
  # Portfolio return and standard deviation
  c(return=252*sum(weight_s * mean_rets),
    stddev=sqrt(252*drop(weight_s %*% cov_mat %*% weight_s)))
})  # end sapply
# Plot scatterplot of random portfolios
x11(wid_th <- 6, hei_ght <- 6)
plot(x=ret_sd["stddev", ], y=ret_sd["return", ],
     main="Efficient Frontier and Random Portfolios",
     xlim=c(0.5*std_dev, 0.8*max(ret_sd["stddev", ])),
     xlab="standard deviation", ylab="return")
# Plot maximum Sharpe portfolios
lines(x=eff_front[, "stddev"],
     y=eff_front[, "return"], lwd=2)
points(x=eff_front[, "stddev"], y=eff_front[, "return"],
 col="red", lwd=3)
# Plot minimum variance portfolio
points(x=std_dev, y=min_ret, col="green", lwd=6)
text(std_dev, min_ret, labels="minimum\nvariance",
     pos=2, cex=0.8)
# Plot market portfolio
points(x=eff_front[in_dex, "stddev"],
 y=eff_front[in_dex, "return"], col="green", lwd=6)
text(x=eff_front[in_dex, "stddev"],
     y=eff_front[in_dex, "return"],
     labels="market\nportfolio",
     pos=2, cex=0.8)

# Plot individual assets
points(x=sqrt(252*diag(cov_mat)),
 y=252*mean_rets, col="blue", lwd=6)
text(x=sqrt(252*diag(cov_mat)), y=252*mean_rets,
     labels=names(mean_rets),
     col="blue", pos=1, cex=0.8)

risk_free <- 0.03
re_turns <- c(asset1=0.05, asset2=0.06)
std_devs <- c(asset1=0.4, asset2=0.5)
cor_rel <- 0.6
cov_mat <- matrix(c(1, cor_rel, cor_rel, 1), nc=2)
cov_mat <- t(t(std_devs*cov_mat)*std_devs)
weight_s <- seq(from=-1, to=2, length.out=31)
weight_s <- cbind(weight_s, 1-weight_s)
portf_rets <- weight_s %*% re_turns
portf_sd <-
  sqrt(rowSums(weight_s * (weight_s %*% cov_mat)))
sharpe_ratios <- (portf_rets-risk_free)/portf_sd
in_dex <- which.max(sharpe_ratios)
max_Sharpe <- max(sharpe_ratios)
# Plot efficient frontier
x11(wid_th <- 6, hei_ght <- 5)
par(mar=c(3,3,2,1)+0.1, oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
plot(portf_sd, portf_rets, t="l",
 main=paste0("Efficient frontier and CML for two assets\ncorrelation = ", 100*cor_rel, "%"),
 xlab="standard deviation", ylab="return",
 lwd=2, col="orange",
 xlim=c(0, max(portf_sd)),
 ylim=c(0.02, max(portf_rets)))
# Add Market Portfolio (maximum Sharpe ratio portfolio)
points(portf_sd[in_dex], portf_rets[in_dex],
 col="blue", lwd=3)
text(x=portf_sd[in_dex], y=portf_rets[in_dex],
     labels=paste(c("market portfolio\n",
 structure(c(weight_s[in_dex], 1-weight_s[in_dex]),
         names=names(re_turns))), collapse=" "),
     pos=2, cex=0.8)

# Plot individual assets
points(std_devs, re_turns, col="green", lwd=3)
text(std_devs, re_turns, labels=names(re_turns), pos=4, cex=0.8)
# Add point at risk-free rate and draw Capital Market Line
points(x=0, y=risk_free, col="blue", lwd=3)
text(0, risk_free, labels="risk-free\nrate", pos=4, cex=0.8)
abline(a=risk_free, b=max_Sharpe, lwd=2, col="blue")
range_s <- par("usr")
text(portf_sd[in_dex]/2, (portf_rets[in_dex]+risk_free)/2,
     labels="Capital Market Line", cex=0.8, , pos=3,
     srt=45*atan(max_Sharpe*(range_s[2]-range_s[1])/
             (range_s[4]-range_s[3])*
             hei_ght/wid_th)/(0.25*pi))

# Plot portfolios in x11() window
x11(wid_th <- 6, hei_ght <- 5)
par(oma=c(0, 0, 0, 0), mar=c(3,3,2,1)+0.1, mgp=c(2, 1, 0), cex.lab=1.0, cex.axis=1.0, cex.main=1.0, cex.sub=1.0)
# Vector of symbol names
sym_bols <- c("VTI", "IEF")
# Matrix of portfolio weights
weight_s <- seq(from=-1, to=2, length.out=31)
weight_s <- cbind(weight_s, 1-weight_s)
# Calculate portfolio returns and volatilities
re_turns <- rutils::etf_env$re_turns[, sym_bols]
ret_sd <- re_turns %*% t(weight_s)
ret_sd <- cbind(252*colMeans(ret_sd),
  sqrt(252)*matrixStats::colSds(ret_sd))
colnames(ret_sd) <- c("returns", "stddev")
risk_free <- 0.06
ret_sd <- cbind(ret_sd,
  (ret_sd[, "returns"]-risk_free)/ret_sd[, "stddev"])
colnames(ret_sd)[3] <- "Sharpe"
in_dex <- which.max(ret_sd[, "Sharpe"])
max_Sharpe <- ret_sd[in_dex, "Sharpe"]
plot(x=ret_sd[, "stddev"], y=ret_sd[, "returns"],
     main="Stock and Bond portfolios", t="l",
     xlim=c(0, 0.7*max(ret_sd[, "stddev"])), ylim=c(0, max(ret_sd[, "returns"])),
     xlab="standard deviation", ylab="return")
# Add blue point for market portfolio
points(x=ret_sd[in_dex, "stddev"], y=ret_sd[in_dex, "returns"], col="blue", lwd=6)
text(x=ret_sd[in_dex, "stddev"], y=ret_sd[in_dex, "returns"],
     labels=paste(c("market portfolio\n", structure(c(weight_s[in_dex, 1], weight_s[in_dex, 2]), names=sym_bols)), collapse=" "),
     pos=3, cex=0.8)

# Plot individual assets
mean_rets <- 252*sapply(re_turns, mean)
std_devs <- sqrt(252)*sapply(re_turns, sd)
points(std_devs, mean_rets, col="green", lwd=6)
text(std_devs, mean_rets, labels=names(re_turns), pos=2, cex=0.8)
# Add point at risk-free rate and draw Capital Market Line
points(x=0, y=risk_free, col="blue", lwd=6)
text(0, risk_free, labels="risk-free", pos=4, cex=0.8)
abline(a=risk_free, b=max_Sharpe, col="blue", lwd=2)
range_s <- par("usr")
text(max(ret_sd[, "stddev"])/3, 0.75*max(ret_sd[, "returns"]),
     labels="Capital Market Line", cex=0.8, , pos=3,
     srt=45*atan(max_Sharpe*(range_s[2]-range_s[1])/
             (range_s[4]-range_s[3])*
             hei_ght/wid_th)/(0.25*pi))

# Plot portfolios in x11() window
x11(wid_th <- 6, hei_ght <- 5)
# Calculate cumulative returns of VTI and IEF
optim_rets <- lapply(re_turns,
  function(re_turns) exp(cumsum(re_turns)))
optim_rets <- rutils::do_call(cbind, optim_rets)
# Calculate market portfolio returns
optim_rets <- cbind(exp(cumsum(re_turns %*%
    c(weight_s[in_dex], 1-weight_s[in_dex]))),
  optim_rets)
colnames(optim_rets)[1] <- "market"
# Plot market portfolio with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue", "green")
chart_Series(optim_rets, theme=plot_theme,
       name="Market portfolio for stocks and bonds")
legend("top", legend=colnames(optim_rets),
 cex=0.8, inset=0.1, bg="white", lty=1,
 lwd=6, col=plot_theme$col$line.col, bty="n")

x11(width=6, height=4)
par(mar=c(3, 2, 1, 0), oma=c(0, 0, 0, 0))
# VTI percentage returns
re_turns <- rutils::diff_it(log(quantmod::Cl(rutils::etf_env$VTI)))
conf_level <- 0.1
va_r <- quantile(re_turns, conf_level)
c_var <- mean(re_turns[re_turns < va_r])
# Or
sort_ed <- sort(as.numeric(re_turns))
in_dex <- round(conf_level*NROW(re_turns))
va_r <- sort_ed[in_dex]
c_var <- mean(sort_ed[1:in_dex])
# Plot histogram of VTI returns
min_var <- (-0.05)
histo_gram <- hist(re_turns, col="lightgrey",
  xlab="returns", breaks=100, xlim=c(min_var, 0.01),
  ylab="frequency", freq=FALSE, main="VTI Returns Histogram")

# Plot density of losses
densi_ty <- density(re_turns, adjust=1.5)
lines(densi_ty, lwd=3, col="blue")
# Add line for VaR
abline(v=va_r, col="red", lwd=3)
y_max <- max(densi_ty$y)
text(x=va_r, y=2*y_max/3, labels="VaR", lwd=2, pos=2)
# Add shading for CVaR
rang_e <- (densi_ty$x < va_r) & (densi_ty$x > min_var)
polygon(
  c(min_var, densi_ty$x[rang_e], va_r),
  c(0, densi_ty$y[rang_e], 0),
  col=rgb(1, 0, 0,0.5), border=NA)
text(x=1.5*va_r, y=y_max/7, labels="CVaR", lwd=2, pos=2)

library(rutils)  # Load rutils
library(Rglpk)
# Vector of symbol names and returns
sym_bols <- c("VTI", "IEF", "DBC")
n_weights <- NROW(sym_bols)
re_turns <- rutils::etf_env$re_turns[((NROW(re_turns)-6):NROW(re_turns)), sym_bols]
mean_rets <- colMeans(re_turns)
conf_level <- 0.05
r_min <- 0 ; w_min <- 0 ; w_max <- 1
weight_sum <- 1
n_cols <- NCOL(re_turns) # number of assets
n_rows <- NROW(re_turns) # number of rows
# Creat objective vector
obj_vector <- c(numeric(n_cols), rep(-1/(conf_level*n_rows), n_rows), -1)
# Specify weight constraints
constraint_s <- rbind(
  cbind(rbind(1, mean_rets),
  matrix(data=0, nrow=2, ncol=(n_rows+1))),
  cbind(coredata(re_turns), diag(n_rows), 1))
rh_s <- c(weight_sum, r_min, rep(0, n_rows))
direction_s <- c("==", ">=", rep(">=", n_rows))
# Specify weight bounds
bound_s <- list(
  lower=list(ind=1:n_cols, val=rep(w_min, n_cols)),
  upper=list(ind=1:n_cols, val=rep(w_max, n_cols)))
# Perform optimization
op_tim <- Rglpk_solve_LP(obj=obj_vector, mat=constraint_s, dir=direction_s, rhs=rh_s, types=rep("C", NROW(obj_vector)), max=T, bounds=bound_s)
op_tim$solution
constraint_s %*% op_tim$solution
obj_vector %*% op_tim$solution
as.numeric(op_tim$solution[1:n_cols])

# Calculate daily percentage re_turns
sym_bols <- c("VTI", "IEF", "DBC")
re_turns <- rutils::etf_env$re_turns[, sym_bols]
# Create initial vector of portfolio weights
weight_s <- rep(1, NROW(sym_bols))
names(weight_s) <- sym_bols
# Objective equal to minus Sharpe ratio
object_ive <- function(weight_s, re_turns) {
  portf_rets <- re_turns %*% weight_s
  if (sd(portf_rets) == 0)
    return(0)
  else
    return(-mean(portf_rets)/sd(portf_rets))
}  # end object_ive
# Objective for equal weight portfolio
object_ive(weight_s, re_turns=re_turns)
op_tim <- unlist(optimize(
  f=function(weight)
    object_ive(c(1, 1, weight), re_turns=re_turns),
  interval=c(-4, 1)))
# Vectorize objective function with respect to third weight
vec_object <- function(weights) sapply(weights,
  function(weight) object_ive(c(1, 1, weight),
    re_turns=re_turns))
# Or
vec_object <- Vectorize(FUN=function(weight)
    object_ive(c(1, 1, weight), re_turns=re_turns),
  vectorize.args="weight")  # end Vectorize
vec_object(1)
vec_object(1:3)

x11(width=6, height=5)
par(oma=c(1, 1, 1, 1), mgp=c(2, 1, 0), mar=c(3, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# Plot objective function with respect to third weight
curve(expr=vec_object,
      type="l", xlim=c(-4.0, 1.0),
      xlab=paste("weight of", names(weight_s[3])),
      ylab="", lwd=2)
title(main="Objective Function", line=-1)  # Add title
points(x=op_tim[1], y=op_tim[2], col="green", lwd=6)
text(x=op_tim[1], y=op_tim[2],
     labels="minimum objective", pos=4, cex=0.8)

#below is simplified code for plotting objective function
# Create vector of DBC weights
weight_s <- seq(from=-4, to=1, by=0.1)
obj_val <- sapply(weight_s,
  function(weight) object_ive(c(1, 1, weight)))
plot(x=weight_s, y=obj_val, t="l",
      xlab="weight of DBC", ylab="", lwd=2)
title(main="Objective Function", line=-1)  # Add title
points(x=op_tim[1], y=op_tim[2], col="green", lwd=6)
text(x=op_tim[1], y=op_tim[2],
     labels="minimum objective", pos=4, cex=0.8)

# Vectorize function with respect to all weights
vec_object <- Vectorize(
  FUN=function(w1, w2, w3) object_ive(c(w1, w2, w3)),
  vectorize.args=c("w2", "w3"))  # end Vectorize
# Calculate objective on 2-d (w2 x w3) parameter grid
w2 <- seq(-3, 7, length=50)
w3 <- seq(-5, 5, length=50)
grid_object <- outer(w2, w3, FUN=vec_object, w1=1)
rownames(grid_object) <- round(w2, 2)
colnames(grid_object) <- round(w3, 2)
# Perspective plot of objective function
persp(w2, w3, -grid_object,
theta=45, phi=30, shade=0.5,
col=rainbow(50), border="green",
main="objective function")

# Interactive perspective plot of objective function
library(rgl)
rgl::persp3d(z=-grid_object, zlab="objective",
  col="green", main="objective function")
rgl::persp3d(
  x=function(w2, w3) {-vec_object(w1=1, w2, w3)},
  xlim=c(-3, 7), ylim=c(-5, 5),
  col="green", axes=FALSE)

# Optimization to find weights with maximum Sharpe ratio
op_tim <- optim(par=weight_s,
             fn=object_ive,
             re_turns=re_turns,
             method="L-BFGS-B",
             upper=c(1.1, 10, 10),
             lower=c(0.9, -10, -10))
# Optimal parameters
op_tim$par
op_tim$par <- op_tim$par/sum(op_tim$par)
# Optimal Sharpe ratio
-object_ive(op_tim$par)

x11(width=6, height=5)
par(oma=c(1, 1, 1, 0), mgp=c(2, 1, 0), mar=c(2, 1, 2, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# Plot in two vertical panels
layout(matrix(c(1,2), 2),
 widths=c(1,1), heights=c(1,3))
# barplot of optimal portfolio weights
barplot(op_tim$par, col=c("red", "green", "blue"),
  main="Optimized portfolio weights")
# Calculate cumulative returns of VTI, IEF, DBC
cum_rets <- lapply(re_turns,
  function(re_turns) exp(cumsum(re_turns)))
cum_rets <- rutils::do_call(cbind, cum_rets)
# Calculate optimal portfolio returns with VTI, IEF, DBC
optim_rets <- cbind(
  exp(cumsum(re_turns %*% op_tim$par)),
  cum_rets)
colnames(optim_rets)[1] <- "optim_rets"
# Plot optimal returns with VTI, IEF, DBC
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("black", "red", "green", "blue")
chart_Series(optim_rets, theme=plot_theme,
       name="Optimized portfolio performance")
legend("top", legend=colnames(optim_rets), cex=0.8,
 inset=0.1, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")
# Or plot non-compounded (simple) cumulative returns
PerformanceAnalytics::chart.CumReturns(
  cbind(re_turns %*% op_tim$par, re_turns),
  lwd=2, ylab="", legend.loc="topleft", main="")

risk_free <- 0.03
re_turns <- c(asset1=0.05, asset2=0.06)
std_devs <- c(asset1=0.4, asset2=0.5)
cor_rel <- 0.6
cov_mat <- matrix(c(1, cor_rel, cor_rel, 1), nc=2)
cov_mat <- t(t(std_devs*cov_mat)*std_devs)
library(quadprog)
# Minimum variance weights without constraints
op_tim <- solve.QP(Dmat=2*cov_mat,
            dvec=rep(0, 2),
            Amat=matrix(0, nr=2, nc=1),
            bvec=0)
# Minimum variance weights sum equal to 1
op_tim <- solve.QP(Dmat=2*cov_mat,
            dvec=rep(0, 2),
            Amat=matrix(1, nr=2, nc=1),
            bvec=1)
# Optimal value of objective function
t(op_tim$solution) %*% cov_mat %*% op_tim$solution
Perform simple optimization for reference
# Objective function for simple optimization
object_ive <- function(x) {
  x <- c(x, 1-x)
  t(x) %*% cov_mat %*% x
}  # end object_ive
unlist(optimize(f=object_ive, interval=c(-1, 2)))

# Calculate daily percentage re_turns
sym_bols <- c("VTI", "IEF", "DBC")
re_turns <- rutils::etf_env$re_turns[, sym_bols]
# Calculate the covariance matrix
cov_mat <- cov(re_turns)
# Minimum variance weights, with sum equal to 1
op_tim <- quadprog::solve.QP(Dmat=2*cov_mat,
            dvec=numeric(3),
            Amat=matrix(1, nr=3, nc=1),
            bvec=1)
# Minimum variance, maximum returns
op_tim <- quadprog::solve.QP(Dmat=2*cov_mat,
            dvec=apply(0.1*re_turns, 2, mean),
            Amat=matrix(1, nr=3, nc=1),
            bvec=1)
# Minimum variance positive weights, sum equal to 1
a_mat <- cbind(matrix(1, nr=3, nc=1),
       diag(3), -diag(3))
b_vec <- c(1, rep(0, 3), rep(-1, 3))
op_tim <- quadprog::solve.QP(Dmat=2*cov_mat,
            dvec=numeric(3),
            Amat=a_mat,
            bvec=b_vec,
            meq=1)

# Rastrigin function with vector argument for optimization
rastri_gin <- function(vec_tor, pa_ram=25){
  sum(vec_tor^2 - pa_ram*cos(vec_tor))
}  # end rastri_gin
vec_tor <- c(pi/6, pi/6)
rastri_gin(vec_tor=vec_tor)
library(DEoptim)
Optimize rastri_gin using DEoptim
op_tim <-  DEoptim(rastri_gin,
  upper=c(6, 6), lower=c(-6, -6),
  DEoptim.control(trace=FALSE, itermax=50))
# Optimal parameters and value
op_tim$optim$bestmem
rastri_gin(op_tim$optim$bestmem)
summary(op_tim)
plot(op_tim)

# Calculate daily percentage re_turns
re_turns <- rutils::etf_env$re_turns[, sym_bols]
# Objective equal to minus Sharpe ratio
object_ive <- function(weight_s, re_turns) {
  portf_rets <- re_turns %*% weight_s
  if (sd(portf_rets) == 0)
    return(0)
  else
    return(-mean(portf_rets)/sd(portf_rets))
}  # end object_ive
# Perform optimization using DEoptim
op_tim <- DEoptim::DEoptim(fn=object_ive,
  upper=rep(10, NCOL(re_turns)),
  lower=rep(-10, NCOL(re_turns)),
  re_turns=re_turns,
  control=list(trace=FALSE, itermax=100, parallelType=1))
weight_s <- op_tim$optim$bestmem/sum(abs(op_tim$optim$bestmem))
names(weight_s) <- colnames(re_turns)

# Objective with shrinkage penalty
object_ive <- function(weight_s, re_turns, lamb_da, al_pha) {
  portf_rets <- re_turns %*% weight_s
  if (sd(portf_rets) == 0)
    return(0)
  else {
    penal_ty <- lamb_da*((1-al_pha)*sum(weight_s^2) +
al_pha*sum(abs(weight_s)))
    return(-mean(portf_rets)/sd(portf_rets) + penal_ty)
  }
}  # end object_ive
# Objective for equal weight portfolio
weight_s <- rep(1, NROW(sym_bols))
names(weight_s) <- sym_bols
lamb_da <- 0.5 ; al_pha <- 0.5
object_ive(weight_s, re_turns=re_turns,
  lamb_da=lamb_da, al_pha=al_pha)
# Perform optimization using DEoptim
op_tim <- DEoptim::DEoptim(fn=object_ive,
  upper=rep(10, NCOL(re_turns)),
  lower=rep(-10, NCOL(re_turns)),
  re_turns=re_turns,
  lamb_da=lamb_da,
  al_pha=al_pha,
  control=list(trace=FALSE, itermax=100, parallelType=1))
weight_s <- op_tim$optim$bestmem/sum(abs(op_tim$optim$bestmem))
names(weight_s) <- colnames(re_turns)

library(rutils)
# Select all the ETF symbols except "VXX", "SVXY" and "MTUM"
sym_bols <- colnames(rutils::etf_env$re_turns)
sym_bols <- sym_bols[!(sym_bols %in% c("VXX", "SVXY", "MTUM"))]
# Extract columns of rutils::etf_env$re_turns and overwrite NA values
re_turns <- rutils::etf_env$re_turns[, sym_bols]
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
portf_is <- xts(rets_is %*% weight_s, index(rets_is))
in_dex <- xts(rowSums(rets_is)/sqrt(n_assets), index(rets_is))
portf_is <- portf_is*sd(in_dex)/sd(portf_is)
# Plot cumulative portfolio returns
weal_th <- cumsum(cbind(portf_is, in_dex))
colnames(weal_th) <- c("Optimal Portfolio", "Equal Weight Portfolio")
dygraphs::dygraph(weal_th, main="In-sample Optimal Portfolio Returns") %>%
  dyOptions(colors=c("red","blue"), strokeWidth=2) %>%
  dyLegend(width=500)

# Out-of-sample portfolio returns
rets_os <- re_turns["2015/"]
portf_os <- xts(rets_os %*% weight_s, index(rets_os))
in_dex <- xts(rowSums(rets_os)/sqrt(n_assets), index(rets_os))
portf_os <- portf_os*sd(in_dex)/sd(portf_os)

# Plot cumulative portfolio returns
weal_th <- cumsum(cbind(portf_os, in_dex))
colnames(weal_th) <- c("Optimal Portfolio", "Equal Weight Portfolio")
dygraphs::dygraph(weal_th, main="Out-of-sample Optimal Portfolio Returns") %>%
  dyOptions(colors=c("red","blue"), strokeWidth=2) %>%
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
portf_is <- xts(rets_is %*% weight_s, index(rets_is))
portf_os <- xts(rets_os %*% weight_s, index(rets_os))
in_dex <- xts(rowSums(re_turns)/sqrt(n_assets), index(re_turns))

# Plot cumulative portfolio returns
weal_th <- rbind(portf_is, portf_os)
weal_th <- weal_th*sd(in_dex)/sd(weal_th)
weal_th <- cumsum(cbind(weal_th, in_dex))
colnames(weal_th) <- c("Optimal Portfolio", "Equal Weight Portfolio")
dygraphs::dygraph(weal_th, main="Out-of-sample Optimal Portfolio Returns for Stocks") %>%
  dyOptions(colors=c("red","blue"), strokeWidth=2) %>%
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
portf_is <- xts(rets_is %*% weight_s, index(rets_is))
portf_os <- xts(rets_os %*% weight_s, index(rets_os))
in_dex <- xts(rowSums(re_turns)/sqrt(n_assets), index(re_turns))

# Plot cumulative portfolio returns
weal_th <- rbind(portf_is, portf_os)
weal_th <- weal_th*sd(in_dex)/sd(weal_th)
weal_th <- cumsum(cbind(weal_th, in_dex))
colnames(weal_th) <- c("Optimal Portfolio", "Equal Weight Portfolio")
dygraphs::dygraph(weal_th, main="Regularized Out-of-sample Optimal Portfolio Returns for Stocks") %>%
  dyOptions(colors=c("red","blue"), strokeWidth=2) %>%
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
portf_is <- xts(rets_is %*% weight_s, index(rets_is))
portf_os <- xts(rets_os %*% weight_s, index(rets_os))
# Plot cumulative portfolio returns
weal_th <- rbind(portf_is, portf_os)
weal_th <- weal_th*sd(in_dex)/sd(weal_th)
weal_th <- cumsum(cbind(weal_th, in_dex))
colnames(weal_th) <- c("Optimal Portfolio", "Equal Weight Portfolio")
dygraphs::dygraph(weal_th, main="Out-of-sample Returns for Stocks With Regularization and Shrinkage") %>%
  dyOptions(colors=c("red","blue"), strokeWidth=2) %>%
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
    xts(re_turns %*% weight_s, index(re_turns))
})  # end lapply
rets_portf <- rutils::do_call(rbind, rets_portf)
# Plot cumulative strategy returns
in_dex <- xts(rowSums(re_turns)/sqrt(n_assets), index(re_turns))
weal_th <- cumsum(na.omit(cbind(rets_portf, in_dex*sd(rets_portf)/sd(in_dex))))
colnames(weal_th) <- c("Rolling Portfolio Strategy", "Equal Weight Portfolio")
dygraphs::dygraph(weal_th, main="Rolling Portfolio Optimization Strategy") %>%
  dyOptions(colors=c("red","blue"), strokeWidth=2) %>%
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
  ex_cess=returns_100, re_turns=returns_100,
  start_p=start_p, end_p=end_p,
  al_pha=al_pha, max_eigen=max_eigen)
# Calculate returns on equal weight portfolio
in_dex <- xts(rowMeans(returns_100), index(returns_100))
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
  ex_cess=returns_100, re_turns=returns_100,
  start_p=start_p, end_p=end_p,
  al_pha=al_pha, max_eigen=max_eigen)
})  # end lapply
pro_files <- sapply(pnl_s, sum)
plot(x=alpha_s, y=pro_files, t="l", main="Strategy PnL as Function of Shrinkage Intensity Alpha",
  xlab="Shrinkage Intensity Alpha", ylab="pnl")
al_pha <- alpha_s[which.max(pro_files)]
pnl_s <- pnl_s[[which.max(pro_files)]]
# Perform backtest over max_eigens
max_eigens <- seq(from=3, to=40, by=2)
pnl_s <- lapply(max_eigens, function(max_eigen) {
  HighFreq::back_test(typ_e="max_sharpe",
    ex_cess=returns_100, re_turns=returns_100,
    start_p=start_p, end_p=end_p,
    al_pha=al_pha, max_eigen=max_eigen)
})  # end lapply
pro_files <- sapply(pnl_s, sum)
plot(x=max_eigens, y=pro_files, t="l", main="Strategy PnL as Function of Max_eigen",
  xlab="Max_eigen", ylab="pnl")
max_eigen <- max_eigens[which.max(pro_files)]
pnl_s <- pnl_s[[which.max(pro_files)]]

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
    ex_cess=returns_100, re_turns=returns_100,
    start_p=start_p, end_p=end_p,
    al_pha=al_pha, max_eigen=max_eigen)
})  # end lapply
pro_files <- sapply(pnl_s, sum)
plot(x=look_backs, y=pro_files, t="l", main="Strategy PnL as Function of Look-back Interval",
  xlab="Look-back Interval", ylab="pnl")
look_back <- look_backs[which.max(pro_files)]
pnl_s <- pnl_s[[which.max(pro_files)]]

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

options(width=200)
# Load package HighFreq
library(HighFreq)
# Or load the high frequency data file directly:
symbol_s <- load("C:/Develop/R/HighFreq/data/hf_data.RData")
head(SPY_TAQ)
head(SPY)
tail(SPY)

library(rutils)
# Read TAQ trade data from csv file
ta_q <- data.table::fread(file="C:/Develop/data/xlk_tick_trades_2020_03_16.csv")
# Inspect the TAQ data
ta_q
class(ta_q)
colnames(ta_q)
sapply(ta_q, class)
sym_bol <- ta_q$SYM_ROOT[1]
# Create date-time index
date_s <- paste(ta_q$DATE, ta_q$TIME_M)
# Coerce date-time index to POSIXlt
date_s <- strptime(date_s, "%Y%m%d %H:%M:%OS")
class(date_s)
# Display more significant digits
# options("digits")
options(digits=20, digits.secs=10)
last(date_s)
unclass(last(date_s))
as.numeric(last(date_s))
# Coerce date-time index to POSIXct
date_s <- as.POSIXct(date_s)
class(date_s)
last(date_s)
unclass(last(date_s))
as.numeric(last(date_s))
# Calculate the number of ticks per second
n_secs <- as.numeric(last(date_s)) - as.numeric(first(date_s))
NROW(ta_q)/(6.5*3600)
# Select TAQ data columns
ta_q <- ta_q[, .(price=PRICE, volume=SIZE)]
# Add date-time index
ta_q <- cbind(index=date_s, ta_q)

# Coerce trade ticks to xts series
x_ts <- xts::xts(ta_q[, .(price, volume)], ta_q$index)
colnames(x_ts) <- paste(sym_bol, c("Close", "Volume"), sep=".")
save(x_ts, file="C:/Develop/data/xlk_tick_trades_2020_03_16.RData")
# Plot dygraph
dygraphs::dygraph(x_ts$XLK.Close,
  main="XLK Trade Ticks for 2020-03-16")
# Plot in x11 window
x11(width=6, height=5)
quantmod::chart_Series(x=x_ts$XLK.Close,
  name="XLK Trade Ticks for 2020-03-16")

# Select the large lots greater than 100
dim(ta_q)
big_ticks <- ta_q[ta_q$volume > 100]
dim(big_ticks)
# Number of large lot ticks per second
NROW(big_ticks)/(6.5*3600)
# Save trade ticks with large lots
data.table::fwrite(big_ticks, file="C:/Develop/data/xlk_tick_trades_2020_03_16_biglots.csv")
# Coerce trade prices to xts
x_ts <- xts::xts(big_ticks[, .(price, volume)], big_ticks$index)
colnames(x_ts) <- c("XLK.Close", "XLK.Volume")

# Plot dygraph of the large lots
dygraphs::dygraph(x_ts$XLK.Close,
  main="XLK Trade Ticks for 2020-03-16 (large lots only)")
# Plot the large lots
x11(width=6, height=5)
quantmod::chart_Series(x=x_ts$XLK.Close,
  name="XLK Trade Ticks for 2020-03-16 (large lots only)")

# Apply centered Hampel filter to remove price jumps
win_dow <- 111
half_window <- win_dow %/% 2
medi_an <- TTR::runMedian(ta_q$price, n=win_dow)
medi_an <- rutils::lag_it(medi_an, lagg=-half_window, pad_zeros=FALSE)
ma_d <- TTR::runMAD(ta_q$price, n=win_dow)
ma_d <- rutils::lag_it(ma_d, lagg=-half_window, pad_zeros=FALSE)
ma_d[1:half_window] <- 1
ma_d[ma_d == 0] <- 1
# Calculate Z-scores
z_scores <- (ta_q$price - medi_an)/ma_d
z_scores[is.na(z_scores)] <- 0
z_scores[!is.finite(z_scores)] <- 0
sum(is.na(z_scores))
sum(!is.finite(z_scores))
range(z_scores)
mad(z_scores)
hist(z_scores, breaks=2000, xlim=c(-5*mad(z_scores), 5*mad(z_scores)))

# Remove price jumps with large z-scores
thresh_old <- 3
bad_ticks <- (abs(z_scores) > thresh_old)
good_ticks <- ta_q[!bad_ticks]
# Calculate number of price jumps
sum(bad_ticks)/NROW(z_scores)
# Coerce trade prices to xts
x_ts <- xts::xts(good_ticks[, .(price, volume)], good_ticks$index)
colnames(x_ts) <- c("XLK.Close", "XLK.Volume")
# Plot dygraph of the clean lots
dygraphs::dygraph(x_ts$XLK.Close,
  main="XLK Trade Ticks for 2020-03-16 (Hampel filtered)")
# Plot the large lots
x11(width=6, height=5)
quantmod::chart_Series(x=x_ts$XLK.Close,
  name="XLK Trade Ticks for 2020-03-16 (Hampel filtered)")

# Round time index to seconds
good_ticks[, index := as.POSIXct(round.POSIXt(index, "secs"))]
# Aggregate to OHLC by seconds
oh_lc <- good_ticks[, .(open=first(price), high=max(price), low=min(price), close=last(price), volume=sum(volume)), by=index]
# Round time index to minutes
good_ticks[, index := as.POSIXct(round.POSIXt(index, "mins"))]
# Aggregate to OHLC by minutes
oh_lc <- good_ticks[, .(open=first(price), high=max(price), low=min(price), close=last(price), volume=sum(volume)), by=index]

# Coerce OHLC prices to xts
x_ts <- xts::xts(oh_lc[, -"index"], oh_lc$index)
# Plot dygraph of the OHLC prices
dygraphs::dygraph(x_ts[, -5], main="XLK Trade Ticks for 2020-03-16 (OHLC)") %>%
  dyCandlestick()
# Plot the OHLC prices
x11(width=6, height=5)
quantmod::chart_Series(x=x_ts, TA="add_Vo()",
  name="XLK Trade Ticks for 2020-03-16 (OHLC)")

# Load package HighFreq
library(HighFreq)
head(SPY)

# Load package HighFreq
library(HighFreq)
# Define sym_bol
sym_bol <- "SPY"
# Load OHLC data
output_dir <- "C:/Develop/data/hfreq/scrub/"
sym_bol <- load(
  file.path(output_dir,
      paste0(sym_bol, ".RData")))
inter_val <-
  "2013-11-11 09:30:00/2013-11-11 10:30:00"
chart_Series(SPY[inter_val], name=sym_bol)

# Install package HighFreq from github
devtools::install_github(repo="algoquant/HighFreq")
# Load package HighFreq
library(HighFreq)
# Get documentation for package HighFreq
# Get short description
packageDescription(HighFreq)
# Load help page
help(package=HighFreq)
# List all datasets in HighFreq
data(package=HighFreq)
# List all objects in HighFreq
ls("package:HighFreq")
# Remove HighFreq from search path
detach("package:HighFreq")

# Load package HighFreq
library(HighFreq)
# You can see SPY when listing objects in HighFreq
ls("package:HighFreq")
# You can see SPY when listing datasets in HighFreq
data(package=HighFreq)
# But the SPY dataset isn't listed in the workspace
ls()
# HighFreq datasets are lazy loaded and available when needed
head(SPY)
# Load all the datasets in package HighFreq
data(hf_data)
# HighFreq datasets are now loaded and in the workspace
head(SPY)

library(rutils)  # Load package rutils
# SPY percentage returns
oh_lc <- HighFreq::SPY
n_rows <- NROW(oh_lc)
clos_e <- log(quantmod::Cl(oh_lc))
re_turns <- rutils::diff_it(clos_e)
colnames(re_turns) <- "SPY"
# Standardize raw returns to make later comparisons
re_turns <- (re_turns - mean(re_turns))/sd(re_turns)
# Calculate moments and perform normality test
sapply(c(var=2, skew=3, kurt=4),
  function(x) sum(re_turns^x)/n_rows)
tseries::jarque.bera.test(re_turns)
# Fit SPY returns using MASS::fitdistr()
optim_fit <- MASS::fitdistr(re_turns, densfun="t", df=2)
lo_cation <- optim_fit$estimate[1]
scal_e <- optim_fit$estimate[2]

x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(1, 1, 1, 1))
# Plot histogram of SPY returns
histo_gram <- hist(re_turns, col="lightgrey", mgp=c(2, 1, 0),
  xlab="returns (standardized)", ylab="frequency", xlim=c(-3, 3),
  breaks=1e3, freq=FALSE, main="Distribution of High Frequency SPY Returns")
# lines(density(re_turns, bw=0.2), lwd=3, col="blue")
# Plot t-distribution function
curve(expr=dt((x-lo_cation)/scal_e, df=2)/scal_e,
type="l", lwd=3, col="red", add=TRUE)
# Plot the Normal probability distribution
curve(expr=dnorm(x, mean=mean(re_turns),
  sd=sd(re_turns)), add=TRUE, lwd=3, col="blue")
# Add legend
legend("topright", inset=0.05, bty="n",
  leg=c("t-distr", "normal"),
  lwd=6, lty=1, col=c("red", "blue"))

# Hourly SPY percentage returns
clos_e <- log(Cl(xts::to.period(x=oh_lc, period="hours")))
hour_ly <- rutils::diff_it(clos_e)
hour_ly <- (hour_ly - mean(hour_ly))/sd(hour_ly)
# Daily SPY percentage returns
clos_e <- log(Cl(xts::to.period(x=oh_lc, period="days")))
dai_ly <- rutils::diff_it(clos_e)
dai_ly <- (dai_ly - mean(dai_ly))/sd(dai_ly)
# Calculate moments
sapply(list(minutely=re_turns, hourly=hour_ly, daily=dai_ly),
 function(rets) {
   sapply(c(var=2, skew=3, kurt=4),
          function(x) mean(rets^x))
})  # end sapply

x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(1, 1, 1, 1))
# Plot densities of SPY returns
plot(density(re_turns, bw=0.4), xlim=c(-3, 3),
     lwd=3, mgp=c(2, 1, 0), col="blue",
     xlab="returns (standardized)", ylab="frequency",
     main="Density of High Frequency SPY Returns")
lines(density(hour_ly, bw=0.4), lwd=3, col="green")
lines(density(dai_ly, bw=0.4), lwd=3, col="red")
# Add legend
legend("topright", inset=0.05, bty="n",
  leg=c("minutely", "hourly", "daily"),
  lwd=6, lty=1, col=c("blue", "green", "red"))

# Calculate rolling volatility of SPY returns
ret_2013 <- re_turns["2013-11-11/2013-11-15"]
# Calculate rolling volatility
look_back <- 11
end_p <- seq_along(ret_2013)
start_p <- c(rep_len(1, look_back-1),
  end_p[1:(NROW(end_p)-look_back+1)])
end_p[end_p < look_back] <- look_back
vol_rolling <- sapply(seq_along(end_p),
  function(it) sd(ret_2013[start_p[it]:end_p[it]]))
vol_rolling <- xts::xts(vol_rolling, index(ret_2013))
# Extract time intervals of SPY returns
in_dex <- c(60, diff(xts::.index(ret_2013)))
head(in_dex)
table(in_dex)
# Scale SPY returns by time intervals
ret_2013 <- 60*ret_2013/in_dex
# Calculate scaled rolling volatility
vol_scaled <- sapply(seq_along(end_p),
  function(it) sd(ret_2013[start_p[it]:end_p[it]]))
vol_rolling <- cbind(vol_rolling, vol_scaled)
vol_rolling <- na.omit(vol_rolling)
sum(is.na(vol_rolling))
sapply(vol_rolling, range)

# Plot rolling volatility
x11(width=6, height=5)
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("blue", "red")
chart_Series(vol_rolling, theme=plot_theme,
     name="Rolling Volatility with Overnight Spikes")
legend("topright", legend=colnames(vol_rolling),
  inset=0.1, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")

price_s <- read.zoo(file="C:/Develop/lecture_slides/data/bid_ask_bounce.csv",
  header=TRUE, sep=",")
price_s <- as.xts(price_s)
x11(width=6, height=4)
par(mar=c(2, 2, 0, 0), oma=c(1, 1, 0, 0))
chart_Series(x=price_s, name="S&P500 Futures Bid-Ask Bounce")

# Volatility of SPY
sqrt(HighFreq::calc_var_ohlc(oh_lc))
# Daily SPY volatility and volume
vol_daily <- sqrt(xts::apply.daily(oh_lc, FUN=calc_var_ohlc))
colnames(vol_daily) <- ("SPY_volatility")
vol_ume <- quantmod::Vo(oh_lc)
volume_daily <- xts::apply.daily(vol_ume, FUN=sum)
colnames(volume_daily) <- ("SPY_volume")
# Plot SPY volatility and volume
da_ta <- cbind(vol_daily, volume_daily)["2008/2009"]
col_names <- colnames(da_ta)
dygraphs::dygraph(da_ta,
  main="SPY Daily Volatility and Trading Volume") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="red", strokeWidth=3) %>%
  dySeries(name=col_names[2], axis="y2", col="blue", strokeWidth=3)

# Regress log of daily volume vs volatility
da_ta <- log(cbind(volume_daily, vol_daily))
col_names <- colnames(da_ta)
data_frame <- as.data.frame(da_ta)
for_mula <- as.formula(paste(col_names, collapse="~"))
mod_el <- lm(for_mula, data=data_frame)
# Durbin-Watson test for autocorrelation of residuals
lmtest::dwtest(mod_el)
# Regress diff log of daily volume vs volatility
data_frame <- as.data.frame(rutils::diff_it(da_ta))
mod_el <- lm(for_mula, data=data_frame)
lmtest::dwtest(mod_el)
summary(mod_el)
plot(for_mula, data=data_frame, main="SPY Daily Trading Volume vs Volatility (log scale)")
abline(mod_el, lwd=3, col="red")
mtext(paste("beta =", round(coef(mod_el)[2], 3)), cex=1.2, lwd=3, side=2, las=2, adj=(-0.5), padj=(-7))

# 60 minutes of data in look_back interval
look_back <- 60
vol_2013 <- vol_ume["2013"]
ret_2013 <- re_turns["2013"]
# Define end points with beginning stub
n_rows <- NROW(ret_2013)
n_agg <- n_rows %/% look_back
end_p <- n_rows-look_back*n_agg + (0:n_agg)*look_back
start_p <- c(1, end_p[1:(NROW(end_p)-1)])
# Calculate SPY volatility and volume
da_ta <- sapply(seq_along(end_p), function(it) {
  point_s <- start_p[it]:end_p[it]
  c(volume=sum(vol_2013[point_s]),
    volatility=sd(ret_2013[point_s]))
})  # end sapply
da_ta <- t(da_ta)
da_ta <- rutils::diff_it(log(da_ta))
data_frame <- as.data.frame(da_ta)

for_mula <- as.formula(paste(colnames(da_ta), collapse="~"))
mod_el <- lm(for_mula, data=data_frame)
lmtest::dwtest(mod_el)
summary(mod_el)
plot(for_mula, data=data_frame,
     main="SPY Hourly Trading Volume vs Volatility (log scale)")
abline(mod_el, lwd=3, col="red")
mtext(paste("beta =", round(coef(mod_el)[2], 3)), cex=1.2, lwd=3, side=2, las=2, adj=(-0.5), padj=(-7))

# Scale returns using volume (volume clock)
rets_scaled <- ifelse(vol_ume > 1e4, re_turns/vol_ume, 0)
rets_scaled <- rets_scaled/sd(rets_scaled)
# Calculate moments of scaled returns
n_rows <- NROW(re_turns)
sapply(list(re_turns=re_turns, rets_scaled=rets_scaled),
  function(rets) {sapply(c(skew=3, kurt=4),
     function(x) sum((rets/sd(rets))^x)/n_rows)
})  # end sapply

x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(1, 1, 1, 1))
# Plot densities of SPY returns
plot(density(re_turns), xlim=c(-3, 3),
     lwd=3, mgp=c(2, 1, 0), col="blue",
     xlab="returns (standardized)", ylab="frequency",
     main="Density of Volume-scaled High Frequency SPY Returns")
lines(density(rets_scaled, bw=0.4), lwd=3, col="red")
curve(expr=dnorm, add=TRUE, lwd=3, col="green")
# Add legend
legend("topright", inset=0.05, bty="n",
  leg=c("minutely", "scaled", "normal"),
  lwd=6, lty=1, col=c("blue", "red", "green"))

# Ljung-Box test for minutely SPY returns
Box.test(re_turns, lag=10, type="Ljung")
# Ljung-Box test for daily SPY returns
Box.test(dai_ly, lag=10, type="Ljung")
# Ljung-Box test statistics for scaled SPY returns
sapply(list(re_turns=re_turns, rets_scaled=rets_scaled),
  function(rets) {
    Box.test(rets, lag=10, type="Ljung")$statistic
})  # end sapply
# Ljung-Box test statistics for aggregated SPY returns
sapply(list(minutely=re_turns, hourly=hour_ly, daily=dai_ly),
  function(rets) {
    Box.test(rets, lag=10, type="Ljung")$statistic
})  # end sapply

# Set plot parameters
x11(width=6, height=8)
par(mar=c(4, 4, 2, 1), oma=c(0, 0, 0, 0))
layout(matrix(c(1, 2), ncol=1), widths=c(6, 6), heights=c(4, 4))
# Plot the partial autocorrelations of minutely SPY returns
pa_cf <- pacf(as.numeric(re_turns), lag=10,
     xlab="lag", ylab="partial autocorrelation", main="")
title("Partial Autocorrelations of Minutely SPY Returns", line=1)
# Plot the partial autocorrelations of scaled SPY returns
pacf_scaled <- pacf(as.numeric(rets_scaled), lag=10,
     xlab="lag", ylab="partial autocorrelation", main="")
title("Partial Autocorrelations of Scaled SPY Returns", line=1)
# Calculate the sums of partial autocorrelations
sum(pa_cf$acf)
sum(pacf_scaled$acf)

# Calculate market illiquidity
liquidi_ty <- sqrt(volume_daily)/vol_daily
# Plot market illiquidity
x11(width=6, height=7) ; par(mfrow=c(2, 1))
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("blue")
chart_Series(liquidi_ty["2010"], theme=plot_theme,
  name="SPY Liquidity in 2010", plot=FALSE)
plot_theme$col$line.col <- c("red")
chart_Series(vol_daily["2010"],
  theme=plot_theme, name="SPY Volatility in 2010")

# Calculate intraday time index with hours and minutes
date_s <- format(zoo::index(re_turns), "%H:%M")
# Aggregate the mean volume
volume_agg <- tapply(X=vol_ume, INDEX=date_s, FUN=mean)
volume_agg <- drop(volume_agg)
# Aggregate the mean volatility
vol_agg <- tapply(X=re_turns^2, INDEX=date_s, FUN=mean)
vol_agg <- sqrt(drop(vol_agg))
# Coerce to xts
intra_day <- as.POSIXct(paste(Sys.Date(), names(volume_agg)))
volume_agg <- xts::xts(volume_agg, intra_day)
vol_agg <- xts::xts(vol_agg, intra_day)
# Plot seasonality of volume and volatility
x11(width=6, height=7) ; par(mfrow=c(2, 1))
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("blue")
chart_Series(volume_agg[c(-1, -NROW(volume_agg))], theme=plot_theme,
  name="Daily Seasonality of SPY Volume", plot=FALSE)
plot_theme$col$line.col <- c("red")
chart_Series(vol_agg[c(-1, -NROW(vol_agg))], theme=plot_theme,
  name="Daily Seasonality of SPY Volatility")

# Calculate market liquidity
liquidi_ty <- sqrt(volume_agg)/vol_agg
# Plot daily seasonality of market liquidity
x11(width=6, height=7) ; par(mfrow=c(2, 1))
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("blue")
chart_Series(liquidi_ty[c(-1, -NROW(liquidi_ty))], theme=plot_theme,
  name="Daily Seasonality of SPY Liquidity", plot=FALSE)
plot_theme$col$line.col <- c("red")
chart_Series(vol_agg[c(-1, -NROW(vol_agg))], theme=plot_theme,
  name="Daily Seasonality of SPY Volatility")

# Futures contracts codes
future_s <- rbind(c("S&P500 index", "ES"),
              c("10yr Treasury", "ZN"),
              c("VIX index", "VX"),
              c("Gold", "GC"),
              c("Oil", "CL"),
              c("Euro FX", "EC"),
              c("Swiss franc", "SF"),
              c("Japanese Yen", "JY"))
colnames(future_s) <- c("Futures contract", "Code")
print(xtable::xtable(future_s), comment=FALSE, size="scriptsize", include.rownames=FALSE, latex.environments="flushleft")

# Monthly futures contract codes
month_codes <- cbind(c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"),
                     c("F", "G", "H", "J", "K", "M", "N", "Q", "U", "V", "X", "Z"))
colnames(month_codes) <- c("Month", "Code")
print(xtable::xtable(month_codes), comment=FALSE, size="scriptsize", include.rownames=FALSE, latex.environments="flushright")

# Futures contracts codes
future_s <- rbind(c("S&P500 index", "SP", "ES"),
              c("10yr Treasury", "ZN", "ZN"),
              c("VIX index", "VX", "delisted"),
              c("Gold", "GC", "YG"),
              c("Oil", "CL", "QM"),
              c("Euro FX", "EC", "E7"),
              c("Swiss franc", "SF", "MSF"),
              c("Japanese Yen", "JY", "J7"))
colnames(future_s) <- c("Futures contract", "Standard", "E-mini")
print(xtable::xtable(future_s), comment=FALSE, size="scriptsize", include.rownames=FALSE, latex.environments="flushleft")

# Load data for S&P Emini futures June 2019 contract
dir_name <- "C:/Develop/data/ib_data"
file_name <- file.path(dir_name, "ES_ohlc.csv")
# Read a data table from CSV file
price_s <- data.table::fread(file_name)
class(price_s)
# Coerce first column from string to date-time
unlist(sapply(price_s, class))
tail(price_s)
price_s$Index <- as.POSIXct(price_s$Index,
  tz="America/New_York", origin="1970-01-01")
# Coerce price_s into xts series
price_s <- data.table::as.xts.data.table(price_s)
class(price_s)
tail(price_s)
colnames(price_s)[1:5] <- c("Open", "High", "Low", "Close", "Volume")
tail(price_s)

# Plot OHLC data in x11 window
x11(width=5, height=4)  # Open x11 for plotting
par(mar=c(5, 5, 2, 1), oma=c(0, 0, 0, 0))
chart_Series(x=price_s, TA="add_Vo()",
  name="S&P500 futures")
# Plot dygraph
dygraphs::dygraph(price_s[, 1:4], main="OHLC prices") %>%
  dyCandlestick()

# Load ESU8 data
dir_name <- "C:/Develop/data/ib_data"
file_name <- file.path(dir_name, "ESU8.csv")
ES_U8 <- data.table::fread(file_name)
# Coerce ES_U8 into xts series
ES_U8$V1 <- as.Date(as.POSIXct.numeric(ES_U8$V1,
    tz="America/New_York", origin="1970-01-01"))
ES_U8 <- data.table::as.xts.data.table(ES_U8)
colnames(ES_U8)[1:5] <- c("Open", "High", "Low", "Close", "Volume")
# Load ESM8 data
file_name <- file.path(dir_name, "ESM8.csv")
ES_M8 <- data.table::fread(file_name)
# Coerce ES_M8 into xts series
ES_M8$V1 <- as.Date(as.POSIXct.numeric(ES_M8$V1,
    tz="America/New_York", origin="1970-01-01"))
ES_M8 <- data.table::as.xts.data.table(ES_M8)
colnames(ES_M8)[1:5] <- c("Open", "High", "Low", "Close", "Volume")

x11(width=6, height=5)  # Open x11 for plotting
# Plot last month of ESU8 and ESM8 volume data
en_d <- end(ES_M8)
star_t <- (en_d - 30)
vol_ume <- cbind(Vo(ES_U8),
  Vo(ES_M8))[paste0(star_t, "/", en_d)]
colnames(vol_ume) <- c("ESU8", "ESM8")
col_ors <- c("blue", "green")
plot(vol_ume, col=col_ors, lwd=3, major.ticks="days",
     format.labels="%b-%d", observation.based=TRUE,
     main="Volumes of ESU8 and ESM8 futures")
legend("topleft", legend=colnames(vol_ume), col=col_ors,
 title=NULL, bty="n", lty=1, lwd=6, inset=0.1, cex=0.7)

# Find date when ESU8 volume exceeds ESM8
exceed_s <- (vol_ume[, "ESU8"] > vol_ume[, "ESM8"])
in_dex <- match(TRUE, exceed_s)
# in_dex <- min(which(exceed_s))
# Scale the ES_M8 prices
in_dex <- index(exceed_s[in_dex])
fac_tor <- as.numeric(Cl(ES_U8[in_dex])/Cl(ES_M8[in_dex]))
ES_M8[, 1:4] <- fac_tor*ES_M8[, 1:4]
# Calculate continuous contract prices
chain_ed <- rbind(ES_M8[index(ES_M8) < in_dex],
            ES_U8[index(ES_U8) >= in_dex])
# Or
# Chain_ed <- rbind(ES_M8[paste0("/", in_dex-1)],
#                   ES_U8[paste0(in_dex, "/")])
# Plot continuous contract prices
chart_Series(x=chain_ed["2018"], TA="add_Vo()",
  name="S&P500 chained futures")

# Download VIX index data from CBOE
vix_index <- data.table::fread("http://www.cboe.com/publish/scheduledtask/mktdata/datahouse/vixcurrent.csv", skip=1)
class(vix_index)
dim(vix_index)
tail(vix_index)
sapply(vix_index, class)
vix_index <- xts(vix_index[, -1],
  order.by=as.Date(vix_index$Date, format="%m/%d/%Y"))
colnames(vix_index) <- c("Open", "High", "Low", "Close")
# Save the VIX data to binary file
load(file="C:/Develop/data/ib_data/vix_cboe.RData")
ls(vix_env)
vix_env$vix_index <- vix_index
ls(vix_env)
save(vix_env, file="C:/Develop/data/ib_data/vix_cboe.RData")
# Plot OHLC data in x11 window
chart_Series(x=vix_index["2018"], name="VIX Index")
# Plot dygraph
dygraphs::dygraph(vix_index, main="VIX Index") %>%
  dyCandlestick()

# Read CBOE monthly futures expiration dates
date_s <- read.csv(
  file="C:/Develop/data/vix_data/vix_dates.csv")
date_s <- as.Date(date_s[, 1])
year_s <- format(date_s, format="%Y")
year_s <- substring(year_s, 4)
# Monthly futures contract codes
month_codes <-
  c("F", "G", "H", "J", "K", "M",
    "N", "Q", "U", "V", "X", "Z")
sym_bols <- paste0("VX", month_codes, year_s)
date_s <- as.data.frame(date_s)
colnames(date_s) <- "exp_dates"
rownames(date_s) <- sym_bols
# Write dates to CSV file, with row names
write.csv(date_s, row.names=TRUE,
  file="C:/Develop/data/vix_data/vix_futures.csv")
# Read back CBOE futures expiration dates
date_s <- read.csv(file="C:/Develop/data/vix_data/vix_futures.csv",
  row.names=1)
date_s[, 1] <- as.Date(date_s[, 1])

# Load VIX futures data from binary file
load(file="C:/Develop/data/vix_data/vix_cboe.RData")
# Get all VIX futures for 2018 except January
sym_bols <- ls(vix_env)
sym_bols <- sym_bols[grep("*8", sym_bols)]
sym_bols <- sym_bols[2:9]
# Specify dates for curves
low_vol <- as.Date("2018-01-11")
hi_vol <- as.Date("2018-02-05")
# Extract all VIX futures prices on the dates
curve_s <- lapply(sym_bols, function(sym_bol) {
  x_ts <- get(x=sym_bol, envir=vix_env)
  Cl(x_ts[c(low_vol, hi_vol)])
})  # end lapply
curve_s <- rutils::do_call(cbind, curve_s)
colnames(curve_s) <- sym_bols
curve_s <- t(coredata(curve_s))
colnames(curve_s) <- c("Contango 01/11/2018",
                 "Backwardation 02/05/2018")

x11(width=7, height=5)
par(mar=c(3, 2, 1, 1), oma=c(0, 0, 0, 0))
plot(curve_s[, 1], type="l", lty=1, col="blue", lwd=3,
     xaxt="n", xlab="", ylab="", ylim=range(curve_s),
     main="VIX Futures Curves")
axis(1, at=(1:NROW(curve_s)), labels=rownames(curve_s))
lines(curve_s[, 2], lty=1, lwd=3, col="red")
legend(x="topright", legend=colnames(curve_s),
 inset=0.05, cex=1.0, bty="n",
 col=c("blue", "red"), lwd=6, lty=1)

# Load VIX futures data from binary file
load(file="C:/Develop/data/vix_data/vix_cboe.RData")
# Read CBOE futures expiration dates
date_s <- read.csv(file="C:/Develop/data/vix_data/vix_futures.csv",
  row.names=1)
sym_bols <- rownames(date_s)
date_s <- as.Date(date_s[, 1])
to_day <- as.Date("2018-05-07")
maturi_ty <- (to_day + 30)
# Find neighboring futures contracts
in_dex <- match(TRUE, date_s > maturi_ty)
front_date <- date_s[in_dex-1]
back_date <- date_s[in_dex]
front_symbol <- sym_bols[in_dex-1]
back_symbol <- sym_bols[in_dex]
front_price <- get(x=front_symbol, envir=vix_env)
front_price <- as.numeric(Cl(front_price[to_day]))
back_price <- get(x=back_symbol, envir=vix_env)
back_price <- as.numeric(Cl(back_price[to_day]))
# Calculate the constant maturity 30-day futures price
ra_tio <- as.numeric(maturi_ty - front_date) /
  as.numeric(back_date - front_date)
pric_e <- (ra_tio*back_price + (1-ra_tio)*front_price)

x11(width=5, height=3)  # Open x11 for plotting
# Load VIX futures data from binary file
load(file="C:/Develop/data/vix_data/vix_cboe.RData")
# Plot VIX and SVXY data in x11 window
plot_theme <- chart_theme()
plot_theme$col$line.col <- "blue"
chart_Series(x=Cl(vix_env$vix_index["2007/"]),
       theme=plot_theme, name="VIX Index")
chart_Series(x=Cl(rutils::etf_env$VTI["2007/"]),
       theme=plot_theme, name="VTI ETF")

chart_Series(x=Cl(vix_env$vix_index["2017/2018"]),
       theme=plot_theme, name="VIX Index")
chart_Series(x=Cl(rutils::etf_env$SVXY["2017/2018"]),
       theme=plot_theme, name="SVXY ETF")
