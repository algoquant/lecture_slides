# Extract ETF returns
sym_bols <- c("VTI", "IEF", "DBC")
re_turns <- rutils::etf_env$re_turns[, sym_bols]
re_turns <- na.omit(re_turns)
# Or, select rows with IEF data
# re_turns <- re_turns[index(rutils::etf_env$IEF)]
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
                endp=rutils::calc_endpoints(re_turns, inter_val=re_balance)[-1],
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
source("/Users/jerzy/Develop/lecture_slides/scripts/back_test.R")
look_backs <- seq(3, 15, by=1)
perform_ance <- function(re_turns) sum(re_turns)/sd(re_turns)
pro_file <- sapply(look_backs, function(look_back) {
  pnl_s <- backtest_momentum(returns=re_turns, endp=end_p,
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
  look_back=look_back, endp=end_p,
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
vt_i <- log(quantmod::Cl(rutils::etf_env$VTI[date_s]))
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
dygraphs::dygraph(cumsum(weal_th), main="Daily Momentum Strategy vs All-Weather") %>%
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
source("/Users/jerzy/Develop/lecture_slides/scripts/back_test.R")
pnl_s <- momentum_daily(returns=re_turns, look_back=252,
  bid_offer=bid_offer)
# Perform sapply loop over look_backs
look_backs <- seq(50, 300, by=50)
pnl_s <- sapply(look_backs, momentum_daily,
  returns=re_turns, bid_offer=bid_offer)
colnames(pnl_s) <- paste0("look_back=", look_backs)
pnl_s <- xts::xts(pnl_s, index(re_turns))
tail(pnl_s)
# Plot dygraph of daily ETF momentum strategies
col_ors <- colorRampPalette(c("blue", "red"))(NCOL(pnl_s))
dygraphs::dygraph(cumsum(pnl_s), main="Daily ETF Momentum Strategies") %>%
  dyOptions(colors=col_ors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Plot EWMA strategies with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(pnl_s))
quantmod::chart_Series(cumsum(pnl_s),
  theme=plot_theme, name="Cumulative Returns of Daily ETF Momentum Strategies")
legend("bottomleft", legend=colnames(pnl_s),
  inset=0.02, bg="white", cex=0.7, lwd=rep(6, NCOL(re_turns)),
  col=plot_theme$col$line.col, bty="n")
# Define backtest functional for daily momentum strategy
# If trend=(-1) then it backtests a mean reverting strategy
momentum_daily <- function(returns, look_back=252, hold_period=5, bid_offer=0.001, trend=1, ...) {
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
  # Average the weights over holding period
  weight_s <- roll::roll_mean(weight_s, width=hold_period, min_obs=1)
  # Calculate momentum profits and losses
  pnl_s <- trend*rowMeans(weight_s*returns)
  # Calculate transaction costs
  cost_s <- 0.5*bid_offer*rowSums(abs(rutils::diff_it(weight_s)))
  (pnl_s - cost_s)
}  # end momentum_daily
# Perform sapply loop over holding periods
hold_periods <- seq(2, 11, by=2)
pnl_s <- sapply(hold_periods, momentum_daily, look_back=120,
            returns=re_turns, bid_offer=bid_offer)
colnames(pnl_s) <- paste0("holding=", hold_periods)
pnl_s <- xts::xts(pnl_s, index(re_turns))
# Plot dygraph of daily ETF momentum strategies
col_ors <- colorRampPalette(c("blue", "red"))(NCOL(pnl_s))
dygraphs::dygraph(cumsum(pnl_s), main="Daily ETF Momentum Strategies with Holding Period") %>%
  dyOptions(colors=col_ors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Plot EWMA strategies with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(pnl_s))
quantmod::chart_Series(cumsum(pnl_s),
  theme=plot_theme, name="Cumulative Returns of Daily ETF Momentum Strategies")
legend("bottomleft", legend=colnames(pnl_s),
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
look_backs <- seq(100, 300, by=20)
pnl_s <- sapply(look_backs, momentum_daily,
  hold_period=5, returns=returns_100, bid_offer=0)
colnames(pnl_s) <- paste0("look_back=", look_backs)
pnl_s <- xts::xts(pnl_s, index(returns_100))
# Plot dygraph of daily ETF momentum strategies
col_ors <- colorRampPalette(c("blue", "red"))(NCOL(pnl_s))
dygraphs::dygraph(cumsum(pnl_s), main="Daily S&P500 Momentum Strategies") %>%
  dyOptions(colors=col_ors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Plot daily S&P500 momentum strategies with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- colorRampPalette(c("blue", "red"))(NCOL(pnl_s))
quantmod::chart_Series(cumsum(pnl_s),
  theme=plot_theme, name="Daily S&P500 Momentum Strategies")
legend("bottomleft", legend=colnames(pnl_s),
  inset=0.02, bg="white", cex=0.7, lwd=rep(6, NCOL(re_turns)),
  col=plot_theme$col$line.col, bty="n")
# Perform sapply loop over look_backs
look_backs <- seq(3, 20, by=2)
pnl_s <- sapply(look_backs, momentum_daily,
  hold_period=5, returns=returns_100, bid_offer=0, trend=(-1))
colnames(pnl_s) <- paste0("look_back=", look_backs)
pnl_s <- xts::xts(pnl_s, index(returns_100))
# Plot dygraph of daily ETF momentum strategies
col_ors <- colorRampPalette(c("blue", "red"))(NCOL(pnl_s))
dygraphs::dygraph(cumsum(pnl_s), main="Daily S&P500 Momentum Strategies") %>%
  dyOptions(colors=col_ors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Plot EWMA strategies with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- colorRampPalette(c("blue", "red"))(NCOL(pnl_s))
quantmod::chart_Series(cumsum(pnl_s),
  theme=plot_theme, name="Cumulative Returns of S&P500 Mean Reverting Strategies")
legend("topleft", legend=colnames(pnl_s),
  inset=0.05, bg="white", cex=0.7, lwd=rep(6, NCOL(re_turns)),
  col=plot_theme$col$line.col, bty="n")
# Plot cumulative returns of VTI vs MTUM ETF
weal_th <- log(na.omit(rutils::etf_env$price_s[, c("VTI", "MTUM")]))
colnames(weal_th) <- c("VTI", "MTUM")
weal_th <- rutils::diff_it(weal_th)
dygraphs::dygraph(cumsum(weal_th), main="VTI vs MTUM ETF") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(width=500)
# Verify that rtools are working properly:
devtools::find_rtools()
devtools::has_devel()
# Load package Rcpp
library(Rcpp)
# Get documentation for package Rcpp
# Get short description
packageDescription("Rcpp")
# Load help page
help(package="Rcpp")
# List all datasets in "Rcpp"
data(package="Rcpp")
# List all objects in "Rcpp"
ls("package:Rcpp")
# Remove Rcpp from search path
detach("package:Rcpp")
# Define Rcpp function
Rcpp::cppFunction("
  int times_two(int x)
    { return 2 * x;}
  ")  # end cppFunction
# Run Rcpp function
times_two(3)
# Source Rcpp functions from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/mult_rcpp.cpp")
# Multiply two numbers
mult_rcpp(2, 3)
mult_rcpp(1:3, 6:4)
# Multiply two vectors
mult_vec_rcpp(2, 3)
mult_vec_rcpp(1:3, 6:4)
# Define Rcpp function with loop
Rcpp::cppFunction("
double inner_mult(NumericVector x, NumericVector y) {
int x_size = x.size();
int y_size = y.size();
if (x_size != y_size) {
    return 0;
  } else {
    double total = 0;
    for(int i = 0; i < x_size; ++i) {
total += x[i] * y[i];
  }
  return total;
  }
}")  # end cppFunction
# Run Rcpp function
inner_mult(1:3, 6:4)
inner_mult(1:3, 6:3)
# Define Rcpp Sugar function with loop
Rcpp::cppFunction("
double inner_mult_sugar(NumericVector x, NumericVector y) {
  return sum(x * y);
}")  # end cppFunction
# Run Rcpp Sugar function
inner_mult_sugar(1:3, 6:4)
inner_mult_sugar(1:3, 6:3)
# Define R function with loop
inner_mult_r <- function(x, y) {
    to_tal <- 0
    for(i in 1:NROW(x)) {
to_tal <- to_tal + x[i] * y[i]
    }
    to_tal
}  # end inner_mult_r
# Run R function
inner_mult_r(1:3, 6:4)
inner_mult_r(1:3, 6:3)
# Compare speed of Rcpp and R
library(microbenchmark)
summary(microbenchmark(
  pure_r=inner_mult_r(1:10000, 1:10000),
  inner_r=1:10000 %*% 1:10000,
  r_cpp=inner_mult(1:10000, 1:10000),
  r_cpp_sugar=inner_mult_sugar(1:10000, 1:10000),
  times=10))[, c(1, 4, 5)]
# Define Ornstein-Uhlenbeck function in R
sim_ou <- function(n_rows=1000, eq_price=5.0,
              vol_at=0.01, theta=0.01) {
  re_turns <- numeric(n_rows)
  price_s <- numeric(n_rows)
  price_s[1] <- eq_price
  for (i in 2:n_rows) {
    re_turns[i] <- the_ta*(eq_price - price_s[i-1]) + vol_at*rnorm(1)
    price_s[i] <- price_s[i-1] + re_turns[i]
  }  # end for
  price_s
}  # end sim_ou
# Simulate Ornstein-Uhlenbeck process in R
eq_price <- 5.0; sig_ma <- 0.01
the_ta <- 0.01; n_rows <- 1000
set.seed(1121)  # Reset random numbers
ou_sim <- sim_ou(n_rows=n_rows, eq_price=eq_price, vol_at=sig_ma, theta=the_ta)
# Define Ornstein-Uhlenbeck function in Rcpp
Rcpp::cppFunction("
NumericVector sim_ou_rcpp(double eq_price,
                double vol_at,
                double the_ta,
                NumericVector in_nov) {
  int n_rows = in_nov.size();
  NumericVector price_s(n_rows);
  NumericVector re_turns(n_rows);
  price_s[0] = eq_price;
  for (int it = 1; it < n_rows; it++) {
    re_turns[it] = the_ta*(eq_price - price_s[it-1]) + vol_at*in_nov[it-1];
    price_s[it] = price_s[it-1] + re_turns[it];
  }  // end for
  return price_s;
}")  # end cppFunction
# Simulate Ornstein-Uhlenbeck process in Rcpp
set.seed(1121)  # Reset random numbers
ou_sim_rcpp <- sim_ou_rcpp(eq_price=eq_price,
  vol_at=sig_ma,
  theta=the_ta,
  innov=rnorm(n_rows))
all.equal(ou_sim, ou_sim_rcpp)
# Compare speed of Rcpp and R
library(microbenchmark)
summary(microbenchmark(
  pure_r=sim_ou(n_rows=n_rows, eq_price=eq_price, vol_at=sig_ma, theta=the_ta),
  r_cpp=sim_ou_rcpp(eq_price=eq_price, vol_at=sig_ma, theta=the_ta, innov=rnorm(n_rows)),
  times=10))[, c(1, 4, 5)]
# Source Rcpp function for Ornstein-Uhlenbeck process from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/sim_ou.cpp")
# Simulate Ornstein-Uhlenbeck process in Rcpp
set.seed(1121)  # Reset random numbers
ou_sim_rcpp <- sim_ou_rcpp(eq_price=eq_price,
  vol_at=sig_ma,
  theta=the_ta,
  innov=rnorm(n_rows))
all.equal(ou_sim, ou_sim_rcpp)
# Compare speed of Rcpp and R
library(microbenchmark)
summary(microbenchmark(
  pure_r=sim_ou(n_rows=n_rows, eq_price=eq_price, vol_at=sig_ma, theta=the_ta),
  r_cpp=sim_ou_rcpp(eq_price=eq_price, vol_at=sig_ma, theta=the_ta, innov=rnorm(n_rows)),
  times=10))[, c(1, 4, 5)]
# Calculate uniformly distributed pseudo-random sequence
uni_form <- function(see_d, n_rows=10) {
  out_put <- numeric(n_rows)
  out_put[1] <- see_d
  for (i in 2:n_rows) {
    out_put[i] <- 4*out_put[i-1]*(1-out_put[i-1])
  }  # end for
  acos(1-2*out_put)/pi
}  # end uni_form
# Source Rcpp functions from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/uni_form.cpp")
# Microbenchmark Rcpp code
library(microbenchmark)
summary(microbenchmark(
  pure_r=runif(1e5),
  r_loop=uni_form(0.3, 1e5),
  r_cpp=uniform_rcpp(0.3, 1e5),
  times=10))[, c(1, 4, 5)]
library(RcppArmadillo)
# Source Rcpp functions from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/armadillo_functions.cpp")
vec1 <- runif(1e5)
vec2 <- runif(1e5)
inner_vec(vec1, vec2)
vec1 %*% vec2
# Microbenchmark RcppArmadillo code
summary(microbenchmark(
  inner_vec = inner_vec(vec1, vec2),
  r_code = (vec1 %*% vec2),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
# Microbenchmark shows:
# inner_vec() is several times faster than %*%, especially for longer vectors.
#     expr     mean   median
# 1 inner_vec 110.7067 110.4530
# 2 r_code 585.5127 591.3575
# Source Rcpp functions from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/sim_arima.cpp")
# Define AR(2) coefficients
co_eff <- c(0.9, 0.09)
n_rows <- 1e4
set.seed(1121)
in_nov <- rnorm(n_rows)
# Simulate ARIMA using filter()
arima_filter <- filter(x=in_nov,
  filter=co_eff, method="recursive")
# Simulate ARIMA using sim_arima()
ari_ma <- sim_arima(in_nov, rev(co_eff))
all.equal(drop(ari_ma),
  as.numeric(arima_filter))
# Microbenchmark RcppArmadillo code
summary(microbenchmark(
  sim_arima = sim_arima(in_nov, rev(co_eff)),
  filter = filter(x=in_nov, filter=co_eff, method="recursive"),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
library(RcppArmadillo)
# Source Rcpp functions from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/armadillo_functions.cpp")
mat_rix <- matrix(runif(1e5), nc=1e3)
# De-mean using apply()
new_mat <- apply(mat_rix, 2, function(x) (x-mean(x)))
# De-mean using demean_mat()
demean_mat(mat_rix)
all.equal(new_mat, mat_rix)
# Microbenchmark RcppArmadillo code
library(microbenchmark)
summary(microbenchmark(
  apply = (apply(mat_rix, 2, mean)),
  demean_mat = demean_mat(mat_rix),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
# Perform matrix inversion
# Create random positive semi-definite matrix
mat_rix <- matrix(runif(25), nc=5)
mat_rix <- t(mat_rix) %*% mat_rix
# Invert the matrix
matrix_inv <- solve(mat_rix)
inv_mat(mat_rix)
all.equal(matrix_inv, mat_rix)
# Microbenchmark RcppArmadillo code
summary(microbenchmark(
  solve = solve(mat_rix),
  inv_mat = inv_mat(mat_rix),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
library(RcppArmadillo)
# Source Rcpp functions from file
Rcpp::sourceCpp("/Users/jerzy/Develop/lecture_slides/scripts/calc_weights.cpp")
# Calculate matrix of random returns
mat_rix <- matrix(rnorm(300), nc=5)
# Regularized inverse of correlation matrix
eigen_max <- 4
cor_mat <- cor(mat_rix)
ei_gen <- eigen(cor_mat)
in_verse <- ei_gen$vectors[, 1:eigen_max] %*%
  (t(ei_gen$vectors[, 1:eigen_max]) / ei_gen$values[1:eigen_max])
# Regularized inverse using RcppArmadillo
inverse_arma <- calc_inv(cor_mat, eigen_max=eigen_max)
all.equal(in_verse, inverse_arma)
# Microbenchmark RcppArmadillo code
library(microbenchmark)
summary(microbenchmark(
  Rcode = {ei_gen <- eigen(cor_mat)
ei_gen$vectors[, 1:eigen_max] %*% (t(ei_gen$vectors[, 1:eigen_max]) / ei_gen$values[1:eigen_max])},
  Rcpp = calc_inv(cor_mat, eigen_max=eigen_max),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
# Select all the ETF symbols except "VXX", "SVXY" and "MTUM"
sym_bols <- colnames(rutils::etf_env$re_turns)
sym_bols <- sym_bols[!(sym_bols %in% c("VXX", "SVXY", "MTUM", "QUAL", "VLUE", "USMV"))]
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
rets_is <- re_turns["/2014"]
in_verse <- MASS::ginv(cov(rets_is))
weight_s <- in_verse %*% colMeans(ex_cess["/2014"])
weight_s <- drop(weight_s/sqrt(sum(weight_s^2)))
names(weight_s) <- colnames(re_turns)
# Plot portfolio weights
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
barplot(sort(weight_s), main="Maximum Sharpe Weights", cex.names=0.7)
# Calculate portfolio returns
portf_is <- xts::xts(rets_is %*% weight_s, index(rets_is))
in_dex <- xts::xts(rowSums(rets_is)/sqrt(n_assets), index(rets_is))
portf_is <- portf_is*sd(in_dex)/sd(portf_is)
# Plot cumulative portfolio returns
pnl_s <- cumsum(cbind(portf_is, in_dex))
colnames(pnl_s) <- c("Optimal Portfolio", "Equal Weight Portfolio")
dygraphs::dygraph(pnl_s, main="In-sample Optimal Portfolio Returns") %>%
  dyOptions(colors=c("red", "blue"), strokeWidth=2) %>%
  dyLegend(width=500)
# Out-of-sample portfolio returns
rets_os <- re_turns["2015/"]
portf_os <- xts::xts(rets_os %*% weight_s, index(rets_os))
in_dex <- xts::xts(rowSums(rets_os)/sqrt(n_assets), index(rets_os))
portf_os <- portf_os*sd(in_dex)/sd(portf_os)
pnl_s <- cbind(portf_os, in_dex, (portf_os + in_dex)/2)
colnames(pnl_s) <- c("Optimal", "Equal Weight", "Combined")
sapply(pnl_s, function(x) mean(x)/sd(x))
# Plot cumulative portfolio returns
dygraphs::dygraph(cumsum(pnl_s), main="Out-of-sample Optimal Portfolio Returns") %>%
  dyOptions(colors=c("red", "blue", "green"), strokeWidth=2) %>%
  dyLegend(width=500)
load("/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
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
pnl_s <- rbind(portf_is, portf_os)
pnl_s <- pnl_s*sd(in_dex)/sd(pnl_s)
pnl_s <- cumsum(cbind(pnl_s, in_dex))
colnames(pnl_s) <- c("Optimal Portfolio", "Equal Weight Portfolio")
dygraphs::dygraph(pnl_s, main="Out-of-sample Optimal Portfolio Returns for Stocks") %>%
  dyOptions(colors=c("red", "blue"), strokeWidth=2) %>%
  dyEvent(index(last(rets_is[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=500)
# Create rectangular matrix with collinear columns
ran_dom <- matrix(rnorm(10*8), nc=10)
# Calculate covariance matrix
cov_mat <- cov(ran_dom)
# Calculate inverse of cov_mat - error
in_verse <- solve(cov_mat)
# Perform eigen decomposition
ei_gen <- eigen(cov_mat)
eigen_vec <- ei_gen$vectors
eigen_val <- ei_gen$values
# Set tolerance for determining zero singular values
to_l <- sqrt(.Machine$double.eps)
# Calculate regularized inverse matrix
not_zero <- (eigen_val > (to_l * eigen_val[1]))
inv_reg <- eigen_vec[, not_zero] %*%
  (t(eigen_vec[, not_zero]) / eigen_val[not_zero])
# Verify inverse property of inv_reg
all.equal(cov_mat, cov_mat %*% inv_reg %*% cov_mat)
# Calculate regularized inverse of cov_mat
in_verse <- MASS::ginv(cov_mat)
# Verify inverse property of mat_rix
all.equal(in_verse, inv_reg)
# Calculate in-sample covariance matrix
cov_mat <- cov(rets_is)
ei_gen <- eigen(cov_mat)
eigen_vec <- ei_gen$vectors
eigen_val <- ei_gen$values
# Calculate shrinkage inverse of covariance matrix
eigen_max <- 21
in_verse <- eigen_vec[, 1:eigen_max] %*%
  (t(eigen_vec[, 1:eigen_max]) / ei_gen$values[1:eigen_max])
# Calculate portfolio weights
weight_s <- in_verse %*% colMeans(ex_cess["/2010"])
weight_s <- drop(weight_s/sqrt(sum(weight_s^2)))
names(weight_s) <- colnames(re_turns)
# Calculate portfolio returns
portf_is <- xts::xts(rets_is %*% weight_s, index(rets_is))
portf_os <- xts::xts(rets_os %*% weight_s, index(rets_os))
in_dex <- xts::xts(rowSums(re_turns)/sqrt(n_assets), index(re_turns))
# Plot cumulative portfolio returns
pnl_s <- rbind(portf_is, portf_os)
pnl_s <- pnl_s*sd(in_dex)/sd(pnl_s)
pnl_s <- cumsum(cbind(pnl_s, in_dex))
colnames(pnl_s) <- c("Optimal Portfolio", "Equal Weight Portfolio")
dygraphs::dygraph(pnl_s, main="Regularized Out-of-sample Optimal Portfolio Returns for Stocks") %>%
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
pnl_s <- rbind(portf_is, portf_os)
pnl_s <- pnl_s*sd(in_dex)/sd(pnl_s)
pnl_s <- cumsum(cbind(pnl_s, in_dex))
colnames(pnl_s) <- c("Optimal Portfolio", "Equal Weight Portfolio")
dygraphs::dygraph(pnl_s, main="Out-of-sample Returns for Stocks With Regularization and Shrinkage") %>%
  dyOptions(colors=c("red", "blue"), strokeWidth=2) %>%
  dyEvent(index(last(rets_is[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=500)
library(RcppArmadillo)
# Source Rcpp functions from file
Rcpp::sourceCpp("/Users/jerzy/Develop/lecture_slides/scripts/calc_weights.cpp")
# Create random matrix of returns
mat_rix <- matrix(rnorm(300), nc=5)
# Regularized inverse of covariance matrix
eigen_max <- 4
ei_gen <- eigen(cov(mat_rix))
cov_inv <- ei_gen$vectors[, 1:eigen_max] %*%
  (t(ei_gen$vectors[, 1:eigen_max]) / ei_gen$values[1:eigen_max])
# Regularized inverse using RcppArmadillo
cov_inv_arma <- calc_inv(mat_rix, eigen_max)
all.equal(cov_inv, cov_inv_arma)
# Microbenchmark RcppArmadillo code
library(microbenchmark)
summary(microbenchmark(
  pure_r={
    ei_gen <- eigen(cov(mat_rix))
    ei_gen$vectors[, 1:eigen_max] %*%
(t(ei_gen$vectors[, 1:eigen_max]) / ei_gen$values[1:eigen_max])
  },
  r_cpp=calc_inv(mat_rix, eigen_max),
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
pnl_s <- cumsum(na.omit(cbind(rets_portf, in_dex*sd(rets_portf)/sd(in_dex))))
colnames(pnl_s) <- c("Rolling Portfolio Strategy", "Equal Weight Portfolio")
dygraphs::dygraph(pnl_s, main="Rolling Portfolio Optimization Strategy") %>%
  dyOptions(colors=c("red", "blue"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
load("/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
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
al_pha <- 0.7 ; eigen_max <- 21
# Perform backtest in Rcpp
pnl_s <- HighFreq::back_test(excess=returns_100, returns=returns_100,
  startp=start_p, endp=end_p, alpha=al_pha, eigen_max=eigen_max, method="max_sharpe")
# Calculate returns on equal weight portfolio
in_dex <- xts::xts(rowMeans(returns_100), index(returns_100))
# Plot cumulative strategy returns
pnl_s <- cbind(pnl_s, in_dex, (pnl_s+in_dex)/2)
pnl_s <- cumsum(na.omit(pnl_s))
col_names <- c("Strategy", "Index", "Average")
colnames(pnl_s) <- col_names
dygraphs::dygraph(pnl_s[end_p], main="Rolling S&P500 Portfolio Optimization Strategy") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="red", strokeWidth=1) %>%
  dySeries(name=col_names[2], axis="y2", col="blue", strokeWidth=1) %>%
  dySeries(name=col_names[3], axis="y2", col="green", strokeWidth=2)
# Perform backtest over alphas
alpha_s <- seq(from=0.01, to=0.91, by=0.1)
pnl_s <- lapply(alpha_s, function(al_pha) {
  HighFreq::back_test(excess=returns_100, returns=returns_100,
  startp=start_p, endp=end_p, alpha=al_pha, eigen_max=eigen_max, method="max_sharpe")
})  # end lapply
pro_file <- sapply(pnl_s, sum)
plot(x=alpha_s, y=pro_file, t="l", main="Strategy PnL as Function of Shrinkage Intensity Alpha",
  xlab="Shrinkage Intensity Alpha", ylab="pnl")
al_pha <- alpha_s[which.max(pro_file)]
pnl_s <- pnl_s[[which.max(pro_file)]]
# Perform backtest over eigen_maxs
eigen_maxs <- seq(from=3, to=40, by=2)
pnl_s <- lapply(eigen_maxs, function(eigen_max) {
  HighFreq::back_test(excess=returns_100, returns=returns_100,
    startp=start_p, endp=end_p, alpha=al_pha, eigen_max=eigen_max, method="max_sharpe")
})  # end lapply
pro_file <- sapply(pnl_s, sum)
plot(x=eigen_maxs, y=pro_file, t="l", main="Strategy PnL as Function of eigen_max",
  xlab="eigen_max", ylab="pnl")
eigen_max <- eigen_maxs[which.max(pro_file)]
pnl_s <- pnl_s[[which.max(pro_file)]]
# Plot cumulative strategy returns
pnl_s <- cbind(pnl_s, in_dex, (pnl_s+in_dex)/2)
pnl_s <- cumsum(na.omit(pnl_s))
col_names <- c("Strategy", "Index", "Average")
colnames(pnl_s) <- col_names
dygraphs::dygraph(pnl_s[end_p], main="Optimal Rolling S&P500 Portfolio Strategy") %>%
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
  HighFreq::back_test(excess=returns_100, returns=returns_100,
    startp=start_p, endp=end_p, alpha=al_pha, eigen_max=eigen_max, method="max_sharpe")
})  # end lapply
pro_file <- sapply(pnl_s, sum)
plot(x=look_backs, y=pro_file, t="l", main="Strategy PnL as Function of Look-back Interval",
  xlab="Look-back Interval", ylab="pnl")
look_back <- look_backs[which.max(pro_file)]
pnl_s <- pnl_s[[which.max(pro_file)]]
# Plot cumulative strategy returns
pnl_s <- cbind(pnl_s, in_dex, (pnl_s+in_dex)/2)
pnl_s <- cumsum(na.omit(pnl_s))
col_names <- c("Strategy", "Index", "Average")
colnames(pnl_s) <- col_names
dygraphs::dygraph(pnl_s[end_p], main="Optimal Rolling S&P500 Portfolio Strategy") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="red", strokeWidth=1) %>%
  dySeries(name=col_names[2], axis="y2", col="blue", strokeWidth=1) %>%
  dySeries(name=col_names[3], axis="y2", col="green", strokeWidth=2)
NA
App setup code that runs only once at startup.
n_data <- 1e4
std_dev <- 1.0
Define the user interface
inter_face <- shiny::fluidPage(
  # Create numeric input for the number of data points.
  numericInput('n_data', "Number of data points:", value=n_data),
  # Create slider input for the standard deviation parameter.
  sliderInput("std_dev", label="Standard deviation:",
        min=0.1, max=3.0, value=std_dev, step=0.1),
  # Render plot in a panel.
  plotOutput("plo_t", height=300, width=500)
)  # end user interface
Define the server function
ser_ver <- function(input, output) {
  output$plo_t <- shiny::renderPlot({
    # Simulate the data
    da_ta <- rnorm(input$n_data, sd=input$std_dev)
    # Plot the data
    par(mar=c(2, 4, 4, 0), oma=c(0, 0, 0, 0))
    hist(da_ta, xlim=c(-4, 4), main="Histogram of Random Data")
  })  # end renderPlot
}  # end ser_ver
# Return a Shiny app object
shiny::shinyApp(ui=inter_face, server=ser_ver)
Create elements of the user interface
inter_face <- shiny::fluidPage(
  titlePanel("VWAP Moving Average"),
  # Create single row of widgets with two slider inputs
  fluidRow(
    # Input stock symbol
    column(width=3, selectInput("sym_bol", label="Symbol",
                          choices=sym_bols, selected=sym_bol)),
    # Input look-back interval
    column(width=3, sliderInput("look_back", label="Lookback interval",
                          min=1, max=150, value=11, step=1))
  ),  # end fluidRow
  # Create output plot panel
  mainPanel(dygraphs::dygraphOutput("dy_graph"), width=12)
)  # end fluidPage interface
Define the server function
ser_ver <- shiny::shinyServer(function(input, output) {
  # Get the close and volume data in a reactive environment
  clos_e <- shiny::reactive({
    # Get the data
    oh_lc <- get(input$sym_bol, data_env)
    clos_e <- log(quantmod::Cl(oh_lc))
    vol_ume <- quantmod::Vo(oh_lc)
    # Return the data
    cbind(clos_e, vol_ume)
  })  # end reactive code
  # Calculate the VWAP indicator in a reactive environment
  v_wap <- shiny::reactive({
    # Get model parameters from input argument
    look_back <- input$look_back
    # Calculate the VWAP indicator
    clos_e <- clos_e()[, 1]
    vol_ume <- clos_e()[, 2]
    v_wap <- HighFreq::roll_sum(se_ries=clos_e*vol_ume, look_back=look_back)
    volume_rolling <- HighFreq::roll_sum(se_ries=vol_ume, look_back=look_back)
    v_wap <- v_wap/volume_rolling
    v_wap[is.na(v_wap)] <- 0
    # Return the plot data
    da_ta <- cbind(clos_e, v_wap)
    colnames(da_ta) <- c(input$sym_bol, "VWAP")
    da_ta
  })  # end reactive code
  # Return the dygraph plot to output argument
  output$dy_graph <- dygraphs::renderDygraph({
    col_names <- colnames(v_wap())
    dygraphs::dygraph(v_wap(), main=paste(col_names[1], "VWAP")) %>%
dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=2, col="blue") %>%
dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=2, col="red")
  })  # end output plot
})  # end server code
Return a Shiny app object
shiny::shinyApp(ui=inter_face, server=ser_ver)
Define the server function
ser_ver <- shiny::shinyServer(function(input, output) {
  # Create an empty list of reactive values.
  value_s <- reactiveValues()
  # Get input parameters from the user interface.
  n_rows <- reactive({
    # Add n_rows to list of reactive values.
    value_s$n_rows <- input$n_rows
    input$n_rows
  })  # end reactive code
  # Broadcast a message to the console when the button is pressed.
  observeEvent(eventExpr=input$but_ton, handlerExpr={
    cat("Input button pressed\n")
  })  # end observeEvent
  # Send the data when the button is pressed.
  da_ta <- eventReactive(eventExpr=input$but_ton, valueExpr={
    # eventReactive() executes on input$but_ton, but not on n_rows() or input$n_rows.
    cat("Sending", n_rows(), "rows of data\n")
    da_ta <- head(mtcars, input$n_rows)
    value_s$mpg <- mean(da_ta$mpg)
    da_ta
  })  # end eventReactive
  #   da_ta
  # Draw table of the data when the button is pressed.
  observeEvent(eventExpr=input$but_ton, handlerExpr={
    da_ta <- da_ta()
    cat("Received", value_s$n_rows, "rows of data\n")
    cat("Average mpg = ", value_s$mpg, "\n")
    cat("Drawing table\n")
    output$tabl_e <- renderTable(da_ta)
  })  # end observeEvent
})  # end server code
Return a Shiny app object
shiny::shinyApp(ui=inter_face, server=ser_ver)
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
