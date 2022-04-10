# Extract ETF returns
symbols <- c("VTI", "IEF", "DBC")
returns <- rutils::etfenv$returns[, symbols]
returns <- na.omit(returns)
# Or, select rows with IEF data
# returns <- returns[index(rutils::etfenv$IEF)]
# Copy over NA values
# returns[1, is.na(returns[1, ])] <- 0
# returns <- zoo::na.locf(returns, na.rm=FALSE)
# Define end of month end points
endp <- rutils::calc_endpoints(returns, interval="months")
endp <- endp[-1]
nrows <- NROW(endp)
dates <- zoo::index(returns)[endp]
# Start points equal end points lagged by 12-month look-back interval
look_back <- 12
startp <- c(rep_len(1, look_back-1),
  endp[1:(nrows - look_back + 1)])
# Calculate matrix of look-back intervals
look_backs <- cbind(startp, endp)
colnames(look_backs) <- c("start", "end")
# Calculate matrix of look-forward intervals
look_fwds <- cbind(endp + 1, rutils::lagit(endp, -1))
look_fwds[nrows, ] <- endp[nrows]
colnames(look_fwds) <- c("start", "end")
# Inspect the intervals
head(cbind(look_backs, look_fwds))
tail(cbind(look_backs, look_fwds))
# Define performance function as Sharpe ratio
objfun <- function(returns) sum(returns)/sd(returns)
# Calculate past performance over look-back intervals
past <- apply(look_backs, 1, function(ep) {
  sapply(returns[ep[1]:ep[2]], objfun)
})  # end sapply
past <- t(past)
past[is.na(past)] <- 0
# Weights are proportional to past performance
weightv <- past
# weightv[weightv < 0] <- 0
# Scale weightv so sum of squares is equal to 1.
weightv <- weightv/sqrt(rowSums(weightv^2))
# Or scale weightv so sum is equal to 1
# weightv <- weightv/rowSums(weightv)
# Set NA values to zero
weightv[is.na(weightv)] <- 0
sum(is.na(weightv))
# Calculate future out-of-sample performance
future <- apply(look_fwds, 1, function(ep) {
  sapply(returns[ep[1]:ep[2]], sum)
})  # end sapply
future <- t(future)
future[is.na(future)] <- 0
tail(future)
# Calculate the momentum pnls
pnls <- rowSums(weightv*future)
# Lag the future and momentum returns to proper dates
future <- rutils::lagit(future)
pnls <- rutils::lagit(pnls)
# The momentum strategy has low correlation to stocks
cor(pnls, future)
# Define all-weather benchmark
weightsaw <- c(0.30, 0.55, 0.15)
all_weather <- future %*% weightsaw
# Calculate the wealth of momentum returns
wealth <- xts::xts(cbind(all_weather, pnls), order.by=dates)
colnames(wealth) <- c("All-Weather", "Momentum")
cor(wealth)
# Plot dygraph of the momentum strategy returns
dygraphs::dygraph(cumsum(wealth), main="Monthly Momentum Strategy vs All-Weather") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Define backtest functional
backtestmom <- function(returns,
                objfun=function(returns) (sum(returns)/sd(returns)),
                look_back=12, rfreq="months", bid_offer=0.001,
                endp=rutils::calc_endpoints(returns, interval=rfreq)[-1],
                with_weights=FALSE, ...) {
  stopifnot("package:rutils" %in% search() || require("rutils", quietly=TRUE))
  # Define look-back and look-forward intervals
  nrows <- NROW(endp)
  startp <- c(rep_len(1, look_back-1), endp[1:(nrows-look_back+1)])
  # Calculate look-back intervals
  look_backs <- cbind(startp, endp)
  # Calculate look-forward intervals
  look_fwds <- cbind(endp + 1, rutils::lagit(endp, -1))
  look_fwds[nrows, ] <- endp[nrows]
  # Calculate past performance over look-back intervals
  past <- t(apply(look_backs, 1, function(ep) sapply(returns[ep[1]:ep[2]], objfun)))
  past[is.na(past)] <- 0
  # Calculate future performance
  future <- t(apply(look_fwds, 1, function(ep) sapply(returns[ep[1]:ep[2]], sum)))
  future[is.na(future)] <- 0
  # Scale weightv so sum of squares is equal to 1
  weightv <- past
  weightv <- weightv/sqrt(rowSums(weightv^2))
  weightv[is.na(weightv)] <- 0  # Set NA values to zero
  # Calculate momentum profits and losses
  pnls <- rowSums(weightv*future)
  # Calculate transaction costs
  costs <- 0.5*bid_offer*cumprod(1 + pnls)*rowSums(abs(rutils::diffit(weightv)))
  pnls <- (pnls - costs)
  if (with_weights)
    rutils::lagit(cbind(pnls, weightv))
  else
    rutils::lagit(pnls)
}  # end backtestmom
source("/Users/jerzy/Develop/lecture_slides/scripts/back_test.R")
look_backs <- seq(3, 15, by=1)
objfun <- function(returns) sum(returns)/sd(returns)
profilev <- sapply(look_backs, function(look_back) {
  pnls <- backtestmom(returns=returns, endp=endp,
    look_back=look_back, objfun=objfun)
  sum(pnls)
})  # end sapply
# Plot momemntum PnLs
x11(width=6, height=5)
plot(x=look_backs, y=profilev, t="l",
  main="Momemntum PnL as function of look_back",
  xlab="look_back (months)", ylab="pnl")
# Optimal look_back
look_back <- look_backs[which.max(profilev)]
pnls <- backtestmom(returns=returns,
  look_back=look_back, endp=endp,
  objfun=objfun, with_weights=TRUE)
tail(pnls)
# Calculate the wealth of momentum returns
retmom <- pnls[, 1]
wealth <- xts::xts(cbind(all_weather, retmom), order.by=dates)
colnames(wealth) <- c("All-Weather", "Momentum")
cor(wealth)
# Plot dygraph of the momentum strategy returns
dygraphs::dygraph(cumsum(wealth), main="Monthly Momentum Strategy vs All-Weather") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Or
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue")
quantmod::chart_Series(cumsum(wealth), theme=plot_theme, lwd=2,
       name="Momentum PnL")
legend("topleft", legend=colnames(wealth),
  inset=0.1, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")
# Plot the momentum portfolio weights
weightv <- pnls[, -1]
vti <- log(quantmod::Cl(rutils::etfenv$VTI[dates]))
colnames(vti) <- "VTI"
datav <- cbind(vti, weightv)
datav <- na.omit(datav)
colnames(datav)[2:NCOL(pnls)] <- paste0(colnames(weightv), "_weight")
zoo::plot.zoo(datav, xlab=NULL, main="Momentum Weights")
# Calculate ETF betas
betas_etf <- sapply(returns, function(x)
  cov(returns$VTI, x)/var(x))
# Momentum beta is equal weights times ETF betas
betas <- weightv %*% betas_etf
betas <- xts::xts(betas, order.by=dates)
colnames(betas) <- "momentum_beta"
datav <- cbind(betas, vti)
zoo::plot.zoo(datav,
  oma = c(3, 1, 3, 0), mar = c(0, 4, 0, 1),
  main="Momentum Beta & VTI Price", xlab="")
# Open x11 for plotting and set parameters to reduce whitespace around plot
x11(width=6, height=5)
par(mar=c(4, 4, 3, 1), oma=c(0, 0, 0, 0))
# Merton-Henriksson test
vti <- rutils::diffit(vti)
design <- cbind(VTI=vti, 0.5*(vti+abs(vti)), vti^2)
colnames(design)[2:3] <- c("merton", "treynor")
model <- lm(retmom ~ VTI + merton, data=design); summary(model)
# Treynor-Mazuy test
model <- lm(retmom ~ VTI + treynor, data=design); summary(model)
# Plot residual scatterplot
plot.default(x=vti, y=retmom, xlab="VTI", ylab="momentum")
title(main="Treynor-Mazuy market timing test\n for Momentum vs VTI", line=0.5)
# Plot fitted (predicted) response values
points.default(x=vti, y=model$fitted.values, pch=16, col="red")
residuals <- model$residuals
text(x=0.0, y=max(residuals), paste("Treynor test t-value =", round(summary(model)$coeff["treynor", "t value"], 2)))
# Standardize the returns
retmom_std <- (retmom-mean(retmom))/sd(retmom)
vti <- (vti-mean(vti))/sd(vti)
# Calculate skewness and kurtosis
apply(cbind(retmom_std, vti), 2, function(x)
  sapply(c(skew=3, kurt=4),
    function(e) sum(x^e)))/nrows
# Plot histogram
hist(retmom_std, breaks=30,
  main="Momentum and VTI Return Distributions (standardized",
  xlim=c(-4, 4),
  xlab="", ylab="", freq=FALSE)
# Draw kernel density of histogram
lines(density(retmom_std), col='red', lwd=2)
lines(density(vti), col='blue', lwd=2)
# Add legend
legend("topright", inset=0.05, cex=0.8, title=NULL,
 leg=c("Momentum", "VTI"),
 lwd=6, bg="white", col=c("red", "blue"))
# Combine momentum strategy with all-weather
all_weather <- sd(retmom)*all_weather/sd(all_weather)
wealth <- cbind(retmom, all_weather, 0.5*(retmom + all_weather))
colnames(wealth) <- c("momentum", "all_weather", "combined")
# Calculate strategy annualized Sharpe ratios
apply(wealth, MARGIN=2, function(x) {
  sqrt(12)*sum(x)/sd(x)/NROW(x)
})  # end apply
# Calculate strategy correlations
cor(wealth)
# Calculate cumulative wealth
wealth <- xts::xts(wealth, dates)
# Plot ETF momentum strategy combined with All-Weather
dygraphs::dygraph(cumsum(wealth), main="ETF Momentum Strategy Combined with All-Weather") %>%
  dyOptions(colors=c("red", "blue", "green"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Or
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("green", "blue", "red")
quantmod::chart_Series(wealth, theme=plot_theme,
       name="ETF Momentum Strategy Combined with All-Weather")
legend("topleft", legend=colnames(wealth),
  inset=0.1, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")
# Calculate rolling variance
look_back <- 252
variance <- roll::roll_var(returns, width=look_back, min_obs=1)
variance[1, ] <- 1
# Calculate rolling Sharpe
past <- roll::roll_mean(returns, width=look_back, min_obs=1)
weightv <- past/sqrt(variance)
weightv <- weightv/sqrt(rowSums(weightv^2))
weightv <- rutils::lagit(weightv)
sum(is.na(weightv))
# Calculate momentum profits and losses
pnls <- rowMeans(weightv*returns)
# Calculate transaction costs
bid_offer <- 0.001
costs <- 0.5*bid_offer*rowSums(abs(rutils::diffit(weightv)))
pnls <- (pnls - costs)
# Define all-weather benchmark
weightsaw <- c(0.30, 0.55, 0.15)
all_weather <- returns %*% weightsaw
# Calculate the wealth of momentum returns
wealth <- xts::xts(cbind(all_weather, pnls), order.by=index(returns))
colnames(wealth) <- c("All-Weather", "Momentum")
cor(wealth)
# Plot dygraph of the momentum strategy returns
dygraphs::dygraph(cumsum(wealth), main="Daily Momentum Strategy vs All-Weather") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Define backtest functional for daily momentum strategy
# If trend=(-1) then it backtests a mean reverting strategy
momentum_daily <- function(returns, look_back=252, bid_offer=0.001, trend=1, ...) {
  stopifnot("package:quantmod" %in% search() || require("quantmod", quietly=TRUE))
  # Calculate rolling variance
  variance <- roll::roll_var(returns, width=look_back, min_obs=1)
  variance[1, ] <- 1
  variance[variance <= 0] <- 1
# Calculate rolling Sharpe
  past <- roll::roll_mean(returns, width=look_back, min_obs=1)
  weightv <- past/sqrt(variance)
  weightv <- weightv/sqrt(rowSums(weightv^2))
  weightv <- rutils::lagit(weightv)
  # Calculate momentum profits and losses
  pnls <- trend*rowMeans(weightv*returns)
  # Calculate transaction costs
  costs <- 0.5*bid_offer*rowSums(abs(rutils::diffit(weightv)))
  (pnls - costs)
}  # end momentum_daily
# Simulate a daily ETF momentum strategy
source("/Users/jerzy/Develop/lecture_slides/scripts/back_test.R")
pnls <- momentum_daily(returns=returns, look_back=252,
  bid_offer=bid_offer)
# Perform sapply loop over look_backs
look_backs <- seq(50, 300, by=50)
pnls <- sapply(look_backs, momentum_daily,
  returns=returns, bid_offer=bid_offer)
colnames(pnls) <- paste0("look_back=", look_backs)
pnls <- xts::xts(pnls, zoo::index(returns))
tail(pnls)
# Plot dygraph of daily ETF momentum strategies
colors <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls), main="Daily ETF Momentum Strategies") %>%
  dyOptions(colors=colors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Plot EWMA strategies with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(pnls))
quantmod::chart_Series(cumsum(pnls),
  theme=plot_theme, name="Cumulative Returns of Daily ETF Momentum Strategies")
legend("bottomleft", legend=colnames(pnls),
  inset=0.02, bg="white", cex=0.7, lwd=rep(6, NCOL(returns)),
  col=plot_theme$col$line.col, bty="n")
# Define backtest functional for daily momentum strategy
# If trend=(-1) then it backtests a mean reverting strategy
momentum_daily <- function(returns, look_back=252, holdperiod=5, bid_offer=0.001, trend=1, ...) {
  stopifnot("package:quantmod" %in% search() || require("quantmod", quietly=TRUE))
  # Calculate rolling variance
  variance <- roll::roll_var(returns, width=look_back, min_obs=1)
  variance[1, ] <- 1
  variance[variance <= 0] <- 1
  # Calculate rolling Sharpe
  past <- roll::roll_mean(returns, width=look_back, min_obs=1)
  weightv <- past/sqrt(variance)
  weightv <- weightv/sqrt(rowSums(weightv^2))
  weightv <- rutils::lagit(weightv)
  # Average the weights over holding period
  weightv <- roll::roll_mean(weightv, width=holdperiod, min_obs=1)
  # Calculate momentum profits and losses
  pnls <- trend*rowMeans(weightv*returns)
  # Calculate transaction costs
  costs <- 0.5*bid_offer*rowSums(abs(rutils::diffit(weightv)))
  (pnls - costs)
}  # end momentum_daily
# Perform sapply loop over holding periods
holdperiods <- seq(2, 11, by=2)
pnls <- sapply(holdperiods, momentum_daily, look_back=120,
            returns=returns, bid_offer=bid_offer)
colnames(pnls) <- paste0("holding=", holdperiods)
pnls <- xts::xts(pnls, zoo::index(returns))
# Plot dygraph of daily ETF momentum strategies
colors <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls), main="Daily ETF Momentum Strategies with Holding Period") %>%
  dyOptions(colors=colors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Plot EWMA strategies with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(pnls))
quantmod::chart_Series(cumsum(pnls),
  theme=plot_theme, name="Cumulative Returns of Daily ETF Momentum Strategies")
legend("bottomleft", legend=colnames(pnls),
  inset=0.02, bg="white", cex=0.7, lwd=rep(6, NCOL(returns)),
  col=plot_theme$col$line.col, bty="n")
# Load daily S&P500 percentage stock returns.
load(file="/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
# Overwrite NA values in returns100
returns100 <- returns100["2000/"]
returns100[1, is.na(returns100[1, ])] <- 0
returns100 <- zoo::na.locf(returns100, na.rm=FALSE)
# Simulate a daily S&P500 momentum strategy.
# Perform sapply loop over look_backs
look_backs <- seq(100, 300, by=20)
pnls <- sapply(look_backs, momentum_daily,
  holdperiod=5, returns=returns100, bid_offer=0)
colnames(pnls) <- paste0("look_back=", look_backs)
pnls <- xts::xts(pnls, zoo::index(returns100))
# Plot dygraph of daily ETF momentum strategies
colors <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls), main="Daily S&P500 Momentum Strategies") %>%
  dyOptions(colors=colors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Plot daily S&P500 momentum strategies with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
quantmod::chart_Series(cumsum(pnls),
  theme=plot_theme, name="Daily S&P500 Momentum Strategies")
legend("bottomleft", legend=colnames(pnls),
  inset=0.02, bg="white", cex=0.7, lwd=rep(6, NCOL(returns)),
  col=plot_theme$col$line.col, bty="n")
# Perform sapply loop over look_backs
look_backs <- seq(3, 20, by=2)
pnls <- sapply(look_backs, momentum_daily,
  holdperiod=5, returns=returns100, bid_offer=0, trend=(-1))
colnames(pnls) <- paste0("look_back=", look_backs)
pnls <- xts::xts(pnls, zoo::index(returns100))
# Plot dygraph of daily ETF momentum strategies
colors <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls), main="Daily S&P500 Momentum Strategies") %>%
  dyOptions(colors=colors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Plot EWMA strategies with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
quantmod::chart_Series(cumsum(pnls),
  theme=plot_theme, name="Cumulative Returns of S&P500 Mean Reverting Strategies")
legend("topleft", legend=colnames(pnls),
  inset=0.05, bg="white", cex=0.7, lwd=rep(6, NCOL(returns)),
  col=plot_theme$col$line.col, bty="n")
# Plot cumulative returns of VTI vs MTUM ETF
wealth <- log(na.omit(rutils::etfenv$prices[, c("VTI", "MTUM")]))
colnames(wealth) <- c("VTI", "MTUM")
wealth <- rutils::diffit(wealth)
dygraphs::dygraph(cumsum(wealth), main="VTI vs MTUM ETF") %>%
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
double inner_lagmugar(NumericVector x, NumericVector y) {
  return sum(x * y);
}")  # end cppFunction
# Run Rcpp Sugar function
inner_lagmugar(1:3, 6:4)
inner_lagmugar(1:3, 6:3)
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
  r_cpp_sugar=inner_lagmugar(1:10000, 1:10000),
  times=10))[, c(1, 4, 5)]
# Define Ornstein-Uhlenbeck function in R
sim_ou <- function(nrows=1000, eq_price=5.0,
              volat=0.01, theta=0.01) {
  returns <- numeric(nrows)
  prices <- numeric(nrows)
  prices[1] <- eq_price
  for (i in 2:nrows) {
    returns[i] <- thetav*(eq_price - prices[i-1]) + volat*rnorm(1)
    prices[i] <- prices[i-1] + returns[i]
  }  # end for
  prices
}  # end sim_ou
# Simulate Ornstein-Uhlenbeck process in R
eq_price <- 5.0; sigmav <- 0.01
thetav <- 0.01; nrows <- 1000
set.seed(1121)  # Reset random numbers
ou_sim <- sim_ou(nrows=nrows, eq_price=eq_price, volat=sigmav, theta=thetav)
# Define Ornstein-Uhlenbeck function in Rcpp
Rcpp::cppFunction("
NumericVector sim_ou_rcpp(double eq_price,
                double volat,
                double thetav,
                NumericVector innov) {
  int nrows = innov.size();
  NumericVector prices(nrows);
  NumericVector returns(nrows);
  prices[0] = eq_price;
  for (int it = 1; it < nrows; it++) {
    returns[it] = thetav*(eq_price - prices[it-1]) + volat*innov[it-1];
    prices[it] = prices[it-1] + returns[it];
  }  // end for
  return prices;
}")  # end cppFunction
# Simulate Ornstein-Uhlenbeck process in Rcpp
set.seed(1121)  # Reset random numbers
ou_sim_rcpp <- sim_ou_rcpp(eq_price=eq_price,
  volat=sigmav,
  theta=thetav,
  innov=rnorm(nrows))
all.equal(ou_sim, ou_sim_rcpp)
# Compare speed of Rcpp and R
library(microbenchmark)
summary(microbenchmark(
  pure_r=sim_ou(nrows=nrows, eq_price=eq_price, volat=sigmav, theta=thetav),
  r_cpp=sim_ou_rcpp(eq_price=eq_price, volat=sigmav, theta=thetav, innov=rnorm(nrows)),
  times=10))[, c(1, 4, 5)]
# Source Rcpp function for Ornstein-Uhlenbeck process from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/sim_ou.cpp")
# Simulate Ornstein-Uhlenbeck process in Rcpp
set.seed(1121)  # Reset random numbers
ou_sim_rcpp <- sim_ou_rcpp(eq_price=eq_price,
  volat=sigmav,
  theta=thetav,
  innov=rnorm(nrows))
all.equal(ou_sim, ou_sim_rcpp)
# Compare speed of Rcpp and R
library(microbenchmark)
summary(microbenchmark(
  pure_r=sim_ou(nrows=nrows, eq_price=eq_price, volat=sigmav, theta=thetav),
  r_cpp=sim_ou_rcpp(eq_price=eq_price, volat=sigmav, theta=thetav, innov=rnorm(nrows)),
  times=10))[, c(1, 4, 5)]
# Calculate uniformly distributed pseudo-random sequence
unifun <- function(seedv, nrows=10) {
  output <- numeric(nrows)
  output[1] <- seedv
  for (i in 2:nrows) {
    output[i] <- 4*output[i-1]*(1-output[i-1])
  }  # end for
  acos(1-2*output)/pi
}  # end unifun
# Source Rcpp functions from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/unifun.cpp")
# Microbenchmark Rcpp code
library(microbenchmark)
summary(microbenchmark(
  pure_r=runif(1e5),
  rloop=unifun(0.3, 1e5),
  r_cpp=uniform_rcpp(0.3, 1e5),
  times=10))[, c(1, 4, 5)]
library(RcppArmadillo)
# Source Rcpp functions from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/armadillofuntions.cpp")
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
coeff <- c(0.9, 0.09)
nrows <- 1e4
set.seed(1121)
innov <- rnorm(nrows)
# Simulate ARIMA using filter()
arima_filter <- filter(x=innov,
  filter=coeff, method="recursive")
# Simulate ARIMA using sim_arima()
arimav <- sim_arima(innov, rev(coeff))
all.equal(drop(arimav),
  as.numeric(arima_filter))
# Microbenchmark RcppArmadillo code
summary(microbenchmark(
  sim_arima = sim_arima(innov, rev(coeff)),
  filter = filter(x=innov, filter=coeff, method="recursive"),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
library(RcppArmadillo)
# Source Rcpp functions from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/armadillofuntions.cpp")
matrixv <- matrix(runif(1e5), nc=1e3)
# De-mean using apply()
newmapt <- apply(matrixv, 2, function(x) (x-mean(x)))
# De-mean using demean_mat()
demean_mat(matrixv)
all.equal(newmapt, matrixv)
# Microbenchmark RcppArmadillo code
library(microbenchmark)
summary(microbenchmark(
  apply = (apply(matrixv, 2, mean)),
  demean_mat = demean_mat(matrixv),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
# Perform matrix inversion
# Create random positive semi-definite matrix
matrixv <- matrix(runif(25), nc=5)
matrixv <- t(matrixv) %*% matrixv
# Invert the matrix
matrix_inv <- solve(matrixv)
inv_mat(matrixv)
all.equal(matrix_inv, matrixv)
# Microbenchmark RcppArmadillo code
summary(microbenchmark(
  solve = solve(matrixv),
  inv_mat = inv_mat(matrixv),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
library(RcppArmadillo)
# Source Rcpp functions from file
Rcpp::sourceCpp("/Users/jerzy/Develop/lecture_slides/scripts/calc_weights.cpp")
# Calculate matrix of random returns
matrixv <- matrix(rnorm(300), nc=5)
# Regularized inverse of correlation matrix
eigen_max <- 4
cormat <- cor(matrixv)
eigend <- eigen(cormat)
inverse <- eigend$vectors[, 1:eigen_max] %*%
  (t(eigend$vectors[, 1:eigen_max]) / eigend$values[1:eigen_max])
# Regularized inverse using RcppArmadillo
inverse_arma <- calc_inv(cormat, eigen_max=eigen_max)
all.equal(inverse, inverse_arma)
# Microbenchmark RcppArmadillo code
library(microbenchmark)
summary(microbenchmark(
  Rcode = {eigend <- eigen(cormat)
eigend$vectors[, 1:eigen_max] %*% (t(eigend$vectors[, 1:eigen_max]) / eigend$values[1:eigen_max])},
  Rcpp = calc_inv(cormat, eigen_max=eigen_max),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
# Select all the ETF symbols except "VXX", "SVXY" and "MTUM"
symbols <- colnames(rutils::etfenv$returns)
symbols <- symbols[!(symbols %in% c("VXX", "SVXY", "MTUM", "QUAL", "VLUE", "USMV"))]
# Extract columns of rutils::etfenv$returns and overwrite NA values
returns <- rutils::etfenv$returns[, symbols]
nassets <- NCOL(returns)
# returns <- na.omit(returns)
returns[1, is.na(returns[1, ])] <- 0
returns <- zoo::na.locf(returns, na.rm=FALSE)
# Returns in excess of risk-free rate
riskf <- 0.03/252
excess <- (returns - riskf)
# Maximum Sharpe weights in-sample interval
rets_is <- returns["/2014"]
inverse <- MASS::ginv(cov(rets_is))
weightv <- inverse %*% colMeans(excess["/2014"])
weightv <- drop(weightv/sqrt(sum(weightv^2)))
names(weightv) <- colnames(returns)
# Plot portfolio weights
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
barplot(sort(weightv), main="Maximum Sharpe Weights", cex.names=0.7)
# Calculate portfolio returns
portf_is <- xts::xts(rets_is %*% weightv, zoo::index(rets_is))
indeks <- xts::xts(rowSums(rets_is)/sqrt(nassets), zoo::index(rets_is))
portf_is <- portf_is*sd(indeks)/sd(portf_is)
# Plot cumulative portfolio returns
pnls <- cumsum(cbind(portf_is, indeks))
colnames(pnls) <- c("Optimal Portfolio", "Equal Weight Portfolio")
dygraphs::dygraph(pnls, main="In-sample Optimal Portfolio Returns") %>%
  dyOptions(colors=c("red", "blue"), strokeWidth=2) %>%
  dyLegend(width=500)
# Out-of-sample portfolio returns
rets_os <- returns["2015/"]
portf_os <- xts::xts(rets_os %*% weightv, zoo::index(rets_os))
indeks <- xts::xts(rowSums(rets_os)/sqrt(nassets), zoo::index(rets_os))
portf_os <- portf_os*sd(indeks)/sd(portf_os)
pnls <- cbind(portf_os, indeks, (portf_os + indeks)/2)
colnames(pnls) <- c("Optimal", "Equal Weight", "Combined")
sapply(pnls, function(x) mean(x)/sd(x))
# Plot cumulative portfolio returns
dygraphs::dygraph(cumsum(pnls), main="Out-of-sample Optimal Portfolio Returns") %>%
  dyOptions(colors=c("red", "blue", "green"), strokeWidth=2) %>%
  dyLegend(width=500)
load("/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
# Overwrite NA values in returns
returns <- returns["2000/"]
nassets <- NCOL(returns)
returns[1, is.na(returns[1, ])] <- 0
returns <- zoo::na.locf(returns, na.rm=FALSE)
riskf <- 0.03/252
excess <- (returns - riskf)
rets_is <- returns["/2010"]
rets_os <- returns["2011/"]
# Maximum Sharpe weights in-sample interval
covmat <- cov(rets_is)
inverse <- MASS::ginv(covmat)
weightv <- inverse %*% colMeans(excess["/2010"])
weightv <- drop(weightv/sqrt(sum(weightv^2)))
names(weightv) <- colnames(returns)
# Calculate portfolio returns
portf_is <- xts::xts(rets_is %*% weightv, zoo::index(rets_is))
portf_os <- xts::xts(rets_os %*% weightv, zoo::index(rets_os))
indeks <- xts::xts(rowSums(returns)/sqrt(nassets), zoo::index(returns))
# Plot cumulative portfolio returns
pnls <- rbind(portf_is, portf_os)
pnls <- pnls*sd(indeks)/sd(pnls)
pnls <- cumsum(cbind(pnls, indeks))
colnames(pnls) <- c("Optimal Portfolio", "Equal Weight Portfolio")
dygraphs::dygraph(pnls, main="Out-of-sample Optimal Portfolio Returns for Stocks") %>%
  dyOptions(colors=c("red", "blue"), strokeWidth=2) %>%
  dyEvent(index(last(rets_is[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=500)
# Create rectangular matrix with collinear columns
ran_dom <- matrix(rnorm(10*8), nc=10)
# Calculate covariance matrix
covmat <- cov(ran_dom)
# Calculate inverse of covmat - error
inverse <- solve(covmat)
# Perform eigen decomposition
eigend <- eigen(covmat)
eigen_vec <- eigend$vectors
eigen_val <- eigend$values
# Set tolerance for determining zero singular values
precision <- sqrt(.Machine$double.eps)
# Calculate regularized inverse matrix
not_zero <- (eigen_val > (precision * eigen_val[1]))
inv_reg <- eigen_vec[, not_zero] %*%
  (t(eigen_vec[, not_zero]) / eigen_val[not_zero])
# Verify inverse property of inv_reg
all.equal(covmat, covmat %*% inv_reg %*% covmat)
# Calculate regularized inverse of covmat
inverse <- MASS::ginv(covmat)
# Verify inverse property of matrixv
all.equal(inverse, inv_reg)
# Calculate in-sample covariance matrix
covmat <- cov(rets_is)
eigend <- eigen(covmat)
eigen_vec <- eigend$vectors
eigen_val <- eigend$values
# Calculate shrinkage inverse of covariance matrix
eigen_max <- 21
inverse <- eigen_vec[, 1:eigen_max] %*%
  (t(eigen_vec[, 1:eigen_max]) / eigend$values[1:eigen_max])
# Calculate portfolio weights
weightv <- inverse %*% colMeans(excess["/2010"])
weightv <- drop(weightv/sqrt(sum(weightv^2)))
names(weightv) <- colnames(returns)
# Calculate portfolio returns
portf_is <- xts::xts(rets_is %*% weightv, zoo::index(rets_is))
portf_os <- xts::xts(rets_os %*% weightv, zoo::index(rets_os))
indeks <- xts::xts(rowSums(returns)/sqrt(nassets), zoo::index(returns))
# Plot cumulative portfolio returns
pnls <- rbind(portf_is, portf_os)
pnls <- pnls*sd(indeks)/sd(pnls)
pnls <- cumsum(cbind(pnls, indeks))
colnames(pnls) <- c("Optimal Portfolio", "Equal Weight Portfolio")
dygraphs::dygraph(pnls, main="Regularized Out-of-sample Optimal Portfolio Returns for Stocks") %>%
  dyOptions(colors=c("red", "blue"), strokeWidth=2) %>%
  dyEvent(index(last(rets_is[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=500)
# Shrink the in-sample returns to their mean
rets_mean <- colMeans(rets_is) - riskf
alpha <- 0.7
rets_mean <- (1 - alpha)*rets_mean + alpha*mean(rets_mean)
# Calculate portfolio weights
weightv <- inverse %*% rets_mean
weightv <- drop(weightv/sqrt(sum(weightv^2)))
# Calculate portfolio returns
portf_is <- xts::xts(rets_is %*% weightv, zoo::index(rets_is))
portf_os <- xts::xts(rets_os %*% weightv, zoo::index(rets_os))
# Plot cumulative portfolio returns
pnls <- rbind(portf_is, portf_os)
pnls <- pnls*sd(indeks)/sd(pnls)
pnls <- cumsum(cbind(pnls, indeks))
colnames(pnls) <- c("Optimal Portfolio", "Equal Weight Portfolio")
dygraphs::dygraph(pnls, main="Out-of-sample Returns for Stocks With Regularization and Shrinkage") %>%
  dyOptions(colors=c("red", "blue"), strokeWidth=2) %>%
  dyEvent(index(last(rets_is[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=500)
library(RcppArmadillo)
# Source Rcpp functions from file
Rcpp::sourceCpp("/Users/jerzy/Develop/lecture_slides/scripts/calc_weights.cpp")
# Create random matrix of returns
matrixv <- matrix(rnorm(300), nc=5)
# Regularized inverse of covariance matrix
eigen_max <- 4
eigend <- eigen(cov(matrixv))
covinv <- eigend$vectors[, 1:eigen_max] %*%
  (t(eigend$vectors[, 1:eigen_max]) / eigend$values[1:eigen_max])
# Regularized inverse using RcppArmadillo
covinv_arma <- calc_inv(matrixv, eigen_max)
all.equal(covinv, covinv_arma)
# Microbenchmark RcppArmadillo code
library(microbenchmark)
summary(microbenchmark(
  pure_r={
    eigend <- eigen(cov(matrixv))
    eigend$vectors[, 1:eigen_max] %*%
(t(eigend$vectors[, 1:eigen_max]) / eigend$values[1:eigen_max])
  },
  r_cpp=calc_inv(matrixv, eigen_max),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
# Calculate vector of monthly end points and start points
endp <- rutils::calc_endpoints(returns, interval="months")
endp <- endp[endp > 2*NCOL(returns)]
nrows <- NROW(endp)
look_back <- 24
startp <- c(rep_len(0, look_back-1),
       endp[1:(nrows-look_back+1)])
# Perform loop over end points
rets_portf <- lapply(2:nrows, function(i) {
    # Subset the excess returns
    excess <- excess[startp[i-1]:endp[i-1], ]
    inverse <- MASS::ginv(cov(excess))
    # Calculate the maximum Sharpe ratio portfolio weights
    weightv <- inverse %*% colMeans(excess)
    weightv <- drop(weightv/sqrt(sum(weightv^2)))
    # Calculate the out-of-sample portfolio returns
    returns <- returns[(endp[i-1]+1):endp[i], ]
    xts::xts(returns %*% weightv, zoo::index(returns))
})  # end lapply
rets_portf <- rutils::do_call(rbind, rets_portf)
# Plot cumulative strategy returns
indeks <- xts::xts(rowSums(returns)/sqrt(nassets), zoo::index(returns))
pnls <- cumsum(na.omit(cbind(rets_portf, indeks*sd(rets_portf)/sd(indeks))))
colnames(pnls) <- c("Rolling Portfolio Strategy", "Equal Weight Portfolio")
dygraphs::dygraph(pnls, main="Rolling Portfolio Optimization Strategy") %>%
  dyOptions(colors=c("red", "blue"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
load("/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
# Overwrite NA values in returns
returns100[1, is.na(returns100[1, ])] <- 0
returns100 <- zoo::na.locf(returns100, na.rm=FALSE)
ncols <- NCOL(returns100) ; dates <- index(returns100)
# Define monthly end points
endp <- rutils::calc_endpoints(returns100, interval="months")
endp <- endp[endp > (ncols+1)]
nrows <- NROW(endp) ; look_back <- 12
startp <- c(rep_len(0, look_back-1), endp[1:(nrows-look_back+1)])
endp <- (endp - 1)
startp <- (startp - 1)
startp[startp < 0] <- 0
alpha <- 0.7 ; eigen_max <- 21
# Perform backtest in Rcpp
pnls <- HighFreq::back_test(excess=returns100, returns=returns100,
  startp=startp, endp=endp, alpha=alpha, eigen_max=eigen_max, method="max_sharpe")
# Calculate returns on equal weight portfolio
indeks <- xts::xts(rowMeans(returns100), zoo::index(returns100))
# Plot cumulative strategy returns
pnls <- cbind(pnls, indeks, (pnls+indeks)/2)
pnls <- cumsum(na.omit(pnls))
colnames <- c("Strategy", "Index", "Average")
colnames(pnls) <- colnames
dygraphs::dygraph(pnls[endp], main="Rolling S&P500 Portfolio Optimization Strategy") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", col="red", strokeWidth=1) %>%
  dySeries(name=colnames[2], axis="y2", col="blue", strokeWidth=1) %>%
  dySeries(name=colnames[3], axis="y2", col="green", strokeWidth=2)
# Perform backtest over alphas
alpha_s <- seq(from=0.01, to=0.91, by=0.1)
pnls <- lapply(alpha_s, function(alpha) {
  HighFreq::back_test(excess=returns100, returns=returns100,
  startp=startp, endp=endp, alpha=alpha, eigen_max=eigen_max, method="max_sharpe")
})  # end lapply
profilev <- sapply(pnls, sum)
plot(x=alpha_s, y=profilev, t="l", main="Strategy PnL as Function of Shrinkage Intensity Alpha",
  xlab="Shrinkage Intensity Alpha", ylab="pnl")
alpha <- alpha_s[which.max(profilev)]
pnls <- pnls[[which.max(profilev)]]
# Perform backtest over eigen_maxs
eigen_maxs <- seq(from=3, to=40, by=2)
pnls <- lapply(eigen_maxs, function(eigen_max) {
  HighFreq::back_test(excess=returns100, returns=returns100,
    startp=startp, endp=endp, alpha=alpha, eigen_max=eigen_max, method="max_sharpe")
})  # end lapply
profilev <- sapply(pnls, sum)
plot(x=eigen_maxs, y=profilev, t="l", main="Strategy PnL as Function of eigen_max",
  xlab="eigen_max", ylab="pnl")
eigen_max <- eigen_maxs[which.max(profilev)]
pnls <- pnls[[which.max(profilev)]]
# Plot cumulative strategy returns
pnls <- cbind(pnls, indeks, (pnls+indeks)/2)
pnls <- cumsum(na.omit(pnls))
colnames <- c("Strategy", "Index", "Average")
colnames(pnls) <- colnames
dygraphs::dygraph(pnls[endp], main="Optimal Rolling S&P500 Portfolio Strategy") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", col="red", strokeWidth=1) %>%
  dySeries(name=colnames[2], axis="y2", col="blue", strokeWidth=1) %>%
  dySeries(name=colnames[3], axis="y2", col="green", strokeWidth=2)
# Perform backtest over look-backs
look_backs <- seq(from=3, to=24, by=1)
pnls <- lapply(look_backs, function(look_back) {
  startp <- c(rep_len(0, look_back-1), endp[1:(nrows-look_back+1)])
  startp <- (startp - 1)
  startp[startp < 0] <- 0
  HighFreq::back_test(excess=returns100, returns=returns100,
    startp=startp, endp=endp, alpha=alpha, eigen_max=eigen_max, method="max_sharpe")
})  # end lapply
profilev <- sapply(pnls, sum)
plot(x=look_backs, y=profilev, t="l", main="Strategy PnL as Function of Look-back Interval",
  xlab="Look-back Interval", ylab="pnl")
look_back <- look_backs[which.max(profilev)]
pnls <- pnls[[which.max(profilev)]]
# Plot cumulative strategy returns
pnls <- cbind(pnls, indeks, (pnls+indeks)/2)
pnls <- cumsum(na.omit(pnls))
colnames <- c("Strategy", "Index", "Average")
colnames(pnls) <- colnames
dygraphs::dygraph(pnls[endp], main="Optimal Rolling S&P500 Portfolio Strategy") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", col="red", strokeWidth=1) %>%
  dySeries(name=colnames[2], axis="y2", col="blue", strokeWidth=1) %>%
  dySeries(name=colnames[3], axis="y2", col="green", strokeWidth=2)
NA
App setup code that runs only once at startup.
ndata <- 1e4
stdev <- 1.0
Define the user interface
uiface <- shiny::fluidPage(
  # Create numeric input for the number of data points.
  numericInput('ndata', "Number of data points:", value=ndata),
  # Create slider input for the standard deviation parameter.
  sliderInput("stdev", label="Standard deviation:",
        min=0.1, max=3.0, value=stdev, step=0.1),
  # Render plot in a panel.
  plotOutput("plotobj", height=300, width=500)
)  # end user interface
Define the server function
servfun <- function(input, output) {
  output$plotobj <- shiny::renderPlot({
    # Simulate the data
    datav <- rnorm(input$ndata, sd=input$stdev)
    # Plot the data
    par(mar=c(2, 4, 4, 0), oma=c(0, 0, 0, 0))
    hist(datav, xlim=c(-4, 4), main="Histogram of Random Data")
  })  # end renderPlot
}  # end servfun
# Return a Shiny app object
shiny::shinyApp(ui=uiface, server=servfun)
Create elements of the user interface
uiface <- shiny::fluidPage(
  titlePanel("VWAP Moving Average"),
  # Create single row of widgets with two slider inputs
  fluidRow(
    # Input stock symbol
    column(width=3, selectInput("symbol", label="Symbol",
                          choices=symbols, selected=symbol)),
    # Input look-back interval
    column(width=3, sliderInput("look_back", label="Lookback interval",
                          min=1, max=150, value=11, step=1))
  ),  # end fluidRow
  # Create output plot panel
  mainPanel(dygraphs::dygraphOutput("dyplot"), width=12)
)  # end fluidPage interface
Define the server function
servfun <- shiny::shinyServer(function(input, output) {
  # Get the close and volume data in a reactive environment
  closep <- shiny::reactive({
    # Get the data
    ohlc <- get(input$symbol, data_env)
    closep <- log(quantmod::Cl(ohlc))
    volumes <- quantmod::Vo(ohlc)
    # Return the data
    cbind(closep, volumes)
  })  # end reactive code
  # Calculate the VWAP indicator in a reactive environment
  vwapv <- shiny::reactive({
    # Get model parameters from input argument
    look_back <- input$look_back
    # Calculate the VWAP indicator
    closep <- closep()[, 1]
    volumes <- closep()[, 2]
    vwapv <- HighFreq::roll_sum(se_ries=closep*volumes, look_back=look_back)
    volume_rolling <- HighFreq::roll_sum(se_ries=volumes, look_back=look_back)
    vwapv <- vwapv/volume_rolling
    vwapv[is.na(vwapv)] <- 0
    # Return the plot data
    datav <- cbind(closep, vwapv)
    colnames(datav) <- c(input$symbol, "VWAP")
    datav
  })  # end reactive code
  # Return the dygraph plot to output argument
  output$dyplot <- dygraphs::renderDygraph({
    colnames <- colnames(vwapv())
    dygraphs::dygraph(vwapv(), main=paste(colnames[1], "VWAP")) %>%
dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
dySeries(name=colnames[1], axis="y", label=colnames[1], strokeWidth=2, col="blue") %>%
dySeries(name=colnames[2], axis="y2", label=colnames[2], strokeWidth=2, col="red")
  })  # end output plot
})  # end server code
Return a Shiny app object
shiny::shinyApp(ui=uiface, server=servfun)
Define the server function
servfun <- shiny::shinyServer(function(input, output) {
  # Create an empty list of reactive values.
  value_s <- reactiveValues()
  # Get input parameters from the user interface.
  nrows <- reactive({
    # Add nrows to list of reactive values.
    value_s$nrows <- input$nrows
    input$nrows
  })  # end reactive code
  # Broadcast a message to the console when the button is pressed.
  observeEvent(eventExpr=input$but_ton, handlerExpr={
    cat("Input button pressed\n")
  })  # end observeEvent
  # Send the data when the button is pressed.
  datav <- eventReactive(eventExpr=input$but_ton, valueExpr={
    # eventReactive() executes on input$but_ton, but not on nrows() or input$nrows.
    cat("Sending", nrows(), "rows of data\n")
    datav <- head(mtcars, input$nrows)
    value_s$mpg <- mean(datav$mpg)
    datav
  })  # end eventReactive
  #   datav
  # Draw table of the data when the button is pressed.
  observeEvent(eventExpr=input$but_ton, handlerExpr={
    datav <- datav()
    cat("Received", value_s$nrows, "rows of data\n")
    cat("Average mpg = ", value_s$mpg, "\n")
    cat("Drawing table\n")
    output$tablev <- renderTable(datav)
  })  # end observeEvent
})  # end server code
Return a Shiny app object
shiny::shinyApp(ui=uiface, server=servfun)
options(width=200)
# Load package HighFreq
library(HighFreq)
# Or load the high frequency data file directly:
symbolv <- load("/Users/jerzy/Develop/R/HighFreq/data/hf_data.RData")
head(SPY_TAQ)
head(SPY)
tail(SPY)
library(rutils)
# Read TAQ trade data from csv file
taq <- data.table::fread(file="/Users/jerzy/Develop/data/xlk_tick_trades2020_0316.csv")
# Inspect the TAQ data
taq
class(taq)
colnames(taq)
sapply(taq, class)
symbol <- taq$SYM_ROOT[1]
# Create date-time index
dates <- paste(taq$DATE, taq$TIME_M)
# Coerce date-time index to POSIXlt
dates <- strptime(dates, "%Y%m%d %H:%M:%OS")
class(dates)
# Display more significant digits
# options("digits")
options(digits=20, digits.secs=10)
last(dates)
unclass(last(dates))
as.numeric(last(dates))
# Coerce date-time index to POSIXct
dates <- as.POSIXct(dates)
class(dates)
last(dates)
unclass(last(dates))
as.numeric(last(dates))
# Calculate the number of ticks per second
n_secs <- as.numeric(last(dates)) - as.numeric(first(dates))
NROW(taq)/(6.5*3600)
# Select TAQ data columns
taq <- taq[, .(price=PRICE, volume=SIZE)]
# Add date-time index
taq <- cbind(index=dates, taq)
# Coerce trade ticks to xts series
xtes <- xts::xts(taq[, .(price, volume)], taq$index)
colnames(xtes) <- paste(symbol, c("Close", "Volume"), sep=".")
save(xtes, file="/Users/jerzy/Develop/data/xlk_tick_trades2020_0316.RData")
# Plot dygraph
dygraphs::dygraph(xtes$XLK.Close,
  main="XLK Trade Ticks for 2020-03-16")
# Plot in x11 window
x11(width=6, height=5)
quantmod::chart_Series(x=xtes$XLK.Close,
  name="XLK Trade Ticks for 2020-03-16")
# Select the large lots greater than 100
dim(taq)
big_ticks <- taq[taq$volume > 100]
dim(big_ticks)
# Number of large lot ticks per second
NROW(big_ticks)/(6.5*3600)
# Save trade ticks with large lots
data.table::fwrite(big_ticks, file="/Users/jerzy/Develop/data/xlk_tick_trades2020_0316_biglots.csv")
# Coerce trade prices to xts
xtes <- xts::xts(big_ticks[, .(price, volume)], big_ticks$index)
colnames(xtes) <- c("XLK.Close", "XLK.Volume")
# Plot dygraph of the large lots
dygraphs::dygraph(xtes$XLK.Close,
  main="XLK Trade Ticks for 2020-03-16 (large lots only)")
# Plot the large lots
x11(width=6, height=5)
quantmod::chart_Series(x=xtes$XLK.Close,
  name="XLK Trade Ticks for 2020-03-16 (large lots only)")
# Apply centered Hampel filter to remove price jumps
win_dow <- 111
half_window <- win_dow %/% 2
medi_an <- TTR::runMedian(taq$price, n=win_dow)
medi_an <- rutils::lagit(medi_an, lagg=-half_window, pad_zeros=FALSE)
madv <- TTR::runMAD(taq$price, n=win_dow)
madv <- rutils::lagit(madv, lagg=-half_window, pad_zeros=FALSE)
madv[1:half_window] <- 1
madv[madv == 0] <- 1
# Calculate Z-scores
zscores <- (taq$price - medi_an)/madv
zscores[is.na(zscores)] <- 0
zscores[!is.finite(zscores)] <- 0
sum(is.na(zscores))
sum(!is.finite(zscores))
range(zscores)
mad(zscores)
hist(zscores, breaks=2000, xlim=c(-5*mad(zscores), 5*mad(zscores)))
# Remove price jumps with large z-scores
threshold <- 3
bad_ticks <- (abs(zscores) > threshold)
good_ticks <- taq[!bad_ticks]
# Calculate number of price jumps
sum(bad_ticks)/NROW(zscores)
# Coerce trade prices to xts
xtes <- xts::xts(good_ticks[, .(price, volume)], good_ticks$index)
colnames(xtes) <- c("XLK.Close", "XLK.Volume")
# Plot dygraph of the clean lots
dygraphs::dygraph(xtes$XLK.Close,
  main="XLK Trade Ticks for 2020-03-16 (Hampel filtered)")
# Plot the large lots
x11(width=6, height=5)
quantmod::chart_Series(x=xtes$XLK.Close,
  name="XLK Trade Ticks for 2020-03-16 (Hampel filtered)")
# Round time index to seconds
good_ticks[, zoo::index := as.POSIXct(round.POSIXt(index, "secs"))]
# Aggregate to OHLC by seconds
ohlc <- good_ticks[, .(open=first(price), high=max(price), low=min(price), close=last(price), volume=sum(volume)), by=index]
# Round time index to minutes
good_ticks[, zoo::index := as.POSIXct(round.POSIXt(index, "mins"))]
# Aggregate to OHLC by minutes
ohlc <- good_ticks[, .(open=first(price), high=max(price), low=min(price), close=last(price), volume=sum(volume)), by=index]
# Coerce OHLC prices to xts
xtes <- xts::xts(ohlc[, -"index"], ohlc$index)
# Plot dygraph of the OHLC prices
dygraphs::dygraph(xtes[, -5], main="XLK Trade Ticks for 2020-03-16 (OHLC)") %>%
  dyCandlestick()
# Plot the OHLC prices
x11(width=6, height=5)
quantmod::chart_Series(x=xtes, TA="add_Vo()",
  name="XLK Trade Ticks for 2020-03-16 (OHLC)")
# Load package HighFreq
library(HighFreq)
head(SPY)
# Load package HighFreq
library(HighFreq)
# Define symbol
symbol <- "SPY"
# Load OHLC data
output_dir <- "/Users/jerzy/Develop/data/hfreq/scrub/"
symbol <- load(
  file.path(output_dir,
      paste0(symbol, ".RData")))
interval <-
  "2013-11-11 09:30:00/2013-11-11 10:30:00"
chart_Series(SPY[interval], name=symbol)
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
ohlc <- HighFreq::SPY
nrows <- NROW(ohlc)
closep <- log(quantmod::Cl(ohlc))
returns <- rutils::diffit(closep)
colnames(returns) <- "SPY"
# Standardize raw returns to make later comparisons
returns <- (returns - mean(returns))/sd(returns)
# Calculate moments and perform normality test
sapply(c(var=2, skew=3, kurt=4),
  function(x) sum(returns^x)/nrows)
tseries::jarque.bera.test(returns)
# Fit SPY returns using MASS::fitdistr()
optim_fit <- MASS::fitdistr(returns, densfun="t", df=2)
loc <- optim_fit$estimate[1]
scalev <- optim_fit$estimate[2]
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(1, 1, 1, 1))
# Plot histogram of SPY returns
histp <- hist(returns, col="lightgrey", mgp=c(2, 1, 0),
  xlab="returns (standardized)", ylab="frequency", xlim=c(-3, 3),
  breaks=1e3, freq=FALSE, main="Distribution of High Frequency SPY Returns")
# lines(density(returns, bw=0.2), lwd=3, col="blue")
# Plot t-distribution function
curve(expr=dt((x-loc)/scalev, df=2)/scalev,
type="l", lwd=3, col="red", add=TRUE)
# Plot the Normal probability distribution
curve(expr=dnorm(x, mean=mean(returns),
  sd=sd(returns)), add=TRUE, lwd=3, col="blue")
# Add legend
legend("topright", inset=0.05, bty="n",
  leg=c("t-distr", "normal"),
  lwd=6, lty=1, col=c("red", "blue"))
# Hourly SPY percentage returns
closep <- log(Cl(xts::to.period(x=ohlc, period="hours")))
hourlly <- rutils::diffit(closep)
hourlly <- (hourlly - mean(hourlly))/sd(hourlly)
# Daily SPY percentage returns
closep <- log(Cl(xts::to.period(x=ohlc, period="days")))
dai_ly <- rutils::diffit(closep)
dai_ly <- (dai_ly - mean(dai_ly))/sd(dai_ly)
# Calculate moments
sapply(list(minutely=returns, hourly=hourlly, daily=dai_ly),
 function(rets) {
   sapply(c(var=2, skew=3, kurt=4),
          function(x) mean(rets^x))
})  # end sapply
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(1, 1, 1, 1))
# Plot densities of SPY returns
plot(density(returns, bw=0.4), xlim=c(-3, 3),
     lwd=3, mgp=c(2, 1, 0), col="blue",
     xlab="returns (standardized)", ylab="frequency",
     main="Density of High Frequency SPY Returns")
lines(density(hourlly, bw=0.4), lwd=3, col="green")
lines(density(dai_ly, bw=0.4), lwd=3, col="red")
# Add legend
legend("topright", inset=0.05, bty="n",
  leg=c("minutely", "hourly", "daily"),
  lwd=6, lty=1, col=c("blue", "green", "red"))
# Calculate rolling volatility of SPY returns
ret2013 <- returns["2013-11-11/2013-11-15"]
# Calculate rolling volatility
look_back <- 11
endp <- seq_along(ret2013)
startp <- c(rep_len(1, look_back-1),
  endp[1:(NROW(endp)-look_back+1)])
endp[endp < look_back] <- look_back
vol_rolling <- sapply(seq_along(endp),
  function(it) sd(ret2013[startp[it]:endp[it]]))
vol_rolling <- xts::xts(vol_rolling, zoo::index(ret2013))
# Extract time intervals of SPY returns
indeks <- c(60, diff(xts::.index(ret2013)))
head(indeks)
table(indeks)
# Scale SPY returns by time intervals
ret2013 <- 60*ret2013/indeks
# Calculate scaled rolling volatility
vol_scaled <- sapply(seq_along(endp),
  function(it) sd(ret2013[startp[it]:endp[it]]))
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
prices <- read.zoo(file="/Users/jerzy/Develop/lecture_slides/data/bid_ask_bounce.csv",
  header=TRUE, sep=",")
prices <- as.xts(prices)
x11(width=6, height=4)
par(mar=c(2, 2, 0, 0), oma=c(1, 1, 0, 0))
chart_Series(x=prices, name="S&P500 Futures Bid-Ask Bounce")
# Volatility of SPY
sqrt(HighFreq::calc_var_ohlc(ohlc))
# Daily SPY volatility and volume
vol_daily <- sqrt(xts::apply.daily(ohlc, FUN=calc_var_ohlc))
colnames(vol_daily) <- ("SPY_volatility")
volumes <- quantmod::Vo(ohlc)
volume_daily <- xts::apply.daily(volumes, FUN=sum)
colnames(volume_daily) <- ("SPY_volume")
# Plot SPY volatility and volume
datav <- cbind(vol_daily, volume_daily)["2008/2009"]
colnames <- colnames(datav)
dygraphs::dygraph(datav,
  main="SPY Daily Volatility and Trading Volume") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", col="red", strokeWidth=3) %>%
  dySeries(name=colnames[2], axis="y2", col="blue", strokeWidth=3)
# Regress log of daily volume vs volatility
datav <- log(cbind(volume_daily, vol_daily))
colnames <- colnames(datav)
data_frame <- as.data.frame(datav)
formulav <- as.formula(paste(colnames, collapse="~"))
model <- lm(formulav, data=data_frame)
# Durbin-Watson test for autocorrelation of residuals
lmtest::dwtest(model)
# Regress diff log of daily volume vs volatility
data_frame <- as.data.frame(rutils::diffit(datav))
model <- lm(formulav, data=data_frame)
lmtest::dwtest(model)
summary(model)
plot(formulav, data=data_frame, main="SPY Daily Trading Volume vs Volatility (log scale)")
abline(model, lwd=3, col="red")
mtext(paste("beta =", round(coef(model)[2], 3)), cex=1.2, lwd=3, side=2, las=2, adj=(-0.5), padj=(-7))
# 60 minutes of data in look_back interval
look_back <- 60
vol2013 <- volumes["2013"]
ret2013 <- returns["2013"]
# Define end points with beginning stub
nrows <- NROW(ret2013)
nagg <- nrows %/% look_back
endp <- nrows-look_back*nagg + (0:nagg)*look_back
startp <- c(1, endp[1:(NROW(endp)-1)])
# Calculate SPY volatility and volume
datav <- sapply(seq_along(endp), function(it) {
  point_s <- startp[it]:endp[it]
  c(volume=sum(vol2013[point_s]),
    volatility=sd(ret2013[point_s]))
})  # end sapply
datav <- t(datav)
datav <- rutils::diffit(log(datav))
data_frame <- as.data.frame(datav)
formulav <- as.formula(paste(colnames(datav), collapse="~"))
model <- lm(formulav, data=data_frame)
lmtest::dwtest(model)
summary(model)
plot(formulav, data=data_frame,
     main="SPY Hourly Trading Volume vs Volatility (log scale)")
abline(model, lwd=3, col="red")
mtext(paste("beta =", round(coef(model)[2], 3)), cex=1.2, lwd=3, side=2, las=2, adj=(-0.5), padj=(-7))
# Scale returns using volume (volume clock)
rets_scaled <- ifelse(volumes > 1e4, returns/volumes, 0)
rets_scaled <- rets_scaled/sd(rets_scaled)
# Calculate moments of scaled returns
nrows <- NROW(returns)
sapply(list(returns=returns, rets_scaled=rets_scaled),
  function(rets) {sapply(c(skew=3, kurt=4),
     function(x) sum((rets/sd(rets))^x)/nrows)
})  # end sapply
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(1, 1, 1, 1))
# Plot densities of SPY returns
plot(density(returns), xlim=c(-3, 3),
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
Box.test(returns, lag=10, type="Ljung")
# Ljung-Box test for daily SPY returns
Box.test(dai_ly, lag=10, type="Ljung")
# Ljung-Box test statistics for scaled SPY returns
sapply(list(returns=returns, rets_scaled=rets_scaled),
  function(rets) {
    Box.test(rets, lag=10, type="Ljung")$statistic
})  # end sapply
# Ljung-Box test statistics for aggregated SPY returns
sapply(list(minutely=returns, hourly=hourlly, daily=dai_ly),
  function(rets) {
    Box.test(rets, lag=10, type="Ljung")$statistic
})  # end sapply
# Set plot parameters
x11(width=6, height=8)
par(mar=c(4, 4, 2, 1), oma=c(0, 0, 0, 0))
layout(matrix(c(1, 2), ncol=1), widths=c(6, 6), heights=c(4, 4))
# Plot the partial autocorrelations of minutely SPY returns
pa_cf <- pacf(as.numeric(returns), lag=10,
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
dates <- format(zoo::index(returns), "%H:%M")
# Aggregate the mean volume
volume_agg <- tapply(X=volumes, INDEX=dates, FUN=mean)
volume_agg <- drop(volume_agg)
# Aggregate the mean volatility
vol_agg <- tapply(X=returns^2, INDEX=dates, FUN=mean)
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
futures <- rbind(c("S&P500 index", "ES"),
              c("10yr Treasury", "ZN"),
              c("VIX index", "VX"),
              c("Gold", "GC"),
              c("Oil", "CL"),
              c("Euro FX", "EC"),
              c("Swiss franc", "SF"),
              c("Japanese Yen", "JY"))
colnames(futures) <- c("Futures contract", "Code")
print(xtable::xtable(futures), comment=FALSE, size="scriptsize", include.rownames=FALSE, latex.environments="flushleft")
# Monthly futures contract codes
codes <- cbind(c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"),
                     c("F", "G", "H", "J", "K", "M", "N", "Q", "U", "V", "X", "Z"))
colnames(codes) <- c("Month", "Code")
print(xtable::xtable(codes), comment=FALSE, size="scriptsize", include.rownames=FALSE, latex.environments="flushright")
# Futures contracts codes
futures <- rbind(c("S&P500 index", "SP", "ES"),
              c("10yr Treasury", "ZN", "ZN"),
              c("VIX index", "VX", "delisted"),
              c("Gold", "GC", "YG"),
              c("Oil", "CL", "QM"),
              c("Euro FX", "EC", "E7"),
              c("Swiss franc", "SF", "MSF"),
              c("Japanese Yen", "JY", "J7"))
colnames(futures) <- c("Futures contract", "Standard", "E-mini")
print(xtable::xtable(futures), comment=FALSE, size="scriptsize", include.rownames=FALSE, latex.environments="flushleft")
# Load data for S&P Emini futures June 2019 contract
dir_name <- "/Users/jerzy/Develop/data/ib_data"
file_name <- file.path(dir_name, "ESohlc.csv")
# Read a data table from CSV file
prices <- data.table::fread(file_name)
class(prices)
# Coerce first column from string to date-time
unlist(sapply(prices, class))
tail(prices)
prices$Index <- as.POSIXct(prices$Index,
  tz="America/New_York", origin="1970-01-01")
# Coerce prices into xts series
prices <- data.table::as.xts.data.table(prices)
class(prices)
tail(prices)
colnames(prices)[1:5] <- c("Open", "High", "Low", "Close", "Volume")
tail(prices)
# Plot OHLC data in x11 window
x11(width=5, height=4)  # Open x11 for plotting
par(mar=c(5, 5, 2, 1), oma=c(0, 0, 0, 0))
chart_Series(x=prices, TA="add_Vo()",
  name="S&P500 futures")
# Plot dygraph
dygraphs::dygraph(prices[, 1:4], main="OHLC prices") %>%
  dyCandlestick()
# Load ESU8 data
dir_name <- "/Users/jerzy/Develop/data/ib_data"
file_name <- file.path(dir_name, "ESU8.csv")
ESU8 <- data.table::fread(file_name)
# Coerce ESU8 into xts series
ESU8$V1 <- as.Date(as.POSIXct.numeric(ESU8$V1,
    tz="America/New_York", origin="1970-01-01"))
ESU8 <- data.table::as.xts.data.table(ESU8)
colnames(ESU8)[1:5] <- c("Open", "High", "Low", "Close", "Volume")
# Load ESM8 data
file_name <- file.path(dir_name, "ESM8.csv")
ESM8 <- data.table::fread(file_name)
# Coerce ESM8 into xts series
ESM8$V1 <- as.Date(as.POSIXct.numeric(ESM8$V1,
    tz="America/New_York", origin="1970-01-01"))
ESM8 <- data.table::as.xts.data.table(ESM8)
colnames(ESM8)[1:5] <- c("Open", "High", "Low", "Close", "Volume")
x11(width=6, height=5)  # Open x11 for plotting
# Plot last month of ESU8 and ESM8 volume data
endd <- end(ESM8)
startd <- (endd - 30)
volumes <- cbind(Vo(ESU8),
  Vo(ESM8))[paste0(startd, "/", endd)]
colnames(volumes) <- c("ESU8", "ESM8")
colors <- c("blue", "green")
plot(volumes, col=colors, lwd=3, major.ticks="days",
     format.labels="%b-%d", observation.based=TRUE,
     main="Volumes of ESU8 and ESM8 futures")
legend("topleft", legend=colnames(volumes), col=colors,
 title=NULL, bty="n", lty=1, lwd=6, inset=0.1, cex=0.7)
# Find date when ESU8 volume exceeds ESM8
exceeds <- (volumes[, "ESU8"] > volumes[, "ESM8"])
indeks <- match(TRUE, exceeds)
# indeks <- min(which(exceeds))
# Scale the ESM8 prices
indeks <- index(exceeds[indeks])
factorv <- as.numeric(Cl(ESU8[indeks])/Cl(ESM8[indeks]))
ESM8[, 1:4] <- factorv*ESM8[, 1:4]
# Calculate continuous contract prices
chain_ed <- rbind(ESM8[index(ESM8) < indeks],
            ESU8[index(ESU8) >= indeks])
# Or
# Chain_ed <- rbind(ESM8[paste0("/", indeks-1)],
#                   ESU8[paste0(indeks, "/")])
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
load(file="/Users/jerzy/Develop/data/ib_data/vix_cboe.RData")
ls(vix_env)
vix_env$vix_index <- vix_index
ls(vix_env)
save(vix_env, file="/Users/jerzy/Develop/data/ib_data/vix_cboe.RData")
# Plot OHLC data in x11 window
chart_Series(x=vix_index["2018"], name="VIX Index")
# Plot dygraph
dygraphs::dygraph(vix_index, main="VIX Index") %>%
  dyCandlestick()
# Read CBOE monthly futures expiration dates
dates <- read.csv(
  file="/Users/jerzy/Develop/data/vix_data/vix_dates.csv")
dates <- as.Date(dates[, 1])
years <- format(dates, format="%Y")
years <- substring(years, 4)
# Monthly futures contract codes
codes <-
  c("F", "G", "H", "J", "K", "M",
    "N", "Q", "U", "V", "X", "Z")
symbols <- paste0("VX", codes, years)
dates <- as.data.frame(dates)
colnames(dates) <- "exp_dates"
rownames(dates) <- symbols
# Write dates to CSV file, with row names
write.csv(dates, row.names=TRUE,
  file="/Users/jerzy/Develop/data/vix_data/vix_futures.csv")
# Read back CBOE futures expiration dates
dates <- read.csv(file="/Users/jerzy/Develop/data/vix_data/vix_futures.csv",
  row.names=1)
dates[, 1] <- as.Date(dates[, 1])
# Load VIX futures data from binary file
load(file="/Users/jerzy/Develop/data/vix_data/vix_cboe.RData")
# Get all VIX futures for 2018 except January
symbols <- ls(vix_env)
symbols <- symbols[grep("*8", symbols)]
symbols <- symbols[2:9]
# Specify dates for curves
low_vol <- as.Date("2018-01-11")
hi_vol <- as.Date("2018-02-05")
# Extract all VIX futures prices on the dates
curve_s <- lapply(symbols, function(symbol) {
  xtes <- get(x=symbol, envir=vix_env)
  Cl(xtes[c(low_vol, hi_vol)])
})  # end lapply
curve_s <- rutils::do_call(cbind, curve_s)
colnames(curve_s) <- symbols
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
load(file="/Users/jerzy/Develop/data/vix_data/vix_cboe.RData")
# Read CBOE futures expiration dates
dates <- read.csv(file="/Users/jerzy/Develop/data/vix_data/vix_futures.csv",
  row.names=1)
symbols <- rownames(dates)
dates <- as.Date(dates[, 1])
todayd <- as.Date("2018-05-07")
maturi_ty <- (todayd + 30)
# Find neighboring futures contracts
indeks <- match(TRUE, dates > maturi_ty)
front_date <- dates[indeks-1]
back_date <- dates[indeks]
front_symbol <- symbols[indeks-1]
back_symbol <- symbols[indeks]
front_price <- get(x=front_symbol, envir=vix_env)
front_price <- as.numeric(Cl(front_price[todayd]))
back_price <- get(x=back_symbol, envir=vix_env)
back_price <- as.numeric(Cl(back_price[todayd]))
# Calculate the constant maturity 30-day futures price
ra_tio <- as.numeric(maturi_ty - front_date) /
  as.numeric(back_date - front_date)
pric_e <- (ra_tio*back_price + (1-ra_tio)*front_price)
x11(width=5, height=3)  # Open x11 for plotting
# Load VIX futures data from binary file
load(file="/Users/jerzy/Develop/data/vix_data/vix_cboe.RData")
# Plot VIX and SVXY data in x11 window
plot_theme <- chart_theme()
plot_theme$col$line.col <- "blue"
chart_Series(x=Cl(vix_env$vix_index["2007/"]),
       theme=plot_theme, name="VIX Index")
chart_Series(x=Cl(rutils::etfenv$VTI["2007/"]),
       theme=plot_theme, name="VTI ETF")
chart_Series(x=Cl(vix_env$vix_index["2017/2018"]),
       theme=plot_theme, name="VIX Index")
chart_Series(x=Cl(rutils::etfenv$SVXY["2017/2018"]),
       theme=plot_theme, name="SVXY ETF")
