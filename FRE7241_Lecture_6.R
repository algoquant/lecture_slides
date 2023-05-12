# Load daily S&P500 percentage stock returns
load(file="/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
# Overwrite NA values in returns
retp <- returns100
retp[1, ] <- 0
datev <- zoo::index(retp) # dates
nrows <- NROW(retp) # number of rows
nstocks <- NCOL(retp) # number of stocks
# Objective function equal to Kelly ratio
objfun <- function(retp) {
  retp <- na.omit(retp)
  varv <- var(retp)
  if (varv > 0) mean(retp)/varv else 0
}  # end objfun
# Calculate performance statistics for all stocks
perfstat <- sapply(retp, objfun)
sum(is.na(perfstat))
sum(!is.finite(perfstat))
# Calculate weights proportional to performance statistic
weightm <- perfstat
hist(weightm)
# Center the weights
weightm <- weightm - mean(weightm)
sum(weightm)
sort(weightm)
# Quadratic constraint
weightv <- weightm/sqrt(sum(weightm^2))
sum(weightv^2)
sum(weightv)
weightv
# Apply the linear constraint
weightv <- weightm/sum(weightm)
sum(weightv)
weightv
# Calculate the weighted returns using transpose
retw <- t(t(retp)*weightm)
# Or using Rcpp
retf <- HighFreq::mult_mat(weightm, retp)
all.equal(retw, retf, check.attributes=FALSE)
# Calculate the in-sample portfolio volatility
volis <- sd(rowMeans(retw, na.rm=TRUE))
# Calculate the equal weight portfolio volatility
volew <- sd(rowMeans(retp, na.rm=TRUE))
# Apply the volatility constraint
weightv <- volew*weightm/volis
# Calculate the in-sample portfolio volatility
retw <- t(t(retp)*weightv)
all.equal(sd(rowMeans(retw, na.rm=TRUE)), volew)
# Apply the volatility target constraint
volt <- 1e-2
weightv <- volt*weightm/volis
retw <- t(t(retp)*weightv)
all.equal(sd(rowMeans(retw, na.rm=TRUE)), volt)
# Compare speed of R with Rcpp
summary(microbenchmark(
  trans=t(t(retp)*weightm),
  rcpp=HighFreq::mult_mat(weightm, retp),
  times=10))[, c(1, 4, 5)]
# Box constraints
weightv[weightv > 1] <- 1
weightv[weightv < 0] <- 0
weightv
# Objective function equal to sum of returns
objfun <- function(retp) sum(na.omit(retp))
# Objective function equal to Sharpe ratio
objfun <- function(retp) {
  retp <- na.omit(retp)
  stdev <- sd(retp)
  if (stdev > 0) mean(retp)/stdev else 0
}  # end objfun
# Objective function equal to Kelly ratio
objfun <- function(retp) {
  retp <- na.omit(retp)
  varv <- var(retp)
  if (varv > 0) mean(retp)/varv else 0
}  # end objfun
# Calculate the performance statistics for all stocks
perfstat <- sapply(retp, objfun)
sum(is.na(perfstat))
# Calculate the best and worst performing stocks
perfstat <- sort(perfstat, decreasing=TRUE)
topstocks <- 10
symbolb <- names(head(perfstat, topstocks))
symbolw <- names(tail(perfstat, topstocks))
# Calculate equal weights for the best and worst performing stocks
weightv <- numeric(NCOL(retp))
names(weightv) <- colnames(retp)
weightv[symbolb] <- 1
weightv[symbolw] <- (-1)
# Center weights so sum is equal to 0
weightv <- weightv - mean(weightv)
# Scale weights so sum of squares is equal to 1
weightv <- weightv/sqrt(sum(weightv^2))
# Calculate the in-sample momentum strategy pnls
pnls <- t(t(retp)*weightv)
# Or using Rcpp
pnls2 <- HighFreq::mult_mat(weightv, retp)
all.equal(pnls, pnls2, check.attributes=FALSE)
pnls <- rowMeans(pnls, na.rm=TRUE)
# Scale weights so in-sample pnl volatility is same as equal weight
indeks <- rowMeans(retp, na.rm=TRUE)
scalev <- sd(indeks)/sd(pnls)
weightv <- scalev*weightv
# Calculate the momentum strategy pnls
pnls <- t(t(retp)*weightv)
pnls <- rowMeans(pnls, na.rm=TRUE)
all.equal(sd(indeks), sd(pnls))
# Objective function equal to Kelly ratio
objfun <- function(retp) {
  retp <- na.omit(retp)
  if (NROW(retp) > 2) {
    varv <- var(retp)
    if (varv > 0) mean(retp)/varv else 0
  }
  else 0
}  # end objfun
# Calculate a vector of monthly end points
endd <- rutils::calc_endpoints(retp, interval="months")
npts <- NROW(endd)
# Perform loop over the end points
look_back <- 8
pnls <- lapply(2:(npts-1), function(ep) {
  # Select the look-back returns
  startp <- endd[max(1, ep-look_back)]
  retis <- retp[startp:endd[ep], ]
  # Calculate the best and worst performing stocks in-sample
  perfstat <- sapply(retis, objfun)
  perfstat <- sort(perfstat, decreasing=TRUE)
  symbolb <- names(head(perfstat, topstocks))
  symbolw <- names(tail(perfstat, topstocks))
  # Calculate the momentum weights
  weightv <- numeric(NCOL(retp))
  names(weightv) <- colnames(retp)
  weightv[symbolb] <- 1
  weightv[symbolw] <- (-1)
  # Calculate the in-sample momentum pnls
  pnls <- HighFreq::mult_mat(weightv, retis)
  pnls <- rowMeans(pnls, na.rm=TRUE)
  # Scale weights so in-sample pnl volatility is same as equal weight
  weightv <- weightv*sd(rowMeans(retis, na.rm=TRUE))/sd(pnls)
  # Calculate the out-of-sample momentum returns
  pnls <- HighFreq::mult_mat(weightv, retp[(endd[ep]+1):endd[ep+1], ])
  pnls <- rowMeans(pnls, na.rm=TRUE)
  drop(pnls)
})  # end lapply
pnls <- rutils::do_call(c, pnls)
# Calculate the average of all stock returns
indeks <- rowMeans(retp, na.rm=TRUE)
indeks <- xts::xts(indeks, order.by=datev)
colnames(indeks) <- "Index"
# Add initial startup interval to the momentum returns
pnls <- c(rowMeans(retp[endd[1]:endd[2], ], na.rm=TRUE), pnls)
pnls <- xts::xts(pnls, order.by=datev)
colnames(pnls) <- "Strategy"
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(indeks, pnls)
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of stock index and momentum strategy
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Stock Index and Momentum Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
btmomtop <- function(retp, objfun, look_back=12, rebalf="months", topstocks=10,
               bid_offer=0.0, endd=rutils::calc_endpoints(retp, interval=rebalf), ...) {
  # Perform loop over end points
  npts <- NROW(endd)
  pnls <- lapply(2:(npts-1), function(ep) {
    # Select the look-back returns
    startp <- endd[max(1, ep-look_back)]
    retis <- retp[startp:endd[ep], ]
    # Calculate the best and worst performing stocks in-sample
    perfstat <- sapply(retis, objfun)
    perfstat <- sort(perfstat, decreasing=TRUE)
    symbolb <- names(head(perfstat, topstocks))
    symbolw <- names(tail(perfstat, topstocks))
    # Calculate the momentum weights
    weightv <- numeric(NCOL(retp))
    names(weightv) <- colnames(retp)
    weightv[symbolb] <- 1
    weightv[symbolw] <- (-1)
    # Calculate the in-sample momentum pnls
    pnls <- HighFreq::mult_mat(weightv, retis)
    pnls <- rowMeans(pnls, na.rm=TRUE)
    # Scale weights so in-sample pnl volatility is same as equal weight
    weightv <- weightv*sd(rowMeans(retis, na.rm=TRUE))/sd(pnls)
    # Calculate the out-of-sample momentum returns
    pnls <- HighFreq::mult_mat(weightv, retp[(endd[ep]+1):endd[ep+1], ])
    pnls <- rowMeans(pnls, na.rm=TRUE)
    drop(pnls)
  })  # end lapply
  pnls <- rutils::do_call(c, pnls)
  pnls
}  # end btmomtop
# Perform backtests for vector of look-back intervals
look_backs <- seq(3, 15, by=1)
endd <- rutils::calc_endpoints(retp, interval="months")
# Warning - takes very long
pnll <- lapply(look_backs, btmomtop, retp=retp, endd=endd, objfun=objfun)
# Perform parallel loop under Mac-OSX or Linux
library(parallel)  # Load package parallel
ncores <- detectCores() - 1
pnll <- mclapply(look_backs, btmomtop, retp=retp, endd=endd, objfun=objfun, mc.cores=ncores)
sharper <- sqrt(252)*sapply(pnll, function(pnl) mean(pnl)/sd(pnl))
# Plot Sharpe ratios of momentum strategies
plot(x=look_backs, y=sharper, t="l",
  main="Momentum Sharpe as Function of Look-back Interval",
  xlab="look-back (months)", ylab="Sharpe")
# Calculate best pnls of momentum strategy
whichmax <- which.max(sharper)
look_backs[whichmax]
pnls <- pnll[[whichmax]]
# Add stub period
pnls <- c(rowMeans(retp[endd[1]:endd[2], ], na.rm=TRUE), pnls)
pnls <- xts::xts(pnls, order.by=datev)
colnames(pnls) <- "Strategy"
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(indeks, pnls)
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of stock index and momentum strategy
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Optimal Momentum Strategy for Stocks") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
btmomweight <- function(retp, objfun, look_back=12, rebalf="months",
  bid_offer=0.0, endd=rutils::calc_endpoints(retp, interval=rebalf), ...) {
  # Perform loop over end points
  npts <- NROW(endd)
  pnls <- lapply(2:(npts-1), function(ep) {
    # Select the look-back returns
    startp <- endd[max(1, ep-look_back)]
    retis <- retp[startp:endd[ep], ]
    # Calculate weights proportional to performance
    perfstat <- sapply(retis, objfun)
    weightv <- perfstat
    # Calculate the in-sample portfolio returns
    pnls <- HighFreq::mult_mat(weightv, retis)
    pnls <- rowMeans(pnls, na.rm=TRUE)
    # Scale weights so in-sample pnl volatility is same as equal weight
    weightv <- weightv*sd(rowMeans(retis, na.rm=TRUE))/sd(pnls)
    # Calculate the out-of-sample momentum returns
    pnls <- HighFreq::mult_mat(weightv, retp[(endd[ep]+1):endd[ep+1], ])
    pnls <- rowMeans(pnls, na.rm=TRUE)
    drop(pnls)
  })  # end lapply
  rutils::do_call(c, pnls)
}  # end btmomweight
# Perform backtests for vector of look-back intervals
look_backs <- seq(3, 15, by=1)
pnll <- lapply(look_backs, btmomweight, retp=retp, endd=endd, objfun=objfun)
# Or perform parallel loop under Mac-OSX or Linux
library(parallel)  # Load package parallel
ncores <- detectCores() - 1
pnll <- mclapply(look_backs, btmomweight, retp=retp, endd=endd, objfun=objfun, mc.cores=ncores)
sharper <- sqrt(252)*sapply(pnll, function(pnl) mean(pnl)/sd(pnl))
# Plot Sharpe ratios of momentum strategies
plot(x=look_backs, y=sharper, t="l",
  main="Momentum Sharpe as Function of Look-back Interval",
  xlab="look-back (months)", ylab="Sharpe")
# Calculate best pnls of momentum strategy
whichmax <- which.max(sharper)
look_backs[whichmax]
pnls <- pnll[[whichmax]]
pnls <- c(rowMeans(retp[endd[1]:endd[2], ], na.rm=TRUE), pnls)
pnls <- xts::xts(pnls, order.by=datev)
colnames(pnls) <- "Strategy"
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(indeks, pnls, 0.5*(indeks+pnls))
colnames(wealthv)[3] <- "Combined"
cor(wealthv)
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of stock index and momentum strategy
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Optimal Weighted Momentum Strategy for Stocks") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", label="Combined", strokeWidth=3) %>%
  dyLegend(show="always", width=300)
# To simplify, set NAs to zero
retp[is.na(retp)] <- 0
# Calculate the trailing average returns and variance using C++ code
lambda <- 0.99
meanm <- HighFreq::run_mean(retp, lambda=lambda)
varm <- HighFreq::run_var(retp, lambda=lambda)
# Calculate the trailing Kelly ratio
weightv <- ifelse(varm > 0, meanm/varm, 0)
weightv[1, ] <- 1
weightv <- weightv/sqrt(rowSums(weightv^2))
weightv <- rutils::lagit(weightv)
# Calculate the momentum profits and losses
pnls <- rowSums(weightv*retp)
# Calculate the transaction costs
bid_offer <- 0.0
costs <- 0.5*bid_offer*rowSums(abs(rutils::diffit(weightv)))
pnls <- (pnls - costs)
# Scale the momentum volatility to the equal weight index
indeksd <- sd(indeks)
pnls <- indeksd*pnls/sd(pnls)
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(indeks, pnls, 0.5*(indeks+pnls))
colnames(wealthv)[2:3] <- c("Momentum", "Combined")
cor(wealthv)
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of stock index and momentum strategy
endd <- rutils::calc_endpoints(retp, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Daily Momentum Strategy for Stocks") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", label="Combined", strokeWidth=3) %>%
  dyLegend(show="always", width=300)
# Define backtest functional for daily momentum strategy
# If trend=(-1) then it backtests a mean reverting strategy
btmomdaily <- function(retp, lambda=0.9, trend=1, bid_offer=0.0, ...) {
  stopifnot("package:quantmod" %in% search() || require("quantmod", quietly=TRUE))
  # Calculate the trailing Kelly ratio
  meanm <- HighFreq::run_mean(retp, lambda=lambda)
  varm <- HighFreq::run_var(retp, lambda=lambda)
  weightv <- ifelse(varm > 0, meanm/varm, 0)
  weightv[1, ] <- 1
  weightv <- weightv/sqrt(rowSums(weightv^2))
  weightv <- rutils::lagit(weightv)
  # Calculate the momentum profits and losses
  pnls <- trend*rowSums(weightv*retp)
  # Calculate the transaction costs
  costs <- 0.5*bid_offer*rowSums(abs(rutils::diffit(weightv)))
  (pnls - costs)
}  # end btmomdaily
# Simulate multiple daily stock momentum strategies
lambdas <- seq(0.99, 0.999, 0.002)
pnls <- sapply(lambdas, btmomdaily, retp=retp)
# Scale the momentum volatility to the equal weight index
pnls <- apply(pnls, MARGIN=2, function(pnl) indeksd*pnl/sd(pnl))
colnames(pnls) <- paste0("lambda=", lambdas)
pnls <- xts::xts(pnls, datev)
tail(pnls)
# Plot dygraph of daily stock momentum strategies
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls)[endd],
  main="Daily Stock Momentum Strategies") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Plot daily stock momentum strategies using quantmod
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(pnls))
quantmod::chart_Series(cumsum(pnls)[endd],
  theme=plot_theme, name="Daily Stock Momentum Strategies")
legend("bottomleft", legend=colnames(pnls),
  inset=0.02, bg="white", cex=0.7, lwd=rep(6, NCOL(retp)),
  col=plot_theme$col$line.col, bty="n")
# Define backtest functional for daily momentum strategy
# If trend=(-1) then it backtests a mean reverting strategy
btmomdailyhold <- function(retp, lambda=0.9, trend=1, bid_offer=0.0, ...) {
  stopifnot("package:quantmod" %in% search() || require("quantmod", quietly=TRUE))
  # Calculate the trailing Kelly ratio
  meanm <- HighFreq::run_mean(retp, lambda=lambda)
  varm <- HighFreq::run_var(retp, lambda=lambda)
  weightv <- ifelse(varm > 0, meanm/varm, 0)
  weightv[1, ] <- 1
  weightv <- weightv/sqrt(rowSums(weightv^2))
  # Average the past weights
  weightv <- HighFreq::run_mean(weightv, lambda=lambda)
  weightv <- rutils::lagit(weightv)
  # Calculate the momentum profits and losses
  pnls <- trend*rowSums(weightv*retp)
  # Calculate the transaction costs
  costs <- 0.5*bid_offer*rowSums(abs(rutils::diffit(weightv)))
  (pnls - costs)
}  # end btmomdailyhold
# Simulate multiple daily stock momentum strategies with holding periods
lambdas <- seq(0.99, 0.999, 0.002)
pnls <- sapply(lambdas, btmomdailyhold, retp=retp)
# Scale the momentum volatility to the equal weight index
pnls <- apply(pnls, MARGIN=2, function(pnl) indeksd*pnl/sd(pnl))
colnames(pnls) <- paste0("lambda=", lambdas)
pnls <- xts::xts(pnls, datev)
# dygraph of daily stock momentum strategies with holding period
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls)[endd],
  main="Daily Stock Momentum Strategies with Holding Period") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Plot of daily stock momentum strategies with holding period
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(pnls))
quantmod::chart_Series(cumsum(pnls)[endd],
  theme=plot_theme, name="Daily Stock Momentum Strategies with Holding Period")
legend("bottomleft", legend=colnames(pnls),
  inset=0.02, bg="white", cex=0.7, lwd=rep(6, NCOL(retp)),
  col=plot_theme$col$line.col, bty="n")
# Calculate best pnls of momentum strategy
sharper <- sqrt(252)*sapply(pnls, function(pnl) mean(pnl)/sd(pnl))
whichmax <- which.max(sharper)
lambdas[whichmax]
pnls <- pnls[, whichmax]
colnames(pnls) <- "Strategy"
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(indeks, pnls, 0.5*(indeks+pnls))
colnames(wealthv)[2:3] <- c("Momentum", "Combined")
cor(wealthv)
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of stock index and momentum strategy
colorv <- c("blue", "red", "green")
endd <- rutils::calc_endpoints(retp, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Optimal Daily Momentum Strategy for Stocks") %>%
  dyOptions(colors=colorv, strokeWidth=1) %>%
  dySeries(name="Combined", label="Combined", strokeWidth=3) %>%
  dyLegend(show="always", width=300)
# Perform sapply loop over look_backs
lambdas <- seq(0.2, 0.7, 0.1)
pnls <- sapply(lambdas, btmomdaily, retp=retp, trend=(-1))
# Scale the momentum volatility to the equal weight index
pnls <- apply(pnls, MARGIN=2, function(pnl) indeksd*pnl/sd(pnl))
colnames(pnls) <- paste0("lambda=", lambdas)
pnls <- xts::xts(pnls, datev)
# Plot dygraph of mean reverting daily stock momentum strategies
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls)[endd],
  main="Mean Reverting Daily Stock Momentum Strategies") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=400)
# Plot mean reverting daily stock momentum strategies using quantmod
plot_theme <- chart_theme()
plot_theme$col$line.col <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
quantmod::chart_Series(cumsum(pnls)[endd],
  theme=plot_theme, name="Mean Reverting Daily Stock Momentum Strategies")
legend("topleft", legend=colnames(pnls),
  inset=0.05, bg="white", cex=0.7, lwd=rep(6, NCOL(retp)),
  col=plot_theme$col$line.col, bty="n")
# Calculate the scaled prices of VTI vs MTUM ETF
wealthv <- na.omit(rutils::etfenv$returns[, c("VTI", "MTUM")])
colnames(wealthv) <- c("VTI", "MTUM")
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of scaled prices of VTI vs MTUM ETF
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="VTI vs MTUM ETF") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(width=300)
# Calculate the PCA weights for standardized returns
retsc <- lapply(retp, function(x) (x - mean(x))/sd(x))
retsc <- do.call(cbind, retsc)
covmat <- cov(retsc)
pcad <- eigen(covmat)
pcaw <- pcad$vectors
rownames(pcaw) <- colnames(retp)
sort(-pcaw[, 1], decreasing=TRUE)
sort(pcaw[, 2], decreasing=TRUE)
round((t(pcaw) %*% pcaw)[1:5, 1:5], 4)
# Calculate the PCA time series from stock returns using PCA weights
retpca <- retsc %*% pcaw
round((t(retpca) %*% retpca)[1:5, 1:5], 4)
# Calculate the PCA using prcomp()
pcad <- prcomp(retsc, center=FALSE, scale=FALSE)
all.equal(abs(pcad$x), abs(retpca), check.attributes=FALSE)
retpca <- xts::xts(retpca, order.by=datev)
# Simulate daily PCA momentum strategies for multiple lambda parameters
dimax <- 21
lambdas <- seq(0.98, 0.99, 0.003)
pnls <- mclapply(lambdas, btmomdailyhold, retp=retpca[, 1:dimax], mc.cores=ncores)
pnls <- lapply(pnls, function(pnl) indeksd*pnl/sd(pnl))
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("lambda=", lambdas)
pnls <- xts::xts(pnls, datev)
# Plot Sharpe ratios of momentum strategies
sharper <- sqrt(252)*sapply(pnls, function(pnl) mean(pnl)/sd(pnl))
plot(x=lambdas, y=sharper, t="l",
  main="PCA Momentum Sharpe as Function of Decay Parameter",
  xlab="lambda", ylab="Sharpe")
# Plot dygraph of daily PCA momentum strategies
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
endd <- rutils::calc_endpoints(retpca, interval="weeks")
dygraphs::dygraph(cumsum(pnls)[endd],
  main="Daily PCA Momentum Strategies") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=400)
# Calculate best pnls of PCA momentum strategy
whichmax <- which.max(sharper)
lambdas[whichmax]
pnls <- pnls[, whichmax]
colnames(pnls) <- "Strategy"
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(indeks, pnls, 0.5*(indeks+pnls))
colnames(wealthv)[2:3] <- c("Momentum", "Combined")
cor(wealthv)
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of stock index and PCA momentum strategy
colorv <- c("blue", "red", "green")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Optimal Daily Momentum Strategy for Stocks") %>%
  dyOptions(colors=colorv, strokeWidth=1) %>%
  dySeries(name="Combined", label="Combined", strokeWidth=3) %>%
  dyLegend(show="always", width=300)
# Simulate daily PCA momentum strategies for multiple lambda parameters
lambdas <- seq(0.6, 0.9, 0.1)
pnls <- mclapply(lambdas, btmomdailyhold, retp=retpca[, (dimax+1):NCOL(retpca)],
   trend=(-1), mc.cores=ncores)
pnls <- lapply(pnls, function(pnl) indeksd*pnl/sd(pnl))
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("lambda=", lambdas)
pnls <- xts::xts(pnls, datev)
# Plot Sharpe ratios of momentum strategies
sharper <- sqrt(252)*sapply(pnls, function(pnl) mean(pnl)/sd(pnl))
plot(x=lambdas, y=sharper, t="l",
  main="PCA Momentum Sharpe as Function of Decay Parameter",
  xlab="lambda", ylab="Sharpe")
# Plot dygraph of daily PCA momentum strategies
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls)[endd],
  main="Mean Reverting Daily PCA Momentum Strategies") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=400)
# Define in-sample and out-of-sample intervals
cutoff <- nrows %/% 2
datev[cutoff]
insample <- 1:cutoff
outsample <- (cutoff + 1):nrows
# Calculate the PCA weights in-sample
pcad <- prcomp(retp[insample], center=FALSE, scale=TRUE)
# Calculate the out-of-sample PCA time series
retsc <- lapply(retp, function(x) x[outsample]/sd(x[insample]))
retsc <- do.call(cbind, retsc)
retpca <- xts::xts(retsc %*% pcad$rotation, order.by=datev[outsample])
# Simulate daily PCA momentum strategies for multiple lambda parameters
lambdas <- seq(0.99, 0.999, 0.003)
pnls <- mclapply(lambdas, btmomdailyhold, retp=retpca[, 1:dimax], mc.cores=ncores)
pnls <- lapply(pnls, function(pnl) indeksd*pnl/sd(pnl))
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("lambda=", lambdas)
pnls <- xts::xts(pnls, datev[outsample])
# Plot Sharpe ratios of momentum strategies
sharper <- sqrt(252)*sapply(pnls, function(pnl) mean(pnl)/sd(pnl))
plot(x=lambdas, y=sharper, t="l",
  main="PCA Momentum Sharpe as Function of Decay Parameter",
  xlab="lambda", ylab="Sharpe")
# Calculate a vector of weekly end points
endd <- rutils::calc_endpoints(retpca, interval="weeks")
# Plot dygraph of daily out-of-sample PCA momentum strategies
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls)[endd],
  main="Daily Out-of-Sample PCA Momentum Strategies") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Simulate daily PCA momentum strategies for multiple lambda parameters
lambdas <- seq(0.5, 0.9, 0.1)
pnls <- mclapply(lambdas, btmomdailyhold, retp=retpca[, (dimax+1):NCOL(retpca)],
   trend=(-1), mc.cores=ncores)
pnls <- lapply(pnls, function(pnl) indeksd*pnl/sd(pnl))
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("lambda=", lambdas)
pnls <- xts::xts(pnls, datev[outsample])
# Plot Sharpe ratios of momentum strategies
sharper <- sqrt(252)*sapply(pnls, function(pnl) mean(pnl)/sd(pnl))
plot(x=lambdas, y=sharper, t="l",
  main="PCA Momentum Sharpe as Function of Decay Parameter",
  xlab="lambda", ylab="Sharpe")
# Calculate a vector of weekly end points
endd <- rutils::calc_endpoints(retpca, interval="weeks")
# Plot dygraph of daily S&P500 momentum strategies
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls)[endd],
  main="Mean Reverting Daily Out-of-Sample PCA Momentum Strategies") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Extract ETF returns
symbolv <- c("VTI", "IEF", "DBC")
retp <- na.omit(rutils::etfenv$returns[, symbolv])
datev <- zoo::index(retp)
# Calculate a vector of monthly end points
endd <- rutils::calc_endpoints(retp, interval="months")
npts <- NROW(endd)
# Perform backtests for vector of look-back intervals
look_backs <- seq(3, 12, by=1)
pnll <- lapply(look_backs, btmomweight, retp=retp, endd=endd, objfun=objfun)
sharper <- sqrt(252)*sapply(pnll, function(pnl) mean(pnl)/sd(pnl))
# Plot Sharpe ratios of momentum strategies
plot(x=look_backs, y=sharper, t="l",
  main="Momentum Sharpe as Function of Look-back Interval",
  xlab="look-back (months)", ylab="Sharpe")
# Calculate best pnls of momentum strategy
whichmax <- which.max(sharper)
look_backs[whichmax]
pnls <- pnll[[whichmax]]
pnls <- c(rowMeans(retp[endd[1]:endd[2], ]), pnls)
# Define all-weather benchmark
weightvaw <- c(0.30, 0.55, 0.15)
all_weather <- retp %*% weightvaw
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(all_weather, pnls, 0.5*(all_weather+pnls))
colnames(wealthv) <- c("All-weather", "Strategy", "Combined")
cor(wealthv)
wealthv <- xts::xts(wealthv, order.by=datev)
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of stock index and momentum strategy
colorv <- c("blue", "red", "green")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Optimal Momentum Strategy and All-weather for ETFs") %>%
  dyOptions(colors=colorv, strokeWidth=1) %>%
  dySeries(name="Combined", label="Combined", strokeWidth=3) %>%
  dyLegend(show="always", width=300)
# Calculate the momentum weights
look_back <- look_backs[whichmax]
weightv <- lapply(2:npts, function(ep) {
  # Select the look-back returns
  startp <- endd[max(1, ep-look_back)]
  retis <- retp[startp:endd[ep], ]
  # Calculate weights proportional to performance
  perfstat <- sapply(retis, objfun)
  weightv <- drop(perfstat)
  # Scale weights so in-sample pnl volatility is same as equal weight
  pnls <- retis %*% weightv
  weightv*sd(rowMeans(retis))/sd(pnls)
})  # end lapply
weightv <- rutils::do_call(rbind, weightv)
# Plot of momentum weights
retvti <- cumsum(retp$VTI)
datav <- cbind(retvti[endd], weightv)
colnames(datav) <- c("VTI", paste0(colnames(retp), "_weight"))
zoo::plot.zoo(datav, xlab=NULL, main="Momentum Weights")
# Calculate ETF betas
betasetf <- sapply(retp, function(x)
  cov(retp$VTI, x)/var(retp$VTI))
# Momentum beta is equal weights times ETF betas
betav <- weightv %*% betasetf
betav <- xts::xts(betav, order.by=datev[endd])
colnames(betav) <- "momentum_beta"
datav <- cbind(retvti[endd], betav)
zoo::plot.zoo(datav, main="Momentum Beta & VTI Price", xlab="")
# Merton-Henriksson test
retvti <- retp$VTI
predm <- cbind(VTI=retvti, 0.5*(retvti+abs(retvti)), retvti^2)
colnames(predm)[2:3] <- c("merton", "treynor")
regmod <- lm(pnls ~ VTI + merton, data=predm); summary(regmod)
# Treynor-Mazuy test
regmod <- lm(pnls ~ VTI + treynor, data=predm); summary(regmod)
# Plot residual scatterplot
resids <- regmod$residuals
plot.default(x=retvti, y=resids, xlab="VTI", ylab="momentum")
title(main="Treynor-Mazuy Market Timing Test\n for Momentum vs VTI", line=0.5)
# Plot fitted (predicted) response values
coefreg <- summary(regmod)$coeff
fitv <- regmod$fitted.values - coefreg["VTI", "Estimate"]*retvti
tvalue <- round(coefreg["treynor", "t value"], 2)
points.default(x=retvti, y=fitv, pch=16, col="red")
text(x=0.0, y=max(resids), paste("Treynor test t-value =", tvalue))
# Standardize the returns
pnlsd <- (pnls-mean(pnls))/sd(pnls)
retvti <- (retvti-mean(retvti))/sd(retvti)
# Calculate skewness and kurtosis
apply(cbind(pnlsd, retvti), 2, function(x)
  sapply(c(skew=3, kurt=4),
    function(e) sum(x^e)))/NROW(retvti)
# Calculate kernel density of VTI
densvti <- density(retvti)
# Plot histogram of momentum returns
hist(pnlsd, breaks=80,
  main="Momentum and VTI Return Distributions (standardized)",
  xlim=c(-4, 4), ylim=range(densvti$y), xlab="", ylab="", freq=FALSE)
# Draw kernel density of histogram
lines(density(pnlsd), col='red', lwd=2)
lines(densvti, col='blue', lwd=2)
# Add legend
legend("topright", inset=0.0, cex=1.0, title=NULL,
 leg=c("Momentum", "VTI"), bty="n", y.intersp=0.5,
 lwd=6, bg="white", col=c("red", "blue"))
# Combine momentum strategy with all-weather
wealthv <- cbind(pnls, all_weather, 0.5*(pnls + all_weather))
colnames(wealthv) <- c("Momentum", "All_weather", "Combined")
wealthv <- xts::xts(wealthv, datev)
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Calculate strategy correlations
cor(wealthv)
# Plot ETF momentum strategy combined with All-Weather
colorv <- c("blue", "red", "green")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Optimal Momentum Strategy and All-weather for ETFs") %>%
  dyOptions(colors=colorv, strokeWidth=1) %>%
  dySeries(name="Combined", label="Combined", strokeWidth=3) %>%
  dyLegend(show="always", width=300)
# Calculate the trailing variance
look_back <- 152
varm <- HighFreq::roll_var(retp, look_back=look_back)
# Calculate the trailing Kelly ratio
meanv <- HighFreq::roll_mean(retp, look_back=look_back)
weightv <- ifelse(varm > 0, meanv/varm, 0)
sum(is.na(weightv))
weightv <- weightv/sqrt(rowSums(weightv^2))
weightv <- rutils::lagit(weightv)
# Calculate the momentum profits and losses
pnls <- rowSums(weightv*retp)
# Calculate the transaction costs
bid_offer <- 0.0
costs <- 0.5*bid_offer*rowSums(abs(rutils::diffit(weightv)))
pnls <- (pnls - costs)
# Scale the momentum volatility to all_weather
pnls <- sd(all_weather)*pnls/sd(pnls)
# Calculate the wealth of momentum returns
wealthv <- cbind(pnls, all_weather, 0.5*(pnls + all_weather))
colnames(wealthv) <- c("Momentum", "All_weather", "Combined")
wealthv <- xts::xts(wealthv, datev)
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
cor(wealthv)
# Plot dygraph of the momentum strategy returns
colorv <- c("blue", "red", "green")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Daily Momentum Strategy for ETFs vs All-Weather") %>%
  dyOptions(colors=colorv, strokeWidth=1) %>%
  dySeries(name="Combined", label="Combined", strokeWidth=3) %>%
  dyLegend(show="always", width=300)
# Select all the ETF symbols except "VXX", "SVXY" "MTUM", "QUAL", "VLUE", and "USMV"
symbolv <- colnames(rutils::etfenv$returns)
symbolv <- symbolv[!(symbolv %in% c("VXX", "SVXY", "MTUM", "QUAL", "VLUE", "USMV"))]
# Extract columns of rutils::etfenv$returns and overwrite NA values
retp <- rutils::etfenv$returns[, symbolv]
nstocks <- NCOL(retp)
# retp <- na.omit(retp)
retp[1, is.na(retp[1, ])] <- 0
retp <- zoo::na.locf(retp, na.rm=FALSE)
datev <- zoo::index(retp)
# Returns in excess of risk-free rate
riskf <- 0.03/252
retx <- (retp - riskf)
# Maximum Sharpe weights in-sample interval
retis <- retp["/2014"]
invmat <- MASS::ginv(cov(retis))
weightv <- invmat %*% colMeans(retx["/2014"])
weightv <- drop(weightv/sqrt(sum(weightv^2)))
names(weightv) <- colnames(retp)
# Plot portfolio weights
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
barplot(sort(weightv), main="Maximum Sharpe Weights", cex.names=0.7)
# Calculate in-sample portfolio returns
insample <- xts::xts(retis %*% weightv, zoo::index(retis))
indeks <- xts::xts(rowMeans(retis), zoo::index(retis))
insample <- insample*sd(indeks)/sd(insample)
# Plot cumulative portfolio returns
pnls <- cbind(indeks, insample)
colnames(pnls) <- c("Equal Weight", "Optimal")
endd <- rutils::calc_endpoints(pnls, interval="weeks")
dygraphs::dygraph(cumsum(pnls)[endd],
  main="In-sample Optimal Portfolio Returns") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(width=300)
# Calculate out-of-sample portfolio returns
retsos <- retp["2015/"]
outsample <- xts::xts(retsos %*% weightv, zoo::index(retsos))
indeks <- xts::xts(rowMeans(retsos), zoo::index(retsos))
outsample <- outsample*sd(indeks)/sd(outsample)
pnls <- cbind(indeks, outsample, (outsample + indeks)/2)
colnames(pnls) <- c("Equal Weight", "Optimal", "Combined")
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(pnls, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot cumulative portfolio returns
endd <- rutils::calc_endpoints(pnls, interval="weeks")
dygraphs::dygraph(cumsum(pnls)[endd],
  main="Out-of-sample Optimal Portfolio Returns") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=2) %>%
  dyLegend(width=300)
# Maximum Sharpe weights in-sample interval
invmat <- MASS::ginv(cov(retis))
weightv <- invmat %*% colMeans(retx["/2014"])
weightv <- drop(weightv/sqrt(sum(weightv^2)))
names(weightv) <- colnames(retp)
# Calculate in-sample portfolio returns
insample <- xts::xts(retis %*% weightv, zoo::index(retis))
# Calculate out-of-sample portfolio returns
retsos <- retp["2015/"]
outsample <- xts::xts(retsos %*% weightv, zoo::index(retsos))
# Plot cumulative portfolio returns
pnls <- rbind(insample, outsample)
indeks <- xts::xts(rowMeans(retp), datev)
pnls <- pnls*sd(indeks)/sd(pnls)
pnls <- cbind(indeks, pnls)
colnames(pnls) <- c("Equal Weight", "Optimal")
endd <- rutils::calc_endpoints(pnls, interval="weeks")
dygraphs::dygraph(cumsum(pnls)[endd],
  main="Out-of-sample Optimal Portfolio Returns for ETFs") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyEvent(zoo::index(last(retis[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=300)
# Create rectangular matrix with collinear columns
matrixv <- matrix(rnorm(10*8), nc=10)
# Calculate covariance matrix
covmat <- cov(matrixv)
# Calculate inverse of covmat - error
invmat <- solve(covmat)
# Perform eigen decomposition
eigend <- eigen(covmat)
eigenvec <- eigend$vectors
eigenval <- eigend$values
# Set tolerance for determining zero singular values
precv <- sqrt(.Machine$double.eps)
# Calculate regularized inverse matrix
notzero <- (eigenval > (precv*eigenval[1]))
invreg <- eigenvec[, notzero] %*%
  (t(eigenvec[, notzero]) / eigenval[notzero])
# Verify inverse property of invreg
all.equal(covmat, covmat %*% invreg %*% covmat)
# Calculate regularized inverse of covmat
invmat <- MASS::ginv(covmat)
# Verify that invmat is same as invreg
all.equal(invmat, invreg)
# Calculate in-sample covariance matrix
covmat <- cov(retis)
eigend <- eigen(covmat)
eigenvec <- eigend$vectors
eigenval <- eigend$values
# Calculate dimension reduction inverse of covariance matrix
dimax <- 3
covinv <- eigenvec[, 1:dimax] %*%
  (t(eigenvec[, 1:dimax]) / eigenval[1:dimax])
# Verify inverse property of inverse
all.equal(covmat, covmat %*% covinv %*% covmat)
# Calculate portfolio weights
weightv <- invmat %*% colMeans(retis)
weightv <- drop(weightv/sqrt(sum(weightv^2)))
names(weightv) <- colnames(retp)
# Calculate portfolio returns
insample <- xts::xts(retis %*% weightv, zoo::index(retis))
outsample <- xts::xts(retsos %*% weightv, zoo::index(retsos))
# Plot cumulative portfolio returns
pnls <- rbind(insample, outsample)
pnls <- pnls*sd(indeks)/sd(pnls)
pnls <- cbind(indeks, pnls)
colnames(pnls) <- c("Equal Weight", "Optimal")
dygraphs::dygraph(cumsum(pnls)[endd],
  main="Optimal Portfolio Returns With Dimension Reduction") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyEvent(zoo::index(last(retis[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=300)
# Shrink the in-sample returns to their mean
alpha <- 0.7
retxm <- rowMeans(retx["/2014"])
retxis <- (1-alpha)*retx["/2014"] + alpha*retxm
# Calculate portfolio weights
weightv <- invmat %*% colMeans(retxis)
weightv <- drop(weightv/sqrt(sum(weightv^2)))
# Calculate portfolio returns
insample <- xts::xts(retis %*% weightv, zoo::index(retis))
outsample <- xts::xts(retsos %*% weightv, zoo::index(retsos))
# Plot cumulative portfolio returns
pnls <- rbind(insample, outsample)
pnls <- pnls*sd(indeks)/sd(pnls)
pnls <- cbind(indeks, pnls)
colnames(pnls) <- c("Equal Weight", "Optimal")
dygraphs::dygraph(cumsum(pnls)[endd],
  main="Optimal Portfolio Returns With Eigen and Return Shrinkage") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyEvent(zoo::index(last(retis[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=300)
# Define monthly end points
endd <- rutils::calc_endpoints(retp, interval="weeks")
endd <- endd[endd > (nstocks+1)]
npts <- NROW(endd)
look_back <- 3
startp <- c(rep_len(0, look_back), endd[1:(npts-look_back)])
# Perform loop over end points
pnls <- lapply(2:npts, function(ep) {
    # Calculate the portfolio weights
    insample <- retx[startp[ep-1]:endd[ep-1], ]
    invmat <- MASS::ginv(cov(insample))
    weightv <- invmat %*% colMeans(insample)
    weightv <- drop(weightv/sqrt(sum(weightv^2)))
    # Calculate the out-of-sample portfolio returns
    outsample <- retp[(endd[ep-1]+1):endd[ep], ]
    xts::xts(outsample %*% weightv, zoo::index(outsample))
})  # end lapply
pnls <- do.call(rbind, pnls)
# Plot dygraph of rolling ETF portfolio strategy
pnls <- pnls*sd(indeks)/sd(pnls)
pnls <- rbind(indeks[paste0("/", start(pnls)-1)], pnls)
wealthv <- cbind(indeks, pnls, (pnls+indeks)/2)
colnames(wealthv) <- c("Index", "Strategy", "Combined")
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
dygraphs::dygraph(cumsum(wealthv)[endd], main="Monthly ETF Rolling Portfolio Strategy") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Define monthly end points
look_back <- 3; dimax <- 9
startp <- c(rep_len(0, look_back), endd[1:(npts-look_back)])
# Perform loop over end points
pnls <- lapply(2:npts, function(ep) {
    # Calculate regularized inverse of covariance matrix
    insample <- retx[startp[ep-1]:endd[ep-1], ]
    eigend <- eigen(cov(insample))
    eigenvec <- eigend$vectors
    eigenval <- eigend$values
    invmat <- eigenvec[, 1:dimax] %*%
(t(eigenvec[, 1:dimax]) / eigenval[1:dimax])
    # Calculate the maximum Sharpe ratio portfolio weights
    weightv <- invmat %*% colMeans(insample)
    weightv <- drop(weightv/sqrt(sum(weightv^2)))
    # Calculate the out-of-sample portfolio returns
    outsample <- retp[(endd[ep-1]+1):endd[ep], ]
    xts::xts(outsample %*% weightv, zoo::index(outsample))
})  # end lapply
pnls <- do.call(rbind, pnls)
# Plot dygraph of rolling ETF portfolio strategy
pnls <- pnls*sd(indeks)/sd(pnls)
pnls <- rbind(indeks[paste0("/", start(pnls)-1)], pnls)
wealthv <- cbind(indeks, pnls, (pnls+indeks)/2)
colnames(wealthv) <- c("Index", "Strategy", "Combined")
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
dygraphs::dygraph(cumsum(wealthv)[endd], main="Rolling Portfolio Strategy With Dimension Reduction") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Define the return shrinkage intensity
alpha <- 0.7
# Perform loop over end points
pnls <- lapply(2:npts, function(ep) {
    # Calculate regularized inverse of covariance matrix
    insample <- retx[startp[ep-1]:endd[ep-1], ]
    eigend <- eigen(cov(insample))
    eigenvec <- eigend$vectors
    eigenval <- eigend$values
    invmat <- eigenvec[, 1:dimax] %*%
(t(eigenvec[, 1:dimax]) / eigenval[1:dimax])
    # Shrink the in-sample returns to their mean
    insample <- (1-alpha)*insample + alpha*rowMeans(insample)
    # Calculate the maximum Sharpe ratio portfolio weights
    weightv <- invmat %*% colMeans(insample)
    weightv <- drop(weightv/sqrt(sum(weightv^2)))
    # Calculate the out-of-sample portfolio returns
    outsample <- retp[(endd[ep-1]+1):endd[ep], ]
    xts::xts(outsample %*% weightv, zoo::index(outsample))
})  # end lapply
pnls <- do.call(rbind, pnls)
# Plot dygraph of rolling ETF portfolio strategy
pnls <- pnls*sd(indeks)/sd(pnls)
pnls <- rbind(indeks[paste0("/", start(pnls)-1)], pnls)
wealthv <- cbind(indeks, pnls, (pnls+indeks)/2)
colnames(wealthv) <- c("Index", "Strategy", "Combined")
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
dygraphs::dygraph(cumsum(wealthv)[endd], main="Rolling Portfolio Strategy With Return Shrinkage") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Define backtest functional for rolling portfolio strategy
roll_portf <- function(excess, # Excess returns
                 returns, # Stock returns
                 endd, # End points
                 look_back=12, # Look-back interval
                 dimax=3, # Dimension reduction intensity
                 alpha=0.0, # Return shrinkage intensity
                 bid_offer=0.0, # Bid-offer spread
                 ...) {
  npts <- NROW(endd)
  startp <- c(rep_len(0, look_back), endd[1:(npts-look_back)])
  pnls <- lapply(2:npts, function(ep) {
    # Calculate regularized inverse of covariance matrix
    insample <- excess[startp[ep-1]:endd[ep-1], ]
    eigend <- eigen(cov(insample))
    eigenvec <- eigend$vectors
    eigenval <- eigend$values
    invmat <- eigenvec[, 1:dimax] %*%
(t(eigenvec[, 1:dimax]) / eigenval[1:dimax])
    # Shrink the in-sample returns to their mean
    insample <- (1-alpha)*insample + alpha*rowMeans(insample)
    # Calculate the maximum Sharpe ratio portfolio weights
    weightv <- invmat %*% colMeans(insample)
    weightv <- drop(weightv/sqrt(sum(weightv^2)))
    # Calculate the out-of-sample portfolio returns
    outsample <- returns[(endd[ep-1]+1):endd[ep], ]
    xts::xts(outsample %*% weightv, zoo::index(outsample))
  })  # end lapply
  pnls <- do.call(rbind, pnls)
  # Add warmup period to pnls
  rbind(indeks[paste0("/", start(pnls)-1)], pnls)
}  # end roll_portf
# Simulate a monthly ETF momentum strategy
pnls <- roll_portf(excess=retx, returns=retp, endd=endd,
  look_back=look_back, dimax=dimax)
# Perform sapply loop over look_backs
look_backs <- seq(2, 15, by=1)
pnls <- lapply(look_backs, roll_portf,
  returns=retp, excess=retx, endd=endd, dimax=dimax)
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("look_back=", look_backs)
pnlsums <- sapply(pnls, sum)
look_back <- look_backs[which.max(pnlsums)]
# Plot dygraph of daily ETF momentum strategies
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls)[endd],
  main="Rolling Portfolio Strategies") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Plot EWMA strategies using quantmod
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(pnls))
quantmod::chart_Series(cumsum(pnls),
  theme=plot_theme, name="Rolling Portfolio Strategies")
legend("bottomleft", legend=colnames(pnls),
  inset=0.02, bg="white", cex=0.7, lwd=rep(6, NCOL(retp)),
  col=plot_theme$col$line.col, bty="n")
# Perform backtest for different dimax values
eigenvals <- 2:11
pnls <- lapply(eigenvals, roll_portf, excess=retx,
  returns=retp, endd=endd, look_back=look_back)
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("eigenval=", eigenvals)
pnlsums <- sapply(pnls, sum)
dimax <- eigenvals[which.max(pnlsums)]
# Plot dygraph of daily ETF momentum strategies
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls)[endd],
  main="Rolling Portfolio Strategies With Dimension Reduction") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Plot EWMA strategies using quantmod
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(pnls))
quantmod::chart_Series(cumsum(pnls),
  theme=plot_theme, name="Rolling Portfolio Strategies")
legend("bottomleft", legend=colnames(pnls),
  inset=0.02, bg="white", cex=0.7, lwd=rep(6, NCOL(retp)),
  col=plot_theme$col$line.col, bty="n")
# Perform backtest over vector of return shrinkage intensities
alphav <- seq(from=0.0, to=0.9, by=0.1)
pnls <- lapply(alphav, roll_portf, excess=retx,
  returns=retp, endd=endd, look_back=look_back, dimax=dimax)
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("alpha=", alphav)
pnlsums <- sapply(pnls, sum)
alpha <- alphav[which.max(pnlsums)]
# Plot dygraph of daily ETF momentum strategies
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls)[endd],
  main="Rolling Portfolio Strategies With Return Shrinkage") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Plot EWMA strategies using quantmod
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(pnls))
quantmod::chart_Series(cumsum(pnls),
  theme=plot_theme, name="Rolling Portfolio Strategies")
legend("bottomleft", legend=colnames(pnls),
  inset=0.02, bg="white", cex=0.7, lwd=rep(6, NCOL(retp)),
  col=plot_theme$col$line.col, bty="n")
load("/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
# Overwrite NA values in returns
retp <- returns["2000/"]
nstocks <- NCOL(retp)
retp[1, is.na(retp[1, ])] <- 0
retp <- zoo::na.locf(retp, na.rm=FALSE)
datev <- zoo::index(retp)
riskf <- 0.03/252
retx <- (retp - riskf)
retis <- retp["/2010"]
retsos <- retp["2011/"]
# Maximum Sharpe weights in-sample interval
covmat <- cov(retis)
invmat <- MASS::ginv(covmat)
weightv <- invmat %*% colMeans(retx["/2010"])
weightv <- drop(weightv/sqrt(sum(weightv^2)))
names(weightv) <- colnames(retp)
# Calculate portfolio returns
insample <- xts::xts(retis %*% weightv, zoo::index(retis))
outsample <- xts::xts(retsos %*% weightv, zoo::index(retsos))
indeks <- xts::xts(rowMeans(retp), datev)
# Combine in-sample and out-of-sample returns
pnls <- rbind(insample, outsample)
pnls <- pnls*sd(indeks)/sd(pnls)
pnls <- cbind(indeks, pnls)
colnames(pnls) <- c("Equal Weight", "Optimal")
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(pnls[index(outsample)],
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of cumulative portfolio returns
endd <- rutils::calc_endpoints(pnls, interval="weeks")
dygraphs::dygraph(cumsum(pnls)[endd],
  main="Out-of-sample Optimal Portfolio Returns for Stocks") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyEvent(zoo::index(last(retis[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=300)
# Calculate regularized inverse of covariance matrix
look_back <- 8; dimax <- 21
eigend <- eigen(cov(retis))
eigenvec <- eigend$vectors
eigenval <- eigend$values
invmat <- eigenvec[, 1:dimax] %*%
  (t(eigenvec[, 1:dimax]) / eigenval[1:dimax])
# Calculate portfolio weights
weightv <- invmat %*% colMeans(retx["/2010"])
weightv <- drop(weightv/sqrt(sum(weightv^2)))
names(weightv) <- colnames(retp)
# Calculate portfolio returns
insample <- xts::xts(retis %*% weightv, zoo::index(retis))
outsample <- xts::xts(retsos %*% weightv, zoo::index(retsos))
indeks <- xts::xts(rowMeans(retp), datev)
# Combine in-sample and out-of-sample returns
pnls <- rbind(insample, outsample)
pnls <- pnls*sd(indeks)/sd(pnls)
pnls <- cbind(indeks, pnls)
colnames(pnls) <- c("Equal Weight", "Optimal")
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(pnls[index(outsample)],
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of cumulative portfolio returns
endd <- rutils::calc_endpoints(pnls, interval="weeks")
dygraphs::dygraph(cumsum(pnls)[endd],
  main="Out-of-sample Returns for Stocks with Dimension Reduction") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyEvent(zoo::index(last(retis[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=300)
# Shrink the in-sample returns to their mean
alpha <- 0.7
retxm <- rowMeans(retx["/2010"])
retxis <- (1-alpha)*retx["/2010"] + alpha*retxm
# Calculate portfolio weights
weightv <- invmat %*% colMeans(retxis)
weightv <- drop(weightv/sqrt(sum(weightv^2)))
# Calculate portfolio returns
insample <- xts::xts(retis %*% weightv, zoo::index(retis))
outsample <- xts::xts(retsos %*% weightv, zoo::index(retsos))
# Combine in-sample and out-of-sample returns
pnls <- rbind(insample, outsample)
pnls <- pnls*sd(indeks)/sd(pnls)
pnls <- cbind(indeks, pnls)
colnames(pnls) <- c("Equal Weight", "Optimal")
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(pnls[index(outsample)],
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of cumulative portfolio returns
dygraphs::dygraph(cumsum(pnls)[endd],
  main="Out-of-sample Returns for Stocks with Return Shrinkage") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyEvent(zoo::index(last(retis[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=300)
library(RcppArmadillo)
# Source Rcpp functions from file
Rcpp::sourceCpp("/Users/jerzy/Develop/lecture_slides/scripts/back_test.cpp")
# Create random matrix of returns
matrixv <- matrix(rnorm(300), nc=5)
# Regularized inverse of covariance matrix
dimax <- 4
eigend <- eigen(cov(matrixv))
covinv <- eigend$vectors[, 1:dimax] %*%
  (t(eigend$vectors[, 1:dimax]) / eigend$values[1:dimax])
# Regularized inverse using RcppArmadillo
covinv_arma <- calc_inv(matrixv, dimax)
all.equal(covinv, covinv_arma)
# Microbenchmark RcppArmadillo code
library(microbenchmark)
summary(microbenchmark(
  rcode={eigend <- eigen(cov(matrixv))
    eigend$vectors[, 1:dimax] %*%
(t(eigend$vectors[, 1:dimax]) / eigend$values[1:dimax])
  },
  cppcode=calc_inv(matrixv, dimax),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
# Overwrite NA values in returns
retp <- returns100
retp[1, is.na(retp[1, ])] <- 0
retp <- zoo::na.locf(retp, na.rm=FALSE)
retx <- (retp - riskf)
nstocks <- NCOL(retp) ; datev <- zoo::index(retp)
# Define monthly end points
endd <- rutils::calc_endpoints(retp, interval="weeks")
endd <- endd[endd > (nstocks+1)]
npts <- NROW(endd) ; look_back <- 12
startp <- c(rep_len(0, look_back), endd[1:(npts-look_back)])
# Perform loop over end points - takes very long !!!
pnls <- lapply(2:npts, function(ep) {
    # Subset the excess returns
    insample <- retx[startp[ep-1]:endd[ep-1], ]
    invmat <- MASS::ginv(cov(insample))
    # Calculate the maximum Sharpe ratio portfolio weights
    weightv <- invmat %*% colMeans(insample)
    weightv <- drop(weightv/sqrt(sum(weightv^2)))
    # Calculate the out-of-sample portfolio returns
    outsample <- retp[(endd[ep-1]+1):endd[ep], ]
    xts::xts(outsample %*% weightv, zoo::index(outsample))
})  # end lapply
pnls <- rutils::do_call(rbind, pnls)
# Calculate returns of equal weight portfolio
indeks <- xts::xts(rowMeans(retp), datev)
pnls <- rbind(indeks[paste0("/", start(pnls)-1)], pnls*sd(indeks)/sd(pnls))
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(indeks, pnls)
colnames(wealthv) <- c("Equal Weight", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot cumulative strategy returns
dygraphs::dygraph(cumsum(wealthv)[endd], main="Rolling Portfolio Optimization Strategy for S&P500 Stocks") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Shift end points to C++ convention
endd <- (endd - 1)
endd[endd < 0] <- 0
startp <- (startp - 1)
startp[startp < 0] <- 0
# Specify dimension reduction and return shrinkage using list of portfolio optimization parameters
controlv <- HighFreq::param_portf(method="maxsharpe", dimax=21, alpha=0.7)
# Perform backtest in Rcpp
pnls <- HighFreq::back_test(excess=retx, returns=retp,
  startp=startp, endd=endd, controlv=controlv)
pnls <- pnls*sd(indeks)/sd(pnls)
# Plot cumulative strategy returns
wealthv <- cbind(indeks, pnls, (pnls+indeks)/2)
colnames(wealthv) <- c("Index", "Strategy", "Combined")
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
dygraphs::dygraph(cumsum(wealthv)[endd], main="Rolling S&P500 Portfolio Optimization Strategy With Shrinkage") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Perform backtest over vector of return shrinkage intensities
alphav <- seq(from=0.01, to=0.91, by=0.1)
pnls <- lapply(alphav, function(alpha) {
  HighFreq::back_test(excess=retx, returns=retp,
  startp=startp, endd=endd, controlv=controlv)
})  # end lapply
profilev <- sapply(pnls, sum)
plot(x=alphav, y=profilev, t="l",
  main="Rolling Strategy as Function of Return Shrinkage",
  xlab="Shrinkage Intensity Alpha", ylab="pnl")
whichmax <- which.max(profilev)
alpha <- alphav[whichmax]
pnls <- pnls[[whichmax]]
# Perform backtest over vector of dimension reduction eigenvals
eigenvals <- seq(from=3, to=40, by=2)
pnls <- lapply(eigenvals, function(dimax) {
  HighFreq::back_test(excess=retx, returns=retp,
    startp=startp, endd=endd, controlv=controlv)
})  # end lapply
profilev <- sapply(pnls, sum)
plot(x=eigenvals, y=profilev, t="l",
  main="Strategy PnL as Function of dimax",
  xlab="dimax", ylab="pnl")
whichmax <- which.max(profilev)
dimax <- eigenvals[whichmax]
pnls <- pnls[[whichmax]]
pnls <- pnls*sd(indeks)/sd(pnls)
# Plot cumulative strategy returns
wealthv <- cbind(indeks, pnls, (pnls+indeks)/2)
colnames(wealthv) <- c("Index", "Strategy", "Combined")
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
dygraphs::dygraph(cumsum(wealthv)[endd], main="Optimal Rolling S&P500 Portfolio Strategy") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Perform backtest over look-backs
look_backs <- seq(from=3, to=12, by=1)
pnls <- lapply(look_backs, function(look_back) {
  startp <- c(rep_len(0, look_back), endd[1:(npts-look_back)])
  startp <- (startp - 1)
  startp[startp < 0] <- 0
  HighFreq::back_test(excess=retx, returns=retp,
    startp=startp, endd=endd, controlv=controlv)
})  # end lapply
profilev <- sapply(pnls, sum)
plot(x=look_backs, y=profilev, t="l", main="Strategy PnL as Function of Look-back Interval",
  xlab="Look-back Interval", ylab="pnl")
whichmax <- which.max(profilev)
look_back <- look_backs[whichmax]
pnls <- pnls[[whichmax]]
pnls <- pnls*sd(indeks)/sd(pnls)
# Plot cumulative strategy returns
wealthv <- cbind(indeks, pnls, (pnls+indeks)/2)
colnames(wealthv) <- c("Index", "Strategy", "Combined")
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
dygraphs::dygraph(cumsum(wealthv)[endd], main="Optimal Rolling S&P500 Portfolio Strategy") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
