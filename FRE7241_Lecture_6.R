# Load daily S&P500 percentage stock returns
load(file="/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
# Select returns of 100 stocks
retp <- retstock100
datev <- zoo::index(retp) # Dates vector
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
hist(perfstat)
perfstat <- sort(perfstat, decreasing=TRUE)
perfstat

# Calculate weights proportional to performance statistic
# With quadratic constraint
weightv <- perfstat/sqrt(sum(perfstat^2))
sum(weightv^2)
sum(weightv)
weightv

# Calculate weights proportional to performance statistic
# With linear constraint
weightv <- perfstat/sum(perfstat)
sum(weightv^2)
sum(weightv)
weightv

# Calculate the weighted returns using transpose
retw <- t(t(retp)*perfstat)
# Or using Rcpp
retf <- HighFreq::mult_mat(perfstat, retp)
all.equal(retw, retf, check.attributes=FALSE)
# Calculate the in-sample portfolio volatility
volis <- sd(rowMeans(retw, na.rm=TRUE))
# Calculate the equal weight portfolio volatility
volew <- sd(rowMeans(retp, na.rm=TRUE))
# Apply the volatility constraint
weightv <- volew*perfstat/volis
# Calculate the in-sample portfolio volatility
retw <- t(t(retp)*weightv)
all.equal(sd(rowMeans(retw, na.rm=TRUE)), volew)
# Apply the volatility target constraint
volt <- 0.01
weightv <- volt*perfstat/volis
retw <- t(t(retp)*weightv)
all.equal(sd(rowMeans(retw, na.rm=TRUE)), volt)
# Compare speed of R with Rcpp
library(microbenchmark)
summary(microbenchmark(
  trans=t(t(retp)*perfstat),
  rcpp=HighFreq::mult_mat(perfstat, retp),
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
  if (NROW(retp) > 12) {
    varv <- var(retp)
    if (varv > 0) mean(retp)/varv else 0
  }
  else 0
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
# Calculate weights proportional to performance statistic
weightv <- perfstat
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
pnls <- HighFreq::mult_mat(weightv, retp)
pnls <- rowMeans(pnls, na.rm=TRUE)
all.equal(sd(indeks), sd(pnls))

# Calculate a vector of monthly end points
endd <- rutils::calc_endpoints(retp, interval="months")
npts <- NROW(endd)
# Perform loop over the end points
look_back <- 8
pnls <- lapply(3:(npts-1), function(tday) {
  # Select the look-back returns
  startp <- endd[max(1, tday-look_back)]
  retis <- retp[startp:endd[tday], ]
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
  pnls <- HighFreq::mult_mat(weightv, retp[(endd[tday]+1):endd[tday+1], ])
  pnls <- rowMeans(pnls, na.rm=TRUE)
  drop(pnls)
})  # end lapply
pnls <- rutils::do_call(c, pnls)

# Calculate the average of all stock returns
indeks <- rowMeans(retp, na.rm=TRUE)
# Add initial startup interval to the momentum returns
pnls <- c(indeks[endd[1]:endd[3]], pnls)
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(indeks, pnls)
wealthv <- xts::xts(wealthv, order.by=datev)
colnames(wealthv) <- c("Index", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))

# Plot dygraph of stock index and momentum strategy
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Stock Index and Momentum Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

btmomtop <- function(retp, objfun, look_back=12, rebalf="months", topstocks=10,
 bidask=0.0, endd=rutils::calc_endpoints(retp, interval=rebalf), ...) {
  # Perform loop over end points
  npts <- NROW(endd)
  pnls <- lapply(3:(npts-1), function(tday) {
    # Select the look-back returns
    startp <- endd[max(1, tday-look_back)]
    retis <- retp[startp:endd[tday], ]
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
    pnls <- HighFreq::mult_mat(weightv, retp[(endd[tday]+1):endd[tday+1], ])
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
# Add initial startup interval to the momentum returns
pnls <- c(indeks[endd[1]:endd[3]], pnls)
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(indeks, pnls)
wealthv <- xts::xts(wealthv, order.by=datev)
colnames(wealthv) <- c("Index", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))

# Plot dygraph of stock index and momentum strategy
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Optimal Momentum Strategy for Stocks") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

btmomweight <- function(retp, objfun, look_back=12, rebalf="months",
  bidask=0.0, endd=rutils::calc_endpoints(retp, interval=rebalf), ...) {
  # Perform loop over end points
  npts <- NROW(endd)
  pnls <- lapply(3:(npts-1), function(tday) {
    # Select the look-back returns
    startp <- endd[max(1, tday-look_back)]
    retis <- retp[startp:endd[tday], ]
    # Calculate weights proportional to performance
    perfstat <- sapply(retis, objfun)
    weightv <- perfstat
    # Calculate the in-sample portfolio returns
    pnls <- HighFreq::mult_mat(weightv, retis)
    pnls <- rowMeans(pnls, na.rm=TRUE)
    # Scale weights so in-sample pnl volatility is same as equal weight
    weightv <- weightv*sd(rowMeans(retis, na.rm=TRUE))/sd(pnls)
    # Calculate the out-of-sample momentum returns
    pnls <- HighFreq::mult_mat(weightv, retp[(endd[tday]+1):endd[tday+1], ])
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
# Add initial startup interval to the momentum returns
pnls <- c(indeks[endd[1]:endd[3]], pnls)
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(indeks, pnls, 0.5*(indeks+pnls))
wealthv <- xts::xts(wealthv, order.by=datev)
colnames(wealthv) <- c("Index", "Momentum", "Combined")
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
bidask <- 0.0
costs <- 0.5*bidask*rowSums(abs(rutils::diffit(weightv)))
pnls <- (pnls - costs)

# Scale the momentum volatility to the equal weight index
indeksd <- sd(indeks)
pnls <- indeksd*pnls/sd(pnls)
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(indeks, pnls, 0.5*(indeks+pnls))
wealthv <- xts::xts(wealthv, datev)
colnames(wealthv) <- c("Index", "Momentum", "Combined")
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
btmomdaily <- function(retp, lambda=0.9, trend=1, bidask=0.0, ...) {
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
  costs <- 0.5*bidask*rowSums(abs(rutils::diffit(weightv)))
  (pnls - costs)
}  # end btmomdaily

# Simulate multiple daily stock momentum strategies
lambdas <- seq(0.98, 0.996, 0.002)
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
btmomdailyhold <- function(retp, lambda=0.9, trend=1, bidask=0.0, ...) {
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
  costs <- 0.5*bidask*rowSums(abs(rutils::diffit(weightv)))
  (pnls - costs)
}  # end btmomdailyhold

# Simulate multiple daily stock momentum strategies with holding periods
lambdas <- seq(0.98, 0.996, 0.002)
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
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(indeks, pnls, 0.5*(indeks+pnls))
colnames(wealthv) <- c("Index", "Momentum", "Combined")
cor(wealthv)
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))

# Plot dygraph of stock index and momentum strategy
endd <- rutils::calc_endpoints(retp, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Optimal Daily Momentum Strategy for Stocks") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
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
wealthv <- na.omit(rutils::etfenv$prices[, c("VTI", "MTUM")])
wealthv <- rutils::diffit(log(wealthv))
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
dimax <- 11
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
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(indeks, pnls, 0.5*(indeks+pnls))
colnames(wealthv) <- c("Index", "Momentum", "Combined")
cor(wealthv)
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))

# Plot dygraph of stock index and PCA momentum strategy
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Optimal Daily Momentum Strategy for Stocks") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", label="Combined", strokeWidth=3) %>%
  dyLegend(show="always", width=300)

# Simulate daily PCA momentum strategies for multiple lambda parameters
lambdas <- seq(0.4, 0.8, 0.1)
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
pcad <- prcomp(retp[insample])
# Calculate the out-of-sample PCA time series
retpca <- xts::xts(retp[outsample] %*% pcad$rotation, order.by=datev[outsample])
# Simulate daily PCA momentum strategies for multiple lambda parameters
lambdas <- seq(0.98, 0.99, 0.003)
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
lambdas <- seq(0.4, 0.8, 0.1)
pnls <- mclapply(lambdas, btmomdailyhold, retp=retpca[, (dimax+1):(nstocks-10)],
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
retp <- na.omit(rutils::etfenv$returns[-1, symbolv])
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
indeks <- rowMeans(retp)
pnls <- c(indeks[endd[1]:endd[3]], pnls)
# Calculate returns of all-weather benchmark
weightaw <- c(0.30, 0.55, 0.15)
retaw <- retp %*% weightaw

# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retaw, pnls, 0.5*(retaw+pnls))
wealthv <- xts::xts(wealthv, order.by=datev)
colnames(wealthv) <- c("All-weather", "Momentum", "Combined")
cor(wealthv)
wealthv <- xts::xts(wealthv, order.by=datev)
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of stock index and momentum strategy
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Optimal Momentum Strategy and All-weather for ETFs") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", label="Combined", strokeWidth=3) %>%
  dyLegend(show="always", width=300)

# Calculate the momentum weights
look_back <- look_backs[whichmax]
weightv <- lapply(2:npts, function(tday) {
  # Select the look-back returns
  startp <- endd[max(1, tday-look_back)]
  retis <- retp[startp:endd[tday], ]
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
betasetf <- sapply(retp, function(x) cov(retp$VTI, x)/var(retp$VTI))
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
wealthv <- cbind(retaw, pnls, 0.5*(pnls + retaw))
colnames(wealthv) <- c("All-weather", "Momentum", "Combined")
wealthv <- xts::xts(wealthv, datev)
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Calculate strategy correlations
cor(wealthv)

# Plot ETF momentum strategy combined with All-Weather
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Optimal Momentum Strategy and All-weather for ETFs") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
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
bidask <- 0.0
costs <- 0.5*bidask*rowSums(abs(rutils::diffit(weightv)))
pnls <- (pnls - costs)

# Scale the momentum volatility to all-weather
pnls <- sd(retaw)*pnls/sd(pnls)
# Calculate the wealth of momentum returns
wealthv <- cbind(retaw, pnls, 0.5*(pnls + retaw))
colnames(wealthv) <- c("All-weather", "Momentum", "Combined")
wealthv <- xts::xts(wealthv, datev)
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
cor(wealthv)
# Plot dygraph of the momentum strategy returns
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Daily Momentum Strategy for ETFs vs All-Weather") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", label="Combined", strokeWidth=3) %>%
  dyLegend(show="always", width=300)

# Select all the ETF symbols except "VXX", "SVXY" "MTUM", "QUAL", "VLUE", "USMV", and "AIEQ"
symbolv <- colnames(rutils::etfenv$returns)
# VYM has bad data in 2006
symbolv <- symbolv[!(symbolv %in% c("VXX", "SVXY", "MTUM", "QUAL", "VLUE", "USMV", "AIEQ", "VYM"))]
# Extract columns of rutils::etfenv$returns and overwrite NA values
retp <- rutils::etfenv$returns[, symbolv]
retp[1, ] <- 0.01
nstocks <- NCOL(retp)
datev <- zoo::index(retp)
# Calculate the covariance ignoring NA values
covmat <- cov(retp, use="pairwise.complete.obs")
sum(is.na(covmat))
# Calculate the inverse of covmat
invmat <- solve(covmat)
# Calculate the generalized inverse of covmat
invreg <- MASS::ginv(covmat)
all.equal(unname(invmat), invreg)

# Create rectangular matrix with collinear columns
matv <- matrix(rnorm(10*8), nc=10)
# Calculate covariance matrix
covmat <- cov(matv)
# Calculate inverse of covmat - error
invmat <- solve(covmat)
# Perform eigen decomposition
eigend <- eigen(covmat)
eigenvec <- eigend$vectors
eigenval <- eigend$values
# Set tolerance for determining zero singular values
precv <- sqrt(.Machine$double.eps)
# Calculate generalized inverse from the eigen decomposition
notzero <- (eigenval > (precv*eigenval[1]))
inveigen <- eigenvec[, notzero] %*%
  (t(eigenvec[, notzero]) / eigenval[notzero])
# Verify inverse property of invreg
all.equal(covmat, inveigen %*% covmat)
# Verify generalized inverse property of invreg
all.equal(covmat, covmat %*% inveigen %*% covmat)
# Calculate generalized inverse of covmat
invreg <- MASS::ginv(covmat)
# Verify that inveigen is the same as invreg
all.equal(inveigen, invreg)

# Returns in excess of risk-free rate
riskf <- 0.03/252
retx <- (retp - riskf)
# Maximum Sharpe weights in-sample interval
retis <- retp["/2014"]
invreg <- MASS::ginv(cov(retis, use="pairwise.complete.obs"))
weightv <- drop(invreg %*% colMeans(retx["/2014"], na.rm=TRUE))
# Calculate in-sample portfolio returns
pnlis <- HighFreq::mult_mat(weightv, retis)
pnlis <- rowMeans(pnlis, na.rm=TRUE)
# Scale the weights to volatility target
weightv <- weightv*0.01/sd(pnlis)
names(weightv) <- colnames(retp)

# Plot portfolio weights
barplot(sort(weightv), main="Maximum Sharpe Weights", cex.names=0.7)

# Calculate the equal weight index
indeks <- xts::xts(rowMeans(retis, na.rm=TRUE), zoo::index(retis))
# Calculate the in-sample weighted returns using transpose
pnlis <- unname(t(t(retis)*weightv))
# Or using Rcpp
# pnlis <- HighFreq::mult_mat(weightv, retis)
pnlis <- rowMeans(pnlis, na.rm=TRUE)
pnlis <- pnlis*sd(indeks)/sd(pnlis)

# Dygraph cumulative wealth
wealthv <- cbind(indeks, pnlis, (pnlis + indeks)/2)
colnames(wealthv) <- c("Equal Weight", "Optimal", "Combined")
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Dygraph cumulative wealth
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="In-Sample Optimal Portfolio Returns") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", label="Combined", strokeWidth=3) %>%
  dyLegend(width=300)

# Calculate the equal weight index
retos <- retp["2015/"]
indeks <- xts::xts(rowMeans(retos, na.rm=TRUE), zoo::index(retos))
# Calculate out-of-sample portfolio returns
pnlos <- HighFreq::mult_mat(weightv, retos)
pnlos <- rowMeans(pnlos, na.rm=TRUE)
pnlos <- pnlos*sd(indeks)/sd(pnlos)
wealthv <- cbind(indeks, pnlos, (pnlos + indeks)/2)
colnames(wealthv) <- c("Equal Weight", "Optimal", "Combined")
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))

# Dygraph cumulative wealth
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Out-of-Sample Optimal Portfolio Returns") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", label="Combined", strokeWidth=3) %>%
  dyLegend(width=300)

# Maximum Sharpe weights in-sample interval
invreg <- MASS::ginv(cov(retis, use="pairwise.complete.obs"))
weightv <- invreg %*% colMeans(retx["/2014"], na.rm=TRUE)
names(weightv) <- colnames(retp)
# Calculate in-sample portfolio returns
pnlis <- HighFreq::mult_mat(weightv, retis)
pnlis <- rowMeans(pnlis, na.rm=TRUE)
# Scale the weights to volatility target
weightv <- weightv*0.01/sd(pnlis)
# Calculate out-of-sample portfolio returns
pnlos <- HighFreq::mult_mat(weightv, retos)
pnlos <- rowMeans(pnlos, na.rm=TRUE)
# Calculate cumulative wealth
pnls <- c(pnlis, pnlos)
pnls <- pnls*sd(indeks)/sd(pnls)
indeks <- xts::xts(rowMeans(retp, na.rm=TRUE), datev)
wealthv <- cbind(indeks, pnls, (pnls + indeks)/2)
colnames(wealthv) <- c("Equal Weight", "Optimal", "Combined")
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))

# Dygraph cumulative wealth
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Out-of-Sample Optimal Portfolio Returns for ETFs") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", label="Combined", strokeWidth=3) %>%
  dyEvent(zoo::index(last(retis[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=300)

# Calculate in-sample covariance matrix
covmat <- cov(retis, use="pairwise.complete.obs")
eigend <- eigen(covmat)
eigenvec <- eigend$vectors
eigenval <- eigend$values
# Negative eigenvalues
eigenval
# Calculate reduced inverse of covariance matrix
dimax <- 9
invred <- eigenvec[, 1:dimax] %*%
  (t(eigenvec[, 1:dimax]) / eigenval[1:dimax])
# Reduced inverse does not satisfy matrix inverse property
all.equal(covmat, covmat %*% invred %*% covmat)

# Calculate portfolio weights
weightv <- invred %*% colMeans(retis, na.rm=TRUE)
names(weightv) <- colnames(retp)
# Calculate in-sample portfolio returns
pnlis <- HighFreq::mult_mat(weightv, retis)
pnlis <- rowMeans(pnlis, na.rm=TRUE)
# Scale the weights to volatility target
weightv <- weightv*0.01/sd(pnlis)
# Calculate out-of-sample portfolio returns
pnlos <- HighFreq::mult_mat(weightv, retos)
pnlos <- rowMeans(pnlos, na.rm=TRUE)
pnls <- c(pnlis, pnlos)
pnls <- pnls*sd(indeks)/sd(pnls)
wealthv <- cbind(indeks, pnls, (pnls + indeks)/2)
colnames(wealthv) <- c("Equal Weight", "DimReduction", "Combined")
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))

# Dygraph cumulative wealth
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Optimal Portfolio Returns With Dimension Reduction") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", label="Combined", strokeWidth=3) %>%
  dyEvent(zoo::index(last(retis[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=300)

# Shrink the in-sample returns to their mean
alpha <- 0.7
retxm <- rowMeans(retx["/2014"], na.rm=TRUE)
retxis <- (1-alpha)*retx["/2014"] + alpha*retxm
# Calculate portfolio weights
weightv <- invred %*% colMeans(retxis, na.rm=TRUE)
# Calculate in-sample portfolio returns
pnlis <- HighFreq::mult_mat(weightv, retis)
pnlis <- rowMeans(pnlis, na.rm=TRUE)
# Scale the weights to volatility target
weightv <- weightv*0.01/sd(pnlis)
# Calculate out-of-sample portfolio returns
pnlos <- HighFreq::mult_mat(weightv, retos)
pnlos <- rowMeans(pnlos, na.rm=TRUE)
pnls <- c(pnlis, pnlos)
pnls <- pnls*sd(indeks)/sd(pnls)
wealthv <- cbind(indeks, pnls, (pnls + indeks)/2)
colnames(wealthv) <- c("Equal Weight", "Optimal", "Combined")

# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Dygraph cumulative wealth
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Optimal Portfolio With Dimension Reduction and Return Shrinkage") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", label="Combined", strokeWidth=3) %>%
  dyEvent(zoo::index(last(retis[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=300)

# Define monthly end points
endd <- rutils::calc_endpoints(retp, interval="months")
endd <- endd[endd > (nstocks+1)]
npts <- NROW(endd)
look_back <- 3
startp <- c(rep_len(0, look_back), endd[1:(npts-look_back)])
# Perform loop over end points
pnls <- lapply(1:(npts-1), function(tday) {
    # Calculate the portfolio weights
    retis <- retx[startp[tday]:endd[tday], ]
    covmat <- cov(retis, use="pairwise.complete.obs")
    covmat[is.na(covmat)] <- 0
    invreg <- MASS::ginv(covmat)
    colm <- colMeans(retis, na.rm=TRUE)
    colm[is.na(colm)] <- 0
    weightv <- invreg %*% colm
    # Scale the weights to volatility target
    pnlis <- HighFreq::mult_mat(weightv, retis)
    pnlis <- rowMeans(pnlis, na.rm=TRUE)
    weightv <- weightv*0.01/sd(pnlis)
    # Calculate the out-of-sample portfolio returns
    retos <- retp[(endd[tday]+1):endd[tday+1], ]
    pnlos <- HighFreq::mult_mat(weightv, retos)
    pnlos <- rowMeans(pnlos, na.rm=TRUE)
    xts::xts(pnlos, zoo::index(retos))
})  # end lapply
pnls <- do.call(rbind, pnls)
pnls <- rbind(indeks[paste0("/", start(pnls)-1)], pnls)

# Calculate the Sharpe and Sortino ratios
pnls <- pnls*sd(indeks)/sd(pnls)
wealthv <- cbind(indeks, pnls, (pnls+indeks)/2)
colnames(wealthv) <- c("Index", "PortfStrat", "Combined")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))

# Dygraph cumulative wealth
dygraphs::dygraph(cumsum(wealthv)[endd], main="Monthly ETF Rolling Portfolio Strategy") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", label="Combined", strokeWidth=3) %>%
  dyLegend(show="always", width=300)

# Perform loop over end points
dimax <- 9
pnls <- lapply(1:(npts-1), function(tday) {
    # Calculate the portfolio weights
    retis <- retx[startp[tday]:endd[tday], ]
    covmat <- cov(retis, use="pairwise.complete.obs")
    covmat[is.na(covmat)] <- 0
    eigend <- eigen(covmat)
    eigenvec <- eigend$vectors
    eigenval <- eigend$values
    invred <- eigenvec[, 1:dimax] %*%
(t(eigenvec[, 1:dimax]) / eigenval[1:dimax])
    colm <- colMeans(retis, na.rm=TRUE)
    colm[is.na(colm)] <- 0
    weightv <- invred %*% colm
    # Scale the weights to volatility target
    pnlis <- HighFreq::mult_mat(weightv, retis)
    pnlis <- rowMeans(pnlis, na.rm=TRUE)
    weightv <- weightv*0.01/sd(pnlis)
    # Calculate the out-of-sample portfolio returns
    retos <- retp[(endd[tday]+1):endd[tday+1], ]
    pnlos <- HighFreq::mult_mat(weightv, retos)
    pnlos <- rowMeans(pnlos, na.rm=TRUE)
    xts::xts(pnlos, zoo::index(retos))
})  # end lapply
pnls <- do.call(rbind, pnls)
pnls <- rbind(indeks[paste0("/", start(pnls)-1)], pnls)

# Calculate the Sharpe and Sortino ratios
pnls <- pnls*sd(indeks)/sd(pnls)
wealthv <- cbind(indeks, pnls, (pnls+indeks)/2)
colnames(wealthv) <- c("Index", "PortfStrat", "Combined")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Dygraph cumulative wealth
dygraphs::dygraph(cumsum(wealthv)[endd], main="Rolling Portfolio Strategy With Dimension Reduction") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", label="Combined", strokeWidth=3) %>%
  dyLegend(show="always", width=300)

alpha <- 0.7 # Return shrinkage intensity
# Perform loop over end points
pnls <- lapply(1:(npts-1), function(tday) {
    # Shrink the in-sample returns to their mean
    retis <- retx[startp[tday]:endd[tday], ]
    rowm <- rowMeans(retis, na.rm=TRUE)
    rowm[is.na(rowm)] <- 0
    retis <- (1-alpha)*retis + alpha*rowm
    # Calculate the portfolio weights
    covmat <- cov(retis, use="pairwise.complete.obs")
    covmat[is.na(covmat)] <- 0
    eigend <- eigen(covmat)
    eigenvec <- eigend$vectors
    eigenval <- eigend$values
    invred <- eigenvec[, 1:dimax] %*%
(t(eigenvec[, 1:dimax]) / eigenval[1:dimax])
    colm <- colMeans(retis, na.rm=TRUE)
    colm[is.na(colm)] <- 0
    weightv <- invred %*% colm
    # Scale the weights to volatility target
    pnlis <- HighFreq::mult_mat(weightv, retis)
    pnlis <- rowMeans(pnlis, na.rm=TRUE)
    weightv <- weightv*0.01/sd(pnlis)
    # Calculate the out-of-sample portfolio returns
    retos <- retp[(endd[tday]+1):endd[tday+1], ]
    pnlos <- HighFreq::mult_mat(weightv, retos)
    pnlos <- rowMeans(pnlos, na.rm=TRUE)
    xts::xts(pnlos, zoo::index(retos))
})  # end lapply
pnls <- do.call(rbind, pnls)
pnls <- rbind(indeks[paste0("/", start(pnls)-1)], pnls)

# Calculate the Sharpe and Sortino ratios
pnls <- pnls*sd(indeks)/sd(pnls)
wealthv <- cbind(indeks, pnls, (pnls+indeks)/2)
colnames(wealthv) <- c("Index", "PortfStrat", "Combined")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Dygraph cumulative wealth
dygraphs::dygraph(cumsum(wealthv)[endd], main="Rolling Portfolio Strategy With Return Shrinkage") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", label="Combined", strokeWidth=3) %>%
  dyLegend(show="always", width=300)

# Define backtest functional for rolling portfolio strategy
roll_portf <- function(retx, # Excess returns
                 retp, # Stock returns
                 endd, # End points
                 look_back=12, # Look-back interval
                 dimax=3, # Dimension reduction parameter
                 alpha=0.0, # Return shrinkage intensity
                 bidask=0.0, # Bid-offer spread
                 ...) {
  npts <- NROW(endd)
  startp <- c(rep_len(0, look_back), endd[1:(npts-look_back)])
  pnls <- lapply(1:(npts-1), function(tday) {
    retis <- retx[startp[tday]:endd[tday], ]
    # Shrink the in-sample returns to their mean
    if (alpha > 0) {
rowm <- rowMeans(retis, na.rm=TRUE)
rowm[is.na(rowm)] <- 0
retis <- (1-alpha)*retis + alpha*rowm
    } # end if
    # Calculate the portfolio weights
    covmat <- cov(retis, use="pairwise.complete.obs")
    covmat[is.na(covmat)] <- 0
    eigend <- eigen(covmat)
    eigenvec <- eigend$vectors
    eigenval <- eigend$values
    invred <- eigenvec[, 1:dimax] %*% (t(eigenvec[, 1:dimax]) / eigenval[1:dimax])
    colm <- colMeans(retis, na.rm=TRUE)
    colm[is.na(colm)] <- 0
    weightv <- invred %*% colm
    # Scale the weights to volatility target
    pnlis <- HighFreq::mult_mat(weightv, retis)
    pnlis <- rowMeans(pnlis, na.rm=TRUE)
    weightv <- weightv*0.01/sd(pnlis)
    # Calculate the out-of-sample portfolio returns
    retos <- retp[(endd[tday]+1):endd[tday+1], ]
    pnlos <- HighFreq::mult_mat(weightv, retos)
    pnlos <- rowMeans(pnlos, na.rm=TRUE)
    xts::xts(pnlos, zoo::index(retos))
  })  # end lapply
  pnls <- do.call(rbind, pnls)
  # Add warmup period to pnls
  rbind(indeks[paste0("/", start(pnls)-1)], pnls)
}  # end roll_portf

# Simulate a monthly ETF portfolio strategy
pnls <- roll_portf(retx=retx, retp=retp, endd=endd,
  look_back=look_back, dimax=dimax)
# Perform sapply loop over look_backs
look_backs <- seq(2, 15, by=1)
pnls <- lapply(look_backs, roll_portf,
  retp=retp, retx=retx, endd=endd, dimax=dimax)
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("lookb=", look_backs)
pnlsums <- sapply(pnls, sum)
look_back <- look_backs[which.max(pnlsums)]

# Plot dygraph of monthly ETF portfolio strategies
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls)[endd], main="Rolling Portfolio Strategies") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=600)
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
dimaxs <- 2:11
pnls <- lapply(dimaxs, roll_portf, retx=retx,
  retp=retp, endd=endd, look_back=look_back)
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("dimax=", dimaxs)
pnlsums <- sapply(pnls, sum)
dimax <- dimaxs[which.max(pnlsums)]

# Plot dygraph of monthly ETF portfolio strategies
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls)[endd],
  main="Rolling Portfolio Strategies With Dimension Reduction") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=500)
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
pnls <- lapply(alphav, roll_portf, retx=retx,
  retp=retp, endd=endd, look_back=look_back, dimax=dimax)
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("alpha=", alphav)
pnlsums <- sapply(pnls, sum)
alpha <- alphav[which.max(pnlsums)]

# Plot dygraph of monthly ETF portfolio strategies
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls)[endd],
  main="Rolling Portfolio Strategies With Return Shrinkage") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=500)
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
retp <- returns
nstocks <- NCOL(retp)
retp[is.na(retp)] <- 0
sum(is.na(retp))
datev <- zoo::index(retp)
riskf <- 0.03/252
retx <- (retp - riskf)
retis <- retp["/2014"]
retos <- retp["2015/"]
# Maximum Sharpe weights in-sample interval
covmat <- cov(retis, use="pairwise.complete.obs")
invreg <- MASS::ginv(covmat)
weightv <- invreg %*% colMeans(retx["/2014"], na.rm=TRUE)
names(weightv) <- colnames(retp)
# Calculate in-sample portfolio returns
pnlis <- (retis %*% weightv)
# Scale the weights to volatility target
weightv <- weightv*0.01/sd(pnlis)
# Calculate out-of-sample portfolio returns
pnlos <- (retos %*% weightv)
indeks <- xts::xts(rowMeans(retp), datev)
# Combine in-sample and out-of-sample returns
pnls <- c(pnlis, pnlos)
pnls <- pnls*sd(indeks)/sd(pnls)
wealthv <- cbind(indeks, pnls)
colnames(wealthv) <- c("Equal Weight", "Optimal")

# Calculate the in-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv[index(retis)],
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv[index(retos)],
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of cumulative portfolio returns
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Out-of-Sample Optimal Portfolio Returns for Stocks") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyEvent(zoo::index(last(retis[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=300)

# Calculate reduced inverse of covariance matrix
dimax <- 9
eigend <- eigen(cov(retis, use="pairwise.complete.obs"))
eigenvec <- eigend$vectors
eigenval <- eigend$values
invred <- eigenvec[, 1:dimax] %*%
  (t(eigenvec[, 1:dimax]) / eigenval[1:dimax])
# Calculate portfolio weights
weightv <- invred %*% colMeans(retx["/2014"], na.rm=TRUE)
names(weightv) <- colnames(retp)
# Calculate in-sample portfolio returns
pnlis <- (retis %*% weightv)
# Scale the weights to volatility target
weightv <- weightv*0.01/sd(pnlis)
# Calculate out-of-sample portfolio returns
pnlos <- (retos %*% weightv)

# Combine in-sample and out-of-sample returns
pnls <- c(pnlis, pnlos)
pnls <- pnls*sd(indeks)/sd(pnls)
wealthv <- cbind(indeks, pnls)
colnames(wealthv) <- c("Equal Weight", "Optimal")
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv[index(retos)],
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of cumulative portfolio returns
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Out-of-Sample Returns for Stocks with Dimension Reduction") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyEvent(zoo::index(last(retis[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=300)

# Shrink the in-sample returns to their mean
alpha <- 0.7
retxm <- rowMeans(retx["/2014"])
retxis <- (1-alpha)*retx["/2014"] + alpha*retxm
# Calculate portfolio weights
weightv <- invred %*% colMeans(retxis, na.rm=TRUE)
# Calculate in-sample portfolio returns
pnlis <- (retis %*% weightv)
# Scale the weights to volatility target
weightv <- weightv*0.01/sd(pnlis)
# Calculate out-of-sample portfolio returns
pnlos <- (retos %*% weightv)

# Combine in-sample and out-of-sample returns
pnls <- c(pnlis, pnlos)
pnls <- pnls*sd(indeks)/sd(pnls)
wealthv <- cbind(indeks, pnls)
colnames(wealthv) <- c("Equal Weight", "Optimal")
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv[index(retos)],
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of cumulative portfolio returns
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Out-of-Sample Returns for Stocks with Return Shrinkage") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyEvent(zoo::index(last(retis[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=300)

# Overwrite NA values in stock returns
retp <- retstock100
retp[is.na(retp)] <- 0
retx <- retp # Set risk-free rate to zero
nstocks <- NCOL(retp)
datev <- zoo::index(retp)
# Define monthly end points
endd <- rutils::calc_endpoints(retp, interval="months")
endd <- endd[endd > (nstocks+1)]
npts <- NROW(endd) ; look_back <- 12
startp <- c(rep_len(0, look_back), endd[1:(npts-look_back)])
# Perform loop over end points - takes long
pnls <- lapply(1:(npts-1), function(tday) {
    # Subset the excess returns
    retis <- retx[startp[tday]:endd[tday], ]
    invreg <- MASS::ginv(cov(retis, use="pairwise.complete.obs"))
    # Calculate the maximum Sharpe ratio portfolio weights
    weightv <- invreg %*% colMeans(retis, na.rm=TRUE)
    # Zero weights if sparse data
    zerov <- sapply(retis, function(x) (sum(x == 0) > 5))
    weightv[zerov] <- 0
    # Calculate in-sample portfolio returns
    pnlis <- (retis %*% weightv)
    # Scale the weights to volatility target
    weightv <- weightv*0.01/sd(pnlis)
    # Calculate the out-of-sample portfolio returns
    retos <- retp[(endd[tday]+1):endd[tday+1], ]
    xts::xts(retos %*% weightv, zoo::index(retos))
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
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Rolling Portfolio Strategy for S&P500 Stocks") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

library(RcppArmadillo)
# Source Rcpp functions from file
Rcpp::sourceCpp("/Users/jerzy/Develop/lecture_slides/scripts/back_test.cpp")
# Create random matrix of returns
matv <- matrix(rnorm(300), nc=5)
# Reduced inverse of covariance matrix
dimax <- 9
eigend <- eigen(covmat)
invred <- eigend$vectors[, 1:dimax] %*%
  (t(eigend$vectors[, 1:dimax]) / eigend$values[1:dimax])
# Reduced inverse using RcppArmadillo
invarma <- calc_inv(covmat, dimax)
all.equal(invred, invarma)
# Microbenchmark RcppArmadillo code
library(microbenchmark)
summary(microbenchmark(
  rcode={eigend <- eigen(covmat)
    eigend$vectors[, 1:dimax] %*%
(t(eigend$vectors[, 1:dimax]) / eigend$values[1:dimax])
  },
  rcpp=calc_inv(covmat, dimax),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# Shift end points to C++ convention
endd <- (endd - 1)
endd[endd < 0] <- 0
startp <- (startp - 1)
startp[startp < 0] <- 0
# Specify dimension reduction and return shrinkage using list of portfolio optimization parameters
dimax <- 9
alpha <- 0.7
controlv <- HighFreq::param_portf(method="maxsharpe", dimax=dimax, alpha=alpha)
# Perform backtest in Rcpp
pnls <- HighFreq::back_test(retx=retx, retp=retp,
  startp=startp, endd=endd, controlv=controlv)
pnls <- pnls*sd(indeks)/sd(pnls)

# Plot cumulative strategy returns
wealthv <- cbind(indeks, pnls, (pnls+indeks)/2)
colnames(wealthv) <- c("Index", "PortfStrat", "Combined")
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Rolling S&P500 Portfolio Strategy With Shrinkage") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", label="Combined", strokeWidth=3) %>%
  dyLegend(show="always", width=300)

# Perform backtest over vector of dimension reduction parameters
dimaxs <- seq(from=3, to=40, by=2)
pnls <- lapply(dimaxs, function(dimax) {
  controlv <- HighFreq::param_portf(method="maxsharpe",
    dimax=dimax, alpha=alpha)
  HighFreq::back_test(retx=retx, retp=retp,
    startp=startp, endd=endd, controlv=controlv)
})  # end lapply
profilev <- sapply(pnls, sum)
whichmax <- which.max(profilev)
dimax <- dimaxs[whichmax]

plot(x=dimaxs, y=profilev, t="l", xlab="dimax", ylab="pnl",
  main="Rolling Strategy PnL as Function of dimax")

# Perform backtest over vector of return shrinkage intensities
alphav <- seq(from=0.8, to=0.99, by=0.01)
pnls <- lapply(alphav, function(alpha) {
  controlv <- HighFreq::param_portf(method="maxsharpe",
      dimax=dimax, alpha=alpha)
  HighFreq::back_test(retx=retx, retp=retp,
      startp=startp, endd=endd, controlv=controlv)
})  # end lapply
profilev <- sapply(pnls, sum)
whichmax <- which.max(profilev)
alpha <- alphav[whichmax]

plot(x=alphav, y=profilev, t="l",
  main="Rolling Strategy PnL as Function of Return Shrinkage",
  xlab="Shrinkage Intensity Alpha", ylab="pnl")

# Create list of model parameters
controlv <- HighFreq::param_portf(method="maxsharpe",
      dimax=dimax, alpha=alpha)
# Perform backtest over look-backs
look_backs <- seq(from=5, to=16, by=1)
pnls <- lapply(look_backs, function(look_back) {
  startp <- c(rep_len(0, look_back), endd[1:(npts-look_back)])
  startp <- (startp - 1) ; startp[startp < 0] <- 0
  HighFreq::back_test(retx=retx, retp=retp,
    startp=startp, endd=endd, controlv=controlv)
})  # end lapply
profilev <- sapply(pnls, sum)
plot(x=look_backs, y=profilev, t="l", main="Strategy PnL as Function of Look-back Interval",
  xlab="Look-back Interval", ylab="pnl")
whichmax <- which.max(profilev)
look_back <- look_backs[whichmax]
pnls <- pnls[[whichmax]]
pnls <- pnls*sd(indeks)/sd(pnls)

# Calculate the out-of-sample Sharpe and Sortino ratios
wealthv <- cbind(indeks, pnls, (pnls+indeks)/2)
colnames(wealthv) <- c("Index", "PortfStrat", "Combined")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Dygraph the cumulative wealth
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Optimal Rolling S&P500 Portfolio Strategy") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", label="Combined", strokeWidth=3) %>%
  dyLegend(show="always", width=300)
