# Extract the log VTI prices
ohlc <- log(rutils::etfenv$VTI)
closep <- quantmod::Cl(ohlc)
colnames(closep) <- "VTI"
nrows <- NROW(closep)
# Calculate the EWMA weights
look_back <- 111
lambda <- 0.9
weightv <- lambda^(0:look_back)
weightv <- weightv/sum(weightv)
# Calculate the EWMA prices as the convolution
ewmacpp <- HighFreq::roll_sumw(closep, weightv=weightv)
pricev <- cbind(closep, ewmacpp)
colnames(pricev) <- c("VTI", "VTI EWMA")
# Dygraphs plot with custom line colors
colnamev <- colnames(pricev)
dygraphs::dygraph(pricev["2008/2009"], main="VTI EWMA Prices") %>%
  dySeries(name=colnamev[1], strokeWidth=1, col="blue") %>%
  dySeries(name=colnamev[2], strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=200)
# Standard plot of  EWMA prices with custom line colors
x11(width=6, height=5)
plot_theme <- chart_theme()
colorv <- c("blue", "red")
plot_theme$col$line.col <- colorv
quantmod::chart_Series(pricev["2009"], theme=plot_theme,
       lwd=2, name="VTI EWMA Prices")
legend("topleft", legend=colnames(pricev), y.intersp=0.5,
 inset=0.1, bg="white", lty=1, lwd=6, cex=0.8,
 col=plot_theme$col$line.col, bty="n")
# Calculate the EWMA prices recursively using C++ code
ewmar <- .Call(stats:::C_rfilter, closep, lambda, c(as.numeric(closep[1])/(1-lambda), double(NROW(closep))))[-1]
# Or R code
# ewmar <- filter(closep, filter=lambda, init=as.numeric(closep[1, 1])/(1-lambda), method="recursive")
ewmar <- (1-lambda)*ewmar
# Calculate the EWMA prices recursively using RcppArmadillo
ewmacpp <- HighFreq::run_mean(closep, lambda=lambda)
all.equal(drop(ewmacpp), ewmar)
# Compare the speed of C++ code with RcppArmadillo
library(microbenchmark)
summary(microbenchmark(
  Rcpp=HighFreq::run_mean(closep, lambda=lambda),
  rfilter=.Call(stats:::C_rfilter, closep, lambda, c(as.numeric(closep[1])/(1-lambda), double(NROW(closep)))),
  times=10))[, c(1, 4, 5)]
# Dygraphs plot with custom line colors
pricev <- cbind(closep, ewmacpp)
colnames(pricev) <- c("VTI", "VTI EWMA")
colnamev <- colnames(pricev)
dygraphs::dygraph(pricev["2008/2009"], main="Recursive VTI EWMA Prices") %>%
  dySeries(name=colnamev[1], strokeWidth=1, col="blue") %>%
  dySeries(name=colnamev[2], strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=200)
# Standard plot of  EWMA prices with custom line colors
plot_theme <- chart_theme()
colorv <- c("blue", "red")
plot_theme$col$line.col <- colorv
quantmod::chart_Series(pricev["2009"], theme=plot_theme,
       lwd=2, name="VTI EWMA Prices")
legend("topleft", legend=colnames(pricev), y.intersp=0.5,
 inset=0.1, bg="white", lty=1, lwd=6, cex=0.8,
 col=plot_theme$col$line.col, bty="n")
# Calculate the EWMA prices recursively using C++ code
lambda <- 0.984
ewmacpp <- HighFreq::run_mean(closep, lambda=lambda)
# Calculate the positions, either: -1, 0, or 1
indic <- sign(closep - ewmacpp)
posv <- rutils::lagit(indic, lagg=1)
# Create colors for background shading
crossd <- (rutils::diffit(posv) != 0)
shadev <- posv[crossd]
crossd <- c(zoo::index(shadev), end(posv))
shadev <- ifelse(drop(zoo::coredata(shadev)) == 1, "lightgreen", "antiquewhite")
# Create dygraph object without plotting it
dyplot <- dygraphs::dygraph(pricev, main="VTI EWMA Prices") %>%
  dySeries(name=colnamev[1], strokeWidth=1, col="blue") %>%
  dySeries(name=colnamev[2], strokeWidth=3, col="red") %>%
  dyLegend(show="always", width=200)
# Add shading to dygraph object
for (i in 1:NROW(shadev)) {
    dyplot <- dyplot %>% dyShading(from=crossd[i], to=crossd[i+1], color=shadev[i])
}  # end for
# Plot the dygraph object
dyplot
# Standard plot of EWMA prices with position shading
quantmod::chart_Series(pricev, theme=plot_theme,
       lwd=2, name="VTI EWMA Prices")
add_TA(posv > 0, on=-1, col="lightgreen", border="lightgreen")
add_TA(posv < 0, on=-1, col="lightgrey", border="lightgrey")
legend("topleft", legend=colnames(pricev),
 inset=0.1, bg="white", lty=1, lwd=6, y.intersp=0.5,
 col=plot_theme$col$line.col, bty="n")
# Calculate the daily profits and losses of EWMA strategy
retp <- rutils::diffit(closep)  # VTI returns
pnls <- retp*posv
colnames(pnls) <- "EWMA"
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "EWMA PnL")
# Annualized Sharpe ratio of EWMA strategy
sqrt(252)*sapply(wealthv, function (x) mean(x)/sd(x))
# The crossover strategy has a negative correlation to VTI
cor(wealthv)[1, 2]
# Plot dygraph of EWMA strategy wealth
# Create dygraph object without plotting it
colorv <- c("blue", "red")
dyplot <- dygraphs::dygraph(cumsum(wealthv), main="Performance of EWMA Strategy") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=200)
# Add shading to dygraph object
for (i in 1:NROW(shadev)) {
    dyplot <- dyplot %>%
dyShading(from=crossd[i], to=crossd[i+1], color=shadev[i])
}  # end for
# Plot the dygraph object
dyplot
# Standard plot of EWMA strategy wealth
x11(width=6, height=5)
plot_theme <- chart_theme()
plot_theme$col$line.col <- colorv
quantmod::chart_Series(cumsum(wealthv), theme=plot_theme,
       name="Performance of EWMA Strategy")
add_TA(posv > 0, on=-1, col="lightgreen", border="lightgreen")
add_TA(posv < 0, on=-1, col="lightgrey", border="lightgrey")
legend("top", legend=colnames(wealthv), y.intersp=0.5,
 inset=0.05, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")
# Test EWMA crossover market timing of VTI using Treynor-Mazuy test
desv <- cbind(pnls, retp, retp^2)
desv <- na.omit(desv)
colnames(desv) <- c("EWMA", "VTI", "treynor")
regmod <- lm(EWMA ~ VTI + treynor, data=desv)
summary(regmod)
# Plot residual scatterplot
resids <- (desv$EWMA - regmod$coeff["VTI"]*retp)
resids <- regmod$residuals
plot.default(x=retp, y=resids, xlab="VTI", ylab="residuals")
title(main="Treynor-Mazuy Market Timing Test\n for EWMA Crossover vs VTI", line=0.5)
# Plot fitted (predicted) response values
coefreg <- summary(regmod)$coeff
fittedv <- regmod$fitted.values - coefreg["VTI", "Estimate"]*retp
tvalue <- round(coefreg["treynor", "t value"], 2)
points.default(x=retp, y=fittedv, pch=16, col="red")
text(x=0.0, y=0.8*max(resids), paste("Treynor test t-value =", tvalue))
# Determine trade dates right after EWMA has crossed prices
indic <- sign(closep - ewmacpp)
# Calculate the positions from lagged indicator
lagg <- 2
indic <- HighFreq::roll_sum(indic, lagg)
# Calculate the positions, either: -1, 0, or 1
posv <- rep(NA_integer_, nrows)
posv[1] <- 0
posv <- ifelse(indic == lagg, 1, posv)
posv <- ifelse(indic == (-lagg), -1, posv)
posv <- zoo::na.locf(posv, na.rm=FALSE)
posv <- xts::xts(posv, order.by=zoo::index(closep))
# Lag the positions to trade in next period
posv <- rutils::lagit(posv, lagg=1)
# Calculate the PnLs of lagged strategy
pnlslag <- retp*posv
colnames(pnlslag) <- "Lagged Strategy"
wealthv <- cbind(pnls, pnlslag)
colnames(wealthv) <- c("EWMA", "Lagged")
# Annualized Sharpe ratios of EWMA strategies
sharper <- sqrt(252)*sapply(wealthv, function (x) mean(x)/sd(x))
# Plot both strategies
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd], main=paste("EWMA Crossover Strategy", paste(paste(names(sharper), round(sharper, 3), sep="="), collapse=", "))) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=200)
# Calculate the positions, either: -1, 0, or 1
indic <- sign(closep - ewmacpp)
posv <- rutils::lagit(indic, lagg=1)
# Calculate the daily pnl for days without trades
pnls <- retp*posv
# Determine trade dates right after EWMA has crossed prices
crossd <- which(rutils::diffit(posv) != 0)
# Calculate the realized pnl for days with trades
openp <- quantmod::Op(ohlc)
closelag <- rutils::lagit(closep)
poslag <- rutils::lagit(posv)
pnls[crossd] <- poslag[crossd]*(openp[crossd] - closelag[crossd])
# Calculate the unrealized pnl for days with trades
pnls[crossd] <- pnls[crossd] +
  posv[crossd]*(closep[crossd] - openp[crossd])
# Calculate the wealth
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "EWMA PnL")
# Annualized Sharpe ratio of EWMA strategy
sqrt(252)*sapply(wealthv, function (x) mean(x)/sd(x))
# The crossover strategy has a negative correlation to VTI
cor(wealthv)[1, 2]
# Plot dygraph of EWMA strategy wealth
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd], main="EWMA Strategy Trading at the Open Price") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=200)
# Standard plot of EWMA strategy wealth
quantmod::chart_Series(cumsum(wealthv)[endd], theme=plot_theme,
       name="EWMA Strategy Trading at the Open Price")
legend("top", legend=colnames(wealthv),
 inset=0.05, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")
# bid_offer equal to 10 bps for liquid ETFs
bid_offer <- 0.001
# Calculate the transaction costs
costs <- 0.5*bid_offer*abs(poslag - posv)
# Plot strategy with transaction costs
wealthv <- cbind(pnls, pnls - costs)
colnames(wealthv) <- c("EWMA", "EWMA w Costs")
colorv <- c("blue", "red")
dygraphs::dygraph(cumsum(wealthv)[endd], main="EWMA Strategy With Transaction Costs") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=200)
sim_ewma <- function(ohlc, lambda=0.9, look_back=333, bid_offer=0.001,
                trend=1, lagg=1) {
  closep <- quantmod::Cl(ohlc)
  retp <- rutils::diffit(closep)
  nrows <- NROW(ohlc)
  # Calculate the EWMA prices
  ewmacpp <- HighFreq::run_mean(closep, lambda=lambda)
  # Calculate the indicator
  indic <- trend*sign(closep - ewmacpp)
  if (lagg > 1) {
    indic <- HighFreq::roll_sum(indic, lagg)
    indic[1:lagg] <- 0
  }  # end if
  # Calculate the positions, either: -1, 0, or 1
  posv <- rep(NA_integer_, nrows)
  posv[1] <- 0
  posv <- ifelse(indic == lagg, 1, posv)
  posv <- ifelse(indic == (-lagg), -1, posv)
  posv <- zoo::na.locf(posv, na.rm=FALSE)
  posv <- xts::xts(posv, order.by=zoo::index(closep))
  # Lag the positions to trade on next day
  posv <- rutils::lagit(posv, lagg=1)
  # Calculate the PnLs of strategy
  pnls <- retp*posv
  costs <- 0.5*bid_offer*abs(rutils::diffit(posv))
  pnls <- (pnls - costs)
  # Calculate the strategy returns
  pnls <- cbind(posv, pnls)
  colnames(pnls) <- c("positions", "pnls")
  pnls
}  # end sim_ewma
source("/Users/jerzy/Develop/lecture_slides/scripts/ewma_model.R")
lambdav <- seq(from=0.97, to=0.99, by=0.004)
# Perform lapply() loop over lambdav
pnltrend <- lapply(lambdav, function(lambda) {
  # Simulate EWMA strategy and Calculate the returns
  sim_ewma(ohlc=ohlc, lambda=lambda, look_back=look_back, bid_offer=0, lagg=2)[, "pnls"]
})  # end lapply
pnltrend <- do.call(cbind, pnltrend)
colnames(pnltrend) <- paste0("lambda=", lambdav)
# Plot dygraph of multiple EWMA strategies
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnltrend))
endd <- rutils::calc_endpoints(pnltrend, interval="weeks")
dygraphs::dygraph(cumsum(pnltrend)[endd], main="Cumulative Returns of Trend Following EWMA Strategies") %>%
  dyOptions(colors=colorv, strokeWidth=1) %>%
  dyLegend(show="always", width=400)
# Plot EWMA strategies with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- colorv
quantmod::chart_Series(cumsum(pnltrend), theme=plot_theme,
  name="Cumulative Returns of EWMA Strategies")
legend("topleft", legend=colnames(pnltrend), inset=0.1,
  bg="white", cex=0.8, lwd=rep(6, NCOL(pnltrend)),
  col=plot_theme$col$line.col, bty="n")
# Initialize compute cluster under Windows
library(parallel)
ncores <- detectCores() - 1  # Number of cores
cluster <- makeCluster(detectCores()-1)
clusterExport(cluster,
  varlist=c("ohlc", "look_back", "sim_ewma"))
# Perform parallel loop over lambdav under Windows
pnltrend <- parLapply(cluster, lambdav, function(lambda) {
  library(quantmod)
  # Simulate EWMA strategy and Calculate the returns
  sim_ewma(ohlc=ohlc, lambda=lambda, look_back=look_back)[, "pnls"]
})  # end parLapply
stopCluster(cluster)  # Stop R processes over cluster under Windows
# Perform parallel loop over lambdav under Mac-OSX or Linux
pnltrend <- mclapply(lambdav, function(lambda) {
  library(quantmod)
  # Simulate EWMA strategy and Calculate the returns
  sim_ewma(ohlc=ohlc, lambda=lambda, look_back=look_back)[, "pnls"]
}, mc.cores=ncores)  # end mclapply
pnltrend <- do.call(cbind, pnltrend)
colnames(pnltrend) <- paste0("lambda=", lambdav)
# Calculate the annualized Sharpe ratios of strategy returns
sharpetrend <- sqrt(252)*sapply(pnltrend, function(xtsv) {
  mean(xtsv)/sd(xtsv)
})  # end sapply
# Plot Sharpe ratios
dev.new(width=6, height=5, noRStudioGD=TRUE)
plot(x=lambdav, y=sharpetrend, t="l",
     xlab="lambda", ylab="Sharpe",
     main="Performance of EWMA Trend Following Strategies
     as Function of the Decay Parameter Lambda")
# Calculate the optimal lambda
lambda <- lambdav[which.max(sharpetrend)]
# Simulate best performing strategy
ewmatrend <- sim_ewma(ohlc=ohlc, lambda=lambda, bid_offer=0, lagg=2)
posv <- ewmatrend[, "positions"]
trendopt <- ewmatrend[, "pnls"]
wealthv <- cbind(retp, trendopt)
colnames(wealthv) <- c("VTI", "EWMA PnL")
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
cor(wealthv)[1, 2]
# Plot dygraph of EWMA strategy wealth
dygraphs::dygraph(cumsum(wealthv)[endd], main="Performance of Optimal Trend Following EWMA Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=200)
# Plot EWMA PnL with position shading
# Standard plot of EWMA strategy wealth
x11(width=6, height=5)
plot_theme <- chart_theme()
plot_theme$col$line.col <- colorv
quantmod::chart_Series(cumsum(wealthv), theme=plot_theme,
       name="Performance of EWMA Strategy")
add_TA(posv > 0, on=-1, col="lightgreen", border="lightgreen")
add_TA(posv < 0, on=-1, col="lightgrey", border="lightgrey")
legend("top", legend=colnames(wealthv),
 inset=0.05, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")
source("/Users/jerzy/Develop/lecture_slides/scripts/ewma_model.R")
lambdav <- seq(0.6, 0.7, 0.01)
# Perform lapply() loop over lambdav
pnlrevert <- lapply(lambdav, function(lambda) {
  # Simulate EWMA strategy and Calculate the returns
  sim_ewma(ohlc=ohlc, lambda=lambda, bid_offer=0, trend=(-1))[, "pnls"]
})  # end lapply
pnlrevert <- do.call(cbind, pnlrevert)
colnames(pnlrevert) <- paste0("lambda=", lambdav)
# Plot dygraph of mean reverting EWMA strategies
colorv <- colorRampPalette(c("blue", "red"))(NROW(lambdav))
dygraphs::dygraph(cumsum(pnlrevert)[endd], main="Returns of Mean Reverting EWMA Strategies (No Costs)") %>%
  dyOptions(colors=colorv, strokeWidth=1) %>%
  dyLegend(show="always", width=400)
# Plot EWMA strategies with custom line colors
x11(width=6, height=5)
plot_theme <- chart_theme()
plot_theme$col$line.col <- colorv
quantmod::chart_Series(pnlrevert,
  theme=plot_theme, name="Cumulative Returns of Mean Reverting EWMA Strategies")
legend("topleft", legend=colnames(pnlrevert),
  inset=0.1, bg="white", cex=0.8, lwd=6,
  col=plot_theme$col$line.col, bty="n")
# Calculate the Sharpe ratios of strategy returns
sharperevert <- sqrt(252)*sapply(pnlrevert, function(xtsv) {
  mean(xtsv)/sd(xtsv)
})  # end sapply
plot(x=lambdav, y=sharperevert, t="l",
     xlab="lambda", ylab="Sharpe",
     main="Performance of EWMA Mean Reverting Strategies
     as Function of the Decay Parameter Lambda")
# Calculate the optimal lambda
lambda <- lambdav[which.max(sharperevert)]
# Simulate best performing strategy
ewmarevert <- sim_ewma(ohlc=ohlc, bid_offer=0.0,
  lambda=lambda, trend=(-1))
posv <- ewmarevert[, "positions"]
revertopt <- ewmarevert[, "pnls"]
wealthv <- cbind(retp, revertopt)
colnames(wealthv) <- c("VTI", "EWMA PnL")
# Plot dygraph of EWMA strategy wealth
colorv <- c("blue", "red")
dygraphs::dygraph(cumsum(wealthv)[endd], main="Optimal Mean Reverting EWMA Strategy (No Costs)") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=200)
# Standard plot of EWMA strategy wealth
x11(width=6, height=5)
plot_theme <- chart_theme()
plot_theme$col$line.col <- colorv
quantmod::chart_Series(cumsum(wealthv), theme=plot_theme,
       name="Optimal Mean Reverting EWMA Strategy")
add_TA(posv > 0, on=-1, col="lightgreen", border="lightgreen")
add_TA(posv < 0, on=-1, col="lightgrey", border="lightgrey")
legend("top", legend=colnames(wealthv),
 inset=0.05, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")
# Calculate the correlation between trend following and mean reverting strategies
trendopt <- ewmatrend[, "pnls"]
colnames(trendopt) <- "trend"
revertopt <- ewmarevert[, "pnls"]
colnames(revertopt) <- "revert"
cor(cbind(retp, trendopt, revertopt))
# Calculate the combined strategy
combstrat <- (retp + trendopt + revertopt)/3
colnames(combstrat) <- "combined"
# Calculate the annualized Sharpe ratio of strategy returns
retc <- cbind(retp, trendopt, revertopt, combstrat)
colnames(retc) <- c("VTI", "Trending", "Reverting", "Combined")
sqrt(252)*sapply(retc, function(xtsv) mean(xtsv)/sd(xtsv))
# Plot dygraph of EWMA strategy wealth
colorv <- c("blue", "red", "green", "purple")
dygraphs::dygraph(cumsum(retc)[endd], main="Performance of Combined EWMA Strategies") %>%
  dySeries("VTI", color="blue", strokeWidth=1) %>%
  dySeries("Trending", color="red", strokeWidth=1) %>%
  dySeries("Reverting", color="green", strokeWidth=1) %>%
  dySeries("Combined", color="purple", strokeWidth=3) %>%
  dyLegend(show="always", width=400)
# Standard plot of EWMA strategy wealth
plot_theme <- chart_theme()
plot_theme$col$line.col <- colorv
quantmod::chart_Series(pnls, theme=plot_theme,
       name="Performance of Combined EWMA Strategies")
legend("topleft", legend=colnames(pnls),
 inset=0.05, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")
# Calculate the weights proportional to Sharpe ratios
weightv <- c(sharpetrend, sharperevert)
weightv[weightv<0] <- 0
weightv <- weightv/sum(weightv)
retc <- cbind(pnltrend, pnlrevert)
retc <- retc %*% weightv
retc <- cbind(retp, retc)
colnames(retc) <- c("VTI", "EWMA PnL")
# Plot dygraph of EWMA strategy wealth
colorv <- c("blue", "red")
dygraphs::dygraph(cumsum(retc)[endd], main="Performance of Ensemble of EWMA Strategies") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=200)
# Standard plot of EWMA strategy wealth
plot_theme <- chart_theme()
plot_theme$col$line.col <- colorv
quantmod::chart_Series(cumsum(retc), theme=plot_theme,
       name="Performance of Ensemble of EWMA Strategies")
legend("topleft", legend=colnames(pnls),
 inset=0.05, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")
# Calculate the fast and slow EWMAs
lambdaf <- 0.89
lambdas <- 0.95
# Calculate the EWMA prices
ewmaf <- HighFreq::run_mean(closep, lambda=lambdaf)
ewmas <- HighFreq::run_mean(closep, lambda=lambdas)
# Calculate the EWMA prices
pricev <- cbind(closep, ewmaf, ewmas)
colnames(pricev) <- c("VTI", "EWMA fast", "EWMA slow")
# Calculate the positions, either: -1, 0, or 1
indic <- sign(ewmaf - ewmas)
lagg <- 2
indic <- HighFreq::roll_sum(indic, lagg)
posv <- rep(NA_integer_, nrows)
posv[1] <- 0
posv <- ifelse(indic == lagg, 1, posv)
posv <- ifelse(indic == (-lagg), -1, posv)
posv <- zoo::na.locf(posv, na.rm=FALSE)
posv <- xts::xts(posv, order.by=zoo::index(closep))
posv <- rutils::lagit(posv, lagg=1)
# Create colors for background shading
crossd <- (rutils::diffit(posv) != 0)
shadev <- posv[crossd]
crossd <- c(zoo::index(shadev), end(posv))
shadev <- ifelse(drop(zoo::coredata(shadev)) == 1, "lightgreen", "antiquewhite")
# Plot dygraph
colnamev <- colnames(pricev)
dyplot <- dygraphs::dygraph(pricev[endd], main="VTI Dual EWMA Prices") %>%
  dySeries(name=colnamev[1], strokeWidth=1, col="blue") %>%
  dySeries(name=colnamev[2], strokeWidth=2, col="red") %>%
  dySeries(name=colnamev[3], strokeWidth=2, col="purple") %>%
  dyLegend(show="always", width=300)
for (i in 1:NROW(shadev)) {
    dyplot <- dyplot %>% dyShading(from=crossd[i], to=crossd[i+1], color=shadev[i])
}  # end for
dyplot
# Calculate the daily profits and losses of strategy
pnls <- retp*posv
colnames(pnls) <- "Strategy"
wealthv <- cbind(retp, pnls)
# Annualized Sharpe ratio of Dual EWMA strategy
sharper <- sqrt(252)*sapply(wealthv, function (x) mean(x)/sd(x))
# The crossover strategy has a negative correlation to VTI
cor(wealthv)[1, 2]
# Plot Dual EWMA strategy
dyplot <- dygraphs::dygraph(cumsum(wealthv)[endd], main=paste("EWMA Dual Crossover Strategy, Sharpe", paste(paste(names(sharper), round(sharper, 3), sep="="), collapse=", "))) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2)
# Add shading to dygraph object
for (i in 1:NROW(shadev)) {
    dyplot <- dyplot %>% dyShading(from=crossd[i], to=crossd[i+1], color=shadev[i])
}  # end for
# Plot the dygraph object
dyplot
sim_ewma2 <- function(ohlc, lambdaf=0.1, lambdas=0.01, look_back=333,
                bid_offer=0.001, trend=1, lagg=1) {
  if (lambdaf >= lambdas) return(NA)
  closep <- quantmod::Cl(ohlc)
  retp <- rutils::diffit(closep)
  nrows <- NROW(ohlc)
  # Calculate the EWMA prices
  ewmaf <- HighFreq::run_mean(closep, lambda=lambdaf)
  ewmas <- HighFreq::run_mean(closep, lambda=lambdas)
  # Calculate the positions, either: -1, 0, or 1
  indic <- sign(ewmaf - ewmas)
  if (lagg > 1) {
    indic <- HighFreq::roll_sum(indic, lagg)
    indic[1:lagg] <- 0
  }  # end if
  posv <- rep(NA_integer_, nrows)
  posv[1] <- 0
  posv <- ifelse(indic == lagg, 1, posv)
  posv <- ifelse(indic == (-lagg), -1, posv)
  posv <- zoo::na.locf(posv, na.rm=FALSE)
  posv <- xts::xts(posv, order.by=zoo::index(closep))
  # Lag the positions to trade on next day
  posv <- rutils::lagit(posv, lagg=1)
  # Calculate the PnLs of strategy
  pnls <- retp*posv
  costs <- 0.5*bid_offer*abs(rutils::diffit(posv))
  pnls <- (pnls - costs)
  # Calculate the strategy returns
  pnls <- cbind(posv, pnls)
  colnames(pnls) <- c("positions", "pnls")
  pnls
}  # end sim_ewma2
source("/Users/jerzy/Develop/lecture_slides/scripts/ewma_model.R")
lambdafv <- seq(from=0.85, to=0.99, by=0.01)
lambdasv <- seq(from=0.85, to=0.99, by=0.01)
# Calculate the Sharpe ratio of dual EWMA strategy
calc_sharpe <- function(ohlc, lambdaf, lambdas, look_back, bid_offer, trend, lagg) {
  if (lambdaf >= lambdas) return(NA)
  pnls <- sim_ewma2(ohlc=ohlc, lambdaf=lambdaf, lambdas=lambdas, look_back=look_back,
    bid_offer=bid_offer, trend=trend, lagg=lagg)[, "pnls"]
  sqrt(252)*mean(pnls)/sd(pnls)
}  # end calc_sharpe
# Vectorize calc_sharpe with respect to lambdaf and lambdas
calc_sharpe <- Vectorize(FUN=calc_sharpe,
  vectorize.args=c("lambdaf", "lambdas"))
# Calculate the matrix of PnLs
sharpem <- outer(lambdafv, lambdasv, FUN=calc_sharpe, ohlc=ohlc,
           look_back=look_back, bid_offer=0.0, trend=1, lagg=2)
# Or perform two sapply() loops over lambda vectors
sharpem <- sapply(lambdasv, function(lambdas) {
  sapply(lambdafv, function(lambdaf) {
    if (lambdaf >= lambdas) return(NA)
    calc_sharpe(ohlc=ohlc, lambdaf=lambdaf, lambdas=lambdas,
          look_back=look_back, bid_offer=0.0, trend=1, lagg=2)
  })  # end sapply
})  # end sapply
colnames(sharpem) <- lambdasv
rownames(sharpem) <- lambdafv
# Calculate the PnLs for the optimal strategy
whichv <- which(sharpem == max(sharpem, na.rm=TRUE), arr.ind=TRUE)
lambdaf <- lambdafv[whichv[1]]
lambdas <- lambdasv[whichv[2]]
ewma_opt <- sim_ewma2(ohlc=ohlc, lambdaf=lambdaf, lambdas=lambdas,
  look_back=look_back, bid_offer=0.0, trend=1, lagg=2)
pnls <- ewma_opt[, "pnls"]
wealthv <- cbind(retp, pnls)
colnames(wealthv)[2] <- "EWMA"
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Annualized Sharpe ratio of Dual EWMA strategy
sharper <- sqrt(252)*sapply(wealthv, function (x) mean(x)/sd(x))
# The crossover strategy has a negative correlation to VTI
cor(wealthv)[1, 2]
# Create colors for background shading
posv <- ewma_opt[, "positions"]
crossd <- (rutils::diffit(posv) != 0)
shadev <- posv[crossd]
crossd <- c(zoo::index(shadev), end(posv))
shadev <- ifelse(drop(zoo::coredata(shadev)) == 1, "lightgreen", "antiquewhite")
# Plot Optimal Dual EWMA strategy
dyplot <- dygraphs::dygraph(cumsum(wealthv), main=paste("Optimal Dual EWMA Strategy, Sharpe", paste(paste(names(sharper), round(sharper, 3), sep="="), collapse=", "))) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2)
# Add shading to dygraph object
for (i in 1:NROW(shadev)) {
    dyplot <- dyplot %>% dyShading(from=crossd[i], to=crossd[i+1], color=shadev[i])
}  # end for
# Plot the dygraph object
dyplot
# Define in-sample and out-of-sample intervals
insample <- 1:(nrows %/% 2)
outsample <- (nrows %/% 2 + 1):nrows
# Calculate the matrix of PnLs
sharpem <- outer(lambdafv, lambdasv,
           FUN=calc_sharpe, ohlc=ohlc[insample, ],
           look_back=look_back, bid_offer=0.0, trend=1, lagg=2)
colnames(sharpem) <- lambdasv
rownames(sharpem) <- lambdafv
# Calculate the PnLs for the optimal strategy
whichv <- which(sharpem == max(sharpem, na.rm=TRUE), arr.ind=TRUE)
lambdaf <- lambdafv[whichv[1]]
lambdas <- lambdasv[whichv[2]]
pnls <- sim_ewma2(ohlc=ohlc, lambdaf=lambdaf, lambdas=lambdas,
            look_back=look_back, bid_offer=0.0, trend=1, lagg=2)[, "pnls"]
wealthv <- cbind(retp, pnls)
colnames(wealthv)[2] <- "EWMA"
# Calculate the Sharpe and Sortino ratios in-sample and out-of-sample
sqrt(252)*sapply(wealthv[insample, ],
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
sqrt(252)*sapply(wealthv[outsample, ],
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Dygraphs plot with custom line colors
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd], main="Dual EWMA Strategy Out-of-Sample") %>%
  dyEvent(zoo::index(wealthv[last(insample)]), label="in-sample", strokePattern="solid", color="green") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2)
# Calculate the log OHLC prices and volumes
ohlc <- rutils::etfenv$VTI
closep <- log(quantmod::Cl(ohlc))
colnames(closep) <- "VTI"
volum <- quantmod::Vo(ohlc)
colnames(volum) <- "Volume"
nrows <- NROW(closep)
# Calculate the VWAP prices
look_back <- 21
vwap <- HighFreq::roll_sum(closep*volum, look_back)
volumr <- HighFreq::roll_sum(volum, look_back)
vwap <- vwap/volumr
colnames(vwap) <- "VWAP"
pricev <- cbind(closep, vwap)
# Dygraphs plot with custom line colors
colorv <- c("blue", "red")
dygraphs::dygraph(pricev["2009"], main="VTI VWAP Prices") %>%
  dyOptions(colors=colorv, strokeWidth=2)
# Plot VWAP prices with custom line colors
x11(width=6, height=5)
plot_theme <- chart_theme()
plot_theme$col$line.col <- colors
quantmod::chart_Series(pricev["2009"], theme=plot_theme,
       lwd=2, name="VTI VWAP Prices")
legend("bottomright", legend=colnames(pricev),
 inset=0.1, bg="white", lty=1, lwd=6, cex=0.8,
 col=plot_theme$col$line.col, bty="n")
# Calculate the VWAP prices recursively using C++ code
lambda <- 0.995
volumer <- .Call(stats:::C_rfilter, volum, lambda, c(as.numeric(volum[1])/(1-lambda), double(NROW(volum))))[-1]
pricer <- .Call(stats:::C_rfilter, volum*closep, lambda, c(as.numeric(volum[1]*closep[1])/(1-lambda), double(NROW(closep))))[-1]
vwapr <- pricer/volumer
# Calculate the VWAP prices recursively using RcppArmadillo
vwapcpp <- HighFreq::run_mean(closep, lambda=lambda, weightv=volum)
all.equal(vwapr, drop(vwapcpp))
# Dygraphs plot the VWAP prices
pricev <- xts(cbind(vwap, vwapr), zoo::index(ohlc))
colnames(pricev) <- c("VWAP rolling", "VWAP recursive")
dygraphs::dygraph(pricev["2009"], main="VWAP Prices") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Calculate the VWAP prices recursively using RcppArmadillo
lambda <- 0.99
vwapcpp <- HighFreq::run_mean(closep, lambda=lambda, weightv=volum)
# Calculate the positions from lagged indicator
indic <- sign(closep - vwapcpp)
lagg <- 2
indic <- HighFreq::roll_sum(indic, lagg)
# Calculate the positions, either: -1, 0, or 1
posv <- rep(NA_integer_, nrows)
posv[1] <- 0
posv <- ifelse(indic == lagg, 1, posv)
posv <- ifelse(indic == (-lagg), -1, posv)
posv <- zoo::na.locf(posv, na.rm=FALSE)
posv <- xts::xts(posv, order.by=zoo::index(closep))
# Lag the positions to trade in next period
posv <- rutils::lagit(posv, lagg=1)
# Calculate the PnLs of VWAP strategy
retp <- rutils::diffit(closep)  # VTI returns
pnls <- retp*posv
colnames(pnls) <- "VWAP"
wealthv <- cbind(retp, pnls)
colnamev <- colnames(wealthv)
# Annualized Sharpe ratios of VTI and VWAP strategy
sharper <- sqrt(252)*sapply(wealthv, function (x) mean(x)/sd(x))
# Create colors for background shading
crossd <- (rutils::diffit(posv) != 0)
shadev <- posv[crossd]
crossd <- c(zoo::index(shadev), end(posv))
shadev <- ifelse(drop(zoo::coredata(shadev)) == 1, "lightgreen", "antiquewhite")
# Plot dygraph of VWAP strategy
# Create dygraph object without plotting it
dyplot <- dygraphs::dygraph(cumsum(wealthv), main=paste("VWAP Crossover Strategy, Sharpe", paste(paste(names(sharper), round(sharper, 3), sep="="), collapse=", "))) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Add shading to dygraph object
for (i in 1:NROW(shadev)) {
    dyplot <- dyplot %>% dyShading(from=crossd[i], to=crossd[i+1], color=shadev[i])
}  # end for
# Plot the dygraph object
dyplot
# Calculate the correlation of VWAP strategy with VTI
cor(retp, pnls)
# Combine VWAP strategy with VTI
wealthv <- cbind(retp, pnls, 0.5*(retp+pnls))
colnames(wealthv) <- c("VTI", "VWAP", "Combined")
sharper <- sqrt(252)*sapply(wealthv, function (x) mean(x)/sd(x))
# Plot dygraph of VWAP strategy combined with VTI
colnamev <- colnames(wealthv)
dygraphs::dygraph(cumsum(wealthv)[endd], paste("VWAP Strategy Sharpe", paste(paste(names(sharper), round(sharper, 3), sep="="), collapse=", "))) %>%
  dySeries(name=colnamev[1], col="blue", strokeWidth=1) %>%
  dySeries(name=colnamev[2], col="red", strokeWidth=1) %>%
  dySeries(name=colnamev[3], col="purple", strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Or
dygraphs::dygraph(cumsum(wealthv)[endd],
  main=paste("VWAP Strategy Sharpe", paste(paste(names(sharper), round(sharper, 3), sep="="), collapse=", "))) %>%
  dyOptions(colors=c("blue", "red", "purple"), strokeWidth=1) %>%
  dyLegend(show="always", width=300)
# Test VWAP crossover market timing of VTI using Treynor-Mazuy test
desv <- cbind(pnls, retp, retp^2)
desv <- na.omit(desv)
colnames(desv) <- c("VWAP", "VTI", "treynor")
regmod <- lm(VWAP ~ VTI + treynor, data=desv)
summary(regmod)
# Plot residual scatterplot
resids <- (desv$VWAP - regmod$coeff["VTI"]*retp)
resids <- regmod$residuals
# x11(width=6, height=6)
plot.default(x=retp, y=resids, xlab="VTI", ylab="residuals")
title(main="Treynor-Mazuy Market Timing Test\n for VWAP Crossover vs VTI", line=0.5)
# Plot fitted (predicted) response values
coefreg <- summary(regmod)$coeff
fittedv <- regmod$fitted.values - coefreg["VTI", "Estimate"]*retp
tvalue <- round(coefreg["treynor", "t value"], 2)
points.default(x=retp, y=fittedv, pch=16, col="red")
text(x=0.0, y=0.8*max(resids), paste("Treynor test t-value =", tvalue))
sim_vwap <- function(ohlc, lambda=0.9, bid_offer=0.001, trend=1, lagg=1) {
  closep <- log(quantmod::Cl(ohlc))
  volum <- quantmod::Vo(ohlc)
  retp <- rutils::diffit(closep)
  nrows <- NROW(ohlc)
  # Calculate the VWAP prices
  vwap <- HighFreq::run_mean(closep, lambda=lambda, weightv=volum)
  # Calculate the indicator
  indic <- trend*sign(closep - vwap)
  if (lagg > 1) {
    indic <- HighFreq::roll_sum(indic, lagg)
    indic[1:lagg] <- 0
  }  # end if
  # Calculate the positions, either: -1, 0, or 1
  posv <- rep(NA_integer_, nrows)
  posv[1] <- 0
  posv <- ifelse(indic == lagg, 1, posv)
  posv <- ifelse(indic == (-lagg), -1, posv)
  posv <- zoo::na.locf(posv, na.rm=FALSE)
  posv <- xts::xts(posv, order.by=zoo::index(closep))
  # Lag the positions to trade on next day
  posv <- rutils::lagit(posv, lagg=1)
  # Calculate the PnLs of strategy
  pnls <- retp*posv
  costs <- 0.5*bid_offer*abs(rutils::diffit(posv))
  pnls <- (pnls - costs)
  # Calculate the strategy returns
  pnls <- cbind(posv, pnls)
  colnames(pnls) <- c("positions", "pnls")
  pnls
}  # end sim_vwap
source("/Users/jerzy/Develop/lecture_slides/scripts/ewma_model.R")
lambdav <- seq(from=0.97, to=0.995, by=0.004)
# Perform lapply() loop over lambdav
pnls <- lapply(lambdav, function(lambda) {
  # Simulate VWAP strategy and Calculate the returns
  sim_vwap(ohlc=ohlc, lambda=lambda, bid_offer=0, lagg=2)[, "pnls"]
})  # end lapply
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("lambda=", lambdav)
# Plot dygraph of multiple VWAP strategies
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls)[endd], main="Cumulative Returns of Trend Following VWAP Strategies") %>%
  dyOptions(colors=colorv, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Plot VWAP strategies with custom line colors
x11(width=6, height=5)
plot_theme <- chart_theme()
plot_theme$col$line.col <- colorv
quantmod::chart_Series(cumsum(pnls), theme=plot_theme,
  name="Cumulative Returns of VWAP Strategies")
legend("topleft", legend=colnames(pnls), inset=0.1,
  bg="white", cex=0.8, lwd=rep(6, NCOL(pnls)),
  col=plot_theme$col$line.col, bty="n")
# Extract the log VTI prices
pricev <- log(na.omit(rutils::etfenv$prices$VTI))
nrows <- NROW(pricev)
# Calculate the trailing mean prices
lambda <- 0.9
meanv <- HighFreq::run_mean(pricev, lambda=lambda)
# Calculate the trailing volatilities
volat <- HighFreq::run_var(pricev, lambda=lambda)
volat <- sqrt(volat)
# Dygraphs plot of Bollinger bands
priceb <- cbind(pricev, meanv, meanv+volat, meanv-volat)
colnames(priceb)[2:4] <- c("mean", "upper", "lower")
colnamev <- colnames(priceb)
dygraphs::dygraph(priceb["2008-09/2009-09"], main="VTI Prices and Bollinger Bands") %>%
  dySeries(name=colnamev[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], strokeWidth=2, col="green") %>%
  dySeries(name=colnamev[3], strokeWidth=2, strokePattern="dashed", col="red") %>%
  dySeries(name=colnamev[4], strokeWidth=2, strokePattern="dashed", col="red") %>%
  dyLegend(show="always", width=300)
# Calculate the trailing mean prices and volatilities
lambda <- 0.1
meanv <- HighFreq::run_mean(pricev, lambda=lambda)
volat <- HighFreq::run_var(pricev, lambda=lambda)
volat <- sqrt(volat)
# Prepare the simulation parameters
pricen <- as.numeric(pricev) # Numeric price
pricem <- pricen - meanv # De-meaned price
threshv <- volat
posv <- integer(nrows) # Stock positions
posv[1] <- 0 # Initial position
# Calculate the positions from Bollinger bands
for (it in 2:nrows) {
  if (pricem[it-1] > threshv[it-1]) {
    # Enter short
    posv[it] <- (-1)
  } else if (pricem[it-1] < (-threshv[it-1])) {
    # Enter long
    posv[it] <- 1
  } else if ((posv[it-1] < 0) && (pricem[it-1] < 0)) {
    # Unwind short
    posv[it] <- 0
  } else if ((posv[it-1] > 0) && (pricem[it-1] > 0)) {
    # Unwind long
    posv[it] <- 0
  } else {
    # Do nothing
    posv[it] <- posv[it-1]
  }  # end if
}  # end for
# Calculate the number of trades
ntrades <- sum(abs(rutils::diffit(posv)) > 0)
# Calculate the pnls
retv <- rutils::diffit(pricev)
pnls <- retv*posv
# Calculate the Sharpe ratios
wealthv <- cbind(retv, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
sharper <- sqrt(252)*sapply(wealthv, function(x) mean(x)/sd(x[x<0]))
sharper <- round(sharper, 3)
# Dygraphs plot of Bollinger strategy
colnamev <- colnames(wealthv)
captiont <- paste("Bollinger Strategy", "/ \n",
  paste0(paste(colnamev[1:2], "Sharpe =", sharper), collapse=" / "), "/ \n",
  "Number of trades=", ntrades)
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd], main=captiont) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Simulate the modified Bollinger strategy
posv <- integer(nrows) # Stock positions
posv[1] <- 0 # Initial position
for (it in 2:nrows) {
  if (pricem[it-1] > threshv[it-1]) {
    # Enter short
    posv[it] <- (-1)
  } else if (pricem[it-1] < (-threshv[it-1])) {
    # Enter long
    posv[it] <- 1
  } else {
    # Do nothing
    posv[it] <- posv[it-1]
  }  # end if
}  # end for
# Calculate the PnLs
pnls2 <- retv*posv
# Calculate the Sharpe ratios
wealthv <- cbind(pnls, pnls2)
colnames(wealthv) <- c("Bollinger", "Modified")
sharper <- sqrt(252)*sapply(wealthv, function(x) mean(x)/sd(x[x<0]))
sharper <- round(sharper, 3)
# Dygraphs plot of Bollinger strategy
colnamev <- colnames(wealthv)
captiont <- paste("Bollinger Strategy", "/ \n",
  paste0(paste(colnamev[1:2], "Sharpe =", sharper), collapse=" / "), "/ \n",
  "Number of trades=", ntrades)
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd], main=captiont) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Simulate the modified Bollinger strategy quickly
posf <- rep(NA_integer_, nrows)
posf[1] <- 0
posf <- ifelse(pricem > threshv, -1, posf)
posf <- ifelse(pricem < -threshv, 1, posf)
posf <- zoo::na.locf(posf)
# Lag the positions to trade in the next period
posf <- rutils::lagit(posf, lagg=1)
# Compare the positions
all.equal(posv, posf)
# Calculate the trailing mean prices and volatilities of SPY
pricev <- log(quantmod::Cl(HighFreq::SPY))
nrows <- NROW(pricev)
lambda <- 0.1
meanv <- HighFreq::run_mean(pricev, lambda=lambda)
volat <- HighFreq::run_var(pricev, lambda=lambda)
volat <- sqrt(volat)
# Calculate the positions from Bollinger bands
threshv <- volat
pricem <- zoo::coredata(pricev - meanv)
posv <- rep(NA_integer_, nrows)
posv[1] <- 0
posv <- ifelse(pricem > threshv, -1, posv)
posv <- ifelse(pricem < -threshv, 1, posv)
posv <- zoo::na.locf(posv)
# Lag the positions to trade in the next period
posv <- rutils::lagit(posv, lagg=1)
# Calculate the number of trades and the PnLs
ntrades <- sum(abs(rutils::diffit(posv)) > 0)
retv <- rutils::diffit(pricev)
pnls <- retv*posv
# Calculate the Sharpe ratios
wealthv <- cbind(retv, pnls)
colnames(wealthv) <- c("SPY", "Strategy")
nyears <- as.numeric(end(pricev)-start(pricev))/365
sharper <- sqrt(nrows/nyears)*sapply(wealthv, function(x) mean(x)/sd(x[x<0]))
sharper <- round(sharper, 3)
# Dygraphs plot of Bollinger strategy
colnamev <- colnames(wealthv)
captiont <- paste("Bollinger Strategy for Minute SPY", "/ \n",
  paste0(paste(colnamev[1:2], "Sharpe =", sharper), collapse=" / "), "/ \n",
  "Number of trades=", ntrades)
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd], main=captiont) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Extract time series of VTI log prices
pricev <- log(na.omit(rutils::etfenv$prices$VTI))
nrows <- NROW(pricev)
# Define look-back window
look_back <- 11
# Calculate time series of trailing medians
medianv <- HighFreq::roll_mean(pricev, look_back, method="nonparametric")
# medianv <- TTR::runMedian(pricev, n=look_back)
# Calculate time series of MAD
madv <- HighFreq::roll_var(pricev, look_back=look_back, method="nonparametric")
# madv <- TTR::runMAD(pricev, n=look_back)
# Calculate time series of z-scores
zscores <- ifelse(madv > 0, (pricev - medianv)/madv, 0)
zscores[1:look_back, ] <- 0
tail(zscores, look_back)
range(zscores)
# Plot histogram of z-scores
histp <- hist(zscores, col="lightgrey",
  xlab="z-scores", breaks=50, xlim=c(-4, 4),
  ylab="frequency", freq=FALSE, main="Hampel Z-Scores histogram")
lines(density(zscores, adjust=1.5), lwd=3, col="blue")
# Dygraphs plot of Hampel bands
priceb <- cbind(pricev, medianv, medianv+madv, medianv-madv)
colnames(priceb)[2:4] <- c("median", "upper", "lower")
colnamev <- colnames(priceb)
dygraphs::dygraph(priceb["2008-09/2009-09"], main="VTI Prices and Hampel Bands") %>%
  dySeries(name=colnamev[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], strokeWidth=2, col="green") %>%
  dySeries(name=colnamev[3], strokeWidth=2, strokePattern="dashed", col="red") %>%
  dySeries(name=colnamev[4], strokeWidth=2, strokePattern="dashed", col="red") %>%
  dyLegend(show="always", width=300)
# Calculate the time series of trailing medians and MAD
look_back <- 3
medianv <- HighFreq::roll_mean(pricev, look_back, method="nonparametric")
madv <- HighFreq::roll_var(pricev, look_back=look_back, method="nonparametric")
# Calculate the time series of z-scores
zscores <- ifelse(madv > 0, (pricev - medianv)/madv, 0)
zscores[1:look_back, ] <- 0
range(zscores)
# Calculate the positions
threshv <- 1
posv <- rep(NA_integer_, nrows)
posv[1] <- 0
posv[zscores > threshv] <- (-1)
posv[zscores < -threshv] <- 1
posv <- zoo::na.locf(posv)
posv <- rutils::lagit(posv)
# Calculate the number of trades and the PnLs
ntrades <- sum(abs(rutils::diffit(posv)) > 0)
retv <- rutils::diffit(pricev)
pnls <- retv*posv
# Calculate the Sharpe ratios
wealthv <- cbind(retv, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
sharper <- sqrt(252)*sapply(wealthv, function(x) mean(x)/sd(x[x<0]))
sharper <- round(sharper, 3)
# Dygraphs plot of Hampel strategy
captiont <- paste("Hampel Strategy", "/ \n",
  paste0(paste(colnamev[1:2], "Sharpe =", sharper), collapse=" / "), "/ \n",
  "Number of trades=", ntrades)
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
colnamev <- colnames(wealthv)
dygraphs::dygraph(cumsum(wealthv)[endd], main=captiont) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Calculate the trailing mean prices and volatilities of SPY
pricev <- log(quantmod::Cl(HighFreq::SPY))
nrows <- NROW(pricev)
# Calculate the price medians and MAD
look_back <- 3
medianv <- HighFreq::roll_mean(pricev, look_back, method="nonparametric")
madv <- HighFreq::roll_var(pricev, look_back=look_back, method="nonparametric")
# Calculate the time series of z-scores
zscores <- ifelse(madv > 0, (pricev - medianv)/madv, 0)
zscores[1:look_back, ] <- 0
# Calculate the positions
threshv <- 1
posv <- rep(NA_integer_, nrows)
posv[1] <- 0
posv[zscores < -threshv] <- 1
posv[zscores > threshv] <- (-1)
posv <- zoo::na.locf(posv)
posv <- rutils::lagit(posv)
# Calculate the number of trades and the PnLs
ntrades <- sum(abs(rutils::diffit(posv)) > 0)
retv <- rutils::diffit(pricev)
pnls <- retv*posv
# Calculate the Sharpe ratios
wealthv <- cbind(retv, pnls)
colnames(wealthv) <- c("SPY", "Strategy")
nyears <- as.numeric(end(pricev)-start(pricev))/365
sharper <- sqrt(nrows/nyears)*sapply(wealthv, function(x) mean(x)/sd(x[x<0]))
sharper <- round(sharper, 3)
# Dygraphs plot of Hampel strategy
colnamev <- colnames(wealthv)
captiont <- paste("Hampel Strategy for Minute SPY", "/ \n",
  paste0(paste(colnamev[1:2], "Sharpe =", sharper), collapse=" / "), "/ \n",
  "Number of trades=", ntrades)
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd], main=captiont) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
