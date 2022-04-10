# Extract log VTI prices
ohlc <- rutils::etfenv$VTI
closep <- log(quantmod::Cl(ohlc))
colnames(closep) <- "VTI"
nrows <- NROW(closep)
# Calculate EWMA weights
look_back <- 333
lambda <- 0.004
weightv <- exp(-lambda*(1:look_back))
weightv <- weightv/sum(weightv)
# Calculate EWMA prices
ewmap <- HighFreq::roll_wsum(closep, weights=weightv)
# Copy over NA values
ewmap <- zoo::na.locf(ewmap, fromLast=TRUE)
prices <- cbind(closep, ewmap)
colnames(prices) <- c("VTI", "VTI EWMA")
# Dygraphs plot with custom line colors
colnames <- colnames(prices)
dygraphs::dygraph(prices["2007/"], main="VTI EWMA Prices") %>%
  dySeries(name=colnames[1], label=colnames[1], strokeWidth=1, col="blue") %>%
  dySeries(name=colnames[2], label=colnames[2], strokeWidth=4, col="red") %>%
  dyLegend(show="always", width=500)
# Standard plot of  EWMA prices with custom line colors
x11(width=6, height=5)
plot_theme <- chart_theme()
colors <- c("blue", "red")
plot_theme$col$line.col <- colors
quantmod::chart_Series(prices["2007/"], theme=plot_theme,
       lwd=2, name="VTI EWMA Prices")
legend("topleft", legend=colnames(prices),
 inset=0.1, bg="white", lty=1, lwd=6, cex=0.8,
 col=plot_theme$col$line.col, bty="n")
# Calculate positions, either: -1, 0, or 1
position_s <- sign(closep - ewmap)
position_s <- xts::xts(position_s, order.by=index(closep))
position_s <- rutils::lagit(position_s, lagg=1)
# Create colors for background shading
dates <- (rutils::diffit(position_s) != 0)
shad_e <- position_s[dates]
dates <- c(index(shad_e), end(position_s))
shad_e <- ifelse(drop(zoo::coredata(shad_e)) == 1, "lightgreen", "antiquewhite")
# Create dygraph object without plotting it
dyplot <- dygraphs::dygraph(prices["2007/"], main="VTI EWMA Prices") %>%
  dySeries(name=colnames[1], label=colnames[1], strokeWidth=1, col="blue") %>%
  dySeries(name=colnames[2], label=colnames[2], strokeWidth=4, col="red") %>%
  dyLegend(show="always", width=500)
# Add shading to dygraph object
for (i in 1:NROW(shad_e)) {
    dyplot <- dyplot %>% dyShading(from=dates[i], to=dates[i+1], color=shad_e[i])
}  # end for
# Plot the dygraph object
dyplot
# Equivalent code to the above
# Determine trade dates right after EWMA has crossed prices
indic <- sign(closep - ewmap)
trade_dates <- (rutils::diffit(indic) != 0)
trade_dates <- which(trade_dates) + 1
trade_dates <- trade_dates[trade_dates < nrows]
# Calculate positions, either: -1, 0, or 1
position_s <- rep(NA_integer_, nrows)
position_s[1] <- 0
position_s[trade_dates] <- indic[trade_dates-1]
position_s <- zoo::na.locf(position_s, na.rm=FALSE)
position_s <- xts::xts(position_s, order.by=index(closep))
# Create indicator for background shading
shad_e <- position_s[trade_dates]
dates <- index(shad_e)
dates <- c(dates, end(position_s))
shad_e <- ifelse(drop(zoo::coredata(shad_e)) == 1, "lightgreen", "antiquewhite")
# Standard plot of EWMA prices with position shading
x11(width=6, height=5)
quantmod::chart_Series(prices["2007/"], theme=plot_theme,
       lwd=2, name="VTI EWMA Prices")
add_TA(position_s > 0, on=-1, col="lightgreen", border="lightgreen")
add_TA(position_s < 0, on=-1, col="lightgrey", border="lightgrey")
legend("topleft", legend=colnames(prices),
 inset=0.1, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")
# Calculate daily profits and losses of EWMA strategy
vti <- rutils::diffit(closep)
colnames(vti) <- "VTI"
pnls <- vti*position_s
colnames(pnls) <- "EWMA"
wealth <- cbind(vti, pnls)
colnames(wealth) <- c("VTI", "EWMA PnL")
# Annualized Sharpe ratio of EWMA strategy
sqrt(252)*sapply(wealth, function (x) mean(x)/sd(x))
# The crossover strategy has a negative correlation to VTI
cor(wealth)
# Plot dygraph of EWMA strategy wealth
# Create dygraph object without plotting it
colors <- c("blue", "red")
dyplot <- dygraphs::dygraph(cumsum(wealth["2007/"]), main="Performance of EWMA Strategy") %>%
  dyOptions(colors=colors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Add shading to dygraph object
for (i in 1:NROW(shad_e)) {
    dyplot <- dyplot %>%
dyShading(from=dates[i], to=dates[i+1], color=shad_e[i])
}  # end for
# Plot the dygraph object
dyplot
# Standard plot of EWMA strategy wealth
x11(width=6, height=5)
plot_theme <- chart_theme()
plot_theme$col$line.col <- colors
quantmod::chart_Series(cumsum(wealth["2007/"]), theme=plot_theme,
       name="Performance of EWMA Strategy")
add_TA(position_s > 0, on=-1, col="lightgreen", border="lightgreen")
add_TA(position_s < 0, on=-1, col="lightgrey", border="lightgrey")
legend("top", legend=colnames(wealth),
 inset=0.05, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")
# Test EWMA crossover market timing of VTI using Treynor-Mazuy test
design <- cbind(pnls, vti, vti^2)
design <- na.omit(design)
colnames(design) <- c("EWMA","VTI","treynor")
model <- lm(EWMA ~ VTI + treynor, data=design)
summary(model)
# Plot residual scatterplot
residuals <- (design$EWMA - model$coeff[2]*design$VTI)
residuals <- model$residuals
x11(width=6, height=6)
plot.default(x=design$VTI, y=residuals, xlab="VTI", ylab="residuals")
title(main="Treynor-Mazuy Market Timing Test\n for EWMA Crossover vs VTI", line=0.5)
# Plot fitted (predicted) response values
fit_ted <- (model$coeff["(Intercept)"] +
        model$coeff["treynor"]*vti^2)
points.default(x=design$VTI, y=fit_ted, pch=16, col="red")
text(x=0.05, y=0.8*max(residuals), paste("EWMA crossover t-value =", round(summary(model)$coeff["treynor", "t value"], 2)))
# Determine trade dates right after EWMA has crossed prices
indic <- sign(closep - ewmap)
# Calculate positions from lagged indicator
lagg <- 2
indic <- roll::roll_sum(indic, width=lagg, min_obs=1)
# Calculate positions, either: -1, 0, or 1
position_s <- rep(NA_integer_, nrows)
position_s[1] <- 0
position_s <- ifelse(indic == lagg, 1, position_s)
position_s <- ifelse(indic == (-lagg), -1, position_s)
position_s <- zoo::na.locf(position_s, na.rm=FALSE)
position_s <- xts::xts(position_s, order.by=index(closep))
# Lag the positions to trade in next period
position_s <- rutils::lagit(position_s, lagg=1)
# Calculate PnLs of lagged strategy
pnls <- vti*position_s
colnames(pnls) <- "Lagged Strategy"
wealth <- cbind(wealth[, 2], pnls)
colnames(wealth) <- c("EWMA Strategy", "Lagged Strategy")
# Annualized Sharpe ratios of EWMA strategies
sharp_e <- sqrt(252)*sapply(wealth, function (x) mean(x)/sd(x))
# Plot both strategies
dygraphs::dygraph(cumsum(wealth["2007/"]), main=paste("EWMA Crossover Strategy, Sharpe", paste(paste(names(sharp_e), round(sharp_e, 3), sep="="), collapse=", "))) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Calculate daily profits and losses
pnls <- vti*position_s
# Calculate realized pnl for days with trade
openp <- log(quantmod::Op(ohlc))
close_lag <- rutils::lagit(closep)
pos_lagged <- rutils::lagit(position_s)
pnls[trade_dates] <- pos_lagged[trade_dates]*
  (openp[trade_dates] - close_lag[trade_dates])
# Calculate unrealized pnl for days with trade
pnls[trade_dates] <- pnls[trade_dates] +
  position_s[trade_dates]*
  (closep[trade_dates] - openp[trade_dates])
wealth <- cbind(vti, pnls)
colnames(wealth) <- c("VTI", "EWMA PnL")
# Annualized Sharpe ratio of EWMA strategy
sqrt(252)*sapply(wealth, function (x) mean(x)/sd(x))
# The crossover strategy has a negative correlation to VTI
cor(wealth)
# Plot dygraph of EWMA strategy wealth
dygraphs::dygraph(cumsum(wealth["2007/"]), main="EWMA Strategy Trading at the Open Price") %>%
  dyOptions(colors=colors, strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Standard plot of EWMA strategy wealth
quantmod::chart_Series(wealth, theme=plot_theme,
       name="EWMA Strategy Trading at the Open Price")
legend("top", legend=colnames(wealth),
 inset=0.05, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")
# bid_offer equal to 10 bps for liquid ETFs
bid_offer <- 0.001
# Calculate transaction costs
costs <- 0.5*bid_offer*abs(pos_lagged - position_s)*closep
# Plot strategy with transaction costs
wealth <- cbind(pnls, pnls - costs)
colnames(wealth) <- c("EWMA", "EWMA w Costs")
colors <- c("blue", "red")
dygraphs::dygraph(cumsum(wealth["2007/"]), main="EWMA Strategy With Transaction Costs") %>%
  dyOptions(colors=colors, strokeWidth=2) %>%
  dyLegend(show="always", width=500)
simu_ewma <- function(ohlc, lambda=0.01, look_back=333, bid_offer=0.001,
                trend=1, lagg=1) {
  close <- log(quantmod::Cl(ohlc))
  returns <- rutils::diffit(close)
  nrows <- NROW(ohlc)
  # Calculate EWMA prices
  weights <- exp(-lambda*(1:look_back))
  weights <- weights/sum(weights)
  ewma <- HighFreq::roll_wsum(close, weights=weights)
  # Calculate the indicator
  indic <- trend*sign(close - ewma)
  if (lagg > 1) {
    indic <- roll::roll_sum(indic, width=lagg, min_obs=1)
    indic[1:lagg] <- 0
  }  # end if
  # Calculate positions, either: -1, 0, or 1
  pos <- rep(NA_integer_, nrows)
  pos[1] <- 0
  pos <- ifelse(indic == lagg, 1, pos)
  pos <- ifelse(indic == (-lagg), -1, pos)
  pos <- zoo::na.locf(pos, na.rm=FALSE)
  pos <- xts::xts(pos, order.by=index(close))
  # Lag the positions to trade on next day
  pos <- rutils::lagit(pos, lagg=1)
  # Calculate PnLs of strategy
  pnls <- returns*pos
  costs <- 0.5*bid_offer*abs(rutils::diffit(pos))*close
  pnls <- (pnls - costs)
  # Calculate strategy returns
  pnls <- cbind(pos, pnls)
  colnames(pnls) <- c("positions", "pnls")
  pnls
}  # end simu_ewma
source("/Users/jerzy/Develop/lecture_slides/scripts/ewma_model.R")
lambdas <- seq(from=0.001, to=0.008, by=0.001)
# Perform lapply() loop over lambdas
pnls <- lapply(lambdas, function(lambda) {
  # Simulate EWMA strategy and calculate returns
  simu_ewma(ohlc=ohlc, lambda=lambda, look_back=look_back, bid_offer=0, lagg=2)[, "pnls"]
})  # end lapply
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("lambda=", lambdas)
# Plot dygraph of multiple EWMA strategies
colors <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls["2007/"]), main="Cumulative Returns of Trend Following EWMA Strategies") %>%
  dyOptions(colors=colors, strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Plot EWMA strategies with custom line colors
x11(width=6, height=5)
plot_theme <- chart_theme()
plot_theme$col$line.col <- colors
quantmod::chart_Series(cumsum(pnls), theme=plot_theme,
  name="Cumulative Returns of EWMA Strategies")
legend("topleft", legend=colnames(pnls), inset=0.1,
  bg="white", cex=0.8, lwd=rep(6, NCOL(pnls)),
  col=plot_theme$col$line.col, bty="n")
# Initialize compute cluster under Windows
library(parallel)
cluster <- makeCluster(detectCores()-1)
clusterExport(cluster,
  varlist=c("ohlc", "look_back", "simu_ewma"))
# Perform parallel loop over lambdas under Windows
pnls <- parLapply(cluster, lambdas, function(lambda) {
  library(quantmod)
  # Simulate EWMA strategy and calculate returns
  simu_ewma(ohlc=ohlc, lambda=lambda, look_back=look_back)[, "pnls"]
})  # end parLapply
stopCluster(cluster)  # Stop R processes over cluster under Windows
# Perform parallel loop over lambdas under Mac-OSX or Linux
pnls <- mclapply(lambdas, function(lambda) {
  library(quantmod)
  # Simulate EWMA strategy and calculate returns
  simu_ewma(ohlc=ohlc, lambda=lambda, look_back=look_back)[, "pnls"]
})  # end mclapply
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("lambda=", lambdas)
# Calculate annualized Sharpe ratios of strategy returns
sharper <- sqrt(252)*sapply(pnls, function(xtes) {
  mean(xtes)/sd(xtes)
})  # end sapply
# Plot Sharpe ratios
dev.new(width=6, height=5, noRStudioGD=TRUE)
plot(x=lambdas, y=sharper, t="l",
     xlab="lambda", ylab="Sharpe",
     main="Performance of EWMA Trend Following Strategies
     as Function of the Decay Parameter Lambda")
# Find optimal lambda
lambda <- lambdas[which.max(sharper)]
# Plot optimal weights
weightv <- exp(-lambda*(1:look_back))
weightv <- weightv/sum(weightv)
plot(weightv, t="l", xlab="days", ylab="weights",
     main="Optimal Weights of EWMA Trend Following Strategy")
trend_returns <- pnls
trend_sharpe <- sharper
# Simulate best performing strategy
ewma_trend <- simu_ewma(ohlc=ohlc, lambda=lambda, look_back=look_back, bid_offer=0, lagg=2)
position_s <- ewma_trend[, "positions"]
pnls <- ewma_trend[, "pnls"]
wealth <- cbind(vti, pnls)
colnames(wealth) <- c("VTI", "EWMA PnL")
# Create colors for background shading
dates <- (rutils::diffit(position_s) != 0)
shad_e <- position_s[dates]
dates <- c(index(shad_e), end(position_s))
shad_e <- ifelse(drop(zoo::coredata(shad_e)) == 1, "lightgreen", "antiquewhite")
colors <- c("blue", "red")
# Plot dygraph of EWMA strategy wealth
# Create dygraph object without plotting it
dyplot <- dygraphs::dygraph(cumsum(wealth["2007/"]), main="Performance of Optimal Trend Following EWMA Strategy") %>%
  dyOptions(colors=colors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Add shading to dygraph object
for (i in 1:NROW(shad_e)) {
    dyplot <- dyplot %>%
dyShading(from=dates[i], to=dates[i+1], color=shad_e[i])
}  # end for
# Plot the dygraph object
dyplot
# Plot EWMA PnL with position shading
# Standard plot of EWMA strategy wealth
x11(width=6, height=5)
plot_theme <- chart_theme()
plot_theme$col$line.col <- colors
quantmod::chart_Series(cumsum(wealth["2007/"]), theme=plot_theme,
       name="Performance of EWMA Strategy")
add_TA(position_s > 0, on=-1, col="lightgreen", border="lightgreen")
add_TA(position_s < 0, on=-1, col="lightgrey", border="lightgrey")
legend("top", legend=colnames(wealth),
 inset=0.05, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")
source("/Users/jerzy/Develop/lecture_slides/scripts/ewma_model.R")
lambdas <- seq(0.05, 1.0, 0.05)
# Perform lapply() loop over lambdas
pnls <- lapply(lambdas, function(lambda) {
  # Simulate EWMA strategy and calculate returns
  simu_ewma(ohlc=ohlc, lambda=lambda, look_back=look_back, trend=(-1))[, "pnls"]
})  # end lapply
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("lambda=", lambdas)
# Plot dygraph of mean reverting EWMA strategies
column_s <- seq(1, NCOL(pnls), by=4)
colors <- colorRampPalette(c("blue", "red"))(NROW(column_s))
dygraphs::dygraph(cumsum(pnls["2007/", column_s]), main="Cumulative Returns of Mean Reverting EWMA Strategies") %>%
  dyOptions(colors=colors, strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Plot EWMA strategies with custom line colors
x11(width=6, height=5)
plot_theme <- chart_theme()
plot_theme$col$line.col <- colors
quantmod::chart_Series(pnls[, column_s],
  theme=plot_theme, name="Cumulative Returns of Mean Reverting EWMA Strategies")
legend("topleft", legend=colnames(pnls[, column_s]),
  inset=0.1, bg="white", cex=0.8, lwd=6,
  col=plot_theme$col$line.col, bty="n")
# Calculate Sharpe ratios of strategy returns
sharper <- sqrt(252)*sapply(pnls, function(xtes) {
  mean(xtes)/sd(xtes)
})  # end sapply
plot(x=lambdas, y=sharper, t="l",
     xlab="lambda", ylab="Sharpe",
     main="Performance of EWMA Mean Reverting Strategies
     as Function of the Decay Parameter Lambda")
revert_returns <- pnls
revert_sharpe <- sharper
# Find optimal lambda
lambda <- lambdas[which.max(sharper)]
# Simulate best performing strategy
ewma_revert <- simu_ewma(ohlc=ohlc, bid_offer=0.0,
  lambda=lambda, look_back=look_back, trend=(-1))
position_s <- ewma_revert[, "positions"]
pnls <- ewma_revert[, "pnls"]
wealth <- cbind(vti, pnls)
colnames(wealth) <- c("VTI", "EWMA PnL")
# Plot dygraph of EWMA strategy wealth
colors <- c("blue", "red")
dygraphs::dygraph(cumsum(wealth["2007/"]), main="Optimal Mean Reverting EWMA Strategy") %>%
  dyOptions(colors=colors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Plot EWMA PnL with position shading
# Standard plot of EWMA strategy wealth
x11(width=6, height=5)
plot_theme <- chart_theme()
plot_theme$col$line.col <- colors
quantmod::chart_Series(cumsum(wealth["2007/"]), theme=plot_theme,
       name="Optimal Mean Reverting EWMA Strategy")
add_TA(position_s > 0, on=-1, col="lightgreen", border="lightgreen")
add_TA(position_s < 0, on=-1, col="lightgrey", border="lightgrey")
legend("top", legend=colnames(wealth),
 inset=0.05, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")
# Calculate correlation between trend following and mean reverting strategies
trend_ing <- ewma_trend[, "pnls"]
colnames(trend_ing) <- "trend"
revert_ing <- ewma_revert[, "pnls"]
colnames(revert_ing) <- "revert"
cor(cbind(vti, trend_ing, revert_ing))
# Calculate combined strategy
com_bined <- (vti + trend_ing + revert_ing)/3
colnames(com_bined) <- "combined"
# Calculate annualized Sharpe ratio of strategy returns
returns <- cbind(vti, trend_ing, revert_ing, com_bined)
colnames(returns) <- c("VTI", "Trending", "Reverting", "EWMA combined")
sqrt(252)*sapply(returns, function(xtes) mean(xtes)/sd(xtes))
# Plot dygraph of EWMA strategy wealth
colors <- c("blue", "red", "green", "purple")
dygraphs::dygraph(cumsum(returns["2007/"]), main="Performance of Combined EWMA Strategies") %>%
  dyOptions(colors=colors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Standard plot of EWMA strategy wealth
plot_theme <- chart_theme()
plot_theme$col$line.col <- colors
quantmod::chart_Series(pnls, theme=plot_theme,
       name="Performance of Combined EWMA Strategies")
legend("topleft", legend=colnames(pnls),
 inset=0.05, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")
weightv <- c(trend_sharpe, revert_sharpe)
weightv[weightv<0] <- 0
weightv <- weightv/sum(weightv)
returns <- cbind(trend_returns, revert_returns)
returns <- returns %*% weightv
returns <- xts::xts(returns, order.by=index(vti))
returns <- cbind(vti, returns)
colnames(returns) <- c("VTI", "EWMA PnL")
# Plot dygraph of EWMA strategy wealth
colors <- c("blue", "red")
dygraphs::dygraph(cumsum(returns["2007/"]), main="Performance of Ensemble of EWMA Strategies") %>%
  dyOptions(colors=colors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Standard plot of EWMA strategy wealth
plot_theme <- chart_theme()
plot_theme$col$line.col <- colors
quantmod::chart_Series(cumsum(returns["2007/"]), theme=plot_theme,
       name="Performance of Ensemble of EWMA Strategies")
legend("topleft", legend=colnames(pnls),
 inset=0.05, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")
# Calculate fast and slow EWMAs
look_back <- 333
lambda1 <- 0.04
lambda2 <- 0.004
weightv <- exp(-lambda1*(1:look_back))
weightv <- weightv/sum(weightv)
ewma1 <- HighFreq::roll_wsum(closep, weights=weightv)
weightv <- exp(-lambda2*(1:look_back))
weightv <- weightv/sum(weightv)
ewma2 <- HighFreq::roll_wsum(closep, weights=weightv)
# Calculate EWMA prices
prices <- cbind(closep, ewma1, ewma2)
colnames(prices) <- c("VTI", "EWMA fast", "EWMA slow")
# Calculate positions, either: -1, 0, or 1
indic <- sign(ewma1 - ewma2)
lagg <- 2
indic <- roll::roll_sum(indic, width=lagg, min_obs=1)
position_s <- rep(NA_integer_, nrows)
position_s[1] <- 0
position_s <- ifelse(indic == lagg, 1, position_s)
position_s <- ifelse(indic == (-lagg), -1, position_s)
position_s <- zoo::na.locf(position_s, na.rm=FALSE)
position_s <- xts::xts(position_s, order.by=index(closep))
position_s <- rutils::lagit(position_s, lagg=1)
# Create colors for background shading
dates <- (rutils::diffit(position_s) != 0)
shad_e <- position_s[dates]
dates <- c(index(shad_e), end(position_s))
shad_e <- ifelse(drop(zoo::coredata(shad_e)) == 1, "lightgreen", "antiquewhite")
# Plot dygraph
colnames <- colnames(prices)
dyplot <- dygraphs::dygraph(prices["2007/"], main="VTI Dual EWMA Prices") %>%
  dySeries(name=colnames[1], label=colnames[1], strokeWidth=1, col="blue") %>%
  dySeries(name=colnames[2], label=colnames[2], strokeWidth=4, col="red") %>%
  dySeries(name=colnames[3], label=colnames[3], strokeWidth=4, col="purple") %>%
  dyLegend(show="always", width=500)
for (i in 1:NROW(shad_e)) {
    dyplot <- dyplot %>% dyShading(from=dates[i], to=dates[i+1], color=shad_e[i])
}  # end for
dyplot
# Calculate daily profits and losses of strategy
pnls <- vti*position_s
colnames(pnls) <- "Strategy"
wealth <- cbind(vti, pnls)
# Annualized Sharpe ratio of Dual EWMA strategy
sharp_e <- sqrt(252)*sapply(wealth, function (x) mean(x)/sd(x))
# The crossover strategy has a negative correlation to VTI
cor(wealth)
# Plot Dual EWMA strategy
dyplot <- dygraphs::dygraph(cumsum(wealth["2007/"]), main=paste("EWMA Dual Crossover Strategy, Sharpe", paste(paste(names(sharp_e), round(sharp_e, 3), sep="="), collapse=", "))) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=1)
# Add shading to dygraph object
for (i in 1:NROW(shad_e)) {
    dyplot <- dyplot %>% dyShading(from=dates[i], to=dates[i+1], color=shad_e[i])
}  # end for
# Plot the dygraph object
dyplot
simu_ewma2 <- function(ohlc, lambda1=0.1, lambda2=0.01, look_back=333,
                bid_offer=0.001, trend=1, lagg=1) {
  close <- log(quantmod::Cl(ohlc))
  returns <- rutils::diffit(close)
  nrows <- NROW(ohlc)
  # Calculate EWMA prices
  weights <- exp(-lambda1*(1:look_back))
  weights <- weights/sum(weights)
  ewma1 <- HighFreq::roll_wsum(closep, weights=weights)
  weights <- exp(-lambda2*(1:look_back))
  weights <- weights/sum(weights)
  ewma2 <- HighFreq::roll_wsum(closep, weights=weights)
  # Calculate positions, either: -1, 0, or 1
  indic <- sign(ewma1 - ewma2)
  if (lagg > 1) {
    indic <- roll::roll_sum(indic, width=lagg, min_obs=1)
    indic[1:lagg] <- 0
  }  # end if
  pos <- rep(NA_integer_, nrows)
  pos[1] <- 0
  pos <- ifelse(indic == lagg, 1, pos)
  pos <- ifelse(indic == (-lagg), -1, pos)
  pos <- zoo::na.locf(pos, na.rm=FALSE)
  pos <- xts::xts(pos, order.by=index(close))
  # Lag the positions to trade on next day
  pos <- rutils::lagit(pos, lagg=1)
  # Calculate PnLs of strategy
  pnls <- returns*pos
  costs <- 0.5*bid_offer*abs(rutils::diffit(pos))*close
  pnls <- (pnls - costs)
  # Calculate strategy returns
  pnls <- cbind(pos, pnls)
  colnames(pnls) <- c("positions", "pnls")
  pnls
}  # end simu_ewma2
source("/Users/jerzy/Develop/lecture_slides/scripts/ewma_model.R")
lambdas1 <- seq(from=0.05, to=0.15, by=0.01)
lambdas2 <- seq(from=0.03, to=0.1, by=0.01)
# Perform sapply() loops over lambdas
sharper <- sapply(lambdas1, function(lambda1) {
  sapply(lambdas2, function(lambda2) {
    if (lambda1 > lambda2) {
# Simulate Dual EWMA strategy
pnls <- simu_ewma2(ohlc=ohlc, lambda1=lambda1, lambda2=lambda2,
                    look_back=look_back, bid_offer=0.0, trend=1, lagg=2)[, "pnls"]
sqrt(252)*mean(pnls)/sd(pnls)
    } else NA
  })  # end sapply
})  # end sapply
colnames(sharper) <- lambdas1
rownames(sharper) <- lambdas2
# Calculate the PnLs for the optimal strategy
whichv <- which(sharper == max(sharper, na.rm=TRUE), arr.ind=TRUE)
lambda1 <- lambdas1[whichv[2]]
lambda2 <- lambdas2[whichv[1]]
pnls <- simu_ewma2(ohlc=ohlc, lambda1=lambda1, lambda2=lambda2,
              look_back=look_back, bid_offer=0.0, trend=1, lagg=2)[, "pnls"]
wealth <- cbind(vti, pnls)
# Annualized Sharpe ratio of Dual EWMA strategy
sharp_e <- sqrt(252)*sapply(wealth, function (x) mean(x)/sd(x))
# The crossover strategy has a negative correlation to VTI
cor(wealth)
# Plot Optimal Dual EWMA strategy
dyplot <- dygraphs::dygraph(cumsum(wealth["2007/"]), main=paste("Optimal EWMA Dual Crossover Strategy, Sharpe", paste(paste(names(sharp_e), round(sharp_e, 3), sep="="), collapse=", "))) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=1)
# Add shading to dygraph object
for (i in 1:NROW(shad_e)) {
    dyplot <- dyplot %>% dyShading(from=dates[i], to=dates[i+1], color=shad_e[i])
}  # end for
# Plot the dygraph object
dyplot
# Calculate log OHLC prices and volumes
ohlc <- rutils::etfenv$VTI
closep <- log(quantmod::Cl(ohlc))
colnames(closep) <- "VTI"
volumes <- quantmod::Vo(ohlc)
colnames(volumes) <- "Volume"
nrows <- NROW(closep)
# Calculate the VWAP prices
look_back <- 170
vwap <- roll::roll_sum(closep*volumes, width=look_back, min_obs=1)
volume_roll <- roll::roll_sum(volumes, width=look_back, min_obs=1)
vwap <- vwap/volume_roll
colnames(vwap) <- "VWAP"
prices <- cbind(closep, vwap)
# Dygraphs plot with custom line colors
colnames <- colnames(prices)
dygraphs::dygraph(prices["2007/"], main="VTI VWAP Prices") %>%
  dySeries(name=colnames[1], label=colnames[1], strokeWidth=1, col="blue") %>%
  dySeries(name=colnames[2], label=colnames[2], strokeWidth=4, col="red") %>%
  dyLegend(show="always", width=500)
# Plot VWAP prices with custom line colors
x11(width=6, height=5)
colors <- c("blue", "red")
plot_theme <- chart_theme()
plot_theme$col$line.col <- colors
quantmod::chart_Series(prices["2009"], theme=plot_theme,
       lwd=2, name="VTI VWAP Prices")
legend("bottomright", legend=colnames(prices),
 inset=0.1, bg="white", lty=1, lwd=6, cex=0.8,
 col=plot_theme$col$line.col, bty="n")
# Calculate positions from lagged indicator
indic <- sign(closep - vwap)
lagg <- 2
indic <- roll::roll_sum(indic, width=lagg, min_obs=1)
# Calculate positions, either: -1, 0, or 1
position_s <- rep(NA_integer_, nrows)
position_s[1] <- 0
position_s <- ifelse(indic == lagg, 1, position_s)
position_s <- ifelse(indic == (-lagg), -1, position_s)
position_s <- zoo::na.locf(position_s, na.rm=FALSE)
position_s <- xts::xts(position_s, order.by=index(closep))
# Lag the positions to trade in next period
position_s <- rutils::lagit(position_s, lagg=1)
# Calculate PnLs of VWAP strategy
pnls <- vti*position_s
colnames(pnls) <- "VWAP Strategy"
wealth <- cbind(vti, pnls)
colnames(wealth) <- c("VTI", "VWAP Strategy")
colnames <- colnames(wealth)
# Annualized Sharpe ratios of VTI and VWAP strategy
sharp_e <- sqrt(252)*sapply(wealth, function (x) mean(x)/sd(x))
# Create colors for background shading
dates <- (rutils::diffit(position_s) != 0)
shad_e <- position_s[dates]
dates <- c(index(shad_e), end(position_s))
shad_e <- ifelse(drop(zoo::coredata(shad_e)) == 1, "lightgreen", "antiquewhite")
# Plot dygraph of VWAP strategy
# Create dygraph object without plotting it
dyplot <- dygraphs::dygraph(cumsum(wealth["2007/"]), main=paste("VWAP Crossover Strategy, Sharpe", paste(paste(names(sharp_e), round(sharp_e, 3), sep="="), collapse=", "))) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Add shading to dygraph object
for (i in 1:NROW(shad_e)) {
    dyplot <- dyplot %>% dyShading(from=dates[i], to=dates[i+1], color=shad_e[i])
}  # end for
# Plot the dygraph object
dyplot
# Calculate correlation of VWAP strategy with VTI
cor(vti, pnls)
# Combine VWAP strategy with VTI
wealth <- cbind(vti, pnls, 0.5*(vti+pnls))
colnames(wealth) <- c("VTI", "VWAP", "Combined")
sharp_e <- sqrt(252)*sapply(wealth, function (x) mean(x)/sd(x))
# Plot dygraph of VWAP strategy combined with VTI
dygraphs::dygraph(cumsum(wealth),
  main=paste("VWAP Crossover Strategy, Sharpe", paste(paste(names(sharp_e), round(sharp_e, 3), sep="="), collapse=", "))) %>%
  dyOptions(colors=c("blue", "red", "purple"), strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Test VWAP crossover market timing of VTI using Treynor-Mazuy test
design <- cbind(pnls, vti, vti^2)
design <- na.omit(design)
colnames(design) <- c("VWAP","VTI","treynor")
model <- lm(VWAP ~ VTI + treynor, data=design)
summary(model)
# Plot residual scatterplot
residuals <- (design$VWAP - model$coeff[2]*design$VTI)
residuals <- model$residuals
x11(width=6, height=6)
plot.default(x=design$VTI, y=residuals, xlab="VTI", ylab="residuals")
title(main="Treynor-Mazuy Market Timing Test\n for VWAP Crossover vs VTI", line=0.5)
# Plot fitted (predicted) response values
fit_ted <- (model$coeff["(Intercept)"] + model$coeff["treynor"]*vti^2)
points.default(x=design$VTI, y=fit_ted, pch=16, col="red")
text(x=0.05, y=0.8*max(residuals), paste("VWAP crossover t-value =", round(summary(model)$coeff["treynor", "t value"], 2)))
simu_vwap <- function(ohlc, look_back=333, bid_offer=0.001, trend=1, lagg=1) {
  close <- log(quantmod::Cl(ohlc))
  volumes <- quantmod::Vo(ohlc)
  returns <- rutils::diffit(close)
  nrows <- NROW(ohlc)
  # Calculate VWAP prices
  vwap <- roll::roll_sum(closep*volumes, width=look_back, min_obs=1)
  volume_roll <- roll::roll_sum(volumes, width=look_back, min_obs=1)
  vwap <- vwap/volume_roll
  # Calculate the indicator
  indic <- trend*sign(close - vwap)
  if (lagg > 1) {
    indic <- roll::roll_sum(indic, width=lagg, min_obs=1)
    indic[1:lagg] <- 0
  }  # end if
  # Calculate positions, either: -1, 0, or 1
  pos <- rep(NA_integer_, nrows)
  pos[1] <- 0
  pos <- ifelse(indic == lagg, 1, pos)
  pos <- ifelse(indic == (-lagg), -1, pos)
  pos <- zoo::na.locf(pos, na.rm=FALSE)
  pos <- xts::xts(pos, order.by=index(close))
  # Lag the positions to trade on next day
  pos <- rutils::lagit(pos, lagg=1)
  # Calculate PnLs of strategy
  pnls <- returns*pos
  costs <- 0.5*bid_offer*abs(rutils::diffit(pos))*close
  pnls <- (pnls - costs)
  # Calculate strategy returns
  pnls <- cbind(pos, pnls)
  colnames(pnls) <- c("positions", "pnls")
  pnls
}  # end simu_vwap
source("/Users/jerzy/Develop/lecture_slides/scripts/ewma_model.R")
look_backs <- seq(70, 200, 10)
# Perform lapply() loop over lambdas
pnls <- lapply(look_backs, function(look_back) {
  # Simulate VWAP strategy and calculate returns
  simu_vwap(ohlc=ohlc, look_back=look_back, bid_offer=0, lagg=2)[, "pnls"]
})  # end lapply
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("look_back=", look_backs)
# Plot dygraph of multiple VWAP strategies
colors <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls["2007/"]), main="Cumulative Returns of Trend Following VWAP Strategies") %>%
  dyOptions(colors=colors, strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Plot VWAP strategies with custom line colors
x11(width=6, height=5)
plot_theme <- chart_theme()
plot_theme$col$line.col <- colors
quantmod::chart_Series(cumsum(pnls), theme=plot_theme,
  name="Cumulative Returns of VWAP Strategies")
legend("topleft", legend=colnames(pnls), inset=0.1,
  bg="white", cex=0.8, lwd=rep(6, NCOL(pnls)),
  col=plot_theme$col$line.col, bty="n")
set.seed(1121)  # Reset random number generator
# Sample from Standard Normal Distribution
nrows <- 1000
datav <- rnorm(nrows)
# Sample mean - MC estimate
mean(datav)
# Sample standard deviation - MC estimate
sd(datav)
# Monte Carlo estimate of cumulative probability
pnorm(1)
sum(datav < 1)/nrows
# Monte Carlo estimate of quantile
confl <- 0.98
qnorm(confl)  # Exact value
cutoff <- confl*nrows
datav <- sort(datav)
datav[cutoff]  # Naive Monte Carlo value
quantile(datav, probs=confl)
# Analyze the source code of quantile()
stats:::quantile.default
# Microbenchmark quantile
library(microbenchmark)
summary(microbenchmark(
  monte_carlo = datav[cutoff],
  quantilev = quantile(datav, probs=confl),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
set.seed(1121)  # Reset random number generator
barp <- 20  # Barrier level
nrows <- 1000  # Number of simulation steps
paths <- numeric(nrows)  # Allocate path vector
paths[1] <- 0  # Initialize path
indeks <- 2  # Initialize simulation index
while ((indeks <= nrows) && (paths[indeks - 1] < barp)) {
# Simulate next step
  paths[indeks] <- paths[indeks - 1] + rnorm(1)
  indeks <- indeks + 1  # Advance indeks
}  # end while
# Fill remaining paths after it crosses barp
if (indeks <= nrows)
  paths[indeks:nrows] <- paths[indeks - 1]
# Plot the Brownian motion
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(1, 1, 1, 1))
plot(paths, type="l", col="black",
     lty="solid", lwd=2, xlab="", ylab="")
abline(h=barp, lwd=3, col="red")
title(main="Brownian Motion Crossing a Barrier Level", line=0.5)
set.seed(1121)  # Reset random number generator
barp <- 20  # Barrier level
nrows <- 1000  # Number of simulation steps
# Simulate path of Brownian motion
paths <- cumsum(rnorm(nrows))
# Find index when paths crosses barp
crossp <- which(paths > barp)
# Fill remaining paths after it crosses barp
if (NROW(crossp)>0) {
  paths[(crossp[1]+1):nrows] <- paths[crossp[1]]
}  # end if
# Plot the Brownian motion
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(1, 1, 1, 1))
plot(paths, type="l", col="black",
     lty="solid", lwd=2, xlab="", ylab="")
abline(h=barp, lwd=3, col="red")
title(main="Brownian Motion Crossing a Barrier Level", line=0.5)
# Define daily volatility and growth rate
sigmav <- 0.01; drift <- 0.0; nrows <- 1000
# Simulate geometric Brownian motion
returns <- sigmav*rnorm(nrows) + drift - sigmav^2/2
prices <- exp(cumsum(returns))
plot(prices, type="l", xlab="time", ylab="prices",
     main="geometric Brownian motion")
# Simulate geometric Brownian motion
sigmav <- 0.01/sqrt(48)
drift <- 0.0
nrows <- 1e4
dates <- seq(from=as.POSIXct(paste(Sys.Date()-250, "09:30:00")),
  length.out=nrows, by="30 min")
prices <- exp(cumsum(sigmav*rnorm(nrows) + drift - sigmav^2/2))
prices <- xts(prices, order.by=dates)
prices <- cbind(prices,
  volume=sample(x=10*(2:18), size=nrows, replace=TRUE))
# Aggregate to daily OHLC data
ohlc <- xts::to.daily(prices)
quantmod::chart_Series(ohlc, name="random prices")
# dygraphs candlestick plot using pipes syntax
library(dygraphs)
dygraphs::dygraph(ohlc[, 1:4]) %>% dyCandlestick()
# dygraphs candlestick plot without using pipes syntax
dygraphs::dyCandlestick(dygraphs::dygraph(ohlc[, 1:4]))
# Standard deviations of log-normal distribution
sigmavs <- c(0.5, 1, 1.5)
# Create plot colors
colors <- c("black", "red", "blue")
# Plot all curves
for (indeks in 1:NROW(sigmavs)) {
  curve(expr=dlnorm(x, sdlog=sigmavs[indeks]),
  type="l", lwd=2, xlim=c(0, 3),
  xlab="", ylab="", col=colors[indeks],
  add=as.logical(indeks-1))
}  # end for
# Add title and legend
title(main="Log-normal Distributions", line=0.5)
legend("topright", inset=0.05, title="Sigmas",
 paste("sigma", sigmavs, sep="="),
 cex=0.8, lwd=2, lty=rep(1, NROW(sigmavs)),
 col=colors)
x11(width=6, height=5)
par(mar=c(4, 4, 3, 1))
# Return volatility of VTI etf
sigmav <- sd(rutils::diffit(log(rutils::etfenv$VTI[, 4])))
sigma2 <- sigmav^2
nrows <- NROW(rutils::etfenv$VTI)
# Standard deviation of log-normal prices
sqrt(nrows)*sigmav
# Skewness of log-normal prices
skew_ness <- function(t) {
  ex_p <- exp(t*sigma2)
  (ex_p + 2)*sqrt(ex_p - 1)
}  # end skew_ness
curve(expr=skew_ness, xlim=c(1, nrows), lwd=3,
xlab="Number of days", ylab="Skewness", col="blue",
main="Skewness of Log-normal Prices
as a Function of Time")
# Probability that random log-normal price will be lower than the mean price
curve(expr=pnorm(sigmav*sqrt(x)/2),
xlim=c(1, nrows), lwd=3,
xlab="Number of days", ylab="Probability", col="blue",
main="Probability That Random Log-normal Price
Will be Lower Than the Mean Price")
# Define daily volatility and growth rate
sigmav <- 0.01; drift <- 0.0; nrows <- 5000
paths <- 10
# Simulate multiple paths of geometric Brownian motion
prices <- matrix(rnorm(paths*nrows, sd=sigmav) +
    drift - sigmav^2/2, nc=paths)
prices <- exp(matrixStats::colCumsums(prices))
# Create xts time series
prices <- xts(prices, order.by=seq.Date(Sys.Date()-NROW(prices)+1, Sys.Date(), by=1))
# Plot xts time series
colors <- colorRampPalette(c("red", "blue"))(NCOL(prices))
colors <- colors[order(order(prices[NROW(prices), ]))]
par(mar=c(3, 3, 2, 2), oma=c(0, 0, 0, 0))
plot.zoo(prices, main="Multiple paths of geometric Brownian motion",
   xlab=NA, ylab=NA, plot.type="single", col=colors)
# Define daily volatility and growth rate
sigmav <- 0.01; drift <- 0.0; nrows <- 10000
paths <- 100
# Simulate multiple paths of geometric Brownian motion
prices <- matrix(rnorm(paths*nrows, sd=sigmav) +
    drift - sigmav^2/2, nc=paths)
prices <- exp(matrixStats::colCumsums(prices))
# Calculate percentage of paths below the expected value
per_centage <- rowSums(prices < 1.0) / paths
# Create xts time series of percentage of paths below the expected value
per_centage <- xts(per_centage, order.by=seq.Date(Sys.Date()-NROW(per_centage)+1, Sys.Date(), by=1))
# Plot xts time series of percentage of paths below the expected value
par(mar=c(3, 3, 2, 2), oma=c(0, 0, 0, 0))
plot.zoo(per_centage, main="Percentage of GBM paths below mean",
   xlab=NA, ylab=NA, col="blue")
# Load S&P500 stock prices
load("/Users/jerzy/Develop/lecture_slides/data/sp500.RData")
ls(sp500env)
# Extract closing prices
prices <- eapply(sp500env, quantmod::Cl)
# Flatten prices into a single xts series
prices <- rutils::do_call(cbind, prices)
# Carry forward and backward non-NA prices
prices <- zoo::na.locf(prices, na.rm=FALSE)
prices <- zoo::na.locf(prices, fromLast=TRUE)
sum(is.na(prices))
# Drop ".Close" from column names
colnames(prices[, 1:4])
colnames(prices) <- rutils::get_name(colnames(prices))
# Or
# colnames(prices) <- do.call(rbind,
#   strsplit(colnames(prices), split="[.]"))[, 1]
# Normalize columns
prices <- xts(t(t(prices) / as.numeric(prices[1, ])),
         order.by=index(prices))
# Calculate permution index for sorting the lowest to highest final prices
ordern <- order(prices[NROW(prices), ])
# Select a few symbols
symbols <- colnames(prices)[ordern]
symbols <- symbols[seq.int(from=1, to=(NROW(symbols)-1), length.out=20)]
# Plot xts time series of prices
colors <- colorRampPalette(c("red", "blue"))(NROW(symbols))
colors <- colors[order(order(prices[NROW(prices), symbols]))]
par(mar=c(3, 3, 2, 2), oma=c(0, 0, 0, 0))
plot.zoo(prices[, symbols], main="20 S&P500 stock prices (normalized)",
   xlab=NA, ylab=NA, plot.type="single", col=colors)
legend(x="topleft", inset=0.05, cex=0.8,
 legend=rev(symbols), col=rev(colors), lwd=6, lty=1)
# Calculate average of valid stock prices
validp <- (prices != 1)  # Valid stocks
nstocks <- rowSums(validp)
nstocks[1] <- NCOL(prices)
indeks <- rowSums(prices * validp) / nstocks
# Calculate percentage of stock prices below the average price
per_centage <- rowSums((prices < indeks) & validp) / nstocks
# Create xts time series of average stock prices
indeks <- xts(indeks, order.by=index(prices))
x11(width=6, height=4)
par(mar=c(3, 3, 2, 2), oma=c(0, 0, 0, 0))
# Plot xts time series of average stock prices
plot.zoo(indeks, main="Average S&P500 stock prices (normalized from 1990)",
   xlab=NA, ylab=NA, col="blue")
# Create xts time series of percentage of stock prices below the average price
per_centage <- xts(per_centage, order.by=index(prices))
# Plot percentage of stock prices below the average price
plot.zoo(per_centage[-(1:2),],
   main="Percentage of S&P500 stock prices below the average price",
   xlab=NA, ylab=NA, col="blue")
# Define Brownian Motion parameters
nrows <- 1000; sigmav <- 0.01
# Simulate 5 paths of Brownian motion
prices <- matrix(rnorm(5*nrows, sd=sigmav), nc=5)
prices <- matrixStats::colCumsums(prices)
# Open plot window on Mac
dev.new(width=6, height=4, noRStudioGD=TRUE)
# Set plot parameters to reduce whitespace around plot
par(mar=c(2, 2, 3, 1), oma=c(0, 0, 0, 0))
# Plot 5 paths of Brownian motion
matplot(y=prices, main="Brownian Motion Paths",
  xlab="", ylab="", type="l", lty="solid", lwd=1, col="blue")
# Save plot to png file on Mac
quartz.save("figure/brown_paths.png", type="png", width=6, height=4)
# Define Ornstein-Uhlenbeck parameters
init_price <- 0.0; eq_price <- 1.0;
sigmav <- 0.02; thetav <- 0.01; nrows <- 1000
# Initialize the data
innov <- rnorm(nrows)
returns <- numeric(nrows)
prices <- numeric(nrows)
prices[1] <- init_price
# Simulate Ornstein-Uhlenbeck process in R
for (i in 2:nrows) {
  returns[i] <- thetav*(eq_price - prices[i-1]) +
    sigmav*innov[i]
  prices[i] <- prices[i-1] + returns[i]
}  # end for
# Simulate Ornstein-Uhlenbeck process in Rcpp
prices_cpp <- HighFreq::sim_ou(init_price=init_price, eq_price=eq_price,
  volat=sigmav, theta=thetav, innov=matrix(innov))
all.equal(prices, drop(prices_cpp))
# Compare the speed of R code with Rcpp
library(microbenchmark)
summary(microbenchmark(
  Rcode={for (i in 2:nrows) {
    returns[i] <- thetav*(eq_price - prices[i-1]) + sigmav*innov[i]
    prices[i] <- prices[i-1] + returns[i]}},
  Rcpp=HighFreq::sim_ou(init_price=init_price, eq_price=eq_price,
    volat=sigmav, theta=thetav, innov=matrix(innov)),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
plot(prices, type="l",
     xlab="time", ylab="prices",
     main="Ornstein-Uhlenbeck Process")
legend("topright",
 title=paste(c(paste0("sigmav = ", sigmav),
               paste0("eq_price = ", eq_price),
               paste0("thetav = ", thetav)),
             collapse="\n"),
 legend="", cex=0.8, inset=0.1, bg="white", bty="n")
abline(h=eq_price, col='red', lwd=2)
returns <- rutils::diffit(prices)
lag_prices <- rutils::lagit(prices)
formulav <- returns ~ lag_prices
l_m <- lm(formulav)
summary(l_m)
# Plot regression
plot(formulav, main="OU Returns Versus Lagged Prices")
abline(l_m, lwd=2, col="red")
# Calculate volatility parameter
c(volatility=sigmav, estimate=sd(returns))
# Extract OU parameters from regression
coeff <- summary(l_m)$coefficients
# Calculate regression alpha and beta directly
betav <- cov(returns, lag_prices)/var(lag_prices)
alpha <- (mean(returns) - betav*mean(lag_prices))
cbind(direct=c(alpha=alpha, beta=betav), lm=coeff[, 1])
all.equal(c(alpha=alpha, beta=betav), coeff[, 1],
    check.attributes=FALSE)
# Calculate regression standard errors directly
betas <- c(alpha=alpha, beta=betav)
fit_ted <- (alpha + betav*lag_prices)
residuals <- (returns - fit_ted)
prices_squared <- sum((lag_prices - mean(lag_prices))^2)
betasd <- sqrt(sum(residuals^2)/prices_squared/(nrows-2))
alpha_sd <- sqrt(sum(residuals^2)/(nrows-2)*(1/nrows + mean(lag_prices)^2/prices_squared))
cbind(direct=c(alpha_sd=alpha_sd, betasd=betasd), lm=coeff[, 2])
all.equal(c(alpha_sd=alpha_sd, betasd=betasd), coeff[, 2],
    check.attributes=FALSE)
# Compare mean reversion parameter theta
c(theta=(-thetav), round(coeff[2, ], 3))
# Compare equilibrium price mu
c(eq_price=eq_price, estimate=-coeff[1, 1]/coeff[2, 1])
# Compare actual and estimated parameters
coeff <- cbind(c(thetav*eq_price, -thetav), coeff[, 1:2])
rownames(coeff) <- c("drift", "theta")
colnames(coeff)[1] <- "actual"
round(coeff, 4)
# Simulate Schwartz process
returns <- numeric(nrows)
prices <- numeric(nrows)
prices[1] <- exp(sigmav*innov[1])
set.seed(1121)  # Reset random numbers
for (i in 2:nrows) {
  returns[i] <- thetav*(eq_price - prices[i-1]) + sigmav*innov[i]
  prices[i] <- prices[i-1]*exp(returns[i])
}  # end for
plot(prices, type="l", xlab="time", ylab="prices",
     main="Schwartz Process")
legend("topright",
 title=paste(c(paste0("sigmav = ", sigmav),
               paste0("eq_price = ", eq_price),
               paste0("thetav = ", thetav)),
             collapse="\n"),
 legend="", cex=0.8, inset=0.12, bg="white", bty="n")
abline(h=eq_price, col='red', lwd=2)
x11(width=6, height=5)
par(mar=c(3, 2, 1, 1), oma=c(1, 0, 0, 0))
returns <- na.omit(rutils::etfenv$returns$VTI)
# Plot autocorrelations using stats::acf()
stats::acf(returns, lag=10, xlab="lag", main="")
title(main="ACF of VTI Returns", line=-1)
# Two-tailed 95% confidence interval
qnorm(0.975)/sqrt(NROW(returns))
# Ljung-Box test for VTI returns
# 'lag' is the number of autocorrelation coefficients
Box.test(returns, lag=10, type="Ljung")
library(Ecdat)  # Load Ecdat
macro_zoo <- as.zoo(Macrodat[, c("lhur", "fygm3")])
colnames(macro_zoo) <- c("unemprate", "3mTbill")
macro_diff <- na.omit(diff(macro_zoo))
# Changes in 3 month T-bill rate are autocorrelated
Box.test(macro_diff[, "3mTbill"], lag=10, type="Ljung")
# Changes in unemployment rate are autocorrelated
Box.test(macro_diff[, "unemprate"], lag=10, type="Ljung")
# Get the ACF data returned invisibly
acf_data <- acf(returns, plot=FALSE)
summary(acf_data)
# Print the ACF data
print(acf_data)
dim(acf_data$acf)
dim(acf_data$lag)
head(acf_data$acf)
plot_acf <- function(xtes, lagg=10, plotobj=TRUE,
               xlab="Lag", ylab="", main="", ...) {
  # Calculate the ACF without a plot
  acf_data <- acf(x=xtes, lag.max=lagg, plot=FALSE, ...)
  # Remove first element of ACF data
  acf_data$acf <- array(data=acf_data$acf[-1],
    dim=c((dim(acf_data$acf)[1]-1), 1, 1))
  acf_data$lag <- array(data=acf_data$lag[-1],
    dim=c((dim(acf_data$lag)[1]-1), 1, 1))
  # Plot ACF
  if (plotobj) {
    ci <- qnorm((1+0.95)/2)*sqrt(1/NROW(xtes))
    ylim <- c(min(-ci, range(acf_data$acf[-1])),
        max(ci, range(acf_data$acf[-1])))
    plot(acf_data, xlab=xlab, ylab=ylab,
   ylim=ylim, main="", ci=0)
    title(main=main, line=0.5)
    abline(h=c(-ci, ci), col="blue", lty=2)
  }  # end if
  # Return the ACF data invisibly
  invisible(acf_data)
}  # end plot_acf
# Improved autocorrelation function
x11(width=6, height=5)
rutils::plot_acf(returns, lag=10, main="")
title(main="ACF of VTI returns", line=-1)
# Ljung-Box test for VTI returns
Box.test(returns, lag=10, type="Ljung")
x11(width=6, height=7)
par(mfrow=c(2,1))  # Set plot panels
par(mar=c(3, 3, 2, 2), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
# Autocorrelation of squared random returns
rutils::plot_acf(rnorm(NROW(returns))^2, lag=10, main="")
title(main="ACF of Squared Random Returns", line=-1)
# Autocorrelation of squared VTI returns
rutils::plot_acf(returns^2, lag=10, main="")
title(main="ACF of Squared VTI Returns", line=-1)
# Ljung-Box test for squared VTI returns
Box.test(returns^2, lag=10, type="Ljung")
library(rutils)  # Load package rutils
library(Ecdat)  # Load Ecdat
colnames(Macrodat)  # United States Macroeconomic Time Series
# Coerce to "zoo"
macro_zoo <- as.zoo(Macrodat[, c("lhur", "fygm3")])
colnames(macro_zoo) <- c("unemprate", "3mTbill")
# ggplot2 in multiple panes
autoplot(  # Generic ggplot2 for "zoo"
  object=macro_zoo, main="US Macro",
  facets=Series ~ .) + # end autoplot
  xlab("") +
theme(  # Modify plot theme
  legend.position=c(0.1, 0.5),
  plot.title=element_text(vjust=-2.0),
  plot.margin=unit(c(-0.5, 0.0, -0.5, 0.0), "cm"),
  plot.background=element_blank(),
  axis.text.y=element_blank()
)  # end theme
par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # Set plot panels
macro_diff <- na.omit(diff(macro_zoo))
rutils::plot_acf(coredata(macro_diff[, "unemprate"]),
  lag=10, main="quarterly unemployment rate")
rutils::plot_acf(coredata(macro_diff[, "3mTbill"]),
  lag=10, main="3 month T-bill EOQ")
# Simulate AR processes
set.seed(1121)  # Reset random numbers
dates <- Sys.Date() + 0:728  # Two year daily series
# AR time series of returns
arimav <- xts(x=arima.sim(n=NROW(dates), model=list(ar=0.2)),
          order.by=dates)
arimav <- cbind(arimav, cumsum(arimav))
colnames(arimav) <- c("AR returns", "AR prices")
library(ggplot2)  # Load ggplot2
library(gridExtra)  # Load gridExtra
autoplot(object=arimav, # ggplot AR process
 facets="Series ~ .",
 main="Autoregressive process (phi=0.2)") +
  facet_grid("Series ~ .", scales="free_y") +
  xlab("") + ylab("") +
theme(legend.position=c(0.1, 0.5),
  plot.background=element_blank(),
  axis.text.y=element_blank())
ar_coeff <- c(-0.9, 0.01, 0.9)  # AR coefficients
# Create three AR time series
arimav <- sapply(ar_coeff, function(phi) {
  set.seed(1121)  # Reset random numbers
  arima.sim(n=NROW(dates), model=list(ar=phi))
})  # end sapply
colnames(arimav) <- paste("autocorr", ar_coeff)
plot.zoo(arimav, main="AR(1) prices", xlab=NA)
# Or plot using ggplot
arimav <- xts(x=arimav, order.by=dates)
library(ggplot)
autoplot(arimav, main="AR(1) prices",
   facets=Series ~ .) +
    facet_grid(Series ~ ., scales="free_y") +
xlab("") +
theme(
  legend.position=c(0.1, 0.5),
  plot.title=element_text(vjust=-2.0),
  plot.margin=unit(c(-0.5, 0.0, -0.5, 0.0), "cm"),
  plot.background=element_blank(),
  axis.text.y=element_blank())
# Define AR(3) coefficients and innovations
coeff <- c(0.1, 0.39, 0.5)
nrows <- 1e2
set.seed(1121); innov <- rnorm(nrows)
# Simulate AR process using recursive loop in R
arimav <- numeric(NROW(innov))
arimav[1] <- innov[1]
arimav[2] <- coeff[1]*arimav[1] + innov[2]
arimav[3] <- coeff[1]*arimav[2] + coeff[2]*arimav[1] + innov[3]
for (it in 4:NROW(arimav)) {
  arimav[it] <- arimav[(it-1):(it-3)] %*% coeff + innov[it]
}  # End for
# Simulate AR process using filter()
arima_faster <- filter(x=innov, filter=coeff, method="recursive")
class(arima_faster)
all.equal(arimav, as.numeric(arima_faster))
# Fast simulation of AR process using C_rfilter()
arima_fastest <- .Call(stats:::C_rfilter, innov, coeff,
                 double(NROW(coeff) + NROW(innov)))[-(1:3)]
all.equal(arimav, arima_fastest)
# Calculate modulus of roots of characteristic equation
root_s <- Mod(polyroot(c(1, -coeff)))
# Calculate warmup period
warm_up <- NROW(coeff) + ceiling(6/log(min(root_s)))
set.seed(1121)
nrows <- 1e4
innov <- rnorm(nrows + warm_up)
# Simulate AR process using arima.sim()
arimav <- arima.sim(n=nrows,
  model=list(ar=coeff),
  start.innov=innov[1:warm_up],
  innov=innov[(warm_up+1):NROW(innov)])
# Simulate AR process using filter()
arima_fast <- filter(x=innov, filter=coeff, method="recursive")
all.equal(arima_fast[-(1:warm_up)], as.numeric(arimav))
# Benchmark the speed of the three methods of simulating AR process
library(microbenchmark)
summary(microbenchmark(
  filter=filter(x=innov, filter=coeff, method="recursive"),
  arima_sim=arima.sim(n=nrows,
                  model=list(ar=coeff),
                  start.innov=innov[1:warm_up],
                  innov=innov[(warm_up+1):NROW(innov)]),
  arima_loop={for (it in 4:NROW(arimav)) {
  arimav[it] <- arimav[(it-1):(it-3)] %*% coeff + innov[it]}}
  ), times=10)[, c(1, 4, 5)]
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0))
# Simulate AR(1) process
arimav <- arima.sim(n=1e3, model=list(ar=0.8))
# ACF of AR(1) process
acfd <- rutils::plot_acf(arimav, lag=10, xlab="", ylab="",
  main="Autocorrelations of AR(1) process")
acfd$acf[1:5]
# PACF of AR(1) process
pacfd <- pacf(arimav, lag=10, xlab="", ylab="", main="")
title("Partial autocorrelations of AR(1) process", line=1)
pacfd <- drop(pacfd$acf)
pacfd[1:5]
# Compute pacf recursively from acf
acfd <- rutils::plot_acf(arimav, lag=10, plotobj=FALSE)
acfd <- drop(acfd$acf)
pacfd <- numeric(3)
pacfd[1] <- acfd[1]
pacfd[2] <- acfd[2] - acfd[1]^2
pacfd[3] <- acfd[3] - pacfd[2]*acfd[1] - acfd[2]*pacfd[1]
# Compute pacf recursively in a loop
pacfd <- numeric(NROW(acfd))
pacfd[1] <- acfd[1]
for (it in 2:NROW(pacfd)) {
  pacfd[it] <- acfd[it] - pacfd[1:(it-1)] %*% acfd[(it-1):1]
}  # end for
par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # Set plot panels
# Simulate AR process of returns
arimav <- arima.sim(n=1e3, model=list(ar=c(0.1, 0.5, 0.1)))
# ACF of AR(3) process
rutils::plot_acf(arimav, lag=10, xlab="", ylab="",
   main="ACF of AR(3) process")
# PACF of AR(3) process
pacf(arimav, lag=10, xlab="", ylab="", main="PACF of AR(3) process")
library(rutils)  # Load rutils
library(ggplot2)  # Load ggplot2
set.seed(1121)  # Initialize random number generator
rand_walk <- cumsum(zoo(matrix(rnorm(3*100), ncol=3),
            order.by=(Sys.Date()+0:99)))
colnames(rand_walk) <- paste("rand_walk", 1:3, sep="_")
plot.zoo(rand_walk, main="Random walks",
     xlab="", ylab="", plot.type="single",
     col=c("black", "red", "blue"))
# Add legend
legend(x="topleft", legend=colnames(rand_walk),
 col=c("black", "red", "blue"), lty=1)
# Simulate arima with large AR coefficient
set.seed(1121)
arimav <- arima.sim(n=nrows, model=list(ar=0.99))
tseries::adf.test(arimav)
# Integrated series has unit root
tseries::adf.test(cumsum(arimav))
# Simulate arima with negative AR coefficient
set.seed(1121)
arimav <- arima.sim(n=nrows, model=list(ar=-0.99))
tseries::adf.test(arimav)
# Integrated series has unit root
tseries::adf.test(cumsum(arimav))
# Simulate random walks using apply() loops
set.seed(1121)  # Initialize random number generator
rand_walks <- matrix(rnorm(1000*100), ncol=1000)
rand_walks <- apply(rand_walks, 2, cumsum)
variance <- apply(rand_walks, 1, var)
# Simulate random walks using vectorized functions
set.seed(1121)  # Initialize random number generator
rand_walks <- matrixStats::colCumsums(matrix(rnorm(1000*100), ncol=1000))
variance <- matrixStats::rowVars(rand_walks)
par(mar=c(5, 3, 2, 2), oma=c(0, 0, 0, 0))
plot(variance, xlab="time steps", ylab="",
     t="l", col="blue", lwd=2,
     main="Variance of Random Walk")
# Define Dickey-Fuller parameters
init_price <- 0.0; eq_price <- 1.0;
sigmav <- 0.02; thetav <- 0.01; nrows <- 1000
# Initialize the data
innov <- rnorm(nrows)
returns <- numeric(nrows)
prices <- numeric(nrows)
# Simulate Dickey-Fuller process in R
prices[1] <- sigmav*innov[1]
for (i in 2:nrows) {
  returns[i] <- thetav*(eq_price - prices[i-1]) + sigmav*innov[i]
  prices[i] <- prices[i-1] + returns[i]
}  # end for
# Simulate Dickey-Fuller process in Rcpp
prices_cpp <- HighFreq::sim_ou(init_price=init_price, eq_price=eq_price,
  volat=sigmav, theta=thetav, innov=matrix(innov))
all.equal(prices, drop(prices_cpp))
# Compare the speed of R code with Rcpp
library(microbenchmark)
summary(microbenchmark(
  Rcode={for (i in 2:nrows) {
    returns[i] <- thetav*(eq_price - prices[i-1]) + sigmav*innov[i]
    prices[i] <- prices[i-1] + returns[i]}},
  Rcpp=HighFreq::sim_ou(eq_price=eq_price, volat=sigmav, theta=thetav, innov=matrix(innov)),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
set.seed(1121); innov <- matrix(rnorm(1e4, sd=0.01))
# Simulate AR(1) process with coefficient=1, with unit root
arimav <- HighFreq::sim_ar(coeff=matrix(1), innov=innov)
x11(); plot(arimav, t="l", main="AR(1) coefficient = 1.0")
# Perform ADF test with lag = 1
tseries::adf.test(arimav, k=1)
# Perform standard Dickey-Fuller test
tseries::adf.test(arimav, k=0)
# Simulate AR(1) with coefficient close to 1, without unit root
arimav <- HighFreq::sim_ar(coeff=matrix(0.99), innov=innov)
x11(); plot(arimav, t="l", main="AR(1) coefficient = 0.99")
tseries::adf.test(arimav, k=1)
# Simulate Ornstein-Uhlenbeck OU process with mean reversion
init_price <- 0.0; eq_price <- 0.0; thetav <- 0.1
prices <- HighFreq::sim_ou(init_price=init_price, eq_price=eq_price,
  volat=1.0, theta=thetav, innov=innov)
x11(); plot(prices, t="l", main=paste("OU coefficient =", thetav))
tseries::adf.test(prices, k=1)
# Simulate Ornstein-Uhlenbeck OU process with zero reversion
thetav <- 0.0
prices <- HighFreq::sim_ou(init_price=init_price, eq_price=eq_price,
  volat=1.0, theta=thetav, innov=innov)
x11(); plot(prices, t="l", main=paste("OU coefficient =", thetav))
tseries::adf.test(prices, k=1)
# Specify AR process parameters
nrows <- 1e3
coeff <- matrix(c(0.1, 0.39, 0.5)); n_coeff <- NROW(coeff)
set.seed(1121); innov <- matrix(rnorm(nrows))
# arimav <- filter(x=innov, filter=coeff, method="recursive")
# Simulate AR process using HighFreq::sim_ar()
arimav <- HighFreq::sim_ar(coeff=coeff, innov=innov)
# Fit AR model using ar.ols()
arfit <- ar.ols(arimav, order.max=n_coeff, aic=FALSE)
class(arfit)
is.list(arfit)
drop(arfit$ar); drop(coeff)
# Define design matrix without intercept column
design <- sapply(1:n_coeff, rutils::lagit, input=arimav)
# Fit AR model using regression
design_inv <- MASS::ginv(design)
coeff_fit <- drop(design_inv %*% arimav)
all.equal(drop(arfit$ar), coeff_fit, check.attributes=FALSE)
# Calculate the regression residuals
fit_ted <- drop(design %*% coeff_fit)
residuals <- drop(arimav - fit_ted)
# Variance of residuals
var_resid <- sum(residuals^2)/(nrows-NROW(coeff_fit))
# Design matrix squared
design2 <- crossprod(design)
# Calculate covariance matrix of AR coefficients
covar <- var_resid*MASS::ginv(design2)
coeff_fitd <- sqrt(diag(covar))
# Calculate t-values of AR coefficients
coeff_tvals <- drop(coeff_fit)/coeff_fitd
# Fit AR(5) model into AR(3) process
design <- sapply(1:5, rutils::lagit, input=arimav)
design_inv <- MASS::ginv(design)
coeff_fit <- drop(design_inv %*% arimav)
# Calculate t-values of AR(5) coefficients
residuals <- drop(arimav - drop(design %*% coeff_fit))
var_resid <- sum(residuals^2)/(nrows-NROW(coeff_fit))
covar <- var_resid*MASS::ginv(crossprod(design))
coeff_fitd <- sqrt(diag(covar))
coeff_tvals <- drop(coeff_fit)/coeff_fitd
# Fit AR(5) model using arima()
arima_fit <- arima(arimav, order=c(5, 0, 0), include.mean=FALSE)
arima_fit$coef
# Fit AR(5) model using auto.arima()
library(forecast)  # Load forecast
arima_fit <- forecast::auto.arima(arimav, max.p=5, max.q=0, max.d=0)
# Fit AR(5) model into VTI returns
returns <- drop(zoo::coredata(na.omit(rutils::etfenv$returns$VTI)))
design <- sapply(1:5, rutils::lagit, input=returns)
design_inv <- MASS::ginv(design)
coeff_fit <- drop(design_inv %*% returns)
# Calculate t-values of AR(5) coefficients
residuals <- drop(returns - drop(design %*% coeff_fit))
var_resid <- sum(residuals^2)/(nrows-NROW(coeff_fit))
covar <- var_resid*MASS::ginv(crossprod(design))
coeff_fitd <- sqrt(diag(covar))
coeff_tvals <- drop(coeff_fit)/coeff_fitd
# Compute autocorrelation coefficients
acfd <- acf(arimav, lag=10, plot=FALSE)
acfd <- drop(acfd$acf)
acf1 <- acfd[-NROW(acfd)]
# Define Yule-Walker matrix
yule_walker <- sapply(2:9, function(lagg) {
  c(acf1[lagg:1], acf1[2:(NROW(acf1)-lagg+1)])
})  # end sapply
yule_walker <- cbind(acf1, yule_walker, rev(acf1))
# Generalized inverse of Yule-Walker matrix
yule_walker_inv <- MASS::ginv(yule_walker)
# Solve Yule-Walker equations
coeff_yw <- drop(yule_walker_inv %*% acfd[-1])
round(coeff_yw, 5)
coeff_fit
nrows <- 1e2
coeff <- c(0.1, 0.39, 0.5); n_coeff <- NROW(coeff)
set.seed(1121); innov <- rnorm(nrows)
# Simulate AR process using filter()
arimav <- filter(x=innov, filter=coeff, method="recursive")
arimav <- as.numeric(arimav)
# Simulate AR process using C_rfilter()
arima_fast <- .Call(stats:::C_rfilter, innov, coeff,
  double(nrows + n_coeff))
all.equal(arimav, arima_fast[-(1:n_coeff)],
  check.attributes=FALSE)
# Forecast AR(3) process using loop in R
forecasts <- numeric(NROW(arimav)+1)
forecasts[1] <- 0
forecasts[2] <- coeff[1]*arimav[1]
forecasts[3] <- coeff[1]*arimav[2] + coeff[2]*arimav[1]
for (it in 4:NROW(forecasts)) {
  forecasts[it] <- arimav[(it-1):(it-3)] %*% coeff
}  # end for
# Plot with legend
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
plot(arimav, main="Forecasting Using AR(3) Model",
  xlab="", ylab="", type="l")
lines(forecasts, col="orange", lwd=3)
legend(x="topright", legend=c("series", "forecasts"),
 col=c("black", "orange"), lty=1, lwd=6,
 cex=0.9, bg="white", bty="n")
# Forecast using filter()
filter_fast <- filter(x=arimav, sides=1,
  filter=coeff, method="convolution")
filter_fast <- as.numeric(filter_fast)
# Compare excluding warmup period
all.equal(forecasts[-(1:n_coeff)], filter_fast[-(1:(n_coeff-1))],
    check.attributes=FALSE)
# Filter using C_cfilter() compiled C++ function directly
filter_fast <- .Call(stats:::C_cfilter, arimav, filter=coeff,
               sides=1, circular=FALSE)
# Compare excluding warmup period
all.equal(forecasts[-(1:n_coeff)], filter_fast[-(1:(n_coeff-1))],
    check.attributes=FALSE)
# Filter using HighFreq::roll_conv() Rcpp function
filter_fast <- HighFreq::roll_conv(matrix(arimav), matrix(coeff))
# Compare excluding warmup period
all.equal(forecasts[-(1:n_coeff)], filter_fast[-(1:(n_coeff-1))],
    check.attributes=FALSE)
# Define predictor matrix for forecasting
predictor <- sapply(0:(n_coeff-1), function(lagg) {
  rutils::lagit(arimav, lagg=lagg)
})  # end sapply
# Forecast using predictor matrix
filter_fast <- c(0, drop(predictor %*% coeff))
# Compare with loop in R
all.equal(forecasts, filter_fast, check.attributes=FALSE)
# Fit ARIMA model using arima()
arima_fit <- arima(arimav, order=c(3,0,0), include.mean=FALSE)
arima_fit$coef
coeff
# One-step-ahead forecast using predict.Arima()
predictv <- predict(arima_fit, n.ahead=1)
# Or directly call predict.Arima()
# predictv <- predict.Arima(arima_fit, n.ahead=1)
# Inspect the prediction object
class(predictv)
names(predictv)
class(predictv$pred)
unlist(predictv)
# One-step-ahead forecast using matrix algebra
forecastv <- drop(arimav[nrows:(nrows-2)] %*% arima_fit$coef)
# Compare one-step-ahead forecasts
all.equal(predictv$pred[[1]], forecastv)
# Get information about predict.Arima()
?stats:::predict.Arima
# Calculate the in-sample forecasting residuals
residuals <- (arimav - forecasts[-NROW(forecasts)])
# Compare residuals with innovations
all.equal(innov, residuals, check.attributes=FALSE)
plot(residuals, t="l", lwd=3, xlab="", ylab="",
     main="ARIMA Forecast Errors")
# Define AR process parameters
nrows <- 1e3
coeff <- c(0.5, 0.0, 0.0); n_coeff <- NROW(coeff)
set.seed(1121); innov <- rnorm(nrows)
# Simulate AR process using C_rfilter()
arimav <- .Call(stats:::C_rfilter, innov, coeff,
  double(nrows + n_coeff))[-(1:n_coeff)]
# Define order of the AR(n) forecasting model
ordern <- 5
# Define predictor matrix for forecasting
design <- sapply(1:ordern, rutils::lagit, input=arimav)
colnames(design) <- paste0("pred_", 1:NCOL(design))
# Add response equal to series
design <- cbind(arimav, design)
colnames(design)[1] <- "response"
# Specify length of look-back interval
look_back <- 100
# Invert the predictor matrix
rangev <- (nrows-look_back):(nrows-1)
design_inv <- MASS::ginv(design[rangev, -1])
# Calculate fitted coefficients
coeff_fit <- drop(design_inv %*% design[rangev, 1])
# Calculate forecast
drop(design[nrows, -1] %*% coeff_fit)
# Calculate a vector of daily VTI log returns
returns <- na.omit(rutils::etfenv$returns$VTI)
dates <- index(returns)
returns <- as.numeric(returns)
nrows <- NROW(returns)
# Define predictor as a rolling sum
nagg <- 5
predictor <- rutils::roll_sum(returns, look_back=nagg)
# Shift the response forward out-of-sample
response <- rutils::lagit(predictor, lagg=(-nagg))
# Define predictor matrix for forecasting
order_max <- 5
predictor <- sapply(1+nagg*(0:order_max), rutils::lagit,
               input=predictor)
predictor <- cbind(rep(1, nrows), predictor)
# Define design matrix
design <- cbind(response, predictor)
# Perform rolling forecasting
look_back <- 100
forecasts <- sapply((look_back+1):nrows, function(endp) {
  # Define rolling look-back range
  startp <- max(1, endp-look_back)
  # Or expanding look-back range
  # startp <- 1
  rangev <- startp:(endp-1)
  # Invert the predictor matrix
  design_inv <- MASS::ginv(design[rangev, -1])
  # Calculate fitted coefficients
  coeff_fit <- drop(design_inv %*% design[rangev, 1])
  # Calculate forecast
  drop(design[endp, -1] %*% coeff_fit)
})  # end sapply
# Add warmup period
forecasts <- c(rep(0, look_back), forecasts)
# Mean squared error
mean((returns - forecasts)^2)
# Correlation
cor(forecasts, returns)
# Plot forecasting series with legend
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0))
plot(forecasts[(nrows-look_back):nrows], col="red",
     xlab="", ylab="", type="l", lwd=2,
     main="Rolling Forecasting Using AR Model")
lines(returns[(nrows-look_back):nrows], col="blue", lwd=2)
legend(x="top", legend=c("returns", "forecasts"),
 col=c("blue", "red"), lty=1, lwd=6,
 cex=0.9, bg="white", bty="n")
# Define backtesting function
sim_forecasts <- function(response, predictor=response, nagg=5,
                ordern=5, look_back=100) {
  nrows <- NROW(response)
  # Define predictor as a rolling sum
  predictor <- rutils::roll_sum(response, look_back=nagg)
  # Shift the response forward out-of-sample
  response <- rutils::lagit(predictor, lagg=(-nagg))
  # Define predictor matrix for forecasting
  predictor <- sapply(1+nagg*(0:ordern), rutils::lagit,
                 input=predictor)
  predictor <- cbind(rep(1, nrows), predictor)
  # Define design matrix
  design <- cbind(response, predictor)
  # Perform rolling forecasting
  forecasts <- sapply((look_back+1):nrows, function(endp) {
    # Define rolling look-back range
    startp <- max(1, endp-look_back)
    # Or expanding look-back range
    # startp <- 1
    rangev <- startp:(endp-1)
    # Invert the predictor matrix
    design_inv <- MASS::ginv(design[rangev, -1])
    # Calculate fitted coefficients
    coeff_fit <- drop(design_inv %*% design[rangev, 1])
    # Calculate forecast
    drop(design[endp, -1] %*% coeff_fit)
  })  # end sapply
  # Add warmup period
  forecasts <- c(rep(0, look_back), forecasts)
  rutils::roll_sum(forecasts, look_back=nagg)
}  # end sim_forecasts
# Simulate the rolling autoregressive forecasts
forecasts <- sim_forecasts(returns, ordern=5, look_back=100)
c(mse=mean((returns - forecasts)^2), cor=cor(returns, forecasts))
look_backs <- seq(20, 200, 20)
back_tests <- sapply(look_backs, back_test, se_ries=arimav, ordern=ordern)
back_tests <- t(back_tests)
rownames(back_tests) <- look_backs
# Plot forecasting series with legend
plot(x=look_backs, y=back_tests[, 1],
  xlab="look-back", ylab="MSE", type="l", lwd=2,
  main="MSE of AR(5) Forecasting Model")
# Calculate a vector of daily VTI log returns
vti <- na.omit(rutils::etfenv$returns$VTI)
dates <- index(vti)
vti <- as.numeric(vti)
nrows <- NROW(vti)
# Define predictor matrix for forecasting
order_max <- 5
predictor <- sapply(1:order_max, rutils::lagit, input=vti)
predictor <- cbind(rep(1, nrows), predictor)
colnames(predictor) <- paste0("pred_", 1:NCOL(predictor))
response <- vti
# Calculate forecasts as function of the AR order
forecasts <- lapply(2:NCOL(predictor), function(ordern) {
  # Calculate fitted coefficients
  inverse <- MASS::ginv(predictor[, 1:ordern])
  coeff_fit <- drop(inverse %*% response)
  # Calculate in-sample forecasts of vti
  drop(predictor[, 1:ordern] %*% coeff_fit)
})  # end lapply
names(forecasts) <- paste0("p=", 2:NCOL(predictor))
# Calculate mean squared errors
mse <- sapply(forecasts, function(x) {
  c(mse=mean((vti - x)^2), cor=cor(vti, x))
})  # end sapply
mse <- t(mse)
rownames(mse) <- names(forecasts)
# Plot forecasting MSE
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
plot(x=2:NCOL(predictor), y=mse[, 1],
  xlab="AR(n) order", ylab="MSE", type="l", lwd=2,
  main="MSE of In-sample AR(n) Forecasting Model for VTI")
in_sample <- 1:(nrows %/% 2)
out_sample <- (nrows %/% 2 + 1):nrows
# Calculate forecasts as function of the AR order
forecasts <- lapply(2:NCOL(predictor), function(ordern) {
  # Calculate fitted coefficients
  inverse <- MASS::ginv(predictor[in_sample, 1:ordern])
  coeff_fit <- drop(inverse %*% response[in_sample])
  # Calculate out-of-sample forecasts of vti
  drop(predictor[out_sample, 1:ordern] %*% coeff_fit)
})  # end lapply
names(forecasts) <- paste0("p=", 2:NCOL(predictor))
# Calculate mean squared errors
mse <- sapply(forecasts, function(x) {
  c(mse=mean((vti[out_sample] - x)^2), cor=cor(vti[out_sample], x))
})  # end sapply
mse <- t(mse)
rownames(mse) <- names(forecasts)
# Plot forecasting MSE
plot(x=2:NCOL(predictor), y=mse[, 1],
  xlab="AR(n) order", ylab="MSE", type="l", lwd=2,
  main="MSE of Out-of-sample AR(n) Forecasting Model for VTI")
# Calculate out-of-sample PnLs
pnls <- sapply(forecasts, function(x) {
  cumsum(sign(x)*vti[out_sample])
})  # end sapply
colnames(pnls) <- names(forecasts)
pnls <- xts::xts(pnls, dates[out_sample])
# Plot dygraph of out-of-sample PnLs
colorv <- colorRampPalette(c("red", "blue"))(NCOL(pnls[, 1:4]))
colnames <- colnames(pnls[, 1:4])
dygraphs::dygraph(pnls[, 1:4],
  main="Autoregressive Strategies Performance With Different Order Parameters") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(width=500)
# Define predictor as a rolling mean
nagg <- 5
predictor <- roll::roll_mean(vti, width=nagg, min_obs=1)
# Shift the response forward out-of-sample
response <- rutils::lagit(predictor, lagg=(-nagg))
# Define predictor matrix for forecasting
predictor <- sapply(1+nagg*(0:order_max), rutils::lagit,
               input=predictor)
predictor <- cbind(rep(1, nrows), predictor)
# Calculate forecasts as function of the AR order
forecasts <- lapply(2:NCOL(predictor), function(ordern) {
  inverse <- MASS::ginv(predictor[in_sample, 1:ordern])
  coeff_fit <- drop(inverse %*% response[in_sample])
  drop(predictor[out_sample, 1:ordern] %*% coeff_fit)
})  # end lapply
names(forecasts) <- paste0("p=", 2:NCOL(predictor))
# Calculate out-of-sample PnLs
pnls <- sapply(forecasts, function(x) {
  cumsum(sign(x)*vti[out_sample])
})  # end sapply
colnames(pnls) <- names(forecasts)
pnls <- xts::xts(pnls, dates[out_sample])
# Plot dygraph of out-of-sample PnLs
dygraphs::dygraph(pnls[, 1:4],
  main="Autoregressive Strategies Performance Using Rolling Average Predictor") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(width=500)
# Calculate out-of-sample PnLs
pnls <- sapply(forecasts, function(x) {
  x <- roll::roll_mean(x, width=nagg, min_obs=1)
  cumsum(sign(x)*vti[out_sample])
})  # end sapply
colnames(pnls) <- names(forecasts)
pnls <- xts::xts(pnls, dates[out_sample])
# Plot dygraph of out-of-sample PnLs
dygraphs::dygraph(pnls[, 1:4],
  main="Autoregressive Strategies Performance Using Rolling Average Forecasts") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(width=500)
# Calculate a vector of daily VTI log returns
vti <- na.omit(rutils::etfenv$returns$VTI)
dates <- index(vti)
vti <- as.numeric(vti)
nrows <- NROW(vti)
# Define predictor as a rolling mean
nagg <- 5
predictor <- roll::roll_mean(vti, width=nagg, min_obs=1)
# Shift the response forward out-of-sample
response <- rutils::lagit(predictor, lagg=(-nagg))
# Define predictor matrix for forecasting
order_max <- 5
predictor <- sapply(1+nagg*(0:order_max), rutils::lagit,
               input=predictor)
predictor <- cbind(rep(1, nrows), predictor)
# Define design matrix
design <- cbind(response, predictor)
# Perform rolling forecasting
look_back <- 100
forecasts <- sapply((look_back+1):nrows, function(endp) {
  # Define rolling look-back range
  startp <- max(1, endp-look_back)
  # Or expanding look-back range
  # startp <- 1
  rangev <- startp:(endp-1)
  # Invert the predictor matrix
  design_inv <- MASS::ginv(design[rangev, -1])
  # Calculate fitted coefficients
  coeff_fit <- drop(design_inv %*% design[rangev, 1])
  # Calculate forecast
  drop(design[endp, -1] %*% coeff_fit)
})  # end sapply
# Add warmup period
forecasts <- c(rep(0, look_back), forecasts)
# Mean squared error
mean((vti - forecasts)^2)
# Correlation
cor(forecasts, vti)
# Plot forecasting series with legend
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0))
plot(forecasts[(nrows-look_back):nrows], col="red",
     xlab="", ylab="", type="l", lwd=2,
     main="Rolling Forecasting Using AR Model")
lines(vti[(nrows-look_back):nrows], col="blue", lwd=2)
legend(x="top", legend=c("VTI returns", "forecasts"),
 col=c("blue", "red"), lty=1, lwd=6,
 cex=0.9, bg="white", bty="n")
# Define backtesting function
sim_forecasts <- function(response, predictor=response, nagg=5,
                ordern=5, look_back=100) {
  nrows <- NROW(response)
  # Define predictor as a rolling mean
  predictor <- roll::roll_mean(vti, width=nagg, min_obs=1)
  # Shift the response forward out-of-sample
  response <- rutils::lagit(predictor, lagg=(-nagg))
  # Define predictor matrix for forecasting
  predictor <- sapply(1+nagg*(0:ordern), rutils::lagit,
                 input=predictor)
  predictor <- cbind(rep(1, nrows), predictor)
  # Define design matrix
  design <- cbind(response, predictor)
  # Perform rolling forecasting
  forecasts <- sapply((look_back+1):nrows, function(endp) {
    # Define rolling look-back range
    startp <- max(1, endp-look_back)
    # Or expanding look-back range
    # startp <- 1
    rangev <- startp:(endp-1)
    # Invert the predictor matrix
    design_inv <- MASS::ginv(design[rangev, -1])
    # Calculate fitted coefficients
    coeff_fit <- drop(design_inv %*% design[rangev, 1])
    # Calculate forecast
    drop(design[endp, -1] %*% coeff_fit)
  })  # end sapply
  # Add warmup period
  forecasts <- c(rep(0, look_back), forecasts)
  roll::roll_mean(forecasts, width=nagg, min_obs=1)
}  # end sim_forecasts
# Simulate the rolling autoregressive forecasts
forecasts <- sim_forecasts(vti, ordern=5, look_back=100)
c(mse=mean((vti - forecasts)^2), cor=cor(vti, forecasts))
look_backs <- seq(20, 600, 40)
library(parallel)  # Load package parallel
# Calculate number of available cores
ncores <- detectCores() - 1
# Initialize compute cluster under Windows
cluster <- makeCluster(ncores)
# clusterExport(cluster, c("startd", "barp"))
# Perform parallel loop under Windows
forecasts <- parLapply(cluster, look_backs, sim_forecasts, response=vti,
                  predictor=vti, nagg=5, ordern=5)
# Perform parallel bootstrap under Mac-OSX or Linux
forecasts <- mclapply(look_backs, sim_forecasts, response=vti,
  predictor=vti, nagg=5, ordern=5, mc.cores=ncores)
# Calculate mean squared errors
mse <- sapply(forecasts, function(x) {
  c(mse=mean((vti - x)^2), cor=cor(vti, x))
})  # end sapply
mse <- t(mse)
rownames(mse) <- look_backs
# Select optimal look_back interval
look_back <- look_backs[which.min(mse[, 1])]
# Plot forecasting MSE
plot(x=look_backs, y=mse[, 1],
  xlab="look-back", ylab="MSE", type="l", lwd=2,
  main="MSE of AR Forecasting Model As Function of Look-back")
orders <- 2:6
library(parallel)  # Load package parallel
# Calculate number of available cores
ncores <- detectCores() - 1
# Initialize compute cluster under Windows
cluster <- makeCluster(ncores)
# clusterExport(cluster, c("startd", "barp"))
# Perform parallel loop under Windows
forecasts <- parLapply(cluster, orders, sim_forecasts, response=vti,
                  predictor=vti, nagg=5, look_back=look_back)
stopCluster(cluster)  # Stop R processes over cluster under Windows
# Perform parallel bootstrap under Mac-OSX or Linux
forecasts <- mclapply(orders, sim_forecasts, response=vti,
  predictor=vti, nagg=5, look_back=look_back, mc.cores=ncores)
stopCluster(cluster)  # Stop R processes over cluster under Windows
# Calculate mean squared errors
mse <- sapply(forecasts, function(x) {
  c(mse=mean((vti - x)^2), cor=cor(vti, x))
})  # end sapply
mse <- t(mse)
rownames(mse) <- orders
# Select optimal order parameter
ordern <- orders[which.min(mse[, 1])]
# Plot forecasting MSE
plot(x=orders, y=mse[, 1],
  xlab="ordern", ylab="MSE", type="l", lwd=2,
  main="MSE of Forecasting Model As Function of AR Order")
# Simulate the rolling autoregressive forecasts
forecasts <- sim_forecasts(vti, ordern=ordern, look_back=look_back)
# Calculate strategy PnLs
pnls <- sign(forecasts)*vti
pnls <- cbind(vti, pnls, (vti+pnls)/2)
colnames(pnls) <- c("VTI", "AR_Strategy", "Combined")
cor(pnls)
# Annualized Sharpe ratios of VTI and AR strategy
sqrt(252)*apply(pnls, 2, function (x) mean(x)/sd(x))
pnls <- xts::xts(pnls, dates)
pnls <- cumsum(pnls)
# Plot the cumulative strategy PnLs
dygraphs::dygraph(pnls, main="Rolling Autoregressive Strategy") %>%
  dyOptions(colors=c("blue","red","green"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Calculate PnLs for ordern=5
forecasts <- sim_forecasts(vti, ordern=5, look_back=look_back)
pnls5 <- cumsum(sign(forecasts)*vti)
# Calculate PnLs for ordern=3
forecasts <- sim_forecasts(vti, ordern=3, look_back=look_back)
pnls3 <- cumsum(sign(forecasts)*vti)
# Plot the cumulative strategy returns
wealth <- cbind(pnls5, pnls3)
wealth <- xts::xts(wealth, dates)
colnames <- c("AR(5)_Strategy", "AR(3)_Strategy")
colnames(wealth) <- colnames
dygraphs::dygraph(wealth, main="Autoregressive Strategies for Different Order Parameters") %>%
  dySeries(name=colnames[1], label=colnames[1], col="blue", strokeWidth=2) %>%
  dySeries(name=colnames[2], label=colnames[2], col="red", strokeWidth=2) %>%
  dyLegend(width=500)
