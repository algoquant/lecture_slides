# Calculate VTI and IEF dollar returns
prices <- rutils::etfenv$prices[, c("VTI", "IEF")]
prices <- na.omit(prices)
dates <- index(prices)
rets_dollar <- rutils::diffit(prices)
# Calculate VTI and IEF percentage returns
rets_percent <- rets_dollar/
  rutils::lagit(prices, lagg=1, pad_zeros=FALSE)
# Wealth of fixed shares (without rebalancing)
weightv <- c(0.5, 0.5)
rets_dollar[1, ] <- prices[1, ]
wealth_fsa <- cumsum(rets_dollar %*% weightv)
# Wealth of fixed dollars (with rebalancing)
wealth_fda <- cumsum(rets_percent %*% weightv)
# Plot log wealth
wealth <- cbind(wealth_fda, log(wealth_fsa))
wealth <- xts::xts(wealth, index(prices))
colnames(wealth) <- c("Fixed dollars", "Fixed shares (log)")
colnames <- colnames(wealth)
dygraphs::dygraph(wealth, main="Wealth of Weighted Portfolios") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", col="red", strokeWidth=2) %>%
  dySeries(name=colnames[2], axis="y2", col="blue", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Margin account for fixed dollars (with rebalancing)
mar_gin <- cumsum(rets_percent %*% weightv)
# Cumulative transaction costs
costs <- bid_offer*cumsum(abs(rets_percent) %*% weightv)/2
# Subtract transaction costs from margin account
mar_gin <- (mar_gin - costs)
# dygraph plot of margin and transaction costs
datav <- cbind(mar_gin, costs)
datav <- xts::xts(datav, index(prices))
colnames <- c("Margin", "Cumulative Transaction Costs")
colnames(datav) <- colnames
dygraphs::dygraph(datav, main="Fixed Dollar Portfolio Transaction Costs") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", col="blue") %>%
  dySeries(name=colnames[2], axis="y2", col="red", strokeWidth=3) %>%
  dyLegend(show="always", width=500)
# Wealth of fixed shares (without rebalancing)
wealth_fsa <- cumsum(rets_dollar %*% weightv)
# Calculate weighted percentage returns
rets_weighted <- rets_percent %*% weightv
# Wealth of fixed ratio of dollar amounts (with rebalancing)
wealth_cda <- cumprod(1 + rets_weighted)
wealth_cda <- wealth_fsa[1]*wealth_cda
# Plot log wealth
wealth <- log(cbind(wealth_fsa, wealth_cda))
wealth <- xts::xts(wealth, index(prices))
colnames(wealth) <- c("Fixed Shares", "Fixed Ratio")
dygraphs::dygraph(wealth, main="Log Wealth of Fixed Dollar Ratios") %>%
  dyOptions(colors=c("blue","red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Returns in excess of weighted returns
excess <- lapply(rets_percent, function(x) (rets_weighted - x))
excess <- do.call(cbind, excess)
sum(excess %*% weightv)
# Calculate weighted sum of absolute excess returns
excess <- abs(excess) %*% weightv
# Total dollar amount of stocks that need to be traded
excess <- excess*rutils::lagit(wealth_cda)
# Cumulative transaction costs
costs <- bid_offer*cumsum(excess)/2
# Subtract transaction costs from wealth
wealth_cda <- (wealth_cda - costs)
# dygraph plot of wealth and transaction costs
wealth <- cbind(wealth_cda, costs)
wealth <- xts::xts(wealth, index(prices))
colnames <- c("Wealth", "Cumulative Transaction Costs")
colnames(wealth) <- colnames
dygraphs::dygraph(wealth, main="Transaction Costs With Fixed Dollar Ratios") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", col="blue") %>%
  dySeries(name=colnames[2], axis="y2", col="red", strokeWidth=3) %>%
  dyLegend(show="always", width=500)
# Calculate stock and bond returns
returns <- na.omit(rutils::etfenv$returns[, c("VTI", "IEF")])
weightv <- c(0.4, 0.6)
returns <- cbind(returns, returns %*% weightv)
colnames(returns)[3] <- "Combined"
# Calculate correlations
cor(returns)
# Calculate Sharpe ratios
sqrt(252)*sapply(returns, function(x) mean(x)/sd(x))
# Calculate standard deviation, skewness, and kurtosis
sapply(returns, function(x) {
  # Calculate standard deviation
  stddev <- sd(x)
  # Standardize the returns
  x <- (x - mean(x))/stddev
  c(stddev=stddev, skew=mean(x^3), kurt=mean(x^4))
})  # end sapply
# Wealth of fixed ratio of dollar amounts
wealth <- cumprod(1 + returns)
# Plot cumulative wealth
dygraphs::dygraph(log(wealth), main="Stock and Bond Portfolio") %>%
  dyOptions(colors=c("blue","green","blue","red")) %>%
  dySeries("Combined", color="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Extract ETF returns
symbols <- c("VTI", "IEF", "DBC")
returns <- na.omit(rutils::etfenv$returns[, symbols])
# Calculate all-weather portfolio wealth
weightsaw <- c(0.30, 0.55, 0.15)
returns <- cbind(returns, returns %*% weightsaw)
colnames(returns)[4] <- "All Weather"
# Calculate cumulative wealth from returns
wealth <- cumsum(returns)
# dygraph all-weather wealth
dygraphs::dygraph(wealth, main="All-Weather Portfolio") %>%
  dyOptions(colors=c("blue", "green", "orange", "red")) %>%
  dySeries("All Weather", color="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Plot all-weather wealth
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue", "green", "red")
quantmod::chart_Series(wealth, theme=plot_theme, lwd=c(2, 2, 2, 4),
       name="All-Weather Portfolio")
legend("topleft", legend=colnames(wealth),
  inset=0.1, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")
# Calculate VTI returns
returns <- na.omit(rutils::etfenv$returns$VTI["2008/2009"])
dates <- index(returns)
nrows <- NROW(returns)
returns <- drop(zoo::coredata(returns))
bfloor <- 60  # bond floor
coeff <- 2  # multiplier
portf_value <- numeric(nrows)
portf_value[1] <- 100  # principal
stock_value <- numeric(nrows)
stock_value[1] <- coeff*(portf_value[1] - bfloor)
bond_value <- numeric(nrows)
bond_value[1] <- (portf_value[1] - stock_value[1])
# Simulate CPPI strategy
for (t in 2:nrows) {
  portf_value[t] <- portf_value[t-1] + stock_value[t-1]*returns[t]
  stock_value[t] <- coeff*(portf_value[t] - bfloor)
  bond_value[t] <- (portf_value[t] - stock_value[t])
}  # end for
# dygraph plot of CPPI strategy
vtis <- 100*cumprod(1+returns)
datav <- xts::xts(cbind(stock_value, bond_value, portf_value, vtis), dates)
colnames(datav) <- c("stocks", "bonds", "CPPI", "VTI")
dygraphs::dygraph(datav, main="CPPI strategy") %>%
  dyOptions(colors=c("red", "green","blue","orange"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Calculate dollar and percentage returns for VTI and IEF.
prices <- rutils::etfenv$prices[, c("VTI", "IEF")]
prices <- na.omit(prices)
rets_dollar <- rutils::diffit(prices)
rets_percent <- rets_dollar/rutils::lagit(prices, lagg=1, pad_zeros=FALSE)
# Calculate wealth of fixed ratio of dollar amounts.
weightv <- c(0.5, 0.5)
rets_weighted <- rets_percent %*% weightv
wealth_cda <- cumprod(1 + rets_weighted)
# Calculate rolling percentage volatility.
look_back <- 21
vo_l <- roll::roll_sd(rets_percent, width=look_back)
vo_l <- zoo::na.locf(vo_l, na.rm=FALSE)
vo_l <- zoo::na.locf(vo_l, fromLast=TRUE)
# Calculate the risk parity portfolio allocations.
allocation_s <- lapply(1:NCOL(prices),
  function(x) weightv[x]/vo_l[, x])
allocation_s <- do.call(cbind, allocation_s)
# Scale allocations to 1 dollar total.
allocation_s <- allocation_s/rowSums(allocation_s)
# Lag the allocations
allocation_s <- rutils::lagit(allocation_s)
# Calculate wealth of risk parity.
rets_weighted <- rowSums(rets_percent*allocation_s)
wealth_risk_parity <- cumprod(1 + rets_weighted)
# Calculate the log wealths.
wealth <- log(cbind(wealth_cda, wealth_risk_parity))
wealth <- xts::xts(wealth, index(prices))
colnames(wealth) <- c("Fixed Ratio", "Risk Parity")
# Calculate the Sharpe ratios.
sqrt(252)*sapply(rutils::diffit(wealth), function (x) mean(x)/sd(x))
# Plot a dygraph of the log wealths.
dygraphs::dygraph(wealth, main="Log Wealth of Risk Parity vs Fixed Dollar Ratios") %>%
  dyOptions(colors=c("blue","red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Test risk parity market timing of VTI using Treynor-Mazuy test
returns <- rutils::diffit(wealth)
vtis <- rets_percent$VTI
design <- cbind(returns, vtis, vtis^2)
design <- na.omit(design)
colnames(design)[1:2] <- c("fixed","risk_parity")
colnames(design)[4] <- "treynor"
model <- lm(risk_parity ~ VTI + treynor, data=design)
summary(model)
# Plot residual scatterplot
residuals <- (design$risk_parity - model$coeff[2]*design$VTI)
residuals <- model$residuals
x11(width=6, height=5)
plot.default(x=design$VTI, y=residuals, xlab="VTI", ylab="residuals")
title(main="Treynor-Mazuy Market Timing Test\n for Risk Parity vs VTI", line=0.5)
# Plot fitted (predicted) response values
fit_ted <- (model$coeff["(Intercept)"] +
        model$coeff["treynor"]*vtis^2)
points.default(x=design$VTI, y=fit_ted, pch=16, col="red")
text(x=0.05, y=0.025, paste("Risk Parity t-value =", round(summary(model)$coeff["treynor", "t value"], 2)))
# Test for fixed ratio market timing of VTI using Treynor-Mazuy test
model <- lm(fixed ~ VTI + treynor, data=design)
summary(model)
# Plot fitted (predicted) response values
fit_ted <- (model$coeff["(Intercept)"] + model$coeff["treynor"]*vtis^2)
points.default(x=design$VTI, y=fit_ted, pch=16, col="blue")
text(x=0.05, y=0.02, paste("Fixed Ratio t-value =", round(summary(model)$coeff["treynor", "t value"], 2)))
# Calculate positions
vtis <- na.omit(rutils::etfenv$returns$VTI)
position_s <- rep(NA_integer_, NROW(vtis))
dates <- index(vtis)
dates <- format(dates, "%m-%d")
position_s[dates == "05-01"] <- 0
position_s[dates == "05-03"] <- 0
position_s[dates == "11-01"] <- 1
position_s[dates == "11-03"] <- 1
# Carry forward and backward non-NA position_s
position_s <- zoo::na.locf(position_s, na.rm=FALSE)
position_s <- zoo::na.locf(position_s, fromLast=TRUE)
# Calculate strategy returns
sell_inmay <- position_s*vtis
wealth <- cbind(vtis, sell_inmay)
colnames(wealth) <- c("VTI", "sell_in_may")
# Calculate Sharpe and Sortino ratios
sqrt(252)*sapply(wealth,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot wealth of Sell in May strategy
dygraphs::dygraph(cumsum(wealth), main="Sell in May Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# OR: Open x11 for plotting
x11(width=6, height=5)
par(mar=c(4, 4, 3, 1), oma=c(0, 0, 0, 0))
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("blue", "red")
quantmod::chart_Series(wealth, theme=plot_theme, name="Sell in May Strategy")
legend("topleft", legend=colnames(wealth),
  inset=0.1, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")
# Test if Sell in May strategy can time VTI
design <- cbind(vtis, 0.5*(vtis+abs(vtis)), vtis^2)
colnames(design) <- c("VTI", "merton", "treynor")
# Perform Merton-Henriksson test
model <- lm(sell_inmay ~ VTI + merton, data=design)
summary(model)
# Perform Treynor-Mazuy test
model <- lm(sell_inmay ~ VTI + treynor, data=design)
summary(model)
# Plot Treynor-Mazuy residual scatterplot
residuals <- (sell_inmay - model$coeff[2]*vtis)
plot.default(x=vtis, y=residuals, xlab="VTI", ylab="residuals")
title(main="Treynor-Mazuy Market Timing Test\n for Sell in May vs VTI", line=0.5)
# Plot fitted (predicted) response values
fit_ted <- (model$coeff["(Intercept)"] +
        model$coeff["treynor"]*vtis^2)
points.default(x=vtis, y=fit_ted, pch=16, col="red")
text(x=0.05, y=0.05, paste("Treynor test t-value =", round(summary(model)$coeff["treynor", "t value"], 2)))
# Calculate the log of OHLC VTI prices
ohlc <- log(rutils::etfenv$VTI)
openp <- quantmod::Op(ohlc)
highp <- quantmod::Hi(ohlc)
lowp <- quantmod::Lo(ohlc)
closep <- quantmod::Cl(ohlc)
# Calculate the close-to-close log returns, the intraday
# open-to-close returns and the overnight close-to-open returns.
close_close <- rutils::diffit(closep)
colnames(close_close) <- "close_close"
open_close <- (closep - openp)
colnames(open_close) <- "open_close"
close_open <- (openp - rutils::lagit(closep, lagg=1, pad_zeros=FALSE))
colnames(close_open) <- "close_open"
# Calculate Sharpe and Sortino ratios
wealth <- cbind(close_close, close_open, open_close)
sqrt(252)*sapply(wealth,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot log wealth
dygraphs::dygraph(cumsum(wealth),
  main="Wealth of Close-to-Close, Close-to-Open, and Open-to-Close Strategies") %>%
  dySeries(name="close_close", label="Close-to-Close (static)", strokeWidth=2, col="blue") %>%
  dySeries(name="close_open", label="Close-to-Open (overnight)", strokeWidth=2, col="red") %>%
  dySeries(name="open_close", label="Open-to-Close (daytime)", strokeWidth=2, col="green") %>%
  dyLegend(width=600)
# Calculate the VTI returns
vtis <- na.omit(rutils::etfenv$returns$VTI)
dates <- zoo::index(vtis)
# Calculate first business day of every month
day_s <- as.numeric(format(dates, "%d"))
indeks <- which(rutils::diffit(day_s) < 0)
dates[head(indeks)]
# Calculate Turn of the Month dates
indeks <- lapply((-1):2, function(x) indeks + x)
indeks <- do.call(c, indeks)
sum(indeks > NROW(dates))
indeks <- sort(indeks)
dates[head(indeks, 11)]
# Calculate Turn of the Month pnls
pnls <- numeric(NROW(vtis))
pnls[indeks] <- vtis[indeks, ]
# Combine data
wealth <- cbind(vtis, pnls)
colnames <- c("VTI", "Strategy")
colnames(wealth) <- colnames
# Calculate Sharpe and Sortino ratios
sqrt(252)*sapply(wealth,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# dygraph plot VTI Turn of the Month strategy
dygraphs::dygraph(cumsum(wealth), main="Turn of the Month Strategy") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=colnames[2], axis="y2", strokeWidth=2, col="red")
# Calculate the VTI returns
vtis <- na.omit(rutils::etfenv$returns$VTI)
dates <- zoo::index(vtis)
vtis <- drop(coredata(vtis))
nrows <- NROW(vtis)
# Simulate stop-loss strategy
sto_p <- 0.05
ma_x <- 0.0
cum_ret <- 0.0
pnls <- vtis
for (i in 1:nrows) {
# Calculate drawdown
  cum_ret <- cum_ret + vtis[i]
  ma_x <- max(ma_x, cum_ret)
  dd <- (cum_ret - ma_x)
# Check for stop-loss
  if (dd < -sto_p*ma_x)
    pnls[i+1] <- 0
}  # end for
# Same but without using explicit loops
cumsumv <- cumsum(vtis)
cum_max <- cummax(cumsum(vtis))
dd <- (cumsumv - cum_max)
pnls2 <- vtis
is_dd <- rutils::lagit(dd < -sto_p*cum_max)
pnls2 <- ifelse(is_dd, 0, pnls2)
all.equal(pnls, pnls2)
# Combine data
wealth <- xts::xts(cbind(vtis, pnls), dates)
colnames <- c("VTI", "Strategy")
colnames(wealth) <- colnames
# Calculate Sharpe and Sortino ratios
sqrt(252)*sapply(wealth,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# dygraph plot VTI stop-loss strategy
dygraphs::dygraph(cumsum(wealth), main="VTI Stop-loss Strategy") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=colnames[2], axis="y2", strokeWidth=2, col="red")
# Simulate multiple stop-loss strategies
cumsumv <- cumsum(vtis)
cum_max <- cummax(cumsum(vtis))
dd <- (cumsumv - cum_max)
cum_pnls <- sapply(0.01*(1:20), function(sto_p) {
  pnls <- vtis
  is_dd <- rutils::lagit(dd < -sto_p*cum_max)
  pnls <- ifelse(is_dd, 0, pnls)
  sum(pnls)
})  # end sapply
# Plot cumulative pnls for stop-loss strategies
plot(x=0.01*(1:20), y=cum_pnls,
     main="Cumulative PnLs for Stop-loss Strategies",
     xlab="stop-loss level", ylab="cumulative pnl",
     t="l", lwd=3, col="blue")
# Extract time series of VTI log prices
closep <- log(na.omit(rutils::etfenv$prices$VTI))
# Inspect the R code of the function filter()
filter
# Calculate EWMA weightv
look_back <- 21
weightv <- exp(-0.1*1:look_back)
weightv <- weightv/sum(weightv)
# Calculate convolution using filter()
filtered <- filter(closep, filter=weightv,
              method="convolution", sides=1)
# filter() returns time series of class "ts"
class(filtered)
# Get information about C_cfilter()
getAnywhere(C_cfilter)
# Filter using C_cfilter() over past values (sides=1).
filter_fast <- .Call(stats:::C_cfilter, closep, filter=weightv,
               sides=1, circular=FALSE)
all.equal(as.numeric(filtered), filter_fast, check.attributes=FALSE)
# Calculate EWMA prices using roll::roll_sum()
weights_rev <- rev(weightv)
roll_ed <- roll::roll_sum(closep, width=look_back, weights=weights_rev, min_obs=1)
all.equal(filter_fast[-(1:look_back)], as.numeric(roll_ed)[-(1:look_back)])
# Benchmark speed of rolling calculations
library(microbenchmark)
summary(microbenchmark(
  filter=filter(closep, filter=weightv, method="convolution", sides=1),
  filter_fast=.Call(stats:::C_cfilter, closep, filter=weightv, sides=1, circular=FALSE),
  roll=roll::roll_sum(closep, width=look_back, weights=weights_rev)
  ), times=10)[, c(1, 4, 5)]
# Simulate AR process using filter()
nrows <- NROW(closep)
# Calculate ARIMA coefficients and innovations
coeff <- weightv/4
n_coeff <- NROW(coeff)
innov <- rnorm(nrows)
arimav <- filter(x=innov, filter=coeff, method="recursive")
# Get information about C_rfilter()
getAnywhere(C_rfilter)
# Filter using C_rfilter() compiled C++ function directly
arima_fast <- .Call(stats:::C_rfilter, innov, coeff,
              double(n_coeff + nrows))
all.equal(as.numeric(arimav), arima_fast[-(1:n_coeff)],
    check.attributes=FALSE)
# Filter using C++ code
arima_fastest <- HighFreq::sim_arima(innov, rev(coeff))
all.equal(arima_fast[-(1:n_coeff)], drop(arima_fastest))
# Benchmark speed of the three methods
summary(microbenchmark(
  filter=filter(x=innov, filter=coeff, method="recursive"),
  filter_fast=.Call(stats:::C_rfilter, innov, coeff, double(n_coeff + nrows)),
  Rcpp=HighFreq::sim_arima(innov, rev(coeff))
  ), times=10)[, c(1, 4, 5)]
# Calculate trailing EWMA prices using roll::roll_sum()
look_back <- 21
weightv <- exp(-0.1*1:look_back)
weightv <- weightv/sum(weightv)
weights_rev <- rev(weightv)
filtered <- roll::roll_sum(closep, width=NROW(weightv), weights=weights_rev)
# Copy warmup period
filtered[1:look_back] <- closep[1:look_back]
# Combine prices with smoothed prices
prices <- cbind(closep, filtered)
colnames(prices)[2] <- "VTI Smooth"
# Calculate standard deviations of returns
sapply(rutils::diffit(prices), sd)
# Plot dygraph
dygraphs::dygraph(prices["2009"], main="VTI Prices and Trailing Smoothed Prices") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2)
# Calculate centered EWMA prices using roll::roll_sum()
weightv <- c(weights_rev, weightv[-1])
weightv <- weightv/sum(weightv)
filtered <- roll::roll_sum(closep, width=NROW(weightv), weights=weightv, online=FALSE)
# Copy warmup period
filtered[1:(2*look_back)] <- closep[1:(2*look_back)]
# Center the data
filtered <- rutils::lagit(filtered, -(look_back-1), pad_zeros=FALSE)
# Combine prices with smoothed prices
prices <- cbind(closep, filtered)
colnames(prices)[2] <- "VTI Smooth"
# Calculate standard deviations of returns
sapply(rutils::diffit(prices), sd)
# Plot dygraph
dygraphs::dygraph(prices["2009"], main="VTI Prices and Centered Smoothed Prices") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2)
# Open plot window
x11(width=6, height=7)
# Set plot parameters
par(oma=c(1, 1, 0, 1), mar=c(1, 1, 1, 1), mgp=c(0, 0.5, 0),
    cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# Set two plot panels
par(mfrow=c(2,1))
# Plot ACF of VTI returns
rutils::plot_acf(returns[, 1], lag=10, xlab="")
title(main="ACF of VTI Returns", line=-1)
# Plot ACF of smoothed VTI returns
rutils::plot_acf(returns[, 2], lag=10, xlab="")
title(main="ACF of Smoothed VTI Returns", line=-1)
# Extract log VTI prices
closep <- log(na.omit(rutils::etfenv$prices$VTI))
nrows <- NROW(closep)
# Calculate EWMA weights
look_back <- 21
lambda <- 0.1
weightv <- exp(lambda*1:look_back)
weightv <- weightv/sum(weightv)
# Calculate EWMA prices
ew_ma <- roll::roll_sum(closep, width=look_back, weights=weightv, min_obs=1)
# Copy over NA values
ew_ma <- zoo::na.locf(ew_ma, fromLast=TRUE)
prices <- cbind(closep, ew_ma)
colnames(prices) <- c("VTI", "VTI EWMA")
# Dygraphs plot with custom line colors
colors <- c("blue", "red")
dygraphs::dygraph(prices["2009"], main="VTI EWMA Prices") %>%
  dyOptions(colors=colors, strokeWidth=2)
# Plot EWMA prices with custom line colors
x11(width=6, height=5)
plot_theme <- chart_theme()
plot_theme$col$line.col <- colors
quantmod::chart_Series(prices["2009"], theme=plot_theme,
       lwd=2, name="VTI EWMA Prices")
legend("bottomright", legend=colnames(prices),
 inset=0.1, bg="white", lty=1, lwd=6, cex=0.8,
 col=plot_theme$col$line.col, bty="n")
# Calculate log OHLC prices and volumes
symbol <- "VTI"
ohlc <- rutils::etfenv$VTI
nrows <- NROW(ohlc)
closep <- log(quantmod::Cl(ohlc))
volumes <- quantmod::Vo(ohlc)
# Calculate the VWAP prices
look_back <- 21
vwapv <- roll::roll_sum(closep*volumes, width=look_back, min_obs=1)
volume_roll <- roll::roll_sum(volumes, width=look_back, min_obs=1)
vwapv <- vwapv/volume_roll
vwapv <- zoo::na.locf(vwapv, fromLast=TRUE)
prices <- cbind(closep, vwapv)
colnames(prices) <- c(symbol, paste(symbol, "VWAP"))
# Dygraphs plot with custom line colors
colors <- c("blue", "red")
dygraphs::dygraph(prices["2009"], main="VTI VWAP Prices") %>%
  dyOptions(colors=colors, strokeWidth=2)
# Plot VWAP prices with custom line colors
x11(width=6, height=5)
plot_theme <- chart_theme()
plot_theme$col$line.col <- colors
quantmod::chart_Series(prices["2009"], theme=plot_theme,
       lwd=2, name="VTI VWAP Prices")
legend("bottomright", legend=colnames(prices),
 inset=0.1, bg="white", lty=1, lwd=6, cex=0.8,
 col=plot_theme$col$line.col, bty="n")
# Calculate two EWMA prices
look_back <- 21
lambda <- 0.1
weightv <- exp(lambda*1:look_back)
weightv <- weightv/sum(weightv)
ewma_fast <- roll::roll_sum(closep, width=look_back, weights=weightv, min_obs=1)
lambda <- 0.05
weightv <- exp(lambda*1:look_back)
weightv <- weightv/sum(weightv)
ewma_slow <- roll::roll_sum(closep, width=look_back, weights=weightv, min_obs=1)
# Calculate VTI returns
returns <- (ewma_fast - ewma_slow)
prices <- cbind(closep, returns)
colnames(prices) <- c(symbol, paste(symbol, "Returns"))
# Plot dygraph of VTI Returns
colnames <- colnames(prices)
dygraphs::dygraph(prices["2009"], main=paste(symbol, "EWMA Returns")) %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", label=colnames[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnames[2], axis="y2", label=colnames[2], strokeWidth=2, col="red")
# Calculate fractional weights
del_ta <- 0.1
weightv <- (del_ta - 0:(look_back-2)) / 1:(look_back-1)
weightv <- (-1)^(1:(look_back-1))*cumprod(weightv)
weightv <- c(1, weightv)
weightv <- (weightv - mean(weightv))
weightv <- rev(weightv)
# Calculate fractional VTI returns
returns <- roll::roll_sum(closep, width=look_back, weights=weightv, min_obs=1, online=FALSE)
prices <- cbind(closep, returns)
colnames(prices) <- c(symbol, paste(symbol, "Returns"))
# Plot dygraph of VTI Returns
colnames <- colnames(prices)
dygraphs::dygraph(prices["2009"], main=paste(symbol, "Fractional Returns")) %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", label=colnames[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnames[2], axis="y2", label=colnames[2], strokeWidth=2, col="red")
# Calculate VTI log returns
closep <- log(quantmod::Cl(rutils::etfenv$VTI))
returns <- rutils::diffit(closep)
# Perform ADF test for prices
tseries::adf.test(closep)
# Perform ADF test for returns
tseries::adf.test(returns)
# Calculate fractional VTI returns
delta_s <- 0.1*c(1, 3, 5, 7, 9)
returns <- lapply(delta_s, function(del_ta) {
  weightv <- (del_ta - 0:(look_back-2)) / 1:(look_back-1)
  weightv <- c(1, (-1)^(1:(look_back-1))*cumprod(weightv))
  weightv <- rev(weightv - mean(weightv))
  roll::roll_sum(closep, width=look_back, weights=weightv, min_obs=1, online=FALSE)
})  # end lapply
returns <- do.call(cbind, returns)
returns <- cbind(closep, returns)
colnames(returns) <- c("VTI", paste0("frac_", delta_s))
# Calculate ADF test statistics
adfstats <- sapply(returns, function(x)
  suppressWarnings(tseries::adf.test(x)$statistic)
)  # end sapply
names(adfstats) <- colnames(returns)
# Plot dygraph of fractional VTI returns
colorv <- colorRampPalette(c("blue", "red"))(NCOL(returns))
colnames <- colnames(returns)
dyplot <- dygraphs::dygraph(returns["2019"], main="Fractional Returns") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", label=colnames[1], strokeWidth=2, col=colorv[1])
for (i in 2:NROW(colnames))
  dyplot <- dyplot %>%
  dyAxis("y2", label=colnames[i], independentTicks=TRUE) %>%
  dySeries(name=colnames[i], axis="y2", label=colnames[i], strokeWidth=2, col=colorv[i])
dyplot <- dyplot %>% dyLegend(width=500)
dyplot
# Calculate volume z-scores
volumes <- quantmod::Vo(rutils::etfenv$VTI)
look_back <- 21
volume_mean <- roll::roll_mean(volumes, width=look_back, min_obs=1)
volume_sd <- roll::roll_sd(rutils::diffit(volumes), width=look_back, min_obs=1)
volume_scores <- (volumes - volume_mean)/volume_sd
# Plot histogram of volume z-scores
x11(width=6, height=5)
hist(volume_scores, breaks=1e2)
# Plot dygraph of volume z-scores of VTI prices
prices <- cbind(closep, volume_scores)
colnames(prices) <- c("VTI", "Z-scores")
colnames <- colnames(prices)
dygraphs::dygraph(prices["2009"], main="VTI Volume Z-Scores") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", label=colnames[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnames[2], axis="y2", label=colnames[2], strokeWidth=2, col="red")
# Extract VTI log OHLC prices
ohlc <- log(rutils::etfenv$VTI)
# Calculate volatility z-scores
volat <- quantmod::Hi(ohlc)-quantmod::Lo(ohlc)
look_back <- 21
volat_mean <- roll::roll_mean(volat, width=look_back, min_obs=1)
volat_sd <- roll::roll_sd(rutils::diffit(volat), width=look_back, min_obs=1)
volat_scores <- ifelse(is.na(volat_sd), 0, (volat - volat_mean)/volat_sd)
# Plot histogram of volatility z-scores
x11(width=6, height=5)
hist(volat_scores, breaks=1e2)
# Plot scatterplot of volume and volatility z-scores
plot(as.numeric(volat_scores), as.numeric(volume_scores),
     xlab="volatility z-score", ylab="volume z-score")
# Plot dygraph of VTI volatility z-scores
closep <- quantmod::Cl(ohlc)
prices <- cbind(closep, volat_scores)
colnames(prices) <- c("VTI", "Z-scores")
colnames <- colnames(prices)
dygraphs::dygraph(prices["2009"], main="VTI Volatility Z-Scores") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", label=colnames[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnames[2], axis="y2", label=colnames[2], strokeWidth=2, col="red")
# Calculate the centered volatility
look_back <- 21
half_back <- look_back %/% 2
volat <- roll::roll_sd(returns, width=look_back, min_obs=1)
volat <- rutils::lagit(volat, lagg=(-half_back))
# Calculate the z-scores of prices
pricescores <- (2*closep -
  rutils::lagit(closep, half_back, pad_zeros=FALSE) -
  rutils::lagit(closep, -half_back, pad_zeros=FALSE))
pricescores <- ifelse(volat > 0, pricescores/volat, 0)
# Plot dygraph of z-scores of VTI prices
prices <- cbind(closep, pricescores)
colnames(prices) <- c("VTI", "Z-scores")
colnames <- colnames(prices)
dygraphs::dygraph(prices["2009"], main="VTI Price Z-Scores") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", label=colnames[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnames[2], axis="y2", label=colnames[2], strokeWidth=2, col="red")
# Calculate thresholds for labeling tops and bottoms
threshold_s <- quantile(pricescores, c(0.1, 0.9))
# Calculate the vectors of tops and bottoms
top_s <- (pricescores > threshold_s[2])
colnames(top_s) <- "tops"
bottom_s <- (pricescores < threshold_s[1])
colnames(bottom_s) <- "bottoms"
# Backtest in-sample VTI strategy
position_s <- rep(NA_integer_, NROW(returns))
position_s[1] <- 0
position_s[top_s] <- (-1)
position_s[bottom_s] <- 1
position_s <- zoo::na.locf(position_s)
position_s <- rutils::lagit(position_s)
pnls <- cumsum(returns*position_s)
# Plot dygraph of in-sample VTI strategy
prices <- cbind(closep, pnls)
colnames(prices) <- c("VTI", "Strategy")
colnames <- colnames(prices)
dygraphs::dygraph(prices, main="VTI Strategy Using In-sample Labels") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", label=colnames[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnames[2], axis="y2", label=colnames[2], strokeWidth=2, col="red")
# Calculate trailing price z-scores
dates <- matrix(as.numeric(zoo::index(closep)))
look_back <- 21
pricescores <- drop(HighFreq::roll_zscores(response=closep, design=dates, look_back=look_back))
pricescores[1:look_back] <- 0
# Plot dygraph of z-scores of VTI prices
prices <- cbind(closep, pricescores)
colnames(prices) <- c("VTI", "Z-scores")
colnames <- colnames(prices)
dygraphs::dygraph(prices["2009"], main="VTI Price Z-Scores") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", label=colnames[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnames[2], axis="y2", label=colnames[2], strokeWidth=2, col="red")
# Extract time series of VTI log prices
closep <- log(na.omit(rutils::etfenv$prices$VTI))
# Define look-back window and a half window
look_back <- 11
# Calculate time series of medians
medi_an <- roll::roll_median(closep, width=look_back)
# medi_an <- TTR::runMedian(closep, n=look_back)
# Calculate time series of MAD
madv <- HighFreq::roll_var(closep, look_back=look_back, method="quantile")
# madv <- TTR::runMAD(closep, n=look_back)
# Calculate time series of z-scores
zscores <- (closep - medi_an)/madv
zscores[1:look_back, ] <- 0
tail(zscores, look_back)
range(zscores)
x11(width=6, height=5)
# Plot prices and medians
dygraphs::dygraph(cbind(closep, medi_an), main="VTI median") %>%
  dyOptions(colors=c("black", "red"))
# Plot histogram of z-scores
histo_gram <- hist(zscores, col="lightgrey",
  xlab="z-scores", breaks=50, xlim=c(-4, 4),
  ylab="frequency", freq=FALSE, main="Hampel Z-scores histogram")
lines(density(zscores, adjust=1.5), lwd=3, col="blue")
# Calculate one-sided Hampel z-scores
medi_an <- roll::roll_median(closep, width=look_back)
# medi_an <- TTR::runMedian(closep, n=look_back)
madv <- HighFreq::roll_var(closep, look_back=look_back, method="quantile")
# madv <- TTR::runMAD(closep, n=look_back)
zscores <- (closep - medi_an)/madv
zscores[1:look_back, ] <- 0
tail(zscores, look_back)
range(zscores)
# Calculate two-sided Hampel z-scores
half_back <- look_back %/% 2
medi_an <- rutils::lagit(medi_an, lagg=-half_back)
madv <- rutils::lagit(madv, lagg=-half_back)
zscores <- (closep - medi_an)/madv
zscores[1:look_back, ] <- 0
tail(zscores, look_back)
range(zscores)
# Calculate VTI percentage returns
returns <- rutils::diffit(closep)
# Define threshold value
threshold <- sum(abs(range(zscores)))/8
# Backtest VTI strategy
position_s <- rep(NA_integer_, NROW(closep))
position_s[1] <- 0
position_s[zscores < -threshold] <- 1
position_s[zscores > threshold] <- (-1)
position_s <- zoo::na.locf(position_s)
position_s <- rutils::lagit(position_s)
pnls <- cumsum(returns*position_s)
# Plot dygraph of in-sample VTI strategy
prices <- cbind(closep, pnls)
colnames(prices) <- c("VTI", "Strategy")
colnames <- colnames(prices)
dygraphs::dygraph(prices, main="VTI Hampel Strategy") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", label=colnames[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnames[2], axis="y2", label=colnames[2], strokeWidth=2, col="red")
library(HighFreq)
# Read TAQ trade data from csv file
taq <- data.table::fread(file="/Volumes/external/Develop/data/xlk_tick_trades2020_0316.csv")
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
save(xtes, file="C:/Develop/data/xlk_tick_trades2020_0316.RData")
# Plot dygraph
dygraphs::dygraph(xtes$XLK.Close,
  main="XLK Trade Ticks for 2020-03-16")
# Plot in x11 window
x11(width=6, height=5)
quantmod::chart_Series(x=xtes$XLK.Close,
  name="XLK Trade Ticks for 2020-03-16")
# Calculate centered Hampel filter to remove price jumps
look_back <- 111
half_back <- look_back %/% 2
medi_an <- roll::roll_median(taq$price, width=look_back)
# medi_an <- TTR::runMedian(taq$price, n=look_back)
medi_an <- rutils::lagit(medi_an, lagg=-half_back, pad_zeros=FALSE)
madv <- HighFreq::roll_var(matrix(taq$price), look_back=look_back, method="quantile")
# madv <- TTR::runMAD(taq$price, n=look_back)
madv <- rutils::lagit(madv, lagg=-half_back, pad_zeros=FALSE)
# Calculate Z-scores
zscores <- (taq$price - medi_an)/madv
zscores[is.na(zscores)] <- 0
zscores[!is.finite(zscores)] <- 0
sum(is.na(zscores))
sum(!is.finite(zscores))
range(zscores); mad(zscores)
hist(zscores, breaks=2000, xlim=c(-5*mad(zscores), 5*mad(zscores)))
# Define discrimination threshold value
threshold <- 6*mad(zscores)
# Remove price jumps with large z-scores
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
# Define discrimination threshold value
threshold <- 6*mad(zscores)
# Calculate number of prices classified as bad data
is_bad <- (abs(zscores) > threshold)
sum(is_bad)
# Add 200 random price jumps into prices
set.seed(1121)
n_bad <- 200
is_jump <- logical(NROW(xtes))
is_jump[sample(x=NROW(is_jump), size=n_bad)] <- TRUE
xtes$XLK.Close[is_jump] <- xtes$XLK.Close[is_jump]*
  sample(c(0.95, 1.05), size=n_bad, replace=TRUE)
# Plot prices and medians
dygraphs::dygraph(cbind(xtes, medi_an), main="VTI median") %>%
  dyOptions(colors=c("black", "red"))
# Calculate time series of z-scores
medi_an <- roll::roll_median(xtes, width=look_back)
# medi_an <- TTR::runMedian(xtes, n=look_back)
madv <- HighFreq::roll_var(xtes, look_back=look_back, method="quantile")
# madv <- TTR::runMAD(xtes, n=look_back)
zscores <- ifelse(madv > 0, (xtes - medi_an)/madv, 0)
zscores[1:look_back, ] <- 0
# Calculate number of prices classified as bad data
is_bad <- (abs(zscores) > threshold)
sum(is_bad)
# Calculate confusion matrix
table(actual=!is_jump, forecast=!is_bad)
sum(is_bad)
# FALSE positive (type I error)
sum(!is_jump & is_bad)
# FALSE negative (type II error)
sum(is_jump & !is_bad)
confu_sion <- table(!is_jump, !(abs(zscores) > threshold))
# Confusion matrix as function of threshold
con_fuse <- function(actu_al, zscores, threshold) {
    confu_sion <- table(!actu_al, !(abs(zscores) > threshold))
    confu_sion <- confu_sion / rowSums(confu_sion)
    c(typeI=confu_sion[2, 1], typeII=confu_sion[1, 2])
  }  # end con_fuse
con_fuse(is_jump, zscores, threshold=threshold)
# Define vector of discrimination thresholds
threshold_s <- seq(from=10.0, to=25.0, by=0.2)
# Calculate error rates
error_rates <- sapply(threshold_s, con_fuse,
  actu_al=is_jump, zscores=zscores)  # end sapply
error_rates <- t(error_rates)
rownames(error_rates) <- threshold_s
error_rates <- rbind(c(1, 0), error_rates)
error_rates <- rbind(error_rates, c(0, 1))
# Calculate area under ROC curve (AUC)
true_pos <- (1 - error_rates[, "typeII"])
true_pos <- (true_pos + rutils::lagit(true_pos))/2
false_pos <- rutils::diffit(error_rates[, "typeI"])
abs(sum(true_pos*false_pos))
# Plot ROC curve for Hampel classifier
x11(width=5, height=5)
plot(x=error_rates[, "typeI"], y=1-error_rates[, "typeII"],
     xlab="FALSE positive rate", ylab="TRUE positive rate",
     xlim=c(0, 1), ylim=c(0, 1),
     main="ROC Curve for Hampel Classifier",
     type="l", lwd=3, col="blue")
abline(a=0.0, b=1.0, lwd=3, col="orange")
# Calculate centered Hampel filter over 3 data points
medi_an <- roll::roll_median(taq$price, width=3)
medi_an[1:2] <- taq$price[1:2]
medi_an <- rutils::lagit(medi_an, lagg=-1, pad_zeros=FALSE)
madv <- HighFreq::roll_var(matrix(taq$price), look_back=look_back, method="quantile")
madv <- rutils::lagit(madv, lagg=-1, pad_zeros=FALSE)
# Calculate Z-scores
zscores <- ifelse(madv > 0, (taq$price - medi_an)/madv, 0)
range(zscores); mad(zscores)
madv <- mad(zscores[abs(zscores)>0])
hist(zscores, breaks=2000, xlim=c(-5*madv, 5*madv))
# Define discrimination threshold value
threshold <- 6*madv
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
