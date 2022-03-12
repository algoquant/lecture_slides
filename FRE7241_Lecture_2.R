library(PerformanceAnalytics)
returns <- rutils::etfenv$returns[, c("VTI", "IEF")]
returns <- na.omit(returns)
# Calculate the Sharpe ratio
confl <- 0.05
PerformanceAnalytics::SharpeRatio(returns, p=(1-confl),
  method="historical")
# Calculate the Sortino ratio
PerformanceAnalytics::SortinoRatio(returns)
# Calculate the Calmar ratio
PerformanceAnalytics::CalmarRatio(returns)
# Calculate the Dowd ratio
PerformanceAnalytics::SharpeRatio(returns, FUN="VaR",
  p=(1-confl), method="historical")
# Calculate the Dowd ratio from scratch
va_r <- sapply(returns, quantile, probs=confl)
-sapply(returns, mean)/va_r
# Calculate the Conditional Dowd ratio
PerformanceAnalytics::SharpeRatio(returns, FUN="ES",
  p=(1-confl), method="historical")
# Calculate the Conditional Dowd ratio from scratch
c_var <- sapply(returns, function(x) {
  mean(x[x < quantile(x, confl)])
})
-sapply(returns, mean)/c_var
# Calculate VTI percentage returns
returns <- na.omit(rutils::etfenv$returns$VTI)
returns <- drop(zoo::coredata(returns))
nrows <- NROW(returns)
# Calculate aggregated VTI returns
nagg <- 252
agg_rets <- sapply(1:nrows, function(x) {
    sum(returns[sample.int(nrows, size=nagg, replace=TRUE)])
})  # end sapply
mean(returns)
mean(agg_rets)/nagg
# Calculate standard deviation, skewness, and kurtosis
datav <- cbind(returns, agg_rets)
colnames(datav) <- c("VTI", "Agg")
apply(datav, MARGIN=2, function(x) {
  # Calculate standard deviation
  stddev <- sd(x)
  # Standardize the returns
  x <- (x - mean(x))/stddev
  c(stddev=stddev, skew=mean(x^3), kurt=mean(x^4))
})  # end sapply
# Calculate the Sharpe ratios
confl <- 0.05
stdev <- sd(returns)
va_r <- unname(quantile(returns, probs=confl))
c_var <- mean(returns[returns < va_r])
sqrt(252)*mean(returns)/c(Sharpe=stdev, Dowd=-va_r, DowdC=-c_var)
# Calculate the Sharpe ratios of aggregated returns
stdev <- sd(agg_rets)
va_r <- unname(quantile(agg_rets, probs=confl))
c_var <- mean(agg_rets[agg_rets < va_r])
sqrt(252/nagg)*mean(agg_rets)/c(Sharpe=stdev, Dowd=-va_r, DowdC=-c_var)
# Test if IEF can time VTI
returns <- na.omit(rutils::etfenv$returns[, c("IEF", "VTI")])
vtis <- returns$VTI
design <- cbind(returns, 0.5*(vtis+abs(vtis)), vtis^2)
colnames(design)[3:4] <- c("merton", "treynor")
# Merton-Henriksson test
model <- lm(IEF ~ VTI + merton, data=design); summary(model)
# Treynor-Mazuy test
model <- lm(IEF ~ VTI + treynor, data=design); summary(model)
# Plot residual scatterplot
x11(width=6, height=5)
residuals <- (design$IEF - model$coeff[2]*design$VTI)
plot.default(x=design$VTI, y=residuals, xlab="VTI", ylab="IEF")
title(main="Treynor-Mazuy Market Timing Test\n for IEF vs VTI", line=0.5)
# Plot fitted (predicted) response values
fit_ted <- (model$coeff["(Intercept)"] +
        model$coeff["treynor"]*vtis^2)
points.default(x=design$VTI, y=fit_ted, pch=16, col="red")
text(x=0.05, y=0.03, paste("Treynor test t-value =", round(summary(model)$coeff["treynor", "t value"], 2)))
library(rutils)
# Extract ETF prices from rutils::etfenv$prices
prices <- rutils::etfenv$prices
prices <- zoo::na.locf(prices, na.rm=FALSE)
prices <- zoo::na.locf(prices, fromLast=TRUE)
# Calculate simple dollar returns
rets_dollar <- rutils::diffit(prices)
# Or
# rets_dollar <- lapply(prices, rutils::diffit)
# rets_dollar <- rutils::do_call(cbind, rets_dollar)
# Calculate log returns
rets_log <- rutils::diffit(log(prices))
# Calculate percentage returns
rets_percent <- rets_dollar/
  rutils::lagit(prices, lagg=1, pad_zeros=FALSE)
# Calculate prices from simple dollar returns
rets_dollar[1, ] <- prices[1, ]
new_prices <- cumsum(rets_dollar)
all.equal(new_prices, prices)
# Compound the percentage returns
new_prices <- cumprod(1+ rets_percent)
new_prices <- lapply(1:NCOL(new_prices), function (i)
    init_prices[i]*new_prices[, i])
new_prices <- rutils::do_call(cbind, new_prices)
all.equal(new_prices, prices)
# Sum the percentage returns
new_prices <- cumsum(rets_percent)
methods(cumsum)
new_prices <- lapply(1:NCOL(new_prices), function (i)
    new_prices[, i] + log(init_prices[i]))
new_prices <- rutils::do_call(cbind, new_prices)
# Only approximately equal
all.equal(new_prices, log(prices))
# Plot log VTI prices
dygraphs::dygraph(log(quantmod::Cl(rutils::etfenv$VTI)),
  main="Logarithm of VTI Prices") %>%
  dyOptions(colors="blue", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Calculate percentage VTI returns
prices <- rutils::etfenv$prices$VTI
prices <- na.omit(prices)
returns <- rutils::diffit(prices)/
  rutils::lagit(prices, lagg=1, pad_zeros=FALSE)
# Funding rate per day
f_rate <- 0.01/252
# Margin account
mar_gin <- cumsum(returns)
# Cumulative funding costs
f_costs <- cumsum(f_rate*mar_gin)
# Add funding costs to margin account
mar_gin <- (mar_gin + f_costs)
# dygraph plot of margin and funding costs
datav <- cbind(mar_gin, f_costs)
colnames <- c("Margin", "Cumulative Funding")
colnames(datav) <- colnames
dygraphs::dygraph(datav, main="VTI Margin Funding Costs") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", col="blue") %>%
  dySeries(name=colnames[2], axis="y2", col="red", strokeWidth=3) %>%
  dyLegend(show="always", width=500)
# bid_offer equal to 10 bps for liquid ETFs
bid_offer <- 0.001
# Cumulative transaction costs
costs <- bid_offer*cumsum(abs(returns))/2
# Subtract transaction costs from margin account
mar_gin <- cumsum(returns)
mar_gin <- (mar_gin - costs)
# dygraph plot of margin and transaction costs
datav <- cbind(mar_gin, costs)
colnames <- c("Margin", "Cumulative Transaction Costs")
colnames(datav) <- colnames
dygraphs::dygraph(datav, main="VTI Transaction Costs") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", col="blue") %>%
  dySeries(name=colnames[2], axis="y2", col="red", strokeWidth=3) %>%
  dyLegend(show="always", width=500)
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
wealth_fixed_shares <- cumsum(rets_dollar %*% weightv)
# Wealth of fixed dollars (with rebalancing)
wealth_fixed_dollars <- cumsum(rets_percent %*% weightv)
# Plot log wealth
wealth <- cbind(wealth_fixed_dollars, log(wealth_fixed_shares))
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
wealth_fixed_shares <- cumsum(rets_dollar %*% weightv)
# Calculate weighted percentage returns
rets_weighted <- rets_percent %*% weightv
# Wealth of fixed ratio of dollar amounts (with rebalancing)
wealth_fixed_ratio <- cumprod(1 + rets_weighted)
wealth_fixed_ratio <- wealth_fixed_shares[1]*wealth_fixed_ratio
# Plot log wealth
wealth <- log(cbind(wealth_fixed_shares, wealth_fixed_ratio))
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
excess <- excess*rutils::lagit(wealth_fixed_ratio)
# Cumulative transaction costs
costs <- bid_offer*cumsum(excess)/2
# Subtract transaction costs from wealth
wealth_fixed_ratio <- (wealth_fixed_ratio - costs)
# dygraph plot of wealth and transaction costs
wealth <- cbind(wealth_fixed_ratio, costs)
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
# Calculate dollar and percentage returns for VTI and IEF.
prices <- rutils::etfenv$prices[, c("VTI", "IEF")]
prices <- na.omit(prices)
rets_dollar <- rutils::diffit(prices)
rets_percent <- rets_dollar/rutils::lagit(prices, lagg=1, pad_zeros=FALSE)
# Calculate wealth of fixed ratio of dollar amounts.
weightv <- c(0.5, 0.5)
rets_weighted <- rets_percent %*% weightv
wealth_fixed_ratio <- cumprod(1 + rets_weighted)
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
wealth <- log(cbind(wealth_fixed_ratio, wealth_risk_parity))
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
