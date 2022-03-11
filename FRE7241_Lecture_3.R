# Calculate VTI and IEF dollar returns
price_s <- rutils::etfenv$price_s[, c("VTI", "IEF")]
price_s <- na.omit(price_s)
date_s <- index(price_s)
rets_dollar <- rutils::diff_it(price_s)
# Calculate VTI and IEF percentage returns
rets_percent <- rets_dollar/
  rutils::lag_it(price_s, lagg=1, pad_zeros=FALSE)
# Wealth of fixed shares (without rebalancing)
weight_s <- c(0.5, 0.5)
rets_dollar[1, ] <- price_s[1, ]
wealth_fsa <- cumsum(rets_dollar %*% weight_s)
# Wealth of fixed dollars (with rebalancing)
wealth_fda <- cumsum(rets_percent %*% weight_s)
# Plot log wealth
weal_th <- cbind(wealth_fda, log(wealth_fsa))
weal_th <- xts::xts(weal_th, index(price_s))
colnames(weal_th) <- c("Fixed dollars", "Fixed shares (log)")
col_names <- colnames(weal_th)
dygraphs::dygraph(weal_th, main="Wealth of Weighted Portfolios") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="red", strokeWidth=2) %>%
  dySeries(name=col_names[2], axis="y2", col="blue", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Margin account for fixed dollars (with rebalancing)
mar_gin <- cumsum(rets_percent %*% weight_s)
# Cumulative transaction costs
cost_s <- bid_offer*cumsum(abs(rets_percent) %*% weight_s)/2
# Subtract transaction costs from margin account
mar_gin <- (mar_gin - cost_s)
# dygraph plot of margin and transaction costs
da_ta <- cbind(mar_gin, cost_s)
da_ta <- xts::xts(da_ta, index(price_s))
col_names <- c("Margin", "Cumulative Transaction Costs")
colnames(da_ta) <- col_names
dygraphs::dygraph(da_ta, main="Fixed Dollar Portfolio Transaction Costs") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="blue") %>%
  dySeries(name=col_names[2], axis="y2", col="red", strokeWidth=3) %>%
  dyLegend(show="always", width=500)
# Wealth of fixed shares (without rebalancing)
wealth_fsa <- cumsum(rets_dollar %*% weight_s)
# Calculate weighted percentage returns
rets_weighted <- rets_percent %*% weight_s
# Wealth of fixed ratio of dollar amounts (with rebalancing)
wealth_cda <- cumprod(1 + rets_weighted)
wealth_cda <- wealth_fsa[1]*wealth_cda
# Plot log wealth
weal_th <- log(cbind(wealth_fsa, wealth_cda))
weal_th <- xts::xts(weal_th, index(price_s))
colnames(weal_th) <- c("Fixed Shares", "Fixed Ratio")
dygraphs::dygraph(weal_th, main="Log Wealth of Fixed Dollar Ratios") %>%
  dyOptions(colors=c("blue","red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Returns in excess of weighted returns
ex_cess <- lapply(rets_percent, function(x) (rets_weighted - x))
ex_cess <- do.call(cbind, ex_cess)
sum(ex_cess %*% weight_s)
# Calculate weighted sum of absolute excess returns
ex_cess <- abs(ex_cess) %*% weight_s
# Total dollar amount of stocks that need to be traded
ex_cess <- ex_cess*rutils::lag_it(wealth_cda)
# Cumulative transaction costs
cost_s <- bid_offer*cumsum(ex_cess)/2
# Subtract transaction costs from wealth
wealth_cda <- (wealth_cda - cost_s)
# dygraph plot of wealth and transaction costs
weal_th <- cbind(wealth_cda, cost_s)
weal_th <- xts::xts(weal_th, index(price_s))
col_names <- c("Wealth", "Cumulative Transaction Costs")
colnames(weal_th) <- col_names
dygraphs::dygraph(weal_th, main="Transaction Costs With Fixed Dollar Ratios") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="blue") %>%
  dySeries(name=col_names[2], axis="y2", col="red", strokeWidth=3) %>%
  dyLegend(show="always", width=500)
# Calculate stock and bond returns
re_turns <- na.omit(rutils::etfenv$re_turns[, c("VTI", "IEF")])
weight_s <- c(0.4, 0.6)
re_turns <- cbind(re_turns, re_turns %*% weight_s)
colnames(re_turns)[3] <- "Combined"
# Calculate correlations
cor(re_turns)
# Calculate Sharpe ratios
sqrt(252)*sapply(re_turns, function(x) mean(x)/sd(x))
# Calculate standard deviation, skewness, and kurtosis
sapply(re_turns, function(x) {
  # Calculate standard deviation
  stddev <- sd(x)
  # Standardize the returns
  x <- (x - mean(x))/stddev
  c(stddev=stddev, skew=mean(x^3), kurt=mean(x^4))
})  # end sapply
# Wealth of fixed ratio of dollar amounts
weal_th <- cumprod(1 + re_turns)
# Plot cumulative wealth
dygraphs::dygraph(log(weal_th), main="Stock and Bond Portfolio") %>%
  dyOptions(colors=c("blue","green","blue","red")) %>%
  dySeries("Combined", color="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Extract ETF returns
sym_bols <- c("VTI", "IEF", "DBC")
re_turns <- na.omit(rutils::etfenv$re_turns[, sym_bols])
# Calculate all-weather portfolio wealth
weights_aw <- c(0.30, 0.55, 0.15)
re_turns <- cbind(re_turns, re_turns %*% weights_aw)
colnames(re_turns)[4] <- "All Weather"
# Calculate cumulative wealth from returns
weal_th <- cumsum(re_turns)
# dygraph all-weather wealth
dygraphs::dygraph(weal_th, main="All-Weather Portfolio") %>%
  dyOptions(colors=c("blue", "green", "orange", "red")) %>%
  dySeries("All Weather", color="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Plot all-weather wealth
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue", "green", "red")
quantmod::chart_Series(weal_th, theme=plot_theme, lwd=c(2, 2, 2, 4),
       name="All-Weather Portfolio")
legend("topleft", legend=colnames(weal_th),
  inset=0.1, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")
# Calculate VTI returns
re_turns <- na.omit(rutils::etfenv$re_turns$VTI["2008/2009"])
date_s <- index(re_turns)
n_rows <- NROW(re_turns)
re_turns <- drop(zoo::coredata(re_turns))
bfloor <- 60  # bond floor
co_eff <- 2  # multiplier
portf_value <- numeric(n_rows)
portf_value[1] <- 100  # principal
stock_value <- numeric(n_rows)
stock_value[1] <- co_eff*(portf_value[1] - bfloor)
bond_value <- numeric(n_rows)
bond_value[1] <- (portf_value[1] - stock_value[1])
# Simulate CPPI strategy
for (t in 2:n_rows) {
  portf_value[t] <- portf_value[t-1] + stock_value[t-1]*re_turns[t]
  stock_value[t] <- co_eff*(portf_value[t] - bfloor)
  bond_value[t] <- (portf_value[t] - stock_value[t])
}  # end for
# dygraph plot of CPPI strategy
vt_i <- 100*cumprod(1+re_turns)
da_ta <- xts::xts(cbind(stock_value, bond_value, portf_value, vt_i), date_s)
colnames(da_ta) <- c("stocks", "bonds", "CPPI", "VTI")
dygraphs::dygraph(da_ta, main="CPPI strategy") %>%
  dyOptions(colors=c("red", "green","blue","orange"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Calculate dollar and percentage returns for VTI and IEF.
price_s <- rutils::etfenv$price_s[, c("VTI", "IEF")]
price_s <- na.omit(price_s)
rets_dollar <- rutils::diff_it(price_s)
rets_percent <- rets_dollar/rutils::lag_it(price_s, lagg=1, pad_zeros=FALSE)
# Calculate wealth of fixed ratio of dollar amounts.
weight_s <- c(0.5, 0.5)
rets_weighted <- rets_percent %*% weight_s
wealth_cda <- cumprod(1 + rets_weighted)
# Calculate rolling percentage volatility.
look_back <- 21
vo_l <- roll::roll_sd(rets_percent, width=look_back)
vo_l <- zoo::na.locf(vo_l, na.rm=FALSE)
vo_l <- zoo::na.locf(vo_l, fromLast=TRUE)
# Calculate the risk parity portfolio allocations.
allocation_s <- lapply(1:NCOL(price_s),
  function(x) weight_s[x]/vo_l[, x])
allocation_s <- do.call(cbind, allocation_s)
# Scale allocations to 1 dollar total.
allocation_s <- allocation_s/rowSums(allocation_s)
# Lag the allocations
allocation_s <- rutils::lag_it(allocation_s)
# Calculate wealth of risk parity.
rets_weighted <- rowSums(rets_percent*allocation_s)
wealth_risk_parity <- cumprod(1 + rets_weighted)
# Calculate the log wealths.
weal_th <- log(cbind(wealth_cda, wealth_risk_parity))
weal_th <- xts::xts(weal_th, index(price_s))
colnames(weal_th) <- c("Fixed Ratio", "Risk Parity")
# Calculate the Sharpe ratios.
sqrt(252)*sapply(rutils::diff_it(weal_th), function (x) mean(x)/sd(x))
# Plot a dygraph of the log wealths.
dygraphs::dygraph(weal_th, main="Log Wealth of Risk Parity vs Fixed Dollar Ratios") %>%
  dyOptions(colors=c("blue","red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Test risk parity market timing of VTI using Treynor-Mazuy test
re_turns <- rutils::diff_it(weal_th)
vt_i <- rets_percent$VTI
de_sign <- cbind(re_turns, vt_i, vt_i^2)
de_sign <- na.omit(de_sign)
colnames(de_sign)[1:2] <- c("fixed","risk_parity")
colnames(de_sign)[4] <- "treynor"
mod_el <- lm(risk_parity ~ VTI + treynor, data=de_sign)
summary(mod_el)
# Plot residual scatterplot
residual_s <- (de_sign$risk_parity - mod_el$coeff[2]*de_sign$VTI)
residual_s <- mod_el$residuals
x11(width=6, height=5)
plot.default(x=de_sign$VTI, y=residual_s, xlab="VTI", ylab="residuals")
title(main="Treynor-Mazuy Market Timing Test\n for Risk Parity vs VTI", line=0.5)
# Plot fitted (predicted) response values
fit_ted <- (mod_el$coeff["(Intercept)"] +
        mod_el$coeff["treynor"]*vt_i^2)
points.default(x=de_sign$VTI, y=fit_ted, pch=16, col="red")
text(x=0.05, y=0.025, paste("Risk Parity t-value =", round(summary(mod_el)$coeff["treynor", "t value"], 2)))
# Test for fixed ratio market timing of VTI using Treynor-Mazuy test
mod_el <- lm(fixed ~ VTI + treynor, data=de_sign)
summary(mod_el)
# Plot fitted (predicted) response values
fit_ted <- (mod_el$coeff["(Intercept)"] + mod_el$coeff["treynor"]*vt_i^2)
points.default(x=de_sign$VTI, y=fit_ted, pch=16, col="blue")
text(x=0.05, y=0.02, paste("Fixed Ratio t-value =", round(summary(mod_el)$coeff["treynor", "t value"], 2)))
# Calculate positions
vt_i <- na.omit(rutils::etfenv$re_turns$VTI)
position_s <- rep(NA_integer_, NROW(vt_i))
date_s <- index(vt_i)
date_s <- format(date_s, "%m-%d")
position_s[date_s == "05-01"] <- 0
position_s[date_s == "05-03"] <- 0
position_s[date_s == "11-01"] <- 1
position_s[date_s == "11-03"] <- 1
# Carry forward and backward non-NA position_s
position_s <- zoo::na.locf(position_s, na.rm=FALSE)
position_s <- zoo::na.locf(position_s, fromLast=TRUE)
# Calculate strategy returns
sell_inmay <- position_s*vt_i
weal_th <- cbind(vt_i, sell_inmay)
colnames(weal_th) <- c("VTI", "sell_in_may")
# Calculate Sharpe and Sortino ratios
sqrt(252)*sapply(weal_th,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot wealth of Sell in May strategy
dygraphs::dygraph(cumsum(weal_th), main="Sell in May Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# OR: Open x11 for plotting
x11(width=6, height=5)
par(mar=c(4, 4, 3, 1), oma=c(0, 0, 0, 0))
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("blue", "red")
quantmod::chart_Series(weal_th, theme=plot_theme, name="Sell in May Strategy")
legend("topleft", legend=colnames(weal_th),
  inset=0.1, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")
# Test if Sell in May strategy can time VTI
de_sign <- cbind(vt_i, 0.5*(vt_i+abs(vt_i)), vt_i^2)
colnames(de_sign) <- c("VTI", "merton", "treynor")
# Perform Merton-Henriksson test
mod_el <- lm(sell_inmay ~ VTI + merton, data=de_sign)
summary(mod_el)
# Perform Treynor-Mazuy test
mod_el <- lm(sell_inmay ~ VTI + treynor, data=de_sign)
summary(mod_el)
# Plot Treynor-Mazuy residual scatterplot
residual_s <- (sell_inmay - mod_el$coeff[2]*vt_i)
plot.default(x=vt_i, y=residual_s, xlab="VTI", ylab="residuals")
title(main="Treynor-Mazuy Market Timing Test\n for Sell in May vs VTI", line=0.5)
# Plot fitted (predicted) response values
fit_ted <- (mod_el$coeff["(Intercept)"] +
        mod_el$coeff["treynor"]*vt_i^2)
points.default(x=vt_i, y=fit_ted, pch=16, col="red")
text(x=0.05, y=0.05, paste("Treynor test t-value =", round(summary(mod_el)$coeff["treynor", "t value"], 2)))
# Calculate the log of OHLC VTI prices
oh_lc <- log(rutils::etfenv$VTI)
op_en <- quantmod::Op(oh_lc)
hi_gh <- quantmod::Hi(oh_lc)
lo_w <- quantmod::Lo(oh_lc)
clos_e <- quantmod::Cl(oh_lc)
# Calculate the close-to-close log returns, the intraday
# open-to-close returns and the overnight close-to-open returns.
close_close <- rutils::diff_it(clos_e)
colnames(close_close) <- "close_close"
open_close <- (clos_e - op_en)
colnames(open_close) <- "open_close"
close_open <- (op_en - rutils::lag_it(clos_e, lagg=1, pad_zeros=FALSE))
colnames(close_open) <- "close_open"
# Calculate Sharpe and Sortino ratios
weal_th <- cbind(close_close, close_open, open_close)
sqrt(252)*sapply(weal_th,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot log wealth
dygraphs::dygraph(cumsum(weal_th),
  main="Wealth of Close-to-Close, Close-to-Open, and Open-to-Close Strategies") %>%
  dySeries(name="close_close", label="Close-to-Close (static)", strokeWidth=2, col="blue") %>%
  dySeries(name="close_open", label="Close-to-Open (overnight)", strokeWidth=2, col="red") %>%
  dySeries(name="open_close", label="Open-to-Close (daytime)", strokeWidth=2, col="green") %>%
  dyLegend(width=600)
# Calculate the VTI returns
vt_i <- na.omit(rutils::etfenv$re_turns$VTI)
date_s <- zoo::index(vt_i)
# Calculate first business day of every month
day_s <- as.numeric(format(date_s, "%d"))
indeks <- which(rutils::diff_it(day_s) < 0)
date_s[head(indeks)]
# Calculate Turn of the Month dates
indeks <- lapply((-1):2, function(x) indeks + x)
indeks <- do.call(c, indeks)
sum(indeks > NROW(date_s))
indeks <- sort(indeks)
date_s[head(indeks, 11)]
# Calculate Turn of the Month pnls
pnl_s <- numeric(NROW(vt_i))
pnl_s[indeks] <- vt_i[indeks, ]
# Combine data
weal_th <- cbind(vt_i, pnl_s)
col_names <- c("VTI", "Strategy")
colnames(weal_th) <- col_names
# Calculate Sharpe and Sortino ratios
sqrt(252)*sapply(weal_th,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# dygraph plot VTI Turn of the Month strategy
dygraphs::dygraph(cumsum(weal_th), main="Turn of the Month Strategy") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=col_names[2], axis="y2", strokeWidth=2, col="red")
# Calculate the VTI returns
vt_i <- na.omit(rutils::etfenv$re_turns$VTI)
date_s <- zoo::index(vt_i)
vt_i <- drop(coredata(vt_i))
n_rows <- NROW(vt_i)
# Simulate stop-loss strategy
sto_p <- 0.05
ma_x <- 0.0
cum_ret <- 0.0
pnl_s <- vt_i
for (i in 1:n_rows) {
# Calculate drawdown
  cum_ret <- cum_ret + vt_i[i]
  ma_x <- max(ma_x, cum_ret)
  dd <- (cum_ret - ma_x)
# Check for stop-loss
  if (dd < -sto_p*ma_x)
    pnl_s[i+1] <- 0
}  # end for
# Same but without using explicit loops
cum_sum <- cumsum(vt_i)
cum_max <- cummax(cumsum(vt_i))
dd <- (cum_sum - cum_max)
pnls2 <- vt_i
is_dd <- rutils::lag_it(dd < -sto_p*cum_max)
pnls2 <- ifelse(is_dd, 0, pnls2)
all.equal(pnl_s, pnls2)
# Combine data
weal_th <- xts::xts(cbind(vt_i, pnl_s), date_s)
col_names <- c("VTI", "Strategy")
colnames(weal_th) <- col_names
# Calculate Sharpe and Sortino ratios
sqrt(252)*sapply(weal_th,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# dygraph plot VTI stop-loss strategy
dygraphs::dygraph(cumsum(weal_th), main="VTI Stop-loss Strategy") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=col_names[2], axis="y2", strokeWidth=2, col="red")
# Simulate multiple stop-loss strategies
cum_sum <- cumsum(vt_i)
cum_max <- cummax(cumsum(vt_i))
dd <- (cum_sum - cum_max)
cum_pnls <- sapply(0.01*(1:20), function(sto_p) {
  pnl_s <- vt_i
  is_dd <- rutils::lag_it(dd < -sto_p*cum_max)
  pnl_s <- ifelse(is_dd, 0, pnl_s)
  sum(pnl_s)
})  # end sapply
# Plot cumulative pnls for stop-loss strategies
plot(x=0.01*(1:20), y=cum_pnls,
     main="Cumulative PnLs for Stop-loss Strategies",
     xlab="stop-loss level", ylab="cumulative pnl",
     t="l", lwd=3, col="blue")
# Extract time series of VTI log prices
clos_e <- log(na.omit(rutils::etfenv$price_s$VTI))
# Inspect the R code of the function filter()
filter
# Calculate EWMA weight_s
look_back <- 21
weight_s <- exp(-0.1*1:look_back)
weight_s <- weight_s/sum(weight_s)
# Calculate convolution using filter()
filter_ed <- filter(clos_e, filter=weight_s,
              method="convolution", sides=1)
# filter() returns time series of class "ts"
class(filter_ed)
# Get information about C_cfilter()
getAnywhere(C_cfilter)
# Filter using C_cfilter() over past values (sides=1).
filter_fast <- .Call(stats:::C_cfilter, clos_e, filter=weight_s,
               sides=1, circular=FALSE)
all.equal(as.numeric(filter_ed), filter_fast, check.attributes=FALSE)
# Calculate EWMA prices using roll::roll_sum()
weights_rev <- rev(weight_s)
roll_ed <- roll::roll_sum(clos_e, width=look_back, weights=weights_rev, min_obs=1)
all.equal(filter_fast[-(1:look_back)], as.numeric(roll_ed)[-(1:look_back)])
# Benchmark speed of rolling calculations
library(microbenchmark)
summary(microbenchmark(
  filter=filter(clos_e, filter=weight_s, method="convolution", sides=1),
  filter_fast=.Call(stats:::C_cfilter, clos_e, filter=weight_s, sides=1, circular=FALSE),
  roll=roll::roll_sum(clos_e, width=look_back, weights=weights_rev)
  ), times=10)[, c(1, 4, 5)]
# Simulate AR process using filter()
n_rows <- NROW(clos_e)
# Calculate ARIMA coefficients and innovations
co_eff <- weight_s/4
n_coeff <- NROW(co_eff)
in_nov <- rnorm(n_rows)
ari_ma <- filter(x=in_nov, filter=co_eff, method="recursive")
# Get information about C_rfilter()
getAnywhere(C_rfilter)
# Filter using C_rfilter() compiled C++ function directly
arima_fast <- .Call(stats:::C_rfilter, in_nov, co_eff,
              double(n_coeff + n_rows))
all.equal(as.numeric(ari_ma), arima_fast[-(1:n_coeff)],
    check.attributes=FALSE)
# Filter using C++ code
arima_fastest <- HighFreq::sim_arima(in_nov, rev(co_eff))
all.equal(arima_fast[-(1:n_coeff)], drop(arima_fastest))
# Benchmark speed of the three methods
summary(microbenchmark(
  filter=filter(x=in_nov, filter=co_eff, method="recursive"),
  filter_fast=.Call(stats:::C_rfilter, in_nov, co_eff, double(n_coeff + n_rows)),
  Rcpp=HighFreq::sim_arima(in_nov, rev(co_eff))
  ), times=10)[, c(1, 4, 5)]
# Calculate trailing EWMA prices using roll::roll_sum()
look_back <- 21
weight_s <- exp(-0.1*1:look_back)
weight_s <- weight_s/sum(weight_s)
weights_rev <- rev(weight_s)
filter_ed <- roll::roll_sum(clos_e, width=NROW(weight_s), weights=weights_rev)
# Copy warmup period
filter_ed[1:look_back] <- clos_e[1:look_back]
# Combine prices with smoothed prices
price_s <- cbind(clos_e, filter_ed)
colnames(price_s)[2] <- "VTI Smooth"
# Calculate standard deviations of returns
sapply(rutils::diff_it(price_s), sd)
# Plot dygraph
dygraphs::dygraph(price_s["2009"], main="VTI Prices and Trailing Smoothed Prices") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2)
# Calculate centered EWMA prices using roll::roll_sum()
weight_s <- c(weights_rev, weight_s[-1])
weight_s <- weight_s/sum(weight_s)
filter_ed <- roll::roll_sum(clos_e, width=NROW(weight_s), weights=weight_s, online=FALSE)
# Copy warmup period
filter_ed[1:(2*look_back)] <- clos_e[1:(2*look_back)]
# Center the data
filter_ed <- rutils::lag_it(filter_ed, -(look_back-1), pad_zeros=FALSE)
# Combine prices with smoothed prices
price_s <- cbind(clos_e, filter_ed)
colnames(price_s)[2] <- "VTI Smooth"
# Calculate standard deviations of returns
sapply(rutils::diff_it(price_s), sd)
# Plot dygraph
dygraphs::dygraph(price_s["2009"], main="VTI Prices and Centered Smoothed Prices") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2)
# Open plot window
x11(width=6, height=7)
# Set plot parameters
par(oma=c(1, 1, 0, 1), mar=c(1, 1, 1, 1), mgp=c(0, 0.5, 0),
    cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# Set two plot panels
par(mfrow=c(2,1))
# Plot ACF of VTI returns
rutils::plot_acf(re_turns[, 1], lag=10, xlab="")
title(main="ACF of VTI Returns", line=-1)
# Plot ACF of smoothed VTI returns
rutils::plot_acf(re_turns[, 2], lag=10, xlab="")
title(main="ACF of Smoothed VTI Returns", line=-1)
# Extract log VTI prices
clos_e <- log(na.omit(rutils::etfenv$price_s$VTI))
n_rows <- NROW(clos_e)
# Calculate EWMA weights
look_back <- 21
lamb_da <- 0.1
weight_s <- exp(lamb_da*1:look_back)
weight_s <- weight_s/sum(weight_s)
# Calculate EWMA prices
ew_ma <- roll::roll_sum(clos_e, width=look_back, weights=weight_s, min_obs=1)
# Copy over NA values
ew_ma <- zoo::na.locf(ew_ma, fromLast=TRUE)
price_s <- cbind(clos_e, ew_ma)
colnames(price_s) <- c("VTI", "VTI EWMA")
# Dygraphs plot with custom line colors
col_ors <- c("blue", "red")
dygraphs::dygraph(price_s["2009"], main="VTI EWMA Prices") %>%
  dyOptions(colors=col_ors, strokeWidth=2)
# Plot EWMA prices with custom line colors
x11(width=6, height=5)
plot_theme <- chart_theme()
plot_theme$col$line.col <- col_ors
quantmod::chart_Series(price_s["2009"], theme=plot_theme,
       lwd=2, name="VTI EWMA Prices")
legend("bottomright", legend=colnames(price_s),
 inset=0.1, bg="white", lty=1, lwd=6, cex=0.8,
 col=plot_theme$col$line.col, bty="n")
# Calculate log OHLC prices and volumes
sym_bol <- "VTI"
oh_lc <- rutils::etfenv$VTI
n_rows <- NROW(oh_lc)
clos_e <- log(quantmod::Cl(oh_lc))
vol_ume <- quantmod::Vo(oh_lc)
# Calculate the VWAP prices
look_back <- 21
v_wap <- roll::roll_sum(clos_e*vol_ume, width=look_back, min_obs=1)
volume_roll <- roll::roll_sum(vol_ume, width=look_back, min_obs=1)
v_wap <- v_wap/volume_roll
v_wap <- zoo::na.locf(v_wap, fromLast=TRUE)
price_s <- cbind(clos_e, v_wap)
colnames(price_s) <- c(sym_bol, paste(sym_bol, "VWAP"))
# Dygraphs plot with custom line colors
col_ors <- c("blue", "red")
dygraphs::dygraph(price_s["2009"], main="VTI VWAP Prices") %>%
  dyOptions(colors=col_ors, strokeWidth=2)
# Plot VWAP prices with custom line colors
x11(width=6, height=5)
plot_theme <- chart_theme()
plot_theme$col$line.col <- col_ors
quantmod::chart_Series(price_s["2009"], theme=plot_theme,
       lwd=2, name="VTI VWAP Prices")
legend("bottomright", legend=colnames(price_s),
 inset=0.1, bg="white", lty=1, lwd=6, cex=0.8,
 col=plot_theme$col$line.col, bty="n")
# Calculate two EWMA prices
look_back <- 21
lamb_da <- 0.1
weight_s <- exp(lamb_da*1:look_back)
weight_s <- weight_s/sum(weight_s)
ewma_fast <- roll::roll_sum(clos_e, width=look_back, weights=weight_s, min_obs=1)
lamb_da <- 0.05
weight_s <- exp(lamb_da*1:look_back)
weight_s <- weight_s/sum(weight_s)
ewma_slow <- roll::roll_sum(clos_e, width=look_back, weights=weight_s, min_obs=1)
# Calculate VTI returns
re_turns <- (ewma_fast - ewma_slow)
price_s <- cbind(clos_e, re_turns)
colnames(price_s) <- c(sym_bol, paste(sym_bol, "Returns"))
# Plot dygraph of VTI Returns
col_names <- colnames(price_s)
dygraphs::dygraph(price_s["2009"], main=paste(sym_bol, "EWMA Returns")) %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=2, col="blue") %>%
  dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=2, col="red")
# Calculate fractional weights
del_ta <- 0.1
weight_s <- (del_ta - 0:(look_back-2)) / 1:(look_back-1)
weight_s <- (-1)^(1:(look_back-1))*cumprod(weight_s)
weight_s <- c(1, weight_s)
weight_s <- (weight_s - mean(weight_s))
weight_s <- rev(weight_s)
# Calculate fractional VTI returns
re_turns <- roll::roll_sum(clos_e, width=look_back, weights=weight_s, min_obs=1, online=FALSE)
price_s <- cbind(clos_e, re_turns)
colnames(price_s) <- c(sym_bol, paste(sym_bol, "Returns"))
# Plot dygraph of VTI Returns
col_names <- colnames(price_s)
dygraphs::dygraph(price_s["2009"], main=paste(sym_bol, "Fractional Returns")) %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=2, col="blue") %>%
  dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=2, col="red")
# Calculate VTI log returns
clos_e <- log(quantmod::Cl(rutils::etfenv$VTI))
re_turns <- rutils::diff_it(clos_e)
# Perform ADF test for prices
tseries::adf.test(clos_e)
# Perform ADF test for returns
tseries::adf.test(re_turns)
# Calculate fractional VTI returns
delta_s <- 0.1*c(1, 3, 5, 7, 9)
re_turns <- lapply(delta_s, function(del_ta) {
  weight_s <- (del_ta - 0:(look_back-2)) / 1:(look_back-1)
  weight_s <- c(1, (-1)^(1:(look_back-1))*cumprod(weight_s))
  weight_s <- rev(weight_s - mean(weight_s))
  roll::roll_sum(clos_e, width=look_back, weights=weight_s, min_obs=1, online=FALSE)
})  # end lapply
re_turns <- do.call(cbind, re_turns)
re_turns <- cbind(clos_e, re_turns)
colnames(re_turns) <- c("VTI", paste0("frac_", delta_s))
# Calculate ADF test statistics
adf_stats <- sapply(re_turns, function(x)
  suppressWarnings(tseries::adf.test(x)$statistic)
)  # end sapply
names(adf_stats) <- colnames(re_turns)
# Plot dygraph of fractional VTI returns
color_s <- colorRampPalette(c("blue", "red"))(NCOL(re_turns))
col_names <- colnames(re_turns)
dy_graph <- dygraphs::dygraph(re_turns["2019"], main="Fractional Returns") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=2, col=color_s[1])
for (i in 2:NROW(col_names))
  dy_graph <- dy_graph %>%
  dyAxis("y2", label=col_names[i], independentTicks=TRUE) %>%
  dySeries(name=col_names[i], axis="y2", label=col_names[i], strokeWidth=2, col=color_s[i])
dy_graph <- dy_graph %>% dyLegend(width=500)
dy_graph
# Calculate volume z-scores
vol_ume <- quantmod::Vo(rutils::etfenv$VTI)
look_back <- 21
volume_mean <- roll::roll_mean(vol_ume, width=look_back, min_obs=1)
volume_sd <- roll::roll_sd(rutils::diff_it(vol_ume), width=look_back, min_obs=1)
volume_scores <- (vol_ume - volume_mean)/volume_sd
# Plot histogram of volume z-scores
x11(width=6, height=5)
hist(volume_scores, breaks=1e2)
# Plot dygraph of volume z-scores of VTI prices
price_s <- cbind(clos_e, volume_scores)
colnames(price_s) <- c("VTI", "Z-scores")
col_names <- colnames(price_s)
dygraphs::dygraph(price_s["2009"], main="VTI Volume Z-Scores") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=2, col="blue") %>%
  dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=2, col="red")
# Extract VTI log OHLC prices
oh_lc <- log(rutils::etfenv$VTI)
# Calculate volatility z-scores
vol_at <- quantmod::Hi(oh_lc)-quantmod::Lo(oh_lc)
look_back <- 21
volat_mean <- roll::roll_mean(vol_at, width=look_back, min_obs=1)
volat_sd <- roll::roll_sd(rutils::diff_it(vol_at), width=look_back, min_obs=1)
volat_scores <- ifelse(is.na(volat_sd), 0, (vol_at - volat_mean)/volat_sd)
# Plot histogram of volatility z-scores
x11(width=6, height=5)
hist(volat_scores, breaks=1e2)
# Plot scatterplot of volume and volatility z-scores
plot(as.numeric(volat_scores), as.numeric(volume_scores),
     xlab="volatility z-score", ylab="volume z-score")
# Plot dygraph of VTI volatility z-scores
clos_e <- quantmod::Cl(oh_lc)
price_s <- cbind(clos_e, volat_scores)
colnames(price_s) <- c("VTI", "Z-scores")
col_names <- colnames(price_s)
dygraphs::dygraph(price_s["2009"], main="VTI Volatility Z-Scores") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=2, col="blue") %>%
  dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=2, col="red")
# Calculate the centered volatility
look_back <- 21
half_back <- look_back %/% 2
vol_at <- roll::roll_sd(re_turns, width=look_back, min_obs=1)
vol_at <- rutils::lag_it(vol_at, lagg=(-half_back))
# Calculate the z-scores of prices
price_scores <- (2*clos_e -
  rutils::lag_it(clos_e, half_back, pad_zeros=FALSE) -
  rutils::lag_it(clos_e, -half_back, pad_zeros=FALSE))
price_scores <- ifelse(vol_at > 0, price_scores/vol_at, 0)
# Plot dygraph of z-scores of VTI prices
price_s <- cbind(clos_e, price_scores)
colnames(price_s) <- c("VTI", "Z-scores")
col_names <- colnames(price_s)
dygraphs::dygraph(price_s["2009"], main="VTI Price Z-Scores") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=2, col="blue") %>%
  dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=2, col="red")
# Calculate thresholds for labeling tops and bottoms
threshold_s <- quantile(price_scores, c(0.1, 0.9))
# Calculate the vectors of tops and bottoms
top_s <- (price_scores > threshold_s[2])
colnames(top_s) <- "tops"
bottom_s <- (price_scores < threshold_s[1])
colnames(bottom_s) <- "bottoms"
# Backtest in-sample VTI strategy
position_s <- rep(NA_integer_, NROW(re_turns))
position_s[1] <- 0
position_s[top_s] <- (-1)
position_s[bottom_s] <- 1
position_s <- zoo::na.locf(position_s)
position_s <- rutils::lag_it(position_s)
pnl_s <- cumsum(re_turns*position_s)
# Plot dygraph of in-sample VTI strategy
price_s <- cbind(clos_e, pnl_s)
colnames(price_s) <- c("VTI", "Strategy")
col_names <- colnames(price_s)
dygraphs::dygraph(price_s, main="VTI Strategy Using In-sample Labels") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=2, col="blue") %>%
  dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=2, col="red")
# Calculate trailing price z-scores
date_s <- matrix(as.numeric(zoo::index(clos_e)))
look_back <- 21
price_scores <- drop(HighFreq::roll_zscores(res_ponse=clos_e, de_sign=date_s, look_back=look_back))
price_scores[1:look_back] <- 0
# Plot dygraph of z-scores of VTI prices
price_s <- cbind(clos_e, price_scores)
colnames(price_s) <- c("VTI", "Z-scores")
col_names <- colnames(price_s)
dygraphs::dygraph(price_s["2009"], main="VTI Price Z-Scores") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=2, col="blue") %>%
  dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=2, col="red")
# Extract time series of VTI log prices
clos_e <- log(na.omit(rutils::etfenv$price_s$VTI))
# Define look-back window and a half window
look_back <- 11
# Calculate time series of medians
medi_an <- roll::roll_median(clos_e, width=look_back)
# medi_an <- TTR::runMedian(clos_e, n=look_back)
# Calculate time series of MAD
ma_d <- HighFreq::roll_var(clos_e, look_back=look_back, method="quantile")
# ma_d <- TTR::runMAD(clos_e, n=look_back)
# Calculate time series of z-scores
z_scores <- (clos_e - medi_an)/ma_d
z_scores[1:look_back, ] <- 0
tail(z_scores, look_back)
range(z_scores)
x11(width=6, height=5)
# Plot prices and medians
dygraphs::dygraph(cbind(clos_e, medi_an), main="VTI median") %>%
  dyOptions(colors=c("black", "red"))
# Plot histogram of z-scores
histo_gram <- hist(z_scores, col="lightgrey",
  xlab="z-scores", breaks=50, xlim=c(-4, 4),
  ylab="frequency", freq=FALSE, main="Hampel Z-scores histogram")
lines(density(z_scores, adjust=1.5), lwd=3, col="blue")
# Calculate one-sided Hampel z-scores
medi_an <- roll::roll_median(clos_e, width=look_back)
# medi_an <- TTR::runMedian(clos_e, n=look_back)
ma_d <- HighFreq::roll_var(clos_e, look_back=look_back, method="quantile")
# ma_d <- TTR::runMAD(clos_e, n=look_back)
z_scores <- (clos_e - medi_an)/ma_d
z_scores[1:look_back, ] <- 0
tail(z_scores, look_back)
range(z_scores)
# Calculate two-sided Hampel z-scores
half_back <- look_back %/% 2
medi_an <- rutils::lag_it(medi_an, lagg=-half_back)
ma_d <- rutils::lag_it(ma_d, lagg=-half_back)
z_scores <- (clos_e - medi_an)/ma_d
z_scores[1:look_back, ] <- 0
tail(z_scores, look_back)
range(z_scores)
# Calculate VTI percentage returns
re_turns <- rutils::diff_it(clos_e)
# Define threshold value
thresh_old <- sum(abs(range(z_scores)))/8
# Backtest VTI strategy
position_s <- rep(NA_integer_, NROW(clos_e))
position_s[1] <- 0
position_s[z_scores < -thresh_old] <- 1
position_s[z_scores > thresh_old] <- (-1)
position_s <- zoo::na.locf(position_s)
position_s <- rutils::lag_it(position_s)
pnl_s <- cumsum(re_turns*position_s)
# Plot dygraph of in-sample VTI strategy
price_s <- cbind(clos_e, pnl_s)
colnames(price_s) <- c("VTI", "Strategy")
col_names <- colnames(price_s)
dygraphs::dygraph(price_s, main="VTI Hampel Strategy") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=2, col="blue") %>%
  dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=2, col="red")
library(HighFreq)
# Read TAQ trade data from csv file
ta_q <- data.table::fread(file="/Volumes/external/Develop/data/xlk_tick_trades_2020_03_16.csv")
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
# Calculate centered Hampel filter to remove price jumps
look_back <- 111
half_back <- look_back %/% 2
medi_an <- roll::roll_median(ta_q$price, width=look_back)
# medi_an <- TTR::runMedian(ta_q$price, n=look_back)
medi_an <- rutils::lag_it(medi_an, lagg=-half_back, pad_zeros=FALSE)
ma_d <- HighFreq::roll_var(matrix(ta_q$price), look_back=look_back, method="quantile")
# ma_d <- TTR::runMAD(ta_q$price, n=look_back)
ma_d <- rutils::lag_it(ma_d, lagg=-half_back, pad_zeros=FALSE)
# Calculate Z-scores
z_scores <- (ta_q$price - medi_an)/ma_d
z_scores[is.na(z_scores)] <- 0
z_scores[!is.finite(z_scores)] <- 0
sum(is.na(z_scores))
sum(!is.finite(z_scores))
range(z_scores); mad(z_scores)
hist(z_scores, breaks=2000, xlim=c(-5*mad(z_scores), 5*mad(z_scores)))
# Define discrimination threshold value
thresh_old <- 6*mad(z_scores)
# Remove price jumps with large z-scores
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
# Define discrimination threshold value
thresh_old <- 6*mad(z_scores)
# Calculate number of prices classified as bad data
is_bad <- (abs(z_scores) > thresh_old)
sum(is_bad)
# Add 200 random price jumps into price_s
set.seed(1121)
n_bad <- 200
is_jump <- logical(NROW(x_ts))
is_jump[sample(x=NROW(is_jump), size=n_bad)] <- TRUE
x_ts$XLK.Close[is_jump] <- x_ts$XLK.Close[is_jump]*
  sample(c(0.95, 1.05), size=n_bad, replace=TRUE)
# Plot prices and medians
dygraphs::dygraph(cbind(x_ts, medi_an), main="VTI median") %>%
  dyOptions(colors=c("black", "red"))
# Calculate time series of z-scores
medi_an <- roll::roll_median(x_ts, width=look_back)
# medi_an <- TTR::runMedian(x_ts, n=look_back)
ma_d <- HighFreq::roll_var(x_ts, look_back=look_back, method="quantile")
# ma_d <- TTR::runMAD(x_ts, n=look_back)
z_scores <- ifelse(ma_d > 0, (x_ts - medi_an)/ma_d, 0)
z_scores[1:look_back, ] <- 0
# Calculate number of prices classified as bad data
is_bad <- (abs(z_scores) > thresh_old)
sum(is_bad)
# Calculate confusion matrix
table(actual=!is_jump, forecast=!is_bad)
sum(is_bad)
# FALSE positive (type I error)
sum(!is_jump & is_bad)
# FALSE negative (type II error)
sum(is_jump & !is_bad)
confu_sion <- table(!is_jump, !(abs(z_scores) > thresh_old))
# Confusion matrix as function of thresh_old
con_fuse <- function(actu_al, z_scores, thresh_old) {
    confu_sion <- table(!actu_al, !(abs(z_scores) > thresh_old))
    confu_sion <- confu_sion / rowSums(confu_sion)
    c(typeI=confu_sion[2, 1], typeII=confu_sion[1, 2])
  }  # end con_fuse
con_fuse(is_jump, z_scores, thresh_old=thresh_old)
# Define vector of discrimination thresholds
threshold_s <- seq(from=10.0, to=25.0, by=0.2)
# Calculate error rates
error_rates <- sapply(threshold_s, con_fuse,
  actu_al=is_jump, z_scores=z_scores)  # end sapply
error_rates <- t(error_rates)
rownames(error_rates) <- threshold_s
error_rates <- rbind(c(1, 0), error_rates)
error_rates <- rbind(error_rates, c(0, 1))
# Calculate area under ROC curve (AUC)
true_pos <- (1 - error_rates[, "typeII"])
true_pos <- (true_pos + rutils::lag_it(true_pos))/2
false_pos <- rutils::diff_it(error_rates[, "typeI"])
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
medi_an <- roll::roll_median(ta_q$price, width=3)
medi_an[1:2] <- ta_q$price[1:2]
medi_an <- rutils::lag_it(medi_an, lagg=-1, pad_zeros=FALSE)
ma_d <- HighFreq::roll_var(matrix(ta_q$price), look_back=look_back, method="quantile")
ma_d <- rutils::lag_it(ma_d, lagg=-1, pad_zeros=FALSE)
# Calculate Z-scores
z_scores <- ifelse(ma_d > 0, (ta_q$price - medi_an)/ma_d, 0)
range(z_scores); mad(z_scores)
ma_d <- mad(z_scores[abs(z_scores)>0])
hist(z_scores, breaks=2000, xlim=c(-5*ma_d, 5*ma_d))
# Define discrimination threshold value
thresh_old <- 6*ma_d
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
