# Calculate VTI and IEF dollar returns
price_s <- rutils::etf_env$price_s[, c("VTI", "IEF")]
price_s <- na.omit(price_s)
rets_dollar <- rutils::diff_it(price_s)
# Calculate VTI and IEF percentage returns
rets_percent <- rets_dollar/
  rutils::lag_it(price_s, lagg=1, pad_zeros=FALSE)

# Wealth of fixed shares (without rebalancing)
weight_s <- c(0.5, 0.5)
rets_dollar[1, ] <- price_s[1, ]
wealth_fixed_shares <- cumsum(rets_dollar %*% weight_s)
# Wealth of fixed dollars (with rebalancing)
wealth_fixed_dollars <- cumsum(rets_percent %*% weight_s)
# Plot log wealth
weal_th <- cbind(wealth_fixed_dollars, log(wealth_fixed_shares))
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
wealth_fixed_shares <- cumsum(rets_dollar %*% weight_s)
# Calculate weighted percentage returns
rets_weighted <- rets_percent %*% weight_s
# Wealth of fixed ratio of dollar amounts (with rebalancing)
wealth_fixed_ratio <- cumprod(1 + rets_weighted)
wealth_fixed_ratio <- wealth_fixed_shares[1]*wealth_fixed_ratio
# Plot log wealth
weal_th <- log(cbind(wealth_fixed_shares, wealth_fixed_ratio))
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
ex_cess <- ex_cess*rutils::lag_it(wealth_fixed_ratio)
# Cumulative transaction costs
cost_s <- bid_offer*cumsum(ex_cess)/2
# Subtract transaction costs from wealth
wealth_fixed_ratio <- (wealth_fixed_ratio - cost_s)

# dygraph plot of wealth and transaction costs
da_ta <- cbind(wealth_fixed_ratio, cost_s)
da_ta <- xts::xts(da_ta, index(price_s))
col_names <- c("Wealth", "Cumulative Transaction Costs")
colnames(da_ta) <- col_names
dygraphs::dygraph(da_ta, main="Transaction Costs With Fixed Dollar Ratios") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="blue") %>%
  dySeries(name=col_names[2], axis="y2", col="red", strokeWidth=3) %>%
  dyLegend(show="always", width=500)

# Calculate stock and bond returns
re_turns <- na.omit(rutils::etf_env$re_turns[, c("VTI", "IEF")])
weight_s <- c(0.4, 0.6)
re_turns <- cbind(re_turns, re_turns %*% weight_s)
colnames(re_turns)[3] <- "Combined"
# Wealth of fixed ratio of dollar amounts
weal_th <- cumprod(1 + re_turns)
# Plot cumulative wealth
dygraphs::dygraph(log(weal_th), main="Stock and Bond Portfolio") %>%
  dyOptions(colors=c("blue","green","blue","red")) %>%
  dySeries("Combined", color="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)

# Calculate correlations
cor(re_turns)
# Calculate Sharpe ratios
sqrt(252)*sapply(re_turns, function(x) mean(x)/sd(x))
# Calculate standard deviations
sapply(re_turns, sd)
# Calculate standardized returns
re_turns <- lapply(re_turns, function(x) (x - mean(x))/sd(x))
re_turns <- do.call(cbind, re_turns)
sapply(re_turns, sd)
# Calculate skewness and kurtosis
sapply(re_turns, function(x) {
  c(skew=mean(x^3), kurt=mean(x^4))
})  # end sapply
# Or
t(sapply(c(skew=3, kurt=4), function(x)
  moments::moment(re_turns, order=x, central=TRUE)))

# Extract ETF returns
sym_bols <- c("VTI", "IEF", "DBC")
re_turns <- na.omit(rutils::etf_env$re_turns[, sym_bols])
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
chart_Series(weal_th, theme=plot_theme, lwd=c(2, 2, 2, 4),
       name="All-Weather Portfolio")
legend("topleft", legend=colnames(weal_th),
  inset=0.1, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")

# Calculate dollar and percentage returns for VTI and IEF
price_s <- rutils::etf_env$price_s[, c("VTI", "IEF")]
price_s <- na.omit(price_s)
rets_dollar <- rutils::diff_it(price_s)
rets_percent <- rets_dollar/rutils::lag_it(price_s, lagg=1, pad_zeros=FALSE)
# Calculate wealth of fixed ratio of dollar amounts
weight_s <- c(0.5, 0.5)
rets_weighted <- rets_percent %*% weight_s
wealth_fixed_ratio <- cumprod(1 + rets_weighted)
# Calculate rolling volatility
look_back <- 21
vol_at <- roll::roll_sd(rets_dollar, width=look_back)
vol_at <- zoo::na.locf(vol_at, na.rm=FALSE)
vol_at <- zoo::na.locf(vol_at, fromLast=TRUE)

# Calculate wealth of risk parity
weights_dollar <- lapply(1:NCOL(price_s),
  function(x) weight_s[x]*price_s[, x]/vol_at[, x])
weights_dollar <- do.call(cbind, weights_dollar)
weights_dollar <- weights_dollar*wealth_fixed_ratio/rowSums(weights_dollar)
weights_dollar <- rutils::lag_it(weights_dollar)
rets_weighted <- rowSums(rets_percent*weights_dollar)
wealth_risk_parity <- cumprod(1 + rets_weighted)
# Plot log wealth
weal_th <- log(cbind(wealth_fixed_ratio, wealth_risk_parity))
weal_th <- xts(weal_th, index(price_s))
colnames(weal_th) <- c("Fixed Ratio", "Risk Parity")
dygraphs::dygraph(weal_th, main="Log Wealth of Fixed Dollar Ratios vs Risk Parity") %>%
  dyOptions(colors=c("blue","red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)

# Open x11 for plotting
x11(width=6, height=5)
# Set plot parameters to reduce whitespace around plot
par(mar=c(4, 4, 3, 1), oma=c(0, 0, 0, 0))
# Test if IEF can time VTI
re_turns <- na.omit(rutils::etf_env$re_turns[, c("IEF", "VTI")])
vt_i <- re_turns$VTI
de_sign <- cbind(re_turns, 0.5*(vt_i+abs(vt_i)), vt_i^2)
colnames(de_sign)[3:4] <- c("merton", "treynor")
# Merton-Henriksson test
mod_el <- lm(IEF ~ VTI + merton, data=de_sign); summary(mod_el)
# Treynor-Mazuy test
mod_el <- lm(IEF ~ VTI + treynor, data=de_sign); summary(mod_el)
# Plot residual scatterplot
residual_s <- (de_sign$IEF - mod_el$coefficients[2]*de_sign$VTI)
plot.default(x=de_sign$VTI, y=residual_s, xlab="VTI", ylab="IEF")
title(main="Treynor-Mazuy Market Timing Test\n for IEF vs VTI", line=0.5)
# Plot fitted (predicted) response values
fit_ted <- (mod_el$fitted.values - mod_el$coefficients[2]*de_sign$VTI)
points.default(x=de_sign$VTI, y=fit_ted, pch=16, col="red")
text(x=0.05, y=0.03, paste("Treynor test t-value =", round(summary(mod_el)$coefficients["treynor", "t value"], 2)))

# Calculate positions
re_turns <- na.omit(rutils::etf_env$re_turns$VTI)
position_s <- rep(NA_integer_, NROW(re_turns))
date_s <- index(re_turns)
date_s <- format(date_s, "%m-%d")
position_s[date_s == "05-01"] <- 0
position_s[date_s == "05-03"] <- 0
position_s[date_s == "11-01"] <- 1
position_s[date_s == "11-03"] <- 1
# Carry forward and backward non-NA position_s
position_s <- zoo::na.locf(position_s, na.rm=FALSE)
position_s <- zoo::na.locf(position_s, fromLast=TRUE)
# Calculate strategy returns
sell_inmay <- position_s*re_turns
weal_th <- cbind(cumsum(re_turns),
           cumsum(sell_inmay))
colnames(weal_th) <- c("vti", "sell_in_may")

# Plot Sell in May strategy
dygraphs::dygraph(weal_th, main="Sell in May Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always")
# OR: Open x11 for plotting
x11(width=6, height=5)
par(mar=c(4, 4, 3, 1), oma=c(0, 0, 0, 0))
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("blue", "red")
chart_Series(weal_th, theme=plot_theme,
       name="Sell in May Strategy")
legend("topleft", legend=colnames(weal_th),
  inset=0.1, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")

# Test if Sell in May strategy can time VTI
vt_i <- re_turns$VTI
re_turns <- cbind(re_turns, 0.5*(vt_i+abs(vt_i)), vt_i^2)
colnames(re_turns)[2:3] <- c("merton", "treynor")
# Merton-Henriksson test
mod_el <- lm(sell_inmay ~ VTI + merton, data=re_turns); summary(mod_el)
# Treynor-Mazuy test
mod_el <- lm(sell_inmay ~ VTI + treynor, data=re_turns); summary(mod_el)
# Plot residual scatterplot
residual_s <- (sell_inmay - mod_el$coefficients[2]*vt_i)
plot.default(x=vt_i, y=residual_s, xlab="VTI", ylab="sell_inmay")
title(main="Treynor-Mazuy market timing test\n for Sell in May vs VTI", line=0.5)
# Plot fitted (predicted) response values
fit_ted <- (mod_el$fitted.values - mod_el$coefficients[2]*vt_i)
points.default(x=vt_i, y=fit_ted, pch=16, col="red")
text(x=0.05, y=0.03, paste("Treynor test t-value =", round(summary(mod_el)$coefficients["treynor", "t value"], 2)))

# Calculate the log of OHLC VTI prices
oh_lc <- log(rutils::etf_env$VTI)
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

# Plot log wealth
weal_th <- cumsum(cbind(close_close, close_open, open_close))
dygraphs::dygraph(weal_th,
  main="Wealth of Close-to-Close, Close-to-Open, and Open-to-Close Strategies") %>%
  dySeries(name="close_close", label="Close-to-Close (static)", strokeWidth=2, col="blue") %>%
  dySeries(name="close_open", label="Close-to-Open (overnight)", strokeWidth=2, col="red") %>%
  dySeries(name="open_close", label="Open-to-Close (daytime)", strokeWidth=2, col="green") %>%
  dyLegend(width=600)

# Define length for weights and decay parameter
wid_th <- 352
lamb_da <- 0.01
# Calculate EWMA prices
weight_s <- exp(-lamb_da*1:wid_th)
weight_s <- weight_s/sum(weight_s)
ew_ma <- stats::filter(clos_e, filter=weight_s,
  sides=1, method="convolution")
ew_ma[1:(wid_th-1)] <- ew_ma[wid_th]
ew_ma <- cbind(clos_e, as.numeric(ew_ma))
colnames(ew_ma) <- c("VTI", "VTI EWMA")

# Plot EWMA prices with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue")
chart_Series(ew_ma["2007/2010"], theme=plot_theme,
       name="EWMA prices")
legend("bottomleft", legend=colnames(ew_ma),
 inset=0.1, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")

# Determine trade dates right after EWMA has crossed prices
indica_tor <- sign(clos_e - ew_ma[, 2])
trade_dates <- (rutils::diff_it(indica_tor) != 0)
trade_dates <- which(trade_dates) + 1
trade_dates <- trade_dates[trade_dates < n_rows]
# Calculate positions, either: -1, 0, or 1
position_s <- rep(NA_integer_, n_rows)
position_s[1] <- 0
position_s[trade_dates] <- indica_tor[trade_dates-1]
position_s <- zoo::na.locf(position_s, na.rm=FALSE)
position_s <- xts(position_s, order.by=index(oh_lc))

# Plot EWMA prices with position shading
chart_Series(ew_ma["2007/2010"], theme=plot_theme,
       name="EWMA prices")
add_TA(position_s > 0, on=-1,
 col="lightgreen", border="lightgreen")
add_TA(position_s < 0, on=-1,
 col="lightgrey", border="lightgrey")
legend("bottomleft", legend=colnames(ew_ma),
 inset=0.1, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")

# Calculate daily profits and losses
# Calculate pnl for days without trade
pnl_s <- rutils::diff_it(clos_e)*position_s
# Calculate realized pnl for days with trade
close_lag <- rutils::lag_it(clos_e)
pos_lagged <- rutils::lag_it(position_s)
pnl_s[trade_dates] <- pos_lagged[trade_dates]*
  (op_en[trade_dates] - close_lag[trade_dates])
# Calculate unrealized pnl for days with trade
pnl_s[trade_dates] <- pnl_s[trade_dates] +
  position_s[trade_dates]*
  (clos_e[trade_dates] - op_en[trade_dates])
# Annualized Sharpe ratio of EWMA strategy
sqrt(252)*sum(pnl_s)/sd(pnl_s)/NROW(pnl_s)
# Cumulative pnls
cum_pnls <- star_t + cumsum(pnl_s)
cum_pnls <- cbind(clos_e, cum_pnls)
colnames(cum_pnls) <- c("VTI", "EWMA PnL")

# Plot EWMA PnL with position shading
chart_Series(cum_pnls, theme=plot_theme,
       name="Performance of EWMA Strategy")
add_TA(position_s > 0, on=-1,
 col="lightgreen", border="lightgreen")
add_TA(position_s < 0, on=-1,
 col="lightgrey", border="lightgrey")
legend("top", legend=colnames(cum_pnls),
 inset=0.05, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")

# bid_offer equal to 10 bps for liquid ETFs
bid_offer <- 0.001
# Calculate transaction costs
cost_s <- 0.5*bid_offer*abs(pos_lagged - position_s)*clos_e
# pnl_s <- (pnl_s - cost_s)
# Plot strategy with transaction costs
cum_pnls <- star_t + cumsum(pnl_s)
cum_pnls <- cbind(cum_pnls, cum_pnls - cumsum(cost_s))
colnames(cum_pnls) <- c(sym_bol, "costs")
dygraphs::dygraph(cum_pnls, main=paste(sym_bol, "EWMA Strategy With Transaction Costs")) %>%
  dySeries(name="costs", label="Strategy With Transaction Costs", strokeWidth=2, col="green") %>%
  dySeries(name=sym_bol, label="EWMA Strategy", strokeWidth=2, col="blue")

simu_ewma <- function(oh_lc, lamb_da=0.01, wid_th=251, bid_offer=0.001, tre_nd=1) {
  n_rows <- NROW(oh_lc)
  # Calculate EWMA prices
  weight_s <- exp(-lamb_da*1:wid_th)
  weight_s <- weight_s/sum(weight_s)
  clos_e <- quantmod::Cl(oh_lc)
  ew_ma <- stats::filter(clos_e, filter=weight_s, sides=1, method="convolution")
  ew_ma[1:(wid_th-1)] <- ew_ma[wid_th]
  # Determine trade dates right after EWMA has crossed prices
  indica_tor <- tre_nd*sign(clos_e - as.numeric(ew_ma))
  trade_dates <- (rutils::diff_it(indica_tor) != 0)
  trade_dates <- which(trade_dates) + 1
  trade_dates <- trade_dates[trade_dates < n_rows]
  # Calculate positions, either: -1, 0, or 1
  position_s <- rep(NA_integer_, n_rows)
  position_s[1] <- 0
  position_s[trade_dates] <- indica_tor[trade_dates-1]
  position_s <- zoo::na.locf(position_s, na.rm=FALSE)
  op_en <- quantmod::Op(oh_lc)
  close_lag <- rutils::lag_it(clos_e)
  pos_lagged <- rutils::lag_it(position_s)
  # Calculate daily profits and losses
  pnl_s <- rutils::diff_it(clos_e)*position_s
  pnl_s[trade_dates] <- pos_lagged[trade_dates]*
    (op_en[trade_dates] - close_lag[trade_dates])
  pnl_s[trade_dates] <- pnl_s[trade_dates] +
    position_s[trade_dates]*
    (clos_e[trade_dates] - op_en[trade_dates])
  # Calculate transaction costs
  cost_s <- 0.5*bid_offer*abs(pos_lagged - position_s)*clos_e
  pnl_s <- (pnl_s - cost_s)
  # Calculate strategy returns
  pnl_s <- cbind(position_s, pnl_s)
  colnames(pnl_s) <- c("positions", "pnls")
  pnl_s
}  # end simu_ewma

source("C:/Develop/lecture_slides/scripts/ewma_model.R")
lamb_das <- seq(from=1e-5, to=0.05, by=0.01)
# Perform lapply() loop over lamb_das
pnl_s <- lapply(lamb_das, function(lamb_da) {
  # Simulate EWMA strategy and calculate re_turns
  star_t + cumsum(simu_ewma(oh_lc=oh_lc,
    lamb_da=lamb_da, wid_th=wid_th)[, "pnls"])
})  # end lapply
pnl_s <- do.call(cbind, pnl_s)
colnames(pnl_s) <- paste0("lambda=", lamb_das)

# Plot EWMA strategies with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(pnl_s))
chart_Series(pnl_s, theme=plot_theme,
  name="Cumulative Returns of EWMA Strategies")
legend("topleft", legend=colnames(pnl_s), inset=0.1,
  bg="white", cex=0.8, lwd=rep(6, NCOL(pnl_s)),
  col=plot_theme$col$line.col, bty="n")

# Initialize compute cluster under Windows
library(parallel)
clus_ter <- makeCluster(detectCores()-1)
clusterExport(clus_ter,
  varlist=c("oh_lc", "wid_th", "simu_ewma"))
# Perform parallel loop over lamb_das under Windows
pnl_s <- parLapply(clus_ter, lamb_das, function(lamb_da) {
  library(quantmod)
  # Simulate EWMA strategy and calculate re_turns
  star_t + cumsum(simu_ewma(oh_lc=oh_lc,
    lamb_da=lamb_da, wid_th=wid_th)[, "pnls"])
})  # end parLapply
# Perform parallel loop over lamb_das under Mac-OSX or Linux
re_turns <- mclapply(lamb_das, function(lamb_da) {
  library(quantmod)
  # Simulate EWMA strategy and calculate re_turns
  star_t + cumsum(simu_ewma(oh_lc=oh_lc,
    lamb_da=lamb_da, wid_th=wid_th)[, "pnls"])
})  # end mclapply
stopCluster(clus_ter)  # Stop R processes over cluster under Windows
pnl_s <- do.call(cbind, pnl_s)
colnames(pnl_s) <- paste0("lambda=", lamb_das)

sharpe_ratios <- sqrt(252)*sapply(pnl_s, function(x_ts) {
  # Calculate annualized Sharpe ratio of strategy returns
  x_ts <- rutils::diff_it(x_ts)
  sum(x_ts)/sd(x_ts)
})/NROW(pnl_s)  # end sapply
plot(x=lamb_das, y=sharpe_ratios, t="l",
     main="Performance of EWMA trend following strategies
     as function of the decay parameter lambda")
trend_returns <- rutils::diff_it(pnl_s)
trend_sharpe <- sharpe_ratios

# Simulate best performing strategy
ewma_trend <- simu_ewma(oh_lc=oh_lc,
  lamb_da=lamb_das[which.max(sharpe_ratios)],
  wid_th=wid_th)
position_s <- ewma_trend[, "positions"]
pnl_s <- star_t + cumsum(ewma_trend[, "pnls"])
pnl_s <- cbind(clos_e, pnl_s)
colnames(pnl_s) <- c("VTI", "EWMA PnL")
# Plot EWMA PnL with position shading
plot_theme$col$line.col <- c("orange", "blue")
chart_Series(pnl_s, theme=plot_theme,
       name="Performance of Trend Following EWMA Strategy")
add_TA(position_s > 0, on=-1,
 col="lightgreen", border="lightgreen")
add_TA(position_s < 0, on=-1,
 col="lightgrey", border="lightgrey")
legend("top", legend=colnames(pnl_s),
  inset=0.05, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")

source("C:/Develop/lecture_slides/scripts/ewma_model.R")
lamb_das <- seq(0.05, 1.0, 0.05)
# Perform lapply() loop over lamb_das
pnl_s <- lapply(lamb_das, function(lamb_da) {
  # Backtest EWMA strategy and calculate re_turns
  star_t + cumsum(simu_ewma(
    oh_lc=oh_lc, lamb_da=lamb_da, wid_th=wid_th, tre_nd=(-1))[, "pnls"])
})  # end lapply
pnl_s <- do.call(cbind, pnl_s)
colnames(pnl_s) <- paste0("lambda=", lamb_das)
# Plot EWMA strategies with custom line colors
column_s <- seq(1, NCOL(pnl_s), by=4)
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NROW(column_s))
chart_Series(pnl_s[, column_s],
  theme=plot_theme, name="Cumulative Returns of Mean Reverting EWMA Strategies")
legend("topleft", legend=colnames(pnl_s[, column_s]),
  inset=0.1, bg="white", cex=0.8, lwd=6,
  col=plot_theme$col$line.col, bty="n")

sharpe_ratios <- sqrt(252)*sapply(pnl_s, function(x_ts) {
  # Calculate annualized Sharpe ratio of strategy returns
  x_ts <- rutils::diff_it(x_ts)
  sum(x_ts)/sd(x_ts)
})/NROW(pnl_s)  # end sapply
plot(x=lamb_das, y=sharpe_ratios, t="l",
     main="Performance of EWMA mean reverting strategies
     as function of the decay parameter lambda")
revert_returns <- rutils::diff_it(pnl_s)
revert_sharpe <- sharpe_ratios

# Backtest best performing strategy
ewma_revert <- simu_ewma(oh_lc=oh_lc, bid_offer=0.0,
  lamb_da=lamb_das[which.max(sharpe_ratios)],
  wid_th=wid_th, tre_nd=(-1))
position_s <- ewma_revert[, "positions"]
pnl_s <- star_t + cumsum(ewma_revert[, "pnls"])
pnl_s <- cbind(clos_e, pnl_s)
colnames(pnl_s) <- c("VTI", "EWMA PnL")

# Plot EWMA PnL with position shading
plot_theme$col$line.col <- c("orange", "blue")
chart_Series(pnl_s, theme=plot_theme,
       name="Performance of Mean Reverting EWMA Strategy")
add_TA(position_s > 0, on=-1,
 col="lightgreen", border="lightgreen")
add_TA(position_s < 0, on=-1,
 col="lightgrey", border="lightgrey")
legend("top", legend=colnames(pnl_s),
  inset=0.05, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")

# Calculate correlation between trend following and mean reverting strategies
trend_ing <- ewma_trend[, "pnls"]
colnames(trend_ing) <- "trend"
revert_ing <- ewma_revert[, "pnls"]
colnames(revert_ing) <- "revert"
close_rets <- rutils::diff_it(clos_e)
cor(cbind(trend_ing, revert_ing, close_rets))
# Calculate combined strategy
com_bined <- trend_ing + revert_ing
colnames(com_bined) <- "combined"
# Calculate annualized Sharpe ratio of strategy returns
re_turns <- cbind(close_rets, trend_ing, revert_ing, com_bined)
sqrt(252)*sapply(re_turns, function(x_ts)
  sum(x_ts)/sd(x_ts))/NROW(com_bined)
pnl_s <- lapply(re_turns, function(x_ts) {star_t + cumsum(x_ts)})
pnl_s <- do.call(cbind, pnl_s)
colnames(pnl_s) <- c("VTI", "trending", "reverting", "EWMA combined PnL")

plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue", "green", "magenta2")
chart_Series(pnl_s, theme=plot_theme,
       name="Performance of Combined EWMA Strategies")
legend("topleft", legend=colnames(pnl_s),
 inset=0.05, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")

weight_s <- c(trend_sharpe, revert_sharpe)
weight_s[weight_s<0] <- 0
weight_s <- weight_s/sum(weight_s)
re_turns <- cbind(trend_returns, revert_returns)
avg_returns <- re_turns %*% weight_s
avg_returns <- xts(avg_returns, order.by=index(re_turns))
pnl_s <- (star_t + cumsum(avg_returns))
pnl_s <- cbind(clos_e, pnl_s)
colnames(pnl_s) <- c("VTI", "EWMA PnL")
# Plot EWMA PnL without position shading
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue")
chart_Series(pnl_s, theme=plot_theme,
  name="Performance of Ensemble EWMA Strategy")
legend("top", legend=colnames(pnl_s),
  inset=0.05, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")

# Calculate open, close, and lagged prices
sym_bol <- "VTI"
# oh_lc <- rutils::etf_env$VTI
oh_lc <- get(sym_bol, rutils::etf_env)
n_rows <- NROW(oh_lc)
op_en <- quantmod::Op(oh_lc)
clos_e <- quantmod::Cl(oh_lc)
vol_ume <- quantmod::Vo(oh_lc)
star_t <- as.numeric(clos_e[1])
# Define aggregation interval and calculate VWAP
look_back <- 200
v_wap <- rutils::roll_sum(x_ts=clos_e*vol_ume,
  look_back=look_back)
volume_roll <- rutils::roll_sum(x_ts=vol_ume,
  look_back=look_back)
v_wap <- v_wap/volume_roll
v_wap[is.na(v_wap)] <- 0

# Plot prices and VWAP
x11(width=6, height=5)
chart_Series(x=clos_e,
  name="VTI prices and VWAP", col="orange")
add_TA(v_wap, on=1, lwd=2, col="blue")
legend("top", legend=c("VTI", "VWAP"),
  bg="white", lty=1, lwd=6,
  col=c("orange", "blue"), bty="n")

# Calculate VWAP indicator
indica_tor <- sign(clos_e - v_wap)
# Calculate positions as lagged indicator
position_s <- rutils::lag_it(indica_tor)
# Calculate daily profits and losses of strategy
re_turns <- rutils::diff_it(clos_e)
pnl_s <- re_turns*position_s
cum_pnls <- star_t + cumsum(pnl_s)
# Annualized Sharpe ratio of VWAP strategy
sqrt(252)*sum(pnl_s)/sd(pnl_s)/NROW(pnl_s)
# Annualized Sharpe ratio of VTI
sqrt(252)*sum(re_turns)/sd(re_turns)/NROW(pnl_s)
# Plot prices and VWAP
chart_Series(x=clos_e, name="VWAP Crossover Strategy for VTI", col="orange")
add_TA(cum_pnls, on=1, lwd=2, col="blue")
add_TA(position_s > 0, on=-1,
 col="lightgreen", border="lightgreen")
add_TA(position_s < 0, on=-1,
 col="lightgrey", border="lightgrey")
legend("top", legend=c(sym_bol, "VWAP strategy"),
 inset=0.1, bg="white", lty=1, lwd=6,
 col=c("orange", "blue"), bty="n")

# Plot prices and VWAP
da_ta <- cbind(clos_e, cum_pnls, v_wap)
colnames(da_ta) <- c(sym_bol, "strategy", "vwap")
dygraphs::dygraph(da_ta, main=paste(sym_bol, "VWAP Strategy")) %>%
  dyAxis("y", label=sym_bol, independentTicks=TRUE) %>%
  dyAxis("y2", label="strategy", independentTicks=TRUE) %>%
  dySeries(name=sym_bol, axis="y", label=sym_bol, strokeWidth=2, col="red") %>%
  dySeries(name="vwap", axis="y", label="vwap", strokeWidth=2, col="green") %>%
  dySeries(name="strategy", axis="y2", label="strategy", strokeWidth=2, col="blue")

# Calculate positions from lagged indicator
lagg <- 2
indic_sum <- roll::roll_sum(indica_tor, width=lagg)
indic_sum[1:lagg] <- 0
position_s <- rep(NA_integer_, NROW(clos_e))
position_s[1] <- 0
position_s <- ifelse(indic_sum == lagg, 1, position_s)
position_s <- ifelse(indic_sum == (-lagg), -1, position_s)
position_s <- zoo::na.locf(position_s, na.rm=FALSE)
# Lag the positions to trade in next period
position_s <- rutils::lag_it(position_s, lagg=1)

# Calculate PnLs of lagged strategy
pnl_s <- re_turns*position_s
cum_pnls_lag <- star_t + cumsum(pnl_s)
# Plot both strategies
da_ta <- cbind(cum_pnls_lag, cum_pnls)
colnames(da_ta) <- c("lag_strategy", "strategy")
dygraphs::dygraph(da_ta, main=paste(sym_bol, "VWAP Strategy With Lag")) %>%
  dySeries(name="lag_strategy", label="Strategy With Lag", strokeWidth=2, col="green") %>%
  dySeries(name="strategy", label="Strategy", strokeWidth=2, col="blue")
