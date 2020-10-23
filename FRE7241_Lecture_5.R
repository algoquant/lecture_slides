# Open x11 for plotting
x11(width=6, height=4)
# Set plot parameters to reduce whitespace around plot
par(mar=c(4, 4, 3, 1), oma=c(0, 0, 0, 0))
# Test if IEF can time VTI
re_turns <- na.omit(rutils::etf_env$re_turns[, c("IEF", "VTI")])
vt_i <- re_turns$VTI
re_turns <- cbind(re_turns, 0.5*(vt_i+abs(vt_i)), vt_i^2)
colnames(re_turns)[3:4] <- c("merton", "treynor")
# Merton-Henriksson test
mod_el <- lm(IEF ~ VTI + merton, data=re_turns); summary(mod_el)
# Treynor-Mazuy test
mod_el <- lm(IEF ~ VTI + treynor, data=re_turns); summary(mod_el)
# Plot residual scatterplot
residual_s <- (re_turns$IEF - mod_el$coefficients[2]*re_turns$VTI)
plot.default(x=re_turns$VTI, y=residual_s, xlab="VTI", ylab="IEF")
title(main="Treynor-Mazuy market timing test\n for IEF vs VTI", line=0.5)
# Plot fitted (predicted) response values
fit_ted <- (mod_el$fitted.values - mod_el$coefficients[2]*re_turns$VTI)
points.default(x=re_turns$VTI, y=fit_ted, pch=16, col="red")
text(x=0.05, y=0.03, paste("Treynor test t-value =", round(summary(mod_el)$coefficients["treynor", "t value"], 2)))

# Calculate positions
re_turns <- na.omit(rutils::etf_env$re_turns$VTI)
position_s <- rep(NA_integer_, NROW(re_turns))
in_dex <- index(re_turns)
in_dex <- format(in_dex, "%m-%d")
position_s[in_dex == "05-01"] <- 0
position_s[in_dex == "05-03"] <- 0
position_s[in_dex == "11-01"] <- 1
position_s[in_dex == "11-03"] <- 1
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
x11(width=6, height=4)
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

# Calculate open, close, and lagged prices
sym_bol <- "VTI"
# oh_lc <- rutils::etf_env$VTI
oh_lc <- get(sym_bol, rutils::etf_env)
op_en <- quantmod::Op(oh_lc)
cl_ose <- quantmod::Cl(oh_lc)
vol_ume <- quantmod::Vo(oh_lc)
star_t <- as.numeric(cl_ose[1])
# Define aggregation interval and calculate VWAP
look_back <- 200
v_wap <- rutils::roll_sum(x_ts=cl_ose*vol_ume,
  look_back=look_back)
volume_roll <- rutils::roll_sum(x_ts=vol_ume,
  look_back=look_back)
v_wap <- v_wap/volume_roll
v_wap[is.na(v_wap)] <- 0

# Plot prices and VWAP
x11(width=6, height=5)
chart_Series(x=cl_ose,
  name="VTI prices and VWAP", col="orange")
add_TA(v_wap, on=1, lwd=2, col="blue")
legend("top", legend=c("VTI", "VWAP"),
  bg="white", lty=1, lwd=6,
  col=c("orange", "blue"), bty="n")

# Calculate VWAP indicator
indica_tor <- sign(cl_ose - v_wap)
# Calculate positions as lagged indicator
position_s <- rutils::lag_it(indica_tor)
# Calculate daily profits and losses of strategy
re_turns <- rutils::diff_it(cl_ose)
pnl_s <- re_turns*position_s
cum_pnls <- star_t + cumsum(pnl_s)
# Annualized Sharpe ratio of VWAP strategy
sqrt(252)*sum(pnl_s)/sd(pnl_s)/NROW(pnl_s)
# Annualized Sharpe ratio of VTI
sqrt(252)*sum(re_turns)/sd(re_turns)/NROW(pnl_s)
# Plot prices and VWAP
chart_Series(x=cl_ose, name="VWAP Crossover Strategy for VTI", col="orange")
add_TA(cum_pnls, on=1, lwd=2, col="blue")
add_TA(position_s > 0, on=-1,
 col="lightgreen", border="lightgreen")
add_TA(position_s < 0, on=-1,
 col="lightgrey", border="lightgrey")
legend("top", legend=c(sym_bol, "VWAP strategy"),
 inset=0.1, bg="white", lty=1, lwd=6,
 col=c("orange", "blue"), bty="n")

# Plot prices and VWAP
da_ta <- cbind(cl_ose, cum_pnls, v_wap)
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
position_s <- rep(NA_integer_, NROW(cl_ose))
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

# Determine dates right after VWAP has crossed prices
trade_dates <- (rutils::diff_it(indica_tor) != 0)
trade_dates <- which(trade_dates) + 1
# Calculate positions, either: -1, 0, or 1
position_s <- rep(NA_integer_, NROW(oh_lc))
position_s[1] <- 0
position_s[trade_dates] <- indica_tor[trade_dates-1]
position_s <- zoo::na.locf(position_s, na.rm=FALSE)
position_s <- xts(position_s, order.by=index(oh_lc))
pos_lagged <- rutils::lag_it(position_s)
# Calculate pnl for days without trade
pnl_s <- rutils::diff_it(cl_ose)*position_s
# Calculate realized pnl for days with trade
close_lag <- rutils::lag_it(cl_ose)
pnl_s[trade_dates] <- pos_lagged[trade_dates]*
  (op_en[trade_dates] - close_lag[trade_dates])
# Calculate unrealized pnl for days with trade
pnl_s[trade_dates] <- pnl_s[trade_dates] +
  position_s[trade_dates]*
  (cl_ose[trade_dates] - op_en[trade_dates])
cum_pnls_open <- star_t + cumsum(pnl_s)
# Annualized Sharpe ratio of VWAP strategy
sqrt(252)*sum(pnl_s)/sd(pnl_s)/NROW(pnl_s)
# Annualized Sharpe ratio of VTI
sqrt(252)*sum(re_turns)/sd(re_turns)/NROW(pnl_s)

# Plot both strategies
da_ta <- cbind(cum_pnls_open, cum_pnls)
colnames(da_ta) <- c("strategy_open", "strategy")
dygraphs::dygraph(da_ta, main=paste(sym_bol, "VWAP Strategy on Open")) %>%
  dySeries(name="strategy_open", label="Strategy on Open", strokeWidth=2, col="green") %>%
  dySeries(name="strategy", label="Strategy", strokeWidth=2, col="blue")
# Plot VTI and VWAP strategy
chart_Series(x=cl_ose, name="VWAP Crossover Strategy for VTI Trade at Open Price", col="orange")
add_TA(cum_pnls, on=1, lwd=2, col="blue")
add_TA(position_s > 0, on=-1,
 col="lightgreen", border="lightgreen")
add_TA(position_s < 0, on=-1,
 col="lightgrey", border="lightgrey")
legend("top", legend=c(sym_bol, "VWAP strategy"),
 inset=0.1, bg="white", lty=1, lwd=6,
 col=c("orange", "blue"), bty="n")

# Define length for weights and decay parameter
wid_th <- 352
lamb_da <- 0.01
# Calculate EWMA prices
weight_s <- exp(-lamb_da*1:wid_th)
weight_s <- weight_s/sum(weight_s)
ew_ma <- stats::filter(cl_ose, filter=weight_s,
  sides=1, method="convolution")
ew_ma[1:(wid_th-1)] <- ew_ma[wid_th]
ew_ma <- cbind(cl_ose, as.numeric(ew_ma))
colnames(ew_ma) <- c("VTI", "VTI EWMA")

# Plot EWMA prices with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue")
chart_Series(ew_ma["2007/2010"], theme=plot_theme,
       name="EWMA prices")
legend("bottomleft", legend=colnames(ew_ma),
 inset=0.1, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")

# Determine dates right after VWAP has crossed prices
indica_tor <- sign(cl_ose - ew_ma[, 2])
trade_dates <- (rutils::diff_it(indica_tor) != 0)
trade_dates <- which(trade_dates) + 1
# Calculate positions, either: -1, 0, or 1
position_s <- rep(NA_integer_, NROW(oh_lc))
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
pnl_s <- rutils::diff_it(cl_ose)*position_s
# Calculate realized pnl for days with trade
close_lag <- rutils::lag_it(cl_ose)
pos_lagged <- rutils::lag_it(position_s)
pnl_s[trade_dates] <- pos_lagged[trade_dates]*
  (op_en[trade_dates] - close_lag[trade_dates])
# Calculate unrealized pnl for days with trade
pnl_s[trade_dates] <- pnl_s[trade_dates] +
  position_s[trade_dates]*
  (cl_ose[trade_dates] - op_en[trade_dates])
# Annualized Sharpe ratio of EWMA strategy
sqrt(252)*sum(pnl_s)/sd(pnl_s)/NROW(pnl_s)
# Cumulative pnls
cum_pnls <- star_t + cumsum(pnl_s)
cum_pnls <- cbind(cl_ose, cum_pnls)
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
cost_s <- 0.5*bid_offer*abs(pos_lagged - position_s)*cl_ose
# pnl_s <- (pnl_s - cost_s)
# Plot strategy with transaction costs
cum_pnls <- star_t + cumsum(pnl_s)
cum_pnls <- cbind(cum_pnls, cum_pnls - cumsum(cost_s))
colnames(cum_pnls) <- c(sym_bol, "costs")
dygraphs::dygraph(cum_pnls, main=paste(sym_bol, "EWMA Strategy With Transaction Costs")) %>%
  dySeries(name="costs", label="Strategy With Transaction Costs", strokeWidth=2, col="green") %>%
  dySeries(name=sym_bol, label="EWMA Strategy", strokeWidth=2, col="blue")

simu_ewma <- function(oh_lc, lamb_da=0.01, wid_th=251, bid_offer=0.001, tre_nd=1) {
  # Calculate EWMA prices
  weight_s <- exp(-lamb_da*1:wid_th)
  weight_s <- weight_s/sum(weight_s)
  cl_ose <- quantmod::Cl(oh_lc)
  ew_ma <- stats::filter(cl_ose, filter=weight_s, sides=1, method="convolution")
  ew_ma[1:(wid_th-1)] <- ew_ma[wid_th]
  # Determine dates right after EWMA has crossed prices
  indica_tor <- tre_nd*sign(cl_ose - as.numeric(ew_ma))
  trade_dates <- (rutils::diff_it(indica_tor) != 0)
  trade_dates <- which(trade_dates) + 1
  trade_dates <- trade_dates[trade_dates < NROW(oh_lc)]
  # Calculate positions, either: -1, 0, or 1
  position_s <- rep(NA_integer_, NROW(oh_lc))
  position_s[1] <- 0
  position_s[trade_dates] <- indica_tor[trade_dates-1]
  position_s <- zoo::na.locf(position_s, na.rm=FALSE)
  op_en <- quantmod::Op(oh_lc)
  close_lag <- rutils::lag_it(cl_ose)
  pos_lagged <- rutils::lag_it(position_s)
  # Calculate daily profits and losses
  pnl_s <- rutils::diff_it(cl_ose)*position_s
  pnl_s[trade_dates] <- pos_lagged[trade_dates]*
    (op_en[trade_dates] - close_lag[trade_dates])
  pnl_s[trade_dates] <- pnl_s[trade_dates] +
    position_s[trade_dates]*
    (cl_ose[trade_dates] - op_en[trade_dates])
  # Calculate transaction costs
  cost_s <- 0.5*bid_offer*abs(pos_lagged - position_s)*cl_ose
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
pnl_s <- cbind(cl_ose, pnl_s)
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
pnl_s <- cbind(cl_ose, pnl_s)
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
close_rets <- rutils::diff_it(cl_ose)
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
pnl_s <- cbind(cl_ose, pnl_s)
colnames(pnl_s) <- c("VTI", "EWMA PnL")
# Plot EWMA PnL without position shading
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue")
chart_Series(pnl_s, theme=plot_theme,
  name="Performance of Ensemble EWMA Strategy")
legend("top", legend=colnames(pnl_s),
  inset=0.05, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")

options(width=50, dev='pdf')
str(optimize)
# Objective function with multiple minima
object_ive <- function(in_put, param1=0.01) {
  sin(0.25*pi*in_put) + param1*(in_put-1)^2
}  # end object_ive
unlist(optimize(f=object_ive, interval=c(-4, 2)))
unlist(optimize(f=object_ive, interval=c(0, 8)))
options(width=80, dev='pdf')

par(oma=c(1, 1, 1, 1), mgp=c(2, 1, 0), mar=c(5, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# Plot the objective function
curve(expr=object_ive, type="l", xlim=c(-8, 9),
xlab="", ylab="", lwd=2)
# Add title
title(main="Objective Function", line=-1)

library(rgl)  # Load rgl
# Define function of two variables
sur_face <- function(x, y) y*sin(x)
# Draw 3d surface plot of function
persp3d(x=sur_face, xlim=c(-5, 5), ylim=c(-5, 5),
  col="green", axes=FALSE)
# Draw 3d surface plot of matrix
x_lim <- seq(from=-5, to=5, by=0.1)
y_lim <- seq(from=-5, to=5, by=0.1)
persp3d(z=outer(x_lim, y_lim, FUN=sur_face),
  xlab="x", ylab="y", zlab="sur_face",
  col="green")
# Save current view to png file
rgl.snapshot("surface_plot.png")
# Define function of two variables and two parameters
sur_face <- function(x, y, par_1=1, par_2=1)
  sin(par_1*x)*sin(par_2*y)
# Draw 3d surface plot of function
persp3d(x=sur_face, xlim=c(-5, 5), ylim=c(-5, 5),
  col="green", axes=FALSE,
  par_1=1, par_2=2)

# Rastrigin function with vector argument for optimization
rastri_gin <- function(vec_tor, pa_ram=25){
  sum(vec_tor^2 - pa_ram*cos(vec_tor))
}  # end rastri_gin
vec_tor <- c(pi/6, pi/6)
rastri_gin(vec_tor=vec_tor)
# Draw 3d surface plot of Rastrigin function
rgl::persp3d(
  x=Vectorize(function(x, y) rastri_gin(vec_tor=c(x, y))),
  xlim=c(-10, 10), ylim=c(-10, 10),
  col="green", axes=FALSE, zlab="", main="rastri_gin")
# Optimize with respect to vector argument
op_tim <- optim(par=vec_tor, fn=rastri_gin,
        method="L-BFGS-B",
        upper=c(4*pi, 4*pi),
        lower=c(pi/2, pi/2),
        pa_ram=1)
# Optimal parameters and value
op_tim$par
op_tim$value
rastri_gin(op_tim$par, pa_ram=1)

# Rastrigin function with vector argument for optimization
rastri_gin <- function(vec_tor, pa_ram=25){
  sum(vec_tor^2 - pa_ram*cos(vec_tor))
}  # end rastri_gin
vec_tor <- c(pi/6, pi/6)
rastri_gin(vec_tor=vec_tor)
library(DEoptim)
Optimize rastri_gin using DEoptim
op_tim <-  DEoptim(rastri_gin,
                   upper=c(4*pi, 4*pi),
                   lower=c(pi/2, pi/2),
                   DEoptim.control(trace=FALSE, itermax=50))
# Optimal parameters and value
op_tim$optim$bestmem
rastri_gin(op_tim$optim$bestmem)
summary(op_tim)
plot(op_tim)

library(rutils)
# Select ETF symbols
sym_bols <- c("IEF", "DBC", "XLU", "XLF", "XLP", "XLI")
# Calculate ETF prices and percentage returns
price_s <- rutils::etf_env$price_s[, sym_bols]
price_s <- zoo::na.locf(price_s, na.rm=FALSE)
price_s <- zoo::na.locf(price_s, fromLast=TRUE)
re_turns <- rutils::diff_it(log(price_s))
# Center (de-mean) and scale the returns
re_turns <- lapply(re_turns, function(x) {(x - mean(x))/sd(x)})
re_turns <- rutils::do_call(cbind, re_turns)
# Alternative (much slower) center (de-mean) and scale the returns
# re_turns <- apply(re_turns, 2, scale)
# re_turns <- xts(re_turns, index(price_s))
# Alternative (much slower) center (de-mean) and scale the returns
# re_turns <- scale(re_turns, center=TRUE, scale=TRUE)
# re_turns <- xts(re_turns, index(price_s))
# Alternative (much slower) center (de-mean) and scale the returns
# re_turns <- t(re_turns) - colMeans(re_turns)
# re_turns <- re_turns/sqrt(rowSums(re_turns^2)/(NCOL(re_turns)-1))
# re_turns <- t(re_turns)
# re_turns <- xts(re_turns, index(price_s))
# Covariance matrix and variance vector of returns
cov_mat <- cov(re_turns)
vari_ance <- diag(cov_mat)
cor_mat <- cor(re_turns)
# Or
# cov_mat <- crossprod(re_turns) / (NROW(re_turns)-1)
# cor_mat <- cov_mat / sqrt(vari_ance)
# cor_mat <- t(t(cor_mat) / sqrt(vari_ance))

# Reorder correlation matrix based on clusters
library(corrplot)
or_der <- corrMatOrder(cor_mat, order="hclust", hclust.method="complete")
cor_mat <- cor_mat[or_der, or_der]
# Plot the correlation matrix
col_ors <- colorRampPalette(c("red", "white", "blue"))
x11(width=6, height=6)
corrplot(cor_mat, tl.col="black", mar=c(0,0,0,0),
    method="square", col=col_ors(NCOL(cor_mat)), tl.cex=0.8,
    cl.offset=0.75, cl.cex=0.7, cl.align.text="l", cl.ratio=0.25)
title("ETF Correlation Matrix", line=1)
# Draw rectangles on the correlation matrix plot
corrRect.hclust(cor_mat, k=NROW(cor_mat) %/% 2, method="complete", col="red")

# create initial vector of portfolio weights
n_weights <- NROW(sym_bols)
weight_s <- rep(1/sqrt(n_weights), n_weights)
names(weight_s) <- sym_bols
# Objective function equal to minus portfolio variance
object_ive <- function(weight_s, re_turns) {
  portf_rets <- re_turns %*% weight_s
  -sum(portf_rets^2) + 1e7*(1 - sum(weight_s^2))^2
}  # end object_ive
# Objective for equal weight portfolio
object_ive(weight_s, re_turns)
# Compare speed of vector multiplication methods
summary(microbenchmark(
  trans_pose=(t(re_turns[, 1]) %*% re_turns[, 1]),
  s_um=sum(re_turns[, 1]^2),
  times=10))[, c(1, 4, 5)]

# Find weights with maximum variance
optim_run <- optim(par=weight_s,
  fn=object_ive,
  re_turns=re_turns,
  method="L-BFGS-B",
  upper=rep(10.0, n_weights),
  lower=rep(-10.0, n_weights))
# Optimal weights and maximum variance
weight_s <- optim_run$par
-object_ive(weight_s, re_turns)
# Plot first principal component weights
barplot(weight_s, names.arg=names(weight_s), xlab="", ylab="",
  main="First Principal Component Weights")

# pc1 weights and returns
weights_1 <- weight_s
pc_1 <- re_turns %*% weights_1
# Redefine objective function
object_ive <- function(weight_s, re_turns) {
  portf_rets <- re_turns %*% weight_s
  -sum(portf_rets^2) + 1e7*(1 - sum(weight_s^2))^2 +
    1e7*(sum(weights_1*weight_s))^2
}  # end object_ive
# Find second PC weights using parallel DEoptim
optim_run <- DEoptim::DEoptim(fn=object_ive,
  upper=rep(10, NCOL(re_turns)),
  lower=rep(-10, NCOL(re_turns)),
  re_turns=re_turns, control=list(parVar="weights_1",
    trace=FALSE, itermax=1000, parallelType=1))

# pc2 weights and returns
weights_2 <- optim_run$optim$bestmem
names(weights_2) <- colnames(re_turns)
sum(weights_2^2)
sum(weights_1*weights_2)
# Plot second principal component loadings
barplot(weights_2, names.arg=names(weights_2), xlab="", ylab="",
  main="Second Principal Component Loadings")

# Calculate eigenvectors and eigenvalues
ei_gen <- eigen(cov_mat)
ei_gen$vectors
weights_1
weights_2
ei_gen$values[1]
var(pc_1)
(cov_mat %*% weights_1) / weights_1
ei_gen$values[2]
var(pc_2)
(cov_mat %*% weights_2) / weights_2
sum(vari_ance)
sum(ei_gen$values)
barplot(ei_gen$values, # Plot eigenvalues
  names.arg=paste0("PC", 1:n_weights),
  las=3, xlab="", ylab="", main="Principal Component Variances")

# Eigen decomposition of covariance matrix
re_turns <- rutils::diff_it(price_s)
cov_mat <- cov(re_turns)
ei_gen <- eigen(cov_mat)
# Perform PCA without scaling
pc_a <- prcomp(re_turns, scale=FALSE)
# Compare outputs
all.equal(ei_gen$values, pc_a$sdev^2)
all.equal(abs(ei_gen$vectors), abs(pc_a$rotation),
    check.attributes=FALSE)
# Eigen decomposition of correlation matrix
cor_mat <- cor(re_turns)
ei_gen <- eigen(cor_mat)
# Perform PCA with scaling
pc_a <- prcomp(re_turns, scale=TRUE)
# Compare outputs
all.equal(ei_gen$values, pc_a$sdev^2)
all.equal(abs(ei_gen$vectors), abs(pc_a$rotation),
    check.attributes=FALSE)

# Redefine objective function to minimize variance
object_ive <- function(weight_s, re_turns) {
  portf_rets <- re_turns %*% weight_s
  sum(portf_rets^2) + 1e7*(1 - sum(weight_s^2))^2
}  # end object_ive
# Find highest order PC weights using parallel DEoptim
optim_run <- DEoptim::DEoptim(fn=object_ive,
  upper=rep(10, NCOL(re_turns)),
  lower=rep(-10, NCOL(re_turns)),
  re_turns=re_turns, control=list(trace=FALSE,
    itermax=1000, parallelType=1))
# pc6 weights and returns
weights_6 <- optim_run$optim$bestmem
names(weights_6) <- colnames(re_turns)
sum(weights_6^2)
sum(weights_1*weights_6)
# Compare with ei_gen vector
weights_6
ei_gen$vectors[, 6]
# Calculate objective function
object_ive(weights_6, re_turns)
object_ive(ei_gen$vectors[, 6], re_turns)

# Plot highest order principal component loadings
x11(width=6, height=5)
par(mar=c(2.5, 2, 2, 3), oma=c(0, 0, 0, 0), mgp=c(2, 0.5, 0))
barplot(weights_6, names.arg=names(weights_2), xlab="", ylab="",
  main="Highest Order Principal Component Loadings")

# Perform principal component analysis PCA
pc_a <- prcomp(re_turns, scale=TRUE)
# Plot standard deviations of principal components
barplot(pc_a$sdev, names.arg=colnames(pc_a$rotation),
  las=3, xlab="", ylab="",
  main="Scree Plot: Volatilities of Principal Components \n of Stock Returns")

# Calculate principal component loadings (weights)
pc_a$rotation
# Plot barplots with PCA weights in multiple panels
x11(width=6, height=7)
par(mfrow=c(n_weights/2, 2))
par(mar=c(3, 2, 2, 1), oma=c(0, 0, 0, 0))
for (or_der in 1:n_weights) {
  barplot(pc_a$rotation[, or_der], las=3, xlab="", ylab="", main="")
  title(paste0("PC", or_der), line=-1, col.main="red")
}  # end for

# Calculate products of principal component time series
round(t(pc_a$x) %*% pc_a$x, 2)
# Calculate principal component time series from re_turns
pca_rets <- xts(re_turns %*% pc_a$rotation, order.by=date_s)
round(cov(pca_rets), 3)
all.equal(coredata(pca_rets), pc_a$x, check.attributes=FALSE)
pca_ts <- cumsum(pca_rets)
# Plot principal component time series in multiple panels
ra_nge <- range(pca_ts)
for (or_der in 1:n_weights) {
  plot.zoo(pca_ts[, or_der], ylim=ra_nge, xlab="", ylab="")
  title(paste0("PC", or_der), line=-1, col.main="red")
}  # end for

# Invert all the principal component time series
pca_rets <- re_turns %*% pc_a$rotation
sol_ved <- pca_rets %*% solve(pc_a$rotation)
all.equal(coredata(re_turns), sol_ved)
# Invert first 3 principal component time series
sol_ved <- pca_rets[, 1:3] %*% solve(pc_a$rotation)[1:3, ]
sol_ved <- xts::xts(sol_ved, date_s)
sol_ved <- cumsum(sol_ved)
cum_returns <- cumsum(re_turns)
# Plot the solved returns
for (sym_bol in sym_bols) {
  plot.zoo(cbind(cum_returns[, sym_bol], sol_ved[, sym_bol]),
    plot.type="single", col=c("black", "blue"), xlab="", ylab="")
  legend(x="topleft", bty="n",
   legend=paste0(sym_bol, c("", " solved")),
   title=NULL, inset=0.0, cex=1.0, lwd=6,
   lty=1, col=c("black", "blue"))
}  # end for
