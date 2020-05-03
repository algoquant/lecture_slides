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
position_s <- na.locf(position_s)
position_s <- xts(position_s, order.by=index(oh_lc))
pos_lagged <- rutils::lag_it(position_s)
# Calculate pnl for days without trade
pnl_s <- rutils::diff_it(cl_ose)*position_s
# Calculate realized pnl for days with trade
close_lag <- rutils::lag_it(cl_ose)
pnl_s[trade_dates] <- pos_lagged[trade_dates] *
  (op_en[trade_dates] - close_lag[trade_dates])
# Calculate unrealized pnl for days with trade
pnl_s[trade_dates] <- pnl_s[trade_dates] +
  position_s[trade_dates] *
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
position_s <- na.locf(position_s)
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
pnl_s[trade_dates] <- pos_lagged[trade_dates] *
  (op_en[trade_dates] - close_lag[trade_dates])
# Calculate unrealized pnl for days with trade
pnl_s[trade_dates] <- pnl_s[trade_dates] +
  position_s[trade_dates] *
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
  position_s <- na.locf(position_s)
  op_en <- quantmod::Op(oh_lc)
  close_lag <- rutils::lag_it(cl_ose)
  pos_lagged <- rutils::lag_it(position_s)
  # Calculate daily profits and losses
  pnl_s <- rutils::diff_it(cl_ose)*position_s
  pnl_s[trade_dates] <- pos_lagged[trade_dates] *
    (op_en[trade_dates] - close_lag[trade_dates])
  pnl_s[trade_dates] <- pnl_s[trade_dates] +
    position_s[trade_dates] *
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

# initialize compute cluster under Windows
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
pnl_s <- rutils::do_call(cbind, pnl_s)
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
  # backtest EWMA strategy and calculate re_turns
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

# backtest best performing strategy
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
pnl_s <- rutils::do_call(cbind, pnl_s)
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

# R startup chunk
# ```{r setup, include=FALSE}
library(shiny)
library(quantmod)
inter_val <- 31
cl_ose <- quantmod::Cl(rutils::etf_env$VTI)
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue")
# ```
#end R startup chunk
inputPanel(
  sliderInput("lamb_da", label="lambda:",
    min=0.01, max=0.2, value=0.1, step=0.01)
)  # end inputPanel

renderPlot({
  # Calculate EWMA prices
  lamb_da <- input$lamb_da
  weight_s <- exp(-lamb_da*1:inter_val)
  weight_s <- weight_s/sum(weight_s)
  ew_ma <- filter(cl_ose, filter=weight_s, sides=1)
  ew_ma[1:(inter_val-1)] <- ew_ma[inter_val]
  ew_ma <- xts(cbind(cl_ose, ew_ma), order.by=index(cl_ose))
  colnames(ew_ma) <- c("VTI", "VTI EWMA")
  # Plot EWMA prices
  ch_ob <- chart_Series(ew_ma, theme=plot_theme, name="EWMA prices")
  plot(ch_ob)
  legend("top", legend=colnames(ew_ma),
   inset=0.1, bg="white", lty=1, lwd=2,
   col=plot_theme$col$line.col, bty="n")
})  # end renderPlot

# VTI percentage returns
re_turns <- rutils::diff_it(log(quantmod::Cl(rutils::etf_env$VTI)))
# Define end points
end_points <- seq_along(re_turns)
n_rows <- NROW(end_points)
look_back <- 51
# start_points are multi-period lag of end_points
start_points <- c(rep_len(1, look_back-1),
    end_points[1:(n_rows-look_back+1)])
# Define list of look-back intervals for aggregations over past
look_backs <- lapply(seq_along(end_points),
  function(in_dex) {
    start_points[in_dex]:end_points[in_dex]
})  # end lapply
# Calculate realized VTI variance in sapply() loop
vari_ance <- sapply(look_backs,
  function(look_back) {
    ret_s <- re_turns[look_back]
    sum((ret_s - mean(ret_s))^2)
}) / (look_back-1)  # end sapply
tail(vari_ance)
class(vari_ance)
# Coerce vari_ance into xts
vari_ance <- xts(vari_ance, order.by=index(re_turns))
colnames(vari_ance) <- "VTI.variance"
head(vari_ance)

# Calculate rolling VTI variance using package roll
library(roll)  # load roll
vari_ance <-
  roll::roll_var(re_turns, width=look_back)
colnames(vari_ance) <- "VTI.variance"
head(vari_ance)
sum(is.na(vari_ance))
vari_ance[1:(look_back-1)] <- 0
# Benchmark calculation of rolling variance
library(microbenchmark)
summary(microbenchmark(
  roll_sapply=sapply(look_backs, function(look_back) {
    ret_s <- re_turns[look_back]
    sum((ret_s - mean(ret_s))^2)
  }),
  ro_ll=roll::roll_var(re_turns, width=look_back),
  times=10))[, c(1, 4, 5)]

# Calculate EWMA VTI variance using filter()
look_back <- 51
weight_s <- exp(-0.1*1:look_back)
weight_s <- weight_s/sum(weight_s)
vari_ance <- stats::filter(re_turns^2,
    filter=weight_s, sides=1)
vari_ance[1:(look_back-1)] <- vari_ance[look_back]
class(vari_ance)
vari_ance <- as.numeric(vari_ance)
x_ts <- xts:::xts(sqrt(vari_ance), order.by=index(re_turns))
# Plot EWMA standard deviation
chart_Series(x_ts,
  name="EWMA standard deviation")
dygraphs::dygraph(x_ts, main="EWMA standard deviation")

# Calculate rolling VTI variance using package roll
library(roll)  # load roll
vari_ance <- roll::roll_var(re_turns,
  weights=rev(weight_s), width=look_back)
colnames(vari_ance) <- "VTI.variance"
class(vari_ance)
head(vari_ance)
sum(is.na(vari_ance))
vari_ance[1:(look_back-1)] <- 0

x11(width=6, height=4)
par(mar=c(4, 3, 1, 1), oma=c(0, 0, 0, 0))
# VTI percentage returns
re_turns <- rutils::diff_it(log(quantmod::Cl(rutils::etf_env$VTI)))
# Calculate rolling VTI variance using package roll
look_back <- 22
vari_ance <-
  roll::roll_var(re_turns, width=look_back)
vari_ance[1:(look_back-1)] <- 0
colnames(vari_ance) <- "VTI.variance"
# Number of look_backs that fit over re_turns
n_rows <- NROW(re_turns)
n_agg <- n_rows %/% look_back
end_points <- # Define end_points with beginning stub
  n_rows-look_back*n_agg + (0:n_agg)*look_back
n_rows <- NROW(end_points)
# subset vari_ance to end_points
vari_ance <- vari_ance[end_points]
# improved autocorrelation function
acf_plus(coredata(vari_ance), lag=10, main="")
title(main="acf of variance", line=-1)
# Partial autocorrelation
pacf(coredata(vari_ance), lag=10, main="", ylab=NA)
title(main="pacf of variance", line=-1)

# Define GARCH parameters
om_ega <- 0.01 ; al_pha <- 0.2
be_ta <- 0.79 ; n_rows <- 1000
re_turns <- numeric(n_rows)
vari_ance <- numeric(n_rows)
vari_ance[1] <- om_ega/(1-al_pha-be_ta)
re_turns[1] <- rnorm(1, sd=sqrt(vari_ance[1]))
# simulate GARCH model
set.seed(1121)  # reset random numbers
for (i in 2:n_rows) {
  re_turns[i] <- rnorm(n=1, sd=sqrt(vari_ance[i-1]))
  vari_ance[i] <- om_ega + al_pha*re_turns[i]^2 +
    be_ta*vari_ance[i-1]
}  # end for

x11(width=5, height=3.5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0))
# Plot GARCH cumulative returns
plot(cumsum(re_turns/100), t="l",
  lwd=2, col="blue", xlab="", ylab="",
  main="GARCH cumulative returns")
# Plot dygraphs GARCH standard deviation
date_s <- seq.Date(from=Sys.Date()-n_rows+1,
  to=Sys.Date(), length.out=n_rows)
x_ts <- xts:::xts(cumsum(re_turns/100), order.by=date_s)
dygraphs::dygraph(x_ts, main="GARCH cumulative returns")
# Plot GARCH standard deviation
plot(sqrt(vari_ance), t="l",
  col="blue", xlab="", ylab="",
  main="GARCH standard deviation")
# Plot dygraphsGARCH standard deviation
x_ts <- xts:::xts(sqrt(vari_ance), order.by=date_s)
dygraphs::dygraph(x_ts, main="GARCH standard deviation")

# Define GARCH parameters
om_ega <- 0.0001 ; al_pha <- 0.5
be_ta <- 0.1 ; n_rows <- 10000
re_turns <- numeric(n_rows)
vari_ance <- numeric(n_rows)
vari_ance[1] <- om_ega/(1-al_pha-be_ta)
re_turns[1] <- rnorm(1, sd=sqrt(vari_ance[1]))
# simulate GARCH model
set.seed(1121)  # reset random numbers
for (i in 2:n_rows) {
  re_turns[i] <- rnorm(n=1, sd=sqrt(vari_ance[i-1]))
  vari_ance[i] <- om_ega + al_pha*re_turns[i]^2 +
    be_ta*vari_ance[i-1]
}  # end for
# Calculate kurtosis of GARCH returns
moments::moment(re_turns, order=4) /
  moments::moment(re_turns, order=2)^2
# Perform Jarque-Bera test of normality
tseries::jarque.bera.test(re_turns)

# Plot histogram of GARCH returns
histo_gram <- hist(re_turns, col="lightgrey",
  xlab="returns", breaks=200, xlim=c(-0.05, 0.05),
  ylab="frequency", freq=FALSE,
  main="GARCH returns histogram")
lines(density(re_turns, adjust=1.5),
lwd=3, col="blue")
optim_fit <- MASS::fitdistr(re_turns,
  densfun="t", df=2, lower=c(-1, 1e-7))
lo_cation <- optim_fit$estimate[1]
scal_e <- optim_fit$estimate[2]
curve(expr=dt((x-lo_cation)/scal_e, df=2)/scal_e,
  type="l", xlab="", ylab="", lwd=3,
  col="red", add=TRUE)
legend("topright", inset=0.05, bty="n",
 leg=c("density", "t-distr w/ 2 dof"),
 lwd=6, lty=1, col=c("blue", "red"))

# use fixed notation instead of exponential notation
options(scipen=999)
library(fGarch)
# fit returns into GARCH
garch_fit <- fGarch::garchFit(data=re_turns)
# fitted GARCH parameters
round(garch_fit@fit$coef, 5)
# Actual GARCH parameters
round(c(mu=mean(re_turns), omega=om_ega,
  alpha=al_pha, beta=be_ta), 5)

# Plot GARCH fitted standard deviation
plot(sqrt(garch_fit@fit$series$h), t="l",
  col="blue", xlab="", ylab="",
  main="GARCH fitted standard deviation")

# specify GARCH model
garch_spec <- fGarch::garchSpec(
  model=list(omega=om_ega, alpha=al_pha, beta=be_ta))
# simulate GARCH model
garch_sim <-
  fGarch::garchSim(spec=garch_spec, n=n_rows)
re_turns <- as.numeric(garch_sim)
# Calculate kurtosis of GARCH returns
moments::moment(re_turns, order=4) /
  moments::moment(re_turns, order=2)^2
# Perform Jarque-Bera test of normality
tseries::jarque.bera.test(re_turns)
# Plot histogram of GARCH returns
histo_gram <- hist(re_turns, col="lightgrey",
  xlab="returns", breaks=200, xlim=c(-0.05, 0.05),
  ylab="frequency", freq=FALSE,
  main="GARCH returns histogram")
lines(density(re_turns, adjust=1.5),
lwd=3, col="blue")

# fit t-distribution into GARCH returns
optim_fit <- MASS::fitdistr(re_turns,
  densfun="t", df=2, lower=c(-1, 1e-7))
lo_cation <- optim_fit$estimate[1]
scal_e <- optim_fit$estimate[2]
curve(expr=dt((x-lo_cation)/scal_e, df=2)/scal_e,
  type="l", xlab="", ylab="", lwd=3,
  col="red", add=TRUE)
legend("topright", inset=0.05, bty="n",
 leg=c("density", "t-distr w/ 2 dof"),
 lwd=6, lty=1, col=c("blue", "red"))

library(HighFreq)  # load HighFreq
# Minutely SPY returns (unit per minute) single day
re_turns <- rutils::diff_it(log(SPY["2012-02-13", 4]))
# Minutely SPY volatility (unit per minute)
sd(re_turns)
# Minutely SPY returns (unit per second)
re_turns <- rutils::diff_it(log(SPY["2012-02-13", 4])) /
  rutils::diff_it(.index(SPY["2012-02-13"]))
# Minutely SPY volatility scaled to unit per minute
60*sd(re_turns)
# Minutely SPY returns multiple days no overnight scaling
re_turns <- rutils::diff_it(log(SPY[, 4]))
# Minutely SPY volatility (unit per minute)
sd(re_turns)
# Minutely SPY returns (unit per second)
re_turns <- rutils::diff_it(log(SPY[, 4])) /
  rutils::diff_it(.index(SPY))
# Minutely SPY volatility scaled to unit per minute
60*sd(re_turns)
# Table of time intervals - 60 second is most frequent
interval_s <- rutils::diff_it(.index(SPY))
table(interval_s)
# hist(interval_s)

library(HighFreq)  # load HighFreq
# Daily OHLC SPY prices
SPY_daily <-
  rutils::to_period(oh_lc=SPY, period="days")
# Daily SPY returns and volatility
sd(rutils::diff_it(log(SPY_daily[, 4])))
# Minutely SPY returns (unit per minute)
re_turns <- rutils::diff_it(log(SPY[, 4]))
# Minutely SPY volatility scaled to daily interval
sqrt(6.5*60)*sd(re_turns)
# Minutely SPY returns (unit per second)
re_turns <- rutils::diff_it(log(SPY[, 4])) /
  rutils::diff_it(.index(SPY))
# Minutely SPY volatility scaled to daily aggregation interval
60*sqrt(6.5*60)*sd(re_turns)
# Daily SPY volatility
# including extra time over weekends and holidays
24*60*60*sd(rutils::diff_it(log(SPY_daily[, 4])) /
    rutils::diff_it(.index(SPY_daily)))
table(rutils::diff_it(.index(SPY_daily)))

# Scaled minutely SPY returns
re_turns <- rutils::diff_it(as.numeric(Cl(SPY)))[-1] /
  rutils::diff_it(.index(SPY))[-1]
range(re_turns)
hist(re_turns, breaks=100,
     xlim=c(-0.005, 0.005))
# SPY prices
price_s <- cumsum(re_turns)
plot(price_s, t="l")
# Perform rescaled range analysis
look_back <- 100
end_points <- rutils::calc_endpoints(re_turns,
  inter_val=look_back)
r_s <- sapply(seq_along(end_points)[-1],
      function(it) {
  indeks <- end_points[it-1]:end_points[it]
  price_s <- price_s[indeks]
  (max(price_s) - min(price_s)) /
    sd(re_turns[indeks])
})  # end sapply
mean(r_s)

# Perform rescaled range analysis over many look-backs
look_backs <- seq.int(1e2, 1e3, 1e2)
r_s <- sapply(look_backs, function(look_back) {
  end_points <- rutils::calc_endpoints(re_turns, inter_val=look_back)
  r_s <- sapply(seq_along(end_points)[-1], function(it) {
    indeks <- end_points[it-1]:end_points[it]
    (max(price_s[indeks]) - min(price_s[indeks]))/sd(re_turns[indeks])
  })  # end sapply
  mean(r_s)
})  # end sapply
names(r_s) <- paste0("agg_", look_backs)
rs_log <- log(r_s)
look_log <- log(look_backs)
mod_el <- lm(rs_log ~ look_log)
hurst_lm <- summary(mod_el)$coeff[2, 1]
look_log <- look_log - mean(look_log)
rs_log <- rs_log - mean(rs_log)
hurst_mat <- sum(rs_log*look_log)/sum(look_log^2)
all.equal(hurst_lm, hurst_mat)

x11(width=6, height=5)
par(mar=c(4, 4, 2, 1), oma=c(1, 1, 1, 1))
plot(rs_log ~ look_log, lwd=6, col="red",
     xlab="aggregation intervals (log)",
     ylab="rescaled range (log)",
     main="Rescaled Range Analysis for SPY")
abline(mod_el, lwd=3, col="blue")
text(-1.2, 0.2, paste0("Hurst = ",
  round(hurs_t, 4)))

library(HighFreq)  # load HighFreq
# Daily SPY volatility from minutely prices using package TTR
library(TTR)
sqrt((6.5*60)*mean(na.omit(
  TTR::volatility(SPY, N=1,
          calc="yang.zhang"))^2))
# SPY volatility using package HighFreq
60*sqrt((6.5*60)*agg_regate(oh_lc=SPY,
    weight_ed=FALSE, mo_ment="run_variance",
    calc_method="yang_zhang"))
