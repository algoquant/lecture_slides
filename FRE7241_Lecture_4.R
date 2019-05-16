library(rutils)  # load package rutils
# Calculate ETF returns
re_turns <-
  rutils::etf_env$re_turns[, c("IEF", "VTI")]
re_turns <- na.omit(re_turns)
re_turns <- cbind(re_turns,
  0.6*re_turns[, "IEF"]+0.4*re_turns[, "VTI"])
colnames(re_turns)[3] <- "combined"
# Calculate correlations
cor(re_turns)
# Calculate Sharpe ratios
sqrt(252)*sapply(re_turns, function(x) mean(x)/sd(x))
# Calculate prices from returns
price_s <- lapply(re_turns,
  function(x) exp(cumsum(x)))
price_s <- rutils::do_call(cbind, price_s)
# Plot prices
dygraphs::dygraph(price_s, main="Stock and Bond Portfolio") %>%
  dyOptions(colors=c("green","blue","green")) %>%
  dySeries("combined", color="red", strokeWidth=2) %>%
  dyLegend(show="always")
# calculate open, close, and lagged prices
oh_lc <- rutils::etf_env$VTI
op_en <- quantmod::Op(oh_lc)
cl_ose <- quantmod::Cl(oh_lc)
star_t <- as.numeric(cl_ose[1, ])
prices_lag <- rutils::lag_it(cl_ose)
# define aggregation interval and calculate VWAP
look_back <- 150
VTI_vwap <- HighFreq::roll_vwap(oh_lc,
        look_back=look_back)
# calculate VWAP indicator
in_dic <- sign(cl_ose - VTI_vwap)
# determine dates right after VWAP has crossed prices
trade_dates <- (rutils::diff_it(in_dic) != 0)
trade_dates <- which(trade_dates) + 1
# plot prices and VWAP
chart_Series(x=cl_ose,
  name="VTI prices", col="orange")
add_TA(VTI_vwap, on=1, lwd=2, col="blue")
legend("top", legend=c("VTI", "VWAP"),
  bg="white", lty=1, lwd=6,
  col=c("orange", "blue"), bty="n")
# Calculate positions, either: -1, 0, or 1
position_s <- rep(NA_integer_, NROW(oh_lc))
position_s[1] <- 0
position_s[trade_dates] <- in_dic[trade_dates]
position_s <- na.locf(position_s)
position_s <- xts(position_s, order.by=index(oh_lc))
pos_lagged <- rutils::lag_it(position_s)
# Calculate daily profits and losses
pnl_s <- pos_lagged*(cl_ose - prices_lag)
pnl_s[trade_dates] <- pos_lagged[trade_dates] *
  (op_en[trade_dates] - prices_lag[trade_dates]) +
  position_s[trade_dates] *
  (cl_ose[trade_dates] - op_en[trade_dates])
# Calculate percentage returns
pnl_s <- pnl_s/cl_ose
# Calculate annualized Sharpe ratio of strategy returns
sqrt(252)*sum(pnl_s)/sd(pnl_s)/NROW(pnl_s)
# plot prices and VWAP
pnl_s <- xts(as.numeric(cl_ose[1])*exp(cumsum(pnl_s)), order.by=index(oh_lc))
chart_Series(x=cl_ose, name="VTI prices", col="orange")
add_TA(pnl_s, on=1, lwd=2, col="blue")
add_TA(position_s > 0, on=-1,
 col="lightgreen", border="lightgreen")
add_TA(position_s < 0, on=-1,
 col="lightgrey", border="lightgrey")
legend("top", legend=c("VTI", "VWAP strategy"),
 inset=0.1, bg="white", lty=1, lwd=6,
 col=c("orange", "blue"), bty="n")
# Define length for weights and decay parameter
wid_th <- 352
lamb_da <- 0.01
# Calculate EWMA prices
weight_s <- exp(-lamb_da*1:wid_th)
weight_s <- weight_s/sum(weight_s)
ew_ma <- stats::filter(cl_ose, filter=weight_s, sides=1)
ew_ma[1:(wid_th-1)] <- ew_ma[wid_th]
ew_ma <- xts(cbind(cl_ose, ew_ma), order.by=index(oh_lc))
colnames(ew_ma) <- c("VTI", "VTI EWMA")
# plot EWMA prices with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue")
chart_Series(ew_ma["2007/2010"], theme=plot_theme,
       name="EWMA prices")
legend("bottomleft", legend=colnames(ew_ma),
 inset=0.1, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")
# determine dates right after EWMA has crossed prices
in_dic <- sign(cl_ose - ew_ma[, 2])
trade_dates <- (rutils::diff_it(in_dic) != 0)
trade_dates <- which(trade_dates) + 1
# calculate positions, either: -1, 0, or 1
position_s <- rep(NA_integer_, NROW(cl_ose))
position_s[1] <- 0
position_s[trade_dates] <-
  rutils::lag_it(in_dic)[trade_dates]
position_s <- na.locf(position_s)
position_s <- xts(position_s, order.by=index(oh_lc))
# plot EWMA prices with position shading
chart_Series(ew_ma["2007/2010"], theme=plot_theme,
       name="EWMA prices")
add_TA(position_s > 0, on=-1,
 col="lightgreen", border="lightgreen")
add_TA(position_s < 0, on=-1,
 col="lightgrey", border="lightgrey")
legend("bottomleft", legend=colnames(ew_ma),
 inset=0.1, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")
# bid_offer is equal to 10 bps for liquid ETFs
bid_offer <- 0.001
# calculate open and lagged prices
op_en <- Op(oh_lc)
prices_lag <- rutils::lag_it(cl_ose)
pos_lagged <- rutils::lag_it(position_s)
# calculate the transaction cost for one share
cost_s <- 0.0*position_s
cost_s[trade_dates] <-
  0.5*bid_offer*abs(pos_lagged[trade_dates] -
  position_s[trade_dates])*op_en[trade_dates]
# Calculate daily profits and losses
re_turns <- pos_lagged*(cl_ose - prices_lag)
re_turns[trade_dates] <-
  pos_lagged[trade_dates] *
  (op_en[trade_dates] - prices_lag[trade_dates]) +
  position_s[trade_dates] *
  (cl_ose[trade_dates] - op_en[trade_dates]) -
  cost_s
# Calculate percentage returns
re_turns <- re_turns/cl_ose
# Calculate annualized Sharpe ratio of strategy returns
sqrt(252)*sum(re_turns)/sd(re_turns)/NROW(re_turns)
pnl_s <- as.numeric(cl_ose[1])*exp(cumsum(re_turns))
pnl_s <- cbind(cl_ose, pnl_s)
colnames(pnl_s) <- c("VTI", "EWMA PnL")
# plot EWMA PnL with position shading
chart_Series(pnl_s, theme=plot_theme,
       name="Performance of EWMA Strategy")
add_TA(position_s > 0, on=-1,
 col="lightgreen", border="lightgreen")
add_TA(position_s < 0, on=-1,
 col="lightgrey", border="lightgrey")
legend("top", legend=colnames(pnl_s),
 inset=0.05, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")
simu_ewma <- function(oh_lc, lamb_da=0.01, wid_th=251, bid_offer=0.001, tre_nd=1) {
  # calculate EWMA prices
  weight_s <- exp(-lamb_da*1:wid_th)
  weight_s <- weight_s/sum(weight_s)
  cl_ose <- quantmod::Cl(oh_lc)
  ew_ma <- stats::filter(as.numeric(cl_ose), filter=weight_s, sides=1)
  ew_ma[1:(wid_th-1)] <- ew_ma[wid_th]
  # determine dates right after EWMA has crossed prices
  in_dic <- tre_nd*xts::xts(sign(as.numeric(cl_ose) - ew_ma), order.by=index(oh_lc))
  trade_dates <- (rutils::diff_it(in_dic) != 0)
  trade_dates <- which(trade_dates) + 1
  trade_dates <- trade_dates[trade_dates<NROW(oh_lc)]
  # calculate positions, either: -1, 0, or 1
  position_s <- rep(NA_integer_, NROW(cl_ose))
  position_s[1] <- 0
  position_s[trade_dates] <- rutils::lag_it(in_dic)[trade_dates]
  position_s <- xts::xts(na.locf(position_s), order.by=index(oh_lc))
  op_en <- quantmod::Op(oh_lc)
  prices_lag <- rutils::lag_it(cl_ose)
  pos_lagged <- rutils::lag_it(position_s)
  # calculate transaction costs
  cost_s <- 0.0*position_s
  cost_s[trade_dates] <- 0.5*bid_offer*abs(pos_lagged[trade_dates] - position_s[trade_dates])*op_en[trade_dates]
  # calculate daily profits and losses
  re_turns <- pos_lagged*(cl_ose - prices_lag)
  re_turns[trade_dates] <- pos_lagged[trade_dates] * (op_en[trade_dates] - prices_lag[trade_dates]) + position_s[trade_dates] * (cl_ose[trade_dates] - op_en[trade_dates]) - cost_s
  # Calculate percentage returns
  re_turns <- re_turns/cl_ose
  out_put <- cbind(position_s, re_turns)
  colnames(out_put) <- c("positions", "returns")
  out_put
}  # end simu_ewma
source("C:/Develop/R/lecture_slides/scripts/ewma_model.R")
lamb_das <- seq(0.0001, 0.05, 0.005)
# perform lapply() loop over lamb_das
pnl_s <- lapply(lamb_das, function(lamb_da) {
  # simulate EWMA strategy and calculate re_turns
  star_t*exp(cumsum(simu_ewma(oh_lc=oh_lc,
    lamb_da=lamb_da, wid_th=wid_th)[, "returns"]))
})  # end lapply
pnl_s <- rutils::do_call(cbind, pnl_s)
colnames(pnl_s) <- paste0("lambda=", lamb_das)
# plot EWMA strategies with custom line colors
column_s <- seq(1, NCOL(pnl_s), by=3)
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NROW(column_s))
chart_Series(pnl_s[, column_s],
  theme=plot_theme, name="Cumulative Returns of EWMA Strategies")
legend("topleft", legend=colnames(pnl_s[, column_s]),
  inset=0.1, bg="white", cex=0.8, lwd=rep(6, NCOL(pnl_s)),
  col=plot_theme$col$line.col, bty="n")
# initialize compute cluster under Windows
library(parallel)
clus_ter <- makeCluster(detectCores()-1)
clusterExport(clus_ter,
  varlist=c("oh_lc", "wid_th", "simu_ewma"))
# perform parallel loop over lamb_das under Windows
pnl_s <- parLapply(clus_ter, lamb_das, function(lamb_da) {
  library(quantmod)
  # simulate EWMA strategy and calculate re_turns
  star_t*exp(cumsum(simu_ewma(
    oh_lc=oh_lc, lamb_da=lamb_da, wid_th=wid_th)[, "returns"]))
})  # end parLapply
# perform parallel loop over lamb_das under Mac-OSX or Linux
re_turns <- mclapply(lamb_das, function(lamb_da) {
  library(quantmod)
  # simulate EWMA strategy and calculate re_turns
  star_t*exp(cumsum(simu_ewma(
    oh_lc=oh_lc, lamb_da=lamb_da, wid_th=wid_th)[, "returns"]))
})  # end mclapply
stopCluster(clus_ter)  # stop R processes over cluster under Windows
pnl_s <- rutils::do_call(cbind, pnl_s)
colnames(pnl_s) <- paste0("lambda=", lamb_das)
sharpe_ratios <- sqrt(252)*sapply(pnl_s, function(x_ts) {
  # calculate annualized Sharpe ratio of strategy returns
  x_ts <- rutils::diff_it(log(x_ts))
  sum(x_ts)/sd(x_ts)
})/NROW(pnl_s)  # end sapply
plot(x=lamb_das, y=sharpe_ratios, t="l",
     main="Performance of EWMA trend-following strategies
     as function of the decay parameter lambda")
trend_returns <- rutils::diff_it(log(pnl_s))
trend_sharpe <- sharpe_ratios
# Simulate best performing strategy
ewma_trend <- simu_ewma(oh_lc=oh_lc,
  lamb_da=lamb_das[which.max(sharpe_ratios)],
  wid_th=wid_th)
position_s <- ewma_trend[, "positions"]
pnl_s <- star_t*exp(cumsum(ewma_trend[, "returns"]))
pnl_s <- cbind(cl_ose, pnl_s)
colnames(pnl_s) <- c("VTI", "EWMA PnL")
# plot EWMA PnL with position shading
plot_theme$col$line.col <- c("orange", "blue")
chart_Series(pnl_s, theme=plot_theme,
       name="Performance of Trend-following EWMA Strategy")
add_TA(position_s > 0, on=-1,
 col="lightgreen", border="lightgreen")
add_TA(position_s < 0, on=-1,
 col="lightgrey", border="lightgrey")
legend("top", legend=colnames(pnl_s),
  inset=0.05, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")
source("C:/Develop/R/lecture_slides/scripts/ewma_model.R")
lamb_das <- seq(0.05, 1.0, 0.05)
# perform lapply() loop over lamb_das
pnl_s <- lapply(lamb_das, function(lamb_da) {
  # backtest EWMA strategy and calculate re_turns
  star_t*exp(cumsum(simu_ewma(
    oh_lc=oh_lc, lamb_da=lamb_da, wid_th=wid_th, tre_nd=(-1))[, "returns"]))
})  # end lapply
pnl_s <- rutils::do_call(cbind, pnl_s)
colnames(pnl_s) <- paste0("lambda=", lamb_das)
# plot EWMA strategies with custom line colors
column_s <- seq(1, NCOL(pnl_s), by=4)
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NROW(column_s))
chart_Series(pnl_s[, column_s],
  theme=plot_theme, name="Cumulative Returns of Mean-reverting EWMA Strategies")
legend("topleft", legend=colnames(pnl_s[, column_s]),
  inset=0.1, bg="white", cex=0.8, lwd=rep(6, NCOL(pnl_s)),
  col=plot_theme$col$line.col, bty="n")
sharpe_ratios <- sqrt(252)*sapply(pnl_s, function(x_ts) {
  # calculate annualized Sharpe ratio of strategy returns
  x_ts <- rutils::diff_it(log(x_ts))
  sum(x_ts)/sd(x_ts)
})/NROW(pnl_s)  # end sapply
plot(x=lamb_das, y=sharpe_ratios, t="l",
     main="Performance of EWMA mean-reverting strategies
     as function of the decay parameter lambda")
revert_returns <- rutils::diff_it(log(pnl_s))
revert_sharpe <- sharpe_ratios
# backtest best performing strategy
ewma_revert <- simu_ewma(oh_lc=oh_lc,
  lamb_da=lamb_das[which.max(sharpe_ratios)],
  wid_th=wid_th, tre_nd=(-1))
position_s <- ewma_revert[, "positions"]
pnl_s <- star_t*exp(cumsum(ewma_revert[, "returns"]))
pnl_s <- cbind(cl_ose, pnl_s)
colnames(pnl_s) <- c("VTI", "EWMA PnL")
# plot EWMA PnL with position shading
plot_theme$col$line.col <- c("orange", "blue")
chart_Series(pnl_s, theme=plot_theme,
       name="Performance of Mean-reverting EWMA Strategy")
add_TA(position_s > 0, on=-1,
 col="lightgreen", border="lightgreen")
add_TA(position_s < 0, on=-1,
 col="lightgrey", border="lightgrey")
legend("top", legend=colnames(pnl_s),
  inset=0.05, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")
# calculate correlation between trend-following and mean-reverting strategies
trend_ing <- ewma_trend[, "returns"]
colnames(trend_ing) <- "trend"
revert_ing <- ewma_revert[, "returns"]
colnames(revert_ing) <- "revert"
close_rets <- rutils::diff_it(log(cl_ose))
cor(cbind(trend_ing, revert_ing, close_rets))
# Calculate combined strategy
com_bined <- trend_ing + revert_ing
colnames(com_bined) <- "combined"
# calculate annualized Sharpe ratio of strategy returns
re_turns <- cbind(close_rets, trend_ing, revert_ing, com_bined)
sqrt(252)*sapply(re_turns, function(x_ts)
  sum(x_ts)/sd(x_ts))/NROW(com_bined)
pnl_s <- lapply(re_turns, function(x_ts) star_t*exp(cumsum(x_ts)))
pnl_s <- rutils::do_call(cbind, pnl_s)
colnames(pnl_s) <- c("VTI", "trending", "reverting", "EWMA combined PnL")
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue", "green", "magenta2")
chart_Series(pnl_s, theme=plot_theme,
       name="Performance of Combined EWMA Strategies")
legend("topleft", legend=colnames(pnl_s),
 inset=0.05, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")
sharpe_ratios <- c(trend_sharpe, revert_sharpe)
weight_s <- sharpe_ratios
weight_s[weight_s<0] <- 0
weight_s <- weight_s/sum(weight_s)
re_turns <- cbind(trend_returns, revert_returns)
avg_returns <- re_turns %*% weight_s
avg_returns <- xts(avg_returns, order.by=index(re_turns))
pnl_s <- star_t*exp(cumsum(avg_returns))
pnl_s <- cbind(cl_ose, pnl_s)
colnames(pnl_s) <- c("VTI", "EWMA PnL")
# plot EWMA PnL without position shading
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue")
chart_Series(pnl_s, theme=plot_theme,
  name="Performance of Ensemble EWMA Strategy")
legend("top", legend=colnames(pnl_s),
  inset=0.05, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")
# Define end of month end_points
end_points <- rutils::calc_endpoints(re_turns,
          inter_val="months")
n_rows <- NROW(end_points)
# Start_points equal end_points lagged by 12-month look-back interval
look_back <- 12
start_points <- c(rep_len(1, look_back-1),
  end_points[1:(n_rows-look_back+1)])
# Calculate past performance over end_points
perform_ance <-
  function(re_turns) sum(re_turns)/sd(re_turns)
past_perf <- sapply(1:(n_rows-1), function(it_er) {
  sapply(re_turns[start_points[it_er]:end_points[it_er]], perform_ance)
})  # end sapply
past_perf <- t(past_perf)
fut_rets <- sapply(1:(n_rows-1), function(it_er) {
  sapply(re_turns[(end_points[it_er]+1):end_points[it_er+1]], sum)
})  # end sapply
fut_rets <- t(fut_rets)
# calculate weight_s proportional to past_perf
weight_s <- past_perf
weight_s[weight_s<0] <- 0
# scale weight_s so their sum is equal to 1
weight_s <- weight_s/rowSums(weight_s)
# set NA values to zero
weight_s[is.na(weight_s)] <- 0
sum(is.na(weight_s))
in_dex <- index(re_turns[end_points[-n_rows]])
trend_weights <- rowMeans(weight_s[, 1:NCOL(trend_returns)])
revert_weights <- rowMeans(weight_s[, -(1:NCOL(trend_returns))])
diff_weights <- xts(trend_weights-revert_weights, order.by=in_dex)
# Find best and worst EWMA Strategies in each period
bes_t <- apply(weight_s, 1, which.max)
wors_t <- apply(weight_s, 1, which.min)
# plot the mean weights of EWMA Strategies
zoo::plot.zoo(cbind(diff_weights,
  cl_ose[end_points[-n_rows]]),
  oma = c(3, 0, 3, 0), mar = c(0, 4, 0, 1),
  xlab=NULL, ylab=c("diff weights", "VTI"),
  main="Trend minus Revert Weights of EWMA strategies")
best_worst <- xts(cbind(bes_t, wors_t), order.by=in_dex)
zoo::plot.zoo(best_worst,
  oma = c(3, 0, 3, 0), mar = c(0, 4, 0, 1),
  xlab=NULL, ylab=c("best EWMA", "worst EWMA"),
  main="Best and Worst EWMA strategies")
# Calculate backtest returns
pnl_s <- rowSums(weight_s*fut_rets)
pnl_s <- xts(pnl_s, order.by=in_dex)
colnames(pnl_s) <- "ewma momentum"
close_rets <- rutils::diff_it(cl_ose[in_dex])
cor(cbind(pnl_s, close_rets))
pnl_s <- star_t*exp(cumsum(pnl_s))
# plot the backtest
chart_Series(x=cl_ose[end_points[-n_rows]],
  name="backtest of EWMA strategies", col="orange")
add_TA(pnl_s, on=1, lwd=2, col="blue")
legend("top", legend=c("VTI", "EWMA"),
 inset=0.1, bg="white", lty=1, lwd=6,
 col=c("orange", "blue"), bty="n")
# shad_e <- xts(index(pnl_s) < as.Date("2008-01-31"), order.by=index(pnl_s))
# add_TA(shad_e, on=-1, col="lightgrey", border="lightgrey")
# text(x=7, y=0, labels="warmup period")
# Calculate ETF prices and simple returns
sym_bols <- c("VTI", "IEF", "DBC")
price_s <- rutils::etf_env$price_s[, sym_bols]
price_s <- na.omit(zoo::na.locf(price_s))
re_turns <- rutils::diff_it(price_s)
# Define look-back and look-forward intervals
end_points <- rutils::calc_endpoints(re_turns,
  inter_val="months")
n_cols <- NCOL(re_turns)
n_rows <- NROW(end_points)
look_back <- 12
start_points <- c(rep_len(1, look_back-1),
  end_points[1:(n_rows-look_back+1)])
# Calculate past performance over end_points
perform_ance <-
  function(re_turns) sum(re_turns)/sd(re_turns)
agg_s <- sapply(1:(n_rows-1), function(it_er) {
  c(past_perf=sapply(re_turns[start_points[it_er]:end_points[it_er]], perform_ance),
    fut_rets=sapply(re_turns[(end_points[it_er]+1):end_points[it_er+1]], sum))
})  # end sapply
agg_s <- t(agg_s)
# Select look-back and look-forward aggregations
past_perf <- agg_s[, 1:n_cols]
fut_rets <- agg_s[, n_cols+1:n_cols]
# Calculate portfolio weights equal to number of shares
end_prices <- price_s[end_points[-n_rows]]
weight_s <-
  past_perf/rowSums(abs(past_perf))/end_prices
weight_s[is.na(weight_s)] <- 0
colnames(weight_s) <- colnames(re_turns)
# Calculate profits and losses
pnl_s <- rowSums(weight_s*fut_rets)
pnl_s <- xts(pnl_s, index(end_prices))
colnames(pnl_s) <- "pnls"
# Calculate transaction costs
bid_offer <- 0.001
cost_s <-
  0.5*bid_offer*end_prices*abs(rutils::diff_it(weight_s))
cost_s <- rowSums(cost_s)
pnl_s <- (pnl_s - cost_s)
pnl_s <- cumsum(pnl_s)
# plot momentum strategy with VTI
cl_ose <- price_s[index(end_prices), "VTI"]
zoo::plot.zoo(cbind(cl_ose, pnl_s, weight_s),
  oma = c(3, 1, 3, 0), mar = c(0, 4, 0, 1), nc=1,
  xlab=NULL, main="ETF Momentum Strategy")
# define backtest functional
backtest_ep <- function(re_turns, price_s, perform_ance=sum,
    look_back=12, re_balance="months", bid_offer=0.001,
    end_points=rutils::calc_endpoints(re_turns, inter_val=re_balance),
    with_weights=FALSE, ...) {
  stopifnot("package:quantmod" %in% search() || require("quantmod", quietly=TRUE))
  # Define look-back and look-forward intervals
  n_cols <- NCOL(re_turns)
  n_rows <- NROW(end_points)
  start_points <- c(rep_len(1, look_back-1), end_points[1:(n_rows-look_back+1)])
  # Calculate past performance over end_points
  agg_s <- sapply(1:(n_rows-1), function(it_er) {
    c(past_perf=sapply(re_turns[start_points[it_er]:end_points[it_er]], perform_ance, ...),  # end sapply
    fut_rets=sapply(re_turns[(end_points[it_er]+1):end_points[it_er+1]], sum))  # end sapply
  })  # end sapply
  agg_s <- t(agg_s)
  # Select look-back and look-forward aggregations
  past_perf <- agg_s[, 1:n_cols]
  fut_rets <- agg_s[, n_cols+1:n_cols]
  # Calculate portfolio weights equal to number of shares
  end_prices <- price_s[end_points[-n_rows]]
  weight_s <- past_perf/rowSums(abs(past_perf))/end_prices
  weight_s[is.na(weight_s)] <- 0
  colnames(weight_s) <- colnames(re_turns)
  # Calculate profits and losses
  pnl_s <- rowSums(weight_s*fut_rets)
  pnl_s <- xts(pnl_s, index(end_prices))
  colnames(pnl_s) <- "pnls"
  # Calculate transaction costs
  cost_s <- 0.5*bid_offer*end_prices*abs(rutils::diff_it(weight_s))
  cost_s <- rowSums(cost_s)
  pnl_s <- (pnl_s - cost_s)
  pnl_s <- cumsum(pnl_s)
  if (with_weights)
    cbind(pnl_s, weight_s)
  else
    pnl_s
}  # end backtest_ep
source("C:/Develop/R/lecture_slides/scripts/back_test.R")
look_backs <- seq(5, 60, by=5)
perform_ance <- function(re_turns) sum(re_turns)/sd(re_turns)
pro_files <- sapply(look_backs, function(x) {
  last(backtest_ep(re_turns=re_turns, price_s=price_s,
    re_balance="weeks", look_back=x, perform_ance=perform_ance))
})  # end sapply
plot(x=look_backs, y=pro_files, t="l",
  main="Strategy PnL as function of look_back",
  xlab="look_back (weeks)", ylab="pnl")
look_back <- look_backs[which.max(pro_files)]
pnl_s <- backtest_ep(re_turns=re_turns, price_s=price_s,
  re_balance="weeks", look_back=look_back, perform_ance=perform_ance,
  with_weights=TRUE)
cl_ose <- Cl(rutils::etf_env$VTI[index(pnl_s)])
# bind model returns with VTI
da_ta <- star_t
da_ta <- cbind(cl_ose, da_ta*pnl_s[, 1]+da_ta)
colnames(da_ta) <- c("VTI", "momentum")
# plot momentum strategy with VTI
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue")
chart_Series(da_ta, theme=plot_theme, lwd=2,
       name="Momentum PnL")
legend("topleft", legend=colnames(da_ta),
  inset=0.1, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")
# combine momentum strategy with static
da_ta <- cbind(da_ta, 0.5* (da_ta[, "VTI"] + da_ta[, "momentum"]))
colnames(da_ta) <- c("VTI", "momentum", "combined")
# calculate strategy annualized Sharpe ratios
sapply(da_ta, function(cumu_lative) {
  x_ts <- na.omit(diff(log(cumu_lative)))
  sqrt(52)*sum(x_ts)/sd(x_ts)/NROW(x_ts)
})  # end sapply
# calculate strategy correlations
cor(na.omit(diff(log(da_ta))))
# plot momentum strategy combined with VTI
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue", "green")
chart_Series(da_ta, theme=plot_theme,
       name="Momentum strategy combined with VTI")
legend("topleft", legend=colnames(da_ta),
  inset=0.1, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")
# Define all-weather symbols and weights
weight_s <- c(0.30, 0.55, 0.15)
all_weather <- (re_turns / price_s) %*% weight_s
all_weather <- cumsum(all_weather)
all_weather <- xts(all_weather, index(re_turns))[index(pnl_s)]
all_weather <- star_t*all_weather +
  star_t
colnames(all_weather) <- "all_weather"
# combine momentum strategy with all-weather
da_ta <- cbind(da_ta, all_weather)
# calculate strategy annualized Sharpe ratios
sapply(da_ta, function(cumu_lative) {
  x_ts <- na.omit(diff(log(cumu_lative)))
  sqrt(52)*sum(x_ts)/sd(x_ts)/NROW(x_ts)
})  # end sapply
# calculate strategy correlations
cor(na.omit(diff(log(da_ta))))
# plot momentum strategy, combined, and all-weather
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue", "green", "violet")
chart_Series(da_ta, theme=plot_theme, lwd=2, name="Momentum PnL")
legend("topleft", legend=colnames(da_ta),
  inset=0.1, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")
# calculate betas
beta_s <- c(1, rutils::etf_env$capm_stats[
  match(sym_bols[-1],
  rownames(rutils::etf_env$capm_stats)),
  "Beta"])
names(beta_s)[1] <- sym_bols[1]
# weights times betas
weight_s <- price_s[index(pnl_s)]*pnl_s[, -1]
beta_s <- weight_s %*% beta_s
beta_s <- xts(beta_s, order.by=index(weight_s))
colnames(beta_s) <- "portf_beta"
zoo::plot.zoo(cbind(beta_s, cl_ose),
  oma = c(3, 1, 3, 0), mar = c(0, 4, 0, 1),
  main="betas & VTI", xlab="")
momentum_rets <- as.numeric(rutils::diff_it(pnl_s[, 1]))
vti_rets <- as.numeric(rutils::diff_it(cl_ose)/100)
# Merton-Henriksson test
vti_b <- cbind(vti_rets, vti_rets+abs(vti_rets))
colnames(vti_b) <- c("rets", "sign")
mod_el <- lm(momentum_rets ~ vti_b)
summary(mod_el)
# open x11 for plotting
x11(width=6, height=4)
# set plot parameters to reduce whitespace around plot
par(mar=c(4, 4, 3, 1), oma=c(0, 0, 0, 0))
# Treynor-Mazuy test
vti_b <- cbind(vti_rets, vti_rets^2)
colnames(vti_b) <- c("rets", "squared")
mod_el <- lm(momentum_rets ~ vti_b)
summary(mod_el)
# plot scatterplot
plot(x=vti_rets, y=momentum_rets,
     xlab="VTI", ylab="momentum")
title(main="Treynor-Mazuy market timing test\n for Momentum vs VTI", line=0.5)
# plot fitted (predicted) response values
points(x=vti_rets, y=mod_el$fitted.values,
 pch=16, col="red")
# Normalize the returns
momentum_rets <-
  (momentum_rets-mean(momentum_rets))
momentum_rets <-
  sd(vti_rets)*momentum_rets/sd(momentum_rets)
vti_rets <- (vti_rets-mean(vti_rets))
# calculate ratios of moments
sapply(2:4, FUN=moments::moment, x=vti_rets)/
  sapply(2:4, FUN=moments::moment, x=momentum_rets)
# plot histogram
x_lim <- 4*sd(momentum_rets)
hist(momentum_rets, breaks=30,
  main="Momentum and VTI Return Distributions",
  xlim=c(-x_lim, x_lim),
  xlab="", ylab="", freq=FALSE)
# draw kernel density of histogram
lines(density(momentum_rets), col='red', lwd=2)
lines(density(vti_rets), col='blue', lwd=2)
# add legend
legend("topright", inset=0.05, cex=0.8, title=NULL,
 leg=c("Momentum", "VTI"),
 lwd=6, bg="white", col=c("red", "blue"))
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
# number of look_backs that fit over re_turns
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
 lwd=6, lty=1,
 col=c("blue", "red"))
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
 lwd=6, lty=1,
 col=c("blue", "red"))
library(HighFreq)  # load HighFreq
# minutely SPY returns (unit per minute) single day
re_turns <- rutils::diff_it(log(SPY["2012-02-13", 4]))
# minutely SPY volatility (unit per minute)
sd(re_turns)
# minutely SPY returns (unit per second)
re_turns <- rutils::diff_it(log(SPY["2012-02-13", 4])) / 
  c(1, diff(.index(SPY["2012-02-13"])))
# minutely SPY volatility scaled to unit per minute
60*sd(re_turns)
# minutely SPY returns multiple days no overnight scaling
re_turns <- rutils::diff_it(log(SPY[, 4]))
# minutely SPY volatility (unit per minute)
sd(re_turns)
# minutely SPY returns (unit per second)
re_turns <- rutils::diff_it(log(SPY[, 4])) / 
  c(1, diff(.index(SPY)))
# minutely SPY volatility scaled to unit per minute
60*sd(re_turns)
table(c(1, diff(.index(SPY))))
library(HighFreq)  # load HighFreq
# Daily OHLC SPY prices
SPY_daily <- 
  rutils::to_period(oh_lc=SPY, period="days")
# Daily SPY returns and volatility
sd(rutils::diff_it(log(SPY_daily[, 4])))
# minutely SPY returns (unit per minute)
re_turns <- rutils::diff_it(log(SPY[, 4]))
# minutely SPY volatility scaled to daily interval
sqrt(6.5*60)*sd(re_turns)

# minutely SPY returns (unit per second)
re_turns <- rutils::diff_it(log(SPY[, 4])) / 
  c(1, diff(.index(SPY)))
# minutely SPY volatility scaled to daily aggregation interval
60*sqrt(6.5*60)*sd(re_turns)

# Daily SPY volatility
# including extra time over weekends and holidays
24*60*60*sd(rutils::diff_it(log(SPY_daily[, 4])) / 
    c(1, diff(.index(SPY_daily))))
table(c(1, diff(.index(SPY_daily))))
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
# formula of linear model with zero intercept
for_mula <- z ~ x + y - 1
for_mula

# Collapse vector of strings into single text string
paste0("x", 1:5)
paste(paste0("x", 1:5), collapse="+")

# Create formula from text string
for_mula <- as.formula(
  # Coerce text strings to formula
  paste("z ~ ",
  paste(paste0("x", 1:5), collapse="+")
  )  # end paste
)  # end as.formula
class(for_mula)
for_mula
# Modify the formula using "update"
update(for_mula, log(.) ~ . + beta)
set.seed(1121)  # initialize random number generator
# Define explanatory (design) variable
len_gth <- 100
de_sign <- runif(len_gth)
noise <- rnorm(len_gth)
# Response equals linear form plus random noise
res_ponse <- (1 + de_sign + noise)
# Calculate de-meaned explanatory (design) and response vectors
design_zm <- de_sign - mean(de_sign)
response_zm <- res_ponse - mean(res_ponse)
# Solve for the regression beta
be_ta <- sum(design_zm*response_zm) / sum(design_zm^2)
# Solve for the regression alpha
al_pha <- mean(res_ponse) - be_ta*mean(de_sign)
# Specify regression formula
for_mula <- res_ponse ~ de_sign
mod_el <- lm(for_mula)  # Perform regression
class(mod_el)  # Regressions have class lm
attributes(mod_el)
eval(mod_el$call$formula)  # Regression formula
mod_el$coeff  # Regression coefficients
all.equal(coef(mod_el), c(al_pha, be_ta), 
  check.attributes=FALSE)
x11(width=5, height=4)  # Open x11 for plotting
# Set plot parameters to reduce whitespace around plot
par(mar=c(5, 5, 2, 1), oma=c(0, 0, 0, 0))
# Plot scatterplot using formula
plot(for_mula, xlab="design", ylab="response")
title(main="Simple Regression", line=0.5)
# Add regression line
abline(mod_el, lwd=3, col="blue")
# Plot fitted (predicted) response values
points(x=de_sign, y=mod_el$fitted.values,
       pch=16, col="blue")
# Plot response without noise
lines(x=de_sign, y=(res_ponse-noise),
      col="red", lwd=3)
legend(x="topleft", # Add legend
       legend=c("response without noise", "fitted values"),
       title=NULL, inset=0.08, cex=0.8, lwd=6,
       lty=1, col=c("red", "blue"))
# Sum of residuals = 0
sum(mod_el$residuals)
x11(width=6, height=5)  # Open x11 for plotting
# Set plot parameters to reduce whitespace around plot
par(mar=c(5, 5, 1, 1), oma=c(0, 0, 0, 0))
# extract residuals
resi_duals <- cbind(de_sign, mod_el$residuals)
colnames(resi_duals) <- c("design", "residuals")
# Plot residuals
plot(resi_duals)
title(main="Residuals of the Linear Regression", line=-1)
abline(h=0, lwd=3, col="red")
model_sum <- summary(mod_el)  # Copy regression summary
model_sum  # Print the summary to console
attributes(model_sum)$names  # get summary elements
model_sum$coeff
model_sum$r.squared
model_sum$adj.r.squared
model_sum$fstatistic
# Standard error of beta
model_sum$
  coefficients["de_sign", "Std. Error"]
sd(model_sum$residuals)/sd(de_sign)/
  sqrt(unname(model_sum$fstatistic[3]))
anova(mod_el)
set.seed(1121)  # initialize random number generator
# high noise compared to coefficient
res_ponse <- (1 + de_sign + rnorm(30, sd=8))
mod_el <- lm(for_mula)  # Perform regression
# values of regression coefficients are not
# Statistically significant
summary(mod_el)
par(oma=c(1, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=1.0, cex.axis=1.0, cex.main=1.0, cex.sub=1.0)
reg_stats <- function(std_dev) {  # noisy regression
  set.seed(1121)  # initialize number generator
# Define explanatory (design) and response variables
  de_sign <- rnorm(100, mean=2)
  res_ponse <- (1 + 0.2*de_sign +
    rnorm(NROW(de_sign), sd=std_dev))
# Specify regression formula
  for_mula <- res_ponse ~ de_sign
# Perform regression and get summary
  model_sum <- summary(lm(for_mula))
# extract regression statistics
  with(model_sum, c(pval=coefficients[2, 4],
   adj_rsquared=adj.r.squared,
   fstat=fstatistic[1]))
}  # end reg_stats
# Apply reg_stats() to vector of std dev values
vec_sd <- seq(from=0.1, to=0.5, by=0.1)
names(vec_sd) <- paste0("sd=", vec_sd)
mat_stats <- t(sapply(vec_sd, reg_stats))
# Plot in loop
par(mfrow=c(NCOL(mat_stats), 1))
for (in_dex in 1:NCOL(mat_stats)) {
  plot(mat_stats[, in_dex], type="l",
 xaxt="n", xlab="", ylab="", main="")
  title(main=colnames(mat_stats)[in_dex], line=-1.0)
  axis(1, at=1:(NROW(mat_stats)),
 labels=rownames(mat_stats))
}  # end for
reg_stats <- function(da_ta) {  # get regression
# Perform regression and get summary
  col_names <- colnames(da_ta)
  for_mula <-
    paste(col_names[2], col_names[1], sep="~")
  model_sum <- summary(lm(for_mula,
                        data=da_ta))
# extract regression statistics
  with(model_sum, c(pval=coefficients[2, 4],
   adj_rsquared=adj.r.squared,
   fstat=fstatistic[1]))
}  # end reg_stats
# Apply reg_stats() to vector of std dev values
vec_sd <- seq(from=0.1, to=0.5, by=0.1)
names(vec_sd) <- paste0("sd=", vec_sd)
mat_stats <-
  t(sapply(vec_sd, function(std_dev) {
    set.seed(1121)  # initialize number generator
# Define explanatory (design) and response variables
    de_sign <- rnorm(100, mean=2)
    res_ponse <- (1 + 0.2*de_sign +
rnorm(NROW(de_sign), sd=std_dev))
    reg_stats(data.frame(de_sign, res_ponse))
    }))
# Plot in loop
par(mfrow=c(NCOL(mat_stats), 1))
for (in_dex in 1:NCOL(mat_stats)) {
  plot(mat_stats[, in_dex], type="l",
 xaxt="n", xlab="", ylab="", main="")
  title(main=colnames(mat_stats)[in_dex], line=-1.0)
  axis(1, at=1:(NROW(mat_stats)),
 labels=rownames(mat_stats))
}  # end for
# Set plot paramaters - margins and font scale
par(oma=c(1,0,1,0), mgp=c(2,1,0), mar=c(2,1,2,1), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2, 2))  # Plot 2x2 panels
plot(mod_el)  # Plot diagnostic scatterplots
plot(mod_el, which=2)  # Plot just Q-Q
library(lmtest)  # load lmtest
# Perform Durbin-Watson test
lmtest::dwtest(mod_el)
set.seed(1121)  # initialize random number generator
# Define design matrix
n_rows <- 100
n_cols <- 5
de_sign <- sapply(1:n_cols, function(col_umn) {
  sin(pi*col_umn*((1:n_rows)-(n_rows+1)/2)/n_rows)
})  # end sapply
# Add column names
colnames(de_sign) <- paste0("col", 1:n_cols)
# Plot design matrix
# matplot(de_sign, type="l", lty="solid", lwd=3)
# Define the design weights
weight_s <- runif(n_cols, min=(-10), max=10)
# Response equals linear form plus random noise
noise <- rnorm(n_rows, sd=0.1)
res_ponse <- (-1 + de_sign %*% weight_s + noise)
# Calculate de-meaned design matrix
design_zm <- t(t(de_sign) - colMeans(de_sign))
# Or
# design_zm <- apply(design_zm, 2, function(x) (x-mean(x)))
# Calculate de-meaned response vector
response_zm <- res_ponse - mean(res_ponse)
# Calculate the regression coefficients
beta_s <- MASS::ginv(design_zm) %*% response_zm
# Solve for the regression alpha
al_pha <- mean(res_ponse) - 
  sum(colSums(de_sign)*drop(beta_s))/n_rows
# Perform multivariate regression using lm()
mod_el <- lm(res_ponse ~ de_sign)
# Compare with coefficients from lm()
all.equal(coef(mod_el), c(al_pha, beta_s), check.attributes=FALSE)
# Compare with actual coefficients
all.equal(c(-1, weight_s), c(al_pha, beta_s), check.attributes=FALSE)
# Calculate fitted values from regression coefficients
fit_ted <- drop(al_pha + de_sign %*% beta_s)
all.equal(fit_ted, mod_el$fitted.values, check.attributes=FALSE)
# Calculate fitted values from zero mean data
fit_ted <- drop(mean(res_ponse) + design_zm %*% beta_s)
all.equal(fit_ted, mod_el$fitted.values, check.attributes=FALSE)
# Calculate the residuals
resid_uals <- drop(res_ponse - fit_ted)
all.equal(resid_uals, mod_el$residuals, check.attributes=FALSE)
# the residuals have zero mean
all.equal(sum(resid_uals), target=0)
# the residuals are orthogonal to the predictors
sapply(resid_uals %*% de_sign, 
       all.equal, target=0)
# the residuals are orthogonal to the fitted values
all.equal(sum(resid_uals*fit_ted), target=0)
# Calculate zero mean fitted values
fitted_zm <- drop(design_zm %*% beta_s)
all.equal(fitted_zm+mean(res_ponse), 
  mod_el$fitted.values, check.attributes=FALSE)
# Calculate the residuals
resid_uals <- drop(response_zm - fitted_zm)
all.equal(resid_uals, mod_el$residuals, check.attributes=FALSE)
# Add intercept column to design matrix
de_sign <- cbind(rep(1, NROW(de_sign)), de_sign)
# Add column name
colnames(de_sign)[1] <- "intercept"
# Calculate generalized inverse of the design matrix
design_inv <- MASS::ginv(de_sign)
# Add weight for intercept
weight_s <- c(-1, weight_s)
# Response equals linear form plus random noise
# noise <- rnorm(n_rows, sd=0.1)
res_ponse <- de_sign %*% weight_s + noise
# Calculate the regression coefficients
beta_s <- design_inv %*% res_ponse
# Perform multivariate regression without intercept term
mod_el <- lm(res_ponse ~ de_sign - 1)
all.equal(drop(beta_s), coef(mod_el), check.attributes=FALSE)
# Calculate the influence matrix
influ_ence <- de_sign %*% design_inv
# Calculate fitted values using influence matrix
fit_ted <- drop(influ_ence %*% res_ponse)
all.equal(fit_ted, mod_el$fitted.values, check.attributes=FALSE)
# Calculate fitted values from regression coefficients
fit_ted <- drop(de_sign %*% beta_s)
all.equal(fit_ted, mod_el$fitted.values, check.attributes=FALSE)
# Calculate the residuals
resid_uals <- drop(res_ponse - fit_ted)
all.equal(resid_uals, mod_el$residuals, check.attributes=FALSE)
# Define transformation matrix
n_cols <- NCOL(de_sign)
trans_mat <- matrix(runif(n_cols^2, min=(-1), max=1), 
            ncol=n_cols)
# Calculate linear combinations of design columns
design_trans <- de_sign %*% trans_mat
# Calculate the influence matrix
influence_trans <- design_trans %*% MASS::ginv(design_trans)
# Compare the influence matrices
all.equal(influ_ence, influence_trans)
# De-mean the design matrix columns
design_trans <- cbind(de_sign[, 1], t(t(de_sign[, -1])-colMeans(de_sign[, -1])))
round(apply(design_trans, 2, mean), 3)
# Calculate the influence matrix
influence_trans <- design_trans %*% MASS::ginv(design_trans)
# Compare the influence matrices
all.equal(influ_ence, influence_trans)
# Regression model summary
model_sum <- summary(mod_el)
# Degrees of freedom of residuals
deg_free <- (n_rows - NCOL(de_sign))
all.equal(deg_free, model_sum$df[2])
# variance of residuals
resid_var <- sum(resid_uals^2)/deg_free
# Design matrix squared
design_2 <- crossprod(de_sign)
# design_2 <- t(de_sign) %*% de_sign
# Calculate covariance matrix of betas
beta_covar <- resid_var*MASS::ginv(design_2)
# Round(beta_covar, 3)
beta_sd <- sqrt(diag(beta_covar))
all.equal(beta_sd, model_sum$coeff[, 2], check.attributes=FALSE)
# Calculate t-values of betas
beta_tvals <- drop(beta_s)/beta_sd
all.equal(beta_tvals, model_sum$coeff[, 3], check.attributes=FALSE)
# Calculate two-sided p-values of betas
beta_pvals <- 2*pt(-abs(beta_tvals), df=deg_free)
all.equal(beta_pvals, model_sum$coeff[, 4], check.attributes=FALSE)
# Calculate the influence matrix
influ_ence <- de_sign %*% design_inv
all.equal(influ_ence, influ_ence %*% influ_ence)
# Calculate covariance matrix of fitted values
fit_covar <- resid_var*influ_ence
# Calculate standard deviations of the fitted values
fit_sd <- sqrt(diag(fit_covar))
# Plot the standard deviations
plot(fit_sd, type="l", lwd=3, col="blue", ylab="",
     main="Standard Deviations of Fitted Values")
x11(width=5, height=4)  # Open x11 for plotting
# Set plot parameters to reduce whitespace around plot
par(mar=c(5, 5, 2, 1), oma=c(0, 0, 0, 0))
# univariate regression with linear predictor
de_sign <- cbind(rep(1, n_rows), 1:n_rows/n_rows)
# Calculate generalized inverse of the design matrix
design_inv <- MASS::ginv(de_sign)
# Calculate the influence matrix
influ_ence <- de_sign %*% design_inv
# Plot the leverage vector
plot(x=de_sign[,2], y=diag(influ_ence),
     type="l", lwd=3, col="blue",
     xlab="predictor", ylab="leverage",
     main="Leverage as Function of Predictor")
# Define the design weights
weight_s <- c(-1, 1)
# Response without random noise equals weighted sum over columns of de_sign
res_ponse <- de_sign %*% weight_s
# Perform loop over different realizations of random noise
fit_ted <- lapply(1:50, function(it) {
  # Add random noise to response
  res_ponse <- res_ponse + rnorm(n_rows, sd=1.0)
  # Calculate fitted values using influence matrix
  influ_ence %*% res_ponse
})  # end lapply
fit_ted <- rutils::do_call(cbind, fit_ted)
x11(width=5, height=4)  # Open x11 for plotting
# Set plot parameters to reduce whitespace around plot
par(mar=c(5, 5, 2, 1), oma=c(0, 0, 0, 0))
# Plot fitted values
matplot(x=de_sign[,2], y=fit_ted,
type="l", lty="solid", lwd=1, col="blue",
xlab="predictor", ylab="fitted",
main="Fitted Values for Different Realizations
of Random Noise")
lines(x=de_sign[,2], y=res_ponse, col="red", lwd=4)
legend(x="topleft", # Add legend
       legend=c("response without noise", "fitted values"),
       title=NULL, inset=0.05, cex=0.8, lwd=6,
       lty=1, col=c("red", "blue"))
# univariate regression with linear predictor
de_sign <- cbind(rep(1, n_rows), 1:n_rows/n_rows)
res_ponse <- de_sign %*% weight_s + rnorm(n_rows, sd=0.3)
design_inv <- MASS::ginv(de_sign)
influ_ence <- de_sign %*% design_inv
beta_s <- design_inv %*% res_ponse
fit_ted <- drop(de_sign %*% beta_s)
resid_uals <- drop(res_ponse - fit_ted)
deg_free <- (n_rows - NCOL(de_sign))
r_ss <- sqrt(sum(resid_uals^2)/deg_free)
# Inverse of design matrix squared
design_2 <- MASS::ginv(crossprod(de_sign))
# Define new predictors
new_predictors <- (max(de_sign[, 2]) + 10*(1:5)/n_rows)
# Calculate the predicted values and standard errors
new_design <- cbind(rep(1, NROW(new_predictors)), new_predictors)
predic_tions <- cbind(
  predicted=drop(new_design %*% beta_s),
  stddev=diag(r_ss*sqrt(new_design %*% design_2 %*% t(new_design))))
# OR: Perform loop over new_predictors
predic_tions <- sapply(new_predictors, function(predic_tor) {
  predic_tor <- cbind(1, predic_tor)
  # Calculate predicted values
  predic_ted <- predic_tor %*% beta_s
  # Calculate standard deviation
  predict_sd <- r_ss*sqrt(predic_tor %*% design_2 %*% t(predic_tor))
  c(predicted=predic_ted, stddev=predict_sd)
})  # end sapply
predic_tions <- t(predic_tions)
# Prepare plot data
x_data <- c(de_sign[,2], new_predictors)
x_lim <- range(x_data)
y_data <- c(fit_ted, predic_tions[, 1])
# Calculate t-quantile
t_quant <- qt(pnorm(2), df=deg_free)
predict_low <- predic_tions[, 1]-t_quant*predic_tions[, 2]
predict_high <- predic_tions[, 1]+t_quant*predic_tions[, 2]
y_lim <- range(c(res_ponse, y_data, predict_low, predict_high))
# Plot the regression predictions
plot(x=x_data, y=y_data,
     xlim=x_lim, ylim=y_lim,
     type="l", lwd=3, col="blue",
     xlab="predictor", ylab="fitted or predicted",
     main="Predictions from Linear Regression")
points(x=de_sign[,2], y=res_ponse, col="blue")
points(x=new_predictors, y=predic_tions[, 1], pch=16, col="blue")
lines(x=new_predictors, y=predict_high, lwd=3, col="red")
lines(x=new_predictors, y=predict_low, lwd=3, col="green")
legend(x="topleft", # Add legend
       legend=c("predictions", "+2SD", "-2SD"),
       title=NULL, inset=0.05, cex=0.8, lwd=6,
       lty=1, col=c("blue", "red", "green"))
# Perform regression
predic_tor <- de_sign[, 2]
mod_el <- lm(res_ponse ~ predic_tor)
# Perform prediction from regression
new_data <- data.frame(predic_tor=new_predictors)
predict_lm <- predict(object=mod_el,
  newdata=new_data, level=1-2*(1-pnorm(2)),
  interval="confidence")
predict_lm <- as.data.frame(predict_lm)
all.equal(predict_lm$fit, predic_tions[, 1])
all.equal(predict_lm$lwr, predict_low)
all.equal(predict_lm$upr, predict_high)
plot(res_ponse ~ predic_tor,
     xlim=range(predic_tor, new_data),
     ylim=range(res_ponse, predict_lm),
     xlab="predictor", ylab="fitted or predicted",
     main="Predictions from lm() Regression")
abline(mod_el, col="blue", lwd=3)
with(predict_lm, {
  points(x=new_data$predic_tor, y=fit, pch=16, col="blue")
  lines(x=new_data$predic_tor, y=lwr, lwd=3, col="green")
  lines(x=new_data$predic_tor, y=upr, lwd=3, col="red")
})  # end with
legend(x="topleft", # Add legend
       legend=c("predictions", "+2SD", "-2SD"),
       title=NULL, inset=0.05, cex=0.8, lwd=6,
       lty=1, col=c("blue", "red", "green"))
# Perform PCA
pc_a <- prcomp(design_zm, 
       center=TRUE, scale=TRUE)
design_pca <- pc_a$x
round(cov(design_pca), 2)
round(apply(design_pca, 2, mean), 3)
round(apply(design_pca, 2, sd), 2)
# Calculate the influence matrix
influ_ence <- design_zm %*% MASS::ginv(design_zm)
influence_pca <- design_pca %*% MASS::ginv(design_pca)
all.equal(influ_ence, influence_pca)
# Calculate the fitted values
fit_ted <- drop(de_sign %*% beta_s)
all.equal(fit_ted, mod_el$fitted.values, check.attributes=FALSE)
# Calculate the residuals
resid_uals <- drop(res_ponse - fit_ted)
all.equal(resid_uals, mod_el$residuals, check.attributes=FALSE)
# Residuals are orthogonal to fitted values
all.equal(sum(resid_uals*fit_ted), target=0)
# TSS = ESS + RSS
t_ss <- (n_rows-1)*var(drop(res_ponse))
e_ss <- (n_rows-1)*var(fit_ted)
r_ss <- (n_rows-1)*var(resid_uals)
all.equal(t_ss, e_ss + r_ss)
# Regression summary
model_sum <- summary(mod_el)
# Regression R-squared
r_squared <- e_ss/t_ss
all.equal(r_squared, model_sum$r.squared)
# Correlation between response and fitted values
cor_fitted <- drop(cor(res_ponse, fit_ted))
# Squared correlation between response and fitted values
all.equal(cor_fitted^2, r_squared)
x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
deg_free <- c(3, 5, 9)  # df values
col_ors <- c("black", "red", "blue", "green")
lab_els <- paste0("df1=", deg_free, ", df2=3")
for (in_dex in 1:NROW(deg_free)) {  # Plot four curves
curve(expr=df(x, df1=deg_free[in_dex], df2=3),
      type="l", xlim=c(0, 4),
      xlab="", ylab="", lwd=2,
      col=col_ors[in_dex],
      add=as.logical(in_dex-1))
}  # end for
# Add title
title(main="F-Distributions", line=0.5)
# Add legend
legend("topright", inset=0.05, title="degrees of freedom",
       lab_els, cex=0.8, lwd=2, lty=1,
       col=col_ors)
# F-statistic from lm()
model_sum$fstatistic
# Degrees of freedom of residuals
deg_free <- (n_rows - n_cols - 1)
# F-statistic from RSS
f_stat <- e_ss*deg_free/r_ss/n_cols
all.equal(f_stat, model_sum$fstatistic[1], check.attributes=FALSE)
# p-value of F-statistic
1-pf(q=f_stat, df1=n_rows-n_cols-1, df2=n_cols)
library(lmtest)  # load lmtest
de_sign <- data.frame(  # Design matrix
  de_sign=1:30, omit_var=sin(0.2*1:30))
# Response depends on both predictors
res_ponse <- with(de_sign,
  0.2*de_sign + omit_var + 0.2*rnorm(30))
# Mis-specified regression only one predictor
mod_el <- lm(res_ponse ~ de_sign,
        data=de_sign)
model_sum <- summary(mod_el)
model_sum$coeff
model_sum$r.squared
# Durbin-Watson test shows residuals are autocorrelated
dwtest(mod_el)$p.value
par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # Set plot panels
plot(for_mula, data=de_sign)
abline(mod_el, lwd=2, col="red")
title(main="OVB Regression", line=-1)
plot(mod_el, which=2, ask=FALSE)  # Plot just Q-Q
set.seed(1121)
library(lmtest)
# Spurious regression in unit root time series
de_sign <- cumsum(rnorm(100))  # unit root time series
res_ponse <- cumsum(rnorm(100))
for_mula <- res_ponse ~ de_sign
mod_el <- lm(for_mula)  # Perform regression
# Summary indicates statistically significant regression
model_sum <- summary(mod_el)
model_sum$coeff
model_sum$r.squared
# Durbin-Watson test shows residuals are autocorrelated
dw_test <- dwtest(mod_el)
c(dw_test$statistic[[1]], dw_test$p.value)
par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # Set plot panels
plot(for_mula, xlab="", ylab="")  # Plot scatterplot using formula
title(main="Spurious Regression", line=-1)
# Add regression line
abline(mod_el, lwd=2, col="red")
plot(mod_el, which=2, ask=FALSE)  # Plot just Q-Q
