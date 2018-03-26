library(knitr)
opts_chunk$set(prompt=TRUE, eval=FALSE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
options(digits=3)
thm <- knit_theme$get("acid")
knit_theme$set(thm)
library(xtable)
binbet_table <- data.frame(win=c("p", "b"), lose=c("q = 1 - p", "a"))
rownames(binbet_table) <- c("probability", "payout")
# print(xtable(binbet_table), comment=FALSE, size="tiny")
print(xtable(binbet_table), comment=FALSE)
par(mar=c(5, 2, 2, 2), mgp=c(1.5, 0.5, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# define utility
utility <- function(frac, p=0.5, a=1, b=4) {
  p*log(1+frac*b) + (1-p)*log(1-frac*a)
}  # end utility
# plot utility
curve(expr=utility, xlim=c(0, 1),
ylim=c(-0.5, 0.3), xlab="betting fraction",
ylab="utility", main="", lwd=2)
title(main="logarithmic utility", line=-0.8)
par(mar=c(5, 2, 2, 2), mgp=c(1.5, 0.5, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# define and plot Kelly fraction
kelly <- function(b, p=0.5, a=1) {
  p/a - (1-p)/b
}  # end kelly
curve(expr=kelly, xlim=c(0, 5),
ylim=c(-2, 1), xlab="betting odds",
ylab="kelly", main="", lwd=2)
abline(h=0.5, lwd=2, col="red")
text(x=1.5, y=0.5, pos=3, cex=0.8, labels="max Kelly fraction=0.5")
title(main="Kelly fraction", line=-0.8)
par(mar=c(5, 2, 2, 2), mgp=c(1.5, 0.5, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# plot several Kelly fractions
curve(expr=kelly, xlim=c(0, 5),
ylim=c(-1, 1.5), xlab="betting odds",
ylab="kelly", main="", lwd=2)
abline(h=0.5, lwd=2, col="red")
text(x=1.5, y=0.5, pos=3, cex=0.8, labels="a=1.0; max fraction=0.5")
kelly2 <- function(b) {kelly(b=b, a=0.5)}
curve(expr=kelly2, add=TRUE, main="", lwd=2)
abline(h=1.0, lwd=2, col="red")
text(x=1.5, y=1.0, pos=3, cex=0.8, labels="a=0.5; max fraction=1.0")
title(main="Kelly fraction", line=-0.8)
par(mar=c(5, 2, 2, 2), mgp=c(1.5, 0.5, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
set.seed(1121)  # reset random number generator
# simulated wealth path
wealth_path <- cumprod(1+runif(1000,
              min=-0.1, max=0.1))
plot(wealth_path, type="l",
     lty="solid", xlab="", ylab="")
title(main="wealth path", line=-1)
len_gth <- 1000  # number of simulation steps
n_simu <- 100  # number of simulation paths
# parameters for stock returns
prob_ab <- 0.51
pro_fit <- 0.001
lo_ss <- 0.001
# parameters for lottery ticket returns
prob_ab <- 0.01
pro_fit <- 0.001*51
lo_ss <- 0.001/(99/49)
# simulate random binary wealth paths
set.seed(1121)  # reset random number generator
path_s <- matrix(
  rbinom(n=n_simu*len_gth, size=1, prob=prob_ab),
  ncol=n_simu)
path_s <- (pro_fit + lo_ss)*path_s - lo_ss
path_s <- matrixStats::colCumsums(path_s)
x11()
ts.plot(path_s, xlab="number of steps", ylab="wealth")
title(main="wealth paths for stocks", line=-1)
par(mar=c(5, 2, 2, 2), mgp=c(1.5, 0.5, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# wealth of multiperiod binary betting
wealth <- function(f, b=2, a=1, n=100, i=51) {
  (1+f*b)^i * (1-f*a)^(n-i)
}  # end wealth
curve(expr=wealth, xlim=c(0, 1),
xlab="betting fraction",
ylab="wealth", main="", lwd=2)
title(main="wealth of multiperiod betting", line=0.1)
par(mar=c(5, 2, 2, 2), mgp=c(1.5, 0.5, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
library(HighFreq)
library(PerformanceAnalytics)
ts_rets <- rutils::env_etf$re_turns[, "VTI"]
c(mean(ts_rets), sd(ts_rets))
utility <- function(frac, r=ts_rets) {
sapply(frac, function (fract) sum(log(1+fract*r)))
}  # end utility
curve(expr=utility,
xlim=c(0.1, 2*PerformanceAnalytics::KellyRatio(R=ts_rets, method="full")),
xlab="kelly", ylab="utility", main="", lwd=2)
title(main="utility", line=-2)
par(mar=c(5, 2, 2, 2), mgp=c(1.5, 0.5, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
library(PerformanceAnalytics)
ts_rets <- rutils::env_etf$re_turns[, "VTI"]
PerformanceAnalytics::KellyRatio(R=ts_rets, method="full")
library(HighFreq)  # load package HighFreq
# Get close prices and calculate close-to-close returns
# price_s <- quantmod::Cl(rutils::env_etf$VTI)
price_s <- quantmod::Cl(HighFreq::SPY)
colnames(price_s) <- rutils::get_name(colnames(price_s))
re_turns <- TTR::ROC(price_s)
re_turns[1] <- 0
# Calculate the RSI indicator
r_si <- TTR::RSI(price_s, 2)
# Calculate the long (up) and short (dn) signals
sig_up <- ifelse(r_si < 10, 1, 0)
sig_dn <- ifelse(r_si > 90, -1, 0)
# Lag signals by one period
sig_up <- rutils::lag_it(sig_up, 1)
sig_dn <- rutils::lag_it(sig_dn, 1)
# Replace NA signals with zero position
sig_up[is.na(sig_up)] <- 0
sig_dn[is.na(sig_dn)] <- 0
# Combine up and down signals into one
sig_nals <- sig_up + sig_dn
# Calculate cumulative returns
eq_up <- exp(cumsum(sig_up*re_turns))
eq_dn <- exp(cumsum(-1*sig_dn*re_turns))
eq_all <- exp(cumsum(sig_nals*re_turns))
# Plot daily cumulative returns in panels
end_points <- endpoints(re_turns, on="days")
plot.zoo(cbind(eq_all, eq_up, eq_dn)[end_points], lwd=c(2, 2, 2),
  ylab=c("Total","Long","Short"), col=c("red","green","blue"),
  main=paste("RSI(2) strategy for", colnames(price_s), "from",
       format(start(re_turns), "%B %Y"), "to",
       format(end(re_turns), "%B %Y")))
library(HighFreq)  # load package HighFreq
# calculate open, close, and lagged prices
op_en <- Op(rutils::env_etf$VTI)
cl_ose <- Cl(rutils::env_etf$VTI)
close_adj <- (cl_ose - as.numeric(cl_ose[1, ]))
prices_lag <- rutils::lag_it(cl_ose)
# define aggregation interval and calculate VWAP
look_back <- 150
VTI_vwap <- HighFreq::roll_vwap(rutils::env_etf$VTI,
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
library(HighFreq)  # load package HighFreq
# calculate positions, either: -1, 0, or 1
position_s <- rep(NA_integer_, NROW(rutils::env_etf$VTI))
position_s[1] <- 0
position_s[trade_dates] <- in_dic[trade_dates]
position_s <- na.locf(position_s)
position_s <- xts(position_s, order.by=index((rutils::env_etf$VTI)))
pos_lagged <- rutils::lag_it(position_s)
# calculate daily profits and losses
pnl_s <- pos_lagged*(cl_ose - prices_lag)
pnl_s[trade_dates] <- pos_lagged[trade_dates] *
  (op_en[trade_dates] - prices_lag[trade_dates]) +
  position_s[trade_dates] *
  (cl_ose[trade_dates] - op_en[trade_dates])
# calculate annualized Sharpe ratio of strategy returns
sqrt(252)*sum(pnl_s)/sd(pnl_s)/NROW(pnl_s)
# plot prices and VWAP
pnl_s <- xts(cumsum(pnl_s), order.by=index((rutils::env_etf$VTI)))
close_adj <- (cl_ose - as.numeric(cl_ose[1, ]))
chart_Series(x=close_adj, name="VTI prices", col="orange")
add_TA(pnl_s, on=1, lwd=2, col="blue")
add_TA(position_s > 0, on=-1,
 col="lightgreen", border="lightgreen")
add_TA(position_s < 0, on=-1,
 col="lightgrey", border="lightgrey")
legend("top", legend=c("VTI", "VWAP strategy"),
 inset=0.1, bg="white", lty=1, lwd=6,
 col=c("orange", "blue"), bty="n")
library(HighFreq)  # load package HighFreq
# select OHLC data
oh_lc <- rutils::env_etf$VTI
# calculate close prices
cl_ose <- quantmod::Cl(oh_lc)
close_adj <- (cl_ose - as.numeric(cl_ose[1, ]))
# define length for weights and decay parameter
wid_th <- 251
lamb_da <- 0.01
# calculate EWMA prices
weight_s <- exp(-lamb_da*1:wid_th)
weight_s <- weight_s/sum(weight_s)
ew_ma <- stats::filter(cl_ose, filter=weight_s, sides=1)
ew_ma[1:(wid_th-1)] <- ew_ma[wid_th]
ew_ma <- xts(cbind(cl_ose, ew_ma),
       order.by=index(oh_lc))
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
# calculate daily profits and losses
re_turns <- pos_lagged*(cl_ose - prices_lag)
re_turns[trade_dates] <-
  pos_lagged[trade_dates] *
  (op_en[trade_dates] - prices_lag[trade_dates]) +
  position_s[trade_dates] *
  (cl_ose[trade_dates] - op_en[trade_dates]) -
  cost_s
# calculate annualized Sharpe ratio of strategy returns
sqrt(252)*sum(re_turns)/sd(re_turns)/NROW(re_turns)
pnl_s <- cumsum(re_turns)
pnl_s <- cbind(close_adj, pnl_s)
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
  out_put <- cbind(position_s, re_turns)
  colnames(out_put) <- c("positions", "returns")
  out_put
}  # end simu_ewma
source("C:/Develop/R/lecture_slides/scripts/ewma_model.R")
lamb_das <- seq(0.0001, 0.05, 0.005)
# perform lapply() loop over lamb_das
pro_files <- lapply(lamb_das, function(lamb_da) {
  # simulate EWMA strategy and calculate re_turns
  simu_ewma(oh_lc=oh_lc, lamb_da=lamb_da,
      wid_th=wid_th)[, "returns"]
})  # end lapply
pro_files <- rutils::do_call(cbind, pro_files)
colnames(pro_files) <- paste0("lambda=", lamb_das)
# plot EWMA strategies with custom line colors
column_s <- seq(1, NCOL(pro_files), by=3)
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NROW(column_s))
chart_Series(cumsum(pro_files[, column_s]),
  theme=plot_theme, name="Cumulative Returns of EWMA Strategies")
legend("topleft", legend=colnames(pro_files[, column_s]),
  inset=0.1, bg="white", cex=0.8, lwd=rep(6, NCOL(pro_files)),
  col=plot_theme$col$line.col, bty="n")
# initialize compute cluster under Windows
library(parallel)
clus_ter <- makeCluster(detectCores()-1)
clusterExport(clus_ter,
  varlist=c("oh_lc", "wid_th", "simu_ewma"))
# perform parallel loop over lamb_das under Windows
re_turns <- parLapply(clus_ter, lamb_das,
        function(lamb_da) {
  library(quantmod)
  # simulate EWMA strategy and calculate re_turns
  simu_ewma(oh_lc=oh_lc,
    lamb_da=lamb_da, wid_th=wid_th)[, "returns"]
})  # end parLapply
# perform parallel loop over lamb_das under Mac-OSX or Linux
re_turns <- mclapply(lamb_das,
        function(lamb_da) {
  library(quantmod)
  # simulate EWMA strategy and calculate re_turns
  simu_ewma(oh_lc=oh_lc,
    lamb_da=lamb_da, wid_th=wid_th)[, "returns"]
})  # end mclapply
stopCluster(clus_ter)  # stop R processes over cluster under Windows
re_turns <- rutils::do_call(cbind, re_turns)
colnames(re_turns) <- paste0("lambda=", lamb_das)
sharpe_ratios <- sqrt(252)*sapply(re_turns, function(x_ts) {
  # calculate annualized Sharpe ratio of strategy returns
  sum(x_ts)/sd(x_ts)
})/NROW(re_turns)  # end sapply
plot(x=lamb_das, y=sharpe_ratios, t="l",
     main="Performance of EWMA trend-following strategies
     as function of the decay parameter lambda")
trend_returns <- re_turns
trend_sharpe_ratios <- sharpe_ratios
# simulate best performing strategy
ewma_trend <- simu_ewma(oh_lc=oh_lc,
  lamb_da=lamb_das[which.max(sharpe_ratios)],
  wid_th=wid_th)
position_s <- ewma_trend[, "positions"]
pnl_s <- cumsum(ewma_trend[, "returns"])
pnl_s <- cbind(close_adj, pnl_s)
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
re_turns <- lapply(lamb_das, function(lamb_da) {
  # simulate EWMA strategy and calculate re_turns
  simu_ewma(oh_lc=oh_lc, lamb_da=lamb_da,
      wid_th=wid_th, tre_nd=(-1))[, "returns"]
})  # end lapply
re_turns <- rutils::do_call(cbind, re_turns)
colnames(re_turns) <- paste0("lambda=", lamb_das)
# plot EWMA strategies with custom line colors
column_s <- seq(1, NCOL(re_turns), by=4)
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NROW(column_s))
chart_Series(cumsum(re_turns[, column_s]),
  theme=plot_theme, name="Cumulative Returns of Mean-reverting EWMA Strategies")
legend("topleft", legend=colnames(re_turns[, column_s]),
  inset=0.1, bg="white", cex=0.8, lwd=rep(6, NCOL(re_turns)),
  col=plot_theme$col$line.col, bty="n")
sharpe_ratios <- sqrt(252)*sapply(re_turns, function(x_ts) {
  # calculate annualized Sharpe ratio of strategy returns
  sum(x_ts)/sd(x_ts)
})/NROW(re_turns)  # end sapply
plot(x=lamb_das, y=sharpe_ratios, t="l",
     main="Performance of EWMA mean-reverting strategies
     as function of the decay parameter lambda")
revert_returns <- re_turns
revert_sharpe_ratios <- sharpe_ratios
# simulate best performing strategy
ewma_revert <- simu_ewma(oh_lc=oh_lc,
  lamb_da=lamb_das[which.max(sharpe_ratios)],
  wid_th=wid_th, tre_nd=(-1))
position_s <- ewma_revert[, "positions"]
pnl_s <- cumsum(ewma_revert[, "returns"])
pnl_s <- cbind(close_adj, pnl_s)
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
close_rets <- rutils::diff_it(cl_ose)
corr_matrix <- cor(cbind(trend_ing, revert_ing, close_rets))
corr_matrix
# calculate combined strategy
com_bined <- trend_ing + revert_ing
colnames(com_bined) <- "combined"
# calculate annualized Sharpe ratio of strategy returns
sqrt(252)*sapply(
  cbind(close_rets, trend_ing, revert_ing, com_bined),
  function(x_ts) sum(x_ts)/sd(x_ts))/NROW(com_bined)  # end sapply
pnl_s <- cumsum(com_bined)
pnl_s <- cbind(close_adj, pnl_s)
colnames(pnl_s) <- c("VTI", "EWMA combined PnL")
chart_Series(pnl_s, theme=plot_theme,
       name="Performance of Combined EWMA Strategies")
add_TA(cumsum(trend_ing), on=1, lwd=2, col="green")
add_TA(cumsum(revert_ing), on=1, lwd=2, col="magenta2")
legend("topleft", legend=c(colnames(pnl_s), "trending", "reverting"),
 inset=0.05, bg="white", lty=rep(1, 4), lwd=rep(4, 4),
 col=c(plot_theme$col$line.col, "green", "magenta2"), bty="n")
sharpe_ratios <- c(trend_sharpe_ratios, revert_sharpe_ratios)
weight_s <- sharpe_ratios
weight_s[weight_s<0] <- 0
weight_s <- weight_s/sum(weight_s)
re_turns <- cbind(trend_returns, revert_returns)
avg_returns <- re_turns %*% weight_s
avg_returns <- xts(avg_returns, order.by=index(re_turns))
pnl_s <- cumsum(avg_returns)
pnl_s <- cbind(close_adj, pnl_s)
colnames(pnl_s) <- c("VTI", "EWMA PnL")
# plot EWMA PnL without position shading
chart_Series(pnl_s, theme=plot_theme,
       name="Performance of Ensemble EWMA Strategy")
legend("top", legend=colnames(pnl_s),
  inset=0.05, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")
library(HighFreq)
# end of month end_points
end_points <- rutils::calc_endpoints(re_turns,
          inter_val="months")
len_gth <- NROW(end_points)
# define 12-month look-back interval
look_back <- 12
# start_points are end_points lagged by look-back interval
start_points <- c(rep_len(1, look_back-1),
  end_points[1:(len_gth-look_back+1)])
# Perform loop over end_points and calculate aggregations
# agg_fun <- function(re_turns) sum(re_turns)/sd(re_turns)
agg_fun <- function(re_turns) sum(re_turns)
back_aggs <- sapply(1:(len_gth-1), function(it_er) {
  sapply(re_turns[start_points[it_er]:end_points[it_er]], agg_fun)
})  # end sapply
back_aggs <- t(back_aggs)
# define forward (future) endpoints
fwd_points <- end_points[c(2:len_gth, len_gth)]
fwd_rets <- sapply(1:(len_gth-1), function(it_er) {
  sapply(re_turns[(end_points[it_er]+1):fwd_points[it_er]], sum)
})  # end sapply
fwd_rets <- t(fwd_rets)
# calculate weight_s proportional to back_aggs
weight_s <- back_aggs
weight_s[weight_s<0] <- 0
# scale weight_s so their sum is equal to 1
weight_s <- weight_s/rowSums(weight_s)
# set NA values to zero
weight_s[is.na(weight_s)] <- 0
sum(is.na(weight_s))
in_dex <- index(re_turns[end_points[-len_gth]])
trend_weights <- rowMeans(weight_s[, 1:NCOL(trend_returns)])
revert_weights <- rowMeans(weight_s[, -(1:NCOL(trend_returns))])
diff_weights <- xts(trend_weights-revert_weights, order.by=in_dex)
close_adj <- (cl_ose - as.numeric(cl_ose[1, ]))
# de-mean weight_s so their sum is equal to 0
# weight_s <- weight_s - rowMeans(weight_s)
# find best and worst EWMA Strategies in each period
bes_t <- apply(weight_s, 1, which.max)
wors_t <- apply(weight_s, 1, which.min)
# plot the mean weights of EWMA Strategies
zoo::plot.zoo(cbind(diff_weights,
  close_adj[end_points[-len_gth]]),
  oma = c(3, 0, 3, 0), mar = c(0, 4, 0, 1),
  xlab=NULL, ylab=c("diff weights", "VTI"),
  main="Trend minus Revert Weights of EWMA strategies")
best_worst <- xts(cbind(bes_t, wors_t), order.by=in_dex)
zoo::plot.zoo(best_worst,
  oma = c(3, 0, 3, 0), mar = c(0, 4, 0, 1),
  xlab=NULL, ylab=c("best EWMA", "worst EWMA"),
  main="Best and Worst EWMA strategies")
# calculate backtest returns
pnl_s <- rowSums(weight_s*fwd_rets)
pnl_s <- xts(pnl_s, order.by=in_dex)
colnames(pnl_s) <- "ewma momentum"
close_rets <- rutils::diff_it(cl_ose[in_dex])
cor(cbind(pnl_s, close_rets))
pnl_s <- cumsum(pnl_s)
# plot the backtest
chart_Series(x=close_adj[end_points[-len_gth]],
  name="Back-test of EWMA strategies", col="orange")
add_TA(pnl_s, on=1, lwd=2, col="blue")
legend("top", legend=c("VTI", "EWMA"),
 inset=0.1, bg="white", lty=1, lwd=6,
 col=c("orange", "blue"), bty="n")
# shad_e <- xts(index(pnl_s) < as.Date("2008-01-31"), order.by=index(pnl_s))
# add_TA(shad_e, on=-1, col="lightgrey", border="lightgrey")
# text(x=7, y=0, labels="warmup period")
# Calculate ETF prices and simple returns
sym_bols <- c("VTI", "IEF", "DBC")
price_s <- rutils::env_etf$price_s[, sym_bols]
price_s <- zoo::na.locf(price_s)
price_s <- na.omit(price_s)
re_turns <- rutils::diff_it(price_s)
# Define look-back and look-forward intervals
end_points <- rutils::calc_endpoints(re_turns,
  inter_val="months")
n_col <- NCOL(re_turns)
len_gth <- NROW(end_points)
look_back <- 12
start_points <- c(rep_len(1, look_back-1),
  end_points[1:(len_gth-look_back+1)])
fwd_points <- end_points[c(2:len_gth, len_gth)]
# Perform loop over end-points and calculate aggregations
agg_fun <-
  function(re_turns) sum(re_turns)/sd(re_turns)
agg_s <- sapply(1:(len_gth-1), function(it_er) {
  c(back_aggs=sapply(re_turns[start_points[it_er]:end_points[it_er]], agg_fun),
    fwd_rets=sapply(re_turns[(end_points[it_er]+1):fwd_points[it_er]], sum))
})  # end sapply
agg_s <- t(agg_s)
# Select look-back and look-forward aggregations
back_aggs <- agg_s[, 1:n_col]
fwd_rets <- agg_s[, n_col+1:n_col]
# Calculate portfolio weights equal to number of shares
end_prices <- price_s[end_points[-len_gth]]
weight_s <-
  back_aggs/rowSums(abs(back_aggs))/end_prices
weight_s[is.na(weight_s)] <- 0
colnames(weight_s) <- colnames(re_turns)
# Calculate profits and losses
pnl_s <- rowSums(weight_s*fwd_rets)
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
cl_ose <- Cl(rutils::env_etf$VTI[index(end_prices)])
zoo::plot.zoo(cbind(cl_ose, pnl_s, weight_s),
  oma = c(3, 1, 3, 0), mar = c(0, 4, 0, 1), nc=1,
  xlab=NULL, main="ETF Momentum Strategy")
# define back-test functional
back_test_ep <- function(re_turns, price_s, agg_fun=sum,
    look_back=12, re_balance="months", bid_offer=0.001,
    end_points=rutils::calc_endpoints(re_turns, inter_val=re_balance),
    with_weights=FALSE, ...) {
  stopifnot("package:quantmod" %in% search() || require("quantmod", quietly=TRUE))
  # Define look-back and look-forward intervals
  n_col <- NCOL(re_turns)
  len_gth <- NROW(end_points)
  start_points <- c(rep_len(1, look_back-1), end_points[1:(len_gth-look_back+1)])
  fwd_points <- end_points[c(2:len_gth, len_gth)]
  # Perform loop over end-points and calculate aggregations
  agg_s <- sapply(1:(len_gth-1), function(it_er) {
    c(back_aggs=sapply(re_turns[start_points[it_er]:end_points[it_er]], agg_fun, ...),  # end sapply
    fwd_rets=sapply(re_turns[(end_points[it_er]+1):fwd_points[it_er]], sum))  # end sapply
  })  # end sapply
  agg_s <- t(agg_s)
  # Select look-back and look-forward aggregations
  back_aggs <- agg_s[, 1:n_col]
  fwd_rets <- agg_s[, n_col+1:n_col]
  # Calculate portfolio weights equal to number of shares
  end_prices <- price_s[end_points[-len_gth]]
  weight_s <- back_aggs/rowSums(abs(back_aggs))/end_prices
  weight_s[is.na(weight_s)] <- 0
  colnames(weight_s) <- colnames(re_turns)
  # Calculate profits and losses
  pnl_s <- rowSums(weight_s*fwd_rets)
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
}  # end back_test_ep
source("C:/Develop/R/lecture_slides/scripts/back_test.R")
look_backs <- seq(5, 60, by=5)
agg_fun <- function(re_turns) sum(re_turns)/sd(re_turns)
pro_files <- sapply(look_backs, function(x) {
  last(back_test_ep(re_turns=re_turns, price_s=price_s,
    re_balance="weeks", look_back=x, agg_fun=agg_fun))
})  # end sapply
plot(x=look_backs, y=pro_files, t="l",
  main="Strategy PnL as function of look_back",
  xlab="look_back (weeks)", ylab="pnl")
look_back <- look_backs[which.max(pro_files)]
pnl_s <- back_test_ep(re_turns=re_turns, price_s=price_s,
  re_balance="weeks", look_back=look_back, agg_fun=agg_fun,
  with_weights=TRUE)
cl_ose <- Cl(rutils::env_etf$VTI[index(pnl_s)])
# bind model returns with VTI
da_ta <- as.numeric(cl_ose[1, ])
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
all_weather <- as.numeric(cl_ose[1, ])*all_weather +
  as.numeric(cl_ose[1, ])
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
beta_s <- c(1, rutils::env_etf$capm_stats[
  match(sym_bols[-1],
  rownames(rutils::env_etf$capm_stats)),
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
vti_ <- cbind(vti_rets, vti_rets+abs(vti_rets))
colnames(vti_) <- c("rets", "sign")
reg_model <- lm(momentum_rets ~ vti_)
summary(reg_model)
# open x11 for plotting
x11(width=6, height=4)
# set plot parameters to reduce whitespace around plot
par(mar=c(4, 4, 3, 1), oma=c(0, 0, 0, 0))
# Treynor-Mazuy test
vti_ <- cbind(vti_rets, vti_rets^2)
colnames(vti_) <- c("rets", "squared")
reg_model <- lm(momentum_rets ~ vti_)
summary(reg_model)
# plot scatterplot
plot(x=vti_rets, y=momentum_rets,
     xlab="VTI", ylab="momentum")
title(main="Treynor-Mazuy market timing test\n for Momentum vs VTI", line=0.5)
# plot fitted (predicted) response values
points(x=vti_rets, y=reg_model$fitted.values,
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
# sym_bols contains all the symbols in rutils::env_etf$re_turns except for "VXX"
sym_bols <- colnames(rutils::env_etf$re_turns)
sym_bols <- sym_bols[!(sym_bols=="VXX")]
# Extract columns of rutils::env_etf$re_turns and remove NA values
re_turns <- rutils::env_etf$re_turns[, sym_bols]
re_turns <- zoo::na.locf(re_turns)
re_turns <- na.omit(re_turns)
# Calculate vector of monthly end points and start points
look_back <- 12
end_points <- rutils::calc_endpoints(re_turns, inter_val="months")
end_points[end_points<2*NCOL(re_turns)] <- 2*NCOL(re_turns)
len_gth <- NROW(end_points)
# sliding window
start_points <- c(rep_len(1, look_back-1), end_points[1:(len_gth-look_back+1)])
# OR expanding window
# start_points <- rep_len(1, NROW(end_points))
# risk_free is the daily risk-free rate
risk_free <- 0.03/260
# Calculate daily excess returns
ex_cess <- re_turns - risk_free
# Perform loop over end_points
portf_rets <- lapply(2:NROW(end_points),
  function(i) {
    # subset the ex_cess returns
    ex_cess <- ex_cess[start_points[i-1]:end_points[i-1], ]
    in_verse <- solve(cov(ex_cess))
    # calculate the maximum Sharpe ratio portfolio weights.
    weight_s <- in_verse %*% colMeans(ex_cess)
    weight_s <- drop(weight_s/sqrt(sum(weight_s^2)))
    # subset the re_turns
    re_turns <- re_turns[(end_points[i-1]+1):end_points[i], ]
    # calculate the out-of-sample portfolio returns
    xts(re_turns %*% weight_s, index(re_turns))
  }  # end anonymous function
)  # end lapply
portf_rets <- rutils::do_call(rbind, portf_rets)
colnames(portf_rets) <- "portf_rets"
# Calculate compounded cumulative portfolio returns
portf_rets <- cumsum(portf_rets)
quantmod::chart_Series(portf_rets,
  name="Cumulative Returns of Max Sharpe Portfolio Strategy")
# load S&P500 constituent stock prices
load("C:/Develop/R/lecture_slides/data/sp500.RData")
price_s <- eapply(env_sp500, quantmod::Cl)
price_s <- rutils::do_call(cbind, price_s)
# carry forward and backward non-NA prices
price_s <- zoo::na.locf(price_s)
price_s <- zoo::na.locf(price_s, fromLast=TRUE)
colnames(price_s) <- sapply(colnames(price_s),
  function(col_name) strsplit(col_name, split="[.]")[[1]][1])
# calculate the simple (dollar) returns of the S&P500 constituent stocks
re_turns <- rutils::diff_it(price_s)
da_ta <- rowSums(re_turns==0)
da_ta <- xts::xts(da_ta, order.by=index(price_s))
dygraphs::dygraph(da_ta, main="Number of S&P 500 Constituents Without Prices")
# select data after 2007
price_s <- price_s["2007/"]
re_turns <- re_turns["2007/"]
n_col <- NCOL(price_s)
save(price_s,
  file="C:/Develop/R/lecture_slides/data/sp500_prices.RData")
# calculate price weighted index of constituent
in_dex <- rowSums(price_s)/n_col
# rescale to VTI
in_dex <- as.numeric(env_etf$VTI[index(price_s[1]), 4])*in_dex/in_dex[1]
in_dex <- xts::xts(in_dex, order.by=index(price_s))
colnames(in_dex) <- "index"
da_ta <- cbind(in_dex, env_etf$VTI[index(price_s), 4])
col_names <- c("index", "VTI")
colnames(da_ta) <- col_names
# plot with VTI
dygraphs::dygraph(da_ta,
  main="S&P 500 Price-weighted Index and VTI") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(col_names[2], axis="y2", col=c("orange", "blue"))
load("C:/Develop/R/lecture_slides/data/sp500_prices.RData")
re_turns <- rutils::diff_it(price_s)
# calculate rolling variance of S&P500 portfolio
wid_th <- 252
vari_ance <- roll::roll_var(re_turns, width=wid_th)
vari_ance <- zoo::na.locf(vari_ance)
vari_ance[is.na(vari_ance)] <- 0
# calculate rolling Sharpe of S&P500 portfolio
returns_width <- rutils::diff_it(price_s, lagg=wid_th)
weight_s <- returns_width/sqrt(wid_th*vari_ance)
weight_s[vari_ance==0] <- 0
weight_s[1:wid_th, ] <- 1
weight_s[is.na(weight_s)] <- 0
weight_s <- weight_s/rowSums(abs(weight_s))/price_s
weight_s[is.na(weight_s)] <- 0
weight_s <- rutils::lag_it(weight_s)
sum(is.na(weight_s))
# calculate portfolio profits and losses
pnl_s <- rowSums(weight_s*re_turns)
# Calculate transaction costs
bid_offer <- 0.001
cost_s <- 0.5*bid_offer*price_s*abs(rutils::diff_it(weight_s))
cost_s <- rowSums(cost_s)
pnl_s <- (pnl_s - cost_s)
pnl_s <- cumsum(pnl_s)
pnl_s <- xts(pnl_s, order.by=index(price_s))
pnl_s <- cbind(rutils::env_etf$VTI[, 4], pnl_s)
pnl_s <- na.omit(pnl_s)
colnames(pnl_s) <- c("VTI", "momentum")
col_names <- colnames(pnl_s)
# plot momentum and VTI
dygraphs::dygraph(pnl_s, main=paste(col_names, collapse=" and ")) %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(col_names[2], axis="y2", col=c("blue", "red"))
# define back-test functional
back_test_rolling <- function(re_turns, price_s,
    wid_th=252, bid_offer=0.001, tre_nd=1, ...) {
  stopifnot("package:quantmod" %in% search() || require("quantmod", quietly=TRUE))
  # Define look-back and look-forward intervals
  n_col <- NCOL(re_turns)
  vari_ance <- roll::roll_var(re_turns, width=wid_th)
  vari_ance <- zoo::na.locf(vari_ance)
  vari_ance[is.na(vari_ance)] <- 0
  # calculate rolling Sharpe of S&P500 portfolio
  returns_width <- rutils::diff_it(price_s, lagg=wid_th)
  weight_s <- tre_nd*returns_width/sqrt(wid_th*vari_ance)
  weight_s[vari_ance==0] <- 0
  weight_s[1:wid_th, ] <- 1
  weight_s[is.na(weight_s)] <- 0
  weight_s <- weight_s/rowSums(abs(weight_s))/price_s
  weight_s[is.na(weight_s)] <- 0
  weight_s <- rutils::lag_it(weight_s)
  # calculate portfolio profits and losses
  pnl_s <- rowSums(weight_s*re_turns)
  # Calculate transaction costs
  bid_offer <- 0.001
  cost_s <- 0.5*bid_offer*price_s*abs(rutils::diff_it(weight_s))
  cost_s <- rowSums(cost_s)
  pnl_s <- (pnl_s - cost_s)
  pnl_s <- cumsum(pnl_s)
  pnl_s
}  # end back_test_rolling
source("C:/Develop/R/lecture_slides/scripts/back_test.R")
pnl_s <- back_test_rolling(wid_th=252, re_turns=re_turns,
  price_s=price_s, bid_offer=bid_offer)
width_s <- seq(50, 300, by=50)
# perform sapply loop over lamb_das
pro_files <- sapply(width_s, back_test_rolling, re_turns=re_turns,
  price_s=price_s, bid_offer=bid_offer)
colnames(pro_files) <- paste0("width=", width_s)
pro_files <- xts(pro_files, index(price_s))
# plot EWMA strategies with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(pro_files))
chart_Series(pro_files,
  theme=plot_theme, name="Cumulative Returns of S&P500 Momentum Strategies")
legend("bottomleft", legend=colnames(pro_files),
  inset=0.0, bg="white", cex=0.7, lwd=rep(6, NCOL(re_turns)),
  col=plot_theme$col$line.col, bty="n")
width_s <- seq(5, 50, by=5)
# perform sapply loop over lamb_das
pro_files <- sapply(width_s, back_test_rolling, re_turns=re_turns,
  price_s=price_s, bid_offer=bid_offer, tre_nd=(-1))
colnames(pro_files) <- paste0("width=", width_s)
pro_files <- xts(pro_files, index(price_s))
# plot EWMA strategies with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(pro_files))
chart_Series(pro_files,
  theme=plot_theme, name="Cumulative Returns of S&P500 Mean-reverting Strategies")
legend("bottomleft", legend=colnames(pro_files),
  inset=0.0, bg="white", cex=0.7, lwd=rep(6, NCOL(re_turns)),
  col=plot_theme$col$line.col, bty="n")
library(HighFreq)
load("C:/Develop/R/lecture_slides/data/sp500_prices.RData")
n_col <- NCOL(price_s)
# define end_points
end_points <- rutils::calc_endpoints(price_s, inter_val="months")
end_points <- end_points[end_points>50]
len_gth <- NROW(end_points)
look_back <- 12
start_points <- c(rep_len(1, look_back-1), end_points[1:(len_gth-look_back+1)])
# scale price_s
date_s <- index(price_s)
price_s <- t(t(price_s) / as.numeric(price_s[1, ]))
sum(is.na(price_s))
in_dex <- xts(rowSums(price_s)/n_col, date_s)
re_turns <- diff_it(price_s)
# compile backtest function
Rcpp::sourceCpp(file="C:/Develop/R/lecture_slides/scripts/rcpp_test6.cpp")
# run backtest function
al_pha <- 0.01
max_eigen <- 2
strat_rets_arma <- roll_portf(re_turns,
  re_turns,
  start_points-1,
  end_points-1,
  al_pha=al_pha,
  max_eigen=max_eigen)
# plot strategy
strat_rets_arma <- cumsum(strat_rets_arma)
strat_rets_arma <- xts(strat_rets_arma, date_s)
library(dygraphs)
strat_rets_arma <- cbind(strat_rets_arma, in_dex)
col_names <- c("Strategy", "Index")
colnames(strat_rets_arma) <- col_names
dygraphs::dygraph(strat_rets_arma, main=paste(col_names, collapse=" and ")) %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(col_names[2], axis="y2", col=c("red", "blue"))
# library(HighFreq)  # load package HighFreq
# define time interval for end points
re_balance <- "weeks"
# look-back interval is multiple of re_balance
look_back <- 41
# create index of rebalancing period end points
end_points <- xts::endpoints(rutils::env_etf$re_turns,
                       on=re_balance)
# start_points are multi-period lag of end_points
len_gth <- NROW(end_points)
start_points <- c(rep_len(1, look_back-1),
  end_points[1:(len_gth-look_back+1)])
# create list of look-back intervals
look_backs <- lapply(2:len_gth,
    function(it_er) {
start_points[it_er]:end_points[it_er]
  })  # end lapply
# library(HighFreq)  # load package HighFreq
# create vector of symbols for model
sym_bols <- c("VTI", "IEF", "DBC")

# calculate risk&ret stats for some symbols, over a range of dates
# perform lapply() loop over look_backs
risk_stats <- lapply(look_backs,
  function(look_back) {
    x_ts <-
rutils::env_etf$re_turns[look_back, sym_bols]
    t(sapply(x_ts,
function(col_umn)
  c(return=mean(col_umn), risk=mad(col_umn))
))  # end sapply
    })  # end lapply
# rbind list into single xts or matrix
# risk_stats <- rutils::do_call_rbind(risk_stats)
# head(risk_stats)
# calculate non-overlapping returns in interval
re_turns <-sapply(2:len_gth,
    function(it_er) {
    sapply(rutils::env_etf$re_turns[
(end_points[it_er-1]+1):end_points[it_er],
sym_bols], sum)
  })  # end sapply
re_turns <- t(re_turns)
