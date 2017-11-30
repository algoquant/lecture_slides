library(HighFreq)  # load package HighFreq
# select OHLC data
oh_lc <- rutils::env_etf$VTI["/2011"]
# calculate close prices
cl_ose <- Cl(oh_lc)
# define length for weights and decay parameter
wid_th <- 51
lamb_da <- 0.05
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
chart_Series(ew_ma, theme=plot_theme,
       name="EWMA prices")
legend("top", legend=colnames(ew_ma),
 inset=0.1, bg="white", lty=c(1, 1), lwd=c(6, 6),
 col=plot_theme$col$line.col, bty="n")
library(HighFreq)  # load package HighFreq
# determine dates right after EWMA has crossed prices
in_dic <- sign(cl_ose - ew_ma[, 2])
trade_dates <- (rutils::diff_xts(in_dic) != 0)
trade_dates <- which(trade_dates) + 1
# calculate positions, either: -1, 0, or 1
position_s <- rep(NA_integer_, NROW(cl_ose))
position_s[1] <- 0
position_s[trade_dates] <-
  rutils::lag_xts(in_dic)[trade_dates]
position_s <- na.locf(position_s)
position_s <- xts(position_s, order.by=index(oh_lc))
# plot EWMA prices with position shading
chart_Series(ew_ma, theme=plot_theme,
       name="EWMA prices")
add_TA(position_s > 0, on=-1,
 col="lightgreen", border="lightgreen")
add_TA(position_s < 0, on=-1,
 col="lightgrey", border="lightgrey")
legend("top", legend=colnames(ew_ma),
 inset=0.1, bg="white", lty=c(1, 1), lwd=c(6, 6),
 col=plot_theme$col$line.col, bty="n")
library(HighFreq)  # load package HighFreq
# calculate open and lagged prices
op_en <- Op(oh_lc)
prices_lag <- rutils::lag_xts(cl_ose)
position_lagged <- rutils::lag_xts(position_s)
# calculate daily profits and losses
re_turns <- position_lagged*(cl_ose - prices_lag)
re_turns[trade_dates] <-
  position_lagged[trade_dates] *
  (op_en[trade_dates] - prices_lag[trade_dates]) +
  position_s[trade_dates] *
  (cl_ose[trade_dates] - op_en[trade_dates])
# calculate annualized Sharpe ratio of strategy returns
sqrt(260)*sum(re_turns)/sd(re_turns)/NROW(re_turns)
pnl_s <- cumsum(re_turns)
pnl_s <- cbind(cl_ose-as.numeric(cl_ose[1, ]), pnl_s)
colnames(pnl_s) <- c("VTI", "EWMA PnL")
library(HighFreq)  # load package HighFreq
# plot EWMA PnL with position shading
chart_Series(pnl_s, theme=plot_theme,
       name="Performance of EWMA Strategy")
add_TA(position_s > 0, on=-1,
 col="lightgreen", border="lightgreen")
add_TA(position_s < 0, on=-1,
 col="lightgrey", border="lightgrey")
legend("top", legend=colnames(pnl_s),
 inset=0.05, bg="white", lty=c(1, 1), lwd=c(6, 6),
 col=plot_theme$col$line.col, bty="n")
library(HighFreq)  # load package HighFreq
simu_ewma <- function(oh_lc, lamb_da=0.05, wid_th=51) {
  # calculate EWMA prices
  weight_s <- exp(-lamb_da*1:wid_th)
  weight_s <- weight_s/sum(weight_s)
  cl_ose <- Cl(oh_lc)
  ew_ma <- stats::filter(as.numeric(cl_ose), filter=weight_s, sides=1)
  ew_ma[1:(wid_th-1)] <- ew_ma[wid_th]
  # determine dates right after EWMA has crossed prices
  in_dic <- xts(sign(as.numeric(cl_ose) - ew_ma), order.by=index(oh_lc))
  trade_dates <- (rutils::diff_xts(in_dic) != 0)
  trade_dates <- which(trade_dates) + 1
  trade_dates <- trade_dates[trade_dates<NROW(oh_lc)]
  # calculate positions, either: -1, 0, or 1
  position_s <- rep(NA_integer_, NROW(cl_ose))
  position_s[1] <- 0
  position_s[trade_dates] <- rutils::lag_xts(in_dic)[trade_dates]
  position_s <- xts(na.locf(position_s), order.by=index(oh_lc))
  op_en <- Op(oh_lc)
  prices_lag <- rutils::lag_xts(cl_ose)
  position_lagged <- rutils::lag_xts(position_s)
  # calculate daily profits and losses
  re_turns <- position_lagged*(cl_ose - prices_lag)
  re_turns[trade_dates] <-
    position_lagged[trade_dates] *
    (op_en[trade_dates] - prices_lag[trade_dates]) +
    position_s[trade_dates] *
    (cl_ose[trade_dates] - op_en[trade_dates])
  out_put <- cbind(position_s, re_turns)
  colnames(out_put) <- c("position_s", "re_turns")
  out_put
}  # end simu_ewma
lamb_das <- seq(0.001, 0.03, 0.001)
sharpe_ratios <- sapply(lamb_das, function(lamb_da) {
  re_turns <- simu_ewma(oh_lc=oh_lc,
        lamb_da=lamb_da)[, 2]
  # calculate annualized Sharpe ratio of strategy returns
  sqrt(260)*sum(re_turns)/sd(re_turns)/NROW(re_turns)
})  # end sapply
plot(x=lamb_das, y=sharpe_ratios, t="l",
     main="Performance of EWMA trend-following strategies
     as function of the decay parameter lambda")
# simulate best performing strategy
ewma_trend <- simu_ewma(oh_lc=oh_lc,
  lamb_da=lamb_das[which.max(sharpe_ratios)])
position_s <- ewma_trend[, 1]
pnl_s <- cumsum(ewma_trend[, 2])
pnl_s <- cbind(cl_ose-as.numeric(cl_ose[1, ]),
        pnl_s)
colnames(pnl_s) <- c("VTI", "EWMA PnL")
# plot EWMA PnL with position shading
chart_Series(pnl_s, theme=plot_theme,
       name="Performance of EWMA Strategy")
add_TA(position_s > 0, on=-1,
 col="lightgreen", border="lightgreen")
add_TA(position_s < 0, on=-1,
 col="lightgrey", border="lightgrey")
legend("top", legend=colnames(pnl_s),
  inset=0.05, bg="white", lty=c(1, 1), lwd=c(6, 6),
  col=plot_theme$col$line.col, bty="n")
lamb_das <- seq(0.05, 0.12, 0.01)
sharpe_ratios <- sapply(lamb_das, function(lamb_da) {
  re_turns <- -simu_ewma(oh_lc=oh_lc,
        lamb_da=lamb_da)[, 2]
  # calculate annualized Sharpe ratio of strategy returns
  sqrt(260)*sum(re_turns)/sd(re_turns)/NROW(re_turns)
})  # end sapply
plot(x=lamb_das, y=sharpe_ratios, t="l",
     main="Performance of EWMA mean-reverting strategies
     as function of the decay parameter lambda")
# simulate best performing strategy
ewma_revert <- -simu_ewma(oh_lc=oh_lc,
  lamb_da=lamb_das[which.max(sharpe_ratios)])
position_s <- ewma_revert[, 1]
pnl_s <- cumsum(ewma_revert[, 2])
pnl_s <- cbind(cl_ose-as.numeric(cl_ose[1, ]),
        pnl_s)
colnames(pnl_s) <- c("VTI", "EWMA PnL")
# plot EWMA PnL with position shading
chart_Series(pnl_s, theme=plot_theme,
       name="Performance of EWMA Strategy")
add_TA(position_s > 0, on=-1,
 col="lightgreen", border="lightgreen")
add_TA(position_s < 0, on=-1,
 col="lightgrey", border="lightgrey")
legend("top", legend=colnames(pnl_s),
  inset=0.05, bg="white", lty=c(1, 1), lwd=c(6, 6),
  col=plot_theme$col$line.col, bty="n")
# calculate correlation between trend-following and mean-reverting strategies
trend_ing <- ewma_trend[, 2]
colnames(trend_ing) <- "trend"
revert_ing <- ewma_revert[, 2]
colnames(revert_ing) <- "revert"
corr_matrix <- cor(cbind(trend_ing, revert_ing))
corr_matrix
# calculate combined strategy
re_turns <- trend_ing + revert_ing
# calculate annualized Sharpe ratio of strategy returns
sapply(cbind(rutils::diff_xts(cl_ose),
    trend_ing, revert_ing, re_turns),
function(re_turns) {
  sqrt(260)*sum(re_turns)/sd(re_turns)/NROW(re_turns)
})  # end sapply
pnl_s <- cumsum(re_turns)
pnl_s <- cbind(cl_ose-as.numeric(cl_ose[1, ]),
        pnl_s)
colnames(pnl_s) <- c("VTI", "EWMA combined PnL")
chart_Series(pnl_s, theme=plot_theme,
       name="Performance of EWMA Strategies")
add_TA(cumsum(trend_ing), on=1, lwd=2, col="green")
add_TA(cumsum(revert_ing), on=1, lwd=2, col="magenta2")
legend("topleft", legend=c(colnames(pnl_s), "trending", "reverting"),
 inset=0.05, bg="white", lty=rep(1, 4), lwd=rep(4, 4),
 col=c(plot_theme$col$line.col, "green", "magenta2"), bty="n")
lamb_das <- seq(0.001, 0.03, 0.001)
sharpe_ratios <- sapply(lamb_das, function(lamb_da) {
  re_turns <- simu_ewma(oh_lc=oh_lc,
        lamb_da=lamb_da)[, 2]
  # calculate annualized Sharpe ratio of strategy returns
  sqrt(260)*sum(re_turns)/sd(re_turns)/NROW(re_turns)
})  # end sapply
weight_s <- sharpe_ratios/sum(sharpe_ratios)
re_turns <- lapply(lamb_das, function(lamb_da) {
  simu_ewma(oh_lc=oh_lc, lamb_da=lamb_da)[, 2]
})  # end sapply
re_turns <- rutils::do_call(cbind, re_turns)
re_turns <- re_turns %*% weight_s
re_turns <- xts(re_turns, order.by=index(oh_lc))
pnl_s <- cumsum(re_turns)
pnl_s <- cbind(cl_ose-as.numeric(cl_ose[1, ]),
        pnl_s)
colnames(pnl_s) <- c("VTI", "EWMA PnL")
# plot EWMA PnL without position shading
chart_Series(pnl_s, theme=plot_theme,
       name="Performance of EWMA Strategy")
legend("top", legend=colnames(pnl_s),
  inset=0.05, bg="white", lty=c(1, 1), lwd=c(6, 6),
  col=plot_theme$col$line.col, bty="n")
library(HighFreq)  # load package HighFreq
# calculate open, close, and lagged prices
op_en <- Op(rutils::env_etf$VTI)
cl_ose <- Cl(rutils::env_etf$VTI)
prices_lag <- rutils::lag_xts(cl_ose)
# define aggregation interval and calculate VWAP
look_back <- 150
VTI_vwap <- HighFreq::roll_vwap(rutils::env_etf$VTI,
        look_back=look_back)
# calculate VWAP indicator
in_dic <- sign(cl_ose - VTI_vwap)
# determine dates right after VWAP has crossed prices
trade_dates <- (rutils::diff_xts(in_dic) != 0)
trade_dates <- which(trade_dates) + 1
# plot prices and VWAP
chart_Series(x=cl_ose,
  name="VTI prices", col="orange")
add_TA(VTI_vwap, on=1, lwd=2, col="blue")
legend("top", legend=c("VTI", "VWAP"),
  bg="white", lty=c(1, 1), lwd=c(6, 6),
  col=c("orange", "blue"), bty="n")
library(HighFreq)  # load package HighFreq
# calculate positions, either: -1, 0, or 1
position_s <- NA*numeric(NROW(rutils::env_etf$VTI))
position_s[1] <- 0
position_s[trade_dates] <- in_dic[trade_dates]
position_s <- na.locf(position_s)
position_s <- xts(position_s, order.by=index((rutils::env_etf$VTI)))
position_lagged <- rutils::lag_xts(position_s)
# calculate daily profits and losses
pnl_s <- position_lagged*(cl_ose - prices_lag)
pnl_s[trade_dates] <- position_lagged[trade_dates] *
  (op_en[trade_dates] - prices_lag[trade_dates]) +
  position_s[trade_dates] *
  (cl_ose[trade_dates] - op_en[trade_dates])
# calculate annualized Sharpe ratio of strategy returns
sqrt(260)*sum(pnl_s)/sd(pnl_s)/NROW(pnl_s)
# plot prices and VWAP
pnl_s <- xts(cumsum(pnl_s), order.by=index((rutils::env_etf$VTI)))
chart_Series(x=(cl_ose-as.numeric(cl_ose[1, ])), name="VTI prices", col="orange")
add_TA(pnl_s, on=1, lwd=2, col="blue")
add_TA(position_s > 0, on=-1,
 col="lightgreen", border="lightgreen")
add_TA(position_s < 0, on=-1,
 col="lightgrey", border="lightgrey")
legend("top", legend=c("VTI", "VWAP strategy"),
 inset=0.1, bg="white", lty=c(1, 1), lwd=c(6, 6),
 col=c("orange", "blue"), bty="n")
library(HighFreq)
# define oh_lc series
oh_lc <- rutils::env_etf$VTI["/2011"]
# end of month end_points
end_points <- rutils::calc_endpoints(oh_lc,
          inter_val="months")
# define EWMA width parameter
wid_th <- 51
# add warmup to end_points so they are greater than EWMA wid_th
end_points[end_points <= wid_th] <- wid_th + 1
# define 12-month look-back interval
look_back <- 12
# start_points are end_points lagged by look-back interval
len_gth <- NROW(end_points)
start_points <- c(rep_len(1, look_back-1),
  end_points[1:(len_gth-look_back+1)])
# create list of look-back intervals
look_backs <- lapply(seq_along(end_points),
  function(in_dex) {
    start_points[in_dex]:end_points[in_dex]
})  # end lapply
names(look_backs) <- index(oh_lc)[end_points]
# source EWMA model simu_ewma() from file
source("C:/Develop/R/lecture_slides/scripts/ewma_model.R")
# define aggregation function
calc_sharpe <- function(oh_lc, lamb_das, ...) {
  sapply(lamb_das, function(lamb_da) {
    # simulate EWMA strategy and calculate Sharpe ratio
    re_turns <- simu_ewma(oh_lc=oh_lc,
lamb_da=lamb_da, ...)[, "re_turns"]
    sqrt(260)*sum(re_turns)/sd(re_turns)/NROW(re_turns)
  })  # end sapply
}  # end calc_sharpe
# define EWMA parameters
wid_th <- 51
lamb_das <- seq(0.01, 1.0, 0.1)
# perform aggregation
calc_sharpe(oh_lc, lamb_das, wid_th)
# perform lapply() loop over look_backs
sharpe_ratios <- lapply(look_backs,
  function(look_back, ...) {
    calc_sharpe(oh_lc[look_back], ...)
    }, lamb_das=lamb_das, wid_th=wid_th)  # end lapply
# rbind list into single xts or matrix
sharpe_ratios <- rutils::do_call(rbind, sharpe_ratios)
if (!is.xts(sharpe_ratios))
  sharpe_ratios <- xts(sharpe_ratios, order.by=index(oh_lc[end_points]))
colnames(sharpe_ratios) <- paste0("lambda=", lamb_das)
roll_agg <- function(x_ts, look_backs, FUN, ...) {
  # perform lapply() loop over look_backs
  agg_s <- lapply(look_backs,
            function(look_back) {
              FUN(x_ts[look_back], ...)
            })  # end lapply
  # rbind list into single xts or matrix
  agg_s <- rutils::do_call_rbind(agg_s)
  if (!is.xts(agg_s))
    agg_s <- xts(agg_s,
order.by=index(x_ts[unlist(lapply(look_backs, last))]))
  agg_s
}  # end roll_agg
sharpe_ratios <- roll_agg(x_ts=oh_lc,
                    look_backs=look_backs,
                    FUN=calc_sharpe,
                    lamb_das=lamb_das,
                    wid_th=wid_th)
colnames(sharpe_ratios) <- paste0("lambda=", lamb_das)
source("C:/Develop/R/scripts/ewma_model.R")
lamb_das <- seq(0.01, 1.0, 0.1)
# perform lapply() loop over lamb_das
re_turns <- lapply(lamb_das, function(lamb_da) {
  # simulate EWMA strategy and calculate re_turns
  simu_ewma(oh_lc=oh_lc, lamb_da=lamb_da, wid_th=wid_th)[, "re_turns"]
})  # end lapply
re_turns <- rutils::do_call(cbind, re_turns)
colnames(re_turns) <- paste0("lambda=", lamb_das)
# plot EWMA strategies with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(re_turns))
chart_Series(cumsum(re_turns),
  theme=plot_theme,
  name="Cumulative Returns of EWMA Strategies")
legend("bottomleft", legend=colnames(re_turns),
  inset=0.02, bg="white", cex=0.8, lwd=rep(6, NCOL(re_turns)),
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
    lamb_da=lamb_da, wid_th=wid_th)[, "re_turns"]
})  # end parLapply
# perform parallel loop over lamb_das under Mac-OSX or Linux
re_turns <- mclapply(lamb_das,
        function(lamb_da) {
  library(quantmod)
  # simulate EWMA strategy and calculate re_turns
  simu_ewma(oh_lc=oh_lc,
    lamb_da=lamb_da, wid_th=wid_th)[, "re_turns"]
})  # end mclapply
stopCluster(clus_ter)  # stop R processes over cluster under Windows
re_turns <- rutils::do_call(cbind, re_turns)
colnames(re_turns) <- paste0("lambda=", lamb_das)
# calculate cumulative forward returns
back_rets <- sapply(look_backs, function(look_back) {
  sapply(re_turns[look_back], sum)  # end sapply
})  # end sapply
back_rets <- t(back_rets)
back_rets <- xts::xts(back_rets,
  order.by=index(re_turns)[end_points])
# define forward (future) endpoints
len_gth <- NROW(end_points)
fwd_points <- end_points[c(2:len_gth, len_gth)]
# create named list of look-forward intervals
look_fwds <- lapply(1:len_gth, function(in_dex)
  (end_points[in_dex]+1):fwd_points[in_dex])
look_fwds[[len_gth]] <- end_points[len_gth]
names(look_fwds) <- index(re_turns)[end_points]
look_backs[2]
look_fwds[2]
# calculate cumulative forward returns
fwd_rets <- sapply(look_fwds, function(look_fwd) {
  sapply(re_turns[look_fwd], sum)  # end sapply
})  # end sapply
fwd_rets <- t(fwd_rets)
fwd_rets <- xts::xts(fwd_rets,
  order.by=index(re_turns)[end_points])
# calculate weight_s proportional to past_aggs
weight_s <- coredata(back_rets)
weight_s <- weight_s/sqrt(rowSums(weight_s^2))
# select best and worst models in each period
bes_t <- apply(weight_s, 1, which.max)
wors_t <- apply(weight_s, 1, which.min)
pnl_s <- rowSums(weight_s * coredata(fwd_rets))
pnl_s <- xts(pnl_s, order.by=index(fwd_rets))
# plot the backtest
chart_Series(x=cumsum(pnl_s),
  name="Back-test of EWMA strategies")
# define aggregation functional
roll_agg <- function(re_turns, agg_fun=sum,
    look_back=12, re_balance="months",
    end_points=rutils::calc_endpoints(re_turns, inter_val=re_balance), ...) {
# define start_points and forward (future) endpoints
  len_gth <- NROW(end_points)
  start_points <- c(rep_len(1, look_back-1), end_points[1:(len_gth-look_back+1)])
  fwd_points <- end_points[c(2:len_gth, len_gth)]
# Perform loop over end_points and calculate aggregations
  agg_s <- sapply(2:(len_gth-1), function(in_dex) {
    c(sapply(re_turns[start_points[in_dex]:end_points[in_dex]], agg_fun, ...),  # end sapply
    sapply(re_turns[(end_points[in_dex]+1):fwd_points[in_dex]], sum))  # end sapply
  })  # end sapply
  agg_s <- t(agg_s)
  colnames(agg_s) <- c(paste0("past_", colnames(re_turns)), paste0("fwd_", colnames(re_turns)))
  xts::xts(agg_s,
    order.by=index(re_turns[end_points[2:(len_gth-1)]]))
}  # end roll_agg
agg_fun <- function(re_turns) sum(re_turns)/sd(re_turns)
agg_s <- roll_agg(re_turns, agg_fun=agg_fun,
  look_back=12, end_points=end_points)
# define agg_fun() equal to the Sharpe ratio
agg_fun <- function(re_turns)
  sum(re_turns)/sd(re_turns)
# Define vector of symbols for the model:
sym_bols <- c("VTI", "IEF", "DBC")
# apply roll_agg() to ETF returns
agg_s <- roll_agg(rutils::env_etf$re_turns[, sym_bols],
            agg_fun=agg_fun,
            look_back=52,
            re_balance="weeks")
# select aggregations over look-back intervals
past_aggs <- agg_s[, seq_along(sym_bols)]
# select aggregations over look-forward intervals
fwd_rets <- agg_s[, NROW(sym_bols) + seq_along(sym_bols)]
# calculate the portfolio weights proportional to past_aggs
weight_s <- past_aggs
# scale weight_s so that their sum of squares is equal to 1
weight_s <- weight_s/sqrt(rowSums(weight_s^2))
# plot the weights in multiple verticle panels
zoo::plot.zoo(weight_s, xlab=NULL, main="Momentum Portfolio Weights")
# calculate portfolio profits and losses
pnl_s <- rowSums(weight_s * fwd_rets)
pnl_s <- xts(pnl_s, order.by=index(fwd_rets))
# calculate vector of transaction costs
# bid_offer is equal to 10 bps for liquid ETFs
bid_offer <- 0.001
cost_s <- bid_offer*abs(rutils::diff_xts(weight_s))
cost_s <- rowSums(cost_s)
# subtract cost_s from pnl_s and calculate cumulative strategy pnl_s
pnl_s <- pnl_s - cost_s
pnl_s <- exp(cumsum(pnl_s))
# bind model returns with VTI
pnl_s <- cbind(rutils::env_etf$VTI[index(pnl_s), 4],
         pnl_s)
pnl_s[, 1] <- pnl_s[, 1] / as.numeric(pnl_s[1, 1])
colnames(pnl_s) <- c("VTI", "momentum")
# plot momentum strategy with VTI
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue")
chart_Series(pnl_s, theme=plot_theme, lwd=c(2, 2),
       name="Momentum PnL")
legend("topleft", legend=colnames(pnl_s),
  inset=0.1, bg="white", lty=c(1, 1), lwd=c(6, 6),
  col=plot_theme$col$line.col, bty="n")
# combine momentum strategy with static
pnl_s <- cbind(pnl_s, 0.5* (pnl_s[, "VTI"] + pnl_s[, "momentum"]))
colnames(pnl_s) <- c("VTI", "momentum", "combined")
# calculate strategy annualized Sharpe ratios
sapply(pnl_s, function(cumu_lative) {
  re_turns <- na.omit(diff(log(cumu_lative)))
  sqrt(260)*sum(re_turns)/sd(re_turns)/NROW(re_turns)
})  # end sapply
# calculate strategy correlations
cor(na.omit(diff(log(pnl_s))))
# plot momentum strategy combined with VTI
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue", "green")
chart_Series(pnl_s, theme=plot_theme,
       name="Momentum strategy combined with VTI")
legend("topleft", legend=colnames(pnl_s),
  inset=0.1, bg="white", lty=c(1, 1, 1), lwd=c(6, 6, 6),
  col=plot_theme$col$line.col, bty="n")
# calculate betas
beta_s <- c(1, rutils::env_etf$capm_stats[
  match(sym_bols[-1],
  rownames(rutils::env_etf$capm_stats)),
  "Beta"])
names(beta_s)[1] <- sym_bols[1]
# weights times betas
beta_s <- weight_s %*% beta_s
beta_s <- xts(beta_s, order.by=index(weight_s))
colnames(beta_s) <- "portf_beta"
plot.zoo(cbind(beta_s,
  rutils::env_etf$VTI[, 4])[index(beta_s)],
  main="betas & VTI", xlab="")
# define back-test functional
back_test <- function(re_turns, agg_fun=sum, look_back=12,
                re_balance="months", bid_offer=0.001) {
  stopifnot("package:quantmod" %in% search() || require("quantmod", quietly=TRUE))
  # Calculate aggregations.
  agg_s <- roll_agg(re_turns, agg_fun=agg_fun,
              look_back=look_back, re_balance=re_balance)
  # Select aggregations over look-back and look-forward intervals.
  past_aggs <- agg_s[, 1:NCOL(re_turns)]
  fwd_rets <- agg_s[, NCOL(re_turns)+1:NCOL(re_turns)]
  # Calculate portfolio weights.
  weight_s <- past_aggs
  weight_s <- weight_s/sqrt(rowSums(weight_s^2))
  # Calculate portfolio profits and losses.
  pnl_s <- rowSums(weight_s * fwd_rets)
  pnl_s <- xts(pnl_s, order.by=index(fwd_rets))
  colnames(pnl_s) <- "pnl_s"
  # Calculate transaction costs.
  cost_s <- bid_offer*abs(rutils::diff_xts(weight_s))
  cost_s <- rowSums(cost_s)
  pnl_s - cost_s
}  # end back_test
# define parameters
re_turns <- rutils::env_etf$re_turns[, sym_bols]
re_balance <- "weeks"
bid_offer <- 0.001
look_backs <- 10*(1:15)
# initialize compute cluster
library(parallel)
clus_ter <- makeCluster(detectCores()-1)
clusterExport(clus_ter,
  varlist=c("back_test", "roll_agg", "agg_fun", "re_turns", "re_balance", "bid_offer"))
# perform parallel loop over look_backs under Windows
pro_files <- parSapply(clus_ter, look_backs,
  function(look_back) {
  # perform back-test
    sum(back_test(re_turns=re_turns,
            agg_fun=agg_fun,
            look_back=look_back,
            re_balance=re_balance,
            bid_offer=bid_offer))
  })  # end parSapply
# perform parallel loop over look_backs under Mac-OSX or Linux
pro_files <- mclapply(look_backs, function(look_back) {
  # perform back-test
  sum(back_test(re_turns=re_turns,
    agg_fun=agg_fun,
    look_back=look_back,
    re_balance=re_balance,
    bid_offer=bid_offer))
})  # end mclapply
# stop R processes over cluster under Windows
stopCluster(clus_ter)
# non-parallel loop - for reference only
pro_files <- sapply(look_backs,
  function(look_back, ...)
    sum(back_test(look_back=look_back, ...)),
  re_turns=rutils::env_etf$re_turns[, sym_bols],
  agg_fun=agg_fun,
  re_balance="weeks",
  bid_offer=0.001)
plot(cbind(look_backs, pro_files), t="l",
     main="Strategy PnL as function of look_back",
     xlab="look_back (weeks)", ylab="pnl")
library(quantmod)
# vector of symbol names
sym_bols <- c("VTI", "IEF", "DBC")
n_weights <- NROW(sym_bols)
# calculate mean returns
re_turns <- na.omit(rutils::env_etf$re_turns[, sym_bols])
mean_rets <- sapply(re_turns, mean)
# specify weight bounds (default is c(0, Inf))
bound_s <-
  list(lower=list(ind=1:n_weights, val=rep(-1, n_weights)),
       upper=list(ind=1:n_weights, val=rep(1, n_weights)))
# specify weight constraints
constraint_s <- matrix(c(rep(1, n_weights),
               1, 1, 0),
               nc=n_weights, byrow=TRUE)
direction_s <- c("==", "<=")
limit_s <- c(1, 0)
# perform optimization
op_tim <- Rglpk::Rglpk_solve_LP(obj=mean_rets,
              mat=constraint_s,
              dir=direction_s,
              bounds=bound_s,
              rhs=limit_s,
              max=TRUE)
unlist(op_tim[1:2])
# define a covariance matrix
std_devs <- c(asset1=0.3, asset2=0.6)
cor_rel <- 0.8
co_var <- matrix(c(1, cor_rel, cor_rel, 1),
           nc=2)
co_var <- t(t(std_devs*co_var)*std_devs)
# calculate inverse of covariance mat_rix
in_verse <- solve(a=co_var)
u_nit <- rep(1, NCOL(co_var))
# minimum variance weights with constraint
# weight_s <- solve(a=co_var, b=u_nit)
weight_s <- in_verse %*% u_nit
weight_s <- weight_s / drop(t(u_nit) %*% weight_s)
# minimum variance
t(weight_s) %*% co_var %*% weight_s
1/(t(u_nit) %*% in_verse %*% u_nit)
# calculate excess re_turns
risk_free <- 0.03/260
ex_cess <- re_turns - risk_free
# calculate covariance and inverse matrix
co_var <- cov(re_turns)
u_nit <- rep(1, NCOL(co_var))
in_verse <- solve(a=co_var)
# calculate mean excess returns
ex_cess <- sapply(ex_cess, mean)
# weights of maximum Sharpe portfolio
# weight_s <- solve(a=co_var, b=re_turns)
weight_s <- in_verse %*% ex_cess
weight_s <- weight_s/drop(t(u_nit) %*% weight_s)
# Sharpe ratios
sqrt(260)*drop((t(weight_s) %*% ex_cess) /
  sqrt(t(weight_s) %*% co_var %*% weight_s))
sapply(re_turns - risk_free,
  function(x) sqrt(260)*mean(x)/sd(x))
weights_maxsharpe <- weight_s
library(quantmod)
# calculate minimum variance weights
weight_s <- in_verse %*% u_nit
weights_minvar <-
  weight_s / drop(t(u_nit) %*% weight_s)
# calculate optimal portfolio returns
optim_rets <- xts(
  x=cbind(exp(cumsum(re_turns %*% weights_maxsharpe)),
    exp(cumsum(re_turns %*% weights_minvar))),
  order.by=index(re_turns))
colnames(optim_rets) <- c("maxsharpe", "minvar")
# plot optimal portfolio returns, with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "green")
x11(width=6, height=5)
chart_Series(optim_rets, theme=plot_theme,
       name="Maximum Sharpe and \nMinimum Variance portfolios")
legend("top", legend=colnames(optim_rets), cex=0.8,
 inset=0.1, bg="white", lty=c(1, 1), lwd=c(6, 6),
 col=plot_theme$col$line.col, bty="n")
x11(wid_th <- 6, hei_ght <- 6)
# calculate minimum variance weights
weight_s <- in_verse %*% u_nit
weight_s <- weight_s / drop(t(u_nit) %*% weight_s)
# minimum standard deviation and return
std_dev <- sqrt(260*drop(t(weight_s) %*% co_var %*% weight_s))
min_ret <- 260*drop((t(weight_s) %*% mean_rets))
# calculate maximum Sharpe portfolios
risk_free <- (min_ret * seq(-10, 10, by=0.1)^3)/260
eff_front <- sapply(risk_free, function(risk_free) {
  weight_s <- in_verse %*% (mean_rets - risk_free)
  weight_s <- weight_s/drop(t(u_nit) %*% weight_s)
  # portfolio return and standard deviation
  c(return=260*drop((t(weight_s) %*% mean_rets)),
    stddev=sqrt(260*drop(t(weight_s) %*% co_var %*% weight_s)))
})  # end sapply
eff_front <- cbind(260*risk_free, t(eff_front))
colnames(eff_front)[1] <- "risk-free"
eff_front <- eff_front[is.finite(eff_front[, "stddev"]), ]
eff_front <- eff_front[order(eff_front[, "return"]), ]
# plot maximum Sharpe portfolios
plot(x=eff_front[, "stddev"],
     y=eff_front[, "return"], t="l",
     xlim=c(0.0*std_dev, 3.0*std_dev),
     ylim=c(0.0*min_ret, 2.0*min_ret),
     main="Efficient Frontier and Capital Market Line",
     xlab="standard deviation", ylab="return")
points(x=eff_front[, "stddev"], y=eff_front[, "return"],
 col="red", lwd=3)
# plot minimum variance portfolio
points(x=std_dev, y=min_ret, col="green", lwd=6)
text(std_dev, min_ret, labels="minimum \nvariance",
     pos=4, cex=0.8)
# draw Capital Market Line
sor_ted <- sort(eff_front[, 1])
risk_free <-
  sor_ted[findInterval(x=0.5*min_ret, vec=sor_ted)]
points(x=0, y=risk_free, col="blue", lwd=6)
text(x=0, y=risk_free, labels="risk-free",
     pos=4, cex=0.8)
in_dex <- match(risk_free, eff_front[, 1])
points(x=eff_front[in_dex, "stddev"],
 y=eff_front[in_dex, "return"],
 col="blue", lwd=6)
text(x=eff_front[in_dex, "stddev"],
     y=eff_front[in_dex, "return"],
     labels="market portfolio",
     pos=2, cex=0.8)
sharp_e <- (eff_front[in_dex, "return"]-risk_free)/
  eff_front[in_dex, "stddev"]
abline(a=risk_free, b=sharp_e, col="blue", lwd=2)
text(x=0.7*eff_front[in_dex, "stddev"],
     y=0.7*eff_front[in_dex, "return"]+0.01,
     labels="Capital Market Line", pos=2, cex=0.8,
     srt=45*atan(sharp_e*hei_ght/wid_th)/(0.25*pi))
# calculate random portfolios
n_portf <- 1000
ret_sd <- sapply(1:n_portf, function(in_dex) {
  weight_s <- runif(n_weights-1, min=-0.25, max=1.0)
  weight_s <- c(weight_s, 1-sum(weight_s))
  # portfolio return and standard deviation
  c(return=260*drop((t(weight_s) %*% mean_rets)),
    stddev=sqrt(260*drop(t(weight_s) %*% co_var %*% weight_s)))
})  # end sapply
# plot scatterplot of random portfolios
x11(wid_th <- 6, hei_ght <- 6)
plot(x=ret_sd["stddev", ], y=ret_sd["return", ],
     main="Efficient Frontier and Random Portfolios",
     xlim=c(0.5*std_dev, 0.8*max(ret_sd["stddev", ])),
     xlab="standard deviation", ylab="return")
# plot maximum Sharpe portfolios
lines(x=eff_front[, "stddev"],
     y=eff_front[, "return"], lwd=2)
points(x=eff_front[, "stddev"], y=eff_front[, "return"],
 col="red", lwd=3)
# plot minimum variance portfolio
points(x=std_dev, y=min_ret, col="green", lwd=6)
text(std_dev, min_ret, labels="minimum\nvariance",
     pos=2, cex=0.8)
# plot market portfolio
points(x=eff_front[in_dex, "stddev"],
 y=eff_front[in_dex, "return"], col="green", lwd=6)
text(x=eff_front[in_dex, "stddev"],
     y=eff_front[in_dex, "return"],
     labels="market\nportfolio",
     pos=2, cex=0.8)
# plot individual assets
points(x=sqrt(260*diag(co_var)),
 y=260*mean_rets, col="blue", lwd=6)
text(x=sqrt(260*diag(co_var)), y=260*mean_rets,
     labels=names(mean_rets),
     col="blue", pos=1, cex=0.8)
risk_free <- 0.03
re_turns <- c(asset1=0.05, asset2=0.06)
std_devs <- c(asset1=0.4, asset2=0.5)
cor_rel <- 0.6
co_var <- matrix(c(1, cor_rel, cor_rel, 1), nc=2)
co_var <- t(t(std_devs*co_var)*std_devs)
weight_s <- seq(from=-1, to=2, length.out=31)
weight_s <- cbind(weight_s, 1-weight_s)
portf_rets <- weight_s %*% re_turns
portf_sd <-
  sqrt(rowSums(weight_s * (weight_s %*% co_var)))
sharpe_ratios <- (portf_rets-risk_free)/portf_sd
in_dex <- which.max(sharpe_ratios)
max_Sharpe <- max(sharpe_ratios)
# plot efficient frontier
x11(wid_th <- 6, hei_ght <- 5)
par(mar=c(3,3,2,1)+0.1, oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
plot(portf_sd, portf_rets, t="l",
 main=paste0("Efficient frontier and CML for two assets\ncorrelation = ", 100*cor_rel, "%"),
 xlab="standard deviation", ylab="return",
 lwd=2, col="orange",
 xlim=c(0, max(portf_sd)),
 ylim=c(0.02, max(portf_rets)))
# add Market Portfolio (maximum Sharpe ratio portfolio)
points(portf_sd[in_dex], portf_rets[in_dex],
 col="blue", lwd=3)
text(x=portf_sd[in_dex], y=portf_rets[in_dex],
     labels=paste(c("market portfolio\n",
 structure(c(weight_s[in_dex], 1-weight_s[in_dex]),
         names=names(re_turns))), collapse=" "),
     pos=2, cex=0.8)
# plot individual assets
points(std_devs, re_turns, col="green", lwd=3)
text(std_devs, re_turns, labels=names(re_turns), pos=4, cex=0.8)
# add point at risk-free rate and draw Capital Market Line
points(x=0, y=risk_free, col="blue", lwd=3)
text(0, risk_free, labels="risk-free\nrate", pos=4, cex=0.8)
abline(a=risk_free, b=max_Sharpe, lwd=2, col="blue")
range_s <- par("usr")
text(portf_sd[in_dex]/2, (portf_rets[in_dex]+risk_free)/2,
     labels="Capital Market Line", cex=0.8, , pos=3,
     srt=45*atan(max_Sharpe*(range_s[2]-range_s[1])/
             (range_s[4]-range_s[3])*
             hei_ght/wid_th)/(0.25*pi))
# plot portfolios in x11() window
x11(wid_th <- 6, hei_ght <- 5)
par(oma=c(0, 0, 0, 0), mar=c(3,3,2,1)+0.1, mgp=c(2, 1, 0), cex.lab=1.0, cex.axis=1.0, cex.main=1.0, cex.sub=1.0)
# vector of symbol names
sym_bols <- c("VTI", "IEF")
# matrix of portfolio weights
weight_s <- seq(from=-1, to=2, length.out=31)
weight_s <- cbind(weight_s, 1-weight_s)
# calculate portfolio returns and volatilities
re_turns <- na.omit(rutils::env_etf$re_turns[, sym_bols])
ret_sd <- re_turns %*% t(weight_s)
ret_sd <- cbind(260*colMeans(ret_sd),
  sqrt(260)*matrixStats::colSds(ret_sd))
colnames(ret_sd) <- c("returns", "stddev")
risk_free <- 0.06
ret_sd <- cbind(ret_sd,
  (ret_sd[, "returns"]-risk_free)/ret_sd[, "stddev"])
colnames(ret_sd)[3] <- "Sharpe"
in_dex <- which.max(ret_sd[, "Sharpe"])
max_Sharpe <- ret_sd[in_dex, "Sharpe"]
plot(x=ret_sd[, "stddev"], y=ret_sd[, "returns"],
     main="Stock and Bond portfolios", t="l",
     xlim=c(0, 0.7*max(ret_sd[, "stddev"])), ylim=c(0, max(ret_sd[, "returns"])),
     xlab="standard deviation", ylab="return")
# add blue point for market portfolio
points(x=ret_sd[in_dex, "stddev"], y=ret_sd[in_dex, "returns"], col="blue", lwd=6)
text(x=ret_sd[in_dex, "stddev"], y=ret_sd[in_dex, "returns"],
     labels=paste(c("market portfolio\n", structure(c(weight_s[in_dex, 1], weight_s[in_dex, 2]), names=sym_bols)), collapse=" "),
     pos=3, cex=0.8)
# plot individual assets
mean_rets <- 260*sapply(re_turns, mean)
std_devs <- sqrt(260)*sapply(re_turns, sd)
points(std_devs, mean_rets, col="green", lwd=6)
text(std_devs, mean_rets, labels=names(re_turns), pos=2, cex=0.8)
# add point at risk-free rate and draw Capital Market Line
points(x=0, y=risk_free, col="blue", lwd=6)
text(0, risk_free, labels="risk-free", pos=4, cex=0.8)
abline(a=risk_free, b=max_Sharpe, col="blue", lwd=2)
range_s <- par("usr")
text(max(ret_sd[, "stddev"])/3, 0.75*max(ret_sd[, "returns"]),
     labels="Capital Market Line", cex=0.8, , pos=3,
     srt=45*atan(max_Sharpe*(range_s[2]-range_s[1])/
             (range_s[4]-range_s[3])*
             hei_ght/wid_th)/(0.25*pi))
# plot portfolios in x11() window
x11(wid_th <- 6, hei_ght <- 5)
# calculate cumulative returns of VTI and IEF
optim_rets <- lapply(re_turns,
  function(re_turns) exp(cumsum(re_turns)))
optim_rets <- rutils::do_call(cbind, optim_rets)
# calculate market portfolio returns
optim_rets <- cbind(
  exp(cumsum(re_turns %*%
    c(weight_s[in_dex], 1-weight_s[in_dex]))),
  optim_rets)
colnames(optim_rets)[1] <- "market"
# plot market portfolio with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue", "green")
chart_Series(optim_rets, theme=plot_theme,
       name="Market portfolio for stocks and bonds")
legend("top", legend=colnames(optim_rets),
 cex=0.8, inset=0.1, bg="white", lty=c(1, 1),
 lwd=c(6, 6), col=plot_theme$col$line.col, bty="n")
# create initial vector of portfolio weights
weight_s <- rep(1, NROW(sym_bols))
names(weight_s) <- sym_bols
# objective equal to minus Sharpe ratio
object_ive <- function(weight_s) {
  portf_rets <- re_turns %*% weight_s
  -mean(portf_rets)/sd(portf_rets)
}  # end object_ive
# objective for equal weight portfolio
object_ive(weight_s)
op_tim <- unlist(optimize(
  f=function(weight) object_ive(c(1, 1, weight)),
  interval=c(-4, 1)))
# vectorize objective function with respect to third weight
vec_object <- function(weights) sapply(weights,
  function(weight) object_ive(c(1, 1, weight)))
# or
vec_object <- Vectorize(
  FUN=function(weight) object_ive(c(1, 1, weight)),
  vectorize.args="weight"
)  # end Vectorize
vec_object(1)
vec_object(1:3)
x11(width=6, height=5)
par(oma=c(1, 1, 1, 1), mgp=c(2, 1, 0), mar=c(3, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# plot objective function with respect to third weight
curve(expr=vec_object,
      type="l", xlim=c(-4.0, 1.0),
      xlab=paste("weight of", names(weight_s[3])),
      ylab="", lwd=2)
title(main="Objective Function", line=-1)  # add title
points(x=op_tim[1], y=op_tim[2], col="green", lwd=6)
text(x=op_tim[1], y=op_tim[2],
     labels="minimum objective", pos=4, cex=0.8)

#below is simplified code for plotting objective function
# create vector of DBC weights
weight_s <- seq(from=-4, to=1, by=0.1)
obj_val <- sapply(weight_s,
  function(weight) object_ive(c(1, 1, weight)))
plot(x=weight_s, y=obj_val, t="l",
      xlab="weight of DBC", ylab="", lwd=2)
title(main="Objective Function", line=-1)  # add title
points(x=op_tim[1], y=op_tim[2], col="green", lwd=6)
text(x=op_tim[1], y=op_tim[2],
     labels="minimum objective", pos=4, cex=0.8)
# vectorize function with respect to all weights
vec_object <- Vectorize(
  FUN=function(w1, w2, w3)
    object_ive(c(w1, w2, w3)),
  vectorize.args=c("w2", "w3"))  # end Vectorize
# calculate objective on 2-d (w2 x w3) parameter grid
w2 <- seq(-3, 7, length=50)
w3 <- seq(-5, 5, length=50)
grid_object <- outer(w2, w3, FUN=vec_object, w1=1)
rownames(grid_object) <- round(w2, 2)
colnames(grid_object) <- round(w3, 2)
# perspective plot of objective function
persp(w2, w3, -grid_object,
theta=45, phi=30, shade=0.5,
col=rainbow(50), border="green",
main="objective function")
# interactive perspective plot of objective function
library(rgl)
rgl::persp3d(z=-grid_object, zlab="objective",
  col="green", main="objective function")
rgl::persp3d(
  x=function(w2, w3)
    -vec_object(w1=1, w2, w3),
  xlim=c(-3, 7), ylim=c(-5, 5),
  col="green", axes=FALSE)
# optimization to find weights with maximum Sharpe ratio
op_tim <- optim(par=weight_s,
             fn=object_ive,
             method="L-BFGS-B",
             upper=c(1.1, 10, 10),
             lower=c(0.9, -10, -10))
# optimal parameters
op_tim$par
op_tim$par <- op_tim$par/sum(op_tim$par)
# optimal Sharpe ratio
-object_ive(op_tim$par)
x11(width=6, height=5)
par(oma=c(1, 1, 1, 0), mgp=c(2, 1, 0), mar=c(2, 1, 2, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# plot in two vertical panels
layout(matrix(c(1,2), 2),
 widths=c(1,1), heights=c(1,3))
# barplot of optimal portfolio weights
barplot(op_tim$par, col=c("red", "green", "blue"),
  main="Optimized portfolio weights")
# calculate cumulative returns of VTI, IEF, DBC
cum_rets <- lapply(re_turns,
  function(re_turns) exp(cumsum(re_turns)))
cum_rets <- rutils::do_call(cbind, cum_rets)
# calculate optimal portfolio returns with VTI, IEF, DBC
optim_rets <- cbind(
  exp(cumsum(re_turns %*% op_tim$par)),
  cum_rets)
colnames(optim_rets)[1] <- "optim_rets"
# plot optimal returns with VTI, IEF, DBC
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("black", "red", "green", "blue")
chart_Series(optim_rets, theme=plot_theme,
       name="Optimized portfolio performance")
legend("top", legend=colnames(optim_rets), cex=0.8,
 inset=0.1, bg="white", lty=c(1, 1), lwd=c(6, 6),
 col=plot_theme$col$line.col, bty="n")
# or plot non-compounded (simple) cumulative returns
PerformanceAnalytics::chart.CumReturns(
  cbind(re_turns %*% op_tim$par, re_turns),
  lwd=2, ylab="", legend.loc="topleft", main="")
library(quadprog)
# minimum variance weights without constraints
op_tim <-  solve.QP(Dmat=2*co_var,
            dvec=rep(0, 2),
            Amat=matrix(0, nr=2, nc=1),
            bvec=0)
# minimum variance weights sum equal to 1
op_tim <-  solve.QP(Dmat=2*co_var,
            dvec=rep(0, 2),
            Amat=matrix(1, nr=2, nc=1),
            bvec=1)
# optimal value of objective function
t(op_tim$solution) %*% co_var %*% op_tim$solution
perform simple optimization for reference
# objective function for simple optimization
object_ive <- function(x) {
  x <- c(x, 1-x)
  t(x) %*% co_var %*% x
}  # end object_ive
unlist(optimize(f=object_ive, interval=c(-1, 2)))
# calculate daily percentage re_turns
re_turns <- na.omit(rutils::env_etf$re_turns[, sym_bols])
# calculate the covariance matrix
co_var <- cov(re_turns)
# minimum variance weights, with sum equal to 1
op_tim <-  quadprog::solve.QP(Dmat=2*co_var,
            dvec=rep(0, 3),
            Amat=matrix(1, nr=3, nc=1),
            bvec=1)
# minimum variance, maximum returns
op_tim <-  quadprog::solve.QP(Dmat=2*co_var,
            dvec=sapply(0.1*re_turns, mean),
            Amat=matrix(1, nr=3, nc=1),
            bvec=1)
# minimum variance positive weights, with sum equal to 1
a_mat <- cbind(matrix(1, nr=3, nc=1),
       diag(3),-diag(3))
b_vec <- c(1, rep(0, 3), rep(-1, 3))
op_tim <-  quadprog::solve.QP(Dmat=2*co_var,
            dvec=rep(0, 3),
            Amat=a_mat,
            bvec=b_vec,
            meq=1)
