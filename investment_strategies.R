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
sig_up <- rutils::lag_xts(sig_up, 1)
sig_dn <- rutils::lag_xts(sig_dn, 1)
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
in_dex <- endpoints(re_turns, on="days")
plot.zoo( cbind(eq_all, eq_up, eq_dn)[in_dex], lwd=c(2, 2, 2),
    ylab=c("Total","Long","Short"), col=c("red","green","blue"),
    main=paste("RSI(2) strategy for", colnames(price_s), "from",
               format(start(re_turns), "%B %Y"), "to",
               format(end(re_turns), "%B %Y")))
library(HighFreq)  # load package HighFreq
# select OHLC data
oh_lc <- rutils::env_etf$VTI["/2011"]
# calculate close prices
cl_ose <- Cl(oh_lc)
# define aggregation window and decay parameter
win_dow <- 51
lamb_da <- 0.05
# calculate EWMA prices
weight_s <- exp(-lamb_da*1:win_dow)
weight_s <- weight_s/sum(weight_s)
ew_ma <- stats::filter(cl_ose, filter=weight_s, sides=1)
ew_ma[1:(win_dow-1)] <- ew_ma[win_dow]
ew_ma <- xts(cbind(cl_ose, ew_ma),
       order.by=index(oh_lc))
colnames(ew_ma) <- c("VTI", "VTI EWMA")
# plot EWMA prices with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue")
chart_Series(ew_ma, theme=plot_theme,
       name="EWMA prices")
legend("top", legend=colnames(ew_ma),
 inset=0.1, bg="white", lty=c(1, 1), lwd=c(2, 2),
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
 inset=0.1, bg="white", lty=c(1, 1), lwd=c(2, 2),
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
 inset=0.05, bg="white", lty=c(1, 1), lwd=c(2, 2),
 col=plot_theme$col$line.col, bty="n")
library(HighFreq)  # load package HighFreq
simu_ewma <- function(oh_lc, lamb_da=0.05, win_dow=51) {
  # calculate EWMA prices
  weight_s <- exp(-lamb_da*1:win_dow)
  weight_s <- weight_s/sum(weight_s)
  cl_ose <- Cl(oh_lc)
  ew_ma <- stats::filter(as.numeric(cl_ose), filter=weight_s, sides=1)
  ew_ma[1:(win_dow-1)] <- ew_ma[win_dow]
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
 inset=0.05, bg="white", lty=c(1, 1), lwd=c(2, 2),
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
 inset=0.05, bg="white", lty=c(1, 1), lwd=c(2, 2),
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
re_turns <- lapply(lamb_das, function(lamb_da) {
  simu_ewma(oh_lc=oh_lc, lamb_da=lamb_da)[, 2]
})  # end sapply
re_turns <- do.call(merge, re_turns)
weight_s <- sharpe_ratios/sum(sharpe_ratios)
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
 inset=0.05, bg="white", lty=c(1, 1), lwd=c(2, 2),
 col=plot_theme$col$line.col, bty="n")
library(HighFreq)  # load package HighFreq
# calculate open, close, and lagged prices
op_en <- Op(rutils::env_etf$VTI)
cl_ose <- Cl(rutils::env_etf$VTI)
prices_lag <- rutils::lag_xts(cl_ose)
# define aggregation window and calculate VWAP
win_dow <- 150
VTI_vwap <- HighFreq::roll_vwap(rutils::env_etf$VTI,
      win_dow=win_dow)
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
bg="white", lty=c(1, 1), lwd=c(2, 2),
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
 inset=0.1, bg="white", lty=c(1, 1), lwd=c(2, 2),
 col=c("orange", "blue"), bty="n")
library(HighFreq)
# define oh_lc series and look-back interval
oh_lc <- rutils::env_etf$VTI["/2011"]
look_back <- 12
# calculate end of month end_points
end_points <- xts::endpoints(oh_lc, on="months")
# start_points are end_points lagged by look_back
len_gth <- NROW(end_points)
start_points <-
  end_points[c(rep_len(1, look_back-1),
         1:(len_gth-look_back+1))]
# create list of look-back intervals
look_backs <- lapply(2:len_gth,
  function(in_dex) start_points[in_dex]:end_points[in_dex])
# second warmup interval spans only two months
warm_up <- oh_lc[look_backs[[3]]]
dim(warm_up)
head(warm_up)
tail(warm_up)
# source EWMA model simu_ewma() from file
source("C:/Develop/R/scripts/ewma_model.R")
# define aggregation function
agg_regate <- function(oh_lc, lamb_das, ...) {
  sapply(lamb_das, function(lamb_da) {
    # simulate EWMA strategy and calculate Sharpe ratio
    re_turns <-
simu_ewma(oh_lc=oh_lc, lamb_da=lamb_da, ...)[, "re_turns"]
    sqrt(260)*sum(re_turns)/sd(re_turns)/NROW(re_turns)
  })  # end sapply
}  # end agg_regate
# define EWMA parameters
win_dow <- 51
lamb_das <- seq(0.001, 0.01, 0.001)
# perform aggregation
agg_regate(oh_lc, lamb_das, win_dow)
# adjust end_points so they are greater than EWMA win_dow
end_points[(end_points > 0) &
  (end_points <= win_dow)] <- win_dow+1
# start_points are end_points lagged by look_back
len_gth <- NROW(end_points)
start_points <- end_points[c(rep_len(1, look_back-1),
         1:(len_gth-look_back+1))]
# create list of look-back intervals
look_backs <- lapply(2:len_gth,
  function(in_dex) start_points[in_dex]:end_points[in_dex])
# perform lapply() loop over look_backs
agg_s <- lapply(look_backs,
  function(look_back, ...) {
    agg_regate(oh_lc[look_back], ...)
    }, lamb_das=lamb_das, win_dow=win_dow)  # end lapply
# rbind list into single xts or matrix
agg_s <- rutils::do_call_rbind(agg_s)
if (!is.xts(agg_s))
  agg_s <- xts(agg_s, order.by=index(oh_lc[end_points]))
roll_agg <- function(x_ts, end_points, look_back, FUN, ...) {
  len_gth <- NROW(end_points)
  # start_points are multi-period lag of end_points
  start_points <- end_points[c(rep_len(1, look_back-1), 1:(len_gth-look_back+1))]
  # perform lapply() loop over length of end_points
  agg_s <- lapply(2:len_gth,
            function(in_dex) {
              FUN(x_ts[start_points[in_dex]:end_points[in_dex]], ...)
            })  # end lapply
  # rbind list into single xts or matrix
  agg_s <- rutils::do_call_rbind(agg_s)
  if (!is.xts(agg_s))
    agg_s <- xts(agg_s, order.by=index(x_ts[end_points]))
  agg_s
}  # end roll_agg
sharpe_ratios <- roll_agg(x_ts=oh_lc,
                    end_points=end_points,
                    look_back=look_back,
                    FUN=agg_regate,
                    lamb_das=lamb_das,
                    win_dow=win_dow)
roll_agg <- function(x_ts, look_backs, FUN, ...) {
  # perform lapply() loop over look_backs
  agg_s <- lapply(look_backs,
            function(look_back) {
              FUN(x_ts[look_back], ...)
            })  # end lapply
  # rbind list into single xts or matrix
  agg_s <- rutils::do_call_rbind(agg_s)
  if (!is.xts(agg_s))
    agg_s <- xts(agg_s, order.by=
             index(x_ts[unlist(lapply(look_backs, last))]))
  agg_s
}  # end roll_agg
sharpe_ratios <- roll_agg(x_ts=oh_lc,
                    look_backs=look_backs,
                    FUN=agg_regate,
                    lamb_das=lamb_das,
                    win_dow=win_dow)
source("C:/Develop/R/scripts/ewma_model.R")
lamb_das <- seq(0.01, 1.0, 0.1)
# perform lapply() loop over lamb_das
re_turns <- lapply(lamb_das, function(lamb_da) {
  # simulate EWMA strategy and calculate re_turns
  simu_ewma(oh_lc=oh_lc, lamb_da=lamb_da, win_dow=win_dow)[, "re_turns"]
})  # end lapply
re_turns <- do.call(merge, re_turns)
colnames(re_turns) <- paste0("lambda=", lamb_das)
# plot EWMA strategies with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- colorRampPalette(c("red", "blue"))(NCOL(re_turns))
chart_Series(cumsum(re_turns), theme=plot_theme, name="Cumulative Returns of EWMA Strategies")
legend("bottomleft", legend=colnames(re_turns),
 inset=0.02, bg="white", cex=0.8, lwd=rep(6, NCOL(re_turns)),
 col=plot_theme$col$line.col, bty="n")
# initialize compute cluster under Windows
library(parallel)
clus_ter <- makeCluster(detectCores()-1)
win_dow <- 51
clusterExport(clus_ter,
        varlist=c("oh_lc", "win_dow", "simu_ewma"))
# perform parallel loop over lamb_das under Windows
lamb_das <- seq(0.01, 1.0, 0.1)
re_turns <- parLapply(clus_ter, lamb_das, function(lamb_da) {
  library(quantmod)
  # simulate EWMA strategy and calculate re_turns
  simu_ewma(oh_lc=oh_lc,
      lamb_da=lamb_da, win_dow=win_dow)[, "re_turns"]
})  # end parLapply
# perform parallel loop over lamb_das under Mac-OSX or Linux
re_turns <- mclapply(lamb_das, function(lamb_da) {
  library(quantmod)
  # simulate EWMA strategy and calculate re_turns
  simu_ewma(oh_lc=oh_lc,
      lamb_da=lamb_da, win_dow=win_dow)[, "re_turns"]
})  # end mclapply
stopCluster(clus_ter)  # stop R processes over cluster under Windows
re_turns <- do.call(merge, re_turns)
colnames(re_turns) <- paste0("lambda=", lamb_das)
look_back <- 12
# calculate end of month end_points
end_points <- xts::endpoints(re_turns, on="months")
# start_points are end_points lagged by look_back
len_gth <- NROW(end_points)
start_points <- end_points[c(rep_len(1, look_back-1), 1:(len_gth-look_back+1))]
# Create a named list of look-back intervals.
look_backs <- lapply(2:len_gth,
               function(in_dex) start_points[in_dex]:end_points[in_dex])
names(look_backs) <- index(re_turns)[end_points[2:len_gth]]
# Perform loop over look_back intervals and calculate past_aggs.
past_aggs <- sapply(look_backs,
             function(look_back) {
               sapply(re_turns[look_back], sum)  # end sapply
             })  # end sapply
past_aggs <- t(past_aggs)
past_aggs <- xts::as.xts(past_aggs)
# define forward (future) endpoints
fwd_points <- end_points[c(2:len_gth, len_gth)]
# create named list of look-forward intervals
look_fwds <- lapply(2:(len_gth-1),
              function(in_dex) (end_points[in_dex]+1):fwd_points[in_dex])
names(look_fwds) <- index(re_turns)[end_points[2:(len_gth-1)]]
# calculate cumulative forward returns
fwd_rets <- sapply(look_fwds,
        function(look_fwd) {
          sapply(re_turns[look_fwd], sum)  # end sapply
        })  # end sapply
fwd_rets <- t(fwd_rets)
fwd_rets <- xts::as.xts(fwd_rets)
# calculate weight_s proportional to past_aggs
weight_s <- coredata(past_aggs[index(fwd_rets)])
weight_s <- weight_s/sqrt(rowSums(weight_s^2))
# select best and worst models in each period
bes_t <- apply(weight_s, 1, which.max)
wors_t <- apply(weight_s, 1, which.min)
pnl_s <- rowSums(weight_s * coredata(fwd_rets))
pnl_s <- xts(pnl_s, order.by=index(fwd_rets))
chart_Series(x=cumsum(pnl_s),
       name="Back-test of EWMA strategies")
# define aggregation functional
roll_agg <- function(re_turns, agg_fun=sum,
    look_back=12, re_balance="months",
    end_points=xts::endpoints(re_turns, on=re_balance), ...) {
# define start_points and forward (future) endpoints
  len_gth <- NROW(end_points)
  start_points <- end_points[c(rep_len(1, look_back-1), 1:(len_gth-look_back+1))]
  fwd_points <- end_points[c(2:len_gth, len_gth)]
# Perform loop over end_points and calculate aggregations
  agg_s <- sapply(2:(len_gth-1), function(in_dex) {
    c(sapply(re_turns[start_points[in_dex]:end_points[in_dex]], agg_fun, ...),  # end sapply
    sapply(re_turns[(end_points[in_dex]+1):fwd_points[in_dex]], sum))  # end sapply
  })  # end sapply
  agg_s <- t(agg_s)
#  colnames(agg_s) <- c(paste0("past_", colnames(re_turns)), paste0("fwd_", colnames(re_turns)))
  xts::xts(agg_s,
     order.by=index(re_turns[end_points[2:(len_gth-1)]]))
}  # end roll_agg
agg_fun <- function(re_turns) sum(re_turns)/sd(re_turns)
agg_s <- roll_agg(re_turns, agg_fun=agg_fun,
            look_back=12, re_balance="months")
# define agg_fun() equal to the Sharpe ratio
agg_fun <- function(re_turns) sum(re_turns)/sd(re_turns)
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
x11()
plot.zoo(cbind(beta_s,
  rutils::env_etf$VTI[, 4])[index(beta_s)],
  main="betas & VTI", xlab="")
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
pnl_s <- merge(rutils::env_etf$VTI[index(pnl_s), 4],
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
pnl_s <- merge(pnl_s, 0.5* (pnl_s[, "VTI"] + pnl_s[, "Momentum"]))
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
start_points <- end_points[
  c(rep_len(1, look_back-1),
    1:(len_gth-look_back+1))]
# create list of look-back intervals
inter_vals <- lapply(2:len_gth,
    function(in_dex) {
start_points[in_dex]:end_points[in_dex]
  })  # end lapply
# library(HighFreq)  # load package HighFreq
# create vector of symbols for model
sym_bols <- c("VTI", "IEF", "DBC")

# calculate risk&ret stats for some symbols, over a range of dates
# perform lapply() loop over inter_vals
risk_stats <- lapply(inter_vals,
  function(inter_val) {
    x_ts <-
rutils::env_etf$re_turns[inter_val, sym_bols]
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
    function(in_dex) {
    sapply(rutils::env_etf$re_turns[
(end_points[in_dex-1]+1):end_points[in_dex],
sym_bols], sum)
  })  # end sapply
re_turns <- t(re_turns)
