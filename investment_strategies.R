library(knitr)
opts_chunk$set(prompt=TRUE, eval=FALSE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
options(digits=3)
thm <- knit_theme$get("acid")
knit_theme$set(thm)
library(xtable)
binbet_table <- data.frame(win=c("p", "b"), lose=c("q = 1 - p", "-a"))
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
# calculate open, close, and lagged prices
open_prices <- Op(rutils::env_etf$VTI)
price_s <- Cl(rutils::env_etf$VTI)
prices_lag <- rutils::lag_xts(price_s)
# define lookback window and calculate VWAP
win_dow <- 150
VTI_vwap <- HighFreq::roll_vwap(rutils::env_etf$VTI,
      win_dow=win_dow)
# calculate VWAP indicator
vwap_indic <- sign(price_s - VTI_vwap)
# determine dates right after VWAP has crossed prices
vwap_crosses <-
  (rutils::diff_xts(vwap_indic) != 0)
trade_dates <- which(vwap_crosses) + 1
# plot prices and VWAP
chart_Series(x=price_s,
  name="VTI prices", col='orange')
add_TA(VTI_vwap, on=1, lwd=2, col='blue')
legend("top", legend=c("VTI", "VWAP"),
bg="white", lty=c(1, 1), lwd=c(2, 2),
col=c('orange', 'blue'), bty="n")
library(HighFreq)  # load package HighFreq
# calculate positions, either: -1, 0, or 1
pos_ition <- NA*numeric(NROW(rutils::env_etf$VTI))
pos_ition[1] <- 0
pos_ition[trade_dates] <- vwap_indic[trade_dates]
pos_ition <- na.locf(pos_ition)
pos_ition <- xts(pos_ition, order.by=index((rutils::env_etf$VTI)))
position_lagged <- rutils::lag_xts(pos_ition)
# calculate periodic profits and losses
pn_l <- position_lagged*(price_s - prices_lag)
pn_l[trade_dates] <- position_lagged[trade_dates] *
  (open_prices[trade_dates] - prices_lag[trade_dates]) +
  pos_ition[trade_dates] *
  (price_s[trade_dates] - open_prices[trade_dates])
# calculate annualized Sharpe ratio of strategy returns
sqrt(260)*sum(pn_l)/sd(pn_l)/NROW(pn_l)
# plot prices and VWAP
pn_l <- xts(cumsum(pn_l), order.by=index((rutils::env_etf$VTI)))
chart_Series(x=(price_s-as.numeric(price_s[1, ])), name="VTI prices", col='orange')
add_TA(pn_l, on=1, lwd=2, col='blue')
add_TA(pos_ition > 0, on=-1,
 col="lightgreen", border="lightgreen")
add_TA(pos_ition < 0, on=-1,
 col="lightgrey", border="lightgrey")
legend("top", legend=c("VTI", "VWAP strategy"),
bg="white", lty=c(1, 1), lwd=c(2, 2),
col=c('orange', 'blue'), bty="n")
# library(HighFreq)  # load package HighFreq
# define time interval for end points
re_balance <- "weeks"
# look-back window is multiple of re_balance
win_dow <- 41
# create index of rebalancing period end points
end_points <- xts::endpoints(rutils::env_etf$re_turns,
                       on=re_balance)
# start_points are multi-period lag of end_points
len_gth <- length(end_points)
start_points <-  end_points[
  c(rep_len(1, win_dow-1),
    1:(len_gth-win_dow+1))]
# create list of look-back intervals
inter_vals <- lapply(2:len_gth,
    function(in_dex) {
start_points[in_dex]:end_points[in_dex]
  })  # end lapply
# library(HighFreq)  # load package HighFreq
# create list of symbols for model
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
# calculate list of portfolio weights
# perform lapply() loop over risk_stats
weight_s <- sapply(risk_stats,
    function(risk_stat) {
weight_s <- risk_stat[, 1]/risk_stat[, 2]
weight_s <- weight_s - mean(weight_s)
weight_s <- weight_s/sum(abs(weight_s))
    })  # end sapply
weight_s <- t(weight_s)
weights_xts <- xts(weight_s,
  order.by=index(rutils::env_etf$re_turns[end_points]))
# plot weights
x11()
zoo::plot.zoo(weights_xts, xlab=NULL)
# calculate pnls over all windows
pn_l <- rowSums(weight_s[-NROW(weight_s), ] *
            re_turns[-1, ])
# calculate transaction costs
bid_offer <- 0.001  # 10 bps for liquid ETFs
cost_s <- bid_offer*abs(diff(weight_s))
cost_s <- rowSums(cost_s)
pn_l <- pn_l - cost_s
pn_l <- xts(cumsum(pn_l),
  order.by=index(rutils::env_etf$re_turns[end_points[-(1:2)]]))
colnames(pn_l)[1] <- "pnl"
# plot strategy pnl
x11()
chart_Series(x=pn_l, name="Strategy PnL")
# calculate betas
beta_s <- c(1, rutils::env_etf$capm_stats[
  match(sym_bols[-1],
  rownames(rutils::env_etf$capm_stats)),
  "Beta"])
names(beta_s)[1] <- sym_bols[1]
# weights times betas
beta_s <- weight_s %*% beta_s
beta_s <- xts(beta_s,
  order.by=index(
    rutils::env_etf$re_turns[end_points[-1]]))
colnames(beta_s) <- "portf_beta"
x11()
plot.zoo(cbind(beta_s,
  rutils::env_etf$VTI[, 4])[index(beta_s)],
  main="betas & VTI", xlab="")
# create trading function
run_strat <- function(win_dow) {
  start_points <-  end_points[
    c(rep_len(1, win_dow-1),
1:(len_gth-win_dow+1))]
  inter_vals <- lapply(2:len_gth,
                 function(in_dex) {
                   start_points[in_dex]:end_points[in_dex]
                 })  # end lapply
  risk_stats <- lapply(inter_vals,
                 function(inter_val) {
                   x_ts <- rutils::env_etf$re_turns[inter_val, sym_bols]
                   t(sapply(x_ts,
                            function(col_umn)
                              c(return=mean(col_umn), risk=mad(col_umn))
                   ))  # end sapply
                 })  # end lapply
  weight_s <- sapply(risk_stats,
               function(risk_stat) {
                 weight_s <- risk_stat[, 1]/risk_stat[, 2]
                 weight_s <- weight_s - mean(weight_s)
                 weight_s <- weight_s/sum(abs(weight_s))
               })  # end sapply
  weight_s <- t(weight_s)
  pn_l <- rowSums(weight_s[-NROW(weight_s), ] * re_turns[-1, ])
  cost_s <- bid_offer*abs(diff(weight_s))
  cost_s <- rowSums(cost_s)
  pn_l <- pn_l - cost_s
  sum(pn_l)
}  # end run_strat
window_s <- 8*(1:7)
strat_profile <- sapply(window_s, run_strat)
plot(cbind(window_s, strat_profile), t="l",
     main="Strategy PnL as function of window size",
     xlab="window", ylab="pnl")
