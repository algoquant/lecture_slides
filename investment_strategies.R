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
library(PerformanceAnalytics)
load(file="C:/Develop/data/etf_data.RData")
ts_rets <- env_etf$re_turns[, "VTI"]
c(mean(ts_rets), sd(ts_rets))
utility <- function(frac, r=ts_rets) {
sapply(frac, function (fract) sum(log(1+fract*r)))
}  # end utility
curve(expr=utility, 
xlim=c(0.1, 2*KellyRatio(R=ts_rets, method="full")), 
xlab="kelly", ylab="utility", main="", lwd=2)
title(main="utility", line=-2)
par(mar=c(5, 2, 2, 2), mgp=c(1.5, 0.5, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
library(PerformanceAnalytics)
load(file="C:/Develop/data/etf_data.RData")
ts_rets <- env_etf$re_turns[, "VTI"]
KellyRatio(R=ts_rets, method="full")
library(HighFreq)  # load package HighFreq
# calculate open, close, and lagged prices
open_prices <- Op(env_etf$VTI)
price_s <- Cl(env_etf$VTI)
prices_lag <- rutils::lag_xts(price_s)
# define lookback window and calculate VWAP
win_dow <- 150
VTI_vwap <- HighFreq::roll_vwap(env_etf$VTI,
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
pos_ition <- NA*numeric(NROW(env_etf$VTI))
pos_ition[1] <- 0
pos_ition[trade_dates] <- vwap_indic[trade_dates]
pos_ition <- na.locf(pos_ition)
pos_ition <- xts(pos_ition, order.by=index((env_etf$VTI)))
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
pn_l <- xts(cumsum(pn_l), order.by=index((env_etf$VTI)))
chart_Series(x=(price_s-as.numeric(price_s[1, ])), name="VTI prices", col='orange')
add_TA(pn_l, on=1, lwd=2, col='blue')
add_TA(pos_ition > 0, on=-1,
 col="lightgreen", border="lightgreen")
add_TA(pos_ition < 0, on=-1,
 col="lightgrey", border="lightgrey")
legend("top", legend=c("VTI", "VWAP strategy"),
bg="white", lty=c(1, 1), lwd=c(2, 2),
col=c('orange', 'blue'), bty="n")
library(PerformanceAnalytics)
load(file="C:/Develop/data/etf_data.RData")
# rebalancing period
re_balance <- "weeks"
# look-back period in number of re_balance
win_dow <- 40

# create index of rebalancing period end points
end_points <- endpoints(env_etf$re_turns, 
                on=re_balance)
end_points[1] <- 1

# create index of rebalancing periods
periods <- lapply(win_dow:(length(end_points)-1),
    function(point)
list(
  back=end_points[point-win_dow+1]:(end_points[point]-1),
  fwd=end_points[point]:end_points[point+1])  # end list
    )  # end lapply
# calculate risk&ret stats for some symbols, over a range of dates
risk_ret_stats <- 
  function(x_ts=env_etf$re_turns,  # daily returns
     sym_bols=colnames(x_ts),  # names
     range=index(x_ts),  # date range
     ret="mean",  # return stat
     risk="mad") {  # risk stat
  ret <- match.fun(ret)  # match to function
  risk <- match.fun(risk)  # match to function
  stats <- sapply(x_ts[range, sym_bols], 
  function(ts)
    c(ret=ret(ts), risk=risk(ts))
  )  # end sapply
  t(stats)
}  # end risk_ret_stats

# example
head(risk_ret_stats(range=
        periods[[1]]$back))
# calculate stats over overlapping period date windows
period_stats <- lapply(periods,
   function(point)
     cbind(risk_ret_stats(range=point$back),
      fut_ret=sapply(env_etf$re_turns[point$fwd, ], sum))
)  # end lapply
head(period_stats[[1]])
# calculate pnl for a given period
pnl_period <-
  function(period_stat, de_mean=FALSE) {
  weights <- period_stat[, "ret"]/
    period_stat[, "risk"]
  weights <- weights - de_mean*mean(weights)
  weights <- weights/sum(abs(weights))
c(sum(period_stat[, "fut_ret"]*weights), weights)
}  # end pnl_period

# calculate pnls over all windows
pnl_xts <- t(sapply(period_stats, pnl_period))
pnl_xts <- xts(pnl_xts,
     order.by=index(env_etf$re_turns)
 [end_points[win_dow:(length(end_points)-1)]]
 )  # end xts
colnames(pnl_xts)[1] <- "pnl"

# calculate transaction costs
bid_offer <- 0.001  # 10 bps for liquid ETFs
co_sts <- bid_offer*abs(diff(pnl_xts[, -1]))
co_sts[1, ] <- 0
co_sts <- rowSums(co_sts)
pnl_xts[, "pnl"] <- pnl_xts[, "pnl"] - co_sts
# plot cumulative pnl of strategy
plot(cumsum(pnl_xts[, "pnl"]),
  main=colnames(pnl_xts[, "pnl"]))
portf_names <- c("VTI", "IEF", "DBC", "XLF", "VNQ", "XLP", "XLV", "XLU", "XLB", "XLE")
# plot portfolio weights
plot.zoo(pnl_xts[, portf_names], main="")
# calculate xts of net beta
betas <- c(1, etf_reg_stats[, 3])
names(betas)[1] <- colnames(pnl_xts[, 2])
# weights times betas
betas <- pnl_xts[, -1] * betas
betas <- xts(rowSums(betas),
    order.by=index(pnl_xts))
colnames(betas) <- "betas"
plot.zoo(cbind(betas,
    cumsum(env_etf$re_turns[, 1])[index(betas)]),
    main="betas & VTI", xlab="")
# create trading function
tot_pnl <- function(win_dow) {
  periods <- lapply(win_dow:(length(end_points)-1),
    function(point)
list(back=
end_points[point-win_dow+1]:(end_points[point]-1),
   fwd=end_points[point]:end_points[point+1])
  )  # end sapply
  period_stats <- lapply(periods,
 function(point)
   cbind(risk_ret_stats(range=point$back),
     fut_ret=sapply(env_etf$re_turns[point$fwd, ], sum))
  )  # end lapply
  pnl_xts <- t(sapply(period_stats, pnl_period))
  co_sts <- bid_offer*abs(diff(pnl_xts[, -1]))
  co_sts <- rowSums(co_sts)
  co_sts <- c(0, co_sts)
  pnl_xts[, 1] <- pnl_xts[, 1] - co_sts
  sum(pnl_xts[, 1])
}  # end tot_pnl
strat_profile <- sapply(4*(5:15), tot_pnl)
plot(cbind(4*(5:15), strat_profile), t="l")
