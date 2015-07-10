library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
thm <- knit_theme$get("acid")
knit_theme$set(thm)
library(PerformanceAnalytics)
load(file="C:/Develop/data/etf_data.Rdata")
# rebalancing period
re_balance <- "weeks"
# look-back period in number of re_balance
win_dow <- 40

# create index of rebalancing period end points
end_points <- endpoints(etf_rets, 
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
risk_ret_stats <- function(x_ts=etf_rets,  # daily returns
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
# calculate stats over overlaping period date windows
period_stats <- lapply(periods,
   function(point)
     cbind(risk_ret_stats(range=point$back),
      fut_ret=sapply(etf_rets[point$fwd, ], sum))
)  # end lapply
head(period_stats[[1]])
# calculate pnl for a given period
pnl_period <- function(period_stat, de_mean=FALSE) {
  weights <- period_stat[, "ret"]/
    period_stat[, "risk"]
  weights <- weights - de_mean*mean(weights)
  weights <- weights/sum(abs(weights))
c(sum(period_stat[, "fut_ret"]*weights), weights)
}  # end pnl_period

# calculate pnls over all windows
pnl_xts <- t(sapply(period_stats, pnl_period))
pnl_xts <- xts(pnl_xts,
     order.by=index(etf_rets)
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
    cumsum(etf_rets[, 1])[index(betas)]),
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
     fut_ret=sapply(etf_rets[point$fwd, ], sum))
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
