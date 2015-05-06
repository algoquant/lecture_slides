

library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=600, dev='pdf')
thm <- knit_theme$get("acid")
knit_theme$set(thm)



## library(factorAnalytics)  # load package "factorAnalytics"
## # get documentation for package "factorAnalytics"
## packageDescription("factorAnalytics")  # get short description
## help(package="factorAnalytics")  # load help page



options(width=50)
library(factorAnalytics)  # load package "factorAnalytics"
# list all objects in "factorAnalytics"
ls("package:factorAnalytics")

# list all datasets in "factorAnalytics"
# data(package="factorAnalytics")

# remove factorAnalytics from search path
detach("package:factorAnalytics")



library(factorAnalytics)
# load ETF returns
load(file="C:/Develop/data/etf_data.Rdata")
# fit a three-factor model using PCA
factor_pca <- fitSfm(etf_rets, k=3)
head(factor_pca$loadings, 3)  # factor loadings
# factor realizations (time series)
head(factor_pca$factors)
# residuals from regression
factor_pca$residuals[1:3, 1:3]



library(factorAnalytics)
factor_pca$alpha  # estimated alphas
factor_pca$r2  # R-squared regression
# covariance matrix estimated by factor model
factor_pca$Omega[1:3, 4:6]



## library(factorAnalytics)
## load(file="C:/Develop/data/portf_optim.RData")
## plot(factor_pca, which.plot.group=3,
##      n.max=30, loop=FALSE)
## # ?plot.sfm



## library(PortfolioAnalytics)
## # plot factor cumulative returns
## chart.CumReturns(factor_pca$factors,
##     lwd=2, ylab="", legend.loc="topleft",
##     main="")
## 
## # plot time series of factor returns
## # plot(factor_pca, which.plot.group=2,
## #   loop=FALSE)



## # asset correlations "hclust" hierarchical clustering order
## plot(factor_pca, which.plot.group=7,
##      loop=FALSE, order="hclust",
##      method="ellipse")



## library(PortfolioAnalytics)
## # plot residual cumulative returns
## chart.CumReturns(
##   factor_pca$residuals[, c("IEF",
##             "DBC", "XLF")],
##   lwd=2, ylab="", legend.loc="topleft",
##   main="")



## library(PortfolioAnalytics)
## # plot residual histogram with normal curve
## plot(factor_pca, asset.name="VTI",
##      which.plot.single=8,
##      plot.single=TRUE, loop=FALSE,
##      xlim=c(-0.007, 0.007))



library(PortfolioAnalytics)
# residual Q-Q plot
plot(factor_pca, asset.name="VTI", 
     which.plot.single=9, 
     plot.single=TRUE, loop=FALSE)



## # SACF and PACF of residuals
## plot(factor_pca, asset.name="VTI",
##      which.plot.single=5,
##      plot.single=TRUE, loop=FALSE)



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
list(back=end_points[point-win_dow+1]:(end_points[point]-1),
  fwd=end_points[point]:end_points[point+1])
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



## # calculate pnl for a given period
## pnl_period <- function(period_stat, de_mean=FALSE) {
##   weights <- period_stat[, "ret"]/
##     period_stat[, "risk"]
##   weights <- weights - de_mean*mean(weights)
##   weights <- weights/sum(abs(weights))
## c(sum(period_stat[, "fut_ret"]*weights), weights)
## }  # end pnl_period
## 
## # calculate pnls over all windows
## pnl_xts <- t(sapply(period_stats, pnl_period))
## pnl_xts <- xts(pnl_xts,
##      order.by=index(etf_rets)
##  [end_points[win_dow:(length(end_points)-1)]]
##  )  # end xts
## colnames(pnl_xts)[1] <- "pnl"
## 
## # calculate transaction costs
## bid_offer <- 0.001  # 10 bps for liquid ETFs
## co_sts <- bid_offer*abs(diff(pnl_xts[, -1]))
## co_sts[1, ] <- 0
## co_sts <- rowSums(co_sts)
## pnl_xts[, "pnl"] <- pnl_xts[, "pnl"] - co_sts



## # plot cumulative pnl of strategy
## plot(cumsum(pnl_xts[, "pnl"]),
##   main=colnames(pnl_xts[, "pnl"]))



## # plot portfolio weights
## plot.zoo(pnl_xts[, portf_names], main="")



## # calculate xts of net beta
## betas <- c(1, etf_reg_stats[, 3])
## names(betas)[1] <- colnames(pnl_xts[, 2])
## # weights times betas
## betas <- pnl_xts[, -1] * betas
## betas <- xts(rowSums(betas),
##     order.by=index(pnl_xts))
## colnames(betas) <- "betas"
## plot.zoo(cbind(betas,
##     cumsum(etf_rets[, 1])[index(betas)]),
##     main="betas & VTI", xlab="")



## # create trading function
## tot_pnl <- function(win_dow) {
##   periods <- lapply(win_dow:(length(end_points)-1),
##     function(point)
## list(back=
## end_points[point-win_dow+1]:(end_points[point]-1),
##    fwd=end_points[point]:end_points[point+1])
##   )  # end sapply
##   period_stats <- lapply(periods,
##  function(point)
##    cbind(risk_ret_stats(range=point$back),
##      fut_ret=sapply(etf_rets[point$fwd, ], sum))
##   )  # end lapply
##   pnl_xts <- t(sapply(period_stats, pnl_period))
##   co_sts <- bid_offer*abs(diff(pnl_xts[, -1]))
##   co_sts <- rowSums(co_sts)
##   co_sts <- c(0, co_sts)
##   pnl_xts[, 1] <- pnl_xts[, 1] - co_sts
##   sum(pnl_xts[, 1])
## }  # end tot_pnl
## strat_profile <- sapply(4*(5:15), tot_pnl)
## plot(cbind(4*(5:15), strat_profile), t="l")


