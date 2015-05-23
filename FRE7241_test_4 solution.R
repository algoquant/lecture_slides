#################################
### FRE7241 Test #4 Solutions 05/19/15
#################################
# Max score 50pts

# The below solutions are examples,
# Slightly different solutions are also possible.


##################################
# 1. (20pts) Load time series data and calculate the returns from 
# backtesting the active investment strategy, called "pnl_xts",
# use the code from lecture #6, 

load(file="C:/Develop/data/etf_rets.Rdata")
library(xts)

### run the active investment strategy code from lecture #6,


# calculate the cumulative returns of the strategy "pnl_xts", 
# and the cumulative returns of "etf_rets[, "VTI"]",
# merge the VTI returns with strategy returns, and call it "cum_rets",
# the result of merge should be an xts with two columns,
# be careful about the order of cumsum and merge operations,
# since the indices of "etf_rets" and "pnl_xts" have different frequencies,
# be sure to omit NAs, and to start the cumulative VTI returns from zero,
# use functions cumsum(), merge() and na.omit(),

vti_rets <- cumsum(etf_rets[, "VTI"])[index(pnl_xts)]
vti_rets <- vti_rets - as.numeric(vti_rets[1, ])
cum_rets <- na.omit(merge(cumsum(pnl_xts[, "pnl"]), vti_rets))
head(cum_rets)


# plot "cum_rets" using generic function plot(), and add a legend,

plot(cum_rets, main="Min vol vs VTI")

legend(x="bottomright", legend=c("Min vol strategy", "VTI"),
       inset=0.2, cex=0.8, bg="white",
       lwd=2, lty=c(1, 1), col=c("black", "red"))



##################################
# 2. (10pts) Calculate the risk/return statistics of "etf_rets[, "VTI"]" 
# and "pnl_xts", 
# be careful to perform the calculations using the same date index,
# since the indices of "etf_rets" and "pnl_xts" have different frequencies,
# hint: use "cum_rets" as a starting point,
# use functions diff(), SharpeRatio(), SortinoRatio(), and CalmarRatio(),  

daily_rets <- diff(cum_rets)[-1]
library(PerformanceAnalytics)
SharpeRatio(daily_rets)
SortinoRatio(daily_rets)
CalmarRatio(daily_rets)



##################################
# 3. (20pts) Modify the function "pnl_period" so that the weights 
# are proportional to the inverse of "period_stat[, "risk"]",
# and scaled by the sum of absolute values,
# recalculate the returns called "pnl_xts",
# perform points #1 and #2 above, using the new "pnl_xts", 
# paste all of your code in this file, so it can be run in sequence,

pnl_period <- function(period_stat, de_mean=FALSE) {
  weights <- 1/period_stat[, "risk"]
  weights <- weights - de_mean*mean(weights)
  weights <- weights/sum(abs(weights))
  c(sum(period_stat[, "fut_ret"]*weights), weights)
}  # end pnl_period

