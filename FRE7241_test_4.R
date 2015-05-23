#################################
### FRE7241 Test #4 05/19/15
#################################
# Max score 50pts

# Please write in this file the R code needed to perform the tasks below, 
# rename the file to your_name_test4.R
# and send the file to Luping Liu (ll2525@nyu.edu)


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

### write your code here


# plot "cum_rets" using generic function plot(), and add a legend,

### write your code here



##################################
# 2. (10pts) Calculate the risk/return statistics of "etf_rets[, "VTI"]" 
# and "pnl_xts", 
# be careful to perform the calculations using the same date index,
# since the indices of "etf_rets" and "pnl_xts" have different frequencies,
# hint: use "cum_rets" as a starting point,
# use functions diff(), SharpeRatio(), SortinoRatio(), and CalmarRatio(),  

### write your code here



##################################
# 3. (20pts) Modify the function "pnl_period" so that the weights 
# are proportional to the inverse of "period_stat[, "risk"]",
# and scaled by the sum of absolute values,
# recalculate the returns called "pnl_xts",
# perform points #1 and #2 above, using the new "pnl_xts", 
# paste all of your code in this file, so it can be run in sequence,

### write your code here
