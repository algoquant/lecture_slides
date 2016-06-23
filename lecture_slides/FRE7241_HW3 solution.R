#################################
### FRE7241 Homework #3 Solution due Oct 20, 2015
#################################
# Max score 65pts

# The below solutions are examples,
# Slightly different solutions are also possible.


############## Part I
# 1. (20pts) create a function called run_sum() that calculates the 
# running sum of an xts series over a sliding window (lookback period). 
# run_sum() should accept two arguments: 
# "x_ts" - an xts series containing one or more columns of data, 
# "win_dow" - an integer specifying the number of lookback periods. 
# run_sum() should return an xts series with the same dimensions 
# as the input xts series. 
# For example, if win_dow=3, then the running sum at any point should 
# be equal to the sum of "x_ts" values for that point plus two preceding 
# points. 
# The initial values of run_sum() should be equal to cumsum() values, 
# so that run_sum() doesn't return any NA values.
# You must use use vectorized functions, such as cumsum() and lag(), 
# not loops, 
# you cannot use for() or apply() loops, 

run_sum <- function(x_ts, win_dow) {
  cum_sum <- cumsum(x_ts)
  out_put <- cum_sum - lag(x=cum_sum, k=win_dow)
  out_put[1:win_dow, ] <- cum_sum[1:win_dow, ]
  out_put
}  # end run_sum

# download the file "etf_data.Rdata" from NYU Classes, and load() it. 
# "etf_data.Rdata" contains an environment called "env_data", 
# with OHLC time series data for ETFs, including "VTI". 
# call run_sum() as follows, to verify it works correctly,

library(xts)
load(file="C:/Develop/data/etf_data.Rdata")
foo <- run_sum(x_ts=env_data$VTI[, "VTI.Volume"], win_dow=3)
head(foo)


############## Part II
# Summary: create a function that calculates the volume-weighted average price, 
# benchmark it to function VWAP() from package TTR, and create plots. 

# 1. (20pts) create a function called v_wap() that calculates the 
# volume-weighted average price over a sliding window (lookback period). 
# The volume-weighted average price (VWAP) over a period is defined as 
# the sum of the prices multiplied by trading volumes, divided by the 
# total trading volume in that period.
# v_wap() should accept two arguments:
# "x_ts" - an xts series containing OHLC prices and trading volumes, 
# "win_dow" - an integer specifying the number of lookback periods. 
# v_wap() should return an xts series with a single column 
# containing the VWAP of the adjusted close prices. 
# The output xts series should have the same number of rows as the 
# input xts series, 
# v_wap() should not return any NA values.
# You can use functions Ad(), Vo() and run_sum(), 

v_wap <- function(x_ts, win_dow) {
  v_wap <- run_sum(x_ts=Ad(x_ts)*Vo(x_ts), win_dow=win_dow)
  vol_ume <- run_sum(x_ts=Vo(x_ts), win_dow=win_dow)
  v_wap/vol_ume
}  # end v_wap

# download the file "etf_data.Rdata" from NYU Classes, and load() it. 
# "etf_data.Rdata" contains an environment called "env_data", 
# with OHLC time series data for ETFs, including "VTI". 

library(quantmod)
load(file="C:/Develop/data/etf_data.Rdata")

# 2. (5pts) calculate the VWAP of "VTI" using v_wap() 
# with "win_dow=11", to verify it works correctly. 
# Calculate the same VWAP using function VWAP() from package TTR, 
# and convert NA values to zero.
# Find which elements are significantly different between the 
# two results (absolute difference is greater than 0.01). 
# You can use functions abs(), is.na(), merge(), and "[]" subsetting, 

foo <- v_wap(x_ts=env_data$VTI, win_dow=11)
bar <- VWAP(price=Ad(env_data$VTI), volume=Vo(env_data$VTI), n = 11)
bar[is.na(bar)] <- 0
is_diff <- abs(foo-bar) > 0.01
merge(foo, bar)[is_diff]

# 3. (10pts) Benchmark the speed of v_wap() compared to 
# function VWAP() from package TTR. 
# Which function is faster? 
# Use function microbenchmark() from package microbenchmark. 
# You can adapt code from the "numerical_analysis" pdf and R files. 
# Be sure to download the most recent version.

library(microbenchmark)
microbenchmark(
  vwap_ttr=VWAP(price=Ad(env_data$VTI), volume=Vo(env_data$VTI), n = 11),
  vwap_custom=v_wap(x_ts=env_data$VTI, win_dow=11),
  times=10)

# 4. (10pts) Plot the adjusted close price of "VTI" for the year 2008 
# only, together with its VWAP, in a single panel on the same plot.
# You can use functions Ad(). 
# You must use either functions chartSeries() and addTA(), 
# or chart_Series() and add_TA(). 
# You can adapt code from the "time_series_univariate" pdf and R files. 
# Be sure to download the most recent version.

chartSeries(x=Ad(env_data$VTI)["2008"], 
            TA="addTA(foo, on=1)", 
            theme=chartTheme("white"), 
            name="VTI plus VWAP")
# or
chartSeries(x=Ad(env_data$VTI)["2008"],
            theme=chartTheme("white"),
            name="VTI plus VWAP")
addTA(foo["2008"], col="red", on=1)

chart_Series(x=Ad(env_data$VTI)["2008"], 
             TA="add_TA(foo, on=1)", 
             name="VTI plus VWAP")
# or
chart_Series(x=Ad(env_data$VTI)["2008"], 
             name="VTI plus VWAP")
add_TA(foo["2008"], col="blue", lwd=2, on=1)


