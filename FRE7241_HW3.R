#################################
### FRE7241 Homework #3 due Oct 20, 2015
#################################
# Max score 65pts

# Please write in this file the R code needed to perform the tasks below, 
# rename it to your_name_hw3.R
# and upload the file to NYU Classes


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

### write your code here

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

### write your code here

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

### write your code here

# 3. (10pts) Benchmark the speed of v_wap() compared to 
# function VWAP() from package TTR. 
# Which function is faster? 
# Use function microbenchmark() from package microbenchmark. 
# You can adapt code from the "numerical_analysis" pdf and R files. 
# Be sure to download the most recent version.

library(microbenchmark)

### write your code here

# 4. (10pts) Plot the adjusted close price of "VTI" for the year 2008 
# only, together with its VWAP, in a single panel on the same plot.
# You can use functions Ad(). 
# You must use either functions chartSeries() and addTA(), 
# or chart_Series() and add_TA(). 
# You can adapt code from the "time_series_univariate" pdf and R files. 
# Be sure to download the most recent version.

### write your code here
