#################################
### FRE7241 Homework #3 due May 3, 2016
#################################
# Max score 90pts

# Please write in this file the R code needed to perform the tasks below, 
# rename it to your_name_hw3.R
# and upload the file to NYU Classes

############## Part I
# Summary: Create a function which calculates the volume-
# weighted average price, benchmark it to function VWAP() 
# from package TTR, and create plots. 

# 1. (20pts) Create a function called v_wap() which 
# calculates the volume-weighted average price over a 
# rolling lookback window. 
# The volume-weighted average price (VWAP) over a time 
# interval is defined as the sum of the prices multiplied 
# by the trading volumes, divided by the total trading 
# volume in that interval.
# The function v_wap() should accept two arguments: 
#  - x_ts - an xts series containing OHLC prices and 
#    trading volumes for a single asset, 
#  - win_dow - an integer specifying the number of 
#    periods in the lookback window. 
# v_wap() should return an xts series with a single 
# column containing the VWAP of the adjusted close prices. 
# The output xts series should have the same number of 
# rows as the input xts series, 
# v_wap() should not return any NA values.
# v_wap() should be set to zero if the total volume 
# is zero. 
# You can use functions Ad(), Vo() and roll_sum(). 
# You can also use functions paste0(), strsplit(), 
# and colnames(), to change the column name. 
# The function roll_sum() was defined in the lecture 
# slides. 

library(quantmod)

### write your code here


# Download the file etf_data.RData from NYU Classes, and load() it. 
# etf_data.RData contains an environment called env_data, 
# with OHLC time series data for ETFs, including "VTI". 

load(file="C:/Develop/data/etf_data.RData")

# calculate the VWAP of "VTI" using v_wap() 
# with "win_dow=11", to verify it works correctly. 

### write your code here

# You should get the following output, including 
# the column name:
#            VTI.vwap
# 2007-01-03 58.17359
# 2007-01-04 58.26733
# 2007-01-05 58.22996
# 2007-01-08 58.21813
# 2007-01-09 58.20874
# 2007-01-10 58.20613


# 2. (10pts) Calculate the same VWAP using function VWAP() 
# from package TTR, and convert any NA values to zero.
# Find which elements are significantly different between 
# the two results (absolute difference is greater than 0.01). 
# You can use functions abs(), is.na(), merge(), and "[]" 
# subsetting, 

library(TTR)

### write your code here


# You should get the following output: 
# 
#            VTI.vwap VWAP
# 2007-01-03 58.17359    0
# 2007-01-04 58.26733    0
# 2007-01-05 58.22996    0
# 2007-01-08 58.21813    0
# 2007-01-09 58.20874    0
# 2007-01-10 58.20613    0
# 2007-01-11 58.22977    0
# 2007-01-12 58.27334    0
# 2007-01-16 58.31010    0
# 2007-01-17 58.34027    0


# 3. (10pts) Benchmark the speed of v_wap() compared to 
# function VWAP() from package TTR. 
# Which function is faster? 
# Use function microbenchmark() from package microbenchmark. 
# You can adapt code from the lecture slides. 

library(microbenchmark)

### write your code here


# 4. (10pts) Plot the adjusted close price of "VTI" for the year 2008 
# only, together with its VWAP, in a single panel on the same plot.
# You must use functions Ad(), chart_Series() and add_TA(). 


### write your code here



############## Part II
# Summary: Calculate the maximum drawdown of a time series.

# Download the file etf_data.RData from NYU Classes, 
# and load() it. 
# etf_data.RData contains an environment called env_data, 
# with OHLC time series data for ETFs, including "VTI". 

load(file="C:/Develop/data/etf_data.RData")

# 1. (10pts) Extract the adjusted close prices from "VTI" 
# into a variable called price_s. 
# You can use function Ad() from package quantmod. 

### write your code here


# The cumulative maximum of a price series is the maximum 
# price in the past, reached up to that point in time. 
# Calculate the cumulative maximum of price_s using 
# function cummax(). 
# Plot the cumulative maximum of price_s using function 
# chart_Series(). 

### write your code here


# A drawdown is a drop in price from its previous maximum.
# Calculate the xts time series of drawdowns of price_s, 
# as the difference between price_s minus the cumulative 
# maximum of price_s, and call it draw_down. 

### write your code here


# plot draw_down using function chart_Series().

### write your code here


# Find the date when draw_down reaches its minimum, and 
# call it date_trough, and find the minimum value of 
# draw_down on that date, and call it max_drawdown. 
# You can use functions index() and which.min(). 

### write your code here


# Add a vertical red line to the draw_down plot, 
# at the date date_trough. 
# hint: use function match() and index() to first 
# calculate the index of date_trough. 
# You can use functions match(), index(), and abline(), 

### write your code here


# 2. (10pts) Divide draw_down into two time series at 
# the date date_trough. 
# First subset draw_down to dates before date_trough, 
# and call it pre_drawdown, 

### write your code here


# Next subset draw_down to dates after date_trough, 
# and call it post_drawdown, 

### write your code here


# Now find the date when the drawdown period starts. 
# The drawdown starts when draw_down is first zero 
# and then starts decreasing to some price below zero. 
# Find the latest date when pre_drawdown was still 
# equal to zero, and call it date_from. 
# date_from is when the drawdown started.
# You can use functions index() and max(). 

### write your code here


# Now find the date when the drawdown period ends. 
# When the current price exceeds the previous maximum 
# price, then draw_down returns back to zero, and the 
# drawdown period is over. 
# A drawdown ends when draw_down first returns back 
# to zero after date_trough.
# Find the first date when post_drawdown returns back 
# to zero, and call it date_to. 
# date_to is when the drawdown has ended. 
# You can use functions index() and min(),

### write your code here


# 3. (10pts) Combine the three dates: date_from, 
# date_trough, and date_to into a named vector with 
# names "from", "trough", and "to", and call it 
# drawdown_dates, 

### write your code here


# You should get the following output:
# drawdown_dates
#         from       trough           to 
# "2007-10-09" "2009-03-09" "2012-03-13" 

# Plot price_s using function chart_Series().

### write your code here


# Add vertical green, red, and blue lines for the 
# three dates: date_from, date_trough, date_to. 
# Add text at the vertical lines equal to 
# names(drawdown_dates). 
# hint: use function match() and index() to first 
# calculate the index of drawdown_dates. 
# You can use functions match(), index(), abline(), 
# and text(). 

### write your code here


# 4. (10pts) Create a function called max_drawdown() which 
# calculates the maximum drawdown of a time series of prices.
# max_drawdown() should accept a single argument called x_ts, 
# which should be an xts time series of prices (not returns). 
# max_drawdown() should return a named vector of three dates: 
# date_from, date_trough, and date_to, with names "from", 
# "trough", and "to". 
# hint: you can reuse the scripts from the previous parts.

### write your code here


# call max_drawdown() with the argument price_s, to verify 
# it works correctly. 

### write your code here

# You should get the following output:
#         from       trough           to 
# "2007-10-09" "2009-03-09" "2012-03-13"

# Calculate the drawdowns of price_s using function 
# table.Drawdowns() from package PerformanceAnalytics. 
# and compare the output to max_drawdown(). 
# hint: use function dailyReturn() to first calculate 
# returns from price_s. 

library(PerformanceAnalytics)

### write your code here

