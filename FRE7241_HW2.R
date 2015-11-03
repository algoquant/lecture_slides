#################################
### FRE7241 HW #2 due due Sep 29, 2015
#################################
# Max score 75pts

# Please write in this file the R code needed to perform the tasks below, 
# rename it to your_name_hw1.R
# and upload the file to NYU Classes


###############
# Summary: Calculate moving averages and crossing points with prices.

# 1. (10pts) 
# Download from Yahoo the "AdjClose" prices and "Volume" for 
# MSFT stock, starting from Jun/01/2007, and call it "zoo_msft",
# use tseries function get.hist.quote(),

library(tseries)  # load package tseries
library(zoo)  # load package zoo

# load MSFT data

### write your code here


# calculate the 50-day moving average of the "AdjClose" prices,
# merge the moving average with "zoo_msft" by adding it as the last column,
# rename the last column to "50DMA",
# you must use function rollmean(), with the proper "align" argument, 
# so that the averages are calculated using values from the past,
# remove rows with NAs using function na.omit(), 

### write your code here



# 2. (15pts) 
# plot "zoo_msft" columns "AdjClose" and "50DMA" in the same panel, 
# starting from "2015-01-01", in the colors "black" and "red", 
# you must use method plot.zoo() with the proper argument "plot.type",
# add a legend and position it so that it doesn't obscure the plot too much,

### write your code here


# calculate the vector of dates right after the "AdjClose" crosses the "50DMA", 
# and call it "cross_es". 
# First calculate a boolean vector that is TRUE for dates right 
# after a cross has just occurred, and FALSE otherwise. 
# Next apply this boolean vector to extract dates when 
# the "AdjClose" crosses the "50DMA". 
# you can use the logical operator "!=", 
# and functions sign(), diff(), and index(), 

### write your code here


# add grey vertical ablines to the plot above, at the dates of "cross_es",
# you must use function abline(), 

### write your code here



# 3. (20pts)
# Calculate the 50-day rolling maximum and minimum of the "AdjClose" prices,
# you must use function rollmax() with the proper "align" argument, so that 
# the aggregations are calculated using values from the past,
# calculate the difference between the rolling maximum and minimum, 
# and call it "ba_nd",

### write your code here


# calculate the rolling upper (lower) band as the 50-day moving average
# plus (minus) one half of "ba_nd",
# merge the rolling upper and lower bands with "zoo_msft" by adding 
# them as the last columns,
# rename the last columns to "Up_band" and "Low_band",
# remove rows with NAs using function na.omit(), 

### write your code here


# plot "zoo_msft" columns "AdjClose", "Up_band", and "Low_band" in the 
# same panel, starting from "2015-01-01",
# use method plot.zoo() with the proper argument "plot.type",
# add legend so that it doesn't obscure the plot,

### write your code here


# calculate the vector of dates right after the "AdjClose"
# crosses any of the two bands, and call it "cross_es". 
# First calculate a boolean vector that is TRUE for dates right 
# after a cross has just occurred, and FALSE otherwise. 
# Next apply this boolean vector to extract dates when 
# the "AdjClose" crosses either "Up_band" or "Low_band". 
# you can use the logical operator "!=", 

### write your code here


# add grey vertical ablines to the plot above, at the dates of "cross_es",
# you must use function abline(), 

### write your code here




###############
# Summary: Calculate the maximum drawdown of a time series.

# 1. (30pts) 
# download the file "zoo_data.Rdata" from NYU Classes, and load() it, 
# the file "zoo_data.Rdata" includes a zoo series called "zoo_stx", 
# containing MSFT stock OHLC data. 
# extract the "AdjClose" prices from "zoo_stx" into a variable 
# called "msft_prices".

### write your code here


# plot "msft_prices", using generic function plot(),

### write your code here


# The cumulative maximum of a price series is the maximum price up to 
# that point in time. 
# Plot the Cumulative maximum of "msft_prices" using function cummax(),

### write your code here


# A drawdown is a drop in price from its previous maximum.
# Calculate the zoo time series of drawdowns of "msft_prices", 
# as the difference between the cumulative maximum of "msft_prices" 
# minus "msft_prices", and call it "draw_down", 

### write your code here


# plot "draw_down",

### write your code here


# Find the date when "draw_down" reaches its maximum, and call it "date_trough", 
# and find the maximum value of "draw_down" on that date, and call it "max_drawdown", 
# you can use functions index() and which.max(),

### write your code here


# Subset "draw_down" to dates before "date_trough", and call it "pre_drawdown", 

### write your code here


# Subset "draw_down" to dates after "date_trough", and call it "post_drawdown", 

### write your code here


# When the current price exceeds the previous maximum, then "draw_down" is zero, 
# a drawdown starts when "draw_down" is first zero and then increases above zero.
# Find the latest date when "pre_drawdown" was still zero before "date_trough", 
# and call it "date_from",
# you can use functions index() and max(),

### write your code here


# A drawdown ends when "draw_down" drops back to zero after "date_trough".
# Find the first date when "post_drawdown" drops to zero after "date_trough", 
# and call it "date_to",
# you can use functions index() and min(),

### write your code here


# Combine the three dates into a named vector: 
# from=date_from, trough=date_trough, to=date_to,
# and call it "drawdown_dates",

### write your code here


# 2. (5pts) plot "msft_prices", using generic function plot(),

### write your code here


# add vertical green, red, and blue lines for the three dates: 
# "date_from", "date_trough", "date_to",

### write your code here

