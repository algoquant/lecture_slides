#################################
### FRE7241 Homework #4 due May 10, 2016
#################################
# Max score 120pts

# Please write in this file the R code needed to perform the tasks below, 
# rename it to your_name_hw4.R
# and upload the file to NYU Classes

############## Part I
# Summary: Study the relationship between the
# standard deviation of returns and trading volumes
# using regression analysis.
# Calculate the rolling standard deviation of VTI
# returns, and the aggregated trading volumes,
# over monthly end points.
# Create plots and perform a regression of the two.

# Download the file etf_data.RData from NYU Classes,
# and load() it.
# etf_data.RData contains an environment called env_data,
# with OHLC time series data for ETFs, including VTI.

library(quantmod)
load(file="C:/Develop/data/etf_data.RData")


# 1. (20pts) Calculate the VTI daily returns from
# the adjusted close prices.
# You can use functions Ad() and dailyReturn(),

### write your code here

# Calculate the monthly end points for re_turns.
# You must use function endpoints().

### write your code here

# Calculate the standard deviation of returns over
# monthly end points, and call it vol_at.
# You can use functions period.apply() and sd().

### write your code here

# Calculate the aggregated trading volumes over
# monthly end points, and call it vol_ume.
# You can use functions period.sum() and Vo().

### write your code here

# Merge vol_at and vol_ume together and call it
# volat_volume. Assign to volat_volume the column
# names "volat" and "volu".
# Remove rows of volat_volume containing NA values.
# You can use functions merge(), colnames(), and
# complete.cases().

### write your code here


# 2. (10pts) Plot the columns of volat_volume in a single
# plot in two panels.
# You can use functions x11(), par(), and chart_Series().

### write your code here

# Plot a scatterplot of the two columns of volat_volume.
# You can use function plot() with the "formula" and
# "data" arguments.
# You must create the formula from the column names of
# volat_volume.
# You can use functions x11(), colnames(), as.formula(),
# and paste().


### write your code here


# 3. (10pts) Perform a regression of the two columns of
# volat_volume,
# You must create a formula from the column names of
# volat_volume.
# Extract from summary() the regression statistics for
# the slope coefficient: t-value, p-value, adj.r.squared,
# and create a named vector with these statistics.
# You can use functions colnames(), as.formula(), lm(),
# and summary(),

### write your code here


# 4. (10pts) Perform the Durbin-Watson test for the
# autocorrelations of residuals.
# Plot the residuals, using function plot().
# Can the null hypothesis be rejected in this case?
# Use function dwtest(), from package lmtest.


### write your code here


# 5. (10pts) Calculate the month-over-month differences
# of volat_volume, and call it volat_vol_diff.
# Use function diff() and na.omit().

### write your code here

# Plot a scatterplot of volat_vol_diff, and repeat the
# whole regression analysis from p.3 and p.4 above for
# volat_vol_diff.

### write your code here



############## Part II
# Summary: Calculate the volume-weighted average price,
# find its crossing points with prices, and plot it.

# Download the file etf_data.RData from NYU Classes,
# and load() it.
# etf_data.RData contains an environment called env_data,
# with OHLC time series data for ETFs, including VTI.

library(quantmod)
load(file="C:/Develop/data/etf_data.RData")


# 1. (10pts) Extract the adjusted close prices from
# VTI into a variable called price_s.
# You can use function Ad() from package quantmod.

### write your code here

# Rename the column name of price_s to "VTI", by 
# dropping ".Adjusted" from the colnames. 
# Use must function strsplit(). 

### write your code here

# Calculate the 50-day moving average of price_s,
# and merge it with price_s by adding it as the
# last column.
# Rename the last column to "VWAP".
# You must use function VWAP() from package TTR.

### write your code here

# At this point price_s should be like this:
# tail(price_s)
#               VTI     VWAP
# 2016-04-08 104.20  99.28782
# 2016-04-11 103.91  99.45781
# 2016-04-12 104.92  99.58764
# 2016-04-13 106.15  99.74678
# 2016-04-14 106.11  99.89194
# 2016-04-15 106.06 100.07063

# Calculate a boolean xts series that is TRUE for 
# dates when VTI prices are above their VWAP, and 
# FALSE otherwise, and call it ma_indic. 

### write your code here


# 2. (10pts) Plot both columns of price_s in the 
# same panel, from "2015-05-01" to "2015-08-01", 
# with custom line colors "black" and "red".
# You must use functions chart_theme(), chart_Series(),
# and you can also use add_TA().

### write your code here

# Add background shading, with "lightgreen" color 
# for dates when the prices are above their VWAP, 
# and "lightgrey" when they're below their VWAP.  
# Add a legend.
# You must use functions add_TA() and legend().

### write your code here

# add legend

### write your code here


# 3. (10pts) Calculate an xts series of dates when 
# the prices cross their VWAP, and call it ma_crosses.
# hint: You can use ma_indic to calculate ma_crosses.
# You can use the functions diff(), abs(), index(),
# and the logical operator ">".

### write your code here

# You should get the following output:
# head(ma_crosses)
# [1] "2007-03-21" "2007-06-25" "2007-06-27" "2007-06-29" "2007-07-02" "2007-07-24"
# tail(ma_crosses)
# [1] "2015-12-28" "2015-12-29" "2015-12-31" "2016-02-22" "2016-02-23" "2016-02-25"


# 3. (20pts) Calculate the 50-day rolling maximum 
# and minimum of the prices (price_s[, 1]).
# You must use the function runMax() from package TTR. 

### write your code here

# Calculate the difference between the rolling 
# maximum and minimum and call it ba_nd. 

### write your code here

# Calculate the rolling upper (lower) band as the 
# 50-day moving average ("VWAP") plus (minus) 
# one half of ba_nd. 
# Merge the rolling upper and lower bands with 
# price_s by adding them as the last columns. 
# Rename the last columns to "up_band" and "low_band". 

### write your code here

# At this point price_s should be like this:
# tail(price_s)
#               VTI      VWAP  up_band low_band
# 2016-04-08 104.20  99.28782 105.9633 92.61229
# 2016-04-11 103.91  99.45781 106.1333 92.78228
# 2016-04-12 104.92  99.58764 106.2632 92.91211
# 2016-04-13 106.15  99.74678 106.7573 92.73625
# 2016-04-14 106.11  99.89194 106.9025 92.88141
# 2016-04-15 106.06 100.07063 107.0812 93.06010

# Calculate two boolean xts series called up_indic 
# and low_indic. 
# up_indic is TRUE for dates when VTI prices are 
# above upper_band, while low_indic is TRUE for 
# dates when VTI prices are below lower_band. 
# They are FALSE otherwise. 

### write your code here


# 4. (10pts) Plot all four columns of price_s 
# in the same panel, from "2015-01-01" onward, 
# with custom line colors "black", "blue", 
# "red", and "green". 
# You must use functions chart_theme() and
# chart_Series(), and you can also use add_TA().

### write your code here

# Add background shading, with "lightgreen" color 
# for dates when up_indic is TRUE, and "coral" 
# when low_indic is TRUE. 
# Add a legend for "VTI" and "VWAP", in the 
# colors "black" and "blue". 
# You must use functions add_TA() and legend().

### write your code here

