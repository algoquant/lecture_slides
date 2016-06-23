#################################
### HW #2 due April 27, 2015
#################################
# Max score 45pts

# Please write in this file the R code needed to perform the tasks below, 
# rename it to your_name_hw2.R
# and send this file to Luping Liu (ll2525@nyu.edu)


##################################
# 1. (30pts) Calculate the maximum drawdown of a time series.

# load packages "lubridate", "xts", and "tseries",
library(xts)


# Create a "Date" vector of 100 daily dates, starting from "2015-01-04", and call it "in_dex", 
# use functions as.Date() and seq(),

### write your code here


# Extract the class from "in_dex" to verify that it is "Date" class, 

### write your code here


# Create a vector of data of length "in_dex" as follows:
da_ta <- sin(0.1*(1:length(in_dex))) + (1:length(in_dex))/50

# Create an "xts" time series with the "da_ta" and the "in_dex", and call it "xts_series",
# use function xts() from package "xts",

### write your code here


# plot "xts_series", using generic function plot(),

### write your code here


# The Cumulative maximum of a price series is the maximum price up to that point in time. 
# Plot the Cumulative maximum of "xts_series" using function cummax(),

### write your code here


# A drawdown is a drop in price from its previous maximum.
# Calculate the time series of drawdowns of "xts_series", as the difference 
# between the cumulative maximum of "xts_series" minus "xts_series", and call it "draw_down", 

### write your code here


# plot "draw_down",

### write your code here


# Find the date when "draw_down" reaches its maximum, and call it "date_trough", 
# and find the maximum value of "draw_down" on that date, and call it "max_draw_down", 
# use function which.max(),

### write your code here


# Subset "draw_down" to dates before "date_trough", and call it "draw_down_pre", 

### write your code here

# Subset "draw_down" to dates after "date_trough", and call it "draw_down_post", 

### write your code here


# When the current price exceeds the previous maximum, then "draw_down" is zero, 
# a drawdown starts when "draw_down" is first zero and then increases above zero.
# Find the latest date when "draw_down_pre" was still zero before "date_trough", and call it "date_from",

### write your code here


# A drawdown ends when "draw_down" drops back to zero after "date_trough".
# Find the first date when "draw_down_post" drops to zero after "date_trough", and call it "date_to",

### write your code here


# Combine the three dates into a named vector: from=date_from, trough=date_trough, to=date_to,
# and call it "dates_draw_down",

### write your code here



##################################
# 2. (5pts) plot "xts_series", using generic function plot(),
# read help: ?plot.xts and ?addEventLines

### write your code here

# add verical dashed red lines for the three dates: "date_from", "date_trough", "date_to",
# use function addEventLines() from package "xts",

### write your code here



##################################
# 3. (10pts) Create function "max_drawdown()" that calculates the maximum drawdown of a time series of prices.
# The function "max_drawdown()" should take one argument, a time series of prices (not returns),
# "max_drawdown()" should return a named vector of three dates: from=date_from, trough=date_trough, to=date_to,
# you can reuse the scripts from p.1,

### write your code here

# call max_drawdown() with the argument "xts_series", to verify it works correctly,

### write your code here


