#################################
### FRE7241 HW #4 due May 12, 2015
#################################
# Max score 40 pts

# Please write in this file the R code needed to perform the tasks below, 
# rename it to your_name_hw4.R
# and send this file to Luping Liu (ll2525@nyu.edu)


##################################
# 1. (20pts) Load time series data and calculate rolling range statistics,
# the file "etf_series.Rdata" contains time series data,
# create a new environment called "data_env",
# load data from the file "etf_series.Rdata" into "data_env",
# use function load(), with the "envir" argument,

### write your code here


# extract the adjusted prices and volume for symbol "VTI" from "data_env", and call it "VTI",
# "VTI" will now be defined both in the default workspace and in "data_env",
# use function merge(), 

### write your code here


# calculate rolling range statistics over a sliding window, called "win_dow",

win_dow <- 22


# calculate two "xts" time series of trailing maximum and minimum values 
# of adjusted prices over the sliding "win_dow", and call them "roll_max" and "roll_min",
# at every point in time, the value of "roll_max" should be equal to the maximum 
# adjusted price from points in the past covered by "win_dow",
# use function rollmax() form package "zoo", with the proper "k" and "align" arguments,

library(zoo)

### write your code here

colnames(roll_max) <- "max"

### write your code here

colnames(roll_min) <- "min"


# calculate the difference between "roll_max" and "roll_min", and call it "ra_nge",

### write your code here

colnames(ra_nge) <- "range"


# calculate an "xts" time series of trailing mean values of the volume 
# over the sliding "win_dow", and call it "roll_volume",
# use function rollmean(), with the proper "k" and "align" arguments,

### write your code here

colnames(roll_volume) <- "volume"


# merge "ra_nge" with "roll_volume" into a single "xts" time series,
# and call it "range_volume", 
# remove rows with NAs,
# use functions merge() and na.omit(),

### write your code here


# create a time series plot of both columns of "range_volume" in two panels, 
# use function plot.zoo(),

### write your code here


# create a scatterplot of "range_volume", 
# use function plot(),

### write your code here



##################################
# 2. (20pts) perform a regression of "ra_nge" vs "roll_volume"
# extract from summary() the regression statistics: p-value, adj.r.squared, fstatistic,
# create a named vector of the regression statistics, 
# use function plot(),

### write your code here


# perform Durbin-Watson test for the autocorrelations of regression residuals,
# write what is the null hypothesis?
# can the null hypothesis be rejected?
# use function dwtest(), form package lmtest,

library(lmtest)  # load lmtest
### write your code here


# perform the same regression on a subset of the data from 2010 and afterwards,

### write your code here


