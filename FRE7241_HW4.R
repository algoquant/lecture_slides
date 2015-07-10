#################################
### FRE7241 HW #4 due July 6, 2015
#################################
# Max score 45pts


##################################
# calculate the rolling standard deviation of returns, 
# and aggregate the volume, over monthly end points, 
# 
# 1. (5pts) Load time series data from file 
# "etf_rets_volume.Rdata" (download it from NYU Classes),
# containing "xts" series with stock return and volume data,
# create a new environment called "env_returns", 
# load data from the file "etf_rets_volume.Rdata" into "env_returns",
# use functions new.env() and load(), with the "envir" argument,

### write your code here


# the environment "env_returns" should contain a number of "xts" series, 
# each containing stock daily return and volume data for a single symbol, 
# you can assume that all the "xts" series have the same date index,
# create a vector of monthly end points for any of the "xts" series 
# in "env_returns",
# called "end_points", and set the first "end_points" equal to 1,
# use function endpoints() from package xts,
library(xts)

### write your code here


# 2. (20pts) create a function called agg_volat_volume(), 
# that accepts three arguments:
#   "x_ts": an "xts" containing returns and volume data, 
#   "end_points": a vector of end points, 
#   "envir": an environment argument, 
# agg_volat_volume() should:
# - extract returns and volume data from "x_ts",
# - extract the symbol name from the columns of "x_ts" ("symbol_name"),
# - calculate the volatility from the returns, 
#    over non-overlapping periods given by "end_points", 
# - calculate the total volume, 
#    over non-overlapping periods given by "end_points", 
# - cbind volatility with volume data into a single "xts" ("volat_volume"),
# - rename the colnames of "volat_volume" to "symbol_name.Volat" 
#    and "symbol_name.Volume", (replace "symbol_name" with the symbol name),
# - assign (copy) "volat_volume" to an object named "symbol_name" 
#   in the "envir" environment, 
# agg_volat_volume() should produce the side effect of creating 
# an "xts" object in the "envir" environment, that contains volatility 
# and volume data calculated from the input "x_ts",
# agg_volat_volume() should return invisible the "symbol_name",
# you can use functions strsplit(), colnames(), period.apply(), 
# period.sum(), cbind(), paste() (or paste0), xts(), 
# assign(), invisible(),

### write your code here


# 2. (10pts) create a new environment called "env_volat", 
# for storing "xts" containing stock return and volume data,
# use function new.env(),

### write your code here

# apply function agg_volat_volume() to a "VTI_rets", to verify 
# it works correctly:

### write your code here


# plot both columns of "env_volat$VTI", in a plot with two panels, 
# you can use function plot.zoo(), or plot.xts() and par(),

### write your code here


# plot a scatterplot of both columns of "env_volat$VTI", 
# you can use function plot() with "data" argument,

### write your code here


# calculate the month-over-month difference of both columns
# of "env_volat$VTI", 
# use function diff(), 
# plot a scatterplot of both the diff columns, 

### write your code here



##################################
# 3. (10pts) perform a regression of volume versus volatility, 
# of "env_volat$VTI", 
# extract from summary() the regression statistics: 
#  p-value, adj.r.squared, fstatistic,
# create a named vector of the regression statistics, 

### write your code here


# perform the Durbin-Watson test for the autocorrelations of 
# regression residuals,
# write what is the null hypothesis?
# can the null hypothesis be rejected?
# use function dwtest(), from package lmtest,
library(lmtest)  # load lmtest

### write your code here


# repeat the whole regression analysis from above for the 
# month-over-month difference of both columns of "env_volat$VTI", 

### write your code here

# perform the Durbin-Watson test for the autocorrelations of 
# regression residuals,
# use function dwtest(), from package lmtest,

### write your code here

