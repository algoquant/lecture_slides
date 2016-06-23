#################################
### FRE7241 Homework #2 due April 26, 2016
#################################
# Max score 90pts

# Please write in this file the R code needed to perform the tasks below, 
# rename it to your_name_hw2.R
# and upload the file to NYU Classes


############## Part I
# Summary: Create a function that extracts the price and 
# volume columns from OHLC data, and perform a single 
# lapply() loop to extract the price and volume columns 
# from all time series contained in env_data. 

# Download the file etf_data.RData from NYU Classes, 
# and load() it. 
# etf_data.RData contains an environment called env_data, 
# with OHLC time series data for ETFs, including "VTI". 

library(quantmod)
load(file="C:/Develop/data/etf_data.RData")

# 1. (10pts) Create a function called ex_tract(), that 
# extracts the adjusted price and volume columns from an 
# OHLC data series, and returns an xts series with two 
# columns. 
# You can use functions merge(), Ad(), and Vo(), 

### write your code here

# Apply function ex_tract() to a single xts series, to verify 
# it works correctly:

foo <- ex_tract(env_data$VTI)
head(foo)


# 2. (20pts) Create a vector of symbols, called some_symbols, 

some_symbols <- c("DBC", "VTI", "IEF")

# Perform an lapply() loop over a subset of env_data 
# containing some_symbols, and call the function ex_tract() 
# on each element in the subset, and call the output da_ta. 
# da_ta should be a list of xts series, with each xts series 
# containing price and volume data for a single symbol. 
# You can use functions as.list(), get(), ex_tract(), and lapply(). 
# There are at least two different ways of performing this, 
# and either way is good.

### write your code here

# Flatten da_ta into a single xts series, and call it da_ta. 
# You can use functions do.call() and merge(), 

### write your code here



############## Part II
# Summary: Create a function which calculates a vector 
# of hypothesis test statistics. 
# Perform an sapply() loop to calculate a matrix of 
# statistics for a vector of symbols. 

# Download the file etf_data.RData from NYU Classes, 
# and load() it. 
# etf_data.RData contains an environment called env_data, 
# with OHLC time series data for ETFs. 

library(quantmod)
load(file="C:/Develop/data/etf_data.RData")


# 1. (20pts) Create a function called get_hyp_stats(), 
# that returns a vector with hypothesis test statistics.
# The function get_hyp_stats() should accept a single 
# argument called re_turns, an xts series containing 
# returns data. 
# 
# The function get_hyp_stats() should perform the 
# Jarque-Bera and the Shapiro-Wilk tests of normality 
# on re_turns, and return a named vector containing 
# the statistics (not the p-values!), 
# You must use the functions jarque.bera.test() and 
# shapiro.test(). 
# Be careful because shapiro.test() doesn't accept 
# arguments of class xts, so you must first coerce 
# it into a matrix using function coredata(). 
# You can use the function unname() to strip the 
# names from the values returned by the functions 
# jarque.bera.test() and shapiro.test(). 

# load package tseries
library(tseries)

### write your code here

# apply get_hyp_stats() as follows, to verify it works correctly:
get_hyp_stats(etf_rets[, 1])

# get_hyp_stats(etf_rets[, 1]) should produce this:
# jarque_bera      shapiro 
# 9008.6452123    0.8979459 


# 2. (20pts) Perform an sapply() loop over the columns 
# of etf_rets, and apply get_hyp_stats() to the columns 
# of etf_rets, and call the output matrix hyp_stats. 
# The first column of hyp_stats should contain the 
# Jarque-Bera statistics, while the second should 
# contain the Shapiro-Wilk statistics. 
# The rownames of hyp_stats should contain the names 
# in some_symbolsetf_rets. 
# You can use functions sapply(), na.omit(), and t(). 
# Be careful because some columns of etf_rets contain 
# NA values, so you must pass them through na.omit(). 

### write your code here

# You should get the following result:
#     jarque_bera   shapiro
# VTI   9008.6452 0.8979459
# VEU   7442.9831 0.8977588
# IEF    415.6686 0.9861641
# VNQ  12500.0078 0.8367968
# DBC    610.5074 0.9704973
# VXX    684.3048 0.9589765
# etc.


# 3. (10pts) Create a scatterplot of hyp_stats, and 
# add labels containing the rownames of hyp_stats. 
# Use functions plot() and text(),

### write your code here


# 4. (10pts) Sort hyp_stats on column "jarque_bera" 
# in ascending (increasing) order. 
# Use function order(),

### write your code here

# save hyp_stats to a comma-delimited CSV file. 
# Use function write.csv(),

### write your code here

