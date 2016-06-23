#################################
### FRE7241 Test #3 May 10, 2016
#################################
# Max score 70pts

# Please write in this file the R code needed to perform the tasks below, 
# rename the file to your_name_test3.R
# and upload the file to NYU Classes


############## Part I
# Summary: Perform an sapply() loop over the columns 
# of etf_rets, and calculate the Sortino and Calmar 
# ratios, and the max drawdowns. 

# Download the file etf_data.RData from NYU Classes,
# and load() it.
# etf_data.RData contains an environment called env_data,
# with OHLC time series data for ETFs.

library(quantmod)
load(file="C:/Develop/data/etf_data.RData")

# create a vector of symbols called some_symbols,

some_symbols <- c("VTI", "VEU", "IEF", "DBC")

# 1. (20pts) Perform an sapply() loop over the columns 
# of etf_rets, subset by some_symbols.  Call the output 
# matrix etf_stats. 
# Inside the loop calculate the Sortino and Calmar 
# ratios, and the max drawdowns. 
# You should use functions sapply(), SortinoRatio(), 
# CalmarRatio(), table.Drawdowns() (column "Depth"),
# and an anonymous function. 
# The anonymous function should return a named vector 
# with the data. 

library(PerformanceAnalytics)

### write your code here

# You should get the following output:
# t(etf_stats)
#       sor_tino     cal_mar  draw_down
# VTI  0.03615478  0.12059340   -0.5545
# VEU  0.01469526  0.01527280   -0.6152
# IEF  0.07903142  0.59006446   -0.1040
# DBC -0.01453087 -0.07397729   -0.7402



############## Part II
# Summary: Create a function that extracts columns from an
# OHLC data series contained in an environment, performs
# calculations on the columns, and saves the result in an
# xts series in a different environment.

# Download the file etf_data.RData from NYU Classes,
# and load() it.
# etf_data.RData contains an environment called env_data,
# with OHLC time series data for ETFs, including VTI.

library(quantmod)
load(file="C:/Develop/data/etf_data.RData")

# 1. (30pts) Create a function called ex_tract(), that accepts
# three arguments:
# - "sym_bol": a string representing the name of an OHLC xts series,
# - "in_env": an input environment containing the xts series,
# - "out_env": an output environment for saving the output xts series,
#
# The function ex_tract() should:
# - extract adjusted prices and volume data from the xts series,
# - merge prices with volume data into a single xts series,
# - assign() (copy) the xts series to the "out_env" environment,
# - invisibly return the string "sym_bol".
# ex_tract() should produce the side effect of creating the
# output xts series in the "out_env" environment.
#
# Note that ex_tract() only receives a string representing the 
# name of an xts series, not the name itself, so you must use 
# the function get() to get the data, and the function assign() 
# to assign it (not "<-").
# You can also use functions merge(), invisible(), Ad(), 
# and Vo(). 

### write your code here

# Create a new environment called env_out. 
# Use function new.env().

### write your code here

# Apply function ex_tract() to the string "VTI" and 
# env_out, to verify it works correctly:

### write your code here

# Also run these commands to verify that a new object 
# was created:

ls(env_out)
head(env_out$VTI)

# You should get the following output:
# > head(env_out$VTI)
#             VTI.Adjusted VTI.Volume
# 2007-01-03     58.17359     798600
# 2007-01-04     58.28998    3305000
# 2007-01-05     57.82858     382000
# 2007-01-08     58.04057     299000
# 2007-01-09     58.04057     267000
# 2007-01-10     58.16943     359200


# 2. (10pts) Remove all the objects in env_etf
# whose names start with "V*".
# You can use the functions rm(), glob2rx() and
# ls() with the "pattern" argument.

### write your code here


# 3. (10pts) Create a vector of strings, 
# called some_symbols. 

some_symbols <- c("DBC", "VTI", "IEF")

# Perform an sapply() loop over some_symbols, to 
# apply the function ex_tract() to all the strings 
# in some_symbols. 
# To get full credit you must pass the arguments 
# "in_env=env_data" and "out_env=env_out" into 
# ex_tract() through the dots argument of the 
# sapply() function. 

### write your code here

