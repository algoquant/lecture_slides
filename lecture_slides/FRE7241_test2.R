#################################
### FRE7241 Test #2 Sep 29, 2015
#################################
# Max score 60pts

# Please write in this file the R code needed to perform the tasks below, 
# rename the file to your_name_test2.R
# and upload the file to NYU Classes


############## Part I
# Summary: subset daily stock prices to dates at the end of the week, 
# and calculate percentage returns over past four week periods.

# 1. (15pts) 
# download the file "etf_data.Rdata" from NYU Classes, and load() it. 

library(quantmod)
load(file="C:/Develop/data/etf_data.Rdata")

# "etf_data.Rdata" contains an environment called "env_data", 
# with OHLC time series data for ETFs, including "VTI". 
# Extract the adjusted close prices from "VTI" into a variable 
# called "stock_prices".
# you can use function Ad(), 

### write your code here

# subset "stock_prices" to the end of the week dates. 
# you can use function endpoints() with the "on" argument, 

### write your code here

# verify that the dates of "stock_prices" are indeed "Friday" 
# (except for some holidays), 
# by printing the tail of the index of "stock_prices", and converting 
# it to days of the week, 
# you can use functions weekdays(), index(), and tail(), 

### write your code here

# calculate percentage returns from "stock_prices", over trailing  
# four week periods, and call it "stock_rets", 
# you can use functions diff() and lag() with the "lag" argument, 

### write your code here

# 2. (15pts) 
# extract the highest and lowest returns, and their associated dates, 
# you can use functions index, which(), or which.max() and which.min, 

### write your code here



############## Part II
# Summary: perform a single lapply() loop to extract price and 
# volume columns from OHLC data series contained in "env_data". 

# download the file "etf_data.Rdata" from NYU Classes, and load() it. 
# "etf_data.Rdata" contains an environment called "env_data", 
# with OHLC time series data for ETFs, including "VTI". 

library(quantmod)
load(file="C:/Develop/data/etf_data.Rdata")

# 1. (10pts) Create a function called ex_tract(), that extracts 
# the adjusted price and volume columns from an OHLC data series, 
# and returns an xts series with two columns, 
# you can use functions merge(), Ad(), and Vo(), 

### write your code here

# Apply function ex_tract() to a single xts series, to verify 
# it works correctly:

foo <- ex_tract(env_data$VTI)
head(foo)


# 2. (20pts) Create a vector of symbols, called "sym_bols", 

sym_bols <- c("DBC", "VTI", "IEF")

# Perform an lapply() loop over a subset of "env_data" 
# containing "sym_bols", and call the function ex_tract() 
# on each element in the subset, and call the output "da_ta". 
# "da_ta" should be a list of xts series, with each xts series 
# containing price and volume data for a single symbol. 
# you can use functions as.list(), ex_tract(), and lapply(), 

### write your code here

# Flatten "da_ta" into a single xts series, and call it "da_ta". 
# you can use functions do.call() and merge(), 

### write your code here

