#################################
### FRE7241 Test #3 Oct 5, 2015
#################################
# Max score 60pts

# Please write in this file the R code needed to perform the tasks below, 
# rename the file to your_name_test3.R
# and upload the file to NYU Classes


# Part I
##############
# Summary: Perform an sapply() loop over the columns of "etf_rets", 
# calculate the max drawdowns, and Sortino and Calmar ratios, 
# extract the data into a named matrix. 

# download the file "etf_data.Rdata" from NYU Classes, and load() it. 
# "etf_data.Rdata" contains an environment called "env_data", 
# with OHLC time series data for ETFs. 

library(quantmod)
load(file="C:/Develop/data/etf_data.Rdata")

# create a vector of symbols called "sym_bols",

sym_bols <- c("VTI", "VEU", "IEF", "DBC")

# 1. (20pts) Perform an sapply() loop over the columns of "etf_rets" 
# subset by "sym_bols", 
# inside the loop calculate the max drawdowns, and Sortino and Calmar ratios, 
# extract the data into a named matrix. 
# you can use functions sapply(), table.Drawdowns() (column "Depth"), 
# SortinoRatio(), CalmarRatio(), and an anonymous function,

library(PerformanceAnalytics)

### write your code here


# Part II
##############
# Summary: calculate the rolling standard deviation of returns, 
# and the aggregated trading volumes, over monthly end points. 
# Create plots and perform a regression of the two. 

# download the file "etf_data.Rdata" from NYU Classes, and load() it. 
# "etf_data.Rdata" contains an environment called "env_data", 
# with OHLC time series data for ETFs, including "VTI". 

library(quantmod)
load(file="C:/Develop/data/etf_data.Rdata")

# 1. (20pts)
# Calculate the "VTI" daily returns from the adjusted close prices.
# you can use functions Ad() and dailyReturn(), 

### write your code here

# Merge the daily returns with the "VTI" volume column, 
# and call it "return_volume",
# you can use functions Vo() and merge(), 

### write your code here

# remove rows of "return_volume" with NA values, 
# you can use function complete.cases(), 

### write your code here

# Calculate the monthly end points for "return_volume". 
# Calculate the standard deviation of returns over the monthly 
# end points. 
# Calculate the aggregated trading volumes over the monthly 
# end points. 
# Merge the standard deviations with the aggregated volumes 
# and call it "volat_volume", 
# Assign to "volat_volume" the column names "volat" and "volu", 
# you can use functions endpoints(), sd(), colnames(), period.sum(), 
# and period.apply(), 

### write your code here


# 2. (10pts)
# plot the columns of "volat_volume" in a single plot in two panels,
# you can use method plot.zoo(), 

### write your code here

# plot a scatterplot of the two columns of "volat_volume", 
# you can use function plot() with a formula argument, 
# you can create a formula from the column names of "volat_volume", 
# you can use functions colnames(), as.formula() and paste(), 

### write your code here


# 3. (10pts) perform a regression of the two columns of "volat_volume", 
# you can create a formula from the column names of "volat_volume". 
# Extract from summary() the regression statistics: 
#  t-value, p-value, adj.r.squared, 
# and create a named vector with these regression statistics. 
# you can use functions colnames(), as.formula(), lm() and summary(), 

### write your code here

# perform the Durbin-Watson test for the autocorrelations of residuals,
# write what is the null hypothesis?
# can the null hypothesis be rejected in this case?
# use function dwtest(), from package lmtest,

library(lmtest)  # load lmtest

### write your code here

