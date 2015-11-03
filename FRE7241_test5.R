#################################
### FRE7241 Test #5 Oct 27, 2015
#################################
# Max score 70pts

# Please write in this file the R code needed to perform the tasks below, 
# rename the file to your_name_test5.R
# and upload the file to NYU Classes

##############
# Summary: create a scatterplot of random portfolios, 
# together with a minimum variance portfolio. 

# Download the file "etf_data.Rdata" from NYU Classes, 
# and load() it. 
# "etf_data.Rdata" contains an xts series called "etf_rets", 
# with ETF returns,

library(xts)
library(quantmod)
load(file="C:/Develop/data/etf_data.Rdata")


# 1. (20pts) Create a vector of symbol names called "sym_bols", 
# create a named vector of initial portfolio weights for the 
# "sym_bols", all equal to 1, and call it "weight_s", 
# You can use functions rep() and names(), 

sym_bols <- c("VTI", "VNQ", "DBC", "XLP", "XLK")

### write your code here

# Create an objective function equal to the standard 
# deviation of returns, and call it object_ive(). 
# The objective function should accept a single argument: 
#  "weights": the portfolio weights, 
# object_ive() should return the standard deviation of 
# portfolio returns, divided by the square root of the 
# sum of squared "weights". 
# You can use functions sqrt(), sd(), and sum(), 

### write your code here

# Calculate object_ive() for the equal weight portfolio,
# and for a portfolio with double the weights, and 
# verify that they both produce the same number.

object_ive(weight_s)
object_ive(2*weight_s)


# 2. (20pts) Perform a portfolio optimization to 
# find the weights that minimize object_ive(). 
# You can use the function optim() , with the 
# "upper" and "lower" parameters equal to 10 
# and -10, respectively. 

### write your code here

# Rescale the optimal weights, so their sum of squares 
# is equal to "1". 
# These are the weights of the minimum variance portfolio. 
# hint: read the forum hint for homework #5.

### write your code here

# Calculate the minimum variance portfolio returns, and 
# coerce them to an xts series, and call them "optim_rets". 
# You can use the index of "etf_rets". 
# You can use functions xts() and index(), 

library(xts)

### write your code here

# Plot the cumulative returns of "optim_rets" using 
# chart_Series(). 
# You must use functions cumsum() and chart_Series(), 

library(quantmod)

### write your code here


# 3. (30pts) Calculate a matrix of cumulative returns and 
# standard deviations of random portfolios, and call it "ret_sd". 
# Define the number of random portfolios:

n_portf <- 1000

# Perform an sapply() loop over "1:n_portf", and at each step 
# create a vector of random weights using rnorm(). 
# Rescale the random weights, so their sum of squares 
# is equal to "1". 
# Multiply "etf_rets" by the random weights, and calculate 
# the cumulative return and standard deviation. 
# The output should ba a named matrix called "ret_sd". 
# You can use functions rnorm(), sum(), and sd(), 
# and an anonymous function,

### write your code here

# Create a scatterplot of "ret_sd", with standard deviations on 
# the x-axis, and returns on the y-axis, and add a title. 
# Add a point in "red" to the scatterplot corresponding to the 
# minimum variance portfolio. 
# You can use functions plot(), points(), text(), and title(), 

### write your code here

# Redefine object_ive() so that it's equal to minus 
# the Sharpe ratio. 

### write your code here

# Perform a portfolio optimization to find the weights 
# that minimize object_ive(). 

### write your code here

# Rescale the optimal weights, so their sum of squares 
# is equal to "1". 
# Calculate the maximum Sharpe ratio portfolio returns, and 
# coerce them to an xts series, and call them "optim_rets". 

### write your code here

# Add a point in "blue" to the scatterplot corresponding to the 
# maximum Sharpe ratio portfolio. 
# You can use functions points() and text(), 

### write your code here

