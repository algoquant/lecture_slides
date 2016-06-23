#################################
### FRE7241 Test #5 Solutions Oct 27, 2015
#################################
# Max score 70pts

# The below solutions are examples,
# Slightly different solutions are also possible.

##############
# Summary: create a scatterplot of random portfolios, 
# together with a minimum variance portfolio. 

# Download the file "etf_data.RData" from NYU Classes, 
# and load() it. 
# "etf_data.RData" contains an xts series called "etf_rets", 
# with ETF returns,

library(xts)
library(quantmod)
load(file="C:/Develop/data/etf_data.RData")


# 1. (20pts) Create a vector of symbol names called "sym_bols", 
# create a named vector of initial portfolio weights for the 
# "sym_bols", all equal to 1, and call it "weight_s", 
# You can use functions rep() and names(), 

sym_bols <- c("VTI", "VNQ", "DBC", "XLP", "XLK")
n_weights <- length(sym_bols)
weight_s <- rep(1, n_weights)
names(weight_s) <- sym_bols

# Create an objective function equal to the standard 
# deviation of returns, and call it object_ive(). 
# The objective function should accept a single argument: 
#  "weights": the portfolio weights, 
# object_ive() should return the standard deviation of 
# portfolio returns, divided by the square root of the 
# sum of squared "weights". 
# You can use functions sqrt(), sd(), and sum(), 

object_ive <- function(weights) {
  portf_ts <- etf_rets[, sym_bols] %*% weights
  sd(portf_ts)/sqrt(sum(weights^2))
}  # end object_ive

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

optim_run <- optim(par=weight_s, 
                   fn=object_ive, 
                   method="L-BFGS-B",
                   upper=rep(10, n_weights),
                   lower=rep(-10, n_weights))

# Rescale the optimal weights, so their sum of squares 
# is equal to "1". 
# These are the weights of the minimum variance portfolio. 
# hint: read the forum hint for homework #5.

weight_s <- optim_run$par/sqrt(sum(optim_run$par^2))

# Calculate the minimum variance portfolio returns, and 
# coerce them to an xts series, and call them "optim_rets". 
# You can use the index of "etf_rets". 
# You can use functions xts() and index(), 

library(xts)
optim_rets <- xts(x=etf_rets[, sym_bols] %*% weight_s, 
                  order.by=index(etf_rets))

# Plot the cumulative returns of "optim_rets" using 
# chart_Series(). 
# You must use functions cumsum() and chart_Series(), 

library(quantmod)
chart_Series(x=cumsum(optim_rets), name="minvar portfolio")


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

ret_sd <- sapply(1:n_portf, 
                 function(in_dex) {
                   weight_s <- rnorm(n_weights)
                   weight_s <- weight_s/sqrt(sum(weight_s^2))
                   portf_ts <- etf_rets[, sym_bols] %*% weight_s
                   c(ret=sum(portf_ts), sd=sd(portf_ts))
                 }  # end anonymous function
)  # end sapply

# Create a scatterplot of "ret_sd", with standard deviations on 
# the x-axis, and returns on the y-axis, and add a title. 
# Add a point in "red" to the scatterplot corresponding to the 
# minimum variance portfolio. 
# You can use functions plot(), points(), text(), and title(), 

plot(x=ret_sd[2, ], y=ret_sd[1, ], xlim=c(0, max(ret_sd[2, ])), 
     xlab=rownames(ret_sd)[2], ylab=rownames(ret_sd)[1])
title(main="Random and minvar portfolios", line=-1)
points(x=sd(optim_rets), y=sum(optim_rets), 
       col="red", lwd=3, pch=21)
text(x=sd(optim_rets), y=sum(optim_rets), 
     labels="minvol", pos=1, cex=0.8)

# Redefine object_ive() so that it's equal to minus 
# the Sharpe ratio. 

object_ive <- function(weights) {
  portf_ts <- etf_rets[, sym_bols] %*% weights
  -sum(portf_ts)/sd(portf_ts)
}  # end object_ive

# Perform a portfolio optimization to find the weights 
# that minimize object_ive(). 

optim_run <- optim(par=weight_s, 
                   fn=object_ive, 
                   method="L-BFGS-B",
                   upper=rep(10, n_weights),
                   lower=rep(-10, n_weights))

# Rescale the optimal weights, so their sum of squares 
# is equal to "1". 
# Calculate the maximum Sharpe ratio portfolio returns, and 
# coerce them to an xts series, and call them "optim_rets". 

weight_s <- optim_run$par/sqrt(sum(optim_run$par^2))
optim_rets <- xts(x=etf_rets[, sym_bols] %*% weight_s, 
                  order.by=index(etf_rets))

# Add a point in "blue" to the scatterplot corresponding to the 
# maximum Sharpe ratio portfolio. 
# You can use functions points() and text(), 

points(x=sd(optim_rets), y=sum(optim_rets), 
       col="blue", lwd=3, pch=21)
text(x=sd(optim_rets), y=sum(optim_rets), 
     labels="maxSR", pos=1, cex=0.8)

