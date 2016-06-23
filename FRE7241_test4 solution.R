#################################
### FRE7241 Test #4 Solutions May 17, 2016
#################################
# Max score 90pts

# The below solutions are examples,
# Slightly different solutions are also possible.

##############
# Summary: Optimize portfolios using the Sortino ratio. 
# Study how the weights of the optimal portfolio 
# change as the risk-free rate changes. 

# Download the file etf_data_new.RData from NYU Classes,
# and load() it. 
# etf_data_new.RData contains the environment env_etf, 
# which contains the xts series called re_turns, with 
# ETF returns. 

library(quantmod)
load(file="C:/Develop/data/etf_data_new.RData")


# 1. (20pts) Create a vector of symbol names called 
# sym_bols. 
# Create a named vector of initial portfolio weights 
# for sym_bols, all equal to 1, and call it weight_s. 
# The names of weight_s should be equal to sym_bols. 
# You can use functions rep() and names(). 

sym_bols <- c("VTI", "IEF", "XLP")
n_weights <- length(sym_bols)
weight_s <- rep(1, n_weights)
names(weight_s) <- sym_bols

# You should get the following output:
# > weight_s
# VTI IEF XLP
# 1   1   1

# Create a variable called risk_free, with the risk 
# free rate, equal to 1 basis point (0.01%). 

risk_free <- 0.01

# Create an objective function called object_ive(), 
# equal to minus the Sortino ratio. 
# The function object_ive() should accept two arguments:
# - p_weights: a named numeric vector of portfolio weights. 
# - risk_free: the risk free rate. 
# 
# Calculate the Sortino ratio as the average returns 
# minus the risk free rate, divided by the standard 
# deviation of returns that are below the risk free 
# rate. 
# You cannot use the function SortinoRatio(), from 
# package PerformanceAnalytics. 
# 
# object_ive() should subset env_etf$re_turns by the 
# names of p_weights, and multiply the returns by 
# p_weights, to obtain the portfolio returns. 
# object_ive() should scale (divide) the portfolio 
# returns by the sum of p_weights, and then subtract 
# the risk free rate from them. object_ive() should 
# finally return minus the Sortino ratio. 
# You can use functions mean(), sd(), sum(), and the 
# "%*%" operator. 
# Use percentages in your calculation, by multiplying 
# env_etf$re_turns by 100. 

# object_ive() returns minus the Sortino ratio
object_ive <- function(p_weights, risk_free) {
  portf_rets <- 100*env_etf$re_turns[, names(p_weights)] %*% p_weights / sum(p_weights)
  portf_rets <- portf_rets - risk_free
  -mean(portf_rets)/sd(portf_rets[portf_rets<0])
}  # end object_ive

# Calculate object_ive() for the equal weight portfolio,
# and for a portfolio with double the weights, and
# verify that they both produce the same number.

# You should get the following output:
# > object_ive(weight_s, risk_free)
# [1] -0.04826844
# > object_ive(2*weight_s, risk_free)
# [1] -0.04826844


# 2. (10pts) Perform a portfolio optimization to find 
# the weights that minimize object_ive().
# You can use the function optim(), with the "upper" 
# and "lower" parameters equal to 10 and -10, respectively.
# 
# You must use the dots "..." argument of function optim(), 
# to pass the risk_free argument to function object_ive(). 

optim_run <- optim(par=weight_s,
                   fn=object_ive,
                   method="L-BFGS-B",
                   upper=rep(10, n_weights),
                   lower=rep(-10, n_weights), 
                   risk_free=risk_free)

# Rescale the optimal weights, so their sum is equal to 1.

weight_s <- optim_run$par/sum(optim_run$par)

# You should get the following output (or close to it):
# > weight_s
#       VTI        IEF        XLP
# -0.04091966  0.63082065  0.41009901

# Calculate the optimal portfolio returns, and coerce 
# them to an xts series, and call them optim_rets.
# You can use the index of env_etf$re_turns. 
# Remember to use percentage returns in your calculation, 
# by multiplying env_etf$re_turns by 100. 
# You can use functions xts() and index(),

optim_rets <- 100*xts(x=env_etf$re_turns[, sym_bols] %*% weight_s,
                      order.by=index(env_etf$re_turns))

# Plot the optimal portfolio cumulative returns using
# chart_Series() from package quantmod.
# Place the title "Sortino portfolio" on the upper left. 
# You must use functions cumsum() and chart_Series(). 

library(quantmod)
x11()
# the paste() part is optional and isn't required for full credit
chart_Series(x=cumsum(optim_rets), 
             name=paste("Sortino portfolio\n", 
                        paste(names(weight_s), collapse=" "), "\n", 
                        paste(format(weight_s, digits=2), collapse=" ")))


# 3. (30pts) Calculate the optimal portfolio weights
# for a vector of risk-free rates. 

# Create a named vector of 6 risk-free rates, 
# from 0.0 to 0.025, with the element names equal 
# to their values:

risk_free_rates <- seq(from=0.0, to=0.025, by=0.005)
names(risk_free_rates) <- risk_free_rates

# You should get the following output:
# > risk_free_rates
#     0 0.005  0.01 0.015  0.02 0.025
# 0.000 0.005 0.010 0.015 0.020 0.025

# Perform an sapply() loop over risk_free_rates. 
# In each step perform a portfolio optimization, 
# and return the rescaled optimal weights. 
# Call the output weights_optim. 
# Allow only positive weights by changing the 
# "lower" parameter of optim().
# Remember to use the dots "..." argument of function 
# optim(), to pass the risk_free argument to function 
# object_ive(). 

weights_optim <- sapply(risk_free_rates, function(risk_free) {
  # perform portfolio optimization
  optim_run <- optim(par=weight_s,
                     fn=object_ive,
                     method="L-BFGS-B",
                     upper=rep(10, n_weights),
                     lower=rep(0, n_weights), 
                     risk_free=risk_free)
  # Rescale the optimal weights, so their sum is equal to 1.
  optim_run$par/sum(optim_run$par)
})  # end sapply

# You should get the following output:
# > weights_optim
#             0   0.005       0.01    0.015     0.02       0.025
# VTI 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000
# IEF 0.6695571 0.6614521 0.6421029 0.6189144 0.5628829 0.4297936
# XLP 0.3304429 0.3385479 0.3578971 0.3810856 0.4371171 0.5702064


# Calculate a named matrix of returns and standard 
# deviations of the optimal portfolios, called ret_sd_optim. 
# You can use functions apply(), c(), mean(), and sd(). 

ret_sd_optim <- apply(weights_optim, 2, function(weight_s) {
  portf_rets <- 100*env_etf$re_turns[, sym_bols] %*% weight_s
  c(ret=mean(portf_rets), sd=sd(portf_rets))
})  # end apply

# You should get the following output:
# > ret_sd_optim
#           0      0.005      0.01      0.015       0.02      0.025
# ret 0.03126726 0.03142853 0.03181355 0.03227497 0.0333899 0.03603816
# sd  0.34434843 0.34632742 0.35200499 0.36050250 0.3878860 0.48206050


# 4. (30pts) Create a scatterplot of random portfolios 
# following the example of the lecture slides, and add 
# to it points for the optimal portfolios from p.3 above. 
# Change the lecture code in two ways: 
# Allow only positive weights by changing the parameter 
# to runif().
# Rescale the random weights so that their sum is equal 
# to 1 (not their sum of squares). 
# Remember to use percentage returns in your calculation, 
# by multiplying env_etf$re_turns by 100. 

# You should get a plot similar to efficient_portfolios.png
# on NYU Classes. 

# Define the number of weights:
n_weights <- length(sym_bols)
# Define the number of random portfolios:
n_portf <- 1000

# Calculate a matrix of mean returns and standard 
# deviations of random portfolios, and call it ret_sd. 

ret_sd <- sapply(1:n_portf, function(in_dex) {
  weight_s <- runif(n_weights, min=0, max=10)
  weight_s <- weight_s/sum(weight_s)
  portf_rets <- 100*env_etf$re_turns[, sym_bols] %*% weight_s
  c(ret=mean(portf_rets), sd=sd(portf_rets))
})  # end sapply

# plot a scatterplot of random portfolios. 

x11()
plot(x=ret_sd[2, ], y=ret_sd[1, ], 
     xlim=c(0, max(ret_sd[2, ])), 
     ylim=c(min(0, min(ret_sd[1, ])), max(ret_sd[1, ])), 
     main="Max Sortino and Random Portfolios", 
     xlab=rownames(ret_sd)[2], ylab=rownames(ret_sd)[1])

# Add points for the optimal portfolios from p.3 above. 

points(ret_sd_optim[2, ], ret_sd_optim[1, ], col="green", lwd=3)
text(ret_sd_optim[2, ], ret_sd_optim[1, ], labels=colnames(ret_sd_optim), pos=2, cex=0.8)

# Add Capital Market Lines corresponding to the risk_free_rates. 
# You can use functions for(), points(), text(), format(), and 
# abline(). 

for(risk_free_rate in risk_free_rates) {
  points(x=0, y=risk_free_rate)
  text(0, risk_free_rate, labels=paste0("risk-free=", risk_free_rate), pos=4, cex=0.8)
  abline(a=risk_free_rate, 
         b=(ret_sd_optim[1, format(risk_free_rate)]-risk_free_rate)/ret_sd_optim[2, format(risk_free_rate)], 
         col="blue")
}  # end for


