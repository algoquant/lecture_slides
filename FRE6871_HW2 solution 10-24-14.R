#################################
### HW #2 Solution
#################################

# The below solutions are examples,
# Slightly different solutions are also possible.
# Max score 35pts

### HW #2:

# 5pts
# 1. Create a function for calculating the kurtosis of a time series of returns,

calc_kurtosis <- function(in_data=rnorm(1000)) {  # default is normal
# Calculates the kurtosis of a time series of returns.
  len_data <- length(in_data)
  mean_data <- mean(in_data)
  sd_data <- sd(in_data)
  len_data*(len_data+1)*sum(((in_data - mean_data)/sd_data)^4)/((len_data-1)^3)
}  # end calc_kurtosis



# 10pts
# 2. Using this function calculate the kurtosis of DAX returns, and of t-distribution returns with four degrees of freedom (use the same number of data points in both cases),

# DAX returns in scale 1% = 1.0
ts_rets <- 100*diff(log(EuStockMarkets[, 1]))
# calculate kurtosis of DAX returns
calc_kurtosis(in_data=ts_rets)
# calculate kurtosis of t-distribution
calc_kurtosis(in_data=rt(n=length(ts_rets), df=4))



# 20pts (even without legend)
# 3. Plot the probability density of DAX returns together with t-distribution returns with four degrees of freedom on a single plot,
# plot t-distribution
x_var <- seq(-5, 5, length=100)
plot(x=x_var, y=dt(x_var, df=4), type="l", lwd=2, xlab="", ylab="", ylim=c(0, 0.6))
# add line for density of DAX returns
lines(density(ts_rets), col="red", lwd=2)
# add legend
legend("topright", title="DAX vs t-distr", legend=c("t-distr", "DAX"), 
       inset=0.05, cex=0.8, lwd=2, lty=c(1, 1), col=c("black", "red"))


