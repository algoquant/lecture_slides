#################################
### FRE7241 HW #3 Solution
#################################
# Max score 40 pts

# The below solutions are examples,
# Slightly different solutions are also possible.


##################################
# 1. (20pts) simulate an ARIMA AR(1) process for returns using function arima.sim(),
# set the burn-in period to zero, and reset random number generator by calling set.seed(1121),
# as follows: 

set.seed(1121)
ts_arima <- arima.sim(n=10, model=list(ar=0.5), start.innov=rep(0, 10))



######
# simulate the same AR(1) returns process as above, but now recursively, 
# by calculating current returns from previous returns in a sapply() or for() loop, 
# use functions rnorm() and either sapply() or for(),
# you should obtain the same "ts_arima" series as above, 
# remember to reset the random number generator by calling set.seed(1121),

set.seed(1121)
ts_arima <- numeric(10)
ts_arima[1] <- rnorm(1)
sapply(2:10, function(in_dex) ts_arima[in_dex] <<- 0.5*ts_arima[in_dex-1] + rnorm(1))



######
# simulate the same AR(1) process as above recursively, using functions rnorm() and filter(),
# use method="recursive" in filter(),
# you should obtain the same "ts_arima" series as above, 
# remember to reset the random number generator by calling set.seed(1121),

set.seed(1121)
ts_arima <- as.ts(rnorm(10))
ts_arima <- filter(x=ts_arima, filter=0.5, method="recursive")




##################################
# 2. (20pts) Calculate partial autocorrelation coefficients


######
# simulate an AR(2) process of length 1000, with coefficients equal to 0.2 and 0.3, 
# Use the function arima.sim(), with the proper "model" argument,
# Call the resulting "ts" time series "ts_arima", 
# remember to reset the random number generator by calling set.seed(1121),

set.seed(1121)
ts_arima <- arima.sim(n=1000, model=list(ar=c(0.2, 0.3)))



######
# Create a "ts" series lagged by one period from "ts_arima", and call it "ts_arima_lag",
# The value of "ts_arima_lag" in a given period should be equal to the value 
# of "ts_arima" in the previous period,
# Create a series lagged by two periods from "ts_arima", and call it "ts_arima_lag2",
# use function lag() with the proper argument "k",

ts_arima_lag <- lag(ts_arima, k=-1)
ts_arima_lag2 <- lag(ts_arima, k=-2)



######
# cbind "ts_arima" with "ts_arima_lag" and "ts_arima_lag2", and call it "ts_lagged",
# note the rows containing NAs at the beginning, and remove those rows,
# use functions cbind() and na.omit(),

ts_lagged <- na.omit(cbind(ts_arima, ts_arima_lag, ts_arima_lag2))
head(ts_lagged)



######
# Extract "ts_arima", "ts_arima_lag", and "ts_arima_lag2" from "ts_lagged" by subsetting,
# After applying cbind(), na.omit() and subsetting, all three time series 
# should be properly alligned and of the same length,

ts_arima <- ts_lagged[, 1]
ts_arima_lag <- ts_lagged[, 2]
ts_arima_lag2 <- ts_lagged[, 3]



######
# Extract the lag=5 vector of autocorrelation coefficients of "ts_arima", and call it "vec_acf", 
# use functions acf() and drop(), drop() removes array dimensions that are equal to 1,
# The function acf() returns an object of class "acf", so you must extract 
# the vector of autocorrelation coefficients from an object of class "acf",

vec_acf <- drop(acf(ts_arima, lag=5, plot=FALSE)$acf)
is.vector(vec_acf)  # check
# plot not required
acf_plus(ts_arima, lag=5)



######
# Calculate the autocorrelation of lag=1 as the correlation between 
# "ts_arima" and its own lag "ts_arima_lag", and call it "auto_corr",
# use function cor(),
# verify that "auto_corr" is almost equal to vec_acf[2],

auto_corr <- cor(ts_arima, ts_arima_lag)
auto_corr - vec_acf[2]



######
# Create a linear combination of "ts_arima" and "ts_arima_lag", and call it "ts_arima_1",
# such that "ts_arima_1" has zero correlation with "ts_arima_lag",
# "ts_arima_1" represents the part of "ts_arima" that is not correlated to "ts_arima_lag",
# verify that the correlation is almost zero, using function cor(),

ts_arima_1 <- ts_arima - auto_corr*sd(ts_arima)*ts_arima_lag/sd(ts_arima_lag)
cor(ts_arima_lag, ts_arima_1)



######
# Calculate the correlation between "ts_arima_lag2" and "ts_arima_lag", and call it "auto_corr",
# use function cor(),
# verify that "auto_corr" is almost equal to vec_acf[2],
# Explain in writing in one sentence why they should be almost equal,
# answer: because "ts_arima" is stationary,

auto_corr <- cor(ts_arima_lag2, ts_arima_lag)
auto_corr - vec_acf[2]



######
# Create a linear combination of "ts_arima_lag2" and "ts_arima_lag", and call it "ts_arima_2",
# such that "ts_arima_2" has zero correlation with "ts_arima_lag",
# "ts_arima_2" represents the part of "ts_arima_lag2" that is not correlated to "ts_arima_lag",
# verify that the correlation is almost zero, using function cor(),

ts_arima_2 <- ts_arima_lag2 - auto_corr*sd(ts_arima_lag2)*ts_arima_lag/sd(ts_arima_lag)
cor(ts_arima_lag, ts_arima_2)



######
# Calculate the correlation between "ts_arima_1" and "ts_arima_2", and call it "pauto_corr",
# "ts_arima_2" represents the part of "ts_arima_lag2" that is not correlated to "ts_arima_lag",

pauto_corr <- cor(ts_arima_1, ts_arima_2)



######
# Extract the lag=5 vector of partial autocorrelation coefficients of "ts_arima", and call it "vec_pacf", 
# use functions pacf() and drop(), drop() removes array dimensions that are equal to 1,
# The function pacf() returns an object of class "acf", so you must extract 
# the vector of partial autocorrelation coefficients from an object of class "acf",
# verify that "pauto_corr" is almost equal to vec_pacf[2],

vec_pacf <- drop(pacf(ts_arima, lag=5, plot=FALSE)$acf)
pauto_corr - vec_pacf[2]


