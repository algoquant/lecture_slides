#################################
### FRE7241 Test #1 Solutions 06/16/15
#################################
# Max score 50pts

# The below solutions are examples,
# Slightly different solutions are also possible.



##################################
# 1. (15pts) subset "zoo_series" to Mondays, 
# download file "zoo_series.Rdata" from NYU Classes, and load() it, 
# the file "zoo_series.Rdata" contains a zoo called "zoo_series",

load(file="C:/Develop/data/zoo_series.RData")

# first create a logical vector from the index of "zoo_series",
# called "mon_days", which is TRUE if an index date is a Monday, 
# and FALSE otherwise,
# use functions index() and weekdays()
library(zoo)
mon_days <- weekdays(index(zoo_series))=="Monday"

# extract (subset) the first column of "zoo_series" for index dates 
# that are Mondays, and call it "zoo_mondays",
zoo_mondays <- zoo_series[weekdays(index(zoo_series))=="Monday", 1]

# calculate weekly percentage returns from "zoo_mondays",
zoo_mondays <- diff(log(zoo_mondays))

# find the dates of the weeks (not just the indices) with 
# the highest and lowest returns,
# use functions which(), or which.max() and which.min,
zoo_mondays[which.max(zoo_mondays)]
zoo_mondays[which.min(zoo_mondays)]



##################################
# 2. (35pts) Create a function called lag_it() that applies a lag to vectors 
# and "zoo" time series objects,
# lag_it() should accept two arguments:
# the first argument called "se_ries" can be a vector or "zoo" time series object,
# if "se_ries" is a vector, then lag_it() should return a lagged vector, 
# of the same length as the input,
# if "se_ries" is a "zoo", then lag_it() should return a lagged "zoo", 
# with the same number of rows as the input,
# the second argument called "lag" is an integer specifying the number of lags,
# if "lag" is positive, then lag_it() should replace the present value with 
# "lag" number of values from the past, 
# if "lag" is negative, then lag_it() should replace the present value with 
# "lag" number of values from the future, 
# for a vector, past values have a smaller index, and future values have a larger index,
# lag_it() should add NA values in place of values that are missing, 
# lag_it() should return NULL if "se_ries" is neither a vector nor a 
# "zoo" time series,
# 
# some observations about the default method lag():
# the default method lag() can accept a vector and returns 
# a "ts" time series object,
# 
# some observations about lag.zoo():
# The method lag.zoo() returns a lagged version of a "zoo" time series, 
# by shifting its time index by "k" observations,
# If "k" is positive, then lag.zoo() shifts values from the future to the present, 
# and if "k" is negative then it shifts them from the past, 
# This is the opposite of what is usually considered as a positive "lag",
# A positive lag should replace the present value with values from the past 
# (negative lags should replace with values from the future), 
# lag.zoo() omits any NA values the lag may have produced, 
# returning a shorter time series than the original,
# 
# hint: you can use functions is.vector(), is.zoo(), cbind(), merge(), 
# lag.zoo(), c(), and rep(), 

lag_it <- function(se_ries, lag=1) {
  if (is.vector(se_ries)) {  # se_ries is a vector
    if(lag>0) {
      se_ries <- c(rep(NA, lag), se_ries)
      se_ries[-((length(se_ries)-lag+1):length(se_ries))]
    } else {
      se_ries <- c(se_ries, rep(NA, -lag))
      se_ries[-(1:(-lag))]
    }
  } else if (is.zoo(se_ries)) {  # se_ries is a zoo
    lag(se_ries, k=-lag, na.pad=TRUE)
# or:
#    cbind(se_ries[, 1], lag(se_ries, k=-lag))[, -1]
  } else {  # se_ries is neither a vector nor a "zoo" time series
    warning(paste0("argument \"", deparse(substitute(se_ries)), "\" must be either a vector, zoo, or ts object"))
    NULL  # return NULL
  }
}  # end lag_it

# call lag_it() as below, to verify it works correctly,
load(file="C:/Develop/data/zoo_series.RData")
lag_it(1:9)
lag_it(1:9, 2)
lag_it(1:9, -1)
lag_it(1:9, -2)
lag_it(zoo_series[1:6, 1:2])
lag_it(zoo_series[1:6, 1:2], 2)
lag_it(zoo_series[1:6, 1:2], -1)
lag_it(matrix(1:9, ncol=1))
lag_it("a", "b")


