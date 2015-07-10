#################################
### FRE7241 HW #2 due June 23, 2015
#################################
# Max score 40pts

# Please write in this file the R code needed to perform the tasks below, 
# rename it to your_name_hw2.R
# and send this file to Jaimin Doshi (jbd316@nyu.edu)



##################################
# 1. (20pts) Download data for multiple symbols using get.hist.quote()
# create a data directory on your computer, and save all files to that directory,
# remember the location of the data directory for future use,
# load package tseries,
library(tseries)

# create a vector of symbols called "sym_bols",
sym_bols <- c("VTI", "VEU", "IEF", "VNQ", "DBC", "XLY", "XLP", "XLE", "XLF", "XLV", "XLI", "XLB", "XLK", "XLU", "IWB", "IWD", "IWF", "IWM", "IWN", "IWO", "IWP", "IWR", "IWS", "IWV", "IUSV", "IUSG")

# download 10yrs of price and volume data for the list of sym_bols, 
# and call it "zoo_series",
# for each symbol download the fields "AdjClose" and "Volume",
field_names <- c("AdjClose", "Volume")

# use get.hist.quote() and an lapply() loop,
# name the list returned by lapply as "zoo_series" (each list element is a zoo object),

### write your code here


# flatten zoo_series into a single zoo object, 
# use functions do.call() and merge(),

### write your code here


# assign new column names to zoo_series, 
# in the format "symbol.Close", "symbol.Volume", etc.
# use colnames(), sapply() and paste(),

### write your code here


# save zoo_series to a comma-separated CSV file called "zoo_series.csv", 
# use function write.zoo(),

### write your code here


# save zoo_series to a binary file called "zoo_series.Rdata", using save(),

### write your code here




##################################
# 1. (20pts) simulate an ARIMA AR(1) process, and call it "ts_arima",
# use function arima.sim(),
# the length of the series should be 100,
# the "model" argument should specify an ARIMA AR(1) process,
# with a single coefficient equal to 0.5,
# set the burn-in period to zero, by specifying the "start.innov"
# argument equal to a vector of zeroes of length 100,
# first reset the random number generator by calling set.seed(1121), 

set.seed(1121)

### write your code here


# simulate the same AR(1) process as above, but now recursively, 
# by calculating current returns from previous returns in a for() loop, 
# and call it "ts_arima_loop",
# use functions rnorm() and for(),
# you should obtain the same "ts_arima" series as above, 
# remember to reset the random number generator by calling set.seed(1121),
# use function numeric() to pre-allocate the vector "ts_arima" before 
# the loop starts,
set.seed(1121)

### write your code here


# use function as.ts() to coerce "ts_arima_loop" to a "ts" time series,

### write your code here


# use the function identical() to confirm that the two methods give 
# the exact same result, and that "ts_arima" and "ts_arima_loop"
# are identical,

### write your code here





