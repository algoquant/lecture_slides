#################################
### Test #1 Solutions 12/02/14
#################################
# Max score 40pts

# The below solutions are an example,
# Slightly different solutions are also possible.
# You must use the requested functions.


# 1. (15pts) Download price data using package quantmod and function getSymbols,
library(quantmod)
sym_bols <- c("VTI", "AA", "MSFT")
data_env <- new.env()
getSymbols(sym_bols, env=data_env)
ls(data_env)


# 2. (5pts) scrub NA values, and calculate returns from adjusted prices,
etf_series <- do.call(merge, as.list(data_env)[sym_bols])
etf_series_ad <- do.call(merge, eapply(data_env, Ad)[sym_bols])
etf_series_ad <- etf_series_ad[complete.cases(etf_series_ad)]

etf_rets <- lapply(etf_series_ad, 
                   function(x_ts) {
                     daily_return <- dailyReturn(x_ts)
                     colnames(daily_return) <- names(x_ts)
                     daily_return
                   })  # end lapply

# 3. (5pts) combine all the data into one xts time series, 
#    and save it to a comma-delimited csv file called "etf_series.csv",
write.zoo(etf_series, file='etf_series.csv', sep=",")


#    hint: first create a data frame of the "setosa" species, which is a subset of the "iris" data frame.

mat_rix <- matrix(rnorm(100), ncol=4)


# 4. (5pts) plot the cumulative returns using charts.CumReturns,


# 5. calculate a table return distribution statistics using table.Stats,
#    perform return statistics ranking based on Skewness and Kurtosis,


# 6. calculate a table of Linear Regression Summary Statistics,


# 7. calculate a table of CAPM Summary Statistics using table.CAPM,

