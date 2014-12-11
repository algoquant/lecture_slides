#################################
### Test #1 Solutions 12/02/14
#################################
# Max score 50pts

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
etf_rets <- do.call(merge, etf_rets)

# this renaming of columns is optional
colnames(etf_rets) <- sapply(colnames(etf_rets), function(strng) strsplit(strng, s="[.]")[[1]][1])

etf_series <- merge(etf_series, etf_series_ad, etf_rets)
write.zoo(etf_series, file='etf_series.csv', sep=",")


# 4. (5pts) plot the cumulative returns using chart.CumReturns,
chart.CumReturns(etf_rets, lwd=2, ylab="", legend.loc="topleft", main="")


# 5. (10pts) calculate a table of return distribution statistics using table.Stats,
#    perform return statistics ranking based on Skewness and Kurtosis,
ret_stats <- table.Stats(etf_rets)
ret_stats <- as.data.frame(t(ret_stats))
ret_stats$skew_kurt <- ret_stats$Skewness/ret_stats$Kurtosis
ret_stats <- ret_stats[order(ret_stats$skew_kurt, decreasing=TRUE), ]
ret_stats$Name <- etf_list[rownames(ret_stats), ]$Name


# 6. (5pts) calculate a table of Linear Regression Summary Statistics,
library(lmtest)
# if they didn't rename columns, then they needed to adjust "reg_formula" for this to work
etf_reg_stats <- sapply(colnames(etf_rets)[-1], 
                        function(etf_name) {
                          # specify regression formula
                          reg_formula <- as.formula(
                            paste(etf_name, "~", colnames(etf_rets)[1]))
                          # perform regression
                          reg_model <- lm(reg_formula, data=etf_rets)
                          # get regression summary
                          reg_model_sum <- summary(reg_model)
                          # collect regression statistics
                          etf_reg_stats <- with(reg_model_sum, 
                                                c(coefficients[1, 1], coefficients[1, 4], 
                                                  coefficients[2, 1], coefficients[2, 4]))
                          etf_reg_stats <- c(etf_reg_stats, 
                                             dwtest(reg_model)$p.value)
                          names(etf_reg_stats) <- c("alpha", "p-alpha", 
                                                    "beta", "p-beta", "p-dw")
                          etf_reg_stats
                        })  # end sapply

etf_reg_stats <- t(etf_reg_stats)
# sort by p-alpha
etf_reg_stats <- etf_reg_stats[order(etf_reg_stats[, "p-alpha"]), ]


# 7. (5pts) calculate a table of CAPM Summary Statistics using table.CAPM,
# if they didn't rename columns, then they needed to adjust this for it to work
table.CAPM(Ra=etf_rets[, -1], Rb=etf_rets[, "VTI"], scale=252)

