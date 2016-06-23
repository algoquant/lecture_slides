#################################
### FRE7241 Test #3 Solutions Oct 5, 2015
#################################
# Max score 60pts

# The below solutions are examples,
# Slightly different solutions are also possible.

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
etf_stats <- sapply(etf_rets[, sym_bols], 
                    function(x_ts) {
                      c(
                        draw_down=table.Drawdowns(x_ts)[1, "Depth"], 
                        sor_tino=SortinoRatio(x_ts), 
                        cal_mar=CalmarRatio(x_ts))
                    })  # end sapply


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

re_turns <- dailyReturn(Ad(env_data$VTI))

# Merge the daily returns with the "VTI" volume column, 
# and call it "return_volume",
# you can use functions Vo() and merge(), 

return_volume <- merge(re_turns, Vo(env_data$VTI))

# remove rows of "return_volume" with NA values, 
# you can use function complete.cases(), 

return_volume <- return_volume[complete.cases(return_volume)]

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

end_points <- endpoints(return_volume, on="months")
vol_at <- period.apply(return_volume[, 1], 
                       INDEX=end_points, 
                       FUN=sd)
vol_ume <- period.sum(return_volume[, 2], INDEX=end_points)
volat_volume <- merge(vol_at, vol_ume)
colnames(volat_volume) <- c("volat", "volu")


# 2. (10pts)
# plot the columns of "volat_volume" in a single plot in two panels,
# you can use method plot.zoo(), 

plot.zoo(volat_volume)

# plot a scatterplot of the two columns of "volat_volume", 
# you can use function plot() with a formula argument, 
# you can create a formula from the column names of "volat_volume", 
# you can use functions colnames(), as.formula() and paste(), 

plot(
  as.formula(paste(colnames(volat_volume), collapse=" ~ ")), 
  data=volat_volume)


# 3. (10pts) perform a regression of the two columns of "volat_volume", 
# you can create a formula from the column names of "volat_volume". 
# Extract from summary() the regression statistics: 
#  t-value, p-value, adj.r.squared, 
# and create a named vector with these regression statistics. 
# you can use functions colnames(), as.formula(), lm() and summary(), 

reg_model <- lm(
  as.formula(paste(colnames(volat_volume), collapse=" ~ ")), 
  data=volat_volume)
reg_model_sum <- summary(reg_model)
c(tval=reg_model_sum$coefficients[2, 3],
  pval=reg_model_sum$coefficients[2, 4],
  adj_rsquared=reg_model_sum$adj.r.squared)

# perform the Durbin-Watson test for the autocorrelations of residuals,
# write what is the null hypothesis?
# can the null hypothesis be rejected in this case?
# use function dwtest(), from package lmtest,

library(lmtest)  # load lmtest
dwtest(reg_model)

