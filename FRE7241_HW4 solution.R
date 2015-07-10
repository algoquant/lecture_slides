#################################
### FRE7241 Homework #4 Solution due July 6, 2015
#################################
# Max score 45pts

# The below solutions are examples,
# Slightly different solutions are also possible.


##################################
# calculate the rolling standard deviation of returns, 
# and aggregate the volume, over monthly end points, 
# 
# 1. (5pts) Load time series data from file 
# "etf_rets_volume.Rdata" (download it from NYU Classes),
# containing "xts" series with stock return and volume data,
# create a new environment called "env_returns", 
# load data from the file "etf_rets_volume.Rdata" into "env_returns",
# use functions new.env() and load(), with the "envir" argument,
env_returns <- new.env()
load(file="C:/Develop/data/etf_rets_volume.Rdata", envir=env_returns)


# the environment "env_returns" should contain a number of "xts" series, 
# each containing stock daily return and volume data for a single symbol, 
# you can assume that all the "xts" series have the same date index,
# create a vector of monthly end points for any of the "xts" series 
# in "env_returns",
# called "end_points", and set the first "end_points" equal to 1,
# use function endpoints() from package xts,
library(xts)
end_points <- endpoints(env_returns$VTI_rets, on='months')
end_points[1] <- 1


# 2. (20pts) create a function called agg_volat_volume(), 
# that accepts three arguments:
#   "x_ts": an "xts" containing returns and volume data, 
#   "end_points": a vector of end points, 
#   "envir": an environment argument, 
# agg_volat_volume() should:
# - extract returns and volume data from "x_ts",
# - extract the symbol name from the columns of "x_ts" ("symbol_name"),
# - calculate the volatility from the returns, 
#    over non-overlapping periods given by "end_points", 
# - calculate the total volume, 
#    over non-overlapping periods given by "end_points", 
# - cbind volatility with volume data into a single "xts" ("volat_volume"),
# - rename the colnames of "volat_volume" to "symbol_name.Volat" 
#    and "symbol_name.Volume", (replace "symbol_name" with the symbol name),
# - assign (copy) "volat_volume" to an object named "symbol_name" 
#   in the "envir" environment, 
# agg_volat_volume() should produce the side effect of creating 
# an "xts" object in the "envir" environment, that contains volatility 
# and volume data calculated from the input "x_ts",
# agg_volat_volume() should return invisible the "symbol_name",
# you can use functions strsplit(), colnames(), period.apply(), 
# period.sum(), cbind(), paste() (or paste0), xts(), 
# assign(), invisible(),

agg_volat_volume <- function(x_ts, end_points, envir=env_returns) {
  symbol_name <- strsplit(
    colnames(x_ts)[1], split="[.]")[[1]][1]
  std_dev <- period.apply(x_ts[, 1], 
                          INDEX=end_points, 
                          FUN=sd)
  vol_ume <- period.sum(x_ts[, 2], 
                        INDEX=end_points)
  volat_volume <- xts(cbind(std_dev, vol_ume[-1, ]), 
                      order.by=index(x_ts[end_points[-1], ]))
  colnames(volat_volume) <- 
    c(paste0(symbol_name, ".Volat"), paste0(symbol_name, ".Volume"))
  assign(symbol_name, volat_volume, envir=envir)
  invisible(symbol_name)
}  # end agg_volat_volume


# 2. (10pts) create a new environment called "env_volat", 
# for storing "xts" containing stock return and volume data,
# use function new.env(),
env_volat <- new.env()

# apply function agg_volat_volume() to a "VTI_rets", to verify 
# it works correctly:
agg_volat_volume(env_returns$VTI_rets, 
                 end_points=end_points, 
                 envir=env_volat)


# plot both columns of "env_volat$VTI", in a plot with two panels, 
# you can use function plot.zoo(), or plot.xts() and par(),
plot.zoo(env_volat$VTI)
# or
par(mfrow=c(2, 1))
plot(env_volat$VTI[, 2])
plot(env_volat$VTI[, 1])


# plot a scatterplot of both columns of "env_volat$VTI", 
# you can use function plot() with "data" argument,
plot(VTI.Volume ~ VTI.Volat, data=env_volat$VTI)

# calculate the month-over-month difference of both columns
# of "env_volat$VTI", 
# use function diff(), 
# plot a scatterplot of both the diff columns, 
plot(VTI.Volume ~ VTI.Volat, data=diff(env_volat$VTI))



##################################
# 3. (10pts) perform a regression of volume versus volatility, 
# of "env_volat$VTI", 
# extract from summary() the regression statistics: 
#  p-value, adj.r.squared, fstatistic,
# create a named vector of the regression statistics, 
reg_model <- lm(VTI.Volume ~ VTI.Volat, data=env_volat$VTI)
reg_model_sum <- summary(reg_model)
c(pval=reg_model_sum$coefficients[2, 4],
  adj.r.squared=reg_model_sum$adj.r.squared,
  fstat=reg_model_sum$fstatistic[1])

# perform the Durbin-Watson test for the autocorrelations of 
# regression residuals,
# write what is the null hypothesis?
# can the null hypothesis be rejected?
# use function dwtest(), from package lmtest,
library(lmtest)  # load lmtest
dwtest(reg_model)


# repeat the whole regression analysis from above for the 
# month-over-month difference of both columns of "env_volat$VTI", 
reg_model <- lm(VTI.Volume ~ VTI.Volat, data=diff(env_volat$VTI))
reg_model_sum <- summary(reg_model)
c(pval=reg_model_sum$coefficients[2, 4],
  adj.r.squared=reg_model_sum$adj.r.squared,
  fstat=reg_model_sum$fstatistic[1])

# perform the Durbin-Watson test for the autocorrelations of 
# regression residuals,
# use function dwtest(), from package lmtest,
library(lmtest)  # load lmtest
dwtest(reg_model)

