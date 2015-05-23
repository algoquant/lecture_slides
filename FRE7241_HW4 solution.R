#################################
### FRE7241 HW #4 Solution due May 12, 2015
#################################
# Max score 40pts

# The below solutions are examples,
# Slightly different solutions are also possible.


##################################
# 1. (20pts) Load time series data and calculate rolling range statistics,
# the file "etf_series.Rdata" contains time series data,
# create a new environment called "data_env",
# load data from the file "etf_series.Rdata" into "data_env",
# use function load(), with the "envir" argument,

data_env <- new.env()
load(file="C:/Develop/data/etf_series.Rdata", envir=data_env)


# extract the adjusted prices and volume for symbol "VTI" from "data_env", and call it "VTI",
# "VTI" will now be defined both in the default workspace and in "data_env",
# use function merge(), 

VTI <- merge(Ad(data_env$VTI), Vo(data_env$VTI))


# calculate rolling range statistics over a sliding window, called "win_dow",

win_dow <- 22


# calculate two "xts" time series of trailing maximum and minimum values 
# of adjusted prices over the sliding "win_dow", and call them "roll_max" and "roll_min",
# at every point in time, the value of "roll_max" should be equal to the maximum 
# adjusted price from points in the past covered by "win_dow",
# use function rollmax() from package "zoo", with the proper "k" and "align" arguments,

library(zoo)
roll_max <- rollmax(x=VTI[, 1], k=win_dow, align="right")
colnames(roll_max) <- "max"
roll_min <- -rollmax(x=(-VTI[, 1]), k=win_dow, align="right")
colnames(roll_min) <- "min"


# calculate the difference between "roll_max" and "roll_min", and call it "ra_nge",

ra_nge <- (roll_max-roll_min)
colnames(ra_nge) <- "range"


# calculate an "xts" time series of trailing mean values of the volume 
# over the sliding "win_dow", and call it "roll_volume",
# use function rollmean(), with the proper "k" and "align" arguments,

roll_volume <- rollmean(x=Vo(data_env$VTI), k=win_dow, align="right")
colnames(roll_volume) <- "volume"


# merge "ra_nge" with "roll_volume" into a single "xts" time series,
# and call it "range_volume", 
# remove rows with NAs,
# use functions merge() and na.omit(),

range_volume <- merge(ra_nge, roll_volume)
range_volume <- na.omit(range_volume)


# create a time series plot of both columns of "range_volume" in two panels, 
# use function plot.zoo(),

plot.zoo(range_volume)


# create a scatterplot of "range_volume", 
# use function plot(),

plot(range~volume, data=range_volume)



##################################
# 2. (20pts) perform a regression of "ra_nge" vs "roll_volume"
# extract from summary() the regression statistics: p-value, adj.r.squared, fstatistic,
# create a named vector of the regression statistics, 
# use function plot(),

reg_model <- lm(range~volume, data=range_volume)
reg_model_sum <- summary(reg_model)
c(pval=reg_model_sum$coefficients[2, 4],
  adj.r.squared=reg_model_sum$adj.r.squared,
  fstat=reg_model_sum$fstatistic[1])


# perform Durbin-Watson test for the autocorrelations of regression residuals,
# write what is the null hypothesis?
# can the null hypothesis be rejected?
# use function dwtest(), from package lmtest,

library(lmtest)  # load lmtest
dwtest(reg_model)


# perform the same regression on a subset of the data from 2010 and afterwards,

reg_model <- lm(range~volume, data=range_volume["2010/"])
reg_model_sum <- summary(reg_model)
c(pval=reg_model_sum$coefficients[2, 4],
  adj.r.squared=reg_model_sum$adj.r.squared,
  fstat=reg_model_sum$fstatistic[1])


