##############################
### Homework and test ideas
##############################


##############################
# miscellaneous

rm(list=ls())  # remove all

### print and display options

options(max.print=40)
options(digits=3)

### plot parameters

par(new=TRUE)  # allow new plot on same chart
par(las=1)  # set text printing to "horizontal"

### startup package loading

suppressPackageStartupMessages(library(zoo))
# good package loading script inside functions
stopifnot("package:xts" %in% search() || require(xts, quietly=TRUE))



### to-do:

### add the following:
# hw: create a function get_reg_stats()
# hw: perform rolling calculations of sd, skew, MAD, VaR, SR, using vectorized functions: packages TTR, caTools,



##############################
# dates and times
##############################

############## test
# 1. (30pts) "my_date" is a numeric variable equal to 970518,
# that represents the date "1997-05-18".
# Convert my_date to a POSIXct date, with time zone equal to
# "America/New_York".
# You can use functions from package lubridate, or other
# functions such as paste() and substr().
# You will need to add the century value "19" to the year.
# You will not receive any credit for creating a date "by hand"
# as follows: as.POSIXct("1997-05-18"),

my_date <- 970518
library(lubridate)
ymd(paste0(19, my_date), tz="America/New_York")
# as.POSIXct("1997-05-18")
as.POSIXct(
  paste(paste0(19, substr(my_date, 1, 2)),
        substr(my_date, 3, 4),
        substr(my_date, 5, 6), sep="-")
)



############## test
# 2. (5pts) create a vector of decimal dates as follows:
# date_time <- 2014 + (1:5)/12
# convert them to POSIXct dates, using a lubridate function,
library(lubridate)
date_time <- 2014 + (1:5)/12
date_decimal(date_time)
# or
date_decimal(date_time, tz="America/New_York")



############## test
# Summary: Create a vector of weekly dates of class "Date"
# corresponding to every Monday, starting with "2013-09-02",
# and ending with the most recent Monday. Use two different methods.

# 1. (10pts) In the first method, you must use functions
# as.Date(), Sys.Date(), and seq() with "by" argument.

mon_days <- seq(from=as.Date("2013-09-02"),
                to=Sys.Date(), by="week")
# mon_days <- weekdays(mon_days)


# 2. (10pts) In the second method, you must use functions
# Sys.Date(), as.Date(), difftime() and trunc(), and
# lubridate function weeks().

library(lubridate)
start_date <- as.Date("2013-09-02")
end_date <- Sys.Date()
num_weeks <- trunc(difftime(end_date, start_date, units="weeks"))
mon_days <- start_date + weeks(0:num_weeks)
# subset - just to be sure
mon_days <- mon_days[(mon_days <= end_date)]



############## hw
# 1. (15pts) create a vector of weekly POSIXct dates corresponding
# to Mondays at 09:30AM, and call it "mon_days",
# start with the date "2015-02-09", and end at the most recent Monday
# before today (today is defined by Sys.time()),
# set the timezone to "America/New_York",
# first calculate the number of weeks between today and the start date,
# and use that number to create a vector of weekly POSIXct dates,
# use functions Sys.setenv(), as.POSIXct(), difftime() and ceiling(),
# and lubridate function weeks(),

Sys.setenv(tz="America/New_York")
start_date <- as.POSIXct("2015-02-09 09:30:00")
end_date <- Sys.time()

num_weeks <- ceiling(difftime(end_date, start_date, units="weeks"))

mon_days <- start_date + weeks(0:num_weeks)
mon_days <- mon_days[(mon_days <= end_date)]
head(mon_days)
tail(mon_days)

# convert "mon_days" to the days of the week, using three different methods,
# to verify that all the dates in "mon_days" are indeed Mondays,
# use function weekdays(),
weekdays(mon_days)

# use function as.POSIXlt(),
as.POSIXlt(mon_days)$wday

# use lubridate function wday(),
wday(mon_days, TRUE)



############## test
# 1. (20pts) Create a vector of daily "Dates" over weekdays, excluding weekends,
# First create a vector of daily "Dates", starting from "2014-07-14"
# until today - Sys.Date(), and call it "week_days",
# use functions as.Date(), Sys.Date(), and seq() with "by" argument,

week_days <- seq(from=as.Date("2014-07-14"), to=Sys.Date(), by="day")

# remove weekends from "week_days", using function weekdays(),

day_of_week <- weekdays(week_days)

# first method
is_weekday <- !((day_of_week == "Saturday") | (day_of_week == "Sunday"))

# second method
is_weekday <- !(day_of_week %in% c("Saturday", "Sunday"))

week_days <- week_days[is_weekday]

# call weekdays() as below, to verify that "week_days" are correct,
weekdays(head(week_days))
weekdays(tail(week_days))



############## test
# Summary: subset "zoo_series" to Mondays, and calculate weekly percentage returns

# 1. (15pts) subset "zoo_series" to Mondays,
# download the file "zoo_series.RData" from NYU Classes, and load() it,
# the file "zoo_series.RData" contains a zoo called "zoo_series",

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



############## test
# Summary: Subset daily stock prices to dates at the
# end of the week, and calculate percentage returns
# over past four week intervals.

# Download the file etf_data.RData from NYU Classes,
# and load() it.

library(quantmod)
load(file="C:/Develop/data/etf_data.RData")

# etf_data.RData contains an environment called env_data,
# with OHLC time series data for ETFs, including VTI.

# 1. (20pts) Extract the adjusted close prices from
# VTI into a variable called price_s.
# You can use function Ad() from package quantmod.

price_s <- Ad(env_data$VTI)

# Subset price_s to select prices from the dates at the
# end of each week. (copy over price_s)
# You can use function endpoints() with the "on" argument,

price_s <- price_s[endpoints(price_s, on="weeks"), ]

# Convert the tail of the index of price_s into days
# of the week, to verify that the dates of price_s are
# indeed "Friday". (except for some holidays)
# You can use functions weekdays(), index(), and tail().

weekdays(index(tail(price_s)))

# Calculate the percentage returns from price_s, over
# trailing four week intervals, and call it re_turns.
# You can use functions log() and diff() with the
# "lag" argument,

re_turns <- diff(log(price_s), lag=4)


# 2. (20pts) Extract the highest and lowest returns,
# and their associated dates.
# You can use functions index(), which(), or
# which.max() and which.min().

re_turns[which.max(re_turns)]
index(re_turns[which.max(re_turns)])
re_turns[which.min(re_turns)]
index(re_turns[which.min(re_turns)])



############## deprecated - in slides now ##############
############## hw
# 1. (15pts)
# Summary: reproduce the results of function endpoints() from
# package xts, when applied to an xts series with minutely time index.
#
# Extract a single day of close prices from "SPY" into a variable
# called price_s.
# price_s is an xts series with minutely time index.
# you can use function Cl() from package quantmod,

# load data, and if needed, change the argument to load() to your path
sym_bol <- load("C:/Develop/data/SPY.RData")
price_s <- Cl(SPY["2012-02-13"])

# define an aggregation window called win_dow, equal to 11,
# win_dow is equal to the number of periods (minutes) in each window,
# so that each aggregation window is 10 minutes long,

win_dow <- 11

# calculate the end points of the aggregation windows,
# i.e. the indices of the last observations in each aggregation
# window, using the function endpoints() from package xts as follows,
# and call it end_points,

end_points <- endpoints(price_s, on="minutes", k=win_dow)

# note that the class(end_points) returns an "integer" vector.

# calculate the maximum number of aggregation windows that fit over
# the rows of price_s,
# you can use functions nrow(), trunc(), and the "%/%" operator,

n_row <- nrow(price_s)
num_agg <- n_row %/% win_dow
# or
num_agg <- trunc(n_row/win_dow)

# now calculate the same vector of end_points without using
# the function endpoints(),
# you can use function c(), and the ":" operator,

end_points <- c(0, win_dow*(1:num_agg)-1, n_row)

# use the functions as.integer() and identical() to confirm that
# the two methods give the exact same result.
# Note that endpoints() returns an "integer" vector, while
# multiplication produces "numeric" numbers, so you must perform
# coercion before applying identical().

identical(
  endpoints(price_s, on="minutes", k=win_dow),
  as.integer(c(0, win_dow*(1:num_agg)-1, n_row)))

# a sequence of time periods ("hours", "days", "weeks", "months", etc.),
# now reproduce this:
# extract time index of the last observations in each hour
end_points <- endpoints(price_s, on="hours")

############## end deprecated ##############


############## hw - very simple adaptation of slides
# Summary: Create a function called end_marks() which
# calculates the end points of an xts series, similar
# to the function endpoints() from package xts.
#
# 1. (15pts)
# The function end_marks() should accept an argument
# called x_ts, which is an xts containing one or more
# columns of data, and also an argument called win_dow,
# which is an integer specifying the number of periods
# between the end points (i.e. length of the aggregation
# window).
# The function end_marks() should return a numeric
# vector of indices that divide x_ts into time intervals
# of length equal to win_dow.
# The indices should start with zero, and end with the
# number of rows of x_ts.
# If the number of rows of x_ts isn't an integer
# multiple of win_dow, then there should be a stub
# interval, either at the beginning or the end of the
# end points.
# The function end_marks() should also accept an argument
# called stub_begin, which is a boolean specifying
# whether the stub interval should be at the beginning
# (TRUE) or at the end (FALSE) of the end points, with
# default value equal to TRUE.
# Note that end_marks() is similar to the function
# endpoints() but produces a slightly different vector
# of indices.
# You can use functions nrow(), trunc(), c(), the "%/%"
# operator, and the ":" operator,

end_marks <- function(x_ts, win_dow, stub_begin=TRUE) {
  n_row <- nrow(x_ts)
  num_agg <- n_row %/% win_dow
  if(n_row > win_dow*num_agg) {
    if(stub_begin)
# stub interval at beginning
      c(0, n_row - win_dow*num_agg + win_dow*(0:num_agg))
    else
# stub interval at end
      c(win_dow*(0:num_agg), n_row)
  }  # end if
  else
    win_dow*(0:num_agg)
}  # end end_marks

# load data, and if needed, change the argument to load()
# equal to your path:

library(xts)
load(file="C:/Develop/data/SPY.RData")
# or
library(HighFreq)  # load package HighFreq
price_s <- Cl(SPY["2012-02-13"])

# call end_marks() as follows, to verify it works
# correctly:

end_marks(price_s, win_dow=10)
# should produce the following output:
#  [1]   0   1  11  21  31  41  51  61  71  81  91 101 111 121 131 141 151 161 171 181 191 201 211
# [24] 221 231 241 251 261 271 281 291 301 311 321 331 341 351 361 371 381 391

end_marks(price_s, win_dow=10, stub_begin=FALSE)
# should produce the following output:
#  [1]   0  10  20  30  40  50  60  70  80  90 100 110 120 130 140 150 160 170 180 190 200 210 220
# [24] 230 240 250 260 270 280 290 300 310 320 330 340 350 360 370 380 390 391



############## hw
# 1. (15pts)
# Summary: modify an lapply() loop which performs aggregations
# over an xts series, and returns an xts series as a side effect.
# Modify the below script which consists of the function agg_regate()
# and an lapply() loop.

# The function agg_regate() calculates an aggregation over
# an xts series, and produces a side effect by appending the
# aggregation to an xts series called "agg_regations"
# using the super-assignment operator "<<-".
# agg_regate() returns the end date of the input xts series.
agg_regate <- function(x_ts) {
  agg_regation <- c(max=max(x_ts), min=min(x_ts))
  agg_regation <- xts(t(agg_regation), order.by=end(x_ts))
  agg_regations <<- rbind(agg_regations, agg_regation)
  end(x_ts)
}  # end agg_regate

# load data, and if needed, change the argument to load() to your path
sym_bol <- load("C:/Develop/data/SPY.RData")
price_s <- Cl(SPY["2012-02-13"])

# extract time index of the last observations in each hour
end_points <- endpoints(price_s, on="hours")

# initialize agg_regations
agg_regations <- NULL

# perform lapply() loop over length of end_points
out_put <- lapply(2:length(end_points), function(in_dex) {
  agg_regate(price_s[(end_points[in_dex-1] + 1):end_points[in_dex]])
})  # end lapply

head(agg_regations)


# Modify the above lapply() loop, and the function agg_regate(),
# so that agg_regate() doesn't produce a side effect and
# doesn't use the operator "<<-".
# Instead, agg_regate() should return the aggregation values
# as an xts series, and the lapply() loop should return a list
# of xts series.
# Add a script that collapses the list of xts series into a single
# xts using the function do_call_rbind() from package HighFreq,
# Demonstrate that the xts series returned by do_call_rbind() is
# exactly the same as "agg_regations".
# Use function identical().

# write your code here:

# modify function agg_regate():
agg_regate <- function(x_ts) {
  xts(t(c(max=max(x_ts), min=min(x_ts))),
      order.by=end(x_ts))
}  # end agg_regate

# perform lapply() loop over length of end_points:
agg_regations <- lapply(2:length(end_points), function(in_dex) {
  agg_regate(price_s[(end_points[in_dex-1] + 1):end_points[in_dex]])
})  # end lapply

# collapse the list of xts series into a single xts
# using the function do_call_rbind() from package HighFreq:
agg_regations <- do_call_rbind(agg_regations)

# compare the xts series returned by do_call_rbind() with
# "agg_regations", using function identical():
identical(foo_bar, agg_regations)



##############################
# time series management
##############################


############## hw
# 1. (35pts) Create a function called lag_it() that applies a lag to vectors
# and "zoo" time series objects,
# The function lag_it() should accept two arguments:
#  "se_ries" a vector or "zoo" time series object,
#  "lag" an integer, specifying the number of periods to lag.
#  "lag" should have a default value of 1.
# lag_it() should first check if "lag" is an integer, and if not
# then it should produce a warning message and return NULL.
# lag_it() should next check if "se_ries" is either a vector or
# a "zoo" series, and if not then it should produce a warning
# message and return NULL.
# If both these tests pass, then lag_it() should return an object
# of the same type and dimensions as "se_ries", that is lagged
# by the number of periods specified by "lag".
# A positive "lag" should replace the present value with values
# from the past, and a negative lag should replace with values
# from the future.
# lag_it() should add NA values in place of values that are missing.
# for example, lag_it() should produce the following output:
#  lag_it(c(1:5), lag=2)
#  [1] NA NA  1  2  3
#
#  lag_it(c(1:5), lag=-2)
#  [1]  3  4  5 NA NA
#
# The default method lag() can accept a numeric vector, but it
# returns a "ts" time series object.
# This isn't what is required for this assignment.
#
# Some observations about lag.zoo():
# The method lag.zoo() returns a lagged version of a "zoo" time
# series, by shifting its time index by "k" observations,
# If "k" is positive, then lag.zoo() shifts values from the future
# to the present, and if "k" is negative then it shifts them from
# the past.
# This is the opposite of what is usually considered a positive "lag".
# A positive lag should replace the present value with values from
# the past (negative lags should replace with values from the future),
# By default lag.zoo() omits any NA values the lag may have produced,
# returning a shorter time series than the original, unless na.pad=TRUE
# is used.
# This isn't what is required for this assignment.
#
# You can use functions is.vector(), is.zoo(), is.numeric(), lag.zoo(),
# cbind(), merge(), length(), c(), rep(), warning(), and return(),

lag_it <- function(se_ries, lag=1) {
  if (!is.numeric(lag)) {  # lag is not numeric
    warning(paste("argument", deparse(substitute(lag)), "must be numeric."))
    return(NULL)  # return NULL
  }  # end if
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
# cbind(se_ries[, 1], lag(se_ries, k=-lag))[, -1]
  } else {  # se_ries is neither a vector nor a "zoo" time series
    warning(paste0("argument \"", deparse(substitute(se_ries)), "\" must be either a vector, zoo, or ts object"))
    NULL  # return NULL
  }  # end if
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



############## test
# The package "Ecdat" contains a data frame called "Cigarette".
# subset "Cigarette" to extract data only for "state"=="NY", and call it "data_ny",
library(Ecdat)  # load econometric data sets
data_ny <- Cigarette[Cigarette[, "state"]=="NY", ]

# the column dates_ny$year contains years as strings in the format "yyyy",
# from the column dates_ny$year create a vector of "Date" dates in the format "yyyy-01-01",
# and call it "dates_ny", use function paste(),
dates_ny <- as.Date(paste(data_ny$year, "-01-01", sep=""))

# Create a "zoo" from data_ny, excluding the columns "state" and "year",
# and the index "dates_ny", and call it "zoo_ny",
library(zoo)
zoo_ny <- zoo(x=data_ny[, -(1:2)], order.by=dates_ny)

# plot the column "income", and add title "Cigarette tax income in NY state",
plot(zoo_ny[, "income"], xlab="", ylab="", main="Cigarette tax income in NY state")


############## test
# Convert integers representing dates to POSIXct date-time objects,

# Load the package "Ecdat", which contains a data frame called "Yen",
# the column Yen$date contains integers representing dates, in the
# format "yyyymmdd",
# from the column Yen$date create a vector of POSIXct dates, and
# call it in_dex, and set the in_dex timezone to "UTC",
# you must perform this using two different methods,

library(Ecdat)  # load Ecdat
head(Yen)  # explore the data

# 1. (10pts)
# first method: you can use functions as.character() and as.POSIXct()
# (with a "format" argument),
# but you cannot use any function from package lubridate,

in_dex <- as.POSIXct(as.character(Yen$date), format="%Y%m%d", tz="UTC")

# 2. (10pts)
# second method: you must use function ymd() from package lubridate,

library(lubridate)
in_dex <- ymd(Yen$date, tz="UTC")

# 3. (5pts)
# Create an xts series from the column Yen$s and in_dex,
# and call it "xts_yen",

library(xts)
xts_yen <- xts(Yen$s, order.by=in_dex)

# plot "xts_yen", using generic function plot(),
plot(xts_yen)



############## hw
# Summary: The package "Ecdat" contains a data frame called "Garch".
# The column Garch$date contains dates as numeric values in the
# format "yymmdd".
# Coerce the numeric values into date-time objects.

# 1. (10pts) Create a vector of dates of class Date from Garch$date,
# and call it in_dex.
# You will need to add the century value "19" to the year.
# You can use functions paste() and as.Date(), with the proper
# "format" argument.

library(Ecdat)  # load econometric data sets
head(Garch)  # explore the data
in_dex <- as.Date(paste0(19, Garch$date), format="%Y%m%d")


# Use three different methods to create a vector of POSIXct dates
# from Garch$date, and call it in_dex.
#
# 2. (10pts) First method: create strings in the format "19yy-mm-dd",
# and then coerce them into POSIXct.
# hint: Extract substrings corresponding to the year, month, and day
# using substr(), and then combine them using paste().
# Apply function as.POSIXct() and set the timezone to "America/New_York".

in_dex <- as.POSIXct(
  paste(paste0(19, substr(Garch$date, 1, 2)),
        substr(Garch$date, 3, 4),
        substr(Garch$date, 5, 6), sep="-"),
  tz="America/New_York")


# 3. (10pts) Second method: create strings in the format "19yymmdd",
# and then coerce them into POSIXct.
# hint: Apply function as.POSIXct() with the proper "format" argument,
# and set the timezone to "America/New_York".

in_dex <- as.POSIXct(paste0(19, Garch$date), format="%Y%m%d",
                     tz="America/New_York")


# 4. (10pts) Third method: use function ymd() from package lubridate,
# and set the timezone to "America/New_York",

in_dex <- ymd(Garch$date, tz="America/New_York")


# 5. (20pts) Create an xts object called xts_series from the
# columns Garch$dm and Garch$cd, and the vector in_dex.
# Use functions c() and xts().

xts_series <- xts(Garch[, c("dm", "cd")], order.by=in_dex)

# Change the column names of xts_series to "DMark" and "CAD".
# Use functions c() and colnames().

colnames(xts_series) <- c("DMark", "CAD")

# Calculate the rolling mean of the "DMark" column of xts_series,
# over a window of 11 points in the past, and call the result dm_mean,
# Use the function runMean() from package TTR.

library(TTR)
dm_mean <- runMean(x=xts_series[, "DMark"], n=11)

# Add dm_mean as the third column of xts_series,
# using the generic function cbind().

xts_series <- cbind(xts_series, dm_mean)

# Change the third column name of xts_series to "dm_mean".
# Use function colnames().

colnames(xts_series)[3] <- "dm_mean"

# Calculate the number of NA values in xts_series.
# Use functions is.na() and sum().

sum(is.na(xts_series))

# Remove any NAs in xts_series using na.omit().

xts_series <- na.omit(xts_series)

# Subset xts_series to the dates from "1982-01-01" to "1986-01-01".

xts_series <- xts_series["1982-01-01/1986-01-01"]

############## deprecated ##############
# Use functions is.na() and sum().
# using either logical operators ">", "<", "&", "|" or function window(),
sub_index <- (in_dex > as.POSIXct("1982-01-01")) & (in_dex < as.POSIXct("1986-01-01"))
xts_series <- xts_series[sub_index]
# or
xts_series <- window(xts_series, start=as.POSIXct("1982-01-01"), end=as.POSIXct("1984-01-01"))
# end deprecated

# 6. (10pts) Plot the "DMark" and "dm_mean" columns of xts_series,
# for the years from 1984 to 1985, using the generic function plot().
# Add a legend using the function legend().

plot(xts_series["1984/1985", c("DMark", "dm_mean")])
legend(x="topleft", legend=c("DMark", "dm_mean"),
       inset=0.2, cex=0.7, bg="white",
       lwd=2, lty=c(1, 1), col=c("black", "red"))

# Save xts_series to a comma-delimited csv file called "dmcad.csv".
# Use function write.zoo().

write.zoo(xts_series, file="dmcad.csv")

# deprecated
# calculate the number of rows with bad data in xts_series,
# and then scrub the bad data using the function na.locf(),
sum(!complete.cases(xts_series))
xts_series <- na.locf(xts_series, fromLast=TRUE)

# plot xts_series with two "y" axes, and add a legend containing the column names
# of xts_series,
# plot the first column
plot(xts_series[, 1], xlab=NA, ylab=NA)
# specify range of second axis
par(usr=c(par("usr")[1:2], range(xts_series[, 2])))
# plot second axis
axis(side=4, col="red")
# plot second column
lines(xts_series[, 2], col="red")
# add legend
legend("bottomleft", legend=colnames(xts_series), bg="white", lty=c(1, 1), lwd=c(2, 2), col=c("black", "red"), bty="n")
# end deprecated



############## test - most is already incorporated into lecture slides
# Summary: subset 1-minute tick data to weekdays and trading hours.

# load packages lubridate and xts,
library(lubridate)
library(xts)

# 1. (10pts) Create a vector of 1-minute POSIXct date-times (ticks),
# and call it "min_ticks",
# starting from "2015-01-01" to "2015-01-31",
# set the POSIXct timezone to "America/New_York",
# use functions as.POSIXct() and seq.POSIXt(),
min_ticks <- seq.POSIXt(from=as.POSIXct("2015-01-01", tz="America/New_York"),
                        to=as.POSIXct("2015-01-31"), by="min")

# extract the timezone from "min_ticks" to verify that it's correct,
# use function tz() from package lubridate,
tz(min_ticks)

# Create an xts time series of rnorm data with a "min_ticks" index, and call it "xts_min_ticks",
# use function xts() from package xts,
xts_min_ticks <- xts(rnorm(length(min_ticks)), order.by=min_ticks)


# 2. (10pts) remove weekends from "xts_min_ticks",
# use function weekdays()
week_days <- weekdays(min_ticks)
is_weekday <- !((week_days == "Saturday") |
                  (week_days == "Sunday"))
xts_min_ticks <- xts_min_ticks[is_weekday]


# 3. (10pts) subset the minute ticks in "xts_min_ticks"
# to trading hours,
# use the "T notation" for specifying recurring times,
# see page 4 in xts reference manual:  "xts.pdf",
# see also answer by Joshua Ulrich here:
# http://stackoverflow.com/questions/12891232/exclude-specific-time-periods-in-r
# and by Chinmay Patil here:
# http://stackoverflow.com/questions/15284943/cut-a-posixct-by-specific-time-for-daily-means
xts_min_ticks <- xts_min_ticks["T09:30:00/T16:00:00", ]

# extract the timezone from "xts_min_ticks" to verify that it's correct,
# use function tzone() from package xts,
tzone(xts_min_ticks)



############## hw
# 1. (10pts) Read file containing date, time, and numeric data,
# the file "time_series.txt" (uploaded to NYU Classes) contains
# time series data in space-delimited format,
# the first two columns contain the date and time,
# while the third contains numeric data,
# the file also contains a header with two strings,
#
# read the data using function read.table() or scan() (read.table is easier),
# and call the resulting data frame "data_frame",
# if you're using function read.table(), then use the proper argument "skip",
# to skip over the header when reading the data.
# Use the argument "stringsAsFactors=FALSE" to avoid creating factors.
data_frame <- read.table(file="time_series.txt",
                         skip=1,
                         stringsAsFactors=FALSE)
head(data_frame)

# 2. (20pts) Formatting dates and times,
# the first two columns of "data_frame" contain strings representing the date and time,
# combine the first two columns of "data_frame" into a vector of strings,
# and call them "date_time",
# use function paste(),
date_time <- paste(data_frame[, 1], data_frame[, 2])

# convert the vector of strings into POSIXct in the UTC time zone,
# using function as.POSIXct(),
# use the proper argument "format" that is appropriate for the strings,
# read help under ?as.POSIXct to learn more about "format",
date_time <- as.POSIXct(date_time,
                        format="%m/%d/%Y %H:%M:%S",
                        tz="UTC")
head(date_time)

# 3. (20pts) Formatting the data frame and its header,
# combine "date_time" with the third column of "data_frame" into a data frame,
# and call it "data_frame",
# use function data.frame(),
data_frame <- data.frame(date_time, data_frame[, 3])

# read the first line of the file "time_series.txt" which contains the header,
# and call it "col_names",
# use function readLines(),
col_names <- readLines(con="time_series.txt", n=1)

# "col_names" contains a single string which is the concatenation
# of the header strings,
# extract the header strings from "col_names"
# using function strsplit(), with the proper argument "split",
col_names <- strsplit(col_names, split=" ")[[1]]

# assign the header strings to the column names of "data_frame"
# using function colnames(),
colnames(data_frame) <- col_names
head(data_frame)



############## hw
# Summary: Create a function which returns a list of attributes of
# a time series object. Coerce time series objects into class xts,
# and list their attributes.

# 1. (20pts) Create a function called attri_butes() which returns a list
# of attributes of a time series object.
# The function attri_butes() should accept a single input called x_ts,
# and verify that it's a time series object of either class ts, zoo, or xts,
# and if not, then it should produce and error and stop.
# hint: you can use functions stopifnot(), is.ts(), is.zoo(), and is.xts(),
# and the "||" operator.
#
# The function attri_butes() should return a named list of data
# with the following information about the input x_ts:
# - dimensions,
# - number of rows,
# - number of rows with missing values (NA),
# - number of columns,
# - column names,
# - the x_ts class,
# - the time index class of x_ts,
# - the first and last rows of x_ts,
# hint: you can use functions list(), dim(), if(), is.null(),
# nrow(), length(), sum(), complete.cases(), ncol(),
# colnames(), class(), index(), head(), and tail().

attri_butes <- function(x_ts) {
# check if argument is a time series object
  stopifnot(is.ts(x_ts) || is.zoo(x_ts) || is.xts(x_ts))
# create list and return it
  list(
    dim=dim(x_ts),
    nrows=if(is.null(nrow(x_ts))) length(x_ts) else nrow(x_ts),
    nrows_nas=sum(!complete.cases(x_ts)),
    ncols=if(is.null(ncol(x_ts))) 1 else ncol(x_ts),
    col_names=if(is.null(colnames(x_ts))) "none" else colnames(x_ts),
    ts_class=class(x_ts),
    ts_index_class=class(index(x_ts)),
    first_row=head(x_ts, 1),
    last_row=tail(x_ts, 1)
  )  # end list
}  # end attri_butes


# 2. (10pts) Coerce the EuStockMarkets time series into class xts,
# and call it xts_eustocks.
# The time index of xts_eustocks should be of class POSIXct, and
# the timezone should be equal to "America/New_York".
# hint: you can use functions coredata(), xts(), index(),
# and date_decimal().

library(lubridate)  # load lubridate
xts_eustocks <- xts(coredata(EuStockMarkets),
                    order.by=date_decimal(index(EuStockMarkets),
                                          tz="America/New_York"))

# plot all 4 columns of xts_eustocks in a single panel:

plot(xts_eustocks)


# call attri_butes() as follows, to verify that it works correctly:

attri_butes(EuStockMarkets)
attri_butes(EuStockMarkets[, 1])
attri_butes(xts_eustocks)
attri_butes(xts_eustocks[, 1])



############## test
# Summary: Calculate the second moment of all the columns of EuStockMarkets,
# using the functions sapply() and moment(),

# Calculate percentage returns of EuStockMarkets,

rets_series <- 100*diff(log(EuStockMarkets))

# Load package "moments",
library(moments)

# 6. (10pts) Calculate the second moment of all the columns of rets_series, using the functions sapply() and moment(),
# Pass in the "order" (=2) and "na.rm" (=TRUE) parameters to function moment(),

sapply(X=as.data.frame(rets_series), FUN=moment, order=2, na.rm=TRUE)



############## test
# Summary: calculate the first four moments of all four time series
# in the EuStockMarkets data (DAX, SMI, CAC, FTSE),

# comment:
# In general, the solution requires performing two loops: one over
# the columns of EuStockMarkets, and another loop over moment orders.
# The two loops can be combinations of for() and apply() loops,
# so that several slightly different solutions are possible.

# Your script should produce a 4x4 matrix containing all 16 moments,
# with proper row and column names.
# Your script should use iteration, instead of manually repeating
# the same calculation for each index,
# You can use functions for(), apply(), and sapply(),
# you must use function moment() from package "moments",

# function moment() accepts an argument "x", representing the numeric
# vector of data,
# and it also accepts an argument "order", representing the order of
# the moment to be computed.
# You must iterate over the columns of EuStockMarkets data, and pass
# them to the argument "x" of function moment(),
# You must also iterate over the moment orders, and pass them to
# the argument "order",

# 30pts
# Calculate percentage returns of EuStockMarkets.

ts_rets <- diff(log(EuStockMarkets))

# Load package "moments".
library(moments)

# create 4x1 matrix of moment orders - for using in apply() loop
moment_orders <- as.matrix(1:4)

# create vector of moment orders - for using in sapply() loop
or_ders <- 1:4

# first solution: perform two sapply loops

eu_moments <- sapply(colnames(ts_rets),
                     FUN=function(col_name) {
                       sapply(or_ders, FUN=moment, x=ts_rets[, col_name])
                       # or instead you could use apply:
                       # apply(X=moment_orders, MARGIN=1, FUN=moment, x=ts_rets[, col_name])
                     }  # end anonymous function
)  # end sapply


# second solution: perform one for() loop, and one sapply loop
# first allocate the matrix "eu_moments",

eu_moments <- matrix(numeric(16), ncol=ncol(ts_rets))

# perform sapply() loop nested in for() loop,

for (col_num in seq(ncol(ts_rets))) {
  eu_moments[, col_num] <- sapply(or_ders, FUN=moment, x=ts_rets[, col_num])
# or instead you could use apply:
# eu_moments[, col_num] <-
#   apply(X=moment_orders, MARGIN=1, FUN=moment, x=ts_rets[, col_num])
}  # end for


# add column names equal to the column names of EuStockMarkets data,
# add row names equal to "moment1", "moment2", etc.
# You can use functions colnames(), rownames(), and paste(),

colnames(eu_moments) <- colnames(EuStockMarkets)
rownames(eu_moments) <- paste0("moment", or_ders)




##############################
# data input output munging scrubbing data error handling
##############################

############## hw idea
# download two series: daily and monthly
# cbind the series and remove NAs



############## hw
# Summary: Perform aggregations on a data frame using 
# the split-apply-combine procedure. 
# Download the latest version of the file expressions.pdf 
# from NYU Classes, and follow the examples on slide #32. 

# 1. (10pts) Download the file "CRSPpanel.txt" from NYU 
# Classes. 
# The file "CRSPpanel.txt" contains a data frame with 
# a single day of panel data. The panel data contains 
# fundamental financial data for 265 S&P500 stocks. 
# Read the file into a data frame called panel_data using 
# read.table(), with the "header" and "sep" arguments. 

panel_data <- read.table(file="C:/Develop/data/CRSPpanel.txt", 
                         header=TRUE, sep="\t")

# You should get the following output for panel_data:
# > dim(panel_data)
# [1] 265  48
# 
# > panel_data[1:3, 1:4]
#       DATE PERMNO    CUSIP     COMPANY.NAME
# 1 20031231  26403 25468710   DISNEY WALT CO
# 2 20031231  89525 20030N10 COMCAST CORP NEW
# 3 20031231  66181 43707610   HOME DEPOT INC

# Coerce the Industry column of panel_data from a factor 
# into a vector called indus_tries. 
# Coerce the Sector column of panel_data from a factor 
# into a vector called sec_tors. 
# You can use function as.vector(). 

indus_tries <- as.vector(panel_data$Industry)
sec_tors <- as.vector(panel_data$Sector)

# You should get the following output:
# > head(unique(indus_tries))
# [1] "Media"                         "Retailing"                     "Hotels Restaurants & Leisure"
# [4] "Automobiles & Components"      "Consumer Durables & Apparel"   "Household & Personal Products"
# 
# > head(unique(sec_tors))
# [1] "Consumer Discretionary" "Consumer Staples"   "Industrials"   "Energy"
# [5] "Utilities"              "Materials"


# 2. (20pts) Each Industry belongs to a single Sector. 
# Calculate a named vector of Industries with the Sectors 
# to which they belong, and call it industry_sectors. 
# The names of industry_sectors should be the Industries, 
# and the strings should be the Sectors to which they 
# belong.
# You must perform the calculation in two different ways. 
# In the first method you must use function tapply(), 
# and you can also use functions drop(), as.matrix(), 
# unique(), and an anonymous function. 
# hint: you can use the vectors sec_tors and indus_tries.

industry_sectors <- tapply(X=sec_tors, INDEX=indus_tries, FUN=unique)
# or
industry_sectors <- tapply(X=sec_tors, INDEX=panel_data$Industry, FUN=unique)
# or
industry_sectors <- tapply(X=sec_tors, INDEX=panel_data$Industry, FUN=function(x) x[1])

# tapply() returns an array which you must coerce into 
# a named vector of strings (not factors!). 
# You can coerce industry_sectors into a named vector 
# using function sapply(), and an anonymous function. 
# hint: you can perform an sapply() loop over the array.
# You can instead use functions drop() and as.matrix(). 

industry_sectors <- structure(as.vector(industry_sectors), names=names(industry_sectors))
# or: 
industry_sectors <- sapply(industry_sectors, function(x) x)
# or: 
industry_sectors <- drop(as.matrix(industry_sectors))


# You should get the following output, with names of Industries 
# without quotes, and names of Sectors with quotes: 
# 
# > industry_sectors
# Automobiles & Components                    Capital Goods 
# "Consumer Discretionary"                    "Industrials" 
# Commercial & Professional Services          Consumer Durables & Apparel 
# "Industrials"                              "Consumer Discretionary" 


# In the second method you must use function sapply(). 
# You cannot use tapply(). 
# You can also use function match(), and an anonymous 
# function. 
# hint: you can use the vectors sec_tors and indus_tries.
# hint: you can either perform an sapply() loop over the 
# levels of panel_data$Industry, or over unique elements 
# of indus_tries. 

industry_sectors_bis <- sapply(unique(indus_tries), 
                               function(x) {
                                 sec_tors[x==indus_tries][1]
                               })  # end sapply
# or:
industry_sectors_bis <- sapply(levels(panel_data$Industry), 
                               function(x) {
                                 sec_tors[match(x, panel_data$Industry)]
                               }, USE.NAMES=TRUE)


# Verify that both methods produce the same Industry 
# to Sector mappings, even though the vectors may be 
# permuted with respect to each other. 
# You must use function identical(). 
# You can also use function names(). 

identical(industry_sectors, 
          industry_sectors_bis[names(industry_sectors)])



# 3. (20pts) Each Sector has one or more Industries that 
# belong to it. 
# Calculate a named list (or array) of Sectors with all 
# the Industries belonging to them, and call it 
# sector_industries. 
# You can either use functions tapply() or sapply(). 
# You can also use functions as.vector(), unique(), 
# and an anonymous function. 
# hint: you can use the vectors sec_tors and indus_tries.

sector_industries <- tapply(X=indus_tries, 
                            INDEX=sec_tors, FUN=unique)
# or: 
sector_industries <- tapply(X=indus_tries, 
                            INDEX=panel_data$Sector, FUN=unique)
# or: 
sector_industries <- tapply(X=as.vector(panel_data$Industry), 
                            INDEX=panel_data$Sector, FUN=unique)
# or: 
sector_industries <- sapply(unique(sec_tors), 
                            function(x) {
                              unique(indus_tries[x==sec_tors])
                            })  # end sapply
# or: 
sector_industries <- sapply(levels(panel_data$Sector), 
                            function(x) {
                              unique(indus_tries[x==panel_data$Sector])
                            })  # end sapply


# You should get the following output, with names of Sectors 
# as list element names, and names of Industries with quotes: 
# 
# > sector_industries
# $`Consumer Discretionary`
# [1] "Media"                        "Retailing"                  "Hotels Restaurants & Leisure"
# [4] "Automobiles & Components"     "Consumer Durables & Apparel" 
# 
# $`Consumer Staples`
# [1] "Household & Personal Products" "Food, Beverage & Tobacco"    "Food & Drug Retailing"        


# 4. (20pts) Calculate a named list with the stock tickers 
# of companies in each industry, and call it industry_tickers. 
# You must perform the calculation in two different ways. 
# In the first method you must use function tapply(), and 
# you can also use functions as.vector(), and an anonymous 
# function. 
# hint: you can use the vector indus_tries.

industry_tickers <- tapply(X=as.vector(panel_data$TICKER), 
                           INDEX=panel_data$Industry, FUN=function(x) x)
# or: 
industry_tickers <- tapply(X=as.vector(panel_data$TICKER), 
                           INDEX=indus_tries, FUN=function(x) x)

# tapply() returns an array which you must coerce into 
# a named list of vectors of strings (not factors!). 
# You can coerce industry_tickers into a named list using 
# function lapply(), and an anonymous function. 
# hint: you can perform an lapply() loop over the array.
# You can instead use functions drop() and as.matrix(). 

industry_tickers <- lapply(industry_tickers, function(x) x)
# or: 
industry_tickers <- drop(as.matrix(industry_tickers))

# You should get the following output, with names 
# of Industries as the list element names:
# 
# > industry_tickers
# $`Automobiles & Components`
# [1] "JCI" "BWA"
# 
# $`Capital Goods`
# [1] "BA"   "MMM"  "HON"  "EMR"  "DHR"  "CMI"  "ETN"  "LMT"  "PCP"  "ITW"  "GD"   "RTN"  "NOC"  "GWW"  "PH"
# [16] "ROK"  "DOV"  "IR"   "FLR"  "TYC"  "FAST" "PNR"  "ROP"  "COL"  "PLL"  "LLL"  "MAS"  "JEC"


# In the second method you must use function sapply(). 
# You cannot use tapply(). 
# You can also use the functions levels(), unique(), 
# as.vector(), and an anonymous function. 
# hint: you can use the vector indus_tries.
# hint: you can either perform an sapply() loop over the 
# levels of panel_data$Industry, or over unique elements 
# of indus_tries. 

industry_tickers <- sapply(unique(indus_tries), 
                           function(x) {
                             as.vector(panel_data$TICKER)[x==indus_tries]
                           })  # end sapply
# or: 
industry_tickers <- sapply(levels(panel_data$Industry), 
                           function(x) {
                             as.vector(panel_data$TICKER)[x==indus_tries]
                           })  # end sapply


# 5. (20pts) Calculate a named vector with the number of 
# companies in each Industry. 
# You can use functions sapply() and length(). 
# hint: you can use the vector industry_tickers.

sapply(industry_tickers, length)

# You should get the following output, with names 
# of Industries as the element names:
# 
# Automobiles & Components                Capital Goods
#             2                                 28
# Commercial & Professional Services      Consumer Durables & Apparel
#             8                                 12


# Calculate a named list with the indices of companies in 
# each industry, and call it industry_indices. 
# The index of a company is its row number in panel_data.
# You can use functions sapply() and match(). 
# hint: you can use the vector industry_tickers.

industry_indices <- sapply(industry_tickers, function(x) {
  match(x, panel_data$TICKER)
})  # end sapply

# You should get the following output, with names 
# of Industries as the element names: 
# 
# > industry_indices
# $`Automobiles & Components`
# [1]  9 21
# 
# $`Capital Goods`
# [1] 139 141 142 144 145 146 147 148 150 151 153 155 156 157 158 160 161 162 163 164 165 166 167 168 170 177 178
# [28] 179



# 6. (10pts) Calculate a named vector (not an array!) with 
# the average "NET.INCOME" of all the companies in each Sector. 
# You can use functions tapply(), sapply(), with(), mean(), 
# unique(), levels(), and an anonymous function. 
# hint: you can use the vector sec_tors. 

with(panel_data, 
     sapply(tapply(X=NET.INCOME, INDEX=Sector, FUN=mean), 
            function(x) x))
# or: 
sapply(
  tapply(X=panel_data$NET.INCOME, INDEX=sec_tors, FUN=mean), 
  function(x) x)
# or: 
sapply(unique(sec_tors), 
       function(x) {
         mean(panel_data$NET.INCOME[x==sec_tors])
       })  # end sapply
# or: 
sapply(levels(panel_data$Sector), 
       function(x) {
         mean(panel_data$NET.INCOME[x==panel_data$Sector])
       })  # end sapply

# You should get the following output: 
# Consumer Discretionary    Consumer Staples    Energy    Financials
#   477.3523                  1033.5947       1542.6779     666.9847
# Health Care   Industrials     Information Technology    Materials
# 898.7432        490.6571            861.5079            216.4407
# Telecommunication Services      Utilities
#         1762.2357               229.8667


# 7. (20pts) Calculate a data frame (not an array!) of 
# companies that have the highest ROE in each Industry, 
# and call it max_roes. 
# The data frame row names should be the Industry names, 
# the first column should be the tickers of companies with 
# the highest ROE, and the second column should be their 
# ROE. 
# You must perform the calculation in two different ways. 
# In the first method you must use function tapply(), and 
# you can also use functions as.vector(), match(), max() 
# and with(). 

# hint: You can first perform a tapply() loop over the 
# Industry column, to get the max ROEs, and then use 
# match() to get the tickers. 

max_roes <- with(panel_data, tapply(X=ROE, INDEX=Industry, FUN=max))
tick_ers <- with(panel_data, as.vector(TICKER)[match(max_roes, ROE)])
max_roes <- data.frame(ticker=tick_ers, roe=max_roes)


# In the second method you must use function sapply(). 
# You cannot use tapply(). 
# You can also use the functions which.max(), max(), 
# with(), list(), as.vector(), data.frame(), unlist(), 
# and an anonymous function. 
# hint: You can use the vector indus_tries.
# hint: You can first perform an sapply() loop over 
# industry_indices, and then pass the output to 
# data.frame(). 

max_roes <- with(panel_data, sapply(industry_indices, function(x) {
  list(as.vector(TICKER)[x[which.max(ROE[x])]], max(ROE[x]))
}))  # end sapply
max_roes <- data.frame(ticker=unlist(max_roes[1, ]), roe=unlist(max_roes[2, ]))

# You should get the following output: 
# > max_roes
#                                           ticker      roe
# Automobiles & Components                    JCI 0.16025626
# Capital Goods                               COL 0.30972389
# Commercial & Professional Services          PBI 0.45809675
# Consumer Durables & Apparel                 COH 0.34344821
# Diversified Financials                      LUK 0.04547642


# some more stuff for the future:

plot(panel_data[, "Industry"], li_st[, "BOOK2MARKET"])

# names of factor columns
fac_tors <- colnames(panel_data)[sapply(panel_data, is.factor)]

# coerce factor columns
panel_data[, fac_tors] <- sapply(fac_tors, function(fac_tor) {
  as.vector(panel_data[, fac_tor])
})

foo <- split(panel_data[, c("Industry", "Sector")], panel_data$Sector)
dim(foo[[1]])
unique((foo[[1]])$Industry)




############## hw factorAnalytics in progress
# Summary: Perform sorts on the data frame stock, from factorAnalytics 
# containing monthly panel data 
# find unique tickers
# create time series
# regress on fundamental ratios

# file Stock.df.RData
# Create a data frame of value ETFs.
# Add an investment style field to the data frame of ETFs.

# 1. (20pts) Download the file "file Stock.df.RData" from NYU Classes,
# and read it into a data frame called etf_list using read.csv().
# etf_list is a database of ETFs.
# Use the argument "stringsAsFactors=FALSE" to avoid creating
# factors.
# stock (Stock.df)           Fundamental and return data for 447 NYSE stocks
# Fundamental and return data: Assets: 447 stocks listed on the NYSE 
# Frequency: Monthly Date, range: 1996-02-29 through 2003-12-31

library(factorAnalytics)
data(Stock.df)
dim(stock)
colnames(stock)
stock[1:5, 1:5]
class_es <- sapply(stock, class)
foo <- unique(stock)
foo <- stock[stock[, "DATE"]==stock[1, "DATE"], ]
dim(foo)
head(foo)
hist(foo[, "MARKET.EQUITY"])
hist(foo[, "MARKET.EQUITY"], breaks=300, xlim=c(-0.5, 1))
hist(foo[, "PRICE"], breaks=50, xlim=c(-3, 3))
plot(foo[, "NET.SALES"], foo[, "NET.INCOME"])
plot(foo[, "LTDEBT"], foo[, "BOOK2MARKET"])



# Load the file etf_data.RData, which contains the
# environment env_etf, containing ETF time series
# and other ETF data.

load(file="C:/Develop/data/etf_data.RData")




############## end factorAnalytics ##############



############## test
# Summary: Create a data frame of value ETFs.
# Add an investment style field to the data frame of ETFs.

# 1. (20pts) Download the file "etf_list.csv" from NYU Classes,
# and read it into a data frame called etf_list using read.csv().
# etf_list is a database of ETFs.
# Use the argument "stringsAsFactors=FALSE" to avoid creating
# factors.

etf_list <- read.csv(file='etf_list.csv', stringsAsFactors=FALSE)

# Extract from etf_list a data frame of ETFs whose "Name"
# field contains the keyword "Value", and call it value_etfs.
# For example, if the "Name" field is equal to:
#  "Vanguard Small-Cap Value ETF"
# then that ETF should be selected into value_etfs.
# You can use functions grep() and glob2rx().
# Look into the file data_structures.pdf to find
# examples of using functions grep() and glob2rx().

value_etfs <- etf_list[grep(glob2rx("*Value*"), etf_list$Name), ]

# Save value_etfs to a comma-delimited CSV file called
# "value_etfs.csv".
# Use function write.csv(),

write.csv(value_etfs, file="value_etfs.csv")


# 2. (20pts) Add a field (column) to etf_list called "style",
# and set it equal to NA.

etf_list$style <- NA

# Set the "style" field equal to "Value" for all the ETFs in
# the etf_list, that are contained in the value_etfs data frame.
# You can use data frame subsetting and the %in% operator.

etf_list[etf_list$Symbol %in% value_etfs$Symbol, ]$style <- "Value"



############## test
# Summary: List the class and dimension attributes
# of objects in an environment.

# Load the file etf_data.RData, which contains the
# environment env_etf, containing ETF time series
# and other ETF data.

load(file="C:/Develop/data/etf_data.RData")

# 1. (10pts) List the names of all the objects in the
# environment env_etf, and save the names in a vector
# of strings called sym_bols.
# You can use function ls().

sym_bols <- ls(env_etf)

# Create a list with the class attributes of all the
# objects in the environment env_etf, and call it
# class_es.
# You can use functions eapply() and class().

class_es <- eapply(env_etf, class)


# 2. (30pts) List the names of all the objects of
# class xts in the environment env_etf, and save the
# names in a vector of strings called sym_bols.
# hint: first calculate a named boolean vector that
# is TRUE for objects of class xts, then extract the
# names of those objects.
# You can use functions eapply(), is.xts(), unlist(),
# and names().
# Or you can use sapply() instead of eapply(), and
# an anonymous function, and the "%in%" operator.

is_xts <- unlist(eapply(env_etf, is.xts))
# or
is_xts <- sapply(class_es, function(x) "xts" %in% x)
sym_bols <- names(is_xts[is_xts])


# 3. (30pts) Create a matrix called dimen_sions,
# containing the dimensions of all the xts objects
# in env_etf.
# You can use the functions eapply(), dim(), rbind(),
# and do.call().
# Or you can use lapply() and an anonymous function,
# instead of eapply().
# hint: eapply() returns a list, and you must flatten
# the list into a matrix using rbind() and do.call().

dimen_sions <- do.call(rbind, eapply(env_etf, dim)[sym_bols])
# or
dimen_sions <- do.call(rbind, lapply(sym_bols,
                                     function(x_ts)
                                       dim(get(x_ts, env_etf))))

# You should get the following output:
#   dimen_sions
#           [,1] [,2]
# DBC       2338    6
# VNQ       2338    6
# price_s   2338   20
# XLB       2338    6
# VTI       2338    6
# etc.



############## test
# Summary: List and manipulate objects with given
# name patterns, in an environment.

# Load the file etf_data.RData, which contains the
# environment env_etf, containing ETF time series
# and other ETF data.

load(file="C:/Develop/data/etf_data.RData")

# 1. (10pts) List the names of all the objects in 
# env_etf whose names start with "X*".
# You can use the functions glob2rx() and ls() with
# the "pattern" argument.

ls(env_etf, pattern=glob2rx("X*"))


# 2. (10pts) Remove all the objects in env_etf
# whose names start with "X*".
# You can use the functions rm(), glob2rx() and
# ls() with the "pattern" argument.

rm(list=ls(env_etf, pattern=glob2rx("X*")),
   envir=env_etf)



############## hw
# Summary: calculate intraday seasonality of trading volumes in high
# frequency data.
# Intraday seasonality means how the average trading volumes change at
# different times of the day,

# load one minute bar data for the SPY ETF from the file SPY.RData.
# The SPY data is an xts series containing OHLC prices and trading volumes
# in one minute bars.
# load package xts,

rm(list=ls())
library(xts)
load(file="C:/Develop/data/SPY.RData")
# or
library(HighFreq)  # load package HighFreq


# Calculate the average trading volumes for every minute bar,
# by aggregating the volumes for the same minute in all the days.
# There are several methods of performing the aggregations, and you
# must apply two different methods.


# 1. (20pts) In the first method, perform aggregations using the
# function tapply().

# Subset the SPY data to the month "2013-05" and call it
# spy_sample, this will reduce the time of future calculations.

# define sym_bol
sym_bol <- "SPY"
spy_sample <- get(sym_bol)["2013-05"]

# if you get a warning message: 
# "timezone of object (America/New_York) is different than current timezone"
# You can suppress it by calling:

options(xts_check_TZ=FALSE)

# Extract the time index of spy_sample, and format it as a vector
# of strings representing hours and minutes, and call it in_dex,
# You can use functions index() and format().

in_dex <- format(index(spy_sample), "%H:%M")

# perform a tapply() loop over the spy_sample data column "SPY.Volume",
# using in_dex and function mean(),
# call the output "vol_ume".

vol_ume <- tapply(X=Vo(spy_sample), INDEX=in_dex, FUN=mean)

# coerce "vol_ume" to a named vector of length equal to the number of
# minutes in a single day of spy_sample data,
# the names should be equal to the hours and minutes, in the
# format "hours:minutes" (like "10:28"),
# You can use functions as.vector(), names(), and structure(),

vol_ume <- structure(as.vector(vol_ume), names=names(vol_ume))
is.vector(vol_ume)
head(vol_ume)


# 2. (20pts) in the second method, first subset in_dex to the number
# of minutes in a single day of spy_sample data,
# and call the output vector "time_of_day",
# "time_of_day" should be a vector of strings representing hours and
# minutes, like "10:28",
# You can use in_dex and the function unique(),

time_of_day <- unique(in_dex)

# perform an lapply() loop over the spy_sample data column "SPY.Volume",
# using "time_of_day", the "==" logical operator, an anonymous function,
# and the function mean(),
# call the output "vol_ume".

vol_ume <- lapply(time_of_day, function(mi_nute) {
  mean(spy_sample[in_dex==mi_nute, "SPY.Volume"])
})  # end lapply

# coerce "vol_ume" to a named vector of length equal to the number of
# minutes in a single day of spy_sample data,
# You can use "time_of_day", and the functions unlist() and structure(),

vol_ume <- structure(unlist(vol_ume), names=time_of_day)


# 3. (10pts) benchmark the speed of the two methods of aggregating volumes,
# use function microbenchmark() from package microbenchmark.
# You can adapt code from the "numerical_analysis" pdf file.

library(microbenchmark)
summary(microbenchmark(
  t_apply=
    tapply(X=Vo(spy_sample), INDEX=in_dex, FUN=mean),
  l_apply=
    lapply(time_of_day, function(mi_nute) {
      mean(Vo(spy_sample)[in_dex==mi_nute])
    }),
  times=10)
)[, c(1, 4, 5)]


# 4. (20pts) plot "vol_ume" using the base graphics package,
# with a custom x-axis, and with tick marks at hourly points.
# You can adapt code from the slide titled:
# "Plotting zoo Time Series With Custom x-axis"
# in the "plotting" pdf file.

# convert the "time_of_day" to strings corresponding to hours,
# like "10:00", "11:00", and call the output "hour_s",
# "hour_s" should have the same length as "time_of_day",
# You can use function substring(),

hour_s <- substring(time_of_day, 1, 2)

# extract from "time_of_day" the strings corresponding to hours,
# call the output "tick_s",
# "tick_s" should be a vector of strings like "10:00", "11:00",
# You can use "hour_s" and functions unique() and match(),

tick_s <- time_of_day[match(unique(hour_s), hour_s)]

# plot "vol_ume" with a custom x-axis, with tick marks at hourly points,
# add vertical grey lines at tick marks,
# You can use "tick_s" and functions plot(), axis(), and abline(),

a_t <- match(x=tick_s, table=time_of_day)
plot(vol_ume, xaxt="n", xlab=NA, ylab=NA, t="l")
axis(side=1, at=a_t, labels=tick_s, tcl=-0.7)
abline(v=a_t, col="grey", lwd=0.5)


# 5. (20pts) plot "vol_ume" using chart_Series() from package quantmod,
# with a custom x-axis, with tick marks at different times of day.

# coerce "vol_ume" to an xts series, with time index equal to "time_of_day".
# of length equal to the number of
# minutes in a single day of spy_sample data,
# You can use "time_of_day", and the functions unlist() and structure(),

# create a time index from today's date combined with "time_of_day",
# and call it in_dex.
# You can use functions paste(), Sys.Date(), and as.POSIXct(),

in_dex <- as.POSIXct(paste(Sys.Date(), time_of_day))

# create an xts series from the vector "vol_ume" and in_dex,
# and call it "vol_ume".
# use function xts(),

vol_ume <- xts(x=vol_ume, order.by=in_dex)

# load package quantmod.
# Extract the plot theme and call it plot_theme.

library(quantmod)
plot_theme <- chart_theme()

# Modify plot_theme, so the x-axis tick marks are in the
# format "%H:%M",
# use function chart_theme(), and change field $format.labels,

plot_theme$format.labels <- "%H:%M"

# Create a plot object for "vol_ume", and call it "ch_ob".
# use function chart_Series(), with arguments theme=plot_theme and
# plot=FALSE,

ch_ob <- chart_Series(x=vol_ume, theme=plot_theme, plot=FALSE)

# Modify the plot object "ch_ob", to reduce the range of the y-axis.
# extract the y-axis range from the "ch_ob" object, and call it "y_lim".
# use function ch_ob$get_ylim(),

y_lim <- ch_ob$get_ylim()

# modify "y_lim", to reduce the range of the y-axis.
# "y_lim" is a list, with y_lim[[2]] equal to a vector containing
# the range of the y-axis combined with the attribute "fixed".
# Create an object similar to y_lim[[2]], but with its second element
# equal to the second element of y_lim[[2]] divided by half,
# You can use function structure(), with attribute "fixed=TRUE".
# Assign this object back to y_lim[[2]] (overwrite it),

y_lim[[2]] <- structure(c(y_lim[[2]][1], y_lim[[2]][2]/2), fixed=TRUE)

# Modify the plot object "ch_ob", using the modified y_lim object,
# use function ch_ob$set_ylim(),

ch_ob$set_ylim(y_lim)

# Create the plot by calling the "ch_ob" object,

ch_ob
# or
plot(ch_ob)



############## hw
# Summary: download data for multiple symbols using
# get.hist.quote().
# Create a data directory on your computer, and save
# all files to that directory.
# Remember the location of the data directory for
# future use.

# load package tseries:
library(tseries)

# 1. (5pts) Download the file "etf_list.csv" from NYU Classes,
# and read it into a data frame called etf_list using read.csv().
# etf_list is a database of ETFs.
# Use the argument "stringsAsFactors=FALSE" to avoid creating
# factors.

etf_list <- read.csv(file='etf_list.csv', stringsAsFactors=FALSE)


# 2. (5pts) create a vector of symbols called sym_bols,
sym_bols <- c("VTI", "VEU", "IEF", "VNQ", "DBC", "XLY", "XLP", "XLE", "XLF", "XLV", "XLI", "XLB", "XLK", "XLU", "IWB", "IWD", "IWF", "IWM", "IWN", "IWO", "IWP", "IWR", "IWS", "IWV", "IUSV", "IUSG")
# subset etf_list to include only those ETF's in sym_bols, using the "%in%" operator,
etf_list <- etf_list[etf_list$Symbol %in% sym_bols, ]

# 3. (15pts) download 10yrs of price and volume data for the
# list of sym_bols, and call it "zoo_series",
# for each symbol download the fields "AdjClose" and "Volume",
field_names <- c("AdjClose", "Volume")

# use get.hist.quote() and an lapply() loop,
# name the list returned by lapply as "zoo_series"
# (each list element is a zoo object).
zoo_series <- suppressWarnings(
  lapply(sym_bols, # loop for loading data
         get.hist.quote,
         quote=field_names,
         start=Sys.Date()-3650,
         end=Sys.Date(),
         origin="1970-01-01")
)  # end suppressWarnings

# 4. (5pts) assign a names() attribute to zoo_series, equal to the sym_bols vector (result should be a named list),
names(zoo_series) <- sym_bols

# 5. (5pts) flatten zoo_series into a single zoo object,
# use functions do.call() and merge(),
zoo_series <- do.call(merge, zoo_series)

# 6. (5pts) assign new column names to zoo_series,
# in the format "symbol.Close", "symbol.Volume", etc.
# use colnames(), sapply() and paste(),
colnames(zoo_series) <- as.vector(sapply(sym_bols, paste, c("Close", "Volume"), sep="."))

# 7. (5pts) save zoo_series to a comma-separated CSV file called "zoo_series.csv",
# use function write.zoo(),
write.zoo(zoo_series, file="zoo_series.csv", sep=",")

# 8. (5pts) save zoo_series to a binary file called "zoo_series.RData", using save(),
save(zoo_series, file="zoo_series.RData")

# 9. (10pts) plot the time series zoo_series[, "VTI.Close"] using autoplot(),
etf_gg <- autoplot(zoo_series[, "VTI.Close"],
                   main="Vanguard Total Stock Market ETF") +
  xlab("") + ylab("") +
  theme(
    legend.position=c(0.1, 0.5),
    plot.title=element_text(vjust=-2.0),
    plot.background=element_blank()
  )  # end theme
# render ggplot
etf_gg



############## test
# 1. (10pts) Perform an sapply() loop to calculate a named
# vector containing the number of NA values in the columns
# of etf_rets.
# You can use functions sum(), is.na(), and sapply().

library(quantmod)
load(file="C:/Develop/data/etf_data.RData")
sapply(etf_rets, function(x_ts) sum(is.na(x_ts)))

# You should get the following output:
# VTI VEU IEF VNQ DBC VXX XLY XLP XLE XLF XLV XLI XLB XLK XLU VYM IVW IWB IWD IWF
#   0  45   0   0   0 524   0   0   0   0   0   0   0   0   0   0   0   0   0   0



############## hw
# Summary: Create a function which calculates a vector
# of hypothesis test statistics.
# Perform an sapply() loop to calculate a matrix of
# statistics for a vector of symbols.

# Download the file etf_data.RData from NYU Classes,
# and load() it.
# etf_data.RData contains an environment called env_data,
# with OHLC time series data for ETFs.

library(quantmod)
load(file="C:/Develop/data/etf_data.RData")


############## deprecated ##############

# perform an eapply() loop to extract the adjusted prices for all
# the variables in env_data, and call it etf_series_ad,
etf_series_ad <- do.call(merge, eapply(env_data, Ad))

# etf_series_ad should be an xts series containing adjusted prices,
# with colnames in the format "name.Adjusted",
# rename the colnames and drop ".Adjusted" from the colnames,
# use functions sapply() and strsplit(),
colnames(etf_series_ad) <- sapply(colnames(etf_series_ad),
                                  function(col_name) strsplit(col_name, split="[.]")[[1]])[1, ]


# scrub (remove) rows with NA values from etf_series_ad,
# use function complete.cases(),
etf_series_ad <- etf_series_ad[complete.cases(etf_series_ad)]


# calculate an xts series containing returns of etf_series_ad, and call it etf_rets,
# use functions lapply(), dailyReturn(), do.call(), and merge(),
# load package quantmod,
library(quantmod)
etf_rets <- lapply(etf_series_ad,
                   function(x_ts) {
                     daily_return <- dailyReturn(x_ts)
                     colnames(daily_return) <- names(x_ts)
                     daily_return
                   })  # end lapply

# flatten list of xts series into a single xts series,
etf_rets <- do.call(merge, etf_rets)

############## end deprecated ##############


# 1. (20pts) Create a function called get_hyp_stats(),
# that returns a vector with hypothesis test statistics.
# The function get_hyp_stats() should accept a single
# argument called re_turns, an xts series containing
# returns data.
#
# The function get_hyp_stats() should perform the
# Jarque-Bera and the Shapiro-Wilk tests of normality
# on re_turns, and return a named vector containing
# the statistics (not the p-values!),
# You must use the functions jarque.bera.test() and
# shapiro.test().
# Be careful because shapiro.test() doesn't accept
# arguments of class xts, so you must first coerce
# it into a matrix using function coredata().
# You can use the function unname() to strip the
# names from the values returned by the functions
# jarque.bera.test() and shapiro.test().

# load package tseries
library(tseries)

get_hyp_stats <- function(re_turns) {
# load package tseries, if it's not loaded already
  stopifnot("package:tseries" %in% search() || require("tseries", quietly=TRUE))
  c(
    jarque_bera=unname(jarque.bera.test(re_turns)$statistic),
    shapiro=unname(shapiro.test(coredata(re_turns))$statistic))
}  # end get_hyp_stats

# apply get_hyp_stats() as follows, to verify it works correctly:
get_hyp_stats(etf_rets[, 1])

# get_hyp_stats(etf_rets[, 1]) should produce this:
# jarque_bera      shapiro
# 9008.6452123    0.8979459


# 2. (20pts) Perform an sapply() loop over the columns
# of etf_rets, and apply get_hyp_stats() to the columns
# of etf_rets, and call the output matrix hyp_stats.
# The first column of hyp_stats should contain the
# Jarque-Bera statistics, while the second should
# contain the Shapiro-Wilk statistics. 
# The rownames of hyp_stats should contain the column 
# names in etf_rets. 
# You can use functions sapply(), na.omit(), and t().
# Be careful because some columns of etf_rets contain
# NA values, so you must pass them through na.omit(). 

hyp_stats <- sapply(etf_rets,
                    function(x_ts)
                      get_hyp_stats(na.omit(x_ts)))
hyp_stats <- t(hyp_stats)

# You should get the following output:
#     jarque_bera   shapiro
# VTI   9008.6452 0.8979459
# VEU   7442.9831 0.8977588
# IEF    415.6686 0.9861641
# VNQ  12500.0078 0.8367968
# DBC    610.5074 0.9704973
# VXX    684.3048 0.9589765
# etc.


# 3. (10pts) Create a scatterplot of hyp_stats, and
# add labels containing the rownames of hyp_stats.
# Use functions plot() and text(),

plot(hyp_stats)
text(x=hyp_stats[, "jarque_bera"],
     y=hyp_stats[, "shapiro"],
     labels=rownames(hyp_stats),
     pos=1, cex=0.8)


# 4. (10pts) Sort hyp_stats on column "jarque_bera"
# in ascending (increasing) order.
# Use function order(),

hyp_stats <- hyp_stats[order(hyp_stats[, "jarque_bera"],
                             decreasing=FALSE), ]

# save hyp_stats to a comma-delimited CSV file.
# Use function write.csv(),

write.csv(hyp_stats, file="hyp_stats.csv")



############## hw
# Summary: Create a function that extracts the price and
# volume columns from OHLC data, and perform a single
# lapply() loop to extract the price and volume columns
# from all time series contained in env_data.

# Download the file etf_data.RData from NYU Classes,
# and load() it.
# etf_data.RData contains an environment called env_data,
# with OHLC time series data for ETFs, including VTI.

library(quantmod)
load(file="C:/Develop/data/etf_data.RData")

# 1. (10pts) Create a function called ex_tract(), that
# extracts the adjusted price and volume columns from an
# OHLC data series, and returns an xts series with two
# columns.
# You can use functions merge(), Ad(), and Vo(),

ex_tract <- function(x_ts) merge(Ad(x_ts), Vo(x_ts))

# Apply function ex_tract() to a single xts series, to verify
# it works correctly:

da_ta <- ex_tract(env_data$VTI)
head(da_ta)


# 2. (20pts) Create a vector of symbols, called sym_bols,

sym_bols <- c("DBC", "VTI", "IEF")

# Perform an lapply() loop over a subset of env_data
# containing sym_bols, and call the function ex_tract()
# on each element in the subset, and call the output da_ta.
# da_ta should be a list of xts series, with each xts series
# containing price and volume data for a single symbol.
# You can use functions as.list(), get(), ex_tract(), and lapply().
# There are at least two different ways of performing this,
# and either way is good.

da_ta <- lapply(as.list(env_data)[sym_bols], ex_tract)
# or
da_ta <- lapply(sym_bols,
                function(sym_bol)
                  ex_tract(get(sym_bol, envir=env_data)))

# Flatten da_ta into a single xts series, and call it da_ta.
# You can use functions do.call() and merge(),

da_ta <- do.call(merge, da_ta)



############## test
# Summary: Create a function that extracts columns from an
# OHLC data series contained in an environment, performs
# calculations on the columns, and saves the result in an
# xts series in a different environment.

# Download the file etf_data.RData from NYU Classes,
# and load() it.
# etf_data.RData contains an environment called env_data,
# with OHLC time series data for ETFs, including VTI.

library(quantmod)
load(file="C:/Develop/data/etf_data.RData")

# 1. (30pts) Create a function called ex_tract(), that accepts
# three arguments:
# - "sym_bol": a string representing the name of an OHLC xts series,
# - "in_env": an input environment containing the xts series,
# - "out_env": an output environment for saving the output xts series,
#
# The function ex_tract() should:
# - extract adjusted prices and volume data from the xts series,
# - merge prices with volume data into a single xts series,
# - assign() (copy) the xts series to the "out_env" environment,
# - invisibly return the string "sym_bol".
# ex_tract() should produce the side effect of creating the
# output xts series in the "out_env" environment.
#
# Note that ex_tract() only receives a string representing the 
# name of an xts series, not the name itself, so you must use 
# the function get() to get the data, and the function assign() 
# to assign it (not "<-").
# You can also use functions merge(), invisible(), Ad(), 
# and Vo(). 

ex_tract <- function(sym_bol, in_env, out_env) {
  x_ts <- get(sym_bol, envir=in_env)
  x_ts <- merge(Ad(x_ts), Vo(x_ts))
  assign(sym_bol, x_ts, envir=out_env)
  invisible(sym_bol)
}  # end ex_tract

# Create a new environment called env_out. 
# Use function new.env().

env_out <- new.env()

# Apply function ex_tract() to the string "VTI" and 
# env_out, to verify it works correctly:

ex_tract("VTI", in_env=env_data, out_env=env_out)

# Also run these commands to verify that a new object 
# was created:

ls(env_out)
head(env_out$VTI)

# You should get the following output:
# > head(env_out$VTI)
#             VTI.Adjusted VTI.Volume
# 2007-01-03     58.17359     798600
# 2007-01-04     58.28998    3305000
# 2007-01-05     57.82858     382000
# 2007-01-08     58.04057     299000
# 2007-01-09     58.04057     267000
# 2007-01-10     58.16943     359200


# 2. (10pts) Remove all the objects in env_etf
# whose names start with "V*".
# You can use the functions rm(), glob2rx() and
# ls() with the "pattern" argument.

rm(list=ls(env_out, pattern=glob2rx("V*")),
   envir=env_out)


# 3. (10pts) Create a vector of strings, 
# called sym_bols. 

sym_bols <- c("DBC", "VTI", "IEF")

# Perform an sapply() loop over sym_bols, to 
# apply the function ex_tract() to all the strings 
# in sym_bols. 
# To get full credit you must pass the arguments 
# "in_env=env_data" and "out_env=env_out" into 
# ex_tract() through the dots argument of the 
# sapply() function. 

sapply(sym_bols, 
       ex_tract, 
       in_env=env_data, 
       out_env=env_out)



############## hw
# Summary: create a function that extracts price and volume
# columns from OHLC time series contained in an environment,
# calculates returns, and saves the returns and volume as
# an xts series in a different environment.

# Download the file etf_data.RData from NYU Classes,
# and load() it.
# etf_data.RData contains an environment called env_data,
# with OHLC time series data for ETFs.

library(quantmod)
load(file="C:/Develop/data/etf_data.RData")


# 1. (20pts) create a function called get_returns_volume(),
# that accepts an xts series argument (x_ts) and an environment
# argument ("envir"),
# get_returns_volume() should:
# - extract adjusted prices and volume data from the xts series,
# - calculate returns from adjusted prices,
# - extract the symbol name from the columns of xts series (symbol_name),
# - merge returns with volume data into a single xts series (return_volume),
# - rename the colnames of return_volume to "symbol_name.Return"
#   and "symbol_name.Volume", (replace symbol_name with the symbol name),
# - assign (copy) return_volume to a object named "symbol_name_rets"
#   in the "envir" environment,
# get_returns_volume() should produce the side effect of creating
# an xts series in the "envir" environment containing returns and
# volume data "from the input x_ts,
# get_returns_volume() should return invisibly the symbol_name,
# You can use functions Ad(), Vo(), strsplit(), colnames(),
# paste() (or paste0), assign(), invisible(), and dailyReturn(),
get_returns_volume <- function(x_ts, envir=env_data) {
  re_turn <- dailyReturn(Ad(x_ts))
  vol_ume <- Vo(x_ts)
  symbol_name <- strsplit(colnames(vol_ume), split="[.]")[[1]][1]
  return_volume <- merge(re_turn, vol_ume)
  colnames(return_volume) <-
    c(paste0(symbol_name, ".Return"), paste0(symbol_name, ".Volume"))
  assign(paste0(symbol_name, "_rets"), return_volume, envir=envir)
  invisible(symbol_name)
}  # end get_returns_volume


# 2. (20pts) create a new environment called "env_returns",
# for storing xts series containing stock return and volume data,
# use function new.env(),
env_returns <- new.env()

# apply function get_returns_volume() to a single xts series, to verify
# it works correctly:
get_returns_volume(env_data$VTI, envir=env_returns)


# perform an eapply() loop to apply get_returns_volume() to all
# the objects in env_data, and copy to "env_returns",
# use functions get_returns_volume() and eapply(),
eapply(env_data, get_returns_volume, envir=env_returns)

# remove all files from "env_returns",
rm(list=ls(env_returns), envir=env_returns)

# perform a for() loop to apply get_returns_volume() to all
# the objects in env_data, and copy to "env_returns",
# use functions get_returns_volume() and for(),
for (x_ts in ls(env_data)) {
  get_returns_volume(get(x=x_ts, envir=env_data), envir=env_returns)
}  # end for

# save all the objects in the environment "env_returns"
# to a binary file called "etf_rets_volume.RData",
# use function save(), with the "list" and "envir" arguments,
# make sure to save the objects in the environment,
# not the environment itself,

save(list=ls(env_returns),
     envir=env_returns,
     file="etf_rets_volume.RData")



##############################
# time series analysis stochastic processes
##############################


############## hw idea
# Summary: Create functions that calculate the maximum 
# drawdown, and the Sortino and Calmar ratios. 
# The functions should replicate table.Drawdowns(), 
# SortinoRatio(), and CalmarRatio(). 


############## test
# Summary: Perform an sapply() loop over the columns 
# of etf_rets, and calculate the Sortino and Calmar 
# ratios, and the max drawdowns. 

# Download the file etf_data.RData from NYU Classes,
# and load() it.
# etf_data.RData contains an environment called env_data,
# with OHLC time series data for ETFs.

library(quantmod)
load(file="C:/Develop/data/etf_data.RData")

# create a vector of symbols called sym_bols,

sym_bols <- c("VTI", "VEU", "IEF", "DBC")

# 1. (20pts) Perform an sapply() loop over the columns 
# of etf_rets, subset by sym_bols.  Call the output 
# matrix etf_stats. 
# Inside the loop calculate the Sortino and Calmar 
# ratios, and the max drawdowns. 
# You should use functions sapply(), SortinoRatio(), 
# CalmarRatio(), table.Drawdowns() (column "Depth"),
# and an anonymous function. 
# The anonymous function should return a named vector 
# with the data. 

library(PerformanceAnalytics)
etf_stats <- sapply(etf_rets[, sym_bols],
                    function(x_ts) {
                      c(sor_tino=SortinoRatio(x_ts),
                        cal_mar=CalmarRatio(x_ts),
                        draw_down=table.Drawdowns(x_ts)[1, "Depth"])
                    })  # end sapply

# You should get the following output:
# t(etf_stats)
#       sor_tino     cal_mar  draw_down
# VTI  0.03615478  0.12059340   -0.5545
# VEU  0.01469526  0.01527280   -0.6152
# IEF  0.07903142  0.59006446   -0.1040
# DBC -0.01453087 -0.07397729   -0.7402



############## hw - already in lecture slides
# 1. (20pts) create a function called roll_sum() which calculates the
# rolling sum of an xts series over a lookback window.
# The function roll_sum() should accept two arguments:
#  x_ts - an xts series containing one or more columns of data,
#  win_dow - an integer specifying the number of points in the
#   lookback window,
# roll_sum() should return an xts series with the same dimensions
# as the input x_ts series, and with column names derived from the
# x_ts column names, with "_stdev" appended to the end.
# For example, if win_dow=3, then the rolling sum at any point should
# be equal to the sum of x_ts values for that point plus two
# preceding points.
# The initial values of roll_sum() from 1 to win_dow should be equal to
# the cumsum() values, so that roll_sum() doesn't return any initial
# NA values.
# You must use use vectorized functions, such as cumsum() and lag(),
# not apply() or for() loops,

roll_sum <- function(x_ts, win_dow) {
  cum_sum <- cumsum(na.omit(x_ts))
  out_put <- cum_sum - lag(x=cum_sum, k=win_dow)
  out_put[1:win_dow, ] <- cum_sum[1:win_dow, ]
  colnames(out_put) <- paste0(colnames(x_ts), "_stdev")
  out_put
}  # end roll_sum

# Download the file etf_data.RData from NYU Classes,
# and load() it.
# etf_data.RData contains an environment called env_data,
# with OHLC time series data for ETFs, including VTI.

load(file="C:/Develop/data/etf_data.RData")

# call roll_sum() as follows, to verify it works correctly,

win_dow <- 11
da_ta <- env_data$VTI[, "VTI.Volume"]
foo <- roll_sum(x_ts=da_ta, win_dow=win_dow)
head(foo)

# benchmark the speed of roll_sum() with function runSum()
# from package TTR, and rollsum() from package zoo.
# Use function microbenchmark() from package microbenchmark.
# You can adapt code from the "numerical_analysis" pdf and R files.
# Be sure to download the most recent version.

library(TTR)
library(microbenchmark)

summary(microbenchmark(
  fre=roll_sum(x_ts=da_ta, win_dow=win_dow),
  TTR=runSum(x=da_ta, n=win_dow),
  zoo=rollsum(x=da_ta, k=win_dow, align="left"),
  times=10))[, c(1, 4, 5)]


############## deprecated ##############
############## hw - can't work using vectorized functions
# 1. (20pts) Create a function called roll_sd() which calculates
# the rolling standard deviation of an xts series over a lookback
# window.
# The function roll_sd() should accept two arguments:
# - x_ts - an xts series containing one or more columns of data,
# - win_dow - an integer specifying the number of points in the
#   lookback window,
# roll_sd() should return an xts series with the same dimensions
# as the input x_ts series, and column names derived from the
# x_ts column names, with "_stdev" appended to the end.
# For example, if win_dow=3, then the rolling standard deviation
# at any point should be equal to the square root of the sum of
# squared values of x_ts for that point plus two preceding
# points divided by (win_dow-1).
# The initial values of roll_sd() from 1 to win_dow should be
# calculated over an expanding window, so that roll_sd() doesn't
# return any initial NA values.
# You must use use vectorized functions, such as cumsum() and
# lag(), not apply() or for() loops,
# hint: you can follow the example of function roll_sum(),
# you can also call roll_sum() directly inside roll_sd().

# this can't work using vectorized functions, because subtracting
# the mean can't be performed without performing a loop.
roll_sd <- function(x_ts, win_dow) {
  x_ts <- na.omit(x_ts)
  roll_mean <- roll_sum(x_ts, win_dow=win_dow)/win_dow
  x_ts <- x_ts - roll_mean
  cum_sum <- cumsum(x_ts^2)
  out_put <- cum_sum - lag(x=cum_sum, k=win_dow)
  out_put[1:win_dow, ] <- cum_sum[1:win_dow, ]
  colnames(out_put) <- paste0(colnames(x_ts), "_stdev")
  sqrt(out_put/(win_dow-1))
}  # end roll_sd

# or simply:

roll_sd <- function(x_ts, win_dow) {
  out_put <- sqrt(roll_sum(x_ts=x_ts^2, win_dow=win_dow)/(win_dow-1))
  colnames(out_put) <- paste0(colnames(x_ts), "_stdev")
  out_put
}  # end roll_sd

# Download the file etf_data.RData from NYU Classes,
# and load() it.
# etf_data.RData contains an environment called env_data,
# with OHLC time series data for ETFs, including VTI.

load(file="C:/Develop/data/etf_data.RData")

# call roll_sd() as follows, to verify it works correctly:

library(quantmod)
re_turns <- dailyReturn(Ad(env_data$VTI))
# or
re_turns <- diff(log(Ad(env_data$VTI)))
win_dow <- 11
foo <- roll_sd(x_ts=re_turns, win_dow=win_dow)
head(foo)
tail(foo)


# Create a function called apply_sd(), similar to roll_sd(),
# which produces the exact same output as roll_sd(), but uses
# an apply() loop and the function sd().

apply_sd <- function(x_ts, win_dow) {
# define end points
  end_points <- 0:nrow(x_ts)
  len_gth <- length(end_points)
# define starting points as lag of end_points
  start_points <-  end_points[
    c(rep_len(1, win_dow), 1:(len_gth-win_dow))] + 1
# perform aggregations over length of end_points
  out_put <- sapply(2:len_gth,
                    function(in_dex) {
                      sapply(
                        .subset_xts(x_ts, start_points[in_dex]:end_points[in_dex]),
                        sd)  # end sapply
                      })  # end sapply
# coerce out_put into matrix and transpose it
  if (is.vector(out_put))
    out_put <- t(out_put)
  out_put <- t(out_put)
# coerce out_put into xts series
  out_put <- xts(out_put,
                 order.by=index(x_ts[end_points]))
  colnames(out_put) <- paste0(colnames(x_ts), "_stdev")
  out_put
}  # end apply_sd

foo <- apply_sd(x_ts=re_turns, win_dow=win_dow)
head(foo)
tail(foo)

# Benchmark the speed of roll_sd() with function runSD()
# from package TTR.
# Use function microbenchmark() from package microbenchmark.
# You can adapt code from the "numerical_analysis" pdf and R files.
# Be sure to download the most recent version.

library(TTR)
library(microbenchmark)

summary(microbenchmark(
  fre=roll_sd(x_ts=re_turns, win_dow=win_dow),
  TTR=runSD(x=re_turns, n=win_dow),
  times=10))[, c(1, 4, 5)]

############## end deprecated ##############



############## hw
# Summary: Create a function which calculates the volume-
# weighted average price, benchmark it to function VWAP()
# from package TTR, and create plots.

# 1. (20pts) Create a function called v_wap() which
# calculates the volume-weighted average price over a
# rolling lookback window.
# The volume-weighted average price (VWAP) over a time
# interval is defined as the sum of the prices multiplied
# by the trading volumes, divided by the total trading
# volume in that interval.
# The function v_wap() should accept two arguments:
#  - x_ts - an xts series containing OHLC prices and
#    trading volumes for a single asset,
#  - win_dow - an integer specifying the number of
#    periods in the lookback window.
# v_wap() should return an xts series with a single
# column containing the VWAP of the adjusted close prices.
# The output xts series should have the same number of
# rows as the input xts series,
# v_wap() should not return any NA values.
# v_wap() should be set to zero if the total volume
# is zero.
# You can use functions Ad(), Vo() and roll_sum().
# You can also use functions paste0(), strsplit(),
# and colnames(), to change the column name.
# The function roll_sum() was defined in the lecture
# slides.

library(quantmod)

v_wap <- function(x_ts, win_dow) {
  vol_wap <- roll_sum(x_ts=Ad(x_ts)*Vo(x_ts), win_dow=win_dow)
  vol_ume <- roll_sum(x_ts=Vo(x_ts), win_dow=win_dow)
  vol_wap <- vol_wap/vol_ume
  vol_wap[is.na(vol_wap) | (vol_ume==0)] <- 0
  colnames(vol_wap) <-
    paste0(strsplit(colnames(vol_wap), split="[.]")[[1]][1], ".vwap")
  vol_wap
}  # end v_wap

# Download the file etf_data.RData from NYU Classes,
# and load() it.
# etf_data.RData contains an environment called env_data,
# with OHLC time series data for ETFs, including VTI.

load(file="C:/Develop/data/etf_data.RData")

# calculate the VWAP of VTI using v_wap()
# with "win_dow=11", to verify it works correctly.

foo <- v_wap(x_ts=env_data$VTI, win_dow=11)

# You should get the following output, including
# the column name:
# head(foo)
#            VTI.vwap
# 2007-01-03 58.17359
# 2007-01-04 58.26733
# 2007-01-05 58.22996
# 2007-01-08 58.21813
# 2007-01-09 58.20874
# 2007-01-10 58.20613


# 2. (10pts) Calculate the same VWAP using function VWAP()
# from package TTR, and convert any NA values to zero.
# Find which elements are significantly different between
# the two results (absolute difference is greater than 0.01).
# You can use functions abs(), is.na(), merge(), and "[]"
# subsetting,

library(TTR)
bar <- VWAP(price=Ad(env_data$VTI),
            volume=Vo(env_data$VTI), n = 11)
bar[is.na(bar)] <- 0
is_diff <- abs(foo-bar) > 0.01
cbind(foo, bar)[is_diff]

# You should get the following output:
#
#            VTI.vwap VWAP
# 2007-01-03 58.17359    0
# 2007-01-04 58.26733    0
# 2007-01-05 58.22996    0
# 2007-01-08 58.21813    0
# 2007-01-09 58.20874    0
# 2007-01-10 58.20613    0
# 2007-01-11 58.22977    0
# 2007-01-12 58.27334    0
# 2007-01-16 58.31010    0
# 2007-01-17 58.34027    0


# 3. (10pts) Benchmark the speed of v_wap() compared to
# function VWAP() from package TTR.
# Which function is faster?
# Use function microbenchmark() from package microbenchmark.
# You can adapt code from the lecture slides.

library(microbenchmark)
summary(microbenchmark(
  v_wap=v_wap(x_ts=env_data$VTI, win_dow=11),
  ttr=VWAP(price=Ad(env_data$VTI), volume=Vo(env_data$VTI), n = 11),
  times=10))[, c(1, 4, 5)]


# 4. (10pts) Plot the adjusted close price of VTI for the year 2008
# only, together with its VWAP, in a single panel on the same plot.
# You must use functions Ad(), chart_Series() and add_TA().

chart_Series(x=Ad(env_data$VTI)["2008"],
             name="VTI plus VWAP")
add_TA(foo["2008"], col="blue", lwd=2, on=1)
# or
chart_Series(x=Ad(env_data$VTI)["2008"],
             TA="add_TA(foo, on=1)",
             name="VTI plus VWAP")



############## hw
# Summary: Calculate the maximum drawdown of a time series.

############## deprecated ##############

# first method: create synthetic xts for calculating maximum drawdown
# load packages lubridate, xts, and tseries,
library(xts)

# Create a "Date" vector of 100 daily dates, starting from "2015-01-04", and call it in_dex,
# use functions as.Date() and seq() with "by" argument,
in_dex <- seq(from=as.Date("2015-01-04"), by="day", length.out=100)

# Extract the class from in_dex to verify that it is "Date" class,
class(in_dex)

# Create a vector of data of length in_dex as follows:
da_ta <- sin(0.1*(1:length(in_dex))) + (1:length(in_dex))/50

# Create an xts time series with the da_ta and the in_dex, and call it xts_series,
# use function xts() from package xts,
xts_series <- xts(da_ta, order.by=in_dex)

# second method for getting time series: load "zoo_data.RData"
# 1. (15pts)
# download the file "zoo_data.RData" from NYU Classes, and load() it,
# the file "zoo_data.RData" includes a zoo series called "zoo_stx",
# containing MSFT stock OHLC data.
# extract the "AdjClose" prices from "zoo_stx" into a variable
# called "msft_prices".

load(file="C:/Develop/data/zoo_data.RData")
msft_prices <- zoo_stx[, "AdjClose"]

# plot "msft_prices", using generic function plot(),

plot(msft_prices)

############## end deprecated ##############

# Download the file etf_data.RData from NYU Classes,
# and load() it.
# etf_data.RData contains an environment called env_data,
# with OHLC time series data for ETFs, including VTI.

load(file="C:/Develop/data/etf_data.RData")

# 1. (10pts) Extract the adjusted close prices from VTI
# into a variable called price_s.
# You can use function Ad() from package quantmod.

price_s <- Ad(env_data$VTI)

# The cumulative maximum of a price series is the maximum
# price in the past, reached up to that point in time.
# Calculate the cumulative maximum of price_s using
# function cummax().
# Plot the cumulative maximum of price_s using function
# chart_Series().

chart_Series(x=cummax(price_s),
             name="Cumulative maximum prices")

# A drawdown is a drop in price from its previous maximum.
# Calculate the xts time series of drawdowns of price_s,
# as the difference between price_s minus the cumulative
# maximum of price_s, and call it draw_down.

draw_down <- price_s - cummax(price_s)

# plot draw_down using function chart_Series().

chart_Series(x=draw_down, name="drawdowns")

# Find the date when draw_down reaches its minimum, and
# call it date_trough, and find the minimum value of
# draw_down on that date, and call it max_drawdown.
# You can use functions index() and which.min().

in_dex <- index(price_s)
date_trough <- in_dex[which.min(draw_down)]
max_drawdown <- draw_down[date_trough]

# Add a vertical red line to the draw_down plot,
# at the date date_trough.
# hint: use function match() and index() to first
# calculate the index of date_trough.
# You can use functions match(), index(), and abline(),

abline(v=match(date_trough, index(draw_down)), col="red")


# 2. (10pts) Divide draw_down into two time series at
# the date date_trough.
# First subset draw_down to dates before date_trough,
# and call it pre_drawdown,

pre_drawdown <- draw_down[in_dex<date_trough]

# Next subset draw_down to dates after date_trough,
# and call it post_drawdown,

post_drawdown <- draw_down[in_dex>date_trough]

# Now find the date when the drawdown period starts.
# The drawdown starts when draw_down is first zero
# and then starts decreasing to some price below zero.
# Find the latest date when pre_drawdown was still
# equal to zero, and call it date_from.
# date_from is when the drawdown started.
# You can use functions index() and max().

date_from <- max((index(pre_drawdown))[pre_drawdown==0])

# Now find the date when the drawdown period ends.
# When the current price exceeds the previous maximum
# price, then draw_down returns back to zero, and the
# drawdown period is over.
# A drawdown ends when draw_down first returns back
# to zero after date_trough.
# Find the first date when post_drawdown returns back
# to zero, and call it date_to.
# date_to is when the drawdown has ended.
# You can use functions index() and min(),

date_to <- min((index(post_drawdown))[post_drawdown==0])


# 3. (10pts) Combine the three dates: date_from,
# date_trough, and date_to into a named vector with
# names "from", "trough", and "to", and call it
# drawdown_dates,

drawdown_dates <- c(from=date_from, trough=date_trough, to=date_to)
# or
names(drawdown_dates) <- c("from", "trough", "to")

# You should get the following output:
# drawdown_dates
#         from       trough           to
# "2007-10-09" "2009-03-09" "2012-03-13"

# Plot price_s using function chart_Series().

chart_Series(x=price_s, name="VTI drawdown dates")

# Add vertical green, red, and blue lines for the
# three dates: date_from, date_trough, date_to.
# Add text at the vertical lines equal to
# names(drawdown_dates).
# hint: use function match() and index() to first
# calculate the index of drawdown_dates.
# You can use functions match(), index(), abline(),
# and text().

abline(v=match(drawdown_dates, index(draw_down)),
       col=c("blue", "red", "green"))
text(x=match(drawdown_dates, index(draw_down)),
     y=as.vector(price_s[drawdown_dates]),
     labels=names(drawdown_dates), pos=3, cex=0.8)


# 4. (10pts) Create a function called max_drawdown() which
# calculates the maximum drawdown of a time series of prices.
# max_drawdown() should accept a single argument called x_ts,
# which should be an xts time series of prices (not returns).
# max_drawdown() should return a named vector of three dates:
# date_from, date_trough, and date_to, with names "from",
# "trough", and "to".
# hint: you can reuse the scripts from the previous parts.

max_drawdown <- function(x_ts) {
  draw_down <- x_ts - cummax(x_ts)
  in_dex <- index(x_ts)
  date_trough <- in_dex[which.min(draw_down)]
  pre_drawdown <- draw_down[in_dex<date_trough]
  post_drawdown <- draw_down[in_dex>date_trough]
  date_from <- max((index(pre_drawdown))[pre_drawdown==0])
  date_to <- min((index(post_drawdown))[post_drawdown==0])
  c(from=date_from, trough=date_trough, to=date_to)
}  # end max_drawdown

# call max_drawdown() with the argument price_s, to verify
# it works correctly.

max_drawdown(price_s)

# You should get the following output:
# max_drawdown(price_s)
#         from       trough           to
# "2007-10-09" "2009-03-09" "2012-03-13"

# Calculate the drawdowns of price_s using function
# table.Drawdowns() from package PerformanceAnalytics.
# and compare the output to max_drawdown().
# hint: use function dailyReturn() to first calculate
# returns from price_s.

library(PerformanceAnalytics)
table.Drawdowns(dailyReturn(price_s))



############## hw - add microbenchmark
# 1. (20pts) simulate an ARIMA AR(1) process, and call it "ts_arima",
# use function arima.sim(),
# the length of the series should be 100,
# the "model" argument should specify an ARIMA AR(1) process,
# with a single coefficient equal to 0.5,
# set the burn-in interval to zero, by specifying the "start.innov"
# argument equal to a vector of zeroes of length 100,
# first reset the random number generator by calling set.seed(1121),

set.seed(1121)
per_iods <- 100
phi <- 0.5
ts_arima <- arima.sim(n=per_iods,
                      model=list(ar=phi),
                      start.innov=rep(0, per_iods))

# simulate the same AR(1) process as above, but now recursively,
# by calculating current returns from previous returns in a for() loop,
# and call it "ts_arima_loop",
# use functions rnorm() and for(),
# you should obtain the same "ts_arima" series as above,
# remember to reset the random number generator by calling set.seed(1121),
# use function numeric() to pre-allocate the vector "ts_arima" before
# the loop starts,
set.seed(1121)
ts_arima_loop <- numeric(per_iods)
ts_arima_loop[1] <- rnorm(1)
for(in_dex in 2:per_iods) {
  ts_arima_loop[in_dex] <- phi*ts_arima_loop[in_dex-1] + rnorm(1)
}

# use function as.ts() to coerce "ts_arima_loop" to a "ts" time series,
ts_arima_loop <- as.ts(ts_arima_loop)


# use the function identical() to confirm that the two methods give
# the exact same result, and that "ts_arima" and "ts_arima_loop"
# are identical,
identical(ts_arima, ts_arima_loop)


######
# simulate the same AR(1) process as above recursively, using functions rnorm() and filter(),
# use method="recursive" in filter(),
# you should obtain the same "ts_arima" series as above,
# remember to reset the random number generator by calling set.seed(1121),
set.seed(1121)
ts_arima <- as.ts(rnorm(per_iods))
ts_arima <- filter(x=ts_arima, filter=phi, method="recursive")



############## hw
# calculate autocorrelations and partial autocorrelations by hand
# create ARIMA time series of class "ts"
set.seed(1121)
zoo_arima <- zoo(
  x=arima.sim(n=1000, model=list(ar=c(0.2, 0.3))),
  order.by=(Sys.Date()-0:999))

# calculate autocorrelations using acf()
vec_acf <- drop(acf(zoo_arima, lag=5, plot=FALSE)$acf)

# create ARIMA time series pure vector
zoo_arima <- coredata(arima.sim(n=1000, model=list(ar=c(0.2, 0.3))))

# calculate autocorrelations by hand
# first apply lag to time series
zoo_arima_lag <- zoo_arima
zoo_arima_lag <- zoo_arima_lag[-length(zoo_arima_lag)]
zoo_arima <- zoo_arima[-1]
head(cbind(zoo_arima, zoo_arima_lag))
tail(cbind(zoo_arima, zoo_arima_lag))
# sqrt(sum(zoo_arima_lag^2)/length(zoo_arima_lag))
# sum((zoo_arima-mean(zoo_arima))*(zoo_arima_lag-mean(zoo_arima_lag)))/(sd(zoo_arima-mean(zoo_arima))*sd(zoo_arima_lag))/(length(zoo_arima)-1)

# autocorrelation equal to cor of time series with its lag
auto_corr <- cor(zoo_arima, zoo_arima_lag)

# create time series that's not correlated with the lagged series,
# but doesn't mean its not correlated with its own lag,
zoo_arima_1 <- zoo_arima - auto_corr*sd(zoo_arima)*zoo_arima_lag/sd(zoo_arima_lag)
zoo_arima_1 <- zoo_arima - vec_acf[2]*sd(zoo_arima)*zoo_arima_lag/sd(zoo_arima_lag)

cor(zoo_arima_lag, zoo_arima_1)


##############
# 2. (20pts) Calculate partial autocorrelation by hand,
# simulate AR(2) process recursively by hand,

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


##############
# 3. (20pts) Perform PCA as follows:
etf_pca <- prcomp(etf_rets, center=TRUE, scale=TRUE)

# etf_rets is an xts time series of ETF returns,
# the principal component vectors are contained in the following matrix:
etf_pca$rotation
# the time series of principal component returns are contained in the following matrix:
etf_pca$x


# 2. (30pts) calculate the time series of principal component returns from etf_pca$rotation and etf_rets,
# hint: you need to first scale etf_rets using function "scale",
# compare your calculated PCA returns to etf_pca$x, to make sure they are exactly the same,
etf_rets_scaled <- apply(etf_rets, 2, scale)
pca_rets <- etf_rets_scaled %*% etf_pca$rotation
head(cbind(etf_pca$x[, 2], pca_rets[, 2]))


# 3. (5pts) convert the PCA returns matrix to a xts time series,
# and rescale (divide) them by 100, so they are decimals, not percentages,
library(xts)
pca_rets <- xts(pca_rets/100, order.by=index(etf_rets))


# 4. (5pts) calculate CAPM Summary Statistics for the first three PCA returns,
library(PerformanceAnalytics)
table.CAPM(Ra=pca_rets[, 1:3], Rb=etf_rets[, "VTI"], scale=252)



############## hw
# Summary: Calculate the volume-weighted average price,
# find its crossing points with prices, and plot it.

# Download the file etf_data.RData from NYU Classes,
# and load() it.
# etf_data.RData contains an environment called env_data,
# with OHLC time series data for ETFs, including VTI.

library(quantmod)
load(file="C:/Develop/data/etf_data.RData")


# 1. (10pts) Extract the adjusted close prices from
# VTI into a variable called price_s.
# You can use function Ad() from package quantmod.

price_s <- Ad(env_data$VTI)

# Rename the column name of price_s to "VTI", by 
# dropping ".Adjusted" from the colnames. 
# Use must function strsplit(). 

colnames(price_s) <- strsplit(colnames(price_s), split="[.]")[[1]][1]

# Calculate the 50-day moving average of price_s,
# and merge it with price_s by adding it as the
# last column.
# Rename the last column to "VWAP".
# You must use function VWAP() from package TTR.

price_s <- merge(price_s, 
    VWAP(price=price_s, volume=Vo(env_data$VTI), n=50))

colnames(price_s)[2] <- "VWAP"

# At this point price_s should be like this:
# tail(price_s)
#               VTI     VWAP
# 2016-04-08 104.20  99.28782
# 2016-04-11 103.91  99.45781
# 2016-04-12 104.92  99.58764
# 2016-04-13 106.15  99.74678
# 2016-04-14 106.11  99.89194
# 2016-04-15 106.06 100.07063

# Calculate a boolean xts series that is TRUE for 
# dates when VTI prices are above their VWAP, and 
# FALSE otherwise, and call it ma_indic. 

ma_indic <- price_s[, 1] > price_s[, 2]


# 2. (10pts) Plot both columns of price_s in the 
# same panel, from "2015-05-01" to "2015-08-01", 
# with custom line colors black and red.
# You must use functions chart_theme(), chart_Series(),
# and you can also use add_TA().

x11()
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("black", "red")
chart_Series(x=price_s["2015-05-01/2015-08-01"],
             name="VTI plus VWAP",
             theme=plot_theme)

# or
plot_theme <- chart_theme()
plot_theme$col$line.col <- "black"
chart_Series(x=price_s["2015-05-01/2015-08-01", 1],
             name="VTI plus VWAP",
             theme=plot_theme)
add_TA(price_s["2015-05-01/2015-08-01", 2], on=1, lwd=2, col="red")

# Add background shading, with "lightgreen" color 
# for dates when the prices are above their VWAP, 
# and "lightgrey" when they're below their VWAP.  
# Add a legend.
# You must use functions add_TA() and legend().

add_TA(ma_indic["2015-05-01/2015-08-01"], on=-1,
       col="lightgreen", border="lightgreen")
add_TA(!ma_indic["2015-05-01/2015-08-01"], on=-1,
       col="lightgrey", border="lightgrey")

# add legend
legend("top", inset=0.05, cex=0.8,title=NULL,
       leg=c("VTI", "VWAP"), lwd=2, bg="white",
       col=c("black", "red"))


# 3. (10pts) Calculate an xts series of dates when 
# the prices cross their VWAP, and call it ma_crosses.
# hint: You can use ma_indic to calculate ma_crosses.
# You can use the functions diff(), abs(), index(),
# and the logical operator ">".

ma_crosses <- (abs(diff(ma_indic)) > 0)
ma_crosses <- index(price_s[ma_crosses])

# You should get the following output:
# head(ma_crosses)
# [1] "2007-03-21" "2007-06-25" "2007-06-27" "2007-06-29" "2007-07-02" "2007-07-24"
# tail(ma_crosses)
# [1] "2015-12-28" "2015-12-29" "2015-12-31" "2016-02-22" "2016-02-23" "2016-02-25"


# 3. (20pts) Calculate the 50-day rolling maximum 
# and minimum of the prices (price_s[, 1]).
# You must use the function runMax() from package TTR. 

roll_max <- runMax(x=price_s[, 1], n=50)
roll_min <- -runMax(x=-price_s[, 1], n=50)

# Calculate the difference between the rolling 
# maximum and minimum and call it ba_nd. 

ba_nd <- roll_max - roll_min

# Calculate the rolling upper (lower) band as the 
# 50-day moving average ("VWAP") plus (minus) 
# one half of ba_nd. 
# Merge the rolling upper and lower bands with 
# price_s by adding them as the last columns. 
# Rename the last columns to "up_band" and "low_band". 

upper_band <- price_s[, "VWAP"] + ba_nd/2
lower_band <- price_s[, "VWAP"] - ba_nd/2
price_s <- merge(price_s, upper_band, lower_band)
colnames(price_s)[2:4] <- c("VWAP", "up_band", "low_band")

# At this point price_s should be like this:
# tail(price_s)
#               VTI      VWAP  up_band low_band
# 2016-04-08 104.20  99.28782 105.9633 92.61229
# 2016-04-11 103.91  99.45781 106.1333 92.78228
# 2016-04-12 104.92  99.58764 106.2632 92.91211
# 2016-04-13 106.15  99.74678 106.7573 92.73625
# 2016-04-14 106.11  99.89194 106.9025 92.88141
# 2016-04-15 106.06 100.07063 107.0812 93.06010

# Calculate two boolean xts series called up_indic 
# and low_indic. 
# up_indic is TRUE for dates when VTI prices are 
# above upper_band, while low_indic is TRUE for 
# dates when VTI prices are below lower_band. 
# They are FALSE otherwise. 

up_indic <- price_s[, 1] > upper_band
low_indic <- price_s[, 1] < lower_band


# 4. (10pts) Plot all four columns of price_s 
# in the same panel, from "2015-01-01" onward, 
# with custom line colors black, blue, 
# red, and green. 
# You must use functions chart_theme() and
# chart_Series(), and you can also use add_TA().

x11()
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("black", "blue", "red", "green")
chart_Series(x=price_s["2015/"],
             name="VTI plus VWAP",
             theme=plot_theme)

# Add background shading, with "lightgreen" color 
# for dates when up_indic is TRUE, and "coral" 
# when low_indic is TRUE. 
# Add a legend for "VTI" and "VWAP", in the 
# colors black and blue. 
# You must use functions add_TA() and legend().

add_TA(up_indic["2015/"], on=-1,
       col="lightgreen", border="lightgreen")
add_TA(low_indic["2015/"], on=-1,
       col="coral", border="coral")

# add legend
legend("top", inset=0.05, cex=0.8,title=NULL,
       leg=c("VTI", "VWAP"), lwd=2, bg="white",
       col=c("black", "blue"))




############## hw
# 1. (20pts) Perform regression of rolling range vs volume
# Load time series data and calculate rolling range statistics,
# the file "etf_series.RData" contains time series data,
# create a new environment called env_data,
# load data from the file "etf_series.RData" into env_data,
# use function load(), with the "envir" argument,

library(quantmod)
env_data <- new.env()
load(file="C:/Develop/data/etf_series.RData", envir=env_data)


# extract the adjusted prices and volume for symbol VTI from env_data, and call it VTI,
# VTI will now be defined both in the default workspace and in env_data,
# use function merge(),

da_ta <- merge(Ad(env_data$VTI), Vo(env_data$VTI))

# calculate rolling range statistics over a rolling window, called win_dow,

win_dow <- 22

# calculate two xts time series of trailing maximum and minimum values
# of adjusted prices over the rolling win_dow, and call them "roll_max" and "roll_min",
# at every point in time, the value of "roll_max" should be equal to the maximum
# adjusted price from points in the past covered by win_dow,
# use function runMax() from package TTR, with the proper "k" and "align" arguments,

roll_max <- runMax(x=da_ta[, 1], n=win_dow)
colnames(roll_max) <- "max"
roll_min <- -runMax(x=(-da_ta[, 1]), n=win_dow)
colnames(roll_min) <- "min"

# calculate the difference between "roll_max" and "roll_min", and call it "ra_nge",

ra_nge <- (roll_max-roll_min)
colnames(ra_nge) <- "range"

# calculate an xts time series of trailing mean values of the volume
# over the rolling win_dow, and call it "roll_volume",
# use function runMean(), with the proper "k" and "align" arguments,

roll_volume <- runMean(x=Vo(env_data$VTI), n=win_dow)
colnames(roll_volume) <- "volume"

# merge "ra_nge" with "roll_volume" into a single xts time series,
# and call it "range_volume",
# remove rows with NAs,
# use functions merge() and na.omit(),

range_volume <- merge(ra_nge, roll_volume)
range_volume <- na.omit(range_volume)

# create a time series plot of both columns of "range_volume" in two panels,
# use method plot.zoo(),

plot.zoo(range_volume)

# create a scatterplot of "range_volume",
# use function plot(),

plot(range ~ volume, data=range_volume)


# 2. (20pts) perform a regression of "ra_nge" vs "roll_volume"
# extract from summary() the regression statistics: p-value, adj.r.squared, fstatistic,
# create a named vector of the regression statistics,
# use function plot(),

reg_model <- lm(range ~ volume, data=range_volume)
reg_model_sum <- summary(reg_model)
c(pval=reg_model_sum$coefficients[2, 4],
  adj_rsquared=reg_model_sum$adj.r.squared,
  fstat=reg_model_sum$fstatistic[1])

# perform Durbin-Watson test for the autocorrelations of regression residuals,
# write what is the null hypothesis?
# can the null hypothesis be rejected?
# use function dwtest(), from package lmtest,

library(lmtest)  # load lmtest
dwtest(reg_model)

# perform the same regression on a subset of the data from 2010 and afterwards,
reg_model <- lm(range ~ volume, data=range_volume["2010/"])
reg_model_sum <- summary(reg_model)
c(pval=reg_model_sum$coefficients[2, 4],
  adj_rsquared=reg_model_sum$adj.r.squared,
  fstat=reg_model_sum$fstatistic[1])



############## hw
# Summary: Study the relationship between the
# standard deviation of returns and trading volumes
# using regression analysis.
# Calculate the rolling standard deviation of VTI
# returns, and the aggregated trading volumes,
# over monthly end points.
# Create plots and perform a regression of the two.

# Download the file etf_data.RData from NYU Classes,
# and load() it.
# etf_data.RData contains an environment called env_data,
# with OHLC time series data for ETFs, including VTI.

library(quantmod)
load(file="C:/Develop/data/etf_data.RData")


# 1. (20pts) Calculate the VTI daily returns from
# the adjusted close prices.
# You can use functions Ad() and dailyReturn(),

re_turns <- dailyReturn(Ad(env_data$VTI))

# Calculate the monthly end points for re_turns.
# You must use function endpoints().

end_points <- endpoints(env_data$VTI, on="months")

# Calculate the standard deviation of returns over
# monthly end points, and call it vol_at.
# You can use functions period.apply() and sd().

vol_at <- period.apply(re_turns, INDEX=end_points, FUN=sd)

# Calculate the aggregated trading volumes over
# monthly end points, and call it vol_ume.
# You can use functions period.sum() and Vo().

vol_ume <- period.sum(Vo(env_data$VTI), INDEX=end_points)

# Merge vol_at and vol_ume together and call it
# volat_volume. Assign to volat_volume the column
# names "volat" and "volu".
# Remove rows of volat_volume containing NA values.
# You can use functions merge(), colnames(), and
# complete.cases().

volat_volume <- merge(vol_at, vol_ume)
colnames(volat_volume) <- c("volat", "volu")
volat_volume <- volat_volume[complete.cases(volat_volume)]


# 2. (10pts) Plot the columns of volat_volume in a single
# plot in two panels.
# You can use functions x11(), par(), and chart_Series().

x11()
par(mfrow=c(2, 1))
chart_Series(vol_at, name="VTI volatility")
chart_Series(vol_ume, name="VTI volume")

# Plot a scatterplot of the two columns of volat_volume.
# You can use function plot() with the "formula" and
# "data" arguments.
# You must create the formula from the column names of
# volat_volume.
# You can use functions x11(), colnames(), as.formula(),
# and paste().

x11()
plot(
  formula=as.formula(paste(colnames(volat_volume), collapse=" ~ ")),
  data=volat_volume)


# 3. (10pts) Perform a regression of the two columns of
# volat_volume,
# You must create a formula from the column names of
# volat_volume.
# Extract from summary() the regression statistics for
# the slope coefficient: t-value, p-value, adj.r.squared,
# and create a named vector with these statistics.
# You can use functions colnames(), as.formula(), lm(),
# and summary(),

reg_model <- lm(
  as.formula(paste(colnames(volat_volume), collapse=" ~ ")),
  data=volat_volume)

reg_model_sum <- summary(reg_model)

with(reg_model_sum,
     c(tval=coefficients[2, 3],
       pval=coefficients[2, 4],
       adj_rsquared=adj.r.squared))


# 4. (10pts) Perform the Durbin-Watson test for the
# autocorrelations of residuals.
# Plot the residuals, using function plot().
# Can the null hypothesis be rejected in this case?
# Use function dwtest(), from package lmtest.

x11()
plot(reg_model$residuals, t="l")

library(lmtest)  # load lmtest
dwtest(reg_model)


# 5. (10pts) Calculate the month-over-month differences
# of volat_volume, and call it volat_vol_diff.
# Use function diff() and na.omit().

volat_vol_diff <- na.omit(diff(volat_volume))

# Plot a scatterplot of volat_vol_diff, and repeat the
# whole regression analysis from p.3 and p.4 above for
# volat_vol_diff.

x11()
plot(
  formula=as.formula(paste(colnames(volat_volume), collapse=" ~ ")),
  data=volat_vol_diff)

reg_model <- lm(
  as.formula(paste(colnames(volat_volume), collapse=" ~ ")),
  data=volat_vol_diff)

reg_model_sum <- summary(reg_model)

with(reg_model_sum,
     c(tval=coefficients[2, 3],
       pval=coefficients[2, 4],
       adj_rsquared=adj.r.squared))

dwtest(reg_model)



############## hw - more complicated version of above - uses environments
# Summary: calculate the rolling standard deviation of returns,
# and aggregate the volume, over monthly end points,
#
# 1. (5pts) Load time series data from file
# "etf_rets_volume.RData" (download it from NYU Classes),
# containing xts series with stock return and volume data,
# create a new environment called "env_returns",
# load data from the file "etf_rets_volume.RData" into "env_returns",
# use functions new.env() and load(), with the "envir" argument,

library(quantmod)
env_returns <- new.env()
load(file="C:/Develop/data/etf_rets_volume.RData", envir=env_returns)


# the environment "env_returns" should contain a number of xts series,
# each containing stock daily return and volume data for a single symbol,
# You can assume that all the xts series have the same date index,
# create a vector of monthly end points for any of the xts series
# in "env_returns", called end_points,
# use function endpoints() from package xts,

end_points <- endpoints(env_returns$VTI_rets, on="months")


# 2. (20pts) create a function called agg_volat_volume(),
# that accepts three arguments:
#   x_ts: an xts series containing returns and volume data,
#   end_points: a vector of end points,
#   "envir": an environment argument,
# agg_volat_volume() should:
# - extract returns and volume data from x_ts,
# - extract the symbol name from the columns of x_ts (symbol_name),
# - calculate the volatility from the returns,
#    over non-overlapping intervals given by end_points,
# - calculate the total (cumulative) volume,
#    over non-overlapping intervals given by end_points,
# - cbind (merge) volatility with volume data into a single xts series
#    called volat_volume,
# - rename the colnames of volat_volume to "symbol_name.Volat"
#    and "symbol_name.Volume", (replace symbol_name with the symbol name),
# - assign (copy) volat_volume to an object named symbol_name
#   in the "envir" environment,
# agg_volat_volume() should produce the side effect of creating
# an xts series in the "envir" environment, that contains volatility
# and volume data calculated from the input x_ts,
# agg_volat_volume() should return invisibly the symbol_name,
# You can use functions strsplit(), colnames(), period.apply(),
# period.sum(), cbind(), paste() (or paste0), xts(),
# assign(), invisible(),

agg_volat_volume <- function(x_ts, end_points, envir=env_returns) {
  symbol_name <- strsplit(
    colnames(x_ts)[1], split="[.]")[[1]][1]
  std_dev <- period.apply(x_ts[, 1],
                          INDEX=end_points,
                          FUN=sd)
  vol_ume <- period.sum(x_ts[, 2], INDEX=end_points)
  volat_volume <- xts(cbind(std_dev, vol_ume[-1, ]),
                      order.by=index(x_ts[end_points, ]))
  colnames(volat_volume) <-
    c(paste0(symbol_name, ".Volat"), paste0(symbol_name, ".Volume"))
  assign(symbol_name, volat_volume, envir=envir)
  invisible(symbol_name)
}  # end agg_volat_volume


# 2. (10pts) create a new environment called "env_volat",
# for storing xts series containing stock return and volume data,
# use function new.env(),

env_volat <- new.env()

# apply function agg_volat_volume() to a "VTI_rets", to verify
# it works correctly:

agg_volat_volume(env_returns$VTI_rets,
                 end_points=end_points,
                 envir=env_volat)


# plot both columns of "env_volat$VTI", in a plot with two panels,
# You can use method plot.zoo(), or plot.xts() and par(),

plot.zoo(env_volat$VTI)
# or
par(mfrow=c(2, 1))
plot(env_volat$VTI[, 2])
plot(env_volat$VTI[, 1])


# plot a scatterplot of both columns of "env_volat$VTI",
# You can use function plot() with "data" argument,

plot(VTI.Volume ~ VTI.Volat, data=env_volat$VTI)

# calculate the month-over-month difference of both columns
# of "env_volat$VTI",
# use function diff(),
# plot a scatterplot of both the diff columns,

plot(VTI.Volume ~ VTI.Volat, data=diff(env_volat$VTI))


# 3. (10pts) perform a regression of rolling volume versus volatility,
# of "env_volat$VTI",
# extract from summary() the regression statistics:
#  p-value, adj.r.squared, fstatistic,
# create a named vector of the regression statistics,
reg_model <- lm(VTI.Volume ~ VTI.Volat, data=env_volat$VTI)
reg_model_sum <- summary(reg_model)
c(pval=reg_model_sum$coefficients[2, 4],
  adj_rsquared=reg_model_sum$adj.r.squared,
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
  adj_rsquared=reg_model_sum$adj.r.squared,
  fstat=reg_model_sum$fstatistic[1])

# perform the Durbin-Watson test for the autocorrelations of
# regression residuals,
# use function dwtest(), from package lmtest,
library(lmtest)  # load lmtest
dwtest(reg_model)




############## hw
# 1. (15pts) calculate returns and aggregations over overlapping intervals,
#
# download the file "zoo_series.RData" from NYU Classes, and load() it,
# the file "zoo_series.RData" contains a zoo called "zoo_series",
load(file="C:/Develop/data/zoo_series.RData")

# create a vector of monthly end points for "zoo_series",
# called end_points,
# use function endpoints() from package xts,
end_points <- endpoints(zoo_series, on="months")

# extract (subset) the monthly prices from the "VTI.Close" column
# of "zoo_series", corresponding to end_points,
# convert them to an xts series, and call it xts_series,
# calculate the log of xts_series, and copy it back onto xts_series,
# use function as.xts() from package xts, and log(),
xts_series <- as.xts(log(zoo_series[end_points, "VTI.Close"]))

# calculate the percentage (log) monthly returns of xts_series,
# by taking the difference between xts_series, and xts_series
# lagged by 1 month, and call it "xts_rets",
# calculate the percentage (log) 3-month returns of xts_series,
# by taking the difference between xts_series, and xts_series
# lagged by 3 months, and call it "xts_rets_3m",
# remove rows containing NAs,
# use functions lag() and na.omit(),

xts_rets <- na.omit(xts_series - lag(xts_series, 1))
xts_rets_3m <- na.omit(xts_series - lag(xts_series, 3))


# below is a more complicated but more flexible way, by using sapply()
l_ag <- 3
xts_rets_3m_bis <- sapply((l_ag+1):length(end_points),
              function(pe_riod, se_ries) {
                se_ries[end_points[pe_riod]] -
                  se_ries[end_points[pe_riod-l_ag]]
              },  # end anon function
              se_ries=log(as.numeric(zoo_series[, "VTI.Close"]))
              )  # end sapply

xts_rets_3m_bis <- xts(x=xts_rets_3m_bis,
                   order.by=index(zoo_series[end_points[-(1:l_ag)], 1]))
identical(as.xts(xts_rets_3m), xts_rets_3m_bis)


# calculate the autocorrelation functions for "xts_rets",
# and for "xts_rets_3m", and call them "acf_rets" and "acf_rets_3m",
# extract the lag=1 autocorrelations for both series,
# use function acf(),
# remember that acf() returns an acf object containing a vector
# of autocorrelation coefficients, with the lag=0 autocorrelation
# being the first element,
acf_rets <- acf(xts_rets, lag=10, xlab=NA, ylab=NA)
acf_rets$acf[2]
acf_rets_3m <- acf(xts_rets_3m, lag=10, xlab=NA, ylab=NA)
acf_rets_3m$acf[2]

# apply the Ljung-Box test for autocorrelations to "xts_rets",
# and to "xts_rets_3m",
Box.test(xts_rets, lag=10, type="Ljung")
Box.test(xts_rets_3m, lag=10, type="Ljung")



############## hw
# 1. (15pts) calculate autocorrelations of absolute deviations
# EuStockMarkets autocorrelation
# extract lag=5 vector of autocorrelation coefficients using functions acf() and drop(),

dax_rets <- diff(log(EuStockMarkets[, 1]))

# extract autocorrelations of absolute deviations
drop(acf(abs(dax_rets), lag=5, plot=FALSE)$acf)[-1]
# plot
acf_plus(abs(dax_rets), lag=5)

# rnorm autocorrelation
len_gth <- length(EuStockMarkets[, 1])
rand_walk <- zoo(rnorm(len_gth), order.by=(Sys.Date()+0:(len_gth-1)))

# extract autocorrelations of absolute deviations
drop(acf(abs(rand_walk), lag=5, plot=FALSE)$acf)[-1]
# plot
acf_plus(abs(rand_walk), lag=5)

# load lmtest
library(lmtest)
dw_test <- dwtest(rand_walk ~ 1)
dw_test$p.value < 0.0227
dw_test <- dwtest(abs(rand_walk) ~ 1)
dw_test$p.value < 0.0227
dw_test <- dwtest(dax_rets ~ 1)
dw_test$p.value < 0.0227
dw_test <- dwtest(abs(dax_rets) ~ 1)
dw_test$p.value < 0.0227



############## hw
# Summary: perform autocorrelation tests of interval statistics, MAD.
# Demonstrate that measures of dispersion are autocorrelated.
# Load time series data and calculate interval statistics over end points.

# 1. (15pts)
# download the file "etf_rets.RData" from NYU Classes,
# "etf_rets.RData" contains an xts of daily ETF returns called etf_rets,
# use function load(),

# load package xts,
library(xts)
load(file="C:/Develop/data/etf_rets.RData")

# create a vector of monthly end points for etf_rets, called end_points,
# use function endpoints() from package xts,

end_points <- endpoints(etf_rets, on="months")

# calculate a numeric vector of returns of etf_rets[, "VTI"],
#   over monthly non-overlapping intervals based on end_points,
#   and call it re_turns,
# calculate a vector of Median Absolute Deviations (MAD) of etf_rets[, "VTI"],
#   over monthly non-overlapping intervals, and call it "vol_at",
# "vol_at" is a vector of volatility estimates over time,
# use function period.apply() from package xts,
# and functions mad(), sum(), and as.numeric(),

re_turns <- as.numeric(period.apply(etf_rets[, "VTI"], INDEX=end_points, sum))
vol_at <- as.numeric(period.apply(etf_rets[, "VTI"], INDEX=end_points, mad))

# create a vector of rnorm() of length equal to etf_rets[, "VTI"],
# and calculate a vector of MAD from this vector, over monthly
# non-overlapping intervals, and call it "rand_vol",
# use functions period.apply(), mad(), rnorm(), and length(),

rand_vol <- period.apply(rnorm(length(etf_rets[, "VTI"])), INDEX=end_points, mad)


# 2. (20pts) Perform autocorrelation tests on interval rets,
# perform the Durbin-Watson autocorrelation test on re_turns,
# "vol_at", and "rand_vol",
# the Durbin-Watson test can be performed on a vector "y" using the syntax:
#   dwtest(y ~ 1)
# extract the p-value and compare it to the 2.27% confidence level,
# for which vector can the null hypothesis of zero autocorrelations
# be rejected at 2.27% confidence level?
# use function dwtest(), from package lmtest,

# load lmtest
library(lmtest)
dw_test <- dwtest(re_turns ~ 1)
dw_test$p.value < 0.0227
dw_test <- dwtest(vol_at ~ 1)
dw_test$p.value < 0.0227
dw_test <- dwtest(rand_vol ~ 1)
dw_test$p.value < 0.0227

# perform the Ljung-Box autocorrelation test on re_turns,
# "vol_at", and "rand_vol",
# extract the p-value and compare it to the 2.27% confidence level,
# for which vector can the null hypothesis of zero autocorrelations
# be rejected at 2.27% confidence level?
# use function Box.test(), with lag=10 and type="Ljung",
box_test <- Box.test(re_turns, lag=10, type="Ljung")
box_test$p.value < 0.0227
box_test <- Box.test(vol_at, lag=10, type="Ljung")
box_test$p.value < 0.0227
box_test <- Box.test(rand_vol, lag=10, type="Ljung")
box_test$p.value < 0.0227

# apply the functions acf_plus() (from FRE7241 lecture #2) and pacf()
# to re_turns, "vol_at", and "rand_vol",
# based on visual inspection, which of them appear to be autocorrelated?
acf_plus(re_turns)
acf_plus(vol_at)
acf_plus(rand_vol)
pacf(re_turns)
pacf(vol_at)
pacf(rand_vol)


# 3. (10pts) fit ARIMA models,
#
# fit ARIMA models to re_turns, "vol_at", and "rand_vol",
# extract the ARIMA coefficients, and their standard errors,
# the standard errors can be calculated as the square roots
# of the diagonal of the field "var.coef" of the ARIMA object,
# see:
# http://r.789695.n4.nabble.com/ARIMA-standard-error-td820763.html
# use functions arima(), sqrt(), and diag(),

a_rima <- arima(re_turns, order=c(5,0,0))
a_rima$coef
sqrt(diag(a_rima$var.coef))

a_rima <- arima(vol_at, order=c(5,0,0))
a_rima$coef
sqrt(diag(a_rima$var.coef))

a_rima <- arima(rand_vol, order=c(5,0,0))
a_rima$coef
sqrt(diag(a_rima$var.coef))




##############################
# portfolio optimization multivariate analysis
##############################


############## hw
# 1. (10pts) Download data and calculate table of regression statistics

# 1. (15pts) Download price data using package quantmod and function getSymbols,
library(quantmod)
sym_bols <- c("VTI", "AA", "MSFT")
env_data <- new.env()
getSymbols(sym_bols, env=env_data)
ls(env_data)


# 2. (5pts) scrub NA values, and calculate returns from adjusted prices,
etf_series <- do.call(merge, as.list(env_data)[sym_bols])
etf_series_ad <- do.call(merge, eapply(env_data, Ad)[sym_bols])
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
write.zoo(etf_series, file="etf_series.csv", sep=",")


# 4. (5pts) plot the cumulative returns using chart.CumReturns,
library(PerformanceAnalytics)
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



############## hw - old version most is already incorporated into lecture slides
# Summary: Create a scatterplot of random portfolios,
# together with a minimum variance portfolio.

# Download the file etf_data.RData from NYU Classes,
# and load() it.
# etf_data.RData contains an xts series called etf_rets,
# with ETF returns,

library(xts)
library(quantmod)
load(file="C:/Develop/data/etf_data.RData")


# 1. (20pts) Create a vector of symbol names called sym_bols,
# create a named vector of initial portfolio weights for the
# sym_bols, all equal to 1, and call it weight_s,
# You can use functions rep() and names(),

sym_bols <- c("VTI", "VNQ", "DBC", "XLP", "XLK")
n_weights <- length(sym_bols)
weight_s <- rep(1, n_weights)
names(weight_s) <- sym_bols

# Create an objective function equal to the standard
# deviation of returns, and call it object_ive(). 
# The objective function should accept a single vector 
# argument:
# - "weights": the portfolio weights,
# object_ive() should return the standard deviation of
# portfolio returns, divided by the square root of the
# sum of squared "weights".
# You can use functions sqrt(), sd(), and sum(),

object_ive <- function(weights) {
  portf_ts <- etf_rets[, sym_bols] %*% weights
  sd(portf_ts)/sqrt(sum(weights^2))
}  # end object_ive

# Calculate object_ive() for the equal weight portfolio,
# and for a portfolio with double the weights, and
# verify that they both produce the same number.

object_ive(weight_s)
object_ive(2*weight_s)


# 2. (20pts) Perform a portfolio optimization to
# find the weights that minimize object_ive().
# You can use the function optim() , with the
# "upper" and "lower" parameters equal to 10
# and -10, respectively.

optim_run <- optim(par=weight_s,
                   fn=object_ive,
                   method="L-BFGS-B",
                   upper=rep(10, n_weights),
                   lower=rep(-10, n_weights))

# Rescale the optimal weights, so their sum of squares
# is equal to "1".
# These are the weights of the minimum variance portfolio.
# hint: read the forum hint for homework #5.

weight_s <- optim_run$par/sqrt(sum(optim_run$par^2))

# Calculate the minimum variance portfolio returns, and
# coerce them to an xts series, and call them optim_rets.
# You can use the index of etf_rets.
# You can use functions xts() and index(),

optim_rets <- xts(x=etf_rets[, sym_bols] %*% weight_s,
                  order.by=index(etf_rets))

# Plot the cumulative returns of optim_rets using
# chart_Series().
# You must use functions cumsum() and chart_Series(),

library(quantmod)
chart_Series(x=cumsum(optim_rets), name="minvar portfolio")


# 3. (30pts) 
# Define the number of random portfolios:

n_portf <- 1000

# Calculate a matrix of cumulative returns and standard 
# deviations of random portfolios, and call it ret_sd. 
# Perform an sapply() loop over "1:n_portf", and at each step
# create a vector of random weights using rnorm().
# Rescale the random weights, so their sum of squares
# is equal to "1".
# Multiply etf_rets by the random weights, and calculate
# the cumulative return and standard deviation.
# The output should ba a named matrix called ret_sd.
# You can use functions rnorm(), sum(), and sd(),
# and an anonymous function,

ret_sd <- sapply(1:n_portf,
                 function(in_dex) {
                   weight_s <- rnorm(n_weights)
                   weight_s <- weight_s/sqrt(sum(weight_s^2))
                   portf_ts <- etf_rets[, sym_bols] %*% weight_s
                   c(ret=sum(portf_ts), sd=sd(portf_ts))
                 }  # end anonymous function
)  # end sapply

# You should get output similar to this:
# ret_sd[1:2, 1:4]
#         [,1]       [,2]       [,3]       [,4]
# ret 0.72786359 0.94225213 1.92288432 1.58426183
# sd  0.02027661 0.02379909 0.02899581 0.02638787


# Create a scatterplot of ret_sd, with standard deviations on
# the x-axis, and returns on the y-axis, and add a title.
# Add a point in red to the scatterplot corresponding to the
# minimum variance portfolio.
# You can use functions plot(), points(), text(), and title(),

plot(x=ret_sd[2, ], y=ret_sd[1, ], xlim=c(0, max(ret_sd[2, ])), 
     ylim=c(min(0, min(ret_sd[1, ])), max(ret_sd[1, ])), 
     xlab=rownames(ret_sd)[2], ylab=rownames(ret_sd)[1])
title(main="Random and minvar portfolios", line=-1)

optim_sd <- sd(optim_rets)
optim_ret <- sum(optim_rets)

points(x=optim_sd, y=optim_ret,
       col="red", lwd=3, pch=21)
text(x=optim_sd, y=optim_ret,
     labels="minvar", pos=1, cex=0.8)

# Redefine object_ive() so that it's equal to minus
# the Sharpe ratio.

object_ive <- function(weights) {
  portf_ts <- etf_rets[, sym_bols] %*% weights
  -sum(portf_ts)/sd(portf_ts)
}  # end object_ive

# Perform a portfolio optimization to find the weights
# that minimize object_ive().

optim_run <- optim(par=weight_s,
                   fn=object_ive,
                   method="L-BFGS-B",
                   upper=rep(10, n_weights),
                   lower=rep(-10, n_weights))

# Rescale the optimal weights, so their sum of squares
# is equal to "1".
# Calculate the maximum Sharpe ratio portfolio returns, and
# coerce them to an xts series, and call them optim_rets.

weight_s <- optim_run$par/sqrt(sum(optim_run$par^2))
optim_rets <- xts(x=etf_rets[, sym_bols] %*% weight_s,
                  order.by=index(etf_rets))

# Add a point in blue to the scatterplot corresponding
# to the maximum Sharpe ratio portfolio.
# You can use functions points() and text(),

optim_sd <- sd(optim_rets)
optim_ret <- sum(optim_rets)

points(x=optim_sd, y=optim_ret,
       col="blue", lwd=3, pch=21)
text(x=optim_sd, y=optim_ret,
     labels="maxSR", pos=1, cex=0.8)

# Add a point with text "origin" at the origin x=0, y=0.
# Draw a straight line from the origin to the maximum
# Sharpe ratio point.
# You can use functions points(), text(), and abline(),

points(x=0, y=0, labels="origin")
text(x=0, y=0, labels="origin", pos=1, cex=0.8)
abline(a=0, b=optim_ret/optim_sd)



############## test
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



############## hw
# Summary: perform a rolling portfolio optimization over
# annual intervals, calculate optimized portfolio weights
# in each year, and apply them to out-of-sample data in
# the following year.

# Download the file etf_data.RData from NYU Classes,
# and load() it.  etf_data.RData contains an xts series
# called etf_rets, containing ETF returns.

load(file="C:/Develop/data/etf_data.RData")

# 1. (10pts) create a vector of annual end points from the index
# of etf_rets, and call it end_points.
# Use function endpoints() from package xts,

library(xts)
end_points <- endpoints(etf_rets, on="years")

# the above code should produce the following data:
# > end_points
# [1] 0  207  460  712  964 1216 1466 1718 1970 2053

# The end_points values are the integer indices
# corresponding to dates that form non-overlapping annual
# intervals.
# Create a list of elements containing the indices belonging
# to the non-overlapping annual intervals, and call the list
# "inter_vals".
# Each element of "inter_vals" represents an interval
# corresponding to a year, and contains the indices of dates
# belonging to that interval.
# For example, the first element should contain the
# integers: 1:207 belonging to the year 2007, the second
# element: 208:460 belonging to the year 2008, etc.
# Assign names to the elements of "inter_vals" corresponding
# the year.
# hint: perform an lapply() loop over end_points.
# You can use functions lapply(), names(), format(), index(),
# and an anonymous function,

inter_vals <- lapply(2:length(end_points),
                     function(in_dex)
                       (end_points[in_dex-1]+1):end_points[in_dex]
)  # end lapply
names(inter_vals) <- format(index(etf_rets[end_points]), "%Y")

# the above code should produce the following data:
# > tail(inter_vals$"2008")
# [1] 455 456 457 458 459 460
# > head(inter_vals$"2009")
# [1] 461 462 463 464 465 466
# notice that there's no overlap between second and third intervals,

# create a vector of symbols for the optimized portfolio,

sym_bols <- c("VTI", "VNQ", "DBC")

# create a named vector of initial portfolio weights for the
# sym_bols, all equal to 1, and call it "portf_weights",
# You can use functions rep() and names(),

portf_weights <- rep(1, length(sym_bols))
names(portf_weights) <- sym_bols


# 2. (10pts) create an objective function equal to minus
# the Sharpe ratio, and call it object_ive().
# The objective function should accept two arguments:
#  "weights": the portfolio weights,
#  re_turns: an xts series containing returns data,
# hint: you can adapt code from the slide "Portfolio
# Objective Function".
# You can use functions sum() and sd(),

object_ive <- function(weights, re_turns) {
  portf_ts <- re_turns %*% weights
  -sum(portf_ts)/sd(portf_ts)
}  # end object_ive

# apply object_ive() to an equal weight portfolio, and
# etf_rets subset to sym_bols,

object_ive(weights=portf_weights, re_turns=etf_rets[, sym_bols])


# 3. (20pts) create a function called optim_portf(),
# that accepts a single argument:
#  re_turns: an xts series containing returns data,
# optim_portf() should:
# - create a named vector of initial portfolio weights
#  all equal to 1, with the names extracted from the
#  column names of re_turns,
# - optimize the weights to minimize the object_ive(),
# - rescale the weights, so their sum of squares is "1",
# - return the vector of optimal portfolio weights,
# hint: you can adapt code from the slide
# "Multi-dimensional Portfolio Optimization".
# You can use the function optim() with the dots "..."
# argument, and with the "upper" and "lower" parameters
# equal to 10 and -10, respectively.
# You can also use the functions rep(), colnames(),
# structure(), and object_ive(),

optim_portf <- function(re_turns) {
  n_col <- ncol(re_turns)
# create initial vector of portfolio weights equal to 1,
  portf_weights <- structure(rep(1, n_col), names=colnames(re_turns))
# optimization to find weights with maximum Sharpe ratio
  optim_run <- optim(par=portf_weights,
                     fn=object_ive,
                     method="L-BFGS-B",
                     upper=rep(10, n_col),
                     lower=rep(-10, n_col),
                     re_turns=re_turns)
# rescale and return the optimal weights
  optim_run$par/sqrt(sum(optim_run$par^2))
}  # end optim_portf

# apply optim_portf() to etf_rets subset to sym_bols
# and the interval "2009" from "inter_vals",

optim_portf(re_turns=etf_rets[inter_vals$"2009", sym_bols])


# 4. (20pts) Perform an sapply() loop over "inter_vals",
# and calculate the optimal portfolio weights in each
# interval. Call the matrix returned by sapply() weight_s.
# You can use functions sapply(), optim_portf(),
# and an anonymous function,

weight_s <- sapply(inter_vals,
                   function(pe_riod) {
                     optim_portf(re_turns=etf_rets[pe_riod, sym_bols])
                   }  # end anon function
)  # end sapply


# 5. (30pts) Perform an lapply() loop over the length of
# "inter_vals", starting in the second interval (year).
# In each interval calculate the portfolio returns,
# using out-of-sample portfolio weights from the previous
# interval.  In each interval coerce the portfolio returns to
# an xts series and return it.
# hint: you can use the names of intervals "names(inter_vals)"
# to extract the weight_s from the previous interval.
# You can use functions lapply(), xts(), index(),
# and an anonymous function,

optim_rets <- lapply(2:length(inter_vals),
                     function(in_dex)
                       xts(x=etf_rets[inter_vals[[in_dex]], sym_bols] %*%
                             weight_s[, names(inter_vals)[in_dex-1]],
                           order.by=index(etf_rets[inter_vals[[in_dex]], ]))
)  # end lapply

# The lapply() loop produces a list of xts series.
# Flatten optim_rets into a single xts series.
# You can use functions do.call() and rbind(),

optim_rets <- do.call(rbind, optim_rets)

# optim_rets should look like this:
# > dim(optim_rets)
# [1] 1846    1
# > head(optim_rets)
#                    [,1]
# 2008-01-02  0.007910575
# 2008-01-03  0.017271646
# 2008-01-04 -0.004024706
# 2008-01-07 -0.013655207
# 2008-01-08  0.007658724
# 2008-01-09 -0.005758695

# plot the cumulative sum of optim_rets using
# chart_Series().
# You must use functions cumsum() and chart_Series(),

library(quantmod)
chart_Series(x=cumsum(optim_rets), name="Rolling optimized portfolio returns")



############## hw
# rolling window portfolio optimization using PortfolioAnalytics
#
# 1. (10pts) create a portfolio object with equal weights,
library(PortfolioAnalytics)
load(file="C:/Develop/data/etf_data.RData")
portf_names <- c("VTI", "IEF", "DBC", "XLF",
                 "VNQ", "XLP", "XLV", "XLU", "XLB", "XLE")
# create portfolio object with equal weights
portf_init <- rep(1/length(portf_names),
                  length(portf_names))
names(portf_init) <- portf_names
portf_init <- portfolio.spec(
  assets=portf_init)

# add leverage constraints, with min_sum=0.9, max_sum=1.1,
# add box constraint long/short, with min=-0.5, max=0.5,
# add objectives "mean" for "return" and "StdDev" for "risk",
portf_maxSRN <- add.constraint(
  portfolio=portf_init, type="leverage",
  min_sum=0.9, max_sum=1.1)

# add box constraint long/short
portf_maxSRN <- add.constraint(
  portfolio=portf_maxSRN,
  type="box", min=-0.5, max=0.5)

# add objectives
portf_maxSRN <- add.objective(
  portfolio=portf_maxSRN,
  type="return",  # maximize mean return
  name="mean")
# add objectives
portf_maxSRN <- add.objective(
  portfolio=portf_maxSRN,
  type="risk",  # minimize StdDev
  name="StdDev")


# 2. (20pts) create a vector of annual date windows by year,
#    from 2007 to 2014, using "as.character",
year_win_dows <- as.character(2007:2014)

#    find optimal weights in each year by performing an "sapply" loop,
#    save the weights into a csv file using "write.zoo",
wei_ghts <- sapply(year_win_dows,
                   function(ye_ar) {
                     maxSR_DEOpt <- optimize.portfolio(
                       R=etf_rets[ye_ar, portf_names],  # specify returns
                       portfolio=portf_maxSRN,  # specify portfolio
                       optimize_method="DEoptim", # use DEoptim
                       maxSR=TRUE,  # maximize Sharpe
                       trace=TRUE, traceDE=0)
                     maxSR_DEOpt$weights
                   }
)

write.csv(wei_ghts,
          file="portf_weights.csv", sep=",")



# new version
############## hw
# Summary: calculate a matrix of the best performing ETFs
# in each year.  Create a scatterplot of alphas for the
# years 2008 and 2009.

# Download the file etf_data.RData from NYU Classes,
# and load() it.
# etf_data.RData contains an xts series called etf_rets,
# with ETF returns,

library(xts)
library(quantmod)
library(PerformanceAnalytics)

load(file="C:/Develop/data/etf_data.RData")

# create a vector of symbol names called sym_bols,

sym_bols <- c("VTI", "VNQ", "DBC", "XLP", "XLK")

# 1. (25pts) Create a vector of yearly end points
# for etf_rets, called end_points,
# you must use function endpoints() from package xts,

end_points <- endpoints(etf_rets, on="years")

# Perform an lapply() loop over the end_points, and call
# the result "list_capm".
# Inside the loop subset etf_rets to the sym_bols and
# end points, and calculate a data frame of statistics
# using table.CAPM(), using VTI as the benchmark asset "Rb".
# Simplify the columnn names and return the data frame.
# You can use functions lapply(), sapply(), table.CAPM(),
# colnames(), strsplit(), and an anonymous function,

list_capm <-
  lapply(2:length(end_points),
         function(in_dex) {
           x_ts <-
             etf_rets[end_points[(in_dex-1)]:end_points[in_dex], sym_bols]
           cap_m <- table.CAPM(Ra=x_ts[, -1],
                               Rb=x_ts[, 1], scale=252)
           colnames(cap_m) <-
             sapply(colnames(cap_m),
                    function(str) {strsplit(str, split=" ")[[1]][1]})
           cap_m
         })  # end lapply

# "list_capm" should be a list of data frames like this:
# list_capm[[1]]
#                         VNQ     DBC    XLP    XLK
# Alpha               -0.0013  0.0012 0.0004 0.0005
# Beta                 1.3835  0.1306 0.5371 0.8745
# Beta+                1.5589 -0.1205 0.4994 0.8482
# Beta-                1.2093  0.2158 0.5446 0.8607
# R-squared            0.6283  0.0178 0.6136 0.7505
# Annualized Alpha    -0.2776  0.3686 0.1137 0.1450
# Correlation          0.7927  0.1333 0.7833 0.8663
# Correlation p-value  0.0000  0.0555 0.0000 0.0000
# Tracking Error       0.1847  0.2129 0.1028 0.0849
# Active Premium      -0.2940  0.2964 0.0863 0.1424
# Information Ratio   -1.5915  1.3926 0.8401 1.6777
# Treynor Ratio       -0.1626  2.7993 0.2894 0.2418

# Assign names to the list using the years of the end_points.
# You can use functions names(), format(), and index(),

names(list_capm) <- format(index(etf_rets[end_points, ]), "%Y")

# 2. (10pts) Perform an sapply() loop over "list_capm",
# and call the result "alphas_capm".
# Inside the loop extract the data frame row called
# "Annualized Alpha", coerce it to a vector using unlist(),
# and return the vector.
# You can use functions sapply(), unlist(),
# and an anonymous function,

alphas_capm <- sapply(list_capm,
                      function(cap_m) {
                        unlist(cap_m["Annualized Alpha", ])
                      })  # end sapply

# "alphas_capm" should be a matrix of alphas like this:
#        2007    2008    2009    2010    2011    2012    2013    2014    2015
# VNQ -0.2776  0.4663 -0.0920  0.0571  0.0842  0.0517 -0.2216  0.2169 -0.1036
# DBC  0.3686 -0.1694  0.0363  0.0033 -0.0041 -0.0600 -0.1722 -0.2912 -0.0659
# XLP  0.1137  0.0487  0.0085  0.0317  0.1294  0.0162  0.0106  0.0745 -0.0485
# XLK  0.1450 -0.0886  0.2132 -0.0419  0.0160 -0.0098 -0.0134  0.0520  0.0386

# 3. (15pts) Perform an apply() loop over the columns of "alphas_capm",
# and extract the ETF names sorted according to their "Annualized Alpha".
# in each year, and call the result "names_capm".
# Inside the loop first sort the column in descending order, then
# extract and return the vector names (not the values).
# You can use functions sapply(), sort(), names(),
# and an anonymous function,

names_capm <- apply(alphas_capm, MARGIN=2,
                    FUN=function(col_umn) {
                      col_umn <- sort(col_umn, decreasing=TRUE)
                      names(col_umn)
                    })  # end apply

# "names_capm" should be a matrix of ETF names like this:
#      2007  2008  2009  2010  2011  2012  2013  2014  2015
# [1,] "DBC" "VNQ" "XLK" "VNQ" "XLP" "VNQ" "XLP" "VNQ" "XLK"
# [2,] "XLK" "XLP" "DBC" "XLP" "VNQ" "XLP" "XLK" "XLP" "XLP"
# [3,] "XLP" "XLK" "XLP" "DBC" "XLK" "XLK" "DBC" "XLK" "DBC"
# [4,] "VNQ" "DBC" "VNQ" "XLK" "DBC" "DBC" "VNQ" "DBC" "VNQ"

# 4. (20pts) Create a scatterplot of alphas for "2008" and "2009",
# and add labels with ETF names,
# use columns of "alphas_capm" and functions plot() and text(),

year1 <- "2008"
year2 <- "2009"
plot(x=alphas_capm[, year1], y=alphas_capm[, year2],
     xlab=year1, ylab=year2)
text(x=alphas_capm[, year1], y=alphas_capm[, year2],
     labels=rownames(alphas_capm), pos=4, cex=0.8)


# old version
############## hw
# create scatterplot of alphas in one year and the next,
# create table of names sorted by alpha in each year,
# 1. (20pts) Load time series data and calculate returns,
# the file "etf_series.RData" contains time series of ETF prices,
# create a new environment called env_data,
# load data from the file "etf_series.RData" into env_data,
# use function load(), with the "envir" argument,

env_data <- new.env()
load(file="C:/Develop/data/etf_series.RData", envir=env_data)

# perform an eapply() loop to extract the adjusted prices for all
# the variables in env_data, and call it etf_series_ad,

# load packages quantmod, lubridate, and PerformanceAnalytics.
library(quantmod)
library(lubridate)
library(PerformanceAnalytics)
etf_series_ad <- do.call(merge, eapply(env_data, Ad))

# etf_series_ad should be an xts series containing adjusted prices,
# with colnames in the format "name.Adjusted",
# rename the colnames and drop ".Adjusted" from the colnames,
# use functions sapply() and strsplit(),
colnames(etf_series_ad) <- sapply(colnames(etf_series_ad),
                                  function(col_name)
                                    strsplit(col_name, split="[.]")[[1]])[1, ]


# scrub (remove) rows with NA values from etf_series_ad,
# use function complete.cases(),
etf_series_ad <- etf_series_ad[complete.cases(etf_series_ad)]


# calculate an xts series containing returns of etf_series_ad, and call it etf_rets,
# use functions lapply(), dailyReturn(), do.call(), and merge(),
etf_rets <- lapply(etf_series_ad,
                   function(x_ts) {
                     daily_return <- dailyReturn(x_ts)
                     colnames(daily_return) <- names(x_ts)
                     daily_return
                   })  # end lapply

# flatten list of xts series into a single xts series,
etf_rets <- do.call(merge, etf_rets)

# rearrange columns according to ETF symbols for asset allocation
sym_bols <- c("VTI", "VEU", "IEF", "VNQ",
              "DBC", "XLY", "XLP", "XLE", "XLF", "XLV",
              "XLI", "XLB", "XLK", "XLU", "IWB", "IWD",
              "IWF", "IWM", "IWN", "IWO", "IWP", "IWR",
              "IWS", "IWV", "IUSV", "IUSG")
etf_rets <- etf_rets[, sym_bols]


# Extract the numeric year from each element of the date index of etf_rets
# calculate a numeric vector of years from the date index of etf_rets,
# and call it "ye_ars",
# you can use either functions format() and as.numeric(),
# or function year() from package lubridate,
ye_ars <- as.numeric(format(index(etf_rets), "%Y"))
ye_ars <- year(index(etf_rets))


# Calculate a matrix containing the annualized alpha for each ETF in each year,
# and call it "ann_alphas"
# the matrix "ann_alphas" should have rows corresponding to ETF names,
# and columns corresponding to years,
# assign row and column names from colnames of etf_rets,
# use functions sapply(), unique(), and either CAPM.alpha()
# or table.CAPM() from package PerformanceAnalytics,
# the function unique() calculates a vector of unique elements of an object,
# and can be used to extract unique years from "ye_ars",
# annualize the alphas by multiplying them by the average number
# of business days in each year (250),

# first method, using CAPM.alpha(),
ann_alphas <- 250*sapply(unique(ye_ars), function(ye_ar) {
  in_dex <- (ye_ars==ye_ar)
  CAPM.alpha(Ra=etf_rets[in_dex, -1],
             Rb=etf_rets[in_dex, "VTI"])
})


# second method, using table.CAPM(),
ann_alphas <- sapply(unique(ye_ars), function(ye_ar) {
  in_dex <- (ye_ars==ye_ar)
  etf_perf_stats <- table.CAPM(Ra=etf_rets[in_dex, -1],
                               Rb=etf_rets[in_dex, "VTI"],
                               scale=250)
  as.numeric(etf_perf_stats["Annualized Alpha", ])
})


# assign row and column names,
rownames(ann_alphas) <- colnames(etf_rets)[-1]
colnames(ann_alphas) <- unique(ye_ars)


##############
# 2. (10pts)
# Create a scatterplot of "ann_alphas" values for "2008" and "2009",
# add labels containing the rownames of "ann_alphas",
# use functions plot() and text(),
year1 <- "2008"
year2 <- "2009"
plot(x=ann_alphas[, year1], y=ann_alphas[, year2],
     xlab=year1, ylab=year2)
text(x=ann_alphas[, year1], y=ann_alphas[, year2],
     labels=rownames(ann_alphas),
     pos=1, cex=0.8)

##############
# 3. (20pts)
# Calculate a matrix containing columns with the symbols of ETFs,
# sorted from the highest to lowest alpha in each year,
# and call it "top_etf"
# use functions apply() and order(),
top_etf <- apply(ann_alphas, 2, function(ann_alpha, ...) {
  rownames(ann_alphas)[order(ann_alpha, ...)]
}, decreasing=TRUE)

# or simply:
top_etf <- apply(ann_alphas, 2, function(ann_alpha) {
  rownames(ann_alphas)[order(ann_alpha, decreasing=TRUE)]
})




##############################
# active investment strategies
##############################

############## hw
# Summary: Simulate a trading strategy based on two VWAPs.

# Download the file etf_data.RData from NYU Classes,
# and load() it.
# etf_data.RData contains an environment called env_data,
# with OHLC time series data for ETFs, including VTI.

library(quantmod)
load(file="C:/Develop/data/etf_data.RData")

# 1. (5pts) Define two integer windows (lookback intervals) called
# "win_short=10" and "win_long=100".
# Calculate two vectors of VWAPs called "vwap_short" and "vwap_long",
# for the VTI OHLC data.
# You must use function v_wap() from the previous homework,

win_short <- 10
win_long <- 100
vwap_short <- v_wap(x_ts=env_data$VTI, win_dow=win_short)
vwap_long <- v_wap(x_ts=env_data$VTI, win_dow=win_long)

# Calculate a numeric vector called ma_indic, that is
# equal to 1 when "vwap_short > vwap_long" and equal to -1
# when "vwap_short < vwap_long",
# The sign of ma_indic will determine the strategy's risk
# positions, either long risk or short risk.
# You can use function sign(),

ma_indic <- sign(vwap_short - vwap_long)

# 2. (10pts) Calculate a boolean vector that is TRUE only on dates
# right after the VWAPs have crossed, and call it ma_crosses.
# For example, if yesterday "vwap_short < vwap_long" and today
# "vwap_short > vwap_long" (or vice versa), then today ma_crosses
# should be TRUE, and otherwise it should be FALSE.
# hint: the diff() of ma_indic is not zero right after the
# VWAPs have crossed, and otherwise it's zero.
# you can use the functions sign(), diff(), and is.na(),
# and the logical operator "!=",
# set any NAs to FALSE,

ma_crosses <- (diff(ma_indic) != 0)
ma_crosses[is.na(ma_crosses)] <- FALSE

# The strategy should perform trades after ma_crosses becomes TRUE,
# but with a one period lag, to reflect that in practice it's
# impossible to trade immediately.
# Calculate a vector of integer indices corresponding to trade
# dates, and call it "trade_dates".
# hint: first calculate the indices corresponding to the points when
# ma_crosses is TRUE, and add "1" to them, to reflect the one period lag.
# you can use function which(),

trade_dates <- which(ma_crosses)+1

# The strategy invests in a fixed number of shares called "num_shares".

num_shares <- 100

# The strategy either owns "num_shares" number of shares (long position),
# or sells the same number of shares short (short position).
# Thus the strategy consists of consecutive time intervals of long risk
# and short risk positions, depending on the sign of ma_indic.
# When ma_indic becomes positive then the strategy buys shares and
# flips to a long risk position, and vice versa.

# 3. (20pts) The strategy should be simulated over a number of points
# of time called "n_periods", which should be equal to the number of
# rows in the OHLC time series data,
# you can use function nrow(),

n_periods <- nrow(env_data$VTI)

# Calculate a numeric vector called "pos_ition", that is equal to the
# number of shares owned by the strategy at any given point of time,
# either positive (long risk position) or negative (short risk position).
# The strategy should start with a position of zero.
# The strategy position should be reset on "trade_dates", depending on
# the sign of ma_indic.
# The strategy position should remain unchamged between the "trade_dates".
# you can use functions numeric() and na.locf(),

pos_ition <- NA*numeric(length=n_periods)
pos_ition[1] <- 0
pos_ition[trade_dates] <- num_shares*ma_indic[trade_dates]
pos_ition <- na.locf(pos_ition)

# Lag the vector "pos_ition" by one period and call it "lag_position".
# The first value of "lag_position" should be zero.
# you can use function c() combined with subsetting "[]",

lag_position <- c(0, pos_ition[-n_periods])

# you can inspect the vectors by merging and subsetting them:

foo <- merge(Ad(env_data$VTI), vwap_short, vwap_long, ma_crosses, pos_ition)
colnames(foo) <- c("price", "vwap_short", "vwap_long", ma_crosses, "pos_ition")
foo[(which(ma_crosses)[2]-3):(which(ma_crosses)[2]+3), ]

# You should get the following output:
#               price vwap_short vwap_long ma_crosses pos_ition
# 2007-02-27 59.09827   60.62776  60.13491        0       100
# 2007-02-28 59.56461   60.42235  60.10604        0       100
# 2007-03-01 59.39503   60.17368  60.06567        0       100
# 2007-03-02 58.54714   59.98786  60.03648        1       100
# 2007-03-05 57.86882   59.65299  59.95280        0      -100
# 2007-03-06 58.84390   59.47699  59.92486        0      -100
# 2007-03-07 58.79727   59.32994  59.90830        0      -100

# Calculate a vector of adjusted close prices from the OHLC data,
# and call it price_s.
# Calculate a vector of lagged price_s, and call it "lag_prices".
# Calculate a vector of open prices from the OHLC data,
# and call it "open_prices".
# you can use functions Ad(), Op(), and c(),

price_s <- Ad(env_data$VTI)
lag_prices <- c(0, price_s[-n_periods])
open_prices <- Op(env_data$VTI)

# 4. (20pts) Calculate a vector of periodic (daily) profits and
# losses and call it "pn_l".
# The periodic (day over day) profit or loss (pnl) for a point
# without any trade, is equal to the position in the previous
# point, times the difference between this point's closing
# price minus the previous point's closing price.
# The periodic pnl for a point with a trade, is equal to the
# sum of two terms.
# The first term is equal to the position in the previous
# point, times the difference between this point's opening
# price minus the previous point's closing price.
# The first term represents the realized pnl after trading
# out of the previous position.
# The second term is equal to the current (new) position times
# the difference between this point's closing minus opening
# prices.
# The second term represents the unrealized pnl of the new
# position on the day of the trade.
# you can use the vectors price_s, "lag_prices", "open_prices",
# "pos_ition", "lag_position" and "trade_dates",

pn_l <- lag_position*(price_s - lag_prices)
pn_l[trade_dates] <-
  lag_position[trade_dates]*(open_prices[trade_dates] - lag_prices[trade_dates]) +
  pos_ition[trade_dates]*(price_s[trade_dates] - open_prices[trade_dates])

# you can inspect the vectors by merging and subsetting them:

foo <- merge(Ad(env_data$VTI), lag_prices, open_prices, pos_ition, lag_position, pn_l)
colnames(foo) <- c("price", "lag_prices", "open_prices", "pos_ition", "lag_position", "pn_l")
foo[(which(ma_crosses)[2]-3):(which(ma_crosses)[2]+3), ]

# You should get the following output:
#               price lag_prices open_prices pos_ition lag_position       pn_l
# 2007-02-27 59.09827   61.37487    60.68807       100          100 -227.66000
# 2007-02-28 59.56461   59.09827    59.36535       100          100   46.63400
# 2007-03-01 59.39503   59.56461    58.48778       100          100  -16.95800
# 2007-03-02 58.54714   59.39503    59.16186       100          100  -84.78900
# 2007-03-05 57.86882   58.54714    57.99600      -100          100  -42.39516
# 2007-03-06 58.84390   57.86882    58.37756      -100         -100  -97.50800
# 2007-03-07 58.79727   58.84390    58.92021      -100         -100    4.66300


# Calculate the Sharpe ratio of the strategy returns "pn_l",
# given by the sum() of "pn_l" divided by the sd() of "pn_l".

sum(pn_l)/sd(pn_l)

# Create an xts series from "pos_ition" and call it "xts_position".
# you can use functions xts() and index(),

xts_position <- xts(pos_ition, order.by=index((env_data$VTI)))

# Create an xts series from the cumulative sum of "pn_l"
# and call it "xts_pnl".
# you can use functions xts(), cumsum(), and index(),

xts_pnl <- xts(cumsum(pn_l), order.by=index((env_data$VTI)))

# 5. (20pts) Plot the time series of prices and the strategy pnl
# in two panels.
# open plot graphics device using function x11(),
# set plot parameters using function par() with argument "mfrow",

x11()
par(mfrow=c(2, 1))

# Plot in the top panel the adjusted close prices of the OHLC data,
# add "vwap_long" to the plot,
# add background shading of areas corresponding to long positions
# in "lightgreen" and short positions in "lightgrey".
# hint: call chart_Series() once, and then call add_TA() three times,
# wrap the first three calls in invisible() to prevent plotting,
# except for the last add_TA() call.
# you must use functions Ad(), chart_Series(), add_TA()
# (with "on" parameter), and invisible(),
# you can use the xts series "xts_position" for shading,
# You can adapt code from the "time_series_univariate" pdf and R files.
# Be sure to download the most recent version.

invisible(chart_Series(x=Ad(env_data$VTI), name="VTI prices"))
invisible(add_TA(vwap_long, on=1, col='red'))
invisible(add_TA(xts_position > 0, on=-1,
                 col="lightgreen", border="lightgreen"))
add_TA(xts_position < 0, on=-1,
       col="lightgrey", border="lightgrey")

# Plot in the bottom panel "xts_pnl",
# add background shading of areas as before.
# you must use functions chart_Series(), add_TA()
# (with "on" parameter), and invisible(),

invisible(chart_Series(x=xts_pnl, name="VTI strategy"))
invisible(add_TA(xts_position > 0, on=-1,
                 col="lightgreen", border="lightgreen"))
add_TA(xts_position < 0, on=-1,
       col="lightgrey", border="lightgrey")


# 6. (10pts) Create a function called run_vwap() which performs
# a simulation of a trading strategy based on two VWAPs (as above),
# and returns the Sharpe ratio of the strategy returns,
# The function run_vwap() should accept three arguments:
#  "win_short" and "win_long" - two integer lookback windows,
#  da_ta - OHLC time series data,
# hint: combine all the code from the previous parts.

run_vwap <- function(win_short=10, win_long=100, da_ta) {
  vwap_short <- v_wap(x_ts=da_ta, win_dow=win_short)
  vwap_long <- v_wap(x_ts=da_ta, win_dow=win_long)
  ma_indic <- sign(vwap_short - vwap_long)
  ma_crosses <- (diff(ma_indic) != 0)
  ma_crosses[is.na(ma_crosses)] <- FALSE
  trade_dates <- which(ma_crosses)+1
  num_shares <- 100
  n_periods <- nrow(da_ta)
  pos_ition <- NA*numeric(length=n_periods)
  pos_ition[1] <- 0
  pos_ition[trade_dates] <- num_shares*ma_indic[trade_dates]
  pos_ition <- na.locf(pos_ition)
  lag_position <- c(0, pos_ition[-n_periods])
  price_s <- Ad(da_ta)
  lag_prices <- c(0, price_s[-n_periods])
  open_prices <- Op(da_ta)
  pn_l <- lag_position*(price_s - lag_prices)
  pn_l[trade_dates] <-
    lag_position[trade_dates]*(open_prices[trade_dates] - lag_prices[trade_dates]) +
    pos_ition[trade_dates]*(price_s[trade_dates] - open_prices[trade_dates])
  sum(pn_l)/sd(pn_l)
}  # end run_vwap

# call run_vwap() as follows, to verify it works correctly,

run_vwap(win_short=40, win_long=350, da_ta=env_data$VTI)

# 7. (10pts)
# Create a named vector of integer values for "win_short" from=30, to=100, by=10
# called "short_windows", with the following values:
# sh30  sh40  sh50  sh60  sh70  sh80  sh90 sh100
#   30    40    50    60    70    80    90   100
# Create a named vector of integer values for "win_long" from=200, to=400, by=25
# called "long_windows", with the following values:
# lo200 lo225 lo250 lo275 lo300 lo325 lo350 lo375 lo400
#   200   225   250   275   300   325   350   375   400
# you can use functions seq(), paste0(), names(), and structure(),

short_windows <- seq(from=30, to=100, by=10)
names(short_windows) <- paste0("sh", short_windows)
# or
short_windows <- structure(short_windows, names=paste0("sh", short_windows))

long_windows <- seq(from=200, to=400, by=25)
names(long_windows) <- paste0("lo", long_windows)
# or
long_windows <- structure(long_windows, names=paste0("lo", long_windows))

# perform two nested sapply() loops calling the function run_vwap(),
# first over "short_windows", second over "long_windows".
# To get full credit you must pass the arguments "da_ta=env_data$VTI"
# into run_vwap() through the dots argument of the sapply() functions,
# you can use an anonymous function,
# the output should ba a named matrix called "mat_rix" as follows
# (transpose will also get full credit):
#           sh30      sh40      sh50      sh60     sh70     sh80     sh90    sh100
# lo200 84.06524  91.24971  93.77047  92.01054 87.79107 76.23158 57.91753 74.07076
# lo225 72.06119  97.77155  92.31084  88.33317 76.08169 75.71970 41.36928 38.56718
# lo250 86.73914  88.73794  91.26042  95.83383 82.29601 67.69120 61.48578 43.94475
# lo275 84.42591  95.18654  92.79157  81.29927 86.75097 55.87239 55.67067 60.19943
# lo300 90.90760  94.00829 103.63414 108.20157 84.86396 64.79328 49.14296 64.55424
# lo325 92.71081 105.49712  99.21752 108.44899 72.44130 66.29816 65.10376 73.93077
# lo350 95.65559 113.38591 112.83352  87.72392 85.38218 70.43943 66.78933 79.28444
# lo375 89.53747 121.43885 105.97034  94.08813 88.17621 71.61272 71.60992 64.63382
# lo400 91.10256 115.04823  95.78384  99.42304 89.05842 78.38954 67.25607 82.20783

mat_rix <- sapply(short_windows,
                  function(win_short, da_ta)
                    sapply(long_windows,
                           run_vwap,
                           win_short=win_short,
                           da_ta=da_ta),
                  da_ta=env_data$VTI)

# 8. (5pts) Draw an interactive 3d surface plot of "mat_rix".
# you can use function persp3d(),
# You can adapt code from the "plotting" pdf and R files.
# Be sure to download the most recent version.

library(rgl)  # load rgl
persp3d(z=mat_rix, col="green", x=short_windows, y=long_windows)



############## hw
# compare returns of active strategy vs static portfolio
#
# 1. (20pts) Load time series data and calculate the returns from
# backtesting the active investment strategy, called "pnl_xts",
# use the code from lecture #6,
load(file="C:/Develop/data/etf_rets.RData")
library(xts)

### run the active investment strategy code from lecture #6,


# calculate the cumulative returns of the strategy "pnl_xts",
# and the cumulative returns of "etf_rets[, "VTI"]",
# merge the VTI returns with strategy returns, and call it "cum_rets",
# the result of merge should be an xts with two columns,
# be careful about the order of cumsum and merge operations,
# since the indices of etf_rets and "pnl_xts" have different frequencies,
# be sure to omit NAs, and to start the cumulative VTI returns from zero,
# use functions cumsum(), merge() and na.omit(),
vti_rets <- cumsum(etf_rets[, "VTI"])[index(pnl_xts)]
vti_rets <- vti_rets - as.numeric(vti_rets[1, ])
cum_rets <- na.omit(merge(cumsum(pnl_xts[, "pnl"]), vti_rets))
head(cum_rets)


# plot "cum_rets" using generic function plot(), and add a legend,
plot(cum_rets, main="Min vol vs VTI")

legend(x="bottomright", legend=c("Min vol strategy", "VTI"),
       inset=0.2, cex=0.8, bg="white",
       lwd=2, lty=c(1, 1), col=c("black", "red"))


# 2. (10pts) Calculate the risk/return statistics of "etf_rets[, "VTI"]"
# and "pnl_xts",
# be careful to perform the calculations using the same date index,
# since the indices of etf_rets and "pnl_xts" have different frequencies,
# hint: use "cum_rets" as a starting point,
# use functions diff(), SharpeRatio(), SortinoRatio(), and CalmarRatio(),
daily_rets <- diff(cum_rets)[-1]
library(PerformanceAnalytics)
SharpeRatio(daily_rets)
SortinoRatio(daily_rets)
CalmarRatio(daily_rets)


# 3. (20pts) Modify the function "pnl_period" so that the weights
# are proportional to the inverse of "period_stat[, "risk"]",
# and scaled by the sum of absolute values,
# recalculate the returns called "pnl_xts",
# perform points #1 and #2 above, using the new "pnl_xts",
# paste all of your code in this file, so it can be run in sequence,
pnl_period <- function(period_stat, de_mean=FALSE) {
  weights <- 1/period_stat[, "risk"]
  weights <- weights - de_mean*mean(weights)
  weights <- weights/sum(abs(weights))
  c(sum(period_stat[, "fut_ret"]*weights), weights)
}  # end pnl_period



############## hw
# perform data mining for seasonal strategies and illustrate false discovery rates
#
# 1. generate all possible seasonal strategies for buying or shorting SPX in different months of the year
# these strategies are similar to the Sell in May Halloween seasonal strategy,
# calculate a histogram of cumulative returns and Sharpe ratios for all the different strategies,
# find the best performing strategy,
# compare the performance of the best performing strategy with the Sell in May Halloween strategy,
# create a time series of random returns and apply the above steps,
# compare the best performing strategy for random returns with the best performing strategy for SPX,
# create a matrix of random numbers and reuse it,
# Perform the simulations without using any apply() loops, only using vectorized functions,

# 1a. increase model parameter space by using weeks instead of months,
# demonstrate that now the best performing strategy performs even better,

# 2. simulate multiple time series of random returns with the same properties as SPX (reshuffle?)
# apply the Halloween strategy on the simulated data,
# calculate a histogram of cumulative returns and Sharpe ratios for all the different data sets,
# find the best performing data set,
# compare the performance of the best performing data set with the SPX strategy,

# 3. now select one of the simulated data sets from 2. at random,
# and then find the optimal seasonal strategy from 1. for which it performs the best,
# this would lead us to believe that we have found an anomaly, whereas in fact we are just data mining,
# for any random data set, there is always a corresponding strategy for which it performs the best,
# Harvey Factor Model Data Mining Bonferroni Adjustment.pdf
# http://www.alexchinco.com/screening-using-false-discovery-rates/
# http://eranraviv.com/modern-statistical-discoveries/

# 4. demonstrate that larger model parameter space increases chance of identifying spurious patterns (false discovery rates)
# repeat 3. using weekly strategies from 1a.
# demonstrate that now the best performing strategy performs even better,

# 5. demonstrate that cross validation helps reduce the false discovery rate, but only to some extent,
# repeat 3. but divide the time series in half, and find the optimal in-sample strategy,
# then demonstrate that this optimal strategy underperforms out-of-sample,
# this demonstrates that cross validation helps reduce the false discovery rates,
# but it's also possible to select a strategy that also performs well out-of-sample,
# this strategy isn't optimal in-sample,
# is it possible then that the Sell in May strategy is just such a strategy?
# so cross validation helps reduce the false discovery rate,
# buy avoiding selecting the optimal in-sample strategy,

# we would need an infinite length of time series data to fully eliminate the false discovery rate, even using cross validation,
# perform a simulation study to find how the false discovery rate drops as we increase the length of the data,



##############################
# numerical analysis
##############################

############## hw - most is already incorporated into lecture slides
# Study effect of noise on the quality of a regression,
#
# 1. (20pts) Create a function called reg_stats() which performs
# a regression and returns a vector of regression statistics,
# the function reg_stats() should accept a single argument std_dev,
# the function reg_stats() should perform the following steps:
# - initialize the random number generator by calling set.seed(1121),
# - create an explanatory variable of length 30, as follows:
  explana_tory <- seq(from=0.1, to=3.0, by=0.1)
# - calculate a response variable as follows:
  res_ponse <- 3 + 0.2*explana_tory + rnorm(30, sd=std_dev)
# - perform a regression between "res_ponse" and "explana_tory",
# - extract from summary() the regression statistics:
#   p-value, adj.r.squared, fstatistic,
# - create a named vector of the regression statistics, and return it,
# hint: you can use the code from the slide titled "Weak Regression",
reg_stats <- function(std_dev) {
# initialize random number generator
  set.seed(1121)
# create explanatory variable
  explana_tory <- seq(from=0.1, to=3.0, by=0.1)
# calculate response variable
  res_ponse <- 3 + 0.2*explana_tory + rnorm(30, sd=std_dev)
# specify regression formula
  reg_formula <- res_ponse ~ explana_tory
# perform regression
  reg_model <- lm(reg_formula)
# calculate regression summary
  reg_model_sum <- summary(reg_model)
# extract regression statistics
  c(pval=reg_model_sum$coefficients[2, 4],
    adj_rsquared=reg_model_sum$adj.r.squared,
    fstat=reg_model_sum$fstatistic[1])
}  # end reg_stats

# apply reg_stats() as follows, to verify it works correctly:
reg_stats(0.1)
reg_stats(1.0)


# 2. (10pts) Create a vector of 10 std_dev values
# from=0.1, to=1.0, and call it vec_sd,
# hint: you can use function seq(),
vec_sd <- seq(from=0.1, to=1.0, length.out=10)

# add the following names to vec_sd: "sd=0.1", "sd=0.2", etc.
# use functions names() and paste0(),
names(vec_sd) <- paste0("sd=", vec_sd)


# apply the function reg_stats() to the vector vec_sd,
# and call it "mat_reg_stats", the first row of "reg_stats" should
# contain the "p-value", the second row should contain
# "adj.r.squared", and third row "fstat",
# use function sapply(),
mat_reg_stats <- sapply(vec_sd, reg_stats)
mat_reg_stats <- t(mat_reg_stats)

# 3. (20pts) plot the three rows of "reg_stats" in three panels
# arranged vertically, write a for() loop and
# call functions par() with "mfrow" argument,
# plot() with "xaxt" argument,
# and axis() with "at" and labels" arguments,
# create custom x-axes with labels equal to colnames(reg_stats),
# read the links below explaining how to create custom axes
# using the axis() function:
# http://www.statmethods.net/advgraphs/axes.html
# http://stackoverflow.com/questions/11775692/how-to-specify-the-actual-x-axis-values-to-plot-as-x-axis-ticks-in-r
# http://stackoverflow.com/questions/5182238/r-replace-x-axis-with-own-values
par(mfrow=c(ncol(mat_reg_stats), 1))
# plot in loop
for (in_dex in 1:ncol(mat_reg_stats)) {
  plot(mat_reg_stats[, in_dex], type="l", xaxt="n",
           xlab="", ylab=colnames(mat_reg_stats)[in_dex])
  axis(1, at=1:(nrow(mat_reg_stats)), labels=rownames(mat_reg_stats))
}  # end for




##############
# 1. (10pts) optimization of log-likelihood objective function using optim(),
# Create a log-likelihood objective function for a mixture of two normal distributions,
# assume that the two distributions both have sd=1.0, but different means,
# the target vector is a sample from the mixture of two normal distributions,
target_vector <- c(rnorm(300, mean=0.0, sd=1.0),
                   rnorm(100, mean=4.0, sd=1.0))
# the objective function should be a function of a vector of three parameters:
# weight, mean1, and mean2,
# and also a function of the sample "target_vector",
object_ive <- function(parm, target) {
  likelihood <- parm[1] * dnorm(target-parm[2]) +
    (1-parm[1])*dnorm(target-parm[3])
  if(any(likelihood <= 0)) Inf else
    -sum(log(likelihood))
}  # end object_ive


# 2. (10pts) create a vectorized objective function, by vectorizing the mean1 and mean2 parameters,
vec_objective <- Vectorize(
  FUN=function(mean1, mean2, w, target)
    object_ive(c(w, mean1, mean2), target),
  vectorize.args=c("mean1", "mean2")
)  # end Vectorize


# 3. (10pts) create vectors of mean1 and mean2 parameters, with 50 values from -10, to 10,
mean1 <- seq(-10, 10, length=50)
mean2 <- seq(-10, 10, length=50)

# calculate the objective function on a parameter grid made from the two vectors,
# set the weight=0.3,
objective_grid <- outer(mean1, mean2,
                        vec_objective,
                        target=target_vector, w=0.3)
rownames(objective_grid) <- round(mean1, 2)
colnames(objective_grid) <- round(mean2, 2)

# create a perspective plot of the objective function,
persp(mean1, mean2, -objective_grid,
      theta = 45, phi = 30,
      shade = 0.5,
      col = rainbow(50),
      border = "green",
      main = "objective function")


# 4. (10pts) perform optimization using the function "optim",
# to find the optimal parameters: weight, mean1, and mean2,
# set the "upper" and "lower" parameter limits to c(1,10,10) and c(0,-10,-10),
# set the initial parameters to:
par_init <- c(weight=0.99, mean1=0, mean2=0)
# perform optimization
optim_run <- optim(par=par_init,
                   fn=object_ive,
                   target=target_vector,
                   method="L-BFGS-B",
                   upper=c(1,10,10),
                   lower=c(0,-10,-10))


# 5. (5pts) plot the histogram of "target_vector",
histo_gram <- hist(target_vector, plot=FALSE)
plot(histo_gram, freq=FALSE,
     main="histogram of target vector")

# plot the mixture distribution with the fitted parameters,
fit_func <- function(x, parm) {
  parm["weight"] * dnorm(x, mean=parm["mean1"]) +
    (1-parm["weight"]) * dnorm(x, mean=parm["mean2"])
}  # end fit_func
curve(expr=fit_func(x, parm=optim_run$par), add=TRUE,
      type="l", lwd=2, col="red")
legend("topright", inset=0.0, cex=0.8, title=NULL,
       leg="optimal parameters",
       lwd=2, bg="white", col="red")




##############
# miscellaneous scratchpad stuff


# create a vector of dates corresponding to end_points,
# and call it in_dex,
# use function index(),
in_dex <- index(zoo_series[end_points, ])

# create from in_dex a vector of dates lagged by 3 months,
# and call it "in_dex_lag",
# use function lag.xts() from package xts,
in_dex_lag <- lag.xts(in_dex, 3)

# between monthly prices
# and the 3-month lagged prices of xts_series,

# create a vector of monthly end points lagged by 3 months,
# and cbind() it to end_points,
# remove rows containing NAs,
# end_points should now have two columns:
# the first column should contain monthly indices (integers),
# the second column should contain monthly indices that are lagged by 3 months,
# use function lag.xts() from package xts, and functions cbind() and na.omit(),
end_points <- cbind(end_points, lag.xts(end_points, 3))
end_points <- na.omit(end_points)



############## deprecated old stuff ##############
# 1. (10pts)
# Download from Yahoo the "AdjClose" prices and "Volume" for
# MSFT stock, starting from Jun/01/2007, and call it "zoo_msft",
# use tseries function get.hist.quote(),

library(tseries)  # load package tseries
library(zoo)  # load package zoo

# load MSFT data
zoo_msft <- suppressWarnings(
  get.hist.quote(
    instrument="MSFT",
    start=as.Date("2007-06-01"),
    end=Sys.Date(),
    quote=c("AdjClose","Volume"),
    origin="1970-01-01")
)  # end suppressWarnings

# 2. (10pts) Plot price_s columns "AdjClose" and "50d_ma" in the same panel,
# starting from "2015-01-01", in the colors black and red,
# you must use method plot.zoo() with the proper argument "plot.type",
# add a legend and position it so that it doesn't obscure the plot too much,

start_date <- as.Date("2015-01-01")
plot(price_s[(index(price_s)>start_date),
             c("AdjClose", "50d_ma")],
     main="MSFT Prices and 50d_ma",
     xlab="", ylab="", plot.type="single",
     col=c("black", "red"))
# add legend
legend("bottomright", inset=0.05, cex=0.5,
       title="MSFT Prices and 50d_ma",
       leg=c("MSFT", "50d_ma"), lwd=2, bg="white",
       col=c("black", "red"))


############## deprecated ##############
