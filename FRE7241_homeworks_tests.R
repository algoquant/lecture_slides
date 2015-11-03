#################################
### Homework and test ideas
#################################


##################################
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



#################################
# data munging input output error handling
#################################


# list all files in "env_data",
sym_bols <- ls(env_data)
# list files named "*_rets"
ls(env_data, pattern=glob2rx("*_rets"))
# remove files named "*_rets"
rm(list=ls(env_data, pattern=glob2rx("*_rets")), 
   envir=env_data)



#################################
# dates and times
#################################

############## test
# 1. (30pts) "my_date" is a numeric date that represents "1997-05-18",
# convert my_date to a POSIXct date, with time zone equal to "America/New_York",
# you can use functions from package lubridate, 
# or other functions such as paste() and substr(),
# you will need to add the century "19" to the year,
# you will not receive any credit for creating a date "by hand" as follows: as.POSIXct("1997-05-18"),
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
# convert them to POSIXct dates, using a "lubridate" function,
library(lubridate)
date_time <- 2014 + (1:5)/12
date_decimal(date_time)
# or
date_decimal(date_time, tz="America/New_York")



############## test
# create a vector of weekly dates of class "Date" corresponding 
# to every Monday, starting with "2013-09-02", until the 
# most recent Monday,
# use two different methods,
# first, use functions as.Date(), Sys.Date(), and seq() with "by" argument,
mon_days <- seq(from=as.Date("2013-09-02"), 
                to=Sys.Date(), by="week")
mon_days <- weekdays(mon_days)

# second, use functions Sys.setenv(), as.POSIXct(), difftime() and ceiling(), 
# and lubridate function weeks(),
library(lubridate)
start_date <- as.Date("2013-09-02")
end_date <- Sys.Date()
num_weeks <- ceiling(difftime(end_date, start_date, units="weeks"))
mon_days <- start_date + weeks(0:num_weeks)
# subset - just to be sure
mon_days <- mon_days[(mon_days <= end_date)]



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
# download the file "zoo_series.Rdata" from NYU Classes, and load() it, 
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



############## test
# Summary: subset daily stock prices to dates at the end of the week, 
# and calculate percentage returns over past four week periods.

# 1. (15pts) 
# download the file "etf_data.Rdata" from NYU Classes, and load() it. 

library(quantmod)
load(file="C:/Develop/data/etf_data.Rdata")

# "etf_data.Rdata" contains an environment called "env_data", 
# with OHLC time series data for ETFs, including "VTI". 
# Extract the adjusted close prices from "VTI" into a variable 
# called "stock_prices".
# you can use function Ad(), 

stock_prices <- Ad(env_data$VTI)

# subset "stock_prices" to the end of the week dates. 
# you can use function endpoints() with the "on" argument, 

stock_prices <- stock_prices[endpoints(stock_prices, on="weeks"), ]

# verify that the dates of "stock_prices" are indeed "Friday" 
# (except for some holidays), 
# by printing the tail of the index of "stock_prices", and converting 
# it to days of the week, 
# you can use functions weekdays(), index(), and tail(), 

weekdays(index(tail(stock_prices)))

# calculate percentage returns from "stock_prices", over trailing  
# four week periods, and call it "stock_rets", 
# you can use functions log() and diff() with the "lag" argument, 

stock_rets <- diff(log(stock_prices), lag=4)

# 2. (15pts) 
# extract the highest and lowest returns, and their associated dates, 
# you can use functions index, which(), or which.max() and which.min, 

stock_rets[which.max(stock_rets)]
index(stock_rets[which.max(stock_rets)])
stock_rets[which.min(stock_rets)]
index(stock_rets[which.min(stock_rets)])



############## hw
# 1. (15pts) create a vector of weekly "POSIXct" dates corresponding  
# to Mondays at 09:30AM, and call it "mon_days", 
# start with the date "2015-02-09", and end at the most recent Monday
# before today (today is defined by Sys.time()),
# set the timezone to "America/New_York", 
# first calculate the number of weeks between today and the start date,
# and use that number to create a vector of weekly "POSIXct" dates,
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




#################################
# time series management
#################################


############## test
# 1. (20pts) create a function called run_sum() that calculates the 
# running sum of an xts series over a sliding window (lookback period). 
# run_sum() should accept two arguments: 
# "x_ts" - an xts series containing one or more columns of data, 
# "win_dow" - an integer specifying the number of lookback periods. 
# run_sum() should return an xts series with the same dimensions 
# as the input xts series. 
# For example, if win_dow=3, then the running sum at any point should 
# be equal to the sum of "x_ts" values for that point plus two preceding 
# points. 
# The initial values of run_sum() should be equal to cumsum() values, 
# so that run_sum() doesn't return any NA values.
# You must use use vectorized functions, such as cumsum() and lag(), 
# not loops, 
# you cannot use for() or apply() loops, 

run_sum <- function(x_ts, win_dow) {
  cum_sum <- cumsum(x_ts)
  out_put <- cum_sum - lag(x=cum_sum, k=win_dow)
  out_put[1:win_dow, ] <- cum_sum[1:win_dow, ]
  out_put
}  # end run_sum

# download the file "etf_data.Rdata" from NYU Classes, and load() it. 
# "etf_data.Rdata" contains an environment called "env_data", 
# with OHLC time series data for ETFs, including "VTI". 
# call run_sum() as follows, to verify it works correctly,

library(xts)
load(file="C:/Develop/data/etf_data.Rdata")
foo <- run_sum(x_ts=env_data$VTI[, "VTI.Volume"], win_dow=3)
head(foo)



############## hw
# 1. (35pts) Create a function called lag_it() that applies a lag to vectors 
# and "zoo" time series objects,
# lag_it() should accept two arguments:
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
# you can use functions is.vector(), is.zoo(), is.numeric(), lag.zoo(), 
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
library("Ecdat")  # load econometric data sets
data_ny <- Cigarette[Cigarette[, "state"]=="NY", ]

# the column dates_ny$year contains years as strings in the format "yyyy",
# from the column dates_ny$year create a vector of "Date" dates in the format "yyyy-01-01", 
# and call it "dates_ny", use function paste(),
dates_ny <- as.Date(paste(data_ny$year, "-01-01", sep=""))

# Create a "zoo" from data_ny, excluding the columns "state" and "year", 
# and the index "dates_ny", and call it "zoo_ny", 
library("zoo")
zoo_ny <- zoo(x=data_ny[, -(1:2)], order.by=dates_ny)

# plot the column "income", and add title "Cigarette tax income in NY state",
plot(zoo_ny[, "income"], xlab="", ylab="", main="Cigarette tax income in NY state")


############## test
# Convert integers representing dates to "POSIXct" date-time objects,

# Load the package "Ecdat", which contains a data frame called "Yen",
# the column Yen$date contains integers representing dates, in the 
# format "yyyymmdd", 
# from the column Yen$date create a vector of "POSIXct" dates, and 
# call it "in_dex", and set the "in_dex" timezone to "UTC", 
# you must perform this using two different methods, 

library("Ecdat")  # load Ecdat
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
# Create an xts object from the column Yen$s and "in_dex", 
# and call it "xts_yen",

library(xts)
xts_yen <- xts(Yen$s, order.by=in_dex)

# plot "xts_yen", using generic function plot(),
plot(xts_yen)


############## test
# 2. (20pts) The package "Ecdat" contains a data frame called "Garch".
# the column Garch$date contains dates as numeric values in the format "yymmdd",
# create a vector of "Date" dates from Garch$date, and call it "in_dex", 
# use functions paste() and as.Date(), with the proper "format" argument,
library("Ecdat")  # load econometric data sets
head(Garch)  # explore the data
in_dex <- as.Date(paste0(19, Garch$date), format="%Y%m%d")

# use three different methods to create a vector of "POSIXct" dates 
# from Garch$date, and call it "in_dex", 
# 
# first method: 
# use functions paste(), substr(), and as.POSIXct(), with default "format" argument,
# set the timezone to "America/New_York", 
# hint: first extract substrings (this converts numeric to character strings),
# then paste them together, and then apply function as.POSIXct(),
in_dex <- as.POSIXct(
  paste(paste0(19, substr(Garch$date, 1, 2)), 
        substr(Garch$date, 3, 4), 
        substr(Garch$date, 5, 6), sep="-"), 
  tz="America/New_York")

# second method: 
# use functions paste() and as.POSIXct(), with the proper "format" argument,
# set the timezone to "America/New_York", 
in_dex <- as.POSIXct(paste0(19, Garch$date), format="%Y%m%d", 
                     tz="America/New_York")

# third method: 
# use function ymd() from package lubridate,
# set the timezone to "America/New_York", 
in_dex <- ymd(Garch$date, tz="America/New_York")

# Create a "zoo" object from the columns Garch$dm, Garch$cd, and "in_dex", 
# and call it "zoo_series",
# use function zoo(),
zoo_series <- zoo(Garch[, c("dm", "cd")], order.by=in_dex)

# change the column names of zoo_series to "DMark" and "CAD"
colnames(zoo_series) <- c("DMark", "CAD")

# count the number of NA values in "zoo_series" using is.na(),
sum(is.na(zoo_series))

# remove any NAs in "zoo_series" using na.omit(),
zoo_series <- na.omit(zoo_series)

# plot "zoo_series", using the generic function plot(),
plot(zoo_series)

# save zoo_series to a comma-delimited text file called "Garch.csv", using write.zoo()
write.zoo(zoo_series, file="Garch.csv", sep=",")

# subset the "zoo_series" object to the dates from "1982-01-01" to "1986-01-01", 
# using either logical operators ">", "<", "&", "|" or function window(),
sub_index <- (in_dex > as.POSIXct("1982-01-01")) & (in_dex < as.POSIXct("1986-01-01"))
zoo_subseries <- zoo_series[sub_index]
# or
zoo_subseries <- window(zoo_series, start=as.POSIXct("1982-01-01"), end=as.POSIXct("1984-01-01"))

# calculate the running mean of the "DMark" column of zoo_series, using the function rollmean(), 
# aggregate values from the past over a window of 11 points, and call the result "zoo_mean",
zoo_mean <- rollmean(x=zoo_series[, "DMark"], k=11, align="right")

# merge the "DMark" column of zoo_series with zoo_mean, using the function merge(), 
# and copy it back to zoo_series,
zoo_series <- merge(zoo_series[, "DMark"], zoo_mean)

# change the column names of zoo_series to "DMark" and "mean_DMark"
colnames(zoo_series) <- c("DMark", "mean_DMark")

# calculate the number of rows with bad data in zoo_series, 
# and then scrub the bad data using the function na.locf(),
sum(!complete.cases(zoo_series))
zoo_series <- na.locf(zoo_series, fromLast=TRUE)

# plot zoo_series with two "y" axes, and add a legend containing the column names 
# of zoo_series,
# plot the first column
plot(zoo_series[, 1], xlab=NA, ylab=NA)
# specify range of second axis
par(usr=c(par("usr")[1:2], range(zoo_series[, 2])))
# plot second axis
axis(side=4, col="red")
# plot second column
lines(zoo_series[, 2], col="red")
# add legend
legend("bottomleft", legend=colnames(zoo_series), bg="white", lty=c(1, 1), lwd=c(2, 2), col=c("black", "red"), bty="n")



# most is already incorporated into lecture notes
############## test
# summary: subset 1-minute tick data to weekdays and trading hours.

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



############## test
# rolling window standard deviation of returns
# 1. (20pts) calculate the percentage (log) returns, from the "AdjClose" prices,
# merge the percentage returns with "zoo_msft" by adding them as the last column,
# rename the last column to "Rets",
# use functions diff(), log(), merge(), and colnames(),

zoo_rets <- diff(log(zoo_msft[, "AdjClose"]))
zoo_msft <- merge(zoo_msft, zoo_rets)
colnames(zoo_msft)[3] <- "Rets"

# calculate the 50-day rolling standard deviation of returns,
# merge the standard deviation with "zoo_msft" by adding it as the last column,
# rename the last column to "SD",
# use functions na.omit() and rollsd(), with the proper "align" argument, 
# so that the averages are taken from values in the past,

roll_sd <- rollsd(x=zoo_msft[, "Rets"], k=50, align="right")
zoo_msft <- na.omit(merge(zoo_msft, roll_sd))
colnames(zoo_msft)[4] <- "SD"



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
# to skip over the header when reading the data,
# also use the proper argument "stringsAsFactors" to avoid creating factors,
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

# convert the vector of strings into "POSIXct" in the UTC time zone, 
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
# 1. (15pts) Create a function that summarizes time series objects called str_ts(),
# The function input is a time series object,
# The function should return a named list object with the following information: length (nrow), dimensions, number of rows with bad data, colnames, the object's class, data type, and the first and last rows of data,
# The function should validate its argument, and throw an error if it's not a time series object,

str_ts <- function(ts_series=NULL) {
  # check if argument is a time series object
  stopifnot(is.ts(ts_series) || is.zoo(ts_series))
  # create list and return it
  list(
    length=ifelse(is.null(nrow(ts_series)), length(ts_series), nrow(ts_series)),
    dim=dim(ts_series),
    bad_data=sum(!complete.cases(ts_series)),
    col_names=colnames(ts_series),
    ts_class=class(ts_series),
    ts_type=typeof(ts_series),
    first_row=head(ts_series, 1),
    last_row=tail(ts_series, 1)
  )  # end list
}  # end str_ts


# 2. (10pts) Create a synthetic zoo time series of prices with two named columns, based on random returns equal to "rnorm",
# Introduce a few NA values into the time series, and call str_ts() on this time series,
library(zoo)  # load package zoo
ts_var <- zoo(matrix(rnorm(20), ncol=2), order.by=(Sys.Date() - 1:10))
colnames(ts_var) <- paste0("col", 1:2)
ts_var[3, 1] <- NA
ts_var[6, 2] <- NA
str_ts(ts_var)


# outline below for: 
# create function which calculates summary statistics of columns of zoo
# first split
# returns list of data frames (zoo)
split_eu <- split(as.zoo(EuStockMarkets), colnames(EuStockMarkets))

# then call lapply or call sapply
# does sapply return list? - yes - if vectors are not same length
# convert to matrix using do_call_rbind(), instead of this:
do.call(rbind, list.data)

out_lapply <- lapply(EuStockMarkets, mean)  # returns list of means
do.call(rbind, out_lapply)  # returns single column matrix
do.call(cbind, out_lapply)  # returns single row matrix
as.vector(out_lapply)
as.vector(do.call(cbind, out_lapply))
unlist(out_lapply)  # returns
class(unlist(out_lapply))
is.vector(unlist(out_lapply))
is.vector(do.call(cbind, out_lapply))
is.matrix(do.call(cbind, out_lapply))

# calculate summary statistics of columns of zoo, without split
sapply(colnames(EuStockMarkets), function (col_name) {
  str_ts(as.zoo(EuStockMarkets[, col_name]))
})



############## test
# Summary: Calculate the second moment of all the columns of EuStockMarkets, 
# using the functions sapply() and moment(),

# Calculate percentage returns of EuStockMarkets, 

rets_series <- 100*diff(log(EuStockMarkets))

# Load package "moments",
library("moments")

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
# you can use functions for(), apply(), and sapply(), 
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
library("moments")

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
# you can use functions colnames(), rownames(), and paste(), 

colnames(eu_moments) <- colnames(EuStockMarkets)
rownames(eu_moments) <- paste0("moment", or_ders)




##############################
# downloading scrubbing data
##############################


############## hw
# Summary: calculate intraday seasonality of trading volumes in high 
# frequency data. 
# Intraday seasonality means how the average trading volumes change at 
# different times of the day,

# load one minute bar data for the SPY ETF from the file SPY.Rdata. 
# The SPY data is an xts series containing OHLC prices and trading volumes 
# in one minute bars. 
# load package "xts", 

rm(list=ls())
library(xts)
load(file="C:/Develop/data/SPY.Rdata")

# Calculate the average trading volumes for every minute bar, 
# by aggregating the volumes for the same minute in all the days. 
# There are several methods of performing the aggregations, and you 
# must apply two different methods. 


# 1. (20pts) In the first method, perform aggregations using the 
# function tapply(). 

# Subset the SPY data to the month "2013-05" and call it "spy_sample", 
# this will reduce the time of future calculations. 

spy_sample <- SPY["2013-05"]

# if you get a warning message:
# "timezone of object (America/New_York) is different than current timezone"
# you can suppress it by calling:

options(xts_check_TZ=FALSE)

# Extract the time index of "spy_sample", and format it as a vector 
# of strings representing hours and minutes, and call it "in_dex", 
# you can use functions index() and format(). 

in_dex <- format(index(spy_sample), "%H:%M")

# perform a tapply() loop over the "spy_sample" data column "SPY.Volume", 
# using "in_dex" and function mean(), 
# call the output "vol_ume". 

vol_ume <- tapply(X=spy_sample[, "SPY.Volume"], INDEX=in_dex, FUN=mean)

# coerce "vol_ume" to a named vector of length equal to the number of 
# minutes in a single day of "spy_sample" data, 
# the names should be equal to the hours and minutes, in the 
# format "hours:minutes" (like "10:28"), 
# you can use functions as.vector(), names(), and structure(), 

vol_ume <- structure(as.vector(vol_ume), names=names(vol_ume))
is.vector(vol_ume)
head(vol_ume)


# 2. (20pts) in the second method, first subset "in_dex" to the number 
# of minutes in a single day of "spy_sample" data, 
# and call the output vector "time_of_day", 
# "time_of_day" should be a vector of strings representing hours and 
# minutes, like "10:28", 
# you can use "in_dex" and the function unique(), 

time_of_day <- unique(in_dex)

# perform an lapply() loop over the "spy_sample" data column "SPY.Volume", 
# using "time_of_day", the "==" logical operator, an anonymous function, 
# and the function mean(), 
# call the output "vol_ume". 

vol_ume <- lapply(time_of_day, function(mi_nute) {
  mean(spy_sample[in_dex==mi_nute, "SPY.Volume"])
})  # end lapply

# coerce "vol_ume" to a named vector of length equal to the number of 
# minutes in a single day of "spy_sample" data, 
# you can use "time_of_day", and the functions unlist() and structure(), 

vol_ume <- structure(unlist(vol_ume), names=time_of_day)


# 3. (10pts) benchmark the speed of the two methods of aggregating volumes, 
# use function microbenchmark() from package microbenchmark. 
# You can adapt code from the "numerical_analysis" pdf file. 

library(microbenchmark)
summary(microbenchmark(
  t_apply=
    tapply(X=spy_sample[, "SPY.Volume"], INDEX=in_dex, FUN=mean), 
  l_apply=
    lapply(time_of_day, function(mi_nute) {
      mean(spy_sample[in_dex==mi_nute, "SPY.Volume"])
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
# you can use function substring(), 

hour_s <- substring(time_of_day, 1, 2)

# extract from "time_of_day" the strings corresponding to hours, 
# call the output "tick_s", 
# "tick_s" should be a vector of strings like "10:00", "11:00", 
# you can use "hour_s" and functions unique() and match(), 

tick_s <- time_of_day[match(unique(hour_s), hour_s)]

# plot "vol_ume" with a custom x-axis, with tick marks at hourly points, 
# add vertical grey lines at tick marks,
# you can use "tick_s" and functions plot(), axis(), and abline(), 

a_t <- match(x=tick_s, table=time_of_day)
plot(vol_ume, xaxt="n", xlab=NA, ylab=NA, t="l")
axis(side=1, at=a_t, labels=tick_s, tcl=-0.7)
abline(v=a_t, col="grey", lwd=0.5)


# 5. (20pts) plot "vol_ume" using chart_Series() from package quantmod, 
# with a custom x-axis, with tick marks at different times of day. 

# coerce "vol_ume" to an xts series, with time index equal to "time_of_day". 
# of length equal to the number of 
# minutes in a single day of "spy_sample" data, 
# you can use "time_of_day", and the functions unlist() and structure(), 

# create a time index from today's date combined with "time_of_day", 
# and call it "in_dex".
# you can use functions paste(), Sys.Date(), and as.POSIXct(), 

in_dex <- as.POSIXct(paste(Sys.Date(), time_of_day))

# create an xts series from the vector "vol_ume" and "in_dex", 
# and call it "vol_ume".
# use function xts(), 

vol_ume <- xts(x=vol_ume, order.by=in_dex)

# load package "quantmod". 
# Extract the plot theme and call it "plot_theme".

library(quantmod)
plot_theme <- chart_theme()

# Modify "plot_theme", so the x-axis tick marks are in the 
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
# you can use function structure(), with attribute "fixed=TRUE". 
# Assign this object back to y_lim[[2]] (overwrite it), 

y_lim[[2]] <- structure(c(y_lim[[2]][1], y_lim[[2]][2]/2), fixed=TRUE)

# Modify the plot object "ch_ob", using the modified y_lim object, 
# use function ch_ob$set_ylim(), 

ch_ob$set_ylim(y_lim)

# Create the plot by calling the "ch_ob" object, 

ch_ob
# or
plot(ch_ob)



##############
# download two series: daily and monthly
# cbind the series and remove NAs


############## hw
# Summary: download data for multiple symbols using get.hist.quote()
# create a data directory on your computer, and save all files to that directory,
# remember the location of the data directory for future use,
# load package tseries,
library(tseries)

# 2. (5pts) read the ETF database file called "etf_list.csv" into a data frame called "etf_list", using read.csv(),
etf_list <- read.csv(file="etf_list.csv")

# 3. (5pts) create a vector of symbols called "sym_bols",
sym_bols <- c("VTI", "VEU", "IEF", "VNQ", "DBC", "XLY", "XLP", "XLE", "XLF", "XLV", "XLI", "XLB", "XLK", "XLU", "IWB", "IWD", "IWF", "IWM", "IWN", "IWO", "IWP", "IWR", "IWS", "IWV", "IUSV", "IUSG")
# subset etf_list to include only those ETF's in sym_bols, using the "%in%" operator,
etf_list <- etf_list[etf_list$Symbol %in% sym_bols, ]

# 4. (15pts) download 10yrs of price and volume data for the list of sym_bols, 
# and call it "zoo_series",
# for each symbol download the fields "AdjClose" and "Volume",
field_names <- c("AdjClose", "Volume")

# use get.hist.quote() and an lapply() loop,
# name the list returned by lapply as "zoo_series" (each list element is a zoo object),
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

# 8. (5pts) save zoo_series to a binary file called "zoo_series.Rdata", using save(),
save(zoo_series, file="zoo_series.Rdata")

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



############## hw
# 1. (10pts) Load time series data, calculate returns, and create 
# a function called get_hyp_stats() that calculates hypothesis test 
# statistics,
# 
# Load time series data and calculate returns,
# the file "etf_series.Rdata" contains time series data,
# create a new environment called "env_data",
# load data from the file "etf_series.Rdata" into "env_data", 
# use function load(), with the "envir" argument,

env_data <- new.env()
load(file="C:/Develop/data/etf_series.Rdata", envir=env_data)


# perform an eapply() loop to extract the adjusted prices for all 
# the variables in "env_data", and call it "etf_series_ad",
etf_series_ad <- do.call(merge, eapply(env_data, Ad))

# "etf_series_ad" should be an xts series containing adjusted prices,
# with colnames in the format "name.Adjusted",
# rename the colnames and drop ".Adjusted" from the colnames,
# use functions sapply() and strsplit(),
colnames(etf_series_ad) <- sapply(colnames(etf_series_ad), 
                                  function(col_name) strsplit(col_name, split="[.]")[[1]])[1, ]


# scrub (remove) rows with NA values from "etf_series_ad",
# use function complete.cases(),
etf_series_ad <- etf_series_ad[complete.cases(etf_series_ad)]


# calculate an xts series containing returns of "etf_series_ad", and call it "etf_rets",
# use functions lapply(), dailyReturn(), do.call(), and merge(),
# load package "quantmod",
library(quantmod)
etf_rets <- lapply(etf_series_ad, 
                   function(x_ts) {
                     daily_return <- dailyReturn(x_ts)
                     colnames(daily_return) <- names(x_ts)
                     daily_return
                   })  # end lapply

# flatten list of xts series into a single xts series,
etf_rets <- do.call(merge, etf_rets)


# 2. (20pts) Create a function called get_hyp_stats() that returns hypothesis test stats,
# function get_hyp_stats() should accept a single xts series argument called "re_turns", 
# The function get_hyp_stats() should perform the following steps:
# perform Jarque-Bera test of normality on "re_turns",
# perform Shapiro-Wilk test of normality on "re_turns",
# return a named vector containing the Jarque-Bera and the Shapiro-Wilk statistics 
# (not p.values!),
# use functions jarque.bera.test() and shapiro.test(), 
# be careful because shapiro.test() doesn't accept arguments of class "xts",

# load package "tseries"
library(tseries)

get_hyp_stats <- function(re_turns) {
# load package "tseries"
  stopifnot("package:tseries" %in% search() || require("tseries", quietly=TRUE))
  c(
    jarque_bera=unname(jarque.bera.test(re_turns)$statistic),
    shapiro=unname(shapiro.test(coredata(re_turns))$statistic))
}  # end get_hyp_stats

# apply get_hyp_stats() as follows, to verify it works correctly:
get_hyp_stats(etf_rets[, 1])


# 3. (10pts) Apply function get_hyp_stats() to all the columns of "etf_rets", 
# and call the result "hyp_stats",
# the first column of "hyp_stats" should contain Jarque-Bera statistics, 
# while the second Shapiro-Wilk,
# the rownames of "hyp_stats" should contain the names of "etf_rets" columns, 
# use functions sapply() and t(), 
hyp_stats <- sapply(etf_rets, get_hyp_stats)
hyp_stats <- t(hyp_stats)


# 4. (10pts) Create a scatterplot of "hyp_stats", 
# and add labels containing the rownames of "hyp_stats",
# use functions plot() and text(),
plot(hyp_stats)
text(x=hyp_stats[, "jarque_bera"], 
     y=hyp_stats[, "shapiro"],
     labels=rownames(hyp_stats),
     pos=1, cex=0.8)


# sort "hyp_stats" on column "jarque_bera" in ascending (increasing) order,
# use function order(),
hyp_stats <- hyp_stats[order(hyp_stats[, "jarque_bera"], decreasing=FALSE), ]

# save "hyp_stats" to comma-delimited CSV file,
# use function write.csv(),
write.csv(hyp_stats, file="hyp_stats.csv")



############## test
# Summary: perform a single lapply() loop to extract price and 
# volume columns from OHLC data series contained in "env_data". 

# download the file "etf_data.Rdata" from NYU Classes, and load() it. 
# "etf_data.Rdata" contains an environment called "env_data", 
# with OHLC time series data for ETFs, including "VTI". 

library(quantmod)
load(file="C:/Develop/data/etf_data.Rdata")

# 1. (10pts) Create a function called ex_tract(), that extracts 
# the adjusted price and volume columns from an OHLC data series, 
# and returns an xts series with two columns, 
# you can use functions merge(), Ad(), and Vo(), 

ex_tract <- function(x_ts) merge(Ad(x_ts), Vo(x_ts))

# Apply function ex_tract() to a single xts series, to verify 
# it works correctly:

foo <- ex_tract(env_data$VTI)
head(foo)


# 2. (20pts) Create a vector of symbols, called "sym_bols", 

sym_bols <- c("DBC", "VTI", "IEF")

# Perform an lapply() loop over a subset of "env_data" 
# containing "sym_bols", and call the function ex_tract() 
# on each element in the subset, and call the output "da_ta". 
# "da_ta" should be a list of xts series, with each xts series 
# containing price and volume data for a single symbol. 
# you can use functions as.list(), ex_tract(), and lapply(), 

da_ta <- lapply(as.list(env_data)[sym_bols], ex_tract)

# Flatten "da_ta" into a single xts series, and call it "da_ta". 
# you can use functions do.call() and merge(), 

da_ta <- do.call(merge, da_ta)



############## hw
# Summary: create a function that extracts columns from an OHLC data 
# series contained in an environment, performs calculations on the columns, 
# and saves the result in an xts series in a different environment. 

# download the file "etf_data.Rdata" from NYU Classes, and load() it. 
# "etf_data.Rdata" contains an environment called "env_data", 
# with OHLC time series data for ETFs, including "VTI". 

library(quantmod)
load(file="C:/Develop/data/etf_data.Rdata")

# 1. (20pts) Create a function called ex_tract(), that accepts 
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
# Note that ex_tract() only receives a string representing the name of 
# an xts series, not the name itself, 
# so you must use functions get() and assign() to get the data 
# and to assign it. 
# 
# you can also use functions merge(), invisible(), Ad(), and Vo(), 

ex_tract <- function(sym_bol, in_env, out_env) {
  x_ts <- get(sym_bol, envir=in_env)
  x_ts <- merge(Ad(x_ts), Vo(x_ts))
  assign(sym_bol, x_ts, envir=out_env)
  invisible(sym_bol)
}  # end ex_tract

# create a new environment called "env_out", 
# use function new.env(),

env_out <- new.env()

# apply function ex_tract() to the symbol "VTI" and "env_out", 
# to verify it works correctly:

ex_tract("VTI", in_env=env_data, out_env=env_out)

# also run these commands to verify that a new object was created:

ls(env_out)
head(env_out$VTI)


# 2. (20pts) remove all files from "env_out",
# you can use functions rm() and ls(), 

rm(list=ls(env_out), envir=env_out)

# Create a vector of symbols, called "sym_bols", 

sym_bols <- c("DBC", "VTI", "IEF")

# Perform an sapply() loop over "sym_bols", to apply the 
# function ex_tract() to all the symbols in "sym_bols", 
# to get full credit you must pass the arguments "in_env=env_data" 
# and "out_env=env_out" into ex_tract() through the dots 
# argument of the sapply() function, 

sapply(sym_bols, ex_tract, in_env=env_data, out_env=env_out)



############## hw
# Summary: create a function that extracts columns from OHLC data 
# series contained in an environment, performs calculations on those, 
# and saves the result in a xts series in another environment. 

# 1. (5pts) Load time series data from file "etf_series.Rdata" (NYU Classes),
# create a new environment called "env_data", for storing the xts series 
# containing stock prices,
# load data from the file "etf_series.Rdata" into "env_data",
# use functions new.env() and load(), with the "envir" argument,

env_data <- new.env()
load(file="C:/Develop/data/etf_series.Rdata", envir=env_data)


# 2. (20pts) create a function called get_returns_volume(), 
# that accepts an xts series argument ("x_ts") and an environment 
# argument ("envir"), 
# get_returns_volume() should:
# - extract adjusted prices and volume data from the xts series,
# - calculate returns from adjusted prices,
# - extract the symbol name from the columns of xts series ("symbol_name"),
# - merge returns with volume data into a single xts series ("return_volume"),
# - rename the colnames of "return_volume" to "symbol_name.Return" 
#   and "symbol_name.Volume", (replace "symbol_name" with the symbol name),
# - assign (copy) "return_volume" to a object named "symbol_name_rets" 
#   in the "envir" environment, 
# get_returns_volume() should produce the side effect of creating 
# an xts series in the "envir" environment containing returns and 
# volume data "from the input "x_ts",
# get_returns_volume() should return invisible the "symbol_name",
# you can use functions Ad(), Vo(), strsplit(), colnames(), 
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
# the objects in "env_data", and copy to "env_returns",
# use functions get_returns_volume() and eapply(),
eapply(env_data, get_returns_volume, envir=env_returns)

# remove all files from "env_returns",
rm(list=ls(env_returns), envir=env_returns)

# perform a for() loop to apply get_returns_volume() to all  
# the objects in "env_data", and copy to "env_returns",
# use functions get_returns_volume() and for(),
for (x_ts in ls(env_data)) {
  get_returns_volume(get(x=x_ts, envir=env_data), envir=env_returns)
}  # end for


# save all the objects in the environment "env_returns" 
# to a binary file called "etf_rets_volume.Rdata", 
# use function save(), with the "list" and "envir" arguments,
# make sure to save the objects in the environment, 
# not the environment itself,
save(list=ls(env_returns), envir=env_returns, file="etf_rets_volume.Rdata")



##############################
# time series analysis stochastic processes
##############################


############## test
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


############## hw
# Summary: Calculate the maximum drawdown of a time series.

# first method: create synthetic xts for calculating maximum drawdown
# load packages lubridate, xts, and tseries,
library(xts)

# Create a "Date" vector of 100 daily dates, starting from "2015-01-04", and call it "in_dex", 
# use functions as.Date() and seq() with "by" argument,
in_dex <- seq(from=as.Date("2015-01-04"), by="day", length.out=100)

# Extract the class from "in_dex" to verify that it is "Date" class, 
class(in_dex)

# Create a vector of data of length "in_dex" as follows:
da_ta <- sin(0.1*(1:length(in_dex))) + (1:length(in_dex))/50

# Create an xts time series with the "da_ta" and the "in_dex", and call it "xts_series",
# use function xts() from package xts,
xts_series <- xts(da_ta, order.by=in_dex)


# second method for getting time series: load "zoo_data.Rdata"
# 1. (15pts) 
# download the file "zoo_data.Rdata" from NYU Classes, and load() it, 
# the file "zoo_data.Rdata" includes a zoo series called "zoo_stx", 
# containing MSFT stock OHLC data. 
# extract the "AdjClose" prices from "zoo_stx" into a variable 
# called "msft_prices".

load(file="C:/Develop/data/zoo_data.RData")
msft_prices <- zoo_stx[, "AdjClose"]

# plot "msft_prices", using generic function plot(),

plot(msft_prices)

# The cumulative maximum of a price series is the maximum price up to 
# that point in time. 
# Plot the Cumulative maximum of "msft_prices" using function cummax(),

plot(cummax(msft_prices))

# A drawdown is a drop in price from its previous maximum.
# Calculate the zoo time series of drawdowns of "msft_prices", 
# as the difference between "msft_prices" minus the cumulative 
# maximum of "msft_prices", and call it "draw_down", 

draw_down <- msft_prices - cummax(msft_prices)

# plot "draw_down",

plot(draw_down)

# Find the date when "draw_down" reaches its minimum, and call it "date_trough", 
# and find the minimum value of "draw_down" on that date, and call it "max_drawdown", 
# you can use functions index() and which.min(),

in_dex <- index(msft_prices)
date_trough <- in_dex[which.min(draw_down)]
max_drawdown <- draw_down[date_trough]

# Subset "draw_down" to dates before "date_trough", and call it "pre_drawdown", 

pre_drawdown <- draw_down[in_dex<date_trough]

# Subset "draw_down" to dates after "date_trough", and call it "post_drawdown", 

post_drawdown <- draw_down[in_dex>date_trough]

# When the current price exceeds the previous maximum, then "draw_down" is zero, 
# a drawdown starts when "draw_down" is first zero and then decreases below zero.
# Find the latest date when "pre_drawdown" was still zero before "date_trough", 
# and call it "date_from",
# you can use functions index() and max(), 

date_from <- max((index(pre_drawdown))[pre_drawdown==0])

# A drawdown ends when "draw_down" returns back to zero after "date_trough".
# Find the first date when "post_drawdown" returns to zero after "date_trough", 
# and call it "date_to",
# you can use functions index() and min(),

date_to <- min((index(post_drawdown))[post_drawdown==0])

# Combine the three dates into a named vector: 
# from=date_from, trough=date_trough, to=date_to,
# and call it "drawdown_dates",

drawdown_dates <- c(from=date_from, trough=date_trough, to=date_to)

# 2. (5pts) plot "msft_prices", using generic function plot(),

plot(msft_prices, main="Drawdown dates")

# add vertical green, red, and blue lines for the three dates: 
# "date_from", "date_trough", "date_to",

abline(v=drawdown_dates, col=c("green", "red", "blue"))

# deprecate:
# use function addEventLines() from package xts,
addEventLines(
  event.dates=drawdown_dates, 
  event.labels=names(drawdown_dates),
  on=1, col="red", lty="dashed", lwd=2)


# 3. (10pts) Create a function called max_drawdown() that calculates 
# the maximum drawdown of a time series of prices.
# max_drawdown() should take one argument called "time_series", 
# which should be a time series of prices (not returns). 
# max_drawdown() should return a named vector of three dates: 
# from=date_from, trough=date_trough, to=date_to, depth
# you can reuse the scripts from p.1,

max_drawdown <- function (time_series) {
  draw_down <- time_series - cummax(time_series)
  in_dex <- index(time_series)
  date_trough <- in_dex[which.min(draw_down)]
  pre_drawdown <- draw_down[in_dex<date_trough]
  post_drawdown <- draw_down[in_dex>date_trough]
  date_from <- max((index(pre_drawdown))[pre_drawdown==0])
  date_to <- min((index(post_drawdown))[post_drawdown==0])
  c(from=date_from, trough=date_trough, to=date_to)
}  # end max_drawdown

# call max_drawdown() with the argument "msft_prices", to verify it works correctly,
max_drawdown(msft_prices)



############## hw
# 1. (20pts) simulate an ARIMA AR(1) process, and call it "ts_arima",
# use function arima.sim(),
# the length of the series should be 100,
# the "model" argument should specify an ARIMA AR(1) process,
# with a single coefficient equal to 0.5,
# set the burn-in period to zero, by specifying the "start.innov"
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
# Summary: create a function that calculates the volume-weighted average price, 
# benchmark it to function VWAP() from package TTR, and create plots. 

# 1. (20pts) create a function called v_wap() that calculates the 
# volume-weighted average price over a sliding window (lookback period). 
# The volume-weighted average price (VWAP) over a period is defined as 
# the sum of the prices multiplied by trading volumes, divided by the 
# total trading volume in that period.
# v_wap() should accept two arguments:
# "x_ts" - an xts series containing OHLC prices and trading volumes, 
# "win_dow" - an integer specifying the number of lookback periods. 
# v_wap() should return an xts series with a single column 
# containing the VWAP of the adjusted close prices. 
# The output xts series should have the same number of rows as the 
# input xts series, 
# v_wap() should not return any NA values.
# You can use functions Ad(), Vo() and run_sum(), 

v_wap <- function(x_ts, win_dow) {
  v_wap <- run_sum(x_ts=Ad(x_ts)*Vo(x_ts), win_dow=win_dow)
  vol_ume <- run_sum(x_ts=Vo(x_ts), win_dow=win_dow)
  v_wap <- v_wap/vol_ume
  v_wap[is.na(v_wap)] <- 0
  v_wap
}  # end v_wap

# download the file "etf_data.Rdata" from NYU Classes, and load() it. 
# "etf_data.Rdata" contains an environment called "env_data", 
# with OHLC time series data for ETFs, including "VTI". 

library(quantmod)
load(file="C:/Develop/data/etf_data.Rdata")

# 2. (5pts) calculate the VWAP of "VTI" using v_wap() 
# with "win_dow=11", to verify it works correctly. 
# Calculate the same VWAP using function VWAP() from package TTR, 
# and convert NA values to zero.
# Find which elements are significantly different between the 
# two results (absolute difference is greater than 0.01). 
# You can use functions abs(), is.na(), merge(), and "[]" subsetting, 

foo <- v_wap(x_ts=env_data$VTI, win_dow=11)
bar <- VWAP(price=Ad(env_data$VTI), volume=Vo(env_data$VTI), n = 11)
bar[is.na(bar)] <- 0
is_diff <- abs(foo-bar) > 0.01
merge(foo, bar)[is_diff]

# 3. (10pts) Benchmark the speed of v_wap() compared to 
# function VWAP() from package TTR. 
# Which function is faster? 
# Use function microbenchmark() from package microbenchmark. 
# You can adapt code from the "numerical_analysis" pdf and R files. 
# Be sure to download the most recent version.

library(microbenchmark)
microbenchmark(
  vwap_ttr=VWAP(price=Ad(env_data$VTI), volume=Vo(env_data$VTI), n = 11),
  vwap_custom=v_wap(x_ts=env_data$VTI, win_dow=11),
  times=10)

# 4. (10pts) Plot the adjusted close price of "VTI" for the year 2008 
# only, together with its VWAP, in a single panel on the same plot.
# You can use functions Ad(). 
# You must use either functions chartSeries() and addTA(), 
# or chart_Series() and add_TA(). 
# You can adapt code from the "time_series_univariate" pdf and R files. 
# Be sure to download the most recent version.

chartSeries(x=Ad(env_data$VTI)["2008"], 
            TA="addTA(foo, on=1)", 
            theme=chartTheme("white"), 
            name="VTI plus VWAP")
# or
chartSeries(x=Ad(env_data$VTI)["2008"],
            theme=chartTheme("white"),
            name="VTI plus VWAP")
addTA(foo["2008"], col="red", on=1)

chart_Series(x=Ad(env_data$VTI)["2008"], 
             TA="add_TA(foo, on=1)", 
             name="VTI plus VWAP")
# or
chart_Series(x=Ad(env_data$VTI)["2008"], 
             name="VTI plus VWAP")
add_TA(foo["2008"], col="blue", lwd=2, on=1)



############## hw
# Summary: Calculate moving averages and crossing points with prices.

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

# calculate the 50-day moving average of the "AdjClose" prices,
# merge the moving average with "zoo_msft" by adding it as the last column,
# rename the last column to "50DMA",
# you must use function rollmean(), with the proper "align" argument, 
# so that the averages are calculated using values from the past,
# remove rows with NAs using function na.omit(), 

roll_mean <- rollmean(x=zoo_msft[, "AdjClose"], k=50, align="right")
zoo_msft <- na.omit(merge(zoo_msft, roll_mean))
colnames(zoo_msft)[3] <- "50DMA"


# 2. (15pts) 
# plot "zoo_msft" columns "AdjClose" and "50DMA" in the same panel, 
# starting from "2015-01-01", in the colors "black" and "red", 
# you must use method plot.zoo() with the proper argument "plot.type",
# add a legend and position it so that it doesn't obscure the plot too much,

start_date <- as.Date("2015-01-01")
plot(zoo_msft[(index(zoo_msft)>start_date), 
              c("AdjClose", "50DMA")], 
     main="MSFT Prices and 50DMA", 
     xlab="", ylab="", plot.type="single", 
     col=c("black", "red"))
# add legend
legend("bottomright", inset=0.05, cex=0.5, 
       title="MSFT Prices and 50DMA", 
       leg=c("MSFT", "50DMA"), lwd=2, bg="white", 
       col=c("black", "red"))

# calculate the vector of dates right after the "AdjClose" crosses the "50DMA", 
# and call it "cross_es". 
# First calculate a boolean vector that is TRUE for dates right 
# after a cross has just occurred, and FALSE otherwise. 
# Next apply this boolean vector to extract dates when 
# the "AdjClose" crosses the "50DMA". 
# you can use the logical operator "!=", 
# and functions sign(), diff(), and index(), 

cross_es <- (diff(sign(zoo_msft[, "AdjClose"] - zoo_msft[, "50DMA"]))!=0)
cross_es <- index(zoo_msft[cross_es, ])

# add grey vertical ablines to the plot above, at the dates of "cross_es",
# you must use function abline(), 

abline(v=cross_es[cross_es>start_date], col="grey")


# 3. (20pts)
# Calculate the 50-day rolling maximum and minimum of the "AdjClose" prices,
# you must use function rollmax() with the proper "align" argument, so that 
# the aggregations are calculated using values from the past,
# calculate the difference between the rolling maximum and minimum, 
# and call it "ba_nd",

roll_max <- rollmax(x=zoo_msft[, "AdjClose"], k=50, align="right")
roll_min <- -rollmax(x=-zoo_msft[, "AdjClose"], k=50, align="right")
ba_nd <- (roll_max-roll_min)

# calculate the rolling upper (lower) band as the 50-day moving average
# plus (minus) one half of "ba_nd",
# merge the rolling upper and lower bands with "zoo_msft" by adding 
# them as the last columns,
# rename the last columns to "Up_band" and "Low_band",
# remove rows with NAs using function na.omit(), 

upper_band <- zoo_msft[, "50DMA"] + ba_nd/2
lower_band <- zoo_msft[, "50DMA"] - ba_nd/2
zoo_msft <- merge(zoo_msft, upper_band, lower_band)
colnames(zoo_msft)[4:5] <- c("Up_band", "Low_band")
zoo_msft <- na.omit(zoo_msft)

# plot "zoo_msft" columns "AdjClose", "Up_band", and "Low_band" in the 
# same panel, starting from "2015-01-01",
# use method plot.zoo() with the proper argument "plot.type",
# add legend so that it doesn't obscure the plot,

plot(zoo_msft[(index(zoo_msft)>start_date), 
              c("AdjClose", "Up_band", "Low_band")], 
     main="MSFT Prices with Bollinger Bands", 
     xlab="", ylab="", plot.type="single", 
     col=c("black", "red", "blue"))
# add legend
legend("top", inset=0.05, cex=0.5, 
       title="MSFT Prices with Bollinger Bands", 
       leg=c("MSFT", "Up_band", "Low_band"), 
       lwd=2, bg="white", 
       col=c("black", "red", "blue"))

# calculate the vector of dates right after the "AdjClose"
# crosses any of the two bands, and call it "cross_es". 
# First calculate a boolean vector that is TRUE for dates right 
# after a cross has just occurred, and FALSE otherwise. 
# Next apply this boolean vector to extract dates when 
# the "AdjClose" crosses either "Up_band" or "Low_band". 
# you can use the logical operator "!=", 

up_cross <- zoo_msft[, "AdjClose"] - zoo_msft[, "Up_band"]
low_cross <- zoo_msft[, "AdjClose"] - zoo_msft[, "Low_band"]
cross_es <- (((diff(sign(up_cross))!=0)) | ((diff(sign(low_cross))!=0)))
cross_es <- index(zoo_msft[cross_es, ])

# add grey vertical ablines to the plot above, at the dates of "cross_es",
# you must use function abline(), 

abline(v=cross_es[cross_es>start_date], col="grey")



############## hw
# 1. (20pts) Perform regression of rolling range vs volume
# Load time series data and calculate rolling range statistics,
# the file "etf_series.Rdata" contains time series data,
# create a new environment called "env_data",
# load data from the file "etf_series.Rdata" into "env_data",
# use function load(), with the "envir" argument,

library(quantmod)
env_data <- new.env()
load(file="C:/Develop/data/etf_series.Rdata", envir=env_data)


# extract the adjusted prices and volume for symbol "VTI" from "env_data", and call it "VTI",
# "VTI" will now be defined both in the default workspace and in "env_data",
# use function merge(), 

da_ta <- merge(Ad(env_data$VTI), Vo(env_data$VTI))


# calculate rolling range statistics over a sliding window, called "win_dow",

win_dow <- 22

# calculate two xts time series of trailing maximum and minimum values 
# of adjusted prices over the sliding "win_dow", and call them "roll_max" and "roll_min",
# at every point in time, the value of "roll_max" should be equal to the maximum 
# adjusted price from points in the past covered by "win_dow",
# use function rollmax() from package "zoo", with the proper "k" and "align" arguments,

roll_max <- rollmax(x=da_ta[, 1], k=win_dow, align="right")
colnames(roll_max) <- "max"
roll_min <- -rollmax(x=(-da_ta[, 1]), k=win_dow, align="right")
colnames(roll_min) <- "min"

# calculate the difference between "roll_max" and "roll_min", and call it "ra_nge",

ra_nge <- (roll_max-roll_min)
colnames(ra_nge) <- "range"

# calculate an xts time series of trailing mean values of the volume 
# over the sliding "win_dow", and call it "roll_volume",
# use function rollmean(), with the proper "k" and "align" arguments,

roll_volume <- rollmean(x=Vo(env_data$VTI), k=win_dow, align="right")
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



############## test
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
# Calculate the standard deviation of returns over monthly end points. 
# Calculate the aggregated trading volumes over monthly end points. 
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
# Extract from summary() the regression statistics for the slope coefficient: 
#  t-value, p-value, adj.r.squared, 
# and create a named vector with these regression statistics. 
# you can use functions colnames(), as.formula(), lm() and summary(), 

reg_model <- lm(
  as.formula(paste(colnames(volat_volume), collapse=" ~ ")), 
  data=volat_volume)

reg_model_sum <- summary(reg_model)

with(reg_model_sum, 
     c(tval=coefficients[2, 3],
       pval=coefficients[2, 4],
       adj_rsquared=adj.r.squared))

# perform the Durbin-Watson test for the autocorrelations of residuals,
# write what is the null hypothesis?
# can the null hypothesis be rejected in this case?
# use function dwtest(), from package lmtest,

library(lmtest)  # load lmtest
dwtest(reg_model)



############## hw
# Summary: calculate the rolling standard deviation of returns, 
# and aggregate the volume, over monthly end points, 
# 
# 1. (5pts) Load time series data from file 
# "etf_rets_volume.Rdata" (download it from NYU Classes), 
# containing xts series with stock return and volume data, 
# create a new environment called "env_returns", 
# load data from the file "etf_rets_volume.Rdata" into "env_returns", 
# use functions new.env() and load(), with the "envir" argument, 

library(quantmod)
env_returns <- new.env()
load(file="C:/Develop/data/etf_rets_volume.Rdata", envir=env_returns)


# the environment "env_returns" should contain a number of xts series, 
# each containing stock daily return and volume data for a single symbol, 
# you can assume that all the xts series have the same date index,
# create a vector of monthly end points for any of the xts series 
# in "env_returns",
# called "end_points", and set the first "end_points" equal to 1,
# use function endpoints() from package xts, 

end_points <- endpoints(env_returns$VTI_rets, on="months")


# 2. (20pts) create a function called agg_volat_volume(), 
# that accepts three arguments:
#   "x_ts": an xts series containing returns and volume data, 
#   "end_points": a vector of end points, 
#   "envir": an environment argument, 
# agg_volat_volume() should:
# - extract returns and volume data from "x_ts",
# - extract the symbol name from the columns of "x_ts" ("symbol_name"),
# - calculate the volatility from the returns, 
#    over non-overlapping periods given by "end_points", 
# - calculate the total (cumulative) volume, 
#    over non-overlapping periods given by "end_points", 
# - cbind (merge) volatility with volume data into a single xts series 
#    called "volat_volume", 
# - rename the colnames of "volat_volume" to "symbol_name.Volat" 
#    and "symbol_name.Volume", (replace "symbol_name" with the symbol name),
# - assign (copy) "volat_volume" to an object named "symbol_name" 
#   in the "envir" environment, 
# agg_volat_volume() should produce the side effect of creating 
# an xts series in the "envir" environment, that contains volatility 
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
  vol_ume <- period.sum(x_ts[, 2], INDEX=end_points)
  volat_volume <- xts(cbind(std_dev, vol_ume[-1, ]), 
                      order.by=index(x_ts[end_points[-1], ]))
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
# you can use method plot.zoo(), or plot.xts() and par(),

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


# 3. (10pts) perform a regression of volume versus volatility, 
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
# 1. (15pts) calculate returns and aggregations over overlapping periods, 
# 
# download the file "zoo_series.Rdata" from NYU Classes, and load() it, 
# the file "zoo_series.Rdata" contains a zoo called "zoo_series",
load(file="C:/Develop/data/zoo_series.Rdata")

# create a vector of monthly end points for "zoo_series",
# called "end_points", and set the first "end_points" equal to 1,
# use function endpoints() from package xts,
end_points <- endpoints(zoo_series, on="months")

# extract (subset) the monthly prices from the "VTI.Close" column 
# of "zoo_series", corresponding to "end_points",
# convert them to an xts object, and call it "xts_series", 
# calculate the log of "xts_series", and copy it back onto "xts_series", 
# use function as.xts() from package xts, and log(),
xts_series <- as.xts(log(zoo_series[end_points, "VTI.Close"]))

# calculate the percentage (log) monthly returns of "xts_series", 
# by taking the difference between "xts_series", and "xts_series"
# lagged by 1 month, and call it "xts_rets", 
# calculate the percentage (log) 3-month returns of "xts_series", 
# by taking the difference between "xts_series", and "xts_series"
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
# Summary: perform autocorrelation tests of period statistics, MAD. 
# Demonstrate that measures of dispersion are autocorrelated. 
# Load time series data and calculate period statistics over end points. 

# 1. (15pts) 
# download the file "etf_rets.Rdata" from NYU Classes,
# "etf_rets.Rdata" contains an xts of daily ETF returns called "etf_rets",
# use function load(),

# load package xts,
library(xts)
load(file="C:/Develop/data/etf_rets.Rdata")

# create a vector of monthly end points for "etf_rets", called "end_points",
# and set the first "end_points" equal to 1,
# use function endpoints() from package xts, 

end_points <- endpoints(etf_rets, on="months")

# calculate a numeric vector of returns of etf_rets[, "VTI"], 
#   over monthly non-overlapping periods based on "end_points", 
#   and call it "period_rets",
# calculate a vector of Median Absolute Deviations (MAD) of etf_rets[, "VTI"], 
#   over monthly non-overlapping periods, and call it "period_vol",
# "period_vol" is a vector of volatility estimates over time,
# use function period.apply() from package xts,
# and functions mad(), sum(), and as.numeric(), 

period_rets <- as.numeric(period.apply(etf_rets[, "VTI"], INDEX=end_points, sum))
period_vol <- as.numeric(period.apply(etf_rets[, "VTI"], INDEX=end_points, mad))

# create a vector of rnorm() of length equal to etf_rets[, "VTI"], 
# and calculate a vector of MAD from this vector, over monthly 
# non-overlapping periods, and call it "rand_vol",
# use functions period.apply(), mad(), rnorm(), and length(), 

rand_vol <- period.apply(rnorm(length(etf_rets[, "VTI"])), INDEX=end_points, mad)


# 2. (20pts) Perform autocorrelation tests on period rets,
# perform the Durbin-Watson autocorrelation test on "period_rets", 
# "period_vol", and "rand_vol",
# the Durbin-Watson test can be performed on a vector "y" using the syntax: 
#   dwtest(y ~ 1) 
# extract the p-value and compare it to the 2.27% confidence level,
# for which vector can the null hypothesis of zero autocorrelations 
# be rejected at 2.27% confidence level?
# use function dwtest(), from package lmtest,

# load lmtest
library(lmtest)
dw_test <- dwtest(period_rets ~ 1)
dw_test$p.value < 0.0227
dw_test <- dwtest(period_vol ~ 1)
dw_test$p.value < 0.0227
dw_test <- dwtest(rand_vol ~ 1)
dw_test$p.value < 0.0227

# perform the Ljung-Box autocorrelation test on "period_rets", 
# "period_vol", and "rand_vol",
# extract the p-value and compare it to the 2.27% confidence level,
# for which vector can the null hypothesis of zero autocorrelations 
# be rejected at 2.27% confidence level?
# use function Box.test(), with lag=10 and type="Ljung",
box_test <- Box.test(period_rets, lag=10, type="Ljung")
box_test$p.value < 0.0227
box_test <- Box.test(period_vol, lag=10, type="Ljung")
box_test$p.value < 0.0227
box_test <- Box.test(rand_vol, lag=10, type="Ljung")
box_test$p.value < 0.0227

# apply the functions acf_plus() (from FRE7241 lecture #2) and pacf() 
# to "period_rets", "period_vol", and "rand_vol",
# based on visual inspection, which of them appear to be autocorrelated?
acf_plus(period_rets)
acf_plus(period_vol)
acf_plus(rand_vol)
pacf(period_rets)
pacf(period_vol)
pacf(rand_vol)


# 3. (10pts) fit ARIMA models,
# 
# fit ARIMA models to "period_rets", "period_vol", and "rand_vol",
# extract the ARIMA coefficients, and their standard errors,
# the standard errors can be calculated as the square roots 
# of the diagonal of the field "var.coef" of the ARIMA object,
# see:
# http://r.789695.n4.nabble.com/ARIMA-standard-error-td820763.html
# use functions arima(), sqrt(), and diag(),

a_rima <- arima(period_rets, order=c(5,0,0))
a_rima$coef
sqrt(diag(a_rima$var.coef))

a_rima <- arima(period_vol, order=c(5,0,0))
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



############## test
# Summary: create a scatterplot of random portfolios, 
# together with a minimum variance portfolio. 

# Download the file "etf_data.Rdata" from NYU Classes, 
# and load() it. 
# "etf_data.Rdata" contains an xts series called "etf_rets", 
# with ETF returns,

library(xts)
library(quantmod)
load(file="C:/Develop/data/etf_data.Rdata")


# 1. (20pts) Create a vector of symbol names called "sym_bols", 
# create a named vector of initial portfolio weights for the 
# "sym_bols", all equal to 1, and call it "weight_s", 
# You can use functions rep() and names(), 

sym_bols <- c("VTI", "VNQ", "DBC", "XLP", "XLK")
n_weights <- length(sym_bols)
weight_s <- rep(1, n_weights)
names(weight_s) <- sym_bols

# Create an objective function equal to the standard 
# deviation of returns, and call it object_ive(). 
# The objective function should accept a single argument: 
#  "weights": the portfolio weights, 
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
# coerce them to an xts series, and call them "optim_rets". 
# You can use the index of "etf_rets". 
# You can use functions xts() and index(), 

library(xts)
optim_rets <- xts(x=etf_rets[, sym_bols] %*% weight_s, 
                  order.by=index(etf_rets))

# Plot the cumulative returns of "optim_rets" using 
# chart_Series(). 
# You must use functions cumsum() and chart_Series(), 

library(quantmod)
chart_Series(x=cumsum(optim_rets), name="minvar portfolio")


# 3. (30pts) Calculate a matrix of cumulative returns and 
# standard deviations of random portfolios, and call it "ret_sd". 
# Define the number of random portfolios:

n_portf <- 1000

# Perform an sapply() loop over "1:n_portf", and at each step 
# create a vector of random weights using rnorm(). 
# Rescale the random weights, so their sum of squares 
# is equal to "1". 
# Multiply "etf_rets" by the random weights, and calculate 
# the cumulative return and standard deviation. 
# The output should ba a named matrix called "ret_sd". 
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

# Create a scatterplot of "ret_sd", with standard deviations on 
# the x-axis, and returns on the y-axis, and add a title. 
# Add a point in "red" to the scatterplot corresponding to the 
# minimum variance portfolio. 
# You can use functions plot(), points(), text(), and title(), 

plot(x=ret_sd[2, ], y=ret_sd[1, ], xlim=c(0, max(ret_sd[2, ])), 
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
# coerce them to an xts series, and call them "optim_rets". 

weight_s <- optim_run$par/sqrt(sum(optim_run$par^2))
optim_rets <- xts(x=etf_rets[, sym_bols] %*% weight_s, 
                  order.by=index(etf_rets))

# Add a point in "blue" to the scatterplot corresponding 
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


############## hw
# Summary: perform a rolling portfolio optimization over 
# annual periods, calculate optimized portfolio weights 
# in each year, and apply them to out-of-sample data in 
# the following year. 

# Download the file "etf_data.Rdata" from NYU Classes, 
# and load() it.  "etf_data.Rdata" contains an xts series 
# called "etf_rets", containing ETF returns. 

load(file="C:/Develop/data/etf_data.Rdata")

# 1. (10pts) create a vector of annual end points from the index 
# of "etf_rets", and call it "end_points". 
# Use "xts" function endpoints(),

library(xts)
end_points <- endpoints(etf_rets, on="years")

# the above code should produce the following data:
# > end_points
# [1] 0  207  460  712  964 1216 1466 1718 1970 2053

# The "end_points" values are the integer indices 
# corresponding to dates that form non-overlapping annual 
# intervals. 
# Create a list of elements containing the indices belonging 
# to the non-overlapping annual intervals, and call the list 
# "period_s". 
# Each element of "period_s" represents an interval 
# corresponding to a year, and contains the indices of dates 
# belonging to that interval. 
# For example, the first element should contain the 
# integers: 1:207 belonging to the year 2007, the second 
# element: 208:460 belonging to the year 2008, etc. 
# Assign names to the elements of "period_s" corresponding 
# the year. 
# hint: perform an lapply() loop over "end_points". 
# You can use functions lapply(), names(), format(), index(), 
# and an anonymous function,

period_s <- lapply(2:length(end_points),
                     function(in_dex)
                       (end_points[in_dex-1]+1):end_points[in_dex]
)  # end lapply
names(period_s) <- format(index(etf_rets[end_points[-1]]), "%Y")

# the above code should produce the following data:
# > tail(period_s$"2008")
# [1] 455 456 457 458 459 460
# > head(period_s$"2009")
# [1] 461 462 463 464 465 466
# notice that there's no overlap between second and third periods, 

# create a vector of symbols for the optimized portfolio,

sym_bols <- c("VTI", "VNQ", "DBC")

# create a named vector of initial portfolio weights for the 
# "sym_bols", all equal to 1, and call it "portf_weights", 
# You can use functions rep() and names(), 

portf_weights <- rep(1, length(sym_bols))
names(portf_weights) <- sym_bols


# 2. (10pts) create an objective function equal to minus 
# the Sharpe ratio, and call it object_ive(). 
# The objective function should accept two arguments: 
#  "weights": the portfolio weights, 
#  "re_turns": an xts series containing returns data, 
# hint: you can adapt code from the slide "Portfolio 
# Objective Function". 
# You can use functions sum() and sd(), 

object_ive <- function(weights, re_turns) {
  portf_ts <- re_turns %*% weights
  -sum(portf_ts)/sd(portf_ts)
}  # end object_ive

# apply object_ive() to an equal weight portfolio, and 
# "etf_rets" subset to "sym_bols", 

object_ive(weights=portf_weights, re_turns=etf_rets[, sym_bols])


# 3. (20pts) create a function called optim_portf(), 
# that accepts a single argument:
#  "re_turns": an xts series containing returns data, 
# optim_portf() should:
# - create a named vector of initial portfolio weights 
#  all equal to 1, with the names extracted from the 
#  column names of "re_turns",
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

# apply optim_portf() to "etf_rets" subset to "sym_bols" 
# and the period "2009" from "period_s", 

optim_portf(re_turns=etf_rets[period_s$"2009", sym_bols])


# 4. (20pts) Perform an sapply() loop over "period_s",
# and calculate the optimal portfolio weights in each 
# period. Call the matrix returned by sapply() "weight_s". 
# You can use functions sapply(), optim_portf(), 
# and an anonymous function,

weight_s <- sapply(period_s, 
                   function(pe_riod) {
                     optim_portf(re_turns=etf_rets[pe_riod, sym_bols])
                   }  # end anon function
)  # end sapply


# 5. (30pts) Perform an lapply() loop over the length of 
# "period_s", starting in the second period (year). 
# In each period calculate the portfolio returns, 
# using out-of-sample portfolio weights from the previous 
# period.  In each period coerce the portfolio returns to 
# an xts series and return it. 
# hint: you can use the names of periods "names(period_s)" 
# to extract the "weight_s" from the previous period. 
# You can use functions lapply(), xts(), index(), 
# and an anonymous function,

optim_rets <- lapply(2:length(period_s),
                     function(in_dex)
                       xts(x=etf_rets[period_s[[in_dex]], sym_bols] %*% 
                             weight_s[, names(period_s)[in_dex-1]], 
                           order.by=index(etf_rets[period_s[[in_dex]], ]))
)  # end lapply

# The lapply() loop produces a list of xts series. 
# Flatten "optim_rets" into a single xts series. 
# You can use functions do.call() and rbind(), 

optim_rets <- do.call(rbind, optim_rets)

# "optim_rets" should look like this: 
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

# plot the cumulative sum of "optim_rets" using 
# chart_Series(). 
# You must use functions cumsum() and chart_Series(), 

library(quantmod)
chart_Series(x=cumsum(optim_rets), name="Rolling optimized portfolio returns")



############## hw
# rolling window portfolio optimization using PortfolioAnalytics
# 
# 1. (10pts) create a portfolio object with equal weights,
library(PortfolioAnalytics)
load(file="C:/Develop/data/etf_data.Rdata")
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

# Download the file "etf_data.Rdata" from NYU Classes, 
# and load() it. 
# "etf_data.Rdata" contains an xts series called "etf_rets", 
# with ETF returns,

library(xts)
library(quantmod)
library(PerformanceAnalytics)

load(file="C:/Develop/data/etf_data.Rdata")

# create a vector of symbol names called "sym_bols", 

sym_bols <- c("VTI", "VNQ", "DBC", "XLP", "XLK")

# 1. (25pts) Create a vector of yearly end points 
# for "etf_rets", called "end_points", 
# you must use function endpoints() from package xts, 

end_points <- endpoints(etf_rets, on="years")

# Perform an lapply() loop over the "end_points", and call 
# the result "list_capm". 
# Inside the loop subset "etf_rets" to the "sym_bols" and 
# end points, and calculate a data frame of statistics 
# using table.CAPM(), using "VTI" as the benchmark asset "Rb". 
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
                    function (str) {strsplit(str, split=" ")[[1]][1]})
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

# Assign names to the list using the years of the "end_points". 
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
# the file "etf_series.Rdata" contains time series of ETF prices,
# create a new environment called "env_data",
# load data from the file "etf_series.Rdata" into "env_data",
# use function load(), with the "envir" argument,

env_data <- new.env()
load(file="C:/Develop/data/etf_series.Rdata", envir=env_data)

# perform an eapply() loop to extract the adjusted prices for all 
# the variables in "env_data", and call it "etf_series_ad",

# load packages "quantmod", "lubridate", and "PerformanceAnalytics",
library(quantmod)
library(lubridate)
library(PerformanceAnalytics)
etf_series_ad <- do.call(merge, eapply(env_data, Ad))

# "etf_series_ad" should be an xts series containing adjusted prices,
# with colnames in the format "name.Adjusted",
# rename the colnames and drop ".Adjusted" from the colnames,
# use functions sapply() and strsplit(),
colnames(etf_series_ad) <- sapply(colnames(etf_series_ad), 
                                  function(col_name) 
                                    strsplit(col_name, split="[.]")[[1]])[1, ]


# scrub (remove) rows with NA values from "etf_series_ad",
# use function complete.cases(),
etf_series_ad <- etf_series_ad[complete.cases(etf_series_ad)]


# calculate an xts series containing returns of "etf_series_ad", and call it "etf_rets",
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


# Extract the numeric year from each element of the date index of "etf_rets"
# calculate a numeric vector of years from the date index of "etf_rets", 
# and call it "ye_ars",
# you can use either functions format() and as.numeric(), 
# or function year() from package lubridate,
ye_ars <- as.numeric(format(index(etf_rets), "%Y"))
ye_ars <- year(index(etf_rets))


# Calculate a matrix containing the annualized alpha for each ETF in each year, 
# and call it "ann_alphas"
# the matrix "ann_alphas" should have rows corresponding to ETF names, 
# and columns corresponding to years,
# assign row and column names from colnames of "etf_rets",
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

# download the file "etf_data.Rdata" from NYU Classes, and load() it. 
# "etf_data.Rdata" contains an environment called "env_data", 
# with OHLC time series data for ETFs, including "VTI". 

library(quantmod)
load(file="C:/Develop/data/etf_data.Rdata")

# 1. (5pts) Define two integer windows (lookback periods) called 
# "win_short=10" and "win_long=100". 
# Calculate two vectors of VWAPs called "vwap_short" and "vwap_long", 
# for the "VTI" OHLC data. 
# You must use function v_wap() from the previous homework, 

win_short <- 10
win_long <- 100
vwap_short <- v_wap(x_ts=env_data$VTI, win_dow=win_short)
vwap_long <- v_wap(x_ts=env_data$VTI, win_dow=win_long)

# Calculate a numeric vector called "indi_cator", that is 
# equal to 1 when "vwap_short > vwap_long" and equal to -1
# when "vwap_short < vwap_long", 
# The sign of "indi_cator" will determine the strategy's risk 
# positions, either long risk or short risk. 
# You can use function sign(), 

indi_cator <- sign(vwap_short - vwap_long)

# 2. (10pts) Calculate a boolean vector that is TRUE only on dates 
# right after the VWAPs have crossed, and call it "cross_es". 
# For example, if yesterday "vwap_short < vwap_long" and today 
# "vwap_short > vwap_long" (or vice versa), then today "cross_es" 
# should be TRUE, and otherwise it should be FALSE. 
# hint: the diff() of "indi_cator" is not zero right after the 
# VWAPs have crossed, and otherwise it's zero. 
# you can use the functions sign(), diff(), and is.na(), 
# and the logical operator "!=", 
# set any NAs to FALSE, 

cross_es <- (diff(indi_cator) != 0)
cross_es[is.na(cross_es)] <- FALSE

# The strategy should perform trades after "cross_es" becomes TRUE, 
# but with a one period lag, to reflect that in practice it's 
# impossible to trade immediately. 
# Calculate a vector of integer indices corresponding to trade 
# dates, and call it "trade_dates". 
# hint: first calculate the indices corresponding to the periods when 
# "cross_es" is TRUE, and add "1" to them, to reflect the one period lag. 
# you can use function which(), 

trade_dates <- which(cross_es)+1

# The strategy invests in a fixed number of shares called "num_shares".  

num_shares <- 100

# The strategy either owns "num_shares" number of shares (long position), 
# or sells the same number of shares short (short position).  
# Thus the strategy consists of consecutive time intervals of long risk 
# and short risk positions, depending on the sign of "indi_cator". 
# When "indi_cator" becomes positive then the strategy buys shares and 
# flips to a long risk position, and vice versa. 

# 3. (20pts) The strategy should be simulated over a number of periods 
# of time called "n_periods", which should be equal to the number of 
# rows in the OHLC time series data, 
# you can use function nrow(), 

n_periods <- nrow(env_data$VTI)

# Calculate a numeric vector called "pos_ition", that is equal to the 
# number of shares owned by the strategy at any given period of time, 
# either positive (long risk position) or negative (short risk position). 
# The strategy should start with a position of zero. 
# The strategy position should be reset on "trade_dates", depending on 
# the sign of "indi_cator". 
# The strategy position should remain unchamged between the "trade_dates". 
# you can use functions numeric() and na.locf(), 

pos_ition <- NA*numeric(length=n_periods)
pos_ition[1] <- 0
pos_ition[trade_dates] <- num_shares*indi_cator[trade_dates]
pos_ition <- na.locf(pos_ition)

# Lag the vector "pos_ition" by one period and call it "lag_position". 
# The first value of "lag_position" should be zero. 
# you can use function c() combined with subsetting "[]", 

lag_position <- c(0, pos_ition[-n_periods])

# you can inspect the vectors by merging and subsetting them:

foo <- merge(Ad(env_data$VTI), vwap_short, vwap_long, cross_es, pos_ition)
colnames(foo) <- c("price", "vwap_short", "vwap_long", "cross_es", "pos_ition")
foo[(which(cross_es)[2]-3):(which(cross_es)[2]+3), ]

# you should get this:
#               price vwap_short vwap_long cross_es pos_ition
# 2007-02-27 59.09827   60.62776  60.13491        0       100
# 2007-02-28 59.56461   60.42235  60.10604        0       100
# 2007-03-01 59.39503   60.17368  60.06567        0       100
# 2007-03-02 58.54714   59.98786  60.03648        1       100
# 2007-03-05 57.86882   59.65299  59.95280        0      -100
# 2007-03-06 58.84390   59.47699  59.92486        0      -100
# 2007-03-07 58.79727   59.32994  59.90830        0      -100

# Calculate a vector of adjusted close prices from the OHLC data, 
# and call it "price_s". 
# Calculate a vector of lagged "price_s", and call it "lag_prices". 
# Calculate a vector of open prices from the OHLC data, 
# and call it "open_prices". 
# you can use functions Ad(), Op(), and c(), 

price_s <- Ad(env_data$VTI)
lag_prices <- c(0, price_s[-n_periods])
open_prices <- Op(env_data$VTI)

# 4. (20pts) Calculate a vector of periodic (daily) profits and 
# losses and call it "pn_l". 
# The periodic (day over day) profit or loss (pnl) for a period 
# without any trade, is equal to the position in the previous 
# period, times the difference between this period's closing 
# price minus the previous period's closing price. 
# The periodic pnl for a period with a trade, is equal to the 
# sum of two terms.  
# The first term is equal to the position in the previous 
# period, times the difference between this period's opening 
# price minus the previous period's closing price. 
# The first term represents the realized pnl after trading 
# out of the previous position. 
# The second term is equal to the current (new) position times 
# the difference between this period's closing minus opening 
# prices. 
# The second term represents the unrealized pnl of the new 
# position on the day of the trade. 
# you can use the vectors "price_s", "lag_prices", "open_prices", 
# "pos_ition", "lag_position" and "trade_dates", 

pn_l <- lag_position*(price_s - lag_prices)
pn_l[trade_dates] <- 
  lag_position[trade_dates]*(open_prices[trade_dates] - lag_prices[trade_dates]) + 
  pos_ition[trade_dates]*(price_s[trade_dates] - open_prices[trade_dates])

# you can inspect the vectors by merging and subsetting them:

foo <- merge(Ad(env_data$VTI), lag_prices, open_prices, pos_ition, lag_position, pn_l)
colnames(foo) <- c("price", "lag_prices", "open_prices", "pos_ition", "lag_position", "pn_l")
foo[(which(cross_es)[2]-3):(which(cross_es)[2]+3), ]

# you should get this:
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
# run_vwap() should accept three arguments: 
#  "win_short" and "win_long" - two integer lookback windows, 
#  "da_ta" - OHLC time series data, 
# hint: combine all the code from the previous parts. 

run_vwap <- function(win_short=10, win_long=100, da_ta) {
  vwap_short <- v_wap(x_ts=da_ta, win_dow=win_short)
  vwap_long <- v_wap(x_ts=da_ta, win_dow=win_long)
  indi_cator <- sign(vwap_short - vwap_long)
  cross_es <- (diff(indi_cator) != 0)
  cross_es[is.na(cross_es)] <- FALSE
  trade_dates <- which(cross_es)+1
  num_shares <- 100
  n_periods <- nrow(da_ta)
  pos_ition <- NA*numeric(length=n_periods)
  pos_ition[1] <- 0
  pos_ition[trade_dates] <- num_shares*indi_cator[trade_dates]
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
load(file="C:/Develop/data/etf_rets.Rdata")
library(xts)

### run the active investment strategy code from lecture #6,


# calculate the cumulative returns of the strategy "pnl_xts", 
# and the cumulative returns of "etf_rets[, "VTI"]",
# merge the VTI returns with strategy returns, and call it "cum_rets",
# the result of merge should be an xts with two columns,
# be careful about the order of cumsum and merge operations,
# since the indices of "etf_rets" and "pnl_xts" have different frequencies,
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
# since the indices of "etf_rets" and "pnl_xts" have different frequencies,
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





##############################
# numerical analysis
##############################

# most is already incorporated into lecture notes
############## hw
# Study effect of noise on the quality of a regression,
# 
# 1. (20pts) Create a function called reg_stats() which performs 
# a regression and returns a vector of regression statistics,
# the function reg_stats() should accept a single argument "std_dev", 
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
# from=0.1, to=1.0, and call it "vec_sd",
# hint: you can use function seq(),
vec_sd <- seq(from=0.1, to=1.0, length.out=10)

# add the following names to "vec_sd": "sd=0.1", "sd=0.2", etc.
# use functions names() and paste0(),
names(vec_sd) <- paste0("sd=", vec_sd)


# apply the function reg_stats() to the vector "vec_sd", 
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


# create a vector of dates corresponding to "end_points", 
# and call it "in_dex", 
# use function index(),
in_dex <- index(zoo_series[end_points, ])

# create from "in_dex" a vector of dates lagged by 3 months, 
# and call it "in_dex_lag", 
# use function lag.xts() from package xts, 
in_dex_lag <- lag.xts(in_dex, 3)

# between monthly prices
# and the 3-month lagged prices of "xts_series", 

# create a vector of monthly end points lagged by 3 months,
# and cbind() it to "end_points",
# remove rows containing NAs,
# "end_points" should now have two columns:
# the first column should contain monthly indices (integers),
# the second column should contain monthly indices that are lagged by 3 months,
# use function lag.xts() from package xts, and functions cbind() and na.omit(), 
end_points <- cbind(end_points, lag.xts(end_points, 3))
end_points <- na.omit(end_points)



