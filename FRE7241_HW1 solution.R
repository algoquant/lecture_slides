#################################
### FRE7241 HW #1 Solution
#################################
# Max score 60 pts

# The below solutions are examples,
# Slightly different solutions are also possible.



##################################
# 1. (20pts) Subset 1-minute tick data to weekdays and trading hours.

# load packages "lubridate" and "xts",
library(lubridate)
library(xts)

# Create a vector of 1-minute POSIXct date-times (ticks), and call it "min_ticks", 
# starting from "2015-01-01" to "2015-01-31",
# set the POSIXct timezone to "America/New_York", 
# use function seq.POSIXt() 
min_ticks <- seq.POSIXt(from=as.POSIXct("2015-01-01", tz="America/New_York"), 
                        to=as.POSIXct("2015-01-31"), by="min")

# extract the timezone from "min_ticks" to verify that it's correct, 
# use function tz() from package "lubridate",
tz(min_ticks)

# Create an "xts" time series of rnorm data with a "min_ticks" index, and call it "xts_min_ticks",
# use function xts() from package "xts",
xts_min_ticks <- xts(rnorm(length(min_ticks)), order.by=min_ticks)

# remove weekends from "xts_min_ticks", using function weekdays()
week_days <- weekdays(min_ticks)
is_weekday <- !((week_days == "Saturday") | 
                  (week_days == "Sunday"))
xts_min_ticks <- xts_min_ticks[is_weekday]


# subset the minute ticks in "xts_min_ticks" to trading hours,
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



##################################
# 2. (20pts) The package "Ecdat" contains a data.frame called "Yen".
# The column Yen$date contains dates as strings in the format "yyyymmdd",
# from the column Yen$date create a vector of "POSIXct" dates, and call it "in_dex", 
# use function ymd() from package "lubridate",
# set the POSIXct timezone to "America/New_York", 

library("Ecdat")  # load econometric data sets
library(lubridate)
head(Yen)  # explore the data
in_dex <- ymd(Yen$date, tz="America/New_York")

# Create an "xts" from the column Yen$s and "in_dex", and call it "xts_yen",
xts_yen <- xts(Yen$s, order.by=in_dex)

# plot "xts_yen", using generic function plot(),
plot(xts_yen)




##################################
# 3. (20pts) Coerce "EuStockMarkets" to "zoo", and call it "zoo_series",
# coerce the "zoo_series" index into class "POSIXct",
# use function date_decimal() from package "lubridate",
# set the POSIXct timezone to "UTC", 

library(lubridate)
library(xts)
# coerce mts object into zoo
zoo_series <- as.zoo(EuStockMarkets)
# coerce index into class "POSIXct"
index(zoo_series) <- date_decimal(index(zoo_series))

# calculate the rolling mean of the "DAX" column of "zoo_series", and call it "zoo_mean",
zoo_mean <- rollmean(zoo_series[, "DAX"], k=11)

# merge "zoo_mean" with the "DAX" column of "zoo_series", and call it "zoo_mean",
zoo_mean <- merge(zoo_series[, "DAX"], zoo_mean)
# replace NA's using na.locf, both forward and backward in time,
zoo_mean <- na.locf(zoo_mean)
zoo_mean <- na.locf(zoo_mean, fromLast=TRUE)

# calculate the number of NA's in "zoo_mean", to make sure there none
sum(is.na(zoo_mean))

# Coerce "zoo_mean" to "xts", and call it "xts_series",
# use function as.xts() from package "xts",
xts_series <- as.xts(zoo_mean)

# plot both columns of "xts_series" in one panel, starting in 1997,
# with original series in black, and mean series in red,
# use generic function plot(),
plot(xts_series["1997-01-01/", ])
# add legend on topleft
legend("topleft", inset=0.05, cex=0.8, title="DAX Rolling Mean Prices", 
       leg=c("orig prices", "mean prices"), lwd=2, bg="white", 
       col=c("black", "red"))

