#################################
### HW #1 due April 27, 2015
#################################
# Max score 60pts

# Please write in this file the R code needed to perform the tasks below, 
# rename it to your_name_hw2.R
# and send this file to Luping Liu (ll2525@nyu.edu)



##################################
# 1. (20pts) Subset 1-minute tick data to weekdays and trading hours.

# load packages "lubridate" and "xts",
library(lubridate)
library(xts)

# Create a vector of 1-minute POSIXct date-times (ticks), and call it "min_ticks", 
# starting from "2015-01-01" to "2015-01-31",
# set the POSIXct timezone to "America/New_York", 
# use function seq.POSIXt() 

### write your code here


# extract the timezone from "min_ticks" to verify that it's correct, 
# use function tz() from package "lubridate",

### write your code here

# Create an "xts" time series of rnorm data with a "min_ticks" index, and call it "xts_min_ticks",
# use function xts() from package "xts",

### write your code here

# remove weekends from "xts_min_ticks", using function weekdays()

### write your code here


# subset the minute ticks in "xts_min_ticks" to trading hours,
# use the "T notation" for specifying recurring times,
# see page 4 in xts reference manual:  "xts.pdf",
# see also answer by Joshua Ulrich here:
# http://stackoverflow.com/questions/12891232/exclude-specific-time-periods-in-r
# and by Chinmay Patil here:
# http://stackoverflow.com/questions/15284943/cut-a-posixct-by-specific-time-for-daily-means

### write your code here

# extract the timezone from "xts_min_ticks" to verify that it's correct, 
# use function tzone() from package xts,

### write your code here



##################################
# 2. (20pts) The package "Ecdat" contains a data.frame called "Yen".
# The column Yen$date contains dates as strings in the format "yyyymmdd",
# from the column Yen$date create a vector of "POSIXct" dates, and call it "in_dex", 
# use function ymd() from package "lubridate",
# set the POSIXct timezone to "America/New_York", 

library("Ecdat")  # load econometric data sets
library(lubridate)
head(Yen)  # explore the data

### write your code here

# Create an "xts" from the column Yen$date and "in_dex", and call it "xts_yen",

### write your code here

# plot "xts_yen", using generic function plot(),

### write your code here




##################################
# 3. (20pts) Coerce "EuStockMarkets" to "zoo", and call it "zoo_series",
# coerce the "zoo_series" index into class "POSIXct",
# use function date_decimal() from package "lubridate",
# set the POSIXct timezone to "UTC", 

library(lubridate)
library(xts)
# coerce mts object into zoo

### write your code here

# coerce index into class "POSIXct"

### write your code here

# calculate the rolling mean of the "DAX" column of "zoo_series", and call it "zoo_mean",

### write your code here

# merge "zoo_mean" with the "DAX" column of "zoo_series", and call it "zoo_mean",

### write your code here

# replace NA's using na.locf, both forward and backward in time,

### write your code here


# calculate the number of NA's in "zoo_mean", to make sure there none

### write your code here

# Coerce "zoo_mean" to "xts", and call it "xts_series",
# use function as.xts() from package "xts",

### write your code here

# plot both columns of "xts_series" in one panel, starting in 1997,
# with original series in black, and mean series in red,
# use generic function plot(),

### write your code here

# add legend on topleft

### write your code here

