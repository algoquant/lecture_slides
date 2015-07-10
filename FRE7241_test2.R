#################################
### FRE7241 Test #2 June 23, 2015
#################################
# Max score 30pts

# Please write in this file the R code needed to perform the tasks below, 
# rename the file to your_name_test2.R
# and send the file to Jaimin Doshi (jbd316@nyu.edu)


##################################
# 1. (10pts) Subset 1-minute tick data to weekdays and trading hours.

# load packages lubridate and xts,
library(lubridate)
library(xts)

# Create a vector of 1-minute POSIXct date-times (ticks), and call it "min_ticks", 
# starting from "2015-01-01" to "2015-01-31",
# set the POSIXct timezone to "America/New_York", 
# use functions as.POSIXct() and seq.POSIXt(),

### write your code here


# extract the timezone from "min_ticks" to verify that it's correct, 
# use function tz() from package lubridate,

### write your code here


# Create an xts time series of rnorm data with a "min_ticks" index, and call it "xts_min_ticks",
# use function xts() from package xts,

### write your code here



##################################
# 2. (10pts) remove weekends from "xts_min_ticks", 
# use function weekdays()

### write your code here



##################################
# 3. (10pts) subset the minute ticks in "xts_min_ticks" 
# to trading hours,
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

