#################################
### FRE6871 Test #2 05/04/15
#################################
# Max score 40pts

# Please write in this file the R code needed to perform the tasks below, 
# rename the file to your_name_test2.R
# and send the file to Harjinder Singh (harjinder.singh@nyu.edu)



##################################
# 1. (20pts) Create a vector of daily "Dates" over weekdays, 
# Create a vector of daily "Dates", starting from "2014-07-14" to "2015-05-01",
# and call it "week_days", 
# use functions as.Date() and seq(),

### write your code here


# remove weekends from "week_days", using function weekdays(),

### write your code here


# verify that it's correct, 

weekdays(head(week_days))
weekdays(tail(week_days))



##################################
# 2. (20pts) Convert integers representing dates to "POSIXct" objects,
# Load the package "Ecdat", which contains a data frame called "Yen",
# the column Yen$date contains integers representing dates, in the format "yyyymmdd",
# from the column Yen$date create a vector of "POSIXct" dates, and call it "in_dex", 
# set the POSIXct timezone to "UTC", 
# hint: you can either use functions as.character() and as.POSIXct() 
# (with a "format" argument), 
# or function ymd() from package "lubridate",

### write your code here


# first method

### write your code here

# second method

### write your code here

