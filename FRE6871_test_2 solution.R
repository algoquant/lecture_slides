#################################
### FRE6871 Test #2 Solutions 05/04/15
#################################
# Max score 40pts

# The below solutions are examples,
# Slightly different solutions are also possible.


##################################
# 1. (20pts) Create a vector of daily "Dates" over weekdays, 
# Create a vector of daily "Dates", starting from "2014-07-14" to "2015-05-01",
# and call it "week_days", 
# use functions as.Date() and seq(),

week_days <- seq(from=as.Date("2014-07-14"), 
                 to=as.Date("2015-05-01"), by="day")


# remove weekends from "week_days", using function weekdays(),

day_of_week <- weekdays(week_days)
# first method
is_weekday <- !((day_of_week == "Saturday") | 
                  (day_of_week == "Sunday"))
# second method
is_weekday <- !(day_of_week %in% c("Saturday", "Sunday"))
week_days <- week_days[is_weekday]


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

library("Ecdat")  # load Ecdat
library(lubridate)
head(Yen)  # explore the data


# first method

in_dex <- as.POSIXct(as.character(Yen$date), format="%Y%m%d", tz="UTC")

# second method

in_dex <- ymd(Yen$date, tz="UTC")

head(in_dex)
