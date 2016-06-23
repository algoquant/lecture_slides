library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
options(digits=3)
thm <- knit_theme$get("acid")
knit_theme$set(thm)
Sys.Date()  # get today's date
date_time <- as.Date("2014-07-14")  # "%Y-%m-%d" or "%Y/%m/%d"
date_time
class(date_time)  # Date object
as.Date("07-14-2014", "%m-%d-%Y")  # specify format
date_time + 20  # add 20 days
# extract internal representation to integer
as.numeric(date_time)
date_old <- as.Date("07/14/2013", "%m/%d/%Y")
date_old
# difference between dates
difftime(date_time, date_old, units="weeks")
weekdays(date_time)  # get day of the week
# coerce numeric into date-times
date_time <- 0
attributes(date_time) <- list(class="Date")
date_time  # "Date" object
structure(0, class="Date")  # "Date" object
structure(10000.25, class="Date")
date_time <- Sys.time()  # get today's date and time
date_time
class(date_time)  # POSIXct object
# POSIXct stored as integer moment of time
as.numeric(date_time)
# parse character string "%Y-%m-%d %H:%M:%S" to POSIXct object
date_time <- as.POSIXct("2014-07-14 13:30:10")
# different time zones can have same clock time
as.POSIXct("2014-07-14 13:30:10", tz="America/New_York")
as.POSIXct("2014-07-14 13:30:10", tz="UTC")
# format argument allows parsing different date-time string formats
as.POSIXct("07/14/2014 13:30:10", format="%m/%d/%Y %H:%M:%S",
     tz="America/New_York")
# same moment of time corresponds to different clock times
time_ny <- as.POSIXct("2014-07-14 13:30:10", 
     tz="America/New_York")
time_ldn <- as.POSIXct("2014-07-14 13:30:10", 
     tz="UTC")
# add five hours to POSIXct
time_ny + 5*60*60
# subtract POSIXct
time_ny - time_ldn
class(time_ny - time_ldn)
# compare POSIXct
time_ny > time_ldn
# create vector of POSIXct times during trading hours
trading_times <- seq(
  from=as.POSIXct("2014-07-14 09:30:00", tz="America/New_York"), 
  to=as.POSIXct("2014-07-14 16:00:00", tz="America/New_York"), 
  by="10 min")
head(trading_times, 3)
tail(trading_times, 3)
# POSIXct is stored as integer moment of time
int_time <- as.numeric(date_time)
# same moment of time corresponds to different clock times
as.POSIXct(int_time, origin="1970-01-01", 
     tz="America/New_York")
as.POSIXct(int_time, origin="1970-01-01", 
     tz="UTC")
# same clock time corresponds to different moments of time
as.POSIXct("2014-07-14 13:30:10", 
     tz="America/New_York") - 
  as.POSIXct("2014-07-14 13:30:10", tz="UTC")
# add 20 seconds to POSIXct
date_time + 20
date_time  # POSIXct date and time
# parse POSIXct to string representing the clock time
format(date_time)
class(format(date_time))  # character string
# get clock times in different time zones
format(date_time, tz="America/New_York")
format(date_time, tz="UTC")
# format with custom format strings
format(date_time, "%m/%Y")
format(date_time, "%m-%d-%Y %H hours")
# trunc to hour
format(date_time, "%m-%d-%Y %H:00:00")
# Date converted to midnight UTC moment of time
as.POSIXct(Sys.Date())
as.POSIXct(as.numeric(as.POSIXct(Sys.Date())), 
     origin="1970-01-01",
     tz="UTC")
# parse character string "%Y-%m-%d %H:%M:%S" to POSIXlt object
date_time <- as.POSIXlt("2014-07-14 18:30:10")
date_time
class(date_time)  # POSIXlt object
as.POSIXct(date_time)  # coerce to POSIXct object
# extract internal list representation to vector
unlist(date_time)
date_time + 20  # add 20 seconds
class(date_time + 20)  # implicit coercion to POSIXct
trunc(date_time, units="hours")  # truncate to closest hour
trunc(date_time, units="days")  # truncate to closest day
methods(trunc)  # trunc methods
trunc.POSIXt
Sys.timezone()  # get time-zone
Sys.setenv(tz="UTC")  # set time-zone to UTC
Sys.timezone()  # get time-zone
# Standard Time in effect
as.POSIXct("2013-03-09 11:00:00", tz="America/New_York")
# Daylight Savings Time in effect
as.POSIXct("2013-03-10 11:00:00", tz="America/New_York")
date_time <- Sys.time()  # today's date and time
# convert to character in different TZ
format(date_time, tz="America/New_York")
format(date_time, tz="UTC")
# parse back to POSIXct
as.POSIXct(format(date_time, tz="America/New_York"))
# difference between local time and UTC
as.POSIXct(format(Sys.time(), tz="UTC")) - 
  as.POSIXct(format(Sys.time(), tz="America/New_York"))
# set time-zone to New York
Sys.setenv(tz="America/New_York")
library(lubridate)  # load lubridate
# parse strings into date-times
as.POSIXct("07-14-2014", format="%m-%d-%Y", tz="America/New_York")
date_time <- mdy("07-14-2014", tz="America/New_York")
date_time
class(date_time)  # POSIXct object
dmy("14.07.2014", tz="America/New_York")

# parse numeric into date-times
as.POSIXct(as.character(14072014), format="%d%m%Y", 
                  tz="America/New_York")
dmy(14072014, tz="America/New_York")

# parse decimal to date-times
decimal_date(date_time)
date_decimal(2014.25, tz="America/New_York")
date_decimal(decimal_date(date_time), tz="America/New_York")
library(lubridate)  # load lubridate
date_time <- ymd_hms(20140714142010, 
               tz="America/New_York")
date_time

# get same moment of time in "UTC" time zone
with_tz(date_time, "UTC")
as.POSIXct(format(date_time, tz="UTC"), tz="UTC")

# get same clock time in "UTC" time zone
force_tz(date_time, "UTC")
as.POSIXct(format(date_time, tz="America/New_York"), 
     tz="UTC")

# same moment of time
date_time - with_tz(date_time, "UTC")

# different moments of time
date_time - force_tz(date_time, "UTC")
library(lubridate)  # load lubridate
# Daylight Savings Time handling periods vs durations
date_time <- as.POSIXct("2013-03-09 11:00:00", 
                  tz="America/New_York")
date_time
date_time + ddays(1)  # add duration
date_time + days(1)  # add period

leap_year(2012)  # leap year
date_time <- dmy(01012012, tz="America/New_York")
date_time
date_time + dyears(1)  # add duration
date_time + years(1)  # add period
library(lubridate)  # load lubridate
date_time <- ymd_hms(20140714142010, tz="America/New_York")
date_time
# add periods to a date-time
c(date_time + seconds(1), date_time + minutes(1), 
date_time + days(1), date_time + months(1))

# create vectors of dates
date_time <- ymd(20140714, tz="America/New_York")
date_time + 0:2 * months(1)  # monthly dates
date_time + months(0:2)
date_time + 0:2 * months(2)  # bi-monthly dates
date_time + seq(0, 5, by=2) * months(1)
seq(date_time, length=3, by="2 months")
library(lubridate)  # load lubridate
# adding monthly periods can create invalid dates
date_time <- ymd(20120131, tz="America/New_York")
date_time + 0:2 * months(1)
date_time + months(1)
date_time + months(2)

# create vector of end-of-month dates
date_time %m-% months(13:1)
library(zoo)  # load zoo
library(RQuantLib)  # load RQuantLib

# create daily date series of class 'Date'
in_dex <- Sys.Date() + -5:2
in_dex

# create boolean vector of business days
is_busday <- isBusinessDay(  # RQuantLib calendar
  calendar="UnitedStates/GovernmentBond", in_dex)

# create daily series of business days
bus_index <- in_dex[is_busday]
bus_index
library(zoo)  # load package zoo
date_time <- Sys.Date()  # create date series of class 'Date'
in_dex <- date_time + 0:365  # daily series over one year
head(in_dex, 4)  # print first few dates
format(head(in_dex, 4), "%m/%d/%Y")  # print first few dates
# create daily date-time series of class 'POSIXct'
in_dex <- seq(Sys.time(), by="days", length.out=365)
head(in_dex, 4)  # print first few dates
format(head(in_dex, 4), "%m/%d/%Y %H:%M:%S")  # print first few dates
# create series of monthly dates of class 'zoo'
monthly_index <- yearmon(2010+0:36/12)
head(monthly_index, 4)  # print first few dates
# create series of quarterly dates of class 'zoo'
qrtly_index <- yearqtr(2010+0:16/4)
head(qrtly_index, 4)  # print first few dates
# parse quarterly 'zoo' dates to POSIXct
Sys.setenv(tz="UTC")
as.POSIXct(head(qrtly_index, 4))
