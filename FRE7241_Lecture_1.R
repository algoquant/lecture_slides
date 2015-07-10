library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
thm <- knit_theme$get("acid")
knit_theme$set(thm)
set.seed(1121)  # initialize the random number generator
runif(3)  # three random numbers from the uniform distribution
runif(3)  # produce another three numbers
set.seed(1121)  # re-initialize the random number generator
runif(3)  # produce another three numbers

# produce random number from standard normal distribution
rnorm(1)
# produce five random numbers from standard normal distribution
rnorm(5)
# produce five random numbers from the normal distribution
rnorm(n=5, mean=1, sd=2)  # match arguments by name
# calculate cumulative standard normal distribution
c(pnorm(-2), pnorm(2))
# calculate inverse cumulative standard normal distribution
c(qnorm(0.75), qnorm(0.25))
rm(list=ls())
set.seed(1121)  # initialize the random number generator
# sample from Standard Normal Distribution
rand_sample <- rnorm(1000)

mean(rand_sample)  # sample mean

median(rand_sample)  # sample median

sd(rand_sample)  # sample standard deviation
rm(list=ls())
ts_rets <- diff(log(EuStockMarkets[, 1]))  # DAX returns
len_rets <- length(ts_rets)  # number of observations
mean_rets <- mean(ts_rets)  # calculate mean
sd_rets <- sd(ts_rets)  # calculate standard deviation
# calculate skew
len_rets*(sum(((ts_rets - mean_rets)/sd_rets)^3))/
  ((len_rets-1)*(len_rets-2))
# calculate kurtosis
len_rets*(len_rets+1)*(sum(((ts_rets - mean_rets)/sd_rets)^4))/
  ((len_rets-1)^3)
ts_rets <- rnorm(len_rets, sd=2)  # random normal returns
mean_rets <- mean(ts_rets); sd_rets <- sd(ts_rets)
# calculate skew
len_rets*(sum(((ts_rets - mean_rets)/sd_rets)^3))/
  ((len_rets-1)*(len_rets-2))
# calculate kurtosis
len_rets*(len_rets+1)*(sum(((ts_rets - mean_rets)/sd_rets)^4))/
  ((len_rets-1)^3)
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

# create logical vector of business days
bus.days <- isBusinessDay(  # RQuantLib calendar
  calendar="UnitedStates/GovernmentBond", in_dex)

# create daily series of business days
bus_index <- in_dex[bus.days]
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
library(lubridate)  # load lubridate
set.seed(1121)  # for reproducibility
# create daily time series ending today
start_date <- decimal_date(Sys.Date()-6)
end_date <- decimal_date(Sys.Date())
da_ta <- cumsum(rnorm(6))
fre_quency <- length(da_ta)/(end_date-start_date)
ts_series <- ts(data=da_ta,
        start=start_date, 
        frequency=fre_quency)
ts_series  # display time series
# display index dates
as.Date(date_decimal(coredata(time(ts_series))))
# create bi-monthly series starting mid-1990
ts_series <- ts(data=cumsum(rnorm(96)), 
     frequency=6, start=1990.5)
# show some methods for class "ts"
matrix(methods(class="ts")[3:8], ncol=2)
# "tsp" attribute specifies the date-time index
attributes(ts_series)
# extract the index
tail(time(ts_series), 11)
# the index is equally spaced
diff(tail(time(ts_series), 11))
# subset the time series
window(ts_series, start=1992, end=1992.25)
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
plot(ts_series, type="l",  # create plot
     col="red", lty="solid", xlab="", ylab="")
title(main="Random Prices", line=-1)  # add title
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
class(EuStockMarkets)  # multiple ts object
dim(EuStockMarkets)
head(EuStockMarkets, 3)  # get first three rows
# EuStockMarkets index is equally spaced
diff(tail(time(EuStockMarkets), 4))
# plot all the columns
plot(EuStockMarkets, main="", xlab="")
# add title
title(main="EuStockMarkets", line=-2)
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# plot in single panel
plot(EuStockMarkets, main="EuStockMarkets", 
     xlab="", ylab="", plot.type="single", 
     col=c("black", "red", "blue", "green"))
# add legend
legend(x=1992, y=8000, 
       legend=colnames(EuStockMarkets), 
       col=c("black", "red", "blue", "green"), 
       lty=1)
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# calculate DAX percentage returns
dax_rets <- diff(log(EuStockMarkets[, 1]))
# mean and standard deviation of returns
c(mean(dax_rets), sd(dax_rets))
# plot histogram
hist(dax_rets, breaks=30, main="", 
     xlim=c(-0.04, 0.04), ylim=c(0, 60), 
     xlab="", ylab="", freq = FALSE)
# draw kernel density of histogram
lines(density(dax_rets), col='red', lwd=2)
# add density of normal distribution
curve(expr=dnorm(x, mean=mean(dax_rets), sd=sd(dax_rets)), 
      add=TRUE, type="l", lwd=2, col="blue")
title(main="Return distributions", line=0)  # add title
# add legend
legend("topright", inset=0.05, cex=0.8, title=NULL, 
       leg=c(colnames(EuStockMarkets)[1], "Normal"), 
       lwd=2, bg="white", col=c("red", "blue"))
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# calculate percentage returns
dax_rets <- diff(log(EuStockMarkets[, 1]))
# create normal Q-Q plot
qqnorm(dax_rets, ylim=c(-0.04, 0.04), 
       xlab='Normal Quantiles', main='')
# fit a line to the normal quantiles
qqline(dax_rets, col='red', lwd=2)
plot_title <- paste(colnames(EuStockMarkets)[1], 
          'Q-Q Plot')
title(main=plot_title, line=-1)  # add title
shapiro.test(dax_rets)  # Shapiro-Wilk test
par(oma=c(15, 1, 1, 1), mgp=c(1.5, 0.5, 0), mar=c(2.5, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # set plot panels
# boxplot method for formula
boxplot(formula=mpg ~ cyl, data=mtcars, 
  main="Mileage by number of cylinders", 
  xlab="Cylinders", ylab="Miles per gallon")

# calculate EuStockMarkets percentage returns
eu_rets <- diff(log(EuStockMarkets))
# boxplot method for data frame
boxplot(x=eu_rets)
set.seed(1121)  # initialize the random number generator
library(zoo)  # load package zoo
# create zoo time series
in_dex <- Sys.Date() + 0:3
zoo_series <- zoo(rnorm(length(in_dex)), 
         order.by=in_dex)
zoo_series
attributes(zoo_series)
class(zoo_series)  # class 'zoo'
tail(zoo_series, 3)  # get last few elements
library(zoo)  # load package zoo
coredata(zoo_series)  # extract coredata
index(zoo_series)  # extract time index
zoo_series[start(zoo_series)]  # first element
zoo_series[end(zoo_series)]  # last element
coredata(zoo_series) <- rep(1, 4)  # replace coredata
cumsum(zoo_series)  # cumulative sum
cummax(cumsum(zoo_series))
cummin(cumsum(zoo_series))
library(zoo)  # load package zoo
coredata(zoo_series) <- 1:4  # replace coredata
zoo_series
diff(zoo_series)  # diff with one day lag
lag(zoo_series, 2)  # two day lag
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
set.seed(1121)  # initialize the random number generator
library(zoo)  # load package zoo
# create index of daily dates
in_dex <- seq(from=as.Date("2014-07-14"),
            by="day", length.out=1000)
# create vector of data
zoo_data <- cumsum(rnorm(length(in_dex)))
# create zoo time series
zoo_series <- zoo(x=zoo_data,
            order.by=in_dex)
# plot using plot.zoo method
plot(zoo_series, xlab="", ylab="")
title(main="Random Prices", line=-1)  # add title
library(zoo)  # load package zoo
# subset zoo as matrix
zoo_series[459:463, 1]
# subset zoo using window()
window(zoo_series, 
 start=as.Date("2014-10-15"), 
 end=as.Date("2014-10-19"))
# subset zoo using Date object
zoo_series[as.Date("2014-10-15")]
set.seed(1121)  # initialize the random number generator
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
library(zoo)  # load package zoo
# create daily date series of class 'Date'
in_dex1 <- seq(Sys.Date(), by="days", 
             length.out=365)
# create zoo time series
zoo_series1 <- zoo(rnorm(length(in_dex1)), 
           order.by=in_dex1)
# create another zoo time series
in_dex2 <- seq(Sys.Date()+350, by="days", 
             length.out=365)
zoo_series2 <- zoo(rnorm(length(in_dex2)), 
           order.by=in_dex2)
# rbind the two time series - ts1 supersedes ts2
zoo_series3 <- rbind(zoo_series1,
           zoo_series2[index(zoo_series2) > end(zoo_series1)])
plot(cumsum(zoo_series3), xlab="", ylab="")
# add vertical lines at stitch point
abline(v=end(zoo_series1), col="blue", lty="dashed")
abline(v=start(zoo_series2), col="red", lty="dashed")
title(main="Random Prices", line=-1)  # add title
# create daily date series of class 'Date'
in_dex1 <- Sys.Date() + -3:1
# create zoo time series
zoo_series1 <- zoo(rnorm(length(in_dex1)), 
         order.by=in_dex1)
# create another zoo time series
in_dex2 <- Sys.Date() + -1:3
zoo_series2 <- zoo(rnorm(length(in_dex2)), 
         order.by=in_dex2)
merge(zoo_series1, zoo_series2)  # union of dates
# intersection of dates
merge(zoo_series1, zoo_series2, all=FALSE)
library(zoo)  # load package zoo
# create zoo time series
zoo_series <- zoo(sample(4), 
            order.by=(Sys.Date() + 0:3))
# add NA
zoo_series[3] <- NA
zoo_series

na.locf(zoo_series)  # replace NA's using locf

na.omit(zoo_series)  # remove NA's using omit
rm(list=ls())
as.numeric(c(1:3, "a"))  # NA from coercion
0/0  # NaN from ambiguous math
1/0  # Inf from divide by zero
is.na(c(NA, NaN, 0/0, 1/0))  # test for NA
is.nan(c(NA, NaN, 0/0, 1/0))  # test for NaN
NA*1:4  # create vector of Nas
bad_data <- c(1, 2, NA, 4, NA, 5)  # create vector with some NA values
mean(bad_data)  # returns NA, when NAs are input
mean(bad_data, na.rm=TRUE)  # remove NAs from input data
bad_data[!is.na(bad_data)]  # delete the NA values
sum(!is.na(bad_data))  # count non-NA values
rm(list=ls())
head(airquality)  # airquality data has some NAs
dim(airquality)
sum(!complete.cases(airquality))  # number of NAs
head(airquality[!complete.cases(airquality), ])  # display some NAs
rm(list=ls())
good_air <- airquality[complete.cases(airquality), ]  # remove NAs
dim(good_air)
head(good_air)  # NAs removed
library(zoo)  # load package zoo
good_air <- na.locf(airquality)  # replace NAs
dim(good_air)
head(good_air)  # NAs replaced
set.seed(1121)  # for reproducibility
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
library(zoo)  # load package zoo
# create zoo time series
in_dex <- Sys.Date() + 0:365
zoo_series <- zoo(rnorm(length(in_dex)), order.by=in_dex)
# create monthly dates
dates_agg <- as.Date(as.yearmon(index(zoo_series)))
# perform monthly 'mean' aggregation
zoo_agg <- aggregate(zoo_series, by=dates_agg, 
               FUN=mean)
# merge with original zoo - union of dates
zoo_agg <- merge(zoo_series, zoo_agg)
# replace NA's using locf
zoo_agg <- na.locf(zoo_agg)
# extract aggregated zoo
zoo_agg <- zoo_agg[index(zoo_series), 2]
# plot original and aggregated zoo
plot(cumsum(zoo_series), xlab="", ylab="")
lines(cumsum(zoo_agg), lwd=2, col="red")
# add legend
legend("topright", inset=0.05, cex=0.8, title="Aggregated Prices", 
 leg=c("orig prices", "agg prices"), lwd=2, bg="white", 
 col=c("black", "red"))
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# perform monthly 'mean' aggregation
zoo_agg <- aggregate(zoo_series, by=dates_agg, 
               FUN=mean)
# merge with original zoo - union of dates
zoo_agg <- merge(zoo_series, zoo_agg)
# replace NA's using linear interpolation
zoo_agg <- na.approx(zoo_agg)
# extract interpolated zoo
zoo_agg <- zoo_agg[index(zoo_series), 2]
# plot original and interpolated zoo
plot(cumsum(zoo_series), xlab="", ylab="")
lines(cumsum(zoo_agg), lwd=2, col="red")
# add legend
legend("topright", inset=0.05, cex=0.8, title="Interpolated Prices", 
 leg=c("orig prices", "interpol prices"), lwd=2, bg="white", 
 col=c("black", "red"))
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# "mean" aggregation over window with width=11
zoo_mean <- rollapply(zoo_series, width=11, FUN=mean)
# merge with original zoo - union of dates
zoo_mean <- merge(zoo_series, zoo_mean)
# replace NA's using na.locf
zoo_mean <- na.locf(zoo_mean, fromLast=TRUE)
# extract mean zoo
zoo_mean <- zoo_mean[index(zoo_series), 2]
# plot original and interpolated zoo
plot(cumsum(zoo_series), xlab="", ylab="")
lines(cumsum(zoo_mean), lwd=2, col="red")
# add legend
legend("topright", inset=0.05, cex=0.8, title="Mean Prices", 
 leg=c("orig prices", "mean prices"), lwd=2, bg="white", 
 col=c("black", "red"))
library(lubridate)  # load lubridate
library(zoo)  # load package zoo
# methods(as.zoo)  # many methods of coercing into zoo
class(EuStockMarkets)  # multiple ts object
# coerce mts object into zoo
zoo_series <- as.zoo(EuStockMarkets)
class(index(zoo_series))  # index is numeric
head(zoo_series, 3)
# approximately convert index into class 'Date'
index(zoo_series) <- 
  as.Date(365*(index(zoo_series)-1970))
head(zoo_series, 3)
# convert index into class 'POSIXct'
zoo_series <- as.zoo(EuStockMarkets)
index(zoo_series) <- date_decimal(index(zoo_series))
head(zoo_series, 3)
library(lubridate)  # load lubridate
library(zoo)  # load package zoo
set.seed(1121)  # initialize the random number generator
# create index of daily dates
in_dex <- seq(from=as.Date("2014-07-14"), 
            by="day", length.out=1000)
# create vector of data
zoo_data <- cumsum(rnorm(length(in_dex)))
# create zoo time series
zoo_series <- zoo(x=zoo_data, 
            order.by=in_dex)
head(zoo_series, 3)  # zoo object
# as.ts() creates ts object with frequency=1
ts_series <- as.ts(zoo_series)
tsp(ts_series)  # frequency=1
# get start and end dates of zoo_series
start_date <- decimal_date(start(zoo_series))
end_date <- decimal_date(end(zoo_series))
# calculate frequency of zoo_series
fre_quency <- length(zoo_series)/(end_date-start_date)
da_ta <- coredata(zoo_series)  # extract data from zoo_series
# create ts object using ts()
ts_series <- ts(data=da_ta, start=start_date, 
          frequency=fre_quency)
# display start of time series
window(ts_series, start=start(ts_series), 
 end=start(ts_series)+4/365)
head(time(ts_series))  # display index dates
head(as.Date(date_decimal(coredata(time(ts_series)))))
library(lubridate)  # load lubridate
library(zoo)  # load package zoo
# create weekday logical vector
week_days <- weekdays(index(zoo_series))
is_weekday <- !((week_days == "Saturday") | 
  (week_days == "Sunday"))
# remove weekends from zoo time series
zoo_series <- zoo_series[is_weekday, ]
head(zoo_series, 7)  # zoo object
# as.ts() creates NA values
ts_series <- as.ts(zoo_series)
head(ts_series, 7)
# create vector of regular dates, including weekends
in_dex <- seq(from=start(zoo_series), 
            by="day", 
            length.out=length(zoo_series))
index(zoo_series) <- in_dex
ts_series <- as.ts(zoo_series)
head(ts_series, 7)
# get documentation for package "tseries"
packageDescription("tseries")  # get short description

help(package="tseries")  # load help page

library(tseries)  # load package "tseries"

data(package="tseries")  # list all datasets in "tseries"

ls("package:tseries")  # list all objects in "tseries"

detach("package:tseries")  # remove tseries from search path
library(tseries)  # load package tseries
# download MSFT data in ts format
ts_stx <- suppressWarnings(
  get.hist.quote(
    instrument="MSFT",
    start=Sys.Date()-3*365,
    end=Sys.Date(),
    retclass="ts",
    quote=c("Open","High","Low","Close",
      "AdjClose","Volume"),
    origin="1970-01-01")
)  # end suppressWarnings
load(file="C:/Develop/data/zoo_data.RData")
# create price adjustment vector
adj_close <- ts_stx[, "AdjClose"] - 
  ts_stx[, "Close"]
# adjust OHLC prices
ts_stx_adj <- ts_stx
ts_stx_adj[, c("Open","High","Low","Close")] <- 
  ts_stx[, c("Open","High","Low","Close")] + adj_close
# inspect the data
tsp(ts_stx_adj)  # frequency=1
head(time(ts_stx_adj))
head(ts_stx_adj)
tail(ts_stx_adj)
library(tseries)  # load package tseries
# download MSFT data
zoo_stx <- suppressWarnings(
  get.hist.quote(
    instrument="MSFT",
    start=Sys.Date()-3*365,
    end=Sys.Date(),
    quote=c("Open","High","Low","Close",
      "AdjClose","Volume"),
    origin="1970-01-01")
)  # end suppressWarnings
load(file="C:/Develop/data/zoo_data.RData")
class(zoo_stx)
dim(zoo_stx)
head(zoo_stx, 4)
library(tseries)  # load package tseries
load(file="C:/Develop/data/zoo_data.RData")
# create price adjustment vector
adj_close <- zoo_stx[, "AdjClose"] - zoo_stx[, "Close"]
head(adj_close, 5)
tail(adj_close, 5)
# adjust OHLC prices
zoo_stx_adj <- zoo_stx
zoo_stx_adj[, c("Open","High","Low","Close")] <- 
  zoo_stx[, c("Open","High","Low","Close")] + adj_close
head(zoo_stx_adj)
tail(zoo_stx_adj)
library(tseries)  # load package tseries
# download EUR/USD data
zoo_eurusd <- suppressWarnings(
  get.hist.quote(
    instrument="EUR/USD",
    provider="oanda",
    start=Sys.Date()-3*365,
    end=Sys.Date(),
    origin="1970-01-01")
)  # end suppressWarnings
# bind and scrub data
zoo_stxeur <- merge(zoo_eurusd,
               zoo_stx[, "AdjClose"])
colnames(zoo_stxeur) <- c("EURUSD", "MSFT")
zoo_stxeur <-
  zoo_stxeur[complete.cases(zoo_stxeur),]
save(zoo_stx, zoo_stx_adj,
     ts_stx, ts_stx_adj,
     zoo_eurusd, zoo_stxeur,
     file="C:/Develop/data/zoo_data.RData")
load(file="C:/Develop/data/zoo_data.RData")
# inspect the data
class(zoo_eurusd)
tail(zoo_eurusd, 4)
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
load(file="C:/Develop/data/zoo_data.RData")
# get start and end dates
in_dex <- time(ts_stx_adj)
e_nd <- in_dex[length(in_dex)]
st_art <- round((4*e_nd + in_dex[1])/5)
# plot using plotOHLC
plotOHLC(window(ts_stx_adj,
          start=st_art,
          end=e_nd)[, 1:4],
   xlab="", ylab="")
title(main="MSFT OHLC Prices")
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
load(file="C:/Develop/data/zoo_data.RData")
# plot using plot.zoo method
seqplot.ts(zoo_stx[, "AdjClose"], xlab="", ylab="")
title(main="MSFT AdjClose Prices", line=-1)
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
load(file="C:/Develop/data/zoo_data.RData")
#plot with two "y" axes
par(las=1)  # set text printing to "horizontal"
# plot first ts
plot(zoo_stxeur[, 1], xlab=NA, ylab=NA)
# set range of "y" coordinates for second axis
par(usr=c(par("usr")[1:2], range(zoo_stxeur[,2])))
lines(zoo_stxeur[, 2], col="red")  # second plot
axis(side=4, col="red")  # second "y" axis on right
# print axis labels
mtext(colnames(zoo_stxeur)[1], side=2, padj=-6, line=-4)
mtext(colnames(zoo_stxeur)[2], col="red", side=4, padj=-2, line=-3)
title(main="EUR and MSFT")  # add title
# add legend without box
legend("bottomright", legend=colnames(zoo_stxeur), bg="white",
 lty=c(1, 1), lwd=c(2, 2), col=c("black", "red"), bty="n")

##########

# slightly different method using par(new=TRUE)
# par(las=1)  # set text printing to "horizontal"
# plot(zoo_stxeur[, 1], xlab=NA, ylab=NA)
# par(new=TRUE)  # allow new plot on same chart
# plot(zoo_stxeur[, 2], xlab=NA, ylab=NA, yaxt="n", col="red")
# axis(side=4, col="red")  # second "y" axis on right
# mtext(colnames(zoo_stxeur)[1], side=2, padj=-6, line=-4)
# mtext(colnames(zoo_stxeur)[2], col="red", side=4, padj=-2, line=-3)
# title(main="EUR and MSFT", line=-1)  # add title
# legend("bottomright", legend=colnames(zoo_stxeur),
#        lty=c(1, 1), lwd=c(2, 2), col=c("black", "red"), bty="n")

##########

# "x" axis with monthly ticks - doesn't work
# plot first ts wthout "x" axis
# plot(zoo_stxeur[, 1], xaxt="n", xlab=NA, ylab=NA)
# # add "x" axis with monthly ticks
# month.ticks <- unique(as.yearmon(index(zoo_eurusd)))
# axis(side=1, at=month.ticks, labels=format(month.ticks, "%b-%y"), tcl=-0.7)

library(tseries)  # load package tseries
load(file="C:/Develop/data/zoo_data.RData")
# calculate maximum drawdown
maxdrawdown(na.remove(ts_stx[, "AdjClose"]))
# calculate Sharpe ratio
sharpe(na.remove(ts_stx[, "AdjClose"]))
# calculate Sterling ratio
sterling(na.remove(ts_stx[, "AdjClose"]))
library(tseries)  # load package tseries
zoo_stx <- suppressWarnings(  # load MSFT data
  get.hist.quote(instrument="MSFT",
           start=Sys.Date()-365,
           end=Sys.Date(),
           origin="1970-01-01")
)  # end suppressWarnings
class(zoo_stx)
dim(zoo_stx)
tail(zoo_stx, 4)

# calculate Sharpe ratio
sharpe(zoo_stx[, "Close"], r=0.01)
# add title
plot(zoo_stx[, "Close"], xlab="", ylab="")
title(main="MSFT Close Prices", line=-1)
library(tseries)  # load package tseries
zoo_stx <- suppressWarnings(  # load MSFT data
  get.hist.quote(instrument="MSFT",
           start=Sys.Date()-365,
           end=Sys.Date(),
           origin="1970-01-01")
)  # end suppressWarnings
class(zoo_stx)
dim(zoo_stx)
tail(zoo_stx, 4)

# calculate Sharpe ratio
sharpe(zoo_stx[, "Close"], r=0.01)
# add title
plot(zoo_stx[, "Close"], xlab="", ylab="")
title(main="MSFT Close Prices", line=-1)
set.seed(1121)  # initialize the random number generator
library(xts)  # load package xts
# create xts time series
in_dex <- Sys.Date() + 0:3
xts_series <- xts(rnorm(length(in_dex)), 
         order.by=in_dex)
names(xts_series) <- "random"
xts_series
tail(xts_series, 3)  # get last few elements
first(xts_series)  # get first element
last(xts_series)  # get last element
class(xts_series)  # class 'xts'
attributes(xts_series)
load(file="C:/Develop/data/zoo_data.RData")
library(xts)  # load package xts
# as.xts() creates xts from zoo
xts_stx <- as.xts(zoo_stx_adj)
dim(xts_stx)
head(xts_stx[, 1:4], 4)
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# plot using plot.xts method
plot(xts_stx[, "AdjClose"], xlab="", ylab="", main="")
title(main="MSFT Prices")  # add title
library(ggplot2)
load(file="C:/Develop/data/etf_data.Rdata")
# create ggplot object
etf_gg <- autoplot(etf_series_ad[, 1],
             main=names(etf_series_ad[, 1])) +
  xlab("") + ylab("") +
  theme(
    legend.position=c(0.1, 0.5),
    plot.title=element_text(vjust=-2.0),
    plot.background=element_blank()
  )  # end theme
# render ggplot object
etf_gg
library(xts)  # load package xts
# subset xts using a date range string
xts_sub <- xts_stx["2014-10-15/2015-01-10", 1:4]
first(xts_sub)
last(xts_sub)
# subset Nov 2014 using a date string
xts_sub <- xts_stx["2014-11", 1:4]
first(xts_sub)
last(xts_sub)
# subset all data after Nov 2014
xts_sub <- xts_stx["2014-11/", 1:4]
first(xts_sub)
last(xts_sub)
# comma after date range not necessary
identical(xts_stx["2014-11", ], xts_stx["2014-11"])
library(xts)  # load package xts
# vector of 1-minute times (ticks)
min_ticks <- seq.POSIXt(
  from=as.POSIXct("2015-04-14", tz="America/New_York"), 
  to=as.POSIXct("2015-04-16"), 
  by="min")
# xts of 1-minute times (ticks)
xts_series <- xts(rnorm(length(min_ticks)), 
               order.by=min_ticks)
# subset recurring time interval using "T notation",
xts_series <- xts_series["T09:30:00/T16:00:00"]
first(xts_series["2015-04-15"])  # first element of day
last(xts_series["2015-04-15"])  # last element of day
library(xts)  # load package xts
str(xts_stx)  # display structure of xts
# subsetting zoo to single column drops dim attribute
dim(zoo_stx_adj)
dim(zoo_stx_adj[, 1])
# zoo with single column are vectors not matrices
c(is.matrix(zoo_stx_adj), is.matrix(zoo_stx_adj[, 1]))
# xts always have a dim attribute
rbind(base=dim(xts_stx), subs=dim(xts_stx[, 1]))
c(is.matrix(xts_stx), is.matrix(xts_stx[, 1]))
library(xts)  # load package xts
# lag of zoo shortens it by one row
rbind(base=dim(zoo_stx_adj), lag=dim(lag(zoo_stx_adj)))
# lag of xts doesn't shorten it
rbind(base=dim(xts_stx), lag=dim(lag(xts_stx)))
# lag of zoo is in opposite direction from xts
head(lag(zoo_stx_adj), 4)
head(lag(xts_stx), 4)
library(xts)  # load package xts
# extract indices of the last observations in each month
end_points <- endpoints(xts_stx, on='months')
head(end_points)
# extract the last observations in each month
head(xts_stx[end_points, ])
library(xts)  # load package xts
# apply "mean" over end_points
period_mean <- period.apply(xts_stx[, "AdjClose"], 
               INDEX=end_points, 
               FUN=mean)
head(period_mean)
period_sum <- period.sum(xts_stx[, "AdjClose"], 
               INDEX=end_points)
head(period_sum)
library(xts)  # load package xts
# apply "mean" over monthly periods
period_mean <- apply.monthly(xts_stx[, "AdjClose"], 
               FUN=mean)
head(period_mean)
library(xts)  # load package xts
# lower the periodicity to months
xts_monthly <- to.period(x=xts_stx[, "AdjClose"], 
                   period="months", name="MSFT")
# convert colnames to standard OHLC format
colnames(xts_monthly)
colnames(xts_monthly) <- sapply(
  strsplit(colnames(xts_monthly), split=".", fixed=TRUE), 
  function(na_me) na_me[-1]
  )  # end sapply
head(xts_monthly, 3)
# lower the periodicity to years
xts_yearly <- to.period(x=xts_monthly, 
                   period="years", name="MSFT")
colnames(xts_yearly) <- sapply(
  strsplit(colnames(xts_yearly), split=".", fixed=TRUE), 
  function(na_me) na_me[-1]
  )  # end sapply
head(xts_yearly)
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
load(file="C:/Develop/data/zoo_data.RData")
library(xts)  # load package xts
# as.xts() creates xts from zoo
xts_stx <- as.xts(zoo_stx_adj)
# subset xts using a date
xts_sub <- xts_stx["2014-11", 1:4]

# plot OHLC using plot.xts method
plot(xts_sub, type="candles", main="")
title(main="MSFT Prices")  # add title
load(file="C:/Develop/data/zoo_data.RData")
ts_stx <- as.ts(zoo_stx)
class(ts_stx)
tail(ts_stx[, 1:4])
library(xts)
xts_stx <- as.xts(zoo_stx)
class(xts_stx)
tail(xts_stx[, 1:4])
