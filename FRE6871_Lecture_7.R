Sys.Date()  # Get today's date
as.Date(1e3)  # Coerce numeric into date object
date_time <- as.Date("2014-07-14")  # "%Y-%m-%d" or "%Y/%m/%d"
date_time
class(date_time)  # Date object
as.Date("07-14-2014", "%m-%d-%Y")  # Specify format
date_time + 20  # Add 20 days
# Extract internal representation to integer
as.numeric(date_time)
date_old <- as.Date("07/14/2013", "%m/%d/%Y")
date_old
# Difference between dates
difftime(date_time, date_old, units="weeks")
weekdays(date_time)  # Get day of the week
# Coerce numeric into date-times
date_time <- 0
attributes(date_time) <- list(class="Date")
date_time  # "Date" object
structure(0, class="Date")  # "Date" object
structure(10000.25, class="Date")

date_time <- Sys.time()  # Get today's date and time
date_time
class(date_time)  # POSIXct object
# POSIXct stored as integer moment of time
as.numeric(date_time)
# Parse character string "%Y-%m-%d %H:%M:%S" to POSIXct object
date_time <- as.POSIXct("2014-07-14 13:30:10")
# Different time zones can have same clock time
as.POSIXct("2014-07-14 13:30:10", tz="America/New_York")
as.POSIXct("2014-07-14 13:30:10", tz="UTC")
# Format argument allows parsing different date-time string formats
as.POSIXct("07/14/2014 13:30:10", format="%m/%d/%Y %H:%M:%S",
     tz="America/New_York")

# Same moment of time corresponds to different clock times
time_ny <- as.POSIXct("2014-07-14 13:30:10",
     tz="America/New_York")
time_ldn <- as.POSIXct("2014-07-14 13:30:10",
     tz="UTC")
# Add five hours to POSIXct
time_ny + 5*60*60
# subtract POSIXct
time_ny - time_ldn
class(time_ny - time_ldn)
# Compare POSIXct
time_ny > time_ldn
# Create vector of POSIXct times during trading hours
trading_times <- seq(
  from=as.POSIXct("2014-07-14 09:30:00", tz="America/New_York"),
  to=as.POSIXct("2014-07-14 16:00:00", tz="America/New_York"),
  by="10 min")
head(trading_times, 3)
tail(trading_times, 3)

# POSIXct is stored as integer moment of time
int_time <- as.numeric(date_time)
# Same moment of time corresponds to different clock times
as.POSIXct(int_time, origin="1970-01-01",
     tz="America/New_York")
as.POSIXct(int_time, origin="1970-01-01",
     tz="UTC")
# Same clock time corresponds to different moments of time
as.POSIXct("2014-07-14 13:30:10",
     tz="America/New_York") -
  as.POSIXct("2014-07-14 13:30:10", tz="UTC")
# Add 20 seconds to POSIXct
date_time + 20

date_time  # POSIXct date and time
# Parse POSIXct to string representing the clock time
format(date_time)
class(format(date_time))  # Character string
# Get clock times in different time zones
format(date_time, tz="America/New_York")
format(date_time, tz="UTC")
# Format with custom format strings
format(date_time, "%m/%Y")
format(date_time, "%m-%d-%Y %H hours")
# Trunc to hour
format(date_time, "%m-%d-%Y %H:00:00")
# Date converted to midnight UTC moment of time
as.POSIXct(Sys.Date())
as.POSIXct(as.numeric(as.POSIXct(Sys.Date())),
     origin="1970-01-01",
     tz="UTC")

# Parse character string "%Y-%m-%d %H:%M:%S" to POSIXlt object
date_time <- as.POSIXlt("2014-07-14 18:30:10")
date_time
class(date_time)  # POSIXlt object
as.POSIXct(date_time)  # Coerce to POSIXct object
# Extract internal list representation to vector
unlist(date_time)
date_time + 20  # Add 20 seconds
class(date_time + 20)  # Implicit coercion to POSIXct
trunc(date_time, units="hours")  # Truncate to closest hour
trunc(date_time, units="days")  # Truncate to closest day
methods(trunc)  # Trunc methods
trunc.POSIXt

Sys.timezone()  # Get time-zone
Sys.setenv(TZ="UTC")  # set time-zone to UTC
Sys.timezone()  # Get time-zone
# Standard Time in effect
as.POSIXct("2013-03-09 11:00:00", tz="America/New_York")
# Daylight Savings Time in effect
as.POSIXct("2013-03-10 11:00:00", tz="America/New_York")
date_time <- Sys.time()  # Today's date and time
# Convert to character in different TZ
format(date_time, tz="America/New_York")
format(date_time, tz="UTC")
# Parse back to POSIXct
as.POSIXct(format(date_time, tz="America/New_York"))
# Difference between New_York time and UTC
as.POSIXct(format(Sys.time(), tz="UTC")) -
  as.POSIXct(format(Sys.time(), tz="America/New_York"))
# set time-zone to New York
Sys.setenv(TZ="America/New_York")

library(lubridate)  # Load lubridate
# Parse strings into date-times
as.POSIXct("07-14-2014", format="%m-%d-%Y", tz="America/New_York")
date_time <- mdy("07-14-2014", tz="America/New_York")
date_time
class(date_time)  # POSIXct object
dmy("14.07.2014", tz="America/New_York")

# Parse numeric into date-times
as.POSIXct(as.character(14072014), format="%d%m%Y",
                  tz="America/New_York")
dmy(14072014, tz="America/New_York")

# Parse decimal to date-times
decimal_date(date_time)
date_decimal(2014.25, tz="America/New_York")
date_decimal(decimal_date(date_time), tz="America/New_York")

library(lubridate)  # Load lubridate
date_time <- lubridate::ymd_hms(20140714142010,
               tz="America/New_York")
date_time
# Get same moment of time in "UTC" time zone
lubridate::with_tz(date_time, "UTC")
as.POSIXct(format(date_time, tz="UTC"), tz="UTC")
# Get same clock time in "UTC" time zone
lubridate::force_tz(date_time, "UTC")
as.POSIXct(format(date_time, tz="America/New_York"),
     tz="UTC")
# Same moment of time
date_time - with_tz(date_time, "UTC")
# Different moments of time
date_time - force_tz(date_time, "UTC")

library(lubridate)  # Load lubridate
# Daylight Savings Time handling periods vs durations
date_time <- as.POSIXct("2013-03-09 11:00:00",
                  tz="America/New_York")
date_time
date_time + lubridate::ddays(1)  # Add duration
date_time + lubridate::days(1)  # Add period

leap_year(2012)  # Leap year
date_time <- lubridate::dmy(01012012, tz="America/New_York")
date_time
date_time + lubridate::dyears(1)  # Add duration
date_time + lubridate::years(1)  # Add period

library(lubridate)  # Load lubridate
date_time <- lubridate::ymd_hms(20140714142010, tz="America/New_York")
date_time
# Add periods to a date-time
c(date_time + lubridate::seconds(1), date_time + lubridate::minutes(1),
  date_time + lubridate::days(1), date_time + lubridate::months(1))

# Create vectors of dates
date_time <- lubridate::ymd(20140714, tz="America/New_York")
date_time + 0:2 * lubridate::months(1)  # Monthly dates
date_time + lubridate::months(0:2)
date_time + 0:2 * lubridate::months(2)  # bi-monthly dates
date_time + seq(0, 5, by=2) * lubridate::months(1)
seq(date_time, length=3, by="2 months")

library(lubridate)  # Load lubridate
# Adding monthly periods can create invalid dates
date_time <- lubridate::ymd(20120131, tz="America/New_York")
date_time + 0:2 * lubridate::months(1)
date_time + lubridate::months(1)
date_time + lubridate::months(2)

# Create vector of end-of-month dates
date_time %m-% lubridate::months(13:1)

library(zoo)  # Load zoo
library(RQuantLib)  # Load RQuantLib

# Create daily date series of class "Date"
in_dex <- Sys.Date() + -5:2
in_dex

# Create Boolean vector of business days
is_busday <- isBusinessDay(  # RQuantLib calendar
  calendar="UnitedStates/GovernmentBond", in_dex)

# Create daily series of business days
bus_index <- in_dex[is_busday]
bus_index

library(zoo)  # Load package zoo
date_time <- Sys.Date()  # Create date series of class "Date"
in_dex <- date_time + 0:365  # Daily series over one year
head(in_dex, 4)  # Print first few dates
format(head(in_dex, 4), "%m/%d/%Y")  # Print first few dates
# Create daily date-time series of class "POSIXct"
in_dex <- seq(Sys.time(), by="days", length.out=365)
head(in_dex, 4)  # Print first few dates
format(head(in_dex, 4), "%m/%d/%Y %H:%M:%S")  # Print first few dates
# Create series of monthly dates of class "zoo"
monthly_index <- yearmon(2010+0:36/12)
head(monthly_index, 4)  # Print first few dates
# Create series of quarterly dates of class "zoo"
qrtly_index <- yearqtr(2010+0:16/4)
head(qrtly_index, 4)  # Print first few dates
# Parse quarterly "zoo" dates to POSIXct
Sys.setenv(TZ="UTC")
as.POSIXct(head(qrtly_index, 4))

library(lubridate)  # Load lubridate
set.seed(1121)  # Reset random number generator
# Create daily time series ending today
start_date <- decimal_date(Sys.Date()-6)
end_date <- decimal_date(Sys.Date())
# Create vector of geometric Brownian motion
da_ta <- exp(cumsum(rnorm(6)/100))
fre_quency <- NROW(da_ta)/(end_date-start_date)
ts_series <- ts(data=da_ta,
          start=start_date,
          frequency=fre_quency)
ts_series  # Display time series
# Display index dates
as.Date(date_decimal(coredata(time(ts_series))))
# bi-monthly geometric Brownian motion starting mid-1990
ts_series <- ts(data=exp(cumsum(rnorm(96)/100)),
       frequency=6, start=1990.5)

# show some methods for class "ts"
matrix(methods(class="ts")[3:8], ncol=2)
# "tsp" attribute specifies the date-time index
attributes(ts_series)
# Extract the index
tail(time(ts_series), 11)
# The index is equally spaced
diff(tail(time(ts_series), 11))
# Subset the time series
window(ts_series, start=1992, end=1992.25)

par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
plot(ts_series, type="l",  # Create plot
     col="red", lty="solid", xlab="", ylab="")
title(main="Random Prices", line=-1)  # Add title

class(EuStockMarkets)  # Multiple ts object
dim(EuStockMarkets)
head(EuStockMarkets, 3)  # Get first three rows
# EuStockMarkets index is equally spaced
diff(tail(time(EuStockMarkets), 11))

par(mar=c(1, 2, 1, 1), oma=c(0, 0, 0, 0))
# Plot all the columns in separate panels
plot(EuStockMarkets, main="EuStockMarkets", xlab="")

par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# Plot in single panel
plot(EuStockMarkets, main="EuStockMarkets",
     xlab="", ylab="", plot.type="single",
     col=c("black", "red", "blue", "green"))
# Add legend
legend(x=1992, y=8000,
 legend=colnames(EuStockMarkets),
 col=c("black", "red", "blue", "green"),
 lwd=6, lty=1)

library(rutils)  # Load package rutils
# Calculate VTI percentage returns
re_turns <- na.omit(rutils::etf_env$re_turns$VTI)
# Mean and standard deviation of returns
c(mean(re_turns), sd(re_turns))
# Plot histogram
x11(width=6, height=5)
par(mar=c(1, 1, 1, 1), oma=c(2, 2, 2, 0))
histo_gram <- hist(re_turns, breaks=100,
  main="", ylim=c(0, 60), xlim=c(-0.04, 0.04),
  xlab="", ylab="", freq=FALSE)
# Draw kernel density of histogram
lines(density(re_turns), col="red", lwd=2)
# Add density of normal distribution
curve(expr=dnorm(x, mean=mean(re_turns), sd=sd(re_turns)),
add=TRUE, type="l", lwd=2, col="blue")
title(main="VTI Return Distribution", line=0)  # Add title
# Add legend
legend("topright", inset=0.05, cex=0.8, title=NULL,
 leg=c("VTI", "Normal"), bty="n",
 lwd=6, bg="white", col=c("red", "blue"))

# Perform Shapiro-Wilk test
shapiro.test(as.numeric(re_turns))
# Create normal Q-Q plot
qqnorm(re_turns, ylim=c(-0.1, 0.1), main="VTI Q-Q Plot",
 xlab="Normal Quantiles")
# Fit a line to the normal quantiles
qqline(re_turns, col="red", lwd=2)

# Boxplot method for formula
boxplot(formula=mpg ~ cyl, data=mtcars,
  main="Mileage by number of cylinders",
  xlab="Cylinders", ylab="Miles per gallon")
# Boxplot method for data frame of EuStockMarkets percentage returns
boxplot(x=diff(log(EuStockMarkets)))

set.seed(1121)  # Reset random number generator
library(zoo)  # Load package zoo
# Create zoo time series of random returns
in_dex <- Sys.Date() + 0:3
zoo_series <- zoo(rnorm(NROW(in_dex)),
         order.by=in_dex)
zoo_series
attributes(zoo_series)
class(zoo_series)  # Class "zoo"
tail(zoo_series, 3)  # Get last few elements

library(zoo)  # Load package zoo
coredata(zoo_series)  # Extract coredata
index(zoo_series)  # Extract time index
start(zoo_series)  # First date
end(zoo_series)  # Last date
zoo_series[start(zoo_series)]  # First element
zoo_series[end(zoo_series)]  # Last element
coredata(zoo_series) <- rep(1, 4)  # Replace coredata
cumsum(zoo_series)  # Cumulative sum
cummax(cumsum(zoo_series))
cummin(cumsum(zoo_series))

library(zoo)  # Load package zoo
zoo_series <-
  zoo(as.matrix(cumsum(rnorm(100)), nc=1),
order.by=seq(from=as.Date("2013-06-15"),
             by="day", length.out=100))
colnames(zoo_series) <- "zoo_series"
tail(zoo_series)
dim(zoo_series)
attributes(zoo_series)

library(zoo)  # Load package zoo
coredata(zoo_series) <- (1:4)^2  # Replace coredata
zoo_series
lag(zoo_series)  # One day lag
lag(zoo_series, 2)  # Two day lag
lag(zoo_series, k=-1)  # Proper one day lag
diff(zoo_series)  # Diff with one day lag
# Proper lag and original length
lag(zoo_series, -2, na.pad=TRUE)

set.seed(1121)  # Reset random number generator
library(zoo)  # Load package zoo
# Create index of daily dates
in_dex <- seq(from=as.Date("2014-07-14"),
            by="day", length.out=1000)
# Create vector of geometric Brownian motion
zoo_data <-
  exp(cumsum(rnorm(NROW(in_dex))/100))
# Create zoo series of geometric Brownian motion
zoo_series <- zoo(x=zoo_data, order.by=in_dex)

par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# Plot using plot.zoo method
plot(zoo_series, xlab="", ylab="")
title(main="Random Prices", line=-1)  # Add title

library(zoo)  # Load package zoo
# Subset zoo as matrix
zoo_series[459:463, 1]
# Subset zoo using window()
window(zoo_series,
 start=as.Date("2014-10-15"),
 end=as.Date("2014-10-19"))
# Subset zoo using Date object
zoo_series[as.Date("2014-10-15")]

set.seed(1121)  # Reset random number generator
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
library(zoo)  # Load package zoo
# Create daily date series of class "Date"
in_dex1 <- seq(Sys.Date(), by="days",
             length.out=365)
# Create zoo time series of random returns
zoo_series1 <- zoo(rnorm(NROW(in_dex1)),
           order.by=in_dex1)
# Create another zoo time series of random returns
in_dex2 <- seq(Sys.Date()+350, by="days",
             length.out=365)
zoo_series2 <- zoo(rnorm(NROW(in_dex2)),
           order.by=in_dex2)
# rbind the two time series - ts1 supersedes ts2
zoo_series3 <- rbind(zoo_series1,
           zoo_series2[index(zoo_series2) > end(zoo_series1)])
# Plot zoo time series of geometric Brownian motion
plot(exp(cumsum(zoo_series3)/100), xlab="", ylab="")
# Add vertical lines at stitch point
abline(v=end(zoo_series1), col="blue", lty="dashed")
abline(v=start(zoo_series2), col="red", lty="dashed")
title(main="Random Prices", line=-1)  # Add title

# Create daily date series of class "Date"
in_dex1 <- Sys.Date() + -3:1
# Create zoo time series of random returns
zoo_series1 <- zoo(rnorm(NROW(in_dex1)),
         order.by=in_dex1)
# Create another zoo time series of random returns
in_dex2 <- Sys.Date() + -1:3
zoo_series2 <- zoo(rnorm(NROW(in_dex2)),
         order.by=in_dex2)
merge(zoo_series1, zoo_series2)  # union of dates
# Intersection of dates
merge(zoo_series1, zoo_series2, all=FALSE)

# Create matrix containing NA values
mat_rix <- sample(18)
mat_rix[sample(NROW(mat_rix), 4)] <- NA
mat_rix <- matrix(mat_rix, nc=3)
# Replace NA values with most recent non-NA values
zoo::na.locf(mat_rix)
rutils::na_locf(mat_rix)
# Get time series of prices
price_s <- mget(c("VTI", "VXX"), envir=rutils::etf_env)
price_s <- lapply(price_s, quantmod::Cl)
price_s <- rutils::do_call(cbind, price_s)
sum(is.na(price_s))
# Carry forward and backward non-NA prices
price_s <- zoo::na.locf(price_s, na.rm=FALSE)
price_s <- zoo::na.locf(price_s, na.rm=FALSE, fromLast=TRUE)
sum(is.na(price_s))
# Remove whole rows containing NA returns
re_turns <- rutils::etf_env$re_turns
sum(is.na(re_turns))
re_turns <- na.omit(re_turns)
# Or carry forward non-NA returns (preferred)
re_turns <- rutils::etf_env$re_turns
re_turns[1, is.na(re_turns[1, ])] <- 0
re_turns <- zoo::na.locf(re_turns, na.rm=FALSE)
sum(is.na(re_turns))

# Replace NAs in xts time series
se_ries <- rutils::etf_env$price_s[, 1]
head(se_ries)
sum(is.na(se_ries))
library(quantmod)
series_zoo <- as.xts(zoo::na.locf(se_ries, na.rm=FALSE, fromLast=TRUE))
series_xts <- xts:::na.locf.xts(se_ries, fromLast=TRUE)
all.equal(series_zoo, series_xts, check.attributes=FALSE)
library(microbenchmark)
summary(microbenchmark(
  zoo=as.xts(zoo::na.locf(se_ries, fromLast=TRUE)),
  xts=xts:::na.locf.xts(se_ries, fromLast=TRUE),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

library(lubridate)  # Load lubridate
library(zoo)  # Load package zoo
# methods(as.zoo)  # Many methods of coercing into zoo
class(EuStockMarkets)  # Multiple ts object
# Coerce mts object into zoo
zoo_series <- as.zoo(EuStockMarkets)
class(index(zoo_series))  # Index is numeric
head(zoo_series, 3)
# Approximately convert index into class "Date"
index(zoo_series) <-
  as.Date(365*(index(zoo_series)-1970))
head(zoo_series, 3)
# Convert index into class "POSIXct"
zoo_series <- as.zoo(EuStockMarkets)
index(zoo_series) <- date_decimal(index(zoo_series))
head(zoo_series, 3)

library(lubridate)  # Load lubridate
library(zoo)  # Load package zoo
set.seed(1121)  # Reset random number generator
# Create index of daily dates
in_dex <- seq(from=as.Date("2014-07-14"),
            by="day", length.out=1000)
# Create vector of geometric Brownian motion
zoo_data <- exp(cumsum(rnorm(NROW(in_dex))/100))
# Create zoo time series of geometric Brownian motion
zoo_series <- zoo(x=zoo_data,
            order.by=in_dex)
head(zoo_series, 3)  # zoo object
# as.ts() creates ts object with frequency=1
ts_series <- as.ts(zoo_series)
tsp(ts_series)  # Frequency=1
# Get start and end dates of zoo_series
start_date <- decimal_date(start(zoo_series))
end_date <- decimal_date(end(zoo_series))
# Calculate frequency of zoo_series
fre_quency <- NROW(zoo_series)/(end_date-start_date)
da_ta <- coredata(zoo_series)  # Extract data from zoo_series
# Create ts object using ts()
ts_series <- ts(data=da_ta, start=start_date,
          frequency=fre_quency)
# Display start of time series
window(ts_series, start=start(ts_series),
 end=start(ts_series)+4/365)
head(time(ts_series))  # Display index dates
head(as.Date(date_decimal(coredata(time(ts_series)))))

library(lubridate)  # Load lubridate
library(zoo)  # Load package zoo
# Create weekday Boolean vector
week_days <- weekdays(index(zoo_series))
is_weekday <- !((week_days == "Saturday") |
  (week_days == "Sunday"))
# Remove weekends from zoo time series
zoo_series <- zoo_series[is_weekday, ]
head(zoo_series, 7)  # zoo object
# as.ts() creates NA values
ts_series <- as.ts(zoo_series)
head(ts_series, 7)
# Create vector of regular dates, including weekends
in_dex <- seq(from=start(zoo_series),
            by="day",
            length.out=NROW(zoo_series))
index(zoo_series) <- in_dex
ts_series <- as.ts(zoo_series)
head(ts_series, 7)

set.seed(1121)  # Reset random number generator
library(xts)  # Load package xts
# Create xts time series of random returns
in_dex <- Sys.Date() + 0:3
x_ts <- xts(rnorm(NROW(in_dex)),
         order.by=in_dex)
names(x_ts) <- "random"
x_ts
tail(x_ts, 3)  # Get last few elements
first(x_ts)  # Get first element
last(x_ts)  # Get last element
class(x_ts)  # Class "xts"
attributes(x_ts)
# Get the time zone of an xts object
indexTZ(x_ts)

load(file="C:/Develop/lecture_slides/data/zoo_data.RData")
library(xts)  # Load package xts
# as.xts() coerces zoo series into xts series
st_ox <- as.xts(zoo_stx)
dim(st_ox)
head(st_ox[, 1:4], 4)
# Plot using plot.xts method
xts::plot.xts(st_ox[, "Close"], xlab="", ylab="", main="")
title(main="MSFT Prices")  # Add title

library(xts)  # Load xts
library(lubridate)  # Load lubridate
# Coerce EuStockMarkets into class xts
x_ts <- xts(coredata(EuStockMarkets),
      order.by=date_decimal(index(EuStockMarkets)))
# Plot all columns in single panel: xts v.0.9-8
col_ors <- rainbow(NCOL(x_ts))
plot(x_ts, main="EuStockMarkets using xts",
     col=col_ors, major.ticks="years",
     minor.ticks=FALSE)
legend("topleft", legend=colnames(EuStockMarkets),
 inset=0.2, cex=0.7, , lty=rep(1, NCOL(x_ts)),
 lwd=3, col=col_ors, bg="white")
# Plot only first column: xts v.0.9-7
plot(x_ts[, 1], main="EuStockMarkets using xts",
     col=col_ors[1], major.ticks="years",
     minor.ticks=FALSE)
# Plot remaining columns
for (col_umn in 2:NCOL(x_ts))
  lines(x_ts[, col_umn], col=col_ors[col_umn])
# Plot using quantmod
library(quantmod)
plot_theme <- chart_theme()
plot_theme$col$line.col <- col_ors
chart_Series(x=x_ts, theme=plot_theme,
       name="EuStockMarkets using quantmod")
legend("topleft", legend=colnames(EuStockMarkets),
 inset=0.2, cex=0.7, , lty=rep(1, NCOL(x_ts)),
 lwd=3, col=col_ors, bg="white")

library(rutils)
library(ggplot2)
price_s <- rutils::etf_env$price_s[, 1]
price_s <- na.omit(price_s)
# Create ggplot object
etf_gg <- qplot(x=index(price_s),
          y=as.numeric(price_s),
          geom="line",
          main=names(price_s)) +
  xlab("") + ylab("") +
  theme(  # Add legend and title
    legend.position=c(0.1, 0.5),
    plot.title=element_text(vjust=-2.0),
    plot.background=element_blank()
  )  # end theme
# Render ggplot object
etf_gg

library(rutils)  # Load xts time series data
library(reshape2)
library(ggplot2)
price_s <- rutils::etf_env$price_s[, c("VTI", "IEF")]
price_s <- na.omit(price_s)
# Create data frame of time series
data_frame <- data.frame(dates=index(price_s),
    coredata(price_s))
# reshape data into a single column
data_frame <-
  reshape2::melt(data_frame, id="dates")
x11(width=6, height=5)  # Open plot window
# ggplot the melted data_frame
ggplot(data=data_frame,
 mapping=aes(x=dates, y=value, colour=variable)) +
 geom_line() +
  xlab("") + ylab("") +
  ggtitle("VTI and IEF") +
  theme(  # Add legend and title
    legend.position=c(0.2, 0.8),
    plot.title=element_text(vjust=-2.0)
  )  # end theme

# Load rutils which contains etf_env dataset
library(rutils)
library(dygraphs)
price_s <- rutils::etf_env$price_s[, c("VTI", "IEF")]
price_s <- na.omit(price_s)
# Plot dygraph with date range selector
dygraph(price_s, main="VTI and IEF prices") %>%
  dyOptions(colors=c("blue","green")) %>%
  dyRangeSelector()

# Load rutils which contains etf_env dataset
library(rutils)
library(plotly)
price_s <- rutils::etf_env$price_s[, c("VTI", "IEF")]
price_s <- na.omit(price_s)
# Create data frame of time series
data_frame <- data.frame(dates=index(price_s),
    coredata(price_s))
# Plotly syntax using pipes
data_frame %>%
  plot_ly(x=~dates, y=~VTI, type="scatter", mode="lines", name="VTI") %>%
  add_trace(x=~dates, y=~IEF, type="scatter", mode="lines", name="IEF") %>%
  layout(title="VTI and IEF prices",
   xaxis=list(title="Time"),
   yaxis=list(title="Stock Prices"),
   legend=list(x=0.1, y=0.9))
# Or use standard plotly syntax
p_lot <- plot_ly(data=data_frame, x=~dates, y=~VTI, type="scatter", mode="lines", name="VTI")
p_lot <- add_trace(p=p_lot, x=~dates, y=~IEF, type="scatter", mode="lines", name="IEF")
p_lot <- layout(p=p_lot, title="VTI and IEF prices", xaxis=list(title="Time"), yaxis=list(title="Stock Prices"), legend=list(x=0.1, y=0.9))
p_lot

# Subset xts using a date range string
price_s <- rutils::etf_env$price_s
sub_prices <- price_s["2014-10-15/2015-01-10", 1:4]
first(sub_prices)
last(sub_prices)
# Subset Nov 2014 using a date string
sub_prices <- price_s["2014-11", 1:4]
first(sub_prices)
last(sub_prices)
# Subset all data after Nov 2014
sub_prices <- price_s["2014-11/", 1:4]
first(sub_prices)
last(sub_prices)
# Comma after date range not necessary
all.equal(price_s["2014-11", ], price_s["2014-11"])
# .subset_xts() is faster than the bracket []
library(microbenchmark)
summary(microbenchmark(
  bracket=price_s[10:20, ],
  subset=xts::.subset_xts(price_s, 10:20),
  times=10))[, c(1, 4, 5)]

# Specify string representing a date
dat_e <- "2014-10-15"
# Subset price_s in two different ways
price_s <- rutils::etf_env$price_s
all.equal(price_s[index(price_s) >= dat_e],
    price_s[paste0(dat_e, "/")])
# Boolean subsetting is slower because coercing string into date
library(microbenchmark)
summary(microbenchmark(
  boolean=(price_s[index(price_s) >= dat_e]),
  date=(price_s[paste0(dat_e, "/")]),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Coerce string into a date
dat_e <- as.Date("2014-10-15")
# Boolean subsetting is faster than using date string
summary(microbenchmark(
  boolean=(price_s[index(price_s) >= dat_e]),
  date=(price_s[paste0(dat_e, "/")]),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

price_s <- HighFreq::SPY["2012-04"]
# Subset recurring time interval using "T notation",
price_s <- price_s["T10:30:00/T15:00:00"]
first(price_s["2012-04-16"])  # First element of day
last(price_s["2012-04-16"])  # Last element of day
# suppress timezone warning messages
options(xts_check_tz=FALSE)

price_s <- rutils::etf_env$price_s[, c("VTI", "IEF")]
price_s <- na.omit(price_s)
str(price_s)  # Display structure of xts
# Subsetting zoo to single column drops dim attribute
zoo_prices <- as.zoo(price_s)
dim(zoo_prices)
dim(zoo_prices[, 1])
# zoo with single column are vectors not matrices
c(is.matrix(zoo_prices), is.matrix(zoo_prices[, 1]))
# xts always have a dim attribute
rbind(base=dim(price_s), subs=dim(price_s[, 1]))
c(is.matrix(price_s), is.matrix(price_s[, 1]))

# Lag of zoo shortens it by one row
rbind(base=dim(zoo_prices), lag=dim(lag(zoo_prices)))
# Lag of xts doesn't shorten it
rbind(base=dim(price_s), lag=dim(lag(price_s)))
# Lag of zoo is in opposite direction from xts
head(lag(zoo_prices, -1), 4)
head(lag(price_s), 4)

# library(rutils)  # Load package rutils
# Indices of last observations in each hour
end_p <- xts::endpoints(price_s, on="hours")
head(end_p)
# Extract the last observations in each hour
head(price_s[end_p, ])

# Lower the periodicity to months
xts_monthly <- to.period(x=price_s,
             period="months", name="MSFT")
# Convert colnames to standard OHLC format
colnames(xts_monthly)
colnames(xts_monthly) <- sapply(
  strsplit(colnames(xts_monthly), split=".", fixed=TRUE),
  function(na_me) na_me[-1]
  )  # end sapply
head(xts_monthly, 3)
# Lower the periodicity to years
xts_yearly <- to.period(x=xts_monthly,
             period="years", name="MSFT")
colnames(xts_yearly) <- sapply(
  strsplit(colnames(xts_yearly), split=".", fixed=TRUE),
  function(na_me) na_me[-1]
  )  # end sapply
head(xts_yearly)

par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
load(file="C:/Develop/lecture_slides/data/zoo_data.RData")
library(xts)  # Load package xts
# as.xts() coerces zoo series into xts series
st_ox <- as.xts(zoo_prices)
# Subset xts using a date
stox_sub <- st_ox["2014-11", 1:4]

# Plot OHLC using plot.xts method
xts::plot.xts(stox_sub, type="candles", main="")
title(main="MSFT Prices")  # Add title

load(file="C:/Develop/lecture_slides/data/zoo_data.RData")
ts_stx <- as.ts(zoo_stx)
class(ts_stx)
tail(ts_stx[, 1:4])
library(xts)
st_ox <- as.xts(zoo_stx)
class(st_ox)
tail(st_ox[, 1:4])

load(file="C:/Develop/lecture_slides/data/zoo_data.RData")
library(tseries)  # Load package tseries
# Download MSFT data in ts format
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
# Calculate price adjustment vector
adj_vector <-
  as.vector(ts_stx[, "AdjClose"] / ts_stx[, "Close"])
# Adjust OHLC prices
ts_stx_adj <- ts_stx
ts_stx_adj[, c("Open","High","Low","Close")] <-
  adj_vector * ts_stx[, c("Open","High","Low","Close")]
# Inspect the data
tsp(ts_stx_adj)  # frequency=1
head(time(ts_stx_adj))
head(ts_stx_adj)
tail(ts_stx_adj)

library(tseries)  # Load package tseries
# Download MSFT data
zoo_stx <- suppressWarnings(
  get.hist.quote(
    instrument="MSFT",
    start=Sys.Date()-3*365,
    end=Sys.Date(),
    quote=c("Open","High","Low","Close",
      "AdjClose","Volume"),
    origin="1970-01-01")
)  # end suppressWarnings

load(file="C:/Develop/lecture_slides/data/zoo_data.RData")
class(zoo_stx)
dim(zoo_stx)
head(zoo_stx, 4)

library(tseries)  # Load package tseries
load(file="C:/Develop/lecture_slides/data/zoo_data.RData")
# Calculate price adjustment vector
adj_vector <-
  as.vector(zoo_stx[, "AdjClose"] / zoo_stx[, "Close"])
head(adj_vector, 5)
tail(adj_vector, 5)
# Adjust OHLC prices
zoo_stx_adj <- zoo_stx
zoo_stx_adj[, c("Open","High","Low","Close")] <-
  adj_vector * zoo_stx[, c("Open","High","Low","Close")]
head(zoo_stx_adj)
tail(zoo_stx_adj)

library(tseries)  # Load package tseries
# Download EUR/USD data
zoo_eurusd <- suppressWarnings(
  get.hist.quote(
    instrument="EUR/USD",
    provider="oanda",
    start=Sys.Date()-3*365,
    end=Sys.Date(),
    origin="1970-01-01")
)  # end suppressWarnings
# Bind and scrub data
zoo_stxeur <- cbind(zoo_eurusd,
               zoo_stx[, "AdjClose"])
colnames(zoo_stxeur) <- c("EURUSD", "MSFT")
zoo_stxeur <-
  zoo_stxeur[complete.cases(zoo_stxeur),]
save(zoo_stx, zoo_stx_adj,
     ts_stx, ts_stx_adj,
     zoo_eurusd, zoo_stxeur,
     file="C:/Develop/lecture_slides/data/zoo_data.RData")

load(file="C:/Develop/lecture_slides/data/zoo_data.RData")
# Inspect the data
class(zoo_eurusd)
head(zoo_eurusd, 4)

library(tseries)  # Load package tseries
# Download price and volume data for sym_bols into list of zoo objects
zoo_series <- suppressWarnings(
  lapply(sym_bols, # Loop for loading data
   get.hist.quote,
   quote=c("AdjClose", "Volume"),
   start=Sys.Date()-3650,
   end=Sys.Date(),
   origin="1970-01-01")  # end lapply
)  # end suppressWarnings
# Flatten list of zoo objects into a single zoo object
zoo_series <- rutils::do_call(cbind, zoo_series)
# Or
# zoo_series <- do.call(cbind, zoo_series)
# Assign names in format "symbol.Close", "symbol.Volume"
names(zoo_series) <-
  as.vector(sapply(sym_bols,
    paste, c("Close", "Volume"), sep="."))
# Save zoo_series to a comma-separated CSV file
write.zoo(zoo_series, file="zoo_series.csv", sep=",")
# Save zoo_series to a binary .RData file
save(zoo_series, file="zoo_series.RData")

library(rutils)  # Load package rutils
# Check of object is an OHLC time series
is.OHLC(etf_env$VTI)
# Adjust single OHLC object using its name
etf_env$VTI <- adjustOHLC(etf_env$VTI,
                    use.Adjusted=TRUE)

# Adjust OHLC object using string as name
assign(sym_bols[1], adjustOHLC(
    get(x=sym_bols[1], envir=etf_env),
    use.Adjusted=TRUE),
  envir=etf_env)

# Adjust objects in environment using vector of strings
for (sym_bol in ls(etf_env)) {
  assign(sym_bol,
   adjustOHLC(get(sym_bol, envir=etf_env),
              use.Adjusted=TRUE),
   envir=etf_env)
}  # end for

library(rutils)  # Load package rutils
# Define ETF symbols
sym_bols <- c("VTI", "VEU", "IEF", "VNQ")
# Extract sym_bols from rutils::etf_env
price_s <- mget(sym_bols, envir=rutils::etf_env)
# price_s is a list of xts series
class(price_s)
class(price_s[[1]])
# Extract Close prices
price_s <- lapply(price_s, quantmod::Cl)
# Collapse list into time series the hard way
xts_1 <- cbind(price_s[[1]], price_s[[2]], price_s[[3]], price_s[[4]])
class(xts_1)
dim(xts_1)
# Collapse list into time series using do.call()
price_s <- do.call(cbind, price_s)
all.equal(xts_1, price_s)
class(price_s)
dim(price_s)
# Extract and cbind in single step
price_s <- do.call(cbind, lapply(
  mget(sym_bols, envir=rutils::etf_env),
  quantmod::Cl))
# Or
# Extract and bind all data, subset by sym_bols
price_s <- lapply(sym_bols, function(sym_bol) {
    quantmod::Cl(get(sym_bol, envir=rutils::etf_env))
})  # end lapply
# Same, but loop over etf_env without anonymous function
price_s <- do.call(cbind,
  lapply(as.list(rutils::etf_env)[sym_bols],
   quantmod::Cl))
# Same, but works only for OHLC series - produces error
price_s <- do.call(cbind,
  eapply(rutils::etf_env, quantmod::Cl)[sym_bols])

# Drop ".Close" from colnames
colnames(price_s) <- unname(sapply(colnames(price_s),
    function(col_name)
strsplit(col_name, split="[.]")[[1]][1]))
tail(price_s, 3)
# Which objects in global environment are class xts?
unlist(eapply(globalenv(), is.xts))
# Save xts to csv file
write.zoo(price_s,
  file="C:/Develop/lecture_slides/data/etf_series.csv", sep=",")
# Copy price_s into etf_env
etf_env$etf_list <- etf_list
# Or
assign("price_s", price_s, envir=etf_env)
# Save to .RData file
save(etf_env, file="etf_data.RData")

# Extract VTI prices
vt_i <- etf_env$price_s[ ,"VTI"]
vt_i <- na.omit(vt_i)
# Calculate percentage returns "by hand"
vti_lag <- as.numeric(vt_i)
vti_lag <- c(vti_lag[1], vti_lag[-NROW(vti_lag)])
vti_lag <- xts(vti_lag, index(vt_i))
vti_returns <- (vt_i-vti_lag)/vti_lag
# Calculate percentage returns using dailyReturn()
daily_returns <- quantmod::dailyReturn(vt_i)
head(cbind(daily_returns, vti_returns))
all.equal(daily_returns, vti_returns, check.attributes=FALSE)
# Calculate returns for all prices in etf_env$price_s
re_turns <- lapply(etf_env$price_s, function(x_ts) {
  daily_returns <- quantmod::dailyReturn(na.omit(x_ts))
  colnames(daily_returns) <- names(x_ts)
  daily_returns
})  # end lapply
# "re_turns" is a list of xts
class(re_turns)
class(re_turns[[1]])
# Flatten list of xts into a single xts
re_turns <- do.call(cbind, re_turns)
class(re_turns)
dim(re_turns)
# Copy re_turns into etf_env and save to .RData file
# assign("re_turns", re_turns, envir=etf_env)
etf_env$re_turns <- re_turns
save(etf_env, file="C:/Develop/lecture_slides/data/etf_data.RData")

library(quantmod)
start_date <- "2012-05-10"; end_date <- "2013-11-20"
# Select all objects in environment and return as environment
new_env <- as.environment(eapply(etf_env, "[",
            paste(start_date, end_date, sep="/")))
# Select only sym_bols in environment and return as environment
new_env <- as.environment(
  lapply(as.list(etf_env)[sym_bols], "[",
   paste(start_date, end_date, sep="/")))
# Extract and cbind Close prices and return to environment
assign("price_s", rutils::do_call(cbind,
         lapply(ls(etf_env), function(sym_bol) {
           x_ts <- quantmod::Cl(get(sym_bol, etf_env))
           colnames(x_ts) <- sym_bol
           x_ts
         })), envir=new_env)
# get sizes of OHLC xts series in etf_env
sapply(mget(sym_bols, envir=etf_env), object.size)
# Extract and cbind adjusted prices and return to environment
col_name <- function(x_ts)
  strsplit(colnames(x_ts), split="[.]")[[1]][1]
assign("price_s", rutils::do_call(cbind,
         lapply(mget(etf_env$sym_bols, envir=etf_env),
                function(x_ts) {
                  x_ts <- Ad(x_ts)
                  colnames(x_ts) <- col_name(x_ts)
                  x_ts
         })), envir=new_env)

# Load data frame of S&P500 constituents from CSV file
sp_500 <- read.csv(file="C:/Develop/lecture_slides/data/sp500_WRDS_08-30-17.csv", stringsAsFactors=FALSE)
# Inspect data frame of S&P500 constituents
dim(sp_500)
colnames(sp_500)
# Extract tickers from the column co_tic
sym_bols <- sp_500$co_tic
# Get duplicate tickers
ta_ble <- table(sym_bols)
dupli_cate <- ta_ble[ta_ble>1]
dupli_cate <- names(dupli_cate)
# Get duplicate records (rows) of sp_500
sp_500[sym_bols %in% dupli_cate, ]
# Get unique tickers
sym_bols <- unique(sym_bols)
# Find index of ticker "BRK.B"
which(sym_bols=="BRK.B")
# Remove "BRK.B" and later download it separately
sym_bols <- sym_bols[-which(sym_bols=="BRK.B")]

# Load package rutils
library(rutils)
# Create new environment for data
sp500_env <- new.env()
# Boolean vector of symbols already downloaded
down_loaded <- sym_bols %in% ls(sp500_env)
# Download in while loop from Tiingo and copy into environment
at_tempt <- 0  # number of download attempts
while (((sum(!down_loaded)) > 0) & (at_tempt<5)) {
  # Download data and copy it into environment
  at_tempt <- at_tempt + 1
  cat("Download attempt = ", at_tempt, "\n")
  for (sym_bol in sym_bols[!down_loaded]) {
    cat("processing: ", sym_bol, "\n")
    tryCatch(  # With error handler
quantmod::getSymbols(sym_bol, src="tiingo", adjust=TRUE, auto.assign=TRUE,
           from="1990-01-01", env=sp500_env, api.key="d84fc2a9c5bde2d68e33034f65a838092c6b9f10"),
# Error handler captures error condition
error=function(error_cond) {
  print(paste("error handler: ", error_cond))
},  # end error handler
finally=print(paste("sym_bol=", sym_bol))
    )  # end tryCatch
  }  # end for
  # Update vector of symbols already downloaded
  down_loaded <- sym_bols %in% ls(sp500_env)
  Sys.sleep(10)  # Wait 10 seconds until next attempt
}  # end while
class(sp500_env$AAPL)
class(index(sp500_env$AAPL))

library(quantmod)
# Rename "LOW" colnames to "LOWES"
colnames(sp500_env$LOW) <- paste("LO_WES",
  sapply(strsplit(colnames(sp500_env$LOW), split="[.]"),
   function(col_name) col_name[2]), sep=".")
sp500_env$LOWES <- sp500_env$LOW[, unique(colnames(sp500_env$LOW))]
rm(LOW, envir=sp500_env)
chart_Series(x=sp500_env$LOWES["2017-06/"],
  TA="add_Vo()", name="LOWES stock")
# Download "BRK.B" separately with auto.assign=FALSE
BRKB <- quantmod::getSymbols("BRK-B", auto.assign=FALSE, src="tiingo", adjust=TRUE, from="1990-01-01", api.key="j84ac2b9c5bde2d68e33034f65d838092c6c9f10")
colnames(BRKB) <- paste("BRKB",
  sapply(strsplit(colnames(BRKB), split="[.]"),
   function(col_name) col_name[2]), sep=".")
sp500_env$BRKB <- BRKB

# Rename "BF-B" colnames to "BFB"
colnames(sp500_env$"BF-B") <- paste("BFB",
  sapply(strsplit(colnames(sp500_env$"BF-B"), split="[.]"),
   function(col_name) col_name[2]), sep=".")
names(colnames(sp500_env$"BF-B")) <- NULL
sp500_env$BFB <- sp500_env$"BF-B"
rm("BF-B", envir=sp500_env)

class(sp500_env$AAPL)
# The date-time index is class POSIXct not Date
class(index(sp500_env$AAPL))
# Coerce time indices from class POSIXct to class Date
for (sym_bol in ls(sp500_env)) {
  x_ts <- get(sym_bol, envir=sp500_env)
  index(x_ts) <- as.Date(index(x_ts))
  assign(sym_bol, x_ts, envir=sp500_env)
}  # end for
class(index(sp500_env$AAPL))
# Save the environment to compressed .RData file
dir_name <- "C:/Develop/lecture_slides/data/"
save(sp500_env, file=paste0(dir_name, "sp500.RData"))
# Save the ETF prices into CSV files
dir_name <- "C:/Develop/lecture_slides/data/SP500/"
for (sym_bol in ls(sp500_env)) {
  zoo::write.zoo(sp500_env$sym_bol, file=paste0(dir_name, sym_bol, ".csv"))
}  # end for
# Or using lapply()
file_names <- lapply(ls(sp500_env), function(sym_bol) {
  x_ts <- get(sym_bol, envir=sp500_env)
  zoo::write.zoo(x_ts, file=paste0(dir_name, sym_bol, ".csv"))
  sym_bol
})  # end lapply
unlist(file_names)
# Or using eapply() and data.table::fwrite()
file_names <- eapply(sp500_env , function(x_ts) {
  file_name <- rutils::get_name(colnames(x_ts)[1])
  data.table::fwrite(data.table::as.data.table(x_ts), file=paste0(dir_name, file_name, ".csv"))
  file_name
})  # end eapply
unlist(file_names)

# Load the environment from compressed .RData file
dir_name <- "C:/Develop/lecture_slides/data/"
load(file=paste0(dir_name, "sp500.RData"))
# Get all the .csv file names in the directory
dir_name <- "C:/Develop/lecture_slides/data/SP500/"
file_names <- Sys.glob(paste0(dir_name, "*.csv"))
# Create new environment for data
sp500_env <- new.env()
for (file_name in file_names) {
  x_ts <- xts::as.xts(zoo::read.csv.zoo(file_name))
  sym_bol <- strsplit(colnames(x_ts), split="[.]")[[1]][1]
  assign(sym_bol, x_ts, envir=sp500_env)
}  # end for
# Or using fread()
for (file_name in file_names) {
  x_ts <- data.table::fread(file_name)
  data.table::setDF(x_ts)
  x_ts <- xts::xts(x_ts[, -1], as.Date(x_ts[, 1]))
  sym_bol <- strsplit(colnames(x_ts), split="[.]")[[1]][1]
  assign(sym_bol, x_ts, envir=sp500_env)
}  # end for

# Remove all files from environment(if necessary)
rm(list=ls(sp500_env), envir=sp500_env)
# Download in while loop from Alpha Vantage and copy into environment
down_loaded <- sym_bols %in% ls(sp500_env)
at_tempt <- 0
while (((sum(!down_loaded)) > 0) & (at_tempt<10)) {
  # Download data and copy it into environment
  at_tempt <- at_tempt + 1
  for (sym_bol in sym_bols[!down_loaded]) {
    cat("processing: ", sym_bol, "\n")
    tryCatch(  # With error handler
quantmod::getSymbols(sym_bol, src="av", adjust=TRUE, auto.assign=TRUE, env=sp500_env,
           output.size="full", api.key="T7JPW54ES8G75310"),
# error handler captures error condition
error=function(error_cond) {
  print(paste("error handler: ", error_cond))
},  # end error handler
finally=print(paste("sym_bol=", sym_bol))
    )  # end tryCatch
  }  # end for
  # Update vector of symbols already downloaded
  down_loaded <- sym_bols %in% ls(sp500_env)
  Sys.sleep(10)  # Wait 10 seconds until next attempt
}  # end while
# Adjust all OHLC prices in environment
for (sym_bol in ls(sp500_env)) {
  assign(sym_bol,
    adjustOHLC(get(x=sym_bol, envir=sp500_env), use.Adjusted=TRUE),
    envir=sp500_env)
}  # end for

library(rutils)  # Load package rutils
# Assign name SP500 to ^GSPC symbol
setSymbolLookup(
  SP500=list(name="^GSPC", src="yahoo"))
getSymbolLookup()
# view and clear options
options("getSymbols.sources")
options(getSymbols.sources=NULL)
# Download S&P500 prices into etf_env
quantmod::getSymbols("SP500", env=etf_env,
    adjust=TRUE, auto.assign=TRUE, from="1990-01-01")
chart_Series(x=etf_env$SP500["2016/"],
       TA="add_Vo()",
       name="S&P500 index")

library(rutils)  # Load package rutils
# Assign name DJIA to ^DJI symbol
setSymbolLookup(
  DJIA=list(name="^DJI", src="yahoo"))
getSymbolLookup()
# view and clear options
options("getSymbols.sources")
options(getSymbols.sources=NULL)
# Download DJIA prices into etf_env
quantmod::getSymbols("DJIA", env=etf_env,
    adjust=TRUE, auto.assign=TRUE, from="1990-01-01")
chart_Series(x=etf_env$DJIA["2016/"],
       TA="add_Vo()",
       name="DJIA index")

library(rutils)  # Load package rutils
library(RCurl)  # Load package RCurl
library(XML)  # Load package XML
# Download text data from URL
sp_500 <- getURL(
  "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies")
# Extract tables from the text data
sp_500 <- readHTMLTable(sp_500,
              stringsAsFactors=FALSE)
str(sp_500)
# Extract colnames of data frames
lapply(sp_500, colnames)
# Extract S&P500 constituents
sp_500 <- sp_500[[1]]
head(sp_500)
# Create valid R names from symbols containing "-" or "."characters
sp_500$names <- gsub("-", "_", sp_500$Ticker)
sp_500$names <- gsub("[.]", "_", sp_500$names)
# Write data frame of S&P500 constituents to CSV file
write.csv(sp_500,
  file="C:/Develop/lecture_slides/data/sp500_Yahoo.csv",
  row.names=FALSE)

library(rutils)  # Load package rutils
# Load data frame of S&P500 constituents from CSV file
sp_500 <- read.csv(file="C:/Develop/lecture_slides/data/sp500_Yahoo.csv",
     stringsAsFactors=FALSE)
# Register symbols corresponding to R names
for (in_dex in 1:NROW(sp_500)) {
  cat("processing: ", sp_500$Ticker[in_dex], "\n")
  setSymbolLookup(structure(
    list(list(name=sp_500$Ticker[in_dex])),
    names=sp_500$names[in_dex]))
}  # end for
sp500_env <- new.env()  # new environment for data
# Remove all files (if necessary)
rm(list=ls(sp500_env), envir=sp500_env)
# Download data and copy it into environment
rutils::get_data(sp_500$names,
   env_out=sp500_env, start_date="1990-01-01")
# Or download in loop
for (sym_bol in sp_500$names) {
  cat("processing: ", sym_bol, "\n")
  rutils::get_data(sym_bol,
   env_out=sp500_env, start_date="1990-01-01")
}  # end for
save(sp500_env, file="C:/Develop/lecture_slides/data/sp500.RData")
chart_Series(x=sp500_env$BRKB["2016/"], TA="add_Vo()",
       name="BRK-B stock")

library(quantmod)
# Download U.S. unemployment rate data
unemp_rate <- quantmod::getSymbols("UNRATE",
            auto.assign=FALSE,
            src="FRED")
# Plot U.S. unemployment rate data
chart_Series(unemp_rate["1990/"],
      name="U.S. unemployment rate")

library(rutils)  # Load package rutils
install.packages("devtools")
library(devtools)
# Install package Quandl from github
install_github("quandl/R-package")
library(Quandl)  # Load package Quandl
# Register Quandl API key
Quandl.api_key("pVJi9Nv3V8CD3Js5s7Qx")
# get short description
packageDescription("Quandl")
# Load help page
help(package="Quandl")
# Remove Quandl from search path
detach("package:Quandl")

library(rutils)  # Load package rutils
# Download EOD AAPL prices from WIKI free database
price_s <- Quandl(code="WIKI/AAPL",
            type="xts", start_date="1990-01-01")
x11(width=14, height=7)
chart_Series(price_s["2016", 1:4],
    name="AAPL OHLC prices")
# Add trade volume in extra panel
add_TA(price_s["2016", 5])
# Download euro currency rates
price_s <- Quandl(code="BNP/USDEUR",
    start_date="2013-01-01",
    end_date="2013-12-01", type="xts")
# Download multiple time series
price_s <- Quandl(code=c("NSE/OIL", "WIKI/AAPL"),
    start_date="2013-01-01", type="xts")
# Download AAPL gross profits
prof_it <- Quandl("RAYMOND/AAPL_GROSS_PROFIT_Q",
    type="xts")
chart_Series(prof_it, name="AAPL gross profits")
# Download Hurst time series
price_s <- Quandl(code="PE/AAPL_HURST",
    start_date="2013-01-01", type="xts")
chart_Series(price_s["2016/", 1],
       name="AAPL Hurst")

library(rutils)  # Load package rutils
# Load S&P500 stock Quandl codes
sp_500 <- read.csv(
  file="C:/Develop/lecture_slides/data/sp500_quandl.csv",
  stringsAsFactors=FALSE)
# Replace "-" with "_" in symbols
sp_500$free_code <-
  gsub("-", "_", sp_500$free_code)
head(sp_500)
# vector of symbols in sp_500 frame
tick_ers <- gsub("-", "_", sp_500$ticker)
# Or
tick_ers <- matrix(unlist(
  strsplit(sp_500$free_code, split="/"),
  use.names=FALSE), ncol=2, byrow=TRUE)[, 2]
# Or
tick_ers <- do_call_rbind(
  strsplit(sp_500$free_code, split="/"))[, 2]

library(rutils)  # Load package rutils
sp500_env <- new.env()  # new environment for data
# Remove all files (if necessary)
rm(list=ls(sp500_env), envir=sp500_env)
# Boolean vector of symbols already downloaded
down_loaded <- tick_ers %in% ls(sp500_env)
# Download data and copy it into environment
for (tick_er in tick_ers[!down_loaded]) {
  cat("processing: ", tick_er, "\n")
  da_ta <- Quandl(code=paste0("WIKI/", tick_er),
            start_date="1990-01-01",
            type="xts")[, -(1:7)]
  colnames(da_ta) <- paste(tick_er,
    c("Open", "High", "Low", "Close", "Volume"), sep=".")
  assign(tick_er, da_ta, envir=sp500_env)
}  # end for
save(sp500_env, file="C:/Develop/lecture_slides/data/sp500.RData")
chart_Series(x=sp500_env$XOM["2016/"], TA="add_Vo()",
       name="XOM stock")

library(rutils)
library(Quandl)
# Register Quandl API key
Quandl.api_key("pVJi9Nv3V8CD3Js5s7Qx")
# Download E-mini S&P500 futures prices
price_s <- Quandl(code="CHRIS/CME_ES1",
  type="xts", start_date="1990-01-01")
price_s <- price_s[, c("Open", "High", "Low", "Last", "Volume")]
colnames(price_s)[4] <- "Close"
# Plot the prices
x11(width=5, height=4)  # Open x11 for plotting
chart_Series(x=price_s["2008-06/2009-06"],
       TA="add_Vo()",
       name="S&P500 Futures")
# Plot dygraph
dygraphs::dygraph(price_s["2008-06/2009-06", -5],
  main="S&P500 Futures") %>%
  dyCandlestick()

# Read CBOE futures expiration dates
date_s <- read.csv(file="C:/Develop/lecture_slides/data/futures_expiration_dates_codes.csv",
  stringsAsFactors=FALSE, row.names=1)
dir_name <- "C:/Develop/data/vix_data"
dir.create(dir_name)
sym_bols <- rownames(date_s)
file_names <- file.path(dir_name, paste0(sym_bols, ".csv"))
log_file <- file.path(dir_name, "log_file.txt")
cboe_url <- "https://markets.cboe.com/us/futures/market_statistics/historical_data/products/csv/VX/"
url_s <- paste0(cboe_url, date_s[, 1])
# Download files in loop
for (it in seq_along(url_s)) {
    tryCatch(  # Warning and error handler
  download.file(url_s[it],
          destfile=file_names[it], quiet=TRUE),
# Warning handler captures warning condition
warning=function(warning_cond) {
  cat(paste("warning handler: ", warning_cond, "\n"), file=log_file, append=TRUE)
},  # end warning handler
# Error handler captures error condition
error=function(error_cond) {
  cat(paste("error handler: ", error_cond, "\n"), append=TRUE)
},  # end error handler
finally=cat(paste("Processing file name =", file_names[it], "\n"), append=TRUE)
    )  # end tryCatch
}  # end for

# Create new environment for data
vix_env <- new.env()
# Download VIX data for the months 6, 7, and 8 in 2018
library(qmao)
quantmod::getSymbols("VX", Months=1:12,
  Years=2018, src="cfe", auto.assign=TRUE, env=vix_env)
# Or
qmao::getSymbols.cfe(Symbols="VX",
  Months=6:8, Years=2018, env=vix_env,
  verbose=FALSE, auto.assign=TRUE)
# Calculate the classes of all the objects
# In the environment vix_env
unlist(eapply(vix_env,
  function(x) {class(x)[1]}))
class(vix_env$VX_M18)
colnames(vix_env$VX_M18)
# Save the data to a binary file called "vix_cboe.RData".
save(vix_env,
  file="C:/Develop/data/vix_data/vix_cboe.RData")

# Install and load package readxl
install.packages("readxl")
library(readxl)
dir_name <- "C:/Develop/lecture_slides/data"
fil_e <- file.path(dir_name, "multi_tabs.xlsx")
# Read a time series from first sheet of xlsx file
tib_ble <- readxl::read_xlsx(fil_e)
class(tib_ble)
# Coerce POSIXct dates into Date class
class(tib_ble$Dates)
tib_ble$Dates <- as.Date(tib_ble$Dates)
# Some columns are character strings
sapply(tib_ble, class)
sapply(tib_ble, is.character)
# Coerce columns with strings to numeric
lis_t <- lapply(tib_ble, function(x) {
  if (is.character(x))
    as.numeric(x)
  else
    x
})  # end lapply
# Coerce list into xts time series
x_ts <- xts::xts(do.call(cbind, lis_t)[, -1], lis_t[[1]])
class(x_ts); dim(x_ts)
# Replace NA values with the most recent non-NA values
sum(is.na(x_ts))
x_ts <- zoo::na.locf(x_ts, na.rm=FALSE)
x_ts <- zoo::na.locf(x_ts, fromLast=TRUE)

# Read names of all the sheets in an Excel spreadsheet
name_s <- readxl::excel_sheets(fil_e)
# Read all the sheets from an Excel spreadsheet
sheet_s <- lapply(name_s, read_xlsx, path=fil_e)
names(sheet_s) <- name_s
# sheet_s is a list of tibbles
sapply(sheet_s, class)
# Create function to coerce tibble to xts
to_xts <- function(tib_ble) {
  tib_ble$Dates <- as.Date(tib_ble$Dates)
  # Coerce columns with strings to numeric
  lis_t <- lapply(tib_ble, function(x) {
    if (is.character(x))
      as.numeric(x)
    else
      x
  })  # end lapply
  # Coerce list into xts series
  xts::xts(do.call(cbind, lis_t)[, -1], lis_t$Dates)
}  # end to_xts
# Coerce list of tibbles to list of xts
class(sheet_s)
sheet_s <- lapply(sheet_s, to_xts)
sapply(sheet_s, class)
# Replace NA values with the most recent non-NA values
sapply(sheet_s, function(x_ts) sum(is.na(x_ts)))
sheet_s <- lapply(sheet_s, zoo::na.locf, na.rm=FALSE)
sheet_s <- lapply(sheet_s, zoo::na.locf, fromLast=TRUE)

#Perform calculations in R,
#And export to CSV files
setwd("C:/Develop/lecture_slides/data")
# Read data frame, with row names from first column
data_read <- read.csv(file="florist.csv",
              row.names=1)
# Subset data frame
data_read <-
  data_read[data_read[, "type"]=="daisy", ]
# Write data frame to CSV file, with row names
write.csv(data_read, file="daisies.csv")

#Perform calculations in R,
#And export to CSV files
setwd("C:/Develop/lecture_slides/data")
# Read data frame, with row names from first column
data_read <- read.csv(file="florist.csv",
              row.names=1)
# Subset data frame
data_read <-
  data_read[data_read[, "type"]=="daisy", ]
# Write data frame to CSV file, with row names
write.csv(data_read, file="daisies.csv")

# Install latest version of googlesheets
devtools::install_github("jennybc/googlesheets")
# Load package googlesheets
library(googlesheets)
library(dplyr)
# Authenticate authorize R to view and manage your files
gs_auth(new_user=TRUE)
# List the files in Google Sheets
googlesheets::gs_ls()
# Register a sheet
google_sheet <- gs_title("my_data")
# view sheet summary
google_sheet
# List tab names in sheet
tab_s <- gs_ws_ls(google_sheet)
# Set curl options
library(httr)
httr::set_config(config(ssl_verifypeer=0L))
# Read data from sheet
gs_read(google_sheet)
# Read data from single tab of sheet
gs_read(google_sheet, ws=tab_s[1])
gs_read_csv(google_sheet, ws=tab_s[1])
# Or using dplyr pipes
google_sheet %>% gs_read(ws=tab_s[1])
# Download data from sheet into file
gs_download(google_sheet, ws=tab_s[1],
      to="C:/Develop/lecture_slides/data/google_sheet.csv")
# Open sheet in internet browser
gs_browse(google_sheet)

library(zoo)  # Load package zoo
# Show the generic function "merge"
merge
# Show the "merge" method dispatched to "zoo" objects
merge.zoo

library(zoo)  # Load package zoo
# Get all methods for generic function merge()
methods(generic.function="merge")
# Get generic function methods applied to "zoo" objects
methods(class="zoo")

cbind.ts  # Can't view non-visible method
stats::cbind.ts  # Can't view non-visible method
stats:::cbind.ts  # Display non-visible method
getAnywhere(cbind.ts)  # Display non-visible method

getOption("repos")  # get default package source
.libPaths()  # get package save directory

install.packages("AER")  # install "AER" from CRAN
# install "PerformanceAnalytics" from R-Forge
install.packages(
  pkgs="PerformanceAnalytics",  # name
  lib="C:/Users/Jerzy/Downloads",  # directory
  repos="http://R-Forge.R-project.org")  # source

# install devtools from CRAN
install.packages("devtools")
# load devtools
library(devtools)
# install package "babynames" from GitHub
install_github(repo="hadley/babynames")

# install package "PortfolioAnalytics" from source
install.packages("PortfolioAnalytics",
  type="source",
  repos="http://r-forge.r-project.org")
# download files for package "PortfolioAnalytics"
download.packages(pkgs = "PortfolioAnalytics",
  destdir = ".",  # download to cwd
  type = "source",
  repos="http://r-forge.r-project.org")
# install "PortfolioAnalytics" from local tar source
install.packages(
  "C:/Users/Jerzy/Downloads/PortfolioAnalytics_0.9.3598.tar.gz",
  repos=NULL, type="source")

getOption("defaultPackages")
# matrix of installed package information
pack_info <- installed.packages()
dim(pack_info)
# get all installed package names
sort(unname(pack_info[, "Package"]))
# get a few package names and their versions
pack_info[sample(x=1:100, 5), c("Package", "Version")]
# get info for package "xts"
t(pack_info["xts", ])

# list directories in "PortfolioAnalytics" sub-directory
gsub(
  "C:/Users/Jerzy/Documents/R/win-library/3.1",
  "~",
  list.dirs(
    file.path(
      .libPaths()[1],
      "PortfolioAnalytics")))

# load package, produce error if can't be loaded
library(MASS)
# load package, return TRUE if loaded successfully
require(MASS)
# load quietly
library(MASS, quietly=TRUE)
# load without any messages
suppressMessages(library(MASS))
# remove package from search path
detach(MASS)
# install package if it can't be loaded successfully
if (!require("xts")) install.packages("xts")

# calculate VTI volume-weighted average price
v_wap <- TTR::VWAP(
  price=quantmod::Ad(rutils::etf_env$VTI),
  volume=quantmod::Vo(rutils::etf_env$VTI), n=10)

library()  # list all packages installed on the system
search()  # list all loaded packages on search path

# get documentation for package "Ecdat"
packageDescription("Ecdat")  # get short description
help(package="Ecdat")  # load help page
library(Ecdat)  # load package "Ecdat"
data(package="Ecdat")  # list all datasets in "Ecdat"
ls("package:Ecdat")  # list all objects in "Ecdat"
browseVignettes("Ecdat")  # view package vignette
detach("package:Ecdat")  # remove Ecdat from search path

library(Ecdat)  # load econometric data sets
class(Garch)  # Garch is a data frame from "Ecdat"
dim(Garch)  # daily currency prices
head(Garch[, -2])  # col 'dm' is Deutsch Mark
detach("package:Ecdat")  # remove Ecdat from search path

rm(list=ls())
search()  # get search path for R objects
library(MASS)  # load package "MASS"
head(ls("package:MASS"))  # list some objects in "MASS"
detach("package:MASS")  # remove "MASS" from search path

loadedNamespaces()  # get names of loaded namespaces

search()  # get search path for R objects

# get session info,
# including packages not attached to the search path
sessionInfo()

plot.xts  # package xts isn't loaded and attached
head(xts::plot.xts, 3)
methods("cbind")  # get all methods for function "cbind"
stats::cbind.ts  # cbind isn't exported from package stats
stats:::cbind.ts  # view the non-visible function
getAnywhere("cbind.ts")
library(MASS)  # load package 'MASS'
select  # code of primitive function from package 'MASS'

getAnywhere("cbind.ts")
