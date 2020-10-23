library(knitr)
opts_chunk$set(prompt=TRUE, eval=FALSE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='tiny', fig.width=4, fig.height=4)
options(width=80, dev='pdf')
options(digits=3)
thm <- knit_theme$get("acid")
knit_theme$set(thm)

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
date_time <- lubridate::mdy("07-14-2014", tz="America/New_York")
date_time
class(date_time)  # POSIXct object
lubridate::dmy("14.07.2014", tz="America/New_York")

# Parse numeric into date-times
as.POSIXct(as.character(14072014), format="%d%m%Y",
                  tz="America/New_York")
lubridate::dmy(14072014, tz="America/New_York")

# Parse decimal to date-times
lubridate::decimal_date(date_time)
lubridate::date_decimal(2014.25, tz="America/New_York")
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
as.Date(date_decimal(zoo::coredata(time(ts_series))))
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
zoo::coredata(zoo_series)  # Extract coredata
zoo::index(zoo_series)  # Extract time index
start(zoo_series)  # First date
end(zoo_series)  # Last date
zoo_series[start(zoo_series)]  # First element
zoo_series[end(zoo_series)]  # Last element
zoo::coredata(zoo_series) <- rep(1, 4)  # Replace coredata
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
zoo::coredata(zoo_series) <- (1:4)^2  # Replace coredata
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
           zoo_series2[zoo::index(zoo_series2) > end(zoo_series1)])
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
class(zoo::index(zoo_series))  # Index is numeric
head(zoo_series, 3)
# Approximately convert index into class "Date"
zoo::index(zoo_series) <-
  as.Date(365*(zoo::index(zoo_series)-1970))
head(zoo_series, 3)
# Convert index into class "POSIXct"
zoo_series <- as.zoo(EuStockMarkets)
zoo::index(zoo_series) <- date_decimal(zoo::index(zoo_series))
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
da_ta <- zoo::coredata(zoo_series)  # Extract data from zoo_series
# Create ts object using ts()
ts_series <- ts(data=da_ta, start=start_date,
          frequency=fre_quency)
# Display start of time series
window(ts_series, start=start(ts_series),
 end=start(ts_series)+4/365)
head(time(ts_series))  # Display index dates
head(as.Date(date_decimal(zoo::coredata(time(ts_series)))))

library(lubridate)  # Load lubridate
library(zoo)  # Load package zoo
# Create weekday Boolean vector
week_days <- weekdays(zoo::index(zoo_series))
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
zoo::index(zoo_series) <- in_dex
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
x_ts <- xts(zoo::coredata(EuStockMarkets),
      order.by=date_decimal(zoo::index(EuStockMarkets)))
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
etf_gg <- qplot(x=zoo::index(price_s),
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
data_frame <- data.frame(dates=zoo::index(price_s),
    zoo::coredata(price_s))
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
data_frame <- data.frame(dates=zoo::index(price_s),
    zoo::coredata(price_s))
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
all.equal(price_s[zoo::index(price_s) >= dat_e],
    price_s[paste0(dat_e, "/")])
# Boolean subsetting is slower because coercing string into date
library(microbenchmark)
summary(microbenchmark(
  boolean=(price_s[zoo::index(price_s) >= dat_e]),
  date=(price_s[paste0(dat_e, "/")]),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Coerce string into a date
dat_e <- as.Date("2014-10-15")
# Boolean subsetting is faster than using date string
summary(microbenchmark(
  boolean=(price_s[zoo::index(price_s) >= dat_e]),
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
