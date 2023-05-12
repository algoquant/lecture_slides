Sys.Date()  # Get today's date
as.Date(1e3)  # Coerce numeric into date object
datetime <- as.Date("2014-07-14")  # "%Y-%m-%d" or "%Y/%m/%d"
datetime
class(datetime)  # Date object
as.Date("07-14-2014", "%m-%d-%Y")  # Specify format
datetime + 20  # Add 20 days
# Extract internal representation to integer
as.numeric(datetime)
date_old <- as.Date("07/14/2013", "%m/%d/%Y")
date_old
# Difference between dates
difftime(datetime, date_old, units="weeks")
weekdays(datetime)  # Get day of the week
# Coerce numeric into date-times
datetime <- 0
attributes(datetime) <- list(class="Date")
datetime  # "Date" object
structure(0, class="Date")  # "Date" object
structure(10000.25, class="Date")
datetime <- Sys.time()  # Get today's date and time
datetime
class(datetime)  # POSIXct object
# POSIXct stored as integer moment of time
as.numeric(datetime)
# Parse character string "%Y-%m-%d %H:%M:%S" to POSIXct object
datetime <- as.POSIXct("2014-07-14 13:30:10")
# Different time zones can have same clock time
as.POSIXct("2014-07-14 13:30:10", tz="America/New_York")
as.POSIXct("2014-07-14 13:30:10", tz="UTC")
# Format argument allows parsing different date-time string formats
as.POSIXct("07/14/2014 13:30:10", format="%m/%d/%Y %H:%M:%S",
     tz="America/New_York")
# Same moment of time corresponds to different clock times
timeny <- as.POSIXct("2014-07-14 13:30:10", tz="America/New_York")
timeldn <- as.POSIXct("2014-07-14 13:30:10", tz="UTC")
# Add five hours to POSIXct
timeny + 5*60*60
# Subtract POSIXct
timeny - timeldn
class(timeny - timeldn)
# Compare POSIXct
timeny > timeldn
# Create vector of POSIXct times during trading hours
timev <- seq(
  from=as.POSIXct("2014-07-14 09:30:00", tz="America/New_York"),
  to=as.POSIXct("2014-07-14 16:00:00", tz="America/New_York"),
  by="10 min")
head(timev, 3)
tail(timev, 3)
# POSIXct is stored as integer moment of time
datetimen <- as.numeric(datetime)
# Same moment of time corresponds to different clock times
as.POSIXct(datetimen, origin="1970-01-01", tz="America/New_York")
as.POSIXct(datetimen, origin="1970-01-01", tz="UTC")
# Same clock time corresponds to different moments of time
as.POSIXct("2014-07-14 13:30:10", tz="America/New_York") -
  as.POSIXct("2014-07-14 13:30:10", tz="UTC")
# Add 20 seconds to POSIXct
datetime + 20
datetime  # POSIXct date and time
# Parse POSIXct to string representing the clock time
format(datetime)
class(format(datetime))  # Character string
# Get clock times in different time zones
format(datetime, tz="America/New_York")
format(datetime, tz="UTC")
# Format with custom format strings
format(datetime, "%m/%Y")
format(datetime, "%m-%d-%Y %H hours")
# Trunc to hour
format(datetime, "%m-%d-%Y %H:00:00")
# Date converted to midnight UTC moment of time
as.POSIXct(Sys.Date())
as.POSIXct(as.numeric(as.POSIXct(Sys.Date())),
     origin="1970-01-01",
     tz="UTC")
# Parse character string "%Y-%m-%d %H:%M:%S" to POSIXlt object
datetime <- as.POSIXlt("2014-07-14 18:30:10")
datetime
class(datetime)  # POSIXlt object
as.POSIXct(datetime)  # Coerce to POSIXct object
# Extract internal list representation to vector
unlist(datetime)
datetime + 20  # Add 20 seconds
class(datetime + 20)  # Implicit coercion to POSIXct
trunc(datetime, units="hours")  # Truncate to closest hour
trunc(datetime, units="days")  # Truncate to closest day
methods(trunc)  # Trunc methods
trunc.POSIXt
Sys.timezone()  # Get time-zone
Sys.setenv(TZ="UTC")  # Set time-zone to UTC
Sys.timezone()  # Get time-zone
# Standard Time in effect
as.POSIXct("2013-03-09 11:00:00", tz="America/New_York")
# Daylight Savings Time in effect
as.POSIXct("2013-03-10 11:00:00", tz="America/New_York")
datetime <- Sys.time()  # Today's date and time
# Convert to character in different TZ
format(datetime, tz="America/New_York")
format(datetime, tz="UTC")
# Parse back to POSIXct
as.POSIXct(format(datetime, tz="America/New_York"))
# Difference between New_York time and UTC
as.POSIXct(format(Sys.time(), tz="UTC")) -
  as.POSIXct(format(Sys.time(), tz="America/New_York"))
# Set time-zone to New York
Sys.setenv(TZ="America/New_York")
library(lubridate)  # Load lubridate
# Parse strings into date-times
as.POSIXct("07-14-2014", format="%m-%d-%Y", tz="America/New_York")
datetime <- lubridate::mdy("07-14-2014", tz="America/New_York")
datetime
class(datetime)  # POSIXct object
lubridate::dmy("14.07.2014", tz="America/New_York")
# Parse numeric into date-times
as.POSIXct(as.character(14072014), format="%d%m%Y",
                  tz="America/New_York")
lubridate::dmy(14072014, tz="America/New_York")
# Parse decimal to date-times
lubridate::decimal_date(datetime)
lubridate::date_decimal(2014.25, tz="America/New_York")
date_decimal(decimal_date(datetime), tz="America/New_York")
library(lubridate)  # Load lubridate
datetime <- lubridate::ymd_hms(20140714142010,
               tz="America/New_York")
datetime
# Get same moment of time in "UTC" time zone
lubridate::with_tz(datetime, "UTC")
as.POSIXct(format(datetime, tz="UTC"), tz="UTC")
# Get same clock time in "UTC" time zone
lubridate::force_tz(datetime, "UTC")
as.POSIXct(format(datetime, tz="America/New_York"),
     tz="UTC")
# Same moment of time
datetime - with_tz(datetime, "UTC")
# Different moments of time
datetime - force_tz(datetime, "UTC")
library(lubridate)  # Load lubridate
# Daylight Savings Time handling periods vs durations
datetime <- as.POSIXct("2013-03-09 11:00:00", tz="America/New_York")
datetime
datetime + lubridate::ddays(1)  # Add duration
datetime + lubridate::days(1)  # Add period
leap_year(2012)  # Leap year
datetime <- lubridate::dmy(01012012, tz="America/New_York")
datetime
datetime + lubridate::dyears(1)  # Add duration
datetime + lubridate::years(1)  # Add period
library(lubridate)  # Load lubridate
datetime <- lubridate::ymd_hms(20140714142010, tz="America/New_York")
datetime
# Add periods to a date-time
c(datetime + lubridate::seconds(1), datetime + lubridate::minutes(1),
  datetime + lubridate::days(1), datetime + lubridate::months(1))
# Create vectors of dates
datetime <- lubridate::ymd(20140714, tz="America/New_York")
datetime + 0:2 * lubridate::months(1)  # Monthly dates
datetime + lubridate::months(0:2)
datetime + 0:2 * lubridate::months(2)  # bi-monthly dates
datetime + seq(0, 5, by=2) * lubridate::months(1)
seq(datetime, length=3, by="2 months")
library(lubridate)  # Load lubridate
# Adding monthly periods can create invalid dates
datetime <- lubridate::ymd(20120131, tz="America/New_York")
datetime + 0:2 * lubridate::months(1)
datetime + lubridate::months(1)
datetime + lubridate::months(2)
# Create vector of end-of-month dates
datetime %m-% lubridate::months(13:1)
library(zoo)  # Load zoo
library(RQuantLib)  # Load RQuantLib
# Create daily date series of class "Date"
dates <- Sys.Date() + -5:2
dates
# Create Boolean vector of business days
# Use RQuantLib calendar
is_busday <- RQuantLib::isBusinessDay(
  calendar="UnitedStates/GovernmentBond", dates)
# Create daily series of business days
bus_index <- dates[is_busday]
bus_index
library(zoo)  # Load package zoo
datetime <- Sys.Date()  # Create date series of class "Date"
dates <- datetime + 0:365  # Daily series over one year
head(dates, 4)  # Print first few dates
format(head(dates, 4), "%m/%d/%Y")  # Print first few dates
# Create daily date-time series of class "POSIXct"
dates <- seq(Sys.time(), by="days", length.out=365)
head(dates, 4)  # Print first few dates
format(head(dates, 4), "%m/%d/%Y %H:%M:%S")  # Print first few dates
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
startd <- decimal_date(Sys.Date()-6)
endd <- decimal_date(Sys.Date())
# Create vector of geometric Brownian motion
datav <- exp(cumsum(rnorm(6)/100))
tstep <- NROW(datav)/(endd-startd)
tseries <- ts(data=datav, start=startd, frequency=tstep)
tseries  # Display time series
# Display index dates
as.Date(date_decimal(zoo::coredata(time(tseries))))
# bi-monthly geometric Brownian motion starting mid-1990
tseries <- ts(data=exp(cumsum(rnorm(96)/100)),
       frequency=6, start=1990.5)
# Show some methods for class "ts"
matrix(methods(class="ts")[3:8], ncol=2)
# "tsp" attribute specifies the date-time index
attributes(tseries)
# Extract the index
tail(time(tseries), 11)
# The index is equally spaced
diff(tail(time(tseries), 11))
# Subset the time series
window(tseries, start=1992, end=1992.25)
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
plot(tseries, type="l",  # Create plot
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
dates <- Sys.Date() + 0:3
zoots <- zoo(rnorm(NROW(dates)), order.by=dates)
zoots
attributes(zoots)
class(zoots)  # Class "zoo"
tail(zoots, 3)  # Get last few elements
library(zoo)  # Load package zoo
zoo::coredata(zoots)  # Extract coredata
zoo::index(zoots)  # Extract time index
start(zoots)  # First date
end(zoots)  # Last date
zoots[start(zoots)]  # First element
zoots[end(zoots)]  # Last element
zoo::coredata(zoots) <- rep(1, 4)  # Replace coredata
cumsum(zoots)  # Cumulative sum
cummax(cumsum(zoots))
cummin(cumsum(zoots))
library(zoo)  # Load package zoo
zoots <- zoo(matrix(cumsum(rnorm(100)), nc=1),
  order.by=seq(from=as.Date("2013-06-15"), by="day", len=100))
colnames(zoots) <- "zoots"
tail(zoots)
dim(zoots)
attributes(zoots)
library(zoo)  # Load package zoo
zoo::coredata(zoots) <- (1:4)^2  # Replace coredata
zoots
lag(zoots)  # One day lag
lag(zoots, 2)  # Two day lag
lag(zoots, k=-1)  # Proper one day lag
diff(zoots)  # Diff with one day lag
# Proper lag and original length
lag(zoots, -2, na.pad=TRUE)
set.seed(1121)  # Reset random number generator
library(zoo)  # Load package zoo
# Create index of daily dates
dates <- seq(from=as.Date("2014-07-14"), by="day", length.out=1000)
# Create vector of geometric Brownian motion
datav <- exp(cumsum(rnorm(NROW(dates))/100))
# Create zoo series of geometric Brownian motion
zoots <- zoo(x=datav, order.by=dates)
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# Plot using plot.zoo method
plot(zoots, xlab="", ylab="")
title(main="Random Prices", line=-1)  # Add title
library(zoo)  # Load package zoo
# Subset zoo as matrix
zoots[459:463, 1]
# Subset zoo using window()
window(zoots,
 start=as.Date("2014-10-15"),
 end=as.Date("2014-10-19"))
# Subset zoo using Date object
zoots[as.Date("2014-10-15")]
set.seed(1121)  # Reset random number generator
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
library(zoo)  # Load package zoo
# Create daily date series of class "Date"
index1 <- seq(Sys.Date(), by="days", length.out=365)
# Create zoo time series of random returns
zoots1 <- zoo(rnorm(NROW(index1)), order.by=index1)
# Create another zoo time series of random returns
index2 <- seq(Sys.Date()+350, by="days", length.out=365)
zoots2 <- zoo(rnorm(NROW(index2)), order.by=index2)
# rbind the two time series - ts1 supersedes ts2
zoots3 <- rbind(zoots1,
  zoots2[zoo::index(zoots2) > end(zoots1)])
# Plot zoo time series of geometric Brownian motion
plot(exp(cumsum(zoots3)/100), xlab="", ylab="")
# Add vertical lines at stitch point
abline(v=end(zoots1), col="blue", lty="dashed")
abline(v=start(zoots2), col="red", lty="dashed")
title(main="Random Prices", line=-1)  # Add title
# Create daily date series of class "Date"
index1 <- Sys.Date() + -3:1
# Create zoo time series of random returns
zoots1 <- zoo(rnorm(NROW(index1)), order.by=index1)
# Create another zoo time series of random returns
index2 <- Sys.Date() + -1:3
zoots2 <- zoo(rnorm(NROW(index2)), order.by=index2)
merge(zoots1, zoots2)  # union of dates
# Intersection of dates
merge(zoots1, zoots2, all=FALSE)
# Create matrix containing NA values
matrixv <- sample(18)
matrixv[sample(NROW(matrixv), 4)] <- NA
matrixv <- matrix(matrixv, nc=3)
# Replace NA values with most recent non-NA values
zoo::na.locf(matrixv)
rutils::na_locf(matrixv)
# Get time series of prices
pricev <- mget(c("VTI", "VXX"), envir=rutils::etfenv)
pricev <- lapply(pricev, quantmod::Cl)
pricev <- rutils::do_call(cbind, pricev)
sum(is.na(pricev))
# Carry forward and backward non-NA prices
pricev <- zoo::na.locf(pricev, na.rm=FALSE)
pricev <- zoo::na.locf(pricev, na.rm=FALSE, fromLast=TRUE)
sum(is.na(pricev))
# Remove whole rows containing NA returns
retp <- rutils::etfenv$returns
sum(is.na(retp))
retp <- na.omit(retp)
# Or carry forward non-NA returns (preferred)
retp <- rutils::etfenv$returns
retp[1, is.na(retp[1, ])] <- 0
retp <- zoo::na.locf(retp, na.rm=FALSE)
sum(is.na(retp))
# Replace NAs in xts time series
pricev <- rutils::etfenv$prices[, 1]
head(pricev)
sum(is.na(pricev))
library(quantmod)
pricezoo <- zoo::na.locf(pricev, na.rm=FALSE, fromLast=TRUE)
pricexts <- xts:::na.locf.xts(pricev, fromLast=TRUE)
all.equal(pricezoo, pricexts, check.attributes=FALSE)
library(microbenchmark)
summary(microbenchmark(
  zoo=zoo::na.locf(pricev, fromLast=TRUE),
  xts=xts:::na.locf.xts(pricev, fromLast=TRUE),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
library(lubridate)  # Load lubridate
library(zoo)  # Load package zoo
# methods(as.zoo)  # Many methods of coercing into zoo
class(EuStockMarkets)  # Multiple ts object
# Coerce mts object into zoo
zoots <- as.zoo(EuStockMarkets)
class(zoo::index(zoots))  # Index is numeric
head(zoots, 3)
# Approximately convert index into class "Date"
zoo::index(zoots) <-
  as.Date(365*(zoo::index(zoots)-1970))
head(zoots, 3)
# Convert index into class "POSIXct"
zoots <- as.zoo(EuStockMarkets)
zoo::index(zoots) <- date_decimal(zoo::index(zoots))
head(zoots, 3)
library(lubridate)  # Load lubridate
library(zoo)  # Load package zoo
set.seed(1121)  # Reset random number generator
# Create index of daily dates
dates <- seq(from=as.Date("2014-07-14"), by="day", length.out=1000)
# Create vector of geometric Brownian motion
datav <- exp(cumsum(rnorm(NROW(dates))/100))
# Create zoo time series of geometric Brownian motion
zoots <- zoo(x=datav, order.by=dates)
head(zoots, 3)  # zoo object
# as.ts() creates ts object with frequency=1
tseries <- as.ts(zoots)
tsp(tseries)  # Frequency=1
# Get start and end dates of zoots
startd <- decimal_date(start(zoots))
endd <- decimal_date(end(zoots))
# Calculate frequency of zoots
tstep <- NROW(zoots)/(endd-startd)
datav <- zoo::coredata(zoots)  # Extract data from zoots
# Create ts object using ts()
tseries <- ts(data=datav, start=startd, frequency=tstep)
# Display start of time series
window(tseries, start=start(tseries),
 end=start(tseries)+4/365)
head(time(tseries))  # Display index dates
head(as.Date(date_decimal(zoo::coredata(time(tseries)))))
library(lubridate)  # Load lubridate
library(zoo)  # Load package zoo
# Create weekday Boolean vector
weekdayv <- weekdays(zoo::index(zoots))
is_weekday <- !((weekdayv == "Saturday") |
  (weekdayv == "Sunday"))
# Remove weekends from zoo time series
zoots <- zoots[is_weekday, ]
head(zoots, 7)  # zoo object
# as.ts() creates NA values
tseries <- as.ts(zoots)
head(tseries, 7)
# Create vector of regular dates, including weekends
dates <- seq(from=start(zoots),
            by="day",
            length.out=NROW(zoots))
zoo::index(zoots) <- dates
tseries <- as.ts(zoots)
head(tseries, 7)
set.seed(1121)  # Reset random number generator
library(xts)  # Load package xts
# Create xts time series of random returns
dates <- Sys.Date() + 0:3
xtsv <- xts(rnorm(NROW(dates)), order.by=dates)
names(xtsv) <- "random"
xtsv
tail(xtsv, 3)  # Get last few elements
first(xtsv)  # Get first element
last(xtsv)  # Get last element
class(xtsv)  # Class "xts"
attributes(xtsv)
# Get the time zone of an xts object
indexTZ(xtsv)
load(file="/Users/jerzy/Develop/lecture_slides/data/datav.RData")
library(xts)  # Load package xts
# as.xts() coerces zoo series into xts series
pricexts <- as.xts(pricezoo)
dim(pricexts)
head(pricexts[, 1:4], 4)
# Plot using plot.xts method
xts::plot.xts(pricexts[, "Close"], xlab="", ylab="", main="")
title(main="MSFT Prices")  # Add title
library(xts)  # Load xts
library(lubridate)  # Load lubridate
# Coerce EuStockMarkets into class xts
xtsv <- xts(zoo::coredata(EuStockMarkets),
      order.by=date_decimal(zoo::index(EuStockMarkets)))
# Plot all columns in single panel: xts v.0.9-8
colorv <- rainbow(NCOL(xtsv))
plot(xtsv, main="EuStockMarkets using xts",
     col=colorv, major.ticks="years",
     minor.ticks=FALSE)
legend("topleft", legend=colnames(EuStockMarkets),
 inset=0.2, cex=0.7, , lty=rep(1, NCOL(xtsv)),
 lwd=3, col=colorv, bg="white")
# Plot only first column: xts v.0.9-7
plot(xtsv[, 1], main="EuStockMarkets using xts",
     col=colorv[1], major.ticks="years",
     minor.ticks=FALSE)
# Plot remaining columns
for (colnum in 2:NCOL(xtsv))
  lines(xtsv[, colnum], col=colorv[colnum])
# Plot using quantmod
library(quantmod)
plot_theme <- chart_theme()
plot_theme$col$line.col <- colors
chart_Series(x=xtsv, theme=plot_theme,
       name="EuStockMarkets using quantmod")
legend("topleft", legend=colnames(EuStockMarkets),
 inset=0.2, cex=0.7, , lty=rep(1, NCOL(xtsv)),
 lwd=3, col=colorv, bg="white")
library(rutils)
library(ggplot2)
pricev <- rutils::etfenv$prices[, 1]
pricev <- na.omit(pricev)
# Create ggplot object
plotobj <- qplot(x=zoo::index(pricev),
          y=as.numeric(pricev),
          geom="line",
          main=names(pricev)) +
  xlab("") + ylab("") +
  theme(  # Add legend and title
    legend.position=c(0.1, 0.5),
    plot.title=element_text(vjust=-2.0),
    plot.background=element_blank()
  )  # end theme
# Render ggplot object
plotobj
library(rutils)  # Load xts time series data
library(reshape2)
library(ggplot2)
pricev <- rutils::etfenv$prices[, c("VTI", "IEF")]
pricev <- na.omit(pricev)
# Create data frame of time series
dframe <- data.frame(dates=zoo::index(pricev),
    zoo::coredata(pricev))
# reshape data into a single column
dframe <-
  reshape2::melt(dframe, id="dates")
x11(width=6, height=5)  # Open plot window
# ggplot the melted dframe
ggplot(data=dframe,
 mapping=aes(x=dates, y=value, colour=variable)) +
 geom_line() +
  xlab("") + ylab("") +
  ggtitle("VTI and IEF") +
  theme(  # Add legend and title
    legend.position=c(0.2, 0.8),
    plot.title=element_text(vjust=-2.0)
  )  # end theme
# Load rutils which contains etfenv dataset
library(rutils)
library(dygraphs)
pricev <- rutils::etfenv$prices[, c("VTI", "IEF")]
pricev <- log(na.omit(pricev))
# Plot dygraph with date range selector
dygraph(pricev, main="VTI and IEF prices") %>%
  dyOptions(colors=c("blue","green")) %>%
  dyRangeSelector()
# Load rutils which contains etfenv dataset
library(rutils)
library(plotly)
pricev <- rutils::etfenv$prices[, c("VTI", "IEF")]
pricev <- na.omit(pricev)
# Create data frame of time series
dframe <- data.frame(dates=zoo::index(pricev),
    zoo::coredata(pricev))
# Plotly syntax using pipes
dframe %>%
  plot_ly(x=~dates, y=~VTI, type="scatter", mode="lines", name="VTI") %>%
  add_trace(x=~dates, y=~IEF, type="scatter", mode="lines", name="IEF") %>%
  layout(title="VTI and IEF prices",
   xaxis=list(title="Time"),
   yaxis=list(title="Stock Prices"),
   legend=list(x=0.1, y=0.9))
# Or use standard plotly syntax
plotobj <- plot_ly(data=dframe, x=~dates, y=~VTI, type="scatter", mode="lines", name="VTI")
plotobj <- add_trace(p=plotobj, x=~dates, y=~IEF, type="scatter", mode="lines", name="IEF")
plotobj <- layout(p=plotobj, title="VTI and IEF prices", xaxis=list(title="Time"), yaxis=list(title="Stock Prices"), legend=list(x=0.1, y=0.9))
plotobj
# Subset xts using a date range string
pricev <- rutils::etfenv$prices
pricesub <- pricev["2014-10-15/2015-01-10", 1:4]
first(pricesub)
last(pricesub)
# Subset Nov 2014 using a date string
pricesub <- pricev["2014-11", 1:4]
first(pricesub)
last(pricesub)
# Subset all data after Nov 2014
pricesub <- pricev["2014-11/", 1:4]
first(pricesub)
last(pricesub)
# Comma after date range not necessary
all.equal(pricev["2014-11", ], pricev["2014-11"])
# .subset_xts() is faster than the bracket []
library(microbenchmark)
summary(microbenchmark(
  bracket=pricev[10:20, ],
  subset=xts::.subset_xts(pricev, 10:20),
  times=10))[, c(1, 4, 5)]
# Specify string representing a date
datev <- "2014-10-15"
# Subset prices in two different ways
pricev <- rutils::etfenv$prices
all.equal(pricev[zoo::index(pricev) >= datev],
    pricev[paste0(datev, "/")])
# Boolean subsetting is slower because coercing string into date
library(microbenchmark)
summary(microbenchmark(
  boolean=(pricev[zoo::index(pricev) >= datev]),
  date=(pricev[paste0(datev, "/")]),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Coerce string into a date
datev <- as.Date("2014-10-15")
# Boolean subsetting is faster than using date string
summary(microbenchmark(
  boolean=(pricev[zoo::index(pricev) >= datev]),
  date=(pricev[paste0(datev, "/")]),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
pricev <- HighFreq::SPY["2012-04"]
# Subset recurring time interval using "T notation",
pricev <- pricev["T10:30:00/T15:00:00"]
first(pricev["2012-04-16"])  # First element of day
last(pricev["2012-04-16"])  # Last element of day
# Suppress timezone warning messages
options(xts_check_tz=FALSE)
# Create time series with overlapping time indices
vti1 <- rutils::etfenv$VTI["/2015"]
vti2 <- rutils::etfenv$VTI["2014/"]
dates1 <- zoo::index(vti1)
dates2 <- zoo::index(vti2)
# Join by rows
vti <- rbind(vti1, vti2)
dates <- zoo::index(vti)
sum(duplicated(dates))
vti <- vti[!duplicated(dates), ]
all.equal(vti, rutils::etfenv$VTI)
# Alternative method - slightly slower
vti <- rbind(vti1, vti2[!(zoo::index(vti2) %in% zoo::index(vti1))])
all.equal(vti, rutils::etfenv$VTI)
# Remove duplicates starting from the end
vti <- rbind(vti1, vti2)
vti <- vti[!duplicated(dates), ]
vtifl <- vti[!duplicated(dates, fromLast=TRUE), ]
all.equal(vti, vtifl)
pricev <- rutils::etfenv$prices[, c("VTI", "IEF")]
pricev <- na.omit(pricev)
str(pricev)  # Display structure of xts
# Subsetting zoo to single column drops dim attribute
pricezoo <- as.zoo(pricev)
dim(pricezoo)
dim(pricezoo[, 1])
# zoo with single column are vectors not matrices
c(is.matrix(pricezoo), is.matrix(pricezoo[, 1]))
# xts always have a dim attribute
rbind(base=dim(pricev), subs=dim(pricev[, 1]))
c(is.matrix(pricev), is.matrix(pricev[, 1]))
# Lag of zoo shortens it by one row
rbind(base=dim(pricezoo), lag=dim(lag(pricezoo)))
# Lag of xts doesn't shorten it
rbind(base=dim(pricev), lag=dim(lag(pricev)))
# Lag of zoo is in opposite direction from xts
head(lag(pricezoo, -1), 4)
head(lag(pricev), 4)
# library(rutils)  # Load package rutils
# Indices of last observations in each hour
endd <- xts::endpoints(pricev, on="hours")
head(endd)
# Extract the last observations in each hour
head(pricev[endd, ])
# Lower the periodicity to months
pricem <- to.period(x=pricev, period="weeks", name="MSFT")
# Convert colnames to standard OHLC format
colnames(pricem)
colnames(pricem) <- sapply(
  strsplit(colnames(pricem), split=".", fixed=TRUE),
  function(na_me) na_me[-1]
  )  # end sapply
head(pricem, 3)
# Lower the periodicity to years
pricesy <- to.period(x=pricem, period="years", name="MSFT")
colnames(pricevy) <- sapply(
  strsplit(colnames(pricevy), split=".", fixed=TRUE),
  function(na_me) na_me[-1]
  )  # end sapply
head(pricevy)
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
load(file="/Users/jerzy/Develop/lecture_slides/data/datav.RData")
library(xts)  # Load package xts
# as.xts() coerces zoo series into xts series
pricexts <- as.xts(pricezoo)
# Subset xts using a date
pricexts <- pricexts["2014-11", 1:4]
# Plot OHLC using plot.xts method
xts::plot.xts(pricexts, type="candles", main="")
title(main="MSFT Prices")  # Add title
load(file="/Users/jerzy/Develop/lecture_slides/data/datav.RData")
pricev <- as.ts(pricezoo)
class(pricev)
tail(pricev[, 1:4])
library(xts)
pricexts <- as.xts(pricezoo)
class(pricexts)
tail(pricexts[, 1:4])
cat("Enter\ttab")  # Cat() interretsp backslash escape sequences
print("Enter\ttab")
my_text <- print("hello")
my_text  # Print() returns its argument
# Create string
my_text <- "Title: My Text\nSome numbers: 1,2,3,...\nRprofile files contain code executed at R startup,\n"
cat(my_text, file="mytext.txt")  # Write to text file
cat("Title: My Text",  # Write several lines to text file
    "Some numbers: 1,2,3,...",
    "Rprofile files contain code executed at R startup,",
    file="mytext.txt", sep="\n")
save(my_text, file="mytext.RData")  # Write to binary file
print(pi)
print(pi, digits=10)
getOption("digits")
foo <- 12
bar <- "weeks"
sprintf("There are %i %s in the year", foo, bar)
# Read text from file
scan(file="mytext.txt", what=character(), sep="\n")
# Read lines from file
readLines(con="mytext.txt")
# Read text from console
input <- readline("Enter a number: ")
class(input)
# Coerce to numeric
input <- as.numeric(input)
# Read text from file and display in editor:
# file.show("mytext.txt")
# file.show("mytext.txt", pager="")
setwd("/Users/jerzy/Develop/lecture_slides/data")
dframe <- data.frame(type=c("rose", "daisy", "tulip"),
  color=c("red", "white", "yellow"),
  price=c(1.5, 0.5, 1.0),
  row.names=c("flower1", "flower2", "flower3"))  # end data.frame
matrixv <- matrix(sample(1:12), ncol=3,
  dimnames=list(NULL, c("col1", "col2", "col3")))
rownames(matrixv) <- paste("row", 1:NROW(matrixv), sep="")
# Write data frame to text file, and then read it back
write.table(dframe, file="florist.txt")
readf <- read.table(file="florist.txt")
readf  # A data frame
# Write matrix to text file, and then read it back
write.table(matrixv, file="matrix.txt")
readmat <- read.table(file="matrix.txt")
readmat  # write.table() coerced matrix to data frame
class(readmat)
# Coerce from data frame back to matrix
readmat <- as.matrix(readmat)
class(readmat)
# Create a data frame
dframe <- data.frame(small=c(3, 5), medium=c(9, 11), large=c(15, 13))
# Launch spreadsheet-style data editor
dframe <- edit(dframe)
# Copy the data frame to clipboard
write.table(x=dframe, file="clipboard", sep="\t")
# Wrapper function for copying data frame from R into clipboard
# by default, data is tab delimited, with a header
write_clip <- function(data, row.names=FALSE, col.names=TRUE, ...) {
  write.table(x=data, file="clipboard", sep="\t",
      row.names=row.names, col.names=col.names, ...)
}  # end write_clip
write_clip(data=dframe)
# Wrapper function for copying data frame from clipboard into R
# by default, data is tab delimited, with a header
read_clip <- function(file="clipboard", sep="\t", header=TRUE, ...) {
  read.table(file=file, sep=sep, header=header, ...)
}  # end read_clip
dframe <- read.table("clipboard", header=TRUE)
dframe <- read_clip()
# Write data frame to CSV file, and then read it back
write.csv(dframe, file="florist.csv")
readf <- read.csv(file="florist.csv")
readf  # the row names are read in as extra column
# Restore row names
rownames(readf) <- readf[, 1]
readf <- readf[, -1]  # Remove extra column
readf
# Read data frame, with row names from first column
readf <- read.csv(file="florist.csv", row.names=1)
readf
# Write data frame to CSV file, without row names
write.csv(dframe, row.names=FALSE, file="florist.csv")
readf <- read.csv(file="florist.csv")
readf  # A data frame without row names
# Open a read connection to a file
con_read = file("/Users/jerzy/Develop/lecture_slides/data/etf_prices_crsp.csv", "r")
# Read the first 10 rows
data10 <- read.csv(con_read, nrows=10)
# Read another 10 rows
data20 <- read.csv(con_read, nrows=10, header=FALSE)
colnames(data20) <- colnames(data10)
# Close the connection to the file
close(con_read)
# Open a read connection to a file
con_read = file("/Users/jerzy/Develop/lecture_slides/data/etf_prices_crsp.csv", "r")
# Read the first 1000 rows
data10 <- read.csv(con_read, nrows=1e3)
colnamev <- colnames(data10)
# Write to a file
countv <- 1
write.csv(data10, paste0("/Users/jerzy/Develop/data/temp/etf_prices_", countv, ".csv"))
# Read remaining rows in a loop 10 rows at a time
# Can produce error without getting to end of file
while (isOpen(con_read)) {
  datav <- read.csv(con_read, nrows=1e3)
  colnames(datav) <- colnamev
  write.csv(datav, paste0("/Users/jerzy/Develop/data/temp/etf_prices_", countv, ".csv"))
  countv <- countv + 1
}  # end while
# Write matrix to csv file, and then read it back
write.csv(matrixv, file="matrix.csv")
readmat <- read.csv(file="matrix.csv", row.names=1)
readmat  # Read.csv() reads matrix as data frame
class(readmat)
readmat <- as.matrix(readmat)  # Coerce to matrix
identical(matrixv, readmat)
write.csv(matrixv, row.names=FALSE,
    file="matrix_ex_rows.csv")
readmat <- read.csv(file="matrix_ex_rows.csv")
readmat <- as.matrix(readmat)
readmat  # A matrix without row names
setwd("/Users/jerzy/Develop/lecture_slides/data")
library(MASS)  # Load package "MASS"
# Write to CSV file by row - it's very SLOW!!!
MASS::write.matrix(matrixv, file="matrix.csv", sep=",")
# Read using scan() and skip first line with colnames
readmat <- scan(file="matrix.csv", sep=",", skip=1,
  what=numeric())
# Read colnames
colnamev <- readLines(con="matrix.csv", n=1)
colnamev  # this is a string!
# Convert to char vector
colnamev <- strsplit(colnamev, split=",")[[1]]
readmat  # readmat is a vector, not matrix!
# Coerce by row to matrix
readmat <- matrix(readmat, ncol=NROW(colnamev), byrow=TRUE)
# Restore colnames
colnames(readmat) <- colnamev
readmat
# Scan() is a little faster than read.csv()
library(microbenchmark)
summary(microbenchmark(
  read_csv=read.csv("matrix.csv"),
  scan=scan(file="matrix.csv", sep=",",
    skip=1, what=numeric()),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Read data from a csv file, including row names
matrixv <- read.csv(file="matrix_bad.csv", row.names=1)
matrixv
class(matrixv)
# Columns with bad data are character or factor
sapply(matrixv, class)
# Coerce character column to numeric
matrixv$col2 <- as.numeric(matrixv$col2)
# Or
# Copy row names
rownames <- row.names(matrixv)
# sapply loop over columns and coerce to numeric
matrixv <- sapply(matrixv, as.numeric)
# Restore row names
row.names(matrixv) <- rownames
# Replace NAs with zero
matrixv[is.na(matrixv)] <- 0
# matrix without NAs
matrixv
setwd("/Users/jerzy/Develop/lecture_slides/data")
rm(list=ls())
set.seed(1121)  # Reset random number generator
library(zoo)  # Load package zoo
# Create zoo with Date index
dates <- seq(from=as.Date("2013-06-15"), by="day",
        length.out=100)
pricev <- zoo(rnorm(NROW(dates)), order.by=dates)
head(pricev, 3)
# Write zoo series to text file, and then read it back
write.zoo(pricev, file="pricev.txt")
pricezoo <- read.zoo("pricev.txt")  # Read it back
all.equal(pricezoo, pricev)
# Perform the same using write.table() and read.table()
# First coerce pricev into data frame
dframe <- as.data.frame(pricev)
dframe <- cbind(dates, dframe)
# Write pricev to text file using write.table
write.table(dframe, file="pricev.txt",
      row.names=FALSE, col.names=FALSE)
# Read data frame from file
pricezoo <- read.table(file="pricev.txt")
sapply(pricezoo, class)  # A data frame
# Coerce data frame into pricev
pricezoo <- zoo::zoo(
  drop(as.matrix(pricezoo[, -1])),
  order.by=as.Date(pricezoo[, 1]))
all.equal(pricezoo, pricev)
library(zoo)  # Load package zoo
# Write zoo series to CSV file, and then read it back
write.zoo(pricev, file="pricev.csv",
    sep=",", col.names=TRUE)
pricezoo <- read.zoo(file="pricev.csv",
  header=TRUE, sep=",", drop=FALSE)
all.equal(pricev, drop(pricezoo))
set.seed(1121)  # Reset random number generator
# Create zoo with POSIXct date-time index
dates <- seq(from=as.POSIXct("2013-06-15"),
        by="hour", length.out=100)
pricev <- zoo(rnorm(NROW(dates)), order.by=dates)
head(pricev, 3)
# Write zoo series to CSV file, and then read it back
write.zoo(pricev, file="pricev.csv",
    sep=",", col.names=TRUE)
# Read from CSV file using read.csv.zoo()
pricezoo <- read.csv.zoo(file="pricev.csv")
all.equal(pricev, pricezoo)
# Coerce to xts series
xtsv <- xts::as.xts(pricezoo)
class(xtsv); head(xtsv, 3)
# Coerce zoo series into data frame with custom date format
dframe <- as.data.frame(pricev)
dframe <- cbind(format(dates, "%m-%d-%Y %H:%M:%S"), dframe)
head(dframe, 3)
# Write zoo series to csv file using write.table
write.table(dframe, file="pricev.csv",
      sep=",", row.names=FALSE, col.names=FALSE)
# Read from CSV file using read.csv.zoo()
pricezoo <- read.zoo(file="pricev.csv",
  header=FALSE, sep=",", FUN=as.POSIXct,
  format="%m-%d-%Y %H:%M:%S", tz="America/New_York")
# Or using read.csv.zoo()
pricezoo <- read.csv.zoo(file="pricev.csv", header=FALSE,
  format="%m-%d-%Y %H:%M:%S", tz="America/New_York")
head(pricezoo, 3)
all.equal(pricev, pricezoo)
# Read time series from CSV file, with numeric date-time
datazoo <- read.table(file="/Users/jerzy/Develop/lecture_slides/data/es_ohlc.csv",
  header=TRUE, sep=",")
# A data frame
class(datazoo)
sapply(datazoo, class)
# Coerce data frame into xts series
datazoo <- xts::xts(as.matrix(datazoo[, -1]),
  order.by=as.POSIXct.numeric(datazoo[, 1], tz="America/New_York",
                        origin="1970-01-01"))
# An xts series
class(datazoo)
head(datazoo, 3)
rm(list=ls())  # Remove all objects
var1 <- 1; var2 <- 2
ls()  # List all objects
ls()[1]  # List first object
args(save)  # List arguments of save function
# Save "var1" to a binary file using string argument
save("var1", file="my_data.RData")
# Save "var1" to a binary file using object name
save(var1, file="my_data.RData")
# Save multiple objects
save(var1, var2, file="my_data.RData")
# Save first object in list by passing to "..." argument
# ls()[1] is not evaluated
save(ls()[1], file="my_data.RData")
# Save first object in list by passing to "list" argument
save(list=ls()[1], file="my_data.RData")
# Save whole list by passing it to the "list" argument
save(list=ls(), file="my_data.RData")
rm(list=ls())  # Remove all objects
# Load objects from file
loadobj <- load(file="my_data.RData")
loadobj  # vector of loaded objects
ls()  # List objects
# Assign new values to objects in  global environment
sapply(loadobj, function(symbol) {
  assign(symbol, runif(1), envir=globalenv())
})  # end sapply
ls()  # List objects
# Assign new values to objects using for loop
for (symbol in loadobj) {
  assign(symbol, runif(1))
}  # end for
ls()  # List objects
# Save vector of objects
save(list=loadobj, file="my_data.RData")
# Remove only loaded objects
rm(list=loadobj)
# Remove the object "loadobj"
rm(loadobj)
sink("sinkdata.txt")# Redirect text output to file
cat("Redirect text output from R\n")
print(runif(10))
cat("\nEnd data\nbye\n")
sink()  # turn redirect off
pdf("Rgraph.pdf", width=7, height=4)  # Redirect graphics to pdf file
cat("Redirect data from R into pdf file\n")
myvar <- seq(-2*pi, 2*pi, len=100)
plot(x=myvar, y=sin(myvar), main="Sine wave",
   xlab="", ylab="", type="l", lwd=2, col="red")
cat("\nEnd data\nbye\n")
dev.off()  # turn pdf output off
png("r_plot.png")  # Redirect graphics output to png file
cat("Redirect graphics from R into png file\n")
plot(x=myvar, y=sin(myvar), main="Sine wave",
 xlab="", ylab="", type="l", lwd=2, col="red")
cat("\nEnd data\nbye\n")
# Install package data.table
install.packages("data.table")
# Load package data.table
library(data.table)
# Get documentation for package data.table
# Get short description
packageDescription("data.table")
# Load help page
help(package="data.table")
# List all datasets in "data.table"
data(package="data.table")
# List all objects in "data.table"
ls("package:data.table")
# Remove data.table from search path
detach("package:data.table")
# Create a data table
library(data.table)
dtable <- data.table::data.table(
  col1=sample(7), col2=sample(7), col3=sample(7))
# Print dtable
class(dtable); dtable
# Column referenced without quotes
dtable[, col2]
# Row referenced without a following comma
dtable[2]
# Print option "datatable.print.nrows"
getOption("datatable.print.nrows")
options(datatable.print.nrows=10)
getOption("datatable.print.nrows")
# Number of rows in dtable
NROW(dtable)
# Or
dtable[, NROW(col1)]
# Or
dtable[, .N]
# microbenchmark speed of data.table syntax
library(microbenchmark)
summary(microbenchmark(
  dt=dtable[, .N],
  rcode=NROW(dtable),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Read a data table from CSV file
dir_name <- "/Users/jerzy/Develop/lecture_slides/data/"
file_name <- file.path(dir_name, "weather_delays14.csv")
dtable <- data.table::fread(file_name)
class(dtable); dim(dtable)
dtable
# fread() reads the same data as read.csv()
all.equal(read.csv(file_name),
    setDF(data.table::fread(file_name)))
# fread() is much faster than read.csv()
library(microbenchmark)
summary(microbenchmark(
  rcode=read.csv(file_name),
  fread=setDF(data.table::fread(file_name)),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Write data table to file in different ways
data.table::fwrite(dtable, file="dtable.csv")
write.csv(dtable, file="dtable2.csv")
cat(unlist(dtable), file="dtable3.csv")
# microbenchmark speed of data.table::fwrite()
summary(microbenchmark(
  fwrite=data.table::fwrite(dtable, file="dtable.csv"),
  write_csv=write.csv(dtable, file="dtable2.csv"),
  cat=cat(unlist(dtable), file="dtable3.csv"),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Select first five rows of dtable
dtable[1:5]
# Select rows with JFK flights
jfk_flights <- dtable[origin=="JFK"]
# Select rows JFK flights in June
jfk_flights <- dtable[origin=="JFK" & month==6]
# Select rows without JFK flights
jfk_flights <- dtable[!(origin=="JFK")]
# Select flights with carrier_delay
dtable[carrier_delay > 0]
# Select column of dtable and return a vector
head(dtable[, origin])
# Select column of dtable and return a dtable, not vector
head(dtable[, list(origin)])
head(dtable[, .(origin)])
# Select two columns of dtable
dtable[, list(origin, month)]
dtable[, .(origin, month)]
columnv <- c("origin", "month")
dtable[, ..columnv]
dtable[, month, origin]
# Select two columns and rename them
dtable[, .(orig=origin, mon=month)]
# Select all columns except origin
head(dtable[, !"origin"])
head(dtable[, -"origin"])
# Select flights with positive carrier_delay
dtable[carrier_delay > 0]
# Number of flights with carrier_delay
dtable[, sum(carrier_delay > 0)]
# Or standard R commands
sum(dtable[, carrier_delay > 0])
# microbenchmark speed of data.table syntax
summary(microbenchmark(
  dt=dtable[, sum(carrier_delay > 0)],
  rcode=sum(dtable[, carrier_delay > 0]),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Average carrier_delay
dtable[, mean(carrier_delay)]
# Average carrier_delay and aircraft_delay
dtable[, .(carrier=mean(carrier_delay),
     aircraft=mean(aircraft_delay))]
# Average aircraft_delay from JFK
dtable[origin=="JFK", mean(aircraft_delay)]
# Number of flights from JFK
dtable[origin=="JFK", NROW(aircraft_delay)]
# Or
dtable[origin=="JFK", .N]
# In R
sum(dtable[, origin]=="JFK")
# Number of flights from each airport
dtable[, .N, by=origin]
# Same, but add names to output
dtable[, .(flights=.N), by=.(airport=origin)]
# Number of AA flights from each airport
dtable[carrier=="AA", .(flights=.N), by=.(airport=origin)]
# Number of flights from each airport and airline
dtable[, .(flights=.N), by=.(airport=origin, airline=carrier)]
# Average aircraft_delay
dtable[, mean(aircraft_delay)]
# Average aircraft_delay from JFK
dtable[origin=="JFK", mean(aircraft_delay)]
# Average aircraft_delay from each airport
dtable[, .(delay=mean(aircraft_delay)), by=.(airport=origin)]
# Average and max delays from each airport and month
dtable[, .(mean_delay=mean(aircraft_delay), max_delay=max(aircraft_delay)),
     by=.(airport=origin, month=month)]
# Average and max delays from each airport and month
dtable[, .(mean_delay=mean(aircraft_delay), max_delay=max(aircraft_delay)),
     keyby=.(airport=origin, month=month)]
# Sort ascending by origin, then descending by dest
dtables <- dtable[order(origin, -dest)]
dtables
# Doesn't work outside dtable
order(origin, -dest)
# Sort dtable by reference
setorder(dtable, origin, -dest)
all.equal(dtable, dtables)
# setorder() is much faster than order()
summary(microbenchmark(
  order=dtable[order(origin, -dest)],
  setorder=setorder(dtable, origin, -dest),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Average aircraft_delay by month
dtables[, .(mean_delay=mean(aircraft_delay)),
      by=.(month=month)]
# Chained brackets to sort output by month
dtables[, .(mean_delay=mean(aircraft_delay)),
  by=.(month=month)][order(month)]
# Select weather_delay and aircraft_delay in two different ways
dtable[1:7, .SD,
     .SDcols=c("weather_delay", "aircraft_delay")]
dtable[1:7, .(weather_delay, aircraft_delay)]
# Calculate mean of weather_delay and aircraft_delay
dtable[, sapply(.SD, mean),
     .SDcols=c("weather_delay", "aircraft_delay")]
sapply(dtable[, .SD,
     .SDcols=c("weather_delay", "aircraft_delay")], mean)
# Return origin and dest, then all other columns
dtable[1:7, .SD, by=.(origin, dest)]
# Return origin and dest, then weather_delay and aircraft_delay columns
dtable[1:7, .SD, by=.(origin, dest),
     .SDcols=c("weather_delay", "aircraft_delay")]
# Return first two rows from each month
dtable[, head(.SD, 2), by=.(month)]
dtable[, head(.SD, 2), by=.(month),
     .SDcols=c("weather_delay", "aircraft_delay")]
# Calculate mean of weather_delay and aircraft_delay, grouped by origin
dtable[, lapply(.SD, mean),
     by=.(origin),
     .SDcols=c("weather_delay", "aircraft_delay")]
# Or simply
dtable[, .(weather_delay=mean(weather_delay),
         aircraft_delay=mean(aircraft_delay)),
     by=.(origin)]
# Add tot_delay column
dtable[, tot_delay := (carrier_delay + aircraft_delay)]
head(dtable, 4)
# Delete tot_delay column
dtable[, tot_delay := NULL]
# Add max_delay column grouped by origin and dest
dtable[, max_delay := max(aircraft_delay), by=.(origin, dest)]
dtable[, max_delay := NULL]
# Add date and tot_delay columns
dtable[, c("date", "tot_delay") :=
       list(paste(month, day, year, sep="/"),
            (carrier_delay + aircraft_delay))]
# Modify select rows of tot_delay column
dtable[month == 12, tot_delay := carrier_delay]
dtable[, c("date", "tot_delay") := NULL]
# Add several columns
dtable[, c("max_carrier", "max_aircraft") := lapply(.SD, max),
 by=.(origin, dest),
 .SDcols=c("carrier_delay", "aircraft_delay")]
# Remove columns
dtable[, c("max_carrier", "max_aircraft") := NULL]
# Modifying by reference is much faster than standard R
summary(microbenchmark(
  dt=dtable[, tot_delay := (carrier_delay + aircraft_delay)],
  rcode=(dtable[, "tot_delay"] <- dtable[, "carrier_delay"] + dtable[, "aircraft_delay"]),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Add a key based on the "origin" column
setkey(dtable, origin)
haskey(dtable)
key(dtable)
# Select rows with LGA using the key
dtable["LGA"]
all.equal(dtable["LGA"], dtable[origin == "LGA"])
# Select rows with LGA and JFK using the key
dtable[c("LGA", "JFK")]
# Add a key based on the "origin" and "dest" columns
setkey(dtable, origin, dest)
key(dtable)
# Select rows with origin from JFK and MIA
dtable[c("JFK", "MIA")]
# Select rows with origin from JFK and dest to MIA
dtable[.("JFK", "MIA")]
all.equal(dtable[.("JFK", "MIA")],
    dtable[origin == "JFK" & dest == "MIA"])
# Selecting rows using a key is much faster than standard R
summary(microbenchmark(
  with_key=dtable[.("JFK", "MIA")],
  standard_r=dtable[origin == "JFK" & dest == "MIA"],
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Create data frame and coerce it to data table
dtable <- data.frame(col1=sample(7), col2=sample(7), col3=sample(7))
class(dtable); dtable
data.table::setDT(dtable)
class(dtable); dtable
# Coerce dtable into data frame
data.table::setDF(dtable)
class(dtable); dtable
# Or
dtable <- data.table:::as.data.frame.data.table(dtable)
# SetDF() is much faster than as.data.frame()
summary(microbenchmark(
  asdataframe=data.table:::as.data.frame.data.table(dtable),
  setDF=data.table::setDF(dtable),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Coerce xts to a data frame
pricev <- rutils::etfenv$VTI
class(pricev); head(pricev)
pricev <- as.data.frame(pricev)
class(pricev); head(pricev)
# Coerce data frame to a data table
data.table::setDT(pricev, keep.rownames=TRUE)
class(pricev); head(pricev)
# Dates are coerced to strings
sapply(pricev, class)
# Coerce xts directly to a data table
dtable <- as.data.table(rutils::etfenv$VTI,
  keep.rownames=TRUE)
class(dtable); head(dtable)
# Dates are not coerced to strings
sapply(dtable, class)
all.equal(pricev, dtable, check.attributes=FALSE)
# Install package fst
install.packages("fst")
# Load package fst
library(fst)
# Get documentation for package fst
# Get short description
packageDescription("fst")
# Load help page
help(package="fst")
# List all datasets in "fst"
data(package="fst")
# List all objects in "fst"
ls("package:fst")
# Remove fst from search path
detach("package:fst")
# Read a data frame from CSV file
dir_name <- "/Users/jerzy/Develop/lecture_slides/data/"
file_name <- file.path(dir_name, "weather_delays14.csv")
data.table::setDF(dframe)
class(dframe); dim(dframe)
# Write data frame to .fst file in different ways
fst::write_fst(dframe, path="dframe.fst")
write.csv(dframe, file="dframe2.csv")
# microbenchmark speed of fst::write_fst()
library(microbenchmark)
summary(microbenchmark(
  fst=fst::write_fst(dframe, path="dframe.csv"),
  write_csv=write.csv(dframe, file="dframe2.csv"),
  cat=cat(unlist(dframe), file="dframe3.csv"),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# fst::read_fst() reads the same data as read.csv()
all.equal(read.csv(file_name),
    fst::read_fst("dframe.fst"))
# fst::read_fst() is 10 times faster than read.csv()
summary(microbenchmark(
  fst=fst::read_fst("dframe.fst"),
  read_csv=read.csv(file_name),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Coerce TAQ xts to a data frame
library(HighFreq)
taq <- HighFreq::SPY_TAQ
taq <- as.data.frame(taq)
class(taq)
# Coerce data frame to a data table
data.table::setDT(taq, keep.rownames=TRUE)
class(taq); head(taq)
# Get memory size of data table
format(object.size(taq), units="MB")
# Save data table to .fst file
fst::write_fst(taq, path="/Users/jerzy/Develop/data/taq.fst")
# Create reference to .fst file similar to a data frame
refst <- fst::fst("/Users/jerzy/Develop/data/taq.fst")
class(refst)
# Memory size of reference to .fst is very small
format(object.size(refst), units="MB")
# Get sizes of all objects in workspace
sort(sapply(mget(ls()), object.size))
# Reference to .fst can be treated similar to a data table
dim(taq); dim(refst)
fst:::print.fst_table(refst)
# Subset reference to .fst just like a data table
refst[1e4:(1e4+5), ]
# Install and load package readxl
install.packages("readxl")
library(readxl)
dir_name <- "/Users/jerzy/Develop/lecture_slides/data"
filev <- file.path(dir_name, "multi_tabs.xlsx")
# Read a time series from first sheet of xlsx file
tibblev <- readxl::read_xlsx(filev)
class(tibblev)
# Coerce POSIXct dates into Date class
class(tibblev$Dates)
tibblev$Dates <- as.Date(tibblev$Dates)
# Some columns are character strings
sapply(tibblev, class)
sapply(tibblev, is.character)
# Coerce columns with strings to numeric
listv <- lapply(tibblev, function(x) {
  if (is.character(x))
    as.numeric(x)
  else
    x
})  # end lapply
# Coerce list into xts time series
xtsv <- xts::xts(do.call(cbind, listv)[, -1], listv[[1]])
class(xtsv); dim(xtsv)
# Replace NA values with the most recent non-NA values
sum(is.na(xtsv))
xtsv <- zoo::na.locf(xtsv, na.rm=FALSE)
xtsv <- zoo::na.locf(xtsv, fromLast=TRUE)
# Read names of all the sheets in an Excel spreadsheet
namesv <- readxl::excel_sheets(filev)
# Read all the sheets from an Excel spreadsheet
sheets <- lapply(namesv, read_xlsx, path=filev)
names(sheets) <- namesv
# sheets is a list of tibbles
sapply(sheets, class)
# Create function to coerce tibble to xts
to_xts <- function(tibblev) {
  tibblev$Dates <- as.Date(tibblev$Dates)
  # Coerce columns with strings to numeric
  listv <- lapply(tibblev, function(x) {
    if (is.character(x))
      as.numeric(x)
    else
      x
  })  # end lapply
  # Coerce list into xts series
  xts::xts(do.call(cbind, listv)[, -1], listv$Dates)
}  # end to_xts
# Coerce list of tibbles to list of xts
class(sheets)
sheets <- lapply(sheets, to_xts)
sapply(sheets, class)
# Replace NA values with the most recent non-NA values
sapply(sheets, function(xtsv) sum(is.na(xtsv)))
sheets <- lapply(sheets, zoo::na.locf, na.rm=FALSE)
sheets <- lapply(sheets, zoo::na.locf, fromLast=TRUE)
#Perform calculations in R,
#And export to CSV files
setwd("/Users/jerzy/Develop/lecture_slides/data")
# Read data frame, with row names from first column
readf <- read.csv(file="florist.csv", row.names=1)
# Subset data frame
readf <- readf[readf[, "type"]=="daisy", ]
# Write data frame to CSV file, with row names
write.csv(readf, file="daisies.csv")
#Perform calculations in R,
#And export to CSV files
setwd("/Users/jerzy/Develop/lecture_slides/data")
# Read data frame, with row names from first column
readf <- read.csv(file="florist.csv", row.names=1)
# Subset data frame
readf <- readf[readf[, "type"]=="daisy", ]
# Write data frame to CSV file, with row names
write.csv(readf, file="daisies.csv")
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
      to="/Users/jerzy/Develop/lecture_slides/data/google_sheet.csv")
# Open sheet in internet browser
gs_browse(google_sheet)
# Verify that Rtools or XCode are working properly:
devtools::find_rtools()  # Under Windows
devtools::has_devel()
# Install the packages Rcpp and RcppArmadillo
install.packages(c("Rcpp", "RcppArmadillo"))
# Load package Rcpp
library(Rcpp)
# Get documentation for package Rcpp
# Get short description
packageDescription("Rcpp")
# Load help page
help(package="Rcpp")
# List all datasets in "Rcpp"
data(package="Rcpp")
# List all objects in "Rcpp"
ls("package:Rcpp")
# Remove Rcpp from search path
detach("package:Rcpp")
# Define Rcpp function
Rcpp::cppFunction("
  int times_two(int x)
    { return 2 * x;}
  ")  # end cppFunction
# Run Rcpp function
times_two(3)
# Source Rcpp functions from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/mult_rcpp.cpp")
# Multiply two numbers
mult_rcpp(2, 3)
mult_rcpp(1:3, 6:4)
# Multiply two vectors
mult_vec_rcpp(2, 3)
mult_vec_rcpp(1:3, 6:4)
# Define Rcpp function with loop
Rcpp::cppFunction("
double inner_mult(NumericVector x, NumericVector y) {
int xsize = x.size();
int ysize = y.size();
if (xsize != ysize) {
    return 0;
  } else {
    double total = 0;
    for(int i = 0; i < xsize; ++i) {
total += x[i] * y[i];
  }
  return total;
  }
}")  # end cppFunction
# Run Rcpp function
inner_mult(1:3, 6:4)
inner_mult(1:3, 6:3)
# Define Rcpp Sugar function with loop
Rcpp::cppFunction("
double inner_sugar(NumericVector x, NumericVector y) {
  return sum(x * y);
}")  # end cppFunction
# Run Rcpp Sugar function
inner_sugar(1:3, 6:4)
inner_sugar(1:3, 6:3)
# Define R function with loop
inner_multr <- function(x, y) {
    sumv <- 0
    for(i in 1:NROW(x)) {
sumv <- sumv + x[i] * y[i]
    }
    sumv
}  # end inner_multr
# Run R function
inner_multr(1:3, 6:4)
inner_multr(1:3, 6:3)
# Compare speed of Rcpp and R
library(microbenchmark)
summary(microbenchmark(
  rcode=inner_multr(1:10000, 1:10000),
  innerp=1:10000 %*% 1:10000,
  Rcpp=inner_mult(1:10000, 1:10000),
  sugar=inner_sugar(1:10000, 1:10000),
  times=10))[, c(1, 4, 5)]
# Define Ornstein-Uhlenbeck function in R
sim_our <- function(nrows=1000, eq_price=5.0,
              volat=0.01, theta=0.01) {
  retp <- numeric(nrows)
  pricev <- numeric(nrows)
  pricev[1] <- eq_price
  for (i in 2:nrows) {
    retp[i] <- theta*(eq_price - pricev[i-1]) + volat*rnorm(1)
    pricev[i] <- pricev[i-1] + retp[i]
  }  # end for
  pricev
}  # end sim_our
# Simulate Ornstein-Uhlenbeck process in R
eq_price <- 5.0; sigmav <- 0.01
thetav <- 0.01; nrows <- 1000
set.seed(1121)  # Reset random numbers
ousim <- sim_our(nrows, eq_price=eq_price, volat=sigmav, theta=thetav)
# Define Ornstein-Uhlenbeck function in Rcpp
Rcpp::cppFunction("
NumericVector sim_oucpp(double eq_price,
                    double volat,
                    double thetav,
                    NumericVector innov) {
  int nrows = innov.size();
  NumericVector pricev(nrows);
  NumericVector returns(nrows);
  pricev[0] = eq_price;
  for (int it = 1; it < nrows; it++) {
    returns[it] = thetav*(eq_price - pricev[it-1]) + volat*innov[it-1];
    pricev[it] = pricev[it-1] + returns[it];
  }  // end for
  return pricev;
}")  # end cppFunction
# Simulate Ornstein-Uhlenbeck process in Rcpp
set.seed(1121)  # Reset random numbers
oucpp <- sim_oucpp(eq_price=eq_price,
  volat=sigmav, theta=thetav, innov=rnorm(nrows))
all.equal(ousim, oucpp)
# Compare speed of Rcpp and R
library(microbenchmark)
summary(microbenchmark(
  rcode=sim_our(nrows, eq_price=eq_price, volat=sigmav, theta=thetav),
  Rcpp=sim_oucpp(eq_price=eq_price, volat=sigmav, theta=thetav, innov=rnorm(nrows)),
  times=10))[, c(1, 4, 5)]
# Source Rcpp function for Ornstein-Uhlenbeck process from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/sim_ou.cpp")
# Simulate Ornstein-Uhlenbeck process in Rcpp
set.seed(1121)  # Reset random numbers
oucpp <- sim_oucpp(eq_price=eq_price,
  volat=sigmav,
  theta=thetav,
  innov=rnorm(nrows))
all.equal(ousim, oucpp)
# Compare speed of Rcpp and R
library(microbenchmark)
summary(microbenchmark(
  rcode=sim_our(nrows, eq_price=eq_price, volat=sigmav, theta=thetav),
  Rcpp=sim_oucpp(eq_price=eq_price, volat=sigmav, theta=thetav, innov=rnorm(nrows)),
  times=10))[, c(1, 4, 5)]
# Calculate uniformly distributed pseudo-random sequence
unifun <- function(seedv, nrows=10) {
  output <- numeric(nrows)
  output[1] <- seedv
  for (i in 2:nrows) {
    output[i] <- 4*output[i-1]*(1-output[i-1])
  }  # end for
  acos(1-2*output)/pi
}  # end unifun
# Source Rcpp functions from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/unifun.cpp")
# Microbenchmark Rcpp code
library(microbenchmark)
summary(microbenchmark(
  rcode=runif(1e5),
  rloop=unifun(0.3, 1e5),
  Rcpp=unifuncpp(0.3, 1e5),
  times=10))[, c(1, 4, 5)]
library(RcppArmadillo)
# Source Rcpp functions from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/armadillo_functions.cpp")
vec1 <- runif(1e5)
vec2 <- runif(1e5)
inner_vec(vec1, vec2)
vec1 %*% vec2
# Microbenchmark \emph{RcppArmadillo} code
summary(microbenchmark(
  rcpp = inner_vec(vec1, vec2),
  rcode = (vec1 %*% vec2),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
# Microbenchmark shows:
# inner_vec() is several times faster than %*%, especially for longer vectors.
#     expr     mean   median
# 1 inner_vec 110.7067 110.4530
# 2 rcode 585.5127 591.3575
# Source Rcpp functions from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/sim_arima.cpp")
# Define AR(2) coefficients
coeff <- c(0.9, 0.09)
nrows <- 1e4
set.seed(1121)
innov <- rnorm(nrows)
# Simulate ARIMA using filter()
arimar <- filter(x=innov, filter=coeff, method="recursive")
# Simulate ARIMA using sim_ar()
innov <- matrix(innov)
coeff <- matrix(coeff)
arimav <- sim_ar(coeff, innov)
all.equal(drop(arimav), as.numeric(arimar))
# Microbenchmark \emph{RcppArmadillo} code
summary(microbenchmark(
  rcpp = sim_ar(coeff, innov),
  filter = filter(x=innov, filter=coeff, method="recursive"),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
library(RcppArmadillo)
# Source Rcpp functions from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/armadillo_functions.cpp")
matrixv <- matrix(runif(1e5), nc=1e3)
# De-mean matrix columns using apply()
matd <- apply(matrixv, 2, function(x) (x-mean(x)))
# De-mean matrix columns in place using Rcpp demeanr()
demeanr(matrixv)
all.equal(matd, matrixv)
# Microbenchmark \emph{RcppArmadillo} code
library(microbenchmark)
summary(microbenchmark(
  rcode = (apply(matrixv, 2, mean)),
  rcpp = demeanr(matrixv),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
# Perform matrix inversion
# Create random positive semi-definite matrix
matrixv <- matrix(runif(25), nc=5)
matrixv <- t(matrixv) %*% matrixv
# Invert the matrix
matrixinv <- solve(matrixv)
inv_mat(matrixv)
all.equal(matrixinv, matrixv)
# Microbenchmark \emph{RcppArmadillo} code
summary(microbenchmark(
  rcode = solve(matrixv),
  rcpp = inv_mat(matrixv),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
library(RcppArmadillo)
# Source Rcpp functions from file
Rcpp::sourceCpp("/Users/jerzy/Develop/lecture_slides/scripts/HighFreq.cpp")
# Calculate matrix of random returns
matrixv <- matrix(rnorm(300), nc=5)
# Regularized inverse of correlation matrix
dimax <- 4
cormat <- cor(matrixv)
eigend <- eigen(cormat)
invmat <- eigend$vectors[, 1:dimax] %*%
  (t(eigend$vectors[, 1:dimax]) / eigend$values[1:dimax])
# Regularized inverse using \emph{RcppArmadillo}
invarma <- calc_inv(cormat, dimax=dimax)
all.equal(invmat, invarma)
# Microbenchmark \emph{RcppArmadillo} code
library(microbenchmark)
summary(microbenchmark(
  rcode = {eigend <- eigen(cormat)
eigend$vectors[, 1:dimax] %*% (t(eigend$vectors[, 1:dimax]) / eigend$values[1:dimax])},
  rcpp = calc_inv(cormat, dimax=dimax),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
# Install package reticulate
install.packages("reticulate")
# Start Python session
reticulate::repl_python()
# Exit Python session
exit
