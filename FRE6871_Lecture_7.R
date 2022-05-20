# ?options  # Get info on global options
getOption("warn")  # Global option for "warn"
options("warn")  # Global option for "warn"
getOption("error")  # Global option for "error"
sqrt_safe <- function(input) {
# Returns its argument
  if (input<0) {
    warning("sqrt_safe: input is negative")
    NULL  # Return NULL for negative argument
  } else {
    sqrt(input)
  }  # end if
}  # end sqrt_safe
sqrt_safe(5)
sqrt_safe(-1)
options(warn=-1)
sqrt_safe(-1)
options(warn=0)
sqrt_safe()
options(warn=1)
sqrt_safe()
options(warn=3)
sqrt_safe()

# Function valido validates its arguments
valido <- function(input=NULL) {
# Check if argument is valid and return double
  if (is.null(input)) {
    return("valido: input is missing")
  } else if (is.numeric(input)) {
    2*input
  } else cat("valido: input not numeric")
}  # end valido
valido(3)
valido("a")
valido()
# valido validates arguments using missing()
valido <- function(input) {
# Check if argument is valid and return double
  if (missing(input)) {
    return("valido: input is missing")
  } else if (is.numeric(input)) {
    2*input
  } else cat("valido: input is not numeric")
}  # end valido
valido(3)
valido("a")
valido()

# valido() validates its arguments and assertions
valido <- function(input) {
# Check if argument is valid and return double
  if (missing(input)) {
    stop("valido: input is missing")
  } else if (!is.numeric(input)) {
    cat("input =", input, "\n")
    stop("valido: input is not numeric")
  } else 2*input
}  # end valido
valido(3)
valido("a")
valido()

# Print the call stack
traceback()

valido <- function(input) {
# Check argument using long form '&&' operator
  stopifnot(!missing(input) && is.numeric(input))
  2*input
}  # end valido
valido(3)
valido()
valido("a")
valido <- function(input) {
# Check argument using logical '&' operator
  stopifnot(!missing(input) & is.numeric(input))
  2*input
}  # end valido
valido()
valido("a")

# sumtwo() returns the sum of its two arguments
sumtwo <- function(input1, input2) {  # Even more robust
# Check if at least one argument is not missing
  stopifnot(!missing(input1) &&
        !missing(input2))
# Check if arguments are valid and return sum
  if (is.numeric(input1) && is.numeric(input2)) {
    input1 + input2  # Both valid
  } else if (is.numeric(input1)) {
    cat("input2 is not numeric\n")
    input1  # input1 is valid
  } else if (is.numeric(input2)) {
    cat("input1 is not numeric\n")
    input2  # input2 is valid
  } else {
    stop("none of the arguments are numeric")
  }
}  # end sumtwo
sumtwo(1, 2)
sumtwo(5, 'a')
sumtwo('a', 5)
sumtwo('a', 'b')
sumtwo()

# Flag "valido" for debugging
debug(valido)
# Calling "valido" starts debugger
valido(3)
# unflag "valido" for debugging
undebug(valido)

valido <- function(input) {
  browser()  # Pause and invoke debugger
# Check argument using long form '&&' operator
  stopifnot(!missing(input) && is.numeric(input))
  2*input
}  # end valido
valido()  # Invokes debugger
options("error")  # Show default NULL "error" option
options(error=recover)  # Set "error" option to "recover"
options(error=NULL)  # Set back to default "error" option

str(tryCatch)  # Get arguments of tryCatch()
tryCatch(  # Without error handler
  {  # Evaluate expressions
    numv <- 101  # Assign
    stop('my error')  # Produce error
  },
  finally=print(paste("numv=", numv))
)  # end tryCatch

tryCatch(  # With error handler
  {  # Evaluate expressions
    numv <- 101  # Assign
    stop('my error')  # Produce error
  },
  # Error handler captures error condition
  error=function(error_cond) {
    print(paste("error handler: ", error_cond))
  },  # end error handler
  # Warning handler captures warning condition
  warning=function(warning_cond) {
    print(paste("warning handler: ", warning_cond))
  },  # end warning handler
  finally=print(paste("numv=", numv))
)  # end tryCatch

# Apply loop without tryCatch
apply(matrix(1:5), 1, function(numv) {  # Anonymous function
    stopifnot(!(numv = 3))  # Check for error
    # Broadcast message to console
    cat("(cat) numv =", numv, "\n")
    # Return a value
    paste("(return) numv =", numv)
  }  # end anonymous function
)  # end apply

# Apply loop with tryCatch
apply(as.matrix(1:5), 1, function(numv) {  # Anonymous function
    tryCatch(  # With error handler
{  # Body
  stopifnot(numv != 3)  # Check for error
  # Broadcast message to console
  cat("(cat) numv =", numv, "\t")
  # Return a value
  paste("(return) numv =", numv)
},
# Error handler captures error condition
error=function(error_cond)
  paste("handler: ", error_cond),
finally=print(paste("(finally) numv =", numv))
    )  # end tryCatch
  }  # end anonymous function
)  # end apply

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
# Subtract POSIXct
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
as.POSIXct("2014-07-14 13:30:10", tz="America/New_York") -
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
Sys.setenv(TZ="UTC")  # Set time-zone to UTC
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
# Set time-zone to New York
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
date_time <- Sys.Date()  # Create date series of class "Date"
dates <- date_time + 0:365  # Daily series over one year
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
prices <- mget(c("VTI", "VXX"), envir=rutils::etfenv)
prices <- lapply(prices, quantmod::Cl)
prices <- rutils::do_call(cbind, prices)
sum(is.na(prices))
# Carry forward and backward non-NA prices
prices <- zoo::na.locf(prices, na.rm=FALSE)
prices <- zoo::na.locf(prices, na.rm=FALSE, fromLast=TRUE)
sum(is.na(prices))
# Remove whole rows containing NA returns
returns <- rutils::etfenv$returns
sum(is.na(returns))
returns <- na.omit(returns)
# Or carry forward non-NA returns (preferred)
returns <- rutils::etfenv$returns
returns[1, is.na(returns[1, ])] <- 0
returns <- zoo::na.locf(returns, na.rm=FALSE)
sum(is.na(returns))

# Replace NAs in xts time series
pricem <- rutils::etfenv$prices[, 1]
head(pricem)
sum(is.na(pricem))
library(quantmod)
pricezoo <- zoo::na.locf(pricem, na.rm=FALSE, fromLast=TRUE)
pricexts <- xts:::na.locf.xts(pricem, fromLast=TRUE)
all.equal(pricezoo, pricexts, check.attributes=FALSE)
library(microbenchmark)
summary(microbenchmark(
  zoo=zoo::na.locf(pricem, fromLast=TRUE),
  xts=xts:::na.locf.xts(pricem, fromLast=TRUE),
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
xtes <- xts(rnorm(NROW(dates)), order.by=dates)
names(xtes) <- "random"
xtes
tail(xtes, 3)  # Get last few elements
first(xtes)  # Get first element
last(xtes)  # Get last element
class(xtes)  # Class "xts"
attributes(xtes)
# Get the time zone of an xts object
indexTZ(xtes)

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
xtes <- xts(zoo::coredata(EuStockMarkets),
      order.by=date_decimal(zoo::index(EuStockMarkets)))
# Plot all columns in single panel: xts v.0.9-8
colors <- rainbow(NCOL(xtes))
plot(xtes, main="EuStockMarkets using xts",
     col=colors, major.ticks="years",
     minor.ticks=FALSE)
legend("topleft", legend=colnames(EuStockMarkets),
 inset=0.2, cex=0.7, , lty=rep(1, NCOL(xtes)),
 lwd=3, col=colors, bg="white")
# Plot only first column: xts v.0.9-7
plot(xtes[, 1], main="EuStockMarkets using xts",
     col=colors[1], major.ticks="years",
     minor.ticks=FALSE)
# Plot remaining columns
for (colnum in 2:NCOL(xtes))
  lines(xtes[, colnum], col=colors[colnum])
# Plot using quantmod
library(quantmod)
plot_theme <- chart_theme()
plot_theme$col$line.col <- colors
chart_Series(x=xtes, theme=plot_theme,
       name="EuStockMarkets using quantmod")
legend("topleft", legend=colnames(EuStockMarkets),
 inset=0.2, cex=0.7, , lty=rep(1, NCOL(xtes)),
 lwd=3, col=colors, bg="white")

library(rutils)
library(ggplot2)
prices <- rutils::etfenv$prices[, 1]
prices <- na.omit(prices)
# Create ggplot object
plotobj <- qplot(x=zoo::index(prices),
          y=as.numeric(prices),
          geom="line",
          main=names(prices)) +
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
prices <- rutils::etfenv$prices[, c("VTI", "IEF")]
prices <- na.omit(prices)
# Create data frame of time series
data_frame <- data.frame(dates=zoo::index(prices),
    zoo::coredata(prices))
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

# Load rutils which contains etfenv dataset
library(rutils)
library(dygraphs)
prices <- rutils::etfenv$prices[, c("VTI", "IEF")]
prices <- na.omit(prices)
# Plot dygraph with date range selector
dygraph(prices, main="VTI and IEF prices") %>%
  dyOptions(colors=c("blue","green")) %>%
  dyRangeSelector()

dyRangeSelector(dyOptions(dygraph(prices, main="VTI and IEF prices"), colors=c("blue","green")))


# Load rutils which contains etfenv dataset
library(rutils)
library(plotly)
prices <- rutils::etfenv$prices[, c("VTI", "IEF")]
prices <- na.omit(prices)
# Create data frame of time series
data_frame <- data.frame(dates=zoo::index(prices),
    zoo::coredata(prices))
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
prices <- rutils::etfenv$prices
pricesub <- prices["2014-10-15/2015-01-10", 1:4]
first(pricesub)
last(pricesub)
# Subset Nov 2014 using a date string
pricesub <- prices["2014-11", 1:4]
first(pricesub)
last(pricesub)
# Subset all data after Nov 2014
pricesub <- prices["2014-11/", 1:4]
first(pricesub)
last(pricesub)
# Comma after date range not necessary
all.equal(prices["2014-11", ], prices["2014-11"])
# .subset_xts() is faster than the bracket []
library(microbenchmark)
summary(microbenchmark(
  bracket=prices[10:20, ],
  subset=xts::.subset_xts(prices, 10:20),
  times=10))[, c(1, 4, 5)]

# Specify string representing a date
datev <- "2014-10-15"
# Subset prices in two different ways
prices <- rutils::etfenv$prices
all.equal(prices[zoo::index(prices) >= datev],
    prices[paste0(datev, "/")])
# Boolean subsetting is slower because coercing string into date
library(microbenchmark)
summary(microbenchmark(
  boolean=(prices[zoo::index(prices) >= datev]),
  date=(prices[paste0(datev, "/")]),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Coerce string into a date
datev <- as.Date("2014-10-15")
# Boolean subsetting is faster than using date string
summary(microbenchmark(
  boolean=(prices[zoo::index(prices) >= datev]),
  date=(prices[paste0(datev, "/")]),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

prices <- HighFreq::SPY["2012-04"]
# Subset recurring time interval using "T notation",
prices <- prices["T10:30:00/T15:00:00"]
first(prices["2012-04-16"])  # First element of day
last(prices["2012-04-16"])  # Last element of day
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

prices <- rutils::etfenv$prices[, c("VTI", "IEF")]
prices <- na.omit(prices)
str(prices)  # Display structure of xts
# Subsetting zoo to single column drops dim attribute
zoo_prices <- as.zoo(prices)
dim(zoo_prices)
dim(zoo_prices[, 1])
# zoo with single column are vectors not matrices
c(is.matrix(zoo_prices), is.matrix(zoo_prices[, 1]))
# xts always have a dim attribute
rbind(base=dim(prices), subs=dim(prices[, 1]))
c(is.matrix(prices), is.matrix(prices[, 1]))

# Lag of zoo shortens it by one row
rbind(base=dim(zoo_prices), lag=dim(lag(zoo_prices)))
# Lag of xts doesn't shorten it
rbind(base=dim(prices), lag=dim(lag(prices)))
# Lag of zoo is in opposite direction from xts
head(lag(zoo_prices, -1), 4)
head(lag(prices), 4)

# library(rutils)  # Load package rutils
# Indices of last observations in each hour
endp <- xts::endpoints(prices, on="hours")
head(endp)
# Extract the last observations in each hour
head(prices[endp, ])

# Lower the periodicity to months
pricesm <- to.period(x=prices, period="months", name="VTI")
# Convert colnames to standard OHLC format
colnames(pricesm)
colnames(pricesm) <- sapply(
  strsplit(colnames(pricesm), split=".", fixed=TRUE),
  function(na_me) na_me[-1]
  )  # end sapply
head(pricesm, 3)
# Lower the periodicity to years
pricesy <- to.period(x=pricesm,
             period="years", name="MSFT")
colnames(pricesy) <- sapply(
  strsplit(colnames(pricesy), split=".", fixed=TRUE),
  function(na_me) na_me[-1]
  )  # end sapply
head(pricesy)

par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
load(file="/Users/jerzy/Develop/lecture_slides/data/datav.RData")
library(xts)  # Load package xts
# as.xts() coerces zoo series into xts series
pricexts <- as.xts(zoo_prices)
# Subset xts using a date
pricexts <- pricexts["2014-11", 1:4]

# Plot OHLC using plot.xts method
xts::plot.xts(pricexts, type="candles", main="")
title(main="MSFT Prices")  # Add title

load(file="/Users/jerzy/Develop/lecture_slides/data/datav.RData")
pricets <- as.ts(pricezoo)
class(pricets)
tail(pricets[, 1:4])
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
bar <- "months"
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
data_frame <- data.frame(type=c("rose", "daisy", "tulip"),
  color=c("red", "white", "yellow"),
  price=c(1.5, 0.5, 1.0),
  row.names=c("flower1", "flower2", "flower3"))  # end data.frame
matrixv <- matrix(sample(1:12), ncol=3,
  dimnames=list(NULL, c("col1", "col2", "col3")))
rownames(matrixv) <- paste("row", 1:NROW(matrixv), sep="")
# Write data frame to text file, and then read it back
write.table(data_frame, file="florist.txt")
data_read <- read.table(file="florist.txt")
data_read  # A data frame

# Write matrix to text file, and then read it back
write.table(matrixv, file="matrix.txt")
mat_read <- read.table(file="matrix.txt")
mat_read  # write.table() coerced matrix to data frame
class(mat_read)
# Coerce from data frame back to matrix
mat_read <- as.matrix(mat_read)
class(mat_read)

setwd("/Users/jerzy/Develop/lecture_slides/data")
data_frame <- data.frame(small=c(3, 5), medium=c(9, 11), large=c(15, 13))
data_frame <- read.table("mydata.txt", header=TRUE)
data_frame <- read.table("clipboard", header=TRUE)

write.table(x=data_frame, file="clipboard", sep="\t")

# Wrapper function for copying data frame from clipboard into R
# by default, data is tab delimited, with a header
read_clip <- function(file="clipboard", sep="\t", header=TRUE, ...) {
  read.table(file=file, sep=sep, header=header, ...)
}  # end read_clip

data_frame <- read_clip()

# Wrapper function for copying data frame from R into clipboard
# by default, data is tab delimited, with a header
write_clip <- function(data, row.names=FALSE, col.names=TRUE, ...) {
  write.table(x=data, file="clipboard", sep="\t",
      row.names=row.names, col.names=col.names, ...)
}  # end write_clip

write_clip(data=data_frame)

# Launch spreadsheet-style data editor
data_frame <- edit(data_frame)

# Write data frame to CSV file, and then read it back
write.csv(data_frame, file="florist.csv")
data_read <- read.csv(file="florist.csv")
data_read  # the row names are read in as extra column
# Restore row names
rownames(data_read) <- data_read[, 1]
data_read <- data_read[, -1]  # Remove extra column
data_read
# Read data frame, with row names from first column
data_read <- read.csv(file="florist.csv", row.names=1)
data_read

# Write data frame to CSV file, without row names
write.csv(data_frame, row.names=FALSE, file="florist.csv")
data_read <- read.csv(file="florist.csv")
data_read  # A data frame without row names

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
mat_read <- read.csv(file="matrix.csv", row.names=1)
mat_read  # Read.csv() reads matrix as data frame
class(mat_read)
mat_read <- as.matrix(mat_read)  # Coerce to matrix
identical(matrixv, mat_read)
write.csv(matrixv, row.names=FALSE,
    file="matrix_ex_rows.csv")
mat_read <- read.csv(file="matrix_ex_rows.csv")
mat_read <- as.matrix(mat_read)
mat_read  # A matrix without row names

setwd("/Users/jerzy/Develop/lecture_slides/data")
library(MASS)  # Load package "MASS"
# Write to CSV file by row - it's very SLOW!!!
MASS::write.matrix(matrixv, file="matrix.csv", sep=",")
# Read using scan() and skip first line with colnames
mat_read <- scan(file="matrix.csv", sep=",", skip=1,
  what=numeric())
# Read colnames
colnamev <- readLines(con="matrix.csv", n=1)
colnamev  # this is a string!
# Convert to char vector
colnamev <- strsplit(colnamev, split=",")[[1]]
mat_read  # mat_read is a vector, not matrix!
# Coerce by row to matrix
mat_read <- matrix(mat_read, ncol=NROW(colnamev), byrow=TRUE)
# Restore colnames
colnames(mat_read) <- colnamev
mat_read
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
# Copy row names
row_names <- row.names(matrixv)
# sapply loop over columns and coerce to numeric
matrixv <- sapply(matrixv, as.numeric)
# Restore row names
row.names(matrixv) <- row_names
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
zoo_series <- zoo(rnorm(NROW(dates)), order.by=dates)
head(zoo_series, 3)
# Write zoo series to text file, and then read it back
write.zoo(zoo_series, file="zoo_series.txt")
zoo_read <- read.zoo("zoo_series.txt")  # Read it back
all.equal(zoo_read, zoo_series)
# Perform the same using write.table() and read.table()
# First coerce zoo_series into data frame
data_frame <- as.data.frame(zoo_series)
data_frame <- cbind(dates, data_frame)
# Write zoo_series to text file using write.table
write.table(data_frame, file="zoo_series.txt",
      row.names=FALSE, col.names=FALSE)
# Read data frame from file
zoo_read <- read.table(file="zoo_series.txt")
sapply(zoo_read, class)  # A data frame
# Coerce data frame into zoo_series
zoo_read <- zoo::zoo(
  drop(as.matrix(zoo_read[, -1])),
  order.by=as.Date(zoo_read[, 1]))
all.equal(zoo_read, zoo_series)

library(zoo)  # Load package zoo
# Write zoo series to CSV file, and then read it back
write.zoo(zoo_series, file="zoo_series.csv",
    sep=",", col.names=TRUE)
zoo_read <- read.zoo(file="zoo_series.csv",
  header=TRUE, sep=",", drop=FALSE)
all.equal(zoo_series, drop(zoo_read))

set.seed(1121)  # Reset random number generator
# Create zoo with POSIXct date-time index
dates <- seq(from=as.POSIXct("2013-06-15"),
        by="hour", length.out=100)
zoo_series <- zoo(rnorm(NROW(dates)), order.by=dates)
head(zoo_series, 3)
# Write zoo series to CSV file, and then read it back
write.zoo(zoo_series, file="zoo_series.csv",
    sep=",", col.names=TRUE)
# Read from CSV file using read.csv.zoo()
zoo_read <- read.csv.zoo(file="zoo_series.csv")
all.equal(zoo_series, zoo_read)
# Coerce to xts series
xtes <- xts::as.xts(zoo_read)
class(xtes); head(xtes, 3)
# Coerce zoo series into data frame with custom date format
data_frame <- as.data.frame(zoo_series)
data_frame <- cbind(format(dates, "%m-%d-%Y %H:%M:%S"),
              data_frame)
head(data_frame, 3)
# Write zoo series to csv file using write.table
write.table(data_frame, file="zoo_series.csv",
      sep=",", row.names=FALSE, col.names=FALSE)
# Read from CSV file using read.csv.zoo()
zoo_read <- read.zoo(file="zoo_series.csv",
  header=FALSE, sep=",", FUN=as.POSIXct,
  format="%m-%d-%Y %H:%M:%S", tz="America/New_York")
# Or using read.csv.zoo()
zoo_read <- read.csv.zoo(file="zoo_series.csv", header=FALSE,
  format="%m-%d-%Y %H:%M:%S", tz="America/New_York")
head(zoo_read, 3)
all.equal(zoo_series, zoo_read)

# Read time series from CSV file, with numeric date-time
zoo_read <- read.table(file="/Users/jerzy/Develop/lecture_slides/data/es_ohlc.csv",
  header=TRUE, sep=",")
# A data frame
class(zoo_read)
sapply(zoo_read, class)
# Coerce data frame into xts series
zoo_read <- xts::xts(as.matrix(zoo_read[, -1]),
  order.by=as.POSIXct.numeric(zoo_read[, 1], tz="America/New_York",
                        origin="1970-01-01"))
# An xts series
class(zoo_read)
head(zoo_read, 3)

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
load_ed <- load(file="my_data.RData")
load_ed  # vector of loaded objects
ls()  # List objects
# Assign new values to objects in  global environment
sapply(load_ed, function(symbol) {
  assign(symbol, runif(1), envir=globalenv())
})  # end sapply
ls()  # List objects
# Assign new values to objects using for loop
for (symbol in load_ed) {
  assign(symbol, runif(1))
}  # end for
ls()  # List objects
# Save vector of objects
save(list=load_ed, file="my_data.RData")
# Remove only loaded objects
rm(list=load_ed)
# Remove the object "load_ed"
rm(load_ed)

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

dev.off()  # turn png output off

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
xtes <- xts::xts(do.call(cbind, listv)[, -1], listv[[1]])
class(xtes); dim(xtes)
# Replace NA values with the most recent non-NA values
sum(is.na(xtes))
xtes <- zoo::na.locf(xtes, na.rm=FALSE)
xtes <- zoo::na.locf(xtes, fromLast=TRUE)

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
sapply(sheets, function(xtes) sum(is.na(xtes)))
sheets <- lapply(sheets, zoo::na.locf, na.rm=FALSE)
sheets <- lapply(sheets, zoo::na.locf, fromLast=TRUE)

#Perform calculations in R,
#And export to CSV files
setwd("/Users/jerzy/Develop/lecture_slides/data")
# Read data frame, with row names from first column
data_read <- read.csv(file="florist.csv", row.names=1)
# Subset data frame
data_read <- data_read[data_read[, "type"]=="daisy", ]
# Write data frame to CSV file, with row names
write.csv(data_read, file="daisies.csv")

#Perform calculations in R,
#And export to CSV files
setwd("/Users/jerzy/Develop/lecture_slides/data")
# Read data frame, with row names from first column
data_read <- read.csv(file="florist.csv", row.names=1)
# Subset data frame
data_read <- data_read[data_read[, "type"]=="daisy", ]
# Write data frame to CSV file, with row names
write.csv(data_read, file="daisies.csv")
