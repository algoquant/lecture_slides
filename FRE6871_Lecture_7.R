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
pack_info <- installed.packages()  # matrix of packages
# get a few package names and their versions
pack_info[sample(x=1:100, 5), c("Package", "Version")]
t(pack_info["xts", ])  # get info for package "xts"
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
  price=quantmod::Ad(rutils::env_etf$VTI),
  volume=quantmod::Vo(rutils::env_etf$VTI), n=10)
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
script_dir <- "C:/Develop/R/scripts"
# execute script file and print the commands
source(file.path(script_dir, "script.R"),
 echo=TRUE)

####################################
#script.R file contains R script to demonstrate sourcing from script files

# print information about this process
print(paste0("print: This test script was run at: ", format(Sys.time())))
cat("cat: This test script was run at:", format(Sys.time()), "\n")

# display first 6 rows of cars data frame
head(cars)

# define a function
fun_c <- function(x) x+1

# read a line from console
readline("Press Return to continue")

# plot sine function in x11 window
x11()
curve(expr=sin, type="l", xlim=c(-2*pi, 2*pi),
xlab="", ylab="", lwd=2, col="orange",
main="Sine function")
# get help about running R scripts and batch processes
?BATCH
?Rscript
#script_args.R contains R script that accepts arguments
# print information about this process
cat("cat: This script was run at:", format(Sys.time()), "\n")
# read arguments supplied on the command line
arg_s <- commandArgs(TRUE)
# print the arguments
cat(paste0("arguments supplied on command line: ", paste(arg_s, collapse=", "), "\n"))
# return sum of arguments
sum(as.numeric(arg_s))
#plot_to_file.R
#R script to demonstrate plotting to file

# redirect graphics output to png file
plot_dir <- "C:/Develop/data"
png(file.path(plot_dir, "r_plot.png"))

# plot sine function
curve(expr=sin, type="l", xlim=c(-2*pi, 2*pi),
xlab="", ylab="", lwd=2, col="orange",
main="Sine function")

# turn png output off
dev.off()
#plot_interactive.R
#R script to demonstrate interactive plotting

# plot sine function in x11 window
x11()
curve(expr=sin, type="l", xlim=c(-2*pi, 2*pi),
xlab="", ylab="", lwd=2, col="orange",
main="Sine function")

# wait until x11 window is closed
while (!is.null(dev.list())) Sys.sleep(1)
#perform calculations in R,
#and export to CSV files
setwd("C:/Develop/R/lecture_slides/data")
# read data frame, with row names from first column
data_read <- read.csv(file="florist.csv",
              row.names=1)
# subset data frame
data_read <-
  data_read[data_read[, "type"]=="daisy", ]
# write data frame to CSV file, with row names
write.csv(data_read, file="daisies.csv")
#perform calculations in R,
#and export to CSV files
setwd("C:/Develop/R/lecture_slides/data")
# read data frame, with row names from first column
data_read <- read.csv(file="florist.csv",
              row.names=1)
# subset data frame
data_read <-
  data_read[data_read[, "type"]=="daisy", ]
# write data frame to CSV file, with row names
write.csv(data_read, file="daisies.csv")
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
Sys.setenv(TZ="UTC")  # set time-zone to UTC
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
# difference between New_York time and UTC
as.POSIXct(format(Sys.time(), tz="UTC")) -
  as.POSIXct(format(Sys.time(), tz="America/New_York"))
# set time-zone to New York
Sys.setenv(TZ="America/New_York")
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

# create Boolean vector of business days
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
Sys.setenv(TZ="UTC")
as.POSIXct(head(qrtly_index, 4))
library(lubridate)  # load lubridate
set.seed(1121)  # reset random number generator
# create daily time series ending today
start_date <- decimal_date(Sys.Date()-6)
end_date <- decimal_date(Sys.Date())
# create vector of geometric Brownian motion
da_ta <- exp(cumsum(rnorm(6)/100))
fre_quency <- NROW(da_ta)/(end_date-start_date)
ts_series <- ts(data=da_ta,
          start=start_date,
          frequency=fre_quency)
ts_series  # display time series
# display index dates
as.Date(date_decimal(coredata(time(ts_series))))
# bi-monthly geometric Brownian motion starting mid-1990
ts_series <- ts(data=exp(cumsum(rnorm(96)/100)),
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
class(EuStockMarkets)  # multiple ts object
dim(EuStockMarkets)
head(EuStockMarkets, 3)  # get first three rows
# EuStockMarkets index is equally spaced
diff(tail(time(EuStockMarkets), 11))
par(mar=c(1, 2, 1, 1), oma=c(0, 0, 0, 0))
# plot all the columns in separate panels
plot(EuStockMarkets, main="EuStockMarkets", xlab="")
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# plot in single panel
plot(EuStockMarkets, main="EuStockMarkets",
     xlab="", ylab="", plot.type="single",
     col=c("black", "red", "blue", "green"))
# add legend
legend(x=1992, y=8000,
 legend=colnames(EuStockMarkets),
 col=c("black", "red", "blue", "green"),
 lwd=6, lty=1)
# calculate DAX percentage returns
dax_rets <- diff(log(EuStockMarkets[, 1]))
# mean and standard deviation of returns
c(mean(dax_rets), sd(dax_rets))
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# plot histogram
hist(dax_rets, breaks=30, main="",
     xlim=c(-0.04, 0.04), ylim=c(0, 60),
     xlab="", ylab="", freq=FALSE)
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
# calculate percentage returns
dax_rets <- diff(log(EuStockMarkets[, 1]))
# perform Shapiro-Wilk test
shapiro.test(dax_rets)
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# create normal Q-Q plot
qqnorm(dax_rets, ylim=c(-0.04, 0.04),
 xlab='Normal Quantiles', main='')
# fit a line to the normal quantiles
qqline(dax_rets, col='red', lwd=2)
plot_title <- paste(colnames(EuStockMarkets)[1],
            'Q-Q Plot')
title(main=plot_title, line=-1)  # add title
# boxplot method for formula
boxplot(formula=mpg ~ cyl, data=mtcars,
  main="Mileage by number of cylinders",
  xlab="Cylinders", ylab="Miles per gallon")
# boxplot method for data frame of EuStockMarkets percentage returns
boxplot(x=diff(log(EuStockMarkets)))
set.seed(1121)  # reset random number generator
library(zoo)  # load package zoo
# create zoo time series of random returns
in_dex <- Sys.Date() + 0:3
zoo_series <- zoo(rnorm(NROW(in_dex)),
         order.by=in_dex)
zoo_series
attributes(zoo_series)
class(zoo_series)  # class 'zoo'
tail(zoo_series, 3)  # get last few elements
library(zoo)  # load package zoo
coredata(zoo_series)  # extract coredata
index(zoo_series)  # extract time index
start(zoo_series)  # first date
end(zoo_series)  # last date
zoo_series[start(zoo_series)]  # first element
zoo_series[end(zoo_series)]  # last element
coredata(zoo_series) <- rep(1, 4)  # replace coredata
cumsum(zoo_series)  # cumulative sum
cummax(cumsum(zoo_series))
cummin(cumsum(zoo_series))
library(zoo)  # load package zoo
zoo_series <- 
  zoo(as.matrix(cumsum(rnorm(100)), nc=1), 
order.by=seq(from=as.Date("2013-06-15"), 
             by="day", length.out=100))
colnames(zoo_series) <- "zoo_series"
tail(zoo_series)
dim(zoo_series)
attributes(zoo_series)
library(zoo)  # load package zoo
coredata(zoo_series) <- (1:4)^2  # replace coredata
zoo_series
lag(zoo_series)  # one day lag
lag(zoo_series, 2)  # two day lag
lag(zoo_series, k=-1)  # proper one day lag
diff(zoo_series)  # diff with one day lag
# proper lag and original length
lag(zoo_series, -2, na.pad=TRUE)
set.seed(1121)  # reset random number generator
library(zoo)  # load package zoo
# create index of daily dates
in_dex <- seq(from=as.Date("2014-07-14"),
            by="day", length.out=1000)
# create vector of geometric Brownian motion
zoo_data <-
  exp(cumsum(rnorm(NROW(in_dex))/100))
# create zoo series of geometric Brownian motion
zoo_series <- zoo(x=zoo_data, order.by=in_dex)
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
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
set.seed(1121)  # reset random number generator
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
library(zoo)  # load package zoo
# create daily date series of class 'Date'
in_dex1 <- seq(Sys.Date(), by="days",
             length.out=365)
# create zoo time series of random returns
zoo_series1 <- zoo(rnorm(NROW(in_dex1)),
           order.by=in_dex1)
# create another zoo time series of random returns
in_dex2 <- seq(Sys.Date()+350, by="days",
             length.out=365)
zoo_series2 <- zoo(rnorm(NROW(in_dex2)),
           order.by=in_dex2)
# rbind the two time series - ts1 supersedes ts2
zoo_series3 <- rbind(zoo_series1,
           zoo_series2[index(zoo_series2) > end(zoo_series1)])
# plot zoo time series of geometric Brownian motion
plot(exp(cumsum(zoo_series3)/100), xlab="", ylab="")
# add vertical lines at stitch point
abline(v=end(zoo_series1), col="blue", lty="dashed")
abline(v=start(zoo_series2), col="red", lty="dashed")
title(main="Random Prices", line=-1)  # add title
# create daily date series of class 'Date'
in_dex1 <- Sys.Date() + -3:1
# create zoo time series of random returns
zoo_series1 <- zoo(rnorm(NROW(in_dex1)),
         order.by=in_dex1)
# create another zoo time series of random returns
in_dex2 <- Sys.Date() + -1:3
zoo_series2 <- zoo(rnorm(NROW(in_dex2)),
         order.by=in_dex2)
merge(zoo_series1, zoo_series2)  # union of dates
# intersection of dates
merge(zoo_series1, zoo_series2, all=FALSE)
# create matrix containing NA values
mat_rix <- sample(18)
mat_rix[sample(NROW(mat_rix), 4)] <- NA
mat_rix <- matrix(mat_rix, nc=3)
# replace NA values with most recent non-NA values
zoo::na.locf(mat_rix)
rutils::na_locf(mat_rix)
# get time series of prices
price_s <- mget(c("VTI", "VXX"), envir=rutils::etf_env)
price_s <- lapply(price_s, quantmod::Ad)
price_s <- rutils::do_call(cbind, price_s)
sum(is.na(price_s))
# carry forward and backward non-NA prices
price_s <- zoo::na.locf(price_s)
price_s <- zoo::na.locf(price_s, fromLast=TRUE)
sum(is.na(price_s))
# remove whole rows containing NA returns
re_turns <- rutils::etf_env$re_turns
sum(is.na(re_turns))
re_turns <- na.omit(re_turns)
# or carry forward non-NA returns (preferred)
re_turns <- rutils::etf_env$re_turns
re_turns[1, is.na(re_turns[1, ])] <- 0
re_turns <- zoo::na.locf(re_turns)
sum(is.na(re_turns))
# Replace NAs in xts time series
se_ries <- rutils::etf_env$price_s[, 1]
head(se_ries)
sum(is.na(se_ries))
library(quantmod)
series_zoo <- as.xts(zoo::na.locf(se_ries, fromLast=TRUE))
series_xts <- xts:::na.locf.xts(se_ries, fromLast=TRUE)
all.equal(series_zoo, series_xts, check.attributes=FALSE)
library(microbenchmark)
summary(microbenchmark(
  zoo=as.xts(zoo::na.locf(se_ries, fromLast=TRUE)),
  xts=xts:::na.locf.xts(se_ries, fromLast=TRUE),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
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
set.seed(1121)  # reset random number generator
# create index of daily dates
in_dex <- seq(from=as.Date("2014-07-14"),
            by="day", length.out=1000)
# create vector of geometric Brownian motion
zoo_data <- exp(cumsum(rnorm(NROW(in_dex))/100))
# create zoo time series of geometric Brownian motion
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
fre_quency <- NROW(zoo_series)/(end_date-start_date)
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
# create weekday Boolean vector
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
            length.out=NROW(zoo_series))
index(zoo_series) <- in_dex
ts_series <- as.ts(zoo_series)
head(ts_series, 7)
set.seed(1121)  # reset random number generator
library(xts)  # load package xts
# create xts time series of random returns
in_dex <- Sys.Date() + 0:3
x_ts <- xts(rnorm(NROW(in_dex)),
         order.by=in_dex)
names(x_ts) <- "random"
x_ts
tail(x_ts, 3)  # get last few elements
first(x_ts)  # get first element
last(x_ts)  # get last element
class(x_ts)  # class 'xts'
attributes(x_ts)
# get the time zone of an xts object
indexTZ(x_ts)
load(file="C:/Develop/R/lecture_slides/data/zoo_data.RData")
library(xts)  # load package xts
# as.xts() creates xts from zoo
st_ox <- as.xts(zoo_stx)
dim(st_ox)
head(st_ox[, 1:4], 4)
# plot using plot.xts method
xts::plot.xts(st_ox[, "Close"], xlab="", ylab="", main="")
title(main="MSFT Prices")  # add title
library(xts)  # load xts
library(lubridate)  # load lubridate
# coerce EuStockMarkets into class xts
x_ts <- xts(coredata(EuStockMarkets),
      order.by=date_decimal(index(EuStockMarkets)))
# plot all columns in single panel: xts v.0.9-8
col_ors <- rainbow(NCOL(x_ts))
plot(x_ts, main="EuStockMarkets using xts",
     col=col_ors, major.ticks="years",
     minor.ticks=FALSE)
legend("topleft", legend=colnames(EuStockMarkets),
 inset=0.2, cex=0.7, , lty=rep(1, NCOL(x_ts)),
 lwd=3, col=col_ors, bg="white")
# plot only first column: xts v.0.9-7
plot(x_ts[, 1], main="EuStockMarkets using xts",
     col=col_ors[1], major.ticks="years",
     minor.ticks=FALSE)
# plot remaining columns
for (col_umn in 2:NCOL(x_ts))
  lines(x_ts[, col_umn], col=col_ors[col_umn])
# plot using quantmod
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
# create ggplot object
etf_gg <- qplot(x=index(price_s),
          y=as.numeric(price_s),
          geom="line",
          main=names(price_s)) +
  xlab("") + ylab("") +
  theme(  # add legend and title
    legend.position=c(0.1, 0.5),
    plot.title=element_text(vjust=-2.0),
    plot.background=element_blank()
  )  # end theme
# render ggplot object
etf_gg
library(rutils)  # load xts time series data
library(reshape2)
library(ggplot2)
price_s <- rutils::etf_env$price_s[, c("VTI", "IEF")]
price_s <- na.omit(price_s)
# create data frame of time series
data_frame <- data.frame(dates=index(price_s),
    coredata(price_s))
# reshape data into a single column
data_frame <-
  reshape2::melt(data_frame, id="dates")
x11(width=6, height=5)  # open plot window
# ggplot the melted data_frame
ggplot(data=data_frame,
 mapping=aes(x=dates, y=value, colour=variable)) +
 geom_line() +
  xlab("") + ylab("") +
  ggtitle("VTI and IEF") +
  theme(  # add legend and title
    legend.position=c(0.2, 0.8),
    plot.title=element_text(vjust=-2.0)
  )  # end theme
# load rutils which contains etf_env dataset
library(rutils)
library(dygraphs)
price_s <- rutils::etf_env$price_s[, c("VTI", "IEF")]
price_s <- na.omit(price_s)
# plot dygraph with date range selector
dygraph(price_s, main="VTI and IEF prices") %>%
  dyOptions(colors=c("blue","green")) %>%
  dyRangeSelector()
# load rutils which contains etf_env dataset
library(rutils)
library(plotly)
price_s <- rutils::etf_env$price_s[, c("VTI", "IEF")]
price_s <- na.omit(price_s)
# create data frame of time series
data_frame <- data.frame(dates=index(price_s),
    coredata(price_s))
# plotly syntax using pipes
data_frame %>%
  plot_ly(x=~dates, y=~VTI, type="scatter", mode="lines", name="VTI") %>%
  add_trace(x=~dates, y=~IEF, type="scatter", mode="lines", name="IEF") %>%
  layout(title="VTI and IEF prices",
   xaxis=list(title="Time"),
   yaxis=list(title="Stock Prices"),
   legend=list(x=0.1, y=0.9))
# or use standard plotly syntax
p_lot <- plot_ly(data=data_frame, x=~dates, y=~VTI, type="scatter", mode="lines", name="VTI")
p_lot <- add_trace(p=p_lot, x=~dates, y=~IEF, type="scatter", mode="lines", name="IEF")
p_lot <- layout(p=p_lot, title="VTI and IEF prices", xaxis=list(title="Time"), yaxis=list(title="Stock Prices"), legend=list(x=0.1, y=0.9))
p_lot
# subset xts using a date range string
price_s <- rutils::etf_env$price_s
sub_prices <- price_s["2014-10-15/2015-01-10", 1:4]
first(sub_prices)
last(sub_prices)
# subset Nov 2014 using a date string
sub_prices <- price_s["2014-11", 1:4]
first(sub_prices)
last(sub_prices)
# subset all data after Nov 2014
sub_prices <- price_s["2014-11/", 1:4]
first(sub_prices)
last(sub_prices)
# comma after date range not necessary
all.equal(price_s["2014-11", ], price_s["2014-11"])
# .subset_xts() is faster than the bracket []
library(microbenchmark)
summary(microbenchmark(
  bracket=price_s[10:20, ],
  subset=xts::.subset_xts(price_s, 10:20),
  times=10))[, c(1, 4, 5)]
# specify string representing a date
dat_e <- "2014-10-15"
# subset price_s in two different ways
price_s <- rutils::etf_env$price_s
all.equal(price_s[index(price_s) >= dat_e],
    price_s[paste0(dat_e, "/")])
# boolean subsetting is slower because coercing string into date
library(microbenchmark)
summary(microbenchmark(
  boolean=(price_s[index(price_s) >= dat_e]),
  date=(price_s[paste0(dat_e, "/")]),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# coerce string into a date
dat_e <- as.Date("2014-10-15")
# boolean subsetting is faster than using date string
summary(microbenchmark(
  boolean=(price_s[index(price_s) >= dat_e]),
  date=(price_s[paste0(dat_e, "/")]),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
price_s <- HighFreq::SPY["2012-04"]
# subset recurring time interval using "T notation",
price_s <- price_s["T10:30:00/T15:00:00"]
first(price_s["2012-04-16"])  # first element of day
last(price_s["2012-04-16"])  # last element of day
# suppress timezone warning messages
options(xts_check_tz=FALSE)
price_s <- rutils::etf_env$price_s[, c("VTI", "IEF")]
price_s <- na.omit(price_s)
str(price_s)  # display structure of xts
# subsetting zoo to single column drops dim attribute
zoo_prices <- as.zoo(price_s)
dim(zoo_prices)
dim(zoo_prices[, 1])
# zoo with single column are vectors not matrices
c(is.matrix(zoo_prices), is.matrix(zoo_prices[, 1]))
# xts always have a dim attribute
rbind(base=dim(price_s), subs=dim(price_s[, 1]))
c(is.matrix(price_s), is.matrix(price_s[, 1]))
# lag of zoo shortens it by one row
rbind(base=dim(zoo_prices), lag=dim(lag(zoo_prices)))
# lag of xts doesn't shorten it
rbind(base=dim(price_s), lag=dim(lag(price_s)))
# lag of zoo is in opposite direction from xts
head(lag(zoo_prices, -1), 4)
head(lag(price_s), 4)
# library(HighFreq)  # load package HighFreq
# indices of last observations in each hour
end_points <- endpoints(price_s, on="hours")
head(end_points)
# extract the last observations in each hour
head(price_s[end_points, ])
# lower the periodicity to months
xts_monthly <- to.period(x=price_s,
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
load(file="C:/Develop/R/lecture_slides/data/zoo_data.RData")
library(xts)  # load package xts
# as.xts() creates xts from zoo
st_ox <- as.xts(zoo_prices)
# subset xts using a date
stox_sub <- st_ox["2014-11", 1:4]

# plot OHLC using plot.xts method
xts::plot.xts(stox_sub, type="candles", main="")
title(main="MSFT Prices")  # add title
load(file="C:/Develop/R/lecture_slides/data/zoo_data.RData")
ts_stx <- as.ts(zoo_stx)
class(ts_stx)
tail(ts_stx[, 1:4])
library(xts)
st_ox <- as.xts(zoo_stx)
class(st_ox)
tail(st_ox[, 1:4])
library(zoo)  # load package zoo
# show the generic function "merge"
merge
# show the "merge" method dispatched to "zoo" objects
merge.zoo
library(zoo)  # load package zoo
# get all methods for generic function merge()
methods(generic.function="merge")
# get generic function methods applied to "zoo" objects
methods(class="zoo")
# define a generic function
gen_sum <- function (a, b, ...) {
  UseMethod("gen_sum")
}  # end gen_sum

# define method for "numeric" class
gen_sum.numeric <- function (a, b, ...) {
  sum(a, b)
}  # end gen_sum.character

# define method for "character" class
gen_sum.character <- function (a, b, ...) {
  paste(a, "plus", b)
}  # end gen_sum.character

# apply gen_sum to "numeric" objects
gen_sum(1, 2)
# apply gen_sum to "character" objects
gen_sum("a", "b")
# 'cbind' is an internal generic function
cbind
# define "+" method for "character" class
"+.character" <- function (a, b, ...) {
  paste(a, "plus", b)
}  # end +.character
methods("+")  # view methods for "+" operator
# define variables with "character" class
char1 <- "a"
char2 <- "b"
class(char1)
char1 + char2  # add two "character" objects - doesn't work
attributes(char1)  # doesn't have explicit "character" class - only implicit
char1 <- structure("a", class="character")
char2 <- structure("b", class="character")
attributes(char1)  # now has explicit "character" class
# add two "character" objects
char1 + char2
# define object of class "string"
obj_string <- "how are you today?"
class(obj_string) <- "string"
obj_string
# overload "print" method for string objects
print.string <- function (str_ing) {
  print(
    paste(strsplit(str_ing, split=" ")[[1]],
  collapse=" + "))
}  # end print.string
# methods("print")  # view new methods for "print" function
print(obj_string)
obj_string
# overwrite "+" operator
"+" = function(a, b) {
  if (is.character(a) && is.character(b)) {
    paste(a, "plus", b)
  } else {
    .Primitive("+") (a, b)
  }
}
methods("+")  # view methods for "+" operator
# add two "numeric" objects
1 + 2
# add two "character" objects
"a" + "b"
# overwrite "+" operator with a generic function
"+" <- function (a, b, ...) {
  UseMethod("+")
}  # end gen_sum
# define method for "numeric" class
"+.numeric" <- function (a, b, ...) {
  sum(a, b)
}  # end gen_sum.character
# define method for "character" class
"+.character" <- function (a, b, ...) {
  paste(a, "plus", b)
}  # end gen_sum.character
methods("+")  # view methods for "+" operator
# add two "numeric" objects
1 + 2
# add two "character" objects
"a" + "b"
cbind.ts  # can't view non-visible method
stats::cbind.ts  # can't view non-visible method
stats:::cbind.ts  # display non-visible method
getAnywhere(cbind.ts)  # display non-visible method
