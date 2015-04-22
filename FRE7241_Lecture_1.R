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
as.numeric(date_time)  # get internal integer representation
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
date_time <- Sys.time()  # get today's date and time
date_time
class(date_time)  # POSIXct object
as.numeric(date_time)  # get internal integer representation
# parse character string "%Y-%m-%d %H:%M:%S" to POSIXct object
as.POSIXct("2014-07-14 13:30:10")
format(date_time)  # convert POSIXct to character string
class(format(date_time))  # character string
date_time + 20  # add 20 seconds
as.POSIXct(as.Date(date_time)+1)  # add a day
trunc(date_time, units="hours")  # truncate to closest hour
as.POSIXct(as.character(as.Date(date_time)))  # truncate to closest day
methods(trunc)  # trunc methods
trunc.POSIXt
# parse character string "%Y-%m-%d %H:%M:%S" to POSIXlt object
date_time <- as.POSIXlt("2014-07-14 18:30:10")
date_time
class(date_time)  # POSIXlt object
aperm(as.matrix(unclass(date_time)))  # get internal representation

date_time + 20  # add 20 seconds
class(date_time + 20)  # implicit coercion to POSIXct
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
Sys.time()  # get today's date and time
Sys.timezone()  # get time-zone
Sys.setenv(tz="UTC")  # set time-zone to UTC
# parse character string "%Y-%m-%d %H:%M:%S" to POSIXlt object
as.POSIXlt("2014-07-14 18:30:10")
class(as.POSIXlt("2014-07-14 18:30:10")+3600)  # coercion to POSIXct
Sys.setenv(tz="America/New_York")  # set time-zone to "New York"
date_time <- as.POSIXct("2014-07-14 18:30:10", tz="America/New_York")
date_time
class(date_time)  # POSIXct object
date_time + 20  # add 20 seconds
as.numeric(date_time)  # get internal integer representation
format(date_time, tz="UTC")  # convert to character in different TZ
as.POSIXct(format(date_time, tz="UTC"))  # parse back to POSIXct
as.POSIXct(format(Sys.time(), tz="UTC")) - # difference between
  as.POSIXct(format(Sys.time(), tz=""))  # local time and UTC
library(zoo)  # load package zoo
date_time <- Sys.Date()  # create date series of class 'Date'
date_index <- date_time + 0:365  # daily series over one year
head(date_index, 4)  # print first few dates
format(head(date_index, 4), "%m/%d/%Y")  # print first few dates
# create daily date-time series of class 'POSIXct'
date_index <- seq(Sys.time(), by="days", length.out=365)
head(date_index, 4)  # print first few dates
format(head(date_index, 4), "%m/%d/%Y %H:%M:%S")  # print first few dates
# create series of monthly dates of class 'zoo'
monthly_index <- yearmon(2010+0:36/12)
head(monthly_index, 4)  # print first few dates
# create series of quarterly dates of class 'zoo'
qrtly_index <- yearqtr(2010+0:16/4)
head(qrtly_index, 4)  # print first few dates
# parse quarterly 'zoo' dates to POSIXct
Sys.setenv(tz="UTC")
as.POSIXct(head(qrtly_index, 4))
set.seed(1121)  # for reproducibility
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# create monthly time series starting 1990
ts_series <- ts(data=cumsum(rnorm(96)), 
     frequency=12, start=c(1990, 1))
class(ts_series)  # class 'ts'
attributes(ts_series)
matrix(methods(class="ts")[3:8], ncol=2)
# window the time series
window(ts_series, start=1992, end=1992.25)

plot(ts_series, type="l",  # create plot
     col="red", lty="solid", xlab="", ylab="")
title(main="Random Prices", line=-1)  # add title
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
class(EuStockMarkets)  # multiple ts object
dim(EuStockMarkets)
head(EuStockMarkets, 3)  # get first three rows
# plot all the columns
plot(EuStockMarkets, main="", xlab="")
# add title
title(main="EuStockMarkets", line=-1)
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
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
library(zoo)  # load package zoo
# create index of daily dates
date_index <- seq(from=as.Date("2014-07-14"), 
            by="day", length.out=1000)
# create zoo time series
zoo_series <- zoo(cumsum(rnorm(length(date_index))), 
            order.by=date_index)

class(zoo_series)  # class 'zoo'
tail(zoo_series, 4)  # get last few elements

# call plot.zoo
plot(zoo_series, xlab="", ylab="")
title(main="Random Prices", line=-1)  # add title
library(zoo)  # load package zoo
# create zoo time series
date_index <- Sys.Date() + 0:3
zoo_series <- zoo(rnorm(length(date_index)), 
         order.by=date_index)
zoo_series
index(zoo_series)  # extract time index
coredata(zoo_series)  # extract coredata
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
set.seed(1121)  # initialize the random number generator
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
library(zoo)  # load package zoo
# create daily date series of class 'Date'
date_index1 <- seq(Sys.Date(), by="days", 
             length.out=365)
# create zoo time series
zoo_series1 <- zoo(rnorm(length(date_index1)), 
           order.by=date_index1)
# create another zoo time series
date_index2 <- seq(Sys.Date()+350, by="days", 
             length.out=365)
zoo_series2 <- zoo(rnorm(length(date_index2)), 
           order.by=date_index2)
# rbind the two time series - ts1 supersedes ts2
zoo_series3 <- rbind(zoo_series1,
           zoo_series2[index(zoo_series2) > end(zoo_series1)])
plot(cumsum(zoo_series3), xlab="", ylab="")
# add vertical lines at stitch point
abline(v=end(zoo_series1), col="blue", lty="dashed")
abline(v=start(zoo_series2), col="red", lty="dashed")
title(main="Random Prices", line=-1)  # add title
# create daily date series of class 'Date'
date_index1 <- Sys.Date() + -3:1
# create zoo time series
zoo_series1 <- zoo(rnorm(length(date_index1)), 
         order.by=date_index1)
# create another zoo time series
date_index2 <- Sys.Date() + -1:3
zoo_series2 <- zoo(rnorm(length(date_index2)), 
         order.by=date_index2)
merge(zoo_series1, zoo_series2)  # union of dates
# intersection of dates
merge(zoo_series1, zoo_series2, all=FALSE)
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
library(zoo)  # load package zoo
# create zoo time series
zoo_series <- zoo(sample(4), 
            order.by=(Sys.Date() + 0:3))
# add NA
zoo_series[3] <- NA
zoo_series

na.locf(zoo_series)  # replace NA's using locf

na.omit(zoo_series)  # remove NA's using omit
set.seed(1121)  # for reproducibility
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
library(zoo)  # load package zoo
# create zoo time series
date_index <- Sys.Date() + 0:365
zoo_series <- zoo(rnorm(length(date_index)), order.by=date_index)
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
# perform monthly 'mean' aggregation
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
# convert mts object into zoo
zoo_series <- as.zoo(EuStockMarkets)
class(index(zoo_series))  # index is numeric
head(zoo_series, 3)
# approximately convert index into class 'Dates'
index(zoo_series) <- as.Date(365*(index(zoo_series)-1970))
head(zoo_series, 3)
# convert index into class 'Dates'
zoo_series <- as.zoo(EuStockMarkets)
index(zoo_series) <- date_decimal(index(zoo_series))
head(zoo_series, 3)
## par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
## library(tseries)  # load package tseries
## zoo_msft <- suppressWarnings(  # load MSFT data
##   get.hist.quote(instrument="MSFT",
##            start=Sys.Date()-365,
##            end=Sys.Date(),
##            origin="1970-01-01")
## )  # end suppressWarnings
## class(zoo_msft)
## dim(zoo_msft)
## tail(zoo_msft, 4)
## 
## # calculate Sharpe ratio
## sharpe(zoo_msft[, "Close"], r=0.01)
## # add title
## plot(zoo_msft[, "Close"], xlab="", ylab="")
## title(main="MSFT Close Prices", line=-1)
## par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
## library(tseries)  # load package tseries
## zoo_eurusd <- suppressWarnings(  # load EUR/USD data
##   get.hist.quote(
##     instrument="EUR/USD", provider="oanda",
##     start=Sys.Date()-365,
##     end=Sys.Date(),
##     origin="1970-01-01")
## )  # end suppressWarnings
## # bind and scrub data
## zoo_msfteur <- merge(zoo_eurusd,
##                zoo_msft[, "Close"])
## colnames(zoo_msfteur) <- c("EURUSD", "MSFT")
## zoo_msfteur <-
##   zoo_msfteur[complete.cases(zoo_msfteur),]
## ### plot with two "y" axes
## par(las=1)  # set text printing to "horizontal"
## # plot first ts
## plot(zoo_msfteur[, 1], xlab=NA, ylab=NA)
## # set range of "y" coordinates for second axis
## par(usr=c(par("usr")[1:2], range(zoo_msfteur[,2])))
## lines(zoo_msfteur[, 2], col="red")  # second plot
## axis(side=4, col="red")  # second "y" axis on right
## # print axis labels
## mtext(colnames(zoo_msfteur)[1], side=2, padj=-6, line=-4)
## mtext(colnames(zoo_msfteur)[2], col="red", side=4, padj=-2, line=-3)
## title(main="EUR and MSFT")  # add title
## # add legend without box
## legend("bottomright", legend=colnames(zoo_msfteur), bg="white",
##  lty=c(1, 1), lwd=c(2, 2), col=c("black", "red"), bty="n")
## 
## save(zoo_msft, zoo_eurusd, file="C:/Develop/data/zoo_data.RData")
## 
## ##########
## 
## # slightly different method using par(new=TRUE)
## # par(las=1)  # set text printing to "horizontal"
## # plot(zoo_msfteur[, 1], xlab=NA, ylab=NA)
## # par(new=TRUE)  # allow new plot on same chart
## # plot(zoo_msfteur[, 2], xlab=NA, ylab=NA, yaxt="n", col="red")
## # axis(side=4, col="red")  # second "y" axis on right
## # mtext(colnames(zoo_msfteur)[1], side=2, padj=-6, line=-4)
## # mtext(colnames(zoo_msfteur)[2], col="red", side=4, padj=-2, line=-3)
## # title(main="EUR and MSFT", line=-1)  # add title
## # legend("bottomright", legend=colnames(zoo_msfteur),
## #        lty=c(1, 1), lwd=c(2, 2), col=c("black", "red"), bty="n")
## 
## ##########
## 
## # "x" axis with monthly ticks - doesn't work
## # plot first ts wthout "x" axis
## # plot(zoo_msfteur[, 1], xaxt="n", xlab=NA, ylab=NA)
## # # add "x" axis with monthly ticks
## # month.ticks <- unique(as.yearmon(index(zoo_eurusd)))
## # axis(side=1, at=month.ticks, labels=format(month.ticks, "%b-%y"), tcl=-0.7)
## 
load(file="C:/Develop/data/zoo_data.RData")
ts_msft <- as.ts(zoo_msft)
class(ts_msft)
# rename colnames
colnames(ts_msft) <- paste0("MSFT.", colnames(ts_msft))
tail(ts_msft)

library(timeSeries)
tser_msft <- as.timeSeries(zoo_msft)
class(ts_msft)
tail(tser_msft)
