#################################
### Homework ideas
#################################

rm(list=ls())  # remove all

options(max.print=80)
options(digits=3)

par(new=TRUE)  # allow new plot on same chart
par(las=1)  # set text printing to "horizontal"

library(zoo)
# good package loading script inside functions
stopifnot("package:xts" %in% search() || require("xts", quietly=TRUE))


#####################
### temp stuff ###


zoomChart("2010")
zoomChart("2010-04/2010-06")

########

blah <- merge(roll_max, roll_min)
plot(blah, main="range stats")
legend("topleft", legend=colnames(blah), bg="white", 
       lty=c(1, 1, 1), lwd=c(2, 2, 2), col=c("black", "red", "blue"))


reg_model <- lm(range~volume, data=range_volume["2008/2009"])
plot(reg_model)

reg_model <- lm(range~volume, data=diff(range_volume))
reg_model <- lm(range~volume, data=diff(range_volume["2010/"], lag=11))
reg_model <- lm(range~volume, data=diff(range_volume["2008/2009"]))
summary(reg_model)
plot(range~volume, data=diff(range_volume["2010/"]))

adf.test(range_volume[, "range"])
adf.test(cumsum(rnorm(nrow(range_volume))))

cor(x=range_volume[, "range"], y=range_volume[, "volume"], method="pearson")
cor.test(x=range_volume[, "range"], y=range_volume[, "volume"], method="pearson")
cor(x=range_volume[, "range"], y=range_volume[, "volume"], method="kendall")
cor.test(x=range_volume[, "range"], y=range_volume[, "volume"], method="kendall")
cor(x=range_volume[, "range"], y=range_volume[, "volume"], method="spearman")
cor.test(x=range_volume[, "range"], y=range_volume[, "volume"], method="spearman")


#####################
### end temp stuff ###


#####################


### assign names to vector elements using the function paste()
vec_tor <- rnorm(10)
names(vec_tor) <- paste("el", 1:10, sep='')
names(vec_tor) <- gsub("el", "num", names(vec_tor))

### create function that throws error if argument is negative
test_func <- function(arg_var) {
  if (!is.numeric(arg_var)) {
    warning(paste("argument", arg_var, "isn't numeric"))
    return(NULL)
  }
  2*arg_var
} # end test_func


### create a function called "my_sqrt" that calculates the square root of its single argument,
# "my_sqrt" should check if the input is numeric and positive,
# if the input is numeric and positive, then "my_sqrt" should return the square root,
# if the input is numeric and negative, then "my_sqrt" should broadcast a warning using "cat", and return the square root of the absolute value,
# if the input is not numeric, then "my_sqrt" should broadcast a different warning using "cat", and return NA,
my_sqrt <- function(arg_var) {
  if (is.numeric(arg_var) && arg_var>=0) {
    sqrt(arg_var)
  } else if (is.numeric(arg_var)) {
    cat("negative input!\t")
    sqrt(abs(arg_var))
  } else {
    cat("not numeric input!\t")
    NULL
  }
}  # my_sqrt
my_sqrt(4)
my_sqrt(-4)
my_sqrt("a")


### create function called "my_sqrt" that throws error if argument is negative
my_sqrt <- function(arg_var) {
  if (arg_var > 0) {
    sqrt(arg_var)
  } else {
    stop('bad input!')  # throw error
  }
}  # my_sqrt


### create function called "read_numeric" that reads numbers input by the user, and returns them in a vector,
# "read_numeric" should ask the user to input a number, and should read the input using the function "readline",
# "read_numeric" should read numbers from the console in a "while" loop,
# "read_numeric" should validate the inputs, and produce errors and Warnings,
# if the user input is numeric, then "read_numeric" should append the input to the numeric output vector,
# if the input is not numeric, then "read_numeric" should produce a Warning "input is not numeric!",
# if the input is empty, then "read_numeric" should terminate, and return the numeric output vector,
# hint: "read_numeric" should use "readline", and can also use "is.na", "nchar", "as.numeric", "length", "identical", etc.
# the function reads numeric lines from input, and returns them in a vector,
# "read_numeric" should create a numeric vector consisting of the input numbers, 
# ignore it

read_numeric <- function() {
  out_put <- numeric(0)
  nu_meric <- readline("Enter a number: ")
  while(nchar(nu_meric) > 0) {
    nu_meric <- as.numeric(nu_meric)
    if (!is.na(nu_meric)) {
      out_put <- c(out_put, as.numeric(nu_meric))
    } else {
      warning("input is not numeric!")
    }  # end if
    nu_meric <- readline("Enter a number: ")
  }  # end while
  out_put
}  # end read_numeric
read_numeric()

# old version
read_numeric <- function() {
  out_put <- numeric(0)
  nu_meric <- readline("Enter a number: ")
  while(!identical(nu_meric, "")) {
    nu_meric <- as.numeric(nu_meric)
    if (!is.na(nu_meric)) {
      out_put <- c(out_put, nu_meric)
    }  # end if
    nu_meric <- readline("Enter a number: ")
  }  # end while
  out_put
}  # end read_numeric
read_numeric()



###############
# Create function that Calculates row and column containing the extreme value of a matrix.
# The extreme value is Calculated by the function "ex_treme", which can be "max" or "min", etc.
which_matrix <- function(mat_rix, ex_treme="max") {
  ex_treme <- match.fun(ex_treme)
  which(mat_rix==ex_treme(mat_rix), arr.ind=T)
#   tmp <- which(mat_rix==ex_treme(mat_rix), arr.ind=T)
#   coordinates <- as.numeric(c(rownames(mat_rix)[tmp[1,1]], colnames(mat_rix)[tmp[1,2]]))
#   coordinates
}  # end which_matrix




###############
# 3. (20pts even without legend) Plot the probability density of DAX returns together with t-distribution returns with four degrees of freedom on a single plot,
# plot t-distribution
x_var <- seq(-5, 5, length=100)
x_var <- seq(-6, -3, length=100)
plot(x=x_var, y=dt(x_var, df=4), type="l", lwd=2, xlab="", ylab="", ylim=c(0, 0.03))
# add line for density of DAX returns
ts_rets <- 100*diff(log(EuStockMarkets[, 1]))
lines(density(ts_rets), col="red", lwd=2)
# add legend
legend("topright", title="DAX vs t-distr", legend=c("t-distr", "DAX"), 
       inset=0.05, cex=0.8, lwd=2, lty=c(1, 1), col=c("black", "red"))



###############
### using mtcars data, plot a boxplot of mpg of cars with six cylinders
boxplot(mtcars[mtcars$cyl==6, ]$mpg)
with(mtcars[mtcars$cyl==6, ], boxplot(mpg))

# 2. (10pts) plot a histogram of "mpg" for all cars in the mtcars data frame, 
#    use function "truehist", and set the "prob" argument so that the plot displays the number of cars in each bin,
#    "truehist" counts the first two bins differently from "hist"
truehist(mtcars$mpg, nbins="FD", prob=FALSE, col="blue", xlab="mpg", ylab="number of cars", main="mpg true histogram")
hist(mtcars$mpg, breaks="FD", prob=FALSE, col="blue", xlab="mpg", ylab="number of cars", main="mpg histogram")

### using mtcars data, create a data frame called "cars_18", containing cars that have mpg greater than 18,
cars_18 <- mtcars[mtcars$mpg>18, ]
# using the function "table", calculate the number of cars in "cars_18", that have four, six, and eight cylinders
table(cars_18$cyl)
# or
sum(cars_18$cyl==4)
sapply(cars_18$cyl, sum)

with(mtcars[mtcars$mpg>20, ], barplot(mpg))

barplot(mtcars$mpg)
barplot(mtcars[mtcars$cyl==6, ]$mpg)
with(mtcars[mtcars$cyl==6, ], barplot(mpg))



###############
# The package "Ecdat" contains a data.frame called "Cigarette".
# Subset "Cigarette" to extract data only for "state"=="NY", and call it "data_ny",
library("Ecdat")  # load econometric data sets
data_ny <- Cigarette[Cigarette[, "state"]=="WY", ]


# the column dates_ny$year contains years as strings in the format "yyyy",
# from the column dates_ny$year create a vector of "Date" dates in the format "yyyy-01-01", 
# and call it "dates_ny", use function paste(),
dates_ny <- as.Date(paste(data_ny$year, "-01-01", sep=""))


# Create a "zoo" from data_ny, excluding the columns "state" and "year", 
# and the index "dates_ny", and call it "zoo_ny", 
library("zoo")
zoo_ny <- zoo(x=data_ny[, -(1:2)], order.by=dates_ny)

# plot the column "income", and add title "Cigarette tax income in NY state",
plot(zoo_ny[, "income"], xlab="", ylab="", main="Cigarette tax income in NY state")



###############
# The package "Ecdat" contains a data.frame called "Garch".
# the column Garch$date contains dates as strings in the format "yymmdd",
# from the column Garch$date create a vector of strings in the format "yyyy-mm-dd", 
# using nested paste() and substring()
library("Ecdat")  # load econometric data sets
head(Garch)  # explore the data
ymd(paste0(19, Garch[1, 1]))
# my_date is a numeric date that represents "1997-18-05"
# use several functions to convert it to a as.POSIXct date
# create a my_date is a numeric date that represents "1997-05-18",
# you can use functions from package lubridate, or other functions paste and substr,
# you will not receive any credit for creating a date "by hand": as.POSIXct("1997-05-18"),
my_date <- 970518
library(lubridate)
ymd(paste0(19, my_date), tz="America/New_York")
# as.POSIXct("1997-05-18")
as.POSIXct(
  paste(paste0(19, substr(my_date, 1, 2)), 
        substr(my_date, 3, 4), 
        substr(my_date, 5, 6), sep="-")
)


# 1. (5pts) Convert integers representing dates to "POSIXct" date-time objects,
# Load the package "Ecdat", which contains a data frame called "Yen",
# the column Yen$date contains integers representing dates, in the format "yyyymmdd",
# from the column Yen$date create a vector of "POSIXct" dates, and call it "in_dex", 
# set the POSIXct timezone to "UTC", 
# hint: you can either use functions as.character() and as.POSIXct() (with a "format" argument), 
# or function ymd() from package "lubridate",

library("Ecdat")  # load Ecdat
library(lubridate)
head(Yen)  # explore the data

# first method
in_dex <- as.POSIXct(as.character(Yen$date), format="%Y%m%d", tz="UTC")

# second method
in_dex <- ymd(Yen$date, tz="UTC")


# Create an "xts" object from the column Yen$s and "in_dex", and call it "xts_yen",

library("xts")
xts_yen <- xts(Yen$s, order.by=in_dex)

# plot "xts_yen", using generic function plot(),

plot(xts_yen)




###############
# time series
###############


###############
# calculate stats of ts and return as vector of variable length
my_stats <- function(ts_var) {
  c(max(ts_var), min(ts_var), mean(ts_var), if (rnorm(1)>0) 1 else NULL)
}

# sapply returns list because of vectors of variable length
out_sapply <- sapply(EuStockMarkets, my_stats)



###############
# create function which calculates summary statistics of zoo
# first split
# returns list of data frames (zoo)
split_eu <- split(as.zoo(EuStockMarkets), colnames(EuStockMarkets))

# then call lapply or call sapply
# does sapply return list? - yes - if vectors are not same length
# convert to matrix using do_call_rbind(), instead of this:
do.call(rbind, list.data)

out_lapply <- lapply(EuStockMarkets, mean)  # returns list of means
do.call(rbind, out_lapply)  # returns single column matrix
do.call(cbind, out_lapply)  # returns single row matrix
as.vector(out_lapply)
as.vector(do.call(cbind, out_lapply))
unlist(out_lapply)  # returns
class(unlist(out_lapply))
is.vector(unlist(out_lapply))
is.vector(do.call(cbind, out_lapply))
is.matrix(do.call(cbind, out_lapply))



###############
# download two series: daily and monthly
# cbind the series and remove NAs



###############
# extract Mondays - weekly dates
as.Date("2013-09-02") + 0:5
weekdays(as.Date("2013-09-02") + 0:5)
# create weekday logical vector
zoo_series <- zoo_series[is_weekday, ]



##################################
# 3. (20pts) Download data for symbols "VTI" and "VEU" from Yahoo, 
# and save the data to a new environment called "data_env",
# use functions new.env() and getSymbols(),
# package quantmod

data_env <- new.env()
getSymbols(c("VTI", "VEU"), env=data_env)


######
# Extract the adjusted price columns from all the variables contained in "data_env", 
# and call it "etf_series_ad", 
# Extract the volume columns from all the variables contained in "data_env", 
# and call it "etf_series_vo", 
# use functions do.call(), merge(), eapply(), Ad(), and Vo(),

etf_series_ad <- do.call(merge, eapply(data_env, Ad))
etf_series_vo <- do.call(merge, eapply(data_env, Vo))



########################
### stochastic processes


### autocorrelations of absolute deviations

# EuStockMarkets autocorrelation
# extract lag=5 vector of autocorrelation coefficients using functions acf() and drop(),
drop(acf(na.omit(diff(log(EuStockMarkets[, 1]))), lag=5, plot=FALSE)$acf)[-1]

# extract autocorrelations of absolute deviations
drop(acf(na.omit(diff(abs(na.omit(diff(log(EuStockMarkets[, 1])))))), lag=5, plot=FALSE)$acf)[-1]
# plot
acf_plus(na.omit(diff(abs(na.omit(diff(log(EuStockMarkets[, 1])))))), lag=5)


# rnorm autocorrelation
len_gth <- length(EuStockMarkets[, 1])
rand_walk <- zoo(rnorm(len_gth), order.by=(Sys.Date()+0:(len_gth-1)))

# extract autocorrelations of absolute deviations
drop(acf(na.omit(diff(abs(rand_walk))), lag=5, plot=FALSE)$acf)[-1]
# plot
acf_plus(na.omit(diff(abs(rand_walk))), lag=5)



### partial autocorrelations of time series

# create ARIMA time series of class "ts"
set.seed(1121)
zoo_arima <- zoo(
  x=arima.sim(n=1000, model=list(ar=c(0.2, 0.3))),
  order.by=(Sys.Date()-0:999))

# calculate autocorrelations using acf()
vec_acf <- drop(acf(zoo_arima, lag=5, plot=FALSE)$acf)


# create ARIMA time series pure vector
zoo_arima <- coredata(arima.sim(n=1000, model=list(ar=c(0.2, 0.3))))

# calculate autocorrelations by hand
# first lag time series
zoo_arima_lag <- zoo_arima
zoo_arima_lag <- zoo_arima_lag[-length(zoo_arima_lag)]
zoo_arima <- zoo_arima[-1]
head(cbind(zoo_arima, zoo_arima_lag))
tail(cbind(zoo_arima, zoo_arima_lag))
# sqrt(sum(zoo_arima_lag^2)/length(zoo_arima_lag))
# sum((zoo_arima-mean(zoo_arima))*(zoo_arima_lag-mean(zoo_arima_lag)))/(sd(zoo_arima-mean(zoo_arima))*sd(zoo_arima_lag))/(length(zoo_arima)-1)

# autocorrelation equal to cor of time series with its lag
auto_corr <- cor(zoo_arima, zoo_arima_lag)


# create time series that's not correlated with the lagged series,
# but doesn't mean its not correlated with its own lag,
zoo_arima_1 <- zoo_arima - auto_corr*sd(zoo_arima)*zoo_arima_lag/sd(zoo_arima_lag)
zoo_arima_1 <- zoo_arima - vec_acf[2]*sd(zoo_arima)*zoo_arima_lag/sd(zoo_arima_lag)

cor(zoo_arima_lag, zoo_arima_1)



### below are scratch or incorrect - doesn't work properly:

######
# Create a series lagged by one period from "ts_arima", and call it "ts_arima_lag",
# The value of "ts_arima_lag" in a given period should be equal to 
# the value of "ts_arima" in the previous period,
# Create a series lagged by two periods from "ts_arima", and call it "ts_arima_lag2",
# use function lag() with the proper argument "k",

# create ARIMA time series of class "ts"
zoo_arima <- arima.sim(n=1000, model=list(ar=c(0.2, 0.3)))


# verify that "ts_arima_lag" and "ts_arima_lag2" are correctly lagged by inspecting them,
# use functions head() and cbind(),

head(cbind(ts_arima, ts_arima_lag, ts_arima_lag2))
tail(cbind(ts_arima, ts_arima_lag, ts_arima_lag2))

######
# Create a linear combination of "ts_arima_1" and its lag=2 series, and call it "ts_arima_2",
# such that the lag=2 autocorrelation of "ts_arima_2" is equal to zero, or is very close to zero,

ts_arima_2 <- ts_arima_1 - vec_acf_1[3]*lag(ts_arima_1, k=-2)
vec_acf_2 <- drop(acf(ts_arima_2, lag=5, plot=FALSE)$acf)
# plot
acf_plus(ts_arima_2, lag=5)



######
# Create a linear combination of "zoo_arima" and "zoo_arima_lag", and call it "zoo_arima_1" (decorrelated),
# such that its lag=1 autocorrelation coefficient is equal to zero, or is very close to zero,
# Extract the lag=5 autocorrelation vector of "zoo_arima_1", and call it "vec_acf_1",
# verify that the lag=1 autocorrelation is very close to zero,

zoo_arima_1 <- zoo_arima - vec_acf[2]*sd(zoo_arima)*zoo_arima_lag/sd(zoo_arima_lag)
vec_acf_1 <- drop(acf(zoo_arima_1, lag=5, plot=FALSE)$acf)
# plot
acf_plus(zoo_arima_1, lag=5)



###########
# classes and inheritance
###############

# create new generic function and method for "string" class, based on: "reverse", "trim", "pad", "scramble",
# create "stringy" class, derived from "string" class
# create new methods for "stringy" class, based on existing generic functions: "length", "+", "print"
# create new methods for "stringy" class, based on "string" generic functions: "", "", ""
# show that "stringy" inherits methods from "string" class

# derive (not create!) new "string" class from "character" object
# simply add "string" to class vector
as.string <- function(x) {
  if(!inherits(x, "string"))
    class(x) <- c("string", class(x))
  x  # return "x"
}

# derive (not create!) new "string" class from "character" object
# define generic "string" class converter
as.string <- function (x, ...) 
  UseMethod("as.string")
# default "string" class converter
as.string.default <- function (x, ...) {
  if(!inherits(x, "string"))
    x <- structure(x, class=c("string", class(x)), ...)
  x  # return "x"
}  # end as.string.default
# numeric "string" class converter
as.string.numeric <- function (x, ...) {
  if(!inherits(x, "string"))
    x <- structure(as.character(x), class=c("string", class(x)), ...)
  x  # return "x"
}  # end as.string.numeric
is.string <- function (x)
  inherits(x=x, what="string")
# define "string" object
obj_string <- as.string("how are you today?")
obj_string
class(obj_string)
is.string(obj_string)
is.string("hello")
as.string(123)
is.string(as.string(123))

# overload "+" function for "string" class
"+.string" <- function (a, b, ...) {
  paste(a, "plus", b)
}  # end +.string
# adds character indices and returns character with index equal to the sum
"+.string" <- function (a, b, ...) {
  in_dex <- (which(letters==substring(a, 1, 1)) + which(letters==substring(b, 1, 1))) %% length(letters)
  letters[in_dex]
}  # end +.string
methods("+")  # view methods for "+" operator
string1 <- structure("hello", class="string")
string2 <- structure("there", class="string")
class(string1)
string1 + string2  # add two "string" objects

# borrow from "stringr": "check_string", "str_length"
# overload "print" function for "string" class
print.string <- function (str_ing) {
  print(
    paste(strsplit(str_ing, split=" ")[[1]],
          collapse=" + "))
}  # end print.string
print(my_string)

# define generic "first" function (if not defined by "xts")
first <- function (x, ...) 
  UseMethod("first")
# define "first" method for "string" class
first.string <- function (str_ing, ...) {
  unclass(substring(str_ing, 1, 1))
}  # end first.string
first(string1)
last.string <- function (str_ing, ...) {
  unclass(substring(str_ing, nchar(str_ing), nchar(str_ing)))
}  # end last.string
last(string1)


### function that adds "character" class objects
add_char <- function (char1, char2) {
# test for "character" class and throw error
  stopifnot(is.character(char1) && is.character(char1))
  in_dex <- (which(letters==substr(char1, 1, 1)) + which(letters==substr(char2, 1, 1))) %% length(letters)
  letters[in_dex]
}  # end add_char

add_char("c", "b")
add_char("1", "b")
add_char(1, "b")
a <- "efg"
b <- "opq"
add_char(a, b)


class(my_stringy) <- c("stringy", "string")

"+.stringy" <- function (a, b, ...) {
  paste(a, "plus", b)
}  # end +.stringy

# create "base5" arithmetic class, derived from "numeric" class
# create new methods for "base5" class, based on existing generic functions: "+", "-", "*", "/"


baz <- function(x) UseMethod("baz", x)
baz.A <- function(x) "A"
baz.B <- function(x) "B"
ab <- 1
class(ab) <- c("A", "B")
ba <- 2
class(ba) <- c("B", "A")

ab <- structure(1, class = c("A", "B"))
ba <- structure(1, class = c("B", "A"))
baz(ab)
baz(ba)

"+.character" <- function(a, b, ...){
  NextMethod()
}



#################################
### Homework assignment #5
#################################


# 1. Download the zoo Quickref manual from CRAN, and read how to query the Oanda database,
# Download from Yahoo the EOD CLOSE quotes for MSFT stock, starting from Sep/01/2013,
library(tseries)  # load package tseries
library(zoo)  # load package zoo
# load MSFT data
suppressWarnings(
  zoo_msft <- get.hist.quote(instrument="MSFT", 
                             start=as.POSIXct("2013-09-01"), 
                             end=Sys.Date(), 
                             origin="1970-01-01")
)  # end suppressWarnings
# extract only EOD CLOSE quotes
zoo_msft <- zoo_msft[, "Close"]
# rename column
colnames(zoo_msft) <- "MSFT"


# 2. Download from Oanda the EOD CLOSE quotes for eur currency, starting from Sep/01/2013,
suppressWarnings(  # load EUR/USD data
  zoo_eurusd <- get.hist.quote(
    instrument="EUR/USD", provider="oanda",
    start=as.POSIXct("2013-09-01"), 
    end=Sys.Date(), 
    origin="1970-01-01")
)  # end suppressWarnings


# 3. Create smooth version of each time series using rollmean over an 11 period window,
zoo_msft_smooth <- rollmean(x=zoo_msft, k=11)
zoo_eurusd_smooth <- rollmean(x=zoo_eurusd, k=11)

# 4. Replace NA values using na.locf, first going forward, and then backward in time,
zoo_msft_smooth <- na.locf(zoo_msft_smooth)
zoo_msft_smooth <- na.locf(zoo_msft_smooth, fromLast=TRUE)



# 5. Plot each time series combined with its smoothed version,
# first plot MSFT
plot(zoo_msft, type="l", lwd=2, xlab="", ylab="")
# add smoothed MSFT
lines(zoo_msft_smooth, col="red", lwd=2)
# add legend
legend("bottomright", title="MSFT smoothed", legend=c("MSFT", "MSFT-smooth"), 
       inset=0.05, cex=0.8, lwd=2, lty=c(1, 1), col=c("black", "red"))

# this is a version with better X-axis date labels (not required for full credit)
# first plot without X-axis
plot(zoo_msft, type="l", lwd=2, xlab="", ylab="", xaxt="n")
# create X-axis date labels
axis_dates <- seq(from=as.Date("2013-09-01"), to=Sys.Date(), by="quarter")
# add X-axis
axis(side=1, at=axis_dates, labels=format(axis_dates, "%b-%y"))
# add smoothed MSFT
lines(zoo_msft_smooth, col="red", lwd=2)
# add legend
legend("bottomright", title="MSFT smoothed", legend=c("MSFT", "MSFT-smooth"), 
       inset=0.05, cex=0.8, lwd=2, lty=c(1, 1), col=c("black", "red"))


# 6. (5pts) Bind the two original time series together using merge,
# Remove observations containing NA values,
zoo_msfteur <- merge(zoo_eurusd, zoo_msft)
colnames(zoo_msfteur) <- c("EURUSD", "MSFT")
zoo_msfteur <- zoo_msfteur[complete.cases(zoo_msfteur), ]


# 7. (5pts) Plot the MSFT stock and eur currency time series together on a plot with two y axes,
# plot first ts
plot(zoo_msfteur[, 1], xlab=NA, ylab=NA)
# set range of "y" coordinates for second axis
par(usr=c(par("usr")[1:2], range(zoo_msfteur[,2])))
lines(zoo_msfteur[, 2], col="red")  # second plot
axis(side=4, col="red")  # second "y" axis on right
# print axis labels
par(las=1)  # set text printing to "horizontal"
mtext(colnames(zoo_msfteur)[1], side=2, padj=-6, line=-4)
mtext(colnames(zoo_msfteur)[2], col="red", side=4, padj=-2, line=-3)
title(main="EUR and MSFT")  # add title
# add legend without box
legend("bottom", legend=colnames(zoo_msfteur), bg="white", 
       lty=c(1, 1), lwd=c(2, 2), col=c("black", "red"), bty="n")




#################################
### HW #6 Solution
#################################
# Max score 25pts

# comment:
# Half of the credit for the first part (max 15pts) is from properly calculating 
# the length (nrow) of the list object, because nrow() returns NULL for one-dimensional objects.

# Homework assignment:

# 1. (15pts) Create a function called str_ts(), which summarizes time series objects,
# The function input is a time series object,
# The function should return a named list object with the following information: length (nrow), dimensions, number of rows with bad data, colnames, the object's class, data type, and the first and last rows of data,
# The function should validate its argument, and throw an error if it's not a time series object,

str_ts <- function(ts_series=NULL) {
  # check if argument is a time series object
  stopifnot(is.ts(ts_series) || is.zoo(ts_series))
  # create list and return it
  list(
    length=ifelse(is.null(nrow(ts_series)), length(ts_series), nrow(ts_series)),
    dim=dim(ts_series),
    bad_data=sum(!complete.cases(ts_series)),
    col_names=colnames(ts_series),
    ts_class=class(ts_series),
    ts_type=typeof(ts_series),
    first_row=head(ts_series, 1),
    last_row=tail(ts_series, 1)
  )  # end list
}  # end str_ts



# 2. (10pts) Create a synthetic zoo time series of prices with two named columns, based on random returns equal to "rnorm",
# Introduce a few NA values into the time series, and call str_ts() on this time series,
library(zoo)  # load package zoo
ts_var <- zoo(matrix(rnorm(20), ncol=2), order.by=(Sys.Date() - 1:10))
colnames(ts_var) <- paste0("col", 1:2)
ts_var[3, 1] <- NA
ts_var[6, 2] <- NA
str_ts(ts_var)

