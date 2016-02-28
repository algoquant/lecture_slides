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


########################
### data input/output

# write code for creating csv files in a specified dir

# write code for reading csv files in a specified dir

# get names of all csv files in cwd
file_names <- list.files(path=getwd(), pattern=glob2rx("*.csv"))
head(file_names)
tail(file_names)

# read the first six csv files into a list of data frames
da_ta <- lapply(head(file_names), function(file_name) {
  read.csv(file=file_name)
})  # end lapply

head(da_ta[[6]])



pollutantmean <- function(directory, pollutant, id) {
  file_names <- file.path(directory, paste0(sprintf(fmt="%03d", id), ".csv"))
  da_ta <- lapply(file_names, 
                  function(file_name)
                    read.csv(file=file_name)[, pollutant]
  )  # end lapply
  da_ta <- do.call(c, da_ta)
  mean(da_ta, na.rm=TRUE)
}  # end pollutantmean

file.path("C:/Develop/data/specdata", paste0(sprintf(fmt="%03d", 1:3), ".csv"))

pollutant <- "nitrate"
foo <- read.csv(file="C:/Develop/data/specdata/001.csv")[, "nitrate"]
class(foo)
head(foo)
tail(foo)

debug(pollutantmean)
pollutantmean("C:/Develop/data/specdata", "sulfate", 1:10)



########################
### functions


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



########################
### expressions


### while loops



########################
### dates and times


########################
### time series


########################
### stochastic processes


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

