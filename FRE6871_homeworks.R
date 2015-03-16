#################################
### Homework ideas
#################################

rm(list=ls())

###########
# classes and inheritance

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

