

library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
thm <- knit_theme$get("acid")
knit_theme$set(thm)



# define a power function factory
make_func <- function(arg_param) {  # wrapper function
  function(arg_var) {  # anonymous closure
    arg_var^arg_param
  }
}  # end make_func

square_func <- make_func(2)  # define square function
square_func(4)
cube_func <- make_func(3)  # define cube function
cube_func(2)
cube_root_func <- make_func(1/3)  # define cube root function
cube_root_func(8)



make_counter <- function() {
# counter function with mutable state
  counter <- 0  # initialize counter
  cat('counter = ', counter)
  function() {  # return anonymous advance function
    counter <<- counter + 1  # advance counter
    cat('counter = ', counter)
  }  # end advance function
}  # end make_counter

advance_counter <- make_counter()  # create new counter
advance_counter()  # advance counter
advance_counter()  # advance counter
advance_counter_two <- make_counter()  # create another counter
advance_counter_two()  # advance counter two
advance_counter()  # advance counter one
advance_counter_two()  # advance counter two
advance_counter()  # advance counter one



# Returns the pseudo-random generating function random_generator
# the formal argument 'seed' persists in the evaluation environment of seed_random
seed_random <- function(seed) {  # seed must be an integer
  random_number <- as.numeric(paste0('0.', seed))  # initialize
# random_generator returns a vector of pseudo-random numbers of length length_rand
  random_generator <- function(length_rand=1) {  # assign function name for recursion
# Returns a vector of pseudo-random numbers of length length_rand
    random_number <<- 4*random_number*(1 - random_number)  # logistic map
    if (length_rand == 1) {
      return(random_number)
    } else {
      return(c(random_number, random_generator(length_rand - 1)))
    }  # end if
  }  # end random_generator
}  # end seed_random

# create a random number generating function and set seed
make_random <- seed_random(88)
make_random(10)  #  calculate vector of 10 pseudo-random numbers
ls(environment(make_random))  # list objects in scope of make_random



rm(list=ls())
# the super-assignment operator '<<-' adjusts the balance
# 'balance' exists in open_account evaluation environment
# bank account example (from Venables) demonstrates mutable states
# 'balance' is persistent between function calls
open_account <- function(balance) {
# returns function list for account operations
  list(
    deposit = function(amount) {  # make deposit
      if(amount > 0) {
balance <<- balance + amount  # '<<-' super-assignment operator
cat(amount, "deposited. Your balance is now:", 
    balance, "\n")
      } else {
cat("Deposits must be positive!\n")
      }
    },  # end deposit
    withdraw = function(amount) {  # make withdrawal
      if(amount <= balance) {
balance <<- balance - amount  # '<<-' super-assignment operator
cat(amount, "withdrawn. Your balance is now:", 
    balance, "\n")
      } else {
cat("You don't have that much money!\n")
      }
    },  # end withdraw
    get_balance = function() {  # get balance
      cat("Your current balance is:", balance, "\n")
    }  # end get_balance
  )  # end list
}  # end open_account



# perform account operations
# open an account with 100 deposit
my_account <- open_account(100)
ls(my_account)  # my_account is a list
# add my_account to search path
attach(my_account)
withdraw(30)  # withdrawal to buy groceries
deposit(100)  # deposit paycheck to account
withdraw(200)  # withdrawal to buy Gucci bag
get_balance()  # get account balance

# list objects in scope of get_balance
ls(environment(get_balance))

detach(my_account)  # remove my_account from search path



library(zoo)  # load package zoo
# get all methods for generic function "cbind"
methods("cbind")

# show the method of "cbind" applied to "zoo" objects
cbind.zoo



library(zoo)  # load package zoo
# get all methods for generic function "cbind"
# get generic function methods applied to "zoo" objects
methods(class="zoo")



cbind.ts  # can't view code of non-visible functions
getAnywhere(cbind.ts)  # display function
stats:::cbind.ts  # display function



# get all methods for generic function "plot"
methods("plot")

getAnywhere(plot)  # display function



rm(list=ls())
my_zoo <- zoo(rnorm(4), order.by=(Sys.Date() + 0:3))
class(my_zoo)
length(my_zoo)

# coerce "zoo" object to new class "newts"
class(my_zoo) <- "newts"
class(my_zoo)

# define "length" method for class "newts"
length.newts <- function(in_ts) {
# "length" method for class" "newts"
  cat("length of object from newts class\n")
  length(unclass(in_ts))
}  # end length.newts

# apply new "length" method
length(my_zoo)



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



par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
library(tseries)  # load package tseries
zoo_msft <- suppressWarnings(  # load MSFT data
  get.hist.quote(instrument="MSFT", 
           start=Sys.Date()-365, 
           end=Sys.Date(), 
           origin="1970-01-01")
)  # end suppressWarnings
class(zoo_msft)
dim(zoo_msft)
tail(zoo_msft, 4)

sharpe(zoo_msft[, "Close"], r=0.01)  # calculate Sharpe ratio

plot(zoo_msft[, "Close"], xlab="", ylab="")
title(main="MSFT Close Prices", line=-1)  # add title



par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
library(tseries)  # load package tseries
zoo_eurusd <- suppressWarnings(  # load EUR/USD data
  get.hist.quote(
    instrument="EUR/USD", provider="oanda",
    start=Sys.Date()-365, 
    end=Sys.Date(), 
    origin="1970-01-01")
)  # end suppressWarnings
# bind and scrub data
zoo_msfteur <- merge(zoo_eurusd, 
               zoo_msft[, "Close"])
colnames(zoo_msfteur) <- c("EURUSD", "MSFT")
zoo_msfteur <- 
  zoo_msfteur[complete.cases(zoo_msfteur),]
### plot with two "y" axes
par(las=1)  # set text printing to "horizontal"
# plot first ts
plot(zoo_msfteur[, 1], xlab=NA, ylab=NA)
# set range of "y" coordinates for second axis
par(usr=c(par("usr")[1:2], range(zoo_msfteur[,2])))
lines(zoo_msfteur[, 2], col="red")  # second plot
axis(side=4, col="red")  # second "y" axis on right
# print axis labels
mtext(colnames(zoo_msfteur)[1], side=2, padj=-6, line=-4)
mtext(colnames(zoo_msfteur)[2], col="red", side=4, padj=-2, line=-3)
title(main="EUR and MSFT")  # add title
# add legend without box
legend("bottomright", legend=colnames(zoo_msfteur), bg="white", 
 lty=c(1, 1), lwd=c(2, 2), col=c("black", "red"), bty="n")


##########

# slightly different method using par(new=TRUE)
# par(las=1)  # set text printing to "horizontal"
# plot(zoo_msfteur[, 1], xlab=NA, ylab=NA)
# par(new=TRUE)  # allow new plot on same chart
# plot(zoo_msfteur[, 2], xlab=NA, ylab=NA, yaxt="n", col="red")
# axis(side=4, col="red")  # second "y" axis on right
# mtext(colnames(zoo_msfteur)[1], side=2, padj=-6, line=-4)
# mtext(colnames(zoo_msfteur)[2], col="red", side=4, padj=-2, line=-3)
# title(main="EUR and MSFT", line=-1)  # add title
# legend("bottomright", legend=colnames(zoo_msfteur), 
#        lty=c(1, 1), lwd=c(2, 2), col=c("black", "red"), bty="n")

##########

# "x" axis with monthly ticks - doesn't work
# plot first ts wthout "x" axis
# plot(zoo_msfteur[, 1], xaxt="n", xlab=NA, ylab=NA)
# # add "x" axis with monthly ticks
# month.ticks <- unique(as.yearmon(index(zoo_eurusd)))
# axis(side=1, at=month.ticks, labels=format(month.ticks, "%b-%y"), tcl=-0.7)




ts_msft <- as.ts(zoo_msft)
class(ts_msft)
# rename colnames
colnames(ts_msft) <- paste0("MSFT.", colnames(ts_msft))
tail(ts_msft, 4)

library(timeSeries)
tser_msft <- as.timeSeries(zoo_msft)
class(ts_msft)
tail(tser_msft, 4)


