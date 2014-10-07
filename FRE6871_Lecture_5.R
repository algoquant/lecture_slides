

library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
thm <- knit_theme$get("acid")
knit_theme$set(thm)



rm(list=ls())
FuncStat <- function(func_arg) {  # function is input
# calculates statistic on random numbers
  set.seed(1)
  func_arg(runif(1e4))  # apply the function name
}  # end FuncStat
FuncStat(mean)
FuncStat(sd)



rm(list=ls())
FuncDots <- function(FuncIn, ...) {
# functional accepts function and additional '...' arguments
  FuncIn(...)  # apply input function to '...' arguments
}  # end FuncDots
FuncDots(sum, 1, 2, 3)



rm(list=ls())
str(apply)  # get list of arguments
my_var <- matrix(6:1, nrow=2, ncol=3)  # create a matrix
my_var
# sum the rows and columns
row.sums <- apply(my_var, 1, sum)
col.sums <- apply(my_var, 2, sum)
m.totals <- cbind(c(sum(row.sums), row.sums), 
          rbind(col.sums, my_var))
dimnames(m.totals) <- list(c("col.sums", "row1", "row2"), 
                   c("row.sums", "col1", "col2", "col3"))
m.totals



rm(list=ls())
str(apply)  # get list of arguments
my_var <- matrix(sample(12), nrow=3, ncol=4)  # create a matrix
my_var
apply(my_var, 2, sort)  # sort matrix columns
apply(my_var, 2, sort, decreasing=TRUE)  # sort decreasing order



my_var[2, 2] <- NA  # introduce NA value
my_var
# calculate median of columns
apply(my_var, 2, median)
# calculate median of columns with na.rm=TRUE
apply(my_var, 2, median, na.rm=TRUE)



rm(list=ls())
FuncDots <- function(..., FuncIn=function(x, y, z) {x+y+z}) {
# functional accepts function and additional '...' arguments
  FuncIn(...)  # apply input function to '...' arguments
}  # end FuncDots
FuncDots(FuncIn=sum, 2, 3, 4)
FuncDots(2, 3, 4)
FuncDots(2, 3, 4, 5)
FuncDots(FuncIn=function(x, y, z) {x*y*z}, 2, 3, 4)



rm(list=ls())
ts.rets <- 100*diff(log(EuStockMarkets[, 1]))  # DAX percent returns
library("moments")  # load library"moments"
str(moment)  # get list of arguments
moment(x=ts.rets, order=3)  # the moment function
v.orders <- as.matrix(1:4)  # 4x1 matrix of moment orders
# anonymous function allows looping over function parameters
apply(X=v.orders, MARGIN=1, 
      FUN=function(n.order) {moment(x=ts.rets, order=n.order)})
# another way of passing data into moment() function
apply(X=v.orders, MARGIN=1, FUN=moment, x=ts.rets)



rm(list=ls())
iris.list <- as.list(iris[1:5, 1:3])  # create list
iris.list
lapply(iris.list, mean)  # compute list of means of list elements



# compute vector of means of list elements
sapply(iris.list, mean)

# create a matrix
my_var <- matrix(sample(12), nrow=3, ncol=4)
# calculate row sums
apply(my_var, 1, sum)

# now the same calculation using sapply
sapply(1:nrow(my_var), function(n.row) {  # anonymous function
    sum(my_var[n.row, ])
  }  # end anonymous function
)  # end sapply



rm(list=ls())
FuncPower <- function(n.exp) {  # wrapper function
# a power function factory
  function(n.arg) {  # anonymous closure
    n.arg^n.exp
  }
}  # end FuncPower
FuncSquare <- FuncPower(2)  # define square power
FuncSquare(4)
FuncCube <- FuncPower(3)  # define cube power
FuncCube(2)
FuncCubeRoot <- FuncPower(1/3)  # define cube root
FuncCubeRoot(8)



rm(list=ls())
MuteCounter <- function() {
# counter function with mutable state
  i.count <- 0  # initialize counter
  cat('counter = ', i.count)
  function() {  # return anonymous advance function
    i.count <<- i.count + 1  # advance counter
    cat('counter = ', i.count)
  }  # end advance function
}  # end MuteCounter
CounterOne <- MuteCounter()  # create new counter
CounterOne()  # advance counter
CounterOne()  # advance counter
CounterTwo <- MuteCounter()  # create another counter
CounterTwo()  # advance counter two
CounterOne()  # advance counter one
CounterTwo()  # advance counter two
CounterOne()  # advance counter one



rm(list=ls())
RandomSeed <- function(seed) {  # seed must be an integer
# Returns pseudo-random generating function based on logistic map
# the formal argument 'seed' exists in the evaluation environment of RandomSeed
  pseudo.random <- as.numeric(paste('0.', seed, sep=''))  # initialize
  RandomVector <- function(n.rand=1) {  # assign function name for recursion
# Returns a vector of pseudo-random numbers of length n.rand
    pseudo.random <<- 4*pseudo.random*(1 - pseudo.random)  # logistic map
    if(n.rand == 1) {
      return(pseudo.random)
    } else {
      return(c(pseudo.random, RandomVector(n.rand - 1)))
    }
  }
}  # end RandomSeed

PseudoRandom <- RandomSeed(88)  # set seed
PseudoRandom(10)  #  calculate vector of 10 pseudo-random numbers
ls(environment(PseudoRandom))  # list objects in scope of PseudoRandom



rm(list=ls())
# the super-assignment operator '<<-' adjusts the balance
# 'balance' exists in OpenAccount evaluation environment
# bank account example (from Venables) demonstrates mutable states
# 'balance' is persistent between function calls
OpenAccount <- function(balance) {
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
    get.balance = function() {  # get balance
      cat("Your current balance is:", balance, "\n")
    }  # end get.balance
  )  # end list
}  # end OpenAccount



# perform account operations
# open an account with 100 deposit
my.account <- OpenAccount(100)
ls(my.account)  # my.account is a list
# add my.account to search path
attach(my.account)
withdraw(30)  # withdrawal to buy groceries
deposit(100)  # deposit paycheck to account
withdraw(200)  # withdrawal to buy Gucci bag
get.balance()  # get account balance

# list objects in scope of get.balance
ls(environment(get.balance))

detach(my.account)  # remove my.account from search path



par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
library(tseries)  # load package tseries
suppressWarnings(  # load MSFT data
  zoo_msft <- get.hist.quote(instrument="MSFT", 
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
suppressWarnings(  # load EUR/USD data
  zoo.eurusd <- get.hist.quote(
    instrument="EUR/USD", provider="oanda",
    start=Sys.Date()-365, 
    end=Sys.Date(), 
    origin="1970-01-01")
  )  # end suppressWarnings
# bind and scrub data
zoo_msfteur <- merge(zoo.eurusd, 
               zoo_msft[, "Close"])
colnames(zoo_msfteur) <- c("EURUSD", "MSFT")
zoo_msfteur <- 
  zoo_msfteur[complete.cases(zoo_msfteur),]
### plot with two "y" axes
par(las=1)  # set text printing to "horizontal"
# plot first ts
plot(zoo_msfteur[, 1], xlab=NA, ylab=NA)
# set range for second "y" axis
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
# month.ticks <- unique(as.yearmon(index(zoo.eurusd)))
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


