library(knitr)
opts_chunk$set(prompt=TRUE, eval=FALSE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=6, fig.height=5)
options(width=60, dev='pdf')
thm <- knit_theme$get("acid")
knit_theme$set(thm)
rm(list=ls())
baseenv()  # get base environment
globalenv()  # get global environment
environment()  # get current environment
class(environment())  # get environment class
n.globvar <- 1  # define variable in current environment
ls(environment())  # get objects in current environment

env.new <- new.env()  # create new environment
parent.env(env.new)  # get calling environment of new environment
assign("new_var1", 3, envir=env.new)  # assign Value to Name
env.new$new_var2 <- 11  # create object in new environment
ls(env.new)  # get objects in new environment
ls(environment())  # get objects in current environment
env.new$new_var1  # environments are subset like lists
env.new[['new_var1']]  # environments are subset like lists
rm(list=ls())
search()  # get search path for R objects
loadedNamespaces()
my_var <- list('flowers'=c('rose', 'daisy', 'tulip'),  # create a list
                'trees'=c('pine', 'oak', 'maple'))
my_var$trees
attach(my_var)
trees
search()  # get search path for R objects
detach(my_var)
rm(list=ls())
MyFunc <- function(my_var1=2, my_var2=1) {  # define function MyFunc
# default values can be specified in the argument list
  my_var1 + 2*my_var2
# the function returns the last evaluated statement
}  # end MyFunc
MyFunc(my_var1=3, my_var2=2)  # match arguments by name
MyFunc(3, 2)  # match arguments by position
MyFunc()  # use default values of arguments
# define function that uses enclosure environment
MyFunc <- function(my_var1=2, my_var2=1) {
# default values can be specified in the argument list
  my_var1 + 2*my_var2 + n.globvar
# the function returns the last evaluated statement
}  # end MyFunc
MyFunc(3, 2)  # n.globvar doesn't exist yet!
n.globvar <- 10
MyFunc(3, 2)  # now works
rm(list=ls())
par(oma=c(1, 1, 1, 1), mgp=c(2, 0.5, 0), mar=c(5, 1, 1, 1), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
set.seed(1121)  # reset random number generator
ts.rets <- diff(log(EuStockMarkets[, 1]))  # DAX returns
# define function CalcSkew to calculate the skew
CalcSkew <- function(ts.data=rnorm(1000)) {  # default is normal
# Calculates the skew of a time series of returns.
  i.num <- length(ts.data)  # number of observations
  n.mean <- mean(ts.data)
  n.sd <- sd(ts.data)
# the last statement is what is returned
  i.num*sum(((ts.data - n.mean)/n.sd)^3)/((i.num-1)*(i.num-2))
}  # end CalcSkew
# calculate skewness of DAX returns
CalcSkew(ts.data=ts.rets)  # match arguments by name
CalcSkew(ts.rets)  # match arguments by position
CalcSkew()  # use default value of arguments
CalcSkew  # show the function code
rm(list=ls())
catch_missing <- function(v.in1, v.in2=NA) {
# returns the sum of its two arguments
  if (is.numeric(v.in2)) {  # check if v.in2 was input
    v.in1 + v.in2  # v.in2 was input
  } else {
    v.in1  # v.in2 wasn't input
  }
}  # end catch_missing
catch_missing(1, 2)
catch_missing(5, 'a')
catch_missing <- function(v.in1, v.in2) {  # even more robust
  stopifnot((!missing(v.in1) && is.numeric(v.in1)) || 
       (!missing(v.in2) && is.numeric(v.in2)))
  if (!missing(v.in2) && is.numeric(v.in2)) {  # check if v.in2 is valid input
    v.in1 + v.in2  # v.in2 was input
  } else {
    v.in1  # v.in2 wasn't input
  }
}  # end catch_missing
catch_missing(5, 'a')
catch_missing('a')
rm(list=ls())
MyFunc <- function(my_var1, my_var2) {  # define function MyFunc
  2*my_var1  # just multiply first argument
}  # end MyFunc
MyFunc(3, 2)  # match arguments by position
MyFunc(3)  # second argument was never evaluated!
MyFunc <- function(my_var1, my_var2) {  # define function MyFunc
  cat(my_var1, '\n')  # write to output
  cat(my_var2)  # write to output
}  # end MyFunc
MyFunc(3, 2)  # match arguments by position
MyFunc(3)  # first argument written to output
rm(list=ls())
MySum <- function(my_var1=2, my_var2=1, ...) {  # define function MySum
# default values can be specified in the argument list
  my_var1 + 2*my_var2 + sum(...)
# the function returns the last evaluated statement
}  # end MySum
MySum(3, 2)  # match arguments by position
MySum(3, 2, 5, 8)  # extra arguments
MySum()  # use default value of arguments
str(paste)  # function 'paste' can take many arguments
paste('a', 'b', sep = ':')  # match arguments by name
paste('a', 'b', se = ':')  # partial name matching fails!
rm(list=ls())
n.globvar <- 1  # define a global variable
ls(environment())  # get all variables in environment
MyFunc <- function() {  # function for exploring environments
  n.locvar <- 1  # define a local variable
  cat('objects in parent environment:\t', 
      ls(parent.env(environment())), '\n')
  cat('objects in function environment:\t', 
      ls(environment()), '\n')
  cat('this is the parent environment:')
  parent.env(environment())  # return parent environment
}  # end MyFunc
MyFunc()
environment(MyFunc)
rm(list=ls())
n.globvar <- 1  # define a global variable
MyFunc <- function() {  # function for exploring scope
  n.locvar <- 1  # define a local variable
  new.globvar <<- 11  # define a global variable
  cat('objects in parent environment:\t', 
      ls(parent.env(environment())), '\n')
  cat('objects in function environment:\t', 
      ls(environment()), '\n')
  cat('this is n.globvar:\t', n.globvar, '\n')
  n.globvar <- 10  # re-define the global variable
  cat('this is re-defined n.globvar:\t', n.globvar, '\n')
}  # end MyFunc
MyFunc()
n.globvar  # global variable is restored
new.globvar  # new.globvar is preserved
n.locvar  # local variable is gone!
rm(list=ls())
n.globvar <- 1  # define a global variable
MyFunc <- function() {  # function for exploring scope
  cat('this is the global n.globvar:\t', n.globvar, '\n')
  n.globvar <- 10  # define local 'n.globvar' variable
  n.globvar <<- 2  # re-define the global variable
  cat('this is a local n.globvar:\t', n.globvar, '\n')
}  # end MyFunc
MyFunc()
n.globvar  # the global variable
rm(list=ls())
FibRec <- function(n.num) {
  if (n.num > 2) {
    fib.seq <- FibRec(n.num-1)  # recursion
    c(fib.seq, sum(tail(fib.seq, 2)))  # return this
  } else {
    c(1, 1)  # initialize and return
  }
}  # end FibRec
FibRec(10)
tail(FibRec(10), 2)
rm(list=ls())
SumDots <- function(my_var, ...) {  # define recursive function
# returns the sum of its argument list
  if (length(list(...)) == 0) {
    return(my_var)  # just one argument left
  } else {
    my_var + SumDots(...)  # sum remaining arguments
  }
}  # end SumDots
SumDots(1, 2, 3, 4)
rm(list=ls())
func_tional <- function(FuncArg) {  # function is input
# calculates statistic on random numbers
  set.seed(1)
  FuncArg(runif(1e4))  # apply the function name
}  # end func_tional
func_tional(mean)
func_tional(sd)
rm(list=ls())
func_tional <- function(func_arg, ...) {
# functional accepts function and additional '...' arguments
  func_arg(...)  # apply input function to '...' arguments
}  # end func_tional
func_tional(sum, 1, 2, 3)
rm(list=ls())
str(apply)  # get list of arguments
my_var <- matrix(6:1, nrow=2, ncol=3)  # create a matrix
my_var
# sum the rows and columns
row_sums <- apply(my_var, 1, sum)
col_sums <- apply(my_var, 2, sum)
m.totals <- cbind(c(sum(row_sums), row_sums), 
          rbind(col_sums, my_var))
dimnames(m.totals) <- list(c("col_sums", "row1", "row2"), 
                   c("row_sums", "col1", "col2", "col3"))
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
func_tional <- function(..., func_arg=function(x, y, z) {x+y+z}) {
# functional accepts function and additional '...' arguments
  func_arg(...)  # apply input function to '...' arguments
}  # end func_tional
func_tional(func_arg=sum, 2, 3, 4)
func_tional(2, 3, 4)
func_tional(2, 3, 4, 5)
func_tional(func_arg=function(x, y, z) {x*y*z}, 2, 3, 4)
rm(list=ls())
ts.rets <- 100*diff(log(EuStockMarkets[, 1]))  # DAX percent returns
library(moments)  # load package moments
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
rm(list=ls())
catch_missing <- function(v.in1, v.in2=NA) {
# returns the sum of its two arguments
  if (is.na(v.in2)) {  # check if v.in2 was input
    v.in1  # v.in2 wasn't input
  } else {
    v.in1 + v.in2  # v.in2 was input
  }
}  # end catch_missing
catch_missing(1, 2)
catch_missing(5)
rm(list=ls())
'+' (2, 3)  # infix operator applied using prefix syntax

n.globvar <- 1  # define a global variable
MyFunc <- function() {  # function for exploring scope
  n.locvar <- 1  # define a local variable
  new.globvar <<- 11  # define a global variable
  cat('objects in parent environment:\t', 
      ls(parent.env(environment())), '\n')
  cat('objects in function environment:\t', 
      ls(environment()), '\n')
  cat('this is n.globvar:\t', n.globvar, '\n')
  n.globvar <- 10  # re-define the global variable
  cat('this is re-defined n.globvar:\t', n.globvar, '\n')
}  # end MyFunc
MyFunc()
n.globvar  # global variable is restored
new.globvar  # new.globvar is preserved
n.locvar  # local variable is gone!
rm(list=ls())
library(zoo)  # load package zoo
# get all methods for generic function "cbind"
tail(methods("cbind"))

# show method of "cbind" applied to "zoo" objects
cbind.zoo

# get generic function methods applied to "zoo" objects
methods(class="zoo")[5:10]
rm(list=ls())
my.ts <- zoo(rnorm(4), order.by=(Sys.Date() + 0:3))
class(my.ts)
length(my.ts)

# coerce "zoo" object to new class "newts"
class(my.ts) <- "newts"
class(my.ts)

# define "length" method for class "newts"
length.newts <- function(in.ts) {
# "length" method for class" "newts"
  cat("getting length of object from newts class\n")
  length(unclass(in.ts))
}  # end length.newts

# apply new "length" method
length(my.ts)
rm(list=ls())
set.seed(1121)  # initialize random number generator
v.xvar <- seq(from=0.1, to=3.0, by=0.1)  # independent variable
v.yvar <- 3 + 2*v.xvar + rnorm(30)  # dependent variable plus noise
lm.simp <- lm(v.yvar ~ v.xvar)  # perform regression
summary(lm.simp)  # regression summary
rm(list=ls())
set.seed(1121)  # initialize random number generator
v.xvar <- seq(from=0.1, to=3.0, by=0.1)
v.yvar <- 3 + 2*v.xvar + rnorm(30)
lm.simp <- lm(v.yvar ~ v.xvar)  # perform regression
# set plot paramaters - margins and font scale
par(oma=c(1, 1, 1, 1))  # set outer margins
par(mgp=c(2, 0.5, 0))  # axis title and labels
par(mar=c(5, 3, 1, 1), cex.lab=0.8, 
    cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
plot(v.yvar ~ v.xvar)  # plot scatterplot
title(main="Simple Regression", line=-1)
abline(lm.simp, col="red")  # add reg line
