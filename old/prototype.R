library(knitr)
opts_chunk$set(prompt=TRUE,comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
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
search()  # get search path for R objects
library(MASS)  # load package 'MASS'
search()  # get search path for R objects
select  # look at primitive function code from package 'MASS'
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
args(paste)  # function 'paste' can take many arguments
paste('a', 'b', sep = ':')  # match arguments by name
paste('a', 'b', se = ':')  # partial name matching fails!
rm(list=ls())
n.globvar <- 1  # define a global variable
ls(environment())  # get all variables in environment
MyFunc <- function() {  # function for exploring environments
  n.locvar <- 1  # define a local variable
  cat('objects in parent environment:', '\t', 
      ls(parent.env(environment())), '\n')
  cat('objects in function environment:', '\t', 
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
  cat('objects in parent environment:', '\t', 
      ls(parent.env(environment())), '\n')
  cat('objects in function environment:', '\t', 
      ls(environment()), '\n')
  cat('this is n.globvar:', '\t', n.globvar, '\n')
  n.globvar <- 10  # re-define the global variable
  cat('this is re-defined n.globvar:', '\t', n.globvar, '\n')
}  # end MyFunc
MyFunc()
n.globvar  # global variable is restored
new.globvar  # new.globvar is preserved
n.locvar  # local variable is gone!
rm(list=ls())
par(oma=c(1, 1, 1, 1), mgp=c(2, 1, 0), mar=c(5, 1, 1, 1), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
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
par(mgp=c(2, 1, 0))  # axis title and labels
par(mar=c(5, 3, 1, 1), cex.lab=0.8, 
    cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
plot(v.yvar ~ v.xvar)  # plot scatterplot
title(main="Simple Regression", line=-1)
abline(lm.simp, col="red")  # add reg line
