

library(knitr)
opts_chunk$set(prompt=TRUE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
thm <- knit_theme$get("acid")
knit_theme$set(thm)



rm(list=ls())
v.type <- c('rose', 'daisy', 'tulip')  # character vector
v.color <- c('red', 'white', 'yellow')  # character vector
v.price <- c(1.5, 0.5, 1.0)  # numeric vector
df.florist <- data.frame(v.type,  # create a data frame
                   v.color, v.price)
rownames(df.florist) <- c('flower1',  # assign rownames
                    'flower2', 'flower3')
df.florist
dim(df.florist)  # get dimension attribute
colnames(df.florist)  # get the colnames attribute
rownames(df.florist)  # get the rownames attribute
class(df.florist)  # get object class
class(df.florist$v.type)  # get column class
class(df.florist$v.price)  # get column class



v.type <- c('rose', 'daisy', 'tulip')  # character vector
v.color <- c('red', 'white', 'yellow')  # character vector
v.price <- c(1.5, 0.5, 1.0)  # numeric vector
df.florist <- data.frame(v.type,  # create a data frame
                   v.color, v.price)
rownames(df.florist) <- c('flower1',  # assign rownames
                    'flower2', 'flower3')
df.florist
df.florist[2, ]  # get second row
df.florist[2, 3]  # get second row and third column
df.florist[[3]]  # get third column
df.florist$v.color[3]  # get third row from column 'v.color'



v.type <- c('rose', 'daisy', 'tulip')  # character vector
v.color <- c('red', 'white', 'yellow')  # character vector
v.price <- c(1.5, 0.5, 1.0)  # numeric vector
df.florist <- data.frame(v.type,  # create a data frame
                   v.color, v.price)
rownames(df.florist) <- c('flower1',  # assign rownames
                    'flower2', 'flower3')
df.florist
str(df.florist)  # display the object structure
dim(cars)  # the cars data frame has 50 rows
head(cars, n=5)  # get first five rows
tail(cars, n=5)  # get last five rows



v.type <- c('rose', 'daisy', 'tulip')  # character vector
v.color <- c('red', 'white', 'yellow')  # character vector
v.price <- c(1.5, 0.5, 1.0)  # numeric vector
df.florist <- data.frame(v.type,  # create a data frame
                   v.color, v.price)
rownames(df.florist) <- c('flower1',  # assign rownames
                    'flower2', 'flower3')
df.florist
order(df.florist$v.price)  # permute on price
df.florist[order(df.florist$v.price), ]  # sort on price
df.florist[order(df.florist$v.color), ]  # sort on color
order(c(2, 1:4))  # there's a tie
order(c(2, 1:4), 1:5)  # there's a tie
# read sort() Examples



rm(list=ls())
as.numeric(c(1:3, "a"))  # NA from coercion
0/0  # NaN from ambiguous math
1/0  # Inf from divide by zero
is.na(c(NA, 0/0, 1/0))  # test for NA
is.nan(c(NA, 0/0, 1/0))  # test for NaN
NA*1:4  # create vector of Nas
v.na <- c(1, 2, NA, 4, NA, 5)  # create vector with some NA values
mean(v.na)  # returns NA, when NAs are input
mean(v.na, na.rm=TRUE)  # remove NAs from input data
v.na[!is.na(v.na)]  # delete the NA values
sum(!is.na(v.na))  # count non-NA values



rm(list=ls())
head(airquality)  # airquality data has some NAs
dim(airquality)
sum(!complete.cases(airquality))  # number of NAs
head(airquality[!complete.cases(airquality), ])  # display some NAs



rm(list=ls())
good.air <- airquality[complete.cases(airquality), ]  # remove NAs
dim(good.air)
head(good.air)  # NAs removed
library(zoo)  # load package zoo
good.air <- na.locf(airquality)  # replace NAs
dim(good.air)
head(good.air)  # NAs replaced



## library("MASS") or require("MASS")



## install.packages("AER")  # install AER
## library("AER")  # load AER



## install.packages("PerformanceAnalytics",
##            repos="http://R-Forge.R-project.org")



## packageDescription("Ecdat")  # get short description
## help(package="Ecdat")  # get documentation for package "Ecdat"
## data(package="Ecdat")  # show all datasets in package "Ecdat"
## library(Ecdat)  # load package Ecdat
## ls("package:Ecdat")  # list all objects in package "Ecdat"
## detach("package:Ecdat")  # remove Ecdat from search path



library("Ecdat")  # data sets for econometric analysis
class(Garch)
dim(Garch)  # daily currency prices
head(Garch[, -2])  # col 'dm' is Deutsch Mark
detach("package:Ecdat")  # remove Ecdat from search path



rm(list=ls())
baseenv()  # get base environment
globalenv()  # get global environment
environment()  # get current environment
class(environment())  # get environment class
n.globvar <- 1  # define variable in current environment
ls(environment())  # get objects in current environment

env.new <- new.env()  # create new environment
parent.env(env.new)  # get calling environment of new environment
assign("new.var1", 3, envir=env.new)  # assign Value to Name
env.new$new.var2 <- 11  # create object in new environment
ls(env.new)  # get objects in new environment
ls(environment())  # get objects in current environment
env.new$new.var1  # environments are subset like lists
env.new[['new.var1']]  # environments are subset like lists



rm(list=ls())
search()  # get search path for R objects
loadedNamespaces()
l.var <- list('flowers'=c('rose', 'daisy', 'tulip'),  # create a list
                'trees'=c('pine', 'oak', 'maple'))
l.var$trees
attach(l.var)
trees
search()  # get search path for R objects
detach(l.var)



rm(list=ls())
search()  # get search path for R objects
library("MASS")  # load package 'MASS'
search()  # get search path for R objects
ls("package:MASS")
detach("package:MASS")  # remove MASS from search path



rm(list=ls())
MyFunc <- function(n.var1=2, n.var2=1) {  # define function MyFunc
# default values can be specified in the argument list
  n.var1 + 2*n.var2
# the function returns the last evaluated statement
}  # end MyFunc
MyFunc(n.var1=3, n.var2=2)  # match arguments by name
MyFunc(3, 2)  # match arguments by position
MyFunc()  # use default values of arguments
# define function that uses enclosure environment
MyFunc <- function(n.var1=2, n.var2=1) {
# default values can be specified in the argument list
  n.var1 + 2*n.var2 + n.globvar
# the function returns the last evaluated statement
}  # end MyFunc
MyFunc(3, 2)  # n.globvar doesn't exist yet!
n.globvar <- 10
MyFunc(3, 2)  # now works



rm(list=ls())
ts.rets <- diff(log(EuStockMarkets))[, 1]  # DAX returns

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

getAnywhere(CalcSkew)  # display function


