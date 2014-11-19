

library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
thm <- knit_theme$get("acid")
knit_theme$set(thm)



# create a list with two elements
list_var <- list(c('a', 'b'), 1:4)
list_var
c(class(list_var), typeof(list_var))
c(is.vector(list_var), is.list(list_var))
length(list_var)
# create named list
list_var <- list(first=c('a', 'b'), second=1:4)
list_var
names(list_var)
unlist(list_var)



list_var[2]  # get second element as sublist
list_var[[2]]  # get second element
list_var[[2]][3]  # get third element of second element
list_var[[c(2, 3)]]  # get third element of second element
list_var$second  # get second element
list_var$s  # get second element - partial name matching
list_var$second[3]  # get third element of second element



# create factor vector
fact_var <- factor(c('b', 'c', 'd', 'a', 'c', 'b'))
fact_var
fact_var[3]

attributes(fact_var)  # get factor attributes
levels(fact_var)  # get allowed values

table(fact_var)  # get contingency (frequency) table

is.vector(fact_var)
as.vector(fact_var)  # coerce factor to character vector



data_frame <- data.frame(  # create a data frame
                type=c('rose', 'daisy', 'tulip'),
                color=c('red', 'white', 'yellow'),
                price=c(1.5, 0.5, 1.0)
              )  # end data.frame
data_frame
dim(data_frame)  # get dimension attribute
colnames(data_frame)  # get the colnames attribute
rownames(data_frame)  # get the rownames attribute
class(data_frame)  # get object class
typeof(data_frame)  # data frames are lists
is.data.frame(data_frame)

class(data_frame$type)  # get column class
class(data_frame$price)  # get column class



data_frame[2, 3]  # get second row and third column
data_frame[[3]]  # get third column
data_frame$color[3]  # get third row from column 'color'



# don't coerce character vectors to factors
options(stringsAsFactors=FALSE)
options("stringsAsFactors")
default.stringsAsFactors()

data_frame <- data.frame(  # create a data frame
                type=c('rose', 'daisy', 'tulip'),
                color=c('red', 'white', 'yellow'),
                price=c(1.5, 0.5, 1.0),
                row.names=c('flower1', 'flower2', 'flower3'),
                stringsAsFactors=FALSE
              )  # end data.frame
data_frame
class(data_frame$type)  # get column class
class(data_frame$price)  # get column class



str(data_frame)  # display the object structure
dim(cars)  # the cars data frame has 50 rows
head(cars, n=5)  # get first five rows
tail(cars, n=5)  # get last five rows



type <- c('rose', 'daisy', 'tulip')  # character vector
color <- c('red', 'white', 'yellow')  # character vector
price <- c(1.5, 0.5, 1.0)  # numeric vector
# create a data frame
data_frame <- data.frame(type, color, price)
# assign rownames
rownames(data_frame) <- c('flower1', 'flower2', 'flower3')
sort_data <- sample(1:6)  # permute data
sort_data
sort(sort_data)  # sorted data
order(sort_data)  # permution index
sort_data[order(sort_data)]  # permution index
order(data_frame$price)  # permute on price
data_frame[order(data_frame$price), ]  # sort on price
data_frame[order(data_frame$color), ]  # sort on color
order(c(2, 1:4))  # there's a tie
order(c(2, 1:4), 1:5)  # there's a tie
# read sort() Examples



# ?iris  # get information on iris
dim(iris)
head(iris, 2)
colnames(iris)
unique(iris$Species)  # extract list of unique elements of iris
class(unique(iris$Species))
# find which columns of iris are numeric
sapply(iris, is.numeric)
# calculate means of iris columns
sapply(iris, mean)  # returns NA for Species



# ?mtcars  # get information on mtcars - data from 1974 Motor Trend magazine
# mpg   Miles/(US) gallon
# qsec   1/4 mile time
# hp   Gross horsepower
# wt	 Weight (lb/1000)
# cyl   Number of cylinders
dim(mtcars)
head(mtcars, 2)
colnames(mtcars)
head(rownames(mtcars), 3)
unique(mtcars$cyl)  # extract list of car cylinders
sapply(mtcars, mean)  # calculate means of mtcars columns



library(MASS)
# ?Cars93  # get information on Cars93
dim(Cars93)
head(colnames(Cars93))
# head(Cars93, 2)
unique(Cars93$Type)  # extract list of car types
# sapply(Cars93, mean)  # calculate means of Cars93 columns
# plot histogram of Highway MPG using the Freedman-Diaconis rule
hist(Cars93$MPG.highway, col="lightblue1", 
     main="Distance per Gallon 1993", xlab="Highway MPG", breaks="FD")



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



# define a function with two arguments
test_func <- function(first_arg, second_arg) {  # body
  first_arg + second_arg  # returns last evaluated statement
}  # end test_func

test_func(1, 2)  # apply the function
args(test_func)  # display argument

# define function that uses variable from enclosure environment
test_func <- function(first_arg, second_arg) {
  first_arg + second_arg + glob_var
}  # end test_func

test_func(3, 2)  # error - glob_var doesn't exist yet!
glob_var <- 10  # create glob_var
test_func(3, 2)  # now works



test_func <- function(first_arg=2, second_arg=1) {
# default values can be specified in the argument list
  first_arg + 2*second_arg
}  # end test_func

test_func(first_arg=3, second_arg=2)  # bind by name
test_func(first=3, second=2)  # partial name matching
test_func(3, 2)  # bind by position
test_func(second_arg=2, 3)  # mixed binding
test_func()  # use default values of arguments
test_func(3, 2, 1)  # too many arguments



# define a function that returns invisibly
test_func <- function(arg_var) {
  if (!is.numeric(arg_var)) {
    warning(paste("argument", arg_var, "isn't numeric"))
    return(NULL)
  }
  2*arg_var
}  # end test_func

test_func(2)
test_func("hello")



# define a function that returns invisibly
return_invisible <- function(arg_var) {
  invisible(arg_var)
}  # end return_invisible

return_invisible(2)

glob_var <- return_invisible(2)
glob_var



str(sum)  # "sum" accepts multiple arguments
# passing a list of arguments to "sum" produces an error
sum(list(1, 2, 3, 4))
# do.call passes the list elements into "sum" one by one
do.call(sum, list(1, 2, 3, 4))

num_list <- list(1, 2, 3, 4)  # create numeric list
do.call(rbind, num_list)  # returns single column matrix
do.call(cbind, num_list)  # returns single row matrix



rm(list=ls())
fibo_nacci <- function(seq_length) {
  if (seq_length > 2) {
    fib.seq <- fibo_nacci(seq_length-1)  # recursion
    c(fib.seq, sum(tail(fib.seq, 2)))  # return this
  } else {
    c(1, 1)  # initialize and return
  }
}  # end fibo_nacci
fibo_nacci(10)
tail(fibo_nacci(10), 2)



rets_series <- diff(log(EuStockMarkets[, 1]))  # DAX returns

# define function calc_skew for calculating the skew
calc_skew <- function(time_series=rnorm(1000)) {  # default is normal
# Calculates the skew of a time series of returns.
  len_data <- length(time_series)  # number of observations
# normalize time_series
  time_series <- (time_series - mean(time_series))/sd(time_series)
# calculate skew - last statement is automatically returned
  len_data*sum(time_series^3)/((len_data-1)*(len_data-2))
}  # end calc_skew

# calculate skewn of DAX returns
calc_skew(time_series=rets_series)  # bind arguments by name
calc_skew(rets_series)  # bind arguments by position
calc_skew()  # use default value of arguments



calc_skew  # show the function code

getAnywhere(calc_skew)  # display function



rm(list=ls())
lazy_func <- function(arg1, arg2) {  # define function lazy_func
  2*arg1  # just multiply first argument
}  # end lazy_func
lazy_func(3, 2)  # bind arguments by position
lazy_func(3)  # second argument was never evaluated!
lazy_func <- function(arg1, arg2) {  # define function lazy_func
  cat(arg1, '\n')  # write to output
  cat(arg2)  # write to output
}  # end lazy_func
lazy_func(3, 2)  # bind arguments by position
lazy_func(3)  # first argument written to output



rm(list=ls())
match_dots <- function(arg1=2, arg2=1, ...) {  # define function match_dots
# default values can be specified in the argument list
  arg1 + 2*arg2 + sum(...)
# the function returns the last evaluated statement
}  # end match_dots
match_dots(3, 2)  # match arguments by position
match_dots(3, 2, 5, 8)  # extra arguments
match_dots()  # use default value of arguments
str(paste)  # function 'paste' can take many arguments
paste('a', 'b', sep = ':')  # bind arguments by name
paste('a', 'b', se = ':')  # partial name matching fails!



sum_dots <- function(arg_var, ...) {  # define recursive function
# returns the sum of its argument list
  if (missing(...)) {  # check if dots are empty
    return(arg_var)  # just one argument left
  } else {
    arg_var + sum_dots(...)  # sum remaining arguments
  }  # end if
}  # end sum_dots
sum_dots(1, 2, 3, 4)



getOption("repos")  # get default package source
.libPaths()  # get package save directory



## install.packages("AER")  # install "AER" from CRAN
## # install "PerformanceAnalytics" from R-Forge
## install.packages(pkgs="PerformanceAnalytics",  # name
##            lib="C:/Users/Jerzy/Downloads",  # directory
##            repos="http://R-Forge.R-project.org")  # source



pack_info <- installed.packages()  # matrix of packages
dim(pack_info)
# get a few package names and their versions
pack_info[sample(x=1:100, 5), c("Package", "Version")]
pack_info["xts", ]  # get info for package "xts"



## library("MASS")
## # or
## require("MASS")



## library()  # list all packages installed on the system
## search()  # list all loaded packages on search path
## 
## # get documentation for package "Ecdat"
## packageDescription("Ecdat")  # get short description
## help(package="Ecdat")  # load help page
## library(Ecdat)  # load package "Ecdat"
## data(package="Ecdat")  # list all datasets in "Ecdat"
## ls("package:Ecdat")  # list all objects in "Ecdat"
## detach("package:Ecdat")  # remove Ecdat from search path



library("Ecdat")  # load econometric data sets
class(Garch)  # Garch is a data frame from "Ecdat"
dim(Garch)  # daily currency prices
head(Garch[, -2])  # col 'dm' is Deutsch Mark
detach("package:Ecdat")  # remove Ecdat from search path



rm(list=ls())
search()  # get search path for R objects
library("MASS")  # load package "MASS"
head(ls("package:MASS"))  # list some objects in "MASS"
detach("package:MASS")  # remove "MASS" from search path



loadedNamespaces()  # get names of loaded namespaces

search()  # get search path for R objects


