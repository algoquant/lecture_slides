library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
options(digits=3)
thm <- knit_theme$get("acid")
knit_theme$set(thm)
# single numbers are vectors of length 1
1
# character strings are vectors of length 1
"a"
# strings without quotes are variable names
a  # variable "a" doesn't exist
# list elements can have different mode
list(aa=c('a', 'b'), bb=1:5)
data.frame(aa=c('a', 'b'), bb=1:2)
is.atomic(data.frame(aa=c('a', 'b'), bb=1:2))
is.recursive(data.frame(aa=c('a', 'b'), bb=1:2))
my_var <- "hello"
c(typeof(my_var), mode(my_var), class(my_var))

my_var <- 1:5
c(typeof(my_var), mode(my_var), class(my_var))

my_var <- runif(5)
c(typeof(my_var), mode(my_var), class(my_var))

my_var <- matrix(1:10, 2, 5)
c(typeof(my_var), mode(my_var), class(my_var))

my_var <- matrix(runif(10), 2, 5)
c(typeof(my_var), mode(my_var), class(my_var))

my_var <- list(aa=c('a', 'b'), bb=1:5)
c(typeof(my_var), mode(my_var), class(my_var))

my_var <- data.frame(aa=c('a', 'b'), bb=1:2)
c(typeof(my_var), mode(my_var), class(my_var))
my_var <- matrix(1:10, 2)
c(typeof(my_var), mode(my_var), class(my_var))

my_var <- vector(mode="numeric", length=10)
c(typeof(my_var), mode(my_var), class(my_var))

my_var <- vector(mode="integer", length=10)
c(typeof(my_var), mode(my_var), class(my_var))
attributes(5:10)  # a simple vector has no attributes
my_var <- c(pi=pi, euler=exp(1), gamma=-digamma(1))
attributes(my_var)  # named vector has "names" attribute
my_var <- 1:10
is.vector(my_var)  # is the object a vector?
attributes(my_var) <- list(my_attr="foo")
my_var
is.vector(my_var)  # is the object a vector?
my_var <- 0
attributes(my_var) <- list(class="Date")
my_var  # "Date" object
structure(0, class="Date")  # "Date" object
my_var <- matrix(runif(10), 2, 5)
class(my_var)  # has implicit class
attributes(my_var)  # but no explicit "class" attribute
c(typeof(my_var), mode(my_var), class(my_var))
class(my_var) <- "my_class"  # assign explicit "class" attribute
class(my_var)  # has explicit "class"
attributes(my_var)  # has explicit "class" attribute
is.matrix(my_var)  # is the object a matrix?
is.vector(my_var)  # is the object a vector?
attributes(unclass(my_var))
my_var <- 1:5
c(typeof(my_var), mode(my_var), class(my_var))
mode(my_var) <- "character"  # coerce to "character"
my_var
c(typeof(my_var), mode(my_var), class(my_var))
my_var <- as.character(1:5)  # explicitly coerce to "character"
c(typeof(my_var), mode(my_var), class(my_var))
mat_rix <- matrix(1:10, 2, 5)  # create matrix
mat_rix <- as.character(mat_rix)  # explicitly coerce to "character"
c(typeof(mat_rix), mode(mat_rix), class(mat_rix))
# coercion converted matrix to vector
c(is.matrix(mat_rix), is.vector(mat_rix))
as.logical(0:3)  # explicit coercion to "logical"
as.numeric(c(FALSE, TRUE, TRUE, TRUE))
c(1:3, 'a')  # implicit coercion to "character"
as.numeric(c(1:3, 'a'))  # explicit coercion to "numeric"
# create factor vector
fac_tor <- factor(c('b', 'c', 'd', 'a', 'c', 'b'))
fac_tor
fac_tor[3]
attributes(fac_tor)  # get factor attributes
levels(fac_tor)  # get allowed values
as.numeric(fac_tor)  # get encoding vector
is.vector(fac_tor)
as.factor(1:5)  # coerce vector to factor
as.vector(as.factor(1:5))  # coerce factor to character vector
fac_tor
levels(fac_tor)  # get allowed values
unique(fac_tor)  # get unique elements
# get contingency (frequency) table
table(fac_tor)
# get contingency table using sapply
sapply(levels(fac_tor), 
 function(le_vel) {
   sum(fac_tor==le_vel)
 })  # end sapply
library(microbenchmark)
str(findInterval)
# get index of the element of "vec" that matches 5
findInterval(x=5, vec=c(3, 5, 7))
match(5, c(3, 5, 7))
# no exact match
findInterval(x=6, vec=c(3, 5, 7))
match(6, c(3, 5, 7))
# indices of "vec" that match elements of "x"
findInterval(x=1:8, vec=c(3, 5, 7))
# return only indices of inside intervals
findInterval(x=1:8, vec=c(3, 5, 7), 
       all.inside=TRUE)
# make rightmost interval inclusive
findInterval(x=1:8, vec=c(3, 5, 7), 
       rightmost.closed=TRUE)
# named numeric vector of breakpoints
brea_ks <- c("freezing"=0, "very_cold"=30,
       "cold"=50, "pleasant"=60,
       "warm"=80, "hot"=90)
brea_ks
tempe_ratures <- runif(10, min=10, max=100)
feels_like <- names(
  brea_ks[findInterval(x=tempe_ratures,
                 vec=brea_ks)])
names(tempe_ratures) <- feels_like
tempe_ratures
library(microbenchmark)
foo <- sample(0:6) + 0.1
foo
cut(x=foo, breaks=c(2, 4, 6, 8))
rbind(foo, cut(x=foo, breaks=c(2, 4, 6, 8)))
# cut() replicates findInterval()
cut(x=1:8, breaks=c(3, 5, 7), labels=1:2,
    right=FALSE)
findInterval(x=1:8, vec=c(3, 5, 7))
# findInterval() is a compiled function, so it's faster than cut()
vec_tor <- rnorm(1000)
summary(microbenchmark(
  find_interval=
    findInterval(x=vec_tor, vec=c(3, 5, 7)),
  cuut=
    cut(x=vec_tor, breaks=c(3, 5, 7)),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
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
str(plot)  # dots for additional plot parameters
bind_dots <- function(in_put, ...) {
  paste0("in_put=", in_put, 
 ", dots=", paste(..., sep=", "))
}  # end bind_dots
bind_dots(1, 2, 3)  # "in_put" bound by position
bind_dots(2, in_put=1, 3)  # "in_put" bound by name
bind_dots(1, 2, 3, foo=10)  # named argument bound to dots
bind_dots <- function(arg1, arg2, ...) {
  arg1 + 2*arg2 + sum(...)
}  # end bind_dots
bind_dots(3, 2)  # bind arguments by position
bind_dots(3, 2, 5, 8)  # extra arguments bound to dots
str(sum)  # dots before other arguments
sum(1, 2, 3)  # dots bind before other arguments
sum(1, 2, NA, 3, na.rm=TRUE)
bind_dots <- function(..., in_put) {
  paste0("in_put=", in_put, 
 ", dots=", paste(..., sep=", "))
}  # end bind_dots
# arguments after dots must be bound by full name
bind_dots(1, 2, 3, in_put=10)
bind_dots(1, 2, 3, in_put=10, foo=4)  # dots bound
bind_dots(1, 2, 3)  # "in_put" not bound
bind_dots <- function(..., in_put=10) {
  paste0("in_put=", in_put, 
 ", dots=", paste(..., sep=", "))
}  # end bind_dots
bind_dots(1, 2, 3)  # "in_put" not bound, but has default
# wrapper for mean() with default na.rm=TRUE
my_mean <- function(x, na.rm=TRUE, ...) {
  mean(x=x, na.rm=na.rm, ...)
}  # end my_mean
foo <- sample(c(1:10, NA, rep(0.1, t=5)))
mean(c(foo, NA))
mean(c(foo, NA), na.rm=TRUE)
my_mean(c(foo, NA))
my_mean(c(foo, NA), trim=0.4)  # pass extra argument
# wrapper for saving data into default directory
save_data <- function(..., 
              file=stop("error: no file name"),
              my_dir="C:/Develop/data") {
# create file path
  file <- file.path(my_dir, file)
  save(..., file=file)
}  # end save_data
foo <- 1:10
save_data(foo, file="scratch.Rdata")
save_data(foo, file="scratch.Rdata", my_dir="C:/Develop")
# wrapper for testing negative arguments
stop_if_neg <- function(in_put) {
  if(!is.numeric(in_put) || in_put<0)
    stop("argument not numeric or negative")
}  # end stop_if_neg
# wrapper for sqrt()
my_sqrt <- function(in_put) {
  stop_if_neg(in_put)
  sqrt(in_put)
}  # end my_sqrt
my_sqrt(2)
my_sqrt(-2)
my_sqrt(NA)
# func_tional accepts function name and additional argument
func_tional <- function(func_name, in_put) {
# produce function name from argument
  func_name <- match.fun(func_name)
# execute function call
  func_name(in_put)
}  # end func_tional
func_tional(sqrt, 4)
str(sum)  # sum() accepts multiple arguments
# func_tional can't accept indefinite number of arguments
func_tional(sum, 1, 2, 3)
# func_tional accepts function name and dots '...' argument
func_tional <- function(func_name, ...) {
  func_name <- match.fun(func_name)
  func_name(...)  # execute function call
}  # end func_tional
func_tional(sum, 1, 2, 3)
func_tional(sum, 1, 2, NA, 4, 5)
func_tional(sum, 1, 2, NA, 4, 5, na.rm=TRUE)
# function with three arguments and dots '...' arguments
my_func <- function(in_put, param1, param2, ...) {
  c(input=in_put, param1=param1, param2=param2, 
dots=c(...))
}  # end my_func
my_func(1, 2, 3, param2=4, param1=5)
func_tional(my_func, 1, 2, 3, param2=4, param1=5)
func_tional(my_func, 1, 2, 3, 4, 5)
# simple anonymous function
(function(x) (x + 3)) (10)
# anonymous function passed to func_tional
func_tional(func_name=(function(x) (x + 3)), 5)
# anonymous function is default value
func_tional <- 
  function(..., func_name=function(x, y, z) {x+y+z}) {
    func_name <- match.fun(func_name)
    func_name(...)  # execute function call
}  # end func_tional
func_tional(2, 3, 4)  # use default func_name
func_tional(2, 3, 4, 5)
# func_name bound by name
func_tional(func_name=sum, 2, 3, 4, 5)
# pass anonymous function to func_name
func_tional(func_name=function(x, y, z) {x*y*z}, 
    2, 3, 4)
str(sum)  # sum() accepts multiple arguments
# sum() can't accept list of arguments
sum(list(1, 2, 3))
str(do.call)  # "what" argument is a function
# do.call passes list elements into "sum" individually
do.call(sum, list(1, 2, 3))
do.call(sum, list(1, 2, NA, 3))
do.call(sum, list(1, 2, NA, 3, na.rm=TRUE))
# func_tional() accepts list with function name and arguments
func_tional <- function(list_arg) {
# produce function name from argument
  func_name <- match.fun(list_arg[[1]])
# execute function call uing do.call()
  do.call(func_name, list_arg[-1])
}  # end func_tional
arg_list <- list("sum", 1, 2, 3)
func_tional(arg_list)
rm(list=ls())
str(apply)  # get list of arguments
# create a matrix
mat_rix <- matrix(6:1, nrow=2, ncol=3)
mat_rix
# sum the rows and columns
row_sums <- apply(mat_rix, 1, sum)
col_sums <- apply(mat_rix, 2, sum)
mat_rix <- cbind(c(sum(row_sums), row_sums), 
          rbind(col_sums, mat_rix))
dimnames(mat_rix) <- list(c("col_sums", "row1", "row2"), 
                 c("row_sums", "col1", "col2", "col3"))
mat_rix
str(apply)  # get list of arguments
mat_rix <- matrix(sample(12), nrow=3, ncol=4)  # create a matrix
mat_rix
apply(mat_rix, 2, sort)  # sort matrix columns
apply(mat_rix, 2, sort, decreasing=TRUE)  # sort decreasing order
mat_rix[2, 2] <- NA  # introduce NA value
mat_rix
# calculate median of columns
apply(mat_rix, 2, median)
# calculate median of columns with na.rm=TRUE
apply(mat_rix, 2, median, na.rm=TRUE)
rm(list=ls())
# DAX percent returns
dax_rets <- 100*diff(log(EuStockMarkets[, 1]))
library("moments")  # load library"moments"
str(moment)  # get list of arguments
# apply moment function
moment(x=dax_rets, order=3)
# 4x1 matrix of moment orders
moment_orders <- as.matrix(1:4)
# anonymous function allows looping over function parameters
apply(X=moment_orders, MARGIN=1, 
      FUN=function(moment_order) {
  moment(x=dax_rets, order=moment_order)
}  # end anonymous function
      )  # end apply

# another way of passing parameters into moment() function
apply(X=moment_orders, MARGIN=1, FUN=moment, 
      x=dax_rets)
str(moment)  # argument list of moment()
str(sapply)  # argument list of sapply()
# calculate moments from 1 to 4
sapply(X=1:4, FUN=moment, x=dax_rets)
# function with three arguments
my_func <- function(in_put, param1, param2) {
  c(input=in_put, param1=param1, param2=param2)
}  # end my_func
my_func(1, 2, 3)
sapply(X=1:4, FUN=my_func, param1=2, param2=3)
sapply(X=1:4, FUN=my_func, in_put=1, param1=2)
sapply(X=1:4, FUN=my_func, in_put=1, param2=3)
sapply(iris[, -5], mean)  # vector of means of numeric columns
lapply(iris[, -5], mean)  # list of means of numeric columns
unlist(lapply(iris,  # lapply using anonymous function
      function(co_lumn) {
        if (is.numeric(co_lumn)) mean(co_lumn)
      }  # end anonymous function
      )  # end sapply
       )  # end unlist
unlist(sapply(iris, function(co_lumn) {
  if (is.numeric(co_lumn)) mean(co_lumn)}))
sapply(6:10, sqrt)  # sapply on vector
sapply(list(6, 7, 8, 9, 10), sqrt)  # sapply on list

# calculate means of iris data frame columns
sapply(iris, mean)  # returns NA for Species

# create a matrix
mat_rix <- matrix(sample(100), ncol=4)
# calculate column means using apply
apply(mat_rix, 2, mean)

# calculate column means using sapply, with anonymous function
sapply(1:ncol(mat_rix), 
       function(col_index) {  # anonymous function
 mean(mat_rix[, col_index])
  }  # end anonymous function
)  # end sapply
# vectors form columns of matrix returned by sapply
sapply(2:4, function(num) c(el1=num, el2=2*num))
# vectors of different lengths returned as list
sapply(2:4, function(num) 1:num)
# vapply is similar to sapply
vapply(2:4, function(num) c(el1=num, el2=2*num), 
       FUN.VALUE=c(row1=0, row2=0))
# vapply produces an error if it can't simplify
vapply(2:4, function(num) 1:num, 
       FUN.VALUE=c(row1=0, row2=0))
