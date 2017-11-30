library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
options(digits=3)
thm <- knit_theme$get("acid")
knit_theme$set(thm)
# verify that rtools are working properly:
devtools::find_rtools()
devtools::has_devel()

# load package Rcpp
library(Rcpp)
# get documentation for package Rcpp
# get short description
packageDescription("Rcpp")
# load help page
help(package="Rcpp")
# list all datasets in "Rcpp"
data(package="Rcpp")
# list all objects in "Rcpp"
ls("package:Rcpp")
# remove Rcpp from search path
detach("package:Rcpp")
# define Rcpp function
Rcpp::cppFunction("
  int times_two(int x)
    { return 2 * x;}
  ")  # end cppFunction
# run Rcpp function
times_two(3)
# source Rcpp functions from file
Rcpp::sourceCpp(file="C:/Develop/R/lecture_slides/scripts/rcpp_mult.cpp")
# multiply two numbers
rcpp_mult(2, 3)
rcpp_mult(1:3, 6:4)
# multiply two vectors
rcpp_mult_vec(2, 3)
rcpp_mult_vec(1:3, 6:4)
# define Rcpp function with loop
Rcpp::cppFunction("
double inner_mult(NumericVector x, NumericVector y) {
int x_size = x.size();
int y_size = y.size();
if (x_size != y_size) {
    return 0;
  } else {
    double total = 0;
    for(int i = 0; i < x_size; ++i) {
total += x[i] * y[i];
  }
  return total;
  }
}")  # end cppFunction
# run Rcpp function
inner_mult(1:3, 6:4)
inner_mult(1:3, 6:3)
# define Rcpp Sugar function with loop
Rcpp::cppFunction("
double inner_mult_sugar(NumericVector x, NumericVector y) {
  return sum(x * y);
}")  # end cppFunction
# run Rcpp Sugar function
inner_mult_sugar(1:3, 6:4)
inner_mult_sugar(1:3, 6:3)
# define R function with loop
inner_mult_r <- function(x, y) {
    to_tal <- 0
    for(i in 1:NROW(x)) {
to_tal <- to_tal + x[i] * y[i]
    }
    to_tal
}  # end inner_mult_r
# run R function
inner_mult_r(1:3, 6:4)
inner_mult_r(1:3, 6:3)
# compare speed of Rcpp and R
library(microbenchmark)
summary(microbenchmark(
  pure_r=inner_mult_r(1:10000, 1:10000),
  inner_r=1:10000 %*% 1:10000,
  r_cpp=inner_mult(1:10000, 1:10000),
  r_cpp_sugar=inner_mult_sugar(1:10000, 1:10000),
  times=10))[, c(1, 4, 5)]
# calculate uniformly distributed pseudo-random sequence
uni_form <- function(see_d, len_gth=10) {
  out_put <- numeric(len_gth)
  out_put[1] <- see_d
  for (i in 2:len_gth) {
    out_put[i] <- 4*out_put[i-1]*(1-out_put[i-1])
  }  # end for
  acos(1-2*out_put)/pi
}  # end uni_form

# source Rcpp functions from file
Rcpp::sourceCpp(file="C:/Develop/R/lecture_slides/scripts/uni_form.cpp")
# microbenchmark Rcpp code
library(microbenchmark)
summary(microbenchmark(
  pure_r=runif(1e5),
  r_loop=uni_form(0.3, 1e5),
  r_cpp=uniform_rcpp(0.3, 1e5),
  times=10))[, c(1, 4, 5)]
# define function that returns NULL for non-numeric argument
test_func <- function(in_put) {
  if (!is.numeric(in_put)) {
    warning(paste("argument", in_put, "isn't numeric"))
    return(NULL)
  }
  2*in_put
}  # end test_func

test_func(2)
test_func("hello")
setwd("C:/Develop/R/lecture_slides/data")
# define a function that returns invisibly
return_invisible <- function(in_put) {
  invisible(in_put)
}  # end return_invisible

return_invisible(2)

glob_var <- return_invisible(2)
glob_var

rm(list=ls())  # remove all objects
# load objects from file
loaded <- load(file="C:/Develop/data/my_data.RData")
loaded  # vector of loaded objects
ls()  # list objects
fibo_nacci <- function(len_gth) {
  if (len_gth > 2) {
    fib_seq <- fibo_nacci(len_gth-1)  # recursion
    c(fib_seq, sum(tail(fib_seq, 2)))  # return this
  } else {
    c(0, 1)  # initialize and return
  }
}  # end fibo_nacci
fibo_nacci(10)
tail(fibo_nacci(9), 2)
# DAX returns
dax_rets <- diff(log(EuStockMarkets[, 1]))
# calc_skew() calculates skew of time series of returns
calc_skew <- function(time_series=rnorm(1000)) {
# default is normal time series
  len_data <- length(time_series)  # number of observations
# normalize time_series
  time_series <-
    (time_series - mean(time_series))/sd(time_series)
# calculate skew last statement automatically returned
  len_data*sum(time_series^3)/((len_data-1)*(len_data-2))
}  # end calc_skew

# calculate skew of DAX returns
calc_skew(time_series=dax_rets)  # bind arguments by name
calc_skew(dax_rets)  # bind arguments by position
calc_skew()  # use default value of arguments
calc_skew  # show the function code

getAnywhere(calc_skew)  # display function
# sum() is a compiled primitive function
sum
# mean() is a generic function
mean
# show all methods of mean()
methods(generic.function=mean)
# show code for mean.default()
mean.default
# get all methods for generic function "plot"
methods("plot")

getAnywhere(plot)  # display function
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
# func_tional accepts function name and additional argument
func_tional <- function(func_name, in_put) {
# produce function name from argument
  func_name <- match.fun(func_name)
# execute function call
  func_name(in_put)
}  # end func_tional
func_tional(sqrt, 4)
# string also works because match.fun() converts it to a function
func_tional("sqrt", 4)
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
library(moments)  # load package moments
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
# function with three arguments
my_func <- function(arg1, arg2, arg3) {
  c(arg1=arg1, arg2=arg2, arg3=arg3)
}  # end my_func
my_func(1, 2, 3)
da_ta <- as.matrix(1:4)
# pass da_ta to arg1
apply(X=da_ta, MAR=1, FUN=my_func, arg2=2, arg3=3)
# pass da_ta to arg2
apply(X=da_ta, MAR=1, FUN=my_func, arg1=1, arg3=3)
# pass da_ta to arg3
apply(X=da_ta, MAR=1, FUN=my_func, arg1=1, arg2=2)
# vector of means of numeric columns
sapply(iris[, -5], mean)
# list of means of numeric columns
lapply(iris[, -5], mean)
# lapply using anonymous function
unlist(lapply(iris,
      function(col_umn) {
        if (is.numeric(col_umn)) mean(col_umn)
      }  # end anonymous function
      )  # end lapply
       )  # end unlist
unlist(sapply(iris, function(col_umn) {
  if (is.numeric(col_umn)) mean(col_umn)}))
sapply(6:10, sqrt)  # sapply on vector
sapply(list(6, 7, 8, 9, 10), sqrt)  # sapply on list

# calculate means of iris data frame columns
sapply(iris, mean)  # returns NA for Species

# create a matrix
mat_rix <- matrix(sample(100), ncol=4)
# calculate column means using apply
apply(mat_rix, 2, mean)

# calculate column means using sapply, with anonymous function
sapply(1:NCOL(mat_rix),
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
num_var <- 2
num_var==2
identical(num_var, 2)

identical(num_var, NULL)
# this doesn't work:
# num_var==NULL
is.null(num_var)

vec_tor <- c(2, 4, 6)
vec_tor==2
identical(vec_tor, 2)

# num_ber is equal to "1.0" within machine precision
num_ber <- 1.0 + 2*sqrt(.Machine$double.eps)
all.equal(num_ber, 1.0)

# info machine precision of computer R is running on
# ?.Machine
# machine precision
.Machine$double.eps
vec_tor <- sample(1:6, 21, replace=TRUE)
mat_rix <- matrix(vec_tor, ncol=3)
vec_tor
which(vec_tor == 5)
# equivalent but slower than above
(1:length(vec_tor))[vec_tor == 5]
which(vec_tor > 5)
# find indices of TRUE elements of Boolean matrix
which((mat_rix == 5)|(mat_rix == 6),
      arr.ind=TRUE)
# equivalent but slower than above
arrayInd(which((mat_rix == 5)|(mat_rix == 6)),
 dim(mat_rix), dimnames(mat_rix))
which.max(vec_tor)
# equivalent but slower than above
which(vec_tor == max(vec_tor))
which.min(vec_tor)
match(5, vec_tor)
# more general but slower than above
which(vec_tor == 5)
match(-5, vec_tor)
5 %in% vec_tor
# equivalent to above
match(5, vec_tor, nomatch=0) > 0
-5 %in% vec_tor
c(5, -5) %in% vec_tor
# equivalent to "5 %in% vec_tor"
any(vec_tor == 5)
# equivalent to "-5 %in% vec_tor"
any(vec_tor == (-5))
if (any(vec_tor < 0))
  cat("vector contains negative values\n")
# partial matching of strings
pmatch("med", c("mean", "median", "mode"))
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
num_var1 <- 3  # "<-" and "=" are valid assignment operators
num_var1
num_var1 = 3
num_var1
2<-3  # "<" operator confused with "<-"
2 < -3  # add space or brackets to avoid confusion
# "=" assignment within argument list
median(x=1:10)
x  # x doesn't exist outside the function
# "<-" assignment within argument list
median(x <- 1:10)
x  # x exists outside the function
# create factor vector
fac_tor <- factor(c('b', 'c', 'd', 'a', 'c', 'b'))
fac_tor
fac_tor[3]
attributes(fac_tor)  # get factor attributes
levels(fac_tor)  # get allowed values
as.numeric(fac_tor)  # get encoding vector
is.vector(fac_tor)
as.factor(1:5)  # coerce vector to factor
# coerce factor to character vector
as.vector(as.factor(1:5))
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
brea_ks <- c(freezing=0, very_cold=30,
       cold=50, pleasant=60,
       warm=80, hot=90)
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
mat_rix <- matrix(5:10, nrow=2, ncol=3)  # create a matrix
mat_rix  # by default matrices are constructed column-wise
# create a matrix row-wise
matrix(5:10, nrow=2, byrow=TRUE)
mat_rix[2, 3]  # extract third element from second row
mat_rix[2, ]  # extract second row
mat_rix[, 3]  # extract third column
mat_rix[, c(1,3)]  # extract first and third column
mat_rix[, -2]  # remove second column
# subset whole matrix
mat_rix[] <- 0
# get the number of rows or columns
nrow(vec_tor); ncol(vec_tor)
NROW(vec_tor); NCOL(vec_tor)
nrow(mat_rix); ncol(mat_rix)
NROW(mat_rix); NCOL(mat_rix)
attributes(mat_rix)  # get matrix attributes
dim(mat_rix)  # get dimension attribute
class(mat_rix)  # get class attribute
rownames(mat_rix) <- c("row1", "row2")  # rownames attribute
colnames(mat_rix) <- c("col1", "col2", "col3")  # colnames attribute
mat_rix
mat_rix["row2", "col3"]  # third element from second row
names(mat_rix)  # get the names attribute
dimnames(mat_rix)  # get dimnames attribute
attributes(mat_rix)  # get matrix attributes
mat_rix  # matrix with column names
mat_rix[1, ]  # subset rows by index
mat_rix[, "col1"]  # subset columns by name
mat_rix[, c(TRUE, FALSE, TRUE)]  # subset columns Boolean vector
mat_rix[1, ]  # subsetting can produce a vector!
class(mat_rix); class(mat_rix[1, ])
is.matrix(mat_rix[1, ]); is.vector(mat_rix[1, ])
mat_rix[1, , drop=FALSE]  # drop=FALSE preserves matrix
class(mat_rix[1, , drop=FALSE])
is.matrix(mat_rix[1, , drop=FALSE]); is.vector(mat_rix[1, , drop=FALSE])
# create a list with two elements
list_var <- list(c('a', 'b'), 1:4)
list_var
c(typeof(list_var), mode(list_var), class(list_var))
# lists are also vectors
c(is.vector(list_var), is.list(list_var))
length(list_var)
# create named list
list_var <- list(first=c('a', 'b'), second=1:4)
list_var
names(list_var)
unlist(list_var)
list_var[2]  # extract second element as sublist
list_var[[2]]  # extract second element
list_var[[2]][3]  # extract third element of second element
list_var[[c(2, 3)]]  # third element of second element
list_var$second  # extract second element
list_var$s  # extract second element - partial name matching
list_var$second[3]  # third element of second element
list_var <- list()  # empty list
list_var$a <- 1
list_var[2] <- 2
list_var
names(list_var)
as.list(c(1,2,3))
list(c(1,2,3))
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
data_frame[, 3]  # extract third column as vector
data_frame[[3]]  # extract third column as vector
data_frame[3]  # extract third column as data frame
data_frame[, 3, drop=FALSE]  # extract third column as data frame
data_frame[[3]][2]  # second element from third column
data_frame$price[2]  # second element from 'price' column
is.data.frame(data_frame[[3]]); is.vector(data_frame[[3]])
data_frame[2, ]  # extract second row
data_frame[2, ][3]  # third element from second column
data_frame[2, 3]  # third element from second column
unlist(data_frame[2, ])  # coerce to vector
is.data.frame(data_frame[2, ]); is.vector(data_frame[2, ])
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
# set option to not coerce character vectors to factors
options(stringsAsFactors=FALSE)
options("stringsAsFactors")
default.stringsAsFactors()
str(data_frame)  # display the object structure
dim(cars)  # the cars data frame has 50 rows
head(cars, n=5)  # get first five rows
tail(cars, n=5)  # get last five rows
# create data sample
stu_dents <- sample(round(runif(5, min=1, max=10), digits=2))
names(stu_dents) <- c("Angie", "Chris", "Suzie", "Matt", "Liz")
# sort the data
sort(stu_dents)
# calculate permution index for sorting the elements
order(stu_dents)
# sort the data
stu_dents[order(stu_dents)]
# calculate ranks of the elements
order(order(stu_dents))
names(stu_dents)[order(order(stu_dents))]
# permute data_frame on price
order(data_frame$price)
# sort data_frame on price
data_frame[order(data_frame$price), ]
# sort data_frame on color
data_frame[order(data_frame$color), ]
order(c(2, 1:4))  # there's a tie
order(c(2, 1:4), 1:5)  # there's a tie
# read sort() Examples
as.matrix(data_frame)
vec_tor <- sample(9)
matrix(vec_tor, ncol=3)
as.matrix(vec_tor, ncol=3)
mat_rix <- matrix(5:10, nrow=2, ncol=3)  # create a matrix
rownames(mat_rix) <- c("row1", "row2")  # rownames attribute
colnames(mat_rix) <- c("col1", "col2", "col3")  # colnames attribute
library(microbenchmark)
# call method instead of generic function
as.data.frame.matrix(mat_rix)
# a few methods for generic function as.data.frame()
sample(methods(as.data.frame), size=4)
# function method is faster than generic function
summary(microbenchmark(
  as_data_frame_matrix=
    as.data.frame.matrix(mat_rix),
  as_data_frame=as.data.frame(mat_rix),
  data_frame=data.frame(mat_rix),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
library(microbenchmark)
# lapply is faster than coercion function
summary(microbenchmark(
  as_list=
    as.list(as.data.frame.matrix(mat_rix)),
  l_apply=
    lapply(seq_along(mat_rix[1, ]),
     function(in_dex) mat_rix[, in_dex]),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# ?iris  # get information on iris
dim(iris)
head(iris, 2)
colnames(iris)
unique(iris$Species)  # list of unique elements of iris
class(unique(iris$Species))
# find which columns of iris are numeric
sapply(iris, is.numeric)
# calculate means of iris columns
sapply(iris, mean)  # returns NA for Species
# ?mtcars  # mtcars data from 1974 Motor Trend magazine
# mpg   Miles/(US) gallon
# qsec   1/4 mile time
# hp	 Gross horsepower
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
# create vector with some NA values
da_ta <- c(1, 2, NA, 4, NA, 5)
da_ta
mean(da_ta)  # returns NA, when NAs are input
mean(da_ta, na.rm=TRUE)  # remove NAs from input data
da_ta[!is.na(da_ta)]  # delete the NA values
sum(!is.na(da_ta))  # count non-NA values
rm(list=ls())
# airquality data has some NAs
head(airquality)
dim(airquality)
# number of NAs
sum(!complete.cases(airquality))
# display rows containing NAs
head(airquality[!complete.cases(airquality), ])
rm(list=ls())
# remove rows containing NAs
good_air <- airquality[complete.cases(airquality), ]
dim(good_air)
head(good_air)  # NAs removed
library(zoo)  # load package zoo
# replace NAs
good_air <- zoo::na.locf(airquality)
dim(good_air)
head(good_air)  # NAs replaced
# create vector containing NA values
vec_tor <- sample(22)
vec_tor[sample(NROW(vec_tor), 4)] <- NA
# replace NA values with the most recent non-NA values
zoo::na.locf(vec_tor)
# NULL values have no mode or type
c(mode(NULL), mode(NA))
c(typeof(NULL), typeof(NA))
c(length(NULL), length(NA))
# check for NULL values
is.null(NULL)
# NULL values are ignored when combined into a vector
c(1, 2, NULL, 4, 5)
# But NA value isn't ignored
c(1, 2, NA, 4, 5)
# vectors can be initialized to NULL
vec_tor <- NULL
is.null(vec_tor)
# grow the vector in a loop - very bad code!!!
for (in_dex in 1:5)
  vec_tor <- c(vec_tor, in_dex)
# initialize empty vector
vec_tor <- numeric()
# grow the vector in a loop - very bad code!!!
for (in_dex in 1:5)
  vec_tor <- c(vec_tor, in_dex)
# allocate vector
vec_tor <- numeric(5)
# assign to vector in a loop - good code
for (in_dex in 1:5)
  vec_tor[in_dex] <- runif(1)
