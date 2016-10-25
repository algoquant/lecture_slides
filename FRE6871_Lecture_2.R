library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
options(digits=3)
thm <- knit_theme$get("acid")
knit_theme$set(thm)
rm(list=ls())
TRUE | FALSE
TRUE | NA
vec_tor1 <- c(2, 4, 6)
vec_tor1 < 5  # element-wise comparison
(vec_tor1 < 5) & (vec_tor1 > 3)
vec_tor1[(vec_tor1 < 5) & (vec_tor1 > 3)]
vec_tor2 <- c(-10, 0, 10)
vec_tor1 < vec_tor2
c(FALSE, TRUE, FALSE) & c(TRUE, TRUE, FALSE)
c(FALSE, TRUE, FALSE) | c(TRUE, TRUE, FALSE)
rm(list=ls())
c(FALSE, TRUE, FALSE) && c(TRUE, TRUE, FALSE)
c(FALSE, TRUE, FALSE) || c(TRUE, TRUE, FALSE)
echo_true <- function() {cat("echo_true\t"); TRUE}
echo_false <- function() {cat("echo_false\t"); FALSE}
echo_true() | echo_false()
echo_true() || echo_false()  # echo_false() isn't evaluated at all!
vec_tor <- c(2, 4, 6)
# works (does nothing) using '&&'
if (is.matrix(vec_tor) && (vec_tor[2, 3] > 0)) {
  vec_tor[2, 3] <- 1
}
# no short-circuit so fails (produces an error)
if (is.matrix(vec_tor) & (vec_tor[2, 3] > 0)) {
  vec_tor[2, 3] <- 1
}
load(file="C:/Develop/data/etf_data.RData")
4.7 * 0.5  # multiplication
4.7 / 0.5  # division
# exponentiation
2**3
2^3
num_var <- 2
num_var==2
identical(num_var, 2)

identical(num_var, NULL)
num_var==NULL
is.null(num_var)

vec_tor <- c(2, 4, 6)
vec_tor==2
identical(vec_tor, 2)
vec_tor <- sample(1:9)
mat_rix <- matrix(vec_tor, ncol=3)
vec_tor
which(vec_tor == 5)
# equivalent but less efficient than above
(1:length(vec_tor))[vec_tor == 5]
which(vec_tor > 5)
# find indices of TRUE elements of boolean matrix
which((mat_rix == 5)|(mat_rix == 6), arr.ind=TRUE)
# equivalent but less efficient than above
arrayInd(which((mat_rix == 5)|(mat_rix == 6)),
 dim(mat_rix), dimnames(mat_rix))
which.max(vec_tor)
# equivalent but less efficient than above
which(vec_tor == max(vec_tor))
which.min(vec_tor)
match(5, vec_tor)
# equivalent but less efficient than above
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
num_var1 <- 3  # "<-" and "=" are valid assignment operators
num_var1
num_var1 = 3
num_var1
2<-3  # "<" operator confused with "<-"
2 < -3  # add space or brackets to avoid confusion
median(x=1:10)  # "=" assignment within argument list
x  # x doesn't exist outside the function
median(x <- 1:10)  # "<-" assignment within argument list
x  # x exists outside the function
rm(list=ls())
num_var1 <- 1

if (num_var1) {  # numeric zero is FALSE, all other numbers are TRUE
  num_var2 <- 4
} else if (num_var1 == 0) {  # 'else if' together on same line
  num_var2 <- 0
} else {  # 'else' together with curly braces
  num_var2 <- -4
}  # end if

num_var2
switch("a", a="aaahh", b="bee", c="see", d=2, "else this")
switch("c", a="aaahh", b="bee", c="see", d=2, "else this")
switch(3, a="aaahh", b="bee", c="see", d=2, "else this")
switch("cc", a="aaahh", b="bee", c="see", d=2, "else this")
# measure of central tendency
centra_lity <- function(in_put,
    meth_od=c("mean", "mean_narm", "median")) {
# validate "meth_od" argument
  meth_od <- match.arg(meth_od)
  switch(meth_od,
 mean=mean(in_put),
 mean_narm=mean(in_put, na.rm=TRUE),
 median=median(in_put))
}  # end centra_lity
my_var <- rnorm(100, mean=2)
centra_lity(my_var, "mean")
centra_lity(my_var, "mean_narm")
centra_lity(my_var, "median")
rm(list=ls())
color_list <- list("red", "white", "blue")
for (some_color in color_list) {  # loop over list
  print(some_color)
}
for (in_dex in 1:3) {  # loop over vector
  print(color_list[[in_dex]])
}

in_dex <- 1  # while loops need initialization
while (in_dex < 4) {  # while loop
  print(color_list[[in_dex]])
  in_dex <- in_dex + 1
}
rm(list=ls())
# fib_seq <- numeric()  # zero length numeric vector
# pre-allocate vector instead of "growing" it
fib_seq <- numeric(10)
fib_seq[1] <- 0  # initialize
fib_seq[2] <- 1  # initialize
for (i in 3:10) {  # perform recurrence loop
  fib_seq[i] <- fib_seq[i-1] + fib_seq[i-2]
}  # end for
fib_seq
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
da_ta <- c(1, 2, NA, 4, NA, 5)  # create vector with some NA values
da_ta
mean(da_ta)  # returns NA, when NAs are input
mean(da_ta, na.rm=TRUE)  # remove NAs from input data
da_ta[!is.na(da_ta)]  # delete the NA values
sum(!is.na(da_ta))  # count non-NA values
rm(list=ls())
head(airquality)  # airquality data has some NAs
dim(airquality)
sum(!complete.cases(airquality))  # number of NAs
# display rows containing NAs
head(airquality[!complete.cases(airquality), ])
rm(list=ls())
# remove rows containing NAs
good_air <- airquality[complete.cases(airquality), ]
dim(good_air)
head(good_air)  # NAs removed
library(zoo)  # load package zoo
good_air <- na.locf(airquality)  # replace NAs
dim(good_air)
head(good_air)  # NAs replaced
# NULL values have no mode or type
c(mode(NULL), mode(NA))
c(typeof(NULL), typeof(NA))
c(length(NULL), length(NA))
# check for NULL values
is.null(NULL)
# NULL values are ignored when combined into a vector
c(1, 2, NULL, 4, 5)  
# vectors can be initialized to NULL
da_ta <- NULL
for (in_dex in 1:5)
  da_ta <- c(da_ta, in_dex)
da_ta
# NA value isn't ignored
da_ta <- NA
for (in_dex in 1:5)
  da_ta <- c(da_ta, in_dex)
da_ta
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
# create functional that accepts a function as input argument
func_tional <- function(func_name) {
# calculates statistic on random numbers
  set.seed(1)
  func_name(runif(1e4))  # apply the function name
}  # end func_tional
func_tional(mean)
func_tional(sd)
# define a power function factory
make_func <- function(arg_param) {  # wrapper function
  function(in_put) {  # anonymous closure
    in_put^arg_param
  }
}  # end make_func

square_func <- make_func(2)  # define square function
square_func(4)
cube_func <- make_func(3)  # define cube function
cube_func(2)
cube_root_func <- make_func(1/3)  # define cube root function
cube_root_func(8)
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
sapply(iris[, -5], mean)  # vector of means of numeric columns
lapply(iris[, -5], mean)  # list of means of numeric columns
unlist(lapply(iris,  # lapply using anonymous function
      function(col_umn) {
        if (is.numeric(col_umn)) mean(col_umn)
      }  # end anonymous function
      )  # end sapply
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
