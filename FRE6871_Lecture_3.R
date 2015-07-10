library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
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
attributes(my_var) <- list(dim=c(2, 5))
is.matrix(my_var)  # is the object a matrix?
my_var  # matrix object
structure(1:10, dim=c(2, 5))  # matrix object

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

my_var <- matrix(1:10, 2, 5)
my_var <- as.character(my_var)  # explicitly coerce to "character"
c(typeof(my_var), mode(my_var), class(my_var))
is.matrix(my_var)  # is the object a matrix?

as.logical(0:3)  # explicit coercion to "logical"

c(1:3, 'a')  # implicit coercion to "character"
as.numeric(c(1:3, 'a'))  # explicit coercion to "numeric"
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
# create functional that accepts a function as input argument
func_tional <- function(func_arg) {
# calculates statistic on random numbers
  set.seed(1)
  func_arg(runif(1e4))  # apply the function name
}  # end func_tional
func_tional(mean)
func_tional(sd)
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
func_tional <- function(func_arg, arg_var) {
# functional accepts function name and one additional argument
  func_arg <- match.fun(func_arg)  # produce function from name
  func_arg(arg_var)  # apply input function
}  # end func_tional

func_tional(sqrt, 4)

# if argument is a list, then we need to call "do.call", to pass them one by one
# Passing a function as a list: first element is function name, remaining elements are arguments
func_tional <- function(list_arg) {
  func_arg <- match.fun(list_arg[[1]])  # produce function from name
  do.call(func_arg, list(as.numeric(list_arg[-1])))
}  # end func_tional

arg_list <- list("mean", 1, 2, 3, 4)
func_tional(arg_list)
func_tional <- function(func_arg, ...) {
# functional accepts function and additional '...' arguments
  func_arg(...)  # apply input function to '...' arguments
}  # end func_tional

func_tional(sum, 1, 2, 3)
rm(list=ls())
str(apply)  # get list of arguments
mat_rix <- matrix(6:1, nrow=2, ncol=3)  # create a matrix
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
# simple anonymous function
(function(x) (x + 3)) (10)

# functional accepts function and additional '...' arguments
func_tional <- function(func_arg, ...) {
  func_arg(...)  # apply input function to '...' arguments
}  # end func_tional
func_tional(sum, 2, 3, 4)
func_tional(mean, 1:10)
func_tional(func_arg=(function(x) (x + 3)), 5)
# anonymous function can be a default value
func_tional <- function(..., 
                func_arg=function(x, y, z) {x+y+z}) {
  func_arg(...)  # apply input function to '...' arguments
}  # end func_tional
func_tional(2, 3, 4)  # use default func_arg
func_tional(func_arg=sum, 2, 3, 4)  # func_arg bound by name
func_tional(func_arg=sum, 2, 3, 4, 5)
func_tional(2, 3, 4, 5)
# pass anonymous function to func_arg
func_tional(func_arg=function(x, y, z) {x*y*z}, 2, 3, 4)
rm(list=ls())
dax_rets <- 100*diff(log(EuStockMarkets[, 1]))  # DAX percent returns
library("moments")  # load library"moments"
str(moment)  # get list of arguments

moment(x=dax_rets, order=3)  # apply moment function

moment_orders <- as.matrix(1:4)  # 4x1 matrix of moment orders

# anonymous function allows looping over function parameters
apply(X=moment_orders, MARGIN=1, 
      FUN=function(moment_order) {
  moment(x=dax_rets, order=moment_order)
}  # end anonymous function
      )  # end apply

# another way of passing parameters into moment() function
apply(X=moment_orders, MARGIN=1, FUN=moment, x=dax_rets)
# vectors form columns of matrix returned by sapply
sapply(2:4, function(num) c(el1=num, el2=2*num))
# vectors of different lengths returned as list
sapply(2:4, function(num) 1:num)
# vapply is similar to sapply
vapply(2:4, function(num) c(el1=num, el2=2*num), 
       FUN.VALUE=c(0,0))
# vapply produces an error if it can't simplify
vapply(2:4, function(num) 1:num, 
       FUN.VALUE=c(0,0))
sapply(iris[, -5], mean)  # vector of means of numeric columns
lapply(iris[, -5], mean)  # calculate means of numeric columns
# calculate means of numeric columns using anonymous function
unlist(lapply(iris, 
      function(co_lumn) {
        if (is.numeric(co_lumn)) mean(co_lumn)
      }  # end anonymous function
      )  # end sapply
       )  # end unlist
unlist(sapply(iris, function(co_lumn) {if (is.numeric(co_lumn)) mean(co_lumn)}))
unique(iris$Species)  # Species takes on three distinct values
# split into separate data frames by hand
set_osa <- iris[iris$Species=="setosa", ]
versi_color <- iris[iris$Species=="versicolor", ]
virgin_ica <- iris[iris$Species=="virginica", ]
dim(set_osa)
head(set_osa, 2)
# split iris into list based on Species
split_iris <- split(iris, iris$Species)
str(split_iris, max.level=1)
names(split_iris)
dim(split_iris$setosa)
head(split_iris$setosa, 2)
unique(mtcars$cyl)  # cyl has three unique values
# split mtcars data frame based on number of cylinders
split_cars <- split(mtcars, mtcars$cyl)
str(split_cars, max.level=1)
names(split_cars)
# get mean mpg for each cylinder group
unlist(lapply(split_cars, function(x) mean(x$mpg)))
# Which is identical to the tapply function
tapply(mtcars$mpg, mtcars$cyl, mean)
# using "with" environment
with(mtcars, tapply(mpg, cyl, mean))
# can also use the functions by() and aggregate()
with(mtcars, by(mpg, cyl, mean))
aggregate(formula=(mpg ~ cyl), data=mtcars, FUN=mean)
# get several mpg stats for each cylinder group
data_cars <- sapply(split_cars,
      function(x) {
        c(mean=mean(x$mpg), max=max(x$mpg), min=min(x$mpg))
      }  # end anonymous function
      )  # end sapply
data_cars  # sapply produces a matrix
data_cars <- lapply(split_cars,  # now same using lapply
      function(x) {
        c(mean=mean(x$mpg), max=max(x$mpg), min=min(x$mpg))
      }  # end anonymous function
      )  # end sapply
is.list(data_cars)  # lapply produces a list
# do.call flattens list into a matrix
do.call(cbind, data_cars)
