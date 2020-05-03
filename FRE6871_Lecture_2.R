rm(list=ls())
TRUE | FALSE
TRUE | NA
vec_tor1 <- c(2, 4, 6)
vec_tor1 < 5  # Element-wise comparison
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
# Works (does nothing) using '&&'
if (is.matrix(vec_tor) && (vec_tor[2, 3] > 0)) {
  vec_tor[2, 3] <- 1
}
# no short-circuit so fails (produces an error)
if (is.matrix(vec_tor) & (vec_tor[2, 3] > 0)) {
  vec_tor[2, 3] <- 1
}

?Arithmetic
4.7 * 0.5  # Multiplication
4.7 / 0.5  # division
# Exponentiation
2**3
2^3

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
# Machine precision
.Machine$double.eps

vec_tor <- sample(1e3, 1e3)
mat_rix <- matrix(vec_tor, ncol=4)
which(vec_tor == 5)
match(5, vec_tor)
# Equivalent but slower than above
(1:NROW(vec_tor))[vec_tor == 5]
which(vec_tor < 5)
# Find indices of TRUE elements of Boolean matrix
which((mat_rix == 5)|(mat_rix == 6),
arr.ind=TRUE)
# Equivalent but slower than above
arrayInd(which((mat_rix == 5)|(mat_rix == 6)),
   dim(mat_rix), dimnames(mat_rix))
# Find index of largest element
which.max(vec_tor)
which(vec_tor == max(vec_tor))
# Find index of smallest element
which.min(vec_tor)
# Benchmark match() versus which()
all.equal(match(5, vec_tor),
    min(which(vec_tor == 5)))
library(microbenchmark)
summary(microbenchmark(
  match=match(5, vec_tor),
  which=min(which(vec_tor == 5)),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# Does 5 belong in vec_tor?
5 %in% vec_tor
match(5, vec_tor, nomatch=0) > 0
# Does (-5) belong in vec_tor?
(-5) %in% vec_tor
c(5, -5) %in% vec_tor
match(-5, vec_tor)
# Equivalent to "5 %in% vec_tor"
any(vec_tor == 5)
# Equivalent to "(-5) %in% vec_tor"
any(vec_tor == (-5))
# Any negative values in vec_tor?
any(vec_tor < 0)
# Example of use in if() statement
if (any(vec_tor < 2))
  cat("vector contains small values\n")
# Partial matching of strings
pmatch("med", c("mean", "median", "mode"))

str(findInterval)
# Get index of the element of "vec" that matches 5
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
# Make rightmost interval inclusive
findInterval(x=1:8, vec=c(3, 5, 7),
       rightmost.closed=TRUE)

num_var1 <- 3  # "<-" and "=" are valid assignment operators
num_var1
num_var1 = 3
num_var1
2<-3  # "<" operator confused with "<-"
2 < -3  # Add space or brackets to avoid confusion
# "=" assignment within argument list
median(x=1:10)
x  # x doesn't exist outside the function
# "<-" assignment within argument list
median(x <- 1:10)
x  # x exists outside the function

# Define a function with two arguments
test_func <- function(first_arg, second_arg) {  # Body
  first_arg + second_arg  # Returns last evaluated statement
}  # end test_func

test_func(1, 2)  # Apply the function
args(test_func)  # Display argument

# Define function that uses variable from enclosure environment
test_func <- function(first_arg, second_arg) {
  first_arg + second_arg + glob_var
}  # end test_func

test_func(3, 2)  # error - glob_var doesn't exist yet!
glob_var <- 10  # Create glob_var
test_func(3, 2)  # Now works

# Define function that returns NULL for non-numeric argument
test_func <- function(in_put) {
  if (!is.numeric(in_put)) {
    warning(paste("argument", in_put, "isn't numeric"))
    return(NULL)
  }
  2*in_put
}  # end test_func

test_func(2)
test_func("hello")

# Define a function that returns invisibly
return_invisible <- function(in_put) {
  invisible(in_put)
}  # end return_invisible

return_invisible(2)

glob_var <- return_invisible(2)
glob_var

rm(list=ls())  # Remove all objects
# Load objects from file
loaded <- load(file="C:/Develop/data/my_data.RData")
loaded  # Vector of loaded objects
ls()  # List objects

test_func <- function(first_arg, second_arg) {
# Last statement of function is return value
  first_arg + 2*second_arg
}  # end test_func
test_func(first_arg=3, second_arg=2)  # Bind by name
test_func(first=3, second=2)  # Partial name binding
test_func(3, 2)  # Bind by position
test_func(second_arg=2, 3)  # mixed binding
test_func(3, 2, 1)  # Too many arguments
test_func(2)  # Not enough arguments

# Function "paste" has two arguments with default values
str(paste)
# Default values of arguments can be specified in argument list
test_func <- function(first_arg, fac_tor=1) {
  fac_tor*first_arg
}  # end test_func
test_func(3)  # Default value used for second argument
test_func(3, 2)  # Default value over-ridden
# Default values can be a vector of strings
test_func <- function(in_put=c("first_val", "second_val")) {
  in_put <- match.arg(in_put)  # Match to arg list
  in_put
}  # end test_func
test_func("second_val")
test_func("se")  # Partial name binding
test_func("some_val")  # Invalid string

# DAX percentage returns
re_turns <- rutils::diff_it(log(EuStockMarkets[, 1]))
# calc_skew() calculates skew of time series of returns
# Default is normal time series
calc_skew <- function(se_ries=rnorm(1000)) {
  # Number of observations
  len_gth <- NROW(se_ries)
  # Normalize se_ries
  se_ries <-
    (se_ries - mean(se_ries))/sd(se_ries)
  # Calculate skew - last statement automatically returned
  len_gth*sum(se_ries^3)/((len_gth-1)*(len_gth-2))
}  # end calc_skew

# Calculate skew of DAX returns
# Bind arguments by name
calc_skew(se_ries=re_turns)
# Bind arguments by position
calc_skew(re_turns)
# Use default value of arguments
calc_skew()

str(plot)  # Dots for additional plot parameters
bind_dots <- function(in_put, ...) {
  paste0("in_put=", in_put,
 ", dots=", paste(..., sep=", "))
}  # end bind_dots
bind_dots(1, 2, 3)  # "in_put" bound by position
bind_dots(2, in_put=1, 3)  # "in_put" bound by name
bind_dots(1, 2, 3, foo=10)  # Named argument bound to dots
bind_dots <- function(arg1, arg2, ...) {
  arg1 + 2*arg2 + sum(...)
}  # end bind_dots
bind_dots(3, 2)  # Bind arguments by position
bind_dots(3, 2, 5, 8)  # Extra arguments bound to dots

str(sum)  # Dots before other arguments
sum(1, 2, 3)  # Dots bind before other arguments
sum(1, 2, NA, 3, na.rm=TRUE)
bind_dots <- function(..., in_put) {
  paste0("in_put=", in_put,
 ", dots=", paste(..., sep=", "))
}  # end bind_dots
# Arguments after dots must be bound by full name
bind_dots(1, 2, 3, in_put=10)
bind_dots(1, 2, 3, in_put=10, foo=4)  # Dots bound
bind_dots(1, 2, 3)  # "in_put" not bound
bind_dots <- function(..., in_put=10) {
  paste0("in_put=", in_put,
 ", dots=", paste(..., sep=", "))
}  # end bind_dots
bind_dots(1, 2, 3)  # "in_put" not bound, but has default

# Wrapper for mean() with default na.rm=TRUE
my_mean <- function(x, na.rm=TRUE, ...) {
  mean(x=x, na.rm=na.rm, ...)
}  # end my_mean
foo <- sample(c(1:10, NA, rep(0.1, t=5)))
mean(c(foo, NA))
mean(c(foo, NA), na.rm=TRUE)
my_mean(c(foo, NA))
my_mean(c(foo, NA), trim=0.4)  # Pass extra argument
# Wrapper for saving data into default directory
save_data <- function(...,
              file=stop("error: no file name"),
              my_dir="C:/Develop/data") {
# Create file path
  file <- file.path(my_dir, file)
  save(..., file=file)
}  # end save_data
foo <- 1:10
save_data(foo, file="scratch.RData")
save_data(foo, file="scratch.RData", my_dir="C:/Develop")
# Wrapper for testing negative arguments
stop_if_neg <- function(in_put) {
  if (!is.numeric(in_put) || in_put<0)
    stop("argument not numeric or negative")
}  # end stop_if_neg
# Wrapper for sqrt()
my_sqrt <- function(in_put) {
  stop_if_neg(in_put)
  sqrt(in_put)
}  # end my_sqrt
my_sqrt(2)
my_sqrt(-2)
my_sqrt(NA)

# Recursive function sums its argument list
sum_dots <- function(in_put, ...) {
  if (missing(...)) {  # Check if dots are empty
    return(in_put)  # just one argument left
  } else {
    in_put + sum_dots(...)  # Sum remaining arguments
  }  # end if
}  # end sum_dots
sum_dots(1, 2, 3, 4)
# Recursive function sums its argument list
sum_dots <- function(in_put, ...) {
  if (NROW(list(...)) == 0) {  # Check if dots are empty
    return(in_put)  # just one argument left
  } else {
    in_put + sum_dots(...)  # Sum remaining arguments
  }  # end if
}  # end sum_dots
sum_dots(1, 2, 3, 4)

fibo_nacci <- function(len_gth) {
  if (len_gth > 2) {
    fib_seq <- fibo_nacci(len_gth-1)  # Recursion
    c(fib_seq, sum(tail(fib_seq, 2)))  # Return this
  } else {
    c(0, 1)  # Initialize and return
  }
}  # end fibo_nacci
fibo_nacci(10)
tail(fibo_nacci(9), 2)

# Show the function code
plot.default
# Display function
getAnywhere(plot.default)

setwd("C:/Develop/lecture_slides/data")
rm(list=ls())  # Remove all objects
ls()  # List objects
# Load objects from file (side effect)
load(file="my_data.RData")
ls()  # List objects
glob_var <- 1  # Define a global variable
# Explore function scope and side effects
side_effect <- function() {
  cat("global glob_var:\t", glob_var, "\n")
# Define local "glob_var" variable
  glob_var <- 10
# Re-define the global "glob_var"
  glob_var <<- 2
  cat("local glob_var:\t", glob_var, "\n")
}  # end side_effect
side_effect()
# Global variable was modified as side effect
glob_var

# Func_tional accepts function name and additional argument
func_tional <- function(func_name, in_put) {
# Produce function name from argument
  func_name <- match.fun(func_name)
# Execute function call
  func_name(in_put)
}  # end func_tional
func_tional(sqrt, 4)
# String also works because match.fun() converts it to a function
func_tional("sqrt", 4)
str(sum)  # Sum() accepts multiple arguments
# Func_tional can't accept indefinite number of arguments
func_tional(sum, 1, 2, 3)

# Func_tional accepts function name and dots '...' argument
func_tional <- function(func_name, ...) {
  func_name <- match.fun(func_name)
  func_name(...)  # Execute function call
}  # end func_tional
func_tional(sum, 1, 2, 3)
func_tional(sum, 1, 2, NA, 4, 5)
func_tional(sum, 1, 2, NA, 4, 5, na.rm=TRUE)
# Function with three arguments and dots '...' arguments
my_func <- function(in_put, param1, param2, ...) {
  c(input=in_put, param1=param1, param2=param2,
dots=c(...))
}  # end my_func
my_func(1, 2, 3, param2=4, param1=5)
func_tional(my_func, 1, 2, 3, param2=4, param1=5)
func_tional(my_func, 1, 2, 3, 4, 5)

# Simple anonymous function
(function(x) (x + 3)) (10)

# Anonymous function passed to func_tional
func_tional(func_name=(function(x) (x + 3)), 5)
# Anonymous function is default value
func_tional <-
  function(..., func_name=function(x, y, z) {x+y+z}) {
    func_name <- match.fun(func_name)
    func_name(...)  # Execute function call
}  # end func_tional
func_tional(2, 3, 4)  # Use default func_name
func_tional(2, 3, 4, 5)
# Func_name bound by name
func_tional(func_name=sum, 2, 3, 4, 5)
# Pass anonymous function to func_name
func_tional(func_name=function(x, y, z) {x*y*z},
    2, 3, 4)

str(sum)  # Sum() accepts multiple arguments
# Sum() can't accept list of arguments
sum(list(1, 2, 3))
str(do.call)  # "what" argument is a function
# Do.call passes list elements into "sum" individually
do.call(sum, list(1, 2, 3))
do.call(sum, list(1, 2, NA, 3))
do.call(sum, list(1, 2, NA, 3, na.rm=TRUE))
# Func_tional() accepts list with function name and arguments
func_tional <- function(list_arg) {
# Produce function name from argument
  func_name <- match.fun(list_arg[[1]])
# Execute function call uing do.call()
  do.call(func_name, list_arg[-1])
}  # end func_tional
arg_list <- list("sum", 1, 2, 3)
func_tional(arg_list)
# Do_call() performs same operation as do.call()
all.equal(
  do.call(sum, list(1, 2, NA, 3, na.rm=TRUE)),
  rutils::do_call(sum, list(1, 2, NA, 3), na.rm=TRUE))

rm(list=ls())
str(apply)  # Get list of arguments
# Create a matrix
mat_rix <- matrix(6:1, nrow=2, ncol=3)
mat_rix
# Sum the rows and columns
row_sums <- apply(mat_rix, 1, sum)
col_sums <- apply(mat_rix, 2, sum)
mat_rix <- cbind(c(sum(row_sums), row_sums),
          rbind(col_sums, mat_rix))
dimnames(mat_rix) <- list(c("col_sums", "row1", "row2"),
                 c("row_sums", "col1", "col2", "col3"))
mat_rix

str(apply)  # Get list of arguments
mat_rix <- matrix(sample(12), nrow=3, ncol=4)  # Create a matrix
mat_rix
apply(mat_rix, 2, sort)  # Sort matrix columns
apply(mat_rix, 2, sort, decreasing=TRUE)  # Sort decreasing order

mat_rix[2, 2] <- NA  # Introduce NA value
mat_rix
# Calculate median of columns
apply(mat_rix, 2, median)
# Calculate median of columns with na.rm=TRUE
apply(mat_rix, 2, median, na.rm=TRUE)

rm(list=ls())
# DAX percentage returns
re_turns <- rutils::diff_it(log(EuStockMarkets[, 1]))
library(moments)  # Load package moments
str(moment)  # Get list of arguments
# Apply moment function
moment(x=re_turns, order=3)
# 4x1 matrix of moment orders
moment_orders <- as.matrix(1:4)
# Anonymous function allows looping over function parameters
apply(X=moment_orders, MARGIN=1,
      FUN=function(moment_order) {
  moment(x=re_turns, order=moment_order)
}  # end anonymous function
      )  # end apply

# Another way of passing parameters into moment() function
apply(X=moment_orders, MARGIN=1, FUN=moment,
      x=re_turns)

# Function with three arguments
my_func <- function(arg1, arg2, arg3) {
  c(arg1=arg1, arg2=arg2, arg3=arg3)
}  # end my_func
my_func(1, 2, 3)
da_ta <- as.matrix(1:4)
# Pass da_ta to arg1
apply(X=da_ta, MAR=1, FUN=my_func, arg2=2, arg3=3)
# Pass da_ta to arg2
apply(X=da_ta, MAR=1, FUN=my_func, arg1=1, arg3=3)
# Pass da_ta to arg3
apply(X=da_ta, MAR=1, FUN=my_func, arg1=1, arg2=2)

# Vector of means of numeric columns
sapply(iris[, -5], mean)
# List of means of numeric columns
lapply(iris[, -5], mean)
# Lapply using anonymous function
unlist(lapply(iris,
      function(col_umn) {
        if (is.numeric(col_umn)) mean(col_umn)
      }  # end anonymous function
      )  # end lapply
       )  # end unlist
unlist(sapply(iris, function(col_umn) {
  if (is.numeric(col_umn)) mean(col_umn)}))

sapply(6:10, sqrt)  # Sapply on vector
sapply(list(6, 7, 8, 9, 10), sqrt)  # Sapply on list

# Calculate means of iris data frame columns
sapply(iris, mean)  # Returns NA for Species

# Create a matrix
mat_rix <- matrix(sample(100), ncol=4)
# Calculate column means using apply
apply(mat_rix, 2, mean)

# Calculate column means using sapply, with anonymous function
sapply(1:NCOL(mat_rix),
       function(col_index) {  # Anonymous function
 mean(mat_rix[, col_index])
  }  # end anonymous function
)  # end sapply

# Vectors form columns of matrix returned by sapply
sapply(2:4, function(num) c(el1=num, el2=2*num))
# Vectors of different lengths returned as list
sapply(2:4, function(num) 1:num)
# vapply is similar to sapply
vapply(2:4, function(num) c(el1=num, el2=2*num),
       FUN.VALUE=c(row1=0, row2=0))
# vapply produces an error if it can't simplify
vapply(2:4, function(num) 1:num,
       FUN.VALUE=c(row1=0, row2=0))

# Create factor vector
fac_tor <- factor(c('b', 'c', 'd', 'a', 'c', 'b'))
fac_tor
fac_tor[3]
attributes(fac_tor)  # Get factor attributes
levels(fac_tor)  # Get allowed values
as.numeric(fac_tor)  # Get encoding vector
is.vector(fac_tor)
as.factor(1:5)  # Coerce vector to factor
# Coerce factor to character vector
as.vector(as.factor(1:5))

fac_tor
levels(fac_tor)  # Get allowed values
unique(fac_tor)  # Get unique elements
# Get contingency (frequency) table
table(fac_tor)
# Get contingency table using sapply
sapply(levels(fac_tor),
 function(le_vel) {
   sum(fac_tor==le_vel)
 })  # end sapply

library(microbenchmark)
str(findInterval)
# Get index of the element of "vec" that matches 5
findInterval(x=5, vec=c(3, 5, 7))
match(5, c(3, 5, 7))
# No exact match
findInterval(x=6, vec=c(3, 5, 7))
match(6, c(3, 5, 7))
# Indices of "vec" that match elements of "x"
findInterval(x=1:8, vec=c(3, 5, 7))
# Return only indices of inside intervals
findInterval(x=1:8, vec=c(3, 5, 7),
       all.inside=TRUE)
# make rightmost interval inclusive
findInterval(x=1:8, vec=c(3, 5, 7),
       rightmost.closed=TRUE)

# Named numeric vector of breakpoints
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
da_ta <- sample(0:6) + 0.1
da_ta
cut(x=da_ta, breaks=c(2, 4, 6, 8))
rbind(da_ta, cut(x=da_ta, breaks=c(2, 4, 6, 8)))
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

# Calculate DAX percentage returns
dax_rets <- diff(log(EuStockMarkets[, 1]))
# Plot histogram
par(mar=c(1, 1, 1, 1), oma=c(2, 2, 2, 0))
histo_gram <- hist(dax_rets, breaks=30,
  main="", ylim=c(0, 60), xlim=c(-0.04, 0.04),
  xlab="", ylab="", freq=FALSE)

# Draw kernel density of histogram
lines(density(dax_rets), col='red', lwd=2)
# Add density of normal distribution
curve(expr=dnorm(x, mean=mean(dax_rets),
  sd=sd(dax_rets)), add=TRUE, type="l",
  lwd=2, col="blue")
title(main="DAX return distribution", line=0)
# Add legend
legend("topright", inset=0.05, cex=0.8, title=NULL,
  leg=c(colnames(EuStockMarkets)[1], "Normal"),
  lwd=6, bg="white", col=c("red", "blue"))
# total area under histogram
diff(histo_gram$breaks) %*% histo_gram$density

mat_rix <- matrix(5:10, nrow=2, ncol=3)  # Create a matrix
mat_rix  # by default matrices are constructed column-wise
# Create a matrix row-wise
matrix(5:10, nrow=2, byrow=TRUE)
mat_rix[2, 3]  # Extract third element from second row
mat_rix[2, ]  # Extract second row
mat_rix[, 3]  # Extract third column
mat_rix[, c(1,3)]  # Extract first and third column
mat_rix[, -2]  # Remove second column
# Subset whole matrix
mat_rix[] <- 0
# Get the number of rows or columns
nrow(vec_tor); ncol(vec_tor)
NROW(vec_tor); NCOL(vec_tor)
nrow(mat_rix); ncol(mat_rix)
NROW(mat_rix); NCOL(mat_rix)

attributes(mat_rix)  # Get matrix attributes
dim(mat_rix)  # Get dimension attribute
class(mat_rix)  # Get class attribute
rownames(mat_rix) <- c("row1", "row2")  # Rownames attribute
colnames(mat_rix) <- c("col1", "col2", "col3")  # Colnames attribute
mat_rix
mat_rix["row2", "col3"]  # third element from second row
names(mat_rix)  # Get the names attribute
dimnames(mat_rix)  # Get dimnames attribute
attributes(mat_rix)  # Get matrix attributes

mat_rix  # matrix with column names
mat_rix[1, ]  # Subset rows by index
mat_rix[, "col1"]  # Subset columns by name
mat_rix[, c(TRUE, FALSE, TRUE)]  # Subset columns Boolean vector
mat_rix[1, ]  # Subsetting can produce a vector!
class(mat_rix); class(mat_rix[1, ])
is.matrix(mat_rix[1, ]); is.vector(mat_rix[1, ])
mat_rix[1, , drop=FALSE]  # Drop=FALSE preserves matrix
class(mat_rix[1, , drop=FALSE])
is.matrix(mat_rix[1, , drop=FALSE]); is.vector(mat_rix[1, , drop=FALSE])

rm(list=ls())
# Expressions enclosed in parenthesis are less ambiguous
-2:5
(-2):5
-(2:5)
# Expressions enclosed in parenthesis are less ambiguous
-2*3+5
-2*(3+5)

# Expressions can be separated by semicolons or by lines
{1+2; 2*3; 1:5}
# or
{1+2
2*3
1:5}

mat_rix <- matrix(nr=3, nc=4)
mat_rix <- 0
# Subset whole matrix
mat_rix[] <- 0

# Parenthesis and braces require a little additional processing time
library(microbenchmark)
summary(microbenchmark(
  ba_se=sqrt(rnorm(10000)^2),
  pa_ren=sqrt(((((rnorm(10000)^2))))),
  bra_ce=sqrt({{{{rnorm(10000)^2}}}}),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

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

switch("a", a="aaahh", b="bee", c="see", d=2,
       "else this")
switch("c", a="aaahh", b="bee", c="see", d=2,
       "else this")
switch(3, a="aaahh", b="bee", c="see", d=2,
       "else this")
switch("cc", a="aaahh", b="bee", c="see", d=2,
       "else this")
# Measure of central tendency
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

for (in_dex in vec_tor) {ex_pressions}

rm(list=ls())
color_list <- list("red", "white", "blue")
# loop over list
for (some_color in color_list) {
  print(some_color)
}  # end for
# loop over vector
for (in_dex in 1:3) {
  print(color_list[[in_dex]])
}  # end for

# While loops require initialization
in_dex <- 1
# While loop
while (in_dex < 4) {
  print(color_list[[in_dex]])
  in_dex <- in_dex + 1
}  # end while

vec_tor <- integer(7)
# loop over a vector and overwrite it
for (i in seq_along(vec_tor)) {
  cat("Changing element:", i, "\n")
  vec_tor[i] <- i^2
}  # end for
# Modifying vec_tor inside sapply() has no effect
vec_tor <- integer(7)
vec_tor
sapply(seq_along(vec_tor),
 function(i) {
   vec_tor[i] <- i^2
 })  # end sapply
vec_tor
# Super-assignment operator "<<-" allows modifying vec_tor
sapply(seq_along(vec_tor),
 function(i) {
   vec_tor[i] <<- i^2 # "<<-" !!!
 })  # end sapply
vec_tor
# sapply() loop returns vector of values
vec_tor <- sapply(seq_along(vec_tor),
            function(i) (i^2))

rm(list=ls())
# fib_seq <- numeric()  # zero length numeric vector
# Pre-allocate vector instead of "growing" it
fib_seq <- numeric(10)
fib_seq[1] <- 0  # initialize
fib_seq[2] <- 1  # initialize
for (i in 3:10) {  # Perform recurrence loop
  fib_seq[i] <- fib_seq[i-1] + fib_seq[i-2]
}  # end for
fib_seq

# Allocate character vector
character()
character(5)
is.character(character(5))
# Allocate integer vector
integer()
integer(5)
is.integer(integer(5))
is.numeric(integer(5))
# Allocate numeric vector
numeric()
numeric(5)
is.integer(numeric(5))
is.numeric(numeric(5))
# Allocate Boolean vector
vector()
vector(length=5)
# Allocate numeric vector
vector(length=5, mode="numeric")
is.null(vector())
# Allocate Boolean matrix
matrix()
is.null(matrix())
# Allocate integer matrix
matrix(NA_integer_, nrow=3, ncol=2)
is.integer(matrix(NA_integer_, nrow=3, ncol=2))
# Allocate numeric matrix
matrix(NA_real_, nrow=3, ncol=2)
is.numeric(matrix(NA_real_, nrow=3, ncol=2))

vec_tor <- sample(1:9)
vec_tor
vec_tor < 5  # Element-wise comparison
vec_tor == 5  # Element-wise comparison
mat_rix <- matrix(vec_tor, ncol=3)
mat_rix
mat_rix < 5  # Element-wise comparison
mat_rix == 5  # Element-wise comparison

mat_rix <- 1:6  # Create a vector
class(mat_rix)  # Get its class
# is it vector or matrix?
c(is.vector(mat_rix), is.matrix(mat_rix))
structure(mat_rix, dim=c(2, 3))  # Matrix object
# Adding dimension attribute coerces into matrix
dim(mat_rix) <- c(2, 3)
class(mat_rix)  # Get its class
# is it vector or matrix?
c(is.vector(mat_rix), is.matrix(mat_rix))
# Assign dimnames attribute
dimnames(mat_rix) <- list(rows=c("row1", "row2"),
            columns=c("col1", "col2", "col3"))
mat_rix

mat_rix <- matrix(1:10, 2, 5)  # Create matrix
mat_rix
# as.numeric strips dim attribute from matrix
as.numeric(mat_rix)
# Explicitly coerce to "character"
mat_rix <- as.character(mat_rix)
c(typeof(mat_rix), mode(mat_rix), class(mat_rix))
# Coercion converted matrix to vector
c(is.matrix(mat_rix), is.vector(mat_rix))

vec_tor1 <- 1:3  # define vector
vec_tor2 <- 6:4  # define vector
# Bind vectors into columns
cbind(vec_tor1, vec_tor2)
# Bind vectors into rows
rbind(vec_tor1, vec_tor2)
# Extend to four elements
vec_tor2 <- c(vec_tor2, 7)
# recycling rule applied
cbind(vec_tor1, vec_tor2)
# Another example of recycling rule
1:6 + c(10, 20)

# replicate a single element
rep("a", 5)
# replicate the whole vector several times
rep(c("a", "b"), 5)
rep(c("a", "b"), times=5)
# replicate the first element, then the second, etc.
rep(c("a", "b"), each=5)
# replicate to specified length
rep(c("a", "b"), length.out=5)

# define vector and matrix
vec_tor1 <- c(2, 4, 3)
mat_rix <- matrix(sample(1:12), ncol=3)
# Multiply matrix by vector column-wise
vec_tor1 * mat_rix
mat_rix * vec_tor1
# Multiply matrix by vector row-wise
t(vec_tor1 * t(mat_rix))

vec_tor1
vec_tor2 <- 6:4  # define vector
# Multiply two vectors element-by-element
vec_tor1 * vec_tor2
# Calculate inner product
vec_tor1 %*% vec_tor2
# Calculate inner product and drop dimensions
drop(vec_tor1 %*% vec_tor2)
# Multiply columns of matrix by vector
mat_rix %*% vec_tor1  # Single column matrix
drop(mat_rix %*% vec_tor1)  # vector
rowSums(t(vec_tor1 * t(mat_rix)))
# using rowSums() and t() is 10 times slower than %*%
library(microbenchmark)
summary(microbenchmark(
  in_ner=drop(mat_rix %*% vec_tor1),
  row_sums=rowSums(t(vec_tor1 * t(mat_rix))),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

library(microbenchmark)
# Multiply matrix by vector fails because dimensions aren't conformable
vec_tor1 %*% mat_rix
# Works after transpose
drop(vec_tor1 %*% t(mat_rix))
# Calculate inner product
crossprod(vec_tor1, vec_tor2)
# Create matrix and vector
mat_rix <- matrix(1:3000, ncol=3)
tmat_rix <- t(mat_rix)
vec_tor <- 1:3
# crossprod() is slightly faster than "%*%" operator
summary(microbenchmark(
  cross_prod=crossprod(tmat_rix, vec_tor),
  inner_prod=mat_rix %*% vec_tor,
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

vec_tor1 <- rnorm(1000000)
vec_tor2 <- rnorm(1000000)
big_vector <- numeric(1000000)
# Sum two vectors in two different ways
summary(microbenchmark(
  # Sum vectors using "for" loop
  r_loop=(for (i in 1:NROW(vec_tor1)) {
    big_vector[i] <- vec_tor1[i] + vec_tor2[i]
  }),
  # Sum vectors using vectorized "+"
  vec_torized=(vec_tor1 + vec_tor2),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Allocate memory for cumulative sum
cum_sum <- numeric(NROW(big_vector))
cum_sum[1] <- big_vector[1]
# Calculate cumulative sum in two different ways
summary(microbenchmark(
# Cumulative sum using "for" loop
  r_loop=(for (i in 2:NROW(big_vector)) {
    cum_sum[i] <- cum_sum[i-1] + big_vector[i]
  }),
# Cumulative sum using "cumsum"
  vec_torized=cumsum(big_vector),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# Matrix with 5,000 rows
mat_rix <- matrix(rnorm(10000), ncol=2)
# Calculate row sums two different ways
all.equal(rowSums(mat_rix),
  apply(mat_rix, 1, sum))
summary(microbenchmark(
  row_sums=rowSums(mat_rix),
  ap_ply=apply(mat_rix, 1, sum),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

library(microbenchmark)
str(pmax)
# Calculate row maximums two different ways
summary(microbenchmark(
  p_max=
    do.call(pmax.int,
lapply(seq_along(mat_rix[1, ]),
  function(in_dex) mat_rix[, in_dex])),
  l_apply=unlist(
    lapply(seq_along(mat_rix[, 1]),
  function(in_dex) max(mat_rix[in_dex, ]))),
  times=10))[, c(1, 4, 5)]

install.packages("matrixStats")  # Install package matrixStats
library(matrixStats)  # Load package matrixStats
# Calculate row min values three different ways
summary(microbenchmark(
  row_mins=rowMins(mat_rix),
  p_min=
    do.call(pmin.int,
      lapply(seq_along(mat_rix[1, ]),
             function(in_dex)
               mat_rix[, in_dex])),
  as_data_frame=
    do.call(pmin.int,
      as.data.frame.matrix(mat_rix)),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

set.seed(1121)  # Reset random number generator
# Sample from Standard Normal Distribution
len_gth <- 1000
da_ta <- rnorm(len_gth)
# Sample mean - MC estimate
mean(da_ta)
# Sample standard deviation - MC estimate
sd(da_ta)
# Monte Carlo estimate of cumulative probability
da_ta <- sort(da_ta)
pnorm(1)
sum(da_ta<1)/len_gth
# Monte Carlo estimate of quantile
conf_level <- 0.99
qnorm(conf_level)
cut_off <- conf_level*len_gth
da_ta[cut_off]
quantile(da_ta, probs=conf_level)
# Analyze the source code of quantile()
stats:::quantile.default
# Microbenchmark quantile
library(microbenchmark)
summary(microbenchmark(
  monte_carlo=da_ta[cut_off],
  quan_tile=quantile(da_ta, probs=conf_level),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary

x11(width=6, height=5)
par(oma=c(1, 1, 1, 1), mar=c(2, 2, 2, 1), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
set.seed(1121)  # Reset random number generator
bar_rier <- 20  # Barrier level
len_gth <- 1000  # Number of simulation steps
pa_th <- numeric(len_gth)  # Allocate path vector
pa_th[1] <- 0  # Initialize path
in_dex <- 2  # Initialize simulation index
while ((in_dex <= len_gth) &&
 (pa_th[in_dex - 1] < bar_rier)) {
# Simulate next step
  pa_th[in_dex] <-
    pa_th[in_dex - 1] + rnorm(1)
  in_dex <- in_dex + 1  # Advance in_dex
}  # end while
# Fill remaining pa_th after it crosses bar_rier
if (in_dex <= len_gth)
  pa_th[in_dex:len_gth] <- pa_th[in_dex - 1]
# Create daily time series starting 2011
ts_path <- ts(data=pa_th, frequency=365, start=c(2011, 1))
plot(ts_path, type="l", col="black",
     lty="solid", lwd=2, xlab="", ylab="")
abline(h=bar_rier, lwd=2, col="red")
title(main="Brownian motion crossing a barrier level",
      line=0.5)

x11(width=6, height=5)
par(oma=c(1, 1, 1, 1), mar=c(2, 2, 2, 1), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
set.seed(1121)  # Reset random number generator
bar_rier <- 20  # Barrier level
len_gth <- 1000  # Number of simulation steps
# Simulate path of Brownian motion
pa_th <- cumsum(rnorm(len_gth))
# Find index when pa_th crosses bar_rier
cro_ss <- which(pa_th > bar_rier)
# Fill remaining pa_th after it crosses bar_rier
if (NROW(cro_ss)>0) {
  pa_th[(cro_ss[1]+1):len_gth] <-
    pa_th[cro_ss[1]]
}  # end if
# Create daily time series starting 2011
ts_path <- ts(data=pa_th, frequency=365,
     start=c(2011, 1))
# Create plot with horizontal line
plot(ts_path, type="l", col="black",
     lty="solid", lwd=2, xlab="", ylab="")
abline(h=bar_rier, lwd=2, col="red")
title(main="Brownian motion crossing a barrier level",
      line=0.5)

set.seed(1121)  # Reset random number generator
# Sample from Standard Normal Distribution
len_gth <- 1000; da_ta <- rnorm(len_gth)
# Sample mean and standard deviation
mean(da_ta); sd(da_ta)
# Bootstrap of sample mean and median
boot_data <- sapply(1:10000, function(x) {
  boot_sample <- da_ta[sample.int(len_gth,
                              replace=TRUE)]
  c(mean=mean(boot_sample), median=median(boot_sample))
})  # end sapply
boot_data <- t(boot_data)

boot_data[1:3, ]
# Standard error from formula
sd(da_ta)/sqrt(len_gth)
# Standard error of mean from bootstrap
sd(boot_data[, "mean"])
# Standard error of median from bootstrap
sd(boot_data[, "median"])
plot(density(boot_data[, "median"]),
     lwd=2, xlab="regression slopes",
     main="Distribution of Bootstrapped Median")
abline(v=mean(boot_data[, "median"]),
 lwd=2, col="red")

set.seed(1121)  # Reset random number generator
len_gth <- 1000
da_ta <- rnorm(len_gth)
# Bootstrap of sample mean and median
boot_data <- sapply(1:10000, function(x) {
  # Boot_sample from Standard Normal Distribution
  boot_sample <- rnorm(len_gth)
  c(mean=mean(boot_sample),
    median=median(boot_sample))
})  # end sapply
boot_data[, 1:3]
# Standard error from formula
sd(da_ta)/sqrt(len_gth)
# Standard error of mean from bootstrap
sd(boot_data["mean", ])
# Standard error of median from bootstrap
sd(boot_data["median", ])

library(parallel)  # Load package parallel
n_cores <- detectCores() - 1  # Number of cores
clus_ter <- makeCluster(n_cores)  # Initialize compute cluster under Windows
set.seed(1121)  # Reset random number generator
# Sample from Standard Normal Distribution
len_gth <- 1000
da_ta <- rnorm(len_gth)
# Bootstrap mean and median under Windows
boot_data <- parLapply(clus_ter, 1:10000,
  function(x, da_ta, len_gth) {
  boot_sample <- da_ta[sample.int(len_gth, replace=TRUE)]
  c(mean=mean(boot_sample), median=median(boot_sample))
  }, da_ta=da_ta, len_gth=len_gth)  # end parLapply
# Bootstrap mean and median under Mac-OSX or Linux
boot_data <- mclapply(1:10000,
  function(x) {
  boot_sample <- da_ta[sample.int(len_gth, replace=TRUE)]
  c(mean=mean(boot_sample), median=median(boot_sample))
  }, mc.cores=n_cores)  # end mclapply
boot_data <- rutils::do_call(rbind, boot_data)
# Means and standard errors from bootstrap
apply(boot_data, MARGIN=2,
function(x) c(mean=mean(x), std_error=sd(x)))
# Standard error from formula
sd(da_ta)/sqrt(len_gth)
stopCluster(clus_ter)  # Stop R processes over cluster under Windows
