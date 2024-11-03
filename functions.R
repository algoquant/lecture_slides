# Define a function with two arguments
testfun <- function(arg1, arg2) {  # Body
  arg1 + arg2  # Returns last evaluated statement
}  # end testfun
testfun(1, 2)  # Apply the function
args(testfun)  # Display argument
# Define function that uses variable from enclosure environment
testfun <- function(arg1, arg2) {
  arg1 + arg2 + globv
}  # end testfun
testfun(3, 2)  # error - globv doesn't exist yet!
globv <- 10  # Create globv
testfun(3, 2)  # Now works
# Define function that returns NULL for non-numeric argument
testfun <- function(inputv) {
  if (!is.numeric(inputv)) {
    warning(paste("argument", inputv, "isn't numeric"))
    return(NULL)
  }
  2*inputv
}  # end testfun
testfun(2)
testfun("hello")
# Define a function that returns invisibly
retinv <- function(inputv) {
  invisible(inputv)
}  # end retinv
retinv(2)
globv <- retinv(2)
globv
rm(list=ls())  # Delete all objects in workspace
# Load objects from file
loaded <- load(file="/Users/jerzy/Develop/data/my_data.RData")
loaded  # Vector of loaded objects
ls()  # List objects
testfun <- function(arg1, arg2) {
# Last statement of function is return value
  arg1 + 2*arg2
}  # end testfun
testfun(arg1=3, arg2=2)  # Bind by name
testfun(first=3, second=2)  # Partial name binding
testfun(3, 2)  # Bind by position
testfun(arg2=2, 3)  # mixed binding
testfun(3, 2, 1)  # Too many arguments
testfun(2)  # Not enough arguments
# Function "paste" has two arguments with default values
str(paste)
# Default values of arguments can be specified in argument list
testfun <- function(arg1, ratio=1) {
  ratio*arg1
}  # end testfun
testfun(3)  # Default value used for second argument
testfun(3, 2)  # Default value over-ridden
# Default values can be a vector of strings
testfun <- function(inputv=c("first_val", "second_val")) {
  inputv <- match.arg(inputv)  # Match to arg list
  inputv
}  # end testfun
testfun("second_val")
testfun("se")  # Partial name binding
testfun("some_val")  # Invalid string
# VTI percentage returns
retp <- rutils::diffit(log(Cl(rutils::etfenv$VTI)))
# calc_skew() calculates skew of time series of returns
# Default is normal time series
calc_skew <- function(retp=rnorm(1000)) {
  # Number of observations
  nrows <- NROW(retp)
  # Standardize returns
  retp <- (retp - mean(retp))/sd(retp)
  # Calculate skew - last statement automatically returned
  nrows*sum(retp^3)/((nrows-1)*(nrows-2))
}  # end calc_skew
# Calculate the skew of VTI returns
# Pass the arguments by name
calc_skew(retp=retp)
# Pass the arguments by position
calc_skew(retp)
# Use default value of arguments
calc_skew()
str(plot)  # Dots for additional plot parameters
bindd <- function(inputv, ...) {
  paste0("inputv=", inputv, ", dots=", paste(..., sep=", "))
}  # end bindd
bindd(1, 2, 3)  # "inputv" bound by position
bindd(2, inputv=1, 3)  # "inputv" bound by name
bindd(1, 2, 3, argv=10)  # Named argument bound to dots
bindd <- function(arg1, arg2, ...) {
  arg1 + 2*arg2 + sum(...)
}  # end bindd
bindd(3, 2)  # Bind arguments by position
bindd(3, 2, 5, 8)  # Extra arguments bound to dots
str(sum)  # Dots before other arguments
sum(1, 2, 3)  # Dots bind before other arguments
sum(1, 2, NA, 3, na.rm=TRUE)
bindd <- function(..., inputv) {
  paste0("inputv=", inputv, ", dots=", paste(..., sep=", "))
}  # end bindd
# Arguments after dots must be bound by full name
bindd(1, 2, 3, inputv=10)
bindd(1, 2, 3, inputv=10, argv=4)  # Dots bound
bindd(1, 2, 3)  # "inputv" not bound
bindd <- function(..., inputv=10) {
  paste0("inputv=", inputv, ", dots=", paste(..., sep=", "))
}  # end bindd
bindd(1, 2, 3)  # "inputv" not bound, but has default
# Wrapper for mean() with default na.rm=TRUE
meanfun <- function(x, na.rm=TRUE, ...) {
  mean(x=x, na.rm=na.rm, ...)
}  # end meanfun
vecv <- sample(c(1:10, NA, rep(0.1, t=5)))
mean(vecv)
mean(vecv, na.rm=TRUE)
meanfun(vecv)
meanfun(vecv, trim=0.4)  # Pass extra argument
# Wrapper for saving data into default directory
save_data <- function(...,
              file=stop("error: no file name"),
              my_dir="/Users/jerzy/Develop/data") {
# Create file path
  file <- file.path(my_dir, file)
  save(..., file=file)
}  # end save_data
vecv <- 1:10
save_data(vecv, file="scratch.RData")
save_data(vecv, file="scratch.RData", my_dir="/Users/jerzy/Develop")
# Wrapper for testing negative arguments
stop_if_neg <- function(inputv) {
  if (!is.numeric(inputv) || inputv < 0)
    stop("argument not numeric or negative")
}  # end stop_if_neg
# Wrapper for sqrt()
my_sqrt <- function(inputv) {
  stop_if_neg(inputv)
  sqrt(inputv)
}  # end my_sqrt
my_sqrt(2)
my_sqrt(-2)
my_sqrt(NA)
# Recursive function sums its argument list
sum_dots <- function(inputv, ...) {
  if (missing(...)) {  # Check if dots are empty
    return(inputv)  # just one argument left
  } else {
    inputv + sum_dots(...)  # Sum remaining arguments
  }  # end if
}  # end sum_dots
sum_dots(1, 2, 3, 4)
# Recursive function sums its argument list
sum_dots <- function(inputv, ...) {
  if (NROW(list(...)) == 0) {  # Check if dots are empty
    return(inputv)  # just one argument left
  } else {
    inputv + sum_dots(...)  # Sum remaining arguments
  }  # end if
}  # end sum_dots
sum_dots(1, 2, 3, 4)
fibonacci <- function(nrows) {
  if (nrows > 2) {
    fib_seq <- fibonacci(nrows-1)  # Recursion
    c(fib_seq, sum(tail(fib_seq, 2)))  # Return this
  } else {
    c(0, 1)  # Initialize and return
  }
}  # end fibonacci
fibonacci(10)
tail(fibonacci(9), 2)
# Show the function code
plot.default
# Display function
getAnywhere(plot.default)
# Sum() is a compiled primitive function
sum
# mean() is a generic function
mean
# Show all methods of mean()
methods(generic.function=mean)
# Show code for mean.default()
mean.default
# Get all methods for generic function "plot"
methods("plot")
getAnywhere(plot)  # Display function
rm(list=ls())
lazyfun <- function(arg1, arg2) {  # Define function lazyfun
  2*arg1  # just multiply first argument
}  # end lazyfun
lazyfun(3, 2)  # Bind arguments by position
lazyfun(3)  # Second argument was never evaluated!
lazyfun <- function(arg1, arg2) {  # Define function lazyfun
  cat(arg1, '\n')  # Write to output
  cat(arg2)  # Write to output
}  # end lazyfun
lazyfun(3, 2)  # Bind arguments by position
lazyfun(3)  # First argument written to output
rm(list=ls())
globv <- 1  # Define a global variable
ls(environment())  # Get all variables in environment
func_env <- function() {  # Explore function environments
  locvar <- 1  # Define a local variable
  cat('objects in evaluation environment:\t',
      ls(environment()), '\n')
  cat('objects in enclosing environment:\t',
      ls(parent.env(environment())), '\n')
  cat('this is the enclosing environment:')
  parent.env(environment())  # Return enclosing environment
}  # end func_env
func_env()
environment(func_env)
environment(print)  # Package namespace is the enclosure
rm(list=ls())
globv <- 1  # Define a global variable
probe_scope <- function() {  # Explore function scope
  locvar <- 2*globv  # Define a local variable
  new_globvar <<- 11  # Define a global variable
  cat('objects in evaluation environment:\t',
      ls(environment()), '\n')
  cat('this is a local locvar:\t', locvar, '\n')
  cat('objects in enclosing environment:\n',
      ls(parent.env(environment())), '\n')
  cat('this is globv:\t', globv, '\n')
  globv <- 10  # Define local globv
  cat('this is the local globv:\t', globv, '\n')
}  # end probe_scope
probe_scope()
globv  # Global variable is unaffected
new_globvar  # new_globvar is preserved
locvar  # Local variable is gone!
a <- 1  # Define a variable
# New variable "b" points to value of "a"
b <- a  # Define a new variable
# When "b" is modified, R makes a copy of it
b <- b+1
# Function doubles its argument and returns it
double_it <- function(inputv) {
  inputv <- 2*inputv
  cat("input argument was doubled to:", inputv, "\n")
  inputv
}
double_it(a)
a  # variable "a" is unchanged
setwd("/Users/jerzy/Develop/lecture_slides/data")
rm(list=ls())  # Delete all objects in workspace
ls()  # List objects
# Load objects from file (side effect)
load(file="my_data.RData")
ls()  # List objects
globv <- 1  # Define a global variable
# Explore function scope and side effects
side_effect <- function() {
  cat("global globv =", globv, "\n")
# Define local "globv" variable
  globv <- 10
  cat("local globv =", globv, "\n")
  # Re-define the global "globv"
  globv <<- 2
  cat("local globv =", globv, "\n")
}  # end side_effect
side_effect()
# Global variable was modified as side effect
globv
# Standard infix operator call syntax
2 + 3
# Infix operator applied using prefix syntax
"+"(2, 3)
# Standard bracket operator
vecv <- c(4, 3, 5, 6)
vecv[2]
# Bracket operator applied using prefix syntax
"["(vecv, 2)
# Define infix operator that returns string
'%+%' <- function(a, b) paste(a, b, sep=" + ")
2 %+% 3
2 %+% 3 %+% 4
"hello" %+% 2 %+% 3 %+% "bye"
obj_string <- "hello"
class(obj_string)
# Assign to value returned by "class" function
class(obj_string) <- "string"
class(obj_string)
# Define function last()
last <- function(vecv) {
  vecv[NROW(vecv)]
}  # end last
last(1:10)
# Define replacement function last()
'last<-' <- function(vecv, value) {
  vecv[NROW(vecv)] <- value
  vecv
}  # end last
x <- 1:5
last(x) <- 11
x
# Create functional that accepts a function as input argument
testfun <- function(funn) {
# Calculates statistic on random numbers
  set.seed(1)
  funn(runif(1e4))  # Apply the function name
}  # end testfun
testfun(mean)
testfun(sd)
# Define a power function factory
makefun <- function(parv) {  # Wrapper function
  function(inputv) {  # Anonymous closure
    inputv^parv
  }
}  # end makefun
squarefun <- makefun(2)  # Define square function
squarefun(4)
cubefun <- makefun(3)  # Define cube function
cubefun(2)
cube_rootfun <- makefun(1/3)  # Define cube root function
cube_rootfun(8)
make_counter <- function() {
# Counter function with mutable state
  counter <- 0  # Initialize counter
  cat('counter = ', counter)
  function() {  # Return anonymous advance function
    counter <<- counter + 1  # Advance counter
    cat('counter = ', counter)
  }  # end advance function
}  # end make_counter
advance_counter <- make_counter()  # Create new counter
advance_counter()  # Advance counter
advance_counter()  # Advance counter
advance_counter_two <- make_counter()  # Create another counter
advance_counter_two()  # Advance counter two
advance_counter()  # Advance counter one
advance_counter_two()  # Advance counter two
advance_counter()  # Advance counter one
# Returns the pseudo-random generating function random_generator
# the formal argument 'seed' persists in the evaluation environment of seed_random
seed_random <- function(seed) {  # Seed must be an integer
  random_number <- as.numeric(paste0('0.', seed))  # Initialize
# Random_generator returns a vector of pseudo-random numbers of length length_rand
  random_generator <- function(length_rand=1) {  # Assign function name for recursion
# Returns a vector of pseudo-random numbers of length length_rand
    random_number <<- 4*random_number*(1 - random_number)  # Logistic map
    if (length_rand == 1) {
      return(random_number)
    } else {
      return(c(random_number, random_generator(length_rand - 1)))
    }  # end if
  }  # end random_generator
}  # end seed_random
# Create a random number generating function and set seed
make_random <- seed_random(88)
make_random(10)  #  calculate vector of 10 pseudo-random numbers
ls(environment(make_random))  # List objects in scope of make_random
rm(list=ls())
# The super-assignment operator '<<-' adjusts the balance
# 'balance' exists in open_account evaluation environment
# Bank account example (from Venables) demonstrates mutable states
# 'balance' is persistent between function calls
open_account <- function(balance) {
# Returns function list for account operations
  list(
    deposit = function(amount) {  # Make deposit
      if (amount > 0) {
balance <<- balance + amount  # '<<-' super-assignment operator
cat(amount, "deposited. Your balance is now:",
    balance, "\n")
      } else {
cat("Deposits must be positive!\n")
      }
    },  # end deposit
    withdraw = function(amount) {  # Make withdrawal
      if (amount <= balance) {
balance <<- balance - amount  # '<<-' super-assignment operator
cat(amount, "withdrawn. Your balance is now:",
    balance, "\n")
      } else {
cat("You don't have that much money!\n")
      }
    },  # end withdraw
    get_balance = function() {  # Get balance
      cat("Your current balance is:", balance, "\n")
    }  # end get_balance
  )  # end list
}  # end open_account
# Perform account operations
# open an account with 100 deposit
my_account <- open_account(100)
ls(my_account)  # my_account is a list
# Add my_account to search path
attach(my_account)
withdraw(30)  # Withdrawal to buy groceries
deposit(100)  # Deposit paycheck to account
withdraw(200)  # Withdrawal to buy Gucci bag
get_balance()  # Get account balance
# List objects in scope of get_balance
ls(environment(get_balance))
detach(my_account)  # Remove my_account from search path
# Functional accepts function name and additional argument
testfun <- function(funn, inputv) {
# Produce function name from argument
  funn <- match.fun(funn)
# Execute function call
  funn(inputv)
}  # end testfun
testfun(sqrt, 4)
# String also works because match.fun() converts it to a function
testfun("sqrt", 4)
str(sum)  # Sum() accepts multiple arguments
# Functional can't accept indefinite number of arguments
testfun(sum, 1, 2, 3)
# Functional accepts function name and dots '...' argument
testfun <- function(funn, ...) {
  funn <- match.fun(funn)
  funn(...)  # Execute function call
}  # end testfun
testfun(sum, 1, 2, 3)
testfun(sum, 1, 2, NA, 4, 5)
testfun(sum, 1, 2, NA, 4, 5, na.rm=TRUE)
# Function with three arguments and dots '...' arguments
testfun <- function(inputv, param1, param2, ...) {
  c(inputv=inputv, param1=param1, param2=param2, dots=c(...))
}  # end testfun
testfun(1, 2, 3, 4, 5)
testfun(1, 2, 3, param2=4, param1=5)
# Simple anonymous function
(function(x) (x + 3)) (10)
# Anonymous function passed to testfun
testfun(funn=(function(x) (x + 3)), 5)
# Anonymous function is default value
testfun <- function(..., funn=function(x, y, z) {x+y+z}) {
  funn <- match.fun(funn)
  funn(...)  # Execute function call
}  # end testfun
testfun(2, 3, 4)  # Use default funn
testfun(2, 3, 4, 5)
# funn bound by name
testfun(funn=sum, 2, 3, 4, 5)
# Pass anonymous function to funn
testfun(funn=function(x, y, z) {x*y*z},
    2, 3, 4)
str(sum)  # Sum() accepts multiple arguments
# Sum() can't accept list of arguments
sum(list(1, 2, 3))
str(do.call)  # "what" argument is a function
# Do.call passes list elements into "sum" individually
do.call(sum, list(1, 2, 3))
do.call(sum, list(1, 2, NA, 3))
do.call(sum, list(1, 2, NA, 3, na.rm=TRUE))
# Functional accepts list with function name and arguments
testfun <- function(list_arg) {
# Produce function name from argument
  funn <- match.fun(list_arg[[1]])
# Execute function call uing do.call()
  do.call(funn, list_arg[-1])
}  # end testfun
arg_list <- list("sum", 1, 2, 3)
testfun(arg_list)
# do_call() performs same operation as do.call()
all.equal(
  do.call(sum, list(1, 2, NA, 3, na.rm=TRUE)),
  rutils::do_call(sum, list(1, 2, NA, 3), na.rm=TRUE))
rm(list=ls())
str(apply)  # Get list of arguments
# Create a matrix
matv <- matrix(6:1, nrow=2, ncol=3)
matv
# Sum the rows and columns
rowsumv <- apply(matv, 1, sum)
colsumv <- apply(matv, 2, sum)
matv <- cbind(c(sum(rowsumv), rowsumv),
          rbind(colsumv, matv))
dimnames(matv) <- list(c("colsumv", "row1", "row2"),
                 c("rowsumv", "col1", "col2", "col3"))
matv
str(apply)  # Get list of arguments
matv <- matrix(sample(12), nrow=3, ncol=4)  # Create a matrix
matv
apply(matv, 2, sort)  # Sort matrix columns
apply(matv, 2, sort, decreasing=TRUE)  # Sort decreasing order
matv[2, 2] <- NA  # Introduce NA value
matv
# Calculate median of columns
apply(matv, 2, median)
# Calculate median of columns with na.rm=TRUE
apply(matv, 2, median, na.rm=TRUE)
# VTI percentage returns
retp <- rutils::diffit(log(Cl(rutils::etfenv$VTI)))
library(moments)  # Load package moments
str(moment)  # Get list of arguments
# Apply moment function
moment(x=retp, order=3)
# 4x1 matrix of moment orders
orderv <- as.matrix(1:4)
# Anonymous function allows looping over function parameters
apply(X=orderv, MARGIN=1, FUN=function(orderp) {
  moment(x=retp, order=orderp)
}  # end anonymous function
)  # end apply
# Another way of passing parameters into moment() function
apply(X=orderv, MARGIN=1, FUN=moment, x=retp)
# Function with three arguments
testfun <- function(arg1, arg2, arg3) {
  c(arg1=arg1, arg2=arg2, arg3=arg3)
}  # end testfun
testfun(1, 2, 3)
datav <- as.matrix(1:4)
# Pass datav to arg1
apply(X=datav, MAR=1, FUN=testfun, arg2=2, arg3=3)
# Pass datav to arg2
apply(X=datav, MAR=1, FUN=testfun, arg1=1, arg3=3)
# Pass datav to arg3
apply(X=datav, MAR=1, FUN=testfun, arg1=1, arg2=2)
# Vector of means of numeric columns
sapply(iris[, -5], mean)
# List of means of numeric columns
lapply(iris[, -5], mean)
# Lapply using anonymous function
unlist(lapply(iris,
      function(column) {
        if (is.numeric(column)) mean(column)
      }  # end anonymous function
      )  # end lapply
       )  # end unlist
unlist(sapply(iris, function(column) {
  if (is.numeric(column)) mean(column)}))
sapply(6:10, sqrt)  # Sapply on vector
sapply(list(6, 7, 8, 9, 10), sqrt)  # sapply on list
# Calculate means of iris data frame columns
sapply(iris, mean)  # Returns NA for Species
# Create a matrix
matv <- matrix(sample(100), ncol=4)
# Calculate column means using apply
apply(matv, 2, mean)
# Calculate column means using sapply, with anonymous function
sapply(1:NCOL(matv), function(colnum) {  # Anonymous function
 mean(matv[, colnum])
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
library(zoo)  # Load package zoo
# Show the generic function "merge"
merge
# Show the "merge" method dispatched to "zoo" objects
merge.zoo
library(zoo)  # Load package zoo
# Get all methods for generic function merge()
methods(generic.function="merge")
# Get generic function methods applied to "zoo" objects
methods(class="zoo")
# Define a generic function
gen_sum <- function(a, b, ...) {
  UseMethod("gen_sum")
}  # end gen_sum
# Define method for "numeric" class
gen_sum.numeric <- function(a, b, ...) {
  sum(a, b)
}  # end gen_sum.character
# Define method for "character" class
gen_sum.character <- function(a, b, ...) {
  paste(a, "plus", b)
}  # end gen_sum.character
# Apply gen_sum to "numeric" objects
gen_sum(1, 2)
# Apply gen_sum to "character" objects
gen_sum("a", "b")
# 'cbind' is an internal generic function
cbind
# Define "+" method for "character" class
"+.character" <- function(a, b, ...) {
  paste(a, "plus", b)
}  # end +.character
methods("+")  # view methods for "+" operator
# Define variables with "character" class
char1 <- "a"
char2 <- "b"
class(char1)
char1 + char2  # Add two "character" objects - doesn't work
attributes(char1)  # Doesn't have explicit "character" class - only implicit
char1 <- structure("a", class="character")
char2 <- structure("b", class="character")
attributes(char1)  # Now has explicit "character" class
# Add two "character" objects
char1 + char2
# Define object of class "string"
obj_string <- "how are you today?"
class(obj_string) <- "string"
obj_string
# overload "print" method for string objects
print.string <- function(str_ing) {
  print(
    paste(strsplit(str_ing, split=" ")[[1]],
  collapse=" + "))
}  # end print.string
# methods("print")  # view new methods for "print" function
print(obj_string)
obj_string
# overwrite "+" operator
"+" = function(a, b) {
  if (is.character(a) && is.character(b)) {
    paste(a, "plus", b)
  } else {
    .Primitive("+") (a, b)
  }
}
methods("+")  # view methods for "+" operator
# Add two "numeric" objects
1 + 2
# Add two "character" objects
"a" + "b"
# overwrite "+" operator with a generic function
"+" <- function(a, b, ...) {
  UseMethod("+")
}  # end gen_sum
# Define method for "numeric" class
"+.numeric" <- function(a, b, ...) {
  sum(a, b)
}  # end gen_sum.character
# Define method for "character" class
"+.character" <- function(a, b, ...) {
  paste(a, "plus", b)
}  # end gen_sum.character
methods("+")  # view methods for "+" operator
# Add two "numeric" objects
1 + 2
# Add two "character" objects
"a" + "b"
cbind.ts  # Can't view non-visible method
stats::cbind.ts  # Can't view non-visible method
stats:::cbind.ts  # Display non-visible method
getAnywhere(cbind.ts)  # Display non-visible method
rm(list=ls())
new_zoo <- zoo(rnorm(10), order.by=(Sys.Date() + 0:9))
# Coerce "zoo" object to new class "zoo_xtra"
class(new_zoo) <- "zoo_xtra"
class(new_zoo)
methods(generic.function="length")
length  # Primitive function
# Define "length" method for class "zoo_xtra"
length.zoo_xtra <- function(in_ts) {
  cat("length of zoo_xtra object:\n")
# Unclass object, then calculate length
  NROW(unclass(in_ts))
}  # end length.zoo_xtra
NROW(new_zoo)  # Apply "length" method to "zoo_xtra" object
methods(generic.function="length")
# Define "last" method for class "zoo_xtra"
last.zoo_xtra <- function(in_ts) {
  in_ts[NROW(in_ts)]
}  # end last.zoo_xtra
last(new_zoo)  # Doesn't work
last.zoo_xtra(new_zoo)  # Works
# Define a generic function
last <- function(a, b, ...) {
  UseMethod("last")
}  # end last
last(new_zoo)  # Now works
# Define generic "string" class converter
as.string <- function(str_ing, ...)
  UseMethod("as.string")
# Default "string" class converter
as.string.default <- function(str_ing, ...)
  structure(str_ing, class="string", ...)
# Numeric "string" class converter
as.string.numeric <- function(str_ing, ...)
  structure(as.character(str_ing), class="string", ...)
# "string" class checker
is.string <- function(str_ing)
  inherits(x=str_ing, what="string")
# Define "string" object
obj_string <- as.string("how are you today?")
obj_string
is.string(obj_string)
is.string("hello")
as.string(123)
is.string(as.string(123))
rm(list=ls())
library(xts)
new_xts <- xts(rnorm(10), order.by=(Sys.Date() + 0:9))
class(new_xts)  # Class attribute is a vector
# "last" is a generic function from package "xts"
last
methods(generic.function="last")
last(new_xts)  # Apply "last" method from "xts" class
# Derive object "xts_xtra" from "xts" object
class(new_xts) <- c("xts_xtra", class(new_xts))
class(new_xts)  # Class attribute is a vector
# "xts_xtra" object inherits "last" method from "xts" class
last(new_xts)
# Define new "last" method for class "xts_xtra"
last.xts_xtra <- function(in_ts) {
  cat("last element of xts_xtra object:\n")
  drop(in_ts[NROW(in_ts), ])
}  # end last.xts_xtra
last(new_xts)  # Apply "last" from "xts_xtra" class
# Define "last" method for class "xts_xtra"
last.xts_xtra <- function(in_ts) {
  cat("last element of xts_xtra object:\n")
  drop(NextMethod())
}  # end last.xts_xtra
last(new_xts)  # Apply "last" from "xts_xtra" class
