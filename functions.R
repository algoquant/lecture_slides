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
# VTI percentage returns
re_turns <- rutils::diff_it(log(Cl(rutils::etf_env$VTI)))
# calc_skew() calculates skew of time series of returns
# Default is normal time series
calc_skew <- function(re_turns=rnorm(1000)) {
  # Number of observations
  n_rows <- NROW(re_turns)
  # Standardize re_turns
  re_turns <- (re_turns - mean(re_turns))/sd(re_turns)
  # Calculate skew - last statement automatically returned
  n_rows*sum(re_turns^3)/((n_rows-1)*(n_rows-2))
}  # end calc_skew
# Calculate skew of DAX returns
# Bind arguments by name
calc_skew(re_turns=re_turns)
# Bind arguments by position
calc_skew(re_turns)
# Use default value of arguments
calc_skew()
str(plot)  # Dots for additional plot parameters
bind_dots <- function(in_put, ...) {
  paste0("in_put=", in_put, ", dots=", paste(..., sep=", "))
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
lazy_func <- function(arg1, arg2) {  # Define function lazy_func
  2*arg1  # just multiply first argument
}  # end lazy_func
lazy_func(3, 2)  # Bind arguments by position
lazy_func(3)  # Second argument was never evaluated!
lazy_func <- function(arg1, arg2) {  # Define function lazy_func
  cat(arg1, '\n')  # Write to output
  cat(arg2)  # Write to output
}  # end lazy_func
lazy_func(3, 2)  # Bind arguments by position
lazy_func(3)  # First argument written to output
rm(list=ls())
glob_var <- 1  # Define a global variable
ls(environment())  # Get all variables in environment
func_env <- function() {  # Explore function environments
  loc_var <- 1  # Define a local variable
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
glob_var <- 1  # Define a global variable
probe_scope <- function() {  # Explore function scope
  loc_var <- 2*glob_var  # Define a local variable
  new_globvar <<- 11  # Define a global variable
  cat('objects in evaluation environment:\t',
      ls(environment()), '\n')
  cat('this is a local loc_var:\t', loc_var, '\n')
  cat('objects in enclosing environment:\n',
      ls(parent.env(environment())), '\n')
  cat('this is glob_var:\t', glob_var, '\n')
  glob_var <- 10  # Define local glob_var
  cat('this is the local glob_var:\t', glob_var, '\n')
}  # end probe_scope
probe_scope()
glob_var  # Global variable is unaffected
new_globvar  # new_globvar is preserved
loc_var  # Local variable is gone!
a <- 1  # Define a variable
# New variable "b" points to value of "a"
b <- a  # Define a new variable
# When "b" is modified, R makes a copy of it
b <- b+1
# Function doubles its argument and returns it
double_it <- function(in_put) {
  in_put <- 2*in_put
  cat("input argument was doubled to:", in_put, "\n")
  in_put
}
double_it(a)
a  # variable "a" is unchanged
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
# Standard infix operator call syntax
2 + 3
# Infix operator applied using prefix syntax
"+"(2, 3)
# Standard bracket operator
vec_tor <- c(4, 3, 5, 6)
vec_tor[2]
# Bracket operator applied using prefix syntax
"["(vec_tor, 2)
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
last <- function(vec_tor) {
  vec_tor[NROW(vec_tor)]
}  # end last
last(1:10)
# Define replacement function last()
'last<-' <- function(vec_tor, value) {
  vec_tor[NROW(vec_tor)] <- value
  vec_tor
}  # end last
x <- 1:5
last(x) <- 11
x
# Create functional that accepts a function as input argument
func_tional <- function(func_name) {
# Calculates statistic on random numbers
  set.seed(1)
  func_name(runif(1e4))  # Apply the function name
}  # end func_tional
func_tional(mean)
func_tional(sd)
# Define a power function factory
make_func <- function(arg_param) {  # Wrapper function
  function(in_put) {  # Anonymous closure
    in_put^arg_param
  }
}  # end make_func
square_func <- make_func(2)  # Define square function
square_func(4)
cube_func <- make_func(3)  # Define cube function
cube_func(2)
cube_root_func <- make_func(1/3)  # Define cube root function
cube_root_func(8)
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
# VTI percentage returns
re_turns <- rutils::diff_it(log(Cl(rutils::etf_env$VTI)))
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
apply(X=moment_orders, MARGIN=1, FUN=moment, x=re_turns)
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
