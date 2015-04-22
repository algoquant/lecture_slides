#################################
### HW #3 Solution FRE6871 R in Finance
#################################
# Max score xxx pts

# The below solutions are examples,
# Slightly different solutions are also possible.



##################################
# 1. (30pts) create a function called "my_sqrt" that calculates the square root of its single argument,
# "my_sqrt" should check if the input is both numeric and positive,
# - if the input is numeric and positive, then "my_sqrt" should return the square root,
# - if the input is numeric and negative, then "my_sqrt" should broadcast a warning using "cat", 
# and return the square root of the absolute value,
# - if the input is not numeric, then "my_sqrt" should broadcast a different warning using "cat", 
# and return NA,
# Use "if" and "else" statements.
my_sqrt <- function(arg_var) {
  if (is.numeric(arg_var) && arg_var>=0) {
    sqrt(arg_var)
  } else if (is.numeric(arg_var)) {
    cat("negative input!\t")
    sqrt(abs(arg_var))
  } else {
    cat("not numeric input!\t")
    NULL
  }
}  # end my_sqrt
my_sqrt(4)
my_sqrt(-4)
my_sqrt("a")


##################################
# 2. (15pts) create a function called "mult_dots", which takes a '...' argument, 
#     and a single numeric argument called "fac_tor", as follows: function (..., fac_tor),
#     The function "mult_dots" should sum up the '...' argument, 
#     multiply the sum by "fac_tor", and return the result,
mult_dots <- function (..., fac_tor) {
  fac_tor*sum(...)
}  #  end mult_dots

#     call the function "mult_dots" on a list of arguments,
#     so that it adds up "3, 4, 5", and then multiplies the sum by "2",
mult_dots(3, 4, 5, fac_tor=2)


##################################
# 3. (15pts) create a vector of 20 random normal numbers:
vec_tor <- rnorm(20)
# find the indices of numbers greater than 1:
which(vec_tor>1)
# find the numbers greater than 1:
vec_tor[which(vec_tor>1)]
# find the index of the max number:
which.max(vec_tor)
# find the max number (don't use max()):
vec_tor[which.max(vec_tor)]



