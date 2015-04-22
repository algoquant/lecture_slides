#################################
### HW #2 FRE6871 R in Finance
#################################
# Max score 60pts

# Please write in this file the R code needed to perform the tasks below, 
# rename it to your_name_hw2.R
# and send this file to Harjinder Singh (harjinder.singh@nyu.edu)


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
### write your code here
}  # end my_sqrt

# call the function "my_sqrt" as follows to verify it works properly:
my_sqrt(4)
my_sqrt(-4)
my_sqrt("a")


##################################
# 2. (15pts) create a function called "mult_dots", which takes a '...' argument, 
#     and a single numeric argument called "fac_tor", as follows: function (..., fac_tor),
#     The function "mult_dots" should sum up the '...' argument, 
#     multiply the sum by "fac_tor", and return the result,

mult_dots <- function (..., fac_tor) {
### write your code here
}  #  end mult_dots

#     call the function "mult_dots" on a list of arguments,
#     so that it adds up "3, 4, 5", and then multiplies the sum by "2",

### write your code here



##################################
# 3. (15pts) create a vector of 20 random normal numbers:
vec_tor <- rnorm(20)

# find the indices of numbers greater than 1:
### write your code here

# find the numbers greater than 1:
### write your code here

# find the index of the max number:
### write your code here

# find the max number (don't use max()):
### write your code here



