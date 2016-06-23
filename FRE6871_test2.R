#################################
### FRE6871 Test #2 May 2, 2016
#################################
# Max score 110pts

# Please write in this file the R code needed to perform the tasks below, 
# rename the file to your_name_test2.R
# and upload it to NYU Classes,


############## Part I
# Summary: Create a function that calculates a contingency 
# table for a single vector or factor, similar to function 
# table(). 
# 
# 1. (20pts) Create a function that calculates a contingency 
# table, called ta_ble(). 
# ta_ble() should accept a single argument, either a vector 
# or a factor, and return a named vector containing the 
# number of times an element occurs in the input argument.  
# The names of the output vector should be the elements of 
# the input argument. 
# The order of the output vector elements is not important. 
# You can't use function table(), 
# You can use functions sapply(), unique(), sum(), and an 
# anonymous function.

### write your code here

# Call ta_ble() as follows, and compare its output to that 
# of table(), to verify that they produce similar output:

vec_tor <- sample(c("a", "b", "c", "d"), 
                  size=20, replace=TRUE)
ta_ble(vec_tor)
table(vec_tor)



############## Part II
# Summary: Perform loops over vectors, and then perform 
# the equivalent vectorized operations over the vectors.

# First create a vector of random numbers as follows: 

set.seed(1121)
vec_tor <- rnorm(10)

# 1. (20pts) Perform a for() loop to replace those elements 
# of vec_tor that are greater than "1" with the number "5". 
# You can use functions for() and seq_along(), 

### write your code here

# vec_tor should be like this:
# [1]  0.1449583  0.4383221  0.1531912  5.0000000  5.0000000 -0.8118832
# [7]  0.1602680  0.5858923  0.3600880 -0.0253084


# 2. (20pts) Perform an sapply() loop over vec_tor, and 
# perform exactly the same calculations as in p.1. 
# You must use either functions apply() lapply(), or sapply(). 
# You can also use functions length() and seq_along(), 
# and an anonymous function.

set.seed(1121)
vec_tor <- rnorm(10)

### write your code here


# 3. (20pts) Perform the same calculations as in p.1, 
# but only using vectorized operations (logical 
# operators and subsetting). 
# You cannot use any for() or apply() loops. 

set.seed(1121)
vec_tor <- rnorm(10)

### write your code here


# 4. (20pts) Perform the same calculations as in p.1, 
# but using function ifelse(). 
# You cannot use any for() or apply() loops. 
# You must use function ifelse(). 

set.seed(1121)
vec_tor <- rnorm(10)

### write your code here


# 5. (10pts) Benchmark the CPU time used by the code 
# from p.2 with the code from p.4, using the function 
# microbenchmark(). 
# Assign the names "s_apply" and "if_else" to each method. 

library(microbenchmark)

### write your code here
