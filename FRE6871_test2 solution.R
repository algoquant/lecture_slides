#################################
### FRE6871 Test #2 Solutions May 2, 2016
#################################
# Max score 110pts

# The below solutions are examples,
# Slightly different solutions are also possible.

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

ta_ble <- function(in_put) {
  sapply(unique(in_put), 
         function(le_vel) {
           sum(in_put==le_vel)
         }) # end sapply
}  # end ta_ble

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

for(in_dex in seq_along(vec_tor)) {
  if (vec_tor[in_dex]>1)
    vec_tor[in_dex] <- 5
}  # end for

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
vec_tor <- sapply(vec_tor, 
                  function(ele_ment) {
                    if (ele_ment>1)
                      5
                    else
                      ele_ment
                  })  # end sapply
# or
vec_tor <- sapply(seq_along(vec_tor), 
                  function(in_dex) {
                    if (vec_tor[in_dex]>1)
                      vec_tor[in_dex] <- 5
                    else
                      vec_tor[in_dex]
                  })  # end sapply


# 3. (20pts) Perform the same calculations as in p.1, 
# but only using vectorized operations (logical 
# operators and subsetting). 
# You cannot use any for() or apply() loops. 

set.seed(1121)
vec_tor <- rnorm(10)
vec_tor[vec_tor>1] <- 5


# 4. (20pts) Perform the same calculations as in p.1, 
# but using function ifelse(). 
# You cannot use any for() or apply() loops. 
# You must use function ifelse(). 

set.seed(1121)
vec_tor <- rnorm(10)
vec_tor <- ifelse(vec_tor>1, 5, vec_tor)


# 5. (10pts) Benchmark the CPU time used by the code 
# from p.2 with the code from p.4, using the function 
# microbenchmark(). 
# Assign the names "s_apply" and "if_else" to each method. 

library(microbenchmark)
summary(microbenchmark(
  s_apply=sapply(seq_along(vec_tor), function(in_dex) {
    if (vec_tor[in_dex]>1)
      vec_tor[in_dex] <- 5
    else
      vec_tor[in_dex]
  }),  # end sapply
  if_else=ifelse(vec_tor>1, 5, vec_tor), 
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary



