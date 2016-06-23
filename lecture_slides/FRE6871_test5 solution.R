#################################
### FRE6871 Test #5 Solutions Oct 26, 2015
#################################
# Max score 50pts

# The below solutions are examples,
# Slightly different solutions are also possible.

##############
# Summary: perform vectorized operations on vectors.

# First create a vector of random numbers as follows: 

set.seed(1121)
vec_tor <- rnorm(10)


# 1. (10pts) Perform a for() loop to replace those elements 
# of "vec_tor" that are greater than "1" with the number "5". 
# You can use functions for() and seq_along(), 

for(in_dex in seq_along(vec_tor)) {
  if (vec_tor[in_dex]>1)
    vec_tor[in_dex] <- 5
}  # end for

# "vec_tor" should be like this:
# [1]  0.1449583  0.4383221  0.1531912  5.0000000  5.0000000 -0.8118832
# [7]  0.1602680  0.5858923  0.3600880 -0.0253084


# 2. (10pts) Perform an sapply() loop to perform the same 
# calculations as in p.1. 
# You can use functions sapply() and seq_along(), 
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


# 3. (10pts) Perform the same calculations as in p.1, 
# but only using vectorized operations. 
# You cannot use any for() or apply() loops, 

set.seed(1121)
vec_tor <- rnorm(10)
vec_tor[vec_tor>1] <- 5


# 4. (10pts) Perform the same calculations as in p.1, 
# but using function ifelse(). 
# You must use function ifelse(), 

set.seed(1121)
vec_tor <- rnorm(10)
vec_tor <- ifelse(vec_tor>1, 5, vec_tor)


# 5. (10pts) Benchmark the CPU time used by the code from p.2 
# with the code from p.4, using the function microbenchmark(). 
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

