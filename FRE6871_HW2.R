#################################
### FRE6871 Homework #2 due April 25, 2016
#################################
# Max score 80pts

# Please write in this file the R code needed to perform the tasks below, 
# rename it to your_name_hw2.R
# and upload the file to NYU Classes


############## Part I
# Summary: Perform sapply() loops over the parameters of 
# function rnorm(), both with, and without using an 
# anonymous function. 

# 1. (10pts) Define two named vectors of parameters called  
# me_ans and std_devs, containing mean and sd parameter values 
# to be passed to function rnorm(). 
# The values of mean and sd should be:
# mean <- -1:1
# sd <- 1:3
# your code should produce the following named vectors: 
# me_ans= 
#   mean=-1  mean=0  mean=1 
#        -1       0       1 
# std_devs= 
#   sd=1 sd=2 sd=3 
#      1    2    3 
# You can use functions paste0(), names(), and/or structure(), 

### write your code here


# 2. (20pts) Perform an sapply() loop over the vector 
# me_ans, and pass its values to the mean parameter of 
# function rnorm(). 
# Pass the parameter "n=2" to rnorm(), so that each call 
# produces two random numbers. 
# You must use functions rnorm() and sapply(). 
# You must perform the calculation in two different ways: 
# the first way without using an anonymous function, and 
# the second way using an anonymous function.

set.seed(1121)

# first without using an anonymous function:

### write your code here

# second way using an anonymous function: 

### write your code here

# your code should produce a matrix like this: 
#        mean=-1 mean=0 mean=1
#   [1,] -0.8550 0.1532 2.9995
#   [2,] -0.5617 1.0849 0.1881


# Perform an sapply() loop over the vector std_devs, and 
# pass its values to the sd parameter of function rnorm(). 
# Pass the parameter "n=2" to rnorm(), so that each call 
# produces two random numbers. 
# You must use functions rnorm() and sapply(). 
# You must perform the calculation in two different ways: 
# the first way without using an anonymous function, and 
# the second way using an anonymous function.

set.seed(1121)

# first without using an anonymous function:

### write your code here

# second way using an anonymous function: 

### write your code here

# your code should produce a matrix like this: 
#        sd=1   sd=2  sd=3
# [1,] 0.1450 0.3064  5.999
# [2,] 0.4383 2.1699 -2.436


# 3. (20pts) Calculate a matrix of random numbers, by 
# performing two sapply() loops, first over the vector 
# me_ans, and a second loop over the vector std_devs, 
# and pass the values of me_ans and std_devs to the 
# mean and sd parameters of the function rnorm(). 
# Pass the parameter "n=1" to the function rnorm(). 
# You must use functions rnorm() and sapply(). 
# hint: you can use an anonymous function with two 
# arguments. The anonymous function should contain 
# the second sapply() loop. 

set.seed(1121)


### write your code here

# your code should produce a matrix like this: 
#      mean=-1 mean=0 mean=1
# sd=1 -0.8550  1.085  1.160
# sd=2 -0.1234  3.999  2.172
# sd=3 -0.5404 -2.436  2.080



############## Part II
# Summary: Create a functional called apply_agg(), that 
# applies an aggregation function over columns of data. 
# 
# 1. (30pts) apply_agg() should accept two arguments:
# - da_ta - a data object with multiple columns (matrix, 
#   data frame, or time series). 
# - agg_regate() - an aggregation function that can return 
#   a vector. 
# apply_agg() should first verify if da_ta has a dim attribute. 
# If da_ta does not have a dim attribute, then apply_agg() 
# should directly apply the function agg_regate() to the da_ta 
# and return it. 
# If da_ta does have a dim attribute, then apply_agg() should 
# perform an apply loop over the columns of da_ta, and apply 
# the function agg_regate() to each column. 
# apply_agg() should return a vector or a matrix, with its 
# elements equal to the results of agg_regate(). 
# hint: you can use functions is.null(), dim(), sapply()
# and and if() statement. 


### write your code here

# call apply_agg() as follows, to verify that it works correctly: 
apply_agg(1:5, sum)
apply_agg(EuStockMarkets[, 1], mean)
apply_agg(EuStockMarkets, mean)


