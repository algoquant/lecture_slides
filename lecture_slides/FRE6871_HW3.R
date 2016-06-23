#################################
### FRE6871 Homework #2 due Oct 19, 2015
#################################
# Max score 50pts

# Please write in this file the R code needed to perform the tasks below, 
# rename it to your_name_hw3.R
# and upload the file to NYU Classes


############## Part I
# Summary: create a functional called cumu_late(), 
# which calculates cumulative values (sums, products, etc.) 
# over a vector argument, 
# and replicates the functions cumsum() and cumprod().

# 1. (20pts) create a functional called cumu_late(), that accepts 
# a vector argument "vec_tor", and a function argument "func_tion",
# cumu_late() should return a vector with elements equal to the 
# application of "func_tion" to the sub-vectors of "vec_tor", 
# cumu_late() should be equivalent to the functions cumsum(), cumprod(), etc.,
# you can use the functions sapply(), seq_along(), match.fun(), and do.call(), 
# and an anonymous function,

### write your code here

# call cumu_late() as follows, to verify that it is correct: 

cumu_late(1:5, sum)
cumu_late(1:5, prod)

# the above should produce exactly the same as:

cumsum(1:5)
cumprod(1:5)

# run the following, to verify that cumu_late() produces exactly 
# the same result as cumsum() and cumprod(), 

identical(
  cumu_late(1:5, sum),
  cumsum(1:5)
)  # end identical

identical(
  cumu_late(1:5, prod),
  cumprod(1:5)
)  # end identical



############## Part II
# Summary: perform an apply() loop, and pass values to a function
# through the dots "..." argument of the functional apply().

# define a single-column matrix called "da_ta" as follows:

da_ta <- matrix(1:4)

# define a function called "my_func" with three arguments 
# as follows:

my_func <- function(arg1, arg2, arg3) {
  c(arg1=arg1, arg2=arg2, arg3=arg3)
}  # end my_func

# 1. (20pts) Perform an apply() loop, and apply my_func() to "da_ta", 
# so that "da_ta" is passed to "arg2". 
# You must also pass the values "5" and "6" to "arg1" and "arg3", 
# using the dots "..." argument of the functional apply(). 

### write your code here

# the output of appply() should be a matrix with three rows named,
# "arg1", "arg2", "arg3", like this:
#       [,1] [,2] [,3] [,4]
# arg1    5    5    5    5
# arg2    1    2    3    4
# arg3    6    6    6    6

