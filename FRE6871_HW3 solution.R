#################################
### FRE6871 Homework #3 Solution due Oct 19, 2015
#################################
# Max score 40pts

# The below solutions are examples,
# Slightly different solutions are also possible.


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

cumu_late <- function(vec_tor, func_tion) {
# validate function name
  func_tion <- match.fun(func_tion)
  sapply(X=seq_along(vec_tor), 
         FUN=function(in_dex) {
           do.call(what=func_tion, args=list(vec_tor[1:in_dex]))
# or simply:
#           func_tion(vec_tor[1:in_dex])
         })  # end sapply
}  # end cumu_late

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

apply(X=da_ta, MARGIN=1, my_func, arg1=5, arg3=6)

# the output of appply() should be a matrix with three rows named,
# "arg1", "arg2", "arg3", like this:
#       [,1] [,2] [,3] [,4]
# arg1    5    5    5    5
# arg2    1    2    3    4
# arg3    6    6    6    6



