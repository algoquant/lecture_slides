#################################
### FRE6871 Homework #2 due Oct 13, 2015
#################################
# Max score 45pts

# Please write in this file the R code needed to perform the tasks below, 
# rename it to your_name_hw2.R
# and upload the file to NYU Classes


############## Part I
# 1. (15pts) Create a function called match_matrix(), similar to match(), 
# but which accepts matrix arguments, as well as vectors. 
# match_matrix() should return the row and column indices of the first 
# element of its second argument, that matches its first argument,
# hint: you can use function which(), with the argument "arr.ind=TRUE", 

### write your code here

# call the function match_matrix() as follows, to make sure it works properly:

mat_rix <- matrix(1:6, ncol=3)
match_matrix(5, mat_rix)



############## Part II
# Summary: create a function called find_interval(),
# that replicates the function findInterval(),

# 1. (30pts) 
# find_interval() should accept a vector argument called "vec_tor", 
# containing numeric values, which should be classified into intervals,
# according to break_points, 
# should also accept a vector of breakpoints called "break_points", 
# which determines the intervals,
# find_interval() should return an integer vector of length equal 
# to "vec_tor", specifying the intervals to which the numeric values 
# contained in "vec_tor" belong, 
# hint: you can perform a for() loop over break_points, 
# you can use functions integer(), length(), seq_along(), 
# logical operators, and a for() loop

### write your code here

# call find_interval() on a vector and compare it to findInterval(), 
# to verify that it works correctly,

find_interval(vec_tor=1:8, break_points=c(3, 5, 7, 9))
findInterval(x=1:8, vec=c(3, 5, 7, 9))

# call find_interval() on a vector of numbers and a vector of breakpoints, 
# to verify that it produces exactly the same output as findInterval(), 
# use function identical(),

### write your code here

