#################################
### FRE6871 Homework #1 due April 18, 2016
#################################
# Max score 70pts

# Please write in this file the R code needed to perform the tasks below, 
# rename it to your_name_hw1.R
# and upload the file to NYU Classes


############## Part I
# 1. (10pts) 
# Summary: Write code for performing operations in the R workspace, 
# change options settings and display them.

# remove all objects in the workspace:

### write your code here

# set the max number of rows printed to console equal to 80:
# you can use function options(),

### write your code here

# show the max number of rows printed to console:
# you can use function options(),

### write your code here

# set the number of digits printed for numeric values equal to 3:
# you can use function options(),

### write your code here

# show the number of digits printed to console for numeric values:
# you can use function options(),

### write your code here

# display today's date and time in the format: 
# "Today is April 05, 2016 at 12:38:36"

### write your code here

# Create objects called var1, var2, var3, 
# and assign the values rnorm(1) to them: 
var1 <- rnorm(1)
var2 <- rnorm(1)
var3 <- rnorm(1)

# list all objects with names starting with "v". 
# hint: you can use function glob2rx() and function 
# ls() with the "pattern" argument,

### write your code here

# save all objects with names ending with "1" to a file 
# called "vobjects.RData" in your cwd. 
# hint: you can use function save() with the "list" argument,

### write your code here

# remove all objects with names starting with "v": 

### write your code here



############## Part II
# Summary: create a function called which_true(), which calculates 
# the indices of the TRUE elements of a boolean vector. 
# which_true() should produce the same result as function which(), 
# when applied to boolean vectors. 
# Implement which_true() using two different methods. 

# 1. (20pts) First method: you must perform a for() loop. 
# hint: you can first create an empty integer vector, 
# and then perform a for() loop to populate it with the 
# index values. 
# you can use functions integer(), seq_along(), c(). 

### write your code here


# 2. (20pts) Second method: you cannot perform any type 
# of loop, only vectorized functions, 
# hint: you can use functions length() or seq_along(), 
# and then apply vector subsetting,

### write your code here

# apply the function which_true() to a boolean vector, and 
# compare the result with using function which(), to verify 
# that it works correctly:

set.seed(1121)
vec_tor <- sample(1:20, replace=TRUE)
which_true(vec_tor==18)
which(vec_tor==18)



############## Part III
# Summary: Create a function called kur_tosis(), for calculating 
# the kurtosis of a time series of returns (a vector of data). 

# 1. (10pts) The function kur_tosis() should accept a single numeric 
# argument called da_ta. 
# The function kur_tosis() should verify that da_ta is numeric, and 
# if it's not, then it should produce a warning and return NULL. 
# If da_ta is numeric, then kur_tosis() should calculate the kurtosis 
# of da_ta and return it. 
# The argument da_ta should be assigned a default value equal to a 
# vector of 1000 random numbers taken from the standard normal 
# distribution. 
# You can use functions is.numeric(), warning(), paste(), return(), 
# length(), mean(), sd(), and sum(). 

### write your code here


# 2. (10pts) Use the function kur_tosis() to calculate the kurtosis  
# of DAX returns from the dataset EuStockMarkets. 
# Next, calculate the kurtosis of a vector of normal random numbers, 
# of the same length as the DAX returns. 
# Next, calculate the kurtosis of a vector of random numbers taken 
# from the t-distribution with four degrees of freedom, of the same 
# length as the DAX returns. 
# You can use the functions rt(), rnorm(), and length():

ts_rets <- 100*diff(log(EuStockMarkets[, 1]))

# call kur_tosis() as follows, to verify that it works correctly: 
kur_tosis(da_ta="hello")
kur_tosis()

# calculate kurtosis of DAX returns:
kur_tosis(da_ta=ts_rets)

# calculate kurtosis of normal returns:

### write your code here

# calculate kurtosis of t-distribution returns:

### write your code here


