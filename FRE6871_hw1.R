#################################
### FRE6871 HW #1 due Sep 28, 2015
#################################
# Max score 60pts

# Please write in this file the R code needed to perform the tasks below, 
# rename it to your_name_hw1.R
# and send this file to Jaimin Doshi (jbd316@nyu.edu)


##################################
# 1. (15pts) Create a vector of permutations of integers from 1 to 90, 
# and call it "vec_tor", 
# extract every third element of "vec_tor", starting with the first one, 
# into a single column matrix called "mat_rix",
# hint: you can use functions sample(), seq() and function matrix(), 
# with the proper "byrow" argument,

### write your code here


# 2. (15pts) extract a vector of odd index elements of "vec_tor" (first, third, etc), 
# and a vector of even index elements (second, fourth, etc.), 
# calculate the scalar ("inner") product of the two vectors, 
# the value should be a vector with a single element, not a matrix,
# hint: you can use the "%*%" operator, and the functions seq(), drop(), 
# or function matrix(), with the proper "byrow" argument,

### write your code here



##################################
# 3. (15pts) create a function called get_index(), that 
# returns the indices of the TRUE elements of a boolean vector,
# get_index() should be equivalent to the function which(), 
# when applied to boolean vectors,
# hint: you can use functions length(), seq_along(), 
# and then apply vector subsetting,

### write your code here

# apply the function get_index() to a boolean vector, and 
# compare the result with using function which(),

vec_tor <- sample(1:9)
get_index(vec_tor==5)
which(vec_tor == 5)



##################################
# 4. (15pts) create a numeric vector of length 10 containing random normal 
# variates, using rnorm(),

### write your code here

# assign the names: "el1, ..., el10", to the vector elements, 
# you can use function paste(), with the proper "sep" argument, 
# you can't use function c()

### write your code here

# change the vector names to: "num1, ..., num10", using the function gsub(), 
# (you can't use functions c() or paste())

### write your code here

# extract the element named "num4",

### write your code here

# change the vector names to: "my.num1, ..., my.num10", 
# using the function paste(), with the proper "sep" argument,

### write your code here

# change the first vector element name back to: "num1", 
# using the function strsplit(), with the proper "split" argument,
# hint: strsplit() returns a list, subset it using [[1]],

### write your code here

# calculate the indices of the elements that are greater than 
# or equal to -0.5 and less than 1.0,

### write your code here

# extract the elements that are greater than or equal to -0.5 and less than 1.0,

### write your code here

# calculate the mean and standard deviation of the vector elements,

### write your code here

# combine this vector with the vector "31:33",

### write your code here

