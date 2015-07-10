#################################
### FRE6871 Test #1 06/15/15
#################################
# Max score 45pts

# Please write in this file the R code needed to perform the tasks below, 
# rename the file to your_name_test1.R
# and send the file to Jaimin Doshi (jbd316@nyu.edu)


##################################
# 1. (15pts) create a function called get_index(), that is equivalent 
# to function which(), when applied to vectors,
# hint: you can use function length() and then apply vector subsetting,

### write your code here



# apply the function get_index() to a boolean vector, and 
# compare the result with using function which(),

### write your code here



##################################
# 2. (15pts) Create a vector of 90 integers, from 1 to 90, and call it "vec_tor", 
# extract every third element of "vec_tor", starting with the first one, 
# into a single column matrix called "mat_rix",
# hint: you can use function matrix(), with the proper "byrow" argument,

### write your code here


# extract a vector of odd elements of "vec_tor", and a vector of even elements,
# calculate the scalar ("inner") product of the two vectors,
# the value should be a vector with a single element, not a matrix,
# hint: you can use function matrix(), with the proper "byrow" argument,
# use function drop(),

### write your code here




##################################
# 3. (15pts) Create a matrix called "mat_rix", as follows:
mat_rix <- matrix(1:6, ncol=3)

# assign to "mat_rix" row names "row1", "row2", and 
# column names "col1", "col2", "col3",
# use the functions rownames(), colnames(), and/or dimnames(), 

### write your code here


# change the names of the rows to "first_row" and "second_row",
# use the function dimnames(), but not the function rownames(),

### write your code here



