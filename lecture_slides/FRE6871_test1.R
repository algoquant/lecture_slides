#################################
### FRE6871 Test #1 Sep 28, 2015
#################################
# Max score 70pts

# Please write in this file the R code needed to perform the tasks below, 
# rename the file to your_name_test1.R
# and upload the file to NYU Classes


##############
# Summary: perform an apply() loop to calculate the max and min 
# values of every column of a matrix. 

# create a matrix as follows:
set.seed(1121)
mat_rix <- matrix(sample(16), nrow=4, ncol=4)
# introduce NA value
mat_rix[3, 2] <- NA

# 1. (5pts) assign to "mat_rix" the column names "col1", "col2", etc.,
# you can use functions colnames(), ncol(), and paste0(), 

### write your code here


# 2. (20pts) perform a single apply() loop over the columns 
# of "mat_rix", 
# the output should be a matrix with two rows named "max" and "min", 
# and with the same number of columns as "mat_rix", 
# the output matrix from apply() should not contain any NAs, 
# to get full credit you must pass the argument "na.rm=TRUE"
# into max() and min() through the dots argument of the 
# apply() function, 
# you must use functions apply(), max(), min(), c(), 
# you can only perform a single apply() loop, 
# you can use an anonymous function, 

### write your code here



##############
# Summary: extract and filter the elements of a matrix. 

# 1. (5pts) create a numeric vector of length 15 containing 
# random normal variates (rnorm), 
# coerce the vector into a matrix of 5 rows and 3 columns,
# call the matrix "mat_rix", 
# you can use functions matrix() and dim(), 

### write your code here

# 2. (5pts) extract all the elements of "mat_rix" that are greater than 1.0,

### write your code here

# 3. (5pts) calculate the row and column indices of all the elements 
# of "mat_rix" that are greater than 1.0,
# you can use function which() with argument "arr.ind", 

### write your code here

# 4. (5pts) calculate the sums of all the rows and columns, 
# the result should be two vectors, 
# you can use functions apply() and sum(), 

### write your code here

# 5. (5pts) bind the vectors of sums to "mat_rix", as extra 
# rows and columns, respectively, 
# you can use functions cbind() and rbind(), 

### write your code here

# 6. (15pts) assign names to the rows and columns follows: 
# "col_sums", "row1", "row2", etc., 
# and 
# "row_sums", "col1", "col2", etc., 
# you can use functions c(), dimnames(), list(), nrow(), ncol(), 
# and paste0(), 

### write your code here

