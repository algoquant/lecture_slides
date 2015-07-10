#################################
### FRE6871 Test #3 July 6, 2015
#################################
# Max score 90pts

# Please write in this file the R code needed to perform the tasks below, 
# rename the file to your_name_test3.R
# and upload it to NYU Classes,


##################################
# 1. (15pts) Calculate a vector of means of the numeric columns 
# of the "iris" data frame,
# calculate the means of only those columns that are numeric,
# the output must be a vector, not a list,
# you can use functions lapply(), sapply(), apply(), is.numeric(), 
# unlist(), and mean(), and an anonymous function.

### write your code here




##################################
# summary: loading and scrubbing a matrix containing bad data,
# 2. (5pts) 
# download the file "matrix_bad.csv" from NYU Classes),
# the file contains a numeric matrix with row and column names, 
# one column contains a bad data element that isn't numeric,
# read the file into a variable called "mat_rix" using read.csv(),
# make sure to read strings as strings, not as  factors,
# read in properly the row names of "mat_rix",
# you can either use the first column of data for row names, 
# or use function read.csv() with arguments 
# "row.names=1" and "stringsAsFactors=FALSE",

### write your code here



# 3. (15pts) determine the class of "mat_rix", and 
# the class of each column of "mat_rix",
# vector of column classes

### write your code here


# calculate the index of the column that is of class "character", 
# you can use which(),

### write your code here


# extract (copy) the column that is of class "character" 
# to a variable named "col_bad",

### write your code here


# coerce "col_bad" to a numeric vector,
# you can use as.numeric(),

### write your code here


# calculate the index of the element of "col_bad" that is NA,
# replace the NA element with zero,
# you can use which() and is.na(),

### write your code here



# 4. (15pts) summary: perform an apply() loop over the columns 
# of "mat_rix", to coerce "mat_rix" to a numeric matrix,
# 
# copy the row names of "mat_rix" to a vector called "row_names",
# you can use row.names(),

### write your code here


# perform an apply() loop over the columns of "mat_rix", 
# and coerce the columns to numeric vectors,
# copy the result back on to "mat_rix", 
# you can use as.numeric(),

### write your code here


# restore the row names of "mat_rix" using "row_names",
# you can use row.names(),

### write your code here


# replace the NA element of "mat_rix" with zero,
# you can use is.na(),

### write your code here


# coerce "mat_rix" to a matrix, 
# you can use as.matrix(),

### write your code here




##################################
# error handling within an sapply loop,
# 
# download the file "matrix_bad.csv" from NYU Classes),
# the file contains a numeric matrix with row and column names, 
# one column contains a bad data element that isn't numeric,
# read the file into a variable called "mat_rix" using read.csv(),
# make sure to read strings as strings, not as  factors,
# read in properly the row names of "mat_rix",
# you can either use the first column of data for row names, 
# or use function read.csv() with arguments 
# "row.names=1" and "stringsAsFactors=FALSE",

### write your code here


# 5. (15pts) calculate the sums of the columns of "mat_rix",
# by performing an sapply loop over the columns of "mat_rix" 
# you can use sapply(), sum() with argument "na.rm",
# and an anonymous function,
# the anonymous function should coerce each column to numeric, 
# and then calculate its sum,

### write your code here



# set warning option to "2",
# perform the above sapply() loop again,
# it now produces an error, and doesn't return anything,
# you can use options() with argument "warn",

### write your code here



# 6. (25pts) rewrite the above sapply loop and wrap the body 
# of the anonymous function in tryCatch(),
# create another anonymous function to use as an error handler 
# in tryCatch(), 
# the error handler should write the text of the error condition 
# to the console, 
# you can omit the "finally" argument,

### write your code here


# the apply loop returns a list,
# flatten the list into a vector using do.call() and cbind(),

### write your code here



