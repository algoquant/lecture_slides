#################################
### FRE6871 Test #1 04/27/15
#################################
# Max score 50pts

# Please write in this file the R code needed to perform the tasks below, 
# rename the file to your_name_test1.R
# and send the file to Harjinder Singh (harjinder.singh@nyu.edu)


# 1. (5pts) download the file attached to the email called "matrix_bad.csv",
# the file contains a numeric matrix with one bad data element that isn't numeric,
# read the file into a variable called "mat_var" using "read.csv",
# restore the column and row names,
# remove the first column containing the row names,
# use read.csv(), class(), and rownames(),

### write your code here

# determine the class of mat_var

### write your code here



# 2. (20pts) determine the class of each column of mat_var,
# find which column is of class "factor", and extract it to a variable named "col_bad",
# coerce "col_bad" to a numeric vector,
# find the row and column numbers of the element of "mat_var" that isn't numeric,
# use sapply(), which(), as.vector(), as.numeric() and is.na(),

### write your code here



# 3. (25pts) coerce "mat_var" to a numeric matrix,
# perform an sapply() loop over the columns of "mat_var", and coerce them to numeric vectors,
# call the result "num_mat",
# "num_mat" is a numeric matrix obtained from coercing the columns of "mat_var" to numeric,
# restore the row names of "num_mat",
# calculate the row means of "num_mat" using the function apply(), and omit the NA values,
# hint: pass the parameter "na.rm=TRUE" to function mean(),
# use the row and column numbers of the element of "num_mat" that isn't numeric, and replace it with zero,
# use apply(), sapply(), as.vector(), as.numeric(), mean(), and an anonymous function,

### write your code here
