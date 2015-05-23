#################################
### FRE6871 HW #5 due May 25, 2015
#################################
# Max score 40pts

# Please write in this file the R code needed to perform the tasks below, 
# rename it to your_name_hw5.R
# and send this file to Harjinder Singh (harjinder.singh@nyu.edu)


##################################
# 1. (40pts) Create a function for reading numeric matrices 
# that contain bad data, and call it "read_matrix()",
# The function read_matrix() should have two arguments:
#   "file" - for a string containing the input file name, 
#   "na_replace" - for a number to replace bad data elements,
# 
# The function read_matrix() should perform the following steps:
#   - read a comma-delimited CSV file - use read.csv() 
#       with "stringsAsFactors=FALSE",
#   - assign rownames from first column of data, and then remove it,
#   - verify that every column of input data is numeric, 
#   - if necessary coerce column data to numeric,
#   - and replace NA elements with "na_replace", 
#     - use sapply(), as.numeric(), is.na(), 
# 
# The input file contains a matrix with row and column names, 
# The function read_matrix() should return a numeric matrix with 
# proper dimensions and names,

read_matrix <- function (file, na_replace=0) {

  ### write your code here

}  # end read_matrix


# download the file "badmatrix.csv" from NYU Classes,
# and run this code, to verify that it works properly,

read_matrix(file="badmatrix.csv", na_replace=1000)

