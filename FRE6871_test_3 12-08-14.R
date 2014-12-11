#################################
### Test #3 12/08/14
#################################
# Max score 60pts

# Please write in this file the R code needed to perform the tasks below, 
# rename the file to your_name_test3.R
# and send the file to Xirui He (xh521@nyu.edu)


# 1. (5pts) download the file attached to the email called "matrix_bad.csv",
#    the file contains a matrix with one bad data element,
#    read the file into a variable called "mat_var" using "read.csv",
#    assign the first column of "mat_var" to its rownames, and remove that column,
#    coerce "mat_var" to a matrix,



# 2. (10pts) set warning option to 2 using "options":
#    write an apply loop over the matrix columns using an anonymous function,
#    the anonymous function should coerce each column to numeric, 
#    and then calculate its sum,
#    assign the object returned by apply to a variable called "col_sums", 



# 3. (45pts) the apply loop produces an error, and doesn't return anything,
#    modify the apply loop, and wrap the body of the anonymous function in "tryCatch",
#    create an error handler in "tryCatch", 
#    the error handler should write the text of the error condition to the console, 
#    the error handler should also write the column data to a file called "error.txt",
#    the apply loop returns a list,
#    flatten the list into a vector using do.call and cbind,

