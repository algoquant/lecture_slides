#################################
### Test #3 12/08/14 solutions
#################################
# Max score 60pts

# The below solutions are an example,
# Slightly different solutions are also possible.
# You must use the requested functions.


# 1. (5pts) download the file attached to the email called "matrix_bad.csv",
#    the file contains a matrix with one bad data element,
#    read the file into a variable called "mat_var" using "read.csv",
#    assign the first column of "mat_var" to its rownames, and remove that column,
#    coerce "mat_var" to a matrix,
mat_var <- read.csv(file='matrix_bad.csv')
rownames(mat_var) <- mat_var[, 1]
mat_var <- mat_var[, -1]
mat_var <- as.matrix(mat_var)



# 2. (10pts) set warning option to 2 using "options":
options(warn=2)

#    write an apply loop over the matrix columns using an anonymous function,
#    the anonymous function should coerce each column to numeric, 
#    and then calculate its sum,
#    assign the object returned by apply to a variable called "col_sums", 
col_sums <- apply(mat_var, 2, function(col) {
  col <- as.numeric(col)
  sum(col)
}  # end anon function
)  # end apply


# 3. (45pts) the apply loop produces an error, and doesn't return anything,
#    modify the apply loop, and wrap the body of the anonymous function in "tryCatch",
#    create an error handler in "tryCatch", 
#    the error handler should write the text of the error condition to the console, 
#    the error handler should also write the column data to a file called "error.txt",
col_sums <- apply(mat_var, 2, 
      function(col) tryCatch({
  col <- as.numeric(col)
  sum(col)
      },  # end anon function
        error=function(error_cond) {
          cat(paste("error:", error_cond))
          cat(col, file="error.txt", sep="\n", append=TRUE)
        },
        finally=cat("done!\n")
        )  # end tryCatch
)  # end apply

# the apply loop returns a list,
# flatten the list into a vector using do.call and cbind,
do.call(cbind, col_sums)
