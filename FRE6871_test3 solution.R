#################################
### FRE6871 Test #3 Solutions July 6, 2015
#################################
# Max score 90pts

# The below solutions are examples,
# Slightly different solutions are also possible.


##################################
# 1. (15pts) Calculate a vector of means of the numeric columns 
# of the "iris" data frame,
# calculate the means of only those columns that are numeric,
# the output must be a vector, not a list,
# you can use functions lapply(), sapply(), apply(), is.numeric(), 
# unlist(), and mean(), and an anonymous function.
unlist(sapply(iris, function(co_lumn) {if (is.numeric(co_lumn)) mean(co_lumn)}))
# or
sapply(iris[, sapply(iris, is.numeric)], mean)



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
mat_rix <- read.csv(file='matrix_bad.csv', stringsAsFactors=FALSE)
rownames(mat_rix) <- mat_rix[, 1]
mat_rix <- mat_rix[, -1]
# or
mat_rix <- read.csv(file="matrix_bad.csv", row.names=1,
                    stringsAsFactors=FALSE)


# 3. (15pts) determine the class of "mat_rix", and 
# the class of each column of "mat_rix",
# vector of column classes
class(mat_rix)
col_class <- sapply(mat_rix, class)

# calculate the index of the column that is of class "character", 
# you can use which(),
col_index <- which(col_class=="character")

# extract (copy) the column that is of class "character" 
# to a variable named "col_bad",
col_bad <- mat_rix[, col_index]

# coerce "col_bad" to a numeric vector,
# you can use as.numeric(),
col_bad <- as.numeric(col_bad)

# calculate the index of the element of "col_bad" that is NA,
# replace the NA element with zero,
# you can use which() and is.na(),
which(is.na(col_bad))
col_bad[is.na(col_bad)] <- 0


# 4. (15pts) summary: perform an apply() loop over the columns 
# of "mat_rix", to coerce "mat_rix" to a numeric matrix,
# 
# copy the row names of "mat_rix" to a vector called "row_names",
# you can use row.names(),
row_names <- row.names(mat_rix)

# perform an apply() loop over the columns of "mat_rix", 
# and coerce the columns to numeric vectors,
# copy the result back on to "mat_rix", 
# you can use as.numeric(),
mat_rix <- apply(mat_rix, 2, as.numeric)

# restore the row names of "mat_rix" using "row_names",
# you can use row.names(),
row.names(mat_rix) <- row_names

# replace the NA element of "mat_rix" with zero,
# you can use is.na(),
mat_rix[is.na(mat_rix)] <- 0

# coerce "mat_rix" to a matrix, 
# you can use as.matrix(),
mat_rix <- as.matrix(mat_rix)



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
mat_rix <- read.csv(file='matrix_bad.csv', stringsAsFactors=FALSE)
rownames(mat_rix) <- mat_rix[, 1]
mat_rix <- mat_rix[, -1]
# or
mat_rix <- read.csv(file="matrix_bad.csv", row.names=1,
                    stringsAsFactors=FALSE)

# 5. (15pts) calculate the sums of the columns of "mat_rix",
# by performing an sapply loop over the columns of "mat_rix" 
# you can use sapply(), sum() with argument "na.rm",
# and an anonymous function,
# the anonymous function should coerce each column to numeric, 
# and then calculate its sum,
col_sums <- sapply(mat_rix, function(co_lumn) {
  sum(as.numeric(co_lumn), na.rm=TRUE)
}  # end anon function
)  # end apply


# set warning option to "2",
options(warn=2)

# perform the above sapply() loop again,
# it now produces an error, and doesn't return anything,
# you can use options() with argument "warn",


# 6. (25pts) rewrite the above sapply loop and wrap the body 
# of the anonymous function in tryCatch(),
# create another anonymous function to use as an error handler 
# in tryCatch(), 
# the error handler should write the text of the error condition 
# to the console, 
# you can omit the "finally" argument,
col_sums <- sapply(mat_rix, 
                   function(co_lumn)
                     tryCatch( {  # body
                       sum(as.numeric(co_lumn), na.rm=TRUE)
                     },
                     error=function(error_cond) {
                       cat(paste("error:", error_cond))
                     }  # end error handler
                     )  # end tryCatch
)  # end sapply

# the apply loop returns a list,
# flatten the list into a vector using do.call() and cbind(),
do.call(cbind, col_sums)


