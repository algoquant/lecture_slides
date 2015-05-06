#################################
### FRE6871 Test #1 Solutions 04/27/15
#################################
# Max score 50pts

# The below solutions are examples,
# Slightly different solutions are also possible.


# 1. (5pts) download the file attached to the email called "matrix_bad.csv",
# the file contains a numeric matrix with one bad data element that isn't numeric,
# read the file into a variable called "mat_var" using "read.csv",
# restore the column and row names,
# remove the first column containing the row names,
# use read.csv(), class(), and rownames(),

mat_var <- read.csv(file='matrix_bad.csv')
rownames(mat_var) <- mat_var[, 1]
mat_var <- mat_var[, -1]

# determine the class of mat_var

class(mat_var)



# 2. (20pts) determine the class of each column of mat_var,
# find which column is of class "factor", and extract it to a variable named "col_bad",
# coerce "col_bad" to a numeric vector,
# find the row and column numbers of the element of "mat_var" that isn't numeric,
# use sapply(), which(), as.vector(), as.numeric() and is.na(),

# vector of column classes
col_class <- sapply(mat_var, class)
col_class

# index of column of class "factor"
col_index <- which(col_class=="factor")

# extract the column of class "factor"
col_bad <- mat_var[, col_index]

# coerce "col_bad" to a numeric vector,
col_bad <- as.numeric(as.vector(col_bad))

# find the row and column numbers of the element that isn't numeric,
row_index <- which(is.na(col_bad))
c(row_index, col_index)



# 3. (25pts) coerce "mat_var" to a numeric matrix,
# perform an sapply() loop over the columns of "mat_var", and coerce them to numeric vectors,
# call the result "num_mat",
# "num_mat" is a numeric matrix obtained from coercing the columns of "mat_var" to numeric,
# restore the row names of "num_mat",
# calculate the row means of "num_mat" using the function apply(), and omit the NA values,
# hint: pass the parameter "na.rm=TRUE" to function mean(),
# use the row and column numbers of the element of "num_mat" that isn't numeric, and replace it with zero,
# use apply(), sapply(), as.vector(), as.numeric(), mean(), and an anonymous function,

num_mat <- sapply(mat_var, function(col_umn) {
  as.numeric(as.vector(col_umn))
}  # end anon function
)  # end sapply

rownames(num_mat) <- rownames(mat_var)

apply(num_mat, 1, mean, na.rm=TRUE)

num_mat[row_index, col_index] <- 0
# even simpler way:
num_mat[is.na(num_mat)] <- 0


