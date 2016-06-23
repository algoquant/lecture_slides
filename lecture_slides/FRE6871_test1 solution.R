#################################
### FRE6871 Test #1 Solutions Sep 28, 2015
#################################
# Max score 70pts

# The below solutions are examples,
# Slightly different solutions are also possible.


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

colnames(mat_rix) <- paste0("col", 1:ncol(mat_rix))


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

apply(mat_rix, 2, 
      function(co_lumn, na.rm) 
        c(max=max(co_lumn, na.rm=na.rm), 
          min=min(co_lumn, na.rm=na.rm)), 
      na.rm=TRUE)



##############
# Summary: extract and filter the elements of a matrix. 

# 1. (5pts) create a numeric vector of length 15 containing 
# random normal variates (rnorm), 
# coerce the vector into a matrix of 5 rows and 3 columns,
# call the matrix "mat_rix", 
# you can use functions matrix() and dim(), 

mat_rix <- matrix(rnorm(15), ncol=3)
# or
mat_rix <- rnorm(15)
dim(mat_rix) <- c(5, 3)

# 2. (5pts) extract all the elements of "mat_rix" that are greater than 1.0,

mat_rix[mat_rix>1.0]

# 3. (5pts) calculate the row and column indices of all the elements 
# of "mat_rix" that are greater than 1.0,
# you can use function which() with argument "arr.ind", 

which(mat_rix>1.0, arr.ind=TRUE)

# 4. (5pts) calculate the sums of all the rows and columns, 
# the result should be two vectors, 
# you can use functions apply() and sum(), 

row_sums <- apply(mat_rix, 1, sum)
col_sums <- apply(mat_rix, 2, sum)

# 5. (5pts) bind the vectors of sums to "mat_rix", as extra 
# rows and columns, respectively, 
# you can use functions cbind() and rbind(), 

mat_rix <- cbind(c(sum(row_sums), row_sums), 
                 rbind(col_sums, mat_rix))

# 6. (15pts) assign names to the rows and columns follows: 
# "col_sums", "row1", "row2", etc., 
# and 
# "row_sums", "col1", "col2", etc., 
# you can use functions c(), dimnames(), list(), nrow(), ncol(), 
# and paste0(), 

dimnames(mat_rix) <- list(c("col_sums", paste0("row", 1:(nrow(mat_rix)-1))), 
                          c("row_sums", paste0("col", 1:(ncol(mat_rix)-1))))

