#################################
### FRE6871 Test #4 Solutions Oct 19, 2015
#################################
# Max score 65pts

# The below solutions are examples,
# Slightly different solutions are also possible.

##############
# Summary: load and scrub a matrix containing bad data. 

# 1. (5pts) 
# Download the file "matrix_bad.csv" from NYU Classes),
# the file contains a numeric matrix with row and column names, 
# with some columns containing bad data elements that aren't numeric.
# Read the file into a variable called "mat_rix" using read.csv(),
# make sure to read strings as strings, not as factors,
# and read in properly the row names of "mat_rix". 
# You can either use the first column of data for row names, 
# or use function read.csv() with arguments 
# "row.names=1" and "stringsAsFactors=FALSE", 

mat_rix <- read.csv(file="badmatrix.csv", stringsAsFactors=FALSE)
rownames(mat_rix) <- mat_rix[, 1]
mat_rix <- mat_rix[, -1]
# or
mat_rix <- read.csv(file="badmatrix.csv", row.names=1,
                    stringsAsFactors=FALSE)

# 2. (15pts) determine the class of "mat_rix", and 
# calculate a vector of the classes of the columns 
# of "mat_rix". 
# You can use the functions sapply() and class(), 

class(mat_rix)
col_class <- sapply(mat_rix, class)

# calculate the vector of indices of the columns that are 
# of class "character", and call it "col_index", 
# you can use function which(),

col_index <- which(col_class=="character")

# 3. (15pts) perform an sapply() loop over the "character" 
# columns of "mat_rix", coerce them to "numeric" vectors, 
# and call the result "col_fixed", 
# you can use functions sapply() and as.numeric(), 

col_fixed <- sapply(mat_rix[, col_index], as.numeric)

# replace the "character" columns of "mat_rix" with 
# "col_fixed", using the vector "col_index", 

mat_rix[, col_index] <- col_fixed

# 4. (10pts) Perform an apply() loop over the rows of 
# "mat_rix", calculate the row means, and call the result 
# "row_means", 
# You can use functions apply() and mean(), 
# ignore NA values using the argument "na.rm=TRUE". 
# You cannot use an anonymous function. 

row_means <- apply(mat_rix, 1, mean, na.rm=TRUE)

# 5. (20pts) Replace NA values in "mat_rix" with the 
# corresponding row means. 
# You can use function is.na(), and function which() with 
# the argument "arr.ind=TRUE". 
# You cannot perform any loops, only subsetting of matrices. 

is_na <- which(is.na(mat_rix), arr.ind=TRUE)
mat_rix[is_na] <- row_means[is_na[, 1]]

# coerce "mat_rix" to a matrix, 
# you can use as.matrix(),

mat_rix <- as.matrix(mat_rix)



