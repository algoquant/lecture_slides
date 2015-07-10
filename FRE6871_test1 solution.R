#################################
### FRE6871 Test #1 Solutions 06/15/15
#################################
# Max score 45pts

# The below solutions are examples,
# Slightly different solutions are also possible.


##################################
# 1. (15pts) create a function called get_index(), that is equivalent 
# to function which(), when applied to vectors,
# hint: you can use function length() and then apply vector subsetting,
get_index <- function(vec_tor) (1:length(vec_tor))[vec_tor]


# apply the function get_index() to a boolean vector, and 
# compare the result with using function which(),
vec_tor <- sample(1:9)
get_index(vec_tor==5)
which(vec_tor == 5)



##################################
# 2. (15pts) Create a vector of 90 integers, from 1 to 90, and call it "vec_tor", 
# extract every third element of "vec_tor", starting with the first one, 
# into a single column matrix called "mat_rix",
# hint: you can use function matrix(), with the proper "byrow" argument,
vec_tor <- 1:90
mat_rix <- matrix(vec_tor[seq(1, length(vec_tor), by=3)], ncol=1)
# or:
mat_rix <- matrix(vec_tor, ncol=3, byrow=TRUE)
mat_rix <- mat_rix[, 1, drop=FALSE]

# extract a vector of odd elements of "vec_tor", and a vector of even elements,
# calculate the scalar ("inner") product of the two vectors,
# the value should be a vector with a single element, not a matrix,
# hint: you can use function matrix(), with the proper "byrow" argument,
# use function drop(),
odd_elements <- vec_tor[seq(1, length(vec_tor), by=2)]
even_elements <- vec_tor[seq(2, length(vec_tor), by=2)]
drop(odd_elements %*% even_elements)
# or:
mat_rix <- matrix(vec_tor, ncol=2, byrow=TRUE)
drop(mat_rix[, 1] %*% mat_rix[, 2])



##################################
# 3. (15pts) Create a matrix called "mat_rix", as follows:
mat_rix <- matrix(1:6, ncol=3)

# assign to "mat_rix" row names "row1", "row2", and 
# column names "col1", "col2", "col3",
# use the functions rownames(), colnames(), and/or dimnames(), 
dimnames(mat_rix) <- list(rows=c("row1", "row2"),
                          columns=c("col1", "col2", "col3"))

# change the names of the rows to "first_row" and "second_row",
# use the function dimnames(), but not the function rownames(),
dimnames(mat_rix)[[1]] <- c("first_row", "second_row")
# or:
dimnames(mat_rix)$rows <- c("first_row", "second_row")


