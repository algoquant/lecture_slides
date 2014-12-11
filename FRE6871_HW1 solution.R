#################################
### HW #1 Solution
#################################
# Max score 35pts

# The below solutions are examples,
# Slightly different solutions are also possible.


# 1. (15pts) using the function paste() with a collapse string, 
#     create the character string "y = x1 + x2 + x3" from 
#     characters "x", "y", "=", "+", and the vector 1:3,
lin_formula <- paste("y =", 
        paste(paste0("x", 1:3), collapse=" + ")
  )  # end paste


# 2. (5pts) create a numeric vector of length 15 containing 
#     random normal variates (rnorm),
#     coerce the vector into a matrix of 5 rows and 3 columns,
mat_var <- matrix(rnorm(15), ncol=3)


# 3. (10pts) get the values of all the elements that are greater than 1.0,
mat_var[mat_var>1.0]

#     get the indices of all the elements that are greater than 1.0,
which(mat_var>1.0)


# 3. (5pts) calculate the mean and standard deviation of the 3rd row, 
#     and the 2nd column,
mean(mat_var[3, ])
sd(mat_var[3, ])
mean(mat_var[, 2])
sd(mat_var[, 2])

