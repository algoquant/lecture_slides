#################################
### FRE6871 Test #1 Solutions April 25, 2016
#################################
# Max score 80pts

# The below solutions are examples,
# Slightly different solutions are also possible.

############## Part I
# Summary: Use function which() to extract elements of 
# data frames. 
# Perform an sapply() loop over unique elements of 
# data frames, and extract its elements. 

# 1. (10pts) Select a subset of the mtcars data frame, 
# that contains only cars with 6 cylinders, and call 
# it mtcars_6cyl. 

mtcars_6cyl <- mtcars[mtcars$cyl==6, ]


# 2. (10pts) Select the row of mtcars_6cyl that 
# contains the car with the highest horsepower, 
# and call it best_car.
# hint: you can use function which.max(). 

best_car <- mtcars_6cyl[which.max(mtcars_6cyl$hp), ]

# Print the name of the car with the highest horsepower: 

rownames(best_car)

# You should get the following result:
# [1] "Ferrari Dino"

# Print the horsepower and the weight of that car:

best_car$hp
best_car$wt


# 3. (20pts) Calculate a named vector of names of 
# cars with the highest horsepower in each cylinder 
# category. 
# You can use functions unique, which.max(), 
# structure(), sapply(), and an anonymous function. 

sapply(unique(mtcars$cyl), function(cyl) {
  mtcars_cyl <- mtcars[mtcars$cyl==cyl, ]
  structure(rownames(mtcars_cyl[which.max(mtcars_cyl$hp), ]), 
            names=paste(cyl, "cylinders"))
})  # end sapply

# You should get the following result:
#   6 cylinders     4 cylinders     8 cylinders 
# "Ferrari Dino"  "Lotus Europa" "Maserati Bora" 



############## Part II
# Summary: Define an aggregation function which accepts 
# a dots "..." argument. 
# Perform an apply() loop over the columns of a matrix, 
# and pass additional arguments through the dots "..." 
# argument to the aggregation function. 

# Create a matrix that contains NA values as follows:

set.seed(1121)
mat_rix <- matrix(sample(c(1:96, rep(NA, 4))), ncol=4)


# 1. (10pts) Assign the following column names to mat_rix: 
# "col1", "col2", etc. 
# You can use functions colnames(), ncol(), and paste0(). 
# You cannot use function c(). 

colnames(mat_rix) <- paste0("col", 1:ncol(mat_rix))


# 2. (10pts) The function agg_regate() calculates the 
# maximum and minimum values over a column of data: 

agg_regate <- function(col_umn) {
  c(max=max(col_umn), min=min(col_umn))
}  # end agg_regate

# Perform an apply() loop over the columns of mat_rix, 
# and apply agg_regate() to each column.

apply(mat_rix, 2, agg_regate)

# You should get the following result:
#     col1 col2 col3 col4
# max   96   NA   NA   NA
# min   19   NA   NA   NA


# 3. (20pts) Modify agg_regate() by adding a dots "..." 
# argument to it, and pass the dots to the functions 
# max() and min(). 

agg_regate <- function(col_umn, ...) {
  c(max=max(col_umn, ...), min=min(col_umn, ...))
}  # end agg_regate

# Perform an apply() loop over the columns of mat_rix, 
# and apply agg_regate() to each column.
# This time pass the argument "na.rm=TRUE" into max() 
# and min() through the dots "..." argument of the 
# apply() function. 

apply(mat_rix, 2, agg_regate, na.rm=TRUE)

# You should get the following result:
#     col1 col2 col3 col4
# max   96   91   94   95
# min   19    2    1    5
