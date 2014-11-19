#################################
### Test #3 Solutions 10/27/14
#################################

# The below solutions are an example,
# Slightly different solutions are also possible.
# You must use the requested functions.


# 1. Calculate the column means of the "iris" data frame from lecture #5.
#    Calculate the means of only those columns that are numeric.
#    You must use the functions lapply (or sapply), is.numeric, and an anonymous function.


# 2. Calculate the column means of the "setosa" species in the "iris" data.
#    You can find all the iris species using the unique function.
unique(iris$Species)
#    hint: first create a data frame of the "setosa" species, which is a subset of the "iris" data frame.
#    hint: Then apply the expression from point #1 on the data frame of the "setosa" species.


# 3. Create a matrix of 100 random normal elements, with 4 columns.


# 4. Calculate column means using the function apply, and omit the NA values.
#    hint: pass the parameter "na.rm=TRUE" to function "mean".


# 5. Calculate percentage returns of EuStockMarkets.
rets_series <- 100*diff(log(EuStockMarkets))

# Load package "moments".
library("moments")

# 6. Calculate the second moment of all the columns of rets_series, using the functions sapply and moment.
#    Pass in the "order" (=2) and "na.rm" (=TRUE) parameters to function "moment".

