#################################
### HW #1 Solution
#################################

# The below solutions are examples,
# Slightly different solutions are also possible.
# Max score 25pts

### HW #1: Write R code that performs the following:

# 1. create a matrix of 30rows x 10columns with random normal variates, (5pts)
mat_rix <- matrix(rnorm(300), ncol=10)

# 2. calculate the mean, standard deviation, skewness, and kurtosis of: (10pts)
# the 5th row, 
in_data <- mat_rix[5, ]
len_data <- length(in_data)
mean_data <- mean(in_data)
sd_data <- sd(in_data)
skew_data <- len_data*sum(((in_data - mean_data)/sd_data)^3)/((len_data-1)*(len_data-2))
kurtosis_data <- len_data*(len_data+1)*sum(((in_data - mean_data)/sd_data)^4)/((len_data-1)^3)
c(mean_data, sd_data, skew_data, kurtosis_data)

# the 8th column,
in_data <- mat_rix[, 8]
len_data <- length(in_data)
mean_data <- mean(in_data)
sd_data <- sd(in_data)
skew_data <- len_data*sum(((in_data - mean_data)/sd_data)^3)/((len_data-1)*(len_data-2))
kurtosis_data <- len_data*(len_data+1)*sum(((in_data - mean_data)/sd_data)^4)/((len_data-1)^3)
c(mean_data, sd_data, skew_data, kurtosis_data)


# 3. find all the elements of the matrix that are greater than 1.0, (10pts)
mat_rix[mat_rix > 1]


