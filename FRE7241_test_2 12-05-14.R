#################################
### Test #2 12/05/14
#################################
# Max score 45pts

# Please write in this file the R code needed to perform the tasks below, 
# rename the file to your_name_test2.R
# and send the file to the TA Dong Huang (dh1716@nyu.edu)



# 1. (10pts) Create a log-likelihood objective function for a mixture of two normal distributions,
#    assume that the two distributions both have sd=1.0, but different means,
#    the target vector is a sample from the mixture of two normal distributions,
target_vector <- c(rnorm(300, mean=0.0, sd=1.0), 
                   rnorm(100, mean=4.0, sd=1.0))
#    the objective function should be a function of a vector of three parameters: 
#    weight, mean1, and mean2,
#    and also a function of the sample "target_vector", 


# 2. (10pts) create a vectorized objective function, by vectorizing the mean1 and mean2 parameters, 



# 3. (10pts) create vectors of mean1 and mean2 parameters, with 50 values from -10, to 10,

#    calculate the objective function on a parameter grid made from the two vectors, 
#    set the weight=0.3,

#    create a perspective plot of the objective function,



# 4. (10pts) perform optimization using the function "optim", 
#    to find the optimal parameters: weight, mean1, and mean2,
#    set the "upper" and "lower" parameter limits to c(1,10,10) and c(0,-10,-10),
#    set the initial parameters to:
par_init <- c(weight=0.99, mean1=0, mean2=0)
# perform optimization



# 5. (5pts) plot the histogram of "target_vector",

#    plot the mixture distribution with the fitted parameters,


