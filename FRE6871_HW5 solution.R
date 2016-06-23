#################################
### FRE6871 Homework #5 Solution due May 16, 2016
#################################
# Max score 70pts

# The below solutions are examples,
# Slightly different solutions are also possible.


############## Part I
# 1. (30pts) Create a function called lag_it() that applies 
# a lag to vectors. 
# lag_it() should accept two arguments:
# - vec_tor a vector argument to which a lag should be applied, 
# - "lag" an integer, specifying the number of periods to lag. 
# - "lag" should have a default value of 1. 
# lag_it() should first check if "lag" is numeric, and if not 
# then it should produce a warning message and return NULL. 
# lag_it() should next check if vec_tor is a vector, and if not 
# then it should produce a warning message and return NULL. 
# If both these tests pass, then lag_it() should return a vector 
# of the same length as vec_tor, that is lagged by the number 
# of periods specified by "lag". 
# A positive "lag" should replace the present value with values 
# from the past, and a negative lag should replace with values 
# from the future. 
# lag_it() should add NA values in place of values that are missing. 
# for example, lag_it() should produce the following output:
#  lag_it(c(1:5), lag=2)
#  [1] NA NA  1  2  3
# 
#  lag_it(c(1:5), lag=-2)
#  [1]  3  4  5 NA NA
# 
# you can use functions is.vector(), is.numeric(), 
# length(), c(), rep(), warning(), and return(), 

lag_it <- function(vec_tor, lag=1) {
  if (!is.numeric(lag)) {  # lag is not numeric
    warning(paste("argument", deparse(substitute(lag)), "must be numeric."))
    return(NULL)  # return NULL
  }  # end if
  if (is.vector(vec_tor)) {  # vec_tor is a vector
    if(lag>0) {
      vec_tor <- c(rep(NA, lag), vec_tor)
      vec_tor[-((length(vec_tor)-lag+1):length(vec_tor))]
    } else {
      vec_tor <- c(vec_tor, rep(NA, -lag))
      vec_tor[-(1:(-lag))]
    }
  } else {  # vec_tor is not a vector
    warning(paste0("argument \"", deparse(substitute(vec_tor)), "\" must be a vector."))
    return(NULL)  # return NULL
  }  # end if
}  # end lag_it

# call lag_it() as below, to verify it works correctly,
lag_it(1:9)
lag_it(1:9, lag=2)
lag_it(1:9, lag=-1)
lag_it(1:9, lag=-2)
lag_it(matrix(1:9, ncol=1))
lag_it("a", "b")

# You should get the following results:
# > lag_it(1:9, lag=2)
# [1] NA NA  1  2  3  4  5  6  7
# > lag_it(1:9, lag=-1)
# [1]  2  3  4  5  6  7  8  9 NA



############## Part II
# Summary: Estimate the standard errors of regression 
# coefficients using bootstrap simulations. 

# 1. (10pts) Specify a regression as follows:

set.seed(1121)  # reset random number generator
# define explanatory and response variables
explana_tory <- seq(from=0.1, to=3.0, by=0.1)
res_ponse <- 3 + 2*explana_tory + rnorm(length(explana_tory))
# specify regression formula and perform regression
reg_formula <- res_ponse ~ explana_tory
reg_model <- lm(reg_formula)

# Extract the regression coefficient standard errors 
# from the regression model summary, and call them 
# std_errors.
# You can use the function summary().

reg_model_sum <- summary(reg_model)
std_errors <- reg_model_sum$coefficients[, 2]


# 2. (20pts) Perform an sapply() loop 10000 times. 
# In each loop bootstrap (sample) the variables 
# explana_tory and res_ponse, perform the regression, 
# and return the regression coefficients.  
# Call the matrix output by sapply() boot_strap. 
# hint: download the latest version of the 
# statistics.pdf file from NYU Classes and follow 
# the bootstrap example there. 
# You can use the functions sample.int(), lm(), 
# coef(), and sapply().

boot_strap <- sapply(1:10000, function(x) {
  boot_sample <- sample.int(length(explana_tory), replace=TRUE)
  explana_tory <- explana_tory[boot_sample]
  res_ponse <- res_ponse[boot_sample]
  coef(lm(res_ponse ~ explana_tory))
})  # end sapply

# You should get the following output:
# > boot_strap[, 1:3]
#                 [,1]     [,2]     [,3]
# (Intercept)  3.339112 3.458160 3.108136
# explana_tory 1.809326 1.737513 1.864102

# Calculate the standard errors from bootstrap, 
# and call them boot_errors.
# You can use the functions sd() and apply(). 
# Calculate the differences between boot_errors 
# and std_errors. 

boot_errors <- apply(boot_strap, MARGIN=1, sd)
boot_errors - std_errors

# You should get the following output:
# > boot_errors
# (Intercept) explana_tory 
#   0.2655507    0.1326947


# 3. (10pts) Create a density plot of the bootstrapped 
# coefficients boot_strap["explana_tory", ], 
# with plot title "Bootstrapped regression slopes". 
# and with x-axis label "regression slopes". 
# Add a vertical line in red at the x-axis point: 
#   mean(boot_strap["explana_tory", ]). 
# You can use the functions x11(), plot(), density(), 
# and abline(). 

x11()
plot(density(boot_strap["explana_tory", ]), 
     lwd=2, xlab="regression slopes", 
     main="Bootstrapped regression slopes")
abline(v=mean(boot_strap["explana_tory", ]), lwd=2, col="red")


