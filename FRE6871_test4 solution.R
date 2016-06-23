#################################
### FRE6871 Test #4 Solutions May 16, 2016
#################################
# Max score 100pts

# The below solutions are examples,
# Slightly different solutions are also possible.

############## Part I
# Summary: Perform regressions in an sapply loop, 
# and produce a warning when the p-value is greater 
# than the significance level. 

# 1. (20pts) Create a function called reg_stats() which 
# performs a regression and returns a vector of regression 
# statistics. 
# reg_stats() should accept a single argument called da_ta, 
# which is a data frame containing the design matrix. 
# The first column of da_ta is the response, and the 
# remaining columns are the explanatory variables. 
# reg_stats() should create a formula from the columns of 
# da_ta, then perform a regression, and finally should 
# return a named vector of the regression statistics: 
# p-value, adj.r.squared, fstatistic. 
# hint: you can adapt the code from the slide titled 
# "Influence of Noise on Regression Another Method". 

reg_stats <- function(da_ta) {
  # perform regression and get summary
  col_names <- colnames(da_ta)
  reg_formula <- as.formula(paste(col_names[1], 
            paste(col_names[-1], collapse="+"), sep="~"))
  reg_model_sum <- summary(lm(reg_formula, data=da_ta))
  # extract regression statistics
  with(reg_model_sum, c(pval=coefficients[2, 4],
                        adj_rsquared=adj.r.squared,
                        fstat=fstatistic[1]))
}  # end reg_stats

# Create a design matrix data frame of response and 
# two explanatory variables as follows:

len_gth <- 10000
set.seed(1121)
noise <- rnorm(len_gth)
explana_tory1 <- rnorm(len_gth)
explana_tory2 <- rnorm(len_gth)
noise_level <- 10
res_ponse <- explana_tory1 + explana_tory2 + noise_level*noise
da_ta <- data.frame(res_ponse, explana_tory1, explana_tory2)

# Apply reg_stats() to da_ta. 
# You should get the following output:
# > reg_stats(da_ta)
#      pval     adj_rsquared  fstat.value
# 5.413256e-26 2.146549e-02 1.106709e+02


# 2. (20pts) Create a named vector of noise levels 
# called noise_levels, 
# from 0.0 to 0.03, with the element names equal 
# to their values:

noise_levels <- seq(from=10, to=100, by=10)
names(noise_levels) <- noise_levels

# You should get the following output:
# > noise_levels
# 10  20  30  40  50  60  70  80  90 100
# 10  20  30  40  50  60  70  80  90 100

# Perform an sapply() loop over noise_levels. 
# In each step create a design matrix data frame 
# with a response and two explanatory variables, 
# and call reg_stats() to perform a regression. 
# If the p-value is greater than the significance 
# level, then produce a warning with text that 
# contains the p-value in it. 
# Finally at the end return the reg_stats() values. 
# You can use the functions sapply(), data.frame(), 
# reg_stats(), paste0(), warning(), and an 
# anonymous function. 

# specify significance level:

sign_level <- 2*pnorm(-2)

regression_stats <- sapply(noise_levels, function(noise_level) {
  # create design matrix data frame end perform regression
  res_ponse <- explana_tory1 + explana_tory2 + noise_level*noise
  da_ta <- data.frame(res_ponse, explana_tory1, explana_tory2)
  regstats <- reg_stats(da_ta)
  if(regstats["pval"] > sign_level)
    warning(paste0("regression p-value=", format(regstats["pval"], digits=4), "\tgreater than significance level"))
  regstats
})  # end sapply

# You should get:
# Warning messages:
#   1: In FUN(X[[i]], ...) :
#   regression p-value=0.06155	greater than significance level
# 2: In FUN(X[[i]], ...) :
#   regression p-value=0.09136	greater than significance level
# 3: In FUN(X[[i]], ...) :
#   regression p-value=0.1218	greater than significance level
# 4: In FUN(X[[i]], ...) :
#   regression p-value=0.1514	greater than significance level

# You should get the following output:
# > regression_stats
#                     10           20           30           40           50           60           70
# pval         5.413256e-26 3.971562e-08 1.432442e-04 0.003106125 0.0143015190 0.0347503907 0.0615452232
# adj_rsquared 2.146549e-02 5.732749e-03 2.643543e-03 0.001517169 0.0009763702 0.0006723125 0.0004828453
# fstat.value  1.106709e+02 2.982613e+01 1.425142e+01 8.596609788 5.8861334606 4.3634877194 3.4151514443
#                    80           90          100
# pval         0.0913585883 0.1217981403 0.1514400901
# adj_rsquared 0.0003559211 0.0002662028 0.0002001017
# fstat.value  2.7800613301 2.3312354440 2.0006085829



############## Part II
# Summary: Perform error handling within an sapply loop. 

# 1. (10pts) Download the file "matrix_bad.csv" from NYU 
# Classes. 
# The file contains a numeric matrix with row and column 
# names.  One column contains a bad data element that 
# isn't numeric. 
# Read the file into a variable called mat_rix using 
# read.csv(). Make sure to read strings as strings, not 
# as factors. Read in properly the row names of mat_rix. 
# You can either use the first column of data for row 
# names, or use function read.csv() with arguments 
# "row.names=1" and "stringsAsFactors=FALSE",

mat_rix <- read.csv(file="matrix_bad.csv", row.names=1,
                    stringsAsFactors=FALSE)
# or
mat_rix <- read.csv(file="matrix_bad.csv", stringsAsFactors=FALSE)
rownames(mat_rix) <- mat_rix[, 1]
mat_rix <- mat_rix[, -1]


# 2. (10pts) Calculate the sums of the columns of mat_rix, 
# by performing an sapply loop over the columns of mat_rix, 
# and call the vector col_sums. 
# You can use functions sapply(), as.numeric(), sum() with 
# argument "na.rm=TRUE", and an anonymous function. 
# The anonymous function should coerce each column to 
# numeric, and then calculate its sum. 

col_sums <- sapply(mat_rix, function(col_umn) {
  sum(as.numeric(col_umn), na.rm=TRUE)
}  # end anon function
)  # end sapply

# Set the warning option "warn" equal to 2, using the 
# function options(). 

options(warn=2)

# Perform the above sapply() loop again. 
# It now produces an error, and doesn't return anything,


# 3. (40pts) Rewrite the above sapply loop and wrap the 
# body of the anonymous function in tryCatch(). 
# Create another anonymous function to use as an error 
# handler in tryCatch(). 
# The error handler should write the text of the error 
# condition to the console using cat(). 
# The error handler should also write the column data 
# that caused the error to a text file called "error.txt", 
# using cat(). 
# You can omit the "finally" argument. 
# You can use functions sapply(), as.numeric(), sum() with 
# argument "na.rm=TRUE", tryCatch(), cat(), and an 
# anonymous function. 

col_sums <- sapply(mat_rix, 
                   function(col_umn)
                     tryCatch( {  # body
                       sum(as.numeric(col_umn), na.rm=TRUE)
                     },
                     error=function(error_cond) {
                       cat(paste("error:", error_cond))
                       cat(col_umn, file="error.txt")
                     }  # end error handler
                     )  # end tryCatch
)  # end sapply

# The apply loop returns a list instead of a vector. 
# Flatten the list into a vector using do.call() and cbind(). 

do.call(cbind, col_sums)


