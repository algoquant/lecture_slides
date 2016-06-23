#################################
### FRE6871 Test #4 May 16, 2016
#################################
# Max score 100pts

# Please write in this file the R code needed to perform the tasks below, 
# rename the file to your_name_test4.R
# and upload it to NYU Classes,

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

### write your code here

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
# 8.357312e-26  1.087473e-02 1.109319e+02


# 2. (20pts) Create a named vector of noise levels 
# called noise_levels, 
# from 0.0 to 0.03, with the element names equal 
# to their values:

### write your code here

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

### write your code here

# You should get Warning messages:
#   1: In FUN(X[[i]], ...) :
#   regression p-value=0.0612	greater than significance level
# 2: In FUN(X[[i]], ...) :
#   regression p-value=0.09092	greater than significance level
# 3: In FUN(X[[i]], ...) :
#   regression p-value=0.1213	greater than significance level
# 4: In FUN(X[[i]], ...) :
#   regression p-value=0.1509	greater than significance level

# You should get the following output:
# > regression_stats
#                     10           20           30           40           50           60           70
# pval         8.357312e-26 3.964922e-08 1.414587e-04 0.0030737513 0.0141830367 0.0345162903 0.0611982791
# adj_rsquared 1.087473e-02 2.913141e-03 1.347700e-03 0.0007762192 0.0005015313 0.0003469311 0.0002505022
# fstat.value  1.109319e+02 3.021360e+01 1.449384e+01 8.7674454188 6.0173275776 4.4701678740 3.5053989289
#                   80           90          100
# pval         0.090916621 0.1212818427 0.1508679431
# adj_rsquared 0.000185846 0.0001401041 0.0001063763
# fstat.value  2.858619788 2.4010970793 2.0637693369



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

### write your code here


# 2. (10pts) Calculate the sums of the columns of mat_rix, 
# by performing an sapply loop over the columns of mat_rix, 
# and call the vector col_sums. 
# You can use functions sapply(), as.numeric(), sum() with 
# argument "na.rm=TRUE", and an anonymous function. 
# The anonymous function should coerce each column to 
# numeric, and then calculate its sum. 

### write your code here

# Set the warning option "warn" equal to 2, using the 
# function options(). 

### write your code here

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

### write your code here

# The apply loop returns a list instead of a vector. 
# Flatten the list into a vector using do.call() and cbind(). 

### write your code here
