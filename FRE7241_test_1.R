#################################
### FRE7241 Test #1 04/28/15
#################################
# Max score 50pts

# Please write in this file the R code needed to perform the tasks below, 
# rename the file to your_name_test1.R
# and send the file to Luping Liu (ll2525@nyu.edu)


# 1. (20pts) Create a function returning regression stats,
# Create a function called "reg_sd()" which accepts a single argument "sd", 
# The function reg_sd() should perform the following steps:
#    initialize random number generator by calling set.seed(1121),
#    create response and explanatory variables of length 30,
#    calculate the response variable using the formula from the slide: 
#    "Weak Regression" in lecture #3, 
#    pass the argument "sd" into rnorm(),
#    perform a regression,
#    extract from summary() the regression statistics: p-value, adj.r.squared, fstatistic,
#    create a named vector of the regression statistics, and return it,
# hint: use the code from the slide "Weak Regression" in lecture #3,

### write your code here


# apply reg_sd() as follows:
reg_sd(0.1)
reg_sd(1.0)



# 2. (10pts) Create a vector of 10 sd values from=0.1, to=1.0, and call it "vec_sd",
# hint: you can use function seq(), but don't have to,

### write your code here


# add the following names to "vec_sd": "sd=0.1", "sd=0.2", etc.
# use functions names() and paste0(),

### write your code here


# apply function reg_sd() over the vector "vec_sd", and call it "reg_stats",
# the first row of "reg_stats" should contain "p-value", the second row 
# should contain "adj.r.squared", and third row "fstat",
# use function sapply(),

### write your code here



# 3. (20pts) write a for() loop to plot the three rows of "reg_stats" in three vertical panels, 
# use functions par() and plot(),

### write your code here
