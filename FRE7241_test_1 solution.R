#################################
### FRE7241 Test #1 Solutions 04/28/15
#################################
# Max score 50pts

# The below solutions are examples,
# Slightly different solutions are also possible.

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

reg_sd <- function(sd) {
# initialize random number generator
  set.seed(1121)
# create explanatory variable
  indep_var <- 0.1*1:30
# calculate response variable
  depend_var <- 3 + 0.2*indep_var + rnorm(30, sd=sd)
# specify regression formula
  reg_formula <- depend_var ~ indep_var
# perform regression
  reg_model <- lm(reg_formula)
# calculate regression summary
  reg_model_sum <- summary(reg_model)
# extract regression statistics
  c(pval=reg_model_sum$coefficients[2, 4],
    adj.r.squared=reg_model_sum$adj.r.squared,
    fstat=reg_model_sum$fstatistic[1])
}  # end reg_sd

# apply reg_sd() as follows:
reg_sd(0.1)
reg_sd(1.0)



# 2. (10pts) Create a vector of 10 sd values from=0.1, to=1.0, and call it "vec_sd",
# hint: you can use function seq(), but don't have to,

vec_sd <- seq(from=0.1, to=1.0, length.out=10)


# add the following names to "vec_sd": "sd=0.1", "sd=0.2", etc.
# use functions names() and paste0(),

names(vec_sd) <- paste0("sd=", vec_sd)


# apply function reg_sd() over the vector "vec_sd", and call it "reg_stats",
# the first row of "reg_stats" should contain "p-value", the second row 
# should contain "adj.r.squared", and third row "fstat",
# use function sapply(),

reg_stats <- sapply(vec_sd, reg_sd)



# 3. (20pts) write a for() loop to plot the three rows of "reg_stats" in three vertical panels, 
# use functions par() and plot(),

# set three vertical plot panels
par(mfrow=c(3,1))
# plot in loop
for (in_dex in 1:3) {
  plot.zoo(reg_stats[in_dex, ], xaxt="n", xlab="", ylab=rownames(reg_stats)[in_dex])
  axis(1, at=1:(ncol(reg_stats)), labels=colnames(reg_stats))  
}  # end for

