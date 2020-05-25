# Perform two-tailed test that sample is
# from Standard Normal Distribution (mean=0, SD=1)
# generate vector of samples and store in data frame
test_frame <- data.frame(samples=rnorm(1e4))
# get p-values for all the samples
test_frame$p_values <- sapply(test_frame$samples,
        function(x) 2*pnorm(-abs(x)))
# Significance level, two-tailed test, critical value=2*SD
signif_level <- 2*(1-pnorm(2))
# Compare p_values to significance level
test_frame$result <-
  test_frame$p_values > signif_level
# Number of null rejections
sum(!test_frame$result) / NROW(test_frame)
# Show null rejections
head(test_frame[!test_frame$result, ])

x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
# Plot the Normal probability distribution
curve(expr=dnorm(x, sd=1), type="l", xlim=c(-4, 4),
xlab="", ylab="", lwd=3, col="blue")
title(main="Two-tailed Test", line=0.5)
# Plot tails of the distribution using polygons
star_t <- 2; e_nd <- 4
# Plot right tail using polygon
x_var <- seq(star_t, e_nd, length=100)
y_var <- dnorm(x_var, sd=1)
y_var[1] <- (-1)
y_var[NROW(y_var)] <- (-1)
polygon(x=x_var, y=y_var, col="red")
# Plot left tail using polygon
y_var <- dnorm(-x_var, sd=1)
y_var[1] <- (-1)
y_var[NROW(y_var)] <- (-1)
polygon(x=(-x_var), y=y_var, col="red")

# t-test for single sample
t.test(rnorm(100))
# t-test for two samples
t.test(rnorm(100),
       rnorm(100, mean=1))
# Plot the normal and t-distribution densities
x11(width=6, height=5)
par(mar=c(3, 3, 3, 1), oma=c(0, 0, 0, 0))
curve(expr=dnorm, xlim=c(-4, 4),
      xlab="", ylab="", lwd=3)
curve(expr=dt(x, df=3),
      xlab="", ylab="", lwd=3,
      col="red", add=TRUE)
# Add title
title(main="Normal and t-distribution densities", line=0.5)
# Add legend
legend("topright", inset=0.05, bty="n",
       title=NULL, c("normal", "t-dist"),
       cex=0.8, lwd=6, lty=1,
       col=c("black", "red"))

# VTI returns
re_turns <- as.numeric(na.omit(
  rutils::etf_env$re_turns[, "VTI"]))
# Wilcoxon test for normal distribution
da_ta <- rnorm(NROW(re_turns), sd=100)
wilcox.test(da_ta)
# Skewed distribution with mean=0
mean(re_turns); median(re_turns)
wilcox.test(re_turns-mean(re_turns))
# Same as
wilcox.test(re_turns-mean(re_turns),
  rep(0, NROW(re_turns)), paired=TRUE)
# Skewed distribution with median=0
wilcox.test(re_turns-median(re_turns))
# Normal samples with different standard deviations
n_rows <- 1e3
sample1 <- rnorm(n_rows, sd=1)
sample2 <- rnorm(n_rows, sd=10)
wilcox.test(sample1, sample2,
      paired=TRUE)

# Wilcoxon test for random data around 0
da_ta <- (runif(n_rows) - 0.5)
wil_cox <- wilcox.test(da_ta)
# Calculate V statistic of Wilcoxon test
wil_cox$statistic
sum(rank(abs(da_ta))[da_ta>0])
# Calculate W statistic of Wilcoxon test
sum(sign(da_ta)*rank(abs(da_ta)))
# Two sets of normal data
sample1 <- rnorm(n_rows)
sample2 <- rnorm(n_rows, mean=0.1)
# Wilcoxon test
wil_cox <- wilcox.test(sample1, sample2,
                 paired=TRUE)
wil_cox$statistic
# Calculate V statistic of Wilcoxon test
da_ta <- (sample1 - sample2)
sum(rank(abs(da_ta))[da_ta>0])
# Calculate W statistic of Wilcoxon test
sum(sign(da_ta)*rank(abs(da_ta)))

# Calculate distributon of Wilcoxon W statistic
wilcox_w <- sapply(1:1e3, function(x) {
  da_ta <- (runif(n_rows) - 0.5)
  sum(sign(da_ta)*rank(abs(da_ta)))
})  # end sapply
wilcox_w <- wilcox_w/sqrt(n_rows*(n_rows+1)*(2*n_rows+1)/6)
var(wilcox_w)
x11(width=6, height=5)  # Plot in window
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
hist(wilcox_w, col="lightgrey",
     xlab="returns", breaks=50, xlim=c(-3, 3),
     ylab="frequency", freq=FALSE,
     main="Wilcoxon W Statistic Histogram")
lines(density(wilcox_w, bw=0.4), lwd=3, col="red")
curve(expr=dnorm, add=TRUE, lwd=3, col="blue")
# Add legend
legend("topright", inset=0.05, bty="n",
 leg=c("W density", "Normal"),
 lwd=6, lty=1, col=c("red", "blue"))

# Wilcoxon test for two normal distributions
sample1 <- rnorm(1e2)
sample2 <- rnorm(1e2, mean=0.1)
wilcox.test(sample1, sample2,
      paired=TRUE)$p.value
t.test(sample1, sample2)$p.value
# Wilcoxon test with data outliers
sample2 <- rnorm(1e2)
sample2[1:3] <- sample2[1:3] + 1e3
wilcox.test(sample1, sample2,
      paired=TRUE)$p.value
t.test(sample1, sample2)$p.value

# Create a list with two elements
lis_t <- list(c('a', 'b'), 1:4)
lis_t
c(typeof(lis_t), mode(lis_t), class(lis_t))
# Lists are also vectors
c(is.vector(lis_t), is.list(lis_t))
NROW(lis_t)
# Create named list
lis_t <- list(first=c('a', 'b'), second=1:4)
lis_t
names(lis_t)
unlist(lis_t)

lis_t[2]  # Extract second element as sublist
lis_t[[2]]  # Extract second element
lis_t[[2]][3]  # Extract third element of second element
lis_t[[c(2, 3)]]  # third element of second element
lis_t$second  # Extract second element
lis_t$s  # Extract second element - partial name matching
lis_t$second[3]  # third element of second element
lis_t <- list()  # empty list
lis_t$a <- 1
lis_t[2] <- 2
lis_t
names(lis_t)

as.list(c(1,2,3))
list(c(1,2,3))

data_frame <- data.frame(  # Create a data frame
                type=c('rose', 'daisy', 'tulip'),
                color=c('red', 'white', 'yellow'),
                price=c(1.5, 0.5, 1.0)
              )  # end data.frame
data_frame
dim(data_frame)  # Get dimension attribute
colnames(data_frame)  # Get the colnames attribute
rownames(data_frame)  # Get the rownames attribute
class(data_frame)  # Get object class
typeof(data_frame)  # Data frames are lists
is.data.frame(data_frame)

class(data_frame$type)  # Get column class
class(data_frame$price)  # Get column class

data_frame[, 3]  # Extract third column as vector
data_frame[[3]]  # Extract third column as vector
data_frame[3]  # Extract third column as data frame
data_frame[, 3, drop=FALSE]  # Extract third column as data frame
data_frame[[3]][2]  # Second element from third column
data_frame$price[2]  # Second element from 'price' column
is.data.frame(data_frame[[3]]); is.vector(data_frame[[3]])
data_frame[2, ]  # Extract second row
data_frame[2, ][3]  # third element from second column
data_frame[2, 3]  # third element from second column
unlist(data_frame[2, ])  # Coerce to vector
is.data.frame(data_frame[2, ]); is.vector(data_frame[2, ])

data_frame <- data.frame(  # Create a data frame
                type=c('rose', 'daisy', 'tulip'),
                color=c('red', 'white', 'yellow'),
                price=c(1.5, 0.5, 1.0),
                row.names=c('flower1', 'flower2', 'flower3'),
                stringsAsFactors=FALSE
              )  # end data.frame
data_frame
class(data_frame$type)  # Get column class
class(data_frame$price)  # Get column class
# Set option to not coerce character vectors to factors
options(stringsAsFactors=FALSE)
options("stringsAsFactors")
default.stringsAsFactors()

str(data_frame)  # Display the object structure
dim(cars)  # the cars data frame has 50 rows
head(cars, n=5)  # Get first five rows
tail(cars, n=5)  # Get last five rows

# Create a named vector
stu_dents <- sample(round(runif(5, min=1, max=10), digits=2))
names(stu_dents) <- c("Angie", "Chris", "Suzie", "Matt", "Liz")
# Sort the vector into ascending order
sort(stu_dents)
# Calculate index to sort into ascending order
order(stu_dents)
# Sort the vector into ascending order
stu_dents[order(stu_dents)]
# Calculate the sorted (ordered) vector
sort_ed <- stu_dents[order(stu_dents)]
# Calculate index to sort into unsorted (original) order
order(order(stu_dents))
sort_ed[order(order(stu_dents))]
stu_dents
# Create a data frame of stu_dents and their ranks
ra_nks <- c("first", "second", "third", "fourth", "fifth")
data.frame(students=stu_dents,
  rank=ra_nks[order(order(stu_dents))])
# Permute data_frame of flowers on price column
order(data_frame$price)
# Sort data_frame on price
data_frame[order(data_frame$price), ]
# Sort data_frame on color
data_frame[order(data_frame$color), ]
# Read the Examples for sort()
order(c(2, 1:4))  # there's a tie
order(c(2, 1:4), 1:5)  # there's a tie

as.matrix(data_frame)
vec_tor <- sample(9)
matrix(vec_tor, ncol=3)
as.matrix(vec_tor, ncol=3)

mat_rix <- matrix(5:10, nrow=2, ncol=3)  # Create a matrix
rownames(mat_rix) <- c("row1", "row2")  # Rownames attribute
colnames(mat_rix) <- c("col1", "col2", "col3")  # Colnames attribute
library(microbenchmark)
# Call method instead of generic function
as.data.frame.matrix(mat_rix)
# A few methods for generic function as.data.frame()
sample(methods(as.data.frame), size=4)
# Function method is faster than generic function
summary(microbenchmark(
  as_data_frame_matrix=
    as.data.frame.matrix(mat_rix),
  as_data_frame=as.data.frame(mat_rix),
  data_frame=data.frame(mat_rix),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

library(microbenchmark)
# lapply is faster than coercion function
summary(microbenchmark(
  as_list=
    as.list(as.data.frame.matrix(mat_rix)),
  l_apply=
    lapply(seq_along(mat_rix[1, ]),
     function(in_dex) mat_rix[, in_dex]),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# ?iris  # Get information on iris
dim(iris)
head(iris, 2)
colnames(iris)
unique(iris$Species)  # List of unique elements of iris
class(unique(iris$Species))
# Find which columns of iris are numeric
sapply(iris, is.numeric)
# Calculate means of iris columns
sapply(iris, mean)  # Returns NA for Species

# ?mtcars  # mtcars data from 1974 Motor Trend magazine
# mpg   Miles/(US) gallon
# qsec   1/4 mile time
# hp	 Gross horsepower
# wt	 Weight (lb/1000)
# cyl   Number of cylinders
dim(mtcars)
head(mtcars, 2)
colnames(mtcars)
head(rownames(mtcars), 3)
unique(mtcars$cyl)  # Extract list of car cylinders
sapply(mtcars, mean)  # Calculate means of mtcars columns

library(MASS)
# ?Cars93  # Get information on Cars93
dim(Cars93)
head(colnames(Cars93))
# head(Cars93, 2)
unique(Cars93$Type)  # Extract list of car types
# sapply(Cars93, mean)  # Calculate means of Cars93 columns
# Plot histogram of Highway MPG using the Freedman-Diaconis rule
hist(Cars93$MPG.highway, col="lightblue1",
     main="Distance per Gallon 1993", xlab="Highway MPG", breaks="FD")

rm(list=ls())
as.numeric(c(1:3, "a"))  # NA from coercion
0/0  # NaN from ambiguous math
1/0  # Inf from divide by zero
is.na(c(NA, NaN, 0/0, 1/0))  # test for NA
is.nan(c(NA, NaN, 0/0, 1/0))  # test for NaN
NA*1:4  # Create vector of Nas
# Create vector with some NA values
da_ta <- c(1, 2, NA, 4, NA, 5)
da_ta
mean(da_ta)  # Returns NA, when NAs are input
mean(da_ta, na.rm=TRUE)  # remove NAs from input data
da_ta[!is.na(da_ta)]  # Delete the NA values
sum(!is.na(da_ta))  # Count non-NA values

rm(list=ls())
# Airquality data has some NAs
head(airquality)
dim(airquality)
# Number of NA elements
sum(is.na(airquality))
# Number of rows with NA elements
sum(!complete.cases(airquality))
# Display rows containing NAs
head(airquality[!complete.cases(airquality), ])

rm(list=ls())
# Remove rows containing NAs
good_air <- airquality[complete.cases(airquality), ]
dim(good_air)
head(good_air)  # NAs removed
library(zoo)  # load package zoo
# Replace NAs
good_air <- zoo::na.locf(airquality)
dim(good_air)
head(good_air)  # NAs replaced
# Create vector containing NA values
vec_tor <- sample(22)
vec_tor[sample(NROW(vec_tor), 4)] <- NA
# Replace NA values with the most recent non-NA values
zoo::na.locf(vec_tor)
# Replace NAs in xts time series
se_ries <- rutils::etf_env$price_s[, 1]
head(se_ries)
sum(is.na(se_ries))
library(quantmod)
series_zoo <- as.xts(zoo::na.locf(se_ries, na.rm=FALSE, fromLast=TRUE))
series_xts <- xts:::na.locf.xts(se_ries, fromLast=TRUE)
all.equal(series_zoo, series_xts, check.attributes=FALSE)
library(microbenchmark)
summary(microbenchmark(
  zoo=as.xts(zoo::na.locf(se_ries, fromLast=TRUE)),
  xts=xts:::na.locf.xts(se_ries, fromLast=TRUE),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# NULL values have no mode or type
c(mode(NULL), mode(NA))
c(typeof(NULL), typeof(NA))
c(length(NULL), length(NA))
# Check for NULL values
is.null(NULL)
# NULL values are ignored when combined into a vector
c(1, 2, NULL, 4, 5)
# But NA value isn't ignored
c(1, 2, NA, 4, 5)
# Vectors can be initialized to NULL
vec_tor <- NULL
is.null(vec_tor)
# Grow the vector in a loop - very bad code!!!
for (in_dex in 1:5)
  vec_tor <- c(vec_tor, in_dex)
# Initialize empty vector
vec_tor <- numeric()
# Grow the vector in a loop - very bad code!!!
for (in_dex in 1:5)
  vec_tor <- c(vec_tor, in_dex)
# Allocate vector
vec_tor <- numeric(5)
# Assign to vector in a loop - good code
for (in_dex in 1:5)
  vec_tor[in_dex] <- runif(1)

# Formula of linear model with zero intercept
for_mula <- z ~ x + y - 1
for_mula

# Collapse vector of strings into single text string
paste0("x", 1:5)
paste(paste0("x", 1:5), collapse="+")

# Create formula from text string
for_mula <- as.formula(
  # Coerce text strings to formula
  paste("z ~ ",
  paste(paste0("x", 1:5), collapse="+")
  )  # end paste
)  # end as.formula
class(for_mula)
for_mula
# Modify the formula using "update"
update(for_mula, log(.) ~ . + beta)

# Define explanatory (design) variable
len_gth <- 100
set.seed(1121)  # initialize random number generator
de_sign <- runif(len_gth)
noise <- rnorm(len_gth)
# Response equals linear form plus random noise
res_ponse <- (1 + de_sign + noise)

# Calculate de-meaned explanatory (design) and response vectors
design_zm <- de_sign - mean(de_sign)
response_zm <- res_ponse - mean(res_ponse)
# Calculate the regression beta
be_ta <- sum(design_zm*response_zm)/sum(design_zm^2)
# Calculate the regression alpha
al_pha <- mean(res_ponse) - be_ta*mean(de_sign)

# Specify regression formula
for_mula <- res_ponse ~ de_sign
mod_el <- lm(for_mula)  # Perform regression
class(mod_el)  # Regressions have class lm
attributes(mod_el)
eval(mod_el$call$formula)  # Regression formula
mod_el$coeff  # Regression coefficients
all.equal(coef(mod_el), c(al_pha, be_ta), 
  check.attributes=FALSE)

fit_ted <- (al_pha + be_ta*de_sign)
all.equal(fit_ted, mod_el$fitted.values, check.attributes=FALSE)
x11(width=5, height=4)  # Open x11 for plotting
# Set plot parameters to reduce whitespace around plot
par(mar=c(5, 5, 2, 1), oma=c(0, 0, 0, 0))
# Plot scatterplot using formula
plot(for_mula, xlab="design", ylab="response")
title(main="Simple Regression", line=0.5)
# Add regression line
abline(mod_el, lwd=3, col="blue")
# Plot fitted (predicted) response values
points(x=de_sign, y=mod_el$fitted.values,
       pch=16, col="blue")

# Plot response without noise
lines(x=de_sign, y=(res_ponse-noise),
      col="red", lwd=3)
legend(x="topleft", # Add legend
       legend=c("response without noise", "fitted values"),
       title=NULL, inset=0.08, cex=0.8, lwd=6,
       lty=1, col=c("red", "blue"))

# Calculate the residuals
fit_ted <- (al_pha + be_ta*de_sign)
resid_uals <- (res_ponse - fit_ted)
all.equal(resid_uals, mod_el$residuals, check.attributes=FALSE)
# Residuals are orthogonal to the de_sign
all.equal(sum(resid_uals*de_sign), target=0)
# Residuals are orthogonal to the fitted values
all.equal(sum(resid_uals*fit_ted), target=0)
# Sum of residuals is equal to zero
all.equal(mean(resid_uals), target=0)

x11(width=6, height=5)  # Open x11 for plotting
# Set plot parameters to reduce whitespace around plot
par(mar=c(5, 5, 1, 1), oma=c(0, 0, 0, 0))
# Extract residuals
da_ta <- cbind(de_sign, mod_el$residuals)
colnames(da_ta) <- c("design", "residuals")
# Plot residuals
plot(da_ta)
title(main="Residuals of the Linear Regression", line=-1)
abline(h=0, lwd=3, col="red")

# Degrees of freedom of residuals
deg_free <- mod_el$df.residual
# Standard deviation of residuals
resid_stddev <- 
  sqrt(sum(resid_uals^2)/deg_free)
# Standard error of beta
beta_stderror <- 
  resid_stddev/sqrt(sum(design_zm^2))
# Standard error of alpha
alpha_stderror <- resid_stddev*
  sqrt(1/len_gth + mean(de_sign)^2/sum(design_zm^2))

model_sum <- summary(mod_el)  # Copy regression summary
model_sum  # Print the summary to console
attributes(model_sum)$names  # get summary elements

model_sum$coeff
# Standard errors
model_sum$coefficients[2, "Std. Error"]
all.equal(c(alpha_stderror, beta_stderror), 
  model_sum$coefficients[, "Std. Error"], check.attributes=FALSE)
# R-squared
model_sum$r.squared
model_sum$adj.r.squared
# F-statistic and ANOVA
model_sum$fstatistic
anova(mod_el)

set.seed(1121)  # initialize random number generator
# High noise compared to coefficient
res_ponse <- (1 + de_sign + rnorm(len_gth, sd=8))
mod_el <- lm(for_mula)  # Perform regression
# Values of regression coefficients are not
# Statistically significant
summary(mod_el)

par(oma=c(1, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=1.0, cex.axis=1.0, cex.main=1.0, cex.sub=1.0)
reg_stats <- function(std_dev) {  # Noisy regression
  set.seed(1121)  # initialize number generator
# Define explanatory (design) and response variables
  de_sign <- rnorm(100, mean=2)
  res_ponse <- (1 + 0.2*de_sign +
    rnorm(NROW(de_sign), sd=std_dev))
# Specify regression formula
  for_mula <- res_ponse ~ de_sign
# Perform regression and get summary
  model_sum <- summary(lm(for_mula))
# Extract regression statistics
  with(model_sum, c(pval=coefficients[2, 4],
   adj_rsquared=adj.r.squared,
   fstat=fstatistic[1]))
}  # end reg_stats
# Apply reg_stats() to vector of std dev values
vec_sd <- seq(from=0.1, to=0.5, by=0.1)
names(vec_sd) <- paste0("sd=", vec_sd)
mat_stats <- t(sapply(vec_sd, reg_stats))
# Plot in loop
par(mfrow=c(NCOL(mat_stats), 1))
for (in_dex in 1:NCOL(mat_stats)) {
  plot(mat_stats[, in_dex], type="l",
 xaxt="n", xlab="", ylab="", main="")
  title(main=colnames(mat_stats)[in_dex], line=-1.0)
  axis(1, at=1:(NROW(mat_stats)),
 labels=rownames(mat_stats))
}  # end for

reg_stats <- function(da_ta) {  # get regression
# Perform regression and get summary
  col_names <- colnames(da_ta)
  for_mula <-
    paste(col_names[2], col_names[1], sep="~")
  model_sum <- summary(lm(for_mula,
                        data=da_ta))
# Extract regression statistics
  with(model_sum, c(pval=coefficients[2, 4],
   adj_rsquared=adj.r.squared,
   fstat=fstatistic[1]))
}  # end reg_stats
# Apply reg_stats() to vector of std dev values
vec_sd <- seq(from=0.1, to=0.5, by=0.1)
names(vec_sd) <- paste0("sd=", vec_sd)
mat_stats <-
  t(sapply(vec_sd, function(std_dev) {
    set.seed(1121)  # initialize number generator
# Define explanatory (design) and response variables
    de_sign <- rnorm(100, mean=2)
    res_ponse <- (1 + 0.2*de_sign +
rnorm(NROW(de_sign), sd=std_dev))
    reg_stats(data.frame(de_sign, res_ponse))
    }))
# Plot in loop
par(mfrow=c(NCOL(mat_stats), 1))
for (in_dex in 1:NCOL(mat_stats)) {
  plot(mat_stats[, in_dex], type="l",
 xaxt="n", xlab="", ylab="", main="")
  title(main=colnames(mat_stats)[in_dex], line=-1.0)
  axis(1, at=1:(NROW(mat_stats)),
 labels=rownames(mat_stats))
}  # end for

# Set plot paramaters - margins and font scale
par(oma=c(1,0,1,0), mgp=c(2,1,0), mar=c(2,1,2,1), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2, 2))  # Plot 2x2 panels
plot(mod_el)  # Plot diagnostic scatterplots
plot(mod_el, which=2)  # Plot just Q-Q

library(lmtest)  # Load lmtest
# Perform Durbin-Watson test
lmtest::dwtest(mod_el)

x11(width=6, height=5)  # Open x11 for plotting
# Set plot parameters to reduce whitespace around plot
par(mar=c(5, 5, 2, 1), oma=c(0, 0, 0, 0))
# Add unit column to the design matrix
de_sign <- cbind(rep(1, NROW(de_sign)), de_sign)
# Calculate generalized inverse of the design matrix
design_inv <- MASS::ginv(de_sign)
# Calculate the influence matrix
influ_ence <- de_sign %*% design_inv
# Plot the leverage vector
or_der <- order(de_sign[, 2])
plot(x=de_sign[or_der, 2], y=diag(influ_ence)[or_der],
     type="l", lwd=3, col="blue",
     xlab="predictor", ylab="leverage",
     main="Leverage as Function of Predictor")

# Calculate the influence matrix
influ_ence <- de_sign %*% design_inv
# The influence matrix is idempotent
all.equal(influ_ence, influ_ence %*% influ_ence)

# Calculate covariance and standard deviations of fitted values
beta_s <- design_inv %*% res_ponse
fit_ted <- drop(de_sign %*% beta_s)
resid_uals <- drop(res_ponse - fit_ted)
deg_free <- (NROW(de_sign) - NCOL(de_sign))
var_resid <- sqrt(sum(resid_uals^2)/deg_free)
fit_covar <- var_resid*influ_ence
fit_sd <- sqrt(diag(fit_covar))
# Plot the standard deviations
fit_sd <- cbind(fitted=fit_ted, stddev=fit_sd)
fit_sd <- fit_sd[order(fit_ted), ]
plot(fit_sd, type="l", lwd=3, col="blue",
     xlab="Fitted Value", ylab="Standard Deviation",
     main="Standard Deviations of Fitted Values\nin Univariate Regression")

# Calculate response without random noise for univariate regression,
# equal to weighted sum over columns of de_sign
weight_s <- c(-1, 1)
res_ponse <- de_sign %*% weight_s
# Perform loop over different realizations of random noise
fit_ted <- lapply(1:50, function(it) {
  # Add random noise to response
  res_ponse <- res_ponse + rnorm(len_gth, sd=1.0)
  # Calculate fitted values using influence matrix
  influ_ence %*% res_ponse
})  # end lapply
fit_ted <- rutils::do_call(cbind, fit_ted)

x11(width=5, height=4)  # Open x11 for plotting
# Set plot parameters to reduce whitespace around plot
par(mar=c(5, 5, 2, 1), oma=c(0, 0, 0, 0))
# Plot fitted values
matplot(x=de_sign[,2], y=fit_ted,
type="l", lty="solid", lwd=1, col="blue",
xlab="predictor", ylab="fitted",
main="Fitted Values for Different Realizations
of Random Noise")
lines(x=de_sign[,2], y=res_ponse, col="red", lwd=4)
legend(x="topleft", # Add legend
       legend=c("response without noise", "fitted values"),
       title=NULL, inset=0.05, cex=0.8, lwd=6,
       lty=1, col=c("red", "blue"))

# Inverse of design matrix squared
design_2 <- MASS::ginv(crossprod(de_sign))
# Define new predictors
new_data <- (max(de_sign[, 2]) + 10*(1:5)/len_gth)
# Calculate the predicted values and standard errors
design_new <- cbind(rep(1, NROW(new_data)), new_data)
predic_tions <- cbind(
  prediction=drop(design_new %*% beta_s),
  stddev=diag(var_resid*sqrt(design_new %*% design_2 %*% t(design_new))))
# OR: Perform loop over new_data
predic_tions <- sapply(new_data, function(predic_tor) {
  predic_tor <- cbind(1, predic_tor)
  # Calculate predicted values
  predic_tion <- predic_tor %*% beta_s
  # Calculate standard deviation
  predict_sd <- var_resid*sqrt(predic_tor %*% design_2 %*% t(predic_tor))
  c(prediction=predic_tion, stddev=predict_sd)
})  # end sapply
predic_tions <- t(predic_tions)

# Prepare plot data
x_data <- c(de_sign[,2], new_data)
x_lim <- range(x_data)
y_data <- c(fit_ted, predic_tions[, 1])
# Calculate t-quantile
t_quant <- qt(pnorm(2), df=deg_free)
predict_low <- predic_tions[, 1]-t_quant*predic_tions[, 2]
predict_high <- predic_tions[, 1]+t_quant*predic_tions[, 2]
y_lim <- range(c(res_ponse, y_data, predict_low, predict_high))
# Plot the regression predictions
plot(x=x_data, y=y_data,
     xlim=x_lim, ylim=y_lim,
     type="l", lwd=3, col="blue",
     xlab="predictor", ylab="fitted or predicted",
     main="Predictions from Linear Regression")
points(x=de_sign[,2], y=res_ponse, col="blue")
points(x=new_data, y=predic_tions[, 1], pch=16, col="blue")
lines(x=new_data, y=predict_high, lwd=3, col="red")
lines(x=new_data, y=predict_low, lwd=3, col="green")
legend(x="topleft", # Add legend
       legend=c("predictions", "+2SD", "-2SD"),
       title=NULL, inset=0.05, cex=0.8, lwd=6,
       lty=1, col=c("blue", "red", "green"))

# Perform univariate regression
predic_tor <- de_sign[, 2]
mod_el <- lm(res_ponse ~ predic_tor)
# Perform prediction from regression
new_data <- data.frame(predic_tor=new_data)
predict_lm <- predict(object=mod_el,
  newdata=new_data, level=1-2*(1-pnorm(2)),
  interval="confidence")
predict_lm <- as.data.frame(predict_lm)
all.equal(predict_lm$fit, predic_tions[, 1])
all.equal(predict_lm$lwr, predict_low)
all.equal(predict_lm$upr, predict_high)
plot(res_ponse ~ predic_tor,
     xlim=range(predic_tor, new_data),
     ylim=range(res_ponse, predict_lm),
     xlab="predictor", ylab="fitted or predicted",
     main="Predictions from lm() Regression")

abline(mod_el, col="blue", lwd=3)
with(predict_lm, {
  points(x=new_data$predic_tor, y=fit, pch=16, col="blue")
  lines(x=new_data$predic_tor, y=lwr, lwd=3, col="green")
  lines(x=new_data$predic_tor, y=upr, lwd=3, col="red")
})  # end with
legend(x="topleft", # Add legend
       legend=c("predictions", "+2SD", "-2SD"),
       title=NULL, inset=0.05, cex=0.8, lwd=6,
       lty=1, col=c("blue", "red", "green"))

set.seed(1121)
library(lmtest)
# Spurious regression in unit root time series
de_sign <- cumsum(rnorm(100))  # Unit root time series
res_ponse <- cumsum(rnorm(100))
for_mula <- res_ponse ~ de_sign
mod_el <- lm(for_mula)  # Perform regression
# Summary indicates statistically significant regression
model_sum <- summary(mod_el)
model_sum$coeff
model_sum$r.squared
# Durbin-Watson test shows residuals are autocorrelated
dw_test <- dwtest(mod_el)
c(dw_test$statistic[[1]], dw_test$p.value)

par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # Set plot panels
plot(for_mula, xlab="", ylab="")  # Plot scatterplot using formula
title(main="Spurious Regression", line=-1)
# Add regression line
abline(mod_el, lwd=2, col="red")
plot(mod_el, which=2, ask=FALSE)  # Plot just Q-Q

set.seed(1121)  # initialize random number generator
# Define design matrix
n_rows <- 100
n_cols <- 5
de_sign <- sapply(rep(n_rows, n_cols), rnorm)
# Add column names
colnames(de_sign) <- paste0("col", 1:n_cols)
# Plot design matrix
# matplot(de_sign, type="l", lty="solid", lwd=3)
# Define the design weights
weight_s <- sample(3:(n_cols+2))
# Response equals linear form plus random noise
noise <- rnorm(n_rows, sd=5)
res_ponse <- (-1 + de_sign %*% weight_s + noise)

# Perform multivariate regression using lm()
mod_el <- lm(res_ponse ~ de_sign)
# Solve multivariate regression using matrix algebra
# Calculate de-meaned design matrix and response vector
design_zm <- t(t(de_sign) - colMeans(de_sign))
# de_sign <- apply(de_sign, 2, function(x) (x-mean(x)))
response_zm <- res_ponse - mean(res_ponse)
# Calculate the regression coefficients
beta_s <- MASS::ginv(design_zm) %*% response_zm
# Calculate the regression alpha
al_pha <- mean(res_ponse) - 
  sum(colSums(de_sign)*drop(beta_s))/n_rows
# Compare with coefficients from lm()
all.equal(coef(mod_el), c(al_pha, beta_s), check.attributes=FALSE)
# Compare with actual coefficients
all.equal(c(-1, weight_s), c(al_pha, beta_s), check.attributes=FALSE)

# Add intercept column to design matrix
de_sign <- cbind(rep(1, NROW(de_sign)), de_sign)
n_cols <- NCOL(de_sign)
# Add column name
colnames(de_sign)[1] <- "intercept"
# Calculate generalized inverse of the design matrix
design_inv <- MASS::ginv(de_sign)
# Calculate the regression coefficients
beta_s <- design_inv %*% res_ponse
# Perform multivariate regression without intercept term
mod_el <- lm(res_ponse ~ de_sign - 1)
all.equal(drop(beta_s), coef(mod_el), check.attributes=FALSE)

# Calculate fitted values from regression coefficients
fit_ted <- drop(de_sign %*% beta_s)
all.equal(fit_ted, mod_el$fitted.values, check.attributes=FALSE)
# Calculate the residuals
resid_uals <- drop(res_ponse - fit_ted)
all.equal(resid_uals, mod_el$residuals, check.attributes=FALSE)
# Residuals are orthogonal to de_sign columns (predictors)
sapply(resid_uals %*% de_sign, 
       all.equal, target=0)
# Residuals are orthogonal to the fitted values
all.equal(sum(resid_uals*fit_ted), target=0)
# Sum of residuals is equal to zero
all.equal(sum(resid_uals), target=0)

# Calculate the influence matrix
influ_ence <- de_sign %*% design_inv
# The influence matrix is idempotent
all.equal(influ_ence, influ_ence %*% influ_ence)
# Calculate fitted values using influence matrix
fit_ted <- drop(influ_ence %*% res_ponse)
all.equal(fit_ted, mod_el$fitted.values, check.attributes=FALSE)
# Calculate fitted values from regression coefficients
fit_ted <- drop(de_sign %*% beta_s)
all.equal(fit_ted, mod_el$fitted.values, check.attributes=FALSE)

# Calculate zero mean fitted values
design_zm <- t(t(de_sign) - colMeans(de_sign))
fitted_zm <- drop(design_zm %*% beta_s)
all.equal(fitted_zm, 
  mod_el$fitted.values - mean(res_ponse), 
  check.attributes=FALSE)
# Calculate the residuals
response_zm <- res_ponse - mean(res_ponse)
resid_uals <- drop(response_zm - fitted_zm)
all.equal(resid_uals, mod_el$residuals, 
  check.attributes=FALSE)
# Calculate the influence matrix
influence_zm <- design_zm %*% MASS::ginv(design_zm)
# Compare the fitted values
all.equal(fitted_zm, 
  drop(influence_zm %*% response_zm), 
  check.attributes=FALSE)

library(lmtest)  # Load lmtest
de_sign <- data.frame(  # Design matrix
  de_sign=1:30, omit_var=sin(0.2*1:30))
# Response depends on both predictors
res_ponse <- with(de_sign,
  0.2*de_sign + omit_var + 0.2*rnorm(30))
# Mis-specified regression only one predictor
mod_el <- lm(res_ponse ~ de_sign,
        data=de_sign)
model_sum <- summary(mod_el)
model_sum$coeff
model_sum$r.squared
# Durbin-Watson test shows residuals are autocorrelated
dwtest(mod_el)$p.value

par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # Set plot panels
plot(for_mula, data=de_sign)
abline(mod_el, lwd=2, col="red")
title(main="OVB Regression", line=-1)
plot(mod_el, which=2, ask=FALSE)  # Plot just Q-Q

# Regression model summary
model_sum <- summary(mod_el)
# Degrees of freedom of residuals
n_rows <- NROW(de_sign)
n_cols <- NCOL(de_sign)
deg_free <- (n_rows - n_cols)
all.equal(deg_free, model_sum$df[2])
# Variance of residuals
var_resid <- sum(resid_uals^2)/deg_free

# Inverse of design matrix squared
design_2 <- MASS::ginv(crossprod(de_sign))
# design_2 <- t(de_sign) %*% de_sign
# Variance of residuals
var_resid <- sum(resid_uals^2)/deg_free
# Calculate covariance matrix of betas
beta_covar <- var_resid*design_2
# Round(beta_covar, 3)
beta_sd <- sqrt(diag(beta_covar))
all.equal(beta_sd, model_sum$coeff[, 2], check.attributes=FALSE)
# Calculate t-values of betas
beta_tvals <- drop(beta_s)/beta_sd
all.equal(beta_tvals, model_sum$coeff[, 3], check.attributes=FALSE)
# Calculate two-sided p-values of betas
beta_pvals <- 2*pt(-abs(beta_tvals), df=deg_free)
all.equal(beta_pvals, model_sum$coeff[, 4], check.attributes=FALSE)
# The square of the generalized inverse is equal 
# to the inverse of the square
all.equal(MASS::ginv(crossprod(de_sign)), 
  design_inv %*% t(design_inv))

# Calculate the influence matrix
influ_ence <- de_sign %*% design_inv
# The influence matrix is idempotent
all.equal(influ_ence, influ_ence %*% influ_ence)

# Calculate covariance and standard deviations of fitted values
fit_covar <- var_resid*influ_ence
fit_sd <- sqrt(diag(fit_covar))
# Sort the standard deviations
fit_sd <- cbind(fitted=fit_ted, stddev=fit_sd)
fit_sd <- fit_sd[order(fit_ted), ]
# Plot the standard deviations
plot(fit_sd, type="l", lwd=3, col="blue",
     xlab="Fitted Value", ylab="Standard Deviation",
     main="Standard Deviations of Fitted Values\nin Multivariate Regression")

# Load time series of ETF percentage returns
re_turns <- rutils::etf_env$re_turns[, c("XLF", "XLE")]
re_turns <- na.omit(re_turns)
n_rows <- NROW(re_turns)
head(re_turns)
# Define regression formula
for_mula <- paste(colnames(re_turns)[1],
  paste(colnames(re_turns)[-1], collapse="+"),
  sep=" ~ ")
# Standard regression
mod_el <- lm(for_mula, data=re_turns)
model_sum <- summary(mod_el)
# Bootstrap of regression
set.seed(1121)  # initialize random number generator
boot_data <- sapply(1:100, function(x) {
  boot_sample <- sample.int(n_rows, replace=TRUE)
  mod_el <- lm(for_mula,
         data=re_turns[boot_sample, ])
  mod_el$coefficients
})  # end sapply
# Means and standard errors from regression
model_sum$coefficients
# Means and standard errors from bootstrap
dim(boot_data)
t(apply(boot_data, MARGIN=1,
function(x) c(mean=mean(x), std_error=sd(x))))

# New data predictor is a data frame or row vector
set.seed(1121)
new_data <- data.frame(matrix(c(1, rnorm(5)), nr=1))
col_names <- colnames(de_sign)
colnames(new_data) <- col_names
new_datav <- as.matrix(new_data)
predic_tion <- drop(new_datav %*% beta_s)
std_dev <- drop(sqrt(
    new_datav %*% beta_covar %*% t(new_datav)))

# Create formula from text string
for_mula <- paste0("res_ponse ~ ", 
  paste(colnames(de_sign), collapse=" + "), " - 1")
# Specify multivariate regression using formula
mod_el <- lm(for_mula, 
     data=data.frame(cbind(res_ponse, de_sign)))
model_sum <- summary(mod_el)
# Predict from lm object
predict_lm <- predict.lm(object=mod_el, newdata=new_data, 
   interval="confidence", level=1-2*(1-pnorm(2)))
# Calculate t-quantile
t_quant <- qt(pnorm(2), df=deg_free)
predict_high <- (predic_tion + t_quant*std_dev)
predict_low <- (predic_tion - t_quant*std_dev)
# Compare with matrix calculations
all.equal(predict_lm[1, "fit"], predic_tion)
all.equal(predict_lm[1, "lwr"], predict_low)
all.equal(predict_lm[1, "upr"], predict_high)

# TSS = ESS + RSS
t_ss <- sum((res_ponse-mean(res_ponse))^2)
e_ss <- sum((fit_ted-mean(fit_ted))^2)
r_ss <- sum(resid_uals^2)
all.equal(t_ss, e_ss + r_ss)

# Set regression attribute for intercept
attributes(mod_el$terms)$intercept <- 1
# Regression summary
model_sum <- summary(mod_el)
# Regression R-squared
r_squared <- e_ss/t_ss
all.equal(r_squared, model_sum$r.squared)
# Correlation between response and fitted values
cor_fitted <- drop(cor(res_ponse, fit_ted))
# Squared correlation between response and fitted values
all.equal(cor_fitted^2, r_squared)

n_rows <- NROW(de_sign)
n_cols <- NCOL(de_sign)
# Degrees of freedom of residuals
deg_free <- (n_rows - n_cols)
# Adjusted R-squared
r_squared_adj <- 
  (1-sum(resid_uals^2)/deg_free/var(res_ponse))
# Compare adjusted R-squared from lm()
all.equal(drop(r_squared_adj), 
  model_sum$adj.r.squared)

par(oma=c(1, 1, 1, 1), mar=c(2, 1, 1, 1), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
lamb_da <- c(0.5, 1, 1.5)
col_ors <- c("red", "blue", "green")
# Plot three curves in loop
for (in_dex in 1:3) {
  curve(expr=plogis(x, scale=lamb_da[in_dex]),
xlim=c(-4, 4), type="l",
xlab="", ylab="", lwd=4,
col=col_ors[in_dex], add=(in_dex>1))
}  # end for

# Add title
title(main="Logistic function", line=0.5)
# Add legend
legend("topleft", title="Scale parameters",
       paste("lambda", lamb_da, sep="="),
       inset=0.05, cex=0.8, lwd=6, bty="n",
       lty=1, col=col_ors)

set.seed(1121)
# Simulate overlapping scores data
sample1 <- runif(100, max=0.6)
sample2 <- runif(100, min=0.4)
# Perform Wilcoxon test for mean
wilcox.test(sample1, sample2)
# Combine scores and add categorical variable
predic_tor <- c(sample1, sample2)
res_ponse <- c(logical(100), !logical(100))
# Perform logit regression
g_lm <- glm(res_ponse ~ predic_tor, family=binomial(logit))
class(g_lm)
summary(g_lm)

x11(width=7, height=5)
par(mar=c(3, 3, 2, 2), mgp=c(2, 1, 0), oma=c(0, 0, 0, 0))
or_der <- order(predic_tor)
plot(x=predic_tor[or_der], y=g_lm$fitted.values[or_der],
     type="l", lwd=4, col="orange",
     main="Category Densities and Logistic Function",
     xlab="score", ylab="density")
den_sity <- density(predic_tor[res_ponse])
den_sity$y <- den_sity$y/max(den_sity$y)
lines(den_sity, col="red")
polygon(c(min(den_sity$x), den_sity$x, max(den_sity$x)), c(min(den_sity$y), den_sity$y, min(den_sity$y)), col=rgb(1, 0, 0, 0.2), border=NA)
den_sity <- density(predic_tor[!res_ponse])
den_sity$y <- den_sity$y/max(den_sity$y)
lines(den_sity, col="blue")
polygon(c(min(den_sity$x), den_sity$x, max(den_sity$x)), c(min(den_sity$y), den_sity$y, min(den_sity$y)), col=rgb(0, 0, 1, 0.2), border=NA)
# Add legend
legend(x="top", cex=1.0, bty="n", lty=c(1, NA, NA),
 lwd=c(6, NA, NA), pch=c(NA, 15, 15),
 legend=c("logistic fit", "TRUE", "FALSE"),
 col=c("orange", "red", "blue"),
 text.col=c("black", "red", "blue"))

library(ISLR)  # Load package ISLR
# get documentation for package tseries
packageDescription("ISLR")  # get short description

help(package="ISLR")  # Load help page

library(ISLR)  # Load package ISLR

data(package="ISLR")  # list all datasets in ISLR

ls("package:ISLR")  # list all objects in ISLR

detach("package:ISLR")  # Remove ISLR from search path

library(ISLR)  # Load package ISLR
# Coerce the student and default columns into Boolean
Default <- ISLR::Default
Default$student <- (Default$student == "No")
Default$default <- (Default$default == "No")
colnames(Default)[1:2] <- c("no_default", "not_student")
attach(Default)
# Explore credit default data
summary(Default)
sapply(Default, class)
dim(Default); head(Default)

# Plot data points for non-defaulters
x_lim <- range(balance); y_lim <- range(income)
plot(income ~ balance,
     main="Default Dataset from Package ISLR",
     xlim=x_lim, ylim=y_lim, pch=4, col="blue",
     data=Default[no_default, ])
# Plot data points for defaulters
points(income ~ balance, pch=4, lwd=2, col="red",
 data=Default[!no_default, ])
# Add legend
legend(x="topright", bty="n",
 legend=c("non-defaulters", "defaulters"),
 col=c("blue", "red"), lty=1, lwd=6, pch=4)

# Wilcoxon test for balance predictor
wilcox.test(balance[no_default], balance[!no_default])
# Wilcoxon test for income predictor
wilcox.test(income[no_default], income[!no_default])

x11()
par(mfrow=c(1,2))  # Set plot panels
# Balance boxplot
boxplot(formula=balance ~ no_default,
  col="lightgrey",
  main="balance", xlab="No default")
# income boxplot
boxplot(formula=income ~ no_default,
  col="lightgrey",
  main="income", xlab="No default")

# Fit logistic regression model
g_lm <- glm(!no_default ~ balance,
        family=binomial(logit))
class(g_lm)
summary(g_lm)

plot(x=balance, y=!no_default,
     main="Logistic Regression of Credit Defaults", col="orange",
     xlab="credit balance", ylab="defaults")
or_der <- order(balance)
lines(x=balance[or_der], y=g_lm$fitted.values[or_der],
col="blue", lwd=3)
legend(x="topleft", inset=0.1, bty="n",
 legend=c("defaults", "logit fitted values"),
 col=c("orange", "blue"), lty=c(NA, 1), pch=c(1, NA), lwd=6)

# Calculate cumulative defaults
to_tal <- sum(!no_default)
default_s <- sapply(balance, function(lim_it) {
    sum(!no_default[balance <= lim_it])
})  # end sapply
# Perform logit regression
g_lm <- glm(
  cbind(default_s, to_tal-default_s) ~
    balance,
  family=binomial(logit))
summary(g_lm)

plot(x=balance, y=default_s/to_tal, col="orange", lwd=1,
     main="Cumulative Defaults Versus Balance",
     xlab="credit balance", ylab="cumulative defaults")
or_der <- order(balance)
lines(x=balance[or_der], y=g_lm$fitted.values[or_der],
col="blue", lwd=3)
legend(x="topleft", inset=0.1, bty="n",
 legend=c("cumulative defaults", "fitted values"),
 col=c("orange", "blue"), lty=c(NA, 1), pch=c(1, NA), lwd=6)

# Fit multifactor logistic regression model
col_names <- colnames(Default)
for_mula <- as.formula(paste(col_names[1],
  paste(col_names[-1], collapse="+"), sep=" ~ "))
g_lm <- glm(for_mula, data=Default,
        family=binomial(logit))
summary(g_lm)

# Calculate cumulative defaults
default_s <- sapply(balance, function(lim_it) {
  c(student=sum(!no_default[!not_student & (balance <= lim_it)]),
    non_student=sum(!no_default[not_student & (balance <= lim_it)]))
})  # end sapply
to_tal <- c(sum(!no_default[!not_student]), sum(!no_default[not_student]))
default_s <- t(default_s / to_tal)

# Plot cumulative defaults
par(mfrow=c(1,2))  # Set plot panels
or_der <- order(balance)
plot(x=balance[or_der], y=default_s[or_der, 1],
     col="red", t="l", lwd=2,
     main="Cumulative defaults of\n students and non-students",
     xlab="credit balance", ylab="")
lines(x=balance[or_der], y=default_s[or_der, 2],
col="blue", lwd=2)
legend(x="topleft", bty="n",
 legend=c("students", "non-students"),
 col=c("red", "blue"), text.col=c("red", "blue"),
 lwd=3)
# Balance boxplot for student factor
boxplot(formula=balance ~ not_student,
  col="lightgrey",
  main="balance", xlab="Not student")

# Fit multifactor logistic regression model
col_names <- colnames(Default)
for_mula <- as.formula(paste(col_names[1],
  paste(col_names[-1], collapse="+"), sep=" ~ "))
g_lm <- glm(for_mula, data=Default, family=binomial(logit))
# Perform forecast in-sample
forecast_s <- predict(g_lm, type="response")
all.equal(g_lm$fitted.values, forecast_s)
# Define discrimination threshold
thresh_old <- 0.95
# Calculate confusion matrix in-sample
table(no_default, (forecast_s > thresh_old))
# Fit logistic regression over training data
set.seed(1121)  # Reset random number generator
n_rows <- NROW(no_default)
da_ta <- sample.int(n=n_rows, size=n_rows/2)
train_data <- Default[da_ta, ]
g_lm <- glm(for_mula, data=train_data, family=binomial(link="logit"))
# Forecast over test data out-of-sample
test_data <- Default[-da_ta, ]
forecast_s <- predict(g_lm, newdata=test_data, type="response")

# Fit logit model and forecast in-sample
g_lm <- glm(for_mula, data=Default, family=binomial(logit))
forecast_s <- predict(g_lm, type="response")
# Calculate FALSE positive (type I error)
sum(no_default & (forecast_s < thresh_old))
# Calculate FALSE negative (type II error)
sum(!no_default & (forecast_s > thresh_old))
# Calculate confusion matrix
table(no_default, (forecast_s > thresh_old))
detach(Default)

col_names <- colnames(Default)
for_mula <- as.formula(paste(col_names[1], paste(col_names[-1], collapse="+"), sep=" ~ "))
set.seed(1121)  # Reset random number generator
n_rows <- NROW(Default)
da_ta <- sample(x=n_rows, size=n_rows/2)
train_data <- Default[da_ta, ]
g_lm <- glm(for_mula, data=train_data, family=binomial(link="logit"))
test_data <- Default[-da_ta, ]
forecast_s <- predict(g_lm, newdata=test_data, type="response")
thresh_old <- 0.95
# Calculate confusion matrix
confu_sion <- table(test_data$no_default,
              (forecast_s > thresh_old))
dimnames(confu_sion) <- list(actual=rownames(confu_sion),
  forecast=colnames(confu_sion))
confu_sion
confu_sion <- confu_sion / rowSums(confu_sion)
c(typeI=confu_sion[2, 1], typeII=confu_sion[1, 2])
# Below is an unsuccessful attempt to draw confusion matrix using xtable
confusion_matrix <- matrix(c("| true positive \\\\ (sensitivity)", "| false negative \\\\ (type II error)", "| false positive \\\\ (type I error)", "| true negative \\\\ (specificity)"), nc=2)
dimnames(confusion_matrix) <- list(forecast=c("FALSE", "TRUE"),
                             actual=c("FALSE", "TRUE"))
print(xtable::xtable(confusion_matrix,
caption="Confusion Matrix"),
caption.placement="top",
comment=FALSE, size="scriptsize",
include.rownames=TRUE,
include.colnames=TRUE)
# end unsuccessful attempt to draw confusion table using xtable

# Confusion matrix as function of thresh_old
con_fuse <- function(res_ponse, forecast_s, thresh_old) {
    confu_sion <- table(res_ponse, (forecast_s > thresh_old))
    confu_sion <- confu_sion / rowSums(confu_sion)
    c(typeI=confu_sion[2, 1], typeII=confu_sion[1, 2])
  }  # end con_fuse
con_fuse(test_data$no_default, forecast_s, thresh_old=thresh_old)
# Define vector of discrimination thresholds
threshold_s <- seq(0.01, 0.95, by=0.01)^2
# Calculate error rates
error_rates <- sapply(threshold_s, con_fuse,
  res_ponse=(test_data$no_default),
  forecast_s=forecast_s)  # end sapply
error_rates <- t(error_rates)
error_rates <- rbind(c(1, 0), error_rates)
error_rates <- rbind(error_rates, c(0, 1))
# Calculate area under ROC curve (AUC)
true_pos <- (1 - error_rates[, "typeII"])
true_pos <- (true_pos + rutils::lag_it(true_pos))/2
false_pos <- rutils::diff_it(error_rates[, "typeI"])
abs(sum(true_pos*false_pos))

# Plot ROC Curve for Defaults
plot(x=error_rates[, "typeI"],
     y=1-error_rates[, "typeII"],
     xlab="FALSE positive rate",
     ylab="TRUE positive rate",
     main="ROC Curve for Defaults",
     type="l", lwd=3, col="blue")
abline(a=0.0, b=1.0, lwd=3, col="orange")
