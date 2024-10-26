# Create a list with two elements
listv <- list(c("a", "b"), 1:4)
listv
c(typeof(listv), mode(listv), class(listv))
# Lists are also vectors
c(is.vector(listv), is.list(listv))
NROW(listv)
# Create named list
listv <- list(first=c("a", "b"), second=1:4)
listv
names(listv)
unlist(listv)

listv[2]  # Extract second element as sublist
listv[[2]]  # Extract second element
listv[[2]][3]  # Extract third element of second element
listv[[c(2, 3)]]  # Third element of second element
listv$second  # Extract second element
listv$s  # Extract second element - partial name matching
listv$second[3]  # Third element of second element
listv <- list()  # Empty list
listv$a <- 1
listv[2] <- 2
listv
names(listv)

# Convert vector elements to list elements
as.list(1:3)
# Convert whole vector to single list element
list(1:3)

dframe <- data.frame(  # Create a data frame
                type=c("rose", "daisy", "tulip"),
                color=c("red", "white", "yellow"),
                price=c(1.5, 0.5, 1.0)
              )  # end data.frame
dframe
dim(dframe)  # Get dimension attribute
colnames(dframe)  # Get the colnames attribute
rownames(dframe)  # Get the rownames attribute
class(dframe)  # Get object class
typeof(dframe)  # Data frames are listv
is.data.frame(dframe)

class(dframe$type)  # Get column class
class(dframe$price)  # Get column class

dframe[, 3]  # Extract third column as vector
dframe[[3]]  # Extract third column as vector
dframe[3]  # Extract third column as data frame
dframe[, 3, drop=FALSE]  # Extract third column as data frame
dframe[[3]][2]  # Second element from third column
dframe$price[2]  # Second element from "price" column
is.data.frame(dframe[[3]]); is.vector(dframe[[3]])
dframe[2, ]  # Extract second row
dframe[2, ][3]  # Third element from second column
dframe[2, 3]  # Third element from second column
unlist(dframe[2, ])  # Coerce to vector
is.data.frame(dframe[2, ]); is.vector(dframe[2, ])

dframe <- data.frame(  # Create a data frame
                type=c("rose", "daisy", "tulip"),
                color=c("red", "white", "yellow"),
                price=c(1.5, 0.5, 1.0),
                row.names=c("flower1", "flower2", "flower3")
              )  # end data.frame
dframe
class(dframe$type)  # Get column class
class(dframe$price)  # Get column class
# Set option to not coerce character vectors to factors - that was old default
options("stringsAsFactors")
options(stringsAsFactors=FALSE)
options("stringsAsFactors")

str(dframe)  # Display the object structure
dim(cars)  # The cars data frame has 50 rows
head(cars, n=5)  # Get first five rows
tail(cars, n=5)  # Get last five rows

# Create a named vector of student scores
scorev <- sample(round(runif(5, min=1, max=10), digits=2))
names(scorev) <- c("Angie", "Chris", "Suzie", "Matt", "Liz")
# Sort the vector into ascending order
sort(scorev)
# Calculate index to sort into ascending order
order(scorev)
# Sort the vector into ascending order
scorev[order(scorev)]
# Calculate the sorted (ordered) vector
sortv <- scorev[order(scorev)]
# Calculate index to sort into unsorted (original) order
order(order(scorev))
sortv[order(order(scorev))]
scorev
# Examples for sort() with ties
order(c(2, 1:4))  # There's a tie
order(c(2, 1:4), 1:5)  # There's a tie

# Create a vector of student ranks
rankv <- c("fifth", "fourth", "third", "second", "first")
# Reverse sort the student ranks according to students
rankv[order(order(scorev))]
# Create a data frame of students and their ranks
rosterdf <- data.frame(score=scorev, 
  rank=rankv[order(order(scorev))])
rosterdf
# Permutation index on price column
order(dframe$price)
# Sort dframe on price column
dframe[order(dframe$price), ]
# Sort dframe on color column
dframe[order(dframe$color), ]


as.matrix(dframe)
vecv <- sample(9)
matrix(vecv, ncol=3)
as.matrix(vecv, ncol=3)

matv <- matrix(5:10, nrow=2, ncol=3)  # Create a matrix
rownames(matv) <- c("row1", "row2")  # Rownames attribute
colnames(matv) <- c("col1", "col2", "col3")  # Colnames attribute
library(microbenchmark)
# Call method instead of generic function
as.data.frame.matrix(matv)
# A few methods for generic function as.data.frame()
sample(methods(as.data.frame), size=4)
# Function method is faster than generic function
summary(microbenchmark(
  as_dframem=as.data.frame.matrix(matv),
  as_dframe=as.data.frame(matv),
  dframe=data.frame(matv),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

library(microbenchmark)
# lapply is faster than coercion function
summary(microbenchmark(
  aslist=as.list(as.data.frame.matrix(matv)),
  lapply=lapply(seq_along(matv[1, ]),
     function(indeks) matv[, indeks]),
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
is.na(c(NA, NaN, 0/0, 1/0))  # Test for NA
is.nan(c(NA, NaN, 0/0, 1/0))  # Test for NaN
NA*1:4  # Create vector of Nas
# Create vector with some NA values
datav <- c(1, 2, NA, 4, NA, 5)
datav
mean(datav)  # Returns NA, when NAs are input
mean(datav, na.rm=TRUE)  # remove NAs from input data
datav[!is.na(datav)]  # Delete the NA values
sum(!is.na(datav))  # Count non-NA values

# airquality data has some NAs
head(airquality)
dim(airquality)
# Number of NA elements
sum(is.na(airquality))
# Number of rows with NA elements
sum(!complete.cases(airquality))
# Display rows containing NAs
head(airquality[!complete.cases(airquality), ])

# Create vector containing NA values
vecv <- sample(22)
vecv[sample(NROW(vecv), 4)] <- NA
# Replace NA values with the most recent non-NA values
zoo::na.locf(vecv)
# Remove rows containing NAs
goodair <- airquality[complete.cases(airquality), ]
dim(goodair)
# NAs removed
head(goodair)
# Another way of removing NAs
freshair <- na.omit(airquality)
all.equal(freshair, goodair, check.attributes=FALSE)
# Replace NAs
goodair <- zoo::na.locf(airquality)
dim(goodair)
# NAs replaced
head(goodair)

# Replace NAs in xts time series
library(rutils)  # load package rutils
pricev <- rutils::etfenv$prices[, 1]
head(pricev, 3)
sum(is.na(pricev))
pricez <- zoo::na.locf(pricev, fromLast=TRUE)
pricex <- xts:::na.locf.xts(pricev, fromLast=TRUE)
all.equal(pricez, pricex, check.attributes=FALSE)
head(pricex, 3)
library(microbenchmark)
summary(microbenchmark(
  zoo=zoo::na.locf(pricev, fromLast=TRUE),
  xts=xts:::na.locf.xts(pricev, fromLast=TRUE),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# NULL values have no mode or type
c(mode(NULL), mode(NA))
c(typeof(NULL), typeof(NA))
c(NROW(NULL), NROW(NA))
# Check for NULL values
is.null(NULL)
# NULL values are ignored when combined into a vector
c(1, 2, NULL, 4, 5)
# But NA value isn't ignored
c(1, 2, NA, 4, 5)
# Vectors can be initialized to NULL
vecv <- NULL
is.null(vecv)
# Grow the vector in a loop - very bad code!!!
for (indeks in 1:5)
  vecv <- c(vecv, indeks)
# Initialize empty vector
vecv <- numeric()
# Grow the vector in a loop - very bad code!!!
for (indeks in 1:5)
  vecv <- c(vecv, indeks)
# Allocate vector
vecv <- numeric(5)
# Assign to vector in a loop - good code
for (indeks in 1:5)
  vecv[indeks] <- runif(1)

lambdav <- c(0.5, 1, 1.5)
colorv <- c("red", "blue", "green")
# Plot three curves in loop
for (it in 1:3) {
  curve(expr=plogis(x, scale=lambdav[it]),
xlim=c(-4, 4), type="l", xlab="", ylab="", lwd=4,
col=colorv[it], add=(it>1))
}  # end for
# Add title
title(main="Logistic function", line=0.5)
# Add legend
legend("topleft", title="Scale parameters",
       paste("lambda", lambdav, sep="="), y.intersp=0.4,
       inset=0.05, cex=0.8, lwd=6, bty="n", lty=1, col=colorv)

# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
# Simulate overlapping scores data
sample1 <- runif(100, max=0.6)
sample2 <- runif(100, min=0.4)
# Perform Mann-Whitney test for data location
wilcox.test(sample1, sample2)
# Combine scores and add categorical variable
predm <- c(sample1, sample2)
respv <- c(logical(100), !logical(100))
# Perform logit regression
logmod <- glm(respv ~ predm, family=binomial(logit))
class(logmod)
summary(logmod)

ordern <- order(predm)
plot(x=predm[ordern], y=logmod$fitted.values[ordern],
     main="Category Densities and Logistic Function",
     type="l", lwd=4, col="orange", xlab="predictor", ylab="density")
densv <- density(predm[respv])
densv$y <- densv$y/max(densv$y)
lines(densv, col="red")
polygon(c(min(densv$x), densv$x, max(densv$x)), c(min(densv$y), densv$y, min(densv$y)), col=rgb(1, 0, 0, 0.2), border=NA)
densv <- density(predm[!respv])
densv$y <- densv$y/max(densv$y)
lines(densv, col="blue")
polygon(c(min(densv$x), densv$x, max(densv$x)), c(min(densv$y), densv$y, min(densv$y)), col=rgb(0, 0, 1, 0.2), border=NA)
# Add legend
legend(x="top", cex=1.0, bty="n", lty=c(1, NA, NA),
 lwd=c(6, NA, NA), pch=c(NA, 15, 15), y.intersp=0.4,
 legend=c("logistic fit", "TRUE", "FALSE"),
 col=c("orange", "red", "blue"),
 text.col=c("black", "red", "blue"))

# Likelihood function of binomial distribution
likefun <- function(prob, b) {
  b*log(prob) + (1-b)*log(1-prob)
}  # end likefun
likefun(prob=0.25, b=1)
# Plot binomial likelihood function
curve(expr=likefun(x, b=1), xlim=c(0, 1), lwd=3,
      xlab="prob", ylab="likelihood", col="blue",
      main="Binomial Likelihood Function")
curve(expr=likefun(x, b=0), lwd=3, col="red", add=TRUE)
legend(x="top", legend=c("b = 1", "b = 0"),
       title=NULL, inset=0.3, cex=1.0, lwd=6, y.intersp=0.4,
       bty="n", lty=1, col=c("blue", "red"))

# Add intercept column to the predictor matrix
predm <- cbind(intercept=rep(1, NROW(respv)), predm)
# Likelihood function of the logistic model
likefun <- function(coeff, respv, predm) {
  probs <- plogis(drop(predm %*% coeff))
  -sum(respv*log(probs) + (1-respv)*log((1-probs)))
}  # end likefun
# Run likelihood function
coeff <- c(1, 1)
likefun(coeff, respv, predm)

# Rastrigin function with vector argument for optimization
rastrigin <- function(vecv, param=25) {
  sum(vecv^2 - param*cos(vecv))
}  # end rastrigin
vecv <- c(pi/6, pi/6)
rastrigin(vecv=vecv)
# Draw 3d surface plot of Rastrigin function
options(rgl.useNULL=TRUE); library(rgl)
rgl::persp3d(
  x=Vectorize(function(x, y) rastrigin(vecv=c(x, y))),
  xlim=c(-10, 10), ylim=c(-10, 10),
  col="green", axes=FALSE, zlab="", main="rastrigin")
# Render the 3d surface plot of function
rgl::rglwidget(elementId="plot3drgl", width=400, height=400)
# Optimize with respect to vector argument
optiml <- optim(par=vecv, fn=rastrigin,
        method="L-BFGS-B",
        upper=c(4*pi, 4*pi),
        lower=c(pi/2, pi/2),
        param=1)
# Optimal parameters and value
optiml$par
optiml$value
rastrigin(optiml$par, param=1)

# Initial parameters
initp <- c(1, 1)
# Find max likelihood parameters using steepest descent optimizer
optiml <- optim(par=initp,
        fn=likefun, # Log-likelihood function
        method="L-BFGS-B", # Quasi-Newton method
        respv=respv,
        predm=predm,
        upper=c(20, 20), # Upper constraint
        lower=c(-20, -20), # Lower constraint
        hessian=TRUE)
# Optimal logistic parameters
optiml$par
unname(logmod$coefficients)
# Standard errors of parameters
sqrt(diag(solve(optiml$hessian)))
regsum <- summary(logmod)
regsum$coefficients[, 2]

library(ISLR)  # Load package ISLR
# get documentation for package tseries
packageDescription("ISLR")  # get short description

help(package="ISLR")  # Load help page

library(ISLR)  # Load package ISLR

data(package="ISLR")  # list all datasets in ISLR

ls("package:ISLR")  # list all objects in ISLR

detach("package:ISLR")  # Remove ISLR from search path

# Coerce the default and student columns to Boolean
Default <- ISLR::Default
Default$default <- (Default$default == "Yes")
Default$student <- (Default$student == "Yes")
attach(Default)  # Attach Default to search path
# Explore credit default data
summary(Default)
sapply(Default, class)
dim(Default)
head(Default)

# Plot data points for non-defaulters
xlim <- range(balance); ylim <- range(income)
plot(income ~ balance,
     main="Default Dataset from Package ISLR",
     xlim=xlim, ylim=ylim, pch=4, col="blue",
     data=Default[!default, ])
# Plot data points for defaulters
points(income ~ balance, pch=4, lwd=2, col="red",
 data=Default[default, ])
# Add legend
legend(x="topright", legend=c("non-defaulters", "defaulters"),
 y.intersp=0.4, bty="n", col=c("blue", "red"), lty=1, lwd=6, pch=4)

# Perform Mann-Whitney test for the location of the balances
wilcox.test(balance[default], balance[!default])
# Perform Mann-Whitney test for the location of the incomes
wilcox.test(income[default], income[!default])

x11(width=6, height=5)
# Set 2 plot panels
par(mfrow=c(1,2))
# Balance boxplot
boxplot(formula=balance ~ default,
  col="lightgrey", main="balance", xlab="Default")
# Income boxplot
boxplot(formula=income ~ default,
  col="lightgrey", main="income", xlab="Default")

# Fit logistic regression model
logmod <- glm(default ~ balance, family=binomial(logit))
class(logmod)
summary(logmod)

x11(width=6, height=5)
par(mar=c(4, 4, 2, 2), oma=c(0, 0, 0, 0), mgp=c(2.5, 1, 0))
plot(x=balance, y=default,
     main="Logistic Regression of Credit Defaults",
     col="orange", xlab="credit balance", ylab="defaults")
ordern <- order(balance)
lines(x=balance[ordern], y=logmod$fitted.values[ordern], col="blue", lwd=3)
legend(x="topleft", inset=0.1, bty="n", lwd=6, y.intersp=0.4,
 legend=c("defaults", "logit fitted values"),
 col=c("orange", "blue"), lty=c(NA, 1), pch=c(1, NA))

# Calculate the cumulative defaults
sumd <- sum(default)
defaultv <- sapply(balance, function(balv) {
    sum(default[balance <= balv])
})  # end sapply
# Perform logit regression
logmod <- glm(cbind(defaultv, sumd-defaultv) ~ balance,
  family=binomial(logit))
summary(logmod)

plot(x=balance, y=defaultv/sumd, col="orange", lwd=1,
     main="Cumulative Defaults Versus Balance",
     xlab="credit balance", ylab="cumulative defaults")
ordern <- order(balance)
lines(x=balance[ordern], y=logmod$fitted.values[ordern],
col="blue", lwd=3)
legend(x="topleft", inset=0.1, bty="n", y.intersp=0.4,
 legend=c("cumulative defaults", "fitted values"),
 col=c("orange", "blue"), lty=c(NA, 1), pch=c(1, NA), lwd=6)

# Fit multifactor logistic regression model
colnamev <- colnames(Default)
formulav <- as.formula(paste(colnamev[1],
  paste(colnamev[-1], collapse="+"), sep=" ~ "))
formulav
logmod <- glm(formulav, data=Default, family=binomial(logit))
summary(logmod)

# Fit single-factor logistic model with student as predictor
logmodstud <- glm(default ~ student, family=binomial(logit))
summary(logmodstud)
# Multifactor coefficient is negative
logmod$coefficients
# Single-factor coefficient is positive
logmodstud$coefficients

# Calculate the cumulative defaults
defcum <- sapply(balance, function(balv) {
c(student=sum(default[student & (balance <= balv)]),
  non_student=sum(default[!student & (balance <= balv)]))
})  # end sapply
deftotal <- c(student=sum(student & default),
      student=sum(!student & default))
defcum <- t(defcum / deftotal)
# Plot cumulative defaults
par(mfrow=c(1,2))  # Set plot panels
ordern <- order(balance)
plot(x=balance[ordern], y=defcum[ordern, 1],
     col="red", t="l", lwd=2, xlab="credit balance", ylab="",
     main="Cumulative defaults of\n students and non-students")
lines(x=balance[ordern], y=defcum[ordern, 2], col="blue", lwd=2)
legend(x="topleft", bty="n", y.intersp=0.4,
 legend=c("students", "non-students"),
 col=c("red", "blue"), text.col=c("red", "blue"), lwd=3)
# Balance boxplot for student factor
boxplot(formula=balance ~ !student,
  col="lightgrey", main="balance", xlab="Student")

# Perform in-sample forecast from logistic regression model
fcast <- predict(logmod, type="response")
all.equal(logmod$fitted.values, fcast)
# Define discrimination threshold value
threshv <- 0.7
# Calculate the confusion matrix in-sample
table(actual=!default, forecast=(fcast < threshv))
# Fit logistic regression over training data
# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
nrows <- NROW(Default)
samplev <- sample.int(n=nrows, size=nrows/2)
trainset <- Default[samplev, ]
logmod <- glm(formulav, data=trainset, family=binomial(logit))
# Forecast over test data out-of-sample
testset <- Default[-samplev, ]
fcast <- predict(logmod, newdata=testset, type="response")
# Calculate the confusion matrix out-of-sample
table(actual=!testset$default, forecast=(fcast < threshv))

# Calculate the confusion matrix out-of-sample
confmat <- table(actual=!testset$default, 
forecast=(fcast < threshv))
confmat
# Calculate the FALSE positive (type I error)
sum(!testset$default & (fcast > threshv))
# Calculate the FALSE negative (type II error)
sum(testset$default & (fcast < threshv))

# Calculate the FALSE positive and FALSE negative rates
confmat <- confmat / rowSums(confmat)
c(typeI=confmat[2, 1], typeII=confmat[1, 2])
detach(Default)
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

# Confusion matrix as function of threshold
confun <- function(actualv, fcast, threshv) {
    confmat <- table(actualv, (fcast < threshv))
    confmat <- confmat / rowSums(confmat)
    c(typeI=confmat[2, 1], typeII=confmat[1, 2])
  }  # end confun
confun(!testset$default, fcast, threshv=threshv)
# Define vector of discrimination thresholds
threshv <- seq(0.05, 0.95, by=0.05)^2
# Calculate the error rates
errorr <- sapply(threshv, confun,
  actualv=!testset$default, fcast=fcast)  # end sapply
errorr <- t(errorr)
rownames(errorr) <- threshv
errorr <- rbind(c(1, 0), errorr)
errorr <- rbind(errorr, c(0, 1))
# Calculate the area under ROC curve (AUC)
truepos <- (1 - errorr[, "typeII"])
truepos <- (truepos + rutils::lagit(truepos))/2
falsepos <- rutils::diffit(errorr[, "typeI"])
abs(sum(truepos*falsepos))

# Plot ROC Curve for Defaults
x11(width=5, height=5)
plot(x=errorr[, "typeI"], y=1-errorr[, "typeII"],
     xlab="FALSE positive rate", ylab="TRUE positive rate",
     main="ROC Curve for Defaults", type="l", lwd=3, col="blue")
abline(a=0.0, b=1.0, lwd=3, col="orange")
