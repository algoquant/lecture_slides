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

Coerce the default and student columns to Boolean
Default <- ISLR::Default
Default$default <- (Default$default == "Yes")
Default$student <- (Default$student == "Yes")
attach(Default)  # Attach Default to search path
Explore credit default data
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
# Plot the densities of the balance amounts for defaulters and non-defaulters
plot(density(balance[default]),
     main="Balance Amounts for Defaulters and Non-Defaulters",
     xlab="balance", ylab="density", col="red", lwd=2)
lines(density(balance[!default]), col="blue", lwd=2)
legend(x="topright", inset=0.0, bty="n", lwd=6, y.intersp=0.4,
 legend=c("non-defaulters", "defaulters"),
 col=c("blue", "red"))

x11(width=6, height=5)
# Set 2 plot panels
par(mfrow=c(1,2))
# Balance boxplot
boxplot(formula=balance ~ default,
  col="lightgrey", main="balance", xlab="Default")
# Income boxplot
boxplot(formula=income ~ default,
  col="lightgrey", main="income", xlab="Default")

Fit logistic regression model
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

Fit multifactor logistic regression model
colv <- colnames(Default)
formulav <- as.formula(paste(colv[1],
  paste(colv[-1], collapse="+"), sep=" ~ "))
formulav
logmod <- glm(formulav, data=Default, family=binomial(logit))
summary(logmod)

# Fit single-factor logistic model with student as predictor
studentmod <- glm(default ~ student, family=binomial(logit))
summary(studentmod)
# Multifactor coefficient is negative
logmod$coefficients
# Single-factor coefficient is positive
studentmod$coefficients

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
boxplot(formula=balance ~ student,
  col="lightgrey", main="balance", xlab="Student")

Perform in-sample forecast from logistic regression model
fcast <- predict(logmod, type="response")
all.equal(logmod$fitted.values, fcast)
Define discrimination threshold value
threshv <- 0.7
Calculate the confusion matrix in-sample
table(actual=!default, forecast=(fcast < threshv))
Fit logistic regression over training data
Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
nrows <- NROW(Default)
samplev <- sample.int(n=nrows, size=nrows/2)
trainset <- Default[samplev, ]
logmod <- glm(formulav, data=trainset, family=binomial(logit))
Forecast over test data out-of-sample
testset <- Default[-samplev, ]
fcast <- predict(logmod, newdata=testset, type="response")
Calculate the confusion matrix out-of-sample
table(actual=!testset$default, forecast=(fcast < threshv))

Calculate the confusion matrix out-of-sample
confmat <- table(actual=!testset$default, 
forecast=(fcast < threshv))
confmat
Calculate the FALSE positive (type I error)
sum(!testset$default & (fcast > threshv))
Calculate the FALSE negative (type II error)
sum(testset$default & (fcast < threshv))

Calculate the FALSE positive and FALSE negative rates
confmat <- confmat / rowSums(confmat)
c(typeI=confmat[2, 1], typeII=confmat[1, 2])
detach(Default)

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

rm(list=ls())
as.numeric(c(1:3, "a"))  # NA from coercion
0/0  # NaN from ambiguous math
1/0  # Inf from divide by zero
is.na(c(NA, NaN, 0/0, 1/0))  # Test for NA
is.nan(c(NA, NaN, 0/0, 1/0))  # Test for NaN
NA*1:4  # Create vector of Nas
Create vector with some NA values
datav <- c(1, 2, NA, 4, NA, 5)
datav
mean(datav)  # Returns NA, when NAs are input
mean(datav, na.rm=TRUE)  # remove NAs from input data
datav[!is.na(datav)]  # Delete the NA values
sum(!is.na(datav))  # Count non-NA values

airquality data has some NAs
head(airquality)
dim(airquality)
Number of NA elements
sum(is.na(airquality))
Number of rows with NA elements
sum(!complete.cases(airquality))
Display rows containing NAs
head(airquality[!complete.cases(airquality), ])

Create vector containing NA values
vecv <- sample(22)
vecv[sample(NROW(vecv), 4)] <- NA
Replace NA values with the most recent non-NA values
zoo::na.locf(vecv)
Remove rows containing NAs
goodair <- airquality[complete.cases(airquality), ]
dim(goodair)
NAs removed
head(goodair)
Another way of removing NAs
freshair <- na.omit(airquality)
all.equal(freshair, goodair, check.attributes=FALSE)
Replace NAs
goodair <- zoo::na.locf(airquality)
dim(goodair)
NAs replaced
head(goodair)

Replace NAs in xts time series
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

NULL values have no mode or type
c(mode(NULL), mode(NA))
c(typeof(NULL), typeof(NA))
c(NROW(NULL), NROW(NA))
Check for NULL values
is.null(NULL)
NULL values are ignored when combined into a vector
c(1, 2, NULL, 4, 5)
c(1, 2, NA, 4, 5)
Vectors can be initialized to NULL
vecv <- NULL
is.null(vecv)
Grow the vector in a loop - very bad code!!!
for (indeks in 1:5)
  vecv <- c(vecv, indeks)
Initialize empty vector
vecv <- numeric()
Grow the vector in a loop - very bad code!!!
for (indeks in 1:5)
  vecv <- c(vecv, indeks)
Allocate vector
vecv <- numeric(5)
Assign to vector in a loop - good code
for (indeks in 1:5)
  vecv[indeks] <- runif(1)

# Load and plot intraday stock prices
load("/Users/jerzy/Develop/lecture_slides/data/xlk_tick_trades_20200316.RData")
pricev <- xlk$price
dygraphs::dygraph(pricev, main="XLK Intraday Prices for 2020-03-16") %>%
  dyOptions(colors="blue", strokeWidth=1)
# Calculate the lagged and advanced prices
pricelag <- rutils::lagit(pricev)
pricelag[1] <- pricelag[2]
pricadv <- rutils::lagit(pricev, lagg=-1)
pricadv[NROW(pricadv)] <- pricadv[NROW(pricadv)-1]
# Calculate the z-scores
diffl <- ifelse(abs(pricelag-pricadv) < 0.01, 0.01, abs(pricelag-pricadv))
zscores <- (pricev - 0.5*(pricelag+pricadv))/diffl

# Z-scores have very fat tails
range(zscores); mad(zscores)
madz <- mad(zscores[abs(zscores) > 0])
hist(zscores, breaks=5000, xlim=c(-2*madz, 2*madz))
# Scrub the price spikes
threshv <- 5*madz # Discrimination threshold
indeks <- which(abs(zscores) > threshv)
pricev[indeks] <- as.numeric(pricev[indeks-1])
# Plot dygraph of the scrubbed prices
dygraphs::dygraph(pricev, main="Scrubbed XLK Intraday Prices") %>%
  dyOptions(colors="blue", strokeWidth=1)

# Calculate the centered Hampel filter to remove bad prices
lookb <- 71 # Look-back interval
halfb <- lookb %/% 2 # Half-back interval
pricev <- xlk$price
# Calculate the trailing median and MAD
medianv <- HighFreq::roll_mean(pricev, lookb=lookb, method="nonparametric")
colnames(medianv) <- c("median")
madv <- HighFreq::roll_var(pricev, lookb=lookb, method="nonparametric")
# madv <- TTR::runMAD(pricev, n=lookb)
# Center the median and the MAD
medianv <- rutils::lagit(medianv, lagg=(-halfb), pad_zeros=FALSE)
madv <- rutils::lagit(madv, lagg=(-halfb), pad_zeros=FALSE)
# Calculate the Z-scores
zscores <- ifelse(madv > 0, (pricev - medianv)/madv, 0)
# Z-scores have very fat tails
range(zscores); mad(zscores)
madz <- mad(zscores[abs(zscores) > 0])
hist(zscores, breaks=5000, xlim=c(-2*madz, 2*madz))

# Define discrimination threshold value
threshv <- 6*madz
# Identify good prices with small z-scores
isgood <- (abs(zscores) < threshv)
# Calculate the number of bad prices
sum(!isgood)
# Overwrite bad prices and calculate time series of scrubbed prices
priceg <- pricev
priceg[!isgood] <- NA
priceg <- zoo::na.locf(priceg)
# Plot dygraph of the scrubbed prices
dygraphs::dygraph(priceg, main="Scrubbed XLK Intraday Prices") %>%
  dyOptions(colors="blue", strokeWidth=1)
# Plot using chart_Series()
x11(width=6, height=5)
quantmod::chart_Series(x=priceg,
  name="Clean XLK Intraday Prices for 2020-03-16")

# Add 200 random price spikes to the clean prices
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
nspikes <- 200
nrows <- NROW(priceg)
ispike <- logical(nrows)
ispike[sample(x=nrows, size=nspikes)] <- TRUE
priceb <- priceg
priceb[ispike] <- priceb[ispike]*
  sample(c(0.999, 1.001), size=nspikes, replace=TRUE)
# Plot the bad prices and their medians
medianv <- HighFreq::roll_mean(priceb, lookb=lookb, method="nonparametric")
pricem <- cbind(priceb, medianv)
colnames(pricem) <- c("prices with spikes", "median")
dygraphs::dygraph(pricem, main="XLK Prices With Spikes") %>%
  dyOptions(colors=c("red", "blue"))
# Calculate the z-scores
madv <- HighFreq::roll_var(priceb, lookb=lookb, method="nonparametric")
zscores <- ifelse(madv > 0, (priceb - medianv)/madv, 0)
# Z-scores have very fat tails
range(zscores); mad(zscores)
madz <- mad(zscores[abs(zscores) > 0])
hist(zscores, breaks=10000, xlim=c(-4*madz, 4*madz))
# Identify good prices with small z-scores
threshv <- 3*madz
isgood <- (abs(zscores) < threshv)
# Calculate the number of bad prices
sum(!isgood)

# Calculate the confusion matrix
table(actual=!ispike, forecast=isgood)
sum(!isgood)
# FALSE positive (type I error)
sum(!ispike & !isgood)
# FALSE negative (type II error)
sum(ispike & isgood)

# Confusion matrix as function of threshold
confun <- function(actualv, zscores, threshv) {
    confmat <- table(actualv, (abs(zscores) < threshv))
    confmat <- confmat / rowSums(confmat)
    c(typeI=confmat[2, 1], typeII=confmat[1, 2])
}  # end confun
confun(!ispike, zscores, threshv=threshv)
# Define vector of discrimination thresholds
threshv <- madz*seq(from=0.1, to=3.0, by=0.05)/2
# Calculate the error rates
errorr <- sapply(threshv, confun, actualv=!ispike, zscores=zscores)
errorr <- t(errorr)
rownames(errorr) <- threshv
errorr <- rbind(c(1, 0), errorr)
errorr <- rbind(errorr, c(0, 1))
# Calculate the area under the ROC curve (AUC)
truepos <- (1 - errorr[, "typeII"])
truepos <- (truepos + rutils::lagit(truepos))/2
falsepos <- rutils::diffit(errorr[, "typeI"])
abs(sum(truepos*falsepos))

# Plot ROC curve for Hampel classifier
plot(x=errorr[, "typeI"], y=1-errorr[, "typeII"],
     xlab="FALSE positive rate", ylab="TRUE positive rate",
     xlim=c(0, 1), ylim=c(0, 1),
     main="ROC Curve for Hampel Classifier",
     type="l", lwd=3, col="blue")
abline(a=0.0, b=1.0, lwd=3, col="orange")

# Load log VXX prices
load("/Users/jerzy/Develop/lecture_slides/data/pricevxx.RData")
nrows <- NROW(pricev)
# Calculate the centered Hampel filter for VXX
lookb <- 7 # Look-back interval
halfb <- lookb %/% 2 # Half-back interval
medianv <- HighFreq::roll_mean(pricev, lookb=lookb, method="nonparametric")
medianv <- rutils::lagit(medianv, lagg=(-halfb), pad_zeros=FALSE)
madv <- HighFreq::roll_var(pricev, lookb=lookb, method="nonparametric")
madv <- rutils::lagit(madv, lagg=(-halfb), pad_zeros=FALSE)
zscores <- ifelse(madv > 0, (pricev - medianv)/madv, 0)
range(zscores); mad(zscores)
madz <- mad(zscores[abs(zscores) > 0])
hist(zscores, breaks=100, xlim=c(-3*madz, 3*madz))
# Define discrimination threshold value
threshv <- 9*madz
# Calculate the good prices
isgood <- (abs(zscores) < threshv)
sum(!isgood)
# Dates of the bad prices
zoo::index(pricev[!isgood])

# Calculate the false positives
falsep <- !isgood
falsep[which(zoo::index(pricev) == as.Date("2010-11-08"))] <- FALSE
# Plot dygraph of the prices with bad prices
datam <- cbind(pricev, zscores)
colnames(datam)[2] <- "ZScores"
colv <- colnames(datam)
dygraphs::dygraph(datam, main="VXX Prices With Z-Scores and False Positives") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", strokeWidth=1, col="blue") %>%
  dySeries(name=colv[2], axis="y2", strokeWidth=1, col="red") %>%
  dyEvent(zoo::index(pricev[falsep]), label=rep("false", sum(falsep)), strokePattern="solid", color="red") %>%
  dyEvent(zoo::index(pricev["2010-11-08"]), label="true", strokePattern="solid", color="green")

# Replace bad stock prices with the previous good prices
priceg <- pricev
priceg[!isgood] <- NA
priceg <- zoo::na.locf(priceg)
# Calculate the Z-scores
medianv <- HighFreq::roll_mean(priceg, lookb=lookb, method="nonparametric")
medianv <- rutils::lagit(medianv, lagg=(-halfb), pad_zeros=FALSE)
madv <- HighFreq::roll_var(priceg, lookb=lookb, method="nonparametric")
madv <- rutils::lagit(madv, lagg=(-halfb), pad_zeros=FALSE)
zscores <- ifelse(madv > 0, (priceg - medianv)/madv, 0)
madz <- mad(zscores[abs(zscores) > 0])
# Calculate the number of bad prices
threshv <- 9*madz
isgood <- (abs(zscores) < threshv)
sum(!isgood)
zoo::index(priceg[!isgood])

# Calculate the false positives
falsep <- !isgood
falsep[which(zoo::index(pricev) == as.Date("2010-11-08"))] <- FALSE
# Plot dygraph of the prices with bad prices
dygraphs::dygraph(priceg, main="Scrubbed VXX Prices With False Positives") %>%
  dyEvent(zoo::index(priceg[falsep]), label=rep("false", sum(falsep)), strokePattern="solid", color="red") %>%
  dyOptions(colors="blue", strokeWidth=1)

# Add 200 random price spikes to the clean prices
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
nspikes <- 200
ispike <- logical(nrows)
ispike[sample(x=nrows, size=nspikes)] <- TRUE
priceb <- priceg
priceb[ispike] <- priceb[ispike]*
  sample(c(0.99, 1.01), size=nspikes, replace=TRUE)
# Calculate the Z-scores
medianv <- HighFreq::roll_mean(priceb, lookb=lookb, method="nonparametric")
medianv <- rutils::lagit(medianv, lagg=(-halfb), pad_zeros=FALSE)
madv <- HighFreq::roll_var(priceb, lookb=lookb, method="nonparametric")
madv <- rutils::lagit(madv, lagg=(-halfb), pad_zeros=FALSE)
zscores <- ifelse(madv > 0, (priceb - medianv)/madv, 0)
madz <- mad(zscores[abs(zscores) > 0])
# Define vector of discrimination thresholds
threshv <- madz*seq(from=0.1, to=3.0, by=0.05)/2
# Calculate the error rates
errorr <- sapply(threshv, confun, actualv=!ispike, zscores=zscores)
errorr <- t(errorr)
rownames(errorr) <- threshv
errorr <- rbind(c(1, 0), errorr)
errorr <- rbind(errorr, c(0, 1))

# Calculate the area under the ROC curve (AUC)
truepos <- (1 - errorr[, "typeII"])
truepos <- (truepos + rutils::lagit(truepos))/2
falsepos <- rutils::diffit(errorr[, "typeI"])
abs(sum(truepos*falsepos))
# Plot ROC curve for Hampel classifier
plot(x=errorr[, "typeI"], y=1-errorr[, "typeII"],
     xlab="FALSE positive rate", ylab="TRUE positive rate",
     xlim=c(0, 1), ylim=c(0, 1),
     main="ROC Curve for Daily Hampel Classifier",
     type="l", lwd=3, col="blue")
abline(a=0.0, b=1.0, lwd=3, col="orange")

# Simulate a Brownian motion path
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
pathv <- cumsum(rnorm(nrows))
plot(pathv, type="l", xlab="time", ylab="path",
     main="Brownian Motion")

# Plot the density of Brownian Motion
curve(expr=dnorm(x), xlim=c(-4, 4), ylim=c(0, 0.9),
  xlab="B_T", ylab="density", lwd=2, col="blue")
# Plot the density of the maximum of Brownian Motion
curve(expr=2*dnorm(x), xlim=c(0, 4), xlab="", ylab="",
  lwd=2, col="red", add=TRUE)
lines(x=c(0, 0), y=c(0, sqrt(2/pi)), lwd=2, col="red")
lines(x=c(-4, 0), y=c(0, 0), lwd=2, col="red")
title(main="Probability Density of
The Maximum Value of Brownian Motion", line=0.5)
legend("topright", inset=0.0, bty="n", y.intersp=0.4,
 title=NULL, c("Brownian", "Max"), lwd=6,
 col=c("blue", "red"))

# Series element
fun1 <- function(n, r) { 2*sin((n-0.5)*pi)/((n-0.5)*pi) *
  (1-exp(-((n-0.5)^2)*pi^2/2/r^2)) }
# fun2 <- function(x) { sum(sapply(1:10, function(n) fun1(n, x))) }
# fun2 <- function(x) { fun1(1, x) + fun1(2, x) + fun1(3, x) + fun1(4, x) + fun1(5, x) + fun1(6, x) }
# Sum of fun1
fun2 <- function(x) {
  valf <- 0
  for (n in 1:20) { valf <- valf + fun1(n, x) }
  return(valf)
  } # end fun2
# Theoretical average value of the range
fun2(2)
# Average value of the range from integration (not quite close)
integrate(fun2, lower=0.01, upper=4)

# Plot the density of Brownian Motion
curve(expr=dnorm(x), xlim=c(-4, 4), ylim=c(0, 1.0),
  xlab="B_T", ylab="density", lwd=2, col="blue")
# Plot the density of the range of Brownian Motion
curve(expr=fun2(x), xlim=c(0, 4), xlab="", ylab="",
  lwd=2, col="red", add=TRUE)
lines(x=c(0, 0), y=c(0, fun2(0.01)), lwd=2, col="red")
lines(x=c(-4, 0), y=c(0, 0), lwd=2, col="red")
title(main="Probability Density of
The Range of Brownian Motion", line=0.5)
legend("topright", inset=0.0, bty="n", y.intersp=0.7,
 title=NULL, c("Brownian", "Range"), lwd=6,
 col=c("blue", "red"))

# Define the daily volatility and growth rate
sigmav <- 0.01; drift <- 0.0; nrows <- 1000
# Simulate geometric Brownian motion
retp <- sigmav*rnorm(nrows) + drift - sigmav^2/2
pricev <- exp(cumsum(retp))
plot(pricev, type="l", xlab="time", ylab="prices",
     main="Geometric Brownian Motion")

# Simulate geometric Brownian motion
sigmav <- 0.01/sqrt(48)
drift <- 0.0
nrows <- 1e4
datev <- seq(from=as.POSIXct(paste(Sys.Date()-250, "09:30:00")),
  length.out=nrows, by="30 min")
pricev <- exp(cumsum(sigmav*rnorm(nrows) + drift - sigmav^2/2))
pricev <- xts(pricev, order.by=datev)
pricev <- cbind(pricev,
  volume=sample(x=10*(2:18), size=nrows, replace=TRUE))
# Aggregate to daily OHLC data
ohlc <- xts::to.daily(pricev)
quantmod::chart_Series(ohlc, name="random prices")
# dygraphs candlestick plot using pipes syntax
library(dygraphs)
dygraphs::dygraph(ohlc[, 1:4]) %>% dyCandlestick()
# dygraphs candlestick plot without using pipes syntax
dygraphs::dyCandlestick(dygraphs::dygraph(ohlc[, 1:4]))

# Standard deviations of log-normal distribution
sigmavs <- c(0.5, 1, 1.5)
# Create plot colors
colorv <- c("black", "red", "blue")
# Plot all curves
for (indeks in 1:NROW(sigmavs)) {
  curve(expr=dlnorm(x, sdlog=sigmavs[indeks]),
  type="l", lwd=2, xlim=c(0, 3),
  xlab="", ylab="", col=colorv[indeks],
  add=as.logical(indeks-1))
}  # end for

# Add title and legend
title(main="Log-normal Distributions", line=0.5)
legend("topright", inset=0.05, title="Sigmas",
 paste("sigma", sigmavs, sep="="),
 cex=0.8, lwd=2, lty=rep(1, NROW(sigmavs)),
 col=colorv)

x11(width=6, height=4)
par(mar=c(4, 4, 3, 1))
# Return volatility of VTI etf
sigmav <- sd(rutils::diffit(log(rutils::etfenv$VTI[, 4])))
sigma2 <- sigmav^2
nrows <- NROW(rutils::etfenv$VTI)
# Standard deviation of log-normal prices
sqrt(nrows)*sigmav

# Skewness of log-normal prices
calcskew <- function(t) {
  expv <- exp(t*sigma2)
  (expv + 2)*sqrt(expv - 1)
}  # end calcskew
curve(expr=calcskew, xlim=c(1, nrows), lwd=3,
xlab="Number of days", ylab="Skewness", col="blue",
main="Skewness of Log-normal Prices
as a Function of Time")

# Probability that random log-normal price will be lower than the mean price
curve(expr=pnorm(sigmav*sqrt(x)/2),
xlim=c(1, nrows), lwd=3,
xlab="Number of days", ylab="Probability", col="blue",
main="Probability That Random Log-normal Price
Will be Lower Than the Mean Price")

# Define the daily volatility and growth rate
sigmav <- 0.01; drift <- 0.0; nrows <- 5000
npaths <- 10
# Simulate multiple paths of geometric Brownian motion
pricev <- rnorm(npaths*nrows, sd=sigmav) + drift - sigmav^2/2
pricev <- matrix(pricev, nc=npaths)
pricev <- exp(matrixStats::colCumsums(pricev))
# Create xts time series
pricev <- xts(pricev, order.by=seq.Date(Sys.Date()-nrows+1, Sys.Date(), by=1))
# Sort the columns according to largest terminal values
pricev <- pricev[, order(pricev[nrows, ])]
# Plot xts time series
colorv <- colorRampPalette(c("red", "blue"))(NCOL(pricev))
par(mar=c(3, 3, 2, 2), oma=c(0, 0, 0, 0))
plot.zoo(pricev, main="Multiple paths of geometric Brownian motion",
   xlab=NA, ylab=NA, plot.type="single", col=colorv)

# Define the daily volatility and growth rate
sigmav <- 0.01; drift <- 0.0; nrows <- 10000
npaths <- 100
# Simulate multiple paths of geometric Brownian motion
pricev <- rnorm(npaths*nrows, sd=sigmav) + drift - sigmav^2/2
pricev <- matrix(pricev, nc=npaths)
pricev <- exp(matrixStats::colCumsums(pricev))
# Calculate fraction of paths below the expected value
fractv <- rowSums(pricev < 1.0) / npaths
# Create xts time series of percentage of paths below the expected value
fractv <- xts(fractv, order.by=seq.Date(Sys.Date()-NROW(fractv)+1, Sys.Date(), by=1))
# Plot xts time series of percentage of paths below the expected value
par(mar=c(3, 3, 2, 2), oma=c(0, 0, 0, 0))
plot.zoo(fractv, main="Percentage of GBM paths below mean",
   xlab=NA, ylab=NA, col="blue")

# Load S&P500 stock prices
load("/Users/jerzy/Develop/lecture_slides/data/sp500.RData")
ls(sp500env)
# Extract the closing prices
pricev <- eapply(sp500env, quantmod::Cl)
# Flatten the prices into a single xts series
pricev <- rutils::do_call(cbind, pricev)
# Carry forward and backward non-NA prices
pricev <- zoo::na.locf(pricev, na.rm=FALSE)
pricev <- zoo::na.locf(pricev, fromLast=TRUE)
sum(is.na(pricev))
# Drop ".Close" from column names
colnames(pricev)
colnames(pricev) <- rutils::get_name(colnames(pricev))
# Or
# colnames(pricev) <- do.call(rbind,
#   strsplit(colnames(pricev), split="[.]"))[, 1]
# Select prices after the year 2000
pricev <- pricev["2000/", ]
# Scale the columns so that prices start at 1
pricev <- lapply(pricev, function(x) x/as.numeric(x[1]))
pricev <- rutils::do_call(cbind, pricev)
# Sort the columns according to the final prices
nrows <- NROW(pricev)
ordern <- order(pricev[nrows, ])
pricev <- pricev[, ordern]
# Select 20 symbols
symbolv <- colnames(pricev)
symbolv <- symbolv[round(seq.int(from=1, to=NROW(symbolv), length.out=20))]

# Plot xts time series of prices
colorv <- colorRampPalette(c("red", "blue"))(NROW(symbolv))
endd <- rutils::calc_endpoints(pricev, interval="weeks")
plot.zoo(pricev[endd, symbolv], main="20 S&P500 Stock Prices (scaled)",
   xlab=NA, ylab=NA, plot.type="single", col=colorv)
legend(x="topleft", inset=0.02, cex=0.5, bty="n", y.intersp=0.5,
 legend=rev(symbolv), col=rev(colorv), lwd=6, lty=1)

# Calculate the final stock prices
pricef <- drop(zoo::coredata(pricev[nrows, ]))
# Calculate the mean and median stock prices
max(pricef); min(pricef)
which.max(pricef)
which.min(pricef)
mean(pricef)
median(pricef)
# Calculate the percentage of stock prices below the mean
sum(pricef < mean(pricef))/NROW(pricef)

# Plot a histogram of final stock prices
hist(pricef, breaks=1e3, xlim=c(0, 300),
     xlab="Stock price", ylab="Count",
     main="Histogram of Final Stock Prices")
# Plot a histogram of final stock prices
abline(v=median(pricef), lwd=3, col="blue")
text(x=median(pricef), y=150, lab="median", pos=4)
abline(v=mean(pricef), lwd=3, col="red")
text(x=mean(pricef), y=100, lab="mean", pos=4)

# Calculate average of valid stock prices
validp <- (pricev != 1)  # Valid stocks
nstocks <- rowSums(validp)
nstocks[1] <- NCOL(pricev)
indeks <- rowSums(pricev*validp)/nstocks
# Calculate fraction of stock prices below the average price
fractv <- rowSums((pricev < indeks) & validp)/nstocks
# Create xts time series of average stock prices
indeks <- xts(indeks, order.by=zoo::index(pricev))

dev.new(width=6, height=4, noRStudioGD=TRUE)
# x11(width=6, height=4)
# Plot xts time series of average stock prices
plot.zoo(indeks, main="Average S&P500 Stock Prices (normalized from 1990)",
   xlab=NA, ylab=NA, col="blue")
# Create xts time series of percentage of stock prices below the average price
fractv <- xts(fractv, order.by=zoo::index(pricev))
# Plot percentage of stock prices below the average price
plot.zoo(fractv[-(1:2),],
   main="Percentage of S&P500 Stock Prices
   Below the Average Price",
   xlab=NA, ylab=NA, col="blue")

library(rutils)  # Load package rutils
# Calculate VTI percentage returns
retp <- rutils::etfenv$returns$VTI
retp <- drop(coredata(na.omit(retp)))
nrows <- NROW(retp)
# Mean and standard deviation of returns
c(mean(retp), sd(retp))
# Calculate the smoothing bandwidth as the MAD of returns 10 points apart
retp <- sort(retp)
bwidth <- 10*mad(rutils::diffit(retp, lagg=10))
# Calculate the kernel density using a loop
dens1 <- sapply(1:nrows, function(it) {
  sum(dnorm(retp-retp[it], sd=bwidth))
})/nrows  # end sapply

# Plot the kernel density
madv <- mad(retp)
plot(retp, dens1, xlim=c(-5*madv, 5*madv),
     t="l", col="blue", lwd=3,
     xlab="returns", ylab="density",
     main="Density of VTI Returns")

# Calculate the kernel density using density()
densv <- density(retp, bw=bwidth)
NROW(densv$y)
plot(densv, xlim=c(-5*madv, 5*madv),
     xlab="returns", ylab="density",
     col="blue", lwd=3, main="Density of VTI Returns")
# Interpolate the densv vector into returns
densv <- approx(densv$x, densv$y, xout=retp)
all.equal(densv$x, retp)

# Plot the two density estimates
plot(retp, dens1, xlim=c(-5*madv, 5*madv),
     xlab="returns", ylab="density",
     t="l", col="blue", lwd=1,
     main="Density of VTI Returns")
lines(retp, densv$y, col="red")
# Add legend
legend("topright", inset=0.05, cex=0.8, title=NULL,
 leg=c("density", "densfun"), bty="n", y.intersp=0.4,
 lwd=6, bg="white", col=c("blue", "red"))

# Plot histogram
histp <- hist(retp, breaks=100, freq=FALSE,
  xlim=c(-5*madv, 5*madv), xlab="", ylab="",
  main="VTI Return Distribution")
# Draw kernel density of histogram
lines(densv, col="red", lwd=2)
# Add density of normal distribution
curve(expr=dnorm(x, mean=mean(retp), sd=sd(retp)),
add=TRUE, lwd=2, col="blue")
# Add legend
legend("topright", inset=0.05, cex=0.8, title=NULL,
 leg=c("VTI", "Normal"), bty="n", y.intersp=0.4,
 lwd=6, bg="white", col=c("red", "blue"))

# Create normal Q-Q plot
qqnorm(retp, ylim=c(-0.1, 0.1), main="VTI Q-Q Plot",
 xlab="Normal Quantiles")
# Fit a line to the normal quantiles
qqline(retp, col="red", lwd=2)
# Perform Shapiro-Wilk test
shapiro.test(retp)

# Boxplot method for formula
boxplot(formula=mpg ~ cyl, data=mtcars,
  main="Mileage by number of cylinders",
  xlab="Cylinders", ylab="Miles per gallon")
# Boxplot method for data frame of EuStockMarkets percentage returns
boxplot(x=diff(log(EuStockMarkets)))

# Calculate VTI percentage returns
retp <- na.omit(rutils::etfenv$returns$VTI)
# Number of observations
nrows <- NROW(retp)
# Mean of VTI returns
retm <- mean(retp)
# Standard deviation of VTI returns
stdev <- sd(retp)
# Skewness of VTI returns
nrows/((nrows-1)*(nrows-2))*sum(((retp - retm)/stdev)^3)
# Kurtosis of VTI returns
nrows*(nrows+1)/((nrows-1)^3)*sum(((retp - retm)/stdev)^4)
# Random normal returns
retp <- rnorm(nrows, sd=stdev)
# Mean and standard deviation of random normal returns
retm <- mean(retp)
stdev <- sd(retp)
# Skewness of random normal returns
nrows/((nrows-1)*(nrows-2))*sum(((retp - retm)/stdev)^3)
# Kurtosis of random normal returns
nrows*(nrows+1)/((nrows-1)^3)*sum(((retp - retm)/stdev)^4)

# calc_skew() calculates skew of returns
calc_skew <- function(retp) {
  retp <- na.omit(retp)
  sum(((retp - mean(retp))/sd(retp))^3)/NROW(retp)
}  # end calc_skew
# calc_kurt() calculates kurtosis of returns
calc_kurt <- function(retp) {
  retp <- na.omit(retp)
  sum(((retp - mean(retp))/sd(retp))^4)/NROW(retp)
}  # end calc_kurt
# Calculate skew and kurtosis of VTI returns
calc_skew(retp)
calc_kurt(retp)
# calc_mom() calculates the moments of returns
calc_mom <- function(retp, moment=3) {
  retp <- na.omit(retp)
  sum(((retp - mean(retp))/sd(retp))^moment)/NROW(retp)
}  # end calc_mom
# Calculate skew and kurtosis of VTI returns
calc_mom(retp, moment=3)
calc_mom(retp, moment=4)

# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
# Sample from Standard Normal Distribution
nrows <- 1000
datav <- rnorm(nrows)
# Sample mean
mean(datav)
# Sample standard deviation
sd(datav)
# Standard error of sample mean
sd(datav)/sqrt(nrows)

xvar <- seq(-5, 7, length=100)
yvar <- dnorm(xvar, mean=1.0, sd=2.0)
plot(xvar, yvar, type="l", lty="solid", xlab="", ylab="")
title(main="Normal Density Function", line=0.5)
startp <- 3; endd <- 5  # Set lower and upper bounds
# Set polygon base
subv <- ((xvar >= startp) & (xvar <= endd))
polygon(c(startp, xvar[subv], endd),  # Draw polygon
  c(-1, yvar[subv], -1), col="red")

par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
sigmavs <- c(0.5, 1, 1.5, 2)  # Sigma values
# Create plot colors
colorv <- c("red", "black", "blue", "green")
# Create legend labels
labelv <- paste("sigma", sigmavs, sep="=")
for (it in 1:4) {  # Plot four curves
  curve(expr=dnorm(x, sd=sigmavs[it]),
  xlim=c(-4, 4), xlab="", ylab="", lwd=2,
  col=colorv[it], add=as.logical(it-1))
}  # end for
# Add title
title(main="Normal Distributions", line=0.5)
# Add legend
legend("topright", inset=0.05, title="Sigmas", y.intersp=0.4,
 labelv, cex=0.8, lwd=2, lty=1, bty="n", col=colorv)

x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
degf <- c(3, 6, 9)  # Df values
colorv <- c("black", "red", "blue", "green")
labelv <- c("normal", paste("df", degf, sep="="))
# Plot a Normal probability distribution
curve(expr=dnorm, xlim=c(-4, 4), xlab="", ylab="", lwd=2)
for (it in 1:3) {  # Plot three t-distributions
  curve(expr=dt(x, df=degf[it]), xlab="", ylab="",
lwd=2, col=colorv[it+1], add=TRUE)
}  # end for

# Add title
title(main="t-distributions", line=0.5)
# Add legend
legend("topright", inset=0.05, bty="n", y.intersp=0.4,
       title="Degrees\n of freedom", labelv,
       cex=0.8, lwd=6, lty=1, col=colorv)

# Mixture of two normal distributions with sd=1 and sd=2
nrows <- 1e5
retp <- c(rnorm(nrows/2), 2*rnorm(nrows/2))
retp <- (retp-mean(retp))/sd(retp)
# Kurtosis of normal
calc_kurt(rnorm(nrows))
# Kurtosis of mixture
calc_kurt(retp)
# Or
nrows*sum(retp^4)/(nrows-1)^2

x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
# Plot the distributions
plot(density(retp), xlab="", ylab="",
  main="Mixture of Normal Returns",
  xlim=c(-3, 3), type="l", lwd=3, col="red")
curve(expr=dnorm, lwd=2, col="blue", add=TRUE)
curve(expr=dt(x, df=3), lwd=2, col="green", add=TRUE)
# Add legend
legend("topright", inset=0.05, lty=1, lwd=6, bty="n",
  legend=c("Mixture", "Normal", "t-distribution"), y.intersp=0.4,
  col=c("red", "blue", "green"))

dev.new(width=6, height=5, noRStudioGD=TRUE)
# x11(width=6, height=5)
# Define density of non-standard t-distribution
tdistr <- function(x, dfree, locv=0, scalev=1) {
  dt((x-locv)/scalev, df=dfree)/scalev
}  # end tdistr
# Or
tdistr <- function(x, dfree, locv=0, scalev=1) {
  gamma((dfree+1)/2)/(sqrt(pi*dfree)*gamma(dfree/2)*scalev)*
    (1+((x-locv)/scalev)^2/dfree)^(-(dfree+1)/2)
}  # end tdistr
# Calculate vector of scale values
scalev <- c(0.5, 1.0, 2.0)
colorv <- c("blue", "black", "red")
labelv <- paste("scale", format(scalev, digits=2), sep="=")
# Plot three t-distributions
for (it in 1:3) {
  curve(expr=tdistr(x, dfree=3, scalev=scalev[it]), xlim=c(-3, 3),
xlab="", ylab="", lwd=2, col=colorv[it], add=(it>1))
}  # end for

# Add title
title(main="t-distributions with Different Scale Parameters", line=0.5)
# Add legend
legend("topright", inset=0.05, bty="n", title="Scale Parameters", labelv,
       cex=0.8, lwd=6, lty=1, col=colorv, y.intersp=0.4)

Calculate VTI percentage returns
library(rutils)
retp <- as.numeric(na.omit(rutils::etfenv$returns$VTI))[1:499]
Reduce number of output digits
ndigits <- options(digits=5)
Shapiro-Wilk test for normal distribution
nrows <- NROW(retp)
shapiro.test(rnorm(nrows))
Shapiro-Wilk test for VTI returns
shapiro.test(retp)
Shapiro-Wilk test for uniform distribution
shapiro.test(runif(nrows))
Restore output digits
options(digits=ndigits$digits)

library(tseries)  # Load package tseries
Jarque-Bera test for normal distribution
jarque.bera.test(rnorm(nrows))
Jarque-Bera test for VTI returns
jarque.bera.test(retp)
Jarque-Bera test for uniform distribution
jarque.bera.test(runif(NROW(retp)))

# KS test for normal distribution
kstest <- ks.test(rnorm(100), pnorm)
kstest$p.value
# KS test for uniform distribution
ks.test(runif(100), pnorm)
# KS test for two shifted normal distributions
ks.test(rnorm(100), rnorm(100, mean=0.1))
ks.test(rnorm(100), rnorm(100, mean=1.0))
# KS test for two different normal distributions
ks.test(rnorm(100), rnorm(100, sd=2.0))
# KS test for VTI returns vs normal distribution
retp <- as.numeric(na.omit(rutils::etfenv$returns$VTI))
retp <- (retp - mean(retp))/sd(retp)
ks.test(retp, pnorm)

x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
# Degrees of freedom
degf <- c(2, 5, 8, 11)
# Plot four curves in loop
colorv <- c("red", "black", "blue", "green")
for (it in 1:4) {
  curve(expr=dchisq(x, df=degf[it]),
  xlim=c(0, 20), ylim=c(0, 0.3),
  xlab="", ylab="", col=colorv[it],
  lwd=2, add=as.logical(it-1))
}  # end for

# Add title
title(main="Chi-squared Distributions", line=0.5)
# Add legend
labelv <- paste("df", degf, sep="=")
legend("topright", inset=0.05, bty="n", y.intersp=0.4,
       title="Degrees of freedom", labelv,
       cex=0.8, lwd=6, lty=1, col=colorv)

# Observed frequencies from random normal data
histp <- hist(rnorm(1e3, mean=0), breaks=100, plot=FALSE)
countsn <- histp$counts
# Theoretical frequencies
countst <- rutils::diffit(pnorm(histp$breaks))
# Perform Chi-squared test for normal data
chisq.test(x=countsn, p=countst, rescale.p=TRUE, simulate.p.value=TRUE)
# Return p-value
chisqtest <- chisq.test(x=countsn, p=countst, rescale.p=TRUE, simulate.p.value=TRUE)
chisqtest$p.value
# Observed frequencies from shifted normal data
histp <- hist(rnorm(1e3, mean=2), breaks=100, plot=FALSE)
countsn <- histp$counts/sum(histp$counts)
# Theoretical frequencies
countst <- rutils::diffit(pnorm(histp$breaks))
# Perform Chi-squared test for shifted normal data
chisq.test(x=countsn, p=countst, rescale.p=TRUE, simulate.p.value=TRUE)
# Calculate histogram of VTI returns
histp <- hist(retp, breaks=100, plot=FALSE)
countsn <- histp$counts
# Calculate cumulative probabilities and then difference them
countst <- pt((histp$breaks-locv)/scalev, df=2)
countst <- rutils::diffit(countst)
# Perform Chi-squared test for VTI returns
chisq.test(x=countsn, p=countst, rescale.p=TRUE, simulate.p.value=TRUE)

# Objective function from function dt()
likefun <- function(par, dfree, datav) {
  -sum(log(dt(x=(datav-par[1])/par[2], df=dfree)/par[2]))
}  # end likefun
# Demonstrate equivalence with log(dt())
likefun(c(1, 0.5), 2, 2:5)
-sum(log(dt(x=(2:5-1)/0.5, df=2)/0.5))
# Objective function is negative log-likelihood
likefun <- function(par, dfree, datav) {
  sum(-log(gamma((dfree+1)/2)/(sqrt(pi*dfree)*gamma(dfree/2))) +
    log(par[2]) + (dfree+1)/2*log(1+((datav-par[1])/par[2])^2/dfree))
}  # end likefun

# Calculate VTI percentage returns
retp <- as.numeric(na.omit(rutils::etfenv$returns$VTI))
# Fit VTI returns using MASS::fitdistr()
fitobj <- MASS::fitdistr(retp, densfun="t", df=3)
summary(fitobj)
# Fitted parameters
fitobj$estimate
locv <- fitobj$estimate[1]
scalev <- fitobj$estimate[2]
locv; scalev
# Standard errors of parameters
fitobj$sd
# Log-likelihood value
fitobj$value
# Fit distribution using optim()
initp <- c(mean=0, scale=0.01)  # Initial parameters
fitobj <- optim(par=initp,
  fn=likefun, # Log-likelihood function
  datav=retp,
  dfree=3, # Degrees of freedom
  method="L-BFGS-B", # Quasi-Newton method
  upper=c(1, 0.1), # Upper constraint
  lower=c(-1, 1e-7)) # Lower constraint
# Optimal parameters
locv <- fitobj$par["mean"]
scalev <- fitobj$par["scale"]
locv; scalev

dev.new(width=6, height=5, noRStudioGD=TRUE)
# x11(width=6, height=5)
# Plot histogram of VTI returns
madv <- mad(retp)
histp <- hist(retp, col="lightgrey",
  xlab="returns", breaks=100, xlim=c(-5*madv, 5*madv),
  ylab="frequency", freq=FALSE, main="Histogram of VTI Returns")
lines(density(retp, adjust=1.5), lwd=3, col="blue")
# Plot the Normal probability distribution
curve(expr=dnorm(x, mean=mean(retp),
  sd=sd(retp)), add=TRUE, lwd=3, col="green")
# Define non-standard t-distribution
tdistr <- function(x, dfree, locv=0, scalev=1) {
  dt((x-locv)/scalev, df=dfree)/scalev
}  # end tdistr
# Plot t-distribution function
curve(expr=tdistr(x, dfree=3, locv=locv, scalev=scalev), col="red", lwd=3, add=TRUE)
# Add legend
legend("topright", inset=0.05, bty="n", y.intersp=0.4,
  leg=c("density", "t-distr", "normal"),
  lwd=6, lty=1, col=c("blue", "red", "green"))

# Calculate sample from non-standard t-distribution with df=3
datat <- locv + scalev*rt(NROW(retp), df=3)
# KS test for VTI returns vs t-distribution data
ks.test(retp, datat)

# Q-Q plot of VTI Returns vs non-standard t-distribution
qqplot(datat, retp, xlab="t-Dist Quantiles", ylab="VTI Quantiles",
       main="Q-Q plot of VTI Returns vs Student's t-distribution")
# Calculate quartiles of the distributions
probs <- c(0.25, 0.75)
qrets <- quantile(retp, probs)
qtdata <- quantile(datat, probs)
# Calculate slope and plot line connecting quartiles
slope <- diff(qrets)/diff(qtdata)
intercept <- qrets[1]-slope*qtdata[1]
abline(intercept, slope, lwd=2, col="red")

# Plot log density of VTI returns
plot(density(retp, adjust=4), xlab="VTI Returns", ylab="Density",
     main="Fat Left Tail of VTI Returns (density in log scale)",
     type="l", lwd=3, col="blue", xlim=c(min(retp), -0.02), log="y")
# Plot t-distribution function
curve(expr=dt((x-locv)/scalev, df=3)/scalev, lwd=3, col="red", add=TRUE, log="y")
# Plot the Normal probability distribution
curve(expr=dnorm(x, mean=mean(retp), sd=sd(retp)), lwd=3, col="green", add=TRUE, log="y")
# Add legend
legend("topleft", inset=0.01, bty="n", y.intersp=c(0.25, 0.25, 0.25),
  legend=c("density", "t-distr", "normal"), y.intersp=0.4,
  lwd=6, lty=1, col=c("blue", "red", "green"))

# Calculate VTI returns and trading volumes
ohlc <- rutils::etfenv$VTI
closep <- drop(coredata(quantmod::Cl(ohlc)))
retp <- rutils::diffit(log(closep))
volumv <- coredata(quantmod::Vo(ohlc))
# Calculate trailing variance
lookb <- 121
varv <- HighFreq::roll_var_ohlc(log(ohlc), method="close", lookb=lookb, scalit=FALSE)
varv[1:lookb, ] <- varv[lookb+1, ]
# Calculate trailing average volume
volumr <- HighFreq::roll_sum(volumv, lookb=lookb)/lookb
# dygraph plot of VTI variance and trading volumes
datav <- xts::xts(cbind(varv, volumr), zoo::index(ohlc))
colv <- c("variance", "volume")
colnames(datav) <- colv
dygraphs::dygraph(datav, main="VTI Variance and Trading Volumes") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], strokeWidth=2, axis="y", col="blue") %>%
  dySeries(name=colv[2], strokeWidth=2, axis="y2", col="red") %>%
  dyLegend(show="always", width=500)

# Scale the returns using volume clock to trading time
retsc <- ifelse(volumv > 0, sqrt(volumr)*retp/sqrt(volumv), 0)
retsc <- sd(retp)*retsc/sd(retsc)
# retsc <- ifelse(volumv > 1e4, retp/volumv, 0)
# Calculate moments of scaled returns
nrows <- NROW(retp)
sapply(list(retp=retp, retsc=retsc),
  function(rets) {sapply(c(skew=3, kurt=4),
     function(x) sum((rets/sd(rets))^x)/nrows)
})  # end sapply

# x11(width=6, height=5)
dev.new(width=6, height=5, noRStudioGD=TRUE)
par(mar=c(3, 3, 2, 1), oma=c(1, 1, 1, 1))
# Plot densities of SPY returns
madv <- mad(retp)
# bwidth <- mad(rutils::diffit(retp))
plot(density(retp, bw=madv/10), xlim=c(-5*madv, 5*madv),
     lwd=3, mgp=c(2, 1, 0), col="blue",
     xlab="returns (standardized)", ylab="frequency",
     main="Density of Volume-scaled VTI Returns")
lines(density(retsc, bw=madv/10), lwd=3, col="red")
curve(expr=dnorm(x, mean=mean(retp), sd=sd(retp)),
add=TRUE, lwd=3, col="green")
# Add legend
legend("topright", inset=0.05, bty="n", y.intersp=0.4,
  leg=c("unscaled", "scaled", "normal"),
  lwd=6, lty=1, col=c("blue", "red", "green"))
quartz.save("figure/vti_scaled.png", type="png", width=6, height=5)
