# Define predictor matrix
nrows <- 100
ncols <- 5
set.seed(1121)  # initialize random number generator
predm <- matrix(runif(nrows*ncols), ncol=ncols)
# Add column names
colnames(predm) <- paste0("pred", 1:ncols)
# Define the predictor weights
weightv <- runif(3:(ncols+2), min=(-1), max=1)
# Response equals weighted predictor plus random noise
noisev <- rnorm(nrows, sd=2)
respv <- (1 + predm %*% weightv + noisev)
# Perform multivariate regression using lm()
regmod <- lm(respv ~ predm)
# Solve multivariate regression using matrix algebra
# Calculate de-meaned predictor matrix and response vector
predzm <- t(t(predm) - colMeans(predm))
# predm <- apply(predm, 2, function(x) (x-mean(x)))
respzm <- respv - mean(respv)
# Calculate the regression coefficients
betav <- drop(MASS::ginv(predzm) %*% respzm)
# Calculate the regression alpha
alpha <- mean(respv) - sum(colSums(predm)*betav)/nrows
# Compare with coefficients from lm()
all.equal(coef(regmod), c(alpha, betav), check.attributes=FALSE)
# Compare with actual coefficients
all.equal(c(-1, weightv), c(alpha, betav), check.attributes=FALSE)
# Add intercept column to predictor matrix
predm <- cbind(rep(1, nrows), predm)
ncols <- NCOL(predm)
# Add column name
colnames(predm)[1] <- "intercept"
# Calculate generalized inverse of the predictor matrix
predinv <- MASS::ginv(predm)
# Calculate the regression coefficients
betav <- predinv %*% respv
# Perform multivariate regression without intercept term
regmod <- lm(respv ~ predm - 1)
all.equal(drop(betav), coef(regmod), check.attributes=FALSE)
# Calculate fitted values from regression coefficients
fitv <- drop(predm %*% betav)
all.equal(fitv, regmod$fitted.values, check.attributes=FALSE)
# Calculate the residuals
resids <- drop(respv - fitv)
all.equal(resids, regmod$residuals, check.attributes=FALSE)
# Residuals are orthogonal to predictor columns (predms)
sapply(resids %*% predm, all.equal, target=0)
# Residuals are orthogonal to the fitted values
all.equal(sum(resids*fitv), target=0)
# Sum of residuals is equal to zero
all.equal(sum(resids), target=0)
# Calculate the influence matrix
infmat <- predm %*% predinv
# The influence matrix is idempotent
all.equal(infmat, infmat %*% infmat)
# Calculate fitted values using influence matrix
fitv <- drop(infmat %*% respv)
all.equal(fitv, regmod$fitted.values, check.attributes=FALSE)
# Calculate fitted values from regression coefficients
fitv <- drop(predm %*% betav)
all.equal(fitv, regmod$fitted.values, check.attributes=FALSE)
# Calculate zero mean fitted values
predzm <- t(t(predm) - colMeans(predm))
fitted_zm <- drop(predzm %*% betav)
all.equal(fitted_zm,
  regmod$fitted.values - mean(respv),
  check.attributes=FALSE)
# Calculate the residuals
respzm <- respv - mean(respv)
resids <- drop(respzm - fitted_zm)
all.equal(resids, regmod$residuals,
  check.attributes=FALSE)
# Calculate the influence matrix
influence_zm <- predzm %*% MASS::ginv(predzm)
# Compare the fitted values
all.equal(fitted_zm,
  drop(influence_zm %*% respzm),
  check.attributes=FALSE)
# Perform PCA of the predictors
pcad <- prcomp(predm, center=FALSE, scale=FALSE)
# Calculate the PCA predictors
predpca <- predm %*% pcad$rotation
# Principal components are orthogonal to each other
round(t(predpca) %*% predpca, 2)
# Calculate the PCA regression coefficients using lm()
regmod <- lm(respv ~ predpca - 1)
summary(regmod)
regmod$coefficients
# Calculate the PCA regression coefficients directly
colSums(predpca*drop(respv))/colSums(predpca^2)
# Create almost collinear predictors
predc <- predm
predc[, 1] <- (predc[, 1]/1e3 + predc[, 2])
# Calculate the PCA predictors
pcad <- prcomp(predc, center=FALSE, scale=FALSE)
predpca <- predc %*% pcad$rotation
round(t(predpca) %*% predpca, 6)
# Calculate the PCA regression coefficients
drop(MASS::ginv(predpca) %*% respv)
# Calculate the PCA regression coefficients directly
colSums(predpca*drop(respv))/colSums(predpca^2)
library(lmtest)  # Load lmtest
# Define predictor matrix
predm <- 1:30
omitv <- sin(0.2*1:30)
# Response depends on both predictors
respv <- 0.2*predm + omitv + 0.2*rnorm(30)
# Mis-specified regression only one predictor
modovb <- lm(respv ~ predm)
regsum <- summary(modovb)
regsum$coeff
regsum$r.squared
# Durbin-Watson test shows residuals are autocorrelated
lmtest::dwtest(modovb)
# Plot the regression diagnostic plots
x11(width=5, height=7)
par(mfrow=c(2,1))  # Set plot panels
par(mar=c(3, 2, 1, 1), oma=c(1, 0, 0, 0))
plot(respv ~ predm)
abline(modovb, lwd=2, col="red")
title(main="Omitted Variable Regression", line=-1)
plot(modovb, which=2, ask=FALSE)  # Plot just Q-Q
# Regression model summary
regsum <- summary(regmod)
# Degrees of freedom of residuals
nrows <- NROW(predm)
ncols <- NCOL(predm)
degf <- (nrows - ncols)
all.equal(degf, regsum$df[2])
# Variance of residuals
residsd <- sum(resids^2)/degf
# Inverse of predictor matrix squared
predm2 <- MASS::ginv(crossprod(predm))
# predm2 <- t(predm) %*% predm
# Variance of residuals
residsd <- sum(resids^2)/degf
# Calculate covariance matrix of betas
betacovar <- residsd*predm2
# Round(betacovar, 3)
betasd <- sqrt(diag(betacovar))
all.equal(betasd, regsum$coeff[, 2], check.attributes=FALSE)
# Calculate t-values of betas
betatvals <- drop(betav)/betasd
all.equal(betatvals, regsum$coeff[, 3], check.attributes=FALSE)
# Calculate two-sided p-values of betas
betapvals <- 2*pt(-abs(betatvals), df=degf)
all.equal(betapvals, regsum$coeff[, 4], check.attributes=FALSE)
# The square of the generalized inverse is equal
# to the inverse of the square
all.equal(MASS::ginv(crossprod(predm)), predinv %*% t(predinv))
# Calculate the influence matrix
infmat <- predm %*% predinv
# The influence matrix is idempotent
all.equal(infmat, infmat %*% infmat)
# Calculate covariance and standard deviations of fitted values
fitcovar <- residsd*infmat
fitsd <- sqrt(diag(fitcovar))
# Sort the standard deviations
fitsd <- cbind(fitted=fitv, stdev=fitsd)
fitsd <- fitsd[order(fitv), ]
# Plot the standard deviations
plot(fitsd, type="l", lwd=3, col="blue",
     xlab="Fitted Value", ylab="Standard Deviation",
     main="Standard Deviations of Fitted Values\nin Multivariate Regression")
# Load time series of ETF percentage returns
retp <- rutils::etfenv$returns[, c("XLF", "XLE")]
retp <- na.omit(retp)
nrows <- NROW(retp)
head(retp)
# Define regression formula
formulav <- paste(colnames(retp)[1],
  paste(colnames(retp)[-1], collapse="+"),
  sep=" ~ ")
# Standard regression
regmod <- lm(formulav, data=retp)
regsum <- summary(regmod)
# Bootstrap of regression
set.seed(1121)  # initialize random number generator
bootd <- sapply(1:100, function(x) {
  samplev <- sample.int(nrows, replace=TRUE)
  regmod <- lm(formulav, data=retp[samplev, ])
  regmod$coefficients
})  # end sapply
# Means and standard errors from regression
regsum$coefficients
# Means and standard errors from bootstrap
dim(bootd)
t(apply(bootd, MARGIN=1,
function(x) c(mean=mean(x), stderror=sd(x))))
# New data predictor is a data frame or row vector
set.seed(1121)
newdata <- data.frame(matrix(c(1, rnorm(5)), nr=1))
colnamev <- colnames(predm)
colnames(newdata) <- colnamev
newdata <- as.matrix(newdata)
fcast <- drop(newdata %*% betav)
predsd <- drop(sqrt(newdata %*% betacovar %*% t(newdata)))
# Create formula from text string
formulav <- paste0("respv ~ ",
  paste(colnames(predm), collapse=" + "), " - 1")
# Specify multivariate regression using formula
regmod <- lm(formulav, data=data.frame(cbind(respv, predm)))
regsum <- summary(regmod)
# Predict from lm object
fcastlm <- predict.lm(object=model, newdata=newdata,
   interval="confidence", confl=1-2*(1-pnorm(2)))
# Calculate t-quantile
tquant <- qt(pnorm(2), df=degf)
fcasth <- (fcast + tquant*predsd)
fcastl <- (fcast - tquant*predsd)
# Compare with matrix calculations
all.equal(fcastlm[1, "fit"], fcast)
all.equal(fcastlm[1, "lwr"], fcastl)
all.equal(fcastlm[1, "upr"], fcasth)
# TSS = ESS + RSS
tss <- sum((respv-mean(respv))^2)
ess <- sum((fitv-mean(fitv))^2)
rss <- sum(resids^2)
all.equal(tss, ess + rss)
# Set regression attribute for intercept
attributes(regmod$terms)$intercept <- 1
# Regression summary
regsum <- summary(regmod)
# Regression R-squared
rsquared <- ess/tss
all.equal(rsquared, regsum$r.squared)
# Correlation between response and fitted values
corfit <- drop(cor(respv, fitv))
# Squared correlation between response and fitted values
all.equal(corfit^2, rsquared)
nrows <- NROW(predm)
ncols <- NCOL(predm)
# Degrees of freedom of residuals
degf <- (nrows - ncols)
# Adjusted R-squared
rsqadj <- (1-sum(resids^2)/degf/var(respv))
# Compare adjusted R-squared from lm()
all.equal(drop(rsqadj), regsum$adj.r.squared)
x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
# Plot three curves in loop
degf <- c(3, 5, 9)  # Degrees of freedom
colorv <- c("black", "red", "blue", "green")
for (it in 1:NROW(degf)) {
curve(expr=df(x, df1=degf[it], df2=3),
xlim=c(0, 4), xlab="", ylab="", lwd=2,
col=colorv[it], add=as.logical(it-1))
}  # end for
# Add title
title(main="F-Distributions", line=0.5)
# Add legend
labelv <- paste("df", degf, sep="=")
legend("topright", inset=0.05, title="degrees of freedom",
       y.intersp=0.4, bty="n", labelv, cex=0.8, lwd=2, lty=1, col=colorv)
sigmax <- var(rnorm(nrows))
sigmay <- var(rnorm(nrows))
fratio <- sigmax/sigmay
# Cumulative probability for q = fratio
pf(fratio, nrows-1, nrows-1)
# p-value for fratios
1-pf((10:20)/10, nrows-1, nrows-1)
# F-statistic from lm()
regsum$fstatistic
# Degrees of freedom of residuals
degf <- (nrows - ncols)
# F-statistic from ESS and RSS
fstat <- (ess/(ncols-1))/(rss/degf)
all.equal(fstat, regsum$fstatistic[1], check.attributes=FALSE)
# p-value of F-statistic
1-pf(q=fstat, df1=(ncols-1), df2=(nrows-ncols))
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
set.seed(1121)  # Reset random number generator
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
# Specify predictor matrix
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
rastrigin <- function(vectorv, param=25) {
  sum(vectorv^2 - param*cos(vectorv))
}  # end rastrigin
vectorv <- c(pi/6, pi/6)
rastrigin(vectorv=vectorv)
# Draw 3d surface plot of Rastrigin function
options(rgl.useNULL=TRUE); library(rgl)
rgl::persp3d(
  x=Vectorize(function(x, y) rastrigin(vectorv=c(x, y))),
  xlim=c(-10, 10), ylim=c(-10, 10),
  col="green", axes=FALSE, zlab="", main="rastrigin")
# Render the 3d surface plot of function
rgl::rglwidget(elementId="plot3drgl", width=400, height=400)
# Optimize with respect to vector argument
optiml <- optim(par=vectorv, fn=rastrigin,
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
# Coerce the student and default columns into Boolean
Default <- ISLR::Default
Default$default <- (Default$default == "Yes")
Default$student <- (Default$student == "Yes")
colnames(Default)[1:2] <- c("default", "student")
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
# Calculate cumulative defaults
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
glm_student <- glm(default ~ student, family=binomial(logit))
summary(glm_student)
# Multifactor coefficient is negative
logmod$coefficients
# Single-factor coefficient is positive
glm_student$coefficients
# Calculate cumulative defaults
cum_defaults <- sapply(balance, function(balv) {
c(student=sum(default[student & (balance <= balv)]),
  non_student=sum(default[!student & (balance <= balv)]))
})  # end sapply
total_defaults <- c(student=sum(student & default),
      student=sum(!student & default))
cum_defaults <- t(cum_defaults / total_defaults)
# Plot cumulative defaults
par(mfrow=c(1,2))  # Set plot panels
ordern <- order(balance)
plot(x=balance[ordern], y=cum_defaults[ordern, 1],
     col="red", t="l", lwd=2, xlab="credit balance", ylab="",
     main="Cumulative defaults of\n students and non-students")
lines(x=balance[ordern], y=cum_defaults[ordern, 2], col="blue", lwd=2)
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
# Calculate confusion matrix in-sample
table(actual=!default, forecast=(fcast < threshv))
# Fit logistic regression over training data
set.seed(1121)  # Reset random number generator
nrows <- NROW(Default)
samplev <- sample.int(n=nrows, size=nrows/2)
trainset <- Default[samplev, ]
logmod <- glm(formulav, data=trainset, family=binomial(logit))
# Forecast over test data out-of-sample
testset <- Default[-samplev, ]
fcast <- predict(logmod, newdata=testset, type="response")
# Calculate confusion matrix out-of-sample
table(actual=!testset$default, forecast=(fcast < threshv))
# Calculate confusion matrix out-of-sample
confmat <- table(actual=!testset$default, 
forecast=(fcast < threshv))
confmat
# Calculate FALSE positive (type I error)
sum(!testset$default & (fcast > threshv))
# Calculate FALSE negative (type II error)
sum(testset$default & (fcast < threshv))
# Calculate FALSE positive and FALSE negative rates
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
# Calculate error rates
errorr <- sapply(threshv, confun,
  actualv=!testset$default, fcast=fcast)  # end sapply
errorr <- t(errorr)
rownames(errorr) <- threshv
errorr <- rbind(c(1, 0), errorr)
errorr <- rbind(errorr, c(0, 1))
# Calculate area under ROC curve (AUC)
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
library(HighFreq)
# Read TAQ trade data from csv file
taq <- data.table::fread(file="/Users/jerzy/Develop/data/xlk_tick_trades_2020_03_16.csv")
# Inspect the TAQ data
taq
class(taq)
colnames(taq)
sapply(taq, class)
symbol <- taq$SYM_ROOT[1]
# Create date-time index
datev <- paste(taq$DATE, taq$TIME_M)
# Coerce date-time index to POSIXlt
datev <- strptime(datev, "%Y%m%d %H:%M:%OS")
class(datev)
# Display more significant digits
# options("digits")
options(digits=20, digits.secs=10)
last(datev)
unclass(last(datev))
as.numeric(last(datev))
# Coerce date-time index to POSIXct
datev <- as.POSIXct(datev)
class(datev)
last(datev)
unclass(last(datev))
as.numeric(last(datev))
# Calculate the number of seconds
nsecs <- as.numeric(last(datev)) - as.numeric(first(datev))
# Calculate the number of ticks per second
NROW(taq)/(6.5*3600)
# Select TAQ data columns
taq <- taq[, .(price=PRICE, volume=SIZE)]
# Add date-time index
taq <- cbind(index=datev, taq)
# Coerce trade ticks to xts series
xlk <- xts::xts(taq[, .(price, volume)], taq$index)
colnames(xlk) <- c("price", "volume")
save(xlk, file="/Users/jerzy/Develop/data/xlk_tick_trades2020_0316.RData")
# save(xlk, file="/Users/jerzy/Develop/data/xlk_tick_trades2020_0316.RData")
# Plot dygraph
dygraphs::dygraph(xlk$price, main="XLK Intraday Prices for 2020-03-16")
# Plot in x11 window
x11(width=6, height=5)
quantmod::chart_Series(x=xlk$price, name="XLK Intraday Prices for 2020-03-16")
# Calculate centered Hampel filter to remove price jumps
look_back <- 111
half_back <- look_back %/% 2
pricev <- xlk$price
medianv <- roll::roll_median(pricev, width=look_back)
colnames(medianv) <- c("median")
# Overwrite leading NA values
medianv[1:look_back, ] <- pricev[1:look_back, ]
# medianv <- TTR::runMedian(pricev, n=look_back)
medianv <- rutils::lagit(medianv, lagg=(-half_back), pad_zeros=FALSE)
madv <- HighFreq::roll_var(pricev, look_back=look_back, method="nonparametric")
# madv <- TTR::runMAD(pricev, n=look_back)
madv <- rutils::lagit(madv, lagg=(-half_back), pad_zeros=FALSE)
# Calculate Z-scores
zscores <- ifelse(madv > 0, (pricev - medianv)/madv, 0)
# Z-scores have very fat tails
range(zscores); mad(zscores)
hist(zscores, breaks=2000, xlim=c(-5*mad(zscores), 5*mad(zscores)))
# Define discrimination threshold value
threshv <- 6*mad(zscores)
# Identify price jumps with large z-scores
isbad <- (abs(zscores) > threshv)
# Calculate number of price jumps
sum(isbad)/NROW(zscores)
# Remove price jumps and calculate time series of clean prices
cleanp <- taq[!isbad]
xlkc <- xts::xts(cleanp[, .(price, volume)], cleanp$index)
colnames(xlkc) <- c("price", "volume")
# Plot dygraph of the clean prices
dygraphs::dygraph(xlkc$price,
  main="Clean XLK Intraday Prices for 2020-03-16")
# Plot using chart_Series()
x11(width=6, height=5)
quantmod::chart_Series(x=xlkc$price,
  name="Clean XLK Intraday Prices for 2020-03-16")
# Add 200 random price jumps into prices
set.seed(1121)
nbad <- 200
isjump <- logical(NROW(pricev))
isjump[sample(x=NROW(isjump), size=nbad)] <- TRUE
pricev <- xlk$price
pricev[isjump] <- pricev[isjump]*
  sample(c(0.98, 1.02), size=nbad, replace=TRUE)
# Calculate time series of z-scores
medianv <- roll::roll_median(pricev, width=look_back)
colnames(medianv) <- c("median")
# Plot the prices and medians
dygraphs::dygraph(cbind(pricev, medianv), main="VTI Median Prices") %>%
  dyOptions(colors=c("black", "red"))
# medianv <- TTR::runMedian(pricev, n=look_back)
madv <- HighFreq::roll_var(pricev, look_back=look_back, method="nonparametric")
# madv <- TTR::runMAD(pricev, n=look_back)
zscores <- ifelse(madv > 0, (pricev - medianv)/madv, 0)
zscores[1:look_back, ] <- 0
zscores <- zscores/(sum(abs(range(zscores)))/2)
# Identify price jumps with large z-scores
threshv <- 0.1
isbad <- (abs(zscores) > threshv)
sum(isbad)
# Calculate confusion matrix
table(actual=!isjump, forecast=!isbad)
sum(isbad)
# FALSE positive (type I error)
sum(!isjump & isbad)
# FALSE negative (type II error)
sum(isjump & !isbad)
# Confusion matrix as function of threshold
confun <- function(actualv, zscores, threshv) {
    confmat <- table(!actualv, !(abs(zscores) > threshv))
    confmat <- confmat / rowSums(confmat)
    c(typeI=confmat[2, 1], typeII=confmat[1, 2])
  }  # end confun
confun(isjump, zscores, threshv=threshv)
# Define vector of discrimination thresholds
threshv <- seq(from=0.001, to=0.1, by=0.001)
# Calculate error rates
errorr <- sapply(threshv, confun, actualv=isjump, zscores=zscores)
errorr <- t(errorr)
rownames(errorr) <- threshv
errorr <- rbind(c(1, 0), errorr)
errorr <- rbind(errorr, c(0, 1))
# Calculate area under ROC curve (AUC)
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
