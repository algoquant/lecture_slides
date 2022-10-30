# Define predictor matrix
nrows <- 100
ncols <- 5
set.seed(1121)  # initialize random number generator
predictor <- matrix(rnorm(nrows*ncols), ncol=ncols)
# Add column names
colnames(predictor) <- paste0("col", 1:ncols)
# Define the predictor weights
weights <- sample(3:(ncols+2))
# Response equals weighted predictor plus random noise
noise <- rnorm(nrows, sd=5)
response <- (-3 + 2*predictor %*% weights + noise)

# Perform multivariate regression using lm()
regmod <- lm(response ~ predictor)
# Solve multivariate regression using matrix algebra
# Calculate de-meaned predictor matrix and response vector
predzm <- t(t(predictor) - colMeans(predictor))
# predictor <- apply(predictor, 2, function(x) (x-mean(x)))
respzm <- response - mean(response)
# Calculate the regression coefficients
betas <- drop(MASS::ginv(predzm) %*% respzm)
# Calculate the regression alpha
alpha <- mean(response) - sum(colSums(predictor)*betas)/nrows
# Compare with coefficients from lm()
all.equal(coef(regmod), c(alpha, betas), check.attributes=FALSE)
# Compare with actual coefficients
all.equal(c(-1, weights), c(alpha, betas), check.attributes=FALSE)

# Add intercept column to predictor matrix
predictor <- cbind(rep(1, NROW(predictor)), predictor)
ncols <- NCOL(predictor)
# Add column name
colnames(predictor)[1] <- "intercept"
# Calculate generalized inverse of the predictor matrix
invpred <- MASS::ginv(predictor)
# Calculate the regression coefficients
betas <- invpred %*% response
# Perform multivariate regression without intercept term
regmod <- lm(response ~ predictor - 1)
all.equal(drop(betas), coef(regmod), check.attributes=FALSE)

# Calculate fitted values from regression coefficients
fittedv <- drop(predictor %*% betas)
all.equal(fittedv, regmod$fitted.values, check.attributes=FALSE)
# Calculate the residuals
residuals <- drop(response - fittedv)
all.equal(residuals, regmod$residuals, check.attributes=FALSE)
# Residuals are orthogonal to predictor columns (predictors)
sapply(residuals %*% predictor, all.equal, target=0)
# Residuals are orthogonal to the fitted values
all.equal(sum(residuals*fittedv), target=0)
# Sum of residuals is equal to zero
all.equal(sum(residuals), target=0)

# Calculate the influence matrix
influencem <- predictor %*% invpred
# The influence matrix is idempotent
all.equal(influencem, influencem %*% influencem)
# Calculate fitted values using influence matrix
fittedv <- drop(influencem %*% response)
all.equal(fittedv, regmod$fitted.values, check.attributes=FALSE)
# Calculate fitted values from regression coefficients
fittedv <- drop(predictor %*% betas)
all.equal(fittedv, regmod$fitted.values, check.attributes=FALSE)

# Calculate zero mean fitted values
predzm <- t(t(predictor) - colMeans(predictor))
fitted_zm <- drop(predzm %*% betas)
all.equal(fitted_zm,
  regmod$fitted.values - mean(response),
  check.attributes=FALSE)
# Calculate the residuals
respzm <- response - mean(response)
residuals <- drop(respzm - fitted_zm)
all.equal(residuals, regmod$residuals,
  check.attributes=FALSE)
# Calculate the influence matrix
influence_zm <- predzm %*% MASS::ginv(predzm)
# Compare the fitted values
all.equal(fitted_zm,
  drop(influence_zm %*% respzm),
  check.attributes=FALSE)

library(lmtest)  # Load lmtest
# Define predictor matrix
predictor <- 1:30
omitv <- sin(0.2*1:30)
# Response depends on both predictors
response <- 0.2*predictor + omitv + 0.2*rnorm(30)
# Mis-specified regression only one predictor
model_ovb <- lm(response ~ predictor)
modelsum <- summary(regmod_ovb)
modelsum$coeff
modelsum$r.squared
# Durbin-Watson test shows residuals are autocorrelated
lmtest::dwtest(regmod_ovb)
# Plot the regression diagnostic plots
x11(width=5, height=7)
par(mfrow=c(2,1))  # Set plot panels
par(mar=c(3, 2, 1, 1), oma=c(1, 0, 0, 0))
plot(response ~ predictor)
abline(regmod_ovb, lwd=2, col="red")
title(main="Omitted Variable Regression", line=-1)
plot(regmod_ovb, which=2, ask=FALSE)  # Plot just Q-Q

# Regression model summary
modelsum <- summary(regmod)
# Degrees of freedom of residuals
nrows <- NROW(predictor)
ncols <- NCOL(predictor)
degf <- (nrows - ncols)
all.equal(degf, modelsum$df[2])
# Variance of residuals
residvar <- sum(residuals^2)/degf

# Inverse of predictor matrix squared
predictor2 <- MASS::ginv(crossprod(predictor))
# predictor2 <- t(predictor) %*% predictor
# Variance of residuals
residvar <- sum(residuals^2)/degf
# Calculate covariance matrix of betas
beta_covar <- residvar*predictor2
# Round(beta_covar, 3)
betasd <- sqrt(diag(beta_covar))
all.equal(betasd, modelsum$coeff[, 2], check.attributes=FALSE)
# Calculate t-values of betas
beta_tvals <- drop(betas)/betasd
all.equal(beta_tvals, modelsum$coeff[, 3], check.attributes=FALSE)
# Calculate two-sided p-values of betas
beta_pvals <- 2*pt(-abs(beta_tvals), df=degf)
all.equal(beta_pvals, modelsum$coeff[, 4], check.attributes=FALSE)
# The square of the generalized inverse is equal
# to the inverse of the square
all.equal(MASS::ginv(crossprod(predictor)),
  invpred %*% t(invpred))

# Calculate the influence matrix
influencem <- predictor %*% invpred
# The influence matrix is idempotent
all.equal(influencem, influencem %*% influencem)

# Calculate covariance and standard deviations of fitted values
fitcovar <- residvar*influencem
fitsd <- sqrt(diag(fitcovar))
# Sort the standard deviations
fitsd <- cbind(fitted=fittedv, stddev=fitsd)
fitsd <- fitsd[order(fittedv), ]
# Plot the standard deviations
plot(fitsd, type="l", lwd=3, col="blue",
     xlab="Fitted Value", ylab="Standard Deviation",
     main="Standard Deviations of Fitted Values\nin Multivariate Regression")

# Load time series of ETF percentage returns
retsp <- rutils::etfenv$returns[, c("XLF", "XLE")]
retsp <- na.omit(retsp)
nrows <- NROW(retsp)
head(retsp)
# Define regression formula
formulav <- paste(colnames(retsp)[1],
  paste(colnames(retsp)[-1], collapse="+"),
  sep=" ~ ")
# Standard regression
regmod <- lm(formulav, data=retsp)
modelsum <- summary(regmod)
# Bootstrap of regression
set.seed(1121)  # initialize random number generator
bootd <- sapply(1:100, function(x) {
  samplev <- sample.int(nrows, replace=TRUE)
  regmod <- lm(formulav, data=retsp[samplev, ])
  regmod$coefficients
})  # end sapply
# Means and standard errors from regression
modelsum$coefficients
# Means and standard errors from bootstrap
dim(bootd)
t(apply(bootd, MARGIN=1,
function(x) c(mean=mean(x), stderror=sd(x))))

# New data predictor is a data frame or row vector
set.seed(1121)
newdata <- data.frame(matrix(c(1, rnorm(5)), nr=1))
colnamev <- colnames(predictor)
colnames(newdata) <- colnamev
newdatav <- as.matrix(newdata)
prediction <- drop(newdatav %*% betas)
predsd <- drop(sqrt(newdatav %*% beta_covar %*% t(newdatav)))

# Create formula from text string
formulav <- paste0("response ~ ",
  paste(colnames(predictor), collapse=" + "), " - 1")
# Specify multivariate regression using formula
regmod <- lm(formulav, data=data.frame(cbind(response, predictor)))
modelsum <- summary(regmod)
# Predict from lm object
predictlm <- predict.lm(object=model, newdata=newdata,
   interval="confidence", confl=1-2*(1-pnorm(2)))
# Calculate t-quantile
tquant <- qt(pnorm(2), df=degf)
predicthigh <- (prediction + tquant*predsd)
predictlow <- (prediction - tquant*predsd)
# Compare with matrix calculations
all.equal(predictlm[1, "fit"], prediction)
all.equal(predictlm[1, "lwr"], predictlow)
all.equal(predictlm[1, "upr"], predicthigh)

# TSS = ESS + RSS
tss <- sum((response-mean(response))^2)
ess <- sum((fittedv-mean(fittedv))^2)
rss <- sum(residuals^2)
all.equal(tss, ess + rss)

# Set regression attribute for intercept
attributes(regmod$terms)$intercept <- 1
# Regression summary
modelsum <- summary(regmod)
# Regression R-squared
rsquared <- ess/tss
all.equal(rsquared, modelsum$r.squared)
# Correlation between response and fitted values
cor_fitted <- drop(cor(response, fittedv))
# Squared correlation between response and fitted values
all.equal(cor_fitted^2, rsquared)

nrows <- NROW(predictor)
ncols <- NCOL(predictor)
# Degrees of freedom of residuals
degf <- (nrows - ncols)
# Adjusted R-squared
rsquared_adj <- (1-sum(residuals^2)/degf/var(response))
# Compare adjusted R-squared from lm()
all.equal(drop(rsquared_adj), modelsum$adj.r.squared)

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
       y.intersp=0.5, bty="n", labelv, cex=0.8, lwd=2, lty=1, col=colorv)

sigmax <- var(rnorm(nrows))
sigmay <- var(rnorm(nrows))
fratio <- sigmax/sigmay
# Cumulative probability for q = fratio
pf(fratio, nrows-1, nrows-1)
# p-value for fratios
1-pf((10:20)/10, nrows-1, nrows-1)

# F-statistic from lm()
modelsum$fstatistic
# Degrees of freedom of residuals
degf <- (nrows - ncols)
# F-statistic from ESS and RSS
fstat <- (ess/(ncols-1))/(rss/degf)
all.equal(fstat, modelsum$fstatistic[1], check.attributes=FALSE)
# p-value of F-statistic
1-pf(q=fstat, df1=(ncols-1), df2=(nrows-ncols))

lambdas <- c(0.5, 1, 1.5)
colorv <- c("red", "blue", "green")
# Plot three curves in loop
for (it in 1:3) {
  curve(expr=plogis(x, scale=lambdas[it]),
xlim=c(-4, 4), type="l", xlab="", ylab="", lwd=4,
col=colorv[it], add=(it>1))
}  # end for
# Add title
title(main="Logistic function", line=0.5)
# Add legend
legend("topleft", title="Scale parameters",
       paste("lambda", lambdas, sep="="), y.intersp=0.5,
       inset=0.05, cex=0.8, lwd=6, bty="n", lty=1, col=colorv)

set.seed(1121)  # Reset random number generator
# Simulate overlapping scores data
sample1 <- runif(100, max=0.6)
sample2 <- runif(100, min=0.4)
# Perform Mann-Whitney test for data location
wilcox.test(sample1, sample2)
# Combine scores and add categorical variable
predictor <- c(sample1, sample2)
response <- c(logical(100), !logical(100))
# Perform logit regression
logmod <- glm(response ~ predictor, family=binomial(logit))
class(logmod)
summary(logmod)

x11(width=6, height=5)
par(mar=c(3, 3, 2, 2), mgp=c(2, 1, 0), oma=c(0, 0, 0, 0))
ordern <- order(predictor)
plot(x=predictor[ordern], y=logmod$fitted.values[ordern],
     main="Category Densities and Logistic Function",
     type="l", lwd=4, col="orange", xlab="predictor", ylab="density")
densityv <- density(predictor[response])
densityv$y <- densityv$y/max(densityv$y)
lines(densityv, col="red")
polygon(c(min(densityv$x), densityv$x, max(densityv$x)), c(min(densityv$y), densityv$y, min(densityv$y)), col=rgb(1, 0, 0, 0.2), border=NA)
densityv <- density(predictor[!response])
densityv$y <- densityv$y/max(densityv$y)
lines(densityv, col="blue")
polygon(c(min(densityv$x), densityv$x, max(densityv$x)), c(min(densityv$y), densityv$y, min(densityv$y)), col=rgb(0, 0, 1, 0.2), border=NA)
# Add legend
legend(x="top", cex=1.0, bty="n", lty=c(1, NA, NA),
 lwd=c(6, NA, NA), pch=c(NA, 15, 15), y.intersp=0.5,
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
       title=NULL, inset=0.3, cex=1.0, lwd=6, y.intersp=0.5,
       bty="n", lty=1, col=c("blue", "red"))

# Specify predictor matrix
predictor=cbind(intercept=rep(1, NROW(response)), predictor)
# Likelihood function of the logistic model
likefun <- function(coeff, response, predictor) {
  probs <- plogis(drop(predictor %*% coeff))
  -sum(response*log(probs) + (1-response)*log((1-probs)))
}  # end likefun
# Run likelihood function
coeff <- c(1, 1)
likefun(coeff, response, predictor)

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
           response=response,
           predictor=predictor,
           upper=c(20, 20), # Upper constraint
           lower=c(-20, -20), # Lower constraint
           hessian=TRUE)
# Optimal logistic parameters
optiml$par
unname(logmod$coefficients)
# Standard errors of parameters
sqrt(diag(solve(optiml$hessian)))
modelsum <- summary(logmod)
modelsum$coefficients[, 2]

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
 y.intersp=0.5, bty="n", col=c("blue", "red"), lty=1, lwd=6, pch=4)

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
legend(x="topleft", inset=0.1, bty="n", lwd=6, y.intersp=0.5,
 legend=c("defaults", "logit fitted values"),
 col=c("orange", "blue"), lty=c(NA, 1), pch=c(1, NA))

# Calculate cumulative defaults
sumd <- sum(default)
defaultv <- sapply(balance, function(lim) {
    sum(default[balance <= lim])
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
legend(x="topleft", inset=0.1, bty="n", y.intersp=0.5,
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
cum_defaults <- sapply(balance, function(lim) {
c(student=sum(default[student & (balance <= lim)]),
  non_student=sum(default[!student & (balance <= lim)]))
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
legend(x="topleft", bty="n", y.intersp=0.5,
 legend=c("students", "non-students"),
 col=c("red", "blue"), text.col=c("red", "blue"), lwd=3)
# Balance boxplot for student factor
boxplot(formula=balance ~ !student,
  col="lightgrey", main="balance", xlab="Student")

# Perform in-sample forecast from logistic regression model
forecastv <- predict(logmod, type="response")
all.equal(logmod$fitted.values, forecastv)
# Define discrimination threshold value
threshold <- 0.7
# Calculate confusion matrix in-sample
table(actual=!default, forecast=(forecastv < threshold))
# Fit logistic regression over training data
set.seed(1121)  # Reset random number generator
nrows <- NROW(Default)
samplev <- sample.int(n=nrows, size=nrows/2)
traindata <- Default[samplev, ]
logmod <- glm(formulav, data=traindata, family=binomial(logit))
# Forecast over test data out-of-sample
testdata <- Default[-samplev, ]
forecastv <- predict(logmod, newdata=testdata, type="response")
# Calculate confusion matrix out-of-sample
table(actual=!testdata$default, forecast=(forecastv < threshold))

# Calculate confusion matrix out-of-sample
confmat <- table(actual=!testdata$default, 
forecast=(forecastv < threshold))
confmat
# Calculate FALSE positive (type I error)
sum(!testdata$default & (forecastv > threshold))
# Calculate FALSE negative (type II error)
sum(testdata$default & (forecastv < threshold))

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
confun <- function(actualv, forecastv, threshold) {
    confmat <- table(actualv, (forecastv < threshold))
    confmat <- confmat / rowSums(confmat)
    c(typeI=confmat[2, 1], typeII=confmat[1, 2])
  }  # end confun
confun(!testdata$default, forecastv, threshold=threshold)
# Define vector of discrimination thresholds
threshv <- seq(0.05, 0.95, by=0.05)^2
# Calculate error rates
error_rates <- sapply(threshv, confun,
  actualv=!testdata$default, forecastv=forecastv)  # end sapply
error_rates <- t(error_rates)
rownames(error_rates) <- threshv
error_rates <- rbind(c(1, 0), error_rates)
error_rates <- rbind(error_rates, c(0, 1))
# Calculate area under ROC curve (AUC)
truepos <- (1 - error_rates[, "typeII"])
truepos <- (truepos + rutils::lagit(truepos))/2
falsepos <- rutils::diffit(error_rates[, "typeI"])
abs(sum(truepos*falsepos))

# Plot ROC Curve for Defaults
x11(width=5, height=5)
plot(x=error_rates[, "typeI"], y=1-error_rates[, "typeII"],
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
dates <- paste(taq$DATE, taq$TIME_M)
# Coerce date-time index to POSIXlt
dates <- strptime(dates, "%Y%m%d %H:%M:%OS")
class(dates)
# Display more significant digits
# options("digits")
options(digits=20, digits.secs=10)
last(dates)
unclass(last(dates))
as.numeric(last(dates))
# Coerce date-time index to POSIXct
dates <- as.POSIXct(dates)
class(dates)
last(dates)
unclass(last(dates))
as.numeric(last(dates))
# Calculate the number of ticks per second
nsecs <- as.numeric(last(dates)) - as.numeric(first(dates))
NROW(taq)/(6.5*3600)
# Select TAQ data columns
taq <- taq[, .(price=PRICE, volume=SIZE)]
# Add date-time index
taq <- cbind(index=dates, taq)

# Coerce trade ticks to xts series
xtsv <- xts::xts(taq[, .(price, volume)], taq$index)
colnames(xtsv) <- paste(symbol, c("Close", "Volume"), sep=".")
save(xtsv, file="/Users/jerzy/Develop/data/xlk_tick_trades2020_0316.RData")
# save(xtsv, file="/Users/jerzy/Develop/data/xlk_tick_trades2020_0316.RData")
# Plot dygraph
dygraphs::dygraph(xtsv$XLK.Close,
  main="XLK Trade Ticks for 2020-03-16")
# Plot in x11 window
x11(width=6, height=5)
quantmod::chart_Series(x=xtsv$XLK.Close,
  name="XLK Trade Ticks for 2020-03-16")

# Calculate centered Hampel filter to remove price jumps
look_back <- 111
half_back <- look_back %/% 2
medianv <- roll::roll_median(taq$price, width=look_back)
# medianv <- TTR::runMedian(taq$price, n=look_back)
medianv <- rutils::lagit(medianv, lagg=(-half_back), pad_zeros=FALSE)
madv <- HighFreq::roll_var(matrix(taq$price), look_back=look_back, method="nonparametric")
# madv <- TTR::runMAD(taq$price, n=look_back)
madv <- rutils::lagit(madv, lagg=(-half_back), pad_zeros=FALSE)
# Calculate Z-scores
zscores <- (taq$price - medianv)/madv
zscores[is.na(zscores)] <- 0
zscores[!is.finite(zscores)] <- 0
sum(is.na(zscores))
sum(!is.finite(zscores))
range(zscores); mad(zscores)
hist(zscores, breaks=2000, xlim=c(-5*mad(zscores), 5*mad(zscores)))

# Define discrimination threshold value
threshold <- 6*mad(zscores)
# Remove price jumps with large z-scores
bad_ticks <- (abs(zscores) > threshold)
good_ticks <- taq[!bad_ticks]
# Calculate number of price jumps
sum(bad_ticks)/NROW(zscores)
# Coerce trade prices to xts
xtsv <- xts::xts(good_ticks[, .(price, volume)], good_ticks$index)
colnames(xtsv) <- c("XLK.Close", "XLK.Volume")
# Plot dygraph of the clean lots
dygraphs::dygraph(xtsv$XLK.Close,
  main="XLK Trade Ticks for 2020-03-16 (Hampel filtered)")
# Plot the large lots
x11(width=6, height=5)
quantmod::chart_Series(x=xtsv$XLK.Close,
  name="XLK Trade Ticks for 2020-03-16 (Hampel filtered)")

# Define discrimination threshold value
threshold <- 6*mad(zscores)
# Calculate number of prices classified as bad data
isbad <- (abs(zscores) > threshold)
sum(isbad)
# Add 200 random price jumps into prices
set.seed(1121)
nbad <- 200
isjump <- logical(NROW(closep))
isjump[sample(x=NROW(isjump), size=nbad)] <- TRUE
closep[isjump] <- closep[isjump]*
  sample(c(0.95, 1.05), size=nbad, replace=TRUE)
# Plot prices and medians
dygraphs::dygraph(cbind(closep, medianv), main="VTI median") %>%
  dyOptions(colors=c("black", "red"))
# Calculate time series of z-scores
medianv <- roll::roll_median(closep, width=look_back)
# medianv <- TTR::runMedian(closep, n=look_back)
madv <- HighFreq::roll_var(closep, look_back=look_back, method="nonparametric")
# madv <- TTR::runMAD(closep, n=look_back)
zscores <- (closep - medianv)/madv
zscores[1:look_back, ] <- 0
# Calculate number of prices classified as bad data
isbad <- (abs(zscores) > threshold)
sum(isbad)

# Calculate confusion matrix
table(actual=!isjump, forecast=!isbad)
sum(isbad)
# FALSE positive (type I error)
sum(!isjump & isbad)
# FALSE negative (type II error)
sum(isjump & !isbad)

# Confusion matrix as function of threshold
confun <- function(actualv, zscores, threshold) {
    confmat <- table(!actualv, !(abs(zscores) > threshold))
    confmat <- confmat / rowSums(confmat)
    c(typeI=confmat[2, 1], typeII=confmat[1, 2])
  }  # end confun
confun(isjump, zscores, threshold=threshold)
# Define vector of discrimination thresholds
threshv <- seq(from=0.2, to=5.0, by=0.2)
# Calculate error rates
error_rates <- sapply(threshv, confun,
  actualv=isjump, zscores=zscores)  # end sapply
error_rates <- t(error_rates)
rownames(error_rates) <- threshv
error_rates <- rbind(c(1, 0), error_rates)
error_rates <- rbind(error_rates, c(0, 1))
# Calculate area under ROC curve (AUC)
truepos <- (1 - error_rates[, "typeII"])
truepos <- (truepos + rutils::lagit(truepos))/2
falsepos <- rutils::diffit(error_rates[, "typeI"])
abs(sum(truepos*falsepos))

# Plot ROC curve for Hampel classifier
x11(width=6, height=5)
plot(x=error_rates[, "typeI"], y=1-error_rates[, "typeII"],
     xlab="FALSE positive rate", ylab="TRUE positive rate",
     xlim=c(0, 1), ylim=c(0, 1),
     main="ROC Curve for Hampel Classifier",
     type="l", lwd=3, col="blue")
abline(a=0.0, b=1.0, lwd=3, col="orange")

# Calculate centered Hampel filter over 3 data points
medianv <- roll::roll_median(taq$price, width=3)
medianv[1:2] <- taq$price[1:2]
medianv <- rutils::lagit(medianv, lagg=-1, pad_zeros=FALSE)
madv <- HighFreq::roll_var(matrix(taq$price), look_back=3, method="nonparametric")
madv <- rutils::lagit(madv, lagg=-1, pad_zeros=FALSE)
# Calculate Z-scores
zscores <- ifelse(madv > 0, (taq$price - medianv)/madv, 0)
range(zscores); mad(zscores)
madv <- mad(zscores[abs(zscores)>0])
hist(zscores, breaks=2000, xlim=c(-5*madv, 5*madv))

# Define discrimination threshold value
threshold <- 6*madv
bad_ticks <- (abs(zscores) > threshold)
good_ticks <- taq[!bad_ticks]
# Calculate number of price jumps
sum(bad_ticks)/NROW(zscores)
# Coerce trade prices to xts
xtsv <- xts::xts(good_ticks[, .(price, volume)], good_ticks$index)
colnames(xtsv) <- c("XLK.Close", "XLK.Volume")
# Plot dygraph of the clean lots
dygraphs::dygraph(xtsv$XLK.Close,
  main="XLK Trade Ticks for 2020-03-16 (Hampel filtered)")
# Plot the large lots
x11(width=6, height=5)
quantmod::chart_Series(x=xtsv$XLK.Close,
  name="XLK Trade Ticks for 2020-03-16 (Hampel filtered)")
