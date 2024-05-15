# Formula of linear model with zero intercept
formulav <- z ~ x + y - 1
formulav
# Collapse vector of strings into single text string
paste0("x", 1:5)
paste(paste0("x", 1:5), collapse="+")
# Create formula from text string
formulav <- as.formula(
  # Coerce text strings to formula
  paste("z ~ ",
  paste(paste0("x", 1:5), collapse="+")
  )  # end paste
)  # end as.formula
class(formulav)
formulav
# Modify the formula using "update"
update(formulav, log(.) ~ . + beta)
# Define explanatory (predm) variable
nrows <- 100
# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
predm <- runif(nrows)
noisev <- rnorm(nrows)
# Response equals linear form plus random noise
respv <- (-3 + 2*predm + noisev)
# Calculate the regression beta
betac <- cov(predm, respv)/var(predm)
# Calculate the regression alpha
alphac <- mean(respv) - betac*mean(predm)
# Specify regression formula
formulav <- respv ~ predm
regmod <- lm(formulav)  # Perform regression
class(regmod)  # Regressions have class lm
attributes(regmod)
eval(regmod$call$formula)  # Regression formula
regmod$coeff  # Regression coefficients
all.equal(coef(regmod), c(alphac, betac),
      check.attributes=FALSE)
# x11(width=5, height=4)  # Open x11 for plotting
# Set plot parameters to reduce whitespace around plot
# par(mar=c(5, 5, 2, 1), oma=c(0, 0, 0, 0))
fitv <- (alphac + betac*predm)
all.equal(fitv, regmod$fitted.values, check.attributes=FALSE)
# Plot scatterplot using formula
plot(formulav, xlab="predictor", ylab="response")
title(main="Simple Regression", line=0.5)
# Add regression line
abline(regmod, lwd=3, col="blue")
# Plot fitted (forecast) response values
points(x=predm, y=regmod$fitted.values, pch=16, col="blue")
# Plot response without noise
lines(x=predm, y=(respv-noisev), col="red", lwd=3)
legend(x="topleft", # Add legend
       legend=c("response without noise", "fitted values"),
       title=NULL, inset=0.0, cex=1.0, y.intersp=0.3,
       bty="n", lwd=6, lty=1, col=c("red", "blue"))
# Calculate the residuals
fitv <- (alphac + betac*predm)
resids <- (respv - fitv)
all.equal(resids, regmod$residuals, check.attributes=FALSE)
# Residuals are orthogonal to the predictor
all.equal(sum(resids*predm), target=0)
# Residuals are orthogonal to the fitted values
all.equal(sum(resids*fitv), target=0)
# Sum of residuals is equal to zero
all.equal(mean(resids), target=0)
x11(width=6, height=5)  # Open x11 for plotting
# Set plot parameters to reduce whitespace around plot
par(mar=c(5, 5, 1, 1), oma=c(0, 0, 0, 0))
# Extract residuals
datav <- cbind(predm, regmod$residuals)
colnames(datav) <- c("predictor", "residuals")
# Plot residuals
plot(datav)
title(main="Residuals of the Linear Regression", line=-1)
abline(h=0, lwd=3, col="red")
# Calculate the centered (de-meaned) predictor and response vectors
predc <- predm - mean(predm)
respc <- respv - mean(respv)
# Degrees of freedom of residuals
degf <- regmod$df.residual
# Standard deviation of residuals
residsd <- sqrt(sum(resids^2)/degf)
# Standard error of beta
betasd <- residsd/sqrt(sum(predc^2))
# Standard error of alpha
alphasd <- residsd*sqrt(1/nrows + mean(predm)^2/sum(predc^2))
regsum <- summary(regmod)  # Copy regression summary
regsum  # Print the summary to console
attributes(regsum)$names  # get summary elements
regsum$coeff
# Standard errors
regsum$coefficients[2, "Std. Error"]
all.equal(c(alphasd, betasd), regsum$coefficients[, "Std. Error"], 
  check.attributes=FALSE)
# R-squared
regsum$r.squared
regsum$adj.r.squared
# F-statistic and ANOVA
regsum$fstatistic
anova(regmod)
# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
# High noise compared to coefficient
respv <- (-3 + 2*predm + rnorm(nrows, sd=8))
regmod <- lm(formulav)  # Perform regression
# Values of regression coefficients are not
# Statistically significant
summary(regmod)
par(oma=c(1, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=1.0, cex.axis=1.0, cex.main=1.0, cex.sub=1.0)
regstats <- function(stdev) {  # Noisy regression
  set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")  # initialize number generator
# Define explanatory (predm) and response variables
  predm <- rnorm(100, mean=2)
  respv <- (1 + 0.2*predm + rnorm(nrows, sd=stdev))
# Specify regression formula
  formulav <- respv ~ predm
# Perform regression and get summary
  regsum <- summary(lm(formulav))
# Extract regression statistics
  with(regsum, c(pval=coefficients[2, 4],
   adj_rsquared=adj.r.squared,
   fstat=fstatistic[1]))
}  # end regstats
# Apply regstats() to vector of stdev dev values
vecsd <- seq(from=0.1, to=0.5, by=0.1)
names(vecsd) <- paste0("sd=", vecsd)
statsmat <- t(sapply(vecsd, regstats))
# Plot in loop
par(mfrow=c(NCOL(statsmat), 1))
for (it in 1:NCOL(statsmat)) {
  plot(statsmat[, it], type="l",
 xaxt="n", xlab="", ylab="", main="")
  title(main=colnames(statsmat)[it], line=-1.0)
  axis(1, at=1:(NROW(statsmat)), labels=rownames(statsmat))
}  # end for
regstats <- function(datav) {  # get regression
# Perform regression and get summary
  colnamev <- colnames(datav)
  formulav <- paste(colnamev[2], colnamev[1], sep="~")
  regsum <- summary(lm(formulav, data=datav))
# Extract regression statistics
  with(regsum, c(pval=coefficients[2, 4],
   adj_rsquared=adj.r.squared,
   fstat=fstatistic[1]))
}  # end regstats
# Apply regstats() to vector of stdev dev values
vecsd <- seq(from=0.1, to=0.5, by=0.1)
names(vecsd) <- paste0("sd=", vecsd)
statsmat <- t(sapply(vecsd, function(stdev) {
    set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")  # initialize number generator
# Define explanatory (predm) and response variables
    predm <- rnorm(100, mean=2)
    respv <- (1 + 0.2*predm + rnorm(nrows, sd=stdev))
    regstats(data.frame(predm, respv))
    }))
# Plot in loop
par(mfrow=c(NCOL(statsmat), 1))
for (it in 1:NCOL(statsmat)) {
  plot(statsmat[, it], type="l",
 xaxt="n", xlab="", ylab="", main="")
  title(main=colnames(statsmat)[it], line=-1.0)
  axis(1, at=1:(NROW(statsmat)),
 labels=rownames(statsmat))
}  # end for
# Set plot paramaters - margins and font scale
par(oma=c(1,0,1,0), mgp=c(2,1,0), mar=c(2,1,2,1), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2, 2))  # Plot 2x2 panels
plot(regmod)  # Plot diagnostic scatterplots
plot(regmod, which=2)  # Plot just Q-Q
library(lmtest)  # Load lmtest
# Perform Durbin-Watson test
lmtest::dwtest(regmod)
# Define linear regression data
# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
nrows <- 100
predm <- runif(nrows)
noisev <- rnorm(nrows)
respv <- (-3 + 2*predm + noisev)
x11(width=6, height=5)  # Open x11 for plotting
# Set plot parameters to reduce whitespace around plot
par(mar=c(5, 5, 2, 1), oma=c(0, 0, 0, 0))
# Add unit column to the predictor matrix
predm <- cbind(rep(1, nrows), predm)
# Calculate the generalized inverse of the predictor matrix
predinv <- MASS::ginv(predm)
# Calculate the influence matrix
infmat <- predm %*% predinv
# Plot the leverage vector
ordern <- order(predm[, 2])
plot(x=predm[ordern, 2], y=diag(infmat)[ordern],
     type="l", lwd=3, col="blue",
     xlab="predictor", ylab="leverage",
     main="Leverage as Function of Predictor")
# Calculate the influence matrix
infmat <- predm %*% predinv
# The influence matrix is idempotent
all.equal(infmat, infmat %*% infmat)
# Calculate the covariance and standard deviations of fitted values
betac <- predinv %*% respv
fitv <- drop(predm %*% betac)
resids <- drop(respv - fitv)
degf <- (NROW(predm) - NCOL(predm))
residsd <- sqrt(sum(resids^2)/degf)
fitcovar <- residsd*infmat
fitsd <- sqrt(diag(fitcovar))
# Plot the standard deviations
fitdata <- cbind(fitted=fitv, stdev=fitsd)
fitdata <- fitdata[order(fitv), ]
plot(fitdata, type="l", lwd=3, col="blue",
     xlab="Fitted Value", ylab="Standard Deviation",
     main="Standard Deviations of Fitted Values\nin Univariate Regression")
# Calculate the response without random noise for univariate regression,
# equal to weighted sum over columns of predictor.
respn <- predm %*% c(-1, 1)
# Perform loop over different realizations of random noise
fitm <- lapply(1:50, function(it) {
  # Add random noise to response
  respv <- respn + rnorm(nrows, sd=1.0)
  # Calculate the fitted values using influence matrix
  infmat %*% respv
})  # end lapply
fitm <- rutils::do_call(cbind, fitm)
x11(width=5, height=4)  # Open x11 for plotting
# Set plot parameters to reduce whitespace around plot
par(mar=c(5, 5, 2, 1), oma=c(0, 0, 0, 0))
# Plot fitted values
matplot(x=predm[, 2], y=fitm,
type="l", lty="solid", lwd=1, col="blue",
xlab="predictor", ylab="fitted",
main="Fitted Values for Different Realizations
of Random Noise")
lines(x=predm[, 2], y=respn, col="red", lwd=4)
legend(x="topleft", # Add legend
       legend=c("response without noise", "fitted values"),
       title=NULL, inset=0.05, cex=1.0, lwd=6, y.intersp=0.4,
       bty="n", lty=1, col=c("red", "blue"))
# Define new predictor
newdata <- (max(predm[, 2]) + 10*(1:5)/nrows)
predn <- cbind(rep(1, NROW(newdata)), newdata)
# Calculate the forecast values
fcast <- drop(predn %*% betac)
# Calculate the inverse of the predictor matrix squared
pred2 <- MASS::ginv(crossprod(predm))
# Calculate the standard errors
predsd <- residsd*sqrt(predn %*% pred2 %*% t(predn))
# Combine the forecast values and standard errors
fcast <- cbind(forecast=fcast, stdev=diag(predsd))
# Prepare plot data
xdata <- c(predm[, 2], newdata)
ydata <- c(fitv, fcast[, 1])
# Calculate the t-quantile
tquant <- qt(pnorm(2), df=degf)
fcastl <- fcast[, 1] - tquant*fcast[, 2]
fcasth <- fcast[, 1] + tquant*fcast[, 2]
# Plot the regression forecasts
xlim <- range(xdata)
ylim <- range(c(respv, ydata, fcastl, fcasth))
plot(x=xdata, y=ydata, xlim=xlim, ylim=ylim,
     type="l", lwd=3, col="blue",
     xlab="predictor", ylab="forecast",
     main="Forecasts from Linear Regression")
points(x=predm[, 2], y=respv, col="blue")
points(x=newdata, y=fcast[, 1], pch=16, col="blue")
lines(x=newdata, y=fcasth, lwd=3, col="red")
lines(x=newdata, y=fcastl, lwd=3, col="green")
legend(x="topleft", # Add legend
       legend=c("forecasts", "+2SD", "-2SD"),
       title=NULL, inset=0.05, cex=1.0, lwd=6, y.intersp=0.4,
       bty="n", lty=1, col=c("blue", "red", "green"))
# Perform univariate regression
dframe <- data.frame(resp=respv, pred=predm[, 2])
regmod <- lm(resp ~ pred, data=dframe)
# Calculate the forecasts from regression
newdf <- data.frame(pred=predn[, 2]) # Same column name
fcastlm <- predict.lm(object=regmod,
  newdata=newdf, confl=1-2*(1-pnorm(2)),
  interval="confidence")
rownames(fcastlm) <- NULL
all.equal(fcastlm[, "fit"], fcast[, 1])
all.equal(fcastlm[, "lwr"], fcastl)
all.equal(fcastlm[, "upr"], fcasth)
plot(x=xdata, y=ydata, xlim=xlim, ylim=ylim,
     type="l", lwd=3, col="blue",
     xlab="predictor", ylab="forecast",
     main="Forecasts from lm() Regression")
points(x=predm[, 2], y=respv, col="blue")
abline(regmod, col="blue", lwd=3)
points(x=newdata, y=fcastlm[, "fit"], pch=16, col="blue")
lines(x=newdata, y=fcastlm[, "lwr"], lwd=3, col="green")
lines(x=newdata, y=fcastlm[, "upr"], lwd=3, col="red")
legend(x="topleft", # Add legend
       legend=c("forecasts", "+2SD", "-2SD"),
       title=NULL, inset=0.05, cex=0.8, lwd=6, y.intersp=0.4,
       bty="n", lty=1, col=c("blue", "red", "green"))
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
library(lmtest)
# Spurious regression in unit root time series
predm <- cumsum(rnorm(100))  # Unit root time series
respv <- cumsum(rnorm(100))
formulav <- respv ~ predm
regmod <- lm(formulav)  # Perform regression
# Summary indicates statistically significant regression
regsum <- summary(regmod)
regsum$coeff
regsum$r.squared
# Durbin-Watson test shows residuals are autocorrelated
dwtest <- lmtest::dwtest(regmod)
c(dwtest$statistic[[1]], dwtest$p.value)
par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # Set plot panels
plot(formulav, xlab="", ylab="")  # Plot scatterplot using formula
title(main="Spurious Regression", line=-1)
# Add regression line
abline(regmod, lwd=2, col="red")
plot(regmod, which=2, ask=FALSE)  # Plot just Q-Q
# Define predictor matrix
nrows <- 100
ncols <- 5
# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
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
# Calculate the centered (de-meaned) predictor matrix and response vector
# predc <- t(t(predm) - colMeans(predm))
predc <- apply(predm, 2, function(x) (x-mean(x)))
respc <- respv - mean(respv)
# Calculate the regression coefficients
betac <- drop(MASS::ginv(predc) %*% respc)
# Calculate the regression alpha
alphac <- mean(respv) - sum(colSums(predm)*betac)/nrows
# Compare with coefficients from lm()
all.equal(coef(regmod), c(alphac, betac), check.attributes=FALSE)
# Compare with actual coefficients
all.equal(c(1, weightv), c(alphac, betac), check.attributes=FALSE)
# Add intercept column to predictor matrix
predm <- cbind(rep(1, nrows), predm)
ncols <- NCOL(predm)
# Add column name
colnames(predm)[1] <- "intercept"
# Calculate the generalized inverse of the predictor matrix
predinv <- MASS::ginv(predm)
# Calculate the regression coefficients
betac <- predinv %*% respv
# Perform multivariate regression without intercept term
regmod <- lm(respv ~ predm - 1)
all.equal(drop(betac), coef(regmod), check.attributes=FALSE)
# Calculate the fitted values from regression coefficients
fitv <- drop(predm %*% betac)
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
# Calculate the fitted values using influence matrix
fitv <- drop(infmat %*% respv)
all.equal(fitv, regmod$fitted.values, check.attributes=FALSE)
# Calculate the fitted values from regression coefficients
fitv <- drop(predm %*% betac)
all.equal(fitv, regmod$fitted.values, check.attributes=FALSE)
# Calculate the centered (de-meaned) fitted values
predc <- t(t(predm) - colMeans(predm))
fittedc <- drop(predc %*% betac)
all.equal(fittedc, regmod$fitted.values - mean(respv),
  check.attributes=FALSE)
# Calculate the residuals
respc <- respv - mean(respv)
resids <- drop(respc - fittedc)
all.equal(resids, regmod$residuals, check.attributes=FALSE)
# Calculate the influence matrix
infmatc <- predc %*% MASS::ginv(predc)
# Compare the fitted values
all.equal(fittedc, drop(infmatc %*% respc), check.attributes=FALSE)
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
predcol <- predm
predcol[, 1] <- (predcol[, 1]/1e3 + predcol[, 2])
# Calculate the PCA predictors
pcad <- prcomp(predcol, center=FALSE, scale=FALSE)
predpca <- predcol %*% pcad$rotation
round(t(predpca) %*% predpca, 6)
# Calculate the PCA regression coefficients
drop(MASS::ginv(predpca) %*% respv)
# Calculate the PCA regression coefficients directly
colSums(predpca*drop(respv))/colSums(predpca^2)
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
pred2 <- MASS::ginv(crossprod(predm))
# pred2 <- t(predm) %*% predm
# Variance of residuals
residsd <- sum(resids^2)/degf
# Calculate the covariance matrix of betas
betacovar <- residsd*pred2
# round(betacovar, 3)
betasd <- sqrt(diag(betacovar))
all.equal(betasd, regsum$coeff[, 2], check.attributes=FALSE)
# Calculate the t-values of betas
betatvals <- drop(betac)/betasd
all.equal(betatvals, regsum$coeff[, 3], check.attributes=FALSE)
# Calculate the two-sided p-values of betas
betapvals <- 2*pt(-abs(betatvals), df=degf)
all.equal(betapvals, regsum$coeff[, 4], check.attributes=FALSE)
# The square of the generalized inverse is equal
# to the inverse of the square
all.equal(MASS::ginv(crossprod(predm)), predinv %*% t(predinv))
# Calculate the influence matrix
infmat <- predm %*% predinv
# The influence matrix is idempotent
all.equal(infmat, infmat %*% infmat)
# Calculate the covariance and standard deviations of fitted values
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
# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
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
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
newdata <- data.frame(matrix(c(1, rnorm(5)), nr=1))
colnamev <- colnames(predm)
colnames(newdata) <- colnamev
newdata <- as.matrix(newdata)
fcast <- drop(newdata %*% betac)
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
# Calculate the t-quantile
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
# Plot four curves in loop
degf <- c(3, 5, 9, 21)  # Degrees of freedom
colorv <- c("black", "red", "blue", "green")
for (indeks in 1:NROW(degf)) {
  curve(expr=df(x, df1=degf[indeks], df2=3),
    xlim=c(0, 4), xlab="", ylab="", lwd=2,
    col=colorv[indeks], add=as.logical(indeks-1))
}  # end for
# Add title
title(main="F-Distributions", line=0.5)
# Add legend
labelv <- paste("degf", degf, sep=" = ")
legend("topright", title="Degrees of Freedom", inset=0.0, bty="n",
       y.intersp=0.4, labelv, cex=1.2, lwd=6, lty=1, col=colorv)
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
glm_student <- glm(default ~ student, family=binomial(logit))
summary(glm_student)
# Multifactor coefficient is negative
logmod$coefficients
# Single-factor coefficient is positive
glm_student$coefficients
# Calculate the cumulative defaults
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
sum(!testset$default & (fcast < threshv))
# Calculate the FALSE negative (type II error)
sum(testset$default & (fcast > threshv))
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
