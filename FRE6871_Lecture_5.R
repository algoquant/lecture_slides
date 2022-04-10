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
# Define explanatory (design) variable
nrows <- 100
set.seed(1121)  # initialize random number generator
design <- runif(nrows)
noise <- rnorm(nrows)
# Response equals linear form plus random noise
response <- (1 + design + noise)
# Calculate de-meaned explanatory (design) and response vectors
design_zm <- design - mean(design)
response_zm <- response - mean(response)
# Calculate the regression beta
betav <- sum(design_zm*response_zm)/sum(design_zm^2)
# Calculate the regression alpha
alpha <- mean(response) - betav*mean(design)
# Specify regression formula
formulav <- response ~ design
model <- lm(formulav)  # Perform regression
class(model)  # Regressions have class lm
attributes(model)
eval(model$call$formula)  # Regression formula
model$coeff  # Regression coefficients
all.equal(coef(model), c(alpha, betav),
  check.attributes=FALSE)
fit_ted <- (alpha + betav*design)
all.equal(fit_ted, model$fitted.values, check.attributes=FALSE)
x11(width=5, height=4)  # Open x11 for plotting
# Set plot parameters to reduce whitespace around plot
par(mar=c(5, 5, 2, 1), oma=c(0, 0, 0, 0))
# Plot scatterplot using formula
plot(formulav, xlab="design", ylab="response")
title(main="Simple Regression", line=0.5)
# Add regression line
abline(model, lwd=3, col="blue")
# Plot fitted (predicted) response values
points(x=design, y=model$fitted.values, pch=16, col="blue")
# Plot response without noise
lines(x=design, y=(response-noise), col="red", lwd=3)
legend(x="topleft", # Add legend
       legend=c("response without noise", "fitted values"),
       title=NULL, inset=0.08, cex=0.8, lwd=6,
       lty=1, col=c("red", "blue"))
# Calculate the residuals
fit_ted <- (alpha + betav*design)
residuals <- (response - fit_ted)
all.equal(residuals, model$residuals, check.attributes=FALSE)
# Residuals are orthogonal to the design
all.equal(sum(residuals*design), target=0)
# Residuals are orthogonal to the fitted values
all.equal(sum(residuals*fit_ted), target=0)
# Sum of residuals is equal to zero
all.equal(mean(residuals), target=0)
x11(width=6, height=5)  # Open x11 for plotting
# Set plot parameters to reduce whitespace around plot
par(mar=c(5, 5, 1, 1), oma=c(0, 0, 0, 0))
# Extract residuals
datav <- cbind(design, model$residuals)
colnames(datav) <- c("design", "residuals")
# Plot residuals
plot(datav)
title(main="Residuals of the Linear Regression", line=-1)
abline(h=0, lwd=3, col="red")
# Degrees of freedom of residuals
deg_free <- model$df.residual
# Standard deviation of residuals
resid_std <- sqrt(sum(residuals^2)/deg_free)
# Standard error of beta
beta_std <- resid_std/sqrt(sum(design_zm^2))
# Standard error of alpha
alpha_std <- resid_std*
  sqrt(1/nrows + mean(design)^2/sum(design_zm^2))
model_sum <- summary(model)  # Copy regression summary
model_sum  # Print the summary to console
attributes(model_sum)$names  # get summary elements
model_sum$coeff
# Standard errors
model_sum$coefficients[2, "Std. Error"]
all.equal(c(alpha_std, beta_std),
  model_sum$coefficients[, "Std. Error"], 
  check.attributes=FALSE)
# R-squared
model_sum$r.squared
model_sum$adj.r.squared
# F-statistic and ANOVA
model_sum$fstatistic
anova(model)
set.seed(1121)  # initialize random number generator
# High noise compared to coefficient
response <- (1 + design + rnorm(nrows, sd=8))
model <- lm(formulav)  # Perform regression
# Values of regression coefficients are not
# Statistically significant
summary(model)
par(oma=c(1, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=1.0, cex.axis=1.0, cex.main=1.0, cex.sub=1.0)
reg_stats <- function(stdev) {  # Noisy regression
  set.seed(1121)  # initialize number generator
# Define explanatory (design) and response variables
  design <- rnorm(100, mean=2)
  response <- (1 + 0.2*design +
  rnorm(NROW(design), sd=stdev))
# Specify regression formula
  formulav <- response ~ design
# Perform regression and get summary
  model_sum <- summary(lm(formulav))
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
for (indeks in 1:NCOL(mat_stats)) {
  plot(mat_stats[, indeks], type="l",
 xaxt="n", xlab="", ylab="", main="")
  title(main=colnames(mat_stats)[indeks], line=-1.0)
  axis(1, at=1:(NROW(mat_stats)), labels=rownames(mat_stats))
}  # end for
reg_stats <- function(datav) {  # get regression
# Perform regression and get summary
  colnames <- colnames(datav)
  formulav <- paste(colnames[2], colnames[1], sep="~")
  model_sum <- summary(lm(formulav, data=datav))
# Extract regression statistics
  with(model_sum, c(pval=coefficients[2, 4],
   adj_rsquared=adj.r.squared,
   fstat=fstatistic[1]))
}  # end reg_stats
# Apply reg_stats() to vector of std dev values
vec_sd <- seq(from=0.1, to=0.5, by=0.1)
names(vec_sd) <- paste0("sd=", vec_sd)
mat_stats <- t(sapply(vec_sd, function(stdev) {
    set.seed(1121)  # initialize number generator
# Define explanatory (design) and response variables
    design <- rnorm(100, mean=2)
    response <- (1 + 0.2*design +
rnorm(NROW(design), sd=stdev))
    reg_stats(data.frame(design, response))
    }))
# Plot in loop
par(mfrow=c(NCOL(mat_stats), 1))
for (indeks in 1:NCOL(mat_stats)) {
  plot(mat_stats[, indeks], type="l",
 xaxt="n", xlab="", ylab="", main="")
  title(main=colnames(mat_stats)[indeks], line=-1.0)
  axis(1, at=1:(NROW(mat_stats)),
 labels=rownames(mat_stats))
}  # end for
# Set plot paramaters - margins and font scale
par(oma=c(1,0,1,0), mgp=c(2,1,0), mar=c(2,1,2,1), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2, 2))  # Plot 2x2 panels
plot(model)  # Plot diagnostic scatterplots
plot(model, which=2)  # Plot just Q-Q
library(lmtest)  # Load lmtest
# Perform Durbin-Watson test
lmtest::dwtest(model)
x11(width=6, height=5)  # Open x11 for plotting
# Set plot parameters to reduce whitespace around plot
par(mar=c(5, 5, 2, 1), oma=c(0, 0, 0, 0))
# Add unit column to the design matrix
design <- cbind(rep(1, NROW(design)), design)
# Calculate generalized inverse of the design matrix
design_inv <- MASS::ginv(design)
# Calculate the influence matrix
influ_ence <- design %*% design_inv
# Plot the leverage vector
ordern <- order(design[, 2])
plot(x=design[ordern, 2], y=diag(influ_ence)[ordern],
     type="l", lwd=3, col="blue",
     xlab="predictor", ylab="leverage",
     main="Leverage as Function of Predictor")
# Calculate the influence matrix
influ_ence <- design %*% design_inv
# The influence matrix is idempotent
all.equal(influ_ence, influ_ence %*% influ_ence)
# Calculate covariance and standard deviations of fitted values
betas <- design_inv %*% response
fit_ted <- drop(design %*% betas)
residuals <- drop(response - fit_ted)
deg_free <- (NROW(design) - NCOL(design))
var_resid <- sqrt(sum(residuals^2)/deg_free)
fit_covar <- var_resid*influ_ence
fit_sd <- sqrt(diag(fit_covar))
# Plot the standard deviations
fit_sd <- cbind(fitted=fit_ted, stddev=fit_sd)
fit_sd <- fit_sd[order(fit_ted), ]
plot(fit_sd, type="l", lwd=3, col="blue",
     xlab="Fitted Value", ylab="Standard Deviation",
     main="Standard Deviations of Fitted Values\nin Univariate Regression")
# Calculate response without random noise for univariate regression,
# equal to weighted sum over columns of design.
betas <- c(-1, 1)
response <- design %*% betas
# Perform loop over different realizations of random noise
fit_ted <- lapply(1:50, function(it) {
  # Add random noise to response
  response <- response + rnorm(nrows, sd=1.0)
  # Calculate fitted values using influence matrix
  influ_ence %*% response
})  # end lapply
fit_ted <- rutils::do_call(cbind, fit_ted)
x11(width=5, height=4)  # Open x11 for plotting
# Set plot parameters to reduce whitespace around plot
par(mar=c(5, 5, 2, 1), oma=c(0, 0, 0, 0))
# Plot fitted values
matplot(x=design[,2], y=fit_ted,
type="l", lty="solid", lwd=1, col="blue",
xlab="predictor", ylab="fitted",
main="Fitted Values for Different Realizations
of Random Noise")
lines(x=design[,2], y=response, col="red", lwd=4)
legend(x="topleft", # Add legend
       legend=c("response without noise", "fitted values"),
       title=NULL, inset=0.05, cex=0.8, lwd=6,
       lty=1, col=c("red", "blue"))
# Inverse of design matrix squared
design2 <- MASS::ginv(crossprod(design))
# Define new predictors
new_data <- (max(design[, 2]) + 10*(1:5)/nrows)
# Calculate the predicted values and standard errors
design_new <- cbind(rep(1, NROW(new_data)), new_data)
stdev <- sqrt(design_new %*% design2 %*% t(design_new))
predic_tions <- cbind(
  prediction=drop(design_new %*% betas),
  stddev=diag(var_resid*stdev))
# OR: Perform loop over new_data
predic_tions <- sapply(new_data, function(predictor) {
  predictor <- cbind(1, predictor)
  # Calculate predicted values
  predic_tion <- predictor %*% betas
  # Calculate standard deviation
  stdev <- sqrt(predictor %*% design2 %*% t(predictor))
  predict_sd <- var_resid*stdev
  c(prediction=predic_tion, stddev=predict_sd)
})  # end sapply
predic_tions <- t(predic_tions)
# Prepare plot data
x_data <- c(design[,2], new_data)
xlim <- range(x_data)
y_data <- c(fit_ted, predic_tions[, 1])
# Calculate t-quantile
t_quant <- qt(pnorm(2), df=deg_free)
predict_low <- predic_tions[, 1]-t_quant*predic_tions[, 2]
predict_high <- predic_tions[, 1]+t_quant*predic_tions[, 2]
ylim <- range(c(response, y_data, predict_low, predict_high))
# Plot the regression predictions
plot(x=x_data, y=y_data, xlim=xlim, ylim=ylim,
     type="l", lwd=3, col="blue",
     xlab="predictor", ylab="fitted or predicted",
     main="Predictions from Linear Regression")
points(x=design[,2], y=response, col="blue")
points(x=new_data, y=predic_tions[, 1], pch=16, col="blue")
lines(x=new_data, y=predict_high, lwd=3, col="red")
lines(x=new_data, y=predict_low, lwd=3, col="green")
legend(x="topleft", # Add legend
       legend=c("predictions", "+2SD", "-2SD"),
       title=NULL, inset=0.05, cex=0.8, lwd=6,
       lty=1, col=c("blue", "red", "green"))
# Perform univariate regression
predictor <- design[, 2]
model <- lm(response ~ predictor)
# Perform prediction from regression
new_data <- data.frame(predictor=new_data)
predict_lm <- predict(object=model,
  newdata=new_data, level=1-2*(1-pnorm(2)),
  interval="confidence")
predict_lm <- as.data.frame(predict_lm)
all.equal(predict_lm$fit, predic_tions[, 1])
all.equal(predict_lm$lwr, predict_low)
all.equal(predict_lm$upr, predict_high)
plot(response ~ predictor,
     xlim=range(predictor, new_data),
     ylim=range(response, predict_lm),
     xlab="predictor", ylab="fitted or predicted",
     main="Predictions from lm() Regression")
abline(model, col="blue", lwd=3)
with(predict_lm, {
  points(x=new_data$predictor, y=fit, pch=16, col="blue")
  lines(x=new_data$predictor, y=lwr, lwd=3, col="green")
  lines(x=new_data$predictor, y=upr, lwd=3, col="red")
})  # end with
legend(x="topleft", # Add legend
       legend=c("predictions", "+2SD", "-2SD"),
       title=NULL, inset=0.05, cex=0.8, lwd=6,
       lty=1, col=c("blue", "red", "green"))
set.seed(1121)
library(lmtest)
# Spurious regression in unit root time series
design <- cumsum(rnorm(100))  # Unit root time series
response <- cumsum(rnorm(100))
formulav <- response ~ design
model <- lm(formulav)  # Perform regression
# Summary indicates statistically significant regression
model_sum <- summary(model)
model_sum$coeff
model_sum$r.squared
# Durbin-Watson test shows residuals are autocorrelated
dw_test <- lmtest::dwtest(model)
c(dw_test$statistic[[1]], dw_test$p.value)
par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # Set plot panels
plot(formulav, xlab="", ylab="")  # Plot scatterplot using formula
title(main="Spurious Regression", line=-1)
# Add regression line
abline(model, lwd=2, col="red")
plot(model, which=2, ask=FALSE)  # Plot just Q-Q
# Define design matrix
nrows <- 100
ncols <- 5
set.seed(1121)  # initialize random number generator
design <- matrix(rnorm(nrows*ncols), ncol=ncols)
# Add column names
colnames(design) <- paste0("col", 1:ncols)
# Define the design weights
weightv <- sample(3:(ncols+2))
# Response equals weighted design plus random noise
noise <- rnorm(nrows, sd=5)
response <- (-1 + design %*% weightv + noise)
# Perform multivariate regression using lm()
model <- lm(response ~ design)
# Solve multivariate regression using matrix algebra
# Calculate de-meaned design matrix and response vector
design_zm <- t(t(design) - colMeans(design))
# design <- apply(design, 2, function(x) (x-mean(x)))
response_zm <- response - mean(response)
# Calculate the regression coefficients
betas <- drop(MASS::ginv(design_zm) %*% response_zm)
# Calculate the regression alpha
alpha <- mean(response) - sum(colSums(design)*betas)/nrows
# Compare with coefficients from lm()
all.equal(coef(model), c(alpha, betas), check.attributes=FALSE)
# Compare with actual coefficients
all.equal(c(-1, weightv), c(alpha, betas), check.attributes=FALSE)
# Add intercept column to design matrix
design <- cbind(rep(1, NROW(design)), design)
ncols <- NCOL(design)
# Add column name
colnames(design)[1] <- "intercept"
# Calculate generalized inverse of the design matrix
design_inv <- MASS::ginv(design)
# Calculate the regression coefficients
betas <- design_inv %*% response
# Perform multivariate regression without intercept term
model <- lm(response ~ design - 1)
all.equal(drop(betas), coef(model), check.attributes=FALSE)
# Calculate fitted values from regression coefficients
fit_ted <- drop(design %*% betas)
all.equal(fit_ted, model$fitted.values, check.attributes=FALSE)
# Calculate the residuals
residuals <- drop(response - fit_ted)
all.equal(residuals, model$residuals, check.attributes=FALSE)
# Residuals are orthogonal to design columns (predictors)
sapply(residuals %*% design, all.equal, target=0)
# Residuals are orthogonal to the fitted values
all.equal(sum(residuals*fit_ted), target=0)
# Sum of residuals is equal to zero
all.equal(sum(residuals), target=0)
# Calculate the influence matrix
influ_ence <- design %*% design_inv
# The influence matrix is idempotent
all.equal(influ_ence, influ_ence %*% influ_ence)
# Calculate fitted values using influence matrix
fit_ted <- drop(influ_ence %*% response)
all.equal(fit_ted, model$fitted.values, check.attributes=FALSE)
# Calculate fitted values from regression coefficients
fit_ted <- drop(design %*% betas)
all.equal(fit_ted, model$fitted.values, check.attributes=FALSE)
# Calculate zero mean fitted values
design_zm <- t(t(design) - colMeans(design))
fitted_zm <- drop(design_zm %*% betas)
all.equal(fitted_zm,
  model$fitted.values - mean(response),
  check.attributes=FALSE)
# Calculate the residuals
response_zm <- response - mean(response)
residuals <- drop(response_zm - fitted_zm)
all.equal(residuals, model$residuals,
  check.attributes=FALSE)
# Calculate the influence matrix
influence_zm <- design_zm %*% MASS::ginv(design_zm)
# Compare the fitted values
all.equal(fitted_zm,
  drop(influence_zm %*% response_zm),
  check.attributes=FALSE)
library(lmtest)  # Load lmtest
# Define design matrix
ex_plan <- 1:30
omit_var <- sin(0.2*1:30)
# Response depends on both predictors
res_p <- 0.2*ex_plan + omit_var + 0.2*rnorm(30)
# Mis-specified regression only one predictor
model_ovb <- lm(res_p ~ ex_plan)
model_sum <- summary(model_ovb)
model_sum$coeff
model_sum$r.squared
# Durbin-Watson test shows residuals are autocorrelated
lmtest::dwtest(model_ovb)
# Plot the regression diagnostic plots
x11(width=5, height=7)
par(mfrow=c(2,1))  # Set plot panels
par(mar=c(3, 2, 1, 1), oma=c(1, 0, 0, 0))
plot(res_p ~ ex_plan)
abline(model_ovb, lwd=2, col="red")
title(main="Omitted Variable Regression", line=-1)
plot(model_ovb, which=2, ask=FALSE)  # Plot just Q-Q
# Regression model summary
model_sum <- summary(model)
# Degrees of freedom of residuals
nrows <- NROW(design)
ncols <- NCOL(design)
deg_free <- (nrows - ncols)
all.equal(deg_free, model_sum$df[2])
# Variance of residuals
var_resid <- sum(residuals^2)/deg_free
# Inverse of design matrix squared
design2 <- MASS::ginv(crossprod(design))
# design2 <- t(design) %*% design
# Variance of residuals
var_resid <- sum(residuals^2)/deg_free
# Calculate covariance matrix of betas
beta_covar <- var_resid*design2
# Round(beta_covar, 3)
betasd <- sqrt(diag(beta_covar))
all.equal(betasd, model_sum$coeff[, 2], check.attributes=FALSE)
# Calculate t-values of betas
beta_tvals <- drop(betas)/betasd
all.equal(beta_tvals, model_sum$coeff[, 3], check.attributes=FALSE)
# Calculate two-sided p-values of betas
beta_pvals <- 2*pt(-abs(beta_tvals), df=deg_free)
all.equal(beta_pvals, model_sum$coeff[, 4], check.attributes=FALSE)
# The square of the generalized inverse is equal
# to the inverse of the square
all.equal(MASS::ginv(crossprod(design)),
  design_inv %*% t(design_inv))
# Calculate the influence matrix
influ_ence <- design %*% design_inv
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
returns <- rutils::etfenv$returns[, c("XLF", "XLE")]
returns <- na.omit(returns)
nrows <- NROW(returns)
head(returns)
# Define regression formula
formulav <- paste(colnames(returns)[1],
  paste(colnames(returns)[-1], collapse="+"),
  sep=" ~ ")
# Standard regression
model <- lm(formulav, data=returns)
model_sum <- summary(model)
# Bootstrap of regression
set.seed(1121)  # initialize random number generator
boot_data <- sapply(1:100, function(x) {
  samplev <- sample.int(nrows, replace=TRUE)
  model <- lm(formulav, data=returns[samplev, ])
  model$coefficients
})  # end sapply
# Means and standard errors from regression
model_sum$coefficients
# Means and standard errors from bootstrap
dim(boot_data)
t(apply(boot_data, MARGIN=1,
function(x) c(mean=mean(x), stderror=sd(x))))
# New data predictor is a data frame or row vector
set.seed(1121)
new_data <- data.frame(matrix(c(1, rnorm(5)), nr=1))
colnames <- colnames(design)
colnames(new_data) <- colnames
new_datav <- as.matrix(new_data)
predic_tion <- drop(new_datav %*% betas)
stdev <- drop(sqrt(new_datav %*% beta_covar %*% t(new_datav)))
# Create formula from text string
formulav <- paste0("response ~ ",
  paste(colnames(design), collapse=" + "), " - 1")
# Specify multivariate regression using formula
model <- lm(formulav, data=data.frame(cbind(response, design)))
model_sum <- summary(model)
# Predict from lm object
predict_lm <- predict.lm(object=model, newdata=new_data,
   interval="confidence", level=1-2*(1-pnorm(2)))
# Calculate t-quantile
t_quant <- qt(pnorm(2), df=deg_free)
predict_high <- (predic_tion + t_quant*stdev)
predict_low <- (predic_tion - t_quant*stdev)
# Compare with matrix calculations
all.equal(predict_lm[1, "fit"], predic_tion)
all.equal(predict_lm[1, "lwr"], predict_low)
all.equal(predict_lm[1, "upr"], predict_high)
# TSS = ESS + RSS
t_ss <- sum((response-mean(response))^2)
e_ss <- sum((fit_ted-mean(fit_ted))^2)
r_ss <- sum(residuals^2)
all.equal(t_ss, e_ss + r_ss)
# Set regression attribute for intercept
attributes(model$terms)$intercept <- 1
# Regression summary
model_sum <- summary(model)
# Regression R-squared
rsquared <- e_ss/t_ss
all.equal(rsquared, model_sum$r.squared)
# Correlation between response and fitted values
cor_fitted <- drop(cor(response, fit_ted))
# Squared correlation between response and fitted values
all.equal(cor_fitted^2, rsquared)
nrows <- NROW(design)
ncols <- NCOL(design)
# Degrees of freedom of residuals
deg_free <- (nrows - ncols)
# Adjusted R-squared
rsquared_adj <- (1-sum(residuals^2)/deg_free/var(response))
# Compare adjusted R-squared from lm()
all.equal(drop(rsquared_adj), model_sum$adj.r.squared)
x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
# Plot three curves in loop
deg_free <- c(3, 5, 9)  # Degrees of freedom
colors <- c("black", "red", "blue", "green")
for (indeks in 1:NROW(deg_free)) {
curve(expr=df(x, df1=deg_free[indeks], df2=3),
xlim=c(0, 4), xlab="", ylab="", lwd=2,
col=colors[indeks], add=as.logical(indeks-1))
}  # end for
# Add title
title(main="F-Distributions", line=0.5)
# Add legend
labelv <- paste("df", deg_free, sep="=")
legend("topright", inset=0.05, title="degrees of freedom",
       labelv, cex=0.8, lwd=2, lty=1, col=colors)
sigma_x <- var(rnorm(nrows))
sigma_y <- var(rnorm(nrows))
fratio <- sigma_x/sigma_y
# Cumulative probability for q = fratio
pf(fratio, nrows-1, nrows-1)
# p-value for fratios
1-pf((10:20)/10, nrows-1, nrows-1)
# F-statistic from lm()
model_sum$fstatistic
# Degrees of freedom of residuals
deg_free <- (nrows - ncols)
# F-statistic from ESS and RSS
fstat <- (e_ss/(ncols-1))/(r_ss/deg_free)
all.equal(fstat, model_sum$fstatistic[1], check.attributes=FALSE)
# p-value of F-statistic
1-pf(q=fstat, df1=ncols-1, df2=nrows-ncols)
# Calculate ETF returns
returns <- na.omit(rutils::etfenv$returns)
# Perform singular value decomposition
svdec <- svd(returns)
x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
barplot(svdec$d, main="Singular Values of ETF Returns")
# Calculate generalized inverse from SVD
inverse <- svdec$v %*% (t(svdec$u) / svdec$d)
# Verify inverse property of inverse
all.equal(zoo::coredata(returns), returns %*% inverse %*% returns)
# Calculate regularized inverse from SVD
eigen_max <- 1:3
inv_reg <- svdec$v[, eigen_max] %*%
  (t(svdec$u[, eigen_max]) / svdec$d[eigen_max])
# Calculate regularized inverse using RcppArmadillo
rcpp_inverse <- HighFreq::calc_inv(returns, eigen_max=3)
all.equal(inv_reg, rcpp_inverse, check.attributes=FALSE)
# Calculate regularized inverse from Moore-Penrose pseudo-inverse
square_d <- t(returns) %*% returns
eigend <- eigen(square_d)
squared_inv <- eigend$vectors[, eigen_max] %*%
  (t(eigend$vectors[, eigen_max]) / eigend$values[eigen_max])
mp_inverse <- squared_inv %*% t(returns)
all.equal(inv_reg, mp_inverse, check.attributes=FALSE)
# Define transformation matrix
trans_mat <- matrix(runif(ncols^2, min=(-1), max=1), ncol=ncols)
# Calculate linear combinations of design columns
design_trans <- design %*% trans_mat
# Calculate the influence matrix
influence_trans <- design_trans %*% MASS::ginv(design_trans)
# Compare the influence matrices
all.equal(influ_ence, influence_trans)
lambda <- c(0.5, 1, 1.5)
colors <- c("red", "blue", "green")
# Plot three curves in loop
for (indeks in 1:3) {
  curve(expr=plogis(x, scale=lambda[indeks]),
xlim=c(-4, 4), type="l", xlab="", ylab="", lwd=4,
col=colors[indeks], add=(indeks>1))
}  # end for
# Add title
title(main="Logistic function", line=0.5)
# Add legend
legend("topleft", title="Scale parameters",
       paste("lambda", lambda, sep="="),
       inset=0.05, cex=0.8, lwd=6, bty="n",
       lty=1, col=colors)
set.seed(1121)  # Reset random number generator
# Simulate overlapping scores data
sample1 <- runif(100, max=0.6)
sample2 <- runif(100, min=0.4)
# Perform Wilcoxon test for mean
wilcox.test(sample1, sample2)
# Combine scores and add categorical variable
predictor <- c(sample1, sample2)
response <- c(logical(100), !logical(100))
# Perform logit regression
glmod <- glm(response ~ predictor, family=binomial(logit))
class(glmod)
summary(glmod)
x11(width=6, height=5)
par(mar=c(3, 3, 2, 2), mgp=c(2, 1, 0), oma=c(0, 0, 0, 0))
ordern <- order(predictor)
plot(x=predictor[ordern], y=glmod$fitted.values[ordern],
     main="Category Densities and Logistic Function",
     type="l", lwd=4, col="orange", xlab="score", ylab="density")
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
 lwd=c(6, NA, NA), pch=c(NA, 15, 15),
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
legend(x="top", bty="n", legend=c("b = 1", "b = 0"),
       title=NULL, inset=0.3, cex=1.0, lwd=6,
       lty=1, col=c("blue", "red"))
# Specify design matrix
design=cbind(intercept=rep(1, NROW(response)), predictor)
# Likelihood function of the logistic model
likefun <- function(coeff, response, design) {
  probs <- plogis(drop(design %*% coeff))
  -sum(response*log(probs) + (1-response)*log((1-probs)))
}  # end likefun
# Run likelihood function
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
# Optimize with respect to vector argument
optimd <- optim(par=vectorv, fn=rastrigin,
        method="L-BFGS-B",
        upper=c(4*pi, 4*pi),
        lower=c(pi/2, pi/2),
        param=1)
# Optimal parameters and value
optimd$par
optimd$value
rastrigin(optimd$par, param=1)
# Initial parameters
initp <- c(1, 1)
# Find max likelihood parameters using steepest descent optimizer
optim_fit <- optim(par=initp,
           fn=likefun, # Log-likelihood function
           method="L-BFGS-B", # Quasi-Newton method
           response=response,
           design=design,
           upper=c(20, 20), # Upper constraint
           lower=c(-20, -20), # Lower constraint
           hessian=TRUE)
# Optimal logistic parameters
optim_fit$par
unname(glmod$coefficients)
# Standard errors of parameters
sqrt(diag(solve(optim_fit$hessian)))
model_sum <- summary(glmod)
model_sum$coefficients[, 2]
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
colnames(Default)[1:2] <- c("de_fault", "stu_dent")
attach(Default)  # Attach Default to search path
# Explore credit default data
summary(Default)
sapply(Default, class)
dim(Default)
head(Default)
# Plot data points for non-defaulters
x11(width=6, height=5)
xlim <- range(balance); ylim <- range(income)
plot(income ~ balance,
     main="Default Dataset from Package ISLR",
     xlim=xlim, ylim=ylim, pch=4, col="blue",
     data=Default[!de_fault, ])
# Plot data points for defaulters
points(income ~ balance, pch=4, lwd=2, col="red",
 data=Default[de_fault, ])
# Add legend
legend(x="topright", legend=c("non-defaulters", "defaulters"),
 bty="n", col=c("blue", "red"), lty=1, lwd=6, pch=4)
# Wilcoxon test for balance predictor
wilcox.test(balance[de_fault], balance[!de_fault])
# Wilcoxon test for income predictor
wilcox.test(income[de_fault], income[!de_fault])
x11(width=6, height=5)
par(mfrow=c(1,2))  # Set plot panels
# Balance boxplot
boxplot(formula=balance ~ de_fault,
  col="lightgrey", main="balance", xlab="Default")
# Income boxplot
boxplot(formula=income ~ de_fault,
  col="lightgrey", main="income", xlab="Default")
# Fit logistic regression model
glmod <- glm(de_fault ~ balance, family=binomial(logit))
class(glmod)
summary(glmod)
x11(width=6, height=5)
par(mar=c(4, 4, 2, 2), oma=c(0, 0, 0, 0), mgp=c(2.5, 1, 0))
plot(x=balance, y=de_fault,
     main="Logistic Regression of Credit Defaults",
     col="orange", xlab="credit balance", ylab="defaults")
ordern <- order(balance)
lines(x=balance[ordern], y=glmod$fitted.values[ordern], col="blue", lwd=3)
legend(x="topleft", inset=0.1, bty="n", lwd=6,
 legend=c("defaults", "logit fitted values"),
 col=c("orange", "blue"), lty=c(NA, 1), pch=c(1, NA))
# Calculate cumulative defaults
to_tal <- sum(de_fault)
default_s <- sapply(balance, function(lim_it) {
    sum(de_fault[balance <= lim_it])
})  # end sapply
# Perform logit regression
glmod <- glm(cbind(default_s, to_tal-default_s) ~ balance,
  family=binomial(logit))
summary(glmod)
plot(x=balance, y=default_s/to_tal, col="orange", lwd=1,
     main="Cumulative Defaults Versus Balance",
     xlab="credit balance", ylab="cumulative defaults")
ordern <- order(balance)
lines(x=balance[ordern], y=glmod$fitted.values[ordern],
col="blue", lwd=3)
legend(x="topleft", inset=0.1, bty="n",
 legend=c("cumulative defaults", "fitted values"),
 col=c("orange", "blue"), lty=c(NA, 1), pch=c(1, NA), lwd=6)
# Fit multifactor logistic regression model
colnames <- colnames(Default)
formulav <- as.formula(paste(colnames[1],
  paste(colnames[-1], collapse="+"), sep=" ~ "))
formulav
glmod <- glm(formulav, data=Default, family=binomial(logit))
summary(glmod)
# Calculate cumulative defaults
cum_defaults <- sapply(balance, function(lim_it) {
c(student=sum(de_fault[stu_dent & (balance <= lim_it)]),
  non_student=sum(de_fault[!stu_dent & (balance <= lim_it)]))
})  # end sapply
total_defaults <- c(student=sum(stu_dent & de_fault),
      stu_dent=sum(!stu_dent & de_fault))
cum_defaults <- t(cum_defaults / total_defaults)
# Plot cumulative defaults
par(mfrow=c(1,2))  # Set plot panels
ordern <- order(balance)
plot(x=balance[ordern], y=cum_defaults[ordern, 1],
     col="red", t="l", lwd=2, xlab="credit balance", ylab="",
     main="Cumulative defaults of\n students and non-students")
lines(x=balance[ordern], y=cum_defaults[ordern, 2], col="blue", lwd=2)
legend(x="topleft", bty="n",
 legend=c("students", "non-students"),
 col=c("red", "blue"), text.col=c("red", "blue"), lwd=3)
# Balance boxplot for student factor
boxplot(formula=balance ~ !stu_dent,
  col="lightgrey", main="balance", xlab="Student")
# Perform in-sample forecast from logistic regression model
forecasts <- predict(glmod, type="response")
all.equal(glmod$fitted.values, forecasts)
# Define discrimination threshold value
threshold <- 0.7
# Calculate confusion matrix in-sample
table(actual=!de_fault, forecast=(forecasts < threshold))
# Fit logistic regression over training data
set.seed(1121)  # Reset random number generator
nrows <- NROW(Default)
samplev <- sample.int(n=nrows, size=nrows/2)
traindata <- Default[samplev, ]
glmod <- glm(formulav, data=traindata, family=binomial(logit))
# Forecast over test data out-of-sample
test_data <- Default[-samplev, ]
forecasts <- predict(glmod, newdata=test_data, type="response")
# Calculate confusion matrix out-of-sample
table(actual=!test_data$de_fault, 
forecast=(forecasts < threshold))
# Calculate confusion matrix out-of-sample
confmat <- table(actual=!test_data$de_fault, 
forecast=(forecasts < threshold))
confmat
# Calculate FALSE positive (type I error)
sum(!test_data$de_fault & (forecasts > threshold))
# Calculate FALSE negative (type II error)
sum(test_data$de_fault & (forecasts < threshold))
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
confun <- function(actu_al, forecasts, threshold) {
    confmat <- table(actu_al, (forecasts < threshold))
    confmat <- confmat / rowSums(confmat)
    c(typeI=confmat[2, 1], typeII=confmat[1, 2])
  }  # end confun
confun(!test_data$de_fault, forecasts, threshold=threshold)
# Define vector of discrimination thresholds
threshv <- seq(0.05, 0.95, by=0.05)^2
# Calculate error rates
error_rates <- sapply(threshv, confun,
  actu_al=!test_data$de_fault, forecasts=forecasts)  # end sapply
error_rates <- t(error_rates)
rownames(error_rates) <- threshv
error_rates <- rbind(c(1, 0), error_rates)
error_rates <- rbind(error_rates, c(0, 1))
# Calculate area under ROC curve (AUC)
true_pos <- (1 - error_rates[, "typeII"])
true_pos <- (true_pos + rutils::lagit(true_pos))/2
false_pos <- rutils::diffit(error_rates[, "typeI"])
abs(sum(true_pos*false_pos))
# Plot ROC Curve for Defaults
x11(width=5, height=5)
plot(x=error_rates[, "typeI"], y=1-error_rates[, "typeII"],
     xlab="FALSE positive rate", ylab="TRUE positive rate",
     main="ROC Curve for Defaults", type="l", lwd=3, col="blue")
abline(a=0.0, b=1.0, lwd=3, col="orange")
