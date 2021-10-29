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
points(x=de_sign, y=mod_el$fitted.values, pch=16, col="blue")

# Plot response without noise
lines(x=de_sign, y=(res_ponse-noise), col="red", lwd=3)
legend(x="topleft", # Add legend
       legend=c("response without noise", "fitted values"),
       title=NULL, inset=0.08, cex=0.8, lwd=6,
       lty=1, col=c("red", "blue"))

# Calculate the residuals
fit_ted <- (al_pha + be_ta*de_sign)
residual_s <- (res_ponse - fit_ted)
all.equal(residual_s, mod_el$residuals, check.attributes=FALSE)
# Residuals are orthogonal to the de_sign
all.equal(sum(residual_s*de_sign), target=0)
# Residuals are orthogonal to the fitted values
all.equal(sum(residual_s*fit_ted), target=0)
# Sum of residuals is equal to zero
all.equal(mean(residual_s), target=0)

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
resid_stddev <- sqrt(sum(residual_s^2)/deg_free)
# Standard error of beta
beta_stderror <- resid_stddev/sqrt(sum(design_zm^2))
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
  model_sum$coefficients[, "Std. Error"], 
  check.attributes=FALSE)
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
  axis(1, at=1:(NROW(mat_stats)), labels=rownames(mat_stats))
}  # end for

reg_stats <- function(da_ta) {  # get regression
# Perform regression and get summary
  col_names <- colnames(da_ta)
  for_mula <- paste(col_names[2], col_names[1], sep="~")
  model_sum <- summary(lm(for_mula, data=da_ta))
# Extract regression statistics
  with(model_sum, c(pval=coefficients[2, 4],
   adj_rsquared=adj.r.squared,
   fstat=fstatistic[1]))
}  # end reg_stats
# Apply reg_stats() to vector of std dev values
vec_sd <- seq(from=0.1, to=0.5, by=0.1)
names(vec_sd) <- paste0("sd=", vec_sd)
mat_stats <- t(sapply(vec_sd, function(std_dev) {
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
residual_s <- drop(res_ponse - fit_ted)
deg_free <- (NROW(de_sign) - NCOL(de_sign))
var_resid <- sqrt(sum(residual_s^2)/deg_free)
fit_covar <- var_resid*influ_ence
fit_sd <- sqrt(diag(fit_covar))
# Plot the standard deviations
fit_sd <- cbind(fitted=fit_ted, stddev=fit_sd)
fit_sd <- fit_sd[order(fit_ted), ]
plot(fit_sd, type="l", lwd=3, col="blue",
     xlab="Fitted Value", ylab="Standard Deviation",
     main="Standard Deviations of Fitted Values\nin Univariate Regression")

# Calculate response without random noise for univariate regression,
# equal to weighted sum over columns of de_sign.
beta_s <- c(-1, 1)
res_ponse <- de_sign %*% beta_s
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
std_dev <- sqrt(design_new %*% design_2 %*% t(design_new))
predic_tions <- cbind(
  prediction=drop(design_new %*% beta_s),
  stddev=diag(var_resid*std_dev))
# OR: Perform loop over new_data
predic_tions <- sapply(new_data, function(predic_tor) {
  predic_tor <- cbind(1, predic_tor)
  # Calculate predicted values
  predic_tion <- predic_tor %*% beta_s
  # Calculate standard deviation
  std_dev <- sqrt(predic_tor %*% design_2 %*% t(predic_tor))
  predict_sd <- var_resid*std_dev
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
plot(x=x_data, y=y_data, xlim=x_lim, ylim=y_lim,
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
dw_test <- lmtest::dwtest(mod_el)
c(dw_test$statistic[[1]], dw_test$p.value)

par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # Set plot panels
plot(for_mula, xlab="", ylab="")  # Plot scatterplot using formula
title(main="Spurious Regression", line=-1)
# Add regression line
abline(mod_el, lwd=2, col="red")
plot(mod_el, which=2, ask=FALSE)  # Plot just Q-Q

# Define design matrix
n_rows <- 100
n_cols <- 5
set.seed(1121)  # initialize random number generator
de_sign <- matrix(rnorm(n_rows*n_cols), ncol=n_cols)
# Add column names
colnames(de_sign) <- paste0("col", 1:n_cols)
# Define the design weights
weight_s <- sample(3:(n_cols+2))
# Response equals weighted de_sign plus random noise
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
beta_s <- drop(MASS::ginv(design_zm) %*% response_zm)
# Calculate the regression alpha
al_pha <- mean(res_ponse) - sum(colSums(de_sign)*beta_s)/n_rows
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
residual_s <- drop(res_ponse - fit_ted)
all.equal(residual_s, mod_el$residuals, check.attributes=FALSE)
# Residuals are orthogonal to de_sign columns (predictors)
sapply(residual_s %*% de_sign, all.equal, target=0)
# Residuals are orthogonal to the fitted values
all.equal(sum(residual_s*fit_ted), target=0)
# Sum of residuals is equal to zero
all.equal(sum(residual_s), target=0)

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
residual_s <- drop(response_zm - fitted_zm)
all.equal(residual_s, mod_el$residuals,
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
model_sum <- summary(mod_el)
# Degrees of freedom of residuals
n_rows <- NROW(de_sign)
n_cols <- NCOL(de_sign)
deg_free <- (n_rows - n_cols)
all.equal(deg_free, model_sum$df[2])
# Variance of residuals
var_resid <- sum(residual_s^2)/deg_free

# Inverse of design matrix squared
design_2 <- MASS::ginv(crossprod(de_sign))
# design_2 <- t(de_sign) %*% de_sign
# Variance of residuals
var_resid <- sum(residual_s^2)/deg_free
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
  sampl_e <- sample.int(n_rows, replace=TRUE)
  mod_el <- lm(for_mula, data=re_turns[sampl_e, ])
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
std_dev <- drop(sqrt(new_datav %*% beta_covar %*% t(new_datav)))

# Create formula from text string
for_mula <- paste0("res_ponse ~ ",
  paste(colnames(de_sign), collapse=" + "), " - 1")
# Specify multivariate regression using formula
mod_el <- lm(for_mula, data=data.frame(cbind(res_ponse, de_sign)))
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
r_ss <- sum(residual_s^2)
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
r_squared_adj <- (1-sum(residual_s^2)/deg_free/var(res_ponse))
# Compare adjusted R-squared from lm()
all.equal(drop(r_squared_adj), model_sum$adj.r.squared)

x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
# Plot three curves in loop
deg_free <- c(3, 5, 9)  # Degrees of freedom
col_ors <- c("black", "red", "blue", "green")
for (in_dex in 1:NROW(deg_free)) {
curve(expr=df(x, df1=deg_free[in_dex], df2=3),
xlim=c(0, 4), xlab="", ylab="", lwd=2,
col=col_ors[in_dex], add=as.logical(in_dex-1))
}  # end for

# Add title
title(main="F-Distributions", line=0.5)
# Add legend
lab_els <- paste("df", deg_free, sep="=")
legend("topright", inset=0.05, title="degrees of freedom",
       lab_els, cex=0.8, lwd=2, lty=1, col=col_ors)

sigma_x <- var(rnorm(n_rows))
sigma_y <- var(rnorm(n_rows))
f_ratio <- sigma_x/sigma_y
# Cumulative probability for q = f_ratio
pf(f_ratio, n_rows-1, n_rows-1)
# p-value for f_ratios
1-pf((10:20)/10, n_rows-1, n_rows-1)

# F-statistic from lm()
model_sum$fstatistic
# Degrees of freedom of residuals
deg_free <- (n_rows - n_cols)
# F-statistic from ESS and RSS
f_stat <- (e_ss/(n_cols-1))/(r_ss/deg_free)
all.equal(f_stat, model_sum$fstatistic[1], check.attributes=FALSE)
# p-value of F-statistic
1-pf(q=f_stat, df1=n_cols-1, df2=n_rows-n_cols)

# Calculate ETF returns
re_turns <- na.omit(rutils::etf_env$re_turns)
# Perform singular value decomposition
s_vd <- svd(re_turns)
x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
barplot(s_vd$d, main="Singular Values of ETF Returns")
# Calculate generalized inverse from SVD
in_verse <- s_vd$v %*% (t(s_vd$u) / s_vd$d)
# Verify inverse property of in_verse
all.equal(zoo::coredata(re_turns), re_turns %*% in_verse %*% re_turns)
# Calculate regularized inverse from SVD
eigen_max <- 1:3
inv_reg <- s_vd$v[, eigen_max] %*%
  (t(s_vd$u[, eigen_max]) / s_vd$d[eigen_max])
# Calculate regularized inverse using RcppArmadillo
rcpp_inverse <- HighFreq::calc_inv(re_turns, eigen_max=3)
all.equal(inv_reg, rcpp_inverse, check.attributes=FALSE)
# Calculate regularized inverse from Moore-Penrose pseudo-inverse
square_d <- t(re_turns) %*% re_turns
ei_gen <- eigen(square_d)
squared_inv <- ei_gen$vectors[, eigen_max] %*%
  (t(ei_gen$vectors[, eigen_max]) / ei_gen$values[eigen_max])
mp_inverse <- squared_inv %*% t(re_turns)
all.equal(inv_reg, mp_inverse, check.attributes=FALSE)

# Define transformation matrix
trans_mat <- matrix(runif(n_cols^2, min=(-1), max=1), ncol=n_cols)
# Calculate linear combinations of design columns
design_trans <- de_sign %*% trans_mat
# Calculate the influence matrix
influence_trans <- design_trans %*% MASS::ginv(design_trans)
# Compare the influence matrices
all.equal(influ_ence, influence_trans)

lamb_da <- c(0.5, 1, 1.5)
col_ors <- c("red", "blue", "green")
# Plot three curves in loop
for (in_dex in 1:3) {
  curve(expr=plogis(x, scale=lamb_da[in_dex]),
xlim=c(-4, 4), type="l", xlab="", ylab="", lwd=4,
col=col_ors[in_dex], add=(in_dex>1))
}  # end for

# Add title
title(main="Logistic function", line=0.5)
# Add legend
legend("topleft", title="Scale parameters",
       paste("lambda", lamb_da, sep="="),
       inset=0.05, cex=0.8, lwd=6, bty="n",
       lty=1, col=col_ors)

set.seed(1121)  # Reset random number generator
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

x11(width=6, height=5)
par(mar=c(3, 3, 2, 2), mgp=c(2, 1, 0), oma=c(0, 0, 0, 0))
or_der <- order(predic_tor)
plot(x=predic_tor[or_der], y=g_lm$fitted.values[or_der],
     main="Category Densities and Logistic Function",
     type="l", lwd=4, col="orange", xlab="score", ylab="density")
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

# Likelihood function of binomial distribution
likeli_hood <- function(prob, b) {
  b*log(prob) + (1-b)*log(1-prob)
}  # end likeli_hood
likeli_hood(prob=0.25, b=1)
# Plot binomial likelihood function
curve(expr=likeli_hood(x, b=1), xlim=c(0, 1), lwd=3,
      xlab="prob", ylab="likelihood", col="blue",
      main="Binomial Likelihood Function")
curve(expr=likeli_hood(x, b=0), lwd=3, col="red", add=TRUE)
legend(x="top", bty="n", legend=c("b = 1", "b = 0"),
       title=NULL, inset=0.3, cex=1.0, lwd=6,
       lty=1, col=c("blue", "red"))

# Specify design matrix
de_sign=cbind(intercept=rep(1, NROW(res_ponse)), predic_tor)
# Likelihood function of the logistic model
likeli_hood <- function(coeff, res_ponse, de_sign) {
  probs <- plogis(drop(de_sign %*% coeff))
  -sum(res_ponse*log(probs) + (1-res_ponse)*log((1-probs)))
}  # end likeli_hood
# Run likelihood function

# Rastrigin function with vector argument for optimization
rastri_gin <- function(vec_tor, pa_ram=25) {
  sum(vec_tor^2 - pa_ram*cos(vec_tor))
}  # end rastri_gin
vec_tor <- c(pi/6, pi/6)
rastri_gin(vec_tor=vec_tor)
# Draw 3d surface plot of Rastrigin function
options(rgl.useNULL=TRUE); library(rgl)
rgl::persp3d(
  x=Vectorize(function(x, y) rastri_gin(vec_tor=c(x, y))),
  xlim=c(-10, 10), ylim=c(-10, 10),
  col="green", axes=FALSE, zlab="", main="rastri_gin")
# Optimize with respect to vector argument
op_tim <- optim(par=vec_tor, fn=rastri_gin,
        method="L-BFGS-B",
        upper=c(4*pi, 4*pi),
        lower=c(pi/2, pi/2),
        pa_ram=1)
# Optimal parameters and value
op_tim$par
op_tim$value
rastri_gin(op_tim$par, pa_ram=1)

# Initial parameters
par_init <- c(1, 1)
# Find max likelihood parameters using steepest descent optimizer
optim_fit <- optim(par=par_init,
           fn=likeli_hood, # Log-likelihood function
           method="L-BFGS-B", # Quasi-Newton method
           res_ponse=res_ponse,
           de_sign=de_sign,
           upper=c(20, 20), # Upper constraint
           lower=c(-20, -20), # Lower constraint
           hessian=TRUE)
# Optimal logistic parameters
optim_fit$par
unname(g_lm$coefficients)
# Standard errors of parameters
sqrt(diag(solve(optim_fit$hessian)))
model_sum <- summary(g_lm)
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
x_lim <- range(balance); y_lim <- range(income)
plot(income ~ balance,
     main="Default Dataset from Package ISLR",
     xlim=x_lim, ylim=y_lim, pch=4, col="blue",
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
g_lm <- glm(de_fault ~ balance, family=binomial(logit))
class(g_lm)
summary(g_lm)

x11(width=6, height=5)
par(mar=c(4, 4, 2, 2), oma=c(0, 0, 0, 0), mgp=c(2.5, 1, 0))
plot(x=balance, y=de_fault,
     main="Logistic Regression of Credit Defaults",
     col="orange", xlab="credit balance", ylab="defaults")
or_der <- order(balance)
lines(x=balance[or_der], y=g_lm$fitted.values[or_der], col="blue", lwd=3)
legend(x="topleft", inset=0.1, bty="n", lwd=6,
 legend=c("defaults", "logit fitted values"),
 col=c("orange", "blue"), lty=c(NA, 1), pch=c(1, NA))

# Calculate cumulative defaults
to_tal <- sum(de_fault)
default_s <- sapply(balance, function(lim_it) {
    sum(de_fault[balance <= lim_it])
})  # end sapply
# Perform logit regression
g_lm <- glm(cbind(default_s, to_tal-default_s) ~ balance,
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
for_mula
g_lm <- glm(for_mula, data=Default, family=binomial(logit))
summary(g_lm)

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
or_der <- order(balance)
plot(x=balance[or_der], y=cum_defaults[or_der, 1],
     col="red", t="l", lwd=2, xlab="credit balance", ylab="",
     main="Cumulative defaults of\n students and non-students")
lines(x=balance[or_der], y=cum_defaults[or_der, 2], col="blue", lwd=2)
legend(x="topleft", bty="n",
 legend=c("students", "non-students"),
 col=c("red", "blue"), text.col=c("red", "blue"), lwd=3)
# Balance boxplot for student factor
boxplot(formula=balance ~ !stu_dent,
  col="lightgrey", main="balance", xlab="Student")

# Perform in-sample forecast from logistic regression model
forecast_s <- predict(g_lm, type="response")
all.equal(g_lm$fitted.values, forecast_s)
# Define discrimination threshold value
thresh_old <- 0.7
# Calculate confusion matrix in-sample
table(actual=!de_fault, forecast=(forecast_s < thresh_old))
# Fit logistic regression over training data
set.seed(1121)  # Reset random number generator
n_rows <- NROW(Default)
sampl_e <- sample.int(n=n_rows, size=n_rows/2)
train_data <- Default[sampl_e, ]
g_lm <- glm(for_mula, data=train_data, family=binomial(logit))
# Forecast over test data out-of-sample
test_data <- Default[-sampl_e, ]
forecast_s <- predict(g_lm, newdata=test_data, type="response")
# Calculate confusion matrix out-of-sample
table(actual=!test_data$de_fault, 
forecast=(forecast_s < thresh_old))

# Calculate confusion matrix out-of-sample
confu_sion <- table(actual=!test_data$de_fault, 
forecast=(forecast_s < thresh_old))
confu_sion
# Calculate FALSE positive (type I error)
sum(!test_data$de_fault & (forecast_s > thresh_old))
# Calculate FALSE negative (type II error)
sum(test_data$de_fault & (forecast_s < thresh_old))

# Calculate FALSE positive and FALSE negative rates
confu_sion <- confu_sion / rowSums(confu_sion)
c(typeI=confu_sion[2, 1], typeII=confu_sion[1, 2])
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

# Confusion matrix as function of thresh_old
con_fuse <- function(actu_al, forecast_s, thresh_old) {
    confu_sion <- table(actu_al, (forecast_s < thresh_old))
    confu_sion <- confu_sion / rowSums(confu_sion)
    c(typeI=confu_sion[2, 1], typeII=confu_sion[1, 2])
  }  # end con_fuse
con_fuse(!test_data$de_fault, forecast_s, thresh_old=thresh_old)
# Define vector of discrimination thresholds
threshold_s <- seq(0.05, 0.95, by=0.05)^2
# Calculate error rates
error_rates <- sapply(threshold_s, con_fuse,
  actu_al=!test_data$de_fault, forecast_s=forecast_s)  # end sapply
error_rates <- t(error_rates)
rownames(error_rates) <- threshold_s
error_rates <- rbind(c(1, 0), error_rates)
error_rates <- rbind(error_rates, c(0, 1))
# Calculate area under ROC curve (AUC)
true_pos <- (1 - error_rates[, "typeII"])
true_pos <- (true_pos + rutils::lag_it(true_pos))/2
false_pos <- rutils::diff_it(error_rates[, "typeI"])
abs(sum(true_pos*false_pos))

# Plot ROC Curve for Defaults
x11(width=5, height=5)
plot(x=error_rates[, "typeI"], y=1-error_rates[, "typeII"],
     xlab="FALSE positive rate", ylab="TRUE positive rate",
     main="ROC Curve for Defaults", type="l", lwd=3, col="blue")
abline(a=0.0, b=1.0, lwd=3, col="orange")
