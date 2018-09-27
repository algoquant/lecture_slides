set.seed(1121)  # reset random number generator
runif(3)  # three numbers from uniform distribution
runif(3)  # produce another three numbers
set.seed(1121)  # reset random number generator
runif(3)  # produce another three numbers

# produce random number from standard normal distribution
rnorm(1)
# produce five random numbers from standard normal distribution
rnorm(5)
# produce five random numbers from the normal distribution
rnorm(n=5, mean=1, sd=2)  # match arguments by name
# calculate cumulative standard normal distribution
c(pnorm(-2), pnorm(2))
# calculate inverse cumulative standard normal distribution
c(qnorm(0.75), qnorm(0.25))
# define logistic map function
log_map <- function(x, r=4) r*x*(1-x)
log_map(0.25, 4)
# plot logistic map
x11(width=6, height=5)
curve(expr=log_map, type="l", xlim=c(0, 1),
xlab="x[n-1]", ylab="x[n]", lwd=2, col="blue",
main="logistic map")
lines(x=c(0, 0.25), y=c(0.75, 0.75), lwd=2, col="orange")
lines(x=c(0.25, 0.25), y=c(0, 0.75), lwd=2, col="orange")
# calculate uniformly distributed pseudo-random
# sequence using logistic map function
uni_form <- function(see_d, len_gth=10) {
  # pre-allocate vector instead of "growing" it
  out_put <- numeric(len_gth)
  # initialize
  out_put[1] <- see_d
  # perform loop
  for (i in 2:len_gth) {
    out_put[i] <- 4*out_put[i-1]*(1-out_put[i-1])
  }  # end for
  acos(1-2*out_put)/pi
}  # end uni_form
uni_form(see_d=0.1, len_gth=15)
plot(
  density(uni_form(see_d=runif(1), len_gth=1e5)),
  xlab="", ylab="", lwd=2, col="blue",
  main="uniform pseudo-random number density")
set.seed(1121)  # reset random number generator
# flip unbiased coin once, 20 times
rbinom(n=20, size=1, 0.5)
# number of heads after flipping twice, 20 times
rbinom(n=20, size=2, 0.5)
# number of heads after flipping thrice, 20 times
rbinom(n=20, size=3, 0.5)
# number of heads after flipping biased coin thrice, 20 times
rbinom(n=20, size=3, 0.8)
# number of heads after flipping biased coin thrice, 20 times
rbinom(n=20, size=3, 0.2)
# flip unbiased coin once, 20 times
sample(x=0:1, size=20, replace=TRUE)  # fast
as.numeric(runif(20) < 0.5)  # slower
# permutation of five numbers
sample(x=5)
# permutation of four strings
sample(x=c("apple", "grape", "orange", "peach"))
# sample of size three
sample(x=5, size=3)
# sample with replacement
sample(x=5, replace=TRUE)
sample(  # sample of strings
  x=c("apple", "grape", "orange", "peach"),
  size=12,
  replace=TRUE)
# binomial sample: flip coin once, 20 times
sample(x=0:1, size=20, replace=TRUE)
# flip unbiased coin once, 20 times
as.numeric(runif(20) > 0.5)  # slower
rm(list=ls())
set.seed(1121)  # reset random number generator
# sample from Standard Normal Distribution
sam_ple <- rnorm(1000)

mean(sam_ple)  # sample mean

median(sam_ple)  # sample median

sd(sam_ple)  # sample standard deviation
rm(list=ls())
# DAX returns
re_turns <- diff(log(EuStockMarkets[, 1]))
# number of observations
len_gth <- NROW(re_turns)
# mean of DAX returns
mean_rets <- mean(re_turns)
# standard deviation of DAX returns
sd_rets <- sd(re_turns)
# skew of DAX returns
len_gth/((len_gth-1)*(len_gth-2))*
  sum(((re_turns - mean_rets)/sd_rets)^3)
# kurtosis of DAX returns
len_gth*(len_gth+1)/((len_gth-1)^3)*
  sum(((re_turns - mean_rets)/sd_rets)^4)
# random normal returns
re_turns <- rnorm(len_gth, sd=2)
# mean and standard deviation of random normal returns
mean_rets <- mean(re_turns)
sd_rets <- sd(re_turns)
# skew of random normal returns
len_gth/((len_gth-1)*(len_gth-2))*
  sum(((re_turns - mean_rets)/sd_rets)^3)
# kurtosis of random normal returns
len_gth*(len_gth+1)/((len_gth-1)^3)*
  sum(((re_turns - mean_rets)/sd_rets)^4)
set.seed(1121)  # reset random number generator
# sample from Standard Normal Distribution
len_gth <- 1000
sam_ple <- rnorm(len_gth)
# sample mean - MC estimate
mean(sam_ple)
# sample standard deviation - MC estimate
sd(sam_ple)
# Monte Carlo estimate of cumulative probability
sam_ple <- sort(sam_ple)
pnorm(1)
sum(sam_ple<1)/len_gth
# Monte Carlo estimate of quantile
conf_level <- 0.99
qnorm(conf_level)
sam_ple[conf_level*len_gth]
quantile(sam_ple, probs=conf_level)
# analyze the source code of quantile()
stats:::quantile.default
# microbenchmark quantile
library(microbenchmark)
summary(microbenchmark(
  sam_ple=sam_ple[cut_off],
  quan_tile=quantile(sam_ple, probs=conf_level),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
set.seed(1121)  # reset random number generator
# sample from Standard Normal Distribution
len_gth <- 1000
sam_ple <- rnorm(len_gth)
# sample mean
mean(sam_ple)
# sample standard deviation
sd(sam_ple)
#Perform two-tailed test that sample is
#from Standard Normal Distribution (mean=0, SD=1)
# generate vector of samples and store in data frame
test_frame <- data.frame(samples=rnorm(1e4))
# get p-values for all the samples
test_frame$p_values <- sapply(test_frame$samples, 
        function(x) 2*pnorm(-abs(x)))
# significance level, two-tailed test, critical value=2*SD
signif_level <- 2*(1-pnorm(2))
# compare p_values to significance level
test_frame$result <-
  test_frame$p_values > signif_level
# number of null rejections
sum(!test_frame$result) / NROW(test_frame)
# show null rejections
head(test_frame[!test_frame$result, ])
x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
# plot the Normal probability distribution
curve(expr=dnorm(x, sd=1), type="l", xlim=c(-4, 4),
xlab="", ylab="", lwd=3, col="blue")
title(main="Two-tailed Test", line=0.5)
# plot tails of the distribution using polygons
star_t <- 2; e_nd <- 4
# plot right tail using polygon
x_var <- seq(star_t, e_nd, length=100)
y_var <- dnorm(x_var, sd=1)
y_var[1] <- (-1)
y_var[NROW(y_var)] <- (-1)
polygon(x=x_var, y=y_var, col="red")
# plot left tail using polygon
y_var <- dnorm(-x_var, sd=1)
y_var[1] <- (-1)
y_var[NROW(y_var)] <- (-1)
polygon(x=(-x_var), y=y_var, col="red")
rm(list=ls())
par(oma=c(1, 1, 1, 1), mgp=c(2, 0.5, 0), mar=c(5, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
library(ggplot2)  # load ggplot2

qplot(  # simple ggplot2
    main="Standard Normal Distribution",
    c(-4, 4),
    stat="function",
    fun=dnorm,
    geom="line",
    xlab=NULL, ylab=NULL
    ) +  # end qplot

theme(  # modify plot theme
    plot.title=element_text(vjust=-1.0),
    plot.background=element_blank()
    ) +  # end theme

geom_vline(  # add vertical line
  aes(xintercept=c(-2.0, 2.0)),
  colour="red",
  linetype="dashed"
  )  # end geom_vline
rm(list=ls())
par(oma=c(1, 1, 1, 1), mgp=c(2, 0.5, 0), mar=c(5, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
#create ggplot2 with shaded area
x_var <- -400:400/100
norm_frame <- data.frame(x_var=x_var,
                 d.norm=dnorm(x_var))
norm_frame$shade <- ifelse(
            abs(norm_frame$x_var) >= 2,
            norm_frame$d.norm, NA)
ggplot(  # main function
  data=norm_frame,
  mapping=aes(x=x_var, y=d.norm)
  ) +  # end ggplot
# plot line
  geom_line() +
# plot shaded area
  geom_ribbon(aes(ymin=0, ymax=shade), fill="red") +
# no axis labels
  xlab("") + ylab("") +
# add title
  ggtitle("Standard Normal Distribution") +
# modify plot theme
  theme(
  plot.title=element_text(vjust=-1.0),
  plot.background=element_blank()
  )  # end theme
# t-test for single sample
t.test(rnorm(100))
# t-test for two samples
t.test(rnorm(100),
       rnorm(100, mean=1))
# Wilcoxon test for normal distribution
wilcox.test(rnorm(100))
# Wilcoxon test for two normal distributions
wilcox.test(rnorm(100), rnorm(100, mean=0.1))
# Wilcoxon test for two normal distributions
wilcox.test(rnorm(100), rnorm(100, mean=1.0))
# Wilcoxon test for a uniform versus normal distribution
wilcox.test(runif(100), rnorm(100))
# KS test for normal distribution
ks.test(rnorm(100), pnorm)
# KS test for uniform distribution
ks.test(runif(100), pnorm)
# KS test for two similar normal distributions
ks.test(rnorm(100), rnorm(100, mean=0.1))
# KS test for two different normal distributions
ks.test(rnorm(100), rnorm(100, mean=1.0))
# calculate DAX percentage returns
dax_rets <- diff(log(EuStockMarkets[, 1]))

# Shapiro-Wilk test for normal distribution
shapiro.test(rnorm(NROW(dax_rets)))

# Shapiro-Wilk test for DAX returns
shapiro.test(dax_rets)

# Shapiro-Wilk test for uniform distribution
shapiro.test(runif(NROW(dax_rets)))
dax_rets <- diff(log(EuStockMarkets[, 1]))
library(tseries)  # load package tseries

# Jarque-Bera test for normal distribution
jarque.bera.test(rnorm(NROW(dax_rets)))

# Jarque-Bera test for DAX returns
jarque.bera.test(dax_rets)

# Jarque-Bera test for uniform distribution
jarque.bera.test(runif(NROW(dax_rets)))
# formula of linear model with zero intercept
for_mula <- z ~ x + y - 1
for_mula

# collapse vector of strings into single text string
paste0("x", 1:5)
paste(paste0("x", 1:5), collapse="+")

# create formula from text string
for_mula <- as.formula(
  # coerce text strings to formula
  paste("z ~ ",
  paste(paste0("x", 1:5), collapse="+")
  )  # end paste
)  # end as.formula
class(for_mula)
for_mula
# modify the formula using "update"
update(for_mula, log(.) ~ . + beta)
set.seed(1121)  # initialize random number generator
# define explanatory (design) variable
len_gth <- 100
de_sign <- runif(len_gth)
noise <- rnorm(len_gth)
# response equals linear form plus random noise
res_ponse <- (1 + de_sign + noise)
# calculate de-meaned explanatory (design) and response vectors
design_zm <- de_sign - mean(de_sign)
response_zm <- res_ponse - mean(res_ponse)
# solve for the regression beta
be_ta <- sum(design_zm*response_zm) / sum(design_zm^2)
# solve for the regression alpha
al_pha <- mean(res_ponse) - be_ta*mean(de_sign)
# specify regression formula
for_mula <- res_ponse ~ de_sign
mod_el <- lm(for_mula)  # perform regression
class(mod_el)  # regressions have class lm
attributes(mod_el)
eval(mod_el$call$formula)  # regression formula
mod_el$coeff  # regression coefficients
all.equal(coef(mod_el), c(al_pha, be_ta), 
  check.attributes=FALSE)
x11(width=5, height=4)  # open x11 for plotting
# set plot parameters to reduce whitespace around plot
par(mar=c(5, 5, 2, 1), oma=c(0, 0, 0, 0))
# plot scatterplot using formula
plot(for_mula, xlab="design", ylab="response")
title(main="Simple Regression", line=0.5)
# add regression line
abline(mod_el, lwd=3, col="blue")
# plot fitted (predicted) response values
points(x=de_sign, y=mod_el$fitted.values,
       pch=16, col="blue")
# plot response without noise
lines(x=de_sign, y=(res_ponse-noise),
      col="red", lwd=3)
legend(x="topleft", # add legend
       legend=c("response without noise", "fitted values"),
       title=NULL, inset=0.08, cex=0.8, lwd=6,
       lty=1, col=c("red", "blue"))
# sum of residuals = 0
sum(mod_el$residuals)
x11(width=6, height=5)  # open x11 for plotting
# set plot parameters to reduce whitespace around plot
par(mar=c(5, 5, 1, 1), oma=c(0, 0, 0, 0))
# extract residuals
resi_duals <- cbind(de_sign, mod_el$residuals)
colnames(resi_duals) <- c("design", "residuals")
# plot residuals
plot(resi_duals)
title(main="Residuals of the Linear Regression", line=-1)
abline(h=0, lwd=3, col="red")
model_sum <- summary(mod_el)  # copy regression summary
model_sum  # print the summary to console
attributes(model_sum)$names  # get summary elements
model_sum$coeff
model_sum$r.squared
model_sum$adj.r.squared
model_sum$fstatistic
# standard error of beta
model_sum$
  coefficients["de_sign", "Std. Error"]
sd(model_sum$residuals)/sd(de_sign)/
  sqrt(unname(model_sum$fstatistic[3]))
anova(mod_el)
set.seed(1121)  # initialize random number generator
# high noise compared to coefficient
res_ponse <- (1 + de_sign + rnorm(30, sd=8))
mod_el <- lm(for_mula)  # perform regression
# values of regression coefficients are not
# statistically significant
summary(mod_el)
par(oma=c(1, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=1.0, cex.axis=1.0, cex.main=1.0, cex.sub=1.0)
reg_stats <- function(std_dev) {  # noisy regression
  set.seed(1121)  # initialize number generator
# define explanatory (design) and response variables
  de_sign <- rnorm(100, mean=2)
  res_ponse <- (1 + 0.2*de_sign +
    rnorm(NROW(de_sign), sd=std_dev))
# specify regression formula
  for_mula <- res_ponse ~ de_sign
# perform regression and get summary
  model_sum <- summary(lm(for_mula))
# extract regression statistics
  with(model_sum, c(pval=coefficients[2, 4],
   adj_rsquared=adj.r.squared,
   fstat=fstatistic[1]))
}  # end reg_stats
# apply reg_stats() to vector of std dev values
vec_sd <- seq(from=0.1, to=0.5, by=0.1)
names(vec_sd) <- paste0("sd=", vec_sd)
mat_stats <- t(sapply(vec_sd, reg_stats))
# plot in loop
par(mfrow=c(NCOL(mat_stats), 1))
for (in_dex in 1:NCOL(mat_stats)) {
  plot(mat_stats[, in_dex], type="l",
 xaxt="n", xlab="", ylab="", main="")
  title(main=colnames(mat_stats)[in_dex], line=-1.0)
  axis(1, at=1:(NROW(mat_stats)),
 labels=rownames(mat_stats))
}  # end for
reg_stats <- function(da_ta) {  # get regression
# perform regression and get summary
  col_names <- colnames(da_ta)
  for_mula <-
    paste(col_names[2], col_names[1], sep="~")
  model_sum <- summary(lm(for_mula,
                        data=da_ta))
# extract regression statistics
  with(model_sum, c(pval=coefficients[2, 4],
   adj_rsquared=adj.r.squared,
   fstat=fstatistic[1]))
}  # end reg_stats
# apply reg_stats() to vector of std dev values
vec_sd <- seq(from=0.1, to=0.5, by=0.1)
names(vec_sd) <- paste0("sd=", vec_sd)
mat_stats <-
  t(sapply(vec_sd, function (std_dev) {
    set.seed(1121)  # initialize number generator
# define explanatory (design) and response variables
    de_sign <- rnorm(100, mean=2)
    res_ponse <- (1 + 0.2*de_sign +
rnorm(NROW(de_sign), sd=std_dev))
    reg_stats(data.frame(de_sign, res_ponse))
    }))
# plot in loop
par(mfrow=c(NCOL(mat_stats), 1))
for (in_dex in 1:NCOL(mat_stats)) {
  plot(mat_stats[, in_dex], type="l",
 xaxt="n", xlab="", ylab="", main="")
  title(main=colnames(mat_stats)[in_dex], line=-1.0)
  axis(1, at=1:(NROW(mat_stats)),
 labels=rownames(mat_stats))
}  # end for
# set plot paramaters - margins and font scale
par(oma=c(1,0,1,0), mgp=c(2,1,0), mar=c(2,1,2,1), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2, 2))  # plot 2x2 panels
plot(mod_el)  # plot diagnostic scatterplots
plot(mod_el, which=2)  # plot just Q-Q
library(lmtest)  # load lmtest
# perform Durbin-Watson test
dwtest(mod_el)
foo <- etf_env$re_turns[, c("VTI", "VEU")]
end_points <- endpoints(foo, on="months")
head(foo)
tail(foo)
class(foo)
dim(foo)
mod_el <- lm(paste(names(foo), collapse=" ~ "), data=foo)
model_sum <- summary(mod_el)
model_sum
dwtest(mod_el)

# filter over non-overlapping periods
bar <- names(foo)
foo <- merge(period.sum(foo[, 1], INDEX=end_points), period.sum(foo[, 2], INDEX=end_points))
foo <- foo[complete.cases(foo), ]
names(foo) <- bar

# filter over overlapping periods
foo <- rollsum(foo, k=11)


set.seed(1121)
library(lmtest)
# spurious regression in unit root time series
de_sign <- cumsum(rnorm(100))  # unit root time series
res_ponse <- cumsum(rnorm(100))
for_mula <- res_ponse ~ de_sign
mod_el <- lm(for_mula)  # perform regression
# summary indicates statistically significant regression
model_sum <- summary(mod_el)
model_sum$coeff
model_sum$r.squared
# Durbin-Watson test shows residuals are autocorrelated
dw_test <- dwtest(mod_el)
c(dw_test$statistic[[1]], dw_test$p.value)
par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # set plot panels
plot(for_mula, xlab="", ylab="")  # plot scatterplot using formula
title(main="Spurious Regression", line=-1)
# add regression line
abline(mod_el, lwd=2, col="red")
plot(mod_el, which=2, ask=FALSE)  # plot just Q-Q
set.seed(1121)  # initialize random number generator
# define design matrix
len_gth <- 100
n_var <- 5
de_sign <- sapply(1:n_var, function(col_umn) {
  sin(pi*col_umn*((1:len_gth)-(len_gth+1)/2)/len_gth)
})  # end sapply
# add column names
colnames(de_sign) <- paste0("predict", 1:n_var)
# plot design matrix
# matplot(de_sign, type="l", lty="solid", lwd=3)
# define the design weights
weight_s <- runif(n_var, min=(-10), max=10)
# response equals linear form plus random noise
noise <- rnorm(len_gth, sd=0.1)
res_ponse <- (-1 + de_sign %*% weight_s + noise)
# calculate de-meaned design matrix
design_zm <- t(t(de_sign) - colMeans(de_sign))
# or
# design_zm <- apply(design_zm, 2, function(x) (x-mean(x)))
# calculate de-meaned response vector
response_zm <- res_ponse - mean(res_ponse)
# calculate the regression coefficients
beta_s <- MASS::ginv(design_zm) %*% response_zm
# solve for the regression alpha
al_pha <- mean(res_ponse) - 
  sum(colSums(de_sign)*drop(beta_s))/len_gth
# perform multivariate regression using lm()
mod_el <- lm(res_ponse ~ de_sign)
# compare with coefficients from lm()
all.equal(coef(mod_el), c(al_pha, beta_s), check.attributes=FALSE)
# compare with actual coefficients
all.equal(c(-1, weight_s), c(al_pha, beta_s), check.attributes=FALSE)
# calculate fitted values from regression coefficients
fit_ted <- drop(al_pha + de_sign %*% beta_s)
all.equal(fit_ted, mod_el$fitted.values, check.attributes=FALSE)
# calculate fitted values from zero mean data
fit_ted <- drop(mean(res_ponse) + design_zm %*% beta_s)
all.equal(fit_ted, mod_el$fitted.values, check.attributes=FALSE)
# calculate the residuals
resid_uals <- drop(res_ponse - fit_ted)
all.equal(resid_uals, mod_el$residuals, check.attributes=FALSE)
# the residuals have zero mean
all.equal(sum(resid_uals), target=0)
# the residuals are orthogonal to the predictors
sapply(resid_uals %*% de_sign, 
       all.equal, target=0)
# the residuals are orthogonal to the fitted values
all.equal(sum(resid_uals*fit_ted), target=0)
# calculate zero mean fitted values
fitted_zm <- drop(design_zm %*% beta_s)
all.equal(fitted_zm+mean(res_ponse), 
  mod_el$fitted.values, check.attributes=FALSE)
# calculate the residuals
resid_uals <- drop(response_zm - fitted_zm)
all.equal(resid_uals, mod_el$residuals, check.attributes=FALSE)
# add intercept column to design matrix
de_sign <- cbind(rep(1, NROW(de_sign)), de_sign)
# add column name
colnames(de_sign)[1] <- "intercept"
# calculate generalized inverse of the design matrix
design_inv <- MASS::ginv(de_sign)
# add define design weight for intercept
weight_s <- c(-1, weight_s)
# response equals linear form plus random noise
# noise <- rnorm(len_gth, sd=0.1)
res_ponse <- de_sign %*% weight_s + noise
# calculate the regression coefficients
beta_s <- design_inv %*% res_ponse
# perform multivariate regression without intercept term
mod_el <- lm(res_ponse ~ de_sign - 1)
all.equal(drop(beta_s), coef(mod_el), check.attributes=FALSE)
# calculate the influence matrix
influ_ence <- de_sign %*% design_inv
# calculate fitted values using influence matrix
fit_ted <- drop(influ_ence %*% res_ponse)
all.equal(fit_ted, mod_el$fitted.values, check.attributes=FALSE)
# calculate fitted values from regression coefficients
fit_ted <- drop(de_sign %*% beta_s)
all.equal(fit_ted, mod_el$fitted.values, check.attributes=FALSE)
# calculate the residuals
resid_uals <- drop(res_ponse - fit_ted)
all.equal(resid_uals, mod_el$residuals, check.attributes=FALSE)
# define transformation matrix
n_col <- NCOL(de_sign)
trans_mat <- matrix(runif(n_col^2, min=(-1), max=1), 
            ncol=n_col)
# calculate linear combinations of design columns
design_trans <- de_sign %*% trans_mat
# calculate the influence matrix
influence_trans <- design_trans %*% MASS::ginv(design_trans)
# compare the influence matrices
all.equal(influ_ence, influence_trans)
# de-mean the design matrix columns
design_trans <- cbind(de_sign[, 1], t(t(de_sign[, -1])-colMeans(de_sign[, -1])))
round(apply(design_trans, 2, mean), 3)
# calculate the influence matrix
influence_trans <- design_trans %*% MASS::ginv(design_trans)
# compare the influence matrices
all.equal(influ_ence, influence_trans)
# regression model summary
model_sum <- summary(mod_el)
# degrees of freedom of residuals
deg_free <- (len_gth - NCOL(de_sign))
all.equal(deg_free, model_sum$df[2])
# variance of residuals
resid_var <- sum(resid_uals^2)/deg_free
# design matrix squared
design_2 <- crossprod(de_sign)
# design_2 <- t(de_sign) %*% de_sign
# calculate covariance matrix of betas
beta_covar <- resid_var*MASS::ginv(design_2)
# round(beta_covar, 3)
beta_sd <- sqrt(diag(beta_covar))
all.equal(beta_sd, model_sum$coeff[, 2], check.attributes=FALSE)
# calculate t-values of betas
beta_tvals <- drop(beta_s)/beta_sd
all.equal(beta_tvals, model_sum$coeff[, 3], check.attributes=FALSE)
# calculate two-sided p-values of betas
beta_pvals <- 2*pt(-abs(beta_tvals), df=deg_free)
all.equal(beta_pvals, model_sum$coeff[, 4], check.attributes=FALSE)
# calculate the influence matrix
influ_ence <- de_sign %*% design_inv
all.equal(influ_ence, influ_ence %*% influ_ence)
# calculate covariance matrix of fitted values
fit_covar <- resid_var*influ_ence
# calculate standard deviations of the fitted values
fit_sd <- sqrt(diag(fit_covar))
# plot the standard deviations
plot(fit_sd, type="l", lwd=3, col="blue", ylab="",
     main="Standard Deviations of Fitted Values")
x11(width=5, height=4)  # open x11 for plotting
# set plot parameters to reduce whitespace around plot
par(mar=c(5, 5, 2, 1), oma=c(0, 0, 0, 0))
# univariate regression with linear predictor
de_sign <- cbind(rep(1, len_gth), 1:len_gth/len_gth)
# calculate generalized inverse of the design matrix
design_inv <- MASS::ginv(de_sign)
# calculate the influence matrix
influ_ence <- de_sign %*% design_inv
# plot the leverage vector
plot(x=de_sign[,2], y=diag(influ_ence),
     type="l", lwd=3, col="blue",
     xlab="predictor", ylab="leverage",
     main="Leverage as Function of Predictor")
# define the design weights
weight_s <- c(-1, 1)
# response without random noise equals weighted sum over columns of de_sign
res_ponse <- de_sign %*% weight_s
# perform loop over different realizations of random noise
fit_ted <- lapply(1:50, function(it) {
  # add random noise to response
  res_ponse <- res_ponse + rnorm(len_gth, sd=1.0)
  # calculate fitted values using influence matrix
  influ_ence %*% res_ponse
})  # end lapply
fit_ted <- rutils::do_call(cbind, fit_ted)
x11(width=5, height=4)  # open x11 for plotting
# set plot parameters to reduce whitespace around plot
par(mar=c(5, 5, 2, 1), oma=c(0, 0, 0, 0))
# plot fitted values
matplot(x=de_sign[,2], y=fit_ted,
type="l", lty="solid", lwd=1, col="blue",
xlab="predictor", ylab="fitted",
main="Fitted Values for Different Realizations
of Random Noise")
lines(x=de_sign[,2], y=res_ponse, col="red", lwd=4)
legend(x="topleft", # add legend
       legend=c("response without noise", "fitted values"),
       title=NULL, inset=0.05, cex=0.8, lwd=6,
       lty=1, col=c("red", "blue"))
# univariate regression with linear predictor
de_sign <- cbind(rep(1, len_gth), 1:len_gth/len_gth)
res_ponse <- de_sign %*% weight_s + rnorm(len_gth, sd=0.3)
design_inv <- MASS::ginv(de_sign)
influ_ence <- de_sign %*% design_inv
beta_s <- design_inv %*% res_ponse
fit_ted <- drop(de_sign %*% beta_s)
resid_uals <- drop(res_ponse - fit_ted)
deg_free <- (len_gth - NCOL(de_sign))
resid_sd <- sqrt(sum(resid_uals^2)/deg_free)
# inverse of design matrix squared
design_2 <- MASS::ginv(crossprod(de_sign))
# perform loop over out-of-sample predictors
new_predictors <- (max(de_sign[, 2]) + 10*(1:5)/len_gth)
predic_tions <- sapply(new_predictors, function(predic_tor) {
  predic_tor <- cbind(1, predic_tor)
  # calculate predicted values
  predic_ted <- predic_tor %*% beta_s
  # calculate standard deviation
  predict_sd <- resid_sd*sqrt(predic_tor %*% design_2 %*% t(predic_tor))
  c(predicted=predic_ted, stddev=predict_sd)
})  # end sapply
predic_tions <- t(predic_tions)
# prepare plot data
x_data <- c(de_sign[,2], new_predictors)
x_lim <- range(x_data)
y_data <- c(fit_ted, predic_tions[, 1])
# calculate t-quantile
t_quant <- qt(pnorm(2), df=deg_free)
predict_low <- predic_tions[, 1]-t_quant*predic_tions[, 2]
predict_high <- predic_tions[, 1]+t_quant*predic_tions[, 2]
y_lim <- range(c(res_ponse, y_data, predict_low, predict_high))
# plot the regression predictions
plot(x=x_data, y=y_data,
     xlim=x_lim, ylim=y_lim,
     type="l", lwd=3, col="blue",
     xlab="predictor", ylab="fitted or predicted",
     main="Predictions from Linear Regression")
points(x=de_sign[,2], y=res_ponse, col="blue")
points(x=new_predictors, y=predic_tions[, 1], pch=16, col="blue")
lines(x=new_predictors, y=predict_high, lwd=3, col="red")
lines(x=new_predictors, y=predict_low, lwd=3, col="green")
legend(x="topleft", # add legend
       legend=c("predictions", "+2SD", "-2SD"),
       title=NULL, inset=0.05, cex=0.8, lwd=6,
       lty=1, col=c("blue", "red", "green"))
# perform regression
predic_tor <- de_sign[, 2]
mod_el <- lm(res_ponse ~ predic_tor)
# perform prediction from regression
new_data <- data.frame(predic_tor=new_predictors)
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
legend(x="topleft", # add legend
       legend=c("predictions", "+2SD", "-2SD"),
       title=NULL, inset=0.05, cex=0.8, lwd=6,
       lty=1, col=c("blue", "red", "green"))
# perform PCA
pc_a <- prcomp(design_zm, 
       center=TRUE, scale=TRUE)
design_pca <- pc_a$x
round(cov(design_pca), 2)
round(apply(design_pca, 2, mean), 3)
round(apply(design_pca, 2, sd), 2)
# calculate the influence matrix
influ_ence <- design_zm %*% MASS::ginv(design_zm)
influence_pca <- design_pca %*% MASS::ginv(design_pca)
all.equal(influ_ence, influence_pca)
# calculate the fitted values
fit_ted <- drop(de_sign %*% beta_s)
all.equal(fit_ted, mod_el$fitted.values, check.attributes=FALSE)
# calculate the residuals
resid_uals <- drop(res_ponse - fit_ted)
all.equal(resid_uals, mod_el$residuals, check.attributes=FALSE)
# residuals are orthogonal to fitted values
all.equal(sum(resid_uals*fit_ted), target=0)
# TSS = ESS + RSS
t_ss <- (len_gth-1)*var(drop(res_ponse))
e_ss <- (len_gth-1)*var(fit_ted)
r_ss <- (len_gth-1)*var(resid_uals)
all.equal(t_ss, e_ss + r_ss)
# regression summary
model_sum <- summary(mod_el)
# regression R-squared
r_squared <- e_ss/t_ss
all.equal(r_squared, model_sum$r.squared)
# correlation between response and fitted values
cor_fitted <- drop(cor(res_ponse, fit_ted))
# squared correlation between response and fitted values
all.equal(cor_fitted^2, r_squared)
x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
deg_free <- c(3, 5, 9)  # df values
col_ors <- c("black", "red", "blue", "green")
lab_els <- paste0("df1=", deg_free, ", df2=3")
for (in_dex in 1:NROW(deg_free)) {  # plot four curves
curve(expr=df(x, df1=deg_free[in_dex], df2=3),
      type="l", xlim=c(0, 4),
      xlab="", ylab="", lwd=2,
      col=col_ors[in_dex],
      add=as.logical(in_dex-1))
}  # end for
# add title
title(main="F-Distributions", line=0.5)
# add legend
legend("topright", inset=0.05, title="degrees of freedom",
       lab_els, cex=0.8, lwd=2, lty=1,
       col=col_ors)
# F-statistic from lm()
model_sum$fstatistic
# degrees of freedom of residuals
deg_free <- len_gth-n_var-1
# F-statistic from RSS
f_stat <- e_ss*deg_free/r_ss/n_var
all.equal(f_stat, model_sum$fstatistic[1], check.attributes=FALSE)
# p-value of F-statistic
1-pf(q=f_stat, df1=len_gth-n_var-1, df2=n_var)
library(lmtest)  # load lmtest
de_sign <- data.frame(  # design matrix
  de_sign=1:30, omit_var=sin(0.2*1:30))
# response depends on both predictors
res_ponse <- with(de_sign,
  0.2*de_sign + omit_var + 0.2*rnorm(30))
# mis-specified regression only one predictor
mod_el <- lm(res_ponse ~ de_sign,
        data=de_sign)
model_sum <- summary(mod_el)
model_sum$coeff
model_sum$r.squared
# Durbin-Watson test shows residuals are autocorrelated
dwtest(mod_el)$p.value
par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # set plot panels
plot(for_mula, data=de_sign)
abline(mod_el, lwd=2, col="red")
title(main="OVB Regression", line=-1)
plot(mod_el, which=2, ask=FALSE)  # plot just Q-Q
set.seed(1121)
library(lmtest)
# spurious regression in unit root time series
de_sign <- cumsum(rnorm(100))  # unit root time series
res_ponse <- cumsum(rnorm(100))
for_mula <- res_ponse ~ de_sign
mod_el <- lm(for_mula)  # perform regression
# summary indicates statistically significant regression
model_sum <- summary(mod_el)
model_sum$coeff
model_sum$r.squared
# Durbin-Watson test shows residuals are autocorrelated
dw_test <- dwtest(mod_el)
c(dw_test$statistic[[1]], dw_test$p.value)
par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # set plot panels
plot(for_mula, xlab="", ylab="")  # plot scatterplot using formula
title(main="Spurious Regression", line=-1)
# add regression line
abline(mod_el, lwd=2, col="red")
plot(mod_el, which=2, ask=FALSE)  # plot just Q-Q
par(oma=c(1, 1, 1, 1), mar=c(2, 1, 1, 1), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
lamb_da <- c(0.5, 1, 1.5)
col_ors <- c("red", "black", "blue")
# plot three curves in loop
for (in_dex in 1:3) {
  curve(expr=plogis(x, scale=lamb_da[in_dex]),
xlim=c(-4, 4), type="l",
xlab="", ylab="", lwd=2,
col=col_ors[in_dex], add=(in_dex>1))
}  # end for
# add title
title(main="Logistic function", line=0.5)
# add legend
legend("topleft", title="Scale parameters",
       paste("lambda", lamb_da, sep="="),
       inset=0.05, cex=0.8, lwd=2,
       lty=c(1, 1, 1), col=col_ors)
set.seed(1121)
# simulate overlapping scores data
scores_1 <- runif(100, max=0.6)
scores_2 <- runif(100, min=0.4)
# perform Wilcoxon test for mean
wilcox.test(scores_1, scores_2)
# combine scores and add categorical variable
score_s <- c(scores_1, scores_2)
ac_tive <- c(logical(100), !logical(100))
# perform logit regression
g_lm <- glm(ac_tive ~ score_s, family=binomial(logit))
class(g_lm)
summary(g_lm)
par(mar=c(3, 3, 2, 2), mgp=c(2, 1, 0), oma=c(0, 0, 0, 0))
or_der <- order(score_s)
plot(x=score_s[or_der], y=g_lm$fitted.values[or_der],
     type="l", lwd=3,
     main="Category densities and logistic function",
     xlab="score", ylab="probability")
den_sity <- density(score_s[ac_tive])
den_sity$y <- den_sity$y/max(den_sity$y)
lines(den_sity, col="red")
polygon(c(min(den_sity$x), den_sity$x, max(den_sity$x)), c(min(den_sity$y), den_sity$y, min(den_sity$y)), col=rgb(1, 0, 0, 0.2), border=NA)
den_sity <- density(score_s[!ac_tive])
den_sity$y <- den_sity$y/max(den_sity$y)
lines(den_sity, col="blue")
polygon(c(min(den_sity$x), den_sity$x, max(den_sity$x)), c(min(den_sity$y), den_sity$y, min(den_sity$y)), col=rgb(0, 0, 1, 0.2), border=NA)
# add legend
legend(x="top", bty="n", lty=c(1, NA, NA), lwd=c(3, NA, NA), pch=c(NA, 15, 15),
 legend=c("logistic fit", "active", "non-active"),
 col=c("black", "red", "blue"),
 text.col=c("black", "red", "blue"))
library(ISLR)  # load package ISLR
# get documentation for package tseries
packageDescription("ISLR")  # get short description

help(package="ISLR")  # load help page

library(ISLR)  # load package ISLR

data(package="ISLR")  # list all datasets in ISLR

ls("package:ISLR")  # list all objects in ISLR

detach("package:ISLR")  # remove ISLR from search path
library(ISLR)  # load package ISLR
# load credit default data
attach(Default)
summary(Default)
sapply(Default, class)
dim(Default); head(Default)
x_lim <- range(balance)
y_lim <- range(income)
# plot data points for non-defaulters
default_ed <- (default=="Yes")
plot(income ~ balance,
     main="Default dataset from package ISLR",
     xlim=x_lim, ylim=y_lim,
     data=Default[!default_ed, ],
     pch=4, col="blue")
# plot data points for defaulters
points(income ~ balance,
 data=Default[default_ed, ],
 pch=4, col="red")
# add legend
legend(x="topright", bty="n",
 legend=c("non-defaulters", "defaulters"),
 col=c("blue", "red"), lty=1, pch=4)
default_ed <- (default=="Yes")
# Wilcoxon test for balance predictor
wilcox.test(balance[default_ed], balance[!default_ed])
# Wilcoxon test for income predictor
wilcox.test(income[default_ed], income[!default_ed])
library(ISLR)  # load package ISLR
attach(Default)  # load credit default data
par(mfrow=c(1,2))  # set plot panels
# balance boxplot
boxplot(formula=balance ~ default,
  col="lightgrey",
  main="balance", xlab="default")
# income boxplot
boxplot(formula=income ~ default,
  col="lightgrey",
  main="income", xlab="default")
# fit logistic regression model
g_lm <- glm(default ~ balance,
        family=binomial(logit))
class(g_lm)
summary(g_lm)
plot(x=balance, y=default_ed,
     main="Logistic regression of credit defaults", col="orange",
     xlab="credit balance", ylab="defaults")
or_der <- order(balance)
lines(x=balance[or_der], y=g_lm$fitted.values[or_der],
col="blue", lwd=2)
legend(x="topleft", inset=0.1,
 legend=c("defaults", "logit fitted values"),
 col=c("orange", "blue"), lty=c(NA, 1), pch=c(1, NA), lwd=c(3, 3))
library(ISLR)  # load package ISLR
attach(Default)  # load credit default data
# calculate cumulative defaults
default_ed <- (default=="Yes")
to_tal <- sum(default_ed)
default_s <- sapply(balance, function(ba_lance) {
    sum(default_ed[balance <= ba_lance])
})  # end sapply
# perform logit regression
g_lm <- glm(
  cbind(default_s, to_tal-default_s) ~
    balance,
  family=binomial(logit))
summary(g_lm)
plot(x=balance, y=default_s/to_tal, col="orange", lwd=1,
     main="Cumulative defaults versus balance",
     xlab="credit balance", ylab="cumulative defaults")
or_der <- order(balance)
lines(x=balance[or_der], y=g_lm$fitted.values[or_der],
col="blue", lwd=2)
legend(x="topleft", inset=0.1,
 legend=c("cumulative defaults", "fitted values"),
 col=c("orange", "blue"), lty=c(NA, 1), pch=c(1, NA), lwd=c(3, 3))
library(ISLR)  # load package ISLR
attach(Default)  # load credit default data
# fit multifactor logistic regression model
col_names <- colnames(Default)
for_mula <- as.formula(paste(col_names[1],
  paste(col_names[-1], collapse="+"), sep=" ~ "))
g_lm <- glm(for_mula, data=Default,
        family=binomial(logit))
summary(g_lm)
library(ISLR)  # load package ISLR
attach(Default)  # load credit default data
default_ed <- (default=="Yes")
stu_dent <- (student=="Yes")
# calculate cumulative defaults
default_s <- sapply(balance,
  function(ba_lance) {
    c(stu_dent=sum(default_ed[stu_dent & (balance <= ba_lance)]),
non_student=sum(default_ed[(!stu_dent) & (balance <= ba_lance)]))
})  # end sapply
to_tal <- c(sum(default_ed[stu_dent]), sum(default_ed[!stu_dent]))
default_s <- t(default_s / to_tal)
library(ISLR)  # load package ISLR
attach(Default)  # load credit default data
# plot cumulative defaults
par(mfrow=c(1,2))  # set plot panels
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
# balance boxplot for student factor
boxplot(formula=balance ~ student,
  col="lightgrey",
  main="balance", xlab="student")
# fit full logistic regression model
for_mula <- as.formula(paste(col_names[1],
  paste(col_names[-1], collapse="+"), sep=" ~ "))
g_lm <- glm(for_mula, data=Default, family=binomial(logit))
fore_casts <- predict(g_lm, type="response")
fore_casts[1:6]
identical(g_lm$fitted.values, fore_casts)
# discrimination threshold
thresh_old <- 0.05
# calculate confusion matrix
table(default_ed, (fore_casts>thresh_old))
sum(default_ed)
# fit logistic regression over training data
set.seed(1121)  # reset random number generator
sam_ple <- sample(x=1:NROW(Default), size=NROW(Default)/2)
train_data <- Default[sam_ple, ]
g_lm <- glm(for_mula, data=train_data, family=binomial(link="logit"))
# forecast over test data
test_data <- Default[-sam_ple, ]
fore_casts <- predict(g_lm, newdata=test_data, type="response")
# calculate confusion matrix
table(test_data$default=="Yes",
(fore_casts>thresh_old))
# FALSE positive (type I error)
sum(test_data$default=="Yes" & (fore_casts<thresh_old))
# FALSE negative (type II error)
sum(test_data$default=="No" & (fore_casts>thresh_old))
detach(Default)
library(ISLR)  # load package ISLR
attach(Default)  # load credit default data
col_names <- colnames(Default)
for_mula <- as.formula(paste(col_names[1], paste(col_names[-1], collapse="+"), sep=" ~ "))
set.seed(1121)  # reset random number generator
sam_ple <- sample(x=1:NROW(Default), size=NROW(Default)/2)
train_data <- Default[sam_ple, ]
g_lm <- glm(for_mula, data=train_data, family=binomial(link="logit"))
test_data <- Default[-sam_ple, ]
fore_casts <- predict(g_lm, newdata=test_data, type="response")
thresh_old <- 0.05
# calculate confusion matrix
confu_sion <- table(test_data$default=="Yes",
              (fore_casts>thresh_old))
dimnames(confu_sion) <- list(hypothesis=rownames(confu_sion),
  forecast=colnames(confu_sion))
confu_sion
confu_sion <- confu_sion / rowSums(confu_sion)
c(typeI=confu_sion[2, 1], typeII=confu_sion[1, 2])
# below is an unsuccessful attempt to draw confusion matrix using xtable
confusion_matrix <- matrix(c("| true positive \\\\ (sensitivity)", "| false negative \\\\ (type II error)", "| false positive \\\\ (type I error)", "| true negative \\\\ (specificity)"), nc=2)
dimnames(confusion_matrix) <- list(forecast=c("FALSE", "TRUE"),
                             hypothesis=c("FALSE", "TRUE"))
print(xtable::xtable(confusion_matrix,
caption="Confusion Matrix"),
caption.placement="top",
comment=FALSE, size="scriptsize",
include.rownames=TRUE,
include.colnames=TRUE)
# end unsuccessful attempt to draw confusion table using xtable
# confusion matrix as function of thresh_old
con_fuse <- function(res_ponse, fore_casts, thresh_old) {
    confu_sion <- table(res_ponse, (fore_casts>thresh_old))
    confu_sion <- confu_sion / rowSums(confu_sion)
    c(typeI=confu_sion[2, 1], typeII=confu_sion[1, 2])
  }  # end con_fuse
con_fuse(test_data$default=="Yes", fore_casts, thresh_old=thresh_old)
# define vector of discrimination thresholds
threshold_s <- seq(0.01, 0.95, by=0.01)^2
# calculate error rates
error_rates <- sapply(threshold_s, con_fuse,
  res_ponse=(test_data$default=="Yes"),
  fore_casts=fore_casts)  # end sapply
error_rates <- t(error_rates)
# calculate area under ROC curve (AUC)
(1 - error_rates[, "typeII"]) %*%
  rutils::diff_it(error_rates[, "typeI"])
# or
type_two <- (error_rates[, "typeII"] +
  rutils::lag_it(error_rates[, "typeII"]))/2
(1 - type_two) %*% rutils::diff_it(error_rates[, "typeI"])
# plot ROC curve for defaults
plot(x=error_rates[, "typeI"],
     y=1-error_rates[, "typeII"],
     xlab="false positive rate",
     ylab="true positive rate",
     main="ROC curve for defaults",
     type="l", lwd=2, col="blue")
abline(a=0.0, b=1.0)
