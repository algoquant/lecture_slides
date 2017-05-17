library(knitr)
opts_chunk$set(prompt=TRUE, eval=FALSE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(digits=3)
options(width=60, dev='pdf')
thm <- knit_theme$get("acid")
knit_theme$set(thm)
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
ts_rets <- diff(log(EuStockMarkets[, 1]))
# number of observations
len_rets <- NROW(ts_rets)
# mean of DAX returns
mean_rets <- mean(ts_rets)
# standard deviation of DAX returns
sd_rets <- sd(ts_rets)
# skew of DAX returns
len_rets/((len_rets-1)*(len_rets-2))*
  sum(((ts_rets - mean_rets)/sd_rets)^3)
# kurtosis of DAX returns
len_rets*(len_rets+1)/((len_rets-1)^3)*
  sum(((ts_rets - mean_rets)/sd_rets)^4)
# random normal returns
ts_rets <- rnorm(len_rets, sd=2)
# mean and standard deviation of random normal returns
mean_rets <- mean(ts_rets)
sd_rets <- sd(ts_rets)
# skew of random normal returns
len_rets/((len_rets-1)*(len_rets-2))*
  sum(((ts_rets - mean_rets)/sd_rets)^3)
# kurtosis of random normal returns
len_rets*(len_rets+1)/((len_rets-1)^3)*
  sum(((ts_rets - mean_rets)/sd_rets)^4)
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
test_frame <- data.frame(samples=rnorm(1000))

# significance level, two-tailed test, critical value=2*SD
signif_level <- 2*(1-pnorm(2))
signif_level
# get p-values for all the samples
test_frame$p_values <-
  sapply(test_frame$samples, pnorm)
test_frame$p_values <-
  2*(0.5-abs(test_frame$p_values-0.5))
# compare p_values to significance level
test_frame$result <-
  test_frame$p_values > signif_level
# number of null rejections
sum(!test_frame$result)
# show null rejections
head(test_frame[!test_frame$result, ])
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
# KS test for normal distribution
ks.test(rnorm(100), rnorm)
# KS test for normal distribution
ks.test(runif(100), rnorm)
# KS test for two normal distributions
ks.test(rnorm(100), rnorm(100, mean=0.1))
# Wilcoxon test for normal distribution
wilcox.test(rnorm(100))
# Wilcoxon test for two normal distributions
wilcox.test(rnorm(100), rnorm(100, mean=0.1))
# Wilcoxon test for two normal distributions
wilcox.test(rnorm(100), rnorm(100, mean=1.0))
# Wilcoxon test for a uniform versus normal distribution
wilcox.test(runif(100), rnorm(100))
# formula of linear model with zero intercept
lin_formula <- z ~ x + y - 1
lin_formula

# collapse vector of strings into single text string
paste0("x", 1:5)
paste(paste0("x", 1:5), collapse="+")

# create formula from text string
lin_formula <- as.formula(
  # coerce text strings to formula
  paste("z ~ ",
  paste(paste0("x", 1:5), collapse="+")
  )  # end paste
)  # end as.formula
class(lin_formula)
lin_formula
# modify the formula using "update"
update(lin_formula, log(.) ~ . + beta)
set.seed(1121)  # initialize random number generator
# define explanatory variable
explana_tory <- rnorm(100, mean=2)
noise <- rnorm(100)
# response equals linear form plus error terms
  res_ponse <- -3 + explana_tory + noise
# specify regression formula
reg_formula <- res_ponse ~ explana_tory
reg_model <- lm(reg_formula)  # perform regression
class(reg_model)  # regressions have class lm
attributes(reg_model)
eval(reg_model$call$formula)  # regression formula
reg_model$coefficients  # regression coefficients
coef(reg_model)
par(oma=c(1, 2, 1, 0), mgp=c(2, 1, 0), mar=c(5, 1, 1, 1), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
x11(width=6, height=6)
plot(reg_formula)  # plot scatterplot using formula
title(main="Simple Regression", line=-1)
# add regression line
abline(reg_model, lwd=2, col="red")
# plot fitted (predicted) response values
points(x=explana_tory, y=reg_model$fitted.values,
       pch=16, col="blue")
reg_model_sum <- summary(reg_model)  # copy regression summary
reg_model_sum  # print the summary to console
attributes(reg_model_sum)$names  # get summary elements
reg_model_sum$coefficients
reg_model_sum$r.squared
reg_model_sum$adj.r.squared
reg_model_sum$fstatistic
# standard error of beta
reg_model_sum$
  coefficients["explana_tory", "Std. Error"]
sd(reg_model_sum$residuals)/sd(explana_tory)/
  sqrt(unname(reg_model_sum$fstatistic[3]))
anova(reg_model)
set.seed(1121)  # initialize random number generator
# high noise compared to coefficient
res_ponse <- 3 + explana_tory + rnorm(30, sd=8)
reg_model <- lm(reg_formula)  # perform regression
# estimate of regression coefficient is not
# statistically significant
summary(reg_model)
par(oma=c(1, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=1.0, cex.axis=1.0, cex.main=1.0, cex.sub=1.0)
reg_stats <- function(std_dev) {  # noisy regression
  set.seed(1121)  # initialize number generator
# create explanatory and response variables
  explana_tory <- rnorm(100, mean=2)
  res_ponse <- 3 + 0.2*explana_tory +
    rnorm(NROW(explana_tory), sd=std_dev)
# specify regression formula
  reg_formula <- res_ponse ~ explana_tory
# perform regression and get summary
  reg_model_sum <- summary(lm(reg_formula))
# extract regression statistics
  with(reg_model_sum, c(pval=coefficients[2, 4],
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
  reg_formula <-
    paste(col_names[2], col_names[1], sep="~")
  reg_model_sum <- summary(lm(reg_formula,
                        data=da_ta))
# extract regression statistics
  with(reg_model_sum, c(pval=coefficients[2, 4],
   adj_rsquared=adj.r.squared,
   fstat=fstatistic[1]))
}  # end reg_stats
# apply reg_stats() to vector of std dev values
vec_sd <- seq(from=0.1, to=0.5, by=0.1)
names(vec_sd) <- paste0("sd=", vec_sd)
mat_stats <-
  t(sapply(vec_sd, function (std_dev) {
    set.seed(1121)  # initialize number generator
# create explanatory and response variables
    explana_tory <- rnorm(100, mean=2)
    res_ponse <- 3 + 0.2*explana_tory +
rnorm(NROW(explana_tory), sd=std_dev)
    reg_stats(data.frame(explana_tory, res_ponse))
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
plot(reg_model)  # plot diagnostic scatterplots
plot(reg_model, which=2)  # plot just Q-Q
library(lmtest)  # load lmtest
# perform Durbin-Watson test
dwtest(reg_model)
foo <- env_etf$re_turns[, c("VTI", "VEU")]
end_points <- endpoints(foo, on="months")
head(foo)
tail(foo)
class(foo)
dim(foo)
reg_model <- lm(paste(names(foo), collapse=" ~ "), data=foo)
reg_model_sum <- summary(reg_model)
reg_model_sum
dwtest(reg_model)

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
explana_tory <- cumsum(rnorm(100))  # unit root time series
res_ponse <- cumsum(rnorm(100))
reg_formula <- res_ponse ~ explana_tory
reg_model <- lm(reg_formula)  # perform regression
# summary indicates statistically significant regression
reg_model_sum <- summary(reg_model)
reg_model_sum$coefficients
reg_model_sum$r.squared
# Durbin-Watson test shows residuals are autocorrelated
dw_test <- dwtest(reg_model)
c(dw_test$statistic[[1]], dw_test$p.value)
par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # set plot panels
plot(reg_formula, xlab="", ylab="")  # plot scatterplot using formula
title(main="Spurious Regression", line=-1)
# add regression line
abline(reg_model, lwd=2, col="red")
plot(reg_model, which=2, ask=FALSE)  # plot just Q-Q
library(lmtest)  # load lmtest
de_sign <- data.frame(  # design matrix
  explana_tory=1:30, omit_var=sin(0.2*1:30))
# response depends on both explanatory variables
res_ponse <- with(de_sign,
  0.2*explana_tory + omit_var + 0.2*rnorm(30))
# mis-specified regression only one explanatory
reg_model <- lm(res_ponse ~ explana_tory,
        data=de_sign)
reg_model_sum <- summary(reg_model)
reg_model_sum$coefficients
reg_model_sum$r.squared
# Durbin-Watson test shows residuals are autocorrelated
dwtest(reg_model)$p.value
par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # set plot panels
plot(reg_formula, data=de_sign)
abline(reg_model, lwd=2, col="red")
title(main="OVB Regression", line=-1)
plot(reg_model, which=2, ask=FALSE)  # plot just Q-Q
set.seed(1121)
library(lmtest)
# spurious regression in unit root time series
explana_tory <- cumsum(rnorm(100))  # unit root time series
res_ponse <- cumsum(rnorm(100))
reg_formula <- res_ponse ~ explana_tory
reg_model <- lm(reg_formula)  # perform regression
# summary indicates statistically significant regression
reg_model_sum <- summary(reg_model)
reg_model_sum$coefficients
reg_model_sum$r.squared
# Durbin-Watson test shows residuals are autocorrelated
dw_test <- dwtest(reg_model)
c(dw_test$statistic[[1]], dw_test$p.value)
par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # set plot panels
plot(reg_formula, xlab="", ylab="")  # plot scatterplot using formula
title(main="Spurious Regression", line=-1)
# add regression line
abline(reg_model, lwd=2, col="red")
plot(reg_model, which=2, ask=FALSE)  # plot just Q-Q
explana_tory <- seq(from=0.1, to=3.0, by=0.1)  # explanatory variable
res_ponse <- 3 + 2*explana_tory + rnorm(30)
reg_formula <- res_ponse ~ explana_tory
reg_model <- lm(reg_formula)  # perform regression
new_data <- data.frame(explana_tory=0.1*31:40)
predict_lm <- predict(object=reg_model,
              newdata=new_data, level=0.95,
              interval="confidence")
predict_lm <- as.data.frame(predict_lm)
head(predict_lm, 2)
plot(reg_formula, xlim=c(1.0, 4.0),
     ylim=range(res_ponse, predict_lm),
     main="Regression predictions")
abline(reg_model, col="red")
with(predict_lm, {
  points(x=new_data$explana_tory, y=fit, pch=16, col="blue")
  lines(x=new_data$explana_tory, y=lwr, lwd=2, col="red")
  lines(x=new_data$explana_tory, y=upr, lwd=2, col="red")
})  # end with
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
# simulate categorical data
sco_re <- sort(runif(100))
ac_tive <- ((sco_re + rnorm(100, sd=0.1)) > 0.5)
# Wilcoxon test for sco_re predictor
wilcox.test(sco_re[ac_tive], sco_re[!ac_tive])
# perform logit regression
log_it <- glm(ac_tive ~ sco_re, family=binomial(logit))
class(log_it)
summary(log_it)
plot(x=sco_re, y=log_it$fitted.values, type="l", lwd=3,
     main="Category densities and logistic function",
     xlab="score", ylab="probability")
den_sity <- density(sco_re[ac_tive])
den_sity$y <- den_sity$y/max(den_sity$y)
lines(den_sity, col="red")
polygon(c(min(den_sity$x), den_sity$x, max(den_sity$x)), c(min(den_sity$y), den_sity$y, min(den_sity$y)), col=rgb(1, 0, 0, 0.2), border=NA)
den_sity <- density(sco_re[!ac_tive])
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
log_it <- glm(default ~ balance,
        family=binomial(logit))
class(log_it)
summary(log_it)
plot(x=balance, y=default_ed,
     main="Logistic regression of credit defaults", col="orange",
     xlab="credit balance", ylab="defaults")
or_der <- order(balance)
lines(x=balance[or_der], y=log_it$fitted.values[or_der],
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
log_it <- glm(
  cbind(default_s, to_tal-default_s) ~
    balance,
  family=binomial(logit))
summary(log_it)
plot(x=balance, y=default_s/to_tal, col="orange", lwd=1,
     main="Cumulative defaults versus balance",
     xlab="credit balance", ylab="cumulative defaults")
or_der <- order(balance)
lines(x=balance[or_der], y=log_it$fitted.values[or_der],
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
log_it <- glm(for_mula, data=Default,
        family=binomial(logit))
summary(log_it)
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
 lwd=c(3, 3))
# balance boxplot for student factor
boxplot(formula=balance ~ student,
  col="lightgrey",
  main="balance", xlab="student")
# fit full logistic regression model
for_mula <- as.formula(paste(col_names[1],
  paste(col_names[-1], collapse="+"), sep=" ~ "))
log_it <- glm(for_mula, data=Default, family=binomial(logit))
fore_casts <- predict(log_it, type="response")
fore_casts[1:6]
identical(log_it$fitted.values, fore_casts)
# discrimination threshold
thresh_old <- 0.05
# calculate confusion matrix
table(default_ed, (fore_casts>thresh_old))
sum(default_ed)
# fit logistic regression over training data
set.seed(1121)  # reset random number generator
sam_ple <- sample(x=1:NROW(Default), size=NROW(Default)/2)
train_data <- Default[sam_ple, ]
log_it <- glm(for_mula, data=train_data, family=binomial(link="logit"))
# forecast over test data
test_data <- Default[-sam_ple, ]
fore_casts <- predict(log_it, newdata=test_data, type="response")
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
log_it <- glm(for_mula, data=train_data, family=binomial(link="logit"))
test_data <- Default[-sam_ple, ]
fore_casts <- predict(log_it, newdata=test_data, type="response")
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
