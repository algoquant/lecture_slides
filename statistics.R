set.seed(1121)  # Reset random number generator
runif(3)  # three numbers from uniform distribution
runif(3)  # Simulate another three numbers
set.seed(1121)  # Reset random number generator
runif(3)  # Simulate another three numbers
# Simulate random number from standard normal distribution
rnorm(1)
# Simulate five standard normal random numbers
rnorm(5)
# Simulate five non-standard normal random numbers
rnorm(n=5, mean=1, sd=2)  # Match arguments by name
# Simulate t-distribution with 2 degrees of freedom
rt(n=5, df=2)
# Define logistic map function
log_map <- function(x, r=4) r*x*(1-x)
log_map(0.25, 4)
# Plot logistic map
x11(width=6, height=5)
curve(expr=log_map, type="l", xlim=c(0, 1),
xlab="x[n-1]", ylab="x[n]", lwd=2, col="blue",
main="logistic map")
lines(x=c(0, 0.25), y=c(0.75, 0.75), lwd=2, col="orange")
lines(x=c(0.25, 0.25), y=c(0, 0.75), lwd=2, col="orange")
# Calculate uniformly distributed pseudo-random
# Sequence using logistic map function
uni_form <- function(see_d, n_rows=10) {
  # Pre-allocate vector instead of "growing" it
  out_put <- numeric(n_rows)
  # initialize
  out_put[1] <- see_d
  # Perform loop
  for (i in 2:n_rows) {
    out_put[i] <- 4*out_put[i-1]*(1-out_put[i-1])
  }  # end for
  acos(1-2*out_put)/pi
}  # end uni_form
uni_form(see_d=0.1, n_rows=15)
plot(
  density(uni_form(see_d=runif(1), n_rows=1e5)),
  xlab="", ylab="", lwd=2, col="blue",
  main="uniform pseudo-random number density")
set.seed(1121)  # Reset random number generator
# Flip unbiased coin once, 20 times
rbinom(n=20, size=1, 0.5)
# Number of heads after flipping twice, 20 times
rbinom(n=20, size=2, 0.5)
# Number of heads after flipping thrice, 20 times
rbinom(n=20, size=3, 0.5)
# Number of heads after flipping biased coin thrice, 20 times
rbinom(n=20, size=3, 0.8)
# Number of heads after flipping biased coin thrice, 20 times
rbinom(n=20, size=3, 0.2)
# Flip unbiased coin once, 20 times
sample(x=0:1, size=20, replace=TRUE)  # Fast
as.numeric(runif(20) < 0.5)  # Slower
# Permutation of five numbers
sample(x=5)
# Permutation of four strings
sample(x=c("apple", "grape", "orange", "peach"))
# Sample of size three
sample(x=5, size=3)
# Sample with replacement
sample(x=5, replace=TRUE)
sample(  # Sample of strings
  x=c("apple", "grape", "orange", "peach"),
  size=12,
  replace=TRUE)
# Binomial sample: flip coin once, 20 times
sample(x=0:1, size=20, replace=TRUE)
# Flip unbiased coin once, 20 times
as.numeric(runif(20) > 0.5)  # Slower
rm(list=ls())
set.seed(1121)  # Reset random number generator
# Sample from Standard Normal Distribution
da_ta <- rnorm(1000)
mean(da_ta)  # Sample mean
median(da_ta)  # Sample median
sd(da_ta)  # Sample standard deviation
rm(list=ls())
# VTI returns
re_turns <- na.omit(rutils::etf_env$re_turns$VTI)
# Number of observations
n_rows <- NROW(re_turns)
# Mean of VTI returns
mea_n <- mean(re_turns)
# Standard deviation of VTI returns
s_d <- sd(re_turns)
# Standardize returns
re_turns <- (re_turns - mea_n)/s_d
# Skewness of VTI returns
n_rows/((n_rows-1)*(n_rows-2))*sum(re_turns^3)
# Kurtosis of VTI returns
n_rows/(n_rows-1)^2*sum(re_turns^4)
# Random normal returns
re_turns <- rnorm(n_rows)
# Mean and standard deviation of random normal returns
mean(re_turns); sd(re_turns)
# Skewness and kurtosis of random normal returns
n_rows/((n_rows-1)*(n_rows-2))*sum(re_turns^3)
n_rows/(n_rows-1)^2*sum(re_turns^4)
# Calculate cumulative standard normal distribution
c(pnorm(-2), pnorm(2))
# Calculate inverse cumulative standard normal distribution
c(qnorm(0.75), qnorm(0.25))
set.seed(1121)  # Reset random number generator
# Sample from Standard Normal Distribution
n_rows <- 1000
da_ta <- rnorm(n_rows)
# Sample mean - MC estimate
mean(da_ta)
# Sample standard deviation - MC estimate
sd(da_ta)
# Monte Carlo estimate of cumulative probability
c(pnorm(1), sum(da_ta < 1)/n_rows)
# Monte Carlo estimate of quantile
conf_level <- 0.99
qnorm(conf_level)
cut_off <- conf_level*n_rows
da_ta <- sort(da_ta)
c(da_ta[cut_off], quantile(da_ta, probs=conf_level))
# Read the source code of quantile()
stats:::quantile.default
# microbenchmark quantile
library(microbenchmark)
summary(microbenchmark(
  monte_carlo=da_ta[cut_off],
  quan_tile=quantile(da_ta, probs=conf_level),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
set.seed(1121)  # Reset random number generator
# Sample from Standard Normal Distribution
n_rows <- 1000
da_ta <- rnorm(n_rows)
# Sample mean
mean(da_ta)
# Sample standard deviation
sd(da_ta)
# Standard error of sample mean
sd(da_ta)/sqrt(n_rows)
# Define Hermite polynomials
her_mite <- function(x, n) {
    switch(n+1, 1, x, (x^2 - 1), (x^3 - 3*x), 0)
}  # end her_mite
x11(width=6, height=5)  # Plot in window
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
col_ors <- c("red", "blue", "green")
for (in_dex in 1:3) {  # Plot three curves
  curve(expr=her_mite(x, in_dex),
  xlim=c(-3, 3), ylim=c(-2.5, 2.5),
  xlab="", ylab="", lwd=4, col=col_ors[in_dex],
  add=as.logical(in_dex-1))
}  # end for
# Add title and legend
title(main="Hermite Polynomials", line=0.5)
lab_els <- paste("Order", 1:3, sep=" = ")
legend("top", inset=0.0, bty="n",
 title=NULL, lab_els, cex=0.8, lwd=6, lty=1,
 col=col_ors)
# Define Hermite functions
hermite_fun <- function(x, n)
  exp(-x^2/4)*her_mite(x, n)/(2*pi)^(0.25)/sqrt(factorial(n))
# Integrate Hermite functions
integrate(function(x, n, m)
  hermite_fun(x, n)*hermite_fun(x, m),
  lower=(-Inf), upper=Inf, n=2, m=3)
x11(width=6, height=5)  # Plot in window
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
col_ors <- c("red", "blue", "green")
for (in_dex in 1:3) {  # Plot three curves
  curve(expr=hermite_fun(x, in_dex),
  xlim=c(-6, 6), ylim=c(-0.6, 0.6),
  xlab="", ylab="", lwd=4, col=col_ors[in_dex],
  add=as.logical(in_dex-1))
}  # end for
# Add title and legend
title(main="Hermite Functions", line=0.5)
lab_els <- paste("Order", 1:3, sep=" = ")
legend("topright", inset=0.0, bty="n",
 title=NULL, lab_els, cex=0.8, lwd=6, lty=1,
 col=col_ors)
# Integrate Hermite functions
integrate(her_mite, lower=(-Inf), upper=Inf, n=2)
integrate(function(x, n, m) her_mite(x, n)*her_mite(x, m),
    lower=(-Inf), upper=Inf, n=2, m=3)
integrate(function(x, n, m) her_mite(x, n)*her_mite(x, m),
    lower=(-Inf), upper=Inf, n=2, m=2)
x11(width=6, height=5)  # Plot in window
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
col_ors <- c("red", "blue", "green")
for (in_dex in 1:3) {  # Plot three curves
  curve(expr=her_mite(x, in_dex),
  xlim=c(-4, 4), ylim=c(-0.6, 0.6),
  xlab="", ylab="", lwd=3, col=col_ors,
  add=as.logical(in_dex-1))
}  # end for
# Add title and legend
title(main="Hermite Functions", line=0.5)
lab_els <- paste("Order", 1:3, sep=" = ")
legend("topright", inset=0.05, bty="n",
 title=NULL, lab_els, cex=0.8, lwd=6, lty=1,
 col=col_ors)
set.seed(1121)  # Reset random number generator
# Sample from Standard Normal Distribution
n_rows <- 1000
da_ta <- rnorm(n_rows)
# Sample mean
mean(da_ta)
# Sample standard deviation
sd(da_ta)
#Perform two-tailed test that sample is
#from Standard Normal Distribution (mean=0, SD=1)
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
rm(list=ls())
par(oma=c(1, 1, 1, 1), mgp=c(2, 0.5, 0), mar=c(5, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
library(ggplot2)  # Load ggplot2
qplot(  # Simple ggplot2
    main="Standard Normal Distribution",
    c(-4, 4),
    stat="function",
    fun=dnorm,
    geom="line",
    xlab=NULL, ylab=NULL
    ) +  # end qplot
theme(  # Modify plot theme
    plot.title=element_text(vjust=-1.0),
    plot.background=element_blank()
    ) +  # end theme
geom_vline(  # Add vertical line
  aes(xintercept=c(-2.0, 2.0)),
  colour="red",
  linetype="dashed"
  )  # end geom_vline
rm(list=ls())
par(oma=c(1, 1, 1, 1), mgp=c(2, 0.5, 0), mar=c(5, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
#Create ggplot2 with shaded area
x_var <- -400:400/100
norm_frame <- data.frame(x_var=x_var,
                 d.norm=dnorm(x_var))
norm_frame$shade <- ifelse(
            abs(norm_frame$x_var) >= 2,
            norm_frame$d.norm, NA)
ggplot(  # Main function
  data=norm_frame,
  mapping=aes(x=x_var, y=d.norm)
  ) +  # end ggplot
# Plot line
  geom_line() +
# Plot shaded area
  geom_ribbon(aes(ymin=0, ymax=shade), fill="red") +
# No axis labels
  xlab("") + ylab("") +
# Add title
  ggtitle("Standard Normal Distribution") +
# Modify plot theme
  theme(
  plot.title=element_text(vjust=-1.0),
  plot.background=element_blank()
  )  # end theme
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
# Calculate VTI percentage returns
library(rutils)
re_turns <- na.omit(rutils::etf_env$re_turns$VTI)
# Reset output digits
dig_its <- options(digits=5)
# Shapiro-Wilk test for normal distribution
shapiro.test(rnorm(NROW(re_turns)))
# Shapiro-Wilk test for VTI returns
shapiro.test(as.numeric(re_turns))
# Shapiro-Wilk test for uniform distribution
shapiro.test(runif(NROW(re_turns)))
# Restore output digits
options(digits=dig_its$digits)
library(tseries)  # Load package tseries
# Jarque-Bera test for normal distribution
jarque.bera.test(rnorm(NROW(re_turns)))
# Jarque-Bera test for VTI returns
jarque.bera.test(re_turns)
# Jarque-Bera test for uniform distribution
jarque.bera.test(runif(NROW(re_turns)))
# KS test for normal distribution
ks_test <- ks.test(rnorm(100), pnorm)
ks_test$p.value
# KS test for uniform distribution
ks.test(runif(100), pnorm)
# KS test for two similar normal distributions
ks.test(rnorm(100), rnorm(100, mean=0.1))
# KS test for two different normal distributions
ks.test(rnorm(100), rnorm(100, sd=2.0))
# KS test for VTI returns vs normal distribution
re_turns <- as.numeric(na.omit(rutils::etf_env$re_turns$VTI))
re_turns <- (re_turns - mean(re_turns))/sd(re_turns)
ks.test(re_turns, rnorm)
# Fit t-dist into VTI returns
optim_fit <- MASS::fitdistr(re_turns, densfun="t", df=2)
lo_cation <- optim_fit$estimate[1]
scal_e <- optim_fit$estimate[2]
# KS test for VTI returns vs t-distribution
# define non-standard Student’s t-distribution
tdistr <- function(x, dfree, loc, scale) {
  dt((x-loc)/scale, df=dfree)/scale
}  # end likeli_hood
# tdistr(x=re_turns[1], dfree=2, loc=lo_cation, scale=scal_e)
# Plot shows that tdistr() fits re_turns very well
den_sity <- density(re_turns)
ma_d <- mad(re_turns)
plot(den_sity, xlim=c(-5*ma_d, 5*ma_d),
     xlab="returns", ylab="density",
     col="blue", lwd=2, main="Density of VTI Returns")
curve(expr=tdistr(x, dfree=2, loc=lo_cation, scale=scal_e), col="red", lwd=2, add=TRUE)
# Or
tdistr <- function(x, dfree, loc, scale) {
  gamma((dfree+1)/2)/(sqrt(pi*dfree)*gamma(dfree/2)*scale)*
    (1+((x-loc)/scale)^2/dfree)^(-(dfree+1)/2)
}  # end likeli_hood
# tdistr(x=re_turns[1], dfree=2, loc=lo_cation, scale=scal_e)
# Not the same as the below - gives p-value << 0.01 - rejects NULL
ks.test(re_turns, tdistr, dfree=2, loc=lo_cation, scale=scal_e)
# Not the same as the above - gives p-value > 0.02 most of the time - doesn't reject NULL
# KS test for VTI returns vs t-distribution
da_ta <- lo_cation + scal_e*rt(NROW(re_turns), df=2)
ks.test(re_turns, da_ta)
x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
# Degrees of freedom
deg_free <- c(2, 5, 8, 11)
# Plot four curves in loop
col_ors <- c("red", "black", "blue", "green")
for (in_dex in 1:4) {
  curve(expr=dchisq(x, df=deg_free[in_dex]),
  xlim=c(0, 20), ylim=c(0, 0.3),
  xlab="", ylab="", col=col_ors[in_dex],
  lwd=2, add=as.logical(in_dex-1))
}  # end for
# Add title
title(main="Chi-squared Distributions", line=0.5)
# Add legend
lab_els <- paste("df", deg_free, sep="=")
legend("topright", inset=0.05, bty="n",
       title="Degrees of freedom", lab_els,
       cex=0.8, lwd=6, lty=1, col=col_ors)
# Observed frequencies from random normal data
histo_gram <- hist(rnorm(1e3, mean=0), breaks=100, plot=FALSE)
freq_o <- histo_gram$counts
# Theoretical frequencies
freq_t <- rutils::diff_it(pnorm(histo_gram$breaks))
# Perform Chi-squared test for normal data
chisq.test(x=freq_o, p=freq_t, rescale.p=TRUE, simulate.p.value=TRUE)
# Return p-value
chisq_test <- chisq.test(x=freq_o, p=freq_t, rescale.p=TRUE, simulate.p.value=TRUE)
chisq_test$p.value
# Observed frequencies from shifted normal data
histo_gram <- hist(rnorm(1e3, mean=2), breaks=100, plot=FALSE)
freq_o <- histo_gram$counts/sum(histo_gram$counts)
# Theoretical frequencies
freq_t <- rutils::diff_it(pnorm(histo_gram$breaks))
# Perform Chi-squared test for shifted normal data
chisq.test(x=freq_o, p=freq_t, rescale.p=TRUE, simulate.p.value=TRUE)
# Calculate histogram of VTI returns
histo_gram <- hist(re_turns, breaks=100, plot=FALSE)
freq_o <- histo_gram$counts
# Calculate cumulative probabilities and then difference them
freq_t <- pt((histo_gram$breaks-lo_cation)/scal_e, df=2)
freq_t <- rutils::diff_it(freq_t)
# Perform Chi-squared test for VTI returns
chisq.test(x=freq_o, p=freq_t, rescale.p=TRUE, simulate.p.value=TRUE)
# Sort a vector into ascending order
da_ta <- round(runif(7), 3)
sort_ed <- sort(da_ta)
da_ta  # original data
sort_ed  # sorted data
# Calculate index to sort into ascending order
in_dex <- order(da_ta)
in_dex  # permutation index to sort
all.equal(sort_ed, da_ta[in_dex])
# Sort the ordered vector back to its original unsorted order
in_dex <- order(order(da_ta))
in_dex  # permutation index to unsort
all.equal(da_ta, sort_ed[in_dex])
# Calculate ranks of the vector elements
rank(da_ta)
all.equal(rank(da_ta), in_dex)
# VTI returns
re_turns <- as.numeric(na.omit(rutils::etf_env$re_turns[, "VTI"]))
n_rows <- NROW(re_turns)
re_turns <- 100*(re_turns-mean(re_turns))/sd(re_turns)
# Simulate normal random data
n_data <- rnorm(n_rows, sd=100)
# Bootstrap the mean and median estimators
boot_data <- sapply(1:1e3, function(x) {
  # Simulate data
  n_data <- rnorm(n_rows, sd=100)
  re_turns <- re_turns[sample.int(n_rows, replace=TRUE)]
  c(n_mean=mean(n_data),
    n_median=median(n_data),
    vti_mean=mean(re_turns),
    vti_median=median(re_turns))
})  # end sapply
boot_data <- t(boot_data)
# Analyze bootstrapped data
head(boot_data)
sum(is.na(boot_data))
# Means and medians from bootstrap
apply(boot_data, MARGIN=2, function(x)
  c(mean=mean(x), std_error=sd(x)))
# Parallel bootstrap under Windows
library(parallel)  # Load package parallel
n_cores <- detectCores() - 1  # Number of cores
clus_ter <- makeCluster(n_cores)  # initialize compute cluster
boot_data <- parLapply(clus_ter, 1:1e4,
  function(x, n_rows, re_turns) {
    # Simulate data
    n_data <- rnorm(n_rows, sd=100)
    re_turns <- re_turns[sample.int(n_rows, replace=TRUE)]
    c(n_mean=mean(n_data),
n_median=median(n_data),
vti_mean=mean(re_turns),
vti_median=median(re_turns))
  }, n_rows, re_turns)  # end parLapply
stopCluster(clus_ter)  # Stop R processes over cluster
# Parallel bootstrap under Mac-OSX or Linux
boot_data <- mclapply(1:1e4, function(x) {
  # Simulate data
  n_data <- rnorm(n_rows)
  t_data <- rt(n_rows, df=2)
  c(n_mean=mean(n_data),
    n_median=median(n_data),
    vti_mean=mean(t_data),
    vti_median=median(t_data))
}, mc.cores=n_cores)  # end mclapply
# Means and medians from bootstrap
boot_data <- rutils::do_call(rbind, boot_data)
apply(boot_data, MARGIN=2, function(x)
  c(mean=mean(x), std_error=sd(x)))
# Bias and variance from bootstrap
bias_var <- apply(boot_data, MARGIN=2,
  function(x) c(bias=mean(x), variance=var(x)))
# MSE of mean
bias_var[1, 3]^2 + bias_var[2, 3]
# MSE of median
bias_var[1, 4]^2 + bias_var[2, 4]
re_turns <- as.numeric(na.omit(rutils::etf_env$re_turns[, "VTI"]))
n_rows <- NROW(re_turns)
re_turns <- 100*(re_turns-mean(re_turns))/sd(re_turns)
# Simulate normal random data
n_data <- rnorm(n_rows, sd=100)
# Hodges-Lehmann estimator
# Bootstrap the mean and median estimators
boot_data <- sapply(1:1e3, function(x) {
  # Simulate data
  n_data <- rnorm(n_rows, sd=100)
  re_turns <- re_turns[sample.int(n_rows, replace=TRUE)]
  c(n_mean=mean(n_data),
    n_median=median(n_data),
    vti_mean=mean(re_turns),
    vti_median=median(re_turns))
})  # end sapply
boot_data <- t(boot_data)
# Analyze bootstrapped data
head(boot_data)
sum(is.na(boot_data))
# Means and medians from bootstrap
apply(boot_data, MARGIN=2, function(x)
  c(mean=mean(x), std_error=sd(x)))
n_rows <- 1e3
da_ta <- rnorm(n_rows)
sd(da_ta)
mad(da_ta)
median(abs(da_ta - median(da_ta)))
median(abs(da_ta - median(da_ta)))/qnorm(0.75)
# Bootstrap of sd and mad estimators
boot_data <- sapply(1:1e4, function(x) {
  sampl_e <- da_ta[sample.int(n_rows, replace=TRUE)]
  c(sd=sd(sampl_e), mad=mad(sampl_e))
})  # end sapply
boot_data <- t(boot_data)
# Analyze bootstrapped variance
head(boot_data)
sum(is.na(boot_data))
# Means and standard errors from bootstrap
apply(boot_data, MARGIN=2, function(x)
  c(mean=mean(x), std_error=sd(x)))
# Parallel bootstrap under Windows
library(parallel)  # Load package parallel
n_cores <- detectCores() - 1  # Number of cores
clus_ter <- makeCluster(n_cores)  # initialize compute cluster
boot_data <- parLapply(clus_ter, 1:1e4,
  function(x, da_ta) {
    sampl_e <- da_ta[sample.int(n_rows, replace=TRUE)]
    c(sd=sd(sampl_e), mad=mad(sampl_e))
  }, da_ta=da_ta)  # end parLapply
stopCluster(clus_ter)  # Stop R processes over cluster
# Parallel bootstrap under Mac-OSX or Linux
boot_data <- mclapply(1:1e4, function(x) {
    sampl_e <- da_ta[sample.int(n_rows, replace=TRUE)]
    c(sd=sd(sampl_e), mad=mad(sampl_e))
  }, mc.cores=n_cores)  # end mclapply
# Means and standard errors from bootstrap
boot_data <- rutils::do_call(rbind, boot_data)
apply(boot_data, MARGIN=2, function(x)
  c(mean=mean(x), std_error=sd(x)))
set.seed(1121)  # Reset random number generator
# Sample from Standard Normal Distribution
n_rows <- 1e3
da_ta <- rnorm(n_rows)
# Sample mean
mean(da_ta)
# Sample standard deviation
sd(da_ta)
n_rows <- 1e3
# Wilcoxon test for normal distribution
nor_mal <- rnorm(n_rows)
wilcox.test(nor_mal)
wilcox.test(nor_mal+1)
# Skewed distribution with median=0
log_norm <- exp(nor_mal)
# log_norm <- rlnorm(n_rows, sdlog=1)
mean(log_norm); median(log_norm)
# Skewed distribution with median!=0
wilcox.test(log_norm)
# Skewed distribution with median=0
wilcox.test(log_norm-median(log_norm))
# Skewed distributions with median!=0
wilcox.test(log_norm, nor_mal, paired=TRUE)
# Two distributions with median=0
wilcox.test(log_norm-median(log_norm), nor_mal-median(nor_mal),
      paired=TRUE)
# Skewed distribution with median=0
wilcox.test(log_norm-median(log_norm))
# Skewed distribution with mean=0
mean(log_norm); median(log_norm)
wilcox.test(log_norm-median(log_norm))
# Same as
wilcox.test(log_norm-median(log_norm), rep(0, n_rows), paired=TRUE)
# Skewed distributions with median!=0
wilcox.test(log_norm, nor_mal, paired=TRUE)
# Two distributions with median=0
wilcox.test(log_norm-median(log_norm), nor_mal-median(nor_mal),
      paired=TRUE)
# Normal samples with different standard deviations
sample1 <- rnorm(n_rows, sd=1)
sample2 <- rnorm(n_rows, sd=10)
wilcox.test(sample1, sample2, paired=TRUE)
# Wilcoxon test for random data around 0
un_if <- (runif(n_rows) - 0.5)
wil_cox <- wilcox.test(un_if)
# Calculate V statistic of Wilcoxon test
wil_cox$statistic
sum(rank(abs(un_if))[un_if>0])
# Calculate W statistic of Wilcoxon test
sum(sign(un_if)*rank(abs(un_if)))
# Two sets of normal data
sample1 <- rnorm(n_rows)
sample2 <- rnorm(n_rows, mean=0.1)
# Wilcoxon test
wil_cox <- wilcox.test(sample1, sample2, paired=TRUE)
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
n_rows <- 1e3
# Wilcoxon test for normal distribution
nor_mal <- rnorm(n_rows)
wilcox.test(nor_mal)
wilcox.test(nor_mal+1)
# Skewed distribution with median=0
log_norm <- rlnorm(n_rows, sdlog=1)
mean(log_norm); median(log_norm)
# Skewed distribution with median!=0
wilcox.test(log_norm)
# Skewed distribution with median=0
wilcox.test(log_norm-median(log_norm))
# Skewed distributions with median!=0
wilcox.test(log_norm, nor_mal, paired=TRUE)
# Two distributions with median=0
wilcox.test(log_norm-median(log_norm), nor_mal-median(nor_mal),
      paired=TRUE)
# Skewed distribution with median=0
wilcox.test(log_norm-median(log_norm))
# Skewed distribution with mean=0
mean(log_norm); median(log_norm)
wilcox.test(log_norm-median(log_norm))
# Same as
wilcox.test(log_norm-median(log_norm), rep(0, n_rows), paired=TRUE)
# Skewed distributions with median!=0
wilcox.test(log_norm, nor_mal, paired=TRUE)
# Two distributions with median=0
wilcox.test(log_norm-median(log_norm), nor_mal-median(nor_mal),
      paired=TRUE)
# Normal samples with different standard deviations
sample1 <- rnorm(n_rows, sd=1)
sample2 <- rnorm(n_rows, sd=10)
wilcox.test(sample1, sample2, paired=TRUE)
# Wilcoxon test for random data around 0
un_if <- (runif(n_rows) - 0.5)
wil_cox <- wilcox.test(un_if)
# Calculate V statistic of Wilcoxon test
wil_cox$statistic
sum(rank(abs(un_if))[un_if>0])
# Calculate W statistic of Wilcoxon test
sum(sign(un_if)*rank(abs(un_if)))
# Two sets of normal data
sample1 <- rnorm(n_rows)
sample2 <- rnorm(n_rows, mean=0.1)
# Wilcoxon test
wil_cox <- wilcox.test(sample1, sample2, paired=TRUE)
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
# Data samples
da_ta <- sort(rnorm(38))
in_dex <- c(1:9, 20:29)
# Or
da_ta <- sort(rnorm(398))
in_dex <- c(1:99, 200:299)
sample1 <- da_ta[in_dex]
sample2 <- da_ta[-in_dex]
# Or
in_dex <- sample(1:n_rows, size=n_rows/2)
sample1 <- re_turns[in_dex]
sample2 <- (-re_turns[-in_dex])
sample1 <- (sample1- median(sample1))
sample2 <- (sample2- median(sample2))
moments::moment(sample1, order=3)
moments::moment(sample2, order=3)
# Mann-Whitney test for normal distribution
wilcox.test(sample1, sample2, paired=FALSE)
wilcox.test(sample1, sample2, paired=TRUE)
blue <- rgb(0, 0, 1, alpha=0.5)
red <- rgb(1, 0, 0, alpha=0.5)
barplot(sample2, col=red)
barplot(sample1, col=blue, add=TRUE)
hist(sample1)
# Mann-Whitney test for normal distribution
da_ta <- rnorm(n_rows, sd=100)
wilcox.test(da_ta, paired=FALSE)
# Skewed distribution with mean=0
mean(re_turns); median(re_turns)
wilcox.test(re_turns-mean(re_turns),
      paired=FALSE)
# Skewed distribution with median=0
wilcox.test(re_turns-median(re_turns),
      paired=FALSE)
# Skewed distribution with median=0
wilcox.test(re_turns-median(re_turns),
      da_ta, paired=FALSE)
sample1 <- sample(re_turns, size=n_rows)
sample2 <- (-sample1)
sample1 <- (sample1-median(sample1))
sample2 <- (sample2-median(sample2))
# Mann-Whitney-Wilcoxon rank sum test
wilcox.test(sample1, sample2,
      paired=FALSE)$p.value
da_ta <- (-re_turns)
da_ta <- (da_ta-median(da_ta))
wilcox.test(re_turns-median(re_turns),
      da_ta, paired=FALSE)
wilcox.test(re_turns-median(re_turns),
      da_ta, paired=TRUE)
da_ta <- (-re_turns)
da_ta <- (da_ta-mean(da_ta))
wilcox.test(re_turns-mean(re_turns),
      da_ta, paired=FALSE)
wilcox.test(re_turns-mean(re_turns),
      da_ta, paired=TRUE)
foo <- sapply(1:100, function(x) {
  # Data samples
  sample1 <- sample(re_turns, size=n_rows/2)
  sample2 <- sample(-re_turns, size=n_rows/2)
  sample1 <- (sample1-median(sample1))
  sample2 <- (sample2-median(sample2))
  # Mann-Whitney-Wilcoxon rank sum test
  wilcox.test(sample1, sample2, paired=FALSE)$p.value
})
hist(foo)
# Skewed distribution with mean=0
mean(re_turns); median(re_turns)
wilcox.test(re_turns-mean(re_turns))
# Same as
wilcox.test(re_turns-mean(re_turns),
      rep(0, n_rows),
      paired=TRUE)
# Skewed distribution with median=0
wilcox.test(re_turns-median(re_turns))
# Data samples
sample1 <- rnorm(200)
sample2 <- rnorm(100, mean=0.1)
# Mann-Whitney-Wilcoxon rank sum test
wil_cox <- wilcox.test(sample1, sample2,
                 paired=FALSE)
wil_cox$statistic
# Calculate U statistics of Mann-Whitney-Wilcoxon test
da_ta <- c(sample1, sample2)
rank_s <- rank(da_ta)
sum(rank_s[1:200]) - 100*201
sum(rank_s[201:300]) - 50*101
# Wilcoxon test for random data around 0
n_rows <- 1e3
da_ta <- (runif(n_rows) - 0.5)
wil_cox <- wilcox.test(da_ta)
# Calculate V statistic of Wilcoxon test
wil_cox$statistic
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
hist(wilcox_w)
# iris data frame
aggregate(Sepal.Length ~ Species, data=iris,
  FUN=function(x) c(mean=mean(x), sd=sd(x)))
# Kruskal-Wallis test for iris data
k_test <- kruskal.test(Sepal.Length ~ Species, data=iris)
str(k_test)
k_test$statistic
# Kruskal-Wallis test for independent normal distributions
sample1 <- rnorm(1e3)
sample2 <- rnorm(1e3)
fac_tor <- c(rep(TRUE, 1e3), rep(FALSE, 1e3))
kruskal.test(x=c(sample1, sample2), g=fac_tor)
# Kruskal-Wallis test for shifted normal distributions
kruskal.test(x=c(sample1+1, sample2), g=fac_tor)
# Kruskal-Wallis test for beta distributions
sample1 <- rbeta(1e3, 2, 8) + 0.3
sample2 <- rbeta(1e3, 8, 2) - 0.3
mean(sample1); mean(sample2)
kruskal.test(x=c(sample1, sample2), g=fac_tor)
# Plot the beta distributions
x11()
plot(density(sample1), col="blue", lwd=3,
     xlim=range(c(sample1, sample2)), xlab="samples",
     main="Two samples from beta distributions with equal means")
lines(density(sample2), col="red", lwd=3)
# Kruskal-Wallis test for iris data
k_test <- kruskal.test(Sepal.Length ~ Species, data=iris)
# Calculate Kruskal-Wallis test Statistic
n_rows <- NROW(iris)
iris_data <- data.frame(rank_s=rank(iris$Sepal.Length),
                  spe_cies=iris$Species)
kruskal_stat <- (12/n_rows/(n_rows+1))*sum(
  aggregate(rank_s ~ spe_cies,
      data=iris_data,
      FUN=function(x) {
        NROW(x)*((n_rows+1)/2 - mean(x))^2
      })[, 2])
c(k_test=unname(k_test$statistic),
  k_stat=kruskal_stat)
# Kruskal-Wallis test with data outliers
sample1 <- rnorm(1e3)
sample2 <- rnorm(1e3)
sample2[1:11] <- sample2[1:11] + 50
fac_tor <- c(rep(TRUE, 1e3), rep(FALSE, 1e3))
kruskal.test(x=c(sample1, sample2), g=fac_tor)$p.value
t.test(sample1, sample2)$p.value
n_rows <- 1e3
da_ta <- rnorm(n_rows)
sd(da_ta)
mad(da_ta)
median(abs(da_ta - median(da_ta)))
median(abs(da_ta - median(da_ta)))/qnorm(0.75)
# Bootstrap of sd and mad estimators
boot_data <- sapply(1:1e4, function(x) {
  sampl_e <- da_ta[sample.int(n_rows, replace=TRUE)]
  c(sd=sd(sampl_e), mad=mad(sampl_e))
})  # end sapply
boot_data <- t(boot_data)
# Analyze bootstrapped variance
head(boot_data)
sum(is.na(boot_data))
# Means and standard errors from bootstrap
apply(boot_data, MARGIN=2,
function(x) c(mean=mean(x), std_error=sd(x)))
# Parallel bootstrap under Windows
library(parallel)  # Load package parallel
n_cores <- detectCores() - 1  # Number of cores
clus_ter <- makeCluster(n_cores)  # initialize compute cluster
boot_data <- parLapply(clus_ter, 1:1e4,
  function(x, da_ta) {
    sampl_e <- da_ta[sample.int(n_rows, replace=TRUE)]
    c(sd=sd(sampl_e), mad=mad(sampl_e))
  }, da_ta=da_ta)  # end parLapply
stopCluster(clus_ter)  # Stop R processes over cluster
# Parallel bootstrap under Mac-OSX or Linux
boot_data <- mclapply(1:1e4,
  function(x) {
    sampl_e <- da_ta[sample.int(n_rows, replace=TRUE)]
    c(sd=sd(sampl_e), mad=mad(sampl_e))
  }, mc.cores=n_cores)  # end mclapply
# Means and standard errors from bootstrap
boot_data <- rutils::do_call(rbind, boot_data)
apply(boot_data, MARGIN=2, function(x)
  c(mean=mean(x), std_error=sd(x)))
n_rows <- 1e3
da_ta <- rnorm(n_rows)
sd(da_ta)
mad(da_ta)
median(abs(da_ta - median(da_ta)))
median(abs(da_ta - median(da_ta)))/qnorm(0.75)
# Bootstrap of sd and mad estimators
boot_data <- sapply(1:1e4, function(x) {
  sampl_e <- da_ta[sample.int(n_rows, replace=TRUE)]
  c(sd=sd(sampl_e), mad=mad(sampl_e))
})  # end sapply
boot_data <- t(boot_data)
# Analyze bootstrapped variance
head(boot_data)
sum(is.na(boot_data))
# Means and standard errors from bootstrap
apply(boot_data, MARGIN=2, function(x)
  c(mean=mean(x), std_error=sd(x)))
# Parallel bootstrap under Windows
library(parallel)  # Load package parallel
n_cores <- detectCores() - 1  # Number of cores
clus_ter <- makeCluster(n_cores)  # initialize compute cluster
boot_data <- parLapply(clus_ter, 1:1e4,
  function(x, da_ta) {
    sampl_e <- da_ta[sample.int(n_rows, replace=TRUE)]
    c(sd=sd(sampl_e), mad=mad(sampl_e))
  }, da_ta=da_ta)  # end parLapply
stopCluster(clus_ter)  # Stop R processes over cluster
# Parallel bootstrap under Mac-OSX or Linux
boot_data <- mclapply(1:1e4, function(x) {
    sampl_e <- da_ta[sample.int(n_rows, replace=TRUE)]
    c(sd=sd(sampl_e), mad=mad(sampl_e))
  }, mc.cores=n_cores)  # end mclapply
# Means and standard errors from bootstrap
boot_data <- rutils::do_call(rbind, boot_data)
apply(boot_data, MARGIN=2, function(x)
  c(mean=mean(x), std_error=sd(x)))
set.seed(1121)  # initialize random number generator
# Define variables and calculate correlation
n_rows <- 100
x_var <- runif(n_rows); y_var <- runif(n_rows)
cor(x_var, y_var)
# Correlate the variables and calculate correlation
rh_o <- 0.5
y_var <- rh_o*x_var + (1-rh_o)*y_var
# Plot in x11 window
x11(width=5, height=4)
# Set plot parameters to reduce whitespace around plot
par(mar=c(4, 4, 2, 1), oma=c(0.5, 0.5, 0, 0))
# Plot scatterplot and exact regression line
plot(x_var, y_var, xlab="x_var", ylab="y_var")
title(main="Correlated Variables", line=0.5)
abline(a=0.25, b=rh_o, lwd=3, col="blue")
# Calculate regression
summary(lm(y_var ~ x_var))
# Simulation of sample correlation
n_rows <- 1e4
rh_o <- 0.99
rho_2 <- sqrt(1-rh_o^2)
da_ta <- sapply(1:1000, function(x) {
  x_var <- rnorm(n_rows)
  y_var <- (rh_o*x_var + rho_2*rnorm(n_rows))
  cor(x_var, y_var)
})  # end sapply
sd(da_ta)
# Correct formula
(1-rh_o^2)/sqrt(n_rows-2)
# Incorrect formula
sqrt((1-rh_o^2)/(n_rows-2))
# Correlation
co_r <- cor(x_var, y_var)
# Standard error of correlation
std_error <- sqrt((1-co_r^2)/(n_rows-2))
# t-value of correlation
co_r/std_error
# 95% confidence intervals
co_r*c(1-qnorm(0.975)*std_error, 1+qnorm(0.975)*std_error)
# Test statistical significance of correlation
cor.test(x_var, y_var)
rh_o <- 0.9
rho_2 <- sqrt(1-rh_o^2)
set.seed(1121)
# Bootstrap of sample mean and median
boot_data <- sapply(1:1000, function(x) {
  x_var <- rnorm(n_rows)
  y_var <- (rh_o*x_var + rho_2*rnorm(n_rows))
  c(rho=mean(y_var*x_var), y_sd=sd(y_var), cor=cor(x_var, y_var))
})  # end sapply
# Means and standard errors from bootstrap
foo <- apply(boot_data, MARGIN=1, function(x)
  c(mean=mean(x), std_error=sd(x)))
foo[2, ]
(1-rh_o^2)/sqrt(n_rows-2)
sqrt((1-rh_o^2)/(n_rows-2))
rho_2^2
rh_o^4
# Simulation of sample correlation
rh_o <- 0.99
rho_2 <- sqrt(1-rh_o^2)
da_ta <- sapply(1:10000, function(x) {
  x_var <- rnorm(n_rows)
  y_var <- (rh_o*x_var + rho_2*rnorm(n_rows))
  cor(x_var, y_var)
})  # end sapply
sd(da_ta)
# Correct formula
(1-rh_o^2)/sqrt(n_rows-2)
# Incorrect formula
sqrt((1-rh_o^2)/(n_rows-2))
da_ta <- sapply(1:10000, function(x) {
  rnorm(n_rows)^2 * rnorm(n_rows)^2
})  # end sapply
sd(da_ta)
foo <- (rnorm(n_rows)^2 * rnorm(n_rows)^2)
mean(rnorm(n_rows)^2 * rnorm(n_rows)^2)
# Calculate correlations
cor(x_var, y_var, method="pearson")
cor(x_var, y_var, method="spearman")
# Test statistical significance of correlations
cor.test(x_var, y_var, method="pearson")
cor.test(x_var, y_var, method="spearman")
# Calculate correlations
cor(x_var, y_var, method="pearson")
cor(x_var, y_var, method="kendall")
# Test statistical significance of correlations
cor.test(x_var, y_var, method="pearson")
cor.test(x_var, y_var, method="kendall")
