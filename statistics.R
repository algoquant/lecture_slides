# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
runif(3)  # three numbers from uniform distribution
runif(3)  # Simulate another three numbers
# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
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
# Calculate uniformly distributed pseudo-random sequence
# using logistic map function.
unifun <- function(seedv, n=10) {
  # Pre-allocate vector instead of "growing" it
  datav <- numeric(n)
  # initialize
  datav[1] <- seedv
  # Perform loop
  for (i in 2:n) {
    datav[i] <- 4*datav[i-1]*(1-datav[i-1])
  }  # end for
  acos(1-2*datav)/pi
}  # end unifun
unifun(seedv=0.1, n=15)
plot(
  density(unifun(seedv=runif(1), n=1e5)),
  xlab="", ylab="", lwd=2, col="blue",
  main="uniform pseudo-random number density")
# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
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
# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
# Sample from Standard Normal Distribution
datav <- rnorm(1000)
mean(datav)  # Sample mean
median(datav)  # Sample median
sd(datav)  # Sample standard deviation
# VTI returns
retp <- na.omit(rutils::etfenv$returns$VTI)
# Number of observations
nrows <- NROW(retp)
# Mean of VTI returns
meanv <- mean(retp)
# Standard deviation of VTI returns
stdev <- sd(retp)
# Standardize returns
retp <- (retp - meanv)/stdev
# Skewness and kurtosis of VTI returns
nrows/((nrows-1)*(nrows-2))*sum(retp^3)
nrows/(nrows-1)^2*sum(retp^4)
# Random normal returns
retp <- rnorm(nrows)
# Mean and standard deviation of random normal returns
mean(retp); sd(retp)
# Skewness and kurtosis of random normal returns
nrows/((nrows-1)*(nrows-2))*sum(retp^3)
nrows/(nrows-1)^2*sum(retp^4)
# Calculate cumulative standard normal distribution
c(pnorm(-2), pnorm(2))
# Calculate inverse cumulative standard normal distribution
c(qnorm(0.75), qnorm(0.25))
# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
# Sample from Standard Normal Distribution
nrows <- 1000
datav <- rnorm(nrows)
# Sample mean - MC estimate
mean(datav)
# Sample standard deviation - MC estimate
sd(datav)
# Monte Carlo estimate of cumulative probability
c(pnorm(1), sum(datav < 1)/nrows)
# Monte Carlo estimate of quantile
confl <- 0.99
qnorm(confl)
cutoff <- confl*nrows
datav <- sort(datav)
c(datav[cutoff], quantile(datav, probs=confl))
# Read the source code of quantile()
stats:::quantile.default
# microbenchmark quantile
library(microbenchmark)
summary(microbenchmark(
  monte_carlo=datav[cutoff],
  quantv=quantile(datav, probs=confl),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
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
# Define Hermite polynomials
her_mite <- function(x, n) {
    switch(n+1, 1, x, (x^2 - 1), (x^3 - 3*x), 0)
}  # end her_mite
colorv <- c("red", "blue", "green")
for (indeks in 1:3) {  # Plot three curves
  curve(expr=her_mite(x, indeks),
  xlim=c(-3, 3), ylim=c(-2.5, 2.5),
  xlab="", ylab="", lwd=4, col=colorv[indeks],
  add=as.logical(indeks-1))
}  # end for
# Add title and legend
title(main="Hermite Polynomials", line=0.5)
labelv <- paste("Order", 1:3, sep=" = ")
legend("top", inset=0.0, bty="n",
 title=NULL, labelv, cex=0.8, lwd=6, lty=1, 
 col=colorv)
# Define Hermite functions
hermite_fun <- function(x, n)
  exp(-x^2/4)*her_mite(x, n)/(2*pi)^(0.25)/sqrt(factorial(n))
# Integrate Hermite functions
integrate(function(x, n, m)
  hermite_fun(x, n)*hermite_fun(x, m),
  lower=(-Inf), upper=Inf, n=2, m=3)
x11(width=6, height=5)  # Plot in window
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
colorv <- c("red", "blue", "green")
for (indeks in 1:3) {  # Plot three curves
  curve(expr=hermite_fun(x, indeks),
  xlim=c(-6, 6), ylim=c(-0.6, 0.6),
  xlab="", ylab="", lwd=4, col=colorv[indeks],
  add=as.logical(indeks-1))
}  # end for
# Add title and legend
title(main="Hermite Functions", line=0.5)
labelv <- paste("Order", 1:3, sep=" = ")
legend("topright", inset=0.0, bty="n",
 title=NULL, labelv, cex=0.8, lwd=6, lty=1,
 col=colorv)
# Integrate Hermite functions
integrate(her_mite, lower=(-Inf), upper=Inf, n=2)
integrate(function(x, n, m) her_mite(x, n)*her_mite(x, m),
    lower=(-Inf), upper=Inf, n=2, m=3)
integrate(function(x, n, m) her_mite(x, n)*her_mite(x, m),
    lower=(-Inf), upper=Inf, n=2, m=2)
x11(width=6, height=5)  # Plot in window
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
colorv <- c("red", "blue", "green")
for (indeks in 1:3) {  # Plot three curves
  curve(expr=her_mite(x, indeks),
  xlim=c(-4, 4), ylim=c(-0.6, 0.6),
  xlab="", ylab="", lwd=3, col=colorv,
  add=as.logical(indeks-1))
}  # end for
# Add title and legend
title(main="Hermite Functions", line=0.5)
labelv <- paste("Order", 1:3, sep=" = ")
legend("topright", inset=0.05, bty="n",
 title=NULL, labelv, cex=0.8, lwd=6, lty=1,
 col=colorv)
# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
# Sample from Standard Normal Distribution
nrows <- 1000
datav <- rnorm(nrows)
# Sample mean
mean(datav)
# Sample standard deviation
sd(datav)
#Perform two-tailed test that sample is
#from Standard Normal Distribution (mean=0, SD=1)
# generate vector of samples and store in data frame
test_frame <- data.frame(samples=rnorm(1e4))
# get p-values for all the samples
test_frame$p_values <- sapply(test_frame$samples,
        function(x) 2*pnorm(-abs(x)))
# Significance level, two-tailed test, critical value=2*SD
signif_confl <- 2*(1-pnorm(2))
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
x1 <- 2; x2 <- 4
# Plot right tail using polygon
xvar <- seq(x1, x2, length=100)
yxvar <- dnorm(xvar, sd=1)
yxvar[1] <- (-1)
yxvar[NROW(yxvar)] <- (-1)
polygon(x=xvar, y=yxvar, col="red")
# Plot left tail using polygon
yxvar <- dnorm(-xvar, sd=1)
yxvar[1] <- (-1)
yxvar[NROW(yxvar)] <- (-1)
polygon(x=(-xvar), y=yxvar, col="red")
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
xvar <- -400:400/100
norm_frame <- data.frame(xvar=xvar,
                 d.norm=dnorm(xvar))
norm_frame$shade <- ifelse(
            abs(norm_frame$xvar) >= 2,
            norm_frame$d.norm, NA)
ggplot(  # Main function
  data=norm_frame,
  mapping=aes(x=xvar, y=d.norm)
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
retp <- as.numeric(na.omit(rutils::etfenv$returns$VTI))[1:499]
# Reduce number of output digits
ndigits <- options(digits=5)
# Shapiro-Wilk test for normal distribution
nrows <- NROW(retp)
shapiro.test(rnorm(nrows))
# Shapiro-Wilk test for VTI returns
shapiro.test(retp)
# Shapiro-Wilk test for uniform distribution
shapiro.test(runif(nrows))
# Restore output digits
options(digits=ndigits$digits)
library(tseries)  # Load package tseries
# Jarque-Bera test for normal distribution
jarque.bera.test(rnorm(nrows))
# Jarque-Bera test for VTI returns
jarque.bera.test(retp)
# Jarque-Bera test for uniform distribution
jarque.bera.test(runif(nrows))
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
for (indeks in 1:4) {
  curve(expr=dchisq(x, df=degf[indeks]),
  xlim=c(0, 20), ylim=c(0, 0.3),
  xlab="", ylab="", col=colorv[indeks],
  lwd=2, add=as.logical(indeks-1))
}  # end for
# Add title
title(main="Chi-squared Distributions", line=0.5)
# Add legend
labelv <- paste("df", degf, sep="=")
legend("topright", inset=0.05, bty="n",
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
countst <- pt((histp$breaks-loc)/scalev, df=2)
countst <- rutils::diffit(countst)
# Perform Chi-squared test for VTI returns
chisq.test(x=countsn, p=countst, rescale.p=TRUE, simulate.p.value=TRUE)
# Sort a vector into ascending order
datav <- round(runif(7), 3)
sortv <- sort(datav)
datav  # original data
sortv  # sorted data
# Calculate index to sort into ascending order
indeks <- order(datav)
indeks  # permutation index to sort
all.equal(sortv, datav[indeks])
# Sort the ordered vector back to its original unsorted order
indeks <- order(order(datav))
indeks  # permutation index to unsort
all.equal(datav, sortv[indeks])
# Calculate ranks of the vector elements
rank(datav)
all.equal(rank(datav), indeks)
# VTI returns
retp <- as.numeric(na.omit(rutils::etfenv$returns[, "VTI"]))
nrows <- NROW(retp)
retp <- 100*(retp-mean(retp))/sd(retp)
# Simulate normal random data
ndata <- rnorm(nrows, sd=100)
# Bootstrap the mean and median estimators
bootd <- sapply(1:1e3, function(x) {
  # Simulate data
  ndata <- rnorm(nrows, sd=100)
  retp <- retp[sample.int(nrows, replace=TRUE)]
  c(n_mean=mean(ndata),
    n_median=median(ndata),
    vti_mean=mean(retp),
    vti_median=median(retp))
})  # end sapply
bootd <- t(bootd)
# Analyze bootstrapped data
head(bootd)
sum(is.na(bootd))
# Means and medians from bootstrap
apply(bootd, MARGIN=2, function(x)
  c(mean=mean(x), stderror=sd(x)))
# Parallel bootstrap under Windows
library(parallel)  # Load package parallel
ncores <- detectCores() - 1  # Number of cores
compclust <- makeCluster(ncores)  # initialize compute cluster
bootd <- parLapply(compclust, 1:1e4,
  function(x, nrows, retp) {
    # Simulate data
    ndata <- rnorm(nrows, sd=100)
    retp <- retp[sample.int(nrows, replace=TRUE)]
    c(n_mean=mean(ndata),
n_median=median(ndata),
vti_mean=mean(retp),
vti_median=median(retp))
  }, nrows, retp)  # end parLapply
stopCluster(compclust)  # Stop R processes over cluster
# Parallel bootstrap under Mac-OSX or Linux
bootd <- mclapply(1:1e4, function(x) {
  # Simulate data
  ndata <- rnorm(nrows)
  datav <- rt(nrows, df=2)
  c(n_mean=mean(ndata),
    n_median=median(ndata),
    vti_mean=mean(datav),
    vti_median=median(datav))
}, mc.cores=ncores)  # end mclapply
# Means and medians from bootstrap
bootd <- rutils::do_call(rbind, bootd)
apply(bootd, MARGIN=2, function(x)
  c(mean=mean(x), stderror=sd(x)))
# Bias and variance from bootstrap
bias_var <- apply(bootd, MARGIN=2,
  function(x) c(bias=mean(x), variance=var(x)))
# MSE of mean
bias_var[1, 3]^2 + bias_var[2, 3]
# MSE of median
bias_var[1, 4]^2 + bias_var[2, 4]
retp <- as.numeric(na.omit(rutils::etfenv$returns[, "VTI"]))
nrows <- NROW(retp)
retp <- 100*(retp-mean(retp))/sd(retp)
# Simulate normal random data
ndata <- rnorm(nrows, sd=100)
# Hodges-Lehmann estimator
# Bootstrap the mean and median estimators
bootd <- sapply(1:1e3, function(x) {
  # Simulate data
  ndata <- rnorm(nrows, sd=100)
  retp <- retp[sample.int(nrows, replace=TRUE)]
  c(n_mean=mean(ndata),
    n_median=median(ndata),
    vti_mean=mean(retp),
    vti_median=median(retp))
})  # end sapply
bootd <- t(bootd)
# Analyze bootstrapped data
head(bootd)
sum(is.na(bootd))
# Means and medians from bootstrap
apply(bootd, MARGIN=2, function(x)
  c(mean=mean(x), stderror=sd(x)))
nrows <- 1e3
datav <- rnorm(nrows)
sd(datav)
mad(datav)
median(abs(datav - median(datav)))
median(abs(datav - median(datav)))/qnorm(0.75)
# Bootstrap of sd and mad estimators
bootd <- sapply(1:1e4, function(x) {
  samplev <- datav[sample.int(nrows, replace=TRUE)]
  c(sd=sd(samplev), mad=mad(samplev))
})  # end sapply
bootd <- t(bootd)
# Analyze bootstrapped variance
head(bootd)
sum(is.na(bootd))
# Means and standard errors from bootstrap
apply(bootd, MARGIN=2, function(x)
  c(mean=mean(x), stderror=sd(x)))
# Parallel bootstrap under Windows
library(parallel)  # Load package parallel
ncores <- detectCores() - 1  # Number of cores
compclust <- makeCluster(ncores)  # initialize compute cluster
bootd <- parLapply(compclust, 1:1e4,
  function(x, datav) {
    samplev <- datav[sample.int(nrows, replace=TRUE)]
    c(sd=sd(samplev), mad=mad(samplev))
  }, datav=datav)  # end parLapply
stopCluster(compclust)  # Stop R processes over cluster
# Parallel bootstrap under Mac-OSX or Linux
bootd <- mclapply(1:1e4, function(x) {
    samplev <- datav[sample.int(nrows, replace=TRUE)]
    c(sd=sd(samplev), mad=mad(samplev))
  }, mc.cores=ncores)  # end mclapply
# Means and standard errors from bootstrap
bootd <- rutils::do_call(rbind, bootd)
apply(bootd, MARGIN=2, function(x)
  c(mean=mean(x), stderror=sd(x)))
# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
# Sample from Standard Normal Distribution
nrows <- 1e3
datav <- rnorm(nrows)
# Sample mean
mean(datav)
# Sample standard deviation
sd(datav)
nrows <- 1e3
# Wilcoxon test for normal distribution
normv <- rnorm(nrows)
wilcox.test(normv)
wilcox.test(normv+1)
# Skewed distribution with median=0
lognormv <- exp(normv)
# lognormv <- rlnorm(nrows, sdlog=1)
mean(lognormv); median(lognormv)
# Skewed distribution with median!=0
wilcox.test(lognormv)
# Skewed distribution with median=0
wilcox.test(lognormv-median(lognormv))
# Skewed distributions with median!=0
wilcox.test(lognormv, normv, paired=TRUE)
# Two distributions with median=0
wilcox.test(lognormv-median(lognormv), normv-median(normv),
      paired=TRUE)
# Skewed distribution with median=0
wilcox.test(lognormv-median(lognormv))
# Skewed distribution with mean=0
mean(lognormv); median(lognormv)
wilcox.test(lognormv-median(lognormv))
# Same as
wilcox.test(lognormv-median(lognormv), rep(0, nrows), paired=TRUE)
# Skewed distributions with median!=0
wilcox.test(lognormv, normv, paired=TRUE)
# Two distributions with median=0
wilcox.test(lognormv-median(lognormv), normv-median(normv),
      paired=TRUE)
# Normal samples with different standard deviations
sample1 <- rnorm(nrows, sd=1)
sample2 <- rnorm(nrows, sd=10)
wilcox.test(sample1, sample2, paired=TRUE)
# Wilcoxon test for random data around 0
unifv <- (runif(nrows) - 0.5)
wilcoxt <- wilcox.test(unifv)
# Calculate V statistic of Wilcoxon test
wilcoxt$statistic
sum(rank(abs(unifv))[unifv>0])
# Calculate W statistic of Wilcoxon test
sum(sign(unifv)*rank(abs(unifv)))
# Two sets of normal data
sample1 <- rnorm(nrows)
sample2 <- rnorm(nrows, mean=0.1)
# Wilcoxon test
wilcoxt <- wilcox.test(sample1, sample2, paired=TRUE)
wilcoxt$statistic
# Calculate V statistic of Wilcoxon test
datav <- (sample1 - sample2)
sum(rank(abs(datav))[datav>0])
# Calculate W statistic of Wilcoxon test
sum(sign(datav)*rank(abs(datav)))
# Calculate distributon of Wilcoxon W statistic
wilcoxw <- sapply(1:1e3, function(x) {
  datav <- (runif(nrows) - 0.5)
  sum(sign(datav)*rank(abs(datav)))
})  # end sapply
wilcoxw <- wilcoxw/sqrt(nrows*(nrows+1)*(2*nrows+1)/6)
var(wilcoxw)
x11(width=6, height=5)  # Plot in window
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
hist(wilcoxw, col="lightgrey",
     xlab="returns", breaks=50, xlim=c(-3, 3),
     ylab="frequency", freq=FALSE,
     main="Wilcoxon W Statistic Histogram")
lines(density(wilcoxw, bw=0.4), lwd=3, col="red")
curve(expr=dnorm, add=TRUE, lwd=3, col="blue")
# Add legend
legend("topright", inset=0.05, bty="n",
 leg=c("W density", "Normal"),
 lwd=6, lty=1, col=c("red", "blue"))
nrows <- 1e3
# Wilcoxon test for normal distribution
normv <- rnorm(nrows)
wilcox.test(normv)
wilcox.test(normv+1)
# Skewed distribution with median=0
lognormv <- rlnorm(nrows, sdlog=1)
mean(lognormv); median(lognormv)
# Skewed distribution with median!=0
wilcox.test(lognormv)
# Skewed distribution with median=0
wilcox.test(lognormv-median(lognormv))
# Skewed distributions with median!=0
wilcox.test(lognormv, normv, paired=TRUE)
# Two distributions with median=0
wilcox.test(lognormv-median(lognormv), normv-median(normv),
      paired=TRUE)
# Skewed distribution with median=0
wilcox.test(lognormv-median(lognormv))
# Skewed distribution with mean=0
mean(lognormv); median(lognormv)
wilcox.test(lognormv-median(lognormv))
# Same as
wilcox.test(lognormv-median(lognormv), rep(0, nrows), paired=TRUE)
# Skewed distributions with median!=0
wilcox.test(lognormv, normv, paired=TRUE)
# Two distributions with median=0
wilcox.test(lognormv-median(lognormv), normv-median(normv),
      paired=TRUE)
# Normal samples with different standard deviations
sample1 <- rnorm(nrows, sd=1)
sample2 <- rnorm(nrows, sd=10)
wilcox.test(sample1, sample2, paired=TRUE)
# Wilcoxon test for random data around 0
unifv <- (runif(nrows) - 0.5)
wilcoxt <- wilcox.test(unifv)
# Calculate V statistic of Wilcoxon test
wilcoxt$statistic
sum(rank(abs(unifv))[unifv>0])
# Calculate W statistic of Wilcoxon test
sum(sign(unifv)*rank(abs(unifv)))
# Two sets of normal data
sample1 <- rnorm(nrows)
sample2 <- rnorm(nrows, mean=0.1)
# Wilcoxon test
wilcoxt <- wilcox.test(sample1, sample2, paired=TRUE)
wilcoxt$statistic
# Calculate V statistic of Wilcoxon test
datav <- (sample1 - sample2)
sum(rank(abs(datav))[datav>0])
# Calculate W statistic of Wilcoxon test
sum(sign(datav)*rank(abs(datav)))
# Calculate distributon of Wilcoxon W statistic
wilcoxw <- sapply(1:1e3, function(x) {
  datav <- (runif(nrows) - 0.5)
  sum(sign(datav)*rank(abs(datav)))
})  # end sapply
wilcoxw <- wilcoxw/sqrt(nrows*(nrows+1)*(2*nrows+1)/6)
var(wilcoxw)
x11(width=6, height=5)  # Plot in window
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
hist(wilcoxw, col="lightgrey",
     xlab="returns", breaks=50, xlim=c(-3, 3),
     ylab="frequency", freq=FALSE,
     main="Wilcoxon W Statistic Histogram")
lines(density(wilcoxw, bw=0.4), lwd=3, col="red")
curve(expr=dnorm, add=TRUE, lwd=3, col="blue")
# Add legend
legend("topright", inset=0.05, bty="n",
 leg=c("W density", "Normal"),
 lwd=6, lty=1, col=c("red", "blue"))
# Wilcoxon test for two normal distributions
sample1 <- rnorm(1e2)
sample2 <- rnorm(1e2, mean=0.1)
wilcox.test(sample1, sample2, paired=TRUE)$p.value
t.test(sample1, sample2)$p.value
# Wilcoxon test with data outliers
sample2 <- rnorm(1e2)
sample2[1:3] <- sample2[1:3] + 1e3
wilcox.test(sample1, sample2, paired=TRUE)$p.value
t.test(sample1, sample2)$p.value
# Data samples
datav <- sort(rnorm(38))
indeks <- c(1:9, 20:29)
# Or
datav <- sort(rnorm(398))
indeks <- c(1:99, 200:299)
sample1 <- datav[indeks]
sample2 <- datav[-indeks]
# Or
indeks <- sample(1:nrows, size=nrows/2)
sample1 <- retp[indeks]
sample2 <- (-retp[-indeks])
sample1 <- rpois(1e2, lambda=4)
sample1 <- rnorm(1e2)
sample1 <- (sample1- median(sample1))
sample2 <- runif(1e2)
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
datav <- rnorm(nrows, sd=100)
wilcox.test(datav, paired=FALSE)
# Skewed distribution with mean=0
mean(retp); median(retp)
wilcox.test(retp-mean(retp), paired=FALSE)
# Skewed distribution with median=0
wilcox.test(retp-median(retp), paired=FALSE)
# Skewed distribution with median=0
wilcox.test(retp-median(retp), datav, paired=FALSE)
sample1 <- sample(retp, size=nrows)
sample2 <- (-sample1)
sample1 <- (sample1-median(sample1))
sample2 <- (sample2-median(sample2))
# Mann-Whitney-Wilcoxon rank sum test
wilcox.test(sample1, sample2, paired=FALSE)$p.value
datav <- (-retp)
datav <- (datav-median(datav))
wilcox.test(retp-median(retp), datav, paired=FALSE)
wilcox.test(retp-median(retp), datav, paired=TRUE)
datav <- (-retp)
datav <- (datav-mean(datav))
wilcox.test(retp-mean(retp), datav, paired=FALSE)
wilcox.test(retp-mean(retp), datav, paired=TRUE)
foo <- sapply(1:100, function(x) {
  # Data samples
  sample1 <- sample(retp, size=nrows/2)
  sample2 <- sample(-retp, size=nrows/2)
  sample1 <- (sample1-median(sample1))
  sample2 <- (sample2-median(sample2))
  # Mann-Whitney-Wilcoxon rank sum test
  wilcox.test(sample1, sample2, paired=FALSE)$p.value
})
hist(foo)
# Skewed distribution with mean=0
mean(retp); median(retp)
wilcox.test(retp-mean(retp))
# Same as
wilcox.test(retp-mean(retp), rep(0, nrows), paired=TRUE)
# Skewed distribution with median=0
wilcox.test(retp-median(retp))
# Data samples
sample1 <- rnorm(200)
sample2 <- rnorm(100, mean=0.1)
# Mann-Whitney-Wilcoxon rank sum test
wilcoxt <- wilcox.test(sample1, sample2,
                 paired=FALSE)
wilcoxt$statistic
# Calculate U statistics of Mann-Whitney-Wilcoxon test
datav <- c(sample1, sample2)
ranks <- rank(datav)
sum(ranks[1:200]) - 100*201
sum(ranks[201:300]) - 50*101
# Wilcoxon test for random data around 0
nrows <- 1e3
datav <- (runif(nrows) - 0.5)
wilcoxt <- wilcox.test(datav)
# Calculate V statistic of Wilcoxon test
wilcoxt$statistic
sum(rank(abs(datav))[datav>0])
# Calculate W statistic of Wilcoxon test
sum(sign(datav)*rank(abs(datav)))
# Calculate distributon of Wilcoxon W statistic
wilcoxw <- sapply(1:1e3, function(x) {
  datav <- (runif(nrows) - 0.5)
  sum(sign(datav)*rank(abs(datav)))
})  # end sapply
wilcoxw <- wilcoxw/sqrt(nrows*(nrows+1)*(2*nrows+1)/6)
var(wilcoxw)
hist(wilcoxw)
# iris data frame
aggregate(Sepal.Length ~ Species, data=iris,
  FUN=function(x) c(mean=mean(x), sd=sd(x)))
# Kruskal-Wallis test for iris data
ktest <- kruskal.test(Sepal.Length ~ Species, data=iris)
str(ktest)
ktest$statistic
# Kruskal-Wallis test for independent normal distributions
sample1 <- rnorm(1e3)
sample2 <- rnorm(1e3)
groupv <- c(rep(TRUE, 1e3), rep(FALSE, 1e3))
kruskal.test(x=c(sample1, sample2), g=groupv)
# Kruskal-Wallis test for shifted normal distributions
kruskal.test(x=c(sample1+1, sample2), g=groupv)
# Kruskal-Wallis test for beta distributions
sample1 <- rbeta(1e3, 2, 8) + 0.3
sample2 <- rbeta(1e3, 8, 2) - 0.3
mean(sample1); mean(sample2)
kruskal.test(x=c(sample1, sample2), g=groupv)
# Plot the beta distributions
x11()
plot(density(sample1), col="blue", lwd=3,
     xlim=range(c(sample1, sample2)), xlab="samples",
     main="Two samples from beta distributions with equal means")
lines(density(sample2), col="red", lwd=3)
# Kruskal-Wallis test for iris data
ktest <- kruskal.test(Sepal.Length ~ Species, data=iris)
# Calculate Kruskal-Wallis test Statistic
nrows <- NROW(iris)
irisdf <- data.frame(ranks=rank(iris$Sepal.Length),
                  species=iris$Species)
kruskal_stat <- (12/nrows/(nrows+1))*sum(
  aggregate(ranks ~ species, data=irisdf,
      FUN=function(x) {NROW(x)*((nrows+1)/2 - mean(x))^2})[, 2])
c(ktest=unname(ktest$statistic), k_stat=kruskal_stat)
# Kruskal-Wallis test with data outliers
sample1 <- rnorm(1e3)
sample2 <- rnorm(1e3)
sample2[1:11] <- sample2[1:11] + 50
groupv <- c(rep(TRUE, 1e3), rep(FALSE, 1e3))
kruskal.test(x=c(sample1, sample2), g=groupv)$p.value
t.test(sample1, sample2)$p.value
nrows <- 1e3
datav <- rnorm(nrows)
sd(datav)
mad(datav)
median(abs(datav - median(datav)))
median(abs(datav - median(datav)))/qnorm(0.75)
# Bootstrap of sd and mad estimators
bootd <- sapply(1:1e4, function(x) {
  samplev <- datav[sample.int(nrows, replace=TRUE)]
  c(sd=sd(samplev), mad=mad(samplev))
})  # end sapply
bootd <- t(bootd)
# Analyze bootstrapped variance
head(bootd)
sum(is.na(bootd))
# Means and standard errors from bootstrap
apply(bootd, MARGIN=2,
function(x) c(mean=mean(x), stderror=sd(x)))
# Parallel bootstrap under Windows
library(parallel)  # Load package parallel
ncores <- detectCores() - 1  # Number of cores
compclust <- makeCluster(ncores)  # initialize compute cluster
bootd <- parLapply(compclust, 1:1e4,
  function(x, datav) {
    samplev <- datav[sample.int(nrows, replace=TRUE)]
    c(sd=sd(samplev), mad=mad(samplev))
  }, datav=datav)  # end parLapply
stopCluster(compclust)  # Stop R processes over cluster
# Parallel bootstrap under Mac-OSX or Linux
bootd <- mclapply(1:1e4,
  function(x) {
    samplev <- datav[sample.int(nrows, replace=TRUE)]
    c(sd=sd(samplev), mad=mad(samplev))
  }, mc.cores=ncores)  # end mclapply
# Means and standard errors from bootstrap
bootd <- rutils::do_call(rbind, bootd)
apply(bootd, MARGIN=2, function(x)
  c(mean=mean(x), stderror=sd(x)))
nrows <- 1e3
datav <- rnorm(nrows)
sd(datav)
mad(datav)
median(abs(datav - median(datav)))
median(abs(datav - median(datav)))/qnorm(0.75)
# Bootstrap of sd and mad estimators
bootd <- sapply(1:1e4, function(x) {
  samplev <- datav[sample.int(nrows, replace=TRUE)]
  c(sd=sd(samplev), mad=mad(samplev))
})  # end sapply
bootd <- t(bootd)
# Analyze bootstrapped variance
head(bootd)
sum(is.na(bootd))
# Means and standard errors from bootstrap
apply(bootd, MARGIN=2, function(x)
  c(mean=mean(x), stderror=sd(x)))
# Parallel bootstrap under Windows
library(parallel)  # Load package parallel
ncores <- detectCores() - 1  # Number of cores
compclust <- makeCluster(ncores)  # initialize compute cluster
bootd <- parLapply(compclust, 1:1e4,
  function(x, datav) {
    samplev <- datav[sample.int(nrows, replace=TRUE)]
    c(sd=sd(samplev), mad=mad(samplev))
  }, datav=datav)  # end parLapply
stopCluster(compclust)  # Stop R processes over cluster
# Parallel bootstrap under Mac-OSX or Linux
bootd <- mclapply(1:1e4, function(x) {
    samplev <- datav[sample.int(nrows, replace=TRUE)]
    c(sd=sd(samplev), mad=mad(samplev))
  }, mc.cores=ncores)  # end mclapply
# Means and standard errors from bootstrap
bootd <- rutils::do_call(rbind, bootd)
apply(bootd, MARGIN=2, function(x)
  c(mean=mean(x), stderror=sd(x)))
# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
# Define variables and calculate correlation
nrows <- 100
xvar <- runif(nrows); yxvar <- runif(nrows)
cor(xvar, yxvar)
# Correlate the variables and calculate correlation
rho <- 0.5
yxvar <- rho*xvar + (1-rho)*yxvar
# Plot in x11 window
x11(width=5, height=4)
# Set plot parameters to reduce whitespace around plot
par(mar=c(4, 4, 2, 1), oma=c(0.5, 0.5, 0, 0))
# Plot scatterplot and exact regression line
plot(xvar, yxvar, xlab="xvar", ylab="yxvar")
title(main="Correlated Variables", line=0.5)
abline(a=0.25, b=rho, lwd=3, col="blue")
# Calculate regression
summary(lm(yxvar ~ xvar))
# Simulation of sample correlation
nrows <- 1e4
rho <- 0.99
rho2 <- sqrt(1-rho^2)
datav <- sapply(1:1000, function(x) {
  xvar <- rnorm(nrows)
  yxvar <- (rho*xvar + rho2*rnorm(nrows))
  cor(xvar, yxvar)
})  # end sapply
sd(datav)
# Correct formula
(1-rho^2)/sqrt(nrows-2)
# Incorrect formula
sqrt((1-rho^2)/(nrows-2))
# Correlation
corv <- cor(xvar, yxvar)
# Standard error of correlation
stderror <- sqrt((1-corv^2)/(nrows-2))
# t-value of correlation
corv/stderror
# 95% confidence intervals
corv*c(1-qnorm(0.975)*stderror, 1+qnorm(0.975)*stderror)
# Test statistical significance of correlation
cor.test(xvar, yxvar)
rho <- 0.9
rho2 <- sqrt(1-rho^2)
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
# Bootstrap of sample mean and median
bootd <- sapply(1:1000, function(x) {
  xvar <- rnorm(nrows)
  yxvar <- (rho*xvar + rho2*rnorm(nrows))
  c(rho=mean(yxvar*xvar), y_sd=sd(yxvar), cor=cor(xvar, yxvar))
})  # end sapply
# Means and standard errors from bootstrap
foo <- apply(bootd, MARGIN=1, function(x)
  c(mean=mean(x), stderror=sd(x)))
foo[2, ]
(1-rho^2)/sqrt(nrows-2)
sqrt((1-rho^2)/(nrows-2))
rho2^2
rho^4
# Simulation of sample correlation
rho <- 0.99
rho2 <- sqrt(1-rho^2)
datav <- sapply(1:10000, function(x) {
  xvar <- rnorm(nrows)
  yxvar <- (rho*xvar + rho2*rnorm(nrows))
  cor(xvar, yxvar)
})  # end sapply
sd(datav)
# Correct formula
(1-rho^2)/sqrt(nrows-2)
# Incorrect formula
sqrt((1-rho^2)/(nrows-2))
datav <- sapply(1:10000, function(x) {
  rnorm(nrows)^2 * rnorm(nrows)^2
})  # end sapply
sd(datav)
foo <- (rnorm(nrows)^2 * rnorm(nrows)^2)
mean(rnorm(nrows)^2 * rnorm(nrows)^2)
# Calculate correlations
cor(xvar, yxvar, method="pearson")
cor(xvar, yxvar, method="spearman")
# Test statistical significance of correlations
cor.test(xvar, yxvar, method="pearson")
cor.test(xvar, yxvar, method="spearman")
# Calculate correlations
cor(xvar, yxvar, method="pearson")
cor(xvar, yxvar, method="kendall")
# Test statistical significance of correlations
cor.test(xvar, yxvar, method="pearson")
cor.test(xvar, yxvar, method="kendall")
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
# Calculate the kernel density
densv <- sapply(1:nrows, function(it) {
  sum(dnorm(retp-retp[it], sd=bwidth))
})  # end sapply
madv <- mad(retp)
plot(retp, densv, xlim=c(-5*madv, 5*madv),
     t="l", col="blue", lwd=3,
     xlab="returns", ylab="density",
     main="Density of VTI Returns")
# Calculate the kernel density using density()
densv <- density(retp, bw=bwidth)
NROW(densv$y)
x11(width=6, height=5)
plot(densv, xlim=c(-5*madv, 5*madv),
     xlab="returns", ylab="density",
     col="blue", lwd=3, main="Density of VTI Returns")
# Interpolate the densv vector into returns
densv <- approx(densv$x, densv$y, xout=retp)
all.equal(densv$x, retp)
plot(densv, xlim=c(-5*madv, 5*madv),
     xlab="returns", ylab="density",
     t="l", col="blue", lwd=3,
     main="Density of VTI Returns")
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
library(rutils)  # Load package rutils
# Calculate VTI percentage returns
retp <- na.omit(rutils::etfenv$returns$VTI)
# Mean and standard deviation of returns
c(mean(retp), sd(retp))
# Plot histogram
x11(width=6, height=5)
par(mar=c(1, 1, 1, 1), oma=c(2, 2, 2, 0))
madv <- mad(retp)
histp <- hist(retp, breaks=100,
  main="", xlim=c(-5*madv, 5*madv),
  xlab="", ylab="", freq=FALSE)
# Draw kernel density of histogram
lines(density(retp), col="red", lwd=2)
# Add density of normal distribution
curve(expr=dnorm(x, mean=mean(retp), sd=sd(retp)),
add=TRUE, type="l", lwd=2, col="blue")
title(main="VTI Return Distribution", line=0)  # Add title
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
# Calculate VTI percentage returns
library(rutils)
retp <- as.numeric(na.omit(rutils::etfenv$returns$VTI))[1:499]
# Reduce number of output digits
ndigits <- options(digits=5)
# Shapiro-Wilk test for normal distribution
nrows <- NROW(retp)
shapiro.test(rnorm(nrows))
# Shapiro-Wilk test for VTI returns
shapiro.test(retp)
# Shapiro-Wilk test for uniform distribution
shapiro.test(runif(nrows))
# Restore output digits
options(digits=ndigits$digits)
library(tseries)  # Load package tseries
# Jarque-Bera test for normal distribution
jarque.bera.test(rnorm(nrows))
# Jarque-Bera test for VTI returns
jarque.bera.test(retp)
# Jarque-Bera test for uniform distribution
jarque.bera.test(runif(NROW(retp)))
# KS test for normal distribution
ks_test <- ks.test(rnorm(100), pnorm)
ks_test$p.value
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
# KS test for VTI returns vs t-distribution data
ks.test(retp, datat)
# Define cumulative distribution of non-standard t-distribution
ptdistr <- function(x, dfree, locv=0, scalev=1) {
  pt((x-locv)/scalev, df=dfree)
}  # end ptdistr
# KS test for VTI returns vs cumulative t-distribution
ks.test(sample(retp, replace=TRUE), ptdistr, dfree=3, locv=locv, scalev=scalev)
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
varv <- HighFreq::roll_var_ohlc(log(ohlc), method="close", lookb=lookb, scale=FALSE)
varv[1:lookb, ] <- varv[lookb+1, ]
# Calculate trailing average volume
volumr <- HighFreq::roll_sum(volumv, lookb=lookb)/lookb
# dygraph plot of VTI variance and trading volumes
datav <- xts::xts(cbind(varv, volumr), zoo::index(ohlc))
colnamev <- c("variance", "volume")
colnames(datav) <- colnamev
dygraphs::dygraph(datav, main="VTI Variance and Trading Volumes") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], strokeWidth=2, axis="y", col="blue") %>%
  dySeries(name=colnamev[2], strokeWidth=2, axis="y2", col="red") %>%
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
