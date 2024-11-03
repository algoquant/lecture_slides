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
# Define the Hermite polynomials
hermitep <- function(x, n) {
    switch(n+1, 1, x, (x^2 - 1), (x^3 - 3*x), 0)
}  # end hermitep
colorv <- c("red", "blue", "green")
for (indeks in 1:3) {  # Plot three curves
  curve(expr=hermitep(x, indeks),
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
# Define the Hermite functions
hermitef <- function(x, n)
  exp(-x^2/4)*hermitep(x, n)/(2*pi)^(0.25)/sqrt(factorial(n))
# Integrate the Hermite functions
integrate(function(x, n, m)
  hermitef(x, n)*hermitef(x, m),
  lower=(-Inf), upper=Inf, n=2, m=3)
x11(width=6, height=5)  # Plot in window
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
colorv <- c("red", "blue", "green")
for (indeks in 1:3) {  # Plot three curves
  curve(expr=hermitef(x, indeks),
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
integrate(hermitep, lower=(-Inf), upper=Inf, n=2)
integrate(function(x, n, m) hermitep(x, n)*hermitep(x, m),
    lower=(-Inf), upper=Inf, n=2, m=3)
integrate(function(x, n, m) hermitep(x, n)*hermitep(x, m),
    lower=(-Inf), upper=Inf, n=2, m=2)
x11(width=6, height=5)  # Plot in window
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
colorv <- c("red", "blue", "green")
for (indeks in 1:3) {  # Plot three curves
  curve(expr=hermitep(x, indeks),
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
