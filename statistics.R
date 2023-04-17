






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

# Calculate uniformly distributed pseudo-random sequence
# using logistic map function.
unifun <- function(seedv, n=10) {
  # Pre-allocate vector instead of "growing" it
  output <- numeric(n)
  # initialize
  output[1] <- seedv
  # Perform loop
  for (i in 2:n) {
    output[i] <- 4*output[i-1]*(1-output[i-1])
  }  # end for
  acos(1-2*output)/pi
}  # end unifun

unifun(seedv=0.1, n=15)
plot(
  density(unifun(seedv=runif(1), n=1e5)),
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
datav <- rnorm(1000)

mean(datav)  # Sample mean

median(datav)  # Sample median

sd(datav)  # Sample standard deviation

rm(list=ls())
# VTI returns
returns <- na.omit(rutils::etfenv$returns$VTI)
# Number of observations
nrows <- NROW(returns)
# Mean of VTI returns
mea_n <- mean(returns)
# Standard deviation of VTI returns
s_d <- sd(returns)
# Standardize returns
returns <- (returns - mea_n)/s_d
# Skewness and kurtosis of VTI returns
nrows/((nrows-1)*(nrows-2))*sum(returns^3)
nrows/(nrows-1)^2*sum(returns^4)
# Random normal returns
returns <- rnorm(nrows)
# Mean and standard deviation of random normal returns
mean(returns); sd(returns)
# Skewness and kurtosis of random normal returns
nrows/((nrows-1)*(nrows-2))*sum(returns^3)
nrows/(nrows-1)^2*sum(returns^4)

# Calculate cumulative standard normal distribution
c(pnorm(-2), pnorm(2))
# Calculate inverse cumulative standard normal distribution
c(qnorm(0.75), qnorm(0.25))
set.seed(1121)  # Reset random number generator
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
cutoff <- confl/nrows
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

set.seed(1121)  # Reset random number generator
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

x11(width=6, height=5)  # Plot in window
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
colors <- c("red", "blue", "green")
for (indeks in 1:3) {  # Plot three curves
  curve(expr=her_mite(x, indeks),
  xlim=c(-3, 3), ylim=c(-2.5, 2.5),
  xlab="", ylab="", lwd=4, col=colors[indeks],
  add=as.logical(indeks-1))
}  # end for
# Add title and legend
title(main="Hermite Polynomials", line=0.5)
labelv <- paste("Order", 1:3, sep=" = ")
legend("top", inset=0.0, bty="n",
 title=NULL, labelv, cex=0.8, lwd=6, lty=1,
 col=colors)

# Define Hermite functions
hermite_fun <- function(x, n)
  exp(-x^2/4)*her_mite(x, n)/(2*pi)^(0.25)/sqrt(factorial(n))
# Integrate Hermite functions
integrate(function(x, n, m)
  hermite_fun(x, n)*hermite_fun(x, m),
  lower=(-Inf), upper=Inf, n=2, m=3)

x11(width=6, height=5)  # Plot in window
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
colors <- c("red", "blue", "green")
for (indeks in 1:3) {  # Plot three curves
  curve(expr=hermite_fun(x, indeks),
  xlim=c(-6, 6), ylim=c(-0.6, 0.6),
  xlab="", ylab="", lwd=4, col=colors[indeks],
  add=as.logical(indeks-1))
}  # end for
# Add title and legend
title(main="Hermite Functions", line=0.5)
labelv <- paste("Order", 1:3, sep=" = ")
legend("topright", inset=0.0, bty="n",
 title=NULL, labelv, cex=0.8, lwd=6, lty=1,
 col=colors)

# Integrate Hermite functions
integrate(her_mite, lower=(-Inf), upper=Inf, n=2)
integrate(function(x, n, m) her_mite(x, n)*her_mite(x, m),
    lower=(-Inf), upper=Inf, n=2, m=3)
integrate(function(x, n, m) her_mite(x, n)*her_mite(x, m),
    lower=(-Inf), upper=Inf, n=2, m=2)

x11(width=6, height=5)  # Plot in window
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
colors <- c("red", "blue", "green")
for (indeks in 1:3) {  # Plot three curves
  curve(expr=her_mite(x, indeks),
  xlim=c(-4, 4), ylim=c(-0.6, 0.6),
  xlab="", ylab="", lwd=3, col=colors,
  add=as.logical(indeks-1))
}  # end for
# Add title and legend
title(main="Hermite Functions", line=0.5)
labelv <- paste("Order", 1:3, sep=" = ")
legend("topright", inset=0.05, bty="n",
 title=NULL, labelv, cex=0.8, lwd=6, lty=1,
 col=colors)

set.seed(1121)  # Reset random number generator
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
startd <- 2; e_nd <- 4
# Plot right tail using polygon
xvar <- seq(startd, e_nd, length=100)
yvar <- dnorm(xvar, sd=1)
yvar[1] <- (-1)
yvar[NROW(yvar)] <- (-1)
polygon(x=xvar, y=yvar, col="red")
# Plot left tail using polygon
yvar <- dnorm(-xvar, sd=1)
yvar[1] <- (-1)
yvar[NROW(yvar)] <- (-1)
polygon(x=(-xvar), y=yvar, col="red")

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
returns <- na.omit(rutils::etfenv$returns$VTI)
# Reset output digits
ndigits <- options(digits=5)
# Shapiro-Wilk test for normal distribution
shapiro.test(rnorm(nrows))
# Shapiro-Wilk test for VTI returns
shapiro.test(returns)
# Shapiro-Wilk test for uniform distribution
shapiro.test(runif(NROW(returns)))
# Restore output digits
options(digits=ndigits$digits)

library(tseries)  # Load package tseries
# Jarque-Bera test for normal distribution
jarque.bera.test(rnorm(nrows))
# Jarque-Bera test for VTI returns
jarque.bera.test(returns)
# Jarque-Bera test for uniform distribution
jarque.bera.test(runif(NROW(returns)))

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
returns <- as.numeric(na.omit(rutils::etfenv$returns$VTI))
returns <- (returns - mean(returns))/sd(returns)
ks.test(returns, rnorm)
# Fit t-dist into VTI returns
optim_fit <- MASS::fitdistr(returns, densfun="t", df=2)
loc <- optim_fit$estimate[1]
scalev <- optim_fit$estimate[2]
# KS test for VTI returns vs t-distribution
# define non-standard Student’s t-distribution
tdistr <- function(x, dfree, loc, scale) {
  dt((x-loc)/scale, df=dfree)/scale
}  # end likefun
# tdistr(x=returns[1], dfree=2, loc=loc, scale=scalev)
# Plot shows that tdistr() fits returns very well
densityv <- density(returns)
madv <- mad(returns)
plot(densityv, xlim=c(-5*madv, 5*madv),
     xlab="returns", ylab="density",
     col="blue", lwd=2, main="Density of VTI Returns")
curve(expr=tdistr(x, dfree=2, loc=loc, scale=scalev), col="red", lwd=2, add=TRUE)
# Or
tdistr <- function(x, dfree, loc, scale) {
  gamma((dfree+1)/2)/(sqrt(pi*dfree)*gamma(dfree/2)*scale)*
    (1+((x-loc)/scale)^2/dfree)^(-(dfree+1)/2)
}  # end likefun
# tdistr(x=returns[1], dfree=2, loc=loc, scale=scalev)

# Not the same as the below - gives p-value << 0.01 - rejects NULL
ks.test(returns, tdistr, dfree=2, loc=loc, scale=scalev)

# Not the same as the above - gives p-value > 0.02 most of the time - doesn't reject NULL
# KS test for VTI returns vs t-distribution
datav <- loc + scalev*rt(NROW(returns), df=2)
ks.test(returns, datav)

x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
# Degrees of freedom
degf <- c(2, 5, 8, 11)
# Plot four curves in loop
colors <- c("red", "black", "blue", "green")
for (indeks in 1:4) {
  curve(expr=dchisq(x, df=degf[indeks]),
  xlim=c(0, 20), ylim=c(0, 0.3),
  xlab="", ylab="", col=colors[indeks],
  lwd=2, add=as.logical(indeks-1))
}  # end for

# Add title
title(main="Chi-squared Distributions", line=0.5)
# Add legend
labelv <- paste("df", degf, sep="=")
legend("topright", inset=0.05, bty="n",
       title="Degrees of freedom", labelv,
       cex=0.8, lwd=6, lty=1, col=colors)

# Observed frequencies from random normal data
histp <- hist(rnorm(1e3, mean=0), breaks=100, plot=FALSE)
freq_o <- histp$counts
# Theoretical frequencies
freq_t <- rutils::diffit(pnorm(histp$breaks))
# Perform Chi-squared test for normal data
chisq.test(x=freq_o, p=freq_t, rescale.p=TRUE, simulate.p.value=TRUE)
# Return p-value
chisq_test <- chisq.test(x=freq_o, p=freq_t, rescale.p=TRUE, simulate.p.value=TRUE)
chisq_test$p.value
# Observed frequencies from shifted normal data
histp <- hist(rnorm(1e3, mean=2), breaks=100, plot=FALSE)
freq_o <- histp$counts/sum(histp$counts)
# Theoretical frequencies
freq_t <- rutils::diffit(pnorm(histp$breaks))
# Perform Chi-squared test for shifted normal data
chisq.test(x=freq_o, p=freq_t, rescale.p=TRUE, simulate.p.value=TRUE)
# Calculate histogram of VTI returns
histp <- hist(returns, breaks=100, plot=FALSE)
freq_o <- histp$counts
# Calculate cumulative probabilities and then difference them
freq_t <- pt((histp$breaks-loc)/scalev, df=2)
freq_t <- rutils::diffit(freq_t)
# Perform Chi-squared test for VTI returns
chisq.test(x=freq_o, p=freq_t, rescale.p=TRUE, simulate.p.value=TRUE)

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
returns <- as.numeric(na.omit(rutils::etfenv$returns[, "VTI"]))
nrows <- NROW(returns)
returns <- 100*(returns-mean(returns))/sd(returns)
# Simulate normal random data
ndata <- rnorm(nrows, sd=100)
# Bootstrap the mean and median estimators
boot_data <- sapply(1:1e3, function(x) {
  # Simulate data
  ndata <- rnorm(nrows, sd=100)
  returns <- returns[sample.int(nrows, replace=TRUE)]
  c(n_mean=mean(ndata),
    n_median=median(ndata),
    vti_mean=mean(returns),
    vti_median=median(returns))
})  # end sapply
boot_data <- t(boot_data)
# Analyze bootstrapped data
head(boot_data)
sum(is.na(boot_data))
# Means and medians from bootstrap
apply(boot_data, MARGIN=2, function(x)
  c(mean=mean(x), stderror=sd(x)))
# Parallel bootstrap under Windows
library(parallel)  # Load package parallel
ncores <- detectCores() - 1  # Number of cores
cluster <- makeCluster(ncores)  # initialize compute cluster
boot_data <- parLapply(cluster, 1:1e4,
  function(x, nrows, returns) {
    # Simulate data
    ndata <- rnorm(nrows, sd=100)
    returns <- returns[sample.int(nrows, replace=TRUE)]
    c(n_mean=mean(ndata),
n_median=median(ndata),
vti_mean=mean(returns),
vti_median=median(returns))
  }, nrows, returns)  # end parLapply
stopCluster(cluster)  # Stop R processes over cluster
# Parallel bootstrap under Mac-OSX or Linux
boot_data <- mclapply(1:1e4, function(x) {
  # Simulate data
  ndata <- rnorm(nrows)
  t_data <- rt(nrows, df=2)
  c(n_mean=mean(ndata),
    n_median=median(ndata),
    vti_mean=mean(t_data),
    vti_median=median(t_data))
}, mc.cores=ncores)  # end mclapply
# Means and medians from bootstrap
boot_data <- rutils::do_call(rbind, boot_data)
apply(boot_data, MARGIN=2, function(x)
  c(mean=mean(x), stderror=sd(x)))

# Bias and variance from bootstrap
bias_var <- apply(boot_data, MARGIN=2,
  function(x) c(bias=mean(x), variance=var(x)))
# MSE of mean
bias_var[1, 3]^2 + bias_var[2, 3]
# MSE of median
bias_var[1, 4]^2 + bias_var[2, 4]

returns <- as.numeric(na.omit(rutils::etfenv$returns[, "VTI"]))
nrows <- NROW(returns)
returns <- 100*(returns-mean(returns))/sd(returns)
# Simulate normal random data
ndata <- rnorm(nrows, sd=100)

# Hodges-Lehmann estimator


# Bootstrap the mean and median estimators
boot_data <- sapply(1:1e3, function(x) {
  # Simulate data
  ndata <- rnorm(nrows, sd=100)
  returns <- returns[sample.int(nrows, replace=TRUE)]
  c(n_mean=mean(ndata),
    n_median=median(ndata),
    vti_mean=mean(returns),
    vti_median=median(returns))
})  # end sapply
boot_data <- t(boot_data)
# Analyze bootstrapped data
head(boot_data)
sum(is.na(boot_data))
# Means and medians from bootstrap
apply(boot_data, MARGIN=2, function(x)
  c(mean=mean(x), stderror=sd(x)))

nrows <- 1e3
datav <- rnorm(nrows)
sd(datav)
mad(datav)
median(abs(datav - median(datav)))
median(abs(datav - median(datav)))/qnorm(0.75)
# Bootstrap of sd and mad estimators
boot_data <- sapply(1:1e4, function(x) {
  samplev <- datav[sample.int(nrows, replace=TRUE)]
  c(sd=sd(samplev), mad=mad(samplev))
})  # end sapply
boot_data <- t(boot_data)
# Analyze bootstrapped variance
head(boot_data)
sum(is.na(boot_data))
# Means and standard errors from bootstrap
apply(boot_data, MARGIN=2, function(x)
  c(mean=mean(x), stderror=sd(x)))
# Parallel bootstrap under Windows
library(parallel)  # Load package parallel
ncores <- detectCores() - 1  # Number of cores
cluster <- makeCluster(ncores)  # initialize compute cluster
boot_data <- parLapply(cluster, 1:1e4,
  function(x, datav) {
    samplev <- datav[sample.int(nrows, replace=TRUE)]
    c(sd=sd(samplev), mad=mad(samplev))
  }, datav=datav)  # end parLapply
stopCluster(cluster)  # Stop R processes over cluster
# Parallel bootstrap under Mac-OSX or Linux
boot_data <- mclapply(1:1e4, function(x) {
    samplev <- datav[sample.int(nrows, replace=TRUE)]
    c(sd=sd(samplev), mad=mad(samplev))
  }, mc.cores=ncores)  # end mclapply
# Means and standard errors from bootstrap
boot_data <- rutils::do_call(rbind, boot_data)
apply(boot_data, MARGIN=2, function(x)
  c(mean=mean(x), stderror=sd(x)))

set.seed(1121)  # Reset random number generator
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
datav <- sort(rnorm(38))
indeks <- c(1:9, 20:29)

# Or
datav <- sort(rnorm(398))
indeks <- c(1:99, 200:299)
sample1 <- datav[indeks]
sample2 <- datav[-indeks]

# Or
indeks <- sample(1:nrows, size=nrows/2)
sample1 <- returns[indeks]
sample2 <- (-returns[-indeks])

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
datav <- rnorm(nrows, sd=100)
wilcox.test(datav, paired=FALSE)
# Skewed distribution with mean=0
mean(returns); median(returns)
wilcox.test(returns-mean(returns),
      paired=FALSE)
# Skewed distribution with median=0
wilcox.test(returns-median(returns),
      paired=FALSE)
# Skewed distribution with median=0
wilcox.test(returns-median(returns),
      datav, paired=FALSE)


sample1 <- sample(returns, size=nrows)
sample2 <- (-sample1)
sample1 <- (sample1-median(sample1))
sample2 <- (sample2-median(sample2))
# Mann-Whitney-Wilcoxon rank sum test
wilcox.test(sample1, sample2,
      paired=FALSE)$p.value


datav <- (-returns)
datav <- (datav-median(datav))
wilcox.test(returns-median(returns),
      datav, paired=FALSE)
wilcox.test(returns-median(returns),
      datav, paired=TRUE)

datav <- (-returns)
datav <- (datav-mean(datav))
wilcox.test(returns-mean(returns),
      datav, paired=FALSE)
wilcox.test(returns-mean(returns),
      datav, paired=TRUE)

foo <- sapply(1:100, function(x) {
  # Data samples
  sample1 <- sample(returns, size=nrows/2)
  sample2 <- sample(-returns, size=nrows/2)
  sample1 <- (sample1-median(sample1))
  sample2 <- (sample2-median(sample2))
  # Mann-Whitney-Wilcoxon rank sum test
  wilcox.test(sample1, sample2, paired=FALSE)$p.value
})
hist(foo)

# Skewed distribution with mean=0
mean(returns); median(returns)
wilcox.test(returns-mean(returns))
# Same as
wilcox.test(returns-mean(returns),
      rep(0, nrows),
      paired=TRUE)
# Skewed distribution with median=0
wilcox.test(returns-median(returns))


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
iris_data <- data.frame(ranks=rank(iris$Sepal.Length),
                  spe_cies=iris$Species)
kruskal_stat <- (12/nrows/(nrows+1))*sum(
  aggregate(ranks ~ spe_cies, data=iris_data,
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
boot_data <- sapply(1:1e4, function(x) {
  samplev <- datav[sample.int(nrows, replace=TRUE)]
  c(sd=sd(samplev), mad=mad(samplev))
})  # end sapply
boot_data <- t(boot_data)
# Analyze bootstrapped variance
head(boot_data)
sum(is.na(boot_data))
# Means and standard errors from bootstrap
apply(boot_data, MARGIN=2,
function(x) c(mean=mean(x), stderror=sd(x)))
# Parallel bootstrap under Windows
library(parallel)  # Load package parallel
ncores <- detectCores() - 1  # Number of cores
cluster <- makeCluster(ncores)  # initialize compute cluster
boot_data <- parLapply(cluster, 1:1e4,
  function(x, datav) {
    samplev <- datav[sample.int(nrows, replace=TRUE)]
    c(sd=sd(samplev), mad=mad(samplev))
  }, datav=datav)  # end parLapply
stopCluster(cluster)  # Stop R processes over cluster
# Parallel bootstrap under Mac-OSX or Linux
boot_data <- mclapply(1:1e4,
  function(x) {
    samplev <- datav[sample.int(nrows, replace=TRUE)]
    c(sd=sd(samplev), mad=mad(samplev))
  }, mc.cores=ncores)  # end mclapply
# Means and standard errors from bootstrap
boot_data <- rutils::do_call(rbind, boot_data)
apply(boot_data, MARGIN=2, function(x)
  c(mean=mean(x), stderror=sd(x)))

nrows <- 1e3
datav <- rnorm(nrows)
sd(datav)
mad(datav)
median(abs(datav - median(datav)))
median(abs(datav - median(datav)))/qnorm(0.75)
# Bootstrap of sd and mad estimators
boot_data <- sapply(1:1e4, function(x) {
  samplev <- datav[sample.int(nrows, replace=TRUE)]
  c(sd=sd(samplev), mad=mad(samplev))
})  # end sapply
boot_data <- t(boot_data)
# Analyze bootstrapped variance
head(boot_data)
sum(is.na(boot_data))
# Means and standard errors from bootstrap
apply(boot_data, MARGIN=2, function(x)
  c(mean=mean(x), stderror=sd(x)))
# Parallel bootstrap under Windows
library(parallel)  # Load package parallel
ncores <- detectCores() - 1  # Number of cores
cluster <- makeCluster(ncores)  # initialize compute cluster
boot_data <- parLapply(cluster, 1:1e4,
  function(x, datav) {
    samplev <- datav[sample.int(nrows, replace=TRUE)]
    c(sd=sd(samplev), mad=mad(samplev))
  }, datav=datav)  # end parLapply
stopCluster(cluster)  # Stop R processes over cluster
# Parallel bootstrap under Mac-OSX or Linux
boot_data <- mclapply(1:1e4, function(x) {
    samplev <- datav[sample.int(nrows, replace=TRUE)]
    c(sd=sd(samplev), mad=mad(samplev))
  }, mc.cores=ncores)  # end mclapply
# Means and standard errors from bootstrap
boot_data <- rutils::do_call(rbind, boot_data)
apply(boot_data, MARGIN=2, function(x)
  c(mean=mean(x), stderror=sd(x)))

set.seed(1121)  # initialize random number generator
# Define variables and calculate correlation
nrows <- 100
xvar <- runif(nrows); yvar <- runif(nrows)
cor(xvar, yvar)
# Correlate the variables and calculate correlation
rho <- 0.5
yvar <- rho*xvar + (1-rho)*yvar
# Plot in x11 window
x11(width=5, height=4)
# Set plot parameters to reduce whitespace around plot
par(mar=c(4, 4, 2, 1), oma=c(0.5, 0.5, 0, 0))
# Plot scatterplot and exact regression line
plot(xvar, yvar, xlab="xvar", ylab="yvar")
title(main="Correlated Variables", line=0.5)
abline(a=0.25, b=rho, lwd=3, col="blue")
# Calculate regression
summary(lm(yvar ~ xvar))

# Simulation of sample correlation
nrows <- 1e4
rho <- 0.99
rho2 <- sqrt(1-rho^2)
datav <- sapply(1:1000, function(x) {
  xvar <- rnorm(nrows)
  yvar <- (rho*xvar + rho2*rnorm(nrows))
  cor(xvar, yvar)
})  # end sapply
sd(datav)
# Correct formula
(1-rho^2)/sqrt(nrows-2)
# Incorrect formula
sqrt((1-rho^2)/(nrows-2))


# Correlation
co_r <- cor(xvar, yvar)
# Standard error of correlation
stderror <- sqrt((1-co_r^2)/(nrows-2))
# t-value of correlation
co_r/stderror
# 95% confidence intervals
co_r*c(1-qnorm(0.975)*stderror, 1+qnorm(0.975)*stderror)


# Test statistical significance of correlation
cor.test(xvar, yvar)

rho <- 0.9
rho2 <- sqrt(1-rho^2)
set.seed(1121)
# Bootstrap of sample mean and median
boot_data <- sapply(1:1000, function(x) {
  xvar <- rnorm(nrows)
  yvar <- (rho*xvar + rho2*rnorm(nrows))
  c(rho=mean(yvar*xvar), y_sd=sd(yvar), cor=cor(xvar, yvar))
})  # end sapply

# Means and standard errors from bootstrap
foo <- apply(boot_data, MARGIN=1, function(x)
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
  yvar <- (rho*xvar + rho2*rnorm(nrows))
  cor(xvar, yvar)
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
cor(xvar, yvar, method="pearson")
cor(xvar, yvar, method="spearman")
# Test statistical significance of correlations
cor.test(xvar, yvar, method="pearson")
cor.test(xvar, yvar, method="spearman")

# Calculate correlations
cor(xvar, yvar, method="pearson")
cor(xvar, yvar, method="kendall")
# Test statistical significance of correlations
cor.test(xvar, yvar, method="pearson")
cor.test(xvar, yvar, method="kendall")
