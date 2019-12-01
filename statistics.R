library(knitr)
opts_chunk$set(prompt=TRUE, eval=FALSE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(digits=3)
options(width=60, dev='pdf')
thm <- knit_theme$get("acid")
knit_theme$set(thm)

set.seed(1121)  # Reset random number generator
runif(3)  # three numbers from uniform distribution
runif(3)  # Produce another three numbers
set.seed(1121)  # Reset random number generator
runif(3)  # Produce another three numbers

# Produce random number from standard normal distribution
rnorm(1)
# Produce five random numbers from standard normal distribution
rnorm(5)
# Produce five random numbers from the normal distribution
rnorm(n=5, mean=1, sd=2)  # Match arguments by name
# Calculate cumulative standard normal distribution
c(pnorm(-2), pnorm(2))
# Calculate inverse cumulative standard normal distribution
c(qnorm(0.75), qnorm(0.25))

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
# DAX returns
re_turns <- diff(log(EuStockMarkets[, 1]))
# Number of observations
n_rows <- NROW(re_turns)
# Mean of DAX returns
mea_n <- mean(re_turns)
# Standard deviation of DAX returns
s_d <- sd(re_turns)
# Normalize returns
re_turns <- (re_turns - mea_n)/s_d
# Skew of DAX returns
skew(re_turns)
# Or
n_rows/((n_rows-1)*(n_rows-2))*sum(re_turns^3)
# Kurtosis of DAX returns
kurt(re_turns)
# Or
n_rows/(n_rows-1)^2*sum(re_turns^4)
# Random normal returns
re_turns <- rnorm(n_rows, sd=2)
# Mean and standard deviation of random normal returns
mean(re_turns); sd(re_turns)
# Skew and kurtosis of random normal returns
skew(re_turns); kurt(re_turns)

set.seed(1121)  # Reset random number generator
# Sample from Standard Normal Distribution
n_rows <- 1000
da_ta <- rnorm(n_rows)
# Sample mean - MC estimate
mean(da_ta)
# Sample standard deviation - MC estimate
sd(da_ta)
# Monte Carlo estimate of cumulative probability
da_ta <- sort(da_ta)
pnorm(1)
sum(da_ta<1)/n_rows
# Monte Carlo estimate of quantile
conf_level <- 0.99
qnorm(conf_level)
cut_off <- conf_level*n_rows
da_ta[cut_off]
quantile(da_ta, probs=conf_level)
# Analyze the source code of quantile()
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

# KS-test for normal distribution
ks.test(rnorm(100), pnorm)
# KS-test for uniform distribution
ks.test(runif(100), pnorm)
# KS-test for two similar normal distributions
ks.test(rnorm(100), rnorm(100, mean=0.1))
# KS-test for two different normal distributions
ks.test(rnorm(100), rnorm(100, mean=1.0))

# Calculate DAX percentage returns
dax_rets <- diff(log(EuStockMarkets[, 1]))
# Shapiro-Wilk test for normal distribution
shapiro.test(rnorm(NROW(dax_rets)))
# Shapiro-Wilk test for DAX returns
shapiro.test(dax_rets)
# Shapiro-Wilk test for uniform distribution
shapiro.test(runif(NROW(dax_rets)))

library(tseries)  # Load package tseries
# Jarque-Bera test for normal distribution
jarque.bera.test(rnorm(NROW(dax_rets)))
# Jarque-Bera test for DAX returns
jarque.bera.test(dax_rets)
# Jarque-Bera test for uniform distribution
jarque.bera.test(runif(NROW(dax_rets)))

# Wilcoxon test for normal distribution
wilcox.test(rnorm(100))
# Wilcoxon test for two normal distributions
sample1 <- rnorm(100)
sample2 <- rnorm(100, mean=0.1)
wilcox.test(sample1, sample2)$p.value
t.test(sample1, sample2)$p.value
# Wilcoxon test with data outliers
sample2 <- sample1
sample2[1:11] <- sample2[1:11] + 5
wilcox.test(sample1, sample2)$p.value
t.test(sample1, sample2)$p.value
# Wilcoxon test for two normal distributions
wilcox.test(rnorm(100), rnorm(100, mean=1.0))
# Wilcoxon test for a uniform versus normal distribution
wilcox.test(runif(100)-0.5, rnorm(100))
# Wilcoxon test for a uniform versus normal distribution
wilcox.test(runif(100), rnorm(100))

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

n_rows <- 1e3
da_ta <- rnorm(n_rows)
mean(da_ta)
median(da_ta)

# wipp
# Bootstrap of sd and mad estimators
boot_data <- sapply(1:10000, function(x) {
  boot_sample <-
    da_ta[sample.int(n_rows, replace=TRUE)]
  c(sd=sd(boot_sample), mad=mad(boot_sample))
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
boot_data <- parLapply(clus_ter, 1:10000,
  function(x, da_ta) {
    boot_sample <-
da_ta[sample.int(n_rows, replace=TRUE)]
    c(sd=sd(boot_sample), mad=mad(boot_sample))
  }, da_ta=da_ta)  # end parLapply
# Parallel bootstrap under Mac-OSX or Linux
boot_data <- mclapply(1:10000,
  function(x) {
    boot_sample <-
da_ta[sample.int(n_rows, replace=TRUE)]
    c(sd=sd(boot_sample), mad=mad(boot_sample))
  }, mc.cores=n_cores)  # end mclapply
stopCluster(clus_ter)  # Stop R processes over cluster
boot_data <- rutils::do_call(rbind, boot_data)
# Means and standard errors from bootstrap
apply(boot_data, MARGIN=2,
function(x) c(mean=mean(x), std_error=sd(x)))

n_rows <- 1e3
da_ta <- rnorm(n_rows)
sd(da_ta)
mad(da_ta)
median(abs(da_ta - median(da_ta)))
median(abs(da_ta - median(da_ta)))/qnorm(0.75)
# Bootstrap of sd and mad estimators
boot_data <- sapply(1:10000, function(x) {
  boot_sample <-
    da_ta[sample.int(n_rows, replace=TRUE)]
  c(sd=sd(boot_sample), mad=mad(boot_sample))
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
boot_data <- parLapply(clus_ter, 1:10000,
  function(x, da_ta) {
    boot_sample <-
da_ta[sample.int(n_rows, replace=TRUE)]
    c(sd=sd(boot_sample), mad=mad(boot_sample))
  }, da_ta=da_ta)  # end parLapply
# Parallel bootstrap under Mac-OSX or Linux
boot_data <- mclapply(1:10000,
  function(x) {
    boot_sample <-
da_ta[sample.int(n_rows, replace=TRUE)]
    c(sd=sd(boot_sample), mad=mad(boot_sample))
  }, mc.cores=n_cores)  # end mclapply
stopCluster(clus_ter)  # Stop R processes over cluster
boot_data <- rutils::do_call(rbind, boot_data)
# Means and standard errors from bootstrap
apply(boot_data, MARGIN=2,
function(x) c(mean=mean(x), std_error=sd(x)))

bar <- wilcox.test(foo, conf.int=TRUE)
bar$estimate


n_rows <- 1e3
da_ta <- rnorm(n_rows)
sd(da_ta)
mad(da_ta)
median(abs(da_ta - median(da_ta)))
median(abs(da_ta - median(da_ta)))/qnorm(0.75)
# Bootstrap of sd and mad estimators
boot_data <- sapply(1:10000, function(x) {
  boot_sample <-
    da_ta[sample.int(n_rows, replace=TRUE)]
  c(sd=sd(boot_sample), mad=mad(boot_sample))
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
boot_data <- parLapply(clus_ter, 1:10000,
  function(x, da_ta) {
    boot_sample <-
da_ta[sample.int(n_rows, replace=TRUE)]
    c(sd=sd(boot_sample), mad=mad(boot_sample))
  }, da_ta=da_ta)  # end parLapply
# Parallel bootstrap under Mac-OSX or Linux
boot_data <- mclapply(1:10000,
  function(x) {
    boot_sample <-
da_ta[sample.int(n_rows, replace=TRUE)]
    c(sd=sd(boot_sample), mad=mad(boot_sample))
  }, mc.cores=n_cores)  # end mclapply
stopCluster(clus_ter)  # Stop R processes over cluster
boot_data <- rutils::do_call(rbind, boot_data)
# Means and standard errors from bootstrap
apply(boot_data, MARGIN=2,
function(x) c(mean=mean(x), std_error=sd(x)))

set.seed(1121)  # Reset random number generator
# Sample from Standard Normal Distribution
n_rows <- 1e3
da_ta <- rnorm(n_rows)
# Sample mean
mean(da_ta)
# Sample standard deviation
sd(da_ta)

set.seed(1121)  # Reset random number generator
# Sample from Standard Normal Distribution
n_rows <- 1e3
da_ta <- rnorm(n_rows)
# Sample mean
mean(da_ta)
# Sample standard deviation
sd(da_ta)

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
  boot_sample <- sample.int(n_rows, replace=TRUE)
  mod_el <- lm(for_mula,
         data=re_turns[boot_sample, ])
  mod_el$coefficients
})  # end sapply
# Means and standard errors from regression
model_sum$coefficients
# Means and standard errors from bootstrap
dim(boot_data)
t(apply(boot_data, MARGIN=1,
function(x) c(mean=mean(x), std_error=sd(x))))

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
boot_data <- sapply(1:10000, function(x) {
  boot_sample <-
    da_ta[sample.int(n_rows, replace=TRUE)]
  c(sd=sd(boot_sample), mad=mad(boot_sample))
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
boot_data <- parLapply(clus_ter, 1:10000,
  function(x, da_ta) {
    boot_sample <-
da_ta[sample.int(n_rows, replace=TRUE)]
    c(sd=sd(boot_sample), mad=mad(boot_sample))
  }, da_ta=da_ta)  # end parLapply
# Parallel bootstrap under Mac-OSX or Linux
boot_data <- mclapply(1:10000,
  function(x) {
    boot_sample <-
da_ta[sample.int(n_rows, replace=TRUE)]
    c(sd=sd(boot_sample), mad=mad(boot_sample))
  }, mc.cores=n_cores)  # end mclapply
stopCluster(clus_ter)  # Stop R processes over cluster
boot_data <- rutils::do_call(rbind, boot_data)
# Means and standard errors from bootstrap
apply(boot_data, MARGIN=2,
function(x) c(mean=mean(x), std_error=sd(x)))

n_rows <- 1e3
da_ta <- rnorm(n_rows)
sd(da_ta)
mad(da_ta)
median(abs(da_ta - median(da_ta)))
median(abs(da_ta - median(da_ta)))/qnorm(0.75)
# Bootstrap of sd and mad estimators
boot_data <- sapply(1:10000, function(x) {
  boot_sample <-
    da_ta[sample.int(n_rows, replace=TRUE)]
  c(sd=sd(boot_sample), mad=mad(boot_sample))
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
boot_data <- parLapply(clus_ter, 1:10000,
  function(x, da_ta) {
    boot_sample <-
da_ta[sample.int(n_rows, replace=TRUE)]
    c(sd=sd(boot_sample), mad=mad(boot_sample))
  }, da_ta=da_ta)  # end parLapply
# Parallel bootstrap under Mac-OSX or Linux
boot_data <- mclapply(1:10000,
  function(x) {
    boot_sample <-
da_ta[sample.int(n_rows, replace=TRUE)]
    c(sd=sd(boot_sample), mad=mad(boot_sample))
  }, mc.cores=n_cores)  # end mclapply
stopCluster(clus_ter)  # Stop R processes over cluster
boot_data <- rutils::do_call(rbind, boot_data)
# Means and standard errors from bootstrap
apply(boot_data, MARGIN=2,
function(x) c(mean=mean(x), std_error=sd(x)))

set.seed(1121)  # initialize random number generator
# Define variables and calculate correlation
n_rows <- 100
x_var <- runif(n_rows); y_var <- runif(n_rows)
cor(x_var, y_var)
# Test statistical significance of correlation
cor.test(x_var, y_var)
# Correlate the variables and calculate correlation
rh_o <- 0.5
y_var <- rh_o*x_var + (1-rh_o)*y_var
# Test statistical significance of correlation
cor.test(x_var, y_var)
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
