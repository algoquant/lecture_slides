library(knitr)
opts_chunk$set(prompt=TRUE, eval=FALSE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size="tiny", fig.width=4, fig.height=4)
options(width=80, dev="pdf")
options(digits=3)
thm <- knit_theme$get("acid")
knit_theme$set(thm)

library(rutils)  # Load package rutils
# Calculate VTI percentage returns
re_turns <- na.omit(rutils::etf_env$re_turns$VTI)
# Mean and standard deviation of returns
c(mean(re_turns), sd(re_turns))

# Plot histogram
x11(width=6, height=5)
par(mar=c(1, 1, 1, 1), oma=c(2, 2, 2, 0))
histo_gram <- hist(re_turns, breaks=100,
  main="", ylim=c(0, 60), xlim=c(-0.04, 0.04),
  xlab="", ylab="", freq=FALSE)
# Draw kernel density of histogram
lines(density(re_turns), col="red", lwd=2)
# Add density of normal distribution
curve(expr=dnorm(x, mean=mean(re_turns), sd=sd(re_turns)),
add=TRUE, type="l", lwd=2, col="blue")
title(main="VTI Return Distribution", line=0)  # Add title
# Add legend
legend("topright", inset=0.05, cex=0.8, title=NULL,
 leg=c("VTI", "Normal"), bty="n",
 lwd=6, bg="white", col=c("red", "blue"))

# Create normal Q-Q plot
qqnorm(re_turns, ylim=c(-0.1, 0.1), main="VTI Q-Q Plot",
 xlab="Normal Quantiles")
# Fit a line to the normal quantiles
qqline(re_turns, col="red", lwd=2)
# Perform Shapiro-Wilk test
shapiro.test(as.numeric(re_turns))

# Boxplot method for formula
boxplot(formula=mpg ~ cyl, data=mtcars,
  main="Mileage by number of cylinders",
  xlab="Cylinders", ylab="Miles per gallon")
# Boxplot method for data frame of EuStockMarkets percentage returns
boxplot(x=diff(log(EuStockMarkets)))

# VTI percentage returns
re_turns <- na.omit(rutils::etf_env$re_turns$VTI)
# Number of observations
n_rows <- NROW(re_turns)
# Mean of VTI returns
mean_rets <- mean(re_turns)
# Standard deviation of VTI returns
sd_rets <- sd(re_turns)
# Skewness of VTI returns
n_rows/((n_rows-1)*(n_rows-2))*
  sum(((re_turns - mean_rets)/sd_rets)^3)
# Kurtosis of VTI returns
n_rows*(n_rows+1)/((n_rows-1)^3)*
  sum(((re_turns - mean_rets)/sd_rets)^4)
# Random normal returns
re_turns <- rnorm(n_rows, sd=sd_rets)
# Mean and standard deviation of random normal returns
mean_rets <- mean(re_turns)
sd_rets <- sd(re_turns)
# Skewness of random normal returns
n_rows/((n_rows-1)*(n_rows-2))*
  sum(((re_turns - mean_rets)/sd_rets)^3)
# Kurtosis of random normal returns
n_rows*(n_rows+1)/((n_rows-1)^3)*
  sum(((re_turns - mean_rets)/sd_rets)^4)

# calc_skew() calculates skew of returns
calc_skew <- function(re_turns) {
  # Standardize re_turns
  re_turns <- (re_turns - mean(re_turns))/sd(re_turns)
  # Calculate skew
  n_rows <- NROW(re_turns)
  n_rows*sum(re_turns^3)/((n_rows-1)*(n_rows-2))
}  # end calc_skew
# calc_kurt() calculates kurtosis of returns
calc_kurt <- function(re_turns) {
  # Standardize re_turns
  re_turns <- (re_turns - mean(re_turns))/sd(re_turns)
  # Calculate skew
  n_rows <- NROW(re_turns)
  n_rows*(n_rows+1)*sum(re_turns^4)/((n_rows-1)*(n_rows-2)*(n_rows-3))
}  # end calc_kurt
# Calculate skew and kurtosis of VTI returns
calc_skew(re_turns)
calc_kurt(re_turns)

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

x_var <- seq(-5, 7, length=100)
y_var <- dnorm(x_var, mean=1.0, sd=2.0)
plot(x_var, y_var, type="l", lty="solid", xlab="", ylab="")
title(main="Normal Density Function", line=0.5)
star_t <- 3; fin_ish <- 5  # Set lower and upper bounds
# Plot polygon area
are_a <- ((x_var >= star_t) & (x_var <= fin_ish))
polygon(c(star_t, x_var[are_a], fin_ish),
  c(-1, y_var[are_a], -1), col="red")

par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
sig_mas <- c(0.5, 1, 1.5, 2)  # Sigma values
# Create plot colors
col_ors <- c("red", "black", "blue", "green")
# Create legend labels
lab_els <- paste("sigma", sig_mas, sep="=")
for (in_dex in 1:4) {  # Plot four curves
  curve(expr=dnorm(x, sd=sig_mas[in_dex]),
  xlim=c(-4, 4), xlab="", ylab="", lwd=2,
  col=col_ors[in_dex], add=as.logical(in_dex-1))
}  # end for
# Add title
title(main="Normal Distributions", line=0.5)
# Add legend
legend("topright", inset=0.05, title="Sigmas",
 lab_els, cex=0.8, lwd=2, lty=1, bty="n", col=col_ors)

x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
deg_free <- c(3, 6, 9)  # Df values
col_ors <- c("black", "red", "blue", "green")
lab_els <- c("normal", paste("df", deg_free, sep="="))
# Plot a Normal probability distribution
curve(expr=dnorm, xlim=c(-4, 4), xlab="", ylab="", lwd=2)
for (in_dex in 1:3) {  # Plot three t-distributions
  curve(expr=dt(x, df=deg_free[in_dex]), xlab="", ylab="",
lwd=2, col=col_ors[in_dex+1], add=TRUE)
}  # end for

# Add title
title(main="t-distributions", line=0.5)
# Add legend
legend("topright", inset=0.05, bty="n",
       title="Degrees\n of freedom", lab_els,
       cex=0.8, lwd=6, lty=1, col=col_ors)

# Mixture of two normal distributions with sd=1 and sd=2
n_rows <- 1e5
re_turns <- c(rnorm(n_rows/2), 2*rnorm(n_rows/2))
re_turns <- (re_turns-mean(re_turns))/sd(re_turns)
# Kurtosis of normal
calc_kurt(rnorm(n_rows))
# Kurtosis of mixture
calc_kurt(re_turns)
# Or
n_rows*sum(re_turns^4)/(n_rows-1)^2

x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
# Plot the distributions
plot(density(re_turns), xlab="", ylab="",
  main="Mixture of Normal Returns",
  xlim=c(-3, 3), type="l", lwd=3, col="red")
curve(expr=dnorm, lwd=2, col="blue", add=TRUE)
curve(expr=dt(x, df=3), lwd=2, col="green", add=TRUE)
# Add legend
legend("topright", inset=0.05, lty=1, lwd=6, bty="n",
  legend=c("Mixture", "Normal", "t-distribution"),
  col=c("red", "blue", "green"))

# Objective function is log-likelihood
likeli_hood <- function(pa_r, free_dom, da_ta) {
  sum(
    -log(gamma((free_dom+1)/2) /
      (sqrt(pi*free_dom) * gamma(free_dom/2))) +
    log(pa_r[2]) +
    (free_dom+1)/2 * log(1 + ((da_ta - pa_r[1])/
                    pa_r[2])^2/free_dom))
}  # end likeli_hood
# Demonstrate equivalence with log(dt())
likeli_hood(c(1, 0.5), 2, 2:5)
-sum(log(dt(x=(2:5-1)/0.5, df=2)/0.5))
# Simpler objective function
likeli_hood <- function(pa_r, free_dom, da_ta) {
  -sum(log(dt(x=(da_ta-pa_r[1])/pa_r[2],
      df=free_dom)/pa_r[2]))
}  # end likeli_hood

# VTI percentage returns
re_turns <- na.omit(rutils::etf_env$re_turns$VTI)
# Initial parameters
par_init <- c(mean=0, scale=0.01)
# Fit distribution using optim()
optim_fit <- optim(par=par_init,
  fn=likeli_hood, # Log-likelihood function
  da_ta=re_turns,
  free_dom=2, # Degrees of freedom
  method="L-BFGS-B", # quasi-Newton method
  upper=c(1, 0.1), # upper constraint
  lower=c(-1, 1e-7)) # Lower constraint
# optimal parameters
lo_cation <- optim_fit$par["mean"]
scal_e <- optim_fit$par["scale"]
# Fit VTI returns using MASS::fitdistr()
optim_fit <- MASS::fitdistr(re_turns,
  densfun="t", df=2)
optim_fit$estimate
optim_fit$sd
lo_cation <- optim_fit$estimate[1]
scal_e <- optim_fit$estimate[2]
summary(optim_fit)

x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
# Plot histogram of VTI returns
histo_gram <- hist(re_turns, col="lightgrey",
  xlab="returns", breaks=100, xlim=c(-0.05, 0.05),
  ylab="frequency", freq=FALSE, main="VTI Returns Histogram")
lines(density(re_turns, adjust=1.5), lwd=3, col="blue")
# Plot the Normal probability distribution
curve(expr=dnorm(x, mean=mean(re_turns),
  sd=sd(re_turns)), add=TRUE, lwd=3, col="green")
# Plot t-distribution function
curve(expr=dt((x-lo_cation)/scal_e, df=2)/scal_e,
type="l", lwd=3, col="red", add=TRUE)
# Add legend
legend("topright", inset=0.05, bty="n",
  leg=c("density", "t-distr", "normal"),
  lwd=6, lty=1, col=c("blue", "red", "green"))

x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
# Plot histogram of VTI returns
histo_gram <- hist(re_turns, breaks=100, plot=FALSE)
plot(histo_gram, xlab="returns", ylab="frequency",
     col="lightgrey", freq=FALSE, main="VTI Left Tail Returns Histogram",
     xlim=c(min(re_turns), -0.02),
     ylim=c(0.0, histo_gram$density[findInterval(-0.02, histo_gram$breaks)]))
lines(density(re_turns, adjust=1.5), lwd=4, col="blue")
# Plot t-distribution function
curve(expr=dt((x-lo_cation)/scal_e, df=2)/scal_e, type="l", lwd=4, col="red", add=TRUE)
# Plot the Normal probability distribution
curve(expr=dnorm(x, mean=mean(re_turns), sd=sd(re_turns)), add=TRUE, lwd=4, col="green")
# Add legend
legend("topleft", inset=0.05, bty="n",
  leg=c("density", "t-distr", "normal"),
  lwd=6, lty=1, col=c("blue", "red", "green"))

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

# KS test for normal distribution
ks.test(rnorm(100), pnorm)
# KS test for uniform distribution
ks.test(runif(100), pnorm)
# KS test for two similar normal distributions
ks.test(rnorm(100), rnorm(100, mean=0.1))
# KS test for two different normal distributions
ks.test(rnorm(100), rnorm(100, mean=1.0))
# Fit t-dist into VTI returns
re_turns <- na.omit(rutils::etf_env$re_turns$VTI)
optim_fit <- MASS::fitdistr(re_turns, densfun="t", df=2)
lo_cation <- optim_fit$estimate[1]
scal_e <- optim_fit$estimate[2]
# Perform Kolmogorov-Smirnov test on VTI returns
da_ta <- lo_cation + scal_e*rt(NROW(re_turns), df=2)
ks.test(as.numeric(re_turns), da_ta)

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

# Load package PerformanceAnalytics
library(PerformanceAnalytics)
# Get documentation for package PerformanceAnalytics
# Get short description
packageDescription("PerformanceAnalytics")
# Load help page
help(package="PerformanceAnalytics")
# List all objects in PerformanceAnalytics
ls("package:PerformanceAnalytics")
# List all datasets in PerformanceAnalytics
data(package="PerformanceAnalytics")
# Remove PerformanceAnalytics from search path
detach("package:PerformanceAnalytics")

perf_data <- unclass(data(
    package="PerformanceAnalytics"))$results[, -(1:2)]
apply(perf_data, 1, paste, collapse=" - ")
# Load "managers" data set
data(managers)
class(managers)
dim(managers)
head(managers, 3)

# Load package "PerformanceAnalytics"
library(PerformanceAnalytics)
# Calculate ETF returns
re_turns <- rutils::etf_env$re_turns[, c("VTI", "DBC", "IEF")]
re_turns <- na.omit(re_turns)
# Plot cumulative ETF returns
x11(width=6, height=5)
chart.CumReturns(re_turns, lwd=2, ylab="",
  legend.loc="topleft", main="ETF Cumulative Returns")

re_turns <- rutils::etf_env$re_turns$VTI
re_turns <- na.omit(re_turns)
x11(width=6, height=5)
chart.Histogram(re_turns, xlim=c(-0.04, 0.04),
  colorset = c("lightgray", "red", "blue"), lwd=3,
  main=paste("Distribution of", colnames(re_turns), "Returns"),
  methods = c("add.density", "add.normal"))
legend("topright", inset=0.05, bty="n",
 leg=c("VTI Density", "Normal"),
 lwd=6, lty=1, col=c("red", "blue"))

re_turns <- rutils::etf_env$re_turns[,
  c("VTI", "IEF", "IVW", "VYM", "IWB", "DBC", "VXX")]
x11(width=6, height=5)
chart.Boxplot(names=FALSE, re_turns)
par(cex.lab=0.8, cex.axis=0.8)
axis(side=2, at=(1:NCOL(re_turns))/7.5-0.05,labels=colnames(re_turns))

# Simulate normally distributed data
n_rows <- 1000
da_ta <- rnorm(n_rows)
sd(da_ta)
mad(da_ta)
median(abs(da_ta - median(da_ta)))
median(abs(da_ta - median(da_ta)))/qnorm(0.75)
# Bootstrap of sd and mad estimators
boot_data <- sapply(1:10000, function(x) {
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
clus_ter <- makeCluster(n_cores)  # Initialize compute cluster
boot_data <- parLapply(clus_ter, 1:10000,
  function(x, da_ta) {
    sampl_e <- da_ta[sample.int(n_rows, replace=TRUE)]
    c(sd=sd(sampl_e), mad=mad(sampl_e))
  }, da_ta=da_ta)  # end parLapply
# Parallel bootstrap under Mac-OSX or Linux
boot_data <- mclapply(1:10000, function(x) {
    sampl_e <- da_ta[sample.int(n_rows, replace=TRUE)]
    c(sd=sd(sampl_e), mad=mad(sampl_e))
  }, mc.cores=n_cores)  # end mclapply
stopCluster(clus_ter)  # Stop R processes over cluster
boot_data <- rutils::do_call(rbind, boot_data)
# Means and standard errors from bootstrap
apply(boot_data, MARGIN=2, function(x)
  c(mean=mean(x), std_error=sd(x)))

# VTI returns
re_turns <- rutils::etf_env$re_turns$VTI
re_turns <- na.omit(re_turns)
n_rows <- NROW(re_turns)
sd(re_turns)
mad(re_turns)
# Bootstrap of sd and mad estimators
boot_data <- sapply(1:10000, function(x) {
  sampl_e <- re_turns[sample.int(n_rows, replace=TRUE)]
  c(sd=sd(sampl_e), mad=mad(sampl_e))
})  # end sapply
boot_data <- t(boot_data)
# Means and standard errors from bootstrap
100*apply(boot_data, MARGIN=2, function(x)
  c(mean=mean(x), std_error=sd(x)))
# Parallel bootstrap under Windows
library(parallel)  # Load package parallel
n_cores <- detectCores() - 1  # Number of cores
clus_ter <- makeCluster(n_cores)  # Initialize compute cluster
clusterExport(clus_ter, c("n_rows", "re_turns"))
boot_data <- parLapply(clus_ter, 1:10000,
  function(x) {
    sampl_e <- re_turns[sample.int(n_rows, replace=TRUE)]
    c(sd=sd(sampl_e), mad=mad(sampl_e))
  })  # end parLapply
# Parallel bootstrap under Mac-OSX or Linux
boot_data <- mclapply(1:10000, function(x) {
    sampl_e <- re_turns[sample.int(n_rows, replace=TRUE)]
    c(sd=sd(sampl_e), mad=mad(sampl_e))
  }, mc.cores=n_cores)  # end mclapply
stopCluster(clus_ter)  # Stop R processes over cluster
boot_data <- rutils::do_call(rbind, boot_data)
# Means and standard errors from bootstrap
apply(boot_data, MARGIN=2, function(x)
  c(mean=mean(x), std_error=sd(x)))

library(PerformanceAnalytics)
# Define target rate of return of 50 bps
tar_get <- 0.005
# Calculate the full downside returns
returns_sub <- (re_turns - tar_get)
returns_sub <- ifelse(returns_sub < 0, returns_sub, 0)
n_rows <- NROW(returns_sub)
# Calculate the downside deviation
all.equal(sqrt(sum(returns_sub^2)/n_rows),
  drop(DownsideDeviation(re_turns, MAR=tar_get, method="full")))
# Calculate the subset downside returns
returns_sub <- (re_turns - tar_get)
returns_sub <- returns_sub[returns_sub < 0]
n_rows <- NROW(returns_sub)
# Calculate the downside deviation
all.equal(sqrt(sum(returns_sub^2)/n_rows),
  DownsideDeviation(re_turns, MAR=tar_get, method="subset"))

# Calculate time series of VTI drawdowns
price_s <- quantmod::Cl(rutils::etf_env$VTI)
draw_downs <- (price_s - cummax(price_s))
# PerformanceAnalytics plot of VTI drawdowns
re_turns <- rutils::diff_it(log(price_s))
PerformanceAnalytics::chart.Drawdown(re_turns,
  ylab="", main="VTI Drawdowns")
# PerformanceAnalytics table of VTI drawdowns
PerformanceAnalytics::table.Drawdowns(re_turns)
# dygraph plot of VTI drawdowns
da_ta <- cbind(price_s, draw_downs)
col_names <- c("VTI", "Drawdowns")
colnames(da_ta) <- col_names
dygraphs::dygraph(da_ta, main="VTI Drawdowns") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], valueRange=c(min(da_ta[, "Drawdowns"]), 5), independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="blue") %>%
  dySeries(name=col_names[2], axis="y2", col="red")
# Plot VTI drawdowns
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("blue")
x11(width=6, height=5)
quantmod::chart_Series(x=draw_downs, name="VTI Drawdowns", theme=plot_theme)

library(xtable)
library(PerformanceAnalytics)
price_s <- quantmod::Cl(rutils::etf_env$VTI)
re_turns <- rutils::diff_it(log(price_s))
# Calculate table of VTI drawdowns
tabl_e <- PerformanceAnalytics::table.Drawdowns(re_turns)
# Convert dates to strings
tabl_e <- cbind(sapply(tabl_e[, 1:3], as.character), tabl_e[, 4:7])
# Print table of VTI drawdowns
print(xtable(tabl_e), comment=FALSE, size="tiny", include.rownames=FALSE)

# Load "managers" data set
data(managers)
charts.PerformanceSummary(ham_1,
  main="", lwd=2, ylog=TRUE)

x11(width=6, height=4)
par(mar=c(3, 2, 1, 0), oma=c(0, 0, 0, 0))
# VTI percentage returns
re_turns <- na.omit(rutils::etf_env$re_turns$VTI)
conf_level <- 0.1
va_r <- quantile(re_turns, conf_level)
c_var <- mean(re_turns[re_turns < va_r])
# Plot histogram of VTI returns
histo_gram <- hist(re_turns, col="lightgrey",
  xlab="returns", ylab="frequency", breaks=100,
  xlim=c(-0.05, 0.01), freq=FALSE, main="VTI Returns Histogram")
# Calculate density
densi_ty <- density(re_turns, adjust=1.5)

# Plot density
lines(densi_ty, lwd=3, col="blue")
# Plot line for VaR
abline(v=va_r, col="red", lwd=3)
text(x=va_r, y=20, labels="VaR", lwd=2, srt=90, pos=2)
# Plot polygon shading for CVaR
var_max <- -0.06
rang_e <- (densi_ty$x < va_r) &  (densi_ty$x > var_max)
polygon(c(var_max, densi_ty$x[rang_e], va_r),
  c(0, densi_ty$y[rang_e], 0), col=rgb(1, 0, 0,0.5), border=NA)
text(x=va_r, y=3, labels="CVaR", lwd=2, pos=2)

# VTI percentage returns
re_turns <- na.omit(rutils::etf_env$re_turns$VTI)
conf_level <- 0.02
# Calculate VaR as quantile
va_r <- quantile(re_turns, conf_level)
# Or by sorting
sort_ed <- sort(as.numeric(re_turns))
in_dex <- round(conf_level*NROW(re_turns))
va_r <- sort_ed[in_dex]
# PerformanceAnalytics VaR
PerformanceAnalytics::VaR(re_turns,
  p=(1-conf_level), method="historical")
all.equal(unname(va_r),
  as.numeric(PerformanceAnalytics::VaR(re_turns,
  p=(1-conf_level), method="historical")))

x11(width=6, height=4)
par(mar=c(3, 2, 1, 0), oma=c(0, 0, 0, 0))
# Calculate VaR as quantile
va_r <- quantile(re_turns, conf_level)
# Calculate CVaR as expected loss
c_var <- mean(re_turns[re_turns < va_r])
# Or by sorting
sort_ed <- sort(as.numeric(re_turns))
in_dex <- round(conf_level*NROW(re_turns))
va_r <- sort_ed[in_dex]
c_var <- mean(sort_ed[1:in_dex])
# PerformanceAnalytics VaR
PerformanceAnalytics::ETL(re_turns,
  p=(1-conf_level), method="historical")
all.equal(c_var,
  as.numeric(PerformanceAnalytics::ETL(re_turns,
  p=(1-conf_level), method="historical")))

# Calculate the Sharpe ratio
risk_return <- table.Stats(rutils::etf_env$re_turns)
class(risk_return)
all.equal(risk_return, rutils::etf_env$risk_return)
# Transpose the data frame
risk_return <- as.data.frame(t(risk_return))
# Remove VIX etf data
risk_return <- risk_return[-match(c("VXX", "SVXY"), rownames(risk_return)), ]
# Plot scatterplot
plot(Kurtosis ~ Skewness, data=risk_return,
     ylim=c(1, max(risk_return$Kurtosis)),
     main="Kurtosis vs Skewness")
# Add labels
text(x=risk_return$Skewness, y=risk_return$Kurtosis,
    labels=rownames(risk_return), pos=1, cex=0.8)

risk_return <- rutils::etf_env$risk_return
risk_return <- as.data.frame(t(risk_return))
# Add skew_kurt column
risk_return$skew_kurt <-
  risk_return$Skewness/risk_return$Kurtosis
# Sort on skew_kurt
risk_return <- risk_return[order(risk_return$skew_kurt,
  decreasing=TRUE), ]

# Add names column
risk_return$Name <- rutils::etf_env$etf_list[rownames(risk_return), ]$Name
knitr::kable(risk_return[, c("Name", "Skewness", "Kurtosis")])

library(PerformanceAnalytics)
re_turns <- rutils::etf_env$re_turns[, c("VTI", "IEF")]
re_turns <- na.omit(re_turns)
# Calculate the Sharpe ratio
PerformanceAnalytics::SharpeRatio(re_turns)
# Calculate the Sortino ratio
PerformanceAnalytics::SortinoRatio(re_turns)
# Calculate the Calmar ratio
PerformanceAnalytics::CalmarRatio(re_turns)
# Calculate the returns statistics
tail(PerformanceAnalytics::table.Stats(re_turns), 4)

price_s <- na.omit(rutils::etf_env$price_s$VTI)
# Define look-back window and a half window
win_dow <- 11
# Calculate time series of medians
medi_an <- TTR::runMedian(price_s, n=win_dow)
# Calculate time series of z-scores
ma_d <- TTR::runMAD(price_s, n=win_dow)
z_scores <- (price_s - medi_an)/ma_d
z_scores[1:win_dow, ] <- 0
tail(z_scores, win_dow)
range(z_scores)

x11(width=6, height=5)
# Plot prices and medians
dygraphs::dygraph(cbind(price_s, medi_an), main="VTI median") %>%
  dyOptions(colors=c("black", "red"))
# Plot histogram of z-scores
histo_gram <- hist(z_scores, col="lightgrey",
  xlab="z-scores", breaks=50, xlim=c(-4, 4),
  ylab="frequency", freq=FALSE, main="Z-scores histogram")
lines(density(z_scores, adjust=1.5), lwd=3, col="blue")

# Calculate one-sided Hampel z-scores
medi_an <- TTR::runMedian(price_s, n=win_dow)
ma_d <- TTR::runMAD(price_s, n=win_dow)
z_scores <- (price_s - medi_an)/ma_d
z_scores[1:win_dow, ] <- 0
tail(z_scores, win_dow)
range(z_scores)
# Calculate two-sided Hampel z-scores
half_window <- win_dow %/% 2
medi_an <- rutils::lag_it(medi_an, lagg=-half_window)
ma_d <- rutils::lag_it(ma_d, lagg=-half_window)
z_scores <- (price_s - medi_an)/ma_d
z_scores[1:win_dow, ] <- 0
tail(z_scores, win_dow)
range(z_scores)

# Define discrimination threshold value
thresh_old <- 3
# Calculate number of prices classified as bad data
is_bad <- (abs(z_scores) > thresh_old)
sum(is_bad)
# Add 200 random price jumps into price_s
set.seed(1121)
n_bad <- 200
is_jump <- logical(NROW(price_s))
is_jump[sample(x=NROW(is_jump), size=n_bad)] <- TRUE
price_s[is_jump] <- price_s[is_jump]*
  sample(c(0.95, 1.05), size=n_bad, replace=TRUE)
# Plot prices and medians
dygraphs::dygraph(cbind(price_s, medi_an), main="VTI median") %>%
  dyOptions(colors=c("black", "red"))
# Calculate time series of z-scores
medi_an <- TTR::runMedian(price_s, n=win_dow)
ma_d <- TTR::runMAD(price_s, n=win_dow)
z_scores <- (price_s - medi_an)/ma_d
z_scores[1:win_dow, ] <- 0
# Calculate number of prices classified as bad data
is_bad <- (abs(z_scores) > thresh_old)
sum(is_bad)

# Calculate confusion matrix
table(actual=!is_jump, forecast=!is_bad)
sum(is_bad)
# FALSE positive (type I error)
sum(!is_jump & is_bad)
# FALSE negative (type II error)
sum(is_jump & !is_bad)

# Confusion matrix as function of thresh_old
con_fuse <- function(actu_al, z_scores, thresh_old) {
    confu_sion <- table(!actu_al, !(abs(z_scores) > thresh_old))
    confu_sion <- confu_sion / rowSums(confu_sion)
    c(typeI=confu_sion[2, 1], typeII=confu_sion[1, 2])
  }  # end con_fuse
con_fuse(is_jump, z_scores, thresh_old=thresh_old)
# Define vector of thresholds
threshold_s <- seq(from=0.2, to=5.0, by=0.2)
# Calculate error rates
error_rates <- sapply(threshold_s, con_fuse,
  actu_al=is_jump, z_scores=z_scores)  # end sapply
error_rates <- t(error_rates)
rownames(error_rates) <- threshold_s
error_rates <- rbind(c(1, 0), error_rates)
error_rates <- rbind(error_rates, c(0, 1))
# Calculate area under ROC curve (AUC)
true_pos <- (1 - error_rates[, "typeII"])
true_pos <- (true_pos + rutils::lag_it(true_pos))/2
false_pos <- rutils::diff_it(error_rates[, "typeI"])
abs(sum(true_pos*false_pos))

# Plot ROC curve for Hampel classifier
plot(x=error_rates[, "typeI"],
     y=1-error_rates[, "typeII"],
     xlab="FALSE positive rate",
     ylab="TRUE positive rate",
     xlim=c(0, 1), ylim=c(0, 1),
     main="ROC Curve for Hampel Classifier",
     type="l", lwd=3, col="blue")
abline(a=0.0, b=1.0, lwd=3, col="orange")

# Calculate EWMA VTI variance using filter()
look_back <- 51
weight_s <- exp(-0.1*1:look_back)
weight_s <- weight_s/sum(weight_s)
vari_ance <- stats::filter(re_turns^2,
    filter=weight_s, sides=1)
vari_ance[1:(look_back-1)] <- vari_ance[look_back]
class(vari_ance)
vari_ance <- as.numeric(vari_ance)
x_ts <- xts:::xts(sqrt(vari_ance), order.by=index(re_turns))
# Plot EWMA standard deviation
chart_Series(x_ts,
  name="EWMA standard deviation")
dygraphs::dygraph(x_ts, main="EWMA standard deviation")

# Calculate EWMA VTI variance using filter()
look_back <- 51
weight_s <- exp(-0.1*1:look_back)
weight_s <- weight_s/sum(weight_s)
vari_ance <- stats::filter(re_turns^2,
    filter=weight_s, sides=1)
vari_ance[1:(look_back-1)] <- vari_ance[look_back]
class(vari_ance)
vari_ance <- as.numeric(vari_ance)
x_ts <- xts:::xts(sqrt(vari_ance), order.by=index(re_turns))
# Plot EWMA standard deviation
chart_Series(x_ts,
  name="EWMA standard deviation")
dygraphs::dygraph(x_ts, main="EWMA standard deviation")

# VTI percentage returns
re_turns <- na.omit(rutils::etf_env$re_turns$VTI)
# Define end points
end_p <- 0:NROW(re_turns)
n_rows <- NROW(end_p)
look_back <- 51
# start_p are multi-period lag of end_p
start_p <- c(rep_len(0, look_back - 1),
    end_p[1:(n_rows- look_back + 1)])
# Calculate realized VTI variance in sapply() loop
vari_ance <- sapply(2:n_rows, function(in_dex) {
  ret_s <- re_turns[start_p[in_dex]:end_p[in_dex]]
  sum((ret_s - mean(ret_s))^2)
}) / (look_back-1)  # end sapply
tail(vari_ance)
class(vari_ance)
# Coerce vari_ance into xts
vari_ance <- xts(vari_ance, order.by=index(re_turns))
colnames(vari_ance) <- "VTI.variance"
head(vari_ance)

# Calculate rolling VTI variance using package roll
library(roll)  # Load roll
vari_ance <-
  roll::roll_var(re_turns, width=look_back)
colnames(vari_ance) <- "VTI.variance"
head(vari_ance)
sum(is.na(vari_ance))
vari_ance[1:(look_back-1)] <- 0
# Benchmark calculation of rolling variance
library(microbenchmark)
summary(microbenchmark(
  roll_sapply=sapply(2:n_rows, function(in_dex) {
    ret_s <- re_turns[start_p[in_dex]:end_p[in_dex]]
    sum((ret_s - mean(ret_s))^2)
  }),
  ro_ll=roll::roll_var(re_turns, width=look_back),
  times=10))[, c(1, 4, 5)]

# Calculate EWMA VTI variance using filter()
look_back <- 51
weight_s <- exp(-0.1*1:look_back)
weight_s <- weight_s/sum(weight_s)
vari_ance <- stats::filter(re_turns^2,
    filter=weight_s, sides=1)
vari_ance[1:(look_back-1)] <- vari_ance[look_back]
class(vari_ance)
vari_ance <- as.numeric(vari_ance)
x_ts <- xts:::xts(sqrt(vari_ance), order.by=index(re_turns))
# Plot EWMA standard deviation
chart_Series(x_ts,
  name="EWMA standard deviation")
dygraphs::dygraph(x_ts, main="EWMA standard deviation")

# Calculate rolling VTI variance using package roll
library(roll)  # Load roll
vari_ance <- roll::roll_var(re_turns,
  weights=rev(weight_s), width=look_back)
colnames(vari_ance) <- "VTI.variance"
class(vari_ance)
head(vari_ance)
sum(is.na(vari_ance))
vari_ance[1:(look_back-1)] <- 0

x11(width=6, height=4)
par(mar=c(4, 3, 1, 1), oma=c(0, 0, 0, 0))
# VTI percentage returns
re_turns <- na.omit(rutils::etf_env$re_turns$VTI)
# Calculate rolling VTI variance using package roll
look_back <- 22
vari_ance <- roll::roll_var(re_turns, width=look_back)
vari_ance[1:(look_back-1)] <- 0
colnames(vari_ance) <- "VTI.variance"
# Number of look_backs that fit over re_turns
n_rows <- NROW(re_turns)
n_agg <- n_rows %/% look_back
# Define end_p with beginning stub
end_p <- c(0, n_rows-look_back*n_agg + (0:n_agg)*look_back)
n_rows <- NROW(end_p)
# Subset vari_ance to end_p
vari_ance <- vari_ance[end_p]
# Plot autocorrelation function
rutils::plot_acf(vari_ance, lag=10, main="ACF of Variance")
# Plot partial autocorrelation
pacf(vari_ance, lag=10, main="PACF of Variance", ylab=NA)

# Define GARCH parameters
om_ega <- 0.01 ; al_pha <- 0.2
be_ta <- 0.79 ; n_rows <- 1000
re_turns <- numeric(n_rows)
vari_ance <- numeric(n_rows)
vari_ance[1] <- om_ega/(1-al_pha-be_ta)
re_turns[1] <- rnorm(1, sd=sqrt(vari_ance[1]))
# Simulate GARCH model
set.seed(1121)  # Reset random numbers
for (i in 2:n_rows) {
  re_turns[i] <- rnorm(n=1, sd=sqrt(vari_ance[i-1]))
  vari_ance[i] <- om_ega + al_pha*re_turns[i]^2 +
    be_ta*vari_ance[i-1]
}  # end for

x11(width=5, height=3.5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0))
# Plot GARCH cumulative returns
plot(cumsum(re_turns/100), t="l",
  lwd=2, col="blue", xlab="", ylab="",
  main="GARCH cumulative returns")
# Plot dygraphs GARCH standard deviation
date_s <- seq.Date(from=Sys.Date()-n_rows+1,
  to=Sys.Date(), length.out=n_rows)
x_ts <- xts:::xts(cumsum(re_turns/100), order.by=date_s)
dygraphs::dygraph(x_ts, main="GARCH cumulative returns")
# Plot GARCH standard deviation
plot(sqrt(vari_ance), t="l",
  col="blue", xlab="", ylab="",
  main="GARCH standard deviation")
# Plot dygraphsGARCH standard deviation
x_ts <- xts:::xts(sqrt(vari_ance), order.by=date_s)
dygraphs::dygraph(x_ts, main="GARCH standard deviation")

# Define GARCH parameters
om_ega <- 0.0001 ; al_pha <- 0.5
be_ta <- 0.1 ; n_rows <- 10000
re_turns <- numeric(n_rows)
vari_ance <- numeric(n_rows)
vari_ance[1] <- om_ega/(1-al_pha-be_ta)
re_turns[1] <- rnorm(1, sd=sqrt(vari_ance[1]))
# Simulate GARCH model
set.seed(1121)  # Reset random numbers
for (i in 2:n_rows) {
  re_turns[i] <- rnorm(n=1, sd=sqrt(vari_ance[i-1]))
  vari_ance[i] <- om_ega + al_pha*re_turns[i]^2 +
    be_ta*vari_ance[i-1]
}  # end for
# Calculate kurtosis of GARCH returns
moments::moment(re_turns, order=4) /
  moments::moment(re_turns, order=2)^2
# Perform Jarque-Bera test of normality
tseries::jarque.bera.test(re_turns)

# Plot histogram of GARCH returns
histo_gram <- hist(re_turns, col="lightgrey",
  xlab="returns", breaks=200, xlim=c(-0.05, 0.05),
  ylab="frequency", freq=FALSE,
  main="GARCH returns histogram")
lines(density(re_turns, adjust=1.5),
lwd=3, col="blue")
optim_fit <- MASS::fitdistr(re_turns,
  densfun="t", df=2, lower=c(-1, 1e-7))
lo_cation <- optim_fit$estimate[1]
scal_e <- optim_fit$estimate[2]
curve(expr=dt((x-lo_cation)/scal_e, df=2)/scal_e,
  type="l", xlab="", ylab="", lwd=3,
  col="red", add=TRUE)
legend("topright", inset=0.05, bty="n",
 leg=c("density", "t-distr w/ 2 dof"),
 lwd=6, lty=1, col=c("blue", "red"))

# use fixed notation instead of exponential notation
options(scipen=999)
library(fGarch)
# Fit returns into GARCH
garch_fit <- fGarch::garchFit(data=re_turns)
# Fitted GARCH parameters
round(garch_fit@fit$coef, 5)
# Actual GARCH parameters
round(c(mu=mean(re_turns), omega=om_ega,
  alpha=al_pha, beta=be_ta), 5)

# Plot GARCH fitted standard deviation
plot(sqrt(garch_fit@fit$series$h), t="l",
  col="blue", xlab="", ylab="",
  main="GARCH fitted standard deviation")

# Specify GARCH model
garch_spec <- fGarch::garchSpec(
  model=list(omega=om_ega, alpha=al_pha, beta=be_ta))
# Simulate GARCH model
garch_sim <-
  fGarch::garchSim(spec=garch_spec, n=n_rows)
re_turns <- as.numeric(garch_sim)
# Calculate kurtosis of GARCH returns
moments::moment(re_turns, order=4) /
  moments::moment(re_turns, order=2)^2
# Perform Jarque-Bera test of normality
tseries::jarque.bera.test(re_turns)
# Plot histogram of GARCH returns
histo_gram <- hist(re_turns, col="lightgrey",
  xlab="returns", breaks=200, xlim=c(-0.05, 0.05),
  ylab="frequency", freq=FALSE,
  main="GARCH returns histogram")
lines(density(re_turns, adjust=1.5),
lwd=3, col="blue")

# Fit t-distribution into GARCH returns
optim_fit <- MASS::fitdistr(re_turns,
  densfun="t", df=2, lower=c(-1, 1e-7))
lo_cation <- optim_fit$estimate[1]
scal_e <- optim_fit$estimate[2]
curve(expr=dt((x-lo_cation)/scal_e, df=2)/scal_e,
  type="l", xlab="", ylab="", lwd=3,
  col="red", add=TRUE)
legend("topright", inset=0.05, bty="n",
 leg=c("density", "t-distr w/ 2 dof"),
 lwd=6, lty=1, col=c("blue", "red"))

library(HighFreq)  # Load HighFreq
# Calculate variance for each period
vari_ance <- 252*(24*60*60)^2*
  HighFreq::run_variance(oh_lc=rutils::etf_env$VTI)
# Calculate EWMA VTI variance using RcppRoll
library(RcppRoll)  # Load RcppRoll
look_back <- 51
weight_s <- exp(0.1*1:look_back)
var_ewma <- RcppRoll::roll_mean(vari_ance,
    align="right", n=look_back, weights=weight_s)
var_ewma <- xts(var_ewma,
    order.by=index(rutils::etf_env$VTI[-(1:(look_back-1)), ]))
colnames(var_ewma) <- "VTI variance"
# Plot EWMA variance with custom line colors
x11(width=6, height=5)
chart_Series(rutils::etf_env$VTI["2010-01/2010-10"],
   name="VTI EWMA variance with May 6, 2010 Flash Crash")
# Add variance in extra panel
add_TA(var_ewma["2010-01/2010-10"], col="black")

library(HighFreq)  # Load HighFreq
# Minutely SPY returns (unit per minute) single day
re_turns <- rutils::diff_it(log(SPY["2012-02-13", 4]))
# Minutely SPY volatility (unit per minute)
sd(re_turns)
# Divide minutely SPY returns by time intervals (unit per second)
re_turns <- re_turns / rutils::diff_it(.index(SPY["2012-02-13"]))
re_turns[1] <- 0
# Minutely SPY volatility scaled to unit per minute
60*sd(re_turns)
# SPY returns multiple days
re_turns <- rutils::diff_it(log(SPY[, 4]))
# Minutely SPY volatility (includes overnight jumps)
sd(re_turns)
# Table of intervals - 60 second is most frequent
in_dex <- rutils::diff_it(.index(SPY))
table(in_dex)
# hist(in_dex)
# SPY returns with overnight scaling (unit per second)
re_turns <- re_turns / in_dex
re_turns[1] <- 0
# Minutely SPY volatility scaled to unit per minute
60*sd(re_turns)

library(HighFreq)  # Load HighFreq
# Minutely SPY returns (unit per minute) single day
# Minutely SPY volatility (unit per minute)
re_turns <- rutils::diff_it(log(SPY["2012-02-13", 4]))
sd(re_turns)
# SPY returns multiple days (includes overnight jumps)
re_turns <- rutils::diff_it(log(SPY[, 4]))
sd(re_turns)
# Table of time intervals - 60 second is most frequent
in_dex <- rutils::diff_it(.index(SPY))
table(in_dex)
# SPY returns divided by the overnight time intervals (unit per second)
re_turns <- re_turns / in_dex
re_turns[1] <- 0
# Minutely SPY volatility scaled to unit per minute
60*sd(re_turns)

library(HighFreq)  # Load HighFreq
# Daily OHLC SPY prices
SPY_daily <-
  rutils::to_period(oh_lc=HighFreq::SPY, period="days")
# Daily SPY volatility from daily returns
sd(rutils::diff_it(log(SPY_daily[, 4])))
# Minutely SPY returns (unit per minute)
re_turns <- rutils::diff_it(log(SPY[, 4]))
# Minutely SPY volatility scaled to daily interval
sqrt(6.5*60)*sd(re_turns)
# Minutely SPY returns with overnight scaling (unit per second)
re_turns <- rutils::diff_it(log(SPY[, 4]))
in_dex <- rutils::diff_it(.index(SPY))
re_turns <- re_turns / in_dex
re_turns[1] <- 0
# Daily SPY volatility from minutely returns
sqrt(6.5*60)*60*sd(re_turns)
# Daily SPY volatility
# Including extra time over weekends and holidays
24*60*60*sd(rutils::diff_it(log(SPY_daily[, 4]))[-1] /
    rutils::diff_it(.index(SPY_daily))[-1])

# Calculate SPY prices adjusted for overnight jumps
price_s <- log(as.numeric(Cl(HighFreq::SPY[, 4])))
re_turns <- rutils::diff_it(price_s) /
  rutils::diff_it(.index(HighFreq::SPY))
re_turns[1] <- 0
price_s <- cumsum(re_turns)
# Calculate volatilities for vector of aggregation intervals
interval_s <- round(seq.int(from=3, to=30, length.out=9)^2)
vol_s <- sapply(interval_s, function(inter_val) {
  end_p <- rutils::calc_endpoints(price_s,
    inter_val=inter_val)
  sd(rutils::diff_it(price_s[end_p]))
})  # end sapply
vol_log <- log(vol_s)
vol_log <- vol_log - mean(vol_log)
inter_log <- log(interval_s)
inter_log <- inter_log - mean(inter_log)
mod_el <- lm(vol_log ~ inter_log)
hurs_t <- summary(mod_el)$coeff[2, 1]
# Or directly from formula
hurst_form <- sum(vol_log*inter_log)/sum(inter_log^2)
all.equal(hurst_form, hurs_t)

x11(width=6, height=5)
par(mar=c(4, 4, 2, 1), oma=c(1, 1, 1, 1))
plot(vol_log ~ inter_log, lwd=6, col="red",
     xlab="aggregation intervals (log)",
     ylab="volatility (log)",
     main="Hurst Exponent for SPY From Volatilities")
abline(mod_el, lwd=3, col="blue")
text(-2, 0.5, paste0("Hurst = ", round(hurs_t, 4)))

# Calculate the rescaled range
inter_val <- 500
end_p <- rutils::calc_endpoints(price_s,
  inter_val=inter_val)
r_s <- sapply(2:NROW(end_p), function(ep) {
  in_dex <- end_p[ep-1]:end_p[ep]
  diff(range(price_s[in_dex])) /
    sd(re_turns[in_dex])
})  # end sapply
mean(r_s)

# Calculate rescaled range for vector of aggregation intervals
r_s <- sapply(interval_s, function(inter_val) {
  end_p <- rutils::calc_endpoints(price_s,
    inter_val=inter_val)
  r_s <- sapply(2:NROW(end_p), function(ep) {
    in_dex <- end_p[ep-1]:end_p[ep]
    diff(range(price_s[in_dex]))/sd(re_turns[in_dex])
  })  # end sapply
  mean(na.omit(r_s))
})  # end sapply
rs_log <- log(r_s)
rs_log <- rs_log - mean(rs_log)
inter_log <- log(interval_s)
inter_log <- inter_log - mean(inter_log)
mod_el <- lm(rs_log ~ inter_log)
hurs_t <- summary(mod_el)$coeff[2, 1]
# Or directly from formula
hurst_form <- sum(rs_log*inter_log)/sum(inter_log^2)
all.equal(hurst_form, hurs_t)

x11(width=6, height=5)
par(mar=c(4, 4, 2, 1), oma=c(1, 1, 1, 1))
plot(rs_log ~ inter_log, lwd=6, col="red",
     xlab="aggregation intervals (log)",
     ylab="rescaled range (log)",
     main="Rescaled Range Analysis for SPY")
abline(mod_el, lwd=3, col="blue")
text(-2, 0.5, paste0("Hurst = ", round(hurs_t, 4)))

library(HighFreq)  # Load HighFreq
# Daily SPY volatility from minutely prices using package TTR
library(TTR)
sqrt((6.5*60)*mean(na.omit(
  TTR::volatility(HighFreq::SPY, N=1,
          calc="yang.zhang"))^2))
# SPY volatility using package HighFreq
60*sqrt((6.5*60)*agg_regate(oh_lc=HighFreq::SPY,
    weight_ed=FALSE, mo_ment="run_variance",
    calc_method="yang_zhang"))

library(HighFreq)  # Load HighFreq
oh_lc <- log(rutils::etf_env$VTI)
# Calculate variance
var_close <- HighFreq::run_variance(oh_lc=oh_lc,
  calc_method="close")
var_yang_zhang <- HighFreq::run_variance(oh_lc=oh_lc)
std_dev <- 24*60*60*sqrt(252*cbind(var_close, var_yang_zhang))
colnames(std_dev) <- c("close std_dev", "Yang-Zhang")
# Plot the time series of volatility
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("black", "red")
chart_Series(std_dev["2011-07/2011-12"],
  theme=plot_theme, name="Standard Deviations: Close and YZ")
legend("top", legend=colnames(std_dev),
 bg="white", lty=1, lwd=6, inset=0.1, cex=0.8,
 col=plot_theme$col$line.col, bty="n")
# Plot volatility around 2010 flash crash
chart_Series(std_dev["2010-04/2010-06"],
  theme=plot_theme, name="Volatility Around 2010 Flash Crash")
legend("top", legend=colnames(std_dev),
 bg="white", lty=1, lwd=6, inset=0.1, cex=0.8,
 col=plot_theme$col$line.col, bty="n")
# Plot density of volatility distributions
plot(density(std_dev[, 1]), xlab="", ylab="",
  main="Density of Volatility Distributions",
  xlim=c(-0.05, range(std_dev[, 1])[2]/3), type="l", lwd=2, col="blue")
lines(density(std_dev[, 2]), col='red', lwd=2)
legend("top", legend=c("Close-to-Close", "Yang-Zhang"),
 bg="white", lty=1, lwd=6, inset=0.1, cex=0.8,
 col=plot_theme$col$line.col, bty="n")
# ? range volatility estimator has lower standard error ?
c(sd(var_close)/mean(var_close), sd(var_yang_zhang)/mean(var_yang_zhang))
foo <- std_dev[var_close<range(var_close)[2]/3, ]
c(sd(foo[, 1])/mean(foo[, 1]), sd(foo[, 2])/mean(foo[, 2]))
plot(density(foo[, 1]), xlab="", ylab="",
  main="Mixture of Normal Returns",
  xlim=c(-0.05, range(foo[, 1])[2]/2), type="l", lwd=2, col="blue")
lines(density(foo[, 2]), col='red', lwd=2)

oh_lc <- rutils::etf_env$VTI
re_turns <- log((oh_lc[, 2] - oh_lc[, 3]) / (oh_lc[, 2] + oh_lc[, 3]))
foo <- rutils::diff_it(log(oh_lc[, 4]))
plot(as.numeric(foo)^2, as.numeric(re_turns)^2)
bar <- lm(re_turns ~ foo)
summary(bar)


# Perform normality tests
shapiro.test(coredata(re_turns))
tseries::jarque.bera.test(re_turns)
# Fit VTI returns using MASS::fitdistr()
optim_fit <- MASS::fitdistr(re_turns,
            densfun="t", df=2)
optim_fit$estimate; optim_fit$sd
# Calculate moments of standardized returns
sapply(3:4, moments::moment,
  x=(re_turns - mean(re_turns))/sd(re_turns))

# Plot histogram of VTI returns
col_ors <- c("lightgray", "blue", "green", "red")
PerformanceAnalytics::chart.Histogram(re_turns,
  main="", xlim=c(-7, -3), col=col_ors[1:3],
  methods = c("add.density", "add.normal"))
curve(expr=dt((x-optim_fit$estimate[1])/
  optim_fit$estimate[2], df=2)/optim_fit$estimate[2],
type="l", xlab="", ylab="", lwd=2,
col=col_ors[4], add=TRUE)
# Add title and legend
title(main="VTI logarithm of range",
cex.main=1.3, line=-1)
legend("topright", inset=0.05,
  legend=c("density", "normal", "t-distr"),
  lwd=6, lty=1, col=col_ors[2:4], bty="n")

# VTI range variance partial autocorrelations
pacf(re_turns^2, lag=10, xlab=NA, ylab=NA,
     main="PACF of VTI log range")
chart_Series(re_turns^2,
       name="VTI log of range squared")

# Standard errors of variance estimators using bootstrap
boot_data <- sapply(1:1e2, function(x) {
  # Create random OHLC
  oh_lc <- HighFreq::random_ohlc()
  # Calculate variance estimate
  c(var=var(oh_lc[, 4]),
    yang_zhang=HighFreq::calc_variance(
oh_lc, calc_method="yang_zhang", scal_e=FALSE))
})  # end sapply
# Analyze bootstrapped variance
boot_data <- t(boot_data)
head(boot_data)
colMeans(boot_data)
apply(boot_data, MARGIN=2, sd) /
  colMeans(boot_data)

par(oma=c(1, 1, 1, 1), mar=c(2, 2, 1, 1), mgp=c(0, 0.5, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # Set plot panels
# Close variance estimator partial autocorrelations
pacf(var_close, lag=10, xlab=NA, ylab=NA)
title(main="VTI close variance partial autocorrelations")

# Range variance estimator partial autocorrelations
pacf(var_yang_zhang, lag=10, xlab=NA, ylab=NA)
title(main="VTI YZ variance partial autocorrelations")

# Squared range partial autocorrelations
re_turns <- log(rutils::etf_env$VTI[,2] /
            rutils::etf_env$VTI[,3])
pacf(re_turns^2, lag=10, xlab=NA, ylab=NA)
title(main="VTI squared range partial autocorrelations")

oh_lc <- rutils::etf_env$VTI
# Number of data points
n_rows <- NROW(oh_lc["2018-06/"])
# Define end_p at each point in time
end_p <- 0:n_rows
# Number of data points in look_back interval
look_back <- 22
# start_p are end_p lagged by look_back
start_p <- c(rep_len(0, look_back - 1),
    end_p[1:(NROW(end_p)- look_back + 1)])
head(start_p, 33)

# Number of data points
price_s <- quantmod::Cl(oh_lc["2018/"])
n_rows <- NROW(price_s)
# Number of periods between endpoints
n_points <- 22
# Number of n_points that fit over n_rows
n_agg <- n_rows %/% n_points
# If n_rows==n_points*n_agg then whole number
end_p <- (0:n_agg)*n_points
# Stub interval at beginning
end_p <- c(0, n_rows-n_points*n_agg +
            (0:n_agg)*n_points)
# Else stub interval at end
end_p <- c((0:n_agg)*n_points, n_rows)
# Or use xts::endpoints()
end_p <- xts::endpoints(price_s, on="months")

# Plot data and endpoints as vertical lines
plot.xts(price_s, col="blue", lwd=2, xlab="", ylab="",
   main="Prices with Endpoints as Vertical Lines")
addEventLines(xts(rep("endpoint", NROW(end_p)-1), index(price_s)[end_p]),
        col="red", lwd=2, pos=4)
# Or
plot_theme <- chart_theme()
plot_theme$col$line.col <- "blue"
chart_Series(price_s, theme=plot_theme,
  name="prices with endpoints as vertical lines")
abline(v=end_p, col="red", lwd=2)

# Number of data points
n_rows <- NROW(rutils::etf_env$VTI["2019/"])
# Number of n_points that fit over n_rows
n_points <- 22
n_agg <- n_rows %/% n_points
# Stub interval at beginning
end_p <- c(0, n_rows-n_points*n_agg +
            (0:n_agg)*n_points)

# look_back defined as number of data points
look_back <- 252
# start_p are end_p lagged by look_back
start_p <- (end_p - look_back + 1)
start_p <- ifelse(start_p < 0, 0,
                 start_p)
# look_back defined as number of end_p
look_back <- 12
start_p <- c(rep_len(0, look_back-1),
    end_p[1:(NROW(end_p)- look_back + 1)])
# Bind start_p with end_p
cbind(start_p, end_p)

# Number of data points
n_rows <- NROW(rutils::etf_env$VTI["2019/"])
# Number of data points per interval
n_points <- 22
# Number of n_pointss that fit over n_rows
n_agg <- n_rows %/% n_points
# Define end_p with beginning stub
end_p <- c(0, n_rows-n_points*n_agg +
            (0:n_agg)*n_points)
# Define contiguous start_p
start_p <- c(0, end_p[1:(NROW(end_p)-1)])
# Define exclusive start_p
start_p <- c(0, end_p[1:(NROW(end_p)-1)]+1)

price_s <- quantmod::Cl(rutils::etf_env$VTI)
end_p <- 0:NROW(price_s)  # End points at each point
n_rows <- NROW(end_p)
look_back <- 22  # Number of data points per look-back interval
# start_p are multi-period lag of end_p
start_p <- c(rep_len(0, look_back - 1),
    end_p[1:(n_rows - look_back + 1)])
# Define list of look-back intervals for aggregations over past
look_backs <- lapply(2:n_rows, function(in_dex) {
    start_p[in_dex]:end_p[in_dex]
})  # end lapply
# Define aggregation function
agg_regate <- function(x_ts) c(max=max(x_ts), min=min(x_ts))
# Perform aggregations over look_backs list
agg_regations <- sapply(look_backs,
    function(look_back) agg_regate(price_s[look_back])
)  # end sapply
# Coerce agg_regations into matrix and transpose it
if (is.vector(agg_regations))
  agg_regations <- t(agg_regations)
agg_regations <- t(agg_regations)
# Coerce agg_regations into xts series
agg_regations <- xts(agg_regations,
               order.by=index(price_s[end_p]))

library(rutils)  # Load package rutils
# Perform aggregations over look_backs list
agg_regations <- lapply(look_backs,
    function(look_back) agg_regate(price_s[look_back])
)  # end lapply
# rbind list into single xts or matrix
agg_regations <- rutils::do_call(rbind, agg_regations)
# Convert into xts
agg_regations <- xts::xts(agg_regations,
    order.by=index(price_s))
agg_regations <- cbind(agg_regations, price_s)
# Plot aggregations with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("black", "red", "green")
x11(width=6, height=5)
chart_Series(agg_regations, theme=plot_theme,
       name="price aggregations")
legend("top", legend=colnames(agg_regations),
  bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")

# library(rutils)  # Load package rutils
# Define functional for rolling aggregations
roll_agg <- function(x_ts, look_back, FUN, ...) {
# Define end points at every period
  end_p <- 0:NROW(x_ts)
  n_rows <- NROW(end_p)
# Define starting points as lag of end_p
  start_p <- c(rep_len(0, look_back - 1),
    end_p[1:(n_rows- look_back + 1)])
# Perform aggregations over look_backs list
  agg_regations <- lapply(2:n_rows, function(in_dex)
    FUN(x_ts[start_p[in_dex]:end_p[in_dex]], ...)
  )  # end lapply
# rbind list into single xts or matrix
  agg_regations <- rutils::do_call(rbind, agg_regations)
# Coerce agg_regations into xts series
  if (!is.xts(agg_regations))
    agg_regations <- xts(agg_regations, order.by=index(x_ts))
  agg_regations
}  # end roll_agg
# Define aggregation function
agg_regate <- function(x_ts)
  c(max=max(x_ts), min=min(x_ts))
# Perform aggregations over rolling interval
agg_regations <- roll_agg(price_s, look_back=look_back,
              FUN=agg_regate)
class(agg_regations)
dim(agg_regations)

# library(rutils)  # Load package rutils
# Define aggregation function that returns a vector
agg_vector <- function(x_ts)
  c(max=max(x_ts), min=min(x_ts))
# Define aggregation function that returns an xts
agg_xts <- function(x_ts)
  xts(t(c(max=max(x_ts), min=min(x_ts))), order.by=end(x_ts))
# Benchmark the speed of aggregation functions
library(microbenchmark)
summary(microbenchmark(
  agg_vector=roll_agg(price_s, look_back=look_back, FUN=agg_vector),
  agg_xts=roll_agg(price_s, look_back=look_back, FUN=agg_xts),
  times=10))[, c(1, 4, 5)]

# library(rutils)  # Load package rutils
# Define aggregation function that returns a single value
agg_regate <- function(x_ts)  max(x_ts)
# Perform aggregations over a rolling interval
agg_regations <- xts:::rollapply.xts(price_s, width=look_back,
              FUN=agg_regate, align="right")
# Perform aggregations over a rolling interval
library(PerformanceAnalytics)  # Load package PerformanceAnalytics
agg_regations <- apply.rolling(price_s,
              width=look_back, FUN=agg_regate)
# Benchmark the speed of the functionals
library(microbenchmark)
summary(microbenchmark(
  roll_agg=roll_agg(price_s, look_back=look_back, FUN=max),
  roll_xts=xts:::rollapply.xts(price_s, width=look_back, FUN=max, align="right"),
  apply_rolling=apply.rolling(price_s, width=look_back, FUN=max),
  times=10))[, c(1, 4, 5)]

# library(rutils)  # Load package rutils
# Rolling sum using cumsum()
roll_sum <- function(x_ts, look_back) {
  cum_sum <- cumsum(na.omit(x_ts))
  out_put <- cum_sum - lag(x=cum_sum, k=look_back)
  out_put[1:look_back, ] <- cum_sum[1:look_back, ]
  colnames(out_put) <- paste0(colnames(x_ts), "_stdev")
  out_put
}  # end roll_sum
agg_regations <- roll_sum(price_s, look_back=look_back)
# Perform rolling aggregations using lapply loop
agg_regations <- lapply(2:n_rows, function(in_dex)
    sum(price_s[start_p[in_dex]:end_p[in_dex]])
)  # end lapply
# rbind list into single xts or matrix
agg_regations <- rutils::do_call(rbind, agg_regations)
head(agg_regations)
tail(agg_regations)
# Benchmark the speed of both methods
library(microbenchmark)
summary(microbenchmark(
  roll_sum=roll_sum(price_s, look_back=look_back),
  s_apply=sapply(look_backs,
    function(look_back) sum(price_s[look_back])),
  times=10))[, c(1, 4, 5)]

# Extract time series of VTI prices
price_s <- quantmod::Cl(rutils::etf_env$VTI)
# Calculate EWMA prices using filter()
look_back <- 21
weight_s <- exp(-0.1*1:look_back)
weight_s <- weight_s/sum(weight_s)
filter_ed <- stats::filter(price_s, filter=weight_s,
                   method="convolution", sides=1)
# filter() returns time series of class "ts"
class(filter_ed)
# Filter using compiled C++ function directly
getAnywhere(C_cfilter)
str(stats:::C_cfilter)
filter_fast <- .Call(stats:::C_cfilter, price_s, filter=weight_s, sides=1, circular=FALSE)
all.equal(as.numeric(filter_ed), filter_fast, check.attributes=FALSE)
# Calculate EWMA prices using roll::roll_sum()
weights_rev <- rev(weight_s)
roll_ed <- roll::roll_sum(price_s, width=look_back, weights=weights_rev)
all.equal(filter_ed[-(1:look_back)],
    as.numeric(roll_ed)[-(1:look_back)],
    check.attributes=FALSE)
# Benchmark speed of rolling calculations
library(microbenchmark)
summary(microbenchmark(
  filter=filter(price_s, filter=weight_s, method="convolution", sides=1),
  filter_fast=.Call(stats:::C_cfilter, price_s, filter=weight_s, sides=1, circular=FALSE),
  cum_sum=cumsum(price_s),
  roll=roll::roll_sum(price_s, width=look_back, weights=weights_rev)
  ), times=10)[, c(1, 4, 5)]

# Calculate the rolling maximum and minimum over a vector of data
roll_maxminr <- function(vec_tor, look_back) {
  n_rows <- NROW(vec_tor)
  max_min <- matrix(numeric(2*n_rows), nc=2)
  # Loop over periods
  for (it in 1:n_rows) {
    sub_vec <- vec_tor[max(1, it-look_back+1):it]
    max_min[it, 1] <- max(sub_vec)
    max_min[it, 2] <- min(sub_vec)
  }  # end for
  return(max_min)
}  # end roll_maxminr
max_minr <- roll_maxminr(price_s, look_back)
max_minr <- xts::xts(max_minr, index(price_s))
library(TTR)  # Load package TTR
max_min <- cbind(TTR::runMax(x=price_s, n=look_back),
           TTR::runMin(x=price_s, n=look_back))
all.equal(max_min[-(1:look_back), ], max_minr[-(1:look_back), ], check.attributes=FALSE)
# Benchmark the speed of TTR::runMax
library(microbenchmark)
summary(microbenchmark(
  pure_r=roll_maxminr(price_s, look_back),
  ttr=TTR::runMax(price_s, n=look_back),
  times=10))[, c(1, 4, 5)]
# Benchmark the speed of TTR::runSum
summary(microbenchmark(
  vector_r=cumsum(coredata(price_s)),
  rutils=rutils::roll_sum(price_s, look_back=look_back),
  ttr=TTR::runSum(price_s, n=look_back),
  times=10))[, c(1, 4, 5)]

library(rutils)
# Calculate rolling VTI variance using package roll
library(roll)  # Load roll
re_turns <- na.omit(rutils::etf_env$re_turns[, "VTI"])
look_back <- 22
# Calculate rolling sum using RcppRoll
sum_roll <- roll::roll_sum(re_turns, width=look_back)
# Calculate rolling sum using rutils
sum_rutils <- rutils::roll_sum(re_turns, look_back=look_back)
all.equal(sum_roll[-(1:look_back), ], sum_rutils[-(1:look_back), ], check.attributes=FALSE)
# Benchmark speed of rolling calculations
library(microbenchmark)
summary(microbenchmark(
  cum_sum=cumsum(re_turns),
  roll=roll::roll_sum(re_turns, width=look_back),
  RcppRoll=RcppRoll::roll_sum(re_turns, n=look_back),
  rutils=rutils::roll_sum(re_turns, look_back=look_back),
  times=10))[, c(1, 4, 5)]

library(RcppRoll)  # Load package RcppRoll
# Calculate rolling sum using RcppRoll
sum_roll <- RcppRoll::roll_sum(re_turns, align="right", n=look_back)
# Calculate rolling sum using rutils
sum_rutils <- rutils::roll_sum(re_turns, look_back=look_back)
all.equal(sum_roll, coredata(sum_rutils[-(1:(look_back-1))]), check.attributes=FALSE)
# Benchmark speed of rolling calculations
library(microbenchmark)
summary(microbenchmark(
  cum_sum=cumsum(re_turns),
  RcppRoll=RcppRoll::roll_sum(re_turns, n=look_back),
  rutils=rutils::roll_sum(re_turns, look_back=look_back),
  times=10))[, c(1, 4, 5)]
# Calculate EWMA prices using RcppRoll
price_s <- na.omit(rutils::etf_env$VTI[, 4])
weight_s <- exp(0.1*1:look_back)
prices_ewma <- RcppRoll::roll_mean(price_s,
align="right", n=look_back, weights=weight_s)
prices_ewma <- cbind(price_s,
  rbind(coredata(price_s[1:(look_back-1), ]), prices_ewma))
colnames(prices_ewma) <- c("VTI", "VTI EWMA")
# Plot an interactive dygraph plot
dygraphs::dygraph(prices_ewma)
# Or static plot of EWMA prices with custom line colors
x11(width=6, height=5)
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("black", "red")
chart_Series(prices_ewma, theme=plot_theme,
       name="EWMA prices")
legend("top", legend=colnames(prices_ewma),
 bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")

# library(rutils)  # Load package rutils
library(caTools)  # Load package "caTools"
# Get documentation for package "caTools"
packageDescription("caTools")  # Get short description
help(package="caTools")  # Load help page
data(package="caTools")  # List all datasets in "caTools"
ls("package:caTools")  # List all objects in "caTools"
detach("package:caTools")  # Remove caTools from search path
# Median filter
look_back <- 2
price_s <- quantmod::Cl(HighFreq::SPY["2012-02-01/2012-04-01"])
med_ian <- runmed(x=price_s, k=look_back)
# Vector of rolling volatility
sigma_r <- runsd(x=price_s, k=look_back,
          endrule="constant", align="center")
# Vector of rolling quantiles
quan_tiles <- runquantile(x=price_s, k=look_back,
  probs=0.9, endrule="constant", align="center")

# Compile Rcpp functions
Rcpp::sourceCpp(file="C:/Develop/R/Rcpp/roll_maxmin.cpp")
max_minarma <- roll_maxmin(price_s, look_back)
max_minarma <- xts::xts(max_minr, index(price_s))
max_min <- cbind(TTR::runMax(x=price_s, n=look_back),
           TTR::runMin(x=price_s, n=look_back))
all.equal(max_min[-(1:look_back), ], max_minarma[-(1:look_back), ], check.attributes=FALSE)
# Benchmark the speed of TTR::runMax
library(microbenchmark)
summary(microbenchmark(
  arma=roll_maxmin(price_s, look_back),
  ttr=TTR::runMax(price_s, n=look_back),
  times=10))[, c(1, 4, 5)]
# Dygraphs plot with max_min lines
da_ta <- cbind(price_s, max_minarma)
colnames(da_ta)[2:3] <- c("max", "min")
col_ors <- c("blue", "red", "green")
dygraphs::dygraph(da_ta, main=paste(colnames(price_s), "max and min lines")) %>%
  dyOptions(colors=col_ors)
# Standard plot with max_min lines
plot_theme <- chart_theme()
plot_theme$col$line.col <- col_ors
quantmod::chart_Series(da_ta["2008/2009"], theme=plot_theme,
  name=paste(colnames(price_s), "max and min lines"))
legend(x="topright", title=NULL, legend=colnames(da_ta),
 inset=0.1, cex=0.9, bg="white", bty="n",
 lwd=6, lty=1, col=col_ors)

library(rutils)  # Load package rutils
# Indices of last observations in each hour
end_p <- xts::endpoints(price_s, on="hours")
head(end_p)
# extract the last observations in each hour
head(price_s[end_p, ])

price_s <- quantmod::Cl(rutils::etf_env$VTI)
# Number of data points
n_rows <- NROW(price_s)
# Number of data points per interval
look_back <- 22
# Number of look_backs that fit over n_rows
n_agg <- n_rows %/% look_back
# Define end_p with beginning stub
end_p <- c(0, n_rows-look_back*n_agg + (0:n_agg)*look_back)
# Define contiguous start_p
start_p <- c(0, end_p[1:(NROW(end_p)-1)])
# Define list of look-back intervals for aggregations over past
look_backs <- lapply(2:NROW(end_p),
  function(in_dex) {
    start_p[in_dex]:end_p[in_dex]
})  # end lapply
look_backs[[1]]
look_backs[[2]]
# Perform sapply() loop over look_backs list
agg_regations <- sapply(look_backs,
    function(look_back) {
x_ts <- price_s[look_back]
c(max=max(x_ts), min=min(x_ts))
  })  # end sapply
# Coerce agg_regations into matrix and transpose it
if (is.vector(agg_regations))
  agg_regations <- t(agg_regations)
agg_regations <- t(agg_regations)
# Coerce agg_regations into xts series
agg_regations <- xts(agg_regations,
    order.by=index(price_s[end_p]))
head(agg_regations)
# Plot aggregations with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("red", "green")
chart_Series(agg_regations, theme=plot_theme,
       name="price aggregations")
legend("top", legend=colnames(agg_regations),
  bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")

# library(rutils)  # Load package rutils
# Perform lapply() loop over look_backs list
agg_regations <- lapply(look_backs,
    function(look_back) {
x_ts <- price_s[look_back]
c(max=max(x_ts), min=min(x_ts))
    })  # end lapply
# rbind list into single xts or matrix
agg_regations <- rutils::do_call(rbind, agg_regations)
# Coerce agg_regations into xts series
agg_regations <- xts(agg_regations,
    order.by=index(price_s[end_p]))
head(agg_regations)
# Plot aggregations with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("red", "green")
chart_Series(agg_regations, theme=plot_theme,
       name="price aggregations")
legend("top", legend=colnames(agg_regations),
  bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")

# library(rutils)  # Load package rutils
# Define functional for rolling aggregations over end_p
roll_agg <- function(x_ts, end_p, FUN, ...) {
  n_rows <- NROW(end_p)
# start_p are single-period lag of end_p
  start_p <- c(1, end_p[1:(n_rows-1)])
# Perform aggregations over look_backs list
  agg_regations <- lapply(look_backs,
    function(look_back) FUN(x_ts[look_back], ...))  # end lapply
# rbind list into single xts or matrix
  agg_regations <- rutils::do_call(rbind, agg_regations)
  if (!is.xts(agg_regations))
    agg_regations <-  # Coerce agg_regations into xts series
    xts(agg_regations, order.by=index(x_ts[end_p]))
  agg_regations
}  # end roll_agg
# Apply sum() over end_p
agg_regations <-
  roll_agg(price_s, end_p=end_p, FUN=sum)
agg_regations <-
  period.apply(price_s, INDEX=end_p, FUN=sum)
# Benchmark the speed of aggregation functions
summary(microbenchmark(
  roll_agg=roll_agg(price_s, end_p=end_p, FUN=sum),
  period_apply=period.apply(price_s, INDEX=end_p, FUN=sum),
  times=10))[, c(1, 4, 5)]
agg_regations <- period.sum(price_s, INDEX=end_p)
head(agg_regations)

# library(rutils)  # Load package rutils
# Load package HighFreq
library(HighFreq)
# Extract closing minutely prices
price_s <- quantmod::Cl(rutils::etf_env$VTI["2019"])
# Apply "mean" over daily periods
agg_regations <- apply.daily(price_s, FUN=sum)
head(agg_regations)

# Define end_p with beginning stub
n_points <- 5
n_rows <- NROW(price_s)
n_agg <- n_rows %/% n_points
end_p <- c(0, n_rows-n_points*n_agg + (0:n_agg)*n_points)
# Number of data points in look_back interval
look_back <- 22
# start_p are end_p lagged by look_back
start_p <- (end_p - look_back + 1)
start_p <- ifelse(start_p < 0, 0, start_p)
# Perform lapply() loop over look_backs list
agg_regations <- lapply(2:NROW(end_p), function(in_dex) {
x_ts <- price_s[start_p[in_dex]:end_p[in_dex]]
c(max=max(x_ts), min=min(x_ts))
})  # end lapply
# rbind list into single xts or matrix
agg_regations <- rutils::do_call(rbind, agg_regations)
# Coerce agg_regations into xts series
agg_regations <- xts(agg_regations,
    order.by=index(price_s[end_p]))
# Plot aggregations with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("red", "green")
chart_Series(agg_regations, theme=plot_theme,
       name="price aggregations")
legend("top", legend=colnames(agg_regations),
  bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")

agg_regations <- cbind(price_s, agg_regations)
tail(agg_regations)
agg_regations <- na.omit(xts:::na.locf.xts(agg_regations))
# Plot aggregations with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("black", "red", "green")
chart_Series(agg_regations, theme=plot_theme,
       name="price aggregations")
legend("top", legend=colnames(agg_regations),
  bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")

set.seed(1121)  # Reset random number generator
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
library(zoo)  # Load package zoo
# Create zoo time series of random returns
in_dex <- Sys.Date() + 0:365
zoo_series <- zoo(rnorm(NROW(in_dex)), order.by=in_dex)
# Create monthly dates
dates_agg <- as.Date(as.yearmon(index(zoo_series)))
# Perform monthly mean aggregation
zoo_agg <- aggregate(zoo_series, by=dates_agg, FUN=mean)
# Merge with original zoo - union of dates
zoo_agg <- cbind(zoo_series, zoo_agg)
# Replace NA's using locf
zoo_agg <- na.locf(zoo_agg, na.rm=FALSE)
# Extract aggregated zoo
zoo_agg <- zoo_agg[index(zoo_series), 2]

# library(rutils)  # Load package rutils
# Plot original and aggregated cumulative returns
plot(cumsum(zoo_series), xlab="", ylab="")
lines(cumsum(zoo_agg), lwd=2, col="red")
# Add legend
legend("topright", inset=0.05, cex=0.8, bty="n",
 title="Aggregated Prices",
 leg=c("orig prices", "agg prices"),
 lwd=2, bg="white", col=c("black", "red"))

par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# Perform monthly mean aggregation
zoo_agg <- aggregate(zoo_series, by=dates_agg, FUN=mean)
# Merge with original zoo - union of dates
zoo_agg <- cbind(zoo_series, zoo_agg)
# Replace NA's using linear interpolation
zoo_agg <- na.approx(zoo_agg)
# Extract interpolated zoo
zoo_agg <- zoo_agg[index(zoo_series), 2]
# Plot original and interpolated zoo
plot(cumsum(zoo_series), xlab="", ylab="")
lines(cumsum(zoo_agg), lwd=2, col="red")
# Add legend
legend("topright", inset=0.05, cex=0.8, title="Interpolated Prices",
 leg=c("orig prices", "interpol prices"), lwd=2, bg="white",
 col=c("black", "red"), bty="n")

par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# "mean" aggregation over interval with width=11
zoo_mean <- rollapply(zoo_series, width=11,
                FUN=mean, align="right")
# Merge with original zoo - union of dates
zoo_mean <- cbind(zoo_series, zoo_mean)
# Replace NA's using na.locf
zoo_mean <- na.locf(zoo_mean, na.rm=FALSE, fromLast=TRUE)
# Extract mean zoo
zoo_mean <- zoo_mean[index(zoo_series), 2]
# Plot original and interpolated zoo
plot(cumsum(zoo_series), xlab="", ylab="")
lines(cumsum(zoo_mean), lwd=2, col="red")
# Add legend
legend("topright", inset=0.05, cex=0.8, title="Mean Prices",
 leg=c("orig prices", "mean prices"), lwd=2, bg="white",
 col=c("black", "red"), bty="n")
