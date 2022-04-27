# Calculate random default probabilities
set.seed(1121)
nassets <- 100
defprobs <- runif(nassets, max=0.2)
mean(defprobs)
# Simulate number of defaults
unifun <- runif(nassets)
sum(unifun < defprobs)
# Simulate average number of defaults using for() loop (inefficient way)
nsimu <- 1000
set.seed(1121)
de_faults <- numeric(nsimu)
for (i in 1:nsimu) {  # Perform loop
  unifun <- runif(nassets)
  de_faults[i] <- sum(unifun < defprobs)
}  # end for
# Calculate average number of defaults
mean(de_faults)
# Simulate using vectorized functions  (efficient way)
set.seed(1121)
unifun <- matrix(runif(nsimu*nassets), ncol=nsimu)
de_faults <- colSums(unifun < defprobs)
mean(de_faults)
# Plot the distribution of defaults
x11(width=6, height=5)
plot(density(de_faults), main="Distribution of Defaults",
     xlab="number of defaults", ylab="frequqncy")
abline(v=mean(de_faults), lwd=3, col="red")

# Calculate default thresholds and asset values
defthresh <- qnorm(defprobs)
assets <- qnorm(unifun)
# Simulate defaults
de_faults <- colSums(assets < defthresh)
mean(de_faults)

# Plot Standard Normal distribution
x11(width=6, height=5)
xlim <- 4; defthresh <- qnorm(0.025)
curve(expr=dnorm(x), type="l", xlim=c(-xlim, xlim),
xlab="asset value", ylab="", lwd=3,
col="blue", main="Distribution of Asset Values")
abline(v=defthresh, col="red", lwd=3)
text(x=defthresh-0.1, y=0.15, labels="default threshold",
 lwd=2, srt=90, pos=3)
# Plot polygon area
xvar <- seq(-xlim, xlim, length=100)
yvar <- dnorm(xvar)
are_a <- ((xvar >= (-xlim)) & (xvar <= defthresh))
polygon(c(xlim, xvar[are_a], defthresh),
  c(-1, yvar[are_a], -1), col="red")

# Define correlation parameters
rho <- 0.2
rho_sqrt <- sqrt(rho) ; rho_sqrtm <- sqrt(1-rho)
nassets <- 5 ; nsimu <- 10000
# Calculate vector of systematic and idiosyncratic factors
sysv <- rnorm(nsimu)
idiosyncvratic <- rnorm(nsimu*nassets)
# Simulate asset values using vectorized functions (efficient way)
assets <- rho_sqrt*sysv + rho_sqrtm*idiosyncvratic
dim(assets) <- c(nsimu, nassets)
# Asset values are standard normally distributed
apply(assets, MARGIN=2, function(x) c(mean=mean(x), sd=sd(x)))
# Calculate correlations between asset values
cor(assets)
# Simulate asset values using for() loop (inefficient way)
# Allocate matrix of assets
assets <- matrix(nr=nsimu, nc=nassets)
# Simulate asset values using for() loop
for (i in 1:nsimu) {  # Perform loop
  assets[i, ] <- rho_sqrt*sysv[i] + rho_sqrtm*rnorm(nassets)
}  # end for
cor(assets)
# benchmark the speed of the two methods
library(microbenchmark)
summary(microbenchmark(
  forloop={for (i in 1:nsimu) {
    rho_sqrt*sysv[i] + rho_sqrtm*rnorm(nassets)}},
  vector_ized={rho_sqrt*sysv + rho_sqrtm*rnorm(nsimu*nassets)},
  times=10))[, c(1, 4, 5)]

# Calculate random default probabilities
nassets <- 5
defprobs <- runif(nassets, max=0.2)
mean(defprobs)
# Calculate default thresholds
defthresh <- qnorm(defprobs)
# Calculate number of defaults using vectorized functions (efficient way)
# Calculate vector of number of defaults
rowMeans(t(assets) < defthresh)
defprobs
# Calculate number of defaults using for() loop (inefficient way)
# allocate matrix of de_faults
de_faults <- matrix(nr=nsimu, nc=nassets)
# Simulate asset values using for() loop
for (i in 1:nsimu) {  # Perform loop
  de_faults[i, ] <- (assets[i, ] < defthresh)
}  # end for
colSums(de_faults) / nsimu
defprobs
# Calculate correlations between defaults
cor(de_faults)

# Define default probabilities
nassets <- 2
defprob <- 0.2
defthresh <- qnorm(defprob)
# Define correlation parameters
rho <- 0.2
rho_sqrt <- sqrt(rho) ; rho_sqrtm <- sqrt(1-rho)
# Calculate vector of systematic factors
nsimu <- 1000
sysv <- rnorm(nsimu)
# Simulate asset values using vectorized functions
assets <- rho_sqrt*sysv + rho_sqrtm*rnorm(nsimu*nassets)
dim(assets) <- c(nsimu, nassets)
# Calculate number of defaults using vectorized functions
de_faults <- t(t(assets) < defthresh)
# Calculate correlations between defaults
cor(de_faults)
# Calculate average number of defaults and compare to defprob
colSums(de_faults) / nsimu
defprob

# Define cumulative default probability function
def_cumdistr <- function(x, defthresh=(-2), rho=0.2)
  pnorm((sqrt(1-rho)*qnorm(x) - defthresh)/sqrt(rho))
def_cumdistr(x=0.2, defthresh=qnorm(defprob), rho=rho)
# Plot cumulative default probability function
defprob <- 0.4; defthresh <- qnorm(defprob)
curve(expr=def_cumdistr(x, defthresh=defthresh, rho=0.05),
xlim=c(0, 0.999), lwd=3,
xlab="percent default", ylab="probability",
col="green", main="Cumulative Default Probabilities")

# Plot default distribution with higher correlation
curve(expr=def_cumdistr(x, defthresh=defthresh, rho=0.2),
xlim=c(0, 0.999), add=TRUE, lwd=3,
col="blue", main="")
# Add legend
legend(x="topleft",
 legend=c("high correlation", "low correlation"),
 title=NULL, inset=0.05, cex=0.8, bg="white",
 bty="n", lwd=6, lty=1, col=c("blue", "green"))
# Add unconditional default probability
abline(v=defprob, col="red", lwd=3)
text(x=defprob, y=0.0,
 labels="default probability",
 lwd=2, srt=90, pos=4)

# Define default probability density function
defdistr <- function(x, defthresh=(-2), rho=0.2)
  sqrt((1-rho)/rho)*exp(-(sqrt(1-rho)*qnorm(x) -
  defthresh)^2/(2*rho) + qnorm(x)^2/2)
# Define parameters
rho <- 0.2 ; rho_sqrt <- sqrt(rho) ; rho_sqrtm <- sqrt(1-rho)
defprob <- 0.3; defthresh <- qnorm(defprob)
defdistr(0.03, defthresh=defthresh, rho=rho)
# Plot probability distribution of defaults
curve(expr=defdistr(x, defthresh=defthresh, rho=0.1),
xlim=c(0, 1.0), lwd=3,
xlab="percentage of defaults", ylab="density",
col="green", main="Distribution of Defaults")

# Plot default distribution with higher correlation
curve(expr=defdistr(x, defthresh=defthresh, rho=0.3),
xlab="default percentage", ylab="",
add=TRUE, lwd=3, col="blue", main="")
# Add legend
legend(x="topright",
 legend=c("high correlation", "low correlation"),
 title=NULL, inset=0.05, cex=0.8, bg="white",
 bty="n", lwd=6, lty=1, col=c("blue", "green"))
# Add unconditional default probability
abline(v=defprob, col="red", lwd=3)
text(x=defprob, y=2,
 labels="default probability",
 lwd=2, srt=90, pos=2)

# Plot default distribution with low correlation
curve(expr=defdistr(x, defthresh=defthresh, rho=0.01),
xlab="default percentage", ylab="", lwd=2,
col="green", main="Distribution of Defaults")
# Plot default distribution with high correlation
curve(expr=defdistr(x, defthresh=defthresh, rho=0.99),
xlab="percentage of defaults", ylab="density",
add=TRUE, lwd=2, n=10001, col="blue", main="")

# Add legend
legend(x="top",
 legend=c("high correlation", "low correlation"),
 title=NULL, inset=0.1, cex=0.8, bg="white",
 bty="n", lwd=6, lty=1, col=c("blue", "green"))
# Add unconditional default probability
abline(v=0.1, col="red", lwd=2)
text(x=0.1, y=10, lwd=2, pos=4,
 labels="default probability")

# Get help for integrate()
?integrate
# Calculate slowly converging integral
func <- function(x) {1/((x+1)*sqrt(x))}
integrate(func, lower=0, upper=10)
integrate(func, lower=0, upper=Inf)
# Integrate function with parameter lambda
func <- function(x, lambda=1) {
  exp(-x*lambda)
}  # end func
integrate(func, lower=0, upper=Inf)
integrate(func, lower=0, upper=Inf, lambda=2)
# Cumulative probability over normal distribution
pnorm(-2)
integrate(dnorm, low=2, up=Inf)
str(dnorm)
pnorm(-1)
integrate(dnorm, low=2, up=Inf, mean=1)
# Expected value over normal distribution
integrate(function(x) x*dnorm(x), low=2, up=Inf)

# Vasicek model parameters
rho <- 0.1; lgd <- 0.4
defprob <- 0.05; defthresh <- qnorm(defprob)
# Define Vasicek loss distribution function
lossdistr <- function(x, defthresh=(-2), rho=0.2, lgd=0.4)
  sqrt((1-rho)/rho)*exp(-(sqrt(1-rho)*qnorm(x/lgd) - defthresh)^2/(2*rho) + qnorm(x/lgd)^2/2)/lgd
integrate(lossdistr, low=0, up=lgd, defthresh=(-2), rho=rho, lgd=lgd)

# Plot probability distribution of losses
x11(width=6, height=5)
curve(expr=lossdistr(x, defthresh=defthresh, rho=rho),
type="l", xlim=c(0, 0.06),
xlab="loss percentage", ylab="density", lwd=3,
col="blue", main="Portfolio Loss Density")
# Add line for expected loss
abline(v=lgd*defprob, col="red", lwd=3)
text(x=lgd*defprob-0.001, y=35, labels="expected loss", lwd=3, pos=4)

# Define cumulative default probability function
cum_loss <- function(x, defthresh=(-2), rho=0.2, lgd=0.4)
  pnorm((sqrt(1-rho)*qnorm(x/lgd) - defthresh)/sqrt(rho))
# Define Vasicek loss distribution function
# (vectorized version with error handling for x)
lossdistr <- function(x, defthresh=-2, rho=0.1, lgd=0.4) {
  qnormv <- ifelse(x/lgd < 0.999, qnorm(x/lgd), 3.1)
  sqrt((1-rho)/rho)*exp(-(sqrt(1-rho)*qnormv - defthresh)^2/(2*rho) + qnormv^2/2)/lgd
}  # end lossdistr

defprob <- 0.2; defthresh <- qnorm(defprob)
rho <- 0.1; lgd <- 0.4
attachp <- 0.15; detachp <- 0.2
# Expected tranche loss is sum of two terms
tranchel <-
  # Loss between attachp and detachp
  integrate(function(x, attachp) (x-attachp)*lossdistr(x, defthresh=defthresh, rho=rho, lgd=lgd),
low=attachp, up=detachp, attachp=attachp)$value / (detachp-attachp) +
  # Loss in excess of detachp
  (1-cum_loss(x=detachp, defthresh=defthresh, rho=rho, lgd=lgd))
# Plot probability distribution of losses
curve(expr=lossdistr(x, defthresh=defthresh, rho=rho),
type="l", xlim=c(0, 3*lgd*defprob),
xlab="loss percentage", ylab="density", lwd=3,
col="orange", main="CDO Tranche Losses")
# Add line for expected loss
abline(v=lgd*defprob, col="red", lwd=3)
text(x=lgd*defprob-0.001, y=4, labels="expected loss",
 lwd=2, srt=90, pos=3)
# Add lines for attach and detach
abline(v=attachp, col="blue", lwd=3)
text(x=attachp-0.001, y=4, labels="attach",
 lwd=2, srt=90, pos=3)
abline(v=detachp, col="green", lwd=3)
text(x=detachp-0.001, y=4, labels="detach",
 lwd=2, srt=90, pos=3)
# Add shading for CDO tranche
vars <- seq(attachp, detachp, length=100)
densv <- sapply(vars, lossdistr,
  defthresh=defthresh, rho=rho)
# Draw shaded polygon
polygon(c(attachp, vars, detachp), density=20,
  c(-1, densv, -1), col="red", border=NA)
text(x=0.5*(attachp+detachp), y=0, labels="CDO tranche", cex=0.9, lwd=2, pos=3)

# Add lines for unexpected loss
abline(v=0.04, col="blue", lwd=3)
arrows(x0=0.02, y0=35, x1=0.04, y1=35, code=3, lwd=3, cex=0.5)
text(x=0.03, y=36, labels="unexpected loss", lwd=2, pos=3)
# Add lines for VaR
abline(v=0.055, col="red", lwd=3)
arrows(x0=0.0, y0=25, x1=0.055, y1=25, code=3, lwd=3, cex=0.5)
text(x=0.03, y=26, labels="VaR", lwd=2, pos=3)
text(x=0.055-0.001, y=10, labels="VaR", lwd=2, srt=90, pos=3)

varisk <- 0.04; varm <- 4*lgd*defprob
# Calculate CVaR
cvar <- integrate(function(x, ...) x*lossdistr(x, ...),
  low=varisk, up=lgd, defthresh=defthresh, rho=rho, lgd=lgd)$value
cvar <- cvar/integrate(lossdistr, low=varisk, up=lgd, defthresh=defthresh, rho=rho, lgd=lgd)$value
# Plot probability distribution of losses
curve(expr=lossdistr(x, defthresh=defthresh, rho=rho),
type="l", xlim=c(0, 0.2),
xlab="loss percentage", ylab="density", lwd=3,
col="orange", main="Conditional Value at Risk")
# Add line for expected loss
abline(v=lgd*defprob, col="red", lwd=3)
text(x=lgd*defprob-0.001, y=10, labels="expected loss", lwd=2, srt=90, pos=3)

# Add lines for VaR
abline(v=varisk, col="red", lwd=3)
text(x=varisk-0.001, y=10, labels="VaR",
 lwd=2, srt=90, pos=3)
# Add shading for CVaR
vars <- seq(varisk, varm, length=100)
densv <- sapply(vars, lossdistr,
  defthresh=defthresh, rho=rho)
# Draw shaded polygon
polygon(c(varisk, vars, varm), density=20,
  c(-1, densv, -1), col="red", border=NA)
text(x=varisk+0.005, y=0, labels="CVaR", lwd=2, pos=3)

# VaR (quantile of the loss distribution)
varfun <- function(x, defthresh=qnorm(0.1), rho=0.1, lgd=0.4)
  lgd*pnorm((sqrt(rho)*qnorm(x) + defthresh)/sqrt(1-rho))
varfun(x=0.99, defthresh=defthresh, rho=rho, lgd=lgd)
# Plot VaR
curve(expr=varfun(x, defthresh=defthresh, rho=rho, lgd=lgd),
type="l", xlim=c(0, 0.999), xlab="confidence level", ylab="VaR", lwd=3,
col="orange", main="VaR versus Confidence Level")
# Add line for expected loss
abline(h=lgd*defprob, col="red", lwd=3)
text(x=0.2, y=lgd*defprob, labels="expected loss", lwd=2, pos=3)

# Integrate lossdistr() over full range
integrate(lossdistr, low=0.0, up=lgd,
    defthresh=defthresh, rho=rho, lgd=lgd)
# Calculate expected losses using lossdistr()
integrate(function(x, ...) x*lossdistr(x, ...),
    low=0.0, up=lgd,
    defthresh=defthresh, rho=rho, lgd=lgd)
# Calculate confidence levels corresponding to VaR values
vars <- seq(0.07, 0.12, 0.001)
confls <- sapply(vars, function(varisk, ...) {
  integrate(lossdistr, low=varisk, up=lgd, ...)
}, defthresh=defthresh, rho=rho, lgd=lgd)  # end sapply
confls <- cbind(as.numeric(t(confls)[, 1]), vars)
colnames(confls) <- c("levels", "VaRs")
# Calculate 95% confidence level VaR value
confls[match(TRUE, confls[, "levels"] < 0.05), "VaRs"]
plot(x=1-confls[, "levels"],
     y=confls[, "VaRs"], lwd=2,
     xlab="confidence level", ylab="VaRs",
     t="l", main="VaR Values and Confidence Levels")

# Calculate CVaR values
cvars <- sapply(vars, function(varisk, ...) {
  integrate(function(x, ...) x*lossdistr(x, ...),
      low=varisk, up=lgd, ...)
}, defthresh=defthresh, rho=rho, lgd=lgd)  # end sapply
confls <- cbind(confls, as.numeric(t(cvars)[, 1]))
colnames(confls)[3] <- "CVaRs"
# Divide CVaR by confidence level
confls[, "CVaRs"] <- confls[, "CVaRs"]/confls[, "levels"]
# Calculate 95% confidence level CVaR value
confls[match(TRUE,
  confls[, "levels"] < 0.05), "CVaRs"]
# Plot CVaRs
plot(x=1-confls[, "levels"],
     y=confls[, "CVaRs"],
     t="l", col="red", lwd=2,
     ylim=range(confls[, c("VaRs", "CVaRs")]),
     xlab="confidence level", ylab="CVaRs",
     main="CVaR Values and Confidence Levels")

# Add VaRs
lines(x=1-confls[, "levels"], y=confls[, "VaRs"], lwd=2)
# Add legend
legend(x="topleft", legend=c("CVaRs", "VaRs"),
 title="default probability = 5%
correlation = 10%
loss given default = 40%",
 inset=0.1, cex=0.8, bg="white", bty="n",
 lwd=6, lty=1, col=c("red", "black"))

# Define model parameters
nassets <- 300; nsimu <- 1000; lgd <- 0.4
# Define correlation parameters
rho <- 0.2; rho_sqrt <- sqrt(rho); rho_sqrtm <- sqrt(1-rho)
# Calculate default probabilities and thresholds
set.seed(1121)
defprobs <- runif(nassets, max=0.2)
defthresh <- qnorm(defprobs)
# Simulate losses under Vasicek model
sysv <- rnorm(nsimu)
assets <- matrix(rnorm(nsimu*nassets), ncol=nsimu)
assets <- t(rho_sqrt*sysv + t(rho_sqrtm*assets))
losses <- lgd*colSums(assets < defthresh)/nassets

# Calculate VaR from confidence level
confl <- 0.95
varisk <- quantile(losses, confl)
# Calculate the CVaR as the mean losses in excess of VaR
cvar <- mean(losses[losses > varisk])
# Plot the density of portfolio losses
x11(width=6, height=5)
densv <- density(losses)
plot(densv, xlab="loss percentage", ylab="density",
     lwd=3, col="blue", main="Portfolio Loss Distribution")
# Add vertical line for expected loss
exploss <- lgd*mean(defprobs)
abline(v=exploss, col="red", lwd=3)
xmax <- max(densv$x); ymax <- max(densv$y)
text(x=exploss, y=(6*ymax/7), labels="expected loss",
     lwd=2, pos=4)
# Add vertical line for VaR
abline(v=varisk, col="red", lwd=3)
text(x=varisk, y=4*ymax/5, labels="VaR", lwd=2, pos=4)

# Draw shaded polygon for CVaR
indeks <- (densv$x > varisk)
xvar <- c(min(densv$x[indeks]), densv$x[indeks], max(densv$x))
polygon(xvar, c(-1, densv$y[indeks], -1), col="red", border=NA, density=10)
# Add text for CVaR
text(x=5*varisk/4, y=(ymax/7), labels="CVaR", lwd=2, pos=4)
# Add text with data
text(xmax, ymax, labels=paste0(
 "Expected Loss = ", format(100*exploss, digits=3), "%", "\n",
 "Loss severity = ", format(100*lgd, digits=3), "%", "\n",
 "Correlation = ", format(100*rho, digits=3), "%", "\n",
 "VaR = ", format(100*varisk, digits=3), "%", "\n",
 "CVaR = ", format(100*cvar, digits=3), "%"),
     adj=c(1, 1), cex=0.8, lwd=2)

# Calculate VaRs from confidence levels
confls <- seq(0.93, 0.99, 0.01)
vars <- quantile(losses, probs=confls)
plot(x=confls, y=vars, t="l", lwd=2,
     xlab="confidence level", ylab="VaRs",
     main="Simulated VaR and Confidence Levels")

# Calculate CVaRs
cvars <- sapply(vars, function(varisk) {
  mean(losses[losses >= varisk])
})  # end sapply
cvars <- cbind(cvars, vars)
# Alternative CVaR calculation using frequency table
# first calculate frequency table of losses
# tablev <- table(losses)/nsimu
# Calculate CVaRs from frequency table
# Cvars <- sapply(vars, function(varisk) {
#   tailrisk <- tablev[names(tablev) > varisk]
#   tailrisk %*% as.numeric(names(tailrisk)) / sum(tailrisk)
# })  # end sapply
# Plot CVaRs
plot(x=confls, y=cvars[, "cvars"],
     t="l", col="red", lwd=2,
     ylim=range(cvars),
     xlab="confidence level", ylab="CVaRs",
     main="Simulated CVaR and Confidence Levels")

# Add VaRs
lines(x=confls, y=cvars[, "vars"], lwd=2)
# Add legend
legend(x="topleft", legend=c("CVaRs", "VaRs"), bty="n",
 title=NULL, inset=0.05, cex=0.8, bg="white",
 lwd=6, lty=1, col=c("red", "black"))

calcvar <- function(defthresh, # Default thresholds
               lgd=0.6, # loss given default
               rho_sqrt, rho_sqrtm, # asset correlation
               nsimu=1000, # number of simulations
               conflvs=seq(0.93, 0.99, 0.01) # Confidence levels
               ) {
  # Define model parameters
  nassets <- NROW(defthresh)
  # Simulate losses under Vasicek model
  sysv <- rnorm(nsimu)
  assets <- matrix(rnorm(nsimu*nassets), ncol=nsimu)
  assets <- t(rho_sqrt*sysv + t(rho_sqrtm*assets))
  losses <- lgd*colSums(assets < defthresh)/nassets
  # Calculate VaRs and CVaRs
  vars <- quantile(losses, probs=conflvs)
  cvars <- sapply(vars, function(varisk) {
    mean(losses[losses >= varisk])
  })  # end sapply
  names(vars) <- conflvs
  names(cvars) <- conflvs
  c(vars, cvars)
}  # end calcvar

# Define model parameters
nassets <- 300; nsimu <- 1000; lgd <- 0.4
rho <- 0.2; rho_sqrt <- sqrt(rho); rho_sqrtm <- sqrt(1-rho)
# Calculate default probabilities and thresholds
set.seed(1121)
defprobs <- runif(nassets, max=0.2)
defthresh <- qnorm(defprobs)
# Define number of bootstrap simulations
nboot <- 500
# Perform bootstrap of calcvar
set.seed(1121)
boot_data <- sapply(rep(lgd, nboot), calcvar,
  defthresh=defthresh,
  rho_sqrt=rho_sqrt, rho_sqrtm=rho_sqrtm,
  nsimu=nsimu)  # end sapply
boot_data <- t(boot_data)
# Calculate vectors of standard errors of VaR and CVaR from boot_data data
stderror_var <- apply(boot_data[, 1:7], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
stderror_cvar <- apply(boot_data[, 8:14], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
# Scale the standard errors of VaRs and CVaRs
stderror_var[2, ] <- stderror_var[2, ]/stderror_var[1, ]
stderror_cvar[2, ] <- stderror_cvar[2, ]/stderror_cvar[1, ]

# Plot the standard errors of VaRs and CVaRs
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
plot(x=colnames(stderror_cvar), y=stderror_cvar[2, ],
  t="l", col="red", lwd=2,
  ylim=range(c(stderror_var[2, ], stderror_cvar[2, ])),
  xlab="confidence level", ylab="standard error",
  main="Scaled standard errors of CVaR and VaR")
lines(x=colnames(stderror_var), y=stderror_var[2, ], lwd=2)
legend(x="topleft", legend=c("CVaRs", "VaRs"), bty="n",
 title=NULL, inset=0.05, cex=0.8, bg="white",
 lwd=6, lty=1, col=c("red", "black"))

library(parallel)  # load package parallel
ncores <- detectCores() - 1  # number of cores
cluster <- makeCluster(ncores)  # Initialize compute cluster
# Perform bootstrap of calcvar for Windows
clusterSetRNGStream(cluster, 1121)
boot_data <- parLapply(cluster, rep(lgd, nboot),
  fun=calcvar, defthresh=defthresh,
  rho_sqrt=rho_sqrt, rho_sqrtm=rho_sqrtm,
  nsimu=nsimu, confls=confls)  # end parLapply
# Bootstrap under Mac-OSX or Linux
boot_data <- mclapply(rep(lgd, nboot),
  FUN=calcvar, defthresh=defthresh,
  rho_sqrt=rho_sqrt, rho_sqrtm=rho_sqrtm,
  nsimu=nsimu, confls=confls)  # end mclapply
boot_data <- rutils::do_call(rbind, boot_data)
stopCluster(cluster)  # Stop R processes over cluster
# Calculate vectors of standard errors of VaR and CVaR from boot_data data
stderror_var <- apply(boot_data[, 1:7], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
stderror_cvar <- apply(boot_data[, 8:14], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
# Scale the standard errors of VaRs and CVaRs
stderror_varscaled <- stderror_var[2, ]/stderror_var[1, ]
stderror_cvarscaled <- stderror_cvar[2, ]/stderror_cvar[1, ]

# Plot the standard errors of VaRs and CVaRs
x11(width=6, height=5)
plot(x=colnames(stderror_cvar),
  y=stderror_cvarscaled, t="l", col="red", lwd=2,
  ylim=range(c(stderror_varscaled, stderror_cvarscaled)),
  xlab="confidence level", ylab="standard error",
  main="Scaled standard errors of CVaR and VaR")
lines(x=colnames(stderror_var), y=stderror_varscaled, lwd=2)
legend(x="topleft", legend=c("CVaRs", "VaRs"), bty="n",
 title=NULL, inset=0.05, cex=0.8, bg="white",
 lwd=6, lty=1, col=c("red", "black"))

calcvar <- function(defprobs, # Default probabilities
               lgd=0.6, # loss given default
               rho_sqrt, rho_sqrtm, # asset correlation
               nsimu=1000, # number of simulations
               confls=seq(0.93, 0.99, 0.01) # Confidence levels
               ) {
  # Calculate random default thresholds
  defthresh <- qnorm(runif(1, min=0.5, max=1.5)*defprobs)
  # Simulate losses under Vasicek model
  nassets <- NROW(defprobs)
  sysv <- rnorm(nsimu)
  assets <- matrix(rnorm(nsimu*nassets), ncol=nsimu)
  assets <- t(rho_sqrt*sysv + t(rho_sqrtm*assets))
  losses <- lgd*colSums(assets < defthresh)/nassets
  # Calculate VaRs and CVaRs
  vars <- quantile(losses, probs=confls)
  cvars <- sapply(vars, function(varisk) {
    mean(losses[losses >= varisk])
  })  # end sapply
  names(vars) <- confls
  names(cvars) <- confls
  c(vars, cvars)
}  # end calcvar

library(parallel)  # load package parallel
ncores <- detectCores() - 1  # number of cores
cluster <- makeCluster(ncores)  # Initialize compute cluster
# Perform bootstrap of calcvar for Windows
clusterSetRNGStream(cluster, 1121)
boot_data <- parLapply(cluster, rep(lgd, nboot),
  fun=calcvar, defprobs=defprobs,
  rho_sqrt=rho_sqrt, rho_sqrtm=rho_sqrtm,
  nsimu=nsimu, confls=confls)  # end parLapply
# Bootstrap under Mac-OSX or Linux
boot_data <- mclapply(rep(lgd, nboot),
  FUN=calcvar, defprobs=defprobs,
  rho_sqrt=rho_sqrt, rho_sqrtm=rho_sqrtm,
  nsimu=nsimu, confls=confls)  # end mclapply
boot_data <- rutils::do_call(rbind, boot_data)
stopCluster(cluster)  # Stop R processes over cluster
# Calculate vectors of standard errors of VaR and CVaR from boot_data data
stderror_var_param <- apply(boot_data[, 1:7], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
stderror_cvar_param <- apply(boot_data[, 8:14], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))

# Plot the standard errors of VaRs under uncertain default probabilities
x11(width=6, height=5)
plot(x=colnames(stderror_var),
  y=stderror_var[2, ], t="l", lwd=3,
  ylim=range(c(stderror_var[2, ], stderror_var_param[2, ])),
  xlab="confidence level", ylab="standard error",
  main="Standard Errors of VaR
  with Uncertain Default Probabilities")
lines(x=colnames(stderror_var), y=stderror_var_param[2, ],
col="red", lwd=3)
legend(x=0.95, y=0.02, bty="n",
 legend=c("VaR Fixed Def Probs", "VaR Random Def Probs"),
 title=NULL, inset=0.05, cex=1.0, bg="white",
 lwd=6, lty=1, col=c("black", "red"))

# Scale the standard errors of VaRs and CVaRs
stderror_varscaled <- stderror_var_param[2, ]/
  stderror_var_param[1, ]
stderror_cvarscaled <- stderror_cvar_param[2, ]/
  stderror_cvar_param[1, ]

# Plot the standard errors of VaRs and CVaRs
x11(width=6, height=5)
plot(x=colnames(stderror_cvar_param),
  y=stderror_cvarscaled, t="l", col="red", lwd=3,
  ylim=range(c(stderror_varscaled, stderror_cvarscaled)),
  xlab="confidence level", ylab="standard error",
  main="Relative Standard Errors of VaR and CVaR
  with Uncertain Default Probabilities")
lines(x=names(stderror_varscaled), y=stderror_varscaled, lwd=3)
legend(x="topright", legend=c("CVaR", "VaR"), bty="n",
 title=NULL, inset=0.05, cex=1.0, bg="white",
 lwd=6, lty=1, col=c("red", "black"))

# Create a plotting expression
ex_pr <- quote({
  par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
  degf <- 2:20
  rangev <- (1:NROW(degf))
  indeks <- 4
  # Plot a curve
  curve(expr=dchisq(x, df=degf[indeks]),
xlim=c(0, 30), ylim=c(0, 0.2),
xlab="", ylab="", lwd=3, col="red")
  # Add grey lines to plot
  for (it in rangev[-indeks]) {
    curve(expr=dchisq(x, df=degf[it]),
  xlim=c(0, 30), ylim=c(0, 0.2),
  xlab="", ylab="", lwd=2, col="grey80", add=TRUE)
  }  # end for
  # Add title
  title(main="Chi-squared Distributions", line=-1.5, cex.main=1.5)
  # Add legend
  text(x=20, y=0.15, labels=paste0("Degrees of freedom=",
      degf[indeks]), pos=1, cex=1.3)
})  # end quote

# View the plotting expression
ex_pr
# Create plot by evaluating the plotting expression
x11(width=6, height=4)
eval(ex_pr)

library(animation)
# Create an expression for creating multiple plots
ex_pr <- quote({
  par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
  degf <- 2:20
  rangev <- (1:NROW(degf))
  # Set image refesh interval
  animation::ani.options(interval=0.5)
  # Create multiple plots with curves
  for (indeks in rangev) {
    curve(expr=dchisq(x, df=degf[indeks]),
  xlim=c(0, 30), ylim=c(0, 0.2),
  xlab="", ylab="", lwd=3, col="red")
    # Add grey lines to plot
    for (it in rangev[-indeks]) {
      curve(expr=dchisq(x, df=degf[it]),
    xlim=c(0, 30), ylim=c(0, 0.2),
    xlab="", ylab="", lwd=2, col="grey80", add=TRUE)
    }  # end for
    # Add title
    title(main="Chi-squared Distributions", line=-1.5, cex.main=1.5)
    # Add legend
    text(x=20, y=0.15, labels=paste0("Degrees of freedom=",
      degf[indeks]), pos=1, cex=1.3)
  }  # end for
})  # end quote

# Create plot by evaluating the plotting expression
x11(width=6, height=4)
eval(ex_pr)
# Create gif with animated plot
animation::saveGIF(expr=eval(ex_pr),
  movie.name="chi_squared.gif",
  img.name="chi_squared")
# Create html with animated plot
animation::saveHTML(expr=eval(ex_pr),
  img.name="chi_squared",
  htmlfile="chi_squared.html",
  description="Chi-squared Distributions")  # end saveHTML

NA

App setup code that runs only once at startup.
ndata <- 1e4
stdev <- 1.0

Define the user interface
uiface <- shiny::fluidPage(
  # Create numeric input for the number of data points.
  numericInput("ndata", "Number of data points:", value=ndata),
  # Create slider input for the standard deviation parameter.
  sliderInput("stdev", label="Standard deviation:",
        min=0.1, max=3.0, value=stdev, step=0.1),
  # Render plot in a panel.
  plotOutput("plotobj", height=300, width=500)
)  # end user interface

Define the server function
servfun <- function(input, output) {
  output$plotobj <- shiny::renderPlot({
    # Simulate the data
    datav <- rnorm(input$ndata, sd=input$stdev)
    # Plot the data
    par(mar=c(2, 4, 4, 0), oma=c(0, 0, 0, 0))
    hist(datav, xlim=c(-4, 4), main="Histogram of Random Data")
  })  # end renderPlot
}  # end servfun

# Return a Shiny app object
shiny::shinyApp(ui=uiface, server=servfun)

Create elements of the user interface
uiface <- shiny::fluidPage(
  titlePanel("VWAP Moving Average"),
  # Create single row of widgets with two slider inputs
  fluidRow(
    # Input stock symbol
    column(width=3, selectInput("symbol", label="Symbol",
                          choices=symbolv, selected=symbol)),
    # Input look-back interval
    column(width=3, sliderInput("look_back", label="Lookback interval",
                          min=1, max=150, value=11, step=1))
  ),  # end fluidRow
  # Create output plot panel
  mainPanel(dygraphs::dygraphOutput("dyplot"), width=12)
)  # end fluidPage interface

Define the server function
servfun <- shiny::shinyServer(function(input, output) {
  # Get the close and volume data in a reactive environment
  closep <- shiny::reactive({
    # Get the data
    ohlc <- get(input$symbol, data_env)
    closep <- log(quantmod::Cl(ohlc))
    volumes <- quantmod::Vo(ohlc)
    # Return the data
    cbind(closep, volumes)
  })  # end reactive code

  # Calculate the VWAP indicator in a reactive environment
  vwapv <- shiny::reactive({
    # Get model parameters from input argument
    look_back <- input$look_back
    # Calculate the VWAP indicator
    closep <- closep()[, 1]
    volumes <- closep()[, 2]
    vwapv <- HighFreq::roll_sum(se_ries=closep*volumes, look_back=look_back)
    volume_rolling <- HighFreq::roll_sum(se_ries=volumes, look_back=look_back)
    vwapv <- vwapv/volume_rolling
    vwapv[is.na(vwapv)] <- 0
    # Return the plot data
    datav <- cbind(closep, vwapv)
    colnames(datav) <- c(input$symbol, "VWAP")
    datav
  })  # end reactive code

  # Return the dygraph plot to output argument
  output$dyplot <- dygraphs::renderDygraph({
    colnamev <- colnames(vwapv())
    dygraphs::dygraph(vwapv(), main=paste(colnamev[1], "VWAP")) %>%
dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col="blue") %>%
dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=2, col="red")
  })  # end output plot
})  # end server code

Return a Shiny app object
shiny::shinyApp(ui=uiface, server=servfun)
