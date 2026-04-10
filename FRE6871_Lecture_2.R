# Calculate random default probabilities
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
nbonds <- 100
probv <- runif(nbonds, max=0.2)
mean(probv)
# Simulate number of defaults
unifv <- runif(nbonds)
sum(unifv < probv)
# Simulate average number of defaults using for() loop (inefficient way)
nsimu <- 1000
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
defaultv <- numeric(nsimu)
for (i in 1:nsimu) {  # Perform loop
  unifv <- runif(nbonds)
  defaultv[i] <- sum(unifv < probv)
}  # end for
# Calculate average number of defaults
mean(defaultv)
# Simulate using vectorized functions (efficient way)
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
unifm <- matrix(runif(nsimu*nbonds), ncol=nsimu)
defaultv <- colSums(unifm < probv)
mean(defaultv)
# Plot the distribution of defaults
x11(width=6, height=5)
plot(density(defaultv), main="Distribution of Defaults",
     xlab="number of defaults", ylab="frequency")
abline(v=mean(defaultv), lwd=3, col="red")

# Calculate default thresholds and asset values
threshv <- qnorm(probv)
assetm <-qnorm(unifm)
# Simulate defaults
defaultv <- colSums(assetm < threshv)
mean(defaultv)

# Plot Standard Normal distribution
x11(width=6, height=5)
xlim <- 4; threshv <- qnorm(0.025)
curve(expr=dnorm(x), type="l", xlim=c(-xlim, xlim),
xlab="asset value", ylab="", lwd=3,
col="blue", main="Distribution of Asset Values")
abline(v=threshv, col="red", lwd=3)
text(x=threshv-0.1, y=0.15, labels="default threshold",
 lwd=2, srt=90, pos=3)
# Plot polygon area
xvar <- seq(-xlim, xlim, length=100)
yvar <- dnorm(xvar)
intail <- ((xvar >= (-xlim)) & (xvar <= threshv))
polygon(c(xlim, xvar[intail], threshv),
  c(-1, yvar[intail], -1), col="red")

# Define correlation parameters
rho <- 0.2
rhos <- sqrt(rho) ; rhosm <- sqrt(1-rho)
nbonds <- 5 ; nsimu <- 10000
# Calculate vector of systematic and idiosyncratic factors
sysv <- rnorm(nsimu)
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
isync <- rnorm(nsimu*nbonds)
dim(isync) <- c(nbonds, nsimu)
# Simulate asset values using vectorized functions (efficient way)
assetm <- t(rhos*sysv + t(rhosm*isync))
# Asset values are standard normally distributed
apply(assetm, MARGIN=1, function(x) c(mean=mean(x), sd=sd(x)))
# Calculate correlations between asset values
cor(t(assetm))
# Simulate asset values using for() loop (inefficient way)
# Allocate matrix of assets
assetn <- matrix(nrow=nbonds, ncol=nsimu)
# Simulate asset values using for() loop
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
for (i in 1:nsimu) {  # Perform loop
  assetn[, i] <- rhos*sysv[i] + rhosm*rnorm(nbonds)
}  # end for
all.equal(assetn, assetm)
# benchmark the speed of the two methods
library(microbenchmark)
summary(microbenchmark(
  forloop={for (i in 1:nsimu) {
    rhos*sysv[i] + rhosm*rnorm(nbonds)}},
  vectorized={t(rhos*sysv + t(rhosm*isync))},
  times=10))[, c(1, 4, 5)]

# Calculate random default probabilities
nbonds <- 5
probv <- runif(nbonds, max=0.2)
mean(probv)
# Calculate default thresholds
threshv <- qnorm(probv)
# Calculate number of defaults using vectorized functions (efficient way)
# Calculate vector of number of defaults
rowMeans(assetm < threshv)
probv
# Calculate number of defaults using for() loop (inefficient way)
# Allocate matrix of defaultm
defaultm <- matrix(nrow=nbonds, ncol=nsimu)
# Simulate asset values using for() loop
for (i in 1:nsimu) {  # Perform loop
  defaultm[, i] <- (assetm[, i] < threshv)
}  # end for
rowMeans(defaultm)
rowMeans(assetm < threshv)
# Calculate correlations between defaults
cor(t(defaultm))

# Define default probabilities
nbonds <- 2
defprob <- 0.2
threshv <- qnorm(defprob)
# Define correlation parameters
rho <- 0.2
rhos <- sqrt(rho) ; rhosm <- sqrt(1-rho)
# Calculate vector of systematic factors
nsimu <- 1000
sysv <- rnorm(nsimu)
isync <- rnorm(nsimu*nbonds)
dim(isync) <- c(nbonds, nsimu)
# Simulate asset values using vectorized functions
assetm <- t(rhos*sysv + t(rhosm*isync))
# Calculate number of defaults using vectorized functions
defaultm <- (assetm < threshv)
# Calculate average number of defaults and compare to defprob
rowMeans(defaultm)
defprob
# Calculate correlations between assets
cor(t(assetm))
# Calculate correlations between defaults
cor(t(defaultm))

# Define cumulative default distribution function
cumdefdistr <- function(x, threshv=(-2), rho=0.2)
  pnorm((sqrt(1-rho)*qnorm(x) - threshv)/sqrt(rho))
defprob <- 0.4; threshv <- qnorm(defprob)
cumdefdistr(x=0.2, threshv=qnorm(defprob), rho=rho)
# Plot cumulative default distribution function
curve(expr=cumdefdistr(x, threshv=threshv, rho=0.05),
xlim=c(0, 0.999), lwd=3, xlab="percent default", ylab="probability",
col="green", main="Cumulative Default Probabilities")

# Plot default distribution with higher correlation
curve(expr=cumdefdistr(x, threshv=threshv, rho=0.2),
    xlim=c(0, 0.999), add=TRUE, lwd=3, col="blue", main="")
# Add legend
legend(x="topleft",
   legend=c("high correlation", "low correlation"),
   title=NULL, inset=0.05, cex=1.0, bg="white",
   bty="n", lwd=6, lty=1, col=c("blue", "green"))
# Add unconditional default probability
abline(v=defprob, col="red", lwd=3)
text(x=defprob, y=0.0, labels="default probability",
 lwd=2, srt=90, pos=4)

# Define default probability density function
defdistr <- function(x, threshv=(-2), rho=0.2)
  sqrt((1-rho)/rho)*exp(-(sqrt(1-rho)*qnorm(x) -
  threshv)^2/(2*rho) + qnorm(x)^2/2)
# Define parameters
rho <- 0.2 ; rhos <- sqrt(rho) ; rhosm <- sqrt(1-rho)
defprob <- 0.3; threshv <- qnorm(defprob)
defdistr(0.03, threshv=threshv, rho=rho)
# Plot probability distribution of defaults
curve(expr=defdistr(x, threshv=threshv, rho=0.1),
xlim=c(0, 1.0), lwd=3,
xlab="Default percentage", ylab="Density",
col="green", main="Distribution of Defaults")

# Plot default distribution with higher correlation
curve(expr=defdistr(x, threshv=threshv, rho=0.3),
xlab="default percentage", ylab="",
add=TRUE, lwd=3, col="blue", main="")
# Add legend
legend(x="topright",
   legend=c("high correlation", "low correlation"),
   title=NULL, inset=0.05, cex=1.0, bg="white",
   bty="n", lwd=6, lty=1, col=c("blue", "green"))
# Add unconditional default probability
abline(v=defprob, col="red", lwd=3)
text(x=defprob, y=2, labels="default probability",
 lwd=2, srt=90, pos=2)

# Plot default distribution with low correlation
curve(expr=defdistr(x, threshv=threshv, rho=0.01),
  xlab="default percentage", ylab="", lwd=2,
  col="green", main="Distribution of Defaults")
# Plot default distribution with high correlation
curve(expr=defdistr(x, threshv=threshv, rho=0.99),
  xlab="percentage of defaults", ylab="density",
  add=TRUE, lwd=2, n=10001, col="blue", main="")

# Add legend
legend(x="top", legend=c("high correlation", "low correlation"),
   title=NULL, inset=0.1, cex=1.0, bg="white",
   bty="n", lwd=6, lty=1, col=c("blue", "green"))
# Add unconditional default probability
abline(v=0.1, col="red", lwd=2)
text(x=0.1, y=10, lwd=2, pos=4, labels="default probability")

# Get help for integrate()
?integrate
# Calculate slowly converging integral
func <- function(x) {1/((x+1)*sqrt(x))}
integrate(func, lower=0, upper=10)
integrate(func, lower=0, upper=Inf)
# Integrate function with parameter lambdaf
func <- function(x, lambdaf=1) {
  exp(-x*lambdaf)
}  # end func
integrate(func, lower=0, upper=Inf)
integrate(func, lower=0, upper=Inf, lambdaf=2)
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
defprob <- 0.05; threshv <- qnorm(defprob)
# Define Vasicek cumulative loss distribution
cumlossdistr <- function(x, threshv=(-2), rho=0.2, lgd=0.4)
  pnorm((sqrt(1-rho)*qnorm(x/lgd) - threshv)/sqrt(rho))
# Define Vasicek loss distribution function
lossdistr <- function(x, threshv=(-2), rho=0.2, lgd=0.4)
  sqrt((1-rho)/rho)*exp(-(sqrt(1-rho)*qnorm(x/lgd) - threshv)^2/(2*rho) + qnorm(x/lgd)^2/2)/lgd
integrate(lossdistr, low=0, up=lgd, threshv=(-2), rho=rho, lgd=lgd)

# Plot probability distribution of losses
x11(width=6, height=5)
curve(expr=lossdistr(x, threshv=threshv, rho=rho),
cex.main=1.8, cex.lab=1.8, cex.axis=1.5,
type="l", xlim=c(0, 0.06),
xlab="loss percentage", ylab="density", lwd=3,
col="blue", main="Portfolio Loss Density")
# Add line for expected loss
abline(v=lgd*defprob, col="red", lwd=3)
text(x=lgd*defprob-0.001, y=35, labels="expected loss", lwd=3, pos=4, cex=1.8)

# Define Vasicek cumulative loss distribution
# (with error handling for x)
cumlossdistr <- function(x, threshv=(-2), rho=0.2, lgd=0.4) {
  qnormv <- ifelse(x/lgd < 0.999, qnorm(x/lgd), 3.1)
  pnorm((sqrt(1-rho)*qnormv - threshv)/sqrt(rho))
}  # end cumlossdistr
# Define Vasicek loss distribution function
# (vectorized version with error handling for x)
lossdistr <- function(x, threshv=(-2), rho=0.1, lgd=0.4) {
  qnormv <- ifelse(x/lgd < 0.999, qnorm(x/lgd), 3.1)
  sqrt((1-rho)/rho)*exp(-(sqrt(1-rho)*qnormv - threshv)^2/(2*rho) + qnormv^2/2)/lgd
}  # end lossdistr

defprob <- 0.2; threshv <- qnorm(defprob)
rho <- 0.1; lgd <- 0.4
attachp <- 0.15; detachp <- 0.2
# Expected tranche loss is sum of two terms
tranchel <-
  # Loss between attachp and detachp
  integrate(function(x) (x-attachp)*
    lossdistr(x, threshv=threshv, rho=rho, lgd=lgd),
    low=attachp, up=detachp)$value/(detachp-attachp) +
  # Loss in excess of detachp
  (1-cumlossdistr(x=detachp, threshv=threshv, rho=rho, lgd=lgd))
# Plot probability distribution of losses
curve(expr=lossdistr(x, threshv=threshv, rho=rho),
cex.main=1.8, cex.lab=1.8, cex.axis=1.5,
type="l", xlim=c(0, 3*lgd*defprob),
xlab="loss percentage", ylab="density", lwd=3,
col="orange", main="CDO Tranche Losses")
# Add line for expected loss
abline(v=lgd*defprob, col="red", lwd=3)
text(x=lgd*defprob-0.001, y=4, labels="expected loss",
 lwd=2, srt=90, pos=3, cex=1.8)
# Add lines for attach and detach
abline(v=attachp, col="blue", lwd=3)
text(x=attachp-0.001, y=4, labels="attach",
 lwd=2, srt=90, pos=3, cex=1.8)
abline(v=detachp, col="green", lwd=3)
text(x=detachp-0.001, y=4, labels="detach",
 lwd=2, srt=90, pos=3, cex=1.8)
# Add shading for CDO tranche
vars <- seq(attachp, detachp, length=100)
densv <- sapply(vars, lossdistr, threshv=threshv, rho=rho)
# Draw shaded polygon
polygon(c(attachp, vars, detachp), density=20,
  c(-1, densv, -1), col="red", border=NA)
text(x=0.5*(attachp+detachp), y=0, labels="CDO tranche", cex=1.8, lwd=2, pos=3)

# Add lines for unexpected loss
abline(v=0.04, col="blue", lwd=3)
arrows(x0=0.02, y0=35, x1=0.04, y1=35, code=3, lwd=3, cex=0.5)
text(x=0.03, y=36, labels="unexpected loss", lwd=2, pos=3)
# Add lines for VaR
abline(v=0.055, col="red", lwd=3)
arrows(x0=0.0, y0=25, x1=0.055, y1=25, code=3, lwd=3, cex=0.5)
text(x=0.03, y=26, labels="VaR", lwd=2, pos=3)
text(x=0.055-0.001, y=10, labels="VaR", lwd=2, srt=90, pos=3)

varisk <- 0.04; varmax <- 4*lgd*defprob
# Calculate CVaR
cvar <- integrate(function(x) x*lossdistr(x, threshv=threshv,
  rho=rho, lgd=lgd), low=varisk, up=lgd)$value
cvar <- cvar/integrate(lossdistr, low=varisk, up=lgd,
   threshv=threshv, rho=rho, lgd=lgd)$value
# Plot probability distribution of losses
curve(expr=lossdistr(x, threshv=threshv, rho=rho),
type="l", xlim=c(0, 0.06),
xlab="loss percentage", ylab="density", lwd=3,
col="blue", main="Conditional Value at Risk")
# Add line for expected loss
abline(v=lgd*defprob, col="red", lwd=3)
text(x=lgd*defprob-0.001, y=10, labels="expected loss", lwd=2, srt=90, pos=3)

# Add lines for VaR
abline(v=varisk, col="red", lwd=3)
text(x=varisk-0.001, y=10, labels="VaR",
 lwd=2, srt=90, pos=3)
# Add shading for CVaR
vars <- seq(varisk, varmax, length=100)
densv <- sapply(vars, lossdistr,
  threshv=threshv, rho=rho)
# Draw shaded polygon
polygon(c(varisk, vars, varmax), density=20,
  c(-1, densv, -1), col="red", border=NA)
text(x=varisk+0.005, y=0, labels="CVaR", lwd=2, pos=3)

# VaR (quantile of the loss distribution)
varfun <- function(x, threshv=qnorm(0.1), rho=0.1, lgd=0.4)
  lgd*pnorm((sqrt(rho)*qnorm(x) + threshv)/sqrt(1-rho))
varfun(x=0.99, threshv=threshv, rho=rho, lgd=lgd)
# Plot VaR
curve(expr=varfun(x, threshv=threshv, rho=rho, lgd=lgd),
type="l", xlim=c(0, 0.999), xlab="confidence level", ylab="VaR", lwd=3,
col="orange", main="VaR versus Confidence Level")
# Add line for expected loss
abline(h=lgd*defprob, col="red", lwd=3)
text(x=0.2, y=lgd*defprob, labels="expected loss", lwd=2, pos=3)

# Integrate lossdistr() over full range
integrate(lossdistr, low=0.0, up=lgd,
    threshv=threshv, rho=rho, lgd=lgd)
# Calculate expected losses using lossdistr()
integrate(function(x) x*lossdistr(x, threshv=threshv,
  rho=rho, lgd=lgd), low=0.0, up=lgd)
# Calculate confidence levels corresponding to VaR values
vars <- seq(0.07, 0.12, 0.001)
conv <- sapply(vars, function(varisk) {
  integrate(lossdistr, low=varisk, up=lgd,
      threshv=threshv, rho=rho, lgd=lgd)
})  # end sapply
conv <- cbind(as.numeric(t(conv)[, 1]), vars)
colnames(conv) <- c("levels", "VaRs")
# Calculate 95% confidence level VaR value
conv[match(TRUE, conv[, "levels"] < 0.05), "VaRs"]
plot(x=1-conv[, "levels"],
     y=conv[, "VaRs"], lwd=2,
     xlab="confidence level", ylab="VaRs",
     t="l", main="VaR Values and Confidence Levels")

# Calculate CVaR values
cvars <- sapply(vars, function(varisk) {
  integrate(function(x) x*lossdistr(x, threshv=threshv,
rho=rho, lgd=lgd), low=varisk, up=lgd)})  # end sapply
conv <- cbind(conv, as.numeric(t(cvars)[, 1]))
colnames(conv)[3] <- "CVaRs"
# Divide CVaR by confidence level
conv[, "CVaRs"] <- conv[, "CVaRs"]/conv[, "levels"]
# Calculate 95% confidence level CVaR value
conv[match(TRUE, conv[, "levels"] < 0.05), "CVaRs"]
# Plot CVaRs
plot(x=1-conv[, "levels"], y=conv[, "CVaRs"],
     t="l", col="red", lwd=2,
     ylim=range(conv[, c("VaRs", "CVaRs")]),
     xlab="confidence level", ylab="CVaRs",
     main="CVaR Values and Confidence Levels")

# Add VaRs
lines(x=1-conv[, "levels"], y=conv[, "VaRs"], lwd=2)
# Add legend
legend(x="topleft", legend=c("CVaRs", "VaRs"),
   title="default probability = 5%
correlation = 10%
loss given default = 40%",
   inset=0.1, cex=1.0, bg="white", bty="n",
   lwd=6, lty=1, col=c("red", "black"))

# Define model parameters
nbonds <- 300; nsimu <- 1000; lgd <- 0.4
# Define correlation parameters
rho <- 0.2; rhos <- sqrt(rho); rhosm <- sqrt(1-rho)
# Calculate default probabilities and thresholds
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
probv <- runif(nbonds, max=0.2)
threshv <- qnorm(probv)
# Simulate losses under the Vasicek model
sysv <- rnorm(nsimu)
assetm <- matrix(rnorm(nsimu*nbonds), ncol=nsimu)
assetm <- t(rhos*sysv + t(rhosm*assetm))
lossv <- lgd*colSums(assetm < threshv)/nbonds

# Calculate VaR from confidence level
confl <- 0.95
varisk <- quantile(lossv, confl)
# Calculate the CVaR as the mean losses in excess of VaR
cvar <- mean(lossv[lossv > varisk])
# Plot the density of portfolio losses
densv <- density(lossv, from=0)
plot(densv, xlab="loss percentage", ylab="density",
   cex.main=1.0, cex.lab=1.0, cex.axis=1.0,
   lwd=3, col="blue", main="Portfolio Loss Distribution")
# Add vertical line for expected loss
exploss <- lgd*mean(probv)
abline(v=exploss, col="red", lwd=3)
xmax <- max(densv$x); ymax <- max(densv$y)
text(x=exploss, y=(6*ymax/7), labels="expected loss",
     lwd=2, pos=4, cex=1.0)
# Add vertical line for VaR
abline(v=varisk, col="red", lwd=3)
text(x=varisk, y=4*ymax/5, labels="VaR", lwd=2, pos=4, cex=1.0)

# Draw shaded polygon for CVaR
intail <- (densv$x > varisk)
xvar <- c(min(densv$x[intail]), densv$x[intail], max(densv$x))
polygon(xvar, c(-1, densv$y[intail], -1), col="red", border=NA, density=10)
# Add text for CVaR
text(x=5*varisk/4, y=(ymax/7), labels="CVaR", lwd=2, pos=4, cex=1.0)
# Add text with data
text(xmax, ymax, labels=paste0(
   "Expected Loss = ", format(100*exploss, digits=3), "%", "\n",
   "Loss severity = ", format(100*lgd, digits=3), "%", "\n",
   "Correlation = ", format(100*rho, digits=3), "%", "\n",
   "VaR = ", format(100*varisk, digits=3), "%", "\n",
   "CVaR = ", format(100*cvar, digits=3), "%"),
   adj=c(1, 1), cex=1.0, lwd=2)

# Calculate VaRs from confidence levels
conv <- seq(0.93, 0.99, 0.01)
vars <- quantile(lossv, probs=conv)
plot(x=conv, y=vars, t="l", lwd=2,
   xlab="confidence level", ylab="VaRs",
   main="Simulated VaR and Confidence Levels")

# Calculate CVaRs
cvars <- sapply(vars, function(varisk) {
  mean(lossv[lossv >= varisk])
})  # end sapply
cvars <- cbind(cvars, vars)
# Alternative CVaR calculation using frequency table
# first calculate frequency table of losses
# tablev <- table(lossv)/nsimu
# Calculate CVaRs from frequency table
# cvars <- sapply(vars, function(varisk) {
#   tailrisk <- tablev[names(tablev) > varisk]
#   tailrisk %*% as.numeric(names(tailrisk)) / sum(tailrisk)
# })  # end sapply

# Plot CVaRs
plot(x=conv, y=cvars[, "cvars"],
   t="l", col="red", lwd=2, ylim=range(cvars),
   xlab="confidence level", ylab="CVaRs",
   main="Simulated CVaR and Confidence Levels")
# Add VaRs
lines(x=conv, y=cvars[, "vars"], lwd=2)
# Add legend
legend(x="topleft", legend=c("CVaRs", "VaRs"), bty="n",
   title=NULL, inset=0.05, cex=1.0, bg="white",
   y.intersp=0.3, lwd=6, lty=1, col=c("red", "black"))

calc_var <- function(threshv, # Default thresholds
   lgd=0.6, # loss given default
   rhos, rhosm, # asset correlation
   nsimu=1000, # number of simulations
   conv=seq(0.93, 0.99, 0.01) # Confidence levels
   ) {
  # Define model parameters
  nbonds <- NROW(threshv)
  # Simulate losses under the Vasicek model
  sysv <- rnorm(nsimu)
  assetm <- matrix(rnorm(nsimu*nbonds), ncol=nsimu)
  assetm <- t(rhos*sysv + t(rhosm*assetm))
  lossv <- lgd*colSums(assetm < threshv)/nbonds
  # Calculate VaRs and CVaRs
  vars <- quantile(lossv, probs=conv)
  cvars <- sapply(vars, function(varisk) {
    mean(lossv[lossv >= varisk])
  })  # end sapply
  names(vars) <- conv
  names(cvars) <- conv
  c(vars, cvars)
}  # end calc_var

# Define model parameters
nbonds <- 300; nsimu <- 1000; lgd <- 0.4
rho <- 0.2; rhos <- sqrt(rho); rhosm <- sqrt(1-rho)
# Calculate default probabilities and thresholds
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
probv <- runif(nbonds, max=0.2)
threshv <- qnorm(probv)
conv <- seq(0.93, 0.99, 0.01)
# Define number of bootstrap simulations
nboot <- 500
# Perform bootstrap of calc_var
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
bootd <- sapply(rep(lgd, nboot), calc_var,
  threshv=threshv,
  rhos=rhos, rhosm=rhosm,
  nsimu=nsimu, conv=conv)  # end sapply
bootd <- t(bootd)
# Calculate standard errors of VaR and CVaR from bootd data
varsd <- apply(bootd[, 1:7], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
cvarsd <- apply(bootd[, 8:14], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
# Scale the standard errors of VaRs and CVaRs
varsds <- varsd[2, ]/varsd[1, ]
cvarsds <- cvarsd[2, ]/cvarsd[1, ]

# Plot the scaled standard errors of VaRs and CVaRs
plot(x=names(varsds), y=varsds,
  t="l", lwd=2, ylim=range(c(varsds, cvarsds)),
  xlab="confidence level", ylab="standard error",
  main="Scaled Standard Errors of CVaR and VaR")
lines(x=names(cvarsds), y=cvarsds, lwd=2, col="red")
legend(x="topleft", legend=c("CVaRs", "VaRs"), bty="n",
   title=NULL, inset=0.05, cex=1.0, bg="white",
   y.intersp=0.3, lwd=6, lty=1, col=c("red", "black"))

library(parallel)  # load package parallel
ncores <- detectCores() - 1  # number of cores
compclust <- makeCluster(ncores)  # Initialize compute cluster
# Perform bootstrap of calc_var for Windows
clusterSetRNGStream(compclust, 1121)
bootd <- parLapply(compclust, rep(lgd, nboot),
  fun=calc_var, threshv=threshv,
  rhos=rhos, rhosm=rhosm,
  nsimu=nsimu, conv=conv)  # end parLapply
stopCluster(compclust)  # Stop R processes over cluster
# Bootstrap under Mac-OSX or Linux
bootd <- mclapply(rep(lgd, nboot),
  FUN=calc_var, threshv=threshv,
  rhos=rhos, rhosm=rhosm,
  nsimu=nsimu, conv=conv)  # end mclapply
bootd <- rutils::do_call(rbind, bootd)
# Calculate standard errors of VaR and CVaR from bootd data
varsd <- apply(bootd[, 1:7], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
cvarsd <- apply(bootd[, 8:14], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
# Scale the standard errors of VaRs and CVaRs
varsds <- varsd[2, ]/varsd[1, ]
cvarsds <- cvarsd[2, ]/cvarsd[1, ]

# Plot the standard errors of VaRs and CVaRs
plot(x=names(varsds), y=varsds, t="l", lwd=2,
  ylim=range(c(varsds, cvarsds)),
  xlab="confidence level", ylab="standard error",
  main="Scaled Standard Errors of CVaR and VaR")
lines(x=names(cvarsds), y=cvarsds, lwd=2, col="red")
legend(x="topleft", legend=c("CVaRs", "VaRs"), bty="n",
   title=NULL, inset=0.05, cex=1.0, bg="white",
   y.intersp=0.3, lwd=6, lty=1, col=c("red", "black"))

calc_var <- function(probv, # Default probabilities
   lgd=0.6, # loss given default
   rhos, rhosm, # asset correlation
   nsimu=1000, # number of simulations
   conv=seq(0.93, 0.99, 0.01) # Confidence levels
   ) {
  # Calculate random default thresholds
  threshv <- qnorm(runif(1, min=0.5, max=1.5)*probv)
  # Simulate losses under the Vasicek model
  nbonds <- NROW(probv)
  sysv <- rnorm(nsimu)
  assetm <- matrix(rnorm(nsimu*nbonds), ncol=nsimu)
  assetm <- t(rhos*sysv + t(rhosm*assetm))
  lossv <- lgd*colSums(assetm < threshv)/nbonds
  # Calculate VaRs and CVaRs
  vars <- quantile(lossv, probs=conv)
  cvars <- sapply(vars, function(varisk) {
    mean(lossv[lossv >= varisk])
  })  # end sapply
  names(vars) <- conv
  names(cvars) <- conv
  c(vars, cvars)
}  # end calc_var

library(parallel)  # load package parallel
ncores <- detectCores() - 1  # number of cores
compclust <- makeCluster(ncores)  # Initialize compute cluster
# Perform bootstrap of calc_var for Windows
clusterSetRNGStream(compclust, 1121)
bootd <- parLapply(compclust, rep(lgd, nboot),
  fun=calc_var, probv=probv, rhos=rhos, rhosm=rhosm,
  nsimu=nsimu, conv=conv)  # end parLapply
stopCluster(compclust)  # Stop R processes over cluster
# Bootstrap under Mac-OSX or Linux
bootd <- mclapply(rep(lgd, nboot),
  FUN=calc_var, probv=probv, rhos=rhos, rhosm=rhosm,
  nsimu=nsimu, conv=conv)  # end mclapply
bootd <- rutils::do_call(rbind, bootd)
# Calculate standard errors of VaR and CVaR from bootd data
varsd <- apply(bootd[, 1:7], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
cvarsd <- apply(bootd[, 8:14], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
# Scale the standard errors of VaRs and CVaRs
varsdsu <- varsd[2, ]/varsd[1, ]
cvarsdsu <- cvarsd[2, ]/cvarsd[1, ]

# Plot the standard errors of VaRs under random default probabilities
plot(x=colnames(varsd), y=varsds, t="l",
 col="black", lwd=2, ylim=range(c(varsds, varsdsu)),
  xlab="confidence level", ylab="standard error",
  main="Standard Errors of VaR
  with Random Default Probabilities")
lines(x=colnames(varsd), y=varsdsu, lwd=2, col="red")
legend(x="topleft",
   legend=c("VaR Fixed Def Probs", "VaR Random Def Probs"),
   bty="n", title=NULL, inset=0.05, cex=1.0, bg="white",
   y.intersp=0.3, lwd=6, lty=1, col=c("black", "red"))

NA

# Plot the standard errors of VaRs and CVaRs
plot(x=colnames(varsd), y=varsdsu, t="l", lwd=2,
  ylim=range(c(varsdsu, cvarsdsu)),
  xlab="confidence level", ylab="standard error",
  main="Relative Standard Errors of VaR and CVaR
  with Random Default Probabilities")
lines(x=colnames(varsd), y=cvarsdsu, lwd=2, col="red")
legend(x="topright", legend=c("CVaR", "VaR"), bty="n",
   title=NULL, inset=0.05, cex=1.0, bg="white",
   y.intersp=0.3, lwd=6, lty=1, col=c("red", "black"))

##########################################
# Summary: Calculate the VaR and CVaR of a credit
# portfolio using importance sampling.

#Run the setup code below.

# Define model parameters
nbonds <- 10; nsimu <- 1000; lgd <- 0.4
# Define correlation parameters
rho <- 0.2; rhos <- sqrt(rho); rhosm <- sqrt(1-rho)
# Calculate default probabilities and thresholds
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
probv <- runif(nbonds, max=0.2)
threshv <- qnorm(probv)
# Simulate losses under the Vasicek model
sysv <- rnorm(nsimu)
assetm <- matrix(rnorm(nsimu*nbonds), ncol=nsimu)
assetm <- t(rhos*sysv + t(rhosm*assetm))
lossv <- lgd*colSums(assetm < threshv)/nbonds

# Calculate VaR from confidence level
confl <- 0.95
varisk <- quantile(lossv, confl)

cumprob <- (1:nsimu)/nsimu
sort(lossv)[findInterval(confl, cumprob)]

# Calculate the CVaR as the mean losses in excess of VaR
cvar <- mean(lossv[lossv > varisk])


# Apply importance sampling
lambdaf <- (-0.1)
assett <- assetm + lambdaf
weightv <- exp(-lambdaf*assett + lambdaf^2/2)

losst <- lgd*colSums(assett < threshv)/nbonds
lossw <- matrixStats::colProds(weightv)

# Calculate the CVaR using importance sampling - doesn't work
sum((losst < varisk)*lossw)/nsimu
# Or
losst <- lgd*colSums((assett < threshv)*weightv)/nbonds
mean(losst[losst > varisk])

# Data must be sorted for importance sampling of VaR
ordern <- order(losst)
losst <- losst[ordern]
lossw <- lossw[ordern]

# Calculate the VaR using importance sampling - doesn't work
cumprob <- cumsum(lossw)/nsimu
losst[findInterval(confl, cumprob)]


# Create a random real symmetric matrix
matv <- matrix(runif(25), nc=5)
matv <- matv + t(matv)
# Calculate the eigenvalues and eigenvectors
eigend <- eigen(matv)
eigenvec <- eigend$vectors
dim(eigenvec)
# Plot eigenvalues
barplot(eigend$values, xlab="", ylab="", las=3,
  names.arg=paste0("ev", 1:NROW(eigend$values)),
  main="Eigenvalues of a real symmetric matrix")

# Eigenvectors form an orthonormal basis
round(t(eigenvec) %*% eigenvec, digits=4)
# Diagonalize matrix using eigenvector matrix
round(t(eigenvec) %*% (matv %*% eigenvec), digits=4)
eigend$values
# Eigen decomposition of matrix by rotating the diagonal matrix
matrixe <- eigenvec %*% (eigend$values * t(eigenvec))
# Create diagonal matrix of eigenvalues
# diagmat <- diag(eigend$values)
# matrixe <- eigenvec %*% (diagmat %*% t(eigenvec))
all.equal(matv, matrixe)

# Create a random positive semi-definite matrix
matv <- matrix(runif(25), nc=5)
matv <- t(matv) %*% matv
# Calculate the eigenvalues and eigenvectors
eigend <- eigen(matv)
eigend$values
# Plot eigenvalues
barplot(eigend$values, las=3, xlab="", ylab="",
  names.arg=paste0("ev", 1:NROW(eigend$values)),
  main="Eigenvalues of positive semi-definite matrix")

# Perform singular value decomposition
matv <- matrix(rnorm(50), nc=5)
svdec <- svd(matv)
# Recompose matv from SVD mat_rices
all.equal(matv, svdec$u %*% (svdec$d*t(svdec$v)))
# Columns of U and V are orthonormal
round(t(svdec$u) %*% svdec$u, 4)
round(t(svdec$v) %*% svdec$v, 4)

# Dimensions of left and right matrices
nrows <- 6 ; ncols <- 4
# Calculate the left matrix
leftmat <- matrix(runif(nrows^2), nc=nrows)
eigend <- eigen(crossprod(leftmat))
leftmat <- eigend$vectors[, 1:ncols]
# Calculate the right matrix and singular values
rightmat <- matrix(runif(ncols^2), nc=ncols)
eigend <- eigen(crossprod(rightmat))
rightmat <- eigend$vectors
singval <- sort(runif(ncols, min=1, max=5), decreasing=TRUE)
# Compose rectangular matrix
matv <- leftmat %*% (singval * t(rightmat))
# Perform singular value decomposition
svdec <- svd(matv)
# Recompose matv from SVD
all.equal(matv, svdec$u %*% (svdec$d*t(svdec$v)))
# Compare SVD with matv components
all.equal(abs(svdec$u), abs(leftmat))
all.equal(abs(svdec$v), abs(rightmat))
all.equal(svdec$d, singval)
# Eigen decomposition of matv squared
retsq <- matv %*% t(matv)
eigend <- eigen(retsq)
all.equal(eigend$values[1:ncols], singval^2)
all.equal(abs(eigend$vectors[, 1:ncols]), abs(leftmat))
# Eigen decomposition of matv squared
retsq <- t(matv) %*% matv
eigend <- eigen(retsq)
all.equal(eigend$values, singval^2)
all.equal(abs(eigend$vectors), abs(rightmat))

# Create a random positive semi-definite matrix
matv <- matrix(runif(25), nc=5)
matv <- t(matv) %*% matv
# Calculate the inverse of matv
invmat <- solve(a=matv)
# Multiply inverse with matrix
round(invmat %*% matv, 4)
round(matv %*% invmat, 4)
# Calculate the eigenvalues and eigenvectors
eigend <- eigen(matv)
eigenvec <- eigend$vectors
# Calculate the inverse from eigen decomposition
inveigen <- eigenvec %*% (t(eigenvec) / eigend$values)
all.equal(invmat, inveigen)
# Decompose diagonal matrix with inverse of eigenvalues
# diagmat <- diag(1/eigend$values)
# inveigen <- eigenvec %*% (diagmat %*% t(eigenvec))

# Random rectangular matrix: nrows > ncols
nrows <- 6 ; ncols <- 4
matv <- matrix(runif(nrows*ncols), nc=ncols)
# Calculate the generalized inverse of matv
invmat <- MASS::ginv(matv)
round(invmat %*% matv, 4)
all.equal(matv, matv %*% invmat %*% matv)
# Random rectangular matrix: nrows < ncols
nrows <- 4 ; ncols <- 6
matv <- matrix(runif(nrows*ncols), nc=ncols)
# Calculate the generalized inverse of matv
invmat <- MASS::ginv(matv)
all.equal(matv, matv %*% invmat %*% matv)
round(matv %*% invmat, 4)
round(invmat %*% matv, 4)
# Perform singular value decomposition
svdec <- svd(matv)
# Calculate the generalized inverse from SVD
invsvd <- svdec$v %*% (t(svdec$u) / svdec$d)
all.equal(invsvd, invmat)
# Calculate the Moore-Penrose pseudo-inverse
invmp <- MASS::ginv(t(matv) %*% matv) %*% t(matv)
all.equal(invmp, invmat)

# Create a random singular matrix
# More columns than rows: ncols > nrows
nrows <- 4 ; ncols <- 6
matv <- matrix(runif(nrows*ncols), nc=ncols)
matv <- t(matv) %*% matv
# Perform singular value decomposition
svdec <- svd(matv)
# Incorrect inverse from SVD because of zero singular values
invsvd <- svdec$v %*% (t(svdec$u) / svdec$d)
# Generalized inverse property doesn't hold
all.equal(matv %*% invsvd %*% matv, matv)

# Set tolerance for determining zero singular values
precv <- sqrt(.Machine$double.eps)
# Check for zero singular values
round(svdec$d, 12)
notzero <- (svdec$d > (precv*svdec$d[1]))
# Calculate the regularized inverse from SVD
invsvd <- svdec$v[, notzero] %*%
  (t(svdec$u[, notzero]) / svdec$d[notzero])
# Verify inverse property of matv
all.equal(matv %*% invsvd %*% matv, matv)
# Calculate the regularized inverse using MASS::ginv()
invmat <- MASS::ginv(matv)
all.equal(invsvd, invmat)
# Calculate the Moore-Penrose pseudo-inverse
invmp <- MASS::ginv(t(matv) %*% matv) %*% t(matv)
all.equal(invmp, invmat)

# Diagonalize the unit matrix
unitmat <- matv %*% invmat
round(unitmat, 4)
round(matv %*% invmat, 4)
round(t(svdec$u) %*% unitmat %*% svdec$v, 4)

# Define a square matrix
matv <- matrix(c(1, 2, -1, 2), nc=2)
vecv <- c(2, 1)
# Calculate the inverse of matv
invmat <- solve(a=matv)
invmat %*% matv
# Calculate the solution using inverse of matv
solutionv <- invmat %*% vecv
matv %*% solutionv
# Calculate the solution of linear system
solutionv <- solve(a=matv, b=vecv)
matv %*% solutionv

# Create a random matrix
matv <- matrix(rnorm(100), nc=10)
# Calculate the matrix inverse using solve()
invmatr <- solve(a=matv)
round(invmatr %*% matv, 4)
# Compile the C++ file using Rcpp
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/calc_invmat.cpp")
# Calculate the matrix inverse using C++
invmat <- calc_invmat(matv)
all.equal(invmat, invmatr)
all.equal(invmat, MASS::ginv(matv))
# Compare the speed of RcppArmadillo with R code
library(microbenchmark)
summary(microbenchmark(
  ginv=MASS::ginv(matv),
  solve=solve(matv),
  cpp=calc_invmat(matv),
  times=10))[, c(1, 4, 5)]

# Create large random positive semi-definite matrix
matv <- matrix(runif(1e4), nc=100)
matv <- t(matv) %*% matv
# Calculate the eigen decomposition
eigend <- eigen(matv)
eigenval <- eigend$values
eigenvec <- eigend$vectors
# Set tolerance for determining zero singular values
precv <- sqrt(.Machine$double.eps)
# If needed convert to positive definite matrix
notzero <- (eigenval > (precv*eigenval[1]))
if (sum(!notzero) > 0) {
  eigenval[!notzero] <- 2*precv
  matv <- eigenvec %*% (eigenval * t(eigenvec))
}  # end if
# Calculate the Cholesky matv
cholmat <- chol(matv)
cholmat[1:5, 1:5]
all.equal(matv, t(cholmat) %*% cholmat)
# Calculate the inverse from Cholesky
invchol <- chol2inv(cholmat)
all.equal(solve(matv), invchol)
# Compare speed of Cholesky inversion
library(microbenchmark)
summary(microbenchmark(
  solve=solve(matv),
  cholmat=chol2inv(chol(matv)),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# Calculate the random covariance matrix
covmat <- matrix(runif(25), nc=5)
covmat <- t(covmat) %*% covmat
# Calculate the Cholesky matrix
cholmat <- chol(covmat)
cholmat
# Simulate random uncorrelated returns
nassets <- 5
nrows <- 10000
retp <- matrix(rnorm(nassets*nrows), nc=nassets)
# Calculate the correlated returns by applying Cholesky
retscorr <- retp %*% cholmat
# Calculate the covariance matrix
covmat2 <- cov(retscorr)
all.equal(covmat, covmat2)

# Simulate random stock returns
nassets <- 10
nrows <- 100
# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
retp <- matrix(rnorm(nassets*nrows), nc=nassets)
# Calculate the centered (de-meaned) returns matrix
retp <- t(t(retp) - colMeans(retp))
# Or
retp <- apply(retp, MARGIN=2, function(x) (x-mean(x)))
# Calculate the covariance matrix
covmat <- crossprod(retp) /(nrows-1)
# Calculate the eigenvalues and eigenvectors
eigend <- eigen(covmat)
eigend$values
barplot(eigend$values, # Plot eigenvalues
  xlab="", ylab="", las=3,
  names.arg=paste0("ev", 1:NROW(eigend$values)),
  main="Eigenvalues of Covariance Matrix")

# Calculate the eigenvalues and eigenvectors
# as function of number of returns
ndata <- ((nassets/2):(2*nassets))
eigenval <- sapply(ndata, function(x) {
  retp <- retp[1:x, ]
  retp <- apply(retp, MARGIN=2, function(y) (y - mean(y)))
  covmat <- crossprod(retp) / (x-1)
  min(eigen(covmat)$values)
})  # end sapply
plot(y=eigenval, x=ndata, t="l", xlab="", ylab="", lwd=3, col="blue",
  main="Smallest eigenvalue of covariance matrix
  as function of number of returns")

# Create rectangular matrix with collinear columns
matv <- matrix(rnorm(10*8), nc=10)
# Calculate the covariance matrix
covmat <- cov(matv)
# Calculate the inverse of covmat - error
invmat <- solve(covmat)
# Calculate the regularized inverse of covmat
invmat <- MASS::ginv(covmat)
# Verify inverse property of matv
all.equal(covmat %*% invmat %*% covmat, covmat)
# Perform eigen decomposition
eigend <- eigen(covmat)
eigenvec <- eigend$vectors
eigenval <- eigend$values
# Set tolerance for determining zero singular values
precv <- sqrt(.Machine$double.eps)
# Calculate the regularized inverse matrix
notzero <- (eigenval > (precv * eigenval[1]))
invreg <- eigenvec[, notzero] %*%
  (t(eigenvec[, notzero]) / eigenval[notzero])
# Verify that invmat is same as invreg
all.equal(invmat, invreg)

# Calculate the regularized inverse matrix using cutoff
dimax <- 3
invmat <- eigenvec[, 1:dimax] %*%
  (t(eigenvec[, 1:dimax]) / eigend$values[1:dimax])
# Verify that invmat is same as invreg
all.equal(invmat, invreg)

# Create a random covariance matrix
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
matv <- matrix(rnorm(5e2), nc=5)
covmat <- cov(matv)
cormat <- cor(matv)
stdev <- sqrt(diag(covmat))
# Calculate the target matrix
cormean <- mean(cormat[upper.tri(cormat)])
targetmat <- matrix(cormean, nr=NROW(covmat), nc=NCOL(covmat))
diag(targetmat) <- 1
targetmat <- t(t(targetmat * stdev) * stdev)
# Calculate the shrinkage covariance matrix
alphac <- 0.5
covshrink <- (1-alphac)*covmat + alphac*targetmat
# Calculate the inverse matrix
invmat <- solve(covshrink)

# Create a random matrix
matv <- matrix(rnorm(100), nc=10)
# Calculate the inverse of matv
invmat <- solve(a=matv)
# Multiply inverse with matrix
round(invmat %*% matv, 4)
# Add noise to the initial inverse
invmatr <- invmat + matrix(rnorm(100, sd=0.1), nc=10)
# Calculate the approximate recursive inverse of matv
invmatr <- (2*invmatr - invmatr %*% matv %*% invmatr)
# Calculate the sum of the off-diagonal elements
uppert <- upper.tri(matv)
sum((invmatr %*% matv)[uppert])

# Add noise to the initial inverse
invmatr <- invmat + matrix(rnorm(100, sd=0.1), nc=10)
sum((invmatr %*% matv)[uppert])
# Calculate the recursive inverse of matv in a loop
iterv <- numeric(5)
for (x in 1:5) {
# Calculate the recursive inverse of matv
  invmatr <- (2*invmatr - invmatr %*% matv %*% invmatr)
# Calculate the sum of the off-diagonal elements
  iterv[x] <- sum((invmatr %*% matv)[uppert])
}  # end for
# Plot the iterations
plot(x=1:5, y=iterv, t="l", xlab="iterations", ylab="error",
     main="Iterations of Recursive Matrix Inverse")

# Create a plotting expression
expv <- quote({
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
expv
# Create plot by evaluating the plotting expression
x11(width=6, height=4)
eval(expv)

library(animation)
# Create an expression for creating multiple plots
expv <- quote({
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
eval(expv)
# Create gif with animated plot
animation::saveGIF(expr=eval(expv),
  movie.name="chi_squared.gif",
  img.name="chi_squared")
# Create html with animated plot
animation::saveHTML(expr=eval(expv),
  img.name="chi_squared",
  htmlfile="chi_squared.html",
  description="Chi-squared Distributions")  # end saveHTML

NA

#App setup code that runs only once at startup.
ndata <- 1e4
stdev <- 1.0

#Define the user interface
uiface <- shiny::fluidPage(
  # Create numeric input for the number of data points.
  numericInput("ndata", "Number of data points:", value=ndata),
  # Create slider input for the standard deviation parameter.
  sliderInput("stdev", label="Standard deviation:",
        min=0.1, max=3.0, value=stdev, step=0.1),
  # Render plot in a panel.
  plotOutput("plotobj", height=300, width=500)
)  # end user interface

#Define the server function
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

#Create elements of the user interface
uiface <- shiny::fluidPage(
  titlePanel("VWAP Moving Average"),
  # Create single row of widgets with two slider inputs
  fluidRow(
    # Input stock symbol
    column(width=3, selectInput("symbol", label="Symbol",
                          choices=symbolv, selected=symbol)),
    # Input look-back interval
    column(width=3, sliderInput("lookb", label="Lookback interval",
                          min=1, max=150, value=11, step=1))
  ),  # end fluidRow
  # Create output plot panel
  mainPanel(dygraphs::dygraphOutput("dyplot"), width=12)
)  # end fluidPage interface

#Define the server function
servfun <- shiny::shinyServer(function(input, output) {
  # Get the close and volume data in a reactive environment
  closep <- shiny::reactive({
    # Get the data
    ohlc <- get(input$symbol, data_env)
    closep <- log(quantmod::Cl(ohlc))
    volum <- quantmod::Vo(ohlc)
    # Return the data
    cbind(closep, volum)
  })  # end reactive code

  # Calculate the VWAP indicator in a reactive environment
  vwapv <- shiny::reactive({
    # Get model parameters from input argument
    lookb <- input$lookb
    # Calculate the VWAP indicator
    closep <- closep()[, 1]
    volum <- closep()[, 2]
    vwapv <- HighFreq::roll_sum(tseries=closep*volum, lookb=lookb)
    volumroll <- HighFreq::roll_sum(tseries=volum, lookb=lookb)
    vwapv <- vwapv/volumroll
    vwapv[is.na(vwapv)] <- 0
    # Return the plot data
    datav <- cbind(closep, vwapv)
    colnames(datav) <- c(input$symbol, "VWAP")
    datav
  })  # end reactive code

  # Return the dygraph plot to output argument
  output$dyplot <- dygraphs::renderDygraph({
    colv <- colnames(vwapv())
    dygraphs::dygraph(vwapv(), main=paste(colv[1], "VWAP")) %>%
dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
dySeries(name=colv[1], axis="y", label=colv[1], strokeWidth=2, col="blue") %>%
dySeries(name=colv[2], axis="y2", label=colv[2], strokeWidth=2, col="red")
  })  # end output plot
})  # end server code

#Return a Shiny app object
shiny::shinyApp(ui=uiface, server=servfun)

#Define the server function
servfun <- shiny::shinyServer(function(input, output) {

  # Create an empty list of reactive values.
  value_s <- reactiveValues()

  # Get input parameters from the user interface.
  nrows <- reactive({
    # Add nrows to list of reactive values.
    value_s*nrows <- input$nrows
    input$nrows
  })  # end reactive code

  # Broadcast a message to the console when the button is pressed.
  observeEvent(eventExpr=input$button, handlerExpr={
    cat("Input button pressed\n")
  })  # end observeEvent

  # Send the data when the button is pressed.
  datav <- eventReactive(eventExpr=input$button, valueExpr={
    # eventReactive() executes on input$button, but not on nrows() or input$nrows.
    cat("Sending", nrows(), "rows of data\n")
    datav <- head(mtcars, input$nrows)
    value_s$mpg <- mean(datav$mpg)
    datav
  })  # end eventReactive
  #   datav

  # Draw table of the data when the button is pressed.
  observeEvent(eventExpr=input$button, handlerExpr={
    datav <- datav()
    cat("Received", value_s*nrows, "rows of data\n")
    cat("Average mpg = ", value_s$mpg, "\n")
    cat("Drawing table\n")
    output$tablev <- renderTable(datav)
  })  # end observeEvent

})  # end server code

#Return a Shiny app object
shiny::shinyApp(ui=uiface, server=servfun)
