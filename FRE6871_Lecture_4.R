# Display the structure of optimize()
str(optimize)
# Objective function with multiple minima
objfun <- function(input, param1=0.01) {
  sin(0.25*pi*input) + param1*(input-1)^2
}  # end objfun
optiml <- optimize(f=objfun, interval=c(-4, 2))
class(optiml)
unlist(optiml)
# Find minimum in different interval
unlist(optimize(f=objfun, interval=c(0, 8)))
# Find minimum with less accuracy
accl <- 1e4*.Machine$double.eps^0.25
unlist(optimize(f=objfun, interval=c(0, 8), tol=accl))
# Microbenchmark optimize() with less accuracy
library(microbenchmark)
summary(microbenchmark(
  more_accurate = optimize(f=objfun, interval=c(0, 8)),
  less_accurate = optimize(f=objfun, interval=c(0, 8), tol=accl),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary

par(oma=c(1, 1, 1, 1), mgp=c(2, 1, 0), mar=c(5, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# Plot the objective function
curve(expr=objfun, type="l", xlim=c(-8, 9),
xlab="", ylab="", lwd=2)
# Add title
title(main="Objective Function", line=-1)

# Rastrigin function
rastrigin <- function(x, y, param=25) {
  x^2 + y^2 - param*(cos(x) + cos(y))
}  # end rastrigin
# Rastrigin function is vectorized!
rastrigin(c(-10, 5), c(-10, 5))
# Set rgl options and load package rgl
library(rgl)
options(rgl.useNULL=TRUE)
# Draw 3d surface plot of function
rgl::persp3d(x=rastrigin, xlim=c(-10, 10), ylim=c(-10, 10),
  col="green", axes=FALSE, param=15)
# Render the 3d surface plot of function
rgl::rglwidget(elementId="plot3drgl", width=400, height=400)

# Rastrigin function with vector argument for optimization
rastrigin <- function(vecv, param=25) {
  sum(vecv^2 - param*cos(vecv))
}  # end rastrigin
vecv <- c(pi, pi/4)
rastrigin(vecv=vecv)
# Draw 3d surface plot of Rastrigin function
rgl::persp3d(
  x=Vectorize(function(x, y) rastrigin(vecv=c(x, y))),
  xlim=c(-10, 10), ylim=c(-10, 10),
  col="green", axes=FALSE, zlab="", main="rastrigin")
# Optimize with respect to vector argument
optiml <- optim(par=vecv, fn=rastrigin,
        method="L-BFGS-B",
        upper=c(14*pi, 14*pi),
        lower=c(pi/2, pi/2),
        param=1)
# Optimal parameters and value
optiml$par
optiml$value
rastrigin(optiml$par, param=1)

# Sample of normal variables
datav <- rnorm(1000, mean=4, sd=2)
# Objective function is log-likelihood
objfun <- function(parv, datav) {
  sum(2*log(parv[2]) +
    ((datav - parv[1])/parv[2])^2)
}  # end objfun
# Objective function on parameter grid
parmean <- seq(1, 6, length=50)
parsd <- seq(0.5, 3.0, length=50)
objective_grid <- sapply(parmean, function(m) {
  sapply(parsd, function(sd) {
    objfun(c(m, sd), datav)
  })  # end sapply
})  # end sapply
# Perform grid search for minimum
objective_min <- which(
  objective_grid==min(objective_grid),
  arr.ind=TRUE)
objective_min
parmean[objective_min[1]]  # mean
parsd[objective_min[2]]  # sd
objective_grid[objective_min]
objective_grid[(objective_min[, 1] + -1:1),
       (objective_min[, 2] + -1:1)]
# Or create parameter grid using function outer()
objvecive <- Vectorize(
  FUN=function(mean, sd, datav)
    objfun(c(mean, sd), datav),
  vectorize.args=c("mean", "sd")
)  # end Vectorize
objective_grid <- outer(parmean, parsd,
objvecive, datav=datav)

par(cex.lab=2.0, cex.axis=2.0, cex.main=2.0, cex.sub=2.0)
# Perspective plot of log-likelihood function
persp(z=-objective_grid,
theta=45, phi=30, shade=0.5,
border="green", zlab="objective",
main="objective function")
# Interactive perspective plot of log-likelihood function
library(rgl)  # Load package rgl
rgl::par3d(cex=2.0)  # Scale text by factor of 2
rgl::persp3d(z=-objective_grid, zlab="objective",
  col="green", main="objective function")
rgl::rglwidget(elementId="plot3drgl", width=400, height=400)

# Initial parameters
initp <- c(mean=0, sd=1)
# Perform optimization using optim()
optiml <- optim(par=initp,
  fn=objfun, # Log-likelihood function
  datav=datav,
  method="L-BFGS-B", # Quasi-Newton method
  upper=c(10, 10), # Upper constraint
  lower=c(-10, 0.1)) # Lower constraint
# Optimal parameters
optiml$par
# Perform optimization using MASS::fitdistr()
optiml <- MASS::fitdistr(datav, densfun="normal")
optiml$estimate
optiml$sd
# Plot histogram
histp <- hist(datav, plot=FALSE)
plot(histp, freq=FALSE, main="histogram of sample")
curve(expr=dnorm(x, mean=optiml$par["mean"], sd=optiml$par["sd"]),
add=TRUE, type="l", lwd=2, col="red")
legend("topright", inset=0.0, cex=0.8, title=NULL, y.intersp=0.4,
 leg="optimal parameters", lwd=2, bg="white", col="red")

# Sample from mixture of normal distributions
datav <- c(rnorm(100, sd=1.0),
      rnorm(100, mean=4, sd=1.0))
# Objective function is log-likelihood
objfun <- function(parv, datav) {
  likev <- parv[1]/parv[3] *
  dnorm((datav-parv[2])/parv[3]) +
  (1-parv[1])/parv[5]*dnorm((datav-parv[4])/parv[5])
  if (any(likev <= 0)) Inf else
    -sum(log(likev))
}  # end objfun
# Vectorize objective function
objvecive <- Vectorize(
  FUN=function(mean, sd, w, m1, s1, datav)
    objfun(c(w, m1, s1, mean, sd), datav),
  vectorize.args=c("mean", "sd")
)  # end Vectorize
# Objective function on parameter grid
parmean <- seq(3, 5, length=50)
parsd <- seq(0.5, 1.5, length=50)
objective_grid <- outer(parmean, parsd,
    objvecive, datav=datav,
    w=0.5, m1=2.0, s1=2.0)
rownames(objective_grid) <- round(parmean, 2)
colnames(objective_grid) <- round(parsd, 2)
objective_min <- which(objective_grid==
  min(objective_grid), arr.ind=TRUE)
objective_min
objective_grid[objective_min]
objective_grid[(objective_min[, 1] + -1:1),
         (objective_min[, 2] + -1:1)]

# Perspective plot of objective function
persp(parmean, parsd, -objective_grid,
theta=45, phi=30,
shade=0.5,
col=rainbow(50),
border="green",
main="objective function")

# Initial parameters
initp <- c(weight=0.5, m1=0, s1=1, m2=2, s2=1)
# Perform optimization
optiml <- optim(par=initp,
      fn=objfun,
      datav=datav,
      method="L-BFGS-B",
      upper=c(1,10,10,10,10),
      lower=c(0,-10,0.2,-10,0.2))
optiml$par
# Plot histogram
histp <- hist(datav, plot=FALSE)
plot(histp, freq=FALSE,
     main="histogram of sample")
fitfun <- function(x, parv) {
  parv["weight"]*dnorm(x, mean=parv["m1"], sd=parv["s1"]) +
  (1-parv["weight"])*dnorm(x, mean=parv["m2"], sd=parv["s2"])
}  # end fitfun
curve(expr=fitfun(x, parv=optiml$par), add=TRUE,
type="l", lwd=2, col="red")
legend("topright", inset=0.0, cex=0.8, title=NULL,
 leg="optimal parameters", y.intersp=0.4,
 lwd=2, bg="white", col="red")

# Rastrigin function with vector argument for optimization
rastrigin <- function(vecv, param=25) {
  sum(vecv^2 - param*cos(vecv))
}  # end rastrigin
vecv <- c(pi/6, pi/6)
rastrigin(vecv=vecv)
library(DEoptim)
# Optimize rastrigin using DEoptim
optiml <-  DEoptim(rastrigin,
  upper=c(6, 6), lower=c(-6, -6),
  DEoptim.control(trace=FALSE, itermax=50))
# Optimal parameters and value
optiml$optim$bestmem
rastrigin(optiml$optim$bestmem)
summary(optiml)
plot(optiml)

# Symbols for constant maturity Treasury rates
symbolv <- c("DGS1", "DGS2", "DGS5", "DGS10", "DGS20", "DGS30")
# Create new environment for time series
ratesenv <- new.env()
# Download time series for symbolv into ratesenv
quantmod::getSymbols(symbolv, env=ratesenv, src="FRED")
# List files in ratesenv
ls(ratesenv)
# Get class of all objects in ratesenv
sapply(ratesenv, class)
# Get class of all objects in R workspace
sapply(ls(), function(name) class(get(name)))
# Save the time series environment into a binary .RData file
save(ratesenv, file="/Users/jerzy/Develop/lecture_slides/data/rates_data.RData")

# Get class of time series object DGS10
class(get(x="DGS10", envir=ratesenv))
# Another way
class(ratesenv$DGS10)
# Get first 6 rows of time series
head(ratesenv$DGS10)
# Plot dygraphs of 10-year Treasury rate
dygraphs::dygraph(ratesenv$DGS10, main="10-year Treasury Rate") %>%
  dyOptions(colors="blue", strokeWidth=2)
# Plot 10-year constant maturity Treasury rate
x11(width=6, height=5)
par(mar=c(2, 2, 0, 0), oma=c(0, 0, 0, 0))
chart_Series(ratesenv$DGS10["1990/"], name="10-year Treasury Rate")

# Load constant maturity Treasury rates
load(file="/Users/jerzy/Develop/lecture_slides/data/rates_data.RData")
# Get most recent yield curve
ycnow <- eapply(ratesenv, xts::last)
class(ycnow)
ycnow <- do.call(cbind, ycnow)
# Check if 2020-03-25 is not a holiday
date2020 <- as.Date("2020-03-25")
weekdays(date2020)
# Get yield curve from 2020-03-25
yc2020 <- eapply(ratesenv, function(x) x[date2020])
yc2020 <- do.call(cbind, yc2020)
# Combine the yield curves
ycurves <- c(yc2020, ycnow)
# Rename columns and rows, sort columns, and transpose into matrix
colnames(ycurves) <- substr(colnames(ycurves), start=4, stop=11)
ycurves <- ycurves[, order(as.numeric(colnames(ycurves)))]
colnames(ycurves) <- paste0(colnames(ycurves), "yr")
ycurves <- t(ycurves)
colnames(ycurves) <- substr(colnames(ycurves), start=1, stop=4)

x11(width=6, height=5)
par(mar=c(3, 3, 2, 0), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
# Plot using matplot()
colorv <- c("blue", "red")
matplot(ycurves, main="Yield Curves in 2020 and 2023", xaxt="n", lwd=3, lty=1,
  type="l", xlab="maturity", ylab="yield", col=colorv)
# Add x-axis
axis(1, seq_along(rownames(ycurves)), rownames(ycurves))
# Add legend
legend("topleft", legend=colnames(ycurves), y.intersp=0.1,
 bty="n", col=colorv, lty=1, lwd=6, inset=0.05, cex=1.0)

x11(width=6, height=5)
par(mar=c(3, 3, 2, 0), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
# Load constant maturity Treasury rates
load(file="/Users/jerzy/Develop/lecture_slides/data/rates_data.RData")
# Get end-of-year dates since 2006
datev <- xts::endpoints(ratesenv$DGS1["2006/"], on="years")
datev <- zoo::index(ratesenv$DGS1["2006/"][dates])
# Create time series of end-of-year rates
ycurves <- eapply(ratesenv, function(ratev) ratev[dates])
ycurves <- rutils::do_call(cbind, ycurves)
# Rename columns and rows, sort columns, and transpose into matrix
colnames(ycurves) <- substr(colnames(ycurves), start=4, stop=11)
ycurves <- ycurves[, order(as.numeric(colnames(ycurves)))]
colnames(ycurves) <- paste0(colnames(ycurves), "yr")
ycurves <- t(ycurves)
colnames(ycurves) <- substr(colnames(ycurves), start=1, stop=4)
# Plot matrix using plot.zoo()
colorv <- colorRampPalette(c("red", "blue"))(NCOL(ycurves))
plot.zoo(ycurves, main="Yield curve since 2006", lwd=3, xaxt="n",
   plot.type="single", xlab="maturity", ylab="yield", col=colorv)
# Add x-axis
axis(1, seq_along(rownames(ycurves)), rownames(ycurves))
# Add legend
legend("topleft", legend=colnames(ycurves), y.intersp=0.1,
 bty="n", col=colorv, lty=1, lwd=4, inset=0.05, cex=0.8)

# Alternative plot using matplot()
matplot(ycurves, main="Yield curve since 2006", xaxt="n", lwd=3, lty=1,
  type="l", xlab="maturity", ylab="yield", col=colorv)
# Add x-axis
axis(1, seq_along(rownames(ycurves)), rownames(ycurves))
# Add legend
legend("topleft", legend=colnames(ycurves), y.intersp=0.1,
 bty="n", col=colorv, lty=1, lwd=4, inset=0.05, cex=0.8)

# Extract rates from ratesenv
symbolv <- c("DGS1", "DGS2", "DGS5", "DGS10", "DGS20")
ratem <- mget(symbolv, envir=ratesenv)
ratem <- rutils::do_call(cbind, ratem)
ratem <- zoo::na.locf(ratem, na.rm=FALSE)
ratem <- zoo::na.locf(ratem, fromLast=TRUE)
# Calculate daily percentage rates changes
retp <- rutils::diffit(log(ratem))
# Center (de-mean) the returns
retp <- lapply(retp, function(x) {x - mean(x)})
retp <- rutils::do_call(cbind, retp)
sapply(retp, mean)
# Covariance and Correlation matrices of Treasury rates
covmat <- cov(retp)
cormat <- cor(retp)
# Reorder correlation matrix based on clusters
library(corrplot)
ordern <- corrMatOrder(cormat, order="hclust",
  hclust.method="complete")
cormat <- cormat[ordern, ordern]

# Plot the correlation matrix
x11(width=6, height=6)
colorv <- colorRampPalette(c("red", "white", "blue"))
corrplot(cormat, title=NA, tl.col="black",
    method="square", col=colorv(NCOL(cormat)), tl.cex=0.8,
    cl.offset=0.75, cl.cex=0.7, cl.align.text="l", cl.ratio=0.25)
title("Correlation of Treasury Rates", line=1)
# Draw rectangles on the correlation matrix plot
corrRect.hclust(cormat, k=NROW(cormat) %/% 2,
  method="complete", col="red")

# Create initial vector of portfolio weights
nweights <- NROW(symbolv)
weightv <- rep(1/sqrt(nweights), nweights)
names(weightv) <- symbolv
# Objective function equal to minus portfolio variance
objfun <- function(weightv, retp) {
  retp <- retp %*% weightv
  -1e7*var(retp) + 1e7*(1 - sum(weightv*weightv))^2
}  # end objfun
# Objective function for equal weight portfolio
objfun(weightv, retp)
# Compare speed of vector multiplication methods
library(microbenchmark)
summary(microbenchmark(
  transp=t(retp) %*% retp,
  sumv=sum(retp*retp),
  times=10))[, c(1, 4, 5)]

# Find weights with maximum variance
optiml <- optim(par=weightv,
  fn=objfun,
  retp=retp,
  method="L-BFGS-B",
  upper=rep(5.0, nweights),
  lower=rep(-5.0, nweights))
# Optimal weights and maximum variance
weights1 <- optiml$par
objfun(weights1, retp)
# Plot first principal component loadings
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
barplot(weights1, names.arg=names(weights1),
  xlab="", ylab="", main="First Principal Component Loadings")

# pc1 weights and returns
pc1 <- drop(retp %*% weights1)
# Redefine objective function
objfun <- function(weightv, retp) {
  retp <- retp %*% weightv
  -1e7*var(retp) + 1e7*(1 - sum(weightv^2))^2 +
    1e7*sum(weights1*weightv)^2
}  # end objfun
# Find second principal component weights
optiml <- optim(par=weightv,
             fn=objfun,
             retp=retp,
             method="L-BFGS-B",
             upper=rep(5.0, nweights),
             lower=rep(-5.0, nweights))

# pc2 weights and returns
weights2 <- optiml$par
pc2 <- drop(retp %*% weights2)
sum(pc1*pc2)
# Plot second principal component loadings
barplot(weights2, names.arg=names(weights2),
  xlab="", ylab="", main="Second Principal Component Loadings")

eigend <- eigen(covmat)
eigend$vectors
# Compare with optimization
all.equal(sum(diag(covmat)), sum(eigend$values))
all.equal(abs(eigend$vectors[, 1]), abs(weights1), check.attributes=FALSE)
all.equal(abs(eigend$vectors[, 2]), abs(weights2), check.attributes=FALSE)
all.equal(eigend$values[1], var(pc1), check.attributes=FALSE)
all.equal(eigend$values[2], var(pc2), check.attributes=FALSE)
# Eigenvalue equations are satisfied approximately
(covmat %*% weights1) / weights1 / var(pc1)
(covmat %*% weights2) / weights2 / var(pc2)
# Plot eigenvalues
barplot(eigend$values, names.arg=paste0("PC", 1:nweights),
  las=3, xlab="", ylab="", main="Principal Component Variances")

# Eigen decomposition of correlation matrix
eigend <- eigen(cormat)
# Perform PCA with scaling
pcad <- prcomp(retp, scale=TRUE)
# Compare outputs
all.equal(eigend$values, pcad$sdev^2)
all.equal(abs(eigend$vectors), abs(pcad$rotation),
    check.attributes=FALSE)
# Eigen decomposition of covariance matrix
eigend <- eigen(covmat)
# Perform PCA without scaling
pcad <- prcomp(retp, scale=FALSE)
# Compare outputs
all.equal(eigend$values, pcad$sdev^2)
all.equal(abs(eigend$vectors), abs(pcad$rotation),
    check.attributes=FALSE)

# Perform principal component analysis PCA
pcad <- prcomp(retp, scale=TRUE)
# Plot standard deviations
barplot(pcad$sdev, names.arg=colnames(pcad$rotation),
  las=3, xlab="", ylab="",
  main="Scree Plot: Volatilities of Principal Components
  of Treasury rates")

x11(width=6, height=7)
# Calculate principal component loadings (weights)
pcad$rotation
# Plot loading barplots in multiple panels
par(mfrow=c(3,2))
par(mar=c(3.5, 2, 2, 1), oma=c(0, 0, 0, 0))
for (ordern in 1:NCOL(pcad$rotation)) {
  barplot(pcad$rotation[, ordern], las=3, xlab="", ylab="", main="")
  title(paste0("PC", ordern), line=-2.0, col.main="red")
}  # end for

# Standardize (center and scale) the returns
retp <- lapply(retp, function(x) {(x - mean(x))/sd(x)})
retp <- rutils::do_call(cbind, retp)
sapply(retp, mean)
sapply(retp, sd)
# Calculate principal component time series
pcacum <- retp %*% pcad$rotation
all.equal(pcad$x, pcacum, check.attributes=FALSE)
# Calculate products of principal component time series
round(t(pcacum) %*% pcacum, 2)
# Coerce to xts time series
pcacum <- xts(pcacum, order.by=zoo::index(retp))
pcacum <- cumsum(pcacum)
# Plot principal component time series in multiple panels
par(mfrow=c(3,2))
par(mar=c(2, 2, 0, 1), oma=c(0, 0, 0, 0))
rangev <- range(pcacum)
for (ordern in 1:NCOL(pcacum)) {
  plot.zoo(pcacum[, ordern], ylim=rangev, xlab="", ylab="")
  title(paste0("PC", ordern), line=-1, col.main="red")
}  # end for

# Invert all the principal component time series
retpca <- retp %*% pcad$rotation
solved <- retpca %*% solve(pcad$rotation)
all.equal(coredata(retp), solved)

# Invert first 3 principal component time series
solved <- retpca[, 1:3] %*% solve(pcad$rotation)[1:3, ]
solved <- xts::xts(solved, zoo::index(retp))
solved <- cumsum(solved)
retc <- cumsum(retp)
# Plot the solved returns
par(mfrow=c(3,2))
par(mar=c(2, 2, 0, 1), oma=c(0, 0, 0, 0))
for (symbol in symbolv) {
  plot.zoo(cbind(retc[, symbol], solved[, symbol]),
    plot.type="single", col=c("black", "blue"), xlab="", ylab="")
  legend(x="topleft", bty="n", y.intersp=0.1,
   legend=paste0(symbol, c("", " solved")),
   title=NULL, inset=0.0, cex=1.0, lwd=6,
   lty=1, col=c("black", "blue"))
}  # end for

library(quantmod)  # Load quantmod
library(RQuantLib)  # Load RQuantLib
# Specify curve parameters
curve_params <- list(tradeDate=as.Date("2018-01-17"),
               settleDate=as.Date("2018-01-19"),
               dt=0.25,
               interpWhat="discount",
               interpHow="loglinear")
# Specify market data: prices of FI instruments
market_data <- list(d3m=0.0363,
              fut1=96.2875,
              fut2=96.7875,
              fut3=96.9875,
              fut4=96.6875,
              s5y=0.0443,
              s10y=0.05165,
              s15y=0.055175)
# Specify dates for calculating the zero rates
disc_dates <- seq(0, 10, 0.25)
# Specify the evaluation (as of) date
setEvaluationDate(as.Date("2018-01-17"))
# Calculate the zero rates
disc_curves <- DiscountCurve(params=curve_params,
                       tsQuotes=market_data,
                       times=disc_dates)
# Plot the zero rates
x11()
plot(x=disc_curves$zerorates, t="l", main="zerorates")

# Formula of linear model with zero intercept
formulav <- z ~ x + y - 1
formulav

# Collapse vector of strings into single text string
paste0("x", 1:5)
paste(paste0("x", 1:5), collapse="+")

# Create formula from text string
formulav <- as.formula(
  # Coerce text strings to formula
  paste("z ~ ",
  paste(paste0("x", 1:5), collapse="+")
  )  # end paste
)  # end as.formula
class(formulav)
formulav
# Modify the formula using "update"
update(formulav, log(.) ~ . + beta)

# Define explanatory (predm) variable
nrows <- 100
set.seed(1121)  # Initialize random number generator
predm <- runif(nrows)
noisev <- rnorm(nrows)
# Response equals linear form plus random noise
respv <- (-3 + 2*predm + noisev)

# Calculate centered (de-meaned) predictor and response vectors
predc <- predm - mean(predm)
respc <- respv - mean(respv)
# Calculate the regression beta
betav <- cov(predm, respv)/var(predm)
# Calculate the regression alpha
alpha <- mean(respv) - betav*mean(predm)

# Specify regression formula
formulav <- respv ~ predm
regmod <- lm(formulav)  # Perform regression
class(regmod)  # Regressions have class lm
attributes(regmod)
eval(regmod$call$formula)  # Regression formula
regmod$coeff  # Regression coefficients
all.equal(coef(regmod), c(alpha, betav),
      check.attributes=FALSE)

# x11(width=5, height=4)  # Open x11 for plotting
# Set plot parameters to reduce whitespace around plot
# par(mar=c(5, 5, 2, 1), oma=c(0, 0, 0, 0))
fitv <- (alpha + betav*predm)
all.equal(fitv, regmod$fitted.values, check.attributes=FALSE)
# Plot scatterplot using formula
plot(formulav, xlab="predictor", ylab="response")
title(main="Simple Regression", line=0.5)
# Add regression line
abline(regmod, lwd=3, col="blue")
# Plot fitted (forecast) response values
points(x=predm, y=regmod$fitted.values, pch=16, col="blue")

# Plot response without noise
lines(x=predm, y=(respv-noisev), col="red", lwd=3)
legend(x="topleft", # Add legend
       legend=c("response without noise", "fitted values"),
       title=NULL, inset=0.0, cex=1.0, y.intersp=0.3,
       bty="n", lwd=6, lty=1, col=c("red", "blue"))

# Calculate the residuals
fitv <- (alpha + betav*predm)
resids <- (respv - fitv)
all.equal(resids, regmod$residuals, check.attributes=FALSE)
# Residuals are orthogonal to the predictor
all.equal(sum(resids*predm), target=0)
# Residuals are orthogonal to the fitted values
all.equal(sum(resids*fitv), target=0)
# Sum of residuals is equal to zero
all.equal(mean(resids), target=0)

x11(width=6, height=5)  # Open x11 for plotting
# Set plot parameters to reduce whitespace around plot
par(mar=c(5, 5, 1, 1), oma=c(0, 0, 0, 0))
# Extract residuals
datav <- cbind(predm, regmod$residuals)
colnames(datav) <- c("predictor", "residuals")
# Plot residuals
plot(datav)
title(main="Residuals of the Linear Regression", line=-1)
abline(h=0, lwd=3, col="red")

# Degrees of freedom of residuals
degf <- regmod$df.residual
# Standard deviation of residuals
residsd <- sqrt(sum(resids^2)/degf)
# Standard error of beta
betasd <- residsd/sqrt(sum(predc^2))
# Standard error of alpha
alphasd <- residsd*sqrt(1/nrows + mean(predm)^2/sum(predc^2))

regsum <- summary(regmod)  # Copy regression summary
regsum  # Print the summary to console
attributes(regsum)$names  # get summary elements

regsum$coeff
# Standard errors
regsum$coefficients[2, "Std. Error"]
all.equal(c(alphasd, betasd), regsum$coefficients[, "Std. Error"], 
  check.attributes=FALSE)
# R-squared
regsum$r.squared
regsum$adj.r.squared
# F-statistic and ANOVA
regsum$fstatistic
anova(regmod)

set.seed(1121)  # initialize random number generator
# High noise compared to coefficient
respv <- (-3 + 2*predm + rnorm(nrows, sd=8))
regmod <- lm(formulav)  # Perform regression
# Values of regression coefficients are not
# Statistically significant
summary(regmod)

par(oma=c(1, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=1.0, cex.axis=1.0, cex.main=1.0, cex.sub=1.0)
regstats <- function(stdev) {  # Noisy regression
  set.seed(1121)  # initialize number generator
# Define explanatory (predm) and response variables
  predm <- rnorm(100, mean=2)
  respv <- (1 + 0.2*predm + rnorm(nrows, sd=stdev))
# Specify regression formula
  formulav <- respv ~ predm
# Perform regression and get summary
  regsum <- summary(lm(formulav))
# Extract regression statistics
  with(regsum, c(pval=coefficients[2, 4],
   adj_rsquared=adj.r.squared,
   fstat=fstatistic[1]))
}  # end regstats
# Apply regstats() to vector of stdev dev values
vecsd <- seq(from=0.1, to=0.5, by=0.1)
names(vecsd) <- paste0("sd=", vecsd)
statsmat <- t(sapply(vecsd, regstats))
# Plot in loop
par(mfrow=c(NCOL(statsmat), 1))
for (it in 1:NCOL(statsmat)) {
  plot(statsmat[, it], type="l",
 xaxt="n", xlab="", ylab="", main="")
  title(main=colnames(statsmat)[it], line=-1.0)
  axis(1, at=1:(NROW(statsmat)), labels=rownames(statsmat))
}  # end for

regstats <- function(datav) {  # get regression
# Perform regression and get summary
  colnamev <- colnames(datav)
  formulav <- paste(colnamev[2], colnamev[1], sep="~")
  regsum <- summary(lm(formulav, data=datav))
# Extract regression statistics
  with(regsum, c(pval=coefficients[2, 4],
   adj_rsquared=adj.r.squared,
   fstat=fstatistic[1]))
}  # end regstats
# Apply regstats() to vector of stdev dev values
vecsd <- seq(from=0.1, to=0.5, by=0.1)
names(vecsd) <- paste0("sd=", vecsd)
statsmat <- t(sapply(vecsd, function(stdev) {
    set.seed(1121)  # initialize number generator
# Define explanatory (predm) and response variables
    predm <- rnorm(100, mean=2)
    respv <- (1 + 0.2*predm + rnorm(nrows, sd=stdev))
    regstats(data.frame(predm, respv))
    }))
# Plot in loop
par(mfrow=c(NCOL(statsmat), 1))
for (it in 1:NCOL(statsmat)) {
  plot(statsmat[, it], type="l",
 xaxt="n", xlab="", ylab="", main="")
  title(main=colnames(statsmat)[it], line=-1.0)
  axis(1, at=1:(NROW(statsmat)),
 labels=rownames(statsmat))
}  # end for

# Set plot paramaters - margins and font scale
par(oma=c(1,0,1,0), mgp=c(2,1,0), mar=c(2,1,2,1), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2, 2))  # Plot 2x2 panels
plot(regmod)  # Plot diagnostic scatterplots
plot(regmod, which=2)  # Plot just Q-Q

library(lmtest)  # Load lmtest
# Perform Durbin-Watson test
lmtest::dwtest(regmod)

# Define linear regression data
set.seed(1121)  # Initialize random number generator
nrows <- 100
predm <- runif(nrows)
noisev <- rnorm(nrows)
respv <- (-3 + 2*predm + noisev)

x11(width=6, height=5)  # Open x11 for plotting
# Set plot parameters to reduce whitespace around plot
par(mar=c(5, 5, 2, 1), oma=c(0, 0, 0, 0))
# Add unit column to the predictor matrix
predm <- cbind(rep(1, nrows), predm)
# Calculate generalized inverse of the predictor matrix
predinv <- MASS::ginv(predm)
# Calculate the influence matrix
infmat <- predm %*% predinv
# Plot the leverage vector
ordern <- order(predm[, 2])
plot(x=predm[ordern, 2], y=diag(infmat)[ordern],
     type="l", lwd=3, col="blue",
     xlab="predictor", ylab="leverage",
     main="Leverage as Function of Predictor")

# Calculate the influence matrix
infmat <- predm %*% predinv
# The influence matrix is idempotent
all.equal(infmat, infmat %*% infmat)

# Calculate covariance and standard deviations of fitted values
betav <- predinv %*% respv
fitv <- drop(predm %*% betav)
resids <- drop(respv - fitv)
degf <- (NROW(predm) - NCOL(predm))
residsd <- sqrt(sum(resids^2)/degf)
fitcovar <- residsd*infmat
fitsd <- sqrt(diag(fitcovar))
# Plot the standard deviations
fitdata <- cbind(fitted=fitv, stdev=fitsd)
fitdata <- fitdata[order(fitv), ]
plot(fitdata, type="l", lwd=3, col="blue",
     xlab="Fitted Value", ylab="Standard Deviation",
     main="Standard Deviations of Fitted Values\nin Univariate Regression")

# Calculate response without random noise for univariate regression,
# equal to weighted sum over columns of predictor.
respn <- predm %*% c(-1, 1)
# Perform loop over different realizations of random noise
fitm <- lapply(1:50, function(it) {
  # Add random noise to response
  respv <- respn + rnorm(nrows, sd=1.0)
  # Calculate fitted values using influence matrix
  infmat %*% respv
})  # end lapply
fitm <- rutils::do_call(cbind, fitm)

x11(width=5, height=4)  # Open x11 for plotting
# Set plot parameters to reduce whitespace around plot
par(mar=c(5, 5, 2, 1), oma=c(0, 0, 0, 0))
# Plot fitted values
matplot(x=predm[, 2], y=fitm,
type="l", lty="solid", lwd=1, col="blue",
xlab="predictor", ylab="fitted",
main="Fitted Values for Different Realizations
of Random Noise")
lines(x=predm[, 2], y=respn, col="red", lwd=4)
legend(x="topleft", # Add legend
       legend=c("response without noise", "fitted values"),
       title=NULL, inset=0.05, cex=1.0, lwd=6, y.intersp=0.4,
       bty="n", lty=1, col=c("red", "blue"))

# Define new predictor
newdata <- (max(predm[, 2]) + 10*(1:5)/nrows)
predn <- cbind(rep(1, NROW(newdata)), newdata)
# Calculate the forecast values and standard errors
predm2 <- MASS::ginv(crossprod(predm)) # Inverse of predictor matrix squared
predsd <- residsd*sqrt(predn %*% predm2 %*% t(predn))
fcast <- cbind(forecast=drop(predn %*% betav),
  stdev=diag(predsd))

# Prepare plot data
xdata <- c(predm[, 2], newdata)
ydata <- c(fitv, fcast[, 1])
# Calculate t-quantile
tquant <- qt(pnorm(2), df=degf)
fcastl <- fcast[, 1] - tquant*fcast[, 2]
fcasth <- fcast[, 1] + tquant*fcast[, 2]
# Plot the regression forecasts
xlim <- range(xdata)
ylim <- range(c(respv, ydata, fcastl, fcasth))
plot(x=xdata, y=ydata, xlim=xlim, ylim=ylim,
     type="l", lwd=3, col="blue",
     xlab="predictor", ylab="forecast",
     main="Forecasts from Linear Regression")
points(x=predm[, 2], y=respv, col="blue")
points(x=newdata, y=fcast[, 1], pch=16, col="blue")
lines(x=newdata, y=fcasth, lwd=3, col="red")
lines(x=newdata, y=fcastl, lwd=3, col="green")
legend(x="topleft", # Add legend
       legend=c("forecasts", "+2SD", "-2SD"),
       title=NULL, inset=0.05, cex=1.0, lwd=6, y.intersp=0.4,
       bty="n", lty=1, col=c("blue", "red", "green"))

# Perform univariate regression
dframe <- data.frame(resp=respv, pred=predm[, 2])
regmod <- lm(resp ~ pred, data=dframe)
# Calculate forecasts from regression
newdf <- data.frame(pred=predn[, 2]) # Same column name
fcastlm <- predict.lm(object=regmod,
  newdata=newdf, confl=1-2*(1-pnorm(2)),
  interval="confidence")
rownames(fcastlm) <- NULL
all.equal(fcastlm[, "fit"], fcast[, 1])
all.equal(fcastlm[, "lwr"], fcastl)
all.equal(fcastlm[, "upr"], fcasth)
plot(x=xdata, y=ydata, xlim=xlim, ylim=ylim,
     type="l", lwd=3, col="blue",
     xlab="predictor", ylab="forecast",
     main="Forecasts from lm() Regression")
points(x=predm[, 2], y=respv, col="blue")

abline(regmod, col="blue", lwd=3)
points(x=newdata, y=fcastlm[, "fit"], pch=16, col="blue")
lines(x=newdata, y=fcastlm[, "lwr"], lwd=3, col="green")
lines(x=newdata, y=fcastlm[, "upr"], lwd=3, col="red")
legend(x="topleft", # Add legend
       legend=c("forecasts", "+2SD", "-2SD"),
       title=NULL, inset=0.05, cex=0.8, lwd=6, y.intersp=0.4,
       bty="n", lty=1, col=c("blue", "red", "green"))

set.seed(1121)
library(lmtest)
# Spurious regression in unit root time series
predm <- cumsum(rnorm(100))  # Unit root time series
respv <- cumsum(rnorm(100))
formulav <- respv ~ predm
regmod <- lm(formulav)  # Perform regression
# Summary indicates statistically significant regression
regsum <- summary(regmod)
regsum$coeff
regsum$r.squared
# Durbin-Watson test shows residuals are autocorrelated
dwtest <- lmtest::dwtest(regmod)
c(dwtest$statistic[[1]], dwtest$p.value)

par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # Set plot panels
plot(formulav, xlab="", ylab="")  # Plot scatterplot using formula
title(main="Spurious Regression", line=-1)
# Add regression line
abline(regmod, lwd=2, col="red")
plot(regmod, which=2, ask=FALSE)  # Plot just Q-Q
