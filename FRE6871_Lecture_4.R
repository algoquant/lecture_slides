# Create random real symmetric matrix
matrixv <- matrix(runif(25), nc=5)
matrixv <- matrixv + t(matrixv)
# Calculate eigenvectors and eigenvalues
eigend <- eigen(matrixv)
eigenvec <- eigend$vectors
dim(eigenvec)
# Plot eigenvalues
barplot(eigend$values, xlab="", ylab="", las=3,
  names.arg=paste0("ev", 1:NROW(eigend$values)),
  main="Eigenvalues of a real symmetric matrix")

# eigenvectors form an orthonormal basis
round(t(eigenvec) %*% eigenvec, digits=4)
# Diagonalize matrix using eigenvector matrix
round(t(eigenvec) %*% (matrixv %*% eigenvec), digits=4)
eigend$values
# eigen decomposition of matrix by rotating the diagonal matrix
matrixe <- eigenvec %*% (eigend$values * t(eigenvec))
# Create diagonal matrix of eigenvalues
# diagmat <- diag(eigend$values)
# matrixe <- eigenvec %*% (diagmat %*% t(eigenvec))
all.equal(matrixv, matrixe)

# Create random positive semi-definite matrix
matrixv <- matrix(runif(25), nc=5)
matrixv <- t(matrixv) %*% matrixv
# Calculate eigenvectors and eigenvalues
eigend <- eigen(matrixv)
eigend$values
# Plot eigenvalues
barplot(eigend$values, las=3, xlab="", ylab="",
  names.arg=paste0("ev", 1:NROW(eigend$values)),
  main="Eigenvalues of positive semi-definite matrix")

# Perform singular value decomposition
matrixv <- matrix(rnorm(50), nc=5)
svdec <- svd(matrixv)
# Recompose matrixv from SVD mat_rices
all.equal(matrixv, svdec$u %*% (svdec$d*t(svdec$v)))
# Columns of U and V are orthonormal
round(t(svdec$u) %*% svdec$u, 4)
round(t(svdec$v) %*% svdec$v, 4)

# Dimensions of left and right matrices
nleft <- 6 ; nright <- 4
# Calculate left matrix
leftmat <- matrix(runif(nleft^2), nc=nleft)
eigend <- eigen(crossprod(leftmat))
leftmat <- eigend$vectors[, 1:nright]
# Calculate right matrix and singular values
rightmat <- matrix(runif(nright^2), nc=nright)
eigend <- eigen(crossprod(rightmat))
rightmat <- eigend$vectors
sing_values <- sort(runif(nright, min=1, max=5), decreasing=TRUE)
# Compose rectangular matrix
matrixv <- leftmat %*% (sing_values * t(rightmat))
# Perform singular value decomposition
svdec <- svd(matrixv)
# Recompose matrixv from SVD
all.equal(matrixv, svdec$u %*% (svdec$d*t(svdec$v)))
# Compare SVD with matrixv components
all.equal(abs(svdec$u), abs(leftmat))
all.equal(abs(svdec$v), abs(rightmat))
all.equal(svdec$d, sing_values)
# Eigen decomposition of matrixv squared
retsq <- matrixv %*% t(matrixv)
eigend <- eigen(retsq)
all.equal(eigend$values[1:nright], sing_values^2)
all.equal(abs(eigend$vectors[, 1:nright]), abs(leftmat))
# Eigen decomposition of matrixv squared
retsq <- t(matrixv) %*% matrixv
eigend <- eigen(retsq)
all.equal(eigend$values, sing_values^2)
all.equal(abs(eigend$vectors), abs(rightmat))

# Create random positive semi-definite matrix
matrixv <- matrix(runif(25), nc=5)
matrixv <- t(matrixv) %*% matrixv
# Calculate the inverse of matrixv
invmat <- solve(a=matrixv)
# Multiply inverse with matrix
round(invmat %*% matrixv, 4)
round(matrixv %*% invmat, 4)

# Calculate eigenvectors and eigenvalues
eigend <- eigen(matrixv)
eigenvec <- eigend$vectors

# Perform eigen decomposition of inverse
inveigen <- eigenvec %*% (t(eigenvec) / eigend$values)
all.equal(invmat, inveigen)
# Decompose diagonal matrix with inverse of eigenvalues
# diagmat <- diag(1/eigend$values)
# inveigen <-
#   eigenvec %*% (diagmat %*% t(eigenvec))

# Random rectangular matrix: nleft > nright
nleft <- 6 ; nright <- 4
matrixv <- matrix(runif(nleft*nright), nc=nright)
# Calculate generalized inverse of matrixv
invmat <- MASS::ginv(matrixv)
round(invmat %*% matrixv, 4)
all.equal(matrixv, matrixv %*% invmat %*% matrixv)
# Random rectangular matrix: nleft < nright
nleft <- 4 ; nright <- 6
matrixv <- matrix(runif(nleft*nright), nc=nright)
# Calculate generalized inverse of matrixv
invmat <- MASS::ginv(matrixv)
all.equal(matrixv, matrixv %*% invmat %*% matrixv)
round(matrixv %*% invmat, 4)
round(invmat %*% matrixv, 4)
# Perform singular value decomposition
svdec <- svd(matrixv)
# Calculate generalized inverse from SVD
invsvd <- svdec$v %*% (t(svdec$u) / svdec$d)
all.equal(invsvd, invmat)
# Calculate Moore-Penrose pseudo-inverse
invmp <- MASS::ginv(t(matrixv) %*% matrixv) %*% t(matrixv)
all.equal(invmp, invmat)

# Create random singular matrix
# More columns than rows: nright > nleft
nleft <- 4 ; nright <- 6
matrixv <- matrix(runif(nleft*nright), nc=nright)
matrixv <- t(matrixv) %*% matrixv
# Perform singular value decomposition
svdec <- svd(matrixv)
# Incorrect inverse from SVD because of zero singular values
invsvd <- svdec$v %*% (t(svdec$u) / svdec$d)
# Inverse property doesn't hold
all.equal(matrixv, matrixv %*% invsvd %*% matrixv)

# Set tolerance for determining zero singular values
precv <- sqrt(.Machine$double.eps)
# Check for zero singular values
round(svdec$d, 12)
notzero <- (svdec$d > (precv * svdec$d[1]))
# Calculate regularized inverse from SVD
invsvd <- svdec$v[, notzero] %*%
  (t(svdec$u[, notzero]) / svdec$d[notzero])
# Verify inverse property of matrixv
all.equal(matrixv, matrixv %*% invsvd %*% matrixv)
# Calculate regularized inverse using MASS::ginv()
invmat <- MASS::ginv(matrixv)
all.equal(invsvd, invmat)
# Calculate Moore-Penrose pseudo-inverse
invmp <- MASS::ginv(t(matrixv) %*% matrixv) %*% t(matrixv)
all.equal(invmp, invmat)

# Diagonalize the unit matrix
unitmat <- matrixv %*% invmat
round(unitmat, 4)
round(matrixv %*% invmat, 4)
round(t(svdec$u) %*% unitmat %*% svdec$v, 4)

# Define a square matrix
matrixv <- matrix(c(1, 2, -1, 2), nc=2)
vectorv <- c(2, 1)
# Calculate the inverse of matrixv
invmat <- solve(a=matrixv)
invmat %*% matrixv
# Calculate solution using inverse of matrixv
solutionv <- invmat %*% vectorv
matrixv %*% solutionv
# Calculate solution of linear system
solutionv <- solve(a=matrixv, b=vectorv)
matrixv %*% solutionv

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
rastrigin <- function(vectorv, param=25) {
  sum(vectorv^2 - param*cos(vectorv))
}  # end rastrigin
vectorv <- c(pi, pi/4)
rastrigin(vectorv=vectorv)
# Draw 3d surface plot of Rastrigin function
rgl::persp3d(
  x=Vectorize(function(x, y) rastrigin(vectorv=c(x, y))),
  xlim=c(-10, 10), ylim=c(-10, 10),
  col="green", axes=FALSE, zlab="", main="rastrigin")
rgl::rglwidget(elementId="persp3d", width=400, height=400)
# Optimize with respect to vector argument
optiml <- optim(par=vectorv, fn=rastrigin,
        method="L-BFGS-B",
        upper=c(14*pi, 14*pi),
        lower=c(pi/2, pi/2),
        param=1)
# Optimal parameters and value
optiml$par
optiml$value
rastrigin(optiml$par, param=1)

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
rates <- c(yc2020, ycnow)
# Rename columns and rows, sort columns, and transpose into matrix
colnames(rates) <- substr(colnames(rates), start=4, stop=11)
rates <- rates[, order(as.numeric(colnames(rates)))]
colnames(rates) <- paste0(colnames(rates), "yr")
rates <- t(rates)
colnames(rates) <- substr(colnames(rates), start=1, stop=4)

x11(width=6, height=5)
par(mar=c(3, 3, 2, 0), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
# Plot using matplot()
colorv <- c("blue", "red")
matplot(rates, main="Yield Curves in 2020 and 2021", xaxt="n", lwd=3, lty=1,
  type="l", xlab="maturity", ylab="yield", col=colorv)
# Add x-axis
axis(1, seq_along(rownames(rates)), rownames(rates))
# Add legend
legend("topleft", legend=colnames(rates), y.intersp=c(0.25, 0.25),
       bty="n", col=colorv, lty=1, lwd=6, inset=0.05, cex=1.0)

x11(width=6, height=5)
par(mar=c(3, 3, 2, 0), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
# Load constant maturity Treasury rates
load(file="/Users/jerzy/Develop/lecture_slides/data/rates_data.RData")
# Get end-of-year dates since 2006
dates <- xts::endpoints(ratesenv$DGS1["2006/"], on="years")
dates <- zoo::index(ratesenv$DGS1["2006/"][dates])
# Create time series of end-of-year rates
rates <- eapply(ratesenv, function(ratev) ratev[dates])
rates <- rutils::do_call(cbind, rates)
# Rename columns and rows, sort columns, and transpose into matrix
colnames(rates) <- substr(colnames(rates), start=4, stop=11)
rates <- rates[, order(as.numeric(colnames(rates)))]
colnames(rates) <- paste0(colnames(rates), "yr")
rates <- t(rates)
colnames(rates) <- substr(colnames(rates), start=1, stop=4)
# Plot matrix using plot.zoo()
colorv <- colorRampPalette(c("red", "blue"))(NCOL(rates))
plot.zoo(rates, main="Yield curve since 2006", lwd=3, xaxt="n",
   plot.type="single", xlab="maturity", ylab="yield", col=colorv)
# Add x-axis
axis(1, seq_along(rownames(rates)), rownames(rates))
# Add legend
legend("topleft", legend=colnames(rates), y.intersp=rep(0.2, NROW(rates)),
       bty="n", col=colorv, lty=1, lwd=6, inset=0.01, cex=1.0)

# Alternative plot using matplot()
matplot(rates, main="Yield curve since 2006", xaxt="n", lwd=3, lty=1,
  type="l", xlab="maturity", ylab="yield", col=colorv)
# Add x-axis
axis(1, seq_along(rownames(rates)), rownames(rates))
# Add legend
legend("topleft", legend=colnames(rates), y.intersp=rep(0.2, NROW(rates)),
       bty="n", col=colorv, lty=1, lwd=6, inset=0.01, cex=1.0)

# Extract rates from ratesenv
symbolv <- c("DGS1", "DGS2", "DGS5", "DGS10", "DGS20")
rates <- mget(symbolv, envir=ratesenv)
rates <- rutils::do_call(cbind, rates)
rates <- zoo::na.locf(rates, na.rm=FALSE)
rates <- zoo::na.locf(rates, fromLast=TRUE)
# Calculate daily percentage rates changes
retsp <- rutils::diffit(log(rates))
# De-mean the returns
retsp <- lapply(retsp, function(x) {x - mean(x)})
retsp <- rutils::do_call(cbind, retsp)
sapply(retsp, mean)
# Covariance and Correlation matrices of Treasury rates
covmat <- cov(retsp)
cormat <- cor(retsp)
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
objfun <- function(weightv, retsp) {
  retsp <- retsp %*% weightv
  -1e7*var(retsp) + 1e7*(1 - sum(weightv*weightv))^2
}  # end objfun
# Objective function for equal weight portfolio
objfun(weightv, retsp)
# Compare speed of vector multiplication methods
library(microbenchmark)
summary(microbenchmark(
  transp=t(retsp) %*% retsp,
  sumv=sum(retsp*retsp),
  times=10))[, c(1, 4, 5)]

# Find weights with maximum variance
optiml <- optim(par=weightv,
  fn=objfun,
  retsp=retsp,
  method="L-BFGS-B",
  upper=rep(5.0, nweights),
  lower=rep(-5.0, nweights))
# Optimal weights and maximum variance
weights1 <- optiml$par
objfun(weights1, retsp)
# Plot first principal component loadings
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
barplot(weights1, names.arg=names(weights1),
  xlab="", ylab="", main="First Principal Component Loadings")

# pc1 weights and returns
pc1 <- drop(retsp %*% weights1)
# Redefine objective function
objfun <- function(weightv, retsp) {
  retsp <- retsp %*% weightv
  -1e7*var(retsp) + 1e7*(1 - sum(weightv^2))^2 +
    1e7*sum(weights1*weightv)^2
}  # end objfun
# Find second principal component weights
optiml <- optim(par=weightv,
             fn=objfun,
             retsp=retsp,
             method="L-BFGS-B",
             upper=rep(50.0, nweights),
             lower=rep(-50.0, nweights))

# pc2 weights and returns
weights2 <- optiml$par
pc2 <- drop(retsp %*% weights2)
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
pcad <- prcomp(retsp, scale=TRUE)
# Compare outputs
all.equal(eigend$values, pcad$sdev^2)
all.equal(abs(eigend$vectors), abs(pcad$rotation),
    check.attributes=FALSE)
# Eigen decomposition of covariance matrix
eigend <- eigen(covmat)
# Perform PCA without scaling
pcad <- prcomp(retsp, scale=FALSE)
# Compare outputs
all.equal(eigend$values, pcad$sdev^2)
all.equal(abs(eigend$vectors), abs(pcad$rotation),
    check.attributes=FALSE)

# Perform principal component analysis PCA
pcad <- prcomp(retsp, scale=TRUE)
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

# Standardize (de-mean and scale) the returns
retsp <- lapply(retsp, function(x) {(x - mean(x))/sd(x)})
retsp <- rutils::do_call(cbind, retsp)
sapply(retsp, mean)
sapply(retsp, sd)
# Calculate principal component time series
pcacum <- retsp %*% pcad$rotation
all.equal(pcad$x, pcacum, check.attributes=FALSE)
# Calculate products of principal component time series
round(t(pcacum) %*% pcacum, 2)
# Coerce to xts time series
pcacum <- xts(pcacum, order.by=zoo::index(retsp))
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
retspca <- retsp %*% pcad$rotation
solved <- retspca %*% solve(pcad$rotation)
all.equal(coredata(retsp), solved)

# Invert first 3 principal component time series
solved <- retspca[, 1:3] %*% solve(pcad$rotation)[1:3, ]
solved <- xts::xts(solved, zoo::index(retsp))
solved <- cumsum(solved)
retc <- cumsum(retsp)
# Plot the solved returns
par(mfrow=c(3,2))
par(mar=c(2, 2, 0, 1), oma=c(0, 0, 0, 0))
for (symbol in symbolv) {
  plot.zoo(cbind(retc[, symbol], solved[, symbol]),
    plot.type="single", col=c("black", "blue"), xlab="", ylab="")
  legend(x="topleft", bty="n",
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

# Define explanatory (predictor) variable
nrows <- 100
set.seed(1121)  # Initialize random number generator
predictor <- runif(nrows)
noise <- rnorm(nrows)
# Response equals linear form plus random noise
response <- (-3 + 2*predictor + noise)

# Calculate de-meaned explanatory (predictor) and response vectors
predictor_zm <- predictor - mean(predictor)
response_zm <- response - mean(response)
# Calculate the regression beta
betav <- cov(predictor, response)/var(predictor)
# Calculate the regression alpha
alpha <- mean(response) - betav*mean(predictor)

# Specify regression formula
formulav <- response ~ predictor
model <- lm(formulav)  # Perform regression
class(model)  # Regressions have class lm
attributes(model)
eval(model$call$formula)  # Regression formula
model$coeff  # Regression coefficients
all.equal(coef(model), c(alpha, betav),
  check.attributes=FALSE)

fittedv <- (alpha + betav*predictor)
all.equal(fittedv, model$fitted.values, check.attributes=FALSE)
x11(width=5, height=4)  # Open x11 for plotting
# Set plot parameters to reduce whitespace around plot
par(mar=c(5, 5, 2, 1), oma=c(0, 0, 0, 0))
# Plot scatterplot using formula
plot(formulav, xlab="predictor", ylab="response")
title(main="Simple Regression", line=0.5)
# Add regression line
abline(model, lwd=3, col="blue")
# Plot fitted (predicted) response values
points(x=predictor, y=model$fitted.values, pch=16, col="blue")

# Plot response without noise
lines(x=predictor, y=(response-noise), col="red", lwd=3)
legend(x="topleft", # Add legend
       legend=c("response without noise", "fitted values"),
       title=NULL, inset=0.08, cex=0.8, lwd=6,
       lty=1, col=c("red", "blue"))

# Calculate the residuals
fittedv <- (alpha + betav*predictor)
residuals <- (response - fittedv)
all.equal(residuals, model$residuals, check.attributes=FALSE)
# Residuals are orthogonal to the predictor
all.equal(sum(residuals*predictor), target=0)
# Residuals are orthogonal to the fitted values
all.equal(sum(residuals*fittedv), target=0)
# Sum of residuals is equal to zero
all.equal(mean(residuals), target=0)

x11(width=6, height=5)  # Open x11 for plotting
# Set plot parameters to reduce whitespace around plot
par(mar=c(5, 5, 1, 1), oma=c(0, 0, 0, 0))
# Extract residuals
datav <- cbind(predictor, model$residuals)
colnames(datav) <- c("predictor", "residuals")
# Plot residuals
plot(datav)
title(main="Residuals of the Linear Regression", line=-1)
abline(h=0, lwd=3, col="red")

# Degrees of freedom of residuals
degf <- model$df.residual
# Standard deviation of residuals
residsd <- sqrt(sum(residuals^2)/degf)
# Standard error of beta
betasd <- residsd/sqrt(sum(predictor_zm^2))
# Standard error of alpha
alphasd <- residsd*
  sqrt(1/nrows + mean(predictor)^2/sum(predictor_zm^2))

modelsum <- summary(model)  # Copy regression summary
modelsum  # Print the summary to console
attributes(modelsum)$names  # get summary elements

modelsum$coeff
# Standard errors
modelsum$coefficients[2, "Std. Error"]
all.equal(c(alphasd, betasd),
  modelsum$coefficients[, "Std. Error"], 
  check.attributes=FALSE)
# R-squared
modelsum$r.squared
modelsum$adj.r.squared
# F-statistic and ANOVA
modelsum$fstatistic
anova(model)

set.seed(1121)  # initialize random number generator
# High noise compared to coefficient
response <- (-3 + 2*predictor + rnorm(nrows, sd=8))
model <- lm(formulav)  # Perform regression
# Values of regression coefficients are not
# Statistically significant
summary(model)

par(oma=c(1, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=1.0, cex.axis=1.0, cex.main=1.0, cex.sub=1.0)
reg_stats <- function(stdev) {  # Noisy regression
  set.seed(1121)  # initialize number generator
# Define explanatory (predictor) and response variables
  predictor <- rnorm(100, mean=2)
  response <- (1 + 0.2*predictor +
  rnorm(NROW(predictor), sd=stdev))
# Specify regression formula
  formulav <- response ~ predictor
# Perform regression and get summary
  modelsum <- summary(lm(formulav))
# Extract regression statistics
  with(modelsum, c(pval=coefficients[2, 4],
   adj_rsquared=adj.r.squared,
   fstat=fstatistic[1]))
}  # end reg_stats
# Apply reg_stats() to vector of std dev values
vecsd <- seq(from=0.1, to=0.5, by=0.1)
names(vecsd) <- paste0("sd=", vecsd)
statsmat <- t(sapply(vecsd, reg_stats))
# Plot in loop
par(mfrow=c(NCOL(statsmat), 1))
for (it in 1:NCOL(statsmat)) {
  plot(statsmat[, it], type="l",
 xaxt="n", xlab="", ylab="", main="")
  title(main=colnames(statsmat)[it], line=-1.0)
  axis(1, at=1:(NROW(statsmat)), labels=rownames(statsmat))
}  # end for

reg_stats <- function(datav) {  # get regression
# Perform regression and get summary
  colnamev <- colnames(datav)
  formulav <- paste(colnamev[2], colnamev[1], sep="~")
  modelsum <- summary(lm(formulav, data=datav))
# Extract regression statistics
  with(modelsum, c(pval=coefficients[2, 4],
   adj_rsquared=adj.r.squared,
   fstat=fstatistic[1]))
}  # end reg_stats
# Apply reg_stats() to vector of std dev values
vecsd <- seq(from=0.1, to=0.5, by=0.1)
names(vecsd) <- paste0("sd=", vecsd)
statsmat <- t(sapply(vecsd, function(stdev) {
    set.seed(1121)  # initialize number generator
# Define explanatory (predictor) and response variables
    predictor <- rnorm(100, mean=2)
    response <- (1 + 0.2*predictor +
rnorm(NROW(predictor), sd=stdev))
    reg_stats(data.frame(predictor, response))
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
plot(model)  # Plot diagnostic scatterplots
plot(model, which=2)  # Plot just Q-Q

library(lmtest)  # Load lmtest
# Perform Durbin-Watson test
lmtest::dwtest(model)

x11(width=6, height=5)  # Open x11 for plotting
# Set plot parameters to reduce whitespace around plot
par(mar=c(5, 5, 2, 1), oma=c(0, 0, 0, 0))
# Add unit column to the predictor matrix
predictor <- cbind(rep(1, nrows), predictor)
# Calculate generalized inverse of the predictor matrix
invpred <- MASS::ginv(predictor)
# Calculate the influence matrix
influencem <- predictor %*% invpred
# Plot the leverage vector
ordern <- order(predictor[, 2])
plot(x=predictor[ordern, 2], y=diag(influencem)[ordern],
     type="l", lwd=3, col="blue",
     xlab="predictor", ylab="leverage",
     main="Leverage as Function of Predictor")

# Calculate the influence matrix
influencem <- predictor %*% invpred
# The influence matrix is idempotent
all.equal(influencem, influencem %*% influencem)

# Calculate covariance and standard deviations of fitted values
betas <- invpred %*% response
fittedv <- drop(predictor %*% betas)
residuals <- drop(response - fittedv)
degf <- (NROW(predictor) - NCOL(predictor))
residvar <- sqrt(sum(residuals^2)/degf)
fitcovar <- residvar*influencem
fitsd <- sqrt(diag(fitcovar))
# Plot the standard deviations
fitsd <- cbind(fitted=fittedv, stddev=fitsd)
fitsd <- fitsd[order(fittedv), ]
plot(fitsd, type="l", lwd=3, col="blue",
     xlab="Fitted Value", ylab="Standard Deviation",
     main="Standard Deviations of Fitted Values\nin Univariate Regression")

# Calculate response without random noise for univariate regression,
# equal to weighted sum over columns of predictor.
betas <- c(-1, 1)
response <- predictor %*% betas
# Perform loop over different realizations of random noise
fittedv <- lapply(1:50, function(it) {
  # Add random noise to response
  response <- response + rnorm(nrows, sd=1.0)
  # Calculate fitted values using influence matrix
  influencem %*% response
})  # end lapply
fittedv <- rutils::do_call(cbind, fittedv)

x11(width=5, height=4)  # Open x11 for plotting
# Set plot parameters to reduce whitespace around plot
par(mar=c(5, 5, 2, 1), oma=c(0, 0, 0, 0))
# Plot fitted values
matplot(x=predictor[,2], y=fittedv,
type="l", lty="solid", lwd=1, col="blue",
xlab="predictor", ylab="fitted",
main="Fitted Values for Different Realizations
of Random Noise")
lines(x=predictor[,2], y=response, col="red", lwd=4)
legend(x="topleft", # Add legend
       legend=c("response without noise", "fitted values"),
       title=NULL, inset=0.05, cex=0.8, lwd=6,
       lty=1, col=c("red", "blue"))

# Inverse of predictor matrix squared
predictor2 <- MASS::ginv(crossprod(predictor))
# Define new predictors
newdata <- (max(predictor[, 2]) + 10*(1:5)/nrows)
# Calculate the predicted values and standard errors
predictorn <- cbind(rep(1, NROW(newdata)), newdata)
predsd <- sqrt(predictorn %*% predictor2 %*% t(predictorn))
predictv <- cbind(
  prediction=drop(predictorn %*% betas),
  stddev=diag(residvar*predsd))
# Or: Perform loop over predictorn
predictv <- apply(predictorn, MARGIN=1, function(predictor) {
  # Calculate predicted values
  prediction <- predictor %*% betas
  # Calculate standard deviation
  predsd <- sqrt(t(predictor) %*% predictor2 %*% predictor)
  predictsd <- residvar*predsd
  c(prediction=prediction, stddev=predictsd)
})  # end sapply
predictv <- t(predictv)

# Prepare plot data
xdata <- c(predictor[,2], newdata)
xlim <- range(xdata)
ydata <- c(fittedv, predictv[, 1])
# Calculate t-quantile
tquant <- qt(pnorm(2), df=degf)
predictlow <- predictv[, 1]-tquant*predictv[, 2]
predicthigh <- predictv[, 1]+tquant*predictv[, 2]
ylim <- range(c(response, ydata, predictlow, predicthigh))
# Plot the regression predictions
plot(x=xdata, y=ydata, xlim=xlim, ylim=ylim,
     type="l", lwd=3, col="blue",
     xlab="predictor", ylab="fitted or predicted",
     main="Predictions from Linear Regression")
points(x=predictor[,2], y=response, col="blue")
points(x=newdata, y=predictv[, 1], pch=16, col="blue")
lines(x=newdata, y=predicthigh, lwd=3, col="red")
lines(x=newdata, y=predictlow, lwd=3, col="green")
legend(x="topleft", # Add legend
       legend=c("predictions", "+2SD", "-2SD"),
       title=NULL, inset=0.05, cex=0.8, lwd=6,
       lty=1, col=c("blue", "red", "green"))

# Perform univariate regression
predictor <- predictor[, 2]
model <- lm(response ~ predictor)
# Perform prediction from regression
newdata <- data.frame(predictor=newdata)
predictlm <- predict(object=model,
  newdata=newdata, confl=1-2*(1-pnorm(2)),
  interval="confidence")
predictlm <- as.data.frame(predictlm)
all.equal(predictlm$fit, predictv[, 1])
all.equal(predictlm$lwr, predictlow)
all.equal(predictlm$upr, predicthigh)
plot(response ~ predictor,
     xlim=range(predictor, newdata),
     ylim=range(response, predictlm),
     xlab="predictor", ylab="fitted or predicted",
     main="Predictions from lm() Regression")

abline(model, col="blue", lwd=3)
with(predictlm, {
  points(x=newdata$predictor, y=fit, pch=16, col="blue")
  lines(x=newdata$predictor, y=lwr, lwd=3, col="green")
  lines(x=newdata$predictor, y=upr, lwd=3, col="red")
})  # end with
legend(x="topleft", # Add legend
       legend=c("predictions", "+2SD", "-2SD"),
       title=NULL, inset=0.05, cex=0.8, lwd=6,
       lty=1, col=c("blue", "red", "green"))
