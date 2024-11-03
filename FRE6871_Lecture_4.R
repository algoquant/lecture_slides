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
# Inverse property doesn't hold
all.equal(matv, matv %*% invsvd %*% matv)
# Set tolerance for determining zero singular values
precv <- sqrt(.Machine$double.eps)
# Check for zero singular values
round(svdec$d, 12)
notzero <- (svdec$d > (precv*svdec$d[1]))
# Calculate the regularized inverse from SVD
invsvd <- svdec$v[, notzero] %*%
  (t(svdec$u[, notzero]) / svdec$d[notzero])
# Verify inverse property of matv
all.equal(matv, matv %*% invsvd %*% matv)
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
all.equal(covmat, covmat %*% invmat %*% covmat)
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
# Calculate the initial inverse
invmatr <- invmat + matrix(rnorm(100, sd=0.1), nc=10)
# Calculate the approximate recursive inverse of matv
invmatr <- (2*invmatr - invmatr %*% matv %*% invmatr)
# Calculate the sum of the off-diagonal elements
sum((invmatr %*% matv)[upper.tri(matv)])
# Calculate the recursive inverse of matv in a loop
invmatr <- invmat + matrix(rnorm(100, sd=0.1), nc=10)
iterv <- sapply(1:5, function(x) {
# Calculate the recursive inverse of matv
  invmatr <<- (2*invmatr - invmatr %*% matv %*% invmatr)
# Calculate the sum of the off-diagonal elements
  sum((invmatr %*% matv)[upper.tri(matv)])
})  # end sapply
# Plot the iterations
plot(x=1:5, y=iterv, t="l", xlab="iterations", ylab="error",
     main="Iterations of Recursive Matrix Inverse")
# Symbols for constant maturity Treasury rates
symbolv <- c("DGS1", "DGS2", "DGS5", "DGS10", "DGS20", "DGS30")
# Create new environment for time series
ratesenv <- new.env()
# Download time series for symbolv into ratesenv
quantmod::getSymbols(symbolv, env=ratesenv, src="FRED")
# Remove NA values in ratesenv
sapply(ratesenv, function(x) sum(is.na(x)))
sapply(ls(ratesenv), function(namev) {
  assign(x=namev, value=na.omit(get(namev, ratesenv)),
   envir=ratesenv)
}) # end sapply
sapply(ratesenv, function(x) sum(is.na(x)))
# Get class of all objects in ratesenv
sapply(ratesenv, class)
# Get class of all objects in R workspace
sapply(ls(), function(namev) class(get(namev)))
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
legend("topleft", legend=colnames(ycurves), y.intersp=1.0,
 bty="n", col=colorv, lty=1, lwd=6, inset=0.05, cex=1.0)
x11(width=6, height=5)
par(mar=c(3, 3, 2, 0), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
# Load constant maturity Treasury rates
load(file="/Users/jerzy/Develop/lecture_slides/data/rates_data.RData")
# Get end-of-year dates since 2006
datev <- xts::endpoints(ratesenv$DGS1["2006/"], on="years")
datev <- zoo::index(ratesenv$DGS1["2006/"][datev])
# Create time series of end-of-year rates
ycurves <- eapply(ratesenv, function(ratev) ratev[datev])
ycurves <- rutils::do_call(cbind, ycurves)
# Rename columns and rows, sort columns, and transpose into matrix
colnames(ycurves) <- substr(colnames(ycurves), start=4, stop=11)
ycurves <- ycurves[, order(as.numeric(colnames(ycurves)))]
colnames(ycurves) <- paste0(colnames(ycurves), "yr")
ycurves <- t(ycurves)
colnames(ycurves) <- substr(colnames(ycurves), start=1, stop=4)
# Plot matrix using plot.zoo()
colorv <- colorRampPalette(c("red", "blue"))(NCOL(ycurves))
plot.zoo(ycurves, main="Yield Curve Since 2006", lwd=3, xaxt="n",
   plot.type="single", xlab="maturity", ylab="yield", col=colorv)
# Add x-axis
axis(1, seq_along(rownames(ycurves)), rownames(ycurves))
# Add legend
legend("topleft", legend=colnames(ycurves), y.intersp=0.5,
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
retpcac <- retp %*% pcad$rotation
all.equal(pcad$x, retpcac, check.attributes=FALSE)
# Calculate products of principal component time series
round(t(retpcac) %*% retpcac, 2)
# Coerce to xts time series
retpcac <- xts(retpcac, order.by=zoo::index(retp))
retpcac <- cumsum(retpcac)
# Plot principal component time series in multiple panels
par(mfrow=c(3,2))
par(mar=c(2, 2, 0, 1), oma=c(0, 0, 0, 0))
rangev <- range(retpcac)
for (ordern in 1:NCOL(retpcac)) {
  plot.zoo(retpcac[, ordern], ylim=rangev, xlab="", ylab="")
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
   legend=paste0(symboln, c("", " solved")),
   title=NULL, inset=0.0, cex=1.0, lwd=6,
   lty=1, col=c("black", "blue"))
}  # end for
library(quantmod)  # Load quantmod
library(RQuantLib)  # Load RQuantLib
# Specify curve parameters
curvep <- list(tradeDate=as.Date("2018-01-17"),
         settleDate=as.Date("2018-01-19"),
         dt=0.25,
         interpWhat="discount",
         interpHow="loglinear")
# Specify market data: prices of FI instruments
pricev <- list(d3m=0.0363,
         fut1=96.2875,
         fut2=96.7875,
         fut3=96.9875,
         fut4=96.6875,
         s5y=0.0443,
         s10y=0.05165,
         s15y=0.055175)
# Specify dates for calculating the zero rates
datev <- seq(0, 10, 0.25)
# Specify the evaluation (as of) date
setEvaluationDate(as.Date("2018-01-17"))
# Calculate the zero rates
ratev <- DiscountCurve(params=curvep, tsQuotes=pricev, times=datev)
# Plot the zero rates
x11()
plot(x=ratev$zerorates, t="l", main="zerorates")
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
# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
predm <- runif(nrows)
noisev <- rnorm(nrows)
# Response equals linear form plus random noise
respv <- (-3 + 2*predm + noisev)
# Calculate the regression beta
betac <- cov(predm, respv)/var(predm)
# Calculate the regression alpha
alphac <- mean(respv) - betac*mean(predm)
# Specify regression formula
formulav <- respv ~ predm
regmod <- lm(formulav)  # Perform regression
class(regmod)  # Regressions have class lm
attributes(regmod)
eval(regmod$call$formula)  # Regression formula
regmod$coeff  # Regression coefficients
all.equal(coef(regmod), c(alphac, betac),
      check.attributes=FALSE)
# x11(width=5, height=4)  # Open x11 for plotting
# Set plot parameters to reduce whitespace around plot
# par(mar=c(5, 5, 2, 1), oma=c(0, 0, 0, 0))
fitv <- (alphac + betac*predm)
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
fitv <- (alphac + betac*predm)
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
# Calculate the centered (de-meaned) predictor and response vectors
predc <- predm - mean(predm)
respc <- respv - mean(respv)
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
# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
# High noise compared to coefficient
respv <- (-3 + 2*predm + rnorm(nrows, sd=8))
regmod <- lm(formulav)  # Perform regression
# Values of regression coefficients are not
# Statistically significant
summary(regmod)
par(oma=c(1, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=1.0, cex.axis=1.0, cex.main=1.0, cex.sub=1.0)
regstats <- function(stdev) {  # Noisy regression
  set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")  # initialize number generator
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
    set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")  # initialize number generator
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
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
nrows <- 100
# Define predictor matrix
predm <- runif(nrows)
# Define response with noise
noisev <- rnorm(nrows)
respv <- (-3 + 2*predm + noisev)
# Add unit column to predictor
predm <- cbind(rep(1, nrows), predm)
colnames(predm)[1] <- "intercept"
# Solve the regression using lm()
formulav <- respv ~ predm[, 2]
regmod <- lm(formulav)  # Perform regression
betalm <- regmod$coeff  # Regression coefficients
# Solve the regression using the generalized inverse
predinv <- MASS::ginv(predm)
betac <- drop(predinv %*% respv)
all.equal(betalm, betac, check.attributes=FALSE)
# Calculate the influence matrix
infmat <- predm %*% predinv
# The influence matrix is idempotent
all.equal(infmat, infmat %*% infmat)
# Calculate the fitted values using influence matrix
fitv <- drop(infmat %*% respv)
all.equal(fitv, regmod$fitted.values, check.attributes=FALSE)
# Calculate the fitted values from regression coefficients
fitv <- drop(predm %*% betac)
all.equal(fitv, regmod$fitted.values, check.attributes=FALSE)
# Calculate the covariance and standard deviations of fitted values
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
# Calculate the response without random noise for univariate regression,
# equal to weighted sum over columns of predictor.
respn <- predm %*% c(-1, 1)
# Perform loop over different realizations of random noise
fitm <- lapply(1:50, function(it) {
  # Add random noise to response
  respv <- respn + rnorm(nrows, sd=1.0)
  # Calculate the fitted values using influence matrix
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
# Calculate the forecast values
fcast <- drop(predn %*% betac)
# Calculate the inverse of the predictor matrix squared
pred2 <- MASS::ginv(crossprod(predm))
# Calculate the standard errors
predsd <- residsd*sqrt(predn %*% pred2 %*% t(predn))
# Combine the forecast values and standard errors
fcast <- cbind(forecast=fcast, stdev=diag(predsd))
# Prepare plot data
xdata <- c(predm[, 2], newdata)
ydata <- c(fitv, fcast[, 1])
# Calculate the t-quantile
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
# Calculate the forecasts from regression
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
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
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
# Define predictor matrix
nrows <- 100
ncols <- 5
# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
predm <- matrix(runif(nrows*ncols), ncol=ncols)
# Add column names
colnames(predm) <- paste0("pred", 1:ncols)
# Define the predictor weights
weightv <- runif(3:(ncols+2), min=(-1), max=1)
# Response equals weighted predictor plus random noise
noisev <- rnorm(nrows, sd=2)
respv <- (1 + predm %*% weightv + noisev)
# Perform multivariate regression using lm()
regmod <- lm(respv ~ predm)
# Solve multivariate regression using matrix algebra
# Calculate the centered (de-meaned) predictor matrix and response vector
# predc <- t(t(predm) - colMeans(predm))
predc <- apply(predm, 2, function(x) (x-mean(x)))
respc <- respv - mean(respv)
# Calculate the regression coefficients
betac <- drop(MASS::ginv(predc) %*% respc)
# Calculate the regression alpha
alphac <- mean(respv) - sum(colSums(predm)*betac)/nrows
# Compare with coefficients from lm()
all.equal(coef(regmod), c(alphac, betac), check.attributes=FALSE)
# Compare with actual coefficients
all.equal(c(1, weightv), c(alphac, betac), check.attributes=FALSE)
# Add intercept column to predictor matrix
predm <- cbind(rep(1, nrows), predm)
ncols <- NCOL(predm)
# Add column name
colnames(predm)[1] <- "intercept"
# Calculate the generalized inverse of the predictor matrix
predinv <- MASS::ginv(predm)
# Calculate the regression coefficients
betac <- predinv %*% respv
# Perform multivariate regression without intercept term
regmod <- lm(respv ~ predm - 1)
all.equal(drop(betac), coef(regmod), check.attributes=FALSE)
# Calculate the fitted values from regression coefficients
fitv <- drop(predm %*% betac)
all.equal(fitv, regmod$fitted.values, check.attributes=FALSE)
# Calculate the residuals
resids <- drop(respv - fitv)
all.equal(resids, regmod$residuals, check.attributes=FALSE)
# Residuals are orthogonal to predictor columns (predms)
sapply(resids %*% predm, all.equal, target=0)
# Residuals are orthogonal to the fitted values
all.equal(sum(resids*fitv), target=0)
# Sum of residuals is equal to zero
all.equal(sum(resids), target=0)
# Calculate the influence matrix
infmat <- predm %*% predinv
# The influence matrix is idempotent
all.equal(infmat, infmat %*% infmat)
# Calculate the fitted values using influence matrix
fitv <- drop(infmat %*% respv)
all.equal(fitv, regmod$fitted.values, check.attributes=FALSE)
# Calculate the fitted values from regression coefficients
fitv <- drop(predm %*% betac)
all.equal(fitv, regmod$fitted.values, check.attributes=FALSE)
# Calculate the centered (de-meaned) fitted values
predc <- t(t(predm) - colMeans(predm))
fittedc <- drop(predc %*% betac)
all.equal(fittedc, regmod$fitted.values - mean(respv),
  check.attributes=FALSE)
# Calculate the residuals
respc <- respv - mean(respv)
resids <- drop(respc - fittedc)
all.equal(resids, regmod$residuals, check.attributes=FALSE)
# Calculate the influence matrix
infmatc <- predc %*% MASS::ginv(predc)
# Compare the fitted values
all.equal(fittedc, drop(infmatc %*% respc), check.attributes=FALSE)
# Perform PCA of the predictors
pcad <- prcomp(predm, center=FALSE, scale=FALSE)
# Calculate the PCA predictors
predpca <- predm %*% pcad$rotation
# Principal components are orthogonal to each other
round(t(predpca) %*% predpca, 2)
# Calculate the PCA regression coefficients using lm()
regmod <- lm(respv ~ predpca - 1)
summary(regmod)
regmod$coefficients
# Calculate the PCA regression coefficients directly
colSums(predpca*drop(respv))/colSums(predpca^2)
# Create almost collinear predictors
predcol <- predm
predcol[, 1] <- (predcol[, 1]/1e3 + predcol[, 2])
# Calculate the PCA predictors
pcad <- prcomp(predcol, center=FALSE, scale=FALSE)
predpca <- predcol %*% pcad$rotation
round(t(predpca) %*% predpca, 6)
# Calculate the PCA regression coefficients
drop(MASS::ginv(predpca) %*% respv)
# Calculate the PCA regression coefficients directly
colSums(predpca*drop(respv))/colSums(predpca^2)
# Regression model summary
regsum <- summary(regmod)
# Degrees of freedom of residuals
nrows <- NROW(predm)
ncols <- NCOL(predm)
degf <- (nrows - ncols)
all.equal(degf, regsum$df[2])
# Variance of residuals
residsd <- sum(resids^2)/degf
# Inverse of predictor matrix squared
pred2 <- MASS::ginv(crossprod(predm))
# pred2 <- t(predm) %*% predm
# Variance of residuals
residsd <- sum(resids^2)/degf
# Calculate the covariance matrix of betas
betacovar <- residsd*pred2
# round(betacovar, 3)
betasd <- sqrt(diag(betacovar))
all.equal(betasd, regsum$coeff[, 2], check.attributes=FALSE)
# Calculate the t-values of betas
betatvals <- drop(betac)/betasd
all.equal(betatvals, regsum$coeff[, 3], check.attributes=FALSE)
# Calculate the two-sided p-values of betas
betapvals <- 2*pt(-abs(betatvals), df=degf)
all.equal(betapvals, regsum$coeff[, 4], check.attributes=FALSE)
# The square of the generalized inverse is equal
# to the inverse of the square
all.equal(MASS::ginv(crossprod(predm)), predinv %*% t(predinv))
# Calculate the influence matrix
infmat <- predm %*% predinv
# The influence matrix is idempotent
all.equal(infmat, infmat %*% infmat)
# Calculate the covariance and standard deviations of fitted values
fitcovar <- residsd*infmat
fitsd <- sqrt(diag(fitcovar))
# Sort the standard deviations
fitsd <- cbind(fitted=fitv, stdev=fitsd)
fitsd <- fitsd[order(fitv), ]
# Plot the standard deviations
plot(fitsd, type="l", lwd=3, col="blue",
     xlab="Fitted Value", ylab="Standard Deviation",
     main="Standard Deviations of Fitted Values\nin Multivariate Regression")
# Load time series of ETF percentage returns
retp <- rutils::etfenv$returns[, c("XLF", "XLE")]
retp <- na.omit(retp)
nrows <- NROW(retp)
head(retp)
# Define regression formula
formulav <- paste(colnames(retp)[1],
  paste(colnames(retp)[-1], collapse="+"),
  sep=" ~ ")
# Standard regression
regmod <- lm(formulav, data=retp)
regsum <- summary(regmod)
cdata <- coredata(retp)
plot(cdata[, 1], cdata[, 2], 
     xlab="XLE", ylab="XLF", main="ETF Returns")
abline(regmod, col="blue", lwd=3)
# Bootstrap of regression
# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
bootd <- sapply(1:100, function(x) {
  samplev <- sample.int(nrows, replace=TRUE)
  regmod <- lm(formulav, data=retp[samplev, ])
  regmod$coefficients
})  # end sapply
# Means and standard errors from regression
regsum$coefficients
# Means and standard errors from bootstrap
dim(bootd)
t(apply(bootd, MARGIN=1,
function(x) c(mean=mean(x), stderror=sd(x))))
# New data predictor is a data frame or row vector
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
newdata <- data.frame(matrix(c(1, rnorm(5)), nr=1))
colnamev <- colnames(predm)
colnames(newdata) <- colnamev
newdata <- as.matrix(newdata)
fcast <- drop(newdata %*% betac)
predsd <- drop(sqrt(newdata %*% betacovar %*% t(newdata)))
# Create formula from text string
formulav <- paste0("respv ~ ",
  paste(colnames(predm), collapse=" + "), " - 1")
# Specify multivariate regression using formula
regmod <- lm(formulav, data=data.frame(cbind(respv, predm)))
regsum <- summary(regmod)
# Predict from lm object
fcastlm <- predict.lm(object=model, newdata=newdata,
   interval="confidence", confl=1-2*(1-pnorm(2)))
# Calculate the t-quantile
tquant <- qt(pnorm(2), df=degf)
fcasth <- (fcast + tquant*predsd)
fcastl <- (fcast - tquant*predsd)
# Compare with matrix calculations
all.equal(fcastlm[1, "fit"], fcast)
all.equal(fcastlm[1, "lwr"], fcastl)
all.equal(fcastlm[1, "upr"], fcasth)
# TSS = ESS + RSS
tss <- sum((respv-mean(respv))^2)
ess <- sum((fitv-mean(fitv))^2)
rss <- sum(resids^2)
all.equal(tss, ess + rss)
# Set regression attribute for intercept
attributes(regmod$terms)$intercept <- 1
# Regression summary
regsum <- summary(regmod)
# Regression R-squared
rsquared <- ess/tss
all.equal(rsquared, regsum$r.squared)
# Correlation between response and fitted values
corfit <- drop(cor(respv, fitv))
# Squared correlation between response and fitted values
all.equal(corfit^2, rsquared)
nrows <- NROW(predm)
ncols <- NCOL(predm)
# Degrees of freedom of residuals
degf <- (nrows - ncols)
# Adjusted R-squared
rsqadj <- (1-sum(resids^2)/degf/var(respv))
# Compare adjusted R-squared from lm()
all.equal(drop(rsqadj), regsum$adj.r.squared)
# Plot four curves in loop
degf <- c(3, 5, 9, 21)  # Degrees of freedom
colorv <- c("black", "red", "blue", "green")
for (indeks in 1:NROW(degf)) {
  curve(expr=df(x, df1=degf[indeks], df2=3),
    xlim=c(0, 4), xlab="", ylab="", lwd=2,
    col=colorv[indeks], add=as.logical(indeks-1))
}  # end for
# Add title
title(main="F-Distributions", line=0.5)
# Add legend
labelv <- paste("degf", degf, sep=" = ")
legend("topright", title="Degrees of Freedom", inset=0.0, bty="n",
       y.intersp=0.4, labelv, cex=1.2, lwd=6, lty=1, col=colorv)
sigmax <- var(rnorm(nrows))
sigmay <- var(rnorm(nrows))
fratio <- sigmax/sigmay
# Cumulative probability for q = fratio
pf(fratio, nrows-1, nrows-1)
# p-value for fratios
1-pf((10:20)/10, nrows-1, nrows-1)
# F-statistic from lm()
regsum$fstatistic
# Degrees of freedom of residuals
degf <- (nrows - ncols)
# F-statistic from ESS and RSS
fstat <- (ess/(ncols-1))/(rss/degf)
all.equal(fstat, regsum$fstatistic[1], check.attributes=FALSE)
# p-value of F-statistic
1-pf(q=fstat, df1=(ncols-1), df2=(nrows-ncols))
library(lmtest)  # Load lmtest
# Define predictor matrix
predm <- 1:30
omitv <- sin(0.2*1:30)
# Response depends on both predictors
respv <- 0.2*predm + omitv + 0.2*rnorm(30)
# Mis-specified regression only one predictor
modovb <- lm(respv ~ predm)
regsum <- summary(modovb)
regsum$coeff
regsum$r.squared
# Durbin-Watson test shows residuals are autocorrelated
lmtest::dwtest(modovb)
# Plot the regression diagnostic plots
x11(width=5, height=7)
par(mfrow=c(2,1))  # Set plot panels
par(mar=c(3, 2, 1, 1), oma=c(1, 0, 0, 0))
plot(respv ~ predm)
abline(modovb, lwd=2, col="red")
title(main="Omitted Variable Regression", line=-1)
plot(modovb, which=2, ask=FALSE)  # Plot just Q-Q
