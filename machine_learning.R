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
# Perform PCA for two stocks
retp <- scale(na.omit(rutils::etfenv$returns
          [, as.character(formulav)[-1]]))
crossprod(retp) / NROW(retp)
w1 <- sqrt(0.5); w2 <- w1
foo <- matrix(c(w1, w2, -w2, w1), nc=2)
t(foo) %*% foo
# bar <- retp %*% t(solve(foo))
(t(bar) %*% bar) / NROW(bar)
covmat <- function(retp, anglev=0) {
  w1 <- cos(anglev)
  w2 <- sin(anglev)
  matv <- matrix(c(w1, -w2, w2, w1), nc=2)
  pcav <- retp %*% t(matv)
  (t(pcav) %*% pcav) / NROW(pcav)
}  # end covmat
bar <- covmat(retp, anglev=pi/4)
crossprod(retp) / NROW(retp)
(t(bar) %*% bar) / NROW(bar)
angles <- seq(0, pi/2, by=pi/24)
covmat <- sapply(angles, function(anglev)
  covmat(retp, anglev=anglev)[1, 1])
plot(x=angles, y=covmat, t="l")
optiml <- optimize(
  f=function(anglev)
    -covmat(retp, anglev=anglev)[1, 1],
  interval=range(angles))
anglev <- optiml$minimum
bar <- covmat(retp, anglev=anglev)
tan(anglev)
w1 <- cos(anglev)
w2 <- sin(anglev)
matv <- matrix(c(w1, -w2, w2, w1), nc=2)
pcav <- retp %*% t(matv)
(t(pcav) %*% pcav) / NROW(pcav)
plot(x=pcav[, 1], y=pcav[, 2],
     xlim=c(-10, 10), ylim=c(-10, 10))
regmod <- lm(formulav, data=retp)
# Get regression coefficients
coef(summary(regmod))
foo <- cbind(rnorm(1000, sd=0.2), rnorm(1000)) %*% t(matv)
(t(foo) %*% foo) / NROW(foo)
plot(x=foo[, 1], y=foo[, 2])
summary(lm(foo[, 1] ~ foo[, 2]))
optiml <- optimize(
  f=function(anglev)
    -covmat(foo, anglev=anglev)[1, 1],
  interval=range(angles))
anglev <- optiml$minimum
tan(anglev)
###
w1 <- cos(0.5)
s1 <- 1
s2 <- 2
foo <- matrix(c(s1^2, s1*s2*w1, s1*s2*w1, s2^2), nc=2)
eigen(foo)
# Durbin-Watson test of autocorrelation of residuals
lmtest::dwtest(regmod)
# Plot scatterplot of returns
plot(formulav, data=rutils::etfenv$returns,
     main="Regression XLP ~ VTI")
# Add regression line
abline(regmod, lwd=2, col="red")
# Select ETF symbols
symbolv <- c("IEF", "DBC", "XLU", "XLF", "XLP", "XLI")
# Calculate the ETF prices and log returns
pricev <- rutils::etfenv$prices[, symbolv]
# Applying zoo::na.locf() can produce bias of the correlations
# pricev <- zoo::na.locf(pricev, na.rm=FALSE)
# pricev <- zoo::na.locf(pricev, fromLast=TRUE)
pricev <- na.omit(pricev)
retp <- rutils::diffit(log(pricev))
# Calculate the covariance matrix
covmat <- cov(retp)
# Standardize (de-mean and scale) the returns
retp <- lapply(retp, function(x) {(x - mean(x))/sd(x)})
retp <- rutils::do_call(cbind, retp)
round(sapply(retp, mean), 6)
sapply(retp, sd)
# Alternative (much slower) center (de-mean) and scale the returns
# retp <- apply(retp, 2, scale)
# retp <- xts::xts(retp, zoo::index(pricev))
# Alternative (much slower) center (de-mean) and scale the returns
# retp <- scale(retp, center=TRUE, scale=TRUE)
# retp <- xts::xts(retp, zoo::index(pricev))
# Alternative (much slower) center (de-mean) and scale the returns
# retp <- t(retp) - colMeans(retp)
# retp <- retp/sqrt(rowSums(retp^2)/(NCOL(retp)-1))
# retp <- t(retp)
# retp <- xts::xts(retp, zoo::index(pricev))
# Calculate the correlation matrix
cormat <- cor(retp)
# Reorder correlation matrix based on clusters
library(corrplot)
ordern <- corrMatOrder(cormat, order="hclust",
  hclust.method="complete")
cormat <- cormat[ordern, ordern]
# Plot the correlation matrix
colorv <- colorRampPalette(c("red", "white", "blue"))
# x11(width=6, height=6)
corrplot(cormat, title=NA, tl.col="black", mar=c(0,0,0,0),
    method="square", col=colorv(NCOL(cormat)), tl.cex=0.8,
    cl.offset=0.75, cl.cex=0.7, cl.align.text="l", cl.ratio=0.25)
title("ETF Correlation Matrix", line=2)
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
  -sum(retp^2) + 1e4*(1 - sum(weightv^2))^2
}  # end objfun
# Objective for equal weight portfolio
objfun(weightv, retp)
# Compare speed of vector multiplication methods
summary(microbenchmark(
  transp=(t(retp[, 1]) %*% retp[, 1]),
  sumv=sum(retp[, 1]^2),
  times=10))[, c(1, 4, 5)]
# Find weights with maximum variance
optiml <- optim(par=weightv,
  fn=objfun,
  retp=retp,
  method="L-BFGS-B",
  upper=rep(10.0, nweights),
  lower=rep(-10.0, nweights))
# Optimal weights and maximum variance
weights1 <- optiml$par
-objfun(weights1, retp)
# Plot first principal component weights
barplot(weights1, names.arg=names(weights1), xlab="", ylab="",
  main="First Principal Component Weights")
# PC1 returns
pc1 <- drop(retp %*% weights1)
# Redefine objective function
objfun <- function(weightv, retp) {
  retp <- retp %*% weightv
  -sum(retp^2) + 1e4*(1 - sum(weightv^2))^2 +
    1e4*(sum(weights1*weightv))^2
}  # end objfun
# Find second PC weights using parallel DEoptim
optiml <- DEoptim::DEoptim(fn=objfun,
  upper=rep(10, NCOL(retp)),
  lower=rep(-10, NCOL(retp)),
  retp=retp, control=list(parVar="weights1",
    trace=FALSE, itermax=1000, parallelType=1))
# PC2 weights
weights2 <- optiml$optim$bestmem
names(weights2) <- colnames(retp)
sum(weights2^2)
sum(weights1*weights2)
# PC2 returns
pc2 <- drop(retp %*% weights2)
# Plot second principal component loadings
barplot(weights2, names.arg=names(weights2), xlab="", ylab="",
  main="Second Principal Component Loadings")
# Calculate the eigenvalues and eigenvectors
eigend <- eigen(cormat)
eigend$vectors
# Compare with optimization
all.equal(sum(diag(cormat)), sum(eigend$values))
all.equal(abs(eigend$vectors[, 1]), abs(weights1), check.attributes=FALSE)
all.equal(abs(eigend$vectors[, 2]), abs(weights2), check.attributes=FALSE)
all.equal(eigend$values[1], var(pc1), check.attributes=FALSE)
all.equal(eigend$values[2], var(pc2), check.attributes=FALSE)
# Eigenvalue equations
(cormat %*% weights1) / weights1 / var(pc1)
(cormat %*% weights2) / weights2 / var(pc2)
# Plot eigenvalues
barplot(eigend$values, names.arg=paste0("PC", 1:nweights),
  las=3, xlab="", ylab="", main="Principal Component Variances")
# Calculate the eigen decomposition of the correlation matrix
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
# Redefine objective function to minimize variance
objfun <- function(weightv, retp) {
  retp <- retp %*% weightv
  sum(retp^2) + 1e4*(1 - sum(weightv^2))^2
}  # end objfun
# Find highest order PC weights using parallel DEoptim
optiml <- DEoptim::DEoptim(fn=objfun,
  upper=rep(10, NCOL(retp)),
  lower=rep(-10, NCOL(retp)),
  retp=retp, control=list(trace=FALSE,
    itermax=1000, parallelType=1))
# PC6 weights and returns
weights6 <- optiml$optim$bestmem
names(weights6) <- colnames(retp)
sum(weights6^2)
sum(weights1*weights6)
# Compare with eigend vector
weights6
eigend$vectors[, 6]
# Calculate the objective function
objfun(weights6, retp)
objfun(eigend$vectors[, 6], retp)
# Plot highest order principal component loadings
weights6 <- eigend$vectors[, 6]
names(weights6) <- colnames(retp)
barplot(weights6, names.arg=names(weights6), xlab="", ylab="",
  main="Highest Order Principal Component Loadings")
# Perform principal component analysis PCA
pcad <- prcomp(retp, scale=TRUE)
# Plot standard deviations of principal components
barplot(pcad$sdev, names.arg=colnames(pcad$rotation),
  las=3, xlab="", ylab="",
  main="Scree Plot: Volatilities of Principal Components \n of ETF Returns")
# Calculate the number of principal components which sum up to at least 80% of the total variance
pcavar <- pcad$sdev^2
which(cumsum(pcavar)/sum(pcavar) > 0.8)[1]
# Plot barplots with PCA loadings (weights) in multiple panels
pcad$rotation
# x11(width=6, height=7)
par(mfrow=c(nweights/2, 2))
par(mar=c(3, 2, 2, 1), oma=c(0, 0, 0, 0))
for (ordern in 1:nweights) {
  barplot(pcad$rotation[, ordern], las=3, xlab="", ylab="", main="")
  title(paste0("PC", ordern), line=-1, col.main="red")
}  # end for
# Calculate the products of principal component time series
round(t(pcad$x) %*% pcad$x, 2)
# Calculate the principal component time series from returns
datev <- zoo::index(pricev)
retpca <- xts::xts(retp %*% pcad$rotation, order.by=datev)
round(cov(retpca), 3)
all.equal(coredata(retpca), pcad$x, check.attributes=FALSE)
retpcac <- cumsum(retpca)
# Plot principal component time series in multiple panels
rangev <- range(retpcac)
for (ordern in 1:nweights) {
  plot.zoo(retpcac[, ordern], ylim=rangev, xlab="", ylab="")
  title(paste0("PC", ordern), line=-1, col.main="red")
}  # end for
# Invert all the principal component time series
retpca <- retp %*% pcad$rotation
solved <- retpca %*% solve(pcad$rotation)
all.equal(coredata(retp), solved)
# Invert first 3 principal component time series
solved <- retpca[, 1:3] %*% solve(pcad$rotation)[1:3, ]
solved <- xts::xts(solved, datev)
solved <- cumsum(solved)
retc <- cumsum(retp)
# Plot the solved returns
for (symbol in symbolv) {
  plot.zoo(cbind(retc[, symbol], solved[, symbol]),
    plot.type="single", col=c("black", "blue"), xlab="", ylab="")
  legend(x="topleft", bty="n", legend=paste0(symbol, c("", " solved")), y.intersp=0.4,
   title=NULL, inset=0.0, cex=1.0, lwd=6, lty=1, col=c("black", "blue"))
}  # end for
# Create a matrix with low correlation
ndata <- 10
cormat <- matrix(rep(0.1, ndata^2), nc=ndata)
diag(cormat) <- rep(1, ndata)
# Calculate the condition number
eigend <- eigen(cormat)
eigenval <- eigend$values
max(eigenval)/min(eigenval)
# Create a matrix with high correlation
cormat <- matrix(rep(0.9, ndata^2), nc=ndata)
diag(cormat) <- rep(1, ndata)
# Calculate the condition number
eigend <- eigen(cormat)
eigenval <- eigend$values
max(eigenval)/min(eigenval)
# Calculate the condition numbers as function correlation
corv <- seq(0.1, 0.9, 0.1)
condv <- sapply(corv, function(corv) {
  cormat <- matrix(rep(corv, ndata^2), nc=ndata)
  diag(cormat) <- rep(1, ndata)
  eigend <- eigen(cormat)
  eigenval <- eigend$values
  max(eigenval)/min(eigenval)
})  # end sapply
# Plot the condition numbers
plot(x=corv, y=condv, t="l",
  main="Condition Number as Function of Correlation",
  xlab="correlation", ylab="condition number")
# Simulate uncorrelated stock returns
nstocks <- 10
nrows <- 100
# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
retp <- matrix(rnorm(nstocks*nrows), nc=nstocks)
# Calculate the condition numbers as function of number of observations
obsvec <- seq(20, nrows, 10)
condv <- sapply(obsvec, function(nobs) {
  cormat <- cor(retp[1:nobs, ])
  eigend <- eigen(cormat)
  eigenval <- eigend$values
  max(eigenval)/min(eigenval)
})  # end sapply
# Plot the condition numbers
plot(x=obsvec, y=condv, t="l",
  main="Condition Number as Function of Number of Observations",
  xlab="number of observations", ylab="condition number")
# Load daily S&P500 log percentage stock returns
load(file="/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
# Calculate the number of NA values in retstock
retp <- retstock
colSums(is.na(retp))
# Calculate the correlations ignoring NA values
cor(retp$DAL, retp$FOXA, use="pairwise.complete.obs")
cor(na.omit(retp[, c("DAL", "FOXA")]))[2]
cormat <- cor(retp, use="pairwise.complete.obs")
sum(is.na(cormat))
cormat[is.na(cormat)] <- 0
# Perform principal component analysis PCA - produces error
pcad <- prcomp(retp, scale=TRUE)
# Calculate the eigen decomposition of the correlation matrix
eigend <- eigen(cormat)
# Calculate the eigenvalues and eigenvectors
eigenval <- eigend$values
eigenvec <- eigend$vectors
# Calculate the number of negative eigenvalues
sum(eigenval<0)
# Calculate the condition number
max(eigenval)/min(abs(eigenval))
# Calculate the number of eigenvalues which sum up to at least 80% of the total variance
which(cumsum(eigenval)/sum(eigenval) > 0.8)[1]
# Plot the eigenvalues
barplot(eigenval, xlab="", ylab="", las=3,
  names.arg=paste0("ev", 1:NROW(eigenval)),
  main="Eigenvalues of Stock Correlation Matrix")
# Calculate the stock variance
varv <- sapply(retp, var, na.rm=TRUE)
# Calculate the returns of low and high volatility stocks
nstocks <- NCOL(retp)
medianv <- median(varv)
retlow <- retp[, varv <= medianv]
rethigh <- retp[, varv > medianv]
# Calculate the correlations of low volatility stocks
cormat <- cor(retlow, use="pairwise.complete.obs")
cormat[is.na(cormat)] <- 0
# Calculate the mean correlations
mean(cormat[upper.tri(cormat)])
# Calculate the eigen decomposition of the correlation matrix
eigend <- eigen(cormat)
eigenval <- eigend$values
# Calculate the number of negative eigenvalues
sum(eigenval < 0)
# Calculate the number of eigenvalues which sum up to at least 80% of the total variance
which(cumsum(eigenval)/sum(eigenval) > 0.8)[1]
# Calculate the condition number
max(eigenval)/min(abs(eigenval))
# Calculate the correlations of high volatility stocks
cormat <- cor(rethigh, use="pairwise.complete.obs")
cormat[is.na(cormat)] <- 0
# Calculate the mean correlations
mean(cormat[upper.tri(cormat)])
# Calculate the eigen decomposition of the correlation matrix
eigend <- eigen(cormat)
eigenval <- eigend$values
# Calculate the number of negative eigenvalues
sum(eigenval < 0)
# Calculate the number of eigenvalues which sum up to at least 80% of the total variance
which(cumsum(eigenval)/sum(eigenval) > 0.8)[1]
# Calculate the condition number
max(eigenval)/min(abs(eigenval))
# Subset (select) the stock returns after the start date of VTI
retvti <- na.omit(rutils::etfenv$returns$VTI)
colnames(retvti) <- "VTI"
retp <- retstock[zoo::index(retvti)]
datev <- zoo::index(retp)
retvti <- retvti[datev]
nrows <- NROW(retp)
nstocks <- NCOL(retp)
head(retp[, 1:5])
# Calculate the monthly end points
endd <- rutils::calc_endpoints(retvti, interval="months")
retvti[head(endd)]
retvti[tail(endd)]
# Remove stub interval at the end
endd <- endd[-NROW(endd)]
npts <- NROW(endd)
# Calculate the monthly stock volatilities and correlations
stdcor <- sapply(2:npts, function(endp) {
  # cat("endp = ", endp, "\n")
  retp <- retp[endd[endp-1]:endd[endp]]
  cormat <- cor(retp, use="pairwise.complete.obs")
  cormat[is.na(cormat)] <- 0
  c(stdev=sd(retvti[endd[endp-1]:endd[endp]]),
    cor=mean(cormat[upper.tri(cormat)]))
})  # end sapply
stdcor <- t(stdcor)
# Scatterplot of stock volatilities and correlations
plot(x=stdcor[, "stdev"], y=stdcor[, "cor"],
 xlab="volatility", ylab="correlation",
 main="Monthly Stock Volatilities and Correlations")
# Plot stock volatilities and correlations
colnamev <- colnames(stdcor)
stdcor <- xts(stdcor, zoo::index(retvti[endd]))
dygraphs::dygraph(stdcor,
  main="Monthly Stock Volatilities and Correlations") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)
# Calculate the median VTI volatility
medianv <- median(stdcor[, "stdev"])
# Calculate the stock returns of low volatility intervals
retlow <- lapply(2:npts, function(endp) {
  if (stdcor[endp-1, "stdev"] <= medianv)
    retp[endd[endp-1]:endd[endp]]
})  # end lapply
retlow <- rutils::do_call(rbind, retlow)
# Calculate the stock returns of high volatility intervals
rethigh <- lapply(2:npts, function(endp) {
  if (stdcor[endp-1, "stdev"] > medianv)
    retp[endd[endp-1]:endd[endp]]
})  # end lapply
rethigh <- rutils::do_call(rbind, rethigh)
# Calculate the correlations of low volatility intervals
cormat <- cor(retlow, use="pairwise.complete.obs")
cormat[is.na(cormat)] <- 0
mean(cormat[upper.tri(cormat)])
# Calculate the eigen decomposition of the correlation matrix
eigend <- eigen(cormat)
eigenval <- eigend$values
sum(eigenval < 0)
# Calculate the number of eigenvalues which sum up to at least 80% of the total variance
which(cumsum(eigenval)/sum(eigenval) > 0.8)[1]
# Calculate the condition number
max(eigenval)/min(abs(eigenval))
# Calculate the correlations of high volatility intervals
cormat <- cor(rethigh, use="pairwise.complete.obs")
cormat[is.na(cormat)] <- 0
mean(cormat[upper.tri(cormat)])
# Calculate the eigen decomposition of the correlation matrix
eigend <- eigen(cormat)
eigenval <- eigend$values
sum(eigenval < 0)
# Calculate the number of eigenvalues which sum up to at least 80% of the total variance
which(cumsum(eigenval)/sum(eigenval) > 0.8)[1]
# Calculate the condition number
max(eigenval)/min(abs(eigenval))
# Calculate the AAPL and XLK returns
retp <- na.omit(cbind(returns$AAPL, rutils::etfenv$returns$XLK))
# Calculate the trailing correlations
lambdaf <- 0.99
covarv <- HighFreq::run_covar(retp, lambdaf)
correlv <- covarv[, 1, drop=FALSE]/sqrt(covarv[, 2]*covarv[, 3])
# Plot dygraph of XLK returns and AAPL correlations
datav <- cbind(cumsum(retp$XLK), correlv)
colnames(datav)[2] <- "correlation"
colnamev <- colnames(datav)
endd <- rutils::calc_endpoints(retp, interval="weeks")
dygraphs::dygraph(datav[endd], main="AAPL Correlations With XLK") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)
# Scatterplot of trailing stock volatilities and correlations
volv <- sqrt(covarv[, 2])
plot(x=volv[endd], y=correlv[endd, ], pch=1, col="blue",
 xlab="AAPL volatility", ylab="Correlation",
 main="Trailing Volatilities and Correlations of AAPL vs XLK")
# Interactive scatterplot of trailing stock volatilities and correlations
datev <- zoo::index(retp[endd])
datav <- data.frame(datev, volv[endd], correlv[endd, ])
colnames(datav) <- c("date", "volatility", "correlation")
library(plotly)
plotly::plot_ly(data=datav, x=~volatility, y=~correlation,
  type="scatter", mode="markers", text=datev) %>%
  layout(title="Trailing Volatilities and Correlations of AAPL vs XLK")
# Plot trailing stock volatilities and correlations
datav <- xts(cbind(volv, correlv), zoo::index(retp))
colnames(datav) <- c("volatility", "correlation")
colnamev <- colnames(datav)
dygraphs::dygraph(datav[endd], main="AAPL Trailing Stock Volatility and Correlation") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)
# Calculate the portfolio returns
retvti <- na.omit(rutils::etfenv$returns$VTI)
colnames(retvti) <- "VTI"
datev <- zoo::index(retvti)
retp <- retstock100
retp[is.na(retp)] <- 0
retp <- retp[datev]
nrows <- NROW(retp)
nstocks <- NCOL(retp)
head(retp[, 1:5])
# Calculate the average trailing portfolio correlations
lambdaf <- 0.9
correlv <- sapply(retp, function(retp) {
  covarv <- HighFreq::run_covar(cbind(retvti, retp), lambdaf)
  covarv[, 1, drop=FALSE]/sqrt(covarv[, 2]*covarv[, 3])
})  # end sapply
correlv[is.na(correlv)] <- 0
correlp <- rowMeans(correlv)
# Scatterplot of trailing stock volatilities and correlations
volvti <- sqrt(HighFreq::run_var(retvti, lambdaf))
endd <- rutils::calc_endpoints(retvti, interval="weeks")
plot(x=volvti[endd], y=correlp[endd],
 xlab="volatility", ylab="correlation",
 main="Trailing Stock Volatilities and Correlations")
# Plot trailing stock volatilities and correlations
datav <- xts(cbind(volvti, correlp), datev)
colnames(datav) <- c("volatility", "correlation")
colnamev <- colnames(datav)
dygraphs::dygraph(datav[endd],
  main="Trailing Stock Volatilities and Correlations") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)
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
foo <- etfenv$returns[, c("VTI", "VEU")]
endd <- endpoints(foo, on="weeks")
head(foo)
tail(foo)
class(foo)
dim(foo)
regmod <- lm(paste(names(foo), collapse=" ~ "), data=foo)
regsum <- summary(regmod)
regsum
lmtest::dwtest(regmod)
# Filter over non-overlapping periods
bar <- names(foo)
foo <- merge(period.sum(foo[, 1], INDEX=endd), period.sum(foo[, 2], INDEX=endd))
foo <- foo[complete.cases(foo), ]
names(foo) <- bar
# Filter over overlapping periods
foo <- rollsum(foo, k=11)
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
# Calculate the ETF returns
retp <- na.omit(rutils::etfenv$returns)
# Perform singular value decomposition
svdec <- svd(retp)
barplot(svdec$d, main="Singular Values of ETF Returns")
# Calculate the generalized inverse from SVD
invmat <- svdec$v %*% (t(svdec$u) / svdec$d)
# Verify inverse property of inverse
all.equal(zoo::coredata(retp), retp %*% invmat %*% retp)
# Calculate the regularized inverse from SVD
dimax <- 1:3
invreg <- svdec$v[, dimax] %*%
  (t(svdec$u[, dimax]) / svdec$d[dimax])
# Calculate the regularized inverse using RcppArmadillo
invcpp <- HighFreq::calc_invsvd(retp, dimax=3)
all.equal(invreg, invcpp, check.attributes=FALSE)
# Calculate the regularized inverse from Moore-Penrose pseudo-inverse
retsq <- t(retp) %*% retp
eigend <- eigen(retsq)
inv2 <- eigend$vectors[, dimax] %*%
  (t(eigend$vectors[, dimax]) / eigend$values[dimax])
invmp <- inv2 %*% t(retp)
all.equal(invreg, invmp, check.attributes=FALSE)
# Define transformation matrix
matv <- matrix(runif(ncols^2, min=(-1), max=1), ncol=ncols)
# Calculate the linear combinations of predictor columns
predt <- predm %*% matv
# Calculate the influence matrix of the transformed predictor
influencet <- predt %*% MASS::ginv(predt)
# Compare the influence matrices
all.equal(infmat, influencet)
# Perform PCA of the predictors
pcad <- prcomp(predm, center=FALSE, scale=FALSE)
# Calculate the PCA predictors
predpca <- predm %*% pcad$rotation
# Principal components are orthogonal to each other
round(t(predpca) %*% predpca, 2)
# Calculate the PCA influence matrix
infmat <- predm %*% MASS::ginv(predm)
infpca <- predpca %*% MASS::ginv(predpca)
all.equal(infmat, infpca)
# Calculate the regression coefficients
coeffv <- drop(MASS::ginv(predm) %*% respv)
# Transform the collinear regression coefficients to the PCA
drop(coeffv %*% pcad$rotation)
# Calculate the PCA regression coefficients
drop(MASS::ginv(predpca) %*% respv)
# Calculate the PCA regression coefficients directly
colSums(predpca*drop(respv))/colSums(predpca^2)
# Create almost collinear predictors
predcol <- predm
predcol[, 1] <- (predcol[, 1]/1e3 + predcol[, 2])
# Calculate the collinear regression coefficients
coeffv <- drop(MASS::ginv(predcol) %*% respv)
coeffv
# Calculate the PCA predictors
pcad <- prcomp(predcol, center=FALSE, scale=FALSE)
predpca <- predcol %*% pcad$rotation
round(t(predpca) %*% predpca, 6)
# Transform the collinear regression coefficients to the PCA
drop(coeffv %*% pcad$rotation)
# Calculate the PCA regression coefficients
coeffpca <- drop(MASS::ginv(predpca) %*% respv)
# Calculate the PCA regression coefficients directly
colSums(predpca*drop(respv))/colSums(predpca^2)
# Transform the PCA regression coefficients to the original coordinates
drop(coeffpca %*% MASS::ginv(pcad$rotation))
coeffv
# Calculate the regression coefficients after dimension reduction
npca <- NROW(coeffpca)
drop(coeffpca[-npca] %*% MASS::ginv(pcad$rotation)[-npca, ])
# Compare with the collinear regression coefficients
coeffv
# Calculate the original regression coefficients
drop(MASS::ginv(predm) %*% respv)
lambdav <- c(0.5, 1, 1.5)
colorv <- c("red", "blue", "green")
# Plot three curves in loop
for (it in 1:3) {
  curve(expr=plogis(x, scale=lambdav[it]),
xlim=c(-4, 4), type="l", xlab="", ylab="", lwd=4,
col=colorv[it], add=(it>1))
}  # end for
# Add title
title(main="Logistic function", line=0.5)
# Add legend
legend("topleft", title="Scale parameters",
       paste("lambda", lambdav, sep="="), y.intersp=0.4,
       inset=0.05, cex=0.8, lwd=6, bty="n", lty=1, col=colorv)
# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
# Simulate overlapping scores data
sample1 <- runif(100, max=0.6)
sample2 <- runif(100, min=0.4)
# Perform Mann-Whitney test for data location
wilcox.test(sample1, sample2)
# Combine scores and add categorical variable
predm <- c(sample1, sample2)
respv <- c(logical(100), !logical(100))
# Perform logit regression
logmod <- glm(respv ~ predm, family=binomial(logit))
class(logmod)
summary(logmod)
ordern <- order(predm)
plot(x=predm[ordern], y=logmod$fitted.values[ordern],
     main="Category Densities and Logistic Function",
     type="l", lwd=4, col="orange", xlab="predictor", ylab="density")
densv <- density(predm[respv])
densv$y <- densv$y/max(densv$y)
lines(densv, col="red")
polygon(c(min(densv$x), densv$x, max(densv$x)), c(min(densv$y), densv$y, min(densv$y)), col=rgb(1, 0, 0, 0.2), border=NA)
densv <- density(predm[!respv])
densv$y <- densv$y/max(densv$y)
lines(densv, col="blue")
polygon(c(min(densv$x), densv$x, max(densv$x)), c(min(densv$y), densv$y, min(densv$y)), col=rgb(0, 0, 1, 0.2), border=NA)
# Add legend
legend(x="top", cex=1.0, bty="n", lty=c(1, NA, NA),
 lwd=c(6, NA, NA), pch=c(NA, 15, 15), y.intersp=0.4,
 legend=c("logistic fit", "TRUE", "FALSE"),
 col=c("orange", "red", "blue"),
 text.col=c("black", "red", "blue"))
# Likelihood function of binomial distribution
likefun <- function(prob, b) {
  b*log(prob) + (1-b)*log(1-prob)
}  # end likefun
likefun(prob=0.25, b=1)
# Plot binomial likelihood function
curve(expr=likefun(x, b=1), xlim=c(0, 1), lwd=3,
      xlab="prob", ylab="likelihood", col="blue",
      main="Binomial Likelihood Function")
curve(expr=likefun(x, b=0), lwd=3, col="red", add=TRUE)
legend(x="top", legend=c("b = 1", "b = 0"),
       title=NULL, inset=0.3, cex=1.0, lwd=6, y.intersp=0.4,
       bty="n", lty=1, col=c("blue", "red"))
# Add intercept column to the predictor matrix
predm <- cbind(intercept=rep(1, NROW(respv)), predm)
# Likelihood function of the logistic model
likefun <- function(coeff, respv, predm) {
  probs <- plogis(drop(predm %*% coeff))
  -sum(respv*log(probs) + (1-respv)*log((1-probs)))
}  # end likefun
# Run likelihood function
coeff <- c(1, 1)
likefun(coeff, respv, predm)
# Rastrigin function with vector argument for optimization
rastrigin <- function(vecv, param=25) {
  sum(vecv^2 - param*cos(vecv))
}  # end rastrigin
vecv <- c(pi/6, pi/6)
rastrigin(vecv=vecv)
# Draw 3d surface plot of Rastrigin function
options(rgl.useNULL=TRUE); library(rgl)
rgl::persp3d(
  x=Vectorize(function(x, y) rastrigin(vecv=c(x, y))),
  xlim=c(-10, 10), ylim=c(-10, 10),
  col="green", axes=FALSE, zlab="", main="rastrigin")
# Render the 3d surface plot of function
rgl::rglwidget(elementId="plot3drgl", width=400, height=400)
# Optimize with respect to vector argument
optiml <- optim(par=vecv, fn=rastrigin,
        method="L-BFGS-B",
        upper=c(4*pi, 4*pi),
        lower=c(pi/2, pi/2),
        param=1)
# Optimal parameters and value
optiml$par
optiml$value
rastrigin(optiml$par, param=1)
# Initial parameters
initp <- c(1, 1)
# Find max likelihood parameters using steepest descent optimizer
optiml <- optim(par=initp,
        fn=likefun, # Log-likelihood function
        method="L-BFGS-B", # Quasi-Newton method
        respv=respv,
        predm=predm,
        upper=c(20, 20), # Upper constraint
        lower=c(-20, -20), # Lower constraint
        hessian=TRUE)
# Optimal logistic parameters
optiml$par
unname(logmod$coefficients)
# Standard errors of parameters
sqrt(diag(solve(optiml$hessian)))
regsum <- summary(logmod)
regsum$coefficients[, 2]
library(ISLR)  # Load package ISLR
# get documentation for package tseries
packageDescription("ISLR")  # get short description
help(package="ISLR")  # Load help page
library(ISLR)  # Load package ISLR
data(package="ISLR")  # list all datasets in ISLR
ls("package:ISLR")  # list all objects in ISLR
detach("package:ISLR")  # Remove ISLR from search path
# Coerce the default and student columns to Boolean
Default <- ISLR::Default
Default$default <- (Default$default == "Yes")
Default$student <- (Default$student == "Yes")
attach(Default)  # Attach Default to search path
# Explore credit default data
summary(Default)
sapply(Default, class)
dim(Default)
head(Default)
# Plot data points for non-defaulters
xlim <- range(balance); ylim <- range(income)
plot(income ~ balance,
     main="Default Dataset from Package ISLR",
     xlim=xlim, ylim=ylim, pch=4, col="blue",
     data=Default[!default, ])
# Plot data points for defaulters
points(income ~ balance, pch=4, lwd=2, col="red",
 data=Default[default, ])
# Add legend
legend(x="topright", legend=c("non-defaulters", "defaulters"),
 y.intersp=0.4, bty="n", col=c("blue", "red"), lty=1, lwd=6, pch=4)
# Perform Mann-Whitney test for the location of the balances
wilcox.test(balance[default], balance[!default])
# Perform Mann-Whitney test for the location of the incomes
wilcox.test(income[default], income[!default])
x11(width=6, height=5)
# Set 2 plot panels
par(mfrow=c(1,2))
# Balance boxplot
boxplot(formula=balance ~ default,
  col="lightgrey", main="balance", xlab="Default")
# Income boxplot
boxplot(formula=income ~ default,
  col="lightgrey", main="income", xlab="Default")
# Fit logistic regression model
logmod <- glm(default ~ balance, family=binomial(logit))
class(logmod)
summary(logmod)
x11(width=6, height=5)
par(mar=c(4, 4, 2, 2), oma=c(0, 0, 0, 0), mgp=c(2.5, 1, 0))
plot(x=balance, y=default,
     main="Logistic Regression of Credit Defaults",
     col="orange", xlab="credit balance", ylab="defaults")
ordern <- order(balance)
lines(x=balance[ordern], y=logmod$fitted.values[ordern], col="blue", lwd=3)
legend(x="topleft", inset=0.1, bty="n", lwd=6, y.intersp=0.4,
 legend=c("defaults", "logit fitted values"),
 col=c("orange", "blue"), lty=c(NA, 1), pch=c(1, NA))
# Calculate the cumulative defaults
sumd <- sum(default)
defaultv <- sapply(balance, function(balv) {
    sum(default[balance <= balv])
})  # end sapply
# Perform logit regression
logmod <- glm(cbind(defaultv, sumd-defaultv) ~ balance,
  family=binomial(logit))
summary(logmod)
plot(x=balance, y=defaultv/sumd, col="orange", lwd=1,
     main="Cumulative Defaults Versus Balance",
     xlab="credit balance", ylab="cumulative defaults")
ordern <- order(balance)
lines(x=balance[ordern], y=logmod$fitted.values[ordern],
col="blue", lwd=3)
legend(x="topleft", inset=0.1, bty="n", y.intersp=0.4,
 legend=c("cumulative defaults", "fitted values"),
 col=c("orange", "blue"), lty=c(NA, 1), pch=c(1, NA), lwd=6)
# Fit multifactor logistic regression model
colnamev <- colnames(Default)
formulav <- as.formula(paste(colnamev[1],
  paste(colnamev[-1], collapse="+"), sep=" ~ "))
formulav
logmod <- glm(formulav, data=Default, family=binomial(logit))
summary(logmod)
# Fit single-factor logistic model with student as predictor
logmodstud <- glm(default ~ student, family=binomial(logit))
summary(logmodstud)
# Multifactor coefficient is negative
logmod$coefficients
# Single-factor coefficient is positive
logmodstud$coefficients
# Calculate the cumulative defaults
defcum <- sapply(balance, function(balv) {
c(student=sum(default[student & (balance <= balv)]),
  non_student=sum(default[!student & (balance <= balv)]))
})  # end sapply
deftotal <- c(student=sum(student & default),
      student=sum(!student & default))
defcum <- t(defcum / deftotal)
# Plot cumulative defaults
par(mfrow=c(1,2))  # Set plot panels
ordern <- order(balance)
plot(x=balance[ordern], y=defcum[ordern, 1],
     col="red", t="l", lwd=2, xlab="credit balance", ylab="",
     main="Cumulative defaults of\n students and non-students")
lines(x=balance[ordern], y=defcum[ordern, 2], col="blue", lwd=2)
legend(x="topleft", bty="n", y.intersp=0.4,
 legend=c("students", "non-students"),
 col=c("red", "blue"), text.col=c("red", "blue"), lwd=3)
# Balance boxplot for student factor
boxplot(formula=balance ~ !student,
  col="lightgrey", main="balance", xlab="Student")
# Fit logistic regression model
logmod <- glm(default ~ student, family=binomial(logit))
summary(logmod)
x11(width=6, height=5)
par(mfrow=c(1,2))  # Set plot panels
# Balance boxplot
boxplot(formula=balance ~ default,
  col="lightgrey", main="balance", xlab="Default")
# Plot data points for non-students
x11(width=6, height=5)
xlim <- range(balance); ylim <- range(income)
plot(income ~ balance,
     main="Default Dataset from Package ISLR",
     xlim=xlim, ylim=ylim, pch=4, col="blue",
     data=Default[!student, ])
# Plot data points for students
points(income ~ balance, pch=4, lwd=2, col="red",
 data=Default[student, ])
# Add legend
legend(x="topright", bty="n", y.intersp=0.4,
 legend=c("non-students", "students"),
 col=c("blue", "red"), lty=1, lwd=6, pch=4)
# Perform in-sample forecast from logistic regression model
fcast <- predict(logmod, type="response")
all.equal(logmod$fitted.values, fcast)
# Define discrimination threshold value
threshv <- 0.7
# Calculate the confusion matrix in-sample
table(actual=!default, forecast=(fcast < threshv))
# Fit logistic regression over training data
# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
nrows <- NROW(Default)
samplev <- sample.int(n=nrows, size=nrows/2)
trainset <- Default[samplev, ]
logmod <- glm(formulav, data=trainset, family=binomial(logit))
# Forecast over test data out-of-sample
testset <- Default[-samplev, ]
fcast <- predict(logmod, newdata=testset, type="response")
# Calculate the confusion matrix out-of-sample
table(actual=!testset$default, forecast=(fcast < threshv))
# Calculate the confusion matrix out-of-sample
confmat <- table(actual=!testset$default, 
forecast=(fcast < threshv))
confmat
# Calculate the FALSE positive (type I error)
sum(!testset$default & (fcast < threshv))
# Calculate the FALSE negative (type II error)
sum(testset$default & (fcast > threshv))
# Calculate the FALSE positive and FALSE negative rates
confmat <- confmat / rowSums(confmat)
c(typeI=confmat[2, 1], typeII=confmat[1, 2])
detach(Default)
# Below is an unsuccessful attempt to draw confusion matrix using xtable
confusion_matrix <- matrix(c("| true positive \\\\ (sensitivity)", "| false negative \\\\ (type II error)", "| false positive \\\\ (type I error)", "| true negative \\\\ (specificity)"), nc=2)
dimnames(confusion_matrix) <- list(forecast=c("FALSE", "TRUE"),
                             actual=c("FALSE", "TRUE"))
print(xtable::xtable(confusion_matrix,
caption="Confusion Matrix"),
caption.placement="top",
comment=FALSE, size="scriptsize",
include.rownames=TRUE,
include.colnames=TRUE)
# end unsuccessful attempt to draw confusion table using xtable
# Confusion matrix as function of threshold
confun <- function(actualv, fcast, threshv) {
    confmat <- table(actualv, (fcast < threshv))
    confmat <- confmat / rowSums(confmat)
    c(typeI=confmat[2, 1], typeII=confmat[1, 2])
  }  # end confun
confun(!testset$default, fcast, threshv=threshv)
# Define vector of discrimination thresholds
threshv <- seq(0.05, 0.95, by=0.05)^2
# Calculate the error rates
errorr <- sapply(threshv, confun,
  actualv=!testset$default, fcast=fcast)  # end sapply
errorr <- t(errorr)
rownames(errorr) <- threshv
errorr <- rbind(c(1, 0), errorr)
errorr <- rbind(errorr, c(0, 1))
# Calculate the area under ROC curve (AUC)
truepos <- (1 - errorr[, "typeII"])
truepos <- (truepos + rutils::lagit(truepos))/2
falsepos <- rutils::diffit(errorr[, "typeI"])
abs(sum(truepos*falsepos))
# Plot ROC Curve for Defaults
x11(width=5, height=5)
plot(x=errorr[, "typeI"], y=1-errorr[, "typeII"],
     xlab="FALSE positive rate", ylab="TRUE positive rate",
     main="ROC Curve for Defaults", type="l", lwd=3, col="blue")
abline(a=0.0, b=1.0, lwd=3, col="orange")
# Calculate the EMA VTI variance using compiled C++ function
lookb <- 51
weightv <- exp(-0.1*1:lookb)
weightv <- weightv/sum(weightv)
varv <- .Call(stats:::C_cfilter, retp^2, filter=weightv, sides=1, circular=FALSE)
varv[1:(lookb-1)] <- varv[lookb]
# Plot EMA volatility
varv <- xts:::xts(sqrt(varv), order.by=zoo::index(retp))
dygraphs::dygraph(varv, main="VTI EMA Volatility")
quantmod::chart_Series(varv, name="VTI EMA Volatility")
# Define Brownian motion prices
# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
nrows <- 100
pricev <- cumsum(rnorm(nrows))
# Calculate the observed values
prico <- pricev + rnorm(nrows)
# Plot the state space model
pricev <- cbind(pricev, prico)
colnames(pricev) <- c("price", "observed")
matplot(y=pricev, main="State Space Model",
  xlab="Time", ylab="Price",
  type="l", lty="solid", lwd=2, col=c("blue", "red"))
legend(x="topright", legend=colnames(pricev),
 inset=0.1, cex=1.0, bg="white", bty="n", y.intersp=0.4,
 lwd=6, lty=1, col=c("blue", "red"))
# Initialize the estimate variance and Kalman gain
nrows <- 10
vareta <- 1; vareps <- 1
varv <- numeric(nrows); varv[1] <- 1
kgain <- numeric(nrows); kgain[1] <- 1
# Update the variance and Kalman gain
for (it in 2:nrows) {
  varv[it] <- (1-kgain[it-1])*(varv[it-1] + vareta)
  kgain[it] <- (varv[it] + vareta)/(varv[it] + vareta + vareps)
}  # end for
# Plot the variance and Kalman gain
datav <- cbind(varv, kgain)
colnames(datav) <- c("variance", "Kalman gain")
matplot(y=datav, main="Estimate Variance and Kalman gain",
  xlab="Time", ylab="Data",
  type="l", lty="solid", lwd=2, col=c("blue", "red"))
legend(x="topright", legend=colnames(datav),
 inset=0.1, cex=1.0, bg="white", bty="n", y.intersp=0.4,
 lwd=6, lty=1, col=c("blue", "red"))
# Define Kalman gain function
kfun <- function(volr) 1/(0.5 + sqrt(0.25 + volr^2))
# Plot Kalman gain
curve(expr=kfun, xlim=c(0.01, 0.99),
  main="Kalman Gain as Function Volatility Ratio",
  xlab="Vol ratio", ylab="Kalman gain", col="blue", lwd=2)
# Define Brownian motion prices
# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
nrows <- 100
pricev <- cumsum(rnorm(nrows))
# Calculate the observed values
prico <- pricev + rnorm(nrows)
# Initialize the price estimates
priceb <- numeric(nrows)
priceb[1] <- prico[1]
# Update the price estimates
kgain <- (-1+sqrt(5))/2
for (it in 2:nrows) {
  priceb[it] <- priceb[it-1] + kgain*(prico[it] - priceb[it-1])
}  # end for
# Plot the Kalman filter
pricev <- cbind(pricev, prico, priceb)
colnames(pricev) <- c("price", "observed", "filtered")
matplot(y=pricev, main="Kalman Filter Prices",
  xlab="Time", ylab="Price",
  type="l", lty="solid", lwd=2, col=c("blue", "green", "red"))
legend(x="topright", legend=colnames(pricev),
 inset=0.1, cex=1.0, bg="white", bty="n", y.intersp=0.4,
 lwd=6, lty=1, col=c("blue", "green", "red"))
# Initialize the price estimates
pricema <- numeric(nrows)
pricema[1] <- prico[1]
# Calculate the EMA prices in R
for (it in 2:nrows) {
  pricema[it] <- (1-kgain)*pricema[it-1] + kgain*prico[it]
}  # end for
all.equal(pricema, priceb)
# Calculate the EMA prices using RcppArmadillo C++
pricpp <- HighFreq::run_mean(matrix(prico), 1-kgain)
all.equal(drop(pricpp), priceb)
# Compare the speed of RcppArmadillo C++ with R code
library(microbenchmark)
summary(microbenchmark(
  rcode={for (it in 2:nrows) {
    pricema[it] <- (1-kgain)*pricema[it-1] + kgain*prico[it]
  }},
  cpp=HighFreq::run_mean(matrix(prico), 1-kgain),
  times=10))[, c(1, 4, 5)]
NA
NA
NA
