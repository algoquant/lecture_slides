# Create a random real symmetric matrix
matrixv <- matrix(runif(25), nc=5)
matrixv <- matrixv + t(matrixv)
# Calculate the eigenvalues and eigenvectors
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
# Create a random positive semi-definite matrix
matrixv <- matrix(runif(25), nc=5)
matrixv <- t(matrixv) %*% matrixv
# Calculate the eigenvalues and eigenvectors
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
nrows <- 6 ; ncols <- 4
# Calculate left matrix
leftmat <- matrix(runif(nrows^2), nc=nrows)
eigend <- eigen(crossprod(leftmat))
leftmat <- eigend$vectors[, 1:ncols]
# Calculate right matrix and singular values
rightmat <- matrix(runif(ncols^2), nc=ncols)
eigend <- eigen(crossprod(rightmat))
rightmat <- eigend$vectors
singval <- sort(runif(ncols, min=1, max=5), decreasing=TRUE)
# Compose rectangular matrix
matrixv <- leftmat %*% (singval * t(rightmat))
# Perform singular value decomposition
svdec <- svd(matrixv)
# Recompose matrixv from SVD
all.equal(matrixv, svdec$u %*% (svdec$d*t(svdec$v)))
# Compare SVD with matrixv components
all.equal(abs(svdec$u), abs(leftmat))
all.equal(abs(svdec$v), abs(rightmat))
all.equal(svdec$d, singval)
# Eigen decomposition of matrixv squared
retsq <- matrixv %*% t(matrixv)
eigend <- eigen(retsq)
all.equal(eigend$values[1:ncols], singval^2)
all.equal(abs(eigend$vectors[, 1:ncols]), abs(leftmat))
# Eigen decomposition of matrixv squared
retsq <- t(matrixv) %*% matrixv
eigend <- eigen(retsq)
all.equal(eigend$values, singval^2)
all.equal(abs(eigend$vectors), abs(rightmat))
# Create a random positive semi-definite matrix
matrixv <- matrix(runif(25), nc=5)
matrixv <- t(matrixv) %*% matrixv
# Calculate the inverse of matrixv
invmat <- solve(a=matrixv)
# Multiply inverse with matrix
round(invmat %*% matrixv, 4)
round(matrixv %*% invmat, 4)
# Calculate the eigenvalues and eigenvectors
eigend <- eigen(matrixv)
eigenvec <- eigend$vectors
# Calculate inverse from eigen decomposition
inveigen <- eigenvec %*% (t(eigenvec) / eigend$values)
all.equal(invmat, inveigen)
# Decompose diagonal matrix with inverse of eigenvalues
# diagmat <- diag(1/eigend$values)
# inveigen <- eigenvec %*% (diagmat %*% t(eigenvec))
# Random rectangular matrix: nrows > ncols
nrows <- 6 ; ncols <- 4
matrixv <- matrix(runif(nrows*ncols), nc=ncols)
# Calculate generalized inverse of matrixv
invmat <- MASS::ginv(matrixv)
round(invmat %*% matrixv, 4)
all.equal(matrixv, matrixv %*% invmat %*% matrixv)
# Random rectangular matrix: nrows < ncols
nrows <- 4 ; ncols <- 6
matrixv <- matrix(runif(nrows*ncols), nc=ncols)
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
# Create a random singular matrix
# More columns than rows: ncols > nrows
nrows <- 4 ; ncols <- 6
matrixv <- matrix(runif(nrows*ncols), nc=ncols)
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
notzero <- (svdec$d > (precv*svdec$d[1]))
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
# Create a random matrix
matrixv <- matrix(rnorm(100), nc=10)
# Calculate the matrix inverse using solve()
invmatr <- solve(a=matrixv)
round(invmatr %*% matrixv, 4)
# Compile the C++ file using Rcpp
Rcpp::sourceCpp(file="/Users/jerzy/Develop/Rcpp/test_fun.cpp")
# Calculate the matrix inverse using C++
invmat <- calc_invmat(matrixv)
all.equal(invmat, invmatr)
# Compare the speed of RcppArmadillo with R code
library(microbenchmark)
summary(microbenchmark(
  ginv=MASS::ginv(matrixv),
  solve=solve(matrixv),
  cpp=calc_invmat(matrixv),
  times=10))[, c(1, 4, 5)]
# Create large random positive semi-definite matrix
matrixv <- matrix(runif(1e4), nc=100)
matrixv <- t(matrixv) %*% matrixv
# Calculate the eigen decomposition
eigend <- eigen(matrixv)
eigenval <- eigend$values
eigenvec <- eigend$vectors
# Set tolerance for determining zero singular values
precv <- sqrt(.Machine$double.eps)
# If needed convert to positive definite matrix
notzero <- (eigenval > (precv*eigenval[1]))
if (sum(!notzero) > 0) {
  eigenval[!notzero] <- 2*precv
  matrixv <- eigenvec %*% (eigenval * t(eigenvec))
}  # end if
# Calculate the Cholesky matrixv
cholmat <- chol(matrixv)
cholmat[1:5, 1:5]
all.equal(matrixv, t(cholmat) %*% cholmat)
# Calculate inverse from Cholesky
invchol <- chol2inv(cholmat)
all.equal(solve(matrixv), invchol)
# Compare speed of Cholesky inversion
library(microbenchmark)
summary(microbenchmark(
  solve=solve(matrixv),
  cholmat=chol2inv(chol(matrixv)),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Calculate random covariance matrix
covmat <- matrix(runif(25), nc=5)
covmat <- t(covmat) %*% covmat
# Calculate the Cholesky matrix
cholmat <- chol(covmat)
cholmat
# Simulate random uncorrelated returns
nassets <- 5
nrows <- 10000
retp <- matrix(rnorm(nassets*nrows), nc=nassets)
# Calculate correlated returns by applying Cholesky
retscorr <- retp %*% cholmat
# Calculate covariance matrix
covmat2 <- crossprod(retscorr) /(nrows-1)
all.equal(covmat, covmat2)
# Simulate random stock returns
nassets <- 10
nrows <- 100
set.seed(1121)  # Initialize random number generator
retp <- matrix(rnorm(nassets*nrows), nc=nassets)
# Calculate de-meaned returns matrix
retp <- t(t(retp) - colMeans(retp))
# Or
retp <- apply(retp, MARGIN=2, function(x) (x-mean(x)))
# Calculate covariance matrix
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
matrixv <- matrix(rnorm(10*8), nc=10)
# Calculate covariance matrix
covmat <- cov(matrixv)
# Calculate inverse of covmat - error
invmat <- solve(covmat)
# Calculate regularized inverse of covmat
invmat <- MASS::ginv(covmat)
# Verify inverse property of matrixv
all.equal(covmat, covmat %*% invmat %*% covmat)
# Perform eigen decomposition
eigend <- eigen(covmat)
eigenvec <- eigend$vectors
eigenval <- eigend$values
# Set tolerance for determining zero singular values
precv <- sqrt(.Machine$double.eps)
# Calculate regularized inverse matrix
notzero <- (eigenval > (precv * eigenval[1]))
invreg <- eigenvec[, notzero] %*%
  (t(eigenvec[, notzero]) / eigenval[notzero])
# Verify that invmat is same as invreg
all.equal(invmat, invreg)
# Calculate regularized inverse matrix using cutoff
dimax <- 3
invmat <- eigenvec[, 1:dimax] %*%
  (t(eigenvec[, 1:dimax]) / eigend$values[1:dimax])
# Verify that invmat is same as invreg
all.equal(invmat, invreg)
# Create a random covariance matrix
set.seed(1121)
matrixv <- matrix(rnorm(5e2), nc=5)
covmat <- cov(matrixv)
cormat <- cor(matrixv)
stdev <- sqrt(diag(covmat))
# Calculate target matrix
cormean <- mean(cormat[upper.tri(cormat)])
targetmat <- matrix(cormean, nr=NROW(covmat), nc=NCOL(covmat))
diag(targetmat) <- 1
targetmat <- t(t(targetmat * stdev) * stdev)
# Calculate shrinkage covariance matrix
alpha <- 0.5
covshrink <- (1-alpha)*covmat + alpha*targetmat
# Calculate inverse matrix
invmat <- solve(covshrink)
# Create a random matrix
matrixv <- matrix(rnorm(100), nc=10)
# Calculate the inverse of matrixv
invmat <- solve(a=matrixv)
# Multiply inverse with matrix
round(invmat %*% matrixv, 4)
# Calculate the initial inverse
invmatr <- invmat + matrix(rnorm(100, sd=0.1), nc=10)
# Calculate the approximate recursive inverse of matrixv
invmatr <- (2*invmatr - invmatr %*% matrixv %*% invmatr)
# Calculate the sum of the off-diagonal elements
sum((invmatr %*% matrixv)[upper.tri(matrixv)])
# Calculate the recursive inverse of matrixv in a loop
invmatr <- invmat + matrix(rnorm(100, sd=0.1), nc=10)
iterv <- sapply(1:5, function(x) {
# Calculate the recursive inverse of matrixv
  invmatr <<- (2*invmatr - invmatr %*% matrixv %*% invmatr)
# Calculate the sum of the off-diagonal elements
  sum((invmatr %*% matrixv)[upper.tri(matrixv)])
})  # end sapply
# Plot the iterations
plot(x=1:5, y=iterv, t="l", xlab="iterations", ylab="error",
     main="Iterations of Recursive Matrix Inverse")
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
rastrigin <- function(vectorv, param=25) {
  sum(vectorv^2 - param*cos(vectorv))
}  # end rastrigin
vectorv <- c(pi/6, pi/6)
rastrigin(vectorv=vectorv)
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
# Verify that Rtools or XCode are working properly:
devtools::find_rtools()  # Under Windows
devtools::has_devel()
# Install the packages Rcpp and RcppArmadillo
install.packages(c("Rcpp", "RcppArmadillo"))
# Load package Rcpp
library(Rcpp)
# Get documentation for package Rcpp
# Get short description
packageDescription("Rcpp")
# Load help page
help(package="Rcpp")
# List all datasets in "Rcpp"
data(package="Rcpp")
# List all objects in "Rcpp"
ls("package:Rcpp")
# Remove Rcpp from search path
detach("package:Rcpp")
# Define Rcpp function
Rcpp::cppFunction("
  int times_two(int x)
    { return 2 * x;}
  ")  # end cppFunction
# Run Rcpp function
times_two(3)
# Source Rcpp functions from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/mult_rcpp.cpp")
# Multiply two numbers
mult_rcpp(2, 3)
mult_rcpp(1:3, 6:4)
# Multiply two vectors
mult_vec_rcpp(2, 3)
mult_vec_rcpp(1:3, 6:4)
# Define Rcpp function with loop
Rcpp::cppFunction("
double inner_mult(NumericVector x, NumericVector y) {
int xsize = x.size();
int ysize = y.size();
if (xsize != ysize) {
    return 0;
  } else {
    double total = 0;
    for(int i = 0; i < xsize; ++i) {
total += x[i] * y[i];
  }
  return total;
  }
}")  # end cppFunction
# Run Rcpp function
inner_mult(1:3, 6:4)
inner_mult(1:3, 6:3)
# Define Rcpp Sugar function with loop
Rcpp::cppFunction("
double inner_sugar(NumericVector x, NumericVector y) {
  return sum(x * y);
}")  # end cppFunction
# Run Rcpp Sugar function
inner_sugar(1:3, 6:4)
inner_sugar(1:3, 6:3)
# Define R function with loop
inner_multr <- function(x, y) {
    sumv <- 0
    for(i in 1:NROW(x)) {
sumv <- sumv + x[i] * y[i]
    }
    sumv
}  # end inner_multr
# Run R function
inner_multr(1:3, 6:4)
inner_multr(1:3, 6:3)
# Compare speed of Rcpp and R
library(microbenchmark)
summary(microbenchmark(
  rcode=inner_multr(1:10000, 1:10000),
  innerp=1:10000 %*% 1:10000,
  Rcpp=inner_mult(1:10000, 1:10000),
  sugar=inner_sugar(1:10000, 1:10000),
  times=10))[, c(1, 4, 5)]
# Define Ornstein-Uhlenbeck function in R
sim_our <- function(nrows=1000, eq_price=5.0,
              volat=0.01, theta=0.01) {
  retp <- numeric(nrows)
  pricev <- numeric(nrows)
  pricev[1] <- eq_price
  for (i in 2:nrows) {
    retp[i] <- theta*(eq_price - pricev[i-1]) + volat*rnorm(1)
    pricev[i] <- pricev[i-1] + retp[i]
  }  # end for
  pricev
}  # end sim_our
# Simulate Ornstein-Uhlenbeck process in R
eq_price <- 5.0; sigmav <- 0.01
thetav <- 0.01; nrows <- 1000
set.seed(1121)  # Reset random numbers
ousim <- sim_our(nrows, eq_price=eq_price, volat=sigmav, theta=thetav)
# Define Ornstein-Uhlenbeck function in Rcpp
Rcpp::cppFunction("
NumericVector sim_oucpp(double eq_price,
                    double volat,
                    double thetav,
                    NumericVector innov) {
  int nrows = innov.size();
  NumericVector prices(nrows);
  NumericVector returns(nrows);
  pricev[0] = eq_price;
  for (int it = 1; it < nrows; it++) {
    returns[it] = thetav*(eq_price - pricev[it-1]) + volat*innov[it-1];
    pricev[it] = pricev[it-1] + returns[it];
  }  // end for
  return prices;
}")  # end cppFunction
# Simulate Ornstein-Uhlenbeck process in Rcpp
set.seed(1121)  # Reset random numbers
oucpp <- sim_oucpp(eq_price=eq_price,
  volat=sigmav, theta=thetav, innov=rnorm(nrows))
all.equal(ousim, oucpp)
# Compare speed of Rcpp and R
library(microbenchmark)
summary(microbenchmark(
  rcode=sim_our(nrows, eq_price=eq_price, volat=sigmav, theta=thetav),
  Rcpp=sim_oucpp(eq_price=eq_price, volat=sigmav, theta=thetav, innov=rnorm(nrows)),
  times=10))[, c(1, 4, 5)]
# Source Rcpp function for Ornstein-Uhlenbeck process from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/sim_ou.cpp")
# Simulate Ornstein-Uhlenbeck process in Rcpp
set.seed(1121)  # Reset random numbers
oucpp <- sim_oucpp(eq_price=eq_price,
  volat=sigmav,
  theta=thetav,
  innov=rnorm(nrows))
all.equal(ousim, oucpp)
# Compare speed of Rcpp and R
library(microbenchmark)
summary(microbenchmark(
  rcode=sim_our(nrows, eq_price=eq_price, volat=sigmav, theta=thetav),
  Rcpp=sim_oucpp(eq_price=eq_price, volat=sigmav, theta=thetav, innov=rnorm(nrows)),
  times=10))[, c(1, 4, 5)]
# Calculate uniformly distributed pseudo-random sequence
unifun <- function(seedv, nrows=10) {
  output <- numeric(nrows)
  output[1] <- seedv
  for (i in 2:nrows) {
    output[i] <- 4*output[i-1]*(1-output[i-1])
  }  # end for
  acos(1-2*output)/pi
}  # end unifun
# Source Rcpp functions from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/unifun.cpp")
# Microbenchmark Rcpp code
library(microbenchmark)
summary(microbenchmark(
  rcode=runif(1e5),
  rloop=unifun(0.3, 1e5),
  Rcpp=unifuncpp(0.3, 1e5),
  times=10))[, c(1, 4, 5)]
library(RcppArmadillo)
# Source Rcpp functions from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/armadillo_functions.cpp")
vec1 <- runif(1e5)
vec2 <- runif(1e5)
inner_vec(vec1, vec2)
vec1 %*% vec2
# Microbenchmark \emph{RcppArmadillo} code
summary(microbenchmark(
  rcpp = inner_vec(vec1, vec2),
  rcode = (vec1 %*% vec2),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
# Microbenchmark shows:
# inner_vec() is several times faster than %*%, especially for longer vectors.
#     expr     mean   median
# 1 inner_vec 110.7067 110.4530
# 2 rcode 585.5127 591.3575
# Source Rcpp functions from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/sim_arima.cpp")
# Define AR(2) coefficients
coeff <- c(0.9, 0.09)
nrows <- 1e4
set.seed(1121)
innov <- rnorm(nrows)
# Simulate ARIMA using filter()
arimar <- filter(x=innov, filter=coeff, method="recursive")
# Simulate ARIMA using sim_ar()
innov <- matrix(innov)
coeff <- matrix(coeff)
arimav <- sim_ar(coeff, innov)
all.equal(drop(arimav), as.numeric(arimar))
# Microbenchmark \emph{RcppArmadillo} code
summary(microbenchmark(
  rcpp = sim_ar(coeff, innov),
  filter = filter(x=innov, filter=coeff, method="recursive"),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
library(RcppArmadillo)
# Source Rcpp functions from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/armadillo_functions.cpp")
matrixv <- matrix(runif(1e5), nc=1e3)
# De-mean matrix columns using apply()
matd <- apply(matrixv, 2, function(x) (x-mean(x)))
# De-mean matrix columns in place using Rcpp demeanr()
demeanr(matrixv)
all.equal(matd, matrixv)
# Microbenchmark \emph{RcppArmadillo} code
library(microbenchmark)
summary(microbenchmark(
  rcode = (apply(matrixv, 2, mean)),
  rcpp = demeanr(matrixv),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
# Perform matrix inversion
# Create random positive semi-definite matrix
matrixv <- matrix(runif(25), nc=5)
matrixv <- t(matrixv) %*% matrixv
# Invert the matrix
matrixinv <- solve(matrixv)
inv_mat(matrixv)
all.equal(matrixinv, matrixv)
# Microbenchmark \emph{RcppArmadillo} code
summary(microbenchmark(
  rcode = solve(matrixv),
  rcpp = inv_mat(matrixv),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
library(RcppArmadillo)
# Source Rcpp functions from file
Rcpp::sourceCpp("/Users/jerzy/Develop/lecture_slides/scripts/HighFreq.cpp")
# Calculate matrix of random returns
matrixv <- matrix(rnorm(300), nc=5)
# Regularized inverse of correlation matrix
dimax <- 4
cormat <- cor(matrixv)
eigend <- eigen(cormat)
invmat <- eigend$vectors[, 1:dimax] %*%
  (t(eigend$vectors[, 1:dimax]) / eigend$values[1:dimax])
# Regularized inverse using \emph{RcppArmadillo}
invarma <- calc_inv(cormat, dimax=dimax)
all.equal(invmat, invarma)
# Microbenchmark \emph{RcppArmadillo} code
library(microbenchmark)
summary(microbenchmark(
  rcode = {eigend <- eigen(cormat)
eigend$vectors[, 1:dimax] %*% (t(eigend$vectors[, 1:dimax]) / eigend$values[1:dimax])},
  rcpp = calc_inv(cormat, dimax=dimax),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
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
matplot(rates, main="Yield Curves in 2020 and 2023", xaxt="n", lwd=3, lty=1,
  type="l", xlab="maturity", ylab="yield", col=colorv)
# Add x-axis
axis(1, seq_along(rownames(rates)), rownames(rates))
# Add legend
legend("topleft", legend=colnames(rates), y.intersp=0.2,
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
legend("topleft", legend=colnames(rates), y.intersp=0.1,
 bty="n", col=colorv, lty=1, lwd=4, inset=0.05, cex=0.8)
# Alternative plot using matplot()
matplot(rates, main="Yield curve since 2006", xaxt="n", lwd=3, lty=1,
  type="l", xlab="maturity", ylab="yield", col=colorv)
# Add x-axis
axis(1, seq_along(rownames(rates)), rownames(rates))
# Add legend
legend("topleft", legend=colnames(rates), y.intersp=0.5,
 bty="n", col=colorv, lty=1, lwd=4, inset=0.05, cex=0.8)
# Extract rates from ratesenv
symbolv <- c("DGS1", "DGS2", "DGS5", "DGS10", "DGS20")
rates <- mget(symbolv, envir=ratesenv)
rates <- rutils::do_call(cbind, rates)
rates <- zoo::na.locf(rates, na.rm=FALSE)
rates <- zoo::na.locf(rates, fromLast=TRUE)
rates <- na.omit(rates)
# Calculate daily percentage rates changes
retp <- rutils::diffit(log(rates))
# De-mean the returns
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
# Standardize (de-mean and scale) the returns
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
retspca <- retp %*% pcad$rotation
solved <- retspca %*% solve(pcad$rotation)
all.equal(coredata(retp), solved)
# Invert first 3 principal component time series
solved <- retspca[, 1:3] %*% solve(pcad$rotation)[1:3, ]
solved <- xts::xts(solved, zoo::index(retp))
solved <- cumsum(solved)
retc <- cumsum(retp)
# Plot the solved returns
par(mfrow=c(3,2))
par(mar=c(2, 2, 0, 1), oma=c(0, 0, 0, 0))
for (symbol in symbolv) {
  plot.zoo(cbind(retc[, symbol], solved[, symbol]),
    plot.type="single", col=c("black", "blue"), xlab="", ylab="")
  legend(x="topleft", bty="n", y.intersp=0.5,
   legend=paste0(symbol, c("", " solved")),
   title=NULL, inset=0.0, cex=1.0, lwd=6,
   lty=1, col=c("black", "blue"))
}  # end for
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
# Calculate de-meaned predictor and response vectors
predzm <- predm - mean(predm)
respzm <- respv - mean(respv)
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
fittedv <- (alpha + betav*predm)
all.equal(fittedv, regmod$fitted.values, check.attributes=FALSE)
x11(width=5, height=4)  # Open x11 for plotting
# Set plot parameters to reduce whitespace around plot
par(mar=c(5, 5, 2, 1), oma=c(0, 0, 0, 0))
# Plot scatterplot using formula
plot(formulav, xlab="predictor", ylab="response")
title(main="Simple Regression", line=0.5)
# Add regression line
abline(regmod, lwd=3, col="blue")
# Plot fitted (predicted) response values
points(x=predm, y=regmod$fitted.values, pch=16, col="blue")
# Plot response without noise
lines(x=predm, y=(respv-noisev), col="red", lwd=3)
legend(x="topleft", # Add legend
       legend=c("response without noise", "fitted values"),
       title=NULL, inset=0.01, cex=1.0, lwd=6, y.intersp=0.4,
       bty="n", lty=1, col=c("red", "blue"))
# Calculate the residuals
fittedv <- (alpha + betav*predm)
resids <- (respv - fittedv)
all.equal(resids, regmod$residuals, check.attributes=FALSE)
# Residuals are orthogonal to the predictor
all.equal(sum(resids*predm), target=0)
# Residuals are orthogonal to the fitted values
all.equal(sum(resids*fittedv), target=0)
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
betasd <- residsd/sqrt(sum(predzm^2))
# Standard error of alpha
alphasd <- residsd*sqrt(1/nrows + mean(predm)^2/sum(predzm^2))
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
  respv <- (1 + 0.2*predm +
  rnorm(NROW(predm), sd=stdev))
# Specify regression formula
  formulav <- respv ~ predm
# Perform regression and get summary
  regsum <- summary(lm(formulav))
# Extract regression statistics
  with(regsum, c(pval=coefficients[2, 4],
   adj_rsquared=adj.r.squared,
   fstat=fstatistic[1]))
}  # end regstats
# Apply regstats() to vector of std dev values
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
# Apply regstats() to vector of std dev values
vecsd <- seq(from=0.1, to=0.5, by=0.1)
names(vecsd) <- paste0("sd=", vecsd)
statsmat <- t(sapply(vecsd, function(stdev) {
    set.seed(1121)  # initialize number generator
# Define explanatory (predm) and response variables
    predm <- rnorm(100, mean=2)
    respv <- (1 + 0.2*predm +
rnorm(NROW(predm), sd=stdev))
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
