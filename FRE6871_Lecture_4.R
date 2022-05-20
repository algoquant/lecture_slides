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
de_comp <- eigenvec %*% (eigend$values * t(eigenvec))
# Create diagonal matrix of eigenvalues
# diago_nal <- diag(eigend$values)
# de_comp <- eigenvec %*% (diago_nal %*% t(eigenvec))
all.equal(matrixv, de_comp)

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
square_d <- matrixv %*% t(matrixv)
eigend <- eigen(square_d)
all.equal(eigend$values[1:nright], sing_values^2)
all.equal(abs(eigend$vectors[, 1:nright]), abs(leftmat))
# Eigen decomposition of matrixv squared
square_d <- t(matrixv) %*% matrixv
eigend <- eigen(square_d)
all.equal(eigend$values, sing_values^2)
all.equal(abs(eigend$vectors), abs(rightmat))

# Create random positive semi-definite matrix
matrixv <- matrix(runif(25), nc=5)
matrixv <- t(matrixv) %*% matrixv
# Calculate the inverse of matrixv
inverse <- solve(a=matrixv)
# Multiply inverse with matrix
round(inverse %*% matrixv, 4)
round(matrixv %*% inverse, 4)

# Calculate eigenvectors and eigenvalues
eigend <- eigen(matrixv)
eigenvec <- eigend$vectors

# Perform eigen decomposition of inverse
eigen_inverse <- eigenvec %*% (t(eigenvec) / eigend$values)
all.equal(inverse, eigen_inverse)
# Decompose diagonal matrix with inverse of eigenvalues
# diago_nal <- diag(1/eigend$values)
# eigen_inverse <-
#   eigenvec %*% (diago_nal %*% t(eigenvec))

# Random rectangular matrix: nleft > nright
nleft <- 6 ; nright <- 4
matrixv <- matrix(runif(nleft*nright), nc=nright)
# Calculate generalized inverse of matrixv
inverse <- MASS::ginv(matrixv)
round(inverse %*% matrixv, 4)
all.equal(matrixv, matrixv %*% inverse %*% matrixv)
# Random rectangular matrix: nleft < nright
nleft <- 4 ; nright <- 6
matrixv <- matrix(runif(nleft*nright), nc=nright)
# Calculate generalized inverse of matrixv
inverse <- MASS::ginv(matrixv)
all.equal(matrixv, matrixv %*% inverse %*% matrixv)
round(matrixv %*% inverse, 4)
round(inverse %*% matrixv, 4)
# Perform singular value decomposition
svdec <- svd(matrixv)
# Calculate generalized inverse from SVD
svd_inverse <- svdec$v %*% (t(svdec$u) / svdec$d)
all.equal(svd_inverse, inverse)
# Calculate Moore-Penrose pseudo-inverse
invmp <- MASS::ginv(t(matrixv) %*% matrixv) %*% t(matrixv)
all.equal(invmp, inverse)

# Create random singular matrix
# More columns than rows: nright > nleft
nleft <- 4 ; nright <- 6
matrixv <- matrix(runif(nleft*nright), nc=nright)
matrixv <- t(matrixv) %*% matrixv
# Perform singular value decomposition
svdec <- svd(matrixv)
# Incorrect inverse from SVD because of zero singular values
svd_inverse <- svdec$v %*% (t(svdec$u) / svdec$d)
# Inverse property doesn't hold
all.equal(matrixv, matrixv %*% svd_inverse %*% matrixv)

# Set tolerance for determining zero singular values
precision <- sqrt(.Machine$double.eps)
# Check for zero singular values
round(svdec$d, 12)
not_zero <- (svdec$d > (precision * svdec$d[1]))
# Calculate regularized inverse from SVD
svd_inverse <- svdec$v[, not_zero] %*%
  (t(svdec$u[, not_zero]) / svdec$d[not_zero])
# Verify inverse property of matrixv
all.equal(matrixv, matrixv %*% svd_inverse %*% matrixv)
# Calculate regularized inverse using MASS::ginv()
inverse <- MASS::ginv(matrixv)
all.equal(svd_inverse, inverse)
# Calculate Moore-Penrose pseudo-inverse
invmp <- MASS::ginv(t(matrixv) %*% matrixv) %*% t(matrixv)
all.equal(invmp, inverse)

# Diagonalize the "unit" matrix
unitmat <- matrixv %*% inverse
round(unitmat, 4)
round(matrixv %*% inverse, 4)
round(t(svdec$u) %*% unitmat %*% svdec$v, 4)

# Define a square matrix
matrixv <- matrix(c(1, 2, -1, 2), nc=2)
vectorv <- c(2, 1)
# Calculate the inverse of matrixv
inverse <- solve(a=matrixv)
inverse %*% matrixv
# Calculate solution using inverse of matrixv
solu_tion <- inverse %*% vectorv
matrixv %*% solu_tion
# Calculate solution of linear system
solu_tion <- solve(a=matrixv, b=vectorv)
matrixv %*% solu_tion

# Create large random positive semi-definite matrix
matrixv <- matrix(runif(1e4), nc=100)
matrixv <- t(matrixv) %*% matrixv
# Calculate eigen decomposition
eigend <- eigen(matrixv)
eigenval <- eigend$values
eigenvec <- eigend$vectors
# Set tolerance for determining zero singular values
precision <- sqrt(.Machine$double.eps)
# If needed convert to positive definite matrix
not_zero <- (eigenval > (precision*eigenval[1]))
if (sum(!not_zero) > 0) {
  eigenval[!not_zero] <- 2*precision
  matrixv <- eigenvec %*% (eigenval * t(eigenvec))
}  # end if
# Calculate the Cholesky matrixv
cholmat <- chol(matrixv)
cholmat[1:5, 1:5]
all.equal(matrixv, t(cholmat) %*% cholmat)
# Calculate inverse from Cholesky
chol_inverse <- chol2inv(cholmat)
all.equal(solve(matrixv), chol_inverse)
# Compare speed of Cholesky inversion
library(microbenchmark)
summary(microbenchmark(
  sol_ve=solve(matrixv),
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
returns <- matrix(rnorm(nassets*nrows), nc=nassets)
# Calculate correlated returns by applying Cholesky
corr_returns <- returns %*% cholmat
# Calculate covariance matrix
cov_returns <- crossprod(corr_returns) /(nrows-1)
all.equal(covmat, cov_returns)

# Simulate random portfolio returns
nassets <- 10
nrows <- 100
set.seed(1121)  # Initialize random number generator
returns <- matrix(rnorm(nassets*nrows), nc=nassets)
# Calculate de-meaned returns matrix
returns <- t(t(returns) - colMeans(returns))
# Or
returns <- apply(returns, MARGIN=2, function(x) (x-mean(x)))
# Calculate covariance matrix
covmat <- crossprod(returns) /(nrows-1)
# Calculate eigenvectors and eigenvalues
eigend <- eigen(covmat)
eigend$values
barplot(eigend$values, # Plot eigenvalues
  xlab="", ylab="", las=3,
  names.arg=paste0("ev", 1:NROW(eigend$values)),
  main="Eigenvalues of covariance matrix")

# Calculate eigenvectors and eigenvalues
# as function of number of returns
ndata <- ((nassets/2):(2*nassets))
e_values <- sapply(ndata, function(x) {
  returns <- returns[1:x, ]
  returns <- apply(returns, MARGIN=2, function(y) (y - mean(y)))
  covmat <- crossprod(returns) / (x-1)
  min(eigen(covmat)$values)
})  # end sapply
plot(y=e_values, x=ndata, t="l", xlab="", ylab="", lwd=3, col="blue",
  main="Smallest eigenvalue of covariance matrix
  as function of number of returns")

# Create rectangular matrix with collinear columns
matrixv <- matrix(rnorm(10*8), nc=10)
# Calculate covariance matrix
covmat <- cov(matrixv)
# Calculate inverse of covmat - error
inverse <- solve(covmat)
# Calculate regularized inverse of covmat
inverse <- MASS::ginv(covmat)
# Verify inverse property of matrixv
all.equal(covmat, covmat %*% inverse %*% covmat)
# Perform eigen decomposition
eigend <- eigen(covmat)
eigenvec <- eigend$vectors
eigenval <- eigend$values
# Set tolerance for determining zero singular values
precision <- sqrt(.Machine$double.eps)
# Calculate regularized inverse matrix
not_zero <- (eigenval > (precision * eigenval[1]))
reg_inverse <- eigenvec[, not_zero] %*%
  (t(eigenvec[, not_zero]) / eigenval[not_zero])
# Verify inverse property of matrixv
all.equal(inverse, reg_inverse)

# Calculate regularized inverse matrix using cutoff
eigen_max <- 3
inverse <- eigenvec[, 1:eigen_max] %*%
  (t(eigenvec[, 1:eigen_max]) / eigend$values[1:eigen_max])
# Verify inverse property of matrixv
all.equal(inverse, reg_inverse)

# Create random covariance matrix
set.seed(1121)
matrixv <- matrix(rnorm(5e2), nc=5)
covmat <- cov(matrixv)
cormat <- cor(matrixv)
stdev <- sqrt(diag(covmat))
# Calculate target matrix
cor_mean <- mean(cormat[upper.tri(cormat)])
targetr <- matrix(cor_mean, nr=NROW(covmat), nc=NCOL(covmat))
diag(targetr) <- 1
targetr <- t(t(targetr * stdev) * stdev)
# Calculate shrinkage covariance matrix
alpha <- 0.5
cov_shrink <- (1-alpha)*covmat + alpha*targetr
# Calculate inverse matrix
inverse <- solve(cov_shrink)

# Symbols for constant maturity Treasury rates
symbolv <- c("DGS1", "DGS2", "DGS5", "DGS10", "DGS20", "DGS30")
# Create new environment for time series
rates_env <- new.env()
# Download time series for symbolv into rates_env
quantmod::getSymbols(symbolv, env=rates_env, src="FRED")
# List files in rates_env
ls(rates_env)
# Get class of all objects in rates_env
sapply(rates_env, class)
# Get class of all objects in R workspace
sapply(ls(), function(name) class(get(name)))
# Save the time series environment into a binary .RData file
save(rates_env, file="/Users/jerzy/Develop/lecture_slides/data/rates_data.RData")

# Get class of time series object DGS10
class(get(x="DGS10", envir=rates_env))
# Another way
class(rates_env$DGS10)
# Get first 6 rows of time series
head(rates_env$DGS10)
# Plot dygraphs of 10-year Treasury rate
dygraphs::dygraph(rates_env$DGS10, main="10-year Treasury Rate") %>%
  dyOptions(colors="blue", strokeWidth=2)
# Plot 10-year constant maturity Treasury rate
x11(width=6, height=5)
par(mar=c(2, 2, 0, 0), oma=c(0, 0, 0, 0))
chart_Series(rates_env$DGS10["1990/"], name="10-year Treasury Rate")

# Load constant maturity Treasury rates
load(file="/Users/jerzy/Develop/lecture_slides/data/rates_data.RData")
# Get most recent yield curve
yc2021 <- eapply(rates_env, xts::last)
class(yc2021)
yc2021 <- do.call(cbind, yc2021)
# Check if 2020-03-25 is not a holiday
day2020 <- as.Date("2020-03-25")
weekdays(day2020)
# Get yield curve from 2020-03-25
yc2020 <- eapply(rates_env, function(x) x[day2020])
yc2020 <- do.call(cbind, yc2020)
# Combine the yield curves
rates <- c(yc2020, yc2021)
# Rename columns and rows, sort columns, and transpose into matrix
colnames(rates) <- substr(colnames(rates), start=4, stop=11)
rates <- rates[, order(as.numeric(colnames(rates)))]
colnames(rates) <- paste0(colnames(rates), "yr")
rates <- t(rates)
colnames(rates) <- substr(colnames(rates), start=1, stop=4)

x11(width=6, height=5)
par(mar=c(3, 3, 2, 0), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
# Plot using matplot()
colors <- c("blue", "red")
matplot(rates, main="Yield Curves in 2020 and 2021", xaxt="n", lwd=3, lty=1,
  type="l", xlab="maturity", ylab="yield", col=colors)
# Add x-axis
axis(1, seq_along(rownames(rates)), rownames(rates))
# Add legend
legend("topleft", legend=colnames(rates),
 col=colors, lty=1, lwd=6, inset=0.05, cex=1.0)

x11(width=6, height=5)
par(mar=c(3, 3, 2, 0), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
# Load constant maturity Treasury rates
load(file="/Users/jerzy/Develop/lecture_slides/data/rates_data.RData")
# Get end-of-year dates since 2006
dates <- xts::endpoints(rates_env$DGS1["2006/"], on="years")
dates <- zoo::index(rates_env$DGS1["2006/"][dates])
# Create time series of end-of-year rates
rates <- eapply(rates_env, function(ra_te) ra_te[dates])
rates <- rutils::do_call(cbind, rates)
# Rename columns and rows, sort columns, and transpose into matrix
colnames(rates) <- substr(colnames(rates), start=4, stop=11)
rates <- rates[, order(as.numeric(colnames(rates)))]
colnames(rates) <- paste0(colnames(rates), "yr")
rates <- t(rates)
colnames(rates) <- substr(colnames(rates), start=1, stop=4)
# Plot matrix using plot.zoo()
colors <- colorRampPalette(c("red", "blue"))(NCOL(rates))
plot.zoo(rates, main="Yield curve since 2006", lwd=3, xaxt="n",
   plot.type="single", xlab="maturity", ylab="yield", col=colors)
# Add x-axis
axis(1, seq_along(rownames(rates)), rownames(rates))
# Add legend
legend("topleft", legend=colnames(rates),
 col=colors, lty=1, lwd=4, inset=0.05, cex=0.8)

# Alternative plot using matplot()
matplot(rates, main="Yield curve since 2006", xaxt="n", lwd=3, lty=1,
  type="l", xlab="maturity", ylab="yield", col=colors)
# Add x-axis
axis(1, seq_along(rownames(rates)), rownames(rates))
# Add legend
legend("topleft", legend=colnames(rates),
 col=colors, lty=1, lwd=4, inset=0.05, cex=0.8)

# Extract rates from rates_env
symbolv <- c("DGS1", "DGS2", "DGS5", "DGS10", "DGS20")
rates <- mget(symbolv, envir=rates_env)
rates <- rutils::do_call(cbind, rates)
rates <- zoo::na.locf(rates, na.rm=FALSE)
rates <- zoo::na.locf(rates, fromLast=TRUE)
# Calculate daily percentage rates changes
returns <- rutils::diffit(log(rates))
# De-mean the returns
returns <- lapply(returns, function(x) {x - mean(x)})
returns <- rutils::do_call(cbind, returns)
sapply(returns, mean)
# Covariance and Correlation matrices of Treasury rates
covmat <- cov(returns)
cormat <- cor(returns)
# Reorder correlation matrix based on clusters
library(corrplot)
ordern <- corrMatOrder(cormat, order="hclust",
  hclust.method="complete")
cormat <- cormat[ordern, ordern]

# Plot the correlation matrix
x11(width=6, height=6)
colors <- colorRampPalette(c("red", "white", "blue"))
corrplot(cormat, title=NA, tl.col="black",
    method="square", col=colors(NCOL(cormat)), tl.cex=0.8,
    cl.offset=0.75, cl.cex=0.7, cl.align.text="l", cl.ratio=0.25)
title("Correlation of Treasury Rates", line=1)
# Draw rectangles on the correlation matrix plot
corrRect.hclust(cormat, k=NROW(cormat) %/% 2,
  method="complete", col="red")

# Create initial vector of portfolio weights
nweights <- NROW(symbolv)
weights <- rep(1/sqrt(nweights), nweights)
names(weights) <- symbolv
# Objective function equal to minus portfolio variance
objfun <- function(weights, returns) {
  retsp <- returns %*% weights
  -1e7*var(retsp) + 1e7*(1 - sum(weights*weights))^2
}  # end objfun
# Objective function for equal weight portfolio
objfun(weights, returns)
# Compare speed of vector multiplication methods
library(microbenchmark)
summary(microbenchmark(
  transp=t(returns) %*% returns,
  sumv=sum(returns*returns),
  times=10))[, c(1, 4, 5)]

# Find weights with maximum variance
optiml <- optim(par=weights,
  fn=objfun,
  returns=returns,
  method="L-BFGS-B",
  upper=rep(5.0, nweights),
  lower=rep(-5.0, nweights))
# Optimal weights and maximum variance
weights1 <- optiml$par
objfun(weights1, returns)
# Plot first principal component loadings
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
barplot(weights1, names.arg=names(weights1),
  xlab="", ylab="", main="First Principal Component Loadings")

# pc1 weights and returns
pc1 <- drop(returns %*% weights1)
# Redefine objective function
objfun <- function(weights, returns) {
  retsp <- returns %*% weights
  -1e7*var(retsp) + 1e7*(1 - sum(weights^2))^2 +
    1e7*sum(weights1*weights)^2
}  # end objfun
# Find second principal component weights
optiml <- optim(par=weights,
             fn=objfun,
             returns=returns,
             method="L-BFGS-B",
             upper=rep(5.0, nweights),
             lower=rep(-5.0, nweights))

# pc2 weights and returns
weights2 <- optiml$par
pc2 <- drop(returns %*% weights2)
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
pcad <- prcomp(returns, scale=TRUE)
# Compare outputs
all.equal(eigend$values, pcad$sdev^2)
all.equal(abs(eigend$vectors), abs(pcad$rotation),
    check.attributes=FALSE)
# Eigen decomposition of covariance matrix
eigend <- eigen(covmat)
# Perform PCA without scaling
pcad <- prcomp(returns, scale=FALSE)
# Compare outputs
all.equal(eigend$values, pcad$sdev^2)
all.equal(abs(eigend$vectors), abs(pcad$rotation),
    check.attributes=FALSE)

# Perform principal component analysis PCA
pcad <- prcomp(returns, scale=TRUE)
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
returns <- lapply(returns, function(x) {(x - mean(x))/sd(x)})
returns <- rutils::do_call(cbind, returns)
sapply(returns, mean)
sapply(returns, sd)
# Calculate principal component time series
pcats <- returns %*% pcad$rotation
all.equal(pcad$x, pcats, check.attributes=FALSE)
# Calculate products of principal component time series
round(t(pcats) %*% pcats, 2)
# Coerce to xts time series
pcats <- xts(pcats, order.by=zoo::index(returns))
pcats <- cumsum(pcats)
# Plot principal component time series in multiple panels
par(mfrow=c(3,2))
par(mar=c(2, 2, 0, 1), oma=c(0, 0, 0, 0))
rangev <- range(pcats)
for (ordern in 1:NCOL(pcats)) {
  plot.zoo(pcats[, ordern], ylim=rangev, xlab="", ylab="")
  title(paste0("PC", ordern), line=-1, col.main="red")
}  # end for

# Invert all the principal component time series
pcarets <- returns %*% pcad$rotation
solved <- pcarets %*% solve(pcad$rotation)
all.equal(coredata(returns), solved)

# Invert first 3 principal component time series
solved <- pcarets[, 1:3] %*% solve(pcad$rotation)[1:3, ]
solved <- xts::xts(solved, zoo::index(returns))
solved <- cumsum(solved)
cumrets <- cumsum(returns)
# Plot the solved returns
par(mfrow=c(3,2))
par(mar=c(2, 2, 0, 1), oma=c(0, 0, 0, 0))
for (symbol in symbolv) {
  plot.zoo(cbind(cumrets[, symbol], solved[, symbol]),
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
response <- (1 + 2*predictor + noise)

# Calculate de-meaned explanatory (predictor) and response vectors
predictor_zm <- predictor - mean(predictor)
response_zm <- response - mean(response)
# Calculate the regression beta
betav <- sum(predictor_zm*response_zm)/sum(predictor_zm^2)
cov(predictor, response)/var(predictor)
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
resid_std <- sqrt(sum(residuals^2)/degf)
# Standard error of beta
beta_std <- resid_std/sqrt(sum(predictor_zm^2))
# Standard error of alpha
alpha_std <- resid_std*
  sqrt(1/nrows + mean(predictor)^2/sum(predictor_zm^2))

model_sum <- summary(model)  # Copy regression summary
model_sum  # Print the summary to console
attributes(model_sum)$names  # get summary elements

model_sum$coeff
# Standard errors
model_sum$coefficients[2, "Std. Error"]
all.equal(c(alpha_std, beta_std),
  model_sum$coefficients[, "Std. Error"], 
  check.attributes=FALSE)
# R-squared
model_sum$r.squared
model_sum$adj.r.squared
# F-statistic and ANOVA
model_sum$fstatistic
anova(model)

set.seed(1121)  # initialize random number generator
# High noise compared to coefficient
response <- (1 + predictor + rnorm(nrows, sd=8))
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
  model_sum <- summary(lm(formulav))
# Extract regression statistics
  with(model_sum, c(pval=coefficients[2, 4],
   adj_rsquared=adj.r.squared,
   fstat=fstatistic[1]))
}  # end reg_stats
# Apply reg_stats() to vector of std dev values
vec_sd <- seq(from=0.1, to=0.5, by=0.1)
names(vec_sd) <- paste0("sd=", vec_sd)
mat_stats <- t(sapply(vec_sd, reg_stats))
# Plot in loop
par(mfrow=c(NCOL(mat_stats), 1))
for (it in 1:NCOL(mat_stats)) {
  plot(mat_stats[, it], type="l",
 xaxt="n", xlab="", ylab="", main="")
  title(main=colnames(mat_stats)[it], line=-1.0)
  axis(1, at=1:(NROW(mat_stats)), labels=rownames(mat_stats))
}  # end for

reg_stats <- function(datav) {  # get regression
# Perform regression and get summary
  colnamev <- colnames(datav)
  formulav <- paste(colnamev[2], colnamev[1], sep="~")
  model_sum <- summary(lm(formulav, data=datav))
# Extract regression statistics
  with(model_sum, c(pval=coefficients[2, 4],
   adj_rsquared=adj.r.squared,
   fstat=fstatistic[1]))
}  # end reg_stats
# Apply reg_stats() to vector of std dev values
vec_sd <- seq(from=0.1, to=0.5, by=0.1)
names(vec_sd) <- paste0("sd=", vec_sd)
mat_stats <- t(sapply(vec_sd, function(stdev) {
    set.seed(1121)  # initialize number generator
# Define explanatory (predictor) and response variables
    predictor <- rnorm(100, mean=2)
    response <- (1 + 0.2*predictor +
rnorm(NROW(predictor), sd=stdev))
    reg_stats(data.frame(predictor, response))
    }))
# Plot in loop
par(mfrow=c(NCOL(mat_stats), 1))
for (it in 1:NCOL(mat_stats)) {
  plot(mat_stats[, it], type="l",
 xaxt="n", xlab="", ylab="", main="")
  title(main=colnames(mat_stats)[it], line=-1.0)
  axis(1, at=1:(NROW(mat_stats)),
 labels=rownames(mat_stats))
}  # end for

# Set plot paramaters - margins and font scale
par(oma=c(1,0,1,0), mgp=c(2,1,0), mar=c(2,1,2,1), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2, 2))  # Plot 2x2 panels
plot(model)  # Plot diagnostic scatterplots
plot(model, which=2)  # Plot just Q-Q

library(lmtest)  # Load lmtest
# Perform Durbin-Watson test
lmtest::dwtest(model)
