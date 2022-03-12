# Create random real symmetric matrix
matrixv <- matrix(runif(25), nc=5)
matrixv <- matrixv + t(matrixv)
# Calculate eigenvectors and eigenvalues
eigend <- eigen(matrixv)
eigen_vec <- eigend$vectors
dim(eigen_vec)
# Plot eigenvalues
barplot(eigend$values, xlab="", ylab="", las=3,
  names.arg=paste0("ev", 1:NROW(eigend$values)),
  main="Eigenvalues of a real symmetric matrix")
# eigenvectors form an orthonormal basis
round(t(eigen_vec) %*% eigen_vec, digits=4)
# Diagonalize matrix using eigenvector matrix
round(t(eigen_vec) %*% (matrixv %*% eigen_vec), digits=4)
eigend$values
# eigen decomposition of matrix by rotating the diagonal matrix
de_comp <- eigen_vec %*% (eigend$values * t(eigen_vec))
# Create diagonal matrix of eigenvalues
# diago_nal <- diag(eigend$values)
# de_comp <- eigen_vec %*% (diago_nal %*% t(eigen_vec))
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
n_left <- 6 ; n_right <- 4
# Calculate left matrix
left_mat <- matrix(runif(n_left^2), nc=n_left)
eigend <- eigen(crossprod(left_mat))
left_mat <- eigend$vectors[, 1:n_right]
# Calculate right matrix and singular values
right_mat <- matrix(runif(n_right^2), nc=n_right)
eigend <- eigen(crossprod(right_mat))
right_mat <- eigend$vectors
sing_values <- sort(runif(n_right, min=1, max=5), decreasing=TRUE)
# Compose rectangular matrix
matrixv <- left_mat %*% (sing_values * t(right_mat))
# Perform singular value decomposition
svdec <- svd(matrixv)
# Recompose matrixv from SVD
all.equal(matrixv, svdec$u %*% (svdec$d*t(svdec$v)))
# Compare SVD with matrixv components
all.equal(abs(svdec$u), abs(left_mat))
all.equal(abs(svdec$v), abs(right_mat))
all.equal(svdec$d, sing_values)
# Eigen decomposition of matrixv squared
square_d <- matrixv %*% t(matrixv)
eigend <- eigen(square_d)
all.equal(eigend$values[1:n_right], sing_values^2)
all.equal(abs(eigend$vectors[, 1:n_right]), abs(left_mat))
# Eigen decomposition of matrixv squared
square_d <- t(matrixv) %*% matrixv
eigend <- eigen(square_d)
all.equal(eigend$values, sing_values^2)
all.equal(abs(eigend$vectors), abs(right_mat))
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
eigen_vec <- eigend$vectors
# Perform eigen decomposition of inverse
eigen_inverse <- eigen_vec %*% (t(eigen_vec) / eigend$values)
all.equal(inverse, eigen_inverse)
# Decompose diagonal matrix with inverse of eigenvalues
# diago_nal <- diag(1/eigend$values)
# eigen_inverse <-
#   eigen_vec %*% (diago_nal %*% t(eigen_vec))
# Random rectangular matrix: n_left > n_right
n_left <- 6 ; n_right <- 4
matrixv <- matrix(runif(n_left*n_right), nc=n_right)
# Calculate generalized inverse of matrixv
inverse <- MASS::ginv(matrixv)
round(inverse %*% matrixv, 4)
all.equal(matrixv, matrixv %*% inverse %*% matrixv)
# Random rectangular matrix: n_left < n_right
n_left <- 4 ; n_right <- 6
matrixv <- matrix(runif(n_left*n_right), nc=n_right)
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
mp_inverse <- MASS::ginv(t(matrixv) %*% matrixv) %*% t(matrixv)
all.equal(mp_inverse, inverse)
# Create random singular matrix
# More columns than rows: n_right > n_left
n_left <- 4 ; n_right <- 6
matrixv <- matrix(runif(n_left*n_right), nc=n_right)
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
# Calculate generalized inverse from SVD
svd_inverse <- svdec$v[, not_zero] %*%
  (t(svdec$u[, not_zero]) / svdec$d[not_zero])
# Verify inverse property of matrixv
all.equal(matrixv, matrixv %*% svd_inverse %*% matrixv)
# Calculate generalized inverse using MASS::ginv()
inverse <- MASS::ginv(matrixv)
all.equal(svd_inverse, inverse)
# Calculate Moore-Penrose pseudo-inverse
mp_inverse <- MASS::ginv(t(matrixv) %*% matrixv) %*% t(matrixv)
all.equal(mp_inverse, inverse)
# Diagonalize the "unit" matrix
unit_mat <- matrixv %*% inverse
round(unit_mat, 4)
round(matrixv %*% inverse, 4)
round(t(svdec$u) %*% unit_mat %*% svdec$v, 4)
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
eigen_val <- eigend$values
eigen_vec <- eigend$vectors
# Set tolerance for determining zero singular values
precision <- sqrt(.Machine$double.eps)
# If needed convert to positive definite matrix
not_zero <- (eigen_val > (precision*eigen_val[1]))
if (sum(!not_zero) > 0) {
  eigen_val[!not_zero] <- 2*precision
  matrixv <- eigen_vec %*% (eigen_val * t(eigen_vec))
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
# Calculate the Cholesky matrixv
cholmat <- chol(covmat)
cholmat
# Simulate random uncorrelated returns
nassets <- 5
nrows <- 10000
returns <- matrix(rnorm(nassets*nrows), nc=nassets)
# Calculate correlated returns by applying Cholesky
corr_returns <- returns %*% cholmat
# Calculate covariance matrix
cov_returns <- crossprod(corr_returns) / (nrows-1)
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
covmat <- crossprod(returns) / (nrows-1)
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
ran_dom <- matrix(rnorm(10*8), nc=10)
# Calculate covariance matrix
covmat <- cov(ran_dom)
# Calculate inverse of covmat - error
inverse <- solve(covmat)
# Calculate regularized inverse of covmat
inverse <- MASS::ginv(covmat)
# Verify inverse property of matrixv
all.equal(covmat, covmat %*% inverse %*% covmat)
# Perform eigen decomposition
eigend <- eigen(covmat)
eigen_vec <- eigend$vectors
eigen_val <- eigend$values
# Set tolerance for determining zero singular values
precision <- sqrt(.Machine$double.eps)
# Calculate regularized inverse matrix
not_zero <- (eigen_val > (precision * eigen_val[1]))
reg_inverse <- eigen_vec[, not_zero] %*%
  (t(eigen_vec[, not_zero]) / eigen_val[not_zero])
# Verify inverse property of matrixv
all.equal(inverse, reg_inverse)
# Calculate regularized inverse matrix using cutoff
max_eigen <- 3
inverse <- eigen_vec[, 1:max_eigen] %*%
  (t(eigen_vec[, 1:max_eigen]) / eigend$values[1:max_eigen])
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
# Create a plotting expression
ex_pr <- quote({
  par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
  deg_free <- 2:20
  rangev <- (1:NROW(deg_free))
  indeks <- 4
  # Plot a curve
  curve(expr=dchisq(x, df=deg_free[indeks]),
xlim=c(0, 30), ylim=c(0, 0.2),
xlab="", ylab="", lwd=3, col="red")
  # Add grey lines to plot
  for (it in rangev[-indeks]) {
    curve(expr=dchisq(x, df=deg_free[it]),
  xlim=c(0, 30), ylim=c(0, 0.2),
  xlab="", ylab="", lwd=2, col="grey80", add=TRUE)
  }  # end for
  # Add title
  title(main="Chi-squared Distributions", line=-1.5, cex.main=1.5)
  # Add legend
  text(x=20, y=0.15, labels=paste0("Degrees of freedom=",
      deg_free[indeks]), pos=1, cex=1.3)
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
  deg_free <- 2:20
  rangev <- (1:NROW(deg_free))
  # Set image refesh interval
  animation::ani.options(interval=0.5)
  # Create multiple plots with curves
  for (indeks in rangev) {
    curve(expr=dchisq(x, df=deg_free[indeks]),
  xlim=c(0, 30), ylim=c(0, 0.2),
  xlab="", ylab="", lwd=3, col="red")
    # Add grey lines to plot
    for (it in rangev[-indeks]) {
      curve(expr=dchisq(x, df=deg_free[it]),
    xlim=c(0, 30), ylim=c(0, 0.2),
    xlab="", ylab="", lwd=2, col="grey80", add=TRUE)
    }  # end for
    # Add title
    title(main="Chi-squared Distributions", line=-1.5, cex.main=1.5)
    # Add legend
    text(x=20, y=0.15, labels=paste0("Degrees of freedom=",
      deg_free[indeks]), pos=1, cex=1.3)
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
# Symbols for constant maturity Treasury rates
symbols <- c("DGS1", "DGS2", "DGS5", "DGS10", "DGS20", "DGS30")
# Create new environment for time series
rates_env <- new.env()
# Download time series for symbols into rates_env
quantmod::getSymbols(symbols, env=rates_env, src="FRED")
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
weekdays(as.Date("2020-03-25"))
# Get yield curve from 2020-03-25
yc2020 <- eapply(rates_env, function(x) x[as.Date("2020-03-25")])
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
symbols <- c("DGS1", "DGS2", "DGS5", "DGS10", "DGS20")
rates <- mget(symbols, envir=rates_env)
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
nweights <- NROW(symbols)
weightv <- rep(1/sqrt(nweights), nweights)
names(weightv) <- symbols
# Objective function equal to minus portfolio variance
object <- function(weightv, returns) {
  retsp <- returns %*% weightv
  -1e7*var(retsp) + 1e7*(1 - sum(weightv*weightv))^2
}  # end object
# Objective for equal weight portfolio
object(weightv, returns)
# Compare speed of vector multiplication methods
library(microbenchmark)
summary(microbenchmark(
  transp=t(returns) %*% returns,
  sumv=sum(returns*returns),
  times=10))[, c(1, 4, 5)]
# Find weights with maximum variance
optimd <- optim(par=weightv,
  fn=object,
  returns=returns,
  method="L-BFGS-B",
  upper=rep(5.0, nweights),
  lower=rep(-5.0, nweights))
# Optimal weights and maximum variance
weights1 <- optimd$par
object(weights1, returns)
# Plot first principal component loadings
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
barplot(weights1, names.arg=names(weights1),
  xlab="", ylab="", main="First Principal Component Loadings")
# pc1 weights and returns
pc1 <- drop(returns %*% weights1)
# Redefine objective function
object <- function(weightv, returns) {
  retsp <- returns %*% weightv
  -1e7*var(retsp) + 1e7*(1 - sum(weightv^2))^2 +
    1e7*sum(weights1*weightv)^2
}  # end object
# Find second principal component weights
optimd <- optim(par=weightv,
             fn=object,
             returns=returns,
             method="L-BFGS-B",
             upper=rep(5.0, nweights),
             lower=rep(-5.0, nweights))
# pc2 weights and returns
weights2 <- optimd$par
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
pcats <- xts(pcats, order.by=index(returns))
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
cum_returns <- cumsum(returns)
# Plot the solved returns
par(mfrow=c(3,2))
par(mar=c(2, 2, 0, 1), oma=c(0, 0, 0, 0))
for (symbol in symbols) {
  plot.zoo(cbind(cum_returns[, symbol], solved[, symbol]),
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
