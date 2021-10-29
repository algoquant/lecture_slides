# Create random real symmetric matrix
mat_rix <- matrix(runif(25), nc=5)
mat_rix <- mat_rix + t(mat_rix)
# Calculate eigenvectors and eigenvalues
ei_gen <- eigen(mat_rix)
eigen_vec <- ei_gen$vectors
dim(eigen_vec)
# Plot eigenvalues
barplot(ei_gen$values, xlab="", ylab="", las=3,
  names.arg=paste0("ev", 1:NROW(ei_gen$values)),
  main="Eigenvalues of a real symmetric matrix")
# eigenvectors form an orthonormal basis
round(t(eigen_vec) %*% eigen_vec, digits=4)
# Diagonalize matrix using eigenvector matrix
round(t(eigen_vec) %*% (mat_rix %*% eigen_vec), digits=4)
ei_gen$values
# eigen decomposition of matrix by rotating the diagonal matrix
de_comp <- eigen_vec %*% (ei_gen$values * t(eigen_vec))
# Create diagonal matrix of eigenvalues
# diago_nal <- diag(ei_gen$values)
# de_comp <- eigen_vec %*% (diago_nal %*% t(eigen_vec))
all.equal(mat_rix, de_comp)
# Create random positive semi-definite matrix
mat_rix <- matrix(runif(25), nc=5)
mat_rix <- t(mat_rix) %*% mat_rix
# Calculate eigenvectors and eigenvalues
ei_gen <- eigen(mat_rix)
ei_gen$values
# Plot eigenvalues
barplot(ei_gen$values, las=3, xlab="", ylab="",
  names.arg=paste0("ev", 1:NROW(ei_gen$values)),
  main="Eigenvalues of positive semi-definite matrix")
# Perform singular value decomposition
mat_rix <- matrix(rnorm(50), nc=5)
s_vd <- svd(mat_rix)
# Recompose mat_rix from SVD mat_rices
all.equal(mat_rix, s_vd$u %*% (s_vd$d*t(s_vd$v)))
# Columns of U and V are orthonormal
round(t(s_vd$u) %*% s_vd$u, 4)
round(t(s_vd$v) %*% s_vd$v, 4)
# Dimensions of left and right matrices
n_left <- 6 ; n_right <- 4
# Calculate left matrix
left_mat <- matrix(runif(n_left^2), nc=n_left)
ei_gen <- eigen(crossprod(left_mat))
left_mat <- ei_gen$vectors[, 1:n_right]
# Calculate right matrix and singular values
right_mat <- matrix(runif(n_right^2), nc=n_right)
ei_gen <- eigen(crossprod(right_mat))
right_mat <- ei_gen$vectors
sing_values <- sort(runif(n_right, min=1, max=5), decreasing=TRUE)
# Compose rectangular matrix
mat_rix <- left_mat %*% (sing_values * t(right_mat))
# Perform singular value decomposition
s_vd <- svd(mat_rix)
# Recompose mat_rix from SVD
all.equal(mat_rix, s_vd$u %*% (s_vd$d*t(s_vd$v)))
# Compare SVD with mat_rix components
all.equal(abs(s_vd$u), abs(left_mat))
all.equal(abs(s_vd$v), abs(right_mat))
all.equal(s_vd$d, sing_values)
# Eigen decomposition of mat_rix squared
square_d <- mat_rix %*% t(mat_rix)
ei_gen <- eigen(square_d)
all.equal(ei_gen$values[1:n_right], sing_values^2)
all.equal(abs(ei_gen$vectors[, 1:n_right]), abs(left_mat))
# Eigen decomposition of mat_rix squared
square_d <- t(mat_rix) %*% mat_rix
ei_gen <- eigen(square_d)
all.equal(ei_gen$values, sing_values^2)
all.equal(abs(ei_gen$vectors), abs(right_mat))
# Create random positive semi-definite matrix
mat_rix <- matrix(runif(25), nc=5)
mat_rix <- t(mat_rix) %*% mat_rix
# Calculate the inverse of mat_rix
in_verse <- solve(a=mat_rix)
# Multiply inverse with matrix
round(in_verse %*% mat_rix, 4)
round(mat_rix %*% in_verse, 4)
# Calculate eigenvectors and eigenvalues
ei_gen <- eigen(mat_rix)
eigen_vec <- ei_gen$vectors
# Perform eigen decomposition of inverse
eigen_inverse <- eigen_vec %*% (t(eigen_vec) / ei_gen$values)
all.equal(in_verse, eigen_inverse)
# Decompose diagonal matrix with inverse of eigenvalues
# diago_nal <- diag(1/ei_gen$values)
# eigen_inverse <-
#   eigen_vec %*% (diago_nal %*% t(eigen_vec))
# Random rectangular matrix: n_left > n_right
n_left <- 6 ; n_right <- 4
mat_rix <- matrix(runif(n_left*n_right), nc=n_right)
# Calculate generalized inverse of mat_rix
in_verse <- MASS::ginv(mat_rix)
round(in_verse %*% mat_rix, 4)
all.equal(mat_rix, mat_rix %*% in_verse %*% mat_rix)
# Random rectangular matrix: n_left < n_right
n_left <- 4 ; n_right <- 6
mat_rix <- matrix(runif(n_left*n_right), nc=n_right)
# Calculate generalized inverse of mat_rix
in_verse <- MASS::ginv(mat_rix)
all.equal(mat_rix, mat_rix %*% in_verse %*% mat_rix)
round(mat_rix %*% in_verse, 4)
round(in_verse %*% mat_rix, 4)
# Perform singular value decomposition
s_vd <- svd(mat_rix)
# Calculate generalized inverse from SVD
svd_inverse <- s_vd$v %*% (t(s_vd$u) / s_vd$d)
all.equal(svd_inverse, in_verse)
# Calculate Moore-Penrose pseudo-inverse
mp_inverse <- MASS::ginv(t(mat_rix) %*% mat_rix) %*% t(mat_rix)
all.equal(mp_inverse, in_verse)
# Create random singular matrix
# More columns than rows: n_right > n_left
n_left <- 4 ; n_right <- 6
mat_rix <- matrix(runif(n_left*n_right), nc=n_right)
mat_rix <- t(mat_rix) %*% mat_rix
# Perform singular value decomposition
s_vd <- svd(mat_rix)
# Incorrect inverse from SVD because of zero singular values
svd_inverse <- s_vd$v %*% (t(s_vd$u) / s_vd$d)
# Inverse property doesn't hold
all.equal(mat_rix, mat_rix %*% svd_inverse %*% mat_rix)
# Set tolerance for determining zero singular values
to_l <- sqrt(.Machine$double.eps)
# Check for zero singular values
round(s_vd$d, 12)
not_zero <- (s_vd$d > (to_l * s_vd$d[1]))
# Calculate generalized inverse from SVD
svd_inverse <- s_vd$v[, not_zero] %*%
  (t(s_vd$u[, not_zero]) / s_vd$d[not_zero])
# Verify inverse property of mat_rix
all.equal(mat_rix, mat_rix %*% svd_inverse %*% mat_rix)
# Calculate generalized inverse using MASS::ginv()
in_verse <- MASS::ginv(mat_rix)
all.equal(svd_inverse, in_verse)
# Calculate Moore-Penrose pseudo-inverse
mp_inverse <- MASS::ginv(t(mat_rix) %*% mat_rix) %*% t(mat_rix)
all.equal(mp_inverse, in_verse)
# Diagonalize the "unit" matrix
unit_mat <- mat_rix %*% in_verse
round(unit_mat, 4)
round(mat_rix %*% in_verse, 4)
round(t(s_vd$u) %*% unit_mat %*% s_vd$v, 4)
# Define a square matrix
mat_rix <- matrix(c(1, 2, -1, 2), nc=2)
vec_tor <- c(2, 1)
# Calculate the inverse of mat_rix
in_verse <- solve(a=mat_rix)
in_verse %*% mat_rix
# Calculate solution using inverse of mat_rix
solu_tion <- in_verse %*% vec_tor
mat_rix %*% solu_tion
# Calculate solution of linear system
solu_tion <- solve(a=mat_rix, b=vec_tor)
mat_rix %*% solu_tion
# Create large random positive semi-definite matrix
mat_rix <- matrix(runif(1e4), nc=100)
mat_rix <- t(mat_rix) %*% mat_rix
# Calculate eigen decomposition
ei_gen <- eigen(mat_rix)
eigen_val <- ei_gen$values
eigen_vec <- ei_gen$vectors
# Set tolerance for determining zero singular values
to_l <- sqrt(.Machine$double.eps)
# If needed convert to positive definite matrix
not_zero <- (eigen_val > (to_l*eigen_val[1]))
if (sum(!not_zero) > 0) {
  eigen_val[!not_zero] <- 2*to_l
  mat_rix <- eigen_vec %*% (eigen_val * t(eigen_vec))
}  # end if
# Calculate the Cholesky mat_rix
choles_ky <- chol(mat_rix)
choles_ky[1:5, 1:5]
all.equal(mat_rix, t(choles_ky) %*% choles_ky)
# Calculate inverse from Cholesky
chol_inverse <- chol2inv(choles_ky)
all.equal(solve(mat_rix), chol_inverse)
# Compare speed of Cholesky inversion
library(microbenchmark)
summary(microbenchmark(
  sol_ve=solve(mat_rix),
  choles_ky=chol2inv(chol(mat_rix)),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Calculate random covariance matrix
cov_mat <- matrix(runif(25), nc=5)
cov_mat <- t(cov_mat) %*% cov_mat
# Calculate the Cholesky mat_rix
choles_ky <- chol(cov_mat)
choles_ky
# Simulate random uncorrelated returns
n_assets <- 5
n_rows <- 10000
re_turns <- matrix(rnorm(n_assets*n_rows), nc=n_assets)
# Calculate correlated returns by applying Cholesky
corr_returns <- re_turns %*% choles_ky
# Calculate covariance matrix
cov_returns <- crossprod(corr_returns) / (n_rows-1)
all.equal(cov_mat, cov_returns)
# Simulate random portfolio returns
n_assets <- 10
n_rows <- 100
set.seed(1121)  # Initialize random number generator
re_turns <- matrix(rnorm(n_assets*n_rows), nc=n_assets)
# Calculate de-meaned re_turns matrix
re_turns <- t(t(re_turns) - colMeans(re_turns))
# Or
re_turns <- apply(re_turns, MARGIN=2, function(x) (x-mean(x)))
# Calculate covariance matrix
cov_mat <- crossprod(re_turns) / (n_rows-1)
# Calculate eigenvectors and eigenvalues
ei_gen <- eigen(cov_mat)
ei_gen$values
barplot(ei_gen$values, # Plot eigenvalues
  xlab="", ylab="", las=3,
  names.arg=paste0("ev", 1:NROW(ei_gen$values)),
  main="Eigenvalues of covariance matrix")
# Calculate eigenvectors and eigenvalues
# as function of number of returns
n_data <- ((n_assets/2):(2*n_assets))
e_values <- sapply(n_data, function(x) {
  re_turns <- re_turns[1:x, ]
  re_turns <- apply(re_turns, MARGIN=2, function(y) (y - mean(y)))
  cov_mat <- crossprod(re_turns) / (x-1)
  min(eigen(cov_mat)$values)
})  # end sapply
plot(y=e_values, x=n_data, t="l", xlab="", ylab="", lwd=3, col="blue",
  main="Smallest eigenvalue of covariance matrix
  as function of number of returns")
# Create rectangular matrix with collinear columns
ran_dom <- matrix(rnorm(10*8), nc=10)
# Calculate covariance matrix
cov_mat <- cov(ran_dom)
# Calculate inverse of cov_mat - error
in_verse <- solve(cov_mat)
# Calculate regularized inverse of cov_mat
in_verse <- MASS::ginv(cov_mat)
# Verify inverse property of mat_rix
all.equal(cov_mat, cov_mat %*% in_verse %*% cov_mat)
# Perform eigen decomposition
ei_gen <- eigen(cov_mat)
eigen_vec <- ei_gen$vectors
eigen_val <- ei_gen$values
# Set tolerance for determining zero singular values
to_l <- sqrt(.Machine$double.eps)
# Calculate regularized inverse matrix
not_zero <- (eigen_val > (to_l * eigen_val[1]))
reg_inverse <- eigen_vec[, not_zero] %*%
  (t(eigen_vec[, not_zero]) / eigen_val[not_zero])
# Verify inverse property of mat_rix
all.equal(in_verse, reg_inverse)
# Calculate regularized inverse matrix using cutoff
max_eigen <- 3
in_verse <- eigen_vec[, 1:max_eigen] %*%
  (t(eigen_vec[, 1:max_eigen]) / ei_gen$values[1:max_eigen])
# Verify inverse property of mat_rix
all.equal(in_verse, reg_inverse)
# Create random covariance matrix
set.seed(1121)
mat_rix <- matrix(rnorm(5e2), nc=5)
cov_mat <- cov(mat_rix)
cor_mat <- cor(mat_rix)
std_dev <- sqrt(diag(cov_mat))
# Calculate target matrix
cor_mean <- mean(cor_mat[upper.tri(cor_mat)])
tar_get <- matrix(cor_mean, nr=NROW(cov_mat), nc=NCOL(cov_mat))
diag(tar_get) <- 1
tar_get <- t(t(tar_get * std_dev) * std_dev)
# Calculate shrinkage covariance matrix
al_pha <- 0.5
cov_shrink <- (1-al_pha)*cov_mat + al_pha*tar_get
# Calculate inverse matrix
in_verse <- solve(cov_shrink)
# Create a plotting expression
ex_pr <- quote({
  par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
  deg_free <- 2:20
  rang_e <- (1:NROW(deg_free))
  in_dex <- 4
  # Plot a curve
  curve(expr=dchisq(x, df=deg_free[in_dex]),
xlim=c(0, 30), ylim=c(0, 0.2),
xlab="", ylab="", lwd=3, col="red")
  # Add grey lines to plot
  for (it in rang_e[-in_dex]) {
    curve(expr=dchisq(x, df=deg_free[it]),
  xlim=c(0, 30), ylim=c(0, 0.2),
  xlab="", ylab="", lwd=2, col="grey80", add=TRUE)
  }  # end for
  # Add title
  title(main="Chi-squared Distributions", line=-1.5, cex.main=1.5)
  # Add legend
  text(x=20, y=0.15, labels=paste0("Degrees of freedom=",
      deg_free[in_dex]), pos=1, cex=1.3)
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
  rang_e <- (1:NROW(deg_free))
  # Set image refesh interval
  animation::ani.options(interval=0.5)
  # Create multiple plots with curves
  for (in_dex in rang_e) {
    curve(expr=dchisq(x, df=deg_free[in_dex]),
  xlim=c(0, 30), ylim=c(0, 0.2),
  xlab="", ylab="", lwd=3, col="red")
    # Add grey lines to plot
    for (it in rang_e[-in_dex]) {
      curve(expr=dchisq(x, df=deg_free[it]),
    xlim=c(0, 30), ylim=c(0, 0.2),
    xlab="", ylab="", lwd=2, col="grey80", add=TRUE)
    }  # end for
    # Add title
    title(main="Chi-squared Distributions", line=-1.5, cex.main=1.5)
    # Add legend
    text(x=20, y=0.15, labels=paste0("Degrees of freedom=",
      deg_free[in_dex]), pos=1, cex=1.3)
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
sym_bols <- c("DGS1", "DGS2", "DGS5", "DGS10", "DGS20", "DGS30")
# Create new environment for time series
rates_env <- new.env()
# Download time series for sym_bols into rates_env
quantmod::getSymbols(sym_bols, env=rates_env, src="FRED")
# List files in rates_env
ls(rates_env)
# Get class of all objects in rates_env
sapply(rates_env, class)
# Get class of all objects in R workspace
sapply(ls(), function(nam_e) class(get(nam_e)))
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
yc_2021 <- eapply(rates_env, xts::last)
class(yc_2021)
yc_2021 <- do.call(cbind, yc_2021)
# Check if 2020-03-25 is not a holiday
weekdays(as.Date("2020-03-25"))
# Get yield curve from 2020-03-25
yc_2020 <- eapply(rates_env, function(x) x[as.Date("2020-03-25")])
yc_2020 <- do.call(cbind, yc_2020)
# Combine the yield curves
rate_s <- c(yc_2020, yc_2021)
# Rename columns and rows, sort columns, and transpose into matrix
colnames(rate_s) <- substr(colnames(rate_s), start=4, stop=11)
rate_s <- rate_s[, order(as.numeric(colnames(rate_s)))]
colnames(rate_s) <- paste0(colnames(rate_s), "yr")
rate_s <- t(rate_s)
colnames(rate_s) <- substr(colnames(rate_s), start=1, stop=4)
x11(width=6, height=5)
par(mar=c(3, 3, 2, 0), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
# Plot using matplot()
col_ors <- c("blue", "red")
matplot(rate_s, main="Yield Curves in 2020 and 2021", xaxt="n", lwd=3, lty=1,
  type="l", xlab="maturity", ylab="yield", col=col_ors)
# Add x-axis
axis(1, seq_along(rownames(rate_s)), rownames(rate_s))
# Add legend
legend("topleft", legend=colnames(rate_s),
 col=col_ors, lty=1, lwd=6, inset=0.05, cex=1.0)
x11(width=6, height=5)
par(mar=c(3, 3, 2, 0), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
# Load constant maturity Treasury rates
load(file="/Users/jerzy/Develop/lecture_slides/data/rates_data.RData")
# Get end-of-year dates since 2006
date_s <- xts::endpoints(rates_env$DGS1["2006/"], on="years")
date_s <- zoo::index(rates_env$DGS1["2006/"][date_s])
# Create time series of end-of-year rates
rate_s <- eapply(rates_env, function(ra_te) ra_te[date_s])
rate_s <- rutils::do_call(cbind, rate_s)
# Rename columns and rows, sort columns, and transpose into matrix
colnames(rate_s) <- substr(colnames(rate_s), start=4, stop=11)
rate_s <- rate_s[, order(as.numeric(colnames(rate_s)))]
colnames(rate_s) <- paste0(colnames(rate_s), "yr")
rate_s <- t(rate_s)
colnames(rate_s) <- substr(colnames(rate_s), start=1, stop=4)
# Plot matrix using plot.zoo()
col_ors <- colorRampPalette(c("red", "blue"))(NCOL(rate_s))
plot.zoo(rate_s, main="Yield curve since 2006", lwd=3, xaxt="n",
   plot.type="single", xlab="maturity", ylab="yield", col=col_ors)
# Add x-axis
axis(1, seq_along(rownames(rate_s)), rownames(rate_s))
# Add legend
legend("topleft", legend=colnames(rate_s),
 col=col_ors, lty=1, lwd=4, inset=0.05, cex=0.8)
# Alternative plot using matplot()
matplot(rate_s, main="Yield curve since 2006", xaxt="n", lwd=3, lty=1,
  type="l", xlab="maturity", ylab="yield", col=col_ors)
# Add x-axis
axis(1, seq_along(rownames(rate_s)), rownames(rate_s))
# Add legend
legend("topleft", legend=colnames(rate_s),
 col=col_ors, lty=1, lwd=4, inset=0.05, cex=0.8)
# Extract rates from rates_env
sym_bols <- c("DGS1", "DGS2", "DGS5", "DGS10", "DGS20")
rate_s <- mget(sym_bols, envir=rates_env)
rate_s <- rutils::do_call(cbind, rate_s)
rate_s <- zoo::na.locf(rate_s, na.rm=FALSE)
rate_s <- zoo::na.locf(rate_s, fromLast=TRUE)
# Calculate daily percentage rates changes
re_turns <- rutils::diff_it(log(rate_s))
# De-mean the returns
re_turns <- lapply(re_turns, function(x) {x - mean(x)})
re_turns <- rutils::do_call(cbind, re_turns)
sapply(re_turns, mean)
# Covariance and Correlation matrices of Treasury rates
cov_mat <- cov(re_turns)
cor_mat <- cor(re_turns)
# Reorder correlation matrix based on clusters
library(corrplot)
or_der <- corrMatOrder(cor_mat, order="hclust",
  hclust.method="complete")
cor_mat <- cor_mat[or_der, or_der]
# Plot the correlation matrix
x11(width=6, height=6)
col_ors <- colorRampPalette(c("red", "white", "blue"))
corrplot(cor_mat, title=NA, tl.col="black",
    method="square", col=col_ors(NCOL(cor_mat)), tl.cex=0.8,
    cl.offset=0.75, cl.cex=0.7, cl.align.text="l", cl.ratio=0.25)
title("Correlation of Treasury Rates", line=1)
# Draw rectangles on the correlation matrix plot
corrRect.hclust(cor_mat, k=NROW(cor_mat) %/% 2,
  method="complete", col="red")
# Create initial vector of portfolio weights
n_weights <- NROW(sym_bols)
weight_s <- rep(1/sqrt(n_weights), n_weights)
names(weight_s) <- sym_bols
# Objective function equal to minus portfolio variance
objec_tive <- function(weight_s, re_turns) {
  portf_rets <- re_turns %*% weight_s
  -1e7*var(portf_rets) + 1e7*(1 - sum(weight_s*weight_s))^2
}  # end objec_tive
# Objective for equal weight portfolio
objec_tive(weight_s, re_turns)
# Compare speed of vector multiplication methods
library(microbenchmark)
summary(microbenchmark(
  trans_pose=t(re_turns) %*% re_turns,
  s_um=sum(re_turns*re_turns),
  times=10))[, c(1, 4, 5)]
# Find weights with maximum variance
op_tim <- optim(par=weight_s,
  fn=objec_tive,
  re_turns=re_turns,
  method="L-BFGS-B",
  upper=rep(5.0, n_weights),
  lower=rep(-5.0, n_weights))
# Optimal weights and maximum variance
weights_1 <- op_tim$par
objec_tive(weights_1, re_turns)
# Plot first principal component loadings
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
barplot(weights_1, names.arg=names(weights_1),
  xlab="", ylab="", main="First Principal Component Loadings")
# pc1 weights and returns
pc_1 <- drop(re_turns %*% weights_1)
# Redefine objective function
objec_tive <- function(weight_s, re_turns) {
  portf_rets <- re_turns %*% weight_s
  -1e7*var(portf_rets) + 1e7*(1 - sum(weight_s^2))^2 +
    1e7*sum(weights_1*weight_s)^2
}  # end objec_tive
# Find second principal component weights
op_tim <- optim(par=weight_s,
             fn=objec_tive,
             re_turns=re_turns,
             method="L-BFGS-B",
             upper=rep(5.0, n_weights),
             lower=rep(-5.0, n_weights))
# pc2 weights and returns
weights_2 <- op_tim$par
pc_2 <- drop(re_turns %*% weights_2)
sum(pc_1*pc_2)
# Plot second principal component loadings
barplot(weights_2, names.arg=names(weights_2),
  xlab="", ylab="", main="Second Principal Component Loadings")
ei_gen <- eigen(cov_mat)
ei_gen$vectors
# Compare with optimization
all.equal(sum(diag(cov_mat)), sum(ei_gen$values))
all.equal(abs(ei_gen$vectors[, 1]), abs(weights_1), check.attributes=FALSE)
all.equal(abs(ei_gen$vectors[, 2]), abs(weights_2), check.attributes=FALSE)
all.equal(ei_gen$values[1], var(pc_1), check.attributes=FALSE)
all.equal(ei_gen$values[2], var(pc_2), check.attributes=FALSE)
# Eigenvalue equations are satisfied approximately
(cov_mat %*% weights_1) / weights_1 / var(pc_1)
(cov_mat %*% weights_2) / weights_2 / var(pc_2)
# Plot eigenvalues
barplot(ei_gen$values, names.arg=paste0("PC", 1:n_weights),
  las=3, xlab="", ylab="", main="Principal Component Variances")
# Eigen decomposition of correlation matrix
ei_gen <- eigen(cor_mat)
# Perform PCA with scaling
pc_a <- prcomp(re_turns, scale=TRUE)
# Compare outputs
all.equal(ei_gen$values, pc_a$sdev^2)
all.equal(abs(ei_gen$vectors), abs(pc_a$rotation),
    check.attributes=FALSE)
# Eigen decomposition of covariance matrix
ei_gen <- eigen(cov_mat)
# Perform PCA without scaling
pc_a <- prcomp(re_turns, scale=FALSE)
# Compare outputs
all.equal(ei_gen$values, pc_a$sdev^2)
all.equal(abs(ei_gen$vectors), abs(pc_a$rotation),
    check.attributes=FALSE)
# Perform principal component analysis PCA
pc_a <- prcomp(re_turns, scale=TRUE)
# Plot standard deviations
barplot(pc_a$sdev, names.arg=colnames(pc_a$rotation),
  las=3, xlab="", ylab="",
  main="Scree Plot: Volatilities of Principal Components
  of Treasury rates")
x11(width=6, height=7)
# Calculate principal component loadings (weights)
pc_a$rotation
# Plot loading barplots in multiple panels
par(mfrow=c(3,2))
par(mar=c(3.5, 2, 2, 1), oma=c(0, 0, 0, 0))
for (or_der in 1:NCOL(pc_a$rotation)) {
  barplot(pc_a$rotation[, or_der], las=3, xlab="", ylab="", main="")
  title(paste0("PC", or_der), line=-2.0, col.main="red")
}  # end for
# Standardize (de-mean and scale) the returns
re_turns <- lapply(re_turns, function(x) {(x - mean(x))/sd(x)})
re_turns <- rutils::do_call(cbind, re_turns)
sapply(re_turns, mean)
sapply(re_turns, sd)
# Calculate principal component time series
pca_ts <- re_turns %*% pc_a$rotation
all.equal(pc_a$x, pca_ts, check.attributes=FALSE)
# Calculate products of principal component time series
round(t(pca_ts) %*% pca_ts, 2)
# Coerce to xts time series
pca_ts <- xts(pca_ts, order.by=index(re_turns))
pca_ts <- cumsum(pca_ts)
# Plot principal component time series in multiple panels
par(mfrow=c(3,2))
par(mar=c(2, 2, 0, 1), oma=c(0, 0, 0, 0))
rang_e <- range(pca_ts)
for (or_der in 1:NCOL(pca_ts)) {
  plot.zoo(pca_ts[, or_der], ylim=rang_e, xlab="", ylab="")
  title(paste0("PC", or_der), line=-1, col.main="red")
}  # end for
# Invert all the principal component time series
pca_rets <- re_turns %*% pc_a$rotation
sol_ved <- pca_rets %*% solve(pc_a$rotation)
all.equal(coredata(re_turns), sol_ved)
# Invert first 3 principal component time series
sol_ved <- pca_rets[, 1:3] %*% solve(pc_a$rotation)[1:3, ]
sol_ved <- xts::xts(sol_ved, zoo::index(re_turns))
sol_ved <- cumsum(sol_ved)
cum_returns <- cumsum(re_turns)
# Plot the solved returns
par(mfrow=c(3,2))
par(mar=c(2, 2, 0, 1), oma=c(0, 0, 0, 0))
for (sym_bol in sym_bols) {
  plot.zoo(cbind(cum_returns[, sym_bol], sol_ved[, sym_bol]),
    plot.type="single", col=c("black", "blue"), xlab="", ylab="")
  legend(x="topleft", bty="n",
   legend=paste0(sym_bol, c("", " solved")),
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
