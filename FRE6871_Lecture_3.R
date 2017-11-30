# define vector and matrix
vec_tor1 <- c(2, 4, 3)
mat_rix <- matrix(sample(1:12), ncol=3)
# multiply matrix by vector column-wise
vec_tor1 * mat_rix
mat_rix * vec_tor1
# multiply matrix by vector row-wise
t(vec_tor1 * t(mat_rix))
vec_tor1
vec_tor2 <- 6:4  # define vector
# multiply two vectors element-by-element
vec_tor1 * vec_tor2
# calculate inner product
vec_tor1 %*% vec_tor2
# calculate inner product and drop dimensions
drop(vec_tor1 %*% vec_tor2)
# multiply columns of matrix by vector
mat_rix %*% vec_tor1  # single column matrix
drop(mat_rix %*% vec_tor1)  # vector
rowSums(t(vec_tor1 * t(mat_rix)))
# using rowSums() and t() is 10 times slower than %*%
library(microbenchmark)
summary(microbenchmark(
  in_ner=drop(mat_rix %*% vec_tor1),
  row_sums=rowSums(t(vec_tor1 * t(mat_rix))),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
library(microbenchmark)
# multiply matrix by vector fails because dimensions aren't conformable
vec_tor1 %*% mat_rix
# works after transpose
drop(vec_tor1 %*% t(mat_rix))
# calculate inner product
crossprod(vec_tor1, vec_tor2)
# create matrix and vector
mat_rix <- matrix(1:3000, ncol=3)
tmat_rix <- t(mat_rix)
vec_tor <- 1:3
# crossprod is slightly faster than "%*%" operator
summary(microbenchmark(
  cross_prod=crossprod(tmat_rix, vec_tor),
  inner_prod=mat_rix %*% vec_tor,
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# define named vectors
vec_tor1 <- sample(1:4)
names(vec_tor1) <-
  paste0("row", 1:4, "=", vec_tor1)
vec_tor1
vec_tor2 <- sample(1:3)
names(vec_tor2) <-
  paste0("col", 1:3, "=", vec_tor2)
vec_tor2
# calculate outer product of two vectors
mat_rix <- outer(vec_tor1, vec_tor2)
mat_rix
# calculate vectorized function spanned over two vectors
mat_rix <- outer(vec_tor1, vec_tor2,
           FUN=function(x1, x2) x2*sin(x1))
mat_rix
# create random real symmetric matrix
mat_rix <- matrix(runif(25), nc=5)
mat_rix <- mat_rix + t(mat_rix)
# calculate eigenvectors and eigenvalues
ei_gen <- eigen(mat_rix)
eigen_vec <- ei_gen$vectors
dim(eigen_vec)
# plot eigenvalues
barplot(ei_gen$values,
  xlab="", ylab="", las=3,
  names.arg=paste0("ev", 1:NROW(ei_gen$values)),
  main="Eigenvalues of a real symmetric matrix")
# eigenvectors form an orthonormal basis
round(t(eigen_vec) %*% eigen_vec,
  digits=4)
# diagonalize matrix using eigenvector matrix
round(t(eigen_vec) %*% (mat_rix %*% eigen_vec),
  digits=4)
ei_gen$values
# eigen decomposition of matrix by rotating the diagonal matrix
eigen_decomp <- eigen_vec %*% (ei_gen$values * t(eigen_vec))
# create diagonal matrix of eigenvalues
# diago_nal <- diag(ei_gen$values)
# eigen_decomp <- eigen_vec %*% (diago_nal %*% t(eigen_vec))
all.equal(mat_rix, eigen_decomp)
# create random positive semi-definite matrix
mat_rix <- matrix(runif(25), nc=5)
mat_rix <- t(mat_rix) %*% mat_rix
# calculate eigenvectors and eigenvalues
ei_gen <- eigen(mat_rix)
ei_gen$values
# plot eigenvalues
barplot(ei_gen$values, las=3,
  xlab="", ylab="",
  names.arg=paste0("ev", 1:NROW(ei_gen$values)),
  main="Eigenvalues of positive semi-definite matrix")
# dimensions of left and right matrices
n_left <- 6 ; n_right <- 4
# create random positive semi-definite matrix
left_mat <- matrix(runif(n_left^2), nc=n_left)
left_mat <- left_mat %*% t(left_mat)
# calculate left eigenvectors
ei_gen <- eigen(left_mat)
left_mat <- ei_gen$vectors[, 1:n_right]
# create random positive semi-definite matrix
right_mat <- matrix(runif(n_right^2), nc=n_right)
right_mat <- right_mat %*% t(right_mat)
# calculate right eigenvectors and singular values
ei_gen <- eigen(right_mat)
right_mat <- ei_gen$vectors
sing_values <- ei_gen$values
# compose rectangular matrix
mat_rix <-
  left_mat %*% (sing_values * t(right_mat))
# mat_rix <- left_mat %*% diag(sing_values) %*% t(right_mat)
# perform singular value decomposition
s_vd <- svd(mat_rix)
# compare SVD with inputs
all.equal(abs(s_vd$u), abs(left_mat))
all.equal(abs(s_vd$v), abs(right_mat))
all.equal(s_vd$d, ei_gen$values)
# create random positive semi-definite matrix
mat_rix <- matrix(runif(25), nc=5)
mat_rix <- t(mat_rix) %*% mat_rix
# calculate the inverse of mat_rix
in_verse <- solve(a=mat_rix)
# multiply inverse with matrix
round(in_verse %*% mat_rix, 4)
round(mat_rix %*% in_verse, 4)

# calculate eigenvectors and eigenvalues
ei_gen <- eigen(mat_rix)
eigen_vec <- ei_gen$vectors

# perform eigen decomposition of inverse
eigen_inverse <-
  eigen_vec %*% (t(eigen_vec) / ei_gen$values)
all.equal(in_verse, eigen_inverse)
# decompose diagonal matrix with inverse of eigenvalues
# diago_nal <- diag(1/ei_gen$values)
# eigen_inverse <-
#   eigen_vec %*% (diago_nal %*% t(eigen_vec))
# create random rectangular matrix
# case when: n_left > n_right
n_left <- 6 ; n_right <- 4
mat_rix <- matrix(runif(n_left*n_right),
  nc=n_right)
# calculate generalized inverse of mat_rix
in_verse <- MASS::ginv(mat_rix)
round(in_verse %*% mat_rix, 4)
all.equal(mat_rix,
    mat_rix %*% in_verse %*% mat_rix)
# create random rectangular matrix
# case when: n_left < n_right
n_left <- 4 ; n_right <- 6
mat_rix <- matrix(runif(n_left*n_right),
  nc=n_right)
# calculate generalized inverse of mat_rix
in_verse <- MASS::ginv(mat_rix)
round(mat_rix %*% in_verse, 4)
# perform singular value decomposition
s_vd <- svd(mat_rix)
# calculate generalized inverse from SVD
svd_inverse <- s_vd$v %*% (t(s_vd$u) / s_vd$d)
all.equal(svd_inverse, in_verse)
# calculate Moore-Penrose pseudo-inverse
mp_inverse <-
  MASS::ginv(t(mat_rix) %*% mat_rix) %*% t(mat_rix)
all.equal(mp_inverse, in_verse)
# create random singular matrix
n_left <- 4 ; n_right <- 6
mat_rix <- matrix(runif(n_left*n_right), nc=n_right)
mat_rix <- t(mat_rix) %*% mat_rix
# calculate generalized inverse of mat_rix
in_verse <- MASS::ginv(mat_rix)
# verify inverse of mat_rix
all.equal(mat_rix,
  mat_rix %*% in_verse %*% mat_rix)
# perform singular value decomposition
s_vd <- svd(mat_rix)
# set tolerance for determining zero singular values
to_l <- sqrt(.Machine$double.eps)
# check for zero singular values
s_vd$d
not_zero <- (s_vd$d > (to_l * s_vd$d[1]))
# calculate generalized inverse from SVD
svd_inverse <-
  s_vd$v[, not_zero] %*%
  (t(s_vd$u[, not_zero]) / s_vd$d[not_zero])
all.equal(svd_inverse, in_verse)
# calculate Moore-Penrose pseudo-inverse
mp_inverse <-
  MASS::ginv(t(mat_rix) %*% mat_rix) %*% t(mat_rix)
all.equal(mp_inverse, in_verse)
# calculate generalized inverse from eigen decomposition
ei_gen <- eigen(mat_rix)
eigen_values <- ei_gen$values
eigen_vec <- ei_gen$vectors
not_zero <- (eigen_values > (to_l * eigen_values[1]))
eigen_inverse <- eigen_vec[, not_zero] %*%
  (t(eigen_vec[, not_zero]) / eigen_values[not_zero])
all.equal(svd_inverse, eigen_inverse)
# diagonalize the "unit" matrix
uni_t <- mat_rix %*% in_verse
round(uni_t, 4)
round(mat_rix %*% in_verse, 4)
round(t(s_vd$u) %*% uni_t %*% s_vd$v, 4)
# define a square matrix
mat_rix <- matrix(c(1, 2, -1, 2), nc=2)
vec_tor <- c(2, 1)
# calculate the inverse of mat_rix
in_verse <- solve(a=mat_rix)
in_verse %*% mat_rix
# calculate solution using inverse of mat_rix
solu_tion <- in_verse %*% vec_tor
mat_rix %*% solu_tion
# calculate solution of linear system
solu_tion <- solve(a=mat_rix, b=vec_tor)
mat_rix %*% solu_tion
# create large random positive semi-definite matrix
mat_rix <- matrix(runif(1e4), nc=100)
mat_rix <- t(mat_rix) %*% mat_rix
# calculate eigen decomposition
ei_gen <- eigen(mat_rix)
eigen_values <- ei_gen$values
eigen_vec <- ei_gen$vectors
# set tolerance for determining zero singular values
to_l <- sqrt(.Machine$double.eps)
# if needed convert to positive definite matrix
not_zero <- (eigen_values > (to_l * eigen_values[1]))
if (sum(!not_zero) > 0) {
  eigen_values[!not_zero] <- 2*to_l
  mat_rix <- eigen_vec %*%
    (eigen_values * t(eigen_vec))
}  # end if
# calculate the Cholesky mat_rix
choles_ky <- chol(mat_rix)
choles_ky[1:5, 1:5]
all.equal(mat_rix, t(choles_ky) %*% choles_ky)
# calculate inverse from Cholesky
chol_inverse <- chol2inv(choles_ky)
all.equal(solve(mat_rix), chol_inverse)
# compare speed of Cholesky inversion
library(microbenchmark)
summary(microbenchmark(
  sol_ve=solve(mat_rix),
  choles_ky=chol2inv(chol(mat_rix)),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# calculate random covariance matrix
cov_mat <- matrix(runif(25), nc=5)
cov_mat <- t(cov_mat) %*% cov_mat
# calculate the Cholesky mat_rix
choles_ky <- chol(cov_mat)
choles_ky
# simulate random uncorrelated returns
n_assets <- 5
n_rows <- 10000
re_turns <- matrix(rnorm(n_assets*n_rows), nc=n_assets)
# calculate correlated returns by applying Cholesky
corr_returns <- re_turns %*% choles_ky
# calculate covariance matrix
cov_returns <- crossprod(corr_returns) / (n_rows-1)
all.equal(cov_mat, cov_returns)
# simulate random portfolio returns
n_assets <- 10
n_rows <- 100
re_turns <- matrix(rnorm(n_assets*n_rows), nc=n_assets)
# de-mean the returns
re_turns <- apply(re_turns, MARGIN=2, function(x) (x-mean(x)))
# calculate covariance matrix
cov_mat <- crossprod(re_turns) / (n_rows-1)
# calculate eigenvectors and eigenvalues
ei_gen <- eigen(cov_mat)
ei_gen$values
barplot(ei_gen$values, # plot eigenvalues
  xlab="", ylab="", las=3,
  names.arg=paste0("ev", 1:NROW(ei_gen$values)),
  main="Eigenvalues of covariance matrix")
# calculate eigenvectors and eigenvalues
# as function of number of returns
n_data <- ((n_assets/2):(2*n_assets))
e_values <- sapply(n_data, function(x) {
  re_turns <- re_turns[1:x, ]
  re_turns <- apply(re_turns, MARGIN=2,
    function(y) (y-mean(y)))
  cov_mat <- crossprod(re_turns) / (x-1)
  min(eigen(cov_mat)$values)
})  # end sapply
plot(y=e_values, x=n_data, t="l",
  xlab="", ylab="", lwd=3, col="blue",
  main="Smallest eigenvalue of covariance matrix\nas function of number of returns")
# calculate random default probabilities
num_assets <- 100
default_probs <- runif(num_assets, max=0.05)
# calculate number of defaults
uni_form <- runif(num_assets)
sum(uni_form < default_probs)
# simulate average number of defaults
num_simu <- 1000
de_faults <- numeric(num_simu)
for (i in 1:num_simu) {  # perform loop
  uni_form <- runif(num_assets)
  de_faults[i] <- sum(uni_form < default_probs)
}  # end for
mean(de_faults)
# average defaults using vectorized functions
uni_form <- matrix(runif(num_simu*num_assets),
             ncol=num_simu)
sum(uni_form < default_probs)/num_simu
# plot Standard Normal distribution
x11(width=6, height=5)
curve(expr=dnorm(x),
type="l", xlim=c(-4, 4),
xlab="asset value", ylab="", lwd=2,
col="blue", main="Distribution of Asset Values")
abline(v=qnorm(0.025), col="red", lwd=2)
text(x=qnorm(0.025)-0.1, y=0.15,
 labels="default threshold",
 lwd=2, srt=90, pos=3)
# define correlation parameters
rh_o <- 0.05
rho_sqrt <- sqrt(rh_o)
rho_sqrtm <- sqrt(1-rh_o)
# calculate default thresholds
default_thresh <- qnorm(default_probs)
# calculate vector of systematic factors
system_atic <- rnorm(num_simu)
# allocate vector of defaults
de_faults <- numeric(num_simu)
# perform loop to calculate de_faults
for (i in 1:num_simu) {
  asset_values <-
    rho_sqrt*system_atic[i] +
    rho_sqrtm*rnorm(num_assets)
  de_faults[i] <-
    sum(asset_values < default_thresh)
}  # end for
# define default probability density function
vasi_cek <- function(x, def_thresh=-2, rh_o=0.08)
  sqrt((1-rh_o)/rh_o)*exp(-(sqrt(1-rh_o)*qnorm(x) - def_thresh)^2/(2*rh_o) + qnorm(x)^2/2)
vasi_cek(0.03, def_thresh=qnorm(0.025), rh_o=0.1)
# plot probability distribution of defaults
curve(expr=vasi_cek(x, def_thresh=qnorm(0.025), rh_o=0.02),
type="l", xlim=c(0, 0.1), lwd=3,
xlab="fraction of defaults", ylab="density",
col="green", main="Distribution of defaults")
# plot default distribution with higher correlation
curve(expr=vasi_cek(x, def_thresh=qnorm(0.025), rh_o=0.08),
type="l", xlim=c(0, 0.1), add=TRUE,
xlab="default fraction", ylab="", lwd=3,
col="blue", main="")
# add legend
legend(x="topright", legend=c("high correlation", "low correlation"),
 title=NULL, inset=0.05, cex=0.8, bg="white",
 lwd=6, lty=c(1, 1), col=c("blue", "green"))
# add unconditional default probability
abline(v=0.025, col="red", lwd=3)
text(x=0.023, y=8,
 labels="default probability",
 lwd=2, srt=90, pos=3)
# plot default distribution with low correlation
curve(expr=vasi_cek(x, def_thresh=qnorm(0.1), rh_o=0.01),
type="l", xlab="default fraction", ylab="",
lwd=2, col="green", main="Distribution of defaults")
# plot default distribution with high correlation
curve(expr=vasi_cek(x, def_thresh=qnorm(0.1), rh_o=0.99),
type="l", add=TRUE, lwd=2, n=10001,
xlab="fraction of defaults", ylab="density",
col="blue", main="")
# add legend
legend(x="topright", legend=c("high correlation", "low correlation"),
 title=NULL, inset=0.1, cex=0.8, bg="white",
 lwd=6, lty=c(1, 1), col=c("blue", "green"))
# add unconditional default probability
abline(v=0.1, col="red", lwd=2)
text(x=0.1, y=10,
 labels="default probability",
 lwd=2, pos=4)
# define Vasicek loss distribution density function
portf_loss <- function(x, def_thresh=-2, rh_o=0.08, l_gd=0.4)
  sqrt((1-rh_o)/rh_o)*exp(-(sqrt(1-rh_o)*qnorm(x/l_gd) - def_thresh)^2/(2*rh_o) + qnorm(x/l_gd)^2/2)/l_gd
integrate(portf_loss, low=0, up=0.3, def_thresh=-2, rh_o=0.08, l_gd=0.4)
# plot probability distribution of losses
curve(expr=portf_loss(x, def_thresh=qnorm(0.05), rh_o=0.08),
type="l", xlim=c(0, 0.06),
xlab="loss fraction", ylab="density", lwd=3,
col="orange", main="Distribution of Losses")
# add line for expected loss
abline(v=0.02, col="red", lwd=3)
text(x=0.02-0.001, y=10, labels="expected loss",
 lwd=2, srt=90, pos=3)
# add lines for unexpected loss
abline(v=0.04, col="blue", lwd=3)
arrows(x0=0.02, y0=35, x1=0.04, y1=35,
 code=3, lwd=3, cex=0.5)
text(x=0.03, y=36, labels="unexpected loss",
     lwd=2, pos=3)
# add lines for VaR
abline(v=0.055, col="red", lwd=3)
arrows(x0=0.0, y0=25, x1=0.055, y1=25,
 code=3, lwd=3, cex=0.5)
text(x=0.03, y=26, labels="VaR", lwd=2, pos=3)
text(x=0.055-0.001, y=10, labels="VaR",
 lwd=2, srt=90, pos=3)
# plot probability distribution of losses
curve(expr=portf_loss(x, def_thresh=qnorm(0.05), rh_o=0.08),
type="l", xlim=c(0, 0.06),
xlab="loss fraction", ylab="density", lwd=3,
col="orange", main="Conditional Value at Risk")
# add line for expected loss
abline(v=0.02, col="red", lwd=3)
text(x=0.02-0.001, y=10, labels="expected loss",
 lwd=2, srt=90, pos=3)
# add lines for VaR
abline(v=0.04, col="red", lwd=3)
text(x=0.04-0.001, y=10, labels="VaR",
 lwd=2, srt=90, pos=3)
# add shading for CVaR
v_ar <- 0.04; var_max <- 0.07
var_s <- seq(v_ar, var_max, length=100)
dens_ity <- sapply(var_s,
  portf_loss, def_thresh=qnorm(0.05), rh_o=0.08)
# draw shaded polygon
polygon(c(v_ar, var_s, var_max),
  c(-1, dens_ity, -1), col="red", border=NA)
text(x=0.045, y=0, labels="CVaR", lwd=2, pos=3)
# integrate portf_loss() over full range
integrate(portf_loss, low=0.0, up=0.3,
    def_thresh=qnorm(0.05), rh_o=0.08, l_gd=0.4)
# calculate expected losses using portf_loss()
integrate(function(x, ...) x*portf_loss(x, ...),
    low=0.0, up=0.3,
    def_thresh=qnorm(0.05), rh_o=0.08, l_gd=0.4)
# calculate confidence levels corresponding to VaR values
var_s <- seq(0.04, 0.06, 0.001)
conf_levels <- sapply(var_s, function(v_ar, ...) {
  integrate(portf_loss, low=v_ar, up=0.3, ...)
}, def_thresh=qnorm(0.05), rh_o=0.08, l_gd=0.4)  # end sapply
conf_levels <- cbind(as.numeric(t(conf_levels)[, 1]), var_s)
colnames(conf_levels) <- c("conf_levels", "VaRs")
# calculate 95% confidence level VaR value
conf_levels[
  match(TRUE, conf_levels[, "conf_levels"] < 0.05),
  "VaRs"]
plot(x=1-conf_levels[, "conf_levels"],
     y=conf_levels[, "VaRs"],
     xlab="conf_levels", ylab="VaRs",
     t="l", main="VaR values and confidence levels")
# calculate CVaR values
cvar_s <- sapply(var_s, function(v_ar, ...) {
  integrate(function(x, ...) x*portf_loss(x, ...),
      low=v_ar, up=0.3, ...)
}, def_thresh=qnorm(0.05), rh_o=0.08, l_gd=0.4)  # end sapply
conf_levels <- cbind(conf_levels, as.numeric(t(cvar_s)[, 1]))
colnames(conf_levels)[3] <- "CVaRs"
# divide CVaR by confidence level
conf_levels[, "CVaRs"] <- conf_levels[, "CVaRs"]/conf_levels[, "conf_levels"]
# calculate 95% confidence level CVaR value
conf_levels[
  match(TRUE, conf_levels[, "conf_levels"] < 0.05),
  "CVaRs"]
# plot CVaRs
plot(x=1-conf_levels[, "conf_levels"],
     y=conf_levels[, "CVaRs"],
     t="l", col="red", lwd=2,
     ylim=range(conf_levels[, c("VaRs", "CVaRs")]),
     xlab="conf_levels", ylab="CVaRs",
     main="CVaR values and confidence levels")
# add VaRs
lines(x=1-conf_levels[, "conf_levels"],
y=conf_levels[, "VaRs"], lwd=2)
# add legend
legend(x="topleft", legend=c("CVaRs", "VaRs"),
 title=NULL, inset=0.05, cex=0.8, bg="white",
 lwd=6, lty=c(1, 1), col=c("red", "black"))
# Define model parameters
num_assets <- 300
num_simu <- 1000
l_gd <- 0.4
# define correlation parameters
rh_o <- 0.08
rho_sqrt <- sqrt(rh_o)
rho_sqrtm <- sqrt(1-rh_o)
# calculate default probabilities and thresholds
set.seed(1121)
default_probs <- runif(num_assets, max=0.1)
default_thresh <- qnorm(default_probs)
# calculate vector of systematic factors
system_atic <- rnorm(num_simu)
# simulate losses under Vasicek model
asset_values <- matrix(rnorm(num_simu*num_assets), ncol=num_simu)
asset_values <- t(rho_sqrt*system_atic + t(rho_sqrtm*asset_values))
loss_es <-
  l_gd*colSums(asset_values < default_thresh)/num_assets
# calculate VaRs
conf_levels <- seq(0.93, 0.99, 0.01)
var_s <- quantile(loss_es, probs=conf_levels)
names(var_s) <- round(conf_levels, 3)
plot(x=conf_levels, y=var_s, t="l",
     main="Simulated VaR and confidence levels")
# calculate frequency table of loss_es
table_losses <- table(loss_es)/num_simu
# calculate CVaRs
cvar_s <- sapply(var_s, function(v_ar) {
  tai_l <- table_losses[v_ar < names(table_losses)]
  tai_l %*% as.numeric(names(tai_l)) / sum(tai_l)
})  # end sapply
cvar_s <- cbind(cvar_s, var_s)
# plot CVaRs
plot(x=rownames(cvar_s), y=cvar_s[, "cvar_s"],
     t="l", col="red", lwd=2,
     ylim=range(cvar_s),
     xlab="conf_levels", ylab="CVaRs",
     main="Simulated CVaR and confidence levels")
# add VaRs
lines(x=rownames(cvar_s), y=cvar_s[, "var_s"], lwd=2)
# add legend
legend(x="topleft", legend=c("CVaRs", "VaRs"),
 title=NULL, inset=0.05, cex=0.8, bg="white",
 lwd=6, lty=c(1, 1), col=c("red", "black"))
calc_var <- function(default_probs,
               l_gd=0.6,
               rh_o=0.08,
               num_simu=1000,
               conf_levels=seq(0.93, 0.99, 0.01)) {
  # Define model parameters
  num_assets <- NROW(default_probs)
  default_thresh <- qnorm(default_probs)
  # Define correlation parameters
  rho_sqrt <- sqrt(rh_o)
  rho_sqrtm <- sqrt(1-rh_o)
  # Simulate losses under Vasicek model
  system_atic <- rnorm(num_simu)
  asset_values <- matrix(rnorm(num_simu*num_assets), ncol=num_simu)
  asset_values <- t(rho_sqrt*system_atic + t(rho_sqrtm*asset_values))
  loss_es <- l_gd*colSums(asset_values < default_thresh)/num_assets
  # Calculate VaRs
  var_s <- quantile(loss_es, probs=conf_levels)
  names(var_s) <- round(conf_levels, 3)
  # Calculate CVaRs from the frequency table
  table_losses <- table(loss_es)/num_simu
  cvar_s <- sapply(var_s, function(v_ar) {
    tai_l <- table_losses[v_ar < names(table_losses)]
    tai_l %*% as.numeric(names(tai_l)) / sum(tai_l)
  })  # end sapply
  names(cvar_s) <- round(conf_levels, 3)
  c(var_s, cvar_s)
}  # end calc_var
# define number of bootstrap simulations
num_boot <- 500
num_assets <- NROW(default_probs)
# perform bootstrap of calc_var
set.seed(1121)
boot_strap <- sapply(rep(l_gd, num_boot),
  calc_var,
  default_probs=
    default_probs[sample.int(num_assets, replace=TRUE)],
  rh_o=rh_o, num_simu=num_simu,
  conf_levels=conf_levels)  # end sapply
boot_strap <- t(boot_strap)
# calculate vectors of standard errors of VaR and CVaR from boot_strap data
std_error_var <- apply(boot_strap[, 1:7], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
std_error_cvar <- apply(boot_strap[, 8:14], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
# scale the standard errors of VaRs and CVaRs
std_error_var[2, ] <- std_error_var[2, ]/std_error_var[1, ]
std_error_cvar[2, ] <- std_error_cvar[2, ]/std_error_cvar[1, ]
# plot the standard errors of VaRs and CVaRs
plot(x=colnames(std_error_cvar),
  y=std_error_cvar[2, ], t="l", col="red", lwd=2,
  ylim=range(c(std_error_var[2, ], std_error_cvar[2, ])),
  xlab="conf_levels", ylab="CVaRs",
  main="Scaled standard errors of CVaR and VaR")
lines(x=colnames(std_error_var), y=std_error_var[2, ], lwd=2)
legend(x="topleft", legend=c("CVaRs", "VaRs"),
 title=NULL, inset=0.05, cex=0.8, bg="white",
 lwd=6, lty=c(1, 1), col=c("red", "black"))
library(parallel)  # load package parallel
num_cores <- detectCores() - 1  # number of cores
clus_ter <- makeCluster(num_cores)  # initialize compute cluster
# perform bootstrap of calc_var for Windows
set.seed(1121)
boot_strap <- parLapply(clus_ter, rep(l_gd, num_boot),
  fun=calc_var,
  default_probs=default_probs[sample.int(num_assets, replace=TRUE)],
  rh_o=rh_o, num_simu=num_simu,
  conf_levels=conf_levels)  # end parLapply
# bootstrap under Mac-OSX or Linux
boot_strap <- mclapply(rep(l_gd, num_boot),
  fun=calc_var,
  default_probs=default_probs[sample.int(num_assets, replace=TRUE)],
  rh_o=rh_o, num_simu=num_simu,
  conf_levels=conf_levels)  # end mclapply
boot_strap <- do.call(rbind, boot_strap)
stopCluster(clus_ter)  # stop R processes over cluster
# calculate vectors of standard errors of VaR and CVaR from boot_strap data
std_error_var <- apply(boot_strap[, 1:7], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
std_error_cvar <- apply(boot_strap[, 8:14], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
# scale the standard errors of VaRs and CVaRs
std_error_var[2, ] <- std_error_var[2, ]/std_error_var[1, ]
std_error_cvar[2, ] <- std_error_cvar[2, ]/std_error_cvar[1, ]
# plot the standard errors of VaRs and CVaRs
plot(x=colnames(std_error_cvar),
  y=std_error_cvar[2, ], t="l", col="red", lwd=2,
  ylim=range(c(std_error_var[2, ], std_error_cvar[2, ])),
  xlab="conf_levels", ylab="CVaRs",
  main="Scaled standard errors of CVaR and VaR")
lines(x=colnames(std_error_var), y=std_error_var[2, ], lwd=2)
legend(x="topleft", legend=c("CVaRs", "VaRs"),
 title=NULL, inset=0.05, cex=0.8, bg="white",
 lwd=6, lty=c(1, 1), col=c("red", "black"))
