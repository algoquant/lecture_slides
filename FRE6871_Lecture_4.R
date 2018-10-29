set.seed(1121)  # reset random number generator
# sample from Standard Normal Distribution
len_gth <- 1000; r_norm <- rnorm(len_gth)
# sample mean and standard deviation
mean(r_norm); sd(r_norm)
# bootstrap of sample mean and median
boot_strap <- sapply(1:10000, function(x) {
  boot_sample <- r_norm[sample.int(len_gth,
                              replace=TRUE)]
  c(mean=mean(boot_sample), median=median(boot_sample))
})  # end sapply
boot_strap <- t(boot_strap)
boot_strap[1:3, ]
# standard error from formula
sd(r_norm)/sqrt(len_gth)
# standard error of mean from bootstrap
sd(boot_strap[, "mean"])
# standard error of median from bootstrap
sd(boot_strap[, "median"])
plot(density(boot_strap[, "median"]),
     lwd=2, xlab="regression slopes",
     main="Distribution of Bootstrapped Median")
abline(v=mean(boot_strap[, "median"]),
 lwd=2, col="red")
set.seed(1121)  # reset random number generator
len_gth <- 1000
r_norm <- rnorm(len_gth)
# bootstrap of sample mean and median
boot_strap <- sapply(1:10000, function(x) {
  # boot_sample from Standard Normal Distribution
  boot_sample <- rnorm(len_gth)
  c(mean=mean(boot_sample),
    median=median(boot_sample))
})  # end sapply
boot_strap[, 1:3]
# standard error from formula
sd(r_norm)/sqrt(len_gth)
# standard error of mean from bootstrap
sd(boot_strap["mean", ])
# standard error of median from bootstrap
sd(boot_strap["median", ])
calc_var <- function(thresh_olds, # default thresholds
               l_gd=0.6, # loss given default
               rho_sqrt, rho_sqrtm, # asset correlation
               n_simu=1000, # number of simulations
               level_s=seq(0.93, 0.99, 0.01) # confidence levels
               ) {
  # Define model parameters
  n_assets <- NROW(thresh_olds)
  # Simulate losses under Vasicek model
  system_atic <- rnorm(n_simu)
  asset_s <- matrix(rnorm(n_simu*n_assets), ncol=n_simu)
  asset_s <- t(rho_sqrt*system_atic + t(rho_sqrtm*asset_s))
  loss_es <- l_gd*colSums(asset_s < thresh_olds)/n_assets
  # Calculate VaRs and CVaRs
  var_s <- quantile(loss_es, probs=level_s)
  cvar_s <- sapply(var_s, function(va_r) {
    mean(loss_es[loss_es >=va_r])
  })  # end sapply
  names(var_s) <- level_s
  names(cvar_s) <- level_s
  c(var_s, cvar_s)
}  # end calc_var
# Calculate default probabilities and thresholds
set.seed(1121)
default_probs <- runif(n_assets, max=0.2)
thresh_olds <- qnorm(default_probs)
# Define number of bootstrap simulations
n_boot <- 500
n_assets <- NROW(default_probs)
# Perform bootstrap of calc_var
set.seed(1121)
boot_strap <- sapply(rep(l_gd, n_boot), calc_var,
  thresh_olds=thresh_olds,
  rho_sqrt=rho_sqrt, rho_sqrtm=rho_sqrtm,
  n_simu=n_simu, level_s=level_s)  # end sapply
boot_strap <- t(boot_strap)
# Calculate vectors of standard errors of VaR and CVaR from boot_strap data
std_error_var <- apply(boot_strap[, 1:7], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
std_error_cvar <- apply(boot_strap[, 8:14], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
# Scale the standard errors of VaRs and CVaRs
std_error_var[2, ] <- std_error_var[2, ]/std_error_var[1, ]
std_error_cvar[2, ] <- std_error_cvar[2, ]/std_error_cvar[1, ]
# Plot the standard errors of VaRs and CVaRs
plot(x=colnames(std_error_cvar),
  y=std_error_cvar[2, ], t="l", col="red", lwd=2,
  ylim=range(c(std_error_var[2, ], std_error_cvar[2, ])),
  xlab="level_s", ylab="CVaRs",
  main="Scaled standard errors of CVaR and VaR")
lines(x=colnames(std_error_var), y=std_error_var[2, ], lwd=2)
legend(x="topleft", legend=c("CVaRs", "VaRs"), bty="n",
 title=NULL, inset=0.05, cex=0.8, bg="white",
 lwd=6, lty=1, col=c("red", "black"))
NA
library(parallel)  # load package parallel
n_cores <- detectCores() - 1  # number of cores
clus_ter <- makeCluster(n_cores)  # initialize compute cluster
# Perform bootstrap of calc_var for Windows
clusterSetRNGStream(clus_ter, 1121)
boot_strap <- parLapply(clus_ter, rep(l_gd, n_boot),
  fun=calc_var, thresh_olds=thresh_olds,
  rho_sqrt=rho_sqrt, rho_sqrtm=rho_sqrtm,
  n_simu=n_simu, level_s=level_s)  # end parLapply
# Bootstrap under Mac-OSX or Linux
boot_strap <- mclapply(rep(l_gd, n_boot),
  FUN=calc_var, thresh_olds=thresh_olds,
  rho_sqrt=rho_sqrt, rho_sqrtm=rho_sqrtm,
  n_simu=n_simu, level_s=level_s)  # end mclapply
boot_strap <- rutils::do_call(rbind, boot_strap)
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
  xlab="level_s", ylab="CVaRs",
  main="Scaled standard errors of CVaR and VaR")
lines(x=colnames(std_error_var), y=std_error_var[2, ], lwd=2)
legend(x="topleft", legend=c("CVaRs", "VaRs"), bty="n",
 title=NULL, inset=0.05, cex=0.8, bg="white",
 lwd=6, lty=1, col=c("red", "black"))
calc_var <- function(default_probs, # default probabilities
               l_gd=0.6, # loss given default
               rho_sqrt, rho_sqrtm, # asset correlation
               n_simu=1000, # number of simulations
               level_s=seq(0.93, 0.99, 0.01) # confidence levels
               ) {
  # Calculate default thresholds
  thresh_olds <- qnorm(runif(1, min=0.5, max=1.5)*default_probs)
  # Simulate losses under Vasicek model
  n_assets <- NROW(default_probs)
  system_atic <- rnorm(n_simu)
  asset_s <- matrix(rnorm(n_simu*n_assets), ncol=n_simu)
  asset_s <- t(rho_sqrt*system_atic + t(rho_sqrtm*asset_s))
  loss_es <- l_gd*colSums(asset_s < thresh_olds)/n_assets
  # Calculate VaRs and CVaRs
  var_s <- quantile(loss_es, probs=level_s)
  cvar_s <- sapply(var_s, function(va_r) {
    mean(loss_es[loss_es >=va_r])
  })  # end sapply
  names(var_s) <- level_s
  names(cvar_s) <- level_s
  c(var_s, cvar_s)
}  # end calc_var
library(parallel)  # load package parallel
n_cores <- detectCores() - 1  # number of cores
clus_ter <- makeCluster(n_cores)  # initialize compute cluster
# Perform bootstrap of calc_var for Windows
clusterSetRNGStream(clus_ter, 1121)
boot_strap <- parLapply(clus_ter, rep(l_gd, n_boot),
  fun=calc_var, default_probs=default_probs,
  rho_sqrt=rho_sqrt, rho_sqrtm=rho_sqrtm,
  n_simu=n_simu, level_s=level_s)  # end parLapply
# Bootstrap under Mac-OSX or Linux
boot_strap <- mclapply(rep(l_gd, n_boot),
  FUN=calc_var, default_probs=default_probs,
  rho_sqrt=rho_sqrt, rho_sqrtm=rho_sqrtm,
  n_simu=n_simu, level_s=level_s)  # end mclapply
boot_strap <- rutils::do_call(rbind, boot_strap)
stopCluster(clus_ter)  # stop R processes over cluster
# calculate vectors of standard errors of VaR and CVaR from boot_strap data
std_error_var <- apply(boot_strap[, 1:7], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
std_error_cvar <- apply(boot_strap[, 8:14], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
# Plot the standard errors of VaRs and CVaRs
plot(x=colnames(std_error_cvar),
  y=std_error_cvar[2, ], t="l", col="red", lwd=2,
  ylim=range(c(std_error_var[2, ], std_error_cvar[2, ])),
  xlab="level_s", ylab="CVaRs",
  main="Standard Errors of CVaR and VaR
  with Uncertain Default Probabilities")
lines(x=colnames(std_error_var), y=std_error_var[2, ], lwd=2)
legend(x="topleft", legend=c("CVaRs", "VaRs"), bty="n",
 title=NULL, inset=0.05, cex=0.8, bg="white",
 lwd=6, lty=1, col=c("red", "black"))
vec_tor1 <- rnorm(1000000)
vec_tor2 <- rnorm(1000000)
big_vector <- numeric(1000000)
# sum two vectors in two different ways
summary(microbenchmark(
  # sum vectors using "for" loop
  r_loop=(for (i in 1:NROW(vec_tor1)) {
    big_vector[i] <- vec_tor1[i] + vec_tor2[i]
  }),
  # sum vectors using vectorized "+"
  vec_torized=(vec_tor1 + vec_tor2),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# allocate memory for cumulative sum
cum_sum <- numeric(NROW(big_vector))
cum_sum[1] <- big_vector[1]
# calculate cumulative sum in two different ways
summary(microbenchmark(
# cumulative sum using "for" loop
  r_loop=(for (i in 2:NROW(big_vector)) {
    cum_sum[i] <- cum_sum[i-1] + big_vector[i]
  }),
# cumulative sum using "cumsum"
  vec_torized=cumsum(big_vector),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# calculate row sums two different ways
summary(microbenchmark(
  row_sums=rowSums(big_matrix),
  ap_ply=apply(big_matrix, 1, sum),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
library(microbenchmark)
str(pmax)
# calculate row maximums two different ways
summary(microbenchmark(
  p_max=
    do.call(pmax.int,
lapply(seq_along(big_matrix[1, ]),
  function(in_dex) big_matrix[, in_dex])),
  l_apply=unlist(
    lapply(seq_along(big_matrix[, 1]),
  function(in_dex) max(big_matrix[in_dex, ]))),
  times=10))[, c(1, 4, 5)]
library(matrixStats)  # load package matrixStats
# calculate row min values three different ways
summary(microbenchmark(
  row_mins=rowMins(big_matrix),
  p_min=
    do.call(pmin.int,
      lapply(seq_along(big_matrix[1, ]),
             function(in_dex)
               big_matrix[, in_dex])),
  as_data_frame=
    do.call(pmin.int,
      as.data.frame.matrix(big_matrix)),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
summary(microbenchmark(  # assign values to vector three different ways
# fast vectorized assignment loop performed in C using brackets "[]"
  brack_ets={vec_tor <- numeric(10)
    vec_tor[] <- 2},
# slow because loop is performed in R
  for_loop={vec_tor <- numeric(10)
    for (in_dex in seq_along(vec_tor))
      vec_tor[in_dex] <- 2},
# very slow because no memory is pre-allocated
# "vec_tor" is "grown" with each new element
  grow_vec={vec_tor <- numeric(0)
    for (in_dex in 1:10)
# add new element to "vec_tor" ("grow" it)
      vec_tor[in_dex] <- 2},
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
summary(microbenchmark(  # assign values to vector two different ways
# fast vectorized assignment loop performed in C using brackets "[]"
  brack_ets={vec_tor <- numeric(10)
    vec_tor[4:7] <- rnorm(4)},
# slow because loop is performed in R
  for_loop={vec_tor <- numeric(10)
    for (in_dex in 4:7)
      vec_tor[in_dex] <- rnorm(1)},
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# define function vectorized automatically
my_fun <- function(in_put, pa_ram) {
  pa_ram*in_put
}  # end my_fun
# "in_put" is vectorized
my_fun(in_put=1:3, pa_ram=2)
# "pa_ram" is vectorized
my_fun(in_put=10, pa_ram=2:4)
# define vectors of parameters of rnorm()
std_devs <-
  structure(1:3, names=paste0("sd=", 1:3))
me_ans <-
  structure(-1:1, names=paste0("mean=", -1:1))
# "sd" argument of rnorm() isn't vectorized
rnorm(1, sd=std_devs)
# "mean" argument of rnorm() isn't vectorized
rnorm(1, mean=me_ans)
# sapply produces desired vector output
set.seed(1121)
sapply(std_devs, function(std_dev) rnorm(n=2, sd=std_dev))
set.seed(1121)
sapply(std_devs, rnorm, n=2, mean=0)
set.seed(1121)
sapply(me_ans,
 function(me_an) rnorm(n=2, mean=me_an))
set.seed(1121)
sapply(me_ans, rnorm, n=2)
# rnorm() vectorized with respect to "std_dev"
vec_rnorm <- function(n, mean=0, sd=1) {
  if (NROW(sd)==1)
    rnorm(n=n, mean=mean, sd=sd)
  else
    sapply(sd, rnorm, n=n, mean=mean)
}  # end vec_rnorm
set.seed(1121)
vec_rnorm(n=2, sd=std_devs)
# rnorm() vectorized with respect to "mean" and "sd"
vec_rnorm <- Vectorize(FUN=rnorm,
        vectorize.args=c("mean", "sd")
)  # end Vectorize
set.seed(1121)
vec_rnorm(n=2, sd=std_devs)
set.seed(1121)
vec_rnorm(n=2, mean=me_ans)
str(sum)
# na.rm is bound by name
mapply(sum, 6:9, c(5, NA, 3), 2:6, na.rm=TRUE)
str(rnorm)
# mapply vectorizes both arguments "mean" and "sd"
mapply(rnorm, n=5, mean=me_ans, sd=std_devs)
mapply(function(in_put, e_xp) in_put^e_xp,
 1:5, seq(from=1, by=0.2, length.out=5))
# rnorm() vectorized with respect to "mean" and "sd"
vec_rnorm <- function(n, mean=0, sd=1) {
  if (NROW(mean)==1 && NROW(sd)==1)
    rnorm(n=n, mean=mean, sd=sd)
  else
    mapply(rnorm, n=n, mean=mean, sd=sd)
}  # end vec_rnorm
# call vec_rnorm() on vector of "sd"
vec_rnorm(n=2, sd=std_devs)
# call vec_rnorm() on vector of "mean"
vec_rnorm(n=2, mean=me_ans)
# create two numeric vectors
vec_tor1 <- sin(0.25*pi*1:10)
vec_tor2 <- cos(0.25*pi*1:10)
# create third vector using 'ifelse'
vec_tor3 <- ifelse(vec_tor1 > vec_tor2,
          vec_tor1, vec_tor2)
# cbind all three together
vec_tor4 <- cbind(vec_tor1, vec_tor2, vec_tor3)

# set plotting parameters
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0),
    cex.lab=0.8, cex.axis=0.8, cex.main=0.8,
    cex.sub=0.5)
# plot matrix
matplot(vec_tor4, type="l", lty="solid",
col=c("green", "blue", "red"),
lwd=c(2, 2, 2), xlab="", ylab="")
# add legend
legend(x="bottomright", legend=colnames(vec_tor4),
       title="", inset=0.05, cex=0.8, lwd=2,
       lty=c(1, 1, 1), col=c("green", "blue", "red"))
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
left_mat <- crossprod(left_mat)
# or
left_mat <- left_mat %*% t(left_mat)
# calculate left eigenvectors
ei_gen <- eigen(left_mat)
left_mat <- ei_gen$vectors[, 1:n_right]
# create random positive semi-definite matrix
right_mat <- matrix(runif(n_right^2), nc=n_right)
right_mat <- crossprod(right_mat)
# or
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
set.seed(1121)  # reset random number generator
runif(3)  # three numbers from uniform distribution
runif(3)  # produce another three numbers
set.seed(1121)  # reset random number generator
runif(3)  # produce another three numbers

# produce random number from standard normal distribution
rnorm(1)
# produce five random numbers from standard normal distribution
rnorm(5)
# produce five random numbers from the normal distribution
rnorm(n=5, mean=1, sd=2)  # match arguments by name
# calculate cumulative standard normal distribution
c(pnorm(-2), pnorm(2))
# calculate inverse cumulative standard normal distribution
c(qnorm(0.75), qnorm(0.25))
# define logistic map function
log_map <- function(x, r=4) r*x*(1-x)
log_map(0.25, 4)
# plot logistic map
x11(width=6, height=5)
curve(expr=log_map, type="l", xlim=c(0, 1),
xlab="x[n-1]", ylab="x[n]", lwd=2, col="blue",
main="logistic map")
lines(x=c(0, 0.25), y=c(0.75, 0.75), lwd=2, col="blue")
lines(x=c(0.25, 0.25), y=c(0, 0.75), lwd=2, col="blue")
# calculate uniformly distributed pseudo-random
# sequence using logistic map function
uni_form <- function(see_d, len_gth=10) {
  # pre-allocate vector instead of "growing" it
  out_put <- numeric(len_gth)
  # initialize
  out_put[1] <- see_d
  # perform loop
  for (i in 2:len_gth) {
    out_put[i] <- 4*out_put[i-1]*(1-out_put[i-1])
  }  # end for
  acos(1-2*out_put)/pi
}  # end uni_form
uni_form(see_d=0.1, len_gth=15)
plot(
  density(uni_form(see_d=runif(1), len_gth=1e5)),
  xlab="", ylab="", lwd=2, col="blue",
  main="uniform pseudo-random number density")
set.seed(1121)  # reset random number generator
# flip unbiased coin once, 20 times
rbinom(n=20, size=1, 0.5)
# number of heads after flipping twice, 20 times
rbinom(n=20, size=2, 0.5)
# number of heads after flipping thrice, 20 times
rbinom(n=20, size=3, 0.5)
# number of heads after flipping biased coin thrice, 20 times
rbinom(n=20, size=3, 0.8)
# number of heads after flipping biased coin thrice, 20 times
rbinom(n=20, size=3, 0.2)
# flip unbiased coin once, 20 times
sample(x=0:1, size=20, replace=TRUE)  # fast
as.numeric(runif(20) < 0.5)  # slower
# permutation of five numbers
sample(x=5)
# permutation of four strings
sample(x=c("apple", "grape", "orange", "peach"))
# sample of size three
sample(x=5, size=3)
# sample with replacement
sample(x=5, replace=TRUE)
sample(  # sample of strings
  x=c("apple", "grape", "orange", "peach"),
  size=12,
  replace=TRUE)
# binomial sample: flip coin once, 20 times
sample(x=0:1, size=20, replace=TRUE)
# flip unbiased coin once, 20 times
as.numeric(runif(20) > 0.5)  # slower
rm(list=ls())
set.seed(1121)  # reset random number generator
# sample from Standard Normal Distribution
sam_ple <- rnorm(1000)

mean(sam_ple)  # sample mean

median(sam_ple)  # sample median

sd(sam_ple)  # sample standard deviation
rm(list=ls())
# DAX returns
re_turns <- diff(log(EuStockMarkets[, 1]))
# number of observations
len_gth <- NROW(re_turns)
# mean of DAX returns
mean_rets <- mean(re_turns)
# standard deviation of DAX returns
sd_rets <- sd(re_turns)
# skew of DAX returns
len_gth/((len_gth-1)*(len_gth-2))*
  sum(((re_turns - mean_rets)/sd_rets)^3)
# kurtosis of DAX returns
len_gth*(len_gth+1)/((len_gth-1)^3)*
  sum(((re_turns - mean_rets)/sd_rets)^4)
# random normal returns
re_turns <- rnorm(len_gth, sd=2)
# mean and standard deviation of random normal returns
mean_rets <- mean(re_turns)
sd_rets <- sd(re_turns)
# skew of random normal returns
len_gth/((len_gth-1)*(len_gth-2))*
  sum(((re_turns - mean_rets)/sd_rets)^3)
# kurtosis of random normal returns
len_gth*(len_gth+1)/((len_gth-1)^3)*
  sum(((re_turns - mean_rets)/sd_rets)^4)
set.seed(1121)  # reset random number generator
# sample from Standard Normal Distribution
len_gth <- 1000
sam_ple <- rnorm(len_gth)
# sample mean
mean(sam_ple)
# sample standard deviation
sd(sam_ple)
#Perform two-tailed test that sample is
#from Standard Normal Distribution (mean=0, SD=1)
# generate vector of samples and store in data frame
test_frame <- data.frame(samples=rnorm(1e4))
# get p-values for all the samples
test_frame$p_values <- sapply(test_frame$samples, 
        function(x) 2*pnorm(-abs(x)))
# significance level, two-tailed test, critical value=2*SD
signif_level <- 2*(1-pnorm(2))
# compare p_values to significance level
test_frame$result <-
  test_frame$p_values > signif_level
# number of null rejections
sum(!test_frame$result) / NROW(test_frame)
# show null rejections
head(test_frame[!test_frame$result, ])
x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
# plot the Normal probability distribution
curve(expr=dnorm(x, sd=1), type="l", xlim=c(-4, 4),
xlab="", ylab="", lwd=3, col="blue")
title(main="Two-tailed Test", line=0.5)
# plot tails of the distribution using polygons
star_t <- 2; e_nd <- 4
# plot right tail using polygon
x_var <- seq(star_t, e_nd, length=100)
y_var <- dnorm(x_var, sd=1)
y_var[1] <- (-1)
y_var[NROW(y_var)] <- (-1)
polygon(x=x_var, y=y_var, col="red")
# plot left tail using polygon
y_var <- dnorm(-x_var, sd=1)
y_var[1] <- (-1)
y_var[NROW(y_var)] <- (-1)
polygon(x=(-x_var), y=y_var, col="red")
rm(list=ls())
par(oma=c(1, 1, 1, 1), mgp=c(2, 0.5, 0), mar=c(5, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
library(ggplot2)  # load ggplot2

qplot(  # simple ggplot2
    main="Standard Normal Distribution",
    c(-4, 4),
    stat="function",
    fun=dnorm,
    geom="line",
    xlab=NULL, ylab=NULL
    ) +  # end qplot

theme(  # modify plot theme
    plot.title=element_text(vjust=-1.0),
    plot.background=element_blank()
    ) +  # end theme

geom_vline(  # add vertical line
  aes(xintercept=c(-2.0, 2.0)),
  colour="red",
  linetype="dashed"
  )  # end geom_vline
rm(list=ls())
par(oma=c(1, 1, 1, 1), mgp=c(2, 0.5, 0), mar=c(5, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
#create ggplot2 with shaded area
x_var <- -400:400/100
norm_frame <- data.frame(x_var=x_var,
                 d.norm=dnorm(x_var))
norm_frame$shade <- ifelse(
            abs(norm_frame$x_var) >= 2,
            norm_frame$d.norm, NA)
ggplot(  # main function
  data=norm_frame,
  mapping=aes(x=x_var, y=d.norm)
  ) +  # end ggplot
# plot line
  geom_line() +
# plot shaded area
  geom_ribbon(aes(ymin=0, ymax=shade), fill="red") +
# no axis labels
  xlab("") + ylab("") +
# add title
  ggtitle("Standard Normal Distribution") +
# modify plot theme
  theme(
  plot.title=element_text(vjust=-1.0),
  plot.background=element_blank()
  )  # end theme
# t-test for single sample
t.test(rnorm(100))
# t-test for two samples
t.test(rnorm(100),
       rnorm(100, mean=1))
# Wilcoxon test for normal distribution
wilcox.test(rnorm(100))
# Wilcoxon test for two normal distributions
wilcox.test(rnorm(100), rnorm(100, mean=0.1))
# Wilcoxon test for two normal distributions
wilcox.test(rnorm(100), rnorm(100, mean=1.0))
# Wilcoxon test for a uniform versus normal distribution
wilcox.test(runif(100), rnorm(100))
# KS test for normal distribution
ks.test(rnorm(100), pnorm)
# KS test for uniform distribution
ks.test(runif(100), pnorm)
# KS test for two similar normal distributions
ks.test(rnorm(100), rnorm(100, mean=0.1))
# KS test for two different normal distributions
ks.test(rnorm(100), rnorm(100, mean=1.0))
# calculate DAX percentage returns
dax_rets <- diff(log(EuStockMarkets[, 1]))

# Shapiro-Wilk test for normal distribution
shapiro.test(rnorm(NROW(dax_rets)))

# Shapiro-Wilk test for DAX returns
shapiro.test(dax_rets)

# Shapiro-Wilk test for uniform distribution
shapiro.test(runif(NROW(dax_rets)))
dax_rets <- diff(log(EuStockMarkets[, 1]))
library(tseries)  # load package tseries

# Jarque-Bera test for normal distribution
jarque.bera.test(rnorm(NROW(dax_rets)))

# Jarque-Bera test for DAX returns
jarque.bera.test(dax_rets)

# Jarque-Bera test for uniform distribution
jarque.bera.test(runif(NROW(dax_rets)))
# formula of linear model with zero intercept
for_mula <- z ~ x + y - 1
for_mula

# collapse vector of strings into single text string
paste0("x", 1:5)
paste(paste0("x", 1:5), collapse="+")

# create formula from text string
for_mula <- as.formula(
  # coerce text strings to formula
  paste("z ~ ",
  paste(paste0("x", 1:5), collapse="+")
  )  # end paste
)  # end as.formula
class(for_mula)
for_mula
# modify the formula using "update"
update(for_mula, log(.) ~ . + beta)
set.seed(1121)  # initialize random number generator
# define explanatory (design) variable
len_gth <- 100
de_sign <- runif(len_gth)
noise <- rnorm(len_gth)
# response equals linear form plus random noise
res_ponse <- (1 + de_sign + noise)
# calculate de-meaned explanatory (design) and response vectors
design_zm <- de_sign - mean(de_sign)
response_zm <- res_ponse - mean(res_ponse)
# solve for the regression beta
be_ta <- sum(design_zm*response_zm) / sum(design_zm^2)
# solve for the regression alpha
al_pha <- mean(res_ponse) - be_ta*mean(de_sign)
# specify regression formula
for_mula <- res_ponse ~ de_sign
mod_el <- lm(for_mula)  # perform regression
class(mod_el)  # regressions have class lm
attributes(mod_el)
eval(mod_el$call$formula)  # regression formula
mod_el$coeff  # regression coefficients
all.equal(coef(mod_el), c(al_pha, be_ta), 
  check.attributes=FALSE)
x11(width=5, height=4)  # open x11 for plotting
# set plot parameters to reduce whitespace around plot
par(mar=c(5, 5, 2, 1), oma=c(0, 0, 0, 0))
# plot scatterplot using formula
plot(for_mula, xlab="design", ylab="response")
title(main="Simple Regression", line=0.5)
# add regression line
abline(mod_el, lwd=3, col="blue")
# plot fitted (predicted) response values
points(x=de_sign, y=mod_el$fitted.values,
       pch=16, col="blue")
# plot response without noise
lines(x=de_sign, y=(res_ponse-noise),
      col="red", lwd=3)
legend(x="topleft", # add legend
       legend=c("response without noise", "fitted values"),
       title=NULL, inset=0.08, cex=0.8, lwd=6,
       lty=1, col=c("red", "blue"))
# sum of residuals = 0
sum(mod_el$residuals)
x11(width=6, height=5)  # open x11 for plotting
# set plot parameters to reduce whitespace around plot
par(mar=c(5, 5, 1, 1), oma=c(0, 0, 0, 0))
# extract residuals
resi_duals <- cbind(de_sign, mod_el$residuals)
colnames(resi_duals) <- c("design", "residuals")
# plot residuals
plot(resi_duals)
title(main="Residuals of the Linear Regression", line=-1)
abline(h=0, lwd=3, col="red")
model_sum <- summary(mod_el)  # copy regression summary
model_sum  # print the summary to console
attributes(model_sum)$names  # get summary elements
model_sum$coeff
model_sum$r.squared
model_sum$adj.r.squared
model_sum$fstatistic
# standard error of beta
model_sum$
  coefficients["de_sign", "Std. Error"]
sd(model_sum$residuals)/sd(de_sign)/
  sqrt(unname(model_sum$fstatistic[3]))
anova(mod_el)
set.seed(1121)  # initialize random number generator
# high noise compared to coefficient
res_ponse <- (1 + de_sign + rnorm(30, sd=8))
mod_el <- lm(for_mula)  # perform regression
# values of regression coefficients are not
# statistically significant
summary(mod_el)
par(oma=c(1, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=1.0, cex.axis=1.0, cex.main=1.0, cex.sub=1.0)
reg_stats <- function(std_dev) {  # noisy regression
  set.seed(1121)  # initialize number generator
# define explanatory (design) and response variables
  de_sign <- rnorm(100, mean=2)
  res_ponse <- (1 + 0.2*de_sign +
    rnorm(NROW(de_sign), sd=std_dev))
# specify regression formula
  for_mula <- res_ponse ~ de_sign
# perform regression and get summary
  model_sum <- summary(lm(for_mula))
# extract regression statistics
  with(model_sum, c(pval=coefficients[2, 4],
   adj_rsquared=adj.r.squared,
   fstat=fstatistic[1]))
}  # end reg_stats
# apply reg_stats() to vector of std dev values
vec_sd <- seq(from=0.1, to=0.5, by=0.1)
names(vec_sd) <- paste0("sd=", vec_sd)
mat_stats <- t(sapply(vec_sd, reg_stats))
# plot in loop
par(mfrow=c(NCOL(mat_stats), 1))
for (in_dex in 1:NCOL(mat_stats)) {
  plot(mat_stats[, in_dex], type="l",
 xaxt="n", xlab="", ylab="", main="")
  title(main=colnames(mat_stats)[in_dex], line=-1.0)
  axis(1, at=1:(NROW(mat_stats)),
 labels=rownames(mat_stats))
}  # end for
reg_stats <- function(da_ta) {  # get regression
# perform regression and get summary
  col_names <- colnames(da_ta)
  for_mula <-
    paste(col_names[2], col_names[1], sep="~")
  model_sum <- summary(lm(for_mula,
                        data=da_ta))
# extract regression statistics
  with(model_sum, c(pval=coefficients[2, 4],
   adj_rsquared=adj.r.squared,
   fstat=fstatistic[1]))
}  # end reg_stats
# apply reg_stats() to vector of std dev values
vec_sd <- seq(from=0.1, to=0.5, by=0.1)
names(vec_sd) <- paste0("sd=", vec_sd)
mat_stats <-
  t(sapply(vec_sd, function (std_dev) {
    set.seed(1121)  # initialize number generator
# define explanatory (design) and response variables
    de_sign <- rnorm(100, mean=2)
    res_ponse <- (1 + 0.2*de_sign +
rnorm(NROW(de_sign), sd=std_dev))
    reg_stats(data.frame(de_sign, res_ponse))
    }))
# plot in loop
par(mfrow=c(NCOL(mat_stats), 1))
for (in_dex in 1:NCOL(mat_stats)) {
  plot(mat_stats[, in_dex], type="l",
 xaxt="n", xlab="", ylab="", main="")
  title(main=colnames(mat_stats)[in_dex], line=-1.0)
  axis(1, at=1:(NROW(mat_stats)),
 labels=rownames(mat_stats))
}  # end for
# set plot paramaters - margins and font scale
par(oma=c(1,0,1,0), mgp=c(2,1,0), mar=c(2,1,2,1), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2, 2))  # plot 2x2 panels
plot(mod_el)  # plot diagnostic scatterplots
plot(mod_el, which=2)  # plot just Q-Q
library(lmtest)  # load lmtest
# perform Durbin-Watson test
lmtest::dwtest(mod_el)
set.seed(1121)  # initialize random number generator
# define design matrix
len_gth <- 100
n_var <- 5
de_sign <- sapply(1:n_var, function(col_umn) {
  sin(pi*col_umn*((1:len_gth)-(len_gth+1)/2)/len_gth)
})  # end sapply
# add column names
colnames(de_sign) <- paste0("predict", 1:n_var)
# plot design matrix
# matplot(de_sign, type="l", lty="solid", lwd=3)
# define the design weights
weight_s <- runif(n_var, min=(-10), max=10)
# response equals linear form plus random noise
noise <- rnorm(len_gth, sd=0.1)
res_ponse <- (-1 + de_sign %*% weight_s + noise)
# calculate de-meaned design matrix
design_zm <- t(t(de_sign) - colMeans(de_sign))
# or
# design_zm <- apply(design_zm, 2, function(x) (x-mean(x)))
# calculate de-meaned response vector
response_zm <- res_ponse - mean(res_ponse)
# calculate the regression coefficients
beta_s <- MASS::ginv(design_zm) %*% response_zm
# solve for the regression alpha
al_pha <- mean(res_ponse) - 
  sum(colSums(de_sign)*drop(beta_s))/len_gth
# perform multivariate regression using lm()
mod_el <- lm(res_ponse ~ de_sign)
# compare with coefficients from lm()
all.equal(coef(mod_el), c(al_pha, beta_s), check.attributes=FALSE)
# compare with actual coefficients
all.equal(c(-1, weight_s), c(al_pha, beta_s), check.attributes=FALSE)
# calculate fitted values from regression coefficients
fit_ted <- drop(al_pha + de_sign %*% beta_s)
all.equal(fit_ted, mod_el$fitted.values, check.attributes=FALSE)
# calculate fitted values from zero mean data
fit_ted <- drop(mean(res_ponse) + design_zm %*% beta_s)
all.equal(fit_ted, mod_el$fitted.values, check.attributes=FALSE)
# calculate the residuals
resid_uals <- drop(res_ponse - fit_ted)
all.equal(resid_uals, mod_el$residuals, check.attributes=FALSE)
# the residuals have zero mean
all.equal(sum(resid_uals), target=0)
# the residuals are orthogonal to the predictors
sapply(resid_uals %*% de_sign, 
       all.equal, target=0)
# the residuals are orthogonal to the fitted values
all.equal(sum(resid_uals*fit_ted), target=0)
# calculate zero mean fitted values
fitted_zm <- drop(design_zm %*% beta_s)
all.equal(fitted_zm+mean(res_ponse), 
  mod_el$fitted.values, check.attributes=FALSE)
# calculate the residuals
resid_uals <- drop(response_zm - fitted_zm)
all.equal(resid_uals, mod_el$residuals, check.attributes=FALSE)
# add intercept column to design matrix
de_sign <- cbind(rep(1, NROW(de_sign)), de_sign)
# add column name
colnames(de_sign)[1] <- "intercept"
# calculate generalized inverse of the design matrix
design_inv <- MASS::ginv(de_sign)
# add define design weight for intercept
weight_s <- c(-1, weight_s)
# response equals linear form plus random noise
# noise <- rnorm(len_gth, sd=0.1)
res_ponse <- de_sign %*% weight_s + noise
# calculate the regression coefficients
beta_s <- design_inv %*% res_ponse
# perform multivariate regression without intercept term
mod_el <- lm(res_ponse ~ de_sign - 1)
all.equal(drop(beta_s), coef(mod_el), check.attributes=FALSE)
# calculate the influence matrix
influ_ence <- de_sign %*% design_inv
# calculate fitted values using influence matrix
fit_ted <- drop(influ_ence %*% res_ponse)
all.equal(fit_ted, mod_el$fitted.values, check.attributes=FALSE)
# calculate fitted values from regression coefficients
fit_ted <- drop(de_sign %*% beta_s)
all.equal(fit_ted, mod_el$fitted.values, check.attributes=FALSE)
# calculate the residuals
resid_uals <- drop(res_ponse - fit_ted)
all.equal(resid_uals, mod_el$residuals, check.attributes=FALSE)
# define transformation matrix
n_cols <- NCOL(de_sign)
trans_mat <- matrix(runif(n_cols^2, min=(-1), max=1), 
            ncol=n_cols)
# calculate linear combinations of design columns
design_trans <- de_sign %*% trans_mat
# calculate the influence matrix
influence_trans <- design_trans %*% MASS::ginv(design_trans)
# compare the influence matrices
all.equal(influ_ence, influence_trans)
# de-mean the design matrix columns
design_trans <- cbind(de_sign[, 1], t(t(de_sign[, -1])-colMeans(de_sign[, -1])))
round(apply(design_trans, 2, mean), 3)
# calculate the influence matrix
influence_trans <- design_trans %*% MASS::ginv(design_trans)
# compare the influence matrices
all.equal(influ_ence, influence_trans)
# regression model summary
model_sum <- summary(mod_el)
# degrees of freedom of residuals
deg_free <- (len_gth - NCOL(de_sign))
all.equal(deg_free, model_sum$df[2])
# variance of residuals
resid_var <- sum(resid_uals^2)/deg_free
# design matrix squared
design_2 <- crossprod(de_sign)
# design_2 <- t(de_sign) %*% de_sign
# calculate covariance matrix of betas
beta_covar <- resid_var*MASS::ginv(design_2)
# round(beta_covar, 3)
beta_sd <- sqrt(diag(beta_covar))
all.equal(beta_sd, model_sum$coeff[, 2], check.attributes=FALSE)
# calculate t-values of betas
beta_tvals <- drop(beta_s)/beta_sd
all.equal(beta_tvals, model_sum$coeff[, 3], check.attributes=FALSE)
# calculate two-sided p-values of betas
beta_pvals <- 2*pt(-abs(beta_tvals), df=deg_free)
all.equal(beta_pvals, model_sum$coeff[, 4], check.attributes=FALSE)
# calculate the influence matrix
influ_ence <- de_sign %*% design_inv
all.equal(influ_ence, influ_ence %*% influ_ence)
# calculate covariance matrix of fitted values
fit_covar <- resid_var*influ_ence
# calculate standard deviations of the fitted values
fit_sd <- sqrt(diag(fit_covar))
# plot the standard deviations
plot(fit_sd, type="l", lwd=3, col="blue", ylab="",
     main="Standard Deviations of Fitted Values")
x11(width=5, height=4)  # open x11 for plotting
# set plot parameters to reduce whitespace around plot
par(mar=c(5, 5, 2, 1), oma=c(0, 0, 0, 0))
# univariate regression with linear predictor
de_sign <- cbind(rep(1, len_gth), 1:len_gth/len_gth)
# calculate generalized inverse of the design matrix
design_inv <- MASS::ginv(de_sign)
# calculate the influence matrix
influ_ence <- de_sign %*% design_inv
# plot the leverage vector
plot(x=de_sign[,2], y=diag(influ_ence),
     type="l", lwd=3, col="blue",
     xlab="predictor", ylab="leverage",
     main="Leverage as Function of Predictor")
# define the design weights
weight_s <- c(-1, 1)
# response without random noise equals weighted sum over columns of de_sign
res_ponse <- de_sign %*% weight_s
# perform loop over different realizations of random noise
fit_ted <- lapply(1:50, function(it) {
  # add random noise to response
  res_ponse <- res_ponse + rnorm(len_gth, sd=1.0)
  # calculate fitted values using influence matrix
  influ_ence %*% res_ponse
})  # end lapply
fit_ted <- rutils::do_call(cbind, fit_ted)
x11(width=5, height=4)  # open x11 for plotting
# set plot parameters to reduce whitespace around plot
par(mar=c(5, 5, 2, 1), oma=c(0, 0, 0, 0))
# plot fitted values
matplot(x=de_sign[,2], y=fit_ted,
type="l", lty="solid", lwd=1, col="blue",
xlab="predictor", ylab="fitted",
main="Fitted Values for Different Realizations
of Random Noise")
lines(x=de_sign[,2], y=res_ponse, col="red", lwd=4)
legend(x="topleft", # add legend
       legend=c("response without noise", "fitted values"),
       title=NULL, inset=0.05, cex=0.8, lwd=6,
       lty=1, col=c("red", "blue"))
# univariate regression with linear predictor
de_sign <- cbind(rep(1, len_gth), 1:len_gth/len_gth)
res_ponse <- de_sign %*% weight_s + rnorm(len_gth, sd=0.3)
design_inv <- MASS::ginv(de_sign)
influ_ence <- de_sign %*% design_inv
beta_s <- design_inv %*% res_ponse
fit_ted <- drop(de_sign %*% beta_s)
resid_uals <- drop(res_ponse - fit_ted)
deg_free <- (len_gth - NCOL(de_sign))
resid_sd <- sqrt(sum(resid_uals^2)/deg_free)
# inverse of design matrix squared
design_2 <- MASS::ginv(crossprod(de_sign))
# perform loop over out-of-sample predictors
new_predictors <- (max(de_sign[, 2]) + 10*(1:5)/len_gth)
predic_tions <- sapply(new_predictors, function(predic_tor) {
  predic_tor <- cbind(1, predic_tor)
  # calculate predicted values
  predic_ted <- predic_tor %*% beta_s
  # calculate standard deviation
  predict_sd <- resid_sd*sqrt(predic_tor %*% design_2 %*% t(predic_tor))
  c(predicted=predic_ted, stddev=predict_sd)
})  # end sapply
predic_tions <- t(predic_tions)
# prepare plot data
x_data <- c(de_sign[,2], new_predictors)
x_lim <- range(x_data)
y_data <- c(fit_ted, predic_tions[, 1])
# calculate t-quantile
t_quant <- qt(pnorm(2), df=deg_free)
predict_low <- predic_tions[, 1]-t_quant*predic_tions[, 2]
predict_high <- predic_tions[, 1]+t_quant*predic_tions[, 2]
y_lim <- range(c(res_ponse, y_data, predict_low, predict_high))
# plot the regression predictions
plot(x=x_data, y=y_data,
     xlim=x_lim, ylim=y_lim,
     type="l", lwd=3, col="blue",
     xlab="predictor", ylab="fitted or predicted",
     main="Predictions from Linear Regression")
points(x=de_sign[,2], y=res_ponse, col="blue")
points(x=new_predictors, y=predic_tions[, 1], pch=16, col="blue")
lines(x=new_predictors, y=predict_high, lwd=3, col="red")
lines(x=new_predictors, y=predict_low, lwd=3, col="green")
legend(x="topleft", # add legend
       legend=c("predictions", "+2SD", "-2SD"),
       title=NULL, inset=0.05, cex=0.8, lwd=6,
       lty=1, col=c("blue", "red", "green"))
# perform regression
predic_tor <- de_sign[, 2]
mod_el <- lm(res_ponse ~ predic_tor)
# perform prediction from regression
new_data <- data.frame(predic_tor=new_predictors)
predict_lm <- predict(object=mod_el,
  newdata=new_data, level=1-2*(1-pnorm(2)),
  interval="confidence")
predict_lm <- as.data.frame(predict_lm)
all.equal(predict_lm$fit, predic_tions[, 1])
all.equal(predict_lm$lwr, predict_low)
all.equal(predict_lm$upr, predict_high)
plot(res_ponse ~ predic_tor,
     xlim=range(predic_tor, new_data),
     ylim=range(res_ponse, predict_lm),
     xlab="predictor", ylab="fitted or predicted",
     main="Predictions from lm() Regression")
abline(mod_el, col="blue", lwd=3)
with(predict_lm, {
  points(x=new_data$predic_tor, y=fit, pch=16, col="blue")
  lines(x=new_data$predic_tor, y=lwr, lwd=3, col="green")
  lines(x=new_data$predic_tor, y=upr, lwd=3, col="red")
})  # end with
legend(x="topleft", # add legend
       legend=c("predictions", "+2SD", "-2SD"),
       title=NULL, inset=0.05, cex=0.8, lwd=6,
       lty=1, col=c("blue", "red", "green"))
# perform PCA
pc_a <- prcomp(design_zm, 
       center=TRUE, scale=TRUE)
design_pca <- pc_a$x
round(cov(design_pca), 2)
round(apply(design_pca, 2, mean), 3)
round(apply(design_pca, 2, sd), 2)
# calculate the influence matrix
influ_ence <- design_zm %*% MASS::ginv(design_zm)
influence_pca <- design_pca %*% MASS::ginv(design_pca)
all.equal(influ_ence, influence_pca)
# calculate the fitted values
fit_ted <- drop(de_sign %*% beta_s)
all.equal(fit_ted, mod_el$fitted.values, check.attributes=FALSE)
# calculate the residuals
resid_uals <- drop(res_ponse - fit_ted)
all.equal(resid_uals, mod_el$residuals, check.attributes=FALSE)
# residuals are orthogonal to fitted values
all.equal(sum(resid_uals*fit_ted), target=0)
# TSS = ESS + RSS
t_ss <- (len_gth-1)*var(drop(res_ponse))
e_ss <- (len_gth-1)*var(fit_ted)
r_ss <- (len_gth-1)*var(resid_uals)
all.equal(t_ss, e_ss + r_ss)
# regression summary
model_sum <- summary(mod_el)
# regression R-squared
r_squared <- e_ss/t_ss
all.equal(r_squared, model_sum$r.squared)
# correlation between response and fitted values
cor_fitted <- drop(cor(res_ponse, fit_ted))
# squared correlation between response and fitted values
all.equal(cor_fitted^2, r_squared)
x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
deg_free <- c(3, 5, 9)  # df values
col_ors <- c("black", "red", "blue", "green")
lab_els <- paste0("df1=", deg_free, ", df2=3")
for (in_dex in 1:NROW(deg_free)) {  # plot four curves
curve(expr=df(x, df1=deg_free[in_dex], df2=3),
      type="l", xlim=c(0, 4),
      xlab="", ylab="", lwd=2,
      col=col_ors[in_dex],
      add=as.logical(in_dex-1))
}  # end for
# add title
title(main="F-Distributions", line=0.5)
# add legend
legend("topright", inset=0.05, title="degrees of freedom",
       lab_els, cex=0.8, lwd=2, lty=1,
       col=col_ors)
# F-statistic from lm()
model_sum$fstatistic
# degrees of freedom of residuals
deg_free <- len_gth-n_var-1
# F-statistic from RSS
f_stat <- e_ss*deg_free/r_ss/n_var
all.equal(f_stat, model_sum$fstatistic[1], check.attributes=FALSE)
# p-value of F-statistic
1-pf(q=f_stat, df1=len_gth-n_var-1, df2=n_var)
library(lmtest)  # load lmtest
de_sign <- data.frame(  # design matrix
  de_sign=1:30, omit_var=sin(0.2*1:30))
# response depends on both predictors
res_ponse <- with(de_sign,
  0.2*de_sign + omit_var + 0.2*rnorm(30))
# mis-specified regression only one predictor
mod_el <- lm(res_ponse ~ de_sign,
        data=de_sign)
model_sum <- summary(mod_el)
model_sum$coeff
model_sum$r.squared
# Durbin-Watson test shows residuals are autocorrelated
dwtest(mod_el)$p.value
par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # set plot panels
plot(for_mula, data=de_sign)
abline(mod_el, lwd=2, col="red")
title(main="OVB Regression", line=-1)
plot(mod_el, which=2, ask=FALSE)  # plot just Q-Q
set.seed(1121)
library(lmtest)
# spurious regression in unit root time series
de_sign <- cumsum(rnorm(100))  # unit root time series
res_ponse <- cumsum(rnorm(100))
for_mula <- res_ponse ~ de_sign
mod_el <- lm(for_mula)  # perform regression
# summary indicates statistically significant regression
model_sum <- summary(mod_el)
model_sum$coeff
model_sum$r.squared
# Durbin-Watson test shows residuals are autocorrelated
dw_test <- dwtest(mod_el)
c(dw_test$statistic[[1]], dw_test$p.value)
par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # set plot panels
plot(for_mula, xlab="", ylab="")  # plot scatterplot using formula
title(main="Spurious Regression", line=-1)
# add regression line
abline(mod_el, lwd=2, col="red")
plot(mod_el, which=2, ask=FALSE)  # plot just Q-Q
