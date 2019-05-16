library(HighFreq)  # load HighFreq
# Daily OHLC SPY prices
SPY_daily <- 
  rutils::to_period(oh_lc=SPY, period="days")
# Daily SPY returns and volatility
sd(rutils::diff_it(log(SPY_daily[, 4])))
# Minutely SPY returns (unit per minute)
re_turns <- rutils::diff_it(log(SPY[, 4]))
# Minutely SPY volatility scaled to daily interval
sqrt(6.5*60)*sd(re_turns)
# Minutely SPY returns (unit per second)
re_turns <- rutils::diff_it(log(SPY[, 4])) / 
  rutils::diff_it(.index(SPY))
# Minutely SPY volatility scaled to daily aggregation interval
60*sqrt(6.5*60)*sd(re_turns)
# Daily SPY volatility
# including extra time over weekends and holidays
24*60*60*sd(rutils::diff_it(log(SPY_daily[, 4])) / 
    rutils::diff_it(.index(SPY_daily)))
table(rutils::diff_it(.index(SPY_daily)))
# Scaled minutely SPY returns
re_turns <- rutils::diff_it(as.numeric(Cl(SPY)))[-1] /
  rutils::diff_it(.index(SPY))[-1]
range(re_turns)
hist(re_turns, breaks=100,
     xlim=c(-0.005, 0.005))
# SPY prices
price_s <- cumsum(re_turns)
plot(price_s, t="l")
# Perform rescaled range analysis
look_back <- 100
end_points <- rutils::calc_endpoints(re_turns,
  inter_val=look_back)
r_s <- sapply(seq_along(end_points)[-1],
      function(it) {
  indeks <- end_points[it-1]:end_points[it]
  price_s <- price_s[indeks]
  (max(price_s) - min(price_s)) /
    sd(re_turns[indeks])
})  # end sapply
mean(r_s)
# Perform rescaled range analysis over many look-backs
look_backs <- seq.int(1e2, 1e3, 1e2)
r_s <- sapply(look_backs, function(look_back) {
  end_points <- rutils::calc_endpoints(re_turns, inter_val=look_back)
  r_s <- sapply(seq_along(end_points)[-1], function(it) {
    indeks <- end_points[it-1]:end_points[it]
    (max(price_s[indeks]) - min(price_s[indeks]))/sd(re_turns[indeks])
  })  # end sapply
  mean(r_s)
})  # end sapply
names(r_s) <- paste0("agg_", look_backs)
rs_log <- log(r_s)
look_log <- log(look_backs)
mod_el <- lm(rs_log ~ look_log)
hurs_t <- summary(mod_el)$coeff[2, 1]
look_log <- look_log - mean(look_log)
rs_log <- rs_log - mean(rs_log)
hurs_t <- sum(rs_log*look_log)/sum(look_log^2)
x11(width=6, height=5)
par(mar=c(4, 4, 2, 1), oma=c(1, 1, 1, 1))
plot(rs_log ~ look_log, lwd=6, col="red",
     xlab="aggregation intervals (log)",
     ylab="rescaled range (log)",
     main="Rescaled Range Analysis for SPY")
abline(mod_el, lwd=3, col="blue")
text(-1.2, 0.2, paste0("Hurst = ",
  round(hurs_t, 4)))
# Create random real symmetric matrix
mat_rix <- matrix(runif(25), nc=5)
mat_rix <- mat_rix + t(mat_rix)
# Calculate eigenvectors and eigenvalues
ei_gen <- eigen(mat_rix)
eigen_vec <- ei_gen$vectors
dim(eigen_vec)
# Plot eigenvalues
barplot(ei_gen$values,
  xlab="", ylab="", las=3,
  names.arg=paste0("ev", 1:NROW(ei_gen$values)),
  main="Eigenvalues of a real symmetric matrix")
# eigenvectors form an orthonormal basis
round(t(eigen_vec) %*% eigen_vec,
  digits=4)
# Diagonalize matrix using eigenvector matrix
round(t(eigen_vec) %*% (mat_rix %*% eigen_vec),
  digits=4)
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
barplot(ei_gen$values, las=3,
  xlab="", ylab="",
  names.arg=paste0("ev", 1:NROW(ei_gen$values)),
  main="Eigenvalues of positive semi-definite matrix")
# Perform singular value decomposition
mat_rix <- matrix(rnorm(50), nc=5)
s_vd <- svd(mat_rix)
# Recompose mat_rix from SVD mat_rices
all.equal(mat_rix,
  s_vd$u %*% (s_vd$d*t(s_vd$v)))
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
eigen_inverse <-
  eigen_vec %*% (t(eigen_vec) / ei_gen$values)
all.equal(in_verse, eigen_inverse)
# Decompose diagonal matrix with inverse of eigenvalues
# diago_nal <- diag(1/ei_gen$values)
# eigen_inverse <-
#   eigen_vec %*% (diago_nal %*% t(eigen_vec))
# Random rectangular matrix: n_left > n_right
n_left <- 6 ; n_right <- 4
mat_rix <- matrix(runif(n_left*n_right),
  nc=n_right)
# Calculate generalized inverse of mat_rix
in_verse <- MASS::ginv(mat_rix)
round(in_verse %*% mat_rix, 4)
all.equal(mat_rix,
  mat_rix %*% in_verse %*% mat_rix)
# Random rectangular matrix: n_left < n_right
n_left <- 4 ; n_right <- 6
mat_rix <- matrix(runif(n_left*n_right),
  nc=n_right)
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
mp_inverse <-
  MASS::ginv(t(mat_rix) %*% mat_rix) %*% t(mat_rix)
all.equal(mp_inverse, in_verse)
# Create random singular matrix
n_left <- 4 ; n_right <- 6
mat_rix <- matrix(runif(n_left*n_right), nc=n_right)
mat_rix <- t(mat_rix) %*% mat_rix
# Calculate generalized inverse of mat_rix
in_verse <- MASS::ginv(mat_rix)
# Verify inverse property of mat_rix
all.equal(mat_rix,
  mat_rix %*% in_verse %*% mat_rix)
# Perform singular value decomposition
s_vd <- svd(mat_rix)
# Set tolerance for determining zero singular values
to_l <- sqrt(.Machine$double.eps)
# Check for zero singular values
s_vd$d
not_zero <- (s_vd$d > (to_l * s_vd$d[1]))
# Calculate generalized inverse from SVD
svd_inverse <-
  s_vd$v[, not_zero] %*%
  (t(s_vd$u[, not_zero]) / s_vd$d[not_zero])
all.equal(svd_inverse, in_verse)
# Calculate Moore-Penrose pseudo-inverse
mp_inverse <-
  MASS::ginv(t(mat_rix) %*% mat_rix) %*% t(mat_rix)
all.equal(mp_inverse, in_verse)
# Diagonalize the "unit" matrix
uni_t <- mat_rix %*% in_verse
round(uni_t, 4)
round(mat_rix %*% in_verse, 4)
round(t(s_vd$u) %*% uni_t %*% s_vd$v, 4)
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
not_zero <- (eigen_val > (to_l * eigen_val[1]))
if (sum(!not_zero) > 0) {
  eigen_val[!not_zero] <- 2*to_l
  mat_rix <- eigen_vec %*%
    (eigen_val * t(eigen_vec))
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
  re_turns <- apply(re_turns, MARGIN=2,
    function(y) (y-mean(y)))
  cov_mat <- crossprod(re_turns) / (x-1)
  min(eigen(cov_mat)$values)
})  # end sapply
plot(y=e_values, x=n_data, t="l",
  xlab="", ylab="", lwd=3, col="blue",
  main="Smallest eigenvalue of covariance matrix\nas function of number of returns")
# Create rectangular matrix with collinear columns
se_ries <- matrix(rnorm(10*8), nc=10)
# Calculate covariance matrix
cov_mat <- cov(se_ries)
# Calculate inverse of cov_mat - error
in_verse <- solve(cov_mat)
# Calculate regularized inverse of cov_mat
in_verse <- MASS::ginv(cov_mat)
# Verify inverse property of mat_rix
all.equal(cov_mat,
  cov_mat %*% in_verse %*% cov_mat)
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
# Create random covariance matrix
set.seed(1121)
mat_rix <- matrix(rnorm(5e2), nc=5)
cov_mat <- cov(mat_rix)
# Perform eigen decomposition
ei_gen <- eigen(cov_mat)
eigen_vec <- ei_gen$vectors
# Calculate regularized inverse matrix
max_eigen <- 2
in_verse <- eigen_vec[, 1:max_eigen] %*%
  (t(eigen_vec[, 1:max_eigen]) / ei_gen$values[1:max_eigen])
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
library(parallel)  # Load package parallel
# Get short description
packageDescription("parallel")
# Load help page
help(package="parallel")
# list all objects in "parallel"
ls("package:parallel")
library(parallel)  # Load package parallel
# Calculate number of available cores
n_cores <- detectCores() - 1
# Define function that pauses execution
paws <- function(x, sleep_time) {
  Sys.sleep(sleep_time)
  x
}  # end paws
# Perform parallel loop under Mac-OSX or Linux
paw_s <- mclapply(1:10, paws, mc.cores=n_cores,
          sleep_time=0.01)
# Initialize compute cluster under Windows
clus_ter <- makeCluster(n_cores)
# Perform parallel loop under Windows
paw_s <- parLapply(clus_ter, 1:10, paws,
           sleep_time=0.01)
library(microbenchmark)  # Load package microbenchmark
# Compare speed of lapply versus parallel computing
summary(microbenchmark(
  l_apply=lapply(1:10, paws, sleep_time=0.01),
  parl_apply=
    parLapply(clus_ter, 1:10, paws, sleep_time=0.01),
  times=10)
)[, c(1, 4, 5)]
# Stop R processes over cluster under Windows
stopCluster(clus_ter)
library(parallel)  # Load package parallel
# Calculate number of available cores
n_cores <- detectCores() - 1
# Initialize compute cluster under Windows
clus_ter <- makeCluster(n_cores)
# Define function that pauses execution
paws <- function(x, sleep_time) {
  Sys.sleep(sleep_time)
  x
}  # end paws
# Compare speed of lapply with parallel computing
iter_ations <- 3:10
compute_times <- sapply(iter_ations,
  function(max_iterations, sleep_time) {
    out_put <- summary(microbenchmark(
lapply=lapply(1:max_iterations, paws,
              sleep_time=sleep_time),
parallel=parLapply(clus_ter, 1:max_iterations,
        paws, sleep_time=sleep_time),
times=10))[, c(1, 4)]
    structure(out_put[, 2],
        names=as.vector(out_put[, 1]))
    }, sleep_time=0.01)
compute_times <- t(compute_times)
rownames(compute_times) <- iter_ations
library(parallel)  # Load package parallel
plot(x=rownames(compute_times),
     y=compute_times[, "lapply"],
     type="l", lwd=2, col="blue",
     main="Compute times",
     xlab="number of iterations in loop", ylab="",
     ylim=c(0, max(compute_times[, "lapply"])))
lines(x=rownames(compute_times),
y=compute_times[, "parallel"], lwd=2, col="green")
legend(x="topleft", legend=colnames(compute_times),
 inset=0.1, cex=1.0, bg="white",
 lwd=2, lty=1, col=c("blue", "green"))
library(parallel)  # Load package parallel
# Calculate number of available cores
n_cores <- detectCores() - 1
# Initialize compute cluster under Windows
clus_ter <- makeCluster(n_cores)
# Define large matrix
mat_rix <- matrix(rnorm(7*10^5), ncol=7)
# Define aggregation function over column of matrix
agg_regate <- function(col_umn) {
  out_put <- 0
  for (in_dex in 1:NROW(col_umn))
    out_put <- out_put + col_umn[in_dex]
  out_put
}  # end agg_regate
# Perform parallel aggregations over columns of matrix
agg_regations <-
  parCapply(clus_ter, mat_rix, agg_regate)
# Compare speed of apply with parallel computing
summary(microbenchmark(
  ap_ply=apply(mat_rix, MARGIN=2, agg_regate),
  parl_apply=
    parCapply(clus_ter, mat_rix, agg_regate),
  times=10)
)[, c(1, 4, 5)]
# Stop R processes over cluster under Windows
stopCluster(clus_ter)
library(parallel)  # Load package parallel
# Calculate number of available cores
n_cores <- detectCores() - 1
# Initialize compute cluster under Windows
clus_ter <- makeCluster(n_cores)
ba_se <- 2
# Fails because child processes don't know ba_se:
parLapply(clus_ter, 2:4,
    function(exponent) ba_se^exponent)
# ba_se passed to child via dots ... argument:
parLapply(clus_ter, 2:4,
    function(exponent, ba_se) ba_se^exponent,
    ba_se=ba_se)
# ba_se passed to child via clusterExport:
clusterExport(clus_ter, "ba_se")
parLapply(clus_ter, 2:4,
    function(exponent) ba_se^exponent)
# Fails because child processes don't know zoo::index():
parSapply(clus_ter, c("VTI", "IEF", "DBC"),
    function(sym_bol)
      NROW(index(get(sym_bol, envir=rutils::etf_env))))
# zoo function referenced using "::" in child process:
parSapply(clus_ter, c("VTI", "IEF", "DBC"),
    function(sym_bol)
      NROW(zoo::index(get(sym_bol, envir=rutils::etf_env))))
# Package zoo loaded in child process:
parSapply(clus_ter, c("VTI", "IEF", "DBC"),
    function(sym_bol) {
      stopifnot("package:zoo" %in% search() || require("zoo", quietly=TRUE))
      NROW(index(get(sym_bol, envir=rutils::etf_env)))
    })  # end parSapply
# Stop R processes over cluster under Windows
stopCluster(clus_ter)
library(parallel)  # Load package parallel
# Calculate number of available cores
n_cores <- detectCores() - 1
# Initialize compute cluster under Windows
clus_ter <- makeCluster(n_cores)
# Set seed for cluster under Windows
# Doesn't work: set.seed(1121)
clusterSetRNGStream(clus_ter, 1121)
# Perform parallel loop under Windows
out_put <- parLapply(clus_ter, 1:70, rnorm, n=100)
sum(unlist(out_put))
# Stop R processes over cluster under Windows
stopCluster(clus_ter)
# Perform parallel loop under Mac-OSX or Linux
out_put <- mclapply(1:10, rnorm, mc.cores=n_cores, n=100)
set.seed(1121)  # Reset random number generator
# Sample from Standard Normal Distribution
n_rows <- 1000
sam_ple <- rnorm(n_rows)
# Sample mean - MC estimate
mean(sam_ple)
# Sample standard deviation - MC estimate
sd(sam_ple)
# Monte Carlo estimate of cumulative probability
sam_ple <- sort(sam_ple)
pnorm(1)
sum(sam_ple<1)/n_rows
# Monte Carlo estimate of quantile
conf_level <- 0.99
qnorm(conf_level)
cut_off <- conf_level*n_rows
sam_ple[cut_off]
quantile(sam_ple, probs=conf_level)
# Analyze the source code of quantile()
stats:::quantile.default
# Microbenchmark quantile
library(microbenchmark)
summary(microbenchmark(
  sam_ple=sam_ple[cut_off],
  quan_tile=quantile(sam_ple, probs=conf_level),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
x11(width=6, height=5)
par(oma=c(1, 1, 1, 1), mar=c(2, 2, 2, 1), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
set.seed(1121)  # Reset random number generator
bar_rier <- 20  # Barrier level
n_rows <- 1000  # Number of simulation steps
pa_th <- numeric(n_rows)  # Allocate path vector
pa_th[1] <- 0  # Initialize path
in_dex <- 2  # Initialize simulation index
while ((in_dex <= n_rows) &&
 (pa_th[in_dex - 1] < bar_rier)) {
# Simulate next step
  pa_th[in_dex] <-
    pa_th[in_dex - 1] + rnorm(1)
  in_dex <- in_dex + 1  # Advance in_dex
}  # end while
# Fill remaining pa_th after it crosses bar_rier
if (in_dex <= n_rows)
  pa_th[in_dex:n_rows] <- pa_th[in_dex - 1]
# Create daily time series starting 2011
ts_path <- ts(data=pa_th, frequency=365, start=c(2011, 1))
plot(ts_path, type="l", col="black",
     lty="solid", lwd=2, xlab="", ylab="")
abline(h=bar_rier, lwd=2, col="red")
title(main="Brownian motion crossing a barrier level",
      line=0.5)
x11(width=6, height=5)
par(oma=c(1, 1, 1, 1), mar=c(2, 2, 2, 1), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
set.seed(1121)  # Reset random number generator
bar_rier <- 20  # Barrier level
n_rows <- 1000  # Number of simulation steps
# Simulate path of Brownian motion
pa_th <- cumsum(rnorm(n_rows))
# Find index when pa_th crosses bar_rier
cro_ss <- which(pa_th > bar_rier)
# Fill remaining pa_th after it crosses bar_rier
if (NROW(cro_ss)>0) {
  pa_th[(cro_ss[1]+1):n_rows] <-
    pa_th[cro_ss[1]]
}  # end if
# Create daily time series starting 2011
ts_path <- ts(data=pa_th, frequency=365,
     start=c(2011, 1))
# Create plot with horizontal line
plot(ts_path, type="l", col="black",
     lty="solid", lwd=2, xlab="", ylab="")
abline(h=bar_rier, lwd=2, col="red")
title(main="Brownian motion crossing a barrier level",
      line=0.5)
set.seed(1121)  # Reset random number generator
# Sample from Standard Normal Distribution
n_rows <- 1000; sam_ple <- rnorm(n_rows)
# Sample mean and standard deviation
mean(sam_ple); sd(sam_ple)
# Bootstrap of sample mean and median
boot_strap <- sapply(1:10000, function(x) {
  boot_sample <- sam_ple[sample.int(n_rows,
                              replace=TRUE)]
  c(mean=mean(boot_sample), median=median(boot_sample))
})  # end sapply
boot_strap <- t(boot_strap)
boot_strap[1:3, ]
# Standard error from formula
sd(sam_ple)/sqrt(n_rows)
# Standard error of mean from bootstrap
sd(boot_strap[, "mean"])
# Standard error of median from bootstrap
sd(boot_strap[, "median"])
plot(density(boot_strap[, "median"]),
     lwd=2, xlab="regression slopes",
     main="Distribution of Bootstrapped Median")
abline(v=mean(boot_strap[, "median"]),
 lwd=2, col="red")
set.seed(1121)  # Reset random number generator
n_rows <- 1000
sam_ple <- rnorm(n_rows)
# Bootstrap of sample mean and median
boot_strap <- sapply(1:10000, function(x) {
  # Boot_sample from Standard Normal Distribution
  boot_sample <- rnorm(n_rows)
  c(mean=mean(boot_sample),
    median=median(boot_sample))
})  # end sapply
boot_strap[, 1:3]
# Standard error from formula
sd(sam_ple)/sqrt(n_rows)
# Standard error of mean from bootstrap
sd(boot_strap["mean", ])
# Standard error of median from bootstrap
sd(boot_strap["median", ])
library(parallel)  # Load package parallel
n_cores <- detectCores() - 1  # Number of cores
clus_ter <- makeCluster(n_cores)  # Initialize compute cluster under Windows
set.seed(1121)  # Reset random number generator
# Sample from Standard Normal Distribution
n_rows <- 1000
sam_ple <- rnorm(n_rows)
# Bootstrap mean and median under Windows
boot_strap <- parLapply(clus_ter, 1:10000,
  function(x, sam_ple, n_rows) {
  boot_sample <- sam_ple[sample.int(n_rows, replace=TRUE)]
  c(mean=mean(boot_sample), median=median(boot_sample))
  }, sam_ple=sam_ple, n_rows=n_rows)  # end parLapply
# Bootstrap mean and median under Mac-OSX or Linux
boot_strap <- mclapply(1:10000,
  function(x) {
  boot_sample <- sam_ple[sample.int(n_rows, replace=TRUE)]
  c(mean=mean(boot_sample), median=median(boot_sample))
  }, mc.cores=n_cores)  # end mclapply
boot_strap <- rutils::do_call(rbind, boot_strap)
# Means and standard errors from bootstrap
apply(boot_strap, MARGIN=2,
function(x) c(mean=mean(x), std_error=sd(x)))
# Standard error from formula
sd(sam_ple)/sqrt(n_rows)
stopCluster(clus_ter)  # Stop R processes over cluster under Windows
n_rows <- 1000
sam_ple <- rnorm(n_rows)
sd(sam_ple)
mad(sam_ple)
median(abs(sam_ple - median(sam_ple)))
median(abs(sam_ple - median(sam_ple)))/qnorm(0.75)
# Bootstrap of sd and mad estimators
boot_strap <- sapply(1:10000, function(x) {
  boot_sample <-
    sam_ple[sample.int(n_rows, replace=TRUE)]
  c(sd=sd(boot_sample), mad=mad(boot_sample))
})  # end sapply
boot_strap <- t(boot_strap)
# Analyze bootstrapped variance
head(boot_strap)
sum(is.na(boot_strap))
# Means and standard errors from bootstrap
apply(boot_strap, MARGIN=2,
function(x) c(mean=mean(x), std_error=sd(x)))
# Parallel bootstrap under Windows
library(parallel)  # Load package parallel
n_cores <- detectCores() - 1  # Number of cores
clus_ter <- makeCluster(n_cores)  # Initialize compute cluster
boot_strap <- parLapply(clus_ter, 1:10000,
  function(x, sam_ple) {
    boot_sample <- sam_ple[sample.int(n_rows, replace=TRUE)]
    c(sd=sd(boot_sample), mad=mad(boot_sample))
  }, sam_ple=sam_ple)  # end parLapply
# Parallel bootstrap under Mac-OSX or Linux
boot_strap <- mclapply(1:10000,
  function(x) {
    boot_sample <- sam_ple[sample.int(n_rows, replace=TRUE)]
    c(sd=sd(boot_sample), mad=mad(boot_sample))
  }, mc.cores=n_cores)  # end mclapply
stopCluster(clus_ter)  # Stop R processes over cluster
boot_strap <- rutils::do_call(rbind, boot_strap)
# Means and standard errors from bootstrap
apply(boot_strap, MARGIN=2,
function(x) c(mean=mean(x), std_error=sd(x)))
# Sample from time series of ETF returns
re_turns <- rutils::etf_env$re_turns[, "VTI"]
re_turns <- na.omit(re_turns)
n_rows <- NROW(re_turns)
# Bootstrap sd and MAD under Windows
library(parallel)  # Load package parallel
n_cores <- detectCores() - 1  # Number of cores
clus_ter <- makeCluster(n_cores)  # Initialize compute cluster under Windows
clusterSetRNGStream(clus_ter, 1121)  # Reset random number generator in all cores
n_boot <- 1e4
boot_strap <- parLapply(clus_ter, 1:n_boot,
  function(x, re_turns, n_rows) {
    boot_sample <- re_turns[sample.int(n_rows, replace=TRUE)]
    c(sd=sd(boot_sample), mad=mad(boot_sample))
  }, re_turns=re_turns, n_rows=n_rows)  # end parLapply
# Bootstrap sd and MAD under Mac-OSX or Linux
boot_strap <- mclapply(1:n_boot,
  function(x) {
    boot_sample <- re_turns[sample.int(n_rows, replace=TRUE)]
    c(sd=sd(boot_sample), mad=mad(boot_sample))
  }, mc.cores=n_cores)  # end mclapply
stopCluster(clus_ter)  # Stop R processes over cluster under Windows
boot_strap <- rutils::do_call(rbind, boot_strap)
# Standard error assuming normal distribution of returns
sd(re_turns)/sqrt(n_boot)
# Means and standard errors from bootstrap
std_errors <- apply(boot_strap, MARGIN=2,
function(x) c(mean=mean(x), std_error=sd(x)))
std_errors
# Relative standard errors
std_errors[2, ]/std_errors[1, ]
# Calculate percentage returns from VTI prices
price_s <- quantmod::Ad(rutils::etf_env$VTI)
star_t <- as.numeric(price_s[1, ])
re_turns <- rutils::diff_it(log(price_s))
class(re_turns); head(re_turns)
sum(is.na(re_turns))
n_rows <- NROW(re_turns)
# Define barrier level with respect to price_s
bar_rier <- 1.5*max(price_s)
# Calculate single bootstrap sample
boot_sample <- re_turns[sample.int(n_rows, replace=TRUE)]
# Calculate prices from percentage returns
boot_sample <- star_t*exp(cumsum(boot_sample))
# Calculate statistic
sum(boot_sample > bar_rier) > 0
library(parallel)  # Load package parallel
n_cores <- detectCores() - 1  # Number of cores
clus_ter <- makeCluster(n_cores)  # Initialize compute cluster under Windows
# Perform parallel bootstrap under Windows
clusterSetRNGStream(clus_ter, 1121)  # Reset random number generator in all cores
clusterExport(clus_ter, c("star_t", "bar_rier"))
n_boot <- 1e4
boot_strap <- parLapply(clus_ter, 1:n_boot,
  function(x, re_turns, n_rows) {
    boot_sample <- re_turns[sample.int(n_rows, replace=TRUE)]
    # Calculate prices from percentage returns
    boot_sample <- star_t*exp(cumsum(boot_sample))
    # Calculate statistic
    sum(boot_sample > bar_rier) > 0
  }, re_turns=re_turns, n_rows=n_rows)  # end parLapply
# Perform parallel bootstrap under Mac-OSX or Linux
boot_strap <- mclapply(1:n_boot,
  function(x) {
    boot_sample <- re_turns[sample.int(n_rows, replace=TRUE)]
    # Calculate prices from percentage returns
    boot_sample <- star_t*exp(cumsum(boot_sample))
    # Calculate statistic
    sum(boot_sample > bar_rier) > 0
  }, mc.cores=n_cores)  # end mclapply
stopCluster(clus_ter)  # Stop R processes over cluster under Windows
boot_strap <- rutils::do_call(rbind, boot_strap)
# Calculate frequency of crossing barrier
sum(boot_strap)/n_boot
# Calculate percentage returns from VTI prices
oh_lc <- rutils::etf_env$VTI
price_s <- as.numeric(oh_lc[, 4])
star_t <- price_s[1]
re_turns <- rutils::diff_it(log(price_s))
n_rows <- NROW(re_turns)
# Calculate difference of OHLC price columns
ohlc_diff <- oh_lc[, 1:3] - price_s
class(re_turns); head(re_turns)
# Calculate bootstrap prices from percentage returns
sam_ple <- sample.int(n_rows, replace=TRUE)
boot_prices <- star_t*exp(cumsum(re_turns[sam_ple]))
boot_ohlc <- ohlc_diff + boot_prices
boot_ohlc <- cbind(boot_ohlc, boot_prices)
# Define barrier level with respect to price_s
bar_rier <- 1.5*max(price_s)
# Calculate if High bootstrapped prices crossed barrier level
sum(boot_ohlc[, 2] > bar_rier) > 0
library(parallel)  # Load package parallel
n_cores <- detectCores() - 1  # Number of cores
clus_ter <- makeCluster(n_cores)  # Initialize compute cluster under Windows
# Perform parallel bootstrap under Windows
clusterSetRNGStream(clus_ter, 1121)  # Reset random number generator in all cores
clusterExport(clus_ter, c("star_t", "bar_rier", "ohlc_diff"))
n_boot <- 1e4
boot_strap <- parLapply(clus_ter, 1:n_boot,
  function(x, re_turns, n_rows) {
    # Calculate OHLC prices from percentage returns
    sam_ple <- sample.int(n_rows, replace=TRUE)
    boot_prices <- star_t*exp(cumsum(re_turns[sam_ple]))
    boot_ohlc <- ohlc_diff + boot_prices
    boot_ohlc <- cbind(boot_ohlc, boot_prices)
    # Calculate statistic
    sum(boot_ohlc[, 2] > bar_rier) > 0
  }, re_turns=re_turns, n_rows=n_rows)  # end parLapply
# Perform parallel bootstrap under Mac-OSX or Linux
boot_strap <- mclapply(1:n_boot,
  function(x) {
    # Calculate OHLC prices from percentage returns
    sam_ple <- sample.int(n_rows, replace=TRUE)
    boot_prices <- star_t*exp(cumsum(re_turns[sam_ple]))
    boot_ohlc <- ohlc_diff + boot_prices
    boot_ohlc <- cbind(boot_ohlc, boot_prices)
    # Calculate statistic
    sum(boot_ohlc[, 2] > bar_rier) > 0
  }, mc.cores=n_cores)  # end mclapply
stopCluster(clus_ter)  # Stop R processes over cluster under Windows
boot_strap <- rutils::do_call(rbind, boot_strap)
# Calculate frequency of crossing barrier
sum(boot_strap)/n_boot
set.seed(1121)  # Reset random number generator
# Sample from Standard Normal Distribution
n_rows <- 1000
sam_ple <- rnorm(n_rows)
# Estimate the 95% quantile
boot_strap <- sapply(1:10000, function(x) {
  boot_sample <- sam_ple[sample.int(n_rows,
                              replace=TRUE)]
  quantile(boot_sample, 0.95)
})  # end sapply
sd(boot_strap)
# Estimate the 95% quantile using antithetic sampling
boot_strap <- sapply(1:10000, function(x) {
  boot_sample <- sam_ple[sample.int(n_rows,
                              replace=TRUE)]
  quantile(c(boot_sample, -boot_sample), 0.95)
})  # end sapply
# Standard error of quantile from bootstrap
sd(boot_strap)
sqrt(2)*sd(boot_strap)
x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
# Plot a Normal probability distribution
curve(expr=dnorm, xlim=c(-3, 4),
main="Shifted Normal distribution function",
xlab="", ylab="", lwd=3, col="blue")
# Add shifted Normal probability distribution
curve(expr=dnorm(x, mean=1), add=TRUE,
lwd=3, col="red")
# Add vertical dashed lines
abline(v=0, lwd=3, col="blue", lty="dashed")
abline(v=1, lwd=3, col="red", lty="dashed")
arrows(x0=0, y0=0.1, x1=1, y1=0.1, lwd=3,
 code=2, angle=20,
 length=grid::unit(0.2, "cm"))
text(x=0.3, 0.1, labels=bquote(lambda),
     pos=3, cex=2)
set.seed(1121) # reset random number generator
# Sample from Standard Normal Distribution
n_rows <- 1000
sam_ple <- rnorm(n_rows)
sam_ple <- sort(sam_ple)
# Monte Carlo cumulative probability
pnorm(-2)
integrate(dnorm, low=2, up=Inf)
sum(sam_ple > 2)/n_rows
# Generate importance sample
lamb_da <- 1.5
sample_is <- sam_ple + lamb_da
# Importance cumulative probability
sum(sample_is > 2)/n_rows
weight_s <- exp(-lamb_da*sample_is + lamb_da^2/2)
sum((sample_is > 2)*weight_s)/n_rows
# Bootstrap of standard errors of cumulative probability
boot_strap <- sapply(1:1000, function(x) {
  sam_ple <- rnorm(n_rows)
  m_c <- sum(sam_ple > 2)/n_rows
  sam_ple <- (sam_ple + lamb_da)
  weight_s <- exp(-lamb_da*sam_ple + lamb_da^2/2)
  i_s <- sum((sam_ple > 2)*weight_s)/n_rows
  c(MC=m_c,Importance=i_s)
}) # end sapply
apply(boot_strap, MARGIN=1,
function(x) c(mean=mean(x), sd=sd(x)))
# Monte Carlo expected value
integrate(function(x) x*dnorm(x), low=2, up=Inf)
sum((sam_ple > 2)*sam_ple)/n_rows
# Importance expected value
sum((sample_is > 2)*sample_is)/n_rows
sum((sample_is > 2)*sample_is*weight_s)/n_rows
# Bootstrap of standard errors of expected value
boot_strap <- sapply(1:1000, function(x) {
  sam_ple <- rnorm(n_rows)
  m_c <- sum((sam_ple > 2)*sam_ple)/n_rows
  sam_ple <- (sam_ple + lamb_da)
  weight_s <- exp(-lamb_da*sam_ple + lamb_da^2/2)
  i_s <- sum((sam_ple > 2)*sam_ple*weight_s)/n_rows
  c(MC=m_c,Importance=i_s)
}) # end sapply
apply(boot_strap, MARGIN=1,
function(x) c(mean=mean(x), sd=sd(x)))
# Binomial sample
n_rows <- 1000
pro_b <- 0.1
sam_ple <- rbinom(n=n_rows, size=1, pro_b)
head(sam_ple, 33)
fre_q <- sum(sam_ple)/n_rows
# Tilted binomial sample
lamb_da <- 5
p_tilted <- lamb_da*pro_b/(1 + pro_b*(lamb_da - 1))
weigh_t <- (1 + pro_b*(lamb_da - 1))/lamb_da
sam_ple <- rbinom(n=n_rows, size=1, p_tilted)
head(sam_ple, 33)
weigh_t*sum(sam_ple)/n_rows
# Bootstrap of standard errors
boot_strap <- sapply(1:1000, function(x) {
  c(MCarlo=sum(rbinom(n=n_rows, size=1, pro_b))/n_rows,
    Importance=weigh_t*sum(rbinom(n=n_rows, size=1, p_tilted))/n_rows)
}) # end sapply
apply(boot_strap, MARGIN=1,
function(x) c(mean=mean(x), sd=sd(x)))
set.seed(1121)  # Initialize random number generator
# Define explanatory and response variables
de_sign <- rnorm(100, mean=2)
noise <- rnorm(100)
res_ponse <- -3 + de_sign + noise
# Define design matrix and regression formula
de_sign <- data.frame(res_ponse, de_sign)
for_mula <- paste(colnames(de_sign)[1],
  paste(colnames(de_sign)[-1], collapse="+"),
  sep=" ~ ")
# Bootstrap of regression
boot_strap <- sapply(1:100, function(x) {
  boot_sample <- sample.int(NROW(de_sign),
                      replace=TRUE)
  mod_el <- lm(for_mula,
          data=de_sign[boot_sample, ])
  mod_el$coefficients
})  # end sapply
par(oma=c(1, 2, 1, 0), mgp=c(2, 1, 0), mar=c(5, 1, 1, 1), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
x11(width=6, height=6)
# Means and standard errors from bootstrap
apply(boot_strap, MARGIN=1,
      function(x) c(mean=mean(x), std_error=sd(x)))
# Means and standard errors from regression summary
mod_el <- lm(for_mula, data=de_sign)
mod_el$coefficients
summary(mod_el)$coefficients[, "Std. Error"]
plot(density(boot_strap["de_sign", ]),
     lwd=2, xlab="regression slopes",
     main="Bootstrapped regression slopes")
abline(v=mean(boot_strap["de_sign", ]),
       lwd=2, col="red")
library(parallel)  # Load package parallel
n_cores <- detectCores() - 1  # Number of cores
clus_ter <- makeCluster(n_cores)  # Initialize compute cluster under Windows
# Bootstrap of regression under Windows
boot_strap <- parLapply(clus_ter, 1:1000,
  function(x, for_mula, de_sign) {
    boot_sample <-
sample.int(NROW(de_sign), replace=TRUE)
    mod_el <- lm(for_mula,
data=de_sign[boot_sample, ])
    mod_el$coefficients
  },
  for_mula=for_mula,
  de_sign=de_sign)  # end parLapply
# Bootstrap of regression under Mac-OSX or Linux
boot_strap <- mclapply(1:1000,
  function(x) {
    boot_sample <-
sample.int(NROW(de_sign), replace=TRUE)
    lm(for_mula,
data=de_sign[boot_sample, ])$coefficients
  }, mc.cores=n_cores)  # end mclapply
stopCluster(clus_ter)  # Stop R processes over cluster under Windows
boot_strap <- rutils::do_call(rbind, boot_strap)
# Means and standard errors from bootstrap
apply(boot_strap, MARGIN=2,
function(x) c(mean=mean(x), std_error=sd(x)))
x11(width=6, height=6)
plot(density(boot_strap[, "de_sign"]),
     lwd=2, xlab="regression slopes",
     main="Bootstrapped regression slopes")
abline(v=mean(boot_strap[, "de_sign"]),
 lwd=2, col="red")
options(width=50, dev='pdf')
str(optimize)
# Objective function with multiple minima
object_ive <- function(in_put, param1=0.01) {
  sin(0.25*pi*in_put) + param1*(in_put-1)^2
}  # end object_ive
unlist(optimize(f=object_ive, interval=c(-4, 2)))
unlist(optimize(f=object_ive, interval=c(0, 8)))
options(width=60, dev='pdf')
par(oma=c(1, 1, 1, 1), mgp=c(2, 1, 0), mar=c(5, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# Plot the objective function
curve(expr=object_ive, type="l", xlim=c(-8, 9),
xlab="", ylab="", lwd=2)
# Add title
title(main="Objective Function", line=-1)
library(rgl)  # Load rgl
# Define function of two variables
sur_face <- function(x, y) y*sin(x)
# Draw 3d surface plot of function
persp3d(x=sur_face, xlim=c(-5, 5), ylim=c(-5, 5),
  col="green", axes=FALSE)
# Draw 3d surface plot of matrix
x_lim <- seq(from=-5, to=5, by=0.1)
y_lim <- seq(from=-5, to=5, by=0.1)
persp3d(z=outer(x_lim, y_lim, FUN=sur_face),
  xlab="x", ylab="y", zlab="sur_face",
  col="green")
# Save current view to png file
rgl.snapshot("surface_plot.png")
# Define function of two variables and two parameters
sur_face <- function(x, y, par_1=1, par_2=1)
  sin(par_1*x)*sin(par_2*y)
# Draw 3d surface plot of function
persp3d(x=sur_face, xlim=c(-5, 5), ylim=c(-5, 5),
  col="green", axes=FALSE,
  par_1=1, par_2=2)
# Rastrigin function with vector argument for optimization
rastri_gin <- function(vec_tor, pa_ram=25){
  sum(vec_tor^2 - pa_ram*cos(vec_tor))
}  # end rastri_gin
vec_tor <- c(pi/6, pi/6)
rastri_gin(vec_tor=vec_tor)
# Draw 3d surface plot of Rastrigin function
rgl::persp3d(
  x=Vectorize(function(x, y) rastri_gin(vec_tor=c(x, y))),
  xlim=c(-10, 10), ylim=c(-10, 10),
  col="green", axes=FALSE, zlab="", main="rastri_gin")
# Optimize with respect to vector argument
op_tim <- optim(par=vec_tor, fn=rastri_gin,
        method="L-BFGS-B",
        upper=c(4*pi, 4*pi),
        lower=c(pi/2, pi/2),
        pa_ram=1)
# Optimal parameters and value
op_tim$par
op_tim$value
rastri_gin(op_tim$par, pa_ram=1)
# Sample of normal variables
r_norm <- rnorm(1000, mean=4, sd=2)
# Objective function is log-likelihood
object_ive <- function(pa_r, r_norm) {
  sum(2*log(pa_r[2]) +
    ((r_norm - pa_r[1])/pa_r[2])^2)
}  # end object_ive
# Vectorize objective function
vec_objective <- Vectorize(
  FUN=function(mean, sd, r_norm)
    object_ive(c(mean, sd), r_norm),
  vectorize.args=c("mean", "sd")
)  # end Vectorize
# Objective function on parameter grid
par_mean <- seq(1, 6, length=50)
par_sd <- seq(0.5, 3.0, length=50)
objective_grid <- outer(par_mean, par_sd,
vec_objective, r_norm=r_norm)
objective_min <- which(  # grid search
  objective_grid==min(objective_grid),
  arr.ind=TRUE)
objective_min
par_mean[objective_min[1]]  # mean
par_sd[objective_min[2]]  # sd
objective_grid[objective_min]
objective_grid[(objective_min[, 1] + -1:1),
       (objective_min[, 2] + -1:1)]
par(cex.lab=2.0, cex.axis=2.0, cex.main=2.0, cex.sub=2.0)
# Perspective plot of log-likelihood function
persp(z=-objective_grid,
theta=45, phi=30, shade=0.5,
border="green", zlab="objective",
main="objective function")
# Interactive perspective plot of log-likelihood function
library(rgl)  # Load package rgl
par3d(cex=2.0)  # Scale text by factor of 2
persp3d(z=-objective_grid, zlab="objective",
  col="green", main="objective function")
# Initial parameters
par_init <- c(mean=0, sd=1)
# Perform optimization using optim()
optim_fit <- optim(par=par_init,
  fn=object_ive, # log-likelihood function
  r_norm=r_norm,
  method="L-BFGS-B", # quasi-Newton method
  upper=c(10, 10), # upper constraint
  lower=c(-10, 0.1)) # lower constraint
# Optimal parameters
optim_fit$par
# Perform optimization using MASS::fitdistr()
optim_fit <- MASS::fitdistr(r_norm, densfun="normal")
optim_fit$estimate
optim_fit$sd
# Plot histogram
histo_gram <- hist(r_norm, plot=FALSE)
plot(histo_gram, freq=FALSE,
     main="histogram of sample")
curve(expr=dnorm(x, mean=optim_fit$par["mean"],
           sd=optim_fit$par["sd"]),
add=TRUE, type="l", lwd=2, col="red")
legend("topright", inset=0.0, cex=0.8, title=NULL,
 leg="optimal parameters",
 lwd=2, bg="white", col="red")
# Sample from mixture of normal distributions
r_norm <- c(rnorm(100, sd=1.0),
      rnorm(100, mean=4, sd=1.0))
# Objective function is log-likelihood
object_ive <- function(pa_r, r_norm) {
  likelihood <- pa_r[1]/pa_r[3] *
  dnorm((r_norm-pa_r[2])/pa_r[3]) +
  (1-pa_r[1])/pa_r[5]*dnorm((r_norm-pa_r[4])/pa_r[5])
  if (any(likelihood <= 0)) Inf else
    -sum(log(likelihood))
}  # end object_ive
# Vectorize objective function
vec_objective <- Vectorize(
  FUN=function(mean, sd, w, m1, s1, r_norm)
    object_ive(c(w, m1, s1, mean, sd), r_norm),
  vectorize.args=c("mean", "sd")
)  # end Vectorize
# Objective function on parameter grid
par_mean <- seq(3, 5, length=50)
par_sd <- seq(0.5, 1.5, length=50)
objective_grid <- outer(par_mean, par_sd,
    vec_objective, r_norm=r_norm,
    w=0.5, m1=2.0, s1=2.0)
rownames(objective_grid) <- round(par_mean, 2)
colnames(objective_grid) <- round(par_sd, 2)
objective_min <- which(objective_grid==
  min(objective_grid), arr.ind=TRUE)
objective_min
objective_grid[objective_min]
objective_grid[(objective_min[, 1] + -1:1),
         (objective_min[, 2] + -1:1)]
# Perspective plot of objective function
persp(par_mean, par_sd, -objective_grid,
theta=45, phi=30,
shade=0.5,
col=rainbow(50),
border="green",
main="objective function")
# Initial parameters
par_init <- c(weight=0.5, m1=0, s1=1, m2=2, s2=1)
# Perform optimization
optim_fit <- optim(par=par_init,
      fn=object_ive,
      r_norm=r_norm,
      method="L-BFGS-B",
      upper=c(1,10,10,10,10),
      lower=c(0,-10,0.2,-10,0.2))
optim_fit$par
# Plot histogram
histo_gram <- hist(r_norm, plot=FALSE)
plot(histo_gram, freq=FALSE,
     main="histogram of sample")
fit_func <- function(x, pa_r) {
  pa_r["weight"] *
    dnorm(x, mean=pa_r["m1"], sd=pa_r["s1"]) +
  (1-pa_r["weight"]) *
    dnorm(x, mean=pa_r["m2"], sd=pa_r["s2"])
}  # end fit_func
curve(expr=fit_func(x, pa_r=optim_fit$par), add=TRUE,
type="l", lwd=2, col="red")
legend("topright", inset=0.0, cex=0.8, title=NULL,
 leg="optimal parameters",
 lwd=2, bg="white", col="red")
# Rastrigin function with vector argument for optimization
rastri_gin <- function(vec_tor, pa_ram=25){
  sum(vec_tor^2 - pa_ram*cos(vec_tor))
}  # end rastri_gin
vec_tor <- c(pi/6, pi/6)
rastri_gin(vec_tor=vec_tor)
library(DEoptim)
Optimize rastri_gin using DEoptim
op_tim <-  DEoptim(rastri_gin,
  upper=c(6, 6), lower=c(-6, -6),
  DEoptim.control(trace=FALSE, itermax=50))
# Optimal parameters and value
op_tim$optim$bestmem
rastri_gin(op_tim$optim$bestmem)
summary(op_tim)
plot(op_tim)
library(HighFreq)
# Select ETF symbols
sym_bols <- c("IEF", "DBC", "XLU", "XLF", "XLP", "XLI")
# calculate ETF prices and simple returns (not percentage)
price_s <- rutils::etf_env$price_s[, sym_bols]
price_s <- xts:::na.locf.xts(price_s)
price_s <- xts:::na.locf.xts(price_s, fromLast=TRUE)
date_s <- index(price_s)
re_turns <- rutils::diff_it(price_s)
# de-mean (center) and scale the returns
re_turns <- t(t(re_turns) - colMeans(re_turns))
re_turns <- t(t(re_turns) / sqrt(colSums(re_turns^2)/(NROW(re_turns)-1)))
re_turns <- xts(re_turns, date_s)
# alternative de-mean (center) and scale the returns
# re_turns <- scale(re_turns, center=TRUE, scale=TRUE)
# re_turns <- xts(re_turns, date_s)
# or
# re_turns <- lapply(re_turns, function(x) {x - sum(x)/NROW(re_turns)})
# re_turns <- rutils::do_call(cbind, re_turns)
# re_turns <- apply(re_turns, 2, scale)
# covariance matrix and variance vector of returns
cov_mat <- cov(re_turns)
vari_ance <- diag(cov_mat)
cor_mat <- cor(re_turns)
# cov_mat <- crossprod(re_turns) / (NROW(re_turns)-1)
# cor_mat <- cov_mat / sqrt(vari_ance)
# cor_mat <- t(t(cor_mat) / sqrt(vari_ance))
# reorder correlation matrix based on clusters
library(corrplot)
or_der <- corrMatOrder(cor_mat,
        order="hclust",
        hclust.method="complete")
cor_mat <- cor_mat[or_der, or_der]
# Plot the correlation matrix
col_ors <- colorRampPalette(c("red", "white", "blue"))
corrplot(cor_mat, title="ETF Correlation Matrix",
    tl.col="black", tl.cex=0.8, mar=c(0,0,1,0),
    method="square", col=col_ors(8),
    cl.offset=0.75, cl.cex=0.7,
    cl.align.text="l", cl.ratio=0.25)
# draw rectangles on the correlation matrix plot
corrRect.hclust(cor_mat, k=NROW(cor_mat) %/% 2,
          method="complete", col="red")
# Plot the correlation matrix
col_ors <- colorRampPalette(c("red", "white", "blue"))
corrplot(cor_mat, title="Correlation Matrix",
    tl.col="black", tl.cex=0.8, mar = c(0,0,1,0),
    method="square", col=col_ors(NCOL(cor_mat)),
    cl.offset=0.75, cl.cex=0.7,
    cl.align.text="l", cl.ratio=0.25)
# draw rectangles on the correlation matrix plot
corrRect.hclust(cor_mat, k=NCOL(cor_mat) %/% 2,
    method="complete", col="red")
# create initial vector of portfolio weights
n_weights <- NROW(sym_bols)
weight_s <- rep(1/sqrt(n_weights), n_weights)
names(weight_s) <- sym_bols
# objective function equal to minus portfolio variance
object_ive <- function(weight_s, re_turns) {
  portf_rets <- re_turns %*% weight_s
  -sum(portf_rets^2) +
    1e7*(1 - sum(weight_s^2))^2
}  # end object_ive
# objective for equal weight portfolio
object_ive(weight_s, re_turns)
# compare speed of vector multiplication methods
summary(microbenchmark(
  trans_pose=(t(re_turns[, 1]) %*% re_turns[, 1]),
  s_um=sum(re_turns[, 1]^2),
  times=10))[, c(1, 4, 5)]
# find weights with maximum variance
optim_run <- optim(par=weight_s,
  fn=object_ive,
  re_turns=re_turns,
  method="L-BFGS-B",
  upper=rep(10.0, n_weights),
  lower=rep(-10.0, n_weights))
# optimal weights and maximum variance
weight_s <- optim_run$par
-object_ive(weight_s, re_turns)
# Plot first principal component weights
barplot(weight_s, names.arg=names(weight_s),
  xlab="", ylab="",
  main="First Principal Component Weights")
# pc1 weights and returns
weights_1 <- weight_s
pc_1 <- re_turns %*% weights_1
# redefine objective function
object_ive <- function(weight_s, re_turns) {
  portf_rets <- re_turns %*% weight_s
  -sum(portf_rets^2) +
    1e7*(1 - sum(weight_s^2))^2 +
    1e7*(sum(weights_1*weight_s))^2
}  # end object_ive
# find second PC weights using parallel DEoptim
optim_run <- DEoptim::DEoptim(fn=object_ive,
  upper=rep(10, NCOL(re_turns)),
  lower=rep(-10, NCOL(re_turns)),
  re_turns=re_turns, control=list(parVar="weights_1",
    trace=FALSE, itermax=1000, parallelType=1))
# pc2 weights and returns
weights_2 <- optim_run$optim$bestmem
names(weights_2) <- colnames(re_turns)
sum(weights_2^2)
sum(weights_1*weights_2)
# Plot second principal component loadings
barplot(weights_2, names.arg=names(weights_2),
  xlab="", ylab="",
  main="Second Principal Component Loadings")
# calculate eigenvectors and eigenvalues
ei_gen <- eigen(cov_mat)
ei_gen$vectors
weights_1
weights_2
ei_gen$values[1]
var(pc_1)
(cov_mat %*% weights_1) / weights_1
ei_gen$values[2]
var(pc_2)
(cov_mat %*% weights_2) / weights_2
sum(vari_ance)
sum(ei_gen$values)
barplot(ei_gen$values, # Plot eigenvalues
  names.arg=paste0("PC", 1:n_weights),
  las=3, xlab="", ylab="", main="Principal Component Variances")
# eigen decomposition of covariance matrix
re_turns <- rutils::diff_it(price_s)
cov_mat <- cov(re_turns)
ei_gen <- eigen(cov_mat)
# Perform PCA without scaling
pc_a <- prcomp(re_turns, scale=FALSE)
# compare outputs
all.equal(ei_gen$values, pc_a$sdev^2)
all.equal(abs(ei_gen$vectors), abs(pc_a$rotation),
    check.attributes=FALSE)
# eigen decomposition of correlation matrix
cor_mat <- cor(re_turns)
ei_gen <- eigen(cor_mat)
# Perform PCA with scaling
pc_a <- prcomp(re_turns, scale=TRUE)
# compare outputs
all.equal(ei_gen$values, pc_a$sdev^2)
all.equal(abs(ei_gen$vectors), abs(pc_a$rotation),
    check.attributes=FALSE)
# redefine objective function to minimize variance
object_ive <- function(weight_s, re_turns) {
  portf_rets <- re_turns %*% weight_s
  sum(portf_rets^2) +
    1e7*(1 - sum(weight_s^2))^2
}  # end object_ive
# find highest order PC weights using parallel DEoptim
optim_run <- DEoptim::DEoptim(fn=object_ive,
  upper=rep(10, NCOL(re_turns)),
  lower=rep(-10, NCOL(re_turns)),
  re_turns=re_turns, control=list(trace=FALSE,
    itermax=1000, parallelType=1))
# pc6 weights and returns
weights_6 <- optim_run$optim$bestmem
names(weights_6) <- colnames(re_turns)
sum(weights_6^2)
sum(weights_1*weights_6)
# calculate objective function
object_ive(weights_6, re_turns)
object_ive(ei_gen$vectors[, 6], re_turns)
# Plot highest order principal component loadings
barplot(weights_6, names.arg=names(weights_2),
  xlab="", ylab="",
  main="Highest Order Principal Component Loadings")
# Perform principal component analysis PCA
pc_a <- prcomp(re_turns, scale=TRUE)
# Plot standard deviations of principal components
barplot(pc_a$sdev,
  names.arg=colnames(pc_a$rotation),
  las=3, xlab="", ylab="",
  main="Scree Plot: Volatilities of Principal Components
  of Stock Returns")
# Calculate principal component loadings (weights)
pc_a$rotation
# Plot barplots with PCA weights in multiple panels
par(mfrow=c(n_weights/2, 2))
par(mar=c(2, 2, 2, 1), oma=c(0, 0, 0, 0))
for (or_der in 1:n_weights) {
  barplot(pc_a$rotation[, or_der],
  las=3, xlab="", ylab="", main="")
  title(paste0("PC", or_der), line=-2.0,
  col.main="red")
}  # end for
# Calculate principal component time series
pca_rets <- xts(re_turns %*% pc_a$rotation,
          order.by=date_s)
round(cov(pca_rets), 3)
all.equal(coredata(pca_rets), pc_a$x, check.attributes=FALSE)
pca_ts <- xts:::cumsum.xts(pca_rets)
# Plot principal component time series in multiple panels
par(mfrow=c(n_weights/2, 2))
par(mar=c(2, 2, 0, 1), oma=c(0, 0, 0, 0))
ra_nge <- range(pca_ts)
for (or_der in 1:n_weights) {
  plot.zoo(pca_ts[, or_der],
     ylim=ra_nge,
     xlab="", ylab="")
  title(paste0("PC", or_der), line=-2.0)
}  # end for
par(mfrow=c(n_weights/2, 2))
par(mar=c(2, 2, 0, 1), oma=c(0, 0, 0, 0))
# Invert all the principal component time series
pca_rets <- re_turns %*% pc_a$rotation
sol_ved <- pca_rets %*% solve(pc_a$rotation)
all.equal(coredata(re_turns), sol_ved)
# Invert first 3 principal component time series
sol_ved <- pca_rets[, 1:3] %*% solve(pc_a$rotation)[1:3, ]
sol_ved <- xts::xts(sol_ved, date_s)
sol_ved <- xts:::cumsum.xts(sol_ved)
cum_returns <- xts:::cumsum.xts(re_turns)
# Plot the solved returns
for (sym_bol in sym_bols) {
  plot.zoo(
    cbind(cum_returns[, sym_bol], sol_ved[, sym_bol]),
    plot.type="single", col=c("black", "blue"), xlab="", ylab="")
  legend(x="topleft", bty="n",
   legend=paste0(sym_bol, c("", " solved")),
   title=NULL, inset=0.0, cex=1.0, lwd=6,
   lty=1, col=c("black", "blue"))
}  # end for
# Linear constraint
weight_s <- weight_s/sum(weight_s)
# Quadratic constraint
weight_s <- weight_s/sqrt(sum(weight_s^2))
# Box constraints
weight_s[weight_s > 1] <- 1
weight_s[weight_s < 0] <- 0
library(quantmod)
library(Rglpk)
# vector of symbol names
sym_bols <- c("VTI", "IEF", "DBC")
n_weights <- NROW(sym_bols)
# Calculate mean returns
re_turns <- rutils::etf_env$re_turns[, sym_bols]
re_turns <- xts:::na.locf.xts(re_turns)
re_turns <- na.omit(re_turns)
mean_rets <- colMeans(re_turns)
# Specify weight constraints
constraint_s <- matrix(c(rep(1, n_weights),
                 1, 1, 0),
                 nc=n_weights, byrow=TRUE)
direction_s <- c("==", "<=")
rh_s <- c(1, 0)
# Specify weight bounds (-1, 1) (default is c(0, Inf))
bound_s <-
  list(lower=list(ind=1:n_weights, val=rep(-1, n_weights)),
 upper=list(ind=1:n_weights, val=rep(1, n_weights)))
# perform optimization
op_tim <- Rglpk::Rglpk_solve_LP(
  obj=mean_rets,
  mat=constraint_s,
  dir=direction_s,
  rhs=rh_s,
  bounds=bound_s,
  max=TRUE)
unlist(op_tim[1:2])
# Calculate covariance matrix of returns and its inverse
cov_mat <- cov(re_turns)
cov_inv <- solve(a=cov_mat)
u_nit <- rep(1, NCOL(cov_mat))
# minimum variance weights with constraint
# weight_s <- solve(a=cov_mat, b=u_nit)
weight_s <- cov_inv %*% u_nit
weight_s <- weight_s / drop(t(u_nit) %*% weight_s)
# minimum variance
t(weight_s) %*% cov_mat %*% weight_s
1/(t(u_nit) %*% cov_inv %*% u_nit)
# Calculate vector of mean returns
mean_rets <- colMeans(re_turns)
# Specify the target return
tar_get <- 1.5*mean(re_turns)
# products of inverse with mean returns and unit vector
f_mat <- matrix(c(
  t(u_nit) %*% cov_inv %*% u_nit,
  t(u_nit) %*% cov_inv %*% mean_rets,
  t(mean_rets) %*% cov_inv %*% u_nit,
  t(mean_rets) %*% cov_inv %*% mean_rets), nc=2)
# Solve for the Lagrange multipliers
multipli_ers <-
  solve(a=f_mat, b=c(2, 2*tar_get))
# Calculate weights
weight_s <- drop(0.5*cov_inv %*%
  cbind(u_nit, mean_rets) %*% multipli_ers)
# Calculate constraints
all.equal(1, sum(weight_s))
all.equal(tar_get, sum(mean_rets*weight_s))
# Calculate portfolio return and standard deviation
portf_rets <- drop(re_turns %*% weight_s)
c(return=mean(portf_rets), sd=sd(portf_rets))
all.equal(mean(portf_rets), tar_get)
# Calculate portfolio variance
uu <- c(1, tar_get)
f_inv <- solve(f_mat)
all.equal(var(portf_rets), drop(t(uu) %*% f_inv %*% uu))
# Calculate vertex of variance parabola
weight_s <- drop(cov_inv %*% u_nit /
  drop(t(u_nit) %*% cov_inv %*% u_nit))
portf_rets <- drop(re_turns %*% weight_s)
v_rets <-
  drop(t(u_nit) %*% cov_inv %*% mean_rets /
  t(u_nit) %*% cov_inv %*% u_nit)
all.equal(mean(portf_rets), v_rets)
var_min <-
  drop(1/t(u_nit) %*% cov_inv %*% u_nit)
all.equal(var(portf_rets), var_min)
# Calculate efficient frontier
target_s <- v_rets*(1+seq(from=-1, to=1, by=0.1))
eff_front <- sapply(target_s, function(tar_get) {
  uu <- c(1, tar_get)
  sqrt(drop(t(uu) %*% f_inv %*% uu))
})  # end sapply
# Plot efficient frontier
x11(width=6, height=5)
plot(x=eff_front, y=target_s, t="l", col="blue", lwd=2,
     main="Efficient Frontier and Minimum Variance Portfolio",
     xlab="standard deviation", ylab="return")
points(x=sqrt(var_min), y=v_rets, col="green", lwd=6)
text(x=sqrt(var_min), y=v_rets, labels="minimum \nvariance",
     pos=4, cex=0.8)
# Calculate portfolio standard deviation
std_dev <- sqrt(drop(t(uu) %*% f_inv %*% uu))
# Calculate the slope of the tangent line
slop_e <- (std_dev*det(f_mat))/(f_mat[1, 1]*tar_get-f_mat[1, 2])
# Calculate the risk-free rate as intercept of the tangent line
risk_free <- tar_get - slop_e*std_dev
# Calculate the risk-free rate from target return
risk_free <- (tar_get*f_mat[1, 2]-f_mat[2, 2]) /
  (tar_get*f_mat[1, 1]-f_mat[1, 2])
# Plot efficient frontier
plot(x=eff_front, y=target_s, t="l", col="blue", lwd=2,
     xlim=c(0.0, max(eff_front)),
     main="Efficient Frontier and Tangency Portfolio",
     xlab="standard deviation", ylab="return")
# Plot minimum variance
points(x=sqrt(var_min), y=v_rets, col="green", lwd=6)
text(x=sqrt(var_min), y=v_rets, labels="minimum \nvariance",
     pos=4, cex=0.8)
# Plot tangent point
points(x=std_dev, y=tar_get, col="red", lwd=6)
text(x=std_dev, y=tar_get, labels="tangency\nportfolio", pos=2, cex=0.8)
# Plot risk-free point
points(x=0, y=risk_free, col="red", lwd=6)
text(x=0, y=risk_free, labels="risk-free", pos=4, cex=0.8)
# Plot tangent line
abline(a=risk_free, b=slop_e, lwd=2, col="green")
# Calculate excess re_turns
risk_free <- 0.03/252
ex_cess <- re_turns - risk_free
# Calculate covariance and inverse matrix
cov_mat <- cov(re_turns)
u_nit <- rep(1, NCOL(cov_mat))
cov_inv <- solve(a=cov_mat)
# Calculate mean excess returns
ex_cess <- sapply(ex_cess, mean)
# weights of maximum Sharpe portfolio
# weight_s <- solve(a=cov_mat, b=re_turns)
weight_s <- cov_inv %*% ex_cess
weight_s <- weight_s/drop(t(u_nit) %*% weight_s)
# Sharpe ratios
sqrt(252)*sum(weight_s * ex_cess) /
  sqrt(drop(weight_s %*% cov_mat %*% weight_s))
sapply(re_turns - risk_free,
  function(x) sqrt(252)*mean(x)/sd(x))
weights_maxsharpe <- weight_s
library(quantmod)
# Calculate minimum variance weights
weight_s <- cov_inv %*% u_nit
weights_minvar <-
  weight_s / drop(t(u_nit) %*% weight_s)
# Calculate optimal portfolio returns
optim_rets <- xts(
  x=cbind(exp(cumsum(re_turns %*% weights_maxsharpe)),
    exp(cumsum(re_turns %*% weights_minvar))),
  order.by=index(re_turns))
colnames(optim_rets) <- c("maxsharpe", "minvar")
# Plot optimal portfolio returns, with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "green")
x11(width=6, height=5)
chart_Series(optim_rets, theme=plot_theme,
       name="Maximum Sharpe and \nMinimum Variance portfolios")
legend("top", legend=colnames(optim_rets), cex=0.8,
 inset=0.1, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")
x11(wid_th <- 6, hei_ght <- 6)
# Calculate minimum variance weights
weight_s <- cov_inv %*% u_nit
weight_s <- weight_s / drop(t(u_nit) %*% weight_s)
# minimum standard deviation and return
std_dev <- sqrt(252*drop(weight_s %*% cov_mat %*% weight_s))
min_ret <- 252*sum(weight_s * mean_rets)
# Calculate maximum Sharpe portfolios
risk_free <- (min_ret * seq(-10, 10, by=0.1)^3)/252
eff_front <- sapply(risk_free, function(risk_free) {
  weight_s <- cov_inv %*% (mean_rets - risk_free)
  weight_s <- weight_s/drop(t(u_nit) %*% weight_s)
  # portfolio return and standard deviation
  c(return=252*sum(weight_s * mean_rets),
    stddev=sqrt(252*drop(weight_s %*% cov_mat %*% weight_s)))
})  # end sapply
eff_front <- cbind(252*risk_free, t(eff_front))
colnames(eff_front)[1] <- "risk-free"
eff_front <- eff_front[is.finite(eff_front[, "stddev"]), ]
eff_front <- eff_front[order(eff_front[, "return"]), ]
# Plot maximum Sharpe portfolios
plot(x=eff_front[, "stddev"],
     y=eff_front[, "return"], t="l",
     xlim=c(0.0*std_dev, 3.0*std_dev),
     ylim=c(0.0*min_ret, 2.0*min_ret),
     main="Efficient Frontier and Capital Market Line",
     xlab="standard deviation", ylab="return")
points(x=eff_front[, "stddev"], y=eff_front[, "return"],
 col="red", lwd=3)
# Plot minimum variance portfolio
points(x=std_dev, y=min_ret, col="green", lwd=6)
text(std_dev, min_ret, labels="minimum \nvariance",
     pos=4, cex=0.8)
# Draw Capital Market Line
sor_ted <- sort(eff_front[, 1])
risk_free <-
  sor_ted[findInterval(x=0.5*min_ret, vec=sor_ted)]
points(x=0, y=risk_free, col="blue", lwd=6)
text(x=0, y=risk_free, labels="risk-free",
     pos=4, cex=0.8)
in_dex <- match(risk_free, eff_front[, 1])
points(x=eff_front[in_dex, "stddev"],
 y=eff_front[in_dex, "return"],
 col="blue", lwd=6)
text(x=eff_front[in_dex, "stddev"],
     y=eff_front[in_dex, "return"],
     labels="market portfolio",
     pos=2, cex=0.8)
sharp_e <- (eff_front[in_dex, "return"]-risk_free)/
  eff_front[in_dex, "stddev"]
abline(a=risk_free, b=sharp_e, col="blue", lwd=2)
text(x=0.7*eff_front[in_dex, "stddev"],
     y=0.7*eff_front[in_dex, "return"]+0.01,
     labels="Capital Market Line", pos=2, cex=0.8,
     srt=45*atan(sharp_e*hei_ght/wid_th)/(0.25*pi))
# Calculate random portfolios
n_portf <- 1000
ret_sd <- sapply(1:n_portf, function(in_dex) {
  weight_s <- runif(n_weights-1, min=-0.25, max=1.0)
  weight_s <- c(weight_s, 1-sum(weight_s))
  # portfolio return and standard deviation
  c(return=252*sum(weight_s * mean_rets),
    stddev=sqrt(252*drop(weight_s %*% cov_mat %*% weight_s)))
})  # end sapply
# Plot scatterplot of random portfolios
x11(wid_th <- 6, hei_ght <- 6)
plot(x=ret_sd["stddev", ], y=ret_sd["return", ],
     main="Efficient Frontier and Random Portfolios",
     xlim=c(0.5*std_dev, 0.8*max(ret_sd["stddev", ])),
     xlab="standard deviation", ylab="return")
# Plot maximum Sharpe portfolios
lines(x=eff_front[, "stddev"],
     y=eff_front[, "return"], lwd=2)
points(x=eff_front[, "stddev"], y=eff_front[, "return"],
 col="red", lwd=3)
# Plot minimum variance portfolio
points(x=std_dev, y=min_ret, col="green", lwd=6)
text(std_dev, min_ret, labels="minimum\nvariance",
     pos=2, cex=0.8)
# Plot market portfolio
points(x=eff_front[in_dex, "stddev"],
 y=eff_front[in_dex, "return"], col="green", lwd=6)
text(x=eff_front[in_dex, "stddev"],
     y=eff_front[in_dex, "return"],
     labels="market\nportfolio",
     pos=2, cex=0.8)
# Plot individual assets
points(x=sqrt(252*diag(cov_mat)),
 y=252*mean_rets, col="blue", lwd=6)
text(x=sqrt(252*diag(cov_mat)), y=252*mean_rets,
     labels=names(mean_rets),
     col="blue", pos=1, cex=0.8)
risk_free <- 0.03
re_turns <- c(asset1=0.05, asset2=0.06)
std_devs <- c(asset1=0.4, asset2=0.5)
cor_rel <- 0.6
cov_mat <- matrix(c(1, cor_rel, cor_rel, 1), nc=2)
cov_mat <- t(t(std_devs*cov_mat)*std_devs)
weight_s <- seq(from=-1, to=2, length.out=31)
weight_s <- cbind(weight_s, 1-weight_s)
portf_rets <- weight_s %*% re_turns
portf_sd <-
  sqrt(rowSums(weight_s * (weight_s %*% cov_mat)))
sharpe_ratios <- (portf_rets-risk_free)/portf_sd
in_dex <- which.max(sharpe_ratios)
max_Sharpe <- max(sharpe_ratios)
# Plot efficient frontier
x11(wid_th <- 6, hei_ght <- 5)
par(mar=c(3,3,2,1)+0.1, oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
plot(portf_sd, portf_rets, t="l",
 main=paste0("Efficient frontier and CML for two assets\ncorrelation = ", 100*cor_rel, "%"),
 xlab="standard deviation", ylab="return",
 lwd=2, col="orange",
 xlim=c(0, max(portf_sd)),
 ylim=c(0.02, max(portf_rets)))
# Add Market Portfolio (maximum Sharpe ratio portfolio)
points(portf_sd[in_dex], portf_rets[in_dex],
 col="blue", lwd=3)
text(x=portf_sd[in_dex], y=portf_rets[in_dex],
     labels=paste(c("market portfolio\n",
 structure(c(weight_s[in_dex], 1-weight_s[in_dex]),
         names=names(re_turns))), collapse=" "),
     pos=2, cex=0.8)
# Plot individual assets
points(std_devs, re_turns, col="green", lwd=3)
text(std_devs, re_turns, labels=names(re_turns), pos=4, cex=0.8)
# Add point at risk-free rate and draw Capital Market Line
points(x=0, y=risk_free, col="blue", lwd=3)
text(0, risk_free, labels="risk-free\nrate", pos=4, cex=0.8)
abline(a=risk_free, b=max_Sharpe, lwd=2, col="blue")
range_s <- par("usr")
text(portf_sd[in_dex]/2, (portf_rets[in_dex]+risk_free)/2,
     labels="Capital Market Line", cex=0.8, , pos=3,
     srt=45*atan(max_Sharpe*(range_s[2]-range_s[1])/
             (range_s[4]-range_s[3])*
             hei_ght/wid_th)/(0.25*pi))
# Plot portfolios in x11() window
x11(wid_th <- 6, hei_ght <- 5)
par(oma=c(0, 0, 0, 0), mar=c(3,3,2,1)+0.1, mgp=c(2, 1, 0), cex.lab=1.0, cex.axis=1.0, cex.main=1.0, cex.sub=1.0)
# vector of symbol names
sym_bols <- c("VTI", "IEF")
# matrix of portfolio weights
weight_s <- seq(from=-1, to=2, length.out=31)
weight_s <- cbind(weight_s, 1-weight_s)
# Calculate portfolio returns and volatilities
re_turns <- rutils::etf_env$re_turns[, sym_bols]
ret_sd <- re_turns %*% t(weight_s)
ret_sd <- cbind(252*colMeans(ret_sd),
  sqrt(252)*matrixStats::colSds(ret_sd))
colnames(ret_sd) <- c("returns", "stddev")
risk_free <- 0.06
ret_sd <- cbind(ret_sd,
  (ret_sd[, "returns"]-risk_free)/ret_sd[, "stddev"])
colnames(ret_sd)[3] <- "Sharpe"
in_dex <- which.max(ret_sd[, "Sharpe"])
max_Sharpe <- ret_sd[in_dex, "Sharpe"]
plot(x=ret_sd[, "stddev"], y=ret_sd[, "returns"],
     main="Stock and Bond portfolios", t="l",
     xlim=c(0, 0.7*max(ret_sd[, "stddev"])), ylim=c(0, max(ret_sd[, "returns"])),
     xlab="standard deviation", ylab="return")
# Add blue point for market portfolio
points(x=ret_sd[in_dex, "stddev"], y=ret_sd[in_dex, "returns"], col="blue", lwd=6)
text(x=ret_sd[in_dex, "stddev"], y=ret_sd[in_dex, "returns"],
     labels=paste(c("market portfolio\n", structure(c(weight_s[in_dex, 1], weight_s[in_dex, 2]), names=sym_bols)), collapse=" "),
     pos=3, cex=0.8)
# Plot individual assets
mean_rets <- 252*sapply(re_turns, mean)
std_devs <- sqrt(252)*sapply(re_turns, sd)
points(std_devs, mean_rets, col="green", lwd=6)
text(std_devs, mean_rets, labels=names(re_turns), pos=2, cex=0.8)
# Add point at risk-free rate and draw Capital Market Line
points(x=0, y=risk_free, col="blue", lwd=6)
text(0, risk_free, labels="risk-free", pos=4, cex=0.8)
abline(a=risk_free, b=max_Sharpe, col="blue", lwd=2)
range_s <- par("usr")
text(max(ret_sd[, "stddev"])/3, 0.75*max(ret_sd[, "returns"]),
     labels="Capital Market Line", cex=0.8, , pos=3,
     srt=45*atan(max_Sharpe*(range_s[2]-range_s[1])/
             (range_s[4]-range_s[3])*
             hei_ght/wid_th)/(0.25*pi))
# Plot portfolios in x11() window
x11(wid_th <- 6, hei_ght <- 5)
# Calculate cumulative returns of VTI and IEF
optim_rets <- lapply(re_turns,
  function(re_turns) exp(cumsum(re_turns)))
optim_rets <- rutils::do_call(cbind, optim_rets)
# Calculate market portfolio returns
optim_rets <- cbind(
  exp(cumsum(re_turns %*%
    c(weight_s[in_dex], 1-weight_s[in_dex]))),
  optim_rets)
colnames(optim_rets)[1] <- "market"
# Plot market portfolio with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue", "green")
chart_Series(optim_rets, theme=plot_theme,
       name="Market portfolio for stocks and bonds")
legend("top", legend=colnames(optim_rets),
 cex=0.8, inset=0.1, bg="white", lty=1,
 lwd=6, col=plot_theme$col$line.col, bty="n")
x11(width=6, height=4)
par(mar=c(3, 2, 1, 0), oma=c(0, 0, 0, 0))
# VTI percentage returns
re_turns <- rutils::diff_it(log(Ad(rutils::etf_env$VTI)))
conf_level <- 0.1
va_r <- quantile(re_turns, conf_level)
c_var <- mean(re_turns[re_turns < va_r])
# or
sort_ed <- sort(as.numeric(re_turns))
in_dex <- round(conf_level*NROW(re_turns))
va_r <- sort_ed[in_dex]
c_var <- mean(sort_ed[1:in_dex])
# Plot histogram of VTI returns
histo_gram <- hist(re_turns, col="lightgrey",
  xlab="returns", breaks=100, xlim=c(-0.05, 0.01),
  ylab="frequency", freq=FALSE,
  main="VTI returns histogram")
dens_ity <- density(re_turns, adjust=1.5)
lines(dens_ity, lwd=3, col="blue")
# Add line for VaR
abline(v=va_r, col="red", lwd=3)
text(x=va_r, y=20, labels="VaR",
     lwd=2, srt=90, pos=2)
# Add shading for CVaR
var_max <- -0.06
rang_e <- (dens_ity$x < va_r) & (dens_ity$x > var_max)
polygon(
  c(var_max, dens_ity$x[rang_e], va_r),
  c(0, dens_ity$y[rang_e], 0),
  col=rgb(1, 0, 0,0.5), border=NA)
text(x=va_r, y=3, labels="CVaR", lwd=2, pos=2)
library(HighFreq)
library(Rglpk)
# vector of symbol names and returns
sym_bols <- c("VTI", "IEF", "DBC")
n_weights <- NROW(sym_bols)
re_turns <- rutils::etf_env$re_turns[((NROW(re_turns)-6):NROW(re_turns)), sym_bols]
mean_rets <- colMeans(re_turns)
conf_level <- 0.05
r_min <- 0 ; w_min <- 0 ; w_max <- 1
weight_sum <- 1
n_cols <- NCOL(re_turns) # number of assets
n_rows <- NROW(re_turns) # number of rows
# Creat objective vector
obj_vector <- c(numeric(n_cols), rep(-1/(conf_level*n_rows), n_rows), -1)
# Specify weight constraints
constraint_s <- rbind(
  cbind(rbind(1, mean_rets),
  matrix(data=0, nrow=2, ncol=(n_rows+1))),
  cbind(coredata(re_turns), diag(n_rows), 1))
rh_s <- c(weight_sum, r_min, rep(0, n_rows))
direction_s <- c("==", ">=", rep(">=", n_rows))
# Specify weight bounds
bound_s <- list(
  lower=list(ind=1:n_cols, val=rep(w_min, n_cols)),
  upper=list(ind=1:n_cols, val=rep(w_max, n_cols)))
# perform optimization
op_tim <- Rglpk_solve_LP(obj=obj_vector, mat=constraint_s, dir=direction_s, rhs=rh_s, types=rep("C", NROW(obj_vector)), max=T, bounds=bound_s)
op_tim$solution
constraint_s %*% op_tim$solution
obj_vector %*% op_tim$solution
as.numeric(op_tim$solution[1:n_cols])
# Calculate daily percentage re_turns
sym_bols <- c("VTI", "IEF", "DBC")
re_turns <- rutils::etf_env$re_turns[, sym_bols]
# Create initial vector of portfolio weights
weight_s <- rep(1, NROW(sym_bols))
names(weight_s) <- sym_bols
# objective equal to minus Sharpe ratio
object_ive <- function(weight_s, re_turns) {
  portf_rets <- re_turns %*% weight_s
  if (sd(portf_rets) == 0)
    return(0)
  else
    return(-mean(portf_rets)/sd(portf_rets))
}  # end object_ive
# objective for equal weight portfolio
object_ive(weight_s, re_turns=re_turns)
op_tim <- unlist(optimize(
  f=function(weight)
    object_ive(c(1, 1, weight), re_turns=re_turns),
  interval=c(-4, 1)))
# vectorize objective function with respect to third weight
vec_object <- function(weights) sapply(weights,
  function(weight) object_ive(c(1, 1, weight),
    re_turns=re_turns))
# or
vec_object <- Vectorize(FUN=function(weight)
    object_ive(c(1, 1, weight), re_turns=re_turns),
  vectorize.args="weight")  # end Vectorize
vec_object(1)
vec_object(1:3)
x11(width=6, height=5)
par(oma=c(1, 1, 1, 1), mgp=c(2, 1, 0), mar=c(3, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# Plot objective function with respect to third weight
curve(expr=vec_object,
      type="l", xlim=c(-4.0, 1.0),
      xlab=paste("weight of", names(weight_s[3])),
      ylab="", lwd=2)
title(main="Objective Function", line=-1)  # Add title
points(x=op_tim[1], y=op_tim[2], col="green", lwd=6)
text(x=op_tim[1], y=op_tim[2],
     labels="minimum objective", pos=4, cex=0.8)

#below is simplified code for plotting objective function
# Create vector of DBC weights
weight_s <- seq(from=-4, to=1, by=0.1)
obj_val <- sapply(weight_s,
  function(weight) object_ive(c(1, 1, weight)))
plot(x=weight_s, y=obj_val, t="l",
      xlab="weight of DBC", ylab="", lwd=2)
title(main="Objective Function", line=-1)  # Add title
points(x=op_tim[1], y=op_tim[2], col="green", lwd=6)
text(x=op_tim[1], y=op_tim[2],
     labels="minimum objective", pos=4, cex=0.8)
# vectorize function with respect to all weights
vec_object <- Vectorize(
  FUN=function(w1, w2, w3)
    object_ive(c(w1, w2, w3)),
  vectorize.args=c("w2", "w3"))  # end Vectorize
# Calculate objective on 2-d (w2 x w3) parameter grid
w2 <- seq(-3, 7, length=50)
w3 <- seq(-5, 5, length=50)
grid_object <- outer(w2, w3, FUN=vec_object, w1=1)
rownames(grid_object) <- round(w2, 2)
colnames(grid_object) <- round(w3, 2)
# perspective plot of objective function
persp(w2, w3, -grid_object,
theta=45, phi=30, shade=0.5,
col=rainbow(50), border="green",
main="objective function")
# Interactive perspective plot of objective function
library(rgl)
rgl::persp3d(z=-grid_object, zlab="objective",
  col="green", main="objective function")
rgl::persp3d(
  x=function(w2, w3)
    -vec_object(w1=1, w2, w3),
  xlim=c(-3, 7), ylim=c(-5, 5),
  col="green", axes=FALSE)
# optimization to find weights with maximum Sharpe ratio
op_tim <- optim(par=weight_s,
             fn=object_ive,
             method="L-BFGS-B",
             upper=c(1.1, 10, 10),
             lower=c(0.9, -10, -10))
# optimal parameters
op_tim$par
op_tim$par <- op_tim$par/sum(op_tim$par)
# optimal Sharpe ratio
-object_ive(op_tim$par)
x11(width=6, height=5)
par(oma=c(1, 1, 1, 0), mgp=c(2, 1, 0), mar=c(2, 1, 2, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# Plot in two vertical panels
layout(matrix(c(1,2), 2),
 widths=c(1,1), heights=c(1,3))
# barplot of optimal portfolio weights
barplot(op_tim$par, col=c("red", "green", "blue"),
  main="Optimized portfolio weights")
# Calculate cumulative returns of VTI, IEF, DBC
cum_rets <- lapply(re_turns,
  function(re_turns) exp(cumsum(re_turns)))
cum_rets <- rutils::do_call(cbind, cum_rets)
# Calculate optimal portfolio returns with VTI, IEF, DBC
optim_rets <- cbind(
  exp(cumsum(re_turns %*% op_tim$par)),
  cum_rets)
colnames(optim_rets)[1] <- "optim_rets"
# Plot optimal returns with VTI, IEF, DBC
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("black", "red", "green", "blue")
chart_Series(optim_rets, theme=plot_theme,
       name="Optimized portfolio performance")
legend("top", legend=colnames(optim_rets), cex=0.8,
 inset=0.1, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")
# or plot non-compounded (simple) cumulative returns
PerformanceAnalytics::chart.CumReturns(
  cbind(re_turns %*% op_tim$par, re_turns),
  lwd=2, ylab="", legend.loc="topleft", main="")
risk_free <- 0.03
re_turns <- c(asset1=0.05, asset2=0.06)
std_devs <- c(asset1=0.4, asset2=0.5)
cor_rel <- 0.6
cov_mat <- matrix(c(1, cor_rel, cor_rel, 1), nc=2)
cov_mat <- t(t(std_devs*cov_mat)*std_devs)
library(quadprog)
# minimum variance weights without constraints
op_tim <- solve.QP(Dmat=2*cov_mat,
            dvec=rep(0, 2),
            Amat=matrix(0, nr=2, nc=1),
            bvec=0)
# minimum variance weights sum equal to 1
op_tim <- solve.QP(Dmat=2*cov_mat,
            dvec=rep(0, 2),
            Amat=matrix(1, nr=2, nc=1),
            bvec=1)
# optimal value of objective function
t(op_tim$solution) %*% cov_mat %*% op_tim$solution
perform simple optimization for reference
# objective function for simple optimization
object_ive <- function(x) {
  x <- c(x, 1-x)
  t(x) %*% cov_mat %*% x
}  # end object_ive
unlist(optimize(f=object_ive, interval=c(-1, 2)))
# Calculate daily percentage re_turns
sym_bols <- c("VTI", "IEF", "DBC")
re_turns <- rutils::etf_env$re_turns[, sym_bols]
# Calculate the covariance matrix
cov_mat <- cov(re_turns)
# minimum variance weights, with sum equal to 1
op_tim <- quadprog::solve.QP(Dmat=2*cov_mat,
            dvec=numeric(3),
            Amat=matrix(1, nr=3, nc=1),
            bvec=1)
# minimum variance, maximum returns
op_tim <- quadprog::solve.QP(Dmat=2*cov_mat,
            dvec=apply(0.1*re_turns, 2, mean),
            Amat=matrix(1, nr=3, nc=1),
            bvec=1)
# minimum variance positive weights, sum equal to 1
a_mat <- cbind(matrix(1, nr=3, nc=1),
       diag(3), -diag(3))
b_vec <- c(1, rep(0, 3), rep(-1, 3))
op_tim <- quadprog::solve.QP(Dmat=2*cov_mat,
            dvec=numeric(3),
            Amat=a_mat,
            bvec=b_vec,
            meq=1)
# Calculate daily percentage re_turns
re_turns <- rutils::etf_env$re_turns[, sym_bols]
# objective equal to minus Sharpe ratio
object_ive <- function(weight_s, re_turns) {
  portf_rets <- re_turns %*% weight_s
  if (sd(portf_rets) == 0)
    return(0)
  else
    return(-mean(portf_rets)/sd(portf_rets))
}  # end object_ive
# perform optimization using DEoptim
op_tim <- DEoptim::DEoptim(fn=object_ive,
  upper=rep(10, NCOL(re_turns)),
  lower=rep(-10, NCOL(re_turns)),
  re_turns=re_turns,
  control=list(trace=FALSE, itermax=100, parallelType=1))
weight_s <- op_tim$optim$bestmem/sum(abs(op_tim$optim$bestmem))
names(weight_s) <- colnames(re_turns)
# objective with shrinkage penalty
object_ive <- function(weight_s, re_turns, lamb_da, al_pha) {
  portf_rets <- re_turns %*% weight_s
  if (sd(portf_rets) == 0)
    return(0)
  else {
    penal_ty <- lamb_da*((1-al_pha)*sum(weight_s^2) +
al_pha*sum(abs(weight_s)))
    return(-mean(portf_rets)/sd(portf_rets) + penal_ty)
  }
}  # end object_ive
# objective for equal weight portfolio
weight_s <- rep(1, NROW(sym_bols))
names(weight_s) <- sym_bols
lamb_da <- 0.5 ; al_pha <- 0.5
object_ive(weight_s, re_turns=re_turns,
  lamb_da=lamb_da, al_pha=al_pha)
# perform optimization using DEoptim
op_tim <- DEoptim::DEoptim(fn=object_ive,
  upper=rep(10, NCOL(re_turns)),
  lower=rep(-10, NCOL(re_turns)),
  re_turns=re_turns,
  lamb_da=lamb_da,
  al_pha=al_pha,
  control=list(trace=FALSE, itermax=100, parallelType=1))
weight_s <-
  op_tim$optim$bestmem/sum(abs(op_tim$optim$bestmem))
names(weight_s) <- colnames(re_turns)
