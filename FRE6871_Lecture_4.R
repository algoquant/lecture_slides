# Define model parameters
n_assets <- 300; n_simu <- 1000; l_gd <- 0.4
# Define correlation parameters
rh_o <- 0.2; rho_sqrt <- sqrt(rh_o); rho_sqrtm <- sqrt(1-rh_o)
# Calculate default probabilities and thresholds
set.seed(1121)
def_probs <- runif(n_assets, max=0.2)
def_thresh <- qnorm(def_probs)
# Calculate vector of systematic factors
system_atic <- rnorm(n_simu)
# Simulate losses under Vasicek model
asset_s <-
  matrix(rnorm(n_simu*n_assets), ncol=n_simu)
asset_s <-
  t(rho_sqrt*system_atic + t(rho_sqrtm*asset_s))
loss_es <-
  l_gd*colSums(asset_s < def_thresh)/n_assets

# Calculate VaRs
level_s <- seq(0.93, 0.99, 0.01)
var_s <- quantile(loss_es, probs=level_s)
plot(x=level_s, y=var_s, t="l", lwd=2,
     xlab="Confidence Levels", ylab="VaRs",
     main="Simulated VaR and Confidence Levels")

# Calculate CVaRs
cvar_s <- sapply(var_s, function(va_r) {
  mean(loss_es[loss_es >=va_r])
})  # end sapply
cvar_s <- cbind(cvar_s, var_s)
# Alternative CVaR calculation using frequency table
# first calculate frequency table of loss_es
# ta_ble <- table(loss_es)/n_simu
# Calculate CVaRs from frequency table
# Cvar_s <- sapply(var_s, function(va_r) {
#   tai_l <- ta_ble[names(ta_ble) > va_r]
#   tai_l %*% as.numeric(names(tai_l)) / sum(tai_l)
# })  # end sapply
# Plot CVaRs
plot(x=level_s, y=cvar_s[, "cvar_s"],
     t="l", col="red", lwd=2,
     ylim=range(cvar_s),
     xlab="Confidence Levels", ylab="CVaRs",
     main="Simulated CVaR and Confidence Levels")

# Add VaRs
lines(x=level_s, y=cvar_s[, "var_s"], lwd=2)
# Add legend
legend(x="topleft", legend=c("CVaRs", "VaRs"), bty="n",
 title=NULL, inset=0.05, cex=0.8, bg="white",
 lwd=6, lty=1, col=c("red", "black"))

calc_var <- function(def_thresh, # Default thresholds
               l_gd=0.6, # loss given default
               rho_sqrt, rho_sqrtm, # asset correlation
               n_simu=1000, # number of simulations
               level_s=seq(0.93, 0.99, 0.01) # Confidence levels
               ) {
  # Define model parameters
  n_assets <- NROW(def_thresh)
  # Simulate losses under Vasicek model
  system_atic <- rnorm(n_simu)
  asset_s <- matrix(rnorm(n_simu*n_assets), ncol=n_simu)
  asset_s <- t(rho_sqrt*system_atic + t(rho_sqrtm*asset_s))
  loss_es <- l_gd*colSums(asset_s < def_thresh)/n_assets
  # Calculate VaRs and CVaRs
  var_s <- quantile(loss_es, probs=level_s)
  cvar_s <- sapply(var_s, function(va_r) {
    mean(loss_es[loss_es >= va_r])
  })  # end sapply
  names(var_s) <- level_s
  names(cvar_s) <- level_s
  c(var_s, cvar_s)
}  # end calc_var

# Define model parameters
n_assets <- 300; n_simu <- 1000; l_gd <- 0.4
rh_o <- 0.2; rho_sqrt <- sqrt(rh_o); rho_sqrtm <- sqrt(1-rh_o)
# Calculate default probabilities and thresholds
set.seed(1121)
def_probs <- runif(n_assets, max=0.2)
def_thresh <- qnorm(def_probs)
# Define number of bootstrap simulations
n_boot <- 500
# Perform bootstrap of calc_var
set.seed(1121)
boot_data <- sapply(rep(l_gd, n_boot),
  calc_var,
  def_thresh=def_thresh,
  rho_sqrt=rho_sqrt, rho_sqrtm=rho_sqrtm,
  n_simu=n_simu, level_s=level_s)  # end sapply
boot_data <- t(boot_data)
# Calculate vectors of standard errors of VaR and CVaR from boot_data data
std_error_var <- apply(boot_data[, 1:7], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
std_error_cvar <- apply(boot_data[, 8:14], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
# Scale the standard errors of VaRs and CVaRs
std_error_var[2, ] <- std_error_var[2, ]/std_error_var[1, ]
std_error_cvar[2, ] <- std_error_cvar[2, ]/std_error_cvar[1, ]

# Plot the standard errors of VaRs and CVaRs
plot(x=colnames(std_error_cvar),
  y=std_error_cvar[2, ], t="l", col="red", lwd=2,
  ylim=range(c(std_error_var[2, ], std_error_cvar[2, ])),
  xlab="level_s", ylab="Standard errors",
  main="Scaled standard errors of CVaR and VaR")
lines(x=colnames(std_error_var), y=std_error_var[2, ], lwd=2)
legend(x="topleft", legend=c("CVaRs", "VaRs"), bty="n",
 title=NULL, inset=0.05, cex=0.8, bg="white",
 lwd=6, lty=1, col=c("red", "black"))
NA

library(parallel)  # load package parallel
n_cores <- detectCores() - 1  # number of cores
clus_ter <- makeCluster(n_cores)  # Initialize compute cluster
# Perform bootstrap of calc_var for Windows
clusterSetRNGStream(clus_ter, 1121)
boot_data <- parLapply(clus_ter, rep(l_gd, n_boot),
  fun=calc_var, def_thresh=def_thresh,
  rho_sqrt=rho_sqrt, rho_sqrtm=rho_sqrtm,
  n_simu=n_simu, level_s=level_s)  # end parLapply
# Bootstrap under Mac-OSX or Linux
boot_data <- mclapply(rep(l_gd, n_boot),
  FUN=calc_var, def_thresh=def_thresh,
  rho_sqrt=rho_sqrt, rho_sqrtm=rho_sqrtm,
  n_simu=n_simu, level_s=level_s)  # end mclapply
boot_data <- rutils::do_call(rbind, boot_data)
stopCluster(clus_ter)  # Stop R processes over cluster
# Calculate vectors of standard errors of VaR and CVaR from boot_data data
std_error_var <- apply(boot_data[, 1:7], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
std_error_cvar <- apply(boot_data[, 8:14], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
# Scale the standard errors of VaRs and CVaRs
std_error_var[2, ] <- std_error_var[2, ]/std_error_var[1, ]
std_error_cvar[2, ] <- std_error_cvar[2, ]/std_error_cvar[1, ]

# Plot the standard errors of VaRs and CVaRs
plot(x=colnames(std_error_cvar),
  y=std_error_cvar[2, ], t="l", col="red", lwd=2,
  ylim=range(c(std_error_var[2, ], std_error_cvar[2, ])),
  xlab="level_s", ylab="Standard errors",
  main="Scaled standard errors of CVaR and VaR")
lines(x=colnames(std_error_var), y=std_error_var[2, ], lwd=2)
legend(x="topleft", legend=c("CVaRs", "VaRs"), bty="n",
 title=NULL, inset=0.05, cex=0.8, bg="white",
 lwd=6, lty=1, col=c("red", "black"))

calc_var <- function(def_probs, # Default probabilities
               l_gd=0.6, # loss given default
               rho_sqrt, rho_sqrtm, # asset correlation
               n_simu=1000, # number of simulations
               level_s=seq(0.93, 0.99, 0.01) # Confidence levels
               ) {
  # Calculate random default thresholds
  def_thresh <- qnorm(runif(1, min=0.5, max=1.5)*def_probs)
  # Simulate losses under Vasicek model
  n_assets <- NROW(def_probs)
  system_atic <- rnorm(n_simu)
  asset_s <- matrix(rnorm(n_simu*n_assets), ncol=n_simu)
  asset_s <- t(rho_sqrt*system_atic + t(rho_sqrtm*asset_s))
  loss_es <- l_gd*colSums(asset_s < def_thresh)/n_assets
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
clus_ter <- makeCluster(n_cores)  # Initialize compute cluster
# Perform bootstrap of calc_var for Windows
clusterSetRNGStream(clus_ter, 1121)
boot_data <- parLapply(clus_ter, rep(l_gd, n_boot),
  fun=calc_var, def_probs=def_probs,
  rho_sqrt=rho_sqrt, rho_sqrtm=rho_sqrtm,
  n_simu=n_simu, level_s=level_s)  # end parLapply
# Bootstrap under Mac-OSX or Linux
boot_data <- mclapply(rep(l_gd, n_boot),
  FUN=calc_var, def_probs=def_probs,
  rho_sqrt=rho_sqrt, rho_sqrtm=rho_sqrtm,
  n_simu=n_simu, level_s=level_s)  # end mclapply
boot_data <- rutils::do_call(rbind, boot_data)
stopCluster(clus_ter)  # Stop R processes over cluster
# Calculate vectors of standard errors of VaR and CVaR from boot_data data
std_error_var <- apply(boot_data[, 1:7], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
std_error_cvar <- apply(boot_data[, 8:14], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))

# Plot the standard errors of VaRs and CVaRs
plot(x=colnames(std_error_cvar),
  y=std_error_cvar[2, ], t="l", col="red", lwd=2,
  ylim=range(c(std_error_var[2, ], std_error_cvar[2, ])),
  xlab="Confidence Levels", ylab="Standard errors",
  main="Standard Errors of CVaR and VaR
  with Uncertain Default Probabilities")
lines(x=colnames(std_error_var), y=std_error_var[2, ], lwd=2)
legend(x="topleft", legend=c("CVaRs", "VaRs"), bty="n",
 title=NULL, inset=0.05, cex=1.0, bg="white",
 lwd=6, lty=1, col=c("red", "black"))

# Define cumulative default probability function
cum_loss <- function(x, def_thresh=(-2), rh_o=0.2, l_gd=0.4)
  pnorm((sqrt(1-rh_o)*qnorm(x/l_gd) - def_thresh)/sqrt(rh_o))
# Define Vasicek loss distribution function
# (vectorized version with error handling for x)
loss_distr <- function(x, def_thresh=-2, rh_o=0.1, l_gd=0.4) {
  q_norm <- ifelse(x/l_gd < 0.999, qnorm(x/l_gd), 3.1)
  sqrt((1-rh_o)/rh_o)*exp(-(sqrt(1-rh_o)*q_norm - def_thresh)^2/(2*rh_o) + q_norm^2/2)/l_gd
}  # end loss_distr

def_prob <- 0.2; def_thresh <- qnorm(def_prob)
rh_o <- 0.1; l_gd <- 0.4
at_tach <- 0.15; de_tach <- 0.2
# Expected tranche loss is sum of two terms
tranche_loss <-
  # Loss between at_tach and de_tach
  integrate(function(x, at_tach) (x-at_tach)*loss_distr(x,
def_thresh=def_thresh, rh_o=rh_o, l_gd=l_gd),
low=at_tach, up=de_tach, at_tach=at_tach)$value / (de_tach-at_tach) +
  # Loss in excess of de_tach
  (1-cum_loss(x=de_tach, def_thresh=def_thresh, rh_o=rh_o, l_gd=l_gd))
# Plot probability distribution of losses
curve(expr=loss_distr(x, def_thresh=def_thresh, rh_o=rh_o),
type="l", xlim=c(0, 3*l_gd*def_prob),
xlab="loss percentage", ylab="density", lwd=3,
col="orange", main="CDO Tranche Losses")
# Add line for expected loss
abline(v=l_gd*def_prob, col="red", lwd=3)
text(x=l_gd*def_prob-0.001, y=4, labels="expected loss",
 lwd=2, srt=90, pos=3)
# Add lines for attach and detach
abline(v=at_tach, col="blue", lwd=3)
text(x=at_tach-0.001, y=4, labels="attach",
 lwd=2, srt=90, pos=3)
abline(v=de_tach, col="green", lwd=3)
text(x=de_tach-0.001, y=4, labels="detach",
 lwd=2, srt=90, pos=3)
# Add shading for CDO tranche
var_s <- seq(at_tach, de_tach, length=100)
densi_ty <- sapply(var_s, loss_distr,
  def_thresh=def_thresh, rh_o=rh_o)
# Draw shaded polygon
polygon(c(at_tach, var_s, de_tach), density=20,
  c(-1, densi_ty, -1), col="red", border=NA)
text(x=0.5*(at_tach+de_tach), y=0, labels="CDO tranche", cex=0.9, lwd=2, pos=3)

set.seed(1121)  # Reset random number generator
# Sample from Standard Normal Distribution
len_gth <- 1000
da_ta <- rnorm(len_gth)
# Estimate the 95% quantile
boot_data <- sapply(1:10000, function(x) {
  boot_sample <- da_ta[sample.int(len_gth,
                              replace=TRUE)]
  quantile(boot_sample, 0.95)
})  # end sapply
sd(boot_data)
# Estimate the 95% quantile using antithetic sampling
boot_data <- sapply(1:10000, function(x) {
  boot_sample <- da_ta[sample.int(len_gth,
                              replace=TRUE)]
  quantile(c(boot_sample, -boot_sample), 0.95)
})  # end sapply
# Standard error of quantile from bootstrap
sd(boot_data)
sqrt(2)*sd(boot_data)

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

set.seed(1121) # Reset random number generator
# Sample from Standard Normal Distribution
len_gth <- 1000
da_ta <- rnorm(len_gth)
# Cumulative probability from Naive Monte Carlo
quan_tile <- 2
pnorm(-quan_tile)
integrate(dnorm, low=quan_tile, up=Inf)
sum(da_ta > quan_tile)/len_gth
# Generate importance sample
lamb_da <- 1.5
data_is <- da_ta + lamb_da
# Cumulative probability from importance sample
sum(data_is > quan_tile)/len_gth
weight_s <- exp(-lamb_da*data_is + lamb_da^2/2)
sum((data_is > quan_tile)*weight_s)/len_gth
# Bootstrap of standard errors of cumulative probability
boot_data <- sapply(1:1000, function(x) {
  da_ta <- rnorm(len_gth)
  na_ive <- sum(da_ta > quan_tile)/len_gth
  da_ta <- (da_ta + lamb_da)
  weight_s <- exp(-lamb_da*da_ta + lamb_da^2/2)
  im_port <- sum((da_ta > quan_tile)*weight_s)/len_gth
  c(naive_mc=na_ive, importance=im_port)
}) # end sapply
apply(boot_data, MARGIN=1,
function(x) c(mean=mean(x), sd=sd(x)))

# Quantile from Naive Monte Carlo
conf_level <- 0.98
qnorm(conf_level)
da_ta <- sort(da_ta)
quantile(da_ta, probs=conf_level)
da_ta[conf_level*len_gth]
# Quantile from importance sample
wei_ght <- exp(-lamb_da*conf_level + lamb_da^2/2)
conf_import <- wei_ght*conf_level
quant_import <-
  da_ta[conf_import*len_gth] + lamb_da
# Bootstrap of standard errors of quantile
boot_data <- sapply(1:1000, function(x) {
  da_ta <- sort(rnorm(len_gth))
  na_ive <- da_ta[conf_level*len_gth]
  im_port <- da_ta[conf_level*exp(-lamb_da*conf_level + lamb_da^2/2)*len_gth] + lamb_da
  c(naive_mc=na_ive, importance=im_port)
}) # end sapply
apply(boot_data, MARGIN=1,
function(x) c(mean=mean(x), sd=sd(x)))

# Quantile from importance sample
quant_import <-
  da_ta[conf_import*len_gth] + lamb_da
# Expected value from Naive Monte Carlo
integrate(function(x) x*dnorm(x), low=quant_import, up=Inf)
sum((da_ta > quant_import)*da_ta)/len_gth
# Expected value from importance sample
data_is <- da_ta + lamb_da
weight_s <- exp(-lamb_da*data_is + lamb_da^2/2)
sum((data_is > quant_import)*data_is*weight_s)/len_gth
# Bootstrap of standard errors of expected value
boot_data <- sapply(1:1000, function(x) {
  da_ta <- rnorm(len_gth)
  na_ive <- sum((da_ta > quant_import)*da_ta)/len_gth
  da_ta <- (da_ta + lamb_da)
  weight_s <- exp(-lamb_da*da_ta + lamb_da^2/2)
  im_port <- sum((da_ta > quant_import)*da_ta*weight_s)/len_gth
  c(naive_mc=na_ive, importance=im_port)
}) # end sapply
apply(boot_data, MARGIN=1,
function(x) c(mean=mean(x), sd=sd(x)))

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

# Symbols for constant maturity Treasury rates
sym_bols <- c("DGS1", "DGS2", "DGS5", "DGS10", "DGS20", "DGS30")
library(quantmod)  # Load package quantmod
rates_env <- new.env()  # new environment for data
# Download data for sym_bols into rates_env
getSymbols(sym_bols, env=rates_env, src="FRED")
ls(rates_env)  # List files in rates_env
# Get class of object in rates_env
class(get(x=sym_bols[1], envir=rates_env))
# Another way
class(rates_env$DGS20)
colnames(rates_env$DGS20)
save(rates_env, file="C:/Develop/lecture_slides/data/rates_data.RData")

x11(width=6, height=4)
par(mar=c(2, 2, 0, 0), oma=c(0, 0, 0, 0))
head(rates_env$DGS20, 3)
# Get class of all objects in rates_env
sapply(rates_env, class)
# Get class of all objects in R workspace
sapply(ls(), function(ob_ject) class(get(ob_ject)))
# Plot 20-year constant maturity Treasury rate
chart_Series(rates_env$DGS20["1990/"],
  name="20-year constant maturity Treasury rate")

par(mar=c(3, 3, 2, 0), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
# Load constant maturity Treasury rates
load(file="C:/Develop/lecture_slides/data/rates_data.RData")
# Get end-of-year dates since 2006
date_s <- xts::endpoints(rates_env$DGS1["2006/"], on="years")
date_s <- zoo::index(rates_env$DGS1["2006/"])[date_s]
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
legend("bottomright", legend=colnames(rate_s),
 col=col_ors, lty=1, lwd=4, inset=0.05, cex=0.8)

x11(width=6, height=4)
par(mar=c(0, 0, 0, 0), oma=c(0, 0, 0, 0), mgp=c(0, 0, 0))
# Load constant maturity Treasury rates
load(file="C:/Develop/lecture_slides/data/rates_data.RData")
# Symbols for constant maturity Treasury rates
sym_bols <- c("DGS1", "DGS2", "DGS5", "DGS20")
# Calculate daily rates changes
rate_s <- xts:::na.locf.xts(rutils::do_call(cbind,
    as.list(rates_env)[sym_bols]))
rate_s <- xts:::na.locf.xts(rate_s)
rate_s <- xts:::na.locf.xts(rate_s, fromLast=TRUE)
re_turns <- rutils::diff_it(rate_s)
date_s <- index(re_turns)
# De-mean (center) and scale the returns
re_turns <- t(t(re_turns) - colMeans(re_turns))
re_turns <- t(t(re_turns) / sqrt(colSums(re_turns^2)/(NROW(re_turns)-1)))
re_turns <- xts(re_turns, date_s)
# Correlation matrix of Treasury rates
cor_mat <- cor(re_turns)
# reorder correlation matrix based on clusters
library(corrplot)
or_der <- corrMatOrder(cor_mat, order="hclust",
  hclust.method="complete")
cor_mat <- cor_mat[or_der, or_der]

# Plot the correlation matrix
col_ors <- colorRampPalette(c("red", "white", "blue"))
corrplot(cor_mat, title=NA, tl.col="black",
  tl.cex=0.8, mar=c(0,0,0,0), method="square",
  col=col_ors(8), cl.offset=0.75, cl.cex=0.7,
  cl.align.text="l", cl.ratio=0.25)
title("Correlation of Treasury rates", line=-1)
# Draw rectangles on the correlation matrix plot
corrRect.hclust(cor_mat, k=NROW(cor_mat) %/% 2,
  method="complete", col="red")

# Create initial vector of portfolio weights
n_weights <- NROW(sym_bols)
weight_s <- rep(1/sqrt(n_weights), n_weights)
names(weight_s) <- sym_bols
# objective function equal to minus portfolio variance
object_ive <- function(weight_s, re_turns) {
  portf_rets <- re_turns %*% weight_s
  -sum(portf_rets*portf_rets) +
    1e7*(1 - sum(weight_s*weight_s))^2
}  # end object_ive
# objective for equal weight portfolio
object_ive(weight_s, re_turns)
# Compare speed of vector multiplication methods
summary(microbenchmark(
  trans_pose=t(re_turns) %*% re_turns,
  s_um=sum(re_turns*re_turns),
  times=10))[, c(1, 4, 5)]

# find weights with maximum variance
optim_run <- optim(par=weight_s,
  fn=object_ive,
  re_turns=re_turns,
  method="L-BFGS-B",
  upper=rep(1.0, n_weights),
  lower=rep(-1.0, n_weights))
# optimal weights and maximum variance
weight_s <- optim_run$par
-object_ive(weight_s, re_turns)
# Plot first principal component loadings
barplot(weight_s, names.arg=names(weight_s),
  xlab="", ylab="",
  main="first principal component loadings")

# pc1 weights and returns
weights_1 <- weight_s
pc_1 <- re_turns %*% weights_1
# redefine objective function
object_ive <- function(weight_s, re_turns) {
  portf_rets <- re_turns %*% weight_s
  -sum(portf_rets*portf_rets) +
    1e7*(1 - sum(weight_s*weight_s))^2 +
    1e7*sum(pc_1*portf_rets)^2
}  # end object_ive
# find second principal component weights
optim_run <- optim(par=weight_s,
             fn=object_ive,
             re_turns=re_turns,
             method="L-BFGS-B",
             upper=rep(1.0, n_weights),
             lower=rep(-1.0, n_weights))

# pc2 weights and returns
weights_2 <- optim_run$par
pc_2 <- re_turns %*% weights_2
sum(pc_1*pc_2)
# Plot second principal component loadings
barplot(weights_2, names.arg=names(weights_2),
  xlab="", ylab="",
  main="second principal component loadings")

# Covariance matrix and variance vector of returns
cov_mat <- cov(re_turns)
vari_ance <- diag(cov_mat)
cor_mat <- cor(re_turns)
# Calculate eigenvectors and eigenvalues
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
cov_mat <- cov(re_turns)
ei_gen <- eigen(cov_mat)
# Perform PCA without scaling
pc_a <- prcomp(re_turns, scale=FALSE)
# Compare outputs
all.equal(ei_gen$values, pc_a$sdev^2)
all.equal(abs(ei_gen$vectors), abs(pc_a$rotation),
    check.attributes=FALSE)
# eigen decomposition of correlation matrix
cor_mat <- cor(re_turns)
ei_gen <- eigen(cor_mat)
# Perform PCA with scaling
pc_a <- prcomp(re_turns, scale=TRUE)
# Compare outputs
all.equal(ei_gen$values, pc_a$sdev^2)
all.equal(abs(ei_gen$vectors), abs(pc_a$rotation),
    check.attributes=FALSE)

# Perform principal component analysis PCA
pc_a <- prcomp(re_turns, scale=TRUE)
# Plot standard deviations
barplot(pc_a$sdev,
  names.arg=colnames(pc_a$rotation),
  las=3, xlab="", ylab="",
  main="Scree Plot: Volatilities of Principal Components
  of Treasury rates")

x11(width=6, height=7)
# Calculate principal component loadings (weights)
pc_a$rotation
# Plot loading barplots in multiple panels
par(mfrow=c(3,2))
par(mar=c(2, 2, 2, 1), oma=c(0, 0, 0, 0))
for (or_der in 1:NCOL(pc_a$rotation)) {
  barplot(pc_a$rotation[, or_der],
  las=3, xlab="", ylab="", main="")
  title(paste0("PC", or_der), line=-2.0,
  col.main="red")
}  # end for

# Calculate products of principal component time series
round(t(pc_a$x) %*% pc_a$x, 2)
# Calculate principal component time series
pca_ts <- xts(re_turns %*% pc_a$rotation,
          order.by=index(re_turns))
pca_ts <- cumsum(pca_ts)
# Plot principal component time series in multiple panels
par(mfrow=c(3,2))
par(mar=c(2, 2, 0, 1), oma=c(0, 0, 0, 0))
ra_nge <- range(pca_ts)
for (or_der in 1:NCOL(pca_ts)) {
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
