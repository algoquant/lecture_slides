# Calculate random default probabilities
set.seed(1121)
n_assets <- 100
def_probs <- runif(n_assets, max=0.2)
mean(def_probs)
# Simulate number of defaults
uni_form <- runif(n_assets)
sum(uni_form < def_probs)
# Simulate average number of defaults using for() loop (inefficient way)
n_simu <- 1000
set.seed(1121)
de_faults <- numeric(n_simu)
for (i in 1:n_simu) {  # Perform loop
  uni_form <- runif(n_assets)
  de_faults[i] <- sum(uni_form < def_probs)
}  # end for
# Calculate average number of defaults
mean(de_faults)
# Simulate using vectorized functions  (efficient way)
set.seed(1121)
uni_form <- matrix(runif(n_simu*n_assets), ncol=n_simu)
de_faults <- colSums(uni_form < def_probs)
mean(de_faults)
# Plot the distribution of defaults
x11(width=6, height=5)
plot(density(de_faults), main="Distribution of Defaults",
     xlab="number of defaults", ylab="frequqncy")
abline(v=mean(de_faults), lwd=3, col="red")
# Calculate default thresholds and asset values
def_thresh <- qnorm(def_probs)
asset_s <- qnorm(uni_form)
# Simulate defaults
de_faults <- colSums(asset_s < def_thresh)
mean(de_faults)
# Plot Standard Normal distribution
x11(width=6, height=5)
x_lim <- 4; def_thresh <- qnorm(0.025)
curve(expr=dnorm(x), type="l", xlim=c(-x_lim, x_lim),
xlab="asset value", ylab="", lwd=3,
col="blue", main="Distribution of Asset Values")
abline(v=def_thresh, col="red", lwd=3)
text(x=def_thresh-0.1, y=0.15, labels="default threshold",
 lwd=2, srt=90, pos=3)
# Plot polygon area
x_var <- seq(-x_lim, x_lim, length=100)
y_var <- dnorm(x_var)
are_a <- ((x_var >= (-x_lim)) & (x_var <= def_thresh))
polygon(c(x_lim, x_var[are_a], def_thresh),
  c(-1, y_var[are_a], -1), col="red")
# Define correlation parameters
rh_o <- 0.2
rho_sqrt <- sqrt(rh_o) ; rho_sqrtm <- sqrt(1-rh_o)
n_assets <- 5 ; n_simu <- 10000
# Calculate vector of systematic and idiosyncratic factors
system_atic <- rnorm(n_simu)
idio_syncratic <- rnorm(n_simu*n_assets)
# Simulate asset values using vectorized functions (efficient way)
asset_s <- rho_sqrt*system_atic + rho_sqrtm*idio_syncratic
dim(asset_s) <- c(n_simu, n_assets)
# Asset values are standard normally distributed
apply(asset_s, MARGIN=2, function(x) c(mean=mean(x), sd=sd(x)))
# Calculate correlations between asset values
cor(asset_s)
# Simulate asset values using for() loop (inefficient way)
# Allocate matrix of assets
asset_s <- matrix(nr=n_simu, nc=n_assets)
# Simulate asset values using for() loop
for (i in 1:n_simu) {  # Perform loop
  asset_s[i, ] <- rho_sqrt*system_atic[i] + rho_sqrtm*rnorm(n_assets)
}  # end for
cor(asset_s)
# benchmark the speed of the two methods
library(microbenchmark)
summary(microbenchmark(
  for_loop={for (i in 1:n_simu) {
    rho_sqrt*system_atic[i] + rho_sqrtm*rnorm(n_assets)}},
  vector_ized={rho_sqrt*system_atic + rho_sqrtm*rnorm(n_simu*n_assets)},
  times=10))[, c(1, 4, 5)]
# Calculate random default probabilities
n_assets <- 5
def_probs <- runif(n_assets, max=0.2)
mean(def_probs)
# Calculate default thresholds
def_thresh <- qnorm(def_probs)
# Calculate number of defaults using vectorized functions (efficient way)
# Calculate vector of number of defaults
rowMeans(t(asset_s) < def_thresh)
def_probs
# Calculate number of defaults using for() loop (inefficient way)
# allocate matrix of de_faults
de_faults <- matrix(nr=n_simu, nc=n_assets)
# Simulate asset values using for() loop
for (i in 1:n_simu) {  # Perform loop
  de_faults[i, ] <- (asset_s[i, ] < def_thresh)
}  # end for
colSums(de_faults) / n_simu
def_probs
# Calculate correlations between defaults
cor(de_faults)
# Define default probabilities
n_assets <- 2
def_prob <- 0.2
def_thresh <- qnorm(def_prob)
# Define correlation parameters
rh_o <- 0.2
rho_sqrt <- sqrt(rh_o) ; rho_sqrtm <- sqrt(1-rh_o)
# Calculate vector of systematic factors
n_simu <- 1000
system_atic <- rnorm(n_simu)
# Simulate asset values using vectorized functions
asset_s <- rho_sqrt*system_atic + rho_sqrtm*rnorm(n_simu*n_assets)
dim(asset_s) <- c(n_simu, n_assets)
# Calculate number of defaults using vectorized functions
de_faults <- t(t(asset_s) < def_thresh)
# Calculate correlations between defaults
cor(de_faults)
# Calculate average number of defaults and compare to def_prob
colSums(de_faults) / n_simu
def_prob
# Define cumulative default probability function
def_cumdistr <- function(x, def_thresh=(-2), rh_o=0.2)
  pnorm((sqrt(1-rh_o)*qnorm(x) - def_thresh)/sqrt(rh_o))
def_cumdistr(x=0.2, def_thresh=qnorm(def_prob), rh_o=rh_o)
# Plot cumulative default probability function
def_prob <- 0.4; def_thresh <- qnorm(def_prob)
curve(expr=def_cumdistr(x, def_thresh=def_thresh, rh_o=0.05),
xlim=c(0, 0.999), lwd=3,
xlab="percent default", ylab="probability",
col="green", main="Cumulative Default Probabilities")
# Plot default distribution with higher correlation
curve(expr=def_cumdistr(x, def_thresh=def_thresh, rh_o=0.2),
xlim=c(0, 0.999), add=TRUE, lwd=3,
col="blue", main="")
# Add legend
legend(x="topleft",
 legend=c("high correlation", "low correlation"),
 title=NULL, inset=0.05, cex=0.8, bg="white",
 bty="n", lwd=6, lty=1, col=c("blue", "green"))
# Add unconditional default probability
abline(v=def_prob, col="red", lwd=3)
text(x=def_prob, y=0.0,
 labels="default probability",
 lwd=2, srt=90, pos=4)
# Define default probability density function
def_distr <- function(x, def_thresh=(-2), rh_o=0.2)
  sqrt((1-rh_o)/rh_o)*exp(-(sqrt(1-rh_o)*qnorm(x) -
  def_thresh)^2/(2*rh_o) + qnorm(x)^2/2)
# Define parameters
rh_o <- 0.2 ; rho_sqrt <- sqrt(rh_o) ; rho_sqrtm <- sqrt(1-rh_o)
def_prob <- 0.3; def_thresh <- qnorm(def_prob)
def_distr(0.03, def_thresh=def_thresh, rh_o=rh_o)
# Plot probability distribution of defaults
curve(expr=def_distr(x, def_thresh=def_thresh, rh_o=0.1),
xlim=c(0, 1.0), lwd=3,
xlab="percentage of defaults", ylab="density",
col="green", main="Distribution of Defaults")
# Plot default distribution with higher correlation
curve(expr=def_distr(x, def_thresh=def_thresh, rh_o=0.3),
xlab="default percentage", ylab="",
add=TRUE, lwd=3, col="blue", main="")
# Add legend
legend(x="topright",
 legend=c("high correlation", "low correlation"),
 title=NULL, inset=0.05, cex=0.8, bg="white",
 bty="n", lwd=6, lty=1, col=c("blue", "green"))
# Add unconditional default probability
abline(v=def_prob, col="red", lwd=3)
text(x=def_prob, y=2,
 labels="default probability",
 lwd=2, srt=90, pos=2)
# Plot default distribution with low correlation
curve(expr=def_distr(x, def_thresh=def_thresh, rh_o=0.01),
xlab="default percentage", ylab="", lwd=2,
col="green", main="Distribution of Defaults")
# Plot default distribution with high correlation
curve(expr=def_distr(x, def_thresh=def_thresh, rh_o=0.99),
xlab="percentage of defaults", ylab="density",
add=TRUE, lwd=2, n=10001, col="blue", main="")
# Add legend
legend(x="top",
 legend=c("high correlation", "low correlation"),
 title=NULL, inset=0.1, cex=0.8, bg="white",
 bty="n", lwd=6, lty=1, col=c("blue", "green"))
# Add unconditional default probability
abline(v=0.1, col="red", lwd=2)
text(x=0.1, y=10, lwd=2, pos=4,
 labels="default probability")
# Get help for integrate()
?integrate
# Calculate slowly converging integral
func_tion <- function(x) {1/((x+1)*sqrt(x))}
integrate(func_tion, lower=0, upper=10)
integrate(func_tion, lower=0, upper=Inf)
# Integrate function with parameter lamb_da
func_tion <- function(x, lamb_da=1) {
  exp(-x*lamb_da)
}  # end func_tion
integrate(func_tion, lower=0, upper=Inf)
integrate(func_tion, lower=0, upper=Inf, lamb_da=2)
# Cumulative probability over normal distribution
pnorm(-2)
integrate(dnorm, low=2, up=Inf)
str(dnorm)
pnorm(-1)
integrate(dnorm, low=2, up=Inf, mean=1)
# Expected value over normal distribution
integrate(function(x) x*dnorm(x), low=2, up=Inf)
# Vasicek model parameters
rh_o <- 0.1; l_gd <- 0.4
def_prob <- 0.05; def_thresh <- qnorm(def_prob)
# Define Vasicek loss distribution function
loss_distr <- function(x, def_thresh=(-2), rh_o=0.2, l_gd=0.4)
  sqrt((1-rh_o)/rh_o)*exp(-(sqrt(1-rh_o)*qnorm(x/l_gd) - def_thresh)^2/(2*rh_o) + qnorm(x/l_gd)^2/2)/l_gd
integrate(loss_distr, low=0, up=l_gd, def_thresh=(-2), rh_o=rh_o, l_gd=l_gd)
# Plot probability distribution of losses
x11(width=6, height=5)
curve(expr=loss_distr(x, def_thresh=def_thresh, rh_o=rh_o),
type="l", xlim=c(0, 0.06),
xlab="loss percentage", ylab="density", lwd=3,
col="blue", main="Portfolio Loss Density")
# Add line for expected loss
abline(v=l_gd*def_prob, col="red", lwd=3)
text(x=l_gd*def_prob-0.001, y=35, labels="expected loss", lwd=3, pos=4)
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
# Add lines for unexpected loss
abline(v=0.04, col="blue", lwd=3)
arrows(x0=0.02, y0=35, x1=0.04, y1=35, code=3, lwd=3, cex=0.5)
text(x=0.03, y=36, labels="unexpected loss", lwd=2, pos=3)
# Add lines for VaR
abline(v=0.055, col="red", lwd=3)
arrows(x0=0.0, y0=25, x1=0.055, y1=25, code=3, lwd=3, cex=0.5)
text(x=0.03, y=26, labels="VaR", lwd=2, pos=3)
text(x=0.055-0.001, y=10, labels="VaR", lwd=2, srt=90, pos=3)
va_r <- 0.04; min_var <- 4*l_gd*def_prob
# Calculate CVaR
c_var <- integrate(function(x, ...) x*loss_distr(x, ...),
  low=va_r, up=l_gd, def_thresh=def_thresh, rh_o=rh_o, l_gd=l_gd)$value
c_var <- c_var/integrate(loss_distr, low=va_r, up=l_gd, def_thresh=def_thresh, rh_o=rh_o, l_gd=l_gd)$value
# Plot probability distribution of losses
curve(expr=loss_distr(x, def_thresh=def_thresh, rh_o=rh_o),
type="l", xlim=c(0, 0.06),
xlab="loss percentage", ylab="density", lwd=3,
col="orange", main="Conditional Value at Risk")
# Add line for expected loss
abline(v=l_gd*def_prob, col="red", lwd=3)
text(x=l_gd*def_prob-0.001, y=10, labels="expected loss", lwd=2, srt=90, pos=3)
# Add lines for VaR
abline(v=va_r, col="red", lwd=3)
text(x=va_r-0.001, y=10, labels="VaR",
 lwd=2, srt=90, pos=3)
# Add shading for CVaR
var_s <- seq(va_r, min_var, length=100)
densi_ty <- sapply(var_s, loss_distr,
  def_thresh=def_thresh, rh_o=rh_o)
# Draw shaded polygon
polygon(c(va_r, var_s, min_var), density=20,
  c(-1, densi_ty, -1), col="red", border=NA)
text(x=va_r+0.005, y=0, labels="CVaR", lwd=2, pos=3)
# VaR (quantile of the loss distribution)
var_func <- function(x, def_thresh=qnorm(0.1), rh_o=0.1, l_gd=0.4)
  l_gd*pnorm((sqrt(rh_o)*qnorm(x) + def_thresh)/sqrt(1-rh_o))
var_func(x=0.99, def_thresh=def_thresh, rh_o=rh_o, l_gd=l_gd)
# Plot VaR
curve(expr=var_func(x, def_thresh=def_thresh, rh_o=rh_o, l_gd=l_gd),
type="l", xlim=c(0, 0.999), xlab="confidence level", ylab="VaR", lwd=3,
col="orange", main="VaR versus Confidence Level")
# Add line for expected loss
abline(h=l_gd*def_prob, col="red", lwd=3)
text(x=0.2, y=l_gd*def_prob, labels="expected loss", lwd=2, pos=3)
# Integrate loss_distr() over full range
integrate(loss_distr, low=0.0, up=l_gd,
    def_thresh=def_thresh, rh_o=rh_o, l_gd=l_gd)
# Calculate expected losses using loss_distr()
integrate(function(x, ...) x*loss_distr(x, ...),
    low=0.0, up=l_gd,
    def_thresh=def_thresh, rh_o=rh_o, l_gd=l_gd)
# Calculate confidence levels corresponding to VaR values
var_s <- seq(0.07, 0.12, 0.001)
level_s <- sapply(var_s, function(va_r, ...) {
  integrate(loss_distr, low=va_r, up=l_gd, ...)
}, def_thresh=def_thresh, rh_o=rh_o, l_gd=l_gd)  # end sapply
level_s <- cbind(as.numeric(t(level_s)[, 1]), var_s)
colnames(level_s) <- c("level_s", "VaRs")
# Calculate 95% confidence level VaR value
level_s[match(TRUE, level_s[, "level_s"] < 0.05), "VaRs"]
plot(x=1-level_s[, "level_s"],
     y=level_s[, "VaRs"], lwd=2,
     xlab="confidence level", ylab="VaRs",
     t="l", main="VaR Values and Confidence Levels")
# Calculate CVaR values
cvar_s <- sapply(var_s, function(va_r, ...) {
  integrate(function(x, ...) x*loss_distr(x, ...),
      low=va_r, up=l_gd, ...)
}, def_thresh=def_thresh, rh_o=rh_o, l_gd=l_gd)  # end sapply
level_s <- cbind(level_s, as.numeric(t(cvar_s)[, 1]))
colnames(level_s)[3] <- "CVaRs"
# Divide CVaR by confidence level
level_s[, "CVaRs"] <- level_s[, "CVaRs"]/level_s[, "level_s"]
# Calculate 95% confidence level CVaR value
level_s[match(TRUE,
  level_s[, "level_s"] < 0.05), "CVaRs"]
# Plot CVaRs
plot(x=1-level_s[, "level_s"],
     y=level_s[, "CVaRs"],
     t="l", col="red", lwd=2,
     ylim=range(level_s[, c("VaRs", "CVaRs")]),
     xlab="confidence level", ylab="CVaRs",
     main="CVaR Values and Confidence Levels")
# Add VaRs
lines(x=1-level_s[, "level_s"], y=level_s[, "VaRs"], lwd=2)
# Add legend
legend(x="topleft", legend=c("CVaRs", "VaRs"),
 title="default probability = 5%
correlation = 10%
loss given default = 40%",
 inset=0.1, cex=0.8, bg="white", bty="n",
 lwd=6, lty=1, col=c("red", "black"))
# Define model parameters
n_assets <- 300; n_simu <- 1000; l_gd <- 0.4
# Define correlation parameters
rh_o <- 0.2; rho_sqrt <- sqrt(rh_o); rho_sqrtm <- sqrt(1-rh_o)
# Calculate default probabilities and thresholds
set.seed(1121)
def_probs <- runif(n_assets, max=0.2)
def_thresh <- qnorm(def_probs)
# Simulate losses under Vasicek model
system_atic <- rnorm(n_simu)
asset_s <- matrix(rnorm(n_simu*n_assets), ncol=n_simu)
asset_s <- t(rho_sqrt*system_atic + t(rho_sqrtm*asset_s))
loss_es <- l_gd*colSums(asset_s < def_thresh)/n_assets
# Calculate VaR from confidence level
conf_level <- 0.95
va_r <- quantile(loss_es, conf_level)
# Calculate the CVaR as the mean losses in excess of VaR
c_var <- mean(loss_es[loss_es > va_r])
# Plot the density of portfolio losses
x11(width=6, height=5)
densi_ty <- density(loss_es)
plot(densi_ty, xlab="loss percentage", ylab="density",
     lwd=3, col="blue", main="Portfolio Loss Distribution")
# Add vertical line for expected loss
expected_loss <- l_gd*mean(def_probs)
abline(v=expected_loss, col="red", lwd=3)
x_max <- max(densi_ty$x); y_max <- max(densi_ty$y)
text(x=expected_loss, y=(6*y_max/7), labels="expected loss",
     lwd=2, pos=4)
# Add vertical line for VaR
abline(v=va_r, col="red", lwd=3)
text(x=va_r, y=4*y_max/5, labels="VaR", lwd=2, pos=4)
# Draw shaded polygon for CVaR
in_dex <- (densi_ty$x > va_r)
x_var <- c(min(densi_ty$x[in_dex]), densi_ty$x[in_dex], max(densi_ty$x))
polygon(x_var, c(-1, densi_ty$y[in_dex], -1), col="red", border=NA, density=10)
# Add text for CVaR
text(x=5*va_r/4, y=(y_max/7), labels="CVaR", lwd=2, pos=4)
# Add text with data
text(x_max, y_max, labels=paste0(
 "Expected Loss = ", format(100*expected_loss, digits=3), "%", "\n",
 "Loss severity = ", format(100*l_gd, digits=3), "%", "\n",
 "Correlation = ", format(100*rh_o, digits=3), "%", "\n",
 "VaR = ", format(100*va_r, digits=3), "%", "\n",
 "CVaR = ", format(100*c_var, digits=3), "%"),
     adj=c(1, 1), cex=0.8, lwd=2)
# Calculate VaRs from confidence levels
level_s <- seq(0.93, 0.99, 0.01)
var_s <- quantile(loss_es, probs=level_s)
plot(x=level_s, y=var_s, t="l", lwd=2,
     xlab="confidence level", ylab="VaRs",
     main="Simulated VaR and Confidence Levels")
# Calculate CVaRs
cvar_s <- sapply(var_s, function(va_r) {
  mean(loss_es[loss_es >= va_r])
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
     xlab="confidence level", ylab="CVaRs",
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
boot_data <- sapply(rep(l_gd, n_boot), calc_var,
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
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
plot(x=colnames(std_error_cvar), y=std_error_cvar[2, ],
  t="l", col="red", lwd=2,
  ylim=range(c(std_error_var[2, ], std_error_cvar[2, ])),
  xlab="confidence level", ylab="standard error",
  main="Scaled standard errors of CVaR and VaR")
lines(x=colnames(std_error_var), y=std_error_var[2, ], lwd=2)
legend(x="topleft", legend=c("CVaRs", "VaRs"), bty="n",
 title=NULL, inset=0.05, cex=0.8, bg="white",
 lwd=6, lty=1, col=c("red", "black"))
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
std_error_var_scaled <- std_error_var[2, ]/std_error_var[1, ]
std_error_cvar_scaled <- std_error_cvar[2, ]/std_error_cvar[1, ]
# Plot the standard errors of VaRs and CVaRs
x11(width=6, height=5)
plot(x=colnames(std_error_cvar),
  y=std_error_cvar_scaled, t="l", col="red", lwd=2,
  ylim=range(c(std_error_var_scaled, std_error_cvar_scaled)),
  xlab="confidence level", ylab="standard error",
  main="Scaled standard errors of CVaR and VaR")
lines(x=colnames(std_error_var), y=std_error_var_scaled, lwd=2)
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
    mean(loss_es[loss_es >= va_r])
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
std_error_var_param <- apply(boot_data[, 1:7], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
std_error_cvar_param <- apply(boot_data[, 8:14], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
# Plot the standard errors of VaRs under uncertain default probabilities
x11(width=6, height=5)
plot(x=colnames(std_error_var),
  y=std_error_var[2, ], t="l", lwd=3,
  ylim=range(c(std_error_var[2, ], std_error_var_param[2, ])),
  xlab="confidence level", ylab="standard error",
  main="Standard Errors of VaR
  with Uncertain Default Probabilities")
lines(x=colnames(std_error_var), y=std_error_var_param[2, ],
col="red", lwd=3)
legend(x=0.95, y=0.02, bty="n",
 legend=c("VaR Fixed Def Probs", "VaR Random Def Probs"),
 title=NULL, inset=0.05, cex=1.0, bg="white",
 lwd=6, lty=1, col=c("black", "red"))
# Scale the standard errors of VaRs and CVaRs
std_error_var_scaled <- std_error_var_param[2, ]/
  std_error_var_param[1, ]
std_error_cvar_scaled <- std_error_cvar_param[2, ]/
  std_error_cvar_param[1, ]
# Plot the standard errors of VaRs and CVaRs
x11(width=6, height=5)
plot(x=colnames(std_error_cvar_param),
  y=std_error_cvar_scaled, t="l", col="red", lwd=3,
  ylim=range(c(std_error_var_scaled, std_error_cvar_scaled)),
  xlab="confidence level", ylab="standard error",
  main="Relative Standard Errors of VaR and CVaR
  with Uncertain Default Probabilities")
lines(x=names(std_error_var_scaled), y=std_error_var_scaled, lwd=3)
legend(x="topright", legend=c("CVaR", "VaR"), bty="n",
 title=NULL, inset=0.05, cex=1.0, bg="white",
 lwd=6, lty=1, col=c("red", "black"))
NA
App setup code that runs only once at startup.
n_data <- 1e4
std_dev <- 1.0
Define the user interface
inter_face <- shiny::fluidPage(
  # Create numeric input for the number of data points.
  numericInput('n_data', "Number of data points:", value=n_data),
  # Create slider input for the standard deviation parameter.
  sliderInput("std_dev", label="Standard deviation:",
        min=0.1, max=3.0, value=std_dev, step=0.1),
  # Render plot in a panel.
  plotOutput("plo_t", height=300, width=500)
)  # end user interface
Define the server function
ser_ver <- function(input, output) {
  output$plo_t <- shiny::renderPlot({
    # Simulate the data
    da_ta <- rnorm(input$n_data, sd=input$std_dev)
    # Plot the data
    par(mar=c(2, 4, 4, 0), oma=c(0, 0, 0, 0))
    hist(da_ta, xlim=c(-4, 4), main="Histogram of Random Data")
  })  # end renderPlot
}  # end ser_ver
# Return a Shiny app object
shiny::shinyApp(ui=inter_face, server=ser_ver)
Create elements of the user interface
inter_face <- shiny::fluidPage(
  titlePanel("VWAP Moving Average"),
  # Create single row of widgets with two slider inputs
  fluidRow(
    # Input stock symbol
    column(width=3, selectInput("sym_bol", label="Symbol",
                          choices=sym_bols, selected=sym_bol)),
    # Input look-back interval
    column(width=3, sliderInput("look_back", label="Lookback interval",
                          min=1, max=150, value=11, step=1))
  ),  # end fluidRow
  # Create output plot panel
  mainPanel(dygraphs::dygraphOutput("dy_graph"), width=12)
)  # end fluidPage interface
Define the server function
ser_ver <- shiny::shinyServer(function(input, output) {
  # Get the close and volume data in a reactive environment
  clos_e <- shiny::reactive({
    # Get the data
    oh_lc <- get(input$sym_bol, data_env)
    clos_e <- log(quantmod::Cl(oh_lc))
    vol_ume <- quantmod::Vo(oh_lc)
    # Return the data
    cbind(clos_e, vol_ume)
  })  # end reactive code
  # Calculate the VWAP indicator in a reactive environment
  v_wap <- shiny::reactive({
    # Get model parameters from input argument
    look_back <- input$look_back
    # Calculate the VWAP indicator
    clos_e <- clos_e()[, 1]
    vol_ume <- clos_e()[, 2]
    v_wap <- HighFreq::roll_sum(se_ries=clos_e*vol_ume, look_back=look_back)
    volume_rolling <- HighFreq::roll_sum(se_ries=vol_ume, look_back=look_back)
    v_wap <- v_wap/volume_rolling
    v_wap[is.na(v_wap)] <- 0
    # Return the plot data
    da_ta <- cbind(clos_e, v_wap)
    colnames(da_ta) <- c(input$sym_bol, "VWAP")
    da_ta
  })  # end reactive code
  # Return the dygraph plot to output argument
  output$dy_graph <- dygraphs::renderDygraph({
    col_names <- colnames(v_wap())
    dygraphs::dygraph(v_wap(), main=paste(col_names[1], "VWAP")) %>%
dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=2, col="blue") %>%
dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=2, col="red")
  })  # end output plot
})  # end server code
Return a Shiny app object
shiny::shinyApp(ui=inter_face, server=ser_ver)
Define the server function
ser_ver <- shiny::shinyServer(function(input, output) {
  # Create an empty list of reactive values.
  value_s <- reactiveValues()
  # Get input parameters from the user interface.
  n_rows <- reactive({
    # Add n_rows to list of reactive values.
    value_s$n_rows <- input$n_rows
    input$n_rows
  })  # end reactive code
  # Broadcast a message to the console when the button is pressed.
  observeEvent(eventExpr=input$but_ton, handlerExpr={
    cat("Input button pressed\n")
  })  # end observeEvent
  # Send the data when the button is pressed.
  da_ta <- eventReactive(eventExpr=input$but_ton, valueExpr={
    # eventReactive() executes on input$but_ton, but not on n_rows() or input$n_rows.
    cat("Sending", n_rows(), "rows of data\n")
    da_ta <- head(mtcars, input$n_rows)
    value_s$mpg <- mean(da_ta$mpg)
    da_ta
  })  # end eventReactive
  #   da_ta
  # Draw table of the data when the button is pressed.
  observeEvent(eventExpr=input$but_ton, handlerExpr={
    da_ta <- da_ta()
    cat("Received", value_s$n_rows, "rows of data\n")
    cat("Average mpg = ", value_s$mpg, "\n")
    cat("Drawing table\n")
    output$tabl_e <- renderTable(da_ta)
  })  # end observeEvent
})  # end server code
Return a Shiny app object
shiny::shinyApp(ui=inter_face, server=ser_ver)
