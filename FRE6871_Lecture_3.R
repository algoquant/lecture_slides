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

# R startup chunk
# ```{r setup, include=FALSE}
library(shiny)
library(quantmod)
inter_val <- 31
cl_ose <- quantmod::Cl(rutils::etf_env$VTI)
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue")
# ```
#end R startup chunk
inputPanel(
  sliderInput("lamb_da", label="lambda:",
    min=0.01, max=0.2, value=0.1, step=0.01)
)  # end inputPanel

renderPlot({
  # Calculate EWMA prices
  lamb_da <- input$lamb_da
  weight_s <- exp(-lamb_da*1:inter_val)
  weight_s <- weight_s/sum(weight_s)
  ew_ma <- filter(cl_ose, filter=weight_s, sides=1)
  ew_ma[1:(inter_val-1)] <- ew_ma[inter_val]
  ew_ma <- xts(cbind(cl_ose, ew_ma), order.by=index(cl_ose))
  colnames(ew_ma) <- c("VTI", "VTI EWMA")
  # Plot EWMA prices
  ch_ob <- chart_Series(ew_ma, theme=plot_theme, name="EWMA prices")
  plot(ch_ob)
  legend("top", legend=colnames(ew_ma),
   inset=0.1, bg="white", lty=1, lwd=2,
   col=plot_theme$col$line.col, bty="n")
})  # end renderPlot

# Calculate random default probabilities
n_assets <- 100
def_probs <- runif(n_assets, max=0.2)
mean(def_probs)
# Calculate number of defaults
uni_form <- runif(n_assets)
sum(uni_form < def_probs)
# Simulate average number of defaults
n_simu <- 1000
de_faults <- numeric(n_simu)
# Simulate using for() loop (inefficient way)
for (i in 1:n_simu) {  # Perform loop
  uni_form <- runif(n_assets)
  de_faults[i] <- sum(uni_form < def_probs)
}  # end for
# Calculate average number of defaults
mean(de_faults)
# Simulate using vectorized functions  (efficient way)
uni_form <- matrix(runif(n_simu*n_assets), ncol=n_simu)
sum(uni_form < def_probs)/n_simu

# Plot Standard Normal distribution
curve(expr=dnorm(x),
type="l", xlim=c(-4, 4),
xlab="asset value", ylab="", lwd=2,
col="blue", main="Distribution of Asset Values")
abline(v=qnorm(0.025), col="red", lwd=2)
text(x=qnorm(0.025)-0.1, y=0.15,
 labels="default threshold",
 lwd=2, srt=90, pos=3)

# Define correlation parameters
rh_o <- 0.2
rho_sqrt <- sqrt(rh_o) ; rho_sqrtm <- sqrt(1-rh_o)
n_assets <- 5 ; n_simu <- 10000
# Calculate vector of systematic factors
system_atic <- rnorm(n_simu)
# Simulate asset values using vectorized functions (efficient way)
asset_s <- rho_sqrt*system_atic + rho_sqrtm*rnorm(n_simu*n_assets)
dim(asset_s) <- c(n_simu, n_assets)
# Calculate correlations between asset values
cor(asset_s)
# Simulate asset values using for() loop (inefficient way)
# allocate matrix of assets
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
integrate(function(x) x*dnorm(x),
    low=2, up=Inf)

rh_o <- 0.1; l_gd <- 0.4
# Define Vasicek loss distribution function
loss_distr <- function(x, def_thresh=(-2), rh_o=0.2, l_gd=0.4)
  sqrt((1-rh_o)/rh_o)*exp(-(sqrt(1-rh_o)*qnorm(x/l_gd) - def_thresh)^2/(2*rh_o) + qnorm(x/l_gd)^2/2)/l_gd
integrate(loss_distr, low=0, up=l_gd,
  def_thresh=(-2), rh_o=rh_o, l_gd=l_gd)

# Plot probability distribution of losses
def_prob <- 0.05; def_thresh <- qnorm(def_prob)
curve(expr=loss_distr(x, def_thresh=def_thresh, rh_o=rh_o),
type="l", xlim=c(0, 0.06),
xlab="loss percentage", ylab="density", lwd=3,
col="orange", main="Distribution of Losses")
# Add line for expected loss
abline(v=l_gd*def_prob, col="red", lwd=3)
text(x=l_gd*def_prob-0.001, y=10, labels="expected loss",
 lwd=2, srt=90, pos=3)

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
arrows(x0=0.02, y0=35, x1=0.04, y1=35,
 code=3, lwd=3, cex=0.5)
text(x=0.03, y=36, labels="unexpected loss",
     lwd=2, pos=3)
# Add lines for VaR
abline(v=0.055, col="red", lwd=3)
arrows(x0=0.0, y0=25, x1=0.055, y1=25,
 code=3, lwd=3, cex=0.5)
text(x=0.03, y=26, labels="VaR", lwd=2, pos=3)
text(x=0.055-0.001, y=10, labels="VaR",
 lwd=2, srt=90, pos=3)

va_r <- 0.04; var_max <- 4*l_gd*def_prob
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
var_s <- seq(va_r, var_max, length=100)
densi_ty <- sapply(var_s, loss_distr,
  def_thresh=def_thresh, rh_o=rh_o)
# Draw shaded polygon
polygon(c(va_r, var_s, var_max), density=20,
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
     xlab="Confidence Levels", ylab="VaRs",
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
     xlab="Confidence Levels", ylab="CVaRs",
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
# Calculate vector of systematic factors
system_atic <- rnorm(n_simu)
# Simulate losses under Vasicek model
asset_s <- matrix(rnorm(n_simu*n_assets), ncol=n_simu)
asset_s <- t(rho_sqrt*system_atic + t(rho_sqrtm*asset_s))
loss_es <- l_gd*colSums(asset_s < def_thresh)/n_assets

# Calculate VaRs
level_s <- seq(0.93, 0.99, 0.01)
var_s <- quantile(loss_es, probs=level_s)
plot(x=level_s, y=var_s, t="l", lwd=2,
     xlab="Confidence Levels", ylab="VaRs",
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
     xlab="Confidence Levels", ylab="CVaRs",
     main="Simulated CVaR and Confidence Levels")

# Add VaRs
lines(x=level_s, y=cvar_s[, "var_s"], lwd=2)
# Add legend
legend(x="topleft", legend=c("CVaRs", "VaRs"), bty="n",
 title=NULL, inset=0.05, cex=0.8, bg="white",
 lwd=6, lty=1, col=c("red", "black"))
