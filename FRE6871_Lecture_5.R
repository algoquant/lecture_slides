str(optimize)
# Objective function with multiple minima
object_ive <- function(in_put, param1=0.01) {
  sin(0.25*pi*in_put) + param1*(in_put-1)^2
}  # end object_ive
unlist(optimize(f=object_ive, interval=c(-4, 2)))
unlist(optimize(f=object_ive, interval=c(0, 8)))
options(width=80, dev='pdf')

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
rgl::persp3d(x=sur_face, xlim=c(-5, 5), ylim=c(-5, 5),
  col="green", axes=FALSE)
# Draw 3d surface plot of matrix
x_lim <- seq(from=-5, to=5, by=0.1)
y_lim <- seq(from=-5, to=5, by=0.1)
rgl::persp3d(z=outer(x_lim, y_lim, FUN=sur_face),
  xlab="x", ylab="y", zlab="sur_face", col="green")
# Save current view to png file
rgl.snapshot("surface_plot.png")
# Define function of two variables and two parameters
sur_face <- function(x, y, par_1=1, par_2=1)
  sin(par_1*x)*sin(par_2*y)
# Draw 3d surface plot of function
rgl::persp3d(x=sur_face, xlim=c(-5, 5), ylim=c(-5, 5),
  col="green", axes=FALSE, par_1=1, par_2=2)

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
da_ta <- rnorm(1000, mean=4, sd=2)
# Objective function is log-likelihood
object_ive <- function(pa_r, da_ta) {
  sum(2*log(pa_r[2]) +
    ((da_ta - pa_r[1])/pa_r[2])^2)
}  # end object_ive

# Grid search
objective_grid <- sapply(par_mean, function(m) {
  sapply(par_sd, function(sd) {
    object_ive(c(m, sd), da_ta)
  })
})


# Vectorize objective function
vec_objective <- Vectorize(
  FUN=function(mean, sd, da_ta)
    object_ive(c(mean, sd), da_ta),
  vectorize.args=c("mean", "sd")
)  # end Vectorize
# Objective function on parameter grid
par_mean <- seq(1, 6, length=50)
par_sd <- seq(0.5, 3.0, length=50)
objective_grid <- outer(par_mean, par_sd,
vec_objective, da_ta=da_ta)
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
rgl::persp3d(z=-objective_grid, zlab="objective",
  col="green", main="objective function")

# Initial parameters
par_init <- c(mean=0, sd=1)
# Perform optimization using optim()
optim_fit <- optim(par=par_init,
  fn=object_ive, # log-likelihood function
  da_ta=da_ta,
  method="L-BFGS-B", # quasi-Newton method
  upper=c(10, 10), # upper constraint
  lower=c(-10, 0.1)) # lower constraint
# Optimal parameters
optim_fit$par
# Perform optimization using MASS::fitdistr()
optim_fit <- MASS::fitdistr(da_ta, densfun="normal")
optim_fit$estimate
optim_fit$sd
# Plot histogram
histo_gram <- hist(da_ta, plot=FALSE)
plot(histo_gram, freq=FALSE,
     main="histogram of sample")
curve(expr=dnorm(x, mean=optim_fit$par["mean"],
           sd=optim_fit$par["sd"]),
add=TRUE, type="l", lwd=2, col="red")
legend("topright", inset=0.0, cex=0.8, title=NULL,
 leg="optimal parameters",
 lwd=2, bg="white", col="red")

# Sample from mixture of normal distributions
da_ta <- c(rnorm(100, sd=1.0),
      rnorm(100, mean=4, sd=1.0))
# Objective function is log-likelihood
object_ive <- function(pa_r, da_ta) {
  likelihood <- pa_r[1]/pa_r[3] *
  dnorm((da_ta-pa_r[2])/pa_r[3]) +
  (1-pa_r[1])/pa_r[5]*dnorm((da_ta-pa_r[4])/pa_r[5])
  if (any(likelihood <= 0)) Inf else
    -sum(log(likelihood))
}  # end object_ive
# Vectorize objective function
vec_objective <- Vectorize(
  FUN=function(mean, sd, w, m1, s1, da_ta)
    object_ive(c(w, m1, s1, mean, sd), da_ta),
  vectorize.args=c("mean", "sd")
)  # end Vectorize
# Objective function on parameter grid
par_mean <- seq(3, 5, length=50)
par_sd <- seq(0.5, 1.5, length=50)
objective_grid <- outer(par_mean, par_sd,
    vec_objective, da_ta=da_ta,
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
      da_ta=da_ta,
      method="L-BFGS-B",
      upper=c(1,10,10,10,10),
      lower=c(0,-10,0.2,-10,0.2))
optim_fit$par

# Plot histogram
histo_gram <- hist(da_ta, plot=FALSE)
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

library(quantmod)  # Load package quantmod
# Symbols for constant maturity Treasury rates
sym_bols <- c("DGS1", "DGS2", "DGS5", "DGS10", "DGS20", "DGS30")
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

# Load constant maturity Treasury rates
load(file="C:/Develop/lecture_slides/data/rates_data.RData")
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
load(file="C:/Develop/lecture_slides/data/rates_data.RData")
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
# Standardize (de-mean and scale) the returns
re_turns <- lapply(re_turns, function(x) {(x - mean(x))/sd(x)})
re_turns <- rutils::do_call(cbind, re_turns)
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
object_ive <- function(weight_s, re_turns) {
  portf_rets <- re_turns %*% weight_s
  -sum(portf_rets^2) + 1e7*(1 - sum(weight_s*weight_s))^2
}  # end object_ive
# Objective for equal weight portfolio
object_ive(weight_s, re_turns)
# Compare speed of vector multiplication methods
summary(microbenchmark(
  trans_pose=t(re_turns) %*% re_turns,
  s_um=sum(re_turns*re_turns),
  times=10))[, c(1, 4, 5)]

# Find weights with maximum variance
op_tim <- optim(par=weight_s,
  fn=object_ive,
  re_turns=re_turns,
  method="L-BFGS-B",
  upper=rep(1.0, n_weights),
  lower=rep(-1.0, n_weights))
# Optimal weights and maximum variance
weights_1 <- op_tim$par
-object_ive(weights_1, re_turns)
# Plot first principal component loadings
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
barplot(weights_1, names.arg=names(weights_1),
  xlab="", ylab="", main="First Principal Component Loadings")

# pc1 weights and returns
pc_1 <- drop(re_turns %*% weights_1)
# Redefine objective function
object_ive <- function(weight_s, re_turns) {
  portf_rets <- re_turns %*% weight_s
  -sum(portf_rets^2) + 1e7*(1 - sum(weight_s^2))^2 +
    1e7*sum(pc_1*portf_rets)^2
}  # end object_ive
# Find second principal component weights
op_tim <- optim(par=weight_s,
             fn=object_ive,
             re_turns=re_turns,
             method="L-BFGS-B",
             upper=rep(1.0, n_weights),
             lower=rep(-1.0, n_weights))

# pc2 weights and returns
weights_2 <- op_tim$par
pc_2 <- drop(re_turns %*% weights_2)
sum(pc_1*pc_2)
# Plot second principal component loadings
barplot(weights_2, names.arg=names(weights_2),
  xlab="", ylab="", main="Second Principal Component Loadings")

ei_gen <- eigen(cor_mat)
ei_gen$vectors
# Compare with optimization
all.equal(sum(diag(cor_mat)), sum(ei_gen$values))
all.equal(abs(ei_gen$vectors[, 1]), abs(weights_1), check.attributes=FALSE)
all.equal(abs(ei_gen$vectors[, 2]), abs(weights_2), check.attributes=FALSE)
all.equal(ei_gen$values[1], var(pc_1), check.attributes=FALSE)
all.equal(ei_gen$values[2], var(pc_2), check.attributes=FALSE)
# Eigenvalue equations
(cor_mat %*% weights_1) / weights_1 / var(pc_1)
(cor_mat %*% weights_2) / weights_2 / var(pc_2)
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

# Calculate products of principal component time series
round(t(pc_a$x) %*% pc_a$x, 2)
# Calculate principal component time series
pca_ts <- xts(re_turns %*% pc_a$rotation,
          order.by=index(re_turns))
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
