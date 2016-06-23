library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
options(digits=3)
thm <- knit_theme$get("acid")
knit_theme$set(thm)
options(width=50, dev='pdf')
str(optimize)
# objective function with multiple minima
object_ive <- function(in_put, param1=0.01) {
  sin(0.25*pi*in_put) + param1*(in_put-1)^2
}  # end object_ive
unlist(optimize(f=object_ive, interval=c(-4, 2)))
unlist(optimize(f=object_ive, interval=c(0, 8)))
options(width=60, dev='pdf')
par(oma=c(1, 1, 1, 1), mgp=c(2, 1, 0), mar=c(5, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# plot objective function
curve(expr=object_ive, type="l", xlim=c(-8, 9),
xlab="", ylab="", lwd=2)
title(main="Objective Function", line=-1)  # add title
# sample of normal variables
sam_ple <- rnorm(1000, mean=4, sd=2)
# objective function is log-likelihood
object_ive <- function(parm, sam_ple) {
  sum(2*log(parm[2]) + 
    ((sam_ple - parm[1])/parm[2])^2)
}  # end object_ive
# vectorize objective function
vec_objective <- Vectorize(
  FUN=function(mean, sd, sam_ple)
    object_ive(c(mean, sd), sam_ple),
  vectorize.args=c("mean", "sd")
)  # end Vectorize
# objective function on parameter grid
par_mean <- seq(1, 6, length=50)
par_sd <- seq(0.5, 3.0, length=50)
objective_grid <- outer(par_mean, par_sd, 
vec_objective, sam_ple=sam_ple)
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
# perspective plot of log-likelihood function
persp(z=-objective_grid,
theta=45, phi=30, shade=0.5,
border="green", zlab="objective",
main="objective function")
# interactive perspective plot of log-likelihood function
library(rgl)
par3d(cex=2.0)  # scale text by factor of 2
persp3d(z=-objective_grid, zlab="objective",
  col="green", main="objective function")
# initial parameters
par_init <- c(mean=0, sd=1)
# perform optimization quasi-Newton method
optim_run <- optim(par=par_init, 
       fn=object_ive, 
       sam_ple=sam_ple,
       method="L-BFGS-B",
       upper=c(10, 10),
       lower=c(-10, 0.1))
# optimal parameters
optim_run$par
# plot histogram
histo_gram <- hist(sam_ple, plot=FALSE)
plot(histo_gram, freq=FALSE,
     main="histogram of sample")
curve(expr=dnorm(x, mean=optim_run$par["mean"],
           sd=optim_run$par["sd"]),
add=TRUE, type="l", lwd=2, col="red")
legend("topright", inset=0.0, cex=0.8, title=NULL,
 leg="optimal parameters",
 lwd=2, bg="white", col="red")
# sample from mixture of normal distributions
sam_ple <- c(rnorm(100, sd=1.0), 
             rnorm(100, mean=4, sd=1.0))
# objective function is log-likelihood
object_ive <- function(parm, sam_ple) {
  likelihood <- parm[1]/parm[3] * 
  dnorm((sam_ple-parm[2])/parm[3]) +
  (1-parm[1])/parm[5]*dnorm((sam_ple-parm[4])/parm[5])
  if(any(likelihood <= 0)) Inf else
    -sum(log(likelihood))
}  # end object_ive
# vectorize objective function
vec_objective <- Vectorize(
  FUN=function(mean, sd, w, m1, s1, sam_ple)
    object_ive(c(w, m1, s1, mean, sd), sam_ple),
  vectorize.args=c("mean", "sd")
)  # end Vectorize
# objective function on parameter grid
par_mean <- seq(3, 5, length=50)
par_sd <- seq(0.5, 1.5, length=50)
objective_grid <- outer(par_mean, par_sd, 
    vec_objective, sam_ple=sam_ple,
    w=0.5, m1=2.0, s1=2.0)
rownames(objective_grid) <- round(par_mean, 2)
colnames(objective_grid) <- round(par_sd, 2)
objective_min <- which(objective_grid==
  min(objective_grid), arr.ind=TRUE)
objective_min
objective_grid[objective_min]
objective_grid[(objective_min[, 1] + -1:1), 
         (objective_min[, 2] + -1:1)]
# perspective plot of objective function
persp(par_mean, par_sd, -objective_grid,
theta=45, phi=30,
shade=0.5,
col=rainbow(50),
border="green",
main="objective function")
# initial parameters
par_init <- c(weight=0.5, m1=0, s1=1, m2=2, s2=1)
# perform optimization
optim_run <- optim(par=par_init, 
      fn=object_ive, 
      sam_ple=sam_ple,
      method="L-BFGS-B",
      upper=c(1,10,10,10,10),
      lower=c(0,-10,0.2,-10,0.2))
optim_run$par
# plot histogram
histo_gram <- hist(sam_ple, plot=FALSE)
plot(histo_gram, freq=FALSE,
     main="histogram of sample")
fit_func <- function(x, parm) {
  parm["weight"] * dnorm(x, mean=parm["m1"], sd=parm["s1"]) +
    (1-parm["weight"]) * dnorm(x, mean=parm["m2"], sd=parm["s2"])
}  # end fit_func
curve(expr=fit_func(x, parm=optim_run$par), add=TRUE,
type="l", lwd=2, col="red")
legend("topright", inset=0.0, cex=0.8, title=NULL,
 leg="optimal parameters",
 lwd=2, bg="white", col="red")
load(file="C:/Develop/data/etf_data.Rdata")
# create list of symbols for optimized portfolio
sym_bols <- c("VTI", "VNQ", "DBC")
# create initial vector of portfolio weights
portf_weights <- rep(1, length(sym_bols))
names(portf_weights) <- sym_bols
# objective equal to minus Sharpe ratio
object_ive <- function(weights) {
  portf_ts <- etf_rets[, sym_bols] %*% weights
  -mean(portf_ts)/sd(portf_ts)
}  # end object_ive
# objective for equal weight portfolio
object_ive(portf_weights)
par(oma=c(1, 1, 1, 1), mgp=c(2, 1, 0), mar=c(5, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# vectorize objective function with respect to third weight
vec_object <- Vectorize(
  FUN=function(weight) object_ive(c(1, 1, weight)),
  vectorize.args="weight"
)  # end Vectorize
# plot objective function with respect to third weight
curve(expr=vec_object,
      type="l", xlim=c(-4.0, 1.0),
      xlab=paste("weight of", names(portf_weights[3])),
      ylab="", lwd=2)
title(main="Objective Function", line=-1)  # add title
# vectorize function with respect to all weights
vec_object <- Vectorize(
  FUN=function(w1, w2, w3)
    object_ive(c(w1, w2, w3)),
  vectorize.args=c("w2", "w3"))  # end Vectorize
# calculate objective on 2-d (w2 x w3) parameter grid
w2 <- seq(-5, 5, length=50)
w3 <- seq(-5, 5, length=50)
grid_object <- outer(w2, w3, FUN=vec_object, w1=1)
rownames(grid_object) <- round(w2, 2)
colnames(grid_object) <- round(w3, 2)
# perspective plot of objective function
persp(w2, w3, -grid_object,
theta=45, phi=30, shade=0.5,
col=rainbow(50), border="green",
main="objective function")
# interactive perspective plot of objective function
library(rgl)
persp3d(z=-grid_object, zlab="objective",
  col="green", main="objective function")
persp3d(
  x=function(w2, w3)
    -vec_object(w1=1, w2, w3),
  xlim=c(-5, 5), ylim=c(-5, 5),
  col="green", axes=FALSE)
# optimization to find weights with maximum Sharpe ratio
optim_run <- optim(par=portf_weights, 
             fn=object_ive, 
             method="L-BFGS-B",
             upper=c(1.1, 10, 10),
             lower=c(0.9, -10, -10))
# optimal parameters
optim_run$par
# optimal Sharpe ratio
-object_ive(optim_run$par)
par(oma=c(1, 0, 1, 0), mgp=c(2, 1, 0), mar=c(2, 1, 2, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
library(PortfolioAnalytics)
# returns of optimal portfolio
optim_rets <- xts(etf_rets[, sym_bols] %*%
optim_run$par, order.by=index(etf_rets))
# assign colnames to this xts
colnames(optim_rets) <- "optim_rets"
# plot in two vertical panels
layout(matrix(c(1,2), 2),
 widths=c(1,1), heights=c(1,3))
# barplot of optimal portfolio weights
barplot(optim_run$par,
  names.arg=names(optim_run$par),
  las=3, ylab="", xlab="Symbol", main="")
# plot optimal returns with "VTI", "VNQ" and "DBC"
chart.CumReturns(
  cbind(optim_rets, etf_rets[, names(portf_weights)]),
  lwd=2, ylab="", legend.loc="topleft", main="")
