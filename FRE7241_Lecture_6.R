x11(width=6, height=4)
par(mar=c(3, 2, 1, 0), oma=c(0, 0, 0, 0))
# VTI percentage returns
returns <- rutils::diffit(log(quantmod::Cl(rutils::etfenv$VTI)))
confl <- 0.1
varisk <- quantile(returns, confl)
cvar <- mean(returns[returns < varisk])
# Or
sortv <- sort(as.numeric(returns))
varind <- round(confl*NROW(returns))
varisk <- sortv[varind]
cvar <- mean(sortv[1:varind])
# Plot histogram of VTI returns
varmin <- (-0.05)
histp <- hist(returns, col="lightgrey",
  xlab="returns", breaks=100, xlim=c(varmin, 0.01),
  ylab="frequency", freq=FALSE, main="VTI Returns Histogram")

# Plot density of losses
densv <- density(returns, adjust=1.5)
lines(densv, lwd=3, col="blue")
# Add line for VaR
abline(v=varisk, col="red", lwd=3)
ymax <- max(densv$y)
text(x=varisk, y=2*ymax/3, labels="VaR", lwd=2, pos=2)
# Add shading for CVaR
rangev <- (densv$x < varisk) & (densv$x > varmin)
polygon(
  c(varmin, densv$x[rangev], varisk),
  c(0, densv$y[rangev], 0),
  col=rgb(1, 0, 0,0.5), border=NA)
text(x=1.5*varisk, y=ymax/7, labels="CVaR", lwd=2, pos=2)

library(rutils)  # Load rutils
library(Rglpk)
# Vector of symbol names and returns
symbolv <- c("VTI", "IEF", "DBC")
nweights <- NROW(symbolv)
returns <- rutils::etfenv$returns[((NROW(returns)-6):NROW(returns)), symbolv]
retsm <- colMeans(returns)
confl <- 0.05
rmin <- 0 ; wmin <- 0 ; wmax <- 1
weightsum <- 1
ncols <- NCOL(returns) # number of assets
nrows <- NROW(returns) # number of rows
# Create objective vector
objvec <- c(numeric(ncols), rep(-1/(confl/nrows), nrows), -1)
# Specify weight constraints
constr <- rbind(
  cbind(rbind(1, retsm),
  matrix(data=0, nrow=2, ncol=(nrows+1))),
  cbind(coredata(returns), diag(nrows), 1))
rhs <- c(weightsum, rmin, rep(0, nrows))
directs <- c("==", ">=", rep(">=", nrows))
# Specify weight bounds
bounds <- list(lower=list(ind=1:ncols, val=rep(wmin, ncols)),
         upper=list(ind=1:ncols, val=rep(wmax, ncols)))
# Perform optimization
optiml <- Rglpk_solve_LP(obj=objvec, mat=constr, dir=directs, rhs=rhs, types=rep("C", NROW(objvec)), max=T, bounds=bounds)
optiml$solution
constr %*% optiml$solution
objvec %*% optiml$solution
as.numeric(optiml$solution[1:ncols])

# Calculate daily percentage returns
symbolv <- c("VTI", "IEF", "DBC")
returns <- rutils::etfenv$returns[, symbolv]
# Create initial vector of portfolio weights
weightv <- rep(1, NROW(symbolv))
names(weightv) <- symbolv
# Objective equal to minus Sharpe ratio
objfun <- function(weightv, returns) {
  retsp <- returns %*% weightv
  if (sd(retsp) == 0)
    return(0)
  else
    return(-mean(retsp)/sd(retsp))
}  # end objfun
# Objective for equal weight portfolio
objfun(weightv, returns=returns)
optiml <- unlist(optimize(
  f=function(weight)
    objfun(c(1, 1, weight), returns=returns),
  interval=c(-4, 1)))
# Vectorize objective function with respect to third weight
objvec <- function(weightv) sapply(weightv,
  function(weight) objfun(c(1, 1, weight),
    returns=returns))
# Or
objvec <- Vectorize(FUN=function(weight)
    objfun(c(1, 1, weight), returns=returns),
  vectorize.args="weight")  # end Vectorize
objvec(1)
objvec(1:3)

x11(width=6, height=5)
par(oma=c(1, 1, 1, 1), mgp=c(2, 1, 0), mar=c(3, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# Plot objective function with respect to third weight
curve(expr=objvec,
      type="l", xlim=c(-4.0, 1.0),
      xlab=paste("weight of", names(weightv[3])),
      ylab="", lwd=2)
title(main="Objective Function", line=(-1))  # Add title
points(x=optiml[1], y=optiml[2], col="green", lwd=6)
text(x=optiml[1], y=optiml[2],
     labels="minimum objective", pos=4, cex=0.8)

#below is simplified code for plotting objective function
# Create vector of DBC weights
weightv <- seq(from=-4, to=1, by=0.1)
obj_val <- sapply(weightv,
  function(weight) objfun(c(1, 1, weight)))
plot(x=weightv, y=obj_val, t="l",
      xlab="weight of DBC", ylab="", lwd=2)
title(main="Objective Function", line=(-1))  # Add title
points(x=optiml[1], y=optiml[2], col="green", lwd=6)
text(x=optiml[1], y=optiml[2],
     labels="minimum objective", pos=4, cex=0.8)

# Vectorize function with respect to all weights
objvec <- Vectorize(
  FUN=function(w1, w2, w3) objfun(c(w1, w2, w3)),
  vectorize.args=c("w2", "w3"))  # end Vectorize
# Calculate objective on 2-d (w2 x w3) parameter grid
w2 <- seq(-3, 7, length=50)
w3 <- seq(-5, 5, length=50)
grid_object <- outer(w2, w3, FUN=objvec, w1=1)
rownames(grid_object) <- round(w2, 2)
colnames(grid_object) <- round(w3, 2)
# Perspective plot of objective function
persp(w2, w3, -grid_object,
theta=45, phi=30, shade=0.5,
col=rainbow(50), border="green",
main="objective function")

# Interactive perspective plot of objective function
library(rgl)
rgl::persp3d(z=-grid_object, zlab="objective",
  col="green", main="objective function")
rgl::persp3d(
  x=function(w2, w3) {-objvec(w1=1, w2, w3)},
  xlim=c(-3, 7), ylim=c(-5, 5),
  col="green", axes=FALSE)

# Optimization to find weights with maximum Sharpe ratio
optiml <- optim(par=weightv,
             fn=objfun,
             returns=returns,
             method="L-BFGS-B",
             upper=c(1.1, 10, 10),
             lower=c(0.9, -10, -10))
# Optimal parameters
optiml$par
optiml$par <- optiml$par/sum(optiml$par)
# Optimal Sharpe ratio
-objfun(optiml$par)

x11(width=6, height=5)
par(oma=c(1, 1, 1, 0), mgp=c(2, 1, 0), mar=c(2, 1, 2, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# Plot in two vertical panels
layout(matrix(c(1,2), 2),
 widths=c(1,1), heights=c(1,3))
# barplot of optimal portfolio weights
barplot(optiml$par, col=c("red", "green", "blue"),
  main="Optimized portfolio weights")
# Calculate cumulative returns of VTI, IEF, DBC
cumrets <- lapply(returns,
  function(returns) exp(cumsum(returns)))
cumrets <- rutils::do_call(cbind, cumrets)
# Calculate optimal portfolio returns with VTI, IEF, DBC
optim_rets <- cbind(
  exp(cumsum(returns %*% optiml$par)),
  cumrets)
colnames(optim_rets)[1] <- "optim_rets"
# Plot optimal returns with VTI, IEF, DBC
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("black", "red", "green", "blue")
chart_Series(optim_rets, theme=plot_theme,
       name="Optimized portfolio performance")
legend("top", legend=colnames(optim_rets), cex=0.8,
 inset=0.1, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")
# Or plot non-compounded (simple) cumulative returns
PerformanceAnalytics::chart.CumReturns(
  cbind(returns %*% optiml$par, returns),
  lwd=2, ylab="", legend.loc="topleft", main="")

riskf <- 0.03
returns <- c(asset1=0.05, asset2=0.06)
stdevs <- c(asset1=0.4, asset2=0.5)
corrp <- 0.6
covmat <- matrix(c(1, corrp, corrp, 1), nc=2)
covmat <- t(t(stdevs*covmat)*stdevs)
library(quadprog)
# Minimum variance weights without constraints
optiml <- solve.QP(Dmat=2*covmat,
            dvec=rep(0, 2),
            Amat=matrix(0, nr=2, nc=1),
            bvec=0)
# Minimum variance weights sum equal to 1
optiml <- solve.QP(Dmat=2*covmat,
            dvec=rep(0, 2),
            Amat=matrix(1, nr=2, nc=1),
            bvec=1)
# Optimal value of objective function
t(optiml$solution) %*% covmat %*% optiml$solution
Perform simple optimization for reference
# Objective function for simple optimization
objfun <- function(x) {
  x <- c(x, 1-x)
  t(x) %*% covmat %*% x
}  # end objfun
unlist(optimize(f=objfun, interval=c(-1, 2)))

# Calculate daily percentage returns
symbolv <- c("VTI", "IEF", "DBC")
returns <- rutils::etfenv$returns[, symbolv]
# Calculate the covariance matrix
covmat <- cov(returns)
# Minimum variance weights, with sum equal to 1
optiml <- quadprog::solve.QP(Dmat=2*covmat,
            dvec=numeric(3),
            Amat=matrix(1, nr=3, nc=1),
            bvec=1)
# Minimum variance, maximum returns
optiml <- quadprog::solve.QP(Dmat=2*covmat,
            dvec=apply(0.1*returns, 2, mean),
            Amat=matrix(1, nr=3, nc=1),
            bvec=1)
# Minimum variance positive weights, sum equal to 1
a_mat <- cbind(matrix(1, nr=3, nc=1),
       diag(3), -diag(3))
b_vec <- c(1, rep(0, 3), rep(-1, 3))
optiml <- quadprog::solve.QP(Dmat=2*covmat,
            dvec=numeric(3),
            Amat=a_mat,
            bvec=b_vec,
            meq=1)

# Rastrigin function with vector argument for optimization
rastrigin <- function(vectorv, param=25){
  sum(vectorv^2 - param*cos(vectorv))
}  # end rastrigin
vectorv <- c(pi/6, pi/6)
rastrigin(vectorv=vectorv)
library(DEoptim)
# Optimize rastrigin using DEoptim
optiml <-  DEoptim(rastrigin,
  upper=c(6, 6), lower=c(-6, -6),
  DEoptim.control(trace=FALSE, itermax=50))
# Optimal parameters and value
optiml$optim$bestmem
rastrigin(optiml$optim$bestmem)
summary(optiml)
plot(optiml)

# Calculate daily percentage returns
returns <- na.omit(rutils::etfenv$returns[, symbolv])
# Objective equal to minus Sharpe ratio
objfun <- function(weightv, returns) {
  retsp <- returns %*% weightv
  if (sd(retsp) == 0)
    return(0)
  else
    return(-mean(retsp)/sd(retsp))
}  # end objfun
# Perform optimization using DEoptim
optiml <- DEoptim::DEoptim(fn=objfun,
  upper=rep(10, NCOL(returns)),
  lower=rep(-10, NCOL(returns)),
  returns=returns,
  control=list(trace=FALSE, itermax=100, parallelType=1))
weightv <- optiml$optim$bestmem/sum(abs(optiml$optim$bestmem))
names(weightv) <- colnames(returns)

# Objective with shrinkage penalty
objfun <- function(weightv, returns, lambda, alpha) {
  retsp <- returns %*% weightv
  if (sd(retsp) == 0)
    return(0)
  else {
    penaltyt <- lambda*((1-alpha)*sum(weightv^2) +
alpha*sum(abs(weightv)))
    return(-mean(retsp)/sd(retsp) + penaltyt)
  }
}  # end objfun
# Objective for equal weight portfolio
weightv <- rep(1, NROW(symbolv))
names(weightv) <- symbolv
lambda <- 0.5 ; alpha <- 0.5
objfun(weightv, returns=returns, lambda=lambda, alpha=alpha)
# Perform optimization using DEoptim
optiml <- DEoptim::DEoptim(fn=objfun,
  upper=rep(10, NCOL(returns)),
  lower=rep(-10, NCOL(returns)),
  returns=returns,
  lambda=lambda,
  alpha=alpha,
  control=list(trace=FALSE, itermax=100, parallelType=1))
weightv <- optiml$optim$bestmem/sum(abs(optiml$optim$bestmem))
names(weightv) <- colnames(returns)

# Select all the ETF symbols except "VXX", "SVXY" "MTUM", "QUAL", "VLUE", and "USMV"
symbolv <- colnames(rutils::etfenv$returns)
symbolv <- symbolv[!(symbolv %in% c("VXX", "SVXY", "MTUM", "QUAL", "VLUE", "USMV"))]
# Extract columns of rutils::etfenv$returns and overwrite NA values
retsp <- rutils::etfenv$returns[, symbolv]
nassets <- NCOL(retsp)
# retsp <- na.omit(retsp)
retsp[1, is.na(retsp[1, ])] <- 0
retsp <- zoo::na.locf(retsp, na.rm=FALSE)
dates <- zoo::index(retsp)
# retsp in excess of risk-free rate
riskf <- 0.03/252
retsx <- (retsp - riskf)

# Maximum Sharpe weights in-sample interval
retsis <- retsp["/2014"]
invmat <- MASS::ginv(cov(retsis))
weightv <- invmat %*% colMeans(retsx["/2014"])
weightv <- drop(weightv/sqrt(sum(weightv^2)))
names(weightv) <- colnames(retsp)
# Plot portfolio weights
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
barplot(sort(weightv), main="Maximum Sharpe Weights", cex.names=0.7)

# Calculate in-sample portfolio returns
portfis <- xts::xts(retsis %*% weightv, zoo::index(retsis))
indeks <- xts::xts(rowMeans(retsis), zoo::index(retsis))
portfis <- portfis*sd(indeks)/sd(portfis)

# Plot cumulative portfolio returns
pnls <- cumsum(cbind(portfis, indeks))
colnames(pnls) <- c("Optimal Portfolio", "Equal Weight Portfolio")
endd <- rutils::calc_endpoints(pnls, interval="months")
dygraphs::dygraph(pnls[endd], main="In-sample Optimal Portfolio Returns") %>%
  dyOptions(colors=c("red", "blue"), strokeWidth=2) %>%
  dyLegend(width=500)

# Calculate out-of-sample portfolio returns
retsos <- retsp["2015/"]
portfos <- xts::xts(retsos %*% weightv, zoo::index(retsos))
indeks <- xts::xts(rowMeans(retsos), zoo::index(retsos))
portfos <- portfos*sd(indeks)/sd(portfos)
pnls <- cbind(portfos, indeks, (portfos + indeks)/2)
colnames(pnls) <- c("Optimal", "Equal Weight", "Combined")
sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x))

# Plot cumulative portfolio returns
endd <- rutils::calc_endpoints(pnls, interval="months")
dygraphs::dygraph(cumsum(pnls)[endd], main="Out-of-sample Optimal Portfolio Returns") %>%
  dyOptions(colors=c("red", "blue", "green"), strokeWidth=2) %>%
  dyLegend(width=500)

# Maximum Sharpe weights in-sample interval
invmat <- MASS::ginv(cov(retsis))
weightv <- invmat %*% colMeans(retsx["/2014"])
weightv <- drop(weightv/sqrt(sum(weightv^2)))
names(weightv) <- colnames(retsp)
# Calculate in-sample portfolio returns
portfis <- xts::xts(retsis %*% weightv, zoo::index(retsis))
# Calculate out-of-sample portfolio returns
retsos <- retsp["2015/"]
portfos <- xts::xts(retsos %*% weightv, zoo::index(retsos))

# Plot cumulative portfolio returns
pnls <- rbind(portfis, portfos)
indeks <- xts::xts(rowMeans(retsp), dates)
pnls <- pnls*sd(indeks)/sd(pnls)
pnls <- cumsum(cbind(pnls, indeks))
colnames(pnls) <- c("Optimal Portfolio", "Equal Weight Portfolio")
endd <- rutils::calc_endpoints(pnls, interval="months")
dygraphs::dygraph(pnls[endd], main="Out-of-sample Optimal Portfolio Returns for ETFs") %>%
  dyOptions(colors=c("red", "blue"), strokeWidth=2) %>%
  dyEvent(zoo::index(last(retsis[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=500)

# Create rectangular matrix with collinear columns
matrixv <- matrix(rnorm(10*8), nc=10)
# Calculate covariance matrix
covmat <- cov(matrixv)
# Calculate inverse of covmat - error
invmat <- solve(covmat)
# Perform eigen decomposition
eigend <- eigen(covmat)
eigenvec <- eigend$vectors
eigenval <- eigend$values
# Set tolerance for determining zero singular values
precision <- sqrt(.Machine$double.eps)
# Calculate regularized inverse matrix
not_zero <- (eigenval > (precision * eigenval[1]))
invreg <- eigenvec[, not_zero] %*%
  (t(eigenvec[, not_zero]) / eigenval[not_zero])
# Verify inverse property of invreg
all.equal(covmat, covmat %*% invreg %*% covmat)
# Calculate regularized inverse of covmat
invmat <- MASS::ginv(covmat)
# Verify inverse property of matrixv
all.equal(invmat, invreg)

# Calculate in-sample covariance matrix
covmat <- cov(retsis)
eigend <- eigen(covmat)
eigenvec <- eigend$vectors
eigenval <- eigend$values
# Calculate shrinkage inverse of covariance matrix
eigen_max <- 3
invmat <- eigenvec[, 1:eigen_max] %*%
  (t(eigenvec[, 1:eigen_max]) / eigend$values[1:eigen_max])
# Verify inverse property of inverse
all.equal(covmat, covmat %*% invmat %*% covmat)

# Calculate portfolio weights
weightv <- invmat %*% colMeans(retsx["/2014"])
weightv <- drop(weightv/sqrt(sum(weightv^2)))
names(weightv) <- colnames(retsp)
# Calculate portfolio returns
portfis <- xts::xts(retsis %*% weightv, zoo::index(retsis))
portfos <- xts::xts(retsos %*% weightv, zoo::index(retsos))

# Plot cumulative portfolio returns
pnls <- rbind(portfis, portfos)
pnls <- pnls*sd(indeks)/sd(pnls)
pnls <- cumsum(cbind(pnls, indeks))
colnames(pnls) <- c("Optimal Portfolio", "Equal Weight Portfolio")
dygraphs::dygraph(pnls[endd], main="Regularized Out-of-sample Optimal Portfolio Returns for ETFs") %>%
  dyOptions(colors=c("red", "blue"), strokeWidth=2) %>%
  dyEvent(zoo::index(last(retsis[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=500)

# Shrink the in-sample returns to their mean
alpha <- 0.7
retsxm <- rowMeans(retsx["/2014"])
retsxis <- (1 - alpha)*retsx["/2014"] + alpha*retsxm

# Calculate portfolio weights
weightv <- invmat %*% colMeans(retsxis)
weightv <- drop(weightv/sqrt(sum(weightv^2)))
# Calculate portfolio returns
portfis <- xts::xts(retsis %*% weightv, zoo::index(retsis))
portfos <- xts::xts(retsos %*% weightv, zoo::index(retsos))
# Plot cumulative portfolio returns
pnls <- rbind(portfis, portfos)
pnls <- pnls*sd(indeks)/sd(pnls)
pnls <- cumsum(cbind(pnls, indeks))
colnames(pnls) <- c("Optimal Portfolio", "Equal Weight Portfolio")
dygraphs::dygraph(pnls[endd], main="Out-of-sample Returns for ETFs With Regularization and Shrinkage") %>%
  dyOptions(colors=c("red", "blue"), strokeWidth=2) %>%
  dyEvent(zoo::index(last(retsis[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=500)

load("/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
# Overwrite NA values in returns
retsp <- returns["2000/"]
nassets <- NCOL(returns)
returns[1, is.na(returns[1, ])] <- 0
returns <- zoo::na.locf(returns, na.rm=FALSE)
dates <- zoo::index(returns)
riskf <- 0.03/252
retsx <- (returns - riskf)
retsis <- returns["/2010"]
retsos <- returns["2011/"]
# Maximum Sharpe weights in-sample interval
covmat <- cov(retsis)
invmat <- MASS::ginv(covmat)
weightv <- invmat %*% colMeans(retsx["/2010"])
weightv <- drop(weightv/sqrt(sum(weightv^2)))
names(weightv) <- colnames(returns)
# Calculate portfolio returns
portfis <- xts::xts(retsis %*% weightv, zoo::index(retsis))
portfos <- xts::xts(retsos %*% weightv, zoo::index(retsos))
indeks <- xts::xts(rowMeans(returns), dates)

# Plot cumulative portfolio returns
pnls <- rbind(portfis, portfos)
pnls <- pnls*sd(indeks)/sd(pnls)
pnls <- cumsum(cbind(pnls, indeks))
colnames(pnls) <- c("Optimal Portfolio", "Equal Weight Portfolio")
endd <- rutils::calc_endpoints(pnls, interval="months")
dygraphs::dygraph(pnls[endd], main="Out-of-sample Optimal Portfolio Returns for Stocks") %>%
  dyOptions(colors=c("red", "blue"), strokeWidth=2) %>%
  dyEvent(zoo::index(last(retsis[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=500)

# Calculate shrinkage inverse of covariance matrix
look_back <- 8; eigen_max <- 3
eigend <- eigen(cov(retsis))
eigenvec <- eigend$vectors
eigenval <- eigend$values
invmat <- eigenvec[, 1:eigen_max] %*%
  (t(eigenvec[, 1:eigen_max]) / eigend$values[1:eigen_max])
# Calculate portfolio weights
weightv <- invmat %*% colMeans(retsx["/2010"])
weightv <- drop(weightv/sqrt(sum(weightv^2)))
names(weightv) <- colnames(returns)
# Calculate portfolio returns
portfis <- xts::xts(retsis %*% weightv, zoo::index(retsis))
portfos <- xts::xts(retsos %*% weightv, zoo::index(retsos))
indeks <- xts::xts(rowMeans(returns), dates)

# Plot cumulative portfolio returns
pnls <- rbind(portfis, portfos)
pnls <- pnls*sd(indeks)/sd(pnls)
pnls <- cumsum(cbind(pnls, indeks))
colnames(pnls) <- c("Optimal Portfolio", "Equal Weight Portfolio")
dygraphs::dygraph(pnls[endd], main="Regularized Out-of-sample Optimal Portfolio Returns for Stocks") %>%
  dyOptions(colors=c("red", "blue"), strokeWidth=2) %>%
  dyEvent(zoo::index(last(retsis[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=500)

# Shrink the in-sample returns to their mean
alpha <- 0.7
retsxm <- rowMeans(retsx["/2010"])
retsxis <- (1 - alpha)*retsx["/2010"] + alpha*retsxm

# Calculate portfolio weights
weightv <- invmat %*% colMeans(retsxis)
weightv <- drop(weightv/sqrt(sum(weightv^2)))
# Calculate portfolio returns
portfis <- xts::xts(retsis %*% weightv, zoo::index(retsis))
portfos <- xts::xts(retsos %*% weightv, zoo::index(retsos))
# Plot cumulative portfolio returns
pnls <- rbind(portfis, portfos)
pnls <- pnls*sd(indeks)/sd(pnls)
pnls <- cumsum(cbind(pnls, indeks))
colnames(pnls) <- c("Optimal Portfolio", "Equal Weight Portfolio")
dygraphs::dygraph(pnls[endd], main="Out-of-sample Returns for Stocks With Regularization and Shrinkage") %>%
  dyOptions(colors=c("red", "blue"), strokeWidth=2) %>%
  dyEvent(zoo::index(last(retsis[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=500)
