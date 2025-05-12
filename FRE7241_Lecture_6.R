# Calculate daily ETF returns
symbolv <- c("VTI", "IEF", "DBC")
nstocks <- NROW(symbolv)
retp <- na.omit(rutils::etfenv$returns[, symbolv])
# Calculate covariance matrix of returns and its inverse
covmat <- cov(retp)
covinv <- solve(a=covmat)
unitv <- rep(1, nstocks)
# Calculate the minimum variance weights
c11 <- drop(t(unitv) %*% covinv %*% unitv)
weightmv <- drop(covinv %*% unitv/c11)
# Calculate the daily minvar portfolio returns in two ways
retmv <- (retp %*% weightmv)
all.equal(retmv, (retp %*% covinv %*% unitv)/c11)
# Calculate the minimum variance in three ways
all.equal(var(retmv),
  t(weightmv) %*% covmat %*% weightmv,
  1/(t(unitv) %*% covinv %*% unitv))
# Calculate vector of mean returns
retm <- colMeans(retp)
# Specify the target return
retarg <- 1.5*mean(retp)
# Products of inverse with mean returns and unit vector
c11 <- drop(t(unitv) %*% covinv %*% unitv)
cr1 <- drop(t(unitv) %*% covinv %*% retm)
crr <- drop(t(retm) %*% covinv %*% retm)
fmat <- matrix(c(c11, cr1, cr1, crr), nc=2)
# Solve for the Lagrange multipliers
lagm <- solve(a=fmat, b=c(2, 2*retarg))
# Calculate the efficient portfolio weights
weightv <- 0.5*drop(covinv %*% cbind(unitv, retm) %*% lagm)
# Calculate constraints
all.equal(1, sum(weightv))
all.equal(retarg, sum(retm*weightv))
# Calculate the efficient portfolio returns
reteff <- drop(retp %*% weightv)
reteffm <- mean(reteff)
all.equal(reteffm, retarg)
# Calculate the efficient portfolio variance in three ways
uu <- c(1, retarg)
finv <- solve(fmat)
detf <- (c11*crr-cr1^2)  # det(fmat)
all.equal(var(reteff),
  drop(t(uu) %*% finv %*% uu),
  (c11*reteffm^2-2*cr1*reteffm+crr)/detf)
# Calculate the daily and mean minvar portfolio returns
c11 <- drop(t(unitv) %*% covinv %*% unitv)
weightv <- drop(covinv %*% unitv/c11)
retmv <- (retp %*% weightv)
retmvm <- sum(weightv*retm)
# Calculate the minimum variance
varmv <- 1/c11
stdevmv <- sqrt(varmv)
# Calculate efficient frontier from target returns
retargv <- retmvm*(1+seq(from=(-1), to=1, by=0.1))
stdevs <- sapply(retargv, function(rett) {
  uu <- c(1, rett)
  sqrt(drop(t(uu) %*% finv %*% uu))
})  # end sapply
# Plot the efficient frontier
plot(x=stdevs, y=retargv, t="l", col="blue", lwd=2,
     main="Efficient Frontier and Minimum Variance Portfolio",
     xlab="standard deviation", ylab="return")
points(x=stdevmv, y=retmvm, col="green", lwd=6)
text(x=stdevmv, y=retmvm, labels="minimum \nvariance",
     pos=4, cex=0.8)
# Calculate standard deviation of efficient portfolio
uu <- c(1, retarg)
stdeveff <- sqrt(drop(t(uu) %*% finv %*% uu))
# Calculate the slope of the tangent line
detf <- (c11*crr-cr1^2)  # det(fmat)
sharper <- (stdeveff*detf)/(c11*retarg-cr1)
# Calculate the risk-free rate as intercept of the tangent line
raterf <- retarg - sharper*stdeveff
# Calculate the risk-free rate from target return
all.equal(raterf,
  (retarg*cr1-crr)/(retarg*c11-cr1))
# Plot efficient frontier
aspectr <- 1.0*max(stdevs)/diff(range(retargv)) # Aspect ratio
plot(x=stdevs, y=retargv, t="l", col="blue", lwd=2, asp=aspectr,
     xlim=c(0.4, 0.6)*max(stdevs), ylim=c(0.2, 0.9)*max(retargv),
     main="Efficient Frontier and Capital Market Line",
     xlab="standard deviation", ylab="return")
# Plot the minimum variance portfolio
points(x=stdevmv, y=retmvm, col="green", lwd=6)
text(x=stdevmv, y=retmvm, labels="minimum \nvariance",
     pos=4, cex=0.8)
# Plot the tangent portfolio
points(x=stdeveff, y=retarg, col="red", lwd=6)
text(x=stdeveff, y=retarg, labels="tangency\nportfolio", pos=2, cex=0.8)
# Plot the risk-free point
points(x=0, y=raterf, col="red", lwd=6)
text(x=0, y=raterf, labels="risk-free", pos=4, cex=0.8)
# Plot the tangent line
abline(a=raterf, b=sharper, lwd=2, col="green")
text(x=0.6*stdev, y=0.8*retarg,
     labels="Capital Market Line", pos=2, cex=0.8,
     srt=180/pi*atan(aspectr*sharper))
# Calculate the mean excess returns
raterf <- retarg - sharper*stdeveff
retx <- (retm - raterf)
# Calculate the efficient portfolio weights
weightv <- 0.5*drop(covinv %*% cbind(unitv, retm) %*% lagm)
# Calculate the maximum Sharpe weights
weightms <- drop(covinv %*% retx)/sum(covinv %*% retx)
all.equal(weightv, weightms)
# Calculate the maximum Sharpe mean return in two ways
all.equal(sum(retm*weightv), (cr1*raterf-crr)/(c11*raterf-cr1))
# Calculate the maximum Sharpe daily returns
retd <- (retp %*% weightms)
# Calculate the maximum Sharpe variance in four ways
detf <- (c11*crr-cr1^2)  # det(fmat)
all.equal(var(retd),
  t(weightv) %*% covmat %*% weightv,
  (t(retx) %*% covinv %*% retx)/sum(covinv %*% retx)^2,
  (c11*retarg^2-2*cr1*retarg+crr)/detf)
# Calculate the maximum Sharpe ratio
sqrt(252)*sum(weightv*retx)/
  sqrt(drop(t(weightv) %*% covmat %*% weightv))
# Calculate the stock Sharpe ratios
sqrt(252)*sapply((retp - raterf), function(x) mean(x)/sd(x))
# Calculate optimal portfolio returns
wealthv <- cbind(retp %*% weightms, retp %*% weightmv)
wealthv <- xts::xts(wealthv, zoo::index(retp))
colnames(wealthv) <- c("MaxSharpe", "MinVar")
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  (mean(x)-raterf)/c(Sharpe=sd(x), Sortino=sd(x[x<0])))
# Plot the log wealth
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Maximum Sharpe and Minimum Variance Portfolios") %>%
  dyOptions(colors=c("blue", "green"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Calculate the maximum Sharpe portfolios for different risk-free rates
detf <- (c11*crr-cr1^2)  # det(fmat)
raterfv <- retmvm*seq(from=1.3, to=20, by=0.1)
raterfv <- c(raterfv, retmvm*seq(from=(-20), to=0.7, by=0.1))
effront <- sapply(raterfv, function(raterf) {
  # Calculate the maximum Sharpe mean return
  reteffm <- (cr1*raterf-crr)/(c11*raterf-cr1)
  # Calculate the maximum Sharpe standard deviation
  stdev <- sqrt((c11*reteffm^2-2*cr1*reteffm+crr)/detf)
  c(return=reteffm, stdev=stdev)
})  # end sapply
effront <- effront[, order(effront["return", ])]
# Plot the efficient frontier
reteffv <- effront["return", ]
stdevs <- effront["stdev", ]
aspectr <- 0.6*max(stdevs)/diff(range(reteffv)) # Aspect ratio
plot(x=stdevs, y=reteffv, t="l", col="blue", lwd=2, asp=aspectr,
  main="Maximum Sharpe Portfolio and Efficient Frontier",
  xlim=c(0.0, max(stdevs)), xlab="standard deviation", ylab="return")
# Plot the minimum variance portfolio
points(x=stdevmv, y=retmvm, col="green", lwd=6)
text(x=stdevmv, y=retmvm, labels="minimum \nvariance", pos=4, cex=0.8)
# Calculate the maximum Sharpe return and standard deviation
raterf <- min(reteffv)
retmax <- (cr1*raterf-crr)/(c11*raterf-cr1)
stdevmax <- sqrt((c11*retmax^2-2*cr1*retmax+crr)/detf)
# Plot the maximum Sharpe portfolio
points(x=stdevmax, y=retmax, col="red", lwd=6)
text(x=stdevmax, y=retmax, labels="Max Sharpe\nportfolio", pos=2, cex=0.8)
# Plot the risk-free point
points(x=0, y=raterf, col="red", lwd=6)
text(x=0, y=raterf, labels="risk-free", pos=4, cex=0.8)
# Plot the tangent line
sharper <- (stdevmax*detf)/(c11*retmax-cr1)
abline(a=raterf, b=sharper, lwd=2, col="green")
text(x=0.6*stdevmax, y=0.8*retmax, labels="Capital Market Line",
     pos=2, cex=0.8, srt=180/pi*atan(aspectr*sharper))
# Plot the efficient frontier
reteffv <- effront["return", ]
stdevs <- effront["stdev", ]
plot(x=stdevs, y=reteffv, t="l", col="blue", lwd=2,
  xlim=c(0.0, max(stdevs)),
  main="Efficient Frontier and Tangent Lines",
  xlab="standard deviation", ylab="return")
# Calculate vector of mean returns
reteffv <- min(reteffv) + diff(range(reteffv))*c(0.2, 0.4, 0.6, 0.8)
# Plot the tangent lines
for (reteffm in reteffv) {
  # Calculate the maximum Sharpe standard deviation
  stdev <- sqrt((c11*reteffm^2-2*cr1*reteffm+crr)/detf)
  # Calculate the slope of the tangent line
  sharper <- (stdev*detf)/(c11*reteffm-cr1)
  # Calculate the risk-free rate as intercept of the tangent line
  raterf <- reteffm - sharper*stdev
  # Plot the tangent portfolio
  points(x=stdev, y=reteffm, col="red", lwd=3)
  # Plot the tangent line
  abline(a=raterf, b=sharper, lwd=2, col="green")
} # end for
# Calculate random portfolios
nportf <- 1000
randportf <- sapply(1:nportf, function(it) {
  weightv <- runif(nstocks-1, min=-0.25, max=1.0)
  weightv <- c(weightv, 1-sum(weightv))
  # Portfolio returns and standard deviation
  c(return=252*sum(weightv*retm),
    stdev=sqrt(252*drop(weightv %*% covmat %*% weightv)))
})  # end sapply
# Plot scatterplot of random portfolios
x11(widthp <- 6, heightp <- 6)
plot(x=randportf["stdev", ], y=randportf["return", ],
     main="Efficient Frontier and Random Portfolios",
     xlim=c(0.5*stdev, 0.8*max(randportf["stdev", ])),
     xlab="standard deviation", ylab="return")
# Plot maximum Sharpe portfolios
lines(x=effront[, "stdev"], y=effront[, "return"], lwd=2)
points(x=effront[, "stdev"], y=effront[, "return"],
 col="red", lwd=3)
# Plot the minimum variance portfolio
points(x=stdev, y=retp, col="green", lwd=6)
text(stdev, retp, labels="minimum\nvariance", pos=2, cex=0.8)
# Plot efficient portfolio
points(x=effront[marketp, "stdev"],
 y=effront[marketp, "return"], col="green", lwd=6)
text(x=effront[marketp, "stdev"], y=effront[marketp, "return"],
     labels="market\nportfolio", pos=2, cex=0.8)
# Plot individual assets
points(x=sqrt(252*diag(covmat)),
 y=252*retm, col="blue", lwd=6)
text(x=sqrt(252*diag(covmat)), y=252*retm,
     labels=names(retm),
     col="blue", pos=1, cex=0.8)
# Define the parameters
raterf <- 0.02 # Risk-free rate
retp <- c(stock1=0.06, stock2=0.09) # Returns
stdevs <- c(stock1=0.4, stock2=0.5) # Standard deviations
corrp <- 0.6 # Correlation
covmat <- matrix(c(1, corrp, corrp, 1), nc=2) # Covariance matrix
covmat <- t(t(stdevs*covmat)*stdevs)
weightv <- seq(from=(-1), to=2, length.out=71) # Weights
weightv <- cbind(weightv, 1-weightv)
retport <- weightv %*% retp # Portfolio returns
portfsd <- sqrt(rowSums(weightv*(weightv %*% covmat))) # Portfolio volatility
sharper <- (retport-raterf)/portfsd # Portfolio Sharpe ratios
# Plot the efficient frontier
# x11(widthp <- 6, heightp <- 5) # Windows
dev.new(widthp <- 6, heightp <- 5, noRStudioGD=TRUE) # Mac
plot(portfsd, retport, t="l",
 main=paste0("Efficient Frontier and CML for Two Stocks\ncorrelation = ", 100*corrp, "%"),
 xlab="standard deviation", ylab="return",
 lwd=2, col="orange", xlim=c(0, max(portfsd)), ylim=c(0.01, max(retport)))
# Add the maximum Sharpe portfolio
whichmax <- which.max(sharper)
sharpem <- max(sharper) # Maximum Sharpe ratio
retmax <- retport[whichmax]
sdeff <- portfsd[whichmax]
weightm <- round(weightv[whichmax], 2)
points(sdeff, retmax, col="blue", lwd=3)
text(x=sdeff, y=retmax, labels=paste(c("Max Sharpe\n",
  structure(c(weightm, (1-weightm)), names=c("stock1", "stock2"))), collapse=" "),
  pos=2, cex=0.8)
# Plot individual stocks
points(stdevs, retp, col="green", lwd=3)
text(stdevs, retp, labels=names(retp), pos=4, cex=0.8)
# Add point at risk-free rate and draw Capital Market Line
points(x=0, y=raterf, col="blue", lwd=3)
text(0, raterf, labels="risk-free\nrate", pos=4, cex=0.8)
abline(a=raterf, b=sharpem, lwd=2, col="blue")
rangev <- par("usr")
text(sdeff/2, (retmax+raterf)/2,
     labels="Capital Market Line", cex=0.8, , pos=3,
     srt=45*atan(sharpem*(rangev[2]-rangev[1])/
             (rangev[4]-rangev[3])*heightp/widthp)/(0.25*pi))
# Plot portfolios in x11() window
x11(widthp <- 6, heightp <- 5)
par(oma=c(0, 0, 0, 0), mar=c(3,3,2,1)+0.1, mgp=c(2, 1, 0), cex.lab=1.0, cex.axis=1.0, cex.main=1.0, cex.sub=1.0)
# Vector of symbol names
symbolv <- c("VTI", "IEF")
# Matrix of portfolio weights
weightv <- seq(from=(-1), to=2, length.out=31)
weightv <- cbind(weightv, 1-weightv)
# Calculate portfolio returns and volatilities
retp <- na.omit(rutils::etfenv$returns[, symbolv])
retport <- retp %*% t(weightv)
portfv <- cbind(252*colMeans(retport),
  sqrt(252)*matrixStats::colSds(retport))
colnames(portfv) <- c("returns", "stdev")
raterf <- 0.06
portfv <- cbind(portfv,
  (portfv[, "returns"]-raterf)/portfv[, "stdev"])
colnames(portfv)[3] <- "Sharpe"
whichmax <- which.max(portfv[, "Sharpe"])
sharpem <- portfv[whichmax, "Sharpe"]
plot(x=portfv[, "stdev"], y=portfv[, "returns"],
     main="Stock and Bond portfolios", t="l",
     xlim=c(0, 0.7*max(portfv[, "stdev"])), ylim=c(0, max(portfv[, "returns"])),
     xlab="standard deviation", ylab="return")
# Add blue point for efficient portfolio
points(x=portfv[whichmax, "stdev"], y=portfv[whichmax, "returns"], col="blue", lwd=6)
text(x=portfv[whichmax, "stdev"], y=portfv[whichmax, "returns"],
     labels=paste(c("efficient portfolio\n",
  structure(c(weightv[whichmax, 1], weightv[whichmax, 2]), names=symbolv)), collapse=" "),
     pos=3, cex=0.8)
# Plot individual stocks
retm <- 252*sapply(retport, mean)
stdevs <- sqrt(252)*sapply(retport, sd)
points(stdevs, retm, col="green", lwd=6)
text(stdevs, retm, labels=names(retport), pos=2, cex=0.8)
# Add point at risk-free rate and draw Capital Market Line
points(x=0, y=raterf, col="blue", lwd=6)
text(0, raterf, labels="risk-free", pos=4, cex=0.8)
abline(a=raterf, b=sharpem, col="blue", lwd=2)
rangev <- par("usr")
text(max(portfv[, "stdev"])/3, 0.75*max(portfv[, "returns"]),
     labels="Capital Market Line", cex=0.8, , pos=3,
     srt=45*atan(sharpem*(rangev[2]-rangev[1])/
             (rangev[4]-rangev[3])*
             heightp/widthp)/(0.25*pi))
# Plot portfolios in x11() window
x11(widthp <- 6, heightp <- 5)
# Calculate cumulative returns of VTI and IEF
retsoptim <- lapply(retp, function(retp) exp(cumsum(retp)))
retsoptim <- rutils::do_call(cbind, retsoptim)
# Calculate the efficient portfolio returns
retsoptim <- cbind(exp(cumsum(retp %*%
    c(weightv[whichmax], 1-weightv[whichmax]))),
  retsoptim)
colnames(retsoptim)[1] <- "efficient"
# Plot efficient portfolio with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue", "green")
chart_Series(retsoptim, theme=plot_theme,
   name="Efficient Portfolio for Stocks and Bonds")
legend("top", legend=colnames(retsoptim),
   cex=0.8, inset=0.1, bg="white", lty=1,
   lwd=6, col=plot_theme$col$line.col, bty="n")
# Calculate daily ETF percentage returns
symbolv <- c("VTI", "IEF", "DBC")
nstocks <- NROW(symbolv)
retp <- na.omit(rutils::etfenv$returns[, symbolv])
nrows <- NROW(retp)
# Create initial vector of portfolio weights
weightv <- rep(1, NROW(symbolv))
names(weightv) <- symbolv
# Objective equal to minus Sharpe ratio
objfun <- function(weightv, retp) {
  retportf <- retp %*% weightv
  stdev <- sd(retportf)
  if (stdev == 0)
    return(0)
  else
    return(-mean(retportf)/stdev)
}  # end objfun
# Objective for equal weight portfolio
objfun(weightv, retp=retp)
optiml <- unlist(optimize(f=function(weightv)
    objfun(c(1, 1, weightv), retp=retp),
  interval=c(-10, 10)))
# Vectorize objective function with respect to third weight
objvec <- function(weightv) sapply(weightv,
  function(weightv) objfun(c(1, 1, weightv), retp=retp))
# Or
objvec <- Vectorize(FUN=function(weightv)
    objfun(c(1, 1, weightv), retp=retp),
  vectorize.args="weightv")  # end Vectorize
objvec(1)
objvec(1:3)
# Plot objective function with respect to third weight
curve(expr=objvec, type="l", xlim=c(-4.0, 1.0),
xlab=paste("weight of", names(weightv[3])),
ylab="", lwd=2)
title(main="Sharpe Ratio", line=(-1))  # Add title
points(x=optiml[1], y=optiml[2], col="green", lwd=6)
text(x=optiml[1], y=optiml[2],
     labels="maximum value", pos=4, cex=0.8)
#Below is simplified plotting of objective function
# Create vector of DBC weights
weightv <- seq(from=-4, to=1, by=0.1)
objv <- sapply(weightv, function(weightv)
  objfun(c(1, 1, weightv), retp))
plot(x=weightv, y=objv, t="l",
xlab="weight of DBC", ylab="", lwd=2)
title(main="Sharpe Ratio", line=(-1))  # Add title
points(x=optiml[1], y=optiml[2], col="green", lwd=6)
text(x=optiml[1], y=optiml[2],
     labels="maximum value", pos=4, cex=0.8)
# Vectorize function with respect to two weights
objvec <- Vectorize(
  FUN=function(w1, w2, w3) objfun(c(w1, w2, w3), retp),
  vectorize.args=c("w2", "w3"))  # end Vectorize
# Calculate objective on 2-d (w2 x w3) parameter grid
w2 <- seq(-3, 7, length=50)
w3 <- seq(-5, 5, length=50)
gridm <- outer(w2, w3, FUN=objvec, w1=1)
rownames(gridm) <- round(w2, 2)
colnames(gridm) <- round(w3, 2)
# Perspective plot of objective function
persp(w2, w3, -gridm,
theta=45, phi=30, shade=0.5,
col=rainbow(50), border="green",
main="objective function")
# Interactive perspective plot of objective function
library(rgl)
rgl::persp3d(z=-gridm, zlab="objective",
  col="green", main="objective function")
rgl::persp3d(
  x=function(w2, w3) {objvec(w1=1, w2, w3)},
  xlim=c(-3, 7), ylim=c(-5, 5),
  col="green", axes=FALSE)
# Render the 3d surface plot of function
rgl::rglwidget(elementId="plot3drgl", width=1000, height=1000)
# Create initial vector of portfolio weights
weightv <- rep(1, NROW(symbolv))
names(weightv) <- symbolv
# Optimization to find weights with maximum Sharpe ratio
optiml <- optim(par=weightv,
          fn=objfun,
          retp=retp,
          method="L-BFGS-B",
          control=list(factr=1e5),
          upper=c(10, 10, 10),
          lower=c(-10, -10, -10))
# Optimal parameters
weightv <- optiml$par
weightv <- weightv/sqrt(sum(weightv^2))
weightv
# Optimal Sharpe ratio
-objfun(weightv, retp)
# Calculate the weights from the inverse covariance matrix
weightv <- drop(solve(cov(retp)) %*% sapply(retp, mean))
weightv <- weightv/sqrt(sum(weightv^2))
weightv
# barplot of optimal portfolio weights
barplot(weightv, col=c("red", "green", "blue"),
  main="Optimized portfolio weights")
# Calculate the cumulative wealth of the optimized portfolio
wealthv <- cbind(retp %*% weightv, retp)
colnames(wealthv)[1] <- "combined"
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  (mean(x)-raterf)/c(Sharpe=sd(x), Sortino=sd(x[x<0])))
# Plot the log wealth
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
colv <- colnames(wealthv)
colr <- c("red", "blue", "green", "grey")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Optimized Portfolio Returns") %>%
  dyOptions(colors=colr, strokeWidth=1) %>%
  dySeries(name=colv[1], col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Rastrigin function with vector argument for optimization
rastrigin <- function(vecv, param=25){
  sum(vecv^2 - param*cos(vecv))
}  # end rastrigin
vecv <- c(pi/6, pi/6)
rastrigin(vecv=vecv)
library(DEoptim)
# Optimize rastrigin using DEoptim
optiml <- DEoptim::DEoptim(rastrigin,
  upper=c(6, 6), lower=c(-6, -6),
  DEoptim.control(trace=FALSE, itermax=50))
# Optimal parameters and value
optiml$optim$bestmem
rastrigin(optiml$optim$bestmem)
summary(optiml)
plot(optiml)
# Perform optimization using DEoptim
optiml <- DEoptim::DEoptim(fn=objfun,
  upper=rep(10, NCOL(retp)),
  lower=rep(-10, NCOL(retp)),
  retp=retp,
  control=list(trace=FALSE, itermax=100, parallelType=1))
weightv <- optiml$optim$bestmem
names(weightv) <- colnames(retp)
weightv <- weightv/sqrt(sum(weightv^2))
weightv
# Objective with shrinkage penalty
objfun <- function(weightv, retp, lambdaf, alphaf) {
  retportf <- retp %*% weightv
  stdev <- sd(retportf)
  if (stdev == 0)
    return(0)
  else {
    penaltyv <- lambdaf*((1-alphaf)*sum(weightv^2) +
alphaf*sum(abs(weightv)))
    return(-mean(retportf)/stdev + penaltyv)
  }
}  # end objfun
# Objective for equal weight portfolio
weightv <- rep(1, NROW(symbolv))
names(weightv) <- symbolv
lambdaf <- 0.5 ; alphaf <- 0.5
objfun(weightv, retp=retp, lambdaf=lambdaf, alphaf=alphaf)
# Perform optimization using DEoptim
optiml <- DEoptim::DEoptim(fn=objfun,
  upper=rep(10, NCOL(retp)),
  lower=rep(-10, NCOL(retp)),
  retp=retp,
  lambdaf=lambdaf,
  alphaf=alphaf,
  control=list(trace=FALSE, itermax=100, parallelType=1))
weightv <- optiml$optim$bestmem
names(weightv) <- colnames(retp)
weightv <- weightv/sqrt(sum(weightv^2))
weightv
# Load stock returns
load("/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
datev <- zoo::index(na.omit(retstock$GOOGL))
retp <- retstock[datev] # Subset the returns to GOOGL
# Remove the stocks with any NA values
numna <- sapply(retp, function(x) sum(is.na(x)))
retp <- retp[, numna==0]
# Select 100 random stocks
retp <- retp[, sample(NCOL(retp), 100)]
symbolv <- colnames(retp)
datev <- zoo::index(retp)
retis <- retp["/2014"] # In-sample returns
raterf <- 0.03/252
retx <- (retis - raterf) # Excess returns
# Calculate the maximum Sharpe weights in-sample interval
colmeanv <- colMeans(retx, na.rm=TRUE)
covmat <- cov(retx, use="pairwise.complete.obs")
invreg <- MASS::ginv(covmat)
wmaxs <- drop(invreg %*% colmeanv)
names(wmaxs) <- symbolv
# Calculate the weights using optimization without shrinkage
lambdaf <- 0.0 ; alphaf <- 1.0
optiml <- optim(par=wmaxs,
          fn=objfun,
          retp=retx,
          lambdaf=lambdaf,
          alphaf=alphaf,
          method="L-BFGS-B",
          control=list(factr=1e5),
          upper=c(10, 10, 10),
          lower=c(-10, -10, -10))
weighto <- optiml$par
names(weighto) <- symbolv
all.equal(weighto, wmaxs)
# Select ETF symbols
symbolv <- c("IEF", "DBC", "XLU", "XLF", "XLP", "XLI")
# Calculate the ETF prices and log returns
pricev <- rutils::etfenv$prices[, symbolv]
# Applying zoo::na.locf() can produce bias of the correlations
# pricev <- zoo::na.locf(pricev, na.rm=FALSE)
# pricev <- zoo::na.locf(pricev, fromLast=TRUE)
pricev <- na.omit(pricev)
retp <- rutils::diffit(log(pricev))
# Calculate the covariance matrix
covmat <- cov(retp)
# Standardize (de-mean and scale) the returns
retp <- lapply(retp, function(x) {(x - mean(x))/sd(x)})
retp <- rutils::do_call(cbind, retp)
round(sapply(retp, mean), 6)
sapply(retp, sd)
# Alternative (much slower) center (de-mean) and scale the returns
# retp <- apply(retp, 2, scale)
# retp <- xts::xts(retp, zoo::index(pricev))
# Alternative (much slower) center (de-mean) and scale the returns
# retp <- scale(retp, center=TRUE, scale=TRUE)
# retp <- xts::xts(retp, zoo::index(pricev))
# Alternative (much slower) center (de-mean) and scale the returns
# retp <- t(retp) - colMeans(retp)
# retp <- retp/sqrt(rowSums(retp^2)/(NCOL(retp)-1))
# retp <- t(retp)
# retp <- xts::xts(retp, zoo::index(pricev))
# Calculate the correlation matrix
cormat <- cor(retp)
# Reorder correlation matrix based on clusters
library(corrplot)
ordern <- corrMatOrder(cormat, order="hclust",
  hclust.method="complete")
cormat <- cormat[ordern, ordern]
# Plot the correlation matrix
colorv <- colorRampPalette(c("red", "white", "blue"))
# x11(width=6, height=6)
corrplot(cormat, title=NA, tl.col="black", mar=c(0,0,0,0),
    method="square", col=colorv(NCOL(cormat)), tl.cex=0.8,
    cl.offset=0.75, cl.cex=0.7, cl.align.text="l", cl.ratio=0.25)
title("ETF Correlation Matrix", line=2)
# Draw rectangles on the correlation matrix plot
corrRect.hclust(cormat, k=NROW(cormat) %/% 2,
  method="complete", col="red")
# Create initial vector of portfolio weights
nweights <- NROW(symbolv)
weightv <- rep(1/sqrt(nweights), nweights)
names(weightv) <- symbolv
# Objective function equal to minus portfolio variance
objfun <- function(weightv, retp) {
  retp <- retp %*% weightv
  -sum(retp^2) + 1e4*(1 - sum(weightv^2))^2
}  # end objfun
# Objective for equal weight portfolio
objfun(weightv, retp)
# Compare speed of vector multiplication methods
summary(microbenchmark(
  transp=(t(retp[, 1]) %*% retp[, 1]),
  sumv=sum(retp[, 1]^2),
  times=10))[, c(1, 4, 5)]
# Find weights with maximum variance
optiml <- optim(par=weightv,
  fn=objfun,
  retp=retp,
  method="L-BFGS-B",
  upper=rep(10.0, nweights),
  lower=rep(-10.0, nweights))
# Optimal weights and maximum variance
weights1 <- optiml$par
-objfun(weights1, retp)
# Plot first principal component weights
barplot(weights1, names.arg=names(weights1), xlab="", ylab="",
  main="First Principal Component Weights")
# PC1 returns
pc1 <- drop(retp %*% weights1)
# Redefine objective function
objfun <- function(weightv, retp) {
  retp <- retp %*% weightv
  -sum(retp^2) + 1e4*(1 - sum(weightv^2))^2 +
    1e4*(sum(weights1*weightv))^2
}  # end objfun
# Find second PC weights using parallel DEoptim
optiml <- DEoptim::DEoptim(fn=objfun,
  upper=rep(10, NCOL(retp)),
  lower=rep(-10, NCOL(retp)),
  retp=retp, control=list(parVar="weights1",
    trace=FALSE, itermax=1000, parallelType=1))
# PC2 weights
weights2 <- optiml$optim$bestmem
names(weights2) <- colnames(retp)
sum(weights2^2)
sum(weights1*weights2)
# PC2 returns
pc2 <- drop(retp %*% weights2)
# Plot second principal component loadings
barplot(weights2, names.arg=names(weights2), xlab="", ylab="",
  main="Second Principal Component Loadings")
# Calculate the eigenvalues and eigenvectors
eigend <- eigen(cormat)
eigend$vectors
# Compare with optimization
all.equal(sum(diag(cormat)), sum(eigend$values))
all.equal(abs(eigend$vectors[, 1]), abs(weights1), check.attributes=FALSE)
all.equal(abs(eigend$vectors[, 2]), abs(weights2), check.attributes=FALSE)
all.equal(eigend$values[1], var(pc1), check.attributes=FALSE)
all.equal(eigend$values[2], var(pc2), check.attributes=FALSE)
# Eigenvalue equations
(cormat %*% weights1) / weights1 / var(pc1)
(cormat %*% weights2) / weights2 / var(pc2)
# Plot eigenvalues
barplot(eigend$values, names.arg=paste0("PC", 1:nweights),
  las=3, xlab="", ylab="", main="Principal Component Variances")
# Calculate the eigen decomposition of the correlation matrix
eigend <- eigen(cormat)
# Perform PCA with scaling
pcad <- prcomp(retp, scale=TRUE)
# Compare outputs
all.equal(eigend$values, pcad$sdev^2)
all.equal(abs(eigend$vectors), abs(pcad$rotation),
    check.attributes=FALSE)
# Eigen decomposition of covariance matrix
eigend <- eigen(covmat)
# Perform PCA without scaling
pcad <- prcomp(retp, scale=FALSE)
# Compare outputs
all.equal(eigend$values, pcad$sdev^2)
all.equal(abs(eigend$vectors), abs(pcad$rotation),
    check.attributes=FALSE)
# Redefine objective function to minimize variance
objfun <- function(weightv, retp) {
  retp <- retp %*% weightv
  sum(retp^2) + 1e4*(1 - sum(weightv^2))^2
}  # end objfun
# Find highest order PC weights using parallel DEoptim
optiml <- DEoptim::DEoptim(fn=objfun,
  upper=rep(10, NCOL(retp)),
  lower=rep(-10, NCOL(retp)),
  retp=retp, control=list(trace=FALSE,
    itermax=1000, parallelType=1))
# PC6 weights and returns
weights6 <- optiml$optim$bestmem
names(weights6) <- colnames(retp)
sum(weights6^2)
sum(weights1*weights6)
# Compare with eigend vector
weights6
eigend$vectors[, 6]
# Calculate the objective function
objfun(weights6, retp)
objfun(eigend$vectors[, 6], retp)
# Plot highest order principal component loadings
weights6 <- eigend$vectors[, 6]
names(weights6) <- colnames(retp)
barplot(weights6, names.arg=names(weights6), xlab="", ylab="",
  main="Highest Order Principal Component Loadings")
# Perform principal component analysis PCA
pcad <- prcomp(retp, scale=TRUE)
# Plot standard deviations of principal components
barplot(pcad$sdev, names.arg=colnames(pcad$rotation),
  las=3, xlab="", ylab="",
  main="Scree Plot: Volatilities of Principal Components \n of ETF Returns")
# Calculate the number of principal components which sum up to at least 80% of the total variance
pcavar <- pcad$sdev^2
which(cumsum(pcavar)/sum(pcavar) > 0.8)[1]
# Plot barplots with PCA loadings (weights) in multiple panels
pcad$rotation
# x11(width=6, height=7)
par(mfrow=c(nweights/2, 2))
par(mar=c(3, 2, 2, 1), oma=c(0, 0, 0, 0))
for (ordern in 1:nweights) {
  barplot(pcad$rotation[, ordern], las=3, xlab="", ylab="", main="")
  title(paste0("PC", ordern), line=-1, col.main="red")
}  # end for
# Calculate the products of principal component time series
round(t(pcad$x) %*% pcad$x, 2)
# Calculate the principal component time series from returns
datev <- zoo::index(pricev)
retpca <- xts::xts(retp %*% pcad$rotation, order.by=datev)
round(cov(retpca), 3)
all.equal(coredata(retpca), pcad$x, check.attributes=FALSE)
retpcac <- cumsum(retpca)
# Plot principal component time series in multiple panels
rangev <- range(retpcac)
for (ordern in 1:nweights) {
  plot.zoo(retpcac[, ordern], ylim=rangev, xlab="", ylab="")
  title(paste0("PC", ordern), line=-1, col.main="red")
}  # end for
# Invert all the principal component time series
retpca <- retp %*% pcad$rotation
solved <- retpca %*% solve(pcad$rotation)
all.equal(coredata(retp), solved)
# Invert first 3 principal component time series
solved <- retpca[, 1:3] %*% solve(pcad$rotation)[1:3, ]
solved <- xts::xts(solved, datev)
solved <- cumsum(solved)
retc <- cumsum(retp)
# Plot the solved returns
for (symbol in symbolv) {
  plot.zoo(cbind(retc[, symbol], solved[, symbol]),
    plot.type="single", col=c("black", "blue"), xlab="", ylab="")
  legend(x="topleft", bty="n", legend=paste0(symbol, c("", " solved")), y.intersp=0.4,
   title=NULL, inset=0.0, cex=1.0, lwd=6, lty=1, col=c("black", "blue"))
}  # end for
# Create a matrix with low correlation
ndata <- 10
cormat <- matrix(rep(0.1, ndata^2), nc=ndata)
diag(cormat) <- rep(1, ndata)
# Calculate the condition number
eigend <- eigen(cormat)
eigenval <- eigend$values
max(eigenval)/min(eigenval)
# Create a matrix with high correlation
cormat <- matrix(rep(0.9, ndata^2), nc=ndata)
diag(cormat) <- rep(1, ndata)
# Calculate the condition number
eigend <- eigen(cormat)
eigenval <- eigend$values
max(eigenval)/min(eigenval)
# Calculate the condition numbers as function correlation
corv <- seq(0.1, 0.9, 0.1)
condv <- sapply(corv, function(corv) {
  cormat <- matrix(rep(corv, ndata^2), nc=ndata)
  diag(cormat) <- rep(1, ndata)
  eigend <- eigen(cormat)
  eigenval <- eigend$values
  max(eigenval)/min(eigenval)
})  # end sapply
# Plot the condition numbers
plot(x=corv, y=condv, t="l",
  main="Condition Number as Function of Correlation",
  xlab="correlation", ylab="condition number")
# Simulate uncorrelated stock returns
nstocks <- 10
nrows <- 100
# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
retp <- matrix(rnorm(nstocks*nrows), nc=nstocks)
# Calculate the condition numbers as function of number of observations
obsvec <- seq(20, nrows, 10)
condv <- sapply(obsvec, function(nobs) {
  cormat <- cor(retp[1:nobs, ])
  eigend <- eigen(cormat)
  eigenval <- eigend$values
  max(eigenval)/min(eigenval)
})  # end sapply
# Plot the condition numbers
plot(x=obsvec, y=condv, t="l",
  main="Condition Number as Function of Number of Observations",
  xlab="number of observations", ylab="condition number")
# Load daily S&P500 log percentage stock returns
load(file="/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
# Calculate the number of NA values in retstock
retp <- retstock
colSums(is.na(retp))
# Calculate the correlations ignoring NA values
cor(retp$DAL, retp$FOXA, use="pairwise.complete.obs")
cor(na.omit(retp[, c("DAL", "FOXA")]))[2]
cormat <- cor(retp, use="pairwise.complete.obs")
sum(is.na(cormat))
cormat[is.na(cormat)] <- 0
# Perform principal component analysis PCA - produces error
pcad <- prcomp(retp, scale=TRUE)
# Calculate the eigen decomposition of the correlation matrix
eigend <- eigen(cormat)
# Calculate the eigenvalues and eigenvectors
eigenval <- eigend$values
eigenvec <- eigend$vectors
# Calculate the number of negative eigenvalues
sum(eigenval<0)
# Calculate the condition number
max(eigenval)/min(abs(eigenval))
# Calculate the number of eigenvalues which sum up to at least 80% of the total variance
which(cumsum(eigenval)/sum(eigenval) > 0.8)[1]
# Plot the eigenvalues
barplot(eigenval, xlab="", ylab="", las=3,
  names.arg=paste0("ev", 1:NROW(eigenval)),
  main="Eigenvalues of Stock Correlation Matrix")
# Calculate the stock variance
varv <- sapply(retp, var, na.rm=TRUE)
# Calculate the returns of low and high volatility stocks
nstocks <- NCOL(retp)
medianv <- median(varv)
retlow <- retp[, varv <= medianv]
rethigh <- retp[, varv > medianv]
# Calculate the correlations of low volatility stocks
cormat <- cor(retlow, use="pairwise.complete.obs")
cormat[is.na(cormat)] <- 0
# Calculate the mean correlations
mean(cormat[upper.tri(cormat)])
# Calculate the eigen decomposition of the correlation matrix
eigend <- eigen(cormat)
eigenval <- eigend$values
# Calculate the number of negative eigenvalues
sum(eigenval < 0)
# Calculate the number of eigenvalues which sum up to at least 80% of the total variance
which(cumsum(eigenval)/sum(eigenval) > 0.8)[1]
# Calculate the condition number
max(eigenval)/min(abs(eigenval))
# Calculate the correlations of high volatility stocks
cormat <- cor(rethigh, use="pairwise.complete.obs")
cormat[is.na(cormat)] <- 0
# Calculate the mean correlations
mean(cormat[upper.tri(cormat)])
# Calculate the eigen decomposition of the correlation matrix
eigend <- eigen(cormat)
eigenval <- eigend$values
# Calculate the number of negative eigenvalues
sum(eigenval < 0)
# Calculate the number of eigenvalues which sum up to at least 80% of the total variance
which(cumsum(eigenval)/sum(eigenval) > 0.8)[1]
# Calculate the condition number
max(eigenval)/min(abs(eigenval))
# Subset (select) the stock returns after the start date of VTI
retvti <- na.omit(rutils::etfenv$returns$VTI)
colnames(retvti) <- "VTI"
retp <- retstock[zoo::index(retvti)]
datev <- zoo::index(retp)
retvti <- retvti[datev]
nrows <- NROW(retp)
nstocks <- NCOL(retp)
head(retp[, 1:5])
# Calculate the monthly end points
endd <- rutils::calc_endpoints(retvti, interval="months")
retvti[head(endd)]
retvti[tail(endd)]
# Remove stub interval at the end
endd <- endd[-NROW(endd)]
npts <- NROW(endd)
# Calculate the monthly stock volatilities and correlations
stdcor <- sapply(2:npts, function(endp) {
  # cat("endp = ", endp, "\n")
  retp <- retp[endd[endp-1]:endd[endp]]
  cormat <- cor(retp, use="pairwise.complete.obs")
  cormat[is.na(cormat)] <- 0
  c(stdev=sd(retvti[endd[endp-1]:endd[endp]]),
    cor=mean(cormat[upper.tri(cormat)]))
})  # end sapply
stdcor <- t(stdcor)
# Scatterplot of stock volatilities and correlations
plot(x=stdcor[, "stdev"], y=stdcor[, "cor"],
 xlab="volatility", ylab="correlation",
 main="Monthly Stock Volatilities and Correlations")
# Plot stock volatilities and correlations
colv <- colnames(stdcor)
stdcor <- xts(stdcor, zoo::index(retvti[endd]))
dygraphs::dygraph(stdcor,
  main="Monthly Stock Volatilities and Correlations") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", label=colv[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colv[2], axis="y2", label=colv[2], strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)
# Calculate the median VTI volatility
medianv <- median(stdcor[, "stdev"])
# Calculate the stock returns of low volatility intervals
retlow <- lapply(2:npts, function(endp) {
  if (stdcor[endp-1, "stdev"] <= medianv)
    retp[endd[endp-1]:endd[endp]]
})  # end lapply
retlow <- rutils::do_call(rbind, retlow)
# Calculate the stock returns of high volatility intervals
rethigh <- lapply(2:npts, function(endp) {
  if (stdcor[endp-1, "stdev"] > medianv)
    retp[endd[endp-1]:endd[endp]]
})  # end lapply
rethigh <- rutils::do_call(rbind, rethigh)
# Calculate the correlations of low volatility intervals
cormat <- cor(retlow, use="pairwise.complete.obs")
cormat[is.na(cormat)] <- 0
mean(cormat[upper.tri(cormat)])
# Calculate the eigen decomposition of the correlation matrix
eigend <- eigen(cormat)
eigenval <- eigend$values
sum(eigenval < 0)
# Calculate the number of eigenvalues which sum up to at least 80% of the total variance
which(cumsum(eigenval)/sum(eigenval) > 0.8)[1]
# Calculate the condition number
max(eigenval)/min(abs(eigenval))
# Calculate the correlations of high volatility intervals
cormat <- cor(rethigh, use="pairwise.complete.obs")
cormat[is.na(cormat)] <- 0
mean(cormat[upper.tri(cormat)])
# Calculate the eigen decomposition of the correlation matrix
eigend <- eigen(cormat)
eigenval <- eigend$values
sum(eigenval < 0)
# Calculate the number of eigenvalues which sum up to at least 80% of the total variance
which(cumsum(eigenval)/sum(eigenval) > 0.8)[1]
# Calculate the condition number
max(eigenval)/min(abs(eigenval))
# Calculate the AAPL and XLK returns
retp <- na.omit(cbind(returns$AAPL, rutils::etfenv$returns$XLK))
# Calculate the trailing correlations
lambdaf <- 0.99
covarv <- HighFreq::run_covar(retp, lambdaf)
correlv <- covarv[, 1, drop=FALSE]/sqrt(covarv[, 2]*covarv[, 3])
# Plot dygraph of XLK returns and AAPL correlations
datav <- cbind(cumsum(retp$XLK), correlv)
colnames(datav)[2] <- "correlation"
colv <- colnames(datav)
endd <- rutils::calc_endpoints(retp, interval="weeks")
dygraphs::dygraph(datav[endd], main="AAPL Correlations With XLK") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", label=colv[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colv[2], axis="y2", label=colv[2], strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)
# Scatterplot of trailing stock volatilities and correlations
volv <- sqrt(covarv[, 2])
plot(x=volv[endd], y=correlv[endd, ], pch=1, col="blue",
 xlab="AAPL volatility", ylab="Correlation",
 main="Trailing Volatilities and Correlations of AAPL vs XLK")
# Interactive scatterplot of trailing stock volatilities and correlations
datev <- zoo::index(retp[endd])
datav <- data.frame(datev, volv[endd], correlv[endd, ])
colnames(datav) <- c("date", "volatility", "correlation")
library(plotly)
plotly::plot_ly(data=datav, x=~volatility, y=~correlation,
  type="scatter", mode="markers", text=datev) %>%
  layout(title="Trailing Volatilities and Correlations of AAPL vs XLK")
# Plot trailing stock volatilities and correlations
datav <- xts(cbind(volv, correlv), zoo::index(retp))
colnames(datav) <- c("volatility", "correlation")
colv <- colnames(datav)
dygraphs::dygraph(datav[endd], main="AAPL Trailing Stock Volatility and Correlation") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", label=colv[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colv[2], axis="y2", label=colv[2], strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)
# Calculate the portfolio returns
retvti <- na.omit(rutils::etfenv$returns$VTI)
colnames(retvti) <- "VTI"
datev <- zoo::index(retvti)
retp <- retstock100
retp[is.na(retp)] <- 0
retp <- retp[datev]
nrows <- NROW(retp)
nstocks <- NCOL(retp)
head(retp[, 1:5])
# Calculate the average trailing portfolio correlations
lambdaf <- 0.9
correlv <- sapply(retp, function(retp) {
  covarv <- HighFreq::run_covar(cbind(retvti, retp), lambdaf)
  covarv[, 1, drop=FALSE]/sqrt(covarv[, 2]*covarv[, 3])
})  # end sapply
correlv[is.na(correlv)] <- 0
correlp <- rowMeans(correlv)
# Scatterplot of trailing stock volatilities and correlations
volvti <- sqrt(HighFreq::run_var(retvti, lambdaf)[, 2])
endd <- rutils::calc_endpoints(retvti, interval="weeks")
plot(x=volvti[endd], y=correlp[endd],
 xlab="volatility", ylab="correlation",
 main="Trailing Stock Volatilities and Correlations")
# Plot trailing stock volatilities and correlations
datav <- xts(cbind(volvti, correlp), datev)
colnames(datav) <- c("volatility", "correlation")
colv <- colnames(datav)
dygraphs::dygraph(datav[endd],
  main="Trailing Stock Volatilities and Correlations") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", label=colv[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colv[2], axis="y2", label=colv[2], strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)
