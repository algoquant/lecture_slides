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
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw],
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
themev <- chart_theme()
themev$col$line.col <- c("orange", "blue", "green")
chart_Series(retsoptim, theme=themev,
   name="Efficient Portfolio for Stocks and Bonds")
legend("top", legend=colnames(retsoptim),
   cex=0.8, inset=0.1, bg="white", lty=1,
   lwd=6, col=themev$col$line.col, bty="n")

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

##Below is simplified plotting of objective function
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
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
colv <- colnames(wealthv)
colr <- c("red", "blue", "green", "grey")
dygraphs::dygraph(cumsum(wealthv)[endw],
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
# Maximum Sharpe weights in-sample interval
colmeanv <- colMeans(retx, na.rm=TRUE)
covmat <- cov(retx, use="pairwise.complete.obs")
invreg <- MASS::ginv(covmat)
wmaxs <- drop(invreg %*% colmeanv)
names(wmaxs) <- symbolv
# Calculate the maximum Sharpe portfolio returns
pnlmaxs <- HighFreq::mult_mat(wmaxs, retp)
pnlmaxs <- rowMeans(pnlmaxs, na.rm=TRUE)
pnlmaxs <- xts::xts(pnlmaxs, datev)
retew <- xts::xts(rowMeans(retp, na.rm=TRUE), datev)
pnlmaxs <- pnlmaxs*sd(retew["/2014"])/sd(pnlmaxs["/2014"])
# Calculate the shrinkage portfolio returns
lambdaf <- 1.0 ; alphaf <- 1.0
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
pnls <- HighFreq::mult_mat(weighto, retp)
pnls <- rowMeans(pnls, na.rm=TRUE)
pnls <- xts::xts(pnls, datev)
pnls <- pnls*sd(retew["/2014"])/sd(pnls["/2014"])

wealthv <- cbind(pnlmaxs, pnls)
colnames(wealthv) <- c("MaxSharpe", "Shrink")
# Calculate the in-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv["/2014"], function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv["2015/"], function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of cumulative portfolio returns
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Out-of-Sample Maximum Sharpe Stock Portfolio") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyEvent(end(retis[, 1]), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=300)

library(rutils)
library(Rglpk)
# Vector of symbol names
symbolv <- c("VTI", "IEF", "DBC")
nstocks <- NROW(symbolv)
# Calculate the objective vector - the mean returns
retp <- na.omit(rutils::etfenv$returns[, symbolv])
objvec <- colMeans(retp)
# Specify matrix of linear constraint coefficients
coeffm <- matrix(c(rep(1, nstocks), 1, 1, 0),
           nc=nstocks, byrow=TRUE)
# Specify the logical constraint operators
logop <- c("==", "<=")
# Specify the vector of constraints
consv <- c(1, 0)
# Specify box constraints (-1, 1) (default is c(0, Inf))
boxc <- list(lower=list(ind=1:nstocks, val=rep(-1, nstocks)),
       upper=list(ind=1:nstocks, val=rep(1, nstocks)))
# Perform optimization
optiml <- Rglpk::Rglpk_solve_LP(
  obj=objvec,
  mat=coeffm,
  dir=logop,
  rhs=consv,
  bounds=boxc,
  max=TRUE)
all.equal(optiml$optimum, sum(objvec*optiml$solution))
optiml$solution
coeffm %*% optiml$solution

# Calculate the VTI percentage returns
retp <- na.omit(rutils::etfenv$returns$VTI)
confl <- 0.1
varisk <- quantile(retp, confl)
cvar <- mean(retp[retp < varisk])
# Or
sortv <- sort(as.numeric(retp))
varind <- round(confl*NROW(retp))
varisk <- sortv[varind]
cvar <- mean(sortv[1:varind])
# Plot histogram of VTI returns
varmin <- (-0.05)
histp <- hist(retp, col="lightgrey",
  xlab="returns", breaks=100, xlim=c(varmin, 0.01),
  ylab="frequency", freq=FALSE, main="VTI Returns Histogram")

# Plot density of losses
densv <- density(retp, adjust=1.5)
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
nstocks <- NROW(symbolv)
retp <- na.omit(rutils::etfenv$returns[, symbolv])
retm <- colMeans(retp)
confl <- 0.05
rmin <- 0 ; wmin <- 0 ; wmax <- 1
weightsum <- 1
ncols <- NCOL(retp) # number of assets
nrows <- NROW(retp) # number of rows
# Create objective vector
objvec <- c(numeric(ncols), rep(-1/(confl/nrows), nrows), -1)
# Specify matrix of linear constraint coefficients
coeffm <- rbind(cbind(rbind(1, retm),
                matrix(data=0, nrow=2, ncol=(nrows+1))),
          cbind(coredata(retp), diag(nrows), 1))
# Specify the logical constraint operators
logop <- c("==", ">=", rep(">=", nrows))
# Specify the vector of constraints
consv <- c(weightvum, rmin, rep(0, nrows))
# Specify box constraints (wmin, wmax) (default is c(0, Inf))
boxc <- list(lower=list(ind=1:ncols, val=rep(wmin, ncols)),
       upper=list(ind=1:ncols, val=rep(wmax, ncols)))
# Perform optimization
optiml <- Rglpk_solve_LP(obj=objvec, mat=coeffm, dir=logop, rhs=consv, types=rep("C", NROW(objvec)), max=T, bounds=boxc)
all.equal(optiml$optimum, sum(objvec*optiml$solution))
coeffm %*% optiml$solution
as.numeric(optiml$solution[1:ncols])

raterf <- 0.03
retp <- c(asset1=0.05, asset2=0.06)
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
#Perform simple optimization for reference
# Objective function for simple optimization
objfun <- function(x) {
  x <- c(x, 1-x)
  t(x) %*% covmat %*% x
}  # end objfun
unlist(optimize(f=objfun, interval=c(-1, 2)))

# Calculate daily ETF percentage returns
symbolv <- c("VTI", "IEF", "DBC")
nstocks <- NROW(symbolv)
retp <- na.omit(rutils::etfenv$returns[, symbolv])
# Calculate the covariance matrix
covmat <- cov(retp)
# Minimum variance weights, with sum equal to 1
optiml <- quadprog::solve.QP(Dmat=2*covmat,
            dvec=numeric(3),
            Amat=matrix(1, nr=3, nc=1),
            bvec=1)
# Minimum variance, maximum returns
optiml <- quadprog::solve.QP(Dmat=2*covmat,
            dvec=apply(0.1*retp, 2, mean),
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
