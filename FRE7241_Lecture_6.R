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

# Calculate the covariance, variances and the mean returns
retp <- na.omit(rutils::etfenv$returns[, c("VTI", "TLT")])
retew <- 0.4*retp[, 1] + 0.6*retp[, 2]
covm <- cov(retp)
covv <- covm[1, 2]
var1 <- covm[1, 1]
var2 <- covm[2, 2]
ret1 <- mean(retp[, 1])
ret2 <- mean(retp[, 2])
# Calculate the inverse covariance matrix
scalev <- (var1*var2 - covv^2)
covinv <- matrix(c(var2, -covv, -covv, var1), nrow=2)/scalev
round(covinv %*% covm, 4)
# Calculate the maximum Sharpe portfolio weights
wv <- (covinv %*% c(ret1, ret2))
# Or
(var2*ret1 - covv*ret2)/scalev
(-covv*ret1 + var1*ret2)/scalev
# Calculate the maximum Sharpe portfolio pnls
pnls <- retp %*% wv
pnls <- sd(retew)/sd(pnls)*pnls
# Calculate the Sharpe ratios
wealthv <- cbind(retew, pnls)
colnames(wealthv) <- c("Balanced", "MaxSharpe")
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of combined log wealth
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Wealth of Fixed Share and Equal Wealth Portfolios") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

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
  xlim=c(0.001, max(stdevs)), xlab="standard deviation", ylab="return")
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
  c(return=sum(weightv*retm),
    stdev=sqrt(drop(weightv %*% covmat %*% weightv)))
})  # end sapply
# Plot scatterplot of random portfolios
stdev <- min(effront["stdev", ])
plot(x=randportf["stdev", ], y=randportf["return", ],
     main="Efficient Frontier and Random Portfolios",
     xlim=c(0.5*stdev, 0.8*max(randportf["stdev", ])),
     xlab="standard deviation", ylab="return")
# Plot maximum Sharpe portfolios
lines(x=effront["stdev", ], y=effront["return", ], lwd=2)
points(x=effront["stdev", ], y=effront["return", ],
 col="red", lwd=3)
# Plot the minimum variance portfolio
points(x=stdevmv, y=retmvm, col="green", lwd=6)
text(stdevmv, retmvm, labels="minimum\nvariance", pos=2, cex=0.8)
# Plot efficient portfolio
points(x=stdevmax, y=retmax, col="green", lwd=6)
text(x=stdevmax, y=retmax, labels="market\nportfolio", pos=2, cex=0.8)

# Plot individual assets
points(x=sqrt(diag(covmat)), y=retm, col="blue", lwd=4)
text(x=sqrt(diag(covmat)), y=retm, labels=names(retm),
     lwd=4, col="blue", pos=1, cex=0.8)

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
widthp <- 6; heightp <- 5
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

# Objective equal to minus ridge Sharpe ratio
objfun <- function(weightv, retp, covret, lambdaf=0, lambdap=1e3) {
  retportf <- retp %*% weightv
  covmat <- covret + lambdaf*diag(ncol(retx))
  stdev <- sqrt(weightv %*% covmat %*% weightv)
  if (stdev == 0)
    return(0)
  else
    return(-mean(retportf)/stdev + lambdap*(sum(weightv)-1)^2)
}  # end objfun
# Calculate ridge covariance matrix of returns and its inverse
covret <- cov(retx)
covmat <- covret + lambdaf*diag(ncol(retx))
covinv <- solve(a=covmat)
# Create initial vector of portfolio weights
weightv <- rep(1, NROW(symbolv))
names(weightv) <- symbolv
# Calculate vector of excess returns
raterf <- 0.03/252
retx <- (retp - raterf)
# Calculate the weights using optimization
lambdaf <- 300.0
objfun(weightv, retp=retx, lambdaf=lambdaf)
optiml <- optim(par=weightv,
          fn=objfun,
          retp=retx,
          covret=covret, Covariance matrix of returns
          lambdaf=lambdaf, Ridge regularization intensity
          lambdap=1e3, Penalty for sum of weights not equal to 1
          method="L-BFGS-B",
          upper=c(10, 10, 10),
          lower=c(-10, -10, -10))
weightv <- optiml$par
names(weightv) <- symbolv
# Calculate the maximum Sharpe weights
retm <- colMeans(retx)
weightms <- drop(covinv %*% retm)/sum(covinv %*% retm)
all.equal(weightv, weightms)

# Objective with regularization penalty
objfun <- function(weightv, retp, lambdaf, alphaf) {
  retportf <- retp %*% weightv
  stdev <- sd(retportf)
  if (stdev == 0)
    return(0)
  else {
    penaltyv <- lambdaf*((1-alphaf)*sum(weightv^2) +
alphaf*sum(abs(weightv)))
    return(-mean(retportf)/stdev + penaltyv)
  }  # end if
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
weightv <- drop(invreg %*% colmeanv)
names(weightv) <- symbolv

# Calculate the weights using optimization without regularization
lambdaf <- 0.0 ; alphaf <- 1.0
optiml <- optim(par=weightv,
          fn=objfun,
          retp=retx,
          covret=covret, Covariance matrix of returns
          lambdaf=lambdaf,
          alphaf=alphaf,
          method="L-BFGS-B",
          control=list(factr=1e5),
          upper=c(10, 10, 10),
          lower=c(-10, -10, -10))
weighto <- optiml$par
names(weighto) <- symbolv
all.equal(weighto, weightv)

# Load S&P 500 stock returns
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
weightv <- drop(invreg %*% colmeanv)
names(weightv) <- symbolv
# Calculate the maximum Sharpe portfolio returns
pnlmaxs <- HighFreq::mult_mat(weightv, retp)
pnlmaxs <- rowMeans(pnlmaxs, na.rm=TRUE)
pnlmaxs <- xts::xts(pnlmaxs, datev)
retew <- xts::xts(rowMeans(retp, na.rm=TRUE), datev)
pnlmaxs <- pnlmaxs*sd(retew["/2014"])/sd(pnlmaxs["/2014"])
# Calculate the regularization portfolio returns
lambdaf <- 1.0 ; alphaf <- 1.0
optiml <- optim(par=weightv,
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

# Select all the ETF symbols except "VXX", "SVXY" "MTUM", "QUAL", "VLUE", "USMV", "AIEQ", and "VYM"
symbolv <- colnames(rutils::etfenv$returns)
symbolv <- symbolv[!(symbolv %in% c("VXX", "SVXY", "MTUM", "QUAL", "VLUE", "USMV", "AIEQ", "VYM"))]
# Extract columns of rutils::etfenv$returns and overwrite NA values
retp <- rutils::etfenv$returns["1999/", symbolv]
sum(is.na(retp)) # Number of NA values
nstocks <- NCOL(retp)
datev <- zoo::index(retp)
# Calculate the covariance ignoring NA values
covmat <- cov(retp, use="pairwise.complete.obs")
sum(is.na(covmat))
# Calculate the inverse of covmat
invmat <- solve(covmat)
round(invmat %*% covmat, digits=5)
# Calculate the generalized inverse of covmat
invreg <- MASS::ginv(covmat)
all.equal(unname(invmat), invreg)

# Create rectangular matrix with collinear columns
matv <- matrix(rnorm(10*8), nc=10)
# Calculate covariance matrix
covmat <- cov(matv)
# Calculate inverse of covmat - error
invmat <- solve(covmat)
# Perform eigen decomposition
eigend <- eigen(covmat)
eigenvec <- eigend$vectors
eigenval <- eigend$values
# Set tolerance for determining zero singular values
precv <- sqrt(.Machine$double.eps)
# Calculate generalized inverse from the eigen decomposition
notzero <- (eigenval > (precv*eigenval[1]))
inveigen <- eigenvec[, notzero] %*%
  (t(eigenvec[, notzero]) / eigenval[notzero])
# Inverse property of inveigen isn't satisfied
all.equal(inveigen %*% covmat, diag(NROW(covmat)))
# Generalized inverse property of inveigen is satisfied
all.equal(covmat %*% inveigen %*% covmat, covmat)
# Calculate generalized inverse using MASS::ginv()
invreg <- MASS::ginv(covmat)
# Verify that inveigen is the same as invreg
all.equal(inveigen, invreg)

# Maximum Sharpe weights in-sample interval
retis <- retp["/2014"]
raterf <- 0.03/252
retx <- (retis - raterf)
invreg <- MASS::ginv(cov(retis, use="pairwise.complete.obs"))
weightv <- drop(invreg %*% colMeans(retx, na.rm=TRUE))
weightv <- weightv/sqrt(sum(weightv^2))
names(weightv) <- colnames(retp)

# Plot portfolio weights
barplot(sort(weightv), main="Maximum Sharpe Weights", cex.names=0.7)

# Calculate the equal weight index
retew <- xts::xts(rowMeans(retp, na.rm=TRUE), datev)
# Calculate the in-sample weighted returns using Rcpp
pnlis <- HighFreq::mult_mat(weightv, retis)
# Or using the transpose
# pnlis <- unname(t(t(retis)*weightv))
pnlis <- rowMeans(pnlis, na.rm=TRUE)
pnlis <- pnlis*sd(retew["/2014"])/sd(pnlis)

# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retew["/2014"], pnlis, (pnlis + retew["/2014"])/2)
colnames(wealthv) <- c("EqualWeight", "Max Sharpe", "Combined")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Dygraph cumulative wealth
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="In-Sample Optimal Portfolio Returns") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", strokeWidth=2) %>%
  dyLegend(width=300)

# Calculate the equal weight index
retos <- retp["2015/"]
# Calculate out-of-sample portfolio returns
pnlos <- HighFreq::mult_mat(weightv, retos)
pnlos <- rowMeans(pnlos, na.rm=TRUE)
pnlos <- pnlos*sd(retew["2015/"])/sd(pnlos)
wealthv <- cbind(retew["2015/"], pnlos, (pnlos + retew["2015/"])/2)
colnames(wealthv) <- c("EqualWeight", "Max Sharpe", "Combined")
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))

# Dygraph cumulative wealth
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Out-of-Sample Optimal Portfolio Returns") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", strokeWidth=2) %>%
  dyLegend(width=300)

# Maximum Sharpe weights in-sample interval
covis <- cov(retis, use="pairwise.complete.obs")
invreg <- MASS::ginv(covis)
weightv <- invreg %*% colMeans(retx, na.rm=TRUE)
names(weightv) <- colnames(retp)
# Calculate cumulative wealth
pnls <- HighFreq::mult_mat(weightv, retp)
pnls <- rowMeans(pnls, na.rm=TRUE)
pnls <- pnls*sd(retew)/sd(pnls)
wealthv <- cbind(retew, pnls, (pnls + retew)/2)
colnames(wealthv) <- c("EqualWeight", "Optimal", "Combined")
# Calculate the in-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv["/2014"], function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))

# Dygraph cumulative wealth
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Out-of-Sample Optimal Portfolio Returns for ETFs") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", strokeWidth=2) %>%
  dyEvent(zoo::index(last(retis[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=400)

# Calculate the eigen decomposition of the in-sample covariance matrix
eigend <- eigen(covis)
eigenvec <- eigend$vectors
eigenval <- eigend$values
# Plot the eigenvalues
barplot(eigenval, main="ETF Covariance Eigenvalues", cex.names=0.7)
# Calculate reduced inverse of covariance matrix
dimax <- 9
invred <- eigenvec[, 1:dimax] %*%
  (t(eigenvec[, 1:dimax]) / eigenval[1:dimax])
# Reduced inverse does not satisfy matrix inverse property
all.equal(covis %*% invred %*% covis, covis)

# Calculate portfolio weights
weightv <- invred %*% colMeans(retx, na.rm=TRUE)
names(weightv) <- colnames(retp)
# Calculate cumulative wealth
pnls <- HighFreq::mult_mat(weightv, retp)
pnls <- rowMeans(pnls, na.rm=TRUE)
pnls <- pnls*sd(retew)/sd(pnls)
wealthv <- cbind(retew, pnls, (pnls + retew)/2)
colnames(wealthv) <- c("EqualWeight", "DimReduction", "Combined")
# Calculate the in-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv["/2014"], function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))

# Dygraph cumulative wealth
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Optimal Portfolio Returns With Dimension Reduction") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", strokeWidth=2) %>%
  dyEvent(zoo::index(last(retis[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=400)

# Shrink the in-sample returns to their mean
alphac <- 0.7
retxm <- rowMeans(retx, na.rm=TRUE)
retxis <- (1-alphac)*retx + alphac*retxm
# Calculate the portfolio weights
weightv <- invred %*% colMeans(retxis, na.rm=TRUE)
# Calculate the cumulative wealth
pnls <- HighFreq::mult_mat(weightv, retp)
pnls <- rowMeans(pnls, na.rm=TRUE)
pnls <- pnls*sd(retew)/sd(pnls)
wealthv <- cbind(retew, pnls, (pnls + retew)/2)
colnames(wealthv) <- c("EqualWeight", "Optimal", "Combined")

# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Dygraph cumulative wealth
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Optimal Portfolio With Dimension Reduction and Return Shrinkage") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", strokeWidth=2) %>%
  dyEvent(zoo::index(last(retis[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=300)

# Define monthly end and start points
retx <- (retp - raterf)
endd <- rutils::calc_endpoints(retp, interval="months")
endd <- endd[endd > (nstocks+1)]
npts <- NROW(endd)
lookb <- 3
startp <- c(rep_len(0, lookb), endd[1:(npts-lookb)])
# Perform loop over end points
pnls <- lapply(1:(npts-1), function(tday) {
    # Calculate the portfolio weights
    retis <- retx[startp[tday]:endd[tday], ]
    covmat <- cov(retis, use="pairwise.complete.obs")
    covmat[is.na(covmat)] <- 0
    invreg <- MASS::ginv(covmat)
    colm <- colMeans(retis, na.rm=TRUE)
    colm[is.na(colm)] <- 0
    weightv <- invreg %*% colm
    # Calculate the in-sample portfolio returns
    pnlis <- HighFreq::mult_mat(weightv, retis)
    pnlis <- rowMeans(pnlis, na.rm=TRUE)
    # Calculate the out-of-sample portfolio returns
    retos <- retp[(endd[tday]+1):endd[tday+1], ]
    pnlos <- HighFreq::mult_mat(weightv, retos)
    pnlos <- rowMeans(pnlos, na.rm=TRUE)
    xts::xts(pnlos, zoo::index(retos))
})  # end lapply
pnls <- do.call(rbind, pnls)
pnls <- rbind(retew[paste0("/", start(pnls)-1)], pnls)

# Calculate the Sharpe and Sortino ratios
pnls <- pnls*sd(retew)/sd(pnls)
wealthv <- cbind(retew, pnls, (pnls + retew)/2)
colnames(wealthv) <- c("EqualWeight", "PortfStrat", "Combined")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))

# Dygraph cumulative wealth
dygraphs::dygraph(cumsum(wealthv)[endw], main="Monthly ETF Portfolio Momentum Strategy") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Perform loop over end points
dimax <- 9
pnls <- lapply(1:(npts-1), function(tday) {
    # Calculate the portfolio weights
    retis <- retx[startp[tday]:endd[tday], ]
    covmat <- cov(retis, use="pairwise.complete.obs")
    covmat[is.na(covmat)] <- 0
    eigend <- eigen(covmat)
    eigenvec <- eigend$vectors
    eigenval <- eigend$values
    invred <- eigenvec[, 1:dimax] %*%
(t(eigenvec[, 1:dimax]) / eigenval[1:dimax])
    colm <- colMeans(retis, na.rm=TRUE)
    colm[is.na(colm)] <- 0
    weightv <- invred %*% colm
    # Calculate the in-sample portfolio returns
    pnlis <- HighFreq::mult_mat(weightv, retis)
    pnlis <- rowMeans(pnlis, na.rm=TRUE)
    weightv <- weightv*0.01/sd(pnlis)
    # Calculate the out-of-sample portfolio returns
    retos <- retp[(endd[tday]+1):endd[tday+1], ]
    pnlos <- HighFreq::mult_mat(weightv, retos)
    pnlos <- rowMeans(pnlos, na.rm=TRUE)
    xts::xts(pnlos, zoo::index(retos))
})  # end lapply
pnls <- do.call(rbind, pnls)
pnls <- rbind(retew[paste0("/", start(pnls)-1)], pnls)

# Calculate the Sharpe and Sortino ratios
pnls <- pnls*sd(retew)/sd(pnls)
wealthv <- cbind(retew, pnls, (pnls + retew)/2)
colnames(wealthv) <- c("EqualWeight", "PortfStrat", "Combined")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Dygraph cumulative wealth
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Portfolio Momentum Strategy With Dimension Reduction") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", strokeWidth=2) %>%
  dyLegend(show="always", width=300)

alphac <- 0.7 # Return shrinkage intensity
# Perform loop over end points
pnls <- lapply(1:(npts-1), function(tday) {
    # Shrink the in-sample returns to their mean
    retis <- retx[startp[tday]:endd[tday], ]
    rowm <- rowMeans(retis, na.rm=TRUE)
    rowm[is.na(rowm)] <- 0
    retis <- (1-alphac)*retis + alphac*rowm
    # Calculate the portfolio weights
    covmat <- cov(retis, use="pairwise.complete.obs")
    covmat[is.na(covmat)] <- 0
    eigend <- eigen(covmat)
    eigenvec <- eigend$vectors
    eigenval <- eigend$values
    invred <- eigenvec[, 1:dimax] %*%
(t(eigenvec[, 1:dimax]) / eigenval[1:dimax])
    colm <- colMeans(retis, na.rm=TRUE)
    colm[is.na(colm)] <- 0
    weightv <- invred %*% colm
    # Scale the weights to volatility target
    pnlis <- HighFreq::mult_mat(weightv, retis)
    pnlis <- rowMeans(pnlis, na.rm=TRUE)
    weightv <- weightv*0.01/sd(pnlis)
    # Calculate the out-of-sample portfolio returns
    retos <- retp[(endd[tday]+1):endd[tday+1], ]
    pnlos <- HighFreq::mult_mat(weightv, retos)
    pnlos <- rowMeans(pnlos, na.rm=TRUE)
    xts::xts(pnlos, zoo::index(retos))
})  # end lapply
pnls <- do.call(rbind, pnls)
pnls <- rbind(retew[paste0("/", start(pnls)-1)], pnls)

# Calculate the Sharpe and Sortino ratios
pnls <- pnls*sd(retew)/sd(pnls)
wealthv <- cbind(retew, pnls, (pnls + retew)/2)
colnames(wealthv) <- c("EqualWeight", "PortfStrat", "Combined")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Dygraph cumulative wealth
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Portfolio Momentum Strategy With Return Shrinkage") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Define backtest functional for portfolio momentum strategy
roll_portf <- function(retx, # Excess returns
                 retp, # Stock returns
                 endd, # End points
                 lookb=12, # Look-back interval
                 dimax=3, # Dimension reduction parameter
                 alphac=0.0, # Return shrinkage intensity
                 bidask=0.0, # Bid-offer spread
                 ...) {
  npts <- NROW(endd)
  startp <- c(rep_len(0, lookb), endd[1:(npts-lookb)])
  pnls <- lapply(1:(npts-1), function(tday) {
    retis <- retx[startp[tday]:endd[tday], ]
    # Shrink the in-sample returns to their mean
    if (alphac > 0) {
rowm <- rowMeans(retis, na.rm=TRUE)
rowm[is.na(rowm)] <- 0
retis <- (1-alphac)*retis + alphac*rowm
    } # end if
    # Calculate the portfolio weights
    covmat <- cov(retis, use="pairwise.complete.obs")
    covmat[is.na(covmat)] <- 0
    eigend <- eigen(covmat)
    eigenvec <- eigend$vectors
    eigenval <- eigend$values
    invred <- eigenvec[, 1:dimax] %*% (t(eigenvec[, 1:dimax]) / eigenval[1:dimax])
    colm <- colMeans(retis, na.rm=TRUE)
    colm[is.na(colm)] <- 0
    weightv <- invred %*% colm
    # Scale the weights to volatility target
    pnlis <- HighFreq::mult_mat(weightv, retis)
    pnlis <- rowMeans(pnlis, na.rm=TRUE)
    weightv <- weightv*0.01/sd(pnlis)
    # Calculate the out-of-sample portfolio returns
    retos <- retp[(endd[tday]+1):endd[tday+1], ]
    pnlos <- HighFreq::mult_mat(weightv, retos)
    pnlos <- rowMeans(pnlos, na.rm=TRUE)
    xts::xts(pnlos, zoo::index(retos))
  })  # end lapply
  pnls <- do.call(rbind, pnls)
  # Add warmup period to pnls
  rbind(retew[paste0("/", start(pnls)-1)], pnls)
}  # end roll_portf

# Simulate a monthly ETF portfolio strategy
pnls <- roll_portf(retx=retx, retp=retp, endd=endd,
  lookb=lookb, dimax=dimax)
# Perform sapply loop over lookbv
lookbv <- seq(6, 13, by=1)
library(parallel)  # Load package parallel
ncores <- detectCores() - 1
pnls <- mclapply(lookbv, roll_portf, retp=retp, retx=retx,
  endd=endd, dimax=dimax, mc.cores=ncores)
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("lookb=", lookbv)
profilev <- sapply(pnls, sum)
lookb <- lookbv[which.max(profilev)]

# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(pnls, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of monthly ETF portfolio strategies
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls)[endw], main="Portfolio Momentum Strategies") %>%
  dyOptions(colors=colorv, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Plot portfolio strategies using quantmod
themev <- chart_theme()
themev$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(pnls))
quantmod::chart_Series(cumsum(pnls),
  theme=themev, name="Portfolio Momentum Strategies")
legend("bottomleft", legend=colnames(pnls),
  inset=0.02, bg="white", cex=0.7, lwd=rep(6, NCOL(retp)),
  col=themev$col$line.col, bty="n")

# Perform backtest for different dimax values
dimv <- 3:9
pnls <- mclapply(dimv, roll_portf, retp=retp, retx=retx,
  endd=endd, lookb=lookb, mc.cores=ncores)
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("dim=", dimv)
profilev <- sapply(pnls, sum)
dimax <- dimv[which.max(profilev)]

# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(pnls, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of monthly ETF portfolio strategies
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls)[endw],
  main="Portfolio Momentum Strategies With Dimension Reduction") %>%
  dyOptions(colors=colorv, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Plot portfolio strategies using quantmod
themev <- chart_theme()
themev$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(pnls))
quantmod::chart_Series(cumsum(pnls),
  theme=themev, name="Portfolio Momentum Strategies")
legend("bottomleft", legend=colnames(pnls),
  inset=0.02, bg="white", cex=0.7, lwd=rep(6, NCOL(retp)),
  col=themev$col$line.col, bty="n")

# Load stock returns
load("/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
datev <- zoo::index(na.omit(retstock$GOOGL))
retp <- retstock[datev] # Subset the returns to GOOGL
# Remove the stocks with any NA values
numna <- sapply(retp, function(x) sum(is.na(x)))
retp <- retp[, numna==0]
nstocks <- NCOL(retp)
datev <- zoo::index(retp)
symbolv <- colnames(retp)
retis <- retp["/2014"] # In-sample returns
raterf <- 0.03/252
retx <- (retis - raterf) # Excess returns
# Maximum Sharpe weights in-sample interval
covmat <- cov(retis, use="pairwise.complete.obs")
invreg <- MASS::ginv(covmat)
colmeanv <- colMeans(retx, na.rm=TRUE)
weightv <- drop(invreg %*% colmeanv)
names(weightv) <- symbolv
head(sort(weightv))
tail(sort(weightv))
# Calculate the portfolio returns
pnls <- HighFreq::mult_mat(weightv, retp)
pnls <- rowMeans(pnls, na.rm=TRUE)
pnls <- xts::xts(pnls, datev)
retew <- xts::xts(rowMeans(retp, na.rm=TRUE), datev)
pnls <- pnls*sd(retew["/2014"])/sd(pnls["/2014"])
wealthv <- cbind(retew, pnls)
colnames(wealthv) <- c("EqualWeight", "MaxSharpe")

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
  dyEvent(zoo::index(last(retis[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=300)

# Calculate reduced inverse of covariance matrix
dimax <- 10
eigend <- eigen(covmat)
eigenvec <- eigend$vectors
eigenval <- eigend$values
invred <- eigenvec[, 1:dimax] %*%
  (t(eigenvec[, 1:dimax]) / eigenval[1:dimax])
# Calculate portfolio weights and PnLs
colmeanv <- colMeans(retx["/2014"], na.rm=TRUE)
weightv <- invred %*% colmeanv
pnls <- HighFreq::mult_mat(weightv, retp)
pnls <- rowMeans(pnls, na.rm=TRUE)
pnls <- xts::xts(pnls, datev)
pnls <- pnls*sd(retew["/2014"])/sd(pnls["/2014"])

# Combine with equal weight
wealthv <- cbind(retew, pnls)
colnames(wealthv) <- c("EqualWeight", "MaxSharpe")
# Calculate the in-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv["/2014"], function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv["2015/"], function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of cumulative portfolio returns
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Maximum Sharpe With Dimension Reduction") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyEvent(zoo::index(last(retis[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=300)

# Perform loop over vector of dimension reduction parameters
dimv <- seq(from=5, to=11)
pnls <- mclapply(dimv, function(dimax) {
  invred <- eigenvec[, 1:dimax] %*%
    (t(eigenvec[, 1:dimax]) / eigenval[1:dimax])
  # Calculate portfolio weights and PnLs
  weightv <- invred %*% colmeanv
  pnls <- HighFreq::mult_mat(weightv, retp)
  pnls <- rowMeans(pnls, na.rm=TRUE)
  pnls <- xts::xts(pnls, datev)
  pnls*sd(retew["/2014"])/sd(pnls["/2014"])
}, mc.cores=ncores)  # end mclapply
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("dim=", dimv)
profilev <- sapply(pnls["2015/"], sum)
dimax <- dimv[which.max(profilev)]

# Plot of cumulative portfolio returns
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
endw <- rutils::calc_endpoints(pnls["2015/"], interval="weeks")
dygraphs::dygraph(cumsum(pnls["2015/"])[endw],
  main="Out-of-Sample PnLs Wih Dimension Reduction") %>%
  dyOptions(colors=colorv, strokeWidth=1) %>%
  dyLegend(show="always", width=300)

# Shrink the in-sample returns to their mean
alphac <- 0.3
retxm <- rowMeans(retx, na.rm=TRUE)
retxis <- (1-alphac)*retx + alphac*retxm
# Calculate portfolio weights and PnLs
weightv <- drop(invred %*% colMeans(retxis, na.rm=TRUE))
names(weightv) <- symbolv
pnls <- HighFreq::mult_mat(weightv, retp)
pnls <- rowMeans(pnls, na.rm=TRUE)
pnls <- xts::xts(pnls, datev)
pnls <- pnls*sd(retew["/2014"])/sd(pnls["/2014"])

# Combine with equal weight
wealthv <- cbind(retew, pnls)
colnames(wealthv) <- c("EqualWeight", "MaxSharpe")
# Calculate the in-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv["/2014"], function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv["2015/"], function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of cumulative portfolio returns
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Maximum Sharpe With Return Shrinkage") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyEvent(zoo::index(last(retis[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=300)

# Perform loop over vector of shrinkage intensities
alphav <- seq(from=0.5, to=0.9, by=0.1)
pnls <- mclapply(alphav, function(alphac) {
  retxis <- (1-alphac)*retx + alphac*retxm
  weightv <- drop(invred %*% colMeans(retxis, na.rm=TRUE))
  pnls <- HighFreq::mult_mat(weightv, retp)
  pnls <- rowMeans(pnls, na.rm=TRUE)
  pnls <- xts::xts(pnls, datev)
  pnls*sd(retew["/2014"])/sd(pnls["/2014"])
}, mc.cores=ncores)  # end mclapply
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("alpha=", alphav)
profilev <- sapply(pnls["2015/"], sum)
alphac <- alphav[which.max(profilev)]

# Plot of cumulative portfolio returns
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
endw <- rutils::calc_endpoints(pnls["2015/"], interval="weeks")
dygraphs::dygraph(cumsum(pnls["2015/"])[endw],
  main="Out-of-Sample PnLs Wih Shrinkage") %>%
  dyOptions(colors=colorv, strokeWidth=1) %>%
  dyLegend(show="always", width=300)

# Define monthly end points
endd <- rutils::calc_endpoints(retp, interval="months")
endd <- endd[endd > (nstocks+1)]
npts <- NROW(endd) ; lookb <- 12
startp <- c(rep_len(0, lookb), endd[1:(npts-lookb)])
# !!! Perform parallel loop over end points - takes very long!!!
library(parallel)  # Load package parallel
ncores <- detectCores() - 1
pnls <- mclapply(1:(npts-1), function(tday) {
    # Subset the excess returns
    retis <- retp[startp[tday]:endd[tday], ]
    retis[is.na(retis)] <- 0
    invreg <- MASS::ginv(cov(retis, use="pairwise.complete.obs"))
    # Calculate the maximum Sharpe ratio portfolio weights
    retm <- colMeans(retis, na.rm=TRUE)
    weightv <- invreg %*% retm
    # Zero weights if sparse data
    zerov <- sapply(retis, function(x) (sum(x == 0) > 5))
    weightv[zerov] <- 0
    # Calculate in-sample portfolio returns
    pnlis <- (retis %*% weightv)
    # Scale the weights to the in-sample volatility
    weightv <- weightv*sd(retm)/sd(pnlis)
    # Calculate the out-of-sample portfolio returns
    retos <- retp[(endd[tday]+1):endd[tday+1], ]
    pnls <- HighFreq::mult_mat(weightv, retos)
    pnls <- rowMeans(pnls, na.rm=TRUE)
    xts::xts(pnls, zoo::index(retos))
}, mc.cores=ncores)  # end mclapply
pnls <- rutils::do_call(rbind, pnls)
pnls <- rbind(retew[paste0("/", start(pnls)-1)], pnls*sd(retew)/sd(pnls))

# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retew, pnls)
colnames(wealthv) <- c("EqualWeight", "MaxSharpe")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))

# Plot cumulative strategy returns
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Rolling Maximum Sharpe Portfolio Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Create random matrix of returns
matv <- matrix(rnorm(300), nc=5)
# Reduced inverse of covariance matrix
dimax <- 3
eigend <- eigen(covmat)
invred <- eigend$vectors[, 1:dimax] %*%
  (t(eigend$vectors[, 1:dimax]) / eigend$values[1:dimax])
# Reduced inverse using RcppArmadillo
invarma <- HighFreq::calc_inv(covmat, dimax)
all.equal(invred, invarma)
# Microbenchmark RcppArmadillo code
library(microbenchmark)
summary(microbenchmark(
  rcode={eigend <- eigen(covmat)
    eigend$vectors[, 1:dimax] %*%
(t(eigend$vectors[, 1:dimax]) / eigend$values[1:dimax])
  },
  rcpp=calc_inv(covmat, dimax),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# Shift the end points to C++ convention
endd <- (endd - 1)
endd[endd < 0] <- 0
startp <- (startp - 1)
startp[startp < 0] <- 0
# Specify dimension reduction and return shrinkage using list of portfolio optimization parameters
dimax <- 9
alphac <- 0.8
controll <- HighFreq::param_portf(method="maxsharpe",
  dimax=dimax, alpha=alphac)
# Perform backtest in Rcpp - takes very long!!!
retx <- (retp - raterf)
pnls <- HighFreq::roll_portf(retx=retx, retp=retp,
  startp=startp, endd=endd, controll=controll)
pnls <- pnls*sd(retew)/sd(pnls)

# Calculate the out-of-sample Sharpe and Sortino ratios
wealthv <- cbind(retew, pnls, (pnls + retew)/2)
colnames(wealthv) <- c("EqualWeight", "MaxSharpe", "Combined")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot cumulative strategy returns
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Rolling S&P500 Portfolio Strategy With Shrinkage") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", label="Combined", strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Perform backtest over vector of dimension reduction parameters
dimv <- seq(from=3, to=21, by=2)
pnls <- mclapply(dimv, function(dimax) {
  controll <- HighFreq::param_portf(method="maxsharpe",
    dimax=dimax, alpha=alphac)
  HighFreq::roll_portf(retx=retx, retp=retp,
    startp=startp, endd=endd, controll=controll)
}, mc.cores=ncores)  # end mclapply
profilev <- sapply(pnls, sum)
dimax <- dimv[which.max(profilev)]

# Plot of rolling strategy PnL as a function of dimax
plot(x=dimv, y=profilev, t="l", xlab="dimax", ylab="pnl",
  main="Rolling Strategy PnL as Function of dimax")

# Perform backtest over vector of shrinkage intensities
alphav <- seq(from=0.5, to=0.9, by=0.1)
pnls <- mclapply(alphav, function(alphac) {
  controll <- HighFreq::param_portf(method="maxsharpe",
      dimax=dimax, alpha=alphac)
  HighFreq::roll_portf(retx=retx, retp=retp,
      startp=startp, endd=endd, controll=controll)
}, mc.cores=ncores)  # end mclapply
profilev <- sapply(pnls, sum)
alphac <- alphav[which.max(profilev)]

# Plot of rolling strategy PnL as a function of shrinkage intensity
plot(x=alphav, y=profilev, t="l",
  main="Rolling Strategy PnL as Function of Return Shrinkage",
  xlab="Shrinkage Intensity Alpha", ylab="pnl")

# Create list of model parameters
controll <- HighFreq::param_portf(method="maxsharpe",
      dimax=dimax, alpha=alphac)
# Perform backtest over look-backs
lookbv <- seq(from=5, to=16, by=1)
pnls <- mclapply(lookbv, function(lookb) {
  startp <- c(rep_len(0, lookb), endd[1:(npts-lookb)])
  startp <- (startp - 1) ; startp[startp < 0] <- 0
  HighFreq::roll_portf(retx=retx, retp=retp,
    startp=startp, endd=endd, controll=controll)
}, mc.cores=ncores)  # end mclapply
profilev <- sapply(pnls, sum)
lookb <- lookbv[which.max(profilev)]

# Plot of rolling strategy PnL as a function of look-back interval
plot(x=lookbv, y=profilev, t="l", main="MaxSharpe PnL as Function of Look-back Interval",
  xlab="Look-back Interval", ylab="pnl")

# Calculate the out-of-sample Sharpe and Sortino ratios
pnls <- pnls[[whichmax]]
pnls <- pnls*sd(retew)/sd(pnls)
wealthv <- cbind(retew, pnls, (pnls + retew)/2)
colnames(wealthv) <- c("EqualWeight", "MaxSharpe", "Combined")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Dygraph the cumulative wealth
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Optimal Maximum Sharpe Portfolio Strategy") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", label="Combined", strokeWidth=2) %>%
  dyLegend(show="always", width=300)
