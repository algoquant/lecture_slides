library(PortfolioAnalytics)
# Use ETF returns from package rutils
library(rutils)
symbolv <- c("VTI", "IEF", "DBC", "XLF",
  "VNQ", "XLP", "XLV", "XLU", "XLB", "XLE")
# Initial portfolio to equal weights
portfi <- rep(1/NROW(symbolv), NROW(symbolv))
# Named vector
names(portfi) <- symbolv
# Create portfolio object
portfi <- portfolio.spec(assets=portfi)
library(PortfolioAnalytics)
# Add constraints
portf_maxSR <- add.constraint(
  portfolio=portfi,  # Initial portfolio
  type="weightsum",  # Constraint sum weights
  min_sum=0.9, max_sum=1.1)
# Add constraints
portf_maxSR <- add.constraint(
  portfolio=portf_maxSR,
  type="long_only")  # box constraint min=0, max=1
# Add objectives
portf_maxSR <- add.objective(
  portfolio=portf_maxSR,
  type="return",  # Maximize mean return
  name="mean")
# Add objectives
portf_maxSR <- add.objective(
  portfolio=portf_maxSR,
  type="risk",  # Minimize stdev
  name="stdev")
library(PortfolioAnalytics)
# Use ETF returns from package rutils
library(rutils)
symbolv <- c("VTI", "IEF", "DBC", "XLF",
  "VNQ", "XLP", "XLV", "XLU", "XLB", "XLE")
# Initial portfolio to equal weights
portfi <- rep(1/NROW(symbolv), NROW(symbolv))
# Named vector
names(portfi) <- symbolv
# Create portfolio object
portfi <- portfolio.spec(assets=portfi)
library(PortfolioAnalytics)
# Add constraints
portf_maxSR <- add.constraint(
  portfolio=portfi,  # Initial portfolio
  type="weightsum",  # Constraint sum weights
  min_sum=0.9, max_sum=1.1)
# Add constraints
portf_maxSR <- add.constraint(
  portfolio=portf_maxSR,
  type="long_only")  # box constraint min=0, max=1
# Add objectives
portf_maxSR <- add.objective(
  portfolio=portf_maxSR,
  type="return",  # Maximize mean return
  name="mean")
# Add objectives
portf_maxSR <- add.objective(
  portfolio=portf_maxSR,
  type="risk",  # Minimize stdev
  name="stdev")
# Calculate daily stock returns
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
sqrt(252)*sapply(wealthv,
  function(x) c(Sharpe=(mean(x)-raterf)/sd(x), Sortino=(mean(x)-raterf)/sd(x[x<0])))
# Plot the log wealth
endd <- rutils::calc_endpoints(retp, interval="weeks")
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
# Vector of symbol names
symbolv <- c("VTI", "IEF", "DBC")
nstocks <- NROW(symbolv)
# Calculate random portfolios
nportf <- 1000
randportf <- sapply(1:nportf, function(it) {
  weightv <- runif(nstocks, min=0, max=10)
  weightv <- weightv/sum(weightv)
  retp <- rutils::etfenv$returns[, symbolv] %*% weightv
  100*c(ret=mean(retp), sd=sd(retp))
})  # end sapply
# Plot scatterplot of random portfolios
x11(width=6, height=5)
plot(x=randportf[2, ], y=randportf[1, ], xlim=c(0, max(randportf[2, ])),
     main="Random portfolios",
     ylim=c(min(0, min(randportf[1, ])), max(randportf[1, ])),
     xlab=rownames(randportf)[2], ylab=rownames(randportf)[1])
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
# Calculate daily percentage returns
symbolv <- c("VTI", "IEF", "DBC")
nstocks <- NROW(symbolv)
retp <- na.omit(rutils::etfenv$returns[, symbolv])
# Create initial vector of portfolio weights
weightv <- rep(1, NROW(symbolv))
names(weightv) <- symbolv
# Objective equal to minus Sharpe ratio
objfun <- function(weightv, retp) {
  retp <- retp %*% weightv
  if (sd(retp) == 0)
    return(0)
  else
    -return(mean(retp)/sd(retp))
}  # end objfun
# Objective for equal weight portfolio
objfun(weightv, retp=retp)
optiml <- unlist(optimize(f=function(weightv)
    objfun(c(1, 1, weightv), retp=retp),
  interval=c(-4, 1)))
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
title(main="Objective Function", line=(-1))  # Add title
points(x=optiml[1], y=optiml[2], col="green", lwd=6)
text(x=optiml[1], y=optiml[2],
     labels="minimum objective", pos=4, cex=0.8)
#below is simplified code for plotting objective function
# Create vector of DBC weights
weightv <- seq(from=-4, to=1, by=0.1)
objv <- sapply(weightv, function(weightv)
  objfun(c(1, 1, weightv)))
plot(x=weightv, y=objv, t="l",
xlab="weight of DBC", ylab="", lwd=2)
title(main="Objective Function", line=(-1))  # Add title
points(x=optiml[1], y=optiml[2], col="green", lwd=6)
text(x=optiml[1], y=optiml[2],
     labels="minimum objective", pos=4, cex=0.8)
# Vectorize function with respect to all weights
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
  x=function(w2, w3) {-objvec(w1=1, w2, w3)},
  xlim=c(-3, 7), ylim=c(-5, 5),
  col="green", axes=FALSE)
# Render the 3d surface plot of function
rgl::rglwidget(elementId="plot3drgl", width=1000, height=1000)
# Vector of initial portfolio weights equal to 1
weightv <- rep(1, nstocks)
names(weightv) <- symbolv
# Objective function equal to standard deviation of returns
objfun <- function(weightv) {
  retp <- retp %*% weightv
  sd(retp)/sum(weightv)
}  # end objfun
# objfun() for equal weight portfolio
objfun(weightv)
objfun(2*weightv)
# Perform portfolio optimization
optiml <- optim(par=weightv,
          fn=objfun,
          method="L-BFGS-B",
          upper=rep(10, nstocks),
          lower=rep(-10, nstocks))
# Rescale the optimal weights
weightv <- optiml$par/sum(optiml$par)
# Minimum variance portfolio returns
retsoptim <- xts(x=retp %*% weightv,
            order.by=zoo::index(retp))
chart_Series(x=exp(cumsum(retsoptim)), name="minvar portfolio")
# Add green point for minimum variance portfolio
optim_sd <- 100*sd(retsoptim)
optim_ret <- 100*mean(retsoptim)
points(x=optim_sd, y=optim_ret, col="green", lwd=6)
text(x=optim_sd, y=optim_ret, labels="minvar", pos=2, cex=0.8)
# Objective function equal to minus Sharpe ratio
raterf <- 0.03
objfun <- function(weightv) {
  retp <- 100*rutils::etfenv$returns[, names(weightv)] %*% weightv / sum(weightv)
  -mean(retp-raterf)/sd(retp)
}  # end objfun
# Perform portfolio optimization
optiml <- optim(par=weightv,
             fn=objfun,
             method="L-BFGS-B",
             upper=rep(10, nstocks),
             lower=rep(-10, nstocks))
# Maximum Sharpe ratio portfolio returns
weightv <- optiml$par/sum(optiml$par)
retsoptim <- xts(x=retp %*% weightv,
            order.by=zoo::index(retp))
chart_Series(x=exp(cumsum(retsoptim)), name="maxSR portfolio")
optim_sd <- 100*sd(retsoptim)
optim_ret <- 100*mean(retsoptim)
points(x=optim_sd, y=optim_ret,
 col="blue", lwd=3)
text(x=optim_sd, y=optim_ret,
     labels="maxSR", pos=2, cex=0.8)
sharpem <- (optim_ret-raterf)/optim_sd
# Plot individual assets
retm <- 100*sapply(retp, mean)
stdevs <- 100*sapply(retp, sd)
points(stdevs, retm, col="green", lwd=3)
text(stdevs, retm, labels=names(retm), pos=2, cex=0.8)
# Add point at risk-free rate and draw Capital Market Line
points(x=0, y=raterf)
text(0, raterf, labels="risk-free", pos=4, cex=0.8)
abline(a=raterf, b=sharpem, col="blue")
rangev <- par("usr")
text(optim_sd/3, (optim_ret+raterf)/2.5,
     labels="Capital Market Line", cex=0.8, , pos=3,
     srt=45*atan(sharpem*(rangev[2]-rangev[1])/
             (rangev[4]-rangev[3])*
             heightp/widthp)/(0.25*pi))
# Create initial vector of portfolio weights
weightv <- rep(1, NROW(symbolv))
names(weightv) <- symbolv
# Optimization to find weights with maximum Sharpe ratio
optiml <- optim(par=weightv,
             fn=objfun,
             retp=retp,
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
retc <- lapply(retp,
  function(retp) exp(cumsum(retp)))
retc <- rutils::do_call(cbind, retc)
# Calculate optimal portfolio returns with VTI, IEF, DBC
retsoptim <- cbind(
  exp(cumsum(retp %*% optiml$par)),
  retc)
colnames(retsoptim)[1] <- "retsoptim"
# Plot optimal returns with VTI, IEF, DBC
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("black", "red", "green", "blue")
chart_Series(retsoptim, theme=plot_theme,
       name="Optimized portfolio performance")
legend("top", legend=colnames(retsoptim), cex=1.0,
   inset=0.1, bg="white", lty=1, lwd=6,
   col=plot_theme$col$line.col, bty="n")
# Or plot non-compounded (simple) cumulative returns
PerformanceAnalytics::chart.CumReturns(
  cbind(retp %*% optiml$par, retp),
  lwd=2, ylab="", legend.loc="topleft", main="")
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
Perform simple optimization for reference
# Objective function for simple optimization
objfun <- function(x) {
  x <- c(x, 1-x)
  t(x) %*% covmat %*% x
}  # end objfun
unlist(optimize(f=objfun, interval=c(-1, 2)))
# Calculate daily percentage returns
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
# Rastrigin function with vector argument for optimization
rastrigin <- function(vecv, param=25){
  sum(vecv^2 - param*cos(vecv))
}  # end rastrigin
vecv <- c(pi/6, pi/6)
rastrigin(vecv=vecv)
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
symbolv <- c("VTI", "IEF", "DBC")
nstocks <- NROW(symbolv)
retp <- na.omit(rutils::etfenv$returns[, symbolv])
# Objective equal to minus Sharpe ratio
objfun <- function(weightv, retp) {
  retp <- retp %*% weightv
  if (sd(retp) == 0)
    return(0)
  else
    -return(mean(retp)/sd(retp))
}  # end objfun
# Perform optimization using DEoptim
optiml <- DEoptim::DEoptim(fn=objfun,
  upper=rep(10, NCOL(retp)),
  lower=rep(-10, NCOL(retp)),
  retp=retp,
  control=list(trace=FALSE, itermax=100, parallelType=1))
weightv <- optiml$optim$bestmem/sum(abs(optiml$optim$bestmem))
names(weightv) <- colnames(retp)
# Objective with shrinkage penalty
objfun <- function(weightv, retp, lambdaf, alpha) {
  retp <- retp %*% weightv
  if (sd(retp) == 0)
    return(0)
  else {
    penaltyv <- lambdaf*((1-alpha)*sum(weightv^2) +
alpha*sum(abs(weightv)))
    -return(mean(retp)/sd(retp) + penaltyv)
  }
}  # end objfun
# Objective for equal weight portfolio
weightv <- rep(1, NROW(symbolv))
names(weightv) <- symbolv
lambdaf <- 0.5 ; alpha <- 0.5
objfun(weightv, retp=retp, lambdaf=lambdaf, alpha=alpha)
# Perform optimization using DEoptim
optiml <- DEoptim::DEoptim(fn=objfun,
  upper=rep(10, NCOL(retp)),
  lower=rep(-10, NCOL(retp)),
  retp=retp,
  lambdaf=lambdaf,
  alpha=alpha,
  control=list(trace=FALSE, itermax=100, parallelType=1))
weightv <- optiml$optim$bestmem/sum(abs(optiml$optim$bestmem))
names(weightv) <- colnames(retp)
# Objective with shrinkage penalty
objfun <- function(weightv, retp, lambdaf, alpha) {
  retp <- retp %*% weightv
  if (sd(retp) == 0)
    return(0)
  else {
    penaltyv <- lambdaf*((1-alpha)*sum(weightv^2) +
alpha*sum(abs(weightv)))
    -return(mean(retp)/sd(retp) + penaltyv)
  }
}  # end objfun
# Objective for equal weight portfolio
weightv <- rep(1, NROW(symbolv))
names(weightv) <- symbolv
lambdaf <- 0.5 ; alpha <- 0.5
objfun(weightv, retp=retp, lambdaf=lambdaf, alpha=alpha)
# Perform optimization using DEoptim
optiml <- DEoptim::DEoptim(fn=objfun,
  upper=rep(10, NCOL(retp)),
  lower=rep(-10, NCOL(retp)),
  retp=retp,
  lambdaf=lambdaf,
  alpha=alpha,
  control=list(trace=FALSE, itermax=100, parallelType=1))
weightv <- optiml$optim$bestmem/sum(abs(optiml$optim$bestmem))
names(weightv) <- colnames(retp)
# Portfolio optimization
library(PortfolioAnalytics)  # load package "PortfolioAnalytics"
# get documentation for package "PortfolioAnalytics"
packageDescription("PortfolioAnalytics")  # get short description
help(package="PortfolioAnalytics")  # load help page
data(package="PortfolioAnalytics")  # list all datasets in "PortfolioAnalytics"
ls("package:PortfolioAnalytics")  # list all objects in "PortfolioAnalytics"
detach("package:PortfolioAnalytics")  # remove PortfolioAnalytics from search path
library(PortfolioAnalytics)
# Use ETF returns from package rutils
library(rutils)
symbolv <- c("VTI", "IEF", "DBC", "XLF",
  "VNQ", "XLP", "XLV", "XLU", "XLB", "XLE")
# Initial portfolio to equal weights
portfi <- rep(1/NROW(symbolv), NROW(symbolv))
# named vector
names(portfi) <- symbolv
# Create portfolio object
portfi <- portfolio.spec(assets=portfi)
library(PortfolioAnalytics)
# Add constraints
portf_maxSR <- add.constraint(
  portfolio=portfi,  # Initial portfolio
  type="weightsum",  # Constraint sum weights
  min_sum=0.9, max_sum=1.1)
# Add constraints
portf_maxSR <- add.constraint(
  portfolio=portf_maxSR,
  type="long_only")  # box constraint min=0, max=1
# Add objectives
portf_maxSR <- add.objective(
  portfolio=portf_maxSR,
  type="return",  # Maximize mean return
  name="mean")
# Add objectives
portf_maxSR <- add.objective(
  portfolio=portf_maxSR,
  type="risk",  # Minimize stdev
  name="stdev")
load(file="/Users/jerzy/Develop/lecture_slides/data/portf_optim.RData")
library(PortfolioAnalytics)
# Perform optimization of weights
maxSR_DEOpt <- optimize.portfolio(
  R=rutils::etfenv$returns[, symbolv],  # Specify returns
  portfolio=portf_maxSR,  # Specify portfolio
  optimize_method="DEoptim", # Use DEoptim
  maxSR=TRUE,  # Maximize Sharpe
  trace=TRUE, traceDE=0)
# Plot optimization
chart.RiskReward(maxSR_DEOpt,
  risk.col="stdev",
  return.col="mean")
options(width=50)
library(PortfolioAnalytics)
load(file="/Users/jerzy/Develop/lecture_slides/data/portf_optim.RData")
maxSR_DEOpt$weights
maxSR_DEOpt$objective_measures$mean[1]
maxSR_DEOpt$objective_measures$stdev[[1]]
library(PortfolioAnalytics)
# Plot optimization
chart.RiskReward(maxSR_DEOpt,
  risk.col="stdev",
  return.col="mean")
# Plot risk/ret points in portfolio scatterplot
risk_ret_points <- function(rets=rutils::etfenv$returns,
  risk=c("sd", "ETL"), symbolv=c("VTI", "IEF")) {
  risk <- match.arg(risk)  # Match to arg list
  if (risk=="ETL") {
    stopifnot(
"package:PerformanceAnalytics" %in% search() ||
require("PerformanceAnalytics", quietly=TRUE))
  }  # end if
  risk <- match.fun(risk)  # Match to function
  risk_ret <- t(sapply(rets[, symbolv],
     function(xtsv)
 c(ret=mean(xtsv), risk=abs(risk(xtsv)))))
  points(x=risk_ret[, "risk"], y=risk_ret[, "ret"],
   col="red", lwd=3)
  text(x=risk_ret[, "risk"], y=risk_ret[, "ret"],
 labels=rownames(risk_ret), col="red",
 lwd=2, pos=4)
}  # end risk_ret_points
risk_ret_points()
library(PortfolioAnalytics)
plot_portf <- function(portfolio,
      rets_data=rutils::etfenv$returns) {
  weightv <- portfolio$weights
  symbolv <- names(weightv)
  # Calculate xts of portfolio
  portf_max <- xts(
    rets_data[, symbolv] %*% weightv,
    order.by=zoo::index(rets_data))
  colnames(portf_max) <-
    deparse(substitute(portfolio))
  graph_params <- par(oma=c(1, 0, 1, 0),
    mgp=c(2, 1, 0), mar=c(2, 1, 2, 1),
    cex.lab=0.8, cex.axis=1.0,
    cex.main=0.8, cex.sub=0.5)
  layout(matrix(c(1,2), 2),
    widths=c(1,1), heights=c(1,3))
  barplot(weightv, names.arg=symbolv,
    las=3, ylab="", xlab="Symbol", main="")
  title(main=paste("Loadings",
          colnames(portf_max)), line=(-1))
  chart.CumReturns(
    cbind(portf_max, rets_data[, c("IEF", "VTI")]),
    lwd=2, ylab="", legend.loc="topleft", main="")
  title(main=paste0(colnames(portf_max),
              ", IEF, VTI"), line=(-1))
  par(graph_params)  # restore original parameters
  invisible(portf_max)
}  # end plot_portf
maxSR_DEOpt_xts <- plot_portf(portfolio=maxSR_DEOpt)
library(PortfolioAnalytics)
# Add leverage constraint abs(weightvum)
portf_maxSRN <- add.constraint(
  portfolio=portfi, type="leverage",
  min_sum=0.9, max_sum=1.1)
# Add box constraint long/short
portf_maxSRN <- add.constraint(
  portfolio=portf_maxSRN,
  type="box", min=-0.2, max=0.2)
# Add objectives
portf_maxSRN <- add.objective(
  portfolio=portf_maxSRN,
  type="return",  # Maximize mean return
  name="mean")
# Add objectives
portf_maxSRN <- add.objective(
  portfolio=portf_maxSRN,
  type="risk",  # Minimize stdev
  name="stdev")
load(file="/Users/jerzy/Develop/lecture_slides/data/portf_optim.RData")
library(PortfolioAnalytics)
# Perform optimization of weights
maxSRN_DEOpt <- optimize.portfolio(
  R=rutils::etfenv$returns[, symbolv],  # Specify returns
  portfolio=portf_maxSRN,  # Specify portfolio
  optimize_method="DEoptim", # Use DEoptim
  maxSR=TRUE,  # Maximize Sharpe
  trace=TRUE, traceDE=0)
# Plot optimization
chart.RiskReward(maxSRN_DEOpt,
  risk.col="stdev",
  return.col="mean",
  xlim=c(
    maxSR_DEOpt$objective_measures$stdev[[1]]-0.001,
    0.016))
  points(x=maxSR_DEOpt$objective_measures$stdev[[1]],
   y=maxSR_DEOpt$objective_measures$mean[1],
   col="green", lwd=3)
  text(x=maxSR_DEOpt$objective_measures$stdev[[1]],
   y=maxSR_DEOpt$objective_measures$mean[1],
 labels="maxSR", col="green",
 lwd=2, pos=4)
# Plot risk/ret points in portfolio scatterplot
risk_ret_points()
library(PortfolioAnalytics)
load(file="/Users/jerzy/Develop/lecture_slides/data/portf_optim.RData")
maxSRN_DEOpt$weights
maxSRN_DEOpt$objective_measures$mean[1]
maxSRN_DEOpt$objective_measures$stdev[[1]]
library(PortfolioAnalytics)
maxSRN_DEOpt_xts <- plot_portf(portfolio=maxSRN_DEOpt)
library(PerformanceAnalytics)
load(file="/Users/jerzy/Develop/lecture_slides/data/portf_optim.RData")
chart.CumReturns(
  cbind(maxSR_DEOpt_xts, maxSRN_DEOpt_xts),
  lwd=2, ylab="",
  legend.loc="topleft", main="")
options(width=50)
library(PerformanceAnalytics)
load(file="/Users/jerzy/Develop/lecture_slides/data/portf_optim.RData")
rbind(maxSR_DEOpt$weights, maxSRN_DEOpt$weightv)
c(maxSR_DEOpt$objective_measures$mean,
maxSRN_DEOpt$objective_measures$mean)
c(maxSR_DEOpt$objective_measures$stdev[[1]],
maxSRN_DEOpt$objective_measures$stdev[[1]])
library(PortfolioAnalytics)
# Add constraints
portf_maxSTARR <- add.constraint(
  portfolio=portfi,  # Initial portfolio
  type="weightsum",  # Constraint sum weights
  min_sum=0.9, max_sum=1.1)
# Add constraints
portf_maxSTARR <- add.constraint(
  portfolio=portf_maxSTARR,
  type="long_only")  # box constraint min=0, max=1
# Add objectives
portf_maxSTARR <- add.objective(
  portfolio=portf_maxSTARR,
  type="return",  # Maximize mean return
  name="mean")
# Add objectives
portf_maxSTARR <- add.objective(
  portfolio=portf_maxSTARR,
  type="risk",  # Minimize Expected Shortfall
  name="ES")
load(file="/Users/jerzy/Develop/lecture_slides/data/portf_optim.RData")
library(PortfolioAnalytics)
# Perform optimization of weights
maxSTARR_DEOpt <- optimize.portfolio(
  R=rutils::etfenv$returns[, symbolv],  # Specify returns
  portfolio=portf_maxSTARR,  # Specify portfolio
  optimize_method="DEoptim", # Use DEoptim
  maxSTARR=TRUE,  # Maximize STARR
  trace=TRUE, traceDE=0)
# Plot optimization
chart.RiskReward(maxSTARR_DEOpt,
  risk.col="ES",
  return.col="mean")
# Plot risk/ret points in portfolio scatterplot
risk_ret_points(risk="ETL")
options(width=50)
library(PortfolioAnalytics)
load(file="/Users/jerzy/Develop/lecture_slides/data/portf_optim.RData")
maxSTARR_DEOpt$weights
maxSTARR_DEOpt$objective_measures$mean[1]
maxSTARR_DEOpt$objective_measures$ES[[1]]
library(PortfolioAnalytics)
maxSTARR_DEOpt_xts <-
  plot_portf(portfolio=maxSTARR_DEOpt)
library(PerformanceAnalytics)
load(file="/Users/jerzy/Develop/lecture_slides/data/portf_optim.RData")
chart.CumReturns(
  cbind(maxSR_DEOpt_xts, maxSTARR_DEOpt_xts),
  lwd=2, ylab="",
  legend.loc="topleft", main="")
options(width=50)
library(PerformanceAnalytics)
load(file="/Users/jerzy/Develop/lecture_slides/data/portf_optim.RData")
rbind(maxSR_DEOpt$weights, maxSTARR_DEOpt$weightv)
c(maxSR_DEOpt$objective_measures$mean,
maxSTARR_DEOpt$objective_measures$mean)
c(maxSR_DEOpt$objective_measures$stdev[[1]],
maxSTARR_DEOpt$objective_measures$ES[[1]])
library(PortfolioAnalytics)
# Plot the efficient frontier
chart.EfficientFrontier(maxSR_DEOpt,
          match.col="stdev",
          n.portfolios=15, type="l")
points(x=maxSRN_DEOpt$objective_measures$stdev[[1]],
   y=maxSRN_DEOpt$objective_measures$mean[1],
   col="green", lwd=3)
text(x=maxSRN_DEOpt$objective_measures$stdev[[1]],
   y=maxSRN_DEOpt$objective_measures$mean[1],
 labels="maxSRN", col="green",
 lwd=2, pos=4)
library(PortfolioAnalytics)
# Add constraints
portf_minES <- add.constraint(
  portfolio=portfi,  # Initial portfolio
  type="weightsum",  # Constraint sum weights
  min_sum=0.9, max_sum=1.1)
# Add constraints
portf_minES <- add.constraint(
  portfolio=portf_minES,
  type="long_only")  # box constraint min=0, max=1
# Add objectives
portf_minES <- add.objective(
  portfolio=portf_minES,
  type="risk",  # Minimize ES
  name="ES")
load(file="/Users/jerzy/Develop/lecture_slides/data/portf_optim.RData")
library(PortfolioAnalytics)
# Perform optimization of weights
minESROI <- optimize.portfolio(
  R=rutils::etfenv$returns[, symbolv],  # Specify returns
  portfolio=portf_minES,  # Specify portfolio
  optimize_method="ROI", # Use ROI
  trace=TRUE, traceDE=0)
# Plot optimization
chart.RiskReward(maxSTARR_DEOpt,
  risk.col="ES",
  return.col="mean")
  points(x=minESROI$objective_measures$ES[[1]],
   y=mean(minESROI_xts),
   col="green", lwd=3)
  text(x=minESROI$objective_measures$ES[[1]],
   y=mean(minESROI_xts),
 labels="minES", col="green",
 lwd=2, pos=4)
# Plot risk/ret points in portfolio scatterplot
risk_ret_points(risk="ETL")
options(width=50)
library(PortfolioAnalytics)
load(file="/Users/jerzy/Develop/lecture_slides/data/portf_optim.RData")
minESROI$weights
minESROI$objective_measures$ES[[1]]
library(PortfolioAnalytics)
minESROI_xts <-
  plot_portf(portfolio=minESROI)
library(PerformanceAnalytics)
load(file="/Users/jerzy/Develop/lecture_slides/data/portf_optim.RData")
chart.CumReturns(
  cbind(maxSR_DEOpt_xts, minESROI_xts),
  lwd=2, ylab="",
  legend.loc="topleft", main="")
options(width=50)
library(PerformanceAnalytics)
load(file="/Users/jerzy/Develop/lecture_slides/data/portf_optim.RData")
rbind(maxSR_DEOpt$weights, minESROI$weightv)
c(maxSR_DEOpt$objective_measures$mean,
minESROI$objective_measures$mean)
c(maxSR_DEOpt$objective_measures$stdev[[1]],
minESROI$objective_measures$ES[[1]])
load(file="/Users/jerzy/Develop/lecture_slides/data/portf_optim.RData")
library(PortfolioAnalytics)
options(width=50)
# Perform optimization of weights
maxSR_DEOpt <- optimize.portfolio(
  R=rutils::etfenv$returns["/2011", symbolv],
  portfolio=portf_maxSR,  # Specify portfolio
  optimize_method="DEoptim", # Use DEoptim
  maxSR=TRUE,  # Maximize Sharpe
  trace=TRUE, traceDE=0)
weights1h <- maxSR_DEOpt$weights
# Plot optimization
maxSR_DEOpt_xts <-
  plot_portf(portfolio=maxSR_DEOpt)
load(file="/Users/jerzy/Develop/lecture_slides/data/portf_optim.RData")
library(PortfolioAnalytics)
options(width=50)
# Perform optimization of weights
maxSR_DEOpt <- optimize.portfolio(
  R=rutils::etfenv$returns["2011/", symbolv],
  portfolio=portf_maxSR,  # Specify portfolio
  optimize_method="DEoptim", # Use DEoptim
  maxSR=TRUE,  # Maximize Sharpe
  trace=TRUE, traceDE=0)
weights2h <- maxSR_DEOpt$weights
# Plot optimization
maxSR_DEOpt_xts <-
  plot_portf(portfolio=maxSR_DEOpt)
options(width=50)
weights1h
weights2h
weights1h - weights2h
par(oma=c(1,0,1,0), mgp=c(2,1,0), mar=c(2,1,2,1), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # Set plot panels
barplot(weights1h,
  names.arg=names(weights1h),
  las=3, ylab="", xlab="",
  main="Portfolio Weights First Half")
barplot(weights2h,
  names.arg=names(weights2h),
  las=3, ylab="", xlab="",
  main="Portfolio Weights Second Half")
# Calculate random default probabilities
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
nbonds <- 100
probv <- runif(nbonds, max=0.2)
mean(probv)
# Simulate number of defaults
unifv <- runif(nbonds)
sum(unifv < probv)
# Simulate average number of defaults using for() loop (inefficient way)
nsimu <- 1000
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
defaultv <- numeric(nsimu)
for (i in 1:nsimu) {  # Perform loop
  unifv <- runif(nbonds)
  defaultv[i] <- sum(unifv < probv)
}  # end for
# Calculate average number of defaults
mean(defaultv)
# Simulate using vectorized functions (efficient way)
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
unifm <- matrix(runif(nsimu*nbonds), ncol=nsimu)
defaultv <- colSums(unifm < probv)
mean(defaultv)
# Plot the distribution of defaults
x11(width=6, height=5)
plot(density(defaultv), main="Distribution of Defaults",
     xlab="number of defaults", ylab="frequency")
abline(v=mean(defaultv), lwd=3, col="red")
# Calculate default thresholds and asset values
threshv <- qnorm(probv)
assetm <-qnorm(unifm)
# Simulate defaults
defaultv <- colSums(assetm < threshv)
mean(defaultv)
# Plot Standard Normal distribution
x11(width=6, height=5)
xlim <- 4; threshv <- qnorm(0.025)
curve(expr=dnorm(x), type="l", xlim=c(-xlim, xlim),
xlab="asset value", ylab="", lwd=3,
col="blue", main="Distribution of Asset Values")
abline(v=threshv, col="red", lwd=3)
text(x=threshv-0.1, y=0.15, labels="default threshold",
 lwd=2, srt=90, pos=3)
# Plot polygon area
xvar <- seq(-xlim, xlim, length=100)
yvar <- dnorm(xvar)
intail <- ((xvar >= (-xlim)) & (xvar <= threshv))
polygon(c(xlim, xvar[intail], threshv),
  c(-1, yvar[intail], -1), col="red")
# Define correlation parameters
rho <- 0.2
rhos <- sqrt(rho) ; rhosm <- sqrt(1-rho)
nbonds <- 5 ; nsimu <- 10000
# Calculate vector of systematic and idiosyncratic factors
sysv <- rnorm(nsimu)
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
isync <- rnorm(nsimu*nbonds)
dim(isync) <- c(nbonds, nsimu)
# Simulate asset values using vectorized functions (efficient way)
assetm <- t(rhos*sysv + t(rhosm*isync))
# Asset values are standard normally distributed
apply(assetm, MARGIN=1, function(x) c(mean=mean(x), sd=sd(x)))
# Calculate correlations between asset values
cor(t(assetm))
# Simulate asset values using for() loop (inefficient way)
# Allocate matrix of assets
assetn <- matrix(nrow=nbonds, ncol=nsimu)
# Simulate asset values using for() loop
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
for (i in 1:nsimu) {  # Perform loop
  assetn[, i] <- rhos*sysv[i] + rhosm*rnorm(nbonds)
}  # end for
all.equal(assetn, assetm)
# benchmark the speed of the two methods
library(microbenchmark)
summary(microbenchmark(
  forloop={for (i in 1:nsimu) {
    rhos*sysv[i] + rhosm*rnorm(nbonds)}},
  vectorized={t(rhos*sysv + t(rhosm*isync))},
  times=10))[, c(1, 4, 5)]
# Calculate random default probabilities
nbonds <- 5
probv <- runif(nbonds, max=0.2)
mean(probv)
# Calculate default thresholds
threshv <- qnorm(probv)
# Calculate number of defaults using vectorized functions (efficient way)
# Calculate vector of number of defaults
rowMeans(assetm < threshv)
probv
# Calculate number of defaults using for() loop (inefficient way)
# Allocate matrix of defaultm
defaultm <- matrix(nrow=nbonds, ncol=nsimu)
# Simulate asset values using for() loop
for (i in 1:nsimu) {  # Perform loop
  defaultm[, i] <- (assetm[, i] < threshv)
}  # end for
rowMeans(defaultm)
rowMeans(assetm < threshv)
# Calculate correlations between defaults
cor(t(defaultm))
# Define default probabilities
nbonds <- 2
defprob <- 0.2
threshv <- qnorm(defprob)
# Define correlation parameters
rho <- 0.2
rhos <- sqrt(rho) ; rhosm <- sqrt(1-rho)
# Calculate vector of systematic factors
nsimu <- 1000
sysv <- rnorm(nsimu)
isync <- rnorm(nsimu*nbonds)
dim(isync) <- c(nbonds, nsimu)
# Simulate asset values using vectorized functions
assetm <- t(rhos*sysv + t(rhosm*isync))
# Calculate number of defaults using vectorized functions
defaultm <- (assetm < threshv)
# Calculate average number of defaults and compare to defprob
rowMeans(defaultm)
defprob
# Calculate correlations between assets
cor(t(assetm))
# Calculate correlations between defaults
cor(t(defaultm))
# Define cumulative default distribution function
cumdefdistr <- function(x, threshv=(-2), rho=0.2)
  pnorm((sqrt(1-rho)*qnorm(x) - threshv)/sqrt(rho))
defprob <- 0.4; threshv <- qnorm(defprob)
cumdefdistr(x=0.2, threshv=qnorm(defprob), rho=rho)
# Plot cumulative default distribution function
curve(expr=cumdefdistr(x, threshv=threshv, rho=0.05),
xlim=c(0, 0.999), lwd=3, xlab="percent default", ylab="probability",
col="green", main="Cumulative Default Probabilities")
# Plot default distribution with higher correlation
curve(expr=cumdefdistr(x, threshv=threshv, rho=0.2),
    xlim=c(0, 0.999), add=TRUE, lwd=3, col="blue", main="")
# Add legend
legend(x="topleft",
   legend=c("high correlation", "low correlation"),
   title=NULL, inset=0.05, cex=1.0, bg="white",
   bty="n", lwd=6, lty=1, col=c("blue", "green"))
# Add unconditional default probability
abline(v=defprob, col="red", lwd=3)
text(x=defprob, y=0.0, labels="default probability",
 lwd=2, srt=90, pos=4)
# Define default probability density function
defdistr <- function(x, threshv=(-2), rho=0.2)
  sqrt((1-rho)/rho)*exp(-(sqrt(1-rho)*qnorm(x) -
  threshv)^2/(2*rho) + qnorm(x)^2/2)
# Define parameters
rho <- 0.2 ; rhos <- sqrt(rho) ; rhosm <- sqrt(1-rho)
defprob <- 0.3; threshv <- qnorm(defprob)
defdistr(0.03, threshv=threshv, rho=rho)
# Plot probability distribution of defaults
curve(expr=defdistr(x, threshv=threshv, rho=0.1),
xlim=c(0, 1.0), lwd=3,
xlab="Default percentage", ylab="Density",
col="green", main="Distribution of Defaults")
# Plot default distribution with higher correlation
curve(expr=defdistr(x, threshv=threshv, rho=0.3),
xlab="default percentage", ylab="",
add=TRUE, lwd=3, col="blue", main="")
# Add legend
legend(x="topright",
   legend=c("high correlation", "low correlation"),
   title=NULL, inset=0.05, cex=1.0, bg="white",
   bty="n", lwd=6, lty=1, col=c("blue", "green"))
# Add unconditional default probability
abline(v=defprob, col="red", lwd=3)
text(x=defprob, y=2, labels="default probability",
 lwd=2, srt=90, pos=2)
# Plot default distribution with low correlation
curve(expr=defdistr(x, threshv=threshv, rho=0.01),
  xlab="default percentage", ylab="", lwd=2,
  col="green", main="Distribution of Defaults")
# Plot default distribution with high correlation
curve(expr=defdistr(x, threshv=threshv, rho=0.99),
  xlab="percentage of defaults", ylab="density",
  add=TRUE, lwd=2, n=10001, col="blue", main="")
# Add legend
legend(x="top", legend=c("high correlation", "low correlation"),
   title=NULL, inset=0.1, cex=1.0, bg="white",
   bty="n", lwd=6, lty=1, col=c("blue", "green"))
# Add unconditional default probability
abline(v=0.1, col="red", lwd=2)
text(x=0.1, y=10, lwd=2, pos=4, labels="default probability")
# Get help for integrate()
?integrate
# Calculate slowly converging integral
func <- function(x) {1/((x+1)*sqrt(x))}
integrate(func, lower=0, upper=10)
integrate(func, lower=0, upper=Inf)
# Integrate function with parameter lambdaf
func <- function(x, lambdaf=1) {
  exp(-x*lambdaf)
}  # end func
integrate(func, lower=0, upper=Inf)
integrate(func, lower=0, upper=Inf, lambdaf=2)
# Cumulative probability over normal distribution
pnorm(-2)
integrate(dnorm, low=2, up=Inf)
str(dnorm)
pnorm(-1)
integrate(dnorm, low=2, up=Inf, mean=1)
# Expected value over normal distribution
integrate(function(x) x*dnorm(x), low=2, up=Inf)
# Vasicek model parameters
rho <- 0.1; lgd <- 0.4
defprob <- 0.05; threshv <- qnorm(defprob)
# Define Vasicek cumulative loss distribution
cumlossdistr <- function(x, threshv=(-2), rho=0.2, lgd=0.4)
  pnorm((sqrt(1-rho)*qnorm(x/lgd) - threshv)/sqrt(rho))
# Define Vasicek loss distribution function
lossdistr <- function(x, threshv=(-2), rho=0.2, lgd=0.4)
  sqrt((1-rho)/rho)*exp(-(sqrt(1-rho)*qnorm(x/lgd) - threshv)^2/(2*rho) + qnorm(x/lgd)^2/2)/lgd
integrate(lossdistr, low=0, up=lgd, threshv=(-2), rho=rho, lgd=lgd)
# Plot probability distribution of losses
x11(width=6, height=5)
curve(expr=lossdistr(x, threshv=threshv, rho=rho),
cex.main=1.8, cex.lab=1.8, cex.axis=1.5,
type="l", xlim=c(0, 0.06),
xlab="loss percentage", ylab="density", lwd=3,
col="blue", main="Portfolio Loss Density")
# Add line for expected loss
abline(v=lgd*defprob, col="red", lwd=3)
text(x=lgd*defprob-0.001, y=35, labels="expected loss", lwd=3, pos=4, cex=1.8)
# Define Vasicek cumulative loss distribution
# (with error handling for x)
cumlossdistr <- function(x, threshv=(-2), rho=0.2, lgd=0.4) {
  qnormv <- ifelse(x/lgd < 0.999, qnorm(x/lgd), 3.1)
  pnorm((sqrt(1-rho)*qnormv - threshv)/sqrt(rho))
}  # end cumlossdistr
# Define Vasicek loss distribution function
# (vectorized version with error handling for x)
lossdistr <- function(x, threshv=(-2), rho=0.1, lgd=0.4) {
  qnormv <- ifelse(x/lgd < 0.999, qnorm(x/lgd), 3.1)
  sqrt((1-rho)/rho)*exp(-(sqrt(1-rho)*qnormv - threshv)^2/(2*rho) + qnormv^2/2)/lgd
}  # end lossdistr
defprob <- 0.2; threshv <- qnorm(defprob)
rho <- 0.1; lgd <- 0.4
attachp <- 0.15; detachp <- 0.2
# Expected tranche loss is sum of two terms
tranchel <-
  # Loss between attachp and detachp
  integrate(function(x, attachp) (x-attachp)*lossdistr(x,
threshv=threshv, rho=rho, lgd=lgd),
low=attachp, up=detachp, attachp=attachp)$value/(detachp-attachp) +
  # Loss in excess of detachp
  (1-cumlossdistr(x=detachp, threshv=threshv, rho=rho, lgd=lgd))
# Plot probability distribution of losses
curve(expr=lossdistr(x, threshv=threshv, rho=rho),
cex.main=1.8, cex.lab=1.8, cex.axis=1.5,
type="l", xlim=c(0, 3*lgd*defprob),
xlab="loss percentage", ylab="density", lwd=3,
col="orange", main="CDO Tranche Losses")
# Add line for expected loss
abline(v=lgd*defprob, col="red", lwd=3)
text(x=lgd*defprob-0.001, y=4, labels="expected loss",
 lwd=2, srt=90, pos=3, cex=1.8)
# Add lines for attach and detach
abline(v=attachp, col="blue", lwd=3)
text(x=attachp-0.001, y=4, labels="attach",
 lwd=2, srt=90, pos=3, cex=1.8)
abline(v=detachp, col="green", lwd=3)
text(x=detachp-0.001, y=4, labels="detach",
 lwd=2, srt=90, pos=3, cex=1.8)
# Add shading for CDO tranche
vars <- seq(attachp, detachp, length=100)
densv <- sapply(vars, lossdistr, threshv=threshv, rho=rho)
# Draw shaded polygon
polygon(c(attachp, vars, detachp), density=20,
  c(-1, densv, -1), col="red", border=NA)
text(x=0.5*(attachp+detachp), y=0, labels="CDO tranche", cex=1.8, lwd=2, pos=3)
# Add lines for unexpected loss
abline(v=0.04, col="blue", lwd=3)
arrows(x0=0.02, y0=35, x1=0.04, y1=35, code=3, lwd=3, cex=0.5)
text(x=0.03, y=36, labels="unexpected loss", lwd=2, pos=3)
# Add lines for VaR
abline(v=0.055, col="red", lwd=3)
arrows(x0=0.0, y0=25, x1=0.055, y1=25, code=3, lwd=3, cex=0.5)
text(x=0.03, y=26, labels="VaR", lwd=2, pos=3)
text(x=0.055-0.001, y=10, labels="VaR", lwd=2, srt=90, pos=3)
varisk <- 0.04; varmax <- 4*lgd*defprob
# Calculate CVaR
cvar <- integrate(function(x) x*lossdistr(x, threshv=threshv,
  rho=rho, lgd=lgd), low=varisk, up=lgd)$value
cvar <- cvar/integrate(lossdistr, low=varisk, up=lgd,
   threshv=threshv, rho=rho, lgd=lgd)$value
# Plot probability distribution of losses
curve(expr=lossdistr(x, threshv=threshv, rho=rho),
type="l", xlim=c(0, 0.06),
xlab="loss percentage", ylab="density", lwd=3,
col="blue", main="Conditional Value at Risk")
# Add line for expected loss
abline(v=lgd*defprob, col="red", lwd=3)
text(x=lgd*defprob-0.001, y=10, labels="expected loss", lwd=2, srt=90, pos=3)
# Add lines for VaR
abline(v=varisk, col="red", lwd=3)
text(x=varisk-0.001, y=10, labels="VaR",
 lwd=2, srt=90, pos=3)
# Add shading for CVaR
vars <- seq(varisk, varmax, length=100)
densv <- sapply(vars, lossdistr,
  threshv=threshv, rho=rho)
# Draw shaded polygon
polygon(c(varisk, vars, varmax), density=20,
  c(-1, densv, -1), col="red", border=NA)
text(x=varisk+0.005, y=0, labels="CVaR", lwd=2, pos=3)
# VaR (quantile of the loss distribution)
varfun <- function(x, threshv=qnorm(0.1), rho=0.1, lgd=0.4)
  lgd*pnorm((sqrt(rho)*qnorm(x) + threshv)/sqrt(1-rho))
varfun(x=0.99, threshv=threshv, rho=rho, lgd=lgd)
# Plot VaR
curve(expr=varfun(x, threshv=threshv, rho=rho, lgd=lgd),
type="l", xlim=c(0, 0.999), xlab="confidence level", ylab="VaR", lwd=3,
col="orange", main="VaR versus Confidence Level")
# Add line for expected loss
abline(h=lgd*defprob, col="red", lwd=3)
text(x=0.2, y=lgd*defprob, labels="expected loss", lwd=2, pos=3)
# Integrate lossdistr() over full range
integrate(lossdistr, low=0.0, up=lgd,
    threshv=threshv, rho=rho, lgd=lgd)
# Calculate expected losses using lossdistr()
integrate(function(x) x*lossdistr(x, threshv=threshv,
  rho=rho, lgd=lgd), low=0.0, up=lgd)
# Calculate confidence levels corresponding to VaR values
vars <- seq(0.07, 0.12, 0.001)
conv <- sapply(vars, function(varisk) {
  integrate(lossdistr, low=varisk, up=lgd,
      threshv=threshv, rho=rho, lgd=lgd)
})  # end sapply
conv <- cbind(as.numeric(t(conv)[, 1]), vars)
colnames(conv) <- c("levels", "VaRs")
# Calculate 95% confidence level VaR value
conv[match(TRUE, conv[, "levels"] < 0.05), "VaRs"]
plot(x=1-conv[, "levels"],
     y=conv[, "VaRs"], lwd=2,
     xlab="confidence level", ylab="VaRs",
     t="l", main="VaR Values and Confidence Levels")
# Calculate CVaR values
cvars <- sapply(vars, function(varisk) {
  integrate(function(x) x*lossdistr(x, threshv=threshv,
rho=rho, lgd=lgd), low=varisk, up=lgd)})  # end sapply
conv <- cbind(conv, as.numeric(t(cvars)[, 1]))
colnames(conv)[3] <- "CVaRs"
# Divide CVaR by confidence level
conv[, "CVaRs"] <- conv[, "CVaRs"]/conv[, "levels"]
# Calculate 95% confidence level CVaR value
conv[match(TRUE, conv[, "levels"] < 0.05), "CVaRs"]
# Plot CVaRs
plot(x=1-conv[, "levels"], y=conv[, "CVaRs"],
     t="l", col="red", lwd=2,
     ylim=range(conv[, c("VaRs", "CVaRs")]),
     xlab="confidence level", ylab="CVaRs",
     main="CVaR Values and Confidence Levels")
# Add VaRs
lines(x=1-conv[, "levels"], y=conv[, "VaRs"], lwd=2)
# Add legend
legend(x="topleft", legend=c("CVaRs", "VaRs"),
   title="default probability = 5%
correlation = 10%
loss given default = 40%",
   inset=0.1, cex=1.0, bg="white", bty="n",
   lwd=6, lty=1, col=c("red", "black"))
# Define model parameters
nbonds <- 300; nsimu <- 1000; lgd <- 0.4
# Define correlation parameters
rho <- 0.2; rhos <- sqrt(rho); rhosm <- sqrt(1-rho)
# Calculate default probabilities and thresholds
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
probv <- runif(nbonds, max=0.2)
threshv <- qnorm(probv)
# Simulate losses under the Vasicek model
sysv <- rnorm(nsimu)
assetm <- matrix(rnorm(nsimu*nbonds), ncol=nsimu)
assetm <- t(rhos*sysv + t(rhosm*assetm))
lossv <- lgd*colSums(assetm < threshv)/nbonds
# Calculate VaR from confidence level
confl <- 0.95
varisk <- quantile(lossv, confl)
# Calculate the CVaR as the mean losses in excess of VaR
cvar <- mean(lossv[lossv > varisk])
# Plot the density of portfolio losses
densv <- density(lossv, from=0)
plot(densv, xlab="loss percentage", ylab="density",
   cex.main=1.0, cex.lab=1.0, cex.axis=1.0,
   lwd=3, col="blue", main="Portfolio Loss Distribution")
# Add vertical line for expected loss
exploss <- lgd*mean(probv)
abline(v=exploss, col="red", lwd=3)
xmax <- max(densv$x); ymax <- max(densv$y)
text(x=exploss, y=(6*ymax/7), labels="expected loss",
     lwd=2, pos=4, cex=1.0)
# Add vertical line for VaR
abline(v=varisk, col="red", lwd=3)
text(x=varisk, y=4*ymax/5, labels="VaR", lwd=2, pos=4, cex=1.0)
# Draw shaded polygon for CVaR
intail <- (densv$x > varisk)
xvar <- c(min(densv$x[intail]), densv$x[intail], max(densv$x))
polygon(xvar, c(-1, densv$y[intail], -1), col="red", border=NA, density=10)
# Add text for CVaR
text(x=5*varisk/4, y=(ymax/7), labels="CVaR", lwd=2, pos=4, cex=1.0)
# Add text with data
text(xmax, ymax, labels=paste0(
   "Expected Loss = ", format(100*exploss, digits=3), "%", "\n",
   "Loss severity = ", format(100*lgd, digits=3), "%", "\n",
   "Correlation = ", format(100*rho, digits=3), "%", "\n",
   "VaR = ", format(100*varisk, digits=3), "%", "\n",
   "CVaR = ", format(100*cvar, digits=3), "%"),
   adj=c(1, 1), cex=1.0, lwd=2)
# Calculate VaRs from confidence levels
conv <- seq(0.93, 0.99, 0.01)
vars <- quantile(lossv, probs=conv)
plot(x=conv, y=vars, t="l", lwd=2,
   xlab="confidence level", ylab="VaRs",
   main="Simulated VaR and Confidence Levels")
# Calculate CVaRs
cvars <- sapply(vars, function(varisk) {
  mean(lossv[lossv >= varisk])
})  # end sapply
cvars <- cbind(cvars, vars)
# Alternative CVaR calculation using frequency table
# first calculate frequency table of losses
# tablev <- table(lossv)/nsimu
# Calculate CVaRs from frequency table
# cvars <- sapply(vars, function(varisk) {
#   tailrisk <- tablev[names(tablev) > varisk]
#   tailrisk %*% as.numeric(names(tailrisk)) / sum(tailrisk)
# })  # end sapply
# Plot CVaRs
plot(x=conv, y=cvars[, "cvars"],
   t="l", col="red", lwd=2, ylim=range(cvars),
   xlab="confidence level", ylab="CVaRs",
   main="Simulated CVaR and Confidence Levels")
# Add VaRs
lines(x=conv, y=cvars[, "vars"], lwd=2)
# Add legend
legend(x="topleft", legend=c("CVaRs", "VaRs"), bty="n",
   title=NULL, inset=0.05, cex=1.0, bg="white",
   y.intersp=0.3, lwd=6, lty=1, col=c("red", "black"))
calc_var <- function(threshv, # Default thresholds
   lgd=0.6, # loss given default
   rhos, rhosm, # asset correlation
   nsimu=1000, # number of simulations
   conv=seq(0.93, 0.99, 0.01) # Confidence levels
   ) {
  # Define model parameters
  nbonds <- NROW(threshv)
  # Simulate losses under the Vasicek model
  sysv <- rnorm(nsimu)
  assetm <- matrix(rnorm(nsimu*nbonds), ncol=nsimu)
  assetm <- t(rhos*sysv + t(rhosm*assetm))
  lossv <- lgd*colSums(assetm < threshv)/nbonds
  # Calculate VaRs and CVaRs
  vars <- quantile(lossv, probs=conv)
  cvars <- sapply(vars, function(varisk) {
    mean(lossv[lossv >= varisk])
  })  # end sapply
  names(vars) <- conv
  names(cvars) <- conv
  c(vars, cvars)
}  # end calc_var
# Define model parameters
nbonds <- 300; nsimu <- 1000; lgd <- 0.4
rho <- 0.2; rhos <- sqrt(rho); rhosm <- sqrt(1-rho)
# Calculate default probabilities and thresholds
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
probv <- runif(nbonds, max=0.2)
threshv <- qnorm(probv)
conv <- seq(0.93, 0.99, 0.01)
# Define number of bootstrap simulations
nboot <- 500
# Perform bootstrap of calc_var
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
bootd <- sapply(rep(lgd, nboot), calc_var,
  threshv=threshv,
  rhos=rhos, rhosm=rhosm,
  nsimu=nsimu, conv=conv)  # end sapply
bootd <- t(bootd)
# Calculate standard errors of VaR and CVaR from bootd data
varsd <- apply(bootd[, 1:7], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
cvarsd <- apply(bootd[, 8:14], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
# Scale the standard errors of VaRs and CVaRs
varsds <- varsd[2, ]/varsd[1, ]
cvarsds <- cvarsd[2, ]/cvarsd[1, ]
# Plot the scaled standard errors of VaRs and CVaRs
plot(x=names(varsds), y=varsds,
  t="l", lwd=2, ylim=range(c(varsds, cvarsds)),
  xlab="confidence level", ylab="standard error",
  main="Scaled Standard Errors of CVaR and VaR")
lines(x=names(cvarsds), y=cvarsds, lwd=2, col="red")
legend(x="topleft", legend=c("CVaRs", "VaRs"), bty="n",
   title=NULL, inset=0.05, cex=1.0, bg="white",
   y.intersp=0.3, lwd=6, lty=1, col=c("red", "black"))
library(parallel)  # load package parallel
ncores <- detectCores() - 1  # number of cores
compclust <- makeCluster(ncores)  # Initialize compute cluster
# Perform bootstrap of calc_var for Windows
clusterSetRNGStream(compclust, 1121)
bootd <- parLapply(compclust, rep(lgd, nboot),
  fun=calc_var, threshv=threshv,
  rhos=rhos, rhosm=rhosm,
  nsimu=nsimu, conv=conv)  # end parLapply
stopCluster(compclust)  # Stop R processes over cluster
# Bootstrap under Mac-OSX or Linux
bootd <- mclapply(rep(lgd, nboot),
  FUN=calc_var, threshv=threshv,
  rhos=rhos, rhosm=rhosm,
  nsimu=nsimu, conv=conv)  # end mclapply
bootd <- rutils::do_call(rbind, bootd)
# Calculate standard errors of VaR and CVaR from bootd data
varsd <- apply(bootd[, 1:7], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
cvarsd <- apply(bootd[, 8:14], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
# Scale the standard errors of VaRs and CVaRs
varsds <- varsd[2, ]/varsd[1, ]
cvarsds <- cvarsd[2, ]/cvarsd[1, ]
# Plot the standard errors of VaRs and CVaRs
plot(x=names(varsds), y=varsds, t="l", lwd=2,
  ylim=range(c(varsds, cvarsds)),
  xlab="confidence level", ylab="standard error",
  main="Scaled Standard Errors of CVaR and VaR")
lines(x=names(cvarsds), y=cvarsds, lwd=2, col="red")
legend(x="topleft", legend=c("CVaRs", "VaRs"), bty="n",
   title=NULL, inset=0.05, cex=1.0, bg="white",
   y.intersp=0.3, lwd=6, lty=1, col=c("red", "black"))
calc_var <- function(probv, # Default probabilities
   lgd=0.6, # loss given default
   rhos, rhosm, # asset correlation
   nsimu=1000, # number of simulations
   conv=seq(0.93, 0.99, 0.01) # Confidence levels
   ) {
  # Calculate random default thresholds
  threshv <- qnorm(runif(1, min=0.5, max=1.5)*probv)
  # Simulate losses under the Vasicek model
  nbonds <- NROW(probv)
  sysv <- rnorm(nsimu)
  assetm <- matrix(rnorm(nsimu*nbonds), ncol=nsimu)
  assetm <- t(rhos*sysv + t(rhosm*assetm))
  lossv <- lgd*colSums(assetm < threshv)/nbonds
  # Calculate VaRs and CVaRs
  vars <- quantile(lossv, probs=conv)
  cvars <- sapply(vars, function(varisk) {
    mean(lossv[lossv >= varisk])
  })  # end sapply
  names(vars) <- conv
  names(cvars) <- conv
  c(vars, cvars)
}  # end calc_var
library(parallel)  # load package parallel
ncores <- detectCores() - 1  # number of cores
compclust <- makeCluster(ncores)  # Initialize compute cluster
# Perform bootstrap of calc_var for Windows
clusterSetRNGStream(compclust, 1121)
bootd <- parLapply(compclust, rep(lgd, nboot),
  fun=calc_var, probv=probv,
  rhos=rhos, rhosm=rhosm,
  nsimu=nsimu, conv=conv)  # end parLapply
stopCluster(compclust)  # Stop R processes over cluster
# Bootstrap under Mac-OSX or Linux
bootd <- mclapply(rep(lgd, nboot),
  FUN=calc_var, probv=probv,
  rhos=rhos, rhosm=rhosm,
  nsimu=nsimu, conv=conv)  # end mclapply
bootd <- rutils::do_call(rbind, bootd)
# Calculate standard errors of VaR and CVaR from bootd data
varsd <- apply(bootd[, 1:7], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
cvarsd <- apply(bootd[, 8:14], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
# Scale the standard errors of VaRs and CVaRs
varsdsu <- varsd[2, ]/varsd[1, ]
cvarsdsu <- cvarsd[2, ]/cvarsd[1, ]
# Plot the standard errors of VaRs under uncertain default probabilities
plot(x=colnames(varsd), y=varsds, t="l",
 col="black", lwd=2, ylim=range(c(varsds, varsdsu)),
  xlab="confidence level", ylab="standard error",
  main="Standard Errors of VaR
  with Random Default Probabilities")
lines(x=colnames(varsd), y=varsdsu, lwd=2, col="red")
legend(x="topleft",
   legend=c("VaR Fixed Def Probs", "VaR Random Def Probs"),
   bty="n", title=NULL, inset=0.05, cex=1.0, bg="white",
   y.intersp=0.3, lwd=6, lty=1, col=c("black", "red"))
NA
# Plot the standard errors of VaRs and CVaRs
plot(x=colnames(varsd), y=varsdsu, t="l", lwd=2,
  ylim=range(c(varsdsu, cvarsdsu)),
  xlab="confidence level", ylab="standard error",
  main="Relative Standard Errors of VaR and CVaR
  with Uncertain Default Probabilities")
lines(x=colnames(varsd), y=cvarsdsu, lwd=2, col="red")
legend(x="topright", legend=c("CVaR", "VaR"), bty="n",
   title=NULL, inset=0.05, cex=1.0, bg="white",
   y.intersp=0.3, lwd=6, lty=1, col=c("red", "black"))
##########################################
# Summary: Calculate the VaR and CVaR of a credit
# portfolio using importance sampling.
Run the setup code below.
# Define model parameters
nbonds <- 10; nsimu <- 1000; lgd <- 0.4
# Define correlation parameters
rho <- 0.2; rhos <- sqrt(rho); rhosm <- sqrt(1-rho)
# Calculate default probabilities and thresholds
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
probv <- runif(nbonds, max=0.2)
threshv <- qnorm(probv)
# Simulate losses under the Vasicek model
sysv <- rnorm(nsimu)
assetm <- matrix(rnorm(nsimu*nbonds), ncol=nsimu)
assetm <- t(rhos*sysv + t(rhosm*assetm))
lossv <- lgd*colSums(assetm < threshv)/nbonds
# Calculate VaR from confidence level
confl <- 0.95
varisk <- quantile(lossv, confl)
cumprob <- (1:nsimu)/nsimu
sort(lossv)[findInterval(confl, cumprob)]
# Calculate the CVaR as the mean losses in excess of VaR
cvar <- mean(lossv[lossv > varisk])
# Apply importance sampling
lambdaf <- (-0.1)
assett <- assetm + lambdaf
weightv <- exp(-lambdaf*assett + lambdaf^2/2)
lossv <- lgd*colSums(assett < threshv)/nbonds
lossw <- matrixStats::colProds(weightv)
# Calculate the CVaR using importance sampling - doesn't work
sum((lossv < varisk)*lossw)/nsimu
# Or
lossv <- lgd*colSums((assett < threshv)*weightv)/nbonds
mean(lossv[lossv > varisk])
# Data must be sorted for importance sampling of VaR
ordern <- order(lossv)
lossv <- lossv[ordern]
lossw <- lossw[ordern]
# Calculate the VaR using importance sampling - doesn't work
cumprob <- cumsum(lossw)/nsimu
lossv[findInterval(confl, cumprob)]
# Define model parameters
nbonds <- 300; nsimu <- 1000; lgd <- 0.4
rho <- 0.2; rhos <- sqrt(rho); rhosm <- sqrt(1-rho)
# Calculate default probabilities and thresholds
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
probv <- runif(nbonds, max=0.2)
threshv <- qnorm(probv)
# Define number of bootstrap simulations
nboot <- 500
# Perform bootstrap of calc_var
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
bootd <- sapply(rep(lgd, nboot),
  calc_var,
  threshv=threshv,
  rhos=rhos, rhosm=rhosm,
  nsimu=nsimu, conv=conv)  # end sapply
bootd <- t(bootd)
# Calculate standard errors of VaR and CVaR from bootd data
varsd <- apply(bootd[, 1:7], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
cvarsd <- apply(bootd[, 8:14], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
# Scale the standard errors of VaRs and CVaRs
varsd[2, ] <- varsd[2, ]/varsd[1, ]
cvarsd[2, ] <- cvarsd[2, ]/cvarsd[1, ]
