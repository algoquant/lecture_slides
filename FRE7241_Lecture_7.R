# Calculate the VTI daily percentage returns
retp <- na.omit(rutils::etfenv$returns$VTI)
datev <- index(retp)
nrows <- NROW(retp)
# Define in-sample and out-of-sample intervals
insample <- 1:(nrows %/% 2)
outsample <- (nrows %/% 2 + 1):nrows
cutoff <- nrows %/% 2
# Define the response and predictor matrices
respv <- retp
orderp <- 8 # 9 predictors!!!
predm <- lapply(1:orderp, rutils::lagit, input=respv)
predm <- rutils::do_call(cbind, predm)
predm <- cbind(rep(1, nrows), predm)
colnames(predm) <- c("phi0", paste0("lag", 1:orderp))
# Calculate the in-sample fitted autoregressive coefficients
predinv <- MASS::ginv(predm[insample, ])
coeff <- predinv %*% respv[insample, ]
# Calculate the in-sample forecasts of VTI (fitted values)
fcasts <- predm %*% coeff
fcastv <- sqrt(HighFreq::run_var(fcasts, lambda=0.8)[, 2])
fcastv[1:100] <- 1
fcasts <- fcasts/fcastv

# Calculate the autoregressive strategy PnLs
pnls <- retp*fcasts
pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])
# Calculate the in-sample and out-of-sample Sharpe and Sortino ratios
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "AR_multifact")
sqrt(252)*sapply(wealthv[insample, ], function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
sqrt(252)*sapply(wealthv[outsample, ], function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of the autoregressive strategies
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
colorv <- colorRampPalette(c("blue", "red"))(NCOL(wealthv))
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Multifactor Autoregressive Strategy") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyEvent(datev[cutoff], label="cutoff", strokePattern="solid", color="red") %>%
  dyLegend(show="always", width=300)

# Calculate singular value decomposition of the predictor matrix
svdec <- svd(predm)
barplot(svdec$d, main="Singular Values of Predictor Matrix")
# Calculate generalized inverse from SVD
invsvd <- svdec$v %*% (t(svdec$u) / svdec$d)
# Verify inverse property of the inverse
all.equal(zoo::coredata(predm), predm %*% invsvd %*% predm)
# Compare with the generalized inverse using MASS::ginv()
invreg <- MASS::ginv(predm)
all.equal(invreg, invsvd)
# Set tolerance for determining zero singular values
precv <- sqrt(.Machine$double.eps)
# Check for zero singular values
round(svdec$d, 12)
notzero <- (svdec$d > (precv*svdec$d[1]))
# Calculate generalized inverse from SVD
invsvd <- svdec$v[, notzero] %*%
  (t(svdec$u[, notzero]) / svdec$d[notzero])
# Verify inverse property of invsvd
all.equal(zoo::coredata(predm), predm %*% invsvd %*% predm)
all.equal(invsvd, invreg)

# Calculate reduced inverse from SVD
dimax <- 3 # Number of dimensions to keep
invred <- svdec$v[, 1:dimax] %*%
  (t(svdec$u[, 1:dimax]) / svdec$d[1:dimax])
# Inverse property fails for invred
all.equal(zoo::coredata(predm), predm %*% invred %*% predm)
# Calculate reduced inverse using RcppArmadillo
invrcpp <- HighFreq::calc_invsvd(predm, dimax=dimax)
all.equal(invred, invrcpp, check.attributes=FALSE)

# Calculate the in-sample SVD
svdec <- svd(predm[insample, ])
# Calculate the in-sample fitted AR coefficients for different dimensions
dimv <- 2:5
# dimv <- c(2, 5, 10, NCOL(predm))
coeffm <- sapply(dimv, function(dimax) {
  predinv <- svdec$v[, 1:dimax] %*%
    (t(svdec$u[, 1:dimax]) / svdec$d[1:dimax])
  predinv %*% respv[insample]
})  # end lapply
colnames(coeffm) <- paste0("dimax=", dimv)
colorv <- colorRampPalette(c("red", "blue"))(NCOL(coeffm))
matplot(y=coeffm, type="l", lty="solid", lwd=1, col=colorv,
  xlab="predictor", ylab="coeff",
  main="AR Coefficients For Different Dimensions")
# Calculate the forecasts of VTI
fcasts <- predm %*% coeffm
fcasts <- apply(fcasts, 2, function(x) {
  fcastv <- sqrt(HighFreq::run_var(matrix(x), lambda=0.8)[, 2])
  fcastv[1:100] <- 1
  x/fcastv
}) # end apply
# Simulate the autoregressive strategies
retn <- coredata(retp)
pnls <- apply(fcasts, 2, function(x) (x*retn))
pnls <- xts(pnls, datev)
# Scale the PnL volatility to that of VTI
pnls <- lapply(pnls, function(x) x/sd(x))
pnls <- sd(retp)*do.call(cbind, pnls)

# Calculate the in-sample and out-of-sample Sharpe and Sortino ratios
wealthv <- cbind(retp, pnls)
sqrt(252)*sapply(wealthv[insample, ], function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
sqrt(252)*sapply(wealthv[outsample, ], function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of the autoregressive strategies
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
colorv <- colorRampPalette(c("blue", "red"))(NCOL(wealthv))
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Autoregressive Strategies With Dimension Reduction") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyEvent(datev[cutoff], label="cutoff", strokePattern="solid", color="red") %>%
  dyLegend(show="always", width=500)

# Objective function for the in-sample AR coefficients
objfun <- function(coeff, lambdaf) {
  fcasts <- predm[insample, ] %*% coeff
  lambdav <- lambdaf^(0:orderp)
  10000*sum((respv[insample, ] - fcasts)^2) + sum(lambdav*coeff^2)
}  # end objfun

# Perform optimization using the quasi-Newton method
optiml <- optim(par=numeric(orderp+1),
          fn=objfun, lambdaf=5.0, method="L-BFGS-B",
          upper=rep(10000, orderp+1),
          lower=rep(-10000, orderp+1))
# Extract the AR coefficients
coeff <- optiml$par
coeffn <- paste0("phi", 0:orderp)
names(coeff) <- coeffn
barplot(coeff ~ coeffn, xlab="", ylab="t-value", col="grey",
  main="AR Coefficients With Shrinkage")

# Calculate the forecasts of VTI (fitted values)
fcasts <- predm %*% coeff
fcastv <- sqrt(HighFreq::run_var(fcasts, lambda=0.8)[, 2])
fcastv[1:100] <- 1
fcasts <- fcasts/fcastv
# Calculate the autoregressive strategy PnLs
pnls <- retp*fcasts
pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])

# Calculate the in-sample and out-of-sample Sharpe and Sortino ratios
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "AR_multifact")
sqrt(252)*sapply(wealthv[insample, ], function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
sqrt(252)*sapply(wealthv[outsample, ], function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of the autoregressive strategies
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
colorv <- colorRampPalette(c("blue", "red"))(NCOL(wealthv))
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Multifactor Autoregressive Strategy With Shrinkage") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyEvent(datev[cutoff], label="cutoff", strokePattern="solid", color="red") %>%
  dyLegend(show="always", width=300)

# Calculate the returns of VTI, TLT, VXX, and SVXY
retp <- na.omit(rutils::etfenv$returns[, c("VTI", "TLT", "VXX", "SVXY")])
datev <- zoo::index(retp)
nrows <- NROW(retp)
# Calculate VTI returns and trading volumes
ohlc <- rutils::etfenv$VTI
volumv <- rutils::diffit(quantmod::Vo(ohlc))[datev]
# Define the response and the VTI predictor matrix
respv <- retp$VTI
orderp <- 5
predm <- lapply(1:orderp, rutils::lagit, input=respv)
predm <- rutils::do_call(cbind, predm)
predm <- cbind(rep(1, nrows), predm)
colnames(predm) <- c("phi0", paste0("lag", 1:orderp))
# Add the TLT predictor matrix
predx <- lapply(1:orderp, rutils::lagit, input=retp$TLT)
predx <- rutils::do_call(cbind, predx)
colnames(predx) <- paste0("TLT", 1:orderp)
predm <- cbind(predm, predx)
# Add the VXX predictor matrix
predx <- lapply(1:orderp, rutils::lagit, input=retp$VXX)
predx <- rutils::do_call(cbind, predx)
colnames(predx) <- paste0("VXX", 1:orderp)
predm <- cbind(predm, predx)
# Perform the multivariate linear regression
regmod <- lm(respv ~ predm - 1)
summary(regmod)

# Define in-sample and out-of-sample intervals
insample <- 1:(nrows %/% 2)
outsample <- (nrows %/% 2 + 1):nrows
cutoff <- nrows %/% 2
# Calculate the in-sample fitted autoregressive coefficients
predinv <- MASS::ginv(predm[insample, ])
coeff <- predinv %*% respv[insample, ]
coeffn <- colnames(predm)
barplot(coeff ~ coeffn, xlab="", ylab="t-value", col="grey",
  main="Coefficients of Kitchen Sink Autoregressive Model")
# Calculate the in-sample forecasts of VTI (fitted values)
fcasts <- predm %*% coeff
fcastv <- sqrt(HighFreq::run_var(fcasts, lambda=0.8)[, 2])
fcastv[1:100] <- 1
fcasts <- fcasts/fcastv

# Calculate the autoregressive strategy PnLs
pnls <- respv*fcasts
pnls <- pnls*sd(respv[respv<0])/sd(pnls[pnls<0])
# Calculate the in-sample and out-of-sample Sharpe and Sortino ratios
wealthv <- cbind(respv, pnls)
colnames(wealthv) <- c("VTI", "Kitchen sink")
sqrt(252)*sapply(wealthv[insample, ], function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
sqrt(252)*sapply(wealthv[outsample, ], function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of the autoregressive strategies
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
colorv <- colorRampPalette(c("blue", "red"))(NCOL(wealthv))
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Kitchen Sink Autoregressive Strategy") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyEvent(datev[cutoff], label="cutoff", strokePattern="solid", color="red") %>%
  dyLegend(show="always", width=300)

# Calculate the in-sample SVD
svdec <- svd(predm[insample, ])
# Calculate the in-sample fitted AR coefficients for different dimensions
dimv <- 2:5
# dimv <- c(2, 5, 10, NCOL(predm))
coeffm <- sapply(dimv, function(dimax) {
  predinv <- svdec$v[, 1:dimax] %*%
    (t(svdec$u[, 1:dimax]) / svdec$d[1:dimax])
  predinv %*% respv[insample]
})  # end lapply
colnames(coeffm) <- paste0("dimax=", dimv)
colorv <- colorRampPalette(c("red", "blue"))(NCOL(coeffm))
matplot(y=coeffm, type="l", lty="solid", lwd=1, col=colorv,
  xlab="predictor", ylab="coeff",
  main="AR Coefficients For Different Dimensions")
# Calculate the forecasts of VTI
fcasts <- predm %*% coeffm
fcasts <- apply(fcasts, 2, function(x) {
  fcastv <- sqrt(HighFreq::run_var(matrix(x), lambda=0.8)[, 2])
  fcastv[1:100] <- 1
  x/fcastv
}) # end apply
# Simulate the autoregressive strategies
retn <- coredata(respv)
pnls <- apply(fcasts, 2, function(x) (x*retn))
pnls <- xts(pnls, datev)
# Scale the PnL volatility to that of VTI
pnls <- lapply(pnls, function(x) x/sd(x))
pnls <- sd(respv)*do.call(cbind, pnls)

# Calculate the in-sample and out-of-sample Sharpe and Sortino ratios
wealthv <- cbind(respv, pnls)
sqrt(252)*sapply(wealthv[insample, ], function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
sqrt(252)*sapply(wealthv[outsample, ], function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of the autoregressive strategies
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
colorv <- colorRampPalette(c("blue", "red"))(NCOL(wealthv))
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Kitchen Sink Strategies With Dimension Reduction") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyEvent(datev[cutoff], label="cutoff", strokePattern="solid", color="red") %>%
  dyLegend(show="always", width=500)

# Objective function for the in-sample AR coefficients
objfun <- function(coeff, respv, predm, lambdaf) {
  fcasts <- predm %*% coeff
  10000*sum((respv - fcasts)^2) + sum(lambdaf*coeff^2)
}  # end objfun

# Perform optimization using the quasi-Newton method
ncoeff <- NROW(coeff)
optiml <- optim(par=numeric(ncoeff),
          fn=objfun,
          respv=respv[insample, ],
          predm=predm[insample, ],
          lambdaf=2.0,
          method="L-BFGS-B",
          upper=rep(10000, ncoeff),
          lower=rep(-10000, ncoeff))
# Extract the AR coefficients
coeff <- optiml$par
names(coeff) <- coeffn
barplot(coeff ~ coeffn, xlab="", ylab="t-value", col="grey",
  main="AR Coefficients With Shrinkage")

# Calculate the forecasts of VTI (fitted values)
fcasts <- predm %*% coeff
fcastv <- sqrt(HighFreq::run_var(fcasts, lambda=0.8)[, 2])
fcastv[1:100] <- 1
fcasts <- fcasts/fcastv
# Calculate the autoregressive strategy PnLs
pnls <- respv*fcasts
pnls <- pnls*sd(respv[respv<0])/sd(pnls[pnls<0])

# Calculate the in-sample and out-of-sample Sharpe and Sortino ratios
wealthv <- cbind(respv, pnls)
colnames(wealthv) <- c("VTI", "AR_multifact")
sqrt(252)*sapply(wealthv[insample, ], function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
sqrt(252)*sapply(wealthv[outsample, ], function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of the autoregressive strategies
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
colorv <- colorRampPalette(c("blue", "red"))(NCOL(wealthv))
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Multifactor Autoregressive Strategy With Shrinkage") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyEvent(datev[cutoff], label="cutoff", strokePattern="solid", color="red") %>%
  dyLegend(show="always", width=300)

# Simulate a Brownian motion path
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
pathv <- cumsum(rnorm(nrows))
plot(pathv, type="l", xlab="time", ylab="path",
     main="Brownian Motion")

# Plot the density of Brownian Motion
curve(expr=dnorm(x), xlim=c(-4, 4), ylim=c(0, 0.9),
  xlab="B_T", ylab="density", lwd=2, col="blue")
# Plot the density of the maximum of Brownian Motion
curve(expr=2*dnorm(x), xlim=c(0, 4), xlab="", ylab="",
  lwd=2, col="red", add=TRUE)
lines(x=c(0, 0), y=c(0, sqrt(2/pi)), lwd=2, col="red")
lines(x=c(-4, 0), y=c(0, 0), lwd=2, col="red")
title(main="Probability Density of
The Maximum Value of Brownian Motion", line=0.5)
legend("topright", inset=0.0, bty="n", y.intersp=0.4,
 title=NULL, c("Brownian", "Max"), lwd=6,
 col=c("blue", "red"))

# Series element
fun1 <- function(n, r) { 2*sin((n-0.5)*pi)/((n-0.5)*pi) *
  (1-exp(-((n-0.5)^2)*pi^2/2/r^2)) }
# fun2 <- function(x) { sum(sapply(1:10, function(n) fun1(n, x))) }
# fun2 <- function(x) { fun1(1, x) + fun1(2, x) + fun1(3, x) + fun1(4, x) + fun1(5, x) + fun1(6, x) }
# Sum of fun1
fun2 <- function(x) {
  valf <- 0
  for (n in 1:20) { valf <- valf + fun1(n, x) }
  return(valf)
  } # end fun2
# Theoretical average value of the range
fun2(2)
# Average value of the range from integration (not quite close)
integrate(fun2, lower=0.01, upper=4)

# Plot the density of Brownian Motion
curve(expr=dnorm(x), xlim=c(-4, 4), ylim=c(0, 1.0),
  xlab="B_T", ylab="density", lwd=2, col="blue")
# Plot the density of the range of Brownian Motion
curve(expr=fun2(x), xlim=c(0, 4), xlab="", ylab="",
  lwd=2, col="red", add=TRUE)
lines(x=c(0, 0), y=c(0, fun2(0.01)), lwd=2, col="red")
lines(x=c(-4, 0), y=c(0, 0), lwd=2, col="red")
title(main="Probability Density of
The Range of Brownian Motion", line=0.5)
legend("topright", inset=0.0, bty="n", y.intersp=0.7,
 title=NULL, c("Brownian", "Range"), lwd=6,
 col=c("blue", "red"))

# Define the daily volatility and growth rate
sigmav <- 0.01; drift <- 0.0; nrows <- 1000
# Simulate geometric Brownian motion
retp <- sigmav*rnorm(nrows) + drift - sigmav^2/2
pricev <- exp(cumsum(retp))
plot(pricev, type="l", xlab="time", ylab="prices",
     main="Geometric Brownian Motion")

# Standard deviations of log-normal distribution
sigmavs <- c(0.5, 1, 1.5)
# Create plot colors
colorv <- c("black", "red", "blue")
# Plot all curves
for (indeks in 1:NROW(sigmavs)) {
  curve(expr=dlnorm(x, sdlog=sigmavs[indeks]),
  type="l", lwd=2, xlim=c(0, 3),
  xlab="", ylab="", col=colorv[indeks],
  add=as.logical(indeks-1))
}  # end for

# Add title and legend
title(main="Log-normal Distributions", line=0.5)
legend("topright", inset=0.05, title="Sigmas",
 paste("sigma", sigmavs, sep="="),
 cex=0.8, lwd=2, lty=rep(1, NROW(sigmavs)),
 col=colorv)

x11(width=6, height=4)
par(mar=c(4, 4, 3, 1))
# Return volatility of VTI etf
sigmav <- sd(rutils::diffit(log(rutils::etfenv$VTI[, 4])))
sigma2 <- sigmav^2
nrows <- NROW(rutils::etfenv$VTI)
# Standard deviation of log-normal prices
sqrt(nrows)*sigmav

# Skewness of log-normal prices
calcskew <- function(t) {
  expv <- exp(t*sigma2)
  (expv + 2)*sqrt(expv - 1)
}  # end calcskew
curve(expr=calcskew, xlim=c(1, nrows), lwd=3,
xlab="Number of days", ylab="Skewness", col="blue",
main="Skewness of Log-normal Prices
as a Function of Time")

# Probability that random log-normal price will be lower than the mean price
curve(expr=pnorm(sigmav*sqrt(x)/2),
xlim=c(1, nrows), lwd=3,
xlab="Number of days", ylab="Probability", col="blue",
main="Probability That Random Log-normal Price
Will be Lower Than the Mean Price")

# Define the daily volatility and growth rate
sigmav <- 0.01; drift <- 0.0; nrows <- 5000
npaths <- 10
# Simulate multiple paths of geometric Brownian motion
pricev <- rnorm(npaths*nrows, sd=sigmav) + drift - sigmav^2/2
pricev <- matrix(pricev, nc=npaths)
pricev <- exp(matrixStats::colCumsums(pricev))
# Create xts time series
pricev <- xts(pricev, order.by=seq.Date(Sys.Date()-nrows+1, Sys.Date(), by=1))
# Sort the columns according to largest terminal values
pricev <- pricev[, order(pricev[nrows, ])]
# Plot xts time series
colorv <- colorRampPalette(c("red", "blue"))(NCOL(pricev))
par(mar=c(3, 3, 2, 2), oma=c(0, 0, 0, 0))
plot.zoo(pricev, main="Multiple paths of geometric Brownian motion",
   xlab=NA, ylab=NA, plot.type="single", col=colorv)

# Define the daily volatility and growth rate
sigmav <- 0.01; drift <- 0.0; nrows <- 10000
npaths <- 100
# Simulate multiple paths of geometric Brownian motion
pricev <- rnorm(npaths*nrows, sd=sigmav) + drift - sigmav^2/2
pricev <- matrix(pricev, nc=npaths)
pricev <- exp(matrixStats::colCumsums(pricev))
# Calculate fraction of paths below the expected value
fractv <- rowSums(pricev < 1.0) / npaths
# Create xts time series of percentage of paths below the expected value
fractv <- xts(fractv, order.by=seq.Date(Sys.Date()-NROW(fractv)+1, Sys.Date(), by=1))
# Plot xts time series of percentage of paths below the expected value
par(mar=c(3, 3, 2, 2), oma=c(0, 0, 0, 0))
plot.zoo(fractv, main="Percentage of GBM paths below mean",
   xlab=NA, ylab=NA, col="blue")

# Load S&P500 stock prices
load("/Users/jerzy/Develop/lecture_slides/data/sp500.RData")
ls(sp500env)
# Extract the closing prices
pricev <- eapply(sp500env, quantmod::Cl)
# Flatten the prices into a single xts series
pricev <- rutils::do_call(cbind, pricev)
# Carry forward and backward non-NA prices
pricev <- zoo::na.locf(pricev, na.rm=FALSE)
pricev <- zoo::na.locf(pricev, fromLast=TRUE)
sum(is.na(pricev))
# Drop ".Close" from column names
colnames(pricev)
colnames(pricev) <- rutils::get_name(colnames(pricev))
# Or
# colnames(pricev) <- do.call(rbind,
#   strsplit(colnames(pricev), split="[.]"))[, 1]
# Select prices after the year 2000
pricev <- pricev["2000/", ]
# Scale the columns so that prices start at 1
pricev <- lapply(pricev, function(x) x/as.numeric(x[1]))
pricev <- rutils::do_call(cbind, pricev)
# Sort the columns according to the final prices
nrows <- NROW(pricev)
ordern <- order(pricev[nrows, ])
pricev <- pricev[, ordern]
# Select 20 symbols
symbolv <- colnames(pricev)
symbolv <- symbolv[round(seq.int(from=1, to=NROW(symbolv), length.out=20))]

# Plot xts time series of prices
colorv <- colorRampPalette(c("red", "blue"))(NROW(symbolv))
endd <- rutils::calc_endpoints(pricev, interval="weeks")
plot.zoo(pricev[endd, symbolv], main="20 S&P500 Stock Prices (scaled)",
   xlab=NA, ylab=NA, plot.type="single", col=colorv)
legend(x="topleft", inset=0.02, cex=0.5, bty="n", y.intersp=0.5,
 legend=rev(symbolv), col=rev(colorv), lwd=6, lty=1)

# Calculate the final stock prices
pricef <- drop(zoo::coredata(pricev[nrows, ]))
# Calculate the mean and median stock prices
max(pricef); min(pricef)
which.max(pricef)
which.min(pricef)
mean(pricef)
median(pricef)
# Calculate the percentage of stock prices below the mean
sum(pricef < mean(pricef))/NROW(pricef)

# Plot a histogram of final stock prices
hist(pricef, breaks=1e3, xlim=c(0, 300),
     xlab="Stock price", ylab="Count",
     main="Histogram of Final Stock Prices")
# Plot a histogram of final stock prices
abline(v=median(pricef), lwd=3, col="blue")
text(x=median(pricef), y=150, lab="median", pos=4)
abline(v=mean(pricef), lwd=3, col="red")
text(x=mean(pricef), y=100, lab="mean", pos=4)

# Calculate average of valid stock prices
validp <- (pricev != 1)  # Valid stocks
nstocks <- rowSums(validp)
nstocks[1] <- NCOL(pricev)
indeks <- rowSums(pricev*validp)/nstocks
# Calculate fraction of stock prices below the average price
fractv <- rowSums((pricev < indeks) & validp)/nstocks
# Create xts time series of average stock prices
indeks <- xts(indeks, order.by=zoo::index(pricev))

dev.new(width=6, height=4, noRStudioGD=TRUE)
# x11(width=6, height=4)
# Plot xts time series of average stock prices
plot.zoo(indeks, main="Average S&P500 Stock Prices (normalized from 1990)",
   xlab=NA, ylab=NA, col="blue")
# Create xts time series of percentage of stock prices below the average price
fractv <- xts(fractv, order.by=zoo::index(pricev))
# Plot percentage of stock prices below the average price
plot.zoo(fractv[-(1:2),],
   main="Percentage of S&P500 Stock Prices
   Below the Average Price",
   xlab=NA, ylab=NA, col="blue")

# Open plot window under MS Windows
x11(width=6, height=4)
par(mar=c(3, 2, 1, 1), oma=c(1, 0, 0, 0))
# Calculate VTI percentage returns
retp <- na.omit(rutils::etfenv$returns$VTI)
retp <- drop(zoo::coredata(retp))
# Plot autocorrelations of VTI returns using stats::acf()
stats::acf(retp, lag=10, xlab="lag", main="")
title(main="ACF of VTI Returns", line=-1)
# Calculate two-tailed 95% confidence interval
qnorm(0.975)/sqrt(NROW(retp))

# Get the ACF data returned invisibly
acfl <- acf(retp, plot=FALSE)
summary(acfl)
# Print the ACF data
print(acfl)
dim(acfl$acf)
dim(acfl$lag)
head(acfl$acf)

plot_acf <- function(xtsv, lagg=10, plotobj=TRUE,
               xlab="Lag", ylab="", main="", ...) {
  # Calculate the ACF without a plot
  acfl <- acf(x=xtsv, lag.max=lagg, plot=FALSE, ...)
  # Remove first element of ACF data
  acfl$acf <- array(data=acfl$acf[-1],
    dim=c((dim(acfl$acf)[1]-1), 1, 1))
  acfl$lag <- array(data=acfl$lag[-1],
    dim=c((dim(acfl$lag)[1]-1), 1, 1))
  # Plot ACF
  if (plotobj) {
    ci <- qnorm((1+0.95)/2)/sqrt(NROW(xtsv))
    ylim <- c(min(-ci, range(acfl$acf[-1])),
        max(ci, range(acfl$acf[-1])))
    plot(acfl, xlab=xlab, ylab=ylab,
   ylim=ylim, main="", ci=0)
    title(main=main, line=0.5)
    abline(h=c(-ci, ci), col="blue", lty=2)
  }  # end if
  # Return the ACF data invisibly
  invisible(acfl)
}  # end plot_acf

# Autocorrelations of VTI returns
rutils::plot_acf(retp, lag=10, main="ACF of VTI returns")

# Ljung-Box test for VTI returns
# 'lag' is the number of autocorrelation coefficients
Box.test(retp, lag=10, type="Ljung")
# Ljung-Box test for random returns
Box.test(rnorm(NROW(retp)), lag=10, type="Ljung")
library(Ecdat)  # Load Ecdat
macrodata <- as.zoo(Macrodat[, c("lhur", "fygm3")])
colnames(macrodata) <- c("unemprate", "3mTbill")
macrodiff <- na.omit(diff(macrodata))
# Changes in 3 month T-bill rate are autocorrelated
Box.test(macrodiff[, "3mTbill"], lag=10, type="Ljung")
# Changes in unemployment rate are autocorrelated
Box.test(macrodiff[, "unemprate"], lag=10, type="Ljung")

# Open plot window under MS Windows
x11(width=6, height=7)
# Set two vertical plot panels
par(mfrow=c(2,1))
par(mar=c(3, 3, 2, 2), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
# Plot ACF of squared random returns
rutils::plot_acf(rnorm(NROW(retp))^2, lag=10,
 main="ACF of Squared Random Returns")
# Plot ACF of squared VTI returns
rutils::plot_acf(retp^2, lag=10,
 main="ACF of Squared VTI Returns")
# Ljung-Box test for squared VTI returns
Box.test(retp^2, lag=10, type="Ljung")

# Calculate the monthly end points
endd <- rutils::calc_endpoints(retp, interval="weeks")
npts <- NROW(endd)
# Calculate the monthly VTI volatilities and their median volatility
stdev <- sapply(2:npts, function(endp) {
  sd(retp[endd[endp-1]:endd[endp]])
})  # end sapply
medianv <- median(stdev)
# Calculate the stock returns of low volatility intervals
retlow <- lapply(2:npts, function(endp) {
  if (stdev[endp-1] <= medianv)
    retp[endd[endp-1]:endd[endp]]
})  # end lapply
retlow <- rutils::do_call(c, retlow)
# Calculate the stock returns of high volatility intervals
rethigh <- lapply(2:npts, function(endp) {
  if (stdev[endp-1] > medianv)
    retp[endd[endp-1]:endd[endp]]
})  # end lapply
rethigh <- rutils::do_call(c, rethigh)
# Plot ACF of low volatility returns
rutils::plot_acf(retlow, lag=10,
 main="ACF of Low Volatility Returns")
Box.test(retlow, lag=10, type="Ljung")
# Plot ACF of high volatility returns
rutils::plot_acf(rethigh, lag=10,
 main="ACF of High Volatility Returns")
Box.test(rethigh, lag=10, type="Ljung")

NA

# Load daily S&P500 stock returns
load(file="/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
# Calculate the stock volatilities and Ljung-Box test statistics
library(parallel)  # Load package parallel
ncores <- detectCores() - 1
statm <- mclapply(returns, function(retp) {
  retp <- na.omit(retp)
  c(stdev=sd(retp), lbstat=Box.test(retp, lag=10, type="Ljung")$statistic)
}, mc.cores=ncores)  # end mclapply
statm <- do.call(rbind, statm)
colnames(statm)[2] <- "lbstat"
# Calculate the median volatility
stdev <- statm[, "stdev"]
lbstat <- statm[, "lbstat"]
stdevm <- median(stdev)
# Calculate the Ljung-Box statistics for stock volatility quantiles
quants <- quantile(stdev, c(0.001, seq(0.1, 0.9, 0.1), 0.999))
lbstatq <- sapply(2:NROW(quants), function(it) {
  mean(lbstat[(stdev > quants[it-1]) & (stdev < quants[it])])
}) # end sapply
# Calculate the Ljung-Box statistics for low and high volatility stocks
lowvol <- (stdev < stdevm)
mean(statm[lowvol, "lbstat"])
mean(statm[!lowvol, "lbstat"])
# Compare the Ljung-Box statistics for lowest volatility stocks with VTI
lbstatq[1]
Box.test(na.omit(rutils::etfenv$returns$VTI), lag=10, type="Ljung")$statistic

# Plot Ljung-Box test statistic for volatility quantiles
plot(x=quants[-NROW(quants)], y=lbstatq, lwd=1, col="blue",
     # xlim=c(0.01, 0.05), ylim=c(0, 100),
     xlab="volatility", ylab="Ljung-Box Stat",
     main="Ljung-Box Statistic For Stock Volatility Quantiles")

# Calculate SPY log prices and percentage returns
ohlc <- HighFreq::SPY
ohlc[, 1:4] <- log(ohlc[, 1:4])
nrows <- NROW(ohlc)
closep <- quantmod::Cl(ohlc)
retp <- rutils::diffit(closep)
colnames(retp) <- "SPY"
# Open plot window under MS Windows
x11(width=6, height=4)
# Open plot window on Mac
dev.new(width=6, height=4, noRStudioGD=TRUE)
# Plot the autocorrelations of minutely SPY returns
acfl <- rutils::plot_acf(as.numeric(retp), lag=10,
     xlab="lag", ylab="Autocorrelation", main="")
title("Autocorrelations of Minutely SPY Returns", line=1)
# Calculate the sum of autocorrelations
sum(acfl$acf)

# Ljung-Box test for minutely SPY returns
Box.test(retp, lag=10, type="Ljung")
# Calculate hourly SPY percentage returns
closeh <- quantmod::Cl(xts::to.period(x=ohlc, period="hours"))
retsh <- rutils::diffit(closeh)
# Ljung-Box test for hourly SPY returns
Box.test(retsh, lag=10, type="Ljung")
# Calculate daily SPY percentage returns
closed <- quantmod::Cl(xts::to.period(x=ohlc, period="days"))
retd <- rutils::diffit(closed)
# Ljung-Box test for daily SPY returns
Box.test(retd, lag=10, type="Ljung")

# Ljung-Box test statistics for aggregated SPY returns
lbstat <- sapply(list(daily=retd, hourly=retsh, minutely=retp),
  function(rets) {
    Box.test(rets, lag=10, type="Ljung")$statistic
})  # end sapply
# Plot Ljung-Box test statistic for different aggregation intervals
plot(lbstat, lwd=6, col="blue", xaxt="n",
     xlab="Aggregation interval", ylab="Ljung-Box Stat",
     main="Ljung-Box Statistic For Different Aggregations")
# Add X-axis with labels
axis(side=1, at=(1:3), labels=c("daily", "hourly", "minutely"))

# Daily SPY volatility from daily returns
sd(retd)
# Minutely SPY volatility scaled to daily interval
sqrt(6.5*60)*sd(retp)
# Minutely SPY returns without overnight price jumps (unit per second)
retp <- retp/rutils::diffit(xts::.index(retp))
retp[1] <- 0
# Daily SPY volatility from minutely returns
sqrt(6.5*60)*60*sd(retp)
# Daily SPY returns without weekend and holiday price jumps (unit per second)
retd <- retd/rutils::diffit(xts::.index(retd))
retd[1] <- 0
# Daily SPY volatility without weekend and holiday price jumps
24*60*60*sd(retd)

# Calculate volatilities for vector of aggregation intervals
aggv <- seq.int(from=3, to=35, length.out=9)^2
volv <- sapply(aggv, function(agg) {
  naggs <- nrows %/% agg
  endd <- c(0, nrows - naggs*agg + (0:naggs)*agg)
  # endd <- rutils::calc_endpoints(closep, interval=agg)
  sd(rutils::diffit(closep[endd]))
})  # end sapply
# Calculate the Hurst from single data point
volog <- log(volv)
agglog <- log(aggv)
(last(volog) - first(volog))/(last(agglog) - first(agglog))
# Calculate the Hurst from regression slope using formula
hurstexp <- cov(volog, agglog)/var(agglog)
# Or using function lm()
model <- lm(volog ~ agglog)
coef(model)[2]

# Plot the volatilities
x11(width=6, height=4)
par(mar=c(4, 4, 2, 1), oma=c(1, 1, 1, 1))
plot(volog ~ agglog, lwd=6, col="red",
     xlab="Aggregation intervals (log)", ylab="Volatility (log)",
     main="Hurst Exponent for SPY From Volatilities")
abline(model, lwd=3, col="blue")
text(agglog[2], volog[NROW(volog)-1],
     paste0("Hurst = ", round(hurstexp, 4)))

# Calculate cumulative SPY returns
closep <- cumsum(retp)
nrows <- NROW(closep)
# Calculate the rescaled range
agg <- 500
naggs <- nrows %/% agg
endd <- c(0, nrows - naggs*agg + (0:naggs)*agg)
# Or
# endd <- rutils::calc_endpoints(closep, interval=agg)
rrange <- sapply(2:NROW(endd), function(np) {
  indeks <- (endd[np-1]+1):endd[np]
  diff(range(closep[indeks]))/sd(retp[indeks])
})  # end sapply
mean(rrange)
# Calculate the Hurst from single data point
log(mean(rrange))/log(agg)

# Calculate the rescaled range for vector of aggregation intervals
rrange <- sapply(aggv, function(agg) {
# Calculate the end points
  naggs <- nrows %/% agg
  endd <- c(0, nrows - naggs*agg + (0:naggs)*agg)
# Calculate the rescaled ranges
  rrange <- sapply(2:NROW(endd), function(np) {
    indeks <- (endd[np-1]+1):endd[np]
    diff(range(closep[indeks]))/sd(retp[indeks])
  })  # end sapply
  mean(na.omit(rrange))
})  # end sapply
# Calculate the Hurst as regression slope using formula
rangelog <- log(rrange)
agglog <- log(aggv)
hurstexp <- cov(rangelog, agglog)/var(agglog)
# Or using function lm()
model <- lm(rangelog ~ agglog)
coef(model)[2]

x11(width=6, height=4)
par(mar=c(4, 4, 2, 1), oma=c(1, 1, 1, 1))
plot(rangelog ~ agglog, lwd=6, col="red",
     xlab="aggregation intervals (log)",
     ylab="rescaled range (log)",
     main="Hurst Exponent for SPY From Rescaled Range")
abline(model, lwd=3, col="blue")
text(agglog[2], rangelog[NROW(rangelog)-1],
     paste0("Hurst = ", round(hurstexp, 4)))

# Load S&P500 constituent OHLC stock prices
load("/Users/jerzy/Develop/lecture_slides/data/sp500.RData")
class(sp500env$AAPL)
head(sp500env$AAPL)
# Calculate log stock prices after the year 2000
pricev <- eapply(sp500env, function(ohlc) {
  closep <- log(quantmod::Cl(ohlc)["2000/"])
# Ignore short lived and penny stocks (less than $1)
  if ((NROW(closep) > 4000) & (last(closep) > 0))
    return(closep)
})  # end eapply
# Calculate the number of NULL prices
sum(sapply(pricev, is.null))
# Calculate the names of the stocks (remove NULL pricev)
namev <- sapply(pricev, is.null)
namev <- namev[!namev]
namev <- names(namev)
pricev <- pricev[namev]
# Calculate the Hurst exponents of stocks
aggv <- trunc(seq.int(from=3, to=10, length.out=5)^2)
hurstv <- sapply(pricev, HighFreq::calc_hurst, aggv=aggv)
# Dygraph of stock with largest Hurst exponent
namev <- names(which.max(hurstv))
dygraphs::dygraph(get(namev, pricev), main=namev)
# Dygraph of stock with smallest Hurst exponent
namev <- names(which.min(hurstv))
dygraphs::dygraph(get(namev, pricev), main=namev)

# Plot a histogram of the Hurst exponents of stocks
hist(hurstv, breaks=20, xlab="Hurst", ylab="Count",
     main="Hurst Exponents of Stocks")
# Add vertical line for H = 0.5
abline(v=0.5, lwd=3, col='red')
text(x=0.5, y=50, lab="H = 0.5", pos=4)

# Calculate the volatility of stocks
volv <- sapply(pricev, function(closep) {
    sqrt(HighFreq::calc_var(HighFreq::diffit(closep)))
})  # end sapply
# Dygraph of stock with highest volatility
namev <- names(which.max(volv))
dygraphs::dygraph(get(namev, pricev), main=namev)
# Dygraph of stock with lowest volatility
namev <- names(which.min(volv))
dygraphs::dygraph(get(namev, pricev), main=namev)
# Calculate the regression of the Hurst exponents versus volatilities
model <- lm(hurstv ~ volv)
summary(model)

# Plot scatterplot of the Hurst exponents versus volatilities
plot(hurstv ~ volv, xlab="Volatility", ylab="Hurst",
     main="Hurst Exponents Versus Volatilities of Stocks")
# Add regression line
abline(model, col='red', lwd=3)
tvalue <- summary(model)$coefficients[2, "t value"]
tvalue <- round(tvalue, 3)
text(x=mean(volv), y=max(hurstv),
     lab=paste("t-value =", tvalue), lwd=2, cex=1.2)

# Calculate the in-sample volatility of stocks
volatis <- sapply(pricev, function(closep) {
    sqrt(HighFreq::calc_var(HighFreq::diffit(closep["/2010"])))
})  # end sapply
# Calculate the out-of-sample volatility of stocks
volatos <- sapply(pricev, function(closep) {
    sqrt(HighFreq::calc_var(HighFreq::diffit(closep["2010/"])))
})  # end sapply
# Calculate the regression of the out-of-sample versus in-sample volatility
model <- lm(volatos ~ volatis)
summary(model)

# Plot scatterplot of the out-of-sample versus in-sample volatility
plot(volatos ~ volatis, xlab="In-sample Volatility", ylab="Out-of-sample Volatility",
     main="Out-of-Sample Versus In-Sample Volatility of Stocks")
# Add regression line
abline(model, col='red', lwd=3)
tvalue <- summary(model)$coefficients[2, "t value"]
tvalue <- round(tvalue, 3)
text(x=mean(volatis), y=max(volatos),
     lab=paste("t-value =", tvalue), lwd=2, cex=1.2)

# Calculate the in-sample Hurst exponents of stocks
hurstis <- sapply(pricev, function(closep) {
  HighFreq::calc_hurst(closep["/2010"], aggv=aggv)
})  # end sapply
# Calculate the out-of-sample Hurst exponents of stocks
hurstos <- sapply(pricev, function(closep) {
  HighFreq::calc_hurst(closep["2010/"], aggv=aggv)
})  # end sapply
# Calculate the regression of the out-of-sample versus in-sample Hurst exponents
model <- lm(hurstos ~ hurstis)
summary(model)

# Plot scatterplot of the out-of-sample versus in-sample Hurst exponents
plot(hurstos ~ hurstis, xlab="In-sample Hurst", ylab="Out-of-sample Hurst",
     main="Out-of-Sample Versus In-Sample Hurst Exponents of Stocks")
# Add regression line
abline(model, col='red', lwd=3)
tvalue <- summary(model)$coefficients[2, "t value"]
tvalue <- round(tvalue, 3)
text(x=mean(hurstis), y=max(hurstos),
     lab=paste("t-value =", tvalue), lwd=2, cex=1.2)

# Calculate the stock trading volumes after the year 2000
volum <- eapply(sp500env, function(ohlc) {
    sum(quantmod::Vo(ohlc)["2000/"])
})  # end eapply
# Remove NULL values
volum <- volum[names(pricev)]
volum <- unlist(volum)
which.max(volum)
# Calculate the number of NULL prices
sum(is.null(volum))
# Calculate the Hurst exponents of stocks
hurstv <- sapply(pricev, HighFreq::calc_hurst, aggv=aggv)
# Calculate the regression of the Hurst exponents versus trading volumes
model <- lm(hurstv ~ volum)
summary(model)

# Plot scatterplot of the Hurst exponents versus trading volumes
plot(hurstv ~ volum, xlab="Trading Volume", ylab="Hurst",
     main="Hurst Exponents Versus Trading Volumes of Stocks")
# Add regression line
abline(model, col='red', lwd=3)
tvalue <- summary(model)$coefficients[2, "t value"]
tvalue <- round(tvalue, 3)
text(x=quantile(volum, 0.998), y=max(hurstv),
     lab=paste("t-value =", tvalue), lwd=2, cex=1.2)

# Calculate log stock returns
retp <- lapply(pricev, rutils::diffit)
retp <- rutils::do_call(cbind, retp)
retp[is.na(retp)] <- 0
sum(is.na(retp))
# Drop ".Close" from column names
colnames(retp[, 1:4])
colnames(retp) <- rutils::get_name(colnames(retp))
# Calculate PCA prices using matrix algebra
eigend <- eigen(cor(retp))
retpca <- retp %*% eigend$vectors
pricepca <- xts::xts(matrixStats::colCumsums(retpca),
                 order.by=index(retp))
colnames(pricepca) <- paste0("PC", 1:NCOL(retp))
# Calculate the Hurst exponents of PCAs
hurstv <- sapply(pricepca, HighFreq::calc_hurst, aggv=aggv)
# Dygraph of PCA with largest Hurst exponent
namev <- names(which.max(hurstv))
dygraphs::dygraph(get(namev, pricepca), main=namev)
# Dygraph of PCA with smallest Hurst exponent
namev <- names(which.min(hurstv))
dygraphs::dygraph(get(namev, pricepca), main=namev)

# Plot the Hurst exponents of principal components without x-axis
plot(hurstv, xlab=NA, ylab=NA, xaxt="n",
     main="Hurst Exponents of Principal Components")
# Add X-axis with PCA labels
axis(side=1, at=(1:NROW(hurstv)), labels=names(hurstv))
# Calculate the regression of the PCA Hurst exponents versus their order
orderv <- 1:NROW(hurstv)
model <- lm(hurstv ~ orderv)
summary(model)
# Add regression line
abline(model, col='red', lwd=3)
tvalue <- summary(model)$coefficients[2, "t value"]
tvalue <- round(tvalue, 3)
text(x=mean(orderv), y=max(hurstv),
     lab=paste("t-value =", tvalue), lwd=2, cex=1.2)

# Calculate in-sample eigen decomposition using matrix algebra
eigend <- eigen(cor(retp["/2010"]))
# Calculate out-of-sample PCA prices
retpca <- retp["2010/"] %*% eigend$vectors
pricepca <- xts::xts(matrixStats::colCumsums(retpca),
                 order.by=index(retp["2010/"]))
colnames(pricepca) <- paste0("PC", 1:NCOL(retp))
# Calculate the Hurst exponents of PCAs
hurstv <- sapply(pricepca, HighFreq::calc_hurst, aggv=aggv)
# Dygraph of PCA with largest Hurst exponent
namev <- names(which.max(hurstv))
dygraphs::dygraph(get(namev, pricepca), main=namev)
# Dygraph of PCA with smallest Hurst exponent
namev <- names(which.min(hurstv))
dygraphs::dygraph(get(namev, pricepca), main=namev)

# Plot the Hurst exponents of principal components without x-axis
plot(hurstv, xlab=NA, ylab=NA, xaxt="n",
     main="Out-of-Sample Hurst Exponents of Principal Components")
# Add X-axis with PCA labels
axis(side=1, at=(1:NROW(hurstv)), labels=names(hurstv))
# Calculate the regression of the PCA Hurst exponents versus their order
model <- lm(hurstv ~ orderv)
summary(model)
# Add regression line
abline(model, col='red', lwd=3)
tvalue <- summary(model)$coefficients[2, "t value"]
tvalue <- round(tvalue, 3)
text(x=mean(orderv), y=max(hurstv),
     lab=paste("t-value =", tvalue), lwd=2, cex=1.2)

# Get ETF log prices
symbolv <- rutils::etfenv$symbolv
symbolv <- symbolv[!(symbolv %in% c("MTUM", "QUAL", "VLUE", "USMV"))]
pricev <- lapply(mget(symbolv, rutils::etfenv), function(x) {
  log(na.omit(quantmod::Cl(x)))
})  # end lapply
# Calculate the Hurst exponents of ETFs
aggv <- trunc(seq.int(from=3, to=10, length.out=5)^2)
hurstv <- sapply(pricev, HighFreq::calc_hurst, aggv=aggv)
hurstv <- sort(unlist(hurstv))
# Dygraph of ETF with smallest Hurst exponent
namev <- names(first(hurstv))
dygraphs::dygraph(get(namev, pricev), main=namev)
# Dygraph of ETF with largest Hurst exponent
namev <- names(last(hurstv))
dygraphs::dygraph(get(namev, pricev), main=namev)

# Plot a histogram of the Hurst exponents of stocks
hist(hurstv, breaks=2e1, xlab="Hurst", ylab="Count",
     main="Hurst Exponents of ETFs")
# Add vertical line for H = 0.5
abline(v=0.5, lwd=3, col='red')
text(x=0.5, y=50, lab="H = 0.5", pos=4)

# Calculate log ETF returns
symbolv <- rutils::etfenv$symbolv
symbolv <- symbolv[!(symbolv %in% c("MTUM", "QUAL", "VLUE", "USMV"))]
retp <- rutils::etfenv$returns[, symbolv]
retp[is.na(retp)] <- 0
sum(is.na(retp))
# Calculate the Hurst exponent of an ETF portfolio
calc_phurst <- function(weightv, retp) {
  -HighFreq::calc_hurst(matrix(cumsum(retp %*% weightv)), aggv=aggv)
}  # end calc_phurst
# Calculate the portfolio weights with maximum Hurst
nweights <- NCOL(retp)
weightv <- rep(1/sqrt(nweights), nweights)
calc_phurst(weightv, retp=retp)
optiml <- optim(par=weightv, fn=calc_phurst, retp=retp,
          method="L-BFGS-B",
          upper=rep(10.0, nweights),
          lower=rep(-10.0, nweights))
# Optimal weights and maximum Hurst
weightv <- optiml$par
names(weightv) <- colnames(retp)
-calc_phurst(weightv, retp=retp)

# Dygraph of ETF portfolio with largest Hurst exponent
wealthv <- xts::xts(cumsum(retp %*% weightv), zoo::index(retp))
dygraphs::dygraph(wealthv, main="ETF Portfolio With Largest Hurst Exponent")

# Calculate the in-sample maximum Hurst portfolio weights
optiml <- optim(par=weightv, fn=calc_phurst, retp=retp["/2010"],
          method="L-BFGS-B",
          upper=rep(10.0, nweights),
          lower=rep(-10.0, nweights))
# Optimal weights and maximum Hurst
weightv <- optiml$par
names(weightv) <- colnames(retp)
# Calculate the in-sample Hurst exponent
-calc_phurst(weightv, retp=retp["/2010"])
# Calculate the out-of-sample Hurst exponent
-calc_phurst(weightv, retp=retp["2010/"])

# Simulate AR processes
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")  # Reset random numbers
datev <- Sys.Date() + 0:728  # Two year daily series
# AR time series of returns
arimav <- xts(x=arima.sim(n=NROW(datev), model=list(ar=0.2)),
          order.by=datev)
arimav <- cbind(arimav, cumsum(arimav))
colnames(arimav) <- c("AR returns", "AR prices")

library(ggplot2)  # Load ggplot2
library(gridExtra)  # Load gridExtra
autoplot(object=arimav, # ggplot AR process
 facets="Series ~ .",
 main="Autoregressive process (phi=0.2)") +
  facet_grid("Series ~ .", scales="free_y") +
  xlab("") + ylab("") +
theme(legend.position=c(0.1, 0.5),
  plot.background=element_blank(),
  axis.text.y=element_blank())

coeff <- c(-0.9, 0.01, 0.9)  # AR coefficients
# Create three AR time series
arimav <- sapply(coeff, function(phi) {
  set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")  # Reset random numbers
  arima.sim(n=NROW(datev), model=list(ar=phi))
})  # end sapply
colnames(arimav) <- paste("autocorr", coeff)
plot.zoo(arimav, main="AR(1) prices", xlab=NA)
# Or plot using ggplot
arimav <- xts(x=arimav, order.by=datev)
library(ggplot)
autoplot(arimav, main="AR(1) prices",
   facets=Series ~ .) +
    facet_grid(Series ~ ., scales="free_y") +
xlab("") +
theme(
  legend.position=c(0.1, 0.5),
  plot.title=element_text(vjust=-2.0),
  plot.margin=unit(c(-0.5, 0.0, -0.5, 0.0), "cm"),
  plot.background=element_blank(),
  axis.text.y=element_blank())

# Define AR(3) coefficients and innovations
coeff <- c(0.1, 0.39, 0.5)
nrows <- 1e2
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection"); innov <- rnorm(nrows)
# Simulate AR process using recursive loop in R
arimav <- numeric(nrows)
arimav[1] <- innov[1]
arimav[2] <- coeff[1]*arimav[1] + innov[2]
arimav[3] <- coeff[1]*arimav[2] + coeff[2]*arimav[1] + innov[3]
for (it in 4:NROW(arimav)) {
  arimav[it] <- arimav[(it-1):(it-3)] %*% coeff + innov[it]
}  # end for
# Simulate AR process using filter()
arimaf <- filter(x=innov, filter=coeff, method="recursive")
class(arimaf)
all.equal(arimav, as.numeric(arimaf))
# Fast simulation of AR process using C_rfilter()
arimacpp <- .Call(stats:::C_rfilter, innov, coeff,
     double(NROW(coeff) + NROW(innov)))[-(1:3)]
all.equal(arimav, arimacpp)
# Fastest simulation of AR process using HighFreq::sim_ar()
arimav <- HighFreq::sim_ar(coeff=matrix(coeff), innov=matrix(innov))
arimav <- drop(arimav)
all.equal(arimav, arimacpp)
# Benchmark the speed of the three methods of simulating AR process
library(microbenchmark)
summary(microbenchmark(
  Rloop={for (it in 4:NROW(arimav)) {
    arimav[it] <- arimav[(it-1):(it-3)] %*% coeff + innov[it]
  }},
  filter=filter(x=innov, filter=coeff, method="recursive"),
  cpp=HighFreq::sim_ar(coeff=matrix(coeff), innov=matrix(innov))
  ), times=10)[, c(1, 4, 5)]

# Calculate modulus of roots of characteristic equation
rootv <- Mod(polyroot(c(1, -coeff)))
# Calculate warmup period
warmup <- NROW(coeff) + ceiling(6/log(min(rootv)))
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
nrows <- 1e4
innov <- rnorm(nrows + warmup)
# Simulate AR process using arima.sim()
arimav <- arima.sim(n=nrows,
  model=list(ar=coeff),
  start.innov=innov[1:warmup],
  innov=innov[(warmup+1):NROW(innov)])
# Simulate AR process using filter()
arimaf <- filter(x=innov, filter=coeff, method="recursive")
all.equal(arimaf[-(1:warmup)], as.numeric(arimav))
# Benchmark the speed of the three methods of simulating AR process
library(microbenchmark)
summary(microbenchmark(
  filter=filter(x=innov, filter=coeff, method="recursive"),
  arima_sim=arima.sim(n=nrows,
                  model=list(ar=coeff),
                  start.innov=innov[1:warmup],
                  innov=innov[(warmup+1):NROW(innov)]),
  arima_loop={for (it in 4:NROW(arimav)) {
  arimav[it] <- arimav[(it-1):(it-3)] %*% coeff + innov[it]}}
  ), times=10)[, c(1, 4, 5)]

x11(width=6, height=4)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0))
# Simulate AR(1) process
arimav <- arima.sim(n=1e3, model=list(ar=0.8))
# ACF of AR(1) process
acfl <- rutils::plot_acf(arimav, lag=10, xlab="", ylab="",
  main="Autocorrelations of AR(1) process")
acfl$acf[1:5]

# PACF of AR(1) process
pacfl <- pacf(arimav, lag=10, xlab="", ylab="", main="")
title("Partial autocorrelations of AR(1) process", line=1)
pacfl <- as.numeric(pacfl$acf)
pacfl[1:5]

library(rutils)  # Load rutils
library(ggplot2)  # Load ggplot2
# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
randw <- cumsum(zoo(matrix(rnorm(3*100), ncol=3),
order.by=(Sys.Date()+0:99)))
colnames(randw) <- paste("randw", 1:3, sep="_")
plot.zoo(randw, main="Random walks",
     xlab="", ylab="", plot.type="single",
     col=c("black", "red", "blue"))
# Add legend
legend(x="topleft", legend=colnames(randw),
 col=c("black", "red", "blue"), lty=1)

# Simulate arima with large AR coefficient
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
nrows <- 1e4
arimav <- arima.sim(n=nrows, model=list(ar=0.99))
tseries::adf.test(arimav)
# Integrated series has unit root
tseries::adf.test(cumsum(arimav))
# Simulate arima with negative AR coefficient
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
arimav <- arima.sim(n=nrows, model=list(ar=-0.99))
tseries::adf.test(arimav)
# Integrated series has unit root
tseries::adf.test(cumsum(arimav))

# Simulate random walks using apply() loops
# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
randws <- matrix(rnorm(1000*100), ncol=1000)
randws <- apply(randws, 2, cumsum)
varv <- apply(randws, 1, var)
# Simulate random walks using vectorized functions
# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
randws <- matrixStats::colCumsums(matrix(rnorm(1000*100), ncol=1000))
varv <- matrixStats::rowVars(randws)
par(mar=c(5, 3, 2, 2), oma=c(0, 0, 0, 0))
plot(varv, xlab="time steps", ylab="",
     t="l", col="blue", lwd=2,
     main="Variance of Random Walk")

# Define Brownian Motion parameters
nrows <- 1000; sigmav <- 0.01
# Simulate 5 paths of Brownian motion
pricev <- matrix(rnorm(5*nrows, sd=sigmav), nc=5)
pricev <- matrixStats::colCumsums(pricev)
# Plot 5 paths of Brownian motion
matplot(y=pricev, main="Brownian Motion Paths",
  xlab="time", ylab="path",
  type="l", lty="solid", lwd=1, col="blue")
# Save plot to png file on Mac
quartz.save("figure/brown_paths.png", type="png", width=6, height=4)

# Define Ornstein-Uhlenbeck parameters
prici <- 0.0; priceq <- 1.0;
sigmav <- 0.02; thetav <- 0.01; nrows <- 1000
# Initialize the data
innov <- rnorm(nrows)
retp <- numeric(nrows)
pricev <- numeric(nrows)
retp[1] <- sigmav*innov[1]
pricev[1] <- prici
# Simulate Ornstein-Uhlenbeck process in R
for (i in 2:nrows) {
  retp[i] <- thetav*(priceq - pricev[i-1]) + sigmav*innov[i]
  pricev[i] <- pricev[i-1] + retp[i]
}  # end for
# Simulate Ornstein-Uhlenbeck process in Rcpp
pricecpp <- HighFreq::sim_ou(prici=prici, priceq=priceq,
  theta=thetav, innov=matrix(sigmav*innov))
all.equal(pricev, drop(pricecpp))
# Compare the speed of R code with Rcpp
library(microbenchmark)
summary(microbenchmark(
  Rcode={for (i in 2:nrows) {
    retp[i] <- thetav*(priceq - pricev[i-1]) + sigmav*innov[i]
    pricev[i] <- pricev[i-1] + retp[i]}},
  Rcpp=HighFreq::sim_ou(prici=prici, priceq=priceq,
    theta=thetav, innov=matrix(sigmav*innov)),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

plot(pricev, type="l", xlab="time", ylab="prices",
     main="Ornstein-Uhlenbeck Process")
legend("topright", title=paste(c(paste0("sigmav = ", sigmav),
     paste0("priceq = ", ),
     paste0("thetav = ", thetav)),
   collapse="\n"),
 legend="", cex=0.8, inset=0.1, bg="white", bty="n")
abline(h=, col='red', lwd=2)

retp <- rutils::diffit(pricev)
pricelag <- rutils::lagit(pricev)
formulav <- retp ~ pricelag
regmod <- lm(formulav)
summary(regmod)
# Plot regression
plot(formulav, main="OU Returns Versus Lagged Prices")
abline(regmod, lwd=2, col="red")

# Calculate volatility parameter
c(volatility=sigmav, estimate=sd(retp))
# Extract OU parameters from regression
coeff <- summary(regmod)$coefficients
# Calculate regression alpha and beta directly
betac <- cov(retp, pricelag)/var(pricelag)
alphac <- (mean(retp) - betac*mean(pricelag))
cbind(direct=c(alpha=alphac, beta=betac), lm=coeff[, 1])
all.equal(c(alpha=alphac, beta=betac), coeff[, 1],
    check.attributes=FALSE)
# Calculate regression standard errors directly
betac <- c(alpha=alphac, beta=betac)
fitv <- (alphac + betac*pricelag)
resids <- (retp - fitv)
prices2 <- sum((pricelag - mean(pricelag))^2)
betasd <- sqrt(sum(resids^2)/prices2/(nrows-2))
alphasd <- sqrt(sum(resids^2)/(nrows-2)*(1:nrows + mean(pricelag)^2/prices2))
cbind(direct=c(alphasd=alphasd, betasd=betasd), lm=coeff[, 2])
all.equal(c(alphasd=alphasd, betasd=betasd), coeff[, 2],
    check.attributes=FALSE)
# Compare mean reversion parameter theta
c(theta=(-thetav), round(coeff[2, ], 3))
# Compare equilibrium price mu
c(priceq=priceq, estimate=-coeff[1, 1]/coeff[2, 1])
# Compare actual and estimated parameters
coeff <- cbind(c(thetav*priceq, -thetav), coeff[, 1:2])
rownames(coeff) <- c("drift", "theta")
colnames(coeff)[1] <- "actual"
round(coeff, 4)

# Simulate Schwartz process
retp <- numeric(nrows)
pricev <- numeric(nrows)
pricev[1] <- exp(sigmav*innov[1])
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")  # Reset random numbers
for (i in 2:nrows) {
  retp[i] <- thetav*(priceq - pricev[i-1]) + sigmav*innov[i]
  pricev[i] <- pricev[i-1]*exp(retp[i])
}  # end for

plot(pricev, type="l", xlab="time", ylab="prices",
     main="Schwartz Process")
legend("topright",
 title=paste(c(paste0("sigmav = ", sigmav),
     paste0("priceq = ", priceq),
     paste0("thetav = ", thetav)),
   collapse="\n"),
 legend="", cex=0.8, inset=0.12, bg="white", bty="n")
abline(h=priceq, col='red', lwd=2)

# Define Dickey-Fuller parameters
prici <- 0.0;  priceq <- 1.0
thetav <- 0.01;  nrows <- 1000
coeff <- c(0.1, 0.39, 0.5)
# Initialize the data
innov <- rnorm(nrows, sd=0.01)
retp <- numeric(nrows)
pricev <- numeric(nrows)
# Simulate Dickey-Fuller process using recursive loop in R
retp[1] <- innov[1]
pricev[1] <- prici
retp[2] <- thetav*(priceq - pricev[1]) + coeff[1]*retp[1] +
  innov[2]
pricev[2] <- pricev[1] + retp[2]
retp[3] <- thetav*(priceq - pricev[2]) + coeff[1]*retp[2] +
  coeff[2]*retp[1] + innov[3]
pricev[3] <- pricev[2] + retp[3]
for (it in 4:nrows) {
  retp[it] <- thetav*(priceq - pricev[it-1]) +
    retp[(it-1):(it-3)] %*% coeff + innov[it]
  pricev[it] <- pricev[it-1] + retp[it]
}  # end for
# Simulate Dickey-Fuller process in Rcpp
pricecpp <- HighFreq::sim_df(prici=prici, priceq=priceq,
   theta=thetav, coeff=matrix(coeff), innov=matrix(innov))
# Compare prices
all.equal(pricev, drop(pricecpp))
# Compare the speed of R code with Rcpp
library(microbenchmark)
summary(microbenchmark(
  Rcode={for (it in 4:nrows) {
  retp[it] <- thetav*(priceq - pricev[it-1]) + retp[(it-1):(it-3)] %*% coeff + innov[it]
  pricev[it] <- pricev[it-1] + retp[it]
  }},
  Rcpp=HighFreq::sim_df(prici=prici, priceq=priceq, theta=thetav, coeff=matrix(coeff), innov=matrix(innov)),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# Simulate AR(1) process with coefficient=1, with unit root
innov <- matrix(rnorm(1e4, sd=0.01))
arimav <- HighFreq::sim_ar(coeff=matrix(1), innov=innov)
plot(arimav, t="l", main="Brownian Motion")
# Perform ADF test with lag = 1
tseries::adf.test(arimav, k=1)
# Perform standard Dickey-Fuller test
tseries::adf.test(arimav, k=0)
# Simulate AR(1) with coefficient close to 1, without unit root
arimav <- HighFreq::sim_ar(coeff=matrix(0.99), innov=innov)
plot(arimav, t="l", main="AR(1) coefficient = 0.99")
tseries::adf.test(arimav, k=1)
# Simulate Ornstein-Uhlenbeck OU process with mean reversion
prici <- 0.0; priceq <- 0.0; thetav <- 0.1
pricev <- HighFreq::sim_ou(prici=prici, priceq=priceq,
  theta=thetav, innov=innov)
plot(pricev, t="l", main=paste("OU coefficient =", thetav))
tseries::adf.test(pricev, k=1)
# Simulate Ornstein-Uhlenbeck OU process with zero reversion
thetav <- 0.0
pricev <- HighFreq::sim_ou(prici=prici, priceq=priceq,
  theta=thetav, innov=innov)
plot(pricev, t="l", main=paste("OU coefficient =", thetav))
tseries::adf.test(pricev, k=1)

# Simulate AR(1) process with different coefficients
coeffv <- seq(0.99, 0.999, 0.001)
retp <- as.numeric(na.omit(rutils::etfenv$returns$VTI))
adft <- sapply(coeffv, function(coeff) {
  arimav <- filter(x=retp, filter=coeff, method="recursive")
  adft <- suppressWarnings(tseries::adf.test(arimav))
  c(adfstat=unname(adft$statistic), pval=adft$p.value)
})  # end sapply
dev.new(width=6, height=4, noRStudioGD=TRUE)
# x11(width=6, height=4)
plot(x=coeffv, y=adft["pval", ], main="ADF p-val Versus AR Coefficient",
     xlab="AR coefficient", ylab="ADF pval", t="l", col="blue", lwd=2)
plot(x=coeffv, y=adft["adfstat", ], main="ADF Stat Versus AR Coefficient",
     xlab="AR coefficient", ylab="ADF stat", t="l", col="blue", lwd=2)

# Extract log VTI prices
closep <- log(na.omit(rutils::etfenv$prices$VTI))
nrows <- NROW(closep)
# Calculate EMA prices using HighFreq::run_mean()
pricema <- HighFreq::run_mean(closep, lambda=0.9)
# Combine prices with EMA prices
pricev <- cbind(closep, pricema)
colnames(pricev)[2] <- "VTI EMA"
# Calculate standard deviations of returns
sapply(rutils::diffit(pricev), sd)

# Plot dygraph
dygraphs::dygraph(pricev["2009"], main="VTI Prices and EMA Prices") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Calculate VTI log returns
retp <- rutils::diffit(closef)
# Open plot window
x11(width=6, height=7)
# Set plot parameters
par(oma=c(1, 1, 0, 1), mar=c(1, 1, 1, 1), mgp=c(0, 0.5, 0),
    cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# Set two plot panels
par(mfrow=c(2,1))
# Plot ACF of VTI returns
rutils::plot_acf(retp[, 1], lag=10, xlab="")
title(main="ACF of VTI Returns", line=-1)
# Plot ACF of smoothed VTI returns
rutils::plot_acf(retp[, 2], lag=10, xlab="")
title(main="ACF of Smoothed VTI Returns", line=-1)

# Extract log VTI prices
ohlc <- rutils::etfenv$VTI
datev <- zoo::index(ohlc)
closep <- log(quantmod::Cl(ohlc))
colnames(closep) <- "VTI"
nrows <- NROW(closep)
# Calculate EMA weights
lookb <- 111
lambdaf <- 0.9
weightv <- lambdaf^(1:lookb)
weightv <- weightv/sum(weightv)
# Calculate EMA prices as the convolution
pricema <- HighFreq::roll_sumw(closep, weightv=weightv)
pricev <- cbind(closep, pricema)
colnames(pricev) <- c("VTI", "VTI EMA")

# Dygraphs plot with custom line colors
colv <- colnames(pricev)
dygraphs::dygraph(pricev["2008/2009"], main="VTI EMA Prices") %>%
  dySeries(name=colv[1], strokeWidth=1, col="blue") %>%
  dySeries(name=colv[2], strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)
# Standard plot of  EMA prices with custom line colors
x11(width=6, height=5)
plot_theme <- chart_theme()
colorv <- c("blue", "red")
plot_theme$col$line.col <- colors
quantmod::chart_Series(pricev["2008/2009"], theme=plot_theme,
       lwd=2, name="VTI EMA Prices")
legend("topleft", legend=colnames(pricev), y.intersp=0.4,
 inset=0.1, bg="white", lty=1, lwd=6, cex=0.8,
 col=plot_theme$col$line.col, bty="n")

# Calculate EMA prices recursively using C++ code
emar <- .Call(stats:::C_rfilter, closep, lambdaf, c(as.numeric(closep[1])/(1-lambdaf), double(NROW(closep))))[-1]
# Or R code
# emar <- filter(closep, filter=lambdaf, init=as.numeric(closep[1, 1])/(1-lambdaf), method="recursive")
emar <- (1-lambdaf)*emar
# Calculate EMA prices recursively using RcppArmadillo C++
pricema <- HighFreq::run_mean(closep, lambda=lambdaf)
all.equal(drop(pricema), emar)
# Compare the speed of C++ code with RcppArmadillo C++
library(microbenchmark)
summary(microbenchmark(
  filtercpp=HighFreq::run_mean(closep, lambda=lambdaf),
  rfilter=.Call(stats:::C_rfilter, closep, lambdaf, c(as.numeric(closep[1])/(1-lambdaf), double(NROW(closep)))),
  times=10))[, c(1, 4, 5)]

# Dygraphs plot with custom line colors
pricev <- cbind(closep, pricema)
colnames(pricev) <- c("VTI", "VTI EMA")
colv <- colnames(pricev)
dygraphs::dygraph(pricev["2008/2009"], main="Recursive VTI EMA Prices") %>%
  dySeries(name=colv[1], strokeWidth=1, col="blue") %>%
  dySeries(name=colv[2], strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)
# Standard plot of  EMA prices with custom line colors
x11(width=6, height=5)
plot_theme <- chart_theme()
colorv <- c("blue", "red")
plot_theme$col$line.col <- colors
quantmod::chart_Series(pricev["2008/2009"], theme=plot_theme,
       lwd=2, name="VTI EMA Prices")
legend("topleft", legend=colnames(pricev),
 inset=0.1, bg="white", lty=1, lwd=6, cex=0.8,
 col=plot_theme$col$line.col, bty="n")

# Calculate log OHLC prices and volumes
volumv <- quantmod::Vo(ohlc)
colnames(volumv) <- "Volume"
nrows <- NROW(closep)
# Calculate the VWAP prices
lookb <- 21
vwap <- HighFreq::roll_sum(closep, lookb=lookb, weightv=volumv)
colnames(vwap) <- "VWAP"
pricev <- cbind(closep, vwap)

# Dygraphs plot with custom line colors
colorv <- c("blue", "red")
dygraphs::dygraph(pricev["2008/2009"], main="VTI VWAP Prices") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Plot VWAP prices with custom line colors
x11(width=6, height=5)
plot_theme <- chart_theme()
plot_theme$col$line.col <- colors
quantmod::chart_Series(pricev["2008/2009"], theme=plot_theme,
       lwd=2, name="VTI VWAP Prices")
legend("bottomright", legend=colnames(pricev),
 inset=0.1, bg="white", lty=1, lwd=6, cex=0.8,
 col=plot_theme$col$line.col, bty="n")

# Calculate VWAP prices recursively using C++ code
lambdaf <- 0.9
volumer <- .Call(stats:::C_rfilter, volumv, lambdaf, c(as.numeric(volumv[1])/(1-lambdaf), double(NROW(volumv))))[-1]
pricer <- .Call(stats:::C_rfilter, volumv*closep, lambdaf, c(as.numeric(volumv[1]*closep[1])/(1-lambdaf), double(NROW(closep))))[-1]
vwapr <- pricer/volumer
# Calculate VWAP prices recursively using RcppArmadillo C++
vwapc <- HighFreq::run_mean(closep, lambda=lambdaf, weightv=volumv)
all.equal(vwapr, drop(vwapc))
# Dygraphs plot the VWAP prices
pricev <- xts(cbind(vwap, vwapr), zoo::index(ohlc))
colnames(pricev) <- c("VWAP trailing", "VWAP recursive")
dygraphs::dygraph(pricev["2008/2009"], main="VWAP Prices") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Calculate two EMA prices
lambdaf <- 0.8 # Fast EMA
emaf <- HighFreq::run_mean(closep, lambda=lambdaf)
lambdas <- 0.9 # Slow EMAs
emas <- HighFreq::run_mean(closep, lambda=lambdas)

# Calculate VTI prices
emad <- (emaf - emas)
pricev <- cbind(closep, emad)
symboln <- "VTI"
colnames(pricev) <- c(symboln, paste(symboln, "Returns"))
# Plot dygraph of VTI Returns
colv <- colnames(pricev)
dygraphs::dygraph(pricev["2008/2009"], main=paste(symboln, "EMA Returns")) %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=colv[2], axis="y2", strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)

# Calculate the fractional weights
lookb <- 21
deltav <- 0.1
weightv <- (deltav - 0:(lookb-2)) / 1:(lookb-1)
weightv <- (-1)^(1:(lookb-1))*cumprod(weightv)
weightv <- c(1, weightv)
weightv <- (weightv - mean(weightv))

# Calculate the fractional VTI returns
retf <- HighFreq::roll_conv(closep, weightv=weightv)
pricev <- cbind(closep, retf)
symboln <- "VTI"
colnames(pricev) <- c(symboln, paste(symboln, "Returns"))
# Plot dygraph of VTI Returns
colv <- colnames(pricev)
dygraphs::dygraph(pricev["2008-08/2009-08"], main=paste(symboln, "Fractional Returns")) %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=colv[2], axis="y2", strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)

# Perform ADF test for prices
tseries::adf.test(closep)
# Perform ADF test for returns
tseries::adf.test(retp)

# Calculate fractional VTI returns
deltav <- 0.1*c(1, 3, 5, 7, 9)
retfrac <- lapply(deltav, function(deltav) {
  weightv <- (deltav - 0:(lookb-2)) / 1:(lookb-1)
  weightv <- c(1, (-1)^(1:(lookb-1))*cumprod(weightv))
  weightv <- (weightv - mean(weightv))
  HighFreq::roll_conv(closep, weightv=weightv)
})  # end lapply
retfrac <- do.call(cbind, retfrac)
retfrac <- cbind(closep, retfrac)
colnames(retfrac) <- c("VTI", paste0("frac_", deltav))
# Calculate ADF test statistics
adfstats <- sapply(retfrac, function(x)
  suppressWarnings(tseries::adf.test(x)$statistic)
)  # end sapply
names(adfstats) <- colnames(retfrac)

# Plot dygraph of fractional VTI returns
colorv <- colorRampPalette(c("blue", "red"))(NCOL(retfrac))
colv <- colnames(retfrac)
dyplot <- dygraphs::dygraph(retfrac["2008-08/2009-08"], main="Fractional Returns") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", strokeWidth=2, col=colorv[1])
for (i in 2:NROW(colv))
  dyplot <- dyplot %>%
  dyAxis("y2", label=colv[i], independentTicks=TRUE) %>%
  dySeries(name=colv[i], axis="y2", strokeWidth=2, col=colorv[i])
dyplot <- dyplot %>% dyLegend(width=300)
dyplot

# Calculate volume z-scores
volumv <- quantmod::Vo(ohlc)
lookb <- 21
volumean <- HighFreq::roll_mean(volumv, lookb=lookb)
volumsd <- sqrt(HighFreq::roll_var(rutils::diffit(volumv), lookb=lookb))
volumsd[1] <- 0
volumz <- ifelse(volumsd > 0, (volumv - volumean)/volumsd, 0)
# Plot histogram of volume z-scores
hist(volumz, breaks=1e2)

# Plot dygraph of volume z-scores of VTI prices
pricev <- cbind(closep, volumz)
colnames(pricev) <- c("VTI", "Z-Scores")
colv <- colnames(pricev)
dygraphs::dygraph(pricev["2008/2009"], main="VTI Volume Z-Scores") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=colv[2], axis="y2", strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)

# Calculate volatility (true range) z-scores
volv <- log(quantmod::Hi(ohlc) - quantmod::Lo(ohlc))
lookb <- 21
volatm <- HighFreq::roll_mean(volv, lookb=lookb)
volv <- (volv - volatm)
volatsd <- sqrt(HighFreq::roll_var(rutils::diffit(volv), lookb=lookb))
volatsd[1] <- 0
volatz <- ifelse(volatsd > 0, volv/volatsd, 0)
# Plot histogram of the volatility z-scores
hist(volatz, breaks=1e2)

# Plot scatterplot of volume and volatility z-scores
plot(as.numeric(volatz), as.numeric(volumz),
     xlab="volatility z-score", ylab="volume z-score")
regmod <- lm(volatz ~ volumz)
abline(regmod, col="red", lwd=3)
# Plot dygraph of VTI volatility z-scores
pricev <- cbind(closep, volatz)
colnames(pricev) <- c("VTI", "Z-Scores")
colv <- colnames(pricev)
dygraphs::dygraph(pricev["2008/2009"], main="VTI Volatility Z-Scores") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=colv[2], axis="y2", strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)

# Calculate the recursive trailing VTI volatility
lambdaf <- 0.8 # Fast lambda
lambdas <- 0.81 # Slow lambda
volatf <- sqrt(HighFreq::run_var(retp, lambda=lambdaf))[, 2]
volats <- sqrt(HighFreq::run_var(retp, lambda=lambdas))[, 2]
# Calculate the recursive trailing z-scores of VTI volatility
volatd <- volatf - volats
volatsd <- sqrt(HighFreq::run_var(rutils::diffit(volatd), lambda=lambdaf)[, 2])
volatsd[1] <- 0
volatz <- ifelse(volatsd > 0, volatd/volatsd, 0)

# Plot histogram of the volatility z-scores
hist(volatz, breaks=1e2)
# Plot scatterplot of volume and volatility z-scores
plot(as.numeric(volatz), as.numeric(volumz),
     xlab="volatility z-score", ylab="volume z-score")
regmod <- lm(volatz ~ volumz)
abline(regmod, col="red", lwd=3)
# Plot dygraph of VTI volatility z-scores
pricev <- cbind(closep, volatz)
colnames(pricev) <- c("VTI", "Z-Scores")
colv <- colnames(pricev)
dygraphs::dygraph(pricev["2008/2009"], main="VTI Online Volatility Z-Scores") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=colv[2], axis="y2", strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)

# Calculate the centered volatility
lookb <- 21
halfb <- lookb %/% 2
volv <- HighFreq::roll_var(closep, lookb=lookb)
volv <- sqrt(volv)
volv <- rutils::lagit(volv, lagg=(-halfb))
# Calculate the z-scores of prices
pricez <- (closep -
  0.5*(rutils::lagit(closep, halfb, pad_zeros=FALSE) +
  rutils::lagit(closep, -halfb, pad_zeros=FALSE)))
pricez <- ifelse(volv > 0, pricez/volv, 0)

# Plot dygraph of z-scores of VTI prices
pricev <- cbind(closep, pricez)
colnames(pricev) <- c("VTI", "Z-Scores")
colv <- colnames(pricev)
dygraphs::dygraph(pricev["2009"], main="VTI Centered Price Z-Scores") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=colv[2], axis="y2", strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)

# Calculate the thresholds for labeling tops and bottoms
confl <- c(0.2, 0.8)
threshv <- quantile(pricez, confl)
# Calculate the vectors of tops and bottoms
topl <- zoo::coredata(pricez > threshv[2])
bottoml <- zoo::coredata(pricez < threshv[1])
# Simulate in-sample VTI strategy
posv <- rep(NA_integer_, nrows)
posv[1] <- 0
posv[topl] <- (-1)
posv[bottoml] <- 1
posv <- zoo::na.locf(posv)
posv <- rutils::lagit(posv)
pnls <- retp*posv

# Plot dygraph of in-sample VTI strategy
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Price Tops and Bottoms Strategy In-sample") %>%
  dyAxis("y", label="VTI", independentTicks=TRUE) %>%
  dyAxis("y2", label="Strategy", independentTicks=TRUE) %>%
  dySeries(name="VTI", axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name="Strategy", axis="y2", strokeWidth=2, col="red")

# Calculate the trailing VTI volatility
volv <- HighFreq::roll_var(closep, lookb=lookb)
volv <- sqrt(volv)
# Calculate the trailing z-scores of VTI prices
pricez <- (closep - rutils::lagit(closep, lookb, pad_zeros=FALSE))
pricez <- ifelse(volv > 0, pricez/volv, 0)

# Plot dygraph of the trailing z-scores of VTI prices
pricev <- cbind(closep, pricez)
colnames(pricev) <- c("VTI", "Z-Scores")
colv <- colnames(pricev)
dygraphs::dygraph(pricev["2009"],
  main="VTI Trailing Price Z-Scores") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(axis="y", label=colv[1], strokeWidth=2, col="blue") %>%
  dySeries(axis="y2", label=colv[2], strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)

# Calculate the EMA returns and volatilities
lambdaf <- 0.9
volv <- HighFreq::run_var(closep, lambda=lambdaf)
# Calculate the recursive trailing z-scores of VTI prices
pricer <- (closep - volv[, 1])
pricer <- ifelse(volv > 0, pricer/volv, 0)
volv <- sqrt(volv[, 2])
# Plot dygraph of the trailing z-scores of VTI prices
pricev <- xts::xts(cbind(pricez, pricer), datev)
colnames(pricev) <- c("Z-Scores", "Recursive")
colv <- colnames(pricev)
dygraphs::dygraph(pricev["2009"], main="VTI Online Trailing Price Z-Scores") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Calculate trailing price regression z-scores
datev <- matrix(zoo::index(closep))
lookb <- 21
# Create a default list of regression parameters
controlv <- HighFreq::param_reg()
regs <- HighFreq::roll_reg(respv=closep, predm=datev,
   lookb=lookb, controlv=controlv)
regs[1:lookb, ] <- 0
# Plot dygraph of z-scores of VTI prices
datav <- cbind(closep, regs[, NCOL(regs)])
colnames(datav) <- c("VTI", "Z-Scores")
colv <- colnames(datav)
dygraphs::dygraph(datav["2009"], main="VTI Regression Z-Scores") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=colv[2], axis="y2", strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)

# Calculate recursive trailing price regression versus time
lambdaf <- 0.9
# Create a list of regression parameters
controlv <- HighFreq::param_reg(residscale="standardize")
regs <- HighFreq::run_reg(closep, matrix(datev), lambda=lambdaf, controlv=controlv)
colnames(regs) <- c("alpha", "beta", "zscores")
tail(regs)

# Plot dygraph of regression betas
datav <- cbind(closep, 252*regs[, "beta"])
colnames(datav) <- c("VTI", "Slope")
colv <- colnames(datav)
dygraphs::dygraph(datav["2009"], main="VTI Online Regression Slope") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(axis="y", label=colv[1], strokeWidth=2, col="blue") %>%
  dySeries(axis="y2", label=colv[2], strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)

# Plot dygraph of z-scores of VTI prices
datav <- cbind(closep, regs[, "zscores"])
colnames(datav) <- c("VTI", "Z-Scores")
colv <- colnames(datav)
dygraphs::dygraph(datav["2009"], main="VTI Online Regression Z-Scores") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=colv[2], axis="y2", strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)

# Extract time series of VTI log prices
closep <- log(na.omit(rutils::etfenv$prices$VTI))
# Define look-back window
lookb <- 11
# Calculate time series of trailing medians
medianv <- HighFreq::roll_mean(closep, lookb=lookb, method="nonparametric")
# Calculate time series of MAD
madv <- HighFreq::roll_var(closep, lookb=lookb, method="nonparametric")
# madv <- TTR::runMAD(closep, n=lookb)
# Calculate time series of z-scores
zscores <- (closep - medianv)/madv
zscores[1:lookb, ] <- 0
tail(zscores, lookb)
range(zscores)

x11(width=6, height=5)
# Plot the prices and medians
dygraphs::dygraph(cbind(closep, medianv), main="VTI median") %>%
  dyOptions(colors=c("black", "red")) %>%
  dyLegend(show="always", width=300)
# Plot histogram of z-scores
histp <- hist(zscores, col="lightgrey",
  xlab="z-scores", breaks=50, xlim=c(-4, 4),
  ylab="frequency", freq=FALSE, main="Hampel Z-Scores histogram")
lines(density(zscores, adjust=1.5), lwd=3, col="blue")

# Calculate one-sided Hampel z-scores
medianv <- HighFreq::roll_mean(closep, lookb=lookb, method="nonparametric")
madv <- HighFreq::roll_var(closep, lookb=lookb, method="nonparametric")
zscores <- (closep - medianv)/madv
zscores[1:lookb, ] <- 0
tail(zscores, lookb)
range(zscores)
# Calculate two-sided Hampel z-scores
halfb <- lookb %/% 2
medianv <- rutils::lagit(medianv, lagg=(-halfb))
madv <- rutils::lagit(madv, lagg=(-halfb))
zscores <- (closep - medianv)/madv
zscores[1:lookb, ] <- 0
tail(zscores, lookb)
range(zscores)

# Calculate VTI percentage returns
retp <- na.omit(rutils::etfenv$returns$VTI)
nrows <- NROW(retp)
# Define end points
endd <- 1:NROW(retp)
# Start points are multi-period lag of endd
lookb <- 11
startp <- c(rep_len(0, lookb-1), endd[1:(nrows-lookb+1)])
# Calculate trailing variance in sapply() loop - takes long
varv <- sapply(1:nrows, function(it) {
  retp <- retp[startp[it]:endd[it]]
  sum((retp - mean(retp))^2)/lookb
})  # end sapply
# Use only vectorized functions
retc <- cumsum(retp)
retc <- (retc - c(rep_len(0, lookb), retc[1:(nrows-lookb)]))
retc2 <- cumsum(retp^2)
retc2 <- (retc2 - c(rep_len(0, lookb), retc2[1:(nrows-lookb)]))
var2 <- (retc2 - retc^2/lookb)/lookb
all.equal(varv[-(1:lookb)], as.numeric(var2)[-(1:lookb)])
# Or using package rutils
retc <- rutils::roll_sum(retp, lookb=lookb)
retc2 <- rutils::roll_sum(retp^2, lookb=lookb)
var2 <- (retc2 - retc^2/lookb)/lookb
# Coerce variance into xts
tail(varv)
class(varv)
varv <- xts(varv, order.by=zoo::index(retp))
colnames(varv) <- "VTI.variance"
head(varv)

# Calculate trailing VTI variance using package HighFreq
varv <- roll::roll_var(retp, width=lookb)
colnames(varv) <- "Variance"
head(varv)
sum(is.na(varv))
varv[1:(lookb-1)] <- 0
# Benchmark calculation of trailing variance
library(microbenchmark)
summary(microbenchmark(
  sapply=sapply(1:nrows, function(it) {
    var(retp[startp[it]:endd[it]])
  }),
  roll=roll::roll_var(retp, width=lookb),
  times=10))[, c(1, 4, 5)]

# Calculate EMA VTI variance using compiled C++ function
lookb <- 51
weightv <- exp(-0.1*1:lookb)
weightv <- weightv/sum(weightv)
varv <- .Call(stats:::C_cfilter, retp^2, filter=weightv, sides=1, circular=FALSE)
varv[1:(lookb-1)] <- varv[lookb]
# Plot EMA volatility
varv <- xts:::xts(sqrt(varv), order.by=zoo::index(retp))
dygraphs::dygraph(varv, main="VTI EMA Volatility") %>%
  dyOptions(colors="blue") %>% dyLegend(show="always", width=300)
quantmod::chart_Series(xtsv, name="VTI EMA Volatility")

# Calculate trailing VTI variance using package roll
library(roll)  # Load roll
varv <- roll::roll_var(retp, weights=rev(weightv), width=lookb)
colnames(varv) <- "VTI.variance"
class(varv)
head(varv)
sum(is.na(varv))
varv[1:(lookb-1)] <- 0

# Calculate realized variance recursively
lambdaf <- 0.9
volv <- HighFreq::run_var(retp, lambda=lambdaf)
volv <- sqrt(volv[, 2])
# Plot EMA volatility
volv <- xts:::xts(volv, order.by=datev)
dygraphs::dygraph(volv, main="VTI Realized Volatility") %>%
  dyOptions(colors="blue") %>% dyLegend(show="always", width=300)

library(HighFreq)  # Load HighFreq
# Minutely SPY returns (unit per minute) single day
# Minutely SPY volatility (unit per minute)
retspy <- rutils::diffit(log(SPY["2012-02-13", 4]))
sd(retspy)
# SPY returns multiple days (includes overnight jumps)
retspy <- rutils::diffit(log(SPY[, 4]))
sd(retspy)
# Table of time intervals - 60 second is most frequent
indeks <- rutils::diffit(xts::.index(SPY))
table(indeks)
# SPY returns divided by the overnight time intervals (unit per second)
retspy <- retspy/indeks
retspy[1] <- 0
# Minutely SPY volatility scaled to unit per minute
60*sd(retspy)

library(HighFreq)  # Load HighFreq
spy <- HighFreq::SPY["2008/2009"]
# Calculate daily SPY volatility using package HighFreq
sqrt(6.5*60*HighFreq::calcvar_ohlc(log(spy),
  method="yang_zhang"))
# Calculate daily SPY volatility from minutely prices using package TTR
sqrt((6.5*60)*mean(na.omit(
  TTR::volatility(spy, N=1, calc="yang.zhang"))^2))
# Calculate trailing SPY variance using package HighFreq
varv <- HighFreq::roll_var_ohlc(log(spy), method="yang_zhang",
  lookb=lookb)
# Plot range volatility
varv <- xts:::xts(sqrt(varv), order.by=zoo::index(spy))
dygraphs::dygraph(varv["2009-02"], main="SPY Trailing Range Volatility") %>%
  dyOptions(colors="blue") %>% dyLegend(show="always", width=300)
# Benchmark the speed of HighFreq vs TTR
library(microbenchmark)
summary(microbenchmark(
  ttr=TTR::volatility(rutils::etfenv$VTI, N=1, calc="yang.zhang"),
  highfreq=HighFreq::calcvar_ohlc(log(rutils::etfenv$VTI), method="yang_zhang"),
  times=2))[, c(1, 4, 5)]

# Calculate VXX log prices
vxx <- na.omit(rutils::etfenv$prices$VXX)
datev <- zoo::index(vxx)
lookb <- 41
vxx <- log(vxx)
# Calculate trailing VTI volatility
closep <- get("VTI", rutils::etfenv)[datev]
closep <- log(closep)
volv <- sqrt(HighFreq::roll_var_ohlc(ohlc=closep, lookb=lookb, scalev=FALSE))
volv[1:lookb] <- volv[lookb+1]

# Plot dygraph of VXX and VTI volatility
datav <- cbind(vxx, volv)
colnames(datav)[2] <- "VTI Volatility"
colv <- colnames(datav)
captiont <- "VXX and VTI Volatility"
dygraphs::dygraph(datav[, 1:2], main=captiont) %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", strokeWidth=1, col="blue") %>%
  dySeries(name=colv[2], axis="y2", strokeWidth=1, col="red") %>%
  dyLegend(show="always", width=300)

x11(width=6, height=5)
par(mar=c(4, 3, 1, 1), oma=c(0, 0, 0, 0))
# Calculate VTI percentage returns
retp <- na.omit(rutils::etfenv$returns$VTI)
# Calculate trailing VTI variance using package roll
lookb <- 21
varv <- HighFreq::roll_var(retp, lookb=lookb)
colnames(varv) <- "Variance"
# Number of lookbv that fit over returns
nrows <- NROW(retp)
nagg <- nrows %/% lookb
# Define end points with beginning stub
endd <- c(0, nrows-lookb*nagg + (0:nagg)*lookb)
nrows <- NROW(endd)
# Subset variance to end points
varv <- varv[endd]
# Plot autocorrelation function
rutils::plot_acf(varv, lag=10, main="ACF of Variance")
# Plot partial autocorrelation
pacf(varv, lag=10, main="PACF of Variance", ylab=NA)

# Define GARCH parameters
alphac <- 0.3; betac <- 0.5;
omega <- 1e-4*(1 - alphac - betac)
nrows <- 1000
# Calculate matrix of standard normal innovations
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")  # Reset random numbers
innov <- rnorm(nrows)
retp <- numeric(nrows)
varv <- numeric(nrows)
varv[1] <- omega/(1 - alphac - betac)
retp[1] <- sqrt(varv[1])*innov[1]
# Simulate GARCH model
for (i in 2:nrows) {
  retp[i] <- sqrt(varv[i-1])*innov[i]
  varv[i] <- omega + alphac*retp[i]^2 + betac*varv[i-1]
}  # end for
# Simulate the GARCH process using Rcpp
garchsim <- HighFreq::sim_garch(omega=omega, alpha=alphac,
  beta=betac, innov=matrix(innov))
all.equal(garchsim, cbind(retp, varv), check.attributes=FALSE)

# Open plot window on Mac
dev.new(width=6, height=5, noRStudioGD=TRUE)
# Set plot parameters to reduce whitespace around plot
par(mar=c(2, 2, 3, 1), oma=c(0, 0, 0, 0))
# Plot GARCH cumulative returns
plot(cumsum(retp), t="l", col="blue", xlab="", ylab="",
  main="GARCH Cumulative Returns")
quartz.save("figure/garch_returns.png", type="png",
  width=6, height=5)
# Plot GARCH volatility
plot(sqrt(varv), t="l", col="blue", xlab="", ylab="",
  main="GARCH Volatility")
quartz.save("figure/garch_volat.png", type="png",
  width=6, height=5)

# Calculate kurtosis of GARCH returns
mean(((retp-mean(retp))/sd(retp))^4)
# Perform Jarque-Bera test of normality
tseries::jarque.bera.test(retp)
# Fit t-distribution into GARCH returns
fitobj <- MASS::fitdistr(retp, densfun="t", df=2)
locv <- fitobj$estimate[1]
scalev <- fitobj$estimate[2]

# Plot histogram of GARCH returns
histp <- hist(retp, col="lightgrey",
  xlab="returns", breaks=200, xlim=c(-0.03, 0.03),
  ylab="frequency", freq=FALSE, main="GARCH Returns Histogram")
lines(density(retp, adjust=1.5), lwd=2, col="blue")
curve(expr=dt((x-locv)/scalev, df=2)/scalev,
  type="l", xlab="", ylab="", lwd=2,
  col="red", add=TRUE)
legend("topright", inset=-0, bty="n", y.intersp=0.4,
 leg=c("density", "t-distr w/ 2 dof"),
 lwd=6, lty=1, col=c("blue", "red"))
quartz.save("figure/garch_hist.png", type="png", width=6, height=5)

# Specify GARCH model
garch_spec <- fGarch::garchSpec(model=list(ar=c(0, 0), omega=omega,
  alpha=alphac, beta=betac))
# Simulate GARCH model
garch_sim <- fGarch::garchSim(spec=garch_spec, n=nrows)
retp <- as.numeric(garch_sim)
# Calculate kurtosis of GARCH returns
moments::moment(retp, order=4) /
  moments::moment(retp, order=2)^2
# Perform Jarque-Bera test of normality
tseries::jarque.bera.test(retp)
# Plot histogram of GARCH returns
histp <- hist(retp, col="lightgrey",
  xlab="returns", breaks=200, xlim=c(-0.05, 0.05),
  ylab="frequency", freq=FALSE,
  main="GARCH Returns Histogram")
lines(density(retp, adjust=1.5), lwd=3, col="blue")

# Fit t-distribution into GARCH returns
fitobj <- MASS::fitdistr(retp, densfun="t", df=2, lower=c(-1, 1e-7))
locv <- fitobj$estimate[1]
scalev <- fitobj$estimate[2]
curve(expr=dt((x-locv)/scalev, df=2)/scalev,
  type="l", xlab="", ylab="", lwd=3,
  col="red", add=TRUE)
legend("topright", inset=0.05, bty="n", y.intersp=0.4,
 leg=c("density", "t-distr w/ 2 dof"),
 lwd=6, lty=1, col=c("blue", "red"))

# Calculate variance of GARCH returns
var(retp)
# Calculate expected value of variance
omega/(1 - alphac - betac)
# Calculate kurtosis of GARCH returns
mean(((retp-mean(retp))/sd(retp))^4)
# Calculate expected value of kurtosis
3 + 6*alpha^2/(1-2*alpha^2-(alphac+betac)^2)

# Calculate the distribution of GARCH kurtosis
kurt <- sapply(1:1e4, function(x) {
  garchsim <- HighFreq::sim_garch(omega=omega, alpha=alphac,
    beta=betac, innov=matrix(rnorm(nrows)))
  retp <- garchsim[, 1]
  c(var(retp), mean(((retp-mean(retp))/sd(retp))^4))
})  # end sapply
kurt <- t(kurt)
apply(kurt, 2, mean)
# Plot the distribution of GARCH kurtosis
dev.new(width=6, height=5, noRStudioGD=TRUE)
par(mar=c(2, 2, 3, 1), oma=c(0, 0, 0, 0))
histp <- hist(kurt[, 2], breaks=500, col="lightgrey",
  xlim=c(2, 8), xlab="returns", ylab="frequency", freq=FALSE,
  main="Distribution of GARCH Kurtosis")
lines(density(kurt[, 2], adjust=1.5), lwd=3, col="blue")
abline(v=(3 + 6*alpha^2/(1-2*alpha^2-(alphac+betac)^2)), lwd=3, col="red")
text(x=7.0, y=0.4, "Expected Kurtosis")
quartz.save("figure/garch_kurtosis.png", type="png", width=6, height=5)

# Simulate the GARCH process using Rcpp
garchsim <- HighFreq::sim_garch(omega=omega, alpha=alphac,
  beta=betac, innov=matrix(innov))
# Extract the returns
retp <- garchsim[, 1]
# Estimate the trailing variance from the returns
varv <- numeric(nrows)
varv[1] <- omega/(1 - alphac - betac)
for (i in 2:nrows) {
  varv[i] <- omega + alphac*retp[i]^2 +
    betac*varv[i-1]
}  # end for
all.equal(garchsim[, 2], varv, check.attributes=FALSE)

library(fGarch)
# Fit returns into GARCH
garchfit <- fGarch::garchFit(data=retp)
# Fitted GARCH parameters
garchfit@fit$coef
# Actual GARCH parameters
c(mu=mean(retp), omega=omega,alpha=alphac, beta=betac)
# Plot GARCH fitted volatility
plot(sqrt(garchfit@fit$series$h), t="l",
  col="blue", xlab="", ylab="",
  main="GARCH Fitted Volatility")
quartz.save("figure/garch_fGarch_fitted.png",
  type="png", width=6, height=5)

# Define likelihood function
likefun <- function(omega, alphac, betac) {
  # Estimate the trailing variance from the returns
  varv <- numeric(nrows)
  varv[1] <- omega/(1 - alphac - betac)
  for (i in 2:nrows) {
    varv[i] <- omega + alphac*retp[i]^2 + betac*varv[i-1]
  }  # end for
  varv <- ifelse(varv > 0, varv, 0.000001)
  # Lag the variance
  varv <- rutils::lagit(varv, pad_zeros=FALSE)
  # Calculate the likelihood
  -sum(retp^2/varv + log(varv))
}  # end likefun
# Calculate the likelihood in R
likefun(omega, alphac, betac)
# Calculate the likelihood in Rcpp
HighFreq::lik_garch(omega=omega, alpha=alphac,
  beta=betac, returns=matrix(retp))
# Benchmark speed of likelihood calculations
library(microbenchmark)
summary(microbenchmark(
  Rcode=likefun(omega, alphac, betac),
  Rcpp=HighFreq::lik_garch(omega=omega, alpha=alphac, beta=betac, returns=matrix(retp))
  ), times=10)[, c(1, 4, 5)]

# Calculate the variance of returns
retp <- garchsim[, 1, drop=FALSE]
varv <- var(retp)
retp <- (retp - mean(retp))
# Calculate likelihood as function of alpha and betac parameters
likefun <- function(alphac, betac) {
  omega <- variance*(1 - alpha - betac)
  -HighFreq::lik_garch(omega=omega, alpha=alphac, beta=betac, returns=retp)
}  # end likefun
# Calculate matrix of likelihood values
alphas <- seq(from=0.15, to=0.35, len=50)
betac <- seq(from=0.35, to=0.5, len=50)
likmat <- sapply(alphacs, function(alphac) sapply(betac,
  function(betac) likefun(alphac, betac)))

# Set rgl options and load package rgl
options(rgl.useNULL=TRUE); library(rgl)
# Draw and render 3d surface plot of likelihood function
ncols <- 100
color <- rainbow(ncols, start=2/6, end=4/6)
zcols <- cut(likmat, ncols)
rgl::persp3d(alphacs, betac, likmat, col=color[zcols],
  xlab="alpha", ylab="beta", zlab="likelihood")
rgl::rglwidget(elementId="plot3drgl", width=700, height=700)
# Perform grid search
coord <- which(likmat == min(likmat), arr.ind=TRUE)
c(alphacs[coord[2]], betac[coord[1]])
likmat[coord]
likefun(alphacs[coord[2]], betac[coord[1]])
# Optimal and actual parameters
options(scipen=2)  # Use fixed not scientific notation
cbind(actual=c(alphac=alphac, beta=betac, omega=omega),
  optimal=c(alphacs[coord[2]], betac[coord[1]], variance*(1 - sum(alphacs[coord[2]], betac[coord[1]]))))

# Define vectorized likelihood function
likefun <- function(x, retp) {
  alphac <- x[1]; betac <- x[2]; omega <- x[3]
  -HighFreq::lik_garch(omega=omega, alpha=alphac, beta=betac, returns=retp)
}  # end likefun
# Initial parameters
initp <- c(alphac=0.2, beta=0.4, omega=varv/0.2)
# Find max likelihood parameters using steepest descent optimizer
fitobj <- optim(par=initp,
  fn=likefun, # Log-likelihood function
  method="L-BFGS-B", # Quasi-Newton method
  returns=retp,
  upper=c(0.35, 0.55, varv), # Upper constraint
  lower=c(0.15, 0.35, varv/100)) # Lower constraint
# Optimal and actual parameters
cbind(actual=c(alphac=alphac, beta=betac, omega=omega),
optimal=c(fitobj$par["alpha"], fitobj$par["beta"], fitobj$par["omega"]))
# Find max likelihood parameters using DEoptim
optiml <- DEoptim::DEoptim(fn=likefun,
  upper=c(0.35, 0.55, varv), # Upper constraint
  lower=c(0.15, 0.35, varv/100), # Lower constraint
  returns=retp,
  control=list(trace=FALSE, itermax=1000, parallelType=1))
# Optimal and actual parameters
cbind(actual=c(alphac=alphac, beta=betac, omega=omega),
optimal=c(optiml$optim$bestmem[1], optiml$optim$bestmem[2], optiml$optim$bestmem[3]))

# Calculate VTI returns
retp <- na.omit(rutils::etfenv$returns$VTI)
# Find max likelihood parameters using DEoptim
optiml <- DEoptim::DEoptim(fn=likefun,
  upper=c(0.4, 0.9, varv), # Upper constraint
  lower=c(0.1, 0.5, varv/100), # Lower constraint
  returns=retp,
  control=list(trace=FALSE, itermax=1000, parallelType=1))
# Optimal parameters
par_am <- unname(optiml$optim$bestmem)
alphac <- par_am[1]; betac <- par_am[2]; omega <- par_am[3]
c(alphac, betac, omega)
# Equilibrium GARCH variance
omega/(1 - alphac - betac)
drop(var(retp))

# Estimate the GARCH volatility of VTI returns
nrows <- NROW(retp)
varv <- numeric(nrows)
varv[1] <- omega/(1 - alphac - betac)
for (i in 2:nrows) {
  varv[i] <- omega + alphac*retp[i]^2 + betac*varv[i-1]
}  # end for
# Estimate the GARCH volatility using Rcpp
garchsim <- HighFreq::sim_garch(omega=omega, alpha=alphac,
  beta=betac, innov=retp, is_random=FALSE)
all.equal(garchsim[, 2], varv, check.attributes=FALSE)
# Plot dygraph of the estimated GARCH volatility
dygraphs::dygraph(xts::xts(sqrt(varv), zoo::index(retp)),
  main="Estimated GARCH Volatility of VTI") %>%
  dyOptions(colors="blue") %>% dyLegend(show="always", width=300)

# Simulate GARCH model
garchsim <- HighFreq::sim_garch(omega=omega, alpha=alphac,
  beta=betac, innov=matrix(innov))
varv <- garchsim[, 2]
# Calculate the equilibrium variance
vareq <- omega/(1 - alphac - betac)
# Calculate the variance forecasts
varf <- numeric(10)
varf[1] <- vareq + (alphac + betac)*(xts::last(varv) - vareq)
for (i in 2:10) {
  varf[i] <- vareq + (alphac + betac)*(varf[i-1] - vareq)
}  # end for

# Open plot window on Mac
dev.new(width=6, height=5, noRStudioGD=TRUE)
par(mar=c(2, 2, 3, 1), oma=c(0, 0, 0, 0))
# Plot GARCH variance forecasts
plot(tail(varv, 30), t="l", col="blue", xlab="", ylab="",
  xlim=c(1, 40), ylim=c(0, max(tail(varv, 30))),
  main="GARCH Variance Forecasts")
text(x=15, y=0.5*vareq, "realized variance")
lines(x=30:40, y=c(xts::last(varv), varf), col="red", lwd=3)
text(x=35, y=0.6*vareq, "variance forecasts")
abline(h=vareq, lwd=3, col="red")
text(x=10, y=1.1*vareq, "Equilibrium variance")
quartz.save("figure/garch_forecast.png", type="png",
  width=6, height=5)

# Verify that Rtools or XCode are working properly:
devtools::find_rtools()  # Under Windows
devtools::has_devel()
# Install the packages Rcpp and RcppArmadillo
install.packages(c("Rcpp", "RcppArmadillo"))
# Load package Rcpp
library(Rcpp)
# Get documentation for package Rcpp
# Get short description
packageDescription("Rcpp")
# Load help page
help(package="Rcpp")
# List all datasets in "Rcpp"
data(package="Rcpp")
# List all objects in "Rcpp"
ls("package:Rcpp")
# Remove Rcpp from search path
detach("package:Rcpp")

# Define Rcpp function
Rcpp::cppFunction("
  int times_two(int x)
    { return 2 * x;}
  ")  # end cppFunction
# Run Rcpp function
times_two(3)
# Source Rcpp functions from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/mult_rcpp.cpp")
# Multiply two numbers
mult_rcpp(2, 3)
mult_rcpp(1:3, 6:4)
# Multiply two vectors
mult_vec_rcpp(2, 3)
mult_vec_rcpp(1:3, 6:4)

# Define Rcpp function with loop
Rcpp::cppFunction("
double inner_mult(NumericVector x, NumericVector y) {
int xsize = x.size();
int ysize = y.size();
if (xsize != ysize) {
    return 0;
  } else {
    double total = 0;
    for(int i = 0; i < xsize; ++i) {
total += x[i] * y[i];
  }
  return total;
  }
}")  # end cppFunction
# Run Rcpp function
inner_mult(1:3, 6:4)
inner_mult(1:3, 6:3)
# Define Rcpp Sugar function with loop
Rcpp::cppFunction("
double inner_sugar(NumericVector x, NumericVector y) {
  return sum(x * y);
}")  # end cppFunction
# Run Rcpp Sugar function
inner_sugar(1:3, 6:4)
inner_sugar(1:3, 6:3)

# Define R function with loop
inner_multr <- function(x, y) {
    sumv <- 0
    for(i in 1:NROW(x)) {
sumv <- sumv + x[i] * y[i]
    }
    sumv
}  # end inner_multr
# Run R function
inner_multr(1:3, 6:4)
inner_multr(1:3, 6:3)
# Compare speed of Rcpp and R
library(microbenchmark)
summary(microbenchmark(
  rcode=inner_multr(1:10000, 1:10000),
  innerp=1:10000 %*% 1:10000,
  Rcpp=inner_mult(1:10000, 1:10000),
  sugar=inner_sugar(1:10000, 1:10000),
  times=10))[, c(1, 4, 5)]

# Define Ornstein-Uhlenbeck function in R
sim_our <- function(nrows=1000, priceq=5.0,
              volat=0.01, theta=0.01) {
  retp <- numeric(nrows)
  pricev <- numeric(nrows)
  pricev[1] <- priceq
  for (i in 2:nrows) {
    retp[i] <- theta*(priceq - pricev[i-1]) + volat*rnorm(1)
    pricev[i] <- pricev[i-1] + retp[i]
  }  # end for
  pricev
}  # end sim_our
# Simulate Ornstein-Uhlenbeck process in R
priceq <- 5.0; sigmav <- 0.01
thetav <- 0.01; nrows <- 1000
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")  # Reset random numbers
ousim <- sim_our(nrows, priceq=priceq, volat=sigmav, theta=thetav)

# Define Ornstein-Uhlenbeck function in Rcpp
Rcpp::cppFunction("
NumericVector sim_oucpp(double priceq,
                  double volat,
                  double thetav,
                  NumericVector innov) {
  int nrows = innov.size();
  NumericVector pricev(nrows);
  NumericVector retv(nrows);
  pricev[0] = priceq;
  for (int it = 1; it < nrows; it++) {
    retv[it] = thetav*(priceq - pricev[it-1]) + volat*innov[it-1];
    pricev[it] = pricev[it-1] + retv[it];
  }  // end for
  return pricev;
}")  # end cppFunction
# Simulate Ornstein-Uhlenbeck process in Rcpp
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")  # Reset random numbers
oucpp <- sim_oucpp(priceq=priceq,
  volat=sigmav, theta=thetav, innov=rnorm(nrows))
all.equal(ousim, oucpp)
# Compare speed of Rcpp and R
library(microbenchmark)
summary(microbenchmark(
  rcode=sim_our(nrows, priceq=priceq, volat=sigmav, theta=thetav),
  Rcpp=sim_oucpp(priceq=priceq, volat=sigmav, theta=thetav, innov=rnorm(nrows)),
  times=10))[, c(1, 4, 5)]

# Source Rcpp function for Ornstein-Uhlenbeck process from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/sim_ou.cpp")
# Simulate Ornstein-Uhlenbeck process in Rcpp
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")  # Reset random numbers
oucpp <- sim_oucpp(priceq=priceq,
  volat=sigmav,
  theta=thetav,
  innov=rnorm(nrows))
all.equal(ousim, oucpp)
# Compare speed of Rcpp and R
library(microbenchmark)
summary(microbenchmark(
  rcode=sim_our(nrows, priceq=priceq, volat=sigmav, theta=thetav),
  Rcpp=sim_oucpp(priceq=priceq, volat=sigmav, theta=thetav, innov=rnorm(nrows)),
  times=10))[, c(1, 4, 5)]

# Calculate uniformly distributed pseudo-random sequence
unifun <- function(seedv, nrows=10) {
  datav <- numeric(nrows)
  datav[1] <- seedv
  for (i in 2:nrows) {
    datav[i] <- 4*datav[i-1]*(1-datav[i-1])
  }  # end for
  acos(1-2*datav)/pi
}  # end unifun

# Source Rcpp functions from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/unifun.cpp")
# Microbenchmark Rcpp code
library(microbenchmark)
summary(microbenchmark(
  rcode=runif(1e5),
  rloop=unifun(0.3, 1e5),
  Rcpp=unifuncpp(0.3, 1e5),
  times=10))[, c(1, 4, 5)]

library(RcppArmadillo)
# Source Rcpp functions from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/armadillo_functions.cpp")
vec1 <- runif(1e5)
vec2 <- runif(1e5)
inner_vec(vec1, vec2)
vec1 %*% vec2

# Microbenchmark \emph{RcppArmadillo} code
summary(microbenchmark(
  rcpp = inner_vec(vec1, vec2),
  rcode = (vec1 %*% vec2),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
# Microbenchmark shows:
# inner_vec() is several times faster than %*%, especially for longer vectors.
#     expr     mean   median
# 1 inner_vec 110.7067 110.4530
# 2 rcode 585.5127 591.3575

# Source Rcpp functions from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/sim_arima.cpp")
# Define AR(2) coefficients
coeff <- c(0.9, 0.09)
nrows <- 1e4
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
innov <- rnorm(nrows)
# Simulate ARIMA using filter()
arimar <- filter(x=innov, filter=coeff, method="recursive")
# Simulate ARIMA using sim_ar()
innov <- matrix(innov)
coeff <- matrix(coeff)
arimav <- sim_ar(coeff, innov)
all.equal(drop(arimav), as.numeric(arimar))
# Microbenchmark \emph{RcppArmadillo} code
summary(microbenchmark(
  rcpp = sim_ar(coeff, innov),
  filter = filter(x=innov, filter=coeff, method="recursive"),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary

library(RcppArmadillo)
# Source Rcpp functions from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/armadillo_functions.cpp")
matv <- matrix(runif(1e5), nc=1e3)
# Center matrix columns using apply()
matd <- apply(matv, 2, function(x) (x-mean(x)))
# Center matrix columns in place using Rcpp demeanr()
demeanr(matv)
all.equal(matd, matv)
# Microbenchmark \emph{RcppArmadillo} code
library(microbenchmark)
summary(microbenchmark(
  rcode = (apply(matv, 2, mean)),
  rcpp = demeanr(matv),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
# Perform matrix inversion
# Create random positive semi-definite matrix
matv <- matrix(runif(25), nc=5)
matv <- t(matv) %*% matv
# Invert the matrix
matrixinv <- solve(matv)
inv_mat(matv)
all.equal(matrixinv, matv)
# Microbenchmark \emph{RcppArmadillo} code
summary(microbenchmark(
  rcode = solve(matv),
  rcpp = inv_mat(matv),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary

library(RcppArmadillo)
# Source Rcpp functions from file
Rcpp::sourceCpp("/Users/jerzy/Develop/lecture_slides/scripts/HighFreq.cpp")
# Calculate matrix of random returns
matv <- matrix(rnorm(300), nc=5)
# Reduced inverse of correlation matrix
dimax <- 4
cormat <- cor(matv)
eigend <- eigen(cormat)
invmat <- eigend$vectors[, 1:dimax] %*%
  (t(eigend$vectors[, 1:dimax]) / eigend$values[1:dimax])
# Reduced inverse using \emph{RcppArmadillo}
invarma <- calc_inv(cormat, dimax=dimax)
all.equal(invmat, invarma)
# Microbenchmark \emph{RcppArmadillo} code
library(microbenchmark)
summary(microbenchmark(
  rcode = {eigend <- eigen(cormat)
eigend$vectors[, 1:dimax] %*% (t(eigend$vectors[, 1:dimax]) / eigend$values[1:dimax])},
  rcpp = calc_inv(cormat, dimax=dimax),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary

# Install package reticulate
install.packages("reticulate")
# Start Python session
reticulate::repl_python()
# Exit Python session
exit
