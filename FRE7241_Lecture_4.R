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
coeff <- drop(predinv %*% respv[insample, ])
names(coeff) <- colnames(predm)
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
# Calculate the t-values of the AR coefficients
resids <- (fcasts[insample, ] - respv[insample, ])
varv <- sum(resids^2)/(nrows-NROW(coeff))
pred2 <- crossprod(predm[insample, ])
covmat <- varv*MASS::ginv(pred2)
coefsd <- sqrt(diag(covmat))
coefft <- drop(coeff/coefsd)
names(coefft) <- colnames(predm)
# Plot the t-values of the AR coefficients
barplot(coefft, xlab="", ylab="t-value", col="grey",
  main="Coefficient t-values of AR Forecasting Model")
# Calculate the autoregressive strategy PnLs
fcasts <- predm %*% coefft
fcastv <- sqrt(HighFreq::run_var(fcasts, lambda=0.8)[, 2])
fcastv[1:100] <- 1
fcasts <- fcasts/fcastv
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
  main="Multifactor Autoregressive Strategy Using t-Values") %>%
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
  sum((respv[insample, ] - fcasts)^2) + lambdaf*sum(coeff^2)
}  # end objfun
# Perform optimization using the quasi-Newton method
optiml <- optim(par=numeric(orderp+1),
          fn=objfun, lambdaf=50.0, method="L-BFGS-B",
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
# Calculate the returns of VTI, TLT, and VXX
retp <- na.omit(rutils::etfenv$returns[, c("VTI", "TLT", "VXX")])
datev <- zoo::index(retp)
nrows <- NROW(retp)
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
coeff <- drop(predinv %*% respv[insample, ])
names(coeff) <- colnames(predm)
barplot(coeff, xlab="", ylab="t-value", col="grey",
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
dimv <- 2:7
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
  sum((respv - fcasts)^2) + lambdaf*sum(coeff^2)
}  # end objfun
# Perform optimization using the quasi-Newton method
ncoeff <- NROW(coeff)
optiml <- optim(par=numeric(ncoeff),
          fn=objfun,
          respv=respv[insample, ], predm=predm[insample, ],
          lambdaf=10.0,
          method="L-BFGS-B",
          upper=rep(10000, ncoeff),
          lower=rep(-10000, ncoeff))
# Extract the AR coefficients
coeff <- optiml$par
names(coeff) <- colnames(predm)
barplot(coeff, xlab="", ylab="t-value", col="grey",
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
# Load constant maturity Treasury rates
load(file="/Users/jerzy/Develop/lecture_slides/data/rates_data.RData")
# Combine rates into single xts series
ratev <- do.call(cbind, as.list(ratesenv))
# Sort the columns of rates according bond maturity
namev <- colnames(ratev)
namev <- substr(namev, start=4, stop=10)
namev <- as.numeric(namev)
indeks <- order(namev)
ratev <- ratev[, indeks]
# Align rates dates with VTI prices
closep <- log(quantmod::Cl(rutils::etfenv$VTI))
colnames(closep) <- "VTI"
nrows <- NROW(closep)
datev <- zoo::index(closep)
ratev <- na.omit(ratev[datev])
closep <- closep[zoo::index(ratev)]
datev <- zoo::index(closep)
# Calculate VTI returns and IR changes
retp <- rutils::diffit(closep)
retr <- rutils::diffit(ratev)
# Regress VTI returns versus the lagged rate differences
predm <- rutils::lagit(retr)
regmod <- lm(retp ~ predm - 1)
summary(regmod)
# Regress VTI returns before and after 2012
summary(lm(retp["/2012"] ~ predm["/2012"]))
summary(lm(retp["2012/"] ~ predm["2012/"]))
# Calculate PCA of rates correlation matrix
eigend <- eigen(cor(retr))
pcar <- -(retr %*% eigend$vectors)
colnames(pcar) <- paste0("PC", 1:6)
# Define predictor as the YC PCAs
predm <- rutils::lagit(pcar)
regmod <- lm(retp ~ predm)
summary(regmod)
# Plot YC steepener principal component with VTI
datav <- cbind(retp, pcar[, 2])
colnames(datav) <- c("VTI", "Steepener")
colv <- colnames(datav)
dygraphs::dygraph(cumsum(datav),
  main="VTI and Yield Curve Steepener") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=colv[2], axis="y2", strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)
# Define predictor with intercept term
predm <- rutils::lagit(retr)
predm <- cbind(rep(1, NROW(predm)), predm)
colnames(predm)[1] <- "intercept"
# Calculate inverse of predictor
invreg <- MASS::ginv(predm)
# Calculate coefficients from response and inverse of predictor
respv <- retp
coeff <- drop(invreg %*% respv)
# Calculate forecasts and PnLs in-sample
fcasts <- (predm %*% coeff)
pnls <- sign(fcasts)*respv
# Calculate in-sample factors
factv <- (predm*coeff)
apply(factv, 2, sd)
# Plot dygraph of in-sample IR strategy
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
colv <- colnames(wealthv)
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Yield Curve Strategy In-sample") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=colv[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Define in-sample and out-of-sample intervals
insample <- (datev < as.Date("2020-01-01"))
outsample <- (datev >= as.Date("2020-01-01"))
# Calculate inverse of predictor in-sample
invreg <- MASS::ginv(predm[insample, ])
# Calculate coefficients in-sample
coeff <- drop(invreg %*% respv[insample, ])
# Calculate forecasts and PnLs out-of-sample
fcasts <- (predm[outsample, ] %*% coeff)
pnls <- sign(fcasts)*respv[outsample, ]
# Plot dygraph of out-of-sample IR PCA strategy
wealthv <- cbind(retp[outsample, ], pnls)
colnames(wealthv) <- c("VTI", "Strategy")
colv <- colnames(wealthv)
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Yield Curve Strategy Out-of-Sample") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=colv[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Define yearly dates
endd <- rutils::calc_endpoints(closep, interval="years")
endd <- index(closep)[endd]
# Perform loop over yearly dates
pnls <- lapply(2:(NROW(endd)-1), function(tday) {
  # Define in-sample and out-of-sample intervals
  insample <- (datev > endd[tday-1]) & (datev < endd[tday])
  outsample <- (datev >= endd[tday]) & (datev < endd[tday+1])
  # Calculate coefficients in-sample
  invreg <- MASS::ginv(predm[insample, ])
  coeff <- drop(invreg %*% respv[insample, ])
  # Calculate forecasts and PnLs out-of-sample
  fcasts <- (predm[outsample, ] %*% coeff)
  sign(fcasts)*respv[outsample, ]
})  # end lapply
pnls <- do.call(rbind, pnls)
# Plot dygraph of rolling yearly IR strategy
retp <- rutils::diffit(closep[zoo::index(pnls),])
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
colv <- colnames(wealthv)
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Rolling Yearly Yield Curve Strategy") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=colv[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Define monthly dates
endd <- rutils::calc_endpoints(closep, interval="month")
endd <- index(closep)[endd]
# Perform loop over monthly dates
pnls <- lapply(12:(NROW(endd)-1), function(tday) {
  # Define in-sample and out-of-sample intervals
  insample <- (datev > endd[tday-11]) & (datev < endd[tday])
  outsample <- (datev > endd[tday]) & (datev < endd[tday+1])
  # Calculate forecasts and PnLs out-of-sample
  invreg <- MASS::ginv(predm[insample, ])
  coeff <- drop(invreg %*% respv[insample, ])
  fcasts <- (predm[outsample, ] %*% coeff)
  sign(fcasts)*respv[outsample, ]
})  # end lapply
pnls <- do.call(rbind, pnls)
# Plot dygraph of rolling monthly IR strategy
retp <- rutils::diffit(closep[zoo::index(pnls),])
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
colv <- colnames(wealthv)
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Rolling Monthly Yield Curve Strategy") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=colv[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Define weekly dates
endd <- rutils::calc_endpoints(closep, interval="weeks")
endd <- index(closep)[endd]
# Perform loop over weekly dates
pnls <- lapply(51:(NROW(endd)-1), function(tday) {
  # Define in-sample and out-of-sample intervals
  insample <- (datev > endd[tday-10]) & (datev < endd[tday])
  outsample <- (datev > endd[tday]) & (datev < endd[tday+1])
  # Calculate forecasts and PnLs out-of-sample
  invreg <- MASS::ginv(predm[insample, ])
  coeff <- drop(invreg %*% respv[insample, ])
  fcasts <- (predm[outsample, ] %*% coeff)
  sign(fcasts)*respv[outsample, ]
})  # end lapply
pnls <- do.call(rbind, pnls)
# Plot dygraph of rolling weekly IR strategy
retp <- rutils::diffit(closep[zoo::index(pnls),])
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
colv <- colnames(wealthv)
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Rolling Weekly Yield Curve Strategy") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=colv[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Calculate in-sample pnls for different dimax values
dimv <- 2:7
pnls <- lapply(dimv, function(dimax) {
  invred <- HighFreq::calc_invsvd(predm, dimax=dimax)
  coeff <- drop(invred %*% respv)
  fcasts <- (predm %*% coeff)
  sign(fcasts)*respv
})
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("eigen", dimv)
# Plot dygraph of in-sample pnls
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
endd <- rutils::calc_endpoints(pnls, interval="weeks")
dygraphs::dygraph(cumsum(pnls)[endd], main="In-Sample Returns of Shrinkage YC Strategies") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Define in-sample and out-of-sample intervals
insample <- (datev < as.Date("2020-01-01"))
outsample <- (datev >= as.Date("2020-01-01"))
# Calculate in-sample pnls for different dimax values
dimv <- 2:7
pnls <- lapply(dimv, function(dimax) {
  invred <- HighFreq::calc_invsvd(predm[insample, ], dimax=dimax)
  coeff <- drop(invred %*% respv[insample, ])
  fcasts <- (predm[outsample, ] %*% coeff)
  sign(fcasts)*respv[outsample, ]
})
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("eigen", dimv)
# Plot dygraph of out-of-sample pnls
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
endd <- rutils::calc_endpoints(pnls, interval="weeks")
dygraphs::dygraph(cumsum(pnls)[endd], main="Out-of-Sample Returns of Shrinkage YC Strategies") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Define monthly dates
endd <- rutils::calc_endpoints(closep, interval="month")
endd <- index(closep)[endd]
# Perform loop over monthly dates
lookb <- 6
dimax <- 3
pnls <- lapply((lookb+1):(NROW(endd)-1), function(tday) {
  # Define in-sample and out-of-sample intervals
  insample <- (datev > endd[tday-lookb]) & (datev < endd[tday])
  outsample <- (datev > endd[tday]) & (datev < endd[tday+1])
  # Calculate forecasts and PnLs out-of-sample
  invred <- HighFreq::calc_invsvd(predm[insample, ], dimax=dimax)
  coeff <- drop(invred %*% respv[insample, ])
  fcasts <- (predm[outsample, ] %*% coeff)
  sign(fcasts)*respv[outsample, ]
})  # end lapply
pnls <- do.call(rbind, pnls)
# Plot dygraph of rolling monthly IR strategy
retp <- rutils::diffit(closep[zoo::index(pnls),])
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
colv <- colnames(wealthv)
endd <- rutils::calc_endpoints(pnls, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Rolling Monthly Shrinkage YC Strategy") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=colv[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Define weekly dates
endd <- rutils::calc_endpoints(closep, interval="weeks")
endd <- index(closep)[endd]
# Perform loop over weekly dates
lookb <- 24
dimax <- 4
pnls <- lapply((lookb+1):(NROW(endd)-1), function(tday) {
  # Define in-sample and out-of-sample intervals
  insample <- (datev > endd[tday-lookb]) & (datev < endd[tday])
  outsample <- (datev > endd[tday]) & (datev < endd[tday+1])
  # Calculate forecasts and PnLs out-of-sample
  invred <- HighFreq::calc_invsvd(predm[insample, ], dimax=dimax)
  coeff <- drop(invred %*% respv[insample, ])
  fcasts <- (predm[outsample, ] %*% coeff)
  sign(fcasts)*respv[outsample, ]
})  # end lapply
pnls <- do.call(rbind, pnls)
# Plot dygraph of rolling weekly IR strategy
retp <- rutils::diffit(closep[zoo::index(pnls),])
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
colv <- colnames(wealthv)
endd <- rutils::calc_endpoints(pnls, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Rolling Weekly Shrinkage YC Strategy") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=colv[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Calculate the percentage VTI returns
retvti <- na.omit(rutils::etfenv$returns$VTI)
colnames(retvti) <- "VTI"
datev <- zoo::index(retvti)
nrows <- NROW(retvti)
# Load daily S&P500 stock prices
load(file="/Users/jerzy/Develop/lecture_slides/data/sp500_prices.RData")
# Select the stock prices since VTI
pricestock <- pricestock[datev]
# Select stocks with no NA values in their prices
numna <- sapply(pricestock, function(x) sum(is.na(x)))
pricestock <- pricestock[, numna == 0]
# Drop penny stocks
pricel <- last(pricestock)
pricel <- drop(coredata(pricel))
pricestock <- pricestock[, pricel > 1]
# Calculate the dollar and percentage stock returns
retd <- rutils::diffit(pricestock)
retp <- retd/rutils::lagit(pricestock)
retp[1, ] <- 0
# Wealth of fixed shares portfolio
wealthfs <- rowMeans(cumprod(1 + retp))
# Wealth of equal wealth portfolio (with rebalancing)
wealthew <- cumprod(1 + rowMeans(retp))
# Calculate combined log wealth
wealthv <- cbind(wealthfs, wealthew)
wealthv <- log(wealthv)
wealthv <- xts::xts(wealthv, datev)
colnames(wealthv) <- c("Fixed shares", "Equal wealth")
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(rutils::diffit(wealthv), function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of combined log wealth
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(wealthv[endd],
  main="Wealth of Fixed Share and Equal Wealth Portfolios") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Select a random, fixed share sub-portfolio of 5 stocks
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
nstocks <- NCOL(retp)
samplev <- sample.int(n=nstocks, size=5, replace=FALSE)
wealthr <- rowMeans(cumprod(1 + retp[, samplev]))
# Plot dygraph of all stocks and random sub-portfolio
wealthv <- cbind(wealthfs, wealthr)
wealthv <- log(wealthv)
wealthv <- xts::xts(wealthv, order.by=datev)
colnames(wealthv) <- c("All stocks", "Random sub-portfolio")
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(wealthv[endd], main="Stock Index and Random Portfolio") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Select 10 random fixed share sub-portfolios
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
nportf <- 10
wealthr <- sapply(1:nportf, function(x) {
  samplev <- sample.int(n=nstocks, size=5, replace=FALSE)
  rowMeans(cumprod(1 + retp[, samplev]))
})  # end sapply
wealthr <- xts::xts(wealthr, order.by=datev)
colnames(wealthr) <- paste0("portf", 1:nportf)
# Sort the sub-portfolios according to performance
wealthr <- wealthr[, order(wealthr[nrows])]
round(head(wealthr), 3)
round(tail(wealthr), 3)
# Plot dygraph of all stock index and random sub-portfolios
colorv <- colorRampPalette(c("red", "blue"))(nportf)
colorv <- c("green", colorv)
wealthv <- cbind(wealthfs, wealthr)
wealthv <- log(wealthv)
colnames(wealthv)[1] <- "Index"
colv <- colnames(wealthv)
dygraphs::dygraph(wealthv[endd], main="Stock Index and Random Portfolios") %>%
  dyOptions(colors=colorv, strokeWidth=1) %>%
  dySeries(name=colv[1], strokeWidth=3) %>%
  dyLegend(show="always", width=500)
# Define in-sample and out-of-sample intervals
cutoff <- nrows %/% 2
datev[cutoff]
insample <- 1:cutoff
outsample <- (cutoff + 1):nrows
# Calculate the 10 best performing stocks in-sample
pricev <- cumprod(1 + retp)
pricet <- pricev[cutoff, ]
pricet <- drop(coredata(pricet))
pricet <- sort(pricet, decreasing=TRUE)
symbolv <- names(head(pricet, 10))
# Calculate the wealth of the 10 best performing stocks
wealthb <- rowMeans(pricev[insample, symbolv])
wealthos <- wealthb[cutoff]*rowMeans(cumprod(1 + retp[outsample, symbolv]))
wealthb <- c(wealthb, wealthos)
# Combine the fixed share wealth with the 10 best performing stocks
wealthv <- cbind(wealthfs, wealthb)
wealthv <- xts::xts(log(wealthv), order.by=datev)
colnames(wealthv) <- c("All stocks", "Best performing")
# Calculate the in-sample Sharpe and Sortino ratios
sqrt(252)*sapply(rutils::diffit(wealthv[insample, ]),
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(rutils::diffit(wealthv[outsample, ]),
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot out-of-sample stock portfolio returns
dygraphs::dygraph(wealthv[endd], main="Out-of-Sample Log Prices of Stock Portfolio") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyEvent(datev[cutoff], label="cutoff", strokePattern="solid", color="green") %>%
  dyLegend(width=300)
# Calculate the stock volatilities, betas, and alphas
# Perform parallel loop under Mac-OSX or Linux
library(parallel)  # Load package parallel
ncores <- detectCores() - 1
riskret <- mclapply(retp, function(rets) {
  rets <- na.omit(rets)
  stdev <- sd(rets)
  retvti <- retvti[zoo::index(rets)]
  varvti <- drop(var(retvti))
  meanvti <- mean(retvti)
  betac <- drop(cov(rets, retvti))/varvti
  resid <- rets - betac*retvti
  alphac <- mean(rets) - betac*meanvti
  c(alpha=alphac, beta=betac, stdev=stdev, ivol=sd(resid))
}, mc.cores=ncores)  # end mclapply
riskret <- do.call(rbind, riskret)
tail(riskret)
# Calculate the median volatility
riskv <- riskret[, "stdev"]
medianv <- median(riskv)
# Calculate the returns of low and high volatility stocks
retlow <- rowMeans(retp[, (riskv <= medianv)], na.rm=TRUE)
rethigh <- rowMeans(retp[, (riskv > medianv)], na.rm=TRUE)
wealthv <- cbind(retlow, rethigh, retlow - 0.25*rethigh)
wealthv <- xts::xts(wealthv, order.by=datev)
colv <- c("low_vol", "high_vol", "long_short")
colnames(wealthv) <- colv
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of cumulative returns of low and high volatility stocks
dygraphs::dygraph(cumsum(wealthv)[endd], main="Low and High Volatility Stocks In-Sample") %>%
  dySeries(name=colv[1], col="blue", strokeWidth=1) %>%
  dySeries(name=colv[2], col="red", strokeWidth=1) %>%
  dySeries(name=colv[3], col="green", strokeWidth=2) %>%
  dyLegend(width=300)
# Merton-Henriksson test
desm <- cbind(VTI=retvti, 0.5*(retvti+abs(retvti)), retvti^2)
colnames(desm)[2:3] <- c("Merton", "Treynor")
regmod <- lm(wealthv$long_short ~ VTI + Merton, data=desm); summary(regmod)
# Treynor-Mazuy test
regmod <- lm(wealthv$long_short ~ VTI + Treynor, data=desm); summary(regmod)
# Plot residual scatterplot
resids <- regmod$residuals
plot.default(x=retvti, y=resids, xlab="VTI", ylab="Low Volatility")
title(main="Treynor-Mazuy Market Timing Test\n for Low Volatility vs VTI", line=0.5)
# Plot fitted (predicted) response values
coefreg <- summary(regmod)$coeff
fitv <- regmod$fitted.values - coefreg["VTI", "Estimate"]*retvti
tvalue <- round(coefreg["Treynor", "t value"], 2)
points.default(x=retvti, y=fitv, pch=16, col="red")
text(x=0.0, y=max(resids), paste("Treynor test t-value =", tvalue))
# Calculate the in-sample stock volatilities, betas, and alphas
riskretis <- mclapply(retp[insample], function(rets) {
  combv <- na.omit(cbind(rets, retvti))
  if (NROW(combv) > 11) {
    rets <- na.omit(rets)
    stdev <- sd(rets)
    retvti <- retvti[zoo::index(rets)]
    varvti <- drop(var(retvti))
    meanvti <- mean(retvti)
    betac <- drop(cov(rets, retvti))/varvti
    resid <- rets - betac*retvti
    alphac <- mean(rets) - betac*meanvti
    return(c(alpha=alphac, beta=betac, stdev=stdev, ivol=sd(resid)))
  } else {
    return(c(alpha=0, beta=0, stdev=0, ivol=0))
  }  # end if
}, mc.cores=ncores)  # end mclapply
riskretis <- do.call(rbind, riskretis)
tail(riskretis)
# Calculate the median volatility
riskv <- riskretis[, "stdev"]
medianv <- median(riskv)
# Calculate the out-of-sample returns of low and high volatility stocks
retlow <- rowMeans(retp[outsample, (riskv <= medianv)], na.rm=TRUE)
rethigh <- rowMeans(retp[outsample, (riskv > medianv)], na.rm=TRUE)
wealthv <- cbind(retlow, rethigh, retlow - 0.25*rethigh)
wealthv <- xts::xts(wealthv, order.by=datev[outsample])
colv <- c("low_vol", "high_vol", "long_short")
colnames(wealthv) <- colv
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of cumulative returns of low and high volatility stocks
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd], main="Low and High Volatility Stocks Out-Of-Sample") %>%
  dySeries(name=colv[1], col="blue", strokeWidth=1) %>%
  dySeries(name=colv[2], col="red", strokeWidth=1) %>%
  dySeries(name=colv[3], col="green", strokeWidth=2) %>%
  dyLegend(width=300)
# Calculate the median idiosyncratic volatility
riskv <- riskret[, "ivol"]
medianv <- median(riskv)
# Calculate the returns of low and high idiosyncratic volatility stocks
retlow <- rowMeans(retp[, (riskv <= medianv)], na.rm=TRUE)
rethigh <- rowMeans(retp[, (riskv > medianv)], na.rm=TRUE)
wealthv <- cbind(retlow, rethigh, retlow - 0.25*rethigh)
wealthv <- xts::xts(wealthv, order.by=datev)
colv <- c("low_vol", "high_vol", "long_short")
colnames(wealthv) <- colv
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of returns of low and high idiosyncratic volatility stocks
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd], main="Low and High Idiosyncratic Volatility Stocks In-Sample") %>%
  dySeries(name=colv[1], col="blue", strokeWidth=1) %>%
  dySeries(name=colv[2], col="red", strokeWidth=1) %>%
  dySeries(name=colv[3], col="green", strokeWidth=2) %>%
  dyLegend(width=300)
# Merton-Henriksson test
desm <- cbind(VTI=retvti, 0.5*(retvti+abs(retvti)), retvti^2)
colnames(desm)[2:3] <- c("Merton", "Treynor")
regmod <- lm(wealthv$long_short ~ VTI + Merton, data=desm); summary(regmod)
# Treynor-Mazuy test
regmod <- lm(wealthv$long_short ~ VTI + Treynor, data=desm); summary(regmod)
# Plot residual scatterplot
resids <- regmod$residuals
plot.default(x=retvti, y=resids, xlab="VTI", ylab="Low Volatility")
title(main="Treynor-Mazuy Market Timing Test\n for Low Idiosyncratic Volatility vs VTI", line=0.5)
# Plot fitted (predicted) response values
coefreg <- summary(regmod)$coeff
fitv <- regmod$fitted.values - coefreg["VTI", "Estimate"]*retvti
tvalue <- round(coefreg["Treynor", "t value"], 2)
points.default(x=retvti, y=fitv, pch=16, col="red")
text(x=0.0, y=max(resids), paste("Treynor test t-value =", tvalue))
# Calculate the median in-sample idiosyncratic volatility
riskv <- riskretis[, "ivol"]
medianv <- median(riskv)
# Calculate the out-of-sample returns of low and high idiosyncratic volatility stocks
retlow <- rowMeans(retp[outsample, (riskv <= medianv)], na.rm=TRUE)
rethigh <- rowMeans(retp[outsample, (riskv > medianv)], na.rm=TRUE)
wealthv <- cbind(retlow, rethigh, retlow - 0.25*rethigh)
wealthv <- xts::xts(wealthv, order.by=datev[outsample])
colv <- c("low_vol", "high_vol", "long_short")
colnames(wealthv) <- colv
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of out-of-sample returns of low and high volatility stocks
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd], main="Low and High Idiosyncratic Volatility Stocks Out-Of-Sample") %>%
  dySeries(name=colv[1], col="blue", strokeWidth=1) %>%
  dySeries(name=colv[2], col="red", strokeWidth=1) %>%
  dySeries(name=colv[3], col="green", strokeWidth=2) %>%
  dyLegend(width=300)
# Calculate the median beta
riskv <- riskret[, "beta"]
medianv <- median(riskv)
# Calculate the returns of low and high beta stocks
betalow <- rowMeans(retp[, names(riskv[riskv <= medianv])], na.rm=TRUE)
betahigh <- rowMeans(retp[, names(riskv[riskv > medianv])], na.rm=TRUE)
wealthv <- cbind(betalow, betahigh, betalow - 0.25*betahigh)
wealthv <- xts::xts(wealthv, order.by=datev)
colv <- c("low_beta", "high_beta", "long_short")
colnames(wealthv) <- colv
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of cumulative returns of low and high beta stocks
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd], main="Low and High Beta Stocks In-Sample") %>%
  dySeries(name=colv[1], col="blue", strokeWidth=1) %>%
  dySeries(name=colv[2], col="red", strokeWidth=1) %>%
  dySeries(name=colv[3], col="green", strokeWidth=2) %>%
  dyLegend(width=300)
# Merton-Henriksson test
desm <- cbind(VTI=retvti, 0.5*(retvti+abs(retvti)), retvti^2)
colnames(desm)[2:3] <- c("Merton", "Treynor")
regmod <- lm(wealthv$long_short ~ VTI + Merton, data=desm); summary(regmod)
# Treynor-Mazuy test
regmod <- lm(wealthv$long_short ~ VTI + Treynor, data=desm); summary(regmod)
# Plot residual scatterplot
resids <- regmod$residuals
plot.default(x=retvti, y=resids, xlab="VTI", ylab="Low Beta")
title(main="Treynor-Mazuy Market Timing Test\n for Low Beta vs VTI", line=0.5)
# Plot fitted (predicted) response values
coefreg <- summary(regmod)$coeff
fitv <- regmod$fitted.values - coefreg["VTI", "Estimate"]*retvti
tvalue <- round(coefreg["Treynor", "t value"], 2)
points.default(x=retvti, y=fitv, pch=16, col="red")
text(x=0.0, y=max(resids), paste("Treynor test t-value =", tvalue))
# Calculate the median beta
riskv <- riskretis[, "beta"]
medianv <- median(riskv)
# Calculate the out-of-sample returns of low and high beta stocks
betalow <- rowMeans(retp[outsample, names(riskv[riskv <= medianv])], na.rm=TRUE)
betahigh <- rowMeans(retp[outsample, names(riskv[riskv > medianv])], na.rm=TRUE)
wealthv <- cbind(betalow, betahigh, betalow - 0.25*betahigh)
wealthv <- xts::xts(wealthv, order.by=datev[outsample])
colv <- c("low_beta", "high_beta", "long_short")
colnames(wealthv) <- colv
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of out-of-sample returns of low and high beta stocks
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd], main="Low and High Beta Stocks Out-Of-Sample") %>%
  dySeries(name=colv[1], col="blue", strokeWidth=1) %>%
  dySeries(name=colv[2], col="red", strokeWidth=1) %>%
  dySeries(name=colv[3], col="green", strokeWidth=2) %>%
  dyLegend(width=300)
# Calculate the trailing percentage volatilities
volp <- HighFreq::run_var(retp, lambda=0.15)
volp <- sqrt(volp[, (nstocks+1):(2*nstocks)])
volp <- rutils::lagit(volp)
volp[volp == 0] <- 1
# Calculate the median volatilities
medianv <- matrixStats::rowMedians(volp)
# Calculate the wealth of low volatility stocks
weightv <- (volp <= medianv)
weightv <- rutils::lagit(weightv)
retlow <- rowMeans(weightv*retp)
# Calculate the wealth of high volatility stocks
weightv <- (volp > medianv)
weightv <- rutils::lagit(weightv)
rethigh <- rowMeans(weightv*retp)
# Combined wealth
wealthv <- cbind(retlow, rethigh)
wealthv <- xts::xts(wealthv, datev)
colnames(wealthv) <- c("LowVol", "HighVol")
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of log wealth
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Wealth of Low and High Volatility Stocks") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Calculate the returns of equal wealth portfolio
retew <- rowMeans(retp, na.rm=TRUE)
retew[1] <- 0
# Calculate the long-short volatility returns
retls <- (retlow - 0.25*rethigh)
# Scale the PnL volatility to that of wealthew
retls <- retls*sd(retew)/sd(retls)
# Combined wealth
wealthv <- cbind(retew, retls)
wealthv <- xts::xts(wealthv, datev)
colv <- c("Equal Weight", "Long-Short Vol")
colnames(wealthv) <- colv
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of log wealth
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Equal Weight and Long-Short Vol Portfolios") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Calculate the trailing dollar volatilities
lambdav <- 0.99
vold <- HighFreq::run_var(retd, lambda=lambdav)
vold <- vold[, (nstocks+1):(2*nstocks)]
vold <- sqrt(vold)
vold[vold == 0] <- 1
# Calculate the rolling risk parity weights
weightv <- 1/vold
weightv <- weightv/rowSums(weightv)
# Wealth of equal wealth portfolio (with rebalancing)
wealthew <- cumprod(1 + rowMeans(retp))
# Calculate the risk parity allocations
pricerp <- pricestock*weightv
# Calculate the dollar returns of risk parity
retrp <- retp*rutils::lagit(pricerp)
retrp[1, ] <- pricerp[1, ]
# Calculate the wealth of risk parity
wealthrp <- cumsum(rowSums(retrp))
# Combined wealth
wealthv <- cbind(wealthrp[1]*wealthew, wealthrp)
wealthv <- xts::xts(wealthv, datev)
colnames(wealthv) <- c("Equal wealth", "Risk parity")
wealthv <- log(wealthv)
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(rutils::diffit(wealthv), function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of log wealth
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(wealthv[endd],
  main="Wealth of Equal Wealth and Risk Parity Portfolios") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=400)
