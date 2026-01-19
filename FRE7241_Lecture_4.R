# Download time series of bond yields
# symbolv <- c("DGS1", "DGS2", "DGS5", "DGS10", "DGS20", "DGS30")
# ratesenv <- new.env()
# quantmod::getSymbols(symbolv, env=ratesenv, src="FRED")
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
datev <- zoo::index(closep)
ratev <- na.omit(ratev[datev])
closep <- closep[zoo::index(ratev)]
datev <- zoo::index(closep)
nrows <- NROW(closep)

# Calculate VTI returns and IR changes
retp <- rutils::diffit(closep)
retr <- rutils::diffit(ratev)
# Regress VTI returns versus the lagged rate differences
predm <- rutils::lagit(retr)
regmod <- lm(retp ~ predm)
summary(regmod)
# Regress VTI returns before and after 2010
summary(lm(retp["/2010"] ~ predm["/2010"]))
summary(lm(retp["2010/"] ~ predm["2010/"]))

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
fcastv <- sqrt(HighFreq::run_var(fcasts, lambda=0.4)[, 2])
fcasts <- ifelse(fcastv > mad(fcastv)/20, fcasts/fcastv, 0)
pnls <- fcasts*respv
# Calculate the in-sample factors
factv <- lapply(1:NCOL(predm), function(x) predm[, x]*coeff[x])
factv <- do.call(cbind, factv)
# Or:
# foo <- HighFreq::mult_mat(coeff, predm)
# all.equal(foo, coredata(factv), check.attributes=FALSE)
# Calculate the factor volatilities
apply(factv, 2, sd)
# Scale the PnL volatility to that of VTI
pnls <- pnls*sd(respv)/sd(pnls)

# Plot dygraph of in-sample YC strategy
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
cor(wealthv)
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
colv <- colnames(wealthv)
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Yield Curve Strategy In-sample") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Calculate inverse of predictor in-sample
invreg <- MASS::ginv(predm["/2018"])
# Calculate coefficients in-sample
coeff <- drop(invreg %*% respv["/2018"])
# Calculate forecasts and PnLs
fcasts <- (predm %*% coeff)
fcastv <- sqrt(HighFreq::run_var(fcasts, lambda=0.4)[, 2])
fcasts <- ifelse(fcastv > mad(fcastv)/20, fcasts/fcastv, 0)
pnls <- fcasts*respv
# Scale the PnL volatility to that of VTI
pnls <- pnls*sd(respv)/sd(pnls)

# Plot dygraph of out-of-sample YC strategy
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
colv <- colnames(wealthv)
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Yield Curve Strategy Out-of-Sample") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyEvent(end(predm["/2018"]), label="cutoff", strokePattern="solid", color="red") %>%
  dyLegend(show="always", width=300)

# Define yearly dates
endd <- rutils::calc_endpoints(respv, interval="years")
# endd <- index(closep)[endd]
# Perform loop over yearly dates
library(parallel)  # Load package parallel
ncores <- detectCores() - 1
lookb <- 500
fcasts <- mclapply(seq_along(endd)[-1], function(tday) {
  # Define in-sample and out-of-sample intervals
  insample <- (max(1, endd[tday-1]-lookb):endd[tday-1])
  # insample <- (1:endd[tday-1]) # Expanding look-back
  outsample <- (endd[tday-1]+1):endd[tday]
  # Calculate coefficients in-sample
  invreg <- MASS::ginv(predm[insample, ])
  coeff <- drop(invreg %*% respv[insample, ])
  # Calculate forecasts out-of-sample
  fcasts <- (predm[outsample, ] %*% coeff)
}, mc.cores=ncores)  # end mclapply
fcasts <- do.call(rbind, fcasts)
# fcastv <- sqrt(HighFreq::run_var(fcasts, lambda=0.4)[, 2])
# fcasts <- ifelse(fcastv > mad(fcastv)/20, fcasts/fcastv, 0)
pnls <- fcasts*respv
# Scale the PnL volatility to that of VTI
pnls <- pnls*sd(respv)/sd(pnls)

# Plot dygraph of rolling yearly YC strategy
wealthv <- cbind(respv, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
colv <- colnames(wealthv)
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Rolling Yearly Yield Curve Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Define monthly dates
endd <- rutils::calc_endpoints(respv, interval="month")
fcasts <- mclapply(seq_along(endd)[-1], function(tday) {
  # Define in-sample and out-of-sample intervals
  insample <- (max(1, endd[tday-1]-lookb):endd[tday-1])
  # insample <- (1:endd[tday-1]) # Expanding look-back
  outsample <- (endd[tday-1]+1):endd[tday]
  # Calculate coefficients in-sample
  invreg <- MASS::ginv(predm[insample, ])
  coeff <- drop(invreg %*% respv[insample, ])
  # Calculate forecasts out-of-sample
  fcasts <- (predm[outsample, ] %*% coeff)
}, mc.cores=ncores)  # end mclapply
fcasts <- do.call(rbind, fcasts)
# fcastv <- sqrt(HighFreq::run_var(fcasts, lambda=0.4)[, 2])
# fcasts <- ifelse(fcastv > mad(fcastv)/20, fcasts/fcastv, 0)
pnls <- fcasts*respv
# Scale the PnL volatility to that of VTI
pnls <- pnls*sd(respv)/sd(pnls)

# Plot dygraph of rolling monthly YC strategy
wealthv <- cbind(respv, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
colv <- colnames(wealthv)
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Rolling Monthly Yield Curve Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Define weekly dates
endd <- rutils::calc_endpoints(respv, interval="weeks")
fcasts <- mclapply(seq_along(endd)[-1], function(tday) {
  # Define in-sample and out-of-sample intervals
  insample <- (max(1, endd[tday-1]-lookb):endd[tday-1])
  # insample <- (1:endd[tday-1]) # Expanding look-back
  outsample <- (endd[tday-1]+1):endd[tday]
  # Calculate coefficients in-sample
  invreg <- MASS::ginv(predm[insample, ])
  coeff <- drop(invreg %*% respv[insample, ])
  # Calculate forecasts out-of-sample
  fcasts <- (predm[outsample, ] %*% coeff)
}, mc.cores=ncores)  # end mclapply
fcasts <- do.call(rbind, fcasts)
# fcastv <- sqrt(HighFreq::run_var(fcasts, lambda=0.4)[, 2])
# fcasts <- ifelse(fcastv > mad(fcastv)/20, fcasts/fcastv, 0)
pnls <- fcasts*respv
# Scale the PnL volatility to that of VTI
pnls <- pnls*sd(respv)/sd(pnls)

# Plot dygraph of rolling weekly YC strategy
wealthv <- cbind(respv, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
colv <- colnames(wealthv)
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Rolling Weekly Yield Curve Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Calculate in-sample pnls for different dimax values
dimv <- 2:7
pnls <- mclapply(dimv, function(dimax) {
  invred <- HighFreq::calc_invsvd(predm, dimax=dimax)
  coeff <- drop(invred %*% respv)
  fcasts <- (predm %*% coeff)
  fcastv <- sqrt(HighFreq::run_var(fcasts, lambda=0.4)[, 2])
  fcasts <- ifelse(fcastv > mad(fcastv)/20, fcasts/fcastv, 0)
  pnls <- fcasts*respv
  pnls/sd(pnls)
}, mc.cores=ncores)  # end mclapply
pnls <- sd(respv)*do.call(cbind, pnls)
colnames(pnls) <- paste0("dim=", dimv)

# Plot dygraph of in-sample pnls
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
endw <- rutils::calc_endpoints(pnls, interval="weeks")
dygraphs::dygraph(cumsum(pnls)[endw],
  main="In-Sample YC Strategies With Dimension Reduction") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Calculate in-sample pnls for different dimax values
pnls <- mclapply(dimv, function(dimax) {
  invred <- HighFreq::calc_invsvd(predm["/2018"], dimax=dimax)
  coeff <- drop(invred %*% respv["/2018"])
  fcasts <- (predm %*% coeff)
  # fcastv <- sqrt(HighFreq::run_var(fcasts, lambda=0.4)[, 2])
  # fcasts <- ifelse(fcastv > mad(fcastv)/20, fcasts/fcastv, 0)
  pnls <- fcasts*respv
  pnls/sd(pnls)
}, mc.cores=ncores)  # end mclapply
pnls <- sd(respv)*do.call(cbind, pnls)
colnames(pnls) <- paste0("dim=", dimv)
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(pnls["2018/"], function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))

# Plot dygraph of out-of-sample pnls
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
endw <- rutils::calc_endpoints(pnls, interval="weeks")
dygraphs::dygraph(cumsum(pnls)[endw],
  main="Out-of-Sample YC Strategies With Dimension Reduction") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyEvent(end(predm["/2018"]), label="cutoff", strokePattern="solid", color="red") %>%
  dyLegend(show="always", width=300)

# Define monthly dates
endd <- rutils::calc_endpoints(respv, interval="month")
enddd <- seq_along(endd)[endd > lookb]
# Perform loop over monthly dates
lookb <- 500
dimax <- 2
fcasts <- mclapply(enddd, function(tday) {
  # Define in-sample and out-of-sample intervals
  insample <- (max(1, endd[tday-1]-lookb):endd[tday-1])
  outsample <- (endd[tday-1]+1):endd[tday]
  # Calculate coefficients in-sample
  invreg <- HighFreq::calc_invsvd(predm[insample, ], dimax=dimax)
  coeff <- drop(invreg %*% respv[insample, ])
  # Calculate forecasts out-of-sample
  fcasts <- (predm[outsample, ] %*% coeff)
}, mc.cores=ncores)  # end mclapply
fcasts <- do.call(rbind, fcasts)
fcasts <- rbind(matrix(rep(0, nrows-NROW(fcasts)), nc=1), fcasts)
# fcastv <- sqrt(HighFreq::run_var(fcasts, lambda=0.4)[, 2])
# fcasts <- ifelse(fcastv > mad(fcastv)/20, fcasts/fcastv, 0)
pnls <- fcasts*respv
# Scale the PnL volatility to that of VTI
pnls <- pnls*sd(respv)/sd(pnls)

# Plot dygraph of rolling monthly YC strategy
wealthv <- cbind(respv, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
colv <- colnames(wealthv)
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Rolling Monthly YC Strategy With Dimension Reduction") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Define weekly dates
endd <- rutils::calc_endpoints(closep, interval="weeks")
enddd <- seq_along(endd)[endd > lookb]
fcasts <- mclapply(enddd, function(tday) {
  # Define in-sample and out-of-sample intervals
  insample <- (max(1, endd[tday-1]-lookb):endd[tday-1])
  outsample <- (endd[tday-1]+1):endd[tday]
  # Calculate coefficients in-sample
  invreg <- HighFreq::calc_invsvd(predm[insample, ], dimax=dimax)
  coeff <- drop(invreg %*% respv[insample, ])
  # Calculate forecasts out-of-sample
  fcasts <- (predm[outsample, ] %*% coeff)
}, mc.cores=ncores)  # end mclapply
fcasts <- do.call(rbind, fcasts)
fcasts <- rbind(matrix(rep(0, nrows-NROW(fcasts)), nc=1), fcasts)
# fcastv <- sqrt(HighFreq::run_var(fcasts, lambda=0.4)[, 2])
# fcasts <- ifelse(fcastv > mad(fcastv)/20, fcasts/fcastv, 0)
pnls <- fcasts*respv
# Scale the PnL volatility to that of VTI
pnls <- pnls*sd(respv)/sd(pnls)

# Plot dygraph of rolling weekly YC strategy
wealthv <- cbind(respv, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
colv <- colnames(wealthv)
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Rolling Weekly YC Strategy With Dimension Reduction") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Calculate PCA of rates from correlation matrix
eigend <- eigen(cor(retr))
pcar <- (retr %*% eigend$vectors)
colnames(pcar) <- paste0("PC", 1:6)
pcar <- xts::xts(pcar, datev)
# Define predictor as the YC PCAs
predm <- rutils::lagit(pcar)
regmod <- lm(retp ~ predm)
summary(regmod)
# After 2010, the PCAs are not good predictors
regmod <- lm(retp["2010/"] ~ predm["2010/"])
summary(regmod)

# Plot YC steepener principal component with VTI
datav <- cbind(respv, pcar[, 2])
colnames(datav) <- c("VTI", "Steepener")
colv <- colnames(datav)
dygraphs::dygraph(cumsum(datav),
  main="VTI and Yield Curve Steepener") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=colv[2], axis="y2", strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)

# Define predictor without intercept term
predm <- rutils::lagit(pcar[, 1:2])
# Calculate inverse of predictor
invreg <- MASS::ginv(predm)
# Calculate coefficients from response and inverse of predictor
coeff <- drop(invreg %*% respv)
# Calculate forecasts and PnLs in-sample
fcasts <- (predm %*% coeff)
# fcastv <- sqrt(HighFreq::run_var(fcasts, lambda=0.4)[, 2])
# fcasts <- ifelse(fcastv > mad(fcastv)/20, fcasts/fcastv, 0)
pnls <- fcasts*respv
# Scale the PnL volatility to that of VTI
pnls <- pnls*sd(respv[respv<0])/sd(pnls[pnls<0])
# Calculate in-sample factors
factv <- (predm*coeff)
apply(factv, 2, sd)

# Plot dygraph of in-sample YC strategy
wealthv <- cbind(respv, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
colv <- colnames(wealthv)
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="PCA Yield Curve Strategy In-sample") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

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
sqrt(6.5*60*HighFreq::calc_var_ohlc(log(spy),
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
  highfreq=HighFreq::calc_var_ohlc(log(rutils::etfenv$VTI), method="yang_zhang"),
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

# Calculate VTI percentage returns
retp <- na.omit(rutils::etfenv$returns$VTI)
# Calculate VTI rolling variance
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
  retp=retp,
  upper=c(0.35, 0.55, varv), # Upper constraint
  lower=c(0.15, 0.35, varv/100)) # Lower constraint
# Optimal and actual parameters
cbind(actual=c(alphac=alphac, beta=betac, omega=omega),
optimal=c(fitobj$par["alpha"], fitobj$par["beta"], fitobj$par["omega"]))
# Find max likelihood parameters using DEoptim
optiml <- DEoptim::DEoptim(fn=likefun,
  upper=c(0.35, 0.55, varv), # Upper constraint
  lower=c(0.15, 0.35, varv/100), # Lower constraint
  retp=retp,
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
paramv <- unname(optiml$optim$bestmem)
alphac <- paramv[1]; betac <- paramv[2]; omega <- paramv[3]
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
quartz.save("figure/garch_forecast.png", type="png", width=6, height=5)

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

# Simulate geometric Brownian motion
sigmav <- 0.01/sqrt(48)
drift <- 0.0
nrows <- 1e4
datev <- seq(from=as.POSIXct(paste(Sys.Date()-250, "09:30:00")),
  length.out=nrows, by="30 min")
pricev <- exp(cumsum(sigmav*rnorm(nrows) + drift - sigmav^2/2))
pricev <- xts(pricev, order.by=datev)
pricev <- cbind(pricev,
  volume=sample(x=10*(2:18), size=nrows, replace=TRUE))
# Aggregate to daily OHLC data
ohlc <- xts::to.daily(pricev)
quantmod::chart_Series(ohlc, name="random prices")
# dygraphs candlestick plot using pipes syntax
library(dygraphs)
dygraphs::dygraph(ohlc[, 1:4]) %>% dyCandlestick()
# dygraphs candlestick plot without using pipes syntax
dygraphs::dyCandlestick(dygraphs::dygraph(ohlc[, 1:4]))

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
nstocks <- NCOL(retp)
# Calculate the returns of equal wealth portfolio
retew <- rowMeans(retp, na.rm=TRUE)
retew[1] <- 0

# Wealth of fixed shares portfolio
wealthfs <- rowMeans(cumprod(1 + retp))
# Wealth of equal wealth portfolio (with rebalancing)
wealthew <- cumprod(1 + retew)

# Calculate combined log wealth
wealthv <- cbind(wealthfs, wealthew)
wealthv <- log(wealthv)
wealthv <- xts::xts(wealthv, datev)
colnames(wealthv) <- c("Fixed shares", "Equal wealth")
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(rutils::diffit(wealthv), function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of combined log wealth
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(wealthv[endw],
  main="Wealth of Fixed Share and Equal Wealth Portfolios") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Select a random, fixed share sub-portfolio of 5 stocks
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
samplev <- sample.int(n=nstocks, size=5, replace=FALSE)
wealthr <- rowMeans(cumprod(1 + retp[, samplev]))

# Plot dygraph of index and random sub-portfolio
wealthv <- cbind(wealthfs, wealthr)
wealthv <- log(wealthv)
wealthv <- xts::xts(wealthv, order.by=datev)
colnames(wealthv) <- c("Index", "Random portfolio")
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(wealthv[endw], main="Stock Index and Random Portfolio") %>%
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
symbolv <- colnames(wealthv)
dygraphs::dygraph(wealthv[endw], main="Stock Index and Random Portfolios") %>%
  dyOptions(colors=colorv, strokeWidth=1) %>%
  dySeries(name=symbolv[1], strokeWidth=2) %>%
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
colnames(wealthv) <- c("Index", "Best performing")
# Calculate the in-sample Sharpe and Sortino ratios
sqrt(252)*sapply(rutils::diffit(wealthv[insample, ]),
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(rutils::diffit(wealthv[outsample, ]),
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot out-of-sample stock portfolio returns
dygraphs::dygraph(wealthv[endw], main="Out-of-Sample Log Prices of Stock Portfolio") %>%
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
symbolv <- c("LowVol", "HighVol", "LongShort")
colnames(wealthv) <- symbolv
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of cumulative returns of low and high volatility stocks
colorv <- c("blue", "red", "green")
dygraphs::dygraph(cumsum(wealthv)[endw], main="Low and High Volatility Stocks In-Sample") %>%
  dyOptions(colors=colorv, strokeWidth=1) %>%
  dySeries(name=symbolv[3], strokeWidth=2) %>%
  dyLegend(width=300)

# Merton-Henriksson test
desm <- cbind(VTI=retvti, 0.5*(retvti+abs(retvti)), retvti^2)
colnames(desm)[2:3] <- c("Merton", "Treynor")
regmod <- lm(wealthv$LongShort ~ VTI + Merton, data=desm); summary(regmod)
# Treynor-Mazuy test
regmod <- lm(wealthv$LongShort ~ VTI + Treynor, data=desm); summary(regmod)
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
symbolv <- c("LowVol", "HighVol", "LongShort")
colnames(wealthv) <- symbolv

# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of cumulative returns of low and high volatility stocks
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
colorv <- c("blue", "red", "green")
dygraphs::dygraph(cumsum(wealthv)[endw], main="Low and High Volatility Stocks Out-Of-Sample") %>%
  dyOptions(colors=colorv, strokeWidth=1) %>%
  dySeries(name=symbolv[3], strokeWidth=2) %>%
  dyLegend(width=300)

# Calculate the median idiosyncratic volatility
riskv <- riskret[, "ivol"]
medianv <- median(riskv)
# Calculate the returns of low and high idiosyncratic volatility stocks
retlow <- rowMeans(retp[, (riskv <= medianv)], na.rm=TRUE)
rethigh <- rowMeans(retp[, (riskv > medianv)], na.rm=TRUE)
wealthv <- cbind(retlow, rethigh, retlow - 0.25*rethigh)
wealthv <- xts::xts(wealthv, order.by=datev)
symbolv <- c("LowVol", "HighVol", "LongShort")
colnames(wealthv) <- symbolv

# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of returns of low and high idiosyncratic volatility stocks
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw], main="Low and High Idiosyncratic Volatility Stocks In-Sample") %>%
  dyOptions(colors=colorv, strokeWidth=1) %>%
  dySeries(name=symbolv[3], strokeWidth=2) %>%
  dyLegend(width=300)

# Merton-Henriksson test
desm <- cbind(VTI=retvti, 0.5*(retvti+abs(retvti)), retvti^2)
colnames(desm)[2:3] <- c("Merton", "Treynor")
regmod <- lm(wealthv$LongShort ~ VTI + Merton, data=desm); summary(regmod)
# Treynor-Mazuy test
regmod <- lm(wealthv$LongShort ~ VTI + Treynor, data=desm); summary(regmod)
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
symbolv <- c("LowVol", "HighVol", "LongShort")
colnames(wealthv) <- symbolv

# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of out-of-sample returns of low and high volatility stocks
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw], main="Low and High Idiosyncratic Volatility Stocks Out-Of-Sample") %>%
  dyOptions(colors=colorv, strokeWidth=1) %>%
  dySeries(name=symbolv[3], strokeWidth=2) %>%
  dyLegend(width=300)

# Calculate the median beta
riskv <- riskret[, "beta"]
medianv <- median(riskv)
# Calculate the returns of low and high beta stocks
betalow <- rowMeans(retp[, names(riskv[riskv <= medianv])], na.rm=TRUE)
betahigh <- rowMeans(retp[, names(riskv[riskv > medianv])], na.rm=TRUE)
wealthv <- cbind(betalow, betahigh, betalow - 0.25*betahigh)
wealthv <- xts::xts(wealthv, order.by=datev)
symbolv <- c("LowBeta", "HighBeta", "LongShort")
colnames(wealthv) <- symbolv

# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of cumulative returns of low and high beta stocks
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw], main="Low and High Beta Stocks In-Sample") %>%
  dyOptions(colors=colorv, strokeWidth=1) %>%
  dySeries(name=symbolv[3], strokeWidth=2) %>%
  dyLegend(width=300)

# Merton-Henriksson test
desm <- cbind(VTI=retvti, 0.5*(retvti+abs(retvti)), retvti^2)
colnames(desm)[2:3] <- c("Merton", "Treynor")
regmod <- lm(wealthv$LongShort ~ VTI + Merton, data=desm); summary(regmod)
# Treynor-Mazuy test
regmod <- lm(wealthv$LongShort ~ VTI + Treynor, data=desm); summary(regmod)
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
symbolv <- c("LowBeta", "HighBeta", "LongShort")
colnames(wealthv) <- symbolv

# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of out-of-sample returns of low and high beta stocks
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw], main="Low and High Beta Stocks Out-Of-Sample") %>%
  dyOptions(colors=colorv, strokeWidth=1) %>%
  dySeries(name=symbolv[3], strokeWidth=2) %>%
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
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Wealth of Low and High Volatility Stocks") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Calculate the long-short volatility returns
retls <- (retlow - 0.25*rethigh)
# Scale the PnL volatility to that of wealthew
retls <- retls*sd(retew)/sd(retls)
# Combined wealth
wealthv <- cbind(retew, retls)
wealthv <- xts::xts(wealthv, datev)
symbolv <- c("EqualWeight", "Long-Short Vol")
colnames(wealthv) <- symbolv
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))

# Plot of log wealth
dygraphs::dygraph(cumsum(wealthv)[endw],
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

# Calculate the risk parity allocations
pricerp <- pricestock*weightv
# Calculate the dollar returns of risk parity
retrp <- retp*rutils::lagit(pricerp)
# Calculate the wealth of risk parity
retrp <- retrp*sd(retew)/sd(retrp)
wealthrp <- cumsum(rowMeans(retrp))
# Wealth of equal wealth portfolio (with rebalancing)
wealthew <- cumsum(retew)

# Combined wealth
wealthv <- cbind(wealthew, wealthrp)
wealthv <- xts::xts(wealthv, datev)
colnames(wealthv) <- c("Equal wealth", "Risk parity")
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(rutils::diffit(wealthv), function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of log wealth
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(wealthv[endw],
  main="Wealth of Equal Wealth and Risk Parity Portfolios") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=400)
