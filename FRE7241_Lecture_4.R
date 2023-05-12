# Extract the VTI log OHLC prices
ohlc <- log(rutils::etfenv$VTI)
nrows <- NROW(ohlc)
closep <- quantmod::Cl(ohlc)
retp <- rutils::diffit(closep)
# Calculate the centered volatility
look_back <- 7
half_back <- look_back %/% 2
stdev <- sqrt(HighFreq::roll_var(retp, look_back))
stdev <- rutils::lagit(stdev, lagg=(-half_back))
# Calculate the z-scores of prices
pricez <- (2*closep -
  rutils::lagit(closep, half_back, pad_zeros=FALSE) -
  rutils::lagit(closep, -half_back, pad_zeros=FALSE))
pricez <- ifelse(stdev > 0, pricez/stdev, 0)
# Plot dygraph of z-scores of VTI prices
pricev <- cbind(closep, pricez)
colnames(pricev) <- c("VTI", "Z-scores")
colnamev <- colnames(pricev)
dygraphs::dygraph(pricev["2009"], main="VTI Price Z-Scores") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", strokeWidth=2, col="red")
# Calculate the thresholds for labeling tops and bottoms
confl <- c(0.2, 0.8)
threshv <- quantile(pricez, confl)
# Calculate the vectors of tops and bottoms
tops <- zoo::coredata(pricez > threshv[2])
bottoms <- zoo::coredata(pricez < threshv[1])
# Simulate in-sample VTI strategy
posv <- rep(NA_integer_, nrows)
posv[1] <- 0
posv[tops] <- (-1)
posv[bottoms] <- 1
posv <- zoo::na.locf(posv)
posv <- rutils::lagit(posv)
pnls <- retp*posv
# Plot dygraph of in-sample VTI strategy
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Price Tops and Bottoms Strategy In-Sample") %>%
  dyAxis("y", label="VTI", independentTicks=TRUE) %>%
  dyAxis("y2", label="Strategy", independentTicks=TRUE) %>%
  dySeries(name="VTI", axis="y", label="VTI", strokeWidth=2, col="blue") %>%
  dySeries(name="Strategy", axis="y2", label="Strategy", strokeWidth=2, col="red")
# Calculate the volatility z-scores
volat <- HighFreq::roll_var_ohlc(ohlc=ohlc, look_back=look_back, scale=FALSE)
volatm <- HighFreq::roll_mean(volat, look_back)
volatsd <- sqrt(HighFreq::roll_var(rutils::diffit(volat), look_back))
volatsd[1] <- 0
volatz <- ifelse(volatsd > 0, (volat - volatm)/volatsd, 0)
colnames(volatz) <- "volat"
# Calculate the volume z-scores
volum <- quantmod::Vo(ohlc)
volumean <- HighFreq::roll_mean(volum, look_back)
volumsd <- sqrt(HighFreq::roll_var(rutils::diffit(volum), look_back))
volumsd[1] <- 0
volumz <- ifelse(volumsd > 0, (volum - volumean)/volumsd, 0)
colnames(volumz) <- "volume"
# Calculate the trailing price regression z-scores
datev <- matrix(zoo::index(closep))
look_back <- 21
controlv <- HighFreq::param_reg()
regz <- HighFreq::roll_reg(respv=closep, predv=datev, look_back=look_back, controlv=controlv)
regz <- drop(regz[, NCOL(regz)])
regz[1:look_back] <- 0
# Plot dygraph of z-scores of VTI prices
pricev <- cbind(closep, regz)
colnames(pricev) <- c("VTI", "Z-scores")
colnamev <- colnames(pricev)
dygraphs::dygraph(pricev["2009"], main="VTI Price Z-Scores") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", strokeWidth=2, col="red")
lambdav <- c(0.5, 1, 1.5)
colorv <- c("red", "blue", "green")
# Plot three curves in loop
for (it in 1:3) {
  curve(expr=plogis(x, scale=lambdav[it]),
xlim=c(-4, 4), type="l", xlab="", ylab="", lwd=4,
col=colorv[it], add=(it>1))
}  # end for
# Add title
title(main="Logistic function", line=0.5)
# Add legend
legend("topleft", title="Scale parameters",
       paste("lambda", lambdav, sep="="), y.intersp=0.4,
       inset=0.05, cex=0.8, lwd=6, bty="n", lty=1, col=colorv)
# Define predictor for tops including intercept column
predm <- cbind(volatz, volumz, regz)
predm[1, ] <- 0
predm <- rutils::lagit(predm)
# Fit in-sample logistic regression for tops
logmod <- glm(tops ~ predm, family=binomial(logit))
summary(logmod)
coeff <- logmod$coefficients
fcast <- drop(cbind(rep(1, nrows), predm) %*% coeff)
ordern <- order(fcast)
# Calculate the in-sample forecasts from logistic regression model
fcast <- 1/(1+exp(-fcast))
all.equal(logmod$fitted.values, fcast, check.attributes=FALSE)
hist(fcast)
plot(x=fcast[ordern], y=tops[ordern],
     main="Logistic Regression of Stock Tops",
     col="orange", xlab="predictor", ylab="top")
lines(x=fcast[ordern], y=logmod$fitted.values[ordern], col="blue", lwd=3)
legend(x="topleft", inset=0.1, bty="n", lwd=6,
 legend=c("tops", "logit fitted values"), y.intersp=0.5,
 col=c("orange", "blue"), lty=c(NA, 1), pch=c(1, NA))
# Define discrimination threshold value
threshv <- quantile(fcast, confl[2])
# Calculate the confusion matrix in-sample
confmat <- table(actual=!tops, forecast=(fcast < threshv))
confmat
# Calculate the FALSE positive (type I error)
sum(tops & (fcast < threshv))
# Calculate the FALSE negative (type II error)
sum(!tops & (fcast > threshv))
# Calculate the FALSE positive and FALSE negative rates
confmat <- confmat / rowSums(confmat)
c(typeI=confmat[2, 1], typeII=confmat[1, 2])
# Below is an unsuccessful attempt to draw confusion matrix using xtable
confusion_matrix <- matrix(c("| true positive \\\\ (sensitivity)", "| false negative \\\\ (type II error)", "| false positive \\\\ (type I error)", "| true negative \\\\ (specificity)"), nc=2)
dimnames(confusion_matrix) <- list(forecast=c("FALSE", "TRUE"),
                             actual=c("FALSE", "TRUE"))
print(xtable::xtable(confusion_matrix,
caption="Confusion Matrix"),
caption.placement="top",
comment=FALSE, size="scriptsize",
include.rownames=TRUE,
include.colnames=TRUE)
# end unsuccessful attempt to draw confusion table using xtable
# Confusion matrix as function of threshold
confun <- function(actual, fcast, threshv) {
  forb <- (fcast < threshv)
  conf <- matrix(c(sum(!actual & !forb), sum(actual & !forb),
             sum(!actual & forb), sum(actual & forb)), ncol=2)
  conf <- conf / rowSums(conf)
  c(typeI=conf[2, 1], typeII=conf[1, 2])
}  # end confun
confun(!tops, fcast, threshv=threshv)
# Define vector of discrimination thresholds
threshv <- quantile(fcast, seq(0.01, 0.99, by=0.01))
# Calculate the error rates
error_rates <- sapply(threshv, confun,
  actual=!tops, fcast=fcast)  # end sapply
error_rates <- t(error_rates)
rownames(error_rates) <- threshv
# Calculate the informedness
informv <- 2 - rowSums(error_rates)
plot(threshv, informv, t="l", main="Informedness")
# Find the threshold corresponding to highest informedness
threshm <- threshv[which.max(informv)]
forecastops <- (fcast > threshm)
# Calculate the area under ROC curve (AUC)
error_rates <- rbind(c(1, 0), error_rates)
error_rates <- rbind(error_rates, c(0, 1))
truepos <- (1 - error_rates[, "typeII"])
truepos <- (truepos + rutils::lagit(truepos))/2
falsepos <- rutils::diffit(error_rates[, "typeI"])
abs(sum(truepos*falsepos))
# Plot ROC Curve for stock tops
plot(x=error_rates[, "typeI"], y=1-error_rates[, "typeII"],
     xlab="FALSE positive rate", ylab="TRUE positive rate",
     main="ROC Curve for Stock Tops", type="l", lwd=3, col="blue")
abline(a=0.0, b=1.0, lwd=3, col="orange")
# Fit in-sample logistic regression for bottoms
logmod <- glm(bottoms ~ predm, family=binomial(logit))
summary(logmod)
# Calculate the in-sample forecast from logistic regression model
coeff <- logmod$coefficients
fcast <- drop(cbind(rep(1, nrows), predm) %*% coeff)
fcast <- 1/(1+exp(-fcast))
# Calculate the error rates
error_rates <- sapply(threshv, confun,
  actual=!bottoms, fcast=fcast)  # end sapply
error_rates <- t(error_rates)
rownames(error_rates) <- threshv
# Calculate the informedness
informv <- 2 - rowSums(error_rates)
plot(threshv, informv, t="l", main="Informedness")
# Find the threshold corresponding to highest informedness
threshm <- threshv[which.max(informv)]
forecastbot <- (fcast > threshm)
# Calculate the area under ROC curve (AUC)
error_rates <- rbind(c(1, 0), error_rates)
error_rates <- rbind(error_rates, c(0, 1))
truepos <- (1 - error_rates[, "typeII"])
truepos <- (truepos + rutils::lagit(truepos))/2
falsepos <- rutils::diffit(error_rates[, "typeI"])
abs(sum(truepos*falsepos))
# Plot ROC Curve for stock tops
plot(x=error_rates[, "typeI"], y=1-error_rates[, "typeII"],
     xlab="FALSE positive rate", ylab="TRUE positive rate",
     main="ROC Curve for Stock Bottoms", type="l", lwd=3, col="blue")
abline(a=0.0, b=1.0, lwd=3, col="orange")
# Average the signals over time
topsav <- HighFreq::roll_sum(matrix(forecastops), 5)/5
botsav <- HighFreq::roll_sum(matrix(forecastbot), 5)/5
# Simulate in-sample VTI strategy
posv <- (botsav - topsav)
# Standard strategy
# posv <- rep(NA_integer_, NROW(retp))
# posv[1] <- 0
# posv[forecastops] <- (-1)
# posv[forecastbot] <- 1
# posv <- zoo::na.locf(posv)
posv <- rutils::lagit(posv)
pnls <- retp*posv
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
sqrt(252)*sapply(wealthv,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of in-sample VTI strategy
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Logistic Top and Bottom Strategy In-Sample") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Define in-sample and out-of-sample intervals
insample <- 1:(nrows %/% 2)
outsample <- (nrows %/% 2 + 1):nrows
# Fit in-sample logistic regression for tops
logmod <- glm(tops[insample] ~ predm[insample, ], family=binomial(logit))
fitv <- logmod$fitted.values
coefftop <- logmod$coefficients
# Calculate the error rates and best threshold value
error_rates <- sapply(threshv, confun,
  actual=!tops[insample], fcast=fitv)  # end sapply
error_rates <- t(error_rates)
informv <- 2 - rowSums(error_rates)
threshtop <- threshv[which.max(informv)]
# Fit in-sample logistic regression for bottoms
logmod <- glm(bottoms[insample] ~ predm[insample, ], family=binomial(logit))
fitv <- logmod$fitted.values
coeffbot <- logmod$coefficients
# Calculate the error rates and best threshold value
error_rates <- sapply(threshv, confun,
  actual=!bottoms[insample], fcast=fitv)  # end sapply
error_rates <- t(error_rates)
informv <- 2 - rowSums(error_rates)
threshbot <- threshv[which.max(informv)]
# Calculate the out-of-sample forecasts from logistic regression model
predictout <- cbind(rep(1, NROW(outsample)), predm[outsample, ])
fcast <- drop(predictout %*% coefftop)
fcast <- 1/(1+exp(-fcast))
forecastops <- (fcast > threshtop)
fcast <- drop(predictout %*% coeffbot)
fcast <- 1/(1+exp(-fcast))
forecastbot <- (fcast > threshbot)
# Simulate out-of-sample VTI strategy
topsav <- HighFreq::roll_sum(matrix(forecastops), 5)/5
botsav <- HighFreq::roll_sum(matrix(forecastbot), 5)/5
posv <- (botsav - topsav)
posv <- rutils::lagit(posv)
pnls <- retp[outsample, ]*posv
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retp[outsample, ], pnls)
colnames(wealthv) <- c("VTI", "Strategy")
sqrt(252)*sapply(wealthv,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of in-sample VTI strategy
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Logistic Strategy Out-of-Sample") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Simulate AR processes
set.seed(1121)  # Reset random numbers
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
  set.seed(1121)  # Reset random numbers
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
set.seed(1121); innov <- rnorm(nrows)
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
set.seed(1121)
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
acfv <- rutils::plot_acf(arimav, lag=10, xlab="", ylab="",
  main="Autocorrelations of AR(1) process")
acfv$acf[1:5]
# PACF of AR(1) process
pacfv <- pacf(arimav, lag=10, xlab="", ylab="", main="")
title("Partial autocorrelations of AR(1) process", line=1)
pacfv <- as.numeric(pacfv$acf)
pacfv[1:5]
library(rutils)  # Load rutils
library(ggplot2)  # Load ggplot2
set.seed(1121)  # Initialize random number generator
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
set.seed(1121)
nrows <- 1e4
arimav <- arima.sim(n=nrows, model=list(ar=0.99))
tseries::adf.test(arimav)
# Integrated series has unit root
tseries::adf.test(cumsum(arimav))
# Simulate arima with negative AR coefficient
set.seed(1121)
arimav <- arima.sim(n=nrows, model=list(ar=-0.99))
tseries::adf.test(arimav)
# Integrated series has unit root
tseries::adf.test(cumsum(arimav))
# Simulate random walks using apply() loops
set.seed(1121)  # Initialize random number generator
randws <- matrix(rnorm(1000*100), ncol=1000)
randws <- apply(randws, 2, cumsum)
varv <- apply(randws, 1, var)
# Simulate random walks using vectorized functions
set.seed(1121)  # Initialize random number generator
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
# Open plot window on Mac
dev.new(width=6, height=4, noRStudioGD=TRUE)
# Set plot parameters to reduce whitespace around plot
par(mar=c(2, 2, 3, 1), oma=c(0, 0, 0, 0))
# Plot 5 paths of Brownian motion
matplot(y=pricev, main="Brownian Motion Paths",
  xlab="", ylab="", type="l", lty="solid", lwd=1, col="blue")
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
pricecpp <- HighFreq::sim_ou(init_price=prici, eq_price=priceq,
  theta=thetav, innov=matrix(sigmav*innov))
all.equal(pricev, drop(pricecpp))
# Compare the speed of R code with Rcpp
library(microbenchmark)
summary(microbenchmark(
  Rcode={for (i in 2:nrows) {
    retp[i] <- thetav*(priceq - pricev[i-1]) + sigmav*innov[i]
    pricev[i] <- pricev[i-1] + retp[i]}},
  Rcpp=HighFreq::sim_ou(init_price=prici, eq_price=priceq,
    theta=thetav, innov=matrix(innov)),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
plot(pricev, type="l", xlab="time", ylab="prices",
     main="Ornstein-Uhlenbeck Process")
legend("topright", title=paste(c(paste0("sigmav = ", sigmav),
     paste0("eq_price = ", ),
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
betav <- cov(retp, pricelag)/var(pricelag)
alpha <- (mean(retp) - betav*mean(pricelag))
cbind(direct=c(alpha=alpha, beta=betav), lm=coeff[, 1])
all.equal(c(alpha=alpha, beta=betav), coeff[, 1],
    check.attributes=FALSE)
# Calculate regression standard errors directly
betav <- c(alpha=alpha, beta=betav)
fitv <- (alpha + betav*pricelag)
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
set.seed(1121)  # Reset random numbers
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
retp[2] <- thetav*(priceq - pricev[1]) + coeff[1]*retp[1] + innov[2]
pricev[2] <- pricev[1] + retp[2]
retp[3] <- thetav*(priceq - pricev[2]) + coeff[1]*retp[2] + coeff[2]*retp[1] + innov[3]
pricev[3] <- pricev[2] + retp[3]
for (it in 4:nrows) {
  retp[it] <- thetav*(priceq - pricev[it-1]) + retp[(it-1):(it-3)] %*% coeff + innov[it]
  pricev[it] <- pricev[it-1] + retp[it]
}  # end for
# Simulate Dickey-Fuller process in Rcpp
pricecpp <- HighFreq::sim_df(init_price=prici, eq_price=priceq, theta=thetav, coeff=matrix(coeff), innov=matrix(innov))
# Compare prices
all.equal(pricev, drop(pricecpp))
# Compare the speed of R code with Rcpp
library(microbenchmark)
summary(microbenchmark(
  Rcode={for (it in 4:nrows) {
  retp[it] <- thetav*(priceq - pricev[it-1]) + retp[(it-1):(it-3)] %*% coeff + innov[it]
  pricev[it] <- pricev[it-1] + retp[it]
  }},
  Rcpp=HighFreq::sim_df(init_price=prici, eq_price=priceq, theta=thetav, coeff=matrix(coeff), innov=matrix(innov)),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
set.seed(1121); innov <- matrix(rnorm(1e4, sd=0.01))
# Simulate AR(1) process with coefficient=1, with unit root
arimav <- HighFreq::sim_ar(coeff=matrix(1), innov=innov)
x11(); plot(arimav, t="l", main="AR(1) coefficient = 1.0")
# Perform ADF test with lag = 1
tseries::adf.test(arimav, k=1)
# Perform standard Dickey-Fuller test
tseries::adf.test(arimav, k=0)
# Simulate AR(1) with coefficient close to 1, without unit root
arimav <- HighFreq::sim_ar(coeff=matrix(0.99), innov=innov)
x11(); plot(arimav, t="l", main="AR(1) coefficient = 0.99")
tseries::adf.test(arimav, k=1)
# Simulate Ornstein-Uhlenbeck OU process with mean reversion
prici <- 0.0; priceq <- 0.0; thetav <- 0.1
pricev <- HighFreq::sim_ou(init_price=prici, eq_price=priceq,
  theta=thetav, innov=innov)
x11(); plot(pricev, t="l", main=paste("OU coefficient =", thetav))
tseries::adf.test(pricev, k=1)
# Simulate Ornstein-Uhlenbeck OU process with zero reversion
thetav <- 0.0
pricev <- HighFreq::sim_ou(init_price=prici, eq_price=priceq,
  theta=thetav, innov=innov)
x11(); plot(pricev, t="l", main=paste("OU coefficient =", thetav))
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
# Load daily S&P500 stock returns
load(file="/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
# Select ETF returns
retetf <- rutils::etfenv$returns[, c("VTI", "XLK", "XLF", "XLE")]
# Calculate the MSFT betas with respect to different ETFs
betas <- sapply(retetf, function(retetf) {
  retv <- na.omit(cbind(returns$MSFT, retetf))
  # Calculate the MSFT beta
  drop(cov(retv$MSFT, retv[, 2])/var(retv[, 2]))
}) # end sapply
# Combine MSFT and XLK returns
retv <- cbind(returns$MSFT, rutils::etfenv$returns$XLK)
retv <- na.omit(retv)
colnames(retv) <- c("MSFT", "XLK")
# Calculate the beta and alpha of returns MSFT ~ XLK
betav <- drop(cov(retv$MSFT, retv$XLK)/var(retv$XLK))
alphav <- retv$MSFT - betav*retv$XLK
# Scatterplot of returns
plot(MSFT ~ XLK, data=retv, main="MSFT ~ XLK Returns",
     xlab="XLK", ylab="MSFT", pch=1, col="blue")
abline(a=mean(alphav), b=betav, col="red", lwd=2)
# dygraph plot of MSFT idiosyncratic returns vs XLK
endd <- rutils::calc_endpoints(retv, interval="weeks")
datev <- zoo::index(retv)[endd]
dateb <- datev[findInterval(as.Date("2014-01-01"), datev)] # Steve Balmer exit date
datav <- cbind(retv$XLK, alphav)
colnames(datav)[2] <- "MSFT alpha"
colnamev <- colnames(datav)
dygraphs::dygraph(cumsum(datav)[endd], main="MSFT Cumulative Alpha vs XLK") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=colnamev[2], axis="y2", col="red", strokeWidth=2) %>%
  dyEvent(dateb, label="Balmer exit", strokePattern="solid", color="red") %>%
  dyLegend(show="always", width=500)
# Calculate the trailing alphas and betas
lambda <- 0.9
covars <- HighFreq::run_covar(retv, lambda)
betav <- covars[, 1]/covars[, 3]
alphav <- retv$MSFT - betav*retv$XLK
# dygraph plot of trailing MSFT idiosyncratic returns vs XLK
datav <- cbind(retv$XLK, alphav)
colnames(datav)[2] <- "MSFT alpha"
colnamev <- colnames(datav)
dygraphs::dygraph(cumsum(datav)[endd], main="MSFT Trailing Cumulative Alpha vs XLK") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=colnamev[2], axis="y2", col="red", strokeWidth=2) %>%
  dyEvent(dateb, label="Balmer exit", strokePattern="solid", color="red") %>%
  dyLegend(show="always", width=500)
# Load daily S&P500 stock prices
load(file="/Users/jerzy/Develop/lecture_slides/data/sp500_prices.RData")
# Combine MSFT and XLK prices
pricev <- cbind(rutils::etfenv$prices$XLK, prices$MSFT)
pricev <- log(na.omit(pricev))
colnames(pricev) <- c("XLK", "MSFT")
datev <- zoo::index(pricev)
# Calculate the beta regression coefficient of prices MSFT ~ XLK
betav <- drop(cov(pricev$MSFT, pricev$XLK)/var(pricev$XLK))
# Calculate the cointegrated portfolio prices
pricec <- pricev$MSFT - betav*pricev$XLK
colnames(pricec) <- "MSFT Coint XLK"
# Scatterplot of MSFT and XLK prices
plot(MSFT ~ XLK, data=pricev, main="MSFT and XLK Prices",
     xlab="XLK", ylab="MSFT", pch=1, col="blue")
abline(a=mean(pricec), b=betav, col="red", lwd=2)
# Plot time series of prices
endd <- rutils::calc_endpoints(pricev, interval="weeks")
dygraphs::dygraph(pricev[endd], main="MSFT and XLK Log Prices") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2)
# Plot histogram of the cointegrated portfolio prices
hist(pricec, breaks=100, xlab="Prices",
  main="Histogram of Cointegrated Portfolio Prices")
# Plot of cointegrated portfolio prices
datav <- cbind(pricev$XLK, pricec)[endd]
colnames(datav)[2] <- "Cointegrated Portfolio"
colnamev <- colnames(datav)
dygraphs::dygraph(datav, main="MSFT and XLK Cointegrated Portfolio Prices") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=colnamev[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Perform ADF test on the individual stocks
sapply(pricev, tseries::adf.test, k=1)
# Perform ADF test on the cointegrated portfolio
tseries::adf.test(pricec, k=1)
# Perform ADF test for vector of cointegrating factors
betas <- seq(1.2, 2.2, 0.1)
adfstat <- sapply(betas, function(betav) {
  pricec <- (pricev$MSFT - betav*pricev$XLK)
  tseries::adf.test(pricec, k=1)$statistic
})  # end sapply
# Plot ADF statistics for vector of cointegrating factors
plot(x=betas, y=adfstat, type="l", xlab="cointegrating factor", ylab="statistic",
 main="ADF Test Statistic as Function of Cointegrating Factor")
# Plot of PACF of the cointegrated portfolio returns
pricen <- zoo::coredata(pricec) # Numeric price
retd <- rutils::diffit(pricen)
pacf(retd, lag=10, xlab=NA, ylab=NA,
     main="PACF of Cointegrated Portfolio Returns")
# Dygraphs plot of cointegrated portfolio prices
endd <- rutils::calc_endpoints(pricec, interval="weeks")
dygraphs::dygraph(pricec[endd], main=
  "MSFT and XLK Cointegrated Portfolio Prices") %>%
  dyOptions(colors=c("blue"), strokeWidth=2) %>%
  dyLegend(show="always", width=200)
# Calculate the trailing mean prices and volatilities
lambda <- 0.9
meanv <- HighFreq::run_mean(pricen, lambda=lambda)
volat <- HighFreq::run_var(pricen, lambda=lambda)
volat <- sqrt(volat)
# Simulate the pairs Bollinger strategy
pricem <- pricen - meanv # De-meaned price
nrows <- NROW(pricec)
threshd <- rutils::lagit(volat)
posv <- rep(NA_integer_, nrows)
posv[1] <- 0
posv <- ifelse(pricem > threshd, -1, posv)
posv <- ifelse(pricem < -threshd, 1, posv)
posv <- zoo::na.locf(posv)
# Lag the positions to trade in the next period
posv <- rutils::lagit(posv, lagg=1)
# Calculate the pnls and the number of trades
retv <- rutils::diffit(pricev)
pnls <-  posv*(retv$MSFT - betav*retv$XLK)
ntrades <- sum(abs(rutils::diffit(posv)) > 0)
# Calculate the Sharpe ratios
wealthv <- cbind(retv$MSFT, pnls)
colnames(wealthv) <- c("MSFT", "Strategy")
sharper <- sqrt(252)*sapply(wealthv, function(x) mean(x)/sd(x[x<0]))
sharper <- round(sharper, 3)
# Dygraphs plot of pairs Bollinger strategy
colnamev <- colnames(wealthv)
captiont <- paste("Pairs Strategy", "/ \n",
  paste0(paste(colnamev[1:2], "Sharpe =", sharper), collapse=" / "), "/ \n",
  "Number of trades=", ntrades)
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd], main=captiont) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=200)
# Calculate the trailing cointegrated pair prices
covars <- HighFreq::run_covar(pricev, lambda)
betav <- covars[, 1]/covars[, 2]
pricec <- (pricev$MSFT - betav*pricev$XLK)
# Recalculate the mean of cointegrated portfolio prices
meanv <- HighFreq::run_mean(pricec, lambda=lambda)
volat <- sqrt(HighFreq::run_var(pricec, lambda=lambda))
# Simulate the pairs Bollinger strategy
pricen <- zoo::coredata(pricec) # Numeric price
pricem <- pricen - meanv # De-meaned price
threshd <- rutils::lagit(volat)
posv <- rep(NA_integer_, nrows)
posv[1] <- 0
posv <- ifelse(pricem > threshd, -1, posv)
posv <- ifelse(pricem < -threshd, 1, posv)
posv <- zoo::na.locf(posv)
posv <- rutils::lagit(posv, lagg=1)
# Calculate the pnls and the number of trades
retv <- rutils::diffit(pricev)
pnls <-  posv*(retv$MSFT - betav*retv$XLK)
ntrades <- sum(abs(rutils::diffit(posv)) > 0)
# Calculate the Sharpe ratios
wealthv <- cbind(retv$MSFT, pnls)
colnames(wealthv) <- c("MSFT", "Strategy")
sharper <- sqrt(252)*sapply(wealthv, function(x) mean(x)/sd(x[x<0]))
sharper <- round(sharper, 3)
# Dygraphs plot of pairs Bollinger strategy
colnamev <- colnames(wealthv)
captiont <- paste("Dynamic Pairs Strategy", "/ \n",
  paste0(paste(colnamev[1:2], "Sharpe =", sharper), collapse=" / "), "/ \n",
  "Number of trades=", ntrades)
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd], main=captiont) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=200)
# Calculate the trailing cointegrated pair prices
covars <- HighFreq::run_covar(pricev, lambda=0.95)
betav <- covars[, 1]/covars[, 2]
pricec <- (pricev$MSFT - betav*pricev$XLK)
# Recalculate the mean of cointegrated portfolio prices
meanv <- HighFreq::run_mean(pricec, lambda=0.3)
vars <- sqrt(HighFreq::run_var(pricec, lambda=0.3))
# Simulate the pairs Bollinger strategy
pricen <- zoo::coredata(pricec) # Numeric price
pricem <- pricen - meanv # De-meaned price
threshd <- rutils::lagit(volat)
posv <- rep(NA_integer_, nrows)
posv[1] <- 0
posv <- ifelse(pricem > threshd, -1, posv)
posv <- ifelse(pricem < -threshd, 1, posv)
posv <- zoo::na.locf(posv)
posv <- rutils::lagit(posv, lagg=1)
# Calculate the pnls and the number of trades
retv <- rutils::diffit(pricev)
pnls <-  posv*(retv$MSFT - betav*retv$XLK)
ntrades <- sum(abs(rutils::diffit(posv)) > 0)
# Calculate the Sharpe ratios
wealthv <- cbind(retv$MSFT, pnls)
colnames(wealthv) <- c("MSFT", "Strategy")
sharper <- sqrt(252)*sapply(wealthv, function(x) mean(x)/sd(x[x<0]))
sharper <- round(sharper, 3)
# Dygraphs plot of pairs Bollinger strategy
colnamev <- colnames(wealthv)
captiont <- paste("Dynamic Pairs Slow Beta", "/ \n",
  paste0(paste(colnamev[1:2], "Sharpe =", sharper), collapse=" / "), "/ \n",
  "Number of trades=", ntrades)
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd], main=captiont) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=200)
# Load daily S&P500 log percentage stock returns
load(file="/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
# Subset (select) the stock returns after the start date of VTI
retvti <- na.omit(rutils::etfenv$returns$VTI)
colnames(retvti) <- "VTI"
retv <- returns[zoo::index(retvti)]
datev <- zoo::index(retv)
retvti <- retvti[datev]
nrows <- NROW(retv)
nstocks <- NCOL(retv)
head(retv[, 1:5])
# Replace NA returns with zeros
retsna <- retv
retsna[is.na(retsna)] <- 0
# Calculate normalized log prices that start at 0
pricev <- cumsum(retsna)
head(pricev[, 1:5])
# Wealth of price-weighted (fixed shares) portfolio
wealthpw <- rowMeans(exp(pricev))
# Wealth of equal-weighted portfolio
wealthew <- exp(rowMeans(pricev))
# Calculate combined log wealth
wealthv <- cbind(wealthpw, wealthew)
wealthv <- log(wealthv)
wealthv <- xts::xts(wealthv, datev)
colnames(wealthv) <- c("Price-weighted", "Equal-weighted")
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(rutils::diffit(wealthv),
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of combined log wealth
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(wealthv[endd],
  main="Wealth of Price-weighted and Equal-weighted Portfolios") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Calculate the price-weighted average of all stock prices
wealthpw <- xts::xts(wealthpw, order.by=datev)
colnames(wealthpw) <- "Index"
# Select a random, price-weighted portfolio of 5 stocks
set.seed(1121)
samplev <- sample.int(n=nstocks, size=5, replace=FALSE)
portf <- pricev[, samplev]
portf <- rowMeans(exp(portf))
# Plot dygraph of stock index and random portfolio
wealthv <- cbind(wealthpw, portf)
wealthv <- log(wealthv)
colnames(wealthv)[2] <- "Random"
colorv <- c("blue", "red")
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(wealthv[endd], main="Stock Index and Random Portfolio") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Select 10 random price-weighted sub-portfolios
set.seed(1121)
nportf <- 10
portfs <- sapply(1:nportf, function(x) {
  portf <- pricev[, sample.int(n=nstocks, size=5, replace=FALSE)]
  rowMeans(exp(portf))
})  # end sapply
portfs <- xts::xts(portfs, order.by=datev)
colnames(portfs) <- paste0("portf", 1:nportf)
# Sort the sub-portfolios according to perfomance
portfs <- portfs[, order(portfs[nrows])]
round(head(portfs), 3)
round(tail(portfs), 3)
# Plot dygraph of stock index and random portfolios
colorv <- colorRampPalette(c("red", "blue"))(nportf)
wealthv <- cbind(wealthpw, portfs)
wealthv <- log(wealthv)
colnames(wealthv)[1] <- "Index"
colnamev <- colnames(wealthv)
colorv <- c("green", colorv)
dygraphs::dygraph(wealthv[endd], main="Stock Index and Random Portfolios") %>%
  dyOptions(colors=colorv, strokeWidth=1) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=3, col="green") %>%
  dyLegend(show="always", width=500)
# Define in-sample and out-of-sample intervals
cutoff <- nrows %/% 2
datev[cutoff]
insample <- 1:cutoff
outsample <- (cutoff + 1):nrows
# Calculate the 10 best performing stocks in-sample
perfstat <- sort(drop(coredata(pricev[cutoff, ])), decreasing=TRUE)
symbolv <- names(head(perfstat, 10))
# Calculate the wealth of the 10 best performing stocks
wealthv <- pricev[, symbolv]
wealthv <- rowMeans(exp(wealthv))
# Combine the price-weighted wealth with the 10 best performing stocks
wealthv <- cbind(wealthpw, wealthv)
wealthv <- log(wealthv)
colnames(wealthv)[2] <- "Portfolio"
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(rutils::diffit(wealthv[outsample, ]),
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot out-of-sample stock portfolio returns
dygraphs::dygraph(wealthv[endd], main="Out-of-sample Log Prices of Stock Portfolio") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyEvent(datev[cutoff], label="in-sample", strokePattern="solid", color="green") %>%
  dyLegend(width=500)
# Calculate the stock volatilities, betas, and alphas
riskret <- sapply(retv, function(retv) {
  retv <- na.omit(retv)
  std <- sd(retv)
  retvti <- retvti[zoo::index(retv)]
  varvti <- drop(var(retvti))
  meanvti <- mean(retvti)
  betav <- drop(cov(retv, retvti))/varvti
  resid <- retv - betav*retvti
  alphav <- mean(retv) - betav*meanvti
  c(alpha=alphav, beta=betav, std=std, ivol=sd(resid))
})  # end sapply
riskret <- t(riskret)
tail(riskret)
# Calculate the median volatility
riskv <- riskret[, "std"]
medianv <- median(riskv)
# Calculate the returns of low and high volatility stocks
retlow <- rowMeans(retsna[, names(riskv[riskv<=medianv])])
rethigh <- rowMeans(retsna[, names(riskv[riskv>medianv])])
wealthv <- cbind(retlow, rethigh, retlow - 0.5*rethigh)
wealthv <- xts::xts(wealthv, order.by=datev)
colnamev <- c("low_vol", "high_vol", "long_short")
colnames(wealthv) <- colnamev
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of cumulative returns of low and high volatility stocks
dygraphs::dygraph(cumsum(wealthv)[endd], main="Low and High Volatility Stocks In-Sample") %>%
  dySeries(name=colnamev[1], col="blue", strokeWidth=1) %>%
  dySeries(name=colnamev[2], col="red", strokeWidth=1) %>%
  dySeries(name=colnamev[3], col="green", strokeWidth=2) %>%
  dyLegend(width=500)
# Merton-Henriksson test
predm <- cbind(VTI=retvti, 0.5*(retvti+abs(retvti)), retvti^2)
colnames(predm)[2:3] <- c("merton", "treynor")
regmod <- lm(wealthv$long_short ~ VTI + merton, data=predm); summary(regmod)
# Treynor-Mazuy test
regmod <- lm(wealthv$long_short ~ VTI + treynor, data=predm); summary(regmod)
# Plot residual scatterplot
resids <- regmod$residuals
plot.default(x=retvti, y=resids, xlab="VTI", ylab="Low Volatility")
title(main="Treynor-Mazuy Market Timing Test\n for Low Volatility vs VTI", line=0.5)
# Plot fitted (predicted) response values
coefreg <- summary(regmod)$coeff
fitv <- regmod$fitted.values - coefreg["VTI", "Estimate"]*retvti
tvalue <- round(coefreg["treynor", "t value"], 2)
points.default(x=retvti, y=fitv, pch=16, col="red")
text(x=0.0, y=max(resids), paste("Treynor test t-value =", tvalue))
# Calculate the in-sample stock volatilities, betas, and alphas
riskretis <- sapply(retv[insample], function(retv) {
  combv <- na.omit(cbind(retv, retvti))
  if (NROW(combv) > 0) {
    retv <- na.omit(retv)
    std <- sd(retv)
    retvti <- retvti[zoo::index(retv)]
    varvti <- drop(var(retvti))
    meanvti <- mean(retvti)
    betav <- drop(cov(retv, retvti))/varvti
    resid <- retv - betav*retvti
    alphav <- mean(retv) - betav*meanvti
    return(c(alpha=alphav, beta=betav, std=std, ivol=sd(resid)))
  } else {
    return(c(alpha=0, beta=0, std=0, ivol=0))
  }  # end if
})  # end sapply
riskretis <- t(riskretis)
tail(riskretis)
# Calculate the median volatility
riskv <- riskretis[, "std"]
riskv[which(is.na(riskv))] <- 0
medianv <- median(riskv)
# Calculate the out-of-sample returns of low and high volatility stocks
retlow <- rowMeans(retsna[outsample, names(riskv[riskv<=medianv])])
rethigh <- rowMeans(retsna[outsample, names(riskv[riskv>medianv])])
wealthv <- cbind(retlow, rethigh, retlow - 0.5*rethigh)
wealthv <- xts::xts(wealthv, order.by=datev[outsample])
colnamev <- c("low_vol", "high_vol", "long_short")
colnames(wealthv) <- colnamev
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of cumulative returns of low and high volatility stocks
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd], main="Low and High Volatility Stocks Out-Of-Sample") %>%
  dySeries(name=colnamev[1], col="blue", strokeWidth=1) %>%
  dySeries(name=colnamev[2], col="red", strokeWidth=1) %>%
  dySeries(name=colnamev[3], col="green", strokeWidth=2) %>%
  dyLegend(width=500)
# Calculate the median idiosyncratic volatility
riskv <- riskret[, "ivol"]
medianv <- median(riskv)
# Calculate the returns of low and high idiosyncratic volatility stocks
retlow <- rowMeans(retsna[, names(riskv[riskv<=medianv])])
rethigh <- rowMeans(retsna[, names(riskv[riskv>medianv])])
wealthv <- cbind(retlow, rethigh, retlow - 0.5*rethigh)
wealthv <- xts::xts(wealthv, order.by=datev)
colnamev <- c("low_vol", "high_vol", "long_short")
colnames(wealthv) <- colnamev
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of returns of low and high idiosyncratic volatility stocks
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd], main="Low and High Idiosyncratic Volatility Stocks In-Sample") %>%
  dySeries(name=colnamev[1], col="blue", strokeWidth=1) %>%
  dySeries(name=colnamev[2], col="red", strokeWidth=1) %>%
  dySeries(name=colnamev[3], col="green", strokeWidth=2) %>%
  dyLegend(width=500)
# Merton-Henriksson test
predm <- cbind(VTI=retvti, 0.5*(retvti+abs(retvti)), retvti^2)
colnames(predm)[2:3] <- c("merton", "treynor")
regmod <- lm(wealthv$long_short ~ VTI + merton, data=predm); summary(regmod)
# Treynor-Mazuy test
regmod <- lm(wealthv$long_short ~ VTI + treynor, data=predm); summary(regmod)
# Plot residual scatterplot
resids <- regmod$residuals
plot.default(x=retvti, y=resids, xlab="VTI", ylab="Low Volatility")
title(main="Treynor-Mazuy Market Timing Test\n for Low Idiosyncratic Volatility vs VTI", line=0.5)
# Plot fitted (predicted) response values
coefreg <- summary(regmod)$coeff
fitv <- regmod$fitted.values - coefreg["VTI", "Estimate"]*retvti
tvalue <- round(coefreg["treynor", "t value"], 2)
points.default(x=retvti, y=fitv, pch=16, col="red")
text(x=0.0, y=max(resids), paste("Treynor test t-value =", tvalue))
# Calculate the median in-sample idiosyncratic volatility
riskv <- riskretis[, "ivol"]
medianv <- median(riskv)
# Calculate the out-of-sample returns of low and high idiosyncratic volatility stocks
retlow <- rowMeans(retsna[outsample, names(riskv[riskv<=medianv])])
rethigh <- rowMeans(retsna[outsample, names(riskv[riskv>medianv])])
wealthv <- cbind(retlow, rethigh, retlow - 0.5*rethigh)
wealthv <- xts::xts(wealthv, order.by=datev[outsample])
colnamev <- c("low_vol", "high_vol", "long_short")
colnames(wealthv) <- colnamev
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of out-of-sample returns of low and high volatility stocks
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd], main="Low and High Idiosyncratic Volatility Stocks Out-Of-Sample") %>%
  dySeries(name=colnamev[1], col="blue", strokeWidth=1) %>%
  dySeries(name=colnamev[2], col="red", strokeWidth=1) %>%
  dySeries(name=colnamev[3], col="green", strokeWidth=2) %>%
  dyLegend(width=500)
# Calculate the median beta
riskv <- riskret[, "beta"]
medianv <- median(riskv)
# Calculate the returns of low and high beta stocks
betalow <- rowMeans(retsna[, names(riskv[riskv<=medianv])])
betahigh <- rowMeans(retsna[, names(riskv[riskv>medianv])])
wealthv <- cbind(betalow, betahigh, betalow - 0.5*betahigh)
wealthv <- xts::xts(wealthv, order.by=datev)
colnamev <- c("low_beta", "high_beta", "long_short")
colnames(wealthv) <- colnamev
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of cumulative returns of low and high beta stocks
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd], main="Low and High Beta Stocks In-Sample") %>%
  dySeries(name=colnamev[1], col="blue", strokeWidth=1) %>%
  dySeries(name=colnamev[2], col="red", strokeWidth=1) %>%
  dySeries(name=colnamev[3], col="green", strokeWidth=2) %>%
  dyLegend(width=500)
# Merton-Henriksson test
predm <- cbind(VTI=retvti, 0.5*(retvti+abs(retvti)), retvti^2)
colnames(predm)[2:3] <- c("merton", "treynor")
regmod <- lm(wealthv$long_short ~ VTI + merton, data=predm); summary(regmod)
# Treynor-Mazuy test
regmod <- lm(wealthv$long_short ~ VTI + treynor, data=predm); summary(regmod)
# Plot residual scatterplot
resids <- regmod$residuals
plot.default(x=retvti, y=resids, xlab="VTI", ylab="Low Beta")
title(main="Treynor-Mazuy Market Timing Test\n for Low Beta vs VTI", line=0.5)
# Plot fitted (predicted) response values
coefreg <- summary(regmod)$coeff
fitv <- regmod$fitted.values - coefreg["VTI", "Estimate"]*retvti
tvalue <- round(coefreg["treynor", "t value"], 2)
points.default(x=retvti, y=fitv, pch=16, col="red")
text(x=0.0, y=max(resids), paste("Treynor test t-value =", tvalue))
# Calculate the median beta
riskv <- riskretis[, "beta"]
medianv <- median(riskv)
# Calculate the out-of-sample returns of low and high beta stocks
betalow <- rowMeans(retsna[outsample, names(riskv[riskv<=medianv])])
betahigh <- rowMeans(retsna[outsample, names(riskv[riskv>medianv])])
wealthv <- cbind(betalow, betahigh, betalow - 0.5*betahigh)
wealthv <- xts::xts(wealthv, order.by=datev[outsample])
colnamev <- c("low_beta", "high_beta", "long_short")
colnames(wealthv) <- colnamev
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of out-of-sample returns of low and high beta stocks
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd], main="Low and High Beta Stocks Out-Of-Sample") %>%
  dySeries(name=colnamev[1], col="blue", strokeWidth=1) %>%
  dySeries(name=colnamev[2], col="red", strokeWidth=1) %>%
  dySeries(name=colnamev[3], col="green", strokeWidth=2) %>%
  dyLegend(width=500)
# Calculate the trailing percentage volatilities
lambda <- 0.6
volat <- HighFreq::run_var(retsna, lambda=lambda)
volat <- sqrt(volat)
# Calculate the risk parity portfolio allocations
alloc <- ifelse(volat > 1e-4, 1/volat, 0)
# Scale allocations to 1 dollar total
allocs <- rowSums(alloc)
alloc <- ifelse(allocs > 0, alloc/allocs, 0)
# Lag the allocations
alloc <- rutils::lagit(alloc)
# Calculate the wealth of risk parity
wealthrp <- exp(cumsum(rowSums(alloc*retv, na.rm=TRUE)))
# Combined wealth
wealthv <- cbind(wealthpw, wealthrp)
wealthv <- xts::xts(wealthv, datev)
colnames(wealthv) <- c("Price-weighted", "Risk parity")
wealthv <- log(wealthv)
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(rutils::diffit(wealthv),
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of log wealth
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(wealthv[endd],
  main="Wealth of Price-weighted and Risk Parity Portfolios") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Calculate the median volatilities
medianv <- matrixStats::rowMedians(volat)
# Calculate the wealth of low volatility stocks
alloc <- matrix(integer(nrows*nstocks), ncol=nstocks)
alloc[volat <= medianv] <- 1
alloc <- rutils::lagit(alloc)
retlow <- rowSums(alloc*retv, na.rm=TRUE)
wealth_lovol <- exp(cumsum(retlow))
# Calculate the wealth of high volatility stocks
alloc <- matrix(integer(nrows*nstocks), ncol=nstocks)
alloc[volat > medianv] <- 1
alloc <- rutils::lagit(alloc)
rethigh <- rowSums(alloc*retv, na.rm=TRUE)
wealth_hivol <- exp(cumsum(rethigh))
# Combined wealth
wealthv <- cbind(wealth_lovol, wealth_hivol)
wealthv <- xts::xts(wealthv, datev)
colnames(wealthv) <- c("Low Volatility", "High Volatility")
wealthv <- log(wealthv)
# Calculate the Sharpe and Sortino ratios
retvol <- rutils::diffit(wealthv)
sqrt(252)*sapply(retvol, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of log wealth
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(wealthv[endd],
  main="Wealth of Low and High Volatility Stocks") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Calculate the volatilities of the low and high volatility stocks
volat <- HighFreq::run_var(retvol, lambda=lambda)
volat <- sqrt(volat)
volat[1:2, ] <- 1
colnames(volat) <- c("Low Volatility", "High Volatility")
# Multiply the high volatility portfolio returns by a factor
factv <- volat[, 1]/volat[, 2]
factv <- rutils::lagit(factv)
# Calculate the long-short volatility returns
retls <- (retlow - factv*rethigh)
wealthls <- exp(cumsum(retls))
# Combined wealth
wealthv <- cbind(wealthpw, wealthls)
wealthv <- xts::xts(wealthv, datev)
colnamev <- c("Price-weighted", "Long-Short Vol")
colnames(wealthv) <- colnamev
wealthv <- log(wealthv)
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(rutils::diffit(wealthv),
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of log wealth
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(wealthv[endd],
  main="Wealth of Price-weighted and Long-Short Vol Portfolios") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=500)
