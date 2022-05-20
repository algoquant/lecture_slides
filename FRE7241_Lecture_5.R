x11(width=6, height=5)
par(mar=c(3, 2, 1, 1), oma=c(1, 0, 0, 0))
returns <- na.omit(rutils::etfenv$returns$VTI)
# Plot autocorrelations using stats::acf()
stats::acf(returns, lag=10, xlab="lag", main="")
title(main="ACF of VTI Returns", line=-1)
# Two-tailed 95% confidence interval
qnorm(0.975)/sqrt(NROW(returns))

# Ljung-Box test for VTI returns
# 'lag' is the number of autocorrelation coefficients
Box.test(returns, lag=10, type="Ljung")
library(Ecdat)  # Load Ecdat
macro_zoo <- as.zoo(Macrodat[, c("lhur", "fygm3")])
colnames(macro_zoo) <- c("unemprate", "3mTbill")
macro_diff <- na.omit(diff(macro_zoo))
# Changes in 3 month T-bill rate are autocorrelated
Box.test(macro_diff[, "3mTbill"], lag=10, type="Ljung")
# Changes in unemployment rate are autocorrelated
Box.test(macro_diff[, "unemprate"], lag=10, type="Ljung")

# Get the ACF data returned invisibly
acf_data <- acf(returns, plot=FALSE)
summary(acf_data)
# Print the ACF data
print(acf_data)
dim(acf_data$acf)
dim(acf_data$lag)
head(acf_data$acf)

plot_acf <- function(xtes, lagg=10, plotobj=TRUE,
               xlab="Lag", ylab="", main="", ...) {
  # Calculate the ACF without a plot
  acf_data <- acf(x=xtes, lag.max=lagg, plot=FALSE, ...)
  # Remove first element of ACF data
  acf_data$acf <- array(data=acf_data$acf[-1],
    dim=c((dim(acf_data$acf)[1]-1), 1, 1))
  acf_data$lag <- array(data=acf_data$lag[-1],
    dim=c((dim(acf_data$lag)[1]-1), 1, 1))
  # Plot ACF
  if (plotobj) {
    ci <- qnorm((1+0.95)/2)/sqrt(NROW(xtes))
    ylim <- c(min(-ci, range(acf_data$acf[-1])),
        max(ci, range(acf_data$acf[-1])))
    plot(acf_data, xlab=xlab, ylab=ylab,
   ylim=ylim, main="", ci=0)
    title(main=main, line=0.5)
    abline(h=c(-ci, ci), col="blue", lty=2)
  }  # end if
  # Return the ACF data invisibly
  invisible(acf_data)
}  # end plot_acf

# Improved autocorrelation function
x11(width=6, height=5)
rutils::plot_acf(returns, lag=10, main="")
title(main="ACF of VTI returns", line=-1)
# Ljung-Box test for VTI returns
Box.test(returns, lag=10, type="Ljung")

x11(width=6, height=7)
par(mfrow=c(2,1))  # Set plot panels
par(mar=c(3, 3, 2, 2), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
# Autocorrelation of squared random returns
rutils::plot_acf(rnorm(NROW(returns))^2, lag=10, main="")
title(main="ACF of Squared Random Returns", line=-1)
# Autocorrelation of squared VTI returns
rutils::plot_acf(returns^2, lag=10, main="")
title(main="ACF of Squared VTI Returns", line=-1)
# Ljung-Box test for squared VTI returns
Box.test(returns^2, lag=10, type="Ljung")

library(rutils)  # Load package rutils
library(Ecdat)  # Load Ecdat
colnames(Macrodat)  # United States Macroeconomic Time Series
# Coerce to "zoo"
macro_zoo <- as.zoo(Macrodat[, c("lhur", "fygm3")])
colnames(macro_zoo) <- c("unemprate", "3mTbill")
# ggplot2 in multiple panes
autoplot(  # Generic ggplot2 for "zoo"
  object=macro_zoo, main="US Macro",
  facets=Series ~ .) + # end autoplot
  xlab("") +
theme(  # Modify plot theme
  legend.position=c(0.1, 0.5),
  plot.title=element_text(vjust=-2.0),
  plot.margin=unit(c(-0.5, 0.0, -0.5, 0.0), "cm"),
  plot.background=element_blank(),
  axis.text.y=element_blank()
)  # end theme

par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # Set plot panels
macro_diff <- na.omit(diff(macro_zoo))
rutils::plot_acf(coredata(macro_diff[, "unemprate"]),
  lag=10, main="quarterly unemployment rate")
rutils::plot_acf(coredata(macro_diff[, "3mTbill"]),
  lag=10, main="3 month T-bill EOQ")

# Simulate AR processes
set.seed(1121)  # Reset random numbers
dates <- Sys.Date() + 0:728  # Two year daily series
# AR time series of returns
arimav <- xts(x=arima.sim(n=NROW(dates), model=list(ar=0.2)),
          order.by=dates)
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

ar_coeff <- c(-0.9, 0.01, 0.9)  # AR coefficients
# Create three AR time series
arimav <- sapply(ar_coeff, function(phi) {
  set.seed(1121)  # Reset random numbers
  arima.sim(n=NROW(dates), model=list(ar=phi))
})  # end sapply
colnames(arimav) <- paste("autocorr", ar_coeff)
plot.zoo(arimav, main="AR(1) prices", xlab=NA)
# Or plot using ggplot
arimav <- xts(x=arimav, order.by=dates)
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
arima_faster <- filter(x=innov, filter=coeff, method="recursive")
class(arima_faster)
all.equal(arimav, as.numeric(arima_faster))
# Fast simulation of AR process using C_rfilter()
arima_fastest <- .Call(stats:::C_rfilter, innov, coeff,
                 double(NROW(coeff) + NROW(innov)))[-(1:3)]
all.equal(arimav, arima_fastest)

# Calculate modulus of roots of characteristic equation
root_s <- Mod(polyroot(c(1, -coeff)))
# Calculate warmup period
warm_up <- NROW(coeff) + ceiling(6/log(min(root_s)))
set.seed(1121)
nrows <- 1e4
innov <- rnorm(nrows + warm_up)
# Simulate AR process using arima.sim()
arimav <- arima.sim(n=nrows,
  model=list(ar=coeff),
  start.innov=innov[1:warm_up],
  innov=innov[(warm_up+1):NROW(innov)])
# Simulate AR process using filter()
arima_fast <- filter(x=innov, filter=coeff, method="recursive")
all.equal(arima_fast[-(1:warm_up)], as.numeric(arimav))
# Benchmark the speed of the three methods of simulating AR process
library(microbenchmark)
summary(microbenchmark(
  filter=filter(x=innov, filter=coeff, method="recursive"),
  arima_sim=arima.sim(n=nrows,
                  model=list(ar=coeff),
                  start.innov=innov[1:warm_up],
                  innov=innov[(warm_up+1):NROW(innov)]),
  arima_loop={for (it in 4:NROW(arimav)) {
  arimav[it] <- arimav[(it-1):(it-3)] %*% coeff + innov[it]}}
  ), times=10)[, c(1, 4, 5)]

x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0))
# Simulate AR(1) process
arimav <- arima.sim(n=1e3, model=list(ar=0.8))
# ACF of AR(1) process
acfd <- rutils::plot_acf(arimav, lag=10, xlab="", ylab="",
  main="Autocorrelations of AR(1) process")
acfd$acf[1:5]

# PACF of AR(1) process
pacfd <- pacf(arimav, lag=10, xlab="", ylab="", main="")
title("Partial autocorrelations of AR(1) process", line=1)
pacfd <- drop(pacfd$acf)
pacfd[1:5]

# Compute pacf recursively from acf
acfd <- rutils::plot_acf(arimav, lag=10, plotobj=FALSE)
acfd <- drop(acfd$acf)
pacfd <- numeric(3)
pacfd[1] <- acfd[1]
pacfd[2] <- acfd[2] - acfd[1]^2
pacfd[3] <- acfd[3] - pacfd[2]*acfd[1] - acfd[2]*pacfd[1]
# Compute pacf recursively in a loop
pacfd <- numeric(NROW(acfd))
pacfd[1] <- acfd[1]
for (it in 2:NROW(pacfd)) {
  pacfd[it] <- acfd[it] - pacfd[1:(it-1)] %*% acfd[(it-1):1]
}  # end for

par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # Set plot panels
# Simulate AR process of returns
arimav <- arima.sim(n=1e3, model=list(ar=c(0.1, 0.5, 0.1)))
# ACF of AR(3) process
rutils::plot_acf(arimav, lag=10, xlab="", ylab="",
   main="ACF of AR(3) process")
# PACF of AR(3) process
pacf(arimav, lag=10, xlab="", ylab="", main="PACF of AR(3) process")

library(rutils)  # Load rutils
library(ggplot2)  # Load ggplot2
set.seed(1121)  # Initialize random number generator
rand_walk <- cumsum(zoo(matrix(rnorm(3*100), ncol=3),
            order.by=(Sys.Date()+0:99)))
colnames(rand_walk) <- paste("rand_walk", 1:3, sep="_")
plot.zoo(rand_walk, main="Random walks",
     xlab="", ylab="", plot.type="single",
     col=c("black", "red", "blue"))
# Add legend
legend(x="topleft", legend=colnames(rand_walk),
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
rand_walks <- matrix(rnorm(1000*100), ncol=1000)
rand_walks <- apply(rand_walks, 2, cumsum)
variance <- apply(rand_walks, 1, var)
# Simulate random walks using vectorized functions
set.seed(1121)  # Initialize random number generator
rand_walks <- matrixStats::colCumsums(matrix(rnorm(1000*100), ncol=1000))
variance <- matrixStats::rowVars(rand_walks)
par(mar=c(5, 3, 2, 2), oma=c(0, 0, 0, 0))
plot(variance, xlab="time steps", ylab="",
     t="l", col="blue", lwd=2,
     main="Variance of Random Walk")

# Define Brownian Motion parameters
nrows <- 1000; sigmav <- 0.01
# Simulate 5 paths of Brownian motion
prices <- matrix(rnorm(5*nrows, sd=sigmav), nc=5)
prices <- matrixStats::colCumsums(prices)
# Open plot window on Mac
dev.new(width=6, height=4, noRStudioGD=TRUE)
# Set plot parameters to reduce whitespace around plot
par(mar=c(2, 2, 3, 1), oma=c(0, 0, 0, 0))
# Plot 5 paths of Brownian motion
matplot(y=prices, main="Brownian Motion Paths",
  xlab="", ylab="", type="l", lty="solid", lwd=1, col="blue")
# Save plot to png file on Mac
quartz.save("figure/brown_paths.png", type="png", width=6, height=4)

# Define Ornstein-Uhlenbeck parameters
init_price <- 0.0; eq_price <- 1.0;
sigmav <- 0.02; thetav <- 0.01; nrows <- 1000
# Initialize the data
innov <- rnorm(nrows)
returns <- numeric(nrows)
prices <- numeric(nrows)
returns[1] <- sigmav*innov[1]
prices[1] <- init_price
# Simulate Ornstein-Uhlenbeck process in R
for (i in 2:nrows) {
  returns[i] <- thetav*(eq_price - prices[i-1]) + sigmav*innov[i]
  prices[i] <- prices[i-1] + returns[i]
}  # end for
# Simulate Ornstein-Uhlenbeck process in Rcpp
prices_cpp <- HighFreq::sim_ou(init_price=init_price, eq_price=eq_price,
  theta=thetav, innov=matrix(innov))
all.equal(prices, drop(prices_cpp))
# Compare the speed of R code with Rcpp
library(microbenchmark)
summary(microbenchmark(
  Rcode={for (i in 2:nrows) {
    returns[i] <- thetav*(eq_price - prices[i-1]) + sigmav*innov[i]
    prices[i] <- prices[i-1] + returns[i]}},
  Rcpp=HighFreq::sim_ou(init_price=init_price, eq_price=eq_price,
    theta=thetav, innov=matrix(innov)),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

plot(prices, type="l", xlab="time", ylab="prices",
     main="Ornstein-Uhlenbeck Process")
legend("topright", title=paste(c(paste0("sigmav = ", sigmav),
               paste0("eq_price = ", eq_price),
               paste0("thetav = ", thetav)),
             collapse="\n"),
 legend="", cex=0.8, inset=0.1, bg="white", bty="n")
abline(h=eq_price, col='red', lwd=2)

returns <- rutils::diffit(prices)
lag_prices <- rutils::lagit(prices)
formulav <- returns ~ lag_prices
lmod <- lm(formulav)
summary(lmod)
# Plot regression
plot(formulav, main="OU Returns Versus Lagged Prices")
abline(lmod, lwd=2, col="red")

# Calculate volatility parameter
c(volatility=sigmav, estimate=sd(returns))
# Extract OU parameters from regression
coeff <- summary(lmod)$coefficients
# Calculate regression alpha and beta directly
betav <- cov(returns, lag_prices)/var(lag_prices)
alpha <- (mean(returns) - betav*mean(lag_prices))
cbind(direct=c(alpha=alpha, beta=betav), lm=coeff[, 1])
all.equal(c(alpha=alpha, beta=betav), coeff[, 1],
    check.attributes=FALSE)
# Calculate regression standard errors directly
betas <- c(alpha=alpha, beta=betav)
fittedv <- (alpha + betav*lag_prices)
residuals <- (returns - fittedv)
prices_squared <- sum((lag_prices - mean(lag_prices))^2)
betasd <- sqrt(sum(residuals^2)/prices_squared/(nrows-2))
alpha_sd <- sqrt(sum(residuals^2)/(nrows-2)*(1:nrows + mean(lag_prices)^2/prices_squared))
cbind(direct=c(alpha_sd=alpha_sd, betasd=betasd), lm=coeff[, 2])
all.equal(c(alpha_sd=alpha_sd, betasd=betasd), coeff[, 2],
    check.attributes=FALSE)
# Compare mean reversion parameter theta
c(theta=(-thetav), round(coeff[2, ], 3))
# Compare equilibrium price mu
c(eq_price=eq_price, estimate=-coeff[1, 1]/coeff[2, 1])
# Compare actual and estimated parameters
coeff <- cbind(c(thetav*eq_price, -thetav), coeff[, 1:2])
rownames(coeff) <- c("drift", "theta")
colnames(coeff)[1] <- "actual"
round(coeff, 4)

# Simulate Schwartz process
returns <- numeric(nrows)
prices <- numeric(nrows)
prices[1] <- exp(sigmav*innov[1])
set.seed(1121)  # Reset random numbers
for (i in 2:nrows) {
  returns[i] <- thetav*(eq_price - prices[i-1]) + sigmav*innov[i]
  prices[i] <- prices[i-1]*exp(returns[i])
}  # end for

plot(prices, type="l", xlab="time", ylab="prices",
     main="Schwartz Process")
legend("topright",
 title=paste(c(paste0("sigmav = ", sigmav),
               paste0("eq_price = ", eq_price),
               paste0("thetav = ", thetav)),
             collapse="\n"),
 legend="", cex=0.8, inset=0.12, bg="white", bty="n")
abline(h=eq_price, col='red', lwd=2)

# Define Dickey-Fuller parameters
init_price <- 0.0;  eq_price <- 1.0
thetav <- 0.01;  nrows <- 1000
coeff <- c(0.1, 0.39, 0.5)
# Initialize the data
innov <- rnorm(nrows, sd=0.01)
returns <- numeric(nrows)
prices <- numeric(nrows)
# Simulate Dickey-Fuller process using recursive loop in R
returns[1] <- innov[1]
prices[1] <- init_price
returns[2] <- thetav*(eq_price - prices[1]) + coeff[1]*returns[1] + innov[2]
prices[2] <- prices[1] + returns[2]
returns[3] <- thetav*(eq_price - prices[2]) + coeff[1]*returns[2] + coeff[2]*returns[1] + innov[3]
prices[3] <- prices[2] + returns[3]
for (it in 4:nrows) {
  returns[it] <- thetav*(eq_price - prices[it-1]) + returns[(it-1):(it-3)] %*% coeff + innov[it]
  prices[it] <- prices[it-1] + returns[it]
}  # end for
# Simulate Dickey-Fuller process in Rcpp
prices_cpp <- HighFreq::sim_df(init_price=init_price, eq_price=eq_price, theta=thetav, coeff=matrix(coeff), innov=matrix(innov))
# Compare prices
all.equal(prices, drop(prices_cpp))
# Compare the speed of R code with Rcpp
library(microbenchmark)
summary(microbenchmark(
  Rcode={for (it in 4:nrows) {
  returns[it] <- thetav*(eq_price - prices[it-1]) + returns[(it-1):(it-3)] %*% coeff + innov[it]
  prices[it] <- prices[it-1] + returns[it]
  }},
  Rcpp=HighFreq::sim_df(init_price=init_price, eq_price=eq_price, theta=thetav, coeff=matrix(coeff), innov=matrix(innov)),
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
init_price <- 0.0; eq_price <- 0.0; thetav <- 0.1
prices <- HighFreq::sim_ou(init_price=init_price, eq_price=eq_price,
  theta=thetav, innov=innov)
x11(); plot(prices, t="l", main=paste("OU coefficient =", thetav))
tseries::adf.test(prices, k=1)
# Simulate Ornstein-Uhlenbeck OU process with zero reversion
thetav <- 0.0
prices <- HighFreq::sim_ou(init_price=init_price, eq_price=eq_price,
  theta=thetav, innov=innov)
x11(); plot(prices, t="l", main=paste("OU coefficient =", thetav))
tseries::adf.test(prices, k=1)

# Simulate AR(1) process with different coefficients
coeffs <- seq(0.99, 0.999, 0.001)
returns <- as.numeric(na.omit(rutils::etfenv$returns$VTI))
adf_test <- sapply(coeffs, function(coeff) {
  arimav <- filter(x=returns, filter=coeff, method="recursive")
  ad_f <- suppressWarnings(tseries::adf.test(arimav))
  c(adfstat=unname(ad_f$statistic), pval=ad_f$p.value)
})  # end sapply
dev.new(width=6, height=4, noRStudioGD=TRUE)
# x11(width=6, height=5)
plot(x=coeffs, y=adf_test["pval", ], main="ADF p-val Versus AR Coefficient",
     xlab="AR coefficient", ylab="ADF pval", t="l", col="blue", lwd=2)
plot(x=coeffs, y=adf_test["adfstat", ], main="ADF Stat Versus AR Coefficient",
     xlab="AR coefficient", ylab="ADF stat", t="l", col="blue", lwd=2)

# Specify AR process parameters
nrows <- 1e3
coeff <- matrix(c(0.1, 0.39, 0.5)); ncoeff <- NROW(coeff)
set.seed(1121); innov <- matrix(rnorm(nrows))
# arimav <- filter(x=innov, filter=coeff, method="recursive")
# Simulate AR process using HighFreq::sim_ar()
arimav <- HighFreq::sim_ar(coeff=coeff, innov=innov)
# Fit AR model using ar.ols()
arfit <- ar.ols(arimav, order.max=ncoeff, aic=FALSE)
class(arfit)
is.list(arfit)
drop(arfit$ar); drop(coeff)
# Define predictor matrix without intercept column
predictor <- sapply(1:ncoeff, rutils::lagit, input=arimav)
# Fit AR model using regression
predinv <- MASS::ginv(predictor)
coeff <- drop(predinv %*% arimav)
all.equal(drop(arfit$ar), coeff, check.attributes=FALSE)

# Calculate the regression residuals
fittedv <- drop(predictor %*% coeff)
residuals <- drop(arimav - fittedv)
# Variance of residuals
residvar <- sum(residuals^2)/(nrows-NROW(coeff))
# Predictor matrix squared
predictor2 <- crossprod(predictor)
# Calculate covariance matrix of AR coefficients
covar <- residvar*MASS::ginv(predictor2)
coeffsd <- sqrt(diag(covar))
# Calculate t-values of AR coefficients
coefftv <- drop(coeff)/coeffsd

# Fit AR(5) model into AR(3) process
predictor <- sapply(1:5, rutils::lagit, input=arimav)
predinv <- MASS::ginv(predictor)
coeff <- drop(predinv %*% arimav)
# Calculate t-values of AR(5) coefficients
residuals <- drop(arimav - drop(predictor %*% coeff))
residvar <- sum(residuals^2)/(nrows-NROW(coeff))
covar <- residvar*MASS::ginv(crossprod(predictor))
coeffsd <- sqrt(diag(covar))
coefftv <- drop(coeff)/coeffsd
# Fit AR(5) model using arima()
arima_fit <- arima(arimav, order=c(5, 0, 0), include.mean=FALSE)
arima_fit$coef
# Fit AR(5) model using auto.arima()
library(forecast)  # Load forecast
arima_fit <- forecast::auto.arima(arimav, max.p=5, max.q=0, max.d=0)
# Fit AR(5) model into VTI returns
returns <- drop(zoo::coredata(na.omit(rutils::etfenv$returns$VTI)))
predictor <- sapply(1:5, rutils::lagit, input=returns)
predinv <- MASS::ginv(predictor)
coeff <- drop(predinv %*% returns)
# Calculate t-values of AR(5) coefficients
residuals <- drop(returns - drop(predictor %*% coeff))
residvar <- sum(residuals^2)/(nrows-NROW(coeff))
covar <- residvar*MASS::ginv(crossprod(predictor))
coeffsd <- sqrt(diag(covar))
coefftv <- drop(coeff)/coeffsd

# Compute autocorrelation coefficients
acfd <- acf(arimav, lag=10, plot=FALSE)
acfd <- drop(acfd$acf)
acf1 <- acfd[-NROW(acfd)]
# Define Yule-Walker matrix
yule_walker <- sapply(2:9, function(lagg) {
  c(acf1[lagg:1], acf1[2:(NROW(acf1)-lagg+1)])
})  # end sapply
yule_walker <- cbind(acf1, yule_walker, rev(acf1))
# Generalized inverse of Yule-Walker matrix
yule_walker_inv <- MASS::ginv(yule_walker)
# Solve Yule-Walker equations
coeff_yw <- drop(yule_walker_inv %*% acfd[-1])
round(coeff_yw, 5)
coeff

nrows <- 1e2
coeff <- c(0.1, 0.39, 0.5); ncoeff <- NROW(coeff)
set.seed(1121); innov <- rnorm(nrows, sd=0.01)
# Simulate AR process using filter()
arimav <- filter(x=innov, filter=coeff, method="recursive")
arimav <- as.numeric(arimav)
# Simulate AR process using C_rfilter()
arima_fast <- .Call(stats:::C_rfilter, innov, coeff,
  double(nrows + ncoeff))
all.equal(arimav, arima_fast[-(1:ncoeff)],
  check.attributes=FALSE)

# Forecast AR(3) process using loop in R
forecasts <- numeric(NROW(arimav)+1)
forecasts[1] <- 0
forecasts[2] <- coeff[1]*arimav[1]
forecasts[3] <- coeff[1]*arimav[2] + coeff[2]*arimav[1]
for (it in 4:NROW(forecasts)) {
  forecasts[it] <- arimav[(it-1):(it-3)] %*% coeff
}  # end for
# Plot with legend
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
plot(arimav, main="Forecasting Using AR(3) Model",
  xlab="", ylab="", type="l")
lines(forecasts, col="red", lwd=2)
legend(x="topright", legend=c("series", "forecasts"),
 col=c("black", "red"), lty=1, lwd=6,
 cex=0.9, bg="white", bty="n")

# Forecast using filter()
filter_fast <- filter(x=arimav, sides=1,
  filter=coeff, method="convolution")
filter_fast <- as.numeric(filter_fast)
# Compare excluding warmup period
all.equal(forecasts[-(1:ncoeff)], filter_fast[-(1:(ncoeff-1))],
    check.attributes=FALSE)
# Filter using C_cfilter() compiled C++ function directly
filter_fast <- .Call(stats:::C_cfilter, arimav, filter=coeff,
               sides=1, circular=FALSE)
# Compare excluding warmup period
all.equal(forecasts[-(1:ncoeff)], filter_fast[-(1:(ncoeff-1))],
    check.attributes=FALSE)
# Filter using HighFreq::roll_conv() Rcpp function
filter_fast <- HighFreq::roll_conv(matrix(arimav), matrix(coeff))
# Compare excluding warmup period
all.equal(forecasts[-(1:ncoeff)], filter_fast[-(1:(ncoeff-1))],
    check.attributes=FALSE)
# Define predictor matrix for forecasting
predictor <- sapply(0:(ncoeff-1), function(lagg) {
  rutils::lagit(arimav, lagg=lagg)
})  # end sapply
# Forecast using predictor matrix
filter_fast <- c(0, drop(predictor %*% coeff))
# Compare with loop in R
all.equal(forecasts, filter_fast, check.attributes=FALSE)

# Fit ARIMA model using arima()
arima_fit <- arima(arimav, order=c(3,0,0), include.mean=FALSE)
arima_fit$coef
coeff
# One-step-ahead forecast using predict.Arima()
predictv <- predict(arima_fit, n.ahead=1)
# Or directly call predict.Arima()
# predictv <- predict.Arima(arima_fit, n.ahead=1)
# Inspect the prediction object
class(predictv)
names(predictv)
class(predictv$pred)
unlist(predictv)
# One-step-ahead forecast using matrix algebra
forecastv <- drop(arimav[nrows:(nrows-2)] %*% arima_fit$coef)
# Compare one-step-ahead forecasts
all.equal(predictv$pred[[1]], forecastv)
# Get information about predict.Arima()
?stats:::predict.Arima

# Calculate the in-sample forecasting residuals
residuals <- (arimav - forecasts[-NROW(forecasts)])
# Compare residuals with innovations
all.equal(innov, residuals, check.attributes=FALSE)
plot(residuals, t="l", lwd=3, xlab="", ylab="",
     main="ARIMA Forecast Errors")

# Define AR process parameters
nrows <- 1e3
coeff <- c(0.5, 0.0, 0.0); ncoeff <- NROW(coeff)
set.seed(1121); innov <- rnorm(nrows, sd=0.01)
# Simulate AR process using C_rfilter()
arimav <- .Call(stats:::C_rfilter, innov, coeff,
  double(nrows + ncoeff))[-(1:ncoeff)]
# Define order of the AR(n) forecasting model
ordern <- 5
# Define predictor matrix for forecasting
predictor <- sapply(1:ordern, rutils::lagit, input=arimav)
colnames(predictor) <- paste0("pred_", 1:NCOL(predictor))
# Specify length of look-back interval
look_back <- 100
# Invert the predictor matrix
rangev <- (nrows-look_back):(nrows-1)
predinv <- MASS::ginv(predictor[rangev, ])
# Calculate fitted coefficients
coeff <- drop(predinv %*% arimav[rangev])
# Calculate forecast
drop(predictor[nrows, ] %*% coeff)

# Calculate a vector of daily VTI log returns
returns <- na.omit(rutils::etfenv$returns$VTI)
dates <- zoo::index(returns)
returns <- as.numeric(returns)
nrows <- NROW(returns)
# Define response equal to returns
response <- returns
# Define predictor as a rolling sum
nagg <- 5
predictor <- rutils::roll_sum(returns, look_back=nagg)
# Define predictor matrix for forecasting
order_max <- 5
predictor <- sapply(1+nagg*(0:order_max), rutils::lagit,
               input=predictor)
predictor <- cbind(rep(1, nrows), predictor)
# Perform rolling forecasting
look_back <- 100
forecasts <- sapply((look_back+1):nrows, function(endp) {
  # Define rolling look-back range
  startp <- max(1, endp-look_back)
  # Or expanding look-back range
  # startp <- 1
  rangev <- startp:(endp-1)
  # Invert the predictor matrix
  predinv <- MASS::ginv(predictor[rangev, ])
  # Calculate fitted coefficients
  coeff <- drop(predinv %*% response[rangev])
  # Calculate forecast
  drop(predictor[endp, ] %*% coeff)
})  # end sapply
# Add warmup period
forecasts <- c(rep(0, look_back), forecasts)

# Mean squared error
mean((returns - forecasts)^2)
# Correlation
cor(forecasts, returns)

# Plot forecasting series with legend
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0))
plot(returns[(nrows-look_back):nrows], col="blue",
     xlab="", ylab="", type="l", lwd=2,
     main="Rolling Forecasting Using AR Model")
lines(forecasts[(nrows-look_back):nrows], col="red", lwd=2)
legend(x="top", legend=c("returns", "forecasts"),
 col=c("blue", "red"), lty=1, lwd=6,
 cex=0.9, bg="white", bty="n")

# Define backtesting function
sim_forecasts <- function(response, nagg=5,
                    ordern=5, look_back=100) {
  nrows <- NROW(response)
  # Define predictor as a rolling sum
  predictor <- rutils::roll_sum(response, look_back=nagg)
  # Define predictor matrix for forecasting
  predictor <- sapply(1+nagg*(0:ordern), rutils::lagit,
                 input=predictor)
  predictor <- cbind(rep(1, nrows), predictor)
  # Perform rolling forecasting
  forecasts <- sapply((look_back+1):nrows, function(endp) {
    # Define rolling look-back range
    startp <- max(1, endp-look_back)
    # Or expanding look-back range
    # startp <- 1
    rangev <- startp:(endp-1)
    # Invert the predictor matrix
    predinv <- MASS::ginv(predictor[rangev, ])
    # Calculate fitted coefficients
    coeff <- drop(predinv %*% response[rangev])
    # Calculate forecast
    drop(predictor[endp, ] %*% coeff)
  })  # end sapply
  # Add warmup period
  forecasts <- c(rep(0, look_back), forecasts)
  # Aggregate the forecasts
  rutils::roll_sum(forecasts, look_back=nagg)
}  # end sim_forecasts
# Simulate the rolling autoregressive forecasts
forecasts <- sim_forecasts(response=returns, ordern=5, look_back=100)
c(mse=mean((returns - forecasts)^2), cor=cor(returns, forecasts))

look_backs <- seq(20, 200, 20)
forecasts <- sapply(look_backs, sim_forecasts, response=returns,
               nagg=nagg, ordern=ordern)
colnames(forecasts) <- look_backs
msev <- apply(forecasts, 2, function(x) mean((returns - x)^2))
# Plot forecasting series with legend
plot(x=look_backs, y=msev,
  xlab="look-back", ylab="MSE", type="l", lwd=2,
  main="MSE of AR(5) Forecasting Model")

# Calculate a vector of daily VTI log returns
vti <- na.omit(rutils::etfenv$returns$VTI)
dates <- zoo::index(vti)
vti <- as.numeric(vti)
nrows <- NROW(vti)
# Define predictor matrix for forecasting
order_max <- 5
predictor <- sapply(1:order_max, rutils::lagit, input=vti)
predictor <- cbind(rep(1, nrows), predictor)
colnames(predictor) <- paste0("pred_", 1:NCOL(predictor))
response <- vti
# Calculate forecasts as function of the AR order
forecasts <- lapply(2:NCOL(predictor), function(ordern) {
  # Calculate fitted coefficients
  predinv <- MASS::ginv(predictor[, 1:ordern])
  coeff <- drop(predinv %*% response)
  # Calculate in-sample forecasts of vti
  drop(predictor[, 1:ordern] %*% coeff)
})  # end lapply
names(forecasts) <- paste0("n=", 2:NCOL(predictor))

# Calculate mean squared errors
mse <- sapply(forecasts, function(x) {
  c(mse=mean((vti - x)^2), cor=cor(vti, x))
})  # end sapply
mse <- t(mse)
rownames(mse) <- names(forecasts)
# Plot forecasting MSE
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
plot(x=2:NCOL(predictor), y=mse[, 1],
  xlab="AR(n) order", ylab="MSE", type="l", lwd=2,
  main="MSE of In-sample AR(n) Forecasting Model for VTI")

insample <- 1:(nrows %/% 2)
outsample <- (nrows %/% 2 + 1):nrows
# Calculate forecasts as function of the AR order
forecasts <- lapply(2:NCOL(predictor), function(ordern) {
  # Calculate fitted coefficients
  predinv <- MASS::ginv(predictor[insample, 1:ordern])
  coeff <- drop(predinv %*% response[insample])
  # Calculate out-of-sample forecasts of vti
  drop(predictor[outsample, 1:ordern] %*% coeff)
})  # end lapply
names(forecasts) <- paste0("n=", 2:NCOL(predictor))

# Calculate mean squared errors
mse <- sapply(forecasts, function(x) {
  c(mse=mean((vti[outsample] - x)^2), cor=cor(vti[outsample], x))
})  # end sapply
mse <- t(mse)
rownames(mse) <- names(forecasts)
# Plot forecasting MSE
plot(x=2:NCOL(predictor), y=mse[, 1],
  xlab="AR(n) order", ylab="MSE", type="l", lwd=2,
  main="MSE of Out-of-sample AR(n) Forecasting Model for VTI")

# Calculate out-of-sample PnLs
pnls <- sapply(forecasts, function(x) {
  cumsum(sign(x)*vti[outsample])
})  # end sapply
colnames(pnls) <- names(forecasts)
pnls <- xts::xts(pnls, dates[outsample])

# Plot dygraph of out-of-sample PnLs
colorv <- colorRampPalette(c("red", "blue"))(NCOL(pnls[, 1:4]))
colnamev <- colnames(pnls[, 1:4])
dygraphs::dygraph(pnls[, 1:4],
  main="Autoregressive Strategies Performance With Different Order Parameters") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(width=500)

# Define predictor as a rolling mean
nagg <- 5
predictor <- roll::roll_mean(vti, width=nagg, min_obs=1)
response <- vti
# Define predictor matrix for forecasting
predictor <- sapply(1+nagg*(0:order_max), rutils::lagit,
               input=predictor)
predictor <- cbind(rep(1, nrows), predictor)
# Calculate forecasts as function of the AR order
forecasts <- lapply(2:NCOL(predictor), function(ordern) {
  predinv <- MASS::ginv(predictor[insample, 1:ordern])
  coeff <- drop(predinv %*% response[insample])
  drop(predictor[outsample, 1:ordern] %*% coeff)
})  # end lapply
names(forecasts) <- paste0("n=", 2:NCOL(predictor))

# Calculate out-of-sample PnLs
pnls <- sapply(forecasts, function(x) {
  cumsum(sign(x)*vti[outsample])
})  # end sapply
colnames(pnls) <- names(forecasts)
pnls <- xts::xts(pnls, dates[outsample])
# Plot dygraph of out-of-sample PnLs
dygraphs::dygraph(pnls[, 1:4],
  main="Autoregressive Strategies Performance Using Rolling Average Predictor") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(width=500)

# Calculate PnLs using the average of past forecasts
nagg <- 5
pnls <- sapply(forecasts, function(x) {
  x <- roll::roll_mean(x, width=nagg, min_obs=1)
  cumsum(sign(x)*vti[outsample])
})  # end sapply
colnames(pnls) <- names(forecasts)
pnls <- xts::xts(pnls, dates[outsample])

# Plot dygraph of out-of-sample PnLs
dygraphs::dygraph(pnls[, 1:4],
  main="Autoregressive Strategies Performance Using Rolling Average Forecasts") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(width=500)

# Calculate a vector of daily VTI log returns
vti <- na.omit(rutils::etfenv$returns$VTI)
dates <- zoo::index(vti)
vti <- as.numeric(vti)
nrows <- NROW(vti)
# Define response equal to vti
response <- vti
# Define predictor as a rolling sum
nagg <- 5
predictor <- rutils::roll_sum(vti, look_back=nagg)
# Define predictor matrix for forecasting
order_max <- 5
predictor <- sapply(1+nagg*(0:order_max), rutils::lagit,
               input=predictor)
predictor <- cbind(rep(1, nrows), predictor)
# Perform rolling forecasting
look_back <- 100
forecasts <- sapply((look_back+1)/nrows, function(endp) {
  # Define rolling look-back range
  startp <- max(1, endp-look_back)
  # Or expanding look-back range
  # startp <- 1
  rangev <- startp:(endp-1)
  # Invert the predictor matrix
  predinv <- MASS::ginv(predictor[rangev, ])
  # Calculate fitted coefficients
  coeff <- drop(predinv %*% response[rangev])
  # Calculate forecast
  drop(predictor[endp, ] %*% coeff)
})  # end sapply
# Add warmup period
forecasts <- c(rep(0, look_back), forecasts)

# Mean squared error
mean((vti - forecasts)^2)
# Correlation
cor(forecasts, vti)

# Plot forecasting series with legend
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0))
plot(vti[(nrows-look_back):nrows], col="blue",
     xlab="", ylab="", type="l", lwd=2,
     main="Rolling Forecasting Using AR Model")
lines(forecasts[(nrows-look_back):nrows], col="red", lwd=2)
legend(x="top", legend=c("returns", "forecasts"),
 col=c("blue", "red"), lty=1, lwd=6,
 cex=0.9, bg="white", bty="n")

# Define backtesting function
sim_forecasts <- function(response, nagg=5,
                    ordern=5, look_back=100) {
  nrows <- NROW(response)
  # Define predictor as a rolling sum
  predictor <- rutils::roll_sum(response, look_back=nagg)
  # Define predictor matrix for forecasting
  predictor <- sapply(1+nagg*(0:ordern), rutils::lagit,
                 input=predictor)
  predictor <- cbind(rep(1, nrows), predictor)
  # Perform rolling forecasting
  forecasts <- sapply((look_back+1)/nrows, function(endp) {
    # Define rolling look-back range
    startp <- max(1, endp-look_back)
    # Or expanding look-back range
    # startp <- 1
    rangev <- startp:(endp-1)
    # Invert the predictor matrix
    predinv <- MASS::ginv(predictor[rangev, ])
    # Calculate fitted coefficients
    coeff <- drop(predinv %*% response[rangev])
    # Calculate forecast
    drop(predictor[endp, ] %*% coeff)
  })  # end sapply
  # Add warmup period
  forecasts <- c(rep(0, look_back), forecasts)
  # Aggregate the forecasts
  rutils::roll_sum(forecasts, look_back=nagg)
}  # end sim_forecasts
# Simulate the rolling autoregressive forecasts
forecasts <- sim_forecasts(response=vti, ordern=5, look_back=100)
c(mse=mean((vti - forecasts)^2), cor=cor(vti, forecasts))

library(parallel)  # Load package parallel
# Calculate number of available cores
ncores <- detectCores() - 1
# Initialize compute cluster under Windows
cluster <- makeCluster(ncores)
# Perform parallel loop under Windows
look_backs <- seq(20, 600, 40)
forecasts <- parLapply(cluster, look_backs, sim_forecasts,
  response=vti, nagg=5, ordern=5)
# Perform parallel bootstrap under Mac-OSX or Linux
forecasts <- mclapply(look_backs, sim_forecasts, response=vti,
  nagg=5, ordern=5, mc.cores=ncores)

# Calculate mean squared errors
mse <- sapply(forecasts, function(x) {
  c(mse=mean((vti - x)^2), cor=cor(vti, x))
})  # end sapply
mse <- t(mse)
rownames(mse) <- look_backs
# Select optimal look_back interval
look_back <- look_backs[which.min(mse[, 1])]
# Plot forecasting MSE
plot(x=look_backs, y=mse[, 1],
  xlab="look-back", ylab="MSE", type="l", lwd=2,
  main="MSE of AR Forecasting Model As Function of Look-back")

library(parallel)  # Load package parallel
# Calculate number of available cores
ncores <- detectCores() - 1
# Initialize compute cluster under Windows
cluster <- makeCluster(ncores)
# Perform parallel loop under Windows
forecasts <- parLapply(cluster, orders, sim_forecasts, response=vti,
  nagg=5, look_back=look_back)
stopCluster(cluster)  # Stop R processes over cluster under Windows
# Perform parallel bootstrap under Mac-OSX or Linux
orders <- 2:6
forecasts <- mclapply(orders, sim_forecasts, response=vti,
  nagg=5, look_back=look_back, mc.cores=ncores)

# Calculate mean squared errors
mse <- sapply(forecasts, function(x) {
  c(mse=mean((vti - x)^2), cor=cor(vti, x))
})  # end sapply
mse <- t(mse)
rownames(mse) <- orders
# Select optimal order parameter
ordern <- orders[which.min(mse[, 1])]
# Plot forecasting MSE
plot(x=orders, y=mse[, 1],
  xlab="ordern", ylab="MSE", type="l", lwd=2,
  main="MSE of Forecasting Model As Function of AR Order")

# Simulate the rolling autoregressive forecasts
forecasts <- sim_forecasts(vti, ordern=ordern, look_back=look_back)
# Calculate strategy PnLs
pnls <- sign(forecasts)*vti
pnls <- cbind(vti, pnls, (vti+pnls)/2)
colnames(pnls) <- c("VTI", "AR_Strategy", "Combined")
cor(pnls)
# Annualized Sharpe ratios of VTI and AR strategy
pnls <- xts::xts(pnls, dates)
sqrt(252)*sapply(pnls, function (x) mean(x)/sd(x))

# Plot the cumulative strategy PnLs
dygraphs::dygraph(cumsum(pnls), main="Rolling Autoregressive Strategy") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
