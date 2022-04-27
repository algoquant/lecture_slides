# Specify AR process parameters
nrows <- 1e3
coeff <- matrix(c(0.1, 0.39, 0.5)); n_coeff <- NROW(coeff)
set.seed(1121); innov <- matrix(rnorm(nrows))
# arimav <- filter(x=innov, filter=coeff, method="recursive")
# Simulate AR process using HighFreq::sim_ar()
arimav <- HighFreq::sim_ar(coeff=coeff, innov=innov)
# Fit AR model using ar.ols()
arfit <- ar.ols(arimav, order.max=n_coeff, aic=FALSE)
class(arfit)
is.list(arfit)
drop(arfit$ar); drop(coeff)
# Define design matrix without intercept column
design <- sapply(1:n_coeff, rutils::lagit, input=arimav)
# Fit AR model using regression
design_inv <- MASS::ginv(design)
coeff_fit <- drop(design_inv %*% arimav)
all.equal(drop(arfit$ar), coeff_fit, check.attributes=FALSE)
# Calculate the regression residuals
fit_ted <- drop(design %*% coeff_fit)
residuals <- drop(arimav - fit_ted)
# Variance of residuals
var_resid <- sum(residuals^2)/(nrows-NROW(coeff_fit))
# Design matrix squared
design2 <- crossprod(design)
# Calculate covariance matrix of AR coefficients
covar <- var_resid*MASS::ginv(design2)
coeff_fitd <- sqrt(diag(covar))
# Calculate t-values of AR coefficients
coeff_tvals <- drop(coeff_fit)/coeff_fitd
# Fit AR(5) model into AR(3) process
design <- sapply(1:5, rutils::lagit, input=arimav)
design_inv <- MASS::ginv(design)
coeff_fit <- drop(design_inv %*% arimav)
# Calculate t-values of AR(5) coefficients
residuals <- drop(arimav - drop(design %*% coeff_fit))
var_resid <- sum(residuals^2)/(nrows-NROW(coeff_fit))
covar <- var_resid*MASS::ginv(crossprod(design))
coeff_fitd <- sqrt(diag(covar))
coeff_tvals <- drop(coeff_fit)/coeff_fitd
# Fit AR(5) model using arima()
arima_fit <- arima(arimav, order=c(5, 0, 0), include.mean=FALSE)
arima_fit$coef
# Fit AR(5) model using auto.arima()
library(forecast)  # Load forecast
arima_fit <- forecast::auto.arima(arimav, max.p=5, max.q=0, max.d=0)
# Fit AR(5) model into VTI returns
returns <- drop(zoo::coredata(na.omit(rutils::etfenv$returns$VTI)))
design <- sapply(1:5, rutils::lagit, input=returns)
design_inv <- MASS::ginv(design)
coeff_fit <- drop(design_inv %*% returns)
# Calculate t-values of AR(5) coefficients
residuals <- drop(returns - drop(design %*% coeff_fit))
var_resid <- sum(residuals^2)/(nrows-NROW(coeff_fit))
covar <- var_resid*MASS::ginv(crossprod(design))
coeff_fitd <- sqrt(diag(covar))
coeff_tvals <- drop(coeff_fit)/coeff_fitd
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
coeff_fit
nrows <- 1e2
coeff <- c(0.1, 0.39, 0.5); n_coeff <- NROW(coeff)
set.seed(1121); innov <- rnorm(nrows)
# Simulate AR process using filter()
arimav <- filter(x=innov, filter=coeff, method="recursive")
arimav <- as.numeric(arimav)
# Simulate AR process using C_rfilter()
arima_fast <- .Call(stats:::C_rfilter, innov, coeff,
  double(nrows + n_coeff))
all.equal(arimav, arima_fast[-(1:n_coeff)],
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
lines(forecasts, col="orange", lwd=3)
legend(x="topright", legend=c("series", "forecasts"),
 col=c("black", "orange"), lty=1, lwd=6,
 cex=0.9, bg="white", bty="n")
# Forecast using filter()
filter_fast <- filter(x=arimav, sides=1,
  filter=coeff, method="convolution")
filter_fast <- as.numeric(filter_fast)
# Compare excluding warmup period
all.equal(forecasts[-(1:n_coeff)], filter_fast[-(1:(n_coeff-1))],
    check.attributes=FALSE)
# Filter using C_cfilter() compiled C++ function directly
filter_fast <- .Call(stats:::C_cfilter, arimav, filter=coeff,
               sides=1, circular=FALSE)
# Compare excluding warmup period
all.equal(forecasts[-(1:n_coeff)], filter_fast[-(1:(n_coeff-1))],
    check.attributes=FALSE)
# Filter using HighFreq::roll_conv() Rcpp function
filter_fast <- HighFreq::roll_conv(matrix(arimav), matrix(coeff))
# Compare excluding warmup period
all.equal(forecasts[-(1:n_coeff)], filter_fast[-(1:(n_coeff-1))],
    check.attributes=FALSE)
# Define predictor matrix for forecasting
predictor <- sapply(0:(n_coeff-1), function(lagg) {
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
coeff <- c(0.5, 0.0, 0.0); n_coeff <- NROW(coeff)
set.seed(1121); innov <- rnorm(nrows)
# Simulate AR process using C_rfilter()
arimav <- .Call(stats:::C_rfilter, innov, coeff,
  double(nrows + n_coeff))[-(1:n_coeff)]
# Define order of the AR(n) forecasting model
ordern <- 5
# Define predictor matrix for forecasting
design <- sapply(1:ordern, rutils::lagit, input=arimav)
colnames(design) <- paste0("pred_", 1:NCOL(design))
# Add response equal to series
design <- cbind(arimav, design)
colnames(design)[1] <- "response"
# Specify length of look-back interval
look_back <- 100
# Invert the predictor matrix
rangev <- (nrows-look_back):(nrows-1)
design_inv <- MASS::ginv(design[rangev, -1])
# Calculate fitted coefficients
coeff_fit <- drop(design_inv %*% design[rangev, 1])
# Calculate forecast
drop(design[nrows, -1] %*% coeff_fit)
# Calculate a vector of daily VTI log returns
returns <- na.omit(rutils::etfenv$returns$VTI)
dates <- zoo::index(returns)
returns <- as.numeric(returns)
nrows <- NROW(returns)
# Define predictor as a rolling sum
nagg <- 5
predictor <- rutils::roll_sum(returns, look_back=nagg)
# Shift the response forward out-of-sample
response <- rutils::lagit(predictor, lagg=(-nagg))
# Define predictor matrix for forecasting
order_max <- 5
predictor <- sapply(1+nagg*(0:order_max), rutils::lagit,
               input=predictor)
predictor <- cbind(rep(1, nrows), predictor)
# Define design matrix
design <- cbind(response, predictor)
# Perform rolling forecasting
look_back <- 100
forecasts <- sapply((look_back+1):nrows, function(endp) {
  # Define rolling look-back range
  startp <- max(1, endp-look_back)
  # Or expanding look-back range
  # startp <- 1
  rangev <- startp:(endp-1)
  # Invert the predictor matrix
  design_inv <- MASS::ginv(design[rangev, -1])
  # Calculate fitted coefficients
  coeff_fit <- drop(design_inv %*% design[rangev, 1])
  # Calculate forecast
  drop(design[endp, -1] %*% coeff_fit)
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
sim_forecasts <- function(response, predictor=response, nagg=5,
                ordern=5, lookback=100) {
  nrows <- NROW(response)
  # Define predictor as a rolling sum
  predictor <- rutils::roll_sum(response, look_back=nagg)
  # Shift the response forward out-of-sample
  response <- rutils::lagit(predictor, lagg=(-nagg))
  # Define predictor matrix for forecasting
  predictor <- sapply(1+nagg*(0:ordern), rutils::lagit,
                 input=predictor)
  predictor <- cbind(rep(1, nrows), predictor)
  # Define design matrix
  design <- cbind(response, predictor)
  # Perform rolling forecasting
  forecasts <- sapply((lookback+1):nrows, function(endp) {
    # Define rolling look-back range
    startp <- max(1, endp-lookback)
    # Or expanding look-back range
    # startp <- 1
    rangev <- startp:(endp-1)
    # Invert the predictor matrix
    design_inv <- MASS::ginv(design[rangev, -1])
    # Calculate fitted coefficients
    coeff_fit <- drop(design_inv %*% design[rangev, 1])
    # Calculate forecast
    drop(design[endp, -1] %*% coeff_fit)
  })  # end sapply
  # Add warmup period
  forecasts <- c(rep(0, lookback), forecasts)
  rutils::roll_sum(forecasts, look_back=nagg)
}  # end sim_forecasts
# Simulate the rolling autoregressive forecasts
forecasts <- sim_forecasts(returns, ordern=5, look_back=100)
c(mse=mean((returns - forecasts)^2), cor=cor(returns, forecasts))
look_backs <- seq(20, 200, 20)
back_tests <- sapply(look_backs, sim_forecasts, response=response, predictor=response, nagg=5, ordern=ordern)
colnames(back_tests) <- look_backs
mse_s <- apply(back_tests, 2, function(x) mean((returns - x)^2))
mean((back_tests[, 1] - returns)^2)
# Plot forecasting series with legend
plot(x=look_backs, y=mse_s,
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
  inverse <- MASS::ginv(predictor[, 1:ordern])
  coeff <- drop(inverse %*% response)
  # Calculate in-sample forecasts of vti
  drop(predictor[, 1:ordern] %*% coeff)
})  # end lapply
names(forecasts) <- paste0("p=", 2:NCOL(predictor))
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
  inverse <- MASS::ginv(predictor[insample, 1:ordern])
  coeff <- drop(inverse %*% response[insample])
  # Calculate out-of-sample forecasts of vti
  drop(predictor[outsample, 1:ordern] %*% coeff)
})  # end lapply
names(forecasts) <- paste0("p=", 2:NCOL(predictor))
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
colnames <- colnames(pnls[, 1:4])
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
  inverse <- MASS::ginv(predictor[insample, 1:ordern])
  coeff <- drop(inverse %*% response[insample])
  drop(predictor[outsample, 1:ordern] %*% coeff)
})  # end lapply
names(forecasts) <- paste0("p=", 2:NCOL(predictor))
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
# Calculate out-of-sample PnLs
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
# Define predictor as a rolling mean
nagg <- 5
predictor <- roll::roll_mean(vti, width=nagg, min_obs=1)
# Shift the response forward out-of-sample
response <- vti
# Define predictor matrix for forecasting
order_max <- 5
predictor <- sapply(1+nagg*(0:order_max), rutils::lagit,
               input=predictor)
predictor <- cbind(rep(1, nrows), predictor)
# Define design matrix
design <- cbind(response, predictor)
# Perform rolling forecasting
look_back <- 100
forecasts <- sapply((look_back+1):nrows, function(endp) {
  # Define rolling look-back range
  startp <- max(1, endp-look_back)
  # Or expanding look-back range
  # startp <- 1
  rangev <- startp:(endp-1)
  # Invert the predictor matrix
  design_inv <- MASS::ginv(design[rangev, -1])
  # Calculate fitted coefficients
  coeff <- drop(design_inv %*% design[rangev, 1])
  # Calculate forecast
  drop(design[endp, -1] %*% coeff)
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
legend(x="top", legend=c("VTI returns", "forecasts"),
 col=c("blue", "red"), lty=1, lwd=6,
 cex=0.9, bg="white", bty="n")
# Define backtesting function
sim_forecasts <- function(response, predictor=response, nagg=5,
                ordern=5, look_back=100) {
  nrows <- NROW(response)
  # Define predictor as a rolling mean
  predictor <- roll::roll_mean(vti, width=nagg, min_obs=1)
  # Define predictor matrix for forecasting
  predictor <- sapply(1+nagg*(0:ordern), rutils::lagit,
                 input=predictor)
  predictor <- cbind(rep(1, nrows), predictor)
  # Define design matrix
  design <- cbind(response, predictor)
  # Perform rolling forecasting
  forecasts <- sapply((look_back+1):nrows, function(endp) {
    # Define rolling look-back range
    startp <- max(1, endp-look_back)
    # Or expanding look-back range
    # startp <- 1
    rangev <- startp:(endp-1)
    # Invert the predictor matrix
    design_inv <- MASS::ginv(design[rangev, -1])
    # Calculate fitted coefficients
    coeff <- drop(design_inv %*% design[rangev, 1])
    # Calculate forecast
    drop(design[endp, -1] %*% coeff)
  })  # end sapply
  # Add warmup period
  forecasts <- c(rep(0, look_back), forecasts)
  roll::roll_mean(forecasts, width=nagg, min_obs=1)
}  # end sim_forecasts
# Simulate the rolling autoregressive forecasts
forecasts <- sim_forecasts(vti, ordern=5, look_back=100)
c(mse=mean((vti - forecasts)^2), cor=cor(vti, forecasts))
look_backs <- seq(20, 600, 40)
library(parallel)  # Load package parallel
# Calculate number of available cores
ncores <- detectCores() - 1
# Initialize compute cluster under Windows
cluster <- makeCluster(ncores)
# clusterExport(cluster, c("startd", "barp"))
# Perform parallel loop under Windows
forecasts <- parLapply(cluster, look_backs, sim_forecasts, response=vti,
                  predictor=vti, nagg=5, ordern=5)
# Perform parallel bootstrap under Mac-OSX or Linux
forecasts <- mclapply(look_backs, sim_forecasts, response=vti,
  predictor=vti, nagg=5, ordern=5, mc.cores=ncores)
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
orders <- 2:6
library(parallel)  # Load package parallel
# Calculate number of available cores
ncores <- detectCores() - 1
# Initialize compute cluster under Windows
cluster <- makeCluster(ncores)
# clusterExport(cluster, c("startd", "barp"))
# Perform parallel loop under Windows
forecasts <- parLapply(cluster, orders, sim_forecasts, response=vti,
                  predictor=vti, nagg=5, look_back=look_back)
stopCluster(cluster)  # Stop R processes over cluster under Windows
# Perform parallel bootstrap under Mac-OSX or Linux
forecasts <- mclapply(orders, sim_forecasts, response=vti,
  predictor=vti, nagg=5, look_back=look_back, mc.cores=ncores)
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
# Load constant maturity Treasury rates
load(file="/Users/jerzy/Develop/lecture_slides/data/rates_data.RData")
# Combine rates into single xts series
rates <- do.call(cbind, as.list(rates_env))
# Sort the columns of rates according bond maturity
namesv <- colnames(rates)
namesv <- substr(namesv, start=4, stop=10)
namesv <- as.numeric(namesv)
indeks <- order(namesv)
rates <- rates[, indeks]
# Align rates dates with VTI prices
closep <- quantmod::Cl(rutils::etfenv$VTI)
colnames(closep) <- "VTI"
nrows <- NROW(closep)
dates <- zoo::index(closep)
rates <- na.omit(rates[dates])
closep <- closep[zoo::index(rates)]
dates <- zoo::index(closep)
# Calculate VTI returns and IR changes
returns <- rutils::diffit(log(closep))
rates_diff <- rutils::diffit(log(rates))
# Regress VTI returns versus the lagged rate differences
predictor <- rutils::lagit(rates_diff)
model <- lm(returns ~ predictor)
summary(model)
# Regress VTI returns before and after 2012
summary(lm(returns["/2012"] ~ predictor["/2012"]))
summary(lm(returns["2012/"] ~ predictor["2012/"]))
# Calculate PCA of rates correlation matrix
eigend <- eigen(cor(rates_diff))
rates_pca <- -rates_diff %*% eigend$vectors
colnames(rates_pca) <- paste0("PC", 1:6)
# Define predictor as the YC PCAs
predictor <- rutils::lagit(rates_pca)
model <- lm(returns ~ predictor)
summary(model)
# Plot YC steepener principal component with VTI
datav <- cbind(returns, rates_pca[, 2])
colnames(datav) <- c("VTI", "Steepener")
colnames <- colnames(datav)
dygraphs::dygraph(cumsum(datav), main="VTI and Yield Curve Steepener") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", label=colnames[1], strokeWidth=1, col="blue") %>%
  dySeries(name=colnames[2], axis="y2", label=colnames[2], strokeWidth=1, col="red")
# Define predictor with intercept term
predictor <- rutils::lagit(rates_diff)
predictor <- cbind(rep(1, NROW(predictor)), predictor)
colnames(predictor)[1] <- "intercept"
# Calculate inverse of predictor
inverse <- MASS::ginv(predictor)
# Calculate coefficients from response and inverse of predictor
response <- returns
coeff <- drop(inverse %*% response)
# Calculate forecasts in-sample
forecasts <- (predictor %*% coeff)
pnls <- forecasts*returns
# Calculate in-sample factors
factors <- (predictor * coeff)
apply(factors, 2, sd)
# Plot dygraph of in-sample IR strategy
wealth <- cbind(returns, pnls)
colnames(wealth) <- c("VTI", "Strategy")
colnames <- colnames(wealth)
dygraphs::dygraph(cumsum(wealth), main="Yield Curve Strategy In-sample") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=colnames[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Define in-sample and out-of-sample intervals
insample <- (dates < as.Date("2020-01-01"))
outsample <- (dates >= as.Date("2020-01-01"))
# Calculate inverse of predictor in-sample
inverse <- MASS::ginv(predictor[insample, ])
# Calculate coefficients in-sample
coeff <- drop(inverse %*% response[insample, ])
# Calculate forecasts and pnls out-of-sample
forecasts <- (predictor[outsample, ] %*% coeff)
pnls <- forecasts*returns[outsample, ]
# Plot dygraph of out-of-sample IR PCA strategy
wealth <- cbind(returns[outsample, ], pnls)
colnames(wealth) <- c("VTI", "Strategy")
colnames <- colnames(wealth)
dygraphs::dygraph(cumsum(wealth), main="Yield Curve Strategy Out-of-Sample") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=colnames[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Find optimal nagg for predictor
naggs <- 5:100
tvalues <- sapply(naggs, function(nagg) {
  predictor <- roll::roll_mean(rates_diff, width=nagg, min_obs=1)
  predictor <- cbind(rep(1, NROW(predictor)), predictor)
  predictor <- rutils::lagit(predictor)
  model <- lm(response ~ predictor - 1)
  model_sum <- summary(model)
  max(abs(model_sum$coefficients[, 3][-1]))
})  # end sapply
naggs[which.max(tvalues)]
plot(naggs, tvalues, t="l", col="blue", lwd=2)
# Calculate aggregated predictor
nagg <- 53
predictor <- roll::roll_mean(rates_diff, width=nagg, min_obs=1)
predictor <- rutils::lagit(predictor)
predictor <- cbind(rep(1, NROW(predictor)), predictor)
model <- lm(response ~ predictor - 1)
summary(model)
# Calculate forecasts in-sample
inverse <- MASS::ginv(predictor)
coeff <- drop(inverse %*% response)
forecasts <- (predictor %*% coeff)
pnls <- forecasts*returns
# Plot dygraph of in-sample IR strategy
wealth <- cbind(returns, pnls)
colnames(wealth) <- c("VTI", "Strategy")
colnames <- colnames(wealth)
dygraphs::dygraph(cumsum(wealth), main="Aggregated YC Strategy In-sample") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=colnames[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Define in-sample and out-of-sample intervals
insample <- (dates < as.Date("2020-01-01"))
outsample <- (dates >= as.Date("2020-01-01"))
# Calculate forecasts and pnls out-of-sample
inverse <- MASS::ginv(predictor[insample, ])
coeff <- drop(inverse %*% response[insample, ])
forecasts <- (predictor[outsample, ] %*% coeff)
pnls <- forecasts*returns[outsample, ]
# Plot dygraph of out-of-sample YC strategy
wealth <- cbind(returns[outsample, ], pnls)
colnames(wealth) <- c("VTI", "Strategy")
colnames <- colnames(wealth)
dygraphs::dygraph(cumsum(wealth), main="Aggregated YC Strategy Out-of-Sample") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=colnames[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Define yearly dates
format(dates[1], "%Y")
years <- paste0(seq(2001, 2022, 1), "-01-01")
years <- as.Date(years)
# Perform loop over yearly dates
pnls <- lapply(3:(NROW(years)-1), function(i) {
  # Define in-sample and out-of-sample intervals
  insample <- (dates > years[i-2]) & (dates < years[i])
  outsample <- (dates >= years[i]) & (dates < years[i+1])
  # Calculate coefficients in-sample
  inverse <- MASS::ginv(predictor[insample, ])
  # inverse <- HighFreq::calc_inv(predictor[insample, ], eigen_max=3)
  coeff <- drop(inverse %*% response[insample, ])
  # Calculate forecasts and pnls out-of-sample
  forecasts <- (predictor[outsample, ] %*% coeff)
  forecasts*returns[outsample, ]
})  # end lapply
pnls <- do.call(rbind, pnls)
# Plot dygraph of rolling yearly IR strategy
wealth <- cbind(returns[zoo::index(pnls),], pnls)
colnames(wealth) <- c("VTI", "Strategy")
colnames <- colnames(wealth)
dygraphs::dygraph(cumsum(wealth), main="Rolling Yearly Yield Curve Strategy") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=colnames[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Define monthly dates
format(dates[1], "%m-%Y")
format(dates[NROW(dates)], "%m-%Y")
months <- seq.Date(from=as.Date("2001-05-01"), to=as.Date("2021-04-01"), by="month")
# Perform loop over monthly dates
pnls <- lapply(12:(NROW(months)-1), function(i) {
  # Define in-sample and out-of-sample intervals
  insample <- (dates > months[i-11]) & (dates < months[i])
  outsample <- (dates > months[i]) & (dates < months[i+1])
  # Calculate forecasts and pnls out-of-sample
  inverse <- MASS::ginv(predictor[insample, ])
  # inverse <- HighFreq::calc_inv(predictor[insample, ], eigen_max=3)
  coeff <- drop(inverse %*% response[insample, ])
  forecasts <- (predictor[outsample, ] %*% coeff)
  forecasts*returns[outsample, ]
})  # end lapply
pnls <- do.call(rbind, pnls)
# Plot dygraph of rolling monthly IR strategy
wealth <- cbind(returns[zoo::index(pnls),], pnls)
colnames(wealth) <- c("VTI", "Strategy")
colnames <- colnames(wealth)
dygraphs::dygraph(cumsum(wealth), main="Rolling Monthly Yield Curve Strategy") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=colnames[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Define weekly dates
weeks <- seq.Date(from=as.Date("2001-05-01"), to=as.Date("2021-04-01"), by="weeks")
# Perform loop over weekly dates
pnls <- lapply(51:(NROW(weeks)-1), function(i) {
  # Define in-sample and out-of-sample intervals
  insample <- (dates > weeks[i-50]) & (dates < weeks[i])
  outsample <- (dates > weeks[i]) & (dates < weeks[i+1])
  # Calculate forecasts and pnls out-of-sample
  inverse <- MASS::ginv(predictor[insample, ])
  # inverse <- HighFreq::calc_inv(predictor[insample, ], eigen_max=3)
  coeff <- drop(inverse %*% response[insample, ])
  forecasts <- (predictor[outsample, ] %*% coeff)
  forecasts*returns[outsample, ]
})  # end lapply
pnls <- do.call(rbind, pnls)
# Plot dygraph of rolling weekly IR strategy
wealth <- cbind(returns[zoo::index(pnls),], pnls)
colnames(wealth) <- c("VTI", "Strategy")
colnames <- colnames(wealth)
dygraphs::dygraph(cumsum(wealth), main="Rolling Weekly Yield Curve Strategy") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=colnames[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Extract ETF returns
symbols <- c("VTI", "IEF", "DBC")
returns <- rutils::etfenv$returns[, symbols]
returns <- na.omit(returns)
# Or, select rows with IEF data
# returns <- returns[zoo::index(rutils::etfenv$IEF)]
# Copy over NA values
# returns[1, is.na(returns[1, ])] <- 0
# returns <- zoo::na.locf(returns, na.rm=FALSE)
# Define end of month end points
endp <- rutils::calc_endpoints(returns, interval="months")
endp <- endp[-1]
nrows <- NROW(endp)
dates <- zoo::index(returns)[endp]
# Start points equal end points lagged by 12-month look-back interval
look_back <- 12
startp <- c(rep_len(1, look_back-1),
  endp[1:(nrows - look_back + 1)])
# Calculate matrix of look-back intervals
look_backs <- cbind(startp, endp)
colnames(look_backs) <- c("start", "end")
# Calculate matrix of look-forward intervals
look_fwds <- cbind(endp + 1, rutils::lagit(endp, -1))
look_fwds[nrows, ] <- endp[nrows]
colnames(look_fwds) <- c("start", "end")
# Inspect the intervals
head(cbind(look_backs, look_fwds))
tail(cbind(look_backs, look_fwds))
# Define performance function as Sharpe ratio
objfun <- function(returns) sum(returns)/sd(returns)
# Calculate past performance over look-back intervals
past <- apply(look_backs, 1, function(ep) {
  sapply(returns[ep[1]:ep[2]], objfun)
})  # end sapply
past <- t(past)
past[is.na(past)] <- 0
# Weights are proportional to past performance
weightv <- past
# weightv[weightv < 0] <- 0
# Scale weightv so sum of squares is equal to 1.
weightv <- weightv/sqrt(rowSums(weightv^2))
# Or scale weightv so sum is equal to 1
# weightv <- weightv/rowSums(weightv)
# Set NA values to zero
weightv[is.na(weightv)] <- 0
sum(is.na(weightv))
# Calculate future out-of-sample performance
future <- apply(look_fwds, 1, function(ep) {
  sapply(returns[ep[1]:ep[2]], sum)
})  # end sapply
future <- t(future)
future[is.na(future)] <- 0
tail(future)
# Calculate the momentum pnls
pnls <- rowSums(weightv*future)
# Lag the future and momentum returns to proper dates
future <- rutils::lagit(future)
pnls <- rutils::lagit(pnls)
# The momentum strategy has low correlation to stocks
cor(pnls, future)
# Define all-weather benchmark
weightsaw <- c(0.30, 0.55, 0.15)
all_weather <- future %*% weightsaw
# Calculate the wealth of momentum returns
wealth <- xts::xts(cbind(all_weather, pnls), order.by=dates)
colnames(wealth) <- c("All-Weather", "Momentum")
cor(wealth)
# Plot dygraph of the momentum strategy returns
dygraphs::dygraph(cumsum(wealth), main="Monthly Momentum Strategy vs All-Weather") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Define backtest functional
backtestmom <- function(returns,
                objfun=function(returns) (sum(returns)/sd(returns)),
                look_back=12, rfreq="months", bid_offer=0.001,
                endp=rutils::calc_endpoints(returns, interval=rfreq)[-1],
                with_weights=FALSE, ...) {
  stopifnot("package:rutils" %in% search() || require("rutils", quietly=TRUE))
  # Define look-back and look-forward intervals
  nrows <- NROW(endp)
  startp <- c(rep_len(1, look_back-1), endp[1:(nrows-look_back+1)])
  # Calculate look-back intervals
  look_backs <- cbind(startp, endp)
  # Calculate look-forward intervals
  look_fwds <- cbind(endp + 1, rutils::lagit(endp, -1))
  look_fwds[nrows, ] <- endp[nrows]
  # Calculate past performance over look-back intervals
  past <- t(apply(look_backs, 1, function(ep) sapply(returns[ep[1]:ep[2]], objfun)))
  past[is.na(past)] <- 0
  # Calculate future performance
  future <- t(apply(look_fwds, 1, function(ep) sapply(returns[ep[1]:ep[2]], sum)))
  future[is.na(future)] <- 0
  # Scale weightv so sum of squares is equal to 1
  weightv <- past
  weightv <- weightv/sqrt(rowSums(weightv^2))
  weightv[is.na(weightv)] <- 0  # Set NA values to zero
  # Calculate momentum profits and losses
  pnls <- rowSums(weightv*future)
  # Calculate transaction costs
  costs <- 0.5*bid_offer*cumprod(1 + pnls)*rowSums(abs(rutils::diffit(weightv)))
  pnls <- (pnls - costs)
  if (with_weights)
    rutils::lagit(cbind(pnls, weightv))
  else
    rutils::lagit(pnls)
}  # end backtestmom
source("/Users/jerzy/Develop/lecture_slides/scripts/back_test.R")
look_backs <- seq(3, 15, by=1)
objfun <- function(returns) sum(returns)/sd(returns)
profilev <- sapply(look_backs, function(look_back) {
  pnls <- backtestmom(returns=returns, endp=endp,
    look_back=look_back, objfun=objfun)
  sum(pnls)
})  # end sapply
# Plot momemntum PnLs
x11(width=6, height=5)
plot(x=look_backs, y=profilev, t="l",
  main="Momemntum PnL as function of look_back",
  xlab="look_back (months)", ylab="pnl")
# Optimal look_back
look_back <- look_backs[which.max(profilev)]
pnls <- backtestmom(returns=returns,
  look_back=look_back, endp=endp,
  objfun=objfun, with_weights=TRUE)
tail(pnls)
# Calculate the wealth of momentum returns
retmom <- pnls[, 1]
wealth <- xts::xts(cbind(all_weather, retmom), order.by=dates)
colnames(wealth) <- c("All-Weather", "Momentum")
cor(wealth)
# Plot dygraph of the momentum strategy returns
dygraphs::dygraph(cumsum(wealth), main="Monthly Momentum Strategy vs All-Weather") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Or
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue")
quantmod::chart_Series(cumsum(wealth), theme=plot_theme, lwd=2,
       name="Momentum PnL")
legend("topleft", legend=colnames(wealth),
  inset=0.1, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")
# Plot the momentum portfolio weights
weightv <- pnls[, -1]
vti <- log(quantmod::Cl(rutils::etfenv$VTI[dates]))
colnames(vti) <- "VTI"
datav <- cbind(vti, weightv)
datav <- na.omit(datav)
colnames(datav)[2:NCOL(pnls)] <- paste0(colnames(weightv), "_weight")
zoo::plot.zoo(datav, xlab=NULL, main="Momentum Weights")
# Calculate ETF betas
betas_etf <- sapply(returns, function(x)
  cov(returns$VTI, x)/var(x))
# Momentum beta is equal weights times ETF betas
betas <- weightv %*% betas_etf
betas <- xts::xts(betas, order.by=dates)
colnames(betas) <- "momentum_beta"
datav <- cbind(betas, vti)
zoo::plot.zoo(datav,
  oma = c(3, 1, 3, 0), mar = c(0, 4, 0, 1),
  main="Momentum Beta & VTI Price", xlab="")
# Open x11 for plotting and set parameters to reduce whitespace around plot
x11(width=6, height=5)
par(mar=c(4, 4, 3, 1), oma=c(0, 0, 0, 0))
# Merton-Henriksson test
vti <- rutils::diffit(vti)
design <- cbind(VTI=vti, 0.5*(vti+abs(vti)), vti^2)
colnames(design)[2:3] <- c("merton", "treynor")
model <- lm(retmom ~ VTI + merton, data=design); summary(model)
# Treynor-Mazuy test
model <- lm(retmom ~ VTI + treynor, data=design); summary(model)
# Plot residual scatterplot
plot.default(x=vti, y=retmom, xlab="VTI", ylab="momentum")
title(main="Treynor-Mazuy market timing test\n for Momentum vs VTI", line=0.5)
# Plot fitted (predicted) response values
points.default(x=vti, y=model$fitted.values, pch=16, col="red")
residuals <- model$residuals
text(x=0.0, y=max(residuals), paste("Treynor test t-value =", round(summary(model)$coeff["treynor", "t value"], 2)))
# Standardize the returns
retmom_std <- (retmom-mean(retmom))/sd(retmom)
vti <- (vti-mean(vti))/sd(vti)
# Calculate skewness and kurtosis
apply(cbind(retmom_std, vti), 2, function(x)
  sapply(c(skew=3, kurt=4),
    function(e) sum(x^e)))/nrows
# Plot histogram
hist(retmom_std, breaks=30,
  main="Momentum and VTI Return Distributions (standardized",
  xlim=c(-4, 4),
  xlab="", ylab="", freq=FALSE)
# Draw kernel density of histogram
lines(density(retmom_std), col='red', lwd=2)
lines(density(vti), col='blue', lwd=2)
# Add legend
legend("topright", inset=0.05, cex=0.8, title=NULL,
 leg=c("Momentum", "VTI"),
 lwd=6, bg="white", col=c("red", "blue"))
# Combine momentum strategy with all-weather
all_weather <- sd(retmom)*all_weather/sd(all_weather)
wealth <- cbind(retmom, all_weather, 0.5*(retmom + all_weather))
colnames(wealth) <- c("momentum", "all_weather", "combined")
# Calculate strategy annualized Sharpe ratios
apply(wealth, MARGIN=2, function(x) {
  sqrt(12)*sum(x)/sd(x)/NROW(x)
})  # end apply
# Calculate strategy correlations
cor(wealth)
# Calculate cumulative wealth
wealth <- xts::xts(wealth, dates)
# Plot ETF momentum strategy combined with All-Weather
dygraphs::dygraph(cumsum(wealth), main="ETF Momentum Strategy Combined with All-Weather") %>%
  dyOptions(colors=c("red", "blue", "green"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Or
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("green", "blue", "red")
quantmod::chart_Series(wealth, theme=plot_theme,
       name="ETF Momentum Strategy Combined with All-Weather")
legend("topleft", legend=colnames(wealth),
  inset=0.1, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")
# Calculate rolling variance
look_back <- 252
variance <- roll::roll_var(returns, width=look_back, min_obs=1)
variance[1, ] <- 1
# Calculate rolling Sharpe
past <- roll::roll_mean(returns, width=look_back, min_obs=1)
weightv <- past/sqrt(variance)
weightv <- weightv/sqrt(rowSums(weightv^2))
weightv <- rutils::lagit(weightv)
sum(is.na(weightv))
# Calculate momentum profits and losses
pnls <- rowMeans(weightv*returns)
# Calculate transaction costs
bid_offer <- 0.001
costs <- 0.5*bid_offer*rowSums(abs(rutils::diffit(weightv)))
pnls <- (pnls - costs)
# Define all-weather benchmark
weightsaw <- c(0.30, 0.55, 0.15)
all_weather <- returns %*% weightsaw
# Calculate the wealth of momentum returns
wealth <- xts::xts(cbind(all_weather, pnls), order.by=zoo::index(returns))
colnames(wealth) <- c("All-Weather", "Momentum")
cor(wealth)
# Plot dygraph of the momentum strategy returns
dygraphs::dygraph(cumsum(wealth)[dates], main="Daily Momentum Strategy vs All-Weather") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Define backtest functional for daily momentum strategy
# If trend=(-1) then it backtests a mean reverting strategy
momentum_daily <- function(returns, look_back=252, bid_offer=0.001, trend=1, ...) {
  stopifnot("package:quantmod" %in% search() || require("quantmod", quietly=TRUE))
  # Calculate rolling variance
  variance <- roll::roll_var(returns, width=look_back, min_obs=1)
  variance[1, ] <- 1
  variance[variance <= 0] <- 1
# Calculate rolling Sharpe
  past <- roll::roll_mean(returns, width=look_back, min_obs=1)
  weightv <- past/sqrt(variance)
  weightv <- weightv/sqrt(rowSums(weightv^2))
  weightv <- rutils::lagit(weightv)
  # Calculate momentum profits and losses
  pnls <- trend*rowMeans(weightv*returns)
  # Calculate transaction costs
  costs <- 0.5*bid_offer*rowSums(abs(rutils::diffit(weightv)))
  (pnls - costs)
}  # end momentum_daily
# Simulate a daily ETF momentum strategy
source("/Users/jerzy/Develop/lecture_slides/scripts/back_test.R")
wealth <- momentum_daily(returns=returns, look_back=252,
  bid_offer=bid_offer)
# Perform sapply loop over look_backs
look_backs <- seq(50, 300, by=50)
wealth <- sapply(look_backs, momentum_daily,
  returns=returns, bid_offer=bid_offer)
colnames(wealth) <- paste0("look_back=", look_backs)
wealth <- xts::xts(wealth, zoo::index(returns))
tail(wealth)
# Plot EWMA strategies with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(wealth))
quantmod::chart_Series(cumsum(wealth),
  theme=plot_theme, name="Cumulative Returns of Daily ETF Momentum Strategies")
legend("bottomleft", legend=colnames(wealth),
  inset=0.02, bg="white", cex=0.7, lwd=rep(6, NCOL(returns)),
  col=plot_theme$col$line.col, bty="n")
# Load daily S&P500 percentage stock returns.
load(file="/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
# Overwrite NA values in returns100
returns100 <- returns100["2000/"]
returns100[1, is.na(returns100[1, ])] <- 0
returns100 <- zoo::na.locf(returns100, na.rm=FALSE)
# Simulate a daily S&P500 momentum strategy.
# Perform sapply loop over look_backs
look_backs <- seq(50, 300, by=50)
wealth <- sapply(look_backs, momentum_daily,
  returns=returns100, bid_offer=0)
colnames(wealth) <- paste0("look_back=", look_backs)
wealth <- xts::xts(wealth, zoo::index(returns100))
# Plot daily S&P500 momentum strategies with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- colorRampPalette(c("blue", "red"))(NCOL(wealth))
quantmod::chart_Series(cumsum(wealth),
  theme=plot_theme, name="Cumulative Returns of S&P500 Momentum Strategies")
legend("bottomleft", legend=colnames(wealth),
  inset=0.02, bg="white", cex=0.7, lwd=rep(6, NCOL(returns)),
  col=plot_theme$col$line.col, bty="n")
# Perform sapply loop over look_backs
look_backs <- seq(5, 50, by=5)
wealth <- sapply(look_backs, momentum_daily,
  returns=returns100, bid_offer=0, trend=(-1))
colnames(wealth) <- paste0("look_back=", look_backs)
wealth <- xts::xts(wealth, zoo::index(returns100))
# Plot EWMA strategies with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- colorRampPalette(c("blue", "red"))(NCOL(wealth))
quantmod::chart_Series(cumsum(wealth),
  theme=plot_theme, name="Cumulative Returns of S&P500 Mean Reverting Strategies")
legend("topleft", legend=colnames(wealth),
  inset=0.05, bg="white", cex=0.7, lwd=rep(6, NCOL(returns)),
  col=plot_theme$col$line.col, bty="n")
library(rutils)
# Select all the ETF symbols except "VXX", "SVXY" and "MTUM"
symbols <- colnames(rutils::etfenv$returns)
symbols <- symbols[!(symbols %in% c("VXX", "SVXY", "MTUM"))]
# Extract columns of rutils::etfenv$returns and overwrite NA values
returns <- rutils::etfenv$returns[, symbols]
nassets <- NCOL(returns)
# returns <- na.omit(returns)
returns[1, is.na(returns[1, ])] <- 0
returns <- zoo::na.locf(returns, na.rm=FALSE)
# Returns in excess of risk-free rate
riskf <- 0.03/252
excess <- (returns - riskf)
# Maximum Sharpe weights in-sample interval
inverse <- MASS::ginv(cov(returns["/2014"]))
weightv <- inverse %*% colMeans(excess["/2014"])
weightv <- drop(weightv/sqrt(sum(weightv^2)))
names(weightv) <- colnames(returns)
# Plot portfolio weights
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
barplot(sort(weightv), main="Maximum Sharpe Weights", cex.names=0.7)
# Calculate portfolio returns
retsis <- returns["/2014"]
portf_is <- xts::xts(retsis %*% weightv, zoo::index(retsis))
indeks <- xts::xts(rowSums(retsis)/sqrt(nassets), zoo::index(retsis))
portf_is <- portf_is*sd(indeks)/sd(portf_is)
# Plot cumulative portfolio returns
wealth <- cumsum(cbind(portf_is, indeks))
colnames(wealth) <- c("Optimal Portfolio", "Equal Weight Portfolio")
dygraphs::dygraph(wealth, main="In-sample Optimal Portfolio Returns") %>%
  dyOptions(colors=c("red", "blue"), strokeWidth=2) %>%
  dyLegend(width=500)
# Out-of-sample portfolio returns
retsos <- returns["2015/"]
portf_os <- xts::xts(retsos %*% weightv, zoo::index(retsos))
indeks <- xts::xts(rowSums(retsos)/sqrt(nassets), zoo::index(retsos))
portf_os <- portf_os*sd(indeks)/sd(portf_os)
# Plot cumulative portfolio returns
wealth <- cumsum(cbind(portf_os, indeks))
colnames(wealth) <- c("Optimal Portfolio", "Equal Weight Portfolio")
dygraphs::dygraph(wealth, main="Out-of-sample Optimal Portfolio Returns") %>%
  dyOptions(colors=c("red", "blue"), strokeWidth=2) %>%
  dyLegend(width=500)
load("/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
# Overwrite NA values in returns
returns <- returns["2000/"]
nassets <- NCOL(returns)
returns[1, is.na(returns[1, ])] <- 0
returns <- zoo::na.locf(returns, na.rm=FALSE)
riskf <- 0.03/252
excess <- (returns - riskf)
retsis <- returns["/2010"]
retsos <- returns["2011/"]
# Maximum Sharpe weights in-sample interval
covmat <- cov(retsis)
inverse <- MASS::ginv(covmat)
weightv <- inverse %*% colMeans(excess["/2010"])
weightv <- drop(weightv/sqrt(sum(weightv^2)))
names(weightv) <- colnames(returns)
# Calculate portfolio returns
portf_is <- xts::xts(retsis %*% weightv, zoo::index(retsis))
portf_os <- xts::xts(retsos %*% weightv, zoo::index(retsos))
indeks <- xts::xts(rowSums(returns)/sqrt(nassets), zoo::index(returns))
# Plot cumulative portfolio returns
wealth <- rbind(portf_is, portf_os)
wealth <- wealth*sd(indeks)/sd(wealth)
wealth <- cumsum(cbind(wealth, indeks))
colnames(wealth) <- c("Optimal Portfolio", "Equal Weight Portfolio")
dygraphs::dygraph(wealth, main="Out-of-sample Optimal Portfolio Returns for Stocks") %>%
  dyOptions(colors=c("red", "blue"), strokeWidth=2) %>%
  dyEvent(zoo::index(last(retsis[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=500)
# Create rectangular matrix with collinear columns
matrixv <- matrix(rnorm(10*8), nc=10)
# Calculate covariance matrix
covmat <- cov(matrixv)
# Calculate inverse of covmat - error
inverse <- solve(covmat)
# Calculate regularized inverse of covmat
inverse <- MASS::ginv(covmat)
# Verify inverse property of matrixv
all.equal(covmat, covmat %*% inverse %*% covmat)
# Perform eigen decomposition
eigend <- eigen(covmat)
eigenvec <- eigend$vectors
eigenval <- eigend$values
# Set tolerance for determining zero singular values
precision <- sqrt(.Machine$double.eps)
# Calculate regularized inverse matrix
not_zero <- (eigenval > (precision * eigenval[1]))
reg_inverse <- eigenvec[, not_zero] %*%
  (t(eigenvec[, not_zero]) / eigenval[not_zero])
# Verify inverse property of matrixv
all.equal(inverse, reg_inverse)
# Calculate in-sample covariance matrix
covmat <- cov(retsis)
eigend <- eigen(covmat)
eigenvec <- eigend$vectors
eigenval <- eigend$values
# Calculate regularized inverse of covariance matrix
max_eigen <- 21
inverse <- eigenvec[, 1:max_eigen] %*%
  (t(eigenvec[, 1:max_eigen]) / eigend$values[1:max_eigen])
# Calculate portfolio weights
weightv <- inverse %*% colMeans(excess["/2010"])
weightv <- drop(weightv/sqrt(sum(weightv^2)))
names(weightv) <- colnames(returns)
# Calculate portfolio returns
portf_is <- xts::xts(retsis %*% weightv, zoo::index(retsis))
portf_os <- xts::xts(retsos %*% weightv, zoo::index(retsos))
indeks <- xts::xts(rowSums(returns)/sqrt(nassets), zoo::index(returns))
# Plot cumulative portfolio returns
wealth <- rbind(portf_is, portf_os)
wealth <- wealth*sd(indeks)/sd(wealth)
wealth <- cumsum(cbind(wealth, indeks))
colnames(wealth) <- c("Optimal Portfolio", "Equal Weight Portfolio")
dygraphs::dygraph(wealth, main="Regularized Out-of-sample Optimal Portfolio Returns for Stocks") %>%
  dyOptions(colors=c("red", "blue"), strokeWidth=2) %>%
  dyEvent(zoo::index(last(retsis[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=500)
# Shrink the in-sample returns to their mean
rets_mean <- colMeans(retsis) - riskf
alpha <- 0.7
rets_mean <- (1 - alpha)*rets_mean + alpha*mean(rets_mean)
# Calculate portfolio weights
weightv <- inverse %*% rets_mean
weightv <- drop(weightv/sqrt(sum(weightv^2)))
# Calculate portfolio returns
portf_is <- xts::xts(retsis %*% weightv, zoo::index(retsis))
portf_os <- xts::xts(retsos %*% weightv, zoo::index(retsos))
# Plot cumulative portfolio returns
wealth <- rbind(portf_is, portf_os)
wealth <- wealth*sd(indeks)/sd(wealth)
wealth <- cumsum(cbind(wealth, indeks))
colnames(wealth) <- c("Optimal Portfolio", "Equal Weight Portfolio")
dygraphs::dygraph(wealth, main="Out-of-sample Returns for Stocks With Regularization and Shrinkage") %>%
  dyOptions(colors=c("red", "blue"), strokeWidth=2) %>%
  dyEvent(zoo::index(last(retsis[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=500)
library(RcppArmadillo)
# Source Rcpp functions from file
Rcpp::sourceCpp("/Users/jerzy/Develop/lecture_slides/scripts/calc_weights.cpp")
# Create random matrix of returns
matrixv <- matrix(rnorm(300), nc=5)
# Regularized inverse of covariance matrix
max_eigen <- 4
eigend <- eigen(cov(matrixv))
covinv <- eigend$vectors[, 1:max_eigen] %*%
  (t(eigend$vectors[, 1:max_eigen]) / eigend$values[1:max_eigen])
# Regularized inverse using RcppArmadillo
covinv_arma <- calc_inv(matrixv, max_eigen)
all.equal(covinv, covinv_arma)
# Microbenchmark RcppArmadillo code
library(microbenchmark)
summary(microbenchmark(
  pure_r={
    eigend <- eigen(cov(matrixv))
    eigend$vectors[, 1:max_eigen] %*%
(t(eigend$vectors[, 1:max_eigen]) / eigend$values[1:max_eigen])
  },
  r_cpp=calc_inv(matrixv, max_eigen),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
# Calculate vector of monthly end points and start points
endp <- rutils::calc_endpoints(returns, interval="months")
endp <- endp[endp > 2*NCOL(returns)]
nrows <- NROW(endp)
look_back <- 24
startp <- c(rep_len(0, look_back-1),
       endp[1:(nrows-look_back+1)])
# Perform loop over end points
rets_portf <- lapply(2:nrows, function(i) {
    # Subset the excess returns
    excess <- excess[startp[i-1]:endp[i-1], ]
    inverse <- MASS::ginv(cov(excess))
    # Calculate the maximum Sharpe ratio portfolio weights
    weightv <- inverse %*% colMeans(excess)
    weightv <- drop(weightv/sqrt(sum(weightv^2)))
    # Calculate the out-of-sample portfolio returns
    returns <- returns[(endp[i-1]+1):endp[i], ]
    xts::xts(returns %*% weightv, zoo::index(returns))
})  # end lapply
rets_portf <- rutils::do_call(rbind, rets_portf)
# Plot cumulative strategy returns
indeks <- xts::xts(rowSums(returns)/sqrt(nassets), zoo::index(returns))
wealth <- cumsum(na.omit(cbind(rets_portf, indeks*sd(rets_portf)/sd(indeks))))
colnames(wealth) <- c("Rolling Portfolio Strategy", "Equal Weight Portfolio")
dygraphs::dygraph(wealth, main="Rolling Portfolio Optimization Strategy") %>%
  dyOptions(colors=c("red", "blue"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
load("/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
# Overwrite NA values in returns
returns100[1, is.na(returns100[1, ])] <- 0
returns100 <- zoo::na.locf(returns100, na.rm=FALSE)
ncols <- NCOL(returns100) ; dates <- zoo::index(returns100)
# Define monthly end points
endp <- rutils::calc_endpoints(returns100, interval="months")
endp <- endp[endp > (ncols+1)]
nrows <- NROW(endp) ; look_back <- 12
startp <- c(rep_len(0, look_back-1), endp[1:(nrows-look_back+1)])
endp <- (endp - 1)
startp <- (startp - 1)
startp[startp < 0] <- 0
alpha <- 0.7 ; max_eigen <- 21
# Perform backtest in Rcpp
pnls <- HighFreq::back_test(typev="max_sharpe",
  excess=returns100, returns=returns100,
  startp=startp, endp=endp,
  alpha=alpha, max_eigen=max_eigen)
# Calculate returns on equal weight portfolio
indeks <- xts::xts(rowMeans(returns100), zoo::index(returns100))
# Plot cumulative strategy returns
wealth <- cbind(pnls, indeks, (pnls+indeks)/2)
wealth <- cumsum(na.omit(wealth))
colnames <- c("Strategy", "Index", "Average")
colnames(wealth) <- colnames
dygraphs::dygraph(wealth[endp], main="Rolling S&P500 Portfolio Optimization Strategy") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", col="red", strokeWidth=1) %>%
  dySeries(name=colnames[2], axis="y2", col="blue", strokeWidth=1) %>%
  dySeries(name=colnames[3], axis="y2", col="green", strokeWidth=2)
# Perform backtest over alphas
alpha_s <- seq(from=0.01, to=0.91, by=0.1)
pnls <- lapply(alpha_s, function(alpha) {
  HighFreq::back_test(typev="max_sharpe",
  excess=returns100, returns=returns100,
  startp=startp, endp=endp,
  alpha=alpha, max_eigen=max_eigen)
})  # end lapply
profilev <- sapply(pnls, sum)
plot(x=alpha_s, y=profilev, t="l", main="Strategy PnL as Function of Shrinkage Intensity Alpha",
  xlab="Shrinkage Intensity Alpha", ylab="pnl")
alpha <- alpha_s[which.max(profilev)]
pnls <- pnls[[which.max(profilev)]]
# Perform backtest over max_eigens
max_eigens <- seq(from=3, to=40, by=2)
pnls <- lapply(max_eigens, function(max_eigen) {
  HighFreq::back_test(typev="max_sharpe",
    excess=returns100, returns=returns100,
    startp=startp, endp=endp,
    alpha=alpha, max_eigen=max_eigen)
})  # end lapply
profilev <- sapply(pnls, sum)
plot(x=max_eigens, y=profilev, t="l", main="Strategy PnL as Function of Max_eigen",
  xlab="Max_eigen", ylab="pnl")
max_eigen <- max_eigens[which.max(profilev)]
pnls <- pnls[[which.max(profilev)]]
# Plot cumulative strategy returns
wealth <- cbind(pnls, indeks, (pnls+indeks)/2)
wealth <- cumsum(na.omit(wealth))
colnames <- c("Strategy", "Index", "Average")
colnames(wealth) <- colnames
dygraphs::dygraph(wealth[endp], main="Optimal Rolling S&P500 Portfolio Strategy") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", col="red", strokeWidth=1) %>%
  dySeries(name=colnames[2], axis="y2", col="blue", strokeWidth=1) %>%
  dySeries(name=colnames[3], axis="y2", col="green", strokeWidth=2)
# Perform backtest over look-backs
look_backs <- seq(from=3, to=24, by=1)
pnls <- lapply(look_backs, function(look_back) {
  startp <- c(rep_len(0, look_back-1), endp[1:(nrows-look_back+1)])
  startp <- (startp - 1)
  startp[startp < 0] <- 0
  HighFreq::back_test(typev="max_sharpe",
    excess=returns100, returns=returns100,
    startp=startp, endp=endp,
    alpha=alpha, max_eigen=max_eigen)
})  # end lapply
profilev <- sapply(pnls, sum)
plot(x=look_backs, y=profilev, t="l", main="Strategy PnL as Function of Look-back Interval",
  xlab="Look-back Interval", ylab="pnl")
look_back <- look_backs[which.max(profilev)]
pnls <- pnls[[which.max(profilev)]]
# Plot cumulative strategy returns
wealth <- cbind(pnls, indeks, (pnls+indeks)/2)
wealth <- cumsum(na.omit(wealth))
colnames <- c("Strategy", "Index", "Average")
colnames(wealth) <- colnames
dygraphs::dygraph(wealth[endp], main="Optimal Rolling S&P500 Portfolio Strategy") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", col="red", strokeWidth=1) %>%
  dySeries(name=colnames[2], axis="y2", col="blue", strokeWidth=1) %>%
  dySeries(name=colnames[3], axis="y2", col="green", strokeWidth=2)
