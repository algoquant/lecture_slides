# Specify AR process parameters
n_rows <- 1e3
co_eff <- c(0.1, 0.39, 0.5); n_coeff <- NROW(co_eff)
set.seed(1121); in_nov <- rnorm(n_rows)
# ari_ma <- filter(x=in_nov, filter=co_eff, method="recursive")
# Simulate AR process using C_rfilter()
ari_ma <- .Call(stats:::C_rfilter, in_nov, co_eff,
  double(n_rows + n_coeff))[-(1:n_coeff)]
# Fit AR model using ar.ols()
ar_fit <- ar.ols(ari_ma, order.max=n_coeff, aic=FALSE)
class(ar_fit)
is.list(ar_fit)
drop(ar_fit$ar)
# Define design matrix with intercept column
de_sign <- sapply(1:n_coeff, function(lagg) {
  rutils::lag_it(ari_ma, lagg=lagg)
})  # end sapply
de_sign <- cbind(rep(1, n_rows), de_sign)
# Fit AR model using regression
design_inv <- MASS::ginv(de_sign)
coeff_reg <- drop(design_inv %*% ari_ma)
all.equal(drop(ar_fit$ar), coeff_reg[-1], check.attributes=FALSE)
# Define design matrix without intercept column
de_sign <- sapply(1:n_coeff, function(lagg) {
  rutils::lag_it(ari_ma, lagg=lagg)
})  # end sapply
# Fit AR model using regression
design_inv <- MASS::ginv(de_sign)
coeff_reg <- drop(design_inv %*% ari_ma)
all.equal(drop(ar_fit$ar), coeff_reg, check.attributes=FALSE)

# Calculate the regression residuals
fit_ted <- drop(de_sign %*% coeff_reg)
residual_s <- drop(ari_ma - fit_ted)
# Variance of residuals
var_resid <- sum(residual_s^2)/(n_rows - NROW(coeff_reg))
# Design matrix squared
design_2 <- crossprod(de_sign)
# Calculate covariance matrix of AR coefficients
co_var <- var_resid*MASS::ginv(design_2)
coeff_regd <- sqrt(diag(co_var))
# Calculate t-values of AR coefficients
coeff_tvals <- drop(coeff_reg)/coeff_regd

# Fit AR(5) model into AR(3) process
de_sign <- sapply(1:5, function(lagg) {
  rutils::lag_it(ari_ma, lagg=lagg)
})  # end sapply
de_sign <- cbind(rep(1, n_rows), de_sign)
design_inv <- MASS::ginv(de_sign)
coeff_reg <- drop(design_inv %*% ari_ma)
# Calculate t-values of AR(5) coefficients
residual_s <- drop(ari_ma - drop(de_sign %*% coeff_reg))
var_resid <- sum(residual_s^2)/(n_rows - NROW(coeff_reg))
co_var <- var_resid*MASS::ginv(crossprod(de_sign))
coeff_regd <- sqrt(diag(co_var))
coeff_tvals <- drop(coeff_reg)/coeff_regd
# Fit AR(5) model using arima()
arima_fit <- arima(ari_ma, order=c(5, 0, 0), include.mean=FALSE)
arima_fit$coef
# Fit AR(5) model using auto.arima()
library(forecast)  # Load forecast
arima_fit <- forecast::auto.arima(ari_ma, max.p=5, max.q=0, max.d=0)
# Fit AR(5) model into VTI returns
re_turns <- drop(zoo::coredata(na.omit(rutils::etf_env$re_turns$VTI)))
de_sign <- sapply(1:5, function(lagg) {
  rutils::lag_it(re_turns, lagg=lagg)
})  # end sapply
de_sign <- cbind(rep(1, NROW(re_turns)), de_sign)
design_inv <- MASS::ginv(de_sign)
coeff_reg <- drop(design_inv %*% re_turns)
# Calculate t-values of AR(5) coefficients
residual_s <- drop(re_turns - drop(de_sign %*% coeff_reg))
var_resid <- sum(residual_s^2)/(n_rows - NROW(coeff_reg))
co_var <- var_resid*MASS::ginv(crossprod(de_sign))
coeff_regd <- sqrt(diag(co_var))
coeff_tvals <- drop(coeff_reg)/coeff_regd

# Compute autocorrelation coefficients
ac_f <- acf(ari_ma, lag=10, plot=FALSE)
ac_f <- drop(ac_f$acf)
acf1 <- ac_f[-NROW(ac_f)]
# Define Yule-Walker matrix
yule_walker <- sapply(2:9, function(lagg) {
  c(acf1[lagg:1], acf1[2:(NROW(acf1)-lagg+1)])
})  # end sapply
yule_walker <- cbind(acf1, yule_walker, rev(acf1))
# Generalized inverse of Yule-Walker matrix
yule_walker_inv <- MASS::ginv(yule_walker)
# Solve Yule-Walker equations
coeff_yw <- drop(yule_walker_inv %*% ac_f[-1])
round(coeff_yw, 5)
coeff_reg

n_rows <- 1e2
co_eff <- c(0.1, 0.39, 0.5); n_coeff <- NROW(co_eff)
set.seed(1121); in_nov <- rnorm(n_rows)
# Simulate AR process using filter()
ari_ma <- filter(x=in_nov, filter=co_eff, method="recursive")
ari_ma <- as.numeric(ari_ma)
# Simulate AR process using C_rfilter()
arima_fast <- .Call(stats:::C_rfilter, in_nov, co_eff,
  double(n_rows + n_coeff))
all.equal(ari_ma, arima_fast[-(1:n_coeff)],
  check.attributes=FALSE)

# Forecast AR(3) process using loop in R
forecast_s <- numeric(NROW(ari_ma)+1)
forecast_s[1] <- 0
forecast_s[2] <- co_eff[1]*ari_ma[1]
forecast_s[3] <- co_eff[1]*ari_ma[2] + co_eff[2]*ari_ma[1]
for (it in 4:NROW(forecast_s)) {
  forecast_s[it] <- ari_ma[(it-1):(it-3)] %*% co_eff
}  # end for
# Plot with legend
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
plot(ari_ma, main="Forecasting Using AR(3) Model",
  xlab="", ylab="", type="l")
lines(forecast_s, col="orange", lwd=3)
legend(x="topright", legend=c("series","forecasts"),
 col=c("black", "orange"), lty=1, lwd=6,
 cex=0.9, bg="white", bty="n")

# Forecast using filter()
filter_fast <- filter(x=ari_ma, sides=1,
  filter=co_eff, method="convolution")
filter_fast <- as.numeric(filter_fast)
# Compare excluding warmup period
all.equal(forecast_s[-(1:n_coeff)], filter_fast[-(1:(n_coeff-1))],
    check.attributes=FALSE)
# Filter using C_cfilter() compiled C++ function directly
filter_fast <- .Call(stats:::C_cfilter, ari_ma, filter=co_eff,
               sides=1, circular=FALSE)
# Compare excluding warmup period
all.equal(forecast_s[-(1:n_coeff)], filter_fast[-(1:(n_coeff-1))],
    check.attributes=FALSE)
# Define design matrix without intercept column
de_sign <- sapply(1:(n_coeff-1), function(lagg) {
  rutils::lag_it(ari_ma, lagg=lagg)
})  # end sapply
de_sign <- cbind(ari_ma, de_sign)
de_sign <- rbind(numeric(n_coeff), de_sign)
# Forecast using design matrix
filter_fast <- drop(de_sign %*% co_eff)
# Compare with loop in R
all.equal(forecast_s, filter_fast, check.attributes=FALSE)

# Fit ARIMA model using arima()
arima_fit <- arima(ari_ma, order=c(3,0,0), include.mean=FALSE)
arima_fit$coef
co_eff
# One-step-ahead forecast using predict.Arima()
pre_dict <- predict(arima_fit, n.ahead=1)
# Or directly call predict.Arima()
# pre_dict <- predict.Arima(arima_fit, n.ahead=1)
# Inspect the prediction object
class(pre_dict)
names(pre_dict)
class(pre_dict$pred)
unlist(pre_dict)
# One-step-ahead forecast using matrix algebra
fore_cast <- drop(ari_ma[n_rows:(n_rows-2)] %*% arima_fit$coef)
# Compare one-step-ahead forecasts
all.equal(pre_dict$pred[[1]], fore_cast)
# Get information about predict.Arima()
?stats:::predict.Arima

# Calculate the in-sample forecasting residuals
residual_s <- (ari_ma - forecast_s[-NROW(forecast_s)])
# Compare residuals with innovations
all.equal(in_nov, residual_s, check.attributes=FALSE)
plot(residual_s, t="l", lwd=3, xlab="", ylab="",
     main="ARIMA Forecast Errors")

# Simulate AR process using C_rfilter()
n_rows <- 1e3
co_eff <- c(0.5, 0.0, 0.0); n_coeff <- NROW(co_eff)
set.seed(1121); in_nov <- rnorm(n_rows)
ari_ma <- .Call(stats:::C_rfilter, in_nov, co_eff,
  double(n_rows + n_coeff))[-(1:n_coeff)]
# Define order of the AR(p) forecasting model
or_der <- 5
# Define design matrix for forecasting
de_sign <- sapply(1:or_der, function(lagg) {
  rutils::lag_it(ari_ma, lagg=lagg)
})  # end sapply
de_sign <- cbind(rep(1, n_rows), de_sign)
de_sign <- cbind(ari_ma, de_sign)
# Length of look-back interval
look_back <- 100
# Perform rolling forecasting
forecast_s <- sapply(look_back:(n_rows-1), function(now) {
  # Subset the design matrix
  star_t <- max(1, now-look_back+1)
  de_sign <- de_sign[star_t:now, ]
  n_rows <- NROW(de_sign)
  # Fit AR(p) coefficients
  design_inv <- MASS::ginv(de_sign[, -1])
  co_eff <- drop(design_inv %*% de_sign[, 1])
  # Calculate forecast
  # de_sign[now:(now-or_der), 1] %*% co_eff
  sum(de_sign[n_rows:(n_rows-or_der), 1]*co_eff)
})  # end sapply
# Add warmup period
forecast_s <- c(numeric(look_back), forecast_s)

# Mean squared error
mean((ari_ma - forecast_s)^2)
# Correlation
cor(forecast_s, ari_ma)
# Plot forecasting series with legend
x11(width=6, height=5)
par(mar=c(3, 3, 2, 0), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
plot(ari_ma[(n_rows-look_back):n_rows], xlab="", ylab="", type="l", lwd=2,
  main="Rolling Forecasting Using AR(5) Model")
lines(forecast_s[(n_rows-look_back):n_rows], col="orange", lwd=2)
legend(x="topright", legend=c("series","forecasts"),
 col=c("black", "orange"), lty=1, lwd=6,
 cex=0.9, bg="white", bty="n")

# Define backtesting function
back_test <- function(ari_ma, or_der=2, look_back=11) {
  n_rows <- NROW(ari_ma)
  # Define design matrix for forecasting
  de_sign <- sapply(1:or_der, function(lagg) {
    rutils::lag_it(ari_ma, lagg=lagg)
  })  # end sapply
  de_sign <- cbind(rep(1, n_rows), de_sign)
  de_sign <- cbind(ari_ma, de_sign)
  # Perform rolling forecasting
  forecast_s <- sapply(look_back:(n_rows-1), function(now) {
    # Subset the design matrix
    star_t <- max(1, now-look_back+1)
    de_sign <- de_sign[star_t:now, ]
    n_rows <- NROW(de_sign)
    # Fit AR(p) coefficients
    design_inv <- MASS::ginv(de_sign[, -1])
    co_eff <- drop(design_inv %*% de_sign[, 1])
    # Calculate forecast
    # de_sign[now:(now-or_der), 1] %*% co_eff
    sum(de_sign[n_rows:(n_rows-or_der), 1]*co_eff)
  })  # end sapply
  # Add warmup period
  forecast_s <- c(numeric(look_back), forecast_s)
  c(mse=mean((ari_ma - forecast_s)^2), cor=cor(forecast_s, ari_ma))
}  # end back_test
# Apply the backtesting function
back_test(ari_ma, or_der=5, look_back=100)

look_backs <- seq(20, 200, 20)
back_tests <- sapply(look_backs, back_test,
        ari_ma=ari_ma, or_der=or_der)
back_tests <- t(back_tests)
rownames(back_tests) <- look_backs
# Plot forecasting series with legend
plot(x=look_backs, y=back_tests[, 1],
  xlab="look-back", ylab="MSE", type="l", lwd=2,
  main="MSE of AR(5) Forecasting Model")

# Define Ornstein-Uhlenbeck parameters
eq_price <- 1.0; sigma_r <- 0.02
the_ta <- 0.01; n_rows <- 1000
drif_t <- the_ta*eq_price
theta_1 <- 1-the_ta
# Simulate Ornstein-Uhlenbeck process
in_nov <- sigma_r*rnorm(n_rows)
price_s <- numeric(n_rows)
price_s[1] <- in_nov[1]
for (i in 2:n_rows) {
  price_s[i] <- theta_1*price_s[i-1] + in_nov[i] + drif_t
}  # end for

plot(price_s, type="l",
     xlab="time", ylab="prices",
     main="Ornstein-Uhlenbeck Process")
legend("topright",
 title=paste(c(paste0("sigma_r = ", sigma_r),
               paste0("eq_price = ", eq_price),
               paste0("the_ta = ", the_ta)),
             collapse="\n"),
 legend="", cex=0.8, inset=0.1, bg="white", bty="n")
abline(h=eq_price, col='red', lwd=2)

re_turns <- rutils::diff_it(price_s)
lag_prices <- rutils::lag_it(price_s)
for_mula <- re_turns ~ lag_prices
l_m <- lm(for_mula)
summary(l_m)
# Plot regression
plot(for_mula, main="OU Returns Versus Lagged Prices")
abline(l_m, lwd=2, col="red")

# Calculate volatility parameter
c(volatility=sigma_r, estimate=sd(re_turns))
# Extract OU parameters from regression
co_eff <- summary(l_m)$coefficients
# Calculate regression alpha and beta directly
be_ta <- cov(re_turns, lag_prices)/var(lag_prices)
al_pha <- (mean(re_turns) - be_ta*mean(lag_prices))
cbind(direct=c(alpha=al_pha, beta=be_ta), lm=co_eff[, 1])
all.equal(c(alpha=al_pha, beta=be_ta), co_eff[, 1],
    check.attributes=FALSE)
# Calculate regression standard errors directly
beta_s <- c(alpha=al_pha, beta=be_ta)
fit_ted <- (al_pha + be_ta*lag_prices)
residual_s <- (re_turns - fit_ted)
prices_squared <- sum((lag_prices - mean(lag_prices))^2)
beta_sd <- sqrt(sum(residual_s^2)/prices_squared/(n_rows-2))
alpha_sd <- sqrt(sum(residual_s^2)/(n_rows-2)*(1/n_rows + mean(lag_prices)^2/prices_squared))
cbind(direct=c(alpha_sd=alpha_sd, beta_sd=beta_sd), lm=co_eff[, 2])
all.equal(c(alpha_sd=alpha_sd, beta_sd=beta_sd), co_eff[, 2],
    check.attributes=FALSE)
# Compare mean reversion parameter theta
c(theta=(-the_ta), round(co_eff[2, ], 3))
# Compare equilibrium price mu
c(eq_price=eq_price, estimate=-co_eff[1, 1]/co_eff[2, 1])
# Compare actual and estimated parameters
co_eff <- cbind(c(the_ta*eq_price, -the_ta), co_eff[, 1:2])
rownames(co_eff) <- c("drift", "theta")
colnames(co_eff)[1] <- "actual"
round(co_eff, 4)

# Simulate Schwartz process
re_turns <- numeric(n_rows)
price_s <- numeric(n_rows)
price_s[1] <- eq_price
set.seed(1121)  # Reset random numbers
for (i in 2:n_rows) {
  re_turns[i] <- the_ta*(eq_price - price_s[i-1]) + sigma_r*rnorm(1)
  price_s[i] <- price_s[i-1] * exp(re_turns[i])
}  # end for

plot(price_s, type="l", xlab="time", ylab="prices",
     main="Schwartz Process")
legend("topright",
 title=paste(c(paste0("sigma_r = ", sigma_r),
               paste0("eq_price = ", eq_price),
               paste0("the_ta = ", the_ta)),
             collapse="\n"),
 legend="", cex=0.8, inset=0.12, bg="white", bty="n")
abline(h=eq_price, col='red', lwd=2)
