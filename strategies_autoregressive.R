# Load the roundtrip trades
dtable <- data.table::fread("/Users/jerzy/Develop/lecture_slides/data/roundtrip_trades.csv")
nrows <- NROW(dtable)
class(dtable$timefill)
# Sort the trades according to the execution time
dtable <- dtable[order(dtable$timefill)]
# Calculate the dollar bid-ask spread
pricebuy <- dtable$price[dtable$side == "buy"]
pricesell <- dtable$price[dtable$side == "sell"]
bidask <- mean(pricebuy-pricesell)
# Calculate the percentage bid-ask spread
bidask/mean(pricesell)
# Calculate the daily VTI percentage returns
retp <- na.omit(rutils::etfenv$returns$VTI)
# Calculate the autocorrelations of daily VTI returns
rutils::plot_acf(retp, lag=10, main="ACF of VTI returns")
# Simulate the contrarian strategy
posv <- -rutils::lagit(sign(retp), lagg=1)
pnls <- retp*posv
# Subtract transaction costs from the pnls
bidask <- 0.0001 # The bid-ask spread is equal to 1 basis point
costv <- 0.5*bidask*abs(rutils::diffit(posv))
pnls <- (pnls - costv)
# Calculate the strategy beta and alpha
betac <- cov(pnls, retp)/var(retp)
alphac <- mean(pnls) - betac*mean(retp)
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retp, pnls, (retp+pnls)/2)
colnames(wealthv) <- c("VTI", "AR_Strategy", "Combined")
cor(wealthv)
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of contrarian strategy
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="VTI Daily Contrarian Strategy") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dyLegend(show="always", width=300)
# Simulate contrarian strategy with two day holding period
posv <- -rutils::roll_sum(sign(retp), lookb=2)/2
pnls <- retp*rutils::lagit(posv)
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of contrarian strategy
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Daily Contrarian Strategy With Two Day Holding Period") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Load daily S&P500 stock returns
load(file="/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
retp <- na.omit(retstock$MSFT)
rutils::plot_acf(retp)
# Simulate contrarian strategy with two day holding period
posv <- -rutils::roll_sum(sign(retp), lookb=2)/2
pnls <- retp*rutils::lagit(posv)
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("MSFT", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of contrarian strategy
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Daily Contrarian Strategy For MSFT") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Simulate contrarian strategy for all S&P500 stocks
library(parallel)  # Load package parallel
ncores <- detectCores() - 1
pnll <- mclapply(retstock, function(retp) {
  retp <- na.omit(retp)
  posv <- -rutils::roll_sum(sign(retp), lookb=2)/2
  retp*rutils::lagit(posv)
}, mc.cores=ncores)  # end mclapply
pnls <- do.call(cbind, pnll)
pnls <- rowMeans(pnls, na.rm=TRUE)
# Calculate the average returns of all S&P500 stocks
datev <- zoo::index(retstock)
datev <- datev[-1]
indeks <- rowMeans(retstock, na.rm=TRUE)
indeks <- indeks[-1]
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(indeks, pnls)
wealthv <- xts::xts(wealthv, datev)
colnames(wealthv) <- c("All Stocks", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of contrarian strategy
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Daily Contrarian Strategy For All Stocks") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Calculate the stock volatilities
volv <- mclapply(retstock, function(retp) {
  sd(na.omit(retp))
}, mc.cores=ncores)  # end mclapply
volv <- do.call(c, volv)
# Calculate the median volatility
medianv <- median(volv)
# Calculate the pnls for low volatility stocks
pnlovol <- do.call(cbind, pnll[volv < medianv])
pnlovol <- rowMeans(pnlovol, na.rm=TRUE)
# Calculate the pnls for high volatility stocks
pnlhivol <- do.call(cbind, pnll[volv >= medianv])
pnlhivol <- rowMeans(pnlhivol, na.rm=TRUE)
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(pnlovol, pnlhivol)
wealthv <- xts::xts(wealthv, datev)
colnames(wealthv) <- c("Low Vol", "High Vol")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of contrarian strategy
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Contrarian Strategy For Low and High Volatility Stocks") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Calculate the VTI daily percentage returns
retp <- na.omit(rutils::etfenv$returns$VTI)
# Calculate the EMA returns recursively using C++ code
retma <- HighFreq::run_mean(retp, lambda=0.1)
# Calculate the positions and PnLs
posv <- -rutils::lagit(retma, lagg=1)
pnls <- retp*posv
# Subtract transaction costs from the pnls
bidask <- 0.0001 # The bid-ask spread is equal to 1 basis point
costv <- 0.5*bidask*abs(rutils::diffit(posv))
pnls <- (pnls - costv)
# Scale the PnL volatility to that of VTI
pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of contrarian strategy
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="VTI EMA Daily Contrarian Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Calculate the EMA returns and volatilities
volv <- HighFreq::run_var(retp, lambda=0.5)
retma <- volv[, 1]
volv <- sqrt(volv[, 2])
# Scale the returns by their trailing volatility
retsc <- retp/volv
# Calculate the positions and PnLs
posv <- -rutils::lagit(retsc, lagg=1)
pnls <- retp*posv
costv <- 0.5*bidask*abs(rutils::diffit(posv))
pnls <- (pnls - costv)
# Scale the PnL volatility to that of VTI
pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of contrarian strategy
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="VTI EMA Daily Contrarian Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Calculate the VTI daily percentage returns
retp <- na.omit(rutils::etfenv$returns$VTI)
nrows <- NROW(retp)
# Define the response and predictor matrices
respv <- retp
orderp <- 5
predm <- lapply(1:orderp, rutils::lagit, input=respv)
predm <- rutils::do_call(cbind, predm)
# Add constant column for intercept coefficient phi0
predm <- cbind(rep(1, nrows), predm)
colnames(predm) <- c("phi0", paste0("lag", 1:orderp))
# Calculate the fitted autoregressive coefficients
predinv <- MASS::ginv(predm)
coeff <- drop(predinv %*% respv)
# Calculate the in-sample forecasts of VTI (fitted values)
fcasts <- predm %*% coeff
# Plot the AR coefficients
names(coeff) <- colnames(predm)
barplot(coeff, xlab="", ylab="coeff", col="grey",
  main="Coefficients of AR Forecasting Model")
# Calculate the autoregressive strategy PnLs
pnls <- retp*fcasts
# costv <- 0.5*bidask*abs(rutils::diffit(posv))
# pnls <- (pnls - costv)
# Scale the PnL volatility to that of VTI
pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of the autoregressive strategy
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Autoregressive Strategy In-Sample") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Calculate the residuals (forecast errors)
resids <- (fcasts - respv)
# The residuals are orthogonal to the predictors and the forecasts
round(cor(resids, fcasts), 6)
round(sapply(predm[, -1], function(x) cor(resids, x)), 6)
# Calculate the variance of the residuals
varv <- sum(resids^2)/(nrows-NROW(coeff))
# Calculate the predictor matrix squared
pred2 <- crossprod(predm)
# Calculate the covariance matrix of the AR coefficients
covmat <- varv*MASS::ginv(pred2)
coefsd <- sqrt(diag(covmat))
# Calculate the t-values of the AR coefficients
coefft <- drop(coeff/coefsd)
names(coefft) <- colnames(predm)
# Plot the t-values of the AR coefficients
barplot(coefft, xlab="", ylab="t-value", col="grey",
  main="Coefficient t-values of AR Forecasting Model")
# Calculate the trailing volatility of the residuals
residv <- sqrt(HighFreq::run_var(resids, lambda=0.9)[, 2])
# Plot dygraph of volatility of residuals
datav <- cbind(cumsum(retp), residv)
colnames(datav) <- c("VTI", "residual vol")
endw <- rutils::calc_endpoints(datav, interval="weeks")
dygraphs::dygraph(datav[endw], main="Volatility of Residuals") %>%
  dyAxis("y", label="VTI", independentTicks=TRUE) %>%
  dyAxis("y2", label="residual vol", independentTicks=TRUE) %>%
  dySeries(name="VTI", axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name="residual vol", axis="y2", strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)
# Scale the forecasts by their volatility
fcastv <- sqrt(HighFreq::run_var(fcasts, lambda=0.4)[, 2])
fcasts <- ifelse(fcastv > mad(fcastv)/20, fcasts/fcastv, 0)
# Calculate the autoregressive strategy PnLs
pnls <- retp*fcasts
# costv <- 0.5*bidask*abs(rutils::diffit(posv))
# pnls <- (pnls - costv)
# Scale the PnL volatility to that of VTI
pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of the autoregressive strategy
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Autoregressive Strategy In-Sample") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Calculate the high volatility AR coefficients
respv <- retp["2008/2011"]
predm <- lapply(1:orderp, rutils::lagit, input=respv)
predm <- rutils::do_call(cbind, predm)
predm <- cbind(rep(1, NROW(predm)), predm)
predinv <- MASS::ginv(predm)
coeffh <- drop(predinv %*% respv)
names(coeffh) <- colnames(predm)
barplot(coeffh, main="High Volatility AR Coefficients",
  col="grey", xlab="", ylab="coefficient", ylim=c(-0.1, 0.05))
# Calculate the low volatility AR coefficients
respv <- retp["2012/2019"]
predm <- lapply(1:orderp, rutils::lagit, input=respv)
predm <- rutils::do_call(cbind, predm)
predm <- cbind(rep(1, NROW(predm)), predm)
predinv <- MASS::ginv(predm)
coeffl <- drop(predinv %*% respv)
names(coeffl) <- colnames(predm)
barplot(coeffl, main="Low Volatility AR Coefficients",
  xlab="", ylab="coefficient", ylim=c(-0.1, 0.05))
NA
# Calculate the pnls for the high volatility AR coefficients
predm <- lapply(1:orderp, rutils::lagit, input=retp)
predm <- rutils::do_call(cbind, predm)
predm <- cbind(rep(1, nrows), predm)
fcasts <- predm %*% coeffh
pnlh <- retp*fcasts
pnlh <- pnlh*sd(retp[retp<0])/sd(pnlh[pnlh<0])
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retp, pnlh)
colnames(wealthv) <- c("VTI", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of the autoregressive strategy
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Autoregressive Strategy High Volatility Coefficients") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Calculate the pnls for the low volatility AR coefficients
fcasts <- predm %*% coeffl
pnll <- retp*fcasts
pnll <- pnll*sd(retp[retp<0])/sd(pnll[pnll<0])
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retp, pnll)
colnames(wealthv) <- c("VTI", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of the autoregressive strategy
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Autoregressive Strategy Low Volatility Coefficients") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Define the response and predictor matrices
respv <- retp
predm <- lapply(1:orderp, rutils::lagit, input=respv)
predm <- rutils::do_call(cbind, predm)
predinv <- MASS::ginv(predm)
# Simulate strategy with high volatility AR coefficients
fcasts <- drop(predm %*% coeffh)
# Calculate the EMA of the squared residuals
resids <- (fcasts - respv)
residv <- HighFreq::run_mean(resids^2, lambda=0.9)
# Plot dygraph of volatility of residuals
datav <- cbind(cumsum(retp), sqrt(residv))
colnames(datav) <- c("VTI", "residuals")
dygraphs::dygraph(datav[endw], main="Volatility of high Residuals") %>%
  dyAxis("y", label="VTI", independentTicks=TRUE) %>%
  dyAxis("y2", label="residuals", independentTicks=TRUE) %>%
  dySeries(name="VTI", axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name="residuals", axis="y2", strokeWidth=2, col="red")
pnlh <- retp*fcasts
pnlh <- pnlh*sd(retp)/sd(pnlh)
# Simulate strategy with low volatility AR coefficients
fcasts <- drop(predm %*% coeffl)
# Calculate the EMA of the squared residuals
resids <- (fcasts - respv)
residv <- HighFreq::run_mean(resids^2, lambda=0.9)
# Plot dygraph of volatility of residuals
datav <- cbind(cumsum(retp), sqrt(residv))
colnames(datav) <- c("VTI", "residuals")
dygraphs::dygraph(datav[endw], main="Volatility of low Residuals") %>%
  dyAxis("y", label="VTI", independentTicks=TRUE) %>%
  dyAxis("y2", label="residuals", independentTicks=TRUE) %>%
  dySeries(name="VTI", axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name="residuals", axis="y2", strokeWidth=2, col="red")
pnll <- retp*fcasts
pnll <- pnll*sd(retp)/sd(pnll)
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of the autoregressive strategy
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Autoregressive Strategy In-Sample") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
lambdav <- c(0.5, 1, 1.5)
colorv <- c("red", "blue", "green")
# Define the winsor function
winsorfun <- function(retp, lambdaf) tanh(lambdaf*retp)
# Plot three curves in loop
for (indeks in 1:3) {
  curve(expr=winsorfun(x, lambda=lambdav[indeks]),
xlim=c(-4, 4), type="l", lwd=4,
xlab="model weight", ylab="dollar amount",
col=colorv[indeks], add=(indeks>1))
}  # end for
# Add title and legend
title(main="Winsor function", line=0.5)
legend("topleft", title="scale parameters\n",
   paste("lambdaf", lambdav, sep="="), inset=0.0, cex=1.0,
   lwd=6, bty="n", y.intersp=0.3, lty=1, col=colorv)
# Winsorize the VTI returns
retw <- winsorfun(retp/0.01, lambda=0.1)
# Define the response and predictor matrices
predm <- lapply(1:orderp, rutils::lagit, input=retw)
predm <- rutils::do_call(cbind, predm)
predm <- cbind(rep(1, nrows), predm)
colnames(predm) <- c("phi0", paste0("lag", 1:orderp))
predinv <- MASS::ginv(predm)
coeff <- predinv %*% retw
# Calculate the scaled in-sample forecasts of VTI
fcasts <- predm %*% coeff
# Winsorize the forecasts
# fcasts <- winsorfun(fcasts/mad(fcasts), lambda=1.5)
# Calculate the autoregressive strategy PnLs
pnls <- retp*fcasts
# Scale the PnL volatility to that of VTI
pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of the autoregressive strategy
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Winsorized Autoregressive Strategy In-Sample") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Scale the returns by their trailing volatility
varv <- HighFreq::run_var(retp, lambda=0.99)[, 2]
retsc <- retp/sqrt(varv)
# Calculate the AR coefficients
predm <- lapply(1:orderp, rutils::lagit, input=retsc)
predm <- rutils::do_call(cbind, predm)
predm <- cbind(rep(1, nrows), predm)
colnames(predm) <- c("phi0", paste0("lag", 1:orderp))
predinv <- MASS::ginv(predm)
coeff <- predinv %*% retsc
# Calculate the scaled in-sample forecasts of VTI
fcasts <- predm %*% coeff
# Calculate the autoregressive strategy PnLs
pnls <- retp*fcasts
# Scale the PnL volatility to that of VTI
pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of the autoregressive strategy
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Autoregressive Strategy With Returns Scaled By Volatility") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Calculate VTI returns and trading volumes
ohlc <- rutils::etfenv$VTI
datev <- zoo::index(ohlc)
nrows <- NROW(ohlc)
closep <- quantmod::Cl(ohlc)
colnames(closep) <- "VTI"
retp <- rutils::diffit(log(closep))
volumv <- quantmod::Vo(ohlc)
# Scale the returns using volume clock to trading time
volumr <- HighFreq::run_mean(volumv, lambda=0.8)
respv <- retp*volumr/volumv
# Calculate the AR coefficients
orderp <- 5
predm <- lapply(1:orderp, rutils::lagit, input=respv)
predm <- rutils::do_call(cbind, predm)
predm <- cbind(rep(1, nrows), predm)
colnames(predm) <- c("phi0", paste0("lag", 1:orderp))
predinv <- MASS::ginv(predm)
coeff <- drop(predinv %*% respv)
# Calculate the scaled in-sample forecasts of VTI
fcasts <- predm %*% coeff
# Calculate the autoregressive strategy PnLs
pnls <- retp*fcasts
# Scale the PnL volatility to that of VTI
pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
sqrt(252)*sapply(wealthv["2010/"], function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of the autoregressive strategy
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Autoregressive Strategy With Returns in Trading Time") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Define the response and predictor matrices
respv <- retp
orderp <- 5
predm <- lapply(1:orderp, rutils::lagit, input=respv)
predm <- rutils::do_call(cbind, predm)
predm <- cbind(rep(1, nrows), predm)
colnames(predm) <- c("phi0", paste0("lag", 1:orderp))
# Calculate the AR coefficients and the in-sample forecasts
predinv <- MASS::ginv(predm)
coeff <- drop(predinv %*% respv)
names(coeff) <- colnames(predm)
fcasts <- predm %*% coeff
# Calculate the correlation between forecasts and returns
cor(fcasts, respv)
# Calculate the volatilities of the returns and forecasts
sd(respv); sd(fcasts)
# Calculate the mean squared error of the forecasts
mean((fcasts - respv)^2)
# Plot the forecasts
datav <- cbind(respv, fcasts)["2020-01/2020-06"]
colnames(datav) <- c("returns", "forecasts")
dygraphs::dygraph(datav, main="VTI Returns And Forecasts") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Objective function for the in-sample AR coefficients
objfun <- function(coeff) {
  fcasts <- predm %*% coeff
  sum((respv - fcasts)^2)
}  # end objfun
# Perform optimization using the quasi-Newton method
optiml <- optim(par=numeric(orderp+1),
          fn=objfun,
          method="L-BFGS-B",
          upper=rep(10, orderp+1),
          lower=rep(-10, orderp+1))
# Extract the AR coefficients
coeffopt <- optiml$par
names(coeffopt) <- colnames(predm)
all.equal(coeffopt, coeff)
barplot(coeffopt, xlab="", ylab="coeff", col="grey",
  main="AR Coefficients From Optimization")
# Calculate the forecasts as a function of the AR order
fcasts <- lapply(2:NCOL(predm), function(ordern) {
  # Calculate the fitted AR coefficients
  predinv <- MASS::ginv(predm[, 1:ordern])
  coeff <- predinv %*% respv
  coeff <- coeff/sqrt(sum(coeff^2))
  # Calculate the in-sample forecasts of VTI
  drop(predm[, 1:ordern] %*% coeff)
})  # end lapply
names(fcasts) <- paste0("n=", 2:NCOL(predm))
# Calculate the mean squared errors
mse <- sapply(fcasts, function(x) {
  c(mse=mean((respv - x)^2), cor=cor(respv, x))
})  # end sapply
mse <- t(mse)
rownames(mse) <- names(fcasts)
# Plot forecasting MSE
plot(x=2:NCOL(predm), y=mse[, 1],
  xlab="AR(n) order", ylab="MSE", type="l", lwd=2,
  main="MSE of In-sample AR(n) Forecasting Model for VTI")
# Define in-sample and out-of-sample intervals
nrows <- NROW(retp)
insample <- 1:(nrows %/% 2)
outsample <- (nrows %/% 2 + 1):nrows
# Calculate the forecasts as a function of the AR order
fcastl <- lapply(2:NCOL(predm), function(ordern) {
  # Calculate the in-sample AR coefficients
  predinv <- MASS::ginv(predm[insample, 1:ordern])
  coeff <- predinv %*% respv[insample]
  coeff <- coeff/sqrt(sum(coeff^2))
  # Calculate the out-of-sample forecasts of VTI
  predm[outsample, 1:ordern] %*% coeff
})  # end lapply
names(fcastl) <- paste0("n=", 2:NCOL(predm))
# Calculate the mean squared errors
mse <- sapply(fcastl, function(x) {
  c(mse=mean((respv[outsample] - x)^2), cor=cor(respv[outsample], x))
})  # end sapply
mse <- t(mse)
rownames(mse) <- names(fcastl)
# Plot forecasting MSE
plot(x=2:NCOL(predm), y=mse[, 1],
  xlab="AR(n) order", ylab="MSE", type="l", lwd=2,
  main="MSE of Out-of-sample AR(n) Forecasting Model for VTI")
# Calculate the out-of-sample PnLs
pnls <- lapply(fcastl, function(fcasts) {
  pnls <- fcasts*retp[outsample]
  pnls*sd(retp[retp<0])/sd(pnls[pnls<0])
})  # end lapply
pnls <- rutils::do_call(cbind, pnls)
colnames(pnls) <- names(fcastl)
# Plot dygraph of out-of-sample PnLs
colorv <- colorRampPalette(c("red", "blue"))(NCOL(pnls))
colv <- colnames(pnls)
sqrt(252)*sapply(pnls, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
endw <- rutils::calc_endpoints(pnls, interval="weeks")
dygraphs::dygraph(cumsum(pnls)[endw],
  main="Autoregressive Strategies Out-of-sample") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(width=300)
# Define the look-back range
lookb <- 100
tday <- nrows
startp <- max(1, tday-lookb)
rangev <- startp:(tday-1)
# Subset the response and predictors
resps <- respv[rangev]
preds <- predm[rangev]
# Invert the predictor matrix
predinv <- MASS::ginv(preds)
# Calculate the fitted AR coefficients
coeff <- predinv %*% resps
# Calculate the in-sample forecasts of VTI (fitted values)
fcasts <- preds %*% coeff
# Calculate the residuals (forecast errors)
resids <- (fcasts - resps)
# Calculate the variance of the residuals
varv <- sum(resids^2)/(NROW(preds)-NROW(coeff))
# Calculate the predictor matrix squared
pred2 <- crossprod(preds)
# Calculate the covariance matrix of the AR coefficients
covmat <- varv*MASS::ginv(pred2)
coefsd <- sqrt(diag(covmat))
# Calculate the t-values of the AR coefficients
coefft <- drop(coeff/coefsd)
# Calculate the out-of-sample forecast
predn <- predm[tday, ]
fcast <- drop(predn %*% coeff)
# Calculate the variance of the forecast
varf <- drop(predn %*% covmat %*% t(predn))
# Calculate the t-value of the out-of-sample forecast
fcast/sqrt(varf)
# Perform rolling forecasting
lookb <- 500
fcasts <- parallel::mclapply(1:nrows, function(tday) {
  if (tday > lookb) {
    # Define the rolling look-back range
    startp <- max(1, tday-lookb)
    # startp <- 1 # Expanding look-back range
    rangev <- startp:(tday-1) # In-sample range
    # Subset the response and predictors
    resps <- respv[rangev]
    preds <- predm[rangev]
    # Calculate the fitted AR coefficients
    predinv <- MASS::ginv(preds)
    coeff <- predinv %*% resps
    # Calculate the in-sample forecasts of VTI (fitted values)
    fcasts <- preds %*% coeff
    # Calculate the residuals (forecast errors)
    resids <- (fcasts - resps)
    # Calculate the variance of the residuals
    varv <- sum(resids^2)/(NROW(preds)-NROW(coeff))
    # Calculate the covariance matrix of the AR coefficients
    pred2 <- crossprod(preds)
    covmat <- varv*MASS::ginv(pred2)
    coefsd <- sqrt(diag(covmat))
    coefft <- drop(coeff/coefsd) # t-values of the AR coefficients
    # Calculate the out-of-sample forecast
    predn <- predm[tday, ]
    fcast <- drop(predn %*% coeff)
    # Calculate the variance of the forecast
    varf <- drop(predn %*% covmat %*% t(predn))
    return(c(sd(resps), fcast=fcast, fstderr=sqrt(varf), coefft=coefft))
  } else {
    return(c(volv=0, fcast=0, fstderr=0, coefft=rep(0, NCOL(predm))))
  } # end if
})  # end sapply
# Coerce fcasts to a time series
fcasts <- do.call(rbind, fcasts)
ncols <- NCOL(fcasts)
colnames(fcasts) <- c("volv", "fcasts", "fstderr", colnames(predm))
fcasts <- xts::xts(fcasts[, "fcasts"], zoo::index(retp))
# Calculate the strategy PnLs
pnls <- retp*fcasts
# Scale the PnL volatility to that of VTI
pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of the autoregressive strategy
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Rolling Autoregressive Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Define backtesting function
sim_fcasts <- function(lookb=100, ordern=5, fixedlb=TRUE) {
  # Perform rolling forecasting
  fcasts <- sapply((lookb+1):nrows, function(tday) {
    # Rolling look-back range
    startp <- max(1, tday-lookb)
    # Expanding look-back range
    if (!fixedlb) {startp <- 1}
    startp <- max(1, tday-lookb)
    rangev <- startp:(tday-1) # In-sample range
    # Subset the response and predictors
    resps <- respv[rangev]
    preds <- predm[rangev, 1:ordern]
    # Invert the predictor matrix
    predinv <- MASS::ginv(preds)
    # Calculate the fitted AR coefficients
    coeff <- predinv %*% resps
    # Calculate the out-of-sample forecast
    drop(predm[tday, 1:ordern] %*% coeff)
  })  # end sapply
  # Add warmup period
  fcasts <- c(rep(0, lookb), fcasts)
}  # end sim_fcasts
# Simulate the rolling autoregressive forecasts
fcasts <- sim_fcasts(lookb=100, ordern=5)
c(mse=mean((fcasts - retp)^2), cor=cor(retp, fcasts))
library(parallel)  # Load package parallel
# Calculate the number of available cores
ncores <- detectCores() - 1
# Initialize compute cluster under Windows
compclust <- makeCluster(ncores)
# Perform parallel loop under Windows
lookbv <- seq(20, 600, 40)
fcasts <- parLapply(compclust, lookbv, sim_fcasts, ordern=6)
# Perform parallel bootstrap under Mac-OSX or Linux
fcasts <- mclapply(lookbv, sim_fcasts, ordern=6, mc.cores=ncores)
# Calculate the mean squared errors
mse <- sapply(fcasts, function(x) {
  c(mse=mean((retp - x)^2), cor=cor(retp, x))
})  # end sapply
mse <- t(mse)
rownames(mse) <- lookbv
# Select optimal lookb interval
lookb <- lookbv[which.min(mse[, 1])]
# Plot forecasting MSE
plot(x=lookbv, y=mse[, 1],
  xlab="look-back", ylab="MSE", type="l", lwd=2,
  main="MSE of AR Forecasting Model As Function of Look-back")
library(parallel)  # Load package parallel
# Calculate the number of available cores
ncores <- detectCores() - 1
# Initialize compute cluster under Windows
compclust <- makeCluster(ncores)
# Perform parallel loop under Windows
orderv <- 2:6
fcasts <- parLapply(compclust, orderv, sim_fcasts, lookb=lookb)
stopCluster(compclust)  # Stop R processes over cluster under Windows
# Perform parallel bootstrap under Mac-OSX or Linux
fcasts <- mclapply(orderv, sim_fcasts,
  lookb=lookb, mc.cores=ncores)
# Calculate the mean squared errors
mse <- sapply(fcasts, function(x) {
  c(mse=mean((retp - x)^2), cor=cor(retp, x))
})  # end sapply
mse <- t(mse)
rownames(mse) <- orderv
# Select optimal order parameter
ordern <- orderv[which.min(mse[, 1])]
# Plot forecasting MSE
plot(x=orderv, y=mse[, 1],
  xlab="AR order", ylab="MSE", type="l", lwd=2,
  main="MSE of Forecasting Model As Function of AR Order")
# Simulate the rolling autoregressive forecasts
fcasts <- sim_fcasts(lookb=lookb, ordern=ordern)
# Calculate the strategy PnLs
pnls <- fcasts*retp
# Scale the PnL volatility to that of VTI
pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])
wealthv <- cbind(retp, pnls, (retp+pnls)/2)
colnames(wealthv) <- c("VTI", "AR_Strategy", "Combined")
cor(wealthv)
# Annualized Sharpe ratios of VTI and AR strategy
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of AR strategy combined with VTI
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Autoregressive Strategy Fixed Look-back") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", strokeWidth=3) %>%
  dyLegend(show="always", width=300)
library(parallel)  # Load package parallel
# Calculate the number of available cores
ncores <- detectCores() - 1
# Initialize compute cluster under Windows
compclust <- makeCluster(ncores)
# Perform parallel loop under Windows
orderv <- 2:6
fcasts <- parLapply(compclust, orderv, sim_fcasts,
  lookb=lookb, fixedlb=FALSE)
stopCluster(compclust)  # Stop R processes over cluster under Windows
# Perform parallel bootstrap under Mac-OSX or Linux
fcasts <- mclapply(orderv, sim_fcasts,
  lookb=lookb, fixedlb=FALSE, mc.cores=ncores)
# Calculate the mean squared errors
mse <- sapply(fcasts, function(x) {
  c(mse=mean((retp - x)^2), cor=cor(retp, x))
})  # end sapply
mse <- t(mse)
rownames(mse) <- orderv
# Select optimal order parameter
ordern <- orderv[which.min(mse[, 1])]
# Plot forecasting MSE
plot(x=orderv, y=mse[, 1],
  xlab="AR order", ylab="MSE", type="l", lwd=2,
  main="MSE With Expanding Look-back As Function of AR Order")
# Simulate the autoregressive forecasts with expanding look-back
fcasts <- sim_fcasts(lookb=lookb, ordern=ordern, fixedlb=FALSE)
# Calculate the strategy PnLs
pnls <- fcasts*retp
# Scale the PnL volatility to that of VTI
pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])
wealthv <- cbind(retp, pnls, (retp+pnls)/2)
colnames(wealthv) <- c("VTI", "AR_Strategy", "Combined")
cor(wealthv)
# Annualized Sharpe ratios of VTI and AR strategy
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of AR strategy combined with VTI
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Autoregressive Strategy Expanding Look-back") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", strokeWidth=3) %>%
  dyLegend(show="always", width=300)
# Calculate VTI returns over non-overlapping 2-day intervals
pricev <- na.omit(rutils::etfenv$prices$VTI)
reta <- rutils::diffit(log(pricev), lag=2)
reta <- reta[2*(1:(NROW(pricev) %/% 2))]
# Calculate the autocorrelations of daily VTI returns
rutils::plot_acf(reta, lag=10, main="ACF of Aggregated 2-day VTI returns")
# Define the response and predictor matrices
reta <- rutils::diffit(log(pricev), lag=2)/2
orderp <- 5
predm <- lapply(2*(1:orderp), rutils::lagit, input=reta)
predm <- rutils::do_call(cbind, predm)
predm <- cbind(rep(1, NROW(reta)), predm)
colnames(predm) <- c("phi0", paste0("lag", 1:orderp))
# Calculate the AR coefficients
predinv <- MASS::ginv(predm)
coeff <- predinv %*% reta
coeffn <- paste0("phi", 0:(NROW(coeff)-1))
barplot(coeff ~ coeffn, xlab="", ylab="t-value", col="grey",
  main="Coefficients of AR Forecasting Model")
# Calculate the in-sample forecasts of VTI (fitted values)
fcasts <- predm %*% coeff
fcastv <- sqrt(HighFreq::run_var(fcasts, lambda=0.4)[, 2])
fcasts <- ifelse(fcastv > mad(fcastv)/20, fcasts/fcastv, 0)
# Calculate the autoregressive strategy PnLs
pnls <- reta*fcasts
pnls <- pnls*sd(reta[reta<0])/sd(pnls[pnls<0])
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(reta, pnls, (reta+pnls)/2)
colnames(wealthv) <- c("VTI", "AR_Strategy", "Combined")
cor(wealthv)
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
sqrt(252)*sapply(wealthv["2010/"], function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of the autoregressive strategy
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Autoregressive Strategy With Aggregated Stock Returns") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dyLegend(show="always", width=300)
# Calculate the log of OHLC VTI prices
ohlc <- log(rutils::etfenv$VTI)
nrows <- NROW(ohlc)
openp <- quantmod::Op(ohlc)
highp <- quantmod::Hi(ohlc)
lowp <- quantmod::Lo(ohlc)
closep <- quantmod::Cl(ohlc)
# Calculate the close-to-close log returns,
# the daytime open-to-close returns
# and the overnight close-to-open returns.
retp <- rutils::diffit(closep)
colnames(retp) <- "daily"
retd <- (closep - openp)
colnames(retd) <- "daytime"
reton <- (openp - rutils::lagit(closep, lagg=1, pad_zeros=FALSE))
colnames(reton) <- "overnight"
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retp, reton, retd)
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of the Daytime and Overnight strategies
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Wealth of Close-to-Close, Overnight, and Daytime Strategies") %>%
  dySeries(name="daily", strokeWidth=2, col="blue") %>%
  dySeries(name="overnight", strokeWidth=2, col="red") %>%
  dySeries(name="daytime", strokeWidth=2, col="green") %>%
  dyLegend(width=600)
# Calculate the autocorrelations of daytime and overnight returns
pacfl <- pacf(retd, lag.max=10, plot=FALSE)
sum(pacfl$acf)
pacfl <- pacf(reton, lag.max=10, plot=FALSE)
sum(pacfl$acf)
# Calculate the EMA returns recursively using C++ code
retma <- HighFreq::run_mean(retd, lambda=0.4)
# Calculate the positions and PnLs
posv <- -rutils::lagit(sign(retma), lagg=1)
pnls <- retd*posv
# Calculate the pnls and the transaction costs
bidask <- 0.0001 # The bid-ask spread is equal to 1 basis point
costv <- 0.5*bidask*abs(rutils::diffit(posv))
pnls <- (pnls - costv)
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retd, pnls)
colnames(wealthv) <- c("VTI daytime", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
+ c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of crossover strategy
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Mean-Reversion Strategy For Daytime VTI Returns") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Calculate the z-scores of the cumulative daytime returns
retc <- cumsum(retd)
lambdaf <- 0.24
retm <- rutils::lagit(HighFreq::run_mean(retc, lambda=lambdaf))
retv <- sqrt(rutils::lagit(HighFreq::run_var(retc, lambda=lambdaf)[, 2]))
zscores <- ifelse(retv > 0, (retc - retm)/retv, 0)
# Calculate the positions from the Bollinger z-scores
posv <- rep(NA_integer_, nrows)
posv[1] <- 0
posv <- ifelse(zscores > 1, -1, posv)
posv <- ifelse(zscores < -1, 1, posv)
posv <- zoo::na.locf(posv)
posv <- rutils::lagit(posv, lagg=1)
# Calculate the pnls and the transaction costs
pnls <- retd*posv
costv <- 0.5*bidask*abs(rutils::diffit(posv))
pnls <- (pnls - costv)
# Calculate the Sharpe ratios
wealthv <- cbind(retd, pnls)
colnames(wealthv) <- c("VTI daytime", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of daytime Bollinger strategy
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Bollinger strategy For Daytime VTI Returns") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Calculate the pnls and the transaction costs
posv <- sign(reton)
pnls <- posv*retd
costv <- 0.5*bidask*abs(rutils::diffit(posv))
pnls <- (pnls - costv)
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retd, pnls)
colnames(wealthv) <- c("VTI daytime", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of crossover strategy
dygraphs::dygraph(cumsum(wealthv)[endw],
main="Overnight Trend For Daytime VTI Returns") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
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
# Calculate the autoregressive strategy PnLs
pnls <- retp*fcasts
pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])
# Calculate the in-sample and out-of-sample Sharpe and Sortino ratios
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "ARMultiFact")
sqrt(252)*sapply(wealthv[insample, ], function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
sqrt(252)*sapply(wealthv[outsample, ], function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of the autoregressive strategies
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
colorv <- colorRampPalette(c("blue", "red"))(NCOL(wealthv))
dygraphs::dygraph(cumsum(wealthv)[endw],
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
fcastv <- sqrt(HighFreq::run_var(fcasts, lambda=0.4)[, 2])
fcasts <- ifelse(fcastv > mad(fcastv)/20, fcasts/fcastv, 0)
pnls <- retp*fcasts
pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])
# Calculate the in-sample and out-of-sample Sharpe and Sortino ratios
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "ARMultiFact")
sqrt(252)*sapply(wealthv[insample, ], function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
sqrt(252)*sapply(wealthv[outsample, ], function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of the autoregressive strategies
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Multifactor Autoregressive Strategy Using t-Values") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
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
colnames(coeffm) <- paste0("dim=", dimv)
colorv <- colorRampPalette(c("red", "blue"))(NCOL(coeffm))
matplot(y=coeffm, type="l", lty="solid", lwd=1, col=colorv,
  xlab="predictor", ylab="coeff",
  main="AR Coefficients For Different Dimensions")
# Calculate the forecasts of VTI
fcasts <- predm %*% coeffm
fcasts <- apply(fcasts, 2, function(x) {
  fcastv <- sqrt(HighFreq::run_var(matrix(x), lambda=0.8)[, 2])
  fcastv[1:10] <- 1 # Warmup
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
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
colorv <- colorRampPalette(c("blue", "red"))(NCOL(wealthv))
dygraphs::dygraph(cumsum(wealthv)[endw],
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
          fn=objfun, lambdaf=50.0,
          method="L-BFGS-B",
          upper=rep(10, orderp+1),
          lower=rep(-10, orderp+1))
# Extract the shrinkage coefficients
coeff <- optiml$par
names(coeff) <- colnames(predm)
# Plot the AR coefficients
barplot(coeff, xlab="", ylab="coeff", col="grey",
  main="AR Coefficients With Shrinkage")
# Plot the shrinkage coefficients and the t-values of the coefficients
plot(coeff/sqrt(sum(coeff^2)), xlab="", ylab="coeff",
     col="blue", lwd=2, t="l",
     main="AR Coefficients With Shrinkage")
lines(coefft/sqrt(sum(coefft^2)), xlab="", ylab="coeff",
     col="red", lwd=2)
legend("topright", c("shrink", "t-value"),
 inset=0.05, col=c("blue", "red"), lwd=6, bty="n")
# Calculate the forecasts of VTI (fitted values)
fcasts <- predm %*% coeff
fcastv <- sqrt(HighFreq::run_var(fcasts, lambda=0.4)[, 2])
fcasts <- ifelse(fcastv > mad(fcastv)/20, fcasts/fcastv, 0)
# Calculate the autoregressive strategy PnLs
pnls <- retp*fcasts
pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])
# Calculate the in-sample and out-of-sample Sharpe and Sortino ratios
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "ARMultiFact")
sqrt(252)*sapply(wealthv[insample, ], function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
sqrt(252)*sapply(wealthv[outsample, ], function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of the autoregressive strategies
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
colorv <- colorRampPalette(c("blue", "red"))(NCOL(wealthv))
dygraphs::dygraph(cumsum(wealthv)[endw],
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
colnames(predm) <- c("phi0", paste0("VTI", 1:orderp))
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
barplot(coeff, xlab="", ylab="coeff", col="grey",
  main="Coefficients of Kitchen Sink Autoregressive Model")
# Calculate the in-sample forecasts of VTI (fitted values)
fcasts <- predm %*% coeff
# fcastv <- sqrt(HighFreq::run_var(fcasts, lambda=0.4)[, 2])
# fcasts <- ifelse(fcastv > mad(fcastv)/20, fcasts/fcastv, 0)
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
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
colorv <- colorRampPalette(c("blue", "red"))(NCOL(wealthv))
dygraphs::dygraph(cumsum(wealthv)[endw],
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
colnames(coeffm) <- paste0("dim=", dimv)
colorv <- colorRampPalette(c("red", "blue"))(NCOL(coeffm))
matplot(y=coeffm, type="l", lty="solid", lwd=1, col=colorv,
  xlab="predictor", ylab="coeff",
  main="AR Coefficients For Different Dimensions")
# Calculate the forecasts of VTI
fcasts <- predm %*% coeffm
fcasts <- apply(fcasts, 2, function(x) {
  fcastv <- sqrt(HighFreq::run_var(matrix(x), lambda=0.8)[, 2])
  fcastv[1:10] <- 1 # Warmup
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
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
colorv <- colorRampPalette(c("blue", "red"))(NCOL(wealthv))
dygraphs::dygraph(cumsum(wealthv)[endw],
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
          lambdaf=1.0,
          method="L-BFGS-B",
          upper=rep(10, ncoeff),
          lower=rep(-10, ncoeff))
# Extract the AR coefficients
coeff <- optiml$par
names(coeff) <- colnames(predm)
barplot(coeff, xlab="", ylab="coeff", col="grey",
  main="AR Coefficients With Shrinkage")
# Calculate the forecasts of VTI (fitted values)
fcasts <- predm %*% coeff
fcastv <- sqrt(HighFreq::run_var(fcasts, lambda=0.4)[, 2])
fcasts <- ifelse(fcastv > mad(fcastv)/20, fcasts/fcastv, 0)
# Calculate the autoregressive strategy PnLs
pnls <- respv*fcasts
pnls <- pnls*sd(respv[respv<0])/sd(pnls[pnls<0])
# Calculate the in-sample and out-of-sample Sharpe and Sortino ratios
wealthv <- cbind(respv, pnls)
colnames(wealthv) <- c("VTI", "KitSink")
sqrt(252)*sapply(wealthv[insample, ], function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
sqrt(252)*sapply(wealthv[outsample, ], function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of the autoregressive strategies
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
colorv <- colorRampPalette(c("blue", "red"))(NCOL(wealthv))
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Multifactor Autoregressive Strategy With Shrinkage") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyEvent(datev[cutoff], label="cutoff", strokePattern="solid", color="red") %>%
  dyLegend(show="always", width=300)
# Calculate the VTI daily percentage returns
retp <- na.omit(rutils::etfenv$returns[, c("VTI", "VXX", "SVXY")])
nrows <- NROW(retp)
# Define the response and predictor matrices
respv <- retp["/2019", "VTI"]
orderp <- 3
predm <- lapply(1:orderp, rutils::lagit, input=retp["/2019", c("VXX", "SVXY")])
predm <- rutils::do_call(cbind, predm)
predm <- cbind(rep(1, NROW(predm)), predm)
colnames(predm) <- c("phi0", paste0(c("VXX", "SVXY"), rep(1:orderp, each=2)))
# Calculate the fitted autoregressive coefficients
predinv <- MASS::ginv(predm)
coeff <- predinv %*% respv
# Calculate the in-sample forecasts of VTI (fitted values)
fcasts <- predm %*% coeff
# Calculate the residuals (forecast errors)
resids <- (fcasts - respv)
# The residuals are orthogonal to the predictors and the forecasts
round(cor(resids, fcasts), 6)
round(sapply(predm[, -1], function(x) cor(resids, x)), 6)
# Calculate the fitted autoregressive coefficients
predinv <- MASS::ginv(predm)
coeff <- predinv %*% respv
sum(coeff[1:orderp])
# Calculate the in-sample forecasts of VTI (fitted values)
fcasts <- predm %*% coeff
# Plot the AR coefficients
coeffn <- paste0("phi", 0:(NROW(coeff)-1))
barplot(coeff ~ coeffn, xlab="", ylab="coeff", col="grey",
  main="Coefficients of AR Forecasting Model")
# Calculate the residuals (forecast errors)
resids <- (fcasts - respv)
# The residuals are orthogonal to the predictors and the forecasts
round(cor(resids, fcasts), 6)
round(sapply(predm[, -1], function(x) cor(resids, x)), 6)
# Calculate the variance of the residuals
varv <- sum(resids^2)/(nrows-NROW(coeff))
# Calculate the predictor matrix squared
pred2 <- crossprod(predm)
# Calculate the covariance matrix of the AR coefficients
covmat <- varv*MASS::ginv(pred2)
coefsd <- sqrt(diag(covmat))
# Calculate the t-values of the AR coefficients
coefft <- drop(coeff/coefsd)
coeffn <- paste0("phi", 0:(NROW(coeff)-1))
# Plot the t-values of the AR coefficients
barplot(coefft ~ coeffn, xlab="", ylab="t-value", col="grey",
  main="Coefficient t-values of AR Forecasting Model")
# Calculate the trailing volatility of the residuals
residv <- sqrt(HighFreq::run_var(resids, lambda=0.9)[, 2])
# Plot dygraph of volatility of residuals
datav <- cbind(cumsum(retp), residv)
colnames(datav) <- c("VTI", "residual vol")
endw <- rutils::calc_endpoints(datav, interval="weeks")
dygraphs::dygraph(datav[endw], main="Volatility of Residuals") %>%
  dyAxis("y", label="VTI", independentTicks=TRUE) %>%
  dyAxis("y2", label="residual vol", independentTicks=TRUE) %>%
  dySeries(name="VTI", axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name="residual vol", axis="y2", strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)
# Calculate the kitchen sink strategy in-sample
pnls <- retp*fcasts
pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of the autoregressive strategy
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Autoregressive Strategy In-Sample") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Define in-sample and out-of-sample intervals
nrows <- NROW(retp)
cutoff <- nrows %/% 2
datev[cutoff]
insample <- 1:cutoff
outsample <- (cutoff + 1):nrows
# Calculate the optimal AR coefficients
predinv <- MASS::ginv(predm[insample, ])
coeff <- drop(predinv %*% respv[insample])
# Calculate the strategy PnLs
fcasts <- predm %*% coeff
pnls <- sign(fcasts)*retp
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
colv <- colnames(wealthv)
# Calculate the in-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv[insample, ],
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv[outsample, ],
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Kitchen Sink Strategy Out-of-sample") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=colv[2], axis="y2", col="red", strokeWidth=2) %>%
  dyEvent(datev[cutoff], label="cutoff", strokePattern="solid", color="red") %>%
  dyLegend(show="always", width=300)
# Define in-sample and out-of-sample intervals
nrows <- NROW(retp)
cutoff <- nrows %/% 2
datev[cutoff]
insample <- 1:cutoff
outsample <- (cutoff + 1):nrows
# Calculate reduced inverse of the predictor matrix from SVD
svdec <- svd(predm[insample, ])
dimax <- 2
predinv <- svdec$v[, 1:dimax] %*%
  (t(svdec$u[, 1:dimax]) / svdec$d[1:dimax])
coeff <- drop(predinv %*% respv[insample])
# Calculate the strategy PnLs
fcasts <- predm %*% coeff
pnls <- sign(fcasts)*retp
pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
colv <- colnames(wealthv)
# Calculate the in-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv[insample, ],
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv[outsample, ],
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Kitchen Sink Strategy Out-of-sample") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=colv[2], axis="y2", col="red", strokeWidth=2) %>%
  dyEvent(datev[cutoff], label="cutoff", strokePattern="solid", color="red") %>%
  dyLegend(show="always", width=300)
# Define the look-back range
lookb <- 100
tday <- nrows
startp <- max(1, tday-lookb)
rangev <- startp:(tday-1)
# Subset the response and predictors
resps <- respv[rangev]
preds <- predm[rangev]
# Invert the predictor matrix
predinv <- MASS::ginv(preds)
# Calculate the fitted AR coefficients
coeff <- predinv %*% resps
# Calculate the in-sample forecasts of VTI (fitted values)
fcasts <- preds %*% coeff
# Calculate the residuals (forecast errors)
resids <- (fcasts - resps)
# Calculate the variance of the residuals
varv <- sum(resids^2)/(NROW(preds)-NROW(coeff))
# Calculate the predictor matrix squared
pred2 <- crossprod(preds)
# Calculate the covariance matrix of the AR coefficients
covmat <- varv*MASS::ginv(pred2)
coefsd <- sqrt(diag(covmat))
# Calculate the t-values of the AR coefficients
coefft <- drop(coeff/coefsd)
# Calculate the out-of-sample forecast
predn <- predm[tday, ]
fcast <- drop(predn %*% coeff)
# Calculate the variance of the forecast
varf <- drop(predn %*% covmat %*% t(predn))
# Calculate the t-value of the out-of-sample forecast
fcast/sqrt(varf)
# Perform rolling forecasting
lookb <- 100
fcasts <- sapply(1:nrows, function(tday) {
  if (tday > lookb) {
    # Define the rolling look-back range
    startp <- max(1, tday-lookb)
    # startp <- 1 # Expanding look-back range
    rangev <- startp:(tday-1) # In-sample range
    # Subset the response and predictors
    resps <- respv[rangev]
    preds <- predm[rangev]
    # Calculate the fitted AR coefficients
    predinv <- MASS::ginv(preds)
    coeff <- predinv %*% resps
    # Calculate the in-sample forecasts of VTI (fitted values)
    fcasts <- preds %*% coeff
    # Calculate the residuals (forecast errors)
    resids <- (fcasts - resps)
    # Calculate the variance of the residuals
    varv <- sum(resids^2)/(NROW(preds)-NROW(coeff))
    # Calculate the covariance matrix of the AR coefficients
    pred2 <- crossprod(preds)
    covmat <- varv*MASS::ginv(pred2)
    coefsd <- sqrt(diag(covmat))
    coefft <- drop(coeff/coefsd) # t-values of the AR coefficients
    # Calculate the out-of-sample forecast
    predn <- predm[tday, ]
    fcast <- drop(predn %*% coeff)
    # Calculate the variance of the forecast
    varf <- drop(predn %*% covmat %*% t(predn))
    return(c(sd(resps), fcast=fcast, fstderr=sqrt(varf), coefft=coefft))
  } else {
    return(c(volv=0, fcast=0, fstderr=0, coefft=rep(0, NCOL(predm))))
  } # end if
})  # end sapply
# Coerce fcasts to a time series
fcasts <- t(fcasts)
ncols <- NCOL(fcasts)
colnames(fcasts) <- c("volv", "fcasts", "fstderr", colnames(predm))
fcasts <- xts::xts(fcasts, zoo::index(retp))
# Calculate the strategy PnLs
pnls <- retp*fcasts$fcasts
# Scale the PnL volatility to that of VTI
pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of the autoregressive strategy
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Rolling Autoregressive Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
dyLegend(show="always", width=300)
# Define backtesting function
sim_fcasts <- function(lookb=100, ordern=5, fixedlb=TRUE) {
  # Perform rolling forecasting
  fcasts <- sapply((lookb+1):nrows, function(tday) {
    # Rolling look-back range
    startp <- max(1, tday-lookb)
    # Expanding look-back range
    if (!fixedlb) {startp <- 1}
    startp <- max(1, tday-lookb)
    rangev <- startp:(tday-1) # In-sample range
    # Subset the response and predictors
    resps <- respv[rangev]
    preds <- predm[rangev, 1:ordern]
    # Invert the predictor matrix
    predinv <- MASS::ginv(preds)
    # Calculate the fitted AR coefficients
    coeff <- predinv %*% resps
    # Calculate the out-of-sample forecast
    drop(predm[tday, 1:ordern] %*% coeff)
  })  # end sapply
  # Add warmup period
  fcasts <- c(rep(0, lookb), fcasts)
}  # end sim_fcasts
# Simulate the rolling autoregressive forecasts
fcasts <- sim_fcasts(lookb=100, ordern=5)
c(mse=mean((fcasts - retp)^2), cor=cor(retp, fcasts))
library(parallel)  # Load package parallel
# Calculate the number of available cores
ncores <- detectCores() - 1
# Initialize compute cluster under Windows
compclust <- makeCluster(ncores)
# Perform parallel loop under Windows
lookbv <- seq(20, 600, 40)
fcasts <- parLapply(compclust, lookbv, sim_fcasts, ordern=6)
# Perform parallel bootstrap under Mac-OSX or Linux
fcasts <- mclapply(lookbv, sim_fcasts, ordern=6, mc.cores=ncores)
# Calculate the mean squared errors
mse <- sapply(fcasts, function(x) {
  c(mse=mean((retp - x)^2), cor=cor(retp, x))
})  # end sapply
mse <- t(mse)
rownames(mse) <- lookbv
# Select optimal lookb interval
lookb <- lookbv[which.min(mse[, 1])]
# Plot forecasting MSE
plot(x=lookbv, y=mse[, 1],
  xlab="look-back", ylab="MSE", type="l", lwd=2,
  main="MSE of AR Forecasting Model As Function of Look-back")
library(parallel)  # Load package parallel
# Calculate the number of available cores
ncores <- detectCores() - 1
# Initialize compute cluster under Windows
compclust <- makeCluster(ncores)
# Perform parallel loop under Windows
orderv <- 2:6
fcasts <- parLapply(compclust, orderv, sim_fcasts, lookb=lookb)
stopCluster(compclust)  # Stop R processes over cluster under Windows
# Perform parallel bootstrap under Mac-OSX or Linux
fcasts <- mclapply(orderv, sim_fcasts,
  lookb=lookb, mc.cores=ncores)
# Calculate the mean squared errors
mse <- sapply(fcasts, function(x) {
  c(mse=mean((retp - x)^2), cor=cor(retp, x))
})  # end sapply
mse <- t(mse)
rownames(mse) <- orderv
# Select optimal order parameter
ordern <- orderv[which.min(mse[, 1])]
# Plot forecasting MSE
plot(x=orderv, y=mse[, 1],
  xlab="AR order", ylab="MSE", type="l", lwd=2,
  main="MSE of Forecasting Model As Function of AR Order")
# Simulate the rolling autoregressive forecasts
fcasts <- sim_fcasts(lookb=lookb, ordern=ordern)
# Calculate the strategy PnLs
pnls <- fcasts*retp
# Scale the PnL volatility to that of VTI
pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])
wealthv <- cbind(retp, pnls, (retp+pnls)/2)
colnames(wealthv) <- c("VTI", "AR_Strategy", "Combined")
cor(wealthv)
# Annualized Sharpe ratios of VTI and AR strategy
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of AR strategy combined with VTI
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Autoregressive Strategy Fixed Look-back") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", strokeWidth=3) %>%
  dyLegend(show="always", width=300)
library(parallel)  # Load package parallel
# Calculate the number of available cores
ncores <- detectCores() - 1
# Initialize compute cluster under Windows
compclust <- makeCluster(ncores)
# Perform parallel loop under Windows
orderv <- 2:6
fcasts <- parLapply(compclust, orderv, sim_fcasts,
  lookb=lookb, fixedlb=FALSE)
stopCluster(compclust)  # Stop R processes over cluster under Windows
# Perform parallel bootstrap under Mac-OSX or Linux
fcasts <- mclapply(orderv, sim_fcasts,
  lookb=lookb, fixedlb=FALSE, mc.cores=ncores)
# Calculate the mean squared errors
mse <- sapply(fcasts, function(x) {
  c(mse=mean((retp - x)^2), cor=cor(retp, x))
})  # end sapply
mse <- t(mse)
rownames(mse) <- orderv
# Select optimal order parameter
ordern <- orderv[which.min(mse[, 1])]
# Plot forecasting MSE
plot(x=orderv, y=mse[, 1],
  xlab="AR order", ylab="MSE", type="l", lwd=2,
  main="MSE With Expanding Look-back As Function of AR Order")
# Simulate the autoregressive forecasts with expanding look-back
fcasts <- sim_fcasts(lookb=lookb, ordern=ordern, fixedlb=FALSE)
# Calculate the strategy PnLs
pnls <- fcasts*retp
# Scale the PnL volatility to that of VTI
pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])
wealthv <- cbind(retp, pnls, (retp+pnls)/2)
colnames(wealthv) <- c("VTI", "AR_Strategy", "Combined")
cor(wealthv)
# Annualized Sharpe ratios of VTI and AR strategy
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of AR strategy combined with VTI
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Autoregressive Strategy Expanding Look-back") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", strokeWidth=3) %>%
  dyLegend(show="always", width=300)
# Calculate the VTI daily percentage returns
retp <- na.omit(rutils::etfenv$returns[, c("VTI", "VXX", "SVXY")])
nrows <- NROW(retp)
# Define the response and predictor matrices
respv <- retp["/2019", "VTI"]
orderp <- 3
predm <- lapply(1:orderp, rutils::lagit, input=retp["/2019", c("VXX", "SVXY")])
predm <- rutils::do_call(cbind, predm)
predm <- cbind(rep(1, NROW(predm)), predm)
colnames(predm) <- c("phi0", paste0(c("VXX", "SVXY"), rep(1:orderp, each=2)))
# Calculate the fitted autoregressive coefficients
predinv <- MASS::ginv(predm)
coeff <- predinv %*% respv
# Calculate the in-sample forecasts of VTI (fitted values)
fcasts <- predm %*% coeff
# Calculate the residuals (forecast errors)
resids <- (fcasts - respv)
# The residuals are orthogonal to the predictors and the forecasts
round(cor(resids, fcasts), 6)
round(sapply(predm[, -1], function(x) cor(resids, x)), 6)
# Calculate the autoregressive strategy PnLs
pnls <- respv*fcasts
# Scale the PnL volatility to that of VTI
pnls <- pnls*sd(respv)/sd(pnls)
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(respv, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of the autoregressive strategy
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Autoregressive Strategy In-Sample") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
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
# Calculate in-sample factors
factv <- (predm*coeff)
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
invreg <- MASS::ginv(predm["/2019"])
# Calculate coefficients in-sample
coeff <- drop(invreg %*% respv["/2019"])
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
endw <- rutils::calc_endpoints(wealthv["2019/"], interval="weeks")
dygraphs::dygraph(cumsum(wealthv["2019/"])[endw],
  main="Yield Curve Strategy Out-of-Sample") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Define yearly dates
endd <- rutils::calc_endpoints(respv, interval="years")
# endd <- index(closep)[endd]
# Perform loop over yearly dates
lookb <- 500
fcasts <- lapply(seq_along(endd)[-1], function(tday) {
  # Define in-sample and out-of-sample intervals
  insample <- (max(1, endd[tday-1]-lookb):endd[tday-1])
  # insample <- (1:endd[tday-1]) # Expanding look-back
  outsample <- (endd[tday-1]+1):endd[tday]
  # Calculate coefficients in-sample
  invreg <- MASS::ginv(predm[insample, ])
  coeff <- drop(invreg %*% respv[insample, ])
  # Calculate forecasts out-of-sample
  fcasts <- (predm[outsample, ] %*% coeff)
})  # end lapply
fcasts <- do.call(rbind, fcasts)
fcastv <- sqrt(HighFreq::run_var(fcasts, lambda=0.4)[, 2])
fcasts <- ifelse(fcastv > mad(fcastv)/20, fcasts/fcastv, 0)
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
fcasts <- lapply(seq_along(endd)[-1], function(tday) {
  # Define in-sample and out-of-sample intervals
  insample <- (max(1, endd[tday-1]-lookb):endd[tday-1])
  # insample <- (1:endd[tday-1]) # Expanding look-back
  outsample <- (endd[tday-1]+1):endd[tday]
  # Calculate coefficients in-sample
  invreg <- MASS::ginv(predm[insample, ])
  coeff <- drop(invreg %*% respv[insample, ])
  # Calculate forecasts out-of-sample
  fcasts <- (predm[outsample, ] %*% coeff)
})  # end lapply
fcasts <- do.call(rbind, fcasts)
fcastv <- sqrt(HighFreq::run_var(fcasts, lambda=0.4)[, 2])
fcasts <- ifelse(fcastv > mad(fcastv)/20, fcasts/fcastv, 0)
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
fcasts <- lapply(seq_along(endd)[-1], function(tday) {
  # Define in-sample and out-of-sample intervals
  insample <- (max(1, endd[tday-1]-lookb):endd[tday-1])
  # insample <- (1:endd[tday-1]) # Expanding look-back
  outsample <- (endd[tday-1]+1):endd[tday]
  # Calculate coefficients in-sample
  invreg <- MASS::ginv(predm[insample, ])
  coeff <- drop(invreg %*% respv[insample, ])
  # Calculate forecasts out-of-sample
  fcasts <- (predm[outsample, ] %*% coeff)
})  # end lapply
fcasts <- do.call(rbind, fcasts)
fcastv <- sqrt(HighFreq::run_var(fcasts, lambda=0.4)[, 2])
fcasts <- ifelse(fcastv > mad(fcastv)/20, fcasts/fcastv, 0)
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
pnls <- lapply(dimv, function(dimax) {
  invred <- HighFreq::calc_invsvd(predm, dimax=dimax)
  coeff <- drop(invred %*% respv)
  fcasts <- (predm %*% coeff)
  fcastv <- sqrt(HighFreq::run_var(fcasts, lambda=0.4)[, 2])
  fcasts <- ifelse(fcastv > mad(fcastv)/20, fcasts/fcastv, 0)
  pnls <- fcasts*respv
  pnls/sd(pnls)
})
pnls <- sd(respv)*do.call(cbind, pnls)
colnames(pnls) <- paste0("dim=", dimv)
# Plot dygraph of in-sample pnls
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
endw <- rutils::calc_endpoints(pnls, interval="weeks")
dygraphs::dygraph(cumsum(pnls)[endw], main="In-Sample YC Strategies With Dimension Reduction") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Calculate in-sample pnls for different dimax values
pnls <- lapply(dimv, function(dimax) {
  invred <- HighFreq::calc_invsvd(predm["/2019"], dimax=dimax)
  coeff <- drop(invred %*% respv["/2019"])
  fcasts <- (predm %*% coeff)
  fcastv <- sqrt(HighFreq::run_var(fcasts, lambda=0.4)[, 2])
  fcasts <- ifelse(fcastv > mad(fcastv)/20, fcasts/fcastv, 0)
  pnls <- fcasts*respv
  pnls/sd(pnls)
})
pnls <- sd(respv)*do.call(cbind, pnls)
colnames(pnls) <- paste0("dim=", dimv)
# Plot dygraph of out-of-sample pnls
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
endw <- rutils::calc_endpoints(pnls["2019/"], interval="weeks")
dygraphs::dygraph(cumsum(pnls["2019/"])[endw], main="Out-of-Sample YC Strategies With Dimension Reduction") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Define monthly dates
endd <- rutils::calc_endpoints(respv, interval="month")
enddd <- seq_along(endd)[endd > lookb]
# Perform loop over monthly dates
lookb <- 500
dimax <- 2
library(parallel)  # Load package parallel
ncores <- detectCores() - 1
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
fcastv <- sqrt(HighFreq::run_var(fcasts, lambda=0.4)[, 2])
fcasts <- ifelse(fcastv > mad(fcastv)/20, fcasts/fcastv, 0)
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
fcastv <- sqrt(HighFreq::run_var(fcasts, lambda=0.4)[, 2])
fcasts <- ifelse(fcastv > mad(fcastv)/20, fcasts/fcastv, 0)
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
predm <- rutils::lagit(pcar[, 1:3])
# Calculate inverse of predictor
invreg <- MASS::ginv(predm)
# Calculate coefficients from response and inverse of predictor
coeff <- drop(invreg %*% respv)
# Calculate forecasts and PnLs in-sample
fcasts <- (predm %*% coeff)
fcastv <- sqrt(HighFreq::run_var(fcasts, lambda=0.4)[, 2])
fcasts <- ifelse(fcastv > mad(fcastv)/20, fcasts/fcastv, 0)
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
# Load the yield curve data
load(file="/Users/jerzy/Develop/lecture_slides/data/rates_data.RData")
ratev <- do.call(cbind, as.list(ratesenv))
namev <- colnames(ratev)
namev <- substr(namev, start=4, stop=10)
namev <- as.numeric(namev)
indeks <- order(namev)
ratev <- ratev[, indeks]
closep <- log(quantmod::Cl(rutils::etfenv$VTI))
colnames(closep) <- "VTI"
nrows <- NROW(closep)
datev <- zoo::index(closep)
ratev <- na.omit(ratev[datev])
closep <- closep[zoo::index(ratev)]
datev <- zoo::index(closep)
retp <- rutils::diffit(closep)
retr <- rutils::diffit(ratev)
# Create a combined predictor matrix
dimax <- 5
predm <- sapply(1:dimax, rutils::lagit, input=as.numeric(retp))
colnames(predm) <- paste0("retslag", 1:NCOL(predm))
predm <- cbind(predm, rutils::lagit(retr))
predm <- cbind(rep(1, NROW(predm)), predm)
colnames(predm)[1] <- "intercept"
respv <- retp
# Calculate in-sample pnls for different dimax values
dimv <- 2:11
pnls <- lapply(dimv, function(dimax) {
  invred <- HighFreq::calc_invsvd(predm, dimax=dimax)
  coeff <- drop(invred %*% respv)
  fcasts <- (predm %*% coeff)
  sign(fcasts)*respv
})
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("dim=", dimv)
# Plot dygraph of in-sample pnls
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls), main="In-Sample Returns of Combined Strategies With Shrinkage") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Define in-sample and out-of-sample intervals
insample <- (datev < as.Date("2020-01-01"))
outsample <- (datev >= as.Date("2020-01-01"))
# Calculate in-sample pnls for different dimax values
dimv <- 2:11
pnls <- lapply(dimv, function(dimax) {
  invred <- HighFreq::calc_invsvd(predm[insample, ], dimax=dimax)
  coeff <- drop(invred %*% respv[insample, ])
  fcasts <- (predm[outsample, ] %*% coeff)
  sign(fcasts)*respv[outsample, ]
})
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("dim=", dimv)
# Plot dygraph of out-of-sample pnls
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls), main="Out-of-Sample Returns of Combined Strategies With Shrinkage") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Define monthly dates
format(datev[1], "%m-%Y")
format(datev[NROW(datev)], "%m-%Y")
endd <- seq.Date(from=as.Date("2001-05-01"), to=as.Date("2021-04-01"), by="month")
# Perform loop over monthly dates
lookb <- 6
dimax <- 3
pnls <- lapply((lookb+1):(NROW(endd)-1), function(tday) {
  # Define in-sample and out-of-sample intervals
  insample <- (datev > endd[tday-lookb]) & (datev < endd[tday])
  outsample <- (datev > endd[tday]) & (datev < endd[tday+1])
  # Calculate forecasts and PnLs out-of-sample
  invred <- HighFreq::calc_invsd(predm[insample, ], dimax=dimax)
  coeff <- drop(invred %*% respv[insample, ])
  fcasts <- (predm[outsample, ] %*% coeff)
  sign(fcasts)*respv[outsample, ]
})  # end lapply
pnls <- do.call(rbind, pnls)
# Plot dygraph of rolling monthly YC strategy
vti <- rutils::diffit(closep[zoo::index(pnls),])
wealthv <- cbind(vti, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
colv <- colnames(wealthv)
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Rolling Monthly Shrinkage YC Strategy") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=colv[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Define weekly dates
endd <- seq.Date(from=as.Date("2001-05-01"), to=as.Date("2021-04-01"), by="weeks")
# Perform loop over weekly dates
lookb <- 8
dimax <- 4
pnls <- lapply((lookb+1):(NROW(endd)-1), function(tday) {
  # Define in-sample and out-of-sample intervals
  insample <- (datev > endd[tday-lookb]) & (datev < endd[tday])
  outsample <- (datev > endd[tday]) & (datev < endd[tday+1])
  # Calculate forecasts and PnLs out-of-sample
  invred <- HighFreq::calc_invsd(predm[insample, ], dimax=dimax)
  coeff <- drop(invred %*% respv[insample, ])
  fcasts <- (predm[outsample, ] %*% coeff)
  sign(fcasts)*respv[outsample, ]
})  # end lapply
pnls <- do.call(rbind, pnls)
# Plot dygraph of rolling weekly YC strategy
vti <- rutils::diffit(closep[zoo::index(pnls),])
wealthv <- cbind(vti, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
colv <- colnames(wealthv)
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Rolling Weekly Shrinkage YC Strategy") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=colv[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Find optimal nagg for predictor
naggs <- 5:100
tvalues <- sapply(naggs, function(nagg) {
  predm <- HighFreq::roll_mean(retr, lookb=nagg)
  predm <- cbind(rep(1, NROW(predm)), predm)
  predm <- rutils::lagit(predm)
  regmod <- lm(respv ~ predm - 1)
  regsum <- summary(regmod)
  max(abs(regsum$coefficients[, 3][-1]))
})  # end sapply
naggs[which.max(tvalues)]
plot(naggs, tvalues, t="l", col="blue", lwd=2)
# Calculate aggregated predictor
nagg <- 53
predm <- HighFreq::roll_mean(retr, lookb=nagg)
predm <- rutils::lagit(predm)
predm <- cbind(rep(1, NROW(predm)), predm)
regmod <- lm(respv ~ predm - 1)
summary(regmod)
# Calculate forecasts and PnLs in-sample
invreg <- MASS::ginv(predm)
coeff <- drop(invreg %*% respv)
fcasts <- (predm %*% coeff)
pnls <- sign(fcasts)*respv
# Plot dygraph of in-sample YC strategy
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
colv <- colnames(wealthv)
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Aggregated YC Strategy In-sample") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=colv[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Define in-sample and out-of-sample intervals
insample <- (datev < as.Date("2020-01-01"))
outsample <- (datev >= as.Date("2020-01-01"))
# Calculate forecasts and PnLs out-of-sample
invreg <- MASS::ginv(predm[insample, ])
coeff <- drop(invreg %*% respv[insample, ])
fcasts <- (predm[outsample, ] %*% coeff)
pnls <- sign(fcasts)*respv[outsample, ]
# Plot dygraph of out-of-sample YC strategy
wealthv <- cbind(retp[outsample, ], pnls)
colnames(wealthv) <- c("VTI", "Strategy")
colv <- colnames(wealthv)
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Aggregated YC Strategy Out-of-Sample") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=colv[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=300)
