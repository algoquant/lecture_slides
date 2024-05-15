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
# Calculate the VTI daily percentage returns
retp <- na.omit(rutils::etfenv$returns$VTI)
# Calculate the autocorrelations of VTI daily returns
rutils::plot_acf(retp)
# Simulate mean reverting strategy
posv <- -sign(retp)
pnls <- retp*rutils::lagit(posv, lagg=1)
# Subtract transaction costs from the pnls
bidask <- 0.0001 # Bid-ask spread equal to 1 basis point
costs <- 0.5*bidask*abs(rutils::diffit(posv))
pnls <- (pnls - costs)
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retp, pnls, (retp+pnls)/2)
colnames(wealthv) <- c("VTI", "AR_Strategy", "Combined")
cor(wealthv)
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of mean reverting strategy
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="VTI Daily Mean Reverting Strategy") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dyLegend(show="always", width=300)
# Simulate mean reverting strategy with two day holding period
posv <- -rutils::roll_sum(sign(retp), look_back=2)/2
pnls <- retp*rutils::lagit(posv)
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of mean reverting strategy
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Daily Mean Reverting Strategy With Two Day Holding Period") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Load daily S&P500 stock returns
load(file="/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
retp <- na.omit(retstock$MSFT)
rutils::plot_acf(retp)
# Simulate mean reverting strategy with two day holding period
posv <- -rutils::roll_sum(sign(retp), look_back=2)/2
pnls <- retp*rutils::lagit(posv)
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("MSFT", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of mean reverting strategy
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Daily Mean Reverting Strategy For MSFT") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Simulate mean reverting strategy for all S&P500 stocks
library(parallel)  # Load package parallel
ncores <- detectCores() - 1
pnll <- mclapply(retstock, function(retp) {
  retp <- na.omit(retp)
  posv <- -rutils::roll_sum(sign(retp), look_back=2)/2
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
# Plot dygraph of mean reverting strategy
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Daily Mean Reverting Strategy For All Stocks") %>%
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
# Plot dygraph of mean reverting strategy
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Mean Reverting Strategy For Low and High Volatility Stocks") %>%
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
bidask <- 0.0001 # Bid-ask spread equal to 1 basis point
costs <- 0.5*bidask*abs(rutils::diffit(posv))
pnls <- (pnls - costs)
# Scale the PnL volatility to that of VTI
pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of mean reverting strategy
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="VTI EMA Daily Mean Reverting Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Calculate the trailing volatility
volv <- HighFreq::run_var(retp, lambda=0.5)
volv <- sqrt(volv)
# Scale the returns by their trailing volatility
retsc <- ifelse(volv > 0, retp/volv, 0)
# Calculate the EMA returns
retma <- HighFreq::run_mean(retsc, lambda=0.1)
# Calculate the positions and PnLs
posv <- -rutils::lagit(retma, lagg=1)
pnls <- retp*posv
costs <- 0.5*bidask*abs(rutils::diffit(posv))
pnls <- (pnls - costs)
# Scale the PnL volatility to that of VTI
pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of mean reverting strategy
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="VTI EMA Daily Mean Reverting Strategy") %>%
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
coeff <- predinv %*% respv
# Calculate the in-sample forecasts of VTI (fitted values)
fcasts <- predm %*% coeff
# Plot the AR coefficients
coeffn <- paste0("phi", 0:(NROW(coeff)-1))
barplot(coeff ~ coeffn, xlab="", ylab="t-value", col="grey",
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
coeffsd <- sqrt(diag(covmat))
# Calculate the t-values of the AR coefficients
coefft <- drop(coeff/coeffsd)
coeffn <- paste0("phi", 0:(NROW(coefft)-1))
# Plot the t-values of the AR coefficients
barplot(coefft ~ coeffn, xlab="", ylab="t-value", col="grey",
  main="Coefficient t-values of AR Forecasting Model")
# Calculate the trailing volatility of the residuals
residv <- sqrt(HighFreq::run_var(resids, lambda=0.9))
# Plot dygraph of volatility of residuals
datav <- cbind(cumsum(retp), residv)
colnames(datav) <- c("VTI", "residual vol")
endd <- rutils::calc_endpoints(datav, interval="weeks")
dygraphs::dygraph(datav[endd], main="Volatility of Residuals") %>%
  dyAxis("y", label="VTI", independentTicks=TRUE) %>%
  dyAxis("y2", label="residual vol", independentTicks=TRUE) %>%
  dySeries(name="VTI", axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name="residual vol", axis="y2", strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)
# Scale the forecasts by their volatility
fcastv <- sqrt(HighFreq::run_var(fcasts, lambda=0.2))
posv <- ifelse(fcastv > 0, fcasts/fcastv, 0)
# Simulate autoregressive strategy in-sample
pnls <- retp*posv
costs <- 0.5*bidask*abs(rutils::diffit(posv))
pnls <- (pnls - costs)
# Scale the PnL volatility to that of VTI
pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of autoregressive strategy
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
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
coeffn <- paste0("phi", 0:(NROW(coeffh)-1))
barplot(coeffh ~ coeffn, main="High Volatility AR Coefficients",
  col="grey", xlab="", ylab="coefficient", ylim=c(-0.1, 0.05))
# Calculate the low volatility AR coefficients
respv <- retp["2012/2019"]
predm <- lapply(1:orderp, rutils::lagit, input=respv)
predm <- rutils::do_call(cbind, predm)
predm <- cbind(rep(1, NROW(predm)), predm)
predinv <- MASS::ginv(predm)
coeffl <- drop(predinv %*% respv)
barplot(coeffl ~ coeffn, main="Low Volatility AR Coefficients",
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
# Plot dygraph of autoregressive strategy
dygraphs::dygraph(cumsum(wealthv)[endd],
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
# Plot dygraph of autoregressive strategy
dygraphs::dygraph(cumsum(wealthv)[endd],
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
dygraphs::dygraph(datav[endd], main="Volatility of high Residuals") %>%
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
dygraphs::dygraph(datav[endd], main="Volatility of low Residuals") %>%
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
# Plot dygraph of autoregressive strategy
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
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
# Calculate the in-sample forecasts of VTI
fcasts <- predm %*% coeff
# Winsorize the forecasts
# fcasts <- winsorfun(fcasts/mad(fcasts), lambda=1.5)
# Simulate autoregressive strategy in-sample
pnls <- retp*fcasts
# Scale the PnL volatility to that of VTI
pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of autoregressive strategy
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Winsorized Autoregressive Strategy In-Sample") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Scale the returns by their trailing volatility
varv <- HighFreq::run_var(retp, lambda=0.99)
retsc <- ifelse(varv > 0, retp/sqrt(varv), 0)
# Calculate the AR coefficients
predm <- lapply(1:orderp, rutils::lagit, input=retsc)
predm <- rutils::do_call(cbind, predm)
predm <- cbind(rep(1, nrows), predm)
colnames(predm) <- c("phi0", paste0("lag", 1:orderp))
predinv <- MASS::ginv(predm)
coeff <- predinv %*% retsc
# Calculate the in-sample forecasts of VTI
fcasts <- predm %*% coeff
# Simulate autoregressive strategy in-sample
pnls <- retp*fcasts
# Scale the PnL volatility to that of VTI
pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of autoregressive strategy
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Autoregressive Strategy With Returns Scaled By Volatility") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Calculate VTI returns and trading volumes
ohlc <- rutils::etfenv$VTI
datev <- zoo::index(ohlc)
nrows <- NROW(ohlc)
closep <- quantmod::Cl(ohlc)
retp <- rutils::diffit(log(closep))
volumv <- quantmod::Vo(ohlc)
# Calculate trailing average volume
volumr <- HighFreq::run_mean(volumv, lambda=0.25)
# Scale the returns using volume clock to trading time
retsc <- ifelse(volumv > 0, volumr*retp/volumv, 0)
# Calculate the AR coefficients
respv <- retsc
orderp <- 5
predm <- lapply(1:orderp, rutils::lagit, input=respv)
predm <- rutils::do_call(cbind, predm)
predm <- cbind(rep(1, nrows), predm)
colnames(predm) <- c("phi0", paste0("lag", 1:orderp))
predinv <- MASS::ginv(predm)
coeff <- predinv %*% respv
# Calculate the in-sample forecasts of VTI
fcasts <- predm %*% coeff
# Simulate autoregressive strategy in-sample
pnls <- retp*fcasts
# Scale the PnL volatility to that of VTI
pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of autoregressive strategy
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Autoregressive Strategy With Returns Scaled By Volume") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Define the response and predictor matrices
respv <- retp
orderp <- 5
predm <- lapply(1:orderp, rutils::lagit, input=respv)
predm <- rutils::do_call(cbind, predm)
predm <- cbind(rep(1, nrows), predm)
colnames(predm) <- c("phi0", paste0("lag", 1:orderp))
# Calculate the in-sample forecasts of VTI (fitted values)
predinv <- MASS::ginv(predm)
coeff <- predinv %*% respv
fcasts <- predm %*% coeff
# Calculate the correlation between forecasts and returns
cor(fcasts, retp)
# Calculate the forecasting errors
errorf <- (fcasts - retp)
# Mean squared error
mean(errorf^2)
# Plot the forecasts
datav <- cbind(retp, fcasts)["2020-01/2020-06"]
colnames(datav) <- c("returns", "forecasts")
dygraphs::dygraph(datav,
  main="VTI Returns And Forecasts") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Calculate the forecasts as function of the AR order
fcasts <- lapply(2:NCOL(predm), function(ordern) {
  # Calculate the fitted AR coefficients
  predinv <- MASS::ginv(predm[, 1:ordern])
  coeff <- predinv %*% respv
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
# Calculate the forecasts as function of the AR order
fcasts <- lapply(2:NCOL(predm), function(ordern) {
  # Calculate the fitted AR coefficients
  predinv <- MASS::ginv(predm[insample, 1:ordern])
  coeff <- predinv %*% respv[insample]
  # Calculate the out-of-sample forecasts of VTI
  drop(predm[outsample, 1:ordern] %*% coeff)
})  # end lapply
names(fcasts) <- paste0("n=", 2:NCOL(predm))
# Calculate the mean squared errors
mse <- sapply(fcasts, function(x) {
  c(mse=mean((respv[outsample] - x)^2), cor=cor(respv[outsample], x))
})  # end sapply
mse <- t(mse)
rownames(mse) <- names(fcasts)
# Plot forecasting MSE
plot(x=2:NCOL(predm), y=mse[, 1],
  xlab="AR(n) order", ylab="MSE", type="l", lwd=2,
  main="MSE of Out-of-sample AR(n) Forecasting Model for VTI")
# Calculate the optimal AR coefficients
predinv <- MASS::ginv(predm[insample, 1:2])
coeff <- drop(predinv %*% respv[insample])
# Calculate the out-of-sample PnLs
pnls <- lapply(fcasts, function(fcast) {
  pnls <- fcast*retp[outsample]
  pnls*sd(retp[retp<0])/sd(pnls[pnls<0])
})  # end lapply
pnls <- rutils::do_call(cbind, pnls)
colnames(pnls) <- names(fcasts)
# Plot dygraph of out-of-sample PnLs
colorv <- colorRampPalette(c("red", "blue"))(NCOL(pnls))
colnamev <- colnames(pnls)
endd <- rutils::calc_endpoints(pnls, interval="weeks")
dygraphs::dygraph(cumsum(pnls)[endd],
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
coeffsd <- sqrt(diag(covmat))
# Calculate the t-values of the AR coefficients
coefft <- drop(coeff/coeffsd)
# Calculate the out-of-sample forecast
predn <- predm[tday, ]
fcast <- drop(predn %*% coeff)
# Calculate the variance of the forecast
varf <- drop(predn %*% covmat %*% t(predn))
# Calculate the t-value of the out-of-sample forecast
fcast/sqrt(varf)
# Perform rolling forecasting
lookb <- 100
fcasts <- sapply((lookb+1):nrows, function(tday) {
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
  coeffsd <- sqrt(diag(covmat))
  coefft <- drop(coeff/coeffsd) # t-values of the AR coefficients
  # Calculate the out-of-sample forecast
  predn <- predm[tday, ]
  fcast <- drop(predn %*% coeff)
  # Calculate the variance of the forecast
  varf <- drop(predn %*% covmat %*% t(predn))
  c(sd(resps), fcast=fcast, fstderr=sqrt(varf), coefft=coefft)
})  # end sapply
# Coerce fcasts to a time series
fcasts <- t(fcasts)
ncols <- NCOL(fcasts)
fcasts <- rbind(fcasts, matrix(numeric(ncols*lookb), nc=ncols))
colnames(fcasts) <- c("volvti", "fcasts", "fstderr", colnames(predm))
fcasts <- xts::xts(fcasts, zoo::index(retp))
# Calculate the strategy PnLs
pnls <- retp*fcasts$fcasts
# Scale the PnL volatility to that of VTI
pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of autoregressive strategy
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Rolling Autoregressive Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
dyLegend(show="always", width=300)
# Define backtesting function
sim_fcasts <- function(lookb=100, ordern=5, fixedlb=TRUE) {
  # Perform rolling forecasting
  fcasts <- sapply((lookb+1):nrows, function(tday) {
    # Define the rolling look-back range
    startp <- max(1, tday-lookb)
    # Expanding look-back range
    # startp <- 1
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
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Autoregressive Strategy Fixed Look-back") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", label="Combined", strokeWidth=3) %>%
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
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Autoregressive Strategy Expanding Look-back") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", label="Combined", strokeWidth=3) %>%
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
# Simulate autoregressive strategy in-sample
pnls <- respv*fcasts
# Scale the PnL volatility to that of VTI
pnls <- pnls*sd(respv)/sd(pnls)
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(respv, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of autoregressive strategy
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Autoregressive Strategy In-Sample") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
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
# Plot log wealth
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
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
bidask <- 0.0001 # Bid-ask spread equal to 1 basis point
costs <- 0.5*bidask*abs(rutils::diffit(posv))
pnls <- (pnls - costs)
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retd, pnls)
colnames(wealthv) <- c("VTI daytime", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
+ c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of crossover strategy
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Mean-Reversion Strategy For Daytime VTI Returns") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Calculate the z-scores of the cumulative daytime returns
retc <- cumsum(retd)
lambdaf <- 0.24
retm <- rutils::lagit(HighFreq::run_mean(retc, lambda=lambdaf))
retv <- sqrt(rutils::lagit(HighFreq::run_var(retc, lambda=lambdaf)))
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
costs <- 0.5*bidask*abs(rutils::diffit(posv))
pnls <- (pnls - costs)
# Calculate the Sharpe ratios
wealthv <- cbind(retd, pnls)
colnames(wealthv) <- c("VTI daytime", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of daytime Bollinger strategy
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Bollinger strategy For Daytime VTI Returns") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Calculate the pnls and the transaction costs
posv <- sign(reton)
pnls <- posv*retd
costs <- 0.5*bidask*abs(rutils::diffit(posv))
pnls <- (pnls - costs)
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retd, pnls)
colnames(wealthv) <- c("VTI daytime", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of crossover strategy
dygraphs::dygraph(cumsum(wealthv)[endd],
main="Overnight Trend For Daytime VTI Returns") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Extract the log VTI prices
pricev <- log(na.omit(rutils::etfenv$prices$VTI))
nrows <- NROW(pricev)
# Calculate the trailing mean prices
lambdaf <- 0.9
pricem <- HighFreq::run_mean(pricev, lambda=lambdaf)
# Calculate the trailing volatilities
volv <- HighFreq::run_var(pricev, lambda=lambdaf)
volv <- sqrt(volv)
# Dygraphs plot of Bollinger bands
priceb <- cbind(pricev, pricem, pricem+volv, pricem-volv)
colnames(priceb)[2:4] <- c("mean", "upper", "lower")
colnamev <- colnames(priceb)
dygraphs::dygraph(priceb["2008-09/2009-09"], main="VTI Prices and Bollinger Bands") %>%
  dySeries(name=colnamev[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], strokeWidth=2, col="green") %>%
  dySeries(name=colnamev[3], strokeWidth=2, strokePattern="dashed", col="red") %>%
  dySeries(name=colnamev[4], strokeWidth=2, strokePattern="dashed", col="red") %>%
  dyLegend(show="always", width=200)
NA
# Center the prices
pricec <- pricev - pricem
# Dygraphs plot of Bollinger bands
priceb <- cbind(pricec, volv, -volv)
colnames(priceb) <- c("price", "upper", "lower")
colnamev <- colnames(priceb)
dygraphs::dygraph(priceb["2008-09/2009-09"],
  main="Centered VTI Prices and Bollinger Bands") %>%
  dySeries(name=colnamev[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], strokeWidth=2, strokePattern="dashed", col="red") %>%
  dySeries(name=colnamev[3], strokeWidth=2, strokePattern="dashed", col="green") %>%
  dyLegend(show="always", width=200)
# Calculate the trailing mean prices and volatilities
lambdaf <- 0.1
pricem <- HighFreq::run_mean(pricev, lambda=lambdaf)
volv <- HighFreq::run_var(pricev, lambda=lambdaf)
volv <- sqrt(volv)
# Prepare the simulation parameters
pricen <- as.numeric(pricev) # Numeric price
pricec <- pricen - pricem # Centered price
threshv <- volv
posv <- integer(nrows) # Stock positions
posv[1] <- 0 # Initial position
# Calculate the positions from Bollinger bands
for (it in 2:nrows) {
  if (pricec[it-1] > threshv[it-1]) {
    # Enter short
    posv[it] <- (-1)
  } else if (pricec[it-1] < (-threshv[it-1])) {
    # Enter long
    posv[it] <- 1
  } else if ((posv[it-1] < 0) && (pricec[it-1] < 0)) {
    # Unwind short
    posv[it] <- 0
  } else if ((posv[it-1] > 0) && (pricec[it-1] > 0)) {
    # Unwind long
    posv[it] <- 0
  } else {
    # Do nothing
    posv[it] <- posv[it-1]
  }  # end if
}  # end for
# Calculate the number of trades
ntrades <- sum(abs(rutils::diffit(posv)) > 0)
# Calculate the pnls
retp <- rutils::diffit(pricev)
pnls <- retp*posv
# Calculate the Sharpe ratios
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
sharper <- sqrt(252)*sapply(wealthv, function(x) mean(x)/sd(x[x<0]))
sharper <- round(sharper, 3)
# Dygraphs plot of Bollinger strategy
colnamev <- colnames(wealthv)
captiont <- paste("Bollinger Strategy", "/ \n",
  paste0(paste(colnamev[1:2], "Sharpe =", sharper), collapse=" / "), "/ \n",
  "Number trades =", ntrades)
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd], main=captiont) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=200)
# Simulate the modified Bollinger strategy
posv <- integer(nrows) # Stock positions
posv[1] <- 0 # Initial position
for (it in 2:nrows) {
  if (pricec[it-1] > threshv[it-1]) {
    # Enter short
    posv[it] <- (-1)
  } else if (pricec[it-1] < (-threshv[it-1])) {
    # Enter long
    posv[it] <- 1
  } else {
    # Do nothing
    posv[it] <- posv[it-1]
  }  # end if
}  # end for
# Calculate the PnLs
pnls2 <- retp*posv
# Calculate the Sharpe ratios
wealthv <- cbind(pnls, pnls2)
colnames(wealthv) <- c("Bollinger", "Modified")
sharper <- sqrt(252)*sapply(wealthv, function(x) mean(x)/sd(x[x<0]))
sharper <- round(sharper, 3)
# Dygraphs plot of Bollinger strategy
colnamev <- colnames(wealthv)
captiont <- paste("Bollinger Strategy", "/ \n",
  paste0(paste(colnamev[1:2], "Sharpe =", sharper), collapse=" / "), "/ \n",
  "Number trades =", ntrades)
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd], main=captiont) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=200)
# Simulate the modified Bollinger strategy quickly
posf <- rep(NA_integer_, nrows)
posf[1] <- 0
posf <- ifelse(pricec > threshv, -1, posf)
posf <- ifelse(pricec < -threshv, 1, posf)
posf <- zoo::na.locf(posf)
# Lag the positions to trade in the next period
posf <- rutils::lagit(posf, lagg=1)
# Compare the positions
all.equal(posv, posf)
# Calculate the daytime open-to-close VTI returns
ohlc <- log(rutils::etfenv$VTI)
nrows <- NROW(ohlc)
openp <- quantmod::Op(ohlc)
highp <- quantmod::Hi(ohlc)
lowp <- quantmod::Lo(ohlc)
closep <- quantmod::Cl(ohlc)
retd <- (closep - openp)
# Calculate the cumulative daytime VTI returns
priced <- cumsum(retd)
lambdaf <- 0.1
pricem <- HighFreq::run_mean(priced, lambda=lambdaf)
volv <- HighFreq::run_var(priced, lambda=lambdaf)
volv <- sqrt(volv)
# Calculate the positions from Bollinger bands
threshv <- volv
pricec <- zoo::coredata(priced - pricem)
posv <- rep(NA_integer_, nrows)
posv[1] <- 0
posv <- ifelse(pricec > threshv, -1, posv)
posv <- ifelse(pricec < -threshv, 1, posv)
posv <- zoo::na.locf(posv)
# Lag the positions to trade in the next period
posv <- rutils::lagit(posv, lagg=1)
# Calculate the number of trades and the PnLs
ntrades <- sum(abs(rutils::diffit(posv)) > 0)
pnls <- retd*posv
# Calculate the Sharpe ratios
wealthv <- cbind(retd, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
nyears <- as.numeric(end(priced)-start(priced))/365
sharper <- sqrt(nrows/nyears)*sapply(wealthv, function(x) mean(x)/sd(x[x<0]))
sharper <- round(sharper, 3)
# Dygraphs plot of Bollinger strategy
colnamev <- colnames(wealthv)
captiont <- paste("Bollinger Strategy for Daytime VTI", "/ \n",
  paste0(paste(colnamev[1:2], "Sharpe =", sharper), collapse=" / "), "/ \n",
  "Number trades =", ntrades)
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd], main=captiont) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=200)
# Calculate the trailing mean prices and volatilities of SPY
pricev <- log(quantmod::Cl(HighFreq::SPY))
nrows <- NROW(pricev)
lambdaf <- 0.1
pricem <- HighFreq::run_mean(pricev, lambda=lambdaf)
volv <- HighFreq::run_var(pricev, lambda=lambdaf)
volv <- sqrt(volv)
# Calculate the positions from Bollinger bands
threshv <- volv
pricec <- zoo::coredata(pricev - pricem)
posv <- rep(NA_integer_, nrows)
posv[1] <- 0
posv <- ifelse(pricec > threshv, -1, posv)
posv <- ifelse(pricec < -threshv, 1, posv)
posv <- zoo::na.locf(posv)
# Lag the positions to trade in the next period
posv <- rutils::lagit(posv, lagg=1)
# Calculate the number of trades and the PnLs
ntrades <- sum(abs(rutils::diffit(posv)) > 0)
retp <- rutils::diffit(pricev)
pnls <- retp*posv
# Subtract transaction costs from the pnls
bidask <- 0.0001 # Bid-ask spread equal to 1 basis point
costs <- 0.5*bidask*abs(rutils::diffit(posv))
pnls <- (pnls - costs)
# Calculate the Sharpe ratios
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("SPY", "Strategy")
nyears <- as.numeric(end(pricev)-start(pricev))/365
sharper <- sqrt(nrows/nyears)*sapply(wealthv, function(x) mean(x)/sd(x[x<0]))
sharper <- round(sharper, 3)
# Dygraphs plot of Bollinger strategy
colnamev <- colnames(wealthv)
captiont <- paste("Bollinger Strategy for Minute SPY", "/ \n",
  paste0(paste(colnamev[1:2], "Sharpe =", sharper), collapse=" / "), "/ \n",
  "Number trades =", ntrades)
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd], main=captiont) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=100)
# Extract time series of VTI log prices
pricev <- log(na.omit(rutils::etfenv$prices$VTI))
nrows <- NROW(pricev)
# Define look-back window
lookb <- 11
# Calculate time series of trailing medians
medianv <- HighFreq::roll_mean(pricev, lookb, method="nonparametric")
# medianv <- TTR::runMedian(pricev, n=lookb)
# Calculate time series of MAD
madv <- HighFreq::roll_var(pricev, lookb=lookb, method="nonparametric")
# madv <- TTR::runMAD(pricev, n=lookb)
# Calculate time series of z-scores
zscores <- ifelse(madv > 0, (pricev - medianv)/madv, 0)
zscores[1:lookb, ] <- 0
tail(zscores, lookb)
range(zscores)
# Plot histogram of z-scores
histp <- hist(zscores, col="lightgrey",
  xlab="z-scores", breaks=50, xlim=c(-4, 4),
  ylab="frequency", freq=FALSE, main="Hampel Z-Scores histogram")
lines(density(zscores, adjust=1.5), lwd=3, col="blue")
# Dygraphs plot of Hampel bands
priceb <- cbind(pricev, medianv, medianv+madv, medianv-madv)
colnames(priceb)[2:4] <- c("median", "upper", "lower")
colnamev <- colnames(priceb)
dygraphs::dygraph(priceb["2008-09/2009-09"], main="VTI Prices and Hampel Bands") %>%
  dySeries(name=colnamev[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], strokeWidth=2, col="green") %>%
  dySeries(name=colnamev[3], strokeWidth=2, strokePattern="dashed", col="red") %>%
  dySeries(name=colnamev[4], strokeWidth=2, strokePattern="dashed", col="red") %>%
  dyLegend(show="always", width=200)
# Calculate the time series of trailing medians and MAD
lookb <- 3
medianv <- HighFreq::roll_mean(pricev, lookb, method="nonparametric")
madv <- HighFreq::roll_var(pricev, lookb=lookb, method="nonparametric")
# Calculate the time series of z-scores
zscores <- ifelse(madv > 0, (pricev - medianv)/madv, 0)
zscores[1:lookb, ] <- 0
range(zscores)
# Calculate the positions
threshv <- 1
posv <- rep(NA_integer_, nrows)
posv[1] <- 0
posv[zscores > threshv] <- (-1)
posv[zscores < -threshv] <- 1
posv <- zoo::na.locf(posv)
posv <- rutils::lagit(posv)
# Calculate the number of trades and the PnLs
ntrades <- sum(abs(rutils::diffit(posv)) > 0)
retp <- rutils::diffit(pricev)
pnls <- retp*posv
# Calculate the Sharpe ratios
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
sharper <- sqrt(252)*sapply(wealthv, function(x) mean(x)/sd(x[x<0]))
sharper <- round(sharper, 3)
# Dygraphs plot of Hampel strategy
captiont <- paste("Hampel Strategy", "/ \n",
  paste0(paste(colnamev[1:2], "Sharpe =", sharper), collapse=" / "), "/ \n",
  "Number trades =", ntrades)
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
colnamev <- colnames(wealthv)
dygraphs::dygraph(cumsum(wealthv)[endd], main=captiont) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=100)
# Calculate the trailing mean prices and volatilities of SPY
pricev <- log(quantmod::Cl(HighFreq::SPY))
nrows <- NROW(pricev)
# Calculate the price medians and MAD
lookb <- 3
medianv <- HighFreq::roll_mean(pricev, lookb, method="nonparametric")
madv <- HighFreq::roll_var(pricev, lookb=lookb, method="nonparametric")
# Calculate the time series of z-scores
zscores <- ifelse(madv > 0, (pricev - medianv)/madv, 0)
zscores[1:lookb, ] <- 0
# Calculate the positions
threshv <- 1
posv <- rep(NA_integer_, nrows)
posv[1] <- 0
posv[zscores < -threshv] <- 1
posv[zscores > threshv] <- (-1)
posv <- zoo::na.locf(posv)
posv <- rutils::lagit(posv)
# Calculate the number of trades and the PnLs
ntrades <- sum(abs(rutils::diffit(posv)) > 0)
retp <- rutils::diffit(pricev)
pnls <- retp*posv
# Subtract transaction costs from the pnls
costs <- 0.5*bidask*abs(rutils::diffit(posv))
pnls <- (pnls - costs)
# Calculate the Sharpe ratios
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("SPY", "Strategy")
nyears <- as.numeric(end(pricev)-start(pricev))/365
sharper <- sqrt(nrows/nyears)*sapply(wealthv, function(x) mean(x)/sd(x[x<0]))
sharper <- round(sharper, 3)
# Dygraphs plot of Hampel strategy
colnamev <- colnames(wealthv)
captiont <- paste("Hampel Strategy for Minute SPY", "/ \n",
  paste0(paste(colnamev[1:2], "Sharpe =", sharper), collapse=" / "), "/ \n",
  "Number trades =", ntrades)
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd], main=captiont) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=100)
