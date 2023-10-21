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
regs <- HighFreq::roll_reg(respv=closep, predm=datev, look_back=look_back, controlv=controlv)
regs <- drop(regs[, NCOL(regs)])
regs[1:look_back] <- 0

# Plot dygraph of z-scores of VTI prices
pricev <- cbind(closep, regs)
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
predm <- cbind(volatz, volumz, regs)
predm[1, ] <- 0
predm <- rutils::lagit(predm)
# Fit in-sample logistic regression for tops
logmod <- glm(tops ~ predm, family=binomial(logit))
summary(logmod)
coeff <- logmod$coefficients
fcasts <- drop(cbind(rep(1, nrows), predm) %*% coeff)
ordern <- order(fcasts)
# Calculate the in-sample forecasts from logistic regression model
fcasts <- 1/(1 + exp(-fcasts))
all.equal(logmod$fitted.values, fcasts, check.attributes=FALSE)
hist(fcasts)

plot(x=fcasts[ordern], y=tops[ordern],
     main="Logistic Regression of Stock Tops",
     col="orange", xlab="predictor", ylab="top")
lines(x=fcasts[ordern], y=logmod$fitted.values[ordern], col="blue", lwd=3)
legend(x=0.1, y=1.2, inset=0.0, bty="n", lwd=6,
 legend=c("tops", "logit fitted values"), y.intersp=0.1,
 col=c("orange", "blue"), lty=c(NA, 1), pch=c(1, NA))

# Define discrimination threshold value
threshv <- quantile(fcasts, confl[2])
# Calculate the confusion matrix in-sample
confmat <- table(actual=!tops, forecast=(fcasts < threshv))
confmat
# Calculate the FALSE positive (type I error)
sum(tops & (fcasts < threshv))
# Calculate the FALSE negative (type II error)
sum(!tops & (fcasts > threshv))

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
confun <- function(actual, fcasts, threshv) {
  forb <- (fcasts < threshv)
  conf <- matrix(c(sum(!actual & !forb), sum(actual & !forb),
             sum(!actual & forb), sum(actual & forb)), ncol=2)
  conf <- conf / rowSums(conf)
  c(typeI=conf[2, 1], typeII=conf[1, 2])
}  # end confun
confun(!tops, fcasts, threshv=threshv)
# Define vector of discrimination thresholds
threshv <- quantile(fcasts, seq(0.01, 0.99, by=0.01))
# Calculate the error rates
errorr <- sapply(threshv, confun,
  actual=!tops, fcasts=fcasts)  # end sapply
errorr <- t(errorr)
rownames(errorr) <- threshv
# Calculate the informedness
informv <- 2 - rowSums(errorr)
plot(threshv, informv, t="l", main="Informedness")
# Find the threshold corresponding to highest informedness
threshm <- threshv[which.max(informv)]
topf <- (fcasts > threshm)

# Calculate the area under ROC curve (AUC)
errorr <- rbind(c(1, 0), errorr)
errorr <- rbind(errorr, c(0, 1))
truepos <- (1 - errorr[, "typeII"])
truepos <- (truepos + rutils::lagit(truepos))/2
falsepos <- rutils::diffit(errorr[, "typeI"])
abs(sum(truepos*falsepos))
# Plot ROC Curve for stock tops
plot(x=errorr[, "typeI"], y=1-errorr[, "typeII"],
     xlab="FALSE positive rate", ylab="TRUE positive rate",
     main="ROC Curve for Stock Tops", type="l", lwd=3, col="blue")
abline(a=0.0, b=1.0, lwd=3, col="orange")

# Fit in-sample logistic regression for bottoms
logmod <- glm(bottoms ~ predm, family=binomial(logit))
summary(logmod)
# Calculate the in-sample forecast from logistic regression model
coeff <- logmod$coefficients
fcasts <- drop(cbind(rep(1, nrows), predm) %*% coeff)
fcasts <- 1/(1 + exp(-fcasts))
# Calculate the error rates
errorr <- sapply(threshv, confun,
  actual=!bottoms, fcasts=fcasts)  # end sapply
errorr <- t(errorr)
rownames(errorr) <- threshv
# Calculate the informedness
informv <- 2 - rowSums(errorr)
plot(threshv, informv, t="l", main="Informedness")
# Find the threshold corresponding to highest informedness
threshm <- threshv[which.max(informv)]
botf <- (fcasts > threshm)

# Calculate the area under ROC curve (AUC)
errorr <- rbind(c(1, 0), errorr)
errorr <- rbind(errorr, c(0, 1))
truepos <- (1 - errorr[, "typeII"])
truepos <- (truepos + rutils::lagit(truepos))/2
falsepos <- rutils::diffit(errorr[, "typeI"])
abs(sum(truepos*falsepos))
# Plot ROC Curve for stock tops
plot(x=errorr[, "typeI"], y=1-errorr[, "typeII"],
     xlab="FALSE positive rate", ylab="TRUE positive rate",
     main="ROC Curve for Stock Bottoms", type="l", lwd=3, col="blue")
abline(a=0.0, b=1.0, lwd=3, col="orange")

# Average the signals over time
topsav <- HighFreq::roll_sum(matrix(topf), 5)/5
botsav <- HighFreq::roll_sum(matrix(botf), 5)/5
# Simulate in-sample VTI strategy
posv <- (botsav - topsav)
# Standard strategy
# posv <- rep(NA_integer_, NROW(retp))
# posv[1] <- 0
# posv[topf] <- (-1)
# posv[botf] <- 1
# posv <- zoo::na.locf(posv)
posv <- rutils::lagit(posv)
pnls <- retp*posv

# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of in-sample VTI strategy
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Logistic Top and Bottom Strategy In-Sample") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=200)

# Define in-sample and out-of-sample intervals
insample <- 1:(nrows %/% 2)
outsample <- (nrows %/% 2 + 1):nrows
# Fit in-sample logistic regression for tops
logmod <- glm(tops[insample] ~ predm[insample, ], family=binomial(logit))
fitv <- logmod$fitted.values
coefftop <- logmod$coefficients
# Calculate the error rates and best threshold value
errorr <- sapply(threshv, confun,
  actual=!tops[insample], fcasts=fitv)  # end sapply
errorr <- t(errorr)
informv <- 2 - rowSums(errorr)
threshtop <- threshv[which.max(informv)]
# Fit in-sample logistic regression for bottoms
logmod <- glm(bottoms[insample] ~ predm[insample, ], family=binomial(logit))
fitv <- logmod$fitted.values
coeffbot <- logmod$coefficients
# Calculate the error rates and best threshold value
errorr <- sapply(threshv, confun,
  actual=!bottoms[insample], fcasts=fitv)  # end sapply
errorr <- t(errorr)
informv <- 2 - rowSums(errorr)
threshbot <- threshv[which.max(informv)]
# Calculate the out-of-sample forecasts from logistic regression model
predictout <- cbind(rep(1, NROW(outsample)), predm[outsample, ])
fcasts <- drop(predictout %*% coefftop)
fcasts <- 1/(1 + exp(-fcasts))
topf <- (fcasts > threshtop)
fcasts <- drop(predictout %*% coeffbot)
fcasts <- 1/(1 + exp(-fcasts))
botf <- (fcasts > threshbot)

# Simulate in-sample VTI strategy
topsav <- HighFreq::roll_sum(matrix(topf), 5)/5
botsav <- HighFreq::roll_sum(matrix(botf), 5)/5
posv <- (botsav - topsav)
posv <- rutils::lagit(posv)
pnls <- retp[outsample, ]*posv
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retp[outsample, ], pnls)
colnames(wealthv) <- c("VTI", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of in-sample VTI strategy
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Logistic Strategy Out-of-Sample") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=200)

# Calculate the VTI daily percentage returns
retp <- na.omit(rutils::etfenv$returns$VTI)
# Calculate the autocorrelations of VTI daily returns
rutils::plot_acf(retp)
# Simulate mean reverting strategy
posv <- rutils::lagit(sign(retp), lagg=1)
pnls <- (-retp*posv)

# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of mean reverting strategy
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="VTI Daily Mean Reverting Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Simulate mean reverting strategy with two day holding period
posv <- rutils::lagit(rutils::roll_sum(sign(retp), look_back=2))/2
pnls <- (-retp*posv)

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
posv <- rutils::lagit(rutils::roll_sum(sign(retp), look_back=2))/2
pnls <- (-retp*posv)

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
  posv <- rutils::roll_sum(sign(retp), look_back=2)/2
  posv <- rutils::lagit(posv)
  pnls <- (-retp*posv)
  pnls
}, mc.cores=ncores)  # end mclapply
pnls <- do.call(cbind, pnll)
pnls <- rowMeans(pnls, na.rm=TRUE)
# Calculate the average returns of all S&P500 stocks
datev <- zoo::index(retstock)
indeks <- rowMeans(retstock, na.rm=TRUE)

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
stdev <- mclapply(retstock, function(retp) {
  sd(na.omit(retp))
}, mc.cores=ncores)  # end mclapply
stdev <- do.call(c, stdev)
# Calculate the median volatility
medianv <- median(stdev)
# Calculate the pnls for low volatility stocks
pnlovol <- do.call(cbind, pnll[stdev < medianv])
pnlovol <- rowMeans(pnlovol, na.rm=TRUE)
# Calculate the pnls for high volatility stocks
pnlhivol <- do.call(cbind, pnll[stdev >= medianv])
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
# Calculate the EWMA returns recursively using C++ code
retma <- HighFreq::run_mean(retp, lambda=0.1)
# Calculate the positions and pnls
posv <- -retma
# posv <- -sign(retma)
posv <- rutils::lagit(posv, lagg=1)
pnls <- retp*posv
pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])

# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of mean reverting strategy
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="VTI EWMA Daily Mean Reverting Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Calculate the trailing volatility
volat <- HighFreq::run_var(retp, lambda=0.5)
volat <- sqrt(volat)
# Scale the returns by their trailing volatility
retsc <- ifelse(volat > 0, retp/volat, 0)
# Calculate the EWMA returns
retma <- HighFreq::run_mean(retsc, lambda=0.1)
# Calculate the positions and pnls
posv <- retma
# posv <- sign(retma)
posv <- rutils::lagit(posv, lagg=1)
pnls <- (-retp*posv)
pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])

# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of mean reverting strategy
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="VTI EWMA Daily Mean Reverting Strategy") %>%
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
vars <- sum(resids^2)/(nrows-NROW(coeff))

# Calculate the predictor matrix squared
predm2 <- crossprod(predm)
# Calculate the covariance matrix of the AR coefficients
covar <- vars*MASS::ginv(predm2)
coeffsd <- sqrt(diag(covar))
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
fcastsc <- ifelse(fcastv > 0, fcasts/fcastv, 0)
# Simulate autoregressive strategy in-sample
pnls <- retp*fcastsc
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

lambdav <- c(0.5, 1, 1.5)
colorv <- c("red", "blue", "green")
# Define the winsor function
winsorfun <- function(retp, lambda) tanh(lambda*retp)
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
   paste("lambda", lambdav, sep="="), inset=0.0, cex=1.0,
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

# Perform rolling forecasting
look_back <- 100
fcasts <- sapply((look_back+1):nrows, function(tday) {
  # Define rolling look-back range
  startp <- max(1, tday-look_back)
  # Or expanding look-back range
  # startp <- 1
  rangev <- startp:(tday-1) # In-sample range
  # Invert the predictor matrix
  predinv <- MASS::ginv(predm[rangev, ])
  # Calculate the fitted AR coefficients
  coeff <- predinv %*% respv[rangev]
  # Calculate the out-of-sample forecast
  predm[tday, ] %*% coeff
})  # end sapply
# Add warmup period
fcasts <- c(rep(0, look_back), fcasts)

# Define backtesting function
sim_fcasts <- function(look_back=100, ordern=5, fixedlb=TRUE) {
  # Perform rolling forecasting
  fcasts <- sapply((look_back+1):nrows, function(tday) {
    # Define rolling look-back range
    if (fixedlb)
startp <- max(1, tday-look_back) # Fixed look-back
    else
startp <- 1 # Expanding look-back
    rangev <- startp:(tday-1) # In-sample range
    # Invert the predictor matrix
    predinv <- MASS::ginv(predm[rangev, 1:ordern])
    # Calculate the fitted AR coefficients
    coeff <- predinv %*% respv[rangev]
    # Calculate the out-of-sample forecast
    predm[tday, 1:ordern] %*% coeff
  })  # end sapply
  # Add warmup period
  fcasts <- c(rep(0, look_back), fcasts)
}  # end sim_fcasts
# Simulate the rolling autoregressive forecasts
fcasts <- sim_fcasts(look_back=100, ordern=5)
c(mse=mean((fcasts - retp)^2), cor=cor(retp, fcasts))

library(parallel)  # Load package parallel
# Calculate the number of available cores
ncores <- detectCores() - 1
# Initialize compute cluster under Windows
cluster <- makeCluster(ncores)
# Perform parallel loop under Windows
look_backs <- seq(20, 600, 40)
fcasts <- parLapply(cluster, look_backs, sim_fcasts, ordern=6)
# Perform parallel bootstrap under Mac-OSX or Linux
fcasts <- mclapply(look_backs, sim_fcasts, ordern=6, mc.cores=ncores)

# Calculate the mean squared errors
mse <- sapply(fcasts, function(x) {
  c(mse=mean((retp - x)^2), cor=cor(retp, x))
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
# Calculate the number of available cores
ncores <- detectCores() - 1
# Initialize compute cluster under Windows
cluster <- makeCluster(ncores)
# Perform parallel loop under Windows
orderv <- 2:6
fcasts <- parLapply(cluster, orderv, sim_fcasts,
  look_back=look_back)
stopCluster(cluster)  # Stop R processes over cluster under Windows
# Perform parallel bootstrap under Mac-OSX or Linux
fcasts <- mclapply(orderv, sim_fcasts,
  look_back=look_back, mc.cores=ncores)

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
fcasts <- sim_fcasts(look_back=look_back, ordern=ordern)
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
cluster <- makeCluster(ncores)
# Perform parallel loop under Windows
orderv <- 2:6
fcasts <- parLapply(cluster, orderv, sim_fcasts,
  look_back=look_back, fixedlb=FALSE)
stopCluster(cluster)  # Stop R processes over cluster under Windows
# Perform parallel bootstrap under Mac-OSX or Linux
fcasts <- mclapply(orderv, sim_fcasts,
  look_back=look_back, fixedlb=FALSE, mc.cores=ncores)

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
fcasts <- sim_fcasts(look_back=look_back, ordern=ordern, fixedlb=FALSE)
# Calculate the strategy PnLs
pnls <- fcasts*retp
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
colnames(retd) <- "intraday"
reton <- (openp - rutils::lagit(closep, lagg=1, pad_zeros=FALSE))
colnames(reton) <- "overnight"

# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retp, reton, retd)
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot log wealth
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Wealth of Close-to-Close, Overnight, and Intraday Strategies") %>%
  dySeries(name="daily", strokeWidth=2, col="blue") %>%
  dySeries(name="overnight", strokeWidth=2, col="red") %>%
  dySeries(name="intraday", strokeWidth=2, col="green") %>%
  dyLegend(width=600)

# Load the roundtrip trades
dtable <- data.table::fread("/Users/jerzy/Develop/lecture_slides/data/roundtrip_trades.csv")
nrows <- NROW(dtable)
class(dtable$timefill)
# Sort the trades according to the execution time
dtable <- dtable[order(dtable$timefill)]
# Calculate the bid-ask spread
priceb <- dtable$price[dtable$side == "buy"]
pricese <- dtable$price[dtable$side == "sell"]
bidask <- mean(priceb-pricese)

# Calculate the autocorrelations of intraday and overnight returns
pacfl <- pacf(retd, lag.max=10, plot=FALSE)
sum(pacfl$acf)
pacfl <- pacf(reton, lag.max=10, plot=FALSE)
sum(pacfl$acf)
# Calculate the EWMA returns recursively using C++ code
retma <- HighFreq::run_mean(retd, lambda=0.4)
# Calculate the positions and pnls
posv <- rutils::lagit(sign(retma), lagg=1)
pnls <- (-retd*posv)

# Calculate the pnls and the transaction costs
bidask <- 0.0001 # Bid-ask spread equal to 1 basis point
costs <- 0.5*bidask*abs(rutils::diffit(posv))
pnls <- (pnls - costs)
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retd, pnls)
colnames(wealthv) <- c("VTI intraday", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
+ c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of EWMA strategy
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="EWMA Strategy For Intraday VTI Returns") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Calculate the pnls and the transaction costs
posv <- sign(reton)
pnls <- posv*retd
costs <- 0.5*bidask*abs(rutils::diffit(posv))
pnls <- (pnls - costs)

# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retd, pnls)
colnames(wealthv) <- c("VTI intraday", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of EWMA strategy
dygraphs::dygraph(cumsum(wealthv)[endd],
main="Overnight Trend For Intraday VTI Returns") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Calculate the z-scores of the cumulative intraday returns
retc <- cumsum(retd)
lambda <- 0.24
retm <- rutils::lagit(HighFreq::run_mean(retc, lambda=lambda))
retv <- sqrt(rutils::lagit(HighFreq::run_var(retc, lambda=lambda)))
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
colnames(wealthv) <- c("VTI intraday", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of intraday Bollinger strategy
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Bollinger strategy For Intraday VTI Returns") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
