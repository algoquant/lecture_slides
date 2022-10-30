# Test if IEF can time VTI
retsp <- na.omit(rutils::etfenv$returns[, c("IEF", "VTI")])
vti <- retsp$VTI
design <- cbind(retsp, 0.5*(vti+abs(vti)), vti^2)
colnames(design)[3:4] <- c("merton", "treynor")

# Merton-Henriksson test
model <- lm(IEF ~ VTI + merton, data=design); summary(model)
# Treynor-Mazuy test
model <- lm(IEF ~ VTI + treynor, data=design); summary(model)
# Plot residual scatterplot
x11(width=6, height=5)
residuals <- (design$IEF - model$coeff["VTI"]*vti)
plot.default(x=vti, y=residuals, xlab="VTI", ylab="IEF")
title(main="Treynor-Mazuy Market Timing Test\n for IEF vs VTI", line=0.5)
# Plot fitted (predicted) response values
fittedv <- (model$coeff["(Intercept)"] + model$coeff["treynor"]*vti^2)
points.default(x=vti, y=fittedv, pch=16, col="red")
text(x=0.05, y=0.8*max(residuals), paste("Treynor test t-value =", round(summary(model)$coeff["treynor", "t value"], 2)))

# Extract time series of VTI log prices
closep <- log(na.omit(rutils::etfenv$prices$VTI))
# Inspect the R code of the function filter()
filter
# Calculate EWMA weights
look_back <- 21
weightv <- exp(-0.1*1:look_back)
weightv <- weightv/sum(weightv)
# Calculate convolution using filter()
filtered <- filter(closep, filter=weightv,
              method="convolution", sides=1)
# filter() returns time series of class "ts"
class(filtered)
# Get information about C_cfilter()
getAnywhere(C_cfilter)
# Filter using C_cfilter() over past values (sides=1).
filterfast <- .Call(stats:::C_cfilter, closep, filter=weightv,
               sides=1, circular=FALSE)
all.equal(as.numeric(filtered), filterfast, check.attributes=FALSE)
# Calculate EWMA prices using roll::roll_sum()
weightrev <- rev(weightv)
filtercpp <- roll::roll_sum(closep, width=look_back, weights=weightrev, min_obs=1)
all.equal(filterfast[-(1:look_back)], as.numeric(filtercpp)[-(1:look_back)])
# Benchmark speed of rolling calculations
library(microbenchmark)
summary(microbenchmark(
  filter=filter(closep, filter=weightv, method="convolution", sides=1),
  filterfast=.Call(stats:::C_cfilter, closep, filter=weightv, sides=1, circular=FALSE),
  roll=roll::roll_sum(closep, width=look_back, weights=weightrev)
  ), times=10)[, c(1, 4, 5)]

# Simulate AR process using filter()
nrows <- NROW(closep)
# Calculate ARIMA coefficients and innovations
coeff <- matrix(weightv)/4
ncoeff <- NROW(coeff)
innov <- matrix(rnorm(nrows))
arimav <- filter(x=innov, filter=coeff, method="recursive")
# Get information about C_rfilter()
getAnywhere(C_rfilter)
# Filter using C_rfilter() compiled C++ function directly
arimafast <- .Call(stats:::C_rfilter, innov, coeff,
              double(ncoeff + nrows))
all.equal(as.numeric(arimav), arimafast[-(1:ncoeff)],
    check.attributes=FALSE)
# Filter using C++ code
arimacpp <- HighFreq::sim_ar(coeff, innov)
all.equal(arimafast[-(1:ncoeff)], drop(arimacpp))
# Benchmark speed of the three methods
summary(microbenchmark(
  filter=filter(x=innov, filter=coeff, method="recursive"),
  filterfast=.Call(stats:::C_rfilter, innov, coeff, double(ncoeff + nrows)),
  Rcpp=HighFreq::sim_ar(coeff, innov)
  ), times=10)[, c(1, 4, 5)]

# Calculate trailing EWMA prices using roll::roll_sum()
look_back <- 21
weightv <- exp(-0.1*1:look_back)
weightv <- weightv/sum(weightv)
weightrev <- rev(weightv)
filtered <- roll::roll_sum(closep, width=NROW(weightv), weights=weightrev)
# Copy warmup period
filtered[1:look_back] <- closep[1:look_back]
# Combine prices with smoothed prices
prices <- cbind(closep, filtered)
colnames(prices)[2] <- "VTI Smooth"
# Calculate standard deviations of returns
sapply(rutils::diffit(prices), sd)
# Plot dygraph
dygraphs::dygraph(prices["2009"], main="VTI Prices and Trailing Smoothed Prices") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2)

# Calculate centered EWMA prices using roll::roll_sum()
weightv <- c(weightrev, weightv[-1])
weightv <- weightv/sum(weightv)
filtered <- roll::roll_sum(closep, width=NROW(weightv), weights=weightv, online=FALSE)
# Copy warmup period
filtered[1:(2*look_back)] <- closep[1:(2*look_back)]
# Center the data
filtered <- rutils::lagit(filtered, -(look_back-1), pad_zeros=FALSE)
# Combine prices with smoothed prices
prices <- cbind(closep, filtered)
colnames(prices)[2] <- "VTI Smooth"
# Calculate standard deviations of returns
sapply(rutils::diffit(prices), sd)
# Plot dygraph
dygraphs::dygraph(prices["2009"], main="VTI Prices and Centered Smoothed Prices") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2)

# Calculate VTI log returns
retsp <- rutils::diffit(prices)
# Open plot window
x11(width=6, height=7)
# Set plot parameters
par(oma=c(1, 1, 0, 1), mar=c(1, 1, 1, 1), mgp=c(0, 0.5, 0),
    cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# Set two plot panels
par(mfrow=c(2,1))
# Plot ACF of VTI returns
rutils::plot_acf(retsp[, 1], lag=10, xlab="")
title(main="ACF of VTI Returns", line=-1)
# Plot ACF of smoothed VTI returns
rutils::plot_acf(retsp[, 2], lag=10, xlab="")
title(main="ACF of Smoothed VTI Returns", line=-1)

# Extract log VTI prices
ohlc <- log(rutils::etfenv$VTI)
closep <- quantmod::Cl(ohlc)
colnames(closep) <- "VTI"
nrows <- NROW(closep)
# Calculate EWMA weights
look_back <- 333
lambda <- 0.9
weightv <- lambda^(1:look_back)
weightv <- weightv/sum(weightv)
# Calculate EWMA prices as the convolution
ewmacpp <- HighFreq::roll_wsum(closep, weights=weightv)
prices <- cbind(closep, ewmacpp)
colnames(prices) <- c("VTI", "VTI EWMA")

# Dygraphs plot with custom line colors
colnamev <- colnames(prices)
dygraphs::dygraph(prices["2009"], main="VTI EWMA Prices") %>%
  dySeries(name=colnamev[1], label=colnamev[1], strokeWidth=1, col="blue") %>%
  dySeries(name=colnamev[2], label=colnamev[2], strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=500)
# Standard plot of  EWMA prices with custom line colors
x11(width=6, height=5)
plot_theme <- chart_theme()
colorv <- c("blue", "red")
plot_theme$col$line.col <- colorv
quantmod::chart_Series(prices["2009"], theme=plot_theme,
       lwd=2, name="VTI EWMA Prices")
legend("topleft", legend=colnames(prices),
 inset=0.1, bg="white", lty=1, lwd=6, cex=0.8,
 col=plot_theme$col$line.col, bty="n")

# Calculate EWMA prices recursively using C++ code
ewmar <- .Call(stats:::C_rfilter, closep, lambda, c(as.numeric(closep[1])/(1-lambda), double(NROW(closep))))[-1]
# Or R code
# ewmar <- filter(closep, filter=lambda, init=as.numeric(closep[1, 1])/(1-lambda), method="recursive")
ewmar <- (1-lambda)*ewmar
# Calculate EWMA prices recursively using RcppArmadillo
ewmacpp <- HighFreq::run_mean(closep, lambda=lambda, weights=0)
all.equal(drop(ewmacpp), ewmar)
# Compare the speed of C++ code with RcppArmadillo
library(microbenchmark)
summary(microbenchmark(
  filtercpp=HighFreq::run_mean(closep, lambda=lambda, weights=0),
  rfilter=.Call(stats:::C_rfilter, closep, lambda, c(as.numeric(closep[1])/(1-lambda), double(NROW(closep)))),
  times=10))[, c(1, 4, 5)]

# Dygraphs plot with custom line colors
prices <- cbind(closep, ewmacpp)
colnames(prices) <- c("VTI", "VTI EWMA")
colnamev <- colnames(prices)
dygraphs::dygraph(prices["2009"], main="Recursive VTI EWMA Prices") %>%
  dySeries(name=colnamev[1], label=colnamev[1], strokeWidth=1, col="blue") %>%
  dySeries(name=colnamev[2], label=colnamev[2], strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=500)
# Standard plot of  EWMA prices with custom line colors
x11(width=6, height=5)
plot_theme <- chart_theme()
colorv <- c("blue", "red")
plot_theme$col$line.col <- colorv
quantmod::chart_Series(prices["2009"], theme=plot_theme,
       lwd=2, name="VTI EWMA Prices")
legend("topleft", legend=colnames(prices),
 inset=0.1, bg="white", lty=1, lwd=6, cex=0.8,
 col=plot_theme$col$line.col, bty="n")

# Calculate log OHLC prices and volumes
ohlc <- rutils::etfenv$VTI
closep <- log(quantmod::Cl(ohlc))
colnames(closep) <- "VTI"
volumes <- quantmod::Vo(ohlc)
colnames(volumes) <- "Volume"
nrows <- NROW(closep)
# Calculate the VWAP prices
look_back <- 21
vwap <- roll::roll_sum(closep*volumes, width=look_back, min_obs=1)
volume_roll <- roll::roll_sum(volumes, width=look_back, min_obs=1)
vwap <- vwap/volume_roll
colnames(vwap) <- "VWAP"
prices <- cbind(closep, vwap)

# Dygraphs plot with custom line colors
colorv <- c("blue", "red")
dygraphs::dygraph(prices["2009"], main="VTI VWAP Prices") %>%
  dyOptions(colors=colorv, strokeWidth=2)
# Plot VWAP prices with custom line colors
x11(width=6, height=5)
plot_theme <- chart_theme()
plot_theme$col$line.col <- colorv
quantmod::chart_Series(prices["2009"], theme=plot_theme,
       lwd=2, name="VTI VWAP Prices")
legend("bottomright", legend=colnames(prices),
 inset=0.1, bg="white", lty=1, lwd=6, cex=0.8,
 col=plot_theme$col$line.col, bty="n")

# Calculate VWAP prices recursively using C++ code
lambda <- 0.98
volumer <- .Call(stats:::C_rfilter, volumes, lambda, c(as.numeric(volumes[1])/(1-lambda), double(NROW(volumes))))[-1]
pricer <- .Call(stats:::C_rfilter, volumes*closep, lambda, c(as.numeric(volumes[1]*closep[1])/(1-lambda), double(NROW(closep))))[-1]
vwapr <- pricer/volumer
# Calculate VWAP prices recursively using RcppArmadillo
vwapcpp <- HighFreq::run_mean(closep, lambda=lambda, weights=volumes)
all.equal(vwapr, drop(vwapcpp))
# Dygraphs plot the VWAP prices
prices <- xts(cbind(vwap, vwapr), zoo::index(ohlc))
colnames(prices) <- c("VWAP rolling", "VWAP recursive")
dygraphs::dygraph(prices["2009"], main="VWAP Prices") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)

# Calculate two EWMA prices
look_back <- 21
lambda <- 0.1
weightv <- exp(lambda*1:look_back)
weightv <- weightv/sum(weightv)
ewma_fast <- roll::roll_sum(closep, width=look_back, weights=weightv, min_obs=1)
lambda <- 0.05
weightv <- exp(lambda*1:look_back)
weightv <- weightv/sum(weightv)
ewma_slow <- roll::roll_sum(closep, width=look_back, weights=weightv, min_obs=1)

# Calculate VTI prices
ewmadiff <- (ewma_fast - ewma_slow)
prices <- cbind(closep, ewmadiff)
symbol <- "VTI"
colnames(prices) <- c(symbol, paste(symbol, "Returns"))
# Plot dygraph of VTI Returns
colnamev <- colnames(prices)
dygraphs::dygraph(prices["2009"], main=paste(symbol, "EWMA Returns")) %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=2, col="red")

# Calculate fractional weights
deltav <- 0.1
weightv <- (deltav - 0:(look_back-2)) / 1:(look_back-1)
weightv <- (-1)^(1:(look_back-1))*cumprod(weightv)
weightv <- c(1, weightv)
weightv <- (weightv - mean(weightv))
weightv <- rev(weightv)
# Calculate fractional VTI returns
retsp <- roll::roll_sum(closep, width=look_back, weights=weightv, min_obs=1, online=FALSE)
prices <- cbind(closep, retsp)
symbol <- "VTI"
colnames(prices) <- c(symbol, paste(symbol, "Returns"))
# Plot dygraph of VTI Returns
colnamev <- colnames(prices)
dygraphs::dygraph(prices["2009"], main=paste(symbol, "Fractional Returns")) %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=2, col="red")

# Calculate VTI log returns
closep <- log(quantmod::Cl(rutils::etfenv$VTI))
retsp <- rutils::diffit(closep)
# Perform ADF test for prices
tseries::adf.test(closep)
# Perform ADF test for returns
tseries::adf.test(retsp)

# Calculate fractional VTI returns
deltav <- 0.1*c(1, 3, 5, 7, 9)
retsp <- lapply(deltav, function(deltav) {
  weightv <- (deltav - 0:(look_back-2)) / 1:(look_back-1)
  weightv <- c(1, (-1)^(1:(look_back-1))*cumprod(weightv))
  weightv <- rev(weightv - mean(weightv))
  roll::roll_sum(closep, width=look_back, weights=weightv, min_obs=1, online=FALSE)
})  # end lapply
retsp <- do.call(cbind, retsp)
retsp <- cbind(closep, retsp)
colnames(retsp) <- c("VTI", paste0("frac_", deltav))
# Calculate ADF test statistics
adfstats <- sapply(retsp, function(x)
  suppressWarnings(tseries::adf.test(x)$statistic)
)  # end sapply
names(adfstats) <- colnames(retsp)

# Plot dygraph of fractional VTI returns
colorv <- colorRampPalette(c("blue", "red"))(NCOL(retsp))
colnamev <- colnames(retsp)
dyplot <- dygraphs::dygraph(retsp["2019"], main="Fractional Returns") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col=colorv[1])
for (i in 2:NROW(colnamev))
  dyplot <- dyplot %>%
  dyAxis("y2", label=colnamev[i], independentTicks=TRUE) %>%
  dySeries(name=colnamev[i], axis="y2", label=colnamev[i], strokeWidth=2, col=colorv[i])
dyplot <- dyplot %>% dyLegend(width=500)
dyplot

# Calculate volume z-scores
volumes <- quantmod::Vo(rutils::etfenv$VTI)
look_back <- 21
volumem <- roll::roll_mean(volumes, width=look_back, min_obs=1)
volumesd <- roll::roll_sd(rutils::diffit(volumes), width=look_back, min_obs=1)
volumesd[1] <- 0
volumez <- ifelse(volumesd > 0, (volumes - volumem)/volumesd, 0)
# Plot histogram of volume z-scores
x11(width=6, height=5)
hist(volumez, breaks=1e2)

# Plot dygraph of volume z-scores of VTI prices
prices <- cbind(closep, volumez)
colnames(prices) <- c("VTI", "Z-scores")
colnamev <- colnames(prices)
dygraphs::dygraph(prices["2009"], main="VTI Volume Z-Scores") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=2, col="red")

# Extract VTI log OHLC prices
ohlc <- log(rutils::etfenv$VTI)
# Calculate volatility z-scores
volat <- quantmod::Hi(ohlc)-quantmod::Lo(ohlc)
look_back <- 21
volatm <- roll::roll_mean(volat, width=look_back, min_obs=1)
volatsd <- roll::roll_sd(rutils::diffit(volat), width=look_back, min_obs=1)
volatsd[1] <- 0
volatz <- ifelse(volatsd > 0, (volat - volatm)/volatsd, 0)
# Plot histogram of volatility z-scores
x11(width=6, height=5)
hist(volatz, breaks=1e2)
# Plot scatterplot of volume and volatility z-scores
plot(as.numeric(volatz), as.numeric(volumez),
     xlab="volatility z-score", ylab="volume z-score")
regmod <- lm(volatz ~ volumez)
abline(regmod, col="red", lwd=3)

# Plot dygraph of VTI volatility z-scores
closep <- quantmod::Cl(ohlc)
prices <- cbind(closep, volatz)
colnames(prices) <- c("VTI", "Z-scores")
colnamev <- colnames(prices)
dygraphs::dygraph(prices["2009"], main="VTI Volatility Z-Scores") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=2, col="red")

# Calculate the centered volatility
look_back <- 21
half_back <- look_back %/% 2
retsp <- rutils::diffit(closep)
volat <- roll::roll_sd(retsp, width=look_back, min_obs=1)
volat <- rutils::lagit(volat, lagg=(-half_back))
# Calculate the z-scores of prices
pricez <- (2*closep -
  rutils::lagit(closep, half_back, pad_zeros=FALSE) -
  rutils::lagit(closep, -half_back, pad_zeros=FALSE))
pricez <- ifelse(volat > 0, pricez/volat, 0)

# Plot dygraph of z-scores of VTI prices
prices <- cbind(closep, pricez)
colnames(prices) <- c("VTI", "Z-scores")
colnamev <- colnames(prices)
dygraphs::dygraph(prices["2009"], main="VTI Price Z-Scores") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=2, col="red")

# Calculate thresholds for labeling tops and bottoms
confl <- c(0.2, 0.8)
threshv <- quantile(pricez, confl)
# Calculate the vectors of tops and bottoms
tops <- zoo::coredata(pricez > threshv[2])
colnames(tops) <- "tops"
bottoms <- zoo::coredata(pricez < threshv[1])
colnames(bottoms) <- "bottoms"
# Backtest in-sample VTI strategy
posit <- rep(NA_integer_, nrows)
posit[1] <- 0
posit[tops] <- (-1)
posit[bottoms] <- 1
posit <- zoo::na.locf(posit)
posit <- rutils::lagit(posit)
pnls <- retsp*posit

# Plot dygraph of in-sample VTI strategy
wealthv <- cbind(retsp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
endp <- rutils::calc_endpoints(wealthv, interval="months")
dygraphs::dygraph(cumsum(wealthv)[endp],
  main="VTI Strategy Using In-sample Labels") %>%
  dyAxis("y", label="VTI", independentTicks=TRUE) %>%
  dyAxis("y2", label="Strategy", independentTicks=TRUE) %>%
  dySeries(name="VTI", axis="y", label="VTI", strokeWidth=2, col="blue") %>%
  dySeries(name="Strategy", axis="y2", label="Strategy", strokeWidth=2, col="red")

# Calculate trailing price z-scores
dates <- matrix(as.numeric(zoo::index(closep)))
look_back <- 21
pricez <- drop(HighFreq::roll_zscores(response=closep, design=dates, look_back=look_back))
pricez[1:look_back] <- 0

# Plot dygraph of z-scores of VTI prices
prices <- cbind(closep, pricez)
colnames(prices) <- c("VTI", "Z-scores")
colnamev <- colnames(prices)
dygraphs::dygraph(prices["2009"], main="VTI Price Z-Scores") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=2, col="red")

# Extract time series of VTI log prices
closep <- log(na.omit(rutils::etfenv$prices$VTI))
# Define look-back window
look_back <- 11
# Calculate time series of medians
medianv <- roll::roll_median(closep, width=look_back)
# medianv <- TTR::runMedian(closep, n=look_back)
# Calculate time series of MAD
madv <- HighFreq::roll_var(closep, look_back=look_back, method="nonparametric")
# madv <- TTR::runMAD(closep, n=look_back)
# Calculate time series of z-scores
zscores <- (closep - medianv)/madv
zscores[1:look_back, ] <- 0
tail(zscores, look_back)
range(zscores)

x11(width=6, height=5)
# Plot prices and medians
dygraphs::dygraph(cbind(closep, medianv), main="VTI median") %>%
  dyOptions(colors=c("black", "red"))
# Plot histogram of z-scores
histp <- hist(zscores, col="lightgrey",
  xlab="z-scores", breaks=50, xlim=c(-4, 4),
  ylab="frequency", freq=FALSE, main="Hampel Z-scores histogram")
lines(density(zscores, adjust=1.5), lwd=3, col="blue")

# Calculate one-sided Hampel z-scores
medianv <- roll::roll_median(closep, width=look_back)
# medianv <- TTR::runMedian(closep, n=look_back)
madv <- HighFreq::roll_var(closep, look_back=look_back, method="nonparametric")
# madv <- TTR::runMAD(closep, n=look_back)
zscores <- (closep - medianv)/madv
zscores[1:look_back, ] <- 0
tail(zscores, look_back)
range(zscores)
# Calculate two-sided Hampel z-scores
half_back <- look_back %/% 2
medianv <- rutils::lagit(medianv, lagg=-half_back)
madv <- rutils::lagit(madv, lagg=-half_back)
zscores <- (closep - medianv)/madv
zscores[1:look_back, ] <- 0
tail(zscores, look_back)
range(zscores)

# Extract log VTI prices
ohlc <- log(rutils::etfenv$VTI)
closep <- quantmod::Cl(ohlc)
colnames(closep) <- "VTI"
nrows <- NROW(closep)
# Calculate EWMA weights
look_back <- 333
lambda <- 0.984
weightv <- lambda^(0:look_back)
weightv <- weightv/sum(weightv)
# Calculate EWMA prices as the convolution
ewmacpp <- HighFreq::roll_wsum(closep, weights=weightv)
pricev <- cbind(closep, ewmacpp)
colnames(pricev) <- c("VTI", "VTI EWMA")

# Dygraphs plot with custom line colors
colnamev <- colnames(pricev)
dygraphs::dygraph(pricev["2008/2009"], main="VTI EWMA Prices") %>%
  dySeries(name=colnamev[1], label=colnamev[1], strokeWidth=1, col="blue") %>%
  dySeries(name=colnamev[2], label=colnamev[2], strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=500)
# Standard plot of  EWMA prices with custom line colors
x11(width=6, height=5)
plot_theme <- chart_theme()
colorv <- c("blue", "red")
plot_theme$col$line.col <- colorv
quantmod::chart_Series(pricev["2009"], theme=plot_theme,
       lwd=2, name="VTI EWMA Prices")
legend("topleft", legend=colnames(pricev),
 inset=0.1, bg="white", lty=1, lwd=6, cex=0.8,
 col=plot_theme$col$line.col, bty="n")

# Calculate EWMA prices recursively using C++ code
ewmar <- .Call(stats:::C_rfilter, closep, lambda, c(as.numeric(closep[1])/(1-lambda), double(NROW(closep))))[-1]
# Or R code
# ewmar <- filter(closep, filter=lambda, init=as.numeric(closep[1, 1])/(1-lambda), method="recursive")
ewmar <- (1-lambda)*ewmar
# Calculate EWMA prices recursively using RcppArmadillo
ewmacpp <- HighFreq::run_mean(closep, lambda=lambda, weights=0)
all.equal(drop(ewmacpp), ewmar)
# Compare the speed of C++ code with RcppArmadillo
library(microbenchmark)
summary(microbenchmark(
  run_mean=HighFreq::run_mean(closep, lambda=lambda, weights=0),
  rfilter=.Call(stats:::C_rfilter, closep, lambda, c(as.numeric(closep[1])/(1-lambda), double(NROW(closep)))),
  times=10))[, c(1, 4, 5)]

# Dygraphs plot with custom line colors
pricev <- cbind(closep, ewmacpp)
colnames(pricev) <- c("VTI", "VTI EWMA")
colnamev <- colnames(pricev)
dygraphs::dygraph(pricev["2008/2009"], main="Recursive VTI EWMA Prices") %>%
  dySeries(name=colnamev[1], label=colnamev[1], strokeWidth=1, col="blue") %>%
  dySeries(name=colnamev[2], label=colnamev[2], strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=500)
# Standard plot of  EWMA prices with custom line colors
x11(width=6, height=5)
plot_theme <- chart_theme()
colorv <- c("blue", "red")
plot_theme$col$line.col <- colorv
quantmod::chart_Series(pricev["2009"], theme=plot_theme,
       lwd=2, name="VTI EWMA Prices")
legend("topleft", legend=colnames(pricev),
 inset=0.1, bg="white", lty=1, lwd=6, cex=0.8,
 col=plot_theme$col$line.col, bty="n")

# Calculate positions, either: -1, 0, or 1
indic <- sign(closep - ewmacpp)
posit <- rutils::lagit(indic, lagg=1)
# Create colors for background shading
crossd <- (rutils::diffit(posit) != 0)
shadev <- posit[crossd]
crossd <- c(zoo::index(shadev), end(posit))
shadev <- ifelse(drop(zoo::coredata(shadev)) == 1, "lightgreen", "antiquewhite")
# Create dygraph object without plotting it
dyplot <- dygraphs::dygraph(pricev, main="VTI EWMA Prices") %>%
  dySeries(name=colnamev[1], label=colnamev[1], strokeWidth=1, col="blue") %>%
  dySeries(name=colnamev[2], label=colnamev[2], strokeWidth=3, col="red") %>%
  dyLegend(show="always", width=500)
# Add shading to dygraph object
for (i in 1:NROW(shadev)) {
    dyplot <- dyplot %>% dyShading(from=crossd[i], to=crossd[i+1], color=shadev[i])
}  # end for
# Plot the dygraph object
dyplot
# Equivalent code to the above
# Determine trade dates right after EWMA has crossed prices
indic <- sign(closep - ewmacpp)
crossd <- (rutils::diffit(indic) != 0)
crossd <- which(crossd) + 1
crossd <- crossd[crossd < nrows]
# Calculate positions, either: -1, 0, or 1
posit <- rep(NA_integer_, nrows)
posit[1] <- 0
posit[crossd] <- indic[crossd-1]
posit <- zoo::na.locf(posit, na.rm=FALSE)
posit <- xts::xts(posit, order.by=zoo::index(closep))
# Create indicator for background shading
shadev <- posit[crossd]
crossd <- zoo::index(shadev)
crossd <- c(crossd, end(posit))
shadev <- ifelse(drop(zoo::coredata(shadev)) == 1, "lightgreen", "antiquewhite")

# Standard plot of EWMA prices with position shading
x11(width=6, height=5)
quantmod::chart_Series(pricev, theme=plot_theme,
       lwd=2, name="VTI EWMA Prices")
add_TA(posit > 0, on=-1, col="lightgreen", border="lightgreen")
add_TA(posit < 0, on=-1, col="lightgrey", border="lightgrey")
legend("topleft", legend=colnames(pricev),
 inset=0.1, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")

# Calculate daily profits and losses of EWMA strategy
retsp <- rutils::diffit(closep)  # VTI returns
pnls <- retsp*posit
colnames(pnls) <- "EWMA"
wealthv <- cbind(retsp, pnls)
colnames(wealthv) <- c("VTI", "EWMA PnL")
# Annualized Sharpe ratio of EWMA strategy
sqrt(252)*sapply(wealthv, function (x) mean(x)/sd(x))
# The crossover strategy has a negative correlation to VTI
cor(wealthv)[1, 2]
# Plot dygraph of EWMA strategy wealth
# Create dygraph object without plotting it
colorv <- c("blue", "red")
dyplot <- dygraphs::dygraph(cumsum(wealthv), main="Performance of EWMA Strategy") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Add shading to dygraph object
for (i in 1:NROW(shadev)) {
    dyplot <- dyplot %>%
dyShading(from=crossd[i], to=crossd[i+1], color=shadev[i])
}  # end for
# Plot the dygraph object
dyplot

# Standard plot of EWMA strategy wealth
x11(width=6, height=5)
plot_theme <- chart_theme()
plot_theme$col$line.col <- colorv
quantmod::chart_Series(cumsum(wealthv), theme=plot_theme,
       name="Performance of EWMA Strategy")
add_TA(posit > 0, on=-1, col="lightgreen", border="lightgreen")
add_TA(posit < 0, on=-1, col="lightgrey", border="lightgrey")
legend("top", legend=colnames(wealthv),
 inset=0.05, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")

# Test EWMA crossover market timing of VTI using Treynor-Mazuy test
design <- cbind(pnls, retsp, retsp^2)
design <- na.omit(design)
colnames(design) <- c("EWMA", "VTI", "treynor")
model <- lm(EWMA ~ VTI + treynor, data=design)
summary(model)
# Plot residual scatterplot
residuals <- (design$EWMA - model$coeff["VTI"]*retsp)
residuals <- model$residuals
x11(width=6, height=6)
plot.default(x=retsp, y=residuals, xlab="VTI", ylab="residuals")
title(main="Treynor-Mazuy Market Timing Test\n for EWMA Crossover vs VTI", line=0.5)
# Plot fitted (predicted) response values
fittedv <- (model$coeff["(Intercept)"] +
        model$coeff["treynor"]*retsp^2)
points.default(x=retsp, y=fittedv, pch=16, col="red")
text(x=0.05, y=0.8*max(residuals), paste("EWMA crossover t-value =", round(summary(model)$coeff["treynor", "t value"], 2)))

# Determine trade dates right after EWMA has crossed prices
indic <- sign(closep - ewmacpp)
# Calculate positions from lagged indicator
lagg <- 2
indic <- roll::roll_sum(indic, width=lagg, min_obs=1)
# Calculate positions, either: -1, 0, or 1
posit <- rep(NA_integer_, nrows)
posit[1] <- 0
posit <- ifelse(indic == lagg, 1, posit)
posit <- ifelse(indic == (-lagg), -1, posit)
posit <- zoo::na.locf(posit, na.rm=FALSE)
posit <- xts::xts(posit, order.by=zoo::index(closep))
# Lag the positions to trade in next period
posit <- rutils::lagit(posit, lagg=1)
# Calculate PnLs of lagged strategy
pnlslag <- retsp*posit
colnames(pnlslag) <- "Lagged Strategy"

wealthv <- cbind(pnls, pnlslag)
colnames(wealthv) <- c("EWMA", "Lagged")
# Annualized Sharpe ratios of EWMA strategies
sharper <- sqrt(252)*sapply(wealthv, function (x) mean(x)/sd(x))
# Plot both strategies
dygraphs::dygraph(cumsum(wealthv), main=paste("EWMA Crossover Strategy", paste(paste(names(sharper), round(sharper, 3), sep="="), collapse=", "))) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)

# Calculate positions, either: -1, 0, or 1
indic <- sign(closep - ewmacpp)
posit <- rutils::lagit(indic, lagg=1)
# Calculate daily pnl for days without trades
pnls <- retsp*posit
# Determine trade dates right after EWMA has crossed prices
crossd <- which(rutils::diffit(posit) != 0)
# Calculate realized pnl for days with trades
openp <- quantmod::Op(ohlc)
closelag <- rutils::lagit(closep)
poslag <- rutils::lagit(posit)
pnls[crossd] <- poslag[crossd]*(openp[crossd] - closelag[crossd])
# Calculate unrealized pnl for days with trades
pnls[crossd] <- pnls[crossd] +
  posit[crossd]*(closep[crossd] - openp[crossd])
# Calculate the wealth
wealthv <- cbind(retsp, pnls)
colnames(wealthv) <- c("VTI", "EWMA PnL")
# Annualized Sharpe ratio of EWMA strategy
sqrt(252)*sapply(wealthv, function (x) mean(x)/sd(x))
# The crossover strategy has a negative correlation to VTI
cor(wealthv)[1, 2]

# Plot dygraph of EWMA strategy wealth
endd <- rutils::calc_endpoints(wealthv, interval="months")
dygraphs::dygraph(cumsum(wealthv)[endd], main="EWMA Strategy Trading at the Open Price") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Standard plot of EWMA strategy wealth
quantmod::chart_Series(cumsum(wealthv)[endd], theme=plot_theme,
       name="EWMA Strategy Trading at the Open Price")
legend("top", legend=colnames(wealthv),
 inset=0.05, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")

# bid_offer equal to 10 bps for liquid ETFs
bid_offer <- 0.001
# Calculate transaction costs
costs <- 0.5*bid_offer*abs(poslag - posit)
# Plot strategy with transaction costs
wealthv <- cbind(pnls, pnls - costs)
colnames(wealthv) <- c("EWMA", "EWMA w Costs")
colorv <- c("blue", "red")
dygraphs::dygraph(cumsum(wealthv)[endd], main="EWMA Strategy With Transaction Costs") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=500)

sim_ewma <- function(ohlc, lambda=0.9, look_back=333, bid_offer=0.001,
                trend=1, lagg=1) {
  closep <- quantmod::Cl(ohlc)
  retsp <- rutils::diffit(closep)
  nrows <- NROW(ohlc)
  # Calculate EWMA prices
  ewmacpp <- HighFreq::run_mean(closep, lambda=lambda, weights=0)
  # Calculate the indicator
  indic <- trend*sign(closep - ewmacpp)
  if (lagg > 1) {
    indic <- roll::roll_sum(indic, width=lagg, min_obs=1)
    indic[1:lagg] <- 0
  }  # end if
  # Calculate positions, either: -1, 0, or 1
  posit <- rep(NA_integer_, nrows)
  posit[1] <- 0
  posit <- ifelse(indic == lagg, 1, posit)
  posit <- ifelse(indic == (-lagg), -1, posit)
  posit <- zoo::na.locf(posit, na.rm=FALSE)
  posit <- xts::xts(posit, order.by=zoo::index(closep))
  # Lag the positions to trade on next day
  posit <- rutils::lagit(posit, lagg=1)
  # Calculate PnLs of strategy
  pnls <- retsp*posit
  costs <- 0.5*bid_offer*abs(rutils::diffit(posit))
  pnls <- (pnls - costs)
  # Calculate strategy returns
  pnls <- cbind(posit, pnls)
  colnames(pnls) <- c("positions", "pnls")
  pnls
}  # end sim_ewma

source("/Users/jerzy/Develop/lecture_slides/scripts/ewma_model.R")
lambdas <- seq(from=0.98, to=0.99, by=0.001)
# Perform lapply() loop over lambdas
pnltrend <- lapply(lambdas, function(lambda) {
  # Simulate EWMA strategy and calculate returns
  sim_ewma(ohlc=ohlc, lambda=lambda, look_back=look_back, bid_offer=0, lagg=2)[, "pnls"]
})  # end lapply
pnltrend <- do.call(cbind, pnltrend)
colnames(pnltrend) <- paste0("lambda=", lambdas)

# Plot dygraph of multiple EWMA strategies
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnltrend))
dygraphs::dygraph(cumsum(pnltrend)[endd], main="Cumulative Returns of Trend Following EWMA Strategies") %>%
  dyOptions(colors=colorv, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Plot EWMA strategies with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- colorv
quantmod::chart_Series(cumsum(pnltrend), theme=plot_theme,
  name="Cumulative Returns of EWMA Strategies")
legend("topleft", legend=colnames(pnltrend), inset=0.1,
  bg="white", cex=0.8, lwd=rep(6, NCOL(pnltrend)),
  col=plot_theme$col$line.col, bty="n")

# Initialize compute cluster under Windows
library(parallel)
cluster <- makeCluster(detectCores()-1)
clusterExport(cluster,
  varlist=c("ohlc", "look_back", "sim_ewma"))
# Perform parallel loop over lambdas under Windows
pnltrend <- parLapply(cluster, lambdas, function(lambda) {
  library(quantmod)
  # Simulate EWMA strategy and calculate returns
  sim_ewma(ohlc=ohlc, lambda=lambda, look_back=look_back)[, "pnls"]
})  # end parLapply
stopCluster(cluster)  # Stop R processes over cluster under Windows
# Perform parallel loop over lambdas under Mac-OSX or Linux
pnltrend <- mclapply(lambdas, function(lambda) {
  library(quantmod)
  # Simulate EWMA strategy and calculate returns
  sim_ewma(ohlc=ohlc, lambda=lambda, look_back=look_back)[, "pnls"]
})  # end mclapply
pnltrend <- do.call(cbind, pnltrend)
colnames(pnltrend) <- paste0("lambda=", lambdas)

# Calculate annualized Sharpe ratios of strategy returns
sharpetrend <- sqrt(252)*sapply(pnltrend, function(xtsv) {
  mean(xtsv)/sd(xtsv)
})  # end sapply
# Plot Sharpe ratios
dev.new(width=6, height=5, noRStudioGD=TRUE)
plot(x=lambdas, y=sharpetrend, t="l",
     xlab="lambda", ylab="Sharpe",
     main="Performance of EWMA Trend Following Strategies
     as Function of the Decay Parameter Lambda")

# Calculate optimal lambda
lambda <- lambdas[which.max(sharpetrend)]
# Simulate best performing strategy
ewmatrend <- sim_ewma(ohlc=ohlc, lambda=lambda, bid_offer=0, lagg=2)
posit <- ewmatrend[, "positions"]
trendopt <- ewmatrend[, "pnls"]
wealthv <- cbind(retsp, trendopt)
colnames(wealthv) <- c("VTI", "EWMA PnL")
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
cor(wealthv)[1, 2]
# Plot dygraph of EWMA strategy wealth
dygraphs::dygraph(cumsum(wealthv)[endd], main="Performance of Optimal Trend Following EWMA Strategy") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=500)

# Plot EWMA PnL with position shading
# Standard plot of EWMA strategy wealth
x11(width=6, height=5)
plot_theme <- chart_theme()
plot_theme$col$line.col <- colorv
quantmod::chart_Series(cumsum(wealthv), theme=plot_theme,
       name="Performance of EWMA Strategy")
add_TA(posit > 0, on=-1, col="lightgreen", border="lightgreen")
add_TA(posit < 0, on=-1, col="lightgrey", border="lightgrey")
legend("top", legend=colnames(wealthv),
 inset=0.05, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")

source("/Users/jerzy/Develop/lecture_slides/scripts/ewma_model.R")
lambdas <- seq(0.6, 0.7, 0.01)
# Perform lapply() loop over lambdas
pnlrevert <- lapply(lambdas, function(lambda) {
  # Simulate EWMA strategy and calculate returns
  sim_ewma(ohlc=ohlc, lambda=lambda, bid_offer=0, trend=(-1))[, "pnls"]
})  # end lapply
pnlrevert <- do.call(cbind, pnlrevert)
colnames(pnlrevert) <- paste0("lambda=", lambdas)
# Plot dygraph of mean reverting EWMA strategies
colorv <- colorRampPalette(c("blue", "red"))(NROW(lambdas))
dygraphs::dygraph(cumsum(pnlrevert)[endd], main="Cumulative Returns of Mean Reverting EWMA Strategies") %>%
  dyOptions(colors=colorv, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Plot EWMA strategies with custom line colors
x11(width=6, height=5)
plot_theme <- chart_theme()
plot_theme$col$line.col <- colorv
quantmod::chart_Series(pnlrevert,
  theme=plot_theme, name="Cumulative Returns of Mean Reverting EWMA Strategies")
legend("topleft", legend=colnames(pnlrevert),
  inset=0.1, bg="white", cex=0.8, lwd=6,
  col=plot_theme$col$line.col, bty="n")

# Calculate Sharpe ratios of strategy returns
sharperevert <- sqrt(252)*sapply(pnlrevert, function(xtsv) {
  mean(xtsv)/sd(xtsv)
})  # end sapply
plot(x=lambdas, y=sharperevert, t="l",
     xlab="lambda", ylab="Sharpe",
     main="Performance of EWMA Mean Reverting Strategies
     as Function of the Decay Parameter Lambda")

# Calculate optimal lambda
lambda <- lambdas[which.max(sharperevert)]
# Simulate best performing strategy
ewmarevert <- sim_ewma(ohlc=ohlc, bid_offer=0.0,
  lambda=lambda, trend=(-1))
posit <- ewmarevert[, "positions"]
revertopt <- ewmarevert[, "pnls"]
wealthv <- cbind(retsp, revertopt)
colnames(wealthv) <- c("VTI", "EWMA PnL")
# Plot dygraph of EWMA strategy wealth
colorv <- c("blue", "red")
dygraphs::dygraph(cumsum(wealthv)[endd], main="Optimal Mean Reverting EWMA Strategy") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=500)

# Standard plot of EWMA strategy wealth
x11(width=6, height=5)
plot_theme <- chart_theme()
plot_theme$col$line.col <- colorv
quantmod::chart_Series(cumsum(wealthv), theme=plot_theme,
       name="Optimal Mean Reverting EWMA Strategy")
add_TA(posit > 0, on=-1, col="lightgreen", border="lightgreen")
add_TA(posit < 0, on=-1, col="lightgrey", border="lightgrey")
legend("top", legend=colnames(wealthv),
 inset=0.05, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")

# Calculate correlation between trend following and mean reverting strategies
trendopt <- ewmatrend[, "pnls"]
colnames(trendopt) <- "trend"
revertopt <- ewmarevert[, "pnls"]
colnames(revertopt) <- "revert"
cor(cbind(retsp, trendopt, revertopt))
# Calculate combined strategy
combstrat <- (retsp + trendopt + revertopt)/3
colnames(combstrat) <- "combined"
# Calculate annualized Sharpe ratio of strategy returns
retsp <- cbind(retsp, trendopt, revertopt, combstrat)
colnames(retsp) <- c("VTI", "Trending", "Reverting", "EWMA combined")
sqrt(252)*sapply(retsp, function(xtsv) mean(xtsv)/sd(xtsv))

# Plot dygraph of EWMA strategy wealth
colorv <- c("blue", "red", "green", "purple")
dygraphs::dygraph(cumsum(retsp)[endd], main="Performance of Combined EWMA Strategies") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Standard plot of EWMA strategy wealth
plot_theme <- chart_theme()
plot_theme$col$line.col <- colorv
quantmod::chart_Series(pnls, theme=plot_theme,
       name="Performance of Combined EWMA Strategies")
legend("topleft", legend=colnames(pnls),
 inset=0.05, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")

# Calculate weights proportional to Sharpe ratios
weightv <- c(sharpetrend, sharperevert)
weightv[weightv<0] <- 0
weightv <- weightv/sum(weightv)
retsp <- cbind(pnltrend, pnlrevert)
retsp <- retsp %*% weightv
retsp <- xts::xts(retsp, order.by=zoo::index(retsp))
retsp <- cbind(retsp, retsp)
colnames(retsp) <- c("VTI", "EWMA PnL")
# Plot dygraph of EWMA strategy wealth
colorv <- c("blue", "red")
dygraphs::dygraph(cumsum(retsp)[endd], main="Performance of Ensemble of EWMA Strategies") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Standard plot of EWMA strategy wealth
plot_theme <- chart_theme()
plot_theme$col$line.col <- colorv
quantmod::chart_Series(cumsum(retsp), theme=plot_theme,
       name="Performance of Ensemble of EWMA Strategies")
legend("topleft", legend=colnames(pnls),
 inset=0.05, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")

# Calculate fast and slow EWMAs
look_back <- 333
lambda1 <- 0.89
lambda2 <- 0.95
# Calculate EWMA prices
ewma1 <- HighFreq::run_mean(closep, lambda=lambda1, weights=0)
ewma2 <- HighFreq::run_mean(closep, lambda=lambda2, weights=0)
# Calculate EWMA prices
pricev <- cbind(closep, ewma1, ewma2)
colnames(pricev) <- c("VTI", "EWMA fast", "EWMA slow")
# Calculate positions, either: -1, 0, or 1
indic <- sign(ewma1 - ewma2)
lagg <- 2
indic <- roll::roll_sum(indic, width=lagg, min_obs=1)
posit <- rep(NA_integer_, nrows)
posit[1] <- 0
posit <- ifelse(indic == lagg, 1, posit)
posit <- ifelse(indic == (-lagg), -1, posit)
posit <- zoo::na.locf(posit, na.rm=FALSE)
posit <- xts::xts(posit, order.by=zoo::index(closep))
posit <- rutils::lagit(posit, lagg=1)

# Create colors for background shading
crossd <- (rutils::diffit(posit) != 0)
shadev <- posit[crossd]
crossd <- c(zoo::index(shadev), end(posit))
shadev <- ifelse(drop(zoo::coredata(shadev)) == 1, "lightgreen", "antiquewhite")
# Plot dygraph
colnamev <- colnames(pricev)
dyplot <- dygraphs::dygraph(pricev[endd], main="VTI Dual EWMA Prices") %>%
  dySeries(name=colnamev[1], label=colnamev[1], strokeWidth=1, col="blue") %>%
  dySeries(name=colnamev[2], label=colnamev[2], strokeWidth=2, col="red") %>%
  dySeries(name=colnamev[3], label=colnamev[3], strokeWidth=2, col="purple") %>%
  dyLegend(show="always", width=500)
for (i in 1:NROW(shadev)) {
    dyplot <- dyplot %>% dyShading(from=crossd[i], to=crossd[i+1], color=shadev[i])
}  # end for
dyplot

# Calculate daily profits and losses of strategy
pnls <- retsp*posit
colnames(pnls) <- "Strategy"
wealthv <- cbind(retsp, pnls)
# Annualized Sharpe ratio of Dual EWMA strategy
sharper <- sqrt(252)*sapply(wealthv, function (x) mean(x)/sd(x))
# The crossover strategy has a negative correlation to VTI
cor(wealthv)[1, 2]

# Plot Dual EWMA strategy
dyplot <- dygraphs::dygraph(cumsum(wealthv), main=paste("EWMA Dual Crossover Strategy, Sharpe", paste(paste(names(sharper), round(sharper, 3), sep="="), collapse=", "))) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2)
# Add shading to dygraph object
for (i in 1:NROW(shadev)) {
    dyplot <- dyplot %>% dyShading(from=crossd[i], to=crossd[i+1], color=shadev[i])
}  # end for
# Plot the dygraph object
dyplot

sim_ewma2 <- function(ohlc, lambda1=0.1, lambda2=0.01, look_back=333,
                bid_offer=0.001, trend=1, lagg=1) {
  closep <- quantmod::Cl(ohlc)
  retsp <- rutils::diffit(closep)
  nrows <- NROW(ohlc)
  # Calculate EWMA prices
  ewma1 <- HighFreq::run_mean(closep, lambda=lambda1, weights=0)
  ewma2 <- HighFreq::run_mean(closep, lambda=lambda2, weights=0)
  # Calculate positions, either: -1, 0, or 1
  indic <- sign(ewma1 - ewma2)
  if (lagg > 1) {
    indic <- roll::roll_sum(indic, width=lagg, min_obs=1)
    indic[1:lagg] <- 0
  }  # end if
  posit <- rep(NA_integer_, nrows)
  posit[1] <- 0
  posit <- ifelse(indic == lagg, 1, posit)
  posit <- ifelse(indic == (-lagg), -1, posit)
  posit <- zoo::na.locf(posit, na.rm=FALSE)
  posit <- xts::xts(posit, order.by=zoo::index(closep))
  # Lag the positions to trade on next day
  posit <- rutils::lagit(posit, lagg=1)
  # Calculate PnLs of strategy
  pnls <- retsp*posit
  costs <- 0.5*bid_offer*abs(rutils::diffit(posit))
  pnls <- (pnls - costs)
  # Calculate strategy returns
  pnls <- cbind(posit, pnls)
  colnames(pnls) <- c("positions", "pnls")
  pnls
}  # end sim_ewma2

source("/Users/jerzy/Develop/lecture_slides/scripts/ewma_model.R")
lambdas1 <- seq(from=0.85, to=0.99, by=0.01)
lambdas2 <- seq(from=0.85, to=0.99, by=0.01)
# Perform sapply() loops over lambdas
sharpem <- sapply(lambdas1, function(lambda1) {
  sapply(lambdas2, function(lambda2) {
    if (lambda2 > lambda1) {
# Simulate Dual EWMA strategy
pnls <- sim_ewma2(ohlc=ohlc, lambda1=lambda1, lambda2=lambda2,
                    look_back=look_back, bid_offer=0.0, trend=1, lagg=2)[, "pnls"]
sqrt(252)*mean(pnls)/sd(pnls)
    } else NA
  })  # end sapply
})  # end sapply
colnames(sharpem) <- lambdas1
rownames(sharpem) <- lambdas2
# Calculate the PnLs for the optimal strategy
whichv <- which(sharpem == max(sharpem, na.rm=TRUE), arr.ind=TRUE)
lambda1 <- lambdas1[whichv[2]]
lambda2 <- lambdas2[whichv[1]]
pnls <- sim_ewma2(ohlc=ohlc, lambda1=lambda1, lambda2=lambda2,
              look_back=look_back, bid_offer=0.0, trend=1, lagg=2)[, "pnls"]
wealthv <- cbind(retsp, pnls)
colnames(wealthv)[2] <- "EWMA"

# Annualized Sharpe ratio of Dual EWMA strategy
sharper <- sqrt(252)*sapply(wealthv, function (x) mean(x)/sd(x))
# The crossover strategy has a negative correlation to VTI
cor(wealthv)[1, 2]
# Plot Optimal Dual EWMA strategy
dyplot <- dygraphs::dygraph(cumsum(wealthv), main=paste("Optimal EWMA Dual Crossover Strategy, Sharpe", paste(paste(names(sharper), round(sharper, 3), sep="="), collapse=", "))) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2)
# Add shading to dygraph object
for (i in 1:NROW(shadev)) {
    dyplot <- dyplot %>% dyShading(from=crossd[i], to=crossd[i+1], color=shadev[i])
}  # end for
# Plot the dygraph object
dyplot

# Calculate positions from lagged indicator
indic <- sign(closep - vwapcpp)
lagg <- 2
indic <- roll::roll_sum(indic, width=lagg, min_obs=1)
# Calculate positions, either: -1, 0, or 1
posit <- rep(NA_integer_, nrows)
posit[1] <- 0
posit <- ifelse(indic == lagg, 1, posit)
posit <- ifelse(indic == (-lagg), -1, posit)
posit <- zoo::na.locf(posit, na.rm=FALSE)
posit <- xts::xts(posit, order.by=zoo::index(closep))
# Lag the positions to trade in next period
posit <- rutils::lagit(posit, lagg=1)
# Calculate PnLs of VWAP strategy
retsp <- rutils::diffit(closep)  # VTI returns
pnls <- retsp*posit
colnames(pnls) <- "VWAP"
wealthv <- cbind(retsp, pnls)
colnamev <- colnames(wealthv)
# Annualized Sharpe ratios of VTI and VWAP strategy
sharper <- sqrt(252)*sapply(wealthv, function (x) mean(x)/sd(x))

# Create colors for background shading
crossd <- (rutils::diffit(posit) != 0)
shadev <- posit[crossd]
crossd <- c(zoo::index(shadev), end(posit))
shadev <- ifelse(drop(zoo::coredata(shadev)) == 1, "lightgreen", "antiquewhite")
# Plot dygraph of VWAP strategy
# Create dygraph object without plotting it
dyplot <- dygraphs::dygraph(cumsum(wealthv), main=paste("VWAP Crossover Strategy, Sharpe", paste(paste(names(sharper), round(sharper, 3), sep="="), collapse=", "))) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Add shading to dygraph object
for (i in 1:NROW(shadev)) {
    dyplot <- dyplot %>% dyShading(from=crossd[i], to=crossd[i+1], color=shadev[i])
}  # end for
# Plot the dygraph object
dyplot

# Calculate correlation of VWAP strategy with VTI
cor(retsp, pnls)
# Combine VWAP strategy with VTI
wealthv <- cbind(retsp, pnls, 0.5*(retsp+pnls))
colnames(wealthv) <- c("VTI", "VWAP", "Combined")
sharper <- sqrt(252)*sapply(wealthv, function (x) mean(x)/sd(x))

# Plot dygraph of VWAP strategy combined with VTI
colnamev <- colnames(wealthv)
dygraphs::dygraph(cumsum(wealthv)[endd], paste("VWAP Strategy Sharpe", paste(paste(names(sharper), round(sharper, 3), sep="="), collapse=", "))) %>%
  dySeries(name=colnamev[1], label=colnamev[1], col="blue", strokeWidth=1) %>%
  dySeries(name=colnamev[2], label=colnamev[2], col="red", strokeWidth=1) %>%
  dySeries(name=colnamev[3], label=colnamev[3], col="purple", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Or
dygraphs::dygraph(cumsum(wealthv)[endd],
  main=paste("VWAP Strategy Sharpe", paste(paste(names(sharper), round(sharper, 3), sep="="), collapse=", "))) %>%
  dyOptions(colors=c("blue", "red", "purple"), strokeWidth=1) %>%
  dyLegend(show="always", width=500)

# Test VWAP crossover market timing of VTI using Treynor-Mazuy test
design <- cbind(pnls, retsp, retsp^2)
design <- na.omit(design)
colnames(design) <- c("VWAP", "VTI", "treynor")
model <- lm(VWAP ~ VTI + treynor, data=design)
summary(model)
# Plot residual scatterplot
residuals <- (design$VWAP - model$coeff["VTI"]*retsp)
residuals <- model$residuals
# x11(width=6, height=6)
plot.default(x=retsp, y=residuals, xlab="VTI", ylab="residuals")
title(main="Treynor-Mazuy Market Timing Test\n for VWAP Crossover vs VTI", line=0.5)
# Plot fitted (predicted) response values
fittedv <- (model$coeff["(Intercept)"] + model$coeff["treynor"]*retsp^2)
points.default(x=retsp, y=fittedv, pch=16, col="red")
text(x=0.05, y=0.8*max(residuals), paste("VWAP crossover t-value =", round(summary(model)$coeff["treynor", "t value"], 2)))

sim_vwap <- function(ohlc, lambda=0.9, bid_offer=0.001, trend=1, lagg=1) {
  closep <- log(quantmod::Cl(ohlc))
  volumes <- quantmod::Vo(ohlc)
  retsp <- rutils::diffit(closep)
  nrows <- NROW(ohlc)
  # Calculate VWAP prices
  vwap <- HighFreq::run_mean(closep, lambda=lambda, weights=volumes)
  # Calculate the indicator
  indic <- trend*sign(closep - vwap)
  if (lagg > 1) {
    indic <- roll::roll_sum(indic, width=lagg, min_obs=1)
    indic[1:lagg] <- 0
  }  # end if
  # Calculate positions, either: -1, 0, or 1
  posit <- rep(NA_integer_, nrows)
  posit[1] <- 0
  posit <- ifelse(indic == lagg, 1, posit)
  posit <- ifelse(indic == (-lagg), -1, posit)
  posit <- zoo::na.locf(posit, na.rm=FALSE)
  posit <- xts::xts(posit, order.by=zoo::index(closep))
  # Lag the positions to trade on next day
  posit <- rutils::lagit(posit, lagg=1)
  # Calculate PnLs of strategy
  pnls <- retsp*posit
  costs <- 0.5*bid_offer*abs(rutils::diffit(posit))
  pnls <- (pnls - costs)
  # Calculate strategy returns
  pnls <- cbind(posit, pnls)
  colnames(pnls) <- c("positions", "pnls")
  pnls
}  # end sim_vwap

source("/Users/jerzy/Develop/lecture_slides/scripts/ewma_model.R")
lambdas <- seq(from=0.97, to=0.995, by=0.002)
# Perform lapply() loop over lambdas
pnls <- lapply(lambdas, function(lambda) {
  # Simulate VWAP strategy and calculate returns
  sim_vwap(ohlc=ohlc, lambda=lambda, bid_offer=0, lagg=2)[, "pnls"]
})  # end lapply
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("lambda=", lambdas)

# Plot dygraph of multiple VWAP strategies
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls)[endd], main="Cumulative Returns of Trend Following VWAP Strategies") %>%
  dyOptions(colors=colorv, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Plot VWAP strategies with custom line colors
x11(width=6, height=5)
plot_theme <- chart_theme()
plot_theme$col$line.col <- colorv
quantmod::chart_Series(cumsum(pnls), theme=plot_theme,
  name="Cumulative Returns of VWAP Strategies")
legend("topleft", legend=colnames(pnls), inset=0.1,
  bg="white", cex=0.8, lwd=rep(6, NCOL(pnls)),
  col=plot_theme$col$line.col, bty="n")

NA

App setup code that runs only once at startup.
ndata <- 1e4
stdev <- 1.0

Define the user interface
uiface <- shiny::fluidPage(
  # Create numeric input for the number of data points.
  numericInput("ndata", "Number of data points:", value=ndata),
  # Create slider input for the standard deviation parameter.
  sliderInput("stdev", label="Standard deviation:",
        min=0.1, max=3.0, value=stdev, step=0.1),
  # Render plot in a panel.
  plotOutput("plotobj", height=300, width=500)
)  # end user interface

Define the server function
servfun <- function(input, output) {
  output$plotobj <- shiny::renderPlot({
    # Simulate the data
    datav <- rnorm(input$ndata, sd=input$stdev)
    # Plot the data
    par(mar=c(2, 4, 4, 0), oma=c(0, 0, 0, 0))
    hist(datav, xlim=c(-4, 4), main="Histogram of Random Data")
  })  # end renderPlot
}  # end servfun

# Return a Shiny app object
shiny::shinyApp(ui=uiface, server=servfun)

Create elements of the user interface
uiface <- shiny::fluidPage(
  titlePanel("VWAP Moving Average"),
  # Create single row of widgets with two slider inputs
  fluidRow(
    # Input stock symbol
    column(width=3, selectInput("symbol", label="Symbol",
                          choices=symbolv, selected=symbol)),
    # Input look-back interval
    column(width=3, sliderInput("look_back", label="Lookback interval",
                          min=1, max=150, value=11, step=1))
  ),  # end fluidRow
  # Create output plot panel
  mainPanel(dygraphs::dygraphOutput("dyplot"), width=12)
)  # end fluidPage interface

Define the server function
servfun <- shiny::shinyServer(function(input, output) {
  # Get the close and volume data in a reactive environment
  closep <- shiny::reactive({
    # Get the data
    ohlc <- get(input$symbol, data_env)
    closep <- log(quantmod::Cl(ohlc))
    volumes <- quantmod::Vo(ohlc)
    # Return the data
    cbind(closep, volumes)
  })  # end reactive code

  # Calculate the VWAP indicator in a reactive environment
  vwapv <- shiny::reactive({
    # Get model parameters from input argument
    look_back <- input$look_back
    # Calculate the VWAP indicator
    closep <- closep()[, 1]
    volumes <- closep()[, 2]
    vwapv <- HighFreq::roll_sum(se_ries=closep*volumes, look_back=look_back)
    volume_rolling <- HighFreq::roll_sum(se_ries=volumes, look_back=look_back)
    vwapv <- vwapv/volume_rolling
    vwapv[is.na(vwapv)] <- 0
    # Return the plot data
    datav <- cbind(closep, vwapv)
    colnames(datav) <- c(input$symbol, "VWAP")
    datav
  })  # end reactive code

  # Return the dygraph plot to output argument
  output$dyplot <- dygraphs::renderDygraph({
    colnamev <- colnames(vwapv())
    dygraphs::dygraph(vwapv(), main=paste(colnamev[1], "VWAP")) %>%
dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col="blue") %>%
dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=2, col="red")
  })  # end output plot
})  # end server code

Return a Shiny app object
shiny::shinyApp(ui=uiface, server=servfun)

Define the server function
servfun <- shiny::shinyServer(function(input, output) {

  # Create an empty list of reactive values.
  value_s <- reactiveValues()

  # Get input parameters from the user interface.
  nrows <- reactive({
    # Add nrows to list of reactive values.
    value_s*nrows <- input$nrows
    input$nrows
  })  # end reactive code

  # Broadcast a message to the console when the button is pressed.
  observeEvent(eventExpr=input$button, handlerExpr={
    cat("Input button pressed\n")
  })  # end observeEvent

  # Send the data when the button is pressed.
  datav <- eventReactive(eventExpr=input$button, valueExpr={
    # eventReactive() executes on input$button, but not on nrows() or input$nrows.
    cat("Sending", nrows(), "rows of data\n")
    datav <- head(mtcars, input$nrows)
    value_s$mpg <- mean(datav$mpg)
    datav
  })  # end eventReactive
  #   datav

  # Draw table of the data when the button is pressed.
  observeEvent(eventExpr=input$button, handlerExpr={
    datav <- datav()
    cat("Received", value_s*nrows, "rows of data\n")
    cat("Average mpg = ", value_s$mpg, "\n")
    cat("Drawing table\n")
    output$tablev <- renderTable(datav)
  })  # end observeEvent

})  # end server code

Return a Shiny app object
shiny::shinyApp(ui=uiface, server=servfun)
