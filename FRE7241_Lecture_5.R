# Define daily volatility and growth rate
sigmav <- 0.01; drift <- 0.0; nrows <- 1000
# Simulate geometric Brownian motion
retsp <- sigmav*rnorm(nrows) + drift - sigmav^2/2
prices <- exp(cumsum(retsp))
plot(prices, type="l", xlab="time", ylab="prices",
     main="geometric Brownian motion")

# Simulate geometric Brownian motion
sigmav <- 0.01/sqrt(48)
drift <- 0.0
nrows <- 1e4
dates <- seq(from=as.POSIXct(paste(Sys.Date()-250, "09:30:00")),
  length.out=nrows, by="30 min")
prices <- exp(cumsum(sigmav*rnorm(nrows) + drift - sigmav^2/2))
prices <- xts(prices, order.by=dates)
prices <- cbind(prices,
  volume=sample(x=10*(2:18), size=nrows, replace=TRUE))
# Aggregate to daily OHLC data
ohlc <- xts::to.daily(prices)
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

# Define daily volatility and growth rate
sigmav <- 0.01; drift <- 0.0; nrows <- 5000
npaths <- 10
# Simulate multiple paths of geometric Brownian motion
prices <- rnorm(npaths*nrows, sd=sigmav) + drift - sigmav^2/2
prices <- matrix(prices, nc=npaths)
prices <- exp(matrixStats::colCumsums(prices))
# Create xts time series
prices <- xts(prices, order.by=seq.Date(Sys.Date()-NROW(prices)+1, Sys.Date(), by=1))
# Plot xts time series
colorv <- colorRampPalette(c("red", "blue"))(NCOL(prices))
colorv <- colorv[order(order(prices[NROW(prices), ]))]
par(mar=c(3, 3, 2, 2), oma=c(0, 0, 0, 0))
plot.zoo(prices, main="Multiple paths of geometric Brownian motion",
   xlab=NA, ylab=NA, plot.type="single", col=colorv)

# Define daily volatility and growth rate
sigmav <- 0.01; drift <- 0.0; nrows <- 10000
npaths <- 100
# Simulate multiple paths of geometric Brownian motion
prices <- rnorm(npaths*nrows, sd=sigmav) + drift - sigmav^2/2
prices <- matrix(prices, nc=npaths)
prices <- exp(matrixStats::colCumsums(prices))
# Calculate fraction of paths below the expected value
fractv <- rowSums(prices < 1.0) / npaths
# Create xts time series of percentage of paths below the expected value
fractv <- xts(fractv, order.by=seq.Date(Sys.Date()-NROW(fractv)+1, Sys.Date(), by=1))
# Plot xts time series of percentage of paths below the expected value
par(mar=c(3, 3, 2, 2), oma=c(0, 0, 0, 0))
plot.zoo(fractv, main="Percentage of GBM paths below mean",
   xlab=NA, ylab=NA, col="blue")

# Load S&P500 stock prices
load("/Users/jerzy/Develop/lecture_slides/data/sp500.RData")
ls(sp500env)
# Extract closing prices
prices <- eapply(sp500env, quantmod::Cl)
# Flatten prices into a single xts series
prices <- rutils::do_call(cbind, prices)
# Carry forward and backward non-NA prices
prices <- zoo::na.locf(prices, na.rm=FALSE)
prices <- zoo::na.locf(prices, fromLast=TRUE)
sum(is.na(prices))
# Drop ".Close" from column names
colnames(prices[, 1:4])
colnames(prices) <- rutils::get_name(colnames(prices))
# Or
# colnames(prices) <- do.call(rbind,
#   strsplit(colnames(prices), split="[.]"))[, 1]
# Normalize the columns so that prices start at 1
pricesn <- lapply(prices, function(x) x/as.numeric(x[1]))
pricesn <- rutils::do_call(cbind, pricesn)
# Calculate permutation index for sorting on the final prices
nrows <- NROW(pricesn)
ordern <- order(pricesn[nrows, ])
# Sort the symbols according to the final prices
symbolv <- colnames(pricesn)[ordern]
# Select 20 symbols
symbolv <- symbolv[seq.int(from=1, to=NROW(symbolv), length.out=20)]

# Plot xts time series of prices
colorv <- colorRampPalette(c("red", "blue"))(NROW(symbolv))
colorv <- colorv[order(order(pricesn[nrows, symbolv]))]
plot.zoo(pricesn["2000/", symbolv], main="20 S&P500 Stock Prices (normalized)",
   xlab=NA, ylab=NA, plot.type="single", col=colorv)
legend(x="topleft", inset=0.02, cex=0.6,
 legend=rev(symbolv), col=rev(colorv), lwd=6, lty=1)

# Calculate the final stock prices
pricesf <- drop(zoo::coredata(pricesn[NROW(pricesn), ]))
# Calculate the mean and median stock prices
max(pricesf); min(pricesf)
which.max(pricesf)
which.min(pricesf)
mean(pricesf)
median(pricesf)
# Calculate the percentage of stock prices below the mean
sum(pricesf < mean(pricesf))/NROW(pricesf)

# Plot a histogram of final stock prices
hist(pricesf, breaks=1e3, xlim=c(0, 300),
     xlab="Stock price", ylab="Count",
     main="Histogram of Final Stock Prices")
# Plot a histogram of final stock prices
abline(v=median(pricesf), lwd=3, col="blue")
text(x=median(pricesf), y=150, lab="median", pos=4)
abline(v=mean(pricesf), lwd=3, col="red")
text(x=mean(pricesf), y=100, lab="mean", pos=4)

# Calculate average of valid stock prices
validp <- (pricesn != 1)  # Valid stocks
nstocks <- rowSums(validp)
nstocks[1] <- NCOL(pricesn)
indeks <- rowSums(pricesn*validp)/nstocks
# Calculate fraction of stock prices below the average price
fractv <- rowSums((pricesn < indeks) & validp)/nstocks
# Create xts time series of average stock prices
indeks <- xts(indeks, order.by=zoo::index(pricesn))

dev.new(width=6, height=4, noRStudioGD=TRUE)
# x11(width=6, height=4)
# Plot xts time series of average stock prices
plot.zoo(indeks, main="Average S&P500 Stock Prices (normalized from 1990)",
   xlab=NA, ylab=NA, col="blue")
# Create xts time series of percentage of stock prices below the average price
fractv <- xts(fractv, order.by=zoo::index(pricesn))
# Plot percentage of stock prices below the average price
plot.zoo(fractv[-(1:2),],
   main="Percentage of S&P500 Stock Prices
   Below the Average Price",
   xlab=NA, ylab=NA, col="blue")

# Open plot window under MS Windows
x11(width=6, height=4)
par(mar=c(3, 2, 1, 1), oma=c(1, 0, 0, 0))
# Calculate VTI percentage returns
retsp <- na.omit(rutils::etfenv$returns$VTI)
# Plot autocorrelations of VTI returns using stats::acf()
stats::acf(retsp, lag=10, xlab="lag", main="")
title(main="ACF of VTI Returns", line=-1)
# Calculate two-tailed 95% confidence interval
qnorm(0.975)/sqrt(NROW(retsp))

# Get the ACF data returned invisibly
acfv <- acf(retsp, plot=FALSE)
summary(acfv)
# Print the ACF data
print(acfv)
dim(acfv$acf)
dim(acfv$lag)
head(acfv$acf)

plot_acf <- function(xtsv, lagg=10, plotobj=TRUE,
               xlab="Lag", ylab="", main="", ...) {
  # Calculate the ACF without a plot
  acfv <- acf(x=xtsv, lag.max=lagg, plot=FALSE, ...)
  # Remove first element of ACF data
  acfv$acf <- array(data=acfv$acf[-1],
    dim=c((dim(acfv$acf)[1]-1), 1, 1))
  acfv$lag <- array(data=acfv$lag[-1],
    dim=c((dim(acfv$lag)[1]-1), 1, 1))
  # Plot ACF
  if (plotobj) {
    ci <- qnorm((1+0.95)/2)/sqrt(NROW(xtsv))
    ylim <- c(min(-ci, range(acfv$acf[-1])),
        max(ci, range(acfv$acf[-1])))
    plot(acfv, xlab=xlab, ylab=ylab,
   ylim=ylim, main="", ci=0)
    title(main=main, line=0.5)
    abline(h=c(-ci, ci), col="blue", lty=2)
  }  # end if
  # Return the ACF data invisibly
  invisible(acfv)
}  # end plot_acf

# Improved autocorrelation function
x11(width=6, height=4)
rutils::plot_acf(retsp, lag=10, main="")
title(main="ACF of VTI returns", line=-1)
# Ljung-Box test for VTI returns
Box.test(retsp, lag=10, type="Ljung")

# Ljung-Box test for VTI returns
# 'lag' is the number of autocorrelation coefficients
Box.test(retsp, lag=10, type="Ljung")
# Ljung-Box test for random returns
Box.test(rnorm(NROW(retsp)), lag=10, type="Ljung")
library(Ecdat)  # Load Ecdat
macrodata <- as.zoo(Macrodat[, c("lhur", "fygm3")])
colnames(macrodata) <- c("unemprate", "3mTbill")
macrodiff <- na.omit(diff(macrodata))
# Changes in 3 month T-bill rate are autocorrelated
Box.test(macrodiff[, "3mTbill"], lag=10, type="Ljung")
# Changes in unemployment rate are autocorrelated
Box.test(macrodiff[, "unemprate"], lag=10, type="Ljung")

# Open plot window under MS Windows
x11(width=6, height=7)
# Set two vertical plot panels
par(mfrow=c(2,1))
par(mar=c(3, 3, 2, 2), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
# Plot autocorrelations of squared random returns
rutils::plot_acf(rnorm(NROW(retsp))^2, lag=10, main="")
title(main="ACF of Squared Random Returns", line=-1)
# Plot autocorrelations of squared VTI returns
rutils::plot_acf(retsp^2, lag=10, main="")
title(main="ACF of Squared VTI Returns", line=-1)
# Ljung-Box test for squared VTI returns
Box.test(retsp^2, lag=10, type="Ljung")

library(rutils)  # Load package rutils
library(Ecdat)  # Load Ecdat
colnames(Macrodat)  # United States Macroeconomic Time Series
# Coerce to "zoo"
macrodata <- as.zoo(Macrodat[, c("lhur", "fygm3")])
colnames(macrodata) <- c("unemprate", "3mTbill")
# ggplot2 in multiple panes
autoplot(  # Generic ggplot2 for "zoo"
  object=macrodata, main="US Macro",
  facets=Series ~ .) + # end autoplot
  xlab("") +
theme(  # Modify plot theme
  legend.position=c(0.1, 0.5),
  plot.title=element_text(vjust=-2.0),
  plot.margin=unit(c(-0.5, 0.0, -0.5, 0.0), "cm"),
  plot.background=element_blank(),
  axis.text.y=element_blank()
)  # end theme

# Open plot window under MS Windows
par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1),
    cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# Set two vertical plot panels
par(mfrow=c(2,1))
macrodiff <- na.omit(diff(macrodata))
# Plot the autocorrelations
rutils::plot_acf(coredata(macrodiff[, "unemprate"]),
  lag=10, main="quarterly unemployment rate")
rutils::plot_acf(coredata(macrodiff[, "3mTbill"]),
  lag=10, main="3 month T-bill EOQ")

# Calculate SPY log prices and percentage returns
ohlc <- HighFreq::SPY
ohlc[, 1:4] <- log(ohlc[, 1:4])
nrows <- NROW(ohlc)
closep <- quantmod::Cl(ohlc)
retsp <- rutils::diffit(closep)
colnames(retsp) <- "SPY"
# Open plot window under MS Windows
x11(width=6, height=4)
# Open plot window on Mac
dev.new(width=6, height=4, noRStudioGD=TRUE)
# Plot the autocorrelations of minutely SPY returns
acfv <- rutils::plot_acf(as.numeric(retsp), lag=10,
     xlab="lag", ylab="Autocorrelation", main="")
title("Autocorrelations of Minutely SPY Returns", line=1)
# Calculate the sum of autocorrelations
sum(acfv$acf)

# Ljung-Box test for minutely SPY returns
Box.test(retsp, lag=10, type="Ljung")
# Calculate hourly SPY percentage returns
closeh <- Cl(xts::to.period(x=ohlc, period="hours"))
retsh <- rutils::diffit(closeh)
# Ljung-Box test for hourly SPY returns
Box.test(retsh, lag=10, type="Ljung")
# Calculate daily SPY percentage returns
closed <- Cl(xts::to.period(x=ohlc, period="days"))
retsd <- rutils::diffit(closed)
# Ljung-Box test for daily SPY returns
Box.test(retsd, lag=10, type="Ljung")
# Ljung-Box test statistics for aggregated SPY returns
sapply(list(minutely=retsp, hourly=retsh, daily=retsd),
  function(rets) {
    Box.test(rets, lag=10, type="Ljung")$statistic
})  # end sapply

# Daily SPY volatility from daily returns
sd(retsd)
# Minutely SPY volatility scaled to daily interval
sqrt(6.5*60)*sd(retsp)
# Minutely SPY returns without overnight price jumps (unit per second)
retsp <- retsp/rutils::diffit(xts::.index(retsp))
retsp[1] <- 0
# Daily SPY volatility from minutely returns
sqrt(6.5*60)*60*sd(retsp)
# Daily SPY returns without weekend and holiday price jumps (unit per second)
retsd <- retsd/rutils::diffit(xts::.index(retsd))
retsd[1] <- 0
# Daily SPY volatility without weekend and holiday price jumps
24*60*60*sd(retsd)

# Calculate volatilities for vector of aggregation intervals
aggv <- seq.int(from=3, to=35, length.out=9)^2
volv <- sapply(aggv, function(aggint) {
  naggs <- nrows %/% aggint
  endp <- c(0, nrows - naggs*aggint + (0:naggs)*aggint)
  # endp <- rutils::calc_endpoints(closep, interval=aggint)
  sd(rutils::diffit(closep[endp]))
})  # end sapply
# Calculate the Hurst from single data point
volog <- log(volv)
agglog <- log(aggv)
(last(volog) - first(volog))/(last(agglog) - first(agglog))
# Calculate the Hurst from regression slope using formula
hurstexp <- cov(volog, agglog)/var(agglog)
# Or using function lm()
model <- lm(volog ~ agglog)
coef(model)[2]

# Plot the volatilities
x11(width=6, height=4)
par(mar=c(4, 4, 2, 1), oma=c(1, 1, 1, 1))
plot(volog ~ agglog, lwd=6, col="red",
     xlab="Aggregation intervals (log)", ylab="Volatility (log)",
     main="Hurst Exponent for SPY From Volatilities")
abline(model, lwd=3, col="blue")
text(agglog[2], volog[NROW(volog)-1],
     paste0("Hurst = ", round(hurstexp, 4)))

# Calculate cumulative SPY returns
closep <- cumsum(retsp)
nrows <- NROW(closep)
# Calculate the rescaled range
aggint <- 500
naggs <- nrows %/% aggint
endp <- c(0, nrows - naggs*aggint + (0:naggs)*aggint)
# Or
# endp <- rutils::calc_endpoints(closep, interval=aggint)
rrange <- sapply(2:NROW(endp), function(np) {
  indeks <- (endp[np-1]+1):endp[np]
  diff(range(closep[indeks]))/sd(retsp[indeks])
})  # end sapply
mean(rrange)
# Calculate the Hurst from single data point
log(mean(rrange))/log(aggint)

# Calculate the rescaled range for vector of aggregation intervals
rrange <- sapply(aggv, function(aggint) {
# Calculate the end points
  naggs <- nrows %/% aggint
  endp <- c(0, nrows - naggs*aggint + (0:naggs)*aggint)
# Calculate the rescaled ranges
  rrange <- sapply(2:NROW(endp), function(np) {
    indeks <- (endp[np-1]+1):endp[np]
    diff(range(closep[indeks]))/sd(retsp[indeks])
  })  # end sapply
  mean(na.omit(rrange))
})  # end sapply
# Calculate the Hurst as regression slope using formula
rangelog <- log(rrange)
agglog <- log(aggv)
hurstexp <- cov(rangelog, agglog)/var(agglog)
# Or using function lm()
model <- lm(rangelog ~ agglog)
coef(model)[2]

x11(width=6, height=4)
par(mar=c(4, 4, 2, 1), oma=c(1, 1, 1, 1))
plot(rangelog ~ agglog, lwd=6, col="red",
     xlab="aggregation intervals (log)",
     ylab="rescaled range (log)",
     main="Hurst Exponent for SPY From Rescaled Range")
abline(model, lwd=3, col="blue")
text(agglog[2], rangelog[NROW(rangelog)-1],
     paste0("Hurst = ", round(hurstexp, 4)))

# Load S&P500 constituent OHLC stock prices
load("/Users/jerzy/Develop/lecture_slides/data/sp500.RData")
class(sp500env$AAPL)
head(sp500env$AAPL)
# Calculate log stock prices after the year 2000
pricev <- eapply(sp500env, function(ohlc) {
  closep <- log(quantmod::Cl(ohlc)["2000/"])
# Ignore short lived and penny stocks (less than $1)
  if ((NROW(closep) > 4000) & (last(closep) > 0))
    return(closep)
})  # end eapply
# Calculate the number of NULL prices
sum(sapply(pricev, is.null))
# Calculate the names of the stocks (remove NULL prices)
namev <- sapply(pricev, is.null)
namev <- namev[!namev]
namev <- names(namev)
pricev <- pricev[namev]
# Calculate the Hurst exponents of stocks
aggv <- trunc(seq.int(from=3, to=10, length.out=5)^2)
hurstv <- sapply(pricev, HighFreq::calc_hurst, aggv=aggv)
# Dygraph of stock with largest Hurst exponent
namev <- names(which.max(hurstv))
dygraphs::dygraph(get(namev, pricev), main=namev)
# Dygraph of stock with smallest Hurst exponent
namev <- names(which.min(hurstv))
dygraphs::dygraph(get(namev, pricev), main=namev)

# Plot a histogram of the Hurst exponents of stocks
hist(hurstv, breaks=20, xlab="Hurst", ylab="Count",
     main="Hurst Exponents of Stocks")
# Add vertical line for H = 0.5
abline(v=0.5, lwd=3, col='red')
text(x=0.5, y=50, lab="H = 0.5", pos=4)

# Calculate the volatility of stocks
volat <- sapply(pricev, function(closep) {
    sqrt(HighFreq::calc_var(HighFreq::diffit(closep)))
})  # end sapply
# Dygraph of stock with highest volatility
namev <- names(which.max(volat))
dygraphs::dygraph(get(namev, pricev), main=namev)
# Dygraph of stock with lowest volatility
namev <- names(which.min(volat))
dygraphs::dygraph(get(namev, pricev), main=namev)
# Calculate the regression of the Hurst exponents versus volatilities
model <- lm(hurstv ~ volat)
summary(model)

# Plot scatterplot of the Hurst exponents versus volatilities
plot(hurstv ~ volat, xlab="Volatility", ylab="Hurst",
     main="Hurst Exponents Versus Volatilities of Stocks")
# Add regression line
abline(model, col='red', lwd=3)
tvalue <- summary(model)$coefficients[2, "t value"]
tvalue <- round(tvalue, 3)
text(x=mean(volat), y=max(hurstv),
     lab=paste("t-value =", tvalue), lwd=2, cex=1.2)

# Calculate the in-sample volatility of stocks
volatis <- sapply(pricev, function(closep) {
    sqrt(HighFreq::calc_var(HighFreq::diffit(closep["/2010"])))
})  # end sapply
# Calculate the out-of-sample volatility of stocks
volatos <- sapply(pricev, function(closep) {
    sqrt(HighFreq::calc_var(HighFreq::diffit(closep["2010/"])))
})  # end sapply
# Calculate the regression of the out-of-sample versus in-sample volatility
model <- lm(volatos ~ volatis)
summary(model)

# Plot scatterplot of the out-of-sample versus in-sample volatility
plot(volatos ~ volatis, xlab="In-sample Volatility", ylab="Out-of-sample Volatility",
     main="Out-of-Sample Versus In-Sample Volatility of Stocks")
# Add regression line
abline(model, col='red', lwd=3)
tvalue <- summary(model)$coefficients[2, "t value"]
tvalue <- round(tvalue, 3)
text(x=mean(volatis), y=max(volatos),
     lab=paste("t-value =", tvalue), lwd=2, cex=1.2)

# Calculate the in-sample Hurst exponents of stocks
hurstis <- sapply(pricev, function(closep) {
  HighFreq::calc_hurst(closep["/2010"], aggv=aggv)
})  # end sapply
# Calculate the out-of-sample Hurst exponents of stocks
hurstos <- sapply(pricev, function(closep) {
  HighFreq::calc_hurst(closep["2010/"], aggv=aggv)
})  # end sapply
# Calculate the regression of the out-of-sample versus in-sample Hurst exponents
model <- lm(hurstos ~ hurstis)
summary(model)

# Plot scatterplot of the out-of-sample versus in-sample Hurst exponents
plot(hurstos ~ hurstis, xlab="In-sample Hurst", ylab="Out-of-sample Hurst",
     main="Out-of-Sample Versus In-Sample Hurst Exponents of Stocks")
# Add regression line
abline(model, col='red', lwd=3)
tvalue <- summary(model)$coefficients[2, "t value"]
tvalue <- round(tvalue, 3)
text(x=mean(hurstis), y=max(hurstos),
     lab=paste("t-value =", tvalue), lwd=2, cex=1.2)

# Calculate the stock trading volumes after the year 2000
volumev <- eapply(sp500env, function(ohlc) {
    sum(quantmod::Vo(ohlc)["2000/"])
})  # end eapply
# Remove NULL values
volumev <- volumev[names(pricev)]
volumev <- unlist(volumev)
which.max(volumev)
# Calculate the number of NULL prices
sum(is.null(volumev))
# Calculate the Hurst exponents of stocks
hurstv <- sapply(pricev, HighFreq::calc_hurst, aggv=aggv)
# Calculate the regression of the Hurst exponents versus trading volumes
model <- lm(hurstv ~ volumev)
summary(model)

# Plot scatterplot of the Hurst exponents versus trading volumes
plot(hurstv ~ volumev, xlab="Trading Volume", ylab="Hurst",
     main="Hurst Exponents Versus Trading Volumes of Stocks")
# Add regression line
abline(model, col='red', lwd=3)
tvalue <- summary(model)$coefficients[2, "t value"]
tvalue <- round(tvalue, 3)
text(x=quantile(volumev, 0.998), y=max(hurstv),
     lab=paste("t-value =", tvalue), lwd=2, cex=1.2)

# Calculate log stock returns
retsp <- lapply(pricev, rutils::diffit)
retsp <- rutils::do_call(cbind, retsp)
retsp[is.na(retsp)] <- 0
sum(is.na(retsp))
# Drop ".Close" from column names
colnames(retsp[, 1:4])
colnames(retsp) <- rutils::get_name(colnames(retsp))
# Calculate PCA prices using matrix algebra
eigend <- eigen(cov(retsp))
retspca <- retsp %*% eigend$vectors
pricepca <- xts::xts(matrixStats::colCumsums(retspca),
                 order.by=index(retsp))
colnames(pricepca) <- paste0("PC", 1:NCOL(retsp))
# Calculate the Hurst exponents of PCAs
hurstv <- sapply(pricepca, HighFreq::calc_hurst, aggv=aggv)
# Dygraph of PCA with largest Hurst exponent
namev <- names(which.max(hurstv))
dygraphs::dygraph(get(namev, pricepca), main=namev)
# Dygraph of PCA with smallest Hurst exponent
namev <- names(which.min(hurstv))
dygraphs::dygraph(get(namev, pricepca), main=namev)

# Plot the Hurst exponents of principal components without x-axis
plot(hurstv, xlab=NA, ylab=NA, xaxt="n",
     main="Hurst Exponents of Principal Components")
# Add X-axis with PCA labels
axis(side=1, at=(1:NROW(hurstv)), labels=names(hurstv))
# Calculate the regression of the PCA Hurst exponents versus their order
orderv <- 1:NROW(hurstv)
model <- lm(hurstv ~ orderv)
summary(model)
# Add regression line
abline(model, col='red', lwd=3)
tvalue <- summary(model)$coefficients[2, "t value"]
tvalue <- round(tvalue, 3)
text(x=mean(orderv), y=max(hurstv),
     lab=paste("t-value =", tvalue), lwd=2, cex=1.2)

# Calculate in-sample eigen decomposition using matrix algebra
eigend <- eigen(cov(retsp["/2010"]))
# Calculate out-of-sample PCA prices
retspca <- retsp["2010/"] %*% eigend$vectors
pricepca <- xts::xts(matrixStats::colCumsums(retspca),
                 order.by=index(retsp["2010/"]))
colnames(pricepca) <- paste0("PC", 1:NCOL(retsp))
# Calculate the Hurst exponents of PCAs
hurstv <- sapply(pricepca, HighFreq::calc_hurst, aggv=aggv)
# Dygraph of PCA with largest Hurst exponent
namev <- names(which.max(hurstv))
dygraphs::dygraph(get(namev, pricepca), main=namev)
# Dygraph of PCA with smallest Hurst exponent
namev <- names(which.min(hurstv))
dygraphs::dygraph(get(namev, pricepca), main=namev)

# Plot the Hurst exponents of principal components without x-axis
plot(hurstv, xlab=NA, ylab=NA, xaxt="n",
     main="Out-of-Sample Hurst Exponents of Principal Components")
# Add X-axis with PCA labels
axis(side=1, at=(1:NROW(hurstv)), labels=names(hurstv))
# Calculate the regression of the PCA Hurst exponents versus their order
model <- lm(hurstv ~ orderv)
summary(model)
# Add regression line
abline(model, col='red', lwd=3)
tvalue <- summary(model)$coefficients[2, "t value"]
tvalue <- round(tvalue, 3)
text(x=mean(orderv), y=max(hurstv),
     lab=paste("t-value =", tvalue), lwd=2, cex=1.2)

# Get ETF log prices
symbolv <- rutils::etfenv$symbolv
symbolv <- symbolv[!(symbolv %in% c("MTUM", "QUAL", "VLUE", "USMV"))]
pricev <- lapply(mget(symbolv, rutils::etfenv), function(x) {
  log(na.omit(quantmod::Cl(x)))
})  # end lapply
# Calculate the Hurst exponents of ETFs
aggv <- trunc(seq.int(from=3, to=10, length.out=5)^2)
hurstv <- sapply(pricev, HighFreq::calc_hurst, aggv=aggv)
hurstv <- sort(unlist(hurstv))
# Dygraph of ETF with smallest Hurst exponent
namev <- names(first(hurstv))
dygraphs::dygraph(get(namev, pricev), main=namev)
# Dygraph of ETF with largest Hurst exponent
namev <- names(last(hurstv))
dygraphs::dygraph(get(namev, pricev), main=namev)

# Plot a histogram of the Hurst exponents of stocks
hist(hurstv, breaks=2e1, xlab="Hurst", ylab="Count",
     main="Hurst Exponents of ETFs")
# Add vertical line for H = 0.5
abline(v=0.5, lwd=3, col='red')
text(x=0.5, y=50, lab="H = 0.5", pos=4)

# Calculate log ETF returns
symbolv <- rutils::etfenv$symbolv
symbolv <- symbolv[!(symbolv %in% c("MTUM", "QUAL", "VLUE", "USMV"))]
retsp <- rutils::etfenv$returns[, symbolv]
retsp[is.na(retsp)] <- 0
sum(is.na(retsp))
# Calculate the Hurst exponent of an ETF portfolio
calc_phurst <- function(weightv, retsp) {
  -HighFreq::calc_hurst(matrix(cumsum(retsp %*% weightv)), aggv=aggv)
}  # end calc_phurst
# Calculate the portfolio weights with maximum Hurst
nweights <- NCOL(retsp)
weightv <- rep(1/sqrt(nweights), nweights)
calc_phurst(weightv, retsp=retsp)
optiml <- optim(par=weightv, fn=calc_phurst, retsp=retsp,
          method="L-BFGS-B",
          upper=rep(10.0, nweights),
          lower=rep(-10.0, nweights))
# Optimal weights and maximum Hurst
weightv <- optiml$par
names(weightv) <- colnames(retsp)
-calc_phurst(weightv, retsp=retsp)

# Dygraph of ETF portfolio with largest Hurst exponent
wealthv <- xts::xts(cumsum(retsp %*% weightv), zoo::index(retsp))
dygraphs::dygraph(wealthv, main="ETF Portfolio With Largest Hurst Exponent")

# Calculate the in-sample maximum Hurst portfolio weights
optiml <- optim(par=weightv, fn=calc_phurst, retsp=retsp["/2010"],
          method="L-BFGS-B",
          upper=rep(10.0, nweights),
          lower=rep(-10.0, nweights))
# Optimal weights and maximum Hurst
weightv <- optiml$par
names(weightv) <- colnames(retsp)
# Calculate the in-sample Hurst exponent
-calc_phurst(weightv, retsp=retsp["/2010"])
# Calculate the out-of-sample Hurst exponent
-calc_phurst(weightv, retsp=retsp["2010/"])

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

coeff <- c(-0.9, 0.01, 0.9)  # AR coefficients
# Create three AR time series
arimav <- sapply(coeff, function(phi) {
  set.seed(1121)  # Reset random numbers
  arima.sim(n=NROW(dates), model=list(ar=phi))
})  # end sapply
colnames(arimav) <- paste("autocorr", coeff)
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
arimafaster <- filter(x=innov, filter=coeff, method="recursive")
class(arimafaster)
all.equal(arimav, as.numeric(arimafaster))
# Fast simulation of AR process using C_rfilter()
arimacpp <- .Call(stats:::C_rfilter, innov, coeff,
                 double(NROW(coeff) + NROW(innov)))[-(1:3)]
all.equal(arimav, arimacpp)

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
arimafast <- filter(x=innov, filter=coeff, method="recursive")
all.equal(arimafast[-(1:warmup)], as.numeric(arimav))
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
init_price <- 0.0; priceq <- 1.0;
sigmav <- 0.02; thetav <- 0.01; nrows <- 1000
# Initialize the data
innov <- rnorm(nrows)
retsp <- numeric(nrows)
prices <- numeric(nrows)
retsp[1] <- sigmav*innov[1]
prices[1] <- init_price
# Simulate Ornstein-Uhlenbeck process in R
for (i in 2:nrows) {
  retsp[i] <- thetav*(priceq - prices[i-1]) + sigmav*innov[i]
  prices[i] <- prices[i-1] + retsp[i]
}  # end for
# Simulate Ornstein-Uhlenbeck process in Rcpp
prices_cpp <- HighFreq::sim_ou(init_price=init_price, eq_price=priceq,
  theta=thetav, innov=matrix(innov))
all.equal(prices, drop(prices_cpp))
# Compare the speed of R code with Rcpp
library(microbenchmark)
summary(microbenchmark(
  Rcode={for (i in 2:nrows) {
    retsp[i] <- thetav*(priceq - prices[i-1]) + sigmav*innov[i]
    prices[i] <- prices[i-1] + retsp[i]}},
  Rcpp=HighFreq::sim_ou(init_price=init_price, eq_price=priceq,
    theta=thetav, innov=matrix(innov)),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

plot(prices, type="l", xlab="time", ylab="prices",
     main="Ornstein-Uhlenbeck Process")
legend("topright", title=paste(c(paste0("sigmav = ", sigmav),
               paste0("eq_price = ", ),
               paste0("thetav = ", thetav)),
             collapse="\n"),
 legend="", cex=0.8, inset=0.1, bg="white", bty="n")
abline(h=, col='red', lwd=2)

retsp <- rutils::diffit(prices)
pricelag <- rutils::lagit(prices)
formulav <- retsp ~ pricelag
regmod <- lm(formulav)
summary(regmod)
# Plot regression
plot(formulav, main="OU Returns Versus Lagged Prices")
abline(regmod, lwd=2, col="red")

# Calculate volatility parameter
c(volatility=sigmav, estimate=sd(retsp))
# Extract OU parameters from regression
coeff <- summary(regmod)$coefficients
# Calculate regression alpha and beta directly
betav <- cov(retsp, pricelag)/var(pricelag)
alpha <- (mean(retsp) - betav*mean(pricelag))
cbind(direct=c(alpha=alpha, beta=betav), lm=coeff[, 1])
all.equal(c(alpha=alpha, beta=betav), coeff[, 1],
    check.attributes=FALSE)
# Calculate regression standard errors directly
betas <- c(alpha=alpha, beta=betav)
fittedv <- (alpha + betav*pricelag)
residuals <- (retsp - fittedv)
prices2 <- sum((pricelag - mean(pricelag))^2)
betasd <- sqrt(sum(residuals^2)/prices2/(nrows-2))
alphasd <- sqrt(sum(residuals^2)/(nrows-2)*(1:nrows + mean(pricelag)^2/prices2))
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
retsp <- numeric(nrows)
prices <- numeric(nrows)
prices[1] <- exp(sigmav*innov[1])
set.seed(1121)  # Reset random numbers
for (i in 2:nrows) {
  retsp[i] <- thetav*(priceq - prices[i-1]) + sigmav*innov[i]
  prices[i] <- prices[i-1]*exp(retsp[i])
}  # end for

plot(prices, type="l", xlab="time", ylab="prices",
     main="Schwartz Process")
legend("topright",
 title=paste(c(paste0("sigmav = ", sigmav),
               paste0("priceq = ", priceq),
               paste0("thetav = ", thetav)),
             collapse="\n"),
 legend="", cex=0.8, inset=0.12, bg="white", bty="n")
abline(h=priceq, col='red', lwd=2)

# Define Dickey-Fuller parameters
init_price <- 0.0;  priceq <- 1.0
thetav <- 0.01;  nrows <- 1000
coeff <- c(0.1, 0.39, 0.5)
# Initialize the data
innov <- rnorm(nrows, sd=0.01)
retsp <- numeric(nrows)
prices <- numeric(nrows)
# Simulate Dickey-Fuller process using recursive loop in R
retsp[1] <- innov[1]
prices[1] <- init_price
retsp[2] <- thetav*(priceq - prices[1]) + coeff[1]*retsp[1] + innov[2]
prices[2] <- prices[1] + retsp[2]
retsp[3] <- thetav*(priceq - prices[2]) + coeff[1]*retsp[2] + coeff[2]*retsp[1] + innov[3]
prices[3] <- prices[2] + retsp[3]
for (it in 4:nrows) {
  retsp[it] <- thetav*(priceq - prices[it-1]) + retsp[(it-1):(it-3)] %*% coeff + innov[it]
  prices[it] <- prices[it-1] + retsp[it]
}  # end for
# Simulate Dickey-Fuller process in Rcpp
prices_cpp <- HighFreq::sim_df(init_price=init_price, eq_price=priceq, theta=thetav, coeff=matrix(coeff), innov=matrix(innov))
# Compare prices
all.equal(prices, drop(prices_cpp))
# Compare the speed of R code with Rcpp
library(microbenchmark)
summary(microbenchmark(
  Rcode={for (it in 4:nrows) {
  retsp[it] <- thetav*(priceq - prices[it-1]) + retsp[(it-1):(it-3)] %*% coeff + innov[it]
  prices[it] <- prices[it-1] + retsp[it]
  }},
  Rcpp=HighFreq::sim_df(init_price=init_price, eq_price=priceq, theta=thetav, coeff=matrix(coeff), innov=matrix(innov)),
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
init_price <- 0.0; priceq <- 0.0; thetav <- 0.1
prices <- HighFreq::sim_ou(init_price=init_price, eq_price=priceq,
  theta=thetav, innov=innov)
x11(); plot(prices, t="l", main=paste("OU coefficient =", thetav))
tseries::adf.test(prices, k=1)
# Simulate Ornstein-Uhlenbeck OU process with zero reversion
thetav <- 0.0
prices <- HighFreq::sim_ou(init_price=init_price, eq_price=priceq,
  theta=thetav, innov=innov)
x11(); plot(prices, t="l", main=paste("OU coefficient =", thetav))
tseries::adf.test(prices, k=1)

# Simulate AR(1) process with different coefficients
coeffv <- seq(0.99, 0.999, 0.001)
retsp <- as.numeric(na.omit(rutils::etfenv$returns$VTI))
adft <- sapply(coeffv, function(coeff) {
  arimav <- filter(x=retsp, filter=coeff, method="recursive")
  adft <- suppressWarnings(tseries::adf.test(arimav))
  c(adfstat=unname(adft$statistic), pval=adft$p.value)
})  # end sapply
dev.new(width=6, height=4, noRStudioGD=TRUE)
# x11(width=6, height=4)
plot(x=coeffv, y=adft["pval", ], main="ADF p-val Versus AR Coefficient",
     xlab="AR coefficient", ylab="ADF pval", t="l", col="blue", lwd=2)
plot(x=coeffv, y=adft["adfstat", ], main="ADF Stat Versus AR Coefficient",
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
# predictor matrix squared
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
retsp <- drop(zoo::coredata(na.omit(rutils::etfenv$returns$VTI)))
predictor <- sapply(1:5, rutils::lagit, input=retsp)
predinv <- MASS::ginv(predictor)
coeff <- drop(predinv %*% retsp)
# Calculate t-values of AR(5) coefficients
residuals <- drop(retsp - drop(predictor %*% coeff))
residvar <- sum(residuals^2)/(nrows-NROW(coeff))
covar <- residvar*MASS::ginv(crossprod(predictor))
coeffsd <- sqrt(diag(covar))
coefftv <- drop(coeff)/coeffsd

# Compute autocorrelation coefficients
acfv <- rutils::plot_acf(arimav, lag=10, plot=FALSE)
acfv <- drop(acfv$acf)
nrows <- NROW(acfv)
acf1 <- c(1, acfv[-nrows])
# Define Yule-Walker matrix
ywmat <- sapply(1:nrows, function(lagg) {
  if (lagg < nrows)
    c(acf1[lagg:1], acf1[2:(nrows-lagg+1)])
  else
    acf1[lagg:1]
})  # end sapply
# Generalized inverse of Yule-Walker matrix
ywmatinv <- MASS::ginv(ywmat)
# Solve Yule-Walker equations
ywcoeff <- drop(ywmatinv %*% acfv)
round(ywcoeff, 5)
coeff

# Calculate PACF from acf using Durbin-Levinson algorithm
acfv <- rutils::plot_acf(arimav, lag=10, plotobj=FALSE)
acfv <- drop(acfv$acf)
nrows <- NROW(acfv)
pacfv <- numeric(2)
pacfv[1] <- acfv[1]
pacfv[2] <- (acfv[2] - acfv[1]^2)/(1 - acfv[1]^2)
# Calculate PACF recursively in a loop using Durbin-Levinson algorithm
pacfvl <- matrix(numeric(nrows*nrows), nc=nrows)
pacfvl[1, 1] <- acfv[1]
for (it in 2:nrows) {
  pacfvl[it, it] <- (acfv[it] - pacfvl[it-1, 1:(it-1)] %*% acfv[(it-1):1])/(1 - pacfvl[it-1, 1:(it-1)] %*% acfv[1:(it-1)])
  for (it2 in 1:(it-1)) {
    pacfvl[it, it2] <- pacfvl[it-1, it2] - pacfvl[it, it] %*% pacfvl[it-1, it-it2]
  }  # end for
}  # end for
pacfvl <- diag(pacfvl)
# Compare with the PACF without loop
all.equal(pacfv, pacfvl[1:2])
# Calculate PACF using pacf()
pacfv <- pacf(arimav, lag=10, plot=FALSE)
pacfv <- drop(pacfv$acf)
all.equal(pacfv, pacfvl)

nrows <- 1e2
coeff <- c(0.1, 0.39, 0.5); ncoeff <- NROW(coeff)
set.seed(1121); innov <- rnorm(nrows, sd=0.01)
# Simulate AR process using filter()
arimav <- filter(x=innov, filter=coeff, method="recursive")
arimav <- as.numeric(arimav)
# Simulate AR process using C_rfilter()
arimafast <- .Call(stats:::C_rfilter, innov, coeff,
  double(nrows + ncoeff))
all.equal(arimav, arimafast[-(1:ncoeff)],
  check.attributes=FALSE)

# Forecast AR(3) process using loop in R
forecastv <- numeric(NROW(arimav)+1)
forecastv[1] <- 0
forecastv[2] <- coeff[1]*arimav[1]
forecastv[3] <- coeff[1]*arimav[2] + coeff[2]*arimav[1]
for (it in 4:NROW(forecastv)) {
  forecastv[it] <- arimav[(it-1):(it-3)] %*% coeff
}  # end for
# Plot with legend
x11(width=6, height=4)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
plot(arimav, main="Forecasting Using AR(3) Model",
  xlab="", ylab="", type="l")
lines(forecastv, col="red", lwd=2)
legend(x="topright", legend=c("series", "forecasts"),
 col=c("black", "red"), lty=1, lwd=6,
 cex=0.9, bg="white", bty="n")

# Forecast using filter()
filterfast <- filter(x=arimav, sides=1,
  filter=coeff, method="convolution")
filterfast <- as.numeric(filterfast)
# Compare excluding warmup period
all.equal(forecastv[-(1:ncoeff)], filterfast[-(1:(ncoeff-1))],
    check.attributes=FALSE)
# Filter using C_cfilter() compiled C++ function directly
filterfast <- .Call(stats:::C_cfilter, arimav, filter=coeff,
               sides=1, circular=FALSE)
# Compare excluding warmup period
all.equal(forecastv[-(1:ncoeff)], filterfast[-(1:(ncoeff-1))],
    check.attributes=FALSE)
# Filter using HighFreq::roll_conv() Rcpp function
filterfast <- HighFreq::roll_conv(matrix(arimav), matrix(coeff))
# Compare excluding warmup period
all.equal(forecastv[-(1:ncoeff)], filterfast[-(1:(ncoeff-1))],
    check.attributes=FALSE)
# Define predictor matrix for forecasting
predictor <- sapply(0:(ncoeff-1), function(lagg) {
  rutils::lagit(arimav, lagg=lagg)
})  # end sapply
# Forecast using predictor matrix
filterfast <- c(0, drop(predictor %*% coeff))
# Compare with loop in R
all.equal(forecastv, filterfast, check.attributes=FALSE)

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
residuals <- (arimav - forecastv[-NROW(forecastv)])
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
colnames(predictor) <- paste0("pred", 1:NCOL(predictor))
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
retsp <- na.omit(rutils::etfenv$returns$VTI)
dates <- zoo::index(retsp)
retsp <- as.numeric(retsp)
nrows <- NROW(retsp)
# Define response equal to returns
response <- retsp
# Define predictor as a rolling sum
nagg <- 5
predictor <- rutils::roll_sum(retsp, look_back=nagg)
# Define predictor matrix for forecasting
order_max <- 5
predictor <- sapply(1+nagg*(0:order_max), rutils::lagit,
               input=predictor)
predictor <- cbind(rep(1, nrows), predictor)
# Perform rolling forecasting
look_back <- 100
forecastv <- sapply((look_back+1):nrows, function(endp) {
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
forecastv <- c(rep(0, look_back), forecastv)

# Mean squared error
mean((retsp - forecastv)^2)
# Correlation
cor(forecastv, retsp)

# Plot forecasting series with legend
x11(width=6, height=4)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0))
plot(retsp[(nrows-look_back):nrows], col="blue",
     xlab="", ylab="", type="l", lwd=2,
     main="Rolling Forecasting Using AR Model")
lines(forecastv[(nrows-look_back):nrows], col="red", lwd=2)
legend(x="top", legend=c("returns", "forecasts"),
 col=c("blue", "red"), lty=1, lwd=6,
 cex=0.9, bg="white", bty="n")

# Define backtesting function
sim_forecasts <- function(response, nagg=5, ordern=5,
                    look_back=100, rollp=TRUE) {
  nrows <- NROW(response)
  # Define predictor as a rolling sum
  predictor <- rutils::roll_sum(response, look_back=nagg)
  # Define predictor matrix for forecasting
  predictor <- sapply(1+nagg*(0:ordern), rutils::lagit,
                 input=predictor)
  predictor <- cbind(rep(1, nrows), predictor)
  # Perform rolling forecasting
  forecastv <- sapply((look_back+1):nrows, function(endp) {
    # Define rolling look-back range
    if (rollp)
startp <- max(1, endp-look_back)
    else
    # Or expanding look-back range
    startp <- 1
    rangev <- startp:(endp-1)
    # Invert the predictor matrix
    designinv <- MASS::ginv(predictor[rangev, ])
    # Calculate fitted coefficients
    coeff <- drop(designinv %*% response[rangev])
    # Calculate forecast
    drop(predictor[endp, ] %*% coeff)
  })  # end sapply
  # Add warmup period
  forecastv <- c(rep(0, look_back), forecastv)
  # Aggregate the forecasts
  rutils::roll_sum(forecastv, look_back=nagg)
}  # end sim_forecasts
# Simulate the rolling autoregressive forecasts
forecastv <- sim_forecasts(response=vti, ordern=5, look_back=100)
c(mse=mean((vti - forecastv)^2), cor=cor(vti, forecastv))

look_backs <- seq(20, 200, 20)
forecastv <- sapply(look_backs, sim_forecasts, response=retsp,
               nagg=nagg, ordern=ordern)
colnames(forecastv) <- look_backs
msev <- apply(forecasts, 2, function(x) mean((retsp - x)^2))
# Plot forecasting series with legend
plot(x=look_backs, y=msev,
  xlab="look-back", ylab="MSE", type="l", lwd=2,
  main="MSE of AR(5) Forecasting Model")

# Calculate a vector of daily VTI percentage returns
retsp <- na.omit(rutils::etfenv$returns$VTI)
dates <- zoo::index(retsp)
retsp <- as.numeric(retsp)
nrows <- NROW(retsp)
# Define predictor matrix for forecasting
orderp <- 5
predictor <- sapply(1:orderp, rutils::lagit, input=retsp)
predictor <- cbind(rep(1, nrows), predictor)
colnames(predictor) <- paste0("pred", 1:NCOL(predictor))
predinv <- MASS::ginv(predictor)
coeff <- drop(predinv %*% retsp)
# Calculate in-sample forecasts of VTI
forecastv <- drop(predictor %*% coeff)
# Calculate the residuals (forecast errors)
residuals <- drop(retsp - forecastv)

# Calculate the variance of the residuals
residvar <- sum(residuals^2)/(nrows-NROW(coeff))
# Calculate the predictor matrix squared
predictor2 <- crossprod(predictor)
# Calculate covariance matrix of the AR coefficients
covar <- residvar*MASS::ginv(predictor2)
coeffsd <- sqrt(diag(covar))
# Calculate the t-values of the AR coefficients
coefftv <- coeff/coeffsd
# Plot the t-values of the AR coefficients
barplot(coefftv, xlab="lag", ylab="t-value",
  main="Coefficient t-values of AR Forecasting Model")

# Define predictor matrix for forecasting
order_max <- 5
predictor <- sapply(1:order_max, rutils::lagit, input=retsp)
predictor <- cbind(rep(1, nrows), predictor)
colnames(predictor) <- paste0("pred", 1:NCOL(predictor))
# Calculate forecasts as function of the AR order
forecastv <- lapply(2:NCOL(predictor), function(ordern) {
  # Calculate fitted coefficients
  predinv <- MASS::ginv(predictor[, 1:ordern])
  coeff <- drop(predinv %*% retsp)
  # Calculate in-sample forecasts of VTI
  drop(predictor[, 1:ordern] %*% coeff)
})  # end lapply
names(forecastv) <- paste0("n=", 2:NCOL(predictor))

# Calculate mean squared errors
mse <- sapply(forecastv, function(x) {
  c(mse=mean((retsp - x)^2), cor=cor(retsp, x))
})  # end sapply
mse <- t(mse)
rownames(mse) <- names(forecastv)
# Plot forecasting MSE
plot(x=2:NCOL(predictor), y=mse[, 1],
  xlab="AR(n) order", ylab="MSE", type="l", lwd=2,
  main="MSE of In-sample AR(n) Forecasting Model for VTI")

insample <- 1:(nrows %/% 2)
outsample <- (nrows %/% 2 + 1):nrows
# Calculate forecasts as function of the AR order
forecastv <- lapply(2:NCOL(predictor), function(ordern) {
  # Calculate fitted coefficients
  predinv <- MASS::ginv(predictor[insample, 1:ordern])
  coeff <- drop(predinv %*% response[insample])
  # Calculate out-of-sample forecasts of VTI
  drop(predictor[outsample, 1:ordern] %*% coeff)
})  # end lapply
names(forecastv) <- paste0("n=", 2:NCOL(predictor))

# Calculate mean squared errors
mse <- sapply(forecastv, function(x) {
  c(mse=mean((retsp[outsample] - x)^2), cor=cor(retsp[outsample], x))
})  # end sapply
mse <- t(mse)
rownames(mse) <- names(forecastv)
# Plot forecasting MSE
plot(x=2:NCOL(predictor), y=mse[, 1],
  xlab="AR(n) order", ylab="MSE", type="l", lwd=2,
  main="MSE of Out-of-sample AR(n) Forecasting Model for VTI")

# Calculate out-of-sample PnLs
pnls <- sapply(forecastv, function(x) {
  cumsum(sign(x)*retsp[outsample])
})  # end sapply
colnames(pnls) <- names(forecastv)
pnls <- xts::xts(pnls, dates[outsample])

# Plot dygraph of out-of-sample PnLs
colorv <- colorRampPalette(c("red", "blue"))(NCOL(pnls[, 1:4]))
colnamev <- colnames(pnls[, 1:4])
endd <- rutils::calc_endpoints(pnls, interval="weeks")
dygraphs::dygraph(pnls[endd, 1:4],
  main="Autoregressive Strategies Performance With Different Order Parameters") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(width=500)

# Define predictor as a rolling mean
nagg <- 5
predictor <- roll::roll_mean(retsp, width=nagg, min_obs=1)
response <- retsp
# Define predictor matrix for forecasting
predictor <- sapply(1+nagg*(0:order_max), rutils::lagit,
               input=predictor)
predictor <- cbind(rep(1, nrows), predictor)
# Calculate forecasts as function of the AR order
forecastv <- lapply(2:NCOL(predictor), function(ordern) {
  predinv <- MASS::ginv(predictor[insample, 1:ordern])
  coeff <- drop(predinv %*% response[insample])
  drop(predictor[outsample, 1:ordern] %*% coeff)
})  # end lapply
names(forecastv) <- paste0("n=", 2:NCOL(predictor))

# Calculate out-of-sample PnLs
pnls <- sapply(forecastv, function(x) {
  cumsum(sign(x)*retsp[outsample])
})  # end sapply
colnames(pnls) <- names(forecastv)
pnls <- xts::xts(pnls, dates[outsample])
# Plot dygraph of out-of-sample PnLs
dygraphs::dygraph(pnls[endd, 1:4],
  main="Autoregressive Strategies Performance Using Rolling Average Predictor") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(width=500)

# Calculate PnLs using the average of past forecasts
nagg <- 5
pnls <- sapply(forecastv, function(x) {
  x <- roll::roll_mean(x, width=nagg, min_obs=1)
  cumsum(sign(x)*retsp[outsample])
})  # end sapply
colnames(pnls) <- names(forecastv)
pnls <- xts::xts(pnls, dates[outsample])

# Plot dygraph of out-of-sample PnLs
dygraphs::dygraph(pnls[endd, 1:4],
  main="Autoregressive Strategies Performance Using Rolling Average Forecasts") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(width=500)

# Define predictor as a rolling sum
nagg <- 5
predictor <- rutils::roll_sum(retsp, look_back=nagg)
# Define predictor matrix for forecasting
order_max <- 5
predictor <- sapply(1+nagg*(0:order_max), rutils::lagit,
               input=predictor)
predictor <- cbind(rep(1, nrows), predictor)
# Perform rolling forecasting
look_back <- 100
forecastv <- sapply((look_back+1):nrows, function(endp) {
  # Define rolling look-back range
  startp <- max(1, endp-look_back)
  # Or expanding look-back range
  # startp <- 1
  rangev <- startp:(endp-1)
  # Invert the predictor matrix
  designinv <- MASS::ginv(predictor[rangev, ])
  # Calculate fitted coefficients
  coeff <- drop(designinv %*% retsp[rangev])
  # Calculate forecast
  drop(predictor[endp, ] %*% coeff)
})  # end sapply
# Add warmup period
forecastv <- c(rep(0, look_back), forecastv)

# Mean squared error
mean((retsp - forecastv)^2)
# Correlation
cor(forecastv, retsp)

# Plot forecasting series with legend
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0))
plot(retsp[(nrows-look_back):nrows], col="blue",
     xlab="", ylab="", type="l", lwd=2,
     main="Rolling Forecasting Using AR Model")
lines(forecastv[(nrows-look_back):nrows], col="red", lwd=2)
legend(x="topleft", legend=c("returns", "forecasts"),
 col=c("blue", "red"), lty=1, lwd=6,
 cex=0.8, bg="white", bty="n")

# Define backtesting function
sim_forecasts <- function(response, nagg=5, ordern=5,
                    look_back=100, rollp=TRUE) {
  nrows <- NROW(response)
  # Define predictor as a rolling sum
  predictor <- rutils::roll_sum(response, look_back=nagg)
  # Define predictor matrix for forecasting
  predictor <- sapply(1+nagg*(0:ordern), rutils::lagit,
                 input=predictor)
  predictor <- cbind(rep(1, nrows), predictor)
  # Perform rolling forecasting
  forecastv <- sapply((look_back+1):nrows, function(endp) {
    # Define rolling look-back range
    if (rollp)
startp <- max(1, endp-look_back)
    else
    # Or expanding look-back range
    startp <- 1
    rangev <- startp:(endp-1)
    # Invert the predictor matrix
    designinv <- MASS::ginv(predictor[rangev, ])
    # Calculate fitted coefficients
    coeff <- drop(designinv %*% response[rangev])
    # Calculate forecast
    drop(predictor[endp, ] %*% coeff)
  })  # end sapply
  # Add warmup period
  forecastv <- c(rep(0, look_back), forecastv)
  # Aggregate the forecasts
  rutils::roll_sum(forecastv, look_back=nagg)
}  # end sim_forecasts
# Simulate the rolling autoregressive forecasts
forecastv <- sim_forecasts(response=retsp, ordern=5, look_back=100)
c(mse=mean((retsp - forecastv)^2), cor=cor(retsp, forecastv))

library(parallel)  # Load package parallel
# Calculate number of available cores
ncores <- detectCores() - 1
# Initialize compute cluster under Windows
cluster <- makeCluster(ncores)
# Perform parallel loop under Windows
look_backs <- seq(20, 600, 40)
forecastv <- parLapply(cluster, look_backs, sim_forecasts,
  response=retsp, nagg=5, ordern=5)
# Perform parallel bootstrap under Mac-OSX or Linux
forecastv <- mclapply(look_backs, sim_forecasts, response=retsp,
  nagg=5, ordern=5, mc.cores=ncores)

# Calculate mean squared errors
mse <- sapply(forecastv, function(x) {
  c(mse=mean((retsp - x)^2), cor=cor(retsp, x))
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
orderv <- 2:6
forecastv <- parLapply(cluster, orderv, sim_forecasts, response=retsp,
  nagg=5, look_back=look_back)
stopCluster(cluster)  # Stop R processes over cluster under Windows
# Perform parallel bootstrap under Mac-OSX or Linux
forecastv <- mclapply(orderv, sim_forecasts, response=retsp,
  nagg=5, look_back=look_back, mc.cores=ncores)

# Calculate mean squared errors
mse <- sapply(forecastv, function(x) {
  c(mse=mean((retsp - x)^2), cor=cor(retsp, x))
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
forecastv <- sim_forecasts(retsp, ordern=ordern, look_back=look_back)
# Calculate strategy PnLs
pnls <- sign(forecastv)*retsp
pnls <- cbind(retsp, pnls, (retsp+pnls)/2)
colnames(pnls) <- c("VTI", "AR_Strategy", "Combined")
cor(pnls)
# Annualized Sharpe ratios of VTI and AR strategy
pnls <- xts::xts(pnls, dates)
sqrt(252)*sapply(pnls, function (x) mean(x)/sd(x))

# Plot the cumulative strategy PnLs
endd <- rutils::calc_endpoints(pnls, interval="weeks")
dygraphs::dygraph(cumsum(pnls)[endd], main="Rolling Autoregressive Strategy") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
