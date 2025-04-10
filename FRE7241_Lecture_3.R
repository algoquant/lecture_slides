# Open plot window under MS Windows
x11(width=6, height=4)
par(mar=c(3, 2, 1, 1), oma=c(1, 0, 0, 0))
# Calculate VTI percentage returns
retp <- na.omit(rutils::etfenv$returns$VTI)
retp <- drop(zoo::coredata(retp))
# Plot autocorrelations of VTI returns using stats::acf()
stats::acf(retp, lag=10, xlab="lag", main="")
title(main="ACF of VTI Returns", line=-1)
# Calculate two-tailed 95% confidence interval
qnorm(0.975)/sqrt(NROW(retp))

# Get the ACF data returned invisibly
acfl <- acf(retp, plot=FALSE)
summary(acfl)
# Print the ACF data
print(acfl)
dim(acfl$acf)
dim(acfl$lag)
head(acfl$acf)

plot_acf <- function(xtsv, lagg=10, plotobj=TRUE,
               xlab="Lag", ylab="", main="", ...) {
  # Calculate the ACF without a plot
  acfl <- acf(x=xtsv, lag.max=lagg, plot=FALSE, ...)
  # Remove first element of ACF data
  acfl$acf <- array(data=acfl$acf[-1],
    dim=c((dim(acfl$acf)[1]-1), 1, 1))
  acfl$lag <- array(data=acfl$lag[-1],
    dim=c((dim(acfl$lag)[1]-1), 1, 1))
  # Plot ACF
  if (plotobj) {
    ci <- qnorm((1+0.95)/2)/sqrt(NROW(xtsv))
    ylim <- c(min(-ci, range(acfl$acf[-1])),
        max(ci, range(acfl$acf[-1])))
    plot(acfl, xlab=xlab, ylab=ylab,
   ylim=ylim, main="", ci=0)
    title(main=main, line=0.5)
    abline(h=c(-ci, ci), col="blue", lty=2)
  }  # end if
  # Return the ACF data invisibly
  invisible(acfl)
}  # end plot_acf

# Autocorrelations of VTI returns
rutils::plot_acf(retp, lag=10, main="ACF of VTI returns")

# Ljung-Box test for VTI returns
# 'lag' is the number of autocorrelation coefficients
Box.test(retp, lag=10, type="Ljung")
# Ljung-Box test for random returns
Box.test(rnorm(NROW(retp)), lag=10, type="Ljung")
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
# Plot ACF of squared random returns
rutils::plot_acf(rnorm(NROW(retp))^2, lag=10,
 main="ACF of Squared Random Returns")
# Plot ACF of squared VTI returns
rutils::plot_acf(retp^2, lag=10,
 main="ACF of Squared VTI Returns")
# Ljung-Box test for squared VTI returns
Box.test(retp^2, lag=10, type="Ljung")

# Calculate the weekly end points
endd <- rutils::calc_endpoints(retp, interval=5)
npts <- NROW(endd)
# Calculate the monthly VTI volatilities and their median volatility
stdev <- sapply(2:npts, function(endp) {
  sd(retp[endd[endp-1]:endd[endp]])
})  # end sapply
medianv <- median(stdev)
# Calculate the stock returns of low volatility intervals
retlow <- lapply(2:npts, function(endp) {
  if (stdev[endp-1] <= medianv)
    retp[endd[endp-1]:endd[endp]]
})  # end lapply
retlow <- rutils::do_call(c, retlow)
# Calculate the stock returns of high volatility intervals
rethigh <- lapply(2:npts, function(endp) {
  if (stdev[endp-1] > medianv)
    retp[endd[endp-1]:endd[endp]]
})  # end lapply
rethigh <- rutils::do_call(c, rethigh)
# Plot ACF of low volatility returns
rutils::plot_acf(retlow, lag=10,
 main="ACF of Low Volatility Returns")
Box.test(retlow, lag=10, type="Ljung")
# Plot ACF of high volatility returns
rutils::plot_acf(rethigh, lag=10,
 main="ACF of High Volatility Returns")
Box.test(rethigh, lag=10, type="Ljung")

NA

# Load daily S&P500 stock returns
load("/Users/jerzy/Develop/lecture_slides/data/sp500_returnstop.RData")
# Calculate the stock volatilities and the sum of the ACF
library(parallel)  # Load package parallel
ncores <- detectCores() - 1
statm <- mclapply(retstock, function(retp) {
  retp <- na.omit(retp)
  # Calculate the sum of the ACF
  acfsum <- sum(pacf(retp, lag=10, plot=FALSE)$acf)
  # Calculate the Ljung-Box statistic
  lbstat <- unname(Box.test(retp, lag=10, type="Ljung")$statistic)
  c(stdev=sd(retp), acfsum=acfsum, lbstat=lbstat)
}, mc.cores=ncores)  # end mclapply
statm <- do.call(rbind, statm)
statm <- as.data.frame(statm)
# Calculate the ACF sum for stock volatility quantiles
confl <- seq(0.1, 0.9, 0.1)
stdq <- quantile(statm[, "stdev"], confl)
acfq <- quantile(statm[, "acfsum"], confl)
plot(stdq, acfq, xlab="volatility", ylab="PACF Sum",
     main="PACF Sum vs Volatility")
# Compare the ACF sum for stock volatility quantile with VTI
acfq[1]
sum(pacf(na.omit(rutils::etfenv$returns$VTI), lag=10, plot=FALSE)$acf)

# Scatter plot of the sum of the ACF vs the standard deviation
regmod <- lm(acfsum ~ stdev, data=statm)
plot(acfsum ~ stdev, data=statm, xlab="SD", ylab="PACF Sum",
     xlim=c(0.5*min(stdq), 1.5*max(stdq)), ylim=c(1.5*min(acfq), 7*max(acfq)),
     main="PACF Sum vs SD")
abline(regmod, lwd=3, col="red")
tvalue <- summary(regmod)$coefficients[2, "t value"]
tvalue <- round(tvalue, 3)
text(x=mean(stdq), y=6*max(acfq),
     lab=paste("t-value =", tvalue), lwd=2, cex=1.2)

# Compare the Ljung-Box statistics for lowest volatility stocks with VTI
lbstatq <- rev(quantile(statm[, "lbstat"], confl))
lbstatq[1]
Box.test(na.omit(rutils::etfenv$returns$VTI), lag=10, type="Ljung")$statistic

# Plot Ljung-Box test statistic for volatility quantiles
plot(stdq, lbstatq, xlab="volatility", ylab="Ljung-Box Stat",
     main="Ljung-Box Statistic For Stock Volatility Quantiles")

# Calculate SPY log prices and percentage returns
ohlc <- HighFreq::SPY
ohlc[, 1:4] <- log(ohlc[, 1:4])
nrows <- NROW(ohlc)
closep <- quantmod::Cl(ohlc)
retp <- rutils::diffit(closep)
colnames(retp) <- "SPY"
# Open plot window under MS Windows
x11(width=6, height=4)
# Open plot window on Mac
dev.new(width=6, height=4, noRStudioGD=TRUE)
# Plot the autocorrelations of minutely SPY returns
acfl <- rutils::plot_acf(as.numeric(retp), lag=10,
     xlab="lag", ylab="Autocorrelation", main="")
title("Autocorrelations of Minutely SPY Returns", line=1)
# Calculate the sum of autocorrelations
sum(acfl$acf)

# Ljung-Box test for minutely SPY returns
Box.test(retp, lag=10, type="Ljung")
# Calculate hourly SPY percentage returns
closeh <- quantmod::Cl(xts::to.period(x=ohlc, period="hours"))
retsh <- rutils::diffit(closeh)
# Ljung-Box test for hourly SPY returns
Box.test(retsh, lag=10, type="Ljung")
# Calculate daily SPY percentage returns
closed <- quantmod::Cl(xts::to.period(x=ohlc, period="days"))
retd <- rutils::diffit(closed)
# Ljung-Box test for daily SPY returns
Box.test(retd, lag=10, type="Ljung")

# Ljung-Box test statistics for aggregated SPY returns
lbstat <- sapply(list(daily=retd, hourly=retsh, minutely=retp),
  function(rets) {
    Box.test(rets, lag=10, type="Ljung")$statistic
})  # end sapply
# Plot Ljung-Box test statistic for different aggregation intervals
plot(lbstat, lwd=6, col="blue", xaxt="n",
     xlab="Aggregation interval", ylab="Ljung-Box Stat",
     main="Ljung-Box Statistic For Different Aggregations")
# Add X-axis with labels
axis(side=1, at=(1:3), labels=c("daily", "hourly", "minutely"))

# Daily SPY volatility from daily returns
sd(retd)
# Minutely SPY volatility scaled to daily interval
sqrt(6.5*60)*sd(retp)
# Minutely SPY returns without overnight price jumps (unit per second)
retp <- retp/rutils::diffit(xts::.index(retp))
retp[1] <- 0
# Daily SPY volatility from minutely returns
sqrt(6.5*60)*60*sd(retp)
# Daily SPY returns without weekend and holiday price jumps (unit per second)
retd <- retd/rutils::diffit(xts::.index(retd))
retd[1] <- 0
# Daily SPY volatility without weekend and holiday price jumps
24*60*60*sd(retd)

# Calculate volatilities for vector of aggregation intervals
aggv <- seq.int(from=3, to=35, length.out=9)^2
volv <- sapply(aggv, function(agg) {
  naggs <- nrows %/% agg
  endd <- c(0, nrows - naggs*agg + (0:naggs)*agg)
  # endd <- rutils::calc_endpoints(closep, interval=agg)
  sd(rutils::diffit(closep[endd]))
})  # end sapply
# Calculate the Hurst from single data point
volog <- log(volv)
agglog <- log(aggv)
(last(volog) - first(volog))/(last(agglog) - first(agglog))
# Calculate the Hurst from regression slope using formula
hurstexp <- cov(volog, agglog)/var(agglog)
# Or using function lm()
regmod <- lm(volog ~ agglog)
coef(regmod)[2]

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
closep <- cumsum(retp)
nrows <- NROW(closep)
# Calculate the rescaled range
agg <- 500
naggs <- nrows %/% agg
endd <- c(0, nrows - naggs*agg + (0:naggs)*agg)
# Or
# endd <- rutils::calc_endpoints(closep, interval=agg)
rrange <- sapply(2:NROW(endd), function(np) {
  indeks <- (endd[np-1]+1):endd[np]
  diff(range(closep[indeks]))/sd(retp[indeks])
})  # end sapply
mean(rrange)
# Calculate the Hurst from single data point
log(mean(rrange))/log(agg)

# Calculate the rescaled range for vector of aggregation intervals
rrange <- sapply(aggv, function(agg) {
# Calculate the end points
  naggs <- nrows %/% agg
  endd <- c(0, nrows - naggs*agg + (0:naggs)*agg)
# Calculate the rescaled ranges
  rrange <- sapply(2:NROW(endd), function(np) {
    indeks <- (endd[np-1]+1):endd[np]
    diff(range(closep[indeks]))/sd(retp[indeks])
  })  # end sapply
  mean(na.omit(rrange))
})  # end sapply
# Calculate the Hurst as regression slope using formula
rangelog <- log(rrange)
agglog <- log(aggv)
hurstexp <- cov(rangelog, agglog)/var(agglog)
# Or using function lm()
regmod <- lm(rangelog ~ agglog)
coef(regmod)[2]

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
# Calculate the names of the stocks (remove NULL pricev)
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
volv <- sapply(pricev, function(closep) {
    sqrt(HighFreq::calc_var(HighFreq::diffit(closep)))
})  # end sapply
# Dygraph of stock with highest volatility
namev <- names(which.max(volv))
dygraphs::dygraph(get(namev, pricev), main=namev)
# Dygraph of stock with lowest volatility
namev <- names(which.min(volv))
dygraphs::dygraph(get(namev, pricev), main=namev)
# Calculate the regression of the Hurst exponents versus volatilities
regmod <- lm(hurstv ~ volv)
summary(regmod)

# Plot scatterplot of the Hurst exponents versus volatilities
plot(hurstv ~ volv, xlab="Volatility", ylab="Hurst",
     main="Hurst Exponents Versus Volatilities of Stocks")
# Add regression line
abline(model, col='red', lwd=3)
tvalue <- summary(regmod)$coefficients[2, "t value"]
tvalue <- round(tvalue, 3)
text(x=mean(volv), y=max(hurstv),
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
regmod <- lm(volatos ~ volatis)
summary(regmod)

# Plot scatterplot of the out-of-sample versus in-sample volatility
plot(volatos ~ volatis, xlab="In-sample Volatility", ylab="Out-of-sample Volatility",
     main="Out-of-Sample Versus In-Sample Volatility of Stocks")
# Add regression line
abline(model, col='red', lwd=3)
tvalue <- summary(regmod)$coefficients[2, "t value"]
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
regmod <- lm(hurstos ~ hurstis)
summary(regmod)

# Plot scatterplot of the out-of-sample versus in-sample Hurst exponents
plot(hurstos ~ hurstis, xlab="In-sample Hurst", ylab="Out-of-sample Hurst",
     main="Out-of-Sample Versus In-Sample Hurst Exponents of Stocks")
# Add regression line
abline(model, col='red', lwd=3)
tvalue <- summary(regmod)$coefficients[2, "t value"]
tvalue <- round(tvalue, 3)
text(x=mean(hurstis), y=max(hurstos),
     lab=paste("t-value =", tvalue), lwd=2, cex=1.2)

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

# Simulate the mean reverting strategy
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
# Plot dygraph of mean reverting strategy
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="VTI Daily Mean Reverting Strategy") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%

  dyLegend(show="always", width=300)

# Simulate mean reverting strategy with two day holding period
posv <- -rutils::roll_sum(sign(retp), lookb=2)/2
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
posv <- -rutils::roll_sum(sign(retp), lookb=2)/2
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
# Plot dygraph of mean reverting strategy
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="VTI EMA Daily Mean Reverting Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Calculate the EMA returns and volatilities
volv <- HighFreq::run_var(retp, lambda=0.5)
retma <- volv[, 1]
volv <- sqrt(volv[, 2])
# Scale the returns by their trailing volatility
retsc <- ifelse(volv > 0, retp/volv, 0)
# Calculate the positions and PnLs
posv <- -rutils::lagit(retma, lagg=1)
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
coefsd <- sqrt(diag(covmat))
# Calculate the t-values of the AR coefficients
coefft <- drop(coeff/coefsd)
coeffn <- paste0("phi", 0:(NROW(coefft)-1))
# Plot the t-values of the AR coefficients
barplot(coefft ~ coeffn, xlab="", ylab="t-value", col="grey",
  main="Coefficient t-values of AR Forecasting Model")

# Calculate the trailing volatility of the residuals
residv <- sqrt(HighFreq::run_var(resids, lambda=0.9)[, 2])

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
fcastv <- sqrt(HighFreq::run_var(fcasts, lambda=0.2)[, 2])
posv <- ifelse(fcastv > 0, fcasts/fcastv, 0)
# Calculate the autoregressive strategy PnLs
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
# Plot dygraph of the autoregressive strategy
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
# Plot dygraph of the autoregressive strategy
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
# Plot dygraph of the autoregressive strategy
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Autoregressive Strategy Low Volatility Coefficients") %>%
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
fcastv <- sqrt(HighFreq::run_var(fcasts, lambda=0.8)[, 2])
fcastv[1:100] <- 1
fcasts <- fcasts/fcastv
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
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Winsorized Autoregressive Strategy In-Sample") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Scale the returns by their trailing volatility
varv <- HighFreq::run_var(retp, lambda=0.99)[, 2]
retsc <- ifelse(varv > 0, retp/sqrt(varv), 0)
# Calculate the AR coefficients
predm <- lapply(1:orderp, rutils::lagit, input=retsc)
predm <- rutils::do_call(cbind, predm)
predm <- cbind(rep(1, nrows), predm)
colnames(predm) <- c("phi0", paste0("lag", 1:orderp))
predinv <- MASS::ginv(predm)
coeff <- predinv %*% retsc
# Calculate the scaled in-sample forecasts of VTI
fcasts <- predm %*% coeff
fcastv <- sqrt(HighFreq::run_var(fcasts, lambda=0.8)[, 2])
fcastv[1:100] <- 1
fcasts <- fcasts/fcastv

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
coeff <- predinv %*% respv
# Calculate the scaled in-sample forecasts of VTI
fcasts <- predm %*% coeff
fcastv <- sqrt(HighFreq::run_var(fcasts, lambda=0.8)[, 2])
fcastv[1:100] <- 1
fcasts <- fcasts/fcastv

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

# Calculate the forecasts as a function of the AR order
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
# Calculate the forecasts as a function of the AR order
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
colv <- colnames(pnls)
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
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
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
dygraphs::dygraph(cumsum(wealthv)[endd],
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
dygraphs::dygraph(cumsum(wealthv)[endd],
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
fcastv <- sqrt(HighFreq::run_var(fcasts, lambda=0.8)[, 2])
fcastv[1:100] <- 1
fcasts <- fcasts/fcastv

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
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
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
bidask <- 0.0001 # The bid-ask spread is equal to 1 basis point
costv <- 0.5*bidask*abs(rutils::diffit(posv))
pnls <- (pnls - costv)
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
dygraphs::dygraph(cumsum(wealthv)[endd],
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
dygraphs::dygraph(cumsum(wealthv)[endd],
main="Overnight Trend For Daytime VTI Returns") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
