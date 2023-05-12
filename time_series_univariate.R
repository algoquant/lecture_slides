# Get documentation for package tseries
packageDescription("tseries")  # Get short description
help(package="tseries")  # Load help page
library(tseries)  # Load package tseries
data(package="tseries")  # List all datasets in "tseries"
ls("package:tseries")  # List all objects in "tseries"
detach("package:tseries")  # Remove tseries from search path
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
load(file="/Users/jerzy/Develop/lecture_slides/data/zoo_data.RData")
# Get start and end dates
datev <- time(stxts_adj)
endd <- datev[NROW(datev)]
startd <- round((4*endd + datev[1])/5)
# Plot using plotOHLC
plotOHLC(window(stxts_adj,
          start=startd,
          end=endd)[, 1:4],
   xlab="", ylab="")
title(main="MSFT OHLC Prices")
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
library(lubridate)  # Load lubridate
# Get start and end dates of msft
startd <- lubridate::decimal_date(start(msft))
endd <- lubridate::decimal_date(end(msft))
# Calculate frequency of msft
tstep <- NROW(msft)/(endd-startd)
# Extract data from msft
datav <- zoo::coredata(
  window(msft, start=as.Date("2015-01-01"),
   end=end(msft)))
# Create ts object using ts()
stxts <- ts(data=datav,
  start=lubridate::decimal_date(as.Date("2015-01-01")),
  frequency=tstep)
seqplot.ts(x=stxts[, 1], y=stxts[, 4], xlab="", ylab="")
title(main="MSFT Open and Close Prices", line=-1)
library(tseries)  # Load package tseries
# Calculate maximum drawdown
maxdrawdown(msft_adj[, "AdjClose"])
max_drawd <- maxdrawdown(msft_adj[, "AdjClose"])
zoo::index(msft_adj)[max_drawd$from]
zoo::index(msft_adj)[max_drawd$to]
# Calculate Sharpe ratio
sharpe(msft_adj[, "AdjClose"])
# Calculate Sterling ratio
sterling(as.numeric(msft_adj[, "AdjClose"]))
msft <- suppressWarnings(  # Load MSFT data
  get.hist.quote(instrument="MSFT",
           start=Sys.Date()-365,
           end=Sys.Date(),
           origin="1970-01-01")
)  # end suppressWarnings
class(msft)
dim(msft)
tail(msft, 4)
# Calculate Sharpe ratio
sharpe(msft[, "Close"], r=0.01)
# Add title
plot(msft[, "Close"], xlab="", ylab="")
title(main="MSFT Close Prices", line=-1)
library(tseries)  # Load package tseries
msft <- suppressWarnings(  # Load MSFT data
  get.hist.quote(instrument="MSFT",
           start=Sys.Date()-365,
           end=Sys.Date(),
           origin="1970-01-01")
)  # end suppressWarnings
class(msft)
dim(msft)
tail(msft, 4)
# Calculate Sharpe ratio
sharpe(msft[, "Close"], r=0.01)
# Add title
plot(msft[, "Close"], xlab="", ylab="")
title(main="MSFT Close Prices", line=-1)
library(tseries)  # Load package tseries
msft <- suppressWarnings(  # Load MSFT data
  get.hist.quote(instrument="MSFT",
           start=Sys.Date()-365,
           end=Sys.Date(),
           origin="1970-01-01")
)  # end suppressWarnings
class(msft)
dim(msft)
tail(msft, 4)
# Calculate Sharpe ratio
sharpe(msft[, "Close"], r=0.01)
# Add title
plot(msft[, "Close"], xlab="", ylab="")
title(main="MSFT Close Prices", line=-1)
# Load package quantmod
library(quantmod)
# Get documentation for package quantmod
# Get short description
packageDescription("quantmod")
# Load help page
help(package="quantmod")
# List all datasets in "quantmod"
data(package="quantmod")
# List all objects in "quantmod"
ls("package:quantmod")
# Remove quantmod from search path
detach("package:quantmod")
library(quantmod)
# Plot OHLC candlechart with volume
chartSeries(etfenv$VTI["2014-11"],
      name="VTI",
      theme=chartTheme("white"))
# Plot OHLC bar chart with volume
chartSeries(etfenv$VTI["2014-11"],
      type="bars",
      name="VTI",
      theme=chartTheme("white"))
library(quantmod)
# Plot OHLC candlechart with volume
chartSeries(etfenv$VTI["2008-11/2009-04"], name="VTI")
# Redraw plot only for Feb-2009, with white theme
reChart(subset="2009-02", theme=chartTheme("white"))
library(quantmod)
# Candlechart with Bollinger Bands
chartSeries(etfenv$VTI["2014"],
      TA="addBBands(): addBBands(draw='percent'): addVo()",
      name="VTI with Bollinger Bands",
      theme=chartTheme("white"))
# Candlechart with two Moving Averages
chartSeries(etfenv$VTI["2014"],
      TA="addVo(): addEMA(10): addEMA(30)",
      name="VTI with Moving Averages",
      theme=chartTheme("white"))
# Candlechart with Commodity Channel Index
chartSeries(etfenv$VTI["2014"],
      TA="addVo(): addBBands(): addCCI()",
      name="VTI with Technical Indicators",
      theme=chartTheme("white"))
library(quantmod)
library(TTR)
ohlc <- rutils::etfenv$VTI["2009-02/2009-03"]
VTI_close <- quantmod::Cl(ohlc)
VTI_vol <- quantmod::Vo(ohlc)
# Calculate volume-weighted average price
vwapv <- TTR::VWAP(price=VTI_close, volume=VTI_vol, n=10)
# Plot OHLC candlechart with volume
chartSeries(ohlc, name="VTI plus VWAP", theme=chartTheme("white"))
# Add VWAP to main plot
addTA(ta=vwapv, on=1, col='red')
# Add price minus VWAP in extra panel
addTA(ta=(VTI_close-vwapv), col='red')
library(quantmod)
library(TTR)
ohlc <- rutils::etfenv$VTI
VTI_close <- quantmod::Cl(ohlc)
VTI_vol <- quantmod::Vo(ohlc)
vwapv <- TTR::VWAP(price=VTI_close, volume=VTI_vol, n=10)
VTI_close <- VTI_close["2009-02/2009-03"]
ohlc <- ohlc["2009-02/2009-03"]
vwapv <- vwapv["2009-02/2009-03"]
# Plot OHLC candlechart with volume
chartSeries(ohlc, name="VTI plus VWAP shaded",
      theme=chartTheme("white"))
# Add VWAP to main plot
addTA(ta=vwapv, on=1, col='red')
# Add price minus VWAP in extra panel
addTA(ta=(VTI_close-vwapv), col='red')
# Add background shading of areas
addTA((VTI_close-vwapv) > 0, on=-1,
col="lightgreen", border="lightgreen")
addTA((VTI_adj-vwapv) < 0, on=-1,
col="lightgrey", border="lightgrey")
# Add vertical and horizontal lines at vwapv minimum
addLines(v=which.min(vwapv), col='red')
addLines(h=min(vwapv), col='red')
library(quantmod)
library(TTR)
ohlc <- rutils::etfenv$VTI
VTI_adj <- quantmod::Cl(ohlc)
VTI_vol <- quantmod::Vo(ohlc)
vwapv <- TTR::VWAP(price=VTI_adj, volume=VTI_vol, n=10)
VTI_adj <- VTI_adj["2009-02/2009-03"]
ohlc <- ohlc["2009-02/2009-03"]
vwapv <- vwapv["2009-02/2009-03"]
# OHLC candlechart VWAP in main plot,
chart_Series(x=ohlc, # Volume in extra panel
       TA="add_Vo(); add_TA(vwapv, on=1)",
       name="VTI plus VWAP shaded")
# Add price minus VWAP in extra panel
add_TA(VTI_adj-vwapv, col='red')
# Add background shading of areas
add_TA((VTI_adj-vwapv) > 0, on=-1,
col="lightgreen", border="lightgreen")
add_TA((VTI_adj-vwapv) < 0, on=-1,
col="lightgrey", border="lightgrey")
# Add vertical and horizontal lines
abline(v=which.min(vwapv), col='red')
abline(h=min(vwapv), col='red')
library(quantmod)
ohlc <- rutils::etfenv$VTI["2009-02/2009-03"]
# Extract plot object
chobj <- chart_Series(x=ohlc, plot=FALSE)
class(chobj)
ls(chobj)
class(chobj$get_ylim)
class(chobj$set_ylim)
# ls(chobj$Env)
class(chobj$Env$actions)
plot_theme <- chart_theme()
class(plot_theme)
ls(plot_theme)
library(quantmod)
ohlc <- rutils::etfenv$VTI["2010-04/2010-05"]
# Extract, modify theme, format tick marks "%b %d"
plot_theme <- chart_theme()
plot_theme$format.labels <- "%b %d"
# Create plot object
chobj <- chart_Series(x=ohlc, theme=plot_theme, plot=FALSE)
# Extract ylim using accessor function
ylim <- chobj$get_ylim()
ylim[[2]] <- structure(range(quantmod::Cl(ohlc)) + c(-1, 1),
  fixed=TRUE)
# Modify plot object to reduce y-axis range
chobj$set_ylim(ylim)  # use setter function
# Render the plot
plot(chobj)
library(rutils)
# Calculate VTI and XLF volume-weighted average price
vwapv <- TTR::VWAP(price=quantmod::Cl(rutils::etfenv$VTI),
      volume=quantmod::Vo(rutils::etfenv$VTI), n=10)
XLF_vwap <- TTR::VWAP(price=quantmod::Cl(rutils::etfenv$XLF),
      volume=quantmod::Vo(rutils::etfenv$XLF), n=10)
# Open graphics device, and define
# Plot area with two horizontal panels
x11(); par(mfrow=c(2, 1))
chobj <- chart_Series(  # Plot in top panel
  x=etfenv$VTI["2009-02/2009-04"],
  name="VTI", plot=FALSE)
add_TA(vwapv["2009-02/2009-04"], lwd=2, on=1, col='blue')
# Plot in bottom panel
chobj <- chart_Series(x=etfenv$XLF["2009-02/2009-04"],
  name="XLF", plot=FALSE)
add_TA(XLF_vwap["2009-02/2009-04"], lwd=2, on=1, col='blue')
# Open plot window and set plot margins
x11(width=6, height=4)
par(mar=c(2, 2, 2, 2), oma=c(1, 1, 1, 1))
# Plot first time series without x-axis
zoo::plot.zoo(pricev[, 1], lwd=2, col="orange",
        xlab=NA, ylab=NA, xaxt="n")
# Create X-axis date labels and add X-axis
datev <- pretty(zoo::index(pricev))
axis(side=1, at=datev, labels=format(datev, "%b-%d-%y"))
# Plot second time series without y-axis
par(new=TRUE)  # Allow new line on same plot
zoo::plot.zoo(pricev[, 2], xlab=NA, ylab=NA,
        lwd=2, yaxt="n", col="blue", xaxt="n")
# Plot second y-axis on right
axis(side=4, lwd=2, col="blue")
# Add axis labels
mtext(colnamev[1], cex=1.2, lwd=3, side=2, las=2, adj=(-0.5), padj=(-5), col="orange")
mtext(colnamev[2], cex=1.2, lwd=3, side=4, las=2, adj=1.5, padj=(-5), col="blue")
# Add title and legend
title(main=paste(colnamev, collapse=" and "), line=0.5)
legend("top", legend=colnamev, cex=1.0, bg="white",
 lty=1, lwd=6, col=c("orange", "blue"), bty="n")
library(dygraphs)
# Calculate volume-weighted average price
ohlc <- rutils::etfenv$VTI
vwapv <- TTR::VWAP(price=quantmod::Cl(ohlc),
    volume=quantmod::Vo(ohlc), n=20)
# Add VWAP to OHLC data
datav <- cbind(ohlc[, 1:4], vwapv)["2009-01/2009-04"]
# Create dygraphs object
dyplot <- dygraphs::dygraph(datav)
# Increase line width and color
dyplot <- dygraphs::dyOptions(dyplot,
  colors="red", strokeWidth=3)
# Convert dygraphs object to candlestick plot
dyplot <- dygraphs::dyCandlestick(dyplot)
# Render candlestick plot
dyplot
# Candlestick plot using pipes syntax
dygraphs::dygraph(datav) %>% dyCandlestick() %>%
  dyOptions(colors="red", strokeWidth=3)
# Candlestick plot without using pipes syntax
dygraphs::dyCandlestick(dygraphs::dyOptions(dygraphs::dygraph(datav),
  colors="red", strokeWidth=3))
# Create candlestick plot with background shading
indic <- (Cl(datav) > datav[, "VWAP"])
whichv <- which(rutils::diffit(indic) != 0)
indic <- rbind(first(indic), indic[whichv, ], last(indic))
datev <- zoo::index(indic)
indic <- ifelse(drop(coredata(indic)), "lightgreen", "antiquewhite")
# Create dygraph object without rendering it
dyplot <- dygraphs::dygraph(datav) %>% dyCandlestick() %>%
  dyOptions(colors="red", strokeWidth=3)
# Add shading
for (i in 1:(NROW(indic)-1)) {
    dyplot <- dyplot %>%
dyShading(from=datev[i], to=datev[i+1], color=indic[i])
}  # end for
# Render the dygraph object
dyplot
library(dygraphs)
# Prepare VTI and IEF prices
pricev <- cbind(Cl(rutils::etfenv$VTI), Cl(rutils::etfenv$IEF))
pricev <- na.omit(pricev)
colnamev <- rutils::get_name(colnames(pricev))
colnames(pricev) <- colnamev
# dygraphs plot with two y-axes
library(dygraphs)
dygraphs::dygraph(pricev, main=paste(colnamev, collapse=" and ")) %>%
  dyAxis(name="y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis(name="y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", strokeWidth=2, col="red") %>%
  dySeries(name=colnamev[2], axis="y2", strokeWidth=2, col="blue")
# Load package qmao
library(qmao)
# Get documentation for package qmao
# Get short description
packageDescription("qmao")
# Load help page
help(package="qmao")
# List all datasets in "qmao"
data(package="qmao")
# List all objects in "qmao"
ls("package:qmao")
# Remove qmao from search path
detach("package:qmao")
set.seed(1121)  # Reset random number generator
# Sample from Standard Normal Distribution
nrows <- 1000
datav <- rnorm(nrows)
# Sample mean - MC estimate
mean(datav)
# Sample standard deviation - MC estimate
sd(datav)
# Monte Carlo estimate of cumulative probability
pnorm(1)
sum(datav < 1)/nrows
# Monte Carlo estimate of quantile
confl <- 0.98
qnorm(confl)  # Exact value
cutoff <- confl*nrows
datav <- sort(datav)
datav[cutoff]  # Naive Monte Carlo value
quantile(datav, probs=confl)
# Analyze the source code of quantile()
stats:::quantile.default
# Microbenchmark quantile
library(microbenchmark)
summary(microbenchmark(
  monte_carlo = datav[cutoff],
  quantv = quantile(datav, probs=confl),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
set.seed(1121)  # Reset random number generator
barl <- 20  # Barrier level
nrows <- 1000  # Number of simulation steps
pathv <- numeric(nrows)  # Allocate path vector
pathv[1] <- rnorm(1)  # Initialize path
it <- 2  # Initialize simulation index
while ((it <= nrows) && (pathv[it - 1] < barl)) {
# Simulate next step
  pathv[it] <- pathv[it - 1] + rnorm(1)
  it <- it + 1  # Advance index
}  # end while
# Fill remaining path after it crosses barl
if (it <= nrows)
  pathv[it:nrows] <- pathv[it - 1]
# Plot the Brownian motion
x11(width=6, height=4)
par(mar=c(3, 3, 2, 1), oma=c(1, 1, 1, 1))
plot(pathv, type="l", col="black",
     lty="solid", lwd=2, xlab="", ylab="")
abline(h=barl, lwd=3, col="red")
title(main="Brownian Motion Crossing a Barrier Level", line=0.5)
set.seed(1121)  # Reset random number generator
barl <- 20  # Barrier level
nrows <- 1000  # Number of simulation steps
# Simulate path of Brownian motion
pathv <- cumsum(rnorm(nrows))
# Find index when path crosses barl
crossp <- which(pathv > barl)
# Fill remaining path after it crosses barl
if (NROW(crossp)>0) {
  pathv[(crossp[1]+1):nrows] <- pathv[crossp[1]]
}  # end if
# Plot the Brownian motion
x11(width=6, height=4)
par(mar=c(3, 3, 2, 1), oma=c(1, 1, 1, 1))
plot(pathv, type="l", col="black",
     lty="solid", lwd=2, xlab="", ylab="")
abline(h=barl, lwd=3, col="red")
title(main="Brownian Motion Crossing a Barrier Level", line=0.5)
# Define daily volatility and growth rate
sigmav <- 0.01; drift <- 0.0; nrows <- 1000
# Simulate geometric Brownian motion
retp <- sigmav*rnorm(nrows) + drift - sigmav^2/2
pricev <- exp(cumsum(retp))
plot(pricev, type="l", xlab="time", ylab="prices",
     main="geometric Brownian motion")
# Simulate geometric Brownian motion
sigmav <- 0.01/sqrt(48)
drift <- 0.0
nrows <- 1e4
datev <- seq(from=as.POSIXct(paste(Sys.Date()-250, "09:30:00")),
  length.out=nrows, by="30 min")
pricev <- exp(cumsum(sigmav*rnorm(nrows) + drift - sigmav^2/2))
pricev <- xts(pricev, order.by=datev)
pricev <- cbind(pricev,
  volume=sample(x=10*(2:18), size=nrows, replace=TRUE))
# Aggregate to daily OHLC data
ohlc <- xts::to.daily(pricev)
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
pricev <- rnorm(npaths*nrows, sd=sigmav) + drift - sigmav^2/2
pricev <- matrix(pricev, nc=npaths)
pricev <- exp(matrixStats::colCumsums(pricev))
# Create xts time series
pricev <- xts(pricev, order.by=seq.Date(Sys.Date()-nrows+1, Sys.Date(), by=1))
# Sort the columns according to largest terminal values
pricev <- pricev[, order(pricev[nrows, ])]
# Plot xts time series
colorv <- colorRampPalette(c("red", "blue"))(NCOL(pricev))
par(mar=c(3, 3, 2, 2), oma=c(0, 0, 0, 0))
plot.zoo(pricev, main="Multiple paths of geometric Brownian motion",
   xlab=NA, ylab=NA, plot.type="single", col=colorv)
# Define daily volatility and growth rate
sigmav <- 0.01; drift <- 0.0; nrows <- 10000
npaths <- 100
# Simulate multiple paths of geometric Brownian motion
pricev <- rnorm(npaths*nrows, sd=sigmav) + drift - sigmav^2/2
pricev <- matrix(pricev, nc=npaths)
pricev <- exp(matrixStats::colCumsums(pricev))
# Calculate fraction of paths below the expected value
fractv <- rowSums(pricev < 1.0) / npaths
# Create xts time series of percentage of paths below the expected value
fractv <- xts(fractv, order.by=seq.Date(Sys.Date()-NROW(fractv)+1, Sys.Date(), by=1))
# Plot xts time series of percentage of paths below the expected value
par(mar=c(3, 3, 2, 2), oma=c(0, 0, 0, 0))
plot.zoo(fractv, main="Percentage of GBM paths below mean",
   xlab=NA, ylab=NA, col="blue")
# Load S&P500 stock prices
load("/Users/jerzy/Develop/lecture_slides/data/sp500.RData")
ls(sp500env)
# Extract the closing prices
pricev <- eapply(sp500env, quantmod::Cl)
# Flatten the prices into a single xts series
pricev <- rutils::do_call(cbind, pricev)
# Carry forward and backward non-NA prices
pricev <- zoo::na.locf(pricev, na.rm=FALSE)
pricev <- zoo::na.locf(pricev, fromLast=TRUE)
sum(is.na(pricev))
# Drop ".Close" from column names
colnames(pricev)
colnames(pricev) <- rutils::get_name(colnames(pricev))
# Or
# colnames(pricev) <- do.call(rbind,
#   strsplit(colnames(pricev), split="[.]"))[, 1]
# Select prices after the year 2000
pricev <- pricev["2000/", ]
# Scale the columns so that prices start at 1
pricev <- lapply(pricev, function(x) x/as.numeric(x[1]))
pricev <- rutils::do_call(cbind, pricev)
# Sort the columns according to the final prices
nrows <- NROW(pricev)
ordern <- order(pricev[nrows, ])
pricev <- pricev[, ordern]
# Select 20 symbols
symbolv <- colnames(pricev)
symbolv <- symbolv[round(seq.int(from=1, to=NROW(symbolv), length.out=20))]
# Plot xts time series of prices
colorv <- colorRampPalette(c("red", "blue"))(NROW(symbolv))
endd <- rutils::calc_endpoints(pricev, interval="weeks")
plot.zoo(pricev[endd, symbolv], main="20 S&P500 Stock Prices (scaled)",
   xlab=NA, ylab=NA, plot.type="single", col=colorv)
legend(x="topleft", inset=0.02, cex=0.5, bty="n", y.intersp=0.5,
 legend=rev(symbolv), col=rev(colorv), lwd=6, lty=1)
# Calculate the final stock prices
pricef <- drop(zoo::coredata(pricev[nrows, ]))
# Calculate the mean and median stock prices
max(pricef); min(pricef)
which.max(pricef)
which.min(pricef)
mean(pricef)
median(pricef)
# Calculate the percentage of stock prices below the mean
sum(pricef < mean(pricef))/NROW(pricef)
# Plot a histogram of final stock prices
hist(pricef, breaks=1e3, xlim=c(0, 300),
     xlab="Stock price", ylab="Count",
     main="Histogram of Final Stock Prices")
# Plot a histogram of final stock prices
abline(v=median(pricef), lwd=3, col="blue")
text(x=median(pricef), y=150, lab="median", pos=4)
abline(v=mean(pricef), lwd=3, col="red")
text(x=mean(pricef), y=100, lab="mean", pos=4)
# Calculate average of valid stock prices
validp <- (pricev != 1)  # Valid stocks
nstocks <- rowSums(validp)
nstocks[1] <- NCOL(pricev)
indeks <- rowSums(pricev*validp)/nstocks
# Calculate fraction of stock prices below the average price
fractv <- rowSums((pricev < indeks) & validp)/nstocks
# Create xts time series of average stock prices
indeks <- xts(indeks, order.by=zoo::index(pricev))
dev.new(width=6, height=4, noRStudioGD=TRUE)
# x11(width=6, height=4)
# Plot xts time series of average stock prices
plot.zoo(indeks, main="Average S&P500 Stock Prices (normalized from 1990)",
   xlab=NA, ylab=NA, col="blue")
# Create xts time series of percentage of stock prices below the average price
fractv <- xts(fractv, order.by=zoo::index(pricev))
# Plot percentage of stock prices below the average price
plot.zoo(fractv[-(1:2),],
   main="Percentage of S&P500 Stock Prices
   Below the Average Price",
   xlab=NA, ylab=NA, col="blue")
# Define daily volatility and growth rate
sigmav <- 0.01; drift <- 0.0; nrows <- 1000
# Simulate geometric Brownian motion
retp <- sigmav*rnorm(nrows) + drift - sigmav^2/2
pricev <- exp(cumsum(retp))
plot(pricev, type="l", xlab="time", ylab="prices",
     main="geometric Brownian motion")
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
acfv <- acf(retp, plot=FALSE)
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
rutils::plot_acf(retp, lag=10, main="")
title(main="ACF of VTI returns", line=-1)
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
# Calculate VTI and XLF percentage returns
retp <- rutils::etfenv$returns[, c("VTI", "XLF")]
retp <- na.omit(retp)
nrows <- NROW(retp)
# De-mean (center) and scale the returns
retp <- apply(retp, MARGIN=2, function(x) (x-mean(x))/sd(x))
apply(retp, MARGIN=2, sd)
# Calculate the correlation
drop(retp[, "VTI"] %*% retp[, "XLF"])/(nrows-1)
corv <- cor(retp[, "VTI"], retp[, "XLF"])
# Test statistical significance of correlation
cortest <- cor.test(retp[, "VTI"], retp[, "XLF"])
confl <- qnorm((1+0.95)/2)/sqrt(nrows)
corv*c(1-confl, 1+confl)
# Get source code
stats:::cor.test.default
# Ljung-Box test for VTI returns
# 'lag' is the number of autocorrelation coefficients
Box.test(retp, lag=10, type="Ljung")
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
# Calculate the monthly end points
endd <- rutils::calc_endpoints(retp, interval="weeks")
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
retp <- rutils::diffit(closep)
colnames(retp) <- "SPY"
# Open plot window under MS Windows
x11(width=6, height=4)
# Open plot window on Mac
dev.new(width=6, height=4, noRStudioGD=TRUE)
# Plot the autocorrelations of minutely SPY returns
acfv <- rutils::plot_acf(as.numeric(retp), lag=10,
     xlab="lag", ylab="Autocorrelation", main="")
title("Autocorrelations of Minutely SPY Returns", line=1)
# Calculate the sum of autocorrelations
sum(acfv$acf)
# Ljung-Box test for minutely SPY returns
Box.test(retp, lag=10, type="Ljung")
# Calculate hourly SPY percentage returns
closeh <- Cl(xts::to.period(x=ohlc, period="hours"))
retsh <- rutils::diffit(closeh)
# Ljung-Box test for hourly SPY returns
Box.test(retsh, lag=10, type="Ljung")
# Calculate daily SPY percentage returns
closed <- Cl(xts::to.period(x=ohlc, period="days"))
retd <- rutils::diffit(closed)
# Ljung-Box test for daily SPY returns
Box.test(retd, lag=10, type="Ljung")
# Ljung-Box test statistics for aggregated SPY returns
sapply(list(minutely=retp, hourly=retsh, daily=retd),
  function(rets) {
    Box.test(rets, lag=10, type="Ljung")$statistic
})  # end sapply
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
volat <- sapply(aggv, function(aggint) {
  naggs <- nrows %/% aggint
  endd <- c(0, nrows - naggs*aggint + (0:naggs)*aggint)
  # endd <- rutils::calc_endpoints(closep, interval=aggint)
  sd(rutils::diffit(closep[endd]))
})  # end sapply
# Calculate the Hurst from single data point
volog <- log(volat)
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
closep <- cumsum(retp)
nrows <- NROW(closep)
# Calculate the rescaled range
aggint <- 500
naggs <- nrows %/% aggint
endd <- c(0, nrows - naggs*aggint + (0:naggs)*aggint)
# Or
# endd <- rutils::calc_endpoints(closep, interval=aggint)
rrange <- sapply(2:NROW(endd), function(np) {
  indeks <- (endd[np-1]+1):endd[np]
  diff(range(closep[indeks]))/sd(retp[indeks])
})  # end sapply
mean(rrange)
# Calculate the Hurst from single data point
log(mean(rrange))/log(aggint)
# Calculate the rescaled range for vector of aggregation intervals
rrange <- sapply(aggv, function(aggint) {
# Calculate the end points
  naggs <- nrows %/% aggint
  endd <- c(0, nrows - naggs*aggint + (0:naggs)*aggint)
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
volum <- eapply(sp500env, function(ohlc) {
    sum(quantmod::Vo(ohlc)["2000/"])
})  # end eapply
# Remove NULL values
volum <- volum[names(pricev)]
volum <- unlist(volum)
which.max(volum)
# Calculate the number of NULL prices
sum(is.null(volum))
# Calculate the Hurst exponents of stocks
hurstv <- sapply(pricev, HighFreq::calc_hurst, aggv=aggv)
# Calculate the regression of the Hurst exponents versus trading volumes
model <- lm(hurstv ~ volum)
summary(model)
# Plot scatterplot of the Hurst exponents versus trading volumes
plot(hurstv ~ volum, xlab="Trading Volume", ylab="Hurst",
     main="Hurst Exponents Versus Trading Volumes of Stocks")
# Add regression line
abline(model, col='red', lwd=3)
tvalue <- summary(model)$coefficients[2, "t value"]
tvalue <- round(tvalue, 3)
text(x=quantile(volum, 0.998), y=max(hurstv),
     lab=paste("t-value =", tvalue), lwd=2, cex=1.2)
# Calculate log stock returns
retp <- lapply(pricev, rutils::diffit)
retp <- rutils::do_call(cbind, retp)
retp[is.na(retp)] <- 0
sum(is.na(retp))
# Drop ".Close" from column names
colnames(retp[, 1:4])
colnames(retp) <- rutils::get_name(colnames(retp))
# Calculate PCA prices using matrix algebra
eigend <- eigen(cor(retp))
retpca <- retp %*% eigend$vectors
pricepca <- xts::xts(matrixStats::colCumsums(retpca),
                 order.by=index(retp))
colnames(pricepca) <- paste0("PC", 1:NCOL(retp))
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
eigend <- eigen(cor(retp["/2010"]))
# Calculate out-of-sample PCA prices
retpca <- retp["2010/"] %*% eigend$vectors
pricepca <- xts::xts(matrixStats::colCumsums(retpca),
                 order.by=index(retp["2010/"]))
colnames(pricepca) <- paste0("PC", 1:NCOL(retp))
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
retp <- rutils::etfenv$returns[, symbolv]
retp[is.na(retp)] <- 0
sum(is.na(retp))
# Calculate the Hurst exponent of an ETF portfolio
calc_phurst <- function(weightv, retp) {
  -HighFreq::calc_hurst(matrix(cumsum(retp %*% weightv)), aggv=aggv)
}  # end calc_phurst
# Calculate the portfolio weights with maximum Hurst
nweights <- NCOL(retp)
weightv <- rep(1/sqrt(nweights), nweights)
calc_phurst(weightv, retp=retp)
optiml <- optim(par=weightv, fn=calc_phurst, retp=retp,
          method="L-BFGS-B",
          upper=rep(10.0, nweights),
          lower=rep(-10.0, nweights))
# Optimal weights and maximum Hurst
weightv <- optiml$par
names(weightv) <- colnames(retp)
-calc_phurst(weightv, retp=retp)
# Dygraph of ETF portfolio with largest Hurst exponent
wealthv <- xts::xts(cumsum(retp %*% weightv), zoo::index(retp))
dygraphs::dygraph(wealthv, main="ETF Portfolio With Largest Hurst Exponent")
# Calculate the in-sample maximum Hurst portfolio weights
optiml <- optim(par=weightv, fn=calc_phurst, retp=retp["/2010"],
          method="L-BFGS-B",
          upper=rep(10.0, nweights),
          lower=rep(-10.0, nweights))
# Optimal weights and maximum Hurst
weightv <- optiml$par
names(weightv) <- colnames(retp)
# Calculate the in-sample Hurst exponent
-calc_phurst(weightv, retp=retp["/2010"])
# Calculate the out-of-sample Hurst exponent
-calc_phurst(weightv, retp=retp["2010/"])
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
# Set two vertical plot panels
par(mfrow=c(2,1))
# Simulate AR process of returns
arimav <- arima.sim(n=1e5, model=list(ar=c(0.0, 0.5, 0.1)))
# ACF of AR(3) process
rutils::plot_acf(arimav, lag=10, xlab="", ylab="",
   main="ACF of AR(3) process")
# PACF of AR(3) process
pacf(arimav, lag=10, xlab="", ylab="", main="PACF of AR(3) process")
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
  theta=thetav, innov=matrix(innov))
all.equal(pricev, drop(pricev_cpp))
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
retp[2] <- thetav*(priceq - pricev[1]) + coeff[1]*retp[1] +
  innov[2]
pricev[2] <- pricev[1] + retp[2]
retp[3] <- thetav*(priceq - pricev[2]) + coeff[1]*retp[2] +
  coeff[2]*retp[1] + innov[3]
pricev[3] <- pricev[2] + retp[3]
for (it in 4:nrows) {
  retp[it] <- thetav*(priceq - pricev[it-1]) +
    retp[(it-1):(it-3)] %*% coeff + innov[it]
  pricev[it] <- pricev[it-1] + retp[it]
}  # end for
# Simulate Dickey-Fuller process in Rcpp
pricecpp <- HighFreq::sim_df(init_price=prici, eq_price=priceq,
   theta=thetav, coeff=matrix(coeff), innov=matrix(innov))
# Compare prices
all.equal(pricev, drop(pricev_cpp))
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
nrows <- 1e3
# Perform ADF test for AR(1) with small coefficient
set.seed(1121)
arimav <- arima.sim(n=nrows, model=list(ar=0.01))
tseries::adf.test(arimav)
# Perform ADF test for AR(1) with large coefficient
set.seed(1121)
arimav <- arima.sim(n=nrows, model=list(ar=0.99))
tseries::adf.test(arimav)
# Perform ADF test with lag = 1
tseries::adf.test(arimav, k=1)
# Perform Dickey-Fuller test
tseries::adf.test(arimav, k=0)
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
# Specify AR process parameters
nrows <- 1e3
coeff <- matrix(c(0.1, 0.39, 0.5)); ncoeff <- NROW(coeff)
set.seed(1121); innov <- matrix(rnorm(nrows))
# Simulate AR process using HighFreq::sim_ar()
arimav <- HighFreq::sim_ar(coeff=coeff, innov=innov)
# Fit AR model using ar.ols()
arfit <- ar.ols(arimav, order.max=ncoeff, aic=FALSE)
class(arfit)
is.list(arfit)
drop(arfit$ar); drop(coeff)
# Define predictor matrix without intercept column
predm <- sapply(1:ncoeff, rutils::lagit, input=arimav)
# Fit AR model using regression
predinv <- MASS::ginv(predm)
coeff <- drop(predinv %*% arimav)
all.equal(drop(arfit$ar), coeff, check.attributes=FALSE)
# Specify AR process parameters
nrows <- 1e3
coeff <- c(0.1, 0.39, 0.5); ncoeff <- NROW(coeff)
# Simulate AR process using C_rfilter()
set.seed(1121); innov <- rnorm(nrows, sd=0.01)
arimav <- .Call(stats:::C_rfilter, innov, coeff,
  double(nrows + ncoeff))[-(1:ncoeff)]
# wippp
# Calibrate ARIMA model using regression
# Define predictor matrix
arimav <- (arimav - mean(arimav))
predm <- sapply(1:3, rutils::lagit, input=arimav)
# Calculate de-meaned returns matrix
predm <- t(t(predm) - colMeans(predm))
predinv <- MASS::ginv(predm)
# Regression coefficients with response equal to arimav
coeff <- drop(predinv %*% arimav)
all.equal(arfit$coef, coeff, check.attributes=FALSE)
# Calculate the model residuals
fitv <- drop(predm %*% coeff)
resids <- drop(arimav - fitv)
# Variance of residuals
residsd <- sum(resids^2)/(nrows-NROW(coeff))
# Inverse of predictor matrix squared
predm2 <- MASS::ginv(crossprod(predm))
# Calculate covariance matrix of AR coefficients
covar <- residsd*predm2
coeffsd <- sqrt(diag(covar))
# Calculate t-values of AR coefficients
coefft <- drop(coeff)/coeffsd
# Plot the t-values of the AR coefficients
barplot(coefft, xlab="lag", ylab="t-value",
  main="Coefficient t-values of AR Forecasting Model")
# Fit AR(5) model into AR(3) process
predm <- sapply(1:5, rutils::lagit, input=arimav)
predinv <- MASS::ginv(predm)
coeff <- drop(predinv %*% arimav)
# Calculate t-values of AR(5) coefficients
resids <- drop(arimav - drop(predm %*% coeff))
residsd <- sum(resids^2)/(nrows-NROW(coeff))
covar <- residsd*MASS::ginv(crossprod(predm))
coeffsd <- sqrt(diag(covar))
coefft <- drop(coeff)/coeffsd
# Fit AR(5) model using arima()
arfit <- arima(arimav, order=c(5, 0, 0), include.mean=FALSE)
arfit$coef
# Fit AR(5) model using auto.arima()
library(forecast)  # Load forecast
arfit <- forecast::auto.arima(arimav, max.p=5, max.q=0, max.d=0)
# Fit AR(5) model into VTI returns
retp <- drop(zoo::coredata(na.omit(rutils::etfenv$returns$VTI)))
predm <- sapply(1:5, rutils::lagit, input=retp)
predinv <- MASS::ginv(predm)
coeff <- drop(predinv %*% retp)
# Calculate t-values of AR(5) coefficients
resids <- drop(retp - drop(predm %*% coeff))
residsd <- sum(resids^2)/(nrows-NROW(coeff))
covar <- residsd*MASS::ginv(crossprod(predm))
coeffsd <- sqrt(diag(covar))
coefft <- drop(coeff)/coeffsd
# Calibrate ARIMA model using auto.arima()
# library(forecast)  # Load forecast
forecast::auto.arima(arimav, max.p=3, max.q=0, max.d=0)
# Calibrate ARIMA model using arima()
arfit <- arima(arimav, order=c(3,0,0), include.mean=FALSE)
arfit$coef
# Calibrate ARIMA model using auto.arima()
# library(forecast)  # Load forecast
forecast::auto.arima(arimav, max.p=3, max.q=0, max.d=0)
# Calibrate ARIMA model using regression
arimav <- as.numeric(arimav)
# Define predictor matrix for arimav
predm <- sapply(1:3, rutils::lagit, input=arimav)
# Generalized inverse of predictor matrix
predinv <- MASS::ginv(predm)
# Regression coefficients with response equal to arimav
coeff <- drop(predinv %*% arimav)
all.equal(arfit$coef, coeff, check.attributes=FALSE)
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
# Simulate AR process using HighFreq::sim_ar()
nrows <- 1e2
coeff <- matrix(c(0.1, 0.39, 0.5)); ncoeff <- NROW(coeff)
set.seed(1121); innov <- matrix(rnorm(nrows))
arimav <- HighFreq::sim_ar(coeff=coeff, innov=innov)
# Forecast AR process using loop in R
fcast <- numeric(nrows+1)
fcast[2] <- coeff[1]*arimav[1]
fcast[3] <- coeff[1]*arimav[2] + coeff[2]*arimav[1]
for (it in 3:nrows) {
  fcast[it+1] <- arimav[it:(it-2)] %*% coeff
}  # end for
# Plot with legend
plot(arimav, main="Forecasting Using AR(3) Model",
  xlab="", ylab="", type="l")
lines(fcast[-(nrows+1)], col="red", lwd=2)
legend(x="topright", legend=c("series", "forecasts"),
 col=c("black", "red"), lty=1, lwd=6,
 cex=0.9, bg="white", bty="n")
# Forecast using filter()
convf <- filter(x=arimav, sides=1, filter=coeff, method="convolution")
convf <- as.numeric(convf)
# Compare excluding warmup period
all.equal(fcast[-(1:ncoeff)], convf[-(1:(ncoeff-1))],
check.attributes=FALSE)
# Filter using C_cfilter() compiled C++ function directly
convf <- .Call(stats:::C_cfilter, arimav, filter=coeff,
               sides=1, circular=FALSE)
# Compare excluding warmup period
all.equal(fcast[-(1:ncoeff)], convf[-(1:(ncoeff-1))],
check.attributes=FALSE)
# Filter using HighFreq::roll_conv() Rcpp function
convf <- HighFreq::roll_conv(arimav, coeff)
# Compare excluding warmup period
all.equal(fcast[-(1:ncoeff)], convf[-(1:(ncoeff-1))],
check.attributes=FALSE)
# Define predictor matrix for forecasting
predm <- sapply(0:(ncoeff-1), function(lagg) {
  rutils::lagit(arimav, lagg=lagg)
})  # end sapply
# Forecast using predictor matrix
convf <- c(0, drop(predm %*% coeff))
# Compare with loop in R
all.equal(fcast, convf, check.attributes=FALSE)
# Fit ARIMA model using arima()
arfit <- arima(arimav, order=c(3,0,0), include.mean=FALSE)
arfit$coef
coeff
# One-step-ahead forecast using predict.Arima()
predm <- predict(arfit, n.ahead=1)
# Or directly call predict.Arima()
# predm <- predict.Arima(arfit, n.ahead=1)
# Inspect the prediction object
class(predm)
names(predm)
class(predm$pred)
unlist(predm)
# One-step-ahead forecast using matrix algebra
fcast1 <- drop(arimav[nrows:(nrows-2)] %*% arfit$coef)
# Compare one-step-ahead forecasts
all.equal(predm$pred[[1]], fcast1)
# Get information about predict.Arima()
?stats:::predict.Arima
# Calculate the volatilities
sd(arimav); sd(fcast)
# Calculate the in-sample forecasting residuals
resids <- (arimav - fcast[-NROW(fcast)])
# Compare residuals with innovations
all.equal(innov, resids, check.attributes=FALSE)
plot(resids, t="l", lwd=3, xlab="", ylab="",
     main="ARIMA Forecast Errors")
# Simulate AR process using filter()
nrows <- 1e2
coeff <- c(0.1, 0.39, 0.5); ncoeff <- NROW(coeff)
set.seed(1121)
arimav <- filter(x=rnorm(nrows), filter=coeff, method="recursive")
arimav <- as.numeric(arimav)
# Forecast AR(3) process
fcast <- numeric(NROW(arimav))
fcast[2] <- coeff[1]*arimav[1]
fcast[3] <- coeff[1]*arimav[2] + coeff[2]*arimav[1]
for (it in 4:NROW(fcast)) {
  fcast[it] <- arimav[(it-1):(it-3)] %*% coeff
}  # end for
# Forecast using filter()
fcastf <- filter(x=arimav, sides=1,
  filter=coeff, method="convolution")
class(fcastf)
all.equal(fcast[-(1:4)],
  fcastf[-c(1:3, NROW(fcastf))],
  check.attributes=FALSE)
# Compare residuals with innovations
resids <- (arimav-fcast)
tail(cbind(innov, resids))
# arimav <- as.numeric(lh)
# nrows <- NROW(arimav)
# Compare one-step-ahead forecasts
# arfit <- arima(arimav, order=c(3,0,0), method="ML", include.mean=FALSE)
# Compare many one-step-ahead forecasts
fcast <- sapply(31:nrows, function(x) {
  cat("len = ", x, "\n")
  # arimav <- filter(x=rnorm(nrows+1), filter=coeff, method="recursive")
  arfit <- arima(arimav[1:x], order=c(3,0,0), include.mean=FALSE)
  predm <- predict(arfit, n.ahead=1)
  fcast <- drop(arimav[x:(x-2)] %*% arfit$coef)
  c(actual=arimav[x+1], forecast=fcast, predict=as.numeric(predm$pred))
})  # end sapply
foo <- t(foo)
# hist(foo[, 1], breaks=30,
#   main="", ylim=c(0, 60), xlim=c(-0.04, 0.04),
#   xlab="", ylab="", freq=FALSE)
hist(foo[, 1], ylim=c(0, 0.15), freq=FALSE)
lines(density(foo[, 1]), col='blue', lwd=3)
lines(density(foo[, 2]), col='green', lwd=3)
lines(density(foo[, 3]), col='red', lwd=3)
# Forecast AR(3) process
fcast <- numeric(NROW(arimav))
fcast[2] <- coeff[1]*arimav[1]
fcast[3] <- coeff[1]*arimav[2] + coeff[2]*arimav[1]
for (it in 4:NROW(fcast)) {
  fcast[it] <- arimav[(it-1):(it-3)] %*% coeff
}  # end for
# Forecast using filter()
fcastf <- filter(x=arimav, sides=1,
  filter=coeff, method="convolution")
class(fcastf)
all.equal(fcast[-(1:4)],
  fcastf[-c(1:3, NROW(fcastf))],
  check.attributes=FALSE)
# Compare residuals with innovations
resids <- (arimav-fcast)
tail(cbind(innov, resids))
# Plot with legend
plot(arimav, main="Forecasting Using AR(3) Model",
  xlab="", ylab="", type="l")
lines(fcast, col="orange", lwd=3)
legend(x="topright", legend=c("series", "forecasts"),
 col=c("black", "orange"), lty=1, lwd=6,
 cex=0.9, bg="white", bty="n")
# Define AR process parameters
nrows <- 1e3
coeff <- matrix(c(0.5, 0.0, 0.0)); ncoeff <- NROW(coeff)
set.seed(1121); innov <- matrix(rnorm(nrows, sd=0.01))
# Simulate AR process using HighFreq::sim_ar()
arimav <- HighFreq::sim_ar(coeff=coeff, innov=innov)
# Define order of the AR(n) forecasting model
ordern <- 5
# Define predictor matrix for forecasting
predm <- sapply(1:ordern, rutils::lagit, input=arimav)
colnames(predm) <- paste0("pred", 1:NCOL(predm))
# Specify length of look-back interval
look_back <- 100
# Invert the predictor matrix
rangev <- (nrows-look_back):(nrows-1)
predinv <- MASS::ginv(predm[rangev, ])
# Calculate fitted coefficients
coeff <- drop(predinv %*% arimav[rangev])
# Calculate forecast
drop(predm[nrows, ] %*% coeff)
# Actual value
arimav[nrows]
# Calculate a vector of daily VTI log returns
retp <- zoo::coredata(na.omit(rutils::etfenv$returns$VTI))
datev <- zoo::index(retp)
retp <- as.numeric(retp)
nrows <- NROW(retp)
# Define response equal to the returns
respv <- retp
# Define predictor matrix for forecasting
maxorder <- 5
predm <- sapply(1:maxorder, rutils::lagit, input=retp)
predm <- cbind(rep(1, nrows), predm)
# Perform rolling forecasting
look_back <- 100
fcast <- sapply((look_back+1):nrows, function(endd) {
  # Define rolling look-back range
  startp <- max(1, endd-look_back)
  # Or expanding look-back range
  # startp <- 1
  rangev <- startp:(endd-1)
  # Invert the predictor matrix
  predinv <- MASS::ginv(predm[rangev, ])
  # Calculate fitted coefficients
  coeff <- drop(predinv %*% respv[rangev])
  # Calculate forecast
  drop(predm[endd, ] %*% coeff)
})  # end sapply
# Add warmup period
fcast <- c(rep(0, look_back), fcast)
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
# Define backtesting function
sim_fcasts <- function(respv, nagg=5, ordern=5,
                 look_back=100, rollp=TRUE) {
  nrows <- NROW(respv)
  # Define predictor as a rolling sum
  predm <- rutils::roll_sum(respv, look_back=nagg)
  # Define predictor matrix for forecasting
  predm <- sapply(1+nagg*(0:ordern), rutils::lagit, input=predm)
  predm <- cbind(rep(1, nrows), predm)
  # Perform rolling forecasting
  fcast <- sapply((look_back+1):nrows, function(endd) {
    # Define rolling look-back range
    if (rollp)
startp <- max(1, endd-look_back)
    else
    # Or expanding look-back range
startp <- 1
    rangev <- startp:(endd-1)
    # Invert the predictor matrix
    predinv <- MASS::ginv(predm[rangev, ])
    # Calculate fitted coefficients
    coeff <- drop(predinv %*% respv[rangev])
    # Calculate forecast
    drop(predm[endd, ] %*% coeff)
  })  # end sapply
  # Add warmup period
  fcast <- c(rep(0, look_back), fcast)
  # Aggregate the forecasts
  rutils::roll_sum(fcast, look_back=nagg)
}  # end sim_fcasts
# Simulate the rolling autoregressive forecasts
fcast <- sim_fcasts(respv=retp, ordern=5, look_back=100)
c(mse=mean((retp - fcast)^2), cor=cor(retp, fcast))
look_backs <- seq(20, 200, 20)
fcast <- sapply(look_backs, sim_fcasts, respv=retp,
               nagg=5, ordern=5)
colnames(fcast) <- look_backs
msev <- apply(fcast, 2, function(x) mean((retp - x)^2))
# Plot forecasting series with legend
plot(x=look_backs, y=msev,
  xlab="look-back", ylab="MSE", type="l", lwd=2,
  main="MSE of AR(5) Forecasting Model")
