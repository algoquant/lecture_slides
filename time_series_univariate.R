






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
dates <- time(stxts_adj)
e_nd <- dates[NROW(dates)]
st_art <- round((4*e_nd + dates[1])/5)
# Plot using plotOHLC
plotOHLC(window(stxts_adj,
          start=st_art,
          end=e_nd)[, 1:4],
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
x11(width=6, height=5)
par(mar=c(2, 2, 2, 2), oma=c(1, 1, 1, 1))
# Plot first time series without x-axis
zoo::plot.zoo(prices[, 1], lwd=2, col="orange",
        xlab=NA, ylab=NA, xaxt="n")
# Create X-axis date labels and add X-axis
dates <- pretty(zoo::index(prices))
axis(side=1, at=dates, labels=format(dates, "%b-%d-%y"))
# Plot second time series without y-axis
par(new=TRUE)  # Allow new line on same plot
zoo::plot.zoo(prices[, 2], xlab=NA, ylab=NA,
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
dates <- zoo::index(indic)
indic <- ifelse(drop(coredata(indic)), "lightgreen", "antiquewhite")
# Create dygraph object without rendering it
dyplot <- dygraphs::dygraph(datav) %>% dyCandlestick() %>%
  dyOptions(colors="red", strokeWidth=3)
# Add shading
for (i in 1:(NROW(indic)-1)) {
    dyplot <- dyplot %>%
dyShading(from=dates[i], to=dates[i+1], color=indic[i])
}  # end for
# Render the dygraph object
dyplot

library(dygraphs)
# Prepare VTI and IEF prices
prices <- cbind(Cl(rutils::etfenv$VTI), Cl(rutils::etfenv$IEF))
prices <- na.omit(prices)
colnamev <- rutils::get_name(colnames(prices))
colnames(prices) <- colnamev
# dygraphs plot with two y-axes
library(dygraphs)
dygraphs::dygraph(prices, main=paste(colnamev, collapse=" and ")) %>%
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
cutoff <- confl/nrows
datav <- sort(datav)
datav[cutoff]  # Naive Monte Carlo value
quantile(datav, probs=confl)
# Analyze the source code of quantile()
stats:::quantile.default
# Microbenchmark quantile
library(microbenchmark)
summary(microbenchmark(
  monte_carlo = datav[cutoff],
  quantilev = quantile(datav, probs=confl),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary

set.seed(1121)  # Reset random number generator
barp <- 20  # Barrier level
nrows <- 1000  # Number of simulation steps
paths <- numeric(nrows)  # Allocate path vector
paths[1] <- 0  # Initialize path
indeks <- 2  # Initialize simulation index
while ((indeks <= nrows) && (paths[indeks - 1] < barp)) {
# Simulate next step
  paths[indeks] <- paths[indeks - 1] + rnorm(1)
  indeks <- indeks + 1  # Advance indeks
}  # end while
# Fill remaining paths after it crosses barp
if (indeks <= nrows)
  paths[indeks*nrows] <- paths[indeks - 1]
# Plot the Brownian motion
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(1, 1, 1, 1))
plot(paths, type="l", col="black",
     lty="solid", lwd=2, xlab="", ylab="")
abline(h=barp, lwd=3, col="red")
title(main="Brownian Motion Crossing a Barrier Level", line=0.5)

set.seed(1121)  # Reset random number generator
barp <- 20  # Barrier level
nrows <- 1000  # Number of simulation steps
# Simulate path of Brownian motion
paths <- cumsum(rnorm(nrows))
# Find index when paths crosses barp
crossp <- which(paths > barp)
# Fill remaining paths after it crosses barp
if (NROW(crossp)>0) {
  paths[(crossp[1]+1)/nrows] <- paths[crossp[1]]
}  # end if
# Plot the Brownian motion
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(1, 1, 1, 1))
plot(paths, type="l", col="black",
     lty="solid", lwd=2, xlab="", ylab="")
abline(h=barp, lwd=3, col="red")
title(main="Brownian Motion Crossing a Barrier Level", line=0.5)

# Define daily volatility and growth rate
sigmav <- 0.01; drift <- 0.0; nrows <- 1000
# Simulate geometric Brownian motion
returns <- sigmav*rnorm(nrows) + drift - sigmav^2/2
prices <- exp(cumsum(returns))
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
colors <- c("black", "red", "blue")
# Plot all curves
for (indeks in 1:NROW(sigmavs)) {
  curve(expr=dlnorm(x, sdlog=sigmavs[indeks]),
  type="l", lwd=2, xlim=c(0, 3),
  xlab="", ylab="", col=colors[indeks],
  add=as.logical(indeks-1))
}  # end for

# Add title and legend
title(main="Log-normal Distributions", line=0.5)
legend("topright", inset=0.05, title="Sigmas",
 paste("sigma", sigmavs, sep="="),
 cex=0.8, lwd=2, lty=rep(1, NROW(sigmavs)),
 col=colors)

x11(width=6, height=5)
par(mar=c(4, 4, 3, 1))
# Return volatility of VTI etf
sigmav <- sd(rutils::diffit(log(rutils::etfenv$VTI[, 4])))
sigma2 <- sigmav^2
nrows <- NROW(rutils::etfenv$VTI)
# Standard deviation of log-normal prices
sqrt(nrows)*sigmav

# Skewness of log-normal prices
skew_ness <- function(t) {
  ex_p <- exp(t*sigma2)
  (ex_p + 2)*sqrt(ex_p - 1)
}  # end skew_ness
curve(expr=skew_ness, xlim=c(1, nrows), lwd=3,
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
paths <- 10
# Simulate multiple paths of geometric Brownian motion
prices <- matrix(rnorm(paths*nrows, sd=sigmav) +
    drift - sigmav^2/2, nc=paths)
prices <- exp(matrixStats::colCumsums(prices))
# Create xts time series
prices <- xts(prices, order.by=seq.Date(Sys.Date()-NROW(prices)+1, Sys.Date(), by=1))
# Plot xts time series
colors <- colorRampPalette(c("red", "blue"))(NCOL(prices))
colors <- colors[order(order(prices[NROW(prices), ]))]
par(mar=c(3, 3, 2, 2), oma=c(0, 0, 0, 0))
plot.zoo(prices, main="Multiple paths of geometric Brownian motion",
   xlab=NA, ylab=NA, plot.type="single", col=colors)

# Define daily volatility and growth rate
sigmav <- 0.01; drift <- 0.0; nrows <- 10000
paths <- 100
# Simulate multiple paths of geometric Brownian motion
prices <- matrix(rnorm(paths*nrows, sd=sigmav) +
    drift - sigmav^2/2, nc=paths)
prices <- exp(matrixStats::colCumsums(prices))
# Calculate fraction of paths below the expected value
fractv <- rowSums(prices < 1.0) / paths
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
# Normalize columns
prices <- xts(t(t(prices) / as.numeric(prices[1, ])),
         order.by=zoo::index(prices))
# Calculate permution index for sorting the lowest to highest final prices
ordern <- order(prices[NROW(prices), ])
# Select a few symbols
symbolv <- colnames(prices)[ordern]
symbolv <- symbolv[seq.int(from=1, to=(NROW(symbolv)-1), length.out=20)]

# Plot xts time series of prices
colors <- colorRampPalette(c("red", "blue"))(NROW(symbolv))
colors <- colors[order(order(prices[NROW(prices), symbolv]))]
par(mar=c(3, 3, 2, 2), oma=c(0, 0, 0, 0))
plot.zoo(prices[, symbolv], main="20 S&P500 stock prices (normalized)",
   xlab=NA, ylab=NA, plot.type="single", col=colors)
legend(x="topleft", inset=0.05, cex=0.8,
 legend=rev(symbolv), col=rev(colors), lwd=6, lty=1)

# Calculate average of valid stock prices
validp <- (prices != 1)  # Valid stocks
nums <- rowSums(validp)
nums[1] <- NCOL(prices)
indeks <- rowSums(prices * validp) / nums
# Calculate fraction of stock prices below the average price
fractv <- rowSums((prices < indeks) & validp) / nums
# Create xts time series of average stock prices
indeks <- xts(indeks, order.by=zoo::index(prices))

x11(width=6, height=4)
par(mar=c(3, 3, 2, 2), oma=c(0, 0, 0, 0))
# Plot xts time series of average stock prices
plot.zoo(indeks, main="Average S&P500 stock prices (normalized from 1990)",
   xlab=NA, ylab=NA, col="blue")
# Create xts time series of percentage of stock prices below the average price
fractv <- xts(fractv, order.by=zoo::index(prices))
# Plot percentage of stock prices below the average price
plot.zoo(fractv[-(1:2),],
   main="Percentage of S&P500 stock prices below the average price",
   xlab=NA, ylab=NA, col="blue")

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
prices[1] <- init_price
# Simulate Ornstein-Uhlenbeck process in R
for (i in 2:nrows) {
  returns[i] <- thetav*(eq_price - prices[i-1]) +
    sigmav*innov[i]
  prices[i] <- prices[i-1] + returns[i]
}  # end for
# Simulate Ornstein-Uhlenbeck process in Rcpp
prices_cpp <- HighFreq::sim_ou(init_price=init_price, eq_price=eq_price,
  volat=sigmav, theta=thetav, innov=matrix(innov))
all.equal(prices, drop(prices_cpp))
# Compare the speed of R code with Rcpp
library(microbenchmark)
summary(microbenchmark(
  Rcode={for (i in 2:nrows) {
    returns[i] <- thetav*(eq_price - prices[i-1]) + sigmav*innov[i]
    prices[i] <- prices[i-1] + returns[i]}},
  Rcpp=HighFreq::sim_ou(init_price=init_price, eq_price=eq_price,
    volat=sigmav, theta=thetav, innov=matrix(innov)),
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
l_m <- lm(formulav)
summary(l_m)
# Plot regression
plot(formulav, main="OU Returns Versus Lagged Prices")
abline(l_m, lwd=2, col="red")

# Calculate volatility parameter
c(volatility=sigmav, estimate=sd(returns))
# Extract OU parameters from regression
coeff <- summary(l_m)$coefficients
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

x11(width=6, height=5)
par(mar=c(3, 2, 1, 1), oma=c(1, 0, 0, 0))
returns <- na.omit(rutils::etfenv$returns$VTI)
# Plot autocorrelations using stats::acf()
stats::acf(returns, lag=10, xlab="lag", main="")
title(main="ACF of VTI Returns", line=-1)
# Two-tailed 95% confidence interval
qnorm(0.975)/sqrt(NROW(returns))

# Calculate VTI and XLF percentage returns
returns <- rutils::etfenv$returns[, c("VTI", "XLF")]
returns <- na.omit(returns)
nrows <- NROW(returns)
# De-mean (center) and scale the returns
returns <- apply(returns, MARGIN=2,
  function(x) (x-mean(x))/sd(x))
apply(returns, MARGIN=2, sd)
# Calculate the correlation
drop(returns[, "VTI"] %*% returns[, "XLF"])/(nrows-1)
co_r <- cor(returns[, "VTI"], returns[, "XLF"])
# Test statistical significance of correlation
cor_test <- cor.test(returns[, "VTI"], returns[, "XLF"])
conf_int <- qnorm((1+0.95)/2)/sqrt(nrows)
co_r*c(1-conf_int, 1+conf_int)

# Get source code
stats:::cor.test.default

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
    ci <- qnorm((1+0.95)/2)*sqrt(1/NROW(xtes))
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
arimav <- numeric(NROW(innov))
arimav[1] <- innov[1]
arimav[2] <- coeff[1]*arimav[1] + innov[2]
arimav[3] <- coeff[1]*arimav[2] + coeff[2]*arimav[1] + innov[3]
for (it in 4:NROW(arimav)) {
  arimav[it] <- arimav[(it-1):(it-3)] %*% coeff + innov[it]
}  # End for
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

# Define Dickey-Fuller parameters
init_price <- 0.0; eq_price <- 1.0;
sigmav <- 0.02; thetav <- 0.01; nrows <- 1000
# Initialize the data
innov <- rnorm(nrows)
returns <- numeric(nrows)
prices <- numeric(nrows)
# Simulate Dickey-Fuller process in R
prices[1] <- sigmav*innov[1]
for (i in 2:nrows) {
  returns[i] <- thetav*(eq_price - prices[i-1]) + sigmav*innov[i]
  prices[i] <- prices[i-1] + returns[i]
}  # end for
# Simulate Dickey-Fuller process in Rcpp
prices_cpp <- HighFreq::sim_ou(init_price=init_price, eq_price=eq_price,
  volat=sigmav, theta=thetav, innov=matrix(innov))
all.equal(prices, drop(prices_cpp))
# Compare the speed of R code with Rcpp
library(microbenchmark)
summary(microbenchmark(
  Rcode={for (i in 2:nrows) {
    returns[i] <- thetav*(eq_price - prices[i-1]) + sigmav*innov[i]
    prices[i] <- prices[i-1] + returns[i]}},
  Rcpp=HighFreq::sim_ou(eq_price=eq_price, volat=sigmav, theta=thetav, innov=matrix(innov)),
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
  volat=1.0, theta=thetav, innov=innov)
x11(); plot(prices, t="l", main=paste("OU coefficient =", thetav))
tseries::adf.test(prices, k=1)
# Simulate Ornstein-Uhlenbeck OU process with zero reversion
thetav <- 0.0
prices <- HighFreq::sim_ou(init_price=init_price, eq_price=eq_price,
  volat=1.0, theta=thetav, innov=innov)
x11(); plot(prices, t="l", main=paste("OU coefficient =", thetav))
tseries::adf.test(prices, k=1)

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
coeffs <- seq(0.99, 0.999, 0.001)
returns <- as.numeric(na.omit(rutils::etfenv$returns$VTI))
adf_test <- sapply(coeffs, function(coeff) {
  arimav <- filter(x=returns, filter=coeff, method="recursive")
  ad_f <- suppressWarnings(tseries::adf.test(arimav))
  c(adfstat=unname(ad_f$statistic), pval=ad_f$p.value)
})  # end sapply
x11(width=6, height=5)
par(mar=c(4, 4, 2, 2), oma=c(0, 0, 0, 0), mgp=c(2.5, 1, 0))
plot(x=coeffs, y=adf_test["pval", ], main="ADF p-val Versus AR Coefficient",
     xlab="AR coefficient", ylab="ADF pval", t="l", col="blue", lwd=2)
plot(x=coeffs, y=adf_test["adfstat", ], main="ADF Stat Versus AR Coefficient",
     xlab="AR coefficient", ylab="ADF stat", t="l", col="blue", lwd=2)

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
designinv <- MASS::ginv(design)
coeff <- drop(designinv %*% arimav)
all.equal(drop(arfit$ar), coeff, check.attributes=FALSE)

# Specify AR process parameters
nrows <- 1e3
coeff <- c(0.1, 0.39, 0.5); n_coeff <- NROW(coeff)
# Simulate AR process using C_rfilter()
set.seed(1121); innov <- rnorm(nrows)
arimav <- .Call(stats:::C_rfilter, innov, coeff,
  double(nrows + n_coeff))[-(1:n_coeff)]


# wippp
# Calibrate ARIMA model using regression
# Define design matrix
arimav <- (arimav - mean(arimav))
design <- sapply(1:3, rutils::lagit, input=arimav)
# Calculate de-meaned returns matrix
design <- t(t(design) - colMeans(design))
designinv <- MASS::ginv(design)
# Regression coefficients with response equal to arimav
coeff <- drop(designinv %*% arimav)

all.equal(arima_fit$coef, coeff, check.attributes=FALSE)

# Calculate the regression residuals
fittedv <- drop(design %*% coeff)
residuals <- drop(arimav - fittedv)
# Variance of residuals
var_resid <- sum(residuals^2)/(nrows-NROW(coeff))
# Design matrix squared
design2 <- crossprod(design)
# Calculate covariance matrix of AR coefficients
covar <- var_resid*MASS::ginv(design2)
coeffd <- sqrt(diag(covar))
# Calculate t-values of AR coefficients
coeff_tvals <- drop(coeff)/coeffd

# Fit AR(5) model into AR(3) process
design <- sapply(1:5, rutils::lagit, input=arimav)
designinv <- MASS::ginv(design)
coeff <- drop(designinv %*% arimav)
# Calculate t-values of AR(5) coefficients
residuals <- drop(arimav - drop(design %*% coeff))
var_resid <- sum(residuals^2)/(nrows-NROW(coeff))
covar <- var_resid*MASS::ginv(crossprod(design))
coeffd <- sqrt(diag(covar))
coeff_tvals <- drop(coeff)/coeffd
# Fit AR(5) model using arima()
arima_fit <- arima(arimav, order=c(5, 0, 0), include.mean=FALSE)
arima_fit$coef
# Fit AR(5) model using auto.arima()
library(forecast)  # Load forecast
arima_fit <- forecast::auto.arima(arimav, max.p=5, max.q=0, max.d=0)
# Fit AR(5) model into VTI returns
returns <- drop(zoo::coredata(na.omit(rutils::etfenv$returns$VTI)))
design <- sapply(1:5, rutils::lagit, input=returns)
designinv <- MASS::ginv(design)
coeff <- drop(designinv %*% returns)
# Calculate t-values of AR(5) coefficients
residuals <- drop(returns - drop(design %*% coeff))
var_resid <- sum(residuals^2)/(nrows-NROW(coeff))
covar <- var_resid*MASS::ginv(crossprod(design))
coeffd <- sqrt(diag(covar))
coeff_tvals <- drop(coeff)/coeffd

# Calibrate ARIMA model using auto.arima()
# library(forecast)  # Load forecast
forecast::auto.arima(arimav, max.p=3, max.q=0, max.d=0)
# Calibrate ARIMA model using arima()
arima_fit <- arima(arimav, order=c(3,0,0), include.mean=FALSE)
arima_fit$coef
# Calibrate ARIMA model using auto.arima()
# library(forecast)  # Load forecast
forecast::auto.arima(arimav, max.p=3, max.q=0, max.d=0)
# Calibrate ARIMA model using regression
arimav <- as.numeric(arimav)
# Define design matrix for arimav
design <- sapply(1:3, rutils::lagit, input=arimav)
# Generalized inverse of design matrix
designinv <- MASS::ginv(design)
# Regression coefficients with response equal to arimav
coeff <- drop(designinv %*% arimav)
all.equal(arima_fit$coef, coeff, check.attributes=FALSE)

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
lines(forecasts, col="red", lwd=2)
legend(x="topright", legend=c("series", "forecasts"),
 col=c("black", "red"), lty=1, lwd=6,
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

# Simulate AR process using filter()
nrows <- 1e2
coeff <- c(0.1, 0.39, 0.5); n_coeff <- NROW(coeff)
set.seed(1121)
arimav <- filter(x=rnorm(nrows), filter=coeff, method="recursive")
arimav <- as.numeric(arimav)
# Forecast AR(3) process
forecasts <- numeric(NROW(arimav))
forecasts[2] <- coeff[1]*arimav[1]
forecasts[3] <- coeff[1]*arimav[2] + coeff[2]*arimav[1]
for (it in 4:NROW(forecasts)) {
  forecasts[it] <- arimav[(it-1):(it-3)] %*% coeff
}  # end for
# Forecast using filter()
forecasts_filter <- filter(x=arimav, sides=1,
  filter=coeff, method="convolution")
class(forecasts_filter)
all.equal(forecasts[-(1:4)],
  forecasts_filter[-c(1:3, NROW(forecasts_filter))],
  check.attributes=FALSE)
# Compare residuals with innovations
residuals <- (arimav-forecasts)
tail(cbind(innov, residuals))


# arimav <- as.numeric(lh)
# nrows <- NROW(arimav)
# Compare one-step-ahead forecasts
# arima_fit <- arima(arimav, order=c(3,0,0), method="ML", include.mean=FALSE)

# Compare many one-step-ahead forecasts
forecasts <- sapply(31:nrows, function(x) {
  cat("len = ", x, "\n")
  # arimav <- filter(x=rnorm(nrows+1), filter=coeff, method="recursive")
  arima_fit <- arima(arimav[1:x], order=c(3,0,0), include.mean=FALSE)
  predictv <- predict(arima_fit, n.ahead=1)
  forecastv <- drop(arimav[x:(x-2)] %*% arima_fit$coef)
  c(actual=arimav[x+1], forecast=forecastv, predict=as.numeric(predictv$pred))
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
forecasts <- numeric(NROW(arimav))
forecasts[2] <- coeff[1]*arimav[1]
forecasts[3] <- coeff[1]*arimav[2] + coeff[2]*arimav[1]
for (it in 4:NROW(forecasts)) {
  forecasts[it] <- arimav[(it-1):(it-3)] %*% coeff
}  # end for
# Forecast using filter()
forecasts_filter <- filter(x=arimav, sides=1,
  filter=coeff, method="convolution")
class(forecasts_filter)
all.equal(forecasts[-(1:4)],
  forecasts_filter[-c(1:3, NROW(forecasts_filter))],
  check.attributes=FALSE)
# Compare residuals with innovations
residuals <- (arimav-forecasts)
tail(cbind(innov, residuals))

# Plot with legend
plot(arimav, main="Forecasting Using AR(3) Model",
  xlab="", ylab="", type="l")
lines(forecasts, col="orange", lwd=3)
legend(x="topright", legend=c("series", "forecasts"),
 col=c("black", "orange"), lty=1, lwd=6,
 cex=0.9, bg="white", bty="n")

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
designinv <- MASS::ginv(design[rangev, -1])
# Calculate fitted coefficients
coeff <- drop(designinv %*% design[rangev, 1])
# Calculate forecast
drop(design[nrows, -1] %*% coeff)

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
forecasts <- sapply((look_back+1)/nrows, function(endp) {
  # Define rolling look-back range
  startp <- max(1, endp-look_back)
  # Or expanding look-back range
  # startp <- 1
  rangev <- startp:(endp-1)
  # Invert the predictor matrix
  designinv <- MASS::ginv(predictor[rangev, ])
  # Calculate fitted coefficients
  coeff <- drop(designinv %*% response[rangev])
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
  forecasts <- sapply((look_back+1)/nrows, function(endp) {
    # Define rolling look-back range
    startp <- max(1, endp-look_back)
    # Or expanding look-back range
    # startp <- 1
    rangev <- startp:(endp-1)
    # Invert the predictor matrix
    designinv <- MASS::ginv(predictor[rangev, ])
    # Calculate fitted coefficients
    coeff <- drop(designinv %*% response[rangev])
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
