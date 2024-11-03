getOption("repos")  # get default package source
.libPaths()  # get package save directory
install.packages("AER")  # install "AER" from CRAN
# install "PerformanceAnalytics" from R-Forge
install.packages(
  pkgs="PerformanceAnalytics",  # name
  lib="C:/Users/Jerzy/Downloads",  # directory
  repos="http://R-Forge.R-project.org")  # source
# install devtools from CRAN
install.packages("devtools")
# load devtools
library(devtools)
# install package "babynamev" from GitHub
install_github(repo="hadley/babynamev")
# install package "PortfolioAnalytics" from source
install.packages("PortfolioAnalytics",
  type="source",
  repos="http://r-forge.r-project.org")
# download files for package "PortfolioAnalytics"
download.packages(pkgs = "PortfolioAnalytics",
  destdir = ".",  # download to cwd
  type = "source",
  repos="http://r-forge.r-project.org")
# install "PortfolioAnalytics" from local tar source
install.packages(
  "C:/Users/Jerzy/Downloads/PortfolioAnalytics_0.9.3598.tar.gz",
  repos=NULL, type="source")
getOption("defaultPackages")
# matrix of installed package information
packinfo <- installed.packages()
dim(packinfo)
# get all installed package names
sort(unname(packinfo[, "Package"]))
# get a few package names and their versions
packinfo[sample(x=1:100, 5), c("Package", "Version")]
# get info for package "xts"
t(packinfo["xts", ])
# list directories in "PortfolioAnalytics" sub-directory
gsub(
  "C:/Users/Jerzy/Documents/R/win-library/3.1",
  "~",
  list.dirs(
    file.path(
      .libPaths()[1],
      "PortfolioAnalytics")))
# load package, produce error if can't be loaded
library(MASS)
# load package, return TRUE if loaded successfully
require(MASS)
# load quietly
library(MASS, quietly=TRUE)
# load without any messages
suppressMessages(library(MASS))
# remove package from search path
detach(MASS)
# install package if it can't be loaded successfully
if (!require("xts")) install.packages("xts")
# calculate VTI volume-weighted average price
vwapv <- TTR::VWAP(
  price=quantmod::Cl(rutils::etfenv$VTI),
  volume=quantmod::Vo(rutils::etfenv$VTI), n=10)
library()  # list all packages installed on the system
search()  # list all loaded packages on search path
# get documentation for package "Ecdat"
packageDescription("Ecdat")  # get short description
help(package="Ecdat")  # load help page
library(Ecdat)  # load package "Ecdat"
data(package="Ecdat")  # list all datasets in "Ecdat"
ls("package:Ecdat")  # list all objects in "Ecdat"
browseVignettes("Ecdat")  # view package vignette
detach("package:Ecdat")  # remove Ecdat from search path
library(Ecdat)  # load econometric data sets
class(Garch)  # Garch is a data frame from "Ecdat"
dim(Garch)  # daily currency prices
head(Garch[, -2])  # col 'dm' is Deutsch Mark
detach("package:Ecdat")  # remove Ecdat from search path
rm(list=ls())
search()  # get search path for R objects
library(MASS)  # load package "MASS"
head(ls("package:MASS"))  # list some objects in "MASS"
detach("package:MASS")  # remove "MASS" from search path
loadedNamespaces()  # get names of loaded namespaces
search()  # get search path for R objects
# get session info,
# including packages not attached to the search path
sessionInfo()
plot.xts  # package xts isn't loaded and attached
head(xts::plot.xts, 3)
methods("cbind")  # get all methods for function "cbind"
stats::cbind.ts  # cbind isn't exported from package stats
stats:::cbind.ts  # view the non-visible function
getAnywhere("cbind.ts")
library(MASS)  # load package 'MASS'
select  # code of primitive function from package 'MASS'
getAnywhere("cbind.ts")
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
plotheme <- chart_theme()
class(plotheme)
ls(plotheme)
library(quantmod)
ohlc <- rutils::etfenv$VTI["2010-04/2010-05"]
# Extract, modify theme, format tick marks "%b %d"
plotheme <- chart_theme()
plotheme$format.labels <- "%b %d"
# Create plot object
chobj <- chart_Series(x=ohlc, theme=plotheme, plot=FALSE)
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
mtext(colv[1], cex=1.2, lwd=3, side=2, las=2, adj=(-0.5), padj=(-5), col="orange")
mtext(colv[2], cex=1.2, lwd=3, side=4, las=2, adj=1.5, padj=(-5), col="blue")
# Add title and legend
title(main=paste(colv, collapse=" and "), line=0.5)
legend("top", legend=colv, cex=1.0, bg="white",
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
indic <- (quantmod::Cl(datav) > datav[, "VWAP"])
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
pricev <- cbind(quantmod::Cl(rutils::etfenv$VTI), quantmod::Cl(rutils::etfenv$IEF))
pricev <- na.omit(pricev)
colv <- rutils::get_name(colnames(pricev))
colnames(pricev) <- colv
# dygraphs plot with two y-axes
library(dygraphs)
dygraphs::dygraph(pricev, main=paste(colv, collapse=" and ")) %>%
  dyAxis(name="y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis(name="y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", strokeWidth=2, col="red") %>%
  dySeries(name=colv[2], axis="y2", strokeWidth=2, col="blue")
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
