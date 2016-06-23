library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
options(digits=3)
thm <- knit_theme$get("acid")
knit_theme$set(thm)
# get documentation for package tseries
packageDescription("tseries")  # get short description

help(package="tseries")  # load help page

library(tseries)  # load package tseries

data(package="tseries")  # list all datasets in "tseries"

ls("package:tseries")  # list all objects in "tseries"

detach("package:tseries")  # remove tseries from search path
library(tseries)  # load package tseries
# download MSFT data in ts format
ts_stx <- suppressWarnings(
  get.hist.quote(
    instrument="MSFT",
    start=Sys.Date()-3*365,
    end=Sys.Date(),
    retclass="ts",
    quote=c("Open","High","Low","Close",
      "AdjClose","Volume"),
    origin="1970-01-01")
)  # end suppressWarnings
load(file="C:/Develop/data/zoo_data.RData")
# create price adjustment vector
adj_close <- ts_stx[, "AdjClose"] -
  ts_stx[, "Close"]
# adjust OHLC prices
ts_stx_adj <- ts_stx
ts_stx_adj[, c("Open","High","Low","Close")] <-
  ts_stx[, c("Open","High","Low","Close")] + adj_close
# inspect the data
tsp(ts_stx_adj)  # frequency=1
head(time(ts_stx_adj))
head(ts_stx_adj)
tail(ts_stx_adj)
library(tseries)  # load package tseries
# download MSFT data
zoo_stx <- suppressWarnings(
  get.hist.quote(
    instrument="MSFT",
    start=Sys.Date()-3*365,
    end=Sys.Date(),
    quote=c("Open","High","Low","Close",
      "AdjClose","Volume"),
    origin="1970-01-01")
)  # end suppressWarnings
load(file="C:/Develop/data/zoo_data.RData")
class(zoo_stx)
dim(zoo_stx)
head(zoo_stx, 4)
library(tseries)  # load package tseries
load(file="C:/Develop/data/zoo_data.RData")
# create price adjustment vector
adj_close <- zoo_stx[, "AdjClose"] - zoo_stx[, "Close"]
head(adj_close, 5)
tail(adj_close, 5)
# adjust OHLC prices
zoo_stx_adj <- zoo_stx
zoo_stx_adj[, c("Open","High","Low","Close")] <-
  zoo_stx[, c("Open","High","Low","Close")] + adj_close
head(zoo_stx_adj)
tail(zoo_stx_adj)
library(tseries)  # load package tseries
# download EUR/USD data
zoo_eurusd <- suppressWarnings(
  get.hist.quote(
    instrument="EUR/USD",
    provider="oanda",
    start=Sys.Date()-3*365,
    end=Sys.Date(),
    origin="1970-01-01")
)  # end suppressWarnings
# bind and scrub data
zoo_stxeur <- merge(zoo_eurusd,
               zoo_stx[, "AdjClose"])
colnames(zoo_stxeur) <- c("EURUSD", "MSFT")
zoo_stxeur <-
  zoo_stxeur[complete.cases(zoo_stxeur),]
save(zoo_stx, zoo_stx_adj,
     ts_stx, ts_stx_adj,
     zoo_eurusd, zoo_stxeur,
     file="C:/Develop/data/zoo_data.RData")
load(file="C:/Develop/data/zoo_data.RData")
# inspect the data
class(zoo_eurusd)
tail(zoo_eurusd, 4)
library(tseries)  # load package tseries
# create vector of symbol names
sym_bols <- c("VTI", "VEU", "IEF", "VNQ", "DBC", "XLY", "XLP", "XLE", "XLF", "XLV", "XLI", "XLB", "XLK", "XLU", "IWB", "IWD", "IWF", "IWM", "IWN", "IWO", "IWP", "IWR", "IWS", "IWV", "IUSV", "IUSG")
# download price and volume data for sym_bols into list of zoo objects
zoo_series <- suppressWarnings(
  lapply(sym_bols, # loop for loading data
   get.hist.quote,
   quote=c("AdjClose", "Volume"),
   start=Sys.Date()-3650,
   end=Sys.Date(),
   origin="1970-01-01")
)  # end suppressWarnings
# flatten list of zoo objects into a single zoo object
zoo_series <- do.call(merge, zoo_series)
# assign names in format "symbol.Close", "symbol.Volume"
names(zoo_series) <-
  as.vector(sapply(sym_bols,
             paste, c("Close", "Volume"), sep="."))
# save zoo_series to a comma-separated CSV file
write.zoo(zoo_series, file='zoo_series.csv', sep=",")
# save zoo_series to a binary .RData file
save(zoo_series, file='zoo_series.RData')
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
load(file="C:/Develop/data/zoo_data.RData")
# get start and end dates
in_dex <- time(ts_stx_adj)
e_nd <- in_dex[length(in_dex)]
st_art <- round((4*e_nd + in_dex[1])/5)
# plot using plotOHLC
plotOHLC(window(ts_stx_adj,
          start=st_art,
          end=e_nd)[, 1:4],
   xlab="", ylab="")
title(main="MSFT OHLC Prices")
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
load(file="C:/Develop/data/zoo_data.RData")
library(zoo)  # load package zoo
library(lubridate)  # load lubridate
# get start and end dates of zoo_series
start_date <- decimal_date(start(zoo_stx))
end_date <- decimal_date(end(zoo_stx))
# calculate frequency of zoo_stx
fre_quency <- length(zoo_stx)/(end_date-start_date)
# extract data from zoo_stx
da_ta <- coredata(
  window(zoo_stx, start=as.Date("2015-01-01"),
   end=end(zoo_stx)))
# create ts object using ts()
ts_stx <- ts(data=da_ta, start=decimal_date(as.Date("2015-01-01")),
          frequency=fre_quency)
seqplot.ts(x=ts_stx[, 1], y=ts_stx[, 4], xlab="", ylab="")
title(main="MSFT Open and Close Prices", line=-1)
library(tseries)  # load package tseries
library(zoo)  # load package zoo
load(file="C:/Develop/data/zoo_data.RData")
# calculate maximum drawdown
maxdrawdown(zoo_stx_adj[, "AdjClose"])
max_drawd <- maxdrawdown(zoo_stx_adj[, "AdjClose"])
index(zoo_stx_adj)[max_drawd$from]
index(zoo_stx_adj)[max_drawd$to]
# calculate Sharpe ratio
sharpe(zoo_stx_adj[, "AdjClose"])
# calculate Sterling ratio
sterling(as.numeric(zoo_stx_adj[, "AdjClose"]))
library(tseries)  # load package tseries
zoo_stx <- suppressWarnings(  # load MSFT data
  get.hist.quote(instrument="MSFT",
           start=Sys.Date()-365,
           end=Sys.Date(),
           origin="1970-01-01")
)  # end suppressWarnings
class(zoo_stx)
dim(zoo_stx)
tail(zoo_stx, 4)

# calculate Sharpe ratio
sharpe(zoo_stx[, "Close"], r=0.01)
# add title
plot(zoo_stx[, "Close"], xlab="", ylab="")
title(main="MSFT Close Prices", line=-1)
library(tseries)  # load package tseries
zoo_stx <- suppressWarnings(  # load MSFT data
  get.hist.quote(instrument="MSFT",
           start=Sys.Date()-365,
           end=Sys.Date(),
           origin="1970-01-01")
)  # end suppressWarnings
class(zoo_stx)
dim(zoo_stx)
tail(zoo_stx, 4)

# calculate Sharpe ratio
sharpe(zoo_stx[, "Close"], r=0.01)
# add title
plot(zoo_stx[, "Close"], xlab="", ylab="")
title(main="MSFT Close Prices", line=-1)
library(tseries)  # load package tseries
zoo_stx <- suppressWarnings(  # load MSFT data
  get.hist.quote(instrument="MSFT",
           start=Sys.Date()-365,
           end=Sys.Date(),
           origin="1970-01-01")
)  # end suppressWarnings
class(zoo_stx)
dim(zoo_stx)
tail(zoo_stx, 4)

# calculate Sharpe ratio
sharpe(zoo_stx[, "Close"], r=0.01)
# add title
plot(zoo_stx[, "Close"], xlab="", ylab="")
title(main="MSFT Close Prices", line=-1)
# load package quantmod
library(quantmod)
# get documentation for package quantmod
# get short description
packageDescription("quantmod")
# load help page
help(package="quantmod")
# list all datasets in "quantmod"
data(package="quantmod")
# list all objects in "quantmod"
ls("package:quantmod")
# remove quantmod from search path
detach("package:quantmod")
rm(list=ls())
setwd("C:/Develop/data")
library(xtable)
# ETF symbols for asset allocation
sym_bols <- c("VTI", "VEU", "IEF", "VNQ", 
  "DBC", "XLY", "XLP", "XLE", "XLF", "XLV", 
  "XLI", "XLB", "XLK", "XLU", "IWB", "IWD", 
  "IWF", "IWM", "IWN", "IWO", "IWP", "IWR", 
  "IWS", "IWV", "IUSV", "IUSG")
# read etf database into data frame
etf_list <- read.csv(file='etf_list.csv', 
               stringsAsFactors=FALSE)
rownames(etf_list) <- etf_list$Symbol
# subset etf_list only those ETF's in sym_bols
etf_list <- etf_list[sym_bols, ]
# shorten names
etf_names <- sapply(etf_list$Name, 
              function(name) {
  name_split <- strsplit(name, split=" ")[[1]]
  name_split <- 
    name_split[c(-1, -length(name_split))]
  name_match <- match("Select", name_split)
  if (!is.na(name_match))
    name_split <- name_split[-name_match]
  paste(name_split, collapse=" ")
})  # end sapply
etf_list$Name <- etf_names
etf_list["IEF", "Name"] <- "Treasury Bond Fund"
etf_list["XLY", "Name"] <- "Consumer Discr. Sector Fund"
etf_list[c(1, 2)]
print(xtable(etf_list), comment=FALSE, size="tiny")
load(file="C:/Develop/data/etf_data.RData")
library(quantmod)  # load package quantmod
env_data <- new.env()  # new environment for data
# download data for sym_bols into env_data
getSymbols(sym_bols, env=env_data, adjust=TRUE,
    from="2007-01-03", to="2015-05-01")
load(file="C:/Develop/data/etf_data.RData")
library(quantmod)  # load package quantmod
ls(env_data)  # list files in env_data
# get class of object in env_data
class(get(x=sym_bols[1], envir=env_data))
# another way
class(env_data$VTI)
colnames(env_data$VTI)
head(env_data$VTI, 3)
library(quantmod)  # load package quantmod
# create name corresponding to "^GSPC" ticker
setSymbolLookup(
  SP500=list(name="^GSPC", src="yahoo"))
getSymbolLookup()
# view and clear options
options("getSymbols.sources")
options(getSymbols.sources=NULL)
# download S&P500 prices into env_data
getSymbols("SP500", env=env_data,
    adjust=TRUE, from="1990-01-01")
chart_Series(x=env_data$SP500["2016/"],
       TA="add_Vo()",
       name="SP500 index")
library(quantmod)  # load package quantmod
library(RCurl)  # load package RCurl
library(XML)  # load package XML
# extract tables from a URL directly
sp_500 <- readHTMLTable(
  "http://www.cboe.com/products/snp500.aspx",
          stringsAsFactors=FALSE)
# download text data from URL
sp_500 <- getURL(
  "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies")
# extract tables from the text data
sp_500 <- readHTMLTable(sp_500,
              stringsAsFactors=FALSE)
str(sp_500)
# extract colnames of data frames
lapply(sp_500, colnames)
# extract S&P500 constituents
sp_500 <- sp_500[[1]]
head(sp_500)
# write data frame of S&P500 constituents to CSV file
write.csv(sp_500,
  file="C:/Develop/data/SP500_Yahoo.csv",
  row.names=FALSE)
library(quantmod)  # load package quantmod
# load data frame of S&P500 constituents from CSV file
sp_500 <-
  read.csv(file="C:/Develop/data/SP500_Yahoo.csv",
     stringsAsFactors=FALSE)
# find tickers containing "-" character
tick_ers <- grep("-", sp_500$Ticker, value=TRUE)
# create names corresponding to invalid tickers
for (tick_er in tick_ers) {
  cat("processing: ", tick_er, "\n")
  setSymbolLookup(structure(
    list(list(name=tick_er)),
    names=gsub("-", "_", tick_er)))
}  # end for
env_data <- new.env()  # new environment for data
# remove all files (if necessary)
rm(list=ls(env_data), envir=env_data)
# download data and copy it into environment
getSymbols(gsub("-", "_", sp_500$Ticker),
   env=env_data, adjust=TRUE, from="1990-01-01")
# or download in loop
for (tick_er in gsub("-", "_", sp_500$Ticker)) {
  cat("processing: ", tick_er, "\n")
  getSymbols(tick_er, env=env_data,
  adjust=TRUE, from="1990-01-01")
}  # end for
save(env_data, file="C:/Develop/data/sp500.RData")
chart_Series(x=env_data$BRK_B["2016/"], TA="add_Vo()",
       name="BRK-B stock")
load(file="C:/Develop/data/etf_data.RData")
library(quantmod)  # load package quantmod
# check of object is an OHLC time series
is.OHLC(env_data$VTI)
# adjust single OHLC object using its name
env_data$VTI <- adjustOHLC(env_data$VTI,
                     use.Adjusted=TRUE)

# adjust OHLC object using string as name
assign(sym_bols[1], adjustOHLC(
    get(x=sym_bols[1], envir=env_data),
    use.Adjusted=TRUE),
  envir=env_data)

# adjust objects in environment using vector of strings
for (sym_bol in sym_bols) {
  assign(sym_bol,
   adjustOHLC(get(sym_bol, envir=env_data),
              use.Adjusted=TRUE),
   envir=env_data)
}  # end for
load(file="C:/Develop/data/etf_data.RData")
library(quantmod)  # load package quantmod
# extract and merge all data, subset by symbols
etf_series <- do.call(merge,
            as.list(env_data)[sym_bols])

# extract and merge adjusted prices, subset by symbols
etf_series_ad <- do.call(merge,
            lapply(as.list(env_data)[sym_bols], Ad))

# extract and merge adjusted prices, subset by symbols
etf_series_ad <- do.call(merge,
            eapply(env_data, Ad)[sym_bols])

# drop ".Adjusted" from colnames
colnames(etf_series_ad) <-
  sapply(colnames(etf_series_ad),
    function(col_name)
strsplit(col_name, split="[.]")[[1]])[1, ]
tail(etf_series_ad[, 1:2], 3)

# which objects in global environment are class xts?
unlist(eapply(globalenv(), is.xts))

# save xts to csv file
write.zoo(etf_series,
     file='etf_series.csv', sep=",")
# save data to .RData file
save(env_data, etf_series, etf_series_ad,
     file='etf_data.RData')
library(quantmod)
load(file="C:/Develop/data/etf_data.RData")
# remove rows with NA values
etf_series_ad <- 
  etf_series_ad[complete.cases(etf_series_ad)]
colnames(etf_series_ad)

# calculate returns from adjusted prices
etf_rets <- lapply(etf_series_ad, 
             function(x_ts) {
# dailyReturn returns single xts with bad colname
  daily_return <- dailyReturn(x_ts)
  colnames(daily_return) <- names(x_ts)
  daily_return
})  # end lapply

# "etf_rets" is a list of xts
class(etf_rets[[1]])

# flatten list of xts into a single xts
etf_rets <- do.call(merge, etf_rets)
class(etf_rets)
dim(etf_rets)
head(etf_rets[, 1:3])
library(quantmod)
load(file="C:/Develop/data/etf_data.RData")
# plot OHLC candlechart with volume
chartSeries(env_data$VTI["2014-11"],
      name="VTI",
      theme=chartTheme("white"))
# plot OHLC bar chart with volume
chartSeries(env_data$VTI["2014-11"],
      type="bars",
      name="VTI",
      theme=chartTheme("white"))
library(quantmod)
load(file="C:/Develop/data/etf_data.RData")
# plot OHLC candlechart with volume
chartSeries(env_data$VTI["2008-11/2009-04"],
      name="VTI")
# redraw plot only for Feb-2009, with white theme
reChart(subset="2009-02",
  theme=chartTheme("white"))
library(quantmod)
load(file="C:/Develop/data/etf_data.RData")
# candlechart with Bollinger Bands
chartSeries(env_data$VTI["2014"],
      TA="addBBands(): addBBands(draw='percent'): addVo()",
      name="VTI with Bollinger Bands",
      theme=chartTheme("white"))
# candlechart with two Moving Averages
chartSeries(env_data$VTI["2014"],
      TA="addVo(): addEMA(10): addEMA(30)",
      name="VTI with Moving Averages",
      theme=chartTheme("white"))
# candlechart with Commodity Channel Index
chartSeries(env_data$VTI["2014"],
      TA="addVo(): addBBands(): addCCI()",
      name="VTI with Technical Indicators",
      theme=chartTheme("white"))
library(quantmod)
library(TTR)
load(file="C:/Develop/data/etf_data.RData")
da_ta <- env_data$VTI["2009-02/2009-03"]
adj_vti <- Ad(da_ta); vol_vti <- Vo(da_ta)
v_wap <- VWAP(price=adj_vti, volume=vol_vti,
        n=10)
# plot OHLC candlechart with volume
chartSeries(da_ta, name="VTI plus VWAP",
      theme=chartTheme("white"))
# add VWAP to main plot
addTA(ta=v_wap, on=1, col='red')
# add price minus VWAP in extra panel
addTA(ta=(adj_vti-v_wap), col='red')
library(quantmod)
library(TTR)
load(file="C:/Develop/data/etf_data.RData")
da_ta <- env_data$VTI
adj_vti <- Ad(da_ta)
vol_vti <- Vo(da_ta)
v_wap <- VWAP(price=adj_vti, volume=vol_vti, n=10)
adj_vti <- adj_vti["2009-02/2009-03"]
da_ta <- da_ta["2009-02/2009-03"]
v_wap <- v_wap["2009-02/2009-03"]
# plot OHLC candlechart with volume
chartSeries(da_ta, name="VTI plus VWAP shaded",
      theme=chartTheme("white"))
# add VWAP to main plot
addTA(ta=v_wap, on=1, col='red')
# add price minus VWAP in extra panel
addTA(ta=(adj_vti-v_wap), col='red')
# add background shading of areas
addTA((adj_vti-v_wap) > 0, on=-1,
col="lightgreen", border="lightgreen")
addTA((adj_vti-v_wap) < 0, on=-1,
col="lightgrey", border="lightgrey")
# add vertical and horizontal lines at v_wap minimum
addLines(v=which.min(v_wap), col='red')
addLines(h=min(v_wap), col='red')
library(quantmod)
library(TTR)
load(file="C:/Develop/data/etf_data.RData")
da_ta <- env_data$VTI
adj_vti <- Ad(da_ta)
vol_vti <- Vo(da_ta)
v_wap <- VWAP(price=adj_vti, volume=vol_vti, n=10)
adj_vti <- adj_vti["2009-02/2009-03"]
da_ta <- da_ta["2009-02/2009-03"]
v_wap <- v_wap["2009-02/2009-03"]
# OHLC candlechart VWAP in main plot,
chart_Series(x=da_ta, # volume in extra panel
       TA="add_Vo(); add_TA(v_wap, on=1)",
       name="VTI plus VWAP shaded")
# add price minus VWAP in extra panel
add_TA(adj_vti-v_wap, col='red')
# add background shading of areas
add_TA((adj_vti-v_wap) > 0, on=-1,
col="lightgreen", border="lightgreen")
add_TA((adj_vti-v_wap) < 0, on=-1,
col="lightgrey", border="lightgrey")
# add vertical and horizontal lines
abline(v=which.min(v_wap), col='red')
abline(h=min(v_wap), col='red')
library(quantmod)
load(file="C:/Develop/data/etf_data.RData")
da_ta <- env_data$VTI["2009-02/2009-03"]
# extract plot object
ch_ob <- chart_Series(x=da_ta, plot=FALSE)
class(ch_ob)
ls(ch_ob)
class(ch_ob$get_ylim)
class(ch_ob$set_ylim)
# ls(ch_ob$Env)
class(ch_ob$Env$actions)
plot_theme <- chart_theme()
class(plot_theme)
ls(plot_theme)
library(quantmod)
load(file="C:/Develop/data/etf_data.RData")
da_ta <- env_data$VTI["2010-04/2010-05"]
# extract, modify theme, format tick marks "%b %d"
plot_theme <- chart_theme()
plot_theme$format.labels <- "%b %d"
# create plot object
ch_ob <- chart_Series(x=da_ta,
                theme=plot_theme, plot=FALSE)
# extract ylim using accessor function
y_lim <- ch_ob$get_ylim()
y_lim[[2]] <- structure(
  range(Ad(da_ta)) + c(-1, 1),
  fixed=TRUE)
# modify plot object to reduce y-axis range
ch_ob$set_ylim(y_lim)  # use setter function
# render the plot
plot(ch_ob)
library(quantmod)
load(file="C:/Develop/data/etf_data.RData")
# specify plot area with two horizontal panels
par(mfrow=c(2, 1))
# plot in top panel
chart_Series(x=env_data$VTI["2009-02/2009-04"],
       name="VTI")
# plot in bottom panel
chart_Series(x=env_data$XLF["2009-02/2009-04"],
       name="XLF")
library(quantmod)
load(file="C:/Develop/data/etf_data.RData")
# download U.S. unemployment rate data
unemp_rate <- getSymbols("UNRATE",
            auto.assign=FALSE,
            src="FRED")
# plot U.S. unemployment rate data
chartSeries(unemp_rate["1990/"],
      name="U.S. unemployment rate",
      theme=chartTheme("white"))
# download 10-Year Treasury constant maturity rate
trs_10yr <- getSymbols("DGS10",
            auto.assign=FALSE,
            src="FRED")
library(quantmod)  # load package quantmod
install.packages("devtools")
library(devtools)
# install package Quandl from github
install_github("quandl/R-package")
library(Quandl)  # load package Quandl
# register Quandl API key
Quandl.api_key("pVJi9Nv3V8CD3Js5s7Qx")
# get short description
packageDescription("Quandl")
# load help page
help(package="Quandl")
# remove Quandl from search path
detach("package:Quandl")
library(quantmod)  # load package quantmod
# download EOD AAPL prices from WIKI free database
price_s <- Quandl(code="WIKI/AAPL", type="xts")
chart_Series(price_s["2013/", 11], name="AAPL close prices")
# add trade volume in extra panel
add_TA(price_s["2013/", 12])
# download euro currency rates
price_s <-
  Quandl(code="BNP/USDEUR", start_date="2013-01-01",
   end_date="2013-12-01", type="xts")
# download multiple time series
price_s <- Quandl(code=c("NSE/OIL", "WIKI/AAPL"),
   start_date="2013-01-01", type="xts")
# download AAPL gross profits
prof_it <-
  Quandl("RAYMOND/AAPL_GROSS_PROFIT_Q", type="xts")
chart_Series(prof_it, name="AAPL gross profits")
# download Hurst time series
price_s <- Quandl(code="PE/AAPL_HURST",
       start_date="2013-01-01", type="xts")
chart_Series(price_s["2016/", 1],
       name="AAPL Hurst")
library(quantmod)  # load package quantmod
# load S&P500 stock Quandl codes
sp_500 <- read.csv(
  file="C:/Develop/data/SP500_Wiki.csv",
  stringsAsFactors=FALSE)
# replace "-" with "_" in tickers
sp_500$free_code <-
  gsub("-", "_", sp_500$free_code)
head(sp_500)
# vector of tickers in sp_500 frame
tick_ers <- sp_500$ticker
# or
tick_ers <- matrix(unlist(
  strsplit(sp_500$free_code, split="/"),
  use.names=FALSE), ncol=2, byrow=TRUE)[, 2]
# or
tick_ers <- do_call_rbind(
  strsplit(sp_500$free_code, split="/"))[, 2]
library(quantmod)  # load package quantmod
env_data <- new.env()  # new environment for data
# remove all files (if necessary)
rm(list=ls(env_data), envir=env_data)
# boolean vector of tickers already downloaded
down_loaded <- tick_ers %in% ls(env_data)
# download data and copy it into environment
for (tick_er in tick_ers[!down_loaded]) {
  cat("processing: ", tick_er, "\n")
  da_ta <- Quandl(code=paste0("WIKI/", tick_er),
            start_date="1990-01-01",
            type="xts")[, -(1:7)]
  colnames(da_ta) <- paste(tick_er,
    c("Open", "High", "Low", "Close", "Volume"), sep=".")
  assign(tick_er, da_ta, envir=env_data)
}  # end for
save(env_data, file="C:/Develop/data/sp500.RData")
chart_Series(x=env_data$XOM["2016/"], TA="add_Vo()",
       name="XOM stock")
library(quantmod)  # load package quantmod
# download Fama-French factors from KFRENCH database
fac_tors <- Quandl(code="KFRENCH/FACTORS_D",
  start_date="2001-01-01", type="xts")
dim(fac_tors)
head(fac_tors)
tail(fac_tors)
chart_Series(cumsum(fac_tors["2001/", 1]/100),
  name="Fama-French factors")
library(PerformanceAnalytics)  # load package "PerformanceAnalytics"
# get documentation for package "PerformanceAnalytics"
packageDescription("PerformanceAnalytics")  # get short description
help(package="PerformanceAnalytics")  # load help page
data(package="PerformanceAnalytics")  # list all datasets in "PerformanceAnalytics"
ls("package:PerformanceAnalytics")  # list all objects in "PerformanceAnalytics"
detach("package:PerformanceAnalytics")  # remove PerformanceAnalytics from search path
library(PerformanceAnalytics)  # load package "PerformanceAnalytics"
perf_data <- 
  unclass(data(
    package="PerformanceAnalytics"))$results[, -(1:2)]
apply(perf_data, 1, paste, collapse=" - ")
data(managers)  # load "managers" data set
class(managers)
dim(managers)
head(managers, 3)
# load package "PerformanceAnalytics"
library(PerformanceAnalytics)
data(managers)  # load "managers" data set
ham_1 <- managers[, c("HAM1", "EDHEC LS EQ",
                "SP500 TR")]

chart.CumReturns(ham_1, lwd=2, ylab="",
  legend.loc="topleft", main="")
# add title
title(main="Managers cumulative returns",
line=-1)
library(PerformanceAnalytics)  # load package "PerformanceAnalytics"
data(managers)  # load "managers" data set
charts.PerformanceSummary(ham_1,
  main="", lwd=2, ylog=TRUE)
library(PerformanceAnalytics)  # load package "PerformanceAnalytics"
chart.CumReturns(
  etf_rets[, c("XLF", "XLP", "IEF")], lwd=2,
  ylab="", legend.loc="topleft", main="")
# add title
title(main="ETF cumulative returns", line=-1)
load(file="C:/Develop/data/etf_data.RData")
options(width=200)
library(PerformanceAnalytics)
chart.Drawdown(etf_rets[, "VTI"], ylab="",
         main="VTI drawdowns")
load(file="C:/Develop/data/etf_data.RData")
options(width=200)
library(PerformanceAnalytics)
table.Drawdowns(etf_rets[, "VTI"])
library(PerformanceAnalytics)
chart.Histogram(etf_rets[, 1], main="",
  xlim=c(-0.06, 0.06),
  methods = c("add.density", "add.normal"))
# add title
title(main=paste(colnames(etf_rets[, 1]),
           "density"), line=-1)
library(PerformanceAnalytics)
chart.Boxplot(etf_rets[,
  c(rownames(head(ret_stats, 3)),
    rownames(tail(ret_stats, 3)))])
library(PerformanceAnalytics)
tail(table.Stats(etf_rets[, 
  c("VTI", "IEF", "DBC", "IUSG")]), 4)
ret_stats <- table.Stats(etf_rets)
class(ret_stats)
# Transpose the data frame
ret_stats <- as.data.frame(t(ret_stats))
# plot scatterplot
plot(Kurtosis ~ Skewness, data=ret_stats,
     main="Kurtosis vs Skewness")
# add labels
text(x=ret_stats$Skewness, y=ret_stats$Kurtosis,
    labels=rownames(ret_stats),
    pos=1, cex=0.8)
load(file="C:/Develop/data/etf_data.RData")
# add skew_kurt column
ret_stats$skew_kurt <- 
  ret_stats$Skewness/ret_stats$Kurtosis
# sort on skew_kurt
ret_stats <- ret_stats[
  order(ret_stats$skew_kurt, 
  decreasing=TRUE), ]
# add names column
ret_stats$Name <- 
  etf_list[rownames(ret_stats), ]$Name
ret_stats[, c("Name", "Skewness", "Kurtosis")]
library(PerformanceAnalytics)
chart.RiskReturnScatter(etf_rets, Rf=0.01/12)
library(PerformanceAnalytics)
vti_ief <- etf_rets[, c("VTI", "IEF")]
SharpeRatio(vti_ief)

SortinoRatio(vti_ief)

CalmarRatio(vti_ief)
tail(table.Stats(vti_ief), 4)
