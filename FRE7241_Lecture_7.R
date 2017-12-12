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
library(xtable)
# ETF symbols for asset allocation
sym_bols <- c("VTI", "VEU", "IEF", "VNQ",
  "DBC", "VXX", "XLY", "XLP", "XLE", "XLF",
  "XLV", "XLI", "XLB", "XLK", "XLU", "VYM",
  "IVW", "IWB", "IWD", "IWF")
# read etf database into data frame
etf_list <- read.csv(
  file='C:/Develop/R/lecture_slides/data/etf_list.csv', 
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
print(xtable(etf_list), comment=FALSE, size="tiny", include.rownames=FALSE)
library(quantmod)  # load package quantmod
env_etf <- new.env()  # new environment for data
# download data for sym_bols into env_etf
getSymbols(sym_bols, env=env_etf, adjust=TRUE,
    from="2007-01-03")
library(quantmod)  # load package quantmod
ls(env_etf)  # list files in env_etf
# get class of object in env_etf
class(get(x=sym_bols[1], envir=env_etf))
# another way
class(env_etf$VTI)
colnames(env_etf$VTI)
head(env_etf$VTI, 3)
# get class of all objects in env_etf
eapply(env_etf, class)
# get class of all objects in R workspace
lapply(ls(), function(ob_ject) class(get(ob_ject)))
library(quantmod)  # load package quantmod
# check of object is an OHLC time series
is.OHLC(env_etf$VTI)
# adjust single OHLC object using its name
env_etf$VTI <- adjustOHLC(env_etf$VTI,
                     use.Adjusted=TRUE)

# adjust OHLC object using string as name
assign(sym_bols[1], adjustOHLC(
    get(x=sym_bols[1], envir=env_etf),
    use.Adjusted=TRUE),
  envir=env_etf)

# adjust objects in environment using vector of strings
for (sym_bol in sym_bols) {
  assign(sym_bol,
   adjustOHLC(get(sym_bol, envir=env_etf),
              use.Adjusted=TRUE),
   envir=env_etf)
}  # end for
library(quantmod)  # load package quantmod
# extract and merge all data, subset by symbols
price_s <- do.call(merge,
  as.list(env_etf)[sym_bols])
# or
price_s <- rutils::do_call(cbind,
  as.list(env_etf)[sym_bols])
# extract and merge adjusted prices, subset by symbols
price_s <- rutils::do_call(cbind,
  lapply(as.list(env_etf)[sym_bols], Ad))
# same, but works only for OHLC series
price_s <- rutils::do_call(cbind,
  eapply(env_etf, Ad)[sym_bols])
# drop ".Adjusted" from colnames
colnames(price_s) <-
  sapply(colnames(price_s),
    function(col_name)
strsplit(col_name, split="[.]")[[1]])[1, ]
tail(price_s[, 1:2], 3)
# which objects in global environment are class xts?
unlist(eapply(globalenv(), is.xts))
# save xts to csv file
write.zoo(price_s,
  file='etf_series.csv', sep=",")
# copy price_s into env_etf and save to .RData file
assign("price_s", price_s, envir=env_etf)
save(env_etf, file='etf_data.RData')
library(quantmod)
# remove rows with NA values
# price_s <- env_etf$price_s[complete.cases(env_etf$price_s)]
# colnames(price_s)
# calculate returns from adjusted prices
re_turns <- lapply(env_etf$price_s, function(x_ts) {
# dailyReturn returns single xts with bad colname
  daily_return <- dailyReturn(x_ts)
  colnames(daily_return) <- names(x_ts)
  daily_return
})  # end lapply

# "re_turns" is a list of xts
class(re_turns)
class(re_turns[[1]])

# flatten list of xts into a single xts
re_turns <- do.call(merge, re_turns)
class(re_turns)
dim(re_turns)
head(re_turns[, 1:3])
# copy re_turns into env_etf and save to .RData file
assign("re_turns", re_turns, envir=env_etf)
save(env_etf, file='etf_data.RData')
library(quantmod)
start_date <- "2012-05-10"; end_date <- "2013-11-20"
# subset all objects in environment and return as environment
new_env <- as.environment(eapply(env_etf, "[",
            paste(start_date, end_date, sep="/")))
# subset only sym_bols in environment and return as environment
new_env <- as.environment(
  lapply(as.list(env_etf)[sym_bols], "[",
   paste(start_date, end_date, sep="/")))
# extract and merge adjusted prices and return to environment
assign("price_s", do.call(merge,
         lapply(ls(env_etf), function(sym_bol) {
           x_ts <- Ad(get(sym_bol, env_etf))
           colnames(x_ts) <- sym_bol
           x_ts
         })), envir=new_env)
# get sizes of OHLC xts series in env_etf
sapply(mget(env_etf$sym_bols, envir=env_etf),
 object.size)
# extract and merge adjusted prices and return to environment
col_name <- function(x_ts)
  strsplit(colnames(x_ts), split="[.]")[[1]][1]
assign("price_s", do.call(merge,
         lapply(mget(env_etf$sym_bols, envir=env_etf),
                function(x_ts) {
                  x_ts <- Ad(x_ts)
                  colnames(x_ts) <- col_name(x_ts)
                  x_ts
         })), envir=new_env)
library(quantmod)
# plot OHLC candlechart with volume
chartSeries(env_etf$VTI["2014-11"],
      name="VTI",
      theme=chartTheme("white"))
# plot OHLC bar chart with volume
chartSeries(env_etf$VTI["2014-11"],
      type="bars",
      name="VTI",
      theme=chartTheme("white"))
library(quantmod)
# plot OHLC candlechart with volume
chartSeries(env_etf$VTI["2008-11/2009-04"],
      name="VTI")
# redraw plot only for Feb-2009, with white theme
reChart(subset="2009-02",
  theme=chartTheme("white"))
library(quantmod)
# candlechart with Bollinger Bands
chartSeries(env_etf$VTI["2014"],
      TA="addBBands(): addBBands(draw='percent'): addVo()",
      name="VTI with Bollinger Bands",
      theme=chartTheme("white"))
# candlechart with two Moving Averages
chartSeries(env_etf$VTI["2014"],
      TA="addVo(): addEMA(10): addEMA(30)",
      name="VTI with Moving Averages",
      theme=chartTheme("white"))
# candlechart with Commodity Channel Index
chartSeries(env_etf$VTI["2014"],
      TA="addVo(): addBBands(): addCCI()",
      name="VTI with Technical Indicators",
      theme=chartTheme("white"))
library(quantmod)
library(TTR)
oh_lc <- rutils::env_etf$VTI["2009-02/2009-03"]
VTI_adj <- Ad(oh_lc); VTI_vol <- Vo(oh_lc)
# calculate volume-weighted average price
VTI_vwap <- TTR::VWAP(price=VTI_adj,
volume=VTI_vol, n=10)
# plot OHLC candlechart with volume
chartSeries(oh_lc, name="VTI plus VWAP",
      theme=chartTheme("white"))
# add VWAP to main plot
addTA(ta=VTI_vwap, on=1, col='red')
# add price minus VWAP in extra panel
addTA(ta=(VTI_adj-VTI_vwap), col='red')
library(quantmod)
library(TTR)
oh_lc <- rutils::env_etf$VTI
VTI_adj <- Ad(oh_lc)
VTI_vol <- Vo(oh_lc)
VTI_vwap <- TTR::VWAP(price=VTI_adj, volume=VTI_vol, n=10)
VTI_adj <- VTI_adj["2009-02/2009-03"]
oh_lc <- oh_lc["2009-02/2009-03"]
VTI_vwap <- VTI_vwap["2009-02/2009-03"]
# plot OHLC candlechart with volume
chartSeries(oh_lc, name="VTI plus VWAP shaded",
      theme=chartTheme("white"))
# add VWAP to main plot
addTA(ta=VTI_vwap, on=1, col='red')
# add price minus VWAP in extra panel
addTA(ta=(VTI_adj-VTI_vwap), col='red')
# add background shading of areas
addTA((VTI_adj-VTI_vwap) > 0, on=-1,
col="lightgreen", border="lightgreen")
addTA((VTI_adj-VTI_vwap) < 0, on=-1,
col="lightgrey", border="lightgrey")
# add vertical and horizontal lines at VTI_vwap minimum
addLines(v=which.min(VTI_vwap), col='red')
addLines(h=min(VTI_vwap), col='red')
library(quantmod)
library(TTR)
oh_lc <- rutils::env_etf$VTI
VTI_adj <- Ad(oh_lc)
VTI_vol <- Vo(oh_lc)
VTI_vwap <- TTR::VWAP(price=VTI_adj, volume=VTI_vol, n=10)
VTI_adj <- VTI_adj["2009-02/2009-03"]
oh_lc <- oh_lc["2009-02/2009-03"]
VTI_vwap <- VTI_vwap["2009-02/2009-03"]
# OHLC candlechart VWAP in main plot,
chart_Series(x=oh_lc, # volume in extra panel
       TA="add_Vo(); add_TA(VTI_vwap, on=1)",
       name="VTI plus VWAP shaded")
# add price minus VWAP in extra panel
add_TA(VTI_adj-VTI_vwap, col='red')
# add background shading of areas
add_TA((VTI_adj-VTI_vwap) > 0, on=-1,
col="lightgreen", border="lightgreen")
add_TA((VTI_adj-VTI_vwap) < 0, on=-1,
col="lightgrey", border="lightgrey")
# add vertical and horizontal lines
abline(v=which.min(VTI_vwap), col='red')
abline(h=min(VTI_vwap), col='red')
library(quantmod)
oh_lc <- rutils::env_etf$VTI["2009-02/2009-03"]
# extract plot object
ch_ob <- chart_Series(x=oh_lc, plot=FALSE)
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
oh_lc <- rutils::env_etf$VTI["2010-04/2010-05"]
# extract, modify theme, format tick marks "%b %d"
plot_theme <- chart_theme()
plot_theme$format.labels <- "%b %d"
# create plot object
ch_ob <- chart_Series(x=oh_lc,
                theme=plot_theme, plot=FALSE)
# extract ylim using accessor function
y_lim <- ch_ob$get_ylim()
y_lim[[2]] <- structure(
  range(Ad(oh_lc)) + c(-1, 1),
  fixed=TRUE)
# modify plot object to reduce y-axis range
ch_ob$set_ylim(y_lim)  # use setter function
# render the plot
plot(ch_ob)
library(HighFreq)
# calculate VTI and XLF volume-weighted average price
VTI_vwap <-
  TTR::VWAP(price=Ad(rutils::env_etf$VTI),
      volume=Vo(rutils::env_etf$VTI), n=10)
XLF_vwap <-
  TTR::VWAP(price=Ad(rutils::env_etf$XLF),
      volume=Vo(rutils::env_etf$XLF), n=10)
# open graphics device, and define
# plot area with two horizontal panels
x11(); par(mfrow=c(2, 1))
ch_ob <- chart_Series(  # plot in top panel
  x=env_etf$VTI["2009-02/2009-04"],
  name="VTI", plot=FALSE)
add_TA(VTI_vwap["2009-02/2009-04"],
 lwd=2, on=1, col='blue')
ch_ob <- chart_Series(  # plot in bottom panel
  x=env_etf$XLF["2009-02/2009-04"],
  name="XLF", plot=FALSE)
add_TA(XLF_vwap["2009-02/2009-04"],
 lwd=2, on=1, col='blue')
library(dygraphs)
# calculate volume-weighted average price
oh_lc <- rutils::env_etf$VTI
VTI_vwap <- TTR::VWAP(price=quantmod::Ad(oh_lc),
    volume=quantmod::Vo(oh_lc), n=20)
# add VWAP to OHLC  data
oh_lc <- cbind(oh_lc[, c(1:3, 6)],
         VTI_vwap)["2009-02/2009-04"]
# create dygraphs object
dy_graph <- dygraphs::dygraph(oh_lc)
# convert dygraphs object to candlestick plot
dy_graph <- dygraphs::dyCandlestick(dy_graph)
# render candlestick plot
dy_graph
# candlestick plot using pipes syntax
dygraphs::dygraph(oh_lc) %>% dyCandlestick()
# candlestick plot without using pipes syntax
dygraphs::dyCandlestick(dygraphs::dygraph(oh_lc))
# create candlestick plot with background shading
in_dex <- index(oh_lc)
in_dic <-
  rutils::diff_xts(oh_lc[, 4] > oh_lc[, "VWAP"])
in_dic <- rbind(cbind(which(in_dic==1), 1),
  cbind(which(in_dic==(-1)), -1))
in_dic <- in_dic[order(in_dic[, 1]), ]
in_dic <- rbind(c(1, -in_dic[1, 2]), in_dic,
  c(NROW(oh_lc), -in_dic[NROW(in_dic), 2]))
in_dic <-
  data.frame(in_dex[in_dic[, 1]], in_dic[, 2])
# create dygraphs object
dy_graph <- dygraphs::dygraph(oh_lc) %>%
  dyCandlestick()
# add shading
for (i in 1:(NROW(in_dic)-1)) {
  if (in_dic[i, 2] == 1)
    dy_graph <- dy_graph %>% dyShading(from=in_dic[i, 1], to=in_dic[i+1, 1], color="lightgreen")
  else
    dy_graph <- dy_graph %>% dyShading(from=in_dic[i, 1], to=in_dic[i+1, 1], color="antiquewhite")
}  # end for
# render plot
dy_graph
library(dygraphs)
# prepare VTI and IEF prices
price_s <- cbind(Ad(rutils::env_etf$VTI),
           Ad(rutils::env_etf$IEF))
col_names <- rutils::get_name(colnames(price_s))
colnames(price_s) <- col_names

# dygraphs plot with two y-axes
library(dygraphs)
dygraphs::dygraph(price_s, main=paste(col_names, collapse=" and ")) %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(col_names[2], axis="y2", col=c("red", "blue"))
library(quantmod)  # load package quantmod
# assign name SP500 to ^GSPC symbol
setSymbolLookup(
  SP500=list(name="^GSPC", src="yahoo"))
getSymbolLookup()
# view and clear options
options("getSymbols.sources")
options(getSymbols.sources=NULL)
# download S&P500 prices into env_etf
getSymbols("SP500", env=env_etf,
    adjust=TRUE, from="1990-01-01")
chart_Series(x=env_etf$SP500["2016/"],
       TA="add_Vo()",
       name="S&P500 index")
library(quantmod)  # load package quantmod
# assign name DJIA to ^DJI symbol
setSymbolLookup(
  DJIA=list(name="^DJI", src="yahoo"))
getSymbolLookup()
# view and clear options
options("getSymbols.sources")
options(getSymbols.sources=NULL)
# download DJIA prices into env_etf
getSymbols("DJIA", env=env_etf,
    adjust=TRUE, from="1990-01-01")
chart_Series(x=env_etf$DJIA["2016/"],
       TA="add_Vo()",
       name="DJIA index")
library(quantmod)  # load package quantmod
library(RCurl)  # load package RCurl
library(XML)  # load package XML
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
# create valid R names from symbols containing "-" or "."characters
sp_500$names <- gsub("-", "_", sp_500$Ticker)
sp_500$names <- gsub("[.]", "_", sp_500$names)
# write data frame of S&P500 constituents to CSV file
write.csv(sp_500,
  file="C:/Develop/R/lecture_slides/data/sp500_Yahoo.csv",
  row.names=FALSE)
library(HighFreq)  # load package HighFreq
# load data frame of S&P500 constituents from CSV file
sp_500 <- read.csv(file="C:/Develop/R/lecture_slides/data/sp500_Yahoo.csv",
     stringsAsFactors=FALSE)
# register symbols corresponding to R names
for (in_dex in 1:NROW(sp_500)) {
  cat("processing: ", sp_500$Ticker[in_dex], "\n")
  setSymbolLookup(structure(
    list(list(name=sp_500$Ticker[in_dex])),
    names=sp_500$names[in_dex]))
}  # end for
env_sp500 <- new.env()  # new environment for data
# remove all files (if necessary)
rm(list=ls(env_sp500), envir=env_sp500)
# download data and copy it into environment
rutils::get_symbols(sp_500$names,
   env_out=env_sp500, start_date="1990-01-01")
# or download in loop
for (sym_bol in sp_500$names) {
  cat("processing: ", sym_bol, "\n")
  rutils::get_symbols(sym_bol,
   env_out=env_sp500, start_date="1990-01-01")
}  # end for
save(env_sp500, file="C:/Develop/R/lecture_slides/data/sp500.RData")
chart_Series(x=env_sp500$BRK_B["2016/"], TA="add_Vo()",
       name="BRK-B stock")
library(quantmod)
# download U.S. unemployment rate data
unemp_rate <- getSymbols("UNRATE",
            auto.assign=FALSE,
            src="FRED")
# plot U.S. unemployment rate data
chart_Series(unemp_rate["1990/"],
      name="U.S. unemployment rate")
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
price_s <- Quandl(code="WIKI/AAPL",
            type="xts", start_date="1990-01-01")
x11(width=14, height=7)
chart_Series(price_s["2016", 1:4],
    name="AAPL OHLC prices")
# add trade volume in extra panel
add_TA(price_s["2016", 5])
# download euro currency rates
price_s <- Quandl(code="BNP/USDEUR",
    start_date="2013-01-01",
    end_date="2013-12-01", type="xts")
# download multiple time series
price_s <- Quandl(code=c("NSE/OIL", "WIKI/AAPL"),
    start_date="2013-01-01", type="xts")
# download AAPL gross profits
prof_it <- Quandl("RAYMOND/AAPL_GROSS_PROFIT_Q",
    type="xts")
chart_Series(prof_it, name="AAPL gross profits")
# download Hurst time series
price_s <- Quandl(code="PE/AAPL_HURST",
    start_date="2013-01-01", type="xts")
chart_Series(price_s["2016/", 1],
       name="AAPL Hurst")
library(quantmod)  # load package quantmod
# load S&P500 stock Quandl codes
sp_500 <- read.csv(
  file="C:/Develop/R/lecture_slides/data/sp500_quandl.csv",
  stringsAsFactors=FALSE)
# replace "-" with "_" in symbols
sp_500$free_code <-
  gsub("-", "_", sp_500$free_code)
head(sp_500)
# vector of symbols in sp_500 frame
tick_ers <- gsub("-", "_", sp_500$ticker)
# or
tick_ers <- matrix(unlist(
  strsplit(sp_500$free_code, split="/"),
  use.names=FALSE), ncol=2, byrow=TRUE)[, 2]
# or
tick_ers <- do_call_rbind(
  strsplit(sp_500$free_code, split="/"))[, 2]
library(quantmod)  # load package quantmod
env_sp500 <- new.env()  # new environment for data
# remove all files (if necessary)
rm(list=ls(env_sp500), envir=env_sp500)
# Boolean vector of symbols already downloaded
down_loaded <- tick_ers %in% ls(env_sp500)
# download data and copy it into environment
for (tick_er in tick_ers[!down_loaded]) {
  cat("processing: ", tick_er, "\n")
  da_ta <- Quandl(code=paste0("WIKI/", tick_er),
            start_date="1990-01-01",
            type="xts")[, -(1:7)]
  colnames(da_ta) <- paste(tick_er,
    c("Open", "High", "Low", "Close", "Volume"), sep=".")
  assign(tick_er, da_ta, envir=env_sp500)
}  # end for
save(env_sp500, file="C:/Develop/R/lecture_slides/data/sp500.RData")
chart_Series(x=env_sp500$XOM["2016/"], TA="add_Vo()",
       name="XOM stock")
library(xtable)
# read etf database into data frame
fundamental_data <-
  read.csv(file='C:/Develop/R/lecture_slides/data/fundamental_stock_data.csv',
               stringsAsFactors=FALSE)
print(xtable(fundamental_data), comment=FALSE, size="scriptsize", include.rownames=FALSE)
library(Quandl)  # load package Quandl
# register Quandl API key
Quandl.api_key("pVJi9Nv3V8CD3Js5s7Qx")

# Quandl stock market data
# https://blog.quandl.com/stock-market-data-ultimate-guide-part-1
# https://blog.quandl.com/stock-market-data-the-ultimate-guide-part-2

# download RAYMOND metadata
# https://www.quandl.com/data/RAYMOND-Raymond/documentation/metadata

# download S&P 500 Index sonstituents
# https://s3.amazonaws.com/static.quandl.com/tickers/SP500.csv

# download AAPL gross profits from RAYMOND
prof_it <-
  Quandl("RAYMOND/AAPL_GROSS_PROFIT_Q", type="xts")
chart_Series(prof_it, name="AAPL gross profits")

# download multiple time series
price_s <- Quandl(code=c("NSE/OIL", "WIKI/AAPL"),
   start_date="2013-01-01", type="xts")

# download datasets for AAPL
# https://www.quandl.com/api/v3/datasets/WIKI/AAPL.json

# download metadata for AAPL
price_s <- Quandl(code=c("NSE/OIL", "WIKI/AAPL"),
   start_date="2013-01-01", type="xts")
# https://www.quandl.com/api/v3/datasets/WIKI/AAPL/metadata.json

# scrape fundamental data from Google using quantmod - doesn't work
funda_mentals <- getFinancials("HPQ", src="google", auto.assign=FALSE)
# view quarterly fundamentals
viewFinancials(funda_mentals,  period="Q")
viewFinancials(funda_mentals)

# scrape fundamental data from Yahoo using quantmod
# table of Yahoo data fields
# http://www.financialwisdomforum.org/gummy-stuff/Yahoo-data.htm

met_rics <- yahooQF(c("Price/Sales",
                "P/E Ratio",
                "Price/EPS Estimate Next Year",
                "PEG Ratio",
                "Dividend Yield",
                "Market Capitalization"))


sym_bols <- c("AAPL", "IBM", "MSFT")
# Not all the metrics are returned by Yahoo.
funda_mentals <- getQuote(paste(sym_bols, sep="", collapse=";"), src="yahoo", what=met_rics)
viewFinancials(funda_mentals,  period="Q")

funda_mentals <- getFinancials("HPQ", src="yahoo", auto.assign=FALSE)
viewFinancials(funda_mentals)


library(quantmod)  # load package quantmod
install.packages("devtools")
library(devtools)
# install package WRDS from github
install_github("WRDS/R-package")
library(WRDS)  # load package WRDS
# register WRDS API key
WRDS.api_key("pVJi9Nv3V8CD3Js5s7Qx")
# get short description
packageDescription("WRDS")
# load help page
help(package="WRDS")
# remove WRDS from search path
detach("package:WRDS")
library(quantmod)  # load package quantmod
env_sp500 <- new.env()  # new environment for data
# remove all files (if necessary)
rm(list=ls(env_sp500), envir=env_sp500)
# Boolean vector of symbols already downloaded
down_loaded <- tick_ers %in% ls(env_sp500)
# download data and copy it into environment
for (tick_er in tick_ers[!down_loaded]) {
  cat("processing: ", tick_er, "\n")
  da_ta <- Quandl(code=paste0("WIKI/", tick_er),
            start_date="1990-01-01",
            type="xts")[, -(1:7)]
  colnames(da_ta) <- paste(tick_er,
    c("Open", "High", "Low", "Close", "Volume"), sep=".")
  assign(tick_er, da_ta, envir=env_sp500)
}  # end for
save(env_sp500, file="C:/Develop/R/lecture_slides/data/sp500.RData")
chart_Series(x=env_sp500$XOM["2016/"], TA="add_Vo()",
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
# load package HighFreq
library(HighFreq)
head(SPY_TAQ)
# load package HighFreq
library(HighFreq)
head(SPY)
# install package HighFreq from github
devtools::install_github(repo="algoquant/HighFreq")
# load package HighFreq
library(HighFreq)
# get documentation for package HighFreq
# get short description
packageDescription("HighFreq")
# load help page
help(package="HighFreq")
# list all datasets in "HighFreq"
data(package="HighFreq")
# list all objects in "HighFreq"
ls("package:HighFreq")
# remove HighFreq from search path
detach("package:HighFreq")
# load package HighFreq
library(HighFreq)
# you can see SPY when listing objects in HighFreq
ls("package:HighFreq")
# you can see SPY when listing datasets in HighFreq
data(package="HighFreq")
# but the SPY dataset isn't listed in the workspace
ls()
# HighFreq datasets are lazy loaded and available when needed
head(SPY)
# load all the datasets in package HighFreq
data(hf_data)
# HighFreq datasets are now loaded and in the workspace
head(SPY)
library(HighFreq)
# specify formula and perform regression
reg_formula <- XLP ~ VTI
reg_model <- lm(reg_formula, 
          data=rutils::env_etf$re_turns)
# get regression coefficients
coef(summary(reg_model))
# Durbin-Watson test of autocorrelation of residuals
lmtest::dwtest(reg_model)
# plot scatterplot of returns with aspect ratio 1
plot(reg_formula, data=rutils::env_etf$re_turns,
     xlim=c(-0.1, 0.1), ylim=c(-0.1, 0.1),
     asp=1, main="Regression XLP ~ VTI")
# add regression line and perpendicular line
abline(reg_model, lwd=2, col="red")
abline(a=0, b=-1/coef(summary(reg_model))[2, 1],
 lwd=2, col="blue")
library(HighFreq)  # load HighFreq
re_turns <- na.omit(rutils::env_etf$re_turns)
# perform regressions and collect statistics
etf_reg_stats <- sapply(colnames(re_turns)[-1],
                  function(etf_name) {
# specify regression formula
  reg_formula <- as.formula(
    paste(etf_name, "~ VTI"))
# perform regression
  reg_model <- lm(reg_formula, data=re_turns)
# get regression summary
  reg_model_sum <- summary(reg_model)
# collect regression statistics
  etf_reg_stats <- with(reg_model_sum,
    c(alpha=coefficients[1, 1],
p_alpha=coefficients[1, 4],
beta=coefficients[2, 1],
p_beta=coefficients[2, 4]))
  etf_reg_stats <- c(etf_reg_stats,
         p_dw=lmtest::dwtest(reg_model)$p.value)
  etf_reg_stats
})  # end sapply
etf_reg_stats <- t(etf_reg_stats)
# sort by p_alpha
etf_reg_stats <- etf_reg_stats[
  order(etf_reg_stats[, "p_alpha"]), ]
etf_reg_stats[, 1:3]
library(HighFreq)
# specify regression formula
reg_formula <- XLP ~ VTI
# perform rolling beta regressions every month
beta_s <- rollapply(rutils::env_etf$re_turns, width=252,
  FUN=function(de_sign)
  coef(lm(reg_formula, data=de_sign))[2],
  by=22, by.column=FALSE, align="right")
beta_s <- na.omit(beta_s)
# plot beta_s in x11() window
x11(width=(wid_th <- 6), height=(hei_ght <- 4))
chart_Series(x=beta_s,
  name=paste("rolling betas", format(reg_formula)))
# perform daily rolling beta regressions in parallel
library(roll)
beta_s <- roll_lm(x=rutils::env_etf$re_turns[, "VTI"],
            y=rutils::env_etf$re_turns[, "XLP"],
            width=252)$coefficients
# compare speed of rollapply() versus roll_lm()
library(microbenchmark)
da_ta <- rutils::env_etf$re_turns["2012", c("VTI", "XLP")]
summary(microbenchmark(
  rollapply=rollapply(da_ta, width=22,
FUN=function(de_sign)
coef(lm(reg_formula, data=de_sign))[2],
  by.column=FALSE, align="right"),
  roll_lm=roll_lm(x=da_ta[, "VTI"],
            y=da_ta[, "XLP"],
            width=22)$coefficients,
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
library(PerformanceAnalytics)
CAPM.beta(Ra=re_turns[, "XLP"],
    Rb=re_turns[, "VTI"])
CAPM.beta.bull(Ra=re_turns[, "XLP"],
  Rb=re_turns[, "VTI"])
CAPM.beta.bear(Ra=re_turns[, "XLP"],
  Rb=re_turns[, "VTI"])
CAPM.alpha(Ra=re_turns[, "XLP"],
     Rb=re_turns[, "VTI"])
library(PerformanceAnalytics)
etf_betas <- sapply(
  re_turns[, colnames(re_turns)!="VXX"],
  CAPM.beta, Rb=re_turns[, "VTI"])
etf_annrets <- sapply(
  re_turns[, colnames(re_turns)!="VXX"],
  Return.annualized)
# plot scatterplot
plot(etf_annrets ~ etf_betas, xlab="betas",
      ylab="ann. rets", xlim=c(-0.25, 1.6))
points(x=1, y=etf_annrets["VTI"], col="red",
 lwd=3, pch=21)
abline(a=0, b=etf_annrets["VTI"])
label_names <- rownames(etf_reg_stats)[1:13]
# add labels
text(x=1, y=etf_annrets["VTI"], labels="VTI",
     pos=2)
text(x=etf_betas[label_names],
     y=etf_annrets[label_names],
     labels=label_names, pos=2, cex=0.8)
library(PerformanceAnalytics)
TreynorRatio(Ra=re_turns[, "XLP"],
     Rb=re_turns[, "VTI"])

InformationRatio(Ra=re_turns[, "XLP"],
     Rb=re_turns[, "VTI"])
library(PerformanceAnalytics)
table.CAPM(Ra=re_turns[, c("XLP", "XLF")],
     Rb=re_turns[, "VTI"], scale=252)
library(PerformanceAnalytics)
capm_stats <- table.CAPM(Ra=re_turns[, colnames(re_turns)!="VTI"],
        Rb=re_turns[, "VTI"], scale=252)
colnames(capm_stats) <-
  sapply(colnames(capm_stats),
  function (str) {strsplit(str, split=" ")[[1]][1]})
capm_stats <- as.matrix(capm_stats)
capm_stats <- t(capm_stats)
capm_stats <- capm_stats[
  order(capm_stats[, "Annualized Alpha"],
  decreasing=TRUE), ]
# copy capm_stats into env_etf and save to .RData file
assign("capm_stats", capm_stats, envir=env_etf)
save(env_etf, file='etf_data.RData')
capm_stats[, c("Information Ratio", "Annualized Alpha")]
# select ETF symbols
sym_bols <- c("IEF", "DBC", "XLF", "XLP", "XLI", "VXX")
# select and de-mean the returns
re_turns <- na.omit(rutils::env_etf$re_turns[, sym_bols])
in_dex <- index(re_turns)
re_turns <- t(t(re_turns) - sapply(re_turns, sum)/NROW(re_turns))
# re_turns <- lapply(re_turns, function(x) {x - sum(x)/NROW(re_turns)})
# re_turns <- rutils::do_call(cbind, re_turns)
# re_turns <- scale(re_turns, scale=FALSE)
# covariance matrix and variance vector of Returns
cov_mat <- (t(re_turns) %*% re_turns) / (NROW(re_turns)-1)
vari_ance <- diag(cov_mat)
cor_mat <- cov_mat / sqrt(vari_ance)
cor_mat <- t(t(cor_mat) / sqrt(vari_ance))
# cor_mat <- cor(re_turns)
# reorder correlation matrix based on clusters
library(corrplot)
or_der <- corrMatOrder(cor_mat,
        order="hclust",
        hclust.method="complete")
cor_mat <- cor_mat[or_der, or_der]
# plot the correlation matrix
col_ors <- colorRampPalette(c("red", "white", "blue"))
corrplot(cor_mat, title="Correlation Matrix",
    tl.col="black", tl.cex=0.8, mar=c(0,0,1,0),
    method="square", col=col_ors(8),
    cl.offset=0.75, cl.cex=0.7,
    cl.align.text="l", cl.ratio=0.25)
# draw rectangles on the correlation matrix plot
corrRect.hclust(cor_mat, k=NROW(cor_mat) %/% 2,
          method="complete", col="red")
# plot the correlation matrix
col_ors <- colorRampPalette(c("red", "white", "blue"))
corrplot(cor_mat, title="Correlation Matrix",
    tl.col="black", tl.cex=0.8, mar = c(0,0,1,0),
    method="square", col=col_ors(NCOL(cor_mat)),
    cl.offset=0.75, cl.cex=0.7,
    cl.align.text="l", cl.ratio=0.25)
# draw rectangles on the correlation matrix plot
corrRect.hclust(cor_mat, k=NCOL(cor_mat) %/% 2,
    method="complete", col="red")
# create initial vector of portfolio weights
n_weights <- NROW(sym_bols)
weight_s <- rep(1/sqrt(n_weights), n_weights)
names(weight_s) <- sym_bols
# objective function equal to minus portfolio variance
object_ive <- function(weight_s, re_turns) {
  portf_rets <- re_turns %*% weight_s
  -sum(portf_rets*portf_rets) +
    1000*(1 - sum(weight_s*weight_s))^2
}  # end object_ive
# objective for equal weight portfolio
object_ive(weight_s, re_turns)
# compare speed of two methods
summary(microbenchmark(
  trans_pose=t(portf_rets) %*% portf_rets,
  s_um=sum(portf_rets*portf_rets),
  times=10))[, c(1, 4, 5)]
# find weights with maximum variance
optim_run <- optim(par=weight_s,
             fn=object_ive,
             re_turns=re_turns,
             method="L-BFGS-B",
             upper=rep(1.0, n_weights),
             lower=rep(-1.0, n_weights))
# optimal weights and maximum variance
weight_s <- optim_run$par
-object_ive(weight_s, re_turns)
# plot first principal component loadings
barplot(weight_s, names.arg=names(weight_s),
  xlab="", ylab="",
  main="first principal component loadings")
# pc1 weights and returns
weights_1 <- weight_s
pc_1 <- re_turns %*% weights_1
# redefine objective function
object_ive <- function(weight_s, re_turns) {
  portf_rets <- re_turns %*% weight_s
  -sum(portf_rets*portf_rets) +
    1000*(1 - sum(weight_s*weight_s))^2 +
    1000*sum(pc_1*portf_rets)^2
}  # end object_ive
# find second principal component weights
optim_run <- optim(par=weight_s,
             fn=object_ive,
             re_turns=re_turns,
             method="L-BFGS-B",
             upper=rep(1.0, n_weights),
             lower=rep(-1.0, n_weights))
# pc2 weights and returns
weights_2 <- optim_run$par
pc_2 <- re_turns %*% weights_2
sum(pc_1*pc_2)
# plot second principal component loadings
barplot(weights_2, names.arg=names(weights_2),
  xlab="", ylab="",
  main="second principal component loadings")
# calculate eigenvectors and eigenvalues
ei_gen <- eigen(cov_mat)
ei_gen$values[1]
var(pc_1)
(cov_mat %*% weights_1) / weights_1
ei_gen$values[2]
var(pc_2)
(cov_mat %*% weights_2) / weights_2
sum(vari_ance)
sum(ei_gen$values)
barplot(ei_gen$values, # plot eigenvalues
  names.arg=paste0("PC", 1:n_weights),
  las=3, xlab="", ylab="", main="PC eigenvalues (variances)")
# perform principal component analysis PCA
p_ca <- prcomp(re_turns,
         center=TRUE, scale=FALSE)
# plot standard deviations of principal components
barplot(p_ca$sdev,
  names.arg=colnames(p_ca$rotation),
  las=3, xlab="", ylab="",
  main="Principal component volatilities")
# principal component loadings (weights)
p_ca$rotation
# plot loading barplots in multiple panels
par(mfrow=c(n_weights/2,2))
par(mar=c(2, 2, 2, 1), oma=c(0, 0, 0, 0))
for (or_der in 1:n_weights) {
  barplot(p_ca$rotation[, or_der],
  las=3, xlab="", ylab="", main="")
  title(paste0("PC", or_der), line=-2.0,
  col.main="red")
}  # end for
# principal component time series
pca_ts <- xts(re_turns %*% p_ca$rotation,
          order.by=in_dex)
pca_ts <- xts:::cumsum.xts(pca_ts)
# plot principal component time series in multiple panels
par(mfrow=c(n_weights/2,2))
par(mar=c(2, 2, 0, 1), oma=c(0, 0, 0, 0))
ra_nge <- range(pca_ts)
for (or_der in 1:n_weights) {
  plot.zoo(pca_ts[, or_der],
     ylim=ra_nge,
     xlab="", ylab="")
  title(paste0("PC", or_der), line=-2.0)
}  # end for
# invert principal component time series
pca_rets <- re_turns %*% p_ca$rotation
sol_ved <- pca_rets %*% solve(p_ca$rotation)
all.equal(re_turns, sol_ved)
sol_ved <- pca_rets[, 1:3] %*% solve(p_ca$rotation)[1:3, ]
# plot the solved returns
par(mfrow=c(n_weights/2,2))
par(mar=c(2, 2, 0, 1), oma=c(0, 0, 0, 0))
col_names <- colnames(re_turns)
for (or_der in 1:n_weights) {
  plot.zoo(
    cumsum(as.xts(cbind(re_turns[, or_der], sol_ved[, or_der]))),
    plot.type="single", col=c("black", "blue"), xlab="", ylab="")
  legend(x="topright",
   legend=paste0(col_names[or_der], c("", " solved")),
   title="", inset=0.05, cex=1.0, lwd=c(6, 6),
   lty=c(1, 1), col=c("black", "blue"))
}  # end for
library(PerformanceAnalytics)  # load package "PerformanceAnalytics"
# PC returns from rotation and scaled re_turns
re_turns_scaled <- apply(re_turns, 2, scale)
pca_rets <- re_turns_scaled %*% p_ca$rotation
# "x" matrix contains time series of PC returns
dim(p_ca$x)
class(p_ca$x)
head(p_ca$x[, 1:3], 3)
# convert PC matrix to xts and rescale to decimals
pca_rets <- xts(p_ca$x/100,
    order.by=index(re_turns))
library(PerformanceAnalytics)  # load package "PerformanceAnalytics"
chart.CumReturns(
  pca_rets[, 1:3], lwd=2, ylab="",
  legend.loc="topright", main="")
# add title
title(main="ETF cumulative returns", line=-1)
library(PerformanceAnalytics)
# Calculate PC correlation matrix
cor_mat <- cor(pca_rets)
colnames(cor_mat) <- colnames(pca_rets)
rownames(cor_mat) <- colnames(pca_rets)
cor_mat[1:3, 1:3]
table.CAPM(Ra=pca_rets[, 1:3],
    Rb=re_turns[, "VTI"], scale=252)
