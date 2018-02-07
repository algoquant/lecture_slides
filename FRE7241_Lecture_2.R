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
# download data for sym_bols into env_etf from Alpha Vantage
getSymbols.av(sym_bols, adjust=TRUE, env=env_etf,
  output.size="full", api.key="T7JPW54ES8G75310")
# getSymbols(sym_bols, env=env_etf, adjust=TRUE, from="2005-01-03")
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
re_turns <- rutils::do_call(cbind, re_turns)
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
sapply(mget(sym_bols, envir=env_etf), object.size)
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
library(HighFreq)  # load package HighFreq
# load data frame of S&P500 constituents from CSV file
sp_500 <- read.csv(file="C:/Develop/R/lecture_slides/data/sp500_WRDS_08-30-17.csv", stringsAsFactors=FALSE)
sym_bols <- sp_500$co_tic
env_sp500 <- new.env()  # new environment for data
# remove all files (if necessary)
rm(list=ls(env_sp500), envir=env_sp500)
# download in while loop from Alpha Vantage and copy into environment
down_loaded <- sym_bols %in% ls(env_sp500)
it_er <- 0
while (((NROW(down_loaded) - sum(down_loaded)) > 0) & (it_er<10)) {
  # Boolean vector of symbols already downloaded
  down_loaded <- sym_bols %in% ls(env_sp500)
  # download data and copy it into environment
  for (sym_bol in sym_bols[!down_loaded]) {
    cat("processing: ", sym_bol, "\n")
    tryCatch(  # with error handler
  getSymbols(sym_bol, adjust=TRUE, env=env_sp500,
             output.size="full", api.key="T7JPW54ES8G75310"),
# error handler captures error condition
error=function(error_cond) {
  print(paste("error handler: ", error_cond))
},  # end error handler
finally=print(paste("sym_bol=", sym_bol))
    )  # end tryCatch
  }  # end for
  it_er <- it_er + 1
  Sys.sleep(2*60)
}  # end while
library(HighFreq)  # load package HighFreq
# rename "LOW" colnames to "L_OWES"
colnames(env_sp500$LOW) <-
  sapply(colnames(env_sp500$LOW),
    function(col_name) {
col_name <- strsplit(col_name, split="[.]")[[1]]
paste("L_OWES", col_name[2], sep=".")
    })
assign("L_OWES", env_sp500$LOW, envir=env_sp500)
rm(LOW, envir=env_sp500)
# adjust all OHLC in environment using vector of strings
for (sym_bol in ls(env_sp500)) {
  assign(sym_bol,
    adjustOHLC(get(x=sym_bol, envir=env_sp500), use.Adjusted=TRUE),
    envir=env_sp500)
}  # end for
save(env_sp500,
  file="C:/Develop/R/lecture_slides/data/sp500.RData")
chart_Series(x=env_sp500$L_OWES["2017-06/"],
  TA="add_Vo()", name="LOWES stock")
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
price_s <- Cl(rutils::env_etf$VTI)
end_points <- seq_along(price_s)  # define end points
len_gth <- NROW(end_points)
look_back <- 22  # number of data points per look-back interval
# start_points are multi-period lag of end_points
start_points <- c(rep_len(1, look_back-1),
    end_points[1:(len_gth-look_back+1)])
# define list of look-back intervals for aggregations over past
look_backs <- lapply(seq_along(end_points),
  function(in_dex) {
    start_points[in_dex]:end_points[in_dex]
})  # end lapply
# define aggregation function
agg_regate <- function(x_ts) c(max=max(x_ts), min=min(x_ts))
# perform aggregations over look_backs list
agg_regations <- sapply(look_backs,
    function(look_back) agg_regate(price_s[look_back])
)  # end sapply
# coerce agg_regations into matrix and transpose it
if (is.vector(agg_regations))
  agg_regations <- t(agg_regations)
agg_regations <- t(agg_regations)
# coerce agg_regations into xts series
agg_regations <- xts(agg_regations,
               order.by=index(price_s[end_points]))
library(HighFreq)  # load package HighFreq
# perform aggregations over look_backs list
agg_regations <- lapply(look_backs,
    function(look_back) agg_regate(price_s[look_back])
)  # end lapply
# rbind list into single xts or matrix
agg_regations <- rutils::do_call_rbind(agg_regations)
# convert into xts
agg_regations <- xts::xts(agg_regations,
    order.by=index(price_s))
agg_regations <- cbind(agg_regations, price_s)
# plot aggregations with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("black", "red", "green")
x11()
chart_Series(agg_regations, theme=plot_theme,
       name="price aggregations")
legend("top", legend=colnames(agg_regations),
  bg="white", lty=c(1, 1, 1), lwd=c(6, 6, 6),
  col=plot_theme$col$line.col, bty="n")
# library(HighFreq)  # load package HighFreq
# define functional for rolling aggregations
roll_agg <- function(x_ts, look_back, FUN, ...) {
# define end points at every period
  end_points <- seq_along(x_ts)
  len_gth <- NROW(end_points)
# define starting points as lag of end_points
  start_points <- c(rep_len(1, look_back-1),
    end_points[1:(len_gth-look_back+1)])
# define list of look-back intervals for aggregations over past
look_backs <- lapply(seq_along(end_points),
  function(in_dex) {
    start_points[in_dex]:end_points[in_dex]
})  # end lapply
# perform aggregations over look_backs list
  agg_regations <- lapply(look_backs,
    function(look_back) FUN(x_ts[look_back], ...)
  )  # end lapply
# rbind list into single xts or matrix
  agg_regations <- rutils::do_call_rbind(agg_regations)
# coerce agg_regations into xts series
  if (!is.xts(agg_regations))
    agg_regations <- xts(agg_regations, order.by=index(x_ts))
  agg_regations
}  # end roll_agg
# define aggregation function
agg_regate <- function(x_ts)
  c(max=max(x_ts), min=min(x_ts))
# perform aggregations over rolling interval
agg_regations <- roll_agg(price_s, look_back=look_back,
              FUN=agg_regate)
class(agg_regations)
dim(agg_regations)
# library(HighFreq)  # load package HighFreq
# define aggregation function that returns a vector
agg_vector <- function(x_ts)
  c(max=max(x_ts), min=min(x_ts))
# define aggregation function that returns an xts
agg_xts <- function(x_ts)
  xts(t(c(max=max(x_ts), min=min(x_ts))),
order.by=end(x_ts))
# benchmark the speed of aggregation functions
library(microbenchmark)
summary(microbenchmark(
  agg_vector=roll_agg(price_s, look_back=look_back,
              FUN=agg_vector),
  agg_xts=roll_agg(price_s, look_back=look_back,
              FUN=agg_xts),
  times=10))[, c(1, 4, 5)]
# library(HighFreq)  # load package HighFreq
# define aggregation function that returns a single value
agg_regate <- function(x_ts)  max(x_ts)
# perform aggregations over a rolling interval
agg_regations <- xts:::rollapply.xts(price_s, width=look_back,
              FUN=agg_regate, align="right")
# perform aggregations over a rolling interval
library(PerformanceAnalytics)  # load package PerformanceAnalytics
agg_regations <- apply.rolling(price_s,
              width=look_back, FUN=agg_regate)
# benchmark the speed of the functionals
library(microbenchmark)
summary(microbenchmark(
  roll_agg=roll_agg(price_s, look_back=look_back,
              FUN=max),
  roll_xts=xts:::rollapply.xts(price_s, width=look_back,
                 FUN=max, align="right"),
  apply_rolling=apply.rolling(price_s,
                        width=look_back, FUN=max),
  times=10))[, c(1, 4, 5)]
# library(HighFreq)  # load package HighFreq
# rolling sum using cumsum()
roll_sum <- function(x_ts, look_back) {
  cum_sum <- cumsum(na.omit(x_ts))
  out_put <- cum_sum - lag(x=cum_sum, k=look_back)
  out_put[1:look_back, ] <- cum_sum[1:look_back, ]
  colnames(out_put) <- paste0(colnames(x_ts), "_stdev")
  out_put
}  # end roll_sum
agg_regations <- roll_sum(price_s, look_back=look_back)
# define list of look-back intervals for aggregations over past
look_backs <- lapply(seq_along(end_points),
  function(in_dex) {
    start_points[in_dex]:end_points[in_dex]
})  # end lapply
# perform rolling aggregations using apply loop
agg_regations <- sapply(look_backs,
    function(look_back) sum(price_s[look_back])
)  # end sapply
head(agg_regations)
tail(agg_regations)
# benchmark the speed of both methods
library(microbenchmark)
summary(microbenchmark(
  roll_sum=roll_sum(price_s, look_back=look_back),
  s_apply=sapply(look_backs,
    function(look_back) sum(price_s[look_back])),
  times=10))[, c(1, 4, 5)]
# library(TTR)  # load package TTR
# benchmark the speed of TTR::runSum
library(microbenchmark)
summary(microbenchmark(
  cum_sum=cumsum(coredata(price_s)),
  roll_sum=rutils::roll_sum(price_s, win_dow=look_back),
  run_sum=TTR::runSum(price_s, n=look_back),
  times=10))[, c(1, 4, 5)]
library(RcppRoll)  # load package RcppRoll
wid_th <- 22  # number of data points per look-back interval
# calculate rolling sum using rutils
prices_mean <-
  rutils::roll_sum(price_s, win_dow=wid_th)
# calculate rolling sum using RcppRoll
prices_mean <- RcppRoll::roll_sum(price_s,
              align="right", n=wid_th)
# benchmark the speed of RcppRoll::roll_sum
library(microbenchmark)
summary(microbenchmark(
  cum_sum=cumsum(coredata(price_s)),
  rcpp_roll_sum=RcppRoll::roll_sum(price_s, n=wid_th),
  roll_sum=rutils::roll_sum(price_s, win_dow=wid_th),
  times=10))[, c(1, 4, 5)]
# calculate EWMA sum using RcppRoll
weight_s <- exp(0.1*1:wid_th)
prices_mean <- RcppRoll::roll_mean(price_s,
align="right", n=wid_th, weights=weight_s)
prices_mean <- cbind(price_s,
  rbind(coredata(price_s[1:(look_back-1), ]), prices_mean))
colnames(prices_mean) <- c("SPY", "SPY EWMA")
# plot EWMA prices with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("black", "red")
x11()
chart_Series(prices_mean, theme=plot_theme,
       name="EWMA prices")
legend("top", legend=colnames(prices_mean),
 bg="white", lty=c(1, 1), lwd=c(6, 6),
 col=plot_theme$col$line.col, bty="n")
# library(HighFreq)  # load package HighFreq
library(caTools)  # load package "caTools"
# get documentation for package "caTools"
packageDescription("caTools")  # get short description
help(package="caTools")  # load help page
data(package="caTools")  # list all datasets in "caTools"
ls("package:caTools")  # list all objects in "caTools"
detach("package:caTools")  # remove caTools from search path
# median filter
look_back <- 11
price_s <- Cl(HighFreq::SPY["2012-02-01/2012-04-01"])
med_ian <- runmed(x=price_s, k=look_back)
# vector of rolling volatility
vol_at <- runsd(x=price_s, k=look_back,
          endrule="constant", align="center")
# vector of rolling quantiles
quan_tiles <- runquantile(x=price_s,
            k=look_back, probs=0.9,
            endrule="constant",
            align="center")
library(HighFreq)  # load package HighFreq
# extract daily closing VTI prices
price_s <- Cl(rutils::env_etf$VTI)
# define number of data points per interval
look_back <- 22
# number of look_backs that fit over price_s
n_row <- NROW(price_s)
num_agg <- n_row %/% look_back
# if n_row==look_back*num_agg then whole number
# of look_backs fit over price_s
end_points <- (1:num_agg)*look_back
# if (n_row > look_back*num_agg)
# then stub interval at beginning
end_points <-
  n_row-look_back*num_agg + (0:num_agg)*look_back
# stub interval at end
end_points <- c((1:num_agg)*look_back, n_row)
# plot data and endpoints as vertical lines
plot_theme <- chart_theme()
plot_theme$col$line.col <- "blue"
chart_Series(price_s, theme=plot_theme,
  name="prices with endpoints as vertical lines")
abline(v=end_points, col="red")
# library(HighFreq)  # load package HighFreq
# indices of last observations in each hour
end_points <- xts::endpoints(price_s, on="hours")
head(end_points)
# extract the last observations in each hour
head(price_s[end_points, ])
# library(HighFreq)  # load package HighFreq
end_points <- # define end_points with beginning stub
  n_row-look_back*num_agg + (0:num_agg)*look_back
len_gth <- NROW(end_points)
# start_points are single-period lag of end_points
start_points <- c(1, end_points[1:(len_gth-1)]+1)
# define list of look-back intervals for aggregations over past
look_backs <- lapply(seq_along(end_points),
  function(in_dex) {
    start_points[in_dex]:end_points[in_dex]
})  # end lapply
look_backs[[1]]
look_backs[[2]]
# perform sapply() loop over look_backs list
agg_regations <- sapply(look_backs,
    function(look_back) {
x_ts <- price_s[look_back]
c(max=max(x_ts), min=min(x_ts))
  })  # end sapply
# coerce agg_regations into matrix and transpose it
if (is.vector(agg_regations))
  agg_regations <- t(agg_regations)
agg_regations <- t(agg_regations)
# coerce agg_regations into xts series
agg_regations <- xts(agg_regations,
    order.by=index(price_s[end_points]))
head(agg_regations)
# plot aggregations with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("red", "green")
chart_Series(agg_regations, theme=plot_theme,
       name="price aggregations")
legend("top", legend=colnames(agg_regations),
  bg="white", lty=c(1, 1), lwd=c(6, 6),
  col=plot_theme$col$line.col, bty="n")
# library(HighFreq)  # load package HighFreq
# perform lapply() loop over look_backs list
agg_regations <- lapply(look_backs,
    function(look_back) {
x_ts <- price_s[look_back]
c(max=max(x_ts), min=min(x_ts))
    })  # end lapply
# rbind list into single xts or matrix
agg_regations <- rutils::do_call_rbind(agg_regations)
# coerce agg_regations into xts series
agg_regations <- xts(agg_regations,
    order.by=index(price_s[end_points]))
head(agg_regations)
# plot aggregations with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("red", "green")
chart_Series(agg_regations, theme=plot_theme,
       name="price aggregations")
legend("top", legend=colnames(agg_regations),
  bg="white", lty=c(1, 1), lwd=c(6, 6),
  col=plot_theme$col$line.col, bty="n")
# library(HighFreq)  # load package HighFreq
# define functional for rolling aggregations over end_points
roll_agg <- function(x_ts, end_points, FUN, ...) {
  len_gth <- NROW(end_points)
# start_points are single-period lag of end_points
  start_points <- c(1, end_points[1:(len_gth-1)]+1)
# perform aggregations over look_backs list
  agg_regations <- lapply(look_backs,
    function(look_back) FUN(x_ts[look_back], ...))  # end lapply
# rbind list into single xts or matrix
  agg_regations <- rutils::do_call_rbind(agg_regations)
  if (!is.xts(agg_regations))
    agg_regations <-  # coerce agg_regations into xts series
    xts(agg_regations, order.by=index(x_ts[end_points]))
  agg_regations
}  # end roll_agg
# apply sum() over end_points
agg_regations <-
  roll_agg(price_s, end_points=end_points, FUN=sum)
agg_regations <-
  period.apply(price_s, INDEX=end_points, FUN=sum)
# benchmark the speed of aggregation functions
summary(microbenchmark(
  roll_agg=roll_agg(price_s, end_points=end_points, FUN=sum),
  period_apply=period.apply(price_s, INDEX=end_points, FUN=sum),
  times=10))[, c(1, 4, 5)]
agg_regations <- period.sum(price_s, INDEX=end_points)
head(agg_regations)
# library(HighFreq)  # load package HighFreq
# load package HighFreq
library(HighFreq)
# extract closing minutely prices
price_s <- Cl(HighFreq::SPY["2012-02-01/2012-04-01"])
# apply "mean" over daily periods
agg_regations <- apply.daily(price_s, FUN=sum)
head(agg_regations)
library(HighFreq)  # load package HighFreq
end_points <- # define end_points with beginning stub
  n_row-look_back*num_agg + (0:num_agg)*look_back
len_gth <- NROW(end_points)
num_points <- 4  # number of end points in look-back interval
# start_points are multi-period lag of end_points
start_points <- c(rep_len(1, num_points-1),
  end_points[1:(len_gth-num_points+1)])
# define list of look-back intervals for aggregations over past
look_backs <- lapply(seq_along(end_points),
  function(in_dex) {
    start_points[in_dex]:end_points[in_dex]
})  # end lapply
# perform lapply() loop over look_backs list
agg_regations <- lapply(look_backs,
    function(look_back) {
x_ts <- price_s[look_back]
c(max=max(x_ts), min=min(x_ts))
    })  # end lapply
# rbind list into single xts or matrix
agg_regations <- rutils::do_call_rbind(agg_regations)
# coerce agg_regations into xts series
agg_regations <- xts(agg_regations,
    order.by=index(price_s[end_points]))
# plot aggregations with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("red", "green")
chart_Series(agg_regations, theme=plot_theme,
       name="price aggregations")
legend("top", legend=colnames(agg_regations),
  bg="white", lty=c(1, 1), lwd=c(6, 6),
  col=plot_theme$col$line.col, bty="n")
library(HighFreq)  # load package HighFreq
agg_regations <- cbind(price_s, agg_regations)
tail(agg_regations, 22)
agg_regations <- na.omit(zoo::na.locf(agg_regations))
# plot aggregations with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("black", "red", "green")
chart_Series(agg_regations, theme=plot_theme,
       name="price aggregations")
legend("top", legend=colnames(agg_regations),
  bg="white", lty=c(1, 1, 1), lwd=c(6, 6, 6),
  col=plot_theme$col$line.col, bty="n")
set.seed(1121)  # reset random number generator
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
library(zoo)  # load package zoo
# create zoo time series of random returns
in_dex <- Sys.Date() + 0:365
zoo_series <-
  zoo(rnorm(NROW(in_dex)), order.by=in_dex)
# create monthly dates
dates_agg <- as.Date(as.yearmon(index(zoo_series)))
# perform monthly mean aggregation
zoo_agg <- aggregate(zoo_series, by=dates_agg,
               FUN=mean)
# merge with original zoo - union of dates
zoo_agg <- cbind(zoo_series, zoo_agg)
# replace NA's using locf
zoo_agg <- na.locf(zoo_agg)
# extract aggregated zoo
zoo_agg <- zoo_agg[index(zoo_series), 2]
# library(HighFreq)  # load package HighFreq
# plot original and aggregated cumulative returns
plot(cumsum(zoo_series), xlab="", ylab="")
lines(cumsum(zoo_agg), lwd=2, col="red")
# add legend
legend("topright", inset=0.05, cex=0.8,
 title="Aggregated Prices",
 leg=c("orig prices", "agg prices"),
 lwd=2, bg="white", col=c("black", "red"))
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# perform monthly mean aggregation
zoo_agg <- aggregate(zoo_series, by=dates_agg,
               FUN=mean)
# merge with original zoo - union of dates
zoo_agg <- cbind(zoo_series, zoo_agg)
# replace NA's using linear interpolation
zoo_agg <- na.approx(zoo_agg)
# extract interpolated zoo
zoo_agg <- zoo_agg[index(zoo_series), 2]
# plot original and interpolated zoo
plot(cumsum(zoo_series), xlab="", ylab="")
lines(cumsum(zoo_agg), lwd=2, col="red")
# add legend
legend("topright", inset=0.05, cex=0.8, title="Interpolated Prices",
 leg=c("orig prices", "interpol prices"), lwd=2, bg="white",
 col=c("black", "red"))
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# "mean" aggregation over interval with width=11
zoo_mean <- rollapply(zoo_series, width=11,
                FUN=mean, align="right")
# merge with original zoo - union of dates
zoo_mean <- cbind(zoo_series, zoo_mean)
# replace NA's using na.locf
zoo_mean <- na.locf(zoo_mean, fromLast=TRUE)
# extract mean zoo
zoo_mean <- zoo_mean[index(zoo_series), 2]
# plot original and interpolated zoo
plot(cumsum(zoo_series), xlab="", ylab="")
lines(cumsum(zoo_mean), lwd=2, col="red")
# add legend
legend("topright", inset=0.05, cex=0.8, title="Mean Prices",
 leg=c("orig prices", "mean prices"), lwd=2, bg="white",
 col=c("black", "red"))
library(parallel)  # load package parallel
# get short description
packageDescription("parallel")
# load help page
help(package="parallel")
# list all objects in "parallel"
ls("package:parallel")
library(parallel)  # load package parallel
# calculate number of available cores
num_cores <- detectCores() - 1
# define function that pauses execution
paws <- function(x, sleep_time) {
  Sys.sleep(sleep_time)
  x
}  # end paws
# perform parallel loop under Mac-OSX or Linux
paw_s <- mclapply(1:10, paws, mc.cores=num_cores,
          sleep_time=0.01)
# initialize compute cluster under Windows
clus_ter <- makeCluster(num_cores)
# perform parallel loop under Windows
paw_s <- parLapply(clus_ter, 1:10, paws,
           sleep_time=0.01)
library(microbenchmark)  # load package microbenchmark
# compare speed of lapply versus parallel computing
summary(microbenchmark(
  l_apply=lapply(1:10, paws, sleep_time=0.01),
  parl_apply=
    parLapply(clus_ter, 1:10, paws, sleep_time=0.01),
  times=10)
)[, c(1, 4, 5)]
# stop R processes over cluster under Windows
stopCluster(clus_ter)
library(parallel)  # load package parallel
# calculate number of available cores
num_cores <- detectCores() - 1
# initialize compute cluster under Windows
clus_ter <- makeCluster(num_cores)
# define function that pauses execution
paws <- function(x, sleep_time) {
  Sys.sleep(sleep_time)
  x
}  # end paws
# compare speed of lapply with parallel computing
iter_ations <- 3:10
compute_times <- sapply(iter_ations,
  function(max_iterations, sleep_time) {
    out_put <- summary(microbenchmark(
lapply=lapply(1:max_iterations, paws,
              sleep_time=sleep_time),
parallel=parLapply(clus_ter, 1:max_iterations,
        paws, sleep_time=sleep_time),
times=10))[, c(1, 4)]
    structure(out_put[, 2],
        names=as.vector(out_put[, 1]))
    }, sleep_time=0.01)
compute_times <- t(compute_times)
rownames(compute_times) <- iter_ations
library(parallel)  # load package parallel
plot(x=rownames(compute_times),
     y=compute_times[, "lapply"],
     type="l", lwd=2, col="blue",
     main="Compute times",
     xlab="number of iterations in loop", ylab="",
     ylim=c(0, max(compute_times[, "lapply"])))
lines(x=rownames(compute_times),
y=compute_times[, "parallel"], lwd=2, col="green")
legend(x="topleft", legend=colnames(compute_times),
 inset=0.1, cex=1.0, bg="white",
 lwd=2, lty=c(1, 1), col=c("blue", "green"))
library(parallel)  # load package parallel
# calculate number of available cores
num_cores <- detectCores() - 1
# initialize compute cluster under Windows
clus_ter <- makeCluster(num_cores)
# define large matrix
mat_rix <- matrix(rnorm(7*10^5), ncol=7)
# define aggregation function over column of matrix
agg_regate <- function(col_umn) {
  out_put <- 0
  for (in_dex in 1:NROW(col_umn))
    out_put <- out_put + col_umn[in_dex]
  out_put
}  # end agg_regate
# perform parallel aggregations over columns of matrix
agg_regations <-
  parCapply(clus_ter, mat_rix, agg_regate)
# compare speed of apply with parallel computing
summary(microbenchmark(
  ap_ply=apply(mat_rix, MARGIN=2, agg_regate),
  parl_apply=
    parCapply(clus_ter, mat_rix, agg_regate),
  times=10)
)[, c(1, 4, 5)]
# stop R processes over cluster under Windows
stopCluster(clus_ter)
library(parallel)  # load package parallel
# calculate number of available cores
num_cores <- detectCores() - 1
# initialize compute cluster under Windows
clus_ter <- makeCluster(num_cores)
ba_se <- 2
# fails because child processes don't know ba_se:
parLapply(clus_ter, 2:4,
    function(exponent) ba_se^exponent)
# ba_se passed to child via dots ... argument:
parLapply(clus_ter, 2:4,
    function(exponent, ba_se) ba_se^exponent,
    ba_se=ba_se)
# ba_se passed to child via clusterExport:
clusterExport(clus_ter, "ba_se")
parLapply(clus_ter, 2:4,
    function(exponent) ba_se^exponent)
# fails because child processes don't know zoo::index():
parSapply(clus_ter, c("VTI", "IEF", "DBC"),
    function(sym_bol)
      NROW(index(get(sym_bol, envir=rutils::env_etf))))
# zoo function referenced using "::" in child process:
parSapply(clus_ter, c("VTI", "IEF", "DBC"),
    function(sym_bol)
      NROW(zoo::index(get(sym_bol, envir=rutils::env_etf))))
# package zoo loaded in child process:
parSapply(clus_ter, c("VTI", "IEF", "DBC"),
    function(sym_bol) {
      stopifnot("package:zoo" %in% search() || require("zoo", quietly=TRUE))
      NROW(index(get(sym_bol, envir=rutils::env_etf)))
    })  # end parSapply
# stop R processes over cluster under Windows
stopCluster(clus_ter)
library(parallel)  # load package parallel
# calculate number of available cores
num_cores <- detectCores() - 1
# initialize compute cluster under Windows
clus_ter <- makeCluster(num_cores)
# set seed for cluster under Windows
# doesn't work: set.seed(1121)
clusterSetRNGStream(clus_ter, 1121)
# perform parallel loop under Windows
out_put <- parLapply(clus_ter, 1:70, rnorm, n=100)
sum(unlist(out_put))
# stop R processes over cluster under Windows
stopCluster(clus_ter)
# perform parallel loop under Mac-OSX or Linux
out_put <- mclapply(1:10, rnorm, mc.cores=num_cores, n=100)
library(HighFreq)  # load package HighFreq
# calculate open, close, and lagged prices
op_en <- Op(rutils::env_etf$VTI)
cl_ose <- Cl(rutils::env_etf$VTI)
close_adj <- (cl_ose - as.numeric(cl_ose[1, ]))
prices_lag <- rutils::lag_it(cl_ose)
# define aggregation interval and calculate VWAP
look_back <- 150
VTI_vwap <- HighFreq::roll_vwap(rutils::env_etf$VTI,
        look_back=look_back)
# calculate VWAP indicator
in_dic <- sign(cl_ose - VTI_vwap)
# determine dates right after VWAP has crossed prices
trade_dates <- (rutils::diff_it(in_dic) != 0)
trade_dates <- which(trade_dates) + 1
# plot prices and VWAP
chart_Series(x=cl_ose,
  name="VTI prices", col="orange")
add_TA(VTI_vwap, on=1, lwd=2, col="blue")
legend("top", legend=c("VTI", "VWAP"),
  bg="white", lty=1, lwd=6,
  col=c("orange", "blue"), bty="n")
library(HighFreq)  # load package HighFreq
# calculate positions, either: -1, 0, or 1
position_s <- NA*numeric(NROW(rutils::env_etf$VTI))
position_s[1] <- 0
position_s[trade_dates] <- in_dic[trade_dates]
position_s <- na.locf(position_s)
position_s <- xts(position_s, order.by=index((rutils::env_etf$VTI)))
position_lagged <- rutils::lag_it(position_s)
# calculate daily profits and losses
pnl_s <- position_lagged*(cl_ose - prices_lag)
pnl_s[trade_dates] <- position_lagged[trade_dates] *
  (op_en[trade_dates] - prices_lag[trade_dates]) +
  position_s[trade_dates] *
  (cl_ose[trade_dates] - op_en[trade_dates])
# calculate annualized Sharpe ratio of strategy returns
sqrt(252)*sum(pnl_s)/sd(pnl_s)/NROW(pnl_s)
# plot prices and VWAP
pnl_s <- xts(cumsum(pnl_s), order.by=index((rutils::env_etf$VTI)))
close_adj <- (cl_ose - as.numeric(cl_ose[1, ]))
chart_Series(x=close_adj, name="VTI prices", col="orange")
add_TA(pnl_s, on=1, lwd=2, col="blue")
add_TA(position_s > 0, on=-1,
 col="lightgreen", border="lightgreen")
add_TA(position_s < 0, on=-1,
 col="lightgrey", border="lightgrey")
legend("top", legend=c("VTI", "VWAP strategy"),
 inset=0.1, bg="white", lty=1, lwd=6,
 col=c("orange", "blue"), bty="n")
library(HighFreq)  # load package HighFreq
# select OHLC data
oh_lc <- rutils::env_etf$VTI
# calculate close prices
cl_ose <- quantmod::Cl(oh_lc)
close_adj <- (cl_ose - as.numeric(cl_ose[1, ]))
# define length for weights and decay parameter
wid_th <- 251
lamb_da <- 0.01
# calculate EWMA prices
weight_s <- exp(-lamb_da*1:wid_th)
weight_s <- weight_s/sum(weight_s)
ew_ma <- stats::filter(cl_ose, filter=weight_s, sides=1)
ew_ma[1:(wid_th-1)] <- ew_ma[wid_th]
ew_ma <- xts(cbind(cl_ose, ew_ma),
       order.by=index(oh_lc))
colnames(ew_ma) <- c("VTI", "VTI EWMA")
# plot EWMA prices with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue")
chart_Series(ew_ma["2007/2010"], theme=plot_theme,
       name="EWMA prices")
legend("bottomleft", legend=colnames(ew_ma),
 inset=0.1, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")
# determine dates right after EWMA has crossed prices
in_dic <- sign(cl_ose - ew_ma[, 2])
trade_dates <- (rutils::diff_it(in_dic) != 0)
trade_dates <- which(trade_dates) + 1
# calculate positions, either: -1, 0, or 1
position_s <- rep(NA_integer_, NROW(cl_ose))
position_s[1] <- 0
position_s[trade_dates] <-
  rutils::lag_it(in_dic)[trade_dates]
position_s <- na.locf(position_s)
position_s <- xts(position_s, order.by=index(oh_lc))
# plot EWMA prices with position shading
chart_Series(ew_ma["2007/2010"], theme=plot_theme,
       name="EWMA prices")
add_TA(position_s > 0, on=-1,
 col="lightgreen", border="lightgreen")
add_TA(position_s < 0, on=-1,
 col="lightgrey", border="lightgrey")
legend("bottomleft", legend=colnames(ew_ma),
 inset=0.1, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")
# bid_offer is equal to 10 bps for liquid ETFs
bid_offer <- 0.001
# calculate open and lagged prices
op_en <- Op(oh_lc)
prices_lag <- rutils::lag_it(cl_ose)
position_lagged <- rutils::lag_it(position_s)
# calculate transaction costs
cost_s <- 0.0*position_s
cost_s[trade_dates] <- 0.5*bid_offer*abs(position_lagged[trade_dates] - position_s[trade_dates])*op_en[trade_dates]
# calculate daily profits and losses
re_turns <- position_lagged*(cl_ose - prices_lag)
re_turns[trade_dates] <-
  position_lagged[trade_dates] *
  (op_en[trade_dates] - prices_lag[trade_dates]) +
  position_s[trade_dates] *
  (cl_ose[trade_dates] - op_en[trade_dates]) -
  cost_s
# calculate annualized Sharpe ratio of strategy returns
sqrt(252)*sum(re_turns)/sd(re_turns)/NROW(re_turns)
pnl_s <- cumsum(re_turns)
pnl_s <- cbind(close_adj, pnl_s)
colnames(pnl_s) <- c("VTI", "EWMA PnL")
# plot EWMA PnL with position shading
chart_Series(pnl_s, theme=plot_theme,
       name="Performance of EWMA Strategy")
add_TA(position_s > 0, on=-1,
 col="lightgreen", border="lightgreen")
add_TA(position_s < 0, on=-1,
 col="lightgrey", border="lightgrey")
legend("top", legend=colnames(pnl_s),
 inset=0.05, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")
simu_ewma <- function(oh_lc, lamb_da=0.01, wid_th=251, bid_offer=0.001, tre_nd=1) {
  # calculate EWMA prices
  weight_s <- exp(-lamb_da*1:wid_th)
  weight_s <- weight_s/sum(weight_s)
  cl_ose <- quantmod::Cl(oh_lc)
  ew_ma <- stats::filter(as.numeric(cl_ose), filter=weight_s, sides=1)
  ew_ma[1:(wid_th-1)] <- ew_ma[wid_th]
  # determine dates right after EWMA has crossed prices
  in_dic <- tre_nd*xts::xts(sign(as.numeric(cl_ose) - ew_ma), order.by=index(oh_lc))
  trade_dates <- (rutils::diff_it(in_dic) != 0)
  trade_dates <- which(trade_dates) + 1
  trade_dates <- trade_dates[trade_dates<NROW(oh_lc)]
  # calculate positions, either: -1, 0, or 1
  position_s <- rep(NA_integer_, NROW(cl_ose))
  position_s[1] <- 0
  position_s[trade_dates] <- rutils::lag_it(in_dic)[trade_dates]
  position_s <- xts::xts(na.locf(position_s), order.by=index(oh_lc))
  op_en <- quantmod::Op(oh_lc)
  prices_lag <- rutils::lag_it(cl_ose)
  position_lagged <- rutils::lag_it(position_s)
  # calculate transaction costs
  cost_s <- 0.0*position_s
  cost_s[trade_dates] <- 0.5*bid_offer*abs(position_lagged[trade_dates] - position_s[trade_dates])*op_en[trade_dates]
  # calculate daily profits and losses
  re_turns <- position_lagged*(cl_ose - prices_lag)
  re_turns[trade_dates] <- position_lagged[trade_dates] * (op_en[trade_dates] - prices_lag[trade_dates]) + position_s[trade_dates] * (cl_ose[trade_dates] - op_en[trade_dates]) - cost_s
  out_put <- cbind(position_s, re_turns)
  colnames(out_put) <- c("positions", "returns")
  out_put
}  # end simu_ewma
source("C:/Develop/R/lecture_slides/scripts/ewma_model.R")
lamb_das <- seq(0.0001, 0.05, 0.005)
# perform lapply() loop over lamb_das
re_turns <- lapply(lamb_das, function(lamb_da) {
  # simulate EWMA strategy and calculate re_turns
  simu_ewma(oh_lc=oh_lc, lamb_da=lamb_da,
      wid_th=wid_th)[, "returns"]
})  # end lapply
re_turns <- rutils::do_call(cbind, re_turns)
colnames(re_turns) <- paste0("lambda=", lamb_das)
# plot EWMA strategies with custom line colors
column_s <- seq(1, NCOL(re_turns), by=3)
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NROW(column_s))
chart_Series(cumsum(re_turns[, column_s]),
  theme=plot_theme, name="Cumulative Returns of EWMA Strategies")
legend("topleft", legend=colnames(re_turns[, column_s]),
  inset=0.1, bg="white", cex=0.8, lwd=rep(6, NCOL(re_turns)),
  col=plot_theme$col$line.col, bty="n")
# initialize compute cluster under Windows
library(parallel)
clus_ter <- makeCluster(detectCores()-1)
clusterExport(clus_ter,
  varlist=c("oh_lc", "wid_th", "simu_ewma"))
# perform parallel loop over lamb_das under Windows
re_turns <- parLapply(clus_ter, lamb_das,
        function(lamb_da) {
  library(quantmod)
  # simulate EWMA strategy and calculate re_turns
  simu_ewma(oh_lc=oh_lc,
    lamb_da=lamb_da, wid_th=wid_th)[, "returns"]
})  # end parLapply
# perform parallel loop over lamb_das under Mac-OSX or Linux
re_turns <- mclapply(lamb_das,
        function(lamb_da) {
  library(quantmod)
  # simulate EWMA strategy and calculate re_turns
  simu_ewma(oh_lc=oh_lc,
    lamb_da=lamb_da, wid_th=wid_th)[, "returns"]
})  # end mclapply
stopCluster(clus_ter)  # stop R processes over cluster under Windows
re_turns <- rutils::do_call(cbind, re_turns)
colnames(re_turns) <- paste0("lambda=", lamb_das)
sharpe_ratios <- sqrt(252)*sapply(re_turns, function(x_ts) {
  # calculate annualized Sharpe ratio of strategy returns
  sum(x_ts)/sd(x_ts)
})/NROW(re_turns)  # end sapply
plot(x=lamb_das, y=sharpe_ratios, t="l",
     main="Performance of EWMA trend-following strategies
     as function of the decay parameter lambda")
trend_returns <- re_turns
trend_sharpe_ratios <- sharpe_ratios
# simulate best performing strategy
ewma_trend <- simu_ewma(oh_lc=oh_lc,
  lamb_da=lamb_das[which.max(sharpe_ratios)],
  wid_th=wid_th)
position_s <- ewma_trend[, "positions"]
pnl_s <- cumsum(ewma_trend[, "returns"])
pnl_s <- cbind(close_adj, pnl_s)
colnames(pnl_s) <- c("VTI", "EWMA PnL")
# plot EWMA PnL with position shading
plot_theme$col$line.col <- c("orange", "blue")
chart_Series(pnl_s, theme=plot_theme,
       name="Performance of Trend-following EWMA Strategy")
add_TA(position_s > 0, on=-1,
 col="lightgreen", border="lightgreen")
add_TA(position_s < 0, on=-1,
 col="lightgrey", border="lightgrey")
legend("top", legend=colnames(pnl_s),
  inset=0.05, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")
source("C:/Develop/R/lecture_slides/scripts/ewma_model.R")
lamb_das <- seq(0.05, 1.0, 0.05)
# perform lapply() loop over lamb_das
re_turns <- lapply(lamb_das, function(lamb_da) {
  # simulate EWMA strategy and calculate re_turns
  simu_ewma(oh_lc=oh_lc, lamb_da=lamb_da,
      wid_th=wid_th, tre_nd=(-1))[, "returns"]
})  # end lapply
re_turns <- rutils::do_call(cbind, re_turns)
colnames(re_turns) <- paste0("lambda=", lamb_das)
# plot EWMA strategies with custom line colors
column_s <- seq(1, NCOL(re_turns), by=4)
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NROW(column_s))
chart_Series(cumsum(re_turns[, column_s]),
  theme=plot_theme, name="Cumulative Returns of Mean-reverting EWMA Strategies")
legend("topleft", legend=colnames(re_turns[, column_s]),
  inset=0.1, bg="white", cex=0.8, lwd=rep(6, NCOL(re_turns)),
  col=plot_theme$col$line.col, bty="n")
sharpe_ratios <- sqrt(252)*sapply(re_turns, function(x_ts) {
  # calculate annualized Sharpe ratio of strategy returns
  sum(x_ts)/sd(x_ts)
})/NROW(re_turns)  # end sapply
plot(x=lamb_das, y=sharpe_ratios, t="l",
     main="Performance of EWMA mean-reverting strategies
     as function of the decay parameter lambda")
revert_returns <- re_turns
revert_sharpe_ratios <- sharpe_ratios
# simulate best performing strategy
ewma_revert <- simu_ewma(oh_lc=oh_lc,
  lamb_da=lamb_das[which.max(sharpe_ratios)],
  wid_th=wid_th, tre_nd=(-1))
position_s <- ewma_revert[, "positions"]
pnl_s <- cumsum(ewma_revert[, "returns"])
pnl_s <- cbind(close_adj, pnl_s)
colnames(pnl_s) <- c("VTI", "EWMA PnL")
# plot EWMA PnL with position shading
plot_theme$col$line.col <- c("orange", "blue")
chart_Series(pnl_s, theme=plot_theme,
       name="Performance of Mean-reverting EWMA Strategy")
add_TA(position_s > 0, on=-1,
 col="lightgreen", border="lightgreen")
add_TA(position_s < 0, on=-1,
 col="lightgrey", border="lightgrey")
legend("top", legend=colnames(pnl_s),
  inset=0.05, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")
# calculate correlation between trend-following and mean-reverting strategies
trend_ing <- ewma_trend[, "returns"]
colnames(trend_ing) <- "trend"
revert_ing <- ewma_revert[, "returns"]
colnames(revert_ing) <- "revert"
close_rets <- rutils::diff_it(cl_ose)
corr_matrix <- cor(cbind(trend_ing, revert_ing, close_rets))
corr_matrix
# calculate combined strategy
com_bined <- trend_ing + revert_ing
colnames(com_bined) <- "combined"
# calculate annualized Sharpe ratio of strategy returns
sqrt(252)*sapply(
  cbind(close_rets, trend_ing, revert_ing, com_bined),
  function(x_ts) sum(x_ts)/sd(x_ts))/NROW(com_bined)  # end sapply
pnl_s <- cumsum(com_bined)
pnl_s <- cbind(close_adj, pnl_s)
colnames(pnl_s) <- c("VTI", "EWMA combined PnL")
chart_Series(pnl_s, theme=plot_theme,
       name="Performance of Combined EWMA Strategies")
add_TA(cumsum(trend_ing), on=1, lwd=2, col="green")
add_TA(cumsum(revert_ing), on=1, lwd=2, col="magenta2")
legend("topleft", legend=c(colnames(pnl_s), "trending", "reverting"),
 inset=0.05, bg="white", lty=rep(1, 4), lwd=rep(4, 4),
 col=c(plot_theme$col$line.col, "green", "magenta2"), bty="n")
sharpe_ratios <- c(trend_sharpe_ratios, revert_sharpe_ratios)
weight_s <- sharpe_ratios
weight_s[weight_s<0] <- 0
weight_s <- weight_s/sum(weight_s)
re_turns <- cbind(trend_returns, revert_returns)
avg_returns <- re_turns %*% weight_s
avg_returns <- xts(avg_returns, order.by=index(re_turns))
pnl_s <- cumsum(avg_returns)
pnl_s <- cbind(close_adj, pnl_s)
colnames(pnl_s) <- c("VTI", "EWMA PnL")
# plot EWMA PnL without position shading
chart_Series(pnl_s, theme=plot_theme,
       name="Performance of Ensemble EWMA Strategy")
legend("top", legend=colnames(pnl_s),
  inset=0.05, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")
library(HighFreq)
# end of month end_points
end_points <- rutils::calc_endpoints(re_turns,
          inter_val="months")
len_gth <- NROW(end_points)
# define 12-month look-back interval
look_back <- 12
# start_points are end_points lagged by look-back interval
start_points <- c(rep_len(1, look_back-1),
  end_points[1:(len_gth-look_back+1)])
# Perform loop over end_points and calculate aggregations
# agg_fun <- function(re_turns) sum(re_turns)/sd(re_turns)
agg_fun <- function(re_turns) sum(re_turns)
back_aggs <- sapply(1:(len_gth-1), function(it_er) {
  sapply(re_turns[start_points[it_er]:end_points[it_er]], agg_fun)
})  # end sapply
back_aggs <- t(back_aggs)
# define forward (future) endpoints
fwd_points <- end_points[c(2:len_gth, len_gth)]
fwd_rets <- sapply(1:(len_gth-1), function(it_er) {
  sapply(re_turns[(end_points[it_er]+1):fwd_points[it_er]], sum)
})  # end sapply
fwd_rets <- t(fwd_rets)
# calculate weight_s proportional to back_aggs
weight_s <- back_aggs
weight_s[weight_s<0] <- 0
# scale weight_s so their sum is equal to 1
weight_s <- weight_s/rowSums(weight_s)
# set NA values to zero
weight_s[is.na(weight_s)] <- 0
sum(is.na(weight_s))
in_dex <- index(re_turns[end_points[-len_gth]])
trend_weights <- rowMeans(weight_s[, 1:NCOL(trend_returns)])
revert_weights <- rowMeans(weight_s[, -(1:NCOL(trend_returns))])
diff_weights <- xts(trend_weights-revert_weights, order.by=in_dex)
close_adj <- (cl_ose - as.numeric(cl_ose[1, ]))
# de-mean weight_s so their sum is equal to 0
# weight_s <- weight_s - rowMeans(weight_s)
# find best and worst EWMA Strategies in each period
bes_t <- apply(weight_s, 1, which.max)
wors_t <- apply(weight_s, 1, which.min)
# plot the mean weights of EWMA Strategies
zoo::plot.zoo(cbind(diff_weights,
  close_adj[end_points[-len_gth]]),
  oma = c(3, 0, 3, 0), mar = c(0, 4, 0, 1),
  xlab=NULL, ylab=c("diff weights", "VTI"),
  main="Trend minus Revert Weights of EWMA strategies")
best_worst <- xts(cbind(bes_t, wors_t), order.by=in_dex)
zoo::plot.zoo(best_worst,
  oma = c(3, 0, 3, 0), mar = c(0, 4, 0, 1),
  xlab=NULL, ylab=c("best EWMA", "worst EWMA"),
  main="Best and Worst EWMA strategies")
# calculate backtest returns
pnl_s <- rowSums(weight_s * fwd_rets)
pnl_s <- xts(pnl_s, order.by=in_dex)
colnames(pnl_s) <- "ewma momentum"
close_rets <- rutils::diff_it(cl_ose[in_dex])
cor(cbind(pnl_s, close_rets))
pnl_s <- cumsum(pnl_s)
# plot the backtest
chart_Series(x=close_adj[end_points[-len_gth]],
  name="Back-test of EWMA strategies", col="orange")
add_TA(pnl_s, on=1, lwd=2, col="blue")
legend("top", legend=c("VTI", "EWMA"),
 inset=0.1, bg="white", lty=1, lwd=6,
 col=c("orange", "blue"), bty="n")
# shad_e <- xts(index(pnl_s) < as.Date("2008-01-31"), order.by=index(pnl_s))
# add_TA(shad_e, on=-1, col="lightgrey", border="lightgrey")
# text(x=7, y=0, labels="warmup period")
# define back-test functional
back_test <- function(re_turns, price_s, agg_fun=sum,
    look_back=12, re_balance="months", bid_offer=0.001,
    end_points=rutils::calc_endpoints(re_turns, inter_val=re_balance),
    with_weights=FALSE, ...) {
  stopifnot("package:quantmod" %in% search() || require("quantmod", quietly=TRUE))
  # define start_points and forward (future) endpoints
  len_gth <- NROW(end_points)
  n_col <- NCOL(re_turns)
  start_points <- c(rep_len(1, look_back-1), end_points[1:(len_gth-look_back+1)])
  fwd_points <- end_points[c(2:len_gth, len_gth)]
  # Perform loop over end_points and calculate aggregations
  agg_s <- sapply(1:(len_gth-1), function(it_er) {
    c(back_aggs=sapply(re_turns[start_points[it_er]:end_points[it_er]], agg_fun, ...),  # end sapply
    fwd_rets=sapply(re_turns[(end_points[it_er]+1):fwd_points[it_er]], sum))  # end sapply
  })  # end sapply
  agg_s <- t(agg_s)
  # Select aggregations over look-back and look-forward intervals
  back_aggs <- agg_s[, 1:n_col]
  fwd_rets <- agg_s[, n_col+1:n_col]
  # Calculate portfolio weights
  weight_s <- back_aggs/rowSums(abs(back_aggs))
  weight_s[is.na(weight_s)] <- 0
  colnames(weight_s) <- colnames(re_turns)
  # Calculate profits and losses
  end_points <- end_points[-len_gth]
  price_s <- price_s[end_points, ]
  pnl_s <- rowSums(weight_s * fwd_rets / price_s)
  pnl_s <- xts(pnl_s, index(re_turns[end_points]))
  pnl_s <- cumsum(pnl_s)
  colnames(pnl_s) <- "pnls"
  # Calculate transaction costs
  if (NCOL(price_s) < NCOL(weight_s))
    price_s <- as.numeric(price_s)
  cost_s <- 0.5*bid_offer*price_s*abs(rutils::diff_it(weight_s))
  cost_s <- rowSums(cost_s)
  pnl_s <- (pnl_s - cost_s)
  if (with_weights)
    cbind(pnl_s, weight_s)
  else
    pnl_s
}  # end back_test
# Calculate ETF prices and simple returns
sym_bols <- c("VTI", "IEF", "DBC")
price_s <- rutils::env_etf$price_s[, sym_bols]
price_s <- zoo::na.locf(price_s)
price_s <- na.omit(price_s)
re_turns <- rutils::diff_it(price_s)
# Perform back-test
agg_fun <-
  function(re_turns) sum(re_turns)/sd(re_turns)
pnl_s <- back_test(re_turns=re_turns, price_s=price_s,
  re_balance="weeks", look_back=20, agg_fun=agg_fun,
  with_weights=TRUE)
zoo::plot.zoo(cbind(cl_ose[index(pnl_s)], pnl_s),
  oma = c(3, 1, 3, 0), mar = c(0, 4, 0, 1), nc=1,
  xlab=NULL, main="ETF Momentum Strategy")
look_backs <- seq(10, 150, by=10)
pro_files <- sapply(look_backs, function(x) {
  last(back_test(re_turns=re_turns, price_s=price_s,
    re_balance="weeks", look_back=x, agg_fun=agg_fun))
})  # end sapply
plot(y=pro_files, x=look_backs, t="l",
  main="Strategy PnL as function of look_back",
  xlab="look_back (weeks)", ylab="pnl")
# bind model returns with VTI
da_ta <- as.numeric(cl_ose[index(pnl_s)][1, 1])
da_ta <- cbind(cl_ose[index(pnl_s)], da_ta*pnl_s[, 1]+da_ta)
colnames(da_ta) <- c("VTI", "momentum")
# plot momentum strategy with VTI
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue")
chart_Series(da_ta, theme=plot_theme, lwd=c(2, 2),
       name="Momentum PnL")
legend("topleft", legend=colnames(da_ta),
  inset=0.1, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")
# combine momentum strategy with static
da_ta <- cbind(da_ta, 0.5* (da_ta[, "VTI"] + da_ta[, "momentum"]))
colnames(da_ta) <- c("VTI", "momentum", "combined")
# calculate strategy annualized Sharpe ratios
sapply(da_ta, function(cumu_lative) {
  x_ts <- na.omit(diff(log(cumu_lative)))
  sqrt(252)*sum(x_ts)/sd(x_ts)/NROW(x_ts)
})  # end sapply
# calculate strategy correlations
cor(na.omit(diff(log(da_ta))))
# plot momentum strategy combined with VTI
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue", "green")
chart_Series(da_ta, theme=plot_theme,
       name="Momentum strategy combined with VTI")
legend("topleft", legend=colnames(da_ta),
  inset=0.1, bg="white", lty=c(1, 1, 1), lwd=c(6, 6, 6),
  col=plot_theme$col$line.col, bty="n")
# Define all-weather symbols and weights
weight_s <- c(0.30, 0.55, 0.15)
all_weather <- re_turns %*% weight_s
all_weather <- cumsum(all_weather)
all_weather <- xts(all_weather, index(re_turns))[index(pnl_s)]
all_weather <- all_weather + as.numeric(cl_ose[index(pnl_s)][1, 1])
colnames(all_weather) <- "all_weather"
# combine momentum strategy with all-weather
da_ta <- cbind(da_ta, all_weather)
# calculate strategy annualized Sharpe ratios
sapply(da_ta, function(cumu_lative) {
  x_ts <- na.omit(diff(log(cumu_lative)))
  sqrt(252)*sum(x_ts)/sd(x_ts)/NROW(x_ts)
})  # end sapply
# calculate strategy correlations
cor(na.omit(diff(log(pnl_s))))
# plot momentum strategy, combined, and all-weather
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue", "green", "violet")
chart_Series(da_ta, theme=plot_theme, lwd=2, name="Momentum PnL")
legend("topleft", legend=colnames(da_ta),
  inset=0.1, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")
