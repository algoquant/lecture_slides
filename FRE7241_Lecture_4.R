library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
options(digits=3)
thm <- knit_theme$get("acid")
knit_theme$set(thm)
library(HighFreq)
# define oh_lc series and look-back interval
oh_lc <- rutils::env_etf$VTI["/2011"]
look_back <- 12
# calculate end of month end_points
end_points <- xts::endpoints(oh_lc, on="months")
# start_points are end_points lagged by look_back
len_gth <- NROW(end_points)
start_points <-
  end_points[c(rep_len(1, look_back-1),
         1:(len_gth-look_back+1))]
# create list of look-back intervals
look_backs <- lapply(seq_along(end_points)[-1],
  function(in_dex) start_points[in_dex]:end_points[in_dex])
# second warmup interval spans only two months
warm_up <- oh_lc[look_backs[[3]]]
dim(warm_up)
head(warm_up)
tail(warm_up)
# source EWMA model simu_ewma() from file
source("C:/Develop/R/scripts/ewma_model.R")
# define aggregation function
agg_regate <- function(oh_lc, lamb_das, ...) {
  sapply(lamb_das, function(lamb_da) {
    # simulate EWMA strategy and calculate Sharpe ratio
    re_turns <-
simu_ewma(oh_lc=oh_lc, lamb_da=lamb_da, ...)[, "re_turns"]
    sqrt(260)*sum(re_turns)/sd(re_turns)/NROW(re_turns)
  })  # end sapply
}  # end agg_regate
# define EWMA parameters
win_dow <- 51
lamb_das <- seq(0.001, 0.01, 0.001)
# perform aggregation
agg_regate(oh_lc, lamb_das, win_dow)
# adjust end_points so they are greater than EWMA win_dow
end_points[(end_points > 0) &
  (end_points <= win_dow)] <- win_dow+1
# start_points are end_points lagged by look_back
len_gth <- NROW(end_points)
start_points <- end_points[c(rep_len(1, look_back-1),
         1:(len_gth-look_back+1))]
# create list of look-back intervals
look_backs <- lapply(seq_along(end_points)[-1],
  function(in_dex) start_points[in_dex]:end_points[in_dex])
# perform lapply() loop over look_backs
agg_s <- lapply(look_backs,
  function(look_back, ...) {
    agg_regate(oh_lc[look_back], ...)
    }, lamb_das=lamb_das, win_dow=win_dow)  # end lapply
# rbind list into single xts or matrix
agg_s <- rutils::do_call_rbind(agg_s)
if (!is.xts(agg_s))
  agg_s <- xts(agg_s, order.by=index(oh_lc[end_points]))
roll_agg <- function(x_ts, look_backs, FUN, ...) {
  # perform lapply() loop over look_backs
  agg_s <- lapply(look_backs,
            function(look_back) {
              FUN(x_ts[look_back], ...)
            })  # end lapply
  # rbind list into single xts or matrix
  agg_s <- rutils::do_call_rbind(agg_s)
  if (!is.xts(agg_s))
    agg_s <- xts(agg_s, order.by=
             index(x_ts[unlist(lapply(look_backs, last))]))
  agg_s
}  # end roll_agg
sharpe_ratios <- roll_agg(x_ts=oh_lc,
                    look_backs=look_backs,
                    FUN=agg_regate,
                    lamb_das=lamb_das)
my_var <- 1  # create new object
assign(x="my_var", value=2)  # assign value to existing object
my_var
rm(my_var)  # remove my_var
assign(x="my_var", value=3)  # create new object from name
my_var
# create new object in new environment
new_env <- new.env()  # create new environment
assign("my_var", 3, envir=new_env)  # assign value to name
ls(new_env)  # list objects in "new_env"
new_env$my_var
rm(list=ls())  # delete all objects
sym_bol <- "my_var"  # define symbol containing string "my_var"
assign(sym_bol, 1)  # assign value to "my_var"
ls()
my_var
assign("sym_bol", "new_var")
assign(sym_bol, 1)  # assign value to "new_var"
ls()
sym_bol <- 10
assign(sym_bol, 1)  # can't assign to non-string
rm(list=ls())  # delete all objects
# create individual vectors from column names of EuStockMarkets
for (col_name in colnames(EuStockMarkets)) {
# assign column values to column names
  assign(col_name, EuStockMarkets[, col_name])
}  # end for
ls()
head(DAX)
head(EuStockMarkets[, "DAX"])
identical(DAX, EuStockMarkets[, "DAX"])
# create new environment
test_env <- new.env()
# pass string as name to create new object
assign("my_var1", 2, envir=test_env)
# create new object using $ string referencing
test_env$my_var2 <- 1
# list objects in new environment
ls(test_env)
# reference an object by name
test_env$my_var1
# reference an object by string name using get
get("my_var1", envir=test_env)
# retrieve and assign value to object
assign("my_var1",
 2*get("my_var1", envir=test_env),
 envir=test_env)
get("my_var1", envir=test_env)
# return all objects in an environment
mget(ls(test_env), envir=test_env)
# delete environment
rm(test_env)
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
etf_list <- 
  read.csv(file='C:/Develop/data/etf_list.csv', 
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
etf_series <- do.call(merge,
            as.list(env_etf)[sym_bols])

# extract and merge adjusted prices, subset by symbols
price_s <- do.call(merge,
         lapply(as.list(env_etf)[sym_bols], Ad))

# same, but works only for OHLC series
price_s <- do.call(merge, eapply(env_etf, Ad)[sym_bols])

# drop ".Adjusted" from colnames
colnames(price_s) <-
  sapply(colnames(price_s),
    function(col_name)
strsplit(col_name, split="[.]")[[1]])[1, ]
tail(price_s[, 1:2], 3)

# which objects in global environment are class xts?
unlist(eapply(globalenv(), is.xts))

# save xts to csv file
write.zoo(etf_series,
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
oh_lc <- env_etf$VTI["2009-02/2009-03"]
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
oh_lc <- env_etf$VTI
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
oh_lc <- env_etf$VTI
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
oh_lc <- env_etf$VTI["2009-02/2009-03"]
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
oh_lc <- env_etf$VTI["2010-04/2010-05"]
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
library(quantmod)
# calculate VTI volume-weighted average price
VTI_vwap <- TTR::VWAP(price=Ad(env_etf$VTI),
    volume=Vo(env_etf$VTI), n=10)
# calculate XLF volume-weighted average price
XLF_vwap <- TTR::VWAP(price=Ad(env_etf$XLF),
    volume=Vo(env_etf$XLF), n=10)
# open plot graphics device
x11()
# define plot area with two horizontal panels
par(mfrow=c(2, 1))
# plot in top panel
invisible(chart_Series(
  x=env_etf$VTI["2009-02/2009-04"], name="VTI"))
add_TA(VTI_vwap["2009-02/2009-04"], lwd=2, on=1, col='blue')
# plot in bottom panel
invisible(chart_Series(
  x=env_etf$XLF["2009-02/2009-04"], name="XLF"))
add_TA(XLF_vwap["2009-02/2009-04"], lwd=2, on=1, col='blue')
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
  file="C:/Develop/data/sp500_Yahoo.csv",
  row.names=FALSE)
library(HighFreq)  # load package HighFreq
# load data frame of S&P500 constituents from CSV file
sp_500 <- read.csv(file="C:/Develop/data/sp500_Yahoo.csv",
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
for (na_me in sp_500$names) {
  cat("processing: ", na_me, "\n")
  rutils::get_symbols(na_me,
   env_out=env_sp500, start_date="1990-01-01")
}  # end for
save(env_sp500, file="C:/Develop/data/sp500.RData")
chart_Series(x=env_sp500$BRK_B["2016/"], TA="add_Vo()",
       name="BRK-B stock")
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
chart_Series(price_s["2016", 1:4], name="AAPL OHLC prices")
# add trade volume in extra panel
add_TA(price_s["2016", 5])
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
  file="C:/Develop/data/sp500_quandl.csv",
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
save(env_sp500, file="C:/Develop/data/sp500.RData")
chart_Series(x=env_sp500$XOM["2016/"], TA="add_Vo()",
       name="XOM stock")
library(HighFreq)
re_turns <- na.omit(env_etf$re_turns)
# specify regression formula
reg_formula <- XLP ~ VTI
# perform regression
reg_model <- lm(reg_formula, data=re_turns)
# plot scatterplot of returns
plot(reg_formula, data=re_turns)
title(main="Regression XLP ~ VTI", line=-1)
# add regression line
abline(reg_model, lwd=2, col="red")
reg_model_sum <- summary(reg_model)
coef(reg_model_sum)
# Durbin-Watson test autocorrelation residuals
library(lmtest)
dwtest(reg_model)
library(quantmod)  # load quantmod
library(lmtest)  # load lmtest
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
         p_dw=dwtest(reg_model)$p.value)
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
beta_s <- rollapply(env_etf$re_turns, width=252,
  FUN=function(design_matrix)
  coef(lm(reg_formula, data=design_matrix))[2],
  by=22, by.column=FALSE, align="right")
beta_s <- na.omit(beta_s)
# plot beta_s in x11() window
x11(width=(wid_th <- 6), height=(hei_ght <- 4))
chart_Series(x=beta_s,
  name=paste("rolling betas", format(reg_formula)))
# perform daily rolling beta regressions in parallel
library(roll)
beta_s <- roll_lm(x=env_etf$re_turns[, "VTI"],
            y=env_etf$re_turns[, "XLP"],
            width=252)$coefficients
# compare speed of rollapply() versus roll_lm()
library(microbenchmark)
da_ta <- env_etf$re_turns["2012", c("VTI", "XLP")]
summary(microbenchmark(
  rollapply=rollapply(da_ta, width=22,
FUN=function(design_matrix)
coef(lm(reg_formula, data=design_matrix))[2],
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
library(quantmod)
#Perform pair-wise correlation analysis
# Calculate correlation matrix
corr_matrix <- cor(re_turns)
colnames(corr_matrix) <- colnames(re_turns)
rownames(corr_matrix) <- colnames(re_turns)
# Reorder the correlation matrix based on clusters
# Calculate permutation vector
library(corrplot)
corr_order <- corrMatOrder(corr_matrix,
        order="hclust",
        hclust.method="complete")
# Apply permutation vector
corr_matrix_ordered <-
  corr_matrix[corr_order, corr_order]
# Plot the correlation matrix
col3 <- colorRampPalette(c("red", "white", "blue"))
corrplot(corr_matrix_ordered,
    tl.col="black", tl.cex=0.8,
    method="square", col=col3(8),
    cl.offset=0.75, cl.cex=0.7,
    cl.align.text="l", cl.ratio=0.25)
# Draw rectangles on the correlation matrix plot
corrRect.hclust(corr_matrix_ordered,
k=13, method="complete", col="red")
# convert correlation matrix into distance object
data_dist <- as.dist(1-corr_matrix_ordered)
# Perform hierarchical clustering analysis
data_cluster <- hclust(data_dist)
plot(data_cluster, ann=FALSE, xlab="", ylab="")
title("Dissimilarity = 1-Correlation",
line=-0.5)
par(oma=c(1,0,1,0), mgp=c(2,1,0), mar=c(2,1,2,1), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # set plot panels
#Perform principal component analysis PCA
re_turns <- na.omit(env_etf$re_turns)
etf_pca <- prcomp(re_turns, center=TRUE, scale=TRUE)
barplot(etf_pca$sdev[1:10],
  names.arg=colnames(etf_pca$rotation)[1:10],
  las=3, ylab="STDEV", xlab="PCVec",
  main="PCA Explain VAR")
# Show first three principal component loadings
head(etf_pca$rotation[,1:3], 3)
# Permute second principal component loadings by size
pca_vec2 <- as.matrix(
  etf_pca$rotation[order(etf_pca$rotation[, 2],
  decreasing=TRUE), 2])
colnames(pca_vec2) <- "pca2"
head(pca_vec2, 3)
# The option las=3 rotates the names.arg labels
barplot(as.vector(pca_vec2),
  names.arg=rownames(pca_vec2),
  las=3, ylab="Loadings",
  xlab="Symbol", main="Loadings pca2")
par(oma=c(1,0,1,0), mgp=c(2,1,0), mar=c(2,1,2,1), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(3,1))  # set plot panels
# get list of principal component vectors
pca_vecs <- lapply(1:3, function(in_dex) {
  pca_vec <- as.matrix(
    etf_pca$rotation[
    order(etf_pca$rotation[, in_dex],
    decreasing=TRUE), in_dex])
  colnames(pca_vec) <- paste0("pca", in_dex)
  pca_vec
})  # end sapply
names(pca_vecs) <- c("pca1", "pca2", "pca3")
# The option las=3 rotates the names.arg labels
for (in_dex in 1:3) {
  barplot(as.vector(pca_vecs[[in_dex]]),
  names.arg=rownames(pca_vecs[[in_dex]]),
  las=3, ylab="", xlab="",
  main=paste("Loadings",
    colnames(pca_vecs[[in_dex]])))
}  # end for
library(PerformanceAnalytics)  # load package "PerformanceAnalytics"
# PC returns from rotation and scaled re_turns
re_turns_scaled <- apply(re_turns, 2, scale)
pca_rets <- re_turns_scaled %*% etf_pca$rotation
# "x" matrix contains time series of PC returns
dim(etf_pca$x)
class(etf_pca$x)
head(etf_pca$x[, 1:3], 3)
# convert PC matrix to xts and rescale to decimals
pca_rets <- xts(etf_pca$x/100,
    order.by=index(re_turns))
library(PerformanceAnalytics)  # load package "PerformanceAnalytics"
chart.CumReturns(
  pca_rets[, 1:3], lwd=2, ylab="",
  legend.loc="topright", main="")
# add title
title(main="ETF cumulative returns", line=-1)
library(PerformanceAnalytics)
# Calculate PC correlation matrix
corr_matrix <- cor(pca_rets)
colnames(corr_matrix) <- colnames(pca_rets)
rownames(corr_matrix) <- colnames(pca_rets)
corr_matrix[1:3, 1:3]
table.CAPM(Ra=pca_rets[, 1:3],
    Rb=re_turns[, "VTI"], scale=252)
