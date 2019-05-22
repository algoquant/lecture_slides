library(knitr)
opts_chunk$set(prompt=TRUE, eval=FALSE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
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

detach("package:tseries")  # Remove tseries from search path

par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
load(file="C:/Develop/R/lecture_slides/data/zoo_data.RData")
# get start and end dates
in_dex <- time(ts_stx_adj)
e_nd <- in_dex[NROW(in_dex)]
st_art <- round((4*e_nd + in_dex[1])/5)
# Plot using plotOHLC
plotOHLC(window(ts_stx_adj,
          start=st_art,
          end=e_nd)[, 1:4],
   xlab="", ylab="")
title(main="MSFT OHLC Prices")

par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
library(lubridate)  # load lubridate
# get start and end dates of zoo_series
start_date <- decimal_date(start(zoo_stx))
end_date <- decimal_date(end(zoo_stx))
# Calculate frequency of zoo_stx
fre_quency <-
  NROW(zoo_stx)/(end_date-start_date)
# Extract data from zoo_stx
da_ta <- zoo::coredata(
  window(zoo_stx, start=as.Date("2015-01-01"),
   end=end(zoo_stx)))
# Create ts object using ts()
ts_stx <- ts(data=da_ta,
  start=decimal_date(as.Date("2015-01-01")),
  frequency=fre_quency)
seqplot.ts(x=ts_stx[, 1], y=ts_stx[, 4], xlab="", ylab="")
title(main="MSFT Open and Close Prices", line=-1)

library(tseries)  # load package tseries
# Calculate maximum drawdown
maxdrawdown(zoo_stx_adj[, "AdjClose"])
max_drawd <- maxdrawdown(zoo_stx_adj[, "AdjClose"])
index(zoo_stx_adj)[max_drawd$from]
index(zoo_stx_adj)[max_drawd$to]
# Calculate Sharpe ratio
sharpe(zoo_stx_adj[, "AdjClose"])
# Calculate Sterling ratio
sterling(as.numeric(zoo_stx_adj[, "AdjClose"]))

zoo_stx <- suppressWarnings(  # load MSFT data
  get.hist.quote(instrument="MSFT",
           start=Sys.Date()-365,
           end=Sys.Date(),
           origin="1970-01-01")
)  # End suppressWarnings
class(zoo_stx)
dim(zoo_stx)
tail(zoo_stx, 4)

# Calculate Sharpe ratio
sharpe(zoo_stx[, "Close"], r=0.01)
# Add title
plot(zoo_stx[, "Close"], xlab="", ylab="")
title(main="MSFT Close Prices", line=-1)

library(tseries)  # load package tseries
zoo_stx <- suppressWarnings(  # load MSFT data
  get.hist.quote(instrument="MSFT",
           start=Sys.Date()-365,
           end=Sys.Date(),
           origin="1970-01-01")
)  # End suppressWarnings
class(zoo_stx)
dim(zoo_stx)
tail(zoo_stx, 4)

# Calculate Sharpe ratio
sharpe(zoo_stx[, "Close"], r=0.01)
# Add title
plot(zoo_stx[, "Close"], xlab="", ylab="")
title(main="MSFT Close Prices", line=-1)

library(tseries)  # load package tseries
zoo_stx <- suppressWarnings(  # load MSFT data
  get.hist.quote(instrument="MSFT",
           start=Sys.Date()-365,
           end=Sys.Date(),
           origin="1970-01-01")
)  # End suppressWarnings
class(zoo_stx)
dim(zoo_stx)
tail(zoo_stx, 4)

# Calculate Sharpe ratio
sharpe(zoo_stx[, "Close"], r=0.01)
# Add title
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
# Remove quantmod from search path
detach("package:quantmod")

library(quantmod)
# Plot OHLC candlechart with volume
chartSeries(etf_env$VTI["2014-11"],
      name="VTI",
      theme=chartTheme("white"))
# Plot OHLC bar chart with volume
chartSeries(etf_env$VTI["2014-11"],
      type="bars",
      name="VTI",
      theme=chartTheme("white"))

library(quantmod)
# Plot OHLC candlechart with volume
chartSeries(etf_env$VTI["2008-11/2009-04"],
      name="VTI")
# Redraw plot only for Feb-2009, with white theme
reChart(subset="2009-02",
  theme=chartTheme("white"))

library(quantmod)
# Candlechart with Bollinger Bands
chartSeries(etf_env$VTI["2014"],
      TA="addBBands(): addBBands(draw='percent'): addVo()",
      name="VTI with Bollinger Bands",
      theme=chartTheme("white"))
# Candlechart with two Moving Averages
chartSeries(etf_env$VTI["2014"],
      TA="addVo(): addEMA(10): addEMA(30)",
      name="VTI with Moving Averages",
      theme=chartTheme("white"))
# Candlechart with Commodity Channel Index
chartSeries(etf_env$VTI["2014"],
      TA="addVo(): addBBands(): addCCI()",
      name="VTI with Technical Indicators",
      theme=chartTheme("white"))

library(quantmod)
library(TTR)
oh_lc <- rutils::etf_env$VTI["2009-02/2009-03"]
VTI_adj <- Ad(oh_lc); VTI_vol <- Vo(oh_lc)
# Calculate volume-weighted average price
VTI_vwap <- TTR::VWAP(price=VTI_adj,
volume=VTI_vol, n=10)
# Plot OHLC candlechart with volume
chartSeries(oh_lc, name="VTI plus VWAP",
      theme=chartTheme("white"))
# Add VWAP to main plot
addTA(ta=VTI_vwap, on=1, col='red')
# Add price minus VWAP in extra panel
addTA(ta=(VTI_adj-VTI_vwap), col='red')

library(quantmod)
library(TTR)
oh_lc <- rutils::etf_env$VTI
VTI_adj <- Ad(oh_lc)
VTI_vol <- Vo(oh_lc)
VTI_vwap <- TTR::VWAP(price=VTI_adj, volume=VTI_vol, n=10)
VTI_adj <- VTI_adj["2009-02/2009-03"]
oh_lc <- oh_lc["2009-02/2009-03"]
VTI_vwap <- VTI_vwap["2009-02/2009-03"]
# Plot OHLC candlechart with volume
chartSeries(oh_lc, name="VTI plus VWAP shaded",
      theme=chartTheme("white"))
# Add VWAP to main plot
addTA(ta=VTI_vwap, on=1, col='red')
# Add price minus VWAP in extra panel
addTA(ta=(VTI_adj-VTI_vwap), col='red')
# Add background shading of areas
addTA((VTI_adj-VTI_vwap) > 0, on=-1,
col="lightgreen", border="lightgreen")
addTA((VTI_adj-VTI_vwap) < 0, on=-1,
col="lightgrey", border="lightgrey")
# Add vertical and horizontal lines at VTI_vwap minimum
addLines(v=which.min(VTI_vwap), col='red')
addLines(h=min(VTI_vwap), col='red')

library(quantmod)
library(TTR)
oh_lc <- rutils::etf_env$VTI
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
# Add price minus VWAP in extra panel
add_TA(VTI_adj-VTI_vwap, col='red')
# Add background shading of areas
add_TA((VTI_adj-VTI_vwap) > 0, on=-1,
col="lightgreen", border="lightgreen")
add_TA((VTI_adj-VTI_vwap) < 0, on=-1,
col="lightgrey", border="lightgrey")
# Add vertical and horizontal lines
abline(v=which.min(VTI_vwap), col='red')
abline(h=min(VTI_vwap), col='red')

library(quantmod)
oh_lc <- rutils::etf_env$VTI["2009-02/2009-03"]
# Extract plot object
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
oh_lc <- rutils::etf_env$VTI["2010-04/2010-05"]
# Extract, modify theme, format tick marks "%b %d"
plot_theme <- chart_theme()
plot_theme$format.labels <- "%b %d"
# Create plot object
ch_ob <- chart_Series(x=oh_lc,
                theme=plot_theme, plot=FALSE)
# Extract ylim using accessor function
y_lim <- ch_ob$get_ylim()
y_lim[[2]] <- structure(
  range(Ad(oh_lc)) + c(-1, 1),
  fixed=TRUE)
# modify plot object to reduce y-axis range
ch_ob$set_ylim(y_lim)  # use setter function
# Render the plot
plot(ch_ob)

library(HighFreq)
# Calculate VTI and XLF volume-weighted average price
VTI_vwap <-
  TTR::VWAP(price=Ad(rutils::etf_env$VTI),
      volume=Vo(rutils::etf_env$VTI), n=10)
XLF_vwap <-
  TTR::VWAP(price=Ad(rutils::etf_env$XLF),
      volume=Vo(rutils::etf_env$XLF), n=10)
# open graphics device, and define
# Plot area with two horizontal panels
x11(); par(mfrow=c(2, 1))
ch_ob <- chart_Series(  # Plot in top panel
  x=etf_env$VTI["2009-02/2009-04"],
  name="VTI", plot=FALSE)
add_TA(VTI_vwap["2009-02/2009-04"],
 lwd=2, on=1, col='blue')
ch_ob <- chart_Series(  # Plot in bottom panel
  x=etf_env$XLF["2009-02/2009-04"],
  name="XLF", plot=FALSE)
add_TA(XLF_vwap["2009-02/2009-04"],
 lwd=2, on=1, col='blue')

library(dygraphs)
# Calculate volume-weighted average price
oh_lc <- rutils::etf_env$VTI
VTI_vwap <- TTR::VWAP(price=quantmod::Ad(oh_lc),
    volume=quantmod::Vo(oh_lc), n=20)
# Add VWAP to OHLC  data
oh_lc <- cbind(oh_lc[, c(1:3, 6)],
         VTI_vwap)["2009-02/2009-04"]
# Create dygraphs object
dy_graph <- dygraphs::dygraph(oh_lc)
# Convert dygraphs object to candlestick plot
dy_graph <- dygraphs::dyCandlestick(dy_graph)
# Render candlestick plot
dy_graph
# Candlestick plot using pipes syntax
dygraphs::dygraph(oh_lc) %>% dyCandlestick()
# Candlestick plot without using pipes syntax
dygraphs::dyCandlestick(dygraphs::dygraph(oh_lc))

# Create candlestick plot with background shading
in_dex <- index(oh_lc)
in_dic <-
  rutils::diff_it(oh_lc[, 4] > oh_lc[, "VWAP"])
in_dic <- rbind(cbind(which(in_dic==1), 1),
  cbind(which(in_dic==(-1)), -1))
in_dic <- in_dic[order(in_dic[, 1]), ]
in_dic <- rbind(c(1, -in_dic[1, 2]), in_dic,
  c(NROW(oh_lc), -in_dic[NROW(in_dic), 2]))
in_dic <-
  data.frame(in_dex[in_dic[, 1]], in_dic[, 2])
# Create dygraphs object
dy_graph <- dygraphs::dygraph(oh_lc) %>%
  dyCandlestick()
# Add shading
for (i in 1:(NROW(in_dic)-1)) {
  if (in_dic[i, 2] == 1)
    dy_graph <- dy_graph %>% dyShading(from=in_dic[i, 1], to=in_dic[i+1, 1], color="lightgreen")
  else
    dy_graph <- dy_graph %>% dyShading(from=in_dic[i, 1], to=in_dic[i+1, 1], color="antiquewhite")
}  # End for
# Render plot
dy_graph

library(dygraphs)
# Prepare VTI and IEF prices
price_s <- cbind(Ad(rutils::etf_env$VTI),
           Ad(rutils::etf_env$IEF))
price_s <- na.omit(price_s)
col_names <- rutils::get_name(colnames(price_s))
colnames(price_s) <- col_names
# dygraphs plot with two y-axes
library(dygraphs)
dygraphs::dygraph(price_s, main=paste(col_names, collapse=" and ")) %>%
  dyAxis(name="y", label="VTI", independentTicks=TRUE) %>%
  dyAxis(name="y2", label="IEF", independentTicks=TRUE) %>%
  dySeries(name="VTI", axis="y", col="red") %>%
  dySeries(name="IEF", axis="y2", col="blue")

# load package qmao
library(qmao)
# get documentation for package qmao
# get short description
packageDescription("qmao")
# load help page
help(package="qmao")
# list all datasets in "qmao"
data(package="qmao")
# list all objects in "qmao"
ls("package:qmao")
# Remove qmao from search path
detach("package:qmao")

# define daily volatility and growth rate
vol_at <- 0.01; dri_ft <- 0.0; len_gth <- 1000
# Simulate geometric Brownian motion
re_turns <- vol_at*rnorm(len_gth) +
  dri_ft - vol_at^2/2
price_s <- exp(cumsum(re_turns))
plot(price_s, type="l",
     xlab="periods", ylab="prices",
     main="geometric Brownian motion")

# Simulate geometric Brownian motion
vol_at <- 0.01/sqrt(48)
dri_ft <- 0.0
len_gth <- 1e4
in_dex <- seq(from=as.POSIXct(paste(Sys.Date()-250, "09:30:00")),
  length.out=len_gth, by="30 min")
price_s <- xts(exp(cumsum(vol_at*rnorm(len_gth) + dri_ft - vol_at^2/2)),
  order.by=in_dex)
price_s <- cbind(price_s,
  volume=sample(x=10*(2:18), size=len_gth, replace=TRUE))
# Aggregate to daily OHLC data
oh_lc <- xts::to.daily(price_s)
quantmod::chart_Series(oh_lc, name="random prices")
# dygraphs candlestick plot using pipes syntax
library(dygraphs)
dygraphs::dygraph(oh_lc[, 1:4]) %>%
  dyCandlestick()
# dygraphs candlestick plot without using pipes syntax
dygraphs::dyCandlestick(dygraphs::dygraph(oh_lc[, 1:4]))

# define daily volatility and growth rate
vol_at <- 0.01; dri_ft <- 0.0; len_gth <- 5000
path_s <- 10
# Simulate multiple paths of geometric Brownian motion
price_s <- matrix(vol_at*rnorm(path_s*len_gth) +
    dri_ft - vol_at^2/2, nc=path_s)
price_s <- exp(matrixStats::colCumsums(price_s))
# Create xts time series
price_s <- xts(price_s, order.by=seq.Date(Sys.Date()-NROW(price_s)+1, Sys.Date(), by=1))
# Plot xts time series
col_ors <- colorRampPalette(c("red", "blue"))(NCOL(price_s))
col_ors <- col_ors[order(order(price_s[NROW(price_s), ]))]
par(mar=c(3, 3, 2, 2), oma=c(0, 0, 0, 0))
plot.zoo(price_s, main="Multiple paths of geometric Brownian motion",
   xlab=NA, ylab=NA, plot.type="single", col=col_ors)

# define daily volatility and growth rate
vol_at <- 0.01; dri_ft <- 0.0; len_gth <- 10000
path_s <- 100
# Simulate multiple paths of geometric Brownian motion
price_s <- matrix(vol_at*rnorm(path_s*len_gth) +
    dri_ft - vol_at^2/2, nc=path_s)
price_s <- exp(matrixStats::colCumsums(price_s))
# Calculate percentage of paths below the expected value
per_centage <- rowSums(price_s < 1.0) / path_s
# Create xts time series of percentage of paths below the expected value
per_centage <- xts(per_centage, order.by=seq.Date(Sys.Date()-NROW(per_centage)+1, Sys.Date(), by=1))
# Plot xts time series of percentage of paths below the expected value
par(mar=c(3, 3, 2, 2), oma=c(0, 0, 0, 0))
plot.zoo(per_centage, main="Percentage of GBM paths below mean",
   xlab=NA, ylab=NA, col="blue")

# Sigma values
sig_mas <- c(0.5, 1, 1.5)
# Create plot colors
col_ors <- c("black", "red", "blue")
# Create legend labels
lab_els <- paste("sigma", sig_mas, sep="=")

# Plot all curves
for (in_dex in 1:NROW(sig_mas)) {
  curve(expr=dlnorm(x, sdlog=sig_mas[in_dex]),
  type="l", xlim=c(0, 3),
  xlab="", ylab="", lwd=2,
  col=col_ors[in_dex],
  add=as.logical(in_dex-1))
}  # End for
# Add title
title(main="Log-normal Distributions", line=0.5)
# Add legend
legend("topright", inset=0.05, title="Sigmas",
 lab_els, cex=0.8, lwd=2,
 lty=rep(1, NROW(sig_mas)),
 col=col_ors)

# load S&P500 stock prices
load("C:/Develop/R/lecture_slides/data/sp500.RData")
ls(env_sp500)
# Extract closing prices
price_s <- eapply(env_sp500, quantmod::Cl)
# Flatten price_s into a single xts series
price_s <- rutils::do_call(cbind, price_s)
# Carry forward and backward non-NA prices
price_s <- xts:::na.locf.xts(price_s)
price_s <- xts:::na.locf.xts(price_s, fromLast=TRUE)
sum(is.na(price_s))
# Rename and normalize columns
colnames(price_s) <- sapply(colnames(price_s),
  function(col_name) strsplit(col_name, split="[.]")[[1]][1])
price_s <- xts(t(t(price_s) / as.numeric(price_s[1, ])),
         order.by=index(price_s))
# Calculate permution index for sorting the lowest to highest final price_s
or_der <- order(price_s[NROW(price_s), ])
# Select a few symbols
sym_bols <- colnames(price_s)[or_der]
sym_bols <- sym_bols[seq.int(from=1, to=(NROW(sym_bols)-1), length.out=20)]

# Plot xts time series of price_s
col_ors <- colorRampPalette(c("red", "blue"))(NROW(sym_bols))
col_ors <- col_ors[order(order(price_s[NROW(price_s), sym_bols]))]
par(mar=c(3, 3, 2, 2), oma=c(0, 0, 0, 0))
plot.zoo(price_s[, sym_bols], main="20 S&P500 stock prices (normalized)",
   xlab=NA, ylab=NA, plot.type="single", col=col_ors)
legend(x="topleft", inset=0.05, cex=0.8,
 legend=rev(sym_bols), col=rev(col_ors), lwd=6, lty=1)

# Calculate average of valid stock prices
val_id <- (price_s != 1)  # valid stocks
n_stocks <- rowSums(val_id)
n_stocks[1] <- NCOL(price_s)
in_dex <- rowSums(price_s * val_id) / n_stocks
# Calculate percentage of stock prices below the average price
per_centage <- rowSums((price_s < in_dex) & val_id) / n_stocks
# Create xts time series of average stock prices
in_dex <- xts(in_dex, order.by=index(price_s))
# Plot xts time series of average stock prices
x11(width=6, height=4)
par(mar=c(3, 3, 2, 2), oma=c(0, 0, 0, 0))
plot.zoo(in_dex, main="Average S&P500 stock prices (normalized from 1990)",
   xlab=NA, ylab=NA, col="blue")
# Create xts time series of percentage of stock prices below the average price
per_centage <- xts(per_centage, order.by=index(price_s))
# Plot percentage of stock prices below the average price
plot.zoo(per_centage[-(1:2),],
   main="Percentage of S&P500 stock prices below the average price",
   xlab=NA, ylab=NA, col="blue")

NA

x11(width=6, height=4)
par(mar=c(4, 3, 1, 1), oma=c(0, 0, 0, 0))
library(zoo)
re_turns <-
  diff(log(as.numeric(EuStockMarkets[, 1])))
# acf() autocorrelation from package stats
acf(re_turns, lag=10, main="")
title(main="acf of DAX returns", line=-1)

library(Ecdat)  # load Ecdat
macro_zoo <- as.zoo(Macrodat[, c("lhur", "fygm3")])
colnames(macro_zoo) <- c("unemprate", "3mTbill")
macro_diff <- na.omit(diff(macro_zoo))
# Ljung-Box test for DAX returns
# 'lag' is the number of autocorrelation coefficients
Box.test(re_turns, lag=10, type="Ljung")

# Changes in 3 month T-bill rate are autocorrelated
Box.test(macro_diff[, "3mTbill"],
   lag=10, type="Ljung")

# Changes in unemployment rate are autocorrelated
Box.test(macro_diff[, "unemprate"],
   lag=10, type="Ljung")

library(zoo)  # load package zoo
dax_acf <- acf(re_turns, plot=FALSE)
summary(dax_acf)  # get the structure of the "acf" object
# Print(dax_acf)  # Print acf data
dim(dax_acf$acf)
dim(dax_acf$lag)
head(dax_acf$acf)

acf_plus <- function(ts_data, plo_t=TRUE,
                xlab="Lag", ylab="",
                main="", ...) {
  acf_data <- acf(x=ts_data, plot=FALSE, ...)
# Remove first element of acf data
  acf_data$acf <-  array(data=acf_data$acf[-1],
    dim=c((dim(acf_data$acf)[1]-1), 1, 1))
  acf_data$lag <-  array(data=acf_data$lag[-1],
    dim=c((dim(acf_data$lag)[1]-1), 1, 1))
  if (plo_t) {
    ci <- qnorm((1+0.95)/2)*sqrt(1/NROW(ts_data))
    ylim <- c(min(-ci, range(acf_data$acf[-1])),
        max(ci, range(acf_data$acf[-1])))
    plot(acf_data, xlab=xlab, ylab=ylab,
   ylim=ylim, main="", ci=0)
    title(main=main, line=0.5)
    abline(h=c(-ci, ci), col="blue", lty=2)
  }
  invisible(acf_data)  # Return invisibly
}  # End acf_plus

par(mar=c(5,0,1,2), oma=c(1,2,1,0), mgp=c(2,1,0), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
library(zoo)  # load package zoo
# improved autocorrelation function
acf_plus(re_turns, lag=10, main="")
title(main="acf of DAX returns", line=-1)
# Ljung-Box test for DAX returns
Box.test(re_turns, lag=10, type="Ljung")

par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # Set plot panels
# Autocorrelation of squared DAX returns
acf_plus(re_turns^2, lag=10, main="")
title(main="acf of squared DAX returns",
line=-1)
# Autocorrelation of squared random returns
acf_plus(rnorm(NROW(re_turns))^2,
   lag=10, main="")
title(main="acf of squared random returns",
line=-1)
# Ljung-Box test for squared DAX returns
Box.test(re_turns^2, lag=10, type="Ljung")

library(zoo)  # load package zoo
library(Ecdat)  # load Ecdat
colnames(Macrodat)  # United States Macroeconomic Time Series
macro_zoo <- as.zoo(  # Coerce to "zoo"
    Macrodat[, c("lhur", "fygm3")])
colnames(macro_zoo) <- c("unemprate", "3mTbill")
# ggplot2 in multiple panes
autoplot(  # generic ggplot2 for "zoo"
  object=macro_zoo, main="US Macro",
  facets=Series ~ .) + # End autoplot
  xlab("") +
theme(  # modify plot theme
  legend.position=c(0.1, 0.5),
  plot.title=element_text(vjust=-2.0),
  plot.margin=unit(c(-0.5, 0.0, -0.5, 0.0), "cm"),
  plot.background=element_blank(),
  axis.text.y=element_blank()
)  # End theme

par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # Set plot panels
macro_diff <- na.omit(diff(macro_zoo))
acf_plus(coredata(macro_diff[, "unemprate"]),
  lag=10, main="quarterly unemployment rate")
acf_plus(coredata(macro_diff[, "3mTbill"]),
  lag=10, main="3 month T-bill EOQ")

# Extract DAX time series
se_ries <- EuStockMarkets[, 1]
# Filter only over past values (sides=1)
co_eff <- rep(1/5,5)
filter_ed <- filter(se_ries, filter=co_eff,
             method="convolution", sides=1)
# filter() returns a time series of class "ts"
class(filter_ed)
# Filter using compiled C++ function directly
getAnywhere(C_cfilter)
str(stats:::C_cfilter)
filter_fast <- .Call(stats:::C_cfilter, se_ries, filter=co_eff, sides=1, circular=FALSE)
all.equal(as.numeric(filter_ed), filter_fast, check.attributes=FALSE)
# microbenchmark speed of the two methods
library(microbenchmark)
summary(microbenchmark(
  filter=filter(se_ries, filter=co_eff, method="convolution", sides=1),
  filter_fast=.Call(stats:::C_cfilter, se_ries, filter=co_eff, sides=1, circular=FALSE)
  ), times=10)[, c(1, 4, 5)]
# Simulate ARIMA using filter()
in_nov <- rnorm(NROW(EuStockMarkets))
arima_filter <- filter(x=in_nov,
  filter=co_eff, method="recursive")

library(zoo)  # load zoo
library(ggplot2)  # load ggplot2
library(gridExtra)  # load gridExtra
# Coerce to zoo and merge the time series
filter_ed <- cbind(as.zoo(se_ries),
            as.zoo(filter_ed))
colnames(filter_ed) <- c("DAX", "DAX filtered")
filter_ed <- window(filter_ed,
             start=1997, end=1998)
autoplot(  # Plot ggplot2
    filter_ed, main="Filtered DAX",
    facets=NULL) +  # End autoplot
xlab("") + ylab("") +
theme(  # modify plot theme
    legend.position=c(0.1, 0.5),
    plot.title=element_text(vjust=-2.0),
    plot.margin=unit(c(-0.5, 0.0, -0.5, 0.0), "cm"),
    plot.background=element_blank(),
    axis.text.y=element_blank()
    )  # End theme
# End ggplot2

par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
re_turns <- na.omit(diff(log(filter_ed)))
par(mfrow=c(2,1))  # Set plot panels

acf_plus(coredata(re_turns[, 1]), lag=10,
   xlab="")
title(main="DAX", line=-1)

acf_plus(coredata(re_turns[, 2]), lag=10,
   xlab="")
title(main="DAX filtered", line=-1)

# ARIMA processes
set.seed(1121)  # Reset random numbers
in_dex <- Sys.Date() + 0:728  # two year daily series
ari_ma <- xts(  # AR time series of returns
  x=arima.sim(n=NROW(in_dex), model=list(ar=0.2)),
  order.by=in_dex)
ari_ma <- cbind(ari_ma, cumsum(ari_ma))
colnames(ari_ma) <- c("AR returns", "AR prices")

library(ggplot2)  # load ggplot2
library(gridExtra)  # load gridExtra
autoplot(object=ari_ma, # ggplot AR process
 facets="Series ~ .",
 main="Autoregressive process (phi=0.2)") +
  facet_grid("Series ~ .", scales="free_y") +
  xlab("") + ylab("") +
theme(legend.position=c(0.1, 0.5),
  plot.background=element_blank(),
  axis.text.y=element_blank())

ar_coeff <- c(-0.9, 0.01, 0.9)  # AR coefficients
# Create three AR time series
ari_ma <- sapply(ar_coeff, function(phi) {
  set.seed(1121)  # Reset random numbers
  arima.sim(n=NROW(in_dex), model=list(ar=phi))
})  # End sapply
colnames(ari_ma) <- paste("autocorr", ar_coeff)
plot.zoo(ari_ma, main="AR(1) prices", xlab=NA)
# Or plot using ggplot
ari_ma <- xts(x=ari_ma, order.by=in_dex)
library(ggplot)
autoplot(ari_ma, main="AR(1) prices",
   facets=Series ~ .) +
    facet_grid(Series ~ ., scales="free_y") +
xlab("") +
theme(
  legend.position=c(0.1, 0.5),
  plot.title=element_text(vjust=-2.0),
  plot.margin=unit(c(-0.5, 0.0, -0.5, 0.0), "cm"),
  plot.background=element_blank(),
  axis.text.y=element_blank())

library(zoo)  # load zoo
library(ggplot2)  # load ggplot2
set.seed(1121)  # initialize random number generator
rand_walk <- cumsum(zoo(matrix(rnorm(3*100), ncol=3),
            order.by=(Sys.Date()+0:99)))
colnames(rand_walk) <-
  paste("rand_walk", 1:3, sep="_")
plot.zoo(rand_walk, main="Random walks",
     xlab="", ylab="", plot.type="single",
     col=c("black", "red", "blue"))
# Add legend
legend(x="topleft",
 legend=colnames(rand_walk),
 col=c("black", "red", "blue"), lty=1)

# define AR(2) coefficients
co_eff <- c(0.9, 0.09)
# Calculate modulus of roots of characteristic equation
root_s <- Mod(polyroot(c(1, -co_eff)))
# Calculate warmup period
warm_up <- NROW(co_eff) + ceiling(6/log(min(root_s)))
set.seed(1121)
len_gth <- 1e4
in_nov <- rnorm(len_gth + warm_up)
# Simulate ARIMA using arima.sim()
ari_ma <- arima.sim(n=len_gth,
  model=list(ar=co_eff),
  start.innov=in_nov[1:warm_up],
  innov=in_nov[(warm_up+1):NROW(in_nov)])
# Simulate ARIMA using filter()
arima_filter <- filter(x=in_nov,
  filter=co_eff, method="recursive")
all.equal(arima_filter[-(1:warm_up)],
  as.numeric(ari_ma))
# Simulate ARIMA using for() loop
arima_loop <- numeric(NROW(in_nov))
arima_loop[1] <- in_nov[1]
arima_loop[2] <- co_eff[1]*arima_loop[1] + in_nov[2]
for (it in 3:NROW(arima_loop)) {
  arima_loop[it] <- arima_loop[(it-1):(it-2)] %*% co_eff + in_nov[it]
}  # End for
all.equal(arima_loop,
  as.numeric(arima_filter))
# microbenchmark the speed of the three methods of simulating ARIMA
library(microbenchmark)
summary(microbenchmark(
  arima_filter=filter(x=in_nov, filter=co_eff, method="recursive"),
  arima_sim=arima.sim(n=len_gth,
                  model=list(ar=co_eff),
                  start.innov=in_nov[1:warm_up],
                  innov=in_nov[(warm_up+1):NROW(in_nov)]),
  arima_loop=for (it in 3:NROW(arima_loop)) {
arima_loop[it] <- arima_loop[(it-1):(it-2)] %*% co_eff + in_nov[it]}
  ), times=10)[, c(1, 4, 5)]

# Simulate random walks using apply() loops
set.seed(1121)  # initialize random number generator
rand_walks <- matrix(rnorm(1000*100), ncol=1000)
rand_walks <- apply(rand_walks, 2, cumsum)
vari_ance <- apply(rand_walks, 1, var)
# Simulate random walks using vectorized functions
set.seed(1121)  # initialize random number generator
rand_walks <- matrixStats::colCumsums(matrix(rnorm(1000*100), ncol=1000))
vari_ance <- matrixStats::rowVars(rand_walks)
par(mar=c(5, 3, 2, 2), oma=c(0, 0, 0, 0))
plot(vari_ance, xlab="time steps", ylab="",
     t="l", col="blue", lwd=2,
     main="Variance of Random Walk")

len_gth <- 1e4
# Simulate arima with small AR coefficient
set.seed(1121)
ari_ma <- arima.sim(n=len_gth, model=list(ar=0.01))
tseries::adf.test(ari_ma)
# Simulate arima with large AR coefficient
set.seed(1121)
ari_ma <- arima.sim(n=len_gth, model=list(ar=0.99))
tseries::adf.test(ari_ma)
# Simulate arima with different AR coefficients
coeff_s <- seq(0.99, 1.0, 0.001) - 0.001
set.seed(1121)
in_nov <- rnorm(len_gth)
adf_test <- sapply(coeff_s, function(co_eff) {
  ari_ma <- filter(x=in_nov, filter=co_eff, method="recursive")
  ad_f <- suppressWarnings(tseries::adf.test(ari_ma))
  c(adf_stat=unname(ad_f$statistic), pval=ad_f$p.value)
})  # End sapply
plot(x=coeff_s, y=adf_test["pval", ], main="ADF Pval versus AR coefficient",
     xlab="AR coefficient", ylab="ADF pval", t="l", col="blue", lwd=2)
plot(x=coeff_s, y=adf_test["adf_stat", ], main="ADF Stat versus AR coefficient",
     xlab="AR coefficient", ylab="ADF stat", t="l", col="blue", lwd=2)

# Simulate arima with large AR coefficient
set.seed(1121)
ari_ma <- arima.sim(n=len_gth, model=list(ar=0.99))
tseries::adf.test(ari_ma)
# integrated series has unit root
tseries::adf.test(cumsum(ari_ma))
# Simulate arima with negative AR coefficient
set.seed(1121)
ari_ma <- arima.sim(n=len_gth, model=list(ar=-0.99))
tseries::adf.test(ari_ma)
# integrated series has unit root
tseries::adf.test(cumsum(ari_ma))

x11(width=5, height=3.5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0))
# Simulate AR(1) process
ari_ma <- arima.sim(n=729, model=list(ar=0.8))
# ACF of AR(1) process
ac_f <- acf_plus(ari_ma, lag=10,
  xlab="", ylab="",
  main="Autocorrelations of AR(1) process")
ac_f$acf[1:5]

# PACF of AR(1) process
pac_f <- pacf(ari_ma, lag=10,
  xlab="", ylab="", main="")
title("Partial autocorrelations of AR(1) process",
  line=1)
pac_f <- drop(pac_f$acf)
pac_f[1:5]

# Compute pacf recursively from acf
ac_f <- acf_plus(ari_ma, lag=10, plo_t=FALSE)
ac_f <- drop(ac_f$acf)
pac_f <- numeric(3)
pac_f[1] <- ac_f[1]
pac_f[2] <- ac_f[2] - ac_f[1]^2
pac_f[3] <- ac_f[3] -
  pac_f[2]*ac_f[1] - ac_f[2]*pac_f[1]
# Compute pacf recursively in a loop
pac_f <- numeric(NROW(ac_f))
pac_f[1] <- ac_f[1]
for (it in 2:NROW(pac_f)) {
  pac_f[it] <- ac_f[it] -
    pac_f[1:(it-1)] %*% ac_f[(it-1):1]
}  # End for

par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # Set plot panels
# Simulate AR(3) time series of returns
ari_ma <- arima.sim(n=729,
  model=list(ar=c(0.1, 0.5, 0.1)))
# ACF of AR(3) process
acf_plus(ari_ma, lag=10, xlab="", ylab="",
   main="ACF of AR(3) process")
# PACF of AR(3) process
pacf(ari_ma, lag=10, xlab="", ylab="",
     main="PACF of AR(3) process")

# Calibrate ARIMA model using arima()
arima_fit <- arima(ari_ma,
  order=c(3,0,0), include.mean=FALSE)
arima_fit$coef
# Calibrate ARIMA model using auto.arima()
# library(forecast)  # load forecast
forecast::auto.arima(ari_ma, max.p=3, max.q=0)
# Calibrate ARIMA model using regression
ari_ma <- as.numeric(ari_ma)
# define design matrix
de_sign <- sapply(1:3, function(lagg) {
  rutils::lag_it(ari_ma, lagg=lagg)
})  # End sapply
# generalized inverse of design matrix
design_inv <- MASS::ginv(de_sign)
# Regression coefficients with response equal to ari_ma
co_eff <- drop(design_inv %*% ari_ma)
all.equal(arima_fit$coef, co_eff, check.attributes=FALSE)

# Compute autocorrelation coefficients
ac_f <- acf_plus(ari_ma, lag=10, plo_t=FALSE)
ac_f <- drop(ac_f$acf)
# define Yule-Walker matrix
acf_1 <- c(1, ac_f[-10])
yule_walker <- sapply(1:9, function(lagg) {
  col_umn <- rutils::lag_it(acf_1, lagg=lagg)
  col_umn[1:lagg] <- acf_1[(lagg+1):2]
  col_umn
})  # End sapply
yule_walker <- cbind(acf_1, yule_walker)
# generalized inverse of Yule-Walker matrix
yule_walker_inv <- MASS::ginv(yule_walker)
# Solve Yule-Walker equations
co_eff <- drop(yule_walker_inv %*% ac_f)

len_gth <- 1e2
# Simulate AR(3) time series using filter()
co_eff <- c(0.1, 0.39, 0.5)
set.seed(1121); in_nov <- rnorm(len_gth)
se_ries <- filter(x=in_nov,
  filter=co_eff, method="recursive")
# Forecast using AR(3) ARIMA
forecast_s <- numeric(NROW(se_ries))
forecast_s[2] <- co_eff[1]*se_ries[1]
forecast_s[3] <- co_eff[1]*se_ries[2] +
  co_eff[2]*se_ries[1]
for (it in 4:NROW(forecast_s)) {
  forecast_s[it] <-
    se_ries[(it-1):(it-3)] %*% co_eff
}  # End for
# Forecast using filter()
forecasts_filter <- filter(x=se_ries, sides=1,
  filter=co_eff, method="convolution")
class(forecasts_filter)
all.equal(forecast_s[-(1:4)],
  forecasts_filter[-c(1:3, NROW(forecasts_filter))],
  check.attributes=FALSE)
# Compare residuals with innovations
residual_s <- (se_ries - forecast_s)
tail(cbind(in_nov, residual_s))
plot(residual_s, t="l", lwd=3, main="ARIMA Forecast Errors")

# Plot with legend
plot.default(se_ries,
  main="Forecasting Using AR(3) Model",
  xlab="", ylab="", type="l")
lines(forecast_s, col="orange", lwd=3)
legend(x="topright", legend=c("series","forecasts"),
 col=c("black", "orange"), lty=1, lwd=6,
 cex=0.9, bg="white", bty="n")

# Forecast using predict.Arima()
?stats:::predict.Arima
# se_ries <- as.numeric(lh)
# len_gth <- NROW(se_ries)
# Compare one-step-ahead forecasts
# arima_fit <- arima(se_ries, order=c(3,0,0), method="ML", include.mean=FALSE)
arima_fit <- arima(se_ries, order=c(3,0,0), method="ML")
pre_dict <- predict(arima_fit, n.ahead=1)
fore_cast <- drop(c(se_ries[(len_gth-2):len_gth], 1) %*% arima_fit$coef)
# fore_cast <- drop(se_ries[(len_gth-2):len_gth] %*% arima_fit$coef)
as.numeric(pre_dict$pred)

# Compare many one-step-ahead forecasts
foo <- sapply(1:1e2, function(x) {
  se_ries <- filter(x=rnorm(len_gth+1), filter=co_eff, method="recursive")
  arima_fit <- arima(se_ries[1:len_gth], order=c(3,0,0), method="ML")
  pre_dict <- predict(arima_fit, n.ahead=1)
  fore_cast <- drop(c(se_ries[(len_gth-2):len_gth], 1) %*% arima_fit$coef)
  c(actual=se_ries[len_gth+1], forecast=fore_cast, predict=as.numeric(pre_dict$pred))
})  # end sapply
foo <- t(foo)
# hist(foo[, 1], breaks=30,
#   main="", ylim=c(0, 60), xlim=c(-0.04, 0.04),
#   xlab="", ylab="", freq=FALSE)

hist(foo[, 1], ylim=c(0, 0.15), freq=FALSE)
lines(density(foo[, 1]), col='blue', lwd=3)
lines(density(foo[, 2]), col='green', lwd=3)
lines(density(foo[, 3]), col='red', lwd=3)


# Forecast using AR(3) ARIMA
forecast_s <- numeric(NROW(se_ries))
forecast_s[2] <- co_eff[1]*se_ries[1]
forecast_s[3] <- co_eff[1]*se_ries[2] +
  co_eff[2]*se_ries[1]
for (it in 4:NROW(forecast_s)) {
  forecast_s[it] <-
    se_ries[(it-1):(it-3)] %*% co_eff
}  # End for
# Forecast using filter()
forecasts_filter <- filter(x=se_ries, sides=1,
  filter=co_eff, method="convolution")
class(forecasts_filter)
all.equal(forecast_s[-(1:4)],
  forecasts_filter[-c(1:3, NROW(forecasts_filter))],
  check.attributes=FALSE)
# Compare residuals with innovations
residual_s <- (se_ries-forecast_s)
tail(cbind(in_nov, residual_s))

# Plot with legend
plot.default(se_ries,
  main="Forecasting Using AR(3) Model",
  xlab="", ylab="", type="l")
lines(forecast_s, col="orange", lwd=3)
legend(x="topright", legend=c("series","forecasts"),
 col=c("black", "orange"), lty=1, lwd=6,
 cex=0.9, bg="white", bty="n")

len_gth <- 1e2
# Simulate AR(3) time series using filter()
co_eff <- c(0.1, 0.39, 0.5)
set.seed(1121)
se_ries <- filter(x=rnorm(len_gth),
  filter=co_eff, method="recursive")
# Forecast using AR(3) ARIMA
forecast_s <- numeric(NROW(se_ries))
forecast_s[2] <- co_eff[1]*se_ries[1]
forecast_s[3] <- co_eff[1]*se_ries[2] +
  co_eff[2]*se_ries[1]
for (it in 4:NROW(forecast_s)) {
  forecast_s[it] <-
    se_ries[(it-1):(it-3)] %*% co_eff
}  # End for
# Forecast using filter()
forecasts_filter <- filter(x=se_ries, sides=1,
  filter=co_eff, method="convolution")
class(forecasts_filter)
all.equal(forecast_s[-(1:4)],
  forecasts_filter[-c(1:3, NROW(forecasts_filter))],
  check.attributes=FALSE)
# Compare residuals with innovations
residual_s <- (se_ries-forecast_s)
tail(cbind(in_nov, residual_s))

# Plot with legend
plot.default(se_ries,
  main="Forecasting Using AR(3) Model",
  xlab="", ylab="", type="l")
lines(forecast_s, col="orange", lwd=3)
legend(x="topright", legend=c("series","forecasts"),
 col=c("black", "orange"), lty=1, lwd=6,
 cex=0.9, bg="white", bty="n")

# Simulate AR(3) time series using filter()
len_gth <- 1e3
co_eff <- c(0.1, 0.39, 0.5)
set.seed(1121)
se_ries <- filter(x=rnorm(len_gth),
  filter=co_eff, method="recursive")
# Define design matrix and end_points
de_sign <- sapply(1:3, function(lagg) {
  rutils::lag_it(se_ries, lagg=lagg)
})  # End sapply
de_sign <- cbind(se_ries, de_sign)
look_back <- 100  # length of look-back interval
end_points <- seq_along(se_ries)
n_rows <- NROW(end_points)
start_points <- c(rep_len(1, look_back-1),
    end_points[1:(n_rows-look_back+1)])
# Perform rolling forecasting
forecast_s <- sapply(seq_along(end_points)[-(1:2)],
  function(it) {
    de_sign <- de_sign[start_points[it]:end_points[it], ]
    # Calculate AR(3) coefficients
    design_inv <- MASS::ginv(de_sign[, -1])
    co_eff <- drop(design_inv %*% de_sign[, 1])
    de_sign[(NROW(de_sign)-2):NROW(de_sign), 1] %*% co_eff
  })  # End sapply
forecast_s <- c(rep(forecast_s[1], 2), forecast_s)
# Lag the forecasts to push them out-of-sample
forecast_s <- rutils::lag_it(forecast_s)

# Mean squared error
ms_e <- mean((forecast_s-se_ries)^2)

# Plot with legend
plot.default(se_ries,
  main="Rolling Forecasting Using AR(3) Model",
  xlab="", ylab="", type="l")
lines(forecast_s, col="orange", lwd=1)
legend(x="topright", legend=c("series","forecasts"),
 col=c("black", "orange"), lty=1, lwd=6,
 cex=0.9, bg="white", bty="n")

# define Ornstein-Uhlenbeck parameters
eq_price <- 1.0; vol_at <- 0.02
the_ta <- 0.01; len_gth <- 1000
drif_t <- the_ta*eq_price
theta_1 <- 1-the_ta
# Simulate Ornstein-Uhlenbeck process
in_nov <- vol_at*rnorm(len_gth)
price_s <- numeric(len_gth)
price_s[1] <- in_nov[1]
for (i in 2:len_gth) {
  price_s[i] <- theta_1*price_s[i-1] +
    in_nov[i] + drif_t
}  # End for

plot(price_s, type="l",
     xlab="periods", ylab="prices",
     main="Ornstein-Uhlenbeck process")
legend("topright",
 title=paste(c(paste0("vol_at = ", vol_at),
               paste0("eq_price = ", eq_price),
               paste0("the_ta = ", the_ta)),
             collapse="\n"),
 legend="", cex=0.8,
 inset=0.1, bg="white", bty="n")
abline(h=eq_price, col='red', lwd=2)

re_turns <- rutils::diff_it(price_s)
lag_price <- rutils::lag_it(price_s)
for_mula <- re_turns ~ lag_price
l_m <- lm(for_mula)
summary(l_m)
# Plot regression
plot(for_mula, main="OU Returns Versus Lagged Prices")
abline(l_m, lwd=2, col="red")

# volatility parameter
c(vol_at, sd(re_turns))
# Extract OU parameters from regression
co_eff <- summary(l_m)$coefficients
# theta strength of mean reversion
round(co_eff[2, ], 3)
# Equilibrium price
co_eff[1, 1]/co_eff[2, 1]
# Parameter and t-values
co_eff <- cbind(c(the_ta*eq_price, the_ta),
  co_eff[, 1:2])
rownames(co_eff) <- c("drift", "theta")
round(co_eff, 3)

# Simulate Ornstein-Uhlenbeck process
re_turns <- numeric(len_gth)
price_s <- numeric(len_gth)
price_s[1] <- eq_price
set.seed(1121)  # Reset random numbers
for (i in 2:len_gth) {
  re_turns[i] <- the_ta*(eq_price - price_s[i-1]) +
    vol_at*rnorm(1)
  price_s[i] <- price_s[i-1] * exp(re_turns[i])
}  # End for

plot(price_s, type="l",
     xlab="periods", ylab="prices",
     main="Log-normal Ornstein-Uhlenbeck process")
legend("topright",
 title=paste(c(paste0("vol_at = ", vol_at),
               paste0("eq_price = ", eq_price),
               paste0("the_ta = ", the_ta)),
             collapse="\n"),
 legend="", cex=0.8,
 inset=0.12, bg="white", bty="n")
abline(h=eq_price, col='red', lwd=2)

# define Ornstein-Uhlenbeck parameters
eq_price <- 5.0; vol_at <- 0.01
the_ta <- 0.01; len_gth <- 1000
# Simulate Ornstein-Uhlenbeck process
re_turns <- numeric(len_gth)
price_s <- numeric(len_gth)
price_s[1] <- 5.0
set.seed(1121)  # Reset random numbers
for (i in 2:len_gth) {
  re_turns[i] <- the_ta*(eq_price - price_s[i-1]) +
    vol_at*rnorm(1)
  price_s[i] <- price_s[i-1] * exp(re_turns[i])
}  # End for

plot(price_s, type="l",
     xlab="periods", ylab="prices",
     main="Ornstein-Uhlenbeck process")
legend("topright",
 title=paste(c(paste0("vol_at = ", vol_at),
               paste0("eq_price = ", eq_price),
               paste0("the_ta = ", the_ta)),
             collapse="\n"),
 legend="", cex=0.8,
 inset=0.1, bg="white", bty="n")
