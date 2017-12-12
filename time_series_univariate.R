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
load(file="C:/Develop/R/lecture_slides/data/zoo_data.RData")
# calculate price adjustment vector
adj_vector <-
  as.vector(ts_stx[, "AdjClose"] / ts_stx[, "Close"])
# adjust OHLC prices
ts_stx_adj <- ts_stx
ts_stx_adj[, c("Open","High","Low","Close")] <-
  adj_vector * ts_stx[, c("Open","High","Low","Close")]
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
load(file="C:/Develop/R/lecture_slides/data/zoo_data.RData")
class(zoo_stx)
dim(zoo_stx)
head(zoo_stx, 4)
library(tseries)  # load package tseries
load(file="C:/Develop/R/lecture_slides/data/zoo_data.RData")
# calculate price adjustment vector
adj_vector <-
  as.vector(zoo_stx[, "AdjClose"] / zoo_stx[, "Close"])
head(adj_vector, 5)
tail(adj_vector, 5)
# adjust OHLC prices
zoo_stx_adj <- zoo_stx
zoo_stx_adj[, c("Open","High","Low","Close")] <-
  adj_vector * zoo_stx[, c("Open","High","Low","Close")]
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
zoo_stxeur <- cbind(zoo_eurusd,
               zoo_stx[, "AdjClose"])
colnames(zoo_stxeur) <- c("EURUSD", "MSFT")
zoo_stxeur <-
  zoo_stxeur[complete.cases(zoo_stxeur),]
save(zoo_stx, zoo_stx_adj,
     ts_stx, ts_stx_adj,
     zoo_eurusd, zoo_stxeur,
     file="C:/Develop/R/lecture_slides/data/zoo_data.RData")
load(file="C:/Develop/R/lecture_slides/data/zoo_data.RData")
# inspect the data
class(zoo_eurusd)
tail(zoo_eurusd, 4)
library(tseries)  # load package tseries
# ETF symbols for asset allocation
sym_bols <- c("VTI", "VEU", "IEF", "VNQ",
  "DBC", "VXX", "XLY", "XLP", "XLE", "XLF",
  "XLV", "XLI", "XLB", "XLK", "XLU", "VYM",
  "IVW", "IWB", "IWD", "IWF")
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
# or
zoo_series <- rutils::do_call(cbind, zoo_series)
# assign names in format "symbol.Close", "symbol.Volume"
names(zoo_series) <-
  as.vector(sapply(sym_bols,
             paste, c("Close", "Volume"), sep="."))
# save zoo_series to a comma-separated CSV file
write.zoo(zoo_series, file='zoo_series.csv', sep=",")
# save zoo_series to a binary .RData file
save(zoo_series, file='zoo_series.RData')
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
load(file="C:/Develop/R/lecture_slides/data/zoo_data.RData")
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
load(file="C:/Develop/R/lecture_slides/data/zoo_data.RData")
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
load(file="C:/Develop/R/lecture_slides/data/zoo_data.RData")
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
# define daily volatility and growth rate
vol_at <- 0.01; dri_ft <- 0.0; len_gth <- 1000
# simulate geometric Brownian motion
re_turns <- vol_at*rnorm(len_gth) +
  dri_ft - vol_at^2/2
price_s <- exp(cumsum(re_turns))
plot(price_s, type="l",
     xlab="periods", ylab="prices",
     main="geometric Brownian motion")
# simulate geometric Brownian motion
vol_at <- 0.01/sqrt(48)
dri_ft <- 0.0
len_gth <- 10000
in_dex <- seq(from=as.POSIXct(paste(Sys.Date()-250, "09:30:00")),
  length.out=len_gth, by="30 min")
price_s <- xts(exp(cumsum(vol_at*rnorm(len_gth) + dri_ft - vol_at^2/2)),
  order.by=in_dex)
price_s <- cbind(price_s,
  volume=sample(x=10*(2:18), size=len_gth, replace=TRUE))
# aggregate to daily OHLC data
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
# simulate multiple paths of geometric Brownian motion
price_s <- matrix(vol_at*rnorm(path_s*len_gth) +
    dri_ft - vol_at^2/2, nc=path_s)
price_s <- exp(matrixStats::colCumsums(price_s))
# create zoo time series
price_s <- zoo(price_s, order.by=seq.Date(Sys.Date()-NROW(price_s)+1, Sys.Date(), by=1))
# plot zoo time series
col_ors <- colorRampPalette(c("red", "blue"))(NCOL(price_s))
col_ors <- col_ors[order(order(price_s[NROW(price_s), ]))]
par(mar=c(3, 3, 2, 2), oma=c(0, 0, 0, 0))
plot.zoo(price_s, main="Multiple paths of geometric Brownian motion",
   xlab=NA, ylab=NA, plot.type="single", col=col_ors)
# define daily volatility and growth rate
vol_at <- 0.01; dri_ft <- 0.0; len_gth <- 10000
path_s <- 100
# simulate multiple paths of geometric Brownian motion
price_s <- matrix(vol_at*rnorm(path_s*len_gth) +
    dri_ft - vol_at^2/2, nc=path_s)
price_s <- exp(matrixStats::colCumsums(price_s))
# calculate percentage of paths below the expected value
per_centage <- rowSums(price_s < 1.0) / path_s
# create zoo time series of percentage of paths below the expected value
per_centage <- zoo(per_centage, order.by=seq.Date(Sys.Date()-NROW(per_centage)+1, Sys.Date(), by=1))
# plot zoo time series of percentage of paths below the expected value
par(mar=c(3, 3, 2, 2), oma=c(0, 0, 0, 0))
plot.zoo(per_centage, main="Percentage of GBM paths below mean",
   xlab=NA, ylab=NA, col="blue")
# sigma values
sig_mas <- c(0.5, 1, 1.5)
# create plot colors
col_ors <- c("black", "red", "blue")
# create legend labels
lab_els <- paste("sigma", sig_mas, sep="=")
# plot all curves
for (in_dex in 1:NROW(sig_mas)) {
  curve(expr=dlnorm(x, sdlog=sig_mas[in_dex]),
  type="l", xlim=c(0, 3),
  xlab="", ylab="", lwd=2,
  col=col_ors[in_dex],
  add=as.logical(in_dex-1))
}  # end for
# add title
title(main="Log-normal Distributions", line=0.5)
# add legend
legend("topright", inset=0.05, title="Sigmas",
 lab_els, cex=0.8, lwd=2,
 lty=rep(1, NROW(sig_mas)),
 col=col_ors)
# load S&P500 stock prices
load("C:/Develop/R/lecture_slides/data/sp500.RData")
ls(env_sp500)
# extract closing prices
price_s <- eapply(env_sp500, quantmod::Cl)
# flatten price_s into a single xts series
price_s <- rutils::do_call(cbind, price_s)
# carry forward and backward non-NA prices
price_s <- zoo::na.locf(price_s)
price_s <- zoo::na.locf(price_s, fromLast=TRUE)
sum(is.na(price_s))
# rename and normalize columns
colnames(price_s) <- sapply(colnames(price_s),
  function(col_name) strsplit(col_name, split="[.]")[[1]][1])
price_s <- xts(t(t(price_s) / as.numeric(price_s[1, ])),
         order.by=index(price_s))
# calculate permution index for sorting the lowest to highest final price_s
or_der <- order(price_s[NROW(price_s), ])
# select a few symbols
sym_bols <- colnames(price_s)[or_der]
sym_bols <- sym_bols[seq.int(from=1, to=(NROW(sym_bols)-1), length.out=20)]
# plot xts time series of price_s
col_ors <- colorRampPalette(c("red", "blue"))(NROW(sym_bols))
col_ors <- col_ors[order(order(price_s[NROW(price_s), sym_bols]))]
par(mar=c(3, 3, 2, 2), oma=c(0, 0, 0, 0))
plot.zoo(price_s[, sym_bols], main="20 S&P500 stock prices (normalized)",
   xlab=NA, ylab=NA, plot.type="single", col=col_ors)
legend(x="topleft", inset=0.05, cex=0.8,
 legend=rev(sym_bols), col=rev(col_ors), lwd=6, lty=1)
# calculate average of valid stock prices
val_id <- (price_s != 1)  # valid stocks
num_stocks <- rowSums(val_id)
num_stocks[1] <- NCOL(price_s)
in_dex <- rowSums(price_s * val_id) / num_stocks
# calculate percentage of stock prices below the average price
per_centage <- rowSums((price_s < in_dex) & val_id) / num_stocks
# create zoo time series of average stock prices
in_dex <- zoo(in_dex, order.by=index(price_s))
# plot zoo time series of average stock prices
x11(width=6, height=4)
par(mar=c(3, 3, 2, 2), oma=c(0, 0, 0, 0))
plot.zoo(in_dex, main="Average S&P500 stock prices (normalized from 1990)",
   xlab=NA, ylab=NA, col="blue")
# create xts time series of percentage of stock prices below the average price
per_centage <- xts(per_centage, order.by=index(price_s))
# plot percentage of stock prices below the average price
plot.zoo(per_centage[-(1:2),],
   main="Percentage of S&P500 stock prices below the average price",
   xlab=NA, ylab=NA, col="blue")
NA
x11(width=6, height=4)
par(mar=c(4, 3, 1, 1), oma=c(0, 0, 0, 0))
library(zoo)
re_turns <- diff(log(EuStockMarkets[, 1]))
# acf() autocorrelation from package stats
acf(zoo::coredata(re_turns), lag=10, main="")
title(main="acf of DAX returns", line=-1)
library(Ecdat)  # load Ecdat
macro_zoo <- as.zoo(Macrodat[, c("lhur", "fygm3")])
colnames(macro_zoo) <- c("unemprate", "3mTbill")
macro_diff <- na.omit(diff(macro_zoo))
# Ljung-Box test for DAX returns
# 'lag' is the number of autocorrelation coefficients
Box.test(re_turns, lag=10, type="Ljung")

# changes in 3 month T-bill rate are autocorrelated
Box.test(macro_diff[, "3mTbill"],
   lag=10, type="Ljung")

# changes in unemployment rate are autocorrelated
Box.test(macro_diff[, "unemprate"],
   lag=10, type="Ljung")
library(zoo)  # load package zoo
dax_acf <- acf(coredata(re_turns), plot=FALSE)
summary(dax_acf)  # get the structure of the "acf" object
# print(dax_acf)  # print acf data
dim(dax_acf$acf)
dim(dax_acf$lag)
head(dax_acf$acf)
acf_plus <- function (ts_data, plot=TRUE,
                xlab="Lag", ylab="",
                main="", ...) {
  acf_data <- acf(x=ts_data, plot=FALSE, ...)
# remove first element of acf data
  acf_data$acf <-  array(data=acf_data$acf[-1],
    dim=c((dim(acf_data$acf)[1]-1), 1, 1))
  acf_data$lag <-  array(data=acf_data$lag[-1],
    dim=c((dim(acf_data$lag)[1]-1), 1, 1))
  if (plot) {
    ci <- qnorm((1+0.95)/2)*sqrt(1/length(ts_data))
    ylim <- c(min(-ci, range(acf_data$acf[-1])),
        max(ci, range(acf_data$acf[-1])))
    plot(acf_data, xlab=xlab, ylab=ylab,
   ylim=ylim, main=main, ci=0)
    abline(h=c(-ci, ci), col="blue", lty=2)
  }
  invisible(acf_data)  # return invisibly
}  # end acf_plus
par(mar=c(5,0,1,2), oma=c(1,2,1,0), mgp=c(2,1,0), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
library(zoo)  # load package zoo
# improved autocorrelation function
acf_plus(coredata(re_turns), lag=10, main="")
title(main="acf of DAX returns", line=-1)
# Ljung-Box test for DAX returns
Box.test(re_turns, lag=10, type="Ljung")
par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # set plot panels
# autocorrelation of squared DAX returns
acf_plus(coredata(re_turns)^2,
   lag=10, main="")
title(main="acf of squared DAX returns",
line=-1)
# autocorrelation of squared random returns
acf_plus(rnorm(length(re_turns))^2,
   lag=10, main="")
title(main="acf of squared random returns",
line=-1)
# Ljung-Box test for squared DAX returns
Box.test(re_turns^2, lag=10, type="Ljung")
library(zoo)  # load package zoo
library(Ecdat)  # load Ecdat
colnames(Macrodat)  # United States Macroeconomic Time Series
macro_zoo <- as.zoo(  # coerce to "zoo"
    Macrodat[, c("lhur", "fygm3")])
colnames(macro_zoo) <- c("unemprate", "3mTbill")
# ggplot2 in multiple panes
autoplot(  # generic ggplot2 for "zoo"
  object=macro_zoo, main="US Macro",
  facets=Series ~ .) + # end autoplot
  xlab("") +
theme(  # modify plot theme
  legend.position=c(0.1, 0.5),
  plot.title=element_text(vjust=-2.0),
  plot.margin=unit(c(-0.5, 0.0, -0.5, 0.0), "cm"),
  plot.background=element_blank(),
  axis.text.y=element_blank()
)  # end theme
par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # set plot panels
macro_diff <- na.omit(diff(macro_zoo))

acf_plus(coredata(macro_diff[, "unemprate"]),
   lag=10)
title(main="quarterly unemployment rate",
line=-1)

acf_plus(coredata(macro_diff[, "3mTbill"]),
   lag=10)
title(main="3 month T-bill EOQ", line=-1)
library(zoo)  # load zoo
library(ggplot2)  # load ggplot2
library(gridExtra)  # load gridExtra
# extract DAX time series
dax_ts <- EuStockMarkets[, 1]
# filter past values only (sides=1)
dax_filt <- filter(dax_ts,
    filter=rep(1/5,5), sides=1)
# coerce to zoo and merge the time series
dax_filt <- cbind(as.zoo(dax_ts),
            as.zoo(dax_filt))
colnames(dax_filt) <- c("DAX", "DAX filtered")
dax_data <- window(dax_filt,
             start=1997, end=1998)
autoplot(  # plot ggplot2
    dax_data, main="Filtered DAX",
    facets=NULL) +  # end autoplot
xlab("") + ylab("") +
theme(  # modify plot theme
    legend.position=c(0.1, 0.5),
    plot.title=element_text(vjust=-2.0),
    plot.margin=unit(c(-0.5, 0.0, -0.5, 0.0), "cm"),
    plot.background=element_blank(),
    axis.text.y=element_blank()
    )  # end theme
# end ggplot2
par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
re_turns <- na.omit(diff(log(dax_filt)))
par(mfrow=c(2,1))  # set plot panels

acf_plus(coredata(re_turns[, 1]), lag=10,
   xlab="")
title(main="DAX", line=-1)

acf_plus(coredata(re_turns[, 2]), lag=10,
   xlab="")
title(main="DAX filtered", line=-1)
par(oma=c(1, 1, 1, 1), mar=c(2, 2, 1, 1), mgp=c(0, 0.5, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # set plot panels
# autocorrelation from "stats"
acf_plus(re_turns[, 2], lag=10, xlab=NA, ylab=NA)
title(main="DAX filtered autocorrelations", line=-1)
# partial autocorrelation
pacf(re_turns[, 2], lag=10, xlab=NA, ylab=NA)
title(main="DAX filtered partial autocorrelations",
      line=-1)
# ARIMA processes
library(ggplot2)  # load ggplot2
library(gridExtra)  # load gridExtra
in_dex <- Sys.Date() + 0:728  # two year daily series
set.seed(1121)  # reset random numbers
zoo_arima <- zoo(  # AR time series of returns
  x=arima.sim(n=729, model=list(ar=0.2)),
  order.by=in_dex)  # zoo_arima
zoo_arima <- cbind(zoo_arima, cumsum(zoo_arima))
colnames(zoo_arima) <- c("AR returns", "AR prices")
autoplot(object=zoo_arima, # ggplot AR process
 facets="Series ~ .",
 main="Autoregressive process (phi=0.2)") +
  facet_grid("Series ~ .", scales="free_y") +
  xlab("") + ylab("") +
theme(
  legend.position=c(0.1, 0.5),
  plot.background=element_blank(),
  axis.text.y=element_blank())
ar_coeff <- c(-0.8, 0.01, 0.8)  # AR coefficients
zoo_arima <- sapply(  # create three AR time series
  ar_coeff, function(phi) {
    set.seed(1121)  # reset random numbers
    arima.sim(n=729, model=list(ar=phi))
  } )
zoo_arima <- zoo(x=zoo_arima, order.by=in_dex)
# convert returns to prices
zoo_arima <- cumsum(zoo_arima)
colnames(zoo_arima) <-
  paste("autocorr", ar_coeff)
autoplot(zoo_arima, main="AR prices",
   facets=Series ~ .) +
    facet_grid(Series ~ ., scales="free_y") +
xlab("") +
theme(
  legend.position=c(0.1, 0.5),
  plot.title=element_text(vjust=-2.0),
  plot.margin=unit(c(-0.5, 0.0, -0.5, 0.0), "cm"),
  plot.background=element_blank(),
  axis.text.y=element_blank())
par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # set plot panels
# simulate AR(1) process
ari_ma <- arima.sim(n=729, model=list(ar=0.8))
# ACF of AR(1) process
acf_plus(ari_ma, lag=10, xlab="", ylab="",
   main="ACF of AR(1) process")
# PACF of AR(1) process
pacf(ari_ma, lag=10, xlab="", ylab="",
     main="PACF of AR(1) process")
library(zoo)  # load zoo
library(ggplot2)  # load ggplot2
set.seed(1121)  # initialize random number generator
rand_walk <- cumsum(zoo(matrix(rnorm(3*100), ncol=3),
            order.by=(Sys.Date()+0:99)))
colnames(rand_walk) <-
  paste("rand_walk", 1:3, sep="_")
plot(rand_walk, main="Random walks",
     xlab="", ylab="", plot.type="single",
     col=c("black", "red", "blue"))
# add legend
legend(x="topleft",
 legend=colnames(rand_walk),
 col=c("black", "red", "blue"), lty=1)
library(zoo)  # load zoo
library(ggplot2)  # load ggplot2
set.seed(1121)  # initialize random number generator
rand_walk <- cumsum(zoo(matrix(rnorm(3*100), ncol=3),
            order.by=(Sys.Date()+0:99)))
colnames(rand_walk) <-
  paste("rand_walk", 1:3, sep="_")
plot(rand_walk, main="Random walks",
     xlab="", ylab="", plot.type="single",
     col=c("black", "red", "blue"))
# add legend
legend(x="topleft",
 legend=colnames(rand_walk),
 col=c("black", "red", "blue"), lty=1)
library(tseries)  # load tseries
# simulate AR(1) process
set.seed(1121)  # initialize random number generator
ari_ma <- arima.sim(n=729, model=list(ar=0.8))
tseries::adf.test(ari_ma)
set.seed(1121)  # initialize random number generator
ari_ma <- arima.sim(n=10000, model=list(ar=0.8))
tseries::adf.test(ari_ma)
set.seed(1121)  # initialize random number generator
rand_walk <- cumsum(rnorm(729))
tseries::adf.test(rand_walk)
set.seed(1121)  # initialize random number generator
rand_walk <- cumsum(rnorm(10000))
tseries::adf.test(rand_walk)
par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # set plot panels
ar3_zoo <- zoo(  # AR(3) time series of returns
  x=arima.sim(n=365,
    model=list(ar=c(0.1, 0.5, 0.1))),
  order.by=in_dex)  # zoo_arima
# ACF of AR(3) process
acf_plus(ar3_zoo, lag=10,
 xlab="", ylab="", main="ACF of AR(3) process")

# PACF of AR(3) process
pacf(ar3_zoo, lag=10,
     xlab="", ylab="", main="PACF of AR(3) process")
ar3_zoo <- arima.sim(n=1000,
      model=list(ar=c(0.1, 0.3, 0.1)))
arima(ar3_zoo, order = c(5,0,0))  # fit AR(5) model
library(forecast)  # load forecast
auto.arima(ar3_zoo)  # fit ARIMA model
# define Ornstein-Uhlenbeck parameters
eq_price <- 5.0; vol_at <- 0.01
the_ta <- 0.01; len_gth <- 1000
# simulate Ornstein-Uhlenbeck process
re_turns <- numeric(len_gth)
price_s <- numeric(len_gth)
price_s[1] <- 5.0
set.seed(1121)  # reset random numbers
for (i in 2:len_gth) {
  re_turns[i] <- the_ta*(eq_price - price_s[i-1]) +
    vol_at*rnorm(1)
  price_s[i] <- price_s[i-1] * exp(re_turns[i])
}  # end for
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
# define Ornstein-Uhlenbeck parameters
eq_price <- 5.0; the_ta <- 0.05
len_gth <- 1000
# simulate Ornstein-Uhlenbeck process
re_turns <- numeric(len_gth)
price_s <- numeric(len_gth)
price_s[1] <- 5.0
set.seed(1121)  # reset random numbers
for (i in 2:len_gth) {
  re_turns[i] <- the_ta*(eq_price - price_s[i-1]) +
    vol_at*rnorm(1)
  price_s[i] <- price_s[i-1] * exp(re_turns[i])
}  # end for
re_turns <- rutils::diff_it(log(price_s))
lag_price <- rutils::lag_it(price_s)
lag_price[1] <- lag_price[2]
for_mula <- re_turns ~ lag_price
l_m <- lm(for_mula)
summary(l_m)
# plot regression
plot(for_mula, main="returns versus lagged prices")
abline(l_m, lwd=2, col="red")
# define Ornstein-Uhlenbeck parameters
eq_price <- 5.0; vol_at <- 0.01
the_ta <- 0.01; len_gth <- 1000
# simulate Ornstein-Uhlenbeck process
re_turns <- numeric(len_gth)
price_s <- numeric(len_gth)
price_s[1] <- 5.0
set.seed(1121)  # reset random numbers
for (i in 2:len_gth) {
  re_turns[i] <- the_ta*(eq_price - price_s[i-1]) +
    vol_at*rnorm(1)
  price_s[i] <- price_s[i-1] * exp(re_turns[i])
}  # end for
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
# create name corresponding to "^GSPC" symbol
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
# library(xts)  # load package xts
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
