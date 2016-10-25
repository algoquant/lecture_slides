library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
options(digits=3)
thm <- knit_theme$get("acid")
knit_theme$set(thm)
# library(xts)  # load package xts
library(HighFreq)  # load package HighFreq
price_s <- Cl(SPY["2012-02-13"])  # extract closing minutely prices
end_points <- 0:NROW(price_s)  # define end points
len_gth <- length(end_points)
win_dow <- 11  # number of data points per look-back window
# define starting points as lag of end_points
start_points <-  end_points[
  c(rep_len(1, win_dow), 1:(len_gth-win_dow))] + 1
# define aggregation function
agg_regate <- function(x_ts)
  c(max=max(x_ts), min=min(x_ts))
# perform aggregations over length of end_points
agg_regations <- sapply(2:len_gth,
    function(in_dex) {
agg_regate(.subset_xts(price_s,
  start_points[in_dex]:end_points[in_dex]))
  })  # end sapply
# coerce agg_regations into matrix and transpose it
if (is.vector(agg_regations))
  agg_regations <- t(agg_regations)
agg_regations <- t(agg_regations)
# coerce agg_regations into xts series
agg_regations <- xts(agg_regations,
               order.by=index(price_s[end_points]))
library(HighFreq)  # load package HighFreq
price_s <- Cl(SPY["2012-02-13"])  # extract closing minutely prices
end_points <- 0:NROW(price_s)  # define end points
len_gth <- length(end_points)
win_dow <- 11  # number of data points per look-back window
# define starting points as lag of end_points
start_points <-  end_points[
  c(rep_len(1, win_dow), 1:(len_gth-win_dow))] + 1
# define aggregation function
agg_regate <- function(x_ts)
  xts(t(c(max=max(x_ts), min=min(x_ts))),
order.by=end(x_ts))
# perform aggregations over length of end_points
agg_regations <- lapply(2:len_gth,
    function(in_dex) {
agg_regate(.subset_xts(price_s,
  start_points[in_dex]:end_points[in_dex]))
  })  # end lapply
# rbind list into single xts or matrix
agg_regations <- rutils::do_call_rbind(agg_regations)
# plot aggregations with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("red", "green")
chart_Series(agg_regations, theme=plot_theme,
       name="price aggregations")
legend("bottomright", legend=colnames(agg_regations),
bg="white", lty=c(1, 1), lwd=c(2, 2),
col=plot_theme$col$line.col, bty="n")
# library(xts)  # load package xts
# define functional for rolling aggregations
roll_agg <- function(x_ts, win_dow, FUN, ...) {
# define end points at every point
  end_points <- 0:NROW(x_ts)
  len_gth <- length(end_points)
# define starting points as lag of end_points
  start_points <-  end_points[
    c(rep_len(1, win_dow), 1:(len_gth-win_dow))] + 1
# perform aggregations over length of end_points
  agg_regations <- lapply(2:len_gth,
        function(in_dex) {
          FUN(.subset_xts(x_ts,
              start_points[in_dex]:end_points[in_dex]), ...)
        })  # end lapply
# rbind list into single xts or matrix
  agg_regations <- rutils::do_call_rbind(agg_regations)
# coerce agg_regations into xts series
  if (!is.xts(agg_regations))
    agg_regations <-
xts(agg_regations, order.by=index(x_ts[end_points]))
  agg_regations
}  # end roll_agg
# define aggregation function
agg_regate <- function(x_ts)
  c(max=max(x_ts), min=min(x_ts))
# perform aggregations over rolling window
agg_regations <- roll_agg(price_s, win_dow=win_dow,
              FUN=agg_regate)
# library(xts)  # load package xts
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
  agg_vector=roll_agg(price_s, win_dow=win_dow,
              FUN=agg_vector),
  agg_xts=roll_agg(price_s, win_dow=win_dow,
              FUN=agg_xts),
  times=10))[, c(1, 4, 5)]
# library(xts)  # load package xts
# define aggregation function that returns a single value
agg_regate <- function(x_ts)  max(x_ts)
# perform aggregations over length of end_points
agg_regations <- xts:::rollapply.xts(price_s, width=win_dow,
              FUN=agg_regate, align="right")
# perform aggregations over length of end_points
library(PerformanceAnalytics)  # load package PerformanceAnalytics
agg_regations <- apply.rolling(price_s,
              width=win_dow, FUN=agg_regate)
# benchmark the speed of the functionals
library(microbenchmark)
summary(microbenchmark(
  roll_agg=roll_agg(price_s, win_dow=win_dow,
              FUN=max),
  roll_xts=xts:::rollapply.xts(price_s, width=win_dow,
                 FUN=max, align="right"),
  apply_rolling=apply.rolling(price_s,
                        width=win_dow, FUN=max),
  times=10))[, c(1, 4, 5)]
# library(xts)  # load package xts
# rolling sum using cumsum()
roll_sum <- function(x_ts, win_dow) {
  cum_sum <- cumsum(na.omit(x_ts))
  out_put <- cum_sum - lag(x=cum_sum, k=win_dow)
  out_put[1:win_dow, ] <- cum_sum[1:win_dow, ]
  colnames(out_put) <- paste0(colnames(x_ts), "_stdev")
  out_put
}  # end roll_sum
agg_regations <- roll_sum(price_s, win_dow=win_dow)
# perform rolling aggregations using apply loop
agg_regations <- sapply(2:len_gth,
    function(in_dex) {
sum(.subset_xts(price_s,
  start_points[in_dex]:end_points[in_dex]))
  })  # end sapply
head(agg_regations)
tail(agg_regations)
# benchmark the speed of both methods
library(microbenchmark)
summary(microbenchmark(
  roll_sum=roll_sum(price_s, win_dow=win_dow),
  s_apply=sapply(2:len_gth,
    function(in_dex) {
sum(.subset_xts(price_s,
  start_points[in_dex]:end_points[in_dex]))
  }),
  times=10))[, c(1, 4, 5)]
# library(xts)  # load package xts
# benchmark the speed of runSum
library(microbenchmark)
summary(microbenchmark(
  roll_sum=roll_sum(price_s, win_dow=win_dow),
  run_sum=runSum(price_s, n=win_dow),
  times=10))[, c(1, 4, 5)]
# library(xts)  # load package xts
library(caTools)  # load package "caTools"
# get documentation for package "caTools"
packageDescription("caTools")  # get short description
help(package="caTools")  # load help page
data(package="caTools")  # list all datasets in "caTools"
ls("package:caTools")  # list all objects in "caTools"
detach("package:caTools")  # remove caTools from search path
# median filter
win_dow <- 11
price_s <- Cl(SPY["2012-02-01/2012-04-01"])
med_ian <- runmed(x=price_s, k=win_dow)
# vector of rolling volatility
vo_lat <- runsd(x=price_s, k=win_dow,
          endrule="constant", align="center")
# vector of rolling quantiles
quan_tiles <- runquantile(x=price_s,
            k=win_dow, probs=0.9,
            endrule="constant",
            align="center")
# library(xts)  # load package xts
library(quantmod)  # load package quantmod
library(HighFreq)  # load package HighFreq
# extract a single day of minutely price data
price_s <- Cl(SPY["2012-02-13"])
# define number of data points per interval
win_dow <- 11
# calculate number of win_dows that fit over price_s
n_row <- NROW(price_s)
num_agg <- n_row %/% win_dow
# if n_row==win_dow*num_agg then whole number
# of win_dows fit over price_s
end_points <- win_dow*(0:num_agg)
# if (n_row > win_dow*num_agg)
# then stub interval at beginning
end_points <-
  c(0, n_row-win_dow*num_agg+win_dow*(0:num_agg))
# stub interval at end
end_points <- c(win_dow*(0:num_agg), n_row)
# plot data and endpoints as vertical lines
plot_theme <- chart_theme()
plot_theme$col$line.col <- "blue"
chart_Series(price_s, theme=plot_theme,
  name="prices with endpoints as vertical lines")
abline(v=end_points, col='red')
# library(xts)  # load package xts
# indices of last observations in each hour
end_points <- endpoints(price_s, on='hours')
head(end_points)
# extract the last observations in each hour
head(price_s[end_points, ])
# library(xts)  # load package xts
end_points <- # define end_points with beginning stub
  c(0, n_row-win_dow*num_agg+win_dow*(0:num_agg))
len_gth <- length(end_points)
# define starting points as lag of end_points
start_points <- end_points[c(1, 1:(len_gth-1))] + 1
# perform sapply() loop over length of end_points
agg_regations <- sapply(2:len_gth,
    function(in_dex) {
x_ts <-
  price_s[start_points[in_dex]:end_points[in_dex]]
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
legend("bottomright", legend=colnames(agg_regations),
bg="white", lty=c(1, 1), lwd=c(2, 2),
col=plot_theme$col$line.col, bty="n")
# library(xts)  # load package xts
end_points <- # define end_points with beginning stub
  c(0, n_row-win_dow*num_agg+win_dow*(0:num_agg))
len_gth <- length(end_points)
# define starting points as lag of end_points
start_points <- end_points[c(1, 1:(len_gth-1))] + 1
# perform lapply() loop over length of end_points
agg_regations <- lapply(2:len_gth,
    function(in_dex) {
x_ts <-
  price_s[start_points[in_dex]:end_points[in_dex]]
xts(t(c(max=max(x_ts), min=min(x_ts))),
      order.by=index(price_s[end_points[in_dex]]))
  })  # end lapply
# rbind list into single xts or matrix
agg_regations <- rutils::do_call_rbind(agg_regations)
head(agg_regations)
# plot aggregations with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("red", "green")
chart_Series(agg_regations, theme=plot_theme,
       name="price aggregations")
legend("bottomright", legend=colnames(agg_regations),
bg="white", lty=c(1, 1), lwd=c(2, 2),
col=plot_theme$col$line.col, bty="n")
# library(xts)  # load package xts
# define functional for rolling aggregations over end_points
roll_agg <- function(x_ts, end_points, FUN, ...) {
  len_gth <- length(end_points)
# define starting points as lag of end_points
  start_points <- end_points[c(1, 1:(len_gth-1))] + 1
# perform aggregations over length of end_points
  agg_regations <- lapply(2:len_gth,
    function(in_dex) FUN(.subset_xts(x_ts,
start_points[in_dex]:end_points[in_dex]), ...))  # end lapply
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
# library(xts)  # load package xts
# load package HighFreq
library(HighFreq)
# extract closing minutely prices
price_s <- Cl(SPY["2012-02-01/2012-04-01"])
# apply "mean" over daily periods
agg_regations <- apply.daily(price_s, FUN=sum)
head(agg_regations)
# library(xts)  # load package xts
library(HighFreq)  # load package HighFreq
end_points <- # define end_points with beginning stub
  c(0, n_row-win_dow*num_agg+win_dow*(0:num_agg))
len_gth <- length(end_points)
win_dow <- 3  # number of look-back time intervals
# define starting points as lag of end_points
start_points <-  end_points[
  c(rep_len(1, win_dow), 1:(len_gth-win_dow))] + 1
# perform lapply() loop over length of end_points
agg_regations <- lapply(2:len_gth,
    function(in_dex) {
x_ts <-
  price_s[start_points[in_dex]:end_points[in_dex]]
xts(t(c(max=max(x_ts), min=min(x_ts))),
      order.by=index(price_s[end_points[in_dex]]))
  })  # end lapply
# rbind list into single xts or matrix
agg_regations <- rutils::do_call_rbind(agg_regations)
# plot aggregations with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("red", "green")
chart_Series(agg_regations, theme=plot_theme,
       name="price aggregations")
legend("bottomright", legend=colnames(agg_regations),
bg="white", lty=c(1, 1), lwd=c(2, 2),
col=plot_theme$col$line.col, bty="n")
# library(xts)  # load package xts
library(HighFreq)  # load package HighFreq
end_points <- # define end_points with beginning stub
  c(0, n_row-win_dow*num_agg+win_dow*(0:num_agg))
len_gth <- length(end_points)
win_dow <- 3  # number of look-back time intervals
# define starting points as lag of end_points
start_points <-  end_points[
  c(rep_len(1, win_dow), 1:(len_gth-win_dow))] + 1
# perform lapply() loop over length of end_points
agg_regations <- lapply(2:len_gth,
          function(in_dex) {mean(
price_s[start_points[in_dex]:end_points[in_dex]])
})  # end lapply
# rbind list into single xts or matrix
agg_regations <- rutils::do_call_rbind(agg_regations)
agg_regations <- xts(agg_regations,
    order.by=index(price_s[end_points]))
agg_regations <- cbind(price_s, agg_regations)
agg_regations <- na.omit(na.locf(agg_regations))
colnames(agg_regations)[2] <- "aggregations"
# plot aggregations with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("red", "green")
chart_Series(agg_regations, theme=plot_theme,
       name="price aggregations")
legend("bottomright", legend=colnames(agg_regations),
bg="white", lty=c(1, 1), lwd=c(2, 2),
col=plot_theme$col$line.col, bty="n")
set.seed(1121)  # reset random number generator
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
library(zoo)  # load package zoo
# create zoo time series of random returns
in_dex <- Sys.Date() + 0:365
zoo_series <-
  zoo(rnorm(length(in_dex)), order.by=in_dex)
# create monthly dates
dates_agg <- as.Date(as.yearmon(index(zoo_series)))
# perform monthly 'mean' aggregation
zoo_agg <- aggregate(zoo_series, by=dates_agg,
               FUN=mean)
# merge with original zoo - union of dates
zoo_agg <- merge(zoo_series, zoo_agg)
# replace NA's using locf
zoo_agg <- na.locf(zoo_agg)
# extract aggregated zoo
zoo_agg <- zoo_agg[index(zoo_series), 2]
# library(xts)  # load package xts
# plot original and aggregated cumulative returns
plot(cumsum(zoo_series), xlab="", ylab="")
lines(cumsum(zoo_agg), lwd=2, col="red")
# add legend
legend("topright", inset=0.05, cex=0.8,
 title="Aggregated Prices",
 leg=c("orig prices", "agg prices"),
 lwd=2, bg="white", col=c("black", "red"))
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# perform monthly 'mean' aggregation
zoo_agg <- aggregate(zoo_series, by=dates_agg,
               FUN=mean)
# merge with original zoo - union of dates
zoo_agg <- merge(zoo_series, zoo_agg)
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
# "mean" aggregation over window with width=11
zoo_mean <- rollapply(zoo_series, width=11,
                FUN=mean, align="right")
# merge with original zoo - union of dates
zoo_mean <- merge(zoo_series, zoo_mean)
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
load(file="C:/Develop/data/zoo_data.RData")
class(zoo_stx)
dim(zoo_stx)
head(zoo_stx, 4)
library(tseries)  # load package tseries
load(file="C:/Develop/data/zoo_data.RData")
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
  "DBC", "VXX", "XLY", "XLP", "XLE", "XLF", 
  "XLV", "XLI", "XLB", "XLK", "XLU", "VYM", 
  "IVW", "IWB", "IWD", "IWF")
# read etf database into data frame
etf_list <- read.csv(file='C:/Develop/data/etf_list.csv', 
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
etf_list["IEF", "Name"] <- 
  "Treasury Bond Fund"
etf_list["XLY", "Name"] <- 
  "Consumer Discr. Sector Fund"
etf_list[c(1, 2)]
print(xtable(etf_list), comment=FALSE, size="tiny")
load(file="C:/Develop/data/etf_data.RData")
library(quantmod)  # load package quantmod
env_etf <- new.env()  # new environment for data
# download data for sym_bols into env_etf
getSymbols(sym_bols, env=env_etf, adjust=TRUE,
    from="2007-01-03")
load(file="C:/Develop/data/etf_data.RData")
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
# create name corresponding to "^GSPC" ticker
setSymbolLookup(
  sp500=list(name="^GSPC", src="yahoo"))
getSymbolLookup()
# view and clear options
options("getSymbols.sources")
options(getSymbols.sources=NULL)
# download S&P500 prices into env_etf
getSymbols("sp500", env=env_etf,
    adjust=TRUE, from="1990-01-01")
chart_Series(x=env_etf$sp500["2016/"],
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
# create valid R names from tickers containing "-" or "."characters
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
# register tickers corresponding to R names
for (in_dex in 1:NROW(sp_500)) {
  cat("processing: ", sp_500$Ticker[in_dex], "\n")
  setSymbolLookup(structure(
    list(list(name=sp_500$Ticker[in_dex])),
    names=sp_500$names[in_dex]))
}  # end for
env_etf <- new.env()  # new environment for data
# remove all files (if necessary)
rm(list=ls(env_etf), envir=env_etf)
# download data and copy it into environment
HighFreq::get_symbols(sp_500$names,
   env_out=env_etf, start_date="1990-01-01")
# or download in loop
for (na_me in sp_500$names) {
  cat("processing: ", na_me, "\n")
  HighFreq::get_symbols(na_me,
   env_out=env_etf, start_date="1990-01-01")
}  # end for
save(env_etf, file="C:/Develop/data/sp500.RData")
chart_Series(x=env_etf$BRK_B["2016/"], TA="add_Vo()",
       name="BRK-B stock")
load(file="C:/Develop/data/etf_data.RData")
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
load(file="C:/Develop/data/etf_data.RData")
library(quantmod)  # load package quantmod
# extract and merge all data, subset by symbols
etf_series <- do.call(merge,
            as.list(env_etf)[sym_bols])

# extract and merge adjusted prices, subset by symbols
price_s <- do.call(merge,
         lapply(as.list(env_etf)[sym_bols], Ad))

# extract and merge adjusted prices, subset by symbols
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
load(file="C:/Develop/data/etf_data.RData")
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
load(file="C:/Develop/data/etf_data.RData")
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
load(file="C:/Develop/data/etf_data.RData")
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
load(file="C:/Develop/data/etf_data.RData")
# plot OHLC candlechart with volume
chartSeries(env_etf$VTI["2008-11/2009-04"],
      name="VTI")
# redraw plot only for Feb-2009, with white theme
reChart(subset="2009-02",
  theme=chartTheme("white"))
library(quantmod)
load(file="C:/Develop/data/etf_data.RData")
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
load(file="C:/Develop/data/etf_data.RData")
da_ta <- env_etf$VTI["2009-02/2009-03"]
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
da_ta <- env_etf$VTI
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
da_ta <- env_etf$VTI
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
da_ta <- env_etf$VTI["2009-02/2009-03"]
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
da_ta <- env_etf$VTI["2010-04/2010-05"]
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
chart_Series(x=env_etf$VTI["2009-02/2009-04"],
       name="VTI")
# plot in bottom panel
chart_Series(x=env_etf$XLF["2009-02/2009-04"],
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
