library(knitr)
opts_chunk$set(prompt=TRUE, eval=FALSE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
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
agg_regations <- do_call_rbind(agg_regations)
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
  agg_regations <- do_call_rbind(agg_regations)
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
inter_val <- 11
# calculate number of inter_vals that fit over price_s
n_row <- NROW(price_s)
num_agg <- n_row %/% inter_val
# if n_row==inter_val*num_agg then whole number
# of inter_vals fit over price_s
end_points <- inter_val*(0:num_agg)
# if (n_row > inter_val*num_agg)
# then stub interval at beginning
end_points <-
  c(0, n_row-inter_val*num_agg+inter_val*(0:num_agg))
# stub interval at end
end_points <- c(inter_val*(0:num_agg), n_row)
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
  c(0, n_row-inter_val*num_agg+inter_val*(0:num_agg))
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
  c(0, n_row-inter_val*num_agg+inter_val*(0:num_agg))
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
agg_regations <- do_call_rbind(agg_regations)
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
  agg_regations <- do_call_rbind(agg_regations)
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
  c(0, n_row-inter_val*num_agg+inter_val*(0:num_agg))
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
agg_regations <- do_call_rbind(agg_regations)
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
  c(0, n_row-inter_val*num_agg+inter_val*(0:num_agg))
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
agg_regations <- do_call_rbind(agg_regations)
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
  env_etf$re_turns[, c("XLF", "XLP", "IEF")], lwd=2,
  ylab="", legend.loc="topleft", main="")
# add title
title(main="ETF cumulative returns", line=-1)
load(file="C:/Develop/data/etf_data.RData")
options(width=200)
library(PerformanceAnalytics)
chart.Drawdown(env_etf$re_turns[, "VTI"], ylab="",
         main="VTI drawdowns")
load(file="C:/Develop/data/etf_data.RData")
options(width=200)
library(PerformanceAnalytics)
table.Drawdowns(env_etf$re_turns[, "VTI"])
library(PerformanceAnalytics)
chart.Histogram(env_etf$re_turns[, 1], main="",
  xlim=c(-0.06, 0.06),
  methods = c("add.density", "add.normal"))
# add title
title(main=paste(colnames(env_etf$re_turns[, 1]),
           "density"), line=-1)
library(PerformanceAnalytics)
chart.Boxplot(env_etf$re_turns[,
  c("VTI", "IEF", "IVW", "VYM", "IWB", "DBC", "VXX")])
library(PerformanceAnalytics)
tail(table.Stats(env_etf$re_turns[,
  c("VTI", "IEF", "DBC", "VXX")]), 4)
risk_return <- table.Stats(env_etf$re_turns)
class(risk_return)
# Transpose the data frame
risk_return <- as.data.frame(t(risk_return))
# plot scatterplot
plot(Kurtosis ~ Skewness, data=risk_return,
     main="Kurtosis vs Skewness")
# add labels
text(x=risk_return$Skewness, y=risk_return$Kurtosis,
    labels=rownames(risk_return),
    pos=1, cex=0.8)
load(file="C:/Develop/data/etf_data.RData")
# add skew_kurt column
risk_return$skew_kurt <-
  risk_return$Skewness/risk_return$Kurtosis
# sort on skew_kurt
risk_return <- risk_return[
  order(risk_return$skew_kurt,
  decreasing=TRUE), ]
# add names column
risk_return$Name <-
  etf_list[rownames(risk_return), ]$Name
risk_return[, c("Name", "Skewness", "Kurtosis")]
library(PerformanceAnalytics)
chart.RiskReturnScatter(
  env_etf$re_turns[, colnames(env_etf$re_turns)!="VXX"],
  Rf=0.01/12)
library(PerformanceAnalytics)
vti_ief <- env_etf$re_turns[, c("VTI", "IEF")]
SharpeRatio(vti_ief)

SortinoRatio(vti_ief)

CalmarRatio(vti_ief)
tail(table.Stats(vti_ief), 4)
