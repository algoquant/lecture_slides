library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
thm <- knit_theme$get("acid")
knit_theme$set(thm)
set.seed(1121)  # for reproducibility
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
library(zoo)  # load package zoo
# create zoo time series
date_index <- Sys.Date() + 0:365
zoo_series <- zoo(rnorm(length(date_index)), order.by=date_index)
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
# plot original and aggregated zoo
plot(cumsum(zoo_series), xlab="", ylab="")
lines(cumsum(zoo_agg), lwd=2, col="red")
# add legend
legend("topright", inset=0.05, cex=0.8, title="Aggregated Prices", 
 leg=c("orig prices", "agg prices"), lwd=2, bg="white", 
 col=c("black", "red"))
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
# perform monthly 'mean' aggregation
zoo_mean <- rollapply(zoo_series, width=11, FUN=mean)
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
library(lubridate)  # load lubridate
library(zoo)  # load package zoo
# methods(as.zoo)  # many methods of coercing into zoo
class(EuStockMarkets)  # multiple ts object
# coerce mts object into zoo
zoo_series <- as.zoo(EuStockMarkets)
class(index(zoo_series))  # index is numeric
head(zoo_series, 3)
# approximately convert index into class 'Date'
index(zoo_series) <- 
  as.Date(365*(index(zoo_series)-1970))
head(zoo_series, 3)
# convert index into class 'POSIXct'
zoo_series <- as.zoo(EuStockMarkets)
index(zoo_series) <- date_decimal(index(zoo_series))
head(zoo_series, 3)
library(lubridate)  # load lubridate
library(zoo)  # load package zoo
set.seed(1121)  # initialize the random number generator
# create index of daily dates
date_index <- seq(from=as.Date("2014-07-14"), 
            by="day", length.out=1000)
# create vector of data
zoo_data <- cumsum(rnorm(length(date_index)))
# create zoo time series
zoo_series <- zoo(x=zoo_data, 
            order.by=date_index)
head(zoo_series, 3)  # zoo object
# as.ts() creates ts object with frequency=1
ts_series <- as.ts(zoo_series)
tsp(ts_series)  # frequency=1
# get start and end dates of zoo_series
start_date <- decimal_date(start(zoo_series))
end_date <- decimal_date(end(zoo_series))
# calculate frequency of zoo_series
fre_quency <- length(zoo_series)/(end_date-start_date)
da_ta <- coredata(zoo_series)  # extract data from zoo_series
# create ts object using ts()
ts_series <- ts(data=da_ta, start=start_date, 
          frequency=fre_quency)
# display start of time series
window(ts_series, start=start(ts_series), 
 end=start(ts_series)+4/365)
head(time(ts_series))  # display index dates
head(as.Date(date_decimal(coredata(time(ts_series)))))
library(lubridate)  # load lubridate
library(zoo)  # load package zoo
# create weekday logical vector
is_weekday <- !((weekdays(index(zoo_series)) == "Saturday") | 
  (weekdays(index(zoo_series)) == "Sunday"))
# remove weekends from zoo time series
zoo_series <- zoo_series[is_weekday, ]
head(zoo_series, 7)  # zoo object
# as.ts() creates NA values
ts_series <- as.ts(zoo_series)
head(ts_series, 7)
# create vector of regular dates, including weekends
date_index <- seq(from=start(zoo_series), 
            by="day", 
            length.out=length(zoo_series))
index(zoo_series) <- date_index
ts_series <- as.ts(zoo_series)
head(ts_series, 7)
## # get documentation for package "tseries"
## packageDescription("tseries")  # get short description
## 
## help(package="tseries")  # load help page
## 
## library(tseries)  # load package "tseries"
## 
## data(package="tseries")  # list all datasets in "tseries"
## 
## ls("package:tseries")  # list all objects in "tseries"
## 
## detach("package:tseries")  # remove tseries from search path
## library(tseries)  # load package tseries
## # download MSFT data
## zoo_stx <- suppressWarnings(
##   get.hist.quote(
##     instrument="MSFT",
##     start=Sys.Date()-3*365,
##     end=Sys.Date(),
##     quote=c("Open","High","Low","Close",
##       "AdjClose","Volume"),
##     origin="1970-01-01")
## )  # end suppressWarnings
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
## library(tseries)  # load package tseries
## # download MSFT data in ts format
## ts_stx <- suppressWarnings(
##   get.hist.quote(
##     instrument="MSFT",
##     start=Sys.Date()-3*365,
##     end=Sys.Date(),
##     retclass="ts",
##     quote=c("Open","High","Low","Close",
##       "AdjClose","Volume"),
##     origin="1970-01-01")
## )  # end suppressWarnings
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
## library(tseries)  # load package tseries
## # download EUR/USD data
## zoo_eurusd <- suppressWarnings(
##   get.hist.quote(
##     instrument="EUR/USD",
##     provider="oanda",
##     start=Sys.Date()-3*365,
##     end=Sys.Date(),
##     origin="1970-01-01")
## )  # end suppressWarnings
## # bind and scrub data
## zoo_stxeur <- merge(zoo_eurusd,
##                zoo_stx[, "AdjClose"])
## colnames(zoo_stxeur) <- c("EURUSD", "MSFT")
## zoo_stxeur <-
##   zoo_stxeur[complete.cases(zoo_stxeur),]
## save(zoo_stx, zoo_stx_adj,
##      ts_stx, ts_stx_adj,
##      zoo_eurusd, zoo_stxeur,
##      file="C:/Develop/data/zoo_data.RData")
load(file="C:/Develop/data/zoo_data.RData")
# inspect the data
class(zoo_eurusd)
tail(zoo_eurusd, 4)
## par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
## load(file="C:/Develop/data/zoo_data.RData")
## # get start and end dates
## in_dex <- time(ts_stx_adj)
## e_nd <- in_dex[length(in_dex)]
## st_art <- round((4*e_nd + in_dex[1])/5)
## # plot using plotOHLC
## plotOHLC(window(ts_stx_adj,
##           start=st_art,
##           end=e_nd)[, 1:4],
##    xlab="", ylab="")
## title(main="MSFT OHLC Prices")
## par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
## load(file="C:/Develop/data/zoo_data.RData")
## # plot using plot.zoo method
## seqplot.ts(zoo_stx[, "AdjClose"], xlab="", ylab="")
## title(main="MSFT AdjClose Prices", line=-1)
## par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
## load(file="C:/Develop/data/zoo_data.RData")
## ### plot with two "y" axes
## par(las=1)  # set text printing to "horizontal"
## # plot first ts
## plot(zoo_stxeur[, 1], xlab=NA, ylab=NA)
## # set range of "y" coordinates for second axis
## par(usr=c(par("usr")[1:2], range(zoo_stxeur[,2])))
## lines(zoo_stxeur[, 2], col="red")  # second plot
## axis(side=4, col="red")  # second "y" axis on right
## # print axis labels
## mtext(colnames(zoo_stxeur)[1], side=2, padj=-6, line=-4)
## mtext(colnames(zoo_stxeur)[2], col="red", side=4, padj=-2, line=-3)
## title(main="EUR and MSFT")  # add title
## # add legend without box
## legend("bottomright", legend=colnames(zoo_stxeur), bg="white",
##  lty=c(1, 1), lwd=c(2, 2), col=c("black", "red"), bty="n")
## 
## ##########
## 
## # slightly different method using par(new=TRUE)
## # par(las=1)  # set text printing to "horizontal"
## # plot(zoo_stxeur[, 1], xlab=NA, ylab=NA)
## # par(new=TRUE)  # allow new plot on same chart
## # plot(zoo_stxeur[, 2], xlab=NA, ylab=NA, yaxt="n", col="red")
## # axis(side=4, col="red")  # second "y" axis on right
## # mtext(colnames(zoo_stxeur)[1], side=2, padj=-6, line=-4)
## # mtext(colnames(zoo_stxeur)[2], col="red", side=4, padj=-2, line=-3)
## # title(main="EUR and MSFT", line=-1)  # add title
## # legend("bottomright", legend=colnames(zoo_stxeur),
## #        lty=c(1, 1), lwd=c(2, 2), col=c("black", "red"), bty="n")
## 
## ##########
## 
## # "x" axis with monthly ticks - doesn't work
## # plot first ts wthout "x" axis
## # plot(zoo_stxeur[, 1], xaxt="n", xlab=NA, ylab=NA)
## # # add "x" axis with monthly ticks
## # month.ticks <- unique(as.yearmon(index(zoo_eurusd)))
## # axis(side=1, at=month.ticks, labels=format(month.ticks, "%b-%y"), tcl=-0.7)
## 
library(tseries)  # load package tseries
load(file="C:/Develop/data/zoo_data.RData")
# calculate maximum drawdown
maxdrawdown(na.remove(ts_stx[, "AdjClose"]))
# calculate Sharpe ratio
sharpe(na.remove(ts_stx[, "AdjClose"]))
# calculate Sterling ratio
sterling(na.remove(ts_stx[, "AdjClose"]))
set.seed(1121)  # initialize the random number generator
library(xts)  # load package xts
# create xts time series
date_index <- Sys.Date() + 0:3
xts_series <- xts(rnorm(length(date_index)), 
         order.by=date_index)
names(xts_series) <- "random"
xts_series
tail(xts_series, 3)  # get last few elements
first(xts_series)  # get first element
last(xts_series)  # get last element
class(xts_series)  # class 'xts'
attributes(xts_series)
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
load(file="C:/Develop/data/zoo_data.RData")
library(xts)  # load package xts
# as.xts() creates xts from zoo
xts_stx <- as.xts(zoo_stx_adj)
dim(xts_stx)
head(xts_stx)
# plot using plot.xts method
plot(xts_stx[, "AdjClose"], xlab="", ylab="", main="")
title(main="MSFT Prices")  # add title
load(file="C:/Develop/data/zoo_data.RData")
library(xts)  # load package xts
# subset xts using a date range
xts_sub <- xts_stx["2013-10-15/2014-01-10", 1:4]
head(xts_sub)
tail(xts_sub)
# subset xts using a date
xts_sub <- xts_stx["2014", 1:4]
## par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
## load(file="C:/Develop/data/zoo_data.RData")
## library(xts)  # load package xts
## # as.xts() creates xts from zoo
## xts_stx <- as.xts(zoo_stx_adj)
## # subset xts using a date
## xts_sub <- xts_stx["2014-11", 1:4]
## 
## # plot OHLC using plot.xts method
## plot(xts_sub, type="candles", main="")
## title(main="MSFT Prices")  # add title
load(file="C:/Develop/data/zoo_data.RData")
ts_stx <- as.ts(zoo_stx)
class(ts_stx)
tail(ts_stx[, 1:4])
library(xts)
xts_stx <- as.xts(zoo_stx)
class(xts_stx)
tail(xts_stx[, 1:4])
### Perform two-tailed test that sample is 
### from Standard Normal Distribution (mean=0, SD=1)
# generate vector of samples and store in data frame
test_frame <- data.frame(samples=rnorm(1000))

# significance level, two-tailed test, critical value=2*SD
signif_level <- 2*(1-pnorm(2))
signif_level
# get p-values for all the samples
test_frame$p_values <- sapply(test_frame$samples, pnorm)
test_frame$p_values <- 2*(0.5-abs(test_frame$p_values-0.5))
# compare p_values to significance level
test_frame$result <- test_frame$p_values > signif_level
sum(!test_frame$result)  # number of null rejections
# show null rejections
head(test_frame[!test_frame$result, ])
rm(list=ls())
par(oma=c(1, 1, 1, 1), mgp=c(2, 0.5, 0), mar=c(5, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
library(ggplot2)  # load ggplot2

qplot(  # simple ggplot2
    main="Standard Normal Distribution", 
    c(-4, 4), 
    stat="function", 
    fun=dnorm, 
    geom="line", 
    xlab=NULL, ylab=NULL
    ) +  # end qplot

theme(  # modify plot theme
    plot.title=element_text(vjust=-1.0), 
    plot.background=element_blank()
    ) +  # end theme

geom_vline(  # add vertical line
  aes(xintercept=c(-2.0, 2.0)), 
  colour="red", 
  linetype="dashed"
  )  # end geom_vline
rm(list=ls())
par(oma=c(1, 1, 1, 1), mgp=c(2, 0.5, 0), mar=c(5, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
### create ggplot2 with shaded area
x_var <- -400:400/100
norm_frame <- data.frame(x_var=x_var, 
                 d.norm=dnorm(x_var))
norm_frame$shade <- ifelse(
            abs(norm_frame$x_var) >= 2, 
            norm_frame$d.norm, NA)
ggplot(  # main function
  data=norm_frame, 
  mapping=aes(x=x_var, y=d.norm)
  ) +  # end ggplot
# plot line
  geom_line() + 
# plot shaded area
  geom_ribbon(aes(ymin=0, ymax=shade), fill="red") + 
# no axis labels
  xlab("") + ylab("") + 
# add title
  ggtitle("Standard Normal Distribution") +
# modify plot theme
  theme(
  plot.title=element_text(vjust=-1.0), 
  plot.background=element_blank()
  )  # end theme
# calculate DAX percentage returns
dax_rets <- diff(log(EuStockMarkets[, 1]))

# Shapiro-Wilk test for normal distribution
shapiro.test(rnorm(length(dax_rets)))

# Shapiro-Wilk test for DAX returns
shapiro.test(dax_rets)

# Shapiro-Wilk test for uniform distribution
shapiro.test(runif(length(dax_rets)))
dax_rets <- diff(log(EuStockMarkets[, 1]))
library(tseries)  # load package tseries

# Jarque-Bera test for normal distribution
jarque.bera.test(rnorm(length(dax_rets)))

# Jarque-Bera test for DAX returns
jarque.bera.test(dax_rets)

# Jarque-Bera test for uniform distribution
jarque.bera.test(runif(length(dax_rets)))
par(mar=c(5,0,1,2), oma=c(1,2,1,0), mgp=c(2,1,0), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
library(zoo)  # load package zoo
# autocorrelation from "stats"
acf(coredata(dax_rets), lag=10, main="")
title(main="acf of DAX returns", line=-1)
library(zoo)  # load package zoo
dax_acf <- acf(coredata(dax_rets), plot=FALSE)
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
  if(plot) {
    ci <- qnorm((1+0.95)/2)*sqrt(1/length(ts_data))
    ylim <- c(min(-ci, range(acf_data$acf[-1, , 1])),
        max(ci, range(acf_data$acf[-1, , 1])))
    plot(acf_data, xlab=xlab, ylab=ylab, 
   ylim=ylim, main=main, ci=0)
    abline(h=c(-ci, ci), col="blue", lty=2)
  }
  invisible(acf_data)  # return invisibly
}  # end acf_plus
par(mar=c(5,0,1,2), oma=c(1,2,1,0), mgp=c(2,1,0), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
library(zoo)  # load package zoo
# improved autocorrelation function
acf_plus(coredata(dax_rets), lag=10, main="")
title(main="acf of DAX returns", line=-1)
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
# Ljung-Box test for DAX data
# 'lag' is the number of autocorrelation coefficients
Box.test(dax_rets, lag=10, type="Ljung")

# changes in 3 month T-bill rate are autocorrelated
Box.test(macro_diff[, "3mTbill"], 
   lag=10, type="Ljung")

# changes in unemployment rate are autocorrelated
Box.test(macro_diff[, "unemprate"], 
   lag=10, type="Ljung")
library(zoo)  # load zoo
library(ggplot2)  # load ggplot2
library(gridExtra)  # load gridExtra
# extract DAX time series
dax_ts <- EuStockMarkets[, 1]
# filter past values only (sides=1)
dax_filt <- filter(dax_ts, 
             filter=rep(1/5,5), sides=1)
# coerce to zoo and merge the time series
dax_filt <- merge(as.zoo(dax_ts), 
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
dax_rets <- na.omit(diff(log(dax_filt)))
par(mfrow=c(2,1))  # set plot panels

acf_plus(coredata(dax_rets[, 1]), lag=10, 
   xlab="")
title(main="DAX", line=-1)

acf_plus(coredata(dax_rets[, 2]), lag=10, 
   xlab="")
title(main="DAX filtered", line=-1)
par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # set plot panels
# autocorrelation from "stats"
acf_plus(dax_rets[, 2], lag=10, xlab=NA, ylab=NA)
title(main="DAX filtered autocorrelations", line=-1)
# partial autocorrelation
pacf(dax_rets[, 2], lag=10, xlab=NA, ylab=NA)
title(main="DAX filtered partial autocorrelations", 
      line=-1)
# ARIMA processes
library(ggplot2)  # load ggplot2
library(gridExtra)  # load gridExtra
daily_index <- Sys.Date() + 0:364  # one year daily series
ar_zoo <- zoo(  # AR time series of returns
  x=arima.sim(n=365, model=list(ar=0.2)),
  order.by=daily_index)  # ar_zoo
ar_zoo <- cbind(ar_zoo, cumsum(ar_zoo))
colnames(ar_zoo) <- c("AR returns", "AR prices")
# plot AR returns
autoplot(object=ar_zoo, 
 facets="Series ~ .", 
 main="Autoregressive process (phi=0.2)") + 
  facet_grid("Series ~ .", scales="free_y") +
  xlab("") + ylab("") + 
theme(
  legend.position=c(0.1, 0.5), 
#  plot.title=element_text(vjust=-1.0), 
  plot.background=element_blank(),
  axis.text.y=element_blank())
ar_coeff <- c(0.01, 0.4, 0.8)  # AR coefficients
ar_zoo <- sapply(  # create three AR time series
  ar_coeff, function(phi)
    arima.sim(n=365, model=list(ar=phi)))
ar_zoo <- zoo(x=ar_zoo, order.by=daily_index)
ar_zoo <- cumsum(ar_zoo)  # returns to prices
colnames(ar_zoo) <- paste("autocorr", ar_coeff)
autoplot(ar_zoo, main="AR prices", 
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
# ACF of AR(1) process
acf_plus(na.omit(diff(ar_zoo[, 2])), lag=10, 
 xlab="", ylab="", main="ACF of AR(1) process")

# PACF of AR(1) process
pacf(na.omit(diff(ar_zoo[, 2])), lag=10,
     xlab="", ylab="", main="PACF of AR(1) process")
par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # set plot panels
ar3_zoo <- zoo(  # AR(3) time series of returns
  x=arima.sim(n=365, 
    model=list(ar=c(0.1, 0.5, 0.1))),
  order.by=daily_index)  # ar_zoo
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
