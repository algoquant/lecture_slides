library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
thm <- knit_theme$get("acid")
knit_theme$set(thm)
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
load(file="C:/Develop/data/zoo_data.RData")
#plot with two "y" axes
par(las=1)  # set text printing to "horizontal"
# plot first ts
plot(zoo_stxeur[, 1], xlab=NA, ylab=NA)
# set range of "y" coordinates for second axis
par(usr=c(par("usr")[1:2], range(zoo_stxeur[,2])))
lines(zoo_stxeur[, 2], col="red")  # second plot
axis(side=4, col="red")  # second "y" axis on right
# print axis labels
mtext(colnames(zoo_stxeur)[1], side=2, padj=-6, line=-4)
mtext(colnames(zoo_stxeur)[2], col="red", side=4, padj=-2, line=-3)
title(main="EUR and MSFT")  # add title
# add legend without box
legend("bottomright", legend=colnames(zoo_stxeur), bg="white",
 lty=c(1, 1), lwd=c(2, 2), col=c("black", "red"), bty="n")

##########

# slightly different method using par(new=TRUE)
# par(las=1)  # set text printing to "horizontal"
# plot(zoo_stxeur[, 1], xlab=NA, ylab=NA)
# par(new=TRUE)  # allow new plot on same chart
# plot(zoo_stxeur[, 2], xlab=NA, ylab=NA, yaxt="n", col="red")
# axis(side=4, col="red")  # second "y" axis on right
# mtext(colnames(zoo_stxeur)[1], side=2, padj=-6, line=-4)
# mtext(colnames(zoo_stxeur)[2], col="red", side=4, padj=-2, line=-3)
# title(main="EUR and MSFT", line=-1)  # add title
# legend("bottomright", legend=colnames(zoo_stxeur),
#        lty=c(1, 1), lwd=c(2, 2), col=c("black", "red"), bty="n")

##########

# "x" axis with monthly ticks - doesn't work
# plot first ts wthout "x" axis
# plot(zoo_stxeur[, 1], xaxt="n", xlab=NA, ylab=NA)
# # add "x" axis with monthly ticks
# month.ticks <- unique(as.yearmon(index(zoo_eurusd)))
# axis(side=1, at=month.ticks, labels=format(month.ticks, "%b-%y"), tcl=-0.7)

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
#Perform two-tailed test that sample is 
#from Standard Normal Distribution (mean=0, SD=1)
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
#create ggplot2 with shaded area
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
par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # set plot panels
# autocorrelation of squared DAX returns
acf_plus(coredata(dax_rets)^2,
   lag=10, main="")
title(main="acf of squared DAX returns",
line=-1)
# autocorrelation of squared random returns
acf_plus(rnorm(length(dax_rets))^2,
   lag=10, main="")
title(main="acf of squared random returns",
line=-1)
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
library(Ecdat)  # load Ecdat
macro_zoo <- as.zoo(Macrodat[, c("lhur", "fygm3")])
colnames(macro_zoo) <- c("unemprate", "3mTbill")
macro_diff <- na.omit(diff(macro_zoo))
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
adf.test(ari_ma)
set.seed(1121)  # initialize random number generator
ari_ma <- arima.sim(n=10000, model=list(ar=0.8))
adf.test(ari_ma)
set.seed(1121)  # initialize random number generator
rand_walk <- cumsum(rnorm(729))
adf.test(rand_walk)
set.seed(1121)  # initialize random number generator
rand_walk <- cumsum(rnorm(10000))
adf.test(rand_walk)
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
set.seed(1121)  # initialize the random number generator
library(xts)  # load package xts
# create xts time series
in_dex <- Sys.Date() + 0:3
xts_series <- xts(rnorm(length(in_dex)), 
         order.by=in_dex)
names(xts_series) <- "random"
xts_series
tail(xts_series, 3)  # get last few elements
first(xts_series)  # get first element
last(xts_series)  # get last element
class(xts_series)  # class 'xts'
attributes(xts_series)
load(file="C:/Develop/data/zoo_data.RData")
library(xts)  # load package xts
# as.xts() creates xts from zoo
xts_stx <- as.xts(zoo_stx_adj)
dim(xts_stx)
head(xts_stx[, 1:4], 4)
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# plot using plot.xts method
plot(xts_stx[, "AdjClose"], xlab="", ylab="", main="")
title(main="MSFT Prices")  # add title
library(ggplot2)
load(file="C:/Develop/data/etf_data.Rdata")
# create ggplot object
etf_gg <- autoplot(etf_series_ad[, 1],
             main=names(etf_series_ad[, 1])) +
  xlab("") + ylab("") +
  theme(
    legend.position=c(0.1, 0.5),
    plot.title=element_text(vjust=-2.0),
    plot.background=element_blank()
  )  # end theme
# render ggplot object
etf_gg
library(xts)  # load package xts
# subset xts using a date range string
xts_sub <- xts_stx["2014-10-15/2015-01-10", 1:4]
first(xts_sub)
last(xts_sub)
# subset Nov 2014 using a date string
xts_sub <- xts_stx["2014-11", 1:4]
first(xts_sub)
last(xts_sub)
# subset all data after Nov 2014
xts_sub <- xts_stx["2014-11/", 1:4]
first(xts_sub)
last(xts_sub)
# comma after date range not necessary
identical(xts_stx["2014-11", ], xts_stx["2014-11"])
library(xts)  # load package xts
# vector of 1-minute times (ticks)
min_ticks <- seq.POSIXt(
  from=as.POSIXct("2015-04-14", tz="America/New_York"), 
  to=as.POSIXct("2015-04-16"), 
  by="min")
# xts of 1-minute times (ticks)
xts_series <- xts(rnorm(length(min_ticks)), 
               order.by=min_ticks)
# subset recurring time interval using "T notation",
xts_series <- xts_series["T09:30:00/T16:00:00"]
first(xts_series["2015-04-15"])  # first element of day
last(xts_series["2015-04-15"])  # last element of day
library(xts)  # load package xts
str(xts_stx)  # display structure of xts
# subsetting zoo to single column drops dim attribute
dim(zoo_stx_adj)
dim(zoo_stx_adj[, 1])
# zoo with single column are vectors not matrices
c(is.matrix(zoo_stx_adj), is.matrix(zoo_stx_adj[, 1]))
# xts always have a dim attribute
rbind(base=dim(xts_stx), subs=dim(xts_stx[, 1]))
c(is.matrix(xts_stx), is.matrix(xts_stx[, 1]))
library(xts)  # load package xts
# lag of zoo shortens it by one row
rbind(base=dim(zoo_stx_adj), lag=dim(lag(zoo_stx_adj)))
# lag of xts doesn't shorten it
rbind(base=dim(xts_stx), lag=dim(lag(xts_stx)))
# lag of zoo is in opposite direction from xts
head(lag(zoo_stx_adj), 4)
head(lag(xts_stx), 4)
library(xts)  # load package xts
# extract indices of the last observations in each month
end_points <- endpoints(xts_stx, on='months')
head(end_points)
# extract the last observations in each month
head(xts_stx[end_points, ])
library(xts)  # load package xts
# apply "mean" over end_points
period_mean <- period.apply(xts_stx[, "AdjClose"], 
               INDEX=end_points, 
               FUN=mean)
head(period_mean)
period_sum <- period.sum(xts_stx[, "AdjClose"], 
               INDEX=end_points)
head(period_sum)
library(xts)  # load package xts
# apply "mean" over monthly periods
period_mean <- apply.monthly(xts_stx[, "AdjClose"], 
               FUN=mean)
head(period_mean)
library(xts)  # load package xts
# lower the periodicity to months
xts_monthly <- to.period(x=xts_stx[, "AdjClose"], 
                   period="months", name="MSFT")
# convert colnames to standard OHLC format
colnames(xts_monthly)
colnames(xts_monthly) <- sapply(
  strsplit(colnames(xts_monthly), split=".", fixed=TRUE), 
  function(na_me) na_me[-1]
  )  # end sapply
head(xts_monthly, 3)
# lower the periodicity to years
xts_yearly <- to.period(x=xts_monthly, 
                   period="years", name="MSFT")
colnames(xts_yearly) <- sapply(
  strsplit(colnames(xts_yearly), split=".", fixed=TRUE), 
  function(na_me) na_me[-1]
  )  # end sapply
head(xts_yearly)
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
load(file="C:/Develop/data/zoo_data.RData")
library(xts)  # load package xts
# as.xts() creates xts from zoo
xts_stx <- as.xts(zoo_stx_adj)
# subset xts using a date
xts_sub <- xts_stx["2014-11", 1:4]

# plot OHLC using plot.xts method
plot(xts_sub, type="candles", main="")
title(main="MSFT Prices")  # add title
load(file="C:/Develop/data/zoo_data.RData")
ts_stx <- as.ts(zoo_stx)
class(ts_stx)
tail(ts_stx[, 1:4])
library(xts)
xts_stx <- as.xts(zoo_stx)
class(xts_stx)
tail(xts_stx[, 1:4])
