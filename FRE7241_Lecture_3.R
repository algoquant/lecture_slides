library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
options(digits=3)
thm <- knit_theme$get("acid")
knit_theme$set(thm)
set.seed(1121)  # reset random number generator
runif(3)  # three random numbers from the uniform distribution
runif(3)  # produce another three numbers
set.seed(1121)  # reset random number generator
runif(3)  # produce another three numbers

# produce random number from standard normal distribution
rnorm(1)
# produce five random numbers from standard normal distribution
rnorm(5)
# produce five random numbers from the normal distribution
rnorm(n=5, mean=1, sd=2)  # match arguments by name
# calculate cumulative standard normal distribution
c(pnorm(-2), pnorm(2))
# calculate inverse cumulative standard normal distribution
c(qnorm(0.75), qnorm(0.25))
set.seed(1121)  # reset random number generator
# flip unbiased coin once, 20 times
rbinom(n=20, size=1, 0.5)
# number of heads after flipping twice, 20 times
rbinom(n=20, size=2, 0.5)
# number of heads after flipping thrice, 20 times
rbinom(n=20, size=3, 0.5)
# number of heads after flipping biased coin thrice, 20 times
rbinom(n=20, size=3, 0.8)
# number of heads after flipping biased coin thrice, 20 times
rbinom(n=20, size=3, 0.2)
# flip unbiased coin once, 20 times
sample(x=0:1, size=20, replace=TRUE)  # fast
as.numeric(runif(20) < 0.5)  # slower
# permutation of five numbers
sample(x=5)
# permutation of four strings
sample(x=c("apple", "grape", "orange", "peach"))
# sample of size three
sample(x=5, size=3)
# sample with replacement
sample(x=5, replace=TRUE)
sample(  # sample of strings
  x=c("apple", "grape", "orange", "peach"),
  size=12,
  replace=TRUE)
# binomial sample: flip coin once, 20 times
sample(x=0:1, size=20, replace=TRUE)
# flip unbiased coin once, 20 times
as.numeric(runif(20) > 0.5)  # slower
rm(list=ls())
set.seed(1121)  # reset random number generator
# sample from Standard Normal Distribution
sam_ple <- rnorm(1000)

mean(sam_ple)  # sample mean

median(sam_ple)  # sample median

sd(sam_ple)  # sample standard deviation
rm(list=ls())
# DAX returns
ts_rets <- diff(log(EuStockMarkets[, 1]))
# number of observations
len_rets <- length(ts_rets)
# mean of DAX returns
mean_rets <- mean(ts_rets)
# standard deviation of DAX returns
sd_rets <- sd(ts_rets)
# skew of DAX returns
len_rets/((len_rets-1)*(len_rets-2))*
  sum(((ts_rets - mean_rets)/sd_rets)^3)
# kurtosis of DAX returns
len_rets*(len_rets+1)/((len_rets-1)^3)*
  sum(((ts_rets - mean_rets)/sd_rets)^4)
# random normal returns
ts_rets <- rnorm(len_rets, sd=2)
# mean and standard deviation of random normal returns
mean_rets <- mean(ts_rets)
sd_rets <- sd(ts_rets)
# skew of random normal returns
len_rets/((len_rets-1)*(len_rets-2))*
  sum(((ts_rets - mean_rets)/sd_rets)^3)
# kurtosis of random normal returns
len_rets*(len_rets+1)/((len_rets-1)^3)*
  sum(((ts_rets - mean_rets)/sd_rets)^4)
set.seed(1121)  # reset random number generator
# sample from Standard Normal Distribution
sample_length <- 1000
sam_ple <- rnorm(sample_length)
# sample mean
mean(sam_ple)
# sample standard deviation
sd(sam_ple)
#Perform two-tailed test that sample is
#from Standard Normal Distribution (mean=0, SD=1)
# generate vector of samples and store in data frame
test_frame <- data.frame(samples=rnorm(1000))

# significance level, two-tailed test, critical value=2*SD
signif_level <- 2*(1-pnorm(2))
signif_level
# get p-values for all the samples
test_frame$p_values <-
  sapply(test_frame$samples, pnorm)
test_frame$p_values <-
  2*(0.5-abs(test_frame$p_values-0.5))
# compare p_values to significance level
test_frame$result <-
  test_frame$p_values > signif_level
# number of null rejections
sum(!test_frame$result)
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
