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
set.seed(1121)
ari_ma <- arima.sim(n=729, model=list(ar=0.8))
tseries::adf.test(ari_ma)
set.seed(1121)
ari_ma <- arima.sim(n=10000, model=list(ar=0.8))
tseries::adf.test(ari_ma)
# simulate Brownian motion

rand_walk <- cumsum(rnorm(729))
tseries::adf.test(rand_walk)
set.seed(1121)
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
# define Ornstein-Uhlenbeck function in R
ou_proc <- function(len_gth=1000, eq_price=5.0,
              vol_at=0.01, the_ta=0.01) {
  re_turns <- numeric(len_gth)
  price_s <- numeric(len_gth)
  price_s[1] <- eq_price
  for (i in 2:len_gth) {
    re_turns[i] <- the_ta*(eq_price - price_s[i-1]) + vol_at*rnorm(1)
    price_s[i] <- price_s[i-1] * exp(re_turns[i])
  }  # end for
  price_s
}  # end ou_proc
# simulate Ornstein-Uhlenbeck process
eq_price <- 5.0; vol_at <- 0.01
the_ta <- 0.01; len_gth <- 1000
set.seed(1121)  # reset random numbers
price_s <- ou_proc(len_gth=len_gth, eq_price=eq_price, vol_at=vol_at, the_ta=the_ta)
# define Ornstein-Uhlenbeck function in Rcpp
Rcpp::cppFunction("
NumericVector rcpp_ou_proc(int len_gth, double eq_price, double vol_at, double the_ta, NumericVector r_norm) {
  NumericVector price_s(len_gth);
  NumericVector re_turns(len_gth);
  price_s[0] = eq_price;
  for (int i = 1; i < len_gth; ++i) {
    re_turns[i] = the_ta*(eq_price - price_s[i-1]) + vol_at*r_norm[i-1];
    price_s[i] = price_s[i-1] * exp(re_turns[i]);
  }
  return price_s;
}")  # end cppFunction
set.seed(1121)  # reset random numbers
price_s <- rcpp_ou_proc(len_gth=len_gth, eq_price=eq_price, vol_at=vol_at, the_ta=the_ta, r_norm=rnorm(len_gth))
# compare speed of Rcpp and R
library(microbenchmark)
summary(microbenchmark(
  pure_r=ou_proc(len_gth=len_gth, eq_price=eq_price, vol_at=vol_at, the_ta=the_ta),
  r_cpp=rcpp_ou_proc(len_gth=len_gth, eq_price=eq_price, vol_at=vol_at, the_ta=the_ta, r_norm=rnorm(len_gth)),
  times=10))[, c(1, 4, 5)]
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
chart_Series(x=beta_s[, "VTI"],
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
library(HighFreq)
# Select ETF symbols
sym_bols <- c("IEF", "DBC", "XLU", "XLF", "XLP", "XLI")
# calculate ETF prices and simple returns (not percentage)
price_s <- rutils::env_etf$price_s[, sym_bols]
price_s <- zoo::na.locf(price_s)
price_s <- zoo::na.locf(price_s, fromLast=TRUE)
in_dex <- index(price_s)
re_turns <- rutils::diff_it(price_s)
# de-mean and scale the returns
re_turns <- t(t(re_turns) - colMeans(re_turns))
re_turns <- t(t(re_turns) / sqrt(colSums(re_turns^2)/(NROW(re_turns)-1)))
re_turns <- xts(re_turns, in_dex)
# alternative de-mean and scale the returns
# re_turns <- rutils::diff_it(price_s)
# re_turns <- scale(re_turns, center=TRUE, scale=TRUE)
# re_turns <- xts(re_turns, in_dex)
# or
# re_turns <- lapply(re_turns, function(x) {x - sum(x)/NROW(re_turns)})
# re_turns <- rutils::do_call(cbind, re_turns)
# re_turns <- apply(re_turns, 2, scale)
# covariance matrix and variance vector of returns
cov_mat <- cov(re_turns)
vari_ance <- diag(cov_mat)
cor_mat <- cor(re_turns)
# cov_mat <- (t(re_turns) %*% re_turns) / (NROW(re_turns)-1)
# cor_mat <- cov_mat / sqrt(vari_ance)
# cor_mat <- t(t(cor_mat) / sqrt(vari_ance))
# reorder correlation matrix based on clusters
library(corrplot)
or_der <- corrMatOrder(cor_mat,
        order="hclust",
        hclust.method="complete")
cor_mat <- cor_mat[or_der, or_der]
# plot the correlation matrix
col_ors <- colorRampPalette(c("red", "white", "blue"))
corrplot(cor_mat, title="ETF Correlation Matrix",
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
  -sum(portf_rets^2) +
    1e7*(1 - sum(weight_s^2))^2
}  # end object_ive
# objective for equal weight portfolio
object_ive(weight_s, re_turns)
# compare speed of vector multiplication methods
summary(microbenchmark(
  trans_pose=(t(re_turns[, 1]) %*% re_turns[, 1]),
  s_um=sum(re_turns[, 1]^2),
  times=10))[, c(1, 4, 5)]
# find weights with maximum variance
optim_run <- optim(par=weight_s,
             fn=object_ive,
             re_turns=re_turns,
             method="L-BFGS-B",
             upper=rep(10.0, n_weights),
             lower=rep(-10.0, n_weights))
# optimal weights and maximum variance
weight_s <- optim_run$par
-object_ive(weight_s, re_turns)
# plot first principal component weights
barplot(weight_s, names.arg=names(weight_s),
  xlab="", ylab="",
  main="First Principal Component Weights")
# pc1 weights and returns
weights_1 <- weight_s
pc_1 <- re_turns %*% weights_1
# redefine objective function
object_ive <- function(weight_s, re_turns) {
  portf_rets <- re_turns %*% weight_s
  -sum(portf_rets^2) +
    1e7*(1 - sum(weight_s^2))^2 +
    1e9*(sum(pc_1*portf_rets))^2
}  # end object_ive
# find second principal component weights
optim_run <- optim(par=weight_s,
             fn=object_ive,
             re_turns=re_turns,
             method="L-BFGS-B",
             upper=rep(10.0, n_weights),
             lower=rep(-10.0, n_weights))
# pc2 weights and returns
weights_2 <- optim_run$par
pc_2 <- re_turns %*% weights_2
sum(pc_1*pc_2)
# plot second principal component loadings
barplot(weights_2, names.arg=names(weights_2),
  xlab="", ylab="",
  main="Second Principal Component Loadings")
# calculate eigenvectors and eigenvalues
ei_gen <- eigen(cov_mat)
ei_gen$vectors
weights_1
weights_2
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
  las=3, xlab="", ylab="", main="Principal Component Variances")
# perform principal component analysis PCA
pc_a <- prcomp(re_turns, scale=TRUE)
# plot standard deviations of principal components
barplot(pc_a$sdev,
  names.arg=colnames(pc_a$rotation),
  las=3, xlab="", ylab="",
  main="Scree Plot: Volatilities of Principal Components
  of Stock Returns")
# principal component loadings (weights)
pc_a$rotation
# Plot barplots with PCA weights in multiple panels
par(mfrow=c(n_weights/2, 2))
par(mar=c(2, 2, 2, 1), oma=c(0, 0, 0, 0))
for (or_der in 1:n_weights) {
  barplot(pc_a$rotation[, or_der],
  las=3, xlab="", ylab="", main="")
  title(paste0("PC", or_der), line=-2.0,
  col.main="red")
}  # end for
# principal component time series
pca_rets <- xts(re_turns %*% pc_a$rotation,
          order.by=in_dex)
round(cov(pca_rets), 3)
all.equal(unname(coredata(pca_rets)), unname(pc_a$x))
pca_ts <- xts:::cumsum.xts(pca_rets)
# plot principal component time series in multiple panels
par(mfrow=c(n_weights/2, 2))
par(mar=c(2, 2, 0, 1), oma=c(0, 0, 0, 0))
ra_nge <- range(pca_ts)
for (or_der in 1:n_weights) {
  plot.zoo(pca_ts[, or_der],
     ylim=ra_nge,
     xlab="", ylab="")
  title(paste0("PC", or_der), line=-2.0)
}  # end for
par(mfrow=c(n_weights/2, 2))
par(mar=c(2, 2, 0, 1), oma=c(0, 0, 0, 0))
# invert all the principal component time series
pca_rets <- re_turns %*% pc_a$rotation
sol_ved <- pca_rets %*% solve(pc_a$rotation)
all.equal(re_turns, sol_ved)
# invert first 3 principal component time series
sol_ved <- pca_rets[, 1:3] %*% solve(pc_a$rotation)[1:3, ]
sol_ved <- xts::xts(sol_ved, in_dex)
sol_ved <- xts:::cumsum.xts(sol_ved)
cum_returns <- xts:::cumsum.xts(re_turns)
# plot the solved returns
for (sym_bol in sym_bols) {
  plot.zoo(
    cbind(cum_returns[, sym_bol], sol_ved[, sym_bol]),
    plot.type="single", col=c("black", "blue"), xlab="", ylab="")
  legend(x="topleft", bty="n",
   legend=paste0(sym_bol, c("", " solved")),
   title=NULL, inset=0.0, cex=1.0, lwd=6,
   lty=1, col=c("black", "blue"))
}  # end for
# eigen decomposition of covariance matrix
re_turns <- rutils::diff_it(price_s)
cov_mat <- cov(re_turns)
ei_gen <- eigen(cov_mat)
# perform PCA without scaling
pc_a <- prcomp(re_turns, scale=FALSE)
# compare outputs
all.equal(ei_gen$values, pc_a$sdev^2)
all.equal(abs(unname(ei_gen$vectors)),
    abs(unname(pc_a$rotation)))
# eigen decomposition of correlation matrix
cor_mat <- cor(re_turns)
ei_gen <- eigen(cor_mat)
# perform PCA with scaling
pc_a <- prcomp(re_turns, scale=TRUE)
# compare outputs
all.equal(ei_gen$values, pc_a$sdev^2)
all.equal(abs(unname(ei_gen$vectors)),
    abs(unname(pc_a$rotation)))
