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
len_gth <- 1e4
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
# create xts time series
price_s <- xts(price_s, order.by=seq.Date(Sys.Date()-NROW(price_s)+1, Sys.Date(), by=1))
# plot xts time series
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
# create xts time series of percentage of paths below the expected value
per_centage <- xts(per_centage, order.by=seq.Date(Sys.Date()-NROW(per_centage)+1, Sys.Date(), by=1))
# plot xts time series of percentage of paths below the expected value
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
price_s <- xts:::na.locf.xts(price_s)
price_s <- xts:::na.locf.xts(price_s, fromLast=TRUE)
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
# create xts time series of average stock prices
in_dex <- xts(in_dex, order.by=index(price_s))
# plot xts time series of average stock prices
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

# changes in 3 month T-bill rate are autocorrelated
Box.test(macro_diff[, "3mTbill"],
   lag=10, type="Ljung")

# changes in unemployment rate are autocorrelated
Box.test(macro_diff[, "unemprate"],
   lag=10, type="Ljung")
library(zoo)  # load package zoo
dax_acf <- acf(re_turns, plot=FALSE)
summary(dax_acf)  # get the structure of the "acf" object
# print(dax_acf)  # print acf data
dim(dax_acf$acf)
dim(dax_acf$lag)
head(dax_acf$acf)
acf_plus <- function (ts_data, plo_t=TRUE,
                xlab="Lag", ylab="",
                main="", ...) {
  acf_data <- acf(x=ts_data, plot=FALSE, ...)
# remove first element of acf data
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
  invisible(acf_data)  # return invisibly
}  # end acf_plus
par(mar=c(5,0,1,2), oma=c(1,2,1,0), mgp=c(2,1,0), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
library(zoo)  # load package zoo
# improved autocorrelation function
acf_plus(re_turns, lag=10, main="")
title(main="acf of DAX returns", line=-1)
# Ljung-Box test for DAX returns
Box.test(re_turns, lag=10, type="Ljung")
par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # set plot panels
# autocorrelation of squared DAX returns
acf_plus(re_turns^2, lag=10, main="")
title(main="acf of squared DAX returns",
line=-1)
# autocorrelation of squared random returns
acf_plus(rnorm(NROW(re_turns))^2,
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
  lag=10, main="quarterly unemployment rate")
acf_plus(coredata(macro_diff[, "3mTbill"]),
  lag=10, main="3 month T-bill EOQ")
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
# ARIMA processes
set.seed(1121)  # reset random numbers
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
  set.seed(1121)  # reset random numbers
  arima.sim(n=NROW(in_dex), model=list(ar=phi))
})  # end sapply
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
plot(rand_walk, main="Random walks",
     xlab="", ylab="", plot.type="single",
     col=c("black", "red", "blue"))
# add legend
legend(x="topleft",
 legend=colnames(rand_walk),
 col=c("black", "red", "blue"), lty=1)
# define AR(2) coefficients
co_eff <- c(0.9, 0.09)
# calculate modulus of roots of characteristic equation
root_s <- Mod(polyroot(c(1, -co_eff)))
# calculate warmup period
warm_up <- NROW(co_eff) + ceiling(6/log(min(root_s)))
set.seed(1121)
len_gth <- 1e4
in_nov <- rnorm(len_gth + warm_up)
# simulate ARIMA using arima.sim()
ari_ma <- arima.sim(n=len_gth,
  model=list(ar=co_eff),
  start.innov=in_nov[1:warm_up],
  innov=in_nov[(warm_up+1):NROW(in_nov)])
# simulate ARIMA using filter()
arima_filter <- filter(x=in_nov,
  filter=co_eff, method="recursive")
all.equal(arima_filter[-(1:warm_up)],
  as.numeric(ari_ma))
# simulate ARIMA using for() loop
arima_loop <- numeric(NROW(in_nov))
arima_loop[1] <- in_nov[1]
arima_loop[2] <- co_eff[1]*arima_loop[1] + in_nov[2]
for (it in 3:NROW(arima_loop)) {
  arima_loop[it] <- arima_loop[(it-1):(it-2)] %*% co_eff + in_nov[it]
}  # end for
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
# simulate random walks using apply() loops
set.seed(1121)  # initialize random number generator
rand_walks <- matrix(rnorm(1000*100), ncol=1000)
rand_walks <- apply(rand_walks, 2, cumsum)
vari_ance <- apply(rand_walks, 1, var)
# simulate random walks using vectorized functions
set.seed(1121)  # initialize random number generator
rand_walks <- matrixStats::colCumsums(matrix(rnorm(1000*100), ncol=1000))
vari_ance <- matrixStats::rowVars(rand_walks)
par(mar=c(5, 3, 2, 2), oma=c(0, 0, 0, 0))
plot(vari_ance, xlab="time steps", ylab="",
     t="l", col="blue", lwd=2,
     main="Variance of Random Walk")
len_gth <- 1e4
# simulate arima with small AR coefficient
set.seed(1121)
ari_ma <- arima.sim(n=len_gth, model=list(ar=0.01))
tseries::adf.test(ari_ma)
# simulate arima with large AR coefficient
set.seed(1121)
ari_ma <- arima.sim(n=len_gth, model=list(ar=0.99))
tseries::adf.test(ari_ma)
# simulate arima with different AR coefficients
coeff_s <- seq(0.99, 1.0, 0.001) - 0.001
set.seed(1121)
in_nov <- rnorm(len_gth)
adf_test <- sapply(coeff_s, function(co_eff) {
  ari_ma <- filter(x=in_nov, filter=co_eff, method="recursive")
  ad_f <- suppressWarnings(tseries::adf.test(ari_ma))
  c(adf_stat=unname(ad_f$statistic), pval=ad_f$p.value)
})  # end sapply
plot(x=coeff_s, y=adf_test["pval", ], main="ADF Pval versus AR coefficient",
     xlab="AR coefficient", ylab="ADF pval", t="l", col="blue", lwd=2)
plot(x=coeff_s, y=adf_test["adf_stat", ], main="ADF Stat versus AR coefficient",
     xlab="AR coefficient", ylab="ADF stat", t="l", col="blue", lwd=2)
# simulate arima with large AR coefficient
set.seed(1121)
ari_ma <- arima.sim(n=len_gth, model=list(ar=0.99))
tseries::adf.test(ari_ma)
# integrated series has unit root
tseries::adf.test(cumsum(ari_ma))
# simulate arima with negative AR coefficient
set.seed(1121)
ari_ma <- arima.sim(n=len_gth, model=list(ar=-0.99))
tseries::adf.test(ari_ma)
# integrated series has unit root
tseries::adf.test(cumsum(ari_ma))
x11(width=5, height=3.5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0))
# simulate AR(1) process
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
# compute pacf recursively from acf
ac_f <- acf_plus(ari_ma, lag=10, plo_t=FALSE)
ac_f <- drop(ac_f$acf)
pac_f <- numeric(3)
pac_f[1] <- ac_f[1]
pac_f[2] <- ac_f[2] - ac_f[1]^2
pac_f[3] <- ac_f[3] -
  pac_f[2]*ac_f[1] - ac_f[2]*pac_f[1]
# compute pacf recursively in a loop
pac_f <- numeric(NROW(ac_f))
pac_f[1] <- ac_f[1]
for (it in 2:NROW(pac_f)) {
  pac_f[it] <- ac_f[it] -
    pac_f[1:(it-1)] %*% ac_f[(it-1):1]
}  # end for
par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # set plot panels
# Simulate AR(3) time series of returns
ari_ma <- arima.sim(n=729,
  model=list(ar=c(0.1, 0.5, 0.1)))
# ACF of AR(3) process
acf_plus(ari_ma, lag=10, xlab="", ylab="",
   main="ACF of AR(3) process")
# PACF of AR(3) process
pacf(ari_ma, lag=10, xlab="", ylab="",
     main="PACF of AR(3) process")
# calibrate ARIMA model using arima()
arima_fit <- arima(ari_ma,
  order=c(3,0,0), include.mean=FALSE)
arima_fit$coef
# calibrate ARIMA model using auto.arima()
# library(forecast)  # load forecast
forecast::auto.arima(ari_ma, max.p=3, max.q=0)
# calibrate ARIMA model using regression
ari_ma <- as.numeric(ari_ma)
# define design matrix
de_sign <- sapply(1:3, function(lagg) {
  rutils::lag_it(ari_ma, lagg=lagg)
})  # end sapply
# generalized inverse of design matrix
design_inv <- MASS::ginv(de_sign)
# regression coefficients with response equal to ari_ma
co_eff <- drop(design_inv %*% ari_ma)
# compute autocorrelation coefficients
ac_f <- acf_plus(ari_ma, lag=10, plo_t=FALSE)
ac_f <- drop(ac_f$acf)
# define Yule-Walker matrix
acf_1 <- c(1, ac_f[-10])
yule_walker <- sapply(1:9, function(lagg) {
  col_umn <- rutils::lag_it(acf_1, lagg=lagg)
  col_umn[1:lagg] <- acf_1[(lagg+1):2]
  col_umn
})  # end sapply
yule_walker <- cbind(acf_1, yule_walker)
# generalized inverse of Yule-Walker matrix
yule_walker_inv <- MASS::ginv(yule_walker)
# solve Yule-Walker equations
co_eff <- drop(yule_walker_inv %*% ac_f)
# define Ornstein-Uhlenbeck parameters
eq_price <- 1.0; vol_at <- 0.02
the_ta <- 0.01; len_gth <- 1000
drif_t <- the_ta*eq_price
theta_1 <- 1-the_ta
# simulate Ornstein-Uhlenbeck process
in_nov <- vol_at*rnorm(len_gth)
price_s <- numeric(len_gth)
price_s[1] <- in_nov[1]
for (i in 2:len_gth) {
  price_s[i] <- theta_1*price_s[i-1] +
    in_nov[i] + drif_t
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
abline(h=eq_price, col='red', lwd=2)
re_turns <- rutils::diff_it(price_s)
lag_price <- rutils::lag_it(price_s)
for_mula <- re_turns ~ lag_price
l_m <- lm(for_mula)
summary(l_m)
# plot regression
plot(for_mula, main="OU Returns Versus Lagged Prices")
abline(l_m, lwd=2, col="red")
# volatility parameter
c(vol_at, sd(re_turns))
# extract OU parameters from regression
co_eff <- summary(l_m)$coefficients
# theta strength of mean reversion
round(co_eff[2, ], 3)
# equilibrium price
co_eff[1, 1]/co_eff[2, 1]
# parameter and t-values
co_eff <- cbind(c(the_ta*eq_price, the_ta),
  co_eff[, 1:2])
rownames(co_eff) <- c("drift", "theta")
round(co_eff, 3)
# simulate Ornstein-Uhlenbeck process
re_turns <- numeric(len_gth)
price_s <- numeric(len_gth)
price_s[1] <- eq_price
set.seed(1121)  # reset random numbers
for (i in 2:len_gth) {
  re_turns[i] <- the_ta*(eq_price - price_s[i-1]) +
    vol_at*rnorm(1)
  price_s[i] <- price_s[i-1] * exp(re_turns[i])
}  # end for
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
price_s <- Cl(rutils::etf_env$VTI)
end_points <- seq_along(price_s)  # define end points
n_row <- NROW(end_points)
look_back <- 22  # number of data points per look-back interval
# start_points are multi-period lag of end_points
start_points <- c(rep_len(1, look_back-1),
    end_points[1:(n_row-look_back+1)])
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
  bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")
# library(HighFreq)  # load package HighFreq
# define functional for rolling aggregations
roll_agg <- function(x_ts, look_back, FUN, ...) {
# define end points at every period
  end_points <- seq_along(x_ts)
  n_row <- NROW(end_points)
# define starting points as lag of end_points
  start_points <- c(rep_len(1, look_back-1),
    end_points[1:(n_row-look_back+1)])
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
# calculate the rolling maximum and minimum over a vector of data
roll_maxminr <- function(vec_tor, look_back) {
  n_row <- NROW(vec_tor)
  max_min <- matrix(numeric(2*n_row), nc=2)
  # loop over periods
  for (it in 1:n_row) {
    sub_vec <- vec_tor[max(1, it-look_back+1):it]
    max_min[it, 1] <- max(sub_vec)
    max_min[it, 2] <- min(sub_vec)
  }  # end for
  return(max_min)
}  # end roll_maxminr
max_minr <- roll_maxminr(price_s, look_back)
max_minr <- xts::xts(max_minr, index(price_s))
library(TTR)  # load package TTR
max_min <- cbind(TTR::runMax(x=price_s, n=look_back),
           TTR::runMin(x=price_s, n=look_back))
all.equal(max_min[-(1:look_back), ], max_minr[-(1:look_back), ], check.attributes=FALSE)
# benchmark the speed of TTR::runMax
library(microbenchmark)
summary(microbenchmark(
  pure_r=roll_maxminr(price_s, look_back),
  ttr=TTR::runMax(price_s, n=look_back),
  times=10))[, c(1, 4, 5)]
# benchmark the speed of TTR::runSum
summary(microbenchmark(
  vector_r=cumsum(coredata(price_s)),
  rutils=rutils::roll_sum(price_s, look_back=look_back),
  ttr=TTR::runSum(price_s, n=look_back),
  times=10))[, c(1, 4, 5)]
# Compile Rcpp functions
Rcpp::sourceCpp(file="C:/Develop/R/Rcpp/roll_maxmin.cpp")
max_minarma <- roll_maxmin(price_s, look_back)
max_minarma <- xts::xts(max_minr, index(price_s))
max_min <- cbind(TTR::runMax(x=price_s, n=look_back),
           TTR::runMin(x=price_s, n=look_back))
all.equal(max_min[-(1:look_back), ], max_minarma[-(1:look_back), ], check.attributes=FALSE)
# benchmark the speed of TTR::runMax
library(microbenchmark)
summary(microbenchmark(
  arma=roll_maxmin(price_s, look_back),
  ttr=TTR::runMax(price_s, n=look_back),
  times=10))[, c(1, 4, 5)]
# dygraphs plot with max_min lines
da_ta <- cbind(price_s, max_minarma)
colnames(da_ta)[2:3] <- c("max", "min")
col_ors <- c("blue", "red", "green")
dygraphs::dygraph(da_ta, main=paste(colnames(price_s), "max and min lines")) %>%
  dyOptions(colors=col_ors)
# standard plot with max_min lines
plot_theme <- chart_theme()
plot_theme$col$line.col <- col_ors
quantmod::chart_Series(da_ta["2008/2009"], theme=plot_theme,
  name=paste(colnames(price_s), "max and min lines"))
legend(x="topright", title=NULL, legend=colnames(da_ta),
 inset=0.1, cex=0.9, bg="white", bty="n",
 lwd=6, lty=1, col=col_ors)
library(RcppRoll)  # load package RcppRoll
# calculate rolling sum using RcppRoll
sum_roll_rcpp <- RcppRoll::roll_sum(price_s, align="right", n=look_back)
# calculate rolling sum using rutils
sum_roll <- rutils::roll_sum(price_s, look_back=look_back)
all.equal(sum_roll_rcpp, coredata(sum_roll[-(1:(look_back-1))]), check.attributes=FALSE)
# benchmark the speed of RcppRoll::roll_sum
library(microbenchmark)
summary(microbenchmark(
  cum_sum=cumsum(coredata(price_s)),
  RcppRoll=RcppRoll::roll_sum(price_s, n=look_back),
  rutils=rutils::roll_sum(price_s, look_back=look_back),
  times=10))[, c(1, 4, 5)]
# calculate EWMA sum using RcppRoll
weight_s <- exp(0.1*1:look_back)
prices_mean <- RcppRoll::roll_mean(price_s,
align="right", n=look_back, weights=weight_s)
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
 bg="white", lty=1, lwd=6,
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
price_s <- Cl(rutils::etf_env$VTI)
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
n_row <- NROW(end_points)
# start_points are single-period lag of end_points
start_points <- c(1, end_points[1:(n_row-1)])
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
  bg="white", lty=1, lwd=6,
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
  bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")
# library(HighFreq)  # load package HighFreq
# define functional for rolling aggregations over end_points
roll_agg <- function(x_ts, end_points, FUN, ...) {
  n_row <- NROW(end_points)
# start_points are single-period lag of end_points
  start_points <- c(1, end_points[1:(n_row-1)])
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
n_row <- NROW(end_points)
num_points <- 4  # number of end points in look-back interval
# start_points are multi-period lag of end_points
start_points <- c(rep_len(1, num_points-1),
  end_points[1:(n_row-num_points+1)])
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
  bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")
library(HighFreq)  # load package HighFreq
agg_regations <- cbind(price_s, agg_regations)
tail(agg_regations, 22)
agg_regations <- na.omit(xts:::na.locf.xts(agg_regations))
# plot aggregations with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("black", "red", "green")
chart_Series(agg_regations, theme=plot_theme,
       name="price aggregations")
legend("top", legend=colnames(agg_regations),
  bg="white", lty=1, lwd=6,
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
