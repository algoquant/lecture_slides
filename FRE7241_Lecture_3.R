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
n_stocks <- rowSums(val_id)
n_stocks[1] <- NCOL(price_s)
in_dex <- rowSums(price_s * val_id) / n_stocks
# calculate percentage of stock prices below the average price
per_centage <- rowSums((price_s < in_dex) & val_id) / n_stocks
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
acf_plus <- function(ts_data, plo_t=TRUE,
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
plot.zoo(rand_walk, main="Random walks",
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
all.equal(arima_fit$coef, co_eff, check.attributes=FALSE)
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
len_gth <- 1e2
# Simulate AR(3) time series using filter()
co_eff <- c(0.1, 0.39, 0.5)
set.seed(1121)
in_nov <- rnorm(len_gth)
se_ries <- filter(x=in_nov,
  filter=co_eff, method="recursive")
# Forecast using AR(3) ARIMA
fore_casts <- numeric(NROW(se_ries))
fore_casts[2] <- co_eff[1]*se_ries[1]
fore_casts[3] <- co_eff[1]*se_ries[2] +
  co_eff[2]*se_ries[1]
for (it in 4:NROW(fore_casts)) {
  fore_casts[it] <-
    se_ries[(it-1):(it-3)] %*% co_eff
}  # end for
# Forecast using filter()
forecasts_filter <- filter(x=se_ries, sides=1,
  filter=co_eff, method="convolution")
class(forecasts_filter)
all.equal(fore_casts[-(1:4)],
  forecasts_filter[-c(1:3, NROW(forecasts_filter))],
  check.attributes=FALSE)
# Compare residuals with innovations
residual_s <- (se_ries-fore_casts)
tail(cbind(in_nov, residual_s))
# Plot with legend
plot.default(se_ries,
  main="Forecasting Using AR(3) Model",
  xlab="", ylab="", type="l")
lines(fore_casts, col="orange", lwd=3)
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
})  # end sapply
de_sign <- cbind(se_ries, de_sign)
look_back <- 100  # length of look-back interval
end_points <- seq_along(se_ries)
n_rows <- NROW(end_points)
start_points <- c(rep_len(1, look_back-1),
    end_points[1:(n_rows-look_back+1)])
# Perform rolling forecasting
fore_casts <- sapply(end_points[-(1:2)], function(it) {
  de_sign <- de_sign[start_points[it]:end_points[it], ]
  # calculate AR(3) coefficients
  design_inv <- MASS::ginv(de_sign[, -1])
  co_eff <- drop(design_inv %*% de_sign[, 1])
  de_sign[(NROW(de_sign)-2):NROW(de_sign), 1] %*% co_eff
})  # end sapply
fore_casts <- c(rep(fore_casts[1], 2), fore_casts)
# Lag the forecasts to push them out-of-sample
fore_casts <- rutils::lag_it(fore_casts)
# Mean squared error
ms_e <- mean((fore_casts-se_ries)^2)
# Plot with legend
plot.default(se_ries,
  main="Rolling Forecasting Using AR(3) Model",
  xlab="", ylab="", type="l")
lines(fore_casts, col="orange", lwd=1)
legend(x="topright", legend=c("series","forecasts"),
 col=c("black", "orange"), lty=1, lwd=6,
 cex=0.9, bg="white", bty="n")
library(rutils)  # load package rutils
# Calculate ETF returns
re_turns <-
  rutils::etf_env$re_turns[, c("IEF", "VTI")]
re_turns <- na.omit(re_turns)
re_turns <- cbind(re_turns,
  0.6*re_turns[, "IEF"]+0.4*re_turns[, "VTI"])
colnames(re_turns)[3] <- "combined"
# Calculate correlations
cor(re_turns)
# Calculate Sharpe ratios
sqrt(252)*sapply(re_turns, function(x) mean(x)/sd(x))
# Calculate prices from returns
price_s <- lapply(re_turns,
  function(x) exp(cumsum(x)))
price_s <- rutils::do_call(cbind, price_s)
# Plot prices
dygraphs::dygraph(price_s, main="Stock and Bond Portfolio") %>%
  dyOptions(colors=c("green","blue","green")) %>%
  dySeries("combined", color="red", strokeWidth=2) %>%
  dyLegend(show="always")
# calculate open, close, and lagged prices
oh_lc <- rutils::etf_env$VTI
op_en <- quantmod::Op(oh_lc)
cl_ose <- quantmod::Cl(oh_lc)
star_t <- as.numeric(cl_ose[1, ])
prices_lag <- rutils::lag_it(cl_ose)
# define aggregation interval and calculate VWAP
look_back <- 150
VTI_vwap <- HighFreq::roll_vwap(oh_lc,
        look_back=look_back)
# calculate VWAP indicator
in_dic <- sign(cl_ose - VTI_vwap)
# determine dates right after VWAP has crossed prices
trade_dates <- (rutils::diff_it(in_dic) != 0)
trade_dates <- which(trade_dates) + 1
# plot prices and VWAP
chart_Series(x=cl_ose,
  name="VTI prices", col="orange")
add_TA(VTI_vwap, on=1, lwd=2, col="blue")
legend("top", legend=c("VTI", "VWAP"),
  bg="white", lty=1, lwd=6,
  col=c("orange", "blue"), bty="n")
# Calculate positions, either: -1, 0, or 1
position_s <- rep(NA_integer_, NROW(oh_lc))
position_s[1] <- 0
position_s[trade_dates] <- in_dic[trade_dates]
position_s <- na.locf(position_s)
position_s <- xts(position_s, order.by=index(oh_lc))
pos_lagged <- rutils::lag_it(position_s)
# Calculate daily profits and losses
pnl_s <- pos_lagged*(cl_ose - prices_lag)
pnl_s[trade_dates] <- pos_lagged[trade_dates] *
  (op_en[trade_dates] - prices_lag[trade_dates]) +
  position_s[trade_dates] *
  (cl_ose[trade_dates] - op_en[trade_dates])
# Calculate percentage returns
pnl_s <- pnl_s/cl_ose
# Calculate annualized Sharpe ratio of strategy returns
sqrt(252)*sum(pnl_s)/sd(pnl_s)/NROW(pnl_s)
# plot prices and VWAP
pnl_s <- xts(as.numeric(cl_ose[1])*exp(cumsum(pnl_s)), order.by=index(oh_lc))
chart_Series(x=cl_ose, name="VTI prices", col="orange")
add_TA(pnl_s, on=1, lwd=2, col="blue")
add_TA(position_s > 0, on=-1,
 col="lightgreen", border="lightgreen")
add_TA(position_s < 0, on=-1,
 col="lightgrey", border="lightgrey")
legend("top", legend=c("VTI", "VWAP strategy"),
 inset=0.1, bg="white", lty=1, lwd=6,
 col=c("orange", "blue"), bty="n")
# Define length for weights and decay parameter
wid_th <- 352
lamb_da <- 0.01
# Calculate EWMA prices
weight_s <- exp(-lamb_da*1:wid_th)
weight_s <- weight_s/sum(weight_s)
ew_ma <- stats::filter(cl_ose, filter=weight_s, sides=1)
ew_ma[1:(wid_th-1)] <- ew_ma[wid_th]
ew_ma <- xts(cbind(cl_ose, ew_ma), order.by=index(oh_lc))
colnames(ew_ma) <- c("VTI", "VTI EWMA")
# plot EWMA prices with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue")
chart_Series(ew_ma["2007/2010"], theme=plot_theme,
       name="EWMA prices")
legend("bottomleft", legend=colnames(ew_ma),
 inset=0.1, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")
# determine dates right after EWMA has crossed prices
in_dic <- sign(cl_ose - ew_ma[, 2])
trade_dates <- (rutils::diff_it(in_dic) != 0)
trade_dates <- which(trade_dates) + 1
# calculate positions, either: -1, 0, or 1
position_s <- rep(NA_integer_, NROW(cl_ose))
position_s[1] <- 0
position_s[trade_dates] <-
  rutils::lag_it(in_dic)[trade_dates]
position_s <- na.locf(position_s)
position_s <- xts(position_s, order.by=index(oh_lc))
# plot EWMA prices with position shading
chart_Series(ew_ma["2007/2010"], theme=plot_theme,
       name="EWMA prices")
add_TA(position_s > 0, on=-1,
 col="lightgreen", border="lightgreen")
add_TA(position_s < 0, on=-1,
 col="lightgrey", border="lightgrey")
legend("bottomleft", legend=colnames(ew_ma),
 inset=0.1, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")
# bid_offer is equal to 10 bps for liquid ETFs
bid_offer <- 0.001
# calculate open and lagged prices
op_en <- Op(oh_lc)
prices_lag <- rutils::lag_it(cl_ose)
pos_lagged <- rutils::lag_it(position_s)
# calculate the transaction cost for one share
cost_s <- 0.0*position_s
cost_s[trade_dates] <-
  0.5*bid_offer*abs(pos_lagged[trade_dates] -
  position_s[trade_dates])*op_en[trade_dates]
# Calculate daily profits and losses
re_turns <- pos_lagged*(cl_ose - prices_lag)
re_turns[trade_dates] <-
  pos_lagged[trade_dates] *
  (op_en[trade_dates] - prices_lag[trade_dates]) +
  position_s[trade_dates] *
  (cl_ose[trade_dates] - op_en[trade_dates]) -
  cost_s
# Calculate percentage returns
re_turns <- re_turns/cl_ose
# Calculate annualized Sharpe ratio of strategy returns
sqrt(252)*sum(re_turns)/sd(re_turns)/NROW(re_turns)
pnl_s <- as.numeric(cl_ose[1])*exp(cumsum(re_turns))
pnl_s <- cbind(cl_ose, pnl_s)
colnames(pnl_s) <- c("VTI", "EWMA PnL")
# plot EWMA PnL with position shading
chart_Series(pnl_s, theme=plot_theme,
       name="Performance of EWMA Strategy")
add_TA(position_s > 0, on=-1,
 col="lightgreen", border="lightgreen")
add_TA(position_s < 0, on=-1,
 col="lightgrey", border="lightgrey")
legend("top", legend=colnames(pnl_s),
 inset=0.05, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")
simu_ewma <- function(oh_lc, lamb_da=0.01, wid_th=251, bid_offer=0.001, tre_nd=1) {
  # calculate EWMA prices
  weight_s <- exp(-lamb_da*1:wid_th)
  weight_s <- weight_s/sum(weight_s)
  cl_ose <- quantmod::Cl(oh_lc)
  ew_ma <- stats::filter(as.numeric(cl_ose), filter=weight_s, sides=1)
  ew_ma[1:(wid_th-1)] <- ew_ma[wid_th]
  # determine dates right after EWMA has crossed prices
  in_dic <- tre_nd*xts::xts(sign(as.numeric(cl_ose) - ew_ma), order.by=index(oh_lc))
  trade_dates <- (rutils::diff_it(in_dic) != 0)
  trade_dates <- which(trade_dates) + 1
  trade_dates <- trade_dates[trade_dates<NROW(oh_lc)]
  # calculate positions, either: -1, 0, or 1
  position_s <- rep(NA_integer_, NROW(cl_ose))
  position_s[1] <- 0
  position_s[trade_dates] <- rutils::lag_it(in_dic)[trade_dates]
  position_s <- xts::xts(na.locf(position_s), order.by=index(oh_lc))
  op_en <- quantmod::Op(oh_lc)
  prices_lag <- rutils::lag_it(cl_ose)
  pos_lagged <- rutils::lag_it(position_s)
  # calculate transaction costs
  cost_s <- 0.0*position_s
  cost_s[trade_dates] <- 0.5*bid_offer*abs(pos_lagged[trade_dates] - position_s[trade_dates])*op_en[trade_dates]
  # calculate daily profits and losses
  re_turns <- pos_lagged*(cl_ose - prices_lag)
  re_turns[trade_dates] <- pos_lagged[trade_dates] * (op_en[trade_dates] - prices_lag[trade_dates]) + position_s[trade_dates] * (cl_ose[trade_dates] - op_en[trade_dates]) - cost_s
  # Calculate percentage returns
  re_turns <- re_turns/cl_ose
  out_put <- cbind(position_s, re_turns)
  colnames(out_put) <- c("positions", "returns")
  out_put
}  # end simu_ewma
source("C:/Develop/R/lecture_slides/scripts/ewma_model.R")
lamb_das <- seq(0.0001, 0.05, 0.005)
# perform lapply() loop over lamb_das
pnl_s <- lapply(lamb_das, function(lamb_da) {
  # simulate EWMA strategy and calculate re_turns
  star_t*exp(cumsum(simu_ewma(oh_lc=oh_lc,
    lamb_da=lamb_da, wid_th=wid_th)[, "returns"]))
})  # end lapply
pnl_s <- rutils::do_call(cbind, pnl_s)
colnames(pnl_s) <- paste0("lambda=", lamb_das)
# plot EWMA strategies with custom line colors
column_s <- seq(1, NCOL(pnl_s), by=3)
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NROW(column_s))
chart_Series(pnl_s[, column_s],
  theme=plot_theme, name="Cumulative Returns of EWMA Strategies")
legend("topleft", legend=colnames(pnl_s[, column_s]),
  inset=0.1, bg="white", cex=0.8, lwd=rep(6, NCOL(pnl_s)),
  col=plot_theme$col$line.col, bty="n")
# initialize compute cluster under Windows
library(parallel)
clus_ter <- makeCluster(detectCores()-1)
clusterExport(clus_ter,
  varlist=c("oh_lc", "wid_th", "simu_ewma"))
# perform parallel loop over lamb_das under Windows
pnl_s <- parLapply(clus_ter, lamb_das, function(lamb_da) {
  library(quantmod)
  # simulate EWMA strategy and calculate re_turns
  star_t*exp(cumsum(simu_ewma(
    oh_lc=oh_lc, lamb_da=lamb_da, wid_th=wid_th)[, "returns"]))
})  # end parLapply
# perform parallel loop over lamb_das under Mac-OSX or Linux
re_turns <- mclapply(lamb_das, function(lamb_da) {
  library(quantmod)
  # simulate EWMA strategy and calculate re_turns
  star_t*exp(cumsum(simu_ewma(
    oh_lc=oh_lc, lamb_da=lamb_da, wid_th=wid_th)[, "returns"]))
})  # end mclapply
stopCluster(clus_ter)  # stop R processes over cluster under Windows
pnl_s <- rutils::do_call(cbind, pnl_s)
colnames(pnl_s) <- paste0("lambda=", lamb_das)
sharpe_ratios <- sqrt(252)*sapply(pnl_s, function(x_ts) {
  # calculate annualized Sharpe ratio of strategy returns
  x_ts <- rutils::diff_it(log(x_ts))
  sum(x_ts)/sd(x_ts)
})/NROW(pnl_s)  # end sapply
plot(x=lamb_das, y=sharpe_ratios, t="l",
     main="Performance of EWMA trend-following strategies
     as function of the decay parameter lambda")
trend_returns <- rutils::diff_it(log(pnl_s))
trend_sharpe <- sharpe_ratios
# Simulate best performing strategy
ewma_trend <- simu_ewma(oh_lc=oh_lc,
  lamb_da=lamb_das[which.max(sharpe_ratios)],
  wid_th=wid_th)
position_s <- ewma_trend[, "positions"]
pnl_s <- star_t*exp(cumsum(ewma_trend[, "returns"]))
pnl_s <- cbind(cl_ose, pnl_s)
colnames(pnl_s) <- c("VTI", "EWMA PnL")
# plot EWMA PnL with position shading
plot_theme$col$line.col <- c("orange", "blue")
chart_Series(pnl_s, theme=plot_theme,
       name="Performance of Trend-following EWMA Strategy")
add_TA(position_s > 0, on=-1,
 col="lightgreen", border="lightgreen")
add_TA(position_s < 0, on=-1,
 col="lightgrey", border="lightgrey")
legend("top", legend=colnames(pnl_s),
  inset=0.05, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")
source("C:/Develop/R/lecture_slides/scripts/ewma_model.R")
lamb_das <- seq(0.05, 1.0, 0.05)
# perform lapply() loop over lamb_das
pnl_s <- lapply(lamb_das, function(lamb_da) {
  # backtest EWMA strategy and calculate re_turns
  star_t*exp(cumsum(simu_ewma(
    oh_lc=oh_lc, lamb_da=lamb_da, wid_th=wid_th, tre_nd=(-1))[, "returns"]))
})  # end lapply
pnl_s <- rutils::do_call(cbind, pnl_s)
colnames(pnl_s) <- paste0("lambda=", lamb_das)
# plot EWMA strategies with custom line colors
column_s <- seq(1, NCOL(pnl_s), by=4)
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NROW(column_s))
chart_Series(pnl_s[, column_s],
  theme=plot_theme, name="Cumulative Returns of Mean-reverting EWMA Strategies")
legend("topleft", legend=colnames(pnl_s[, column_s]),
  inset=0.1, bg="white", cex=0.8, lwd=rep(6, NCOL(pnl_s)),
  col=plot_theme$col$line.col, bty="n")
sharpe_ratios <- sqrt(252)*sapply(pnl_s, function(x_ts) {
  # calculate annualized Sharpe ratio of strategy returns
  x_ts <- rutils::diff_it(log(x_ts))
  sum(x_ts)/sd(x_ts)
})/NROW(pnl_s)  # end sapply
plot(x=lamb_das, y=sharpe_ratios, t="l",
     main="Performance of EWMA mean-reverting strategies
     as function of the decay parameter lambda")
revert_returns <- rutils::diff_it(log(pnl_s))
revert_sharpe <- sharpe_ratios
# backtest best performing strategy
ewma_revert <- simu_ewma(oh_lc=oh_lc,
  lamb_da=lamb_das[which.max(sharpe_ratios)],
  wid_th=wid_th, tre_nd=(-1))
position_s <- ewma_revert[, "positions"]
pnl_s <- star_t*exp(cumsum(ewma_revert[, "returns"]))
pnl_s <- cbind(cl_ose, pnl_s)
colnames(pnl_s) <- c("VTI", "EWMA PnL")
# plot EWMA PnL with position shading
plot_theme$col$line.col <- c("orange", "blue")
chart_Series(pnl_s, theme=plot_theme,
       name="Performance of Mean-reverting EWMA Strategy")
add_TA(position_s > 0, on=-1,
 col="lightgreen", border="lightgreen")
add_TA(position_s < 0, on=-1,
 col="lightgrey", border="lightgrey")
legend("top", legend=colnames(pnl_s),
  inset=0.05, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")
# calculate correlation between trend-following and mean-reverting strategies
trend_ing <- ewma_trend[, "returns"]
colnames(trend_ing) <- "trend"
revert_ing <- ewma_revert[, "returns"]
colnames(revert_ing) <- "revert"
close_rets <- rutils::diff_it(log(cl_ose))
cor(cbind(trend_ing, revert_ing, close_rets))
# Calculate combined strategy
com_bined <- trend_ing + revert_ing
colnames(com_bined) <- "combined"
# calculate annualized Sharpe ratio of strategy returns
re_turns <- cbind(close_rets, trend_ing, revert_ing, com_bined)
sqrt(252)*sapply(re_turns, function(x_ts)
  sum(x_ts)/sd(x_ts))/NROW(com_bined)
pnl_s <- lapply(re_turns, function(x_ts) star_t*exp(cumsum(x_ts)))
pnl_s <- rutils::do_call(cbind, pnl_s)
colnames(pnl_s) <- c("VTI", "trending", "reverting", "EWMA combined PnL")
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue", "green", "magenta2")
chart_Series(pnl_s, theme=plot_theme,
       name="Performance of Combined EWMA Strategies")
legend("topleft", legend=colnames(pnl_s),
 inset=0.05, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")
sharpe_ratios <- c(trend_sharpe, revert_sharpe)
weight_s <- sharpe_ratios
weight_s[weight_s<0] <- 0
weight_s <- weight_s/sum(weight_s)
re_turns <- cbind(trend_returns, revert_returns)
avg_returns <- re_turns %*% weight_s
avg_returns <- xts(avg_returns, order.by=index(re_turns))
pnl_s <- star_t*exp(cumsum(avg_returns))
pnl_s <- cbind(cl_ose, pnl_s)
colnames(pnl_s) <- c("VTI", "EWMA PnL")
# plot EWMA PnL without position shading
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue")
chart_Series(pnl_s, theme=plot_theme,
  name="Performance of Ensemble EWMA Strategy")
legend("top", legend=colnames(pnl_s),
  inset=0.05, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")
# Define end of month end_points
end_points <- rutils::calc_endpoints(re_turns,
          inter_val="months")
n_rows <- NROW(end_points)
# Start_points equal end_points lagged by 12-month look-back interval
look_back <- 12
start_points <- c(rep_len(1, look_back-1),
  end_points[1:(n_rows-look_back+1)])
# Calculate past performance over end_points
perform_ance <-
  function(re_turns) sum(re_turns)/sd(re_turns)
past_perf <- sapply(1:(n_rows-1), function(it_er) {
  sapply(re_turns[start_points[it_er]:end_points[it_er]], perform_ance)
})  # end sapply
past_perf <- t(past_perf)
fut_rets <- sapply(1:(n_rows-1), function(it_er) {
  sapply(re_turns[(end_points[it_er]+1):end_points[it_er+1]], sum)
})  # end sapply
fut_rets <- t(fut_rets)
# calculate weight_s proportional to past_perf
weight_s <- past_perf
weight_s[weight_s<0] <- 0
# scale weight_s so their sum is equal to 1
weight_s <- weight_s/rowSums(weight_s)
# set NA values to zero
weight_s[is.na(weight_s)] <- 0
sum(is.na(weight_s))
in_dex <- index(re_turns[end_points[-n_rows]])
trend_weights <- rowMeans(weight_s[, 1:NCOL(trend_returns)])
revert_weights <- rowMeans(weight_s[, -(1:NCOL(trend_returns))])
diff_weights <- xts(trend_weights-revert_weights, order.by=in_dex)
# Find best and worst EWMA Strategies in each period
bes_t <- apply(weight_s, 1, which.max)
wors_t <- apply(weight_s, 1, which.min)
# plot the mean weights of EWMA Strategies
zoo::plot.zoo(cbind(diff_weights,
  cl_ose[end_points[-n_rows]]),
  oma = c(3, 0, 3, 0), mar = c(0, 4, 0, 1),
  xlab=NULL, ylab=c("diff weights", "VTI"),
  main="Trend minus Revert Weights of EWMA strategies")
best_worst <- xts(cbind(bes_t, wors_t), order.by=in_dex)
zoo::plot.zoo(best_worst,
  oma = c(3, 0, 3, 0), mar = c(0, 4, 0, 1),
  xlab=NULL, ylab=c("best EWMA", "worst EWMA"),
  main="Best and Worst EWMA strategies")
# Calculate backtest returns
pnl_s <- rowSums(weight_s*fut_rets)
pnl_s <- xts(pnl_s, order.by=in_dex)
colnames(pnl_s) <- "ewma momentum"
close_rets <- rutils::diff_it(cl_ose[in_dex])
cor(cbind(pnl_s, close_rets))
pnl_s <- star_t*exp(cumsum(pnl_s))
# plot the backtest
chart_Series(x=cl_ose[end_points[-n_rows]],
  name="backtest of EWMA strategies", col="orange")
add_TA(pnl_s, on=1, lwd=2, col="blue")
legend("top", legend=c("VTI", "EWMA"),
 inset=0.1, bg="white", lty=1, lwd=6,
 col=c("orange", "blue"), bty="n")
# shad_e <- xts(index(pnl_s) < as.Date("2008-01-31"), order.by=index(pnl_s))
# add_TA(shad_e, on=-1, col="lightgrey", border="lightgrey")
# text(x=7, y=0, labels="warmup period")
# Calculate ETF prices and simple returns
sym_bols <- c("VTI", "IEF", "DBC")
price_s <- rutils::etf_env$price_s[, sym_bols]
price_s <- na.omit(zoo::na.locf(price_s))
re_turns <- rutils::diff_it(price_s)
# Define look-back and look-forward intervals
end_points <- rutils::calc_endpoints(re_turns,
  inter_val="months")
n_cols <- NCOL(re_turns)
n_rows <- NROW(end_points)
look_back <- 12
start_points <- c(rep_len(1, look_back-1),
  end_points[1:(n_rows-look_back+1)])
# Calculate past performance over end_points
perform_ance <-
  function(re_turns) sum(re_turns)/sd(re_turns)
agg_s <- sapply(1:(n_rows-1), function(it_er) {
  c(past_perf=sapply(re_turns[start_points[it_er]:end_points[it_er]], perform_ance),
    fut_rets=sapply(re_turns[(end_points[it_er]+1):end_points[it_er+1]], sum))
})  # end sapply
agg_s <- t(agg_s)
# Select look-back and look-forward aggregations
past_perf <- agg_s[, 1:n_cols]
fut_rets <- agg_s[, n_cols+1:n_cols]
# Calculate portfolio weights equal to number of shares
end_prices <- price_s[end_points[-n_rows]]
weight_s <-
  past_perf/rowSums(abs(past_perf))/end_prices
weight_s[is.na(weight_s)] <- 0
colnames(weight_s) <- colnames(re_turns)
# Calculate profits and losses
pnl_s <- rowSums(weight_s*fut_rets)
pnl_s <- xts(pnl_s, index(end_prices))
colnames(pnl_s) <- "pnls"
# Calculate transaction costs
bid_offer <- 0.001
cost_s <-
  0.5*bid_offer*end_prices*abs(rutils::diff_it(weight_s))
cost_s <- rowSums(cost_s)
pnl_s <- (pnl_s - cost_s)
pnl_s <- cumsum(pnl_s)
# plot momentum strategy with VTI
cl_ose <- price_s[index(end_prices), "VTI"]
zoo::plot.zoo(cbind(cl_ose, pnl_s, weight_s),
  oma = c(3, 1, 3, 0), mar = c(0, 4, 0, 1), nc=1,
  xlab=NULL, main="ETF Momentum Strategy")
# define backtest functional
backtest_ep <- function(re_turns, price_s, perform_ance=sum,
    look_back=12, re_balance="months", bid_offer=0.001,
    end_points=rutils::calc_endpoints(re_turns, inter_val=re_balance),
    with_weights=FALSE, ...) {
  stopifnot("package:quantmod" %in% search() || require("quantmod", quietly=TRUE))
  # Define look-back and look-forward intervals
  n_cols <- NCOL(re_turns)
  n_rows <- NROW(end_points)
  start_points <- c(rep_len(1, look_back-1), end_points[1:(n_rows-look_back+1)])
  # Calculate past performance over end_points
  agg_s <- sapply(1:(n_rows-1), function(it_er) {
    c(past_perf=sapply(re_turns[start_points[it_er]:end_points[it_er]], perform_ance, ...),  # end sapply
    fut_rets=sapply(re_turns[(end_points[it_er]+1):end_points[it_er+1]], sum))  # end sapply
  })  # end sapply
  agg_s <- t(agg_s)
  # Select look-back and look-forward aggregations
  past_perf <- agg_s[, 1:n_cols]
  fut_rets <- agg_s[, n_cols+1:n_cols]
  # Calculate portfolio weights equal to number of shares
  end_prices <- price_s[end_points[-n_rows]]
  weight_s <- past_perf/rowSums(abs(past_perf))/end_prices
  weight_s[is.na(weight_s)] <- 0
  colnames(weight_s) <- colnames(re_turns)
  # Calculate profits and losses
  pnl_s <- rowSums(weight_s*fut_rets)
  pnl_s <- xts(pnl_s, index(end_prices))
  colnames(pnl_s) <- "pnls"
  # Calculate transaction costs
  cost_s <- 0.5*bid_offer*end_prices*abs(rutils::diff_it(weight_s))
  cost_s <- rowSums(cost_s)
  pnl_s <- (pnl_s - cost_s)
  pnl_s <- cumsum(pnl_s)
  if (with_weights)
    cbind(pnl_s, weight_s)
  else
    pnl_s
}  # end backtest_ep
source("C:/Develop/R/lecture_slides/scripts/back_test.R")
look_backs <- seq(5, 60, by=5)
perform_ance <- function(re_turns) sum(re_turns)/sd(re_turns)
pro_files <- sapply(look_backs, function(x) {
  last(backtest_ep(re_turns=re_turns, price_s=price_s,
    re_balance="weeks", look_back=x, perform_ance=perform_ance))
})  # end sapply
plot(x=look_backs, y=pro_files, t="l",
  main="Strategy PnL as function of look_back",
  xlab="look_back (weeks)", ylab="pnl")
look_back <- look_backs[which.max(pro_files)]
pnl_s <- backtest_ep(re_turns=re_turns, price_s=price_s,
  re_balance="weeks", look_back=look_back, perform_ance=perform_ance,
  with_weights=TRUE)
cl_ose <- Cl(rutils::etf_env$VTI[index(pnl_s)])
# bind model returns with VTI
da_ta <- star_t
da_ta <- cbind(cl_ose, da_ta*pnl_s[, 1]+da_ta)
colnames(da_ta) <- c("VTI", "momentum")
# plot momentum strategy with VTI
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue")
chart_Series(da_ta, theme=plot_theme, lwd=2,
       name="Momentum PnL")
legend("topleft", legend=colnames(da_ta),
  inset=0.1, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")
# combine momentum strategy with static
da_ta <- cbind(da_ta, 0.5* (da_ta[, "VTI"] + da_ta[, "momentum"]))
colnames(da_ta) <- c("VTI", "momentum", "combined")
# calculate strategy annualized Sharpe ratios
sapply(da_ta, function(cumu_lative) {
  x_ts <- na.omit(diff(log(cumu_lative)))
  sqrt(52)*sum(x_ts)/sd(x_ts)/NROW(x_ts)
})  # end sapply
# calculate strategy correlations
cor(na.omit(diff(log(da_ta))))
# plot momentum strategy combined with VTI
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue", "green")
chart_Series(da_ta, theme=plot_theme,
       name="Momentum strategy combined with VTI")
legend("topleft", legend=colnames(da_ta),
  inset=0.1, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")
# Define all-weather symbols and weights
weight_s <- c(0.30, 0.55, 0.15)
all_weather <- (re_turns / price_s) %*% weight_s
all_weather <- cumsum(all_weather)
all_weather <- xts(all_weather, index(re_turns))[index(pnl_s)]
all_weather <- star_t*all_weather +
  star_t
colnames(all_weather) <- "all_weather"
# combine momentum strategy with all-weather
da_ta <- cbind(da_ta, all_weather)
# calculate strategy annualized Sharpe ratios
sapply(da_ta, function(cumu_lative) {
  x_ts <- na.omit(diff(log(cumu_lative)))
  sqrt(52)*sum(x_ts)/sd(x_ts)/NROW(x_ts)
})  # end sapply
# calculate strategy correlations
cor(na.omit(diff(log(da_ta))))
# plot momentum strategy, combined, and all-weather
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue", "green", "violet")
chart_Series(da_ta, theme=plot_theme, lwd=2, name="Momentum PnL")
legend("topleft", legend=colnames(da_ta),
  inset=0.1, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")
# calculate betas
beta_s <- c(1, rutils::etf_env$capm_stats[
  match(sym_bols[-1],
  rownames(rutils::etf_env$capm_stats)),
  "Beta"])
names(beta_s)[1] <- sym_bols[1]
# weights times betas
weight_s <- price_s[index(pnl_s)]*pnl_s[, -1]
beta_s <- weight_s %*% beta_s
beta_s <- xts(beta_s, order.by=index(weight_s))
colnames(beta_s) <- "portf_beta"
zoo::plot.zoo(cbind(beta_s, cl_ose),
  oma = c(3, 1, 3, 0), mar = c(0, 4, 0, 1),
  main="betas & VTI", xlab="")
momentum_rets <- as.numeric(rutils::diff_it(pnl_s[, 1]))
vti_rets <- as.numeric(rutils::diff_it(cl_ose)/100)
# Merton-Henriksson test
vti_b <- cbind(vti_rets, vti_rets+abs(vti_rets))
colnames(vti_b) <- c("rets", "sign")
mod_el <- lm(momentum_rets ~ vti_b)
summary(mod_el)
# open x11 for plotting
x11(width=6, height=4)
# set plot parameters to reduce whitespace around plot
par(mar=c(4, 4, 3, 1), oma=c(0, 0, 0, 0))
# Treynor-Mazuy test
vti_b <- cbind(vti_rets, vti_rets^2)
colnames(vti_b) <- c("rets", "squared")
mod_el <- lm(momentum_rets ~ vti_b)
summary(mod_el)
# plot scatterplot
plot(x=vti_rets, y=momentum_rets,
     xlab="VTI", ylab="momentum")
title(main="Treynor-Mazuy market timing test\n for Momentum vs VTI", line=0.5)
# plot fitted (predicted) response values
points(x=vti_rets, y=mod_el$fitted.values,
 pch=16, col="red")
# Normalize the returns
momentum_rets <-
  (momentum_rets-mean(momentum_rets))
momentum_rets <-
  sd(vti_rets)*momentum_rets/sd(momentum_rets)
vti_rets <- (vti_rets-mean(vti_rets))
# calculate ratios of moments
sapply(2:4, FUN=moments::moment, x=vti_rets)/
  sapply(2:4, FUN=moments::moment, x=momentum_rets)
# plot histogram
x_lim <- 4*sd(momentum_rets)
hist(momentum_rets, breaks=30,
  main="Momentum and VTI Return Distributions",
  xlim=c(-x_lim, x_lim),
  xlab="", ylab="", freq=FALSE)
# draw kernel density of histogram
lines(density(momentum_rets), col='red', lwd=2)
lines(density(vti_rets), col='blue', lwd=2)
# add legend
legend("topright", inset=0.05, cex=0.8, title=NULL,
 leg=c("Momentum", "VTI"),
 lwd=6, bg="white", col=c("red", "blue"))
