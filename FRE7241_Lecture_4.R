# define GARCH parameters
om_ega <- 0.01 ; al_pha <- 0.2
be_ta <- 0.2 ; len_gth <- 1000
re_turns <- numeric(len_gth)
vari_ance <- numeric(len_gth)
vari_ance[1] <- om_ega/(1-al_pha-be_ta)
re_turns[1] <- rnorm(1, sd=sqrt(vari_ance[1]))
# simulate GARCH model
set.seed(1121)  # reset random numbers
for (i in 2:len_gth) {
  re_turns[i] <- rnorm(n=1, sd=sqrt(vari_ance[i-1]))
  vari_ance[i] <- om_ega + al_pha*re_turns[i]^2 +
    be_ta*vari_ance[i-1]
}  # end for
# plot GARCH cumulative returns
plot(cumsum(re_turns/100), t="l",
  lwd=2, col="blue", xlab="", ylab="",
  main="GARCH cumulative returns")
date_s <- seq.Date(from=Sys.Date()-len_gth+1,
  to=Sys.Date(), length.out=len_gth)
x_ts <- xts:::xts(cumsum(re_turns/100), order.by=date_s)
dygraphs::dygraph(x_ts, main="GARCH cumulative returns")
# plot GARCH standard deviation
plot(sqrt(vari_ance), t="l",
  lwd=2, col="blue", xlab="", ylab="",
  main="GARCH standard deviation")
x_ts <- xts:::xts(sqrt(vari_ance), order.by=date_s)
dygraphs::dygraph(x_ts, main="GARCH standard deviation")
# define GARCH parameters
om_ega <- 0.0001 ; al_pha <- 0.5
be_ta <- 0.1 ; len_gth <- 10000
re_turns <- numeric(len_gth)
vari_ance <- numeric(len_gth)
vari_ance[1] <- om_ega/(1-al_pha-be_ta)
re_turns[1] <- rnorm(1, sd=sqrt(vari_ance[1]))
# simulate GARCH model
set.seed(1121)  # reset random numbers
for (i in 2:len_gth) {
  re_turns[i] <- rnorm(n=1, sd=sqrt(vari_ance[i-1]))
  vari_ance[i] <- om_ega + al_pha*re_turns[i]^2 +
    be_ta*vari_ance[i-1]
}  # end for
# calculate kurtosis of GARCH returns
moments::moment(re_turns, order=4) /
  moments::moment(re_turns, order=2)^2
# perform Jarque-Bera test of normality
tseries::jarque.bera.test(re_turns)
# plot histogram of GARCH returns
histo_gram <- hist(re_turns, col="lightgrey",
  xlab="returns", breaks=200, xlim=c(-0.05, 0.05),
  ylab="frequency", freq=FALSE,
  main="GARCH returns histogram")
lines(density(re_turns, adjust=1.5),
lwd=3, col="blue")
optim_fit <- MASS::fitdistr(re_turns,
  densfun="t", df=2, lower=c(-1, 1e-7))
lo_cation <- optim_fit$estimate[1]
sc_ale <- optim_fit$estimate[2]
curve(expr=dt((x-lo_cation)/sc_ale, df=2)/sc_ale,
  type="l", xlab="", ylab="", lwd=3,
  col="red", add=TRUE)
legend("topright", inset=0.05,
 leg=c("density", "t-distr w/ 2 dof"),
 lwd=6, lty=c(1, 1),
 col=c("blue", "red"))
# use fixed notation instead of exponential notation
options(scipen=999)
library(fGarch)
# fit returns into GARCH
garch_fit <- fGarch::garchFit(data=re_turns)
# fitted GARCH parameters
round(garch_fit@fit$coef, 5)
# actual GARCH parameters
round(c(mu=mean(re_turns), omega=om_ega,
  alpha=al_pha, beta=be_ta), 5)
# specify and simulate GARCH model
garch_spec <- fGarch::garchSpec(model=list(omega=om_ega,
  alpha=al_pha, beta=be_ta))
garch_sim <- fGarch::garchSim(spec=garch_spec, n=len_gth)
re_turns <- as.numeric(garch_sim)
# calculate kurtosis of GARCH returns
moments::moment(re_turns, order=4) /
  moments::moment(re_turns, order=2)^2
# perform Jarque-Bera test of normality
tseries::jarque.bera.test(re_turns)
# plot histogram of GARCH returns
histo_gram <- hist(re_turns, col="lightgrey",
  xlab="returns", breaks=200, xlim=c(-0.05, 0.05),
  ylab="frequency", freq=FALSE,
  main="GARCH returns histogram")
lines(density(re_turns, adjust=1.5),
lwd=3, col="blue")
optim_fit <- MASS::fitdistr(re_turns,
  densfun="t", df=2, lower=c(-1, 1e-7))
lo_cation <- optim_fit$estimate[1]
sc_ale <- optim_fit$estimate[2]
curve(expr=dt((x-lo_cation)/sc_ale, df=2)/sc_ale,
  type="l", xlab="", ylab="", lwd=3,
  col="red", add=TRUE)
legend("topright", inset=0.05,
 leg=c("density", "t-distr w/ 2 dof"),
 lwd=6, lty=c(1, 1),
 col=c("blue", "red"))
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
price_s <- merge(price_s,
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
# remove NA values
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
library(HighFreq)  # load package HighFreq
# select OHLC data
oh_lc <- rutils::env_etf$VTI["/2011"]
# calculate close prices
cl_ose <- Cl(oh_lc)
# define aggregation interval and decay parameter
look_back <- 31
lamb_da <- 0.05
# calculate EWMA prices
weight_s <- exp(-lamb_da*1:look_back)
weight_s <- weight_s/sum(weight_s)
ew_ma <- stats::filter(cl_ose, filter=rev(weight_s), sides=1)
ew_ma[1:(look_back-1)] <- ew_ma[look_back]
ew_ma <- xts(cbind(cl_ose, ew_ma),
       order.by=index(oh_lc))
colnames(ew_ma) <- c("VTI", "VTI EWMA")
# plot EWMA prices with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue")
chart_Series(ew_ma, theme=plot_theme,
       name="EWMA prices")
legend("top", legend=colnames(ew_ma),
 inset=0.1, bg="white", lty=c(1, 1), lwd=c(6, 6),
 col=plot_theme$col$line.col, bty="n")
library(HighFreq)  # load package HighFreq
# determine dates right after EWMA has crossed prices
in_dic <- sign(cl_ose - ew_ma[, 2])
trade_dates <- (rutils::diff_xts(in_dic) != 0)
trade_dates <- which(trade_dates) + 1
# calculate positions, either: -1, 0, or 1
position_s <- rep(NA_integer_, NROW(cl_ose))
position_s[1] <- 0
position_s[trade_dates] <-
  rutils::lag_xts(in_dic)[trade_dates]
position_s <- na.locf(position_s)
position_s <- xts(position_s, order.by=index(oh_lc))
# plot EWMA prices with position shading
chart_Series(ew_ma, theme=plot_theme,
       name="EWMA prices")
add_TA(position_s > 0, on=-1,
 col="lightgreen", border="lightgreen")
add_TA(position_s < 0, on=-1,
 col="lightgrey", border="lightgrey")
legend("top", legend=colnames(ew_ma),
 inset=0.1, bg="white", lty=c(1, 1), lwd=c(6, 6),
 col=plot_theme$col$line.col, bty="n")
library(HighFreq)  # load package HighFreq
# calculate open and lagged prices
op_en <- Op(oh_lc)
prices_lag <- rutils::lag_xts(cl_ose)
position_lagged <- rutils::lag_xts(position_s)
# calculate daily profits and losses
re_turns <- position_lagged*(cl_ose - prices_lag)
re_turns[trade_dates] <-
  position_lagged[trade_dates] *
  (op_en[trade_dates] - prices_lag[trade_dates]) +
  position_s[trade_dates] *
  (cl_ose[trade_dates] - op_en[trade_dates])
# calculate annualized Sharpe ratio of strategy returns
sqrt(260)*sum(re_turns)/sd(re_turns)/NROW(re_turns)
pnl_s <- cumsum(re_turns)
pnl_s <- cbind(cl_ose-as.numeric(cl_ose[1, ]), pnl_s)
colnames(pnl_s) <- c("VTI", "EWMA PnL")
library(HighFreq)  # load package HighFreq
# plot EWMA PnL with position shading
chart_Series(pnl_s, theme=plot_theme,
       name="Performance of EWMA Strategy")
add_TA(position_s > 0, on=-1,
 col="lightgreen", border="lightgreen")
add_TA(position_s < 0, on=-1,
 col="lightgrey", border="lightgrey")
legend("top", legend=colnames(pnl_s),
 inset=0.05, bg="white", lty=c(1, 1), lwd=c(6, 6),
 col=plot_theme$col$line.col, bty="n")
library(HighFreq)  # load package HighFreq
simu_ewma <- function(oh_lc, lamb_da=0.05, look_back=31) {
  # calculate EWMA prices
  weight_s <- exp(-lamb_da*1:look_back)
  weight_s <- weight_s/sum(weight_s)
  cl_ose <- Cl(oh_lc)
  ew_ma <- stats::filter(as.numeric(cl_ose), filter=rev(weight_s), sides=1)
  ew_ma[1:(look_back-1)] <- ew_ma[look_back]
  # determine dates right after EWMA has crossed prices
  in_dic <- xts(sign(as.numeric(cl_ose) - ew_ma), order.by=index(oh_lc))
  trade_dates <- (rutils::diff_xts(in_dic) != 0)
  trade_dates <- which(trade_dates) + 1
  trade_dates <- trade_dates[trade_dates<NROW(oh_lc)]
  # calculate positions, either: -1, 0, or 1
  position_s <- rep(NA_integer_, NROW(cl_ose))
  position_s[1] <- 0
  position_s[trade_dates] <- rutils::lag_xts(in_dic)[trade_dates]
  position_s <- xts(na.locf(position_s), order.by=index(oh_lc))
  op_en <- Op(oh_lc)
  prices_lag <- rutils::lag_xts(cl_ose)
  position_lagged <- rutils::lag_xts(position_s)
  # calculate daily profits and losses
  re_turns <- position_lagged*(cl_ose - prices_lag)
  re_turns[trade_dates] <-
    position_lagged[trade_dates] *
    (op_en[trade_dates] - prices_lag[trade_dates]) +
    position_s[trade_dates] *
    (cl_ose[trade_dates] - op_en[trade_dates])
  out_put <- cbind(position_s, re_turns)
  colnames(out_put) <- c("position_s", "re_turns")
  out_put
}  # end simu_ewma
lamb_das <- seq(0.001, 0.03, 0.001)
sharpe_ratios <- sapply(lamb_das, function(lamb_da) {
  re_turns <- simu_ewma(oh_lc=oh_lc,
        lamb_da=lamb_da)[, 2]
  # calculate annualized Sharpe ratio of strategy returns
  sqrt(260)*sum(re_turns)/sd(re_turns)/NROW(re_turns)
})  # end sapply
plot(x=lamb_das, y=sharpe_ratios, t="l",
     main="Performance of EWMA trend-following strategies
     as function of the decay parameter lambda")
# simulate best performing strategy
ewma_trend <- simu_ewma(oh_lc=oh_lc,
  lamb_da=lamb_das[which.max(sharpe_ratios)])
position_s <- ewma_trend[, 1]
pnl_s <- cumsum(ewma_trend[, 2])
pnl_s <- cbind(cl_ose-as.numeric(cl_ose[1, ]),
        pnl_s)
colnames(pnl_s) <- c("VTI", "EWMA PnL")
# plot EWMA PnL with position shading
chart_Series(pnl_s, theme=plot_theme,
       name="Performance of EWMA Strategy")
add_TA(position_s > 0, on=-1,
 col="lightgreen", border="lightgreen")
add_TA(position_s < 0, on=-1,
 col="lightgrey", border="lightgrey")
legend("top", legend=colnames(pnl_s),
 inset=0.05, bg="white", lty=c(1, 1), lwd=c(6, 6),
 col=plot_theme$col$line.col, bty="n")
lamb_das <- seq(0.05, 0.12, 0.01)
sharpe_ratios <- sapply(lamb_das, function(lamb_da) {
  re_turns <- -simu_ewma(oh_lc=oh_lc,
        lamb_da=lamb_da)[, 2]
  # calculate annualized Sharpe ratio of strategy returns
  sqrt(260)*sum(re_turns)/sd(re_turns)/NROW(re_turns)
})  # end sapply
plot(x=lamb_das, y=sharpe_ratios, t="l",
     main="Performance of EWMA mean-reverting strategies
     as function of the decay parameter lambda")
# simulate best performing strategy
ewma_revert <- -simu_ewma(oh_lc=oh_lc,
  lamb_da=lamb_das[which.max(sharpe_ratios)])
position_s <- ewma_revert[, 1]
pnl_s <- cumsum(ewma_revert[, 2])
pnl_s <- cbind(cl_ose-as.numeric(cl_ose[1, ]),
        pnl_s)
colnames(pnl_s) <- c("VTI", "EWMA PnL")
# plot EWMA PnL with position shading
chart_Series(pnl_s, theme=plot_theme,
       name="Performance of EWMA Strategy")
add_TA(position_s > 0, on=-1,
 col="lightgreen", border="lightgreen")
add_TA(position_s < 0, on=-1,
 col="lightgrey", border="lightgrey")
legend("top", legend=colnames(pnl_s),
 inset=0.05, bg="white", lty=c(1, 1), lwd=c(6, 6),
 col=plot_theme$col$line.col, bty="n")
# calculate correlation between trend-following and mean-reverting strategies
trend_ing <- ewma_trend[, 2]
colnames(trend_ing) <- "trend"
revert_ing <- ewma_revert[, 2]
colnames(revert_ing) <- "revert"
corr_matrix <- cor(cbind(trend_ing, revert_ing))
corr_matrix
# calculate combined strategy
re_turns <- trend_ing + revert_ing
# calculate annualized Sharpe ratio of strategy returns
sapply(cbind(rutils::diff_xts(cl_ose),
    trend_ing, revert_ing, re_turns),
function(re_turns) {
  sqrt(260)*sum(re_turns)/sd(re_turns)/NROW(re_turns)
})  # end sapply
pnl_s <- cumsum(re_turns)
pnl_s <- cbind(cl_ose-as.numeric(cl_ose[1, ]),
        pnl_s)
colnames(pnl_s) <- c("VTI", "EWMA combined PnL")
chart_Series(pnl_s, theme=plot_theme,
       name="Performance of EWMA Strategies")
add_TA(cumsum(trend_ing), on=1, lwd=2, col="green")
add_TA(cumsum(revert_ing), on=1, lwd=2, col="magenta2")
legend("topleft", legend=c(colnames(pnl_s), "trending", "reverting"),
 inset=0.05, bg="white", lty=rep(1, 4), lwd=rep(4, 4),
 col=c(plot_theme$col$line.col, "green", "magenta2"), bty="n")
lamb_das <- seq(0.001, 0.03, 0.001)
re_turns <- lapply(lamb_das, function(lamb_da) {
  simu_ewma(oh_lc=oh_lc, lamb_da=lamb_da)[, 2]
})  # end sapply
re_turns <- do.call(merge, re_turns)
weight_s <- sharpe_ratios/sum(sharpe_ratios)
re_turns <- re_turns %*% weight_s
re_turns <- xts(re_turns, order.by=index(oh_lc))
pnl_s <- cumsum(re_turns)
pnl_s <- cbind(cl_ose-as.numeric(cl_ose[1, ]),
        pnl_s)
colnames(pnl_s) <- c("VTI", "EWMA PnL")
# plot EWMA PnL without position shading
chart_Series(pnl_s, theme=plot_theme,
       name="Performance of EWMA Strategy")
legend("top", legend=colnames(pnl_s),
 inset=0.05, bg="white", lty=c(1, 1), lwd=c(6, 6),
 col=plot_theme$col$line.col, bty="n")
library(HighFreq)  # load package HighFreq
# calculate open, close, and lagged prices
op_en <- Op(rutils::env_etf$VTI)
cl_ose <- Cl(rutils::env_etf$VTI)
prices_lag <- rutils::lag_xts(cl_ose)
# define aggregation interval and calculate VWAP
look_back <- 150
VTI_vwap <- HighFreq::roll_vwap(rutils::env_etf$VTI,
      look_back=look_back)
# calculate VWAP indicator
in_dic <- sign(cl_ose - VTI_vwap)
# determine dates right after VWAP has crossed prices
trade_dates <- (rutils::diff_xts(in_dic) != 0)
trade_dates <- which(trade_dates) + 1
# plot prices and VWAP
chart_Series(x=cl_ose,
  name="VTI prices", col="orange")
add_TA(VTI_vwap, on=1, lwd=2, col="blue")
legend("top", legend=c("VTI", "VWAP"),
bg="white", lty=c(1, 1), lwd=c(6, 6),
col=c("orange", "blue"), bty="n")
library(HighFreq)  # load package HighFreq
# calculate positions, either: -1, 0, or 1
position_s <- NA*numeric(NROW(rutils::env_etf$VTI))
position_s[1] <- 0
position_s[trade_dates] <- in_dic[trade_dates]
position_s <- na.locf(position_s)
position_s <- xts(position_s, order.by=index((rutils::env_etf$VTI)))
position_lagged <- rutils::lag_xts(position_s)
# calculate daily profits and losses
pnl_s <- position_lagged*(cl_ose - prices_lag)
pnl_s[trade_dates] <- position_lagged[trade_dates] *
  (op_en[trade_dates] - prices_lag[trade_dates]) +
  position_s[trade_dates] *
  (cl_ose[trade_dates] - op_en[trade_dates])
# calculate annualized Sharpe ratio of strategy returns
sqrt(260)*sum(pnl_s)/sd(pnl_s)/NROW(pnl_s)
# plot prices and VWAP
pnl_s <- xts(cumsum(pnl_s), order.by=index((rutils::env_etf$VTI)))
chart_Series(x=(cl_ose-as.numeric(cl_ose[1, ])), name="VTI prices", col="orange")
add_TA(pnl_s, on=1, lwd=2, col="blue")
add_TA(position_s > 0, on=-1,
 col="lightgreen", border="lightgreen")
add_TA(position_s < 0, on=-1,
 col="lightgrey", border="lightgrey")
legend("top", legend=c("VTI", "VWAP strategy"),
 inset=0.1, bg="white", lty=c(1, 1), lwd=c(6, 6),
 col=c("orange", "blue"), bty="n")
# verify that rtools are working properly:
devtools::find_rtools()
devtools::has_devel()

# load package Rcpp
library(Rcpp)
# get documentation for package Rcpp
# get short description
packageDescription("Rcpp")
# load help page
help(package="Rcpp")
# list all datasets in "Rcpp"
data(package="Rcpp")
# list all objects in "Rcpp"
ls("package:Rcpp")
# remove Rcpp from search path
detach("package:Rcpp")
# define Rcpp function
Rcpp::cppFunction("
  int times_two(int x)
    { return 2 * x;}
  ")  # end cppFunction
# run Rcpp function
times_two(3)
# source Rcpp functions from file
Rcpp::sourceCpp(file="C:/Develop/R/lecture_slides/scripts/rcpp_mult.cpp")
# multiply two numbers
rcpp_mult(2, 3)
rcpp_mult(1:3, 6:4)
# multiply two vectors
rcpp_mult_vec(2, 3)
rcpp_mult_vec(1:3, 6:4)
# define Rcpp function with loop
Rcpp::cppFunction("
double inner_mult(NumericVector x, NumericVector y) {
int x_size = x.size();
int y_size = y.size();
if (x_size != y_size) {
    return 0;
  } else {
    double total = 0;
    for(int i = 0; i < x_size; ++i) {
total += x[i] * y[i];
  }
  return total;
  }
}")  # end cppFunction
# run Rcpp function
inner_mult(1:3, 6:4)
inner_mult(1:3, 6:3)
# define Rcpp Sugar function with loop
Rcpp::cppFunction("
double inner_mult_sugar(NumericVector x, NumericVector y) {
  return sum(x * y);
}")  # end cppFunction
# run Rcpp Sugar function
inner_mult_sugar(1:3, 6:4)
inner_mult_sugar(1:3, 6:3)
# define R function with loop
inner_mult_r <- function(x, y) {
    to_tal <- 0
    for(i in 1:NROW(x)) {
to_tal <- to_tal + x[i] * y[i]
    }
    to_tal
}  # end inner_mult_r
# run R function
inner_mult_r(1:3, 6:4)
inner_mult_r(1:3, 6:3)
# compare speed of Rcpp and R
library(microbenchmark)
summary(microbenchmark(
  pure_r=inner_mult_r(1:10000, 1:10000),
  inner_r=1:10000 %*% 1:10000,
  r_cpp=inner_mult(1:10000, 1:10000),
  r_cpp_sugar=inner_mult_sugar(1:10000, 1:10000),
  times=10))[, c(1, 4, 5)]
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
# create random real symmetric matrix
mat_rix <- matrix(runif(25), nc=5)
mat_rix <- mat_rix + t(mat_rix)
# calculate eigenvectors and eigenvalues
ei_gen <- eigen(mat_rix)
eigen_vec <- ei_gen$vectors
dim(eigen_vec)
# plot eigenvalues
barplot(ei_gen$values,
  xlab="", ylab="", las=3,
  names.arg=paste0("ev", 1:NROW(ei_gen$values)),
  main="Eigenvalues of a real symmetric matrix")
# eigenvectors form an orthonormal basis
round(t(eigen_vec) %*% eigen_vec,
  digits=4)
# diagonalize matrix using eigenvector matrix
round(t(eigen_vec) %*% (mat_rix %*% eigen_vec),
  digits=4)
ei_gen$values
# eigen decomposition of matrix by rotating the diagonal matrix
eigen_decomp <- eigen_vec %*% (ei_gen$values * t(eigen_vec))
# create diagonal matrix of eigenvalues
# diago_nal <- diag(ei_gen$values)
# eigen_decomp <- eigen_vec %*% (diago_nal %*% t(eigen_vec))
all.equal(mat_rix, eigen_decomp)
# create random positive semi-definite matrix
mat_rix <- matrix(runif(25), nc=5)
mat_rix <- t(mat_rix) %*% mat_rix
# calculate eigenvectors and eigenvalues
ei_gen <- eigen(mat_rix)
ei_gen$values
# plot eigenvalues
barplot(ei_gen$values, las=3,
  xlab="", ylab="",
  names.arg=paste0("ev", 1:NROW(ei_gen$values)),
  main="Eigenvalues of positive semi-definite matrix")
# dimensions of left and right matrices
n_left <- 6 ; n_right <- 4
# create random positive semi-definite matrix
left_mat <- matrix(runif(n_left^2), nc=n_left)
left_mat <- crossprod(left_mat)
# or
left_mat <- left_mat %*% t(left_mat)
# calculate left eigenvectors
ei_gen <- eigen(left_mat)
left_mat <- ei_gen$vectors[, 1:n_right]
# create random positive semi-definite matrix
right_mat <- matrix(runif(n_right^2), nc=n_right)
right_mat <- crossprod(right_mat)
# or
right_mat <- right_mat %*% t(right_mat)
# calculate right eigenvectors and singular values
ei_gen <- eigen(right_mat)
right_mat <- ei_gen$vectors
sing_values <- ei_gen$values
# compose rectangular matrix
mat_rix <-
  left_mat %*% (sing_values * t(right_mat))
# mat_rix <- left_mat %*% diag(sing_values) %*% t(right_mat)
# perform singular value decomposition
s_vd <- svd(mat_rix)
# compare SVD with inputs
all.equal(abs(s_vd$u), abs(left_mat))
all.equal(abs(s_vd$v), abs(right_mat))
all.equal(s_vd$d, ei_gen$values)
# create random positive semi-definite matrix
mat_rix <- matrix(runif(25), nc=5)
mat_rix <- t(mat_rix) %*% mat_rix
# calculate the inverse of mat_rix
in_verse <- solve(a=mat_rix)
# multiply inverse with matrix
round(in_verse %*% mat_rix, 4)
round(mat_rix %*% in_verse, 4)

# calculate eigenvectors and eigenvalues
ei_gen <- eigen(mat_rix)
eigen_vec <- ei_gen$vectors

# perform eigen decomposition of inverse
eigen_inverse <-
  eigen_vec %*% (t(eigen_vec) / ei_gen$values)
all.equal(in_verse, eigen_inverse)
# decompose diagonal matrix with inverse of eigenvalues
# diago_nal <- diag(1/ei_gen$values)
# eigen_inverse <-
#   eigen_vec %*% (diago_nal %*% t(eigen_vec))
# create random rectangular matrix
# case when: n_left > n_right
n_left <- 6 ; n_right <- 4
mat_rix <- matrix(runif(n_left*n_right),
  nc=n_right)
# calculate generalized inverse of mat_rix
in_verse <- MASS::ginv(mat_rix)
round(in_verse %*% mat_rix, 4)
all.equal(mat_rix,
    mat_rix %*% in_verse %*% mat_rix)
# create random rectangular matrix
# case when: n_left < n_right
n_left <- 4 ; n_right <- 6
mat_rix <- matrix(runif(n_left*n_right),
  nc=n_right)
# calculate generalized inverse of mat_rix
in_verse <- MASS::ginv(mat_rix)
round(mat_rix %*% in_verse, 4)
# perform singular value decomposition
s_vd <- svd(mat_rix)
# calculate generalized inverse from SVD
svd_inverse <- s_vd$v %*% (t(s_vd$u) / s_vd$d)
all.equal(svd_inverse, in_verse)
# calculate Moore-Penrose pseudo-inverse
mp_inverse <-
  MASS::ginv(t(mat_rix) %*% mat_rix) %*% t(mat_rix)
all.equal(mp_inverse, in_verse)
# create random singular matrix
n_left <- 4 ; n_right <- 6
mat_rix <- matrix(runif(n_left*n_right), nc=n_right)
mat_rix <- t(mat_rix) %*% mat_rix
# calculate generalized inverse of mat_rix
in_verse <- MASS::ginv(mat_rix)
# verify inverse of mat_rix
all.equal(mat_rix,
  mat_rix %*% in_verse %*% mat_rix)
# perform singular value decomposition
s_vd <- svd(mat_rix)
# set tolerance for determining zero singular values
to_l <- sqrt(.Machine$double.eps)
# check for zero singular values
s_vd$d
not_zero <- (s_vd$d > (to_l * s_vd$d[1]))
# calculate generalized inverse from SVD
svd_inverse <-
  s_vd$v[, not_zero] %*%
  (t(s_vd$u[, not_zero]) / s_vd$d[not_zero])
all.equal(svd_inverse, in_verse)
# calculate Moore-Penrose pseudo-inverse
mp_inverse <-
  MASS::ginv(t(mat_rix) %*% mat_rix) %*% t(mat_rix)
all.equal(mp_inverse, in_verse)
# diagonalize the "unit" matrix
uni_t <- mat_rix %*% in_verse
round(uni_t, 4)
round(mat_rix %*% in_verse, 4)
round(t(s_vd$u) %*% uni_t %*% s_vd$v, 4)
# define a square matrix
mat_rix <- matrix(c(1, 2, -1, 2), nc=2)
vec_tor <- c(2, 1)
# calculate the inverse of mat_rix
in_verse <- solve(a=mat_rix)
in_verse %*% mat_rix
# calculate solution using inverse of mat_rix
solu_tion <- in_verse %*% vec_tor
mat_rix %*% solu_tion
# calculate solution of linear system
solu_tion <- solve(a=mat_rix, b=vec_tor)
mat_rix %*% solu_tion
# create large random positive semi-definite matrix
mat_rix <- matrix(runif(1e4), nc=100)
mat_rix <- t(mat_rix) %*% mat_rix
# calculate eigen decomposition
ei_gen <- eigen(mat_rix)
eigen_values <- ei_gen$values
eigen_vec <- ei_gen$vectors
# set tolerance for determining zero singular values
to_l <- sqrt(.Machine$double.eps)
# if needed convert to positive definite matrix
not_zero <- (eigen_values > (to_l * eigen_values[1]))
if (sum(!not_zero) > 0) {
  eigen_values[!not_zero] <- 2*to_l
  mat_rix <- eigen_vec %*%
    (eigen_values * t(eigen_vec))
}  # end if
# calculate the Cholesky mat_rix
choles_ky <- chol(mat_rix)
choles_ky[1:5, 1:5]
all.equal(mat_rix, t(choles_ky) %*% choles_ky)
# calculate inverse from Cholesky
chol_inverse <- chol2inv(choles_ky)
all.equal(solve(mat_rix), chol_inverse)
# compare speed of Cholesky inversion
library(microbenchmark)
summary(microbenchmark(
  sol_ve=solve(mat_rix),
  choles_ky=chol2inv(chol(mat_rix)),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# calculate random covariance matrix
cov_mat <- matrix(runif(25), nc=5)
cov_mat <- t(cov_mat) %*% cov_mat
# calculate the Cholesky mat_rix
choles_ky <- chol(cov_mat)
choles_ky
# simulate random uncorrelated returns
n_assets <- 5
n_rows <- 10000
re_turns <- matrix(rnorm(n_assets*n_rows), nc=n_assets)
# calculate correlated returns by applying Cholesky
corr_returns <- re_turns %*% choles_ky
# calculate covariance matrix
cov_returns <- crossprod(corr_returns) / (n_rows-1)
all.equal(cov_mat, cov_returns)
# simulate random portfolio returns
n_assets <- 10
n_rows <- 100
re_turns <- matrix(rnorm(n_assets*n_rows), nc=n_assets)
# de-mean the returns
re_turns <- apply(re_turns, MARGIN=2, function(x) (x-mean(x)))
# calculate covariance matrix
cov_mat <- crossprod(re_turns) / (n_rows-1)
# calculate eigenvectors and eigenvalues
ei_gen <- eigen(cov_mat)
ei_gen$values
barplot(ei_gen$values, # plot eigenvalues
  xlab="", ylab="", las=3,
  names.arg=paste0("ev", 1:NROW(ei_gen$values)),
  main="Eigenvalues of covariance matrix")
# calculate eigenvectors and eigenvalues
# as function of number of returns
n_data <- ((n_assets/2):(2*n_assets))
e_values <- sapply(n_data, function(x) {
  re_turns <- re_turns[1:x, ]
  re_turns <- apply(re_turns, MARGIN=2,
    function(y) (y-mean(y)))
  cov_mat <- crossprod(re_turns) / (x-1)
  min(eigen(cov_mat)$values)
})  # end sapply
plot(y=e_values, x=n_data, t="l",
  xlab="", ylab="", lwd=3, col="blue",
  main="Smallest eigenvalue of covariance matrix\nas function of number of returns")
# formula of linear model with zero intercept
lin_formula <- z ~ x + y - 1
lin_formula

# collapse vector of strings into single text string
paste0("x", 1:5)
paste(paste0("x", 1:5), collapse="+")

# create formula from text string
lin_formula <- as.formula(
  # coerce text strings to formula
  paste("z ~ ",
  paste(paste0("x", 1:5), collapse="+")
  )  # end paste
)  # end as.formula
class(lin_formula)
lin_formula
# modify the formula using "update"
update(lin_formula, log(.) ~ . + beta)
set.seed(1121)  # initialize random number generator
# define explanatory variable
len_gth <- 100
explana_tory <- rnorm(len_gth, mean=2)
noise <- rnorm(len_gth)
# response equals linear form plus error terms
res_ponse <- -3 + explana_tory + noise
# calculate de-meaned explanatory and response vectors
explan_zm <- explana_tory - mean(explana_tory)
response_zm <- res_ponse - mean(res_ponse)
# solve for regression beta
be_ta <- sum(explan_zm*response_zm) / sum(explan_zm^2)
# solve for regression alpha
al_pha <- mean(res_ponse) - be_ta * mean(explana_tory)
# specify regression formula
reg_formula <- res_ponse ~ explana_tory
reg_model <- lm(reg_formula)  # perform regression
class(reg_model)  # regressions have class lm
attributes(reg_model)
eval(reg_model$call$formula)  # regression formula
reg_model$coefficients  # regression coefficients
coef(reg_model)
c(al_pha, be_ta)
x11(width=6, height=5)  # open x11 for plotting
# set plot parameters to reduce whitespace around plot
par(mar=c(5, 5, 1, 1), oma=c(0, 0, 0, 0))
# plot scatterplot using formula
plot(reg_formula)
title(main="Simple Regression", line=-1)
# add regression line
abline(reg_model, lwd=2, col="red")
# plot fitted (predicted) response values
points(x=explana_tory, y=reg_model$fitted.values,
       pch=16, col="blue")
# sum of residuals = 0
sum(reg_model$residuals)
x11(width=6, height=5)  # open x11 for plotting
# set plot parameters to reduce whitespace around plot
par(mar=c(5, 5, 1, 1), oma=c(0, 0, 0, 0))
# extract residuals
resi_duals <- cbind(explana_tory, reg_model$residuals)
colnames(resi_duals) <- c("explanatory variable", "residuals")
# plot residuals
plot(resi_duals)
title(main="Residuals of the Linear Regression", line=-1)
abline(h=0, lwd=2, col="red")
reg_model_sum <- summary(reg_model)  # copy regression summary
reg_model_sum  # print the summary to console
attributes(reg_model_sum)$names  # get summary elements
reg_model_sum$coefficients
reg_model_sum$r.squared
reg_model_sum$adj.r.squared
reg_model_sum$fstatistic
# standard error of beta
reg_model_sum$
  coefficients["explana_tory", "Std. Error"]
sd(reg_model_sum$residuals)/sd(explana_tory)/
  sqrt(unname(reg_model_sum$fstatistic[3]))
anova(reg_model)
set.seed(1121)  # initialize random number generator
# high noise compared to coefficient
res_ponse <- 3 + explana_tory + rnorm(30, sd=8)
reg_model <- lm(reg_formula)  # perform regression
# estimate of regression coefficient is not
# statistically significant
summary(reg_model)
par(oma=c(1, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=1.0, cex.axis=1.0, cex.main=1.0, cex.sub=1.0)
reg_stats <- function(std_dev) {  # noisy regression
  set.seed(1121)  # initialize number generator
# create explanatory and response variables
  explana_tory <- rnorm(100, mean=2)
  res_ponse <- 3 + 0.2*explana_tory +
    rnorm(NROW(explana_tory), sd=std_dev)
# specify regression formula
  reg_formula <- res_ponse ~ explana_tory
# perform regression and get summary
  reg_model_sum <- summary(lm(reg_formula))
# extract regression statistics
  with(reg_model_sum, c(pval=coefficients[2, 4],
   adj_rsquared=adj.r.squared,
   fstat=fstatistic[1]))
}  # end reg_stats
# apply reg_stats() to vector of std dev values
vec_sd <- seq(from=0.1, to=0.5, by=0.1)
names(vec_sd) <- paste0("sd=", vec_sd)
mat_stats <- t(sapply(vec_sd, reg_stats))
# plot in loop
par(mfrow=c(NCOL(mat_stats), 1))
for (in_dex in 1:NCOL(mat_stats)) {
  plot(mat_stats[, in_dex], type="l",
 xaxt="n", xlab="", ylab="", main="")
  title(main=colnames(mat_stats)[in_dex], line=-1.0)
  axis(1, at=1:(NROW(mat_stats)),
 labels=rownames(mat_stats))
}  # end for
reg_stats <- function(da_ta) {  # get regression
# perform regression and get summary
  col_names <- colnames(da_ta)
  reg_formula <-
    paste(col_names[2], col_names[1], sep="~")
  reg_model_sum <- summary(lm(reg_formula,
                        data=da_ta))
# extract regression statistics
  with(reg_model_sum, c(pval=coefficients[2, 4],
   adj_rsquared=adj.r.squared,
   fstat=fstatistic[1]))
}  # end reg_stats
# apply reg_stats() to vector of std dev values
vec_sd <- seq(from=0.1, to=0.5, by=0.1)
names(vec_sd) <- paste0("sd=", vec_sd)
mat_stats <-
  t(sapply(vec_sd, function (std_dev) {
    set.seed(1121)  # initialize number generator
# create explanatory and response variables
    explana_tory <- rnorm(100, mean=2)
    res_ponse <- 3 + 0.2*explana_tory +
rnorm(NROW(explana_tory), sd=std_dev)
    reg_stats(data.frame(explana_tory, res_ponse))
    }))
# plot in loop
par(mfrow=c(NCOL(mat_stats), 1))
for (in_dex in 1:NCOL(mat_stats)) {
  plot(mat_stats[, in_dex], type="l",
 xaxt="n", xlab="", ylab="", main="")
  title(main=colnames(mat_stats)[in_dex], line=-1.0)
  axis(1, at=1:(NROW(mat_stats)),
 labels=rownames(mat_stats))
}  # end for
# set plot paramaters - margins and font scale
par(oma=c(1,0,1,0), mgp=c(2,1,0), mar=c(2,1,2,1), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2, 2))  # plot 2x2 panels
plot(reg_model)  # plot diagnostic scatterplots
plot(reg_model, which=2)  # plot just Q-Q
library(lmtest)  # load lmtest
# perform Durbin-Watson test
dwtest(reg_model)
set.seed(1121)  # initialize random number generator
# define explanatory variable
len_gth <- 100
n_var <- 5
explana_tory <- matrix(rnorm(n_var*len_gth), 
  nc=n_var)
noise <- rnorm(len_gth, sd=0.5)
# response equals linear form plus error terms
weight_s <- rnorm(n_var)
res_ponse <- 
  -3 + explana_tory %*% weight_s + noise
# calculate de-meaned explanatory matrix
explan_zm <- t(t(explana_tory) - colSums(explana_tory)/len_gth)
# or
explan_zm <- apply(explan_zm, 2, function(x) (x-mean(x)))
# calculate de-meaned response vector
response_zm <- res_ponse - mean(res_ponse)
# solve for regression betas
beta_s <- MASS::ginv(explan_zm) %*% response_zm
# solve for regression alpha
al_pha <- mean(res_ponse) - colSums(explana_tory) %*% beta_s / len_gth
# multivariate regression using lm()
reg_model <- lm(res_ponse ~ explana_tory)
coef(reg_model)
c(al_pha, beta_s)
c(-3, weight_s)
library(lmtest)  # load lmtest
de_sign <- data.frame(  # design matrix
  explana_tory=1:30, omit_var=sin(0.2*1:30))
# response depends on both explanatory variables
res_ponse <- with(de_sign,
  0.2*explana_tory + omit_var + 0.2*rnorm(30))
# mis-specified regression only one explanatory
reg_model <- lm(res_ponse ~ explana_tory,
        data=de_sign)
reg_model_sum <- summary(reg_model)
reg_model_sum$coefficients
reg_model_sum$r.squared
# Durbin-Watson test shows residuals are autocorrelated
dwtest(reg_model)$p.value
par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # set plot panels
plot(reg_formula, data=de_sign)
abline(reg_model, lwd=2, col="red")
title(main="OVB Regression", line=-1)
plot(reg_model, which=2, ask=FALSE)  # plot just Q-Q
set.seed(1121)
library(lmtest)
# spurious regression in unit root time series
explana_tory <- cumsum(rnorm(100))  # unit root time series
res_ponse <- cumsum(rnorm(100))
reg_formula <- res_ponse ~ explana_tory
reg_model <- lm(reg_formula)  # perform regression
# summary indicates statistically significant regression
reg_model_sum <- summary(reg_model)
reg_model_sum$coefficients
reg_model_sum$r.squared
# Durbin-Watson test shows residuals are autocorrelated
dw_test <- dwtest(reg_model)
c(dw_test$statistic[[1]], dw_test$p.value)
par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # set plot panels
plot(reg_formula, xlab="", ylab="")  # plot scatterplot using formula
title(main="Spurious Regression", line=-1)
# add regression line
abline(reg_model, lwd=2, col="red")
plot(reg_model, which=2, ask=FALSE)  # plot just Q-Q
explana_tory <- seq(from=0.1, to=3.0, by=0.1)  # explanatory variable
res_ponse <- 3 + 2*explana_tory + rnorm(30)
reg_formula <- res_ponse ~ explana_tory
reg_model <- lm(reg_formula)  # perform regression
new_data <- data.frame(explana_tory=0.1*31:40)
predict_lm <- predict(object=reg_model,
              newdata=new_data, level=0.95,
              interval="confidence")
predict_lm <- as.data.frame(predict_lm)
head(predict_lm, 2)
plot(reg_formula, xlim=c(1.0, 4.0),
     ylim=range(res_ponse, predict_lm),
     main="Regression predictions")
abline(reg_model, col="red")
with(predict_lm, {
  points(x=new_data$explana_tory, y=fit, pch=16, col="blue")
  lines(x=new_data$explana_tory, y=lwr, lwd=2, col="red")
  lines(x=new_data$explana_tory, y=upr, lwd=2, col="red")
})  # end with
