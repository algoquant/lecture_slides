library(knitr)
opts_chunk$set(prompt=TRUE, eval=FALSE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
options(digits=3)
thm <- knit_theme$get("acid")
knit_theme$set(thm)
library(HighFreq)  # load package HighFreq
# select OHLC data
oh_lc <- rutils::env_etf$VTI["/2011"]
# calculate close prices
cl_ose <- Cl(oh_lc)
# define lookback window and decay parameter
win_dow <- 51
lamb_da <- 0.05
# calculate EWMA prices
weight_s <- exp(-lamb_da*1:win_dow)
weight_s <- weight_s/sum(weight_s)
ew_ma <- filter(cl_ose, filter=weight_s, sides=1)
ew_ma[1:(win_dow-1)] <- ew_ma[win_dow]
ew_ma <- xts(cbind(cl_ose, ew_ma),
       order.by=index(oh_lc))
colnames(ew_ma) <- c("VTI", "VTI EWMA")
# plot EWMA prices with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue")
chart_Series(ew_ma, theme=plot_theme,
       name="EWMA prices")
legend("top", legend=colnames(ew_ma),
 inset=0.1, bg="white", lty=c(1, 1), lwd=c(2, 2),
 col=plot_theme$col$line.col, bty="n")
library(HighFreq)  # load package HighFreq
# determine dates right after EWMA has crossed prices
in_dic <- sign(cl_ose - ew_ma[, 2])
trade_dates <- (rutils::diff_xts(in_dic) != 0)
trade_dates <- which(trade_dates) + 1
# calculate positions, either: -1, 0, or 1
po_sitions <- rep(NA_integer_, NROW(cl_ose))
po_sitions[1] <- 0
po_sitions[trade_dates] <-
  rutils::lag_xts(in_dic)[trade_dates]
po_sitions <- na.locf(po_sitions)
po_sitions <- xts(po_sitions, order.by=index(oh_lc))
# plot EWMA prices with position shading
chart_Series(ew_ma, theme=plot_theme,
       name="EWMA prices")
add_TA(po_sitions > 0, on=-1,
 col="lightgreen", border="lightgreen")
add_TA(po_sitions < 0, on=-1,
 col="lightgrey", border="lightgrey")
legend("top", legend=colnames(ew_ma),
 inset=0.1, bg="white", lty=c(1, 1), lwd=c(2, 2),
 col=plot_theme$col$line.col, bty="n")
library(HighFreq)  # load package HighFreq
# calculate open and lagged prices
op_en <- Op(oh_lc)
prices_lag <- rutils::lag_xts(cl_ose)
position_lagged <- rutils::lag_xts(po_sitions)
# calculate daily profits and losses
re_turns <- position_lagged*(cl_ose - prices_lag)
re_turns[trade_dates] <-
  position_lagged[trade_dates] *
  (op_en[trade_dates] - prices_lag[trade_dates]) +
  po_sitions[trade_dates] *
  (cl_ose[trade_dates] - op_en[trade_dates])
# calculate annualized Sharpe ratio of strategy returns
sqrt(260)*sum(re_turns)/sd(re_turns)/NROW(re_turns)
pn_l <- cumsum(re_turns)
pn_l <- cbind(cl_ose-as.numeric(cl_ose[1, ]), pn_l)
colnames(pn_l) <- c("VTI", "EWMA PnL")
library(HighFreq)  # load package HighFreq
# plot EWMA PnL with position shading
chart_Series(pn_l, theme=plot_theme,
       name="Performance of EWMA Strategy")
add_TA(po_sitions > 0, on=-1,
 col="lightgreen", border="lightgreen")
add_TA(po_sitions < 0, on=-1,
 col="lightgrey", border="lightgrey")
legend("top", legend=colnames(pn_l),
 inset=0.05, bg="white", lty=c(1, 1), lwd=c(2, 2),
 col=plot_theme$col$line.col, bty="n")
library(HighFreq)  # load package HighFreq
simu_ewma <- function(oh_lc, lamb_da=0.05, win_dow=51) {
  # calculate EWMA prices
  weight_s <- exp(-lamb_da*1:win_dow)
  weight_s <- weight_s/sum(weight_s)
  cl_ose <- Cl(oh_lc)
  ew_ma <- filter(cl_ose, filter=weight_s, sides=1)
  ew_ma[1:(win_dow-1)] <- ew_ma[win_dow]
  # determine dates right after EWMA has crossed prices
  in_dic <- xts(sign(as.numeric(cl_ose) - ew_ma), order.by=index(oh_lc))
  trade_dates <- (rutils::diff_xts(in_dic) != 0)
  trade_dates <- which(trade_dates) + 1
  # calculate positions, either: -1, 0, or 1
  po_sitions <- rep(NA_integer_, NROW(cl_ose))
  po_sitions[1] <- 0
  po_sitions[trade_dates] <- rutils::lag_xts(in_dic)[trade_dates]
  po_sitions <- xts(na.locf(po_sitions), order.by=index(oh_lc))
  op_en <- Op(oh_lc)
  prices_lag <- rutils::lag_xts(cl_ose)
  position_lagged <- rutils::lag_xts(po_sitions)
  # calculate daily profits and losses
  re_turns <- position_lagged*(cl_ose - prices_lag)
  re_turns[trade_dates] <-
    position_lagged[trade_dates] *
    (op_en[trade_dates] - prices_lag[trade_dates]) +
    po_sitions[trade_dates] *
    (cl_ose[trade_dates] - op_en[trade_dates])
  cbind(po_sitions, re_turns)
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
po_sitions <- ewma_trend[, 1]
pn_l <- cumsum(ewma_trend[, 2])
pn_l <- cbind(cl_ose-as.numeric(cl_ose[1, ]),
        pn_l)
colnames(pn_l) <- c("VTI", "EWMA PnL")
# plot EWMA PnL with position shading
chart_Series(pn_l, theme=plot_theme,
       name="Performance of EWMA Strategy")
add_TA(po_sitions > 0, on=-1,
 col="lightgreen", border="lightgreen")
add_TA(po_sitions < 0, on=-1,
 col="lightgrey", border="lightgrey")
legend("top", legend=colnames(pn_l),
 inset=0.05, bg="white", lty=c(1, 1), lwd=c(2, 2),
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
po_sitions <- ewma_revert[, 1]
pn_l <- cumsum(ewma_revert[, 2])
pn_l <- cbind(cl_ose-as.numeric(cl_ose[1, ]),
        pn_l)
colnames(pn_l) <- c("VTI", "EWMA PnL")
# plot EWMA PnL with position shading
chart_Series(pn_l, theme=plot_theme,
       name="Performance of EWMA Strategy")
add_TA(po_sitions > 0, on=-1,
 col="lightgreen", border="lightgreen")
add_TA(po_sitions < 0, on=-1,
 col="lightgrey", border="lightgrey")
legend("top", legend=colnames(pn_l),
 inset=0.05, bg="white", lty=c(1, 1), lwd=c(2, 2),
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
pn_l <- cumsum(re_turns)
pn_l <- cbind(cl_ose-as.numeric(cl_ose[1, ]),
        pn_l)
colnames(pn_l) <- c("VTI", "EWMA combined PnL")
chart_Series(pn_l, theme=plot_theme,
       name="Performance of EWMA Strategies")
add_TA(cumsum(trend_ing), on=1, lwd=2, col="green")
add_TA(cumsum(revert_ing), on=1, lwd=2, col="magenta2")
legend("topleft", legend=c(colnames(pn_l), "trending", "reverting"),
 inset=0.05, bg="white", lty=rep(1, 4), lwd=rep(4, 4),
 col=c(plot_theme$col$line.col, "green", "magenta2"), bty="n")
lamb_das <- seq(0.001, 0.03, 0.001)
re_turns <- lapply(lamb_das, function(lamb_da) {
  simu_ewma(oh_lc=oh_lc, lamb_da=lamb_da)[, 2]
})  # end sapply
re_turns <- do.call(cbind, re_turns)
weight_s <- sharpe_ratios/sum(sharpe_ratios)
re_turns <- re_turns %*% weight_s
re_turns <- xts(re_turns, order.by=index(oh_lc))
pn_l <- cumsum(re_turns)
pn_l <- cbind(cl_ose-as.numeric(cl_ose[1, ]),
        pn_l)
colnames(pn_l) <- c("VTI", "EWMA PnL")
# plot EWMA PnL without position shading
chart_Series(pn_l, theme=plot_theme,
       name="Performance of EWMA Strategy")
legend("top", legend=colnames(pn_l),
 inset=0.05, bg="white", lty=c(1, 1), lwd=c(2, 2),
 col=plot_theme$col$line.col, bty="n")
inputPanel(
  sliderInput("lamb_da", label="lambda:",
        min=0.01, max=0.2, value=0.1, step=0.01)
)  # end inputPanel

renderPlot({
  lamb_da <- input$lamb_da
  # calculate EWMA prices
  weight_s <- exp(-lamb_da*1:win_dow)
  weight_s <- weight_s/sum(weight_s)
  ew_ma <- filter(cl_ose, filter=weight_s, sides=1)
  ew_ma[1:(win_dow-1)] <- ew_ma[win_dow]
  ew_ma <- xts(cbind(cl_ose, ew_ma), order.by=index(oh_lc))
  colnames(ew_ma) <- c("VTI", "VTI EWMA")
  # plot EWMA prices
  ch_ob <- chart_Series(ew_ma, theme=plot_theme, name="EWMA prices")
  plot(ch_ob)
  legend("top", legend=colnames(ew_ma),
   inset=0.1, bg="white", lty=c(1, 1), lwd=c(2, 2),
   col=plot_theme$col$line.col, bty="n")
})  # end renderPlot
# library(HighFreq)  # load package HighFreq
library(HighFreq)  # load package HighFreq
price_s <- SPY["2012-02-13", 4]  # extract closing minutely prices
end_points <- 0:NROW(price_s)  # define end points
len_gth <- length(end_points)
win_dow <- 11  # number of data points per look-back window
# start_points are multi-period lag of end_points
start_points <-  end_points[
  c(rep_len(1, win_dow-1), 1:(len_gth-win_dow+1))]
# define aggregation function
agg_regate <- function(x_ts)
  c(max=max(x_ts), min=min(x_ts))
# perform aggregations over length of end_points
agg_regations <- sapply(2:len_gth,
    function(in_dex) {
agg_regate(price_s[start_points[in_dex]:end_points[in_dex]])
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
  c(rep_len(1, win_dow-1), 1:(len_gth-win_dow+1))]
# define aggregation function
agg_regate <- function(x_ts)
  xts(t(c(max=max(x_ts), min=min(x_ts))),
order.by=end(x_ts))
# perform aggregations over length of end_points
agg_regations <- lapply(2:len_gth,
    function(in_dex) {
agg_regate(price_s[start_points[in_dex]:end_points[in_dex]])
  })  # end lapply
# rbind list into single xts or matrix
agg_regations <- rutils::do_call_rbind(agg_regations)
agg_regations <- merge(price_s, agg_regations)
# plot aggregations with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("black", "red", "green")
chart_Series(agg_regations, theme=plot_theme,
       name="price aggregations")
legend("top", legend=colnames(agg_regations),
bg="white", lty=c(1, 1), lwd=c(2, 2),
col=plot_theme$col$line.col, bty="n")
# library(HighFreq)  # load package HighFreq
# define functional for rolling aggregations
roll_agg <- function(x_ts, win_dow, FUN, ...) {
# define end points at every point
  end_points <- 0:NROW(x_ts)
  len_gth <- length(end_points)
# define starting points as lag of end_points
  start_points <-  end_points[
    c(rep_len(1, win_dow-1), 1:(len_gth-win_dow+1))]
# perform aggregations over length of end_points
  agg_regations <- lapply(2:len_gth,
    function(in_dex) {
FUN(x_ts[start_points[in_dex]:end_points[in_dex]], ...)
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
  agg_vector=roll_agg(price_s, win_dow=win_dow,
              FUN=agg_vector),
  agg_xts=roll_agg(price_s, win_dow=win_dow,
              FUN=agg_xts),
  times=10))[, c(1, 4, 5)]
# library(HighFreq)  # load package HighFreq
# define aggregation function that returns a single value
agg_regate <- function(x_ts)  max(x_ts)
# perform aggregations over a rolling window
agg_regations <- xts:::rollapply.xts(price_s, width=win_dow,
              FUN=agg_regate, align="right")
# perform aggregations over a rolling window
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
# library(HighFreq)  # load package HighFreq
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
sum(price_s[start_points[in_dex]:end_points[in_dex]])
  })  # end sapply
head(agg_regations)
tail(agg_regations)
# benchmark the speed of both methods
library(microbenchmark)
summary(microbenchmark(
  roll_sum=roll_sum(price_s, win_dow=win_dow),
  s_apply=sapply(2:len_gth,
    function(in_dex) {
sum(price_s[start_points[in_dex]:end_points[in_dex]])
  }),
  times=10))[, c(1, 4, 5)]
# library(TTR)  # load package TTR
# benchmark the speed of TTR::runSum
library(microbenchmark)
summary(microbenchmark(
  cum_sum=cumsum(coredata(price_s)),
  roll_sum=rutils::roll_sum(price_s, win_dow=win_dow),
  run_sum=TTR::runSum(price_s, n=win_dow),
  times=10))[, c(1, 4, 5)]
library(RcppRoll)  # load package RcppRoll
win_dow <- 11  # number of data points per look-back window
# calculate rolling sum using rutils
prices_mean <-
  rutils::roll_sum(price_s, win_dow=win_dow)
# calculate rolling sum using RcppRoll
prices_mean <- RcppRoll::roll_sum(price_s,
              align="right", n=win_dow)
# benchmark the speed of RcppRoll::roll_sum
library(microbenchmark)
summary(microbenchmark(
  cum_sum=cumsum(coredata(price_s)),
  rcpp_roll_sum=RcppRoll::roll_sum(price_s, n=win_dow),
  roll_sum=rutils::roll_sum(price_s, win_dow=win_dow),
  times=10))[, c(1, 4, 5)]
# calculate EWMA sum using RcppRoll
weight_s <- exp(0.1*1:win_dow)
prices_mean <- RcppRoll::roll_mean(price_s,
align="right", n=win_dow, weights=weight_s)
prices_mean <- merge(price_s,
  rbind(coredata(price_s[1:(win_dow-1), ]), prices_mean))
colnames(prices_mean) <- c("SPY", "SPY EWMA")
# plot EWMA prices with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("black", "red")
x11()
chart_Series(prices_mean, theme=plot_theme,
       name="EWMA prices")
legend("top", legend=colnames(prices_mean),
 bg="white", lty=c(1, 1), lwd=c(2, 2),
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
win_dow <- 11
price_s <- Cl(SPY["2012-02-01/2012-04-01"])
med_ian <- runmed(x=price_s, k=win_dow)
# vector of rolling volatility
vol_at <- runsd(x=price_s, k=win_dow,
          endrule="constant", align="center")
# vector of rolling quantiles
quan_tiles <- runquantile(x=price_s,
            k=win_dow, probs=0.9,
            endrule="constant",
            align="center")
library(HighFreq)  # load package HighFreq
# extract a single day of minutely price data
price_s <- Cl(SPY["2012-02-13"])
# define number of data points per interval
win_dow <- 11
# number of win_dows that fit over price_s
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
abline(v=end_points, col="red")
# library(HighFreq)  # load package HighFreq
# indices of last observations in each hour
end_points <- endpoints(price_s, on="hours")
head(end_points)
# extract the last observations in each hour
head(price_s[end_points, ])
# library(HighFreq)  # load package HighFreq
end_points <- # define end_points with beginning stub
  c(0, n_row-win_dow*num_agg+win_dow*(0:num_agg))
len_gth <- length(end_points)
# start_points are single-period lag of end_points
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
# library(HighFreq)  # load package HighFreq
end_points <- # define end_points with beginning stub
  c(0, n_row-win_dow*num_agg+win_dow*(0:num_agg))
len_gth <- length(end_points)
# start_points are single-period lag of end_points
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
# library(HighFreq)  # load package HighFreq
# define functional for rolling aggregations over end_points
roll_agg <- function(x_ts, end_points, FUN, ...) {
  len_gth <- length(end_points)
# start_points are single-period lag of end_points
  start_points <- end_points[c(1, 1:(len_gth-1))] + 1
# perform aggregations over length of end_points
  agg_regations <- lapply(2:len_gth,
    function(in_dex) {
FUN(x_ts[start_points[in_dex]:end_points[in_dex]], ...)
    })  # end lapply
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
price_s <- Cl(SPY["2012-02-01/2012-04-01"])
# apply "mean" over daily periods
agg_regations <- apply.daily(price_s, FUN=sum)
head(agg_regations)
library(HighFreq)  # load package HighFreq
end_points <- # define end_points with beginning stub
  c(0, n_row-win_dow*num_agg+win_dow*(0:num_agg))
len_gth <- length(end_points)
win_dow <- 4  # number of end points per look-back window
# start_points are multi-period lag of end_points
start_points <-  end_points[
  c(rep_len(1, win_dow-1), 1:(len_gth-win_dow+1))]
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
# library(HighFreq)  # load package HighFreq
library(HighFreq)  # load package HighFreq
end_points <- # define end_points with beginning stub
  c(0, n_row-win_dow*num_agg+win_dow*(0:num_agg))
len_gth <- length(end_points)
win_dow <- 4  # number of end points per look-back window
# start_points are multi-period lag of end_points
start_points <-  end_points[
  c(rep_len(1, win_dow-1), 1:(len_gth-win_dow+1))]
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
# perform monthly mean aggregation
zoo_agg <- aggregate(zoo_series, by=dates_agg,
               FUN=mean)
# merge with original zoo - union of dates
zoo_agg <- merge(zoo_series, zoo_agg)
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
