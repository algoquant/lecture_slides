oh_lc <- rutils::etf_env$VTI
# Number of data points
n_rows <- NROW(oh_lc["2018-06/"])
# Define end_points at each point in time
end_points <- 1:n_rows
# Number of data points in look_back interval
look_back <- 22
# start_points are end_points lagged by look_back
start_points <- c(rep_len(1, look_back-1),
    end_points[1:(n_rows-look_back+1)])
# Or
start_points <- (end_points-look_back+1)
start_points <- ifelse(start_points > 0,
  start_points, 1)
# Number of data points
price_s <- Cl(oh_lc["2018/"])
n_rows <- NROW(price_s)
# Number of periods between endpoints
n_points <- 22
# Number of n_points that fit over n_rows
n_agg <- n_rows %/% n_points
# if n_rows==n_points*n_agg then whole number
end_points <- (1:n_agg)*n_points
# else stub interval at beginning
end_points <-
  n_rows-n_points*n_agg + (0:n_agg)*n_points
# else stub interval at end
end_points <- c((1:n_agg)*n_points, n_rows)
# Or use xts::endpoints()
end_points <- xts::endpoints(price_s, on="months")
# Plot data and endpoints as vertical lines
plot.xts(price_s, col="blue", lwd=2, xlab="", ylab="",
   main="Prices with Endpoints as Vertical Lines")
addEventLines(xts(rep("endpoint", NROW(end_points)), index(price_s)[end_points]),
        col="red", lwd=2, pos=4)
# Or
plot_theme <- chart_theme()
plot_theme$col$line.col <- "blue"
chart_Series(price_s, theme=plot_theme,
  name="prices with endpoints as vertical lines")
abline(v=end_points, col="red", lwd=2)
# Number of data points
n_rows <- NROW(rutils::etf_env$VTI["2017/"])
# Number of n_points that fit over n_rows
n_points <- 22
n_agg <- n_rows %/% n_points
# end_points with stub interval at beginning
end_points <-
  n_rows-n_points*n_agg + (0:n_agg)*n_points
# look_back defined as number of data points
look_back <- 252
# start_points are end_points lagged by look_back
start_points <- (end_points-look_back+1)
start_points <- ifelse(start_points > 0,
  start_points, 1)
cbind(start_points, end_points)
# look_back defined as number of end_points
look_back <- 12
start_points <- c(rep_len(1, look_back-1),
    end_points[1:(NROW(end_points)-look_back+1)])
# Number of data points
n_rows <- NROW(rutils::etf_env$VTI["2017/"])
# Number of data points per interval
look_back <- 22
# Number of look_backs that fit over n_rows
n_agg <- n_rows %/% look_back
# Define end_points with beginning stub
end_points <-
  n_rows-look_back*n_agg + (0:n_agg)*look_back
# Define contiguous start_points
start_points <- c(1, end_points[1:(NROW(end_points)-1)])
# Define exclusive start_points
start_points <- c(1, end_points[1:(NROW(end_points)-1)]+1)
price_s <- Cl(rutils::etf_env$VTI)
end_points <- seq_along(price_s)  # Define end points
n_rows <- NROW(end_points)
look_back <- 22  # number of data points per look-back interval
# start_points are multi-period lag of end_points
start_points <- c(rep_len(1, look_back-1),
    end_points[1:(n_rows-look_back+1)])
# Define list of look-back intervals for aggregations over past
look_backs <- lapply(seq_along(end_points),
  function(in_dex) {
    start_points[in_dex]:end_points[in_dex]
})  # end lapply
# Define aggregation function
agg_regate <- function(x_ts) c(max=max(x_ts), min=min(x_ts))
# Perform aggregations over look_backs list
agg_regations <- sapply(look_backs,
    function(look_back) agg_regate(price_s[look_back])
)  # end sapply
# Coerce agg_regations into matrix and transpose it
if (is.vector(agg_regations))
  agg_regations <- t(agg_regations)
agg_regations <- t(agg_regations)
# Coerce agg_regations into xts series
agg_regations <- xts(agg_regations,
               order.by=index(price_s[end_points]))
library(HighFreq)  # load package HighFreq
# Perform aggregations over look_backs list
agg_regations <- lapply(look_backs,
    function(look_back) agg_regate(price_s[look_back])
)  # end lapply
# rbind list into single xts or matrix
agg_regations <- rutils::do_call_rbind(agg_regations)
# Convert into xts
agg_regations <- xts::xts(agg_regations,
    order.by=index(price_s))
agg_regations <- cbind(agg_regations, price_s)
# Plot aggregations with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("black", "red", "green")
x11(width=6, height=5)
chart_Series(agg_regations, theme=plot_theme,
       name="price aggregations")
legend("top", legend=colnames(agg_regations),
  bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")
# library(HighFreq)  # load package HighFreq
# Define functional for rolling aggregations
roll_agg <- function(x_ts, look_back, FUN, ...) {
# Define end points at every period
  end_points <- seq_along(x_ts)
  n_rows <- NROW(end_points)
# Define starting points as lag of end_points
  start_points <- c(rep_len(1, look_back-1),
    end_points[1:(n_rows-look_back+1)])
# Define list of look-back intervals for aggregations over past
look_backs <- lapply(seq_along(end_points),
  function(in_dex) {
    start_points[in_dex]:end_points[in_dex]
})  # end lapply
# Perform aggregations over look_backs list
  agg_regations <- lapply(look_backs,
    function(look_back) FUN(x_ts[look_back], ...)
  )  # end lapply
# rbind list into single xts or matrix
  agg_regations <- rutils::do_call_rbind(agg_regations)
# Coerce agg_regations into xts series
  if (!is.xts(agg_regations))
    agg_regations <- xts(agg_regations, order.by=index(x_ts))
  agg_regations
}  # end roll_agg
# Define aggregation function
agg_regate <- function(x_ts)
  c(max=max(x_ts), min=min(x_ts))
# Perform aggregations over rolling interval
agg_regations <- roll_agg(price_s, look_back=look_back,
              FUN=agg_regate)
class(agg_regations)
dim(agg_regations)
# library(HighFreq)  # load package HighFreq
# Define aggregation function that returns a vector
agg_vector <- function(x_ts)
  c(max=max(x_ts), min=min(x_ts))
# Define aggregation function that returns an xts
agg_xts <- function(x_ts)
  xts(t(c(max=max(x_ts), min=min(x_ts))),
order.by=end(x_ts))
# Benchmark the speed of aggregation functions
library(microbenchmark)
summary(microbenchmark(
  agg_vector=roll_agg(price_s, look_back=look_back,
              FUN=agg_vector),
  agg_xts=roll_agg(price_s, look_back=look_back,
              FUN=agg_xts),
  times=10))[, c(1, 4, 5)]
# library(HighFreq)  # load package HighFreq
# Define aggregation function that returns a single value
agg_regate <- function(x_ts)  max(x_ts)
# Perform aggregations over a rolling interval
agg_regations <- xts:::rollapply.xts(price_s, width=look_back,
              FUN=agg_regate, align="right")
# Perform aggregations over a rolling interval
library(PerformanceAnalytics)  # load package PerformanceAnalytics
agg_regations <- apply.rolling(price_s,
              width=look_back, FUN=agg_regate)
# Benchmark the speed of the functionals
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
# Define list of look-back intervals for aggregations over past
look_backs <- lapply(seq_along(end_points),
  function(in_dex) {
    start_points[in_dex]:end_points[in_dex]
})  # end lapply
# Perform rolling aggregations using apply loop
agg_regations <- sapply(look_backs,
    function(look_back) sum(price_s[look_back])
)  # end sapply
head(agg_regations)
tail(agg_regations)
# Benchmark the speed of both methods
library(microbenchmark)
summary(microbenchmark(
  roll_sum=roll_sum(price_s, look_back=look_back),
  s_apply=sapply(look_backs,
    function(look_back) sum(price_s[look_back])),
  times=10))[, c(1, 4, 5)]
# Calculate the rolling maximum and minimum over a vector of data
roll_maxminr <- function(vec_tor, look_back) {
  n_rows <- NROW(vec_tor)
  max_min <- matrix(numeric(2*n_rows), nc=2)
  # loop over periods
  for (it in 1:n_rows) {
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
# Benchmark the speed of TTR::runMax
library(microbenchmark)
summary(microbenchmark(
  pure_r=roll_maxminr(price_s, look_back),
  ttr=TTR::runMax(price_s, n=look_back),
  times=10))[, c(1, 4, 5)]
# Benchmark the speed of TTR::runSum
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
# Benchmark the speed of TTR::runMax
library(microbenchmark)
summary(microbenchmark(
  arma=roll_maxmin(price_s, look_back),
  ttr=TTR::runMax(price_s, n=look_back),
  times=10))[, c(1, 4, 5)]
# Dygraphs plot with max_min lines
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
library(rutils)
# Calculate rolling VTI variance using package roll
library(roll)  # load roll
re_turns <- na.omit(rutils::etf_env$re_turns[, "VTI"])
look_back <- 22
# Calculate rolling sum using RcppRoll
sum_roll <- roll::roll_sum(re_turns, width=look_back)
# Calculate rolling sum using rutils
sum_rutils <- rutils::roll_sum(re_turns, look_back=look_back)
all.equal(sum_roll[-(1:look_back), ], sum_rutils[-(1:look_back), ], check.attributes=FALSE)
# Benchmark speed of rolling calculations
library(microbenchmark)
summary(microbenchmark(
  cum_sum=cumsum(re_turns),
  roll=roll::roll_sum(re_turns, width=look_back),
  RcppRoll=RcppRoll::roll_sum(re_turns, n=look_back),
  rutils=rutils::roll_sum(re_turns, look_back=look_back),
  times=10))[, c(1, 4, 5)]
library(RcppRoll)  # load package RcppRoll
# Calculate rolling sum using RcppRoll
sum_roll <- RcppRoll::roll_sum(re_turns, align="right", n=look_back)
# Calculate rolling sum using rutils
sum_rutils <- rutils::roll_sum(re_turns, look_back=look_back)
all.equal(sum_roll, coredata(sum_rutils[-(1:(look_back-1))]), check.attributes=FALSE)
# Benchmark speed of rolling calculations
library(microbenchmark)
summary(microbenchmark(
  cum_sum=cumsum(re_turns),
  RcppRoll=RcppRoll::roll_sum(re_turns, n=look_back),
  rutils=rutils::roll_sum(re_turns, look_back=look_back),
  times=10))[, c(1, 4, 5)]
# Calculate EWMA prices using RcppRoll
price_s <- na.omit(rutils::etf_env$VTI[, 4])
weight_s <- exp(0.1*1:look_back)
prices_ewma <- RcppRoll::roll_mean(price_s,
align="right", n=look_back, weights=weight_s)
prices_ewma <- cbind(price_s,
  rbind(coredata(price_s[1:(look_back-1), ]), prices_ewma))
colnames(prices_ewma) <- c("VTI", "VTI EWMA")
# Plot an interactive dygraph plot
dygraphs::dygraph(prices_ewma)
# Or static plot of EWMA prices with custom line colors
x11(width=6, height=5)
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("black", "red")
chart_Series(prices_ewma, theme=plot_theme,
       name="EWMA prices")
legend("top", legend=colnames(prices_ewma),
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
look_back <- 2
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
# indices of last observations in each hour
end_points <- xts::endpoints(price_s, on="hours")
head(end_points)
# extract the last observations in each hour
head(price_s[end_points, ])
price_s <- Cl(rutils::etf_env$VTI)
# Number of data points
n_rows <- NROW(price_s)
# Number of data points per interval
look_back <- 22
# Number of look_backs that fit over n_rows
n_agg <- n_rows %/% look_back
# Define end_points with beginning stub
end_points <-
  n_rows-look_back*n_agg + (0:n_agg)*look_back
# Define contiguous start_points
start_points <- c(1, end_points[1:(NROW(end_points)-1)])
# Define list of look-back intervals for aggregations over past
look_backs <- lapply(seq_along(end_points),
  function(in_dex) {
    start_points[in_dex]:end_points[in_dex]
})  # end lapply
look_backs[[1]]
look_backs[[2]]
# Perform sapply() loop over look_backs list
agg_regations <- sapply(look_backs,
    function(look_back) {
x_ts <- price_s[look_back]
c(max=max(x_ts), min=min(x_ts))
  })  # end sapply
# Coerce agg_regations into matrix and transpose it
if (is.vector(agg_regations))
  agg_regations <- t(agg_regations)
agg_regations <- t(agg_regations)
# Coerce agg_regations into xts series
agg_regations <- xts(agg_regations,
    order.by=index(price_s[end_points]))
head(agg_regations)
# Plot aggregations with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("red", "green")
chart_Series(agg_regations, theme=plot_theme,
       name="price aggregations")
legend("top", legend=colnames(agg_regations),
  bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")
# library(HighFreq)  # load package HighFreq
# Perform lapply() loop over look_backs list
agg_regations <- lapply(look_backs,
    function(look_back) {
x_ts <- price_s[look_back]
c(max=max(x_ts), min=min(x_ts))
    })  # end lapply
# rbind list into single xts or matrix
agg_regations <- rutils::do_call_rbind(agg_regations)
# Coerce agg_regations into xts series
agg_regations <- xts(agg_regations,
    order.by=index(price_s[end_points]))
head(agg_regations)
# Plot aggregations with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("red", "green")
chart_Series(agg_regations, theme=plot_theme,
       name="price aggregations")
legend("top", legend=colnames(agg_regations),
  bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")
# library(HighFreq)  # load package HighFreq
# Define functional for rolling aggregations over end_points
roll_agg <- function(x_ts, end_points, FUN, ...) {
  n_rows <- NROW(end_points)
# start_points are single-period lag of end_points
  start_points <- c(1, end_points[1:(n_rows-1)])
# Perform aggregations over look_backs list
  agg_regations <- lapply(look_backs,
    function(look_back) FUN(x_ts[look_back], ...))  # end lapply
# rbind list into single xts or matrix
  agg_regations <- rutils::do_call_rbind(agg_regations)
  if (!is.xts(agg_regations))
    agg_regations <-  # Coerce agg_regations into xts series
    xts(agg_regations, order.by=index(x_ts[end_points]))
  agg_regations
}  # end roll_agg
# Apply sum() over end_points
agg_regations <-
  roll_agg(price_s, end_points=end_points, FUN=sum)
agg_regations <-
  period.apply(price_s, INDEX=end_points, FUN=sum)
# Benchmark the speed of aggregation functions
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
# Apply "mean" over daily periods
agg_regations <- apply.daily(price_s, FUN=sum)
head(agg_regations)
# Number of n_points that fit over n_rows
n_points <- 22
n_agg <- n_rows %/% n_points
# end_points with stub interval at beginning
end_points <- # Define end_points with beginning stub
  n_rows-n_points*n_agg + (0:n_agg)*n_points
# Number of data points in look_back interval
look_back <- 252
# start_points are end_points lagged by look_back
start_points <- (end_points-look_back+1)
start_points <- ifelse(start_points > 0,
  start_points, 1)
# Define list of look-back intervals for aggregations over past
look_backs <- lapply(seq_along(end_points),
  function(in_dex) {
    start_points[in_dex]:end_points[in_dex]
})  # end lapply
# Perform lapply() loop over look_backs list
agg_regations <- lapply(look_backs,
    function(look_back) {
x_ts <- price_s[look_back]
c(max=max(x_ts), min=min(x_ts))
    })  # end lapply
# rbind list into single xts or matrix
agg_regations <- rutils::do_call_rbind(agg_regations)
# Coerce agg_regations into xts series
agg_regations <- xts(agg_regations,
    order.by=index(price_s[end_points]))
# Plot aggregations with custom line colors
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
# Plot aggregations with custom line colors
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
# Create zoo time series of random returns
in_dex <- Sys.Date() + 0:365
zoo_series <-
  zoo(rnorm(NROW(in_dex)), order.by=in_dex)
# Create monthly dates
dates_agg <- as.Date(as.yearmon(index(zoo_series)))
# Perform monthly mean aggregation
zoo_agg <- aggregate(zoo_series, by=dates_agg,
               FUN=mean)
# merge with original zoo - union of dates
zoo_agg <- cbind(zoo_series, zoo_agg)
# replace NA's using locf
zoo_agg <- na.locf(zoo_agg, na.rm=FALSE)
# extract aggregated zoo
zoo_agg <- zoo_agg[index(zoo_series), 2]
# library(HighFreq)  # load package HighFreq
# Plot original and aggregated cumulative returns
plot(cumsum(zoo_series), xlab="", ylab="")
lines(cumsum(zoo_agg), lwd=2, col="red")
# Add legend
legend("topright", inset=0.05, cex=0.8, bty="n",
 title="Aggregated Prices",
 leg=c("orig prices", "agg prices"),
 lwd=2, bg="white", col=c("black", "red"))
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# Perform monthly mean aggregation
zoo_agg <- aggregate(zoo_series, by=dates_agg,
               FUN=mean)
# merge with original zoo - union of dates
zoo_agg <- cbind(zoo_series, zoo_agg)
# replace NA's using linear interpolation
zoo_agg <- na.approx(zoo_agg)
# extract interpolated zoo
zoo_agg <- zoo_agg[index(zoo_series), 2]
# Plot original and interpolated zoo
plot(cumsum(zoo_series), xlab="", ylab="")
lines(cumsum(zoo_agg), lwd=2, col="red")
# Add legend
legend("topright", inset=0.05, cex=0.8, title="Interpolated Prices",
 leg=c("orig prices", "interpol prices"), lwd=2, bg="white",
 col=c("black", "red"), bty="n")
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# "mean" aggregation over interval with width=11
zoo_mean <- rollapply(zoo_series, width=11,
                FUN=mean, align="right")
# merge with original zoo - union of dates
zoo_mean <- cbind(zoo_series, zoo_mean)
# replace NA's using na.locf
zoo_mean <- na.locf(zoo_mean, na.rm=FALSE, fromLast=TRUE)
# extract mean zoo
zoo_mean <- zoo_mean[index(zoo_series), 2]
# Plot original and interpolated zoo
plot(cumsum(zoo_series), xlab="", ylab="")
lines(cumsum(zoo_mean), lwd=2, col="red")
# Add legend
legend("topright", inset=0.05, cex=0.8, title="Mean Prices",
 leg=c("orig prices", "mean prices"), lwd=2, bg="white",
 col=c("black", "red"), bty="n")
n_rows <- 1000
r_norm <- rnorm(n_rows)
sd(r_norm)
mad(r_norm)
median(abs(r_norm - median(r_norm)))
median(abs(r_norm - median(r_norm)))/qnorm(0.75)
# Bootstrap of sd and mad estimators
boot_strap <- sapply(1:10000, function(x) {
  boot_sample <-
    r_norm[sample.int(n_rows, replace=TRUE)]
  c(sd=sd(boot_sample), mad=mad(boot_sample))
})  # end sapply
boot_strap <- t(boot_strap)
# Analyze bootstrapped variance
head(boot_strap)
sum(is.na(boot_strap))
# Means and standard errors from bootstrap
apply(boot_strap, MARGIN=2,
function(x) c(mean=mean(x), std_error=sd(x)))
# Parallel bootstrap under Windows
library(parallel)  # load package parallel
n_cores <- detectCores() - 1  # number of cores
clus_ter <- makeCluster(n_cores)  # initialize compute cluster
boot_strap <- parLapply(clus_ter, 1:10000,
  function(x, r_norm) {
    boot_sample <-
r_norm[sample.int(n_rows, replace=TRUE)]
    c(sd=sd(boot_sample), mad=mad(boot_sample))
  }, r_norm=r_norm)  # end parLapply
# Parallel bootstrap under Mac-OSX or Linux
boot_strap <- mclapply(1:10000,
  function(x) {
    boot_sample <-
r_norm[sample.int(n_rows, replace=TRUE)]
    c(sd=sd(boot_sample), mad=mad(boot_sample))
  }, mc.cores=n_cores)  # end mclapply
stopCluster(clus_ter)  # Stop R processes over cluster
boot_strap <- rutils::do_call(rbind, boot_strap)
# Means and standard errors from bootstrap
apply(boot_strap, MARGIN=2,
function(x) c(mean=mean(x), std_error=sd(x)))
price_s <- rutils::etf_env$VTI[, 4]
# Define look-back window and a half window
win_dow <- 11
# Calculate time series of medians
media_n <- TTR::runMedian(price_s, n=win_dow)
# Plot prices and medians
dygraphs::dygraph(cbind(price_s, media_n), main="VTI median") %>%
  dyOptions(colors=c("black", "red"))
# Calculate time series of z-scores
ma_d <- TTR::runMAD(price_s, n=win_dow)
z_scores <- (price_s - media_n)/ma_d
z_scores[1:win_dow, ] <- 0
tail(z_scores, win_dow)
range(z_scores)
# Plot histogram of z-scores
x11(width=6, height=5)
histo_gram <- hist(z_scores, col="lightgrey",
  xlab="z-scores", breaks=50, xlim=c(-4, 4),
  ylab="frequency", freq=FALSE,
  main="Z-scores histogram")
lines(density(z_scores, adjust=1.5),
lwd=3, col="blue")
# Calculate one-sided Hampel z-scores
media_n <- TTR::runMedian(price_s, n=win_dow)
ma_d <- TTR::runMAD(price_s, n=win_dow)
z_scores <- (price_s - media_n)/ma_d
z_scores[1:win_dow, ] <- 0
tail(z_scores, win_dow)
range(z_scores)
# Calculate two-sided Hampel z-scores
half_window <- win_dow %/% 2
media_n <- rutils::lag_it(media_n, lagg=-half_window)
ma_d <- rutils::lag_it(ma_d, lagg=-half_window)
z_scores <- (price_s - media_n)/ma_d
z_scores[1:win_dow, ] <- 0
tail(z_scores, win_dow)
range(z_scores)
# Define threshold value
thresh_old <- 3
# Calculate number of prices classified as bad data
ba_d <- (abs(z_scores) > thresh_old)
sum(ba_d)
# Add 200 random price jumps into price_s
set.seed(1121)
n_bad <- 200
jump_s <- logical(NROW(price_s))
jump_s[sample(x=NROW(jump_s), size=n_bad)] <- TRUE
price_s[jump_s] <- price_s[jump_s]*
  sample(c(0.95, 1.05), size=n_bad, replace=TRUE)
# Plot prices and medians
dygraphs::dygraph(cbind(price_s, media_n), main="VTI median") %>%
  dyOptions(colors=c("black", "red"))
# Calculate time series of z-scores
media_n <- TTR::runMedian(price_s, n=win_dow)
ma_d <- TTR::runMAD(price_s, n=win_dow)
z_scores <- (price_s - media_n)/ma_d
z_scores[1:win_dow, ] <- 0
# Calculate number of prices classified as bad data
ba_d <- (abs(z_scores) > thresh_old)
sum(ba_d)
# Calculate confusion matrix
table(jump_s, ba_d)
sum(ba_d)
# FALSE positive (type I error)
sum(jump_s & !ba_d)
# FALSE negative (type II error)
sum(!jump_s & ba_d)
# Confusion matrix as function of thresh_old
con_fuse <- function(actu_al, z_scores, thresh_old) {
    confu_sion <- table(actu_al, (abs(z_scores) > thresh_old))
    confu_sion <- confu_sion / rowSums(confu_sion)
    c(typeI=confu_sion[2, 1], typeII=confu_sion[1, 2])
  }  # end con_fuse
con_fuse(jump_s, z_scores, thresh_old=thresh_old)
# Define vector of thresholds
threshold_s <- seq(from=0.2, to=5.0, by=0.2)
# Calculate error rates
error_rates <- sapply(threshold_s, con_fuse,
  actu_al=jump_s,
  z_scores=z_scores)  # end sapply
error_rates <- t(error_rates)
# Calculate area under ROC curve (AUC)
true_pos <- sort(1- c(error_rates[, "typeII"], 0))
true_pos <- (true_pos + rutils::lag_it(true_pos))/2
false_pos <- c(sort(error_rates[, "typeI"]), 1)
false_pos <- rutils::diff_it(false_pos)
sum(true_pos*false_pos)
# Plot ROC Curve for Defaults
plot(x=error_rates[, "typeI"],
     y=1-error_rates[, "typeII"],
     xlab="FALSE positive rate",
     ylab="TRUE positive rate",
     xlim=c(0, 1), ylim=c(0, 1),
     main="ROC Curve for Hampel Classifier",
     type="l", lwd=3, col="blue")
abline(a=0.0, b=1.0, lwd=3, col="orange")
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
