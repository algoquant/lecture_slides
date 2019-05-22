library(knitr)
opts_chunk$set(prompt=TRUE, eval=FALSE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size="scriptsize", fig.width=4, fig.height=4)
options(width=60, dev="pdf")
options(digits=3)
thm <- knit_theme$get("acid")
knit_theme$set(thm)

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
price_s <- quantmod::Cl(oh_lc["2018/"])
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

price_s <- quantmod::Cl(rutils::etf_env$VTI)
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

# Extract time series of VTI prices
price_s <- quantmod::Cl(rutils::etf_env$VTI)
# Calculate EWMA prices using filter()
look_back <- 21
weight_s <- exp(-0.1*1:look_back)
weight_s <- weight_s/sum(weight_s)
filter_ed <- stats::filter(price_s, filter=weight_s,
                   method="convolution", sides=1)
# filter() returns time series of class "ts"
class(filter_ed)
# Filter using compiled C++ function directly
getAnywhere(C_cfilter)
str(stats:::C_cfilter)
filter_fast <- .Call(stats:::C_cfilter, price_s, filter=weight_s, sides=1, circular=FALSE)
all.equal(as.numeric(filter_ed), filter_fast, check.attributes=FALSE)
# Calculate EWMA prices using roll::roll_sum()
weights_rev <- rev(weight_s)
roll_ed <- roll::roll_sum(price_s, width=look_back, weights=weights_rev)
all.equal(filter_ed[-(1:look_back)],
    as.numeric(roll_ed)[-(1:look_back)],
    check.attributes=FALSE)
# Benchmark speed of rolling calculations
library(microbenchmark)
summary(microbenchmark(
  filter=filter(price_s, filter=weight_s, method="convolution", sides=1),
  filter_fast=.Call(stats:::C_cfilter, price_s, filter=weight_s, sides=1, circular=FALSE),
  cum_sum=cumsum(price_s),
  roll=roll::roll_sum(price_s, width=look_back, weights=weights_rev)
  ), times=10)[, c(1, 4, 5)]

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
price_s <- quantmod::Cl(HighFreq::SPY["2012-02-01/2012-04-01"])
med_ian <- runmed(x=price_s, k=look_back)
# vector of rolling volatility
vol_at <- runsd(x=price_s, k=look_back,
          endrule="constant", align="center")
# vector of rolling quantiles
quan_tiles <- runquantile(x=price_s,
            k=look_back, probs=0.9,
            endrule="constant",
            align="center")

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

library(HighFreq)  # load package HighFreq
# indices of last observations in each hour
end_points <- xts::endpoints(price_s, on="hours")
head(end_points)
# extract the last observations in each hour
head(price_s[end_points, ])

price_s <- quantmod::Cl(rutils::etf_env$VTI)
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
price_s <- quantmod::Cl(HighFreq::SPY["2012-02-01/2012-04-01"])
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
con_fuse <- function(res_ponse, z_scores, thresh_old) {
    confu_sion <- table(res_ponse, (abs(z_scores) > thresh_old))
    confu_sion <- confu_sion / rowSums(confu_sion)
    c(typeI=confu_sion[2, 1], typeII=confu_sion[1, 2])
  }  # end con_fuse
con_fuse(jump_s, z_scores, thresh_old=thresh_old)
# Define vector of thresholds
threshold_s <- seq(from=0.2, to=5.0, by=0.2)
# Calculate error rates
error_rates <- sapply(threshold_s, con_fuse,
  res_ponse=jump_s,
  z_scores=z_scores)  # end sapply
error_rates <- t(error_rates)
# Calculate area under ROC curve (AUC)
true_pos <- (1 - c(0, error_rates[, "typeII"]))
true_pos <- (true_pos + rutils::lag_it(true_pos))/2
false_pos <- c(1, error_rates[, "typeI"])
false_pos <- rutils::diff_it(false_pos)
abs(sum(true_pos*false_pos))

# Plot ROC Curve for Defaults
plot(x=error_rates[, "typeI"],
     y=1-error_rates[, "typeII"],
     xlab="FALSE positive rate",
     ylab="TRUE positive rate",
     xlim=c(0, 1), ylim=c(0, 1),
     main="ROC Curve for Hampel Classifier",
     type="l", lwd=3, col="blue")
abline(a=0.0, b=1.0, lwd=3, col="orange")

# Calculate EWMA VTI variance using filter()
look_back <- 51
weight_s <- exp(-0.1*1:look_back)
weight_s <- weight_s/sum(weight_s)
vari_ance <- stats::filter(re_turns^2,
    filter=weight_s, sides=1)
vari_ance[1:(look_back-1)] <- vari_ance[look_back]
class(vari_ance)
vari_ance <- as.numeric(vari_ance)
x_ts <- xts:::xts(sqrt(vari_ance), order.by=index(re_turns))
# Plot EWMA standard deviation
chart_Series(x_ts,
  name="EWMA standard deviation")
dygraphs::dygraph(x_ts, main="EWMA standard deviation")

# Calculate EWMA VTI variance using filter()
look_back <- 51
weight_s <- exp(-0.1*1:look_back)
weight_s <- weight_s/sum(weight_s)
vari_ance <- stats::filter(re_turns^2,
    filter=weight_s, sides=1)
vari_ance[1:(look_back-1)] <- vari_ance[look_back]
class(vari_ance)
vari_ance <- as.numeric(vari_ance)
x_ts <- xts:::xts(sqrt(vari_ance), order.by=index(re_turns))
# Plot EWMA standard deviation
chart_Series(x_ts,
  name="EWMA standard deviation")
dygraphs::dygraph(x_ts, main="EWMA standard deviation")

# VTI percentage returns
re_turns <- rutils::diff_it(log(quantmod::Cl(rutils::etf_env$VTI)))
# number of observations
n_rows <- NROW(re_turns)
# mean of VTI returns
mean_rets <- mean(re_turns)
# standard deviation of VTI returns
sd_rets <- sd(re_turns)
# skew of VTI returns
n_rows/((n_rows-1)*(n_rows-2))*
  sum(((re_turns - mean_rets)/sd_rets)^3)
# kurtosis of VTI returns
n_rows*(n_rows+1)/((n_rows-1)^3)*
  sum(((re_turns - mean_rets)/sd_rets)^4)
# random normal returns
re_turns <- rnorm(n_rows, sd=sd_rets)
# mean and standard deviation of random normal returns
mean_rets <- mean(re_turns)
sd_rets <- sd(re_turns)
# skew of random normal returns
n_rows/((n_rows-1)*(n_rows-2))*
  sum(((re_turns - mean_rets)/sd_rets)^3)
# kurtosis of random normal returns
n_rows*(n_rows+1)/((n_rows-1)^3)*
  sum(((re_turns - mean_rets)/sd_rets)^4)

set.seed(1121)  # reset random number generator
# sample from Standard Normal Distribution
n_rows <- 1000
sam_ple <- rnorm(n_rows)
# sample mean
mean(sam_ple)
# sample standard deviation
sd(sam_ple)

x_var <- seq(-5, 7, length=100)
y_var <- dnorm(x_var, mean=1.0, sd=2.0)
plot(x_var, y_var, type="l", lty="solid",
     xlab="", ylab="")
title(main="Normal Density Function", line=0.5)
star_t <- 3; fin_ish <- 5  # set lower and upper bounds
# set polygon base
are_a <- ((x_var >= star_t) & (x_var <= fin_ish))
polygon(c(star_t, x_var[are_a], fin_ish),  # Draw polygon
  c(-1, y_var[are_a], -1), col="red")

par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
sig_mas <- c(0.5, 1, 1.5, 2)  # sigma values
# Create plot colors
col_ors <- c("red", "black", "blue", "green")
# Create legend labels
lab_els <- paste("sigma", sig_mas, sep="=")
for (in_dex in 1:4) {  # Plot four curves
curve(expr=dnorm(x, sd=sig_mas[in_dex]),
type="l", xlim=c(-4, 4),
xlab="", ylab="", lwd=2,
col=col_ors[in_dex],
add=as.logical(in_dex-1))
}  # end for
# Add title
title(main="Normal Distributions", line=0.5)
# Add legend
legend("topright", inset=0.05, title="Sigmas",
 lab_els, cex=0.8, lwd=2, lty=1, bty="n",
 col=col_ors)

x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
deg_free <- c(3, 6, 9)  # Df values
col_ors <- c("black", "red", "blue", "green")
lab_els <- c("normal", paste("df", deg_free, sep="="))
# Plot a Normal probability distribution
curve(expr=dnorm, xlim=c(-4, 4),
      xlab="", ylab="", lwd=2)
for (in_dex in 1:3) {  # Plot three t-distributions
curve(expr=dt(x, df=deg_free[in_dex]),
      xlab="", ylab="", lwd=2,
      col=col_ors[in_dex+1], add=TRUE)
}  # end for

# Add title
title(main="t-distributions", line=0.5)
# Add legend
legend("topright", inset=0.05, bty="n",
       title="Degrees\n of freedom", lab_els,
       cex=0.8, lwd=6, lty=1, col=col_ors)

# Mixture of two normal distributions with sd=1 and sd=2
n_rows <- 1e5
re_turns <- c(rnorm(n_rows/2), 2*rnorm(n_rows/2))
re_turns <- (re_turns-mean(re_turns))/sd(re_turns)
# Kurtosis of normal
kurt(rnorm(n_rows))
# Kurtosis of mixture
kurt(re_turns)
# Or
n_rows*sum(re_turns^4)/(n_rows-1)^2

x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
# Plot the distributions
plot(density(re_turns), xlab="", ylab="",
  main="Mixture of Normal Returns",
  xlim=c(-3, 3), type="l", lwd=3, col="red")
curve(expr=dnorm, lwd=2, col="blue", add=TRUE)
curve(expr=dt(x, df=3), lwd=2, col="green", add=TRUE)
# Add legend
legend("topright", inset=0.05, lty=1, lwd=6, bty="n",
  legend=c("Mixture", "Normal", "t-distribution"),
  col=c("red", "blue", "green"))

# Objective function is log-likelihood
object_ive <- function(pa_r, free_dom, sam_ple) {
  sum(
    -log(gamma((free_dom+1)/2) /
      (sqrt(pi*free_dom) * gamma(free_dom/2))) +
    log(pa_r[2]) +
    (free_dom+1)/2 * log(1 + ((sam_ple - pa_r[1])/
                    pa_r[2])^2/free_dom))
}  # end object_ive
# Demonstrate equivalence with log(dt())
object_ive(c(1, 0.5), 2, 2:5)
-sum(log(dt(x=(2:5-1)/0.5, df=2)/0.5))
# Simpler objective function
object_ive <- function(pa_r, free_dom, sam_ple) {
  -sum(log(dt(x=(sam_ple-pa_r[1])/pa_r[2],
      df=free_dom)/pa_r[2]))
}  # end object_ive

# VTI percentage returns
re_turns <- rutils::diff_it(log(quantmod::Cl(rutils::etf_env$VTI)))
# initial parameters
par_init <- c(mean=0, scale=0.01)
# fit distribution using optim()
optim_fit <- optim(par=par_init,
  fn=object_ive, # log-likelihood function
  sam_ple=re_turns,
  free_dom=2, # Degrees of freedom
  method="L-BFGS-B", # quasi-Newton method
  upper=c(1, 0.1), # upper constraint
  lower=c(-1, 1e-7)) # lower constraint
# optimal parameters
lo_cation <- optim_fit$par["mean"]
scal_e <- optim_fit$par["scale"]
# Fit VTI returns using MASS::fitdistr()
optim_fit <- MASS::fitdistr(re_turns,
  densfun="t", df=2)
optim_fit$estimate
optim_fit$sd
lo_cation <- optim_fit$estimate[1]
scal_e <- optim_fit$estimate[2]
summary(optim_fit)

x11(width=6, height=5)
# Plot histogram of VTI returns
histo_gram <- hist(re_turns, col="lightgrey",
  xlab="returns", breaks=100, xlim=c(-0.05, 0.05),
  ylab="frequency", freq=FALSE,
  main="VTI returns histogram")
lines(density(re_turns, adjust=1.5),
lwd=3, col="blue")
# Plot the Normal probability distribution
curve(expr=dnorm(x, mean=mean(re_turns),
  sd=sd(re_turns)), add=TRUE, lwd=3, col="green")
# Plot t-distribution function
curve(expr=dt((x-lo_cation)/scal_e, df=2)/scal_e,
type="l", lwd=3, col="red", add=TRUE)
# Add legend
legend("topright", inset=0.05, bty="n",
  leg=c("density", "t-distr", "normal"),
  lwd=6, lty=c(1, 1, 1),
  col=c("blue", "red", "green"))

x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
deg_free <- c(2, 5, 8, 11)  # Df values
# Create plot colors
col_ors <- c("red", "black", "blue", "green")
# Create legend labels
lab_els <- paste("df", deg_free, sep="=")
for (in_dex in 1:4) {  # Plot four curves
curve(expr=dchisq(x, df=deg_free[in_dex]),
      type="l", xlim=c(0, 20), ylim=c(0, 0.3),
      xlab="", ylab="", lwd=2,
      col=col_ors[in_dex],
      add=as.logical(in_dex-1))
}  # end for

# Add title
title(main="Chi-squared Distributions", line=0.5)
# Add legend
legend("topright", inset=0.05, bty="n",
       title="Degrees of freedom", lab_els,
       cex=0.8, lwd=6, lty=1,
       col=col_ors)

# KS test for normal distribution
ks.test(rnorm(100), pnorm)
# KS test for uniform distribution
ks.test(runif(100), pnorm)
# KS test for two similar normal distributions
ks.test(rnorm(100), rnorm(100, mean=0.1))
# KS test for two different normal distributions
ks.test(rnorm(100), rnorm(100, mean=1.0))
# Fit t-dist into VTI returns
re_turns <- rutils::diff_it(log(quantmod::Cl(rutils::etf_env$VTI)))
optim_fit <- MASS::fitdistr(re_turns, densfun="t", df=2)
lo_cation <- optim_fit$estimate[1]
scal_e <- optim_fit$estimate[2]
# Perform Kolmogorov-Smirnov test on VTI returns
sam_ple <- lo_cation + scal_e*rt(NROW(re_turns), df=2)
ks.test(as.numeric(re_turns), sam_ple)

# Observed frequencies from random normal data
histo_gram <- hist(rnorm(1e3, mean=0), breaks=100, plot=FALSE)
freq_o <- histo_gram$counts
# Theoretical frequencies
freq_t <- rutils::diff_it(pnorm(histo_gram$breaks))
# Perform Chi-squared test for normal data
chisq.test(x=freq_o, p=freq_t, rescale.p=TRUE, simulate.p.value=TRUE)
# return p-value
chisq_test <- chisq.test(x=freq_o, p=freq_t, rescale.p=TRUE, simulate.p.value=TRUE)
chisq_test$p.value
# Observed frequencies from shifted normal data
histo_gram <- hist(rnorm(1e3, mean=2), breaks=100, plot=FALSE)
freq_o <- histo_gram$counts/sum(histo_gram$counts)
# Theoretical frequencies
freq_t <- rutils::diff_it(pnorm(histo_gram$breaks))
# Perform Chi-squared test for shifted normal data
chisq.test(x=freq_o, p=freq_t, rescale.p=TRUE, simulate.p.value=TRUE)
# Calculate histogram of VTI returns
histo_gram <- hist(re_turns, breaks=100, plot=FALSE)
freq_o <- histo_gram$counts
# Calculate cumulative probabilities and then difference them
freq_t <- pt((histo_gram$breaks-lo_cation)/scal_e, df=2)
freq_t <- rutils::diff_it(freq_t)
# Perform Chi-squared test for VTI returns
chisq.test(x=freq_o, p=freq_t, rescale.p=TRUE, simulate.p.value=TRUE)

# VTI percentage returns
re_turns <- rutils::diff_it(log(quantmod::Cl(rutils::etf_env$VTI)))
# Define end points
end_points <- seq_along(re_turns)
n_rows <- NROW(end_points)
look_back <- 51
# start_points are multi-period lag of end_points
start_points <- c(rep_len(1, look_back-1),
    end_points[1:(n_rows-look_back+1)])
# Define list of look-back intervals for aggregations over past
look_backs <- lapply(seq_along(end_points),
  function(in_dex) {
    start_points[in_dex]:end_points[in_dex]
})  # end lapply
# Calculate realized VTI variance in sapply() loop
vari_ance <- sapply(look_backs,
  function(look_back) {
    ret_s <- re_turns[look_back]
    sum((ret_s - mean(ret_s))^2)
}) / (look_back-1)  # end sapply
tail(vari_ance)
class(vari_ance)
# Coerce vari_ance into xts
vari_ance <- xts(vari_ance, order.by=index(re_turns))
colnames(vari_ance) <- "VTI.variance"
head(vari_ance)

# Calculate rolling VTI variance using package roll
library(roll)  # load roll
vari_ance <-
  roll::roll_var(re_turns, width=look_back)
colnames(vari_ance) <- "VTI.variance"
head(vari_ance)
sum(is.na(vari_ance))
vari_ance[1:(look_back-1)] <- 0
# Benchmark calculation of rolling variance
library(microbenchmark)
summary(microbenchmark(
  roll_sapply=sapply(look_backs, function(look_back) {
    ret_s <- re_turns[look_back]
    sum((ret_s - mean(ret_s))^2)
  }),
  ro_ll=roll::roll_var(re_turns, width=look_back),
  times=10))[, c(1, 4, 5)]

# Calculate EWMA VTI variance using filter()
look_back <- 51
weight_s <- exp(-0.1*1:look_back)
weight_s <- weight_s/sum(weight_s)
vari_ance <- stats::filter(re_turns^2,
    filter=weight_s, sides=1)
vari_ance[1:(look_back-1)] <- vari_ance[look_back]
class(vari_ance)
vari_ance <- as.numeric(vari_ance)
x_ts <- xts:::xts(sqrt(vari_ance), order.by=index(re_turns))
# Plot EWMA standard deviation
chart_Series(x_ts,
  name="EWMA standard deviation")
dygraphs::dygraph(x_ts, main="EWMA standard deviation")

# Calculate rolling VTI variance using package roll
library(roll)  # load roll
vari_ance <- roll::roll_var(re_turns,
  weights=rev(weight_s), width=look_back)
colnames(vari_ance) <- "VTI.variance"
class(vari_ance)
head(vari_ance)
sum(is.na(vari_ance))
vari_ance[1:(look_back-1)] <- 0

x11(width=6, height=4)
par(mar=c(4, 3, 1, 1), oma=c(0, 0, 0, 0))
# VTI percentage returns
re_turns <- rutils::diff_it(log(quantmod::Cl(rutils::etf_env$VTI)))
# Calculate rolling VTI variance using package roll
look_back <- 22
vari_ance <-
  roll::roll_var(re_turns, width=look_back)
vari_ance[1:(look_back-1)] <- 0
colnames(vari_ance) <- "VTI.variance"
# number of look_backs that fit over re_turns
n_rows <- NROW(re_turns)
n_agg <- n_rows %/% look_back
end_points <- # Define end_points with beginning stub
  n_rows-look_back*n_agg + (0:n_agg)*look_back
n_rows <- NROW(end_points)
# subset vari_ance to end_points
vari_ance <- vari_ance[end_points]
# improved autocorrelation function
acf_plus(coredata(vari_ance), lag=10, main="")
title(main="acf of variance", line=-1)
# Partial autocorrelation
pacf(coredata(vari_ance), lag=10, main="", ylab=NA)
title(main="pacf of variance", line=-1)

# Define GARCH parameters
om_ega <- 0.01 ; al_pha <- 0.2
be_ta <- 0.79 ; n_rows <- 1000
re_turns <- numeric(n_rows)
vari_ance <- numeric(n_rows)
vari_ance[1] <- om_ega/(1-al_pha-be_ta)
re_turns[1] <- rnorm(1, sd=sqrt(vari_ance[1]))
# simulate GARCH model
set.seed(1121)  # reset random numbers
for (i in 2:n_rows) {
  re_turns[i] <- rnorm(n=1, sd=sqrt(vari_ance[i-1]))
  vari_ance[i] <- om_ega + al_pha*re_turns[i]^2 +
    be_ta*vari_ance[i-1]
}  # end for

x11(width=5, height=3.5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0))
# Plot GARCH cumulative returns
plot(cumsum(re_turns/100), t="l",
  lwd=2, col="blue", xlab="", ylab="",
  main="GARCH cumulative returns")
# Plot dygraphs GARCH standard deviation
date_s <- seq.Date(from=Sys.Date()-n_rows+1,
  to=Sys.Date(), length.out=n_rows)
x_ts <- xts:::xts(cumsum(re_turns/100), order.by=date_s)
dygraphs::dygraph(x_ts, main="GARCH cumulative returns")
# Plot GARCH standard deviation
plot(sqrt(vari_ance), t="l",
  col="blue", xlab="", ylab="",
  main="GARCH standard deviation")
# Plot dygraphsGARCH standard deviation
x_ts <- xts:::xts(sqrt(vari_ance), order.by=date_s)
dygraphs::dygraph(x_ts, main="GARCH standard deviation")

# Define GARCH parameters
om_ega <- 0.0001 ; al_pha <- 0.5
be_ta <- 0.1 ; n_rows <- 10000
re_turns <- numeric(n_rows)
vari_ance <- numeric(n_rows)
vari_ance[1] <- om_ega/(1-al_pha-be_ta)
re_turns[1] <- rnorm(1, sd=sqrt(vari_ance[1]))
# simulate GARCH model
set.seed(1121)  # reset random numbers
for (i in 2:n_rows) {
  re_turns[i] <- rnorm(n=1, sd=sqrt(vari_ance[i-1]))
  vari_ance[i] <- om_ega + al_pha*re_turns[i]^2 +
    be_ta*vari_ance[i-1]
}  # end for
# Calculate kurtosis of GARCH returns
moments::moment(re_turns, order=4) /
  moments::moment(re_turns, order=2)^2
# Perform Jarque-Bera test of normality
tseries::jarque.bera.test(re_turns)

# Plot histogram of GARCH returns
histo_gram <- hist(re_turns, col="lightgrey",
  xlab="returns", breaks=200, xlim=c(-0.05, 0.05),
  ylab="frequency", freq=FALSE,
  main="GARCH returns histogram")
lines(density(re_turns, adjust=1.5),
lwd=3, col="blue")
optim_fit <- MASS::fitdistr(re_turns,
  densfun="t", df=2, lower=c(-1, 1e-7))
lo_cation <- optim_fit$estimate[1]
scal_e <- optim_fit$estimate[2]
curve(expr=dt((x-lo_cation)/scal_e, df=2)/scal_e,
  type="l", xlab="", ylab="", lwd=3,
  col="red", add=TRUE)
legend("topright", inset=0.05, bty="n",
 leg=c("density", "t-distr w/ 2 dof"),
 lwd=6, lty=1,
 col=c("blue", "red"))

# use fixed notation instead of exponential notation
options(scipen=999)
library(fGarch)
# fit returns into GARCH
garch_fit <- fGarch::garchFit(data=re_turns)
# fitted GARCH parameters
round(garch_fit@fit$coef, 5)
# Actual GARCH parameters
round(c(mu=mean(re_turns), omega=om_ega,
  alpha=al_pha, beta=be_ta), 5)

# Plot GARCH fitted standard deviation
plot(sqrt(garch_fit@fit$series$h), t="l",
  col="blue", xlab="", ylab="",
  main="GARCH fitted standard deviation")

# specify GARCH model
garch_spec <- fGarch::garchSpec(
  model=list(omega=om_ega, alpha=al_pha, beta=be_ta))
# simulate GARCH model
garch_sim <-
  fGarch::garchSim(spec=garch_spec, n=n_rows)
re_turns <- as.numeric(garch_sim)
# Calculate kurtosis of GARCH returns
moments::moment(re_turns, order=4) /
  moments::moment(re_turns, order=2)^2
# Perform Jarque-Bera test of normality
tseries::jarque.bera.test(re_turns)
# Plot histogram of GARCH returns
histo_gram <- hist(re_turns, col="lightgrey",
  xlab="returns", breaks=200, xlim=c(-0.05, 0.05),
  ylab="frequency", freq=FALSE,
  main="GARCH returns histogram")
lines(density(re_turns, adjust=1.5),
lwd=3, col="blue")

# fit t-distribution into GARCH returns
optim_fit <- MASS::fitdistr(re_turns,
  densfun="t", df=2, lower=c(-1, 1e-7))
lo_cation <- optim_fit$estimate[1]
scal_e <- optim_fit$estimate[2]
curve(expr=dt((x-lo_cation)/scal_e, df=2)/scal_e,
  type="l", xlab="", ylab="", lwd=3,
  col="red", add=TRUE)
legend("topright", inset=0.05, bty="n",
 leg=c("density", "t-distr w/ 2 dof"),
 lwd=6, lty=1,
 col=c("blue", "red"))

library(HighFreq)  # load HighFreq
# Calculate variance for each period
vari_ance <- 252*(24*60*60)^2*
  HighFreq::run_variance(oh_lc=etf_env$VTI)
# Calculate EWMA VTI variance using RcppRoll
library(RcppRoll)  # load RcppRoll
look_back <- 51
weight_s <- exp(0.1*1:look_back)
var_ewma <- RcppRoll::roll_mean(vari_ance,
    align="right", n=look_back, weights=weight_s)
var_ewma <- xts(var_ewma,
    order.by=index(etf_env$VTI[-(1:(look_back-1)), ]))
colnames(var_ewma) <- "VTI variance"
# Plot EWMA variance with custom line colors
x11(width=6, height=5)
chart_Series(etf_env$VTI["2010-01/2010-10"],
   name="VTI EWMA variance with May 6, 2010 Flash Crash")
# Add variance in extra panel
add_TA(var_ewma["2010-01/2010-10"], col="black")

library(HighFreq)  # load HighFreq
# Minutely SPY returns (unit per minute) single day
re_turns <- rutils::diff_it(log(SPY["2012-02-13", 4]))
# Minutely SPY volatility (unit per minute)
sd(re_turns)
# Minutely SPY returns (unit per second)
re_turns <- rutils::diff_it(log(SPY["2012-02-13", 4])) / 
  rutils::diff_it(.index(SPY["2012-02-13"]))
# Minutely SPY volatility scaled to unit per minute
60*sd(re_turns)
# Minutely SPY returns multiple days no overnight scaling
re_turns <- rutils::diff_it(log(SPY[, 4]))
# Minutely SPY volatility (unit per minute)
sd(re_turns)
# Minutely SPY returns (unit per second)
re_turns <- rutils::diff_it(log(SPY[, 4])) / 
  rutils::diff_it(.index(SPY))
# Minutely SPY volatility scaled to unit per minute
60*sd(re_turns)
# Table of time intervals - 60 second is most frequent
interval_s <- rutils::diff_it(.index(SPY))
table(interval_s)
hist(interval_s)

library(HighFreq)  # load HighFreq
# Daily OHLC SPY prices
SPY_daily <- 
  rutils::to_period(oh_lc=SPY, period="days")
# Daily SPY returns and volatility
sd(rutils::diff_it(log(SPY_daily[, 4])))
# Minutely SPY returns (unit per minute)
re_turns <- rutils::diff_it(log(SPY[, 4]))
# Minutely SPY volatility scaled to daily interval
sqrt(6.5*60)*sd(re_turns)
# Minutely SPY returns (unit per second)
re_turns <- rutils::diff_it(log(SPY[, 4])) / 
  rutils::diff_it(.index(SPY))
# Minutely SPY volatility scaled to daily aggregation interval
60*sqrt(6.5*60)*sd(re_turns)
# Daily SPY volatility
# including extra time over weekends and holidays
24*60*60*sd(rutils::diff_it(log(SPY_daily[, 4])) / 
    rutils::diff_it(.index(SPY_daily)))
table(rutils::diff_it(.index(SPY_daily)))

# Scaled minutely SPY returns
re_turns <- rutils::diff_it(as.numeric(Cl(SPY)))[-1] /
  rutils::diff_it(.index(SPY))[-1]
range(re_turns)
hist(re_turns, breaks=100,
     xlim=c(-0.005, 0.005))
# SPY prices
price_s <- cumsum(re_turns)
plot(price_s, t="l")
# Perform rescaled range analysis
look_back <- 100
end_points <- rutils::calc_endpoints(re_turns,
  inter_val=look_back)
r_s <- sapply(seq_along(end_points)[-1],
      function(it) {
  indeks <- end_points[it-1]:end_points[it]
  price_s <- price_s[indeks]
  (max(price_s) - min(price_s)) /
    sd(re_turns[indeks])
})  # end sapply
mean(r_s)

# Perform rescaled range analysis over many look-backs
look_backs <- seq.int(1e2, 1e3, 1e2)
r_s <- sapply(look_backs, function(look_back) {
  end_points <- rutils::calc_endpoints(re_turns, inter_val=look_back)
  r_s <- sapply(seq_along(end_points)[-1], function(it) {
    indeks <- end_points[it-1]:end_points[it]
    (max(price_s[indeks]) - min(price_s[indeks]))/sd(re_turns[indeks])
  })  # end sapply
  mean(r_s)
})  # end sapply
names(r_s) <- paste0("agg_", look_backs)
rs_log <- log(r_s)
look_log <- log(look_backs)
mod_el <- lm(rs_log ~ look_log)
hurst_lm <- summary(mod_el)$coeff[2, 1]
look_log <- look_log - mean(look_log)
rs_log <- rs_log - mean(rs_log)
hurst_mat <- sum(rs_log*look_log)/sum(look_log^2)
all.equal(hurst_lm, hurst_mat)

x11(width=6, height=5)
par(mar=c(4, 4, 2, 1), oma=c(1, 1, 1, 1))
plot(rs_log ~ look_log, lwd=6, col="red",
     xlab="aggregation intervals (log)",
     ylab="rescaled range (log)",
     main="Rescaled Range Analysis for SPY")
abline(mod_el, lwd=3, col="blue")
text(-1.2, 0.2, paste0("Hurst = ",
  round(hurs_t, 4)))

library(HighFreq)  # load HighFreq
# Daily SPY volatility from minutely prices using package TTR
library(TTR)
sqrt((6.5*60)*mean(na.omit(
  TTR::volatility(SPY, N=1,
          calc="yang.zhang"))^2))
# SPY volatility using package HighFreq
60*sqrt((6.5*60)*agg_regate(oh_lc=SPY,
    weight_ed=FALSE, mo_ment="run_variance",
    calc_method="yang_zhang"))

library(HighFreq)  # load HighFreq
oh_lc <- log(rutils::etf_env$VTI)
# Calculate variance
var_close <- HighFreq::run_variance(oh_lc=oh_lc,
  calc_method="close")
var_yang_zhang <- HighFreq::run_variance(oh_lc=oh_lc)
std_dev <- 24*60*60*sqrt(252*cbind(var_close, var_yang_zhang))
colnames(std_dev) <- c("close std_dev", "Yang-Zhang")
# Plot the time series of volatility
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("black", "red")
chart_Series(std_dev["2011-07/2011-12"],
  theme=plot_theme, name="Standard Deviations: Close and YZ")
legend("top", legend=colnames(std_dev),
 bg="white", lty=1, lwd=6, inset=0.1, cex=0.8,
 col=plot_theme$col$line.col, bty="n")
# Plot volatility around 2010 flash crash
chart_Series(std_dev["2010-04/2010-06"],
  theme=plot_theme, name="Volatility Around 2010 Flash Crash")
legend("top", legend=colnames(std_dev),
 bg="white", lty=1, lwd=6, inset=0.1, cex=0.8,
 col=plot_theme$col$line.col, bty="n")
# Plot density of volatility distributions
plot(density(std_dev[, 1]), xlab="", ylab="",
  main="Density of Volatility Distributions",
  xlim=c(-0.05, range(std_dev[, 1])[2]/3), type="l", lwd=2, col="blue")
lines(density(std_dev[, 2]), col='red', lwd=2)
legend("top", legend=c("Close-to-Close", "Yang-Zhang"),
 bg="white", lty=1, lwd=6, inset=0.1, cex=0.8,
 col=plot_theme$col$line.col, bty="n")
# ? range volatility estimator has lower standard error ?
c(sd(var_close)/mean(var_close), sd(var_yang_zhang)/mean(var_yang_zhang))
foo <- std_dev[var_close<range(var_close)[2]/3, ]
c(sd(foo[, 1])/mean(foo[, 1]), sd(foo[, 2])/mean(foo[, 2]))
plot(density(foo[, 1]), xlab="", ylab="",
  main="Mixture of Normal Returns",
  xlim=c(-0.05, range(foo[, 1])[2]/2), type="l", lwd=2, col="blue")
lines(density(foo[, 2]), col='red', lwd=2)

oh_lc <- rutils::etf_env$VTI
re_turns <- log((oh_lc[, 2] - oh_lc[, 3]) / (oh_lc[, 2] + oh_lc[, 3]))
foo <- rutils::diff_it(log(oh_lc[, 4]))
plot(as.numeric(foo)^2, as.numeric(re_turns)^2)
bar <- lm(re_turns ~ foo)
summary(bar)


# Perform normality tests
shapiro.test(coredata(re_turns))
tseries::jarque.bera.test(re_turns)
# Fit VTI returns using MASS::fitdistr()
optim_fit <- MASS::fitdistr(re_turns,
            densfun="t", df=2)
optim_fit$estimate; optim_fit$sd
# Calculate moments of standardized returns
sapply(3:4, moments::moment,
  x=(re_turns - mean(re_turns))/sd(re_turns))

# Plot histogram of VTI returns
col_ors <- c("lightgray", "blue", "green", "red")
PerformanceAnalytics::chart.Histogram(re_turns,
  main="", xlim=c(-7, -3), col=col_ors[1:3],
  methods = c("add.density", "add.normal"))
curve(expr=dt((x-optim_fit$estimate[1])/
  optim_fit$estimate[2], df=2)/optim_fit$estimate[2],
type="l", xlab="", ylab="", lwd=2,
col=col_ors[4], add=TRUE)
# Add title and legend
title(main="VTI logarithm of range",
cex.main=1.3, line=-1)
legend("topright", inset=0.05,
  legend=c("density", "normal", "t-distr"),
  lwd=6, lty=1, col=col_ors[2:4], bty="n")

# VTI range variance partial autocorrelations
pacf(re_turns^2, lag=10, xlab=NA, ylab=NA,
     main="PACF of VTI log range")
chart_Series(re_turns^2,
       name="VTI log of range squared")

# standard errors of variance estimators using bootstrap
boot_strap <- sapply(1:1e2, function(x) {
  # Create random OHLC
  oh_lc <- HighFreq::random_ohlc()
  # Calculate variance estimate
  c(var=var(oh_lc[, 4]),
    yang_zhang=HighFreq::calc_variance(
oh_lc, calc_method="yang_zhang", scal_e=FALSE))
})  # end sapply
# Analyze bootstrapped variance
boot_strap <- t(boot_strap)
head(boot_strap)
colMeans(boot_strap)
apply(boot_strap, MARGIN=2, sd) /
  colMeans(boot_strap)

par(oma=c(1, 1, 1, 1), mar=c(2, 2, 1, 1), mgp=c(0, 0.5, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # set plot panels
# Close variance estimator partial autocorrelations
pacf(var_close, lag=10, xlab=NA, ylab=NA)
title(main="VTI close variance partial autocorrelations")

# Range variance estimator partial autocorrelations
pacf(var_yang_zhang, lag=10, xlab=NA, ylab=NA)
title(main="VTI YZ variance partial autocorrelations")

# Squared range partial autocorrelations
re_turns <- log(rutils::etf_env$VTI[,2] /
            rutils::etf_env$VTI[,3])
pacf(re_turns^2, lag=10, xlab=NA, ylab=NA)
title(main="VTI squared range partial autocorrelations")

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
# Add title
title(main="Managers cumulative returns",
line=-1)

library(PerformanceAnalytics)  # load package "PerformanceAnalytics"
data(managers)  # load "managers" data set
charts.PerformanceSummary(ham_1,
  main="", lwd=2, ylog=TRUE)

library(PerformanceAnalytics)  # load package "PerformanceAnalytics"
chart.CumReturns(
  etf_env$re_turns[, c("XLF", "DBC", "IEF")], lwd=2,
  ylab="", legend.loc="topleft", main="")
# Add title
title(main="ETF cumulative returns", line=-1)

options(width=200)
library(PerformanceAnalytics)
chart.Drawdown(etf_env$re_turns[, "VTI"], ylab="",
         main="VTI drawdowns")

options(width=200)
library(PerformanceAnalytics)
table.Drawdowns(etf_env$re_turns[, "VTI"])

library(PerformanceAnalytics)
chart.Histogram(etf_env$re_turns[, 1], main="",
  xlim=c(-0.06, 0.06),
  methods = c("add.density", "add.normal"))
# Add title
title(main=paste(colnames(etf_env$re_turns[, 1]),
           "density"), line=-1)

library(PerformanceAnalytics)
chart.Boxplot(etf_env$re_turns[,
  c("VTI", "IEF", "IVW", "VYM", "IWB", "DBC", "VXX")])

library(PerformanceAnalytics)
tail(table.Stats(etf_env$re_turns[,
  c("VTI", "IEF", "DBC", "VXX")]), 4)
risk_return <- table.Stats(etf_env$re_turns)
class(risk_return)
# Transpose the data frame
risk_return <- as.data.frame(t(risk_return))

# Plot scatterplot
plot(Kurtosis ~ Skewness, data=risk_return,
     main="Kurtosis vs Skewness")
# Add labels
text(x=risk_return$Skewness, y=risk_return$Kurtosis,
    labels=rownames(risk_return),
    pos=1, cex=0.8)

# Add skew_kurt column
risk_return$skew_kurt <-
  risk_return$Skewness/risk_return$Kurtosis
# sort on skew_kurt
risk_return <- risk_return[
  order(risk_return$skew_kurt,
  decreasing=TRUE), ]
# Add names column
risk_return$Name <-
  etf_list[rownames(risk_return), ]$Name

risk_return[, c("Name", "Skewness", "Kurtosis")]

library(PerformanceAnalytics)
chart.RiskReturnScatter(
  etf_env$re_turns[, colnames(etf_env$re_turns)!="VXX"],
  Rf=0.01/12)

library(PerformanceAnalytics)
vti_ief <- etf_env$re_turns[, c("VTI", "IEF")]
SharpeRatio(vti_ief)

SortinoRatio(vti_ief)

CalmarRatio(vti_ief)
tail(table.Stats(vti_ief), 4)
