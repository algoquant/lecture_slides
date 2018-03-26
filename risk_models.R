library(knitr)
opts_chunk$set(prompt=TRUE, eval=FALSE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size="scriptsize", fig.width=4, fig.height=4)
options(width=60, dev="pdf")
options(digits=3)
thm <- knit_theme$get("acid")
knit_theme$set(thm)
# VTI percentage returns
re_turns <- rutils::diff_xts(log(quantmod::Cl(rutils::env_etf$VTI)))
# number of observations
n_row <- NROW(re_turns)
# mean of VTI returns
mean_rets <- mean(re_turns)
# standard deviation of VTI returns
sd_rets <- sd(re_turns)
# skew of VTI returns
n_row/((n_row-1)*(n_row-2))*
  sum(((re_turns - mean_rets)/sd_rets)^3)
# kurtosis of VTI returns
n_row*(n_row+1)/((n_row-1)^3)*
  sum(((re_turns - mean_rets)/sd_rets)^4)
# random normal returns
re_turns <- rnorm(n_row, sd=sd_rets)
# mean and standard deviation of random normal returns
mean_rets <- mean(re_turns)
sd_rets <- sd(re_turns)
# skew of random normal returns
n_row/((n_row-1)*(n_row-2))*
  sum(((re_turns - mean_rets)/sd_rets)^3)
# kurtosis of random normal returns
n_row*(n_row+1)/((n_row-1)^3)*
  sum(((re_turns - mean_rets)/sd_rets)^4)
set.seed(1121)  # reset random number generator
# sample from Standard Normal Distribution
n_row <- 1000
sam_ple <- rnorm(n_row)
# sample mean
mean(sam_ple)
# sample standard deviation
sd(sam_ple)
re_turns <- rnorm(1000)
sd(re_turns)
mad(re_turns)
median(abs(re_turns - median(re_turns)))
median(abs(re_turns - median(re_turns)))/qnorm(0.75)
# bootstrap of sd and mad estimators
boot_strap <- sapply(1:10000, function(x) {
  boot_sample <-
    re_turns[sample.int(n_row, replace=TRUE)]
  c(sd=sd(boot_sample),
    mad=mad(boot_sample))
})  # end sapply
boot_strap <- t(boot_strap)
# parallel bootstrap under Windows
library(parallel)  # load package parallel
num_cores <- detectCores() - 1  # number of cores
clus_ter <- makeCluster(num_cores)  # initialize compute cluster
boot_strap <- parLapply(clus_ter, 1:10000,
  function(x, re_turns) {
    boot_sample <-
re_turns[sample.int(NROW(re_turns), replace=TRUE)]
    c(sd=sd(boot_sample),
mad=mad(boot_sample))
  }, re_turns=re_turns)  # end parLapply
# parallel bootstrap under Mac-OSX or Linux
boot_strap <- mclapply(1:10000,
  function(x) {
    boot_sample <-
re_turns[sample.int(NROW(re_turns), replace=TRUE)]
    c(sd=sd(boot_sample),
mad=mad(boot_sample))
  }, mc.cores=num_cores)  # end mclapply
stopCluster(clus_ter)  # stop R processes over cluster
# analyze bootstrapped variance
boot_strap <- rutils::do_call(rbind, boot_strap)
head(boot_strap)
sum(is.na(boot_strap))
# means and standard errors from bootstrap
apply(boot_strap, MARGIN=2,
function(x) c(mean=mean(x), std_error=sd(x)))
x_var <- seq(-5, 7, length=100)
y_var <- dnorm(x_var, mean=1.0, sd=2.0)
plot(x_var, y_var, type="l", lty="solid",
     xlab="", ylab="")
title(main="Normal Density Function", line=0.5)
star_t <- 3; fin_ish <- 5  # set lower and upper bounds
# set polygon base
are_a <- ((x_var >= star_t) & (x_var <= fin_ish))
polygon(c(star_t, x_var[are_a], fin_ish),  # draw polygon
  c(-1, y_var[are_a], -1), col="red")
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
sig_mas <- c(0.5, 1, 1.5, 2)  # sigma values
# create plot colors
col_ors <- c("red", "black", "blue", "green")
# create legend labels
lab_els <- paste("sigma", sig_mas, sep="=")
for (in_dex in 1:4) {  # plot four curves
curve(expr=dnorm(x, sd=sig_mas[in_dex]),
type="l", xlim=c(-4, 4),
xlab="", ylab="", lwd=2,
col=col_ors[in_dex],
add=as.logical(in_dex-1))
}  # end for
# add title
title(main="Normal Distributions", line=0.5)
# add legend
legend("topright", inset=0.05, title="Sigmas",
 lab_els, cex=0.8, lwd=2, lty=c(1, 1, 1, 1),
 col=col_ors)
x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
deg_free <- c(3, 6, 9)  # df values
col_ors <- c("black", "red", "blue", "green")
lab_els <- c("normal", paste("df", deg_free, sep="="))
# plot a Normal probability distribution
curve(expr=dnorm, type="l", xlim=c(-4, 4),
      xlab="", ylab="", lwd=2)
for (in_dex in 1:3) {  # plot three t-distributions
curve(expr=dt(x, df=deg_free[in_dex]),
      type="l", xlab="", ylab="", lwd=2,
      col=col_ors[in_dex+1], add=TRUE)
}  # end for
# add title
title(main="t-distributions", line=0.5)
# add legend
legend("topright", inset=0.05,
       title="Degrees\n of freedom", lab_els,
       cex=0.8, lwd=6, lty=c(1, 1, 1, 1),
       col=col_ors)
# objective function is log-likelihood
object_ive <- function(pa_r, free_dom, sam_ple) {
  sum(
    -log(gamma((free_dom+1)/2) /
      (sqrt(pi*free_dom) * gamma(free_dom/2))) +
    log(pa_r[2]) +
    (free_dom+1)/2 * log(1 + ((sam_ple - pa_r[1])/
                    pa_r[2])^2/free_dom))
}  # end object_ive
# simpler objective function
object_ive <- function(pa_r, free_dom, sam_ple) {
  -sum(log(dt(x=(sam_ple-pa_r[1])/pa_r[2],
      df=free_dom)/pa_r[2]))
}  # end object_ive
# demonstrate equivalence of the two methods
object_ive(c(0, 1), 2, 2:5)
-sum(log(dt(x=2:5, df=2)))
object_ive(c(1, 0.5), 2, 2:5)
-sum(log(dt(x=(2:5-1)/0.5, df=2)/0.5))
# VTI percentage returns
re_turns <- rutils::diff_xts(log(quantmod::Cl(rutils::env_etf$VTI)))
# initial parameters
par_init <- c(mean=0, scale=0.01)
# fit distribution using optim()
optim_fit <- optim(par=par_init,
  fn=object_ive, # log-likelihood function
  sam_ple=re_turns,
  free_dom=2, # degrees of freedom
  method="L-BFGS-B", # quasi-Newton method
  upper=c(1, 0.1), # upper constraint
  lower=c(-1, 1e-7)) # lower constraint
# optimal parameters
lo_cation <- optim_fit$par["mean"]
sc_ale <- optim_fit$par["scale"]
# fit distribution using MASS::fitdistr()
optim_fit <- MASS::fitdistr(re_turns,
  densfun="t", df=2, lower=c(-1, 1e-7))
optim_fit$estimate
optim_fit$sd
lo_cation <- optim_fit$estimate[1]
sc_ale <- optim_fit$estimate[2]
x11(width=6, height=5)
# plot histogram of VTI returns
histo_gram <- hist(re_turns, col="lightgrey",
  xlab="returns", breaks=100, xlim=c(-0.05, 0.05),
  ylab="frequency", freq=FALSE,
  main="VTI returns histogram")
lines(density(re_turns, adjust=1.5),
lwd=3, col="blue")
# plot the Normal probability distribution
curve(expr=dnorm(x, mean=mean(re_turns),
  sd=sd(re_turns)), add=TRUE, type="l",
  xlab="", ylab="", lwd=3, col="green")
# plot t-distribution function
curve(expr=dt((x-lo_cation)/sc_ale, df=2)/sc_ale,
type="l", xlab="", ylab="", lwd=3,
col="red", add=TRUE)
# add legend
legend("topright", inset=0.05,
  leg=c("density", "t-distr", "normal"),
  lwd=6, lty=c(1, 1, 1),
  col=c("blue", "red", "green"))
x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
deg_free <- c(2, 5, 8, 11)  # df values
# create plot colors
col_ors <- c("red", "black", "blue", "green")
# create legend labels
lab_els <- paste("df", deg_free, sep="=")
for (in_dex in 1:4) {  # plot four curves
curve(expr=dchisq(x, df=deg_free[in_dex]),
      type="l", xlim=c(0, 20), ylim=c(0, 0.3),
      xlab="", ylab="", lwd=2,
      col=col_ors[in_dex],
      add=as.logical(in_dex-1))
}  # end for
# add title
title(main="Chi-squared Distributions", line=0.5)
# add legend
legend("topright", inset=0.05,
       title="Degrees of freedom", lab_els,
       cex=0.8, lwd=6, lty=c(1, 1, 1, 1),
       col=col_ors)
# KS test for normal distribution
ks.test(rnorm(100), pnorm)
# KS test for uniform distribution
ks.test(runif(100), pnorm)
# KS test for two similar normal distributions
ks.test(rnorm(100), rnorm(100, mean=0.1))
# KS test for two different normal distributions
ks.test(rnorm(100), rnorm(100, mean=1.0))
# calculate cumulative probabilities and then difference them
prob_s <- pt((histo_gram$breaks-lo_cation)/sc_ale, df=2)
prob_s <- diff(prob_s)
# perform Chi-squared test
chisq.test(histo_gram$counts, p=prob_s,
  rescale.p=TRUE, simulate.p.value=TRUE)

# calculate sample from t-distribution
sam_ple <- lo_cation + sc_ale*rt(NROW(re_turns), df=2)
# perform Kolmogorov-Smirnov test
ks.test(as.numeric(re_turns), sam_ple)
price_s <- Cl(rutils::env_etf$VTI)
end_points <- seq_along(price_s)  # define end points
len_gth <- NROW(end_points)
look_back <- 22  # number of data points per look-back interval
# start_points are multi-period lag of end_points
start_points <- c(rep_len(1, look_back-1),
    end_points[1:(len_gth-look_back+1)])
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
  bg="white", lty=c(1, 1, 1), lwd=c(6, 6, 6),
  col=plot_theme$col$line.col, bty="n")
# library(HighFreq)  # load package HighFreq
# define functional for rolling aggregations
roll_agg <- function(x_ts, look_back, FUN, ...) {
# define end points at every period
  end_points <- seq_along(x_ts)
  len_gth <- NROW(end_points)
# define starting points as lag of end_points
  start_points <- c(rep_len(1, look_back-1),
    end_points[1:(len_gth-look_back+1)])
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
# library(TTR)  # load package TTR
# benchmark the speed of TTR::runSum
library(microbenchmark)
summary(microbenchmark(
  cum_sum=cumsum(coredata(price_s)),
  roll_sum=rutils::roll_sum(price_s, win_dow=look_back),
  run_sum=TTR::runSum(price_s, n=look_back),
  times=10))[, c(1, 4, 5)]
library(RcppRoll)  # load package RcppRoll
wid_th <- 22  # number of data points per look-back interval
# calculate rolling sum using rutils
prices_mean <-
  rutils::roll_sum(price_s, win_dow=wid_th)
# calculate rolling sum using RcppRoll
prices_mean <- RcppRoll::roll_sum(price_s,
              align="right", n=wid_th)
# benchmark the speed of RcppRoll::roll_sum
library(microbenchmark)
summary(microbenchmark(
  cum_sum=cumsum(coredata(price_s)),
  rcpp_roll_sum=RcppRoll::roll_sum(price_s, n=wid_th),
  roll_sum=rutils::roll_sum(price_s, win_dow=wid_th),
  times=10))[, c(1, 4, 5)]
# calculate EWMA sum using RcppRoll
weight_s <- exp(0.1*1:wid_th)
prices_mean <- RcppRoll::roll_mean(price_s,
align="right", n=wid_th, weights=weight_s)
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
 bg="white", lty=c(1, 1), lwd=c(6, 6),
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
price_s <- Cl(rutils::env_etf$VTI)
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
len_gth <- NROW(end_points)
# start_points are single-period lag of end_points
start_points <- c(1, end_points[1:(len_gth-1)])
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
  bg="white", lty=c(1, 1), lwd=c(6, 6),
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
  bg="white", lty=c(1, 1), lwd=c(6, 6),
  col=plot_theme$col$line.col, bty="n")
# library(HighFreq)  # load package HighFreq
# define functional for rolling aggregations over end_points
roll_agg <- function(x_ts, end_points, FUN, ...) {
  len_gth <- NROW(end_points)
# start_points are single-period lag of end_points
  start_points <- c(1, end_points[1:(len_gth-1)])
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
len_gth <- NROW(end_points)
num_points <- 4  # number of end points in look-back interval
# start_points are multi-period lag of end_points
start_points <- c(rep_len(1, num_points-1),
  end_points[1:(len_gth-num_points+1)])
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
  bg="white", lty=c(1, 1), lwd=c(6, 6),
  col=plot_theme$col$line.col, bty="n")
library(HighFreq)  # load package HighFreq
agg_regations <- cbind(price_s, agg_regations)
tail(agg_regations, 22)
agg_regations <- na.omit(zoo::na.locf(agg_regations))
# plot aggregations with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("black", "red", "green")
chart_Series(agg_regations, theme=plot_theme,
       name="price aggregations")
legend("top", legend=colnames(agg_regations),
  bg="white", lty=c(1, 1, 1), lwd=c(6, 6, 6),
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
# VTI percentage returns
re_turns <- rutils::diff_xts(log(quantmod::Cl(rutils::env_etf$VTI)))
# define end points
end_points <- seq_along(re_turns)
len_gth <- NROW(end_points)
look_back <- 51
# start_points are multi-period lag of end_points
start_points <- c(rep_len(1, look_back-1),
    end_points[1:(len_gth-look_back+1)])
# define list of look-back intervals for aggregations over past
look_backs <- lapply(seq_along(end_points),
  function(in_dex) {
    start_points[in_dex]:end_points[in_dex]
})  # end lapply
# calculate realized VTI variance in sapply() loop
vari_ance <- sapply(look_backs,
  function(look_back) {
    ret_s <- re_turns[look_back]
    sum((ret_s - mean(ret_s))^2)
}) / (look_back-1)  # end sapply
tail(vari_ance)
class(vari_ance)
# coerce vari_ance into xts
vari_ance <- xts(vari_ance, order.by=index(re_turns))
colnames(vari_ance) <- "VTI.variance"
head(vari_ance)
# calculate VTI variance using package roll
library(roll)  # load roll
vari_ance <-
  roll::roll_var(re_turns, width=look_back)
colnames(vari_ance) <- "VTI.variance"
head(vari_ance)
sum(is.na(vari_ance))
vari_ance[1:(look_back-1)] <- 0
# benchmark calculation of rolling variance
library(microbenchmark)
summary(microbenchmark(
  roll_sapply=sapply(look_backs, function(look_back) {
    ret_s <- re_turns[look_back]
    sum((ret_s - mean(ret_s))^2)
  }),
  ro_ll=roll::roll_var(re_turns, width=look_back),
  times=10))[, c(1, 4, 5)]
# calculate EWMA VTI variance using filter()
wid_th <- 51
weight_s <- exp(-0.1*1:wid_th)
weight_s <- weight_s/sum(weight_s)
vari_ance <- stats::filter(re_turns^2,
    filter=weight_s, sides=1)
vari_ance[1:(wid_th-1)] <- vari_ance[wid_th]
class(vari_ance)
vari_ance <- as.numeric(vari_ance)
x_ts <- xts:::xts(sqrt(vari_ance), order.by=index(re_turns))
# plot EWMA standard deviation
chart_Series(x_ts,
  name="EWMA standard deviation")
dygraphs::dygraph(x_ts, main="EWMA standard deviation")
# calculate VTI variance using package roll
library(roll)  # load roll
vari_ance <- roll::roll_var(re_turns,
  weights=rev(weight_s), width=wid_th)
colnames(vari_ance) <- "VTI.variance"
class(vari_ance)
head(vari_ance)
sum(is.na(vari_ance))
vari_ance[1:(wid_th-1)] <- 0
x11(width=6, height=4)
par(mar=c(4, 3, 1, 1), oma=c(0, 0, 0, 0))
# VTI percentage returns
re_turns <- rutils::diff_xts(log(quantmod::Cl(rutils::env_etf$VTI)))
# calculate VTI variance using package roll
look_back <- 22
vari_ance <-
  roll::roll_var(re_turns, width=look_back)
vari_ance[1:(look_back-1)] <- 0
colnames(vari_ance) <- "VTI.variance"
# number of look_backs that fit over re_turns
n_row <- NROW(re_turns)
num_agg <- n_row %/% look_back
end_points <- # define end_points with beginning stub
  n_row-look_back*num_agg + (0:num_agg)*look_back
len_gth <- NROW(end_points)
# subset vari_ance to end_points
vari_ance <- vari_ance[end_points]
# improved autocorrelation function
acf_plus(coredata(vari_ance), lag=10, main="")
title(main="acf of variance", line=-1)
# partial autocorrelation
pacf(coredata(vari_ance), lag=10, main="", ylab=NA)
title(main="pacf of variance", line=-1)
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
x11(width=6, height=4)
par(mar=c(3, 3, 1, 1), oma=c(0, 0, 0, 0))
# plot GARCH cumulative returns
plot(cumsum(re_turns/100), t="l",
  lwd=2, col="orange", xlab="", ylab="",
  main="GARCH cumulative returns")
date_s <- seq.Date(from=Sys.Date()-len_gth+1,
  to=Sys.Date(), length.out=len_gth)
x_ts <- xts:::xts(cumsum(re_turns/100), order.by=date_s)
dygraphs::dygraph(x_ts, main="GARCH cumulative returns")
# plot GARCH standard deviation
plot(sqrt(vari_ance), t="l",
  col="orange", xlab="", ylab="",
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
# plot GARCH fitted standard deviation
plot.zoo(sqrt(garch_fit@fit$series$h), t="l",
  col="orange", xlab="", ylab="",
  main="GARCH fitted standard deviation")
# specify GARCH model
garch_spec <- fGarch::garchSpec(
  model=list(omega=om_ega, alpha=al_pha, beta=be_ta))
# simulate GARCH model
garch_sim <-
  fGarch::garchSim(spec=garch_spec, n=len_gth)
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
# fit t-distribution into GARCH returns
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
library(HighFreq)  # load HighFreq
# calculate variance for each period
vari_ance <- 252*(24*60*60)^2*
  HighFreq::run_variance(oh_lc=env_etf$VTI)
# calculate EWMA VTI variance using RcppRoll
library(RcppRoll)  # load RcppRoll
wid_th <- 51
weight_s <- exp(0.1*1:wid_th)
var_ewma <- RcppRoll::roll_mean(vari_ance,
    align="right", n=wid_th, weights=weight_s)
var_ewma <- xts(var_ewma,
    order.by=index(env_etf$VTI[-(1:(wid_th-1)), ]))
colnames(var_ewma) <- "VTI variance"
# plot EWMA variance with custom line colors
x11()
chart_Series(env_etf$VTI["2010-01/2010-10"],
   name="VTI EWMA variance with May 6, 2010 Flash Crash")
# add variance in extra panel
add_TA(var_ewma["2010-01/2010-10"], col="black")
# extract time index from VTI
in_dex <- index(rutils::env_etf$VTI)
n_row <- NROW(in_dex)
half_length <- n_row %/% 2
# simulate returns with random volatility
set.seed(1121)  # reset random number generator
# specify random volatility with sd=1 and sd=4
vol_at <- 0.01*sample(c(rep(1, half_length), rep(4, n_row-half_length)))
re_turns <- vol_at*rnorm(n_row) - vol_at^2/2
re_turns <- re_turns/sd(re_turns)
re_turns <- xts(re_turns, order.by=in_dex)
# calculate moments
sapply(1:4, FUN=moments::moment, x=re_turns)
# fit distribution using MASS::fitdistr()
optim_fit <-
  MASS::fitdistr(re_turns, densfun="t", df=2)
# plot returns histogram using PerformanceAnalytics
col_ors <- c("lightgray", "blue", "green", "red")
x11()
PerformanceAnalytics::chart.Histogram(re_turns,
  main="", ylim=c(0.0, 0.8), col=col_ors[1:3],
  methods = c("add.density", "add.normal"))
curve(expr=dt((x-optim_fit$estimate[1])/
  optim_fit$estimate[2], df=2)/optim_fit$estimate[2],
type="l", xlab="", ylab="", lwd=2,
col=col_ors[4], add=TRUE)
# add title
title(main="Mixture of volatilities returns histogram", cex.main=1.3, line=-1)
# add legend
lab_els <- c("density", "normal", "t-distr")
legend("topright", inset=0.05, lab_els,
 lwd=2, lty=c(1, 1, 1),
 col=col_ors[2:4])
library(HighFreq)  # load HighFreq
# minutely SPY returns (unit per minute) single day
re_turns <- rutils::diff_xts(log(SPY["2012-02-13", 4]))
# minutely SPY volatility (unit per minute)
sd(re_turns)
# minutely SPY returns (unit per second)
re_turns <- rutils::diff_xts(log(SPY["2012-02-13", 4])) / 
  c(1, diff(.index(SPY["2012-02-13"])))
# minutely SPY volatility scaled to unit per minute
60*sd(re_turns)
# minutely SPY returns multiple days no overnight scaling
re_turns <- rutils::diff_xts(log(SPY[, 4]))
# minutely SPY volatility (unit per minute)
sd(re_turns)
# minutely SPY returns (unit per second)
re_turns <- rutils::diff_xts(log(SPY[, 4])) / 
  c(1, diff(.index(SPY)))
# minutely SPY volatility scaled to unit per minute
60*sd(re_turns)
table(c(1, diff(.index(SPY))))
library(HighFreq)  # load HighFreq
# daily OHLC SPY prices
SPY_daily <- 
  rutils::to_period(oh_lc=SPY, period="days")
# daily SPY returns and volatility
sd(rutils::diff_xts(log(SPY_daily[, 4])))
# minutely SPY returns (unit per minute)
re_turns <- rutils::diff_xts(log(SPY[, 4]))
# minutely SPY volatility scaled to daily interval
sqrt(6.5*60)*sd(re_turns)

# minutely SPY returns (unit per second)
re_turns <- rutils::diff_xts(log(SPY[, 4])) / 
  c(1, diff(.index(SPY)))
# minutely SPY volatility scaled to daily aggregation interval
60*sqrt(6.5*60)*sd(re_turns)

# daily SPY volatility
# including extra time over weekends and holidays
24*60*60*sd(rutils::diff_xts(log(SPY_daily[, 4])) / 
    c(1, diff(.index(SPY_daily))))
table(c(1, diff(.index(SPY_daily))))
library(HighFreq)  # load HighFreq
# daily SPY volatility from minutely prices using package TTR
library(TTR)
sqrt((6.5*60)*mean(na.omit(
  TTR::volatility(SPY, N=1,
          calc="yang.zhang"))^2))
# SPY volatility using package HighFreq
60*sqrt((6.5*60)*agg_regate(oh_lc=SPY,
    weight_ed=FALSE, mo_ment="run_variance",
    calc_method="yang_zhang"))
library(HighFreq)  # load HighFreq
# calculate variance
var_close <-
  HighFreq::run_variance(oh_lc=env_etf$VTI,
                   calc_method="close")
var_yang_zhang <-
  HighFreq::run_variance(oh_lc=env_etf$VTI)
vari_ance <-
  252*(24*60*60)^2*cbind(var_close, var_yang_zhang)
colnames(vari_ance) <-
  c("close var", "Yang-Zhang var")
# plot
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("black", "red")
x11()
chart_Series(vari_ance["2011-06/2011-12"],
  theme=plot_theme, name="Close and YZ variances")
legend("top", legend=colnames(vari_ance),
 bg="white", lty=c(1, 1), lwd=c(6, 6),
 col=plot_theme$col$line.col, bty="n")
re_turns <-
  ifelse(env_etf$VTI[, 2] > env_etf$VTI[, 3],
  log((env_etf$VTI[, 2] - env_etf$VTI[, 3]) /
    (env_etf$VTI[, 2] + env_etf$VTI[, 3])), 0)
# perform normality tests
shapiro.test(coredata(re_turns))
tseries::jarque.bera.test(re_turns)
# fit distribution using MASS::fitdistr()
optim_fit <- MASS::fitdistr(re_turns,
            densfun="t", df=2)
optim_fit$estimate; optim_fit$sd
# calculate moments of standardized returns
sapply(3:4, moments::moment,
 x=(re_turns - mean(re_turns))/sd(re_turns))
# plot histogram of VTI returns
col_ors <- c("lightgray", "blue", "green", "red")
PerformanceAnalytics::chart.Histogram(re_turns,
  main="", xlim=c(-7, -3), col=col_ors[1:3],
  methods = c("add.density", "add.normal"))
curve(expr=dt((x-optim_fit$estimate[1])/
  optim_fit$estimate[2], df=2)/optim_fit$estimate[2],
type="l", xlab="", ylab="", lwd=2,
col=col_ors[4], add=TRUE)
# add title and legend
title(main="VTI logarithm of range",
cex.main=1.3, line=-1)
legend("topright", inset=0.05,
  legend=c("density", "normal", "t-distr"),
  lwd=2, lty=c(1, 1, 1), col=col_ors[2:4])
# VTI range variance partial autocorrelations
pacf(re_turns^2, lag=10, xlab=NA, ylab=NA,
     main="PACF of VTI log range")
chart_Series(re_turns^2,
       name="VTI log of range squared")
# standard errors of TTR variance estimators using bootstrap
boot_strap <- sapply(1:100, function(x) {
# create random OHLC
  oh_lc <- HighFreq::random_ohlc()
# calculate variance estimate
  sqrt((6.5*60)*mean(na.omit(
    TTR::volatility(oh_lc, N=1,
              calc="yang.zhang"))^2))
})  # end sapply
# analyze bootstrapped variance
head(boot_strap)
sum(is.na(boot_strap))
apply(boot_strap, MARGIN=2, mean)
apply(boot_strap, MARGIN=2, sd)
par(oma=c(1, 1, 1, 1), mar=c(2, 2, 1, 1), mgp=c(0, 0.5, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # set plot panels
# Close variance estimator partial autocorrelations
pacf(var_close, lag=10, xlab=NA, ylab=NA)
title(main="VTI close variance partial autocorrelations")

# Range variance estimator partial autocorrelations
pacf(var_yang_zhang, lag=10, xlab=NA, ylab=NA)
title(main="VTI YZ variance partial autocorrelations")

# Squared range partial autocorrelations
re_turns <- log(rutils::env_etf$VTI[,2] /
            rutils::env_etf$VTI[,3])
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
# add title
title(main="Managers cumulative returns",
line=-1)
library(PerformanceAnalytics)  # load package "PerformanceAnalytics"
data(managers)  # load "managers" data set
charts.PerformanceSummary(ham_1,
  main="", lwd=2, ylog=TRUE)
library(PerformanceAnalytics)  # load package "PerformanceAnalytics"
chart.CumReturns(
  env_etf$re_turns[, c("XLF", "DBC", "IEF")], lwd=2,
  ylab="", legend.loc="topleft", main="")
# add title
title(main="ETF cumulative returns", line=-1)
options(width=200)
library(PerformanceAnalytics)
chart.Drawdown(env_etf$re_turns[, "VTI"], ylab="",
         main="VTI drawdowns")
options(width=200)
library(PerformanceAnalytics)
table.Drawdowns(env_etf$re_turns[, "VTI"])
library(PerformanceAnalytics)
chart.Histogram(env_etf$re_turns[, 1], main="",
  xlim=c(-0.06, 0.06),
  methods = c("add.density", "add.normal"))
# add title
title(main=paste(colnames(env_etf$re_turns[, 1]),
           "density"), line=-1)
library(PerformanceAnalytics)
chart.Boxplot(env_etf$re_turns[,
  c("VTI", "IEF", "IVW", "VYM", "IWB", "DBC", "VXX")])
library(PerformanceAnalytics)
tail(table.Stats(env_etf$re_turns[,
  c("VTI", "IEF", "DBC", "VXX")]), 4)
risk_return <- table.Stats(env_etf$re_turns)
class(risk_return)
# Transpose the data frame
risk_return <- as.data.frame(t(risk_return))
# plot scatterplot
plot(Kurtosis ~ Skewness, data=risk_return,
     main="Kurtosis vs Skewness")
# add labels
text(x=risk_return$Skewness, y=risk_return$Kurtosis,
    labels=rownames(risk_return),
    pos=1, cex=0.8)
# add skew_kurt column
risk_return$skew_kurt <-
  risk_return$Skewness/risk_return$Kurtosis
# sort on skew_kurt
risk_return <- risk_return[
  order(risk_return$skew_kurt,
  decreasing=TRUE), ]
# add names column
risk_return$Name <-
  etf_list[rownames(risk_return), ]$Name
risk_return[, c("Name", "Skewness", "Kurtosis")]
library(PerformanceAnalytics)
chart.RiskReturnScatter(
  env_etf$re_turns[, colnames(env_etf$re_turns)!="VXX"],
  Rf=0.01/12)
library(PerformanceAnalytics)
vti_ief <- env_etf$re_turns[, c("VTI", "IEF")]
SharpeRatio(vti_ief)

SortinoRatio(vti_ief)

CalmarRatio(vti_ief)
tail(table.Stats(vti_ief), 4)
