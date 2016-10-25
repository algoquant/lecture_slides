library(knitr)
opts_chunk$set(prompt=TRUE, eval=FALSE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
options(digits=3)
thm <- knit_theme$get("acid")
knit_theme$set(thm)
# define daily volatility and drift rate
vol_at <- 0.01; dri_ft <- 0.0; len_gth <- 1000
# simulate geometric Brownian motion
set.seed(1121)  # reset random number generator
re_turns <- vol_at*rnorm(len_gth) +
  dri_ft - vol_at^2/2
price_s <- exp(cumsum(re_turns))
x11()
plot(price_s, type="l", xlab="periods", ylab="prices",
     main="geometric Brownian motion")
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
d_free <- c(3, 6, 9)  # df values
# create plot colors
col_ors <- c("black", "red", "blue", "green")
# create legend labels
lab_els <- c("normal", paste("df", d_free, sep="="))
# plot a Normal probability distribution
curve(expr=dnorm, type="l", xlim=c(-4, 4),
      xlab="", ylab="", lwd=2)
for (in_dex in 1:3) {  # plot three curves
curve(expr=dt(x, df=d_free[in_dex]),
      type="l", xlab="", ylab="", lwd=2,
      col=col_ors[in_dex+1], add=TRUE)
}  # end for
# add title
title(main="t-distributions", line=0.5)
# add legend
legend("topright", inset=0.05,
       title="Degrees\n of freedom", lab_els,
       cex=0.8, lwd=2, lty=c(1, 1, 1, 1),
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
re_turns <- rutils::diff_xts(
  log(rutils::env_etf$VTI[,4]))
# initial parameters
par_init <- c(mean=0, scale=0.01)
# fit distribution using optim()
optim_fit <- optim(par=par_init,
  fn=object_ive, # log-likelihood function
  sam_ple=re_turns,
  free_dom=2, # degrees of freedom
  method="L-BFGS-B", # quasi-Newton method
  upper=c(0.1, 0.1), # upper constraint
  lower=c(-0.1, 0.001)) # lower constraint
# optimal parameters
optim_fit$par
# fit distribution using MASS::fitdistr()
optim_fit <-
  MASS::fitdistr(re_turns, densfun="t", df=2)
optim_fit$estimate; optim_fit$sd
par(oma=c(1, 1, 1, 1), mar=c(3, 3, 1, 1), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
x11(7, 7)
# plot histogram of VTI returns
library(PerformanceAnalytics)
# create plot colors
col_ors <- c("lightgray", "blue", "green", "red")
chart.Histogram(re_turns, main="",
  xlim=c(-0.05, 0.05), ylim=c(0, 60), col=col_ors[1:3],
  methods = c("add.density", "add.normal"))
curve(expr=dt((x-optim_fit$estimate[1])/
  optim_fit$estimate[2], df=2)/optim_fit$estimate[2],
type="l", xlab="", ylab="", lwd=2,
col=col_ors[4], add=TRUE)
# add title
title(main="VTI returns histogram", cex.main=1.3, line=-1)
# add legend
lab_els <- c("density", "normal", "t-distr")
legend("topright", inset=0.05, lab_els,
 lwd=2, lty=c(1, 1, 1),
 col=col_ors[2:4])
# extract time index from VTI
in_dex <- index(rutils::env_etf$VTI)
len_gth <- NROW(in_dex)
half_length <- len_gth %/% 2
# simulate returns with random volatility
set.seed(1121)  # reset random number generator
# specify random volatility with sd=1 and sd=4
vol_at <- 0.01*sample(c(rep(1, half_length), rep(4, len_gth-half_length)))
re_turns <- vol_at*rnorm(len_gth) - vol_at^2/2
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
re_turns <- rnorm(1000)
sd(re_turns)
mad(re_turns)
median(abs(re_turns - median(re_turns)))
qnorm(0.75)
# bootstrap of sd and mad estimators
boot_strap <- sapply(1:10000, function(x) {
  boot_sample <-
    re_turns[sample.int(len_gth, replace=TRUE)]
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
boot_strap <- do.call(rbind, boot_strap)
head(boot_strap)
sum(is.na(boot_strap))
# means and standard errors from bootstrap
apply(boot_strap, MARGIN=2,
function(x) c(mean=mean(x), std_error=sd(x)))
library(HighFreq)  # load HighFreq
# minutely SPY returns (unit per minute) single day
re_turns <- rutils::diff_xts(log(SPY["2012-02-13", 4]))
# minutely SPY volatility
sd(re_turns)
# minutely SPY returns (unit per second)
re_turns <- rutils::diff_xts(log(SPY["2012-02-13", 4])) / 
  c(1, diff(.index(SPY["2012-02-13"])))
# minutely SPY volatility scaled to daily frequency
60*sd(re_turns)
# minutely SPY returns multiple days no overnight scaling
re_turns <- rutils::diff_xts(log(SPY[, 4]))
# minutely SPY volatility
sd(re_turns)
# minutely SPY returns (unit per second)
re_turns <- rutils::diff_xts(log(SPY[, 4])) / 
  c(1, diff(.index(SPY)))
# minutely SPY volatility scaled to daily frequency
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
# minutely SPY volatility scaled to daily frequency
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
var_running <-
  252*(24*60*60)^2*merge(var_close, var_yang_zhang)
colnames(var_running) <-
  c("close var", "Yang-Zhang var")
# plot
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("black", "red")
x11()
chart_Series(var_running["2011-06/2011-12"],
  theme=plot_theme, name="Close and YZ variances")
legend("top", legend=colnames(var_running),
 bg="white", lty=c(1, 1), lwd=c(2, 2),
 col=plot_theme$col$line.col, bty="n")
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
library(HighFreq)  # load HighFreq
# calculate variance at each point in time
var_running <- 252*(24*60*60)^2*
  HighFreq::run_variance(oh_lc=env_etf$VTI)
# calculate average realized VTI variance using rutils
win_dow <- 31
var_avg <- rutils::roll_sum(var_running,
            win_dow=win_dow)/win_dow
# calculate EWMA VTI variance using RcppRoll
library(RcppRoll)  # load RcppRoll
weight_s <- exp(0.1*1:win_dow)
var_ewma <- RcppRoll::roll_mean(var_running,
    align="left", n=win_dow, weights=weight_s)
var_ewma <- xts(var_ewma,
    order.by=index(env_etf$VTI[-(1:(win_dow-1)), ]))
colnames(var_ewma) <- "VTI.var_ewma"
# plot average and EWMA variances with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("black", "red")
x11()
chart_Series(merge(var_avg, var_ewma)["2012"],
       theme=plot_theme, name="VTI variances")
legend("top", legend=c(colnames(var_avg),
                 colnames(var_ewma)),
 bg="white", lty=c(1, 1), lwd=c(2, 2),
 col=plot_theme$col$line.col, bty="n")
# plot EWMA variance with prices
x11()
chart_Series(env_etf$VTI["2010-01/2010-10"],
   name="VTI EWMA variance with May 6, 2010 Flash Crash")
# add variance in extra panel
add_TA(var_ewma["2010-01/2010-10"], col="black")
library(HighFreq)  # load HighFreq
# calculate variance at each point in time
var_running <- 252*(24*60*60)^2*
  HighFreq::run_variance(oh_lc=env_etf$VTI)
# calculate EWMA VTI variance using RcppRoll
library(RcppRoll)  # load RcppRoll
win_dow <- 31
weight_s <- exp(0.1*1:win_dow)
var_ewma <- RcppRoll::roll_mean(var_running,
    align="left", n=win_dow, weights=weight_s)
var_ewma <- xts(var_ewma,
    order.by=index(env_etf$VTI[-(1:(win_dow-1)), ]))
colnames(var_ewma) <- "VTI variance"
# plot EWMA variance with custom line colors
x11()
chart_Series(env_etf$VTI["2010-01/2010-10"],
   name="VTI EWMA variance with May 6, 2010 Flash Crash")
# add variance in extra panel
add_TA(var_ewma["2010-01/2010-10"], col="black")
