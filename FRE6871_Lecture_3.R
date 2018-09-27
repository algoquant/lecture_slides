library(parallel)  # load package parallel
# get short description
packageDescription("parallel")
# load help page
help(package="parallel")
# list all objects in "parallel"
ls("package:parallel")
library(parallel)  # load package parallel
# calculate number of available cores
num_cores <- detectCores() - 1
# define function that pauses execution
paws <- function(x, sleep_time) {
  Sys.sleep(sleep_time)
  x
}  # end paws
# perform parallel loop under Mac-OSX or Linux
paw_s <- mclapply(1:10, paws, mc.cores=num_cores,
          sleep_time=0.01)
# initialize compute cluster under Windows
clus_ter <- makeCluster(num_cores)
# perform parallel loop under Windows
paw_s <- parLapply(clus_ter, 1:10, paws,
           sleep_time=0.01)
library(microbenchmark)  # load package microbenchmark
# compare speed of lapply versus parallel computing
summary(microbenchmark(
  l_apply=lapply(1:10, paws, sleep_time=0.01),
  parl_apply=
    parLapply(clus_ter, 1:10, paws, sleep_time=0.01),
  times=10)
)[, c(1, 4, 5)]
# stop R processes over cluster under Windows
stopCluster(clus_ter)
library(parallel)  # load package parallel
# calculate number of available cores
num_cores <- detectCores() - 1
# initialize compute cluster under Windows
clus_ter <- makeCluster(num_cores)
# define function that pauses execution
paws <- function(x, sleep_time) {
  Sys.sleep(sleep_time)
  x
}  # end paws
# compare speed of lapply with parallel computing
iter_ations <- 3:10
compute_times <- sapply(iter_ations,
  function(max_iterations, sleep_time) {
    out_put <- summary(microbenchmark(
lapply=lapply(1:max_iterations, paws,
              sleep_time=sleep_time),
parallel=parLapply(clus_ter, 1:max_iterations,
        paws, sleep_time=sleep_time),
times=10))[, c(1, 4)]
    structure(out_put[, 2],
        names=as.vector(out_put[, 1]))
    }, sleep_time=0.01)
compute_times <- t(compute_times)
rownames(compute_times) <- iter_ations
library(parallel)  # load package parallel
plot(x=rownames(compute_times),
     y=compute_times[, "lapply"],
     type="l", lwd=2, col="blue",
     main="Compute times",
     xlab="number of iterations in loop", ylab="",
     ylim=c(0, max(compute_times[, "lapply"])))
lines(x=rownames(compute_times),
y=compute_times[, "parallel"], lwd=2, col="green")
legend(x="topleft", legend=colnames(compute_times),
 inset=0.1, cex=1.0, bg="white",
 lwd=2, lty=c(1, 1), col=c("blue", "green"))
library(parallel)  # load package parallel
# calculate number of available cores
num_cores <- detectCores() - 1
# initialize compute cluster under Windows
clus_ter <- makeCluster(num_cores)
# define large matrix
mat_rix <- matrix(rnorm(7*10^5), ncol=7)
# define aggregation function over column of matrix
agg_regate <- function(col_umn) {
  out_put <- 0
  for (in_dex in 1:NROW(col_umn))
    out_put <- out_put + col_umn[in_dex]
  out_put
}  # end agg_regate
# perform parallel aggregations over columns of matrix
agg_regations <-
  parCapply(clus_ter, mat_rix, agg_regate)
# compare speed of apply with parallel computing
summary(microbenchmark(
  ap_ply=apply(mat_rix, MARGIN=2, agg_regate),
  parl_apply=
    parCapply(clus_ter, mat_rix, agg_regate),
  times=10)
)[, c(1, 4, 5)]
# stop R processes over cluster under Windows
stopCluster(clus_ter)
library(parallel)  # load package parallel
# calculate number of available cores
num_cores <- detectCores() - 1
# initialize compute cluster under Windows
clus_ter <- makeCluster(num_cores)
ba_se <- 2
# fails because child processes don't know ba_se:
parLapply(clus_ter, 2:4,
    function(exponent) ba_se^exponent)
# ba_se passed to child via dots ... argument:
parLapply(clus_ter, 2:4,
    function(exponent, ba_se) ba_se^exponent,
    ba_se=ba_se)
# ba_se passed to child via clusterExport:
clusterExport(clus_ter, "ba_se")
parLapply(clus_ter, 2:4,
    function(exponent) ba_se^exponent)
# fails because child processes don't know zoo::index():
parSapply(clus_ter, c("VTI", "IEF", "DBC"),
    function(sym_bol)
      NROW(index(get(sym_bol, envir=rutils::etf_env))))
# zoo function referenced using "::" in child process:
parSapply(clus_ter, c("VTI", "IEF", "DBC"),
    function(sym_bol)
      NROW(zoo::index(get(sym_bol, envir=rutils::etf_env))))
# package zoo loaded in child process:
parSapply(clus_ter, c("VTI", "IEF", "DBC"),
    function(sym_bol) {
      stopifnot("package:zoo" %in% search() || require("zoo", quietly=TRUE))
      NROW(index(get(sym_bol, envir=rutils::etf_env)))
    })  # end parSapply
# stop R processes over cluster under Windows
stopCluster(clus_ter)
library(parallel)  # load package parallel
# calculate number of available cores
num_cores <- detectCores() - 1
# initialize compute cluster under Windows
clus_ter <- makeCluster(num_cores)
# set seed for cluster under Windows
# doesn't work: set.seed(1121)
clusterSetRNGStream(clus_ter, 1121)
# perform parallel loop under Windows
out_put <- parLapply(clus_ter, 1:70, rnorm, n=100)
sum(unlist(out_put))
# stop R processes over cluster under Windows
stopCluster(clus_ter)
# perform parallel loop under Mac-OSX or Linux
out_put <- mclapply(1:10, rnorm, mc.cores=num_cores, n=100)
set.seed(1121)  # reset random number generator
# sample from Standard Normal Distribution
len_gth <- 1000
sam_ple <- rnorm(len_gth)
# sample mean - MC estimate
mean(sam_ple)
# sample standard deviation - MC estimate
sd(sam_ple)
# Monte Carlo estimate of cumulative probability
sam_ple <- sort(sam_ple)
pnorm(1)
sum(sam_ple<1)/len_gth
# Monte Carlo estimate of quantile
conf_level <- 0.99
qnorm(conf_level)
cut_off <- conf_level*len_gth
sam_ple[cut_off]
quantile(sam_ple, probs=conf_level)
# analyze the source code of quantile()
stats:::quantile.default
# microbenchmark quantile
library(microbenchmark)
summary(microbenchmark(
  sam_ple=sam_ple[cut_off],
  quan_tile=quantile(sam_ple, probs=conf_level),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
x11(width=6, height=5)
par(oma=c(1, 1, 1, 1), mar=c(2, 2, 2, 1), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
set.seed(1121)  # reset random number generator
bar_rier <- 20  # barrier level
len_gth <- 1000  # number of simulation steps
pa_th <- numeric(len_gth)  # allocate path vector
pa_th[1] <- 0  # initialize path
in_dex <- 2  # initialize simulation index
while ((in_dex <= len_gth) &&
 (pa_th[in_dex - 1] < bar_rier)) {
# simulate next step
  pa_th[in_dex] <-
    pa_th[in_dex - 1] + rnorm(1)
  in_dex <- in_dex + 1  # advance in_dex
}  # end while
# fill remaining pa_th after it crosses bar_rier
if (in_dex <= len_gth)
  pa_th[in_dex:len_gth] <- pa_th[in_dex - 1]
# create daily time series starting 2011
ts_path <- ts(data=pa_th, frequency=365, start=c(2011, 1))
plot(ts_path, type="l", col="black",
     lty="solid", lwd=2, xlab="", ylab="")
abline(h=bar_rier, lwd=2, col="red")
title(main="Brownian motion crossing a barrier level",
      line=0.5)
x11(width=6, height=5)
par(oma=c(1, 1, 1, 1), mar=c(2, 2, 2, 1), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
set.seed(1121)  # reset random number generator
bar_rier <- 20  # barrier level
len_gth <- 1000  # number of simulation steps
# simulate path of Brownian motion
pa_th <- cumsum(rnorm(len_gth))
# find index when pa_th crosses bar_rier
cro_ss <- which(pa_th > bar_rier)
# fill remaining pa_th after it crosses bar_rier
if (NROW(cro_ss)>0) {
  pa_th[(cro_ss[1]+1):len_gth] <-
    pa_th[cro_ss[1]]
}  # end if
# create daily time series starting 2011
ts_path <- ts(data=pa_th, frequency=365,
     start=c(2011, 1))
# create plot with horizontal line
plot(ts_path, type="l", col="black",
     lty="solid", lwd=2, xlab="", ylab="")
abline(h=bar_rier, lwd=2, col="red")
title(main="Brownian motion crossing a barrier level",
      line=0.5)
set.seed(1121)  # reset random number generator
# sample from Standard Normal Distribution
len_gth <- 1000
sam_ple <- rnorm(len_gth)
# sample mean
mean(sam_ple)
# sample standard deviation
sd(sam_ple)
# bootstrap of sample mean and median
boot_strap <- sapply(1:10000, function(x) {
  boot_sample <- sam_ple[sample.int(len_gth,
                              replace=TRUE)]
  c(mean=mean(boot_sample),
    median=median(boot_sample))
})  # end sapply
boot_strap[, 1:3]
# standard error from formula
sd(sam_ple)/sqrt(len_gth)
# standard error of mean from bootstrap
sd(boot_strap["mean", ])
# standard error of median from bootstrap
sd(boot_strap["median", ])
library(parallel)  # load package parallel
num_cores <- detectCores() - 1  # number of cores
clus_ter <- makeCluster(num_cores)  # initialize compute cluster under Windows
set.seed(1121)  # reset random number generator
# sample from Standard Normal Distribution
len_gth <- 1000
sam_ple <- rnorm(len_gth)
# bootstrap mean and median under Windows
boot_strap <- parLapply(clus_ter, 1:10000,
  function(x, sam_ple, len_gth) {
  boot_sample <- sam_ple[sample.int(len_gth, replace=TRUE)]
  c(mean=mean(boot_sample), median=median(boot_sample))
  }, sam_ple=sam_ple, len_gth=len_gth)  # end parLapply
# bootstrap mean and median under Mac-OSX or Linux
boot_strap <- mclapply(1:10000,
  function(x) {
  boot_sample <- sam_ple[sample.int(len_gth, replace=TRUE)]
  c(mean=mean(boot_sample), median=median(boot_sample))
  }, mc.cores=num_cores)  # end mclapply
boot_strap <- rutils::do_call(rbind, boot_strap)
# means and standard errors from bootstrap
apply(boot_strap, MARGIN=2,
function(x) c(mean=mean(x), sd=sd(x)))
# standard error from formula
sd(sam_ple)/sqrt(len_gth)
stopCluster(clus_ter)  # stop R processes over cluster under Windows
# sample from time series of ETF returns
sam_ple <- rutils::etf_env$re_turns[, "VTI"]
sam_ple <- na.omit(sam_ple)
len_gth <- NROW(sam_ple)
# bootstrap mean and median under Windows
library(parallel)  # load package parallel
num_cores <- detectCores() - 1  # number of cores
clus_ter <- makeCluster(num_cores)  # initialize compute cluster under Windows
clusterSetRNGStream(clus_ter, 1121)  # reset random number generator in all cores
num_boot <- 1e4
boot_strap <- parLapply(clus_ter, 1:num_boot,
  function(x, sam_ple, len_gth) {
    boot_sample <- sam_ple[sample.int(len_gth, replace=TRUE)]
    c(sd=sd(boot_sample), mad=mad(boot_sample))
  }, sam_ple=sam_ple, len_gth=len_gth)  # end parLapply
# bootstrap mean and median under Mac-OSX or Linux
boot_strap <- mclapply(1:num_boot,
  function(x) {
    boot_sample <- sam_ple[sample.int(len_gth, replace=TRUE)]
    c(mean=mean(boot_sample), median=median(boot_sample))
  }, mc.cores=num_cores)  # end mclapply
boot_strap <- rutils::do_call(rbind, boot_strap)
# means and standard errors from bootstrap
apply(boot_strap, MARGIN=2,
function(x) c(mean=mean(x), sd=sd(x)))
# standard error assuming normal distribution of returns
sd(sam_ple)/sqrt(num_boot)
stopCluster(clus_ter)  # stop R processes over cluster under Windows
# load time series of ETF prices
sam_ple <- rutils::etf_env$VTI[, 4]
len_gth <- NROW(sam_ple)
# calculate percentage returns
sam_ple <- rutils::diff_it(log(sam_ple))
# define barrier level with respect to sam_ple
bar_rier <- 0.8*max(exp(cumsum(sam_ple)))
library(parallel)  # load package parallel
num_cores <- detectCores() - 1  # number of cores
clus_ter <- makeCluster(num_cores)  # initialize compute cluster under Windows
num_boot <- 1e4
# perform parallel bootstrap under Windows
set.seed(1121)  # reset random number generator
boot_strap <- parLapply(clus_ter, 1:num_boot,
  function(x, sam_ple, len_gth, bar_rier) {
    boot_sample <- sam_ple[sample.int(len_gth, replace=TRUE)]
    # calculate prices from percentage returns
    boot_sample <- exp(cumsum(boot_sample))
    # calculate payout
    sum(boot_sample > bar_rier) > 0
  }, sam_ple=sam_ple, len_gth=len_gth, bar_rier=bar_rier)  # end parLapply
stopCluster(clus_ter)  # stop R processes over cluster under Windows
# perform parallel bootstrap under Mac-OSX or Linux
boot_strap <- mclapply(1:num_boot,
  function(x) {
    boot_sample <- sam_ple[sample.int(len_gth, replace=TRUE)]
    # calculate prices from percentage returns
    boot_sample <- exp(cumsum(boot_sample))
    # calculate payout
    sum(boot_sample > bar_rier) > 0
  }, mc.cores=num_cores)  # end mclapply
boot_strap <- rutils::do_call(rbind, boot_strap)
# calculate probability of crossing barrier
sum(boot_strap)/num_boot
set.seed(1121)  # reset random number generator
# sample from Standard Normal Distribution
len_gth <- 1000
sam_ple <- rnorm(len_gth)
# estimate the 95% quantile
boot_strap <- sapply(1:10000, function(x) {
  boot_sample <- sam_ple[sample.int(len_gth,
                              replace=TRUE)]
  quantile(boot_sample, 0.95)
})  # end sapply
sd(boot_strap)
# estimate the 95% quantile using antithetic sampling
boot_strap <- sapply(1:10000, function(x) {
  boot_sample <- sam_ple[sample.int(len_gth,
                              replace=TRUE)]
  quantile(c(boot_sample, -boot_sample), 0.95)
})  # end sapply
# standard error of quantile from bootstrap
sd(boot_strap)
sqrt(2)*sd(boot_strap)
# create factor vector
fac_tor <- factor(c('b', 'c', 'd', 'a', 'c', 'b'))
fac_tor
fac_tor[3]
attributes(fac_tor)  # get factor attributes
levels(fac_tor)  # get allowed values
as.numeric(fac_tor)  # get encoding vector
is.vector(fac_tor)
as.factor(1:5)  # coerce vector to factor
# coerce factor to character vector
as.vector(as.factor(1:5))
fac_tor
levels(fac_tor)  # get allowed values
unique(fac_tor)  # get unique elements
# get contingency (frequency) table
table(fac_tor)
# get contingency table using sapply
sapply(levels(fac_tor),
 function(le_vel) {
   sum(fac_tor==le_vel)
 })  # end sapply
library(microbenchmark)
str(findInterval)
# get index of the element of "vec" that matches 5
findInterval(x=5, vec=c(3, 5, 7))
match(5, c(3, 5, 7))
# no exact match
findInterval(x=6, vec=c(3, 5, 7))
match(6, c(3, 5, 7))
# indices of "vec" that match elements of "x"
findInterval(x=1:8, vec=c(3, 5, 7))
# return only indices of inside intervals
findInterval(x=1:8, vec=c(3, 5, 7),
       all.inside=TRUE)
# make rightmost interval inclusive
findInterval(x=1:8, vec=c(3, 5, 7),
       rightmost.closed=TRUE)
# named numeric vector of breakpoints
brea_ks <- c(freezing=0, very_cold=30,
       cold=50, pleasant=60,
       warm=80, hot=90)
brea_ks
tempe_ratures <- runif(10, min=10, max=100)
feels_like <- names(
  brea_ks[findInterval(x=tempe_ratures,
                 vec=brea_ks)])
names(tempe_ratures) <- feels_like
tempe_ratures
library(microbenchmark)
da_ta <- sample(0:6) + 0.1
da_ta
cut(x=da_ta, breaks=c(2, 4, 6, 8))
rbind(da_ta, cut(x=da_ta, breaks=c(2, 4, 6, 8)))
# cut() replicates findInterval()
cut(x=1:8, breaks=c(3, 5, 7), labels=1:2,
    right=FALSE)
findInterval(x=1:8, vec=c(3, 5, 7))
# findInterval() is a compiled function, so it's faster than cut()
vec_tor <- rnorm(1000)
summary(microbenchmark(
  find_interval=
    findInterval(x=vec_tor, vec=c(3, 5, 7)),
  cuut=
    cut(x=vec_tor, breaks=c(3, 5, 7)),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# calculate DAX percentage returns
dax_rets <- diff(log(EuStockMarkets[, 1]))
# plot histogram
par(mar=c(1, 1, 1, 1), oma=c(2, 2, 2, 0))
histo_gram <- hist(dax_rets, breaks=30,
  main="", ylim=c(0, 60), xlim=c(-0.04, 0.04),
  xlab="", ylab="", freq=FALSE)
# draw kernel density of histogram
lines(density(dax_rets), col='red', lwd=2)
# add density of normal distribution
curve(expr=dnorm(x, mean=mean(dax_rets),
  sd=sd(dax_rets)), add=TRUE, type="l",
  lwd=2, col="blue")
title(main="DAX return distribution", line=0)
# add legend
legend("topright", inset=0.05, cex=0.8, title=NULL,
  leg=c(colnames(EuStockMarkets)[1], "Normal"),
  lwd=6, bg="white", col=c("red", "blue"))
# total area under histogram
diff(histo_gram$breaks) %*% histo_gram$density
# calculate random default probabilities
num_assets <- 100
default_probs <- runif(num_assets, max=0.2)
mean(default_probs)
# calculate number of defaults
uni_form <- runif(num_assets)
sum(uni_form < default_probs)
# simulate average number of defaults
num_simu <- 1000
de_faults <- numeric(num_simu)
# simulate using for() loop (inefficient way)
for (i in 1:num_simu) {  # perform loop
  uni_form <- runif(num_assets)
  de_faults[i] <- sum(uni_form < default_probs)
}  # end for
# calculate average number of defaults
mean(de_faults)
# simulate using vectorized functions  (efficient way)
uni_form <- matrix(runif(num_simu*num_assets),
             ncol=num_simu)
sum(uni_form < default_probs)/num_simu
# plot Standard Normal distribution
curve(expr=dnorm(x),
type="l", xlim=c(-4, 4),
xlab="asset value", ylab="", lwd=2,
col="blue", main="Distribution of Asset Values")
abline(v=qnorm(0.025), col="red", lwd=2)
text(x=qnorm(0.025)-0.1, y=0.15,
 labels="default threshold",
 lwd=2, srt=90, pos=3)
# define correlation parameters
rh_o <- 0.2
rho_sqrt <- sqrt(rh_o) ; rho_sqrtm <- sqrt(1-rh_o)
num_assets <- 5 ; num_simu <- 10000
# calculate vector of systematic factors
system_atic <- rnorm(num_simu)
# simulate asset values using vectorized functions (efficient way)
asset_values <- rho_sqrt*system_atic +
  rho_sqrtm*rnorm(num_simu*num_assets)
dim(asset_values) <- c(num_simu, num_assets)
# calculate correlations between asset values
cor(asset_values)
# simulate asset values using for() loop (inefficient way)
# allocate matrix of assets
asset_values <- matrix(nr=num_simu, nc=num_assets)
# simulate asset values using for() loop
for (i in 1:num_simu) {  # perform loop
  asset_values[i, ] <-
    rho_sqrt*system_atic[i] +
    rho_sqrtm*rnorm(num_assets)
}  # end for
cor(asset_values)
# benchmark the speed of the two methods
library(microbenchmark)
summary(microbenchmark(
  for_loop={for (i in 1:num_simu) {
    rho_sqrt*system_atic[i] +
    rho_sqrtm*rnorm(num_assets)}},
  vector_ized={rho_sqrt*system_atic +
        rho_sqrtm*rnorm(num_simu*num_assets)},
  times=10))[, c(1, 4, 5)]
# calculate random default probabilities
num_assets <- 5
default_probs <- runif(num_assets, max=0.2)
mean(default_probs)
# calculate default thresholds
default_thresh <- qnorm(default_probs)
# calculate number of defaults using vectorized functions (efficient way)
# calculate vector of number of defaults
de_faults <-
  colSums(t(t(asset_values) < default_thresh))
de_faults / num_simu
default_probs
# calculate number of defaults using for() loop (inefficient way)
# allocate matrix of de_faults
de_faults <- matrix(nr=num_simu, nc=num_assets)
# simulate asset values using for() loop
for (i in 1:num_simu) {  # perform loop
  de_faults[i, ] <-
    (asset_values[i, ] < default_thresh)
}  # end for
colSums(de_faults) / num_simu
default_probs
# calculate correlations between defaults
cor(de_faults)
# define default probabilities
num_assets <- 2
default_prob <- 0.2
default_thresh <- qnorm(default_prob)
# define correlation parameters
rh_o <- 0.2
rho_sqrt <- sqrt(rh_o) ; rho_sqrtm <- sqrt(1-rh_o)
# calculate vector of systematic factors
num_simu <- 1000
system_atic <- rnorm(num_simu)
# simulate asset values using vectorized functions
asset_values <- rho_sqrt*system_atic +
  rho_sqrtm*rnorm(num_simu*num_assets)
dim(asset_values) <- c(num_simu, num_assets)
# calculate number of defaults using vectorized functions
de_faults <- t(t(asset_values) < default_thresh)
# calculate correlations between defaults
cor(de_faults)
# calculate average number of defaults and compare to default_prob
colSums(de_faults) / num_simu
default_prob
# define cumulative default probability function
def_prob <- function(x, def_thresh=qnorm(0.1), rh_o=0.1)
  pnorm((sqrt(1-rh_o)*qnorm(x) - def_thresh)/sqrt(rh_o))
def_prob(x=0.2, def_thresh=qnorm(0.2), rh_o=0.2)
# plot cumulative default probability function
curve(expr=def_prob(x, def_thresh=qnorm(0.4), rh_o=0.05),
xlim=c(0, 0.999), lwd=3,
xlab="percent default", ylab="probability",
col="green", main="Cumulative Default Probabilities")
# plot default distribution with higher correlation
curve(expr=def_prob(x, def_thresh=qnorm(0.4), rh_o=0.2),
xlim=c(0, 0.999), add=TRUE, lwd=3,
col="blue", main="")
# add legend
legend(x="topleft",
 legend=c("high correlation", "low correlation"),
 title=NULL, inset=0.05, cex=0.8, bg="white",
 bty="n", lwd=6, lty=1, col=c("blue", "green"))
# add unconditional default probability
abline(v=0.4, col="red", lwd=3)
text(x=0.4, y=0.0,
 labels="default probability",
 lwd=2, srt=90, pos=4)
# define default probability density function
vasi_cek <- function(x, def_thresh=-2, rh_o=0.1)
  sqrt((1-rh_o)/rh_o)*exp(-(sqrt(1-rh_o)*qnorm(x) -
  def_thresh)^2/(2*rh_o) + qnorm(x)^2/2)
vasi_cek(0.03, def_thresh=qnorm(0.025), rh_o=0.1)
# plot probability distribution of defaults
curve(expr=vasi_cek(x, def_thresh=qnorm(0.025), rh_o=0.02),
xlim=c(0, 0.1), lwd=3,
xlab="percentage of defaults", ylab="density",
col="green", main="Distribution of Defaults")
# plot default distribution with higher correlation
curve(expr=vasi_cek(x, def_thresh=qnorm(0.025), rh_o=0.1),
xlab="default percentage", ylab="",
add=TRUE, lwd=3, col="blue", main="")
# add legend
legend(x="topright",
 legend=c("high correlation", "low correlation"),
 title=NULL, inset=0.05, cex=0.8, bg="white",
 bty="n", lwd=6, lty=1, col=c("blue", "green"))
# add unconditional default probability
abline(v=0.025, col="red", lwd=3)
text(x=0.023, y=8,
 labels="default probability",
 lwd=2, srt=90, pos=3)
# plot default distribution with low correlation
curve(expr=vasi_cek(x, def_thresh=qnorm(0.1), rh_o=0.01),
xlab="default percentage", ylab="", lwd=2,
col="green", main="Distribution of Defaults")
# plot default distribution with high correlation
curve(expr=vasi_cek(x, def_thresh=qnorm(0.1), rh_o=0.99),
xlab="percentage of defaults", ylab="density",
add=TRUE, lwd=2, n=10001, col="blue", main="")
# add legend
legend(x="top",
 legend=c("high correlation", "low correlation"),
 title=NULL, inset=0.1, cex=0.8, bg="white",
 bty="n", lwd=6, lty=1, col=c("blue", "green"))
# add unconditional default probability
abline(v=0.1, col="red", lwd=2)
text(x=0.1, y=10, lwd=2, pos=4,
 labels="default probability")
# define Vasicek loss distribution density function
portf_loss <- function(x, def_thresh=-2, rh_o=0.1, l_gd=0.4)
  sqrt((1-rh_o)/rh_o)*exp(-(sqrt(1-rh_o)*qnorm(x/l_gd) - def_thresh)^2/(2*rh_o) + qnorm(x/l_gd)^2/2)/l_gd
integrate(portf_loss, low=0, up=0.3,
  def_thresh=-2, rh_o=0.1, l_gd=0.4)
# plot probability distribution of losses
curve(expr=portf_loss(x, def_thresh=qnorm(0.06), rh_o=0.1),
type="l", xlim=c(0, 0.06),
xlab="loss percentage", ylab="density", lwd=3,
col="orange", main="Distribution of Losses")
# add line for expected loss
abline(v=0.02, col="red", lwd=3)
text(x=0.02-0.001, y=10, labels="expected loss",
 lwd=2, srt=90, pos=3)
# add lines for unexpected loss
abline(v=0.04, col="blue", lwd=3)
arrows(x0=0.02, y0=35, x1=0.04, y1=35,
 code=3, lwd=3, cex=0.5)
text(x=0.03, y=36, labels="unexpected loss",
     lwd=2, pos=3)
# add lines for VaR
abline(v=0.055, col="red", lwd=3)
arrows(x0=0.0, y0=25, x1=0.055, y1=25,
 code=3, lwd=3, cex=0.5)
text(x=0.03, y=26, labels="VaR", lwd=2, pos=3)
text(x=0.055-0.001, y=10, labels="VaR",
 lwd=2, srt=90, pos=3)
# plot probability distribution of losses
curve(expr=portf_loss(x, def_thresh=qnorm(0.1), rh_o=0.1),
type="l", xlim=c(0, 0.06),
xlab="loss percentage", ylab="density", lwd=3,
col="orange", main="Conditional Value at Risk")
# add line for expected loss
abline(v=0.02, col="red", lwd=3)
text(x=0.02-0.001, y=10, labels="expected loss",
 lwd=2, srt=90, pos=3)
# add lines for VaR
abline(v=0.04, col="red", lwd=3)
text(x=0.04-0.001, y=10, labels="VaR",
 lwd=2, srt=90, pos=3)
# add shading for CVaR
va_r <- 0.04; var_max <- 0.07
var_s <- seq(va_r, var_max, length=100)
dens_ity <- sapply(var_s, portf_loss,
  def_thresh=qnorm(0.1), rh_o=0.1)
# draw shaded polygon
polygon(c(va_r, var_s, var_max),
  c(-1, dens_ity, -1), col="red", border=NA)
text(x=0.045, y=0, labels="CVaR", lwd=2, pos=3)
# VaR (quantile of the loss distribution)
var_func <- function(x, def_thresh=qnorm(0.1), rh_o=0.1, l_gd=0.4)
  l_gd*pnorm((sqrt(rh_o)*qnorm(x) + def_thresh)/sqrt(1-rh_o))
var_func(x=0.99, def_thresh=qnorm(0.1), rh_o=0.2, l_gd=0.4)
# plot VaR
curve(expr=var_func(x, def_thresh=qnorm(0.1), rh_o=0.1, l_gd=0.4),
type="l", xlim=c(0, 0.999),
xlab="confidence level", ylab="VaR", lwd=3,
col="orange", main="VaR versus Confidence Level")
# add line for expected loss
abline(h=0.04, col="red", lwd=3)
text(x=0.2, y=0.04, labels="expected loss",
     lwd=2, pos=3)
# integrate portf_loss() over full range
integrate(portf_loss, low=0.0, up=0.3,
    def_thresh=qnorm(0.1), rh_o=0.1, l_gd=0.4)
# calculate expected losses using portf_loss()
integrate(function(x, ...) x*portf_loss(x, ...),
    low=0.0, up=0.3,
    def_thresh=qnorm(0.1), rh_o=0.1, l_gd=0.4)
# calculate confidence levels corresponding to VaR values
var_s <- seq(0.07, 0.12, 0.001)
conf_levels <- sapply(var_s, function(va_r, ...) {
  integrate(portf_loss, low=va_r, up=0.3, ...)
}, def_thresh=qnorm(0.1), rh_o=0.1, l_gd=0.4)  # end sapply
conf_levels <- cbind(as.numeric(t(conf_levels)[, 1]), var_s)
colnames(conf_levels) <- c("conf_levels", "VaRs")
# calculate 95% confidence level VaR value
conf_levels[
  match(TRUE, conf_levels[, "conf_levels"] < 0.05), "VaRs"]
plot(x=1-conf_levels[, "conf_levels"],
     y=conf_levels[, "VaRs"], lwd=2,
     xlab="conf_levels", ylab="VaRs",
     t="l", main="VaR values and confidence levels")
# calculate CVaR values
cvar_s <- sapply(var_s, function(va_r, ...) {
  integrate(function(x, ...) x*portf_loss(x, ...),
      low=va_r, up=0.3, ...)
}, def_thresh=qnorm(0.1), rh_o=0.1, l_gd=0.4)  # end sapply
conf_levels <- cbind(conf_levels, as.numeric(t(cvar_s)[, 1]))
colnames(conf_levels)[3] <- "CVaRs"
# divide CVaR by confidence level
conf_levels[, "CVaRs"] <-
  conf_levels[, "CVaRs"]/conf_levels[, "conf_levels"]
# calculate 95% confidence level CVaR value
conf_levels[match(TRUE,
  conf_levels[, "conf_levels"] < 0.05), "CVaRs"]
# plot CVaRs
plot(x=1-conf_levels[, "conf_levels"],
     y=conf_levels[, "CVaRs"],
     t="l", col="red", lwd=2,
     ylim=range(conf_levels[, c("VaRs", "CVaRs")]),
     xlab="conf_levels", ylab="CVaRs",
     main="CVaR values and confidence levels")
# add VaRs
lines(x=1-conf_levels[, "conf_levels"],
y=conf_levels[, "VaRs"], lwd=2)
# add legend
legend(x="topleft", legend=c("CVaRs", "VaRs"),
 title="default probability = 10%
correlation = 10%
loss given default = 40%",
 inset=0.1, cex=0.8, bg="white", bty="n",
 lwd=6, lty=1, col=c("red", "black"))
# Define model parameters
num_assets <- 300
num_simu <- 1000
l_gd <- 0.4
# define correlation parameters
rh_o <- 0.2
rho_sqrt <- sqrt(rh_o)
rho_sqrtm <- sqrt(1-rh_o)
# calculate default probabilities and thresholds
set.seed(1121)
default_probs <- runif(num_assets, max=0.2)
default_thresh <- qnorm(default_probs)
# calculate vector of systematic factors
system_atic <- rnorm(num_simu)
# simulate losses under Vasicek model
asset_values <- matrix(rnorm(num_simu*num_assets), ncol=num_simu)
asset_values <- t(rho_sqrt*system_atic + t(rho_sqrtm*asset_values))
loss_es <-
  l_gd*colSums(asset_values < default_thresh)/num_assets
# calculate VaRs
conf_levels <- seq(0.93, 0.99, 0.01)
var_s <- quantile(loss_es, probs=conf_levels)
plot(x=conf_levels, y=var_s, t="l", lwd=2,
     main="Simulated VaR and confidence levels")
# calculate CVaRs
cvar_s <- sapply(var_s, function(va_r) {
  mean(loss_es[loss_es >=va_r])
})  # end sapply
cvar_s <- cbind(cvar_s, var_s)
# alternative CVaR calculation using frequency table
# first calculate frequency table of loss_es
# table_losses <- table(loss_es)/num_simu
# calculate CVaRs from frequency table
# cvar_s <- sapply(var_s, function(va_r) {
#   tai_l <- table_losses[names(table_losses) > va_r]
#   tai_l %*% as.numeric(names(tai_l)) / sum(tai_l)
# })  # end sapply
# plot CVaRs
plot(x=rownames(cvar_s), y=cvar_s[, "cvar_s"],
     t="l", col="red", lwd=2,
     ylim=range(cvar_s),
     xlab="conf_levels", ylab="CVaRs",
     main="Simulated CVaR and confidence levels")
# add VaRs
lines(x=rownames(cvar_s), y=cvar_s[, "var_s"], lwd=2)
# add legend
legend(x="topleft", legend=c("CVaRs", "VaRs"), bty="n",
 title=NULL, inset=0.05, cex=0.8, bg="white",
 lwd=6, lty=1, col=c("red", "black"))
library(quantmod)
some_cars <- mtcars[sample(NROW(mtcars), 10), ]
# plot scatterplot horsepower vs miles per gallon
plot(some_cars[, "hp"], some_cars[, "mpg"],
     xlab="horsepower", ylab="miles per gallon",
     main="miles per gallon vs horsepower")
# add a solid red point (pch=16) for the last car
points(x=some_cars[NROW(some_cars), "hp"],
 y=some_cars[NROW(some_cars), "mpg"],
 col="red", pch=16)
# add labels with the car names
text(x=some_cars[, "hp"], y=some_cars[, "mpg"],
     labels=rownames(some_cars[, ]),
     pos=1, cex=0.8)
# or add labels using wordcloud, to prevent overlaps
library(wordcloud)
textplot(x=some_cars[, "hp"], y=some_cars[, "mpg"],
   words=rownames(some_cars))
# plot the tree Height
plot(trees[, "Height"],
     type="l",
     lwd=2,
     col="blue",
     main="Tree heights and volumes",
     xlab="tree number", ylab="",
     ylim=c(min(trees[, c("Height", "Volume")]),
      max(trees[, c("Height", "Volume")])))
# plot the tree Volume
lines(trees[, "Volume"], lwd=2, col="green")
# add legend
legend(x="left", legend=c("Height", "Volume"),
 inset=0.1, cex=1.0, bg="white",
 lwd=2, lty=c(1, 1), col=c("blue", "green"))
x_var <- seq(-2*pi, 2*pi, len=100)  # x values

# open Windows graphics device
x11(width=11, height=7, title="simple plot")

# plot a sine function using basic line plot
plot(x=x_var, y=sin(x_var), xlab="x-val",
     ylab="y-val", type="l", lwd=2, col="red")
# add a cosine function
lines(x=x_var, y=cos(x_var), lwd=2, col="blue")
# add title
title(main="sine and cosine functions", line=0.1)
# add legend
legend(x="topright", legend=c("sine", "cosine"),
 title="legend", inset=0.1, cex=1.0, bg="white",
 lwd=2, lty=c(1, 1), col=c("red", "blue"))
graphics.off()  # close all graphics devices
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# plot a Normal probability distribution
curve(expr=dnorm, type="l", xlim=c(-3, 3),
xlab="", ylab="", lwd=2, col="blue")
# add shifted Normal probability distribution
curve(expr=dnorm(x, mean=1), add=TRUE,
type="l", lwd=2, col="red")

# add title
title(main="Normal probability distribution functions",
line=0.1)
# add legend
legend(x="topright", legend=c("Normal", "shifted"),
 title="legend", inset=0.05, cex=0.8, bg="white",
 lwd=2, lty=c(1, 1), col=c("blue", "red"))
graph_params <- par()  # get existing parameters
par("mar")  # get plot margins
par(mar=c(2, 1, 2, 1))  # set plot margins
par(oma=c(1, 1, 1, 1))  # set outer margins
par(mgp=c(2, 1, 0))  # set title and label margins
par(cex.lab=0.8,  # set font scales
    cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(las=1)  # set axis labels to horizontal
par(ask=TRUE)  # pause, ask before plotting
par(mfrow=c(2, 2))  # plot on 2x2 grid by rows
for (i in 1:4) {  # plot 4 panels
  barplot(sample(1:6), main=paste("panel", i),
    col=rainbow(6), border=NA, axes=FALSE)
  box()
}  # end for
par(ask=FALSE)  # restore automatic plotting
par(new=TRUE)  # allow new plot on same chart
par(graph_params)  # restore original parameters
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
rm(list=ls())
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
x_var <- seq(-4, 4, length=100)
sig_mas <- c(0.5, 1, 1.5, 2)  # sigma values
# create plot colors
col_ors <- c("red", "black", "blue", "green")
# create legend labels
lab_els <- paste("sigma", sig_mas, sep="=")
# plot the first chart
plot(x_var, dnorm(x_var, sd=sig_mas[1]),
     type="n", xlab="", ylab="",
     main="Normal Distributions")
# add lines to plot
for (in_dex in 1:4) {
  lines(x_var, dnorm(x_var, sd=sig_mas[in_dex]),
  lwd=2, col=col_ors[in_dex])
}  # end for
# add legend
legend("topright", inset=0.05, title="Sigmas",
 lab_els, cex=0.8, lwd=2, lty=c(1, 1, 1, 1),
 col=col_ors)
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
x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
deg_free <- c(2, 5, 8, 11)  # df values
# create plot colors
col_ors <- c("red", "black", "blue", "green")
# create legend labels
lab_els <- paste("df", deg_free, sep="=")
# plot an empty chart
x_var <- seq(0, 20, length=100)
plot(x_var, dchisq(x_var, df=deg_free[1]),
     type="n", xlab="", ylab="", ylim=c(0, 0.3))
# add lines to plot
for (in_dex in 1:4) {
  lines(x_var, dchisq(x_var, df=deg_free[in_dex]),
lwd=2, col=col_ors[in_dex])
}  # end for
# add title
title(main="Chi-squared Distributions", line=0.5)
# add legend
legend("topright", inset=0.05,
       title="Degrees of freedom", lab_els,
       cex=0.8, lwd=6, lty=c(1, 1, 1, 1),
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
x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
x_var <- seq(-4, 4, length=100)
deg_free <- c(3, 6, 9)  # df values
col_ors <- c("black", "red", "blue", "green")
lab_els <- c("normal", paste("df", deg_free, sep="="))
# plot chart of normal distribution
plot(x_var, dnorm(x_var), type="l",
     lwd=2, xlab="", ylab="")
for (in_dex in 1:3) {  # add lines for t-distributions
  lines(x_var, dt(x_var, df=deg_free[in_dex]),
lwd=2, col=col_ors[in_dex+1])
}  # end for
# add title
title(main="t-distributions", line=0.5)
# add legend
legend("topright", inset=0.05,
       title="Degrees\n of freedom", lab_els,
       cex=0.8, lwd=6, lty=c(1, 1, 1, 1),
       col=col_ors)
x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
deg_free <- c(3, 5, 9)  # df values
col_ors <- c("black", "red", "blue", "green")
lab_els <- paste0("df1=", deg_free, ", df2=3")
for (in_dex in 1:NROW(deg_free)) {  # plot four curves
curve(expr=df(x, df1=deg_free[in_dex], df2=3),
      type="l", xlim=c(0, 4),
      xlab="", ylab="", lwd=2,
      col=col_ors[in_dex],
      add=as.logical(in_dex-1))
}  # end for
# add title
title(main="F-Distributions", line=0.5)
# add legend
legend("topright", inset=0.05, title="degrees of freedom",
       lab_els, cex=0.8, lwd=2, lty=1,
       col=col_ors)
rm(list=ls())
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
poisson_events <- 0:11  # Poisson events
poisson_freq <- dpois(poisson_events, lambda=4)
names(poisson_freq) <- as.character(poisson_events)
# Poisson function
poisson_func <- function(x, lambda)
              {exp(-lambda)*lambda^x/factorial(x)}
curve(expr=poisson_func(x, lambda=4), xlim=c(0, 11), main="Poisson distribution",
xlab="No. of events", ylab="Frequency of events", lwd=2, col="red")
legend(x="topright", legend="Poisson density", title="",
 inset=0.05, cex=0.8, bg="white", lwd=4, lty=1, col="red")
set.seed(1121)  # reset random number generator
runif(3)  # three numbers from uniform distribution
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
# define logistic map function
log_map <- function(x, r=4) r*x*(1-x)
log_map(0.25, 4)
# plot logistic map
x11(width=6, height=5)
curve(expr=log_map, type="l", xlim=c(0, 1),
xlab="x[n-1]", ylab="x[n]", lwd=2, col="blue",
main="logistic map")
lines(x=c(0, 0.25), y=c(0.75, 0.75), lwd=2, col="blue")
lines(x=c(0.25, 0.25), y=c(0, 0.75), lwd=2, col="blue")
# calculate uniformly distributed pseudo-random
# sequence using logistic map function
uni_form <- function(see_d, len_gth=10) {
  # pre-allocate vector instead of "growing" it
  out_put <- numeric(len_gth)
  # initialize
  out_put[1] <- see_d
  # perform loop
  for (i in 2:len_gth) {
    out_put[i] <- 4*out_put[i-1]*(1-out_put[i-1])
  }  # end for
  acos(1-2*out_put)/pi
}  # end uni_form
uni_form(see_d=0.1, len_gth=15)
plot(
  density(uni_form(see_d=runif(1), len_gth=1e5)),
  xlab="", ylab="", lwd=2, col="blue",
  main="uniform pseudo-random number density")
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
re_turns <- diff(log(EuStockMarkets[, 1]))
# number of observations
len_gth <- NROW(re_turns)
# mean of DAX returns
mean_rets <- mean(re_turns)
# standard deviation of DAX returns
sd_rets <- sd(re_turns)
# skew of DAX returns
len_gth/((len_gth-1)*(len_gth-2))*
  sum(((re_turns - mean_rets)/sd_rets)^3)
# kurtosis of DAX returns
len_gth*(len_gth+1)/((len_gth-1)^3)*
  sum(((re_turns - mean_rets)/sd_rets)^4)
# random normal returns
re_turns <- rnorm(len_gth, sd=2)
# mean and standard deviation of random normal returns
mean_rets <- mean(re_turns)
sd_rets <- sd(re_turns)
# skew of random normal returns
len_gth/((len_gth-1)*(len_gth-2))*
  sum(((re_turns - mean_rets)/sd_rets)^3)
# kurtosis of random normal returns
len_gth*(len_gth+1)/((len_gth-1)^3)*
  sum(((re_turns - mean_rets)/sd_rets)^4)
set.seed(1121)  # reset random number generator
# sample from Standard Normal Distribution
len_gth <- 1000
sam_ple <- rnorm(len_gth)
# sample mean
mean(sam_ple)
# sample standard deviation
sd(sam_ple)
#Perform two-tailed test that sample is
#from Standard Normal Distribution (mean=0, SD=1)
# generate vector of samples and store in data frame
test_frame <- data.frame(samples=rnorm(1e4))
# get p-values for all the samples
test_frame$p_values <- sapply(test_frame$samples, 
        function(x) 2*pnorm(-abs(x)))
# significance level, two-tailed test, critical value=2*SD
signif_level <- 2*(1-pnorm(2))
# compare p_values to significance level
test_frame$result <-
  test_frame$p_values > signif_level
# number of null rejections
sum(!test_frame$result) / NROW(test_frame)
# show null rejections
head(test_frame[!test_frame$result, ])
x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
# plot the Normal probability distribution
curve(expr=dnorm(x, sd=1), type="l", xlim=c(-4, 4),
xlab="", ylab="", lwd=3, col="blue")
title(main="Two-tailed Test", line=0.5)
# plot tails of the distribution using polygons
star_t <- 2; e_nd <- 4
# plot right tail using polygon
x_var <- seq(star_t, e_nd, length=100)
y_var <- dnorm(x_var, sd=1)
y_var[1] <- (-1)
y_var[NROW(y_var)] <- (-1)
polygon(x=x_var, y=y_var, col="red")
# plot left tail using polygon
y_var <- dnorm(-x_var, sd=1)
y_var[1] <- (-1)
y_var[NROW(y_var)] <- (-1)
polygon(x=(-x_var), y=y_var, col="red")
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
# t-test for single sample
t.test(rnorm(100))
# t-test for two samples
t.test(rnorm(100),
       rnorm(100, mean=1))
# Wilcoxon test for normal distribution
wilcox.test(rnorm(100))
# Wilcoxon test for two normal distributions
wilcox.test(rnorm(100), rnorm(100, mean=0.1))
# Wilcoxon test for two normal distributions
wilcox.test(rnorm(100), rnorm(100, mean=1.0))
# Wilcoxon test for a uniform versus normal distribution
wilcox.test(runif(100), rnorm(100))
# KS test for normal distribution
ks.test(rnorm(100), pnorm)
# KS test for uniform distribution
ks.test(runif(100), pnorm)
# KS test for two similar normal distributions
ks.test(rnorm(100), rnorm(100, mean=0.1))
# KS test for two different normal distributions
ks.test(rnorm(100), rnorm(100, mean=1.0))
# calculate DAX percentage returns
dax_rets <- diff(log(EuStockMarkets[, 1]))

# Shapiro-Wilk test for normal distribution
shapiro.test(rnorm(NROW(dax_rets)))

# Shapiro-Wilk test for DAX returns
shapiro.test(dax_rets)

# Shapiro-Wilk test for uniform distribution
shapiro.test(runif(NROW(dax_rets)))
dax_rets <- diff(log(EuStockMarkets[, 1]))
library(tseries)  # load package tseries

# Jarque-Bera test for normal distribution
jarque.bera.test(rnorm(NROW(dax_rets)))

# Jarque-Bera test for DAX returns
jarque.bera.test(dax_rets)

# Jarque-Bera test for uniform distribution
jarque.bera.test(runif(NROW(dax_rets)))
