library(knitr)
opts_chunk$set(prompt=TRUE, eval=FALSE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
options(digits=3)
thm <- knit_theme$get("acid")
knit_theme$set(thm)
foo <- 0.3/3
foo  # printed as "0.1"
foo - 0.1  # foo is not equal to "0.1"
foo == 0.1  # foo is not equal to "0.1"
format(foo, digits=10)
format(foo, digits=16)
# foo is equal to "0.1" within machine precision
all.equal(foo, 0.1)
foo <- (3-2.9)
format(foo, digits=20)
# info machine precision of computer R is running on
# ?.Machine
# machine precision
.Machine$double.eps
foo <- sqrt(2)
foo^2  # printed as "2"
foo^2 == 2  # foo^2 is not equal to "2"
format(foo^2, digits=20)
# foo^2 is equal to "2" within machine precision
all.equal(foo^2, 2)
# numbers with precision 0.1
0.1*(1:10)
# round to precision 0.1
round(3.675, 1)
# round to precision 1.0
round(3.675)
# round to nearest even number
c(round(2.5), round(3.5), round(4.5))
round(4:20/2)  # round to nearest even number
trunc(3.675)  # truncate
num_var <- 2
num_var==2
identical(num_var, 2)

identical(num_var, NULL)
# this doesn't work:
# num_var==NULL
is.null(num_var)

vec_tor <- c(2, 4, 6)
vec_tor==2
identical(vec_tor, 2)

# num_ber is equal to "1.0" within machine precision
num_ber <- 1.0 + 2*sqrt(.Machine$double.eps)
all.equal(num_ber, 1.0)

# info machine precision of computer R is running on
# ?.Machine
# machine precision
.Machine$double.eps
4.7 %/% 0.5  # modulo division
4.7 %% 0.5  # remainder of modulo division
# reversing modulo division usually
# returns the original number
(4.7 %% 0.5) + 0.5 * (4.7 %/% 0.5)
# modulo division of non-integer numbers can
# produce incorrect results
0.6 %/% 0.2  # produces 2 instead of 3
6 %/% 2  # use integers to get correct result
# 0.2 stored as binary number
# slightly larger than 0.2
format(0.2, digits=22)
# get size of an object
object.size(runif(1e6))
format(object.size(runif(1e6)), units="MB")
# get sizes of objects in workspace
sort(sapply(ls(),
  function(ob_ject) {
    format(object.size(get(ob_ject)), units="KB")}))
# get sizes of objects in workspace
sort(sapply(mget(ls()), object.size))
sort(sapply(mget(ls()),
function(ob_ject) {
  format(object.size(ob_ject), units="KB")}
))
# get sizes of objects in env_etf environment
sort(sapply(ls(env_etf),
  function(ob_ject) {
    object.size(get(ob_ject, env_etf))}))
# get sizes of objects in env_etf environment
sort(sapply(mget(ls(env_etf), env_etf),
      object.size))
# get total size of all objects in workspace
format(object.size(x=mget(ls())), units="MB")
library(gdata)  # load package gdata
# get names, class, and size of objects in workspace
ob_jects <- ll(unit="bytes")
# sort by memory size (descending)
ob_jects[order(ob_jects[, 2], decreasing=TRUE), ]
ll()[order(ll()$KB, decreasing=TRUE), ]
# get sizes of objects in env_etf environment
ll(unit="bytes", env_etf)
library(SOAR)  # load package SOAR
# get sizes of objects in workspace
sort(sapply(mget(ls()), object.size))
Store(etf_list)  # store in object cache
# get sizes of objects in workspace
sort(sapply(mget(ls()), object.size))
search()  # get search path for R objects
Ls()  # list object cache
find("etf_list")  # find object on search path
# get R memory
v_cells <- gc()["Vcells", "used"]
# create vector with 1,000,000 cells
foo_bar <- numeric(1000000)
# get extra R memory
gc()["Vcells", "used"] - v_cells
# get total size of all objects in workspace
format(object.size(x=mget(ls())), units="MB")
library(microbenchmark)
foo <- runif(1e6)
system.time(foo^0.5)
microbenchmark(sqrt(foo), foo^0.5, times=10)
library(microbenchmark)
# sum() is a compiled primitive function
sum
# mean() is a generic function
mean
foo <- runif(1e6)
# sum() is much faster than mean()
summary(
  microbenchmark(sum(foo), mean(foo), times=10)
  )[, c(1, 4, 5)]
# any() is a compiled primitive function
any
# any() is much faster than %in% wrapper for match()
summary(
  microbenchmark(any(foo == 1), {1 %in% foo}, times=10)
  )[, c(1, 4, 5)]
library(microbenchmark)
mat_rix <- matrix(1:9, ncol=3, # create matrix
  dimnames=list(paste0("row", 1:3),
          paste0("col", 1:3)))
# create specialized function
matrix_to_dframe <- function(mat_rix) {
  n_col <- ncol(mat_rix)
  dframe <- vector("list", n_col)  # empty vector
  for (in_dex in 1:n_col)  # populate vector
    dframe <- mat_rix[, in_dex]
  attr(dframe, "row.names") <-  # add attributes
    .set_row_names(NROW(mat_rix))
  attr(dframe, "class") <- "data.frame"
  dframe  # return data frame
}  # end matrix_to_dframe
# compare speed of three methods
summary(microbenchmark(
  matrix_to_dframe(mat_rix),
  as.data.frame.matrix(mat_rix),
  as.data.frame(mat_rix),
  times=10))[, c(1, 4, 5)]
# matrix with 5,000 rows
big_matrix <- matrix(rnorm(10000), ncol=2)
# allocate memory for row sums
row_sums <- numeric(NROW(big_matrix))
summary(microbenchmark(
  row_sums=rowSums(big_matrix),  # end row_sums
  ap_ply=apply(big_matrix, 1, sum),  # end apply
  l_apply=lapply(1:NROW(big_matrix), function(in_dex)
    sum(big_matrix[in_dex, ])),  # end lapply
  v_apply=vapply(1:NROW(big_matrix), function(in_dex)
    sum(big_matrix[in_dex, ]),
    FUN.VALUE=c(sum=0)),  # end vapply
  s_apply=sapply(1:NROW(big_matrix), function(in_dex)
    sum(big_matrix[in_dex, ])),  # end sapply
  for_loop=for (i in 1:NROW(big_matrix)) {
    row_sums[i] <- sum(big_matrix[i,])
  },  # end for
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
big_vector <- rnorm(5000)
summary(microbenchmark(
# allocate full memory for cumulative sum
  for_loop={cum_sum <- numeric(NROW(big_vector))
    cum_sum[1] <- big_vector[1]
    for (i in 2:NROW(big_vector)) {
      cum_sum[i] <- cum_sum[i-1] + big_vector[i]
    }},  # end for
# allocate zero memory for cumulative sum
  grow_vec={cum_sum <- numeric(0)
    cum_sum[1] <- big_vector[1]
    for (i in 2:NROW(big_vector)) {
# add new element to "cum_sum" ("grow" it)
      cum_sum[i] <- cum_sum[i-1] + big_vector[i]
    }},  # end for
# allocate zero memory for cumulative sum
  com_bine={cum_sum <- numeric(0)
    cum_sum[1] <- big_vector[1]
    for (i in 2:NROW(big_vector)) {
# add new element to "cum_sum" ("grow" it)
      cum_sum <- c(cum_sum, big_vector[i])
    }},  # end for
  times=10))[, c(1, 4, 5)]
library(microbenchmark)
foo <- runif(1e6)
system.time(foo^0.5)
summary(
  microbenchmark(sqrt(foo), foo^0.5, times=10)
  )[, c(1, 4, 5)]
vec_tor1 <- rnorm(1000000)
vec_tor2 <- rnorm(1000000)
big_vector <- numeric(1000000)
system.time(  # sum vectors using "for" loop
  for (i in 1:NROW(vec_tor1)) {
    big_vector[i] <- vec_tor1[i] + vec_tor2[i]
  }  # end for
)  # end system.time
# sum vectors using vectorized "+"
system.time(big_vector <- vec_tor1 + vec_tor2)
# allocate memory for cumulative sum
cum_sum <- numeric(NROW(big_vector))
# cumulative sum using "for" loop
cum_sum[1] <- big_vector[1]
system.time(
  for (i in 2:NROW(big_vector)) {
    cum_sum[i] <- cum_sum[i-1] + big_vector[i]
  }  # end for
)  # end system.time
# cumulative sum using "cumsum"
system.time(cum_sum <- cumsum(big_vector))
# calculate row sums two different ways
summary(microbenchmark(
  row_sums=rowSums(big_matrix),
  ap_ply=apply(big_matrix, 1, sum),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
library(microbenchmark)
str(pmax)
# calculate row maximums two different ways
summary(microbenchmark(
  p_max=
    do.call(pmax.int,
lapply(seq_along(big_matrix[1, ]),
  function(in_dex) big_matrix[, in_dex])),
  l_apply=unlist(
    lapply(seq_along(big_matrix[, 1]),
  function(in_dex) max(big_matrix[in_dex, ]))),
  times=10))[, c(1, 4, 5)]
library(matrixStats)  # load package matrixStats
# calculate row min values three different ways
summary(microbenchmark(
  row_mins=rowMins(big_matrix),
  p_min=
    do.call(pmin.int,
      lapply(seq_along(big_matrix[1, ]),
             function(in_dex)
               big_matrix[, in_dex])),
  as_data_frame=
    do.call(pmin.int,
      as.data.frame.matrix(big_matrix)),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
summary(microbenchmark(  # assign values to vector three different ways
# fast vectorized assignment loop performed in C using brackets "[]"
  brack_ets={vec_tor <- numeric(10)
    vec_tor[] <- 2},
# slow because loop is performed in R
  for_loop={vec_tor <- numeric(10)
    for (in_dex in seq_along(vec_tor))
      vec_tor[in_dex] <- 2},
# very slow because no memory is pre-allocated
# "vec_tor" is "grown" with each new element
  grow_vec={vec_tor <- numeric(0)
    for (in_dex in 1:10)
# add new element to "vec_tor" ("grow" it)
      vec_tor[in_dex] <- 2},
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
summary(microbenchmark(  # assign values to vector two different ways
# fast vectorized assignment loop performed in C using brackets "[]"
  brack_ets={vec_tor <- numeric(10)
    vec_tor[4:7] <- rnorm(4)},
# slow because loop is performed in R
  for_loop={vec_tor <- numeric(10)
    for (in_dex in 4:7)
      vec_tor[in_dex] <- rnorm(1)},
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# define function vectorized automatically
my_fun <- function(in_put, pa_ram) {
  pa_ram*in_put
}  # end my_fun
# "in_put" is vectorized
my_fun(in_put=1:3, pa_ram=2)
# "pa_ram" is vectorized
my_fun(in_put=10, pa_ram=2:4)
# define vectors of parameters of rnorm()
std_devs <-
  structure(1:3, names=paste0("sd=", 1:3))
me_ans <-
  structure(-1:1, names=paste0("mean=", -1:1))
# "sd" argument of rnorm() isn't vectorized
rnorm(1, sd=std_devs)
# "mean" argument of rnorm() isn't vectorized
rnorm(1, mean=me_ans)
# sapply produces desired vector output
set.seed(1121)
sapply(std_devs, function(std_dev) rnorm(n=2, sd=std_dev))
set.seed(1121)
sapply(std_devs, rnorm, n=2, mean=0)
set.seed(1121)
sapply(me_ans,
 function(me_an) rnorm(n=2, mean=me_an))
set.seed(1121)
sapply(me_ans, rnorm, n=2)
# rnorm() vectorized with respect to "std_dev"
vec_rnorm <- function(n, mean=0, sd=1) {
  if (NROW(sd)==1)
    rnorm(n=n, mean=mean, sd=sd)
  else
    sapply(sd, rnorm, n=n, mean=mean)
}  # end vec_rnorm
set.seed(1121)
vec_rnorm(n=2, sd=std_devs)
# rnorm() vectorized with respect to "mean" and "sd"
vec_rnorm <- Vectorize(FUN=rnorm,
        vectorize.args=c("mean", "sd")
)  # end Vectorize
set.seed(1121)
vec_rnorm(n=2, sd=std_devs)
set.seed(1121)
vec_rnorm(n=2, mean=me_ans)
str(sum)
# na.rm is bound by name
mapply(sum, 6:9, c(5, NA, 3), 2:6, na.rm=TRUE)
str(rnorm)
# mapply vectorizes both arguments "mean" and "sd"
mapply(rnorm, n=5, mean=me_ans, sd=std_devs)
mapply(function(in_put, e_xp) in_put^e_xp,
 1:5, seq(from=1, by=0.2, length.out=5))
# rnorm() vectorized with respect to "mean" and "sd"
vec_rnorm <- function(n, mean=0, sd=1) {
  if (NROW(mean)==1 && NROW(sd)==1)
    rnorm(n=n, mean=mean, sd=sd)
  else
    mapply(rnorm, n=n, mean=mean, sd=sd)
}  # end vec_rnorm
# call vec_rnorm() on vector of "sd"
vec_rnorm(n=2, sd=std_devs)
# call vec_rnorm() on vector of "mean"
vec_rnorm(n=2, mean=me_ans)
# create two numeric vectors
vec_tor1 <- sin(0.25*pi*1:10)
vec_tor2 <- cos(0.25*pi*1:10)
# create third vector using 'ifelse'
vec_tor3 <- ifelse(vec_tor1 > vec_tor2,
          vec_tor1, vec_tor2)
# cbind all three together
vec_tor4 <- cbind(vec_tor1, vec_tor2, vec_tor3)

# set plotting parameters
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0),
    cex.lab=0.8, cex.axis=0.8, cex.main=0.8,
    cex.sub=0.5)
# plot matrix
matplot(vec_tor4, type="l", lty="solid",
col=c("green", "blue", "red"),
lwd=c(2, 2, 2), xlab="", ylab="")
# add legend
legend(x="bottomright", legend=colnames(vec_tor4),
       title="", inset=0.05, cex=0.8, lwd=2,
       lty=c(1, 1, 1), col=c("green", "blue", "red"))
set.seed(1121)  # reset random number generator
# sample from Standard Normal Distribution
len_gth <- 1000
sam_ple <- rnorm(len_gth)
# sample mean - MC estimate
mean(sam_ple)
# sample standard deviation - MC estimate
sd(sam_ple)
# MC estimate of cumulative probability
sam_ple <- sort(sam_ple)
pnorm(1)
sum(sam_ple<1)/len_gth
# MC estimate of quantile
qnorm(0.75)
sam_ple[0.75*len_gth]
x11(width=6, height=5)
par(oma=c(1, 1, 1, 1), mar=c(2, 2, 2, 1), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
set.seed(1121)  # reset random number generator
lev_el <- 20  # barrier level
len_gth <- 1000  # number of simulation steps
pa_th <- numeric(len_gth)  # allocate path vector
pa_th[1] <- 0  # initialize path
in_dex <- 2  # initialize simulation index
while ((in_dex <= len_gth) &&
 (pa_th[in_dex - 1] < lev_el)) {
# simulate next step
  pa_th[in_dex] <-
    pa_th[in_dex - 1] + rnorm(1)
  in_dex <- in_dex + 1  # advance in_dex
}  # end while
# fill remaining pa_th after it crosses lev_el
if (in_dex <= len_gth)
  pa_th[in_dex:len_gth] <- pa_th[in_dex - 1]
# create daily time series starting 2011
ts_path <- ts(data=pa_th, frequency=365, start=c(2011, 1))
plot(ts_path, type="l", col="black",
     lty="solid", lwd=2, xlab="", ylab="")
abline(h=lev_el, lwd=2, col="red")
title(main="Brownian motion crossing a barrier level",
      line=0.5)
x11(width=6, height=5)
par(oma=c(1, 1, 1, 1), mar=c(2, 2, 2, 1), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
set.seed(1121)  # reset random number generator
lev_el <- 20  # barrier level
len_gth <- 1000  # number of simulation steps
# simulate path of Brownian motion
pa_th <- cumsum(rnorm(len_gth))
# find index when pa_th crosses lev_el
cro_ss <- which(pa_th > lev_el)
# fill remaining pa_th after it crosses lev_el
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
abline(h=lev_el, lwd=2, col="red")
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
# standard error of mean from bootstrap
sd(boot_strap)
sqrt(2)*sd(boot_strap)
set.seed(1121)  # initialize random number generator
# define explanatory and response variables
explana_tory <- rnorm(100, mean=2)
noise <- rnorm(100)
res_ponse <- -3 + explana_tory + noise
# define design matrix and regression formula
de_sign <- data.frame(res_ponse, explana_tory)
reg_formula <- paste(colnames(de_sign)[1],
  paste(colnames(de_sign)[-1], collapse="+"),
  sep=" ~ ")
# bootstrap the regression
boot_strap <- sapply(1:100, function(x) {
  boot_sample <- sample.int(dim(de_sign)[1],
                      replace=TRUE)
  reg_model <- lm(reg_formula,
          data=de_sign[boot_sample, ])
  reg_model$coefficients
})  # end sapply
par(oma=c(1, 2, 1, 0), mgp=c(2, 1, 0), mar=c(5, 1, 1, 1), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
x11(width=6, height=6)
# means and standard errors from bootstrap
apply(boot_strap, MARGIN=1,
      function(x) c(mean=mean(x), sd=sd(x)))
# means and standard errors from regression summary
reg_model <- lm(reg_formula, data=de_sign)
reg_model$coefficients
summary(reg_model)$coefficients[, "Std. Error"]
plot(density(boot_strap["explana_tory", ]),
     lwd=2, xlab="regression slopes",
     main="Bootstrapped regression slopes")
abline(v=mean(boot_strap["explana_tory", ]),
       lwd=2, col="red")
library(parallel)  # load package parallel
num_cores <- detectCores() - 1  # number of cores
clus_ter <- makeCluster(num_cores)  # initialize compute cluster under Windows
# bootstrap the regression under Windows
boot_strap <- parLapply(clus_ter, 1:1000,
  function(x, reg_formula, de_sign) {
    boot_sample <-
sample.int(dim(de_sign)[1], replace=TRUE)
    reg_model <- lm(reg_formula,
data=de_sign[boot_sample, ])
    reg_model$coefficients
  },
  reg_formula=reg_formula,
  de_sign=de_sign)  # end parLapply
# bootstrap the regression under Mac-OSX or Linux
boot_strap <- mclapply(1:1000,
  function(x) {
    boot_sample <-
sample.int(dim(de_sign)[1], replace=TRUE)
    lm(reg_formula,
data=de_sign[boot_sample, ])$coefficients
  }, mc.cores=num_cores)  # end mclapply
stopCluster(clus_ter)  # stop R processes over cluster under Windows
boot_strap <- rutils::do_call(rbind, boot_strap)
# means and standard errors from bootstrap
apply(boot_strap, MARGIN=2,
function(x) c(mean=mean(x), sd=sd(x)))
x11(width=6, height=6)
plot(density(boot_strap[, "explana_tory"]),
     lwd=2, xlab="regression slopes",
     main="Bootstrapped regression slopes")
abline(v=mean(boot_strap[, "explana_tory"]),
 lwd=2, col="red")
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
      NROW(index(get(sym_bol, envir=rutils::env_etf))))
# zoo function referenced using "::" in child process:
parSapply(clus_ter, c("VTI", "IEF", "DBC"),
    function(sym_bol)
      NROW(zoo::index(get(sym_bol, envir=rutils::env_etf))))
# package zoo loaded in child process:
parSapply(clus_ter, c("VTI", "IEF", "DBC"),
    function(sym_bol) {
      stopifnot("package:zoo" %in% search() || require("zoo", quietly=TRUE))
      NROW(index(get(sym_bol, envir=rutils::env_etf)))
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
options(width=50, dev='pdf')
str(optimize)
# objective function with multiple minima
object_ive <- function(in_put, param1=0.01) {
  sin(0.25*pi*in_put) + param1*(in_put-1)^2
}  # end object_ive
unlist(optimize(f=object_ive, interval=c(-4, 2)))
unlist(optimize(f=object_ive, interval=c(0, 8)))
options(width=60, dev='pdf')
par(oma=c(1, 1, 1, 1), mgp=c(2, 1, 0), mar=c(5, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# plot the objective function
curve(expr=object_ive, type="l", xlim=c(-8, 9),
xlab="", ylab="", lwd=2)
# add title
title(main="Objective Function", line=-1)
library(rgl)  # load rgl
# define function of two variables
sur_face <- function(x, y) y*sin(x)
# draw 3d surface plot of function
persp3d(x=sur_face, xlim=c(-5, 5), ylim=c(-5, 5),
  col="green", axes=FALSE)
# draw 3d surface plot of matrix
x_lim <- seq(from=-5, to=5, by=0.1)
y_lim <- seq(from=-5, to=5, by=0.1)
persp3d(z=outer(x_lim, y_lim, FUN=sur_face),
  xlab="x", ylab="y", zlab="sur_face",
  col="green")
# save current view to png file
rgl.snapshot("surface_plot.png")
# define function of two variables and two parameters
sur_face <- function(x, y, par_1=1, par_2=1)
  sin(par_1*x)*sin(par_2*y)
# draw 3d surface plot of function
persp3d(x=sur_face, xlim=c(-5, 5), ylim=c(-5, 5),
  col="green", axes=FALSE,
  par_1=1, par_2=2)
# Rastrigin function with vector argument for optimization
rastri_gin <- function(vec_tor, pa_ram=25){
  sum(vec_tor^2 - pa_ram*cos(vec_tor))
}  # end rastri_gin
vec_tor <- c(pi/6, pi/6)
rastri_gin(vec_tor=vec_tor)
# draw 3d surface plot of Rastrigin function
rgl::persp3d(
  x=Vectorize(function(x, y) rastri_gin(vec_tor=c(x, y))),
  xlim=c(-10, 10), ylim=c(-10, 10),
  col="green", axes=FALSE, zlab="", main="rastri_gin")
# optimize with respect to vector argument
op_tim <- optim(par=vec_tor, fn=rastri_gin,
        method="L-BFGS-B",
        upper=c(4*pi, 4*pi),
        lower=c(pi/2, pi/2),
        pa_ram=1)
# optimal parameters and value
op_tim$par
op_tim$value
rastri_gin(op_tim$par, pa_ram=1)
# sample of normal variables
sam_ple <- rnorm(1000, mean=4, sd=2)
# objective function is log-likelihood
object_ive <- function(pa_r, sam_ple) {
  sum(2*log(pa_r[2]) +
    ((sam_ple - pa_r[1])/pa_r[2])^2)
}  # end object_ive
# vectorize objective function
vec_objective <- Vectorize(
  FUN=function(mean, sd, sam_ple)
    object_ive(c(mean, sd), sam_ple),
  vectorize.args=c("mean", "sd")
)  # end Vectorize
# objective function on parameter grid
par_mean <- seq(1, 6, length=50)
par_sd <- seq(0.5, 3.0, length=50)
objective_grid <- outer(par_mean, par_sd,
vec_objective, sam_ple=sam_ple)
objective_min <- which(  # grid search
  objective_grid==min(objective_grid),
  arr.ind=TRUE)
objective_min
par_mean[objective_min[1]]  # mean
par_sd[objective_min[2]]  # sd
objective_grid[objective_min]
objective_grid[(objective_min[, 1] + -1:1),
       (objective_min[, 2] + -1:1)]
par(cex.lab=2.0, cex.axis=2.0, cex.main=2.0, cex.sub=2.0)
# perspective plot of log-likelihood function
persp(z=-objective_grid,
theta=45, phi=30, shade=0.5,
border="green", zlab="objective",
main="objective function")
# interactive perspective plot of log-likelihood function
library(rgl)  # load package rgl
par3d(cex=2.0)  # scale text by factor of 2
persp3d(z=-objective_grid, zlab="objective",
  col="green", main="objective function")
# initial parameters
par_init <- c(mean=0, sd=1)
# perform optimization using optim()
optim_fit <- optim(par=par_init,
  fn=object_ive, # log-likelihood function
  sam_ple=sam_ple,
  method="L-BFGS-B", # quasi-Newton method
  upper=c(10, 10), # upper constraint
  lower=c(-10, 0.1)) # lower constraint
# optimal parameters
optim_fit$par
# perform optimization using MASS::fitdistr()
optim_fit <- MASS::fitdistr(sam_ple, densfun="normal")
optim_fit$estimate
optim_fit$sd
# plot histogram
histo_gram <- hist(sam_ple, plot=FALSE)
plot(histo_gram, freq=FALSE,
     main="histogram of sample")
curve(expr=dnorm(x, mean=optim_fit$par["mean"],
           sd=optim_fit$par["sd"]),
add=TRUE, type="l", lwd=2, col="red")
legend("topright", inset=0.0, cex=0.8, title=NULL,
 leg="optimal parameters",
 lwd=2, bg="white", col="red")
# sample from mixture of normal distributions
sam_ple <- c(rnorm(100, sd=1.0),
             rnorm(100, mean=4, sd=1.0))
# objective function is log-likelihood
object_ive <- function(pa_r, sam_ple) {
  likelihood <- pa_r[1]/pa_r[3] *
  dnorm((sam_ple-pa_r[2])/pa_r[3]) +
  (1-pa_r[1])/pa_r[5]*dnorm((sam_ple-pa_r[4])/pa_r[5])
  if (any(likelihood <= 0)) Inf else
    -sum(log(likelihood))
}  # end object_ive
# vectorize objective function
vec_objective <- Vectorize(
  FUN=function(mean, sd, w, m1, s1, sam_ple)
    object_ive(c(w, m1, s1, mean, sd), sam_ple),
  vectorize.args=c("mean", "sd")
)  # end Vectorize
# objective function on parameter grid
par_mean <- seq(3, 5, length=50)
par_sd <- seq(0.5, 1.5, length=50)
objective_grid <- outer(par_mean, par_sd,
    vec_objective, sam_ple=sam_ple,
    w=0.5, m1=2.0, s1=2.0)
rownames(objective_grid) <- round(par_mean, 2)
colnames(objective_grid) <- round(par_sd, 2)
objective_min <- which(objective_grid==
  min(objective_grid), arr.ind=TRUE)
objective_min
objective_grid[objective_min]
objective_grid[(objective_min[, 1] + -1:1),
         (objective_min[, 2] + -1:1)]
# perspective plot of objective function
persp(par_mean, par_sd, -objective_grid,
theta=45, phi=30,
shade=0.5,
col=rainbow(50),
border="green",
main="objective function")
# initial parameters
par_init <- c(weight=0.5, m1=0, s1=1, m2=2, s2=1)
# perform optimization
optim_fit <- optim(par=par_init,
      fn=object_ive,
      sam_ple=sam_ple,
      method="L-BFGS-B",
      upper=c(1,10,10,10,10),
      lower=c(0,-10,0.2,-10,0.2))
optim_fit$par
# plot histogram
histo_gram <- hist(sam_ple, plot=FALSE)
plot(histo_gram, freq=FALSE,
     main="histogram of sample")
fit_func <- function(x, pa_r) {
  pa_r["weight"] *
    dnorm(x, mean=pa_r["m1"], sd=pa_r["s1"]) +
  (1-pa_r["weight"]) *
    dnorm(x, mean=pa_r["m2"], sd=pa_r["s2"])
}  # end fit_func
curve(expr=fit_func(x, pa_r=optim_fit$par), add=TRUE,
type="l", lwd=2, col="red")
legend("topright", inset=0.0, cex=0.8, title=NULL,
 leg="optimal parameters",
 lwd=2, bg="white", col="red")
# Rastrigin function with vector argument for optimization
rastri_gin <- function(vec_tor, pa_ram=25){
  sum(vec_tor^2 - pa_ram*cos(vec_tor))
}  # end rastri_gin
vec_tor <- c(pi/6, pi/6)
rastri_gin(vec_tor=vec_tor)
library(DEoptim)
optimize rastri_gin using DEoptim
op_tim <-  DEoptim(rastri_gin,
  upper=c(6, 6), lower=c(-6, -6),
  DEoptim.control(trace=FALSE, itermax=50))
# optimal parameters and value
op_tim$optim$bestmem
rastri_gin(op_tim$optim$bestmem)
summary(op_tim)
plot(op_tim)
# Rastrigin function with vector argument for optimization
rastri_gin <- function(vec_tor, pa_ram=25){
  sum(vec_tor^2 - pa_ram*cos(vec_tor))
}  # end rastri_gin
vec_tor <- c(pi/6, pi/6)
rastri_gin(vec_tor=vec_tor)
library(DEoptim)
optimize rastri_gin using DEoptim
op_tim <-  DEoptim(rastri_gin,
  upper=c(6, 6), lower=c(-6, -6),
  DEoptim.control(trace=FALSE, itermax=50))
# optimal parameters and value
op_tim$optim$bestmem
rastri_gin(op_tim$optim$bestmem)
summary(op_tim)
plot(op_tim)
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
# calculate uniformly distributed pseudo-random sequence
uni_form <- function(see_d, len_gth=10) {
  out_put <- numeric(len_gth)
  out_put[1] <- see_d
  for (i in 2:len_gth) {
    out_put[i] <- 4*out_put[i-1]*(1-out_put[i-1])
  }  # end for
  acos(1-2*out_put)/pi
}  # end uni_form

# source Rcpp functions from file
Rcpp::sourceCpp(file="C:/Develop/R/lecture_slides/scripts/uni_form.cpp")
# microbenchmark Rcpp code
library(microbenchmark)
summary(microbenchmark(
  pure_r=runif(1e5),
  r_loop=uni_form(0.3, 1e5),
  r_cpp=uniform_rcpp(0.3, 1e5),
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
par(oma=c(1, 1, 1, 1), mgp=c(2, 1, 0), mar=c(5, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
set.seed(1121)  # reset random number generator
lev_el <- 20  # barrier level
len_gth <- 1000  # number of simulation steps
pa_th <- numeric(len_gth)  # allocate path vector
pa_th[1] <- 0  # initialize path
in_dex <- 2  # initialize simulation index
while ((in_dex <= len_gth) &&
 (pa_th[in_dex - 1] < lev_el)) {
# simulate next step
  pa_th[in_dex] <-
    pa_th[in_dex - 1] + rnorm(1)
  in_dex <- in_dex + 1  # advance in_dex
}  # end while
# fill remaining pa_th after it crosses lev_el
if (in_dex <= len_gth)
  pa_th[in_dex:len_gth] <- pa_th[in_dex - 1]
# create daily time series starting 2011
ts_path <- ts(data=pa_th, frequency=365, start=c(2011, 1))
plot(ts_path, type="l", col="black",  # create plot
     lty="solid", xlab="", ylab="")
abline(h=lev_el, lwd=2, col="red")  # add horizontal line
title(main="Brownian motion crossing a barrier level",
      line=0.5)
