library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
options(digits=3)
thm <- knit_theme$get("acid")
knit_theme$set(thm)
foo <- 0.3/3
foo  # printed as "0.1"
foo - 0.1  # foo is not equal to "0.1"
foo == 0.1  # foo is not equal to "0.1"
print(foo, digits=10)
print(foo, digits=16)
# foo is equal to "0.1" within machine precision
all.equal(foo, 0.1)
foo <- (3-2.9)
print(foo, digits=20)
# info machine precision of computer R is running on
# ?.Machine
# machine precision
.Machine$double.eps
foo <- sqrt(2)
foo^2  # printed as "2"
foo^2 == 2  # foo^2 is not equal to "2"
print(foo^2, digits=20)
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
print(0.2, digits=22)
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
  for(in_dex in 1:n_col)  # populate vector
    dframe <- mat_rix[, in_dex]
  attr(dframe, "row.names") <-  # add attributes
    .set_row_names(nrow(mat_rix))
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
row_sums <- numeric(nrow(big_matrix))
summary(microbenchmark(
  v_apply=vapply(1:nrow(big_matrix), function(in_dex)
    sum(big_matrix[in_dex, ]),
    FUN.VALUE=c(sum=0)),  # end vapply
  l_apply=lapply(1:nrow(big_matrix), function(in_dex)
    sum(big_matrix[in_dex, ])),  # end lapply
  apply=apply(big_matrix, 1, sum),
  s_apply=sapply(1:nrow(big_matrix), function(in_dex)
    sum(big_matrix[in_dex, ])),  # end sapply
  for_loop=for(i in 1:nrow(big_matrix)) {
    row_sums[i] <- sum(big_matrix[i,])
  },  # end for
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
big_vector <- rnorm(5000)
summary(microbenchmark(
# allocate full memory for cumulative sum
  for_loop={cum_sum <- numeric(length(big_vector))
    cum_sum[1] <- big_vector[1]
    for(i in 2:length(big_vector)) {
      cum_sum[i] <- cum_sum[i-1] + big_vector[i]
    }},  # end for
# allocate zero memory for cumulative sum
  grow_vec={cum_sum <- numeric(0)
    cum_sum[1] <- big_vector[1]
    for(i in 2:length(big_vector)) {
# add new element to "cum_sum" ("grow" it)
      cum_sum[i] <- cum_sum[i-1] + big_vector[i]
    }},  # end for
# allocate zero memory for cumulative sum
  com_bine={cum_sum <- numeric(0)
    cum_sum[1] <- big_vector[1]
    for(i in 2:length(big_vector)) {
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
  for(i in 1:length(vec_tor1)) {
    big_vector[i] <- vec_tor1[i] + vec_tor2[i]
  }  # end for
)  # end system.time
# sum vectors using vectorized "+"
system.time(big_vector <- vec_tor1 + vec_tor2)
# allocate memory for cumulative sum
cum_sum <- numeric(length(big_vector))
# cumulative sum using "for" loop
cum_sum[1] <- big_vector[1]
system.time(
  for(i in 2:length(big_vector)) {
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
library(matrixStats)  # load package "matrixStats"
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
library(caTools)  # load package "caTools"
# get documentation for package "caTools"
packageDescription("caTools")  # get short description
help(package="caTools")  # load help page
data(package="caTools")  # list all datasets in "caTools"
ls("package:caTools")  # list all objects in "caTools"
detach("package:caTools")  # remove caTools from search path
# median filter
vol_window <- 11
med_ian <- runmed(x=big_vector, k=vol_window)
# vector of rolling volatility
vo_lat <- runsd(x=big_vector, k=vol_window,
          endrule="constant", align="center")
# vector of rolling quantiles
quan_tiles <- runquantile(x=big_vector,
            k=vol_window, probs=0.9,
            endrule="constant",
            align="center")
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
my_fun(in_put=1:3, pa_ram=2)  # "in_put" is vectorized
my_fun(in_put=10, pa_ram=2:4)  # "pa_ram" is vectorized
rnorm(1, sd=1:3) # rnorm() "sd" argument not vectorized
# sapply produces desired vector output
sapply(1:3, function(sd, ...) rnorm(sd=sd, ...), n=3)
sapply(1:3, rnorm, n=3, mean=0)
sapply(-1:1, 
       function(mean, ...) rnorm(mean=mean, ...), n=3)
# rnorm vectorized with respect to "sd"
vec_rnorm <- function(n, mean=0, sd=1) {
  if (length(sd)==1)
    rnorm(n=n, mean=mean, sd=sd)
  else
    sapply(sd, rnorm, n=n, mean=mean)
}  # end vec_rnorm
foo <- structure(1:3, names=paste0("sd=", 1:3))
vec_rnorm(n=3, sd=foo)
# rnorm vectorized with respect to "mean" and "sd"
vec_rnorm <- Vectorize(
      FUN=rnorm,
      vectorize.args=c("mean", "sd")
)  # end Vectorize
vec_rnorm(n=3, sd=foo)
foo <- structure(-1:1, names=paste0("mean=", -1:1))
vec_rnorm(n=3, mean=foo)
str(sum)
# na.rm is bound by name
mapply(sum, 6:9, c(5, NA, 3), 2:6, na.rm=TRUE)
str(rnorm)
# mapply vectorizes both arguments "mean" and "sd"
mapply(rnorm, n=5, mean=(-1:1), sd=(1:3))

mapply(function(in_put, e_xp) in_put^e_xp, 
       1:5, seq(from=1, by=0.2, length.out=5))
# rnorm vectorized with respect to both "mean" and "sd"
vec_rnorm <- function(n, mean=0, sd=1) {
  if (length(mean)==1 && length(sd)==1)
    rnorm(n=n, mean=mean, sd=sd)
  else
    mapply(rnorm, n=n, mean=mean, sd=sd)
}  # end vec_rnorm
# call vec_rnorm() on vector of "sd"
foo <- structure(1:3, names=paste0("sd=", 1:3))
vec_rnorm(n=3, sd=foo)
# call vec_rnorm() on vector of "mean"
foo <- structure(-1:1, names=paste0("mean=", -1:1))
vec_rnorm(n=3, mean=foo)
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
par(oma=c(1, 1, 1, 1), mgp=c(2, 1, 0), mar=c(5, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
set.seed(1121)  # reset random number generator
simu_max <- 1000  # max simulation trials
simu_prices <- numeric(simu_max)  # initialize prices
barrier_level <- 20  # barrier level
simu_prices[1] <- 0  # first simulated price
in_dex <- 2  # initialize simulation index
while ((in_dex <= simu_max) &&
 (simu_prices[in_dex - 1] < barrier_level)) {
  simu_prices[in_dex] <- # simulate next price
    simu_prices[in_dex - 1] + rnorm(1)
  in_dex <- in_dex + 1  # advance in_dex
}  # end while
if (in_dex <= simu_max) {  # fill zero prices
  simu_prices[in_dex:simu_max] <- simu_prices[in_dex - 1]
}
# create daily time series starting 2011
ts_var <- ts(data=simu_prices, frequency=365, start=c(2011, 1))
plot(ts_var, type="l", col="black",  # create plot
     lty="solid", xlab="", ylab="")
abline(h=barrier_level, lwd=2, col="red")  # add horizontal line
title(main="Random Prices", line=0)  # add title
par(oma=c(1, 1, 1, 1), mgp=c(2, 1, 0), mar=c(5, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
set.seed(1121)  # reset random number generator
simu_max <- 1000  # max simulation trials
barrier_level <- 20  # barrier level
# simulated prices
simu_prices <- cumsum(rnorm(simu_max))
# find index when prices cross barrier_level
which_index <- which(simu_prices > barrier_level)
# fill prices after crossing barrier_level
if (length(which_index)>0) {
  simu_prices[(which_index[1]+1):simu_max] <-
    simu_prices[which_index[1]]
}  # end if
# create daily time series starting 2011
ts_var <- ts(data=simu_prices, frequency=365,
     start=c(2011, 1))
plot(ts_var, type="l", col="black",  # create plot
     lty="solid", xlab="", ylab="")
abline(h=barrier_level, lwd=2, col="red")  # add horizontal line
title(main="Random Prices", line=0)  # add title
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
# plot objective function
curve(expr=object_ive, type="l", xlim=c(-8, 9),
xlab="", ylab="", lwd=2)
title(main="Objective Function", line=-1)  # add title
# sample of normal variables
sam_ple <- rnorm(1000, mean=4, sd=2)
# objective function is log-likelihood
object_ive <- function(parm, sam_ple) {
  sum(2*log(parm[2]) + 
    ((sam_ple - parm[1])/parm[2])^2)
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
library(rgl)
par3d(cex=2.0)  # scale text by factor of 2
persp3d(z=-objective_grid, zlab="objective",
  col="green", main="objective function")
# initial parameters
par_init <- c(mean=0, sd=1)
# perform optimization quasi-Newton method
optim_run <- optim(par=par_init, 
       fn=object_ive, 
       sam_ple=sam_ple,
       method="L-BFGS-B",
       upper=c(10, 10),
       lower=c(-10, 0.1))
# optimal parameters
optim_run$par
# plot histogram
histo_gram <- hist(sam_ple, plot=FALSE)
plot(histo_gram, freq=FALSE,
     main="histogram of sample")
curve(expr=dnorm(x, mean=optim_run$par["mean"],
           sd=optim_run$par["sd"]),
add=TRUE, type="l", lwd=2, col="red")
legend("topright", inset=0.0, cex=0.8, title=NULL,
 leg="optimal parameters",
 lwd=2, bg="white", col="red")
# sample from mixture of normal distributions
sam_ple <- c(rnorm(100, sd=1.0), 
             rnorm(100, mean=4, sd=1.0))
# objective function is log-likelihood
object_ive <- function(parm, sam_ple) {
  likelihood <- parm[1]/parm[3] * 
  dnorm((sam_ple-parm[2])/parm[3]) +
  (1-parm[1])/parm[5]*dnorm((sam_ple-parm[4])/parm[5])
  if(any(likelihood <= 0)) Inf else
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
optim_run <- optim(par=par_init, 
      fn=object_ive, 
      sam_ple=sam_ple,
      method="L-BFGS-B",
      upper=c(1,10,10,10,10),
      lower=c(0,-10,0.2,-10,0.2))
optim_run$par
# plot histogram
histo_gram <- hist(sam_ple, plot=FALSE)
plot(histo_gram, freq=FALSE,
     main="histogram of sample")
fit_func <- function(x, parm) {
  parm["weight"] * dnorm(x, mean=parm["m1"], sd=parm["s1"]) +
    (1-parm["weight"]) * dnorm(x, mean=parm["m2"], sd=parm["s2"])
}  # end fit_func
curve(expr=fit_func(x, parm=optim_run$par), add=TRUE,
type="l", lwd=2, col="red")
legend("topright", inset=0.0, cex=0.8, title=NULL,
 leg="optimal parameters",
 lwd=2, bg="white", col="red")
