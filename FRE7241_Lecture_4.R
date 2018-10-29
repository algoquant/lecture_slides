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
Rcpp::sourceCpp(file="C:/Develop/R/lecture_slides/scripts/mult_rcpp.cpp")
# multiply two numbers
mult_rcpp(2, 3)
mult_rcpp(1:3, 6:4)
# multiply two vectors
mult_rcpp_vec(2, 3)
mult_rcpp_vec(1:3, 6:4)
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
sim_ou <- function(len_gth=1000, eq_price=5.0,
              vol_at=0.01, the_ta=0.01) {
  re_turns <- numeric(len_gth)
  price_s <- numeric(len_gth)
  price_s[1] <- eq_price
  for (i in 2:len_gth) {
    re_turns[i] <- the_ta*(eq_price - price_s[i-1]) + vol_at*rnorm(1)
    price_s[i] <- price_s[i-1] * exp(re_turns[i])
  }  # end for
  price_s
}  # end sim_ou
# simulate Ornstein-Uhlenbeck process in R
eq_price <- 5.0; vol_at <- 0.01
the_ta <- 0.01; len_gth <- 1000
set.seed(1121)  # reset random numbers
ou_sim <- sim_ou(len_gth=len_gth, eq_price=eq_price, vol_at=vol_at, the_ta=the_ta)
# define Ornstein-Uhlenbeck function in Rcpp
Rcpp::cppFunction("
NumericVector sim_ou_rcpp(double eq_price,
                double vol_at,
                double the_ta,
                NumericVector in_nov) {
  int len_gth = in_nov.size();
  NumericVector price_s(len_gth);
  NumericVector re_turns(len_gth);
  price_s[0] = eq_price;
  for (int it = 1; it < len_gth; it++) {
    re_turns[it] = the_ta*(eq_price - price_s[it-1]) + vol_at*in_nov[it-1];
    price_s[it] = price_s[it-1] * exp(re_turns[it]);
  }  // end for
  return price_s;
}")  # end cppFunction
# simulate Ornstein-Uhlenbeck process in Rcpp
set.seed(1121)  # reset random numbers
ou_sim_rcpp <- sim_ou_rcpp(eq_price=eq_price,
  vol_at=vol_at,
  the_ta=the_ta,
  in_nov=rnorm(len_gth))
all.equal(ou_sim, ou_sim_rcpp)
# compare speed of Rcpp and R
library(microbenchmark)
summary(microbenchmark(
  pure_r=sim_ou(len_gth=len_gth, eq_price=eq_price, vol_at=vol_at, the_ta=the_ta),
  r_cpp=sim_ou_rcpp(eq_price=eq_price, vol_at=vol_at, the_ta=the_ta, in_nov=rnorm(len_gth)),
  times=10))[, c(1, 4, 5)]
# source Rcpp function for Ornstein-Uhlenbeck process from file
Rcpp::sourceCpp(file="C:/Develop/R/lecture_slides/scripts/sim_ou.cpp")
# simulate Ornstein-Uhlenbeck process in Rcpp
set.seed(1121)  # reset random numbers
ou_sim_rcpp <- sim_ou_rcpp(eq_price=eq_price,
  vol_at=vol_at,
  the_ta=the_ta,
  in_nov=rnorm(len_gth))
all.equal(ou_sim, ou_sim_rcpp)
# compare speed of Rcpp and R
library(microbenchmark)
summary(microbenchmark(
  pure_r=sim_ou(len_gth=len_gth, eq_price=eq_price, vol_at=vol_at, the_ta=the_ta),
  r_cpp=sim_ou_rcpp(eq_price=eq_price, vol_at=vol_at, the_ta=the_ta, in_nov=rnorm(len_gth)),
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
library(RcppArmadillo)
# source Rcpp functions from file
Rcpp::sourceCpp(file="C:/Develop/R/lecture_slides/scripts/armadillo_functions.cpp")
vec1 <- runif(1e5)
vec2 <- runif(1e5)
vec_in(vec1, vec2)
vec1 %*% vec2
# microbenchmark RcppArmadillo code
summary(microbenchmark(
  vec_in=vec_in(vec1, vec2),
  r_code=(vec1 %*% vec2),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
# microbenchmark shows:
# vec_in() is several times faster than %*%, especially for longer vectors.
#     expr     mean   median
# 1 vec_in 110.7067 110.4530
# 2 r_code 585.5127 591.3575
library(RcppArmadillo)
# source Rcpp functions from file
Rcpp::sourceCpp(file="C:/Develop/R/lecture_slides/scripts/armadillo_functions.cpp")
mat_rix <- matrix(runif(1e5), nc=1e3)
# de-mean using apply()
new_mat <- apply(mat_rix, 2,
  function(x) (x-mean(x)))
# de-mean using demean_mat()
demean_mat(mat_rix)
all.equal(new_mat, mat_rix)
# microbenchmark RcppArmadillo code
summary(microbenchmark(
  demean_mat=demean_mat(mat_rix),
  apply=(apply(mat_rix, 2, mean)),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
# microbenchmark shows:
# demean_mat() is over 70 times faster than apply()
#         expr       mean   median
# 1 demean_mat   127.7539  125.604
# 2      apply 10781.7534 9291.674

# perform matrix inversion
# create random positive semi-definite matrix
mat_rix <- matrix(runif(25), nc=5)
mat_rix <- t(mat_rix) %*% mat_rix
# invert the matrix
matrix_inv <- solve(mat_rix)
inv_mat(mat_rix)
all.equal(inv_mat, mat_rix)
# microbenchmark RcppArmadillo code
library(microbenchmark)
summary(microbenchmark(
  inv_mat=inv_mat(mat_rix),
  solve=solve(mat_rix),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
# microbenchmark shows:
# inv_mat() is over 10 times faster than solve()
#      expr     mean median
# 1 inv_mat  3.42669  2.933
# 2 solve   32.00254 31.280
# source Rcpp functions from file
Rcpp::sourceCpp(file="C:/Develop/R/lecture_slides/scripts/sim_arima.cpp")
# define AR(2) coefficients
co_eff <- c(0.9, 0.09)
len_gth <- 1e4
set.seed(1121)
in_nov <- rnorm(len_gth)
# simulate ARIMA using filter()
arima_filter <- filter(x=in_nov,
  filter=co_eff, method="recursive")
# simulate ARIMA using sim_arima()
ari_ma <- sim_arima(in_nov, rev(co_eff))
all.equal(drop(ari_ma),
  as.numeric(arima_filter))
# microbenchmark RcppArmadillo code
summary(microbenchmark(
  filter=filter(x=in_nov, filter=co_eff, method="recursive"),
  sim_arima=sim_arima(in_nov, rev(co_eff)),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
# Number of data points
n_rows <- NROW(rutils::etf_env$VTI)
# Number of data points per interval
look_back <- 22
# Number of look_backs that fit over n_rows
num_agg <- n_rows %/% look_back
# Define end_points with beginning stub
end_points <-
  n_rows-look_back*num_agg + (0:num_agg)*look_back
n_rows <- NROW(end_points)
# Define contiguous start points as single-period lag of end_points
start_points <- c(1, end_points[1:(n_rows-1)])
# Define exclusive start points as single-period lag of end_points plus 1
start_points <- c(1, end_points[1:(n_rows-1)]+1)
len_gth <- 1e2
# Simulate AR(3) time series using filter()
co_eff <- c(0.1, 0.39, 0.5)
set.seed(1121)
se_ries <- filter(x=rnorm(len_gth),
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
set.seed(1121)
in_nov <- rnorm(len_gth)
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
ms_e <- mean(fore_casts-se_ries)^2
# Plot with legend
plot.default(se_ries,
  main="Rolling Forecasting Using AR(3) Model",
  xlab="", ylab="", type="l")
lines(fore_casts, col="orange", lwd=1)
legend(x="topright", legend=c("series","forecasts"),
 col=c("black", "orange"), lty=1, lwd=6,
 cex=0.9, bg="white", bty="n")
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
re_turns <- rnorm(1000)
sd(re_turns)
mad(re_turns)
median(abs(re_turns - median(re_turns)))
median(abs(re_turns - median(re_turns)))/qnorm(0.75)
# bootstrap of sd and mad estimators
boot_strap <- sapply(1:10000, function(x) {
  boot_sample <-
    re_turns[sample.int(n_rows, replace=TRUE)]
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
 lab_els, cex=0.8, lwd=2, lty=1,
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
       cex=0.8, lwd=6, lty=1,
       col=col_ors)
len_gth <- 1e5
da_ta <- rnorm(len_gth)
# mixture of two normal distributions with sd=1 and sd=2
re_turns <- c(da_ta[1:len_gth/2],
  2*da_ta[(len_gth/2+1):len_gth])
re_turns <- re_turns/sd(re_turns)
# fourth moment kurtosis
sum(da_ta**4)/(len_gth-1)
sum(re_turns**4)/(len_gth-1)
# plot the distributions
plot(density(re_turns), xlab="", ylab="",
  main="Mixture of Normal Returns",
  xlim=c(-3, 3), type="l", lwd=3, col="red")
curve(expr=dnorm, lwd=2, col="blue", add=TRUE)
curve(expr=dt(x, df=2), lwd=2, col="green", add=TRUE)
# add legend
legend("topright", inset=0.05, lty=1, lwd=6, bty="n",
  legend=c("Mixture", "Normal", "t-distribution"),
  col=c("red", "blue", "green"))
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
re_turns <- rutils::diff_it(log(quantmod::Cl(rutils::etf_env$VTI)))
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
# Fit VTI returns using MASS::fitdistr()
optim_fit <- MASS::fitdistr(re_turns,
  densfun="t", df=2)
optim_fit$estimate
optim_fit$sd
lo_cation <- optim_fit$estimate[1]
sc_ale <- optim_fit$estimate[2]
summary(optim_fit)
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
sc_ale <- optim_fit$estimate[2]
# Perform Kolmogorov-Smirnov test on VTI returns
sam_ple <- lo_cation + sc_ale*rt(NROW(re_turns), df=2)
ks.test(as.numeric(re_turns), sam_ple)
# Observed frequencies from random normal data
histo_gram <- hist(rnorm(1e3, mean=0), breaks=100, plot=FALSE)
freq_o <- histo_gram$counts
# Theoretical frequencies
freq_t <- diff(pnorm(histo_gram$breaks))
# Perform Chi-squared test for normal data
chisq.test(x=freq_o, p=freq_t, rescale.p=TRUE, simulate.p.value=TRUE)
# return p-value
chisq_test <- chisq.test(x=freq_o, p=freq_t, rescale.p=TRUE, simulate.p.value=TRUE)
chisq_test$p.value
# Observed frequencies from shifted normal data
histo_gram <- hist(rnorm(1e3, mean=2), breaks=100, plot=FALSE)
freq_o <- histo_gram$counts/sum(histo_gram$counts)
# Theoretical frequencies
freq_t <- diff(pnorm(histo_gram$breaks))
# Perform Chi-squared test for shifted normal data
chisq.test(x=freq_o, p=freq_t, rescale.p=TRUE, simulate.p.value=TRUE)
# Calculate histogram of VTI returns
histo_gram <- hist(re_turns, breaks=100, plot=FALSE)
freq_o <- histo_gram$counts
# Calculate cumulative probabilities and then difference them
freq_t <- pt((histo_gram$breaks-lo_cation)/sc_ale, df=2)
freq_t <- diff(freq_t)
# perform Chi-squared test for VTI returns
chisq.test(x=freq_o, p=freq_t, rescale.p=TRUE, simulate.p.value=TRUE)
# VTI percentage returns
re_turns <- rutils::diff_it(log(quantmod::Cl(rutils::etf_env$VTI)))
# define end points
end_points <- seq_along(re_turns)
n_rows <- NROW(end_points)
look_back <- 51
# start_points are multi-period lag of end_points
start_points <- c(rep_len(1, look_back-1),
    end_points[1:(n_rows-look_back+1)])
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
look_back <- 51
weight_s <- exp(-0.1*1:look_back)
weight_s <- weight_s/sum(weight_s)
vari_ance <- stats::filter(re_turns^2,
    filter=weight_s, sides=1)
vari_ance[1:(look_back-1)] <- vari_ance[look_back]
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
# calculate VTI variance using package roll
look_back <- 22
vari_ance <-
  roll::roll_var(re_turns, width=look_back)
vari_ance[1:(look_back-1)] <- 0
colnames(vari_ance) <- "VTI.variance"
# number of look_backs that fit over re_turns
n_rows <- NROW(re_turns)
num_agg <- n_rows %/% look_back
end_points <- # define end_points with beginning stub
  n_rows-look_back*num_agg + (0:num_agg)*look_back
n_rows <- NROW(end_points)
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
be_ta <- 0.2 ; n_rows <- 1000
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
x11(width=6, height=4)
par(mar=c(3, 3, 1, 1), oma=c(0, 0, 0, 0))
# plot GARCH cumulative returns
plot(cumsum(re_turns/100), t="l",
  lwd=2, col="orange", xlab="", ylab="",
  main="GARCH cumulative returns")
date_s <- seq.Date(from=Sys.Date()-n_rows+1,
  to=Sys.Date(), length.out=n_rows)
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
 lwd=6, lty=1,
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
  fGarch::garchSim(spec=garch_spec, n=n_rows)
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
 lwd=6, lty=1,
 col=c("blue", "red"))
library(HighFreq)  # load HighFreq
# minutely SPY returns (unit per minute) single day
re_turns <- rutils::diff_it(log(SPY["2012-02-13", 4]))
# minutely SPY volatility (unit per minute)
sd(re_turns)
# minutely SPY returns (unit per second)
re_turns <- rutils::diff_it(log(SPY["2012-02-13", 4])) / 
  c(1, diff(.index(SPY["2012-02-13"])))
# minutely SPY volatility scaled to unit per minute
60*sd(re_turns)
# minutely SPY returns multiple days no overnight scaling
re_turns <- rutils::diff_it(log(SPY[, 4]))
# minutely SPY volatility (unit per minute)
sd(re_turns)
# minutely SPY returns (unit per second)
re_turns <- rutils::diff_it(log(SPY[, 4])) / 
  c(1, diff(.index(SPY)))
# minutely SPY volatility scaled to unit per minute
60*sd(re_turns)
table(c(1, diff(.index(SPY))))
library(HighFreq)  # load HighFreq
# daily OHLC SPY prices
SPY_daily <- 
  rutils::to_period(oh_lc=SPY, period="days")
# daily SPY returns and volatility
sd(rutils::diff_it(log(SPY_daily[, 4])))
# minutely SPY returns (unit per minute)
re_turns <- rutils::diff_it(log(SPY[, 4]))
# minutely SPY volatility scaled to daily interval
sqrt(6.5*60)*sd(re_turns)

# minutely SPY returns (unit per second)
re_turns <- rutils::diff_it(log(SPY[, 4])) / 
  c(1, diff(.index(SPY)))
# minutely SPY volatility scaled to daily aggregation interval
60*sqrt(6.5*60)*sd(re_turns)

# daily SPY volatility
# including extra time over weekends and holidays
24*60*60*sd(rutils::diff_it(log(SPY_daily[, 4])) / 
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
  HighFreq::run_variance(oh_lc=etf_env$VTI,
                   calc_method="close")
var_yang_zhang <-
  HighFreq::run_variance(oh_lc=etf_env$VTI)
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
 bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")
library(rutils)
# end of month end_points
end_points <- rutils::calc_endpoints(re_turns,
          inter_val="months")
len_gth <- NROW(end_points)
# define 12-month look-back interval
look_back <- 12
# start_points are end_points lagged by look-back interval
start_points <- c(rep_len(1, look_back-1),
  end_points[1:(len_gth-look_back+1)])
# Perform loop over end_points and calculate aggregations
# agg_fun <- function(re_turns) sum(re_turns)/sd(re_turns)
agg_fun <- function(re_turns) sum(re_turns)
back_aggs <- sapply(1:(len_gth-1), function(it_er) {
  sapply(re_turns[start_points[it_er]:end_points[it_er]], agg_fun)
})  # end sapply
back_aggs <- t(back_aggs)
fwd_rets <- sapply(1:(len_gth-1), function(it_er) {
  sapply(re_turns[(end_points[it_er]+1):end_points[it_er+1]], sum)
})  # end sapply
fwd_rets <- t(fwd_rets)
# calculate weight_s proportional to back_aggs
weight_s <- back_aggs
weight_s[weight_s<0] <- 0
# scale weight_s so their sum is equal to 1
weight_s <- weight_s/rowSums(weight_s)
# set NA values to zero
weight_s[is.na(weight_s)] <- 0
sum(is.na(weight_s))
in_dex <- index(re_turns[end_points[-len_gth]])
trend_weights <- rowMeans(weight_s[, 1:NCOL(trend_returns)])
revert_weights <- rowMeans(weight_s[, -(1:NCOL(trend_returns))])
diff_weights <- xts(trend_weights-revert_weights, order.by=in_dex)
close_adj <- (cl_ose - as.numeric(cl_ose[1, ]))
# de-mean weight_s so their sum is equal to 0
# weight_s <- weight_s - rowMeans(weight_s)
# find best and worst EWMA Strategies in each period
bes_t <- apply(weight_s, 1, which.max)
wors_t <- apply(weight_s, 1, which.min)
# plot the mean weights of EWMA Strategies
zoo::plot.zoo(cbind(diff_weights,
  close_adj[end_points[-len_gth]]),
  oma = c(3, 0, 3, 0), mar = c(0, 4, 0, 1),
  xlab=NULL, ylab=c("diff weights", "VTI"),
  main="Trend minus Revert Weights of EWMA strategies")
best_worst <- xts(cbind(bes_t, wors_t), order.by=in_dex)
zoo::plot.zoo(best_worst,
  oma = c(3, 0, 3, 0), mar = c(0, 4, 0, 1),
  xlab=NULL, ylab=c("best EWMA", "worst EWMA"),
  main="Best and Worst EWMA strategies")
# calculate backtest returns
pnl_s <- rowSums(weight_s*fwd_rets)
pnl_s <- xts(pnl_s, order.by=in_dex)
colnames(pnl_s) <- "ewma momentum"
close_rets <- rutils::diff_it(cl_ose[in_dex])
cor(cbind(pnl_s, close_rets))
pnl_s <- cumsum(pnl_s)
# plot the backtest
chart_Series(x=close_adj[end_points[-len_gth]],
  name="Back-test of EWMA strategies", col="orange")
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
price_s <- zoo::na.locf(price_s)
price_s <- na.omit(price_s)
re_turns <- rutils::diff_it(price_s)
# Define look-back and look-forward intervals
end_points <- rutils::calc_endpoints(re_turns,
  inter_val="months")
n_col <- NCOL(re_turns)
len_gth <- NROW(end_points)
look_back <- 12
start_points <- c(rep_len(1, look_back-1),
  end_points[1:(len_gth-look_back+1)])
# Perform loop over end-points and calculate aggregations
agg_fun <-
  function(re_turns) sum(re_turns)/sd(re_turns)
agg_s <- sapply(1:(len_gth-1), function(it_er) {
  c(back_aggs=sapply(re_turns[start_points[it_er]:end_points[it_er]], agg_fun),
    fwd_rets=sapply(re_turns[(end_points[it_er]+1):end_points[it_er+1]], sum))
})  # end sapply
agg_s <- t(agg_s)
# Select look-back and look-forward aggregations
back_aggs <- agg_s[, 1:n_col]
fwd_rets <- agg_s[, n_col+1:n_col]
# Calculate portfolio weights equal to number of shares
end_prices <- price_s[end_points[-len_gth]]
weight_s <-
  back_aggs/rowSums(abs(back_aggs))/end_prices
weight_s[is.na(weight_s)] <- 0
colnames(weight_s) <- colnames(re_turns)
# Calculate profits and losses
pnl_s <- rowSums(weight_s*fwd_rets)
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
cl_ose <- Cl(rutils::etf_env$VTI[index(end_prices)])
zoo::plot.zoo(cbind(cl_ose, pnl_s, weight_s),
  oma = c(3, 1, 3, 0), mar = c(0, 4, 0, 1), nc=1,
  xlab=NULL, main="ETF Momentum Strategy")
# define back-test functional
back_test_ep <- function(re_turns, price_s, agg_fun=sum,
    look_back=12, re_balance="months", bid_offer=0.001,
    end_points=rutils::calc_endpoints(re_turns, inter_val=re_balance),
    with_weights=FALSE, ...) {
  stopifnot("package:quantmod" %in% search() || require("quantmod", quietly=TRUE))
  # Define look-back and look-forward intervals
  n_col <- NCOL(re_turns)
  len_gth <- NROW(end_points)
  start_points <- c(rep_len(1, look_back-1), end_points[1:(len_gth-look_back+1)])
  # Perform loop over end-points and calculate aggregations
  agg_s <- sapply(1:(len_gth-1), function(it_er) {
    c(back_aggs=sapply(re_turns[start_points[it_er]:end_points[it_er]], agg_fun, ...),  # end sapply
    fwd_rets=sapply(re_turns[(end_points[it_er]+1):end_points[it_er+1]], sum))  # end sapply
  })  # end sapply
  agg_s <- t(agg_s)
  # Select look-back and look-forward aggregations
  back_aggs <- agg_s[, 1:n_col]
  fwd_rets <- agg_s[, n_col+1:n_col]
  # Calculate portfolio weights equal to number of shares
  end_prices <- price_s[end_points[-len_gth]]
  weight_s <- back_aggs/rowSums(abs(back_aggs))/end_prices
  weight_s[is.na(weight_s)] <- 0
  colnames(weight_s) <- colnames(re_turns)
  # Calculate profits and losses
  pnl_s <- rowSums(weight_s*fwd_rets)
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
}  # end back_test_ep
source("C:/Develop/R/lecture_slides/scripts/back_test.R")
look_backs <- seq(5, 60, by=5)
agg_fun <- function(re_turns) sum(re_turns)/sd(re_turns)
pro_files <- sapply(look_backs, function(x) {
  last(back_test_ep(re_turns=re_turns, price_s=price_s,
    re_balance="weeks", look_back=x, agg_fun=agg_fun))
})  # end sapply
plot(x=look_backs, y=pro_files, t="l",
  main="Strategy PnL as function of look_back",
  xlab="look_back (weeks)", ylab="pnl")
look_back <- look_backs[which.max(pro_files)]
pnl_s <- back_test_ep(re_turns=re_turns, price_s=price_s,
  re_balance="weeks", look_back=look_back, agg_fun=agg_fun,
  with_weights=TRUE)
cl_ose <- Cl(rutils::etf_env$VTI[index(pnl_s)])
# bind model returns with VTI
da_ta <- as.numeric(cl_ose[1, ])
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
all_weather <- as.numeric(cl_ose[1, ])*all_weather +
  as.numeric(cl_ose[1, ])
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
# sym_bols contains all the symbols in rutils::etf_env$re_turns except for "VXX"
sym_bols <- colnames(rutils::etf_env$re_turns)
sym_bols <- sym_bols[!(sym_bols=="VXX")]
# Extract columns of rutils::etf_env$re_turns and remove NA values
re_turns <- rutils::etf_env$re_turns[, sym_bols]
re_turns <- zoo::na.locf(re_turns)
re_turns <- na.omit(re_turns)
# Calculate vector of monthly end points and start points
look_back <- 12
end_points <- rutils::calc_endpoints(re_turns, inter_val="months")
end_points[end_points<2*NCOL(re_turns)] <- 2*NCOL(re_turns)
len_gth <- NROW(end_points)
# sliding window
start_points <- c(rep_len(1, look_back-1), end_points[1:(len_gth-look_back+1)])
# OR expanding window
# start_points <- rep_len(1, NROW(end_points))
# risk_free is the daily risk-free rate
risk_free <- 0.03/260
# Calculate daily excess returns
ex_cess <- re_turns - risk_free
# Perform loop over end_points
portf_rets <- lapply(2:NROW(end_points),
  function(i) {
    # subset the ex_cess returns
    ex_cess <- ex_cess[start_points[i-1]:end_points[i-1], ]
    in_verse <- solve(cov(ex_cess))
    # calculate the maximum Sharpe ratio portfolio weights.
    weight_s <- in_verse %*% colMeans(ex_cess)
    weight_s <- drop(weight_s/sqrt(sum(weight_s^2)))
    # subset the re_turns
    re_turns <- re_turns[(end_points[i-1]+1):end_points[i], ]
    # calculate the out-of-sample portfolio returns
    xts(re_turns %*% weight_s, index(re_turns))
  }  # end anonymous function
)  # end lapply
portf_rets <- rutils::do_call(rbind, portf_rets)
colnames(portf_rets) <- "portf_rets"
# Calculate compounded cumulative portfolio returns
portf_rets <- cumsum(portf_rets)
quantmod::chart_Series(portf_rets,
  name="Cumulative Returns of Max Sharpe Portfolio Strategy")
# load S&P500 constituent stock prices
load("C:/Develop/R/lecture_slides/data/sp500.RData")
price_s <- eapply(env_sp500, quantmod::Cl)
price_s <- rutils::do_call(cbind, price_s)
# carry forward and backward non-NA prices
price_s <- zoo::na.locf(price_s)
price_s <- zoo::na.locf(price_s, fromLast=TRUE)
colnames(price_s) <- sapply(colnames(price_s),
  function(col_name) strsplit(col_name, split="[.]")[[1]][1])
# Calculate the simple (dollar) returns of the S&P500 constituent stocks
re_turns <- rutils::diff_it(price_s)
save(price_s, re_turns,
  file="C:/Develop/R/lecture_slides/data/sp500_prices.RData")
# Calculate number of constituents without prices
da_ta <- rowSums(rutils::roll_sum(re_turns, 4)==0)
da_ta <- xts::xts(da_ta, order.by=index(price_s))
dygraphs::dygraph(da_ta, main="Number of S&P 500 Constituents Without Prices") %>%
  dyAxis("y", valueRange=c(0, 300))
# Calculate price weighted index of constituent
n_col <- NCOL(price_s)
in_dex <- xts(rowSums(price_s)/n_col, index(price_s))
colnames(in_dex) <- "index"
# Combine index with VTI
da_ta <- cbind(in_dex[index(etf_env$VTI)], etf_env$VTI[, 4])
col_names <- c("index", "VTI")
colnames(da_ta) <- col_names
# Plot index with VTI
dygraphs::dygraph(da_ta,
  main="S&P 500 Price-weighted Index and VTI") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="red") %>%
  dySeries(name=col_names[2], axis="y2", col="blue")
load("C:/Develop/R/lecture_slides/data/sp500_prices.RData")
# calculate rolling variance of S&P500 portfolio
wid_th <- 252
vari_ance <- roll::roll_var(re_turns, width=wid_th)
vari_ance <- zoo::na.locf(vari_ance)
vari_ance[is.na(vari_ance)] <- 0
# calculate rolling Sharpe of S&P500 portfolio
returns_width <- rutils::diff_it(price_s, lagg=wid_th)
weight_s <- returns_width/sqrt(wid_th*vari_ance)
weight_s[vari_ance==0] <- 0
weight_s[1:wid_th, ] <- 1
weight_s[is.na(weight_s)] <- 0
weight_s <- weight_s/rowSums(abs(weight_s))/price_s
weight_s[is.na(weight_s)] <- 0
weight_s <- rutils::lag_it(weight_s)
sum(is.na(weight_s))
# calculate portfolio profits and losses
pnl_s <- rowSums(weight_s*re_turns)
# Calculate transaction costs
bid_offer <- 0.001
cost_s <- 0.5*bid_offer*price_s*abs(rutils::diff_it(weight_s))
cost_s <- rowSums(cost_s)
pnl_s <- (pnl_s - cost_s)
pnl_s <- cumsum(pnl_s)
pnl_s <- xts(pnl_s, order.by=index(price_s))
pnl_s <- cbind(rutils::etf_env$VTI[, 4], pnl_s)
pnl_s <- na.omit(pnl_s)
colnames(pnl_s) <- c("VTI", "momentum")
col_names <- colnames(pnl_s)
# plot momentum and VTI
dygraphs::dygraph(pnl_s, main=paste(col_names, collapse=" and ")) %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="blue") %>%
  dySeries(name=col_names[2], axis="y2", col="red")
# define back-test functional
back_test_rolling <- function(re_turns, price_s,
    wid_th=252, bid_offer=0.001, tre_nd=1, ...) {
  stopifnot("package:quantmod" %in% search() || require("quantmod", quietly=TRUE))
  # Define look-back and look-forward intervals
  n_col <- NCOL(re_turns)
  vari_ance <- roll::roll_var(re_turns, width=wid_th)
  vari_ance <- zoo::na.locf(vari_ance)
  vari_ance[is.na(vari_ance)] <- 0
  # calculate rolling Sharpe of S&P500 portfolio
  returns_width <- rutils::diff_it(price_s, lagg=wid_th)
  weight_s <- tre_nd*returns_width/sqrt(wid_th*vari_ance)
  weight_s[vari_ance==0] <- 0
  weight_s[1:wid_th, ] <- 1
  weight_s[is.na(weight_s)] <- 0
  weight_s <- weight_s/rowSums(abs(weight_s))/price_s
  weight_s[is.na(weight_s)] <- 0
  weight_s <- rutils::lag_it(weight_s)
  # calculate portfolio profits and losses
  pnl_s <- rowSums(weight_s*re_turns)
  # Calculate transaction costs
  bid_offer <- 0.001
  cost_s <- 0.5*bid_offer*price_s*abs(rutils::diff_it(weight_s))
  cost_s <- rowSums(cost_s)
  pnl_s <- (pnl_s - cost_s)
  pnl_s <- cumsum(pnl_s)
  pnl_s
}  # end back_test_rolling
source("C:/Develop/R/lecture_slides/scripts/back_test.R")
pnl_s <- back_test_rolling(wid_th=252, re_turns=re_turns,
  price_s=price_s, bid_offer=bid_offer)
width_s <- seq(50, 300, by=50)
# perform sapply loop over lamb_das
pro_files <- sapply(width_s, back_test_rolling, re_turns=re_turns,
  price_s=price_s, bid_offer=bid_offer)
colnames(pro_files) <- paste0("width=", width_s)
pro_files <- xts(pro_files, index(price_s))
# plot EWMA strategies with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(pro_files))
chart_Series(pro_files,
  theme=plot_theme, name="Cumulative Returns of S&P500 Momentum Strategies")
legend("bottomleft", legend=colnames(pro_files),
  inset=0.0, bg="white", cex=0.7, lwd=rep(6, NCOL(re_turns)),
  col=plot_theme$col$line.col, bty="n")
width_s <- seq(5, 50, by=5)
# perform sapply loop over lamb_das
pro_files <- sapply(width_s, back_test_rolling, re_turns=re_turns,
  price_s=price_s, bid_offer=bid_offer, tre_nd=(-1))
colnames(pro_files) <- paste0("width=", width_s)
pro_files <- xts(pro_files, index(price_s))
# plot EWMA strategies with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(pro_files))
chart_Series(pro_files,
  theme=plot_theme, name="Cumulative Returns of S&P500 Mean-reverting Strategies")
legend("bottomleft", legend=colnames(pro_files),
  inset=0.0, bg="white", cex=0.7, lwd=rep(6, NCOL(re_turns)),
  col=plot_theme$col$line.col, bty="n")
library(rutils)
load("C:/Develop/R/lecture_slides/data/sp500_prices.RData")
n_col <- NCOL(price_s)
date_s <- index(price_s)
in_dex <- xts(rowSums(price_s)/n_col, index(price_s))
colnames(in_dex) <- "index"
# define end_points
end_points <- rutils::calc_endpoints(price_s, inter_val="months")
end_points <- end_points[end_points>50]
len_gth <- NROW(end_points)
look_back <- 12
start_points <- c(rep_len(1, look_back-1), end_points[1:(len_gth-look_back+1)])
# run backtest function
al_pha <- 0.01
max_eigen <- 3
strat_rets_arma <- HighFreq::roll_portf(re_turns,
  re_turns,
  start_points-1,
  end_points-1,
  al_pha=al_pha,
  max_eigen=max_eigen)
# plot strategy
strat_rets_arma <- cumsum(strat_rets_arma)
strat_rets_arma <- xts(strat_rets_arma, date_s)
strat_rets_arma <- cbind(strat_rets_arma, in_dex)
col_names <- c("Strategy", "Index")
colnames(strat_rets_arma) <- col_names
dygraphs::dygraph(strat_rets_arma, main=paste(col_names, collapse=" and ")) %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="red") %>%
  dySeries(name=col_names[2], axis="y2", col="blue")
