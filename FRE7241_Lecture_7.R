va_r <- 0.3/3
va_r  # Printed as "0.1"
va_r - 0.1  # va_r is not equal to "0.1"
va_r == 0.1  # va_r is not equal to "0.1"
print(va_r, digits=10)
print(va_r, digits=16)
# va_r is equal to "0.1" within machine precision
all.equal(va_r, 0.1)
va_r <- (3-2.9)
print(va_r, digits=20)
# Info machine precision of computer R is running on
# ?.Machine
# Machine precision
.Machine$double.eps

va_r <- sqrt(2)
va_r^2  # Printed as "2"
va_r^2 == 2  # va_r^2 is not equal to "2"
print(va_r^2, digits=20)
# va_r^2 is equal to "2" within machine precision
all.equal(va_r^2, 2)
# Numbers with precision 0.1
0.1*(1:10)
# Round to precision 0.1
round(3.675, 1)
# Round to precision 1.0
round(3.675)
# Round to nearest even number
c(round(2.5), round(3.5), round(4.5))
round(4:20/2)  # Round to nearest even number
trunc(3.675)  # Truncate

num_var <- 2
num_var==2
identical(num_var, 2)

identical(num_var, NULL)
# This doesn't work:
# num_var==NULL
is.null(num_var)

vec_tor <- c(2, 4, 6)
vec_tor==2
identical(vec_tor, 2)

# num_ber is equal to "1.0" within machine precision
num_ber <- 1.0 + 2*sqrt(.Machine$double.eps)
all.equal(num_ber, 1.0)

# Info machine precision of computer R is running on
# ?.Machine
# Machine precision
.Machine$double.eps

4.7 %/% 0.5  # Modulo division
4.7 %% 0.5  # Remainder of modulo division
# Reversing modulo division usually
# Returns the original number
(4.7 %% 0.5) + 0.5 * (4.7 %/% 0.5)
# Modulo division of non-integer numbers can
# Produce incorrect results
0.6 %/% 0.2  # Produces 2 instead of 3
6 %/% 2  # use integers to get correct result
# 0.2 stored as binary number
# Slightly larger than 0.2
print(0.2, digits=22)

# Get help for integrate()
?integrate
# Calculate slowly converging integral
func_tion <- function(x) {1/((x+1)*sqrt(x))}
integrate(func_tion, lower=0, upper=10)
integrate(func_tion, lower=0, upper=Inf)
# Integrate function with parameter lamb_da
func_tion <- function(x, lamb_da=1) {
  exp(-x*lamb_da)
}  # end func_tion
integrate(func_tion, lower=0, upper=Inf)
integrate(func_tion, lower=0, upper=Inf,
    lamb_da=2)
# Cumulative probability over normal distribution
pnorm(-2)
integrate(dnorm, low=2, up=Inf)
str(dnorm)
pnorm(-1)
integrate(dnorm, low=2, up=Inf, mean=1)
# Expected value over normal distribution
integrate(function(x) x*dnorm(x),
    low=2, up=Inf)

# Get size of an object
vec_tor <- runif(1e6)
object.size(vec_tor)
format(object.size(vec_tor), units="MB")
# Get sizes of objects in workspace
sort(sapply(ls(), function(ob_ject) {
  format(object.size(get(ob_ject)), units="KB")}))
# Get sizes of all objects in workspace
sort(sapply(mget(ls()), object.size))
sort(sapply(mget(ls()), function(ob_ject) {
  format(object.size(ob_ject), units="KB")}
))
# Get total size of all objects in workspace
format(object.size(x=mget(ls())), units="MB")
# Get sizes of objects in rutils::etf_env environment
sort(sapply(ls(rutils::etf_env), function(ob_ject) {
  object.size(get(ob_ject, rutils::etf_env))}))
sort(sapply(mget(ls(rutils::etf_env), rutils::etf_env),
      object.size))
library(gdata)  # Load package gdata
# Get size of data frame columns
gdata::ll(unit="bytes", mtcars)
# Get names, class, and size of objects in workspace
ob_jects <- gdata::ll(unit="bytes")
# Sort by memory size (descending)
ob_jects[order(ob_jects[, 2], decreasing=TRUE), ]
gdata::ll()[order(ll()$KB, decreasing=TRUE), ]
# Get sizes of objects in etf_env environment
gdata::ll(unit="bytes", etf_env)

library(SOAR)  # Load package SOAR
# Get sizes of objects in workspace
sort(sapply(mget(ls()), object.size))
Store(etf_list)  # Store in object cache
# Get sizes of objects in workspace
sort(sapply(mget(ls()), object.size))
search()  # Get search path for R objects
Ls()  # list object cache
find("etf_list")  # Find object on search path

# Get R memory
v_cells <- gc()["Vcells", "used"]
# Create vector with 1,000,000 cells
va_r_bar <- numeric(1000000)
# Get extra R memory
gc()["Vcells", "used"] - v_cells
# Get total size of all objects in workspace
print(object.size(x=mget(ls())), units="MB")

library(microbenchmark)
va_r <- runif(1e6)
system.time(va_r^0.5)
microbenchmark(sqrt(va_r), va_r^0.5, times=10)

# Calculate the square root of a vector
vec_tor <- runif(1e6)
all.equal(sqrt(vec_tor), vec_tor^0.5)
# Microbenchmark the two methods
library(microbenchmark)
summary(microbenchmark(
  sqrt(vec_tor),
  vec_tor^0.5,
  times=10))[, c(1, 4, 5)]

library(microbenchmark)
# sum() is a compiled primitive function
sum
# mean() is a generic function
mean
va_r <- runif(1e6)
# sum() is much faster than mean()
summary(
  microbenchmark(sum(va_r), mean(va_r), times=10)
  )[, c(1, 4, 5)]
# any() is a compiled primitive function
any
# any() is much faster than %in% wrapper for match()
summary(
  microbenchmark(any(va_r == 1), {1 %in% va_r}, times=10)
  )[, c(1, 4, 5)]

library(microbenchmark)
mat_rix <- matrix(1:9, ncol=3, # Create matrix
  dimnames=list(paste0("row", 1:3),
          paste0("col", 1:3)))
# Create specialized function
matrix_to_dframe <- function(mat_rix) {
  n_cols <- ncol(mat_rix)
  dframe <- vector("list", n_cols)  # empty vector
  for (in_dex in 1:n_cols)  # Populate vector
    dframe <- mat_rix[, in_dex]
  attr(dframe, "row.names") <-  # Add attributes
    .set_row_names(NROW(mat_rix))
  attr(dframe, "class") <- "data.frame"
  dframe  # Return data frame
}  # end matrix_to_dframe
# Compare speed of three methods
summary(microbenchmark(
  matrix_to_dframe(mat_rix),
  as.data.frame.matrix(mat_rix),
  as.data.frame(mat_rix),
  times=10))[, c(1, 4, 5)]

# Matrix with 5,000 rows
mat_rix <- matrix(rnorm(10000), ncol=2)
# Allocate memory for row sums
row_sums <- numeric(NROW(mat_rix))
summary(microbenchmark(
  row_sums=rowSums(mat_rix),  # end row_sums
  ap_ply=apply(mat_rix, 1, sum),  # end apply
  l_apply=lapply(1:NROW(mat_rix), function(in_dex)
    sum(mat_rix[in_dex, ])),  # end lapply
  v_apply=vapply(1:NROW(mat_rix), function(in_dex)
    sum(mat_rix[in_dex, ]),
    FUN.VALUE=c(sum=0)),  # end vapply
  s_apply=sapply(1:NROW(mat_rix), function(in_dex)
    sum(mat_rix[in_dex, ])),  # end sapply
  for_loop=for (i in 1:NROW(mat_rix)) {
    row_sums[i] <- sum(mat_rix[i,])
  },  # end for
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

big_vector <- rnorm(5000)
summary(microbenchmark(
# Allocate full memory for cumulative sum
  for_loop={cum_sum <- numeric(NROW(big_vector))
    cum_sum[1] <- big_vector[1]
    for (i in 2:NROW(big_vector)) {
      cum_sum[i] <- cum_sum[i-1] + big_vector[i]
    }},  # end for
# Allocate zero memory for cumulative sum
  grow_vec={cum_sum <- numeric(0)
    cum_sum[1] <- big_vector[1]
    for (i in 2:NROW(big_vector)) {
# Add new element to "cum_sum" ("grow" it)
      cum_sum[i] <- cum_sum[i-1] + big_vector[i]
    }},  # end for
# Allocate zero memory for cumulative sum
  com_bine={cum_sum <- numeric(0)
    cum_sum[1] <- big_vector[1]
    for (i in 2:NROW(big_vector)) {
# Add new element to "cum_sum" ("grow" it)
      cum_sum <- c(cum_sum, big_vector[i])
    }},  # end for
  times=10))[, c(1, 4, 5)]

# Disable JIT
jit_level <- compiler::enableJIT(0)
# Create inefficient function
my_mean <- function(x) {
  out_put <- 0; n_elem <- NROW(x)
  for(it in 1:n_elem)
    out_put <- out_put + x[it]/n_elem
  out_put
}  # end my_mean
# Byte-compile function and inspect it
mymean_comp <- compiler::cmpfun(my_mean)
mymean_comp
# Test function
vec_tor <- runif(1e3)
all.equal(mean(vec_tor), mymean_comp(vec_tor), my_mean(vec_tor))
# microbenchmark byte-compile function
summary(microbenchmark(
  mean(vec_tor),
  mymean_comp(vec_tor),
  my_mean(vec_tor),
  times=10))[, c(1, 4, 5)]
# Create another inefficient function
sapply2 <- function(x, FUN, ...) {
  out_put <- vector(length=NROW(x))
  for (it in seq_along(x))
    out_put[it] <- FUN(x[it], ...)
  out_put
}  # end sapply2
sapply2_comp <- compiler::cmpfun(sapply2)
all.equal(sqrt(vec_tor),
  sapply2(vec_tor, sqrt),
  sapply2_comp(vec_tor, sqrt))
summary(microbenchmark(
  sqrt(vec_tor),
  sapply2_comp(vec_tor, sqrt),
  sapply2(vec_tor, sqrt),
  times=10))[, c(1, 4, 5)]
# enable JIT
compiler::enableJIT(jit_level)

# Define functions for profiling
out_er <- function() {fa_st(); sl_ow()}
fa_st <- function() Sys.sleep(0.1)
sl_ow <- function() Sys.sleep(0.2)
# Turn on profiling
Rprof(filename="C:/Develop/data_def/profile.out")
# Run code for profiling
replicate(n=10, out_er())
# Turn off profiling
Rprof(NULL)
# Compile summary of profiling from file
summaryRprof("C:/Develop/data_def/profile.out")

# Profile plotting of regression
profvis::profvis({
  plot(price ~ carat, data=ggplot2::diamonds)
  mod_el <- lm(price ~ carat, data=ggplot2::diamonds)
  abline(mod_el, col="red")
})  # end profvis
# Four methods of calculating matrix column means
mat_rix <- matrix(rnorm(1e5), ncol=5e4)
profvis::profvis({
  mean_s <- apply(mat_rix, 2, mean)
  mean_s <- colMeans(mat_rix)
  mean_s <- lapply(mat_rix, mean)
  mean_s <- vapply(mat_rix, mean, numeric(1))
})  # end profvis
# Four methods of calculating data frame column means
data_frame <- as.data.frame(mat_rix)
profvis::profvis({
  mean_s <- apply(data_frame, 2, mean)
  mean_s <- colMeans(data_frame)
  mean_s <- lapply(data_frame, mean)
  mean_s <- vapply(data_frame, mean, numeric(1))
})  # end profvis
# Profile a shiny app
profvis::profvis(
  shiny::runExample(example="06_tabsets",
            display.mode="normal")
)  # end profvis

vec_tor1 <- rnorm(1000000)
vec_tor2 <- rnorm(1000000)
big_vector <- numeric(1000000)
# Sum two vectors in two different ways
summary(microbenchmark(
  # Sum vectors using "for" loop
  r_loop=(for (i in 1:NROW(vec_tor1)) {
    big_vector[i] <- vec_tor1[i] + vec_tor2[i]
  }),
  # Sum vectors using vectorized "+"
  vec_torized=(vec_tor1 + vec_tor2),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Allocate memory for cumulative sum
cum_sum <- numeric(NROW(big_vector))
cum_sum[1] <- big_vector[1]
# Calculate cumulative sum in two different ways
summary(microbenchmark(
# Cumulative sum using "for" loop
  r_loop=(for (i in 2:NROW(big_vector)) {
    cum_sum[i] <- cum_sum[i-1] + big_vector[i]
  }),
# Cumulative sum using "cumsum"
  vec_torized=cumsum(big_vector),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# Matrix with 5,000 rows
mat_rix <- matrix(rnorm(10000), ncol=2)
# Calculate row sums two different ways
all.equal(rowSums(mat_rix),
  apply(mat_rix, 1, sum))
summary(microbenchmark(
  row_sums=rowSums(mat_rix),
  ap_ply=apply(mat_rix, 1, sum),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

library(microbenchmark)
str(pmax)
# Calculate row maximums two different ways
summary(microbenchmark(
  p_max=
    do.call(pmax.int,
lapply(seq_along(mat_rix[1, ]),
  function(in_dex) mat_rix[, in_dex])),
  l_apply=unlist(
    lapply(seq_along(mat_rix[, 1]),
  function(in_dex) max(mat_rix[in_dex, ]))),
  times=10))[, c(1, 4, 5)]

install.packages("matrixStats")  # Install package matrixStats
library(matrixStats)  # Load package matrixStats
# Calculate row min values three different ways
summary(microbenchmark(
  row_mins=rowMins(mat_rix),
  p_min=
    do.call(pmin.int,
      lapply(seq_along(mat_rix[1, ]),
             function(in_dex)
               mat_rix[, in_dex])),
  as_data_frame=
    do.call(pmin.int,
      as.data.frame.matrix(mat_rix)),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

install.packages("Rfast")  # Install package Rfast
library(Rfast)  # Load package Rfast
# Benchmark speed of calculating ranks
va_r <- 1e3
all.equal(rank(va_r), Rfast::Rank(va_r))
summary(microbenchmark(
  r=rank(va_r),
  fast=Rank(va_r),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Benchmark speed of calculating column medians
va_r <- matrix(1e4, nc=10)
all.equal(matrixStats::colMedians(va_r), Rfast::colMedians(va_r))
summary(microbenchmark(
  r=matrixStats::colMedians(va_r),
  fast=Rfast::colMedians(va_r),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

summary(microbenchmark(  # Assign values to vector three different ways
# Fast vectorized assignment loop performed in C using brackets "[]"
  brack_ets={vec_tor <- numeric(10)
    vec_tor[] <- 2},
# Slow because loop is performed in R
  for_loop={vec_tor <- numeric(10)
    for (in_dex in seq_along(vec_tor))
      vec_tor[in_dex] <- 2},
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
summary(microbenchmark(  # Assign values to vector two different ways
# Fast vectorized assignment loop performed in C using brackets "[]"
  brack_ets={vec_tor <- numeric(10)
    vec_tor[4:7] <- rnorm(4)},
# Slow because loop is performed in R
  for_loop={vec_tor <- numeric(10)
    for (in_dex in 4:7)
      vec_tor[in_dex] <- rnorm(1)},
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# Define function vectorized automatically
my_fun <- function(in_put, pa_ram) {
  pa_ram*in_put
}  # end my_fun
# "in_put" is vectorized
my_fun(in_put=1:3, pa_ram=2)
# "pa_ram" is vectorized
my_fun(in_put=10, pa_ram=2:4)
# Define vectors of parameters of rnorm()
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
# Call vec_rnorm() on vector of "sd"
vec_rnorm(n=2, sd=std_devs)
# Call vec_rnorm() on vector of "mean"
vec_rnorm(n=2, mean=me_ans)

# Create two numeric vectors
vec_tor1 <- sin(0.25*pi*1:10)
vec_tor2 <- cos(0.25*pi*1:10)
# Create third vector using 'ifelse'
vec_tor3 <- ifelse(vec_tor1 > vec_tor2,
          vec_tor1, vec_tor2)
# cbind all three together
vec_tor4 <- cbind(vec_tor1, vec_tor2, vec_tor3)

# Set plotting parameters
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0),
    cex.lab=0.8, cex.axis=0.8, cex.main=0.8,
    cex.sub=0.5)
# Plot matrix
matplot(vec_tor4, type="l", lty="solid",
col=c("green", "blue", "red"),
lwd=2, xlab="", ylab="")
# Add legend
legend(x="bottomright", legend=colnames(vec_tor4),
       title="", inset=0.05, cex=0.8, lwd=2,
       lty=1, col=c("green", "blue", "red"))

# Verify that rtools are working properly:
devtools::find_rtools()
devtools::has_devel()

# Load package Rcpp
library(Rcpp)
# Get documentation for package Rcpp
# Get short description
packageDescription("Rcpp")
# Load help page
help(package="Rcpp")
# list all datasets in "Rcpp"
data(package="Rcpp")
# list all objects in "Rcpp"
ls("package:Rcpp")
# Remove Rcpp from search path
detach("package:Rcpp")

# Define Rcpp function
Rcpp::cppFunction("
  int times_two(int x)
    { return 2 * x;}
  ")  # end cppFunction
# Run Rcpp function
times_two(3)
# Source Rcpp functions from file
Rcpp::sourceCpp(file="C:/Develop/R/lecture_slides/scripts/mult_rcpp.cpp")
# Multiply two numbers
mult_rcpp(2, 3)
mult_rcpp(1:3, 6:4)
# Multiply two vectors
mult_vec_rcpp(2, 3)
mult_vec_rcpp(1:3, 6:4)

# Define Rcpp function with loop
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
# Run Rcpp function
inner_mult(1:3, 6:4)
inner_mult(1:3, 6:3)
# Define Rcpp Sugar function with loop
Rcpp::cppFunction("
double inner_mult_sugar(NumericVector x, NumericVector y) {
  return sum(x * y);
}")  # end cppFunction
# Run Rcpp Sugar function
inner_mult_sugar(1:3, 6:4)
inner_mult_sugar(1:3, 6:3)

# Define R function with loop
inner_mult_r <- function(x, y) {
    to_tal <- 0
    for(i in 1:NROW(x)) {
to_tal <- to_tal + x[i] * y[i]
    }
    to_tal
}  # end inner_mult_r
# Run R function
inner_mult_r(1:3, 6:4)
inner_mult_r(1:3, 6:3)
# Compare speed of Rcpp and R
library(microbenchmark)
summary(microbenchmark(
  pure_r=inner_mult_r(1:10000, 1:10000),
  inner_r=1:10000 %*% 1:10000,
  r_cpp=inner_mult(1:10000, 1:10000),
  r_cpp_sugar=inner_mult_sugar(1:10000, 1:10000),
  times=10))[, c(1, 4, 5)]

# Define Ornstein-Uhlenbeck function in R
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
# Simulate Ornstein-Uhlenbeck process in R
eq_price <- 5.0; vol_at <- 0.01
the_ta <- 0.01; len_gth <- 1000
set.seed(1121)  # Reset random numbers
ou_sim <- sim_ou(len_gth=len_gth, eq_price=eq_price, vol_at=vol_at, the_ta=the_ta)

# Define Ornstein-Uhlenbeck function in Rcpp
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
# Simulate Ornstein-Uhlenbeck process in Rcpp
set.seed(1121)  # Reset random numbers
ou_sim_rcpp <- sim_ou_rcpp(eq_price=eq_price,
  vol_at=vol_at,
  the_ta=the_ta,
  in_nov=rnorm(len_gth))
all.equal(ou_sim, ou_sim_rcpp)
# Compare speed of Rcpp and R
library(microbenchmark)
summary(microbenchmark(
  pure_r=sim_ou(len_gth=len_gth, eq_price=eq_price, vol_at=vol_at, the_ta=the_ta),
  r_cpp=sim_ou_rcpp(eq_price=eq_price, vol_at=vol_at, the_ta=the_ta, in_nov=rnorm(len_gth)),
  times=10))[, c(1, 4, 5)]

# Source Rcpp function for Ornstein-Uhlenbeck process from file
Rcpp::sourceCpp(file="C:/Develop/R/lecture_slides/scripts/sim_ou.cpp")
# Simulate Ornstein-Uhlenbeck process in Rcpp
set.seed(1121)  # Reset random numbers
ou_sim_rcpp <- sim_ou_rcpp(eq_price=eq_price,
  vol_at=vol_at,
  the_ta=the_ta,
  in_nov=rnorm(len_gth))
all.equal(ou_sim, ou_sim_rcpp)
# Compare speed of Rcpp and R
library(microbenchmark)
summary(microbenchmark(
  pure_r=sim_ou(len_gth=len_gth, eq_price=eq_price, vol_at=vol_at, the_ta=the_ta),
  r_cpp=sim_ou_rcpp(eq_price=eq_price, vol_at=vol_at, the_ta=the_ta, in_nov=rnorm(len_gth)),
  times=10))[, c(1, 4, 5)]

# Calculate uniformly distributed pseudo-random sequence
uni_form <- function(see_d, len_gth=10) {
  out_put <- numeric(len_gth)
  out_put[1] <- see_d
  for (i in 2:len_gth) {
    out_put[i] <- 4*out_put[i-1]*(1-out_put[i-1])
  }  # end for
  acos(1-2*out_put)/pi
}  # end uni_form

# Source Rcpp functions from file
Rcpp::sourceCpp(file="C:/Develop/R/lecture_slides/scripts/uni_form.cpp")
# Microbenchmark Rcpp code
library(microbenchmark)
summary(microbenchmark(
  pure_r=runif(1e5),
  r_loop=uni_form(0.3, 1e5),
  r_cpp=uniform_rcpp(0.3, 1e5),
  times=10))[, c(1, 4, 5)]

library(RcppArmadillo)
# Source Rcpp functions from file
Rcpp::sourceCpp(file="C:/Develop/R/lecture_slides/scripts/armadillo_functions.cpp")
vec1 <- runif(1e5)
vec2 <- runif(1e5)
vec_in(vec1, vec2)
vec1 %*% vec2

# Microbenchmark RcppArmadillo code
summary(microbenchmark(
  vec_in=vec_in(vec1, vec2),
  r_code=(vec1 %*% vec2),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
# Microbenchmark shows:
# vec_in() is several times faster than %*%, especially for longer vectors.
#     expr     mean   median
# 1 vec_in 110.7067 110.4530
# 2 r_code 585.5127 591.3575

library(RcppArmadillo)
# Source Rcpp functions from file
Rcpp::sourceCpp(file="C:/Develop/R/lecture_slides/scripts/armadillo_functions.cpp")
mat_rix <- matrix(runif(1e5), nc=1e3)
# De-mean using apply()
new_mat <- apply(mat_rix, 2,
  function(x) (x-mean(x)))
# De-mean using demean_mat()
demean_mat(mat_rix)
all.equal(new_mat, mat_rix)
# Microbenchmark RcppArmadillo code
summary(microbenchmark(
  demean_mat=demean_mat(mat_rix),
  apply=(apply(mat_rix, 2, mean)),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
# Microbenchmark shows:
# Demean_mat() is over 70 times faster than apply()
#         expr       mean   median
# 1 demean_mat   127.7539  125.604
# 2      apply 10781.7534 9291.674

# Perform matrix inversion
# Create random positive semi-definite matrix
mat_rix <- matrix(runif(25), nc=5)
mat_rix <- t(mat_rix) %*% mat_rix
# Invert the matrix
matrix_inv <- solve(mat_rix)
inv_mat(mat_rix)
all.equal(inv_mat, mat_rix)
# Microbenchmark RcppArmadillo code
library(microbenchmark)
summary(microbenchmark(
  inv_mat=inv_mat(mat_rix),
  solve=solve(mat_rix),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
# Microbenchmark shows:
# inv_mat() is over 10 times faster than solve()
#      expr     mean median
# 1 inv_mat  3.42669  2.933
# 2 solve   32.00254 31.280

# Source Rcpp functions from file
Rcpp::sourceCpp(file="C:/Develop/R/lecture_slides/scripts/sim_arima.cpp")
# Define AR(2) coefficients
co_eff <- c(0.9, 0.09)
n_rows <- 1e4
set.seed(1121)
in_nov <- rnorm(n_rows)
# Simulate ARIMA using filter()
arima_filter <- filter(x=in_nov,
  filter=co_eff, method="recursive")
# Simulate ARIMA using sim_arima()
ari_ma <- sim_arima(in_nov, rev(co_eff))
all.equal(drop(ari_ma),
  as.numeric(arima_filter))
# Microbenchmark RcppArmadillo code
summary(microbenchmark(
  filter=filter(x=in_nov, filter=co_eff, method="recursive"),
  sim_arima=sim_arima(in_nov, rev(co_eff)),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary

library(parallel)  # Load package parallel
# Get short description
packageDescription("parallel")
# Load help page
help(package="parallel")
# list all objects in "parallel"
ls("package:parallel")

library(parallel)  # Load package parallel
# Calculate number of available cores
n_cores <- detectCores() - 1
# Define function that pauses execution
paws <- function(x, sleep_time) {
  Sys.sleep(sleep_time)
  x
}  # end paws
# Perform parallel loop under Mac-OSX or Linux
paw_s <- mclapply(1:10, paws, mc.cores=n_cores,
          sleep_time=0.01)
# Initialize compute cluster under Windows
clus_ter <- makeCluster(n_cores)
# Perform parallel loop under Windows
paw_s <- parLapply(clus_ter, 1:10, paws,
           sleep_time=0.01)
library(microbenchmark)  # Load package microbenchmark
# Compare speed of lapply versus parallel computing
summary(microbenchmark(
  l_apply=lapply(1:10, paws, sleep_time=0.01),
  parl_apply=
    parLapply(clus_ter, 1:10, paws, sleep_time=0.01),
  times=10)
)[, c(1, 4, 5)]
# Stop R processes over cluster under Windows
stopCluster(clus_ter)

library(parallel)  # Load package parallel
# Calculate number of available cores
n_cores <- detectCores() - 1
# Initialize compute cluster under Windows
clus_ter <- makeCluster(n_cores)
# Define function that pauses execution
paws <- function(x, sleep_time) {
  Sys.sleep(sleep_time)
  x
}  # end paws
# Compare speed of lapply with parallel computing
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

library(parallel)  # Load package parallel
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
 lwd=2, lty=1, col=c("blue", "green"))

library(parallel)  # Load package parallel
# Calculate number of available cores
n_cores <- detectCores() - 1
# Initialize compute cluster under Windows
clus_ter <- makeCluster(n_cores)
# Define large matrix
mat_rix <- matrix(rnorm(7*10^5), ncol=7)
# Define aggregation function over column of matrix
agg_regate <- function(col_umn) {
  out_put <- 0
  for (in_dex in 1:NROW(col_umn))
    out_put <- out_put + col_umn[in_dex]
  out_put
}  # end agg_regate
# Perform parallel aggregations over columns of matrix
agg_regations <-
  parCapply(clus_ter, mat_rix, agg_regate)
# Compare speed of apply with parallel computing
summary(microbenchmark(
  ap_ply=apply(mat_rix, MARGIN=2, agg_regate),
  parl_apply=
    parCapply(clus_ter, mat_rix, agg_regate),
  times=10)
)[, c(1, 4, 5)]
# Stop R processes over cluster under Windows
stopCluster(clus_ter)

library(parallel)  # Load package parallel
# Calculate number of available cores
n_cores <- detectCores() - 1
# Initialize compute cluster under Windows
clus_ter <- makeCluster(n_cores)
ba_se <- 2
# Fails because child processes don't know ba_se:
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
# Fails because child processes don't know zoo::index():
parSapply(clus_ter, c("VTI", "IEF", "DBC"),
    function(sym_bol)
      NROW(index(get(sym_bol, envir=rutils::etf_env))))
# zoo function referenced using "::" in child process:
parSapply(clus_ter, c("VTI", "IEF", "DBC"),
    function(sym_bol)
      NROW(zoo::index(get(sym_bol, envir=rutils::etf_env))))
# Package zoo loaded in child process:
parSapply(clus_ter, c("VTI", "IEF", "DBC"),
    function(sym_bol) {
      stopifnot("package:zoo" %in% search() || require("zoo", quietly=TRUE))
      NROW(index(get(sym_bol, envir=rutils::etf_env)))
    })  # end parSapply
# Stop R processes over cluster under Windows
stopCluster(clus_ter)

library(parallel)  # Load package parallel
# Calculate number of available cores
n_cores <- detectCores() - 1
# Initialize compute cluster under Windows
clus_ter <- makeCluster(n_cores)
# Set seed for cluster under Windows
# Doesn't work: set.seed(1121)
clusterSetRNGStream(clus_ter, 1121)
# Perform parallel loop under Windows
out_put <- parLapply(clus_ter, 1:70, rnorm, n=100)
sum(unlist(out_put))
# Stop R processes over cluster under Windows
stopCluster(clus_ter)
# Perform parallel loop under Mac-OSX or Linux
out_put <- mclapply(1:10, rnorm, mc.cores=n_cores, n=100)

# Load package HighFreq
library(HighFreq)
# Or load the high frequency data file directly:
symbol_s <- load("C:/Develop/R/HighFreq/data/hf_data.RData")
head(SPY_TAQ)
head(SPY)
tail(SPY)

# Load package HighFreq
library(HighFreq)
head(SPY)

# Install package HighFreq from github
devtools::install_github(repo="algoquant/HighFreq")
# Load package HighFreq
library(HighFreq)
# Get documentation for package HighFreq
# Get short description
packageDescription("HighFreq")
# Load help page
help(package="HighFreq")
# List all datasets in "HighFreq"
data(package="HighFreq")
# List all objects in "HighFreq"
ls("package:HighFreq")
# Remove HighFreq from search path
detach("package:HighFreq")

# Load package HighFreq
library(HighFreq)
# you can see SPY when listing objects in HighFreq
ls("package:HighFreq")
# you can see SPY when listing datasets in HighFreq
data(package="HighFreq")
# but the SPY dataset isn't listed in the workspace
ls()
# HighFreq datasets are lazy loaded and available when needed
head(SPY)
# Load all the datasets in package HighFreq
data(hf_data)
# HighFreq datasets are now loaded and in the workspace
head(SPY)

# Install package IBrokers
install.packages("IBrokers")
# Load package IBrokers
library(IBrokers)
# Get documentation for package IBrokers
# Get short description
packageDescription("IBrokers")
# Load help page
help(package="IBrokers")
# List all datasets in "IBrokers"
data(package="IBrokers")
# List all objects in "IBrokers"
ls("package:IBrokers")
# Remove IBrokers from search path
detach("package:IBrokers")
# Install package IBrokers2
devtools::install_github(repo="algoquant/IBrokers2")

# Connect to Interactive Brokers TWS
ib_connect <- IBrokers::twsConnect(port=7497)
# Or connect to IB Gateway
# Ib_connect <- ibgConnect(port=4002)
# Check connection
IBrokers::isConnected(ib_connect)
# Close the Interactive Brokers API connection
IBrokers::twsDisconnect(ib_connect)

# Connect to Interactive Brokers TWS
ib_connect <- IBrokers::twsConnect(port=7497)
# Or connect to IB Gateway
# Ib_connect <- ibgConnect(port=4002)
# Download account information from IB
ib_account <- IBrokers::reqAccountUpdates(conn=ib_connect, acctCode="DI1207807")
# Extract account balances
balance_s <- ib_account[[1]]
balance_s$AvailableFunds
# Extract contract names, net positions, and profits and losses
IBrokers::twsPortfolioValue(ib_account)
# Close the Interactive Brokers API connection
IBrokers::twsDisconnect(ib_connect)

# Define AAPL stock contract (object)
con_tract <- IBrokers::twsEquity("AAPL", primary="ISLAND")
# Define CHF currency contract
con_tract <- IBrokers::twsCurrency("CHF", currency="USD")
# Define S&P Emini future June 2019 contract
con_tract <- IBrokers::twsFuture(symbol="ES",
  exch="GLOBEX", expiry="201906")
# Define 10yr Treasury future June 2019 contract
con_tract <- IBrokers::twsFuture(symbol="ZN",
  exch="ECBOT", expiry="201906")
# Define euro currency future June 2019 contract
con_tract <- IBrokers::twsFuture(symbol="EUR",
  exch="GLOBEX", expiry="201906")
# Define Gold future June 2019 contract
con_tract <- IBrokers::twsFuture(symbol="GC",
  exch="NYMEX", expiry="201906")
# Define Oil future January 2019 contract
con_tract <- IBrokers::twsFuture(symbol="QM",
  exch="NYMEX", expiry="201901")
# Test if contract object is correct
IBrokers::is.twsContract(con_tract)
# Get list with instrument information
IBrokers::reqContractDetails(conn=ib_connect, Contract=con_tract)
# Install the package twsInstrument
install.packages("twsInstrument", repos="http://r-forge.r-project.org")
# Define euro future using getContract() and Conid
con_tract <- twsInstrument::getContract("317631411")
# Get list with instrument information
IBrokers::reqContractDetails(conn=ib_connect, Contract=con_tract)

# Define VIX monthly and weekly futures June 2019 contract
sym_bol <- "VIX"
con_tract <- IBrokers::twsFuture(symbol=sym_bol,
  exch="CFE", expiry="201906")
# Define VIX monthly futures June 2019 contract
con_tract <- IBrokers::twsFuture(symbol=sym_bol,
  local="VXV8", exch="CFE", expiry="201906")
# Define VIX weekly futures October 3rd 2018 contract
con_tract <- IBrokers::twsFuture(symbol=sym_bol,
  local="VX40V8", exch="CFE", expiry="201906")
# Get list with instrument information
IBrokers::reqContractDetails(conn=ib_connect,
  Contract=con_tract)

# Define S&P Emini futures June 2019 contract
sym_bol <- "ES"
con_tract <- IBrokers::twsFuture(symbol=sym_bol,
  exch="GLOBEX", expiry="201906")
# Open file for data download
dir_name <- "C:/Develop/data/ib_data"
dir.create(dir_name)
file_name <- file.path(dir_name, paste0(sym_bol, "_201906.csv"))
file_connect <- file(file_name, open="w")
# Connect to Interactive Brokers TWS
ib_connect <- IBrokers::twsConnect(port=7497)
# Write header to file
cat(paste(paste(sym_bol, c("Index", "Open", "High", "Low", "Close", "Volume", "WAP", "Count"), sep="."), collapse=","), "\n", file=file_connect)
# Download historical data to file
IBrokers::reqHistoricalData(conn=ib_connect,
  Contract=con_tract,
  barSize="1 day", duration="6 M",
  file=file_connect)
# Close data file
close(file_connect)
# Close the Interactive Brokers API connection
IBrokers::twsDisconnect(ib_connect)

# Define IB contract objects for stock symbols
sym_bols <- c("AAPL", "F", "MSFT")
con_tracts <- lapply(sym_bols, IBrokers::twsEquity, primary="ISLAND")
# Open file connections for data download
dir_name <- "C:/Develop/data/ib_data"
file_names <- file.path(dir_name, paste0(sym_bols, format(Sys.time(), format="_%m_%d_%Y_%H_%M"), ".csv"))
file_connects <- lapply(file_names, function(file_name) file(file_name, open="w"))
# Connect to Interactive Brokers TWS
ib_connect <- IBrokers::twsConnect(port=7497)
# Download historical 1-minute bar data to files
for (it in 1:NROW(sym_bols)) {
  sym_bol <- sym_bols[it]
  file_connect <- file_connects[[it]]
  con_tract <- con_tracts[[it]]
  cat("Downloading data for: ", sym_bol, "\n")
  # Write header to file
  cat(paste(paste(sym_bol, c("Index", "Open", "High", "Low", "Close", "Volume", "WAP", "XTRA", "Count"), sep="."), collapse=","), "\n", file=file_connect)
  IBrokers::reqHistoricalData(conn=ib_connect,
                         Contract=con_tract,
                         barSize="1 min", duration="2 D",
                         file=file_connect)
  Sys.sleep(10) # 10s pause to avoid IB pacing violation
}  # end for
# Close data files
for (file_connect in file_connects) close(file_connect)
# Close the Interactive Brokers API connection
IBrokers::twsDisconnect(ib_connect)

# Define S&P Emini futures June 2018 contract
sym_bol <- "ES"
con_tract <- IBrokers::twsFuture(symbol=sym_bol,
  include_expired="1",
  exch="GLOBEX", expiry="201806")
# Open file connection for ESM8 data download
file_name <- file.path(dir_name, paste0(sym_bol, "M8.csv"))
file_connect <- file(file_name, open="w")
# Connect to Interactive Brokers TWS
ib_connect <- IBrokers::twsConnect(port=7497)
# Download historical data to file
IBrokers::reqHistoricalData(conn=ib_connect,
  Contract=con_tract,
  barSize="1 day", duration="2 Y",
  file=file_connect)
# Close data file
close(file_connect)
# Close the Interactive Brokers API connection
IBrokers::twsDisconnect(ib_connect)

# Load OHLC data and coerce it into xts series
price_s <- data.table::fread(file_name)
data.table::setDF(price_s)
price_s <- xts::xts(price_s[, 2:6],
  order.by=as.Date(as.POSIXct.numeric(price_s[, 1],
    tz="America/New_York", origin="1970-01-01")))
colnames(price_s) <- c("Open", "High", "Low", "Close", "Volume")
# Plot OHLC data in x11 window
chart_Series(x=price_s, TA="add_Vo()",
  name="S&P500 ESM8 futures")
# Plot dygraph
dygraphs::dygraph(price_s[, 1:4], main="S&P500 ESM8 futures") %>%
  dyCandlestick()

# Define S&P Emini futures June 2019 contract
sym_bol <- "ES"
con_tract <- IBrokers::twsFuture(symbol=sym_bol,
  exch="GLOBEX", expiry="201906")
# Open file connection for data download
dir_name <- "C:/Develop/data/ib_data"
# Dir.create(dir_name)
file_name <- file.path(dir_name, paste0(sym_bol, "_taq_live.csv"))
file_connect <- file(file_name, open="w")
# Connect to Interactive Brokers TWS
ib_connect <- IBrokers::twsConnect(port=7497)
# Download live data to file
IBrokers::reqMktData(conn=ib_connect,
     Contract=con_tract,
     eventWrapper=eWrapper.MktData.CSV(1),
     file=file_connect)
# Close data file
close(file_connect)
# Close the Interactive Brokers API connection
IBrokers::twsDisconnect(ib_connect)

# Define S&P Emini futures June 2019 contract
sym_bol <- "ES"
con_tract <- IBrokers::twsFuture(symbol=sym_bol,
  exch="GLOBEX", expiry="201906")
# Open file connection for data download
dir_name <- "C:/Develop/data/ib_data"
# Dir.create(dir_name)
file_name <- file.path(dir_name, paste0(sym_bol, "_ohlc_live.csv"))
file_connect <- file(file_name, open="w")
# Connect to Interactive Brokers TWS
ib_connect <- IBrokers::twsConnect(port=7497)
# Download live data to file
IBrokers::reqRealTimeBars(conn=ib_connect,
     Contract=con_tract, barSize="1",
     eventWrapper=eWrapper.RealTimeBars.CSV(1),
     file=file_connect)
# Close the Interactive Brokers API connection
IBrokers::twsDisconnect(ib_connect)
# Close data file
close(file_connect)
# Load OHLC data and coerce it into xts series
library(data.table)
price_s <- data.table::fread(file_name)
price_s <- xts::xts(price_s[, paste0("V", 2:6)],
  as.POSIXct.numeric(as.numeric(price_s[, V1]), tz="America/New_York", origin="1970-01-01"))
colnames(price_s) <- c("Open", "High", "Low", "Close", "Volume")
# Plot OHLC data in x11 window
x11()
chart_Series(x=price_s, TA="add_Vo()",
       name="S&P500 ESM9 futures")
# Plot dygraph
library(dygraphs)
dygraphs::dygraph(price_s[, 1:4], main="S&P500 ESM9 futures") %>%
  dyCandlestick()

library(IBrokers)
# Define list of S&P futures and 10yr Treasury contracts
con_tracts <- list(ES=IBrokers::twsFuture(symbol="ES", exch="GLOBEX", expiry="201906"),
             ZN=IBrokers::twsFuture(symbol="ZN", exch="ECBOT", expiry="201906"))
# Open the file connection for storing the bar data
dir_name <- "C:/Develop/data/ib_data"
file_names <- file.path(dir_name, paste0(c("ES_", "ZN_"), format(Sys.time(), format="%m_%d_%Y_%H_%M"), ".csv"))
file_connects <- lapply(file_names, function(file_name) file(file_name, open="w"))
# Connect to Interactive Brokers TWS
ib_connect <- IBrokers::twsConnect(port=7497)
# Download live data to file
IBrokers::reqRealTimeBars(conn=ib_connect,
                    Contract=con_tracts,
                    barSize="1", useRTH=FALSE,
                    eventWrapper=eWrapper.RealTimeBars.CSV(NROW(con_tracts)),
                    file=file_connects)
# Close the Interactive Brokers API connection
IBrokers::twsDisconnect(ib_connect)
# Close data files
for (file_connect in file_connects)
  close(file_connect)
library(data.table)
# Load ES futures June 2019 contract and coerce it into xts series
price_s <- data.table::fread(file_names[1])
price_s <- xts::xts(price_s[, paste0("V", 2:6)],
  as.POSIXct.numeric(as.numeric(price_s[, V1]), tz="America/New_York", origin="1970-01-01"))
colnames(price_s) <- c("Open", "High", "Low", "Close", "Volume")
# Plot dygraph
library(dygraphs)
dygraphs::dygraph(price_s[, 1:4], main="S&P500 ESM9 futures") %>%
  dyCandlestick()
# Load ZN 10yr Treasury futures June 2019 contract
price_s <- data.table::fread(file_names[2])
price_s <- xts::xts(price_s[, paste0("V", 2:6)],
  as.POSIXct.numeric(as.numeric(price_s[, V1]), tz="America/New_York", origin="1970-01-01"))
colnames(price_s) <- c("Open", "High", "Low", "Close", "Volume")
# Plot dygraph
dygraphs::dygraph(price_s[, 1:4], main="ZN 10yr Treasury futures") %>%
  dyCandlestick()

# Define S&P Emini future June 2019 contract
con_tract <- IBrokers::twsFuture(symbol="ES", exch="GLOBEX", expiry="201906")
# Define euro currency contract EUR.USD
con_tract <- IBrokers::twsCurrency("EUR", currency="USD")
# Define euro currency E-mini futures June 2019 contract E7Z8
con_tract <- IBrokers::twsFuture(symbol="E7", exch="GLOBEX", expiry="201906")
# Define Japanese yen currency contract JPY.USD
con_tract <- IBrokers::twsCurrency("JPY", currency="USD")
# Define Japanese yen currency E-mini futures June 2019 contract J7Z8
con_tract <- IBrokers::twsFuture(symbol="J7", exch="GLOBEX", expiry="201906")
# Define Japanese yen currency futures June 2019 contract 6JZ8
con_tract <- IBrokers::twsFuture(symbol="JPY", exch="GLOBEX", expiry="201906")
# Connect to Interactive Brokers TWS
ib_connect <- IBrokers::twsConnect(port=7497)
IBrokers::reqContractDetails(conn=ib_connect, Contract=con_tract)
# Request trade order ID
order_id <- IBrokers::reqIds(ib_connect)
# Create buy market order object
ib_order <- IBrokers::twsOrder(order_id, orderType="MKT",
  action="BUY", totalQuantity=1)
# Place trade order
IBrokers::placeOrder(ib_connect, con_tract, ib_order)
# Execute sell market order
order_id <- IBrokers::reqIds(ib_connect)
ib_order <- IBrokers::twsOrder(order_id, orderType="MKT",
  action="SELL", totalQuantity=1)
IBrokers::placeOrder(ib_connect, con_tract, ib_order)
# Execute buy market order
order_id <- IBrokers::reqIds(ib_connect)
ib_order <- IBrokers::twsOrder(order_id, orderType="MKT",
  action="BUY", totalQuantity=1)
IBrokers::placeOrder(ib_connect, con_tract, ib_order)

# Request trade order ID
order_id <- IBrokers::reqIds(ib_connect)
# Create buy limit order object
ib_order <- IBrokers::twsOrder(order_id, orderType="LMT",
  lmtPrice="1.1511", action="BUY", totalQuantity=1)
# Place trade order
IBrokers::placeOrder(ib_connect, con_tract, ib_order)
# Cancel trade order
IBrokers::cancelOrder(ib_connect, order_id)
# Execute sell limit order
order_id <- IBrokers::reqIds(ib_connect)
ib_order <- IBrokers::twsOrder(order_id, orderType="LMT",
  lmtPrice="1.1512", action="SELL", totalQuantity=1)
IBrokers::placeOrder(ib_connect, con_tract, ib_order)
# Cancel trade order
IBrokers::cancelOrder(ib_connect, order_id)
# Close the Interactive Brokers API connection
IBrokers::twsDisconnect(ib_connect)

eWrapper_realtimebars <- function(n = 1) {
  eW <- eWrapper_new(NULL)
  # eW <- IBrokers::eWrapper(NULL)
  eW$assign.Data("data", rep(list(structure(.xts(matrix(rep(NA_real_, 7), ncol = 7), 0), .Dimnames = list(NULL, c("Open", "High", "Low", "Close", "Volume", "WAP", "Count")))), n))
  eW$realtimeBars <- function(curMsg, msg, timestamp, file, ...) {
    id <- as.numeric(msg[2])
    file <- file[[id]]
    data <- eW$get.Data("data")
    attr(data[[id]], "index") <- as.numeric(msg[3])
    nr.data <- NROW(data[[id]])
    # write to file
    cat(paste(msg[3], msg[4], msg[5], msg[6], msg[7], msg[8], msg[9], msg[10], sep = ","), "\n", file = file, append = TRUE)
    # write to console
    # eW$count_er <- eW$count_er + 1
    eW$assign.Data("count_er", eW$get.Data("count_er")+1)
    cat(paste0("count_er=", eW$get.Data("count_er"), "\tOpen=", msg[4], "\tHigh=", msg[5], "\tLow=", msg[6], "\tClose=", msg[7], "\tVolume=", msg[8]), "\n")
    # cat(paste0("Open=", msg[4], "\tHigh=", msg[5], "\tLow=", msg[6], "\tClose=", msg[7], "\tVolume=", msg[8]), "\n")
    #Trade
    # Cancel previous trade orders
    buy_id <- eW$get.Data("buy_id")
    sell_id <- eW$get.Data("sell_id")
    if (buy_id>0) IBrokers::cancelOrder(ib_connect, buy_id)
    if (sell_id>0) IBrokers::cancelOrder(ib_connect, sell_id)
    # Execute buy limit order
    buy_id <- IBrokers::reqIds(ib_connect)
    buy_order <- IBrokers::twsOrder(buy_id, orderType="LMT",
                              lmtPrice=msg[6]-0.25, action="BUY", totalQuantity=1)
    IBrokers::placeOrder(ib_connect, con_tract, buy_order)
    # Execute sell limit order
    sell_id <- IBrokers::reqIds(ib_connect)
    sell_order <- IBrokers::twsOrder(sell_id, orderType="LMT",
                               lmtPrice=msg[5]+0.25, action="SELL", totalQuantity=1)
    IBrokers::placeOrder(ib_connect, con_tract, sell_order)
    # Copy new trade orders
    eW$assign.Data("buy_id", buy_id)
    eW$assign.Data("sell_id", sell_id)
    #Trade finished
    data[[id]][nr.data, 1:7] <- as.numeric(msg[4:10])
    eW$assign.Data("data", data)
    c(curMsg, msg)
  }  # end eW$realtimeBars
  return(eW)
}  # end eWrapper_realtimebars

cat("Enter\ttab")  # Cat() interprets backslash escape sequences
print("Enter\ttab")

my_text <- print("hello")
my_text  # print() returns its argument

# Create string
my_text <- "Title: My Text\nSome numbers: 1,2,3,...\nRprofile files contain code executed at R startup,\n"

cat(my_text, file="mytext.txt")  # Write to text file

cat("Title: My Text",  # Write several lines to text file
    "Some numbers: 1,2,3,...",
    "Rprofile files contain code executed at R startup,",
    file="mytext.txt", sep="\n")

save(my_text, file="mytext.RData")  # Write to binary file

print(pi)
print(pi, digits=10)
getOption("digits")
foo <- 12
bar <- "months"
sprintf("There are %i %s in the year", foo, bar)

# Read text from file
scan(file="mytext.txt", what=character(), sep="\n")

# Read lines from file
readLines(con="mytext.txt")

# Read text from console
in_put <- readline("Enter a number: ")
class(in_put)
# Coerce to numeric
in_put <- as.numeric(in_put)

# Read text from file and display in editor:
# file.show("mytext.txt")
# file.show("mytext.txt", pager="")

setwd("C:/Develop/R/lecture_slides/data")
data_frame <- data.frame(type=c("rose", "daisy", "tulip"), color=c("red", "white", "yellow"), price=c(1.5, 0.5, 1.0), row.names=c("flower1", "flower2", "flower3"))  # end data.frame
mat_rix <- matrix(sample(1:12), ncol=3, dimnames=list(NULL, c("col1", "col2", "col3")))
rownames(mat_rix) <- paste("row", 1:NROW(mat_rix), sep="")
# Write data frame to text file, and then read it back
write.table(data_frame, file="florist.txt")
data_read <- read.table(file="florist.txt")
data_read  # A data frame

# Write matrix to text file, and then read it back
write.table(mat_rix, file="matrix.txt")
mat_read <- read.table(file="matrix.txt")
mat_read  # write.table() coerced matrix to data frame
class(mat_read)
# Coerce from data frame back to matrix
mat_read <- as.matrix(mat_read)
class(mat_read)

setwd("C:/Develop/R/lecture_slides/data")
data_frame <- data.frame(small=c(3, 5), medium=c(9, 11), large=c(15, 13))
data_frame <- read.table("mydata.txt", header=TRUE)
data_frame <- read.table("clipboard", header=TRUE)

write.table(x=data_frame, file="clipboard", sep="\t")

# Wrapper function for copying data frame from clipboard into R
# by default, data is tab delimited, with a header
read_clip <- function(file="clipboard", sep="\t",
              header=TRUE, ...) {
  read.table(file=file, sep=sep, header=header, ...)
}  # end read_clip

data_frame <- read_clip()

# Wrapper function for copying data frame from R into clipboard
# by default, data is tab delimited, with a header
write_clip <- function(data, row.names=FALSE,
               col.names=TRUE, ...) {
  write.table(x=data, file="clipboard", sep="\t",
      row.names=row.names, col.names=col.names, ...)
}  # end write_clip

write_clip(data=data_frame)

# Launch spreadsheet-style data editor
data_frame <- edit(data_frame)

# Write data frame to CSV file, and then read it back
write.csv(data_frame, file="florist.csv")
data_read <- read.csv(file="florist.csv",
                 stringsAsFactors=FALSE)
data_read  # the row names are read in as extra column
# Restore row names
rownames(data_read) <- data_read[, 1]
data_read <- data_read[, -1]  # Remove extra column
data_read
# Read data frame, with row names from first column
data_read <- read.csv(file="florist.csv", row.names=1)
data_read

# Write data frame to CSV file, without row names
write.csv(data_frame, row.names=FALSE, file="florist.csv")
data_read <- read.csv(file="florist.csv")
data_read  # A data frame without row names

# Write matrix to csv file, and then read it back
write.csv(mat_rix, file="matrix.csv")
mat_read <- read.csv(file="matrix.csv", row.names=1)
mat_read  # Read.csv() reads matrix as data frame
class(mat_read)
mat_read <- as.matrix(mat_read)  # Coerce to matrix
identical(mat_rix, mat_read)
write.csv(mat_rix, row.names=FALSE,
    file="matrix_ex_rows.csv")
mat_read <- read.csv(file="matrix_ex_rows.csv")
mat_read <- as.matrix(mat_read)
mat_read  # A matrix without row names

setwd("C:/Develop/R/lecture_slides/data")
library(MASS)  # Load package "MASS"
# Write to CSV file by row - it's very SLOW!!!
MASS::write.matrix(mat_rix,
  file="matrix.csv", sep=",")
# Read using scan() and skip first line with colnames
mat_read <- scan(file="matrix.csv",
  sep=",", skip=1, what=numeric())
# Read colnames
col_names <- readLines(con="matrix.csv", n=1)
col_names  # this is a string!
# Convert to char vector
col_names <- strsplit(col_names,
  s=",")[[1]]
mat_read  # mat_read is a vector, not matrix!
# Coerce by row to matrix
mat_read <- matrix(mat_read,
  ncol=NROW(col_names), byrow=TRUE)
# Restore colnames
colnames(mat_read) <- col_names
mat_read
# Scan() is a little faster than read.csv()
library(microbenchmark)
summary(microbenchmark(
  read_csv=read.csv("matrix.csv"),
  scan=scan(file="matrix.csv", sep=",",
    skip=1, what=numeric()),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# Read data from a csv file, including row names
mat_rix <- read.csv(file="matrix_bad.csv",
  row.names=1, stringsAsFactors=FALSE)
mat_rix
class(mat_rix)
# Columns with bad data are character or factor
sapply(mat_rix, class)
# Copy row names
row_names <- row.names(mat_rix)
# sapply loop over columns and coerce to numeric
mat_rix <- sapply(mat_rix, as.numeric)
# Restore row names
row.names(mat_rix) <- row_names
# Replace NAs with zero
mat_rix[is.na(mat_rix)] <- 0
# matrix without NAs
mat_rix

setwd("C:/Develop/R/lecture_slides/data")
rm(list=ls())
set.seed(1121)  # Reset random number generator
library(zoo)  # Load package zoo
# Create zoo with Date index
in_dex <- seq(from=as.Date("2013-06-15"),
        by="day", length.out=100)
zoo_series <- zoo(rnorm(NROW(in_dex)), order.by=in_dex)
head(zoo_series, 3)
# Write zoo series to text file, and then read it back
write.zoo(zoo_series, file="zoo_series.txt")
zoo_read <- read.zoo("zoo_series.txt")  # Read it back
all.equal(zoo_read, zoo_series)
# Perform the same using write.table() and read.table()
# first coerce zoo_series into data frame
data_frame <- as.data.frame(zoo_series)
data_frame <- cbind(in_dex, data_frame)
# Write zoo_series to text file using write.table
write.table(data_frame, file="zoo_series.txt",
      row.names=FALSE, col.names=FALSE)
# Read data frame from file
zoo_read <- read.table(file="zoo_series.txt",
                 stringsAsFactors=FALSE)
sapply(zoo_read, class)  # A data frame
# Coerce data frame into zoo_series
zoo_read <- zoo::zoo(
  drop(as.matrix(zoo_read[, -1])),
  order.by=as.Date(zoo_read[, 1]))
all.equal(zoo_read, zoo_series)

library(zoo)  # Load package zoo
# Write zoo series to CSV file, and then read it back
write.zoo(zoo_series, file="zoo_series.csv",
    sep=",", col.names=TRUE)
zoo_read <- read.zoo(file="zoo_series.csv",
  header=TRUE, sep=",", drop=FALSE)
all.equal(zoo_series, drop(zoo_read))

set.seed(1121)  # Reset random number generator
# Create zoo with POSIXct date-time index
in_dex <- seq(from=as.POSIXct("2013-06-15"),
        by="hour", length.out=100)
zoo_series <- zoo(rnorm(NROW(in_dex)), order.by=in_dex)
head(zoo_series, 3)
# Write zoo series to CSV file, and then read it back
write.zoo(zoo_series, file="zoo_series.csv",
    sep=",", col.names=TRUE)
# Read from CSV file using read.csv.zoo()
zoo_read <- read.csv.zoo(file="zoo_series.csv")
all.equal(zoo_series, zoo_read)
# Coerce to xts series
x_ts <- xts::as.xts(zoo_read)
class(x_ts); head(x_ts, 3)
# Coerce zoo series into data frame with custom date format
data_frame <- as.data.frame(zoo_series)
data_frame <- cbind(format(in_dex, "%m-%d-%Y %H:%M:%S"), data_frame)
head(data_frame, 3)
# Write zoo series to csv file using write.table
write.table(data_frame, file="zoo_series.csv",
      sep=",", row.names=FALSE, col.names=FALSE)
# Read from CSV file using read.csv.zoo()
zoo_read <- read.zoo(file="zoo_series.csv",
  header=FALSE, sep=",", FUN=as.POSIXct,
  format="%m-%d-%Y %H:%M:%S", tz="America/New_York")
# Or using read.csv.zoo()
zoo_read <- read.csv.zoo(file="zoo_series.csv",
  header=FALSE,  format="%m-%d-%Y %H:%M:%S", tz="America/New_York")
head(zoo_read, 3)
all.equal(zoo_series, zoo_read)

# Read time series from CSV file, with numeric date-time
zoo_read <- read.table(file="C:/Develop/R/lecture_slides/data/es_ohlc.csv",
  header=TRUE, sep=",")
# A data frame
class(zoo_read)
sapply(zoo_read, class)
# Coerce data frame into xts series
zoo_read <- xts::xts(as.matrix(zoo_read[, -1]),
  order.by=as.POSIXct.numeric(zoo_read[, 1], tz="America/New_York", origin="1970-01-01"))
# An xts series
class(zoo_read)
head(zoo_read, 3)

rm(list=ls())  # Remove all objects
var1 <- 1; var2 <- 2
ls()  # List all objects
ls()[1]  # List first object
args(save)  # List arguments of save function
# Save "var1" to a binary file using string argument
save("var1", file="my_data.RData")
# Save "var1" to a binary file using object name
save(var1, file="my_data.RData")
# Save multiple objects
save(var1, var2, file="my_data.RData")
# Save first object in list by passing to "..." argument
# Ls()[1] is not evaluated
save(ls()[1], file="my_data.RData")
# Save first object in list by passing to "list" argument
save(list=ls()[1], file="my_data.RData")
# Save whole list by passing it to the "list" argument
save(list=ls(), file="my_data.RData")

rm(list=ls())  # Remove all objects
# Load objects from file
load_ed <- load(file="my_data.RData")
load_ed  # vector of loaded objects
ls()  # List objects
# Assign new values to objects in  global environment
sapply(load_ed, function(sym_bol) {
  assign(sym_bol, runif(1), envir=globalenv())
})  # end sapply
ls()  # List objects
# Assign new values to objects using for loop
for (sym_bol in load_ed) {
  assign(sym_bol, runif(1))
}  # end for
ls()  # List objects
# Save vector of objects
save(list=load_ed, file="my_data.RData")
# Remove only loaded objects
rm(list=load_ed)
# Remove the object "load_ed"
rm(load_ed)

sink("sinkdata.txt")# Redirect text output to file

cat("Redirect text output from R\n")
print(runif(10))
cat("\nEnd data\nbye\n")

sink()  # turn redirect off

pdf("Rgraph.pdf", width=7, height=4)  # Redirect graphics to pdf file

cat("Redirect data from R into pdf file\n")
my_var <- seq(-2*pi, 2*pi, len=100)
plot(x=my_var, y=sin(my_var), main="Sine wave",
   xlab="", ylab="", type="l", lwd=2, col="red")
cat("\nEnd data\nbye\n")

dev.off()  # turn pdf output off

png("r_plot.png")  # Redirect graphics output to png file

cat("Redirect graphics from R into png file\n")
plot(x=my_var, y=sin(my_var), main="Sine wave",
 xlab="", ylab="", type="l", lwd=2, col="red")
cat("\nEnd data\nbye\n")

dev.off()  # turn png output off

# Install package data.table
install.packages("data.table")
# Load package data.table
library(data.table)
# get documentation for package data.table
# get short description
packageDescription("data.table")
# Load help page
help(package="data.table")
# List all datasets in "data.table"
data(package="data.table")
# List all objects in "data.table"
ls("package:data.table")
# Remove data.table from search path
detach("package:data.table")

# Create a data table
library(data.table)
data_table <- data.table::data.table(
  col1=sample(7), col2=sample(7), col3=sample(7))
# Print data_table
class(data_table); data_table
# column referenced without quotes
data_table[, col2]
# row referenced without a following comma
data_table[2]
# Print option "datatable.print.nrows"
getOption("datatable.print.nrows")
options(datatable.print.nrows=10)
getOption("datatable.print.nrows")
# Number of rows in data_table
NROW(data_table)
# Or
data_table[, NROW(col1)]
# Or
data_table[, .N]
# microbenchmark speed of data.table syntax
library(microbenchmark)
summary(microbenchmark(
  dt=data_table[, .N],
  pure_r=NROW(data_table),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# Create data frame and coerce it to data table
data_table <- data.frame(
  col1=sample(7), col2=sample(7), col3=sample(7))
class(data_table); data_table
data.table::setDT(data_table)
class(data_table); data_table
# Coerce data_table into data frame
data.table::setDF(data_table)
class(data_table); data_table
# Or
data_table <- data.table:::as.data.frame.data.table(data_table)
# SetDF() is much faster than as.data.frame()
summary(microbenchmark(
  as.data.frame=data.table:::as.data.frame.data.table(data_table),
  setDF=data.table::setDF(data_table),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# Coerce xts to a data frame
price_s <- rutils::etf_env$VTI
class(price_s); head(price_s)
price_s <- as.data.frame(price_s)
class(price_s); head(price_s)
# Coerce data frame to a data table
data.table::setDT(price_s, keep.rownames=TRUE)
class(price_s); head(price_s)
# Dates are coerced to strings
sapply(price_s, class)
# Coerce xts directly to a data table
data_table <- as.data.table(rutils::etf_env$VTI,
  keep.rownames=TRUE)
class(data_table); head(data_table)
# Dates are not coerced to strings
sapply(data_table, class)
all.equal(price_s, data_table, check.attributes=FALSE)

# Read a data table from CSV file
dir_name <- "C:/Develop/R/lecture_slides/data/"
file_name <- file.path(dir_name, "weather_delays14.csv")
data_table <- data.table::fread(file_name)
class(data_table); dim(data_table)
data_table
# fread() reads the same data as read.csv()
all.equal(read.csv(file_name, stringsAsFactors=FALSE),
    setDF(fread(file_name)))
# fread() is much faster than read.csv()
summary(microbenchmark(
  pure_r=read.csv(file_name),
  data.table=setDF(fread(file_name)),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Write data table to file in different ways
data.table::fwrite(data_table, file="data_table.csv")
write.csv(data_table, file="data_table2.csv")
cat(unlist(data_table), file="data_table3.csv")
# microbenchmark speed of data.table::fwrite()
summary(microbenchmark(
  fwrite=data.table::fwrite(data_table, file="data_table.csv"),
  write_csv=write.csv(data_table, file="data_table2.csv"),
  cat=cat(unlist(data_table), file="data_table3.csv"),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# Select first five rows of data_table
data_table[1:5]
# Select rows with JFK flights
jfk_flights <- data_table[origin=="JFK"]
# Select rows JFK flights in June
jfk_flights <- data_table[origin=="JFK" & month==6]
# Select rows without JFK flights
jfk_flights <- data_table[!(origin=="JFK")]
# Select flights with carrier_delay
data_table[carrier_delay > 0]
# Select column of data_table and return a vector
head(data_table[, origin])
# Select column of data_table and return a data_table, not vector
head(data_table[, list(origin)])
# Or
head(data_table[, .(origin)])
# Select two columns of data_table
data_table[, list(origin, month)]
# Or
data_table[, .(origin, month)]
# Select two columns and rename them
data_table[, list(or=origin, mo=month)]

# Select flights with positive carrier_delay
data_table[carrier_delay > 0]
# Number of flights with carrier_delay
data_table[, sum(carrier_delay > 0)]
# Or
NROW(data_table[carrier_delay > 0])
# microbenchmark speed of data.table syntax
summary(microbenchmark(
  dt=data_table[, sum(carrier_delay > 0)],
  pure_r=NROW(data_table[carrier_delay > 0]),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Average carrier_delay
data_table[, mean(carrier_delay)]
# Average carrier_delay and aircraft_delay
data_table[, .(carrier=mean(carrier_delay),
         aircraft=mean(aircraft_delay))]
# Average aircraft_delay from JFK
data_table[origin=="JFK", mean(aircraft_delay)]
# Number of flights from JFK
data_table[origin=="JFK", NROW(aircraft_delay)]
# Or
data_table[origin=="JFK", .N]

# Number of flights from each airport
data_table[, .N, by=origin]
# Or with names for output
data_table[, .(flights=.N), by=.(airport=origin)]
# Number of AA flights from each airport
data_table[carrier=="AA", .(flights=.N),
     by=.(airport=origin)]
# Number of flights from each airport and airline
data_table[, .(flights=.N),
     by=.(airport=origin, airline=carrier)]
# Average aircraft_delay
data_table[, mean(aircraft_delay)]
# Average aircraft_delay from JFK
data_table[origin=="JFK", mean(aircraft_delay)]
# Average aircraft_delay from each airport
data_table[, .(delay=mean(aircraft_delay)),
     by=.(airport=origin)]
# Average and max delays from each airport and month
data_table[, .(mean_delay=mean(aircraft_delay), max_delay=max(aircraft_delay)),
     by=.(airport=origin, month=month)]
# Average and max delays from each airport and month
data_table[, .(mean_delay=mean(aircraft_delay), max_delay=max(aircraft_delay)),
     keyby=.(airport=origin, month=month)]

# Sort data_table by origin in ascending order, then by dest in descending order
order_table <- data_table[order(origin, -dest)]
order_table
# Doesn't work outside data_table
order(origin, -dest)
# Average aircraft_delay by month
order_table[, .(mean_delay=mean(aircraft_delay)),
      by=.(month=month)]
# Sort output by month
order_table[, .(mean_delay=mean(aircraft_delay)),
      by=.(month=month)][order(month)]

# Return all columns except origin and dest
data_table[1:7]
data_table[1:7, .SD, by=.(origin, dest)]
# Return first two rows from each month
data_table[, head(.SD, 2), by=.(month)]
# Return all columns except origin and dest
data_table[1:7, .SD,
     by=.(origin, dest), .SDcols="aircraft_delay"]
# Return the columns carrier_delay, weather_delay, aircraft_delay
data_table[1:7, .SD,
     by=.(origin, dest),
     .SDcols=c("carrier_delay", "weather_delay", "aircraft_delay")]
# Calculate mean of weather_delay and aircraft_delay by origin
data_table[,
     lapply(.SD, mean),
     by=.(origin),
     .SDcols=c("weather_delay", "aircraft_delay")]
# Or simply
data_table[,
     .(weather_delay=mean(weather_delay), aircraft_delay=mean(aircraft_delay)),
     by=.(origin)]

# Install package fst
install.packages("fst")
# Load package fst
library(fst)
# get documentation for package fst
# get short description
packageDescription("fst")
# Load help page
help(package="fst")
# List all datasets in "fst"
data(package="fst")
# List all objects in "fst"
ls("package:fst")
# Remove fst from search path
detach("package:fst")

# Read a data frame from CSV file
dir_name <- "C:/Develop/R/lecture_slides/data/"
file_name <- file.path(dir_name, "weather_delays14.csv")
data.table::setDF(data_frame)
class(data_frame); dim(data_frame)
# Write data frame to .fst file in different ways
fst::write_fst(data_frame, path="data_frame.fst")
write.csv(data_frame, file="data_frame2.csv")
# microbenchmark speed of fst::write_fst()
library(microbenchmark)
summary(microbenchmark(
  fst=fst::write_fst(data_frame, path="data_frame.csv"),
  write_csv=write.csv(data_frame, file="data_frame2.csv"),
  cat=cat(unlist(data_frame), file="data_frame3.csv"),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# fst::read_fst() reads the same data as read.csv()
all.equal(read.csv(file_name, stringsAsFactors=FALSE),
    fst::read_fst("data_frame.fst"))
# fst::read_fst() is 10 times faster than read.csv()
summary(microbenchmark(
  fst=fst::read_fst("data_frame.fst"),
  read_csv=read.csv(file_name),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Create reference to data file similar to a data frame
fs_t <- fst::fst("data_frame.fst")
class(fs_t)
dim(data_frame); dim(fs_t)
# Get classes of data frame columns
sapply(data_frame, class)
sapply(fs_t, class)
# Get sizes of all objects in workspace
sort(sapply(mget(ls()), object.size))

# load package quantmod
library(quantmod)
# get documentation for package quantmod
# get short description
packageDescription("quantmod")
# load help page
help(package="quantmod")
# list all datasets in "quantmod"
data(package="quantmod")
# list all objects in "quantmod"
ls("package:quantmod")
# Remove quantmod from search path
detach("package:quantmod")

library(quantmod)
# Plot OHLC candlechart with volume
chartSeries(etf_env$VTI["2014-11"],
      name="VTI",
      theme=chartTheme("white"))
# Plot OHLC bar chart with volume
chartSeries(etf_env$VTI["2014-11"],
      type="bars",
      name="VTI",
      theme=chartTheme("white"))

library(quantmod)
# Plot OHLC candlechart with volume
chartSeries(etf_env$VTI["2008-11/2009-04"],
      name="VTI")
# Redraw plot only for Feb-2009, with white theme
reChart(subset="2009-02",
  theme=chartTheme("white"))

library(quantmod)
# Candlechart with Bollinger Bands
chartSeries(etf_env$VTI["2014"],
      TA="addBBands(): addBBands(draw='percent'): addVo()",
      name="VTI with Bollinger Bands",
      theme=chartTheme("white"))
# Candlechart with two Moving Averages
chartSeries(etf_env$VTI["2014"],
      TA="addVo(): addEMA(10): addEMA(30)",
      name="VTI with Moving Averages",
      theme=chartTheme("white"))
# Candlechart with Commodity Channel Index
chartSeries(etf_env$VTI["2014"],
      TA="addVo(): addBBands(): addCCI()",
      name="VTI with Technical Indicators",
      theme=chartTheme("white"))

library(quantmod)
library(TTR)
oh_lc <- rutils::etf_env$VTI["2009-02/2009-03"]
VTI_adj <- Ad(oh_lc); VTI_vol <- Vo(oh_lc)
# Calculate volume-weighted average price
VTI_vwap <- TTR::VWAP(price=VTI_adj,
volume=VTI_vol, n=10)
# Plot OHLC candlechart with volume
chartSeries(oh_lc, name="VTI plus VWAP",
      theme=chartTheme("white"))
# Add VWAP to main plot
addTA(ta=VTI_vwap, on=1, col='red')
# Add price minus VWAP in extra panel
addTA(ta=(VTI_adj-VTI_vwap), col='red')

library(quantmod)
library(TTR)
oh_lc <- rutils::etf_env$VTI
VTI_adj <- Ad(oh_lc)
VTI_vol <- Vo(oh_lc)
VTI_vwap <- TTR::VWAP(price=VTI_adj, volume=VTI_vol, n=10)
VTI_adj <- VTI_adj["2009-02/2009-03"]
oh_lc <- oh_lc["2009-02/2009-03"]
VTI_vwap <- VTI_vwap["2009-02/2009-03"]
# Plot OHLC candlechart with volume
chartSeries(oh_lc, name="VTI plus VWAP shaded",
      theme=chartTheme("white"))
# Add VWAP to main plot
addTA(ta=VTI_vwap, on=1, col='red')
# Add price minus VWAP in extra panel
addTA(ta=(VTI_adj-VTI_vwap), col='red')
# Add background shading of areas
addTA((VTI_adj-VTI_vwap) > 0, on=-1,
col="lightgreen", border="lightgreen")
addTA((VTI_adj-VTI_vwap) < 0, on=-1,
col="lightgrey", border="lightgrey")
# Add vertical and horizontal lines at VTI_vwap minimum
addLines(v=which.min(VTI_vwap), col='red')
addLines(h=min(VTI_vwap), col='red')

library(quantmod)
library(TTR)
oh_lc <- rutils::etf_env$VTI
VTI_adj <- Ad(oh_lc)
VTI_vol <- Vo(oh_lc)
VTI_vwap <- TTR::VWAP(price=VTI_adj, volume=VTI_vol, n=10)
VTI_adj <- VTI_adj["2009-02/2009-03"]
oh_lc <- oh_lc["2009-02/2009-03"]
VTI_vwap <- VTI_vwap["2009-02/2009-03"]
# OHLC candlechart VWAP in main plot,
chart_Series(x=oh_lc, # volume in extra panel
       TA="add_Vo(); add_TA(VTI_vwap, on=1)",
       name="VTI plus VWAP shaded")
# Add price minus VWAP in extra panel
add_TA(VTI_adj-VTI_vwap, col='red')
# Add background shading of areas
add_TA((VTI_adj-VTI_vwap) > 0, on=-1,
col="lightgreen", border="lightgreen")
add_TA((VTI_adj-VTI_vwap) < 0, on=-1,
col="lightgrey", border="lightgrey")
# Add vertical and horizontal lines
abline(v=which.min(VTI_vwap), col='red')
abline(h=min(VTI_vwap), col='red')

library(quantmod)
oh_lc <- rutils::etf_env$VTI["2009-02/2009-03"]
# Extract plot object
ch_ob <- chart_Series(x=oh_lc, plot=FALSE)
class(ch_ob)
ls(ch_ob)
class(ch_ob$get_ylim)
class(ch_ob$set_ylim)
# ls(ch_ob$Env)
class(ch_ob$Env$actions)
plot_theme <- chart_theme()
class(plot_theme)
ls(plot_theme)

library(quantmod)
oh_lc <- rutils::etf_env$VTI["2010-04/2010-05"]
# Extract, modify theme, format tick marks "%b %d"
plot_theme <- chart_theme()
plot_theme$format.labels <- "%b %d"
# Create plot object
ch_ob <- chart_Series(x=oh_lc,
                theme=plot_theme, plot=FALSE)
# Extract ylim using accessor function
y_lim <- ch_ob$get_ylim()
y_lim[[2]] <- structure(
  range(Ad(oh_lc)) + c(-1, 1),
  fixed=TRUE)
# modify plot object to reduce y-axis range
ch_ob$set_ylim(y_lim)  # use setter function
# Render the plot
plot(ch_ob)

library(HighFreq)
# Calculate VTI and XLF volume-weighted average price
VTI_vwap <-
  TTR::VWAP(price=Ad(rutils::etf_env$VTI),
      volume=Vo(rutils::etf_env$VTI), n=10)
XLF_vwap <-
  TTR::VWAP(price=Ad(rutils::etf_env$XLF),
      volume=Vo(rutils::etf_env$XLF), n=10)
# open graphics device, and define
# Plot area with two horizontal panels
x11(); par(mfrow=c(2, 1))
ch_ob <- chart_Series(  # Plot in top panel
  x=etf_env$VTI["2009-02/2009-04"],
  name="VTI", plot=FALSE)
add_TA(VTI_vwap["2009-02/2009-04"],
 lwd=2, on=1, col='blue')
ch_ob <- chart_Series(  # Plot in bottom panel
  x=etf_env$XLF["2009-02/2009-04"],
  name="XLF", plot=FALSE)
add_TA(XLF_vwap["2009-02/2009-04"],
 lwd=2, on=1, col='blue')

library(dygraphs)
# Calculate volume-weighted average price
oh_lc <- rutils::etf_env$VTI
VTI_vwap <- TTR::VWAP(price=quantmod::Ad(oh_lc),
    volume=quantmod::Vo(oh_lc), n=20)
# Add VWAP to OHLC  data
oh_lc <- cbind(oh_lc[, c(1:3, 6)],
         VTI_vwap)["2009-02/2009-04"]
# Create dygraphs object
dy_graph <- dygraphs::dygraph(oh_lc)
# Convert dygraphs object to candlestick plot
dy_graph <- dygraphs::dyCandlestick(dy_graph)
# Render candlestick plot
dy_graph
# Candlestick plot using pipes syntax
dygraphs::dygraph(oh_lc) %>% dyCandlestick()
# Candlestick plot without using pipes syntax
dygraphs::dyCandlestick(dygraphs::dygraph(oh_lc))

# Create candlestick plot with background shading
in_dex <- index(oh_lc)
in_dic <-
  rutils::diff_it(oh_lc[, 4] > oh_lc[, "VWAP"])
in_dic <- rbind(cbind(which(in_dic==1), 1),
  cbind(which(in_dic==(-1)), -1))
in_dic <- in_dic[order(in_dic[, 1]), ]
in_dic <- rbind(c(1, -in_dic[1, 2]), in_dic,
  c(NROW(oh_lc), -in_dic[NROW(in_dic), 2]))
in_dic <-
  data.frame(in_dex[in_dic[, 1]], in_dic[, 2])
# Create dygraphs object
dy_graph <- dygraphs::dygraph(oh_lc) %>%
  dyCandlestick()
# Add shading
for (i in 1:(NROW(in_dic)-1)) {
  if (in_dic[i, 2] == 1)
    dy_graph <- dy_graph %>% dyShading(from=in_dic[i, 1], to=in_dic[i+1, 1], color="lightgreen")
  else
    dy_graph <- dy_graph %>% dyShading(from=in_dic[i, 1], to=in_dic[i+1, 1], color="antiquewhite")
}  # End for
# Render plot
dy_graph

library(dygraphs)
# Prepare VTI and IEF prices
price_s <- cbind(Ad(rutils::etf_env$VTI),
           Ad(rutils::etf_env$IEF))
price_s <- na.omit(price_s)
col_names <- rutils::get_name(colnames(price_s))
colnames(price_s) <- col_names
# dygraphs plot with two y-axes
library(dygraphs)
dygraphs::dygraph(price_s, main=paste(col_names, collapse=" and ")) %>%
  dyAxis(name="y", label="VTI", independentTicks=TRUE) %>%
  dyAxis(name="y2", label="IEF", independentTicks=TRUE) %>%
  dySeries(name="VTI", axis="y", col="red") %>%
  dySeries(name="IEF", axis="y2", col="blue")

load(file="C:/Develop/R/lecture_slides/data/zoo_data.RData")
library(tseries)  # Load package tseries
# Download MSFT data in ts format
ts_stx <- suppressWarnings(
  get.hist.quote(
    instrument="MSFT",
    start=Sys.Date()-3*365,
    end=Sys.Date(),
    retclass="ts",
    quote=c("Open","High","Low","Close",
      "AdjClose","Volume"),
    origin="1970-01-01")
)  # end suppressWarnings
# Calculate price adjustment vector
adj_vector <-
  as.vector(ts_stx[, "AdjClose"] / ts_stx[, "Close"])
# Adjust OHLC prices
ts_stx_adj <- ts_stx
ts_stx_adj[, c("Open","High","Low","Close")] <-
  adj_vector * ts_stx[, c("Open","High","Low","Close")]
# Inspect the data
tsp(ts_stx_adj)  # frequency=1
head(time(ts_stx_adj))
head(ts_stx_adj)
tail(ts_stx_adj)

library(tseries)  # Load package tseries
# Download MSFT data
zoo_stx <- suppressWarnings(
  get.hist.quote(
    instrument="MSFT",
    start=Sys.Date()-3*365,
    end=Sys.Date(),
    quote=c("Open","High","Low","Close",
      "AdjClose","Volume"),
    origin="1970-01-01")
)  # end suppressWarnings

load(file="C:/Develop/R/lecture_slides/data/zoo_data.RData")
class(zoo_stx)
dim(zoo_stx)
head(zoo_stx, 4)

library(tseries)  # Load package tseries
load(file="C:/Develop/R/lecture_slides/data/zoo_data.RData")
# Calculate price adjustment vector
adj_vector <-
  as.vector(zoo_stx[, "AdjClose"] / zoo_stx[, "Close"])
head(adj_vector, 5)
tail(adj_vector, 5)
# Adjust OHLC prices
zoo_stx_adj <- zoo_stx
zoo_stx_adj[, c("Open","High","Low","Close")] <-
  adj_vector * zoo_stx[, c("Open","High","Low","Close")]
head(zoo_stx_adj)
tail(zoo_stx_adj)

library(tseries)  # Load package tseries
# Download EUR/USD data
zoo_eurusd <- suppressWarnings(
  get.hist.quote(
    instrument="EUR/USD",
    provider="oanda",
    start=Sys.Date()-3*365,
    end=Sys.Date(),
    origin="1970-01-01")
)  # end suppressWarnings
# bind and scrub data
zoo_stxeur <- cbind(zoo_eurusd,
               zoo_stx[, "AdjClose"])
colnames(zoo_stxeur) <- c("EURUSD", "MSFT")
zoo_stxeur <-
  zoo_stxeur[complete.cases(zoo_stxeur),]
save(zoo_stx, zoo_stx_adj,
     ts_stx, ts_stx_adj,
     zoo_eurusd, zoo_stxeur,
     file="C:/Develop/R/lecture_slides/data/zoo_data.RData")

load(file="C:/Develop/R/lecture_slides/data/zoo_data.RData")
# Inspect the data
class(zoo_eurusd)
head(zoo_eurusd, 4)

library(xtable)
# Define ETF symbols for asset allocation
sym_bols <- c("VTI", "VEU", "IEF", "VNQ",
  "DBC", "XLY", "XLP", "XLE", "XLF", "XLV",
  "XLI", "XLB", "XLK", "XLU", "VYM", "IVW",
  "IWB", "IWD", "IWF", "VXX", "SVXY")
# Read etf database into data frame
etf_list <- read.csv(
  file="C:/Develop/R/lecture_slides/data/etf_list.csv",
         stringsAsFactors=FALSE)
rownames(etf_list) <- etf_list$Symbol
# Select from etf_list only those ETF's in sym_bols
etf_list <- etf_list[sym_bols, ]
# Shorten names
etf_names <- sapply(etf_list$Name,
              function(name) {
  name_split <- strsplit(name, split=" ")[[1]]
  name_split <-
    name_split[c(-1, -NROW(name_split))]
  name_match <- match("Select", name_split)
  if (!is.na(name_match))
    name_split <- name_split[-name_match]
  paste(name_split, collapse=" ")
})  # end sapply
etf_list$Name <- etf_names
etf_list["IEF", "Name"] <- "Treasury Bond Fund"
etf_list["XLY", "Name"] <- "Consumer Discr. Sector Fund"
etf_list["SVXY", "Name"] <- "Short VIX Futures"

print(xtable(etf_list), comment=FALSE, size="tiny", include.rownames=FALSE)

library(tseries)  # Load package tseries
# Download price and volume data for sym_bols into list of zoo objects
zoo_series <- suppressWarnings(
  lapply(sym_bols, # Loop for loading data
   get.hist.quote,
   quote=c("AdjClose", "Volume"),
   start=Sys.Date()-3650,
   end=Sys.Date(),
   origin="1970-01-01")  # end lapply
)  # end suppressWarnings
# flatten list of zoo objects into a single zoo object
zoo_series <- rutils::do_call(cbind, zoo_series)
# Or
# zoo_series <- do.call(cbind, zoo_series)
# Assign names in format "symbol.Close", "symbol.Volume"
names(zoo_series) <-
  as.vector(sapply(sym_bols,
    paste, c("Close", "Volume"), sep="."))
# Save zoo_series to a comma-separated CSV file
write.zoo(zoo_series, file="zoo_series.csv", sep=",")
# Save zoo_series to a binary .RData file
save(zoo_series, file="zoo_series.RData")

library(HighFreq)  # Load package HighFreq
etf_env <- new.env()  # new environment for data
# Download data for sym_bols into etf_env from Alpha Vantage
getSymbols.av(sym_bols, adjust=TRUE, env=etf_env,
  output.size="full", api.key="T7JPW54ES8G75310")
# getSymbols(sym_bols, env=etf_env, adjust=TRUE, from="2005-01-03")

library(HighFreq)  # Load package HighFreq
ls(etf_env)  # List files in etf_env
# get class of object in etf_env
class(get(x=sym_bols[1], envir=etf_env))
# Another way
class(etf_env$VTI)
colnames(etf_env$VTI)
head(etf_env$VTI, 3)
# get class of all objects in etf_env
eapply(etf_env, class)
# get class of all objects in R workspace
lapply(ls(), function(ob_ject) class(get(ob_ject)))

library(HighFreq)  # Load package HighFreq
# Check of object is an OHLC time series
is.OHLC(etf_env$VTI)
# Adjust single OHLC object using its name
etf_env$VTI <- adjustOHLC(etf_env$VTI,
                    use.Adjusted=TRUE)

# Adjust OHLC object using string as name
assign(sym_bols[1], adjustOHLC(
    get(x=sym_bols[1], envir=etf_env),
    use.Adjusted=TRUE),
  envir=etf_env)

# Adjust objects in environment using vector of strings
for (sym_bol in ls(etf_env)) {
  assign(sym_bol,
   adjustOHLC(get(sym_bol, envir=etf_env),
              use.Adjusted=TRUE),
   envir=etf_env)
}  # end for

library(HighFreq)  # Load package HighFreq
# extract and cbind all data, subset by symbols
price_s <- rutils::do_call(cbind,
  as.list(etf_env)[sym_bols])
# Or
# price_s <- do.call(cbind,
#   as.list(etf_env)[sym_bols])
# extract and cbind adjusted prices, subset by symbols
price_s <- rutils::do_call(cbind,
  lapply(as.list(etf_env)[sym_bols], Ad))
# Same, but works only for OHLC series
price_s <- rutils::do_call(cbind,
  eapply(etf_env, Ad)[sym_bols])
# Drop ".Adjusted" from colnames
colnames(price_s) <-
  sapply(colnames(price_s),
    function(col_name)
strsplit(col_name, split="[.]")[[1]],
    USE.NAMES=FALSE)[1, ]
head(price_s[, 1:2], 3)
# Which objects in global environment are class xts?
unlist(eapply(globalenv(), is.xts))
# Save xts to csv file
write.zoo(price_s,
  file="etf_series.csv", sep=",")
# Copy price_s into etf_env and save to .RData file
assign("price_s", price_s, envir=etf_env)
save(etf_env, file="etf_data.RData")

# extract VTI prices
vt_i <- etf_env$price_s[ ,"VTI"]
vt_i <- na.omit(vt_i)
# Calculate percentage returns "by hand"
vti_lag <- as.numeric(vt_i)
vti_lag <- c(vti_lag[1], vti_lag[-NROW(vti_lag)])
vti_lag <- xts(vti_lag, index(vt_i))
vti_returns <- (vt_i-vti_lag)/vti_lag
# Calculate percentage returns using dailyReturn()
daily_returns <- quantmod::dailyReturn(vt_i)
head(cbind(daily_returns, vti_returns))
all.equal(daily_returns, vti_returns, check.attributes=FALSE)
# Calculate returns for all prices in etf_env$price_s
re_turns <- lapply(etf_env$price_s, function(x_ts) {
  daily_returns <- quantmod::dailyReturn(x_ts)
  colnames(daily_returns) <- names(x_ts)
  daily_returns
})  # end lapply
# "re_turns" is a list of xts
class(re_turns)
class(re_turns[[1]])
# flatten list of xts into a single xts
re_turns <- rutils::do_call(cbind, re_turns)
class(re_turns)
dim(re_turns)
# Copy re_turns into etf_env and save to .RData file
assign("re_turns", re_turns, envir=etf_env)
save(etf_env, file="etf_data.RData")

library(quantmod)
start_date <- "2012-05-10"; end_date <- "2013-11-20"
# Select all objects in environment and return as environment
new_env <- as.environment(eapply(etf_env, "[",
            paste(start_date, end_date, sep="/")))
# Select only sym_bols in environment and return as environment
new_env <- as.environment(
  lapply(as.list(etf_env)[sym_bols], "[",
   paste(start_date, end_date, sep="/")))
# extract and cbind adjusted prices and return to environment
assign("price_s", rutils::do_call(cbind,
         lapply(ls(etf_env), function(sym_bol) {
           x_ts <- Ad(get(sym_bol, etf_env))
           colnames(x_ts) <- sym_bol
           x_ts
         })), envir=new_env)
# get sizes of OHLC xts series in etf_env
sapply(mget(sym_bols, envir=etf_env), object.size)
# extract and cbind adjusted prices and return to environment
col_name <- function(x_ts)
  strsplit(colnames(x_ts), split="[.]")[[1]][1]
assign("price_s", rutils::do_call(cbind,
         lapply(mget(etf_env$sym_bols, envir=etf_env),
                function(x_ts) {
                  x_ts <- Ad(x_ts)
                  colnames(x_ts) <- col_name(x_ts)
                  x_ts
         })), envir=new_env)

# Load data frame of S&P500 constituents from CSV file
sp_500 <- read.csv(file="C:/Develop/R/lecture_slides/data/sp500_WRDS_08-30-17.csv", stringsAsFactors=FALSE)
# Inspect data frame of S&P500 constituents
dim(sp_500)
colnames(sp_500)
# Extract tickers from the column co_tic
sym_bols <- sp_500$co_tic
# Get duplicate tickers
ta_ble <- table(sym_bols)
ta_ble[ta_ble>1]
dupli_cate <- names(ta_ble[ta_ble>1])
# Get duplicate records (rows) of sp_500
sp_500[sym_bols %in% dupli_cate, ]
# Get unique tickers
sym_bols <- unique(sym_bols)
# Find index of ticker "BRK.B"
which(sym_bols=="BRK.B")
# Remove "BRK.B" and later download it separately
sym_bols <- sym_bols[-which(sym_bols=="BRK.B")]

# Load package HighFreq
library(HighFreq)
# Create new environment for data
env_sp500 <- new.env()
# Boolean vector of symbols already downloaded
down_loaded <- sym_bols %in% ls(env_sp500)
# Download in while loop from Tiingo and copy into environment
at_tempt <- 0  # number of download attempts
while (((sum(!down_loaded)) > 0) & (at_tempt<5)) {
  # Download data and copy it into environment
  at_tempt <- at_tempt + 1
  cat("Download attempt = ", at_tempt, "\n")
  for (sym_bol in sym_bols[!down_loaded]) {
    cat("processing: ", sym_bol, "\n")
    tryCatch(  # With error handler
getSymbols(sym_bol, src="tiingo", adjust=TRUE,
           from="1990-01-01", env=env_sp500, api.key="j84ac2b9c5bde2d68e33034f65d838092c6c9f10"),
# error handler captures error condition
error=function(error_cond) {
  print(paste("error handler: ", error_cond))
},  # end error handler
finally=print(paste("sym_bol=", sym_bol))
    )  # end tryCatch
  }  # end for
  # Update vector of symbols already downloaded
  down_loaded <- sym_bols %in% ls(env_sp500)
  Sys.sleep(10)  # Wait 10 seconds until next attempt
}  # end while
class(env_sp500$AAPL)
class(index(env_sp500$AAPL))

library(quantmod)
# Rename "LOW" colnames to "LO_WES"
colnames(env_sp500$LOW) <- paste("LO_WES",
  sapply(strsplit(colnames(env_sp500$LOW), split="[.]"),
   function(col_name) col_name[2]), sep=".")
env_sp500$LO_WES <- env_sp500$LOW[, unique(colnames(env_sp500$LOW))]
rm(LOW, envir=env_sp500)
chart_Series(x=env_sp500$LO_WES["2017-06/"],
  TA="add_Vo()", name="LOWES stock")
# Download "BRK.B" separately with auto.assign=FALSE
BRK_B <- getSymbols("BRK-B", auto.assign=FALSE, src="tiingo", adjust=TRUE, from="1990-01-01", api.key="j84ac2b9c5bde2d68e33034f65d838092c6c9f10")
colnames(BRK_B) <- paste("BRK_B",
  sapply(strsplit(colnames(BRK_B), split="[.]"),
   function(col_name) col_name[2]), sep=".")
env_sp500$BRK_B <- BRK_B

# Rename "BF-B" colnames to "BF_B"
colnames(env_sp500$"BF-B") <- paste("BF_B",
  sapply(strsplit(colnames(env_sp500$"BF-B"), split="[.]"),
   function(col_name) col_name[2]), sep=".")
names(colnames(env_sp500$"BF-B")) <- NULL
env_sp500$BF_B <- env_sp500$"BF-B"
rm("BF-B", envir=env_sp500)

class(env_sp500$AAPL)
# The date-time index is class POSIXct not Date
class(index(env_sp500$AAPL))
# Coerce time indices from class POSIXct to class Date
for (sym_bol in ls(env_sp500)) {
  x_ts <- get(sym_bol, envir=env_sp500)
  index(x_ts) <- as.Date(index(x_ts))
  assign(sym_bol, x_ts, envir=env_sp500)
}  # end for
class(index(env_sp500$AAPL))
# Save the environment to compressed .RData file
dir_name <- "C:/Develop/R/lecture_slides/data/"
save(env_sp500, file=paste0(dir_name, "sp500.RData"))
# Save the ETF prices into CSV files
dir_name <- "C:/Develop/R/lecture_slides/data/SP500/"
for (sym_bol in ls(env_sp500)) {
  zoo::write.zoo(env_sp500$sym_bol, file=paste0(dir_name, sym_bol, ".csv"))
}  # end for
# Or using lapply()
file_names <- lapply(ls(env_sp500), function(sym_bol) {
  x_ts <- get(sym_bol, envir=env_sp500)
  zoo::write.zoo(x_ts, file=paste0(dir_name, sym_bol, ".csv"))
  sym_bol
})  # end lapply
unlist(file_names)
# Or using eapply() and data.table::fwrite()
file_names <- eapply(env_sp500 , function(x_ts) {
  file_name <- rutils::get_name(colnames(x_ts)[1])
  data.table::fwrite(data.table::as.data.table(x_ts), file=paste0(dir_name, file_name, ".csv"))
  file_name
})  # end eapply
unlist(file_names)

# Load the environment from compressed .RData file
dir_name <- "C:/Develop/R/lecture_slides/data/"
load(file=paste0(dir_name, "sp500.RData"))
# Get all the .csv file names in the directory
dir_name <- "C:/Develop/R/lecture_slides/data/SP500/"
file_names <- Sys.glob(paste0(dir_name, "*.csv"))
# Create new environment for data
env_sp500 <- new.env()
for (file_name in file_names) {
  x_ts <- xts::as.xts(zoo::read.csv.zoo(file_name))
  sym_bol <- strsplit(colnames(x_ts), split="[.]")[[1]][1]
  assign(sym_bol, x_ts, envir=env_sp500)
}  # end for
# Or using fread()
for (file_name in file_names) {
  x_ts <- data.table::fread(file_name)
  data.table::setDF(x_ts)
  x_ts <- xts::xts(x_ts[, -1], as.Date(x_ts[, 1]))
  sym_bol <- strsplit(colnames(x_ts), split="[.]")[[1]][1]
  assign(sym_bol, x_ts, envir=env_sp500)
}  # end for

# Remove all files from environment(if necessary)
rm(list=ls(env_sp500), envir=env_sp500)
# Download in while loop from Alpha Vantage and copy into environment
down_loaded <- sym_bols %in% ls(env_sp500)
at_tempt <- 0
while (((sum(!down_loaded)) > 0) & (at_tempt<10)) {
  # Download data and copy it into environment
  at_tempt <- at_tempt + 1
  for (sym_bol in sym_bols[!down_loaded]) {
    cat("processing: ", sym_bol, "\n")
    tryCatch(  # With error handler
getSymbols(sym_bol, src="av", adjust=TRUE, env=env_sp500,
           output.size="full", api.key="T7JPW54ES8G75310"),
# error handler captures error condition
error=function(error_cond) {
  print(paste("error handler: ", error_cond))
},  # end error handler
finally=print(paste("sym_bol=", sym_bol))
    )  # end tryCatch
  }  # end for
  # Update vector of symbols already downloaded
  down_loaded <- sym_bols %in% ls(env_sp500)
  Sys.sleep(10)  # Wait 10 seconds until next attempt
}  # end while
# Adjust all OHLC prices in environment
for (sym_bol in ls(env_sp500)) {
  assign(sym_bol,
    adjustOHLC(get(x=sym_bol, envir=env_sp500), use.Adjusted=TRUE),
    envir=env_sp500)
}  # end for

library(HighFreq)  # Load package HighFreq
# Assign name SP500 to ^GSPC symbol
setSymbolLookup(
  SP500=list(name="^GSPC", src="yahoo"))
getSymbolLookup()
# view and clear options
options("getSymbols.sources")
options(getSymbols.sources=NULL)
# Download S&P500 prices into etf_env
getSymbols("SP500", env=etf_env,
    adjust=TRUE, from="1990-01-01")
chart_Series(x=etf_env$SP500["2016/"],
       TA="add_Vo()",
       name="S&P500 index")

library(HighFreq)  # Load package HighFreq
# Assign name DJIA to ^DJI symbol
setSymbolLookup(
  DJIA=list(name="^DJI", src="yahoo"))
getSymbolLookup()
# view and clear options
options("getSymbols.sources")
options(getSymbols.sources=NULL)
# Download DJIA prices into etf_env
getSymbols("DJIA", env=etf_env,
    adjust=TRUE, from="1990-01-01")
chart_Series(x=etf_env$DJIA["2016/"],
       TA="add_Vo()",
       name="DJIA index")

library(HighFreq)  # Load package HighFreq
library(RCurl)  # Load package RCurl
library(XML)  # Load package XML
# Download text data from URL
sp_500 <- getURL(
  "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies")
# extract tables from the text data
sp_500 <- readHTMLTable(sp_500,
              stringsAsFactors=FALSE)
str(sp_500)
# extract colnames of data frames
lapply(sp_500, colnames)
# extract S&P500 constituents
sp_500 <- sp_500[[1]]
head(sp_500)
# Create valid R names from symbols containing "-" or "."characters
sp_500$names <- gsub("-", "_", sp_500$Ticker)
sp_500$names <- gsub("[.]", "_", sp_500$names)
# Write data frame of S&P500 constituents to CSV file
write.csv(sp_500,
  file="C:/Develop/R/lecture_slides/data/sp500_Yahoo.csv",
  row.names=FALSE)

library(HighFreq)  # Load package HighFreq
# Load data frame of S&P500 constituents from CSV file
sp_500 <- read.csv(file="C:/Develop/R/lecture_slides/data/sp500_Yahoo.csv",
     stringsAsFactors=FALSE)
# Register symbols corresponding to R names
for (in_dex in 1:NROW(sp_500)) {
  cat("processing: ", sp_500$Ticker[in_dex], "\n")
  setSymbolLookup(structure(
    list(list(name=sp_500$Ticker[in_dex])),
    names=sp_500$names[in_dex]))
}  # end for
env_sp500 <- new.env()  # new environment for data
# Remove all files (if necessary)
rm(list=ls(env_sp500), envir=env_sp500)
# Download data and copy it into environment
rutils::get_symbols(sp_500$names,
   env_out=env_sp500, start_date="1990-01-01")
# Or download in loop
for (sym_bol in sp_500$names) {
  cat("processing: ", sym_bol, "\n")
  rutils::get_symbols(sym_bol,
   env_out=env_sp500, start_date="1990-01-01")
}  # end for
save(env_sp500, file="C:/Develop/R/lecture_slides/data/sp500.RData")
chart_Series(x=env_sp500$BRK_B["2016/"], TA="add_Vo()",
       name="BRK-B stock")

library(quantmod)
# Download U.S. unemployment rate data
unemp_rate <- getSymbols("UNRATE",
            auto.assign=FALSE,
            src="FRED")
# plot U.S. unemployment rate data
chart_Series(unemp_rate["1990/"],
      name="U.S. unemployment rate")

library(HighFreq)  # Load package HighFreq
install.packages("devtools")
library(devtools)
# Install package Quandl from github
install_github("quandl/R-package")
library(Quandl)  # Load package Quandl
# Register Quandl API key
Quandl.api_key("pVJi9Nv3V8CD3Js5s7Qx")
# get short description
packageDescription("Quandl")
# Load help page
help(package="Quandl")
# Remove Quandl from search path
detach("package:Quandl")

library(HighFreq)  # Load package HighFreq
# Download EOD AAPL prices from WIKI free database
price_s <- Quandl(code="WIKI/AAPL",
            type="xts", start_date="1990-01-01")
x11(width=14, height=7)
chart_Series(price_s["2016", 1:4],
    name="AAPL OHLC prices")
# Add trade volume in extra panel
add_TA(price_s["2016", 5])
# Download euro currency rates
price_s <- Quandl(code="BNP/USDEUR",
    start_date="2013-01-01",
    end_date="2013-12-01", type="xts")
# Download multiple time series
price_s <- Quandl(code=c("NSE/OIL", "WIKI/AAPL"),
    start_date="2013-01-01", type="xts")
# Download AAPL gross profits
prof_it <- Quandl("RAYMOND/AAPL_GROSS_PROFIT_Q",
    type="xts")
chart_Series(prof_it, name="AAPL gross profits")
# Download Hurst time series
price_s <- Quandl(code="PE/AAPL_HURST",
    start_date="2013-01-01", type="xts")
chart_Series(price_s["2016/", 1],
       name="AAPL Hurst")

library(HighFreq)  # Load package HighFreq
# Load S&P500 stock Quandl codes
sp_500 <- read.csv(
  file="C:/Develop/R/lecture_slides/data/sp500_quandl.csv",
  stringsAsFactors=FALSE)
# Replace "-" with "_" in symbols
sp_500$free_code <-
  gsub("-", "_", sp_500$free_code)
head(sp_500)
# vector of symbols in sp_500 frame
tick_ers <- gsub("-", "_", sp_500$ticker)
# Or
tick_ers <- matrix(unlist(
  strsplit(sp_500$free_code, split="/"),
  use.names=FALSE), ncol=2, byrow=TRUE)[, 2]
# Or
tick_ers <- do_call_rbind(
  strsplit(sp_500$free_code, split="/"))[, 2]

library(HighFreq)  # Load package HighFreq
env_sp500 <- new.env()  # new environment for data
# Remove all files (if necessary)
rm(list=ls(env_sp500), envir=env_sp500)
# Boolean vector of symbols already downloaded
down_loaded <- tick_ers %in% ls(env_sp500)
# Download data and copy it into environment
for (tick_er in tick_ers[!down_loaded]) {
  cat("processing: ", tick_er, "\n")
  da_ta <- Quandl(code=paste0("WIKI/", tick_er),
            start_date="1990-01-01",
            type="xts")[, -(1:7)]
  colnames(da_ta) <- paste(tick_er,
    c("Open", "High", "Low", "Close", "Volume"), sep=".")
  assign(tick_er, da_ta, envir=env_sp500)
}  # end for
save(env_sp500, file="C:/Develop/R/lecture_slides/data/sp500.RData")
chart_Series(x=env_sp500$XOM["2016/"], TA="add_Vo()",
       name="XOM stock")

library(HighFreq)
library(Quandl)
# Register Quandl API key
Quandl.api_key("pVJi9Nv3V8CD3Js5s7Qx")
# Download E-mini S&P500 futures prices
price_s <- Quandl(code="CHRIS/CME_ES1",
  type="xts", start_date="1990-01-01")
price_s <- price_s[, c("Open", "High", "Low", "Last", "Volume")]
colnames(price_s)[4] <- "Close"
# plot the prices
x11(width=5, height=4)  # Open x11 for plotting
chart_Series(x=price_s["2008-06/2009-06"],
       TA="add_Vo()",
       name="S&P500 Futures")
# plot dygraph
dygraphs::dygraph(price_s["2008-06/2009-06", -5],
  main="S&P500 Futures") %>%
  dyCandlestick()

# Read CBOE futures expiration dates
date_s <- read.csv(file="C:/Develop/R/lecture_slides/data/futures_expiration_dates_codes.csv",
  stringsAsFactors=FALSE, row.names=1)
dir_name <- "C:/Develop/data/vix_data"
dir.create(dir_name)
sym_bols <- rownames(date_s)
file_names <- file.path(dir_name, paste0(sym_bols, ".csv"))
log_file <- file.path(dir_name, "log_file.txt")
cboe_url <- "https://markets.cboe.com/us/futures/market_statistics/historical_data/products/csv/VX/"
url_s <- paste0(cboe_url, date_s[, 1])
# Download files in loop
for (it in seq_along(url_s)) {
    tryCatch(  # Warning and error handler
  download.file(url_s[it],
          destfile=file_names[it], quiet=TRUE),
# Warning handler captures warning condition
warning=function(warning_cond) {
  cat(paste("warning handler: ", warning_cond, "\n"), file=log_file, append=TRUE)
},  # end warning handler
# error handler captures error condition
error=function(error_cond) {
  cat(paste("error handler: ", error_cond, "\n"), append=TRUE)
},  # end error handler
finally=cat(paste("Processing file name =", file_names[it], "\n"), append=TRUE)
    )  # end tryCatch
}  # end for

# Create new environment for data
vix_env <- new.env()
# Download VIX data for the months 6, 7, and 8 in 2018
library(qmao)
quantmod::getSymbols("VX", Months=1:12,
  Years=2018, src="cfe", env=vix_env)
# Or
qmao::getSymbols.cfe(Symbols="VX",
  Months=6:8, Years=2018, env=vix_env,
  verbose=FALSE, auto.assign=TRUE)
# Calculate the classes of all the objects
# In the environment vix_env
unlist(eapply(vix_env,
  function(x) {class(x)[1]}))
class(vix_env$VX_M18)
colnames(vix_env$VX_M18)
# Save the data to a binary file called "vix_cboe.RData".
save(vix_env,
  file="C:/Develop/data/vix_data/vix_cboe.RData")
