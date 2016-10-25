library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
options(digits=3)
thm <- knit_theme$get("acid")
knit_theme$set(thm)
# ?options  # get info on global options
getOption("warn")  # global option for "warn"
options("warn")  # global option for "warn"
getOption("error")  # global option for "error"
sqrt_safe <- function(in_put) {
# returns its argument
  if (in_put<0) {
    warning("sqrt_safe: in_put is negative")
    NULL  # return NULL for negative argument
  } else {
    sqrt(in_put)
  }  # end if
}  # end sqrt_safe
sqrt_safe(5)
sqrt_safe(-1)
options(warn=-1)
sqrt_safe(-1)
options(warn=0)
sqrt_safe()
options(warn=1)
sqrt_safe()
options(warn=3)
sqrt_safe()
# function vali_date validates its arguments
vali_date <- function(in_put=NULL) {
# check if argument is valid and return double
  if (is.null(in_put)) {
    return("vali_date: in_put is missing")
  } else if (is.numeric(in_put)) {
    2*in_put
  } else cat("vali_date: in_put not numeric")
}  # end vali_date
vali_date(3)
vali_date("a")
vali_date()
# vali_date validates arguments using missing()
vali_date <- function(in_put) {
# check if argument is valid and return double
  if (missing(in_put)) {
    return("vali_date: in_put is missing")
  } else if (is.numeric(in_put)) {
    2*in_put
  } else cat("vali_date: in_put is not numeric")
}  # end vali_date
vali_date(3)
vali_date("a")
vali_date()
# vali_date() validates its arguments and assertions
vali_date <- function(in_put) {
# check if argument is valid and return double
  if (missing(in_put)) {
    stop("vali_date: in_put is missing")
  } else if (!is.numeric(in_put)) {
    cat("in_put=", in_put)
    stop("vali_date: in_put is not numeric")
  } else 2*in_put
}  # end vali_date
vali_date(3)
vali_date("a")
vali_date()
# print the call stack
traceback()
vali_date <- function(in_put) {
# check argument using long form '&&' operator
  stopifnot(!missing(in_put) &&
      is.numeric(in_put))
  2*in_put
}  # end vali_date
vali_date(3)
vali_date()
vali_date("a")
vali_date <- function(in_put) {
# check argument using logical '&' operator
  stopifnot(!missing(in_put) & is.numeric(in_put))
  2*in_put
}  # end vali_date
vali_date()
vali_date("a")
# sum_two() returns the sum of its two arguments
sum_two <- function(in_put1, in_put2) {  # even more robust
# check if at least one argument is not missing
  stopifnot(!missing(in_put1) &&
      !missing(in_put2))
# check if arguments are valid and return sum
  if (is.numeric(in_put1) &&
      is.numeric(in_put2)) {
    in_put1 + in_put2  # both valid
  } else if (is.numeric(in_put1)) {
    cat("in_put2 is not numeric\n")
    in_put1  # in_put1 is valid
  } else if (is.numeric(in_put2)) {
    cat("in_put1 is not numeric\n")
    in_put2  # in_put2 is valid
  } else {
    stop("none of the arguments are numeric")
  }
}  # end sum_two
sum_two(1, 2)
sum_two(5, 'a')
sum_two('a', 5)
sum_two('a', 'b')
sum_two()
# flag "vali_date" for debugging
debug(vali_date)
# calling "vali_date" starts debugger
vali_date(3)
# unflag "vali_date" for debugging
undebug(vali_date)
vali_date <- function(in_put) {
  browser()  # pause and invoke browser
# check argument using long form '&&' operator
  stopifnot(!missing(in_put) &&
      is.numeric(in_put))
  2*in_put
}  # end vali_date
vali_date()  # invokes debugger
options("error")  # show default NULL "error" option
options(error=recover)  # set "error" option to "recover"
options(error=NULL)  # set back to default "error" option
str(tryCatch)  # get arguments of tryCatch()
tryCatch(  # without error handler
  {  # evaluate expressions
    num_var <- 101  # assign
    stop('my error')  # produce error
  },
  finally=print(paste("num_var=", num_var))
)  # end tryCatch

tryCatch(  # with error handler
  {  # evaluate expressions
    num_var <- 101  # assign
    stop('my error')  # produce error
  },
  error=function(error_cond)  # handler captures error condition
    print(paste("error handler: ", error_cond)),
  finally=print(paste("num_var=", num_var))
)  # end tryCatch
rm(list=ls())
# apply loop without tryCatch
apply(as.matrix(1:5), 1, function(num_var) {  # anonymous function
    stopifnot(num_var != 3)  # check for error
    cat("(cat) num_var =", num_var, "\n")  # broadcast
    paste("(return) num_var =", num_var)  # return value
  }  # end anonymous function
)  # end apply
# apply loop with tryCatch
apply(as.matrix(1:5), 1, function(num_var) {  # anonymous function
    tryCatch(  # with error handler
{  # body
  stopifnot(num_var != 3)  # check for error
  cat("(cat) num_var =", num_var, "\t")  # broadcast
  paste("(return) num_var =", num_var)  # return value
},
error=function(error_cond)  # handler captures error condition
  paste("handler: ", error_cond),
finally=print(paste("(finally) num_var =", num_var))
    )  # end tryCatch
  }  # end anonymous function
)  # end apply
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
foo <- mclapply(1:10, paws, mc.cores=num_cores,
          sleep_time=0.01)
# initialize compute cluster
clus_ter <- makeCluster(num_cores)
# perform parallel loop under Windows
foo <- parLapply(clus_ter, 1:10, paws,
           sleep_time=0.01)
library(microbenchmark)  # load package microbenchmark
# compare speed of lapply versus parallel computing
summary(microbenchmark(
  l_apply=lapply(1:10, paws, sleep_time=0.01),
  parl_apply=
    parLapply(clus_ter, 1:10, paws, sleep_time=0.01),
  times=10)
)[, c(1, 4, 5)]
# stop R processes over cluster
stopCluster(clus_ter)
library(parallel)  # load package parallel
# calculate number of available cores
num_cores <- detectCores() - 1
# initialize compute cluster
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
# initialize compute cluster
clus_ter <- makeCluster(num_cores)
# define large matrix
mat_rix <- matrix(rnorm(7*10^5), ncol=7)
# define aggregation function over column of matrix
agg_regate <- function(col_umn) {
  out_put <- 0
  for (in_dex in 1:length(col_umn))
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
# stop R processes over cluster
stopCluster(clus_ter)
set.seed(1121)  # initialize random number generator
# define explanatory and response variables
explana_tory <- rnorm(100, mean=2)
noise <- rnorm(100)
res_ponse <- -3 + explana_tory + noise
# define design matrix and regression formula
design_matrix <- data.frame(res_ponse, explana_tory)
reg_formula <- paste(colnames(design_matrix)[1],
  paste(colnames(design_matrix)[-1], collapse="+"),
  sep=" ~ ")
# bootstrap the regression
boot_strap <- sapply(1:100, function(x) {
  boot_sample <-
    sample.int(dim(design_matrix)[1], replace=TRUE)
  reg_model <- lm(reg_formula,
          data=design_matrix[boot_sample, ])
  reg_model$coefficients
})  # end sapply
par(oma=c(1, 2, 1, 0), mgp=c(2, 1, 0), mar=c(5, 1, 1, 1), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
x11(width=6, height=6)
# means and standard errors from bootstrap
apply(boot_strap, MARGIN=1,
      function(x) c(mean=mean(x), sd=sd(x)))
# means and standard errors from regression summary
reg_model <- lm(reg_formula, data=design_matrix)
reg_model$coefficients
summary(reg_model)$coefficients[, "Std. Error"]
plot(density(boot_strap["explana_tory", ]),
     lwd=2, xlab="regression slopes",
     main="Bootstrapped regression slopes")
abline(v=mean(boot_strap["explana_tory", ]),
       lwd=2, col="red")
library(parallel)  # load package parallel
num_cores <- detectCores() - 1  # number of cores
clus_ter <- makeCluster(num_cores)  # initialize compute cluster
# bootstrap the regression under Windows
boot_strap <- parLapply(clus_ter, 1:1000,
  function(x, reg_formula, design_matrix) {
    boot_sample <-
sample.int(dim(design_matrix)[1], replace=TRUE)
    reg_model <- lm(reg_formula,
data=design_matrix[boot_sample, ])
    reg_model$coefficients
  },
  reg_formula=reg_formula,
  design_matrix=design_matrix)  # end parLapply
# bootstrap the regression under Mac-OSX or Linux
boot_strap <- mclapply(1:1000,
  function(x) {
    boot_sample <-
sample.int(dim(design_matrix)[1], replace=TRUE)
    lm(reg_formula,
data=design_matrix[boot_sample, ])$coefficients
  }, mc.cores=num_cores)  # end mclapply
stopCluster(clus_ter)  # stop R processes over cluster
boot_strap <- do.call(rbind, boot_strap)
# means and standard errors from bootstrap
apply(boot_strap, MARGIN=2,
function(x) c(mean=mean(x), sd=sd(x)))
x11(width=6, height=6)
plot(density(boot_strap[, "explana_tory"]),
     lwd=2, xlab="regression slopes",
     main="Bootstrapped regression slopes")
abline(v=mean(boot_strap[, "explana_tory"]),
 lwd=2, col="red")
