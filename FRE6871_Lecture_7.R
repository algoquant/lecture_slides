library(knitr)
opts_chunk$set(prompt=TRUE, eval=TRUE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
options(digits=3)
thm <- knit_theme$get("acid")
knit_theme$set(thm)
library(zoo)  # load package zoo
# show the generic function "merge"
merge
# show the "merge" method dispatched to "zoo" objects
merge.zoo
library(zoo)  # load package zoo
# get all methods for generic function merge()
methods(generic.function="merge")
# get generic function methods applied to "zoo" objects
methods(class="zoo")
# define a generic function
gen_sum <- function (a, b, ...) {
  UseMethod("gen_sum")
}  # end gen_sum

# define method for "numeric" class
gen_sum.numeric <- function (a, b, ...) {
  sum(a, b)
}  # end gen_sum.character

# define method for "character" class
gen_sum.character <- function (a, b, ...) {
  paste(a, "plus", b)
}  # end gen_sum.character

# apply gen_sum to "numeric" objects
gen_sum(1, 2)
# apply gen_sum to "character" objects
gen_sum("a", "b")
# 'cbind' is an internal generic function
cbind
# define "+" method for "character" class
"+.character" <- function (a, b, ...) {
  paste(a, "plus", b)
}  # end +.character
methods("+")  # view methods for "+" operator
# define variables with "character" class
char1 <- "a"
char2 <- "b"
class(char1)
char1 + char2  # add two "character" objects - doesn't work
attributes(char1)  # doesn't have explicit "character" class - only implicit
char1 <- structure("a", class="character")
char2 <- structure("b", class="character")
attributes(char1)  # now has explicit "character" class
# add two "character" objects
char1 + char2
# define object of class "string"
obj_string <- "how are you today?"
class(obj_string) <- "string"
obj_string
# overload "print" method for string objects
print.string <- function (str_ing) {
  print(
    paste(strsplit(str_ing, split=" ")[[1]], 
  collapse=" + "))
}  # end print.string
# methods("print")  # view new methods for "print" function
print(obj_string)
obj_string
# overwrite "+" operator
"+" = function(a, b) {
  if(is.character(a) && is.character(b)) {
    paste(a, "plus", b)
  } else {
    .Primitive("+") (a, b)
  }
}
methods("+")  # view methods for "+" operator
# add two "numeric" objects
1 + 2
# add two "character" objects
"a" + "b"
# overwrite "+" operator with a generic function
"+" <- function (a, b, ...) {
  UseMethod("+")
}  # end gen_sum
# define method for "numeric" class
"+.numeric" <- function (a, b, ...) {
  sum(a, b)
}  # end gen_sum.character
# define method for "character" class
"+.character" <- function (a, b, ...) {
  paste(a, "plus", b)
}  # end gen_sum.character
methods("+")  # view methods for "+" operator
# add two "numeric" objects
1 + 2
# add two "character" objects
"a" + "b"
cbind.ts  # can't view non-visible method
stats::cbind.ts  # can't view non-visible method
stats:::cbind.ts  # display non-visible method
getAnywhere(cbind.ts)  # display non-visible method
rm(list=ls())
new_zoo <- zoo(rnorm(10), order.by=(Sys.Date() + 0:9))
# coerce "zoo" object to new class "zoo_xtra"
class(new_zoo) <- "zoo_xtra"
class(new_zoo)
methods(generic.function="length")
length  # primitive function
# define "length" method for class "zoo_xtra"
length.zoo_xtra <- function(in_ts) {
  cat("length of zoo_xtra object:\n")
# unclass object, then calculate length
  length(unclass(in_ts))
}  # end length.zoo_xtra
length(new_zoo)  # apply "length" method to "zoo_xtra" object
methods(generic.function="length")
# define "last" method for class "zoo_xtra"
last.zoo_xtra <- function(in_ts) {
  in_ts[length(in_ts)]
}  # end last.zoo_xtra
last(new_zoo)  # doesn't work
last.zoo_xtra(new_zoo)  # works
# define a generic function
last <- function (a, b, ...) {
  UseMethod("last")
}  # end last
last(new_zoo)  # now works
# define generic "string" class converter
as.string <- function (str_ing, ...) 
  UseMethod("as.string")
# default "string" class converter
as.string.default <- function (str_ing, ...)
  structure(str_ing, class="string", ...)
# numeric "string" class converter
as.string.numeric <- function (str_ing, ...)
  structure(as.character(str_ing), class="string", ...)
# "string" class checker
is.string <- function (str_ing)
  inherits(x=str_ing, what="string")
# define "string" object
obj_string <- as.string("how are you today?")
obj_string
is.string(obj_string)
is.string("hello")
as.string(123)
is.string(as.string(123))
rm(list=ls())
library(xts)
new_xts <- xts(rnorm(10), order.by=(Sys.Date() + 0:9))
class(new_xts)  # class attribute is a vector
# "last" is a generic function from package "xts"
last
methods(generic.function="last")
last(new_xts)  # apply "last" method from "xts" class
# derive object "xts_xtra" from "xts" object
class(new_xts) <- c("xts_xtra", class(new_xts))
class(new_xts)  # class attribute is a vector
# "xts_xtra" object inherits "last" method from "xts" class
last(new_xts)
# define new "last" method for class "xts_xtra"
last.xts_xtra <- function(in_ts) {
  cat("last element of xts_xtra object:\n")
  drop(in_ts[length(in_ts), ])
}  # end last.xts_xtra
last(new_xts)  # apply "last" from "xts_xtra" class
# define "last" method for class "xts_xtra"
last.xts_xtra <- function(in_ts) {
  cat("last element of xts_xtra object:\n")
  drop(NextMethod())
}  # end last.xts_xtra
last(new_xts)  # apply "last" from "xts_xtra" class
## library(parallel)  # load package parallel
## # get short description
## packageDescription("parallel")
## # load help page
## help(package="parallel")
## # list all objects in "parallel"
## ls("package:parallel")
## load(file="C:/Develop/data/etf_data.RData")
## # calculate number of available cores
## no_cores <- detectCores() - 1
## # initialize compute cluster
## clus_ters <- makeCluster(no_cores)
## # define function that pauses execution
## paws <- function(x, sleep_time) {
##   Sys.sleep(sleep_time)
##   x
## }  # end paws
## library(microbenchmark)  # load package microbenchmark
## # compare speed of lapply with parallel computing
## summary(microbenchmark(
##   l_apply=lapply(1:10, paws, sleep_time=0.01),
##   parl_apply=
##     parLapply(clus_ters, 1:10, paws, sleep_time=0.01),
##   times=10)
## )[, c(1, 4, 5)]
## # stop R processes over cluster
## stopCluster(clus_ters)
## library(parallel)  # load package parallel
## # calculate number of available cores
## no_cores <- detectCores() - 1
## # initialize compute cluster
## clus_ters <- makeCluster(no_cores)
## # define function that pauses execution
## paws <- function(x, sleep_time) {
##   Sys.sleep(sleep_time)
##   x
## }  # end paws
## # compare speed of lapply with parallel computing
## iter_ations <- 3:10
## compute_times <- sapply(iter_ations,
##   function(max_iterations, sleep_time) {
##     out_put <- summary(microbenchmark(
## lapply=lapply(1:max_iterations, paws,
##               sleep_time=sleep_time),
## parallel=parLapply(clus_ters, 1:max_iterations,
##         paws, sleep_time=sleep_time),
## times=10))[, c(1, 4)]
##     structure(out_put[, 2],
##         names=as.vector(out_put[, 1]))
##     }, sleep_time=0.01)
## compute_times <- t(compute_times)
## rownames(compute_times) <- iter_ations
## library(parallel)  # load package parallel
## plot(x=rownames(compute_times),
##      y=compute_times[, "lapply"],
##      type="l", lwd=2, col="blue",
##      main="Compute times",
##      xlab="number of iterations in loop", ylab="",
##      ylim=c(0, max(compute_times[, "lapply"])))
## lines(x=rownames(compute_times),
## y=compute_times[, "parallel"], lwd=2, col="green")
## legend(x="topleft", legend=colnames(compute_times),
##  inset=0.1, cex=1.0, bg="white",
##  lwd=2, lty=c(1, 1), col=c("blue", "green"))
## library(parallel)  # load package parallel
## # calculate number of available cores
## no_cores <- detectCores() - 1
## # initialize compute cluster
## clus_ters <- makeCluster(no_cores)
## # define large matrix
## mat_rix <- matrix(rnorm(7*10^5), ncol=7)
## # define aggregation function over column of matrix
## agg_regate <- function(col_umn) {
##   out_put <- 0
##   for (in_dex in 1:length(col_umn))
##     out_put <- out_put + col_umn[in_dex]
##   out_put
## }  # end agg_regate
## # perform parallel aggregations over columns of matrix
## agg_regations <-
##   parCapply(clus_ters, mat_rix, agg_regate)
## # compare speed of apply with parallel computing
## summary(microbenchmark(
##   ap_ply=apply(mat_rix, MARGIN=2, agg_regate),
##   parl_apply=
##     parCapply(clus_ters, mat_rix, agg_regate),
##   times=10)
## )[, c(1, 4, 5)]
## # stop R processes over cluster
## stopCluster(clus_ters)
