library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
options(digits=3)
thm <- knit_theme$get("acid")
knit_theme$set(thm)
vec_tor <- sample(1:9)
vec_tor
vec_tor < 5  # element-wise comparison
vec_tor == 5  # element-wise comparison
mat_rix <- matrix(vec_tor, ncol=3)
mat_rix
mat_rix < 5  # element-wise comparison
mat_rix == 5  # element-wise comparison
mat_rix <- 1:6  # create a vector
class(mat_rix)  # get its class
# is it vector or matrix?
c(is.vector(mat_rix), is.matrix(mat_rix))
structure(mat_rix, dim=c(2, 3))  # matrix object
# adding dimension attribute coerces into matrix
dim(mat_rix) <- c(2, 3)
class(mat_rix)  # get its class
# is it vector or matrix?
c(is.vector(mat_rix), is.matrix(mat_rix))
# assign dimnames attribute
dimnames(mat_rix) <- list(rows=c("row1", "row2"),
                  columns=c("col1", "col2", "col3"))
mat_rix
mat_rix <- matrix(1:10, 2, 5)  # create matrix
mat_rix
# as.numeric strips dim attribute from matrix
as.numeric(mat_rix)
mat_rix <- as.character(mat_rix)  # explicitly coerce to "character"
c(typeof(mat_rix), mode(mat_rix), class(mat_rix))
# coercion converted matrix to vector
c(is.matrix(mat_rix), is.vector(mat_rix))
vec_tor1 <- 1:3  # define vector
vec_tor2 <- 6:4  # define vector
cbind(vec_tor1, vec_tor2)  # bind into columns
rbind(vec_tor1, vec_tor2)  # bind into rows
vec_tor2 <- c(vec_tor2, 7)  # extend to four elements
cbind(vec_tor1, vec_tor2)  # recycling rule applied
1:6 + c(10, 20)  # another example of recycling rule
# replicate a single element
rep("a", 5)
# replicate the whole vector several times
rep(c("a", "b"), 5)
rep(c("a", "b"), times=5)
# replicate the first element, then the second, etc.
rep(c("a", "b"), each=5)
# replicate to specified length
rep(c("a", "b"), length.out=5)
# define vector and matrix
vec_tor1 <- c(2, 4, 3)
mat_rix <- matrix(sample(1:12), ncol=3)
# multiply matrix by vector column-wise
vec_tor1 * mat_rix
# multiply matrix by vector row-wise
t(vec_tor1 * t(mat_rix))
mat_rix <- matrix(1:6, ncol=3)  # create matrix
vec_tor1
vec_tor2 <- 6:4  # define vector
# multiply two vectors element-by-element
vec_tor1 * vec_tor2
# calculate inner product
vec_tor1 %*% vec_tor2
# calculate inner product and drop dimensions
drop(vec_tor1 %*% vec_tor2)
mat_rix
# multiply vector by matrix
mat_rix %*% vec_tor1  # single column matrix
drop(mat_rix %*% vec_tor1)  # vector
library(microbenchmark)
# multiply matrix by vector fails because dimensions aren't conformable
vec_tor1 %*% mat_rix
# works after transpose
drop(vec_tor1 %*% t(mat_rix))
# calculate inner product
crossprod(vec_tor1, vec_tor2)
# create matrix and vector
mat_rix <- matrix(1:3000, ncol=3)
tmat_rix <- t(mat_rix)
vec_tor <- 1:3
# crossprod is slightly faster than "%*%" operator
summary(microbenchmark(
  cross_prod=crossprod(tmat_rix, vec_tor),
  inner_prod=mat_rix %*% vec_tor,
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# define named vectors
vec_tor1 <- sample(1:4)
names(vec_tor1) <- paste0("row", 1:4, "=", vec_tor1)
vec_tor1
vec_tor2 <- sample(1:3)
names(vec_tor2) <- paste0("col", 1:3, "=", vec_tor2)
vec_tor2
# calculate outer product of two vectors
mat_rix <- outer(vec_tor1, vec_tor2)
mat_rix
# calculate vectorized function spanned over two vectors
mat_rix <- outer(vec_tor1, vec_tor2,
           FUN=function(x1, x2) x2*sin(x1))
mat_rix
# create list of vectors
li_st <- lapply(1:3, function(x) sample(6))
# bind list elements into matrix - doesn't work
rbind(li_st)
# bind list elements into matrix - tedious
rbind(li_st[[1]], li_st[[2]], li_st[[3]])
# bind list elements into matrix - works!
do.call(rbind, li_st)
# create numeric list
li_st <- list(1, 2, 3, 4)
do.call(rbind, li_st)  # returns single column matrix
do.call(cbind, li_st)  # returns single row matrix
# recycling rule applied
do.call(cbind, list(1:2, 3:5))
# NULL element is skipped
do.call(cbind, list(1, NULL, 3, 4))
# NA element isn't skipped
do.call(cbind, list(1, NA, 3, 4))
list_vectors <- lapply(1:5, rnorm, n=10)
mat_rix <- do.call(rbind, list_vectors)
dim(mat_rix)
do_call_rbind <- function(li_st) {
  while (length(li_st) > 1) {
# index of odd list elements
    odd_index <- seq(from=1, to=length(li_st), by=2)
# bind neighboring elements and divide li_st by half
    li_st <- lapply(odd_index, function(in_dex) {
      if (in_dex==length(li_st)) {
return(li_st[[in_dex]])
      }
      return(rbind(li_st[[in_dex]],
           li_st[[in_dex+1]]))
    })  # end lapply
  }  # end while
# li_st has only one element - return it
  li_st[[1]]
}  # end do_call_rbind
identical(mat_rix, do_call_rbind(list_vectors))
library(microbenchmark)
airquality[(airquality$Solar.R>320 &
        !is.na(airquality$Solar.R)), ]
subset(x=airquality, subset=(Solar.R>320))
summary(microbenchmark(
    subset=subset(x=airquality, subset=(Solar.R>320)),
    brackets=airquality[(airquality$Solar.R>320 &
            !is.na(airquality$Solar.R)), ],
times=10))[, c(1, 4, 5)]  # end microbenchmark summary
unique(iris$Species)  # Species has three distinct values
# split into separate data frames by hand
set_osa <- iris[iris$Species=="setosa", ]
versi_color <- iris[iris$Species=="versicolor", ]
virgin_ica <- iris[iris$Species=="virginica", ]
dim(set_osa)
head(set_osa, 2)
# split iris into list based on Species
split_iris <- split(iris, iris$Species)
str(split_iris, max.level=1)
names(split_iris)
dim(split_iris$setosa)
head(split_iris$setosa, 2)
unique(mtcars$cyl)  # cyl has three unique values
# split mtcars data frame based on number of cylinders
split_cars <- split(mtcars, mtcars$cyl)
str(split_cars, max.level=1)
names(split_cars)
# mean mpg for each cylinder group
unlist(lapply(split_cars, function(x) mean(x$mpg)))
# function aggregate() performs split-apply-combine
aggregate(formula=(mpg ~ cyl), data=mtcars, FUN=mean)
# aggregate() all columns
aggregate(x=mtcars, by=list(cyl=mtcars$cyl), FUN=mean)
# mean mpg for each cylinder group
tapply(X=mtcars$mpg, INDEX=mtcars$cyl, FUN=mean)
# using with() environment
with(mtcars,
     tapply(X=mpg, INDEX=cyl, FUN=mean))
# function sapply() instead of tapply()
with(mtcars,
     sapply(sort(unique(cyl)), function(x) {
       structure(mean(mpg[x==cyl]), names=x)
       }, USE.NAMES=TRUE))  # end with

# function by() instead of tapply()
with(mtcars,
     by(data=mpg, INDICES=cyl, FUN=mean))
# get several mpg stats for each cylinder group
data_cars <- sapply(split_cars,
      function(x) {
        c(mean=mean(x$mpg), max=max(x$mpg), min=min(x$mpg))
      }  # end anonymous function
      )  # end sapply
data_cars  # sapply produces a matrix
data_cars <- lapply(split_cars,  # now same using lapply
      function(x) {
        c(mean=mean(x$mpg), max=max(x$mpg), min=min(x$mpg))
      }  # end anonymous function
      )  # end sapply
is.list(data_cars)  # lapply produces a list
# do.call flattens list into a matrix
do.call(cbind, data_cars)
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
  ap_ply=apply(big_matrix, 1, sum),  # end apply
  l_apply=lapply(1:NROW(big_matrix), function(in_dex)
    sum(big_matrix[in_dex, ])),  # end lapply
  v_apply=vapply(1:NROW(big_matrix), function(in_dex)
    sum(big_matrix[in_dex, ]),
    FUN.VALUE=c(sum=0)),  # end vapply
  s_apply=sapply(1:NROW(big_matrix), function(in_dex)
    sum(big_matrix[in_dex, ])),  # end sapply
  for_loop=for(i in 1:NROW(big_matrix)) {
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
load(file="C:/Develop/data/etf_data.RData")
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
load(file="C:/Develop/data/etf_data.RData")
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
load(file="C:/Develop/data/etf_data.RData")
# rnorm() vectorized with respect to "std_dev"
vec_rnorm <- function(n, mean=0, sd=1) {
  if (length(sd)==1)
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
load(file="C:/Develop/data/etf_data.RData")
str(sum)
# na.rm is bound by name
mapply(sum, 6:9, c(5, NA, 3), 2:6, na.rm=TRUE)
str(rnorm)
# mapply vectorizes both arguments "mean" and "sd"
mapply(rnorm, n=5, mean=me_ans, sd=std_devs)
mapply(function(in_put, e_xp) in_put^e_xp,
 1:5, seq(from=1, by=0.2, length.out=5))
load(file="C:/Develop/data/etf_data.RData")
# rnorm() vectorized with respect to "mean" and "sd"
vec_rnorm <- function(n, mean=0, sd=1) {
  if (length(mean)==1 && length(sd)==1)
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
sample_length <- 1000
sam_ple <- rnorm(sample_length)
# sample mean - MC estimate
mean(sam_ple)
# sample standard deviation - MC estimate
sd(sam_ple)
# MC estimate of cumulative probability
sam_ple <- sort(sam_ple)
pnorm(1)
sum(sam_ple<1)/sample_length
# MC estimate of quantile
qnorm(0.75)
sam_ple[0.75*sample_length]
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
ts_var <- ts(data=pa_th, frequency=365, start=c(2011, 1))
plot(ts_var, type="l", col="black",  # create plot
     lty="solid", xlab="", ylab="")
abline(h=lev_el, lwd=2, col="red")  # add horizontal line
title(main="Brownian motion crossing a barrier level",
      line=0.5)
par(oma=c(1, 1, 1, 1), mgp=c(2, 1, 0), mar=c(5, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
set.seed(1121)  # reset random number generator
lev_el <- 20  # barrier level
len_gth <- 1000  # number of simulation steps
# simulate path of Brownian motion
pa_th <- cumsum(rnorm(len_gth))
# find index when pa_th crosses lev_el
cro_ss <- which(pa_th > lev_el)
# fill remaining pa_th after it crosses lev_el
if (length(cro_ss)>0) {
  pa_th[(cro_ss[1]+1):len_gth] <-
    pa_th[cro_ss[1]]
}  # end if
# create daily time series starting 2011
ts_var <- ts(data=pa_th, frequency=365,
     start=c(2011, 1))
# create plot with horizontal line
plot(ts_var, type="l", col="black",
     lty="solid", xlab="", ylab="")
abline(h=lev_el, lwd=2, col="red")
title(main="Brownian motion crossing
  a barrier level", line=0.5)
set.seed(1121)  # reset random number generator
# sample from Standard Normal Distribution
sample_length <- 1000
sam_ple <- rnorm(sample_length)
# sample mean
mean(sam_ple)
# sample standard deviation
sd(sam_ple)
# bootstrap of sample mean and median
boot_strap <- sapply(1:10000, function(x) {
  boot_sample <-
    sam_ple[sample.int(sample_length,
                 replace=TRUE)]
  c(mean=mean(boot_sample),
    median=median(boot_sample))
})  # end sapply
boot_strap[, 1:3]
# standard error from formula
sd(sam_ple)/sqrt(sample_length)
# standard error of mean from bootstrap
sd(boot_strap["mean", ])
# standard error of median from bootstrap
sd(boot_strap["median", ])
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
