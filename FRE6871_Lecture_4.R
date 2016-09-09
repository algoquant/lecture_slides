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
## library(microbenchmark)
## # multiplication fails because dimensions aren't conformable
## vec_tor1 %*% mat_rix
## # works after transpose
## drop(vec_tor1 %*% t(mat_rix))
## # calculate inner product
## crossprod(vec_tor1, vec_tor2)
## # create matrix and vector
## mat_rix <- matrix(1:3000, ncol=3)
## tmat_rix <- t(mat_rix)
## vec_tor <- 1:3
## # crossprod is slightly faster than "%*%" operator
## summary(microbenchmark(
##   cross_prod=crossprod(tmat_rix, vec_tor),
##   inner_prod=mat_rix %*% vec_tor,
##   times=10))[, c(1, 4, 5)]  # end microbenchmark summary
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
rm(list=ls())
# get base environment
baseenv()
# get global environment
globalenv()
# get current environment
environment()
# get environment class
class(environment())
# define variable in current environment
glob_var <- 1
# get objects in current environment
ls(environment())
# create new environment
new_env <- new.env()
# get calling environment of new environment
parent.env(new_env)
# assign Value to Name
assign("new_var1", 3, envir=new_env)
# create object in new environment
new_env$new_var2 <- 11
# get objects in new environment
ls(new_env)
# get objects in current environment
ls(environment())
# environments are subset like lists
new_env$new_var1
# environments are subset like lists
new_env[["new_var1"]]
search()  # get search path for R objects
my_list <- 
  list(flowers=c("rose", "daisy", "tulip"), 
       trees=c("pine", "oak", "maple"))
my_list$trees
attach(my_list)
trees
search()  # get search path for R objects
detach(my_list)
head(trees)  # "trees" is in datasets base package
# "trees" is in datasets base package
head(trees, 3)
colnames(trees)
mean(Girth)
mean(trees$Girth)
with(trees, 
     c(mean(Girth), mean(Height), mean(Volume)))
num_list <- list(1, 2, 3, 4)  # create numeric list
do.call(rbind, num_list)  # returns single column matrix
do.call(cbind, num_list)  # returns single row matrix
# recycling rule applied
do.call(cbind, list(1:2, 3:5))
# NULL element is skipped
do.call(cbind, list(1, NULL, 3, 4))
# NA element isn't skipped
do.call(cbind, list(1, NA, 3, 4))
list_vectors <- lapply(1:5, rnorm, n=10)
mat_rix <- do.call(rbind, list_vectors)
dim(mat_rix)
do_call_rbind <- function(list_var) {
  while (length(list_var) > 1) {
# index of odd list elements
    odd_index <- seq(from=1, to=length(list_var), by=2)
# bind neighboring elements and divide list_var by half
    list_var <- lapply(odd_index, function(in_dex) {
      if (in_dex==length(list_var)) {
return(list_var[[in_dex]])
      }
      return(rbind(list_var[[in_dex]], 
           list_var[[in_dex+1]]))
    })  # end lapply
  }  # end while
# list_var has only one element - return it
  list_var[[1]]
}  # end do_call_rbind
identical(mat_rix, do_call_rbind(list_vectors))
## library(microbenchmark)
## airquality[(airquality$Solar.R>320 &
##         !is.na(airquality$Solar.R)), ]
## subset(x=airquality, subset=(Solar.R>320))
## summary(microbenchmark(
##     subset=subset(x=airquality, subset=(Solar.R>320)),
##     brackets=airquality[(airquality$Solar.R>320 &
##             !is.na(airquality$Solar.R)), ],
## times=10))[, c(1, 4, 5)]  # end microbenchmark summary
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
with(mtcars, tapply(X=mpg, INDEX=cyl, FUN=mean))
# function by() instead of tapply()
with(mtcars, by(data=mpg, INDICES=cyl, FUN=mean))
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
## load(file="C:/Develop/data/etf_data.RData")
## foo <- 0.3/3
## foo  # printed as "0.1"
## foo - 0.1  # foo is not equal to "0.1"
## foo == 0.1  # foo is not equal to "0.1"
## print(foo, digits=10)
## print(foo, digits=16)
## # foo is equal to "0.1" within machine precision
## all.equal(foo, 0.1)
## foo <- (3-2.9)
## print(foo, digits=20)
## # info machine precision of computer R is running on
## # ?.Machine
## # machine precision
## .Machine$double.eps
## load(file="C:/Develop/data/etf_data.RData")
## foo <- sqrt(2)
## foo^2  # printed as "2"
## foo^2 == 2  # foo^2 is not equal to "2"
## print(foo^2, digits=20)
## # foo^2 is equal to "2" within machine precision
## all.equal(foo^2, 2)
## # numbers with precision 0.1
## 0.1*(1:10)
## # round to precision 0.1
## round(3.675, 1)
## # round to precision 1.0
## round(3.675)
## # round to nearest even number
## c(round(2.5), round(3.5), round(4.5))
## round(4:20/2)  # round to nearest even number
## trunc(3.675)  # truncate
## load(file="C:/Develop/data/etf_data.RData")
## 4.7 %/% 0.5  # modulo division
## 4.7 %% 0.5  # remainder of modulo division
## # reversing modulo division usually
## # returns the original number
## (4.7 %% 0.5) + 0.5 * (4.7 %/% 0.5)
## # modulo division of non-integer numbers can
## # produce incorrect results
## 0.6 %/% 0.2  # produces 2 instead of 3
## 6 %/% 2  # use integers to get correct result
## # 0.2 stored as binary number
## # slightly larger than 0.2
## print(0.2, digits=22)
## # get size of an object
## object.size(runif(1e6))
## format(object.size(runif(1e6)), units="MB")
## load(file="C:/Develop/data/etf_data.RData")
## # get sizes of objects in workspace
## sort(sapply(ls(),
##   function(ob_ject) {
##     format(object.size(get(ob_ject)), units="KB")}))
## # get sizes of objects in workspace
## sort(sapply(mget(ls()), object.size))
## sort(sapply(mget(ls()),
## function(ob_ject) {
##   format(object.size(ob_ject), units="KB")}
## ))
## # get sizes of objects in env_data environment
## sort(sapply(ls(env_data),
##   function(ob_ject) {
##     object.size(get(ob_ject, env_data))}))
## # get sizes of objects in env_data environment
## sort(sapply(mget(ls(env_data), env_data),
##       object.size))
## # get total size of all objects in workspace
## format(object.size(x=mget(ls())), units="MB")
## library(gdata)  # load package gdata
## # get names, class, and size of objects in workspace
## ob_jects <- ll(unit="bytes")
## # sort by memory size (descending)
## ob_jects[order(ob_jects[, 2], decreasing=TRUE), ]
## ll()[order(ll()$KB, decreasing=TRUE), ]
## # get sizes of objects in env_data environment
## ll(unit="bytes", env_data)
## library(microbenchmark)
## foo <- runif(1e6)
## system.time(foo^0.5)
## microbenchmark(sqrt(foo), foo^0.5, times=10)
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
## library(microbenchmark)
## mat_rix <- matrix(1:9, ncol=3, # create matrix
##   dimnames=list(paste0("row", 1:3),
##           paste0("col", 1:3)))
## # create specialized function
## matrix_to_dframe <- function(mat_rix) {
##   n_col <- ncol(mat_rix)
##   dframe <- vector("list", n_col)  # empty vector
##   for(in_dex in 1:n_col)  # populate vector
##     dframe <- mat_rix[, in_dex]
##   attr(dframe, "row.names") <-  # add attributes
##     .set_row_names(nrow(mat_rix))
##   attr(dframe, "class") <- "data.frame"
##   dframe  # return data frame
## }  # end matrix_to_dframe
## # compare speed of three methods
## summary(microbenchmark(
##   matrix_to_dframe(mat_rix),
##   as.data.frame.matrix(mat_rix),
##   as.data.frame(mat_rix),
##   times=10))[, c(1, 4, 5)]
# matrix with 5,000 rows
big_matrix <- matrix(rnorm(10000), ncol=2)
# allocate memory for row sums
row_sums <- numeric(nrow(big_matrix))
summary(microbenchmark(
  ap_ply=apply(big_matrix, 1, sum),  # end apply
  l_apply=lapply(1:nrow(big_matrix), function(in_dex)
    sum(big_matrix[in_dex, ])),  # end lapply
  v_apply=vapply(1:nrow(big_matrix), function(in_dex)
    sum(big_matrix[in_dex, ]),
    FUN.VALUE=c(sum=0)),  # end vapply
  s_apply=sapply(1:nrow(big_matrix), function(in_dex)
    sum(big_matrix[in_dex, ])),  # end sapply
  for_loop=for(i in 1:nrow(big_matrix)) {
    row_sums[i] <- sum(big_matrix[i,])
  },  # end for
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
## big_vector <- rnorm(5000)
## summary(microbenchmark(
## # allocate full memory for cumulative sum
##   for_loop={cum_sum <- numeric(length(big_vector))
##     cum_sum[1] <- big_vector[1]
##     for(i in 2:length(big_vector)) {
##       cum_sum[i] <- cum_sum[i-1] + big_vector[i]
##     }},  # end for
## # allocate zero memory for cumulative sum
##   grow_vec={cum_sum <- numeric(0)
##     cum_sum[1] <- big_vector[1]
##     for(i in 2:length(big_vector)) {
## # add new element to "cum_sum" ("grow" it)
##       cum_sum[i] <- cum_sum[i-1] + big_vector[i]
##     }},  # end for
## # allocate zero memory for cumulative sum
##   com_bine={cum_sum <- numeric(0)
##     cum_sum[1] <- big_vector[1]
##     for(i in 2:length(big_vector)) {
## # add new element to "cum_sum" ("grow" it)
##       cum_sum <- c(cum_sum, big_vector[i])
##     }},  # end for
##   times=10))[, c(1, 4, 5)]
## library(microbenchmark)
## foo <- runif(1e6)
## system.time(foo^0.5)
## summary(
##   microbenchmark(sqrt(foo), foo^0.5, times=10)
##   )[, c(1, 4, 5)]
