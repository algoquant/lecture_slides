library(knitr)
opts_chunk$set(prompt=TRUE, eval=FALSE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
options(digits=3)
thm <- knit_theme$get("acid")
knit_theme$set(thm)
rm(list=ls())
TRUE | FALSE
TRUE | NA
vec_tor1 <- c(2, 4, 6)
vec_tor1 < 5  # element-wise comparison
(vec_tor1 < 5) & (vec_tor1 > 3)
vec_tor1[(vec_tor1 < 5) & (vec_tor1 > 3)]
vec_tor2 <- c(-10, 0, 10)
vec_tor1 < vec_tor2
c(FALSE, TRUE, FALSE) & c(TRUE, TRUE, FALSE)
c(FALSE, TRUE, FALSE) | c(TRUE, TRUE, FALSE)
rm(list=ls())
c(FALSE, TRUE, FALSE) && c(TRUE, TRUE, FALSE)
c(FALSE, TRUE, FALSE) || c(TRUE, TRUE, FALSE)
echo_true <- function() {cat("echo_true\t"); TRUE}
echo_false <- function() {cat("echo_false\t"); FALSE}
echo_true() | echo_false()
echo_true() || echo_false()  # echo_false() isn't evaluated at all!
vec_tor <- c(2, 4, 6)
# works (does nothing) using '&&'
if (is.matrix(vec_tor) && (vec_tor[2, 3] > 0)) {
  vec_tor[2, 3] <- 1
}
# no short-circuit so fails (produces an error)
if (is.matrix(vec_tor) & (vec_tor[2, 3] > 0)) {
  vec_tor[2, 3] <- 1
}
num_var <- 2
num_var==2
identical(num_var, 2)

identical(num_var, NULL) 
num_var==NULL
is.null(num_var)

vec_tor <- c(2, 4, 6)
vec_tor==2
identical(vec_tor, 2)
vec_tor <- sample(1:9)
mat_rix <- matrix(vec_tor, ncol=3)
vec_tor
which(vec_tor == 5)
# equivalent but less efficient than above
(1:length(vec_tor))[vec_tor == 5]
which(vec_tor > 5)
# find indices of TRUE elements of boolean matrix
which((mat_rix == 5)|(mat_rix == 6), arr.ind=TRUE)
# equivalent but less efficient than above
arrayInd(which((mat_rix == 5)|(mat_rix == 6)), 
 dim(mat_rix), dimnames(mat_rix))
which.max(vec_tor)
# equivalent but less efficient than above
which(vec_tor == max(vec_tor))
which.min(vec_tor)
match(5, vec_tor)
# equivalent but less efficient than above
which(vec_tor == 5)
match(-5, vec_tor)
5 %in% vec_tor
# equivalent to above
match(5, vec_tor, nomatch=0) > 0
-5 %in% vec_tor
c(5, -5) %in% vec_tor
# equivalent to "5 %in% vec_tor"
any(vec_tor == 5)
# equivalent to "-5 %in% vec_tor"
any(vec_tor == (-5))
if(any(vec_tor < 0))
  cat("vector contains negative values\n")
num_var1 <- 3  # "<-" and "=" are valid assignment operators
num_var1
num_var1 = 3
num_var1
2<-3  # "<" operator confused with "<-"
2 < -3  # add space or brackets to avoid confusion
median(x=1:10)  # "=" assignment within argument list
x  # x doesn't exist outside the function
median(x <- 1:10)  # "<-" assignment within argument list
x  # x exists outside the function
my_var <- 1  # create new object
assign(x="my_var", value=2)  # assign value to existing object
my_var
rm(my_var)  # remove my_var
assign(x="my_var", value=3)  # create new object from name
my_var
# create new object in new environment
new_env <- new.env()  # create new environment
assign("my_var", 3, envir=new_env)  # assign value to name
ls(new_env)  # list objects in "new_env"
new_env$my_var
rm(list=ls())  # delete all objects
sym_bol <- "my_var"  # define symbol containing string "my_var"
assign(sym_bol, 1)  # assign value to "my_var"
ls()
my_var
assign("sym_bol", "new_var")
assign(sym_bol, 1)  # assign value to "new_var"
ls()
sym_bol <- 10
assign(sym_bol, 1)  # can't assign to non-string
rm(list=ls())  # delete all objects
# create individual vectors from column names of EuStockMarkets
for (col_name in colnames(EuStockMarkets)) {
# assign column values to column names
  assign(col_name, EuStockMarkets[, col_name])
}  # end for
ls()
head(DAX)
head(EuStockMarkets[, "DAX"])
identical(DAX, EuStockMarkets[, "DAX"])
# create new environment
test_env <- new.env()
# pass string as name to create new object
assign("my_var1", 2, envir=test_env)
# create new object using $ string referencing
test_env$my_var2 <- 1
# list objects in new environment
ls(test_env)
# reference an object by name
test_env$my_var1
# reference an object by string name using get
get("my_var1", envir=test_env)
# retrieve and assign value to object
assign("my_var1", 
 2*get("my_var1", envir=test_env), 
 envir=test_env)
get("my_var1", envir=test_env)
# return all objects in an environment
mget(ls(test_env), envir=test_env)
# delete environment
rm(test_env)
rm(list=ls())  # delete all objects
as.symbol("some_string")  # convert string to symbol
class(as.symbol("some_string"))  # name is synonymous with symbol
symbol_name <- 2  # symbols are created during assignments
eval(symbol_name)  # evaluate symbol (same as typing it)
eval(as.symbol("symbol_name"))  # convert string to symbol and evaluate it

# convert string to expression
ex_pression <- parse(text="symbol_new <- symbol_name")
ex_pression
class(ex_pression)
ls()
eval(ex_pression)  # evaluate expression
ls()  # expression evaluation created new object
symbol_new
quote(symbol_name + symbol_new)  # return expression without evaluating it
# substitute objects in an expression
ex_pression <- substitute(symbol_name + symbol_new, 
                env=list(symbol_name=1, symbol_new=2))
ex_pression
eval(ex_pression)  # evaluate expression
make_expression <- function(in_put) {
  substitute(symbol_name + in_put)
}  # end make_expression
make_expression(1)
my_var <- 2
make_expression(my_var)
# define symbol
my_var <- 10
# convert symbol value into string
deparse(my_var)
# convert symbol into string without evaluating it
deparse(quote(my_var))
# substitute object with value from named list
deparse(substitute(symbol_name + symbol_new, 
             env=list(symbol_new=2)))
# capture name of input argument
deparse_name <- function(in_put) {
  names(in_put) <- deparse(substitute(in_put))
  in_put
}  # end deparse_name
deparse_name(my_var)
my_var <- deparse_name(my_var)
my_var
rm(list=ls())
num_var1 <- 1

if (num_var1) {  # numeric zero is FALSE, all other numbers are TRUE
  num_var2 <- 4
} else if (num_var1 == 0) {  # 'else if' together on same line
  num_var2 <- 0
} else {  # 'else' together with curly braces
  num_var2 <- -4
}  # end if

num_var2
switch("a", a="aaahh", b="bee", c="see", d=2, "else this")
switch("c", a="aaahh", b="bee", c="see", d=2, "else this")
switch(3, a="aaahh", b="bee", c="see", d=2, "else this")
switch("cc", a="aaahh", b="bee", c="see", d=2, "else this")
# measure of central tendency
centra_lity <- function(in_put, 
    meth_od=c("mean", "mean_narm", "median")) {
# validate "meth_od" argument
  meth_od <- match.arg(meth_od)
  switch(meth_od,
 mean=mean(in_put),
 mean_narm=mean(in_put, na.rm=TRUE),
 median=median(in_put))
}  # end centra_lity
my_var <- rnorm(100, mean=2)
centra_lity(my_var, "mean")
centra_lity(my_var, "mean_narm")
centra_lity(my_var, "median")
rm(list=ls())
color_list <- list("red", "white", "blue")
for (some_color in color_list) {  # loop over list
  print(some_color)
}
for (in_dex in 1:3) {  # loop over vector
  print(color_list[[in_dex]])
}

in_dex <- 1  # while loops need initialization
while (in_dex < 4) {  # while loop
  print(color_list[[in_dex]])
  in_dex <- in_dex + 1
}
rm(list=ls())
# fib_seq <- numeric()  # zero length numeric vector
# pre-allocate vector instead of "growing" it
fib_seq <- numeric(10)
fib_seq[1] <- 0  # initialize
fib_seq[2] <- 1  # initialize
for (i in 3:10) {  # perform recurrence loop
  fib_seq[i] <- fib_seq[i-1] + fib_seq[i-2]
}  # end for
fib_seq
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
data(ChickWeight)
# ?ChickWeight

# get grouping variables
sapply(ChickWeight, function(x) length(unique(x)))

# mean weight of the chickens
tapply(ChickWeight$weight, ChickWeight$Diet, mean)

# access individual groups
# create list of my.splits corresponding to individual Diet values
my.splits = split(ChickWeight, ChickWeight$Diet)
length(my.splits)
names(my.splits)
head(my.splits[[1]])

# apply is passing each element to a function - performs for loop "under the hood"
# get all the chicks from each group that weigh less that 40 grams
my.results.lapply = lapply(my.splits, subset, weight <= 40)
# longer way with anonymous function:
my.results.lapply = lapply(my.splits, function(x) subset(x, weight <= 40) )

# fold back into a data frame
my.df = do.call(rbind, my.results.lapply)
my.df

# a different way:
sorted.chickens = ChickWeight[order(ChickWeight$Diet), ]
(sorted.chickens = subset(sorted.chickens, weight <= 40))

# same with for-loop:
my.results.for = list()
for (ii in 1:length(my.splits)) {
     my.results.for[[ii]] = subset(my.splits[[ii]], weight <= 40)
}
names(my.results.for) = names(my.splits)
all.equal(my.results.lapply, my.results.for) # Should be equal to my.results.lapply

# or in one line:
lapply(split(ChickWeight, ChickWeight$Diet), subset, weight <= 40)
# or
(do.call(rbind, lapply(split(ChickWeight, ChickWeight$Diet), subset, weight <= 40)))
library(plyr)
one <- ozone[1, 1, ]
month <- ordered(rep(1:12, length72))
model <- rlm(one ~ month - 1)
deseas <- resid(model)
deseasf <- function(value) rlm(value ~ month - 1)

# For loops
models <- as.list(rep(NA, 24 * 24))
dim(models) <- c(24, 24)
deseas <- array(NA, c(24, 24, 72))
dimnames(deseas) <- dimnames(ozone)
for (i in seq_len(24)) {
for(j in seq_len(24)) {
mod <- deseasf(ozone[i, j, ])
models[[i, j]] <- mod
deseas[i, j, ] <- resid(mod)
}
}

# apply functions
models <- apply(ozone, 1:2, deseasf)
resids_list <- lapply(models, resid)
resids <- unlist(resids_list)
dim(resids) <- c(72, 24, 24)
deseas <- aperm(resids, c(2, 3, 1))
dimnames(deseas) <- dimnames(ozone)

# InsectSprays dataset
head(InsectSprays)

# split the count column by the spray column.
count_by_spray <- with(InsectSprays, split(count, spray))

# next apply the statistic to each element of the list. Lets use the mean here.
mean_by_spray <- lapply(count_by_spray, mean)

# finally combine the list as a vector
unlist(mean_by_spray)

# or in one line
sapply(count_by_spray, mean)

# can also use the functions tapply(), aggregate() and by():
with(InsectSprays, tapply(count, spray, mean))
with(InsectSprays, by(count, spray, mean))
aggregate(count ~ spray, InsectSprays, mean)
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
