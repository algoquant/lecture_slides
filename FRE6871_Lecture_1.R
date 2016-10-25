library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
options(digits=3)
thm <- knit_theme$get("acid")
knit_theme$set(thm)
# display documentation on function "getwd"
help(getwd)
?getwd  # equivalent to "help(getwd)"
help.start()  # open the hypertext documentation
# "<-" and "=" are valid assignment operators
my_var <- 3

# typing a symbol or expression evaluates it
my_var

# text in quotes is interpreted as a string
my_var <- "Hello World!"

# typing a symbol or expression evaluates it
my_var

my_var  # text after hash is treated as comment
getwd()  # get cwd
setwd("C:/Develop/R")  # set cwd
getwd()  # get cwd
Sys.time()  # get date and time

Sys.Date()  # get date only
rm(list=ls())
setwd("C:/Develop/data")
var1 <- 3  # define new object
ls()  # list all objects in workspace
# list objects starting with "v"
ls(pattern=glob2rx("v*"))
save.image()  # save workspace to file .RData in cwd
rm(var1)  # remove object
ls()  # list objects
load(".RData")
ls()  # list objects
var2 <- 5  # define another object
save(var1, var2,  # save selected objects
     file="C:/Develop/data/my_data.RData")
rm(list=ls())  # remove all objects
ls()  # list objects
load_ed <- load(file="C:/Develop/data/my_data.RData")
load_ed
ls()  # list objects
  q()  # quit R session
history(5)  # display last 5 commands
savehistory(file="myfile")  # default is ".Rhistory"
loadhistory(file="myfile")  # default is ".Rhistory"
sessionInfo()  # get R version and other session info
Sys.getenv()[5:7]  # list some environment variables

Sys.getenv("Home")  # get R user HOME directory

Sys.setenv(Home="C:/Develop/data")  # set HOME directory

Sys.getenv("Home")  # get user HOME directory

Sys.getenv("R_home")  # get R_HOME directory

R.home()  # get R_HOME directory

R.home("etc")  # get "etc" sub-directory of R_HOME
# ?options  # long list of global options
# interpret strings as characters, not factors
getOption("stringsAsFactors")  # display option
options("stringsAsFactors")  # display option
options(stringsAsFactors=FALSE)  # set option
# number of digits printed for numeric values
options(digits=3)
# control exponential scientific notation of print method
# positive "scipen" values bias towards fixed notation
# negative "scipen" values bias towards scientific notation
options(scipen=100)
# maximum number of items printed to console
options(max.print=30)
# warning levels options
# negative - warnings are ignored
options(warn=-1)
# zero - warnings are stored and printed after top-level function has completed
options(warn=0)
# one - warnings are printed as they occur
options(warn=1)
# two or larger - warnings are turned into errors
options(warn=2)
# save all options in variable
op_tions <- options()
# restore all options from variable
options(op_tions)
# single numbers are vectors of length 1
1
# character strings are vectors of length 1
"a"
# strings without quotes are variable names
a  # variable "a" doesn't exist
# list elements can have different mode
list(aa=c('a', 'b'), bb=1:5)
data.frame(aa=c('a', 'b'), bb=1:2)
is.atomic(data.frame(aa=c('a', 'b'), bb=1:2))
is.recursive(data.frame(aa=c('a', 'b'), bb=1:2))
my_var <- "hello"
c(typeof(my_var), mode(my_var), class(my_var))

my_var <- 1:5
c(typeof(my_var), mode(my_var), class(my_var))

my_var <- runif(5)
c(typeof(my_var), mode(my_var), class(my_var))

my_var <- matrix(1:10, 2, 5)
c(typeof(my_var), mode(my_var), class(my_var))

my_var <- matrix(runif(10), 2, 5)
c(typeof(my_var), mode(my_var), class(my_var))

my_var <- list(aa=c('a', 'b'), bb=1:5)
c(typeof(my_var), mode(my_var), class(my_var))

my_var <- data.frame(aa=c('a', 'b'), bb=1:2)
c(typeof(my_var), mode(my_var), class(my_var))
# a simple vector has no attributes
attributes(5:10)
my_var <- c(pi=pi, euler=exp(1), gamma=-digamma(1))
# named vector has "names" attribute
attributes(my_var)
my_var <- 1:10
is.vector(my_var)  # is the object a vector?
attributes(my_var) <- list(my_attr="foo")
my_var
is.vector(my_var)  # is the object a vector?
my_var <- 0
attributes(my_var) <- list(class="Date")
my_var  # "Date" object
structure(0, class="Date")  # "Date" object
my_var <- matrix(runif(10), 2, 5)
class(my_var)  # has implicit class
# but no explicit "class" attribute
attributes(my_var)
c(typeof(my_var), mode(my_var), class(my_var))
# assign explicit "class" attribute
class(my_var) <- "my_class"
class(my_var)  # has explicit "class"
# has explicit "class" attribute
attributes(my_var)
is.matrix(my_var)  # is the object a matrix?
is.vector(my_var)  # is the object a vector?
attributes(unclass(my_var))
# integer implicit class derived from type
my_var <- vector(mode="integer", length=10)
c(typeof(my_var), mode(my_var), class(my_var))
# numeric implicit class derived from mode
my_var <- vector(mode="numeric", length=10)
c(typeof(my_var), mode(my_var), class(my_var))
# adding dim attribute changes implicit class to matrix
dim(my_var) <- c(5, 2)
c(typeof(my_var), mode(my_var), class(my_var))
# data frames have implicit dim attribute
my_var <- data.frame(aa=c('a', 'b'), bb=1:2)
c(typeof(my_var), mode(my_var), class(my_var))
attributes(my_var)
dim(my_var)
my_var <- 1:5
c(typeof(my_var), mode(my_var), class(my_var))
mode(my_var) <- "character"  # coerce to "character"
my_var
c(typeof(my_var), mode(my_var), class(my_var))
# explicitly coerce to "character"
my_var <- as.character(1:5)
c(typeof(my_var), mode(my_var), class(my_var))
mat_rix <- matrix(1:10, 2, 5)  # create matrix
# explicitly coerce to "character"
mat_rix <- as.character(mat_rix)
c(typeof(mat_rix), mode(mat_rix), class(mat_rix))
# coercion converted matrix to vector
c(is.matrix(mat_rix), is.vector(mat_rix))
as.logical(0:3)  # explicit coercion to "logical"
as.numeric(c(FALSE, TRUE, TRUE, TRUE))
c(1:3, 'a')  # implicit coercion to "character"
# explicit coercion to "numeric"
as.numeric(c(1:3, 'a'))
"Hello World!"  # type some text
# hello is a variable name, because it's not in quotes
hello  # R interprets "hello" as a variable name
is.vector(1)  # single number is a vector
is.vector("a")  # string is a vector
4:8  # create a vector
# create vector using c() combine function
c(1, 2, 3, 4, 5)
# create vector using c() combine function
c('a', 'b', 'c')
# create vector using c() combine function
c(1, 'b', 'c')
str_var <- "Some string"
str_var
str_var[1]
str_var[2]

length(str_var)  # length of vector
nchar(str_var)  # length of string

# concatenate and echo to console
cat("Hello", "World!")
cat("Enter\ttab")
cat("Enter\nnewline")
cat("Enter\\backslash")
str_var1 <- "Hello"  # define a character string
str_var2 <- "World!"  # define a character string
paste(str_var1, str_var2, sep=' ')  # concatenate and return value
cat(str_var1, str_var2)  # concatenate and echo to console
paste('a', 1:4, sep='-')  # convert, recycle and concatenate
paste(c("a1", "a2", "a3"), collapse="+")  # collapse vector to string
paste(list("a1", "a2", "a3"), collapse="+")
paste("Today is", Sys.time())  # coerce and concatenate strings
paste("Today is", format(Sys.time(), "%B-%d-%Y"))
strsplit("Hello World", split='r')  # split string
strsplit("Hello.World", split='[.]')  # split string
strsplit("Hello.World", split='.', fixed=TRUE)  # split string
substring("Hello World", 3, 6)  # extract characters from 3 to 6
gsub("is", "XX", "is this gratis?")  # replace "is" with "XX"

grep("b+", c("abc", "xyz", "cba d", "bbb"))  # get indexes

grep("b+", c("abc", "xyz", "cba d", "bbb"), value=TRUE)  # get values

glob2rx("abc.*")  # convert globs into regex
glob2rx("*.doc")
is.vector(1)  # single number is a vector
is.vector("a")  # string is a vector
vec_tor <- c(8, 6, 5, 7)  # create vector
vec_tor
vec_tor[2]  # extract second element
# extract all elements, except the second element
vec_tor[-2]
# create boolean vector
c(FALSE, TRUE, TRUE)
# extract second and third elements
vec_tor[c(FALSE, TRUE, TRUE)]
letters[5:10]  # vector of letters
c('a', letters[5:10])  # combine two vectors of letters
0:10  # vector of integers from 0 to 10
vector()  # create empty vector
vector(mode="numeric", length=10)  # numeric vector of zeros
seq(10)  # sequence from 1 to 10
seq(along=(-5:5))  # instead of 1:length(obj)
seq_along(c("a", "b", "c"))  # instead of 1:length(obj)
seq(from=0, to=1, len=11)  # decimals from 0 to 1.0
seq(from=0, to=1, by=0.1)  # decimals from 0 to 1.0
seq(-2,2, len=11)  # 10 numbers from -2 to 2
rep(100, times=5)  # replicate a number
character(5)  # create empty character vector
numeric(5)  # create empty numeric vector
numeric(0)  # create zero length vector
2*4:8  # multiply a vector
2*(4:8)  # multiply a vector
4:8/2  # divide a vector
(0:10)/10  # divide vector - decimals from 0 to 1.0
vec_tor <- c(8, 6, 5, 7)  # create vector
vec_tor
# boolean vector TRUE if element is equal to second one
vec_tor == vec_tor[2]
# boolean vector TRUE for elements greater than six
vec_tor > 6
2*vec_tor  # multiply all elements by 2
vec_tor^2  # square all elements
c(11, 5:10)  # combine two vectors
c(vec_tor, 2.0)  # append number to vector
vec_tor <- # create named vector
  c(pi_const=pi, euler=exp(1), gamma=-digamma(1))
vec_tor
names(vec_tor)  # get names of elements
vec_tor["euler"]  # get element named "euler"
names(vec_tor) <- c("pie","eulery","gammy")  # rename elements
vec_tor
unname(vec_tor)  # remove names attribute
letters[5:10]  # vector of letters
c('a', letters[5:10])  # combine two vectors of letters
# create named vector
structure(sample(1:5), names=paste0("el", 1:5))
vec_tor  # named vector
# extract second element
vec_tor[2]
# extract all elements, except the second element
vec_tor[-2]
# extract zero elements - returns zero-length vector
vec_tor[0]
# extract second and third elements
vec_tor[c(FALSE, TRUE, TRUE)]
# extract elements using their names
vec_tor["eulery"]
# extract elements using their names
vec_tor[c("pie", "gammy")]
vec_tor <- runif(5)
vec_tor
vec_tor > 0.5  # boolean vector
# boolean vector of elements equal to the second one
vec_tor == vec_tor[2]
# extract all elements equal to the second one
vec_tor[vec_tor == vec_tor[2]]
vec_tor < 1  # boolean vector of elements less than one
# extract all elements greater than one
vec_tor[vec_tor > 1]
vec_tor[vec_tor > 0.5]  # filter elements > 0.5
which(vec_tor > 0.5)  # index of elements > 0.5
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
foo <- sample(0:6) + 0.1
foo
cut(x=foo, breaks=c(2, 4, 6, 8))
rbind(foo, cut(x=foo, breaks=c(2, 4, 6, 8)))
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
mat_rix <- matrix(5:10, nrow=2, ncol=3)  # create a matrix
mat_rix  # by default matrices are constructed column-wise
# create a matrix row-wise
matrix(5:10, nrow=2, byrow=TRUE)
mat_rix[2, 3]  # extract third element from second row
mat_rix[2, ]  # extract second row
mat_rix[, 3]  # extract third column
mat_rix[, c(1,3)]  # extract first and third column
mat_rix[, -2]  # remove second column
# get the number of rows or columns
nrow(vec_tor); ncol(vec_tor)
NROW(vec_tor); NCOL(vec_tor)
nrow(mat_rix); ncol(mat_rix)
NROW(mat_rix); NCOL(mat_rix)
attributes(mat_rix)  # get matrix attributes
dim(mat_rix)  # get dimension attribute
class(mat_rix)  # get class attribute
rownames(mat_rix) <- c("row1", "row2")  # rownames attribute
colnames(mat_rix) <- c("col1", "col2", "col3")  # colnames attribute
mat_rix
mat_rix["row2", "col3"]  # third element from second row
names(mat_rix)  # get the names attribute
dimnames(mat_rix)  # get dimnames attribute
attributes(mat_rix)  # get matrix attributes
mat_rix  # matrix with column names
mat_rix[1, ]  # subset rows by index
mat_rix[, "col1"]  # subset columns by name
mat_rix[, c(TRUE, FALSE, TRUE)]  # subset columns boolean vector
mat_rix[1, ]  # subsetting can produce a vector!
class(mat_rix); class(mat_rix[1, ])
is.matrix(mat_rix[1, ]); is.vector(mat_rix[1, ])
mat_rix[1, , drop=FALSE]  # drop=FALSE preserves matrix
class(mat_rix[1, , drop=FALSE])
is.matrix(mat_rix[1, , drop=FALSE]); is.vector(mat_rix[1, , drop=FALSE])
set.seed(1121)  # reset random number generator
runif(3)  # three random numbers from the uniform distribution
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
ts_rets <- diff(log(EuStockMarkets[, 1]))
# number of observations
len_rets <- length(ts_rets)
# mean of DAX returns
mean_rets <- mean(ts_rets)
# standard deviation of DAX returns
sd_rets <- sd(ts_rets)
# skew of DAX returns
len_rets/((len_rets-1)*(len_rets-2))*
  sum(((ts_rets - mean_rets)/sd_rets)^3)
# kurtosis of DAX returns
len_rets*(len_rets+1)/((len_rets-1)^3)*
  sum(((ts_rets - mean_rets)/sd_rets)^4)
# random normal returns
ts_rets <- rnorm(len_rets, sd=2)
# mean and standard deviation of random normal returns
mean_rets <- mean(ts_rets)
sd_rets <- sd(ts_rets)
# skew of random normal returns
len_rets/((len_rets-1)*(len_rets-2))*
  sum(((ts_rets - mean_rets)/sd_rets)^3)
# kurtosis of random normal returns
len_rets*(len_rets+1)/((len_rets-1)^3)*
  sum(((ts_rets - mean_rets)/sd_rets)^4)
set.seed(1121)  # reset random number generator
# sample from Standard Normal Distribution
sample_length <- 1000
sam_ple <- rnorm(sample_length)
# sample mean
mean(sam_ple)
# sample standard deviation
sd(sam_ple)
# define a function with two arguments
test_func <- function(first_arg, second_arg) {  # body
  first_arg + second_arg  # returns last evaluated statement
}  # end test_func

test_func(1, 2)  # apply the function
args(test_func)  # display argument

# define function that uses variable from enclosure environment
test_func <- function(first_arg, second_arg) {
  first_arg + second_arg + glob_var
}  # end test_func

test_func(3, 2)  # error - glob_var doesn't exist yet!
glob_var <- 10  # create glob_var
test_func(3, 2)  # now works
test_func <- function(first_arg, second_arg) {
# last statement of function is return value
  first_arg + 2*second_arg
}  # end test_func
test_func(first_arg=3, second_arg=2)  # bind by name
test_func(first=3, second=2)  # partial name binding
test_func(3, 2)  # bind by position
test_func(second_arg=2, 3)  # mixed binding
test_func(3, 2, 1)  # too many arguments
test_func(2)  # not enough arguments
# function "paste" has two arguments with default values
str(paste)
# default values of arguments can be specified in argument list
test_func <- function(first_arg, fac_tor=1) {
  fac_tor*first_arg
}  # end test_func
test_func(3)  # default value used for second argument
test_func(3, 2)  # default value over-ridden
# default values can be a vector of strings
test_func <- function(in_put=c("first_val", "second_val")) {
  in_put <- match.arg(in_put)  # match to arg list
  in_put
}  # end test_func
test_func("second_val")
test_func("se")  # partial name binding
test_func("some_val")  # invalid string
# define function that returns NULL for non-numeric argument
test_func <- function(in_put) {
  if (!is.numeric(in_put)) {
    warning(paste("argument", in_put, "isn't numeric"))
    return(NULL)
  }
  2*in_put
}  # end test_func

test_func(2)
test_func("hello")
setwd("C:/Develop/data")
# define a function that returns invisibly
return_invisible <- function(in_put) {
  invisible(in_put)
}  # end return_invisible

return_invisible(2)

glob_var <- return_invisible(2)
glob_var

rm(list=ls())  # remove all objects
# load objects from file
loaded <- load(file="my_data.RData")
loaded  # vector of loaded objects
ls()  # list objects
