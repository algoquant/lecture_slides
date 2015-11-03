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
my_var <- 3  # "<-" and "=" are valid assignment operators

my_var  # typing a symbol or expression evaluates it

my_var <- "Hello World!"  # text in quotes is interpreted as a string

my_var  # typing a symbol or expression evaluates it
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

# number of items printed to console
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
"Hello World!"  # type some text
# hello is a variable name, because it's not in quotes
hello  # R interprets "hello" as a variable name
is.vector(1)  # single number is a vector
is.vector("a")  # string is a vector
4:8  # create a vector
c(1, 2, 3, 4, 5)  # create vector using c() combine function
c('a', 'b', 'c')  # create vector using c() combine function
c(1, 'b', 'c')  # create vector using c() combine function
str_var <- "Some string"
str_var
str_var[1]
str_var[2]

length(str_var)  # length of vector
nchar(str_var)  # length of string

cat("Hello", "World!")  # concatenate and echo to console
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
mat_rix <- matrix(5:10, nrow=2, ncol=3)  # create a matrix
mat_rix  # by default matrices are constructed column-wise
# create a matrix row-wise
matrix(5:10, nrow=2, byrow=TRUE)
mat_rix[2, 3]  # extract third element from second row
mat_rix[2, ]  # extract second row
mat_rix[, 3]  # extract third column
mat_rix[, c(1,3)]  # extract first and third column
mat_rix[, -2]  # remove second column
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
sample(x=5)  # permutation of five numbers
sample(x=5, size=3)  # sample of size three
sample(x=5, replace=TRUE)  # sample with replacement
sample(  # sample of strings
  x=c("apple", "grape", "orange", "peach"),
  size=12,
  replace=TRUE)
# binomial sample: flip unbiased coin once, 20 times
sample(x=0:1, size=20, replace=TRUE)
# flip unbiased coin once, 20 times
as.numeric(runif(20) < 0.5)  # slower
rm(list=ls())
set.seed(1121)  # reset random number generator
# sample from Standard Normal Distribution
rand_sample <- rnorm(1000)

mean(rand_sample)  # sample mean

median(rand_sample)  # sample median

sd(rand_sample)  # sample standard deviation
rm(list=ls())
ts_rets <- diff(log(EuStockMarkets[, 1]))  # DAX returns
len_rets <- length(ts_rets)  # number of observations
mean_rets <- mean(ts_rets)  # calculate mean
sd_rets <- sd(ts_rets)  # calculate standard deviation
# calculate skew
len_rets*(sum(((ts_rets - mean_rets)/sd_rets)^3))/
  ((len_rets-1)*(len_rets-2))
# calculate kurtosis
len_rets*(len_rets+1)*(sum(((ts_rets - mean_rets)/sd_rets)^4))/
  ((len_rets-1)^3)
ts_rets <- rnorm(len_rets, sd=2)  # random normal returns
mean_rets <- mean(ts_rets); sd_rets <- sd(ts_rets)
# calculate skew
len_rets*(sum(((ts_rets - mean_rets)/sd_rets)^3))/
  ((len_rets-1)*(len_rets-2))
# calculate kurtosis
len_rets*(len_rets+1)*(sum(((ts_rets - mean_rets)/sd_rets)^4))/
  ((len_rets-1)^3)
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
