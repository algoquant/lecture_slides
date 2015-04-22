library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
thm <- knit_theme$get("acid")
knit_theme$set(thm)
## # display documentation on function "getwd"
## help(getwd)
## ?getwd  # equivalent to "help(getwd)"
## help.start()  # open the hypertext documentation
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
loaded <- load(file="C:/Develop/data/my_data.RData")
loaded
ls()  # list objects
##   q()  # quit R session
## history(5)  # display last 5 commands
## savehistory(file="myfile")  # default is ".Rhistory"
## loadhistory(file="myfile")  # default is ".Rhistory"
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
"Hello World!"  # type some text
# R thinks 'hello' is a variable name, because it's not in quotes
hello
4:8  # create a vector
2*4:8  # create a vector
2*(4:8)  # create a vector
4:8/2  # create a vector
round(4:8/2)  # create a vector
trunc(4:8/2)  # create a vector
c(1, 2, 3, 4, 5)  # create vector using c() combine function
c('a', 'b', 'c')  # create vector using c() combine function
c(1, 'b', 'c')  # create vector using c() combine function
str_var <- "Some string"
str_var
str_var[1]
str_var[2]

length(str_var)  # length of vector
nchar(str_var)  # length of string

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
substring("Hello World", 3, 6)  # extract characters from 3 to 6
gsub("is", "XX", "is this gratis?")  # replace "is" with "XX"

grep("b+", c("abc", "xyz", "cba d", "bbb"))  # get indexes

grep("b+", c("abc", "xyz", "cba d", "bbb"), value=TRUE)  # get values

glob2rx("abc.*")  # convert globs into regex
glob2rx("*.doc")
rm(list=ls())
TRUE | FALSE
TRUE | NA
vec_var1 <- c(2, 4, 6)
vec_var1 < 5
(vec_var1 < 5) & (vec_var1 > 3)
vec_var1[(vec_var1 < 5) & (vec_var1 > 3)]
vec_var2 <- c(-10, 0, 10)
vec_var1 < vec_var2
c(FALSE, TRUE, FALSE) & c(TRUE, TRUE, FALSE)
c(FALSE, TRUE, FALSE) | c(TRUE, TRUE, FALSE)
vec_var <- c(pi, exp(1), -digamma(1))  # define a vector
vec_var

vec_var[2]  # extract second element
vec_var[-2]  # extract all elements, except the second element

vec_var[c(FALSE, TRUE, TRUE)]  # extract second and third elements
vec_var == vec_var[2]  # logical vector of elements equal to the second one
vec_var[vec_var == vec_var[2]]  # extract all elements equal to the second one

vec_var < 1  # logical vector of elements less than one
vec_var[vec_var > 1]  # extract all elements greater than one

2*vec_var  # multiply all elements by 2
vec_var^2  # square all elements
0:10  # vector of integers from 0 to 10
(0:10)/10  # vector of decimals from 0 to 1.0
vector()  # create empty vector
vector(mode="numeric", length=10)  # create numeric vector of zeros
seq(10)  # sequence from 1 to 10
seq(along=(-5:5))  # instead of 1:length(obj)
seq(from=0, to=1, len=11)  # vector of decimals from 0 to 1.0
seq(from=0, to=1, by=0.1)  # vector of decimals from 0 to 1.0
seq(-2,2, len=11)  # 10 evenly distributed numbers from -2 to 2
rep(100, times=5)  # replicate a number
character(5)  # create empty character vector
numeric(5)  # create empty numeric vector
numeric(0)  # create zero length vector
vec_var <- c(pi=pi, euler=exp(1), gamma=-digamma(1))
vec_var
names(vec_var)  # get names of elements
vec_var['euler']  # get element named 'euler'
names(vec_var) <- c("pie","eulery","gammy")  # rename elements
vec_var
unname(vec_var)  # remove names attribute
c(11, 5:10)  # combine two vectors
c(vec_var, 2.0)  # append number to vector

letters[5:10]  # vector of letters
c('a', letters[5:10])  # combine two vectors of letters
vec_var  # named vector
vec_var[2]  # extract second element
vec_var[-2]  # extract all elements, except the second element
vec_var[c(FALSE, TRUE, TRUE)]  # extract second and third elements
vec_var["eulery"]  # extract elements using their names
vec_var[c("pie", "gammy")]  # extract elements using their names
vec_var <- runif(5)
vec_var

vec_var > 0.5  # logical vector

vec_var[vec_var > 0.5]  # filter elements > 0.5

which(vec_var > 0.5)  # index of elements > 0.5
set.seed(1121)  # initialize the random number generator
runif(3)  # three random numbers from the uniform distribution
runif(3)  # produce another three numbers
set.seed(1121)  # re-initialize the random number generator
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
rm(list=ls())
set.seed(1121)  # initialize the random number generator
# sample from Standard Normal Distribution
rand_sample <- rnorm(1000)

mean(rand_sample)  # sample mean

median(rand_sample)  # sample median

sd(rand_sample)  # sample standard deviation
