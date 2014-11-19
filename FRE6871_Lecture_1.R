

library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
thm <- knit_theme$get("acid")
knit_theme$set(thm)



getwd()  # get cwd



setwd("C:/Develop/R")  # set cwd



Sys.time()  # get date and time

Sys.Date()  # get date only



num_var1 <- 3  # "<-" and "=" are valid assignment operators
num_var1
num_var1 = 3
num_var1
median(x = 1:10)  # "=" assignment within argument list
x  # x doesn't exist outside the function
median(x <- 1:10)  # "<-" assignment within argument list
x  # x exists outside the function



rm(list=ls())
var1 <- 3  # define new object
ls()  # list objects

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

load(file="C:/Develop/data/my_data.RData")
ls()  # list objects



##   q()  # quit R session



## history(5)  # display last 5 commands
## savehistory(file="myfile")  # default is ".Rhistory"
## loadhistory(file="myfile")  # default is ".Rhistory"



"Hello World!"  # type some text

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

paste("Today is", Sys.time())  # coerce and concatenate strings
paste("Today is", format(Sys.time(), "%B-%d-%Y"))

paste(c(str_var1, str_var2), collapse="+")
paste(list(str_var1, str_var2), collapse="+")

strsplit("Hello World", split=' ')  # split string
substring("Hello World", 3, 6)  # extract characters from 3 to 6



gsub("is", "XX", "is this gratis?")  # replace "is" with "XX"

grep("b+", c("abc", "xyz", "cba d", "bbb"))  # get indexes

grep("b+", c("abc", "xyz", "cba d", "bbb"), value=TRUE)  # get values

glob2rx("abc.*")  # convert globs into regex
glob2rx("*.doc")



vec_var <- c(pi, exp(1), -digamma(1))  # define a vector
vec_var
vec_var[2]  # get second element
vec_var[-2]  # get all elements, except the second element
vec_var[c(F, T, T)]  # get second and third elements
vec_var == vec_var[2]  # logical vector of elements equal to the second one
vec_var < 1  # logical vector of elements less than one
vec_var[vec_var > 1]  # get all elements greater than one



vec_var <- c(pi, exp(1), -digamma(1))
vec_var
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
seq(-2,2, len=10)  # 10 evenly distributed numbers from -2 to 2
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

c(11, 5:10)  # combine two vectors
c(vec_var, 2.0)  # append number to vector

letters[5:10]  # vector of letters
c('a', letters[5:10])  # combine two vectors of letters



rm(list=ls())
set.seed(1121)  # initialize the random number generator
runif(3)  # three random numbers from the uniform distribution
runif(3)  # produce another three numbers
set.seed(1121)  # re-initialize the random number generator
runif(3)  # produce another three numbers
# produce a random number from the standard normal distribution
rnorm(1)
# produce five random numbers from the standard normal distribution
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



vec_var <- runif(5)
vec_var

vec_var > 0.5  # logical vector

vec_var[vec_var > 0.5]  # filter elements > 0.5

which(vec_var > 0.5)  # index of elements > 0.5



mat_var <- matrix(5:10, nrow=2, ncol=3)  # create a matrix
mat_var  # by default matrices are constructed column-wise

mat_var[2, 3]  # get third element from second row
mat_var[2, ]  # get second row
mat_var[, 3]  # get third column
mat_var[, c(1,3)]  # get first and third column
mat_var[, -2]  # remove second column



attributes(mat_var)  # get matrix attributes
dim(mat_var)  # get dimension attribute
class(mat_var)  # get class attribute

rownames(mat_var) <- c("row1", "row2")  # set the rownames attribute
colnames(mat_var) <- c("col1", "col2", "col3")  # set the colnames attribute
mat_var
mat_var["row2", "col3"]  # get third element from second row
names(mat_var)  # get the names attribute
dimnames(mat_var)  # get dimnames attribute
attributes(mat_var)  # get matrix attributes



mat_var[1, ]  # subsetting produces a vector!
class(mat_var)
class(mat_var[1, ])
is.matrix(mat_var[1, ])
is.vector(mat_var[1, ])

mat_var[1, , drop=FALSE]  # drop=FALSE preserves matrix
class(mat_var[1, , drop=FALSE])
is.matrix(mat_var[1, , drop=FALSE])
is.vector(mat_var[1, , drop=FALSE])



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



attributes(5:10)  # a simple vector has no attributes
my_var <- c(pi=pi, euler=exp(1), gamma=-digamma(1))
attributes(my_var)  # named vector has "names" attribute
my_var <- 1:10
is.vector(my_var)  # is the object a vector?
attributes(my_var) <- list(dim=c(2, 5))
is.matrix(my_var)  # is the object a matrix?
my_var  # matrix object
structure(1:10, dim=c(2, 5))  # matrix object

my_var <- 0
attributes(my_var) <- list(class="Date")
my_var  # "Date" object
structure(0, class="Date")  # "Date" object



my_var <- matrix(runif(10), 2, 5)
class(my_var)  # has implicit class
attributes(my_var)  # but no explicit "class" attribute
c(typeof(my_var), mode(my_var), class(my_var))
class(my_var) <- "my_class"  # assign explicit "class" attribute
class(my_var)  # has explicit "class"
attributes(my_var)  # has explicit "class" attribute
is.matrix(my_var)  # is the object a matrix?
is.vector(my_var)  # is the object a vector?
attributes(unclass(my_var))



my_var <- 1:5
c(typeof(my_var), mode(my_var), class(my_var))

mode(my_var) <- "character"  # coerce to "character"
my_var
c(typeof(my_var), mode(my_var), class(my_var))
my_var <- as.character(1:5)  # explicitly coerce to "character"
c(typeof(my_var), mode(my_var), class(my_var))

my_var <- matrix(1:10, 2, 5)
my_var <- as.character(my_var)  # explicitly coerce to "character"
c(typeof(my_var), mode(my_var), class(my_var))
is.matrix(my_var)  # is the object a matrix?

as.logical(0:3)  # explicit coercion to "logical"

c(1:3, 'a')  # implicit coercion to "character"
as.numeric(c(1:3, 'a'))  # explicit coercion to "numeric"



mat_var <- 1:6  # create a vector
class(mat_var)  # get its class

dim(mat_var) <- c(2, 3)  # add dimension attribute to coerce into matrix
class(mat_var)  # get its class
is.matrix(mat_var)  # is the object a matrix?

dimnames(mat_var) <- list('rows'=c('row1', 'row2'),  # set dimnames attribute
                  'columns'=c('col1', 'col2', 'col3'))

mat_var



vec_var1 <- c(1, 2, 3)
vec_var2 <- c(4, 5, 6)
cbind(vec_var1, vec_var2)  # bind into columns
rbind(vec_var1, vec_var2)  # bind into rows
vec_var2 <- c(4, 5, 6, 7)  # extend second vector to four elements
cbind(vec_var1, vec_var2)  # recycling rule applied
1:6 + c(10, 20)  # another example of recycling rule


