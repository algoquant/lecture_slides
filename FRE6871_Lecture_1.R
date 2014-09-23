

library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
thm <- knit_theme$get("acid")
knit_theme$set(thm)



getwd()  # get cwd



setwd("C:/Develop/R")  # set cwd



Sys.time()  # returns a character string
format(Sys.time(), "%B-%d-%Y")  # format the string
paste("Today is", Sys.time())  # concatenate strings
paste("Today is", format(Sys.time(), "%B-%d-%Y"))



var1 <- 3  # "<-" and "=" are valid assignment operators
var1
var1 = 3
var1
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
     file="C:/jerzy/temp/my_data.RData")
rm(var1, var2)  # remove objects
ls()  # list objects
load(file="C:/jerzy/temp/my_data.RData")
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

paste(str_var1, str_var2, sep=' ')  # concatenate

paste('a', 1:4, sep='-')  # convert, recycle and concatenate

cat(str_var1, str_var2)  # concatenate and write to output

strsplit("Hello World", split='r')  # split string

substring("Hello World", 3, 6)  # extract characters from 3 to 6



gsub("is", "XX", "is this gratis?")  # replace "is" with "XX"

grep("b+", c("abc", "xyz", "cba d", "bbb"))  # get indexes

grep("b+", c("abc", "xyz", "cba d", "bbb"), value=TRUE)  # get values



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



vec_var <- c(pi=pi, euler=exp(1), gamma=-digamma(1))
vec_var
names(vec_var)  # get names of elements
vec_var['euler']  # get element named 'euler'
names(vec_var) <- c("pie","eulery","gammy")  # rename elements
vec_var



seq(from=0, to=1, len=11)  # vector of decimals from 0 to 1.0
seq(from=0, to=1, by=0.1)  # vector of decimals from 0 to 1.0
0:10  # vector of integers from 0 to 10
(0:10)/10  # vector of decimals from 0 to 1.0
letters[5:10]  # vector of letters



c(11, 5:10)  # append to vector
c(20:25, 5:10)  # append to vector
vec_var <- vector()  # create empty vector
vec_var <- vector("numeric",  # create numeric vector of zeros
          length=10)
c(vec_var, 2.0)  # append to vector
c('a', letters[5:10])  # vector of letters



list_var <- list(c('a', 'b'), 1:4)  # create a list
list_var
typeof(list_var)
list_var[[2]]  # get second element
list_var[[2]][3]  # get third element of second element
list_var[[c(2, 3)]]  # get third element of second element
list_var <- list(first=c('a', 'b'),  # create named list
        second=1:4)
names(list_var)
list_var$second  # get second element
list_var$s  # get second element - partial name matching
list_var$second[3]  # get third element of second element



attributes(5:10)  # a simple vector has no attributes
vec_var <- c(pi=pi, euler=exp(1), gamma=-digamma(1))
attributes(vec_var)  # a named vector has 'names' attribute
class(attributes(vec_var))  # the attribute is a list object 
length(vec_var)
is.vector(vec_var)  # is the object a vector?
is.vector(names(vec_var))  # names are character vectors



vec_var <- c(pi=pi, euler=exp(1), gamma=-digamma(1))
typeof(vec_var)  # get object type
class(vec_var)  # get object class
typeof(names(vec_var))
class(names(vec_var))
vec_var <- c(1:5, 'a')  # append to vector
vec_var  # type coercion
class(vec_var)  # get object class



c(1:3, 'a')  # implicit coercion
as.numeric(c(1:3, 'a'))  # explicit coercion
as.logical(0:3)
as.character(0:3)



mat_var <- matrix(5:10, nrow=2, ncol=3)  # create a matrix
mat_var  # by default matrices are constructed column-wise

mat_var[2, 3]  # get third element from second row
mat_var[2, ]  # get second row
mat_var[ ,3]  # get third column
mat_var[, c(1,3)]  # get first and third column
mat_var[ ,-2]  # remove second column



dim(mat_var)  # get dimension attribute
class(mat_var)  # get class of matrix object

rownames(mat_var) <- c("row1", "row2")  # set the rownames attribute
colnames(mat_var) <- c("col1", "col2", "col3")  # set the colnames attribute
mat_var
mat_var["row2", "col3"]  # get third element from second row

names(mat_var)  # get the names attribute
dimnames(mat_var)  # get dimnames attribute



mat_var <- 1:6  # create a vector
class(mat_var)  # get its class

dim(mat_var) <- c(2, 3)  # add dimension attribute to coerce into matrix
class(mat_var)  # get its class

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


