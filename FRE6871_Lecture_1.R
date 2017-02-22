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
# create Boolean vector
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
# Boolean vector TRUE if element is equal to second one
vec_tor == vec_tor[2]
# Boolean vector TRUE for elements greater than six
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
vec_tor > 0.5  # Boolean vector
# Boolean vector of elements equal to the second one
vec_tor == vec_tor[2]
# extract all elements equal to the second one
vec_tor[vec_tor == vec_tor[2]]
vec_tor < 1  # Boolean vector of elements less than one
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
mat_rix[, c(TRUE, FALSE, TRUE)]  # subset columns Boolean vector
mat_rix[1, ]  # subsetting can produce a vector!
class(mat_rix); class(mat_rix[1, ])
is.matrix(mat_rix[1, ]); is.vector(mat_rix[1, ])
mat_rix[1, , drop=FALSE]  # drop=FALSE preserves matrix
class(mat_rix[1, , drop=FALSE])
is.matrix(mat_rix[1, , drop=FALSE]); is.vector(mat_rix[1, , drop=FALSE])
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
4.7 * 0.5  # multiplication
4.7 / 0.5  # division
# exponentiation
2**3
2^3
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
switch("a", a="aaahh", b="bee", c="see", d=2, 
       "else this")
switch("c", a="aaahh", b="bee", c="see", d=2, 
       "else this")
switch(3, a="aaahh", b="bee", c="see", d=2, 
       "else this")
switch("cc", a="aaahh", b="bee", c="see", d=2, 
       "else this")
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
library(quantmod)
some_cars <- mtcars[sample(NROW(mtcars), 10), ]
# plot scatterplot horsepower vs miles per gallon
plot(some_cars[, "hp"], some_cars[, "mpg"],
     xlab="horsepower", ylab="miles per gallon",
     main="miles per gallon vs horsepower")
# add a solid red point (pch=16) for the last car
points(x=some_cars[NROW(some_cars), "hp"],
 y=some_cars[NROW(some_cars), "mpg"],
 col="red", pch=16)
# add labels with the car names
text(x=some_cars[, "hp"], y=some_cars[, "mpg"],
     labels=rownames(some_cars[, ]),
     pos=1, cex=0.8)
# or add labels using wordcloud, to prevent overlaps
library(wordcloud)
textplot(x=some_cars[, "hp"], y=some_cars[, "mpg"],
   words=rownames(some_cars))
# plot the tree Height
plot(trees[, "Height"],
     type="l",
     lwd=2,
     col="blue",
     main="Tree heights and volumes",
     xlab="tree number", ylab="",
     ylim=c(min(trees[, c("Height", "Volume")]),
      max(trees[, c("Height", "Volume")])))
# plot the tree Volume
lines(trees[, "Volume"], lwd=2, col="green")
# add legend
legend(x="left", legend=c("Height", "Volume"),
 inset=0.1, cex=1.0, bg="white",
 lwd=2, lty=c(1, 1), col=c("blue", "green"))
x_var <- seq(-2*pi, 2*pi, len=100)  # x values

# open Windows graphics device
x11(width=11, height=7, title="simple plot")

# plot a sine function using basic line plot
plot(x=x_var, y=sin(x_var), xlab="x-val",
     ylab="y-val", type="l", lwd=2, col="red")
# add a cosine function
lines(x=x_var, y=cos(x_var), lwd=2, col="blue")
# add title
title(main="sine and cosine functions", line=0.1)
# add legend
legend(x="topright", legend=c("sine", "cosine"),
 title="legend", inset=0.1, cex=1.0, bg="white",
 lwd=2, lty=c(1, 1), col=c("red", "blue"))
graphics.off()  # close all graphics devices
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# plot a Normal probability distribution
curve(expr=dnorm, type="l", xlim=c(-3, 3),
xlab="", ylab="", lwd=2, col="blue")
# add shifted Normal probability distribution
curve(expr=dnorm(x, mean=1), add=TRUE,
type="l", lwd=2, col="red")

# add title
title(main="Normal probability distribution functions",
line=0.1)
# add legend
legend(x="topright", legend=c("Normal", "shifted"),
 title="legend", inset=0.05, cex=0.8, bg="white",
 lwd=2, lty=c(1, 1), col=c("blue", "red"))
rm(list=ls())
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
x_var <- seq(-5, 7, length=100)
y_var <- dnorm(x_var, mean=1.0, sd=2.0)
plot(x_var, y_var, type="l", lty="solid",
     xlab="", ylab="")
title(main="Normal Density Function", line=0.5)
star_t <- 3; fin_ish <- 5  # set lower and upper bounds
# set polygon base
are_a <- ((x_var >= star_t) & (x_var <= fin_ish))
polygon(c(star_t, x_var[are_a], fin_ish),  # draw polygon
  c(-1, y_var[are_a], -1), col="red")
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
sig_mas <- c(0.5, 1, 1.5, 2)  # sigma values
# create plot colors
col_ors <- c("red", "black", "blue", "green")
# create legend labels
lab_els <- paste("sigma", sig_mas, sep="=")
for (in_dex in 1:4) {  # plot four curves
curve(expr=dnorm(x, sd=sig_mas[in_dex]),
type="l", xlim=c(-4, 4),
xlab="", ylab="", lwd=2,
col=col_ors[in_dex],
add=as.logical(in_dex-1))
}  # end for
# add title
title(main="Normal Distributions", line=0.5)
# add legend
legend("topright", inset=0.05, title="Sigmas",
 lab_els, cex=0.8, lwd=2, lty=c(1, 1, 1, 1),
 col=col_ors)
set.seed(1121)  # reset random number generator
runif(3)  # three numbers from uniform distribution
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
# define logistic map function
log_map <- function(x, r=4) r*x*(1-x)
log_map(0.25, 4)
# plot logistic map
x11(width=6, height=5)
curve(expr=log_map, type="l", xlim=c(0, 1),
xlab="x[n-1]", ylab="x[n]", lwd=2, col="blue",
main="logistic map")
# calculate uniformly distributed pseudo-random
# sequence using logistic map function
uni_form <- function(see_d, len_gth=10) {
  # pre-allocate vector instead of "growing" it
  out_put <- numeric(len_gth)
  # initialize
  out_put[1] <- see_d
  # perform loop
  for (i in 2:len_gth) {
    out_put[i] <- 4*out_put[i-1]*(1-out_put[i-1])
  }  # end for
  acos(1-2*out_put)/pi
}  # end uni_form
uni_form(see_d=0.1, len_gth=15)
plot(
  density(uni_form(see_d=runif(1), len_gth=1e5)),
  xlab="", ylab="", lwd=2, col="blue",
  main="uniform pseudo-random number density")
# calculate random default probabilities
num_assets <- 100
default_probs <- runif(num_assets, max=0.05)
# calculate number of defaults
uni_form <- runif(num_assets)
sum(uni_form < default_probs)
# calculate average number of defaults
de_faults <- numeric(200)
for (i in 1:200) {  # perform loop
  uni_form <- runif(num_assets)
  de_faults[i] <- sum(uni_form < default_probs)
}  # end for
mean(de_faults)
# average defaults using vectorized functions
uni_form <- matrix(runif(200*num_assets),
             ncol=200)
sum(uni_form < default_probs)/200
# plot Standard Normal distribution
x11(width=6, height=5)
curve(expr=dnorm(x),
type="l", xlim=c(-4, 4),
xlab="asset value", ylab="", lwd=2,
col="blue", main="Distribution of Asset Values")
abline(v=qnorm(0.025), col="red", lwd=2)
text(x=qnorm(0.025)-0.1, y=0.15,
 labels="default threshold",
 lwd=2, srt=90, pos=3)
# define default probability function
vasi_cek <- function(x, def_thresh=-2, rh_o=0.01)
  sqrt(1-rh_o)*dnorm((sqrt(1-rh_o)*qnorm(x) - def_thresh)/sqrt(rh_o))/sqrt(rh_o)
vasi_cek(0.03, def_thresh=qnorm(0.025), rh_o=0.1)
# plot probability distribution of defaults
curve(expr=vasi_cek(x, def_thresh=qnorm(0.025), rh_o=0.02),
type="l", xlim=c(0, 0.1),
xlab="fraction of defaults", ylab="", lwd=2,
col="green", main="Distribution of defaults")
# plot default distribution with higher correlation
curve(expr=vasi_cek(x, def_thresh=qnorm(0.025), rh_o=0.08),
type="l", xlim=c(0, 0.1), add=TRUE,
xlab="default fraction", ylab="", lwd=2,
col="blue", main="")
# add legend
legend(x="topright", legend=c("high correlation", "low correlation"),
 title="", inset=0.05, cex=0.8, bg="white",
 lwd=2, lty=c(1, 1), col=c("blue", "green"))
# add unconditional default probability
abline(v=0.025, col="red", lwd=2)
text(x=0.023, y=1,
 labels="default probability",
 lwd=2, srt=90, pos=3)
