library(knitr)
opts_chunk$set(prompt=TRUE, eval=FALSE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
options(digits=3)
thm <- knit_theme$get("acid")
knit_theme$set(thm)
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

grep("b", c("abc", "xyz", "cba d", "bbb"))  # get indexes

grep("b", c("abc", "xyz", "cba d", "bbb"), value=TRUE)  # get values

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
numeric(0)  # create zero-length vector
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
# subset whole vector
vec_tor[] <- 0
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
da_ta <- sample(0:6) + 0.1
da_ta
cut(x=da_ta, breaks=c(2, 4, 6, 8))
rbind(da_ta, cut(x=da_ta, breaks=c(2, 4, 6, 8)))
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
# calculate DAX percentage returns
dax_rets <- diff(log(EuStockMarkets[, 1]))
# plot histogram
par(mar=c(1, 1, 1, 1), oma=c(2, 2, 2, 0))
histo_gram <- hist(dax_rets, breaks=30,
  main="", ylim=c(0, 60), xlim=c(-0.04, 0.04),
  xlab="", ylab="", freq=FALSE)
# draw kernel density of histogram
lines(density(dax_rets), col='red', lwd=2)
# add density of normal distribution
curve(expr=dnorm(x, mean=mean(dax_rets),
  sd=sd(dax_rets)), add=TRUE, type="l",
  lwd=2, col="blue")
title(main="DAX return distribution", line=0)
# add legend
legend("topright", inset=0.05, cex=0.8, title=NULL,
  leg=c(colnames(EuStockMarkets)[1], "Normal"),
  lwd=6, bg="white", col=c("red", "blue"))
# total area under histogram
diff(histo_gram$breaks) %*% histo_gram$density
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
mat_rix <- matrix(5:10, nrow=2, ncol=3)  # create a matrix
mat_rix  # by default matrices are constructed column-wise
# create a matrix row-wise
matrix(5:10, nrow=2, byrow=TRUE)
mat_rix[2, 3]  # extract third element from second row
mat_rix[2, ]  # extract second row
mat_rix[, 3]  # extract third column
mat_rix[, c(1,3)]  # extract first and third column
mat_rix[, -2]  # remove second column
# subset whole matrix
mat_rix[] <- 0
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
# create a list with two elements
list_var <- list(c('a', 'b'), 1:4)
list_var
c(typeof(list_var), mode(list_var), class(list_var))
# lists are also vectors
c(is.vector(list_var), is.list(list_var))
length(list_var)
# create named list
list_var <- list(first=c('a', 'b'), second=1:4)
list_var
names(list_var)
unlist(list_var)
list_var[2]  # extract second element as sublist
list_var[[2]]  # extract second element
list_var[[2]][3]  # extract third element of second element
list_var[[c(2, 3)]]  # third element of second element
list_var$second  # extract second element
list_var$s  # extract second element - partial name matching
list_var$second[3]  # third element of second element
list_var <- list()  # empty list
list_var$a <- 1
list_var[2] <- 2
list_var
names(list_var)
as.list(c(1,2,3))
list(c(1,2,3))
data_frame <- data.frame(  # create a data frame
                type=c('rose', 'daisy', 'tulip'),
                color=c('red', 'white', 'yellow'),
                price=c(1.5, 0.5, 1.0)
              )  # end data.frame
data_frame
dim(data_frame)  # get dimension attribute
colnames(data_frame)  # get the colnames attribute
rownames(data_frame)  # get the rownames attribute
class(data_frame)  # get object class
typeof(data_frame)  # data frames are lists
is.data.frame(data_frame)

class(data_frame$type)  # get column class
class(data_frame$price)  # get column class
data_frame[, 3]  # extract third column as vector
data_frame[[3]]  # extract third column as vector
data_frame[3]  # extract third column as data frame
data_frame[, 3, drop=FALSE]  # extract third column as data frame
data_frame[[3]][2]  # second element from third column
data_frame$price[2]  # second element from 'price' column
is.data.frame(data_frame[[3]]); is.vector(data_frame[[3]])
data_frame[2, ]  # extract second row
data_frame[2, ][3]  # third element from second column
data_frame[2, 3]  # third element from second column
unlist(data_frame[2, ])  # coerce to vector
is.data.frame(data_frame[2, ]); is.vector(data_frame[2, ])
data_frame <- data.frame(  # create a data frame
                type=c('rose', 'daisy', 'tulip'),
                color=c('red', 'white', 'yellow'),
                price=c(1.5, 0.5, 1.0),
                row.names=c('flower1', 'flower2', 'flower3'),
                stringsAsFactors=FALSE
              )  # end data.frame
data_frame
class(data_frame$type)  # get column class
class(data_frame$price)  # get column class
# set option to not coerce character vectors to factors
options(stringsAsFactors=FALSE)
options("stringsAsFactors")
default.stringsAsFactors()
str(data_frame)  # display the object structure
dim(cars)  # the cars data frame has 50 rows
head(cars, n=5)  # get first five rows
tail(cars, n=5)  # get last five rows
# create a named vector
stu_dents <- sample(round(runif(5, min=1, max=10), digits=2))
names(stu_dents) <- c("Angie", "Chris", "Suzie", "Matt", "Liz")
# sort the vector into ascending order
sort(stu_dents)
# calculate index to sort into ascending order
order(stu_dents)
# sort the vector into ascending order
stu_dents[order(stu_dents)]
# calculate the sorted (ordered) vector
sort_ed <- stu_dents[order(stu_dents)]
# calculate index to sort into unsorted (original) order
order(order(stu_dents))
sort_ed[order(order(stu_dents))]
stu_dents
# create a data frame of stu_dents and their ranks
ra_nks <- c("first", "second", "third", "fourth", "fifth")
data.frame(students=stu_dents, rank=ra_nks[order(order(stu_dents))])
# permute data_frame of flowers on price column
order(data_frame$price)
# sort data_frame on price
data_frame[order(data_frame$price), ]
# sort data_frame on color
data_frame[order(data_frame$color), ]
order(c(2, 1:4))  # there's a tie
order(c(2, 1:4), 1:5)  # there's a tie
# read the Examples for sort()
as.matrix(data_frame)
vec_tor <- sample(9)
matrix(vec_tor, ncol=3)
as.matrix(vec_tor, ncol=3)
mat_rix <- matrix(5:10, nrow=2, ncol=3)  # create a matrix
rownames(mat_rix) <- c("row1", "row2")  # rownames attribute
colnames(mat_rix) <- c("col1", "col2", "col3")  # colnames attribute
library(microbenchmark)
# call method instead of generic function
as.data.frame.matrix(mat_rix)
# a few methods for generic function as.data.frame()
sample(methods(as.data.frame), size=4)
# function method is faster than generic function
summary(microbenchmark(
  as_data_frame_matrix=
    as.data.frame.matrix(mat_rix),
  as_data_frame=as.data.frame(mat_rix),
  data_frame=data.frame(mat_rix),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
library(microbenchmark)
# lapply is faster than coercion function
summary(microbenchmark(
  as_list=
    as.list(as.data.frame.matrix(mat_rix)),
  l_apply=
    lapply(seq_along(mat_rix[1, ]),
     function(in_dex) mat_rix[, in_dex]),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# ?iris  # get information on iris
dim(iris)
head(iris, 2)
colnames(iris)
unique(iris$Species)  # list of unique elements of iris
class(unique(iris$Species))
# find which columns of iris are numeric
sapply(iris, is.numeric)
# calculate means of iris columns
sapply(iris, mean)  # returns NA for Species
# ?mtcars  # mtcars data from 1974 Motor Trend magazine
# mpg   Miles/(US) gallon
# qsec   1/4 mile time
# hp	 Gross horsepower
# wt	 Weight (lb/1000)
# cyl   Number of cylinders
dim(mtcars)
head(mtcars, 2)
colnames(mtcars)
head(rownames(mtcars), 3)
unique(mtcars$cyl)  # extract list of car cylinders
sapply(mtcars, mean)  # calculate means of mtcars columns
library(MASS)
# ?Cars93  # get information on Cars93
dim(Cars93)
head(colnames(Cars93))
# head(Cars93, 2)
unique(Cars93$Type)  # extract list of car types
# sapply(Cars93, mean)  # calculate means of Cars93 columns
# plot histogram of Highway MPG using the Freedman-Diaconis rule
hist(Cars93$MPG.highway, col="lightblue1",
     main="Distance per Gallon 1993", xlab="Highway MPG", breaks="FD")
rm(list=ls())
as.numeric(c(1:3, "a"))  # NA from coercion
0/0  # NaN from ambiguous math
1/0  # Inf from divide by zero
is.na(c(NA, NaN, 0/0, 1/0))  # test for NA
is.nan(c(NA, NaN, 0/0, 1/0))  # test for NaN
NA*1:4  # create vector of Nas
# create vector with some NA values
da_ta <- c(1, 2, NA, 4, NA, 5)
da_ta
mean(da_ta)  # returns NA, when NAs are input
mean(da_ta, na.rm=TRUE)  # remove NAs from input data
da_ta[!is.na(da_ta)]  # delete the NA values
sum(!is.na(da_ta))  # count non-NA values
rm(list=ls())
# airquality data has some NAs
head(airquality)
dim(airquality)
# number of NAs
sum(!complete.cases(airquality))
# display rows containing NAs
head(airquality[!complete.cases(airquality), ])
rm(list=ls())
# remove rows containing NAs
good_air <- airquality[complete.cases(airquality), ]
dim(good_air)
head(good_air)  # NAs removed
library(zoo)  # load package zoo
# replace NAs
good_air <- zoo::na.locf(airquality)
dim(good_air)
head(good_air)  # NAs replaced
# create vector containing NA values
vec_tor <- sample(22)
vec_tor[sample(NROW(vec_tor), 4)] <- NA
# replace NA values with the most recent non-NA values
zoo::na.locf(vec_tor)
# NULL values have no mode or type
c(mode(NULL), mode(NA))
c(typeof(NULL), typeof(NA))
c(length(NULL), length(NA))
# check for NULL values
is.null(NULL)
# NULL values are ignored when combined into a vector
c(1, 2, NULL, 4, 5)
# But NA value isn't ignored
c(1, 2, NA, 4, 5)
# vectors can be initialized to NULL
vec_tor <- NULL
is.null(vec_tor)
# grow the vector in a loop - very bad code!!!
for (in_dex in 1:5)
  vec_tor <- c(vec_tor, in_dex)
# initialize empty vector
vec_tor <- numeric()
# grow the vector in a loop - very bad code!!!
for (in_dex in 1:5)
  vec_tor <- c(vec_tor, in_dex)
# allocate vector
vec_tor <- numeric(5)
# assign to vector in a loop - good code
for (in_dex in 1:5)
  vec_tor[in_dex] <- runif(1)
