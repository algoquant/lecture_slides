library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
thm <- knit_theme$get("acid")
knit_theme$set(thm)
setwd("C:/Develop/data")
cat("Enter\ttab")  # cat() interprets backslash escape sequences
print("Enter\ttab")

my_text <- print("hello")
my_text  # print() returns its argument

# create string
my_text <- "Title: My Text\nSome numbers: 1,2,3,...\nRprofile files contain code executed at R startup,\n"

cat(my_text, file="mytext.txt")  # write to text file

cat("Title: My Text",  # write several lines to text file
    "Some numbers: 1,2,3,...",
    "Rprofile files contain code executed at R startup,", 
    file="mytext.txt", sep="\n")

save(my_text, file="mytext.Rdata")  # write to binary file
setwd("C:/Develop/data")
# read text from file
scan(file="mytext.txt", what=character(), sep="\n")

# read lines from file
readLines(con="mytext.txt")

# read text from console
in_put <- readline("Enter a number: ")
class(in_put)
# coerce to numeric
in_put <- as.numeric(in_put)

# read text from file and display in editor:
# file.show("mytext.txt")
# file.show("mytext.txt", pager="")
data_frame <- data.frame(
        type=c("rose", "daisy", "tulip"), 
        color=c("red", "white", "yellow"), 
        price=c(1.5, 0.5, 1.0), 
        row.names=c("flower1", "flower2", "flower3")
        )  # end data_frame
mat_var <- matrix(sample(1:12), ncol=3, dimnames=list(NULL, c("col1", "col2", "col3")))
rownames(mat_var) <- paste("row", 1:nrow(mat_var), sep="")
# write data frame to text file, and then read it back
write.table(data_frame, file="florist.txt")
data_read <- read.table(file="florist.txt")
data_read  # a data frame

# write matrix to text file, and then read it back
write.table(mat_var, file="matrix.txt")
mat_read <- read.table(file="matrix.txt")
mat_read  # write.table() coerced matrix to data frame
class(mat_read)
# coerce from data frame back to matrix
mat_read <- as.matrix(mat_read)
class(mat_read)
## data_frame <- data.frame(small=c(3, 5), medium=c(9, 11), large=c(15, 13))
## setwd("C:/Develop/data")
## data_frame <- read.table("mydata.txt", header=TRUE)
## data_frame <- read.table("clipboard", header=TRUE)
## 
## write.table(x=data_frame, file="clipboard", sep="\t")
## 
## # wrapper function for copying data frame from clipboard into R
## # by default, data is tab delimited, with a header
## read_clip <- function(file="clipboard", sep="\t",
##               header=TRUE, ...) {
##   read.table(file=file, sep=sep, header=header, ...)
## }  # end read_clip
## 
## data_frame <- read_clip()
## 
## # wrapper function for copying data frame from R into clipboard
## # by default, data is tab delimited, with a header
## write.clip <- function(data, row.names=FALSE,
##                col.names=TRUE, ...) {
##   write.table(x=data, file="clipboard", sep="\t",
##       row.names=row.names, col.names=col.names, ...)
## }  # end write.clip
## 
## write.clip(data=data_frame)
## 
## # launch spreadsheet-style data editor
## data_frame <- edit(data_frame)
# write data frame to CSV file, and then read it back
write.csv(data_frame, file="florist.csv")
data_read <- read.csv(file="florist.csv", 
                 stringsAsFactors=FALSE)
data_read  # the rownames are read in as extra column
# restore rownames
rownames(data_read) <- data_read[, 1]
data_read <- data_read[, -1]  # remove extra column
data_read
# write matrix to csv file, and then read it back
write.csv(mat_var, file="matrix.csv")
mat_read <- read.csv(file="matrix.csv", 
               stringsAsFactors=FALSE)
mat_read  # read.csv() read matrix as data frame
class(mat_read)
# restore rownames
rownames(mat_read) <- mat_read[, 1]
mat_read <- mat_read[, -1]  # remove extra column
# coerce from data frame back to matrix
mat_read <- as.matrix(mat_read)
class(mat_read)
mat_read  # a matrix
library("MASS")  # load library "MASS"
# write to CSV file by row - it's very SLOW!!!
write.matrix(mat_var, file="matrix.csv", sep=",")
system.time(  # scan reads faster - skip first line with colnames
  mat_read <- scan(file="matrix.csv", sep=",", 
            skip=1, what=numeric()))
col_names <- readLines(con="matrix.csv", n=1)  # read colnames
col_names  # this is a string!
col_names <- strsplit(col_names, s=",")[[1]]  # convert to char vector
mat_read  # mat_read is a vector, not matrix!
# coerce by row to matrix
mat_read <- matrix(mat_read, ncol=length(col_names), 
            byrow=TRUE)
colnames(mat_read) <- col_names  # restore colnames
mat_read
mat_read <- read.csv(file="badmatrix.csv")
# restore rownames
rownames(mat_read) <- as.character(mat_read[, 1])
mat_read <- mat_read[, -1]  # remove extra column
dim_names <- dimnames(mat_read)  # save dimnames
num_col <- ncol(mat_read)  # save num columns
mat_read  # data frame with bad data
class(mat_read)
class(mat_read[, 1])
class(mat_read[, 2])  # numeric coerced to factor by bad data
# convert non-numeric to NA by coercing to matrix and then to numeric
mat_read <- as.numeric(as.matrix(mat_read))
mat_read  # vector with NAs
mat_read[is.na(mat_read)] <- 0  # overwrite NAs
# coerce from vector back to matrix
mat_read <- matrix(mat_read, ncol=num_col, dimnames=dim_names)
mat_read  # matrix without NAs
setwd("C:/Develop/data")
{
sink("sinkdata.txt")# redirect text output to file

cat("Redirect text output from R\n")
print(runif(10))
cat("\nEnd data\nbye\n")

sink()  # turn redirect off

pdf("Rgraph.pdf", width=7, height=4)  # redirect graphics to pdf file

cat("Redirect data from R into pdf file\n")
my_var <- seq(-2*pi, 2*pi, len=100)
plot(x=my_var, y=sin(my_var), main="Sine wave", 
   xlab="", ylab="", type="l", lwd=2, col="red")
cat("\nEnd data\nbye\n")

dev.off()  # turn pdf output off

png("Rgraph.png")  # redirect output to png file

cat("Redirect graphics from R into png file\n")
plot(x=my_var, y=sin(my_var), main="Sine wave", 
 xlab="", ylab="", type="l", lwd=2, col="red")
cat("\nEnd data\nbye\n")

dev.off()  # turn png output off
}
# create functional that accepts a function as input argument
func_tional <- function(func_arg) {
# calculates statistic on random numbers
  set.seed(1)
  func_arg(runif(1e4))  # apply the function name
}  # end func_tional
func_tional(mean)
func_tional(sd)
func_tional <- function(func_arg, arg_var) {
# functional accepts function name and one additional argument
  func_arg <- match.fun(func_arg)  # produce function from name
  func_arg(arg_var)  # apply input function
}  # end func_tional

func_tional(sqrt, 4)

# if argument is a list, then we need to call "do.call", to pass them one by one
# Passing a function as a list: first element is function name, remaining elements are arguments
func_tional <- function(list_arg) {
  func_arg <- match.fun(list_arg[[1]])  # produce function from name
  do.call(func_arg, list(as.numeric(list_arg[-1])))
}  # end func_tional

arg_list <- list("mean", 1, 2, 3, 4)
func_tional(arg_list)
func_tional <- function(func_arg, ...) {
# functional accepts function and additional '...' arguments
  func_arg(...)  # apply input function to '...' arguments
}  # end func_tional

func_tional(sum, 1, 2, 3)
rm(list=ls())
str(apply)  # get list of arguments
mat_rix <- matrix(6:1, nrow=2, ncol=3)  # create a matrix
mat_rix
# sum the rows and columns
row_sums <- apply(mat_rix, 1, sum)
col_sums <- apply(mat_rix, 2, sum)
mat_rix <- cbind(c(sum(row_sums), row_sums), 
          rbind(col_sums, mat_rix))
dimnames(mat_rix) <- list(c("col_sums", "row1", "row2"), 
                   c("row_sums", "col1", "col2", "col3"))
mat_rix
str(apply)  # get list of arguments
mat_rix <- matrix(sample(12), nrow=3, ncol=4)  # create a matrix
mat_rix
apply(mat_rix, 2, sort)  # sort matrix columns
apply(mat_rix, 2, sort, decreasing=TRUE)  # sort decreasing order
mat_rix[2, 2] <- NA  # introduce NA value
mat_rix
# calculate median of columns
apply(mat_rix, 2, median)
# calculate median of columns with na.rm=TRUE
apply(mat_rix, 2, median, na.rm=TRUE)
# simple anonymous function
(function(x) (x + 3)) (10)

# functional accepts function and additional '...' arguments
func_tional <- function(func_arg, ...) {
  func_arg(...)  # apply input function to '...' arguments
}  # end func_tional
func_tional(sum, 2, 3, 4)
func_tional(mean, 1:10)
func_tional((func_arg=function(x) (x + 3)), 5)
# anonymous function can be a default value
func_tional <- function(..., 
                func_arg=function(x, y, z) {x+y+z}) {
  func_arg(...)  # apply input function to '...' arguments
}  # end func_tional
func_tional(2, 3, 4)  # use default func_arg
func_tional(func_arg=sum, 2, 3, 4)  # func_arg bound by name
func_tional(func_arg=sum, 2, 3, 4, 5)
func_tional(2, 3, 4, 5)
# pass anonymous function to func_arg
func_tional(func_arg=function(x, y, z) {x*y*z}, 2, 3, 4)
rm(list=ls())
dax_rets <- 100*diff(log(EuStockMarkets[, 1]))  # DAX percent returns
library("moments")  # load library"moments"
str(moment)  # get list of arguments

moment(x=dax_rets, order=3)  # apply moment function

moment_orders <- as.matrix(1:4)  # 4x1 matrix of moment orders

# anonymous function allows looping over function parameters
apply(X=moment_orders, MARGIN=1, 
      FUN=function(moment_order) {
  moment(x=dax_rets, order=moment_order)
}  # end anonymous function
      )  # end apply

# another way of passing parameters into moment() function
apply(X=moment_orders, MARGIN=1, FUN=moment, x=dax_rets)
sapply(6:10, sqrt)  # sapply on vector
sapply(list(6, 7, 8, 9, 10), sqrt)  # sapply on list

# calculate means of iris data frame columns
sapply(iris, mean)  # returns NA for Species

# create a matrix
mat_rix <- matrix(sample(100), ncol=4)
# calculate column means using apply
apply(mat_rix, 2, mean)

# calculate column means using sapply, with anonymous function
sapply(1:ncol(mat_rix), 
       function(col_index) {  # anonymous function
 mean(mat_rix[, col_index])
  }  # end anonymous function
)  # end sapply
sapply(iris[, -5], mean)  # vector of means of numeric columns
lapply(iris[, -5], mean)  # calculate means of numeric columns
# calculate means of numeric columns using anonymous function
unlist(lapply(iris, 
      function(col_umn) {
        if (is.numeric(col_umn)) mean(col_umn)
      }  # end anonymous function
      )  # end sapply
       )  # end unlist
unlist(sapply(iris, function(col_umn) {if (is.numeric(col_umn)) mean(col_umn)}))
# ?iris  # get information on iris
dim(iris)
head(iris, 2)
colnames(iris)
unique(iris$Species)  # extract list of unique elements of iris
class(unique(iris$Species))
# find which columns of iris are numeric
sapply(iris, is.numeric)
# calculate means of iris columns
sapply(iris, mean)  # returns NA for Species
# ?mtcars  # get information on mtcars - data from 1974 Motor Trend magazine
# mpg   Miles/(US) gallon
# qsec   1/4 mile time
# hp   Gross horsepower
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
unique(iris$Species)  # Species takes on three distinct values
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
# split the mtcars data frame based on number of cylinders
split_cars <- split(mtcars, mtcars$cyl)
str(split_cars, max.level=1)
names(split_cars)
# get mean mpg for each cylinder group
unlist(lapply(split_cars, function(x) mean(x$mpg)))
# Which is identical to the tapply function
tapply(mtcars$mpg, mtcars$cyl, mean)
# using "with" environment
with(mtcars, tapply(mpg, cyl, mean))
# can also use the functions by() and aggregate()
with(mtcars, by(mpg, cyl, mean))
aggregate(formula=(mpg ~ cyl), data=mtcars, FUN=mean)
data_cars <- sapply(split_cars,  # get several mpg stats for each cylinder group
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
do.call(cbind, data_cars)  # do.call flattens list into a matrix
rm(list=ls())
as.numeric(c(1:3, "a"))  # NA from coercion
0/0  # NaN from ambiguous math
1/0  # Inf from divide by zero
is.na(c(NA, NaN, 0/0, 1/0))  # test for NA
is.nan(c(NA, NaN, 0/0, 1/0))  # test for NaN
NA*1:4  # create vector of Nas
bad_data <- c(1, 2, NA, 4, NA, 5)  # create vector with some NA values
mean(bad_data)  # returns NA, when NAs are input
mean(bad_data, na.rm=TRUE)  # remove NAs from input data
bad_data[!is.na(bad_data)]  # delete the NA values
sum(!is.na(bad_data))  # count non-NA values
rm(list=ls())
head(airquality)  # airquality data has some NAs
dim(airquality)
sum(!complete.cases(airquality))  # number of NAs
head(airquality[!complete.cases(airquality), ])  # display some NAs
rm(list=ls())
good_air <- airquality[complete.cases(airquality), ]  # remove NAs
dim(good_air)
head(good_air)  # NAs removed
library(zoo)  # load package zoo
good_air <- na.locf(airquality)  # replace NAs
dim(good_air)
head(good_air)  # NAs replaced
