

library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
thm <- knit_theme$get("acid")
knit_theme$set(thm)



set.seed(1121)  # for reproducibility
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# create monthly time series starting 1990
ts_series <- ts(data=cumsum(rnorm(96)), 
     frequency=12, start=c(1990, 1))
class(ts_series)  # class 'ts'
attributes(ts_series)
matrix(methods(class="ts")[3:8], ncol=2)
# window the time series
window(ts_series, start=1992, end=1992.25)

plot(ts_series, type="l",  # create plot
     col="red", lty="solid", xlab="", ylab="")
title(main="Random Prices", line=-1)  # add title



par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
class(EuStockMarkets)  # multiple ts object
dim(EuStockMarkets)
head(EuStockMarkets)  # get first six rows
plot(EuStockMarkets, main="", xlab="")  # plot all the columns
title(main="EuStockMarkets", line=-1)  # add title



par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# calculate DAX percentage returns
dax_rets <- diff(log(EuStockMarkets[, 1]))
# mean and standard deviation of returns
c(mean(dax_rets), sd(dax_rets))
# plot histogram
hist(dax_rets, breaks=30, main="", 
     xlim=c(-0.04, 0.04), ylim=c(0, 60), 
     xlab="", ylab="", freq = FALSE)
# draw kernel density of histogram
lines(density(dax_rets), col='red', lwd=2)
# add density of normal distribution
curve(expr=dnorm(x, mean=mean(dax_rets), sd=sd(dax_rets)), 
      add=TRUE, type="l", lwd=2, col="blue")
title(main="Return distributions", line=0)  # add title
# add legend
legend("topright", inset=0.05, cex=0.8, title=NULL, 
       leg=c(colnames(EuStockMarkets)[1], "Normal"), 
       lwd=2, bg="white", col=c("red", "blue"))



par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# calculate percentage returns
dax_rets <- diff(log(EuStockMarkets[, 1]))
# create normal Q-Q plot
qqnorm(dax_rets, ylim=c(-0.04, 0.04), 
       xlab='Normal Quantiles', main='')
# fit a line to the normal quantiles
qqline(dax_rets, col='red', lwd=2)
plot_title <- paste(colnames(EuStockMarkets)[1], 
          'Q-Q Plot')
title(main=plot_title, line=-1)  # add title
shapiro.test(dax_rets)  # Shapiro-Wilk test



par(oma=c(15, 1, 1, 1), mgp=c(1.5, 0.5, 0), mar=c(2.5, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # set plot panels
# boxplot method for formula
boxplot(formula=mpg ~ cyl, data=mtcars, 
  main="Mileage by number of cylinders", 
  xlab="Cylinders", ylab="Miles per gallon")

# calculate EuStockMarkets percentage returns
eu_rets <- diff(log(EuStockMarkets))
# boxplot method for data frame
boxplot(x=eu_rets)



set.seed(1121)  # initialize the random number generator
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
library(zoo)  # load package zoo
# create index of daily dates
date_index <- seq(from=as.Date("2014-07-14"), 
            by="day", length.out=1000)
# create zoo time series
zoo_series <- zoo(cumsum(rnorm(length(date_index))), 
            order.by=date_index)

class(zoo_series)  # class 'zoo'
tail(zoo_series, 4)  # get last few elements

# call plot.zoo
plot(zoo_series, xlab="", ylab="")
title(main="Random Prices", line=-1)  # add title



library(zoo)  # load package zoo
# create zoo time series
date_index <- Sys.Date() + 0:3
zoo_series <- zoo(rnorm(length(date_index)), 
         order.by=date_index)
zoo_series
index(zoo_series)  # extract time index
coredata(zoo_series)  # extract coredata
zoo_series[start(zoo_series)]  # first element
zoo_series[end(zoo_series)]  # last element
coredata(zoo_series) <- rep(1, 4)  # replace coredata
cumsum(zoo_series)  # cumulative sum
cummax(cumsum(zoo_series))
cummin(cumsum(zoo_series))



library(zoo)  # load package zoo
coredata(zoo_series) <- 1:4  # replace coredata
zoo_series
diff(zoo_series)  # diff with one day lag
lag(zoo_series, 2)  # two day lag



set.seed(1121)  # initialize the random number generator
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
library(zoo)  # load package zoo
# create daily date series of class 'Date'
date_index1 <- seq(Sys.Date(), by="days", 
             length.out=365)
# create zoo time series
zoo_series1 <- zoo(rnorm(length(date_index1)), 
           order.by=date_index1)
# create another zoo time series
date_index2 <- seq(Sys.Date()+350, by="days", 
             length.out=365)
zoo_series2 <- zoo(rnorm(length(date_index2)), 
           order.by=date_index2)
# rbind the two time series - ts1 supersedes ts2
zoo_series3 <- rbind(zoo_series1,
           zoo_series2[index(zoo_series2) > end(zoo_series1)])
plot(cumsum(zoo_series3), xlab="", ylab="")
# add vertical lines at stitch point
abline(v=end(zoo_series1), col="blue", lty="dashed")
abline(v=start(zoo_series2), col="red", lty="dashed")
title(main="Random Prices", line=-1)  # add title



# create daily date series of class 'Date'
date_index1 <- Sys.Date() + -3:1
# create zoo time series
zoo_series1 <- zoo(rnorm(length(date_index1)), 
         order.by=date_index1)
# create another zoo time series
date_index2 <- Sys.Date() + -1:3
zoo_series2 <- zoo(rnorm(length(date_index2)), 
         order.by=date_index2)
merge(zoo_series1, zoo_series2)  # union of dates
# intersection of dates
merge(zoo_series1, zoo_series2, all=FALSE)



library(zoo)  # load package zoo
# create zoo time series
zoo_series <- zoo(rnorm(4), 
            order.by=(Sys.Date() + 0:3))
# add NA
zoo_series[3] <- NA
zoo_series

na.locf(zoo_series)  # replace NA's using locf

na.omit(zoo_series)  # remove NA's using omit



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
   xlab="", ylab="", type='l', lwd=2, col="red")
cat("\nEnd data\nbye\n")

dev.off()  # turn pdf output off

png("Rgraph.png")  # redirect output to png file

cat("Redirect graphics from R into png file\n")
plot(x=my_var, y=sin(my_var), main="Sine wave", 
 xlab="", ylab="", type='l', lwd=2, col="red")
cat("\nEnd data\nbye\n")

dev.off()  # turn png output off
}



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

readLines(con="mytext.txt")  # read text from file

# read text from file and display in editor:
# file.show("mytext.txt")
# file.show("mytext.txt", pager="")



data_frame <- data.frame(
        type=c('rose', 'daisy', 'tulip'), 
        color=c('red', 'white', 'yellow'), 
        price=c(1.5, 0.5, 1.0), 
        row.names=c('flower1', 'flower2', 'flower3')
        )  # end data_frame
mat_var <- matrix(sample(1:12), ncol=3, dimnames=list(NULL, c("col1", "col2", "col3")))
rownames(mat_var) <- paste("row", 1:nrow(mat_var), sep="")
# write data frame to text file, and then read it back
write.table(data_frame, file='florist.txt')
data_frame <- read.table(file='florist.txt')
data_frame  # a data frame

# write matrix to text file, and then read it back
write.table(mat_var, file='matrix.txt')
mat_var <- read.table(file='matrix.txt')
mat_var  # write.table() coerced matrix to data frame
class(mat_var)
# coerce from data frame back to matrix
mat_var <- as.matrix(mat_var)
class(mat_var)



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
## # data_frame <- edit(data_frame)



data_frame <- data.frame(
        type=c('rose', 'daisy', 'tulip'), 
        color=c('red', 'white', 'yellow'), 
        price=c(1.5, 0.5, 1.0), 
        row.names=c('flower1', 'flower2', 'flower3')
        )  # end data_frame
mat_var <- matrix(sample(1:12), ncol=3, dimnames=list(NULL, c("col1", "col2", "col3")))
rownames(mat_var) <- paste("row", 1:nrow(mat_var), sep="")
# write data frame to CSV file, and then read it back
write.csv(data_frame, file='florist.csv')
data_frame <- read.csv(file='florist.csv')
data_frame  # the rownames are read in as extra column
# restore rownames
rownames(data_frame) <- as.character(data_frame[, 1])
data_frame <- data_frame[, -1]  # remove extra column

# write matrix to csv file, and then read it back
write.csv(mat_var, file='matrix.csv')
mat_var <- read.csv(file='matrix.csv')
mat_var  # write.csv() coerced matrix to data frame
class(mat_var)
# restore rownames
rownames(mat_var) <- as.character(mat_var[, 1])
mat_var <- mat_var[, -1]  # remove extra column
# coerce from data frame back to matrix
mat_var <- as.matrix(mat_var)
class(mat_var)
mat_var  # a matrix



mat_var <- read.csv(file='badmatrix.csv')
# restore rownames
rownames(mat_var) <- as.character(mat_var[, 1])
mat_var <- mat_var[, -1]  # remove extra column
dim_names <- dimnames(mat_var)  # save dimnames
num_col <- ncol(mat_var)  # save num columns
mat_var  # data frame with bad data
class(mat_var)
class(mat_var[, 1])
class(mat_var[, 2])  # numeric coerced to factor by bad data
# convert non-numeric to NA by coercing to matrix and then to numeric
mat_var <- as.numeric(as.matrix(mat_var))
mat_var  # vector with NAs
mat_var[is.na(mat_var)] <- 0  # overwrite NAs
# coerce from vector back to matrix
mat_var <- matrix(mat_var, ncol=num_col, dimnames=dim_names)
mat_var  # matrix without NAs



mat_var <- matrix(1:6, nrow=2, ncol=3,  # create a matrix without rownames
          dimnames=list(NULL, c("col1", "col2", "col3")))
mat_var  # matrix without rownames
library('MASS')  # load library 'MASS'
# write to CSV file by row - it's very SLOW!!!
write.matrix(mat_var, file='matrix.csv', sep=',')
system.time(  # scan reads faster - skip first line with colnames
  mat_var <- scan(file='matrix.csv', sep=',', 
            skip=1, what=numeric()))
col_names <- readLines(con='matrix.csv', n=1)  # read colnames
col_names  # this is a string!
col_names <- strsplit(col_names, s=',')[[1]]  # convert to char vector
mat_var  # mat_var is a vector, not matrix!
# coerce by row to matrix
mat_var <- matrix(mat_var, ncol=length(col_names), 
            byrow=TRUE)
colnames(mat_var) <- col_names  # restore colnames
mat_var



rm(list=ls())
set.seed(1121)  # initialize the random number generator
library(zoo)  # load package zoo
# create POSIXct index
date_index <- seq(from=as.POSIXct("2013-06-15"), 
            by="hour", length.out=1000)
# create zoo time series
zoo_series <- zoo(cumsum(rnorm(length(date_index))), 
            order.by=date_index)
# write zoo to text file, and then read it back
write.zoo(zoo_series, file="zoo_series.txt")
zoo_series <- read.zoo("zoo_series.txt")  # read it back
# write zoo to CSV file, and then read it back
write.zoo(zoo_series, file="zoo_series.csv", sep=",")
zoo_series <- read.zoo(file='zoo_series.csv', 
            header=TRUE, sep=",", FUN=as.POSIXct)
# read zoo from CSV file, with custom date-time format
zoo_frame <- read.table(file='zoo_series2.csv', sep=",")
tail(zoo_frame, 3)  # date-time format mm/dd/yyyy hh:mm
zoo_series <- read.zoo(file='zoo_series2.csv', 
            header=TRUE, sep=",", FUN=as.POSIXct, 
            format="%m/%d/%Y %H:%M")
tail(zoo_series, 3)



rm(list=ls())
baseenv()  # get base environment
globalenv()  # get global environment
environment()  # get current environment
class(environment())  # get environment class
glob_var <- 1  # define variable in current environment
ls(environment())  # get objects in current environment

new_env <- new.env()  # create new environment
parent.env(new_env)  # get calling environment of new environment
assign("new_var1", 3, envir=new_env)  # assign Value to Name
new_env$new_var2 <- 11  # create object in new environment
ls(new_env)  # get objects in new environment
ls(environment())  # get objects in current environment
new_env$new_var1  # environments are subset like lists
new_env[['new_var1']]  # environments are subset like lists



search()  # get search path for R objects
my_list <- list(flowers=c('rose', 'daisy', 'tulip'),  # create a list
                trees=c('pine', 'oak', 'maple'))
my_list$trees
attach(my_list)
trees
search()  # get search path for R objects
detach(my_list)
head(trees)  # "trees" is part of the datasets base package



head(trees)  # "trees" is part of the datasets base package
colnames(trees)
mean(trees$Girth)
with(trees, mean(Girth))



rm(list=ls())
glob_var <- 1  # define a global variable
ls(environment())  # get all variables in environment
func_env <- function() {  # explore function environments
  loc_var <- 1  # define a local variable
  cat('objects in evaluation environment:\t', 
      ls(environment()), '\n')
  cat('objects in enclosing environment:\t', 
      ls(parent.env(environment())), '\n')
  cat('this is the enclosing environment:')
  parent.env(environment())  # return enclosing environment
}  # end func_env
func_env()

environment(func_env)
environment(print)  # package namespace is the enclosure



rm(list=ls())
glob_var <- 1  # define a global variable
probe_scope <- function() {  # explore function scope
  loc_var <- 2*glob_var  # define a local variable
  new_globvar <<- 11  # define a global variable
  cat('objects in evaluation environment:\t', 
      ls(environment()), '\n')
  cat('this is a local loc_var:\t', loc_var, '\n')
  cat('objects in enclosing environment:\n', 
      ls(parent.env(environment())), '\n')
  cat('this is glob_var:\t', glob_var, '\n')
  glob_var <- 10  # define local glob_var
  cat('this is the local glob_var:\t', glob_var, '\n')
}  # end probe_scope
probe_scope()
glob_var  # global variable is unaffected
new_globvar  # new_globvar is preserved
loc_var  # local variable is gone!



rm(list=ls())
glob_var <- 1  # define a global variable
probe_scope <- function() {  # explore function scope
  cat('this is the global glob_var:\t', glob_var, '\n')
  glob_var <- 10  # define local 'glob_var' variable
  glob_var <<- 2  # re-define the global variable
  cat('this is a local glob_var:\t', glob_var, '\n')
}  # end probe_scope
probe_scope()
glob_var  # the global variable



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


