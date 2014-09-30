

library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
thm <- knit_theme$get("acid")
knit_theme$set(thm)



rm(list=ls())
library(zoo)  # load zoo
library(RQuantLib)  # load RQuantLib

# create daily date series of class 'Date'
date_index <- Sys.Date() + -5:2
date_index

# create logical vector of business days
bus.days <- isBusinessDay(  # RQuantLib calendar
  calendar="UnitedStates/GovernmentBond", date_index)

# create daily series of business days
bus_index <- date_index[bus.days]
bus_index



par(oma=c(1, 1, 1, 1), mgp=c(2, 1, 0), mar=c(5, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# calculate percentage returns
ts_rets <- diff(log(EuStockMarkets[, 1]))
# create normal Q-Q plot
qqnorm(ts_rets, ylim=c(-0.04, 0.04), 
       xlab='Normal Quantiles', main='')
# fit a line to the normal quantiles
qqline(ts_rets, col='red', lwd=2)
plot_title <- paste(colnames(EuStockMarkets)[1], 
          'Q-Q Plot')
title(main=plot_title, line=-1)  # add title
shapiro.test(ts_rets)  # Shapiro-Wilk test



rm(list=ls())
set.seed(1121)  # initialize the random number generator
par(oma=c(1, 1, 1, 1))  # set outer margins
par(mgp=c(2, 0.5, 0))  # set axis title and labels
par(mar=c(5, 1, 1, 1))  # set plot margins
par(cex.lab=0.8,  # set font scales
    cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
library(zoo)  # load package zoo
# create daily dates
date_index <- as.Date("2010-06-15") + 1:1000
# create zoo time series
zoo_series <- zoo(cumsum(rnorm(1000)), 
           order.by=date_index)
class(zoo_series)  # class 'zoo'
tail(zoo_series, 4)  # get last few elements
write.zoo(zoo_series, file="zoo_series.txt")
rm(zoo_series)  # remove zoo object
zoo_series <- read.zoo("zoo_series.txt")  # read it back
tail(zoo_series, 4)  # get last few elements
# call plot.zoo
plot(zoo_series, xlab="", ylab="")
title(main="Random Prices", line=-1)  # add title



rm(list=ls())
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



rm(list=ls())
set.seed(1121)  # initialize the random number generator
par(oma=c(1, 1, 1, 1))  # set outer margins
par(mgp=c(2, 0.5, 0))  # set axis title and labels
par(mar=c(5, 1, 1, 1))  # set plot margins
par(cex.lab=0.8,  # set font scales
    cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
library(zoo)  # load package zoo
# create daily date series of class 'Date'
date_index1 <- Sys.Date() + 0:365
# create zoo time series
zoo_series1 <- zoo(rnorm(length(date_index1)), 
           order.by=date_index1)
# create another zoo time series
date_index2 <- Sys.Date() + 365:730
zoo_series2 <- zoo(rnorm(length(date_index2)), 
           order.by=date_index2)
# rbind the two time series
zoo_series3 <- rbind(zoo_series1,  # ts1 supersedes ts2
           zoo_series2[index(zoo_series2) > 
                     end(zoo_series1)])
plot(cumsum(zoo_series3), xlab="", ylab="")
# add vertical line at stitch point
abline(v=index(tail(zoo_series1, 1)), col="red", lty="dashed")
title(main="Random Prices", line=-1)  # add title



rm(list=ls())
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
merge(zoo_series1, zoo_series2, all=FALSE)  # intersection of dates



rm(list=ls())
library(zoo)  # load package zoo
# create zoo time series
zoo_series <- zoo(rnorm(4), order.by=(Sys.Date() + 0:3))
zoo_series[3] <- NA
zoo_series

na.locf(zoo_series)  # replace NA's using locf

na.omit(zoo_series)  # remove NA's using omit



rm(list=ls())
set.seed(1121)  # initialize the random number generator
par(oma=c(1, 1, 1, 1))  # set outer margins
par(mgp=c(2, 0.5, 0))  # set axis title and labels
par(mar=c(5, 1, 1, 1))  # set plot margins
par(cex.lab=0.8,  # set font scales
    cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
library(zoo)  # load package zoo
# create zoo time series
date_index <- Sys.Date() + 0:365
zoo_series <- zoo(rnorm(length(date_index)), 
           order.by=date_index)
# create monthly dates
dates_agg <- as.Date(as.yearmon(index(zoo_series)))
# perform monthly 'mean' aggregation
zoo_agg <- aggregate(zoo_series, by=dates_agg, 
               FUN=mean)
# merge with original zoo - union of dates
zoo_agg <- merge(zoo_series, zoo_agg)
# replace NA's using locf
zoo_agg <- na.locf(zoo_agg)
# extract aggregated zoo
zoo_agg <- zoo_agg[index(zoo_series), 2]
# plot original and aggregated zoo
plot(cumsum(zoo_series), xlab="", ylab="")
lines(cumsum(zoo_agg), lwd=2, col="red")
# add legend
legend("topright", inset=0.05, cex=0.8, title="Aggregated Prices", 
 leg=c("orig prices", "agg prices"), lwd=2, 
 col=c("black", "red"))



set.seed(1121)  # initialize the random number generator
par(oma=c(1, 1, 1, 1))  # set outer margins
par(mgp=c(2, 0.5, 0))  # set axis title and labels
par(mar=c(5, 1, 1, 1))  # set plot margins
par(cex.lab=0.8,  # set font scales
    cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# perform monthly 'mean' aggregation
zoo_agg <- aggregate(zoo_series, by=dates_agg, 
               FUN=mean)
# merge with original zoo - union of dates
zoo_agg <- merge(zoo_series, zoo_agg)
# replace NA's using linear interpolation
zoo_agg <- na.approx(zoo_agg)
# extract interpolated zoo
zoo_agg <- zoo_agg[index(zoo_series), 2]
# plot original and interpolated zoo
plot(cumsum(zoo_series), xlab="", ylab="")
lines(cumsum(zoo_agg), lwd=2, col="red")
# add legend
legend("topright", inset=0.05, cex=0.8, title="Interpolated Prices", 
 leg=c("orig prices", "interpol prices"), lwd=2, 
 col=c("black", "red"))



set.seed(1121)  # initialize the random number generator
par(oma=c(1, 1, 1, 1))  # set outer margins
par(mgp=c(2, 0.5, 0))  # set axis title and labels
par(mar=c(5, 1, 1, 1))  # set plot margins
par(cex.lab=0.8,  # set font scales
    cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# perform monthly 'mean' aggregation
zoo_mean <- rollapply(zoo_series, width=11, FUN=mean)
# merge with original zoo - union of dates
zoo_mean <- merge(zoo_series, zoo_mean)
# replace NA's using na.locf
zoo_mean <- na.locf(zoo_mean, fromLast=TRUE)
# extract mean zoo
zoo_mean <- zoo_mean[index(zoo_series), 2]
# plot original and interpolated zoo
plot(cumsum(zoo_series), xlab="", ylab="")
lines(cumsum(zoo_mean), lwd=2, col="red")
# add legend
legend("topright", inset=0.05, cex=0.8, title="Mean Prices", 
 leg=c("orig prices", "mean prices"), lwd=2, 
 col=c("black", "red"))



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



v.type <- c('rose', 'daisy', 'tulip')  # character vector
v.color <- c('red', 'white', 'yellow')  # character vector
v.price <- c(1.5, 0.5, 1.0)  # numeric vector
df.florist <- data.frame(v.type,  # create a data frame
                   v.color, v.price)
rownames(df.florist) <- c('flower1',  # assign rownames
                    'flower2', 'flower3')
write.csv(df.florist, file='florist.csv')  # write to file
df.bad <- read.csv(file='badflorist.csv')  # read from file
df.bad  # v.price has bad data point
class(df.bad$v.price)  # numeric coerced to factor by bad data
df.bad$v.price <- as.numeric(  # explicitly coerce to numeric
                      as.character(df.bad$v.price))
df.bad$v.price[2] <- 0.5  # fix value
mean(df.bad$v.price)



my_data <- data.frame(small=c(3, 5), medium=c(9, 11), large=c(15, 13))
setwd("C:/Develop/data")
my_data <- read.table("mydata.txt", header=TRUE)
my_data <- read.table("clipboard", header=TRUE)

write.table(x=my_data, file="clipboard", sep="\t")

# wrapper function for copying data frame from clipboard into R
# by default, data is tab delimited, with a header
read_clip <- function(file="clipboard", sep="\t", 
              header=TRUE, ...) {
  read.table(file=file, sep=sep, header=header, ...)
}  # end read_clip

my_data <- read_clip()

# wrapper function for copying data frame from R into clipboard
# by default, data is tab delimited, with a header
write.clip <- function(data, row.names=FALSE, 
               col.names=TRUE, ...) {
  write.table(x=data, file="clipboard", sep="\t", 
      row.names=row.names, col.names=col.names, ...)
}  # end write.clip

write.clip(data=my_data)

# launch spreadsheet-style data editor
# my_data <- edit(my_data)



mat_var <- matrix(1:6, nrow=2, ncol=3, dimnames=list(c("row1", "row2"), c("col1", "col2", "col3")))  # create a matrix
mat_var
write.csv(mat_var, file='matrix.csv')  # write to file
mat_in <- read.csv(file='matrix.csv')  # read from file
mat_in  # this is a data frame!
class(mat_in)  # this is a data frame!
name.rows <- as.character(mat_in[, 1])  # get rownames
mat_in <- as.matrix(mat_in[, -1])  # coerce to matrix
rownames(mat_in) <- name.rows  # restore rownames
mat_in



mat_var <- matrix(1:6, nrow=2, ncol=3,  # create a matrix without rownames
          dimnames=list(NULL, c("col1", "col2", "col3")))
mat_var  # matrix without rownames
library('MASS')  # load library 'MASS'
write.matrix(mat_var, file='matrix.csv',  # write to CSV file by row - it's very SLOW!!!
                         sep=',')
system.time(mat_in <- scan(file='matrix.csv',  # skip first line with colnames
                    sep=',', skip=1, what=numeric()))
mat_in  # this is a vector!
mat_in <- matrix(mat_in, nrow=2, byrow=TRUE)  # coerce by row to matrix
#        dim(mat_in) <- c(2, 3)  # coerce by column to matrix!
col_names <- readLines(con='matrix.csv', n=1)  # read colnames
col_names  # this is a string!
col_names <- strsplit(col_names, s=',')[[1]]  # convert to char vector
colnames(mat_in) <- col_names  # restore colnames
mat_in



rm(list=ls())
MyFunc <- function(my_var1, my_var2) {  # define function MyFunc
  2*my_var1  # just multiply first argument
}  # end MyFunc
MyFunc(3, 2)  # match arguments by position
MyFunc(3)  # second argument was never evaluated!
MyFunc <- function(my_var1, my_var2) {  # define function MyFunc
  cat(my_var1, '\n')  # write to output
  cat(my_var2)  # write to output
}  # end MyFunc
MyFunc(3, 2)  # match arguments by position
MyFunc(3)  # first argument written to output



rm(list=ls())
MySum <- function(my_var1=2, my_var2=1, ...) {  # define function MySum
# default values can be specified in the argument list
  my_var1 + 2*my_var2 + sum(...)
# the function returns the last evaluated statement
}  # end MySum
MySum(3, 2)  # match arguments by position
MySum(3, 2, 5, 8)  # extra arguments
MySum()  # use default value of arguments
str(paste)  # function 'paste' can take many arguments
paste('a', 'b', sep = ':')  # match arguments by name
paste('a', 'b', se = ':')  # partial name matching fails!



rm(list=ls())
SumDots <- function(my_var, ...) {  # define recursive function
# returns the sum of its argument list
  if (length(list(...)) == 0) {
    return(my_var)  # just one argument left
  } else {
    my_var + SumDots(...)  # sum remaining arguments
  }
}  # end SumDots
SumDots(1, 2, 3, 4)



rm(list=ls())
glob_var <- 1  # define a global variable
ls(environment())  # get all variables in environment
func_env <- function() {  # function for exploring environments
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
func_scope <- function() {  # function for exploring scope
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
}  # end func_scope
func_scope()
glob_var  # global variable is unaffected
new_globvar  # new_globvar is preserved
loc_var  # local variable is gone!



rm(list=ls())
glob_var <- 1  # define a global variable
func_scope <- function() {  # function for exploring scope
  cat('this is the global glob_var:\t', glob_var, '\n')
  glob_var <- 10  # define local 'glob_var' variable
  glob_var <<- 2  # re-define the global variable
  cat('this is a local glob_var:\t', glob_var, '\n')
}  # end func_scope
func_scope()
glob_var  # the global variable


