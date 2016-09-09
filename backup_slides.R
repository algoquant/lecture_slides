library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
thm <- knit_theme$get("acid")
knit_theme$set(thm)
rm(list=ls())
catch_missing <- function(in_var1, in_var2=NULL) {
# returns the sum of its two arguments
  if (is.numeric(in_var2)) {  # check if in_var2 was input
    in_var1 + in_var2  # in_var2 was input
  } else {
    in_var1  # in_var2 wasn't input
  }
}  # end catch_missing
catch_missing(1, 2)
catch_missing(5, 'a')
catch_missing <- function(in_var1, in_var2) {  # even more robust
  stopifnot((!missing(in_var1) && is.numeric(in_var1)) || 
       (!missing(in_var2) && is.numeric(in_var2)))
  if (!missing(in_var2) && is.numeric(in_var2)) {  # check if in_var2 is valid input
    in_var1 + in_var2  # in_var2 was input
  } else {
    in_var1  # in_var2 wasn't input
  }
}  # end catch_missing
catch_missing(5, 'a')
catch_missing('a')
catch_missing <- function(in_var1=NULL, in_var2=NULL) {
# returns the sum of its two arguments
  if (is.null(in_var2)) {  # check if in_var2 was input
    warning("in_var2 wasn't input")
    in_var1  # in_var2 wasn't input
  } else if (is.null(in_var2)) {
    in_var1 + in_var2  # in_var2 wasn't input
  } else {
    in_var1 + in_var2  # in_var2 was input
  }
}  # end catch_missing
catch_missing(1, 2)
catch_missing(5)
rm(list=ls())
str(tryCatch)  # get arguments of tryCatch()
tryCatch(  # without error handler
  {  # evaluate expressions
    n.val <- 101  # assign
    stop('my error')  # throw error
  }, 
  finally=print(paste("n.val=", n.val))
)  # end tryCatch

tryCatch(  # with error handler
  {  # evaluate expressions
    n.val <- 101  # assign
    stop('my error')  # throw error
  }, 
  error=function(e.cond)  # handler captures error condition
    print(paste("error handler: ", e.cond)),
  finally=print(paste("n.val=", n.val))
)  # end tryCatch
poisson_var <- rpois(100, 4)  # produce 100 Poisson random numbers
pois_table <- table(poisson_var)  # calculate contingency table
pois_table <- pois_table/sum(pois_table)  # calculate frequency table
pois_table
names(pois_table)  # get names of table

# open Windows graphics device
x11(width=11, height=7, title="function plot")

# create histogram of Poisson variables
library(MASS)
truehist(poisson_var, nbins="FD", col="blue", xlab="No. of events", ylab="Frequency of events", main="Poisson histogram")
# add Poisson density
x_var <- 0:max(poisson_var)
lines(x=x_var, y=dpois(x_var, lambda=4), lwd=4, col="red")
# add legend
legend(x="topright", legend="Poisson density",
       title="", inset=0.05, cex=1.0, bg="white",
       lwd=4, lty=1, col="red")

graphics.off()  # close all graphics devices
rm(list=ls())
poisson_func <- function(x, lambda) 
            {exp(-lambda)*lambda^x/factorial(x)}
poisson_events <- 0:11  # Poisson events
poisson_dist <- dpois(poisson_events, lambda=4)  # Poisson probabilities
names(poisson_dist) <- as.character(poisson_events)
poisson_dist
x11(width=11, height=7, title="function plot")
barplot(poisson_dist, ylab="Frequency of events", xlab="No. of events", main="Poisson distribution")
curve(expr=poisson_func(x, lambda=4), add=TRUE, xlim=c(0, 11), ylab="", lwd=2, col="red")
legend(x="topright", legend="Poisson density", title="", 
       inset=0.1, cex=1.0, bg="white", lwd=4, lty=1, col="red")
graphics.off()  # close all graphics devices
rm(list=ls())
type <- c('rose', 'daisy', 'tulip')  # character vector
color <- c('red', 'white', 'yellow')  # character vector
price <- c(1.5, 0.5, 1.0)  # numeric vector
df.florist <- data.frame(type,  # create a data frame
                   color, price)
rownames(df.florist) <- c('flower1',  # assign rownames
                    'flower2', 'flower3')
df.florist
dim(df.florist)  # get dimension attribute
colnames(df.florist)  # get the colnames attribute
rownames(df.florist)  # get the rownames attribute
class(df.florist)  # get object class
class(df.florist$type)  # get column class
class(df.florist$price)  # get column class
type <- c('rose', 'daisy', 'tulip')  # character vector
color <- c('red', 'white', 'yellow')  # character vector
price <- c(1.5, 0.5, 1.0)  # numeric vector
df.florist <- data.frame(type,  # create a data frame
                   color, price)
rownames(df.florist) <- c('flower1',  # assign rownames
                    'flower2', 'flower3')
df.florist
df.florist[2, ]  # get second row
df.florist[2, 3]  # get second row and third column
df.florist[[3]]  # get third column
df.florist$color[3]  # get third row from column 'color'
type <- c('rose', 'daisy', 'tulip')  # character vector
color <- c('red', 'white', 'yellow')  # character vector
price <- c(1.5, 0.5, 1.0)  # numeric vector
df.florist <- data.frame(type,  # create a data frame
                   color, price)
rownames(df.florist) <- c('flower1',  # assign rownames
                    'flower2', 'flower3')
df.florist
str(df.florist)  # display the object structure
dim(cars)  # the cars data frame has 50 rows
head(cars, n=5)  # get first five rows
tail(cars, n=5)  # get last five rows
data_frame <- data.frame(
        type=c('rose', 'daisy', 'tulip'), 
        color=c('red', 'white', 'yellow'), 
        price=c(1.5, 0.5, 1.0), 
        row.names=c('flower1', 'flower2', 'flower3')
        )  # end data_frame
mat_var <- matrix(1:6, nrow=2, ncol=3, dimnames=list(c("row1", "row2"), c("col1", "col2", "col3")))
# write data frame to text file, and then read it back
write.table(data_frame, file='florist.txt')
data_frame <- read.table(file='florist.txt')
data_frame  # a data frame

mat_var  # a matrix
class(mat_var)
# write matrix to text file, and then read it back
write.table(mat_var, file='matrix.txt')
mat_var <- read.table(file='matrix.txt')
mat_var  # write.table() coerced matrix to data frame
class(mat_var)
# coerce from data frame back to matrix
mat_var <- as.matrix(mat_var)
class(mat_var)
data_frame <- data.frame(small=c(3, 5), medium=c(9, 11), large=c(15, 13))
setwd("C:/Develop/data")
data_frame <- read.table("mydata.txt", header=TRUE)
data_frame <- read.table("clipboard", header=TRUE)

write.table(x=data_frame, file="clipboard", sep="\t")

# wrapper function for copying data frame from clipboard into R
# by default, data is tab delimited, with a header
read_clip <- function(file="clipboard", sep="\t", 
              header=TRUE, ...) {
  read.table(file=file, sep=sep, header=header, ...)
}  # end read_clip

data_frame <- read_clip()

# wrapper function for copying data frame from R into clipboard
# by default, data is tab delimited, with a header
write.clip <- function(data, row.names=FALSE, 
               col.names=TRUE, ...) {
  write.table(x=data, file="clipboard", sep="\t", 
      row.names=row.names, col.names=col.names, ...)
}  # end write.clip

write.clip(data=data_frame)

# launch spreadsheet-style data editor
# data_frame <- edit(data_frame)
data_frame <- data.frame(
        type=c('rose', 'daisy', 'tulip'), 
        color=c('red', 'white', 'yellow'), 
        price=c(1.5, 0.5, 1.0), 
        row.names=c('flower1', 'flower2', 'flower3')
        )  # end data_frame
mat_var <- matrix(1:6, nrow=2, ncol=3, dimnames=list(c("row1", "row2"), c("col1", "col2", "col3")))
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
mat_var <- matrix(1:6, nrow=2, ncol=3, dimnames=list(c("row1", "row2"), c("col1", "col2", "col3")))
mat_var  # a matrix
# introduce bad data element
mat_var["row2", "col2"] <- "."
# write data frame to CSV file, and then read it back
write.csv(mat_var, file='matrix.csv')
mat_var <- read.csv(file='matrix.csv')

# restore rownames
rownames(mat_var) <- as.character(mat_var[, 1])
mat_var <- mat_var[, -1]  # remove extra column
# coerce from data frame back to matrix
mat_var <- as.matrix(mat_var)
class(mat_var)
mat_var  # a matrix

class(mat_var[, "col2"])  # numeric coerced to factor by bad data
mat_var$price <- as.numeric(  # explicitly coerce to numeric
                      as.character(mat_var$price))
mat_var$price[2] <- 0.5  # fix value
mean(mat_var$price)
mat_var <- matrix(1:6, nrow=2, ncol=3,  # create a matrix without rownames
          dimnames=list(NULL, c("col1", "col2", "col3")))
mat_var  # matrix without rownames
library('MASS')  # load package 'MASS'
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
library('MASS')  # load package 'MASS'
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
methods("cbind")  # get all methods for function "cbind"
stats::cbind.ts  # cbind isn't exported from package stats
stats:::cbind.ts  # view the non-visible function
getAnywhere("cbind.ts")
plot.xts  # package xts isn't loaded and attached
head(xts::plot.xts)
library(MASS)  # load package 'MASS'
select  # code of primitive function from package 'MASS'
par(oma=c(15, 1, 1, 1), mgp=c(2, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# calculate DAX percentage returns
dax_rets <- diff(log(EuStockMarkets[, 1]))
par(mfrow=c(2,1))  # set plot panels
library(forecast)  # load forecast
# autocorrelation from "stats"
acf(dax_rets, lag=5, xlab=NA)
title(main="acf (base package)", line=-1)
# autocorrelation from "forecast"
Acf(dax_rets, lag=5, xlab=NA)
title(main="acf (forecast package)", line=-1)
library(zoo)  # load zoo
library(ggplot2)  # load ggplot2
library(gridExtra)  # load gridExtra
# coerce DAX time series to zoo
dax_zoo <- as.zoo(EuStockMarkets)[, 1]
index(dax_zoo) <-  # index to class 'Dates'
  as.Date(365*(index(dax_zoo)-1970))
# filter past values only (sides=1)
dax_filt <- filter(dax_zoo, 
             filter=rep(1/5,5), sides=1)
dax_filt <- zoo(coredata(dax_filt), 
          order.by=index(dax_zoo))
dax_filt <- merge(dax_zoo, dax_filt)
dax_filt <- na.omit(dax_filt)
colnames(dax_filt) <- c("DAX", "DAX filtered")
dax_data <- window(dax_filt, 
             start=as.Date("1997-01-01"), 
             end=as.Date("1998-01-01"))
autoplot(  # plot ggplot2
    dax_data, main="Filtered DAX", facets=NULL
) +  # end autoplot
xlab("") + ylab("") +
theme(  # modify plot theme
    legend.position=c(0.1, 0.5), 
    plot.title=element_text(vjust=-2.0), 
    plot.margin=unit(c(-0.5, 0.0, -0.5, 0.0), "cm"), 
    plot.background=element_blank(),
    axis.text.y=element_blank()
    )  # end theme
# end ggplot2
rm(list=ls())
set.seed(1121)  # reset random number generator
library(zoo)  # load zoo
library(ggplot2)  # load ggplot2
library(scales)  # load scales
# coerce mts object into zoo
zoo_series <- as.zoo(EuStockMarkets)
# coerce index into class 'Dates'
index(zoo_series) <- as.Date(365*(index(zoo_series)-1970))
# create simple ggplot2 in single pane
gg2.zoo <- autoplot(zoo_series, main="Eu Stox", 
              facets=NULL)
# customize ggplot2 using 'theme' function
gg2.zoo + xlab("") + 
  theme(legend.position=c(0.1, 0.5), 
  plot.title=element_text(vjust=-2.0), 
  plot.background=element_blank())

rm(list=ls())
library(zoo)  # load zoo
library(ggplot2)  # load ggplot2
library(gridExtra)
# coerce to zoo
zoo_series <- as.zoo(EuStockMarkets)
# coerce index into class 'Dates'
index(zoo_series) <- 
  as.Date(365*(index(zoo_series)-1970))
# plot ggplot2 in single pane
ggp.zoo1 <- autoplot(zoo_series, main="Eu Stox", 
   facets=NULL) + xlab("") + 
  theme(legend.position=c(0.1, 0.5), 
  plot.title=element_text(vjust=-2.0), 
  plot.margin=unit(c(-0.5, 0.0, -0.5, 0.0), "cm"), 
  plot.background=element_blank(),
  axis.text.y=element_blank()
  )
# plot ggplot2 in multiple panes
ggp.zoo2 <- autoplot(zoo_series, main="Eu Stox", 
   facets=Series ~ .) + xlab("") + 
  theme(legend.position=c(0.1, 0.5), 
  plot.title=element_text(vjust=-2.0), 
  plot.margin=unit(c(-0.5, 0.0, -0.5, 0.0), "cm"), 
  plot.background=element_blank(),
  axis.text.y=element_blank()
  )
# create plot ggplot2 in multiple panes
grid.arrange(ggp.zoo1, ggp.zoo2, ncol=1)
library(zoo)  # load zoo
library(ggplot2)  # load ggplot2
library(gridExtra)  # load gridExtra
library(forecast)  # load forecast
# calculate DAX percentage returns
dax_rets <- diff(log(EuStockMarkets[, 1]))
# autocorrelation from "stats"
dax_acf <- acf(dax_rets, plot=FALSE)
str(dax_acf)  # get the structure of the "acf" object
dim(dax_acf$acf)
dim(dax_acf$lag)
head(dax_acf$acf)
# wrapper for base acf()
acf_plus <- function (ts_data, plot=TRUE, xlab="", 
                ylab="", main="", ...) {
  acf_data <- acf(x=ts_data, plot=FALSE, ...)
  acf_data$acf <-  # remove first element
    array(data=acf_data$acf[-1], 
    dim=c((dim(acf_data$acf)[1]-1), 1, 1))
  acf_data$lag <-  # remove first element
    array(data=acf_data$lag[-1], 
    dim=c((dim(acf_data$lag)[1]-1), 1, 1))
  if(plot) {
    kurtosis <- sum(((ts_data - mean(ts_data))/
               sd(ts_data))^4)/length(ts_data)
    ci <- qnorm((1 + 0.95)/2)*sqrt(kurtosis/length(ts_data))
    ylim <- max(ci, max(abs(range(acf_data$acf[-1, , 1]))))
    plot(acf_data, xlab=xlab, ylab=ylab, ylim=c(-ylim, ylim), main=main, ci=0)
    abline(h=c(-ci, ci), col="blue", lty=2)
  }
  invisible(acf_data)
}  # end acf_plus
par(mar=c(5,0,1,2), oma=c(1,2,1,0), mgp=c(2,1,0), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
library(zoo)  # load package zoo
# autocorrelation from "stats"
acf(coredata(dax_rets), lag=5, main="")
title(main="acf of DAX returns", line=-1)
library(zoo)  # load package zoo
dax_acf <- acf(coredata(dax_rets), plot=FALSE)
summary(dax_acf)  # get the structure of the "acf" object
# print(dax_acf)  # print acf data
dim(dax_acf$acf)
dim(dax_acf$lag)
head(dax_acf$acf)
acf_plus <- function (ts_data, plot=TRUE, 
                xlab="Lag", ylab="", 
                main="", ...) {
  acf_data <- acf(x=ts_data, plot=FALSE, ...)
# remove first element of acf data
  acf_data$acf <-  array(data=acf_data$acf[-1], 
    dim=c((dim(acf_data$acf)[1]-1), 1, 1))
  acf_data$lag <-  array(data=acf_data$lag[-1], 
    dim=c((dim(acf_data$lag)[1]-1), 1, 1))
  if(plot) {
    kurtosis <- sum(((ts_data-mean(ts_data))/
         sd(ts_data))^4)/length(ts_data)
    ci <- qnorm((1+0.95)/2)*sqrt(kurtosis/length(ts_data))
    ylim <- c(min(-ci, range(acf_data$acf[-1, , 1])),
        max(ci, range(acf_data$acf[-1, , 1])))
    plot(acf_data, xlab=xlab, ylab=ylab, 
   ylim=ylim, main=main, ci=0)
    abline(h=c(-ci, ci), col="blue", lty=2)
  }
  invisible(acf_data)  # return invisibly
}  # end acf_plus
