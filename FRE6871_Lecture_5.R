library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
options(digits=3)
thm <- knit_theme$get("acid")
knit_theme$set(thm)
# create list of vectors
li_st <- lapply(1:3, function(x) sample(6))
# bind list elements into matrix - doesn't work
rbind(li_st)
# bind list elements into matrix - tedious
rbind(li_st[[1]], li_st[[2]], li_st[[3]])
# bind list elements into matrix - works!
do.call(rbind, li_st)
# create numeric list
li_st <- list(1, 2, 3, 4)
do.call(rbind, li_st)  # returns single column matrix
do.call(cbind, li_st)  # returns single row matrix
# recycling rule applied
do.call(cbind, list(1:2, 3:5))
# NULL element is skipped
do.call(cbind, list(1, NULL, 3, 4))
# NA element isn't skipped
do.call(cbind, list(1, NA, 3, 4))
library(microbenchmark)
list_vectors <- lapply(1:5, rnorm, n=10)
mat_rix <- do.call(rbind, list_vectors)
dim(mat_rix)
do_call_rbind <- function(li_st) {
  while (length(li_st) > 1) {
# index of odd list elements
    odd_index <- seq(from=1, to=length(li_st), by=2)
# bind odd and even elements, and divide li_st by half
    li_st <- lapply(odd_index, function(in_dex) {
if (in_dex==length(li_st)) return(li_st[[in_dex]])
rbind(li_st[[in_dex]], li_st[[in_dex+1]])
    })  # end lapply
  }  # end while
# li_st has only one element - return it
  li_st[[1]]
}  # end do_call_rbind
identical(mat_rix, do_call_rbind(list_vectors))
library(microbenchmark)
airquality[(airquality$Solar.R>320 &
        !is.na(airquality$Solar.R)), ]
subset(x=airquality, subset=(Solar.R>320))
summary(microbenchmark(
    subset=subset(x=airquality, subset=(Solar.R>320)),
    brackets=airquality[(airquality$Solar.R>320 &
            !is.na(airquality$Solar.R)), ],
times=10))[, c(1, 4, 5)]  # end microbenchmark summary
unique(iris$Species)  # Species has three distinct values
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
# split mtcars data frame based on number of cylinders
split_cars <- split(mtcars, mtcars$cyl)
str(split_cars, max.level=1)
names(split_cars)
# mean mpg for each cylinder group
sapply(split_cars, function(x) mean(x$mpg))
# function aggregate() performs split-apply-combine
aggregate(formula=(mpg ~ cyl), data=mtcars, FUN=mean)
# aggregate() all columns
aggregate(x=mtcars, by=list(cyl=mtcars$cyl), FUN=mean)
# mean mpg for each cylinder group
tapply(X=mtcars$mpg, INDEX=mtcars$cyl, FUN=mean)
# using with() environment
with(mtcars,
     tapply(X=mpg, INDEX=cyl, FUN=mean))
# function sapply() instead of tapply()
with(mtcars,
     sapply(sort(unique(cyl)), function(x) {
       structure(mean(mpg[x==cyl]), names=x)
       }, USE.NAMES=TRUE))  # end with

# function by() instead of tapply()
with(mtcars,
     by(data=mpg, INDICES=cyl, FUN=mean))
# get several mpg stats for each cylinder group
data_cars <- sapply(split_cars,
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
# do.call flattens list into a matrix
do.call(cbind, data_cars)
rm(list=ls())
# get base environment
baseenv()
# get global environment
globalenv()
# get current environment
environment()
# get environment class
class(environment())
# define variable in current environment
glob_var <- 1
# get objects in current environment
ls(environment())
# create new environment
new_env <- new.env()
# get calling environment of new environment
parent.env(new_env)
# assign Value to Name
assign("new_var1", 3, envir=new_env)
# create object in new environment
new_env$new_var2 <- 11
# get objects in new environment
ls(new_env)
# get objects in current environment
ls(environment())
# environments are subset like lists
new_env$new_var1
# environments are subset like lists
new_env[["new_var1"]]
search()  # get search path for R objects
my_list <- 
  list(flowers=c("rose", "daisy", "tulip"), 
       trees=c("pine", "oak", "maple"))
my_list$trees
attach(my_list)
trees
search()  # get search path for R objects
detach(my_list)
head(trees)  # "trees" is in datasets base package
library(HighFreq)  # load package HighFreq
# ETF symbols
sym_bols <- c("VTI", "VEU", "IEF", "VNQ")
# extract and merge all data, subset by sym_bols
price_s <- do.call(merge,
  as.list(rutils::env_etf)[sym_bols])
# extract and merge adjusted prices, subset by sym_bols
price_s <- do.call(merge,
  lapply(as.list(rutils::env_etf)[sym_bols], Ad))
# same, but works only for OHLC series
price_s <- do.call(merge,
  eapply(rutils::env_etf, Ad)[sym_bols])
# drop ".Adjusted" from colnames
colnames(price_s) <-
  sapply(colnames(price_s),
    function(col_name)
strsplit(col_name, split="[.]")[[1]])[1, ]
tail(price_s[, 1:2], 3)
# which objects in global environment are class xts?
unlist(eapply(globalenv(), is.xts))

# save xts to csv file
write.zoo(price_s,
     file='etf_series.csv', sep=",")
# copy price_s into env_etf and save to .RData file
assign("price_s", price_s, envir=env_etf)
save(env_etf, file='etf_data.RData')
# "trees" is in datasets base package
head(trees, 3)
colnames(trees)
mean(Girth)
mean(trees$Girth)
with(trees, 
     c(mean(Girth), mean(Height), mean(Volume)))
library(lubridate)  # load lubridate
set.seed(1121)  # reset random number generator
# create daily time series ending today
start_date <- decimal_date(Sys.Date()-6)
end_date <- decimal_date(Sys.Date())
# create vector of geometric Brownian motion
da_ta <- exp(cumsum(rnorm(6)/100))
fre_quency <- length(da_ta)/(end_date-start_date)
ts_series <- ts(data=da_ta,
          start=start_date,
          frequency=fre_quency)
ts_series  # display time series
# display index dates
as.Date(date_decimal(coredata(time(ts_series))))
# bi-monthly geometric Brownian motion starting mid-1990
ts_series <- ts(data=exp(cumsum(rnorm(96)/100)),
       frequency=6, start=1990.5)
# show some methods for class "ts"
matrix(methods(class="ts")[3:8], ncol=2)
# "tsp" attribute specifies the date-time index
attributes(ts_series)
# extract the index
tail(time(ts_series), 11)
# the index is equally spaced
diff(tail(time(ts_series), 11))
# subset the time series
window(ts_series, start=1992, end=1992.25)
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
plot(ts_series, type="l",  # create plot
     col="red", lty="solid", xlab="", ylab="")
title(main="Random Prices", line=-1)  # add title
class(EuStockMarkets)  # multiple ts object
dim(EuStockMarkets)
head(EuStockMarkets, 3)  # get first three rows
# EuStockMarkets index is equally spaced
diff(tail(time(EuStockMarkets), 4))
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# plot all the columns
plot(EuStockMarkets, main="", xlab="")
# add title
title(main="EuStockMarkets", line=-2)
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# plot in single panel
plot(EuStockMarkets, main="EuStockMarkets",
     xlab="", ylab="", plot.type="single",
     col=c("black", "red", "blue", "green"))
# add legend
legend(x=1992, y=8000,
 legend=colnames(EuStockMarkets),
 col=c("black", "red", "blue", "green"),
 lty=1)
# calculate DAX percentage returns
dax_rets <- diff(log(EuStockMarkets[, 1]))
# mean and standard deviation of returns
c(mean(dax_rets), sd(dax_rets))
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# plot histogram
hist(dax_rets, breaks=30, main="",
     xlim=c(-0.04, 0.04), ylim=c(0, 60),
     xlab="", ylab="", freq=FALSE)
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
# calculate percentage returns
dax_rets <- diff(log(EuStockMarkets[, 1]))
# perform Shapiro-Wilk test
shapiro.test(dax_rets)
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# create normal Q-Q plot
qqnorm(dax_rets, ylim=c(-0.04, 0.04),
 xlab='Normal Quantiles', main='')
# fit a line to the normal quantiles
qqline(dax_rets, col='red', lwd=2)
plot_title <- paste(colnames(EuStockMarkets)[1],
            'Q-Q Plot')
title(main=plot_title, line=-1)  # add title
# boxplot method for formula
boxplot(formula=mpg ~ cyl, data=mtcars,
  main="Mileage by number of cylinders",
  xlab="Cylinders", ylab="Miles per gallon")
# boxplot method for data frame of EuStockMarkets percentage returns
boxplot(x=diff(log(EuStockMarkets)))
set.seed(1121)  # reset random number generator
library(zoo)  # load package zoo
# create zoo time series of random returns
in_dex <- Sys.Date() + 0:3
zoo_series <- zoo(rnorm(length(in_dex)),
         order.by=in_dex)
zoo_series
attributes(zoo_series)
class(zoo_series)  # class 'zoo'
tail(zoo_series, 3)  # get last few elements
library(zoo)  # load package zoo
coredata(zoo_series)  # extract coredata
index(zoo_series)  # extract time index
start(zoo_series)  # first date
end(zoo_series)  # last date
zoo_series[start(zoo_series)]  # first element
zoo_series[end(zoo_series)]  # last element
coredata(zoo_series) <- rep(1, 4)  # replace coredata
cumsum(zoo_series)  # cumulative sum
cummax(cumsum(zoo_series))
cummin(cumsum(zoo_series))
library(zoo)  # load package zoo
coredata(zoo_series) <- (1:4)^2  # replace coredata
zoo_series
lag(zoo_series)  # one day lag
lag(zoo_series, 2)  # two day lag
lag(zoo_series, k=-1)  # proper one day lag
diff(zoo_series)  # diff with one day lag
# proper lag and original length
lag(zoo_series, -2, na.pad=TRUE)
set.seed(1121)  # reset random number generator
library(zoo)  # load package zoo
# create index of daily dates
in_dex <- seq(from=as.Date("2014-07-14"),
            by="day", length.out=1000)
# create vector of geometric Brownian motion
zoo_data <- 
  exp(cumsum(rnorm(length(in_dex))/100))
# create zoo series of geometric Brownian motion
zoo_series <- zoo(x=zoo_data, order.by=in_dex)
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# plot using plot.zoo method
plot(zoo_series, xlab="", ylab="")
title(main="Random Prices", line=-1)  # add title
library(zoo)  # load package zoo
# subset zoo as matrix
zoo_series[459:463, 1]
# subset zoo using window()
window(zoo_series,
 start=as.Date("2014-10-15"),
 end=as.Date("2014-10-19"))
# subset zoo using Date object
zoo_series[as.Date("2014-10-15")]
set.seed(1121)  # reset random number generator
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
library(zoo)  # load package zoo
# create daily date series of class 'Date'
in_dex1 <- seq(Sys.Date(), by="days",
             length.out=365)
# create zoo time series of random returns
zoo_series1 <- zoo(rnorm(length(in_dex1)),
           order.by=in_dex1)
# create another zoo time series of random returns
in_dex2 <- seq(Sys.Date()+350, by="days",
             length.out=365)
zoo_series2 <- zoo(rnorm(length(in_dex2)),
           order.by=in_dex2)
# rbind the two time series - ts1 supersedes ts2
zoo_series3 <- rbind(zoo_series1,
           zoo_series2[index(zoo_series2) > end(zoo_series1)])
# plot zoo time series of geometric Brownian motion
plot(exp(cumsum(zoo_series3)/100), xlab="", ylab="")
# add vertical lines at stitch point
abline(v=end(zoo_series1), col="blue", lty="dashed")
abline(v=start(zoo_series2), col="red", lty="dashed")
title(main="Random Prices", line=-1)  # add title
# create daily date series of class 'Date'
in_dex1 <- Sys.Date() + -3:1
# create zoo time series of random returns
zoo_series1 <- zoo(rnorm(length(in_dex1)),
         order.by=in_dex1)
# create another zoo time series of random returns
in_dex2 <- Sys.Date() + -1:3
zoo_series2 <- zoo(rnorm(length(in_dex2)),
         order.by=in_dex2)
merge(zoo_series1, zoo_series2)  # union of dates
# intersection of dates
merge(zoo_series1, zoo_series2, all=FALSE)
library(zoo)  # load package zoo
# create zoo time series
zoo_series <- zoo(sample(4),
            order.by=(Sys.Date() + 0:3))
# add NA
zoo_series[3] <- NA
zoo_series

na.locf(zoo_series)  # replace NA's using locf

na.omit(zoo_series)  # remove NA's using omit
library(lubridate)  # load lubridate
library(zoo)  # load package zoo
# methods(as.zoo)  # many methods of coercing into zoo
class(EuStockMarkets)  # multiple ts object
# coerce mts object into zoo
zoo_series <- as.zoo(EuStockMarkets)
class(index(zoo_series))  # index is numeric
head(zoo_series, 3)
# approximately convert index into class 'Date'
index(zoo_series) <-
  as.Date(365*(index(zoo_series)-1970))
head(zoo_series, 3)
# convert index into class 'POSIXct'
zoo_series <- as.zoo(EuStockMarkets)
index(zoo_series) <- date_decimal(index(zoo_series))
head(zoo_series, 3)
library(lubridate)  # load lubridate
library(zoo)  # load package zoo
set.seed(1121)  # reset random number generator
# create index of daily dates
in_dex <- seq(from=as.Date("2014-07-14"),
            by="day", length.out=1000)
# create vector of geometric Brownian motion
zoo_data <- exp(cumsum(rnorm(length(in_dex))/100))
# create zoo time series of geometric Brownian motion
zoo_series <- zoo(x=zoo_data,
            order.by=in_dex)
head(zoo_series, 3)  # zoo object
# as.ts() creates ts object with frequency=1
ts_series <- as.ts(zoo_series)
tsp(ts_series)  # frequency=1
# get start and end dates of zoo_series
start_date <- decimal_date(start(zoo_series))
end_date <- decimal_date(end(zoo_series))
# calculate frequency of zoo_series
fre_quency <- length(zoo_series)/(end_date-start_date)
da_ta <- coredata(zoo_series)  # extract data from zoo_series
# create ts object using ts()
ts_series <- ts(data=da_ta, start=start_date,
          frequency=fre_quency)
# display start of time series
window(ts_series, start=start(ts_series),
 end=start(ts_series)+4/365)
head(time(ts_series))  # display index dates
head(as.Date(date_decimal(coredata(time(ts_series)))))
library(lubridate)  # load lubridate
library(zoo)  # load package zoo
# create weekday Boolean vector
week_days <- weekdays(index(zoo_series))
is_weekday <- !((week_days == "Saturday") |
  (week_days == "Sunday"))
# remove weekends from zoo time series
zoo_series <- zoo_series[is_weekday, ]
head(zoo_series, 7)  # zoo object
# as.ts() creates NA values
ts_series <- as.ts(zoo_series)
head(ts_series, 7)
# create vector of regular dates, including weekends
in_dex <- seq(from=start(zoo_series),
            by="day",
            length.out=length(zoo_series))
index(zoo_series) <- in_dex
ts_series <- as.ts(zoo_series)
head(ts_series, 7)
set.seed(1121)  # reset random number generator
library(xts)  # load package xts
# create xts time series of random returns
in_dex <- Sys.Date() + 0:3
x_ts <- xts(rnorm(length(in_dex)),
         order.by=in_dex)
names(x_ts) <- "random"
x_ts
tail(x_ts, 3)  # get last few elements
first(x_ts)  # get first element
last(x_ts)  # get last element
class(x_ts)  # class 'xts'
attributes(x_ts)
load(file="C:/Develop/data/zoo_data.RData")
library(xts)  # load package xts
# as.xts() creates xts from zoo
st_ox <- as.xts(zoo_stx_adj)
dim(st_ox)
head(st_ox[, 1:4], 4)
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# plot using plot.xts method
plot(st_ox[, "AdjClose"], xlab="", ylab="", main="")
title(main="MSFT Prices")  # add title
library(xts)  # load xts
library(lubridate)  # load lubridate
# coerce EuStockMarkets into class xts
x_ts <- xts(coredata(EuStockMarkets),
      order.by=date_decimal(index(EuStockMarkets)))
# plot all columns in single panel: xts v.0.9-8
col_ors <- rainbow(NCOL(x_ts))
plot(x_ts, main="EuStockMarkets using xts",
     col=col_ors, major.ticks="years",
     minor.ticks=FALSE)
legend("topleft", legend=colnames(EuStockMarkets),
 inset=0.2, cex=0.7, , lty=rep(1, NCOL(x_ts)),
 lwd=3, col=col_ors, bg="white")
# plot only first column: xts v.0.9-7
plot(x_ts[, 1], main="EuStockMarkets using xts",
     col=col_ors[1], major.ticks="years",
     minor.ticks=FALSE)
# plot remaining columns
for (col_umn in 2:NCOL(x_ts))
  lines(x_ts[, col_umn], col=col_ors[col_umn])
# plot using quantmod
library(quantmod)
plot_theme <- chart_theme()
plot_theme$col$line.col <- col_ors
chart_Series(x=x_ts, theme=plot_theme,
       name="EuStockMarkets using quantmod")
legend("topleft", legend=colnames(EuStockMarkets),
 inset=0.2, cex=0.7, , lty=rep(1, NCOL(x_ts)),
 lwd=3, col=col_ors, bg="white")
library(rutils)
library(ggplot2)
# create ggplot object
etf_gg <- qplot(x=index(env_etf$price_s[, 1]),
          y=as.numeric(env_etf$price_s[, 1]),
          geom="line",
          main=names(env_etf$price_s[, 1])) +
  xlab("") + ylab("") +
  theme(  # add legend and title
    legend.position=c(0.1, 0.5),
    plot.title=element_text(vjust=-2.0),
    plot.background=element_blank()
  )  # end theme
# render ggplot object
etf_gg
library(rutils)  # load xts time series data
library(reshape2)
library(ggplot2)
# create data frame of time series
data_frame <-
  data.frame(dates=index(env_etf$price_s),
    coredata(env_etf$price_s[, c("VTI", "IEF")]))
# reshape data into a single column
data_frame <-
  reshape2::melt(data_frame, id="dates")
x11(width=6, height=5)  # open plot window
# ggplot the melted data_frame
ggplot(data=data_frame,
 mapping=aes(x=dates, y=value, colour=variable)) +
 geom_line() +
  xlab("") + ylab("") +
  ggtitle("VTI and IEF") +
  theme(  # add legend and title
    legend.position=c(0.2, 0.8),
    plot.title=element_text(vjust=-2.0)
  )  # end theme
# load rutils which contains env_etf dataset
suppressMessages(suppressWarnings(library(rutils)))
suppressMessages(suppressWarnings(library(dygraphs)))
x_ts <- env_etf$price_s[, c("VTI", "IEF")]
# plot dygraph with date range selector
dygraph(x_ts, main="VTI and IEF prices") %>%
  dyOptions(colors=c("blue","green")) %>%
  dyRangeSelector()
# load rutils which contains env_etf dataset
suppressMessages(suppressWarnings(library(rutils)))
suppressMessages(suppressWarnings(library(plotly)))
# create data frame of time series
data_frame <-
  data.frame(dates=index(env_etf$price_s),
    coredata(env_etf$price_s[, c("VTI", "IEF")]))
# plotly syntax using pipes
data_frame %>%
  plot_ly(x=~dates, y=~VTI, type="scatter", mode="lines+markers", fill="tozeroy", name="VTI") %>%
  add_trace(x=~dates, y=~IEF, type="scatter", mode="lines+markers", fill="tonexty", name="IEF") %>%
  layout(title="VTI and IEF prices",
   xaxis=list(title="Time"),
   yaxis=list(title="Stock Prices"),
   legend=list(x=0.1, y=0.9))
# or use standard plotly syntax
p_lot <- plot_ly(data=data_frame, x=~dates, y=~VTI, type="scatter", mode="lines+markers", fill="tozeroy", name="VTI")
p_lot <- add_trace(p=p_lot, x=~dates, y=~IEF, type="scatter", mode="lines+markers", fill="tonexty", name="IEF")
p_lot <- layout(p=p_lot, title="VTI and IEF prices", xaxis=list(title="Time"), yaxis=list(title="Stock Prices"), legend=list(x=0.1, y=0.9))
p_lot
library(xts)  # load package xts
# subset xts using a date range string
stox_sub <- st_ox["2014-10-15/2015-01-10", 1:4]
first(stox_sub)
last(stox_sub)
# subset Nov 2014 using a date string
stox_sub <- st_ox["2014-11", 1:4]
first(stox_sub)
last(stox_sub)
# subset all data after Nov 2014
stox_sub <- st_ox["2014-11/", 1:4]
first(stox_sub)
last(stox_sub)
# comma after date range not necessary
identical(st_ox["2014-11", ], st_ox["2014-11"])
# benchmark the speed of subsetting
library(microbenchmark)
summary(microbenchmark(
  bracket=sapply(500,
  function(in_dex) max(st_ox[in_dex:(in_dex+10), ])),
  subset=sapply(500,
  function(in_dex) max(xts::.subset_xts(st_ox, in_dex:(in_dex+10)))),
  times=10))[, c(1, 4, 5)]
library(xts)  # load package xts
# vector of 1-minute times (ticks)
min_ticks <- seq.POSIXt(
  from=as.POSIXct("2015-04-14", tz="America/New_York"),
  to=as.POSIXct("2015-04-16"),
  by="min")
# xts of 1-minute times (ticks) of random returns
x_ts <- xts(rnorm(length(min_ticks)),
               order.by=min_ticks)
# subset recurring time interval using "T notation",
x_ts <- x_ts["T09:30:00/T16:00:00"]
first(x_ts["2015-04-15"])  # first element of day
last(x_ts["2015-04-15"])  # last element of day
# suppress timezone warning messages
options(xts_check_tz=FALSE)
library(xts)  # load package xts
str(st_ox)  # display structure of xts
# subsetting zoo to single column drops dim attribute
dim(zoo_stx_adj)
dim(zoo_stx_adj[, 1])
# zoo with single column are vectors not matrices
c(is.matrix(zoo_stx_adj), is.matrix(zoo_stx_adj[, 1]))
# xts always have a dim attribute
rbind(base=dim(st_ox), subs=dim(st_ox[, 1]))
c(is.matrix(st_ox), is.matrix(st_ox[, 1]))
library(xts)  # load package xts
# lag of zoo shortens it by one row
rbind(base=dim(zoo_stx_adj), lag=dim(lag(zoo_stx_adj)))
# lag of xts doesn't shorten it
rbind(base=dim(st_ox), lag=dim(lag(st_ox)))
# lag of zoo is in opposite direction from xts
head(lag(zoo_stx_adj), 4)
head(lag(st_ox), 4)
library(xts)  # load package xts
# lower the periodicity to months
xts_monthly <- to.period(x=st_ox,
             period="months", name="MSFT")
# convert colnames to standard OHLC format
colnames(xts_monthly)
colnames(xts_monthly) <- sapply(
  strsplit(colnames(xts_monthly), split=".", fixed=TRUE),
  function(na_me) na_me[-1]
  )  # end sapply
head(xts_monthly, 3)
# lower the periodicity to years
xts_yearly <- to.period(x=xts_monthly,
             period="years", name="MSFT")
colnames(xts_yearly) <- sapply(
  strsplit(colnames(xts_yearly), split=".", fixed=TRUE),
  function(na_me) na_me[-1]
  )  # end sapply
head(xts_yearly)
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
load(file="C:/Develop/data/zoo_data.RData")
library(xts)  # load package xts
# as.xts() creates xts from zoo
st_ox <- as.xts(zoo_stx_adj)
# subset xts using a date
stox_sub <- st_ox["2014-11", 1:4]

# plot OHLC using plot.xts method
plot(stox_sub, type="candles", main="")
title(main="MSFT Prices")  # add title
load(file="C:/Develop/data/zoo_data.RData")
ts_stx <- as.ts(zoo_stx)
class(ts_stx)
tail(ts_stx[, 1:4])
library(xts)
st_ox <- as.xts(zoo_stx)
class(st_ox)
tail(st_ox[, 1:4])
options(width=50, dev='pdf')
str(optimize)
# objective function with multiple minima
object_ive <- function(in_put, param1=0.01) {
  sin(0.25*pi*in_put) + param1*(in_put-1)^2
}  # end object_ive
unlist(optimize(f=object_ive, interval=c(-4, 2)))
unlist(optimize(f=object_ive, interval=c(0, 8)))
options(width=60, dev='pdf')
par(oma=c(1, 1, 1, 1), mgp=c(2, 1, 0), mar=c(5, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# plot the objective function
curve(expr=object_ive, type="l", xlim=c(-8, 9),
xlab="", ylab="", lwd=2)
# add title
title(main="Objective Function", line=-1)
library(rgl)  # load rgl
# define function of two variables
sur_face <- function(x, y) y*sin(x)
# draw 3d surface plot of function
persp3d(x=sur_face, xlim=c(-5, 5), ylim=c(-5, 5),
  col="green", axes=FALSE)
# draw 3d surface plot of matrix
x_lim <- seq(from=-5, to=5, by=0.1)
y_lim <- seq(from=-5, to=5, by=0.1)
persp3d(z=outer(x_lim, y_lim, FUN=sur_face),
  xlab="x", ylab="y", zlab="sur_face",
  col="green")
# save current view to png file
rgl.snapshot("surface_plot.png")
# define function of two variables and two parameters
sur_face <- function(x, y, lambda_1=1, lambda_2=1)
  sin(lambda_1*x)*sin(lambda_2*y)
# draw 3d surface plot of function
persp3d(x=sur_face, xlim=c(-5, 5), ylim=c(-5, 5),
  col="green", axes=FALSE,
  lambda_1=1, lambda_2=2)
# define object_ive function of one vector argument and two parameters
object_ive <- function(vec_tor, lambda_1=1, lambda_2=1)
  sin(lambda_1*vec_tor[1])*sin(lambda_2*vec_tor[2])
# optimization to find weights with maximum Sharpe ratio
weight_s <- c(pi/6, pi/6)
object_ive(weight_s)
optim_run <- optim(par=weight_s,
           fn=object_ive,
           method="L-BFGS-B",
           upper=c(4*pi, 4*pi),
           lower=c(pi/2, pi/2),
           lambda_1=1, lambda_2=1)
# optimal parameters and value
optim_run$par
optim_run$value
-object_ive(optim_run$par)
# select ETF symbols
sym_bols <- c("IEF", "DBC", "XLF", "XLP", "XLI", "VXX")
# select and de-mean the returns
re_turns <- na.omit(rutils::env_etf$re_turns[, sym_bols])
in_dex <- index(re_turns)
re_turns <- t(t(re_turns) - sapply(re_turns, sum)/NROW(re_turns))
# re_turns <- lapply(re_turns, function(x) {x - sum(x)/NROW(re_turns)})
# re_turns <- do.call(cbind, re_turns)
# re_turns <- scale(re_turns, scale=FALSE)
# covariance matrix and variance vector of the data
cov_mat <- (t(re_turns) %*% re_turns) / (NROW(re_turns)-1)
vari_ance <- diag(cov_mat)
cor_mat <- cov_mat / sqrt(vari_ance)
cor_mat <- t(t(cor_mat) / sqrt(vari_ance))
# cor_mat <- cor(re_turns)
# reorder correlation matrix based on clusters
library(corrplot)
or_der <- corrMatOrder(cor_mat,
        order="hclust",
        hclust.method="complete")
cor_mat <- cor_mat[or_der, or_der]
# plot the correlation matrix
color_ramp <- colorRampPalette(c("red", "white", "blue"))
corrplot(cor_mat, title="Correlation Matrix",
    tl.col="black", tl.cex=0.8, mar=c(0,0,1,0),
    method="square", col=color_ramp(8),
    cl.offset=0.75, cl.cex=0.7,
    cl.align.text="l", cl.ratio=0.25)
# draw rectangles on the correlation matrix plot
corrRect.hclust(cor_mat, k=NROW(cor_mat) %/% 2,
          method="complete", col="red")
# plot the correlation matrix
color_ramp <- colorRampPalette(c("red", "white", "blue"))
corrplot(cor_mat, title="Correlation Matrix",
    tl.col="black", tl.cex=0.8, mar = c(0,0,1,0),
    method="square", col=color_ramp(8),
    cl.offset=0.75, cl.cex=0.7,
    cl.align.text="l", cl.ratio=0.25)
# draw rectangles on the correlation matrix plot
corrRect.hclust(cor_mat, k=NROW(cor_mat) %/% 2,
    method="complete", col="red")
# create initial vector of portfolio weights
n_weights <- NROW(sym_bols)
weight_s <- rep(1/sqrt(n_weights), n_weights)
names(weight_s) <- sym_bols
# objective function equal to minus portfolio variance
object_ive <- function(weight_s, re_turns) {
  portf_rets <- re_turns %*% weight_s
  -sum(portf_rets*portf_rets) +
    1000*(1 - sum(weight_s*weight_s))^2
}  # end object_ive
# objective for equal weight portfolio
object_ive(weight_s, re_turns)
# compare speed of two methods
summary(microbenchmark(
  trans_pose=t(portf_rets) %*% portf_rets,
  s_um=sum(portf_rets*portf_rets),
  times=10))[, c(1, 4, 5)]
# find weights with maximum variance
optim_run <- optim(par=weight_s,
             fn=object_ive,
             re_turns=re_turns,
             method="L-BFGS-B",
             upper=rep(1.0, n_weights),
             lower=rep(-1.0, n_weights))
# optimal weights and maximum variance
weight_s <- optim_run$par
-object_ive(weight_s, re_turns)
# plot first principal component loadings
barplot(weight_s, names.arg=names(weight_s),
  xlab="", ylab="",
  main="first principal component loadings")
# pc1 weights and returns
weights_1 <- weight_s
pc_1 <- re_turns %*% weights_1
# redefine objective function
object_ive <- function(weight_s, re_turns) {
  portf_rets <- re_turns %*% weight_s
  -sum(portf_rets*portf_rets) +
    1000*(1 - sum(weight_s*weight_s))^2 +
    1000*sum(pc_1*portf_rets)^2
}  # end object_ive
# find second principal component weights
optim_run <- optim(par=weight_s,
             fn=object_ive,
             re_turns=re_turns,
             method="L-BFGS-B",
             upper=rep(1.0, n_weights),
             lower=rep(-1.0, n_weights))
# pc2 weights and returns
weights_2 <- optim_run$par
pc_2 <- re_turns %*% weights_2
sum(pc_1*pc_2)
# plot second principal component loadings
barplot(weights_2, names.arg=names(weights_2),
  xlab="", ylab="",
  main="second principal component loadings")
# calculate eigenvectors and eigenvalues
ei_gen <- eigen(cov_mat)
ei_gen$values[1]
var(pc_1)
(cov_mat %*% weights_1) / weights_1
ei_gen$values[2]
var(pc_2)
(cov_mat %*% weights_2) / weights_2
sum(vari_ance)
sum(ei_gen$values)
barplot(ei_gen$values, # plot eigenvalues
  names.arg=paste0("PC", 1:n_weights),
  las=3, xlab="", ylab="", main="PC eigenvalues (variances)")
# perform principal component analysis PCA
p_ca <- prcomp(re_turns,
         center=TRUE, scale=FALSE)
# plot standard deviations
barplot(p_ca$sdev,
  names.arg=colnames(p_ca$rotation),
  las=3, xlab="", ylab="",
  main="PC volatilities")
# principal component loadings (weights)
p_ca$rotation
# plot loading barplots in multiple panels
par(mfrow=c(n_weights/2,2))
par(mar=c(2, 2, 2, 1), oma=c(0, 0, 0, 0))
for (or_der in 1:n_weights) {
  barplot(p_ca$rotation[, or_der],
  las=3, xlab="", ylab="", main="")
  title(paste0("PC", or_der), line=-2.0,
  col.main="red")
}  # end for
# principal component time series
pca_ts <- xts(re_turns %*% p_ca$rotation,
          order.by=in_dex)
pca_ts <- cumsum(pca_ts)
# plot principal component time series in multiple panels
par(mfrow=c(n_weights/2,2))
par(mar=c(2, 2, 0, 1), oma=c(0, 0, 0, 0))
ra_nge <- range(pca_ts)
for (or_der in 1:n_weights) {
  plot.zoo(pca_ts[, or_der],
     ylim=ra_nge,
     xlab="", ylab="")
  title(paste0("PC", or_der), line=-2.0)
}  # end for
# invert principal component time series
pca_rets <- re_turns %*% p_ca$rotation
sol_ved <- pca_rets %*% solve(p_ca$rotation)
all.equal(re_turns, sol_ved)
sol_ved <- pca_rets[, 1:3] %*% solve(p_ca$rotation)[1:3, ]
# plot the solved returns
par(mfrow=c(n_weights/2,2))
par(mar=c(2, 2, 0, 1), oma=c(0, 0, 0, 0))
col_names <- colnames(re_turns)
for (or_der in 1:n_weights) {
  plot.zoo(
    cumsum(as.xts(cbind(re_turns[, or_der], sol_ved[, or_der]))),
    plot.type="single", col=c("black", "blue"), xlab="", ylab="")
  legend(x="topright",
   legend=paste0(col_names[or_der], c("", " solved")),
   title="", inset=0.05, cex=1.0, lwd=c(6, 6),
   lty=c(1, 1), col=c("black", "blue"))
}  # end for
