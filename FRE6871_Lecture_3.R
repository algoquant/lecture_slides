

library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
thm <- knit_theme$get("acid")
knit_theme$set(thm)



rm(list=ls())
TRUE | FALSE
TRUE | NA
my_var1 <- c(2, 4, 6)
my_var1 < 5
(my_var1 < 5) & (my_var1 > 3)
my_var1[(my_var1 < 5) & (my_var1 > 3)]
my_var2 <- c(-10, 0, 10)
my_var1 < my_var2
c(FALSE, TRUE, FALSE) & c(TRUE, TRUE, FALSE)
c(FALSE, TRUE, FALSE) | c(TRUE, TRUE, FALSE)



rm(list=ls())
c(FALSE, TRUE, FALSE) && c(TRUE, TRUE, FALSE)
c(FALSE, TRUE, FALSE) || c(TRUE, TRUE, FALSE)
FuncTrue = function() {cat("FuncTrue\t"); TRUE}
FuncFalse = function() {cat("FuncFalse\t"); FALSE}
FuncTrue() | FuncFalse()
FuncTrue() || FuncFalse()  # FuncFalse() isn't evaluated at all!
my_var <- c(2, 4, 6)
# works (does nothing) using '&&'
if (is.matrix(my_var) && (my_var[2, 3] > 0)) {
  my_var[2, 3] <- 1
}
# no short-circuit so fails (throws an error)
if (is.matrix(my_var) & (my_var[2, 3] > 0)) {
  my_var[2, 3] <- 1
}



rm(list=ls())
my_var1 <- -1
if (my_var1) {  # positive numbers are TRUE, otherwise FALSE
  my_var2 <- 4
} else if (my_var1 == 0) {  # 'else if' together on same line
  my_var2 <- 0
} else {  # keep 'else' together with curly braces
  my_var2 <- -4
}
my_var2



rm(list=ls())
# create two vectors
my_var1 <- sin(0.25*pi*1:10)
my_var2 <- cos(0.25*pi*1:10)
# create third vector using 'ifelse'
my_var3 <- ifelse(my_var1 > my_var2, my_var1, 
         my_var2)
# cbind all three together
my_var4 <- cbind(my_var1, my_var2, my_var3)

# set plotting parameters
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), 
    cex.lab=0.8, cex.axis=0.8, cex.main=0.8, 
    cex.sub=0.5)
# plot matrix
matplot(my_var4, type="l", lty="solid", 
col=c("green", "blue", "red"), 
lwd=c(2, 2, 2), xlab="", ylab="")
# add legend
legend(x="bottomright", legend=colnames(my_var4), 
       title="", inset=0.05, cex=0.8, lwd=2, 
       lty=c(1, 1, 1), col=c("green", "blue", "red"))



rm(list=ls())
my.colors <- list("red", "white", "blue")
for (some.color in my.colors) {  # loop over list
  print(some.color)
}
for (some.index in 1:3) {  # loop over vector
  print(my.colors[[some.index]])
}

some.index <- 1  # 'while' loops need initialization
while (some.index < 4) {  # while loop
  print(my.colors[[some.index]])
  some.index <- some.index + 1
}



rm(list=ls())
fib.seq <- c()  # create empty vector
fib.seq[1] <- 1  # initialize
fib.seq[2] <- 1  # initialize
for (i in 3:10) {  # perform recurrence loop
  fib.seq[i] <- fib.seq[i-1] + fib.seq[i-2]
}  # end for
fib.seq



rm(list=ls())
FibRec <- function(n.num) {
  if (n.num > 2) {
    fib.seq <- FibRec(n.num-1)  # recursion
    c(fib.seq, sum(tail(fib.seq, 2)))  # return this
  } else {
    c(1, 1)  # initialize and return
  }
}  # end FibRec
FibRec(10)
tail(FibRec(10), 2)



rm(list=ls())
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
v.xval <- seq(-5, 7, length=100)
v.yval <- dnorm(v.xval, mean=1.0, sd=2.0)
plot(v.xval, v.yval, type="l", lty="solid", 
     xlab="", ylab="")
title(main="Normal Density Function", line=0.5)
n.low <- 3; n.up <- 5  # set lower and upper bounds
# set polygon base
v.reg <- ((v.xval >= n.low) & (v.xval <= n.up))
polygon(c(n.low, v.xval[v.reg], n.up),  # draw polygon
c(-1, v.yval[v.reg], -1), col="red")



rm(list=ls())
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
v.xval <- seq(-4, 4, length=100)
v.sigma <- c(0.5, 1, 1.5, 2)  # sigma values
# create plot colors
v.colors <- c("red", "black", "blue", "green")
# create legend labels
v.labels <- paste("sigma", v.sigma, sep='=')
# plot an empty chart
plot(v.xval, dnorm(v.xval, sd=v.sigma[1]), 
     type="n", xlab="", ylab="", 
     main="Normal Distributions")
# add lines to plot
for (in_dex in 1:4) {
  lines(v.xval, dnorm(v.xval, sd=v.sigma[in_dex]), 
lwd=2, col=v.colors[in_dex])
}
# add legend
legend("topright", inset=0.05, title="Sigmas", 
       v.labels, cex=0.8, lwd=2, lty=c(1, 1, 1, 1), 
       col=v.colors)



rm(list=ls())
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
v.xval <- seq(0, 20, length=100)
v.df <- c(2, 5, 8, 11)  # df values
# create plot colors
v.colors <- c("red", "black", "blue", "green")
# create legend labels
v.labels <- paste("df", v.df, sep='=')
# plot an empty chart
plot(v.xval, dchisq(v.xval, df=v.df[1]), 
     type="n", xlab="", ylab="", 
     main="Chi-squared Distributions")
# add lines to plot
for (in_dex in 1:4) {
  lines(v.xval, dchisq(v.xval, df=v.df[in_dex]), 
lwd=2, col=v.colors[in_dex])
}
# add legend
legend("topright", inset=0.05, 
       title="Degrees of freedom", v.labels, 
       cex=0.8, lwd=2, lty=c(1, 1, 1, 1), 
       col=v.colors)



rm(list=ls())
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
v.xval <- seq(-5, 5, length=100)
v.df <- c(3, 6, 9)  # df values
# create plot colors
v.colors <- c("black", "red", "blue", "green")
# create legend labels
v.labels <- c('normal', paste("df", v.df, sep='='))
# plot chart of normal distribution
plot(v.xval, dnorm(v.xval), type="l", 
     lwd=2, xlab="", ylab="", 
     main="t-distributions")
# add lines to plot
for (in_dex in 1:3) {
  lines(v.xval, dt(v.xval, df=v.df[in_dex]), 
lwd=2, col=v.colors[in_dex+1])
}
# add legend
legend("topright", inset=0.05, 
       title="Degrees\n of freedom", v.labels, 
       cex=0.8, lwd=2, lty=c(1, 1, 1, 1), 
       col=v.colors)



rm(list=ls())
Sys.Date()  # get today's date
date_time <- as.Date("2013-06-15")  # "%Y-%m-%d" or "%Y/%m/%d"
date_time
class(date_time)
as.Date("06-15-2013", "%m-%d-%Y")  # specify format
date_time + 20  # add 20 days
as.numeric(date_time)  # get internal integer representation
some.date <- as.Date("11/22/2013", "%m/%d/%Y")
some.date
# difference between dates
difftime(some.date, date_time, units="weeks")
weekdays(date_time)  # get day of the week



rm(list=ls())
date_time <- Sys.time()  # get today's date and time
date_time
class(date_time)  # date and time is are POSIXct objects
as.numeric(date_time)  # get internal integer representation
# convert character string "%Y-%m-%d %H:%M:%S" to POSIXct object
as.POSIXct("2014-08-15 15:30:10")
format(date_time)  # convert POSIXct to character string
class(format(date_time))  # character string
date_time + 20  # add 20 seconds
as.POSIXct(as.Date(date_time)+1)  # add a day
trunc(date_time, units="hours")  # truncate to closest hour
as.POSIXct(as.character(as.Date(date_time)))  # truncate to closest day
methods(trunc)  # trunc methods
trunc.POSIXt



# convert character string "%Y-%m-%d %H:%M:%S" to POSIXlt object
date_time <- as.POSIXlt("2013-06-15 18:30:10")
date_time
class(date_time)
aperm(as.matrix(unclass(date_time)))  # get internal representation

date_time + 20  # add 20 seconds
class(date_time + 20)  # implicit coercion to POSIXct



Sys.time()  # get today's date and time
Sys.timezone()  # get time-zone
Sys.setenv(tz="UTC")  # set time-zone to UTC
Sys.timezone()  # get time-zone

format(date_time, tz="EDT")  # convert to character in different TZ

as.POSIXlt(format(date_time, tz="UTC"))  # convert back to POSIXlt

Sys.setenv(tz="America/New_York")  # set time-zone to EDT
format(Sys.time(), tz="")
format(Sys.time(), tz="UTC")
as.POSIXlt(format(Sys.time(), tz="UTC")) - # difference between
  as.POSIXlt(format(Sys.time(), tz=""))  # local time and UTC



rm(list=ls())
library(zoo)  # load package zoo
date_time <- Sys.Date()  # create date series of class 'Date'
daily.index <- date_time + 0:365  # daily series over one year
head(daily.index, 4)  # print first few dates
format(head(daily.index, 4), "%m/%d/%Y")  # print first few dates
# create daily date and time series of class 'POSIXct'
daily.index <- seq(Sys.time(), by="days", length.out=365)
head(daily.index, 4)  # print first few dates
format(head(daily.index, 4), "%m/%d/%Y %H:%M:%S")  # print first few dates
# create series of monthly dates of class 'zoo'
monthly.index <- yearmon(2010+0:36/12)
head(monthly.index, 4)  # print first few dates
# create series of quarterly dates of class 'zoo'
qrtly.index <- yearqtr(2010+0:16/4)
head(qrtly.index, 4)  # print first few dates
# convert quarterly 'zoo' dates to POSIXct
Sys.setenv(tz="UTC")
as.POSIXct(head(qrtly.index, 4))



set.seed(1121)  # for reproducibility
my_var <- 100
# create monthly time series starting 1990
ts_var <- ts(data=cumsum(rnorm(my_var)), 
     frequency=12, start=c(1990, 1))
class(ts_var)  # class 'ts'
# set plot paramaters - margins and font scale
par(oma=c(1, 1, 1, 1))  # set outer margins
par(mgp=c(2, 1, 0))  # set axis title and labels
par(mar=c(5, 1, 1, 1))  # set plot margins
par(cex.lab=0.8,  # set font scales
    cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
plot(ts_var, type="l",  # perform plot
     col="red", lty="solid", xlab="", ylab="")
title(main="Random Prices", line=-1)  # add title
# window the time series
ts.new <- window(ts_var, start=1992, end=1997)



par(oma=c(1, 1, 1, 1), mgp=c(2, 1, 0), mar=c(5, 1, 1, 1), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
class(EuStockMarkets)  # multiple ts object
dim(EuStockMarkets)
head(EuStockMarkets)  # get first six rows
plot(EuStockMarkets, main="", xlab="")  # plot all the columns
title(main="EuStockMarkets", line=-1)  # add title



par(oma=c(1, 1, 1, 1), mgp=c(2, 1, 0), mar=c(5, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# calculate percentage returns
ts.rets <- diff(log(EuStockMarkets))
# calculate mean and standard deviation of returns
c(mean(ts.rets[, 1]), sd(ts.rets[, 1]))
# plot histogram
hist(ts.rets[, 1], breaks=30, main="", xlim=c(-0.04, 0.04), 
     ylim=c(0, 60), xlab="", ylab="", freq = FALSE)
lines(density(ts.rets[, 1]),  # draw a line
      col='red', lwd=2)
ch.title <- paste(colnames(EuStockMarkets)[1], 'returns')
title(main=ch.title, line=-1)  # add title



# set plot paramaters - margins and font scale
par(oma=c(1, 1, 1, 1))  # set outer margins
par(mgp=c(2, 1, 0))  # set axis title and labels
par(mar=c(5, 1, 1, 1))  # set plot margins
par(cex.lab=0.8,  # set font scales
    cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
set.seed(1121)  # for reproducibility
max.simu <- 1000  # max simulation trials
v.simu <- 0*1:max.simu  # initialize trials
barrier.level <- 20  # barrier level
v.simu[1] <- rnorm(1)  # first simulation value
sim.index <- 2  # initialize simulation index
while ((sim.index <= max.simu) && 
 (v.simu[sim.index - 1] < barrier.level)) {
  v.simu[sim.index] <- v.simu[sim.index - 1] + rnorm(1)
  sim.index <- sim.index + 1
}  # end while
if (sim.index <= max.simu) {  # fill zero prices
  v.simu[sim.index:max.simu] <- v.simu[sim.index - 1]
}
# create daily time series starting 2011
ts_var <- ts(data=v.simu, frequency=365, start=c(2011, 1))
plot(ts_var, type="l", col="black",  # perform plot
     lty="solid", xlab="", ylab="")
abline(h=barrier.level, lwd=2, col="red")  # add horizontal line
title(main="Random Prices", line=0)  # add title


