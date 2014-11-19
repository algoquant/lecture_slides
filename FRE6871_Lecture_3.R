

library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
thm <- knit_theme$get("acid")
knit_theme$set(thm)



rm(list=ls())
TRUE | FALSE
TRUE | NA
vec_var1 <- c(2, 4, 6)
vec_var1 < 5
(vec_var1 < 5) & (vec_var1 > 3)
vec_var1[(vec_var1 < 5) & (vec_var1 > 3)]
vec_var2 <- c(-10, 0, 10)
vec_var1 < vec_var2
c(FALSE, TRUE, FALSE) & c(TRUE, TRUE, FALSE)
c(FALSE, TRUE, FALSE) | c(TRUE, TRUE, FALSE)



rm(list=ls())
c(FALSE, TRUE, FALSE) && c(TRUE, TRUE, FALSE)
c(FALSE, TRUE, FALSE) || c(TRUE, TRUE, FALSE)
echo_true = function() {cat("echo_true\t"); TRUE}
echo_false = function() {cat("echo_false\t"); FALSE}
echo_true() | echo_false()
echo_true() || echo_false()  # echo_false() isn't evaluated at all!
vec_var <- c(2, 4, 6)
# works (does nothing) using '&&'
if (is.matrix(vec_var) && (vec_var[2, 3] > 0)) {
  vec_var[2, 3] <- 1
}
# no short-circuit so fails (throws an error)
if (is.matrix(vec_var) & (vec_var[2, 3] > 0)) {
  vec_var[2, 3] <- 1
}



num_var <- 2
num_var==2
identical(num_var, 2)

identical(num_var, NULL) 
num_var==NULL

vec_var <- c(2, 4, 6)
vec_var==2
identical(vec_var, 2)



table_values <- sample(1:10)
table_values
which(table_values==5)
which(table_values>5)
which.max(table_values)
which.min(table_values)
match(5, table_values)
match(-5, table_values)
5 %in% table_values
-5 %in% table_values
c(5, -5) %in% table_values



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



rm(list=ls())
# create two numeric vectors
vec_var1 <- sin(0.25*pi*1:10)
vec_var2 <- cos(0.25*pi*1:10)
# create third vector using 'ifelse'
vec_var3 <- ifelse(vec_var1 > vec_var2, 
          vec_var1, vec_var2)
# cbind all three together
vec_var4 <- cbind(vec_var1, vec_var2, vec_var3)

# set plotting parameters
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), 
    cex.lab=0.8, cex.axis=0.8, cex.main=0.8, 
    cex.sub=0.5)
# plot matrix
matplot(vec_var4, type="l", lty="solid", 
col=c("green", "blue", "red"), 
lwd=c(2, 2, 2), xlab="", ylab="")
# add legend
legend(x="bottomright", legend=colnames(vec_var4), 
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
fib.seq <- numeric()  # create zero length numeric vector
fib.seq[1] <- 1  # initialize
fib.seq[2] <- 1  # initialize

for (i in 3:10) {  # perform recurrence loop
  fib.seq[i] <- fib.seq[i-1] + fib.seq[i-2]
}  # end for

fib.seq



rm(list=ls())
fibo_nacci <- function(seq_length) {
  if (seq_length > 2) {
    fib.seq <- fibo_nacci(seq_length-1)  # recursion
    c(fib.seq, sum(tail(fib.seq, 2)))  # return this
  } else {
    c(1, 1)  # initialize and return
  }
}  # end fibo_nacci
fibo_nacci(10)
tail(fibo_nacci(10), 2)



x_var <- seq(-2*pi, 2*pi, len=100)  # x values

# open Windows graphics device
x11(width=11, height=7, title="simple plot")

# plot a sine function using basic line plot
plot(x=x_var, y=sin(x_var), xlab="x-val", 
     ylab="y-val", type='l', lwd=2, col="red")
# add a cosine function
lines(x=x_var, y=cos(x_var), lwd=2, col="blue")
# add title
title(main="sine and cosine functions", line=0.1)
# add legend
legend(x="topright", legend=c("sine", "cosine"),
       title="legend", inset=0.1, cex=1.0, bg="white",
       lwd=2, lty=c(1, 1), col=c("red", "blue"))
graphics.off()  # close all graphics devices



par(mar=c(5, 3, 1, 1), oma=c(1, 1, 1, 1), mgp=c(2, 0.5, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
attach(mtcars)  # add mtcars to search path
# plot scatterplot horsepower vs miles per gallon
plot(hp, mpg, 
     main="miles per gallon vs horsepower")

# add labels using wordcloud, to prevent overlaps
library(wordcloud)
textplot(x=hp, y=mpg, words=rownames(mtcars))

# don't forget to detach!!!
detach(mtcars)



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



par(mar=c(7, 4, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
pois_vector <- rpois(100, 4)  # Poisson numbers
# calculate contingency table
pois_table <- table(pois_vector)
pois_table <- pois_table/sum(pois_table)
pois_table

library(MASS)  # create histogram of Poisson variables
truehist(pois_vector, nbins="FD", col="blue", xlab="No. of events", 
 ylab="Frequency of events", main="Poisson histogram")
x_var <- 0:max(pois_vector)  # add Poisson density
lines(x=x_var, y=dpois(x_var, lambda=4), lwd=4, col="red")
# add legend
legend(x="topright", legend="Poisson density", title="", inset=0.05, 
       cex=0.8, bg="white", lwd=4, lty=1, col="red")



graph_params <- par()  # get existing parameters
par("mar")  # get plot margins
par(mar=c(2, 1, 2, 1))  # set plot margins
par(oma=c(1, 1, 1, 1))  # set outer margins
par(mgp=c(2, 1, 0))  # set title and label margins
par(cex.lab=0.8,  # set font scales
    cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(las=1)  # set axis labels to horizontal
par(ask=TRUE)  # pause, ask before plotting
par(mfrow=c(2, 2))  # plot on 2x2 grid by rows
for (i in 1:4) {  # plot 4 panels
  barplot(sample(1:6), main=paste("panel", i), 
  col=rainbow(6), border=NA, axes=FALSE)
  box()
}
par(ask=FALSE)  # restore automatic plotting
par(new=TRUE)  # allow new plot on same chart
par(graph_params)  # restore original parameters



Sys.Date()  # get today's date
date_time <- as.Date("2014-07-14")  # "%Y-%m-%d" or "%Y/%m/%d"
date_time
class(date_time)  # Date object
as.Date("07-14-2014", "%m-%d-%Y")  # specify format
date_time + 20  # add 20 days
as.numeric(date_time)  # get internal integer representation
date_old <- as.Date("07/14/2013", "%m/%d/%Y")
date_old
# difference between dates
difftime(date_time, date_old, units="weeks")
weekdays(date_time)  # get day of the week
# coerce numeric into date-times
date_time <- 0
attributes(date_time) <- list(class="Date")
date_time  # "Date" object
structure(0, class="Date")  # "Date" object



date_time <- Sys.time()  # get today's date and time
date_time
class(date_time)  # POSIXct object
as.numeric(date_time)  # get internal integer representation
# parse character string "%Y-%m-%d %H:%M:%S" to POSIXct object
as.POSIXct("2014-07-14 13:30:10")
format(date_time)  # convert POSIXct to character string
class(format(date_time))  # character string
date_time + 20  # add 20 seconds
as.POSIXct(as.Date(date_time)+1)  # add a day
trunc(date_time, units="hours")  # truncate to closest hour
as.POSIXct(as.character(as.Date(date_time)))  # truncate to closest day
methods(trunc)  # trunc methods
trunc.POSIXt



# parse character string "%Y-%m-%d %H:%M:%S" to POSIXlt object
date_time <- as.POSIXlt("2014-07-14 18:30:10")
date_time
class(date_time)  # POSIXlt object
aperm(as.matrix(unclass(date_time)))  # get internal representation

date_time + 20  # add 20 seconds
class(date_time + 20)  # implicit coercion to POSIXct



Sys.timezone()  # get time-zone
Sys.setenv(tz="UTC")  # set time-zone to UTC
Sys.timezone()  # get time-zone
# Standard Time in effect
as.POSIXct("2013-03-09 11:00:00", tz="America/New_York")
# Daylight Savings Time in effect
as.POSIXct("2013-03-10 11:00:00", tz="America/New_York")
date_time <- Sys.time()  # today's date and time
# convert to character in different TZ
format(date_time, tz="America/New_York")
format(date_time, tz="UTC")
# parse back to POSIXct
as.POSIXct(format(date_time, tz="America/New_York"))
# difference between local time and UTC
as.POSIXct(format(Sys.time(), tz="UTC")) - 
  as.POSIXct(format(Sys.time(), tz="America/New_York"))



Sys.time()  # get today's date and time
Sys.timezone()  # get time-zone
Sys.setenv(tz="UTC")  # set time-zone to UTC
# parse character string "%Y-%m-%d %H:%M:%S" to POSIXlt object
as.POSIXlt("2014-07-14 18:30:10")
class(as.POSIXlt("2014-07-14 18:30:10")+3600)  # coercion to POSIXct
Sys.setenv(tz="America/New_York")  # set time-zone to "New York"
date_time <- as.POSIXct("2014-07-14 18:30:10", tz="America/New_York")
date_time
class(date_time)  # POSIXct object
date_time + 20  # add 20 seconds
as.numeric(date_time)  # get internal integer representation
format(date_time, tz="UTC")  # convert to character in different TZ
as.POSIXct(format(date_time, tz="UTC"))  # parse back to POSIXct
as.POSIXct(format(Sys.time(), tz="UTC")) - # difference between
  as.POSIXct(format(Sys.time(), tz=""))  # local time and UTC



library(lubridate)  # load lubridate
# parse strings into date-times
as.POSIXct("07-14-2014", format="%m-%d-%Y", tz="America/New_York")
date_time <- mdy("07-14-2014", tz="America/New_York")
date_time
class(date_time)  # POSIXct object
dmy("14.07.2014", tz="America/New_York")

# parse numeric into date-times
as.POSIXct(as.character(14072014), format="%d%m%Y", 
                  tz="America/New_York")
dmy(14072014, tz="America/New_York")

# parse decimal to date-times
decimal_date(date_time)
date_decimal(2014.25, tz="America/New_York")
date_decimal(decimal_date(date_time), tz="America/New_York")



library(lubridate)  # load lubridate
date_time <- ymd_hms(20140714142010, 
               tz="America/New_York")
date_time

# get same moment of time in "UTC" time zone
with_tz(date_time, "UTC")
as.POSIXct(format(date_time, tz="UTC"), tz="UTC")

# get same clock time in "UTC" time zone
force_tz(date_time, "UTC")
as.POSIXct(format(date_time), tz="UTC")

# same moment of time
date_time - with_tz(date_time, "UTC")

# different moments of time
date_time - force_tz(date_time, "UTC")



library(lubridate)  # load lubridate
# Daylight Savings Time handling periods vs durations
date_time <- as.POSIXct("2013-03-09 11:00:00", 
                  tz="America/New_York")
date_time
date_time + ddays(1)  # add duration
date_time + days(1)  # add period

leap_year(2012)  # leap year
date_time <- dmy(01012012, tz="America/New_York")
date_time
date_time + dyears(1)  # add duration
date_time + years(1)  # add period



library(lubridate)  # load lubridate
date_time <- ymd_hms(20140714142010, tz="America/New_York")
date_time
# add periods to a date-time
c(date_time + seconds(1), date_time + minutes(1), 
date_time + days(1), date_time + months(1))

# create vectors of dates
date_time <- ymd(20140714, tz="America/New_York")
date_time + 0:2 * months(1)  # monthly dates
date_time + months(0:2)
date_time + 0:2 * months(2)  # bi-monthly dates
date_time + seq(0, 5, by=2) * months(1)
seq(date_time, length=3, by="2 months")

# adding monthly periods can create invalid dates
date_time <- ymd(20120131, tz="America/New_York")
date_time + 0:2 * months(1)
date_time + months(1)
date_time + months(2)



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



library(zoo)  # load package zoo
date_time <- Sys.Date()  # create date series of class 'Date'
date_index <- date_time + 0:365  # daily series over one year
head(date_index, 4)  # print first few dates
format(head(date_index, 4), "%m/%d/%Y")  # print first few dates
# create daily date-time series of class 'POSIXct'
date_index <- seq(Sys.time(), by="days", length.out=365)
head(date_index, 4)  # print first few dates
format(head(date_index, 4), "%m/%d/%Y %H:%M:%S")  # print first few dates
# create series of monthly dates of class 'zoo'
monthly_index <- yearmon(2010+0:36/12)
head(monthly_index, 4)  # print first few dates
# create series of quarterly dates of class 'zoo'
qrtly_index <- yearqtr(2010+0:16/4)
head(qrtly_index, 4)  # print first few dates
# parse quarterly 'zoo' dates to POSIXct
Sys.setenv(tz="UTC")
as.POSIXct(head(qrtly_index, 4))


