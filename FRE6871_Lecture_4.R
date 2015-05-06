library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
thm <- knit_theme$get("acid")
knit_theme$set(thm)
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
my_var <- matrix(1:10, 2)
c(typeof(my_var), mode(my_var), class(my_var))

my_var <- vector(mode="numeric", length=10)
c(typeof(my_var), mode(my_var), class(my_var))

my_var <- vector(mode="integer", length=10)
c(typeof(my_var), mode(my_var), class(my_var))
attributes(5:10)  # a simple vector has no attributes
my_var <- c(pi=pi, euler=exp(1), gamma=-digamma(1))
attributes(my_var)  # named vector has "names" attribute
my_var <- 1:10
is.vector(my_var)  # is the object a vector?
attributes(my_var) <- list(dim=c(2, 5))
is.matrix(my_var)  # is the object a matrix?
my_var  # matrix object
structure(1:10, dim=c(2, 5))  # matrix object

my_var <- 0
attributes(my_var) <- list(class="Date")
my_var  # "Date" object
structure(0, class="Date")  # "Date" object
my_var <- matrix(runif(10), 2, 5)
class(my_var)  # has implicit class
attributes(my_var)  # but no explicit "class" attribute
c(typeof(my_var), mode(my_var), class(my_var))
class(my_var) <- "my_class"  # assign explicit "class" attribute
class(my_var)  # has explicit "class"
attributes(my_var)  # has explicit "class" attribute
is.matrix(my_var)  # is the object a matrix?
is.vector(my_var)  # is the object a vector?
attributes(unclass(my_var))
my_var <- 1:5
c(typeof(my_var), mode(my_var), class(my_var))

mode(my_var) <- "character"  # coerce to "character"
my_var
c(typeof(my_var), mode(my_var), class(my_var))
my_var <- as.character(1:5)  # explicitly coerce to "character"
c(typeof(my_var), mode(my_var), class(my_var))

my_var <- matrix(1:10, 2, 5)
my_var <- as.character(my_var)  # explicitly coerce to "character"
c(typeof(my_var), mode(my_var), class(my_var))
is.matrix(my_var)  # is the object a matrix?

as.logical(0:3)  # explicit coercion to "logical"

c(1:3, 'a')  # implicit coercion to "character"
as.numeric(c(1:3, 'a'))  # explicit coercion to "numeric"
Sys.Date()  # get today's date
date_time <- as.Date("2012-07-14")  # "%Y-%m-%d" or "%Y/%m/%d"
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
as.POSIXct("2012-07-14 13:30:10")
format(date_time)  # convert POSIXct to character string
class(format(date_time))  # character string
date_time + 20  # add 20 seconds
trunc(date_time, units="hours")  # truncate to closest hour
as.POSIXct(format(as.Date(date_time)))  # truncate to closest day
as.POSIXct(format(as.Date(date_time)+1))  # add one day
methods(trunc)  # trunc methods
trunc.POSIXt
# parse character string "%Y-%m-%d %H:%M:%S" to POSIXlt object
date_time <- as.POSIXlt("2012-07-14 18:30:10")
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
library(lubridate)  # load lubridate
# adding monthly periods can create invalid dates
date_time <- ymd(20120131, tz="America/New_York")
date_time + 0:2 * months(1)
date_time + months(1)
date_time + months(2)

# create vector of end-of-month dates
date_time %m-% months(13:1)
library(zoo)  # load zoo
library(RQuantLib)  # load RQuantLib

# create daily date series of class 'Date'
in_dex <- Sys.Date() + -5:2
in_dex

# create logical vector of business days
bus.days <- isBusinessDay(  # RQuantLib calendar
  calendar="UnitedStates/GovernmentBond", in_dex)

# create daily series of business days
bus_index <- in_dex[bus.days]
bus_index
library(zoo)  # load package zoo
date_time <- Sys.Date()  # create date series of class 'Date'
in_dex <- date_time + 0:365  # daily series over one year
head(in_dex, 4)  # print first few dates
format(head(in_dex, 4), "%m/%d/%Y")  # print first few dates
# create daily date-time series of class 'POSIXct'
in_dex <- seq(Sys.time(), by="days", length.out=365)
head(in_dex, 4)  # print first few dates
format(head(in_dex, 4), "%m/%d/%Y %H:%M:%S")  # print first few dates
# create series of monthly dates of class 'zoo'
monthly_index <- yearmon(2010+0:36/12)
head(monthly_index, 4)  # print first few dates
# create series of quarterly dates of class 'zoo'
qrtly_index <- yearqtr(2010+0:16/4)
head(qrtly_index, 4)  # print first few dates
# parse quarterly 'zoo' dates to POSIXct
Sys.setenv(tz="UTC")
as.POSIXct(head(qrtly_index, 4))
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
new_env[["new_var1"]]  # environments are subset like lists
search()  # get search path for R objects
my_list <- list(flowers=c("rose", "daisy", "tulip"),  # create a list
                trees=c("pine", "oak", "maple"))
my_list$trees
attach(my_list)
trees
search()  # get search path for R objects
detach(my_list)
head(trees)  # "trees" is part of the datasets base package
# "trees" is in datasets base package
head(trees, 3)
colnames(trees)
mean(Girth)
mean(trees$Girth)
with(trees, c(mean(Girth), mean(Height), mean(Volume)))
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
a <- 1  # define a variable
# new variable "b" points to value of "a"
b <- a  # define a new variable
# when "b" is modified, R makes a copy of it
b <- b+1
# function doubles its argument and returns it
double_it <- function(in_var) {
  in_var <- 2*in_var
  cat("input argument was doubled to:", in_var, "\n")
  in_var
}
double_it(a)
a  # variable "a" is unchanged
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
