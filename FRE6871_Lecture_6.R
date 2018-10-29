# R startup chunk
# ```{r setup, include=FALSE}
library(shiny)
library(quantmod)
inter_val <- 31
cl_ose <- quantmod::Cl(rutils::etf_env$VTI)
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue")
# ```
#end R startup chunk
inputPanel(
  sliderInput("lamb_da", label="lambda:",
    min=0.01, max=0.2, value=0.1, step=0.01)
)  # end inputPanel
renderPlot({
  # calculate EWMA prices
  lamb_da <- input$lamb_da
  weight_s <- exp(-lamb_da*1:inter_val)
  weight_s <- weight_s/sum(weight_s)
  ew_ma <- filter(cl_ose, filter=weight_s, sides=1)
  ew_ma[1:(inter_val-1)] <- ew_ma[inter_val]
  ew_ma <- xts(cbind(cl_ose, ew_ma), order.by=index(cl_ose))
  colnames(ew_ma) <- c("VTI", "VTI EWMA")
  # plot EWMA prices
  ch_ob <- chart_Series(ew_ma, theme=plot_theme, name="EWMA prices")
  plot(ch_ob)
  legend("top", legend=colnames(ew_ma),
   inset=0.1, bg="white", lty=1, lwd=2,
   col=plot_theme$col$line.col, bty="n")
})  # end renderPlot
# Define cumulative default probability function
cum_loss <- function(x, def_thresh=(-2), rh_o=0.2, l_gd=0.4)
  pnorm((sqrt(1-rh_o)*qnorm(x/l_gd) - def_thresh)/sqrt(rh_o))
# Define Vasicek loss distribution density function
# (vectorized version with error handling for x)
portf_loss <- function(x, def_thresh=-2, rh_o=0.1, l_gd=0.4) {
  q_norm <- ifelse(x/l_gd < 0.999, qnorm(x/l_gd), 3.1)
  sqrt((1-rh_o)/rh_o)*exp(-(sqrt(1-rh_o)*q_norm - def_thresh)^2/(2*rh_o) + q_norm^2/2)/l_gd
}  # end portf_loss
def_prob <- 0.2; def_thresh <- qnorm(def_prob)
rh_o <- 0.1; l_gd <- 0.4
at_tach <- 0.15; de_tach <- 0.2
# Expected tranche loss is sum of two terms
tranche_loss <-
  # Loss between at_tach and de_tach
  integrate(function(x, at_tach) (x-at_tach)*portf_loss(x,
def_thresh=def_thresh, rh_o=rh_o, l_gd=l_gd),
low=at_tach, up=de_tach, at_tach=at_tach)$value / (de_tach-at_tach) +
  # Loss in excess of de_tach
  (1-cum_loss(x=de_tach, def_thresh=def_thresh, rh_o=rh_o, l_gd=l_gd))
# Plot probability distribution of losses
curve(expr=portf_loss(x, def_thresh=def_thresh, rh_o=rh_o),
type="l", xlim=c(0, 3*l_gd*def_prob),
xlab="loss percentage", ylab="density", lwd=3,
col="orange", main="CDO Tranche Losses")
# Add line for expected loss
abline(v=l_gd*def_prob, col="red", lwd=3)
text(x=l_gd*def_prob-0.001, y=4, labels="expected loss",
 lwd=2, srt=90, pos=3)
# Add lines for attach and detach
abline(v=at_tach, col="blue", lwd=3)
text(x=at_tach-0.001, y=4, labels="attach",
 lwd=2, srt=90, pos=3)
abline(v=de_tach, col="green", lwd=3)
text(x=de_tach-0.001, y=4, labels="detach",
 lwd=2, srt=90, pos=3)
# Add shading for CDO tranche
var_s <- seq(at_tach, de_tach, length=100)
dens_ity <- sapply(var_s, portf_loss,
  def_thresh=def_thresh, rh_o=rh_o)
# Draw shaded polygon
polygon(c(at_tach, var_s, de_tach), density=20,
  c(-1, dens_ity, -1), col="red", border=NA)
text(x=0.5*(at_tach+de_tach), y=0, labels="CDO tranche", cex=0.9, lwd=2, pos=3)
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
  while (NROW(li_st) > 1) {
# index of odd list elements
    odd_index <- seq(from=1, to=NROW(li_st), by=2)
# bind odd and even elements, and divide li_st by half
    li_st <- lapply(odd_index, function(in_dex) {
if (in_dex==NROW(li_st)) return(li_st[[in_dex]])
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
# split mpg column based on number of cylinders
split(mtcars$mpg, mtcars$cyl)
# split mtcars data frame based on number of cylinders
split_cars <- split(mtcars, mtcars$cyl)
str(split_cars, max.level=1)
names(split_cars)
# aggregate the mean mpg over split mtcars data frame
sapply(split_cars, function(x) mean(x$mpg))
# Or: split mpg column and aggregate the mean
sapply(split(mtcars$mpg, mtcars$cyl), mean)
# same but using with()
with(mtcars, sapply(split(mpg, cyl), mean))
# Or: aggregate() using formula syntax
aggregate(formula=(mpg ~ cyl), data=mtcars,
    FUN=mean)
# Or: aggregate() using data frame syntax
aggregate(x=mtcars$mpg,
  by=list(cyl=mtcars$cyl), FUN=mean)
# Or: using name for mpg
aggregate(x=list(mpg=mtcars$mpg),
  by=list(cyl=mtcars$cyl), FUN=mean)
# aggregate() all columns
aggregate(x=mtcars,
  by=list(cyl=mtcars$cyl), FUN=mean)
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
# Download CRSPpanel.txt from NYU Classes
# Read the file using read.table() with header and sep arguments
panel_data <- read.table(file="C:/Develop/R/lecture_slides/data/CRSPpanel.txt",
                   header=TRUE, sep="\t")
# split panel_data based on Industry column
split_panel <- split(panel_data, panel_data$Industry)
# number of companies in each Industry
sapply(split_panel, NROW)
# number of Sectors that each Industry belongs to
sapply(split_panel, function(x) {
  NROW(unique(x$Sector))
})  # end sapply
# Or
aggregate(formula=(Sector ~ Industry),
  data=panel_data, FUN=function(x) NROW(unique(x)))
# Industries and the Sector to which they belong
aggregate(formula=(Sector ~ Industry),
  data=panel_data, FUN=unique)
# Or
with(panel_data, aggregate(x=Sector,
  by=list(Industry), FUN=unique))
# Or
with(panel_data, sapply(levels(Industry),
  function(x) {
    Sector[match(x, Industry)]
  }))  # end sapply
# split panel_data based on Sector column
split_panel <- split(panel_data, panel_data$Sector)
# number of companies in each Sector
sapply(split_panel, NROW)
# Industries belonging to each Sector (jagged array)
sec_ind <- sapply(split_panel,
  function(x) unique(as.vector(x$Industry)))
# Or use aggregate() (returns a data frame)
sec_ind2 <- aggregate(formula=(Industry ~ Sector),
  data=panel_data, FUN=function(x) unique(as.vector(x)))
# Or use aggregate() with "by" argument
sec_ind2 <- with(panel_data,
  aggregate(x=Industry, by=list(Sector),
    FUN=function(x) as.vector(unique(x))))
# coerce sec_ind2 into a jagged array
name_s <- as.vector(sec_ind2[, 1])
sec_ind2 <- sec_ind2[, 2]
names(sec_ind2) <- name_s
all.equal(sec_ind2, sec_ind)
# Or use tapply() (returns an array)
sec_ind2 <- with(panel_data,
  tapply(X=as.vector(Industry), INDEX=Sector, FUN=unique))
# coerce sec_ind2 into a jagged array
sec_ind2 <- drop(as.matrix(sec_ind2))
all.equal(sec_ind2, sec_ind)
# average ROE in each Industry
with(panel_data,
  sapply(split(ROE, Industry), mean))
# average, min, and max ROE in each Industry
t(with(panel_data,
  sapply(split(ROE, Industry),
    FUN=function(x)
c(mean=mean(x), max=max(x), min=min(x)))))
# split panel_data based on Industry column
split_panel <- split(panel_data,
  panel_data$Industry)
# average ROE and EPS in each Industry
t(sapply(split_panel, FUN=function(x)
  c(mean_roe=mean(x$ROE),
    mean_eps=mean(x$EPS.EXCLUDE.EI))))
# Or: split panel_data based on Industry column
split_panel <-
  split(panel_data[, c("ROE", "EPS.EXCLUDE.EI")],
  panel_data$Industry)
# average ROE and EPS in each Industry
t(sapply(split_panel,
  FUN=function(x) sapply(x, mean)))
# average ROE and EPS using aggregate()
aggregate(x=panel_data[, c("ROE", "EPS.EXCLUDE.EI")],
  by=list(panel_data$Industry),
  FUN=mean)
# ?options  # get info on global options
getOption("warn")  # global option for "warn"
options("warn")  # global option for "warn"
getOption("error")  # global option for "error"
sqrt_safe <- function(in_put) {
# returns its argument
  if (in_put<0) {
    warning("sqrt_safe: in_put is negative")
    NULL  # return NULL for negative argument
  } else {
    sqrt(in_put)
  }  # end if
}  # end sqrt_safe
sqrt_safe(5)
sqrt_safe(-1)
options(warn=-1)
sqrt_safe(-1)
options(warn=0)
sqrt_safe()
options(warn=1)
sqrt_safe()
options(warn=3)
sqrt_safe()
# function vali_date validates its arguments
vali_date <- function(in_put=NULL) {
# check if argument is valid and return double
  if (is.null(in_put)) {
    return("vali_date: in_put is missing")
  } else if (is.numeric(in_put)) {
    2*in_put
  } else cat("vali_date: in_put not numeric")
}  # end vali_date
vali_date(3)
vali_date("a")
vali_date()
# vali_date validates arguments using missing()
vali_date <- function(in_put) {
# check if argument is valid and return double
  if (missing(in_put)) {
    return("vali_date: in_put is missing")
  } else if (is.numeric(in_put)) {
    2*in_put
  } else cat("vali_date: in_put is not numeric")
}  # end vali_date
vali_date(3)
vali_date("a")
vali_date()
# vali_date() validates its arguments and assertions
vali_date <- function(in_put) {
# check if argument is valid and return double
  if (missing(in_put)) {
    stop("vali_date: in_put is missing")
  } else if (!is.numeric(in_put)) {
    cat("in_put=", in_put)
    stop("vali_date: in_put is not numeric")
  } else 2*in_put
}  # end vali_date
vali_date(3)
vali_date("a")
vali_date()
# print the call stack
traceback()
vali_date <- function(in_put) {
# check argument using long form '&&' operator
  stopifnot(!missing(in_put) &&
        is.numeric(in_put))
  2*in_put
}  # end vali_date
vali_date(3)
vali_date()
vali_date("a")
vali_date <- function(in_put) {
# check argument using logical '&' operator
  stopifnot(!missing(in_put) & is.numeric(in_put))
  2*in_put
}  # end vali_date
vali_date()
vali_date("a")
# sum_two() returns the sum of its two arguments
sum_two <- function(in_put1, in_put2) {  # even more robust
# check if at least one argument is not missing
  stopifnot(!missing(in_put1) &&
        !missing(in_put2))
# check if arguments are valid and return sum
  if (is.numeric(in_put1) &&
is.numeric(in_put2)) {
    in_put1 + in_put2  # both valid
  } else if (is.numeric(in_put1)) {
    cat("in_put2 is not numeric\n")
    in_put1  # in_put1 is valid
  } else if (is.numeric(in_put2)) {
    cat("in_put1 is not numeric\n")
    in_put2  # in_put2 is valid
  } else {
    stop("none of the arguments are numeric")
  }
}  # end sum_two
sum_two(1, 2)
sum_two(5, 'a')
sum_two('a', 5)
sum_two('a', 'b')
sum_two()
# flag "vali_date" for debugging
debug(vali_date)
# calling "vali_date" starts debugger
vali_date(3)
# unflag "vali_date" for debugging
undebug(vali_date)
vali_date <- function(in_put) {
  browser()  # pause and invoke browser
# check argument using long form '&&' operator
  stopifnot(!missing(in_put) &&
        is.numeric(in_put))
  2*in_put
}  # end vali_date
vali_date()  # invokes debugger
options("error")  # show default NULL "error" option
options(error=recover)  # set "error" option to "recover"
options(error=NULL)  # set back to default "error" option
str(tryCatch)  # get arguments of tryCatch()
tryCatch(  # without error handler
  {  # evaluate expressions
    num_var <- 101  # assign
    stop('my error')  # produce error
  },
  finally=print(paste("num_var=", num_var))
)  # end tryCatch

tryCatch(  # with error handler
  {  # evaluate expressions
    num_var <- 101  # assign
    stop('my error')  # produce error
  },
  # error handler captures error condition
  error=function(error_cond) {
    print(paste("error handler: ", error_cond))
  },  # end error handler
  # warning handler captures warning condition
  warning=function(warning_cond) {
    print(paste("warning handler: ", warning_cond))
  },  # end warning handler
  finally=print(paste("num_var=", num_var))
)  # end tryCatch
rm(list=ls())
# apply loop without tryCatch
apply(as.matrix(1:5), 1, function(num_var) {  # anonymous function
    stopifnot(num_var != 3)  # check for error
    # broadcast message to console
    cat("(cat) num_var =", num_var, "\n")
    # return a value
    paste("(return) num_var =", num_var)
  }  # end anonymous function
)  # end apply
# apply loop with tryCatch
apply(as.matrix(1:5), 1, function(num_var) {  # anonymous function
    tryCatch(  # with error handler
{  # body
  stopifnot(num_var != 3)  # check for error
  # broadcast message to console
  cat("(cat) num_var =", num_var, "\t")
  # return a value
  paste("(return) num_var =", num_var)
},
# error handler captures error condition
error=function(error_cond)
  paste("handler: ", error_cond),
finally=print(paste("(finally) num_var =", num_var))
    )  # end tryCatch
  }  # end anonymous function
)  # end apply
my_var <- 1  # create new object
assign(x="my_var", value=2)  # assign value to existing object
my_var
rm(my_var)  # remove my_var
assign(x="my_var", value=3)  # create new object from name
my_var
# create new object in new environment
new_env <- new.env()  # create new environment
assign("my_var", 3, envir=new_env)  # assign value to name
ls(new_env)  # list objects in "new_env"
new_env$my_var
rm(list=ls())  # delete all objects
sym_bol <- "my_var"  # define symbol containing string "my_var"
assign(sym_bol, 1)  # assign value to "my_var"
ls()
my_var
assign("sym_bol", "new_var")
assign(sym_bol, 1)  # assign value to "new_var"
ls()
sym_bol <- 10
assign(sym_bol, 1)  # can't assign to non-string
rm(list=ls())  # delete all objects
# create individual vectors from column names of EuStockMarkets
for (col_name in colnames(EuStockMarkets)) {
# assign column values to column names
  assign(col_name, EuStockMarkets[, col_name])
}  # end for
ls()
head(DAX)
head(EuStockMarkets[, "DAX"])
identical(DAX, EuStockMarkets[, "DAX"])
# create new environment
test_env <- new.env()
# pass string as name to create new object
assign("my_var1", 2, envir=test_env)
# create new object using $ string referencing
test_env$my_var2 <- 1
# list objects in new environment
ls(test_env)
# reference an object by name
test_env$my_var1
# reference an object by string name using get
get("my_var1", envir=test_env)
# retrieve and assign value to object
assign("my_var1",
       2*get("my_var1", envir=test_env),
       envir=test_env)
get("my_var1", envir=test_env)
# return all objects in an environment
mget(ls(test_env), envir=test_env)
# delete environment
rm(test_env)
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
price_s <- rutils::do_call(cbind, # do.call(merge
  as.list(rutils::etf_env)[sym_bols])
# extract and merge adjusted prices, subset by sym_bols
price_s <- rutils::do_call(cbind,
  lapply(as.list(rutils::etf_env)[sym_bols], quantmod::Ad))
# same, but works only for OHLC series
price_s <- rutils::do_call(cbind,
  eapply(rutils::etf_env, quantmod::Ad)[sym_bols])
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
# copy price_s into etf_env and save to .RData file
assign("price_s", price_s, envir=etf_env)
save(etf_env, file='etf_data.RData')
# "trees" is in datasets base package
head(trees, 3)
colnames(trees)
mean(Girth)
mean(trees$Girth)
with(trees, 
     c(mean(Girth), mean(Height), mean(Volume)))
library(tseries)  # Load package tseries
# Download MSFT data in ts format
ts_stx <- suppressWarnings(
  get.hist.quote(
    instrument="MSFT",
    start=Sys.Date()-3*365,
    end=Sys.Date(),
    retclass="ts",
    quote=c("Open","High","Low","Close",
      "AdjClose","Volume"),
    origin="1970-01-01")
)  # end suppressWarnings
load(file="C:/Develop/R/lecture_slides/data/zoo_data.RData")
# Calculate price adjustment vector
adj_vector <-
  as.vector(ts_stx[, "AdjClose"] / ts_stx[, "Close"])
# Adjust OHLC prices
ts_stx_adj <- ts_stx
ts_stx_adj[, c("Open","High","Low","Close")] <-
  adj_vector * ts_stx[, c("Open","High","Low","Close")]
# Inspect the data
tsp(ts_stx_adj)  # frequency=1
head(time(ts_stx_adj))
head(ts_stx_adj)
tail(ts_stx_adj)
library(tseries)  # Load package tseries
# Download MSFT data
zoo_stx <- suppressWarnings(
  get.hist.quote(
    instrument="MSFT",
    start=Sys.Date()-3*365,
    end=Sys.Date(),
    quote=c("Open","High","Low","Close",
      "AdjClose","Volume"),
    origin="1970-01-01")
)  # end suppressWarnings
load(file="C:/Develop/R/lecture_slides/data/zoo_data.RData")
class(zoo_stx)
dim(zoo_stx)
head(zoo_stx, 4)
library(tseries)  # Load package tseries
load(file="C:/Develop/R/lecture_slides/data/zoo_data.RData")
# Calculate price adjustment vector
adj_vector <-
  as.vector(zoo_stx[, "AdjClose"] / zoo_stx[, "Close"])
head(adj_vector, 5)
tail(adj_vector, 5)
# Adjust OHLC prices
zoo_stx_adj <- zoo_stx
zoo_stx_adj[, c("Open","High","Low","Close")] <-
  adj_vector * zoo_stx[, c("Open","High","Low","Close")]
head(zoo_stx_adj)
tail(zoo_stx_adj)
library(tseries)  # Load package tseries
# Download EUR/USD data
zoo_eurusd <- suppressWarnings(
  get.hist.quote(
    instrument="EUR/USD",
    provider="oanda",
    start=Sys.Date()-3*365,
    end=Sys.Date(),
    origin="1970-01-01")
)  # end suppressWarnings
# bind and scrub data
zoo_stxeur <- cbind(zoo_eurusd,
               zoo_stx[, "AdjClose"])
colnames(zoo_stxeur) <- c("EURUSD", "MSFT")
zoo_stxeur <-
  zoo_stxeur[complete.cases(zoo_stxeur),]
save(zoo_stx, zoo_stx_adj,
     ts_stx, ts_stx_adj,
     zoo_eurusd, zoo_stxeur,
     file="C:/Develop/R/lecture_slides/data/zoo_data.RData")
load(file="C:/Develop/R/lecture_slides/data/zoo_data.RData")
# Inspect the data
class(zoo_eurusd)
tail(zoo_eurusd, 4)
library(xtable)
# Define ETF symbols for asset allocation
sym_bols <- c("VTI", "VEU", "IEF", "VNQ",
  "DBC", "XLY", "XLP", "XLE", "XLF", "XLV",
  "XLI", "XLB", "XLK", "XLU", "VYM", "IVW", 
  "IWB", "IWD", "IWF", "VXX", "SVXY")
# Read etf database into data frame
etf_list <- read.csv(
  file='C:/Develop/R/lecture_slides/data/etf_list.csv', 
         stringsAsFactors=FALSE)
rownames(etf_list) <- etf_list$Symbol
# subset etf_list only those ETF's in sym_bols
etf_list <- etf_list[sym_bols, ]
# shorten names
etf_names <- sapply(etf_list$Name,
              function(name) {
  name_split <- strsplit(name, split=" ")[[1]]
  name_split <-
    name_split[c(-1, -NROW(name_split))]
  name_match <- match("Select", name_split)
  if (!is.na(name_match))
    name_split <- name_split[-name_match]
  paste(name_split, collapse=" ")
})  # end sapply
etf_list$Name <- etf_names
etf_list["IEF", "Name"] <- "Treasury Bond Fund"
etf_list["XLY", "Name"] <- "Consumer Discr. Sector Fund"
etf_list["SVXY", "Name"] <- "Short VIX Futures"
print(xtable(etf_list), comment=FALSE, size="tiny", include.rownames=FALSE)
library(tseries)  # Load package tseries
# Download price and volume data for sym_bols into list of zoo objects
zoo_series <- suppressWarnings(
  lapply(sym_bols, # Loop for loading data
   get.hist.quote,
   quote=c("AdjClose", "Volume"),
   start=Sys.Date()-3650,
   end=Sys.Date(),
   origin="1970-01-01")  # end lapply
)  # end suppressWarnings
# flatten list of zoo objects into a single zoo object
zoo_series <- rutils::do_call(cbind, zoo_series)
# Or
# zoo_series <- do.call(cbind, zoo_series)
# Assign names in format "symbol.Close", "symbol.Volume"
names(zoo_series) <-
  as.vector(sapply(sym_bols,
    paste, c("Close", "Volume"), sep="."))
# save zoo_series to a comma-separated CSV file
write.zoo(zoo_series, file='zoo_series.csv', sep=",")
# save zoo_series to a binary .RData file
save(zoo_series, file='zoo_series.RData')
library(HighFreq)  # Load package HighFreq
etf_env <- new.env()  # new environment for data
# Download data for sym_bols into etf_env from Alpha Vantage
getSymbols.av(sym_bols, adjust=TRUE, env=etf_env,
  output.size="full", api.key="T7JPW54ES8G75310")
# getSymbols(sym_bols, env=etf_env, adjust=TRUE, from="2005-01-03")
library(HighFreq)  # Load package HighFreq
ls(etf_env)  # List files in etf_env
# get class of object in etf_env
class(get(x=sym_bols[1], envir=etf_env))
# Another way
class(etf_env$VTI)
colnames(etf_env$VTI)
head(etf_env$VTI, 3)
# get class of all objects in etf_env
eapply(etf_env, class)
# get class of all objects in R workspace
lapply(ls(), function(ob_ject) class(get(ob_ject)))
library(HighFreq)  # Load package HighFreq
# Check of object is an OHLC time series
is.OHLC(etf_env$VTI)
# Adjust single OHLC object using its name
etf_env$VTI <- adjustOHLC(etf_env$VTI,
                    use.Adjusted=TRUE)

# Adjust OHLC object using string as name
assign(sym_bols[1], adjustOHLC(
    get(x=sym_bols[1], envir=etf_env),
    use.Adjusted=TRUE),
  envir=etf_env)

# Adjust objects in environment using vector of strings
for (sym_bol in ls(etf_env)) {
  assign(sym_bol,
   adjustOHLC(get(sym_bol, envir=etf_env),
              use.Adjusted=TRUE),
   envir=etf_env)
}  # end for
library(HighFreq)  # Load package HighFreq
# extract and cbind all data, subset by symbols
price_s <- rutils::do_call(cbind,
  as.list(etf_env)[sym_bols])
# Or
# price_s <- do.call(cbind,
#   as.list(etf_env)[sym_bols])
# extract and cbind adjusted prices, subset by symbols
price_s <- rutils::do_call(cbind,
  lapply(as.list(etf_env)[sym_bols], Ad))
# same, but works only for OHLC series
price_s <- rutils::do_call(cbind,
  eapply(etf_env, Ad)[sym_bols])
# Drop ".Adjusted" from colnames
colnames(price_s) <-
  sapply(colnames(price_s),
    function(col_name)
strsplit(col_name, split="[.]")[[1]],
    USE.NAMES=FALSE)[1, ]
tail(price_s[, 1:2], 3)
# which objects in global environment are class xts?
unlist(eapply(globalenv(), is.xts))
# save xts to csv file
write.zoo(price_s,
  file='etf_series.csv', sep=",")
# Copy price_s into etf_env and save to .RData file
assign("price_s", price_s, envir=etf_env)
save(etf_env, file='etf_data.RData')
# extract VTI prices
vt_i <- etf_env$price_s[ ,"VTI"]
vt_i <- na.omit(vt_i)
# Calculate percentage returns "by hand"
vti_lag <- as.numeric(vt_i)
vti_lag <- c(vti_lag[1], vti_lag[-NROW(vti_lag)])
vti_lag <- xts(vti_lag, index(vt_i))
vti_returns <- (vt_i-vti_lag)/vti_lag
# Calculate percentage returns using dailyReturn()
daily_returns <- quantmod::dailyReturn(vt_i)
head(cbind(daily_returns, vti_returns))
all.equal(daily_returns, vti_returns, check.attributes=FALSE)
# Calculate returns for all prices in etf_env$price_s
re_turns <- lapply(etf_env$price_s, function(x_ts) {
  daily_returns <- quantmod::dailyReturn(x_ts)
  colnames(daily_returns) <- names(x_ts)
  daily_returns
})  # end lapply
# "re_turns" is a list of xts
class(re_turns)
class(re_turns[[1]])
# flatten list of xts into a single xts
re_turns <- rutils::do_call(cbind, re_turns)
class(re_turns)
dim(re_turns)
# Copy re_turns into etf_env and save to .RData file
assign("re_turns", re_turns, envir=etf_env)
save(etf_env, file='etf_data.RData')
library(quantmod)
start_date <- "2012-05-10"; end_date <- "2013-11-20"
# subset all objects in environment and return as environment
new_env <- as.environment(eapply(etf_env, "[",
            paste(start_date, end_date, sep="/")))
# subset only sym_bols in environment and return as environment
new_env <- as.environment(
  lapply(as.list(etf_env)[sym_bols], "[",
   paste(start_date, end_date, sep="/")))
# extract and cbind adjusted prices and return to environment
assign("price_s", rutils::do_call(cbind,
         lapply(ls(etf_env), function(sym_bol) {
           x_ts <- Ad(get(sym_bol, etf_env))
           colnames(x_ts) <- sym_bol
           x_ts
         })), envir=new_env)
# get sizes of OHLC xts series in etf_env
sapply(mget(sym_bols, envir=etf_env), object.size)
# extract and cbind adjusted prices and return to environment
col_name <- function(x_ts)
  strsplit(colnames(x_ts), split="[.]")[[1]][1]
assign("price_s", rutils::do_call(cbind,
         lapply(mget(etf_env$sym_bols, envir=etf_env),
                function(x_ts) {
                  x_ts <- Ad(x_ts)
                  colnames(x_ts) <- col_name(x_ts)
                  x_ts
         })), envir=new_env)
library(HighFreq)  # Load package HighFreq
# Load data frame of S&P500 constituents from CSV file
sp_500 <- read.csv(file="C:/Develop/R/lecture_slides/data/sp500_WRDS_08-30-17.csv", stringsAsFactors=FALSE)
sym_bols <- sp_500$co_tic
# Get duplicate sym_bols
ta_ble <- table(sym_bols)
ta_ble[ta_ble>1]
sym_bols <- unique(sym_bols)
# Remove "BRK.B" and later download separately
sym_bols <- sym_bols[-which(sym_bols=="BRK.B")]
env_sp500 <- new.env()  # new environment for data
# Download in while loop from Tiingo and copy into environment
down_loaded <- sym_bols %in% ls(env_sp500)
it_er <- 0
while (((sum(!down_loaded)) > 0) & (it_er<10)) {
  # Boolean vector of symbols already downloaded
  down_loaded <- sym_bols %in% ls(env_sp500)
  # Download data and copy it into environment
  for (sym_bol in sym_bols[!down_loaded]) {
    cat("processing: ", sym_bol, "\n")
    tryCatch(  # with error handler
getSymbols(sym_bol, src="tiingo", adjust=TRUE,
  from="1990-01-01", env=env_sp500, api.key="j84ac2b9c5bde2d68e33034f65d838092c6c9f10"),
# error handler captures error condition
error=function(error_cond) {
  print(paste("error handler: ", error_cond))
},  # end error handler
finally=print(paste("sym_bol=", sym_bol))
    )  # end tryCatch
  }  # end for
  it_er <- it_er + 1
  Sys.sleep(2*60)
}  # end while
# Rename "LOW" colnames to "LO_WES"
colnames(env_sp500$LOW) <-
  sapply(colnames(env_sp500$LOW),
    function(col_name) {
col_name <- strsplit(col_name, split="[.]")[[1]]
paste("LO_WES", col_name[2], sep=".")
    })
env_sp500$LO_WES <- env_sp500$LOW[, unique(colnames(env_sp500$LOW))]
rm(LOW, envir=env_sp500)
# Download "BRK.B" separately
BRK_B <- getSymbols("BRK-B", auto.assign=FALSE, src="tiingo", adjust=TRUE, from="1990-01-01", api.key="j84ac2b9c5bde2d68e33034f65d838092c6c9f10")
colnames(BRK_B) <- paste("BRK_B", sapply(strsplit(colnames(BRK_B), split="[.]"), function(x) x[2]), sep=".")
env_sp500$BRK_B <- BRK_B
# Rename "BF-B" colnames to "BF_B"
colnames(env_sp500$"BF-B") <-
  sapply(colnames(env_sp500$"BF-B"),
    function(col_name) {
col_name <- strsplit(col_name, split="[.]")[[1]]
paste("BF_B", col_name[2], sep=".")
    })
env_sp500$BF_B <- env_sp500$"BF-B"
names(colnames(env_sp500$BF_B)) <- NULL
rm("BF-B", envir=env_sp500)
# Coerce the time indices from class POSIXct to class Date
for (sym_bol in ls(env_sp500)) {
  x_ts <- get(sym_bol, envir=env_sp500)
  index(x_ts) <- as.Date(index(x_ts))
  assign(sym_bol, x_ts, envir=env_sp500)
}  # end for
save(env_sp500,
  file="C:/Develop/R/lecture_slides/data/sp500.RData")
chart_Series(x=env_sp500$LO_WES["2017-06/"],
  TA="add_Vo()", name="LOWES stock")
# Remove all files from environment(if necessary)
rm(list=ls(env_sp500), envir=env_sp500)
# Download in while loop from Alpha Vantage and copy into environment
down_loaded <- sym_bols %in% ls(env_sp500)
it_er <- 0
while (((sum(!down_loaded)) > 0) & (it_er<10)) {
  # Boolean vector of symbols already downloaded
  down_loaded <- sym_bols %in% ls(env_sp500)
  # Download data and copy it into environment
  for (sym_bol in sym_bols[!down_loaded]) {
    cat("processing: ", sym_bol, "\n")
    tryCatch(  # with error handler
getSymbols(sym_bol, src="av", adjust=TRUE, env=env_sp500,
           output.size="full", api.key="T7JPW54ES8G75310"),
# error handler captures error condition
error=function(error_cond) {
  print(paste("error handler: ", error_cond))
},  # end error handler
finally=print(paste("sym_bol=", sym_bol))
    )  # end tryCatch
  }  # end for
  it_er <- it_er + 1
  Sys.sleep(2*60)
}  # end while
# Adjust all OHLC prices in environment
for (sym_bol in ls(env_sp500)) {
  assign(sym_bol,
    adjustOHLC(get(x=sym_bol, envir=env_sp500), use.Adjusted=TRUE),
    envir=env_sp500)
}  # end for
library(HighFreq)  # Load package HighFreq
# Assign name SP500 to ^GSPC symbol
setSymbolLookup(
  SP500=list(name="^GSPC", src="yahoo"))
getSymbolLookup()
# view and clear options
options("getSymbols.sources")
options(getSymbols.sources=NULL)
# Download S&P500 prices into etf_env
getSymbols("SP500", env=etf_env,
    adjust=TRUE, from="1990-01-01")
chart_Series(x=etf_env$SP500["2016/"],
       TA="add_Vo()",
       name="S&P500 index")
library(HighFreq)  # Load package HighFreq
# Assign name DJIA to ^DJI symbol
setSymbolLookup(
  DJIA=list(name="^DJI", src="yahoo"))
getSymbolLookup()
# view and clear options
options("getSymbols.sources")
options(getSymbols.sources=NULL)
# Download DJIA prices into etf_env
getSymbols("DJIA", env=etf_env,
    adjust=TRUE, from="1990-01-01")
chart_Series(x=etf_env$DJIA["2016/"],
       TA="add_Vo()",
       name="DJIA index")
library(HighFreq)  # Load package HighFreq
library(RCurl)  # Load package RCurl
library(XML)  # Load package XML
# Download text data from URL
sp_500 <- getURL(
  "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies")
# extract tables from the text data
sp_500 <- readHTMLTable(sp_500,
              stringsAsFactors=FALSE)
str(sp_500)
# extract colnames of data frames
lapply(sp_500, colnames)
# extract S&P500 constituents
sp_500 <- sp_500[[1]]
head(sp_500)
# Create valid R names from symbols containing "-" or "."characters
sp_500$names <- gsub("-", "_", sp_500$Ticker)
sp_500$names <- gsub("[.]", "_", sp_500$names)
# write data frame of S&P500 constituents to CSV file
write.csv(sp_500,
  file="C:/Develop/R/lecture_slides/data/sp500_Yahoo.csv",
  row.names=FALSE)
library(HighFreq)  # Load package HighFreq
# Load data frame of S&P500 constituents from CSV file
sp_500 <- read.csv(file="C:/Develop/R/lecture_slides/data/sp500_Yahoo.csv",
     stringsAsFactors=FALSE)
# Register symbols corresponding to R names
for (in_dex in 1:NROW(sp_500)) {
  cat("processing: ", sp_500$Ticker[in_dex], "\n")
  setSymbolLookup(structure(
    list(list(name=sp_500$Ticker[in_dex])),
    names=sp_500$names[in_dex]))
}  # end for
env_sp500 <- new.env()  # new environment for data
# Remove all files (if necessary)
rm(list=ls(env_sp500), envir=env_sp500)
# Download data and copy it into environment
rutils::get_symbols(sp_500$names,
   env_out=env_sp500, start_date="1990-01-01")
# Or download in loop
for (sym_bol in sp_500$names) {
  cat("processing: ", sym_bol, "\n")
  rutils::get_symbols(sym_bol,
   env_out=env_sp500, start_date="1990-01-01")
}  # end for
save(env_sp500, file="C:/Develop/R/lecture_slides/data/sp500.RData")
chart_Series(x=env_sp500$BRK_B["2016/"], TA="add_Vo()",
       name="BRK-B stock")
library(quantmod)
# Download U.S. unemployment rate data
unemp_rate <- getSymbols("UNRATE",
            auto.assign=FALSE,
            src="FRED")
# plot U.S. unemployment rate data
chart_Series(unemp_rate["1990/"],
      name="U.S. unemployment rate")
library(HighFreq)  # Load package HighFreq
install.packages("devtools")
library(devtools)
# Install package Quandl from github
install_github("quandl/R-package")
library(Quandl)  # Load package Quandl
# Register Quandl API key
Quandl.api_key("pVJi9Nv3V8CD3Js5s7Qx")
# get short description
packageDescription("Quandl")
# Load help page
help(package="Quandl")
# Remove Quandl from search path
detach("package:Quandl")
library(HighFreq)  # Load package HighFreq
# Download EOD AAPL prices from WIKI free database
price_s <- Quandl(code="WIKI/AAPL",
            type="xts", start_date="1990-01-01")
x11(width=14, height=7)
chart_Series(price_s["2016", 1:4],
    name="AAPL OHLC prices")
# Add trade volume in extra panel
add_TA(price_s["2016", 5])
# Download euro currency rates
price_s <- Quandl(code="BNP/USDEUR",
    start_date="2013-01-01",
    end_date="2013-12-01", type="xts")
# Download multiple time series
price_s <- Quandl(code=c("NSE/OIL", "WIKI/AAPL"),
    start_date="2013-01-01", type="xts")
# Download AAPL gross profits
prof_it <- Quandl("RAYMOND/AAPL_GROSS_PROFIT_Q",
    type="xts")
chart_Series(prof_it, name="AAPL gross profits")
# Download Hurst time series
price_s <- Quandl(code="PE/AAPL_HURST",
    start_date="2013-01-01", type="xts")
chart_Series(price_s["2016/", 1],
       name="AAPL Hurst")
library(HighFreq)  # Load package HighFreq
# Load S&P500 stock Quandl codes
sp_500 <- read.csv(
  file="C:/Develop/R/lecture_slides/data/sp500_quandl.csv",
  stringsAsFactors=FALSE)
# Replace "-" with "_" in symbols
sp_500$free_code <-
  gsub("-", "_", sp_500$free_code)
head(sp_500)
# vector of symbols in sp_500 frame
tick_ers <- gsub("-", "_", sp_500$ticker)
# Or
tick_ers <- matrix(unlist(
  strsplit(sp_500$free_code, split="/"),
  use.names=FALSE), ncol=2, byrow=TRUE)[, 2]
# Or
tick_ers <- do_call_rbind(
  strsplit(sp_500$free_code, split="/"))[, 2]
library(HighFreq)  # Load package HighFreq
env_sp500 <- new.env()  # new environment for data
# Remove all files (if necessary)
rm(list=ls(env_sp500), envir=env_sp500)
# Boolean vector of symbols already downloaded
down_loaded <- tick_ers %in% ls(env_sp500)
# Download data and copy it into environment
for (tick_er in tick_ers[!down_loaded]) {
  cat("processing: ", tick_er, "\n")
  da_ta <- Quandl(code=paste0("WIKI/", tick_er),
            start_date="1990-01-01",
            type="xts")[, -(1:7)]
  colnames(da_ta) <- paste(tick_er,
    c("Open", "High", "Low", "Close", "Volume"), sep=".")
  assign(tick_er, da_ta, envir=env_sp500)
}  # end for
save(env_sp500, file="C:/Develop/R/lecture_slides/data/sp500.RData")
chart_Series(x=env_sp500$XOM["2016/"], TA="add_Vo()",
       name="XOM stock")
library(HighFreq)
library(Quandl)
# Register Quandl API key
Quandl.api_key("pVJi9Nv3V8CD3Js5s7Qx")
# Download E-mini S&P500 futures prices
price_s <- Quandl(code="CHRIS/CME_ES1",
  type="xts", start_date="1990-01-01")
price_s <- price_s[, c("Open", "High", "Low", "Last", "Volume")]
colnames(price_s)[4] <- "Close"
# plot the prices
x11(width=5, height=4)  # Open x11 for plotting
chart_Series(x=price_s["2008-06/2009-06"],
       TA="add_Vo()",
       name="S&P500 Futures")
# plot dygraph
dygraphs::dygraph(price_s["2008-06/2009-06", -5],
  main="S&P500 Futures") %>%
  dyCandlestick()
# Read CBOE futures expiration dates
date_s <- read.csv(file="C:/Develop/R/lecture_slides/data/futures_expiration_dates_codes.csv",
  stringsAsFactors=FALSE, row.names=1)
data_dir <- "C:/Develop/data/vix_data"
dir.create(data_dir)
sym_bols <- rownames(date_s)
file_names <- file.path(data_dir, paste0(sym_bols, ".csv"))
log_file <- file.path(data_dir, "log_file.txt")
cboe_url <- "https://markets.cboe.com/us/futures/market_statistics/historical_data/products/csv/VX/"
url_s <- paste0(cboe_url, date_s[, 1])
# Download files in loop
for (it in seq_along(url_s)) {
    tryCatch(  # warning and error handler
  download.file(url_s[it],
          destfile=file_names[it], quiet=TRUE),
# warning handler captures warning condition
warning=function(warning_cond) {
  cat(paste("warning handler: ", warning_cond, "\n"), file=log_file, append=TRUE)
},  # end warning handler
# error handler captures error condition
error=function(error_cond) {
  cat(paste("error handler: ", error_cond, "\n"), append=TRUE)
},  # end error handler
finally=cat(paste("Processing file name =", file_names[it], "\n"), append=TRUE)
    )  # end tryCatch
}  # end for
# Create new environment for data
vix_env <- new.env()
# Download VIX data for the months 6, 7, and 8 in 2018
library(qmao)
quantmod::getSymbols("VX", Months=1:12,
  Years=2018, src="cfe", env=vix_env)
# Or
qmao::getSymbols.cfe(Symbols="VX",
  Months=6:8, Years=2018, env=vix_env,
  verbose=FALSE, auto.assign=TRUE)
# Calculate the classes of all the objects
# In the environment vix_env
unlist(eapply(vix_env,
  function(x) {class(x)[1]}))
class(vix_env$VX_M18)
colnames(vix_env$VX_M18)
# Save the data to a binary file called "vix_cboe.RData".
save(vix_env,
  file="C:/Develop/data/vix_data/vix_cboe.RData")
