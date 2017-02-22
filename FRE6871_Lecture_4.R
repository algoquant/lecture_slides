library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
options(digits=3)
thm <- knit_theme$get("acid")
knit_theme$set(thm)
# Define model parameters
num_assets <- 300
num_simu <- 1000
l_gd <- 0.4
# define correlation parameters
rh_o <- 0.08
rho_sqrt <- sqrt(rh_o)
rho_sqrtm <- sqrt(1-rh_o)
# calculate default probabilities and thresholds
set.seed(1121)
default_probs <- runif(num_assets, max=0.1)
default_thresh <- qnorm(default_probs)
# calculate vector of systematic factors
system_atic <- rnorm(num_simu)
# simulate losses under Vasicek model
asset_values <- matrix(rnorm(num_simu*num_assets), ncol=num_simu)
asset_values <- t(rho_sqrt*system_atic + t(rho_sqrtm*asset_values))
loss_es <-
  l_gd*colSums(asset_values < default_thresh)/num_assets
# calculate VaRs
conf_levels <- seq(0.93, 0.99, 0.01)
var_s <- quantile(loss_es, probs=conf_levels)
names(var_s) <- round(conf_levels, 3)
plot(x=conf_levels, y=var_s, t="l",
     main="Simulated VaR and confidence levels")
# calculate frequency table of loss_es
table_losses <- table(loss_es)/num_simu
# calculate CVaRs
cvar_s <- sapply(var_s, function(v_ar) {
  tai_l <- table_losses[v_ar < names(table_losses)]
  tai_l %*% as.numeric(names(tai_l)) / sum(tai_l)
})  # end sapply
cvar_s <- cbind(cvar_s, var_s)
# plot CVaRs
plot(x=rownames(cvar_s), y=cvar_s[, "cvar_s"],
     t="l", col="red", lwd=2,
     ylim=range(cvar_s),
     xlab="conf_levels", ylab="CVaRs",
     main="Simulated CVaR and confidence levels")
# add VaRs
lines(x=rownames(cvar_s), y=cvar_s[, "var_s"], lwd=2)
# add legend
legend(x="topleft", legend=c("CVaRs", "VaRs"),
 title=NULL, inset=0.05, cex=0.8, bg="white",
 lwd=6, lty=c(1, 1), col=c("red", "black"))
calc_var <- function(default_probs,
               l_gd=0.6,
               rh_o=0.08,
               num_simu=1000,
               conf_levels=seq(0.93, 0.99, 0.01)) {
  # Define model parameters
  num_assets <- NROW(default_probs)
  default_thresh <- qnorm(default_probs)
  # Define correlation parameters
  rho_sqrt <- sqrt(rh_o)
  rho_sqrtm <- sqrt(1-rh_o)
  # Simulate losses under Vasicek model
  system_atic <- rnorm(num_simu)
  asset_values <- matrix(rnorm(num_simu*num_assets), ncol=num_simu)
  asset_values <- t(rho_sqrt*system_atic + t(rho_sqrtm*asset_values))
  loss_es <- l_gd*colSums(asset_values < default_thresh)/num_assets
  # Calculate VaRs
  var_s <- quantile(loss_es, probs=conf_levels)
  names(var_s) <- round(conf_levels, 3)
  # Calculate CVaRs from the frequency table
  table_losses <- table(loss_es)/num_simu
  cvar_s <- sapply(var_s, function(v_ar) {
    tai_l <- table_losses[v_ar < names(table_losses)]
    tai_l %*% as.numeric(names(tai_l)) / sum(tai_l)
  })  # end sapply
  names(cvar_s) <- round(conf_levels, 3)
  c(var_s, cvar_s)
}  # end calc_var
# define number of bootstrap simulations
num_boot <- 500
sample_length <- NROW(default_probs)
# perform bootstrap of calc_var
set.seed(1121)
boot_strap <- sapply(rep(l_gd, num_boot),
  calc_var,
  default_probs=
    default_probs[sample.int(sample_length, replace=TRUE)],
  rh_o=rh_o, num_simu=num_simu,
  conf_levels=conf_levels)  # end sapply
boot_strap <- t(boot_strap)
# calculate vectors of standard errors of VaR and CVaR from boot_strap data
std_error_var <- apply(boot_strap[, 1:7], MARGIN=2, sd)
std_error_cvar <- apply(boot_strap[, 8:14], MARGIN=2, sd)
# plot the standard errors of VaRs and CVaRs
plot(x=names(std_error_cvar),
  y=std_error_cvar/cvar_s[, 1],
  t="l", col="red", lwd=2,
  ylim=range(c(std_error_cvar/cvar_s[, 1],
    std_error_var/cvar_s[, 2])),
  xlab="conf_levels", ylab="CVaRs",
  main="Scaled standard errors of CVaR and VaR")
lines(x=names(std_error_var), y=std_error_var/cvar_s[, 2])
legend(x="topleft", legend=c("CVaRs", "VaRs"),
 title=NULL, inset=0.05, cex=0.8, bg="white",
 lwd=6, lty=c(1, 1), col=c("red", "black"))
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
type <- c('rose', 'daisy', 'tulip')  # character vector
color <- c('red', 'white', 'yellow')  # character vector
price <- c(1.5, 0.5, 1.0)  # numeric vector
# create a data frame
data_frame <- data.frame(type, color, price)
# assign rownames
rownames(data_frame) <- c('flower1', 'flower2', 'flower3')
sort_data <- sample(1:6)  # permute data
sort_data
sort(sort_data)  # sorted data
order(sort_data)  # permution index
sort_data[order(sort_data)]  # permution index
order(data_frame$price)  # permute on price
data_frame[order(data_frame$price), ]  # sort on price
data_frame[order(data_frame$color), ]  # sort on color
order(c(2, 1:4))  # there's a tie
order(c(2, 1:4), 1:5)  # there's a tie
# read sort() Examples
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
good_air <- na.locf(airquality)
dim(good_air)
head(good_air)  # NAs replaced
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
# load package quantmod
library(quantmod)
# get documentation for package quantmod
# get short description
packageDescription("quantmod")
# load help page
help(package="quantmod")
# list all datasets in "quantmod"
data(package="quantmod")
# list all objects in "quantmod"
ls("package:quantmod")
# remove quantmod from search path
detach("package:quantmod")
library(quantmod)
library(TTR)
oh_lc <- env_etf$VTI
VTI_adj <- Ad(oh_lc)
VTI_vol <- Vo(oh_lc)
VTI_vwap <- TTR::VWAP(price=VTI_adj, volume=VTI_vol, n=10)
VTI_adj <- VTI_adj["2009-02/2009-03"]
oh_lc <- oh_lc["2009-02/2009-03"]
VTI_vwap <- VTI_vwap["2009-02/2009-03"]
# OHLC candlechart VWAP in main plot,
chart_Series(x=oh_lc, # volume in extra panel
       TA="add_Vo(); add_TA(VTI_vwap, on=1)",
       name="VTI plus VWAP shaded")
# add price minus VWAP in extra panel
add_TA(VTI_adj-VTI_vwap, col='red')
# add background shading of areas
add_TA((VTI_adj-VTI_vwap) > 0, on=-1,
col="lightgreen", border="lightgreen")
add_TA((VTI_adj-VTI_vwap) < 0, on=-1,
col="lightgrey", border="lightgrey")
# add vertical and horizontal lines
abline(v=which.min(VTI_vwap), col='red')
abline(h=min(VTI_vwap), col='red')
library(quantmod)
oh_lc <- env_etf$VTI["2009-02/2009-03"]
# extract plot object
ch_ob <- chart_Series(x=oh_lc, plot=FALSE)
class(ch_ob)
ls(ch_ob)
class(ch_ob$get_ylim)
class(ch_ob$set_ylim)
# ls(ch_ob$Env)
class(ch_ob$Env$actions)
plot_theme <- chart_theme()
class(plot_theme)
ls(plot_theme)
library(quantmod)
oh_lc <- env_etf$VTI["2010-04/2010-05"]
# extract, modify theme, format tick marks "%b %d"
plot_theme <- chart_theme()
plot_theme$format.labels <- "%b %d"
# create plot object
ch_ob <- chart_Series(x=oh_lc,
                theme=plot_theme, plot=FALSE)
# extract ylim using accessor function
y_lim <- ch_ob$get_ylim()
y_lim[[2]] <- structure(
  range(Ad(oh_lc)) + c(-1, 1),
  fixed=TRUE)
# modify plot object to reduce y-axis range
ch_ob$set_ylim(y_lim)  # use setter function
# render the plot
plot(ch_ob)
library(quantmod)
# calculate VTI volume-weighted average price
VTI_vwap <- TTR::VWAP(price=Ad(env_etf$VTI),
    volume=Vo(env_etf$VTI), n=10)
# calculate XLF volume-weighted average price
XLF_vwap <- TTR::VWAP(price=Ad(env_etf$XLF),
    volume=Vo(env_etf$XLF), n=10)
# open plot graphics device
x11()
# define plot area with two horizontal panels
par(mfrow=c(2, 1))
# plot in top panel
invisible(chart_Series(
  x=env_etf$VTI["2009-02/2009-04"], name="VTI"))
add_TA(VTI_vwap["2009-02/2009-04"], lwd=2, on=1, col='blue')
# plot in bottom panel
invisible(chart_Series(
  x=env_etf$XLF["2009-02/2009-04"], name="XLF"))
add_TA(XLF_vwap["2009-02/2009-04"], lwd=2, on=1, col='blue')
library(quantmod)
# download U.S. unemployment rate data
unemp_rate <- getSymbols("UNRATE",
            auto.assign=FALSE,
            src="FRED")
# plot U.S. unemployment rate data
chart_Series(unemp_rate["1990/"],
      name="U.S. unemployment rate")
# symbols for constant maturity Treasury rates
sym_bols <- c("DGS1", "DGS2", "DGS5", "DGS10", "DGS30")
library(quantmod)  # load package quantmod
env_rates <- new.env()  # new environment for data
# download data for sym_bols into env_rates
getSymbols(sym_bols, env=env_rates, src="FRED")
ls(env_rates)  # list files in env_rates
# get class of object in env_rates
class(get(x=sym_bols[1], envir=env_rates))
# another way
class(env_rates$DGS10)
colnames(env_rates$DGS10)
head(env_rates$DGS10, 3)
# get class of all objects in env_rates
eapply(env_rates, class)
# get class of all objects in R workspace
lapply(ls(), function(ob_ject) class(get(ob_ject)))
# plot 10-year constant maturity Treasury rate
chart_Series(env_rates$DGS10["1990/"],
      name="10-year constant maturity Treasury rate")
library(parallel)  # load package parallel
# get short description
packageDescription("parallel")
# load help page
help(package="parallel")
# list all objects in "parallel"
ls("package:parallel")
library(parallel)  # load package parallel
# calculate number of available cores
num_cores <- detectCores() - 1
# define function that pauses execution
paws <- function(x, sleep_time) {
  Sys.sleep(sleep_time)
  x
}  # end paws
# perform parallel loop under Mac-OSX or Linux
foo <- mclapply(1:10, paws, mc.cores=num_cores,
          sleep_time=0.01)
# initialize compute cluster
clus_ter <- makeCluster(num_cores)
# perform parallel loop under Windows
foo <- parLapply(clus_ter, 1:10, paws,
           sleep_time=0.01)
library(microbenchmark)  # load package microbenchmark
# compare speed of lapply versus parallel computing
summary(microbenchmark(
  l_apply=lapply(1:10, paws, sleep_time=0.01),
  parl_apply=
    parLapply(clus_ter, 1:10, paws, sleep_time=0.01),
  times=10)
)[, c(1, 4, 5)]
# stop R processes over cluster
stopCluster(clus_ter)
library(parallel)  # load package parallel
# calculate number of available cores
num_cores <- detectCores() - 1
# initialize compute cluster
clus_ter <- makeCluster(num_cores)
# define function that pauses execution
paws <- function(x, sleep_time) {
  Sys.sleep(sleep_time)
  x
}  # end paws
# compare speed of lapply with parallel computing
iter_ations <- 3:10
compute_times <- sapply(iter_ations,
  function(max_iterations, sleep_time) {
    out_put <- summary(microbenchmark(
lapply=lapply(1:max_iterations, paws,
              sleep_time=sleep_time),
parallel=parLapply(clus_ter, 1:max_iterations,
        paws, sleep_time=sleep_time),
times=10))[, c(1, 4)]
    structure(out_put[, 2],
        names=as.vector(out_put[, 1]))
    }, sleep_time=0.01)
compute_times <- t(compute_times)
rownames(compute_times) <- iter_ations
library(parallel)  # load package parallel
plot(x=rownames(compute_times),
     y=compute_times[, "lapply"],
     type="l", lwd=2, col="blue",
     main="Compute times",
     xlab="number of iterations in loop", ylab="",
     ylim=c(0, max(compute_times[, "lapply"])))
lines(x=rownames(compute_times),
y=compute_times[, "parallel"], lwd=2, col="green")
legend(x="topleft", legend=colnames(compute_times),
 inset=0.1, cex=1.0, bg="white",
 lwd=2, lty=c(1, 1), col=c("blue", "green"))
library(parallel)  # load package parallel
# calculate number of available cores
num_cores <- detectCores() - 1
# initialize compute cluster
clus_ter <- makeCluster(num_cores)
# define large matrix
mat_rix <- matrix(rnorm(7*10^5), ncol=7)
# define aggregation function over column of matrix
agg_regate <- function(col_umn) {
  out_put <- 0
  for (in_dex in 1:length(col_umn))
    out_put <- out_put + col_umn[in_dex]
  out_put
}  # end agg_regate
# perform parallel aggregations over columns of matrix
agg_regations <-
  parCapply(clus_ter, mat_rix, agg_regate)
# compare speed of apply with parallel computing
summary(microbenchmark(
  ap_ply=apply(mat_rix, MARGIN=2, agg_regate),
  parl_apply=
    parCapply(clus_ter, mat_rix, agg_regate),
  times=10)
)[, c(1, 4, 5)]
# stop R processes over cluster
stopCluster(clus_ter)
set.seed(1121)  # reset random number generator
# sample from Standard Normal Distribution
sample_length <- 1000
sam_ple <- rnorm(sample_length)
# sample mean
mean(sam_ple)
# sample standard deviation
sd(sam_ple)
# bootstrap of sample mean and median
boot_strap <- sapply(1:10000, function(x) {
  boot_sample <-
    sam_ple[sample.int(sample_length,
                 replace=TRUE)]
  c(mean=mean(boot_sample),
    median=median(boot_sample))
})  # end sapply
boot_strap[, 1:3]
# standard error from formula
sd(sam_ple)/sqrt(sample_length)
# standard error of mean from bootstrap
sd(boot_strap["mean", ])
# standard error of median from bootstrap
sd(boot_strap["median", ])
library(parallel)  # load package parallel
num_cores <- detectCores() - 1  # number of cores
clus_ter <- makeCluster(num_cores)  # initialize compute cluster
set.seed(1121)  # reset random number generator
# sample from Standard Normal Distribution
sample_length <- 1000
sam_ple <- rnorm(sample_length)
# bootstrap mean and median under Windows
boot_strap <- parLapply(clus_ter, 1:10000,
  function(x, sam_ple, sample_length) {
  boot_sample <- sam_ple[sample.int(sample_length, replace=TRUE)]
  c(mean=mean(boot_sample), median=median(boot_sample))
  }, sam_ple=sam_ple, sample_length=sample_length)  # end parLapply
# bootstrap mean and median under Mac-OSX or Linux
boot_strap <- mclapply(1:10000,
  function(x) {
  boot_sample <- sam_ple[sample.int(sample_length, replace=TRUE)]
  c(mean=mean(boot_sample), median=median(boot_sample))
  }, mc.cores=num_cores)  # end mclapply
boot_strap <- do.call(rbind, boot_strap)
# means and standard errors from bootstrap
apply(boot_strap, MARGIN=2,
function(x) c(mean=mean(x), sd=sd(x)))
# standard error from formula
sd(sam_ple)/sqrt(sample_length)
stopCluster(clus_ter)  # stop R processes over cluster
