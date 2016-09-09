library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
options(digits=3)
thm <- knit_theme$get("acid")
knit_theme$set(thm)
getOption("repos")  # get default package source
.libPaths()  # get package save directory
## install.packages("AER")  # install "AER" from CRAN
## # install "PerformanceAnalytics" from R-Forge
## install.packages(
##   pkgs="PerformanceAnalytics",  # name
##   lib="C:/Users/Jerzy/Downloads",  # directory
##   repos="http://R-Forge.R-project.org")  # source
## # install devtools from CRAN
## install.packages("devtools")
## # load devtools
## library(devtools)
## # install package "babynames" from GitHub
## install_github(repo="hadley/babynames")
## # install package "PortfolioAnalytics" from source
## install.packages("PortfolioAnalytics",
##   type="source",
##   repos="http://r-forge.r-project.org")
## # download files for package "PortfolioAnalytics"
## download.packages(pkgs = "PortfolioAnalytics",
##   destdir = ".",  # download to cwd
##   type = "source",
##   repos="http://r-forge.r-project.org")
## # install "PortfolioAnalytics" from local tar source
## install.packages(
##   "C:/Users/Jerzy/Downloads/PortfolioAnalytics_0.9.3598.tar.gz",
##   repos=NULL, type="source")
getOption("defaultPackages")
pack_info <- installed.packages()  # matrix of packages
# get a few package names and their versions
pack_info[sample(x=1:100, 5), c("Package", "Version")]
t(pack_info["xts", ])  # get info for package "xts"
# list directories in "PortfolioAnalytics" sub-directory
gsub(
  "C:/Users/Jerzy/Documents/R/win-library/3.1", 
  "~",
  list.dirs(
    file.path(
      .libPaths()[1], 
      "PortfolioAnalytics")))
## # load package, produce error if can't be loaded
## library(MASS)
## # load package, return TRUE if loaded successfully
## require(MASS)
## # load quietly
## library(MASS, quietly=TRUE)
## # load without any messages
## suppressMessages(library(MASS))
## # remove package from search path
## detach(MASS)
## # install package if it can't be loaded successfully
## if (!require("xts")) install.packages("xts")
## library()  # list all packages installed on the system
## search()  # list all loaded packages on search path
## 
## # get documentation for package "Ecdat"
## packageDescription("Ecdat")  # get short description
## help(package="Ecdat")  # load help page
## library(Ecdat)  # load package "Ecdat"
## data(package="Ecdat")  # list all datasets in "Ecdat"
## ls("package:Ecdat")  # list all objects in "Ecdat"
## detach("package:Ecdat")  # remove Ecdat from search path
library(Ecdat)  # load econometric data sets
class(Garch)  # Garch is a data frame from "Ecdat"
dim(Garch)  # daily currency prices
head(Garch[, -2])  # col 'dm' is Deutsch Mark
detach("package:Ecdat")  # remove Ecdat from search path
rm(list=ls())
search()  # get search path for R objects
library(MASS)  # load package "MASS"
head(ls("package:MASS"))  # list some objects in "MASS"
detach("package:MASS")  # remove "MASS" from search path
loadedNamespaces()  # get names of loaded namespaces

search()  # get search path for R objects
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
fac_tor
levels(fac_tor)  # get allowed values
unique(fac_tor)  # get unique elements
# get contingency (frequency) table
table(fac_tor)
# get contingency table using sapply
sapply(levels(fac_tor), 
 function(le_vel) {
   sum(fac_tor==le_vel)
 })  # end sapply
library(microbenchmark)
str(findInterval)
# get index of the element of "vec" that matches 5
findInterval(x=5, vec=c(3, 5, 7))
match(5, c(3, 5, 7))
# no exact match
findInterval(x=6, vec=c(3, 5, 7))
match(6, c(3, 5, 7))
# indices of "vec" that match elements of "x"
findInterval(x=1:8, vec=c(3, 5, 7))
# return only indices of inside intervals
findInterval(x=1:8, vec=c(3, 5, 7), 
       all.inside=TRUE)
# make rightmost interval inclusive
findInterval(x=1:8, vec=c(3, 5, 7), 
       rightmost.closed=TRUE)
## # named numeric vector of breakpoints
## brea_ks <- c("freezing"=0, "very_cold"=30,
##        "cold"=50, "pleasant"=60,
##        "warm"=80, "hot"=90)
## brea_ks
## tempe_ratures <- runif(10, min=10, max=100)
## feels_like <- names(
##   brea_ks[findInterval(x=tempe_ratures,
##                  vec=brea_ks)])
## names(tempe_ratures) <- feels_like
## tempe_ratures
## library(microbenchmark)
## foo <- sample(0:6) + 0.1
## foo
## cut(x=foo, breaks=c(2, 4, 6, 8))
## rbind(foo, cut(x=foo, breaks=c(2, 4, 6, 8)))
## # cut() replicates findInterval()
## cut(x=1:8, breaks=c(3, 5, 7), labels=1:2,
##     right=FALSE)
## findInterval(x=1:8, vec=c(3, 5, 7))
## # findInterval() is a compiled function, so it's faster than cut()
## vec_tor <- rnorm(1000)
## summary(microbenchmark(
##   find_interval=
##     findInterval(x=vec_tor, vec=c(3, 5, 7)),
##   cuut=
##     cut(x=vec_tor, breaks=c(3, 5, 7)),
##   times=10))[, c(1, 4, 5)]  # end microbenchmark summary
vec_tor1 <- rnorm(1000000)
vec_tor2 <- rnorm(1000000)
big_vector <- numeric(1000000)
system.time(  # sum vectors using "for" loop
  for(i in 1:length(vec_tor1)) {
    big_vector[i] <- vec_tor1[i] + vec_tor2[i]
  }  # end for
)  # end system.time
# sum vectors using vectorized "+"
system.time(big_vector <- vec_tor1 + vec_tor2)
# allocate memory for cumulative sum
cum_sum <- numeric(length(big_vector))
# cumulative sum using "for" loop
cum_sum[1] <- big_vector[1]
system.time(
  for(i in 2:length(big_vector)) {
    cum_sum[i] <- cum_sum[i-1] + big_vector[i]
  }  # end for
)  # end system.time
# cumulative sum using "cumsum"
system.time(cum_sum <- cumsum(big_vector))
## # calculate row sums two different ways
## summary(microbenchmark(
##   row_sums=rowSums(big_matrix),
##   ap_ply=apply(big_matrix, 1, sum),
##   times=10))[, c(1, 4, 5)]  # end microbenchmark summary
## library(microbenchmark)
## str(pmax)
## # calculate row maximums two different ways
## summary(microbenchmark(
##   p_max=
##     do.call(pmax.int,
## lapply(seq_along(big_matrix[1, ]),
##   function(in_dex) big_matrix[, in_dex])),
##   l_apply=unlist(
##     lapply(seq_along(big_matrix[, 1]),
##   function(in_dex) max(big_matrix[in_dex, ]))),
##   times=10))[, c(1, 4, 5)]
## library(matrixStats)  # load package matrixStats
## # calculate row min values three different ways
## summary(microbenchmark(
##   row_mins=rowMins(big_matrix),
##   p_min=
##     do.call(pmin.int,
##       lapply(seq_along(big_matrix[1, ]),
##              function(in_dex)
##                big_matrix[, in_dex])),
##   as_data_frame=
##     do.call(pmin.int,
##       as.data.frame.matrix(big_matrix)),
##   times=10))[, c(1, 4, 5)]  # end microbenchmark summary
## summary(microbenchmark(  # assign values to vector three different ways
## # fast vectorized assignment loop performed in C using brackets "[]"
##   brack_ets={vec_tor <- numeric(10)
##     vec_tor[] <- 2},
## # slow because loop is performed in R
##   for_loop={vec_tor <- numeric(10)
##     for (in_dex in seq_along(vec_tor))
##       vec_tor[in_dex] <- 2},
## # very slow because no memory is pre-allocated
## # "vec_tor" is "grown" with each new element
##   grow_vec={vec_tor <- numeric(0)
##     for (in_dex in 1:10)
## # add new element to "vec_tor" ("grow" it)
##       vec_tor[in_dex] <- 2},
##   times=10))[, c(1, 4, 5)]  # end microbenchmark summary
## summary(microbenchmark(  # assign values to vector two different ways
## # fast vectorized assignment loop performed in C using brackets "[]"
##   brack_ets={vec_tor <- numeric(10)
##     vec_tor[4:7] <- rnorm(4)},
## # slow because loop is performed in R
##   for_loop={vec_tor <- numeric(10)
##     for (in_dex in 4:7)
##       vec_tor[in_dex] <- rnorm(1)},
##   times=10))[, c(1, 4, 5)]  # end microbenchmark summary
## load(file="C:/Develop/data/etf_data.RData")
## # define function vectorized automatically
## my_fun <- function(in_put, pa_ram) {
##   pa_ram*in_put
## }  # end my_fun
## # "in_put" is vectorized
## my_fun(in_put=1:3, pa_ram=2)
## # "pa_ram" is vectorized
## my_fun(in_put=10, pa_ram=2:4)
## # define vectors of parameters of rnorm()
## std_devs <-
##   structure(1:3, names=paste0("sd=", 1:3))
## me_ans <-
##   structure(-1:1, names=paste0("mean=", -1:1))
## # "sd" argument of rnorm() isn't vectorized
## rnorm(1, sd=std_devs)
## # "mean" argument of rnorm() isn't vectorized
## rnorm(1, mean=me_ans)
## load(file="C:/Develop/data/etf_data.RData")
## # sapply produces desired vector output
## set.seed(1121)
## sapply(std_devs, function(sd) rnorm(n=2, sd=sd))
## set.seed(1121)
## sapply(std_devs, rnorm, n=2, mean=0)
## set.seed(1121)
## sapply(me_ans,
##  function(mean) rnorm(n=2, mean=mean))
## set.seed(1121)
## sapply(me_ans, rnorm, n=2)
## load(file="C:/Develop/data/etf_data.RData")
## # rnorm() vectorized with respect to "sd"
## vec_rnorm <- function(n, mean=0, sd=1) {
##   if (length(sd)==1)
##     rnorm(n=n, mean=mean, sd=sd)
##   else
##     sapply(sd, rnorm, n=n, mean=mean)
## }  # end vec_rnorm
## set.seed(1121)
## vec_rnorm(n=2, sd=std_devs)
## # rnorm() vectorized to "mean" and "sd"
## vec_rnorm <- Vectorize(FUN=rnorm,
##         vectorize.args=c("mean", "sd")
## )  # end Vectorize
## set.seed(1121)
## vec_rnorm(n=2, sd=std_devs)
## set.seed(1121)
## vec_rnorm(n=2, mean=me_ans)
## load(file="C:/Develop/data/etf_data.RData")
## str(sum)
## # na.rm is bound by name
## mapply(sum, 6:9, c(5, NA, 3), 2:6, na.rm=TRUE)
## str(rnorm)
## # mapply vectorizes both arguments "mean" and "sd"
## mapply(rnorm, n=5, mean=me_ans, sd=std_devs)
## mapply(function(in_put, e_xp) in_put^e_xp,
##  1:5, seq(from=1, by=0.2, length.out=5))
## load(file="C:/Develop/data/etf_data.RData")
## # rnorm() vectorized with respect to "mean" and "sd"
## vec_rnorm <- function(n, mean=0, sd=1) {
##   if (length(mean)==1 && length(sd)==1)
##     rnorm(n=n, mean=mean, sd=sd)
##   else
##     mapply(rnorm, n=n, mean=mean, sd=sd)
## }  # end vec_rnorm
## # call vec_rnorm() on vector of "sd"
## vec_rnorm(n=2, sd=std_devs)
## # call vec_rnorm() on vector of "mean"
## vec_rnorm(n=2, mean=me_ans)
## # create two numeric vectors
## vec_tor1 <- sin(0.25*pi*1:10)
## vec_tor2 <- cos(0.25*pi*1:10)
## # create third vector using 'ifelse'
## vec_tor3 <- ifelse(vec_tor1 > vec_tor2,
##           vec_tor1, vec_tor2)
## # cbind all three together
## vec_tor4 <- cbind(vec_tor1, vec_tor2, vec_tor3)
## 
## # set plotting parameters
## par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0),
##     cex.lab=0.8, cex.axis=0.8, cex.main=0.8,
##     cex.sub=0.5)
## # plot matrix
## matplot(vec_tor4, type="l", lty="solid",
## col=c("green", "blue", "red"),
## lwd=c(2, 2, 2), xlab="", ylab="")
## # add legend
## legend(x="bottomright", legend=colnames(vec_tor4),
##        title="", inset=0.05, cex=0.8, lwd=2,
##        lty=c(1, 1, 1), col=c("green", "blue", "red"))
## par(oma=c(1, 1, 1, 1), mgp=c(2, 1, 0), mar=c(5, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
## set.seed(1121)  # reset random number generator
## simu_length <- 1000  # number of simulation steps
## simu_prices <- numeric(simu_length)  # initialize prices
## barrier_level <- 20  # barrier level
## simu_prices[1] <- 0  # first simulated price
## in_dex <- 2  # initialize simulation index
## while ((in_dex <= simu_length) &&
##  (simu_prices[in_dex - 1] < barrier_level)) {
##   simu_prices[in_dex] <- # simulate next price
##     simu_prices[in_dex - 1] + rnorm(1)
##   in_dex <- in_dex + 1  # advance in_dex
## }  # end while
## if (in_dex <= simu_length) {  # fill zero prices
##   simu_prices[in_dex:simu_length] <- simu_prices[in_dex - 1]
## }
## # create daily time series starting 2011
## ts_var <- ts(data=simu_prices, frequency=365, start=c(2011, 1))
## plot(ts_var, type="l", col="black",  # create plot
##      lty="solid", xlab="", ylab="")
## abline(h=barrier_level, lwd=2, col="red")  # add horizontal line
## title(main="Random Prices", line=0)  # add title
## par(oma=c(1, 1, 1, 1), mgp=c(2, 1, 0), mar=c(5, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
## set.seed(1121)  # reset random number generator
## simu_length <- 1000  # number of simulation steps
## barrier_level <- 20  # barrier level
## # simulate prices
## simu_prices <- cumsum(rnorm(simu_length))
## # find index when prices cross barrier_level
## which_index <- which(simu_prices > barrier_level)
## # fill prices after crossing barrier_level
## if (length(which_index)>0) {
##   simu_prices[(which_index[1]+1):simu_length] <-
##     simu_prices[which_index[1]]
## }  # end if
## # create daily time series starting 2011
## ts_var <- ts(data=simu_prices, frequency=365,
##      start=c(2011, 1))
## plot(ts_var, type="l", col="black",  # create plot
##      lty="solid", xlab="", ylab="")
## abline(h=barrier_level, lwd=2, col="red")  # add horizontal line
## title(main="Random Prices", line=0)  # add title
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

save(my_text, file="mytext.RData")  # write to binary file
print(pi)
print(pi, digits=10)
getOption("digits")
foo <- 12
bar <- "months"
sprintf("There are %i %s in the year", foo, bar)
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
setwd("C:/Develop/data")
data_frame <- data.frame(type=c("rose", "daisy", "tulip"), color=c("red", "white", "yellow"), price=c(1.5, 0.5, 1.0), row.names=c("flower1", "flower2", "flower3"))  # end data.frame
mat_rix <- matrix(sample(1:12), ncol=3, dimnames=list(NULL, c("col1", "col2", "col3")))
rownames(mat_rix) <- paste("row", 1:nrow(mat_rix), sep="")
# write data frame to text file, and then read it back
write.table(data_frame, file="florist.txt")
data_read <- read.table(file="florist.txt")
data_read  # a data frame

# write matrix to text file, and then read it back
write.table(mat_rix, file="matrix.txt")
mat_read <- read.table(file="matrix.txt")
mat_read  # write.table() coerced matrix to data frame
class(mat_read)
# coerce from data frame back to matrix
mat_read <- as.matrix(mat_read)
class(mat_read)
## setwd("C:/Develop/data")
## data_frame <- data.frame(small=c(3, 5), medium=c(9, 11), large=c(15, 13))
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
## write_clip <- function(data, row.names=FALSE,
##                col.names=TRUE, ...) {
##   write.table(x=data, file="clipboard", sep="\t",
##       row.names=row.names, col.names=col.names, ...)
## }  # end write_clip
## 
## write_clip(data=data_frame)
## 
## # launch spreadsheet-style data editor
## data_frame <- edit(data_frame)
setwd("C:/Develop/data")
# write data frame to CSV file, and then read it back
write.csv(data_frame, file="florist.csv")
data_read <- read.csv(file="florist.csv", 
                 stringsAsFactors=FALSE)
data_read  # the row names are read in as extra column
# restore row names
rownames(data_read) <- data_read[, 1]
data_read <- data_read[, -1]  # remove extra column
data_read
# read row names from first column
data_read <- read.csv(file="florist.csv", row.names=1)
data_read
setwd("C:/Develop/data")
# write data frame to CSV file, without row names
write.csv(data_frame, row.names=FALSE, file="florist.csv")
data_read <- read.csv(file="florist.csv")
data_read  # a data frame without row names
setwd("C:/Develop/data")
# write matrix to csv file, and then read it back
write.csv(mat_rix, file="matrix.csv")
mat_read <- read.csv(file="matrix.csv", row.names=1)
mat_read  # read.csv() reads matrix as data frame
class(mat_read)
mat_read <- as.matrix(mat_read)  # coerce to matrix
identical(mat_rix, mat_read)
write.csv(mat_rix, row.names=FALSE, 
    file="matrix_ex_rows.csv")
mat_read <- read.csv(file="matrix_ex_rows.csv")
mat_read <- as.matrix(mat_read)
mat_read  # a matrix without row names
setwd("C:/Develop/data")
library(MASS)  # load package "MASS"
# write to CSV file by row - it's very SLOW!!!
write.matrix(mat_rix, file="matrix.csv", sep=",")
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
setwd("C:/Develop/data")
# read data from a csv file, including row names
mat_rix <- read.csv(file="matrix_bad.csv", row.names=1,
               stringsAsFactors=FALSE)
mat_rix
class(mat_rix)
# columns with bad data are character or factor
sapply(mat_rix, class)
row_names <- row.names(mat_rix)  # copy row names
# sapply loop over columns and coerce to numeric
mat_rix <- sapply(mat_rix, as.numeric)
row.names(mat_rix) <- row_names  # restore row names
# replace NAs with zero
mat_rix[is.na(mat_rix)] <- 0
# matrix without NAs
mat_rix
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
