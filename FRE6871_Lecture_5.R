x11(width=6, height=4)
par(mar=c(0, 0, 0, 0), oma=c(0, 0, 0, 0), mgp=c(0, 0, 0))
# Load constant maturity Treasury rates
load(file="C:/Develop/R/lecture_slides/data/rates_data.RData")
# Symbols for constant maturity Treasury rates
sym_bols <- c("DGS1", "DGS2", "DGS5", "DGS10", "DGS20")
# Calculate daily rates changes
rate_s <- xts:::na.locf.xts(rutils::do_call(cbind,
    as.list(rates_env)[sym_bols]))
rate_s <- xts:::na.locf.xts(rate_s)
rate_s <- xts:::na.locf.xts(rate_s, fromLast=TRUE)
re_turns <- rutils::diff_it(rate_s)
date_s <- index(re_turns)
# De-mean (center) and scale the returns
re_turns <- t(t(re_turns) - colMeans(re_turns))
re_turns <- t(t(re_turns) / sqrt(colSums(re_turns^2)/(NROW(re_turns)-1)))
re_turns <- xts(re_turns, date_s)
# Correlation matrix of Treasury rates
cor_mat <- cor(re_turns)
# reorder correlation matrix based on clusters
library(corrplot)
or_der <- corrMatOrder(cor_mat, order="hclust",
  hclust.method="complete")
cor_mat <- cor_mat[or_der, or_der]
# Plot the correlation matrix
col_ors <- colorRampPalette(c("red", "white", "blue"))
corrplot(cor_mat, title=NA, tl.col="black",
  tl.cex=0.8, mar=c(0,0,0,0), method="square",
  col=col_ors(8), cl.offset=0.75, cl.cex=0.7,
  cl.align.text="l", cl.ratio=0.25)
title("Correlation of Treasury rates", line=-1)
# Draw rectangles on the correlation matrix plot
corrRect.hclust(cor_mat, k=NROW(cor_mat) %/% 2,
  method="complete", col="red")
# eigen decomposition of covariance matrix
cov_mat <- cov(re_turns)
ei_gen <- eigen(cov_mat)
# Perform PCA without scaling
pc_a <- prcomp(re_turns, scale=FALSE)
# compare outputs
all.equal(ei_gen$values, pc_a$sdev^2)
all.equal(abs(ei_gen$vectors), abs(pc_a$rotation),
    check.attributes=FALSE)
# eigen decomposition of correlation matrix
cor_mat <- cor(re_turns)
ei_gen <- eigen(cor_mat)
# Perform PCA with scaling
pc_a <- prcomp(re_turns, scale=TRUE)
# compare outputs
all.equal(ei_gen$values, pc_a$sdev^2)
all.equal(abs(ei_gen$vectors), abs(pc_a$rotation),
    check.attributes=FALSE)
par(mfrow=c(n_weights/2, 2))
par(mar=c(2, 2, 0, 1), oma=c(0, 0, 0, 0))
# Invert all the principal component time series
pca_rets <- re_turns %*% pc_a$rotation
sol_ved <- pca_rets %*% solve(pc_a$rotation)
all.equal(coredata(re_turns), sol_ved)
# Invert first 3 principal component time series
sol_ved <- pca_rets[, 1:3] %*% solve(pc_a$rotation)[1:3, ]
sol_ved <- xts::xts(sol_ved, date_s)
sol_ved <- xts:::cumsum.xts(sol_ved)
cum_returns <- xts:::cumsum.xts(re_turns)
# Plot the solved returns
for (sym_bol in sym_bols) {
  plot.zoo(
    cbind(cum_returns[, sym_bol], sol_ved[, sym_bol]),
    plot.type="single", col=c("black", "blue"), xlab="", ylab="")
  legend(x="topleft", bty="n",
   legend=paste0(sym_bol, c("", " solved")),
   title=NULL, inset=0.0, cex=1.0, lwd=6,
   lty=1, col=c("black", "blue"))
}  # end for
set.seed(1121)  # Initialize random number generator
# Define explanatory and response variables
de_sign <- rnorm(100, mean=2)
noise <- rnorm(100)
res_ponse <- -3 + de_sign + noise
# Define design matrix and regression formula
de_sign <- data.frame(res_ponse, de_sign)
for_mula <- paste(colnames(de_sign)[1],
  paste(colnames(de_sign)[-1], collapse="+"),
  sep=" ~ ")
# Bootstrap of regression
boot_strap <- sapply(1:100, function(x) {
  boot_sample <- sample.int(NROW(de_sign),
                      replace=TRUE)
  mod_el <- lm(for_mula,
          data=de_sign[boot_sample, ])
  mod_el$coefficients
})  # end sapply
par(oma=c(1, 2, 1, 0), mgp=c(2, 1, 0), mar=c(5, 1, 1, 1), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
x11(width=6, height=6)
# Means and standard errors from bootstrap
apply(boot_strap, MARGIN=1,
      function(x) c(mean=mean(x), std_error=sd(x)))
# Means and standard errors from regression summary
mod_el <- lm(for_mula, data=de_sign)
mod_el$coefficients
summary(mod_el)$coefficients[, "Std. Error"]
plot(density(boot_strap["de_sign", ]),
     lwd=2, xlab="regression slopes",
     main="Bootstrapped regression slopes")
abline(v=mean(boot_strap["de_sign", ]),
       lwd=2, col="red")
library(parallel)  # Load package parallel
n_cores <- detectCores() - 1  # Number of cores
clus_ter <- makeCluster(n_cores)  # Initialize compute cluster under Windows
# Bootstrap of regression under Windows
boot_strap <- parLapply(clus_ter, 1:1000,
  function(x, for_mula, de_sign) {
    boot_sample <-
sample.int(NROW(de_sign), replace=TRUE)
    mod_el <- lm(for_mula,
data=de_sign[boot_sample, ])
    mod_el$coefficients
  },
  for_mula=for_mula,
  de_sign=de_sign)  # end parLapply
# Bootstrap of regression under Mac-OSX or Linux
boot_strap <- mclapply(1:1000,
  function(x) {
    boot_sample <-
sample.int(NROW(de_sign), replace=TRUE)
    lm(for_mula,
data=de_sign[boot_sample, ])$coefficients
  }, mc.cores=n_cores)  # end mclapply
stopCluster(clus_ter)  # Stop R processes over cluster under Windows
boot_strap <- rutils::do_call(rbind, boot_strap)
# Means and standard errors from bootstrap
apply(boot_strap, MARGIN=2,
function(x) c(mean=mean(x), std_error=sd(x)))
x11(width=6, height=6)
plot(density(boot_strap[, "de_sign"]),
     lwd=2, xlab="regression slopes",
     main="Bootstrapped regression slopes")
abline(v=mean(boot_strap[, "de_sign"]),
 lwd=2, col="red")
# create factor vector
fac_tor <- factor(c('b', 'c', 'd', 'a', 'c', 'b'))
fac_tor
fac_tor[3]
attributes(fac_tor)  # get factor attributes
levels(fac_tor)  # get allowed values
as.numeric(fac_tor)  # get encoding vector
is.vector(fac_tor)
as.factor(1:5)  # coerce vector to factor
# coerce factor to character vector
as.vector(as.factor(1:5))
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
# named numeric vector of breakpoints
brea_ks <- c(freezing=0, very_cold=30,
       cold=50, pleasant=60,
       warm=80, hot=90)
brea_ks
tempe_ratures <- runif(10, min=10, max=100)
feels_like <- names(
  brea_ks[findInterval(x=tempe_ratures,
                 vec=brea_ks)])
names(tempe_ratures) <- feels_like
tempe_ratures
library(microbenchmark)
da_ta <- sample(0:6) + 0.1
da_ta
cut(x=da_ta, breaks=c(2, 4, 6, 8))
rbind(da_ta, cut(x=da_ta, breaks=c(2, 4, 6, 8)))
# cut() replicates findInterval()
cut(x=1:8, breaks=c(3, 5, 7), labels=1:2,
    right=FALSE)
findInterval(x=1:8, vec=c(3, 5, 7))
# findInterval() is a compiled function, so it's faster than cut()
vec_tor <- rnorm(1000)
summary(microbenchmark(
  find_interval=
    findInterval(x=vec_tor, vec=c(3, 5, 7)),
  cuut=
    cut(x=vec_tor, breaks=c(3, 5, 7)),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# calculate DAX percentage returns
dax_rets <- diff(log(EuStockMarkets[, 1]))
# plot histogram
par(mar=c(1, 1, 1, 1), oma=c(2, 2, 2, 0))
histo_gram <- hist(dax_rets, breaks=30,
  main="", ylim=c(0, 60), xlim=c(-0.04, 0.04),
  xlab="", ylab="", freq=FALSE)
# draw kernel density of histogram
lines(density(dax_rets), col='red', lwd=2)
# add density of normal distribution
curve(expr=dnorm(x, mean=mean(dax_rets),
  sd=sd(dax_rets)), add=TRUE, type="l",
  lwd=2, col="blue")
title(main="DAX return distribution", line=0)
# add legend
legend("topright", inset=0.05, cex=0.8, title=NULL,
  leg=c(colnames(EuStockMarkets)[1], "Normal"),
  lwd=6, bg="white", col=c("red", "blue"))
# total area under histogram
diff(histo_gram$breaks) %*% histo_gram$density
# create a list with two elements
list_var <- list(c('a', 'b'), 1:4)
list_var
c(typeof(list_var), mode(list_var), class(list_var))
# lists are also vectors
c(is.vector(list_var), is.list(list_var))
NROW(list_var)
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
# create a named vector
stu_dents <- sample(round(runif(5, min=1, max=10), digits=2))
names(stu_dents) <- c("Angie", "Chris", "Suzie", "Matt", "Liz")
# sort the vector into ascending order
sort(stu_dents)
# calculate index to sort into ascending order
order(stu_dents)
# sort the vector into ascending order
stu_dents[order(stu_dents)]
# calculate the sorted (ordered) vector
sort_ed <- stu_dents[order(stu_dents)]
# calculate index to sort into unsorted (original) order
order(order(stu_dents))
sort_ed[order(order(stu_dents))]
stu_dents
# create a data frame of stu_dents and their ranks
ra_nks <- c("first", "second", "third", "fourth", "fifth")
data.frame(students=stu_dents, rank=ra_nks[order(order(stu_dents))])
# permute data_frame of flowers on price column
order(data_frame$price)
# sort data_frame on price
data_frame[order(data_frame$price), ]
# sort data_frame on color
data_frame[order(data_frame$color), ]
order(c(2, 1:4))  # there's a tie
order(c(2, 1:4), 1:5)  # there's a tie
# read the Examples for sort()
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
# Replace NAs
good_air <- zoo::na.locf(airquality)
dim(good_air)
head(good_air)  # NAs replaced
# Create vector containing NA values
vec_tor <- sample(22)
vec_tor[sample(NROW(vec_tor), 4)] <- NA
# Replace NA values with the most recent non-NA values
zoo::na.locf(vec_tor)
# Replace NAs in xts time series
se_ries <- rutils::etf_env$price_s[, 1]
head(se_ries)
sum(is.na(se_ries))
library(quantmod)
series_zoo <- as.xts(zoo::na.locf(se_ries, na.rm=FALSE, fromLast=TRUE))
series_xts <- xts:::na.locf.xts(se_ries, fromLast=TRUE)
all.equal(series_zoo, series_xts, check.attributes=FALSE)
library(microbenchmark)
summary(microbenchmark(
  zoo=as.xts(zoo::na.locf(se_ries, fromLast=TRUE)),
  xts=xts:::na.locf.xts(se_ries, fromLast=TRUE),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
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
par(oma=c(1, 1, 1, 1), mar=c(2, 1, 1, 1), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
lamb_da <- c(0.5, 1, 1.5)
col_ors <- c("red", "blue", "green")
# Plot three curves in loop
for (in_dex in 1:3) {
  curve(expr=plogis(x, scale=lamb_da[in_dex]),
xlim=c(-4, 4), type="l",
xlab="", ylab="", lwd=4,
col=col_ors[in_dex], add=(in_dex>1))
}  # end for
# Add title
title(main="Logistic function", line=0.5)
# Add legend
legend("topleft", title="Scale parameters",
       paste("lambda", lamb_da, sep="="),
       inset=0.05, cex=0.8, lwd=6, bty="n",
       lty=1, col=col_ors)
set.seed(1121)
# Simulate overlapping scores data
scores_1 <- runif(100, max=0.6)
scores_2 <- runif(100, min=0.4)
# Perform Wilcoxon test for mean
wilcox.test(scores_1, scores_2)
# Combine scores and add categorical variable
score_s <- c(scores_1, scores_2)
res_ponse <- c(logical(100), !logical(100))
# Perform logit regression
g_lm <- glm(res_ponse ~ score_s, family=binomial(logit))
class(g_lm)
summary(g_lm)
x11(width=7, height=5)
par(mar=c(3, 3, 2, 2), mgp=c(2, 1, 0), oma=c(0, 0, 0, 0))
or_der <- order(score_s)
plot(x=score_s[or_der], y=g_lm$fitted.values[or_der],
     type="l", lwd=4, col="orange",
     main="Category Densities and Logistic Function",
     xlab="score", ylab="density")
den_sity <- density(score_s[res_ponse])
den_sity$y <- den_sity$y/max(den_sity$y)
lines(den_sity, col="red")
polygon(c(min(den_sity$x), den_sity$x, max(den_sity$x)), c(min(den_sity$y), den_sity$y, min(den_sity$y)), col=rgb(1, 0, 0, 0.2), border=NA)
den_sity <- density(score_s[!res_ponse])
den_sity$y <- den_sity$y/max(den_sity$y)
lines(den_sity, col="blue")
polygon(c(min(den_sity$x), den_sity$x, max(den_sity$x)), c(min(den_sity$y), den_sity$y, min(den_sity$y)), col=rgb(0, 0, 1, 0.2), border=NA)
# Add legend
legend(x="top", cex=1.0, bty="n", lty=c(1, NA, NA),
 lwd=c(6, NA, NA), pch=c(NA, 15, 15),
 legend=c("logistic fit", "TRUE", "FALSE"),
 col=c("orange", "red", "blue"),
 text.col=c("black", "red", "blue"))
library(ISLR)  # load package ISLR
# get documentation for package tseries
packageDescription("ISLR")  # get short description

help(package="ISLR")  # load help page

library(ISLR)  # load package ISLR

data(package="ISLR")  # list all datasets in ISLR

ls("package:ISLR")  # list all objects in ISLR

detach("package:ISLR")  # Remove ISLR from search path
library(ISLR)  # load package ISLR
# load credit default data
attach(Default)
summary(Default)
sapply(Default, class)
dim(Default); head(Default)
x_lim <- range(balance)
y_lim <- range(income)
# Plot data points for non-defaulters
default_ed <- (default=="Yes")
plot(income ~ balance,
     main="Default Dataset from Package ISLR",
     xlim=x_lim, ylim=y_lim,
     data=Default[!default_ed, ],
     pch=4, col="blue")
# Plot data points for defaulters
points(income ~ balance,
 data=Default[default_ed, ],
 pch=4, lwd=2, col="red")
# Add legend
legend(x="topright", bty="n",
 legend=c("non-defaulters", "defaulters"),
 col=c("blue", "red"), lty=1, lwd=6, pch=4)
default_ed <- (default=="Yes")
# Wilcoxon test for balance predictor
wilcox.test(balance[default_ed], balance[!default_ed])
# Wilcoxon test for income predictor
wilcox.test(income[default_ed], income[!default_ed])
library(ISLR)  # load package ISLR
attach(Default)  # load credit default data
par(mfrow=c(1,2))  # Set plot panels
# balance boxplot
boxplot(formula=balance ~ default,
  col="lightgrey",
  main="balance", xlab="default")
# income boxplot
boxplot(formula=income ~ default,
  col="lightgrey",
  main="income", xlab="default")
# fit logistic regression model
g_lm <- glm(default ~ balance,
        family=binomial(logit))
class(g_lm)
summary(g_lm)
plot(x=balance, y=default_ed,
     main="Logistic Regression of Credit Defaults", col="orange",
     xlab="credit balance", ylab="defaults")
or_der <- order(balance)
lines(x=balance[or_der], y=g_lm$fitted.values[or_der],
col="blue", lwd=3)
legend(x="topleft", inset=0.1, bty="n",
 legend=c("defaults", "logit fitted values"),
 col=c("orange", "blue"), lty=c(NA, 1), pch=c(1, NA), lwd=6)
library(ISLR)  # load package ISLR
attach(Default)  # load credit default data
# Calculate cumulative defaults
default_ed <- (default=="Yes")
to_tal <- sum(default_ed)
default_s <- sapply(balance, function(ba_lance) {
    sum(default_ed[balance <= ba_lance])
})  # end sapply
# Perform logit regression
g_lm <- glm(
  cbind(default_s, to_tal-default_s) ~
    balance,
  family=binomial(logit))
summary(g_lm)
plot(x=balance, y=default_s/to_tal, col="orange", lwd=1,
     main="Cumulative Defaults Versus Balance",
     xlab="credit balance", ylab="cumulative defaults")
or_der <- order(balance)
lines(x=balance[or_der], y=g_lm$fitted.values[or_der],
col="blue", lwd=3)
legend(x="topleft", inset=0.1, bty="n",
 legend=c("cumulative defaults", "fitted values"),
 col=c("orange", "blue"), lty=c(NA, 1), pch=c(1, NA), lwd=6)
library(ISLR)  # load package ISLR
attach(Default)  # load credit default data
# fit multifactor logistic regression model
col_names <- colnames(Default)
for_mula <- as.formula(paste(col_names[1],
  paste(col_names[-1], collapse="+"), sep=" ~ "))
g_lm <- glm(for_mula, data=Default,
        family=binomial(logit))
summary(g_lm)
library(ISLR)  # load package ISLR
attach(Default)  # load credit default data
default_ed <- (default=="Yes")
stu_dent <- (student=="Yes")
# Calculate cumulative defaults
default_s <- sapply(balance,
  function(ba_lance) {
    c(stu_dent=sum(default_ed[stu_dent & (balance <= ba_lance)]),
non_student=sum(default_ed[(!stu_dent) & (balance <= ba_lance)]))
})  # end sapply
to_tal <- c(sum(default_ed[stu_dent]), sum(default_ed[!stu_dent]))
default_s <- t(default_s / to_tal)
library(ISLR)  # load package ISLR
attach(Default)  # load credit default data
# Plot cumulative defaults
par(mfrow=c(1,2))  # Set plot panels
or_der <- order(balance)
plot(x=balance[or_der], y=default_s[or_der, 1],
     col="red", t="l", lwd=2,
     main="Cumulative defaults of\n students and non-students",
     xlab="credit balance", ylab="")
lines(x=balance[or_der], y=default_s[or_der, 2],
col="blue", lwd=2)
legend(x="topleft", bty="n",
 legend=c("students", "non-students"),
 col=c("red", "blue"), text.col=c("red", "blue"),
 lwd=3)
# balance boxplot for student factor
boxplot(formula=balance ~ student,
  col="lightgrey",
  main="balance", xlab="student")
# fit multifactor logistic regression model
col_names <- colnames(Default)
for_mula <- as.formula(paste(col_names[1],
  paste(col_names[-1], collapse="+"), sep=" ~ "))
g_lm <- glm(for_mula, data=Default, family=binomial(logit))
fore_casts <- predict(g_lm, type="response")
fore_casts[1:6]
all.equal(g_lm$fitted.values, fore_casts)
# Discrimination threshold
thresh_old <- 0.05
# Calculate confusion matrix
table(default_ed, (fore_casts>thresh_old))
sum(default_ed)
sum(Default$default=="Yes")
# fit logistic regression over training data
set.seed(1121)  # Reset random number generator
n_rows <- NROW(Default)
sam_ple <- sample.int(n=n_rows, size=n_rows/2)
train_data <- Default[sam_ple, ]
g_lm <- glm(for_mula, data=train_data, family=binomial(link="logit"))
# forecast over test data
test_data <- Default[-sam_ple, ]
fore_casts <- predict(g_lm, newdata=test_data, type="response")
# Calculate confusion matrix
table(test_data$default=="No",
(fore_casts<thresh_old))
# FALSE positive (type I error)
sum(test_data$default=="No" & (fore_casts>thresh_old))
# FALSE negative (type II error)
sum(test_data$default=="Yes" & (fore_casts<thresh_old))
detach(Default)
library(ISLR)  # load package ISLR
attach(Default)  # load credit default data
col_names <- colnames(Default)
for_mula <- as.formula(paste(col_names[1], paste(col_names[-1], collapse="+"), sep=" ~ "))
set.seed(1121)  # Reset random number generator
n_rows <- NROW(Default)
sam_ple <- sample(x=1:n_rows, size=n_rows/2)
train_data <- Default[sam_ple, ]
g_lm <- glm(for_mula, data=train_data, family=binomial(link="logit"))
test_data <- Default[-sam_ple, ]
fore_casts <- predict(g_lm, newdata=test_data, type="response")
thresh_old <- 0.05
# Calculate confusion matrix
confu_sion <- table(test_data$default=="No",
              (fore_casts<thresh_old))
dimnames(confu_sion) <- list(actual=rownames(confu_sion),
  forecast=colnames(confu_sion))
confu_sion
confu_sion <- confu_sion / rowSums(confu_sion)
c(typeI=confu_sion[2, 1], typeII=confu_sion[1, 2])
# below is an unsuccessful attempt to draw confusion matrix using xtable
confusion_matrix <- matrix(c("| true positive \\\\ (sensitivity)", "| false negative \\\\ (type II error)", "| false positive \\\\ (type I error)", "| true negative \\\\ (specificity)"), nc=2)
dimnames(confusion_matrix) <- list(forecast=c("FALSE", "TRUE"),
                             actual=c("FALSE", "TRUE"))
print(xtable::xtable(confusion_matrix,
caption="Confusion Matrix"),
caption.placement="top",
comment=FALSE, size="scriptsize",
include.rownames=TRUE,
include.colnames=TRUE)
# end unsuccessful attempt to draw confusion table using xtable
# Confusion matrix as function of thresh_old
con_fuse <- function(actu_al, fore_casts, thresh_old) {
    confu_sion <- table(actu_al, (fore_casts<thresh_old))
    confu_sion <- confu_sion / rowSums(confu_sion)
    c(typeI=confu_sion[2, 1], typeII=confu_sion[1, 2])
  }  # end con_fuse
con_fuse(test_data$default=="No", fore_casts, thresh_old=thresh_old)
# Define vector of discrimination thresholds
threshold_s <- seq(0.01, 0.95, by=0.01)^2
# Calculate error rates
error_rates <- sapply(threshold_s, con_fuse,
  actu_al=(test_data$default=="No"),
  fore_casts=fore_casts)  # end sapply
error_rates <- t(error_rates)
# Calculate area under ROC curve (AUC)
true_pos <- sort(1- c(error_rates[, "typeII"], 0))
true_pos <- (true_pos + rutils::lag_it(true_pos))/2
false_pos <- c(sort(error_rates[, "typeI"]), 1)
false_pos <- rutils::diff_it(false_pos)
sum(true_pos*false_pos)
# Plot ROC Curve for Defaults
plot(x=error_rates[, "typeI"],
     y=1-error_rates[, "typeII"],
     xlab="FALSE positive rate",
     ylab="TRUE positive rate",
     main="ROC Curve for Defaults",
     type="l", lwd=3, col="blue")
abline(a=0.0, b=1.0, lwd=3, col="orange")
