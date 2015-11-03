library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
options(digits=3)
thm <- knit_theme$get("acid")
knit_theme$set(thm)
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
data_frame <- data.frame(
        type=c("rose", "daisy", "tulip"), 
        color=c("red", "white", "yellow"), 
        price=c(1.5, 0.5, 1.0), 
        row.names=c("flower1", "flower2", "flower3")
        )  # end data.frame
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
setwd("C:/Develop/data")
data_frame <- data.frame(small=c(3, 5), medium=c(9, 11), large=c(15, 13))
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
write_clip <- function(data, row.names=FALSE,
               col.names=TRUE, ...) {
  write.table(x=data, file="clipboard", sep="\t",
      row.names=row.names, col.names=col.names, ...)
}  # end write_clip

write_clip(data=data_frame)

# launch spreadsheet-style data editor
data_frame <- edit(data_frame)
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
library("MASS")  # load library "MASS"
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
# coerce columns to numeric, using apply loop over columns
mat_rix <- as.matrix(apply(mat_rix, 2, as.numeric))
row.names(mat_rix) <- row_names  # restore row names
# replace NAs with zero
mat_rix[is.na(mat_rix)] <- 0
# matrix without NAs
mat_rix
setwd("C:/Develop/data")
rm(list=ls())
set.seed(1121)  # reset random number generator
library(zoo)  # load package zoo
# create zoo with Date index
in_dex <- seq(from=as.Date("2013-06-15"), 
            by="day", length.out=100)
zoo_series <- zoo(cumsum(rnorm(length(in_dex))), 
            order.by=in_dex)
tail(zoo_series, 3)
# write zoo to text file, and then read it back
write.zoo(zoo_series, file="zoo_series.txt")
zoo_series <- read.zoo("zoo_series.txt")  # read it back
tail(zoo_series, 3)
setwd("C:/Develop/data")
rm(list=ls())
set.seed(1121)  # reset random number generator
library(zoo)  # load package zoo
# create zoo with POSIXct date-time index
in_dex <- seq(from=as.POSIXct("2013-06-15"), 
            by="hour", length.out=1000)
zoo_series <- zoo(cumsum(rnorm(length(in_dex))), 
            order.by=in_dex)
tail(zoo_series, 3)
# write zoo to text file, and then read it back
write.zoo(zoo_series, file="zoo_series.txt")
zoo_series <- read.zoo("zoo_series.txt")  # read it back
# time field was read as a separate column
tail(zoo_series, 3)
# read and specify that second column is time field
zoo_series <- read.zoo(file="zoo_series.txt", 
                 index.column=list(1,2), 
                 tz="America/New_York")
tail(zoo_series, 3)
setwd("C:/Develop/data")
library(zoo)  # load package zoo
# write zoo to CSV file, and then read it back
write.zoo(zoo_series, file="zoo_series.csv", sep=",")
zoo_series <- read.zoo(file="zoo_series.csv", 
            header=TRUE, sep=",", FUN=as.POSIXct,
            tz="America/New_York")
tail(zoo_series, 3)
# read zoo from CSV file, with custom date-time format
zoo_frame <- read.table(file="zoo_series2.csv", sep=",")
tail(zoo_frame, 3)  # date-time format mm/dd/yyyy hh:mm
zoo_series <- read.zoo(file="zoo_series2.csv", 
            header=TRUE, sep=",", FUN=as.POSIXct, 
            tz="America/New_York",
            format="%m/%d/%Y %H:%M")
tail(zoo_series, 3)
rm(list=ls())  # remove all objects
var1 <- 1; var2 <- 2
ls()  # list all objects
ls()[1]  # list first object
args(save)  # list arguments of save function
# save "var1" to a binary file
save("var1", file="my_data.RData")  # use string
save(var1, file="my_data.RData")  # use object name
save(var1, var2, file="my_data.RData")  # mulltiple object names
# save first list object "var1" by passing it to the "..." argument
save(ls()[1], file="my_data.RData")  # 'ls()[1]' not evaluated
# save first list object "var1" by passing it to the "list" argument
save(list=ls()[1], file="my_data.RData")
# save whole list by passing it to the "list" argument
save(list=ls(), file="my_data.RData")
rm(list=ls())  # remove all objects
# load objects from file
load_ed <- load(file="my_data.RData")
load_ed  # vector of loaded objects
ls()  # list objects
# assign new values to objects in  global environment
sapply(load_ed, function(sym_bol) {
  assign(sym_bol, runif(1), envir=globalenv())
})  # end sapply
ls()  # list objects
# assign new values to objects using for loop
for (sym_bol in load_ed) {
  assign(sym_bol, runif(1))
}  # end for
ls()  # list objects
# save vector of objects
save(list=load_ed, file="my_data.RData")
# remove only loaded objects
rm(list=load_ed)
# remove the object "load_ed"
rm(load_ed)
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
library(microbenchmark)
foo <- runif(1e6)
system.time(foo^0.5)
microbenchmark(sqrt(foo), foo^0.5, times=10)
library(microbenchmark)
# sum() is a compiled primitive function
sum
# mean() is a generic function
mean
foo <- runif(1e6)
# sum() is much faster than mean()
summary(
  microbenchmark(sum(foo), mean(foo), times=10)
  )[, c(1, 4, 5)]
# any() is a compiled primitive function
any
# any() is much faster than %in% wrapper for match()
summary(
  microbenchmark(any(foo == 1), {1 %in% foo}, times=10)
  )[, c(1, 4, 5)]
library(microbenchmark)
mat_rix <- matrix(1:9, ncol=3, # create matrix
  dimnames=list(paste0("row", 1:3),
          paste0("col", 1:3)))
# create specialized function
matrix_to_dframe <- function(mat_rix) {
  n_col <- ncol(mat_rix)
  dframe <- vector("list", n_col)  # empty vector
  for(in_dex in 1:n_col)  # populate vector
    dframe <- mat_rix[, in_dex]
  attr(dframe, "row.names") <-  # add attributes
    .set_row_names(nrow(mat_rix))
  attr(dframe, "class") <- "data.frame"
  dframe  # return data frame
}  # end matrix_to_dframe
# compare speed of three methods
summary(microbenchmark(
  matrix_to_dframe(mat_rix),
  as.data.frame.matrix(mat_rix),
  as.data.frame(mat_rix),
  times=10))[, c(1, 4, 5)]
# matrix with 5,000 rows
big_matrix <- matrix(rnorm(10000), ncol=2)
# allocate memory for row sums
row_sums <- numeric(nrow(big_matrix))
summary(microbenchmark(
  v_apply=vapply(1:nrow(big_matrix), function(in_dex)
    sum(big_matrix[in_dex, ]),
    FUN.VALUE=c(sum=0)),  # end vapply
  l_apply=lapply(1:nrow(big_matrix), function(in_dex)
    sum(big_matrix[in_dex, ])),  # end lapply
  apply=apply(big_matrix, 1, sum),
  s_apply=sapply(1:nrow(big_matrix), function(in_dex)
    sum(big_matrix[in_dex, ])),  # end sapply
  for_loop=for(i in 1:nrow(big_matrix)) {
    row_sums[i] <- sum(big_matrix[i,])
  },  # end for
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
big_vector <- rnorm(5000)
summary(microbenchmark(
# allocate full memory for cumulative sum
  for_loop={cum_sum <- numeric(length(big_vector))
    cum_sum[1] <- big_vector[1]
    for(i in 2:length(big_vector)) {
      cum_sum[i] <- cum_sum[i-1] + big_vector[i]
    }},  # end for
# allocate zero memory for cumulative sum
  grow_vec={cum_sum <- numeric(0)
    cum_sum[1] <- big_vector[1]
    for(i in 2:length(big_vector)) {
# add new element to "cum_sum" ("grow" it)
      cum_sum[i] <- cum_sum[i-1] + big_vector[i]
    }},  # end for
# allocate zero memory for cumulative sum
  com_bine={cum_sum <- numeric(0)
    cum_sum[1] <- big_vector[1]
    for(i in 2:length(big_vector)) {
# add new element to "cum_sum" ("grow" it)
      cum_sum <- c(cum_sum, big_vector[i])
    }},  # end for
  times=10))[, c(1, 4, 5)]
library(microbenchmark)
foo <- runif(1e6)
system.time(foo^0.5)
summary(
  microbenchmark(sqrt(foo), foo^0.5, times=10)
  )[, c(1, 4, 5)]
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
# calculate row sums two different ways
summary(microbenchmark(
  row_sums=rowSums(big_matrix),
  ap_ply=apply(big_matrix, 1, sum),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
library(microbenchmark)
str(pmax)
# calculate row maximums two different ways
summary(microbenchmark(
  p_max=
    do.call(pmax.int,
lapply(seq_along(big_matrix[1, ]),
  function(in_dex) big_matrix[, in_dex])),
  l_apply=unlist(
    lapply(seq_along(big_matrix[, 1]),
  function(in_dex) max(big_matrix[in_dex, ]))),
  times=10))[, c(1, 4, 5)]
library(matrixStats)  # load package "matrixStats"
# calculate row min values three different ways
summary(microbenchmark(
  row_mins=rowMins(big_matrix),
  p_min=
    do.call(pmin.int,
      lapply(seq_along(big_matrix[1, ]),
             function(in_dex)
               big_matrix[, in_dex])),
  as_data_frame=
    do.call(pmin.int,
      as.data.frame.matrix(big_matrix)),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
library(caTools)  # load package "caTools"
# get documentation for package "caTools"
packageDescription("caTools")  # get short description
help(package="caTools")  # load help page
data(package="caTools")  # list all datasets in "caTools"
ls("package:caTools")  # list all objects in "caTools"
detach("package:caTools")  # remove caTools from search path
# median filter
vol_window <- 11
med_ian <- runmed(x=big_vector, k=vol_window)
# vector of rolling volatility
vo_lat <- runsd(x=big_vector, k=vol_window,
          endrule="constant", align="center")
# vector of rolling quantiles
quan_tiles <- runquantile(x=big_vector,
            k=vol_window, probs=0.9,
            endrule="constant",
            align="center")
summary(microbenchmark(  # assign values to vector three different ways
# fast vectorized assignment loop performed in C using brackets "[]"
  brack_ets={vec_tor <- numeric(10)
    vec_tor[] <- 2},
# slow because loop is performed in R
  for_loop={vec_tor <- numeric(10)
    for (in_dex in seq_along(vec_tor))
      vec_tor[in_dex] <- 2},
# very slow because no memory is pre-allocated
# "vec_tor" is "grown" with each new element
  grow_vec={vec_tor <- numeric(0)
    for (in_dex in 1:10)
# add new element to "vec_tor" ("grow" it)
      vec_tor[in_dex] <- 2},
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
summary(microbenchmark(  # assign values to vector two different ways
# fast vectorized assignment loop performed in C using brackets "[]"
  brack_ets={vec_tor <- numeric(10)
    vec_tor[4:7] <- rnorm(4)},
# slow because loop is performed in R
  for_loop={vec_tor <- numeric(10)
    for (in_dex in 4:7)
      vec_tor[in_dex] <- rnorm(1)},
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# define function vectorized automatically
my_fun <- function(in_put, pa_ram) {
  pa_ram*in_put
}  # end my_fun
my_fun(in_put=1:3, pa_ram=2)  # "in_put" is vectorized
my_fun(in_put=10, pa_ram=2:4)  # "pa_ram" is vectorized
rnorm(1, sd=1:3) # rnorm() "sd" argument not vectorized
# sapply produces desired vector output
sapply(1:3, function(sd, ...) rnorm(sd=sd, ...), n=3)
sapply(1:3, rnorm, n=3, mean=0)
sapply(-1:1, 
       function(mean, ...) rnorm(mean=mean, ...), n=3)
# rnorm vectorized with respect to "sd"
vec_rnorm <- function(n, mean=0, sd=1) {
  if (length(sd)==1)
    rnorm(n=n, mean=mean, sd=sd)
  else
    sapply(sd, rnorm, n=n, mean=mean)
}  # end vec_rnorm
foo <- structure(1:3, names=paste0("sd=", 1:3))
vec_rnorm(n=3, sd=foo)
# rnorm vectorized with respect to "mean" and "sd"
vec_rnorm <- Vectorize(
      FUN=rnorm,
      vectorize.args=c("mean", "sd")
)  # end Vectorize
vec_rnorm(n=3, sd=foo)
foo <- structure(-1:1, names=paste0("mean=", -1:1))
vec_rnorm(n=3, mean=foo)
str(sum)
# na.rm is bound by name
mapply(sum, 6:9, c(5, NA, 3), 2:6, na.rm=TRUE)
str(rnorm)
# mapply vectorizes both arguments "mean" and "sd"
mapply(rnorm, n=5, mean=(-1:1), sd=(1:3))

mapply(function(in_put, e_xp) in_put^e_xp, 
       1:5, seq(from=1, by=0.2, length.out=5))
# rnorm vectorized with respect to both "mean" and "sd"
vec_rnorm <- function(n, mean=0, sd=1) {
  if (length(mean)==1 && length(sd)==1)
    rnorm(n=n, mean=mean, sd=sd)
  else
    mapply(rnorm, n=n, mean=mean, sd=sd)
}  # end vec_rnorm
# call vec_rnorm() on vector of "sd"
foo <- structure(1:3, names=paste0("sd=", 1:3))
vec_rnorm(n=3, sd=foo)
# call vec_rnorm() on vector of "mean"
foo <- structure(-1:1, names=paste0("mean=", -1:1))
vec_rnorm(n=3, mean=foo)
# create two numeric vectors
vec_tor1 <- sin(0.25*pi*1:10)
vec_tor2 <- cos(0.25*pi*1:10)
# create third vector using 'ifelse'
vec_tor3 <- ifelse(vec_tor1 > vec_tor2,
          vec_tor1, vec_tor2)
# cbind all three together
vec_tor4 <- cbind(vec_tor1, vec_tor2, vec_tor3)

# set plotting parameters
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0),
    cex.lab=0.8, cex.axis=0.8, cex.main=0.8,
    cex.sub=0.5)
# plot matrix
matplot(vec_tor4, type="l", lty="solid",
col=c("green", "blue", "red"),
lwd=c(2, 2, 2), xlab="", ylab="")
# add legend
legend(x="bottomright", legend=colnames(vec_tor4),
       title="", inset=0.05, cex=0.8, lwd=2,
       lty=c(1, 1, 1), col=c("green", "blue", "red"))
par(oma=c(1, 1, 1, 1), mgp=c(2, 1, 0), mar=c(5, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
set.seed(1121)  # reset random number generator
simu_max <- 1000  # max simulation trials
simu_prices <- numeric(simu_max)  # initialize prices
barrier_level <- 20  # barrier level
simu_prices[1] <- 0  # first simulated price
in_dex <- 2  # initialize simulation index
while ((in_dex <= simu_max) &&
 (simu_prices[in_dex - 1] < barrier_level)) {
  simu_prices[in_dex] <- # simulate next price
    simu_prices[in_dex - 1] + rnorm(1)
  in_dex <- in_dex + 1  # advance in_dex
}  # end while
if (in_dex <= simu_max) {  # fill zero prices
  simu_prices[in_dex:simu_max] <- simu_prices[in_dex - 1]
}
# create daily time series starting 2011
ts_var <- ts(data=simu_prices, frequency=365, start=c(2011, 1))
plot(ts_var, type="l", col="black",  # create plot
     lty="solid", xlab="", ylab="")
abline(h=barrier_level, lwd=2, col="red")  # add horizontal line
title(main="Random Prices", line=0)  # add title
par(oma=c(1, 1, 1, 1), mgp=c(2, 1, 0), mar=c(5, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
set.seed(1121)  # reset random number generator
simu_max <- 1000  # max simulation trials
barrier_level <- 20  # barrier level
# simulated prices
simu_prices <- cumsum(rnorm(simu_max))
# find index when prices cross barrier_level
which_index <- which(simu_prices > barrier_level)
# fill prices after crossing barrier_level
if (length(which_index)>0) {
  simu_prices[(which_index[1]+1):simu_max] <-
    simu_prices[which_index[1]]
}  # end if
# create daily time series starting 2011
ts_var <- ts(data=simu_prices, frequency=365,
     start=c(2011, 1))
plot(ts_var, type="l", col="black",  # create plot
     lty="solid", xlab="", ylab="")
abline(h=barrier_level, lwd=2, col="red")  # add horizontal line
title(main="Random Prices", line=0)  # add title
