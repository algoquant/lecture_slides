library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
thm <- knit_theme$get("acid")
knit_theme$set(thm)
# define a function that returns invisibly
test_func <- function(arg_var) {
  if (!is.numeric(arg_var)) {
    warning(paste("argument", arg_var, "isn't numeric"))
    return(NULL)
  }
  2*arg_var
}  # end test_func

test_func(2)
test_func("hello")
# define a function that returns invisibly
return_invisible <- function(arg_var) {
  invisible(arg_var)
}  # end return_invisible

return_invisible(2)

glob_var <- return_invisible(2)
glob_var

rm(list=ls())  # remove all objects
# load objects from file
loaded <- load(file="my_data.RData")
loaded  # vector of loaded objects
ls()  # list objects
str(sum)  # "sum" accepts multiple arguments
# passing a list of arguments to "sum" produces an error
sum(list(1, 2, 3, 4))
# do.call passes list elements into "sum" individually
do.call(sum, list(1, 2, 3, 4))
num_list <- list(1, 2, 3, 4)  # create numeric list
do.call(rbind, num_list)  # returns single column matrix
do.call(cbind, num_list)  # returns single row matrix
# recycling rule applied
do.call(cbind, list(1:2, 3:5))
# NULL element is skipped
do.call(cbind, list(1, NULL, 3, 4))
# NA element isn't skipped
do.call(cbind, list(1, NA, 3, 4))
list_vectors <- lapply(1:5, rnorm, n=10)
mat_rix <- do.call(rbind, list_vectors)
dim(mat_rix)
do_call_rbind <- function(list_var) {
  while (length(list_var) > 1) {
# index of odd list elements
    odd_index <- seq(from=1, to=length(list_var), by=2)
# bind neighboring elements and divide list_var by half
    list_var <- lapply(odd_index, function(in_dex) {
      if (in_dex==length(list_var)) {
return(list_var[[in_dex]])
      }
      return(rbind(list_var[[in_dex]], 
           list_var[[in_dex+1]]))
    })  # end lapply
  }  # end while
# list_var has only one element - return it
  list_var[[1]]
}  # end do_call_rbind
identical(mat_rix, do_call_rbind(list_vectors))
unique(iris$Species)  # Species takes on three distinct values
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
# get mean mpg for each cylinder group
unlist(lapply(split_cars, function(x) mean(x$mpg)))
# Which is identical to the tapply function
tapply(mtcars$mpg, mtcars$cyl, mean)
# using "with" environment
with(mtcars, tapply(mpg, cyl, mean))
# can also use the functions by() and aggregate()
with(mtcars, by(mpg, cyl, mean))
aggregate(formula=(mpg ~ cyl), data=mtcars, FUN=mean)
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
        )  # end data_frame
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
data_read  # the rownames are read in as extra column
# restore rownames
rownames(data_read) <- data_read[, 1]
data_read <- data_read[, -1]  # remove extra column
data_read
setwd("C:/Develop/data")
# write matrix to csv file, and then read it back
write.csv(mat_rix, file="matrix.csv")
mat_read <- read.csv(file="matrix.csv", 
               stringsAsFactors=FALSE)
mat_read  # read.csv() read matrix as data frame
class(mat_read)
# restore rownames
rownames(mat_read) <- mat_read[, 1]
mat_read <- mat_read[, -1]  # remove extra column
# coerce from data frame back to matrix
mat_read <- as.matrix(mat_read)
class(mat_read)
mat_read  # a matrix
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
mat_read <- read.csv(
  file="badmatrix.csv",
  stringsAsFactors=FALSE)
# restore rownames
rownames(mat_read) <- as.character(mat_read[, 1])
mat_read <- mat_read[, -1]  # remove extra column
dim_names <- dimnames(mat_read)  # save dimnames
num_col <- ncol(mat_read)  # save num columns
mat_read  # data frame with bad data
class(mat_read)
class(mat_read[, 1])
# numeric data column coerced to character by bad data
class(mat_read[, 2])
# perform sapply over columns
mat_scrub <- sapply(mat_read, function(co_lumn) {
# coerce to numeric
  co_lumn <- as.numeric(co_lumn)
# replace NAs with zero
  co_lumn[is.na(co_lumn)] <- 0
  co_lumn
}  # end anon function
)  # end sapply
# restore dimnames
dimnames(mat_scrub) <- dimnames(mat_read)
# coerce to matrix
mat_scrub <- as.matrix(mat_scrub)
# matrix without NAs
mat_scrub
rm(list=ls())  # remove all objects
var1 <- 1; var2 <- 2
ls()  # list all objects
ls()[1]  # list first object
args(save)  # list arguments of save function
# save "var1" to a binary file
save("var1", file="my_data.RData")
# save first list object "var1" by passing it to the "..." argument
save(ls()[1], file="my_data.RData")  # 'ls()[1]' not evaluated
# save first list object "var1" by passing it to the "list" argument
save(list=ls()[1], file="my_data.RData")
# save whole list by passing it to the "list" argument
save(list=ls(), file="my_data.RData")
rm(list=ls())  # remove all objects
# load objects from file
loaded <- load(file="my_data.RData")
loaded  # vector of loaded objects
ls()  # list objects
# assign new values to objects
sapply(loaded, function(sym_bol) {
  assign(sym_bol, runif(1))
})
# save vector of objects
save(list=loaded, file="my_data.RData")
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
# matrix with 500,000 rows
big_matrix <- matrix(rnorm(1000000), ncol=2)
# allocate memory for row sums
row_sums <- numeric(nrow(big_matrix))
# sum up row using "for" loop
system.time(
  for(i in 1:nrow(big_matrix)) {
    row_sums[i] <- sum(big_matrix[i,])
  }  # end for
)  # end system.time
# row sums using "apply" loop
system.time(row_sums <- apply(big_matrix, 1, sum))
system.time(  # row sums using "sapply" loop
  row_sums <- sapply(1:nrow(big_matrix), 
             function(in_dex) 
                 sum(big_matrix[in_dex, ])))
system.time(  # row sums using "lapply" loop
  row_sums <- lapply(1:nrow(big_matrix), 
             function(in_dex) 
                 sum(big_matrix[in_dex, ])))
big_vector <- rnorm(50000)
#allocate zero memory for cumulative sum
cum_sum <- numeric(0)
# cumulative sum using "for" loop
cum_sum[1] <- big_vector[1]
system.time(
  for(i in 2:length(big_vector)) {
    cum_sum[i] <- cum_sum[i-1] + big_vector[i]
  }  # end for
)  # end system.time
#allocate full memory for cumulative sum
cum_sum <- numeric(length(big_matrix))
# cumulative sum using "for" loop
cum_sum[1] <- big_vector[1]
system.time(
  for(i in 2:length(big_vector)) {
    cum_sum[i] <- cum_sum[i-1] + big_vector[i]
  }  # end for
)  # end system.time
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
rm(list=ls())
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
system.time(row_sums <- apply(big_matrix, 1, sum))

str(rowSums)  # get list of arguments

# calculate row sums
system.time(row_sums <- rowSums(big_matrix))
library(caTools)  # load package "caTools"
# get documentation for package "caTools"
packageDescription("caTools")  # get short description
help(package="caTools")  # load help page
data(package="caTools")  # list all datasets in "caTools"
ls("package:caTools")  # list all objects in "caTools"
detach("package:caTools")  # remove caTools from search path

# median filter
med_ian <- runmed(x=coredata(xts_series), k=vol_window)
# vector of rolling volatility
vo_lat <- runsd(x=abs(coredata(xts_series)),
          k=vol_window, probs=0.9,
          endrule="constant", align="center")
# vector of rolling quantiles
quan_tiles <- runquantile(x=abs(coredata(xts_series)),
                    k=vol_window, probs=0.9,
                    endrule="constant", align="center")
par(oma=c(1, 1, 1, 1), mgp=c(2, 1, 0), mar=c(5, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
set.seed(1121)  # for reproducibility
simu_max <- 1000  # max simulation trials
simu_prices <- 0*1:simu_max  # initialize prices
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
set.seed(1121)  # for reproducibility
simu_max <- 1000  # max simulation trials
barrier_level <- 20  # barrier level
# simulated prices
simu_prices <- cumsum(rnorm(simu_max))
# in_dex is "1" after prices cross barrier_level
in_dex <- cummax(simu_prices > barrier_level)
# find index when prices cross barrier_level
which_index <- which(diff(in_dex)==1)
# fill prices after crossing barrier_level
if (length(which_index)>0) {
  simu_prices[as.logical(in_dex)] <-
    simu_prices[which_index + 1]
}  # end if
# create daily time series starting 2011
ts_var <- ts(data=simu_prices, frequency=365, start=c(2011, 1))
plot(ts_var, type="l", col="black",  # create plot
     lty="solid", xlab="", ylab="")
abline(h=barrier_level, lwd=2, col="red")  # add horizontal line
title(main="Random Prices", line=0)  # add title
