

library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
thm <- knit_theme$get("acid")
knit_theme$set(thm)



par(oma=c(1, 1, 1, 1), mgp=c(2, 1, 0), mar=c(5, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
set.seed(1121)  # for reproducibility
simu_max <- 1000  # max simulation trials
simu_prices <- 0*1:simu_max  # initialize prices
barrier_level <- 20  # barrier level
simu_prices[1] <- 0  # first simulated price
simu_index <- 2  # initialize simulation index
while ((simu_index <= simu_max) && 
 (simu_prices[simu_index - 1] < barrier_level)) {
  simu_prices[simu_index] <- # simulate next price
    simu_prices[simu_index - 1] + rnorm(1)
  simu_index <- simu_index + 1  # advance simu_index
}  # end while
if (simu_index <= simu_max) {  # fill zero prices
  simu_prices[simu_index:simu_max] <- simu_prices[simu_index - 1]
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
# simu_index is "1" after prices cross barrier_level
simu_index <- cummax(simu_prices > barrier_level)
# find index when prices cross barrier_level
which_index <- which(diff(simu_index)==1)
# fill prices after crossing barrier_level
if (length(which_index)>0) {
  simu_prices[as.logical(simu_index)] <- 
    simu_prices[which_index + 1]
}  # end if
# create daily time series starting 2011
ts_var <- ts(data=simu_prices, frequency=365, start=c(2011, 1))
plot(ts_var, type="l", col="black",  # create plot
     lty="solid", xlab="", ylab="")
abline(h=barrier_level, lwd=2, col="red")  # add horizontal line
title(main="Random Prices", line=0)  # add title



big_matrix <- matrix(1:1000000, ncol=10)
# allocate memory
row_sums <- vector(mode="numeric", length=nrow(big_matrix))

system.time(
  for(i in 1:nrow(big_matrix)) {
    row_sums[i] <- sum(big_matrix[i,])
  }  # end for
)  # end system.time

system.time(row_sums <- apply(big_matrix, 1, sum))



system.time(row_sums <- apply(big_matrix, 1, sum))

str(rowSums)  # get list of arguments

# calculate row sums
system.time(row_sums <- rowSums(big_matrix))



# function "double_it" returns the double of its argument
double_it <- function(in_var=NULL) {
# check if argument is valid and return double
  if (is.null(in_var)) {
    return("double_it: in_var is missing")
  } else if (is.numeric(in_var)) {
    2*in_var
  } else {
    cat("double_it: in_var is not numeric")
  }
}  # end double_it
double_it(3)
double_it("a")
double_it()

double_it <- function(in_var) {
# check if argument is valid and return double
  if (missing(in_var)) {
    return("double_it: in_var is missing")
  } else if (is.numeric(in_var)) {
    2*in_var
  } else {
    cat("double_it: in_var is not numeric")
  }
}  # end double_it
double_it(3)
double_it("a")
double_it()



# function "double_it" returns the double of its argument
double_it <- function(in_var=NULL) {
# check if argument is valid and return double
  if (missing(in_var)) {
    stop("double_it: in_var is missing")
  } else if (!is.numeric(in_var)) {
    stop("double_it: in_var is not numeric")
  } else {
    2*in_var
  }
}  # end double_it
double_it(3)
double_it("a")
double_it()



# function "double_it" returns the double of its argument
double_it <- function(in_var=NULL) {
# check if argument is valid and return double
  stopifnot(!is.null(in_var) && is.numeric(in_var))
  2*in_var
}  # end double_it
double_it(3)
double_it("a")
double_it()



# function "sum_two" returns the sum of its two arguments
sum_two <- function(in_var1, in_var2) {  # even more robust
# check if at least one argument is not missing
  stopifnot(!missing(in_var1) || !missing(in_var2))
# check if arguments are valid and return sum
  if (is.numeric(in_var1) && is.numeric(in_var2)) {
    in_var1 + in_var2  # both valid
  } else if (is.numeric(in_var1)) {
    cat("in_var2 is not numeric")
    in_var1  # in_var1 is valid
  } else if (is.numeric(in_var2)) {
    cat("in_var1 is not numeric")
    in_var2  # in_var2 is valid
  } else {
    stop("none of the arguments are numeric")
  }
}  # end sum_two
sum_two(1, 2)
sum_two(5, 'a')
sum_two('a', 5)
sum_two('a', 'b')
sum_two()



# ?options
getOption("warn")
getOption("error")
catch_missing <- function(in_var) {
# returns its argument
  if (missing(in_var)) {
    warning("catch_missing: in_var was missing")
  } else {
    in_var
  }
}  # end catch_missing
catch_missing(5)
options(warn=-1)
catch_missing()
options(warn=0)
catch_missing()
options(warn=1)
catch_missing()
options(warn=3)
catch_missing()



str(tryCatch)  # get arguments of tryCatch()
tryCatch(  # without error handler
  {  # evaluate expressions
    num_var <- 101  # assign
    stop('my error')  # throw error
  }, 
  finally=print(paste("num_var=", num_var))
)  # end tryCatch

tryCatch(  # with error handler
  {  # evaluate expressions
    num_var <- 101  # assign
    stop('my error')  # throw error
  }, 
  error=function(error_cond)  # handler captures error condition
    print(paste("error handler: ", error_cond)),
  finally=print(paste("num_var=", num_var))
)  # end tryCatch



rm(list=ls())
# apply loop without tryCatch
apply(as.matrix(1:5), 1, function(num_var) {  # anonymous function
    stopifnot(num_var != 3)  # check for error
    cat("(cat) num_var =", num_var, "\n")  # broadcast
    paste("(return) num_var =", num_var)  # return value
  }  # end anonymous function
)  # end apply



# apply loop with tryCatch
apply(as.matrix(1:5), 1, function(num_var) {  # anonymous function
    tryCatch(  # with error handler
{  # body
  stopifnot(num_var != 3)  # check for error
  cat("(cat) num_var =", num_var, "\t")  # broadcast
  paste("(return) num_var =", num_var)  # return value
},
error=function(error_cond)  # handler captures error condition
  paste("handler: ", error_cond),
finally=print(paste("(finally) num_var =", num_var))
    )  # end tryCatch
  }  # end anonymous function
)  # end apply



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
# split the mtcars data frame based on number of cylinders
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



data_cars <- sapply(split_cars,  # get several mpg stats for each cylinder group
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
do.call(cbind, data_cars)  # do.call flattens list into a matrix


