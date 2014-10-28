

library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
thm <- knit_theme$get("acid")
knit_theme$set(thm)



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
    my_var <- 101  # assign
    stop('my error')  # throw error
  }, 
  finally=print(paste("my_var=", my_var))
)  # end tryCatch

tryCatch(  # with error handler
  {  # evaluate expressions
    my_var <- 101  # assign
    stop('my error')  # throw error
  }, 
  error=function(error_cond)  # handler captures error condition
    print(paste("error handler: ", error_cond)),
  finally=print(paste("my_var=", my_var))
)  # end tryCatch



rm(list=ls())
# apply loop without tryCatch
apply(as.matrix(1:5), 1, function(my_var) {  # anonymous function
    stopifnot(my_var != 3)  # check for error
    cat("(cat) my_var=", my_var)  # broadcast
    paste("(return) my_var=", my_var)  # return value
  }  # end anonymous function
)  # end apply



# apply loop with tryCatch
apply(as.matrix(1:5), 1, function(my_var) {  # anonymous function
    tryCatch(  # with error handler
{  # body
  stopifnot(my_var != 3)  # check for error
  cat("(cat) my_var=", my_var)  # broadcast
  paste("(return) my_var=", my_var)  # return value
},
error=function(error_cond)  # handler captures error condition
  paste("handler: ", error_cond),
finally=print(paste("(finally) my_var=", my_var))
    )  # end tryCatch
  }  # end anonymous function
)  # end apply



library(zoo)  # load package zoo
# get all methods for generic function "cbind"
methods("cbind")

# show the method of "cbind" applied to "zoo" objects
cbind.zoo



library(zoo)  # load package zoo
# get all methods for generic function "cbind"
# get generic function methods applied to "zoo" objects
methods(class="zoo")



loadedNamespaces()  # get names of loaded namespaces

search()  # get search path for R objects



# get session info,
# including packages not attached to the search path
sessionInfo()



plot.xts  # package xts isn't loaded and attached
head(xts::plot.xts, 3)
methods("cbind")  # get all methods for function "cbind"
stats::cbind.ts  # cbind isn't exported from package stats
stats:::cbind.ts  # view the non-visible function
getAnywhere("cbind.ts")
library("MASS")  # load package 'MASS'
select  # code of primitive function from package 'MASS'



getAnywhere("cbind.ts")



cbind.ts  # can't view code of non-visible functions
getAnywhere(cbind.ts)  # display function
stats:::cbind.ts  # display function



# get all methods for generic function "plot"
methods("plot")

getAnywhere(plot)  # display function



rm(list=ls())
my.ts <- zoo(rnorm(4), order.by=(Sys.Date() + 0:3))
class(my.ts)
length(my.ts)

# coerce "zoo" object to new class "newts"
class(my.ts) <- "newts"
class(my.ts)

# define "length" method for class "newts"
length.newts <- function(in.ts) {
# "length" method for class" "newts"
  cat("getting length of object from newts class\n")
  length(unclass(in.ts))
}  # end length.newts

# apply new "length" method
length(my.ts)


