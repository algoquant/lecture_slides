library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
options(digits=3)
thm <- knit_theme$get("acid")
knit_theme$set(thm)
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
# split mtcars data frame based on number of cylinders
split_cars <- split(mtcars, mtcars$cyl)
str(split_cars, max.level=1)
names(split_cars)
# mean mpg for each cylinder group
unlist(lapply(split_cars, function(x) mean(x$mpg)))
# function aggregate() performs split-apply-combine
aggregate(formula=(mpg ~ cyl), data=mtcars, FUN=mean)
# aggregate() all columns
aggregate(x=mtcars, by=list(cyl=mtcars$cyl), FUN=mean)
# mean mpg for each cylinder group
tapply(X=mtcars$mpg, INDEX=mtcars$cyl, FUN=mean)
# using with() environment
with(mtcars, tapply(X=mpg, INDEX=cyl, FUN=mean))
# function by() instead of tapply()
with(mtcars, by(data=mpg, INDICES=cyl, FUN=mean))
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
foo <- 0.3/3
foo  # printed as "0.1"
foo - 0.1  # foo is not equal to "0.1"
foo == 0.1  # foo is not equal to "0.1"
print(foo, digits=10)
print(foo, digits=16)
# foo is equal to "0.1" within machine precision
all.equal(foo, 0.1)
foo <- (3-2.9)
print(foo, digits=20)
# info machine precision of computer R is running on
# ?.Machine
# machine precision
.Machine$double.eps
foo <- sqrt(2)
foo^2  # printed as "2"
foo^2 == 2  # foo^2 is not equal to "2"
print(foo^2, digits=20)
# foo^2 is equal to "2" within machine precision
all.equal(foo^2, 2)
# numbers with precision 0.1
0.1*(1:10)
# round to precision 0.1
round(3.675, 1)
# round to precision 1.0
round(3.675)
# round to nearest even number
c(round(2.5), round(3.5), round(4.5))
round(4:20/2)  # round to nearest even number
trunc(3.675)  # truncate
4.7 %/% 0.5  # modulo division
4.7 %% 0.5  # remainder of modulo division
# reversing modulo division usually
# returns the original number
(4.7 %% 0.5) + 0.5 * (4.7 %/% 0.5)
# modulo division of non-integer numbers can
# produce incorrect results
0.6 %/% 0.2  # produces 2 instead of 3
6 %/% 2  # use integers to get correct result
# 0.2 stored as binary number
# slightly larger than 0.2
print(0.2, digits=22)
