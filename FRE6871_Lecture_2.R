library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
thm <- knit_theme$get("acid")
knit_theme$set(thm)
# create factor vector
fact_var <- factor(c('b', 'c', 'd', 'a', 'c', 'b'))
fact_var
fact_var[3]
attributes(fact_var)  # get factor attributes
levels(fact_var)  # get allowed values
as.numeric(fact_var)  # get encoding vector
table(fact_var)  # get contingency (frequency) table
is.vector(fact_var)
as.vector(fact_var)  # coerce factor to character vector
mat_var <- matrix(5:10, nrow=2, ncol=3)  # create a matrix
mat_var  # by default matrices are constructed column-wise

mat_var[2, 3]  # extract third element from second row
mat_var[2, ]  # extract second row
mat_var[, 3]  # extract third column
mat_var[, c(1,3)]  # extract first and third column
mat_var[, -2]  # remove second column
attributes(mat_var)  # get matrix attributes
dim(mat_var)  # get dimension attribute
class(mat_var)  # get class attribute
rownames(mat_var) <- c("row1", "row2")  # set the rownames attribute
colnames(mat_var) <- c("col1", "col2", "col3")  # set the colnames attribute
mat_var
mat_var["row2", "col3"]  # get third element from second row
names(mat_var)  # get the names attribute
dimnames(mat_var)  # get dimnames attribute
attributes(mat_var)  # get matrix attributes
mat_var  # matrix with column names
mat_var[1, ]  # subset rows by index
mat_var[, "col1"]  # subset columns by name
mat_var[, c(TRUE, FALSE, TRUE)]  # subset columns by logical vector
mat_var[1, ]  # subsetting can produce a vector!
class(mat_var); class(mat_var[1, ])
is.matrix(mat_var[1, ]); is.vector(mat_var[1, ])
mat_var[1, , drop=FALSE]  # drop=FALSE preserves matrix
class(mat_var[1, , drop=FALSE])
is.matrix(mat_var[1, , drop=FALSE]); is.vector(mat_var[1, , drop=FALSE])
mat_var <- 1:6  # create a vector
class(mat_var)  # get its class

dim(mat_var) <- c(2, 3)  # add dimension attribute to coerce into matrix
class(mat_var)  # get its class
is.matrix(mat_var)  # is the object a matrix?

dimnames(mat_var) <- list('rows'=c('row1', 'row2'),  # set dimnames attribute
                  'columns'=c('col1', 'col2', 'col3'))

mat_var
vec_var1 <- 1:3  # define vector
vec_var2 <- 6:4  # define vector
cbind(vec_var1, vec_var2)  # bind into columns
rbind(vec_var1, vec_var2)  # bind into rows
vec_var2 <- c(vec_var2, 7)  # extend second vector to four elements
cbind(vec_var1, vec_var2)  # recycling rule applied
1:6 + c(10, 20)  # another example of recycling rule
vec_var1
vec_var2 <- 6:4  # define vector
# multiply two vectors element-by-element
vec_var1 * vec_var2
# calculate scalar ("inner") product
vec_var1 %*% vec_var2
# calculate inner product and drop dimensions
drop(vec_var1 %*% vec_var2)
mat_var
# multiply vector by matrix
mat_var %*% vec_var1  # single column matrix
drop(mat_var %*% vec_var1)  # vector
# multiply matrix by vector
# fails because dimensions of objects aren't conformable
vec_var1 %*% mat_var
# works after transpose
drop(vec_var1 %*% t(mat_var))
rm(list=ls())
TRUE | FALSE
TRUE | NA
vec_var1 <- c(2, 4, 6)
vec_var1 < 5
(vec_var1 < 5) & (vec_var1 > 3)
vec_var1[(vec_var1 < 5) & (vec_var1 > 3)]
vec_var2 <- c(-10, 0, 10)
vec_var1 < vec_var2
c(FALSE, TRUE, FALSE) & c(TRUE, TRUE, FALSE)
c(FALSE, TRUE, FALSE) | c(TRUE, TRUE, FALSE)
rm(list=ls())
c(FALSE, TRUE, FALSE) && c(TRUE, TRUE, FALSE)
c(FALSE, TRUE, FALSE) || c(TRUE, TRUE, FALSE)
echo_true = function() {cat("echo_true\t"); TRUE}
echo_false = function() {cat("echo_false\t"); FALSE}
echo_true() | echo_false()
echo_true() || echo_false()  # echo_false() isn't evaluated at all!
vec_var <- c(2, 4, 6)
# works (does nothing) using '&&'
if (is.matrix(vec_var) && (vec_var[2, 3] > 0)) {
  vec_var[2, 3] <- 1
}
# no short-circuit so fails (throws an error)
if (is.matrix(vec_var) & (vec_var[2, 3] > 0)) {
  vec_var[2, 3] <- 1
}
num_var <- 2
num_var==2
identical(num_var, 2)

identical(num_var, NULL) 
num_var==NULL

vec_var <- c(2, 4, 6)
vec_var==2
identical(vec_var, 2)
some_values <- sample(1:10)
some_values
which(some_values==5)
which(some_values>5)
which.max(some_values)
which.min(some_values)
match(5, some_values)
match(-5, some_values)
5 %in% some_values
-5 %in% some_values
c(5, -5) %in% some_values
some_values <- rnorm(10) + 1
some_values
if(any(some_values < 0))
  cat("vector contains negative values\n")
num_var1 <- 3  # "<-" and "=" are valid assignment operators
num_var1
num_var1 = 3
num_var1
2<-3  # "<" operator confused with "<-"
2 < -3  # add space or brackets to avoid confusion
median(x = 1:10)  # "=" assignment within argument list
x  # x doesn't exist outside the function
median(x <- 1:10)  # "<-" assignment within argument list
x  # x exists outside the function
# define a function with two arguments
test_func <- function(first_arg, second_arg) {  # body
  first_arg + second_arg  # returns last evaluated statement
}  # end test_func

test_func(1, 2)  # apply the function
args(test_func)  # display argument

# define function that uses variable from enclosure environment
test_func <- function(first_arg, second_arg) {
  first_arg + second_arg + glob_var
}  # end test_func

test_func(3, 2)  # error - glob_var doesn't exist yet!
glob_var <- 10  # create glob_var
test_func(3, 2)  # now works
test_func <- function(first_arg=2, second_arg=1) {
# default values can be specified in the argument list
  first_arg + 2*second_arg
}  # end test_func

test_func(first_arg=3, second_arg=2)  # bind by name
test_func(first=3, second=2)  # partial name matching
test_func(3, 2)  # bind by position
test_func(second_arg=2, 3)  # mixed binding
test_func()  # use default values of arguments
test_func(3, 2, 1)  # too many arguments
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
calc_skew  # show the function code

getAnywhere(calc_skew)  # display function
rm(list=ls())
lazy_func <- function(arg1, arg2) {  # define function lazy_func
  2*arg1  # just multiply first argument
}  # end lazy_func
lazy_func(3, 2)  # bind arguments by position
lazy_func(3)  # second argument was never evaluated!
lazy_func <- function(arg1, arg2) {  # define function lazy_func
  cat(arg1, '\n')  # write to output
  cat(arg2)  # write to output
}  # end lazy_func
lazy_func(3, 2)  # bind arguments by position
lazy_func(3)  # first argument written to output
rm(list=ls())
match_dots <- function(arg1=2, arg2=1, ...) {  # define function match_dots
# default values can be specified in the argument list
  arg1 + 2*arg2 + sum(...)
# the function returns the last evaluated statement
}  # end match_dots
match_dots(3, 2)  # match arguments by position
match_dots(3, 2, 5, 8)  # extra arguments
match_dots()  # use default value of arguments
str(paste)  # function 'paste' can take many arguments
paste('a', 'b', sep = ':')  # bind arguments by name
paste('a', 'b', se = ':')  # partial name matching fails!
sum_dots <- function(arg_var, ...) {  # define recursive function
# returns the sum of its argument list
  if (missing(...)) {  # check if dots are empty
    return(arg_var)  # just one argument left
  } else {
    arg_var + sum_dots(...)  # sum remaining arguments
  }  # end if
}  # end sum_dots
sum_dots(1, 2, 3, 4)
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
# create a list with two elements
list_var <- list(c('a', 'b'), 1:4)
list_var
c(class(list_var), typeof(list_var))
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
list_var[[c(2, 3)]]  # extract third element of second element
list_var$second  # extract second element
list_var$s  # extract second element - partial name matching
list_var$second[3]  # extract third element of second element
list_var <- list()  # empty list
list_var$a <- 1
list_var[2] <- 2
list_var
names(list_var)
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
data_frame[2, 3]  # extract second row and third column
data_frame[[3]]  # extract third column
data_frame$color[3]  # extract third row from column 'color'
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
# ?iris  # get information on iris
dim(iris)
head(iris, 2)
colnames(iris)
unique(iris$Species)  # extract list of unique elements of iris
class(unique(iris$Species))
# find which columns of iris are numeric
sapply(iris, is.numeric)
# calculate means of iris columns
sapply(iris, mean)  # returns NA for Species
# ?mtcars  # get information on mtcars - data from 1974 Motor Trend magazine
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
da_ta <- c(1, 2, NA, 4, NA, 5)  # create vector with some NA values
da_ta
mean(da_ta)  # returns NA, when NAs are input
mean(da_ta, na.rm=TRUE)  # remove NAs from input data
da_ta[!is.na(da_ta)]  # delete the NA values
sum(!is.na(da_ta))  # count non-NA values
rm(list=ls())
head(airquality)  # airquality data has some NAs
dim(airquality)
sum(!complete.cases(airquality))  # number of NAs
head(airquality[!complete.cases(airquality), ])  # display some NAs
rm(list=ls())
good_air <- airquality[complete.cases(airquality), ]  # remove NAs
dim(good_air)
head(good_air)  # NAs removed
library(zoo)  # load package zoo
good_air <- na.locf(airquality)  # replace NAs
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
# vectors can be initialized to NULL
da_ta <- NULL
for (in_dex in 1:5)
  da_ta <- c(da_ta, in_dex)
da_ta
# NA value isn't ignored
da_ta <- NA
for (in_dex in 1:5)
  da_ta <- c(da_ta, in_dex)
da_ta
