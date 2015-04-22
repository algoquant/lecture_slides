library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
thm <- knit_theme$get("acid")
knit_theme$set(thm)
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
num_var1 <- 3  # "<-" and "=" are valid assignment operators
num_var1
num_var1 = 3
num_var1
median(x = 1:10)  # "=" assignment within argument list
x  # x doesn't exist outside the function
median(x <- 1:10)  # "<-" assignment within argument list
x  # x exists outside the function
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
rm(list=ls())
num_var1 <- 1

if (num_var1) {  # numeric zero is FALSE, all other numbers are TRUE
  num_var2 <- 4
} else if (num_var1 == 0) {  # 'else if' together on same line
  num_var2 <- 0
} else {  # 'else' together with curly braces
  num_var2 <- -4
}  # end if

num_var2
rm(list=ls())
color_list <- list("red", "white", "blue")
for (some_color in color_list) {  # loop over list
  print(some_color)
}
for (in_dex in 1:3) {  # loop over vector
  print(color_list[[in_dex]])
}

in_dex <- 1  # 'while' loops need initialization
while (in_dex < 4) {  # while loop
  print(color_list[[in_dex]])
  in_dex <- in_dex + 1
}
# create factor vector
fact_var <- factor(c('b', 'c', 'd', 'a', 'c', 'b'))
fact_var
fact_var[3]

attributes(fact_var)  # get factor attributes
levels(fact_var)  # get allowed values

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
# define a function that returns invisibly
return_invisible <- function(arg_var) {
  invisible(arg_var)
}  # end return_invisible

return_invisible(2)

glob_var <- return_invisible(2)
glob_var
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
