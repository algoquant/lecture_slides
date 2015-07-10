#################################
### Homework and test ideas
#################################


##################################
# miscellaneous

rm(list=ls())  # remove all


### print and display options

options(max.print=80)
options(digits=3)



### plot parameters

par(new=TRUE)  # allow new plot on same chart
par(las=1)  # set text printing to "horizontal"


### startup package loading

library(zoo)
# good package loading script inside functions
stopifnot("package:xts" %in% search() || require("xts", quietly=TRUE))



#################################
# expressions data structures
#################################

################################## test
# 1. (15pts) Create the character string "y = x1 + x2 + x3" from 
# characters "x", "y", "=", "+", and the vector 1:3, using the function 
# paste() with a collapse string,
# You can also use these characters with spaces around them, say " x ",

paste("y", "=", paste(paste0("x", 1:3), collapse=" + "))



################################## test
# 3. (15pts) create a vector of 20 random normal numbers:
vec_tor <- rnorm(20)
# find the indices of numbers greater than 1:
which(vec_tor>1)
# find the numbers greater than 1:
vec_tor[which(vec_tor>1)]
# find the index of the max number:
which.max(vec_tor)
# find the max number (don't use max()):
vec_tor[which.max(vec_tor)]



################################## test
# 1. (15pts) Create a vector sample() of 90 integers, from 1 to 90, 
# and call it "vec_tor", 
# extract every third element of "vec_tor", starting with the first one, 
# into a single column matrix called "mat_rix",
# hint: you can use function seq() and function matrix(), 
# with the proper "byrow" argument,
vec_tor <- sample(1:90)
mat_rix <- matrix(vec_tor[seq(1, length(vec_tor), by=3)], ncol=1)
# or:
mat_rix <- matrix(vec_tor, ncol=3, byrow=TRUE)
mat_rix <- mat_rix[, 1, drop=FALSE]

# extract a vector of odd index elements of "vec_tor" (first, third, etc), 
# and a vector of even index elements (second, fourth, etc.),
# calculate the scalar ("inner") product of the two vectors,
# the value should be a vector with a single element, not a matrix,
# hint: you can use function seq(), or function matrix(), 
# with the proper "byrow" argument,
# use function drop(),
odd_elements <- vec_tor[seq(1, length(vec_tor), by=2)]
even_elements <- vec_tor[seq(2, length(vec_tor), by=2)]
drop(odd_elements %*% even_elements)
# or:
mat_rix <- matrix(vec_tor, ncol=2, byrow=TRUE)
drop(mat_rix[, 1] %*% mat_rix[, 2])



################################## test
# 1. (15pts) Create a matrix called "mat_rix", as follows:
mat_rix <- matrix(1:6, ncol=3)

# assign to "mat_rix" row names "row1", "row2", and 
# column names "col1", "col2", "col3",
# use the functions rownames(), colnames(), and/or dimnames(), 
dimnames(mat_rix) <- list(rows=c("row1", "row2"),
                          columns=c("col1", "col2", "col3"))

# change the names of the rows to "first_row" and "second_row",
# use the function dimnames(), but not the function rownames(),
dimnames(mat_rix)[[1]] <- c("first_row", "second_row")
dimnames(mat_rix)$rows <- c("first_row", "second_row")



################################## test
# 1. (15pts) create a function called get_index(), that 
# calculates the indices of the TRUE elements of a boolean vector,
# get_index() should be equivalent to the function which(), 
# when applied to boolean vectors,
# hint: you can use function length() and then apply vector subsetting,
get_index <- function(vec_tor) (1:length(vec_tor))[vec_tor]


# apply the function get_index() to a boolean vector, and 
# compare the result with using function which(),
vec_tor <- sample(1:9)
get_index(vec_tor==5)
which(vec_tor == 5)


##################################
# Calculate the row and column index (number) containing the specified value of a matrix
# without using which()
# create matrix
set.seed(1121)
mat_rix <- matrix(sample(x=1:9), ncol=3)
# select some value from matrix
val_ue <- median(mat_rix)
# find index of column value of matrix
in_dex <- (val_ue==apply(mat_rix, 2, median))
c_ol <- (1:length(in_dex))[in_dex]
co_lumn <- mat_rix[, in_dex]
in_dex <- (val_ue==co_lumn)
r_ow <- (1:length(in_dex))[in_dex]



##################################
# 2. (5pts) create a numeric vector of length 15 containing 
# random normal variates (rnorm),
# coerce the vector into a matrix of 5 rows and 3 columns,
mat_rix <- matrix(rnorm(15), ncol=3)

# 3. (10pts) extract the values of all the elements that are greater than 1.0,
mat_rix[mat_rix>1.0]

# calculate the indices of all the elements that are greater than 1.0,
which(mat_rix>1.0)

# 4. (5pts) calculate the mean and standard deviation of the 3rd row, 
# and the 2nd column,
mean(mat_rix[3, ])
sd(mat_rix[3, ])
mean(mat_rix[, 2])
sd(mat_rix[, 2])



################################## test
# 2. (15pts) create a numeric vector of length 10 containing random normal variates, using rnorm(),
vec_tor <- rnorm(10)

# assign the names: "el1, ..., el10", to the vector elements, 
# use function paste(), with the proper "sep" argument, 
# you can't use function c()
names(vec_tor) <- paste("el", 1:10, sep='')

# change the vector names to: "num1, ..., num10", using the function gsub(), 
# (you can't use functions c() or paste())
names(vec_tor) <- gsub("el", "num", names(vec_tor))


# extract the element named "num4",
vec_tor["num4"]


# change the vector names to: "my.num1, ..., my.num10", 
# using the function paste(), with the proper "sep" argument,
names(vec_tor) <- paste("my", names(vec_tor), sep='.')


# change the first vector element name back to: "num1", 
# using the function strsplit(), with the proper "split" argument,
# hint: strsplit() returns a list, subset it using [[1]],
names(vec_tor)[1] <- strsplit(names(vec_tor)[1], split='[.]')[[1]][2]
# or:
names(vec_tor)[1] <- strsplit(names(vec_tor)[1], split='.', fixed=TRUE)[[1]][2]


# calculate the indices of the elements that are greater than or equal to -0.5 and less than 1.0,
which((vec_tor >= -0.5) & (vec_tor < 1.0))


# extract the elements that are greater than or equal to -0.5 and less than 1.0,
vec_tor[(vec_tor >= -0.5) & (vec_tor < 1.0)]


# calculate the mean and standard deviation of the vector elements,
mean(vec_tor)
sd(vec_tor)


# combine this vector with the vector "31:33",
c(vec_tor, 31:33)



##################################
# 3. (5pts) Create a matrix of 100 random normal elements, with 4 columns.
mat_rix <- matrix(rnorm(100), ncol=4)

# 4. (5pts) Calculate column means using the function apply(), and omit the NA values.
# hint: pass the parameter "na.rm=TRUE" to function mean().
apply(mat_rix, 2, mean, na.rm=TRUE)

# 3. find all the elements of the matrix that are greater than 1.0, (10pts)
mat_rix[mat_rix > 1]




################################## hw
# 1. (30pts) Create a data frame containing the homework and test scores for 20 students, 
# and call the data frame "student_scores",
# 
# create a vector of strings containing student names, called "student_names",
student_names <- c("Chloe", "Olivia", "Madison", "Ethan", "James", "Amelia", "Anthony", 
                   "Joseph", "Evelyn", "Matthew", "Michael", "Liam", "Allison", "Mason", 
                   "Emma", "Jayden", "Emily", "William", "Ella", "Elizabeth")


# create a factor variable called "tra_ck" of length(student_names),
# containing values sampled from the following vector of strings:
# "Corporate", "Computational", "InfoTech", "Risk",
# Use functions sample() and as.factor(), 
tra_ck <- as.factor(sample(
  x=c("Corporate", "Computational", "InfoTech", "Risk"),
  size=length(student_names),
  replace=TRUE))


# Create the data frame "student_scores" from the vectors "student_names" and "tra_ck",
# Use function data.frame() with "stringsAsFactors=FALSE" to avoid coercing "character" to "factor",
student_scores <- data.frame(
  name=student_names, 
  finance_track=tra_ck,
  stringsAsFactors=FALSE)


# Create a numeric matrix with six columns, called "sco_res", 
# and assign to it random integer scores between 30 and 60,
# Use functions sample() and matrix(), 
sco_res <- matrix(sample(x=30:60, size=6*length(student_names), 
                         replace=TRUE), ncol=6)


# calculate a vector containing average scores for each student, and bind it to "sco_res",
# Use functions cbind() and rowMeans(), 
sco_res <- cbind(sco_res, rowMeans(sco_res))

# Assign the following column names to "sco_res":
# "HW1_score", "HW2_score", "HW3_score", "HW4_score", "test1_score", "test2_score", "avg_score",
colnames(sco_res) <- c("HW1_score", "HW2_score", "HW3_score", "HW4_score", "test1_score", "test2_score", "avg_score")
head(sco_res)

# cbind "sco_res" to the data frame "student_scores",
student_scores <- cbind(student_scores, sco_res)
head(student_scores)



### this is a version where students download "student_scores":
################################## hw
# download the file "student_scores.csv" from NYU Classes,
# the file contains a data frame with student names, scores, and track,
# read the file into a variable called "student_scores" using read.csv(),
# use read.csv(), 
student_scores <- read.csv(file='student_scores.csv')


# Assign letter grades to each student, based on their "avg_score" column, 
# use the following table:
# "A" if "avg_score" >= 50.0
# "A-" if "avg_score" >= 47.5
# "B+" if "avg_score" >= 45.0
# "B" if "avg_score" >= 42.5
# "B-" if "avg_score" >= 40.0
# "C+" if "avg_score" >= 37.5
# "C" if "avg_score" >= 35.0

# breakpoints correspond to categories of the data,
# the first breakpoint should correspond to the lowest categoriy,
# and should have a value less than any of the data,
# Create a named numeric vector of breakpoints for "avg_score", 
# called "brea_ks" as follows: 
brea_ks <- seq(from=35, to=50.0, by=2.5)
names(brea_ks) <- c("C", "C+", "B-", "B", "B+", "A-", "A")


# Create a factor variable containing letter grades, called "letter_grades", 
# There are at least two ways of doing this, but you only need to do it one way,
# 
# In the first approach you can use the names of "brea_ks", and either 
# a for() loop, and/or if() and else(), and/or logical operators "<", ">", etc., 

# first create vector "letter_grades" containing empty strings:
letter_grades <- character(20)
# next populate "letter_grades" with letter grades using a for() loop:
for (brea_k in 1:length(brea_ks)) {
  in_dex <- student_scores[, "avg_score"] >= brea_ks[brea_k]
  letter_grades[in_dex] <- names(brea_ks[brea_k])
}  # end for
letter_grades <- as.factor(letter_grades)

# In the second approach you can use function findInterval() 
# and the names of "brea_ks",
letter_grades <- names(brea_ks[findInterval(x=student_scores[, "avg_score"], 
                                            vec=brea_ks)])
letter_grades <- as.factor(letter_grades)


# cbind "letter_grades" to the data frame "student_scores",
student_scores <- cbind(student_scores, letter_grades)
head(student_scores)



##################################
# 2. (15pts) Sort "student_scores" by "avg_score" column, 
# first in descending order, then in ascending order,
# use function order()
student_scores <- student_scores[order(student_scores$avg_score), ]
head(student_scores)
student_scores <- student_scores[order(student_scores$avg_score, decreasing=TRUE), ]
head(student_scores)


# Calculate the average scores for students for each "finance_track" category,
# use the split-apply-combine procedure, and use functions with() and tapply(),
with(student_scores, 
     tapply(avg_score, finance_track, mean)  # end tapply
)  # end with


##################################
# 3. (15pts) Plot a histogram of the number of students in each 
# "letter_grade" category, you can use the lecture slide titled 
# "Cars93 Data Frame",
# 
# There are at least two ways of doing this, but you need to do 
# it only one way.
# 
# In the first approach you can use column "student_scores$avg_score" 
# and function hist(), and the vector "brea_ks",
hist(student_scores$avg_score, breaks=brea_ks)

# In the second approach you can use column "student_scores$letter_grade", 
# and functions table() and barplot(), 
cont_table <- table(student_scores$letter_grade)
cont_table <- cont_table[order(names(cont_table), decreasing=TRUE)]
barplot(cont_table)

# extract the "class" of the columns of "student_scores" using 
# functions class() and sapply(),
# make sure that none of the columns are factors, 
# except for "finance_track" and "letter_grades",
sapply(student_scores, class)

# save "student_scores" to a comma-delimited CSV file,
# use function write.csv(),
write.csv(student_scores, row.names=FALSE, file='student_scores.csv')





################################## test
# 2. (5pts) Calculate the vector of "class" attributes of the 
# columns of the data frame "Cars93", and call it "class_cars",
# use functions class() and sapply(), 
library(MASS)
class_cars <- sapply(Cars93, class)

# calculate the vector of unique "class" attributes in "class_cars",
# use function unique(), 
unique(class_cars)


##################################
# 3. (10pts) calculate the number of columns with each 
# unique "class" attribute in "class_cars", 
# that is, calculate how many columns are of class "factor", 
# how many are of class "numeric", etc.
# the answer is given simply by using function table(), 
# but you can't use function table(), 
# you must use functions sapply(), sum(), and an anonymous function,
sapply(unique(class_cars), function(unique_class) {
  sum(class_cars==unique_class)
})  # end sapply

# comment: the above is equivalent to using table() below, 
# but the test requires using functions sapply(), sum(), 
# and an anonymous function,
table(class_cars)


##################################
# 4. (10pts) extract the vectors of column names in each 
# unique "class", that is, extract the names of columns 
# of class "factor", the names of columns of class "numeric", etc.
# use functions sapply(), colnames(), and an anonymous function,
sapply(unique(class_cars), function(unique_class) {
  colnames(Cars93)[class_cars==unique_class]
})  # end sapply


##################################
# 5. (10pts) calculate the "means" of columns that are 
# either of class "integer" or "numeric",
# you can use functions sapply(), mean(), and the "%in%" operator,
# some elements of Cars93 are NA, so you will need to pass the 
# parameter "na.rm=TRUE" to mean(),
integer_or_numeric <- class_cars %in% c("numeric", "integer")
sapply(Cars93[, integer_or_numeric], mean, na.rm=TRUE)



##################################
# using mtcars data, plot a boxplot of mpg of cars with six cylinders
boxplot(mtcars[mtcars$cyl==6, ]$mpg)
with(mtcars[mtcars$cyl==6, ], boxplot(mpg))

# 2. (10pts) plot a histogram of "mpg" for all cars in the mtcars data frame, 
# use function truehist(), and set the "prob" argument so that the plot displays the number of cars in each bin,
# "truehist" counts the first two bins differently from "hist"
truehist(mtcars$mpg, nbins="FD", prob=FALSE, col="blue", xlab="mpg", ylab="number of cars", main="mpg true histogram")
hist(mtcars$mpg, breaks="FD", prob=FALSE, col="blue", xlab="mpg", ylab="number of cars", main="mpg histogram")

### using mtcars data, create a data frame called "cars_18", containing cars that have mpg greater than 18,
cars_18 <- mtcars[mtcars$mpg>18, ]
# using the function table(), calculate the number of cars in "cars_18", 
# that have four, six, and eight cylinders
table(cars_18$cyl)
# or
sum(cars_18$cyl==4)
sapply(cars_18$cyl, sum)

with(mtcars[mtcars$mpg>20, ], barplot(mpg))

barplot(mtcars$mpg)
barplot(mtcars[mtcars$cyl==6, ]$mpg)
with(mtcars[mtcars$cyl==6, ], barplot(mpg))



##################################
# 1. (5pts) using the function which() perform the following,
# create a data frame that is a subset of mtcars, 
# and that contains only cars with 6 cylinders,
mtcars_6cyl <- mtcars[mtcars$cyl==6, ]
which(mtcars_6cyl$hp==max(mtcars_6cyl$hp))

# 2. (5pts) find the name of the car with the highest horsepower among 6 cylinder cars,
best_car <- mtcars_6cyl[which(mtcars_6cyl$hp==max(mtcars_6cyl$hp)), ]
rownames(best_car)

# calculate the horsepower and the weight of that car,
best_car$hp
best_car$wt


################################## test
# 1. (15pts) Calculate a vector of means of the numeric columns 
# of the "iris" data frame,
# calculate the means of only those columns that are numeric,
# the output must be a vector, not a list,
# you can use functions lapply(), sapply(), apply(), is.numeric(), 
# unlist(), and mean(), and an anonymous function.
unlist(sapply(iris, function(co_lumn) {if (is.numeric(co_lumn)) mean(co_lumn)}))
# or
sapply(iris[, sapply(iris, is.numeric)], mean)


# 2. (5pts) Calculate the column means of the "setosa" species in the "iris" data.
# You can find all the iris species using the unique function.
unique(iris$Species)
# hint: first create a data frame of the "setosa" species, which is a subset of the "iris" data frame.
# hint: Then apply the expression from point #1 on the data frame of the "setosa" species.
iris_setosa <- iris[iris$Species=="setosa", ]
unlist(sapply(iris_setosa, function(co_lumn) {if (is.numeric(co_lumn)) mean(co_lumn)}))



##############################
# statistis and probability
##############################


##################################
# 1. create a matrix of 30rows x 10columns with random normal variates, (5pts)
mat_rix <- matrix(rnorm(300), ncol=10)

# 2. calculate the mean, standard deviation, skewness, and kurtosis of: (10pts)
# the 5th row, 
in_data <- mat_rix[5, ]
len_data <- length(in_data)
mean_data <- mean(in_data)
sd_data <- sd(in_data)
skew_data <- len_data*sum(((in_data - mean_data)/sd_data)^3)/((len_data-1)*(len_data-2))
kurtosis_data <- len_data*(len_data+1)*sum(((in_data - mean_data)/sd_data)^4)/((len_data-1)^3)
c(mean_data, sd_data, skew_data, kurtosis_data)

# the 8th column,
in_data <- mat_rix[, 8]
len_data <- length(in_data)
mean_data <- mean(in_data)
sd_data <- sd(in_data)
skew_data <- len_data*sum(((in_data - mean_data)/sd_data)^3)/((len_data-1)*(len_data-2))
kurtosis_data <- len_data*(len_data+1)*sum(((in_data - mean_data)/sd_data)^4)/((len_data-1)^3)
c(mean_data, sd_data, skew_data, kurtosis_data)



##################################
# 1. Create a function for calculating the kurtosis of a time series of returns,
calc_kurtosis <- function(in_data=rnorm(1000)) {  # default is normal
  # Calculates the kurtosis of a time series of returns.
  len_data <- length(in_data)
  mean_data <- mean(in_data)
  sd_data <- sd(in_data)
  len_data*(len_data+1)*sum(((in_data - mean_data)/sd_data)^4)/((len_data-1)^3)
}  # end calc_kurtosis

# 2. Using this function calculate the kurtosis of DAX returns, and of t-distribution returns with four degrees of freedom (use the same number of data points in both cases),
# DAX returns in scale 1% = 1.0
ts_rets <- 100*diff(log(EuStockMarkets[, 1]))
# calculate kurtosis of DAX returns
calc_kurtosis(in_data=ts_rets)
# calculate kurtosis of t-distribution
calc_kurtosis(in_data=rt(n=length(ts_rets), df=4))




#################################
# functions
#################################


################################## test
# 3. (15pts) create a function called mult_dots(), 
# which takes dots '...' as its first argument, 
# and "fac_tor" as its second argument, as follows: mult_dots(..., fac_tor),
# The function mult_dots() should sum up the dots '...' argument, 
# then multiply the sum by "fac_tor", and return the result,
mult_dots <- function (..., fac_tor) {
  fac_tor*sum(...)
}  # end mult_dots

# apply the function mult_dots() so that it adds the numbers "1, 2, 3", 
# and then multiplies the sum by "2",
mult_dots(1, 2, 3, fac_tor=2)


################################## hw
# 1. (15pts) Create a function called match_matrix(), similar to match(), 
# but which accepts matrix arguments, as well as vectors,
# match_matrix() should return the row and column indices of the first 
# element of its second argument, that matches its first argument,
# hint: you can use function which(), with the argument "arr.ind=TRUE",
match_matrix <- function(val_ue, mat_rix) {
  which(mat_rix==val_ue, arr.ind=TRUE)
}  # end match_matrix

# call the function match_matrix() as follows, to make sure it works properly:
mat_rix <- matrix(1:6, ncol=3)
match_matrix(5, mat_rix)


##################################
# Create function that calculates row and column containing the extreme value of a matrix.
# The extreme value is calculated by the function "func_tion", which can be "max" or "min", etc.
which_matrix <- function(mat_rix, func_tion="max") {
  func_tion <- match.fun(func_tion)
  which(mat_rix==func_tion(mat_rix), arr.ind=T)
  # tmp <- which(mat_rix==func_tion(mat_rix), arr.ind=T)
  # coordinates <- as.numeric(c(rownames(mat_rix)[tmp[1,1]], colnames(mat_rix)[tmp[1,2]]))
  # coordinates
}  # end which_matrix



################################## hw
# 1. (20pts) Create a function called my_sqrt() that calculates 
# the square root of its single argument,
# my_sqrt() should check if the input is both numeric and positive,
# if the input is numeric and positive, 
#  then my_sqrt() should return the square root,
# if the input is numeric and negative, 
#  then my_sqrt() should issue a warning using warning(), 
#  and return the square root of the absolute value,
# if the input is not numeric, 
#  then my_sqrt() should halt execution using stop(), 
#  and not return anything,
# use "if" and "else" statements,
# you can use functions sqrt(), is.numeric(), warning(), stop(), 
# and the operator "&&",

my_sqrt <- function(arg_var) {
  if (is.numeric(arg_var) && arg_var>=0) {
    sqrt(arg_var)
  } else if (is.numeric(arg_var)) {
    warning("negative input - taking square root of absolute!\n")
    sqrt(abs(arg_var))
  } else {
    stop(paste0("argument \"", arg_var, "\" isn't numeric"))
  }  # end if
}  # end my_sqrt

# call my_sqrt() as follows, to make sure it works properly:
my_sqrt(4)
my_sqrt(-4)
my_sqrt("a")



##################################
# create function called read_numeric() that reads numbers input by the user, and returns them in a vector,
# read_numeric() should ask the user to input a number, and should read the input using the function readline(),
# read_numeric() should read numbers from the console in a "while" loop,
# read_numeric() should validate the inputs, and produce errors and Warnings,
# if the user input is numeric, then read_numeric() should append the input to the numeric output vector,
# if the input is not numeric, then read_numeric() should produce a Warning "input is not numeric!",
# if the input is empty, then read_numeric() should terminate, and return the numeric output vector,
# hint: read_numeric() should use readline(), and can also use is.na(), nchar(), as.numeric(), length(), identical(), etc.
# the function reads numeric lines from input, and returns them in a vector,
# read_numeric() should create a numeric vector consisting of the input numbers, 
# ignore it

read_numeric <- function() {
  out_put <- numeric(0)
  nu_meric <- readline("Enter a number: ")
  while(nchar(nu_meric) > 0) {
    nu_meric <- as.numeric(nu_meric)
    if (!is.na(nu_meric)) {
      out_put <- c(out_put, as.numeric(nu_meric))
    } else {
      warning("input is not numeric!")
    }  # end if
    nu_meric <- readline("Enter a number: ")
  }  # end while
  out_put
}  # end read_numeric
read_numeric()

# old version
read_numeric <- function() {
  out_put <- numeric(0)
  nu_meric <- readline("Enter a number: ")
  while(!identical(nu_meric, "")) {
    nu_meric <- as.numeric(nu_meric)
    if (!is.na(nu_meric)) {
      out_put <- c(out_put, nu_meric)
    }  # end if
    nu_meric <- readline("Enter a number: ")
  }  # end while
  out_put
}  # end read_numeric
read_numeric()



################################## hw
# 2. (20pts) Create a function called re_move(), which takes two arguments:
# re_move() removes the first argument from the second argument, and returns the result,
# the first argument can be a single numeric or string vlaue, 
# the second argument can be a vector, or other object with mupltiple elements,
# if the first argument isn't among the elements of the second argument, 
# then re_move() just returns the second argument unchanged,
# you can use the functions match(), which(), 
# or the operator "%in%", but you don't have to use all of them,
re_move <- function(zap_it, vec_tor) {
  name_match <- match(zap_it, vec_tor)
  if (is.na(name_match))
    vec_tor
  else
    vec_tor[-name_match]
}  # end re_move

# call the function re_move() as follows, to make sure it works properly:
re_move(3, 1:5)
re_move(6, 1:5)
re_move("bye", c("hello", "bye", "there"))
re_move("bye", c("hello", "there"))



#################################
# plotting
#################################


##################################
# add better format X-axis date labels (not required for full credit)
# first plot without X-axis
plot(zoo_series, type="l", lwd=2, xlab="", ylab="", xaxt="n")
# create X-axis date labels
axis_dates <- seq(from=as.Date("2013-09-01"), to=Sys.Date(), by="quarter")
# add X-axis
axis(side=1, at=axis_dates, labels=format(axis_dates, "%b-%y"))



##################################
# 20pts (even without legend)
# 3. Plot the probability density of DAX returns together with t-distribution returns with four degrees of freedom on a single plot,
# plot t-distribution
x_var <- seq(-5, 5, length=100)
plot(x=x_var, y=dt(x_var, df=4), type="l", lwd=2, xlab="", ylab="", ylim=c(0, 0.6))
# add line for density of DAX returns
lines(density(ts_rets), col="red", lwd=2)
# add legend
legend("topright", title="DAX vs t-distr", legend=c("t-distr", "DAX"), 
       inset=0.05, cex=0.8, lwd=2, lty=c(1, 1), col=c("black", "red"))




#################################
# dates and times
#################################

##################################
# 1. (30pts) "my_date" is a numeric date that represents "1997-05-18",
# convert my_date to a POSIXct date, with time zone equal to "America/New_York",
# you can use functions from package lubridate, 
# or other functions such as paste() and substr(),
# you will need to add the century "19" to the year,
# you will not receive any credit for creating a date "by hand" as follows: as.POSIXct("1997-05-18"),
my_date <- 970518
library(lubridate)
ymd(paste0(19, my_date), tz="America/New_York")
# as.POSIXct("1997-05-18")
as.POSIXct(
  paste(paste0(19, substr(my_date, 1, 2)), 
        substr(my_date, 3, 4), 
        substr(my_date, 5, 6), sep="-")
)



##################################
# 2. (5pts) create a vector of decimal dates as follows:
# date_time <- 2014 + (1:5)/12
# convert them to POSIXct dates, using a "lubridate" function,
library(lubridate)
date_time <- 2014 + (1:5)/12
date_decimal(date_time)
# or
date_decimal(date_time, tz="America/New_York")



##############################
# numerical methods
##############################


##################################
# double barrier simulation
# 1. Create an R script for simulating 1000 random prices using rnorm(),
set.seed(1121)  # for reproducibility
## set up simulation parameters
simu_max <- 1000  # max simulation trials
barrier_first <- -10  # barrier level #1
barrier_second <- 10  # barrier level #2

## initialize simulation variables
# initialize vector of prices to zero
simu_prices <- 0.0*(1:simu_max)
# first simulated price
simu_prices[1] <- rnorm(1)
# starting value of simulation index
simu_index <- 2
# Boolean variable keeps track if barrier_first was crossed yet
first_crossed <- FALSE
# Boolean variable keeps track if barrier_first was crossed yet
both_crossed <- FALSE


# 2. (35pts) Use a while loop to stop the simulation if the prices first cross barrier_first=-10, 
# and then cross barrier_second=10,

## perform the simulation in while loop
while ((simu_index <= simu_max) && !both_crossed) {
# the "while" clause is the critical part of this homework
# the "while" clause is TRUE only if simulation hasn't reached end yet
# and if price haven't crossed both barriers yet
# first, check if first barrier was crossed
  first_crossed <- first_crossed || (simu_prices[simu_index-1] < barrier_first)
# second, check if both barriers were crossed
  both_crossed <- both_crossed || 
    (first_crossed && 
       (simu_prices[simu_index - 1] > barrier_second))
# simulate next price
  simu_prices[simu_index] <- simu_prices[simu_index - 1] + rnorm(1)
# advance simu_index
  simu_index <- simu_index + 1
}  # end while

## fill remaining zero prices
if (simu_index <= simu_max) {
  simu_prices[simu_index:simu_max] <- simu_prices[simu_index - 1]
}

# create daily time series starting 2011
ts_prices <- ts(data=simu_prices, frequency=365, start=c(2011, 1))

# 3. plot the prices and the two barrier levels,

plot(ts_prices, type="l", col="black", lty="solid", xlab="", ylab="")
abline(h=barrier_first, lwd=2, col="blue")  # add horizontal line
abline(h=barrier_second, lwd=2, col="red")  # add horizontal line
title(main="Random Prices", line=0)  # add title



##################################
# 1. (30pts) Simulate random events using a "while" loop.
# "while" loops are often used in simulations, 
# when the number of required loops is unknown in advance,
# 
# Perform a simulation of randomly flipping a coin,
# record the number of heads, and stop when the number 
# of heads (stored in "he_ads") reaches "heads_max=10", 
# record the number of simulation loops "num_loops",
# use functions while() and runif(),
# 
# the coin may be biased, that is, the probability of 
# obtaining heads or tails may not be equal to 0.5,
# let "coin_bias" be the probability of obtaining heads,
# perform the simulation for a biased coin, with "coin_bias=0.4"

set.seed(1121)  # for reproducibility
heads_max <- 10  # max number of heads
coin_bias <- 0.4  # probability of obtaining heads
he_ads <- 0  # initialize heads counter
num_loops <- 0  # initialize loop counter
while (he_ads < heads_max) {
# flip coin and record it if it's heads
  he_ads <- he_ads + (runif(1) < coin_bias)
# or for unbiased coins:
#  he_ads <- he_ads + sample(0:1, 1)
# advance loop counter
  num_loops <- num_loops + 1
}  # end while


# Create a function called coin_flip(), that calculates the 
# number of coin flips needed to obtain a certain number of heads, 
# equal to "heads_max", 
# coin_flip() should perform a simulation of randomly flipping 
# a biased coin, using a "while" loop,
# coin_flip() should take two arguments:
# "heads_max" for the number of heads that need to be tossed
# before the simulation ends, and 
# "coin_bias" the probability of obtaining heads (with default
# value equal to 0.5),
# coin_flip() should return the number of simulation loops that 
# were performed before "heads_max" heads were tossed,
# coin_flip() should call set.seed(1121), for reproducibility,

coin_flip <- function(heads_max, coin_bias=0.5) {
  set.seed(1121)  # for reproducibility
  he_ads <- 0  # initialize heads counter
  num_loops <- 0  # initialize loop counter
  while (he_ads < heads_max) {
# flip coin and record it if it's heads
    he_ads <- he_ads + (runif(1) < coin_bias)
# advance loop counter
    num_loops <- num_loops + 1
  }  # end while
  num_loops
}  # end coin_flip


# call coin_flip() as follows, to make sure it works properly:
coin_flip(heads_max=10, coin_bias=0.5)
coin_flip(heads_max=100, coin_bias=0.5)
coin_flip(heads_max=10, coin_bias=0.1)
coin_flip(heads_max=10, coin_bias=0.9)


# Calculate the number of coin flips needed to obtain 
# a certain number of heads, using different "coin_bias" 
# parameters, from 0.2 to 0.8, in 0.1 increments, 
# with "heads_max=10",
# 
# perform an sapply() loop over a vector of "coin_bias" parameters,
# and pass "heads_max=10" to coin_flip() using the "..." argument,
# plot the vector returned by sapply(), and give proper names to 
# the axis labels,
coin_biases <- seq(from=0.2, to=0.8, by=0.1)
num_flips <- sapply(coin_biases, coin_flip, heads_max=10)
plot(x=coin_biases, y=num_flips, t="l", 
     xlab="coin bias", ylab="number of coin flips")




#################################
# data munging input output error handling
#################################

################################## hw
# summary: loading and scrubbing a matrix containing bad data,
# 1. (5pts) 
# download the file "matrix_bad.csv" from NYU Classes),
# the file contains a numeric matrix with row and column names, 
# one column contains a bad data element that isn't numeric,
# read the file into a variable called "mat_rix" using read.csv(),
# make sure to read strings as strings, not as  factors,
# read in properly the row names of "mat_rix",
# you can either use the first column of data for row names, 
# or use function read.csv() with arguments 
# "row.names=1" and "stringsAsFactors=FALSE",
mat_rix <- read.csv(file="matrix_bad.csv", stringsAsFactors=FALSE)
rownames(mat_rix) <- mat_rix[, 1]
mat_rix <- mat_rix[, -1]
# or
mat_rix <- read.csv(file="matrix_bad.csv", row.names=1,
                    stringsAsFactors=FALSE)


# 2. (15pts) determine the class of "mat_rix", and 
# the class of each column of "mat_rix",
# vector of column classes
class(mat_rix)
col_class <- sapply(mat_rix, class)

# calculate the index of the column that is of class "character", 
# you can use which(),
col_index <- which(col_class=="character")

# extract (copy) the column that is of class "character" 
# to a variable named "col_bad",
col_bad <- mat_rix[, col_index]

# coerce "col_bad" to a numeric vector,
# you can use as.numeric(),
col_bad <- as.numeric(col_bad)

# calculate the index of the element of "col_bad" that is NA,
# replace the NA element with zero,
# you can use which() and is.na(),
which(is.na(col_bad))
col_bad[is.na(col_bad)] <- 0


# 3. (15pts) summary: perform an apply() loop over the columns 
# of "mat_rix", to coerce "mat_rix" to a numeric matrix,
# 
# copy the row names of "mat_rix" to a vector called "row_names",
# you can use row.names(),
row_names <- row.names(mat_rix)

# perform an apply() loop over the columns of "mat_rix", 
# and coerce the columns to numeric vectors,
# copy the result back on to "mat_rix", 
# you can use as.numeric(),
mat_rix <- apply(mat_rix, 2, as.numeric)

# restore the row names of "mat_rix" using "row_names",
# you can use row.names(),
row.names(mat_rix) <- row_names

# replace the NA element of "mat_rix" with zero,
# you can use is.na(),
mat_rix[is.na(mat_rix)] <- 0

# coerce "mat_rix" to a matrix, 
# you can use as.matrix(),
mat_rix <- as.matrix(mat_rix)



################################## hw
# 1. (5pts) download and read the comma-delimited CSV file called "drawdowns.csv",
# you can use functions read.csv(), readLines(), scan(), or any other functions you choose,
# you may also want to use the option "stringsAsFactors=FALSE",
# the first column of "drawdowns.csv" are rownames,
# call the resulting data frame as "draw_downs",
# hint: open "drawdowns.csv" in Notepad or some other text editor,
draw_downs <- read.csv(file="C:/Develop/R/FRE6871/drawdowns.csv")
# or better:
draw_downs <- read.csv(file="C:/Develop//R/FRE6871/drawdowns.csv", stringsAsFactors=FALSE)

# 2. (15pts) replace colnames of "draw_downs" with the strings in the first row of "draw_downs",
# if necessary, convert factors into character strings, 
# colnames should be strings, starting with "From", "Trough", ...
# hint: you may need to use "sapply",
colnames(draw_downs) <- sapply(draw_downs[1, ], as.character)
# or if "stringsAsFactors=FALSE" was used then:
colnames(draw_downs) <- draw_downs[1, ]
# remove the first row,
draw_downs <- draw_downs[-1, ]
colnames(draw_downs)

# 3. (25pts) convert the first three columns of "draw_downs" to "POSIXct" dates, 
# with "tz" set to "America/New_York",
# you can use function as.POSIXct() with "format" options,
# you can also use package "lubridate", and functions mdy(), dmy_hm(), etc.,
# the first column of "draw_downs" are dates formatted as "month-day-year",
# the second column are dates as "year-month-day",
# the third column are dates as "day-month-year",
library(lubridate)
draw_downs[, 1] <- mdy(draw_downs[, 1], tz="America/New_York")
draw_downs[, 2] <- ymd(draw_downs[, 2], tz="America/New_York")
draw_downs[, 3] <- dmy_hm(draw_downs[, 3], tz="America/New_York")
# or:
draw_downs[, 1] <- as.POSIXct(draw_downs[, 1], format="%m/%d/%Y", tz="America/New_York")
draw_downs[, 2] <- as.POSIXct(draw_downs[, 2], format="%y-%m-%d", tz="America/New_York")
draw_downs[, 3] <- as.POSIXct(draw_downs[, 3], format="%d/%m/%Y", tz="America/New_York")

# 4. (15pts) convert the remaining columns to numeric, 
# hint: you may need to use "sapply",
draw_downs[, 4:7] <- sapply(draw_downs[, 4:7], as.character)
draw_downs[, 4:7] <- sapply(draw_downs[, 4:7], as.numeric)
# or if "stringsAsFactors=FALSE" was used then:
draw_downs[, 4:7] <- sapply(draw_downs[, 4:7], as.numeric)



################################## hw
# error handling within an sapply loop,
# 
# download the file "matrix_bad.csv" from NYU Classes),
# the file contains a numeric matrix with row and column names, 
# one column contains a bad data element that isn't numeric,
# read the file into a variable called "mat_rix" using read.csv(),
# make sure to read strings as strings, not as  factors,
# read in properly the row names of "mat_rix",
# you can either use the first column of data for row names, 
# or use function read.csv() with arguments 
# "row.names=1" and "stringsAsFactors=FALSE",
mat_rix <- read.csv(file="matrix_bad.csv", stringsAsFactors=FALSE)
rownames(mat_rix) <- mat_rix[, 1]
mat_rix <- mat_rix[, -1]
# or
mat_rix <- read.csv(file="matrix_bad.csv", row.names=1,
                    stringsAsFactors=FALSE)

# 1. (5pts) calculate the sums of the columns of "mat_rix",
# by performing an sapply loop over the columns of "mat_rix" 
# you can use sapply(), sum() with argument "na.rm",
# and an anonymous function,
# the anonymous function should coerce each column to numeric, 
# and then calculate its sum,
col_sums <- sapply(mat_rix, function(co_lumn) {
  sum(as.numeric(co_lumn), na.rm=TRUE)
}  # end anon function
)  # end apply


# set warning option to "2",
# perform the above sapply() loop again,
# it now produces an error, and doesn't return anything,
# you can use options() with argument "warn",
options(warn=2)


# 3. (45pts) rewrite the above sapply loop and wrap the body 
# of the anonymous function in tryCatch(),
# create another anonymous function to use as an error handler 
# in tryCatch(), 
# the error handler should write the text of the error condition 
# to the console, 
# the error handler should also write the column data to a file 
# called "error.txt",
# you can omit the "finally" argument,
col_sums <- sapply(mat_rix, 
                  function(co_lumn)
                    tryCatch( {  # body
                      sum(as.numeric(co_lumn), na.rm=TRUE)
                    },
                    error=function(error_cond) {
                      cat(paste("error:", error_cond))
                      cat(co_lumn, file="error.txt", sep="\n", append=TRUE)
                    }  # end error handler
                    )  # end tryCatch
)  # end sapply

# the apply loop returns a list,
# flatten the list into a vector using do.call() and cbind(),
do.call(cbind, col_sums)



##################################
# demonstrate that sapply returns list if function returns vectors of variable length
# calculate stats of ts and return as vector of variable length
my_stats <- function(ts_var) {
  c(max(ts_var), min(ts_var), mean(ts_var), if (rnorm(1)>0) 1 else NULL)
}  # end my_stats

# sapply returns list because of vectors of variable length
out_sapply <- sapply(EuStockMarkets, my_stats)



