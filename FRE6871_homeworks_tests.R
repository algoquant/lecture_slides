##############################
### Homework and test ideas
##############################


##############################
# miscellaneous

# remove all
rm(list=ls())
# check if package is installed - slow
any(grepl("xts", installed.packages()))


### print and display options

options(max.print=80)
options(digits=3)

### startup package loading

suppressPackageStartupMessages(library(zoo))
# good package loading script inside functions
stopifnot("package:xts" %in% search() || require(xts, quietly=TRUE))


### plot parameters

par(new=TRUE)  # allow new plot on same chart
par(las=1)  # set text printing to "horizontal"


### startup package loading

library(zoo)
# good package loading script inside functions
stopifnot("package:xts" %in% search() || require("xts", quietly=TRUE))



##############################
# expressions data structures
##############################

############## hw
# 1. (10pts) 
# Summary: Write code for performing operations in the R workspace, 
# change options settings and display them.

# remove all objects in the workspace:
rm(list=ls())

# set the maximum number of items printed to console equal to 80:
# you can use function options(),
options(max.print=80)

# show the max number of rows printed to console:
# you can use function options(),
options("max.print")

# set the number of digits printed for numeric values equal to 3:
# you can use function options(),
options(digits=3)

# show the number of digits printed to console for numeric values:
# you can use function options(),
options("digits")

# display today's date and time in the format: 
# "Today is April 05, 2016 at 12:38:36"
paste("Today is", format(Sys.time(), "%B %d, %Y at %H:%M:%S"))

# Create objects called var1, var2, var3, 
# and assign the values rnorm(1) to them: 
var1 <- rnorm(1)
var2 <- rnorm(1)
var3 <- rnorm(1)

# list all objects with names starting with "v". 
# hint: you can use function glob2rx() and function 
# ls() with the "pattern" argument,
ls(pattern=glob2rx("v*"))

# save all objects with names ending with "1" to a file 
# called "vobjects.RData" in your cwd. 
# hint: you can use function save() with the "list" argument,
save(list=ls(pattern=glob2rx("*1")), file="my_data.RData")

# remove all objects with names starting with "v": 
rm(list=ls(pattern=glob2rx("v*")))



############## test
# 1. (10pts) Create objects called var1, var2, var3, 
# and assign the values rnorm(1) to them. 
# You must use a for() loop, instead of individual assignments. 
# hint: you can use functions paste0() and assign(),

for (new_var in paste0("var", 1:3))
  assign(x=new_var, rnorm(1))



############## test - already in slides
# 1. (20pts) Create the character string "y = x1 + x2 - x3 - x4" from 
# characters "x", "y", "=", "+", "-", and the vectors 1:2 and 3:4, using 
# the functions paste0() and paste() with a collapse string,
# You can also use these characters with spaces around them, say " x ",
# hint: you must call paste0() and paste() several times and nest them, 

paste("y", "=", 
      paste(
        paste(paste0("x", 1:2), collapse=" + "), 
        paste(paste0("x", 3:4), collapse=" - "), 
      sep=" - "))



############## test - too simple
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



############## test
# 1. (15pts) Create a vector of permutations of integers from 1 to 90, 
# and call it vec_tor, 
# extract every third element of vec_tor, starting with the first one, 
# into a single column matrix called mat_rix,
# hint: you can use functions sample(), seq() and function matrix(), 
# with the proper "byrow" argument,

vec_tor <- sample(1:90)
mat_rix <- matrix(vec_tor[seq(from=1, to=length(vec_tor), by=3)], ncol=1)
# or:
mat_rix <- matrix(vec_tor, ncol=3, byrow=TRUE)
mat_rix <- mat_rix[, 1, drop=FALSE]

# extract a vector of odd index elements of vec_tor (first, third, etc), 
# and a vector of even index elements (second, fourth, etc.),
# calculate the scalar ("inner") product of the two vectors,
# the value should be a vector with a single element, not a matrix,
# hint: you can use the "%*%" operator, and the functions seq(), drop(), 
# or function matrix(), with the proper "byrow" argument,

odd_elements <- vec_tor[seq(from=1, to=length(vec_tor), by=2)]
even_elements <- vec_tor[seq(from=2, to=length(vec_tor), by=2)]
drop(odd_elements %*% even_elements)
# or:
mat_rix <- matrix(vec_tor, ncol=2, byrow=TRUE)
drop(mat_rix[, 1] %*% mat_rix[, 2])



############## test
# 1. (15pts) Create a matrix called mat_rix, as follows:

mat_rix <- matrix(1:6, ncol=3)

# assign to mat_rix row names "row1", "row2", and 
# column names "col1", "col2", "col3",
# You can use the functions rownames(), colnames(), 
# and/or dimnames(), 

dimnames(mat_rix) <- list(rows=c("row1", "row2"),
                          columns=c("col1", "col2", "col3"))

# change the names of the rows to "first_row" and "second_row",
# use the function dimnames(), but not the function rownames(),

dimnames(mat_rix)[[1]] <- c("first_row", "second_row")
dimnames(mat_rix)$rows <- c("first_row", "second_row")



############## test
# Summary: Multiply the columns of a matrix by the 
# elements of a vector. 

# Create a vector and a matrix as follows:

vec_tor <- c(2, 1, 3)
set.seed(1121)
mat_rix <- matrix(sample(1:12), ncol=3)

# 1. (20pts) Multiply mat_rix by vec_tor, so that the 
# first column of mat_rix is multiplied by the first 
# element of vec_tor, the second column by the second 
# element, etc. 
# Call this product mult_matrix. 
# mult_matrix should have the same dimensions as mat_rix. 
# You can use the function t(), and the "*" operator. 
# hint: use the function t() twice. 

mult_matrix <- t(t(mat_rix)*vec_tor)

# You should get the following result:
# > mult_matrix
#      [,1] [,2] [,3]
# [1,]   14    5   33
# [2,]    8   10    3
# [3,]   24    6   24
# [4,]    6    9    6

# Calculate the row sums of mult_matrix. 
# You can use the function rowSums(), 

rowSums(mult_matrix)

# You should get the following result:
# [1] 52 21 54 21

# Calculate the inner product of mat_rix and vec_tor. 
# You can use the "%*%" operator, 

mat_rix %*% vec_tor

# You should get the following result:
#      [,1]
# [1,]   52
# [2,]   21
# [3,]   54
# [4,]   21



############## test
# Summary: Perform loops over vectors, and then perform 
# the equivalent vectorized operations over the vectors.

# First create a vector of random numbers as follows: 

set.seed(1121)
vec_tor <- rnorm(10)


# 1. (20pts) Perform a for() loop to replace those elements 
# of vec_tor that are greater than "1" with the number "5". 
# You can use functions for() and seq_along(), 

for(in_dex in seq_along(vec_tor)) {
  if (vec_tor[in_dex]>1)
    vec_tor[in_dex] <- 5
}  # end for

# vec_tor should be like this:
# [1]  0.1449583  0.4383221  0.1531912  5.0000000  5.0000000 -0.8118832
# [7]  0.1602680  0.5858923  0.3600880 -0.0253084


# 2. (20pts) Perform an sapply() loop over vec_tor, and 
# perform exactly the same calculations as in p.1. 
# You must use either functions apply() lapply(), or sapply(). 
# You can also use functions length() and seq_along(), 
# and an anonymous function.

set.seed(1121)
vec_tor <- rnorm(10)
vec_tor <- sapply(vec_tor, 
                  function(ele_ment) {
                    if (ele_ment>1)
                      5
                    else
                      ele_ment
                  })  # end sapply
# or
vec_tor <- sapply(seq_along(vec_tor), 
                  function(in_dex) {
                    if (vec_tor[in_dex]>1)
                      vec_tor[in_dex] <- 5
                    else
                      vec_tor[in_dex]
                  })  # end sapply


# 3. (20pts) Perform the same calculations as in p.1, 
# but only using vectorized operations (logical 
# operators and subsetting). 
# You cannot use any for() or apply() loops. 

set.seed(1121)
vec_tor <- rnorm(10)
vec_tor[vec_tor>1] <- 5


# 4. (20pts) Perform the same calculations as in p.1, 
# but using function ifelse(). 
# You cannot use any for() or apply() loops. 
# You must use function ifelse(). 

set.seed(1121)
vec_tor <- rnorm(10)
vec_tor <- ifelse(vec_tor>1, 5, vec_tor)


# 5. (10pts) Benchmark the CPU time used by the code 
# from p.2 with the code from p.4, using the function 
# microbenchmark(). 
# Assign the names "s_apply" and "if_else" to each method. 

library(microbenchmark)
summary(microbenchmark(
  s_apply=sapply(seq_along(vec_tor), function(in_dex) {
    if (vec_tor[in_dex]>1)
      vec_tor[in_dex] <- 5
    else
      vec_tor[in_dex]
  }),  # end sapply
  if_else=ifelse(vec_tor>1, 5, vec_tor), 
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary



############## hw
# Summary: create a function called which_true(), which calculates 
# the indices of the TRUE elements of a boolean vector. 
# which_true() should produce the same result as function which(), 
# when applied to boolean vectors. 
# Implement which_true() using two different methods. 

# 1. (20pts) First method: you must perform a for() loop. 
# hint: you can first create an empty integer vector, 
# and then perform a for() loop to populate it with the 
# index values. 
# you can use functions integer(), seq_along(), c(). 

which_true <- function(vec_tor){
  in_dex <- integer()
  for (i in seq_along(vec_tor)) {
    if (vec_tor[i])
      in_dex <- c(in_dex, i)
  }  # end for
  in_dex
}  # end which_true

# or a more complicated method:
which_true <- function(vec_tor){
  in_dex <- integer()
  j <- 1
  for (i in seq_along(vec_tor)) {
    if (vec_tor[i]) {
      in_dex[j] <- i
      j <- j + 1
    }  # end if
  }  # end for
  in_dex
}  # end which_true


# 2. (20pts) Second method: you cannot perform any type 
# of loop, only vectorized functions, 
# hint: you can use functions length() or seq_along(), 
# and then apply vector subsetting,

which_true <- function(vec_tor) (seq_along(vec_tor))[vec_tor]
# or
which_true <- function(vec_tor) (1:length(vec_tor))[vec_tor]

# apply the function which_true() to a boolean vector, and 
# compare the result with using function which(), to verify 
# that it works correctly:

set.seed(1121)
vec_tor <- sample(1:20, replace=TRUE)
which_true(vec_tor==18)
which(vec_tor==18)



############## test - similar to hw
# 1. (20pts) Calculate the row and column index (number) 
# containing the specified value of a matrix, 
# without using which(),

# first, create a matrix,
set.seed(1121)
mat_rix <- matrix(sample(x=1:9), ncol=3)
# select a value from the matrix
set.seed(1121)
val_ue <- sample(x=mat_rix, size=1)

# hint: you can first calculate a boolean matrix,
# then use logical operators and subsetting, 
# and functions apply(), as.logical(), seq_along(), 
bool_matrix <- (mat_rix==val_ue)

# first find the column index,
col_umn <- as.logical(apply(bool_matrix, 2, sum))
col_umn <- (seq_along(mat_rix[1, ]))[col_umn]
# then find the row index,
in_dex <- (val_ue==mat_rix[, col_umn])
r_ow <- (seq_along(in_dex))[in_dex]



############## deprecated ##############
############## test - already in slides?
# Summary: extract and filter the elements of a matrix. 

# 1. (5pts) create a numeric vector of length 15 containing 
# random normal variates (rnorm), 
# coerce the vector into a matrix of 5 rows and 3 columns,
# call the matrix mat_rix, 
# you can use functions matrix() and dim(), 

mat_rix <- matrix(rnorm(15), ncol=3)
# or
mat_rix <- rnorm(15)
dim(mat_rix) <- c(5, 3)

# 2. (5pts) extract all the elements of mat_rix that are greater than 1.0,

mat_rix[mat_rix>1.0]

# 3. (5pts) calculate the row and column indices of all the elements 
# of mat_rix that are greater than 1.0,
# you can use function which() with argument "arr.ind", 

which(mat_rix>1.0, arr.ind=TRUE)

# 4. (5pts) calculate the sums of all the rows and columns, 
# the result should be two vectors, 
# you can use functions apply() and sum(), 

row_sums <- apply(mat_rix, 1, sum)
col_sums <- apply(mat_rix, 2, sum)

# 5. (5pts) bind the vectors of sums to mat_rix, as extra 
# rows and columns, respectively, 
# you can use functions cbind() and rbind(), 

mat_rix <- cbind(c(sum(row_sums), row_sums), 
                 rbind(col_sums, mat_rix))

# 6. (15pts) assign names to the rows and columns follows: 
# "col_sums", "row1", "row2", etc., 
# and 
# "row_sums", "col1", "col2", etc., 
# you can use functions c(), dimnames(), list(), nrow(), ncol(), 
# and paste0(), 

dimnames(mat_rix) <- list(c("col_sums", paste0("row", 1:(nrow(mat_rix)-1))), 
                          c("row_sums", paste0("col", 1:(ncol(mat_rix)-1))))
############## end deprecated ##############



############## hw
# 2. (15pts) create a numeric vector of length 10 containing random normal 
# variates, using rnorm(),
vec_tor <- rnorm(10)

# assign the names: "el1, ..., el10", to the vector elements, 
# you can use function paste(), with the proper "sep" argument, 
# you can't use function c()

names(vec_tor) <- paste("el", 1:10, sep="")

# change the vector names to: "num1, ..., num10", 
# you must create new names from the existing vector names, 
# you must use function gsub(), 
# (you can't use functions c() or paste()). 

names(vec_tor) <- gsub("el", "num", names(vec_tor))

# extract the element named "num4",

vec_tor["num4"]

# change the vector names to: "my.num1, ..., my.num10", 
# you must create new names from the existing vector names, 
# you can use the function paste(), with the proper "sep" argument, 

names(vec_tor) <- paste("my", names(vec_tor), sep=".")

# change the first vector element name back to: "num1", 
# using the function strsplit(), with the proper "split" argument,
# hint: strsplit() returns a list, subset it using [[1]],

names(vec_tor)[1] <- strsplit(names(vec_tor)[1], split="[.]")[[1]][2]
# or:
names(vec_tor)[1] <- strsplit(names(vec_tor)[1], split=".", fixed=TRUE)[[1]][2]

# calculate the indices of the elements that are greater than 
# or equal to -0.5 and less than 1.0,

which((vec_tor >= -0.5) & (vec_tor < 1.0))

# extract the elements that are greater than or equal to -0.5 and less than 1.0,
vec_tor[(vec_tor >= -0.5) & (vec_tor < 1.0)]

# calculate the mean and standard deviation of the vector elements,

mean(vec_tor)
sd(vec_tor)

# combine this vector with the vector "31:33",
c(vec_tor, 31:33)



############## test - too easy
# 3. (5pts) Create a matrix of 100 random normal elements, with 4 columns and 25 rows.
mat_rix <- matrix(rnorm(100), ncol=4)

# 4. (5pts) Calculate column means using the function apply(), and omit the NA values.
# hint: pass the parameter "na.rm=TRUE" to function mean().
apply(mat_rix, 2, mean, na.rm=TRUE)

# 3. (10pts) find all the elements of the matrix that are greater than 1.0, 
mat_rix[mat_rix > 1]



############## deprecated ##############
# comment: this part is already in lecture notes "The lapply() Functional"
############## test
# 1. (15pts) Calculate a vector of means of the numeric columns 
# of the "iris" data frame,
# calculate the means of only those columns that are numeric,
# the output must be a vector, not a list,
# you can use functions lapply(), sapply(), apply(), is.numeric(), 
# unlist(), and mean(), and an anonymous function.

unlist(sapply(iris, function(col_umn) {if (is.numeric(col_umn)) mean(col_umn)}))
# or
sapply(iris[, sapply(iris, is.numeric)], mean)


# 2. (5pts) Calculate the column means of the "setosa" species in the "iris" data.
# You can find all the iris species using the unique function.
unique(iris$Species)
# hint: first create a data frame of the "setosa" species, which is a subset of the "iris" data frame.
# hint: Then apply the expression from point #1 on the data frame of the "setosa" species.
iris_setosa <- iris[iris$Species=="setosa", ]
unlist(sapply(iris_setosa, function(col_umn) {if (is.numeric(col_umn)) mean(col_umn)}))
############## end deprecated ##############



############## hw
# Summary: Read a data frame containing student homework and test 
# scores, and assign letter grades. 

############## deprecated ##############
# 1. (30pts) Create a data frame containing the homework 
# and test scores for 20 students, and call the data 
# frame student_scores, 
# 
# First create a vector of strings containing student names, 
# called "student_names": 

student_names <- c("Chloe", "Olivia", "Madison", "Ethan", "James", "Amelia", 
                   "Anthony", "Joseph", "Evelyn", "Matthew", "Michael", "Liam", 
                   "Allison", "Mason", "Emma", "Jayden", "Emily", "William", 
                   "Ella", "Elizabeth")

# 1. (30pts) Create a data frame containing the homework and test scores 
# create a factor variable called "tra_ck" of length(student_names),
# containing values sampled from the following vector of strings:
# "Corporate", "Computational", "InfoTech", "Risk",
# Use functions sample() and as.factor(), 
tra_ck <- as.factor(sample(
  x=c("Corporate", "Computational", "InfoTech", "Risk"),
  size=length(student_names),
  replace=TRUE))

# Create the data frame student_scores from the vectors "student_names" and "tra_ck",
# Use function data.frame() with "stringsAsFactors=FALSE" to avoid coercing "character" to "factor",
student_scores <- data.frame(name=student_names, 
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

# cbind "sco_res" to the data frame student_scores,
student_scores <- cbind(student_scores, sco_res)
head(student_scores)

############## end deprecated ##############

### this is a version where students download student_scores:

# Download the file "student_scores.csv" from NYU Classes. 
# The file contains a data frame with student names, track, 
# and scores. 
# Read the file into a variable called student_scores using 
# read.csv(). 

student_scores <- read.csv(file="student_scores.csv", stringsAsFactors=FALSE)

# The data frame student_scores contains 8 columns: student names, 
# track, and six columns of numerical scores for homeworks and tests. 
# But some of the numeric columns contain NAs and characters, which 
# forces their coercion into factors.

# 1. (10pts) Perform an sapply() loop over the columns 3 to 8, 
# containing numerical scores and coerce them to numeric. 
# You can use functions sapply() and as.numeric(), 

student_scores[, -(1:2)] <- sapply(student_scores[, -(1:2)], as.numeric)

# Extract the "class" of the columns of student_scores using 
# functions class() and sapply(), and verify that the columns with 
# scores are numeric. 

sapply(student_scores, class)

# Now the columns containing numerical scores should all be class 
# "numeric", with some NAs in them, representing scores that are 
# not available. 


# 2. (20pts) Calculate a vector called "num_nas" containing the 
# number of NA scores for each student, and cbind() it to 
# student_scores as the last (9th) column,
# You can use functions apply(), cbind(), sum(), is.na(), 
# and an anonymous function, 

num_nas <- apply(student_scores[, -(1:2)], MARGIN=1, 
                 function(row) sum(is.na(row))
                 )  # end apply

student_scores <- cbind(student_scores, num_nas)

# Sort student_scores in descending order by column "num_nas", 
# use function order()

student_scores <- student_scores[order(student_scores$num_nas, decreasing=TRUE), ]


# 3. (10pts) Calculate a vector called avg_score 
# containing the average score of each student, and 
# bind it to student_scores as the last (10th) column. 
# Remember to omit NA values. 
# You can use functions apply(), cbind(), and mean(). 
# You cannot use an anonymous function. 

avg_score <- apply(student_scores[, 3:8], MARGIN=1, mean, na.rm=TRUE)

student_scores <- cbind(student_scores, avg_score)


# 4. (20pts) Assign letter grades to each student, based on 
# their avg_score column. 
# First calculate a histogram of avg_score values, using the 
# function hist(), with the Freedman-Diaconis rule for 
# determining the breakpoints. 
# The function hist() invisibly returns a list that includes 
# a vector of breakpoints called "breaks". 
# Assign the return value of function hist() to a list 
# called student_hist

student_hist <- hist(student_scores$avg_score, col="lightblue1", 
                     main="Student scores", xlab="student scores", 
                     breaks="FD")

# Calculate a vector of letter grades corresponding to 
# the avg_score values, using the vector of breaks: 
#   student_hist$breaks
# and call it letter_grade. 
# You must use function findInterval(), 

letter_grade <- findInterval(x=student_scores[, "avg_score"], 
                              vec=student_hist$breaks)

# letter_grade is an integer vector from 1 to 5, 
# with 1 corresponding to the highest avg_score 
# category, and 5 corresponding to the lowest category. 
# Convert letter_grade to a vector of strings 
# representing letter grades. 
# Use the following vector of strings called grade_s: 

grade_s <- c("A", "B", "C", "D", "F")

# Convert the letter_grade to a vector of strings, 
# by assigning 1 to letter grade "A", 2 to letter 
# grade "B", etc.

letter_grade <- grade_s[max(letter_grade) - letter_grade + 1]

# cbind() letter_grade to the data frame student_scores as 
# the last column. 

student_scores <- cbind(student_scores, letter_grade)
head(student_scores)


# 5. (15pts) Plot a histogram of the number of students 
# in each "letter_grade" category. 
# You can use the lecture slide titled "Cars93 Data Frame". 
# 
# There are at least two ways of doing this, but you need 
# to do it only one way.
# 
# In the first method you can use column "avg_score" 
# and function hist(), and the vector student_hist$breaks. 

hist(student_scores$avg_score, breaks=student_hist$breaks)

# In the second method you can use column "letter_grade", 
# and functions table() and barplot(). 

cont_table <- table(student_scores$letter_grade)
cont_table <- cont_table[order(names(cont_table), decreasing=TRUE)]
barplot(cont_table)


# this part is in case student_scores have to be loaded from student_scores.RData
############## hw
# Summary: Perform tapply() and sapply() loops to aggregate 
# student scores. 

# Download the file student_scores.RData from NYU Classes,
# and load() it.
# student_scores.RData contains the data frame student_scores.

load(file="C:/Develop/data/student_scores.RData")


# 6. (30pts) Calculate the average scores for students 
# in each finance_track category (the average of the 
# "avg_score" column), using the split-apply-combine 
# procedure. 
# You must perform the calculation in two different ways. 
# In the first method you must use functions with(), 
# tapply(), and mean(). 

with(student_scores, 
     tapply(avg_score, finance_track, mean)
)  # end with

# You should get the following output:
# Computational     Corporate      InfoTech       Risk 
#     25.60833      28.53333      20.20000      29.66250

# In the second method you must use functions with(), 
# sapply(), levels(), and mean().  You cannot use tapply(). 
# hint: use argument "USE.NAMES=TRUE" in sapply(). 

with(student_scores, 
     sapply(levels(finance_track), function(x) {
       mean(avg_score[x==finance_track])
     }, USE.NAMES=TRUE))  # end with


# 7. (20pts) Calculate the highest avg_scores in each 
# finance_track category, and call it high_scores. 
# You can use functions with(), tapply(), and max(). 
# Or you can use functions with(), sapply(), 
# levels(), and max().

high_scores <- with(student_scores, 
                    tapply(avg_score, finance_track, max))

# or: 
high_scores <- with(student_scores, 
               sapply(levels(finance_track), function(x) {
                 max(avg_score[x==finance_track])
               }, USE.NAMES=TRUE))  # end with


# 8. (20pts) Find the names of the students with the 
# highest avg_score in each finance_track category. 
# You must perform the calculation in two different ways. 
# In the first method you must use high_scores and 
# the function match(). 
# hint: use the "name" column of student_scores. 

student_scores$name[match(high_scores, student_scores$avg_score)]

# You should get the following output:
# [1] William   Jayden    Elizabeth Amelia

# In the second method you must use functions with() 
# and sapply(). 

with(student_scores, 
     sapply(high_scores, function(x) {
       name[x==avg_score]
     })
     )  # end with


# 9. (10pts) Sort student_scores by avg_score column, 
# first in descending order, then in ascending order,
# use function order()

student_scores <- student_scores[order(student_scores$avg_score), ]
head(student_scores)
student_scores <- student_scores[order(student_scores$avg_score, decreasing=TRUE), ]
head(student_scores)

# Save student_scores to a comma-delimited CSV file. 
# You must use function write.csv(). 

write.csv(student_scores, row.names=FALSE, 
          file="student_scores.csv")



############## deprecated ##############

# Assign letter grades to each student, based on their avg_score column, 
# use the following table:
# "A" if avg_score >= 50.0
# "A-" if avg_score >= 47.5
# "B+" if avg_score >= 45.0
# "B" if avg_score >= 42.5
# "B-" if avg_score >= 40.0
# "C+" if avg_score >= 37.5
# "C" if avg_score >= 35.0

# breakpoints correspond to categories of the data,
# the first breakpoint should correspond to the lowest category,
# and should have a value less than any of the data,
# Create a named numeric vector of breakpoints for avg_score, 
# called brea_ks as follows: 

brea_ks <- seq(from=35, to=50.0, by=2.5)
names(brea_ks) <- c("C", "C+", "B-", "B", "B+", "A-", "A")


# Create a factor variable containing letter grades, called letter_grade, 
# There are at least two ways of doing this, but you only need to do it one way,
# 
# In the first method you can use the names of brea_ks, and either 
# a for() loop, and/or if() and else(), and/or logical operators "<", ">", etc., 

# first create vector letter_grade containing empty strings:
letter_grade <- character(20)
# next populate letter_grade with letter grades using a for() loop:
# perform for() loop over breaks, which is shorter than number of students
for (brea_k in seq_along(brea_ks)) {
# create boolean vector indicating if student score is greater than break score
  in_dex <- (student_scores[, avg_score] >= brea_ks[brea_k])
# assign break score to students
  letter_grade[in_dex] <- names(brea_ks[brea_k])
}  # end for
letter_grade <- as.factor(letter_grade)

# In the second method you can use function findInterval() 
# and the names of brea_ks,

letter_grade <- names(brea_ks[findInterval(x=student_scores[, avg_score], 
                                            vec=brea_ks)])
letter_grade <- as.factor(letter_grade)


############## end deprecated ##############



############## hw
# Summary: create a function called find_interval(),
# that reproduces the function findInterval(),

# 1. (30pts) 
# find_interval() should accept a vector argument called vec_tor, 
# containing numeric values, which should be classified into intervals,
# according to break_points, 
# should also accept a vector of breakpoints called "break_points", 
# which determines the intervals,
# find_interval() should return an integer vector of length equal 
# to vec_tor, specifying the intervals to which the numeric values 
# contained in vec_tor belong, 
# hint: you can perform a for() loop over break_points, 
# you can use functions integer(), length(), seq_along(), 
# logical operators, and a for() loop

find_interval <- function(vec_tor, break_points) {
# first create an empty vector "intervals",
  intervals <- integer(length(vec_tor))
# next populate "intervals" with integer values corresponding to intervals,
# perform for() loop over break_points, 
  for (break_point in seq_along(break_points)) {
# "in_dex" is boolean vector indicating if element is greater than break value,
    in_dex <- (vec_tor >= break_points[break_point])
# assign integer value corresponding to interval,
    intervals[in_dex] <- break_point
  }  # end for
  intervals
}  # end find_interval

# call find_interval() on a vector and compare it to findInterval(), 
# to verify that it works correctly,

find_interval(vec_tor=1:8, break_points=c(3, 5, 7, 9))
findInterval(x=1:8, vec=c(3, 5, 7, 9))

# call find_interval() on a vector of numbers and a vector of breakpoints, 
# to verify that it produces exactly the same output as findInterval(), 
# use function identical(),

identical(
  find_interval(vec_tor=1:8, break_points=c(3, 5, 7, 9)),
  findInterval(x=1:8, vec=c(3, 5, 7, 9))
)

# benchmark the speed of findInterval() versus find_interval(), 
# using the function microbenchmark(),
# use a vector of data equal to rnorm(1000), 
# and breakpoints equal to c(-4, -2, 0, 2, 4), 

foo <- rnorm(1000)
break_points <- c(-4, -2, 0, 2, 4)
library(microbenchmark)
summary(microbenchmark(
  findInterval=
    findInterval(x=foo, vec=break_points),
  find_interval=
    find_interval(vec_tor=foo, break_points=break_points),
  times=10))[, c(1, 4, 5)]



############## test - simple adaptation of lecture slides from data_structures
# Summary: Create a function that calculates a contingency 
# table for a single vector or factor, similar to function 
# table(). 
# 
# 1. (20pts) Create a function that calculates a contingency 
# table, called ta_ble(). 
# ta_ble() should accept a single argument, either a vector 
# or a factor, and return a named vector containing the 
# number of times an element occurs in the input argument.  
# The names of the output vector should be the elements of 
# the input argument. 
# The order of the output vector elements is not important. 
# You can't use function table(), 
# You can use functions sapply(), unique(), sum(), and an 
# anonymous function.

ta_ble <- function(in_put) {
  sapply(unique(in_put), 
         function(le_vel) {
           sum(in_put==le_vel)
         }) # end sapply
}  # end ta_ble

# Call ta_ble() as follows, and compare its output to that 
# of table(), to verify that they produce similar output:

vec_tor <- sample(c("a", "b", "c", "d"), 
                  size=20, replace=TRUE)
ta_ble(vec_tor)
table(vec_tor)



############## hw
# Summary: Perform aggregations over data frame columns, 
# given intervals defined by breakpoints. 

# 1. (10pts) calculate a vector containing the number 
# of NAs in each column of the airquality data frame. 
# You can use functions sapply(), is.na(), sum(), 
# and an anonymous function. 

sapply(airquality, function(col_umn) sum(is.na(col_umn)))

# You should get the following result:
#   Ozone Solar.R    Wind    Temp   Month     Day 
#     37       7       0       0       0       0


# Create a data frame called good_air, by subsetting 
# the "Temp" and "Solar.R" columns of the airquality data 
# frame, and then removing any rows containing NAs. 
# You can use function complete.cases(). 

good_air <- airquality[, c("Temp", "Solar.R")]
good_air <- good_air[complete.cases(good_air), ]

# You should get a data frame with the following dimensions:
# dim(good_air)
# [1] 146   2


# 2. (10pts) Calculate a vector of breakpoints based on 
# the "Temp" column of good_air. 
# You can use function hist() with argument "plot=FALSE". 
# The function hist() returns a structure (list) with an 
# element called "breaks", which are the breakpoints. 

hist_air <- hist(good_air$Temp, plot=FALSE)
hist_air$breaks

# You should get the following vector of breakpoints:
# [1]  55  60  65  70  75  80  85  90  95 100


# 3. (10pts) Calculate a vector of categorical data 
# from the "Temp" column of good_air, using the 
# breakpoints from p.2.  Add the categorical vector 
# as a column to good_air called "categ".
# The categorical column shows to which category 
# of "Temp" each row of good_air belongs. 
# You can use the function findInterval(). 

good_air$categ <- findInterval(x=good_air$Temp, vec=hist_air$breaks)

# You should get the following output:
# head(good_air)
#   Temp Solar.R categ
# 1   67     190     3
# 2   72     118     4
# 3   74     149     4
# 4   62     313     2
# 7   65     299     3
# 8   59      99     1


# 4. (20pts) Perform a split-apply-combine procedure on 
# the "Solar.R" column of good_air, using the categorical 
# column of good_air from p.3.  
# You can use the function aggregate() with argument 
# "formula". 
# Pass the function range() to aggregate(), so that it's 
# applied to the "Solar.R" column of good_air. 
# Assign the data frame returned by function aggregate() 
# to a variable called "ran_ge". 

ran_ge <- aggregate(formula=(Solar.R ~ categ), 
                    data=good_air, FUN=range)

# Add the breakpoints from p.2. as a column to ran_ge 
# called "temp".

ran_ge$temp <- hist_air$breaks[ran_ge$categ]

# The end result of the split-apply-combine procedure 
# should be a data frame as follows:
# ran_ge
#   categ Solar.R.1 Solar.R.2 temp
# 1     1         8       266   55
# 2     2        19       334   60
# 3     3        13       322   65
# 4     4         7       320   70
# 5     5        27       322   75
# 6     6        24       332   80
# 7     7        82       323   85
# 8     8       167       291   90
# 9     9       203       237   95

# Plot the difference between the two "Solar.R" columns 
# of ran_ge versus "temp", to display how the variability 
# of "Solar.R" depends on "temp". 
# You can use the functions with() and plot(). 

with(ran_ge, plot(temp, Solar.R[, 2]-Solar.R[, 1], t="l", 
                  xlab="temp", ylab="Solar range"))



# comment: some of the below is already in lecture notes "Tables of Categorical Data"
# it's also in the test for function ta_ble()
############## hw
# Summary: create a function that calculates a contingency table 

# 1. (5pts) Calculate the vector of "class" attributes of the 
# columns of the data frame "Cars93", and call it "class_cars",
# you can use functions class() and sapply(), 

library(MASS)
class_cars <- sapply(Cars93, class)

# calculate the vector of unique "class" attributes (elements) 
# in "class_cars",
# you can use function unique(), 

unique(class_cars)

# 2. (10pts) calculate the number of columns with each 
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


# 2. (20pts) calculate a list containing the column names in each 
# unique "class", that is, calculate the names of the columns 
# of class "factor", the names of the columns of class "numeric", etc.
# you can use functions lapply(), sapply(), colnames(), 
# and an anonymous function,

lapply(unique(class_cars), function(unique_class) {
  colnames(Cars93)[class_cars==unique_class]
})  # end lapply
# or to preserve list element names:
sapply(unique(class_cars), function(unique_class) {
  colnames(Cars93)[class_cars==unique_class]
}, simplify=FALSE, USE.NAMES=TRUE)  # end sapply

# 3. (20pts) calculate the "means" of columns that are 
# either of class "integer" or "numeric",
# you can use functions sapply(), mean(), and the "%in%" operator,
# some elements of Cars93 are NA, so you will need to pass the 
# parameter "na.rm=TRUE" to mean(),

integer_or_numeric <- class_cars %in% c("numeric", "integer")
sapply(Cars93[, integer_or_numeric], mean, na.rm=TRUE)



############## test
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



############## test
# Summary: Use function which() to extract elements of 
# data frames. 
# Perform an sapply() loop over unique elements of 
# data frames, and extract its elements. 

# 1. (10pts) Select a subset of the mtcars data frame, 
# that contains only cars with 6 cylinders, and call 
# it mtcars_6cyl. 

mtcars_6cyl <- mtcars[mtcars$cyl==6, ]


# 2. (10pts) Select the row of mtcars_6cyl that 
# contains the car with the highest horsepower, 
# and call it best_car.
# hint: you can use function which.max(). 

best_car <- mtcars_6cyl[which.max(mtcars_6cyl$hp), ]

# Print the name of the car with the highest horsepower: 

rownames(best_car)

# You should get the following result:
# [1] "Ferrari Dino"

# Print the horsepower and the weight of that car:

best_car$hp
best_car$wt


# 3. (30pts) Calculate a named vector of names of 
# cars with the highest horsepower in each cylinder 
# category. 
# You can use functions unique, which.max(), 
# structure(), sapply(), and an anonymous function. 

sapply(unique(mtcars$cyl), function(cyl) {
  mtcars_cyl <- mtcars[mtcars$cyl==cyl, ]
  structure(rownames(mtcars_cyl[which.max(mtcars_cyl$hp), ]), 
            names=paste(cyl, "cylinders"))
})  # end sapply

# You should get the following result:
#   6 cylinders     4 cylinders     8 cylinders 
# "Ferrari Dino"  "Lotus Europa" "Maserati Bora" 



############## hw
# Summary: create a functional which calculates the outer product 
# of a function over two vectors, that for vector arguments 
# is equivalent to outer(), 
# 
# 1. (5pts) create a functional called out_er() that accepts two vector 
# arguments "vec1", "vec2", and a function argument "func_tion",
# out_er() should return a matrix equal to the outer product 
# of "func_tion" over the vectors "vec1" and "vec2", 
# out_er() should also work for functions that aren't vectorized,
# 
# you can use functions for(), rep(), seq(), matrix(), 
# any of the apply() functions, and an anonymous function,
# you cannot use outer() or any other similar function from a package,
# you cannot copy the source code of outer(), but you may borrow from it,
# you must create two different versions of out_er(), with at least 
# one version that uses any of the apply() functions, 

# first version using for()

out_er <- function(vec1, vec2, func_tion) {
  mat_rix <- matrix(nrow=length(vec1), ncol=length(vec2))
  for (i in seq_along(vec1))
    for (j in seq_along(vec2))
      mat_rix[i,j] <- func_tion(vec1[i], vec2[j])
    mat_rix
}  # end out_er

# second version using sapply()

out_er <- function(vec1, vec2, func_tion) {
  sapply(X=vec2, FUN=function(in_put, vec_tor, func_tion) {
    sapply(X=vec_tor, FUN=func_tion, in_put)
  }, vec_tor=vec1, func_tion=func_tion)
}  # end out_er

# third version using mapply()

out_er <- function(vec1, vec2, func_tion) {
  matrix(mapply(func_tion, rep(vec1, length(vec2)), 
                rep(vec2, each=length(vec1))), 
         length(vec1), length(vec2))
}  # end out_er

# call out_er() on two vectors and a vectorized function, to verify 
# that it works correctly,
# verify that all the versions of out_er() produce a matrix exactly 
# equal to the one produced by outer(),
# use function identical(),

foo <- function(x1, x2) paste(x1, x2, sep="x")
identical(out_er(1:3, 5:7, foo), outer(1:3, 5:7, foo))



##############################
# statistis and probability
##############################


############## test
# 1. (5pts) create a matrix of 30rows x 10columns with random 
# normal variates, 
mat_rix <- matrix(rnorm(300), ncol=10)

# 2. (10pts) calculate the mean, standard deviation, skewness, 
# and kurtosis of the 5th row: 

da_ta <- mat_rix[5, ]
len_data <- length(da_ta)
mean_data <- mean(da_ta)
sd_data <- sd(da_ta)
skew_data <- len_data*sum(((da_ta - mean_data)/sd_data)^3)/((len_data-1)*(len_data-2))
kurtosis_data <- len_data*(len_data+1)*sum(((da_ta - mean_data)/sd_data)^4)/((len_data-1)^3)
c(mean_data, sd_data, skew_data, kurtosis_data)

# the 8th column,
da_ta <- mat_rix[, 8]
len_data <- length(da_ta)
mean_data <- mean(da_ta)
sd_data <- sd(da_ta)
skew_data <- len_data*sum(((da_ta - mean_data)/sd_data)^3)/((len_data-1)*(len_data-2))
kurtosis_data <- len_data*(len_data+1)*sum(((da_ta - mean_data)/sd_data)^4)/((len_data-1)^3)
c(mean_data, sd_data, skew_data, kurtosis_data)



############## hw
# Summary: Create a function called kur_tosis(), for calculating 
# the kurtosis of a time series of returns (a vector of data). 

# 1. (10pts) The function kur_tosis() should accept a single numeric 
# argument called da_ta. 
# The function kur_tosis() should verify that da_ta is numeric, and 
# if it's not, then it should produce a warning and return NULL. 
# If da_ta is numeric, then kur_tosis() should calculate the kurtosis 
# of da_ta and return it. 
# The argument da_ta should be assigned a default value equal to a 
# vector of 1000 random numbers taken from the standard normal 
# distribution. 
# You can use functions is.numeric(), warning(), paste(), return(), 
# length(), mean(), sd(), and sum(). 

kur_tosis <- function(da_ta=rnorm(1000)) {
  if (!is.numeric(da_ta)) {
    warning(paste("argument", da_ta, "isn't numeric"))
    return(NULL)
  }  # end if
  len_data <- length(da_ta)
  mean_data <- mean(da_ta)
  sd_data <- sd(da_ta)
  len_data*(len_data+1)*sum(((da_ta-mean_data)/sd_data)^4)/((len_data-1)^3)
}  # end kur_tosis


# 2. (10pts) Use the function kur_tosis() to calculate the kurtosis  
# of DAX returns from the dataset EuStockMarkets. 
# Next, calculate the kurtosis of a vector of normal random numbers, 
# of the same length as the DAX returns. 
# Next, calculate the kurtosis of a vector of random numbers taken 
# from the t-distribution with four degrees of freedom, of the same 
# length as the DAX returns. 
# You can use the functions rt(), rnorm(), and length():

ts_rets <- 100*diff(log(EuStockMarkets[, 1]))

# call kur_tosis() as follows, to verify that it works correctly: 
kur_tosis(da_ta="hello")
kur_tosis()

# calculate kurtosis of DAX returns:
kur_tosis(da_ta=ts_rets)

# calculate kurtosis of normal returns:
kur_tosis(da_ta=rnorm(n=length(ts_rets)))

# calculate kurtosis of t-distribution returns:
kur_tosis(da_ta=rt(n=length(ts_rets), df=4))



##############################
# functions
##############################

############## hw
# Summary: Perform sapply() loops over the parameters of 
# function rnorm(), both with, and without using an 
# anonymous function. 

# 1. (10pts) Define two named vectors of parameters called  
# me_ans and std_devs, containing mean and sd parameter values 
# to be passed to function rnorm(). 
# The values of mean and sd should be:
# mean <- -1:1
# sd <- 1:3
# your code should produce the following named vectors: 
# me_ans= 
#   mean=-1  mean=0  mean=1 
#        -1       0       1 
# std_devs= 
#   sd=1 sd=2 sd=3 
#      1    2    3 
# You can use functions paste0(), names(), and/or structure(), 

me_ans <- structure(-1:1, names=paste0("mean=", -1:1))
std_devs <- structure(1:3, names=paste0("sd=", 1:3))


# 2. (20pts) Perform an sapply() loop over the vector 
# me_ans, and pass its values to the mean parameter of 
# function rnorm(). 
# Pass the parameter "n=2" to rnorm(), so that each call 
# produces two random numbers. 
# You must use functions rnorm() and sapply(). 
# You must perform the calculation in two different ways: 
# the first way without using an anonymous function, and 
# the second way using an anonymous function.

set.seed(1121)

# first without using an anonymous function:
sapply(me_ans, rnorm, n=2, sd=1)

# second way using an anonymous function: 
sapply(me_ans, 
       function(me_an) 
         rnorm(n=2, mean=me_an))
# or
sapply(me_ans, 
       function(me_an, ...) 
         rnorm(mean=me_an, ...), n=2)

# your code should produce a matrix like this: 
#        mean=-1 mean=0 mean=1
#   [1,] -0.8550 0.1532 2.9995
#   [2,] -0.5617 1.0849 0.1881


# Perform an sapply() loop over the vector std_devs, and 
# pass its values to the sd parameter of function rnorm(). 
# Pass the parameter "n=2" to rnorm(), so that each call 
# produces two random numbers. 
# You must use functions rnorm() and sapply(). 
# You must perform the calculation in two different ways: 
# the first way without using an anonymous function, and 
# the second way using an anonymous function.

set.seed(1121)

# first without using an anonymous function:
sapply(std_devs, rnorm, n=2, mean=0)

# second way using an anonymous function: 
sapply(std_devs, 
       function(std_dev) 
         rnorm(n=2, sd=std_dev))
# or
sapply(std_devs, 
       function(std_dev, ...) 
         rnorm(sd=std_dev, ...), n=2)

# your code should produce a matrix like this: 
#        sd=1   sd=2  sd=3
# [1,] 0.1450 0.3064  5.999
# [2,] 0.4383 2.1699 -2.436


# 3. (20pts) Calculate a matrix of random numbers, by 
# performing two sapply() loops, first over the vector 
# me_ans, and a second loop over the vector std_devs, 
# and pass the values of me_ans and std_devs to the 
# mean and sd parameters of the function rnorm(). 
# Pass the parameter "n=1" to the function rnorm(). 
# You must use functions rnorm() and sapply(). 
# hint: you can use an anonymous function with two 
# arguments. The anonymous function should contain 
# the second sapply() loop. 

set.seed(1121)

sapply(me_ans, 
       function(me_an, std_devs) 
         sapply(std_devs, rnorm, n=1, mean=me_an), 
       std_devs=std_devs)

# your code should produce a matrix like this: 
#      mean=-1 mean=0 mean=1
# sd=1 -0.8550  1.085  1.160
# sd=2 -0.1234  3.999  2.172
# sd=3 -0.5404 -2.436  2.080

# deprecate
# 1. (20pts) Perform the same two sapply() loops as above, 
# but without using anonymous functions. 
# Each time reset the seed using set.seed() to be able to 
# compare the results, 

set.seed(1121)
sapply(me_ans, rnorm, n=2, sd=1)
set.seed(1121)
sapply(std_devs, rnorm, n=2, mean=0)

# run microbenchmark() to see which code is faster, 

library(microbenchmark)
summary(microbenchmark(
  anon={sapply(std_devs, function(sd) rnorm(n=2, sd=sd))}, 
  no_anon={sapply(std_devs, rnorm, n=2, mean=0)},
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# end deprecate



############## test - too simple?
# Summary: create a wrapper for function sum(), 
# 
# 1. (20pts) Create a wrapper for function sum(), called my_sum(). 
# The function my_sum() should be able to accept an indefinite number 
# of arguments, and pass them to the dots "..." argument of function sum().
# The function my_sum() should also accept an argument called "na.rm", 
# with default value TRUE, and pass its value to the "na.rm" argument of 
# function sum(). 

my_sum <- function(..., na.rm=TRUE) {
  sum(..., na.rm=na.rm)
}  # end my_sum

# call my_sum() as follows, to verify that it works correctly:

my_sum(1, 2, 3, NA)



############## test
# Summary: create functions with dots argument, 

# 1. (15pts) create a function called mult_dots(), 
# which takes dots "..." as its first argument, 
# and "fac_tor" as its second argument, as follows: mult_dots(..., fac_tor),
# The function mult_dots() should sum up the dots "..." argument, 
# then multiply the sum by "fac_tor", and return the result,
# you can use function sum(), 

mult_dots <- function(..., fac_tor) {
  fac_tor*sum(...)
}  # end mult_dots

# apply the function mult_dots() so that it adds the numbers "1, 2, 3", 
# and then multiplies the sum by "2",
mult_dots(1, 2, 3, fac_tor=2)



############## test
# 1. (15pts) create a function called get_dots(), 
# which takes two arguments: 
# "in_dex" as its first argument, and 
# dots "..." as its second argument, 
# as follows: get_dots(in_dex, ...),
# get_dots() should return the variable bound to the dots, 
# which has an index equal to "in_dex",
# for example, get_dots(in_dex=2, ...) should return the 
# second variable bound to the dots,
#  get_dots(in_dex=2, 5, 6, 7) should return "6",
#  get_dots(in_dex=3, 5, 6, 7) should return "7", etc.,
# get_dots() should not return a list with a single element, 
# get_dots() should check that "in_dex" isn't greater than 
# the number of variables bound to the dots, 
# if "in_dex" is greater than the number of variables bound 
# to the dots, then get_dots() should produce an error,
# you can use functions list(), length(),

get_dots <- function(..., in_dex) {
  dots <- list(...)
  stopifnot(in_dex <= length(dots))
  dots[[in_dex]]
}  # end get_dots

# apply the function get_dots() so that it returns 
# "5" from "7, 5, 6", 
get_dots(in_dex=2, 7, 5, 6)



############## test
# Summary: Define an aggregation function which accepts 
# a dots "..." argument. 
# Perform an apply() loop over the columns of a matrix, 
# and pass additional arguments through the dots "..." 
# argument to the aggregation function. 

# Create a matrix that contains NA values as follows:

set.seed(1121)
mat_rix <- matrix(sample(c(1:96, rep(NA, 4))), ncol=4)


# 1. (10pts) Assign the following column names to mat_rix: 
# "col1", "col2", etc. 
# You can use functions colnames(), ncol(), and paste0(). 
# You cannot use function c(). 

colnames(mat_rix) <- paste0("col", 1:ncol(mat_rix))


# 2. (10pts) The function agg_regate() calculates the 
# maximum and minimum values over a column of data: 

agg_regate <- function(col_umn) {
  c(max=max(col_umn), min=min(col_umn))
}  # end agg_regate

# Perform an apply() loop over the columns of mat_rix, 
# and apply agg_regate() to each column.

apply(mat_rix, 2, agg_regate)

# You should get the following result:
#     col1 col2 col3 col4
# max   96   NA   NA   NA
# min   19   NA   NA   NA


# 3. (20pts) Modify agg_regate() by adding a dots "..." 
# argument to it, and pass the dots to the functions 
# max() and min(). 

agg_regate <- function(col_umn, ...) {
  c(max=max(col_umn, ...), min=min(col_umn, ...))
}  # end agg_regate

# Perform an apply() loop over the columns of mat_rix, 
# and apply agg_regate() to each column.
# This time pass the argument "na.rm=TRUE" into max() 
# and min() through the dots "..." argument of the 
# apply() function. 

apply(mat_rix, 2, agg_regate, na.rm=TRUE)

# You should get the following result:
#     col1 col2 col3 col4
# max   96   91   94   95
# min   19    2    1    5



############## test
# Summary: perform an apply() loop, and pass values to a function
# through the dots "..." argument of the functional apply().

# define a single-column matrix called "da_ta" as follows:

da_ta <- matrix(1:4)

# define a function called "my_func" with three arguments 
# as follows:

my_func <- function(arg1, arg2, arg3) {
  c(arg1=arg1, arg2=arg2, arg3=arg3)
}  # end my_func

# 1. (20pts) Perform an apply() loop, and apply my_func() to "da_ta", 
# so that "da_ta" is passed to "arg2". 
# You must also pass the values "5" and "6" to "arg1" and "arg3", 
# using the dots "..." argument of the functional apply(). 

apply(X=da_ta, MARGIN=1, my_func, arg1=5, arg3=6)

# the output of appply() should be a matrix with three rows named,
# "arg1", "arg2", "arg3", like this:
#       [,1] [,2] [,3] [,4]
# arg1    5    5    5    5
# arg2    1    2    3    4
# arg3    6    6    6    6



############## hw
# 1. (30pts) Create a function called lag_it() that applies 
# a lag to vectors. 
# lag_it() should accept two arguments:
# - vec_tor a vector argument to which a lag should be applied, 
# - "lag" an integer, specifying the number of periods to lag. 
# - "lag" should have a default value of 1. 
# lag_it() should first check if "lag" is numeric, and if not 
# then it should produce a warning message and return NULL. 
# lag_it() should next check if vec_tor is a vector, and if not 
# then it should produce a warning message and return NULL. 
# If both these tests pass, then lag_it() should return a vector 
# of the same length as vec_tor, that is lagged by the number 
# of periods specified by "lag". 
# A positive "lag" should replace the present value with values 
# from the past, and a negative lag should replace with values 
# from the future. 
# lag_it() should add NA values in place of values that are missing. 
# for example, lag_it() should produce the following output:
#  lag_it(c(1:5), lag=2)
#  [1] NA NA  1  2  3
# 
#  lag_it(c(1:5), lag=-2)
#  [1]  3  4  5 NA NA
# 
# you can use functions is.vector(), is.numeric(), 
# length(), c(), rep(), warning(), and return(), 

lag_it <- function(vec_tor, lag=1) {
  if (!is.numeric(lag)) {  # lag is not numeric
    warning(paste("argument", deparse(substitute(lag)), "must be numeric."))
    return(NULL)  # return NULL
  }  # end if
  if (is.vector(vec_tor)) {  # vec_tor is a vector
    if(lag>0) {
      vec_tor <- c(rep(NA, lag), vec_tor)
      vec_tor[-((length(vec_tor)-lag+1):length(vec_tor))]
    } else {
      vec_tor <- c(vec_tor, rep(NA, -lag))
      vec_tor[-(1:(-lag))]
    }
  } else {  # vec_tor is not a vector
    warning(paste0("argument \"", deparse(substitute(vec_tor)), "\" must be a vector."))
    return(NULL)  # return NULL
  }  # end if
}  # end lag_it

# call lag_it() as below, to verify it works correctly,
lag_it(1:9)
lag_it(1:9, lag=2)
lag_it(1:9, lag=-1)
lag_it(1:9, lag=-2)
lag_it(matrix(1:9, ncol=1))
lag_it("a", "b")

# You should get the following results:
# > lag_it(1:9, lag=2)
# [1] NA NA  1  2  3  4  5  6  7
# > lag_it(1:9, lag=-1)
# [1]  2  3  4  5  6  7  8  9 NA


############## hw
# Summary: create functions that replicates vectors without 
# using the function rep(), 

# 1. (15pts) create a function called my_rep(), that replicates 
# a single input element (vector of length 1), a given number of times, 
# my_rep() should be equivalent to rep() for a single input element 
# (vector of length 1), 
# my_rep() should accept an argument called "in_put", 
# my_rep() should check if "in_put" is a vector, and if it's 
# of length 1, 
# if it's not a vector, then my_rep() should produce a warning()
# and return NULL, 
# if it's not of length 1, then it should be subset it to its 
# first element, i.e. it should replicate only the first element 
# of a vector, 
# my_rep() should also accept an integer called "n_rep", 
# which specifies the number of replications, 
# 
# you can use functions is.vector(), length(), vector(),
# hint: first allocate space for a vector using vector(),
# then perform vectorized assignment using brackets "[]", 
# you cannot use a for() loop, 
# do not "grow" vectors by adding new elements to them,

my_rep <- function(in_put, n_rep) {
  if (is.vector(in_put)) {
    if (length(in_put)>1)
      in_put <- in_put[1]
# create empty vector of same mode as "in_put"
    out_put <- vector(mode=mode(in_put), length=n_rep)
    out_put[] <- in_put
    out_put
  } else {
    warning("input isn't a vector")
    NULL
  }  # end if
}  # end my_rep

# call my_rep() on several different vectors to verify 
# that it works correctly,
# remember that a single object is a vector of length 1, 

my_rep(in_put="a", n_rep=3)  # output: "a" "a" "a"
my_rep(in_put=5:7, n_rep=3)  # output: 5 5 5
my_rep(in_put=matrix(1:10, ncol=2), n_rep=3)  # output: NULL Warning message:


# 2. (15pts) create a function called my_rep(), that replicates 
# vectors and lists a given number of times, 
# my_rep() should be equivalent to rep() for vector arguments, 
# my_rep() should accept a vector or list argument called "in_put", 
# my_rep() should accept an integer argument called "n_rep", 
#  specifying the number of replications, 
# my_rep() should also accept a string called "meth_od", 
#  specifying the replication method, with possible values 
#  "times", "each", "length.out", 
#  default should be "times",
# my_rep() should return a vector conating the replicated 
# elements of "in_put", 
# 
# hint: first allocate space for a vector using vector(),
# then perform vectorized assignment using brackets "[]", 
# you can use functions switch(), length(), vector(),
# you cannot use nested multiple for() loops, 
# you can only use one for() loop in each logical branch,
# do not "grow" vectors by adding new elements to them,

my_rep <- function(in_put, n_rep, meth_od=c("times", "each", "length.out")) {
  meth_od <- match.arg(meth_od)
# create empty vector of same mode as "in_put"
  out_put <- vector(mode=mode(in_put), length=n_rep*length(in_put))
  switch(EXPR=meth_od,
         "times"={
           for (in_dex in 1:n_rep)
             out_put[(length(in_put)*(in_dex-1)+1):(length(in_put)*in_dex)] <- in_put
         },
         "each"={
           for (in_dex in seq_along(in_put))
             out_put[(n_rep*(in_dex-1)+1):(n_rep*in_dex)] <- in_put[in_dex]
         },
         "length.out"={
           re_peat <- n_rep %/% length(in_put) + 1
           for (in_dex in 1:re_peat)
             out_put[(length(in_put)*(in_dex-1)+1):(length(in_put)*in_dex)] <- in_put
           out_put <- out_put[1:n_rep]
         }
  )  # end switch
  out_put
}  # end my_rep

# call my_rep() on several different vectors to verify that 
# it works correctly for all three possible values of "meth_od",
# verify that my_rep() produce a vector exactly equal to the 
# one produced by rep(),
# use function identical(),
my_rep(in_put=1:3, n_rep=3, meth_od="times")
identical(my_rep(in_put=1:3, n_rep=3), rep(1:3, 3))
my_rep(in_put=1:3, n_rep=3, meth_od="each")
my_rep(in_put=1:3, n_rep=5, meth_od="length.out")



############## test - very simple
# 1. (15pts) Create a function called match_matrix(), similar to match(), 
# but which accepts matrix arguments, as well as vectors. 
# match_matrix() should return the row and column indices of the first 
# element of its second argument, that matches its first argument,
# hint: you can use function which(), with the argument "arr.ind=TRUE", 

match_matrix <- function(val_ue, mat_rix) {
  which(mat_rix==val_ue, arr.ind=TRUE)
}  # end match_matrix

# call the function match_matrix() as follows, to make sure it works properly:

mat_rix <- matrix(1:6, ncol=3)
match_matrix(5, mat_rix)


############## deprecated ##############
# this is old - match_matrix() above is better
############## hw
# Create a function which calculates row and column containing the extreme value of a matrix.
# The extreme value is calculated by the function "func_tion", which can be "max" or "min", etc.
which_matrix <- function(mat_rix, func_tion="max") {
# validate function name
  func_tion <- match.fun(func_tion)
  which(mat_rix==func_tion(mat_rix), arr.ind=TRUE)
# tmp <- which(mat_rix==func_tion(mat_rix), arr.ind=T)
# coordinates <- as.numeric(c(rownames(mat_rix)[tmp[1,1]], colnames(mat_rix)[tmp[1,2]]))
# coordinates
}  # end which_matrix
############## end deprecated ##############



############## hw
# 1. (20pts) create a function called my_sqrt() which calculates 
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



############## hw
# Summary: Create function called read_numeric() that reads 
# numbers input by the user, and returns them in a vector. 
# read_numeric() should ask the user to input a number, and 
# should read the input using the function readline(). 
# read_numeric() should read numbers from the console in 
# a "while" loop. 
# read_numeric() should validate the inputs, and produce 
# errors and Warnings: 
# - if the user input is numeric, then read_numeric() should 
#   append the input to the numeric output vector,
# - if the input is not numeric, then read_numeric() should 
#   produce a Warning "input is not numeric!",
# - if the input is empty, then read_numeric() should 
#   terminate, and return the numeric output vector. 
# hint: read_numeric() should use readline(), and can also 
# use is.na(), nchar(), as.numeric(), length(), identical(), etc.
# read_numeric() should create a numeric vector consisting of 
# the input numbers, ignore it. 

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



############## hw
# Summary: create a function called re_move(), which removes 
# an element from an object,
# 
# 1. (20pts) Create a function called re_move(), which takes two arguments:
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



############## hw - already in slides?
# 1. (30pts) summary:
# benchmark the speed of cumulative sum calculations in the 
# slide titled "Vectorized Functions for Vector Computations", 
# using the function microbenchmark(),
# 
# create a vector of 1000 random normal numbers,

big_vector <- rnorm(1000)

# calculate the cumulative sum of "big_vector" using two 
# different methods:
# first using the function cumsum(), second using a for() loop, 
# 
# benchmark the speed of both calculations using the function 
# microbenchmark(),
# assign the names "cum_sum" and "for_loop" to each method, 

library(microbenchmark)
summary(microbenchmark(
  cum_sum=cumsum(big_vector), 
  for_loop={
    # allocate memory for cumulative sum
    cum_sum <- numeric(length(big_vector))
    cum_sum[1] <- big_vector[1]
    # cumulative sum using "for" loop
    for(i in 2:length(big_vector)) {
      cum_sum[i] <- cum_sum[i-1] + big_vector[i]
    }},  # end for
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary



############## hw
# Summary: create a functional called cumu_late(), 
# which calculates cumulative values (sums, products, etc.) 
# over a vector argument, 
# and reproduces the functions cumsum() and cumprod().

# 1. (20pts) create a functional called cumu_late(), that accepts 
# a vector argument vec_tor, and a function argument "func_tion",
# cumu_late() should return a vector with elements equal to the 
# application of "func_tion" to the sub-vectors of vec_tor, 
# cumu_late() should be equivalent to the functions cumsum(), cumprod(), etc.,
# you can use the functions sapply(), seq_along(), match.fun(), and do.call(), 
# and an anonymous function,

cumu_late <- function(vec_tor, func_tion) {
# validate function name
  func_tion <- match.fun(func_tion)
  sapply(X=seq_along(vec_tor), 
         FUN=function(in_dex) {
           do.call(what=func_tion, args=list(vec_tor[1:in_dex]))
# or simply:
#           func_tion(vec_tor[1:in_dex])
           })  # end sapply
}  # end cumu_late

# call cumu_late() as follows, to verify that it works correctly: 

cumu_late(1:5, sum)
cumu_late(1:5, prod)

# the above should produce exactly the same as:

cumsum(1:5)
cumprod(1:5)

# run the following, to verify that cumu_late() produces exactly 
# the same result as cumsum() and cumprod(), 

identical(
  cumu_late(1:5, sum),
  cumsum(1:5)
)  # end identical

identical(
  cumu_late(1:5, prod),
  cumprod(1:5)
)  # end identical

# 2. (10pts) Benchmark the speed of cumu_late() and cumsum(), 
# on the vector 1:100, using the function microbenchmark(). 

library(microbenchmark)
summary(microbenchmark(
  cumu_late=cumu_late(1:100, sum), 
  cum_sum=cumsum(1:1000), 
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary



############## hw
# Summary: Create a functional called apply_agg(), that 
# applies an aggregation function over columns of data. 

# 1. (30pts) apply_agg() should accept two arguments:
# - da_ta - a data object with multiple columns (matrix, 
#   data frame, or time series). 
# - agg_regate() - an aggregation function that can return 
#   a vector. 
# apply_agg() should first verify if da_ta has a dim attribute. 
# If da_ta does not have a dim attribute, then apply_agg() 
# should directly apply the function agg_regate() to the da_ta 
# and return it. 
# If da_ta does have a dim attribute, then apply_agg() should 
# perform an apply loop over the columns of da_ta, and apply 
# the function agg_regate() to each column. 
# apply_agg() should return a vector or a matrix, with its 
# elements equal to the results of agg_regate(). 
# hint: you can use functions is.null(), dim(), sapply()
# and and if() statement. 

apply_agg <- function(da_ta, agg_regate) {
# validate function name
  agg_regate <- match.fun(agg_regate)
# check if da_ta has a dim attribute
  if (is.null(dim(da_ta)))
# call agg_regate on da_ta
    agg_regate(da_ta)
  else
# perform apply loop over columns of da_ta
    sapply(da_ta, agg_regate)
}  # end apply_agg

# call apply_agg() as follows, to verify that it works correctly: 
apply_agg(1:5, sum)
apply_agg(EuStockMarkets[, 1], mean)
apply_agg(EuStockMarkets, mean)



############## hw
# Summary: Create a functional called do_call(), which is 
# a generalization of do_call_rbind() from the lecture slides.  

# 1. (20pts) do_call() should apply a function to 
# a list of objects, and return a single object.
# do_call() should accept three arguments:
# - func_tion - a function that returns a single object 
#   from a list of input objects (for example paste(), 
#   rbind(), etc). 
# - li_st - a list or vector of objects.
# - "..." - dots optional arguments to func_tion. 
# The dots "..." argument should be passed to func_tion. 
# You can use functions length(), while(), lapply(), 
# and return(). 

do_call <- function(func_tion, li_st, ...) {
# produce function name from argument
  func_tion <- match.fun(func_tion)
  while (length(li_st) > 1) {
# index of odd list elements
    odd_index <- seq(from=1, to=length(li_st), by=2)
# bind neighboring elements and divide li_st by half
    li_st <- lapply(odd_index, function(in_dex) {
      if (in_dex==length(li_st)) {
        return(li_st[[in_dex]])
      }
      return(func_tion(li_st[[in_dex]], li_st[[in_dex+1]], ...))
    })  # end lapply
  }  # end while
# li_st has only one element - return it
  li_st[[1]]
}  # end do_call


# call do_call() as below, to verify that it works correctly,

do_call(paste, c("a", "b", "c"), sep="/")
# should produce:
# [1] "a/b/c"

do_call(rbind, list(1:4, 8:11))
# should produce:
#       [,1] [,2] [,3] [,4]
# [1,]    1    2    3    4
# [2,]    8    9   10   11



############## hw
# Summary: create a vectorized version of function sum(), 
# called vec_sum(), 
# comment: the "+" operator is a vectorized function, as 
# can be seen on this example:
(1:3 + 4)
# But sum() isn't a vectorized function, since it produces 
# a single number, not a vector:
sum(1:3, 4)

# 1. (15pts) create a function called vec_sum(), that accepts 
# two vector arguments "vec1", "vec2", and returns their sum,
# the output of vec_sum() should be a vector equal to "vec1 + vec2".
# You can use the functions sum() and Vectorize(), 
# and an anonymous function,
# you cannot use the "+" operator,

vec_sum <- Vectorize(function(vec1, vec2) sum(vec1, vec2))

# call vec_sum() as follows, to verify that it is vectorized: 
vec_sum(1:3, 4)
# the above should produce exactly the same as:
(1:3 + 4)


# 2. (15pts) create a function called vec_sum(), as above,
# but without using the function Vectorize(),
# you can use the functions sum() and mapply(), 
# you cannot use the function Vectorize() or the "+" operator,

vec_sum <- function(vec1, vec2) {
  mapply(sum, vec1, vec2)
}  # end vec_sum

# call vec_sum() as follows, to verify that it is vectorized: 
vec_sum(1, 4)
vec_sum(1:3, 4)
# the above should produce exactly the same as:
(1:3 + 4)



############## hw
# summary: create a vectorized function that sums up the 
# first few elements of a vector,
# 
# 1. (10pts) create a function called sum_first(), that sums up 
# the elements of a vector called vec_tor, from element "1" 
# up to "first_elements",
sum_first <- function(vec_tor, first_elements) {
  sum(vec_tor[1:first_elements])
}  # end sum_first

# call sum_first() as below, to verify that it works correctly,
sum_first(vec_tor=1:10, first_elements=3)

# perform an sapply loop over "first_elements=3:5" for 
# the vector "vec_tor=1:10",
# the result should be a vector equal to: (6, 10, 15)
sapply(3:5, sum_first, vec_tor=1:10)


# create a new version of function sum_first(), that is 
# vectorized with respect to its "first_elements" argument,
# you can use functions length() and sapply()
sum_first <- function(vec_tor, first_elements) {
  if (length(first_elements)==1)
    sum(vec_tor[1:first_elements])
  else
    sapply(first_elements, sum_first, vec_tor=vec_tor)
}  # end sum_first
sum_first(vec_tor=1:10, first_elements=3:5)


# call sum_first() as below, to verify that it works correctly,
# the result should be a vector equal to: (6, 10, 15)
sum_first(vec_tor=1:10, first_elements=3:5)



##############################
# plotting
##############################


############## hw
# add better format X-axis date labels (not required for full credit)
# first plot without X-axis
plot(zoo_series, type="l", lwd=2, xlab="", ylab="", xaxt="n")
# create X-axis date labels
axis_dates <- seq(from=as.Date("2013-09-01"), to=Sys.Date(), by="quarter")
# add X-axis
axis(side=1, at=axis_dates, labels=format(axis_dates, "%b-%y"))



############## hw
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




##############################
# dates and times
##############################



##############################
# numerical methods
##############################


############## test
# 2. (20pts) Calculate the Fibonacci Sequence using a while loop,
# Calculate the Fibonacci Sequence up to the first element whose 
# value exceeds 100,
# the while loop should stop when the first element of the 
# Fibonacci Sequence exceeds 100,

# fib_seq <- numeric()  # zero length numeric vector
# pre-allocate vector instead of "growing" it
fib_seq <- numeric(10)
fib_seq[1] <- 0  # initialize
fib_seq[2] <- 1  # initialize

in_dex <- 3
while(fib_seq[in_dex-1] < 100) {
  fib_seq[in_dex] <- fib_seq[in_dex-1] + fib_seq[in_dex-2]
  in_dex <- in_dex + 1
}  # end while

fib_seq


############## hw
# add estimate of standard error of estimated price?
# add bootstrap random numbers by sampling columns or rows?
############## hw
# Summary: Estimate the probability of crossing a price 
# barrier by performing multiple simulations of prices.  
# Estimate the probabilities for several different price 
# barrier levels. 
# 
# Start by defining the simulation parameters: 
# number of simulations
simu_number <- 500
# number of steps in each simulation
simu_length <- 1000
# barrier level
lev_el <- 20


# 1. (20pts) 
# Summary: You'll need to perform a number simulations of 
# prices crossing a barrier, equal to simu_number.
# Each simulation should consist of a number of simulation 
# steps equal to simu_length.  

# Perform an sapply() loop, starting from one to 
# simu_number. Inside each loop perform a simulation of 
# prices crossing a barrier.  
# hint: you can adapt the code from the slide titled 
# "Simulating Barrier Options Using Vectorized Functions".
# The sapply() loop should return a boolean vector called 
# did_cross, that should be equal to TRUE if prices did 
# cross the lev_el, and otherwise it should be FALSE. 
# did_cross should be a vector of length simu_number. 

# hint: you can use an anonymous function that accepts an 
# integer argument (the loop count) and returns a boolean 
# value. 

# You can compare the simulated price vector to lev_el, 
# to determine if at any point the prices reached above the 
# lev_el.  If they did, then they must have crossed lev_el 
# at some point. 
# The comparison of prices with lev_el produces a boolean 
# vector, whose sum is zero only if prices never crossed 
# lev_el, and is greater than zero if they did. 
# You can use functions sapply(), sum(), cumsum(), and 
# rnorm(), 

# reset random number generator
set.seed(1121)

did_cross <- sapply(1:simu_number, function(sim_u) {
# simulate prices, return TRUE if they crossed lev_el
  sum(cumsum(rnorm(simu_length)) > lev_el) > 0
})  # end sapply

# Calculate the probability of crossing the lev_el 
# as the sum of did_cross divided by simu_number. 
# The probability should be equal to 0.516. 

sum(did_cross)/simu_number


# 2. (20pts) Perform the same simulation as in p.1 but 
# without using an sapply() loop, only using vectorized 
# functions. 
# Start by creating a matrix of random numbers with 
# dimensions equal to simu_number columns by simu_length 
# rows, and call it price_s. 
# Apply function colCumsums() from package matrixStats to 
# price_s, to calculate the cumulative sums of its columns. 
# You can use functions matrix(), colCumsums(), and rnorm(). 

# load package matrixStats
library(matrixStats)
# reset random number generator
set.seed(1121)
price_s <- matrix(rnorm(simu_number*simu_length), 
                  ncol=simu_number)
price_s <- colCumsums(price_s)

# The columns of price_s represent vectors of simulated prices. 
# Following the methodology of p.1, compare the simulated 
# prices to lev_el, and produce a boolean matrix. 
# Sum up the columns of the boolean matrix to determine the 
# simulations for which the prices crossed the lev_el. 
# and call this boolean vector did_cross. 
# did_cross should be a vector of length simu_number. 
# You can use function colSums() from package matrixStats, 

did_cross <- colSums(price_s > lev_el) > 0

# Calculate the probability of crossing the lev_el 
# as the sum of did_cross divided by simu_number. 
# The probability should be equal to 0.516. 

sum(did_cross)/simu_number


# 3. (20pts) Estimate the probabilities for a vector of 
# different price barrier levels. 
# Create a named numeric vector called lev_els with 
# values from=5, to=60, by=5. 
# You can use functions seq(), structure(), and names(), 

lev_els <- seq(from=5, to=60, by=5)
names(lev_els) <- paste0("level=", lev_els)
# or
lev_els <- seq(from=5, to=60, by=5)
lev_els <- structure(lev_els, names=paste0("level=", lev_els))

# You should get the following result:
# lev_els
# level=5 level=10 level=15 level=20 level=25
#       5       10       15       20       25

# Perform an sapply() loop over lev_els. 
# In each loop calculate the probabilities of crossing 
# the lev_el, and call the resulting vector cross_probs. 
# To receive full credit you shouldn't recalculate 
# price_s for each loop (lev_el), but instead use the 
# price_s already calculated in p.2. 
# You can use functions sapply(), sum(), colSums(), and 
# an anonymous function.

cross_probs <- sapply(lev_els, function(lev_el) {
# sum up number of simulations when prices crossed lev_el
  sum(colSums(price_s > lev_el) > 0)/simu_number
})  # end sapply

# Create a scatterplot of cross_probs versus lev_els. 
# You can use functions plot() and title(). 

plot(x=lev_els, y=cross_probs)
# add title
title(main="barrier crossing probabilities", line=-1)



############## hw
# negative barrier simulation
# 3. (20pts) Modify the code below, to simulate prices crossing 
# a *negative* barrier level,
# after the prices cross the negative barrier level, the simulation 
# should stop, and "simu_prices" should be constant,

### first run this code:

set.seed(1121)  # for reproducibility
simu_length <- 1000  # number of simulation steps
lev_el <- -10  # barrier level
# simulated prices
simu_prices <- cumsum(rnorm(simu_length))


### modify some of the code below this line, as needed,

# in_dex should be "1" after prices cross lev_el
in_dex <- cummax(simu_prices < lev_el)

# find index when prices cross lev_el
which_index <- which(diff(in_dex)==1)

# fill prices after crossing lev_el
if (length(which_index)>0) {
  simu_prices[as.logical(in_dex)] <- 
    simu_prices[which_index + 1]
}  # end if

### end code to be modified,

# after you modify and run the code, the variable "which_index" should be 
# equal to the index for which the value of "simu_prices" is just about 
# to cross the barrier level,
# verify that this is true by looking at:

simu_prices[(which_index-1):(which_index+2)]


############## hw
# double barrier simulation
# 1. Create an R script for simulating 1000 random prices using rnorm(),
set.seed(1121)  # for reproducibility
## set up simulation parameters
simu_length <- 1000  # number of simulation steps
barrier_first <- -10  # barrier level #1
barrier_second <- 10  # barrier level #2

## initialize simulation variables
# initialize vector of prices to zero
simu_prices <- 0.0*(1:simu_length)
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
while ((simu_index <= simu_length) && !both_crossed) {
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
if (simu_index <= simu_length) {
  simu_prices[simu_index:simu_length] <- simu_prices[simu_index - 1]
}

# create daily time series starting 2011
ts_prices <- ts(data=simu_prices, frequency=365, start=c(2011, 1))

# 3. plot the prices and the two barrier levels,

plot(ts_prices, type="l", col="black", lty="solid", xlab="", ylab="")
abline(h=barrier_first, lwd=2, col="blue")  # add horizontal line
abline(h=barrier_second, lwd=2, col="red")  # add horizontal line
title(main="Random Prices", line=0)  # add title



############## hw
# Summary: Perform regressions in an sapply loop, 
# and produce a warning when the p-value is greater 
# than the significance level. 

# 1. (20pts) Create a function called reg_stats() which 
# performs a regression and returns a vector of regression 
# statistics. 
# reg_stats() should accept a single argument called da_ta, 
# which is a data frame containing the design matrix. 
# The first column of da_ta is the response, and the 
# remaining columns are the explanatory variables. 
# reg_stats() should create a formula from the columns of 
# da_ta, then perform a regression, and finally should 
# return a named vector of the regression statistics: 
# p-value, adj.r.squared, fstatistic. 
# hint: you can adapt the code from the slide titled 
# "Influence of Noise on Regression Another Method". 

reg_stats <- function(da_ta) {
# perform regression and get summary
  col_names <- colnames(da_ta)
  reg_formula <- as.formula(paste(col_names[1], 
            paste(col_names[-1], collapse="+"), sep="~"))
  reg_model_sum <- summary(lm(reg_formula, data=da_ta))
# extract regression statistics
  with(reg_model_sum, c(pval=coefficients[2, 4],
                        adj_rsquared=adj.r.squared,
                        fstat=fstatistic[1]))
}  # end reg_stats

# Create a design matrix data frame of response and 
# two explanatory variables as follows:

len_gth <- 10000
set.seed(1121)
noise <- rnorm(len_gth)
explana_tory1 <- rnorm(len_gth)
explana_tory2 <- rnorm(len_gth)
noise_level <- 10
res_ponse <- explana_tory1 + explana_tory2 + noise_level*noise
da_ta <- data.frame(res_ponse, explana_tory1, explana_tory2)

# Apply reg_stats() to da_ta. 
# You should get the following output:
# > reg_stats(da_ta)
#      pval     adj_rsquared  fstat.value
# 5.413256e-26 2.146549e-02 1.106709e+02


# 2. (20pts) Create a named vector of noise levels 
# called noise_levels, 
# from 0.0 to 0.03, with the element names equal 
# to their values:

noise_levels <- seq(from=10, to=100, by=10)
names(noise_levels) <- noise_levels

# You should get the following output:
# > noise_levels
# 10  20  30  40  50  60  70  80  90 100
# 10  20  30  40  50  60  70  80  90 100

# Perform an sapply() loop over noise_levels. 
# In each step create a design matrix data frame 
# with a response and two explanatory variables, 
# and call reg_stats() to perform a regression. 
# If the p-value is greater than the significance 
# level, then produce a warning with text that 
# contains the p-value in it. 
# Finally at the end return the reg_stats() values. 
# You can use the functions sapply(), data.frame(), 
# reg_stats(), paste0(), warning(), and an 
# anonymous function. 

# specify significance level:

sign_level <- 2*pnorm(-2)

regression_stats <- sapply(noise_levels, function(noise_level) {
# create design matrix data frame end perform regression
  res_ponse <- explana_tory1 + explana_tory2 + noise_level*noise
  da_ta <- data.frame(res_ponse, explana_tory1, explana_tory2)
  regstats <- reg_stats(da_ta)
  if(regstats["pval"] > sign_level)
    warning(paste0("regression p-value=", format(regstats["pval"], digits=4), "\tgreater than significance level"))
  regstats
})  # end sapply

# You should get:
# Warning messages:
#   1: In FUN(X[[i]], ...) :
#   regression p-value=0.06155	greater than significance level
# 2: In FUN(X[[i]], ...) :
#   regression p-value=0.09136	greater than significance level
# 3: In FUN(X[[i]], ...) :
#   regression p-value=0.1218	greater than significance level
# 4: In FUN(X[[i]], ...) :
#   regression p-value=0.1514	greater than significance level

# You should get the following output:
# > regression_stats
#                     10           20           30           40           50           60           70
# pval         5.413256e-26 3.971562e-08 1.432442e-04 0.003106125 0.0143015190 0.0347503907 0.0615452232
# adj_rsquared 2.146549e-02 5.732749e-03 2.643543e-03 0.001517169 0.0009763702 0.0006723125 0.0004828453
# fstat.value  1.106709e+02 2.982613e+01 1.425142e+01 8.596609788 5.8861334606 4.3634877194 3.4151514443
#                    80           90          100
# pval         0.0913585883 0.1217981403 0.1514400901
# adj_rsquared 0.0003559211 0.0002662028 0.0002001017
# fstat.value  2.7800613301 2.3312354440 2.0006085829



############## hw - most is already incorporated into lecture slides
# Summary: Estimate the standard errors of regression 
# coefficients using bootstrap simulations. 

# 1. (10pts) Specify a regression as follows:

set.seed(1121)  # reset random number generator
# define explanatory and response variables
explana_tory <- seq(from=0.1, to=3.0, by=0.1)
res_ponse <- 3 + 2*explana_tory + rnorm(length(explana_tory))
# specify regression formula and perform regression
reg_formula <- res_ponse ~ explana_tory
reg_model <- lm(reg_formula)

# Extract the regression coefficient standard errors 
# from the regression model summary, and call them 
# std_errors.
# You can use the function summary().

reg_model_sum <- summary(reg_model)
std_errors <- reg_model_sum$coefficients[, 2]


# 2. (20pts) Perform an sapply() loop 10000 times. 
# In each loop bootstrap (sample) the variables 
# explana_tory and res_ponse, perform the regression, 
# and return the regression coefficients.  
# Call the matrix output by sapply() boot_strap. 
# hint: download the latest version of the 
# statistics.pdf file from NYU Classes and follow 
# the bootstrap example there. 
# You can use the functions sample.int(), lm(), 
# coef(), and sapply().

boot_strap <- sapply(1:10000, function(x) {
  boot_sample <- sample.int(length(explana_tory), replace=TRUE)
  explana_tory <- explana_tory[boot_sample]
  res_ponse <- res_ponse[boot_sample]
  coef(lm(res_ponse ~ explana_tory))
})  # end sapply

# You should get the following output:
# > boot_strap[, 1:3]
#                 [,1]     [,2]     [,3]
# (Intercept)  3.339112 3.458160 3.108136
# explana_tory 1.809326 1.737513 1.864102

# Calculate the standard errors from bootstrap, 
# and call them boot_errors.
# You can use the functions sd() and apply(). 
# Calculate the differences between boot_errors 
# and std_errors. 

boot_errors <- apply(boot_strap, MARGIN=1, sd)
boot_errors - std_errors

# You should get the following output:
# > boot_errors
# (Intercept) explana_tory 
#   0.2655507    0.1326947


# 3. (10pts) Create a density plot of the bootstrapped 
# coefficients boot_strap["explana_tory", ], 
# with plot title "Bootstrapped regression slopes". 
# and with x-axis label "regression slopes". 
# Add a vertical line in red at the x-axis point: 
#   mean(boot_strap["explana_tory", ]). 
# You can use the functions x11(), plot(), density(), 
# and abline(). 

x11()
plot(density(boot_strap["explana_tory", ]), 
     lwd=2, xlab="regression slopes", 
     main="Bootstrapped regression slopes")
abline(v=mean(boot_strap["explana_tory", ]), lwd=2, col="red")



############## hw
# Summary: Create function for bootstrapping a statistical model.

# 1. (10pts) Specify a regression as follows:
# The function read_matrix() should have two arguments:
#   "file" - for a string containing the input file name, 
#   "na_replace" - for a number to replace bad data elements,
# 
# The function read_matrix() should perform the following steps:
#   - read a comma-delimited CSV file - use read.csv() 
#       with "stringsAsFactors=FALSE",
#   - assign rownames from first column of data, and then remove it,
#   - verify that every column of input data is numeric, 
#   - if necessary coerce column data to numeric,
#   - and replace NA elements with "na_replace", 
#     - use sapply(), as.numeric(), is.na(), 
# 





############## hw
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
# flip coin and advance "he_ads" by 1 if it's heads
  he_ads <- he_ads + (runif(1) < coin_bias)
# or for unbiased coins:
#  he_ads <- he_ads + sample(0:1, 1)
# advance loop counter
  num_loops <- num_loops + 1
}  # end while


# Create a function called coin_flip(), which calculates the 
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
# a certain number of heads, using a vector of different "coin_bias" 
# parameters, from 0.2 to 0.8, in 0.1 increments, 
# with "heads_max=10",
# 
# perform an sapply() loop over a vector of "coin_bias" parameters,
# and pass "heads_max=10" to coin_flip() using the dots "..." argument,
# plot the vector returned by sapply(), and give proper names to 
# the axis labels,
coin_biases <- seq(from=0.2, to=0.8, by=0.1)
num_flips <- sapply(coin_biases, coin_flip, heads_max=10)
plot(x=coin_biases, y=num_flips, t="l", 
     xlab="coin bias", ylab="number of coin flips")




##############################
# data munging input output error handling
##############################


############## hw - already in slides?
# Summary: load and scrub a matrix containing bad data. 

# 1. (5pts) 
# Download the file "matrix_bad.csv" from NYU Classes),
# the file contains a numeric matrix with row and column names, 
# with some columns containing bad data elements that aren't numeric.
# Read the file into a variable called mat_rix using read.csv(),
# make sure to read strings as strings, not as factors,
# and read in properly the row names of mat_rix. 
# You can either use the first column of data for row names, 
# or use function read.csv() with arguments 
# "row.names=1" and "stringsAsFactors=FALSE", 

mat_rix <- read.csv(file="badmatrix.csv", stringsAsFactors=FALSE)
rownames(mat_rix) <- mat_rix[, 1]
mat_rix <- mat_rix[, -1]
# or
mat_rix <- read.csv(file="badmatrix.csv", row.names=1,
                    stringsAsFactors=FALSE)

# 2. (15pts) determine the class of mat_rix, and 
# calculate a vector of the classes of the columns 
# of mat_rix. 
# You can use the functions sapply() and class(), 

class(mat_rix)
col_class <- sapply(mat_rix, class)

# calculate the vector of indices of the columns that are 
# of class "character", and call it "col_index", 
# you can use function which(),

col_index <- which(col_class=="character")

# 3. (15pts) perform an sapply() loop over the "character" 
# columns of mat_rix, coerce them to "numeric" vectors, 
# and call the result "col_fixed", 
# you can use functions sapply() and as.numeric(), 

col_fixed <- sapply(mat_rix[, col_index], as.numeric)

# replace the "character" columns of mat_rix with 
# "col_fixed", using the vector "col_index", 

mat_rix[, col_index] <- col_fixed

# 4. (10pts) Perform an apply() loop over the rows of 
# mat_rix, calculate the row means, and call the result 
# "row_means", 
# You can use functions apply() and mean(), 
# ignore NA values using the argument "na.rm=TRUE". 
# You cannot use an anonymous function. 

row_means <- apply(mat_rix, 1, mean, na.rm=TRUE)

# 5. (20pts) Replace NA values in mat_rix with the 
# corresponding row means. 
# You can use function is.na(), and function which() with 
# the argument "arr.ind=TRUE". 
# You cannot perform any loops, only subsetting of matrices. 

is_na <- which(is.na(mat_rix), arr.ind=TRUE)
mat_rix[is_na] <- row_means[is_na[, 1]]

# coerce mat_rix to a matrix, 
# you can use as.matrix(),

mat_rix <- as.matrix(mat_rix)


############## hw
# 1. (40pts) Create a function for reading numeric matrices 
# that contain bad data, and call it "read_matrix()",
# The function read_matrix() should have two arguments:
#   "file" - for a string containing the input file name, 
#   "na_replace" - for a number to replace bad data elements,
# 
# The function read_matrix() should perform the following steps:
#   - read a comma-delimited CSV file - use read.csv() 
#       with "stringsAsFactors=FALSE",
#   - assign rownames from first column of data, and then remove it,
#   - verify that every column of input data is numeric, 
#   - if necessary coerce column data to numeric,
#   - and replace NA elements with "na_replace", 
#     - use sapply(), as.numeric(), is.na(), 
# 
# The input file contains a matrix with row and column names, 
# The function read_matrix() should return a numeric matrix with 
# proper dimensions and names,

read_matrix <- function(file, na_replace=0) {
# read the CSV file
  mat_rix <- read.csv(
    file="badmatrix.csv",
    stringsAsFactors=FALSE)
# assign rownames and remove first column
  rownames(mat_rix) <- mat_rix[, 1]
  mat_rix <- mat_rix[, -1]
# save dimnames
  dim_names <- dimnames(mat_rix)
# coerce columns to numeric and replace NAs
  mat_rix <- sapply(mat_rix, function(col_umn) {
    col_umn <- as.numeric(col_umn)
    col_umn[is.na(col_umn)] <- na_replace
    col_umn
  }  # end anon function
  )  # end sapply
# restore dimnames
  dimnames(mat_rix) <- dim_names
  as.matrix(mat_rix)
}  # end read_matrix


# download the file "badmatrix.csv" from NYU Classes,
# and run this code, to verify that it works properly,

read_matrix(file="badmatrix.csv", na_replace=1000)


############## hw
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



############## test
# Summary: Perform error handling within an sapply loop. 

# 1. (10pts) Download the file "matrix_bad.csv" from NYU 
# Classes. 
# The file contains a numeric matrix with row and column 
# names.  One column contains a bad data element that 
# isn't numeric. 
# Read the file into a variable called mat_rix using 
# read.csv(). Make sure to read strings as strings, not 
# as factors. Read in properly the row names of mat_rix. 
# You can either use the first column of data for row 
# names, or use function read.csv() with arguments 
# "row.names=1" and "stringsAsFactors=FALSE",

mat_rix <- read.csv(file="matrix_bad.csv", stringsAsFactors=FALSE)
rownames(mat_rix) <- mat_rix[, 1]
mat_rix <- mat_rix[, -1]
# or
mat_rix <- read.csv(file="matrix_bad.csv", row.names=1,
                    stringsAsFactors=FALSE)


# 2. (10pts) Calculate the sums of the columns of mat_rix, 
# by performing an sapply loop over the columns of mat_rix, 
# and call the vector col_sums. 
# You can use functions sapply(), as.numeric(), sum() with 
# argument "na.rm=TRUE", and an anonymous function. 
# The anonymous function should coerce each column to 
# numeric, and then calculate its sum. 

col_sums <- sapply(mat_rix, function(col_umn) {
  sum(as.numeric(col_umn), na.rm=TRUE)
}  # end anon function
)  # end sapply

# Set the warning option "warn" equal to 2, using the 
# function options(). 

options(warn=2)

# Perform the above sapply() loop again. 
# It now produces an error, and doesn't return anything,


# 3. (40pts) Rewrite the above sapply loop and wrap the 
# body of the anonymous function in tryCatch(). 
# Create another anonymous function to use as an error 
# handler in tryCatch(). 
# The error handler should write the text of the error 
# condition to the console using cat(). 
# The error handler should also write the column data 
# that caused the error to a text file called "error.txt", 
# using cat(). 
# You can omit the "finally" argument. 
# You can use functions sapply(), as.numeric(), sum() with 
# argument "na.rm=TRUE", tryCatch(), cat(), and an 
# anonymous function. 

col_sums <- sapply(mat_rix, 
                   function(col_umn)
                     tryCatch( {  # body
                       sum(as.numeric(col_umn), na.rm=TRUE)
                     },
                     error=function(error_cond) {
                       cat(paste("error:", error_cond))
                       cat(col_umn, file="error.txt")
                     }  # end error handler
                     )  # end tryCatch
)  # end sapply

# The apply loop returns a list instead of a vector. 
# Flatten the list into a vector using do.call() and cbind(). 

do.call(cbind, col_sums)

# demonstrate that sapply returns list if function returns vectors of variable length
# calculate stats of ts and return as vector of variable length

my_stats <- function(ts_var) {
  c(max(ts_var), min(ts_var), mean(ts_var), if (rnorm(1)>0) 1 else NULL)
}  # end my_stats

# sapply returns list because of vectors of variable length

out_sapply <- sapply(EuStockMarkets, my_stats)

