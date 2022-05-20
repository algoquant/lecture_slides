# Create a list with two elements
listv <- list(c("a", "b"), 1:4)
listv
c(typeof(listv), mode(listv), class(listv))
# Lists are also vectors
c(is.vector(listv), is.list(listv))
NROW(listv)
# Create named list
listv <- list(first=c("a", "b"), second=1:4)
listv
names(listv)
unlist(listv)

listv[2]  # Extract second element as sublist
listv[[2]]  # Extract second element
listv[[2]][3]  # Extract third element of second element
listv[[c(2, 3)]]  # Third element of second element
listv$second  # Extract second element
listv$s  # Extract second element - partial name matching
listv$second[3]  # Third element of second element
listv <- list()  # Empty list
listv$a <- 1
listv[2] <- 2
listv
names(listv)

# Convert vector elements to list elements
as.list(1:3)
# Convert whole vector to single list element
list(1:3)

data_frame <- data.frame(  # Create a data frame
                type=c("rose", "daisy", "tulip"),
                color=c("red", "white", "yellow"),
                price=c(1.5, 0.5, 1.0)
              )  # end data.frame
data_frame
dim(data_frame)  # Get dimension attribute
colnames(data_frame)  # Get the colnames attribute
rownames(data_frame)  # Get the rownames attribute
class(data_frame)  # Get object class
typeof(data_frame)  # Data frames are listv
is.data.frame(data_frame)

class(data_frame$type)  # Get column class
class(data_frame$price)  # Get column class

data_frame[, 3]  # Extract third column as vector
data_frame[[3]]  # Extract third column as vector
data_frame[3]  # Extract third column as data frame
data_frame[, 3, drop=FALSE]  # Extract third column as data frame
data_frame[[3]][2]  # Second element from third column
data_frame$price[2]  # Second element from "price" column
is.data.frame(data_frame[[3]]); is.vector(data_frame[[3]])
data_frame[2, ]  # Extract second row
data_frame[2, ][3]  # Third element from second column
data_frame[2, 3]  # Third element from second column
unlist(data_frame[2, ])  # Coerce to vector
is.data.frame(data_frame[2, ]); is.vector(data_frame[2, ])

data_frame <- data.frame(  # Create a data frame
                type=c("rose", "daisy", "tulip"),
                color=c("red", "white", "yellow"),
                price=c(1.5, 0.5, 1.0),
                row.names=c("flower1", "flower2", "flower3")
              )  # end data.frame
data_frame
class(data_frame$type)  # Get column class
class(data_frame$price)  # Get column class
# Set option to not coerce character vectors to factors
options("stringsAsFactors")
default.stringsAsFactors()
options(stringsAsFactors=FALSE)

str(data_frame)  # Display the object structure
dim(cars)  # The cars data frame has 50 rows
head(cars, n=5)  # Get first five rows
tail(cars, n=5)  # Get last five rows

# Create a named vector of student scores
score_s <- sample(round(runif(5, min=1, max=10), digits=2))
names(score_s) <- c("Angie", "Chris", "Suzie", "Matt", "Liz")
# Sort the vector into ascending order
sort(score_s)
# Calculate index to sort into ascending order
order(score_s)
# Sort the vector into ascending order
score_s[order(score_s)]
# Calculate the sorted (ordered) vector
sortv <- score_s[order(score_s)]
# Calculate index to sort into unsorted (original) order
order(order(score_s))
sortv[order(order(score_s))]
score_s
# Examples for sort() with ties
order(c(2, 1:4))  # There's a tie
order(c(2, 1:4), 1:5)  # There's a tie

# Create a vector of student ranks
ra_nks <- c("fifth", "fourth", "third", "second", "first")
# Reverse sort the student ranks according to students
ra_nks[order(order(score_s))]
# Create a data frame of students and their ranks
ros_ter <- data.frame(score=score_s, 
  rank=ra_nks[order(order(score_s))])
ros_ter
# Permutation index on price column
order(data_frame$price)
# Sort data_frame on price column
data_frame[order(data_frame$price), ]
# Sort data_frame on color column
data_frame[order(data_frame$color), ]


as.matrix(data_frame)
vectorv <- sample(9)
matrix(vectorv, ncol=3)
as.matrix(vectorv, ncol=3)

matrixv <- matrix(5:10, nrow=2, ncol=3)  # Create a matrix
rownames(matrixv) <- c("row1", "row2")  # Rownames attribute
colnames(matrixv) <- c("col1", "col2", "col3")  # Colnames attribute
library(microbenchmark)
# Call method instead of generic function
as.data.frame.matrix(matrixv)
# A few methods for generic function as.data.frame()
sample(methods(as.data.frame), size=4)
# Function method is faster than generic function
summary(microbenchmark(
  as_data_frame_matrix=
    as.data.frame.matrix(matrixv),
  as_data_frame=as.data.frame(matrixv),
  data_frame=data.frame(matrixv),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

library(microbenchmark)
# lapply is faster than coercion function
summary(microbenchmark(
  aslist=as.list(as.data.frame.matrix(matrixv)),
  lapply=lapply(seq_along(matrixv[1, ]),
     function(indeks) matrixv[, indeks]),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# ?iris  # Get information on iris
dim(iris)
head(iris, 2)
colnames(iris)
unique(iris$Species)  # List of unique elements of iris
class(unique(iris$Species))
# Find which columns of iris are numeric
sapply(iris, is.numeric)
# Calculate means of iris columns
sapply(iris, mean)  # Returns NA for Species

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
unique(mtcars$cyl)  # Extract list of car cylinders
sapply(mtcars, mean)  # Calculate means of mtcars columns

library(MASS)
# ?Cars93  # Get information on Cars93
dim(Cars93)
head(colnames(Cars93))
# head(Cars93, 2)
unique(Cars93$Type)  # Extract list of car types
# sapply(Cars93, mean)  # Calculate means of Cars93 columns
# Plot histogram of Highway MPG using the Freedman-Diaconis rule
hist(Cars93$MPG.highway, col="lightblue1",
     main="Distance per Gallon 1993", xlab="Highway MPG", breaks="FD")

rm(list=ls())
as.numeric(c(1:3, "a"))  # NA from coercion
0/0  # NaN from ambiguous math
1/0  # Inf from divide by zero
is.na(c(NA, NaN, 0/0, 1/0))  # Test for NA
is.nan(c(NA, NaN, 0/0, 1/0))  # Test for NaN
NA*1:4  # Create vector of Nas
# Create vector with some NA values
datav <- c(1, 2, NA, 4, NA, 5)
datav
mean(datav)  # Returns NA, when NAs are input
mean(datav, na.rm=TRUE)  # remove NAs from input data
datav[!is.na(datav)]  # Delete the NA values
sum(!is.na(datav))  # Count non-NA values

# airquality data has some NAs
head(airquality)
dim(airquality)
# Number of NA elements
sum(is.na(airquality))
# Number of rows with NA elements
sum(!complete.cases(airquality))
# Display rows containing NAs
head(airquality[!complete.cases(airquality), ])

# Create vector containing NA values
vectorv <- sample(22)
vectorv[sample(NROW(vectorv), 4)] <- NA
# Replace NA values with the most recent non-NA values
zoo::na.locf(vectorv)
# Remove rows containing NAs
good_air <- airquality[complete.cases(airquality), ]
dim(good_air)
# NAs removed
head(good_air)
# Another way of removing NAs
fresh_air <- na.omit(airquality)
all.equal(fresh_air, good_air, check.attributes=FALSE)
# Replace NAs
good_air <- zoo::na.locf(airquality)
dim(good_air)
# NAs replaced
head(good_air)

# Replace NAs in xts time series
library(rutils)  # load package rutils
se_ries <- rutils::etfenv$prices[, 1]
head(se_ries, 3)
sum(is.na(se_ries))
series_zoo <- zoo::na.locf(se_ries, fromLast=TRUE)
series_xts <- xts:::na.locf.xts(se_ries, fromLast=TRUE)
all.equal(series_zoo, series_xts, check.attributes=FALSE)
head(series_xts, 3)
library(microbenchmark)
summary(microbenchmark(
  zoo=zoo::na.locf(se_ries, fromLast=TRUE),
  xts=xts:::na.locf.xts(se_ries, fromLast=TRUE),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# NULL values have no mode or type
c(mode(NULL), mode(NA))
c(typeof(NULL), typeof(NA))
c(length(NULL), length(NA))
# Check for NULL values
is.null(NULL)
# NULL values are ignored when combined into a vector
c(1, 2, NULL, 4, 5)
# But NA value isn't ignored
c(1, 2, NA, 4, 5)
# Vectors can be initialized to NULL
vectorv <- NULL
is.null(vectorv)
# Grow the vector in a loop - very bad code!!!
for (indeks in 1:5)
  vectorv <- c(vectorv, indeks)
# Initialize empty vector
vectorv <- numeric()
# Grow the vector in a loop - very bad code!!!
for (indeks in 1:5)
  vectorv <- c(vectorv, indeks)
# Allocate vector
vectorv <- numeric(5)
# Assign to vector in a loop - good code
for (indeks in 1:5)
  vectorv[indeks] <- runif(1)

# Create list of vectors
listv <- lapply(1:3, function(x) sample(6))
# Bind list elements into matrix - doesn't work
rbind(listv)
# Bind list elements into matrix - tedious
rbind(listv[[1]], listv[[2]], listv[[3]])
# Bind list elements into matrix - works!
do.call(rbind, listv)
# Create numeric list
listv <- list(1, 2, 3, 4)
do.call(rbind, listv)  # Returns single column matrix
do.call(cbind, listv)  # Returns single row matrix
# Recycling rule applied
do.call(cbind, list(1:2, 3:5))
# NULL element is skipped
do.call(cbind, list(1, NULL, 3, 4))
# NA element isn't skipped
do.call(cbind, list(1, NA, 3, 4))

library(microbenchmark)
list_vectors <- lapply(1:5, rnorm, n=10)
matrixv <- do.call(rbind, list_vectors)
dim(matrixv)
do_call_rbind <- function(listv) {
  while (NROW(listv) > 1) {
# Index of odd list elements
    odd_index <- seq(from=1, to=NROW(listv), by=2)
# Bind odd and even elements, and divide listv by half
    listv <- lapply(odd_index, function(indeks) {
if (indeks==NROW(listv)) return(listv[[indeks]])
rbind(listv[[indeks]], listv[[indeks+1]])
    })  # end lapply
  }  # end while
# listv has only one element - return it
  listv[[1]]
}  # end do_call_rbind
identical(matrixv, do_call_rbind(list_vectors))

library(microbenchmark)
airquality[(airquality$Solar.R > 320 &
        !is.na(airquality$Solar.R)), ]
subset(x=airquality, subset=(Solar.R > 320))
summary(microbenchmark(
    subset=subset(x=airquality, subset=(Solar.R > 320)),
    brackets=airquality[(airquality$Solar.R > 320 &
            !is.na(airquality$Solar.R)), ],
times=10))[, c(1, 4, 5)]  # end microbenchmark summary

unique(iris$Species)  # Species has three distinct values
# Split into separate data frames by hand
setosa <- iris[iris$Species=="setosa", ]
versi <- iris[iris$Species=="versicolor", ]
virgin <- iris[iris$Species=="virginica", ]
dim(setosa)
head(setosa, 2)
# Split iris into list based on Species
split_iris <- split(iris, iris$Species)
str(split_iris, max.confl=1)
names(split_iris)
dim(split_iris$setosa)
head(split_iris$setosa, 2)
all.equal(setosa, split_iris$setosa)

unique(mtcars$cyl)  # cyl has three unique values
# Split mpg column based on number of cylinders
split(mtcars$mpg, mtcars$cyl)
# Split mtcars data frame based on number of cylinders
split_cars <- split(mtcars, mtcars$cyl)
str(split_cars, max.confl=1)
names(split_cars)
# Aggregate the mean mpg over split mtcars data frame
sapply(split_cars, function(x) mean(x$mpg))
# Or: split mpg column and aggregate the mean
sapply(split(mtcars$mpg, mtcars$cyl), mean)
# Same but using with()
with(mtcars, sapply(split(mpg, cyl), mean))
# Or: aggregate() using formula syntax
aggregate(formula=(mpg ~ cyl), data=mtcars, FUN=mean)
# Or: aggregate() using data frame syntax
aggregate(x=mtcars$mpg, by=list(cyl=mtcars$cyl), FUN=mean)
# Or: using name for mpg
aggregate(x=list(mpg=mtcars$mpg), by=list(cyl=mtcars$cyl), FUN=mean)
# Aggregate() all columns
aggregate(x=mtcars, by=list(cyl=mtcars$cyl), FUN=mean)

# Mean mpg for each cylinder group
tapply(X=mtcars$mpg, INDEX=mtcars$cyl, FUN=mean)
# using with() environment
with(mtcars, tapply(X=mpg, INDEX=cyl, FUN=mean))
# Function sapply() instead of tapply()
with(mtcars, sapply(sort(unique(cyl)), function(x) {
 structure(mean(mpg[x==cyl]), names=x)
     }))  # end with
# Function by() instead of tapply()
with(mtcars, by(data=mpg, INDICES=cyl, FUN=mean))

# Get several mpg stats for each cylinder group
data_cars <- sapply(split_cars,
        function(x) {
          c(mean=mean(x$mpg), max=max(x$mpg), min=min(x$mpg))
        }  # end anonymous function
        )  # end sapply
data_cars  # sapply() produces a matrix
data_cars <- lapply(split_cars,  # Now same using lapply
        function(x) {
          c(mean=mean(x$mpg), max=max(x$mpg), min=min(x$mpg))
        }  # end anonymous function
        )  # end sapply
is.list(data_cars)  # lapply produces a list
# do.call flattens list into a matrix
do.call(cbind, data_cars)

# Download CRSPpanel.txt from Brightspace
# Read the file using read.table() with header and sep arguments
panel_data <- read.table(file="/Users/jerzy/Develop/lecture_slides/data/CRSPpanel.txt",
                   header=TRUE, sep="\t")
# Split panel_data based on Industry column
split_panel <- split(panel_data, panel_data$Industry)
# Number of companies in each Industry
sapply(split_panel, NROW)
# Number of Sectors that each Industry belongs to
sapply(split_panel, function(x) {
  NROW(unique(x$Sector))
})  # end sapply
# Or
aggregate(formula=(Sector ~ Industry),
  data=panel_data, FUN=function(x) NROW(unique(x)))
# Industries and the Sector to which they belong
aggregate(formula=(Sector ~ Industry), data=panel_data,
    FUN=unique)
# Or
with(panel_data, aggregate(x=Sector, by=list(Industry),
                     FUN=unique))
# Or
with(panel_data, sapply(unique(Industry),
  function(x) {
    Sector[match(x, Industry)]
  }))  # end sapply

# Split panel_data based on Sector column
split_panel <- split(panel_data, panel_data$Sector)
# Number of companies in each Sector
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
# Coerce sec_ind2 into a jagged array
namesv <- as.vector(sec_ind2[, 1])
sec_ind2 <- sec_ind2[, 2]
names(sec_ind2) <- namesv
all.equal(sec_ind2, sec_ind)
# Or use tapply() (returns an array)
sec_ind2 <- with(panel_data,
  tapply(X=as.vector(Industry), INDEX=Sector, FUN=unique))
# Coerce sec_ind2 into a jagged array
sec_ind2 <- drop(as.matrix(sec_ind2))
all.equal(sec_ind2, sec_ind)

# Average ROE in each Industry
with(panel_data,
  sapply(split(ROE, Industry), mean))
# Average, min, and max ROE in each Industry
t(with(panel_data,
  sapply(split(ROE, Industry), FUN=function(x)
c(mean=mean(x), max=max(x), min=min(x)))  # end sapply
  ))  # end with
# Split panel_data based on Industry column
split_panel <- split(panel_data, panel_data$Industry)
# Average ROE and EPS in each Industry
t(sapply(split_panel, FUN=function(x)
  c(mean_roe=mean(x$ROE),
    mean_eps=mean(x$EPS.EXCLUDE.EI))))
# Or: split panel_data based on Industry column
split_panel <- split(panel_data[, c("ROE", "EPS.EXCLUDE.EI")],
  panel_data$Industry)
# Average ROE and EPS in each Industry
t(sapply(split_panel, FUN=function(x) sapply(x, mean)))
# Average ROE and EPS using aggregate()
aggregate(x=panel_data[, c("ROE", "EPS.EXCLUDE.EI")],
  by=list(panel_data$Industry), FUN=mean)
