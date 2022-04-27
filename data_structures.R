






# Single numbers are vectors of length 1
1
# Character strings are vectors of length 1
"a"
# Strings without quotes are variable names
a  # Variable "a" doesn't exist
# List elements can have different mode
list(aa=c("a", "b"), bb=1:5)
data.frame(aa=c("a", "b"), bb=1:2)
is.atomic(data.frame(aa=c("a", "b"), bb=1:2))
is.recursive(data.frame(aa=c("a", "b"), bb=1:2))

myvar <- "hello"
c(typeof(myvar), mode(myvar), class(myvar))

myvar <- 1:5
c(typeof(myvar), mode(myvar), class(myvar))

myvar <- runif(5)
c(typeof(myvar), mode(myvar), class(myvar))

myvar <- matrix(1:10, 2, 5)
c(typeof(myvar), mode(myvar), class(myvar))

myvar <- matrix(runif(10), 2, 5)
c(typeof(myvar), mode(myvar), class(myvar))

myvar <- list(aa=c("a", "b"), bb=1:5)
c(typeof(myvar), mode(myvar), class(myvar))

myvar <- data.frame(aa=c("a", "b"), bb=1:2)
c(typeof(myvar), mode(myvar), class(myvar))

# A simple vector has no attributes
attributes(5:10)
myvar <- c(pi=pi, euler=exp(1), gamma=-digamma(1))
# Named vector has "namesv" attribute
attributes(myvar)
myvar <- 1:10
is.vector(myvar)  # Is the object a vector?
attributes(myvar) <- list(my_attr="foo")
myvar
is.vector(myvar)  # Is the object a vector?
myvar <- 0
attributes(myvar) <- list(class="Date")
myvar  # "Date" object
structure(0, class="Date")  # "Date" object

myvar <- matrix(runif(10), 2, 5)
class(myvar)  # Has implicit class
# But no explicit "class" attribute
attributes(myvar)
c(typeof(myvar), mode(myvar), class(myvar))
# Assign explicit "class" attribute
class(myvar) <- "my_class"
class(myvar)  # Has explicit "class"
# Has explicit "class" attribute
attributes(myvar)
is.matrix(myvar)  # Is the object a matrix?
is.vector(myvar)  # Is the object a vector?
attributes(unclass(myvar))

# Integer implicit class derived from type
myvar <- vector(mode="integer", length=10)
c(typeof(myvar), mode(myvar), class(myvar))
# Numeric implicit class derived from mode
myvar <- vector(mode="numeric", length=10)
c(typeof(myvar), mode(myvar), class(myvar))
# Adding dim attribute changes implicit class to matrix
dim(myvar) <- c(5, 2)
c(typeof(myvar), mode(myvar), class(myvar))
# Data frames have implicit dim attribute
myvar <- data.frame(aa=c("a", "b"), bb=1:2)
c(typeof(myvar), mode(myvar), class(myvar))
attributes(myvar)
dim(myvar)

myvar <- 1:5
c(typeof(myvar), mode(myvar), class(myvar))
mode(myvar) <- "character"  # Coerce to "character"
myvar
c(typeof(myvar), mode(myvar), class(myvar))
# Explicitly coerce to "character"
myvar <- as.character(1:5)
c(typeof(myvar), mode(myvar), class(myvar))
matrixv <- matrix(1:10, 2, 5)  # Create matrix
# Explicitly coerce to "character"
matrixv <- as.character(matrixv)
c(typeof(matrixv), mode(matrixv), class(matrixv))
# Coercion converted matrix to vector
c(is.matrix(matrixv), is.vector(matrixv))
as.logical(0:3)  # Explicit coercion to "logical"
as.numeric(c(FALSE, TRUE, TRUE, TRUE))
c(1:3, "a")  # Implicit coercion to "character"
# Explicit coercion to "numeric"
as.numeric(c(1:3, "a"))

"Hello World!"  # Type some text
# hello is a variable name, because it's not in quotes
hello  # R interretsp "hello" as a variable name
is.vector(1)  # Single number is a vector
is.vector("a")  # String is a vector
4:8  # Create a vector
# Create vector using c() combine function
c(1, 2, 3, 4, 5)
# Create vector using c() combine function
c("a", "b", "c")
# Create vector using c() combine function
c(1, "b", "c")

str_var <- "Some string"
str_var
str_var[1]
str_var[2]

NROW(str_var)  # length of vector
nchar(str_var)  # length of string

# Concatenate and echo to console
cat("Hello", "World!")
cat("Enter\ttab")
cat("Enter\nnewline")
cat("Enter\\backslash")

str_var1 <- "Hello"  # Define a character string
str_var2 <- "World!"  # Define a character string
paste(str_var1, str_var2, sep=" ")  # Concatenate and return value
cat(str_var1, str_var2)  # Concatenate and echo to console
paste("a", 1:4, sep="-")  # Convert, recycle and concatenate
paste(c("a1", "a2", "a3"), collapse="+")  # Collapse vector to string
paste(list("a1", "a2", "a3"), collapse="+")
paste("Today is", Sys.time())  # Coerce and concatenate strings
paste("Today is", format(Sys.time(), "%B-%d-%Y"))
strsplit("Hello World", split="r")  # Split string
strsplit("Hello.World", split="[.]")  # Split string
strsplit("Hello.World", split=".", fixed=TRUE)  # Split string
substring("Hello World", 3, 6)  # Extract characters from 3 to 6

gsub("is", "XX", "is this gratis?")  # Replace "is" with "XX"

grep("b", c("abc", "xyz", "cba d", "bbb"))  # Get indexes

grep("b", c("abc", "xyz", "cba d", "bbb"), value=TRUE)  # Get values

glob2rx("abc.*")  # Convert globs into regex
glob2rx("*.doc")

is.vector(1)  # Single number is a vector
is.vector("a")  # String is a vector
vectorv <- c(8, 6, 5, 7)  # Create vector
vectorv
vectorv[2]  # Extract second element
# Extract all elements, except the second element
vectorv[-2]
# Create Boolean vector
c(FALSE, TRUE, TRUE)
# Extract second and third elements
vectorv[c(FALSE, TRUE, TRUE)]
letters[5:10]  # Vector of letters
c("a", letters[5:10])  # Combine two vectors of letters

0:10  # Vector of integers from 0 to 10
vector()  # Create empty vector
vector(mode="numeric", length=10)  # Numeric vector of zeros
seq(10)  # Sequence from 1 to 10
seq(along=(-5:5))  # Instead of 1:NROW(obj)
seq_along(c("a", "b", "c"))  # Instead of 1:NROW(obj)
seq(from=0, to=1, len=11)  # Decimals from 0 to 1.0
seq(from=0, to=1, by=0.1)  # Decimals from 0 to 1.0
seq(-2,2, len=11)  # 10 numbers from -2 to 2
rep(100, times=5)  # Replicate a number
character(5)  # Create empty character vector
numeric(5)  # Create empty numeric vector
numeric(0)  # Create zero-length vector

2*4:8  # Multiply a vector
2*(4:8)  # Multiply a vector
4:8/2  # Divide a vector
(0:10)/10  # Divide vector - decimals from 0 to 1.0
vectorv <- c(8, 6, 5, 7)  # Create vector
vectorv
# Boolean vector TRUE if element is equal to second one
vectorv == vectorv[2]
# Boolean vector TRUE for elements greater than six
vectorv > 6
2*vectorv  # Multiply all elements by 2
vectorv^2  # Square all elements
c(11, 5:10)  # Combine two vectors
c(vectorv, 2.0)  # Append number to vector

vectorv <- # Create named vector
  c(pi_const=pi, euler=exp(1), gamma=-digamma(1))
vectorv
names(vectorv)  # Get names of elements
vectorv["euler"]  # Get element named "euler"
names(vectorv) <- c("pie","eulery","gammy")  # Rename elements
vectorv
unname(vectorv)  # Remove names attribute
letters[5:10]  # Vector of letters
c("a", letters[5:10])  # Combine two vectors of letters
# Create named vector
structure(sample(1:5), names=paste0("el", 1:5))

vectorv  # Named vector
# Extract second element
vectorv[2]
# Extract all elements, except the second element
vectorv[-2]
# Extract zero elements - returns zero-length vector
vectorv[0]
# Extract second and third elements
vectorv[c(FALSE, TRUE, TRUE)]
# Extract elements using their names
vectorv["eulery"]
# Extract elements using their names
vectorv[c("pie", "gammy")]
# Subset whole vector
vectorv[] <- 0

vectorv <- runif(5)
vectorv
vectorv > 0.5  # Boolean vector
# Boolean vector of elements equal to the second one
vectorv == vectorv[2]
# Extract all elements equal to the second one
vectorv[vectorv == vectorv[2]]
vectorv < 1  # Boolean vector of elements less than one
# Extract all elements greater than one
vectorv[vectorv > 1]
vectorv[vectorv > 0.5]  # Filter elements > 0.5
which(vectorv > 0.5)  # Index of elements > 0.5

# Create factor vector
factorv <- factor(c("b", "c", "d", "a", "c", "b"))
factorv
factorv[3]
# Get factor attributes
attributes(factorv)
# Get allowed values
levels(factorv)
# Get encoding vector
as.numeric(factorv)
is.vector(factorv)
# Coerce vector to factor
as.factor(1:5)
# Coerce factor to character vector
as.vector(as.factor(1:5))

factorv
# Get unique elements
unique(factorv)
# Get levels attribute of the factor
levels(factorv)
# Calculate the factor elements from its levels
levels(factorv)[as.numeric(factorv)]
# Get contingency (frequency) table
table(factorv)
# Get contingency table using sapply
sapply(confls(factorv),
 function(confl)v) {
   sum(confl)v == factorv)
 })  # end sapply

# Display the formal arguments of findInterval
args(findInterval)
# Get index of the element of "vec" that matches 5
findInterval(x=5, vec=c(3, 5, 7))
match(5, c(3, 5, 7))
# No exact match
findInterval(x=6, vec=c(3, 5, 7))
match(6, c(3, 5, 7))
# Indices of "vec" that match elements of "x"
findInterval(x=1:8, vec=c(3, 5, 7))
# Return only indices of inside intervals
findInterval(x=1:8, vec=c(3, 5, 7), all.inside=TRUE)
# make rightmost interval inclusive
findInterval(x=1:8, vec=c(3, 5, 7), rightmost.closed=TRUE)

# Named numeric vector of breakpoints
brea_ks <- c(freezing=0, very_cold=30, cold=50, 
       pleasant=60, warm=80, hot=90)
brea_ks
tempe_ratures <- runif(10, min=10, max=100)
feels_like <- names(
  brea_ks[findInterval(x=tempe_ratures, vec=brea_ks)])
names(tempe_ratures) <- feels_like
tempe_ratures

library(microbenchmark)
datav <- sample(0:6) + 0.1
datav
cut(x=datav, breaks=c(2, 4, 6, 8))
rbind(datav, cut(x=datav, breaks=c(2, 4, 6, 8)))
# cut() replicates findInterval()
cut(x=1:8, breaks=c(3, 5, 7), labels=1:2,
    right=FALSE)
findInterval(x=1:8, vec=c(3, 5, 7))
# findInterval() is a compiled function, so it's faster than cut()
vectorv <- rnorm(1000)
summary(microbenchmark(
  find_interval=
    findInterval(x=vectorv, vec=c(3, 5, 7)),
  cuut=
    cut(x=vectorv, breaks=c(3, 5, 7)),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# Calculate VTI percentage returns
returns <- na.omit(rutils::etfenv$returns$VTI)
# Plot histogram
x11(width=6, height=5)
par(mar=c(1, 1, 1, 1), oma=c(2, 2, 2, 0))
madv <- mad(returns)
histp <- hist(returns, breaks=100,
  main="", xlim=c(-5*madv, 5*madv),
  xlab="", ylab="", freq=FALSE)

# Draw kernel density of histogram
lines(density(returns), col="red", lwd=2)
# Add density of normal distribution
curve(expr=dnorm(x, mean=mean(returns), sd=sd(returns)),
add=TRUE, type="l", lwd=2, col="blue")
title(main="VTI Return Distribution", line=0)
# Add legend
legend("topright", inset=0.05, cex=0.8, title=NULL,
  leg=c("VTI", "Normal"), bty="n",
  lwd=6, bg="white", col=c("red", "blue"))
# Total area under histogram
sum(diff(histp$breaks) * histp$density)

matrixv <- matrix(5:10, nrow=2, ncol=3)  # Create a matrix
matrixv  # By default matrices are constructed column-wise
# Create a matrix row-wise
matrix(5:10, nrow=2, byrow=TRUE)
matrixv[2, 3]  # Extract third element from second row
matrixv[2, ]  # Extract second row
matrixv[, 3]  # Extract third column
matrixv[, c(1,3)]  # Extract first and third column
matrixv[, -2]  # Remove second column
# Subset whole matrix
matrixv[] <- 0
# Get the number of rows or columns
nrow(vectorv); ncol(vectorv)
NROW(vectorv); NCOL(vectorv)
nrow(matrixv); ncol(matrixv)
NROW(matrixv); NCOL(matrixv)

attributes(matrixv)  # Get matrix attributes
dim(matrixv)  # Get dimension attribute
class(matrixv)  # Get class attribute
rownames(matrixv) <- c("row1", "row2")  # Rownames attribute
colnames(matrixv) <- c("col1", "col2", "col3")  # Colnames attribute
matrixv
matrixv["row2", "col3"]  # Third element from second row
names(matrixv)  # Get the names attribute
dimnames(matrixv)  # Get dimnames attribute
attributes(matrixv)  # Get matrix attributes

matrixv  # matrix with column names
matrixv[1, ]  # Subset rows by index
matrixv[, "col1"]  # Subset columns by name
matrixv[, c(TRUE, FALSE, TRUE)]  # Subset columns Boolean vector
matrixv[1, ]  # Subsetting can produce a vector!
class(matrixv); class(matrixv[1, ])
is.matrix(matrixv[1, ]); is.vector(matrixv[1, ])
matrixv[1, , drop=FALSE]  # Drop=FALSE preserves matrix
class(matrixv[1, , drop=FALSE])
is.matrix(matrixv[1, , drop=FALSE]); is.vector(matrixv[1, , drop=FALSE])

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
  as_list=as.list(as.data.frame.matrix(matrixv)),
  applyloop=lapply(seq_along(matrixv[1, ]),
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
