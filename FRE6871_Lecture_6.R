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

dframe <- data.frame(  # Create a data frame
                type=c("rose", "daisy", "tulip"),
                color=c("red", "white", "yellow"),
                price=c(1.5, 0.5, 1.0)
              )  # end data.frame
dframe
dim(dframe)  # Get dimension attribute
colnames(dframe)  # Get the colnames attribute
rownames(dframe)  # Get the rownames attribute
class(dframe)  # Get object class
typeof(dframe)  # Data frames are listv
is.data.frame(dframe)

class(dframe$type)  # Get column class
class(dframe$price)  # Get column class

dframe[, 3]  # Extract third column as vector
dframe[[3]]  # Extract third column as vector
dframe[3]  # Extract third column as data frame
dframe[, 3, drop=FALSE]  # Extract third column as data frame
dframe[[3]][2]  # Second element from third column
dframe$price[2]  # Second element from "price" column
is.data.frame(dframe[[3]]); is.vector(dframe[[3]])
dframe[2, ]  # Extract second row
dframe[2, ][3]  # Third element from second column
dframe[2, 3]  # Third element from second column
unlist(dframe[2, ])  # Coerce to vector
is.data.frame(dframe[2, ]); is.vector(dframe[2, ])

dframe <- data.frame(  # Create a data frame
                type=c("rose", "daisy", "tulip"),
                color=c("red", "white", "yellow"),
                price=c(1.5, 0.5, 1.0),
                row.names=c("flower1", "flower2", "flower3")
              )  # end data.frame
dframe
class(dframe$type)  # Get column class
class(dframe$price)  # Get column class
# Set option to not coerce character vectors to factors
options("stringsAsFactors")
default.stringsAsFactors()
options(stringsAsFactors=FALSE)

str(dframe)  # Display the object structure
dim(cars)  # The cars data frame has 50 rows
head(cars, n=5)  # Get first five rows
tail(cars, n=5)  # Get last five rows

# Create a named vector of student scores
scorev <- sample(round(runif(5, min=1, max=10), digits=2))
names(scorev) <- c("Angie", "Chris", "Suzie", "Matt", "Liz")
# Sort the vector into ascending order
sort(scorev)
# Calculate index to sort into ascending order
order(scorev)
# Sort the vector into ascending order
scorev[order(scorev)]
# Calculate the sorted (ordered) vector
sortv <- scorev[order(scorev)]
# Calculate index to sort into unsorted (original) order
order(order(scorev))
sortv[order(order(scorev))]
scorev
# Examples for sort() with ties
order(c(2, 1:4))  # There's a tie
order(c(2, 1:4), 1:5)  # There's a tie

# Create a vector of student ranks
rankv <- c("fifth", "fourth", "third", "second", "first")
# Reverse sort the student ranks according to students
rankv[order(order(scorev))]
# Create a data frame of students and their ranks
rosterdf <- data.frame(score=scorev, 
  rank=rankv[order(order(scorev))])
rosterdf
# Permutation index on price column
order(dframe$price)
# Sort dframe on price column
dframe[order(dframe$price), ]
# Sort dframe on color column
dframe[order(dframe$color), ]


as.matrix(dframe)
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
  as_dframe_matrix=
    as.data.frame.matrix(matrixv),
  as_dframe=as.data.frame(matrixv),
  dframe=data.frame(matrixv),
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
aggregate(x=(mpg ~ cyl), data=mtcars, FUN=mean)
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
aggregate(x=(Sector ~ Industry),
  data=panel_data, FUN=function(x) NROW(unique(x)))
# Industries and the Sector to which they belong
aggregate(x=(Sector ~ Industry), data=panel_data,
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
sec_ind2 <- aggregate(x=(Industry ~ Sector),
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

# ?options  # Get info on global options
getOption("warn")  # Global option for "warn"
options("warn")  # Global option for "warn"
getOption("error")  # Global option for "error"
sqrt_safe <- function(input) {
# Returns its argument
  if (input<0) {
    warning("sqrt_safe: input is negative")
    NULL  # Return NULL for negative argument
  } else {
    sqrt(input)
  }  # end if
}  # end sqrt_safe
sqrt_safe(5)
sqrt_safe(-1)
options(warn=-1)
sqrt_safe(-1)
options(warn=0)
sqrt_safe()
options(warn=1)
sqrt_safe()
options(warn=3)
sqrt_safe()

# Function valido validates its arguments
valido <- function(input=NULL) {
# Check if argument is valid and return double
  if (is.null(input)) {
    return("valido: input is missing")
  } else if (is.numeric(input)) {
    2*input
  } else cat("valido: input not numeric")
}  # end valido
valido(3)
valido("a")
valido()
# valido validates arguments using missing()
valido <- function(input) {
# Check if argument is valid and return double
  if (missing(input)) {
    return("valido: input is missing")
  } else if (is.numeric(input)) {
    2*input
  } else cat("valido: input is not numeric")
}  # end valido
valido(3)
valido("a")
valido()

# valido() validates its arguments and assertions
valido <- function(input) {
# Check if argument is valid and return double
  if (missing(input)) {
    stop("valido: input is missing")
  } else if (!is.numeric(input)) {
    cat("input =", input, "\n")
    stop("valido: input is not numeric")
  } else 2*input
}  # end valido
valido(3)
valido("a")
valido()

# Print the call stack
traceback()

valido <- function(input) {
# Check argument using long form '&&' operator
  stopifnot(!missing(input) && is.numeric(input))
  2*input
}  # end valido
valido(3)
valido()
valido("a")
valido <- function(input) {
# Check argument using logical '&' operator
  stopifnot(!missing(input) & is.numeric(input))
  2*input
}  # end valido
valido()
valido("a")

# sumtwo() returns the sum of its two arguments
sumtwo <- function(input1, input2) {  # Even more robust
# Check if at least one argument is not missing
  stopifnot(!missing(input1) &&
        !missing(input2))
# Check if arguments are valid and return sum
  if (is.numeric(input1) && is.numeric(input2)) {
    input1 + input2  # Both valid
  } else if (is.numeric(input1)) {
    cat("input2 is not numeric\n")
    input1  # input1 is valid
  } else if (is.numeric(input2)) {
    cat("input1 is not numeric\n")
    input2  # input2 is valid
  } else {
    stop("none of the arguments are numeric")
  }
}  # end sumtwo
sumtwo(1, 2)
sumtwo(5, 'a')
sumtwo('a', 5)
sumtwo('a', 'b')
sumtwo()

# Flag "valido" for debugging
debug(valido)
# Calling "valido" starts debugger
valido(3)
# unflag "valido" for debugging
undebug(valido)

valido <- function(input) {
  browser()  # Pause and invoke debugger
# Check argument using long form '&&' operator
  stopifnot(!missing(input) && is.numeric(input))
  2*input
}  # end valido
valido()  # Invokes debugger
options("error")  # Show default NULL "error" option
options(error=recover)  # Set "error" option to "recover"
options(error=NULL)  # Set back to default "error" option

str(tryCatch)  # Get arguments of tryCatch()
tryCatch(  # Without error handler
  {  # Evaluate expressions
    numv <- 101  # Assign
    stop('my error')  # Produce error
  },
  finally=print(paste("numv=", numv))
)  # end tryCatch

tryCatch(  # With error handler
  {  # Evaluate expressions
    numv <- 101  # Assign
    stop('my error')  # Produce error
  },
  # Error handler captures error condition
  error=function(error_cond) {
    print(paste("error handler: ", error_cond))
  },  # end error handler
  # Warning handler captures warning condition
  warning=function(warning_cond) {
    print(paste("warning handler: ", warning_cond))
  },  # end warning handler
  finally=print(paste("numv=", numv))
)  # end tryCatch

# Apply loop without tryCatch
apply(matrix(1:5), 1, function(numv) {  # Anonymous function
    stopifnot(!(numv = 3))  # Check for error
    # Broadcast message to console
    cat("(cat) numv =", numv, "\n")
    # Return a value
    paste("(return) numv =", numv)
  }  # end anonymous function
)  # end apply

# Apply loop with tryCatch
apply(as.matrix(1:5), 1, function(numv) {  # Anonymous function
    tryCatch(  # With error handler
{  # Body
  stopifnot(numv != 3)  # Check for error
  # Broadcast message to console
  cat("(cat) numv =", numv, "\t")
  # Return a value
  paste("(return) numv =", numv)
},
# Error handler captures error condition
error=function(error_cond)
  paste("handler: ", error_cond),
finally=print(paste("(finally) numv =", numv))
    )  # end tryCatch
  }  # end anonymous function
)  # end apply

cat("Enter\ttab")  # Cat() interretsp backslash escape sequences
print("Enter\ttab")

my_text <- print("hello")
my_text  # Print() returns its argument

# Create string
my_text <- "Title: My Text\nSome numbers: 1,2,3,...\nRprofile files contain code executed at R startup,\n"

cat(my_text, file="mytext.txt")  # Write to text file

cat("Title: My Text",  # Write several lines to text file
    "Some numbers: 1,2,3,...",
    "Rprofile files contain code executed at R startup,",
    file="mytext.txt", sep="\n")

save(my_text, file="mytext.RData")  # Write to binary file

print(pi)
print(pi, digits=10)
getOption("digits")
foo <- 12
bar <- "months"
sprintf("There are %i %s in the year", foo, bar)

# Read text from file
scan(file="mytext.txt", what=character(), sep="\n")

# Read lines from file
readLines(con="mytext.txt")

# Read text from console
input <- readline("Enter a number: ")
class(input)
# Coerce to numeric
input <- as.numeric(input)

# Read text from file and display in editor:
# file.show("mytext.txt")
# file.show("mytext.txt", pager="")

setwd("/Users/jerzy/Develop/lecture_slides/data")
dframe <- data.frame(type=c("rose", "daisy", "tulip"),
  color=c("red", "white", "yellow"),
  price=c(1.5, 0.5, 1.0),
  row.names=c("flower1", "flower2", "flower3"))  # end data.frame
matrixv <- matrix(sample(1:12), ncol=3,
  dimnames=list(NULL, c("col1", "col2", "col3")))
rownames(matrixv) <- paste("row", 1:NROW(matrixv), sep="")
# Write data frame to text file, and then read it back
write.table(dframe, file="florist.txt")
readf <- read.table(file="florist.txt")
readf  # A data frame

# Write matrix to text file, and then read it back
write.table(matrixv, file="matrix.txt")
readmat <- read.table(file="matrix.txt")
readmat  # write.table() coerced matrix to data frame
class(readmat)
# Coerce from data frame back to matrix
readmat <- as.matrix(readmat)
class(readmat)

# Create a data frame
dframe <- data.frame(small=c(3, 5), medium=c(9, 11), large=c(15, 13))

# Launch spreadsheet-style data editor
dframe <- edit(dframe)

# Copy the data frame to clipboard
write.table(x=dframe, file="clipboard", sep="\t")

# Wrapper function for copying data frame from R into clipboard
# by default, data is tab delimited, with a header
write_clip <- function(data, row.names=FALSE, col.names=TRUE, ...) {
  write.table(x=data, file="clipboard", sep="\t",
      row.names=row.names, col.names=col.names, ...)
}  # end write_clip

write_clip(data=dframe)

# Wrapper function for copying data frame from clipboard into R
# by default, data is tab delimited, with a header
read_clip <- function(file="clipboard", sep="\t", header=TRUE, ...) {
  read.table(file=file, sep=sep, header=header, ...)
}  # end read_clip

dframe <- read.table("clipboard", header=TRUE)
dframe <- read_clip()

# Write data frame to CSV file, and then read it back
write.csv(dframe, file="florist.csv")
readf <- read.csv(file="florist.csv")
readf  # the row names are read in as extra column
# Restore row names
rownames(readf) <- readf[, 1]
readf <- readf[, -1]  # Remove extra column
readf
# Read data frame, with row names from first column
readf <- read.csv(file="florist.csv", row.names=1)
readf

# Write data frame to CSV file, without row names
write.csv(dframe, row.names=FALSE, file="florist.csv")
readf <- read.csv(file="florist.csv")
readf  # A data frame without row names

# Open a read connection to a file
con_read = file("/Users/jerzy/Develop/lecture_slides/data/etf_prices_crsp.csv", "r")
# Read the first 10 rows
data10 <- read.csv(con_read, nrows=10)
# Read another 10 rows
data20 <- read.csv(con_read, nrows=10, header=FALSE)
colnames(data20) <- colnames(data10)
# Close the connection to the file
close(con_read)
# Open a read connection to a file
con_read = file("/Users/jerzy/Develop/lecture_slides/data/etf_prices_crsp.csv", "r")
# Read the first 1000 rows
data10 <- read.csv(con_read, nrows=1e3)
colnamev <- colnames(data10)
# Write to a file
countv <- 1
write.csv(data10, paste0("/Users/jerzy/Develop/data/temp/etf_prices_", countv, ".csv"))
# Read remaining rows in a loop 10 rows at a time
# Can produce error without getting to end of file
while (isOpen(con_read)) {
  datav <- read.csv(con_read, nrows=1e3)
  colnames(datav) <- colnamev
  write.csv(datav, paste0("/Users/jerzy/Develop/data/temp/etf_prices_", countv, ".csv"))
  countv <- countv + 1
}  # end while

# Write matrix to csv file, and then read it back
write.csv(matrixv, file="matrix.csv")
readmat <- read.csv(file="matrix.csv", row.names=1)
readmat  # Read.csv() reads matrix as data frame
class(readmat)
readmat <- as.matrix(readmat)  # Coerce to matrix
identical(matrixv, readmat)
write.csv(matrixv, row.names=FALSE,
    file="matrix_ex_rows.csv")
readmat <- read.csv(file="matrix_ex_rows.csv")
readmat <- as.matrix(readmat)
readmat  # A matrix without row names

setwd("/Users/jerzy/Develop/lecture_slides/data")
library(MASS)  # Load package "MASS"
# Write to CSV file by row - it's very SLOW!!!
MASS::write.matrix(matrixv, file="matrix.csv", sep=",")
# Read using scan() and skip first line with colnames
readmat <- scan(file="matrix.csv", sep=",", skip=1,
  what=numeric())
# Read colnames
colnamev <- readLines(con="matrix.csv", n=1)
colnamev  # this is a string!
# Convert to char vector
colnamev <- strsplit(colnamev, split=",")[[1]]
readmat  # readmat is a vector, not matrix!
# Coerce by row to matrix
readmat <- matrix(readmat, ncol=NROW(colnamev), byrow=TRUE)
# Restore colnames
colnames(readmat) <- colnamev
readmat
# Scan() is a little faster than read.csv()
library(microbenchmark)
summary(microbenchmark(
  read_csv=read.csv("matrix.csv"),
  scan=scan(file="matrix.csv", sep=",",
    skip=1, what=numeric()),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# Read data from a csv file, including row names
matrixv <- read.csv(file="matrix_bad.csv", row.names=1)
matrixv
class(matrixv)
# Columns with bad data are character or factor
sapply(matrixv, class)
# Coerce character column to numeric
matrixv$col2 <- as.numeric(matrixv$col2)
# Or
# Copy row names
rownames <- row.names(matrixv)
# sapply loop over columns and coerce to numeric
matrixv <- sapply(matrixv, as.numeric)
# Restore row names
row.names(matrixv) <- rownames
# Replace NAs with zero
matrixv[is.na(matrixv)] <- 0
# matrix without NAs
matrixv

setwd("/Users/jerzy/Develop/lecture_slides/data")
rm(list=ls())
set.seed(1121)  # Reset random number generator
library(zoo)  # Load package zoo
# Create zoo with Date index
dates <- seq(from=as.Date("2013-06-15"), by="day",
        length.out=100)
pricev <- zoo(rnorm(NROW(dates)), order.by=dates)
head(pricev, 3)
# Write zoo series to text file, and then read it back
write.zoo(pricev, file="pricev.txt")
pricezoo <- read.zoo("pricev.txt")  # Read it back
all.equal(pricezoo, pricev)
# Perform the same using write.table() and read.table()
# First coerce pricev into data frame
dframe <- as.data.frame(pricev)
dframe <- cbind(dates, dframe)
# Write pricev to text file using write.table
write.table(dframe, file="pricev.txt",
      row.names=FALSE, col.names=FALSE)
# Read data frame from file
pricezoo <- read.table(file="pricev.txt")
sapply(pricezoo, class)  # A data frame
# Coerce data frame into pricev
pricezoo <- zoo::zoo(
  drop(as.matrix(pricezoo[, -1])),
  order.by=as.Date(pricezoo[, 1]))
all.equal(pricezoo, pricev)

library(zoo)  # Load package zoo
# Write zoo series to CSV file, and then read it back
write.zoo(pricev, file="pricev.csv",
    sep=",", col.names=TRUE)
pricezoo <- read.zoo(file="pricev.csv",
  header=TRUE, sep=",", drop=FALSE)
all.equal(pricev, drop(pricezoo))

set.seed(1121)  # Reset random number generator
# Create zoo with POSIXct date-time index
dates <- seq(from=as.POSIXct("2013-06-15"),
        by="hour", length.out=100)
pricev <- zoo(rnorm(NROW(dates)), order.by=dates)
head(pricev, 3)
# Write zoo series to CSV file, and then read it back
write.zoo(pricev, file="pricev.csv",
    sep=",", col.names=TRUE)
# Read from CSV file using read.csv.zoo()
pricezoo <- read.csv.zoo(file="pricev.csv")
all.equal(pricev, pricezoo)
# Coerce to xts series
xtsv <- xts::as.xts(pricezoo)
class(xtsv); head(xtsv, 3)
# Coerce zoo series into data frame with custom date format
dframe <- as.data.frame(pricev)
dframe <- cbind(format(dates, "%m-%d-%Y %H:%M:%S"), dframe)
head(dframe, 3)
# Write zoo series to csv file using write.table
write.table(dframe, file="pricev.csv",
      sep=",", row.names=FALSE, col.names=FALSE)
# Read from CSV file using read.csv.zoo()
pricezoo <- read.zoo(file="pricev.csv",
  header=FALSE, sep=",", FUN=as.POSIXct,
  format="%m-%d-%Y %H:%M:%S", tz="America/New_York")
# Or using read.csv.zoo()
pricezoo <- read.csv.zoo(file="pricev.csv", header=FALSE,
  format="%m-%d-%Y %H:%M:%S", tz="America/New_York")
head(pricezoo, 3)
all.equal(pricev, pricezoo)

# Read time series from CSV file, with numeric date-time
datazoo <- read.table(file="/Users/jerzy/Develop/lecture_slides/data/es_ohlc.csv",
  header=TRUE, sep=",")
# A data frame
class(datazoo)
sapply(datazoo, class)
# Coerce data frame into xts series
datazoo <- xts::xts(as.matrix(datazoo[, -1]),
  order.by=as.POSIXct.numeric(datazoo[, 1], tz="America/New_York",
                        origin="1970-01-01"))
# An xts series
class(datazoo)
head(datazoo, 3)

rm(list=ls())  # Remove all objects
var1 <- 1; var2 <- 2
ls()  # List all objects
ls()[1]  # List first object
args(save)  # List arguments of save function
# Save "var1" to a binary file using string argument
save("var1", file="my_data.RData")
# Save "var1" to a binary file using object name
save(var1, file="my_data.RData")
# Save multiple objects
save(var1, var2, file="my_data.RData")
# Save first object in list by passing to "..." argument
# ls()[1] is not evaluated
save(ls()[1], file="my_data.RData")
# Save first object in list by passing to "list" argument
save(list=ls()[1], file="my_data.RData")
# Save whole list by passing it to the "list" argument
save(list=ls(), file="my_data.RData")

rm(list=ls())  # Remove all objects
# Load objects from file
loadobj <- load(file="my_data.RData")
loadobj  # vector of loaded objects
ls()  # List objects
# Assign new values to objects in  global environment
sapply(loadobj, function(symbol) {
  assign(symbol, runif(1), envir=globalenv())
})  # end sapply
ls()  # List objects
# Assign new values to objects using for loop
for (symbol in loadobj) {
  assign(symbol, runif(1))
}  # end for
ls()  # List objects
# Save vector of objects
save(list=loadobj, file="my_data.RData")
# Remove only loaded objects
rm(list=loadobj)
# Remove the object "loadobj"
rm(loadobj)

sink("sinkdata.txt")# Redirect text output to file

cat("Redirect text output from R\n")
print(runif(10))
cat("\nEnd data\nbye\n")

sink()  # turn redirect off

pdf("Rgraph.pdf", width=7, height=4)  # Redirect graphics to pdf file

cat("Redirect data from R into pdf file\n")
myvar <- seq(-2*pi, 2*pi, len=100)
plot(x=myvar, y=sin(myvar), main="Sine wave",
   xlab="", ylab="", type="l", lwd=2, col="red")
cat("\nEnd data\nbye\n")

dev.off()  # turn pdf output off

png("r_plot.png")  # Redirect graphics output to png file

cat("Redirect graphics from R into png file\n")
plot(x=myvar, y=sin(myvar), main="Sine wave",
 xlab="", ylab="", type="l", lwd=2, col="red")
cat("\nEnd data\nbye\n")

dev.off()  # turn png output off
