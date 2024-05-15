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
# Set option to not coerce character vectors to factors - that was old default
options("stringsAsFactors")
options(stringsAsFactors=FALSE)
options("stringsAsFactors")
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
vecv <- sample(9)
matrix(vecv, ncol=3)
as.matrix(vecv, ncol=3)
matv <- matrix(5:10, nrow=2, ncol=3)  # Create a matrix
rownames(matv) <- c("row1", "row2")  # Rownames attribute
colnames(matv) <- c("col1", "col2", "col3")  # Colnames attribute
library(microbenchmark)
# Call method instead of generic function
as.data.frame.matrix(matv)
# A few methods for generic function as.data.frame()
sample(methods(as.data.frame), size=4)
# Function method is faster than generic function
summary(microbenchmark(
  as_dframem=as.data.frame.matrix(matv),
  as_dframe=as.data.frame(matv),
  dframe=data.frame(matv),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
library(microbenchmark)
# lapply is faster than coercion function
summary(microbenchmark(
  aslist=as.list(as.data.frame.matrix(matv)),
  lapply=lapply(seq_along(matv[1, ]),
     function(indeks) matv[, indeks]),
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
vecv <- sample(22)
vecv[sample(NROW(vecv), 4)] <- NA
# Replace NA values with the most recent non-NA values
zoo::na.locf(vecv)
# Remove rows containing NAs
goodair <- airquality[complete.cases(airquality), ]
dim(goodair)
# NAs removed
head(goodair)
# Another way of removing NAs
freshair <- na.omit(airquality)
all.equal(freshair, goodair, check.attributes=FALSE)
# Replace NAs
goodair <- zoo::na.locf(airquality)
dim(goodair)
# NAs replaced
head(goodair)
# Replace NAs in xts time series
library(rutils)  # load package rutils
pricev <- rutils::etfenv$prices[, 1]
head(pricev, 3)
sum(is.na(pricev))
pricez <- zoo::na.locf(pricev, fromLast=TRUE)
pricex <- xts:::na.locf.xts(pricev, fromLast=TRUE)
all.equal(pricez, pricex, check.attributes=FALSE)
head(pricex, 3)
library(microbenchmark)
summary(microbenchmark(
  zoo=zoo::na.locf(pricev, fromLast=TRUE),
  xts=xts:::na.locf.xts(pricev, fromLast=TRUE),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# NULL values have no mode or type
c(mode(NULL), mode(NA))
c(typeof(NULL), typeof(NA))
c(NROW(NULL), NROW(NA))
# Check for NULL values
is.null(NULL)
# NULL values are ignored when combined into a vector
c(1, 2, NULL, 4, 5)
# But NA value isn't ignored
c(1, 2, NA, 4, 5)
# Vectors can be initialized to NULL
vecv <- NULL
is.null(vecv)
# Grow the vector in a loop - very bad code!!!
for (indeks in 1:5)
  vecv <- c(vecv, indeks)
# Initialize empty vector
vecv <- numeric()
# Grow the vector in a loop - very bad code!!!
for (indeks in 1:5)
  vecv <- c(vecv, indeks)
# Allocate vector
vecv <- numeric(5)
# Assign to vector in a loop - good code
for (indeks in 1:5)
  vecv[indeks] <- runif(1)
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
matv <- do.call(rbind, list_vectors)
dim(matv)
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
identical(matv, do_call_rbind(list_vectors))
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
splitiris <- split(iris, iris$Species)
str(splitiris, max.confl=1)
names(splitiris)
dim(splitiris$setosa)
head(splitiris$setosa, 2)
all.equal(setosa, splitiris$setosa)
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
# Aggregate multiple columns using formula syntax
aggregate(x=(cbind(mpg, hp) ~ cyl), data=mtcars, FUN=mean)
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
cardata <- sapply(split_cars, function(x) {
  c(mean=mean(x$mpg), max=max(x$mpg), min=min(x$mpg))
}  # end anonymous function
)  # end sapply
cardata  # sapply() produces a matrix
# Now same using lapply
cardata <- lapply(split_cars, function(x) {
  c(mean=mean(x$mpg), max=max(x$mpg), min=min(x$mpg))
}  # end anonymous function
)  # end sapply
is.list(cardata)  # lapply produces a list
# do.call flattens list into a matrix
do.call(cbind, cardata)
# Download CRSPpanel.txt from the NYU share drive
# Read the file using read.table() with header and sep arguments
paneld <- read.table(file="/Users/jerzy/Develop/lecture_slides/data/CRSPpanel.txt",
                   header=TRUE, sep="\t")
paneld[1:5, 1:5]
attach(paneld)
# Split paneld based on Industry column
panelds <- split(paneld, paneld$Industry)
# Number of companies in each Industry
sapply(panelds, NROW)
# Number of Sectors that each Industry belongs to
sapply(panelds, function(x) {
  NROW(unique(x$Sector))
})  # end sapply
# Or
aggregate(x=(Sector ~ Industry),
  data=paneld, FUN=function(x) NROW(unique(x)))
# Industries and the Sector to which they belong
aggregate(x=(Sector ~ Industry), data=paneld, FUN=unique)
# Or
aggregate(x=Sector, by=list(Industry), FUN=unique)
# Or
sapply(unique(Industry), function(x) {
  Sector[match(x, Industry)]
})  # end sapply
# Split paneld based on Sector column
panelds <- split(paneld, paneld$Sector)
# Number of companies in each Sector
sapply(panelds, NROW)
# Industries belonging to each Sector (jagged array)
secind <- sapply(panelds, function(x) unique(x$Industry))
# Or use aggregate() (returns a data frame)
secind2 <- aggregate(x=(Industry ~ Sector),
  data=paneld, FUN=function(x) unique(x))
# Or use aggregate() with "by" argument
secind2 <- aggregate(x=Industry, by=list(Sector),
    FUN=function(x) as.vector(unique(x)))
# Coerce secind2 into a jagged array
namev <- secind2[, 1]
secind2 <- secind2[, 2]
names(secind2) <- namev
all.equal(secind2, secind)
# Or use tapply() (returns an array)
secind2 <- tapply(X=Industry, INDEX=Sector, FUN=unique)
# Coerce secind2 into a jagged array
secind2 <- drop(as.matrix(secind2))
all.equal(secind2, secind)
# Average ROE in each Industry
sapply(split(ROE, Industry), mean)
# Average, min, and max ROE in each Industry
t(sapply(split(ROE, Industry), FUN=function(x)
  c(mean=mean(x), max=max(x), min=min(x))))
# Split paneld based on Industry column
panelds <- split(paneld, paneld$Industry)
# Average ROE and EPS in each Industry
t(sapply(panelds, FUN=function(x)
  c(mean_roe=mean(x$ROE),
    mean_eps=mean(x$EPS.EXCLUDE.EI))))
# Or: split paneld based on Industry column
panelds <- split(paneld[, c("ROE", "EPS.EXCLUDE.EI")],
  paneld$Industry)
# Average ROE and EPS in each Industry
t(sapply(panelds, FUN=function(x) sapply(x, mean)))
# Average ROE and EPS using aggregate()
aggregate(x=paneld[, c("ROE", "EPS.EXCLUDE.EI")],
  by=list(paneld$Industry), FUN=mean)
# ?options  # Get info on global options
getOption("warn")  # Global option for "warn"
options("warn")  # Global option for "warn"
getOption("error")  # Global option for "error"
calc_sqrt <- function(inputv) {
# Returns its argument
  if (inputv < 0) {
    warning("calc_sqrt: input is negative")
    NULL  # Return NULL for negative argument
  } else {
    sqrt(inputv)
  }  # end if
}  # end calc_sqrt
calc_sqrt(5)
calc_sqrt(-1)
options(warn=-1)
calc_sqrt(-1)
options(warn=0)
calc_sqrt()
options(warn=1)
calc_sqrt()
options(warn=3)
calc_sqrt()
# Function valido validates its arguments
valido <- function(inputv=NULL) {
# Check if argument is valid and return double
  if (is.null(inputv)) {
    return("valido: input is missing")
  } else if (is.numeric(inputv)) {
    2*inputv
  } else cat("valido: input not numeric")
}  # end valido
valido(3)
valido("a")
valido()
# valido validates arguments using missing()
valido <- function(inputv) {
# Check if argument is valid and return double
  if (missing(inputv)) {
    return("valido: input is missing")
  } else if (is.numeric(inputv)) {
    2*inputv
  } else cat("valido: input is not numeric")
}  # end valido
valido(3)
valido("a")
valido()
# valido() validates its arguments and assertions
valido <- function(inputv) {
# Check if argument is valid and return double
  if (missing(inputv)) {
    stop("valido: input is missing")
  } else if (!is.numeric(inputv)) {
    cat("input =", inputv, "\n")
    stop("valido: input is not numeric")
  } else 2*inputv
}  # end valido
valido(3)
valido("a")
valido()
# Print the call stack
traceback()
valido <- function(inputv) {
# Check argument using long form '&&' operator
  stopifnot(!missing(inputv) && is.numeric(inputv))
  2*inputv
}  # end valido
valido(3)
valido()
valido("a")
valido <- function(inputv) {
# Check argument using logical '&' operator
  stopifnot(!missing(inputv) & is.numeric(inputv))
  2*inputv
}  # end valido
valido()
valido("a")
# sumtwo() returns the sum of its two arguments
sumtwo <- function(input1, input2) {  # Even more robust
# Check if at least one argument is not missing
  stopifnot(!missing(input1) && !missing(input2))
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
valido <- function(inputv) {
  browser()  # Pause and invoke debugger
# Check argument using long form '&&' operator
  stopifnot(!missing(inputv) && is.numeric(inputv))
  2*inputv
}  # end valido
valido()  # Invokes debugger
options("error")  # Show default NULL "error" option
options(error=recover)  # Set "error" option to "recover"
options(error=NULL)  # Set back to default "error" option
str(tryCatch)  # Get arguments of tryCatch()
tryCatch(  # Without error handler
  {  # Evaluate expressions
    numv <- 101  # Assign
    stop("my error")  # Produce error
  },
  finally=print(paste0("numv = ", numv))
)  # end tryCatch
tryCatch(  # With error handler
  {  # Evaluate expressions
    numv <- 101  # Assign
    stop("my error")  # Produce error
  },
  # Error handler captures error condition
  error=function(msg) {
    print(paste0("Error handler: ", msg))
  },  # end error handler
  # Warning handler captures warning condition
  warning=function(msg) {
    print(paste0("Warning handler: ", msg))
  },  # end warning handler
  finally=print(paste0("numv = ", numv))
)  # end tryCatch
# Apply loop without tryCatch
apply(matrix(1:5), 1, function(numv) {  # Anonymous function
    stopifnot(!(numv = 3))  # Check for error
    # Broadcast message to console
    cat("(cat) numv = ", numv, "\n")
    # Return a value
    paste0("(return) numv = ", numv)
  }  # end anonymous function
)  # end apply
# Apply loop with tryCatch
apply(as.matrix(1:5), 1, function(numv) {  # Anonymous function
    tryCatch(  # With error handler
{  # Body
  stopifnot(numv != 3)  # Check for error
  # Broadcast message to console
  cat("(cat) numv = ", numv, "\t")
  # Return a value
  paste0("(return) numv = ", numv)
},
# Error handler captures error condition
error=function(msg)
  paste0("handler: ", msg),
finally=print(paste0("(finally) numv = ", numv))
    )  # end tryCatch
  }  # end anonymous function
)  # end apply
Sys.Date()  # Get today's date
as.Date(1e3)  # Coerce numeric into date object
datetime <- as.Date("2014-07-14")  # "%Y-%m-%d" or "%Y/%m/%d"
datetime
class(datetime)  # Date object
as.Date("07-14-2014", "%m-%d-%Y")  # Specify format
datetime + 20  # Add 20 days
# Extract internal representation to integer
as.numeric(datetime)
datep <- as.Date("07/14/2013", "%m/%d/%Y")
datep
# Difference between dates
difftime(datetime, datep, units="weeks")
weekdays(datetime)  # Get day of the week
# Coerce numeric into date-times
datetime <- 0
attributes(datetime) <- list(class="Date")
datetime  # "Date" object
structure(0, class="Date")  # "Date" object
structure(10000.25, class="Date")
datetime <- Sys.time()  # Get today's date and time
datetime
class(datetime)  # POSIXct object
# POSIXct stored as integer moment of time
as.numeric(datetime)
# Parse character string "%Y-%m-%d %H:%M:%S" to POSIXct object
datetime <- as.POSIXct("2014-07-14 13:30:10")
# Different time zones can have same clock time
as.POSIXct("2014-07-14 13:30:10", tz="America/New_York")
as.POSIXct("2014-07-14 13:30:10", tz="UTC")
# Format argument allows parsing different date-time string formats
as.POSIXct("07/14/2014 13:30:10", format="%m/%d/%Y %H:%M:%S",
     tz="America/New_York")
# Same moment of time corresponds to different clock times
timeny <- as.POSIXct("2014-07-14 13:30:10", tz="America/New_York")
timeldn <- as.POSIXct("2014-07-14 13:30:10", tz="UTC")
# Add five hours to POSIXct
timeny + 5*60*60
# Subtract POSIXct
timeny - timeldn
class(timeny - timeldn)
# Compare POSIXct
timeny > timeldn
# Create vector of POSIXct times during trading hours
timev <- seq(
  from=as.POSIXct("2014-07-14 09:30:00", tz="America/New_York"),
  to=as.POSIXct("2014-07-14 16:00:00", tz="America/New_York"),
  by="10 min")
head(timev, 3)
tail(timev, 3)
# POSIXct is stored as integer moment of time
datetimen <- as.numeric(datetime)
# Same moment of time corresponds to different clock times
as.POSIXct(datetimen, origin="1970-01-01", tz="America/New_York")
as.POSIXct(datetimen, origin="1970-01-01", tz="UTC")
# Same clock time corresponds to different moments of time
as.POSIXct("2014-07-14 13:30:10", tz="America/New_York") -
  as.POSIXct("2014-07-14 13:30:10", tz="UTC")
# Add 20 seconds to POSIXct
datetime + 20
datetime  # POSIXct date and time
# Parse POSIXct to string representing the clock time
format(datetime)
class(format(datetime))  # Character string
# Get clock times in different time zones
format(datetime, tz="America/New_York")
format(datetime, tz="UTC")
# Format with custom format strings
format(datetime, "%m/%Y")
format(datetime, "%m-%d-%Y %H hours")
# Trunc to hour
format(datetime, "%m-%d-%Y %H:00:00")
# Date converted to midnight UTC moment of time
as.POSIXct(Sys.Date())
as.POSIXct(as.numeric(as.POSIXct(Sys.Date())),
     origin="1970-01-01",
     tz="UTC")
# Parse character string "%Y-%m-%d %H:%M:%S" to POSIXlt object
datetime <- as.POSIXlt("2014-07-14 18:30:10")
datetime
class(datetime)  # POSIXlt object
as.POSIXct(datetime)  # Coerce to POSIXct object
# Extract internal list representation to vector
unlist(datetime)
datetime + 20  # Add 20 seconds
class(datetime + 20)  # Implicit coercion to POSIXct
trunc(datetime, units="hours")  # Truncate to closest hour
trunc(datetime, units="days")  # Truncate to closest day
methods(trunc)  # Trunc methods
trunc.POSIXt
# Set time-zone to UTC
Sys.setenv(TZ="UTC")
Sys.timezone()  # Get time-zone
Sys.time()  # Today's date and time
# Set time-zone back to New York
Sys.setenv(TZ="America/New_York")
Sys.time()  # Today's date and time
# Standard Time in effect
as.POSIXct("2013-03-09 11:00:00", tz="America/New_York")
# Daylight Savings Time in effect
as.POSIXct("2013-03-10 11:00:00", tz="America/New_York")
datetime <- Sys.time()  # Today's date and time
# Convert to character in different TZ
format(datetime, tz="America/New_York")
format(datetime, tz="UTC")
# Parse back to POSIXct
as.POSIXct(format(datetime, tz="America/New_York"))
# Difference between New_York time and UTC
as.POSIXct(format(Sys.time(), tz="UTC")) -
  as.POSIXct(format(Sys.time(), tz="America/New_York"))
library(lubridate)  # Load lubridate
# Parse strings into date-times
as.POSIXct("07-14-2014", format="%m-%d-%Y", tz="America/New_York")
datetime <- lubridate::mdy("07-14-2014", tz="America/New_York")
datetime
class(datetime)  # POSIXct object
lubridate::dmy("14.07.2014", tz="America/New_York")
# Parse numeric into date-times
as.POSIXct(as.character(14072014), format="%d%m%Y",
                  tz="America/New_York")
lubridate::dmy(14072014, tz="America/New_York")
# Parse decimal to date-times
lubridate::decimal_date(datetime)
lubridate::date_decimal(2014.25, tz="America/New_York")
date_decimal(decimal_date(datetime), tz="America/New_York")
library(lubridate)  # Load lubridate
datetime <- lubridate::ymd_hms(20140714142010,
               tz="America/New_York")
datetime
# Get same moment of time in "UTC" time zone
lubridate::with_tz(datetime, "UTC")
as.POSIXct(format(datetime, tz="UTC"), tz="UTC")
# Get same clock time in "UTC" time zone
lubridate::force_tz(datetime, "UTC")
as.POSIXct(format(datetime, tz="America/New_York"),
     tz="UTC")
# Same moment of time
datetime - with_tz(datetime, "UTC")
# Different moments of time
datetime - force_tz(datetime, "UTC")
library(lubridate)  # Load lubridate
# Daylight Savings Time handling periods vs durations
datetime <- as.POSIXct("2013-03-09 11:00:00", tz="America/New_York")
datetime
datetime + lubridate::ddays(1)  # Add duration
datetime + lubridate::days(1)  # Add period
leap_year(2012)  # Leap year
datetime <- lubridate::dmy(01012012, tz="America/New_York")
datetime
datetime + lubridate::dyears(1)  # Add duration
datetime + lubridate::years(1)  # Add period
library(lubridate)  # Load lubridate
datetime <- lubridate::ymd_hms(20140714142010, tz="America/New_York")
datetime
# Add periods to a date-time
c(datetime + lubridate::seconds(1), datetime + lubridate::minutes(1),
  datetime + lubridate::days(1), datetime + period(months=1))
# Create vectors of dates
datetime <- lubridate::ymd(20140714, tz="America/New_York")
datetime + 0:2 * period(months=1)  # Monthly dates
datetime + period(months=0:2)
datetime + 0:2 * period(months=2)  # bi-monthly dates
datetime + seq(0, 5, by=2) * period(months=1)
seq(datetime, length=3, by="2 months")
library(lubridate)  # Load lubridate
# Adding monthly periods can create invalid dates
datetime <- lubridate::ymd(20120131, tz="America/New_York")
datetime + 0:2 * period(months=1)
datetime + period(months=1)
datetime + period(months=2)
# Create vector of end-of-month dates
datetime %m-% lubridate::months(13:1)
library(zoo)  # Load zoo
library(RQuantLib)  # Load RQuantLib
# Create daily date series of class "Date"
dates <- Sys.Date() + -5:2
dates
# Create Boolean vector of business days
# Use RQuantLib calendar
is_busday <- RQuantLib::isBusinessDay(
  calendar="UnitedStates/GovernmentBond", dates)
# Create daily series of business days
bus_index <- dates[is_busday]
bus_index
library(zoo)  # Load package zoo
datetime <- Sys.Date()  # Create date series of class "Date"
dates <- datetime + 0:365  # Daily series over one year
head(dates, 4)  # Print first few dates
format(head(dates, 4), "%m/%d/%Y")  # Print first few dates
# Create daily date-time series of class "POSIXct"
dates <- seq(Sys.time(), by="days", length.out=365)
head(dates, 4)  # Print first few dates
format(head(dates, 4), "%m/%d/%Y %H:%M:%S")  # Print first few dates
# Create series of monthly dates of class "zoo"
monthly_index <- yearmon(2010+0:36/12)
head(monthly_index, 4)  # Print first few dates
# Create series of quarterly dates of class "zoo"
qrtly_index <- yearqtr(2010+0:16/4)
head(qrtly_index, 4)  # Print first few dates
# Parse quarterly "zoo" dates to POSIXct
Sys.setenv(TZ="UTC")
as.POSIXct(head(qrtly_index, 4))
