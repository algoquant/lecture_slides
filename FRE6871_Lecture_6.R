# Create a list with two elements
lis_t <- list(c("a", "b"), 1:4)
lis_t
c(typeof(lis_t), mode(lis_t), class(lis_t))
# Lists are also vectors
c(is.vector(lis_t), is.list(lis_t))
NROW(lis_t)
# Create named list
lis_t <- list(first=c("a", "b"), second=1:4)
lis_t
names(lis_t)
unlist(lis_t)
lis_t[2]  # Extract second element as sublist
lis_t[[2]]  # Extract second element
lis_t[[2]][3]  # Extract third element of second element
lis_t[[c(2, 3)]]  # Third element of second element
lis_t$second  # Extract second element
lis_t$s  # Extract second element - partial name matching
lis_t$second[3]  # Third element of second element
lis_t <- list()  # Empty list
lis_t$a <- 1
lis_t[2] <- 2
lis_t
names(lis_t)
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
typeof(data_frame)  # Data frames are lists
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
sort_ed <- score_s[order(score_s)]
# Calculate index to sort into unsorted (original) order
order(order(score_s))
sort_ed[order(order(score_s))]
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
vec_tor <- sample(9)
matrix(vec_tor, ncol=3)
as.matrix(vec_tor, ncol=3)
mat_rix <- matrix(5:10, nrow=2, ncol=3)  # Create a matrix
rownames(mat_rix) <- c("row1", "row2")  # Rownames attribute
colnames(mat_rix) <- c("col1", "col2", "col3")  # Colnames attribute
library(microbenchmark)
# Call method instead of generic function
as.data.frame.matrix(mat_rix)
# A few methods for generic function as.data.frame()
sample(methods(as.data.frame), size=4)
# Function method is faster than generic function
summary(microbenchmark(
  as_data_frame_matrix=as.data.frame.matrix(mat_rix),
  as_data_frame=as.data.frame(mat_rix),
  data_frame=data.frame(mat_rix),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
library(microbenchmark)
# lapply is faster than coercion function
summary(microbenchmark(
  as_list=as.list(as.data.frame.matrix(mat_rix)),
  l_apply=lapply(seq_along(mat_rix[1, ]),
     function(in_dex) mat_rix[, in_dex]),
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
da_ta <- c(1, 2, NA, 4, NA, 5)
da_ta
mean(da_ta)  # Returns NA, when NAs are input
mean(da_ta, na.rm=TRUE)  # remove NAs from input data
da_ta[!is.na(da_ta)]  # Delete the NA values
sum(!is.na(da_ta))  # Count non-NA values
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
vec_tor <- sample(22)
vec_tor[sample(NROW(vec_tor), 4)] <- NA
# Replace NA values with the most recent non-NA values
zoo::na.locf(vec_tor)
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
se_ries <- rutils::etf_env$price_s[, 1]
head(se_ries, 3)
sum(is.na(se_ries))
series_zoo <- as.xts(zoo::na.locf(se_ries, fromLast=TRUE))
series_xts <- xts:::na.locf.xts(se_ries, fromLast=TRUE)
all.equal(series_zoo, series_xts, check.attributes=FALSE)
head(series_xts, 3)
library(microbenchmark)
summary(microbenchmark(
  zoo=as.xts(zoo::na.locf(se_ries, fromLast=TRUE)),
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
vec_tor <- NULL
is.null(vec_tor)
# Grow the vector in a loop - very bad code!!!
for (in_dex in 1:5)
  vec_tor <- c(vec_tor, in_dex)
# Initialize empty vector
vec_tor <- numeric()
# Grow the vector in a loop - very bad code!!!
for (in_dex in 1:5)
  vec_tor <- c(vec_tor, in_dex)
# Allocate vector
vec_tor <- numeric(5)
# Assign to vector in a loop - good code
for (in_dex in 1:5)
  vec_tor[in_dex] <- runif(1)
# Create list of vectors
li_st <- lapply(1:3, function(x) sample(6))
# Bind list elements into matrix - doesn't work
rbind(li_st)
# Bind list elements into matrix - tedious
rbind(li_st[[1]], li_st[[2]], li_st[[3]])
# Bind list elements into matrix - works!
do.call(rbind, li_st)
# Create numeric list
li_st <- list(1, 2, 3, 4)
do.call(rbind, li_st)  # Returns single column matrix
do.call(cbind, li_st)  # Returns single row matrix
# Recycling rule applied
do.call(cbind, list(1:2, 3:5))
# NULL element is skipped
do.call(cbind, list(1, NULL, 3, 4))
# NA element isn't skipped
do.call(cbind, list(1, NA, 3, 4))
library(microbenchmark)
list_vectors <- lapply(1:5, rnorm, n=10)
mat_rix <- do.call(rbind, list_vectors)
dim(mat_rix)
do_call_rbind <- function(li_st) {
  while (NROW(li_st) > 1) {
# Index of odd list elements
    odd_index <- seq(from=1, to=NROW(li_st), by=2)
# Bind odd and even elements, and divide li_st by half
    li_st <- lapply(odd_index, function(in_dex) {
if (in_dex==NROW(li_st)) return(li_st[[in_dex]])
rbind(li_st[[in_dex]], li_st[[in_dex+1]])
    })  # end lapply
  }  # end while
# li_st has only one element - return it
  li_st[[1]]
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
# Split into separate data frames by hand
set_osa <- iris[iris$Species=="setosa", ]
versi_color <- iris[iris$Species=="versicolor", ]
virgin_ica <- iris[iris$Species=="virginica", ]
dim(set_osa)
head(set_osa, 2)
# Split iris into list based on Species
split_iris <- split(iris, iris$Species)
str(split_iris, max.level=1)
names(split_iris)
dim(split_iris$setosa)
head(split_iris$setosa, 2)
all.equal(set_osa, split_iris$setosa)
unique(mtcars$cyl)  # cyl has three unique values
# Split mpg column based on number of cylinders
split(mtcars$mpg, mtcars$cyl)
# Split mtcars data frame based on number of cylinders
split_cars <- split(mtcars, mtcars$cyl)
str(split_cars, max.level=1)
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
with(panel_data, sapply(levels(Industry),
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
name_s <- as.vector(sec_ind2[, 1])
sec_ind2 <- sec_ind2[, 2]
names(sec_ind2) <- name_s
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
sqrt_safe <- function(in_put) {
# Returns its argument
  if (in_put<0) {
    warning("sqrt_safe: in_put is negative")
    NULL  # Return NULL for negative argument
  } else {
    sqrt(in_put)
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
# Function vali_date validates its arguments
vali_date <- function(in_put=NULL) {
# Check if argument is valid and return double
  if (is.null(in_put)) {
    return("vali_date: in_put is missing")
  } else if (is.numeric(in_put)) {
    2*in_put
  } else cat("vali_date: in_put not numeric")
}  # end vali_date
vali_date(3)
vali_date("a")
vali_date()
# vali_date validates arguments using missing()
vali_date <- function(in_put) {
# Check if argument is valid and return double
  if (missing(in_put)) {
    return("vali_date: in_put is missing")
  } else if (is.numeric(in_put)) {
    2*in_put
  } else cat("vali_date: in_put is not numeric")
}  # end vali_date
vali_date(3)
vali_date("a")
vali_date()
# vali_date() validates its arguments and assertions
vali_date <- function(in_put) {
# Check if argument is valid and return double
  if (missing(in_put)) {
    stop("vali_date: in_put is missing")
  } else if (!is.numeric(in_put)) {
    cat("in_put =", in_put, "\n")
    stop("vali_date: in_put is not numeric")
  } else 2*in_put
}  # end vali_date
vali_date(3)
vali_date("a")
vali_date()
# Print the call stack
traceback()
vali_date <- function(in_put) {
# Check argument using long form '&&' operator
  stopifnot(!missing(in_put) && is.numeric(in_put))
  2*in_put
}  # end vali_date
vali_date(3)
vali_date()
vali_date("a")
vali_date <- function(in_put) {
# Check argument using logical '&' operator
  stopifnot(!missing(in_put) & is.numeric(in_put))
  2*in_put
}  # end vali_date
vali_date()
vali_date("a")
# sum_two() returns the sum of its two arguments
sum_two <- function(in_put1, in_put2) {  # Even more robust
# Check if at least one argument is not missing
  stopifnot(!missing(in_put1) &&
        !missing(in_put2))
# Check if arguments are valid and return sum
  if (is.numeric(in_put1) && is.numeric(in_put2)) {
    in_put1 + in_put2  # Both valid
  } else if (is.numeric(in_put1)) {
    cat("in_put2 is not numeric\n")
    in_put1  # in_put1 is valid
  } else if (is.numeric(in_put2)) {
    cat("in_put1 is not numeric\n")
    in_put2  # in_put2 is valid
  } else {
    stop("none of the arguments are numeric")
  }
}  # end sum_two
sum_two(1, 2)
sum_two(5, 'a')
sum_two('a', 5)
sum_two('a', 'b')
sum_two()
# Flag "vali_date" for debugging
debug(vali_date)
# Calling "vali_date" starts debugger
vali_date(3)
# unflag "vali_date" for debugging
undebug(vali_date)
vali_date <- function(in_put) {
  browser()  # Pause and invoke debugger
# Check argument using long form '&&' operator
  stopifnot(!missing(in_put) && is.numeric(in_put))
  2*in_put
}  # end vali_date
vali_date()  # Invokes debugger
options("error")  # Show default NULL "error" option
options(error=recover)  # Set "error" option to "recover"
options(error=NULL)  # Set back to default "error" option
str(tryCatch)  # Get arguments of tryCatch()
tryCatch(  # Without error handler
  {  # Evaluate expressions
    num_var <- 101  # Assign
    stop('my error')  # Produce error
  },
  finally=print(paste("num_var=", num_var))
)  # end tryCatch
tryCatch(  # With error handler
  {  # Evaluate expressions
    num_var <- 101  # Assign
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
  finally=print(paste("num_var=", num_var))
)  # end tryCatch
# Apply loop without tryCatch
apply(matrix(1:5), 1, function(num_var) {  # Anonymous function
    stopifnot(!(num_var = 3))  # Check for error
    # Broadcast message to console
    cat("(cat) num_var =", num_var, "\n")
    # Return a value
    paste("(return) num_var =", num_var)
  }  # end anonymous function
)  # end apply
# Apply loop with tryCatch
apply(as.matrix(1:5), 1, function(num_var) {  # Anonymous function
    tryCatch(  # With error handler
{  # Body
  stopifnot(num_var != 3)  # Check for error
  # Broadcast message to console
  cat("(cat) num_var =", num_var, "\t")
  # Return a value
  paste("(return) num_var =", num_var)
},
# Error handler captures error condition
error=function(error_cond)
  paste("handler: ", error_cond),
finally=print(paste("(finally) num_var =", num_var))
    )  # end tryCatch
  }  # end anonymous function
)  # end apply
