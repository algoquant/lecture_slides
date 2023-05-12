rm(list=ls())
TRUE | FALSE
TRUE | NA
vector1 <- c(2, 4, 6)
vector1 < 5  # Element-wise comparison
(vector1 < 5) & (vector1 > 3)
vector1[(vector1 < 5) & (vector1 > 3)]
vector2 <- c(-10, 0, 10)
vector1 < vector2
c(FALSE, TRUE, FALSE) & c(TRUE, TRUE, FALSE)
c(FALSE, TRUE, FALSE) | c(TRUE, TRUE, FALSE)
rm(list=ls())
c(FALSE, TRUE, FALSE) && c(TRUE, TRUE, FALSE)
c(FALSE, TRUE, FALSE) || c(TRUE, TRUE, FALSE)
echo_true <- function() {cat("echo_true\t"); TRUE}
echo_false <- function() {cat("echo_false\t"); FALSE}
echo_true() | echo_false()
echo_true() || echo_false()  # echo_false() isn't evaluated at all!
vectorv <- c(2, 4, 6)
# Works (does nothing) using '&&'
if (is.matrix(vectorv) && (vectorv[2, 3] > 0)) {
  vectorv[2, 3] <- 1
}
# No short-circuit so fails (produces an error)
if (is.matrix(vectorv) & (vectorv[2, 3] > 0)) {
  vectorv[2, 3] <- 1
}
?Arithmetic
4.7 * 0.5  # Multiplication
4.7 / 0.5  # Division
# Exponentiation
2**3
2^3
numv <- 2
numv==2
identical(numv, 2)
identical(numv, NULL)
# This doesn't work:
# numv==NULL
is.null(numv)
vectorv <- c(2, 4, 6)
vectorv==2
identical(vectorv, 2)
# numv is equal to "1.0" within machine precision
numv <- 1.0 + 2*sqrt(.Machine$double.eps)
all.equal(numv, 1.0)
# Info machine precision of computer R is running on
# ?.Machine
# Machine precision
.Machine$double.eps
vectorv <- sample(1e3, 1e3)
matrixv <- matrix(vectorv, ncol=4)
which(vectorv == 5)
match(5, vectorv)
# Equivalent but slower than above
(1:NROW(vectorv))[vectorv == 5]
which(vectorv < 5)
# Find indices of TRUE elements of Boolean matrix
which((matrixv == 5)|(matrixv == 6), arr.ind=TRUE)
# Equivalent but slower than above
arrayInd(which((matrixv == 5)|(matrixv == 6)),
   dim(matrixv), dimnames(matrixv))
# Find index of largest element
which.max(vectorv)
which(vectorv == max(vectorv))
# Find index of smallest element
which.min(vectorv)
# Benchmark match() versus which()
all.equal(match(5, vectorv), min(which(vectorv == 5)))
library(microbenchmark)
summary(microbenchmark(
  match=match(5, vectorv),
  which=min(which(vectorv == 5)),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Does 5 belong in vectorv?
5 %in% vectorv
match(5, vectorv, nomatch=0) > 0
# Does (-5) belong in vectorv?
(-5) %in% vectorv
c(5, -5) %in% vectorv
match(-5, vectorv)
# Equivalent to "5 %in% vectorv"
any(vectorv == 5)
# Equivalent to "(-5) %in% vectorv"
any(vectorv == (-5))
# Any negative values in vectorv?
any(vectorv < 0)
# Example of use in if() statement
if (any(vectorv < 2))
  cat("vector contains small values\n")
# Partial matching of strings
pmatch("med", c("mean", "median", "mode"))
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
# Make rightmost interval inclusive
findInterval(x=1:8, vec=c(3, 5, 7), rightmost.closed=TRUE)
numv1 <- 3  # "<-" and "=" are valid assignment operators
numv1
numv1 = 3
numv1
2<-3  # "<" operator confused with "<-"
2 < -3  # Add space or brackets to avoid confusion
# "=" assignment within argument list
median(x=1:10)
x  # x doesn't exist outside the function
# "<-" assignment within argument list
median(x <- 1:10)
x  # x exists outside the function
myvar <- 1  # Create new object
assign(x="myvar", value=2)  # Assign value to existing object
myvar
rm(myvar)  # Remove myvar
assign(x="myvar", value=3)  # Create new object from name
myvar
# Create new object in new environment
new_env <- new.env()  # Create new environment
assign("myvar", 3, envir=new_env)  # Assign value to name
ls(new_env)  # List objects in "new_env"
new_env$myvar
rm(list=ls())  # Delete all objects
symbol <- "myvar"  # Define symbol containing string "myvar"
assign(symbol, 1)  # Assign value to "myvar"
ls()
myvar
assign("symbol", "new_var")
assign(symbol, 1)  # Assign value to "new_var"
ls()
symbol <- 10
assign(symbol, 1)  # Can't assign to non-string
rm(list=ls())  # Delete all objects
# Create individual vectors from column names of EuStockMarkets
for (colname in colnames(EuStockMarkets)) {
# Assign column values to column names
  assign(colname, EuStockMarkets[, colname])
}  # end for
ls()
head(DAX)
head(EuStockMarkets[, "DAX"])
identical(DAX, EuStockMarkets[, "DAX"])
# Create new environment
test_env <- new.env()
# Pass string as name to create new object
assign("myvar1", 2, envir=test_env)
# Create new object using $ string referencing
test_env$myvar2 <- 1
# List objects in new environment
ls(test_env)
# Reference an object by name
test_env$myvar1
# Reference an object by string name using get
get("myvar1", envir=test_env)
# Retrieve and assign value to object
assign("myvar1",
       2*get("myvar1", envir=test_env),
       envir=test_env)
get("myvar1", envir=test_env)
# Return all objects in an environment
mget(ls(test_env), envir=test_env)
# Delete environment
rm(test_env)
rm(list=ls())  # Delete all objects
# Convert string to symbol
as.symbol("some_string")
# The "name" class is synonymous with a symbol
class(as.symbol("some_string"))
# Symbols are created during assignments
symbol <- 2
# Evaluate symbol (same as typing it)
eval(symbol)
# Convert string into a symbol and evaluate it
eval(as.symbol("symbol"))
# Convert string into unevaluated expression
expv <- parse(text="newv <- symbol")
expv
class(expv)
ls()
eval(expv)  # Evaluate expression
ls()  # Expression evaluation created new object
newv
# Create the expression "1+1"
quote(1+1)
# Evaluate the expression "1+1"
eval(quote(1+1))
# Create an expression containing several commands
expv <- quote({x <- 1; y <- 2; x+y})
expv
# Evaluate all the commands in the expression
eval(expv)
ls()
# Return an expression without evaluating it
newv <- 2*symbol
expv <- quote(symbol + newv)
expv
eval(expv)  # Evaluate expression
# Substitute objects in an expression
expv <- substitute(symbol + newv,
              env=list(symbol=1, newv=2))
expv
eval(expv)  # Evaluate expression
# Get_input() substitutes its formal argument with the actual argument
get_input <- function(input) {
  substitute(input)
}  # end get_input
myvar <- 2
get_input(myvar)
eval(get_input(myvar))
# Define symbol
myvar <- 10
# Convert symbol value into string
deparse(myvar)
# Convert symbol into string without evaluating it
deparse(quote(myvar))
# Substitute object with value from named list
symbol <- 2
deparse(substitute(symbol + myvar,
       env=list(myvar=2)))
# Create string with name of input argument
get_name <- function(input) {
  names(input) <- deparse(substitute(input))
  input
}  # end get_name
get_name(myvar)
rm(list=ls())
# Expressions enclosed in parenthesis are less ambiguous
-2:5
(-2):5
-(2:5)
# Expressions enclosed in parenthesis are less ambiguous
-2*3+5
-2*(3+5)
# Expressions can be separated by semicolons or by lines
{1+2; 2*3; 1:5}
# or
{1+2
2*3
1:5}
matrixv <- matrix(nr=3, nc=4)
matrixv <- 0
# Subset whole matrix
matrixv[] <- 0
# Parenthesis and braces require a little additional processing time
library(microbenchmark)
summary(microbenchmark(
  basep=sqrt(rnorm(10000)^2),
  parven=sqrt(((((rnorm(10000)^2))))),
  bra_ce=sqrt({{{{rnorm(10000)^2}}}}),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
rm(list=ls())
numv1 <- 1
if (numv1) {  # Numeric zero is FALSE, all other numbers are TRUE
  numv2 <- 4
} else if (numv1 == 0) {  # 'else if' together on same line
  numv2 <- 0
} else {  # 'else' together with curly braces
  numv2 <- -4
}  # end if
numv2
switch("a", a="aaahh", b="bee", c="see", d=2,
       "else this")
switch("c", a="aaahh", b="bee", c="see", d=2,
       "else this")
switch(3, a="aaahh", b="bee", c="see", d=2,
       "else this")
switch("cc", a="aaahh", b="bee", c="see", d=2,
       "else this")
# Measure of central tendency
centra_lity <- function(input,
    method=c("mean", "mean_narm", "median")) {
# validate "method" argument
  method <- match.arg(method)
  switch(method,
 mean=mean(input),
 mean_narm=mean(input, na.rm=TRUE),
 median=median(input))
}  # end centra_lity
myvar <- rnorm(100, mean=2)
centra_lity(myvar, "mean")
centra_lity(myvar, "mean_narm")
centra_lity(myvar, "median")
for (indeks in vectorv) {expvs}
rm(list=ls())
colorl <- list("red", "white", "blue")
# Loop over list
for (some_color in colorl) {
  print(some_color)
}  # end for
# Loop over vector
for (indeks in 1:3) {
  print(colorl[[indeks]])
}  # end for
# While loops require initialization
indeks <- 1
# While loop
while (indeks < 4) {
  print(colorl[[indeks]])
  indeks <- indeks + 1
}  # end while
vectorv <- integer(7)
# Loop over a vector and overwrite it
for (i in seq_along(vectorv)) {
  cat("Changing element:", i, "\n")
  vectorv[i] <- i^2
}  # end for
# Modifying vectorv inside sapply() has no effect
vectorv <- integer(7)
vectorv
sapply(seq_along(vectorv),
 function(i) {
   vectorv[i] <- i^2
 })  # end sapply
vectorv
# Super-assignment operator "<<-" allows modifying vectorv
sapply(seq_along(vectorv),
 function(i) {
   vectorv[i] <<- i^2 # "<<-" !!!
 })  # end sapply
vectorv
# sapply() loop returns vector of values
vectorv <- sapply(seq_along(vectorv), function(i) (i^2))
rm(list=ls())
# fib_seq <- numeric()  # zero length numeric vector
# Pre-allocate vector instead of "growing" it
fib_seq <- numeric(10)
fib_seq[1] <- 0  # Initialize
fib_seq[2] <- 1  # Initialize
for (i in 3:10) {  # Perform recurrence loop
  fib_seq[i] <- fib_seq[i-1] + fib_seq[i-2]
}  # end for
fib_seq
# Allocate character vector
character()
character(5)
is.character(character(5))
# Allocate integer vector
integer()
integer(5)
is.integer(integer(5))
is.numeric(integer(5))
# Allocate numeric vector
numeric()
numeric(5)
is.integer(numeric(5))
is.numeric(numeric(5))
# Allocate Boolean vector
vector()
vector(length=5)
# Allocate numeric vector
vector(length=5, mode="numeric")
is.null(vector())
# Allocate Boolean matrix
matrix()
is.null(matrix())
# Allocate integer matrix
matrix(NA_integer_, nrow=3, ncol=2)
is.integer(matrix(NA_integer_, nrow=3, ncol=2))
# Allocate numeric matrix
matrix(NA_real_, nrow=3, ncol=2)
is.numeric(matrix(NA_real_, nrow=3, ncol=2))
vectorv <- sample(1:9)
vectorv
vectorv < 5  # Element-wise comparison
vectorv == 5  # Element-wise comparison
matrixv <- matrix(vectorv, ncol=3)
matrixv
matrixv < 5  # Element-wise comparison
matrixv == 5  # Element-wise comparison
matrixv <- 1:6  # Create a vector
class(matrixv)  # Get its class
# Is it vector or matrix?
c(is.vector(matrixv), is.matrix(matrixv))
structure(matrixv, dim=c(2, 3))  # Matrix object
# Adding dimension attribute coerces into matrix
dim(matrixv) <- c(2, 3)
class(matrixv)  # Get its class
# Is it vector or matrix?
c(is.vector(matrixv), is.matrix(matrixv))
# Assign dimnames attribute
dimnames(matrixv) <- list(rows=c("row1", "row2"),
            columns=c("col1", "col2", "col3"))
matrixv
matrixv <- matrix(1:10, 2, 5)  # Create matrix
matrixv
# as.numeric strips dim attribute from matrix
as.numeric(matrixv)
# Explicitly coerce to "character"
matrixv <- as.character(matrixv)
c(typeof(matrixv), mode(matrixv), class(matrixv))
# Coercion converted matrix to vector
c(is.matrix(matrixv), is.vector(matrixv))
vector1 <- 1:3  # Define vector
vector2 <- 6:4  # Define vector
# Bind vectors into columns
cbind(vector1, vector2)
# Bind vectors into rows
rbind(vector1, vector2)
# Extend to four elements
vector2 <- c(vector2, 7)
# Recycling rule applied
cbind(vector1, vector2)
# Another example of recycling rule
1:6 + c(10, 20)
# Replicate a single element
rep("a", 5)
# Replicate the whole vector several times
rep(c("a", "b"), 5)
rep(c("a", "b"), times=5)
# Replicate the first element, then the second, etc.
rep(c("a", "b"), each=5)
# Replicate to specified length
rep(c("a", "b"), length.out=5)
# Define vector and matrix
vector1 <- c(2, 4, 3)
matrixv <- matrix(sample(1:12), ncol=3)
# Multiply columns of matrix by vector
vector1*matrixv
# Or
matrixv*vector1
# Multiply rows of matrix by vector
t(vector1*t(matrixv))
# Multiply rows of matrix by vector - transpose is very slow
matrixp <- lapply(1:NCOL(matrixv), 
  function(x) vector1[x]*matrixv[, x])
do.call(cbind, matrixp)
library(microbenchmark)
summary(microbenchmark(
  trans=t(vector1*t(matrixv)),
  lapp={
    matrixp <- lapply(1:NCOL(matrixv), function(x) vector1[x]*matrixv[, x])
    do.call(cbind, matrixp)
  },
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
vector1
vector2 <- 6:4  # Define vector
# Multiply two vectors element-by-element
vector1 * vector2
# Calculate inner product
vector1 %*% vector2
# Calculate inner product and drop dimensions
drop(vector1 %*% vector2)
# Multiply columns of matrix by vector
matrixv %*% vector1  # Single column matrix
drop(matrixv %*% vector1)  # vector
rowSums(t(vector1 * t(matrixv)))
# using rowSums() and t() is 10 times slower than %*%
library(microbenchmark)
summary(microbenchmark(
  inner=drop(matrixv %*% vector1),
  transp=rowSums(t(vector1 * t(matrixv))),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
library(microbenchmark)
# Multiply matrix by vector fails because dimensions aren't conformable
vector1 %*% matrixv
# Works after transpose
drop(vector1 %*% t(matrixv))
# Calculate inner product
crossprod(vector1, vector2)
# Create matrix and vector
matrixv <- matrix(1:3000, ncol=3)
tmatrixv <- t(matrixv)
vectorv <- 1:3
# crossprod() is slightly faster than "%*%" operator
summary(microbenchmark(
  cross_prod=crossprod(tmatrixv, vectorv),
  inner_prod=matrixv %*% vectorv,
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Define named vectors
vector1 <- sample(1:4)
names(vector1) <- paste0("row", 1:4, "=", vector1)
vector1
vector2 <- sample(1:3)
names(vector2) <- paste0("col", 1:3, "=", vector2)
vector2
# Calculate outer product of two vectors
matrixv <- outer(vector1, vector2)
matrixv
# Calculate vectorized function spanned over two vectors
matrixv <- outer(vector1, vector2,
           FUN=function(x1, x2) x2*sin(x1))
matrixv
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
namesv <- secind2[, 1]
secind2 <- secind2[, 2]
names(secind2) <- namesv
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
library(plyr)
one <- ozone[1, 1, ]
month <- ordered(rep(1:12, length72))
model <- rlm(one ~ month - 1)
deseas <- resid(model)
deseasf <- function(value) rlm(value ~ month - 1)
# For loops
models <- as.list(rep(NA, 24 * 24))
dim(models) <- c(24, 24)
deseas <- array(NA, c(24, 24, 72))
dimnames(deseas) <- dimnames(ozone)
for (i in seq_len(24)) {
for (j in seq_len(24)) {
mod <- deseasf(ozone[i, j, ])
models[[i, j]] <- mod
deseas[i, j, ] <- resid(mod)
}
}
# Apply functions
models <- apply(ozone, 1:2, deseasf)
resids_list <- lapply(models, resid)
resids <- unlist(resids_list)
dim(resids) <- c(72, 24, 24)
deseas <- aperm(resids, c(2, 3, 1))
dimnames(deseas) <- dimnames(ozone)
# InsectSprays dataset
head(InsectSprays)
# Split the count column by the spray column.
count_by_spray <- with(InsectSprays, split(count, spray))
# Next apply the statistic to each element of the list. Lets use the mean here.
mean_by_spray <- lapply(count_by_spray, mean)
# Finally combine the list as a vector
unlist(mean_by_spray)
# or in one line
sapply(count_by_spray, mean)
# Can also use the functions tapply(), aggregate() and by():
with(InsectSprays, tapply(count, spray, mean))
with(InsectSprays, by(count, spray, mean))
aggregate(count ~ spray, InsectSprays, mean)
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
