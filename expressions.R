rm(list=ls())
TRUE | FALSE
TRUE | NA
vec1 <- c(2, 4, 6)
vec1 < 5  # Element-wise comparison
(vec1 < 5) & (vec1 > 3)
vec1[(vec1 < 5) & (vec1 > 3)]
vec2 <- c(-10, 0, 10)
vec1 < vec2
c(FALSE, TRUE, FALSE) & c(TRUE, TRUE, FALSE)
c(FALSE, TRUE, FALSE) | c(TRUE, TRUE, FALSE)
rm(list=ls())
FALSE && TRUE
FALSE || TRUE
echo_true <- function() {cat("echo_true\t"); TRUE}
echo_false <- function() {cat("echo_false\t"); FALSE}
echo_true() | echo_false()
echo_true() || echo_false()  # echo_false() isn't evaluated at all!
vecv <- c(2, 4, 6)
# Works (does nothing) using '&&'
if (is.matrix(vecv) && (vecv[2, 3] > 0)) {
  vecv[2, 3] <- 1
}
# No short-circuit so fails (produces an error)
if (is.matrix(vecv) & (vecv[2, 3] > 0)) {
  vecv[2, 3] <- 1
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
vecv <- c(2, 4, 6)
vecv==2
identical(vecv, 2)
# numv is equal to "1.0" within machine precision
numv <- 1.0 + 2*sqrt(.Machine$double.eps)
all.equal(numv, 1.0)
# Info machine precision of computer R is running on
# ?.Machine
# Machine precision
.Machine$double.eps
vecv <- sample(1e3, 1e3)
matv <- matrix(vecv, ncol=4)
which(vecv == 5)
match(5, vecv)
# Equivalent but slower than above
(1:NROW(vecv))[vecv == 5]
which(vecv < 5)
# Find indices of TRUE elements of Boolean matrix
which((matv == 5)|(matv == 6), arr.ind=TRUE)
# Equivalent but slower than above
arrayInd(which((matv == 5)|(matv == 6)),
   dim(matv), dimnames(matv))
# Find index of largest element
which.max(vecv)
which(vecv == max(vecv))
# Find index of smallest element
which.min(vecv)
# Benchmark match() versus which()
all.equal(match(5, vecv), min(which(vecv == 5)))
library(microbenchmark)
summary(microbenchmark(
  match=match(5, vecv),
  which=min(which(vecv == 5)),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Does 5 belong in vecv?
5 %in% vecv
match(5, vecv, nomatch=0) > 0
# Does (-5) belong in vecv?
(-5) %in% vecv
c(5, -5) %in% vecv
match(-5, vecv)
# Equivalent to "5 %in% vecv"
any(vecv == 5)
# Equivalent to "(-5) %in% vecv"
any(vecv == (-5))
# Any negative values in vecv?
any(vecv < 0)
# Example of use in if() statement
if (any(vecv < 2))
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
envv <- new.env()  # Create new environment
assign("myvar", 3, envir=envv)  # Assign value to name
ls(envv)  # List objects in "envv"
envv$myvar
rm(list=ls())  # Delete all objects in workspace
symboln <- "myvar"  # Define symbol containing string "myvar"
assign(symboln, 1)  # Assign value to "myvar"
ls()
myvar
assign("symboln", "new_var")
assign(symboln, 1)  # Assign value to "new_var"
ls()
symboln <- 10
assign(symboln, 1)  # Can't assign to non-string
rm(list=ls())  # Delete all objects in workspace
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
envv <- new.env()
# Pass string as name to create new object
assign("myvar1", 2, envir=envv)
# Create new object using $ string referencing
envv$myvar2 <- 1
# List objects in new environment
ls(envv)
# Reference an object by name
envv$myvar1
# Reference an object by string name using get
get("myvar1", envir=envv)
# Retrieve and assign value to object
assign("myvar1",
       2*get("myvar1", envir=envv),
       envir=envv)
get("myvar1", envir=envv)
# Return all objects in an environment
mget(ls(envv), envir=envv)
# Delete environment
rm(envv)
rm(list=ls())  # Delete all objects in workspace
# Convert string to symbol
as.symbol("some_string")
# The "name" class is synonymous with a symbol
class(as.symbol("some_string"))
# Symbols are created during assignments
symboln <- 2
# Evaluate symbol (same as typing it)
eval(symboln)
# Convert string into a symbol and evaluate it
eval(as.symbol("symboln"))
# Convert string into unevaluated expression
expv <- parse(text="newv <- symboln")
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
newv <- 2*symboln
expv <- quote(symboln + newv)
expv
eval(expv)  # Evaluate expression
# Substitute objects in an expression
expv <- substitute(symboln + newv,
       env=list(symbol=1, newv=2))
expv
eval(expv)  # Evaluate expression
# Get_input() substitutes its formal argument with the actual argument
get_input <- function(inputv) {
  substitute(inputv)
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
symboln <- 2
deparse(substitute(symboln + myvar, env=list(myvar=2)))
# Create string with name of input argument
get_name <- function(inputv) {
  names(inputv) <- deparse(substitute(inputv))
  inputv
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
matv <- matrix(nr=3, nc=4)
matv <- 0
# Subset whole matrix
matv[] <- 0
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
centerf <- function(inputv, method=c("mean", "mean_narm", "median")) {
# validate "method" argument
  method <- match.arg(method)
  switch(method,
 mean=mean(inputv),
 mean_narm=mean(inputv, na.rm=TRUE),
 median=median(inputv))
}  # end centerf
myvar <- rnorm(100, mean=2)
centerf(myvar, "mean")
centerf(myvar, "mean_narm")
centerf(myvar, "median")
for (indeks in vecv) {expvs}
rm(list=ls())
colorl <- list("red", "white", "blue")
# Loop over list
for (colorv in colorl) {
  print(colorv)
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
vecv <- integer(7)
# Loop over a vector and overwrite it
for (i in seq_along(vecv)) {
  cat("Changing element:", i, "\n")
  vecv[i] <- i^2
}  # end for
# Modifying vecv inside sapply() has no effect
vecv <- integer(7)
vecv
sapply(seq_along(vecv), function(i) {
  vecv[i] <- i^2
})  # end sapply
vecv
# Super-assignment operator "<<-" allows modifying vecv
sapply(seq_along(vecv), function(i) {
  vecv[i] <<- i^2 # "<<-" !!!
})  # end sapply
vecv
# sapply() loop returns vector of values
vecv <- sapply(seq_along(vecv), function(i) (i^2))
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
vecv <- sample(1:9)
vecv
vecv < 5  # Element-wise comparison
vecv == 5  # Element-wise comparison
matv <- matrix(vecv, ncol=3)
matv
matv < 5  # Element-wise comparison
matv == 5  # Element-wise comparison
matv <- 1:6  # Create a vector
class(matv)  # Get its class
# Is it vector or matrix?
c(is.vector(matv), is.matrix(matv))
structure(matv, dim=c(2, 3))  # Matrix object
# Adding dimension attribute coerces into matrix
dim(matv) <- c(2, 3)
class(matv)  # Get its class
# Is it vector or matrix?
c(is.vector(matv), is.matrix(matv))
# Assign dimnames attribute
dimnames(matv) <- list(rows=c("row1", "row2"),
            columns=c("col1", "col2", "col3"))
matv
matv <- matrix(1:10, 2, 5)  # Create matrix
matv
# as.numeric strips dim attribute from matrix
as.numeric(matv)
# Explicitly coerce to "character"
matv <- as.character(matv)
c(typeof(matv), mode(matv), class(matv))
# Coercion converted matrix to vector
c(is.matrix(matv), is.vector(matv))
vec1 <- 1:3  # Define vector
vec2 <- 6:4  # Define vector
# Bind vectors into columns
cbind(vec1, vec2)
# Bind vectors into rows
rbind(vec1, vec2)
# Extend to four elements
vec2 <- c(vec2, 7)
# Recycling rule applied
cbind(vec1, vec2)
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
vec1 <- c(2, 4, 3)
matv <- matrix(sample(1:12), ncol=3)
# Multiply columns of matrix by vector
vec1*matv
# Or
matv*vec1
# Multiply rows of matrix by vector
t(vec1*t(matv))
# Multiply rows of matrix by vector - transpose is very slow
matrixp <- lapply(1:NCOL(matv), 
  function(x) vec1[x]*matv[, x])
do.call(cbind, matrixp)
library(microbenchmark)
summary(microbenchmark(
  trans=t(vec1*t(matv)),
  lapp={
    matrixp <- lapply(1:NCOL(matv), function(x) vec1[x]*matv[, x])
    do.call(cbind, matrixp)
  },
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
vec1
vec2 <- 6:4  # Define vector
# Multiply two vectors element-by-element
vec1 * vec2
# Calculate inner product
vec1 %*% vec2
# Calculate inner product and drop dimensions
drop(vec1 %*% vec2)
# Multiply columns of matrix by vector
matv %*% vec1  # Single column matrix
drop(matv %*% vec1)  # vector
rowSums(t(vec1 * t(matv)))
# using rowSums() and t() is 10 times slower than %*%
library(microbenchmark)
summary(microbenchmark(
  inner=drop(matv %*% vec1),
  transp=rowSums(t(vec1 * t(matv))),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
library(microbenchmark)
# Multiply matrix by vector fails because dimensions aren't conformable
vec1 %*% matv
# Works after transpose
drop(vec1 %*% t(matv))
# Calculate inner product
crossprod(vec1, vec2)
# Create matrix and vector
matv <- matrix(1:3000, ncol=3)
tmatv <- t(matv)
vecv <- 1:3
# crossprod() is slightly faster than "%*%" operator
summary(microbenchmark(
  cross_prod=crossprod(tmatv, vecv),
  inner_prod=matv %*% vecv,
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Define named vectors
vec1 <- sample(1:4)
names(vec1) <- paste0("row", 1:4, "=", vec1)
vec1
vec2 <- sample(1:3)
names(vec2) <- paste0("col", 1:3, "=", vec2)
vec2
# Calculate outer product of two vectors
matv <- outer(vec1, vec2)
matv
# Calculate vectorized function spanned over two vectors
matv <- outer(vec1, vec2,
           FUN=function(x1, x2) x2*sin(x1))
matv
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
listv <- lapply(1:5, rnorm, n=10)
matv <- do.call(rbind, listv)
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
all.equal(matv, do_call_rbind(listv))
library(microbenchmark)
airquality[(airquality$Solar.R > 320 & !is.na(airquality$Solar.R)), ]
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
irisplit <- split(iris, iris$Species)
str(irisplit, max.confl=1)
names(irisplit)
dim(irisplit$setosa)
head(irisplit$setosa, 2)
all.equal(setosa, irisplit$setosa)
unique(mtcars$cyl)  # cyl has three unique values
# Split mpg column based on number of cylinders
split(mtcars$mpg, mtcars$cyl)
# Split mtcars data frame based on number of cylinders
carsplit <- split(mtcars, mtcars$cyl)
str(carsplit, max.confl=1)
names(carsplit)
# Aggregate the mean mpg over split mtcars data frame
sapply(carsplit, function(x) mean(x$mpg))
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
cardata <- sapply(carsplit, function(x) {
  c(mean=mean(x$mpg), max=max(x$mpg), min=min(x$mpg))
}  # end anonymous function
)  # end sapply
cardata  # sapply() produces a matrix
# Now same using lapply
cardata <- lapply(carsplit, function(x) {
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
panelds <- split(paneld, Industry)
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
panelds <- split(paneld, Sector)
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
panelds <- split(paneld, Industry)
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
    stopifnot(!(numv == 3))  # Check for error
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
