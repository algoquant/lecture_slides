# Display documentation on function "getwd"
help(getwd)
# Equivalent to "help(getwd)"
?getwd

# Open the hypertext documentation
help.start()

# Calculate cumulative sum of a vector
vecv <- runif(1e5)
# Use compiled function
cumsumv <- cumsum(vecv)
# Use for loop
cumsumv2 <- vecv
for (i in 2:NROW(vecv))
  cumsumv2[i] <- (vecv[i] + cumsumv2[i-1])
# Compare the outputs of the two methods
all.equal(cumsumv, cumsumv2)
# Microbenchmark the two methods
library(microbenchmark)
summary(microbenchmark(
  cumsum=cumsum(vecv), # Vectorized
  loop_alloc={cumsumv2 <- vecv # Allocate memory to cumsumv3
    for (i in 2:NROW(vecv))
cumsumv2[i] <- (vecv[i] + cumsumv2[i-1])
  },
  loop_nalloc={cumsumv3 <- vecv[1] # Doesn't allocate memory to cumsumv3
    for (i in 2:NROW(vecv))
cumsumv3[i] <- (vecv[i] + cumsumv3[i-1])
  }, times=10))[, c(1, 4, 5)]

# "<-" and "=" are valid assignment operators
myvar <- 3

# typing a symbol or expression evaluates it
myvar

# text in quotes is interpreted as a string
myvar <- "Hello World!"

# typing a symbol or expression evaluates it
myvar

myvar  # text after hash is treated as comment

getwd()  # get cwd
setwd("/Users/jerzy/Develop/R")  # Set cwd
getwd()  # get cwd

Sys.time()  # get date and time

Sys.Date()  # get date only

rm(list=ls())
setwd("/Users/jerzy/Develop/lecture_slides/data")
var1 <- 3  # Define new object
ls()  # List all objects in workspace
# List objects starting with "v"
ls(pattern=glob2rx("v*"))
# Delete all objects in workspace starting with "v"
rm(list=ls(pattern=glob2rx("v*")))
save.image()  # Save workspace to file .RData in cwd
rm(var1)  # Remove object
ls()  # List objects
load(".RData")
ls()  # List objects
var2 <- 5  # Define another object
save(var1, var2,  # Save selected objects
     file="/Users/jerzy/Develop/lecture_slides/data/my_data.RData")
rm(list=ls())  # Delete all objects in workspace
ls()  # List objects
loadv <- load(file="/Users/jerzy/Develop/lecture_slides/data/my_data.RData")
loadv
ls()  # List objects

  q()  # quit R session

history(5)  # Display last 5 commands
savehistory(file="myfile")  # Default is ".Rhistory"
loadhistory(file="myfile")  # Default is ".Rhistory"

sessionInfo()  # get R version and other session info

Sys.getenv()[5:7]  # List some environment variables

Sys.getenv("HOME")  # get R user HOME directory

Sys.setenv(Home="/Users/jerzy/Develop/data")  # Set HOME directory

Sys.getenv("HOME")  # get user HOME directory

Sys.getenv("R_HOME")  # get R_HOME directory

R.home()  # get R_HOME directory

R.home("etc")  # get "etc" sub-directory of R_HOME

# ?options  # Long list of global options
# Interpret strings as characters, not factors
getOption("stringsAsFactors")  # Display option
options("stringsAsFactors")  # Display option
options(stringsAsFactors=FALSE)  # Set option
# number of digits printed for numeric values
options(digits=3)
# control exponential scientific notation of print method
# positive "scipen" values bias towards fixed notation
# negative "scipen" values bias towards scientific notation
options(scipen=100)
# maximum number of items printed to console
options(max.print=30)
# Warning levels options
# negative - warnings are ignored
options(warn=-1)
# zero - warnings are stored and printed after top-confl function has completed
options(warn=0)
# One - warnings are printed as they occur
options(warn=1)
# two or larger - warnings are turned into errors
options(warn=2)
# Save all options in variable
optionv <- options()
# Restore all options from variable
options(optionv)

# R startup (site) directory
paste(R.home(), "etc", sep="/")

file.path(R.home(), "etc")  # better way

# perform tilde-expansions and convert to readable format
normalizePath(file.path(R.home(), "etc"), winslash="/")

normalizePath(R.home("etc"), winslash="/")

normalizePath("~", winslash="/")  # Windows user HOME directory

Sys.getenv("HOME")  # R user HOME directory

setwd("/Users/jerzy/Develop/R")
getwd()  # current working directory

# R startup (site) directory
normalizePath(file.path(R.home(), "etc"), winslash="/")

# R executable directory
normalizePath(file.path(R.home(), "bin/x64"), winslash="/")

# R documentation directory
normalizePath(file.path(R.home(), "doc/manual"), winslash="/")

sample(dir(), 5)  # get 5 file names - dir() lists all files
sample(dir(pattern="csv"), 5)  # List files containing "csv"
sample(list.files(R.home()), 5)  # All files in R_HOME directory
sample(list.files(R.home("etc")), 5)  # All files in "etc" sub-directory of R_HOME directory
sample(list.dirs(), 5)  # Directories in cwd
list.dirs(R.home("etc"))  # Directories in "etc" sub-directory
sample(Sys.glob("*.csv"), 5)
Sys.glob(R.home("etc"))

getwd()  # get cwd

setwd("/Users/jerzy/Develop/R")
# help(Startup)  # Description of R session startup mechanism

# files in R startup directory directory
dir(normalizePath(file.path(R.home(), "etc"), winslash="/"))

# *.R* files in cwd directory
getwd()
dir(getwd(), all.files=TRUE, pattern="\\.R")
dir(getwd(), all.files=TRUE, pattern=glob2rx("*.R*"))

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
# Named vector has "namev" attribute
attributes(myvar)
myvar <- 1:10
is.vector(myvar)  # Is the object a vector?
attributes(myvar) <- list(my_attr="my_attr")
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
matv <- matrix(1:10, 2, 5)  # Create matrix
# Explicitly coerce to "character"
matv <- as.character(matv)
c(typeof(matv), mode(matv), class(matv))
# Coercion converted matrix to vector
c(is.matrix(matv), is.vector(matv))
as.logical(0:3)  # Explicit coercion to "logical"
as.numeric(c(FALSE, TRUE, TRUE, TRUE))
c(1:3, "a")  # Implicit coercion to "character"
# Explicit coercion to "numeric"
as.numeric(c(1:3, "a"))

"Hello World!"  # Type some text
# hello is a variable name, because it's not in quotes
hello  # R interprets "hello" as a variable name
is.vector(1)  # Single number is a vector
is.vector("a")  # String is a vector
4:8  # Create a vector
# Create vector using c() combine function
c(1, 2, 3, 4, 5)
# Create vector using c() combine function
c("a", "b", "c")
# Create vector using c() combine function
c(1, "b", "c")

stringv <- "Some string"
stringv
stringv[1]
stringv[2]

NROW(stringv)  # length of vector
nchar(stringv)  # length of string

# Concatenate and echo to console
cat("Hello", "World!")
cat("Enter\ttab")
cat("Enter\nnewline")
cat("Enter\\backslash")

stringv1 <- "Hello"  # Define a character string
stringv2 <- "World!"  # Define a character string
paste(stringv1, stringv2, sep=" ")  # Concatenate and return value
cat(stringv1, stringv2)  # Concatenate and echo to console
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
vecv <- c(8, 6, 5, 7)  # Create vector
vecv
vecv[2]  # Extract second element
# Extract all elements, except the second element
vecv[-2]
# Create Boolean vector
c(FALSE, TRUE, TRUE)
# Extract second and third elements
vecv[c(FALSE, TRUE, TRUE)]
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
vecv <- c(8, 6, 5, 7)  # Create vector
vecv
# Boolean vector TRUE if element is equal to second one
vecv == vecv[2]
# Boolean vector TRUE for elements greater than six
vecv > 6
2*vecv  # Multiply all elements by 2
vecv^2  # Square all elements
c(11, 5:10)  # Combine two vectors
c(vecv, 2.0)  # Append number to vector

vecv <- # Create named vector
  c(pi_const=pi, euler=exp(1), gamma=-digamma(1))
vecv
names(vecv)  # Get names of elements
vecv["euler"]  # Get element named "euler"
names(vecv) <- c("pie","eulery","gammy")  # Rename elements
vecv
unname(vecv)  # Remove names attribute
letters[5:10]  # Vector of letters
c("a", letters[5:10])  # Combine two vectors of letters
# Create named vector
structure(sample(1:5), names=paste0("el", 1:5))

vecv  # Named vector
# Extract second element
vecv[2]
# Extract all elements, except the second element
vecv[-2]
# Extract zero elements - returns zero-length vector
vecv[0]
# Extract second and third elements
vecv[c(FALSE, TRUE, TRUE)]
# Extract elements using their names
vecv["eulery"]
# Extract elements using their names
vecv[c("pie", "gammy")]
# Subset whole vector
vecv[] <- 0

vecv <- runif(5)
vecv
vecv > 0.5  # Boolean vector
# Boolean vector of elements equal to the second one
vecv == vecv[2]
# Extract all elements equal to the second one
vecv[vecv == vecv[2]]
vecv < 1  # Boolean vector of elements less than one
# Extract all elements greater than one
vecv[vecv > 1]
vecv[vecv > 0.5]  # Filter elements > 0.5
which(vecv > 0.5)  # Index of elements > 0.5

# Create factor vector
factv <- factor(c("b", "c", "d", "a", "c", "b"))
factv
factv[3]
# Get factor attributes
attributes(factv)
# Get allowed values
levels(factv)
# Get encoding vector
as.numeric(factv)
is.vector(factv)
# Coerce vector to factor
as.factor(1:5)
# Coerce factor to character vector
as.vector(as.factor(1:5))

# Print factor vector
factv
# Get unique elements of factv
unique(factv)
# Get levels attribute of factv
levels(factv)
# Calculate the factor elements from its levels
levels(factv)[as.numeric(factv)]
# Get contingency (frequency) table
table(factv)

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
breakv <- c(freezing=0, very_cold=30, cold=50, pleasant=60, warm=80, hot=90)
breakv
tempv <- runif(10, min=10, max=100)
feels_like <- names(breakv[findInterval(x=tempv, vec=breakv)])
names(tempv) <- feels_like
tempv

library(microbenchmark)
datav <- sample(0:6) + 0.1
datav
cut(x=datav, breaks=c(2, 4, 6, 8))
rbind(datav, cut(x=datav, breaks=c(2, 4, 6, 8)))
# cut() replicates findInterval()
cut(x=1:8, breaks=c(3, 5, 7), labels=1:2, right=FALSE)
findInterval(x=1:8, vec=c(3, 5, 7))
# findInterval() is a compiled function, so it's faster than cut()
vecv <- rnorm(1000)
summary(microbenchmark(
  find_interval=findInterval(x=vecv, vec=c(3, 5, 7)),
  cut=cut(x=vecv, breaks=c(3, 5, 7)),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# Calculate VTI percentage returns
retp <- na.omit(rutils::etfenv$returns$VTI)
# Plot histogram
x11(width=6, height=5)
par(mar=c(1, 1, 1, 1), oma=c(2, 2, 2, 0))
madv <- mad(retp)
histp <- hist(retp, breaks=100,
  main="", xlim=c(-5*madv, 5*madv),
  xlab="", ylab="", freq=FALSE)

# Draw kernel density of histogram
lines(density(retp), col="red", lwd=2)
# Add density of normal distribution
curve(expr=dnorm(x, mean=mean(retp), sd=sd(retp)),
add=TRUE, type="l", lwd=2, col="blue")
title(main="VTI Return Distribution", line=0)
# Add legend
legend("topright", inset=0.05, cex=0.8, title=NULL,
  leg=c("VTI", "Normal"), bty="n",
  lwd=6, bg="white", col=c("red", "blue"))
# Total area under histogram
sum(diff(histp$breaks) * histp$density)

matv <- matrix(5:10, nrow=2, ncol=3)  # Create a matrix
matv  # By default matrices are constructed column-wise
# Create a matrix row-wise
matrix(5:10, nrow=2, byrow=TRUE)
matv[2, 3]  # Extract third element from second row
matv[2, ]  # Extract second row
matv[, 3]  # Extract third column
matv[, c(1,3)]  # Extract first and third column
matv[, -2]  # Remove second column
# Subset whole matrix
matv[] <- 0
# Get the number of rows or columns
nrow(vecv); ncol(vecv)
NROW(vecv); NCOL(vecv)
nrow(matv); ncol(matv)
NROW(matv); NCOL(matv)

attributes(matv)  # Get matrix attributes
dim(matv)  # Get dimension attribute
class(matv)  # Get class attribute
rownames(matv) <- c("row1", "row2")  # Rownames attribute
colnames(matv) <- c("col1", "col2", "col3")  # Colnames attribute
matv
matv["row2", "col3"]  # Third element from second row
names(matv)  # Get the names attribute
dimnames(matv)  # Get dimnames attribute
attributes(matv)  # Get matrix attributes

matv  # matrix with column names
matv[1, ]  # Subset rows by index
matv[, "col1"]  # Subset columns by name
matv[, c(TRUE, FALSE, TRUE)]  # Subset columns Boolean vector
matv[1, ]  # Subsetting can produce a vector!
class(matv); class(matv[1, ])
is.matrix(matv[1, ]); is.vector(matv[1, ])
matv[1, , drop=FALSE]  # Drop=FALSE preserves matrix
class(matv[1, , drop=FALSE])
is.matrix(matv[1, , drop=FALSE]); is.vector(matv[1, , drop=FALSE])

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
calc_center <- function(inputv, method=c("mean", "mean_narm", "median")) {
# validate "method" argument
  method <- match.arg(method)
  switch(method,
 mean=mean(inputv),
 mean_narm=mean(inputv, na.rm=TRUE),
 median=median(inputv))
}  # end calc_center
myvar <- rnorm(100, mean=2)
calc_center(myvar, "mean")
calc_center(myvar, "mean_narm")
calc_center(myvar, "median")

for (indeks in vecv) {expvs}

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

vecv <- integer(7)
# Loop over a vector and overwrite it
for (i in seq_along(vecv)) {
  cat("Changing element:", i, "\n")
  vecv[i] <- i^2
}  # end for
# Modifying vecv inside sapply() has no effect
vecv <- integer(7)
vecv
sapply(seq_along(vecv),
 function(i) {
   vecv[i] <- i^2
 })  # end sapply
vecv
# Super-assignment operator "<<-" allows modifying vecv
sapply(seq_along(vecv),
 function(i) {
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

# Define a function with two arguments
testfun <- function(arg1, arg2) {  # Body
  arg1 + arg2  # Returns last evaluated statement
}  # end testfun

testfun(1, 2)  # Apply the function
args(testfun)  # Display argument

# Define function that uses variable from enclosure environment
testfun <- function(arg1, arg2) {
  arg1 + arg2 + globv
}  # end testfun

testfun(3, 2)  # error - globv doesn't exist yet!
globv <- 10  # Create globv
testfun(3, 2)  # Now works

# Define function that returns NULL for non-numeric argument
testfun <- function(inputv) {
  if (!is.numeric(inputv)) {
    warning(paste("argument", inputv, "isn't numeric"))
    return(NULL)
  }
  2*inputv
}  # end testfun

testfun(2)
testfun("hello")

# Define a function that returns invisibly
retinv <- function(inputv) {
  invisible(inputv)
}  # end retinv

retinv(2)

globv <- retinv(2)
globv

rm(list=ls())  # Delete all objects in workspace
# Load objects from file
loaded <- load(file="/Users/jerzy/Develop/data/my_data.RData")
loaded  # Vector of loaded objects
ls()  # List objects

testfun <- function(arg1, arg2) {
# Last statement of function is return value
  arg1 + 2*arg2
}  # end testfun
testfun(arg1=3, arg2=2)  # Bind by name
testfun(first=3, second=2)  # Partial name binding
testfun(3, 2)  # Bind by position
testfun(arg2=2, 3)  # mixed binding
testfun(3, 2, 1)  # Too many arguments
testfun(2)  # Not enough arguments

# Function "paste" has two arguments with default values
str(paste)
# Default values of arguments can be specified in argument list
testfun <- function(arg1, ratio=1) {
  ratio*arg1
}  # end testfun
testfun(3)  # Default value used for second argument
testfun(3, 2)  # Default value over-ridden
# Default values can be a vector of strings
testfun <- function(inputv=c("first_val", "second_val")) {
  inputv <- match.arg(inputv)  # Match to arg list
  inputv
}  # end testfun
testfun("second_val")
testfun("se")  # Partial name binding
testfun("some_val")  # Invalid string

# VTI percentage returns
retp <- rutils::diffit(log(Cl(rutils::etfenv$VTI)))
# calc_skew() calculates skew of time series of returns
# Default is normal time series
calc_skew <- function(retp=rnorm(1000)) {
  # Number of observations
  nrows <- NROW(retp)
  # Standardize returns
  retp <- (retp - mean(retp))/sd(retp)
  # Calculate skew - last statement automatically returned
  nrows*sum(retp^3)/((nrows-1)*(nrows-2))
}  # end calc_skew

# Calculate the skew of VTI returns
# Pass the arguments by name
calc_skew(retp=retp)
# Pass the arguments by position
calc_skew(retp)
# Use default value of arguments
calc_skew()

str(plot)  # Dots for additional plot parameters
bind_dots <- function(inputv, ...) {
  paste0("inputv=", inputv, ", dots=", paste(..., sep=", "))
}  # end bind_dots
bind_dots(1, 2, 3)  # "inputv" bound by position
bind_dots(2, inputv=1, 3)  # "inputv" bound by name
bind_dots(1, 2, 3, argv=10)  # Named argument bound to dots
bind_dots <- function(arg1, arg2, ...) {
  arg1 + 2*arg2 + sum(...)
}  # end bind_dots
bind_dots(3, 2)  # Bind arguments by position
bind_dots(3, 2, 5, 8)  # Extra arguments bound to dots

str(sum)  # Dots before other arguments
sum(1, 2, 3)  # Dots bind before other arguments
sum(1, 2, NA, 3, na.rm=TRUE)
bind_dots <- function(..., inputv) {
  paste0("inputv=", inputv, ", dots=", paste(..., sep=", "))
}  # end bind_dots
# Arguments after dots must be bound by full name
bind_dots(1, 2, 3, inputv=10)
bind_dots(1, 2, 3, inputv=10, argv=4)  # Dots bound
bind_dots(1, 2, 3)  # "inputv" not bound
bind_dots <- function(..., inputv=10) {
  paste0("inputv=", inputv, ", dots=", paste(..., sep=", "))
}  # end bind_dots
bind_dots(1, 2, 3)  # "inputv" not bound, but has default

# Wrapper for mean() with default na.rm=TRUE
meanfun <- function(x, na.rm=TRUE, ...) {
  mean(x=x, na.rm=na.rm, ...)
}  # end meanfun
vecv <- sample(c(1:10, NA, rep(0.1, t=5)))
mean(vecv)
mean(vecv, na.rm=TRUE)
meanfun(vecv)
meanfun(vecv, trim=0.4)  # Pass extra argument
# Wrapper for saving data into default directory
save_data <- function(...,
              file=stop("error: no file name"),
              my_dir="/Users/jerzy/Develop/data") {
# Create file path
  file <- file.path(my_dir, file)
  save(..., file=file)
}  # end save_data
vecv <- 1:10
save_data(vecv, file="scratch.RData")
save_data(vecv, file="scratch.RData", my_dir="/Users/jerzy/Develop")
# Wrapper for testing negative arguments
stop_if_neg <- function(inputv) {
  if (!is.numeric(inputv) || inputv < 0)
    stop("argument not numeric or negative")
}  # end stop_if_neg
# Wrapper for sqrt()
my_sqrt <- function(inputv) {
  stop_if_neg(inputv)
  sqrt(inputv)
}  # end my_sqrt
my_sqrt(2)
my_sqrt(-2)
my_sqrt(NA)

# Recursive function sums its argument list
sum_dots <- function(inputv, ...) {
  if (missing(...)) {  # Check if dots are empty
    return(inputv)  # just one argument left
  } else {
    inputv + sum_dots(...)  # Sum remaining arguments
  }  # end if
}  # end sum_dots
sum_dots(1, 2, 3, 4)
# Recursive function sums its argument list
sum_dots <- function(inputv, ...) {
  if (NROW(list(...)) == 0) {  # Check if dots are empty
    return(inputv)  # just one argument left
  } else {
    inputv + sum_dots(...)  # Sum remaining arguments
  }  # end if
}  # end sum_dots
sum_dots(1, 2, 3, 4)

fibonacci <- function(nrows) {
  if (nrows > 2) {
    fib_seq <- fibonacci(nrows-1)  # Recursion
    c(fib_seq, sum(tail(fib_seq, 2)))  # Return this
  } else {
    c(0, 1)  # Initialize and return
  }
}  # end fibonacci
fibonacci(10)
tail(fibonacci(9), 2)

# Show the function code
plot.default
# Display function
getAnywhere(plot.default)

# Sum() is a compiled primitive function
sum
# mean() is a generic function
mean
# Show all methods of mean()
methods(generic.function=mean)
# Show code for mean.default()
mean.default

# Get all methods for generic function "plot"
methods("plot")

getAnywhere(plot)  # Display function

rm(list=ls())
lazyfun <- function(arg1, arg2) {  # Define function lazyfun
  2*arg1  # just multiply first argument
}  # end lazyfun
lazyfun(3, 2)  # Bind arguments by position
lazyfun(3)  # Second argument was never evaluated!
lazyfun <- function(arg1, arg2) {  # Define function lazyfun
  cat(arg1, '\n')  # Write to output
  cat(arg2)  # Write to output
}  # end lazyfun
lazyfun(3, 2)  # Bind arguments by position
lazyfun(3)  # First argument written to output

rm(list=ls())
globv <- 1  # Define a global variable
ls(environment())  # Get all variables in environment
func_env <- function() {  # Explore function environments
  locvar <- 1  # Define a local variable
  cat('objects in evaluation environment:\t',
      ls(environment()), '\n')
  cat('objects in enclosing environment:\t',
      ls(parent.env(environment())), '\n')
  cat('this is the enclosing environment:')
  parent.env(environment())  # Return enclosing environment
}  # end func_env
func_env()

environment(func_env)
environment(print)  # Package namespace is the enclosure

rm(list=ls())
globv <- 1  # Define a global variable
probe_scope <- function() {  # Explore function scope
  locvar <- 2*globv  # Define a local variable
  new_globvar <<- 11  # Define a global variable
  cat('objects in evaluation environment:\t',
      ls(environment()), '\n')
  cat('this is a local locvar:\t', locvar, '\n')
  cat('objects in enclosing environment:\n',
      ls(parent.env(environment())), '\n')
  cat('this is globv:\t', globv, '\n')
  globv <- 10  # Define local globv
  cat('this is the local globv:\t', globv, '\n')
}  # end probe_scope
probe_scope()
globv  # Global variable is unaffected
new_globvar  # new_globvar is preserved
locvar  # Local variable is gone!

a <- 1  # Define a variable
# New variable "b" points to value of "a"
b <- a  # Define a new variable
# When "b" is modified, R makes a copy of it
b <- b+1
# Function doubles its argument and returns it
double_it <- function(inputv) {
  inputv <- 2*inputv
  cat("input argument was doubled to:", inputv, "\n")
  inputv
}
double_it(a)
a  # variable "a" is unchanged

setwd("/Users/jerzy/Develop/lecture_slides/data")
rm(list=ls())  # Delete all objects in workspace
ls()  # List objects
# Load objects from file (side effect)
load(file="my_data.RData")
ls()  # List objects
globv <- 1  # Define a global variable
# Explore function scope and side effects
side_effect <- function() {
  cat("global globv =", globv, "\n")
# Define local "globv" variable
  globv <- 10
  cat("local globv =", globv, "\n")
  # Re-define the global "globv"
  globv <<- 2
  cat("local globv =", globv, "\n")
}  # end side_effect
side_effect()
# Global variable was modified as side effect
globv

# Standard infix operator call syntax
2 + 3
# Infix operator applied using prefix syntax
"+"(2, 3)
# Standard bracket operator
vecv <- c(4, 3, 5, 6)
vecv[2]
# Bracket operator applied using prefix syntax
"["(vecv, 2)


# Define infix operator that returns string
'%+%' <- function(a, b) paste(a, b, sep=" + ")
2 %+% 3
2 %+% 3 %+% 4
"hello" %+% 2 %+% 3 %+% "bye"

obj_string <- "hello"
class(obj_string)
# Assign to value returned by "class" function
class(obj_string) <- "string"
class(obj_string)
# Define function last()
last <- function(vecv) {
  vecv[NROW(vecv)]
}  # end last
last(1:10)
# Define replacement function last()
'last<-' <- function(vecv, value) {
  vecv[NROW(vecv)] <- value
  vecv
}  # end last
x <- 1:5
last(x) <- 11
x

# Functional accepts function name and additional argument
testfun <- function(funn, inputv) {
# Produce function name from argument
  funn <- match.fun(funn)
# Execute function call
  funn(inputv)
}  # end testfun
testfun(sqrt, 4)
# String also works because match.fun() converts it to a function
testfun("sqrt", 4)
str(sum)  # Sum() accepts multiple arguments
# Functional can't accept indefinite number of arguments
testfun(sum, 1, 2, 3)

# Functional accepts function name and dots '...' argument
testfun <- function(funn, ...) {
  funn <- match.fun(funn)
  funn(...)  # Execute function call
}  # end testfun
testfun(sum, 1, 2, 3)
testfun(sum, 1, 2, NA, 4, 5)
testfun(sum, 1, 2, NA, 4, 5, na.rm=TRUE)
# Function with three arguments and dots '...' arguments
testfun <- function(inputv, param1, param2, ...) {
  c(inputv=inputv, param1=param1, param2=param2, dots=c(...))
}  # end testfun
testfun(1, 2, 3, 4, 5)
testfun(1, 2, 3, param2=4, param1=5)

# Simple anonymous function
(function(x) (x + 3)) (10)

# Anonymous function passed to testfun
testfun(funn=(function(x) (x + 3)), 5)
# Anonymous function is default value
testfun <-
  function(..., funn=function(x, y, z) {x+y+z}) {
    funn <- match.fun(funn)
    funn(...)  # Execute function call
}  # end testfun
testfun(2, 3, 4)  # Use default funn
testfun(2, 3, 4, 5)
# funn bound by name
testfun(funn=sum, 2, 3, 4, 5)
# Pass anonymous function to funn
testfun(funn=function(x, y, z) {x*y*z},
    2, 3, 4)

str(sum)  # Sum() accepts multiple arguments
# Sum() can't accept list of arguments
sum(list(1, 2, 3))
str(do.call)  # "what" argument is a function
# Do.call passes list elements into "sum" individually
do.call(sum, list(1, 2, 3))
do.call(sum, list(1, 2, NA, 3))
do.call(sum, list(1, 2, NA, 3, na.rm=TRUE))
# Functional accepts list with function name and arguments
testfun <- function(list_arg) {
# Produce function name from argument
  funn <- match.fun(list_arg[[1]])
# Execute function call uing do.call()
  do.call(funn, list_arg[-1])
}  # end testfun
arg_list <- list("sum", 1, 2, 3)
testfun(arg_list)
# do_call() performs same operation as do.call()
all.equal(
  do.call(sum, list(1, 2, NA, 3, na.rm=TRUE)),
  rutils::do_call(sum, list(1, 2, NA, 3), na.rm=TRUE))

rm(list=ls())
str(apply)  # Get list of arguments
# Create a matrix
matv <- matrix(6:1, nrow=2, ncol=3)
matv
# Sum the rows and columns
rowsumv <- apply(matv, 1, sum)
colsumv <- apply(matv, 2, sum)
matv <- cbind(c(sum(rowsumv), rowsumv),
          rbind(colsumv, matv))
dimnames(matv) <- list(c("colsumv", "row1", "row2"),
                 c("rowsumv", "col1", "col2", "col3"))
matv

str(apply)  # Get list of arguments
matv <- matrix(sample(12), nrow=3, ncol=4)  # Create a matrix
matv
apply(matv, 2, sort)  # Sort matrix columns
apply(matv, 2, sort, decreasing=TRUE)  # Sort decreasing order

matv[2, 2] <- NA  # Introduce NA value
matv
# Calculate median of columns
apply(matv, 2, median)
# Calculate median of columns with na.rm=TRUE
apply(matv, 2, median, na.rm=TRUE)

# VTI percentage returns
retp <- rutils::diffit(log(Cl(rutils::etfenv$VTI)))
library(moments)  # Load package moments
str(moment)  # Get list of arguments
# Apply moment function
moment(x=retp, order=3)
# 4x1 matrix of moment orders
orderv <- as.matrix(1:4)
# Anonymous function allows looping over function parameters
apply(X=orderv, MARGIN=1, FUN=function(orderp) {
  moment(x=retp, order=orderp)
}  # end anonymous function
)  # end apply

# Another way of passing parameters into moment() function
apply(X=orderv, MARGIN=1, FUN=moment, x=retp)

# Function with three arguments
testfun <- function(arg1, arg2, arg3) {
  c(arg1=arg1, arg2=arg2, arg3=arg3)
}  # end testfun
testfun(1, 2, 3)
datav <- as.matrix(1:4)
# Pass datav to arg1
apply(X=datav, MAR=1, FUN=testfun, arg2=2, arg3=3)
# Pass datav to arg2
apply(X=datav, MAR=1, FUN=testfun, arg1=1, arg3=3)
# Pass datav to arg3
apply(X=datav, MAR=1, FUN=testfun, arg1=1, arg2=2)

# Vector of means of numeric columns
sapply(iris[, -5], mean)
# List of means of numeric columns
lapply(iris[, -5], mean)
# Lapply using anonymous function
unlist(lapply(iris,
      function(column) {
        if (is.numeric(column)) mean(column)
      }  # end anonymous function
      )  # end lapply
       )  # end unlist
unlist(sapply(iris, function(column) {
  if (is.numeric(column)) mean(column)}))

sapply(6:10, sqrt)  # Sapply on vector
sapply(list(6, 7, 8, 9, 10), sqrt)  # sapply on list

# Calculate means of iris data frame columns
sapply(iris, mean)  # Returns NA for Species

# Create a matrix
matv <- matrix(sample(100), ncol=4)
# Calculate column means using apply
apply(matv, 2, mean)

# Calculate column means using sapply, with anonymous function
sapply(1:NCOL(matv), function(colnum) {  # Anonymous function
 mean(matv[, colnum])
  }  # end anonymous function
)  # end sapply

# Vectors form columns of matrix returned by sapply
sapply(2:4, function(num) c(el1=num, el2=2*num))
# Vectors of different lengths returned as list
sapply(2:4, function(num) 1:num)
# vapply is similar to sapply
vapply(2:4, function(num) c(el1=num, el2=2*num),
       FUN.VALUE=c(row1=0, row2=0))
# vapply produces an error if it can't simplify
vapply(2:4, function(num) 1:num,
       FUN.VALUE=c(row1=0, row2=0))
