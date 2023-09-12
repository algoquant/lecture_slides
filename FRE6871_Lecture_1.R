# Display documentation on function "getwd"
help(getwd)
# Equivalent to "help(getwd)"
?getwd

# Open the hypertext documentation
help.start()

# Calculate cumulative sum of a vector
vectorv <- runif(1e5)
# Use compiled function
cumsumv <- cumsum(vectorv)
# Use for loop
cumsumv2 <- vectorv
for (i in 2:NROW(vectorv))
  cumsumv2[i] <- (vectorv[i] + cumsumv2[i-1])
# Compare the outputs of the two methods
all.equal(cumsumv, cumsumv2)
# Microbenchmark the two methods
library(microbenchmark)
summary(microbenchmark(
  cumsum=cumsum(vectorv), # Vectorized
  loop_alloc={cumsumv2 <- vectorv # Allocate memory to cumsumv3
    for (i in 2:NROW(vectorv))
cumsumv2[i] <- (vectorv[i] + cumsumv2[i-1])
  },
  loop_nalloc={cumsumv3 <- vectorv[1] # Doesn't allocate memory to cumsumv3
    for (i in 2:NROW(vectorv))
cumsumv3[i] <- (vectorv[i] + cumsumv3[i-1])
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
# Remove all objects starting with "v"
rm(list=ls(pattern=glob2rx("v*")))
save.image()  # Save workspace to file .RData in cwd
rm(var1)  # Remove object
ls()  # List objects
load(".RData")
ls()  # List objects
var2 <- 5  # Define another object
save(var1, var2,  # Save selected objects
     file="/Users/jerzy/Develop/lecture_slides/data/my_data.RData")
rm(list=ls())  # Remove all objects
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

sample(dir(), 5)  # get 5 file names - dir() listv all files
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
vectorv <- rnorm(1000)
summary(microbenchmark(
  find_interval=findInterval(x=vectorv, vec=c(3, 5, 7)),
  cut=cut(x=vectorv, breaks=c(3, 5, 7)),
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
4.7 / 0.5  # division
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

str(findInterval)
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
rm(list=ls())  # delete all objects
symbol <- "myvar"  # define symbol containing string "myvar"
assign(symbol, 1)  # Assign value to "myvar"
ls()
myvar
assign("symbol", "new_var")
assign(symbol, 1)  # Assign value to "new_var"
ls()
symbol <- 10
assign(symbol, 1)  # Can't assign to non-string

rm(list=ls())  # delete all objects
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
# delete environment
rm(test_env)

rm(list=ls())
# expressions enclosed in parenthesis are less ambiguous
-2:5
(-2):5
-(2:5)
# expressions enclosed in parenthesis are less ambiguous
-2*3+5
-2*(3+5)

# expressions can be separated by semicolons or by lines
{1+2; 2*3; 1:5}
# or
{1+2
2*3
1:5}

matrixv <- matrix(nr=3, nc=4)
matrixv <- 0
# subset whole matrix
matrixv[] <- 0

# parenthesis and braces require a little additional processing time
library(microbenchmark)
summary(microbenchmark(
  basep=sqrt(rnorm(10000)^2),
  parven=sqrt(((((rnorm(10000)^2))))),
  bra_ce=sqrt({{{{rnorm(10000)^2}}}}),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

rm(list=ls())
numv1 <- 1

if (numv1) {  # numeric zero is FALSE, all other numbers are TRUE
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
# measure of central tendency
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

for (indeks in vectorv) {ex_pressions}

rm(list=ls())
colorl <- list("red", "white", "blue")
# loop over list
for (some_color in colorl) {
  print(some_color)
}  # end for
# loop over vector
for (indeks in 1:3) {
  print(colorl[[indeks]])
}  # end for

# while loops require initialization
indeks <- 1
# while loop
while (indeks < 4) {
  print(colorl[[indeks]])
  indeks <- indeks + 1
}  # end while

rm(list=ls())
# loop over a vector and overwrite it
vectorv <- integer(7)
for (i in 1:7) {
  cat("Changing element:", i, "\n")
  vectorv[i] <- i^2
}  # end for
# equivalent way (without cat side effect)
for (i in seq_along(vectorv)) 
  vectorv[i] <- i^2

# sapply() loop returns vector of values
vectorv <- sapply(seq_along(vectorv), function(x) (x^2))

rm(list=ls())
# fib_seq <- numeric()  # zero length numeric vector
# pre-allocate vector instead of "growing" it
fib_seq <- numeric(10)
fib_seq[1] <- 0  # initialize
fib_seq[2] <- 1  # initialize
for (i in 3:10) {  # perform recurrence loop
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

# Define a function with two arguments
testfun <- function(first_arg, second_arg) {  # Body
  first_arg + second_arg  # Returns last evaluated statement
}  # end testfun

testfun(1, 2)  # Apply the function
args(testfun)  # Display argument

# Define function that uses variable from enclosure environment
testfun <- function(first_arg, second_arg) {
  first_arg + second_arg + globv
}  # end testfun

testfun(3, 2)  # error - globv doesn't exist yet!
globv <- 10  # Create globv
testfun(3, 2)  # Now works

# Define function that returns NULL for non-numeric argument
testfun <- function(input) {
  if (!is.numeric(input)) {
    warning(paste("argument", input, "isn't numeric"))
    return(NULL)
  }
  2*input
}  # end testfun

testfun(2)
testfun("hello")

# Define a function that returns invisibly
return_invisible <- function(input) {
  invisible(input)
}  # end return_invisible

return_invisible(2)

globv <- return_invisible(2)
globv

rm(list=ls())  # Remove all objects
# Load objects from file
loaded <- load(file="/Users/jerzy/Develop/data/my_data.RData")
loaded  # Vector of loaded objects
ls()  # List objects

testfun <- function(first_arg, second_arg) {
# Last statement of function is return value
  first_arg + 2*second_arg
}  # end testfun
testfun(first_arg=3, second_arg=2)  # Bind by name
testfun(first=3, second=2)  # Partial name binding
testfun(3, 2)  # Bind by position
testfun(second_arg=2, 3)  # mixed binding
testfun(3, 2, 1)  # Too many arguments
testfun(2)  # Not enough arguments

# Function "paste" has two arguments with default values
str(paste)
# Default values of arguments can be specified in argument list
testfun <- function(first_arg, ratio=1) {
  ratio*first_arg
}  # end testfun
testfun(3)  # Default value used for second argument
testfun(3, 2)  # Default value over-ridden
# Default values can be a vector of strings
testfun <- function(input=c("first_val", "second_val")) {
  input <- match.arg(input)  # Match to arg list
  input
}  # end testfun
testfun("second_val")
testfun("se")  # Partial name binding
testfun("some_val")  # Invalid string

# DAX percentage returns
retp <- rutils::diffit(log(EuStockMarkets[, 1]))
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

# Calculate skew of DAX returns
# Bind arguments by name
calc_skew(retp=retp)
# Bind arguments by position
calc_skew(retp)
# Use default value of arguments
calc_skew()

str(plot)  # Dots for additional plot parameters
bind_dots <- function(input, ...) {
  paste0("input=", input,
 ", dots=", paste(..., sep=", "))
}  # end bind_dots
bind_dots(1, 2, 3)  # "input" bound by position
bind_dots(2, input=1, 3)  # "input" bound by name
bind_dots(1, 2, 3, foo=10)  # Named argument bound to dots
bind_dots <- function(arg1, arg2, ...) {
  arg1 + 2*arg2 + sum(...)
}  # end bind_dots
bind_dots(3, 2)  # Bind arguments by position
bind_dots(3, 2, 5, 8)  # Extra arguments bound to dots

str(sum)  # Dots before other arguments
sum(1, 2, 3)  # Dots bind before other arguments
sum(1, 2, NA, 3, na.rm=TRUE)
bind_dots <- function(..., input) {
  paste0("input=", input,
 ", dots=", paste(..., sep=", "))
}  # end bind_dots
# Arguments after dots must be bound by full name
bind_dots(1, 2, 3, input=10)
bind_dots(1, 2, 3, input=10, foo=4)  # Dots bound
bind_dots(1, 2, 3)  # "input" not bound
bind_dots <- function(..., input=10) {
  paste0("input=", input,
 ", dots=", paste(..., sep=", "))
}  # end bind_dots
bind_dots(1, 2, 3)  # "input" not bound, but has default

# Wrapper for mean() with default na.rm=TRUE
my_mean <- function(x, na.rm=TRUE, ...) {
  mean(x=x, na.rm=na.rm, ...)
}  # end my_mean
foo <- sample(c(1:10, NA, rep(0.1, t=5)))
mean(c(foo, NA))
mean(c(foo, NA), na.rm=TRUE)
my_mean(c(foo, NA))
my_mean(c(foo, NA), trim=0.4)  # Pass extra argument
# Wrapper for saving data into default directory
save_data <- function(...,
              file=stop("error: no file name"),
              my_dir="/Users/jerzy/Develop/data") {
# Create file path
  file <- file.path(my_dir, file)
  save(..., file=file)
}  # end save_data
foo <- 1:10
save_data(foo, file="scratch.RData")
save_data(foo, file="scratch.RData", my_dir="/Users/jerzy/Develop")
# Wrapper for testing negative arguments
stop_if_neg <- function(input) {
  if (!is.numeric(input) || input<0)
    stop("argument not numeric or negative")
}  # end stop_if_neg
# Wrapper for sqrt()
my_sqrt <- function(input) {
  stop_if_neg(input)
  sqrt(input)
}  # end my_sqrt
my_sqrt(2)
my_sqrt(-2)
my_sqrt(NA)

# Recursive function sums its argument list
sum_dots <- function(input, ...) {
  if (missing(...)) {  # Check if dots are empty
    return(input)  # just one argument left
  } else {
    input + sum_dots(...)  # Sum remaining arguments
  }  # end if
}  # end sum_dots
sum_dots(1, 2, 3, 4)
# Recursive function sums its argument list
sum_dots <- function(input, ...) {
  if (NROW(list(...)) == 0) {  # Check if dots are empty
    return(input)  # just one argument left
  } else {
    input + sum_dots(...)  # Sum remaining arguments
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

setwd("/Users/jerzy/Develop/lecture_slides/data")
rm(list=ls())  # Remove all objects
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

# Create functional that accepts a function as input argument
testfun <- function(func_name) {
# Calculates statistic on random numbers
  set.seed(1)
  func_name(runif(1e4))  # Apply the function name
}  # end testfun
testfun(mean)
testfun(sd)

# Functional accepts function name and additional argument
testfun <- function(func_name, input) {
# Produce function name from argument
  func_name <- match.fun(func_name)
# Execute function call
  func_name(input)
}  # end testfun
testfun(sqrt, 4)
# String also works because match.fun() converts it to a function
testfun("sqrt", 4)
str(sum)  # Sum() accepts multiple arguments
# Functional can't accept indefinite number of arguments
testfun(sum, 1, 2, 3)

# Functional accepts function name and dots '...' argument
testfun <- function(func_name, ...) {
  func_name <- match.fun(func_name)
  func_name(...)  # Execute function call
}  # end testfun
testfun(sum, 1, 2, 3)
testfun(sum, 1, 2, NA, 4, 5)
testfun(sum, 1, 2, NA, 4, 5, na.rm=TRUE)
# Function with three arguments and dots '...' arguments
testfun <- function(input, param1, param2, ...) {
  c(input=input, param1=param1, param2=param2, dots=c(...))
}  # end testfun
testfun(1, 2, 3, param2=4, param1=5)
testfun(testfun, 1, 2, 3, param2=4, param1=5)
testfun(testfun, 1, 2, 3, 4, 5)

# Simple anonymous function
(function(x) (x + 3)) (10)

# Anonymous function passed to testfun
testfun(func_name=(function(x) (x + 3)), 5)
# Anonymous function is default value
testfun <-
  function(..., func_name=function(x, y, z) {x+y+z}) {
    func_name <- match.fun(func_name)
    func_name(...)  # Execute function call
}  # end testfun
testfun(2, 3, 4)  # Use default func_name
testfun(2, 3, 4, 5)
# Func_name bound by name
testfun(func_name=sum, 2, 3, 4, 5)
# Pass anonymous function to func_name
testfun(func_name=function(x, y, z) {x*y*z},
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
  func_name <- match.fun(list_arg[[1]])
# Execute function call uing do.call()
  do.call(func_name, list_arg[-1])
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
matrixv <- matrix(6:1, nrow=2, ncol=3)
matrixv
# Sum the rows and columns
rowsumv <- apply(matrixv, 1, sum)
colsumv <- apply(matrixv, 2, sum)
matrixv <- cbind(c(sum(rowsumv), rowsumv),
          rbind(colsumv, matrixv))
dimnames(matrixv) <- list(c("colsumv", "row1", "row2"),
                 c("rowsumv", "col1", "col2", "col3"))
matrixv

str(apply)  # Get list of arguments
matrixv <- matrix(sample(12), nrow=3, ncol=4)  # Create a matrix
matrixv
apply(matrixv, 2, sort)  # Sort matrix columns
apply(matrixv, 2, sort, decreasing=TRUE)  # Sort decreasing order

matrixv[2, 2] <- NA  # Introduce NA value
matrixv
# Calculate median of columns
apply(matrixv, 2, median)
# Calculate median of columns with na.rm=TRUE
apply(matrixv, 2, median, na.rm=TRUE)

rm(list=ls())
# DAX percentage returns
retp <- rutils::diffit(log(EuStockMarkets[, 1]))
library(moments)  # Load package moments
str(moment)  # Get list of arguments
# Apply moment function
moment(x=retp, order=3)
# 4x1 matrix of moment orders
orderv <- as.matrix(1:4)
# Anonymous function allows looping over function parameters
apply(X=orderv, MARGIN=1,
      FUN=function(orderp) {
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
sapply(list(6, 7, 8, 9, 10), sqrt)  # Sapply on list

# Calculate means of iris data frame columns
sapply(iris, mean)  # Returns NA for Species

# Create a matrix
matrixv <- matrix(sample(100), ncol=4)
# Calculate column means using apply
apply(matrixv, 2, mean)

# Calculate column means using sapply, with anonymous function
sapply(1:NCOL(matrixv), function(colnum) {  # Anonymous function
 mean(matrixv[, colnum])
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

xvar <- seq(-5, 7, length=100)
yvar <- dnorm(xvar, mean=1.0, sd=2.0)
plot(xvar, yvar, type="l", lty="solid",
     xlab="", ylab="")
title(main="Normal Density Function", line=0.5)
startp <- 3; endd <- 5  # Set lower and upper bounds
# Set polygon base
subv <- ((xvar >= startp) & (xvar <= endd))
polygon(c(startp, xvar[subv], endd),  # Draw polygon
  c(-1, yvar[subv], -1), col="red")

par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
sigmavs <- c(0.5, 1, 1.5, 2)  # Sigma values
# Create plot colors
colorv <- c("red", "black", "blue", "green")
# Create legend labels
labelv <- paste("sigma", sigmavs, sep="=")
for (indeks in 1:4) {  # Plot four curves
curve(expr=dnorm(x, sd=sigmavs[indeks]),
xlim=c(-4, 4),
xlab="", ylab="", lwd=2,
col=colorv[indeks],
add=as.logical(indeks-1))
}  # end for
# Add title
title(main="Normal Distributions", line=0.5)
# Add legend
legend("topright", inset=0.05, title="Sigmas",
 labelv, cex=0.8, lwd=2, lty=1, bty="n",
 col=colorv)

rm(list=ls())
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
xvar <- seq(-4, 4, length=100)
sigmavs <- c(0.5, 1, 1.5, 2)  # Sigma values
# Create plot colors
colorv <- c("red", "black", "blue", "green")
# Create legend labels
labelv <- paste("sigma", sigmavs, sep="=")
# Plot the first chart
plot(xvar, dnorm(xvar, sd=sigmavs[1]),
     type="n", xlab="", ylab="",
     main="Normal Distributions")
# Add lines to plot
for (indeks in 1:4) {
  lines(xvar, dnorm(xvar, sd=sigmavs[indeks]),
  lwd=2, col=colorv[indeks])
}  # end for
# Add legend
legend("topright", inset=0.05, title="Sigmas",
 labelv, cex=0.8, lwd=2, lty=1, bty="n",
 col=colorv)

# Standard deviations of log-normal distribution
sigmavs <- c(0.5, 1, 1.5)
# Create plot colors
colorv <- c("black", "red", "blue")
# Plot all curves
for (indeks in 1:NROW(sigmavs)) {
  curve(expr=dlnorm(x, sdlog=sigmavs[indeks]),
  type="l", xlim=c(0, 3), lwd=2,
  xlab="", ylab="", col=colorv[indeks],
  add=as.logical(indeks-1))
}  # end for

# Add title and legend
title(main="Log-normal Distributions", line=0.5)
legend("topright", inset=0.05, title="Sigmas",
 paste("sigma", sigmavs, sep="="),
 cex=0.8, lwd=2, lty=rep(1, NROW(sigmavs)),
 col=colorv)

x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
# Degrees of freedom
degf <- c(2, 5, 8, 11)
# Plot four curves in loop
colorv <- c("red", "black", "blue", "green")
for (indeks in 1:4) {
curve(expr=dchisq(x, df=degf[indeks]),
xlim=c(0, 20), ylim=c(0, 0.3),
xlab="", ylab="", col=colorv[indeks],
lwd=2, add=as.logical(indeks-1))
}  # end for

# Add title
title(main="Chi-squared Distributions", line=0.5)
# Add legend
labelv <- paste("df", degf, sep="=")
legend("topright", inset=0.05, bty="n",
       title="Degrees of freedom", labelv,
       cex=0.8, lwd=6, lty=1,
       col=colorv)

x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
degf <- c(2, 5, 8, 11)  # df values
# Create plot colors
colorv <- c("red", "black", "blue", "green")
# Create legend labels
labelv <- paste("df", degf, sep="=")
# Plot an empty chart
xvar <- seq(0, 20, length=100)
plot(xvar, dchisq(xvar, df=degf[1]),
     type="n", xlab="", ylab="", ylim=c(0, 0.3))
# Add lines to plot
for (indeks in 1:4) {
  lines(xvar, dchisq(xvar, df=degf[indeks]),
lwd=2, col=colorv[indeks])
}  # end for

# Add title
title(main="Chi-squared Distributions", line=0.5)
# Add legend
legend("topright", inset=0.05,
       title="Degrees of freedom", labelv,
       cex=0.8, lwd=6, lty=1, bty="n", col=colorv)

x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
# Plot three curves in loop
degf <- c(3, 5, 9)  # Degrees of freedom
colorv <- c("black", "red", "blue", "green")
for (indeks in 1:NROW(degf)) {
curve(expr=df(x, df1=degf[indeks], df2=3),
xlim=c(0, 4), xlab="", ylab="", lwd=2,
col=colorv[indeks], add=as.logical(indeks-1))
}  # end for

# Add title
title(main="F-Distributions", line=0.5)
# Add legend
labelv <- paste("df", degf, sep="=")
legend("topright", inset=0.05, title="degrees of freedom",
       labelv, cex=0.8, lwd=2, lty=1,
       col=colorv)

x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
degf <- c(3, 6, 9)  # df values
colorv <- c("black", "red", "blue", "green")
labelv <- c("normal", paste("df", degf, sep="="))
# Plot a Normal probability distribution
curve(expr=dnorm, xlim=c(-4, 4),
      xlab="", ylab="", lwd=2)
for (indeks in 1:3) {  # Plot three t-distributions
curve(expr=dt(x, df=degf[indeks]),
      lwd=2, col=colorv[indeks+1], add=TRUE)
}  # end for

# Add title
title(main="t-distributions", line=0.5)
# Add legend
legend("topright", inset=0.05, bty="n",
       title="Degrees\n of freedom", labelv,
       cex=0.8, lwd=6, lty=1, col=colorv)

x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
xvar <- seq(-4, 4, length=100)
degf <- c(3, 6, 9)  # df values
colorv <- c("black", "red", "blue", "green")
labelv <- c("normal", paste("df", degf, sep="="))
# Plot chart of normal distribution
plot(xvar, dnorm(xvar), type="l",
     lwd=2, xlab="", ylab="")
for (indeks in 1:3) {  # Add lines for t-distributions
  lines(xvar, dt(xvar, df=degf[indeks]),
lwd=2, col=colorv[indeks+1])
}  # end for

# Add title
title(main="t-distributions", line=0.5)
# Add legend
legend("topright", inset=0.05, bty="n",
       title="Degrees\n of freedom", labelv,
       cex=0.8, lwd=6, lty=1, col=colorv)

x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
# Plot the Normal and Cauchy probability distributions
curve(expr=dnorm, xlim=c(-4, 4), xlab="", ylab="", lwd=2)
curve(expr=dcauchy, lwd=3, col="blue", add=TRUE)
# Add title
title(main="Cauchy and Normal Distributions", line=0.5)
# Add legend
legend("topright", inset=0.05, bty="n",
       title=NULL,leg=c("Normal", "Cauchy"),
       cex=0.8, lwd=6, lty=1, col=c("black", "blue"))

x11(width=6, height=5)  # Plot in window
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
# Define Pareto function
paretofun <- function(x, alpha) alpha*x^(-alpha-1)
colorv <- c("red", "blue", "green")
alphas <- c(1.0, 2.0, 3.0)
for (indeks in 1:3) {  # Plot three curves
  curve(expr=paretofun(x, alphas[indeks]),
  xlim=c(1, 2), ylim=c(0.0, 3.5), xlab="", ylab="",
  lwd=3, col=colorv[indeks], add=as.logical(indeks-1))
}  # end for
# Add title and legend
title(main="Pareto Distributions", line=0.5)
labelv <- paste("alpha", 1:3, sep=" = ")
legend("topright", inset=0.2, bty="n", y.intersp=0.4,
 title=NULL, labelv, cex=0.8, lwd=6, lty=1, col=colorv)

# Poisson frequency
eventv <- 0:11  # Poisson events
poissonf <- dpois(eventv, lambda=4)
names(poissonf) <- as.character(eventv)
# Poisson function
poissonfun <- function(x, lambda) {exp(-lambda)*lambda^x/factorial(x)}
curve(expr=poissonfun(x, lambda=4), xlim=c(0, 11), main="Poisson distribution",
xlab="No. of events", ylab="Frequency of events", lwd=2, col="blue")
legend(x="topright", legend="Poisson density", title="", bty="n",
 inset=0.05, cex=0.8, bg="white", lwd=6, lty=1, col="blue")
