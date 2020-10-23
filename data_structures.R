library(knitr)
opts_chunk$set(prompt=TRUE, eval=FALSE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='tiny', fig.width=4, fig.height=4)
options(digits=3)
options(width=80, dev='pdf')
thm <- knit_theme$get("acid")
knit_theme$set(thm)

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

my_var <- "hello"
c(typeof(my_var), mode(my_var), class(my_var))

my_var <- 1:5
c(typeof(my_var), mode(my_var), class(my_var))

my_var <- runif(5)
c(typeof(my_var), mode(my_var), class(my_var))

my_var <- matrix(1:10, 2, 5)
c(typeof(my_var), mode(my_var), class(my_var))

my_var <- matrix(runif(10), 2, 5)
c(typeof(my_var), mode(my_var), class(my_var))

my_var <- list(aa=c("a", "b"), bb=1:5)
c(typeof(my_var), mode(my_var), class(my_var))

my_var <- data.frame(aa=c("a", "b"), bb=1:2)
c(typeof(my_var), mode(my_var), class(my_var))

# A simple vector has no attributes
attributes(5:10)
my_var <- c(pi=pi, euler=exp(1), gamma=-digamma(1))
# Named vector has "names" attribute
attributes(my_var)
my_var <- 1:10
is.vector(my_var)  # Is the object a vector?
attributes(my_var) <- list(my_attr="foo")
my_var
is.vector(my_var)  # Is the object a vector?
my_var <- 0
attributes(my_var) <- list(class="Date")
my_var  # "Date" object
structure(0, class="Date")  # "Date" object

my_var <- matrix(runif(10), 2, 5)
class(my_var)  # Has implicit class
# but no explicit "class" attribute
attributes(my_var)
c(typeof(my_var), mode(my_var), class(my_var))
# Assign explicit "class" attribute
class(my_var) <- "my_class"
class(my_var)  # Has explicit "class"
# Has explicit "class" attribute
attributes(my_var)
is.matrix(my_var)  # Is the object a matrix?
is.vector(my_var)  # Is the object a vector?
attributes(unclass(my_var))

# Integer implicit class derived from type
my_var <- vector(mode="integer", length=10)
c(typeof(my_var), mode(my_var), class(my_var))
# Numeric implicit class derived from mode
my_var <- vector(mode="numeric", length=10)
c(typeof(my_var), mode(my_var), class(my_var))
# Adding dim attribute changes implicit class to matrix
dim(my_var) <- c(5, 2)
c(typeof(my_var), mode(my_var), class(my_var))
# Data frames have implicit dim attribute
my_var <- data.frame(aa=c("a", "b"), bb=1:2)
c(typeof(my_var), mode(my_var), class(my_var))
attributes(my_var)
dim(my_var)

my_var <- 1:5
c(typeof(my_var), mode(my_var), class(my_var))
mode(my_var) <- "character"  # Coerce to "character"
my_var
c(typeof(my_var), mode(my_var), class(my_var))
# Explicitly coerce to "character"
my_var <- as.character(1:5)
c(typeof(my_var), mode(my_var), class(my_var))
mat_rix <- matrix(1:10, 2, 5)  # Create matrix
# Explicitly coerce to "character"
mat_rix <- as.character(mat_rix)
c(typeof(mat_rix), mode(mat_rix), class(mat_rix))
# Coercion converted matrix to vector
c(is.matrix(mat_rix), is.vector(mat_rix))
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
vec_tor <- c(8, 6, 5, 7)  # Create vector
vec_tor
vec_tor[2]  # Extract second element
# Extract all elements, except the second element
vec_tor[-2]
# Create Boolean vector
c(FALSE, TRUE, TRUE)
# Extract second and third elements
vec_tor[c(FALSE, TRUE, TRUE)]
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
vec_tor <- c(8, 6, 5, 7)  # Create vector
vec_tor
# Boolean vector TRUE if element is equal to second one
vec_tor == vec_tor[2]
# Boolean vector TRUE for elements greater than six
vec_tor > 6
2*vec_tor  # Multiply all elements by 2
vec_tor^2  # Square all elements
c(11, 5:10)  # Combine two vectors
c(vec_tor, 2.0)  # Append number to vector

vec_tor <- # Create named vector
  c(pi_const=pi, euler=exp(1), gamma=-digamma(1))
vec_tor
names(vec_tor)  # Get names of elements
vec_tor["euler"]  # Get element named "euler"
names(vec_tor) <- c("pie","eulery","gammy")  # Rename elements
vec_tor
unname(vec_tor)  # Remove names attribute
letters[5:10]  # Vector of letters
c("a", letters[5:10])  # Combine two vectors of letters
# Create named vector
structure(sample(1:5), names=paste0("el", 1:5))

vec_tor  # Named vector
# Extract second element
vec_tor[2]
# Extract all elements, except the second element
vec_tor[-2]
# Extract zero elements - returns zero-length vector
vec_tor[0]
# Extract second and third elements
vec_tor[c(FALSE, TRUE, TRUE)]
# Extract elements using their names
vec_tor["eulery"]
# Extract elements using their names
vec_tor[c("pie", "gammy")]
# Subset whole vector
vec_tor[] <- 0

vec_tor <- runif(5)
vec_tor
vec_tor > 0.5  # Boolean vector
# Boolean vector of elements equal to the second one
vec_tor == vec_tor[2]
# Extract all elements equal to the second one
vec_tor[vec_tor == vec_tor[2]]
vec_tor < 1  # Boolean vector of elements less than one
# Extract all elements greater than one
vec_tor[vec_tor > 1]
vec_tor[vec_tor > 0.5]  # Filter elements > 0.5
which(vec_tor > 0.5)  # Index of elements > 0.5

# Create factor vector
fac_tor <- factor(c("b", "c", "d", "a", "c", "b"))
fac_tor
fac_tor[3]
attributes(fac_tor)  # Get factor attributes
levels(fac_tor)  # Get allowed values
as.numeric(fac_tor)  # Get encoding vector
is.vector(fac_tor)
as.factor(1:5)  # Coerce vector to factor
# Coerce factor to character vector
as.vector(as.factor(1:5))

fac_tor
levels(fac_tor)  # Get allowed values
unique(fac_tor)  # Get unique elements
# Get contingency (frequency) table
table(fac_tor)
# Get contingency table using sapply
sapply(levels(fac_tor),
 function(le_vel) {
   sum(le_vel == fac_tor)
 })  # end sapply

library(microbenchmark)
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
findInterval(x=1:8, vec=c(3, 5, 7),
       all.inside=TRUE)
# make rightmost interval inclusive
findInterval(x=1:8, vec=c(3, 5, 7),
       rightmost.closed=TRUE)

# Named numeric vector of breakpoints
brea_ks <- c(freezing=0, very_cold=30,
       cold=50, pleasant=60,
       warm=80, hot=90)
brea_ks
tempe_ratures <- runif(10, min=10, max=100)
feels_like <- names(
  brea_ks[findInterval(x=tempe_ratures,
                 vec=brea_ks)])
names(tempe_ratures) <- feels_like
tempe_ratures

library(microbenchmark)
da_ta <- sample(0:6) + 0.1
da_ta
cut(x=da_ta, breaks=c(2, 4, 6, 8))
rbind(da_ta, cut(x=da_ta, breaks=c(2, 4, 6, 8)))
# cut() replicates findInterval()
cut(x=1:8, breaks=c(3, 5, 7), labels=1:2,
    right=FALSE)
findInterval(x=1:8, vec=c(3, 5, 7))
# findInterval() is a compiled function, so it's faster than cut()
vec_tor <- rnorm(1000)
summary(microbenchmark(
  find_interval=
    findInterval(x=vec_tor, vec=c(3, 5, 7)),
  cuut=
    cut(x=vec_tor, breaks=c(3, 5, 7)),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# Calculate VTI percentage returns
re_turns <- na.omit(rutils::etf_env$re_turns$VTI)
# Plot histogram
x11(width=6, height=5)
par(mar=c(1, 1, 1, 1), oma=c(2, 2, 2, 0))
histo_gram <- hist(re_turns, breaks=100,
  main="", ylim=c(0, 60), xlim=c(-0.04, 0.04),
  xlab="", ylab="", freq=FALSE)

# Draw kernel density of histogram
lines(density(re_turns), col="red", lwd=2)
# Add density of normal distribution
curve(expr=dnorm(x, mean=mean(re_turns),
  sd=sd(re_turns)), add=TRUE, type="l",
  lwd=2, col="blue")
title(main="VTI Return Distribution", line=0)
# Add legend
legend("topright", inset=0.05, cex=0.8, title=NULL,
  leg=c("VTI", "Normal"), bty="n",
  lwd=6, bg="white", col=c("red", "blue"))
# Total area under histogram
diff(histo_gram$breaks) %*% histo_gram$density

mat_rix <- matrix(5:10, nrow=2, ncol=3)  # Create a matrix
mat_rix  # by default matrices are constructed column-wise
# Create a matrix row-wise
matrix(5:10, nrow=2, byrow=TRUE)
mat_rix[2, 3]  # Extract third element from second row
mat_rix[2, ]  # Extract second row
mat_rix[, 3]  # Extract third column
mat_rix[, c(1,3)]  # Extract first and third column
mat_rix[, -2]  # Remove second column
# Subset whole matrix
mat_rix[] <- 0
# Get the number of rows or columns
nrow(vec_tor); ncol(vec_tor)
NROW(vec_tor); NCOL(vec_tor)
nrow(mat_rix); ncol(mat_rix)
NROW(mat_rix); NCOL(mat_rix)

attributes(mat_rix)  # Get matrix attributes
dim(mat_rix)  # Get dimension attribute
class(mat_rix)  # Get class attribute
rownames(mat_rix) <- c("row1", "row2")  # Rownames attribute
colnames(mat_rix) <- c("col1", "col2", "col3")  # Colnames attribute
mat_rix
mat_rix["row2", "col3"]  # Third element from second row
names(mat_rix)  # Get the names attribute
dimnames(mat_rix)  # Get dimnames attribute
attributes(mat_rix)  # Get matrix attributes

mat_rix  # matrix with column names
mat_rix[1, ]  # Subset rows by index
mat_rix[, "col1"]  # Subset columns by name
mat_rix[, c(TRUE, FALSE, TRUE)]  # Subset columns Boolean vector
mat_rix[1, ]  # Subsetting can produce a vector!
class(mat_rix); class(mat_rix[1, ])
is.matrix(mat_rix[1, ]); is.vector(mat_rix[1, ])
mat_rix[1, , drop=FALSE]  # Drop=FALSE preserves matrix
class(mat_rix[1, , drop=FALSE])
is.matrix(mat_rix[1, , drop=FALSE]); is.vector(mat_rix[1, , drop=FALSE])

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

as.list(c(1,2,3))
list(c(1,2,3))

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

# Create a named vector
stu_dents <- sample(round(runif(5, min=1, max=10), digits=2))
names(stu_dents) <- c("Angie", "Chris", "Suzie", "Matt", "Liz")
# Sort the vector into ascending order
sort(stu_dents)
# Calculate index to sort into ascending order
order(stu_dents)
# Sort the vector into ascending order
stu_dents[order(stu_dents)]
# Calculate the sorted (ordered) vector
sort_ed <- stu_dents[order(stu_dents)]
# Calculate index to sort into unsorted (original) order
order(order(stu_dents))
sort_ed[order(order(stu_dents))]
stu_dents
# Create a data frame of stu_dents and their ranks
ra_nks <- c("first", "second", "third", "fourth", "fifth")
data.frame(students=stu_dents,
  rank=ra_nks[order(order(stu_dents))])
# Permute data_frame of flowers on price column
order(data_frame$price)
# Sort data_frame on price
data_frame[order(data_frame$price), ]
# Sort data_frame on color
data_frame[order(data_frame$color), ]
# Read the Examples for sort()
order(c(2, 1:4))  # There's a tie
order(c(2, 1:4), 1:5)  # There's a tie

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
  as_data_frame_matrix=
    as.data.frame.matrix(mat_rix),
  as_data_frame=as.data.frame(mat_rix),
  data_frame=data.frame(mat_rix),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

library(microbenchmark)
# lapply is faster than coercion function
summary(microbenchmark(
  as_list=
    as.list(as.data.frame.matrix(mat_rix)),
  l_apply=
    lapply(seq_along(mat_rix[1, ]),
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

rm(list=ls())
# Airquality data has some NAs
head(airquality)
dim(airquality)
# Number of NA elements
sum(is.na(airquality))
# Number of rows with NA elements
sum(!complete.cases(airquality))
# Display rows containing NAs
head(airquality[!complete.cases(airquality), ])

rm(list=ls())
# Remove rows containing NAs
good_air <- airquality[complete.cases(airquality), ]
dim(good_air)
head(good_air)  # NAs removed
library(zoo)  # load package zoo
# Replace NAs
good_air <- zoo::na.locf(airquality)
dim(good_air)
head(good_air)  # NAs replaced
# Create vector containing NA values
vec_tor <- sample(22)
vec_tor[sample(NROW(vec_tor), 4)] <- NA
# Replace NA values with the most recent non-NA values
zoo::na.locf(vec_tor)
# Replace NAs in xts time series
se_ries <- rutils::etf_env$price_s[, 1]
head(se_ries)
sum(is.na(se_ries))
library(quantmod)
series_zoo <- as.xts(zoo::na.locf(se_ries, fromLast=TRUE))
series_xts <- xts:::na.locf.xts(se_ries, fromLast=TRUE)
all.equal(series_zoo, series_xts, check.attributes=FALSE)
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
