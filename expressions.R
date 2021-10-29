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
vec_tor <- c(2, 4, 6)
# Works (does nothing) using '&&'
if (is.matrix(vec_tor) && (vec_tor[2, 3] > 0)) {
  vec_tor[2, 3] <- 1
}
# No short-circuit so fails (produces an error)
if (is.matrix(vec_tor) & (vec_tor[2, 3] > 0)) {
  vec_tor[2, 3] <- 1
}
?Arithmetic
4.7 * 0.5  # Multiplication
4.7 / 0.5  # division
# Exponentiation
2**3
2^3
num_var <- 2
num_var==2
identical(num_var, 2)
identical(num_var, NULL)
# This doesn't work:
# num_var==NULL
is.null(num_var)
vec_tor <- c(2, 4, 6)
vec_tor==2
identical(vec_tor, 2)
# num_ber is equal to "1.0" within machine precision
num_ber <- 1.0 + 2*sqrt(.Machine$double.eps)
all.equal(num_ber, 1.0)
# Info machine precision of computer R is running on
# ?.Machine
# Machine precision
.Machine$double.eps
vec_tor <- sample(1e3, 1e3)
mat_rix <- matrix(vec_tor, ncol=4)
which(vec_tor == 5)
match(5, vec_tor)
# Equivalent but slower than above
(1:NROW(vec_tor))[vec_tor == 5]
which(vec_tor < 5)
# Find indices of TRUE elements of Boolean matrix
which((mat_rix == 5)|(mat_rix == 6), arr.ind=TRUE)
# Equivalent but slower than above
arrayInd(which((mat_rix == 5)|(mat_rix == 6)),
   dim(mat_rix), dimnames(mat_rix))
# Find index of largest element
which.max(vec_tor)
which(vec_tor == max(vec_tor))
# Find index of smallest element
which.min(vec_tor)
# Benchmark match() versus which()
all.equal(match(5, vec_tor),
    min(which(vec_tor == 5)))
library(microbenchmark)
summary(microbenchmark(
  match=match(5, vec_tor),
  which=min(which(vec_tor == 5)),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Does 5 belong in vec_tor?
5 %in% vec_tor
match(5, vec_tor, nomatch=0) > 0
# Does (-5) belong in vec_tor?
(-5) %in% vec_tor
c(5, -5) %in% vec_tor
match(-5, vec_tor)
# Equivalent to "5 %in% vec_tor"
any(vec_tor == 5)
# Equivalent to "(-5) %in% vec_tor"
any(vec_tor == (-5))
# Any negative values in vec_tor?
any(vec_tor < 0)
# Example of use in if() statement
if (any(vec_tor < 2))
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
findInterval(x=1:8, vec=c(3, 5, 7),
       all.inside=TRUE)
# Make rightmost interval inclusive
findInterval(x=1:8, vec=c(3, 5, 7),
       rightmost.closed=TRUE)
num_var1 <- 3  # "<-" and "=" are valid assignment operators
num_var1
num_var1 = 3
num_var1
2<-3  # "<" operator confused with "<-"
2 < -3  # Add space or brackets to avoid confusion
# "=" assignment within argument list
median(x=1:10)
x  # x doesn't exist outside the function
# "<-" assignment within argument list
median(x <- 1:10)
x  # x exists outside the function
my_var <- 1  # Create new object
assign(x="my_var", value=2)  # Assign value to existing object
my_var
rm(my_var)  # Remove my_var
assign(x="my_var", value=3)  # Create new object from name
my_var
# Create new object in new environment
new_env <- new.env()  # Create new environment
assign("my_var", 3, envir=new_env)  # Assign value to name
ls(new_env)  # List objects in "new_env"
new_env$my_var
rm(list=ls())  # delete all objects
sym_bol <- "my_var"  # define symbol containing string "my_var"
assign(sym_bol, 1)  # Assign value to "my_var"
ls()
my_var
assign("sym_bol", "new_var")
assign(sym_bol, 1)  # Assign value to "new_var"
ls()
sym_bol <- 10
assign(sym_bol, 1)  # Can't assign to non-string
rm(list=ls())  # delete all objects
# Create individual vectors from column names of EuStockMarkets
for (col_name in colnames(EuStockMarkets)) {
# Assign column values to column names
  assign(col_name, EuStockMarkets[, col_name])
}  # end for
ls()
head(DAX)
head(EuStockMarkets[, "DAX"])
identical(DAX, EuStockMarkets[, "DAX"])
# Create new environment
test_env <- new.env()
# Pass string as name to create new object
assign("my_var1", 2, envir=test_env)
# Create new object using $ string referencing
test_env$my_var2 <- 1
# List objects in new environment
ls(test_env)
# Reference an object by name
test_env$my_var1
# Reference an object by string name using get
get("my_var1", envir=test_env)
# Retrieve and assign value to object
assign("my_var1",
       2*get("my_var1", envir=test_env),
       envir=test_env)
get("my_var1", envir=test_env)
# Return all objects in an environment
mget(ls(test_env), envir=test_env)
# delete environment
rm(test_env)
rm(list=ls())  # delete all objects
# Convert string to symbol
as.symbol("some_string")
# The "name" class is synonymous with a symbol
class(as.symbol("some_string"))
# Symbols are created during assignments
sym_bol <- 2
# Evaluate symbol (same as typing it)
eval(sym_bol)
# Convert string into a symbol and evaluate it
eval(as.symbol("sym_bol"))
# Convert string into unevaluated expression
ex_pression <- parse(text="ne_w <- sym_bol")
ex_pression
class(ex_pression)
ls()
eval(ex_pression)  # Evaluate expression
ls()  # Expression evaluation created new object
ne_w
# Create the expression "1+1"
quote(1+1)
# Evaluate the expression "1+1"
eval(quote(1+1))
# Create an expression containing several commands
ex_pression <- quote({x <- 1; y <- 2; x+y})
ex_pression
# Evaluate all the commands in the expression
eval(ex_pression)
ls()
# Return an expression without evaluating it
ne_w <- 2*sym_bol
ex_pression <- quote(sym_bol + ne_w)
ex_pression
eval(ex_pression)  # Evaluate expression
# Substitute objects in an expression
ex_pression <- substitute(sym_bol + ne_w,
              env=list(sym_bol=1, ne_w=2))
ex_pression
eval(ex_pression)  # Evaluate expression
# Get_input() substitutes its formal argument with the actual argument
get_input <- function(in_put) {
  substitute(in_put)
}  # end get_input
my_var <- 2
get_input(my_var)
eval(get_input(my_var))
# Define symbol
my_var <- 10
# Convert symbol value into string
deparse(my_var)
# Convert symbol into string without evaluating it
deparse(quote(my_var))
# Substitute object with value from named list
sym_bol <- 2
deparse(substitute(sym_bol + my_var,
       env=list(my_var=2)))
# Create string with name of input argument
get_name <- function(in_put) {
  names(in_put) <- deparse(substitute(in_put))
  in_put
}  # end get_name
get_name(my_var)
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
mat_rix <- matrix(nr=3, nc=4)
mat_rix <- 0
# Subset whole matrix
mat_rix[] <- 0
# Parenthesis and braces require a little additional processing time
library(microbenchmark)
summary(microbenchmark(
  ba_se=sqrt(rnorm(10000)^2),
  pa_ren=sqrt(((((rnorm(10000)^2))))),
  bra_ce=sqrt({{{{rnorm(10000)^2}}}}),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
rm(list=ls())
num_var1 <- 1
if (num_var1) {  # Numeric zero is FALSE, all other numbers are TRUE
  num_var2 <- 4
} else if (num_var1 == 0) {  # 'else if' together on same line
  num_var2 <- 0
} else {  # 'else' together with curly braces
  num_var2 <- -4
}  # end if
num_var2
switch("a", a="aaahh", b="bee", c="see", d=2,
       "else this")
switch("c", a="aaahh", b="bee", c="see", d=2,
       "else this")
switch(3, a="aaahh", b="bee", c="see", d=2,
       "else this")
switch("cc", a="aaahh", b="bee", c="see", d=2,
       "else this")
# Measure of central tendency
centra_lity <- function(in_put,
    meth_od=c("mean", "mean_narm", "median")) {
# validate "meth_od" argument
  meth_od <- match.arg(meth_od)
  switch(meth_od,
 mean=mean(in_put),
 mean_narm=mean(in_put, na.rm=TRUE),
 median=median(in_put))
}  # end centra_lity
my_var <- rnorm(100, mean=2)
centra_lity(my_var, "mean")
centra_lity(my_var, "mean_narm")
centra_lity(my_var, "median")
for (in_dex in vec_tor) {ex_pressions}
rm(list=ls())
color_list <- list("red", "white", "blue")
# Loop over list
for (some_color in color_list) {
  print(some_color)
}  # end for
# Loop over vector
for (in_dex in 1:3) {
  print(color_list[[in_dex]])
}  # end for
# While loops require initialization
in_dex <- 1
# While loop
while (in_dex < 4) {
  print(color_list[[in_dex]])
  in_dex <- in_dex + 1
}  # end while
vec_tor <- integer(7)
# Loop over a vector and overwrite it
for (i in seq_along(vec_tor)) {
  cat("Changing element:", i, "\n")
  vec_tor[i] <- i^2
}  # end for
# Modifying vec_tor inside sapply() has no effect
vec_tor <- integer(7)
vec_tor
sapply(seq_along(vec_tor),
 function(i) {
   vec_tor[i] <- i^2
 })  # end sapply
vec_tor
# Super-assignment operator "<<-" allows modifying vec_tor
sapply(seq_along(vec_tor),
 function(i) {
   vec_tor[i] <<- i^2 # "<<-" !!!
 })  # end sapply
vec_tor
# sapply() loop returns vector of values
vec_tor <- sapply(seq_along(vec_tor),
            function(i) (i^2))
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
vec_tor <- sample(1:9)
vec_tor
vec_tor < 5  # Element-wise comparison
vec_tor == 5  # Element-wise comparison
mat_rix <- matrix(vec_tor, ncol=3)
mat_rix
mat_rix < 5  # Element-wise comparison
mat_rix == 5  # Element-wise comparison
mat_rix <- 1:6  # Create a vector
class(mat_rix)  # Get its class
# Is it vector or matrix?
c(is.vector(mat_rix), is.matrix(mat_rix))
structure(mat_rix, dim=c(2, 3))  # Matrix object
# Adding dimension attribute coerces into matrix
dim(mat_rix) <- c(2, 3)
class(mat_rix)  # Get its class
# Is it vector or matrix?
c(is.vector(mat_rix), is.matrix(mat_rix))
# Assign dimnames attribute
dimnames(mat_rix) <- list(rows=c("row1", "row2"),
            columns=c("col1", "col2", "col3"))
mat_rix
mat_rix <- matrix(1:10, 2, 5)  # Create matrix
mat_rix
# as.numeric strips dim attribute from matrix
as.numeric(mat_rix)
# Explicitly coerce to "character"
mat_rix <- as.character(mat_rix)
c(typeof(mat_rix), mode(mat_rix), class(mat_rix))
# Coercion converted matrix to vector
c(is.matrix(mat_rix), is.vector(mat_rix))
vector1 <- 1:3  # define vector
vector2 <- 6:4  # define vector
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
# define vector and matrix
vector1 <- c(2, 4, 3)
mat_rix <- matrix(sample(1:12), ncol=3)
# Multiply columns of matrix by vector
vector1*mat_rix
# Or
mat_rix*vector1
# Multiply rows of matrix by vector
t(vector1*t(mat_rix))
# Multiply rows of matrix by vector - transpose is very slow
pro_duct <- lapply(1:NCOL(mat_rix), 
  function(x) vector1[x]*mat_rix[, x])
do.call(cbind, pro_duct)
library(microbenchmark)
summary(microbenchmark(
  trans=t(vector1*t(mat_rix)),
  lapp={
    pro_duct <- lapply(1:NCOL(mat_rix), function(x) vector1[x]*mat_rix[, x])
    do.call(cbind, pro_duct)
  },
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
vector1
vector2 <- 6:4  # define vector
# Multiply two vectors element-by-element
vector1 * vector2
# Calculate inner product
vector1 %*% vector2
# Calculate inner product and drop dimensions
drop(vector1 %*% vector2)
# Multiply columns of matrix by vector
mat_rix %*% vector1  # Single column matrix
drop(mat_rix %*% vector1)  # vector
rowSums(t(vector1 * t(mat_rix)))
# using rowSums() and t() is 10 times slower than %*%
library(microbenchmark)
summary(microbenchmark(
  in_ner=drop(mat_rix %*% vector1),
  row_sums=rowSums(t(vector1 * t(mat_rix))),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
library(microbenchmark)
# Multiply matrix by vector fails because dimensions aren't conformable
vector1 %*% mat_rix
# Works after transpose
drop(vector1 %*% t(mat_rix))
# Calculate inner product
crossprod(vector1, vector2)
# Create matrix and vector
mat_rix <- matrix(1:3000, ncol=3)
tmat_rix <- t(mat_rix)
vec_tor <- 1:3
# crossprod() is slightly faster than "%*%" operator
summary(microbenchmark(
  cross_prod=crossprod(tmat_rix, vec_tor),
  inner_prod=mat_rix %*% vec_tor,
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# define named vectors
vector1 <- sample(1:4)
names(vector1) <- paste0("row", 1:4, "=", vector1)
vector1
vector2 <- sample(1:3)
names(vector2) <- paste0("col", 1:3, "=", vector2)
vector2
# Calculate outer product of two vectors
mat_rix <- outer(vector1, vector2)
mat_rix
# Calculate vectorized function spanned over two vectors
mat_rix <- outer(vector1, vector2,
           FUN=function(x1, x2) x2*sin(x1))
mat_rix
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
    cat("in_put =", in_put)
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
