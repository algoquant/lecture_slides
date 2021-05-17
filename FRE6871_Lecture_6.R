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

# Permutation index on price column
order(data_frame$price)
# Sort data_frame on price column
data_frame[order(data_frame$price), ]
# Sort data_frame on color column
data_frame[order(data_frame$color), ]
# Create a vector of student ranks
ra_nks <- c("first", "second", "third", "fourth", "fifth")
# Reverse sort the student ranks according to students
ra_nks[order(order(score_s))]
# Create a data frame of students and their ranks
ros_ter <- data.frame(score=score_s, 
  rank=ra_nks[order(order(score_s))])
ros_ter

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

# Airquality data has some NAs
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
with(mtcars,
     sapply(sort(unique(cyl)), function(x) {
 structure(mean(mpg[x==cyl]), names=x)
     }, USE.NAMES=TRUE))  # end with

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

# Download CRSPpanel.txt from NYU Classes
# Read the file using read.table() with header and sep arguments
panel_data <- read.table(file="C:/Develop/lecture_slides/data/CRSPpanel.txt",
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

par(oma=c(1, 1, 1, 1), mar=c(2, 1, 1, 1), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
lamb_da <- c(0.5, 1, 1.5)
col_ors <- c("red", "blue", "green")
# Plot three curves in loop
for (in_dex in 1:3) {
  curve(expr=plogis(x, scale=lamb_da[in_dex]),
xlim=c(-4, 4), type="l", xlab="", ylab="", lwd=4,
col=col_ors[in_dex], add=(in_dex>1))
}  # end for

# Add title
title(main="Logistic function", line=0.5)
# Add legend
legend("topleft", title="Scale parameters",
       paste("lambda", lamb_da, sep="="),
       inset=0.05, cex=0.8, lwd=6, bty="n",
       lty=1, col=col_ors)

set.seed(1121)
# Simulate overlapping scores data
sample1 <- runif(100, max=0.6)
sample2 <- runif(100, min=0.4)
# Perform Wilcoxon test for mean
wilcox.test(sample1, sample2)
# Combine scores and add categorical variable
predic_tor <- c(sample1, sample2)
res_ponse <- c(logical(100), !logical(100))
# Perform logit regression
g_lm <- glm(res_ponse ~ predic_tor, family=binomial(logit))
class(g_lm)
summary(g_lm)

x11(width=6, height=5)
par(mar=c(3, 3, 2, 2), mgp=c(2, 1, 0), oma=c(0, 0, 0, 0))
or_der <- order(predic_tor)
plot(x=predic_tor[or_der], y=g_lm$fitted.values[or_der],
     main="Category Densities and Logistic Function",
     type="l", lwd=4, col="orange", xlab="score", ylab="density")
den_sity <- density(predic_tor[res_ponse])
den_sity$y <- den_sity$y/max(den_sity$y)
lines(den_sity, col="red")
polygon(c(min(den_sity$x), den_sity$x, max(den_sity$x)), c(min(den_sity$y), den_sity$y, min(den_sity$y)), col=rgb(1, 0, 0, 0.2), border=NA)
den_sity <- density(predic_tor[!res_ponse])
den_sity$y <- den_sity$y/max(den_sity$y)
lines(den_sity, col="blue")
polygon(c(min(den_sity$x), den_sity$x, max(den_sity$x)), c(min(den_sity$y), den_sity$y, min(den_sity$y)), col=rgb(0, 0, 1, 0.2), border=NA)
# Add legend
legend(x="top", cex=1.0, bty="n", lty=c(1, NA, NA),
 lwd=c(6, NA, NA), pch=c(NA, 15, 15),
 legend=c("logistic fit", "TRUE", "FALSE"),
 col=c("orange", "red", "blue"),
 text.col=c("black", "red", "blue"))

library(ISLR)  # Load package ISLR
# get documentation for package tseries
packageDescription("ISLR")  # get short description

help(package="ISLR")  # Load help page

library(ISLR)  # Load package ISLR

data(package="ISLR")  # list all datasets in ISLR

ls("package:ISLR")  # list all objects in ISLR

detach("package:ISLR")  # Remove ISLR from search path

# Coerce the student and default columns into Boolean
Default <- ISLR::Default
Default$default <- (Default$default == "Yes")
Default$student <- (Default$student == "Yes")
colnames(Default)[1:2] <- c("de_fault", "stu_dent")
attach(Default)  # Attach Default to search path
# Explore credit default data
summary(Default)
sapply(Default, class)
dim(Default); head(Default)

# Plot data points for non-defaulters
x11(width=6, height=5)
x_lim <- range(balance); y_lim <- range(income)
plot(income ~ balance,
     main="Default Dataset from Package ISLR",
     xlim=x_lim, ylim=y_lim, pch=4, col="blue",
     data=Default[!de_fault, ])
# Plot data points for defaulters
points(income ~ balance, pch=4, lwd=2, col="red",
 data=Default[de_fault, ])
# Add legend
legend(x="topright", legend=c("non-defaulters", "defaulters"),
 bty="n", col=c("blue", "red"), lty=1, lwd=6, pch=4)

# Wilcoxon test for balance predictor
wilcox.test(balance[de_fault], balance[!de_fault])
# Wilcoxon test for income predictor
wilcox.test(income[de_fault], income[!de_fault])

x11(width=6, height=5)
par(mfrow=c(1,2))  # Set plot panels
# Balance boxplot
boxplot(formula=balance ~ de_fault,
  col="lightgrey", main="balance", xlab="Default")
# Income boxplot
boxplot(formula=income ~ de_fault,
  col="lightgrey", main="income", xlab="Default")

# Fit logistic regression model
g_lm <- glm(de_fault ~ balance, family=binomial(logit))
class(g_lm)
summary(g_lm)

x11(width=6, height=5)
par(mar=c(4, 4, 2, 2), oma=c(0, 0, 0, 0), mgp=c(2.5, 1, 0))
plot(x=balance, y=de_fault,
     main="Logistic Regression of Credit Defaults",
     col="orange", xlab="credit balance", ylab="defaults")
or_der <- order(balance)
lines(x=balance[or_der], y=g_lm$fitted.values[or_der], col="blue", lwd=3)
legend(x="topleft", inset=0.1, bty="n", lwd=6,
 legend=c("defaults", "logit fitted values"),
 col=c("orange", "blue"), lty=c(NA, 1), pch=c(1, NA))

# Calculate cumulative defaults
to_tal <- sum(de_fault)
default_s <- sapply(balance, function(lim_it) {
    sum(de_fault[balance <= lim_it])
})  # end sapply
# Perform logit regression
g_lm <- glm(cbind(default_s, to_tal-default_s) ~ balance,
  family=binomial(logit))
summary(g_lm)

plot(x=balance, y=default_s/to_tal, col="orange", lwd=1,
     main="Cumulative Defaults Versus Balance",
     xlab="credit balance", ylab="cumulative defaults")
or_der <- order(balance)
lines(x=balance[or_der], y=g_lm$fitted.values[or_der],
col="blue", lwd=3)
legend(x="topleft", inset=0.1, bty="n",
 legend=c("cumulative defaults", "fitted values"),
 col=c("orange", "blue"), lty=c(NA, 1), pch=c(1, NA), lwd=6)

# Fit multifactor logistic regression model
col_names <- colnames(Default)
for_mula <- as.formula(paste(col_names[1],
  paste(col_names[-1], collapse="+"), sep=" ~ "))
for_mula
g_lm <- glm(for_mula, data=Default, family=binomial(logit))
summary(g_lm)

# Calculate cumulative defaults
cum_defaults <- sapply(balance, function(lim_it) {
c(student=sum(de_fault[stu_dent & (balance <= lim_it)]),
  non_student=sum(de_fault[!stu_dent & (balance <= lim_it)]))
})  # end sapply
total_defaults <- c(student=sum(stu_dent & de_fault),
      stu_dent=sum(!stu_dent & de_fault))
cum_defaults <- t(cum_defaults / total_defaults)

# Plot cumulative defaults
par(mfrow=c(1,2))  # Set plot panels
or_der <- order(balance)
plot(x=balance[or_der], y=cum_defaults[or_der, 1],
     col="red", t="l", lwd=2, xlab="credit balance", ylab="",
     main="Cumulative defaults of\n students and non-students")
lines(x=balance[or_der], y=cum_defaults[or_der, 2], col="blue", lwd=2)
legend(x="topleft", bty="n",
 legend=c("students", "non-students"),
 col=c("red", "blue"), text.col=c("red", "blue"), lwd=3)
# Balance boxplot for student factor
boxplot(formula=balance ~ !stu_dent,
  col="lightgrey", main="balance", xlab="Student")

# Perform in-sample forecast from logistic regression model
forecast_s <- predict(g_lm, type="response")
all.equal(g_lm$fitted.values, forecast_s)
# Define discrimination threshold value
thresh_old <- 0.7
# Calculate confusion matrix in-sample
table(actual=!de_fault, forecast=(forecast_s < thresh_old))
# Fit logistic regression over training data
set.seed(1121)  # Reset random number generator
n_rows <- NROW(Default)
sampl_e <- sample.int(n=n_rows, size=n_rows/2)
train_data <- Default[sampl_e, ]
g_lm <- glm(for_mula, data=train_data, family=binomial(logit))
# Forecast over test data out-of-sample
test_data <- Default[-sampl_e, ]
forecast_s <- predict(g_lm, newdata=test_data, type="response")
# Calculate confusion matrix out-of-sample
table(actual=!test_data$de_fault, 
forecast=(forecast_s < thresh_old))

# Calculate confusion matrix out-of-sample
confu_sion <- table(actual=!test_data$de_fault, 
forecast=(forecast_s < thresh_old))
confu_sion
# Calculate FALSE positive (type I error)
sum(!test_data$de_fault & (forecast_s > thresh_old))
# Calculate FALSE negative (type II error)
sum(test_data$de_fault & (forecast_s < thresh_old))

# Calculate FALSE positive and FALSE negative rates
confu_sion <- confu_sion / rowSums(confu_sion)
c(typeI=confu_sion[2, 1], typeII=confu_sion[1, 2])
detach(Default)
# Below is an unsuccessful attempt to draw confusion matrix using xtable
confusion_matrix <- matrix(c("| true positive \\\\ (sensitivity)", "| false negative \\\\ (type II error)", "| false positive \\\\ (type I error)", "| true negative \\\\ (specificity)"), nc=2)
dimnames(confusion_matrix) <- list(forecast=c("FALSE", "TRUE"),
                             actual=c("FALSE", "TRUE"))
print(xtable::xtable(confusion_matrix,
caption="Confusion Matrix"),
caption.placement="top",
comment=FALSE, size="scriptsize",
include.rownames=TRUE,
include.colnames=TRUE)
# end unsuccessful attempt to draw confusion table using xtable

# Confusion matrix as function of thresh_old
con_fuse <- function(actu_al, forecast_s, thresh_old) {
    confu_sion <- table(actu_al, (forecast_s < thresh_old))
    confu_sion <- confu_sion / rowSums(confu_sion)
    c(typeI=confu_sion[2, 1], typeII=confu_sion[1, 2])
  }  # end con_fuse
con_fuse(!test_data$de_fault, forecast_s, thresh_old=thresh_old)
# Define vector of discrimination thresholds
threshold_s <- seq(0.05, 0.95, by=0.05)^2
# Calculate error rates
error_rates <- sapply(threshold_s, con_fuse,
  actu_al=!test_data$de_fault, forecast_s=forecast_s)  # end sapply
error_rates <- t(error_rates)
rownames(error_rates) <- threshold_s
error_rates <- rbind(c(1, 0), error_rates)
error_rates <- rbind(error_rates, c(0, 1))
# Calculate area under ROC curve (AUC)
true_pos <- (1 - error_rates[, "typeII"])
true_pos <- (true_pos + rutils::lag_it(true_pos))/2
false_pos <- rutils::diff_it(error_rates[, "typeI"])
abs(sum(true_pos*false_pos))

# Plot ROC Curve for Defaults
x11(width=5, height=5)
plot(x=error_rates[, "typeI"], y=1-error_rates[, "typeII"],
     xlab="FALSE positive rate", ylab="TRUE positive rate",
     main="ROC Curve for Defaults", type="l", lwd=3, col="blue")
abline(a=0.0, b=1.0, lwd=3, col="orange")
