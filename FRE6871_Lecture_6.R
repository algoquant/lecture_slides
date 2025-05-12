# Load and plot intraday stock prices
load("/Users/jerzy/Develop/lecture_slides/data/xlk_tick_trades_20200316.RData")
pricev <- xlk$price
dygraphs::dygraph(pricev, main="XLK Intraday Prices for 2020-03-16") %>%
  dyOptions(colors="blue", strokeWidth=1)
# Calculate the lagged and advanced prices
pricelag <- rutils::lagit(pricev)
pricelag[1] <- pricelag[2]
pricadv <- rutils::lagit(pricev, lagg=-1)
pricadv[NROW(pricadv)] <- pricadv[NROW(pricadv)-1]
# Calculate the z-scores
diffl <- ifelse(abs(pricelag-pricadv) < 0.01, 0.01, abs(pricelag-pricadv))
zscores <- (pricev - 0.5*(pricelag+pricadv))/diffl
# Z-scores have very fat tails
range(zscores); mad(zscores)
madz <- mad(zscores[abs(zscores) > 0])
hist(zscores, breaks=5000, xlim=c(-2*madz, 2*madz))
# Scrub the price spikes
threshv <- 5*madz # Discrimination threshold
indeks <- which(abs(zscores) > threshv)
pricev[indeks] <- as.numeric(pricev[indeks-1])
# Plot dygraph of the scrubbed prices
dygraphs::dygraph(pricev, main="Scrubbed XLK Intraday Prices") %>%
  dyOptions(colors="blue", strokeWidth=1)
# Calculate the centered Hampel filter to remove bad prices
lookb <- 71 # Look-back interval
halfb <- lookb %/% 2 # Half-back interval
pricev <- xlk$price
# Calculate the trailing median and MAD
medianv <- HighFreq::roll_mean(pricev, lookb=lookb, method="nonparametric")
colnames(medianv) <- c("median")
madv <- HighFreq::roll_var(pricev, lookb=lookb, method="nonparametric")
# madv <- TTR::runMAD(pricev, n=lookb)
# Center the median and the MAD
medianv <- rutils::lagit(medianv, lagg=(-halfb), pad_zeros=FALSE)
madv <- rutils::lagit(madv, lagg=(-halfb), pad_zeros=FALSE)
# Calculate the Z-scores
zscores <- ifelse(madv > 0, (pricev - medianv)/madv, 0)
# Z-scores have very fat tails
range(zscores); mad(zscores)
madz <- mad(zscores[abs(zscores) > 0])
hist(zscores, breaks=5000, xlim=c(-2*madz, 2*madz))
# Define discrimination threshold value
threshv <- 6*madz
# Identify good prices with small z-scores
isgood <- (abs(zscores) < threshv)
# Calculate the number of bad prices
sum(!isgood)
# Overwrite bad prices and calculate time series of scrubbed prices
priceg <- pricev
priceg[!isgood] <- NA
priceg <- zoo::na.locf(priceg)
# Plot dygraph of the scrubbed prices
dygraphs::dygraph(priceg, main="Scrubbed XLK Intraday Prices") %>%
  dyOptions(colors="blue", strokeWidth=1)
# Plot using chart_Series()
x11(width=6, height=5)
quantmod::chart_Series(x=priceg,
  name="Clean XLK Intraday Prices for 2020-03-16")
# Add 200 random price spikes to the clean prices
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
nspikes <- 200
nrows <- NROW(priceg)
ispike <- logical(nrows)
ispike[sample(x=nrows, size=nspikes)] <- TRUE
priceb <- priceg
priceb[ispike] <- priceb[ispike]*
  sample(c(0.999, 1.001), size=nspikes, replace=TRUE)
# Plot the bad prices and their medians
medianv <- HighFreq::roll_mean(priceb, lookb=lookb, method="nonparametric")
pricem <- cbind(priceb, medianv)
colnames(pricem) <- c("prices with spikes", "median")
dygraphs::dygraph(pricem, main="XLK Prices With Spikes") %>%
  dyOptions(colors=c("red", "blue"))
# Calculate the z-scores
madv <- HighFreq::roll_var(priceb, lookb=lookb, method="nonparametric")
zscores <- ifelse(madv > 0, (priceb - medianv)/madv, 0)
# Z-scores have very fat tails
range(zscores); mad(zscores)
madz <- mad(zscores[abs(zscores) > 0])
hist(zscores, breaks=10000, xlim=c(-4*madz, 4*madz))
# Identify good prices with small z-scores
threshv <- 3*madz
isgood <- (abs(zscores) < threshv)
# Calculate the number of bad prices
sum(!isgood)
# Calculate the confusion matrix
table(actual=!ispike, forecast=isgood)
sum(!isgood)
# FALSE positive (type I error)
sum(!ispike & !isgood)
# FALSE negative (type II error)
sum(ispike & isgood)
# Confusion matrix as function of threshold
confun <- function(actualv, zscores, threshv) {
    confmat <- table(actualv, (abs(zscores) < threshv))
    confmat <- confmat / rowSums(confmat)
    c(typeI=confmat[2, 1], typeII=confmat[1, 2])
}  # end confun
confun(!ispike, zscores, threshv=threshv)
# Define vector of discrimination thresholds
threshv <- madz*seq(from=0.1, to=3.0, by=0.05)/2
# Calculate the error rates
errorr <- sapply(threshv, confun, actualv=!ispike, zscores=zscores)
errorr <- t(errorr)
rownames(errorr) <- threshv
errorr <- rbind(c(1, 0), errorr)
errorr <- rbind(errorr, c(0, 1))
# Calculate the area under the ROC curve (AUC)
truepos <- (1 - errorr[, "typeII"])
truepos <- (truepos + rutils::lagit(truepos))/2
falsepos <- rutils::diffit(errorr[, "typeI"])
abs(sum(truepos*falsepos))
# Plot ROC curve for Hampel classifier
plot(x=errorr[, "typeI"], y=1-errorr[, "typeII"],
     xlab="FALSE positive rate", ylab="TRUE positive rate",
     xlim=c(0, 1), ylim=c(0, 1),
     main="ROC Curve for Hampel Classifier",
     type="l", lwd=3, col="blue")
abline(a=0.0, b=1.0, lwd=3, col="orange")
# Load log VXX prices
load("/Users/jerzy/Develop/lecture_slides/data/pricevxx.RData")
nrows <- NROW(pricev)
# Calculate the centered Hampel filter for VXX
lookb <- 7 # Look-back interval
halfb <- lookb %/% 2 # Half-back interval
medianv <- HighFreq::roll_mean(pricev, lookb=lookb, method="nonparametric")
medianv <- rutils::lagit(medianv, lagg=(-halfb), pad_zeros=FALSE)
madv <- HighFreq::roll_var(pricev, lookb=lookb, method="nonparametric")
madv <- rutils::lagit(madv, lagg=(-halfb), pad_zeros=FALSE)
zscores <- ifelse(madv > 0, (pricev - medianv)/madv, 0)
range(zscores); mad(zscores)
madz <- mad(zscores[abs(zscores) > 0])
hist(zscores, breaks=100, xlim=c(-3*madz, 3*madz))
# Define discrimination threshold value
threshv <- 9*madz
# Calculate the good prices
isgood <- (abs(zscores) < threshv)
sum(!isgood)
# Dates of the bad prices
zoo::index(pricev[!isgood])
# Calculate the false positives
falsep <- !isgood
falsep[which(zoo::index(pricev) == as.Date("2010-11-08"))] <- FALSE
# Plot dygraph of the prices with bad prices
datam <- cbind(pricev, zscores)
colnames(datam)[2] <- "ZScores"
colv <- colnames(datam)
dygraphs::dygraph(datam, main="VXX Prices With Z-Scores and False Positives") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", strokeWidth=1, col="blue") %>%
  dySeries(name=colv[2], axis="y2", strokeWidth=1, col="red") %>%
  dyEvent(zoo::index(pricev[falsep]), label=rep("false", sum(falsep)), strokePattern="solid", color="red") %>%
  dyEvent(zoo::index(pricev["2010-11-08"]), label="true", strokePattern="solid", color="green")
# Replace bad stock prices with the previous good prices
priceg <- pricev
priceg[!isgood] <- NA
priceg <- zoo::na.locf(priceg)
# Calculate the Z-scores
medianv <- HighFreq::roll_mean(priceg, lookb=lookb, method="nonparametric")
medianv <- rutils::lagit(medianv, lagg=(-halfb), pad_zeros=FALSE)
madv <- HighFreq::roll_var(priceg, lookb=lookb, method="nonparametric")
madv <- rutils::lagit(madv, lagg=(-halfb), pad_zeros=FALSE)
zscores <- ifelse(madv > 0, (priceg - medianv)/madv, 0)
madz <- mad(zscores[abs(zscores) > 0])
# Calculate the number of bad prices
threshv <- 9*madz
isgood <- (abs(zscores) < threshv)
sum(!isgood)
zoo::index(priceg[!isgood])
# Calculate the false positives
falsep <- !isgood
falsep[which(zoo::index(pricev) == as.Date("2010-11-08"))] <- FALSE
# Plot dygraph of the prices with bad prices
dygraphs::dygraph(priceg, main="Scrubbed VXX Prices With False Positives") %>%
  dyEvent(zoo::index(priceg[falsep]), label=rep("false", sum(falsep)), strokePattern="solid", color="red") %>%
  dyOptions(colors="blue", strokeWidth=1)
# Add 200 random price spikes to the clean prices
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
nspikes <- 200
ispike <- logical(nrows)
ispike[sample(x=nrows, size=nspikes)] <- TRUE
priceb <- priceg
priceb[ispike] <- priceb[ispike]*
  sample(c(0.99, 1.01), size=nspikes, replace=TRUE)
# Calculate the Z-scores
medianv <- HighFreq::roll_mean(priceb, lookb=lookb, method="nonparametric")
medianv <- rutils::lagit(medianv, lagg=(-halfb), pad_zeros=FALSE)
madv <- HighFreq::roll_var(priceb, lookb=lookb, method="nonparametric")
madv <- rutils::lagit(madv, lagg=(-halfb), pad_zeros=FALSE)
zscores <- ifelse(madv > 0, (priceb - medianv)/madv, 0)
madz <- mad(zscores[abs(zscores) > 0])
# Define vector of discrimination thresholds
threshv <- madz*seq(from=0.1, to=3.0, by=0.05)/2
# Calculate the error rates
errorr <- sapply(threshv, confun, actualv=!ispike, zscores=zscores)
errorr <- t(errorr)
rownames(errorr) <- threshv
errorr <- rbind(c(1, 0), errorr)
errorr <- rbind(errorr, c(0, 1))
# Calculate the area under the ROC curve (AUC)
truepos <- (1 - errorr[, "typeII"])
truepos <- (truepos + rutils::lagit(truepos))/2
falsepos <- rutils::diffit(errorr[, "typeI"])
abs(sum(truepos*falsepos))
# Plot ROC curve for Hampel classifier
plot(x=errorr[, "typeI"], y=1-errorr[, "typeII"],
     xlab="FALSE positive rate", ylab="TRUE positive rate",
     xlim=c(0, 1), ylim=c(0, 1),
     main="ROC Curve for Daily Hampel Classifier",
     type="l", lwd=3, col="blue")
abline(a=0.0, b=1.0, lwd=3, col="orange")
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
datetime %m-% months(13:1)
library(zoo)  # Load zoo
library(RQuantLib)  # Load RQuantLib
# Create daily date series of class "Date"
datev <- Sys.Date() + -5:2
datev
# Create Boolean vector of business days
# Use RQuantLib calendar
isbusday <- RQuantLib::isBusinessDay(
  calendar="UnitedStates/GovernmentBond", datev)
# Create daily series of business days
datev[isbusday]
library(zoo)  # Load package zoo
datetime <- Sys.Date()  # Create date series of class "Date"
datev <- datetime + 0:365  # Daily series over one year
head(datev, 4)  # Print first few dates
format(head(datev, 4), "%m/%d/%Y")  # Print first few dates
# Create daily date-time series of class "POSIXct"
datev <- seq(Sys.time(), by="days", length.out=365)
head(datev, 4)  # Print first few dates
format(head(datev, 4), "%m/%d/%Y %H:%M:%S")  # Print first few dates
# Create series of monthly dates of class "zoo"
monthv <- yearmon(2010+0:36/12)
head(monthv, 4)  # Print first few dates
# Create series of quarterly dates of class "zoo"
qrtv <- yearqtr(2010+0:16/4)
head(qrtv, 4)  # Print first few dates
# Parse quarterly "zoo" dates to POSIXct
Sys.setenv(TZ="UTC")
as.POSIXct(head(qrtv, 4))
library(lubridate)  # Load lubridate
# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
# Create daily time series ending today
startd <- decimal_date(Sys.Date()-6)
endd <- decimal_date(Sys.Date())
# Create vector of geometric Brownian motion
datav <- exp(cumsum(rnorm(6)/100))
tstep <- NROW(datav)/(endd-startd)
timeser <- ts(data=datav, start=startd, frequency=tstep)
timeser  # Display time series
# Display index dates
as.Date(date_decimal(zoo::coredata(time(timeser))))
# bi-monthly geometric Brownian motion starting mid-1990
timeser <- ts(data=exp(cumsum(rnorm(96)/100)),
       frequency=6, start=1990.5)
# Show some methods for class "ts"
matrix(methods(class="ts")[3:8], ncol=2)
# "tsp" attribute specifies the date-time index
attributes(timeser)
# Extract the index
tail(time(timeser), 11)
# The index is equally spaced
diff(tail(time(timeser), 11))
# Subset the time series
window(timeser, start=1992, end=1992.25)
# Create plot
plot(timeser, type="l", col="red", lty="solid",
     xlab="", ylab="")
title(main="Brownian Motion", line=1)  # Add title
class(EuStockMarkets)  # Multiple ts object
dim(EuStockMarkets)
head(EuStockMarkets, 3)  # Get first three rows
# EuStockMarkets index is equally spaced
diff(tail(time(EuStockMarkets), 11))
par(mar=c(1, 2, 1, 1), oma=c(0, 0, 0, 0))
# Plot all the columns in separate panels
plot(EuStockMarkets, main="EuStockMarkets", xlab="")
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# Plot in single panel
plot(EuStockMarkets, main="EuStockMarkets",
     xlab="", ylab="", plot.type="single",
     col=c("black", "red", "blue", "green"))
# Add legend
legend(x=1992, y=8000,
 legend=colnames(EuStockMarkets),
 col=c("black", "red", "blue", "green"),
 lwd=6, lty=1)
# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
library(zoo)  # Load package zoo
# Create zoo time series of random returns
datev <- Sys.Date() + 0:11
zoots <- zoo(rnorm(NROW(datev)), order.by=datev)
zoots
attributes(zoots)
class(zoots)  # Class "zoo"
tail(zoots, 3)  # Get last few elements
library(zoo)  # Load package zoo
zoo::coredata(zoots)  # Extract coredata
zoo::index(zoots)  # Extract time index
start(zoots)  # First date
end(zoots)  # Last date
zoots[start(zoots)]  # First element
zoots[end(zoots)]  # Last element
zoo::coredata(zoots) <- rep(1, NROW(zoots))  # Replace coredata
cumsum(zoots)  # Cumulative sum
cummax(cumsum(zoots))
cummin(cumsum(zoots))
library(zoo)  # Load package zoo
zoots <- zoo(matrix(cumsum(rnorm(10)), nc=1),
  order.by=seq(from=as.Date("2013-06-15"), by="day", len=10))
colnames(zoots) <- "zoots"
tail(zoots)
dim(zoots)
attributes(zoots)
library(zoo)  # Load package zoo
zoo::coredata(zoots) <- (1:10)^2  # Replace coredata
zoots
lag(zoots)  # One day lag
lag(zoots, 2)  # Two day lag
lag(zoots, k=-1)  # Proper one day lag
diff(zoots)  # Diff with one day lag
# Proper lag and original length
lag(zoots, -2, na.pad=TRUE)
# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
library(zoo)  # Load package zoo
# Create index of daily dates
datev <- seq(from=as.Date("2014-07-14"), by="day", length.out=1000)
# Create vector of geometric Brownian motion
datav <- exp(cumsum(rnorm(NROW(datev))/100))
# Create zoo series of geometric Brownian motion
zoots <- zoo(x=datav, order.by=datev)
# Plot using method plot.zoo()
plot.zoo(zoots, xlab="", ylab="")
title(main="Brownian Motion", line=1)  # Add title
library(zoo)  # Load package zoo
# Subset zoo as matrix
zoots[459:463, 1]
# Subset zoo using window()
window(zoots,
 start=as.Date("2014-10-15"),
 end=as.Date("2014-10-19"))
# Subset zoo using Date object
zoots[as.Date("2014-10-15")]
# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
library(zoo)  # Load package zoo
# Create daily date series of class "Date"
tday <- Sys.Date()
index1 <- seq(tday-2*365, by="days", length.out=365)
# Create zoo time series of random returns
zoo1 <- zoo(rnorm(NROW(index1)), order.by=index1)
# Create another zoo time series of random returns
index2 <- seq(tday-360, by="days", length.out=365)
zoo2 <- zoo(rnorm(NROW(index2)), order.by=index2)
# rbind the two time series - ts1 supersedes ts2
zooub2 <- zoo2[zoo::index(zoo2) > end(zoo1)]
zoo3 <- rbind(zoo1, zooub2)
# Plot zoo time series of geometric Brownian motion
plot(exp(cumsum(zoo3)/100), xlab="", ylab="")
# Add vertical lines at stitch point
abline(v=end(zoo1), col="blue", lty="dashed")
abline(v=start(zoo2), col="red", lty="dashed")
title(main="Brownian Motions Stitched Together", line=1)  # Add title
# Create daily date series of class "Date"
index1 <- Sys.Date() + -3:1
# Create zoo time series of random returns
zoo1 <- zoo(rnorm(NROW(index1)), order.by=index1)
# Create another zoo time series of random returns
index2 <- Sys.Date() + -1:3
zoo2 <- zoo(rnorm(NROW(index2)), order.by=index2)
merge(zoo1, zoo2)  # union of dates
# Intersection of dates
merge(zoo1, zoo2, all=FALSE)
# Create matrix containing NA values
matv <- sample(18)
matv[sample(NROW(matv), 4)] <- NA
matv <- matrix(matv, nc=3)
# Replace NA values with most recent non-NA values
zoo::na.locf(matv)
# Get time series of prices
pricev <- mget(c("VTI", "VXX"), envir=rutils::etfenv)
pricev <- lapply(pricev, quantmod::Cl)
pricev <- rutils::do_call(cbind, pricev)
sum(is.na(pricev))
# Carry forward and backward non-NA prices
pricev <- zoo::na.locf(pricev, na.rm=FALSE)
pricev <- zoo::na.locf(pricev, na.rm=FALSE, fromLast=TRUE)
sum(is.na(pricev))
# Remove whole rows containing NA returns
retp <- rutils::etfenv$returns
sum(is.na(retp))
retp <- na.omit(retp)
# Or carry forward non-NA returns (preferred)
retp <- rutils::etfenv$returns
retp[1, is.na(retp[1, ])] <- 0
retp <- zoo::na.locf(retp, na.rm=FALSE)
sum(is.na(retp))
# Replace NAs in xts time series
pricev <- rutils::etfenv$prices[, 1]
head(pricev)
sum(is.na(pricev))
library(quantmod)
pricezoo <- zoo::na.locf(pricev, na.rm=FALSE, fromLast=TRUE)
pricexts <- xts:::na.locf.xts(pricev, fromLast=TRUE)
all.equal(pricezoo, pricexts, check.attributes=FALSE)
library(microbenchmark)
summary(microbenchmark(
  zoo=zoo::na.locf(pricev, fromLast=TRUE),
  xts=xts:::na.locf.xts(pricev, fromLast=TRUE),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
library(lubridate)  # Load lubridate
library(zoo)  # Load package zoo
# methods(as.zoo)  # Many methods of coercing into zoo
class(EuStockMarkets)  # Multiple ts object
# Coerce mts object into zoo
zoots <- as.zoo(EuStockMarkets)
class(zoo::index(zoots))  # Index is numeric
head(zoots, 3)
# Approximately convert index into class "Date"
zoo::index(zoots) <-
  as.Date(365*(zoo::index(zoots)-1970))
head(zoots, 3)
# Convert index into class "POSIXct"
zoots <- as.zoo(EuStockMarkets)
zoo::index(zoots) <- date_decimal(zoo::index(zoots))
head(zoots, 3)
library(lubridate)  # Load lubridate
library(zoo)  # Load package zoo
# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
# Create index of daily dates
datev <- seq(from=as.Date("2014-07-14"), by="day", length.out=1000)
# Create vector of geometric Brownian motion
datav <- exp(cumsum(rnorm(NROW(datev))/100))
# Create zoo time series of geometric Brownian motion
zoots <- zoo(x=datav, order.by=datev)
head(zoots, 3)  # zoo object
# as.ts() creates ts object with frequency=1
timeser <- as.ts(zoots)
tsp(timeser)  # Frequency=1
# Get start and end dates of zoots
startd <- decimal_date(start(zoots))
endd <- decimal_date(end(zoots))
# Calculate frequency of zoots
tstep <- NROW(zoots)/(endd-startd)
datav <- zoo::coredata(zoots)  # Extract data from zoots
# Create ts object using ts()
timeser <- ts(data=datav, start=startd, frequency=tstep)
# Display start of time series
window(timeser, start=start(timeser), end=start(timeser)+4/365)
head(time(timeser))  # Display index dates
head(as.Date(date_decimal(zoo::coredata(time(timeser)))))
library(lubridate)  # Load lubridate
library(zoo)  # Load package zoo
# Create weekday Boolean vector
wkdays <- weekdays(zoo::index(zoots))
wkdayl <- !((wkdays == "Saturday") | (wkdays == "Sunday"))
# Remove weekends from zoo time series
zoots <- zoots[wkdayl, ]
head(zoots, 7)  # zoo object
# as.ts() creates NA values
timeser <- as.ts(zoots)
head(timeser, 7)
# Create vector of regular dates, including weekends
datev <- seq(from=start(zoots), by="day", length.out=NROW(zoots))
zoo::index(zoots) <- datev
timeser <- as.ts(zoots)
head(timeser, 7)
# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
library(xts)  # Load package xts
# Create xts time series of random returns
datev <- Sys.Date() + 0:3
xtsv <- xts(rnorm(NROW(datev)), order.by=datev)
names(xtsv) <- "random"
xtsv
tail(xtsv, 3)  # Get last few elements
first(xtsv)  # Get first element
last(xtsv)  # Get last element
class(xtsv)  # Class "xts"
attributes(xtsv)
# Get the time zone of an xts object
tzone(xtsv)
load(file="/Users/jerzy/Develop/lecture_slides/data/zoo_data.RData")
class(zoo_stx)
# as.xts() coerces zoo series into xts series
library(xts)  # Load package xts
pricexts <- as.xts(zoo_stx)
dim(pricexts)
head(pricexts[, 1:4], 4)
# Plot using plot.xts method
xts::plot.xts(pricexts[, "Close"], xlab="", ylab="", main="")
title(main="Stock Prices")  # Add title
library(xts)  # Load xts
library(lubridate)  # Load lubridate
# Coerce EuStockMarkets into class xts
xtsv <- xts(zoo::coredata(EuStockMarkets),
      order.by=date_decimal(zoo::index(EuStockMarkets)))
# Plot all columns in single panel: xts v.0.9-8
colorv <- rainbow(NCOL(xtsv))
plot(xtsv, main="EuStockMarkets using xts",
     col=colorv, major.ticks="years",
     minor.ticks=FALSE)
legend("topleft", legend=colnames(EuStockMarkets),
 inset=0.2, cex=0.7, , lty=rep(1, NCOL(xtsv)),
 lwd=3, col=colorv, bg="white")
# Plot only first column: xts v.0.9-7
plot(xtsv[, 1], main="EuStockMarkets using xts",
     col=colorv[1], major.ticks="years",
     minor.ticks=FALSE)
# Plot remaining columns
for (colnum in 2:NCOL(xtsv))
  lines(xtsv[, colnum], col=colorv[colnum])
# Plot using quantmod
library(quantmod)
plotheme <- chart_theme()
plotheme$col$line.col <- colors
chart_Series(x=xtsv, theme=plotheme,
       name="EuStockMarkets using quantmod")
legend("topleft", legend=colnames(EuStockMarkets),
 inset=0.2, cex=0.7, , lty=rep(1, NCOL(xtsv)),
 lwd=3, col=colorv, bg="white")
library(rutils)
library(ggplot2)
pricev <- rutils::etfenv$prices[, 1]
pricev <- na.omit(pricev)
# Create ggplot object
plotobj <- qplot(x=zoo::index(pricev),
          y=as.numeric(pricev),
          geom="line",
          main=names(pricev)) +
  xlab("") + ylab("") +
  theme(  # Add legend and title
    legend.position=c(0.1, 0.5),
    plot.title=element_text(vjust=-2.0),
    plot.background=element_blank()
  )  # end theme
# Render ggplot object
plotobj
library(rutils)  # Load xts time series data
library(reshape2)
library(ggplot2)
pricev <- rutils::etfenv$prices[, c("VTI", "IEF")]
pricev <- na.omit(pricev)
# Create data frame of time series
dframe <- data.frame(datev=zoo::index(pricev), zoo::coredata(pricev))
# reshape data into a single column
dframe <- reshape2::melt(dframe, id="dates")
x11(width=6, height=5)  # Open plot window
# ggplot the melted dframe
ggplot(data=dframe,
 mapping=aes(x=datev, y=value, colour=variable)) +
 geom_line() +
  xlab("") + ylab("") +
  ggtitle("VTI and IEF") +
  theme(  # Add legend and title
    legend.position=c(0.2, 0.8),
    plot.title=element_text(vjust=-2.0)
  )  # end theme
# Load rutils which contains etfenv dataset
library(rutils)
library(dygraphs)
pricev <- rutils::etfenv$prices[, c("VTI", "IEF")]
pricev <- na.omit(pricev)
# Plot dygraph with date range selector
dygraph(pricev, main="VTI and IEF prices") %>%
  dyOptions(colors=c("blue","green")) %>%
  dyRangeSelector()
# Load rutils which contains etfenv dataset
library(rutils)
library(plotly)
pricev <- rutils::etfenv$prices[, c("VTI", "IEF")]
pricev <- na.omit(pricev)
# Create data frame of time series
dframe <- data.frame(datev=zoo::index(pricev),
    zoo::coredata(pricev))
# Plotly syntax using pipes
dframe %>%
  plot_ly(x=~datev, y=~VTI, type="scatter", mode="lines", name="VTI") %>%
  add_trace(x=~datev, y=~IEF, type="scatter", mode="lines", name="IEF") %>%
  layout(title="VTI and IEF prices",
   xaxis=list(title="Time"),
   yaxis=list(title="Stock Prices"),
   legend=list(x=0.1, y=0.9))
# Or use standard plotly syntax
plotobj <- plot_ly(data=dframe, x=~datev, y=~VTI, type="scatter", mode="lines", name="VTI")
plotobj <- add_trace(p=plotobj, x=~datev, y=~IEF, type="scatter", mode="lines", name="IEF")
plotobj <- layout(p=plotobj, title="VTI and IEF prices", xaxis=list(title="Time"), yaxis=list(title="Stock Prices"), legend=list(x=0.1, y=0.9))
plotobj
# Subset xts using a date range string
pricev <- rutils::etfenv$prices
pricesub <- pricev["2014-10-15/2015-01-10", 1:4]
first(pricesub)
last(pricesub)
# Subset Nov 2014 using a date string
pricesub <- pricev["2014-11", 1:4]
first(pricesub)
last(pricesub)
# Subset all data after Nov 2014
pricesub <- pricev["2014-11/", 1:4]
first(pricesub)
last(pricesub)
# Comma after date range not necessary
all.equal(pricev["2014-11", ], pricev["2014-11"])
# .subset_xts() is faster than the bracket []
library(microbenchmark)
summary(microbenchmark(
  bracket=pricev[10:20, ],
  subset=xts::.subset_xts(pricev, 10:20),
  times=10))[, c(1, 4, 5)]
# Specify string representing a date
datev <- "2014-10-15"
# Subset prices in two different ways
pricev <- rutils::etfenv$prices
all.equal(pricev[zoo::index(pricev) >= datev],
    pricev[paste0(datev, "/")])
# Boolean subsetting is slower because coercing string into date
library(microbenchmark)
summary(microbenchmark(
  boolean=(pricev[zoo::index(pricev) >= datev]),
  date=(pricev[paste0(datev, "/")]),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Coerce string into a date
datev <- as.Date("2014-10-15")
# Boolean subsetting is faster than using date string
summary(microbenchmark(
  boolean=(pricev[zoo::index(pricev) >= datev]),
  date=(pricev[paste0(datev, "/")]),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
pricev <- HighFreq::SPY["2012-04"]
# Subset recurring time interval using "T notation",
pricev <- pricev["T10:30:00/T15:00:00"]
first(pricev["2012-04-16"])  # First element of day
last(pricev["2012-04-16"])  # Last element of day
# Suppress timezone warning messages
options(xts_check_tz=FALSE)
# Create time series with overlapping time indices
vti1 <- rutils::etfenv$VTI["/2015"]
vti2 <- rutils::etfenv$VTI["2014/"]
dates1 <- zoo::index(vti1)
dates2 <- zoo::index(vti2)
# Join by rows
vti <- rbind(vti1, vti2)
datev <- zoo::index(vti)
sum(duplicated(datev))
vti <- vti[!duplicated(datev), ]
all.equal(vti, rutils::etfenv$VTI)
# Alternative method - slightly slower
vti <- rbind(vti1, vti2[!(zoo::index(vti2) %in% zoo::index(vti1))])
all.equal(vti, rutils::etfenv$VTI)
# Remove duplicates starting from the end
vti <- rbind(vti1, vti2)
vti <- vti[!duplicated(datev), ]
vtifl <- vti[!duplicated(datev, fromLast=TRUE), ]
all.equal(vti, vtifl)
pricev <- rutils::etfenv$prices[, c("VTI", "IEF")]
pricev <- na.omit(pricev)
str(pricev)  # Display structure of xts
# Subsetting zoo to single column drops dim attribute
pricezoo <- as.zoo(pricev)
dim(pricezoo)
dim(pricezoo[, 1])
# zoo with single column are vectors not matrices
c(is.matrix(pricezoo), is.matrix(pricezoo[, 1]))
# xts always have a dim attribute
rbind(base=dim(pricev), subs=dim(pricev[, 1]))
c(is.matrix(pricev), is.matrix(pricev[, 1]))
# Lag of zoo shortens it by one row
rbind(base=dim(pricezoo), lag=dim(lag(pricezoo)))
# Lag of xts doesn't shorten it
rbind(base=dim(pricev), lag=dim(lag(pricev)))
# Lag of zoo is in opposite direction from xts
head(lag(pricezoo, -1), 4)
head(lag(pricev), 4)
# library(rutils)  # Load package rutils
# Indices of last observations in each hour
endd <- xts::endpoints(pricev, on="hours")
head(endd)
# Extract the last observations in each hour
head(pricev[endd, ])
# Lower the periodicity to months
pricem <- to.period(x=pricev, period="months", name="MSFT")
# Convert colnames to standard OHLC format
colnames(pricem)
colnames(pricem) <- sapply(
  strsplit(colnames(pricem), split=".", fixed=TRUE),
  function(namev) namev[-1]
  )  # end sapply
head(pricem, 3)
# Lower the periodicity to years
pricey <- to.period(x=pricem, period="years", name="MSFT")
colnames(pricey) <- sapply(
  strsplit(colnames(pricey), split=".", fixed=TRUE),
  function(namev) namev[-1]
  )  # end sapply
head(pricey)
load(file="/Users/jerzy/Develop/lecture_slides/data/zoo_data.RData")
library(quantmod)  # Load package quantmod
# as.xts() coerces zoo series into xts series
class(zoo_stx)
pricexts <- as.xts(zoo_stx)
dim(pricexts)
head(pricexts[, 1:4], 4)
# OHLC candlechart
plotheme <- chart_theme()
plotheme$col$up.col <- c("green")
plotheme$col$dn.col <- c("red")
chart_Series(x=pricexts["2016-05/2016-06", 1:4], theme=plotheme,
  name="Candlestick Plot of OHLC Stock Prices")
library(dygraphs)
# Create dygraphs object
dyplot <- dygraphs::dygraph(pricexts["2016-05/2016-06", 1:4])
# Convert dygraphs object to candlestick plot
dyplot <- dygraphs::dyCandlestick(dyplot)
# Render candlestick plot
dyplot
# Candlestick plot using pipes syntax
dygraphs::dygraph(pricexts["2016-05/2016-06", 1:4]) %>%
  dyCandlestick() %>%
  dyOptions(colors="red", strokeWidth=3)
# Candlestick plot without using pipes syntax
dygraphs::dyCandlestick(dygraphs::dyOptions(
  dygraphs::dygraph(pricexts["2016-05/2016-06", 1:4]),
  colors="red", strokeWidth=3))
# Create zoo time series
datev <- seq(from=as.Date("2014-07-14"), by="day", length.out=10)
timeser <- zoo(x=sample(10), order.by=datev)
class(timeser)
timeser
library(xts)
# Coerce zoo time series to class xts
pricexts <- as.xts(timeser)
class(xtseries)
xtseries
