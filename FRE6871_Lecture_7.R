# Verify that Rtools or XCode are working properly:
devtools::find_rtools()  # Under Windows
devtools::has_devel()
# Install the packages Rcpp and RcppArmadillo
install.packages(c("Rcpp", "RcppArmadillo"))
# Load package Rcpp
library(Rcpp)
# Get documentation for package Rcpp
# Get short description
packageDescription("Rcpp")
# Load help page
help(package="Rcpp")
# List all datasets in "Rcpp"
data(package="Rcpp")
# List all objects in "Rcpp"
ls("package:Rcpp")
# Remove Rcpp from search path
detach("package:Rcpp")

# Define Rcpp function
Rcpp::cppFunction("
  int times_two(int x)
    { return 2 * x;}
  ")  # end cppFunction
# Run Rcpp function
times_two(3)
# Source Rcpp functions from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/mult_rcpp.cpp")
# Multiply two numbers
mult_rcpp(2, 3)
mult_rcpp(1:3, 6:4)
# Multiply two vectors
mult_vec_rcpp(2, 3)
mult_vec_rcpp(1:3, 6:4)

# Define Rcpp function with loop
Rcpp::cppFunction("
double inner_mult(NumericVector x, NumericVector y) {
int xsize = x.size();
int ysize = y.size();
if (xsize != ysize) {
    return 0;
  } else {
    double total = 0;
    for(int i = 0; i < xsize; ++i) {
total += x[i] * y[i];
  }
  return total;
  }
}")  # end cppFunction
# Run Rcpp function
inner_mult(1:3, 6:4)
inner_mult(1:3, 6:3)
# Define Rcpp Sugar function with loop
Rcpp::cppFunction("
double inner_sugar(NumericVector x, NumericVector y) {
  return sum(x * y);
}")  # end cppFunction
# Run Rcpp Sugar function
inner_sugar(1:3, 6:4)
inner_sugar(1:3, 6:3)

# Define R function with loop
inner_multr <- function(x, y) {
    sumv <- 0
    for(i in 1:NROW(x)) {
sumv <- sumv + x[i] * y[i]
    }
    sumv
}  # end inner_multr
# Run R function
inner_multr(1:3, 6:4)
inner_multr(1:3, 6:3)
# Compare speed of Rcpp and R
library(microbenchmark)
summary(microbenchmark(
  rcode=inner_multr(1:10000, 1:10000),
  innerp=1:10000 %*% 1:10000,
  Rcpp=inner_mult(1:10000, 1:10000),
  sugar=inner_sugar(1:10000, 1:10000),
  times=10))[, c(1, 4, 5)]

# Define Ornstein-Uhlenbeck function in R
sim_our <- function(nrows=1000, priceq=5.0,
              volat=0.01, theta=0.01) {
  retp <- numeric(nrows)
  pricev <- numeric(nrows)
  pricev[1] <- priceq
  for (i in 2:nrows) {
    retp[i] <- theta*(priceq - pricev[i-1]) + volat*rnorm(1)
    pricev[i] <- pricev[i-1] + retp[i]
  }  # end for
  pricev
}  # end sim_our
# Simulate Ornstein-Uhlenbeck process in R
priceq <- 5.0; sigmav <- 0.01
thetav <- 0.01; nrows <- 1000
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")  # Reset random numbers
ousim <- sim_our(nrows, priceq=priceq, volat=sigmav, theta=thetav)

# Define Ornstein-Uhlenbeck function in Rcpp
Rcpp::cppFunction("
NumericVector sim_oucpp(double priceq,
                  double volat,
                  double thetav,
                  NumericVector innov) {
  int nrows = innov.size();
  NumericVector pricev(nrows);
  NumericVector retv(nrows);
  pricev[0] = priceq;
  for (int it = 1; it < nrows; it++) {
    retv[it] = thetav*(priceq - pricev[it-1]) + volat*innov[it-1];
    pricev[it] = pricev[it-1] + retv[it];
  }  // end for
  return pricev;
}")  # end cppFunction
# Simulate Ornstein-Uhlenbeck process in Rcpp
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")  # Reset random numbers
oucpp <- sim_oucpp(priceq=priceq,
  volat=sigmav, theta=thetav, innov=rnorm(nrows))
all.equal(ousim, oucpp)
# Compare speed of Rcpp and R
library(microbenchmark)
summary(microbenchmark(
  rcode=sim_our(nrows, priceq=priceq, volat=sigmav, theta=thetav),
  Rcpp=sim_oucpp(priceq=priceq, volat=sigmav, theta=thetav, innov=rnorm(nrows)),
  times=10))[, c(1, 4, 5)]

# Source Rcpp function for Ornstein-Uhlenbeck process from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/sim_ou.cpp")
# Simulate Ornstein-Uhlenbeck process in Rcpp
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")  # Reset random numbers
oucpp <- sim_oucpp(priceq=priceq,
  volat=sigmav,
  theta=thetav,
  innov=rnorm(nrows))
all.equal(ousim, oucpp)
# Compare speed of Rcpp and R
library(microbenchmark)
summary(microbenchmark(
  rcode=sim_our(nrows, priceq=priceq, volat=sigmav, theta=thetav),
  Rcpp=sim_oucpp(priceq=priceq, volat=sigmav, theta=thetav, innov=rnorm(nrows)),
  times=10))[, c(1, 4, 5)]

# Calculate uniformly distributed pseudo-random sequence
unifun <- function(seedv, nrows=10) {
  datav <- numeric(nrows)
  datav[1] <- seedv
  for (i in 2:nrows) {
    datav[i] <- 4*datav[i-1]*(1-datav[i-1])
  }  # end for
  acos(1-2*datav)/pi
}  # end unifun

# Source Rcpp functions from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/unifun.cpp")
# Microbenchmark Rcpp code
library(microbenchmark)
summary(microbenchmark(
  rcode=runif(1e5),
  rloop=unifun(0.3, 1e5),
  Rcpp=unifuncpp(0.3, 1e5),
  times=10))[, c(1, 4, 5)]

library(RcppArmadillo)
# Source Rcpp functions from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/armadillo_functions.cpp")
vec1 <- runif(1e5)
vec2 <- runif(1e5)
inner_vec(vec1, vec2)
vec1 %*% vec2

# Microbenchmark \emph{RcppArmadillo} code
summary(microbenchmark(
  rcpp = inner_vec(vec1, vec2),
  rcode = (vec1 %*% vec2),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
# Microbenchmark shows:
# inner_vec() is several times faster than %*%, especially for longer vectors.
#     expr     mean   median
# 1 inner_vec 110.7067 110.4530
# 2 rcode 585.5127 591.3575

# Source Rcpp functions from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/sim_arima.cpp")
# Define AR(2) coefficients
coeff <- c(0.9, 0.09)
nrows <- 1e4
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
innov <- rnorm(nrows)
# Simulate ARIMA using filter()
arimar <- filter(x=innov, filter=coeff, method="recursive")
# Simulate ARIMA using sim_ar()
innov <- matrix(innov)
coeff <- matrix(coeff)
arimav <- sim_ar(coeff, innov)
all.equal(drop(arimav), as.numeric(arimar))
# Microbenchmark \emph{RcppArmadillo} code
summary(microbenchmark(
  rcpp = sim_ar(coeff, innov),
  filter = filter(x=innov, filter=coeff, method="recursive"),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary

library(RcppArmadillo)
# Source Rcpp functions from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/armadillo_functions.cpp")
matv <- matrix(runif(1e5), nc=1e3)
# Center matrix columns using apply()
matd <- apply(matv, 2, function(x) (x-mean(x)))
# Center matrix columns in place using Rcpp demeanr()
demeanr(matv)
all.equal(matd, matv)
# Microbenchmark \emph{RcppArmadillo} code
library(microbenchmark)
summary(microbenchmark(
  rcode = (apply(matv, 2, mean)),
  rcpp = demeanr(matv),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
# Perform matrix inversion
# Create random positive semi-definite matrix
matv <- matrix(runif(25), nc=5)
matv <- t(matv) %*% matv
# Invert the matrix
matrixinv <- solve(matv)
inv_mat(matv)
all.equal(matrixinv, matv)
# Microbenchmark \emph{RcppArmadillo} code
summary(microbenchmark(
  rcode = solve(matv),
  rcpp = inv_mat(matv),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary

library(RcppArmadillo)
# Source Rcpp functions from file
Rcpp::sourceCpp("/Users/jerzy/Develop/lecture_slides/scripts/HighFreq.cpp")
# Calculate matrix of random returns
matv <- matrix(rnorm(300), nc=5)
# Reduced inverse of correlation matrix
dimax <- 4
cormat <- cor(matv)
eigend <- eigen(cormat)
invmat <- eigend$vectors[, 1:dimax] %*%
  (t(eigend$vectors[, 1:dimax]) / eigend$values[1:dimax])
# Reduced inverse using \emph{RcppArmadillo}
invarma <- calc_inv(cormat, dimax=dimax)
all.equal(invmat, invarma)
# Microbenchmark \emph{RcppArmadillo} code
library(microbenchmark)
summary(microbenchmark(
  rcode = {eigend <- eigen(cormat)
eigend$vectors[, 1:dimax] %*% (t(eigend$vectors[, 1:dimax]) / eigend$values[1:dimax])},
  rcpp = calc_inv(cormat, dimax=dimax),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary

# Install package reticulate
install.packages("reticulate")
# Start Python session
reticulate::repl_python()
# Exit Python session
exit

# Install and load package readxl
install.packages("readxl")
library(readxl)
dirn <- "/Users/jerzy/Develop/lecture_slides/data"
filev <- file.path(dirn, "multi_tabs.xlsx")
# Read a time series from first sheet of xlsx file
tibblev <- readxl::read_xlsx(filev)
class(tibblev)
# Coerce POSIXct dates into Date class
class(tibblev$Dates)
tibblev$Dates <- as.Date(tibblev$Dates)
# Some columns are character strings
sapply(tibblev, class)
sapply(tibblev, is.character)
# Coerce columns with strings to numeric
listv <- lapply(tibblev, function(x) {
  if (is.character(x))
    as.numeric(x)
  else
    x
})  # end lapply
# Coerce list into xts time series
xtsv <- xts::xts(do.call(cbind, listv)[, -1], listv[[1]])
class(xtsv); dim(xtsv)
# Replace NA values with the most recent non-NA values
sum(is.na(xtsv))
xtsv <- zoo::na.locf(xtsv, na.rm=FALSE)
xtsv <- zoo::na.locf(xtsv, fromLast=TRUE)

# Read names of all the sheets in an Excel spreadsheet
namev <- readxl::excel_sheets(filev)
# Read all the sheets from an Excel spreadsheet
sheetv <- lapply(namev, read_xlsx, path=filev)
names(sheetv) <- namev
# sheetv is a list of tibbles
sapply(sheetv, class)
# Create function to coerce tibble to xts
to_xts <- function(tibblev) {
  tibblev$Dates <- as.Date(tibblev$Dates)
  # Coerce columns with strings to numeric
  listv <- lapply(tibblev, function(x) {
    if (is.character(x))
      as.numeric(x)
    else
      x
  })  # end lapply
  # Coerce list into xts series
  xts::xts(do.call(cbind, listv)[, -1], listv$Dates)
}  # end to_xts
# Coerce list of tibbles to list of xts
class(sheetv)
sheetv <- lapply(sheetv, to_xts)
sapply(sheetv, class)
# Replace NA values with the most recent non-NA values
sapply(sheetv, function(xtsv) sum(is.na(xtsv)))
sheetv <- lapply(sheetv, zoo::na.locf, na.rm=FALSE)
sheetv <- lapply(sheetv, zoo::na.locf, fromLast=TRUE)

##Perform calculations in R,
##And export to CSV files
setwd("/Users/jerzy/Develop/lecture_slides/data")
# Read data frame, with row names from first column
readf <- read.csv(file="florist.csv", row.names=1)
# Subset data frame
readf <- readf[readf[, "type"]=="daisy", ]
all.equal(readf, dframe)
# Write data frame to CSV file, with row names
write.csv(readf, file="daisies.csv")

##Perform calculations in R,
##And export to CSV files
setwd("/Users/jerzy/Develop/lecture_slides/data")
# Read data frame, with row names from first column
readf <- read.csv(file="florist.csv", row.names=1)
# Subset data frame
readf <- readf[readf[, "type"]=="daisy", ]
all.equal(readf, dframe)
# Write data frame to CSV file, with row names
write.csv(readf, file="daisies.csv")

# Install latest version of googlesheets
devtools::install_github("jennybc/googlesheets")
# Load package googlesheets
library(googlesheets)
library(dplyr)
# Authenticate authorize R to view and manage your files
gs_auth(new_user=TRUE)
# List the files in Google Sheets
googlesheets::gs_ls()
# Register a sheet
googsheet <- gs_title("my_data")
# view sheet summary
googsheet
# List tab names in sheet
tabv <- gs_ws_ls(googsheet)
# Set curl options
library(httr)
httr::set_config(config(ssl_verifypeer=0L))
# Read data from sheet
gs_read(googsheet)
# Read data from single tab of sheet
gs_read(googsheet, ws=tabv[1])
gs_read_csv(googsheet, ws=tabv[1])
# Or using dplyr pipes
googsheet %>% gs_read(ws=tabv[1])
# Download data from sheet into file
gs_download(googsheet, ws=tabv[1],
      to="/Users/jerzy/Develop/lecture_slides/data/googsheet.csv")
# Open sheet in internet browser
gs_browse(googsheet)

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

# Install package data.table
install.packages("data.table")
# Load package data.table
library(data.table)
# Get documentation for package data.table
# Get short description
packageDescription("data.table")
# Load help page
help(package="data.table")
# List all datasets in "data.table"
data(package="data.table")
# List all objects in "data.table"
ls("package:data.table")
# Remove data.table from search path
detach("package:data.table")

# Create a data table
library(data.table)
dtable <- data.table::data.table(
  col1=sample(7), col2=sample(7), col3=sample(7))
# Print dtable
class(dtable); dtable
# Column referenced without quotes
dtable[, col2]
# Row referenced without a following comma
dtable[2]
# Print option "datatable.print.nrows"
getOption("datatable.print.nrows")
options(datatable.print.nrows=10)
getOption("datatable.print.nrows")
# Number of rows in dtable
NROW(dtable)
# Or
dtable[, NROW(col1)]
# Or
dtable[, .N]
# microbenchmark speed of data.table syntax
library(microbenchmark)
summary(microbenchmark(
  dt=dtable[, .N],
  rcode=NROW(dtable),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# Read a data table from CSV file
dirn <- "/Users/jerzy/Develop/lecture_slides/data/"
filen <- file.path(dirn, "weather_delays14.csv")
dtable <- data.table::fread(filen)
class(dtable); dim(dtable)
dtable
# fread() reads the same data as read.csv()
all.equal(read.csv(filen),
          data.table::setDF(data.table::fread(filen)))
# fread() is much faster than read.csv()
library(microbenchmark)
summary(microbenchmark(
  rcode=read.csv(filen),
  fread=data.table::setDF(data.table::fread(filen)),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Write data table to file in different ways
data.table::fwrite(dtable, file="dtable.csv")
write.csv(dtable, file="dtable2.csv")
cat(unlist(dtable), file="dtable3.csv")
# microbenchmark speed of data.table::fwrite()
summary(microbenchmark(
  fwrite=data.table::fwrite(dtable, file="dtable.csv"),
  write_csv=write.csv(dtable, file="dtable2.csv"),
  cat=cat(unlist(dtable), file="dtable3.csv"),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# Select first five rows of dtable
dtable[1:5]
# Select rows with JFK flights
jfkf <- dtable[origin=="JFK"]
# Select rows JFK flights in June
jfkf <- dtable[origin=="JFK" & month==6]
# Select rows without JFK flights
jfkf <- dtable[!(origin=="JFK")]
# Select flights with carrier_delay
dtable[carrier_delay > 0]
# Select column of dtable and return a vector
head(dtable[, origin])
# Select column of dtable and return a dtable, not vector
head(dtable[, list(origin)])
head(dtable[, .(origin)])
# Select two columns of dtable
dtable[, list(origin, month)]
dtable[, .(origin, month)]
columnv <- c("origin", "month")
dtable[, ..columnv]
dtable[, month, origin]
# Select two columns and rename them
dtable[, .(depairp=origin, monnthy=month)]
# Select all columns except origin
head(dtable[, !"origin"])
head(dtable[, -"origin"])

# Select flights with positive carrier_delay
dtable[carrier_delay > 0]
# Number of flights with carrier_delay
dtable[, sum(carrier_delay > 0)]
# Or standard R commands
sum(dtable[, carrier_delay > 0])
# microbenchmark speed of data.table syntax
summary(microbenchmark(
  dt=dtable[, sum(carrier_delay > 0)],
  rcode=sum(dtable[, carrier_delay > 0]),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Average carrier_delay
dtable[, mean(carrier_delay)]
# Average carrier_delay and aircraft_delay
dtable[, .(carrier=mean(carrier_delay),
     aircraft=mean(aircraft_delay))]
# Average aircraft_delay from JFK
dtable[origin=="JFK", mean(aircraft_delay)]
# Number of flights from JFK
dtable[origin=="JFK", NROW(aircraft_delay)]
# Or
dtable[origin=="JFK", .N]
# In R
sum(dtable[, origin]=="JFK")

# Number of flights from each airport
dtable[, .N, by=origin]
# Same, but add names to output
dtable[, .(flights=.N), by=.(airport=origin)]
# Number of AA flights from each airport
dtable[carrier=="AA", .(flights=.N), by=.(airport=origin)]
# Number of flights from each airport and airline
dtable[, .(flights=.N), by=.(airport=origin, airline=carrier)]
# Average aircraft_delay
dtable[, mean(aircraft_delay)]
# Average aircraft_delay from JFK
dtable[origin=="JFK", mean(aircraft_delay)]
# Average aircraft_delay from each airport
dtable[, .(delay=mean(aircraft_delay)), by=.(airport=origin)]
# Average and max delays from each airport and month
dtable[, .(mean_delay=mean(aircraft_delay), max_delay=max(aircraft_delay)),
     by=.(airport=origin, month=month)]
# Average and max delays from each airport and month
dtable[, .(mean_delay=mean(aircraft_delay), max_delay=max(aircraft_delay)),
     keyby=.(airport=origin, month=month)]

# Sort ascending by origin, then descending by dest
dtables <- dtable[order(origin, -dest)]
dtables
# Doesn't work outside dtable
order(origin, -dest)
# Sort dtable by reference
setorder(dtable, origin, -dest)
all.equal(dtable, dtables)
# setorder() is much faster than order()
summary(microbenchmark(
  order=dtable[order(origin, -dest)],
  setorder=setorder(dtable, origin, -dest),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Average aircraft_delay by month
dtables[, .(mean_delay=mean(aircraft_delay)),
      by=.(month=month)]
# Chained brackets to sort output by month
dtables[, .(mean_delay=mean(aircraft_delay)),
  by=.(month=month)][order(month)]

# Select weather_delay and aircraft_delay in two different ways
dtable[1:7, .SD,
     .SDcols=c("weather_delay", "aircraft_delay")]
dtable[1:7, .(weather_delay, aircraft_delay)]
# Calculate mean of weather_delay and aircraft_delay
dtable[, sapply(.SD, mean),
     .SDcols=c("weather_delay", "aircraft_delay")]
sapply(dtable[, .SD,
     .SDcols=c("weather_delay", "aircraft_delay")], mean)
# Return origin and dest, then all other columns
dtable[1:7, .SD, by=.(origin, dest)]
# Return origin and dest, then weather_delay and aircraft_delay columns
dtable[1:7, .SD, by=.(origin, dest),
     .SDcols=c("weather_delay", "aircraft_delay")]
# Return first two rows from each month
dtable[, head(.SD, 2), by=.(month)]
dtable[, head(.SD, 2), by=.(month),
     .SDcols=c("weather_delay", "aircraft_delay")]
# Calculate mean of weather_delay and aircraft_delay, grouped by origin
dtable[, lapply(.SD, mean),
     by=.(origin),
     .SDcols=c("weather_delay", "aircraft_delay")]
# Or simply
dtable[, .(weather_delay=mean(weather_delay),
         aircraft_delay=mean(aircraft_delay)),
     by=.(origin)]

# Add tot_delay column
dtable[, tot_delay := (carrier_delay + aircraft_delay)]
head(dtable, 4)
# Delete tot_delay column
dtable[, tot_delay := NULL]
# Add max_delay column grouped by origin and dest
dtable[, max_delay := max(aircraft_delay), by=.(origin, dest)]
dtable[, max_delay := NULL]
# Add date and tot_delay columns
dtable[, c("date", "tot_delay") :=
       list(paste(month, day, year, sep="/"),
            (carrier_delay + aircraft_delay))]
# Modify select rows of tot_delay column
dtable[month == 12, tot_delay := carrier_delay]
dtable[, c("date", "tot_delay") := NULL]
# Add several columns
dtable[, c("max_carrier", "max_aircraft") := lapply(.SD, max),
 by=.(origin, dest),
 .SDcols=c("carrier_delay", "aircraft_delay")]
# Remove columns
dtable[, c("max_carrier", "max_aircraft") := NULL]
# Modifying by reference is much faster than standard R
summary(microbenchmark(
  dt=dtable[, tot_delay := (carrier_delay + aircraft_delay)],
  rcode=(dtable[, "tot_delay"] <- dtable[, "carrier_delay"] + dtable[, "aircraft_delay"]),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# Add a key based on the "origin" column
setkey(dtable, origin)
haskey(dtable)
key(dtable)
# Select rows with LGA using the key
dtable["LGA"]
all.equal(dtable["LGA"], dtable[origin == "LGA"])
# Select rows with LGA and JFK using the key
dtable[c("LGA", "JFK")]
# Add a key based on the "origin" and "dest" columns
setkey(dtable, origin, dest)
key(dtable)
# Select rows with origin from JFK and MIA
dtable[c("JFK", "MIA")]
# Select rows with origin from JFK and dest to MIA
dtable[.("JFK", "MIA")]
all.equal(dtable[.("JFK", "MIA")],
    dtable[origin == "JFK" & dest == "MIA"])
# Selecting rows using a key is much faster than standard R
summary(microbenchmark(
  with_key=dtable[.("JFK", "MIA")],
  standard_r=dtable[origin == "JFK" & dest == "MIA"],
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# Create data frame and coerce it to data table
dtable <- data.frame(col1=sample(7), col2=sample(7), col3=sample(7))
class(dtable); dtable
data.table::setDT(dtable)
class(dtable); dtable
# Coerce dtable into data frame
data.table::setDF(dtable)
class(dtable); dtable
# Or
dtable <- data.table:::as.data.frame.data.table(dtable)
# SetDF() is much faster than as.data.frame()
summary(microbenchmark(
  asdataframe=data.table:::as.data.frame.data.table(dtable),
  setDF=data.table::setDF(dtable),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# Coerce xts to a data frame
pricev <- rutils::etfenv$VTI
class(pricev); head(pricev)
pricev <- as.data.frame(pricev)
class(pricev); head(pricev)
# Coerce data frame to a data table
data.table::setDT(pricev, keep.rownames=TRUE)
class(pricev); head(pricev)
# Dates are coerced to strings
sapply(pricev, class)
# Coerce xts directly to a data table
dtable <- as.data.table(rutils::etfenv$VTI,
  keep.rownames=TRUE)
class(dtable); head(dtable)
# Dates are not coerced to strings
sapply(dtable, class)
all.equal(pricev, dtable, check.attributes=FALSE)

# Install package fst
install.packages("fst")
# Load package fst
library(fst)
# Get documentation for package fst
# Get short description
packageDescription("fst")
# Load help page
help(package="fst")
# List all datasets in "fst"
data(package="fst")
# List all objects in "fst"
ls("package:fst")
# Remove fst from search path
detach("package:fst")

# Read a data frame from CSV file
dirn <- "/Users/jerzy/Develop/lecture_slides/data/"
filen <- file.path(dirn, "weather_delays14.csv")
data.table::setDF(dframe)
class(dframe); dim(dframe)
# Write data frame to .fst file in different ways
fst::write_fst(dframe, path="dframe.fst")
write.csv(dframe, file="dframe2.csv")
# microbenchmark speed of fst::write_fst()
library(microbenchmark)
summary(microbenchmark(
  fst=fst::write_fst(dframe, path="dframe.csv"),
  write_csv=write.csv(dframe, file="dframe2.csv"),
  cat=cat(unlist(dframe), file="dframe3.csv"),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# fst::read_fst() reads the same data as read.csv()
all.equal(read.csv(filen),
    fst::read_fst("dframe.fst"))
# fst::read_fst() is 10 times faster than read.csv()
summary(microbenchmark(
  fst=fst::read_fst("dframe.fst"),
  read_csv=read.csv(filen),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# Coerce TAQ xts to a data frame
library(HighFreq)
taq <- HighFreq::SPY_TAQ
taq <- as.data.frame(taq)
class(taq)
# Coerce data frame to a data table
data.table::setDT(taq, keep.rownames=TRUE)
class(taq); head(taq)
# Get memory size of data table
format(object.size(taq), units="MB")
# Save data table to .fst file
fst::write_fst(taq, path="/Users/jerzy/Develop/data/taq.fst")
# Create reference to .fst file similar to a data frame
refst <- fst::fst("/Users/jerzy/Develop/data/taq.fst")
class(refst)
# Memory size of reference to .fst is very small
format(object.size(refst), units="MB")
# Get sizes of all objects in workspace
sort(sapply(mget(ls()), object.size))
# Reference to .fst can be treated similar to a data table
dim(taq); dim(refst)
fst:::print.fst_table(refst)
# Subset reference to .fst just like a data table
refst[1e4:(1e4+5), ]

cat("Enter\ttab")  # Cat() parses backslash escape sequences
print("Enter\ttab")

textv <- print("hello")
textv  # Print() returns its argument

# Create string
textv <- "Title: My Text\nSome numbers: 1,2,3,...\nRprofile files contain code executed at R startup,\n"

cat(textv, file="mytext.txt")  # Write to text file

cat("Title: My Text",  # Write several lines to text file
    "Some numbers: 1,2,3,...",
    "Rprofile files contain code executed at R startup,",
    file="mytext.txt", sep="\n")

save(textv, file="mytext.RData")  # Write to binary file

print(pi)
print(pi, digits=10)
getOption("digits")
foo <- 12
bar <- "weeks"
sprintf("There are %i %s in the year", foo, bar)

# Read text from file
scan(file="mytext.txt", what=character(), sep="\n")

# Read lines from file
readLines(con="mytext.txt")

# Read text from console
inputv <- readline("Enter a number: ")
class(inputv)
# Coerce to numeric
inputv <- as.numeric(inputv)

# Read text from file and display in editor:
# file.show("mytext.txt")
# file.show("mytext.txt", pager="")

setwd("/Users/jerzy/Develop/lecture_slides/data")
dframe <- data.frame(type=c("rose", "daisy", "tulip"),
  color=c("red", "white", "yellow"),
  price=c(1.5, 0.5, 1.0),
  row.names=c("flower1", "flower2", "flower3"))  # end data.frame
matv <- matrix(sample(1:12), ncol=3,
  dimnames=list(NULL, c("col1", "col2", "col3")))
rownames(matv) <- paste("row", 1:NROW(matv), sep="")
# Write data frame to text file, and then read it back
write.table(dframe, file="florist.txt")
readf <- read.table(file="florist.txt")
readf  # A data frame
all.equal(readf, dframe)

# Write matrix to text file, and then read it back
write.table(matv, file="matrix.txt")
readmat <- read.table(file="matrix.txt")
readmat  # write.table() coerced matrix to data frame
class(readmat)
all.equal(readmat, matv)
# Coerce from data frame back to matrix
readmat <- as.matrix(readmat)
class(readmat)
all.equal(readmat, matv)

# Create a data frame
dframe <- data.frame(small=c(3, 5), medium=c(9, 11), large=c(15, 13))

# Launch spreadsheet-style data editor
dframe <- edit(dframe)

# Copy the data frame to clipboard
write.table(x=dframe, file="clipboard", sep="\t")

# Wrapper function for copying data frame from R into clipboard
# by default, data is tab delimited, with a header
write_clip <- function(data, namev=FALSE, col.names=TRUE, ...) {
  write.table(x=data, file="clipboard", sep="\t",
      row.names=namev, col.names=col.names, ...)
}  # end write_clip

write_clip(data=dframe)

# Wrapper function for copying data frame from clipboard into R
# by default, data is tab delimited, with a header
read_clip <- function(file="clipboard", sep="\t", header=TRUE, ...) {
  read.table(file=file, sep=sep, header=header, ...)
}  # end read_clip

dframe <- read.table("clipboard", header=TRUE)
dframe <- read_clip()

# Write data frame to CSV file, and then read it back
write.csv(dframe, file="florist.csv")
readf <- read.csv(file="florist.csv")
readf  # the row names are read in as extra column
# Restore row names
rownames(readf) <- readf[, 1]
readf <- readf[, -1]  # Remove extra column
readf
all.equal(readf, dframe)
# Read data frame, with row names from first column
readf <- read.csv(file="florist.csv", row.names=1)
readf
all.equal(readf, dframe)

# Write data frame to CSV file, without row names
write.csv(dframe, row.names=FALSE, file="florist.csv")
readf <- read.csv(file="florist.csv")
readf  # A data frame without row names
all.equal(readf, dframe)

# Open a read connection to a file
filecon = file("/Users/jerzy/Develop/lecture_slides/data/etf_prices_crsp.csv", "r")
# Read the first 10 rows
data10 <- read.csv(filecon, nrows=10)
# Read another 10 rows
data20 <- read.csv(filecon, nrows=10, header=FALSE)
colnames(data20) <- colnames(data10)
# Close the connection to the file
close(filecon)
# Open a read connection to a file
filecon = file("/Users/jerzy/Develop/lecture_slides/data/etf_prices_crsp.csv", "r")
# Read the first 1000 rows
data10 <- read.csv(filecon, nrows=1e3)
colv <- colnames(data10)
# Write to a file
countv <- 1
write.csv(data10, paste0("/Users/jerzy/Develop/data/temp/etf_prices_", countv, ".csv"))
# Read remaining rows in a loop 10 rows at a time
# Can produce error without getting to end of file
while (isOpen(filecon)) {
  datav <- read.csv(filecon, nrows=1e3)
  colnames(datav) <- colv
  write.csv(datav, paste0("/Users/jerzy/Develop/data/temp/etf_prices_", countv, ".csv"))
  countv <- countv + 1
}  # end while

# Write matrix to csv file, and then read it back
write.csv(matv, file="matrix.csv")
readmat <- read.csv(file="matrix.csv", row.names=1)
readmat  # Read.csv() reads matrix as data frame
class(readmat)
readmat <- as.matrix(readmat)  # Coerce to matrix
all.equal(readmat, matv)
write.csv(matv, row.names=FALSE,
    file="matrix_ex_rows.csv")
readmat <- read.csv(file="matrix_ex_rows.csv")
readmat <- as.matrix(readmat)
readmat  # A matrix without row names
all.equal(readmat, matv)

setwd("/Users/jerzy/Develop/lecture_slides/data")
library(MASS)  # Load package "MASS"
# Write to CSV file by row - it's very SLOW!!!
MASS::write.matrix(matv, file="matrix.csv", sep=",")
# Read using scan() and skip first line with colnames
readmat <- scan(file="matrix.csv", sep=",", skip=1,
  what=numeric())
# Read colnames
colv <- readLines(con="matrix.csv", n=1)
colv  # this is a string!
# Convert to char vector
colv <- strsplit(colv, split=",")[[1]]
readmat  # readmat is a vector, not matrix!
# Coerce by row to matrix
readmat <- matrix(readmat, ncol=NROW(colv), byrow=TRUE)
# Restore colnames
colnames(readmat) <- colv
readmat
# Scan() is a little faster than read.csv()
library(microbenchmark)
summary(microbenchmark(
  read_csv=read.csv("matrix.csv"),
  scan=scan(file="matrix.csv", sep=",",
    skip=1, what=numeric()),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# Read data from a csv file, including row names
matv <- read.csv(file="data/matrix_bad.csv", row.names=1)
matv
class(matv)
# Columns with bad data are character or factor
sapply(matv, class)
# Coerce character column to numeric
matv$col2 <- as.numeric(matv$col2)
# Or
# Copy row names
namev <- rownames(matv)
# sapply loop over columns and coerce to numeric
matv <- sapply(matv, as.numeric)
# Restore row names
rownames(matv) <- namev
# Replace NAs with zero
matv[is.na(matv)] <- 0
# matrix without NAs
matv

setwd("/Users/jerzy/Develop/lecture_slides/data")
rm(list=ls())
# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
library(zoo)  # Load package zoo
# Create zoo with Date index
datev <- seq(from=as.Date("2013-06-15"), by="day",
        length.out=100)
pricev <- zoo(rnorm(NROW(datev)), order.by=datev)
head(pricev, 3)
# Write zoo series to text file, and then read it back
write.zoo(pricev, file="pricev.txt")
pricezoo <- read.zoo("pricev.txt")  # Read it back
all.equal(pricezoo, pricev)
# Perform the same using write.table() and read.table()
# First coerce pricev into data frame
dframe <- as.data.frame(pricev)
dframe <- cbind(datev, dframe)
# Write pricev to text file using write.table
write.table(dframe, file="pricev.txt",
      row.names=FALSE, col.names=FALSE)
# Read data frame from file
pricezoo <- read.table(file="pricev.txt")
sapply(pricezoo, class)  # A data frame
# Coerce data frame into pricev
pricezoo <- zoo::zoo(
  drop(as.matrix(pricezoo[, -1])),
  order.by=as.Date(pricezoo[, 1]))
all.equal(pricezoo, pricev)

library(zoo)  # Load package zoo
# Write zoo series to CSV file, and then read it back
write.zoo(pricev, file="zooseries.csv", sep=",", col.names=TRUE)
pricezoo <- read.zoo(file="zooseries.csv",
  header=TRUE, sep=",", drop=FALSE)
all.equal(pricev, drop(pricezoo))

# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
# Create zoo with POSIXct date-time index
datev <- seq(from=as.POSIXct("2014-07-14"),
        by="hour", length.out=100)
zooseries <- zoo(rnorm(NROW(datev)), order.by=datev)
head(zooseries, 3)
# Write zoo series to CSV file using write.zoo()
write.zoo(zooseries, file="zooseries.csv", sep=",", col.names=TRUE)
# Read from CSV file using read.csv.zoo() - doesn't work
zooread <- read.csv.zoo(file="zooseries.csv", header=FALSE,
  format="%Y-%m-%d %H:%M:%S", tz="America/New_York")
# Read from CSV file using read.zoo() - error
zooread <- read.zoo(file="zooseries.csv", header=FALSE,
  sep=",", FUN=as.POSIXct, format="%Y-%m-%d %H:%M:%S")
# Write zoo series to CSV file using write.table()
write.table(zooseries, file="zooseries.csv", sep=",",
      row.names=TRUE, col.names=FALSE)
# Read from CSV file using read.zoo() with format argument
zooread <- read.zoo(file="zooseries.csv", header=FALSE,
  sep=",", FUN=as.POSIXct, format="%Y-%m-%d %H:%M:%S")
all.equal(zooseries, zooread) # Works
# Coerce zoo series into data frame with custom date format
dframe <- as.data.frame(zooseries)
rownames(dframe) <- format(index(zooseries), format="%m-%d-%Y %H:%M:%S")
# Write zoo series to csv file using write.table
write.table(dframe, file="zooseries.csv", sep=",",
      row.names=TRUE, col.names=FALSE)
# Read from CSV file using read.zoo()
zooread <- read.zoo(file="zooseries.csv", header=FALSE, sep=",",
  FUN=as.POSIXct, format="%m-%d-%Y %H:%M:%S")
all.equal(zooseries, zooread) # Works
# Or using read.csv.zoo()
zooread <- read.csv.zoo(file="zooseries.csv", header=FALSE,
  format="%m-%d-%Y %H:%M:%S", tz="America/New_York")
head(zooread, 3)
all.equal(zooseries, zooread, check.attributes=FALSE) # Works

# Read time series from CSV file, with numeric date-time
datazoo <- read.table(file="/Users/jerzy/Develop/lecture_slides/data/es_ohlc.csv",
  header=TRUE, sep=",")
# A data frame
class(datazoo)
sapply(datazoo, class)
# Coerce data frame into xts series
datazoo <- xts::xts(as.matrix(datazoo[, -1]),
  order.by=as.POSIXct.numeric(datazoo[, 1], tz="America/New_York",
                        origin="1970-01-01"))
# An xts series
class(datazoo)
head(datazoo, 3)

rm(list=ls())  # Delete all objects in workspace
var1 <- 1; var2 <- 2
ls()  # List all objects
ls()[1]  # List first object
args(save)  # List arguments of save function
Save "var1" to a binary file using string argument
save("var1", file="my_data.RData")
Save "var1" to a binary file using object name
save(var1, file="my_data.RData")
Save multiple objects
save(var1, var2, file="my_data.RData")
Save first object in list by passing to "..." argument
ls()[1] is not evaluated
save(ls()[1], file="my_data.RData")
Save first object in list by passing to "list" argument
save(list=ls()[1], file="my_data.RData")
Save whole list by passing it to the "list" argument
save(list=ls(), file="my_data.RData")

rm(list=ls())  # Delete all objects in workspace
# Load objects from file
loadobj <- load(file="my_data.RData")
loadobj  # vector of loaded objects
ls()  # List objects
# Assign new values to objects in  global environment
sapply(loadobj, function(symboln) {
  assign(symboln, runif(1), envir=globalenv())
})  # end sapply
ls()  # List objects
# Assign new values to objects using for loop
for (symboln in loadobj) {
  assign(symboln, runif(1))
}  # end for
ls()  # List objects
# Save vector of objects
save(list=loadobj, file="my_data.RData")
# Remove only loaded objects
rm(list=loadobj)
# Remove the object "loadobj"
rm(loadobj)

sink("sinkdata.txt")Redirect text output to file

cat("Redirect text output from R\n")
print(runif(10))
cat("\nEnd data\nbye\n")

sink()  # turn redirect off

pdf("Rgraph.pdf", width=7, height=4)  # Redirect graphics to pdf file

cat("Redirect data from R into pdf file\n")
myvar <- seq(-2*pi, 2*pi, len=100)
plot(x=myvar, y=sin(myvar), main="Sine wave",
   xlab="", ylab="", type="l", lwd=2, col="red")
cat("\nEnd data\nbye\n")

dev.off()  # turn pdf output off

png("r_plot.png")  # Redirect graphics output to png file

cat("Redirect graphics from R into png file\n")
plot(x=myvar, y=sin(myvar), main="Sine wave",
 xlab="", ylab="", type="l", lwd=2, col="red")
cat("\nEnd data\nbye\n")

dev.off()  # turn png output off
