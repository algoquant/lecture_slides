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
# Compare the two methods
all.equal(cumsumv, cumsumv2)
# Microbenchmark the two methods
library(microbenchmark)
summary(microbenchmark(
  cumsum=cumsum(vectorv),
  loop_alloc={
    cumsumv2 <- vectorv
    for (i in 2:NROW(vectorv))
cumsumv2[i] <- (vectorv[i] + cumsumv2[i-1])
  },
  loop_nalloc={
    # Doesn't allocate memory to cumsumv3
    cumsumv3 <- vectorv[1]
    for (i in 2:NROW(vectorv))
# This command adds an extra element to cumsumv3
cumsumv3[i] <- (vectorv[i] + cumsumv3[i-1])
  },
  times=10))[, c(1, 4, 5)]
# "<-" and "=" are valid assignment operators
myvar <- 3
# Typing a symbol or expression evaluates it
myvar
# Text in quotes is interpreted as a string
myvar <- "Hello World!"
# Typing a symbol or expression evaluates it
myvar
myvar  # Text after hash is treated as comment
getwd()  # Get cwd
setwd("C:/Develop/R")  # Set cwd
getwd()  # Get cwd
Sys.time()  # Get date and time
Sys.Date()  # Get date only
rm(list=ls())
setwd("C:/Develop/lecture_slides/data")
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
     file="C:/Develop/lecture_slides/data/my_data.RData")
rm(list=ls())  # Remove all objects
ls()  # List objects
load_ed <- load(file="C:/Develop/lecture_slides/data/my_data.RData")
load_ed
ls()  # List objects
  q()  # quit R session
history(5)  # Display last 5 commands
savehistory(file="myfile")  # Default is ".Rhistory"
loadhistory(file="myfile")  # Default is ".Rhistory"
sessionInfo()  # Get R version and other session info
Sys.getenv()[5:7]  # List some environment variables
Sys.getenv("HOME")  # Get R user HOME directory
Sys.setenv(Home="C:/Develop/data")  # Set HOME directory
Sys.getenv("HOME")  # Get user HOME directory
Sys.getenv("R_HOME")  # Get R_HOME directory
R.home()  # Get R_HOME directory
R.home("etc")  # Get "etc" sub-directory of R_HOME
# ?options  # Long list of global options
# Interpret strings as characters, not factors
getOption("stringsAsFactors")  # Display option
options("stringsAsFactors")  # Display option
options(stringsAsFactors=FALSE)  # Set option
# Number of digits printed for numeric values
# Control exponential scientific notation of print method
# Positive "scipen" values bias towards fixed notation
# Negative "scipen" values bias towards scientific notation
options(scipen=100)
# Maximum number of items printed to console
options(max.print=30)
# Warning levels options
# Negative - warnings are ignored
options(warn=-1)
# zero - warnings are stored and printed after top-level function has completed
options(warn=0)
# One - warnings are printed as they occur
options(warn=1)
# 2 or larger - warnings are turned into errors
options(warn=2)
# Save all options in variable
op_tions <- options()
# Restore all options from variable
options(op_tions)
# R startup (site) directory
paste(R.home(), "etc", sep="/")
file.path(R.home(), "etc")  # Better way
# Perform tilde-expansions and convert to readable format
normalizePath(file.path(R.home(), "etc"), winslash="/")
normalizePath(R.home("etc"), winslash="/")
normalizePath("~", winslash="/")  # Windows user HOME directory
Sys.getenv("HOME")  # R user HOME directory
setwd("C:/Develop/R")
getwd()  # Current working directory
# R startup (site) directory
normalizePath(file.path(R.home(), "etc"), winslash="/")
# R executable directory
normalizePath(file.path(R.home(), "bin/x64"), winslash="/")
# R documentation directory
normalizePath(file.path(R.home(), "doc/manual"), winslash="/")
sample(dir(), 5)  # Get 5 file names - dir() listv all files
sample(dir(pattern="csv"), 5)  # List files containing "csv"
sample(list.files(R.home()), 5)  # All files in R_HOME directory
sample(list.files(R.home("etc")), 5)  # All files in "etc" sub-directory of R_HOME directory
sample(list.dirs(), 5)  # Directories in cwd
list.dirs(R.home("etc"))  # Directories in "etc" sub-directory
sample(Sys.glob("*.csv"), 5)
Sys.glob(R.home("etc"))
getwd()  # Get cwd
setwd("C:/Develop/R")
# help(Startup)  # Description of R session startup mechanism
# Files in R startup directory directory
dir(normalizePath(file.path(R.home(), "etc"), winslash="/"))
# *.R* files in cwd directory
getwd()
dir(getwd(), all.files=TRUE, pattern="\\.R")
dir(getwd(), all.files=TRUE, pattern=glob2rx("*.R*"))
setwd("C:/Develop/R")
scan(file=".Rprofile", what=character(), sep="\n")
cat("sourcing .Rprofile file\n")
cat("sourcing .Rprofile file\n")
rm(list=ls())
# Get base environment
baseenv()
# Get global environment
globalenv()
# Get current environment
environment()
# Get environment class
class(environment())
# Define variable in current environment
glob_var <- 1
# Get objects in current environment
ls(environment())
# Create new environment
new_env <- new.env()
# Get calling environment of new environment
parent.env(new_env)
# Assign Value to Name
assign("new_var1", 3, envir=new_env)
# Create object in new environment
new_env$new_var2 <- 11
# Get objects in new environment
ls(new_env)
# Get objects in current environment
ls(environment())
# Environments are subset like listv
new_env$new_var1
# Environments are subset like listv
new_env[["new_var1"]]
search()  # Get search path for R objects
my_list <- list(flowers=c("rose", "daisy", "tulip"),
        trees=c("pine", "oak", "maple"))
my_list$trees
attach(my_list)
trees
search()  # Get search path for R objects
detach(my_list)
head(trees)  # "trees" is in datasets base package
library(rutils)  # Load package rutils
# Define ETF symbols
symbols <- c("VTI", "VEU", "IEF", "VNQ")
# Extract symbols from rutils::etfenv
prices <- mget(symbols, envir=rutils::etfenv)
# prices is a list of xts series
class(prices)
class(prices[[1]])
# Extract Close prices
prices <- lapply(prices, quantmod::Cl)
# Collapse list into time series the hard way
xts1 <- cbind(prices[[1]], prices[[2]], prices[[3]], prices[[4]])
class(xts1)
dim(xts1)
# Collapse list into time series using do.call()
prices <- do.call(cbind, prices)
all.equal(xts1, prices)
class(prices)
dim(prices)
# Extract and cbind in single step
prices <- do.call(cbind, lapply(
  mget(symbols, envir=rutils::etfenv), quantmod::Cl))
# Or
# Extract and bind all data, subset by symbols
prices <- lapply(symbols, function(symbol) {
    quantmod::Cl(get(symbol, envir=rutils::etfenv))
})  # end lapply
# Same, but loop over etfenv without anonymous function
prices <- do.call(cbind,
  lapply(as.list(rutils::etfenv)[symbols], quantmod::Cl))
# Same, but works only for OHLC series - produces error
prices <- do.call(cbind,
  eapply(rutils::etfenv, quantmod::Cl)[symbols])
# Drop ".Close" from column names
colnames(prices[, 1:4])
do.call(rbind, strsplit(colnames(prices[, 1:4]), split="[.]"))[, 1]
colnames(prices) <- do.call(rbind, strsplit(colnames(prices), split="[.]"))[, 1]
# Or
colnames(prices) <- unname(sapply(colnames(prices),
    function(colname) strsplit(colname, split="[.]")[[1]][1]))
tail(prices, 3)
# Which objects in global environment are class xts?
unlist(eapply(globalenv(), is.xts))
# Save xts to csv file
write.zoo(prices,
  file="C:/Develop/lecture_slides/data/etf_series.csv", sep=",")
# Copy prices into etfenv
etfenv$etf_list <- etf_list
# Or
assign("prices", prices, envir=etfenv)
# Save to .RData file
save(etfenv, file="etf_data.RData")
# "trees" is in datasets base package
head(trees, 3)
colnames(trees)
mean(Girth)
mean(trees$Girth)
with(trees,
     c(mean(Girth), mean(Height), mean(Volume)))
script_dir <- "C:/Develop/R/scripts"
# Execute script file and print the commands
source(file.path(script_dir, "script.R"),
 echo=TRUE)
####################################
#Script.R file contains R script to demonstrate sourcing from script files
# Print information about this process
print(paste0("print: This test script was run at: ", format(Sys.time())))
cat("cat: This test script was run at:", format(Sys.time()), "\n")
# Display first 6 rows of cars data frame
head(cars)
# Define a function
fun_c <- function(x) x+1
# Read a line from console
readline("Press Return to continue")
# Plot sine function in x11 window
x11()
curve(expr=sin, type="l", xlim=c(-2*pi, 2*pi),
xlab="", ylab="", lwd=2, col="orange",
main="Sine function")
# Get help about running R scripts and batch processes
?BATCH
?Rscript
#Script_args.R contains R script that accepts arguments
# Print information about this process
cat("cat: This script was run at:", format(Sys.time()), "\n")
# Read arguments supplied on the command line
arg_s <- commandArgs(TRUE)
# Print the arguments
cat(paste0("arguments supplied on command line: ", paste(arg_s, collapse=", "), "\n"))
# Return sum of arguments
sum(as.numeric(arg_s))
#Plot_to_file.R
#R script to demonstrate plotting to file
# Redirect graphics output to png file
plot_dir <- "C:/Develop/data"
png(file.path(plot_dir, "r_plot.png"))
# Plot sine function
curve(expr=sin, type="l", xlim=c(-2*pi, 2*pi),
xlab="", ylab="", lwd=2, col="orange",
main="Sine function")
# Turn png output off
dev.off()
#Plot_interactive.R
#R script to demonstrate interactive plotting
# Plot sine function in x11 window
x11()
curve(expr=sin, type="l", xlim=c(-2*pi, 2*pi),
xlab="", ylab="", lwd=2, col="orange",
main="Sine function")
# Wait until x11 window is closed
while (!is.null(dev.list())) Sys.sleep(1)
