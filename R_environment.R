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
# Compare the two methods
all.equal(cumsumv, cumsumv2)
# Microbenchmark the two methods
library(microbenchmark)
summary(microbenchmark(
  cumsum=cumsum(vecv),
  loop_alloc={
    cumsumv2 <- vecv
    for (i in 2:NROW(vecv))
cumsumv2[i] <- (vecv[i] + cumsumv2[i-1])
  },
  loop_nalloc={
    # Doesn't allocate memory to cumsumv3
    cumsumv3 <- vecv[1]
    for (i in 2:NROW(vecv))
# This command adds an extra element to cumsumv3
cumsumv3[i] <- (vecv[i] + cumsumv3[i-1])
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
setwd("/Users/jerzy/Develop/R")  # Set cwd
getwd()  # Get cwd
Sys.time()  # Get date and time
Sys.Date()  # Get date only
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
loadobj <- load(file="/Users/jerzy/Develop/lecture_slides/data/my_data.RData")
loadobj
ls()  # List objects
  q()  # quit R session
history(5)  # Display last 5 commands
savehistory(file="myfile")  # Default is ".Rhistory"
loadhistory(file="myfile")  # Default is ".Rhistory"
sessionInfo()  # Get R version and other session info
Sys.getenv()[5:7]  # List some environment variables
Sys.getenv("HOME")  # Get R user HOME directory
Sys.setenv(Home="/Users/jerzy/Develop/data")  # Set HOME directory
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
# zero - warnings are stored and printed after top-confl function has completed
options(warn=0)
# One - warnings are printed as they occur
options(warn=1)
# 2 or larger - warnings are turned into errors
options(warn=2)
# Save all options in variable
optionv <- options()
# Restore all options from variable
options(optionv)
# R startup (site) directory
paste(R.home(), "etc", sep="/")
file.path(R.home(), "etc")  # Better way
# Perform tilde-expansions and convert to readable format
normalizePath(file.path(R.home(), "etc"), winslash="/")
normalizePath(R.home("etc"), winslash="/")
normalizePath("~", winslash="/")  # Windows user HOME directory
Sys.getenv("HOME")  # R user HOME directory
setwd("/Users/jerzy/Develop/R")
getwd()  # Current working directory
# R startup (site) directory
normalizePath(file.path(R.home(), "etc"), winslash="/")
# R executable directory
normalizePath(file.path(R.home(), "bin/x64"), winslash="/")
# R documentation directory
normalizePath(file.path(R.home(), "doc/manual"), winslash="/")
sample(dir(), 5)  # Get 5 file names - dir() lists all files
sample(dir(pattern="csv"), 5)  # List files containing "csv"
sample(list.files(R.home()), 5)  # All files in R_HOME directory
sample(list.files(R.home("etc")), 5)  # All files in "etc" sub-directory of R_HOME directory
sample(list.dirs(), 5)  # Directories in cwd
list.dirs(R.home("etc"))  # Directories in "etc" sub-directory
sample(Sys.glob("*.csv"), 5)
Sys.glob(R.home("etc"))
getwd()  # Get cwd
setwd("/Users/jerzy/Develop/R")
# help(Startup)  # Description of R session startup mechanism
# Files in R startup directory directory
dir(normalizePath(file.path(R.home(), "etc"), winslash="/"))
# *.R* files in cwd directory
getwd()
dir(getwd(), all.files=TRUE, pattern="\\.R")
dir(getwd(), all.files=TRUE, pattern=glob2rx("*.R*"))
setwd("/Users/jerzy/Develop/R")
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
globv <- 1
# Get objects in current environment
ls(environment())
# Create new environment
envv <- new.env()
# Get calling environment of new environment
parent.env(envv)
# Assign Value to Name
assign("var1", 3, envir=envv)
# Create object in new environment
envv$var2 <- 11
# Get objects in new environment
ls(envv)
# Get objects in current environment
ls(environment())
# Environments are subset like listv
envv$var1
# Environments are subset like listv
envv[["var1"]]
search()  # Get search path for R objects
listv <- list(flowers=c("rose", "daisy", "tulip"),
      trees=c("pine", "oak", "maple"))
listv$trees
attach(listv)
trees
search()  # Get search path for R objects
detach(listv)
head(trees)  # "trees" is in datasets base package
library(rutils)  # Load package rutils
# Define ETF symbols
symbolv <- c("VTI", "VEU", "IEF", "VNQ")
# Extract symbolv from rutils::etfenv
pricev <- mget(symbolv, envir=rutils::etfenv)
# pricev is a list of xts series
class(pricev)
class(pricev[[1]])
# Extract Close prices
pricev <- lapply(pricev, quantmod::Cl)
# Collapse list into time series the hard way
xts1 <- cbind(pricev[[1]], pricev[[2]], pricev[[3]], pricev[[4]])
class(xts1)
dim(xts1)
# Collapse list into time series using do.call()
pricev <- do.call(cbind, pricev)
all.equal(xts1, pricev)
class(pricev)
dim(pricev)
# Extract and cbind in single step
pricev <- do.call(cbind, lapply(
  mget(symbolv, envir=rutils::etfenv), quantmod::Cl))
# Or
# Extract and bind all data, subset by symbolv
pricev <- lapply(symbolv, function(symbol) {
    quantmod::Cl(get(symbol, envir=rutils::etfenv))
})  # end lapply
# Same, but loop over etfenv without anonymous function
pricev <- do.call(cbind,
  lapply(as.list(rutils::etfenv)[symbolv], quantmod::Cl))
# Same, but works only for OHLC series - produces error
pricev <- do.call(cbind,
  eapply(rutils::etfenv, quantmod::Cl)[symbolv])
# Drop ".Close" from column names
colnames(pricev)
do.call(rbind, strsplit(colnames(pricev), split="[.]"))[, 1]
colnames(pricev) <- do.call(rbind, strsplit(colnames(pricev), split="[.]"))[, 1]
# Or
colnames(pricev) <- unname(sapply(colnames(pricev),
    function(colname) strsplit(colname, split="[.]")[[1]][1]))
tail(pricev, 3)
# Which objects in global environment are class xts?
unlist(eapply(globalenv(), is.xts))
# Save xts to csv file
write.zoo(pricev,
  file="/Users/jerzy/Develop/lecture_slides/data/etf_series.csv", sep=",")
# Copy prices into etfenv
etfenv$etf_list <- etf_list
# Or
assign("prices", pricev, envir=etfenv)
# Save to .RData file
save(etfenv, file="etf_data.RData")
# "trees" is in datasets base package
head(trees, 3)
colnames(trees)
mean(Girth)
mean(trees$Girth)
with(trees,
     c(mean(Girth), mean(Height), mean(Volume)))
script_dir <- "/Users/jerzy/Develop/R/scripts"
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
plot_dir <- "/Users/jerzy/Develop/data"
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
