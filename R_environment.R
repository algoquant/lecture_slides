# Display documentation on function "getwd"
help(getwd)
# Equivalent to "help(getwd)"
?getwd
# Open the hypertext documentation
help.start()
# Calculate cumulative sum of a vector
vec_tor <- runif(1e5)
# Use compiled function
cum_sum <- cumsum(vec_tor)
# Use for loop
cum_sum2 <- vec_tor
for (i in 2:NROW(vec_tor))
  cum_sum2[i] <- (vec_tor[i] + cum_sum2[i-1])
# Compare the two methods
all.equal(cum_sum, cum_sum2)
# Microbenchmark the two methods
library(microbenchmark)
summary(microbenchmark(
  cumsum=cumsum(vec_tor),
  loop_alloc={
    cum_sum2 <- vec_tor
    for (i in 2:NROW(vec_tor))
cum_sum2[i] <- (vec_tor[i] + cum_sum2[i-1])
  },
  loop_nalloc={
    # Doesn't allocate memory to cum_sum3
    cum_sum3 <- vec_tor[1]
    for (i in 2:NROW(vec_tor))
# This command adds an extra element to cum_sum3
cum_sum3[i] <- (vec_tor[i] + cum_sum3[i-1])
  },
  times=10))[, c(1, 4, 5)]
# "<-" and "=" are valid assignment operators
my_var <- 3
# Typing a symbol or expression evaluates it
my_var
# Text in quotes is interpreted as a string
my_var <- "Hello World!"
# Typing a symbol or expression evaluates it
my_var
my_var  # Text after hash is treated as comment
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
sample(dir(), 5)  # Get 5 file names - dir() lists all files
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
# Environments are subset like lists
new_env$new_var1
# Environments are subset like lists
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
sym_bols <- c("VTI", "VEU", "IEF", "VNQ")
# Extract sym_bols from rutils::etf_env
price_s <- mget(sym_bols, envir=rutils::etf_env)
# price_s is a list of xts series
class(price_s)
class(price_s[[1]])
# Extract Close prices
price_s <- lapply(price_s, quantmod::Cl)
# Collapse list into time series the hard way
xts_1 <- cbind(price_s[[1]], price_s[[2]], price_s[[3]], price_s[[4]])
class(xts_1)
dim(xts_1)
# Collapse list into time series using do.call()
price_s <- do.call(cbind, price_s)
all.equal(xts_1, price_s)
class(price_s)
dim(price_s)
# Extract and cbind in single step
price_s <- do.call(cbind, lapply(
  mget(sym_bols, envir=rutils::etf_env), quantmod::Cl))
# Or
# Extract and bind all data, subset by sym_bols
price_s <- lapply(sym_bols, function(sym_bol) {
    quantmod::Cl(get(sym_bol, envir=rutils::etf_env))
})  # end lapply
# Same, but loop over etf_env without anonymous function
price_s <- do.call(cbind,
  lapply(as.list(rutils::etf_env)[sym_bols], quantmod::Cl))
# Same, but works only for OHLC series - produces error
price_s <- do.call(cbind,
  eapply(rutils::etf_env, quantmod::Cl)[sym_bols])
# Drop ".Close" from column names
colnames(price_s[, 1:4])
do.call(rbind, strsplit(colnames(price_s[, 1:4]), split="[.]"))[, 1]
colnames(price_s) <- do.call(rbind, strsplit(colnames(price_s), split="[.]"))[, 1]
# Or
colnames(price_s) <- unname(sapply(colnames(price_s),
    function(col_name) strsplit(col_name, split="[.]")[[1]][1]))
tail(price_s, 3)
# Which objects in global environment are class xts?
unlist(eapply(globalenv(), is.xts))
# Save xts to csv file
write.zoo(price_s,
  file="C:/Develop/lecture_slides/data/etf_series.csv", sep=",")
# Copy price_s into etf_env
etf_env$etf_list <- etf_list
# Or
assign("price_s", price_s, envir=etf_env)
# Save to .RData file
save(etf_env, file="etf_data.RData")
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
