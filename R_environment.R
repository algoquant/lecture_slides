library(knitr)
opts_chunk$set(prompt=TRUE, eval=FALSE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=6, fig.height=5)
options(width=60, dev='pdf')
options(digits=3)
thm <- knit_theme$get("acid")
knit_theme$set(thm)
# display documentation on function "getwd"
help(getwd)
?getwd  # equivalent to "help(getwd)"
help.start()  # open the hypertext documentation
# "<-" and "=" are valid assignment operators
my_var <- 3

# typing a symbol or expression evaluates it
my_var

# text in quotes is interpreted as a string
my_var <- "Hello World!"

# typing a symbol or expression evaluates it
my_var

my_var  # text after hash is treated as comment
getwd()  # get cwd
setwd("C:/Develop/R")  # set cwd
getwd()  # get cwd
Sys.time()  # get date and time

Sys.Date()  # get date only
rm(list=ls())
setwd("C:/Develop/R/lecture_slides/data")
var1 <- 3  # define new object
ls()  # list all objects in workspace
# list objects starting with "v"
ls(pattern=glob2rx("v*"))
# remove all objects starting with "v"
rm(list=ls(pattern=glob2rx("v*")))
save.image()  # save workspace to file .RData in cwd
rm(var1)  # remove object
ls()  # list objects
load(".RData")
ls()  # list objects
var2 <- 5  # define another object
save(var1, var2,  # save selected objects
     file="C:/Develop/R/lecture_slides/data/my_data.RData")
rm(list=ls())  # remove all objects
ls()  # list objects
load_ed <- load(file="C:/Develop/R/lecture_slides/data/my_data.RData")
load_ed
ls()  # list objects
  q()  # quit R session
history(5)  # display last 5 commands
savehistory(file="myfile")  # default is ".Rhistory"
loadhistory(file="myfile")  # default is ".Rhistory"
sessionInfo()  # get R version and other session info
Sys.getenv()[5:7]  # list some environment variables

Sys.getenv("Home")  # get R user HOME directory

Sys.setenv(Home="C:/Develop/data")  # set HOME directory

Sys.getenv("Home")  # get user HOME directory

Sys.getenv("R_home")  # get R_HOME directory

R.home()  # get R_HOME directory

R.home("etc")  # get "etc" sub-directory of R_HOME
# ?options  # long list of global options
# interpret strings as characters, not factors
getOption("stringsAsFactors")  # display option
options("stringsAsFactors")  # display option
options(stringsAsFactors=FALSE)  # set option
# number of digits printed for numeric values
options(digits=3)
# control exponential scientific notation of print method
# positive "scipen" values bias towards fixed notation
# negative "scipen" values bias towards scientific notation
options(scipen=100)
# maximum number of items printed to console
options(max.print=30)
# warning levels options
# negative - warnings are ignored
options(warn=-1)
# zero - warnings are stored and printed after top-level function has completed
options(warn=0)
# one - warnings are printed as they occur
options(warn=1)
# two or larger - warnings are turned into errors
options(warn=2)
# save all options in variable
op_tions <- options()
# restore all options from variable
options(op_tions)
# R startup (site) directory
paste(R.home(), "etc", sep="/")

file.path(R.home(), "etc")  # better way

# perform tilde-expansions and convert to readable format
normalizePath(file.path(R.home(), "etc"), winslash="/")

normalizePath(R.home("etc"), winslash="/")
normalizePath("~", winslash="/")  # Windows user HOME directory

Sys.getenv("Home")  # R user HOME directory

setwd("C:/Develop/R")
getwd()  # current working directory

# R startup (site) directory
normalizePath(file.path(R.home(), "etc"), winslash="/")

# R executable directory
normalizePath(file.path(R.home(), "bin/x64"), winslash="/")

# R documentation directory
normalizePath(file.path(R.home(), "doc/manual"), winslash="/")
sample(dir(), 5)  # get 5 file names - dir() lists all files
sample(dir(pattern="csv"), 5)  # list files containing "csv"
sample(list.files(R.home()), 5)  # all files in R_HOME directory
sample(list.files(R.home("etc")), 5)  # all files in "etc" sub-directory of R_HOME directory
sample(list.dirs(), 5)  # directories in cwd
list.dirs(R.home("etc"))  # directories in "etc" sub-directory
sample(Sys.glob("*.csv"), 5)
Sys.glob(R.home("etc"))
getwd()  # get cwd
setwd("C:/Develop/R")
# help(Startup)  # description of R session startup mechanism

# files in R startup directory directory
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
# get base environment
baseenv()
# get global environment
globalenv()
# get current environment
environment()
# get environment class
class(environment())
# define variable in current environment
glob_var <- 1
# get objects in current environment
ls(environment())
# create new environment
new_env <- new.env()
# get calling environment of new environment
parent.env(new_env)
# assign Value to Name
assign("new_var1", 3, envir=new_env)
# create object in new environment
new_env$new_var2 <- 11
# get objects in new environment
ls(new_env)
# get objects in current environment
ls(environment())
# environments are subset like lists
new_env$new_var1
# environments are subset like lists
new_env[["new_var1"]]
search()  # get search path for R objects
my_list <- 
  list(flowers=c("rose", "daisy", "tulip"), 
       trees=c("pine", "oak", "maple"))
my_list$trees
attach(my_list)
trees
search()  # get search path for R objects
detach(my_list)
head(trees)  # "trees" is in datasets base package
library(HighFreq)  # load package HighFreq
# ETF symbols
sym_bols <- c("VTI", "VEU", "IEF", "VNQ")
# extract and merge all data, subset by sym_bols
price_s <- rutils::do_call(cbind, # do.call(merge
  as.list(rutils::etf_env)[sym_bols])
# extract and merge adjusted prices, subset by sym_bols
price_s <- rutils::do_call(cbind,
  lapply(as.list(rutils::etf_env)[sym_bols], quantmod::Ad))
# same, but works only for OHLC series
price_s <- rutils::do_call(cbind,
  eapply(rutils::etf_env, quantmod::Ad)[sym_bols])
# drop ".Adjusted" from colnames
colnames(price_s) <-
  sapply(colnames(price_s),
    function(col_name)
strsplit(col_name, split="[.]")[[1]])[1, ]
tail(price_s[, 1:2], 3)
# which objects in global environment are class xts?
unlist(eapply(globalenv(), is.xts))

# save xts to csv file
write.zoo(price_s,
     file='etf_series.csv', sep=",")
# copy price_s into etf_env and save to .RData file
assign("price_s", price_s, envir=etf_env)
save(etf_env, file='etf_data.RData')
# "trees" is in datasets base package
head(trees, 3)
colnames(trees)
mean(Girth)
mean(trees$Girth)
with(trees, 
     c(mean(Girth), mean(Height), mean(Volume)))
script_dir <- "C:/Develop/R/scripts"
# execute script file and print the commands
source(file.path(script_dir, "script.R"),
 echo=TRUE)

####################################
#script.R file contains R script to demonstrate sourcing from script files

# print information about this process
print(paste0("print: This test script was run at: ", format(Sys.time())))
cat("cat: This test script was run at:", format(Sys.time()), "\n")

# display first 6 rows of cars data frame
head(cars)

# define a function
fun_c <- function(x) x+1

# read a line from console
readline("Press Return to continue")

# plot sine function in x11 window
x11()
curve(expr=sin, type="l", xlim=c(-2*pi, 2*pi),
xlab="", ylab="", lwd=2, col="orange",
main="Sine function")
# get help about running R scripts and batch processes
?BATCH
?Rscript
#script_args.R contains R script that accepts arguments
# print information about this process
cat("cat: This script was run at:", format(Sys.time()), "\n")
# read arguments supplied on the command line
arg_s <- commandArgs(TRUE)
# print the arguments
cat(paste0("arguments supplied on command line: ", paste(arg_s, collapse=", "), "\n"))
# return sum of arguments
sum(as.numeric(arg_s))
#plot_to_file.R
#R script to demonstrate plotting to file

# redirect graphics output to png file
plot_dir <- "C:/Develop/data"
png(file.path(plot_dir, "r_plot.png"))

# plot sine function
curve(expr=sin, type="l", xlim=c(-2*pi, 2*pi),
xlab="", ylab="", lwd=2, col="orange",
main="Sine function")

# turn png output off
dev.off()
#plot_interactive.R
#R script to demonstrate interactive plotting

# plot sine function in x11 window
x11()
curve(expr=sin, type="l", xlim=c(-2*pi, 2*pi),
xlab="", ylab="", lwd=2, col="orange",
main="Sine function")

# wait until x11 window is closed
while (!is.null(dev.list())) Sys.sleep(1)
#perform calculations in R,
#and export to CSV files
setwd("C:/Develop/R/lecture_slides/data")
# read data frame, with row names from first column
data_read <- read.csv(file="florist.csv",
              row.names=1)
# subset data frame
data_read <-
  data_read[data_read[, "type"]=="daisy", ]
# write data frame to CSV file, with row names
write.csv(data_read, file="daisies.csv")
#perform calculations in R,
#and export to CSV files
setwd("C:/Develop/R/lecture_slides/data")
# read data frame, with row names from first column
data_read <- read.csv(file="florist.csv",
              row.names=1)
# subset data frame
data_read <-
  data_read[data_read[, "type"]=="daisy", ]
# write data frame to CSV file, with row names
write.csv(data_read, file="daisies.csv")
# install latest version of googlesheets
devtools::install_github("jennybc/googlesheets")
# load package googlesheets
library(googlesheets)
library(dplyr)
# authenticate authorize R to view and manage your files
gs_auth(new_user=TRUE)
# list the files in Google Sheets
googlesheets::gs_ls()
# register a sheet
google_sheet <- gs_title("my_data")
# view sheet summary
google_sheet
# list tab names in sheet
tab_s <- gs_ws_ls(google_sheet)
# set curl options
library(httr)
httr::set_config(config(ssl_verifypeer=0L))
# read data from sheet
gs_read(google_sheet)
# read data from single tab of sheet
gs_read(google_sheet, ws=tab_s[1])
gs_read_csv(google_sheet, ws=tab_s[1])
# or using dplyr pipes
google_sheet %>% gs_read(ws=tab_s[1])
# download data from sheet into file
gs_download(google_sheet, ws=tab_s[1],
      to="C:/Develop/R/lecture_slides/data/google_sheet.csv")
# open sheet in internet browser
gs_browse(google_sheet)
# install latest version of googlesheets
devtools::install_github("jennybc/googlesheets")
# load package googlesheets
library(googlesheets)
library(dplyr)
# authenticate authorize R to view and manage your files
gs_auth(new_user=TRUE)
# list the files in Google Sheets
googlesheets::gs_ls()
# register a sheet
google_sheet <- gs_title("my_data")
# view sheet summary
google_sheet
# list tab names in sheet
tab_s <- gs_ws_ls(google_sheet)
# set curl options
library(httr)
httr::set_config(config(ssl_verifypeer=0L))
# read data from sheet
gs_read(google_sheet)
# read data from single tab of sheet
gs_read(google_sheet, ws=tab_s[1])
gs_read_csv(google_sheet, ws=tab_s[1])
# or using dplyr pipes
google_sheet %>% gs_read(ws=tab_s[1])
# download data from sheet into file
gs_download(google_sheet, ws=tab_s[1],
      to="C:/Develop/R/lecture_slides/data/google_sheet.csv")
# open sheet in internet browser
gs_browse(google_sheet)
