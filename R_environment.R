library(knitr)
opts_chunk$set(prompt=TRUE, eval=FALSE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=6, fig.height=5)
options(width=60, dev='pdf')
options(digits=3)
thm <- knit_theme$get("acid")
knit_theme$set(thm)
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
setwd("C:/Develop/data")
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
     file="C:/Develop/data/my_data.RData")
rm(list=ls())  # remove all objects
ls()  # list objects
load_ed <- load(file="C:/Develop/data/my_data.RData")
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
# options(max.print=5)
setwd("C:/Develop/data")
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
price_s <- do.call(merge,
  as.list(rutils::env_etf)[sym_bols])
# extract and merge adjusted prices, subset by sym_bols
price_s <- do.call(merge,
  lapply(as.list(rutils::env_etf)[sym_bols], Ad))
# same, but works only for OHLC series
price_s <- do.call(merge,
  eapply(rutils::env_etf, Ad)[sym_bols])
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
# copy price_s into env_etf and save to .RData file
assign("price_s", price_s, envir=env_etf)
save(env_etf, file='etf_data.RData')
# "trees" is in datasets base package
head(trees, 3)
colnames(trees)
mean(Girth)
mean(trees$Girth)
with(trees, 
     c(mean(Girth), mean(Height), mean(Volume)))
setwd("C:/Develop/data")
cat("Enter\ttab")  # cat() interprets backslash escape sequences
print("Enter\ttab")

my_text <- print("hello")
my_text  # print() returns its argument

# create string
my_text <- "Title: My Text\nSome numbers: 1,2,3,...\nRprofile files contain code executed at R startup,\n"

cat(my_text, file="mytext.txt")  # write to text file

cat("Title: My Text",  # write several lines to text file
    "Some numbers: 1,2,3,...",
    "Rprofile files contain code executed at R startup,", 
    file="mytext.txt", sep="\n")

save(my_text, file="mytext.RData")  # write to binary file
print(pi)
print(pi, digits=10)
getOption("digits")
foo <- 12
bar <- "months"
sprintf("There are %i %s in the year", foo, bar)
setwd("C:/Develop/data")
# read text from file
scan(file="mytext.txt", what=character(), sep="\n")

# read lines from file
readLines(con="mytext.txt")

# read text from console
in_put <- readline("Enter a number: ")
class(in_put)
# coerce to numeric
in_put <- as.numeric(in_put)

# read text from file and display in editor:
# file.show("mytext.txt")
# file.show("mytext.txt", pager="")
setwd("C:/Develop/data")
data_frame <- data.frame(type=c("rose", "daisy", "tulip"), color=c("red", "white", "yellow"), price=c(1.5, 0.5, 1.0), row.names=c("flower1", "flower2", "flower3"))  # end data.frame
mat_rix <- matrix(sample(1:12), ncol=3, dimnames=list(NULL, c("col1", "col2", "col3")))
rownames(mat_rix) <- paste("row", 1:NROW(mat_rix), sep="")
# write data frame to text file, and then read it back
write.table(data_frame, file="florist.txt")
data_read <- read.table(file="florist.txt")
data_read  # a data frame

# write matrix to text file, and then read it back
write.table(mat_rix, file="matrix.txt")
mat_read <- read.table(file="matrix.txt")
mat_read  # write.table() coerced matrix to data frame
class(mat_read)
# coerce from data frame back to matrix
mat_read <- as.matrix(mat_read)
class(mat_read)
setwd("C:/Develop/data")
data_frame <- data.frame(small=c(3, 5), medium=c(9, 11), large=c(15, 13))
data_frame <- read.table("mydata.txt", header=TRUE)
data_frame <- read.table("clipboard", header=TRUE)

write.table(x=data_frame, file="clipboard", sep="\t")

# wrapper function for copying data frame from clipboard into R
# by default, data is tab delimited, with a header
read_clip <- function(file="clipboard", sep="\t",
              header=TRUE, ...) {
  read.table(file=file, sep=sep, header=header, ...)
}  # end read_clip

data_frame <- read_clip()

# wrapper function for copying data frame from R into clipboard
# by default, data is tab delimited, with a header
write_clip <- function(data, row.names=FALSE,
               col.names=TRUE, ...) {
  write.table(x=data, file="clipboard", sep="\t",
      row.names=row.names, col.names=col.names, ...)
}  # end write_clip

write_clip(data=data_frame)

# launch spreadsheet-style data editor
data_frame <- edit(data_frame)
setwd("C:/Develop/data")
# write data frame to CSV file, and then read it back
write.csv(data_frame, file="florist.csv")
data_read <- read.csv(file="florist.csv", 
                 stringsAsFactors=FALSE)
data_read  # the row names are read in as extra column
# restore row names
rownames(data_read) <- data_read[, 1]
data_read <- data_read[, -1]  # remove extra column
data_read
# read data frame, with row names from first column
data_read <- read.csv(file="florist.csv", row.names=1)
data_read
setwd("C:/Develop/data")
# write data frame to CSV file, without row names
write.csv(data_frame, row.names=FALSE, file="florist.csv")
data_read <- read.csv(file="florist.csv")
data_read  # a data frame without row names
setwd("C:/Develop/data")
# write matrix to csv file, and then read it back
write.csv(mat_rix, file="matrix.csv")
mat_read <- read.csv(file="matrix.csv", row.names=1)
mat_read  # read.csv() reads matrix as data frame
class(mat_read)
mat_read <- as.matrix(mat_read)  # coerce to matrix
identical(mat_rix, mat_read)
write.csv(mat_rix, row.names=FALSE, 
    file="matrix_ex_rows.csv")
mat_read <- read.csv(file="matrix_ex_rows.csv")
mat_read <- as.matrix(mat_read)
mat_read  # a matrix without row names
setwd("C:/Develop/data")
library(MASS)  # load package "MASS"
# write to CSV file by row - it's very SLOW!!!
write.matrix(mat_rix, file="matrix.csv", sep=",")
system.time(  # scan reads faster - skip first line with colnames
  mat_read <- scan(file="matrix.csv", sep=",", 
            skip=1, what=numeric()))
col_names <- readLines(con="matrix.csv", n=1)  # read colnames
col_names  # this is a string!
col_names <- strsplit(col_names, s=",")[[1]]  # convert to char vector
mat_read  # mat_read is a vector, not matrix!
# coerce by row to matrix
mat_read <- matrix(mat_read, ncol=length(col_names), 
            byrow=TRUE)
colnames(mat_read) <- col_names  # restore colnames
mat_read
setwd("C:/Develop/data")
# read data from a csv file, including row names
mat_rix <- read.csv(file="matrix_bad.csv", row.names=1,
               stringsAsFactors=FALSE)
mat_rix
class(mat_rix)
# columns with bad data are character or factor
sapply(mat_rix, class)
row_names <- row.names(mat_rix)  # copy row names
# sapply loop over columns and coerce to numeric
mat_rix <- sapply(mat_rix, as.numeric)
row.names(mat_rix) <- row_names  # restore row names
# replace NAs with zero
mat_rix[is.na(mat_rix)] <- 0
# matrix without NAs
mat_rix
setwd("C:/Develop/data")
rm(list=ls())
set.seed(1121)  # reset random number generator
library(zoo)  # load package zoo
# create zoo with Date index
in_dex <- seq(from=as.Date("2013-06-15"), 
            by="day", length.out=100)
zoo_series <- zoo(cumsum(rnorm(length(in_dex))), 
            order.by=in_dex)
tail(zoo_series, 3)
# write zoo to text file, and then read it back
write.zoo(zoo_series, file="zoo_series.txt")
zoo_series <- read.zoo("zoo_series.txt")  # read it back
tail(zoo_series, 3)
setwd("C:/Develop/data")
rm(list=ls())
set.seed(1121)  # reset random number generator
library(zoo)  # load package zoo
# create zoo with POSIXct date-time index
in_dex <- seq(from=as.POSIXct("2013-06-15"), 
            by="hour", length.out=1000)
zoo_series <- zoo(cumsum(rnorm(length(in_dex))), 
            order.by=in_dex)
tail(zoo_series, 3)
# write zoo to text file, and then read it back
write.zoo(zoo_series, file="zoo_series.txt")
zoo_series <- read.zoo("zoo_series.txt")  # read it back
# time field was read as a separate column
tail(zoo_series, 3)
# read and specify that second column is time field
zoo_series <- read.zoo(file="zoo_series.txt", 
                 index.column=list(1,2), 
                 tz="America/New_York")
tail(zoo_series, 3)
setwd("C:/Develop/data")
library(zoo)  # load package zoo
# write zoo to CSV file, and then read it back
write.zoo(zoo_series, file="zoo_series.csv", sep=",")
zoo_series <- read.zoo(file="zoo_series.csv", 
            header=TRUE, sep=",", FUN=as.POSIXct,
            tz="America/New_York")
tail(zoo_series, 3)
# read zoo from CSV file, with custom date-time format
zoo_frame <- read.table(file="zoo_series2.csv", sep=",")
tail(zoo_frame, 3)  # date-time format mm/dd/yyyy hh:mm
zoo_series <- read.zoo(file="zoo_series2.csv", 
            header=TRUE, sep=",", FUN=as.POSIXct, 
            tz="America/New_York",
            format="%m/%d/%Y %H:%M")
tail(zoo_series, 3)
rm(list=ls())  # remove all objects
var1 <- 1; var2 <- 2
ls()  # list all objects
ls()[1]  # list first object
args(save)  # list arguments of save function
# save "var1" to a binary file using string argument
save("var1", file="my_data.RData")
# save "var1" to a binary file using object name
save(var1, file="my_data.RData")
# save multiple objects
save(var1, var2, file="my_data.RData")
# save first object in list by passing to "..." argument
# ls()[1] is not evaluated
save(ls()[1], file="my_data.RData")
# save first object in list by passing to "list" argument
save(list=ls()[1], file="my_data.RData")
# save whole list by passing it to the "list" argument
save(list=ls(), file="my_data.RData")
rm(list=ls())  # remove all objects
# load objects from file
load_ed <- load(file="my_data.RData")
load_ed  # vector of loaded objects
ls()  # list objects
# assign new values to objects in  global environment
sapply(load_ed, function(sym_bol) {
  assign(sym_bol, runif(1), envir=globalenv())
})  # end sapply
ls()  # list objects
# assign new values to objects using for loop
for (sym_bol in load_ed) {
  assign(sym_bol, runif(1))
}  # end for
ls()  # list objects
# save vector of objects
save(list=load_ed, file="my_data.RData")
# remove only loaded objects
rm(list=load_ed)
# remove the object "load_ed"
rm(load_ed)
sink("sinkdata.txt")# redirect text output to file

cat("Redirect text output from R\n")
print(runif(10))
cat("\nEnd data\nbye\n")

sink()  # turn redirect off

pdf("Rgraph.pdf", width=7, height=4)  # redirect graphics to pdf file

cat("Redirect data from R into pdf file\n")
my_var <- seq(-2*pi, 2*pi, len=100)
plot(x=my_var, y=sin(my_var), main="Sine wave",
   xlab="", ylab="", type="l", lwd=2, col="red")
cat("\nEnd data\nbye\n")

dev.off()  # turn pdf output off

png("r_plot.png")  # redirect graphics output to png file

cat("Redirect graphics from R into png file\n")
plot(x=my_var, y=sin(my_var), main="Sine wave",
 xlab="", ylab="", type="l", lwd=2, col="red")
cat("\nEnd data\nbye\n")

dev.off()  # turn png output off
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
gs_ws_ls(google_sheet)
# set curl options
library(httr)
httr::set_config(config(ssl_verifypeer=0L))
# read data from sheet
gs_read(google_sheet)
# read data from single tab of sheet
gs_read(google_sheet, ws="Sheet1")
gs_read_csv(google_sheet, ws="Sheet1")
# or using dplyr pipes
google_sheet %>% gs_read(ws="Sheet1")
# download data from sheet into file
gs_download(google_sheet, ws="Sheet1",
      to="C:/Develop/data/google_sheet.csv")
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
gs_ws_ls(google_sheet)
# set curl options
library(httr)
httr::set_config(config(ssl_verifypeer=0L))
# read data from sheet
gs_read(google_sheet)
# read data from single tab of sheet
gs_read(google_sheet, ws="Sheet1")
gs_read_csv(google_sheet, ws="Sheet1")
# or using dplyr pipes
google_sheet %>% gs_read(ws="Sheet1")
# download data from sheet into file
gs_download(google_sheet, ws="Sheet1",
      to="C:/Develop/data/google_sheet.csv")
# open sheet in internet browser
gs_browse(google_sheet)
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
setwd("C:/Develop/data")
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
setwd("C:/Develop/data")
# read data frame, with row names from first column
data_read <- read.csv(file="florist.csv",
              row.names=1)
# subset data frame
data_read <-
  data_read[data_read[, "type"]=="daisy", ]
# write data frame to CSV file, with row names
write.csv(data_read, file="daisies.csv")
# display documentation on function "getwd"
help(getwd)
?getwd  # equivalent to "help(getwd)"
help.start()  # open the hypertext documentation
