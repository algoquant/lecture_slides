# create list of vectors
li_st <- lapply(1:3, function(x) sample(6))
# bind list elements into matrix - doesn't work
rbind(li_st)
# bind list elements into matrix - tedious
rbind(li_st[[1]], li_st[[2]], li_st[[3]])
# bind list elements into matrix - works!
do.call(rbind, li_st)
# create numeric list
li_st <- list(1, 2, 3, 4)
do.call(rbind, li_st)  # returns single column matrix
do.call(cbind, li_st)  # returns single row matrix
# recycling rule applied
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
  while (length(li_st) > 1) {
# index of odd list elements
    odd_index <- seq(from=1, to=length(li_st), by=2)
# bind odd and even elements, and divide li_st by half
    li_st <- lapply(odd_index, function(in_dex) {
if (in_dex==length(li_st)) return(li_st[[in_dex]])
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
# split into separate data frames by hand
set_osa <- iris[iris$Species=="setosa", ]
versi_color <- iris[iris$Species=="versicolor", ]
virgin_ica <- iris[iris$Species=="virginica", ]
dim(set_osa)
head(set_osa, 2)
# split iris into list based on Species
split_iris <- split(iris, iris$Species)
str(split_iris, max.level=1)
names(split_iris)
dim(split_iris$setosa)
head(split_iris$setosa, 2)
unique(mtcars$cyl)  # cyl has three unique values
# split mtcars data frame based on number of cylinders
split_cars <- split(mtcars, mtcars$cyl)
str(split_cars, max.level=1)
names(split_cars)
# mean mpg for each cylinder group
sapply(split_cars, function(x) mean(x$mpg))
# function aggregate() performs split-apply-combine
aggregate(formula=(mpg ~ cyl), data=mtcars, FUN=mean)
# aggregate() all columns
aggregate(x=mtcars, by=list(cyl=mtcars$cyl), FUN=mean)
# mean mpg for each cylinder group
tapply(X=mtcars$mpg, INDEX=mtcars$cyl, FUN=mean)
# using with() environment
with(mtcars,
     tapply(X=mpg, INDEX=cyl, FUN=mean))
# function sapply() instead of tapply()
with(mtcars,
     sapply(sort(unique(cyl)), function(x) {
       structure(mean(mpg[x==cyl]), names=x)
       }, USE.NAMES=TRUE))  # end with

# function by() instead of tapply()
with(mtcars,
     by(data=mpg, INDICES=cyl, FUN=mean))
# get several mpg stats for each cylinder group
data_cars <- sapply(split_cars,
      function(x) {
        c(mean=mean(x$mpg), max=max(x$mpg), min=min(x$mpg))
      }  # end anonymous function
      )  # end sapply
data_cars  # sapply produces a matrix
data_cars <- lapply(split_cars,  # now same using lapply
      function(x) {
        c(mean=mean(x$mpg), max=max(x$mpg), min=min(x$mpg))
      }  # end anonymous function
      )  # end sapply
is.list(data_cars)  # lapply produces a list
# do.call flattens list into a matrix
do.call(cbind, data_cars)
# ?options  # get info on global options
getOption("warn")  # global option for "warn"
options("warn")  # global option for "warn"
getOption("error")  # global option for "error"
sqrt_safe <- function(in_put) {
# returns its argument
  if (in_put<0) {
    warning("sqrt_safe: in_put is negative")
    NULL  # return NULL for negative argument
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
# function vali_date validates its arguments
vali_date <- function(in_put=NULL) {
# check if argument is valid and return double
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
# check if argument is valid and return double
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
# check if argument is valid and return double
  if (missing(in_put)) {
    stop("vali_date: in_put is missing")
  } else if (!is.numeric(in_put)) {
    cat("in_put=", in_put)
    stop("vali_date: in_put is not numeric")
  } else 2*in_put
}  # end vali_date
vali_date(3)
vali_date("a")
vali_date()
# print the call stack
traceback()
vali_date <- function(in_put) {
# check argument using long form '&&' operator
  stopifnot(!missing(in_put) &&
      is.numeric(in_put))
  2*in_put
}  # end vali_date
vali_date(3)
vali_date()
vali_date("a")
vali_date <- function(in_put) {
# check argument using logical '&' operator
  stopifnot(!missing(in_put) & is.numeric(in_put))
  2*in_put
}  # end vali_date
vali_date()
vali_date("a")
# sum_two() returns the sum of its two arguments
sum_two <- function(in_put1, in_put2) {  # even more robust
# check if at least one argument is not missing
  stopifnot(!missing(in_put1) &&
      !missing(in_put2))
# check if arguments are valid and return sum
  if (is.numeric(in_put1) &&
      is.numeric(in_put2)) {
    in_put1 + in_put2  # both valid
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
# flag "vali_date" for debugging
debug(vali_date)
# calling "vali_date" starts debugger
vali_date(3)
# unflag "vali_date" for debugging
undebug(vali_date)
vali_date <- function(in_put) {
  browser()  # pause and invoke browser
# check argument using long form '&&' operator
  stopifnot(!missing(in_put) &&
      is.numeric(in_put))
  2*in_put
}  # end vali_date
vali_date()  # invokes debugger
options("error")  # show default NULL "error" option
options(error=recover)  # set "error" option to "recover"
options(error=NULL)  # set back to default "error" option
str(tryCatch)  # get arguments of tryCatch()
tryCatch(  # without error handler
  {  # evaluate expressions
    num_var <- 101  # assign
    stop('my error')  # produce error
  },
  finally=print(paste("num_var=", num_var))
)  # end tryCatch

tryCatch(  # with error handler
  {  # evaluate expressions
    num_var <- 101  # assign
    stop('my error')  # produce error
  },
  # error handler captures error condition
  error=function(error_cond) {
    print(paste("error handler: ", error_cond))
  },  # end error handler
  # warning handler captures warning condition
  warning=function(warning_cond) {
    print(paste("warning handler: ", warning_cond))
  },  # end warning handler
  finally=print(paste("num_var=", num_var))
)  # end tryCatch
rm(list=ls())
# apply loop without tryCatch
apply(as.matrix(1:5), 1, function(num_var) {  # anonymous function
    stopifnot(num_var != 3)  # check for error
    # broadcast message to console
    cat("(cat) num_var =", num_var, "\n")
    # return a value
    paste("(return) num_var =", num_var)
  }  # end anonymous function
)  # end apply
# apply loop with tryCatch
apply(as.matrix(1:5), 1, function(num_var) {  # anonymous function
    tryCatch(  # with error handler
{  # body
  stopifnot(num_var != 3)  # check for error
  # broadcast message to console
  cat("(cat) num_var =", num_var, "\t")
  # return a value
  paste("(return) num_var =", num_var)
},
# error handler captures error condition
error=function(error_cond)
  paste("handler: ", error_cond),
finally=print(paste("(finally) num_var =", num_var))
    )  # end tryCatch
  }  # end anonymous function
)  # end apply
setwd("C:/Develop/R/lecture_slides/data")
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
setwd("C:/Develop/R/lecture_slides/data")
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
setwd("C:/Develop/R/lecture_slides/data")
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
setwd("C:/Develop/R/lecture_slides/data")
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
setwd("C:/Develop/R/lecture_slides/data")
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
setwd("C:/Develop/R/lecture_slides/data")
# write data frame to CSV file, without row names
write.csv(data_frame, row.names=FALSE, file="florist.csv")
data_read <- read.csv(file="florist.csv")
data_read  # a data frame without row names
setwd("C:/Develop/R/lecture_slides/data")
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
setwd("C:/Develop/R/lecture_slides/data")
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
setwd("C:/Develop/R/lecture_slides/data")
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
setwd("C:/Develop/R/lecture_slides/data")
rm(list=ls())
set.seed(1121)  # reset random number generator
library(zoo)  # load package zoo
# create zoo with Date index
in_dex <- seq(from=as.Date("2013-06-15"), 
            by="day", length.out=100)
zoo_series <- zoo(cumsum(rnorm(NROW(in_dex))), 
            order.by=in_dex)
tail(zoo_series, 3)
# write zoo to text file, and then read it back
write.zoo(zoo_series, file="zoo_series.txt")
zoo_series <- read.zoo("zoo_series.txt")  # read it back
tail(zoo_series, 3)
setwd("C:/Develop/R/lecture_slides/data")
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
setwd("C:/Develop/R/lecture_slides/data")
library(zoo)  # load package zoo
# write zoo to CSV file, and then read it back
write.zoo(zoo_series, file="zoo_series.csv", 
    sep=",", col.names=TRUE)
zoo_series <- read.zoo(file="zoo_series.csv", 
            header=TRUE, sep=",", 
            drop=FALSE, 
            FUN=as.POSIXct, tz="America/New_York")
tail(zoo_series, 3)
# read zoo from CSV file, with custom date-time format
zoo_frame <- read.table(file="zoo_series2.csv", 
                  sep=",")
tail(zoo_frame, 3)  # date-time format mm/dd/yyyy hh:mm
zoo_series <- read.zoo(file="zoo_series2.csv", 
            header=TRUE, sep=",", 
            drop=FALSE, 
            FUN=as.POSIXct, 
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
library(zoo)  # load package zoo
# show the generic function "merge"
merge
# show the "merge" method dispatched to "zoo" objects
merge.zoo
library(zoo)  # load package zoo
# get all methods for generic function merge()
methods(generic.function="merge")
# get generic function methods applied to "zoo" objects
methods(class="zoo")
# define a generic function
gen_sum <- function (a, b, ...) {
  UseMethod("gen_sum")
}  # end gen_sum

# define method for "numeric" class
gen_sum.numeric <- function (a, b, ...) {
  sum(a, b)
}  # end gen_sum.character

# define method for "character" class
gen_sum.character <- function (a, b, ...) {
  paste(a, "plus", b)
}  # end gen_sum.character

# apply gen_sum to "numeric" objects
gen_sum(1, 2)
# apply gen_sum to "character" objects
gen_sum("a", "b")
# 'cbind' is an internal generic function
cbind
# define "+" method for "character" class
"+.character" <- function (a, b, ...) {
  paste(a, "plus", b)
}  # end +.character
methods("+")  # view methods for "+" operator
# define variables with "character" class
char1 <- "a"
char2 <- "b"
class(char1)
char1 + char2  # add two "character" objects - doesn't work
attributes(char1)  # doesn't have explicit "character" class - only implicit
char1 <- structure("a", class="character")
char2 <- structure("b", class="character")
attributes(char1)  # now has explicit "character" class
# add two "character" objects
char1 + char2
# define object of class "string"
obj_string <- "how are you today?"
class(obj_string) <- "string"
obj_string
# overload "print" method for string objects
print.string <- function (str_ing) {
  print(
    paste(strsplit(str_ing, split=" ")[[1]],
  collapse=" + "))
}  # end print.string
# methods("print")  # view new methods for "print" function
print(obj_string)
obj_string
# overwrite "+" operator
"+" = function(a, b) {
  if (is.character(a) && is.character(b)) {
    paste(a, "plus", b)
  } else {
    .Primitive("+") (a, b)
  }
}
methods("+")  # view methods for "+" operator
# add two "numeric" objects
1 + 2
# add two "character" objects
"a" + "b"
# overwrite "+" operator with a generic function
"+" <- function (a, b, ...) {
  UseMethod("+")
}  # end gen_sum
# define method for "numeric" class
"+.numeric" <- function (a, b, ...) {
  sum(a, b)
}  # end gen_sum.character
# define method for "character" class
"+.character" <- function (a, b, ...) {
  paste(a, "plus", b)
}  # end gen_sum.character
methods("+")  # view methods for "+" operator
# add two "numeric" objects
1 + 2
# add two "character" objects
"a" + "b"
cbind.ts  # can't view non-visible method
stats::cbind.ts  # can't view non-visible method
stats:::cbind.ts  # display non-visible method
getAnywhere(cbind.ts)  # display non-visible method
rm(list=ls())
new_zoo <- zoo(rnorm(10), order.by=(Sys.Date() + 0:9))
# coerce "zoo" object to new class "zoo_xtra"
class(new_zoo) <- "zoo_xtra"
class(new_zoo)
methods(generic.function="length")
length  # primitive function
# define "length" method for class "zoo_xtra"
length.zoo_xtra <- function(in_ts) {
  cat("length of zoo_xtra object:\n")
# unclass object, then calculate length
  length(unclass(in_ts))
}  # end length.zoo_xtra
length(new_zoo)  # apply "length" method to "zoo_xtra" object
methods(generic.function="length")
# define "last" method for class "zoo_xtra"
last.zoo_xtra <- function(in_ts) {
  in_ts[length(in_ts)]
}  # end last.zoo_xtra
last(new_zoo)  # doesn't work
last.zoo_xtra(new_zoo)  # works
# define a generic function
last <- function (a, b, ...) {
  UseMethod("last")
}  # end last
last(new_zoo)  # now works
# define generic "string" class converter
as.string <- function (str_ing, ...)
  UseMethod("as.string")
# default "string" class converter
as.string.default <- function (str_ing, ...)
  structure(str_ing, class="string", ...)
# numeric "string" class converter
as.string.numeric <- function (str_ing, ...)
  structure(as.character(str_ing), class="string", ...)
# "string" class checker
is.string <- function (str_ing)
  inherits(x=str_ing, what="string")
# define "string" object
obj_string <- as.string("how are you today?")
obj_string
is.string(obj_string)
is.string("hello")
as.string(123)
is.string(as.string(123))
rm(list=ls())
library(xts)
new_xts <- xts(rnorm(10), order.by=(Sys.Date() + 0:9))
class(new_xts)  # class attribute is a vector
# "last" is a generic function from package "xts"
last
methods(generic.function="last")
last(new_xts)  # apply "last" method from "xts" class
# derive object "xts_xtra" from "xts" object
class(new_xts) <- c("xts_xtra", class(new_xts))
class(new_xts)  # class attribute is a vector
# "xts_xtra" object inherits "last" method from "xts" class
last(new_xts)
# define new "last" method for class "xts_xtra"
last.xts_xtra <- function(in_ts) {
  cat("last element of xts_xtra object:\n")
  drop(in_ts[length(in_ts), ])
}  # end last.xts_xtra
last(new_xts)  # apply "last" from "xts_xtra" class
# define "last" method for class "xts_xtra"
last.xts_xtra <- function(in_ts) {
  cat("last element of xts_xtra object:\n")
  drop(NextMethod())
}  # end last.xts_xtra
last(new_xts)  # apply "last" from "xts_xtra" class
