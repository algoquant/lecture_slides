cat("Enter\ttab")  # Cat() interprets backslash escape sequences
print("Enter\ttab")
my_text <- print("hello")
my_text  # Print() returns its argument
# Create string
my_text <- "Title: My Text\nSome numbers: 1,2,3,...\nRprofile files contain code executed at R startup,\n"
cat(my_text, file="mytext.txt")  # Write to text file
cat("Title: My Text",  # Write several lines to text file
    "Some numbers: 1,2,3,...",
    "Rprofile files contain code executed at R startup,",
    file="mytext.txt", sep="\n")
save(my_text, file="mytext.RData")  # Write to binary file
print(pi)
print(pi, digits=10)
getOption("digits")
foo <- 12
bar <- "months"
sprintf("There are %i %s in the year", foo, bar)
# Read text from file
scan(file="mytext.txt", what=character(), sep="\n")
# Read lines from file
readLines(con="mytext.txt")
# Read text from console
in_put <- readline("Enter a number: ")
class(in_put)
# Coerce to numeric
in_put <- as.numeric(in_put)
# Read text from file and display in editor:
# file.show("mytext.txt")
# file.show("mytext.txt", pager="")
setwd("/Users/jerzy/Develop/lecture_slides/data")
data_frame <- data.frame(type=c("rose", "daisy", "tulip"),
  color=c("red", "white", "yellow"),
  price=c(1.5, 0.5, 1.0),
  row.names=c("flower1", "flower2", "flower3"))  # end data.frame
mat_rix <- matrix(sample(1:12), ncol=3,
  dimnames=list(NULL, c("col1", "col2", "col3")))
rownames(mat_rix) <- paste("row", 1:NROW(mat_rix), sep="")
# Write data frame to text file, and then read it back
write.table(data_frame, file="florist.txt")
data_read <- read.table(file="florist.txt")
data_read  # A data frame
# Write matrix to text file, and then read it back
write.table(mat_rix, file="matrix.txt")
mat_read <- read.table(file="matrix.txt")
mat_read  # write.table() coerced matrix to data frame
class(mat_read)
# Coerce from data frame back to matrix
mat_read <- as.matrix(mat_read)
class(mat_read)
setwd("/Users/jerzy/Develop/lecture_slides/data")
data_frame <- data.frame(small=c(3, 5), medium=c(9, 11), large=c(15, 13))
data_frame <- read.table("mydata.txt", header=TRUE)
data_frame <- read.table("clipboard", header=TRUE)
write.table(x=data_frame, file="clipboard", sep="\t")
# Wrapper function for copying data frame from clipboard into R
# by default, data is tab delimited, with a header
read_clip <- function(file="clipboard", sep="\t", header=TRUE, ...) {
  read.table(file=file, sep=sep, header=header, ...)
}  # end read_clip
data_frame <- read_clip()
# Wrapper function for copying data frame from R into clipboard
# by default, data is tab delimited, with a header
write_clip <- function(data, row.names=FALSE, col.names=TRUE, ...) {
  write.table(x=data, file="clipboard", sep="\t",
      row.names=row.names, col.names=col.names, ...)
}  # end write_clip
write_clip(data=data_frame)
# Launch spreadsheet-style data editor
data_frame <- edit(data_frame)
# Write data frame to CSV file, and then read it back
write.csv(data_frame, file="florist.csv")
data_read <- read.csv(file="florist.csv")
data_read  # the row names are read in as extra column
# Restore row names
rownames(data_read) <- data_read[, 1]
data_read <- data_read[, -1]  # Remove extra column
data_read
# Read data frame, with row names from first column
data_read <- read.csv(file="florist.csv", row.names=1)
data_read
# Write data frame to CSV file, without row names
write.csv(data_frame, row.names=FALSE, file="florist.csv")
data_read <- read.csv(file="florist.csv")
data_read  # A data frame without row names
# Open a read connection to a file
con_read = file("/Users/jerzy/Develop/lecture_slides/data/etf_prices_crsp.csv", "r")
# Read the first 10 rows
data_10 <- read.csv(con_read, nrows=10)
# Read another 10 rows
data_20 <- read.csv(con_read, nrows=10, header=FALSE)
colnames(data_20) <- colnames(data_10)
# Close the connection to the file
close(con_read)
# Open a read connection to a file
con_read = file("/Users/jerzy/Develop/lecture_slides/data/etf_prices_crsp.csv", "r")
# Read the first 1000 rows
data_10 <- read.csv(con_read, nrows=1e3)
col_names <- colnames(data_10)
# Write to a file
coun_t <- 1
write.csv(data_10, paste0("C:/Develop/data/temp/etf_prices_", coun_t, ".csv"))
# Read remaining rows in a loop 10 rows at a time
# Can produce error without getting to end of file
while (isOpen(con_read)) {
  da_ta <- read.csv(con_read, nrows=1e3)
  colnames(da_ta) <- col_names
  write.csv(da_ta, paste0("C:/Develop/data/temp/etf_prices_", coun_t, ".csv"))
  coun_t <- coun_t + 1
}  # end while
# Write matrix to csv file, and then read it back
write.csv(mat_rix, file="matrix.csv")
mat_read <- read.csv(file="matrix.csv", row.names=1)
mat_read  # Read.csv() reads matrix as data frame
class(mat_read)
mat_read <- as.matrix(mat_read)  # Coerce to matrix
identical(mat_rix, mat_read)
write.csv(mat_rix, row.names=FALSE,
    file="matrix_ex_rows.csv")
mat_read <- read.csv(file="matrix_ex_rows.csv")
mat_read <- as.matrix(mat_read)
mat_read  # A matrix without row names
setwd("/Users/jerzy/Develop/lecture_slides/data")
library(MASS)  # Load package "MASS"
# Write to CSV file by row - it's very SLOW!!!
MASS::write.matrix(mat_rix, file="matrix.csv", sep=",")
# Read using scan() and skip first line with colnames
mat_read <- scan(file="matrix.csv", sep=",", skip=1,
  what=numeric())
# Read colnames
col_names <- readLines(con="matrix.csv", n=1)
col_names  # this is a string!
# Convert to char vector
col_names <- strsplit(col_names, split=",")[[1]]
mat_read  # mat_read is a vector, not matrix!
# Coerce by row to matrix
mat_read <- matrix(mat_read, ncol=NROW(col_names), byrow=TRUE)
# Restore colnames
colnames(mat_read) <- col_names
mat_read
# Scan() is a little faster than read.csv()
library(microbenchmark)
summary(microbenchmark(
  read_csv=read.csv("matrix.csv"),
  scan=scan(file="matrix.csv", sep=",",
    skip=1, what=numeric()),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Read data from a csv file, including row names
mat_rix <- read.csv(file="matrix_bad.csv", row.names=1)
mat_rix
class(mat_rix)
# Columns with bad data are character or factor
sapply(mat_rix, class)
# Copy row names
row_names <- row.names(mat_rix)
# sapply loop over columns and coerce to numeric
mat_rix <- sapply(mat_rix, as.numeric)
# Restore row names
row.names(mat_rix) <- row_names
# Replace NAs with zero
mat_rix[is.na(mat_rix)] <- 0
# matrix without NAs
mat_rix
setwd("/Users/jerzy/Develop/lecture_slides/data")
rm(list=ls())
set.seed(1121)  # Reset random number generator
library(zoo)  # Load package zoo
# Create zoo with Date index
date_s <- seq(from=as.Date("2013-06-15"), by="day",
        length.out=100)
zoo_series <- zoo(rnorm(NROW(date_s)), order.by=date_s)
head(zoo_series, 3)
# Write zoo series to text file, and then read it back
write.zoo(zoo_series, file="zoo_series.txt")
zoo_read <- read.zoo("zoo_series.txt")  # Read it back
all.equal(zoo_read, zoo_series)
# Perform the same using write.table() and read.table()
# First coerce zoo_series into data frame
data_frame <- as.data.frame(zoo_series)
data_frame <- cbind(date_s, data_frame)
# Write zoo_series to text file using write.table
write.table(data_frame, file="zoo_series.txt",
      row.names=FALSE, col.names=FALSE)
# Read data frame from file
zoo_read <- read.table(file="zoo_series.txt")
sapply(zoo_read, class)  # A data frame
# Coerce data frame into zoo_series
zoo_read <- zoo::zoo(
  drop(as.matrix(zoo_read[, -1])),
  order.by=as.Date(zoo_read[, 1]))
all.equal(zoo_read, zoo_series)
library(zoo)  # Load package zoo
# Write zoo series to CSV file, and then read it back
write.zoo(zoo_series, file="zoo_series.csv",
    sep=",", col.names=TRUE)
zoo_read <- read.zoo(file="zoo_series.csv",
  header=TRUE, sep=",", drop=FALSE)
all.equal(zoo_series, drop(zoo_read))
set.seed(1121)  # Reset random number generator
# Create zoo with POSIXct date-time index
date_s <- seq(from=as.POSIXct("2013-06-15"),
        by="hour", length.out=100)
zoo_series <- zoo(rnorm(NROW(date_s)), order.by=date_s)
head(zoo_series, 3)
# Write zoo series to CSV file, and then read it back
write.zoo(zoo_series, file="zoo_series.csv",
    sep=",", col.names=TRUE)
# Read from CSV file using read.csv.zoo()
zoo_read <- read.csv.zoo(file="zoo_series.csv")
all.equal(zoo_series, zoo_read)
# Coerce to xts series
x_ts <- xts::as.xts(zoo_read)
class(x_ts); head(x_ts, 3)
# Coerce zoo series into data frame with custom date format
data_frame <- as.data.frame(zoo_series)
data_frame <- cbind(format(date_s, "%m-%d-%Y %H:%M:%S"),
              data_frame)
head(data_frame, 3)
# Write zoo series to csv file using write.table
write.table(data_frame, file="zoo_series.csv",
      sep=",", row.names=FALSE, col.names=FALSE)
# Read from CSV file using read.csv.zoo()
zoo_read <- read.zoo(file="zoo_series.csv",
  header=FALSE, sep=",", FUN=as.POSIXct,
  format="%m-%d-%Y %H:%M:%S", tz="America/New_York")
# Or using read.csv.zoo()
zoo_read <- read.csv.zoo(file="zoo_series.csv", header=FALSE,
  format="%m-%d-%Y %H:%M:%S", tz="America/New_York")
head(zoo_read, 3)
all.equal(zoo_series, zoo_read)
# Read time series from CSV file, with numeric date-time
zoo_read <- read.table(file="/Users/jerzy/Develop/lecture_slides/data/es_ohlc.csv",
  header=TRUE, sep=",")
# A data frame
class(zoo_read)
sapply(zoo_read, class)
# Coerce data frame into xts series
zoo_read <- xts::xts(as.matrix(zoo_read[, -1]),
  order.by=as.POSIXct.numeric(zoo_read[, 1], tz="America/New_York",
                        origin="1970-01-01"))
# An xts series
class(zoo_read)
head(zoo_read, 3)
rm(list=ls())  # Remove all objects
var1 <- 1; var2 <- 2
ls()  # List all objects
ls()[1]  # List first object
args(save)  # List arguments of save function
# Save "var1" to a binary file using string argument
save("var1", file="my_data.RData")
# Save "var1" to a binary file using object name
save(var1, file="my_data.RData")
# Save multiple objects
save(var1, var2, file="my_data.RData")
# Save first object in list by passing to "..." argument
# ls()[1] is not evaluated
save(ls()[1], file="my_data.RData")
# Save first object in list by passing to "list" argument
save(list=ls()[1], file="my_data.RData")
# Save whole list by passing it to the "list" argument
save(list=ls(), file="my_data.RData")
rm(list=ls())  # Remove all objects
# Load objects from file
load_ed <- load(file="my_data.RData")
load_ed  # vector of loaded objects
ls()  # List objects
# Assign new values to objects in  global environment
sapply(load_ed, function(sym_bol) {
  assign(sym_bol, runif(1), envir=globalenv())
})  # end sapply
ls()  # List objects
# Assign new values to objects using for loop
for (sym_bol in load_ed) {
  assign(sym_bol, runif(1))
}  # end for
ls()  # List objects
# Save vector of objects
save(list=load_ed, file="my_data.RData")
# Remove only loaded objects
rm(list=load_ed)
# Remove the object "load_ed"
rm(load_ed)
sink("sinkdata.txt")# Redirect text output to file
cat("Redirect text output from R\n")
print(runif(10))
cat("\nEnd data\nbye\n")
sink()  # turn redirect off
("Rgraph.", width=7, height=4)  # Redirect graphics to  file
cat("Redirect data from R into  file\n")
my_var <- seq(-2*pi, 2*pi, len=100)
plot(x=my_var, y=sin(my_var), main="Sine wave",
   xlab="", ylab="", type="l", lwd=2, col="red")
cat("\nEnd data\nbye\n")
dev.off()  # turn  output off
png("r_plot.png")  # Redirect graphics output to png file
cat("Redirect graphics from R into png file\n")
plot(x=my_var, y=sin(my_var), main="Sine wave",
 xlab="", ylab="", type="l", lwd=2, col="red")
cat("\nEnd data\nbye\n")
dev.off()  # turn png output off
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
data_table <- data.table::data.table(
  col1=sample(7), col2=sample(7), col3=sample(7))
# Print data_table
class(data_table); data_table
# Column referenced without quotes
data_table[, col2]
# Row referenced without a following comma
data_table[2]
# Print option "datatable.print.nrows"
getOption("datatable.print.nrows")
options(datatable.print.nrows=10)
getOption("datatable.print.nrows")
# Number of rows in data_table
NROW(data_table)
# Or
data_table[, NROW(col1)]
# Or
data_table[, .N]
# microbenchmark speed of data.table syntax
library(microbenchmark)
summary(microbenchmark(
  dt=data_table[, .N],
  pure_r=NROW(data_table),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Read a data table from CSV file
dir_name <- "/Users/jerzy/Develop/lecture_slides/data/"
file_name <- file.path(dir_name, "weather_delays14.csv")
data_table <- data.table::fread(file_name)
class(data_table); dim(data_table)
data_table
# fread() reads the same data as read.csv()
all.equal(read.csv(file_name),
    setDF(data.table::fread(file_name)))
# fread() is much faster than read.csv()
library(microbenchmark)
summary(microbenchmark(
  pure_r=read.csv(file_name),
  fread=setDF(data.table::fread(file_name)),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Write data table to file in different ways
data.table::fwrite(data_table, file="data_table.csv")
write.csv(data_table, file="data_table2.csv")
cat(unlist(data_table), file="data_table3.csv")
# microbenchmark speed of data.table::fwrite()
summary(microbenchmark(
  fwrite=data.table::fwrite(data_table, file="data_table.csv"),
  write_csv=write.csv(data_table, file="data_table2.csv"),
  cat=cat(unlist(data_table), file="data_table3.csv"),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Select first five rows of data_table
data_table[1:5]
# Select rows with JFK flights
jfk_flights <- data_table[origin=="JFK"]
# Select rows JFK flights in June
jfk_flights <- data_table[origin=="JFK" & month==6]
# Select rows without JFK flights
jfk_flights <- data_table[!(origin=="JFK")]
# Select flights with carrier_delay
data_table[carrier_delay > 0]
# Select column of data_table and return a vector
head(data_table[, origin])
# Select column of data_table and return a data_table, not vector
head(data_table[, list(origin)])
head(data_table[, .(origin)])
# Select two columns of data_table
data_table[, list(origin, month)]
data_table[, .(origin, month)]
column_s <- c("origin", "month")
data_table[, ..column_s]
data_table[, month, origin]
# Select two columns and rename them
data_table[, .(orig=origin, mon=month)]
# Select all columns except origin
head(data_table[, !"origin"])
head(data_table[, -"origin"])
# Select flights with positive carrier_delay
data_table[carrier_delay > 0]
# Number of flights with carrier_delay
data_table[, sum(carrier_delay > 0)]
# Or standard R commands
sum(data_table[, carrier_delay > 0])
# microbenchmark speed of data.table syntax
summary(microbenchmark(
  dt=data_table[, sum(carrier_delay > 0)],
  pure_r=sum(data_table[, carrier_delay > 0]),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Average carrier_delay
data_table[, mean(carrier_delay)]
mean(data_table[, carrier_delay])
# Average carrier_delay and aircraft_delay
data_table[, .(carrier=mean(carrier_delay),
         aircraft=mean(aircraft_delay))]
# Average aircraft_delay from JFK
data_table[origin=="JFK", mean(aircraft_delay)]
# Number of flights from JFK
data_table[origin=="JFK", NROW(aircraft_delay)]
# Or
data_table[origin=="JFK", .N]
# Number of flights from each airport
data_table[, .N, by=origin]
# Same, but add names to output
data_table[, .(flights=.N), by=.(airport=origin)]
# Number of AA flights from each airport
data_table[carrier=="AA", .(flights=.N),
     by=.(airport=origin)]
# Number of flights from each airport and airline
data_table[, .(flights=.N),
     by=.(airport=origin, airline=carrier)]
# Average aircraft_delay
data_table[, mean(aircraft_delay)]
# Average aircraft_delay from JFK
data_table[origin=="JFK", mean(aircraft_delay)]
# Average aircraft_delay from each airport
data_table[, .(delay=mean(aircraft_delay)),
     by=.(airport=origin)]
# Average and max delays from each airport and month
data_table[, .(mean_delay=mean(aircraft_delay), max_delay=max(aircraft_delay)),
     by=.(airport=origin, month=month)]
# Average and max delays from each airport and month
data_table[, .(mean_delay=mean(aircraft_delay), max_delay=max(aircraft_delay)),
     keyby=.(airport=origin, month=month)]
# Sort ascending by origin, then descending by dest
order_table <- data_table[order(origin, -dest)]
order_table
# Doesn't work outside data_table
order(origin, -dest)
# Sort data_table by reference
setorder(data_table, origin, -dest)
all.equal(data_table, order_table)
# setorder() is much faster than order()
summary(microbenchmark(
  order=data_table[order(origin, -dest)],
  setorder=setorder(data_table, origin, -dest),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Average aircraft_delay by month
order_table[, .(mean_delay=mean(aircraft_delay)),
      by=.(month=month)]
# Chained brackets to sort output by month
order_table[, .(mean_delay=mean(aircraft_delay)),
      by=.(month=month)][order(month)]
# Select weather_delay and aircraft_delay in two different ways
data_table[1:7, .SD,
     .SDcols=c("weather_delay", "aircraft_delay")]
data_table[1:7, .(weather_delay, aircraft_delay)]
# Calculate mean of weather_delay and aircraft_delay
data_table[, sapply(.SD, mean),
     .SDcols=c("weather_delay", "aircraft_delay")]
sapply(data_table[, .SD,
     .SDcols=c("weather_delay", "aircraft_delay")], mean)
# Return origin and dest, then all other columns
data_table[1:7, .SD, by=.(origin, dest)]
# Return origin and dest, then weather_delay and aircraft_delay columns
data_table[1:7, .SD,
     by=.(origin, dest),
     .SDcols="weather_delay", "aircraft_delay"]
# Return first two rows from each month
data_table[, head(.SD, 2), by=.(month)]
data_table[, head(.SD, 2), by=.(month),
     .SDcols=c("weather_delay", "aircraft_delay")]
# Calculate mean of weather_delay and aircraft_delay, grouped by origin
data_table[, lapply(.SD, mean),
     by=.(origin),
     .SDcols=c("weather_delay", "aircraft_delay")]
# Or simply
data_table[, .(weather_delay=mean(weather_delay),
         aircraft_delay=mean(aircraft_delay)),
     by=.(origin)]
# Add tot_delay column
data_table[, tot_delay := (carrier_delay + aircraft_delay)]
head(data_table, 4)
# Delete tot_delay column
data_table[, tot_delay := NULL]
# Add max_delay column grouped by origin and dest
data_table[, max_delay := max(aircraft_delay),
     by=.(origin, dest)]
data_table[, max_delay := NULL]
# Add date and tot_delay columns
data_table[, c("date", "tot_delay") :=
       list(paste(month, day, year, sep="/"),
            (carrier_delay + aircraft_delay))]
# Modify select rows of tot_delay column
data_table[month == 12, tot_delay := carrier_delay]
data_table[, c("date", "tot_delay") := NULL]
# Add several columns
data_table[, c("max_carrier", "max_aircraft") :=
       lapply(.SD, max),
     by=.(origin, dest),
     .SDcols=c("carrier_delay", "aircraft_delay")]
data_table[, c("max_carrier", "max_aircraft") := NULL]
# Modifying by reference is much faster than standard R
summary(microbenchmark(
  dt=data_table[, tot_delay := (carrier_delay + aircraft_delay)],
  pure_r=(data_table[, "tot_delay"] <- data_table[, "carrier_delay"] + data_table[, "aircraft_delay"]),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Add a key based on the "origin" column
setkey(data_table, origin)
haskey(data_table)
key(data_table)
# Select rows with LGA using the key
data_table["LGA"]
all.equal(data_table["LGA"],
    data_table[origin == "LGA"])
# Select rows with LGA and JFK using the key
data_table[c("LGA", "JFK")]
# Add a key based on the "origin" and "dest" columns
setkey(data_table, origin, dest)
key(data_table)
# Select rows with origin from JFK and MIA
data_table[c("JFK", "MIA")]
# Select rows with origin from JFK and dest to MIA
data_table[.("JFK", "MIA")]
all.equal(data_table[.("JFK", "MIA")],
    data_table[origin == "JFK" & dest == "MIA"])
# Selecting rows using a key is much faster than standard R
summary(microbenchmark(
  with_key=data_table[.("JFK", "MIA")],
  standard_r=data_table[origin == "JFK" & dest == "MIA"],
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Create data frame and coerce it to data table
data_table <- data.frame(
  col1=sample(7), col2=sample(7), col3=sample(7))
class(data_table); data_table
data.table::setDT(data_table)
class(data_table); data_table
# Coerce data_table into data frame
data.table::setDF(data_table)
class(data_table); data_table
# Or
data_table <- data.table:::as.data.frame.data.table(data_table)
# SetDF() is much faster than as.data.frame()
summary(microbenchmark(
  as.data.frame=data.table:::as.data.frame.data.table(data_table),
  setDF=data.table::setDF(data_table),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Coerce xts to a data frame
price_s <- rutils::etf_env$VTI
class(price_s); head(price_s)
price_s <- as.data.frame(price_s)
class(price_s); head(price_s)
# Coerce data frame to a data table
data.table::setDT(price_s, keep.rownames=TRUE)
class(price_s); head(price_s)
# Dates are coerced to strings
sapply(price_s, class)
# Coerce xts directly to a data table
data_table <- as.data.table(rutils::etf_env$VTI,
  keep.rownames=TRUE)
class(data_table); head(data_table)
# Dates are not coerced to strings
sapply(data_table, class)
all.equal(price_s, data_table, check.attributes=FALSE)
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
dir_name <- "/Users/jerzy/Develop/lecture_slides/data/"
file_name <- file.path(dir_name, "weather_delays14.csv")
data.table::setDF(data_frame)
class(data_frame); dim(data_frame)
# Write data frame to .fst file in different ways
fst::write_fst(data_frame, path="data_frame.fst")
write.csv(data_frame, file="data_frame2.csv")
# microbenchmark speed of fst::write_fst()
library(microbenchmark)
summary(microbenchmark(
  fst=fst::write_fst(data_frame, path="data_frame.csv"),
  write_csv=write.csv(data_frame, file="data_frame2.csv"),
  cat=cat(unlist(data_frame), file="data_frame3.csv"),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# fst::read_fst() reads the same data as read.csv()
all.equal(read.csv(file_name),
    fst::read_fst("data_frame.fst"))
# fst::read_fst() is 10 times faster than read.csv()
summary(microbenchmark(
  fst=fst::read_fst("data_frame.fst"),
  read_csv=read.csv(file_name),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Coerce TAQ xts to a data frame
library(HighFreq)
t_aq <- HighFreq::SPY_TAQ
t_aq <- as.data.frame(t_aq)
class(t_aq)
# Coerce data frame to a data table
data.table::setDT(t_aq, keep.rownames=TRUE)
class(t_aq); head(t_aq)
# Get memory size of data table
format(object.size(t_aq), units="MB")
# Save data table to .fst file
fst::write_fst(t_aq, path="C:/Develop/data/taq.fst")
# Create reference to .fst file similar to a data frame
fs_t <- fst::fst("C:/Develop/data/taq.fst")
class(fs_t)
# Memory size of reference to .fst is very small
format(object.size(fs_t), units="MB")
# Get sizes of all objects in workspace
sort(sapply(mget(ls()), object.size))
# Reference to .fst can be treated similar to a data table
dim(t_aq); dim(fs_t)
fst:::print.fst_table(fs_t)
# Subset reference to .fst just like a data table
fs_t[1e4:(1e4+5), ]
# Verify that rtools are working properly:
devtools::find_rtools()
devtools::has_devel()
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
int x_size = x.size();
int y_size = y.size();
if (x_size != y_size) {
    return 0;
  } else {
    double total = 0;
    for(int i = 0; i < x_size; ++i) {
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
double inner_mult_sugar(NumericVector x, NumericVector y) {
  return sum(x * y);
}")  # end cppFunction
# Run Rcpp Sugar function
inner_mult_sugar(1:3, 6:4)
inner_mult_sugar(1:3, 6:3)
# Define R function with loop
inner_mult_r <- function(x, y) {
    to_tal <- 0
    for(i in 1:NROW(x)) {
to_tal <- to_tal + x[i] * y[i]
    }
    to_tal
}  # end inner_mult_r
# Run R function
inner_mult_r(1:3, 6:4)
inner_mult_r(1:3, 6:3)
# Compare speed of Rcpp and R
library(microbenchmark)
summary(microbenchmark(
  pure_r=inner_mult_r(1:10000, 1:10000),
  inner_r=1:10000 %*% 1:10000,
  r_cpp=inner_mult(1:10000, 1:10000),
  r_cpp_sugar=inner_mult_sugar(1:10000, 1:10000),
  times=10))[, c(1, 4, 5)]
# Define Ornstein-Uhlenbeck function in R
sim_ou <- function(n_rows=1000, eq_price=5.0,
              vol_at=0.01, theta=0.01) {
  re_turns <- numeric(n_rows)
  price_s <- numeric(n_rows)
  price_s[1] <- eq_price
  for (i in 2:n_rows) {
    re_turns[i] <- the_ta*(eq_price - price_s[i-1]) + vol_at*rnorm(1)
    price_s[i] <- price_s[i-1] + re_turns[i]
  }  # end for
  price_s
}  # end sim_ou
# Simulate Ornstein-Uhlenbeck process in R
eq_price <- 5.0; sig_ma <- 0.01
the_ta <- 0.01; n_rows <- 1000
set.seed(1121)  # Reset random numbers
ou_sim <- sim_ou(n_rows=n_rows, eq_price=eq_price, vol_at=sig_ma, theta=the_ta)
# Define Ornstein-Uhlenbeck function in Rcpp
Rcpp::cppFunction("
NumericVector sim_ou_rcpp(double eq_price,
                double vol_at,
                double the_ta,
                NumericVector in_nov) {
  int n_rows = in_nov.size();
  NumericVector price_s(n_rows);
  NumericVector re_turns(n_rows);
  price_s[0] = eq_price;
  for (int it = 1; it < n_rows; it++) {
    re_turns[it] = the_ta*(eq_price - price_s[it-1]) + vol_at*in_nov[it-1];
    price_s[it] = price_s[it-1] + re_turns[it];
  }  // end for
  return price_s;
}")  # end cppFunction
# Simulate Ornstein-Uhlenbeck process in Rcpp
set.seed(1121)  # Reset random numbers
ou_sim_rcpp <- sim_ou_rcpp(eq_price=eq_price,
  vol_at=sig_ma,
  the_ta=the_ta,
  in_nov=rnorm(n_rows))
all.equal(ou_sim, ou_sim_rcpp)
# Compare speed of Rcpp and R
library(microbenchmark)
summary(microbenchmark(
  pure_r=sim_ou(n_rows=n_rows, eq_price=eq_price, vol_at=sig_ma, theta=the_ta),
  r_cpp=sim_ou_rcpp(eq_price=eq_price, vol_at=sig_ma, the_ta=the_ta, in_nov=rnorm(n_rows)),
  times=10))[, c(1, 4, 5)]
# Source Rcpp function for Ornstein-Uhlenbeck process from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/sim_ou.cpp")
# Simulate Ornstein-Uhlenbeck process in Rcpp
set.seed(1121)  # Reset random numbers
ou_sim_rcpp <- sim_ou_rcpp(eq_price=eq_price,
  vol_at=sig_ma,
  theta=the_ta,
  innov=rnorm(n_rows))
all.equal(ou_sim, ou_sim_rcpp)
# Compare speed of Rcpp and R
library(microbenchmark)
summary(microbenchmark(
  pure_r=sim_ou(n_rows=n_rows, eq_price=eq_price, vol_at=sig_ma, theta=the_ta),
  r_cpp=sim_ou_rcpp(eq_price=eq_price, vol_at=sig_ma, theta=the_ta, innov=rnorm(n_rows)),
  times=10))[, c(1, 4, 5)]
# Calculate uniformly distributed pseudo-random sequence
uni_form <- function(see_d, n_rows=10) {
  out_put <- numeric(n_rows)
  out_put[1] <- see_d
  for (i in 2:n_rows) {
    out_put[i] <- 4*out_put[i-1]*(1-out_put[i-1])
  }  # end for
  acos(1-2*out_put)/pi
}  # end uni_form
# Source Rcpp functions from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/uni_form.cpp")
# Microbenchmark Rcpp code
library(microbenchmark)
summary(microbenchmark(
  pure_r=runif(1e5),
  r_loop=uni_form(0.3, 1e5),
  r_cpp=uniform_rcpp(0.3, 1e5),
  times=10))[, c(1, 4, 5)]
library(RcppArmadillo)
# Source Rcpp functions from file
Rcpp::sourceCpp(file="C:/Develop/R/Rcpp/armadillo_functions.cpp")
vec1 <- runif(1e5)
vec2 <- runif(1e5)
vec_in(vec1, vec2)
vec1 %*% vec2
# Microbenchmark RcppArmadillo code
summary(microbenchmark(
  vec_in = vec_prod(vec1, vec2),
  r_code = (vec1 %*% vec2),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
# Microbenchmark shows:
# vec_in() is several times faster than %*%, especially for longer vectors.
#     expr     mean   median
# 1 vec_in 110.7067 110.4530
# 2 r_code 585.5127 591.3575
# Source Rcpp functions from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/sim_arima.cpp")
# Define AR(2) coefficients
co_eff <- c(0.9, 0.09)
n_rows <- 1e4
set.seed(1121)
in_nov <- rnorm(n_rows)
# Simulate ARIMA using filter()
arima_filter <- filter(x=in_nov,
  filter=co_eff, method="recursive")
# Simulate ARIMA using sim_arima()
ari_ma <- sim_arima(in_nov, rev(co_eff))
all.equal(drop(ari_ma),
  as.numeric(arima_filter))
# Microbenchmark RcppArmadillo code
summary(microbenchmark(
  sim_arima = sim_arima(in_nov, rev(co_eff)),
  filter = filter(x=in_nov, filter=co_eff, method="recursive"),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
library(RcppArmadillo)
# Source Rcpp functions from file
Rcpp::sourceCpp(file="C:/Develop/R/Rcpp/armadillo_functions.cpp")
mat_rix <- matrix(runif(1e5), nc=1e3)
# De-mean using apply()
new_mat <- apply(mat_rix, 2,
  function(x) (x-mean(x)))
# De-mean using demean_mat()
demean_mat(mat_rix)
all.equal(new_mat, mat_rix)
# Microbenchmark RcppArmadillo code
summary(microbenchmark(
  apply = (apply(mat_rix, 2, mean)),
  demean_mat = demean_mat(mat_rix),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
# Microbenchmark shows:
# Demean_mat() is over 70 times faster than apply()
#         expr       mean   median
# 1 demean_mat   127.7539  125.604
# 2      apply 10781.7534 9291.674
# Perform matrix inversion
# Create random positive semi-definite matrix
mat_rix <- matrix(runif(25), nc=5)
mat_rix <- t(mat_rix) %*% mat_rix
# Invert the matrix
matrix_inv <- solve(mat_rix)
inv_mat(mat_rix)
all.equal(inv_mat, mat_rix)
# Microbenchmark RcppArmadillo code
library(microbenchmark)
summary(microbenchmark(
  solve = solve(mat_rix),
  inv_mat = inv_mat(mat_rix),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
# Microbenchmark shows:
# inv_mat() is over 10 times faster than solve()
#      expr     mean median
# 1 inv_mat  3.42669  2.933
# 2 solve   32.00254 31.280
library(RcppArmadillo)
# Source Rcpp functions from file
Rcpp::sourceCpp("/Users/jerzy/Develop/lecture_slides/scripts/calc_weights.cpp")
# Calculate matrix of random returns
mat_rix <- matrix(rnorm(300), nc=5)
# Regularized inverse of covariance matrix
eigen_max <- 4
ei_gen <- eigen(cov(mat_rix))
cov_inv <- ei_gen$vectors[, 1:eigen_max] %*%
  (t(ei_gen$vectors[, 1:eigen_max]) / ei_gen$values[1:eigen_max])
# Regularized inverse using RcppArmadillo
cov_inv_arma <- calc_inv(mat_rix, eigen_max)
all.equal(cov_inv, cov_inv_arma)
# Microbenchmark RcppArmadillo code
library(microbenchmark)
summary(microbenchmark(
  Rcode = {
    ei_gen <- eigen(cov(mat_rix))
    ei_gen$vectors[, 1:eigen_max] %*%
(t(ei_gen$vectors[, 1:eigen_max]) / ei_gen$values[1:eigen_max])
  },
  Rcpp = calc_inv(mat_rix, eigen_max),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
Sys.Date()  # Get today's date
as.Date(1e3)  # Coerce numeric into date object
date_time <- as.Date("2014-07-14")  # "%Y-%m-%d" or "%Y/%m/%d"
date_time
class(date_time)  # Date object
as.Date("07-14-2014", "%m-%d-%Y")  # Specify format
date_time + 20  # Add 20 days
# Extract internal representation to integer
as.numeric(date_time)
date_old <- as.Date("07/14/2013", "%m/%d/%Y")
date_old
# Difference between dates
difftime(date_time, date_old, units="weeks")
weekdays(date_time)  # Get day of the week
# Coerce numeric into date-times
date_time <- 0
attributes(date_time) <- list(class="Date")
date_time  # "Date" object
structure(0, class="Date")  # "Date" object
structure(10000.25, class="Date")
date_time <- Sys.time()  # Get today's date and time
date_time
class(date_time)  # POSIXct object
# POSIXct stored as integer moment of time
as.numeric(date_time)
# Parse character string "%Y-%m-%d %H:%M:%S" to POSIXct object
date_time <- as.POSIXct("2014-07-14 13:30:10")
# Different time zones can have same clock time
as.POSIXct("2014-07-14 13:30:10", tz="America/New_York")
as.POSIXct("2014-07-14 13:30:10", tz="UTC")
# Format argument allows parsing different date-time string formats
as.POSIXct("07/14/2014 13:30:10", format="%m/%d/%Y %H:%M:%S",
     tz="America/New_York")
# Same moment of time corresponds to different clock times
time_ny <- as.POSIXct("2014-07-14 13:30:10",
     tz="America/New_York")
time_ldn <- as.POSIXct("2014-07-14 13:30:10",
     tz="UTC")
# Add five hours to POSIXct
time_ny + 5*60*60
# Subtract POSIXct
time_ny - time_ldn
class(time_ny - time_ldn)
# Compare POSIXct
time_ny > time_ldn
# Create vector of POSIXct times during trading hours
trading_times <- seq(
  from=as.POSIXct("2014-07-14 09:30:00", tz="America/New_York"),
  to=as.POSIXct("2014-07-14 16:00:00", tz="America/New_York"),
  by="10 min")
head(trading_times, 3)
tail(trading_times, 3)
# POSIXct is stored as integer moment of time
int_time <- as.numeric(date_time)
# Same moment of time corresponds to different clock times
as.POSIXct(int_time, origin="1970-01-01",
     tz="America/New_York")
as.POSIXct(int_time, origin="1970-01-01",
     tz="UTC")
# Same clock time corresponds to different moments of time
as.POSIXct("2014-07-14 13:30:10",
     tz="America/New_York") -
  as.POSIXct("2014-07-14 13:30:10", tz="UTC")
# Add 20 seconds to POSIXct
date_time + 20
date_time  # POSIXct date and time
# Parse POSIXct to string representing the clock time
format(date_time)
class(format(date_time))  # Character string
# Get clock times in different time zones
format(date_time, tz="America/New_York")
format(date_time, tz="UTC")
# Format with custom format strings
format(date_time, "%m/%Y")
format(date_time, "%m-%d-%Y %H hours")
# Trunc to hour
format(date_time, "%m-%d-%Y %H:00:00")
# Date converted to midnight UTC moment of time
as.POSIXct(Sys.Date())
as.POSIXct(as.numeric(as.POSIXct(Sys.Date())),
     origin="1970-01-01",
     tz="UTC")
# Parse character string "%Y-%m-%d %H:%M:%S" to POSIXlt object
date_time <- as.POSIXlt("2014-07-14 18:30:10")
date_time
class(date_time)  # POSIXlt object
as.POSIXct(date_time)  # Coerce to POSIXct object
# Extract internal list representation to vector
unlist(date_time)
date_time + 20  # Add 20 seconds
class(date_time + 20)  # Implicit coercion to POSIXct
trunc(date_time, units="hours")  # Truncate to closest hour
trunc(date_time, units="days")  # Truncate to closest day
methods(trunc)  # Trunc methods
trunc.POSIXt
Sys.timezone()  # Get time-zone
Sys.setenv(TZ="UTC")  # Set time-zone to UTC
Sys.timezone()  # Get time-zone
# Standard Time in effect
as.POSIXct("2013-03-09 11:00:00", tz="America/New_York")
# Daylight Savings Time in effect
as.POSIXct("2013-03-10 11:00:00", tz="America/New_York")
date_time <- Sys.time()  # Today's date and time
# Convert to character in different TZ
format(date_time, tz="America/New_York")
format(date_time, tz="UTC")
# Parse back to POSIXct
as.POSIXct(format(date_time, tz="America/New_York"))
# Difference between New_York time and UTC
as.POSIXct(format(Sys.time(), tz="UTC")) -
  as.POSIXct(format(Sys.time(), tz="America/New_York"))
# Set time-zone to New York
Sys.setenv(TZ="America/New_York")
library(lubridate)  # Load lubridate
# Parse strings into date-times
as.POSIXct("07-14-2014", format="%m-%d-%Y", tz="America/New_York")
date_time <- lubridate::mdy("07-14-2014", tz="America/New_York")
date_time
class(date_time)  # POSIXct object
lubridate::dmy("14.07.2014", tz="America/New_York")
# Parse numeric into date-times
as.POSIXct(as.character(14072014), format="%d%m%Y",
                  tz="America/New_York")
lubridate::dmy(14072014, tz="America/New_York")
# Parse decimal to date-times
lubridate::decimal_date(date_time)
lubridate::date_decimal(2014.25, tz="America/New_York")
date_decimal(decimal_date(date_time), tz="America/New_York")
library(lubridate)  # Load lubridate
date_time <- lubridate::ymd_hms(20140714142010,
               tz="America/New_York")
date_time
# Get same moment of time in "UTC" time zone
lubridate::with_tz(date_time, "UTC")
as.POSIXct(format(date_time, tz="UTC"), tz="UTC")
# Get same clock time in "UTC" time zone
lubridate::force_tz(date_time, "UTC")
as.POSIXct(format(date_time, tz="America/New_York"),
     tz="UTC")
# Same moment of time
date_time - with_tz(date_time, "UTC")
# Different moments of time
date_time - force_tz(date_time, "UTC")
library(lubridate)  # Load lubridate
# Daylight Savings Time handling periods vs durations
date_time <- as.POSIXct("2013-03-09 11:00:00",
                  tz="America/New_York")
date_time
date_time + lubridate::ddays(1)  # Add duration
date_time + lubridate::days(1)  # Add period
leap_year(2012)  # Leap year
date_time <- lubridate::dmy(01012012, tz="America/New_York")
date_time
date_time + lubridate::dyears(1)  # Add duration
date_time + lubridate::years(1)  # Add period
library(lubridate)  # Load lubridate
date_time <- lubridate::ymd_hms(20140714142010, tz="America/New_York")
date_time
# Add periods to a date-time
c(date_time + lubridate::seconds(1), date_time + lubridate::minutes(1),
  date_time + lubridate::days(1), date_time + lubridate::months(1))
# Create vectors of dates
date_time <- lubridate::ymd(20140714, tz="America/New_York")
date_time + 0:2 * lubridate::months(1)  # Monthly dates
date_time + lubridate::months(0:2)
date_time + 0:2 * lubridate::months(2)  # bi-monthly dates
date_time + seq(0, 5, by=2) * lubridate::months(1)
seq(date_time, length=3, by="2 months")
library(lubridate)  # Load lubridate
# Adding monthly periods can create invalid dates
date_time <- lubridate::ymd(20120131, tz="America/New_York")
date_time + 0:2 * lubridate::months(1)
date_time + lubridate::months(1)
date_time + lubridate::months(2)
# Create vector of end-of-month dates
date_time %m-% lubridate::months(13:1)
library(zoo)  # Load zoo
library(RQuantLib)  # Load RQuantLib
# Create daily date series of class "Date"
date_s <- Sys.Date() + -5:2
date_s
# Create Boolean vector of business days
is_busday <- isBusinessDay(  # RQuantLib calendar
  calendar="UnitedStates/GovernmentBond", date_s)
# Create daily series of business days
bus_index <- date_s[is_busday]
bus_index
library(zoo)  # Load package zoo
date_time <- Sys.Date()  # Create date series of class "Date"
date_s <- date_time + 0:365  # Daily series over one year
head(date_s, 4)  # Print first few dates
format(head(date_s, 4), "%m/%d/%Y")  # Print first few dates
# Create daily date-time series of class "POSIXct"
date_s <- seq(Sys.time(), by="days", length.out=365)
head(date_s, 4)  # Print first few dates
format(head(date_s, 4), "%m/%d/%Y %H:%M:%S")  # Print first few dates
# Create series of monthly dates of class "zoo"
monthly_index <- yearmon(2010+0:36/12)
head(monthly_index, 4)  # Print first few dates
# Create series of quarterly dates of class "zoo"
qrtly_index <- yearqtr(2010+0:16/4)
head(qrtly_index, 4)  # Print first few dates
# Parse quarterly "zoo" dates to POSIXct
Sys.setenv(TZ="UTC")
as.POSIXct(head(qrtly_index, 4))
library(lubridate)  # Load lubridate
set.seed(1121)  # Reset random number generator
# Create daily time series ending today
start_date <- decimal_date(Sys.Date()-6)
end_date <- decimal_date(Sys.Date())
# Create vector of geometric Brownian motion
da_ta <- exp(cumsum(rnorm(6)/100))
fre_quency <- NROW(da_ta)/(end_date-start_date)
ts_series <- ts(data=da_ta,
          start=start_date,
          frequency=fre_quency)
ts_series  # Display time series
# Display index dates
as.Date(date_decimal(zoo::coredata(time(ts_series))))
# bi-monthly geometric Brownian motion starting mid-1990
ts_series <- ts(data=exp(cumsum(rnorm(96)/100)),
       frequency=6, start=1990.5)
# Show some methods for class "ts"
matrix(methods(class="ts")[3:8], ncol=2)
# "tsp" attribute specifies the date-time index
attributes(ts_series)
# Extract the index
tail(time(ts_series), 11)
# The index is equally spaced
diff(tail(time(ts_series), 11))
# Subset the time series
window(ts_series, start=1992, end=1992.25)
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
plot(ts_series, type="l",  # Create plot
     col="red", lty="solid", xlab="", ylab="")
title(main="Random Prices", line=-1)  # Add title
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
set.seed(1121)  # Reset random number generator
library(zoo)  # Load package zoo
# Create zoo time series of random returns
date_s <- Sys.Date() + 0:3
zoo_series <- zoo(rnorm(NROW(date_s)), order.by=date_s)
zoo_series
attributes(zoo_series)
class(zoo_series)  # Class "zoo"
tail(zoo_series, 3)  # Get last few elements
library(zoo)  # Load package zoo
zoo::coredata(zoo_series)  # Extract coredata
zoo::index(zoo_series)  # Extract time index
start(zoo_series)  # First date
end(zoo_series)  # Last date
zoo_series[start(zoo_series)]  # First element
zoo_series[end(zoo_series)]  # Last element
zoo::coredata(zoo_series) <- rep(1, 4)  # Replace coredata
cumsum(zoo_series)  # Cumulative sum
cummax(cumsum(zoo_series))
cummin(cumsum(zoo_series))
library(zoo)  # Load package zoo
zoo_series <- zoo(matrix(cumsum(rnorm(100)), nc=1),
  order.by=seq(from=as.Date("2013-06-15"), by="day", len=100))
colnames(zoo_series) <- "zoo_series"
tail(zoo_series)
dim(zoo_series)
attributes(zoo_series)
library(zoo)  # Load package zoo
zoo::coredata(zoo_series) <- (1:4)^2  # Replace coredata
zoo_series
lag(zoo_series)  # One day lag
lag(zoo_series, 2)  # Two day lag
lag(zoo_series, k=-1)  # Proper one day lag
diff(zoo_series)  # Diff with one day lag
# Proper lag and original length
lag(zoo_series, -2, na.pad=TRUE)
set.seed(1121)  # Reset random number generator
library(zoo)  # Load package zoo
# Create index of daily dates
date_s <- seq(from=as.Date("2014-07-14"), by="day", length.out=1000)
# Create vector of geometric Brownian motion
zoo_data <- exp(cumsum(rnorm(NROW(date_s))/100))
# Create zoo series of geometric Brownian motion
zoo_series <- zoo(x=zoo_data, order.by=date_s)
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# Plot using plot.zoo method
plot(zoo_series, xlab="", ylab="")
title(main="Random Prices", line=-1)  # Add title
library(zoo)  # Load package zoo
# Subset zoo as matrix
zoo_series[459:463, 1]
# Subset zoo using window()
window(zoo_series,
 start=as.Date("2014-10-15"),
 end=as.Date("2014-10-19"))
# Subset zoo using Date object
zoo_series[as.Date("2014-10-15")]
set.seed(1121)  # Reset random number generator
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
library(zoo)  # Load package zoo
# Create daily date series of class "Date"
index1 <- seq(Sys.Date(), by="days", length.out=365)
# Create zoo time series of random returns
zoo_series1 <- zoo(rnorm(NROW(index1)), order.by=index1)
# Create another zoo time series of random returns
index2 <- seq(Sys.Date()+350, by="days", length.out=365)
zoo_series2 <- zoo(rnorm(NROW(index2)), order.by=index2)
# rbind the two time series - ts1 supersedes ts2
zoo_series3 <- rbind(zoo_series1,
  zoo_series2[zoo::index(zoo_series2) > end(zoo_series1)])
# Plot zoo time series of geometric Brownian motion
plot(exp(cumsum(zoo_series3)/100), xlab="", ylab="")
# Add vertical lines at stitch point
abline(v=end(zoo_series1), col="blue", lty="dashed")
abline(v=start(zoo_series2), col="red", lty="dashed")
title(main="Random Prices", line=-1)  # Add title
# Create daily date series of class "Date"
index1 <- Sys.Date() + -3:1
# Create zoo time series of random returns
zoo_series1 <- zoo(rnorm(NROW(index1)), order.by=index1)
# Create another zoo time series of random returns
index2 <- Sys.Date() + -1:3
zoo_series2 <- zoo(rnorm(NROW(index2)), order.by=index2)
merge(zoo_series1, zoo_series2)  # union of dates
# Intersection of dates
merge(zoo_series1, zoo_series2, all=FALSE)
# Create matrix containing NA values
mat_rix <- sample(18)
mat_rix[sample(NROW(mat_rix), 4)] <- NA
mat_rix <- matrix(mat_rix, nc=3)
# Replace NA values with most recent non-NA values
zoo::na.locf(mat_rix)
rutils::na_locf(mat_rix)
# Get time series of prices
price_s <- mget(c("VTI", "VXX"), envir=rutils::etf_env)
price_s <- lapply(price_s, quantmod::Cl)
price_s <- rutils::do_call(cbind, price_s)
sum(is.na(price_s))
# Carry forward and backward non-NA prices
price_s <- zoo::na.locf(price_s, na.rm=FALSE)
price_s <- zoo::na.locf(price_s, na.rm=FALSE, fromLast=TRUE)
sum(is.na(price_s))
# Remove whole rows containing NA returns
re_turns <- rutils::etf_env$re_turns
sum(is.na(re_turns))
re_turns <- na.omit(re_turns)
# Or carry forward non-NA returns (preferred)
re_turns <- rutils::etf_env$re_turns
re_turns[1, is.na(re_turns[1, ])] <- 0
re_turns <- zoo::na.locf(re_turns, na.rm=FALSE)
sum(is.na(re_turns))
# Replace NAs in xts time series
se_ries <- rutils::etf_env$price_s[, 1]
head(se_ries)
sum(is.na(se_ries))
library(quantmod)
series_zoo <- as.xts(zoo::na.locf(se_ries, na.rm=FALSE,
                            fromLast=TRUE))
series_xts <- xts:::na.locf.xts(se_ries, fromLast=TRUE)
all.equal(series_zoo, series_xts, check.attributes=FALSE)
library(microbenchmark)
summary(microbenchmark(
  zoo=as.xts(zoo::na.locf(se_ries, fromLast=TRUE)),
  xts=xts:::na.locf.xts(se_ries, fromLast=TRUE),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
library(lubridate)  # Load lubridate
library(zoo)  # Load package zoo
# methods(as.zoo)  # Many methods of coercing into zoo
class(EuStockMarkets)  # Multiple ts object
# Coerce mts object into zoo
zoo_series <- as.zoo(EuStockMarkets)
class(zoo::index(zoo_series))  # Index is numeric
head(zoo_series, 3)
# Approximately convert index into class "Date"
zoo::index(zoo_series) <-
  as.Date(365*(zoo::index(zoo_series)-1970))
head(zoo_series, 3)
# Convert index into class "POSIXct"
zoo_series <- as.zoo(EuStockMarkets)
zoo::index(zoo_series) <- date_decimal(zoo::index(zoo_series))
head(zoo_series, 3)
library(lubridate)  # Load lubridate
library(zoo)  # Load package zoo
set.seed(1121)  # Reset random number generator
# Create index of daily dates
date_s <- seq(from=as.Date("2014-07-14"), by="day", length.out=1000)
# Create vector of geometric Brownian motion
zoo_data <- exp(cumsum(rnorm(NROW(date_s))/100))
# Create zoo time series of geometric Brownian motion
zoo_series <- zoo(x=zoo_data, order.by=date_s)
head(zoo_series, 3)  # zoo object
# as.ts() creates ts object with frequency=1
ts_series <- as.ts(zoo_series)
tsp(ts_series)  # Frequency=1
# Get start and end dates of zoo_series
start_date <- decimal_date(start(zoo_series))
end_date <- decimal_date(end(zoo_series))
# Calculate frequency of zoo_series
fre_quency <- NROW(zoo_series)/(end_date-start_date)
da_ta <- zoo::coredata(zoo_series)  # Extract data from zoo_series
# Create ts object using ts()
ts_series <- ts(data=da_ta, start=start_date,
          frequency=fre_quency)
# Display start of time series
window(ts_series, start=start(ts_series),
 end=start(ts_series)+4/365)
head(time(ts_series))  # Display index dates
head(as.Date(date_decimal(zoo::coredata(time(ts_series)))))
library(lubridate)  # Load lubridate
library(zoo)  # Load package zoo
# Create weekday Boolean vector
week_days <- weekdays(zoo::index(zoo_series))
is_weekday <- !((week_days == "Saturday") |
  (week_days == "Sunday"))
# Remove weekends from zoo time series
zoo_series <- zoo_series[is_weekday, ]
head(zoo_series, 7)  # zoo object
# as.ts() creates NA values
ts_series <- as.ts(zoo_series)
head(ts_series, 7)
# Create vector of regular dates, including weekends
date_s <- seq(from=start(zoo_series),
            by="day",
            length.out=NROW(zoo_series))
zoo::index(zoo_series) <- date_s
ts_series <- as.ts(zoo_series)
head(ts_series, 7)
set.seed(1121)  # Reset random number generator
library(xts)  # Load package xts
# Create xts time series of random returns
date_s <- Sys.Date() + 0:3
x_ts <- xts(rnorm(NROW(date_s)), order.by=date_s)
names(x_ts) <- "random"
x_ts
tail(x_ts, 3)  # Get last few elements
first(x_ts)  # Get first element
last(x_ts)  # Get last element
class(x_ts)  # Class "xts"
attributes(x_ts)
# Get the time zone of an xts object
indexTZ(x_ts)
load(file="/Users/jerzy/Develop/lecture_slides/data/zoo_data.RData")
library(xts)  # Load package xts
# as.xts() coerces zoo series into xts series
st_ox <- as.xts(zoo_stx)
dim(st_ox)
head(st_ox[, 1:4], 4)
# Plot using plot.xts method
xts::plot.xts(st_ox[, "Close"], xlab="", ylab="", main="")
title(main="MSFT Prices")  # Add title
library(xts)  # Load xts
library(lubridate)  # Load lubridate
# Coerce EuStockMarkets into class xts
x_ts <- xts(zoo::coredata(EuStockMarkets),
      order.by=date_decimal(zoo::index(EuStockMarkets)))
# Plot all columns in single panel: xts v.0.9-8
col_ors <- rainbow(NCOL(x_ts))
plot(x_ts, main="EuStockMarkets using xts",
     col=col_ors, major.ticks="years",
     minor.ticks=FALSE)
legend("topleft", legend=colnames(EuStockMarkets),
 inset=0.2, cex=0.7, , lty=rep(1, NCOL(x_ts)),
 lwd=3, col=col_ors, bg="white")
# Plot only first column: xts v.0.9-7
plot(x_ts[, 1], main="EuStockMarkets using xts",
     col=col_ors[1], major.ticks="years",
     minor.ticks=FALSE)
# Plot remaining columns
for (col_umn in 2:NCOL(x_ts))
  lines(x_ts[, col_umn], col=col_ors[col_umn])
# Plot using quantmod
library(quantmod)
plot_theme <- chart_theme()
plot_theme$col$line.col <- col_ors
chart_Series(x=x_ts, theme=plot_theme,
       name="EuStockMarkets using quantmod")
legend("topleft", legend=colnames(EuStockMarkets),
 inset=0.2, cex=0.7, , lty=rep(1, NCOL(x_ts)),
 lwd=3, col=col_ors, bg="white")
library(rutils)
library(ggplot2)
price_s <- rutils::etf_env$price_s[, 1]
price_s <- na.omit(price_s)
# Create ggplot object
etf_gg <- qplot(x=zoo::index(price_s),
          y=as.numeric(price_s),
          geom="line",
          main=names(price_s)) +
  xlab("") + ylab("") +
  theme(  # Add legend and title
    legend.position=c(0.1, 0.5),
    plot.title=element_text(vjust=-2.0),
    plot.background=element_blank()
  )  # end theme
# Render ggplot object
etf_gg
library(rutils)  # Load xts time series data
library(reshape2)
library(ggplot2)
price_s <- rutils::etf_env$price_s[, c("VTI", "IEF")]
price_s <- na.omit(price_s)
# Create data frame of time series
data_frame <- data.frame(dates=zoo::index(price_s),
    zoo::coredata(price_s))
# Reshape data into a single column
data_frame <-
  reshape2::melt(data_frame, id="dates")
x11(width=6, height=5)  # Open plot window
# ggplot the melted data_frame
ggplot(data=data_frame,
 mapping=aes(x=dates, y=value, colour=variable)) +
 geom_line() +
  xlab("") + ylab("") +
  ggtitle("VTI and IEF") +
  theme(  # Add legend and title
    legend.position=c(0.2, 0.8),
    plot.title=element_text(vjust=-2.0)
  )  # end theme
# Load rutils which contains etf_env dataset
library(rutils)
library(dygraphs)
price_s <- rutils::etf_env$price_s[, c("VTI", "IEF")]
price_s <- na.omit(price_s)
# Plot dygraph with date range selector
dygraph(price_s, main="VTI and IEF prices") %>%
  dyOptions(colors=c("blue","green")) %>%
  dyRangeSelector()
# Load rutils which contains etf_env dataset
library(rutils)
library(plotly)
price_s <- rutils::etf_env$price_s[, c("VTI", "IEF")]
price_s <- na.omit(price_s)
# Create data frame of time series
data_frame <- data.frame(dates=zoo::index(price_s),
    zoo::coredata(price_s))
# Plotly syntax using pipes
data_frame %>%
  plot_ly(x=~dates, y=~VTI, type="scatter", mode="lines", name="VTI") %>%
  add_trace(x=~dates, y=~IEF, type="scatter", mode="lines", name="IEF") %>%
  layout(title="VTI and IEF prices",
   xaxis=list(title="Time"),
   yaxis=list(title="Stock Prices"),
   legend=list(x=0.1, y=0.9))
# Or use standard plotly syntax
p_lot <- plot_ly(data=data_frame, x=~dates, y=~VTI, type="scatter", mode="lines", name="VTI")
p_lot <- add_trace(p=p_lot, x=~dates, y=~IEF, type="scatter", mode="lines", name="IEF")
p_lot <- layout(p=p_lot, title="VTI and IEF prices", xaxis=list(title="Time"), yaxis=list(title="Stock Prices"), legend=list(x=0.1, y=0.9))
p_lot
# Subset xts using a date range string
price_s <- rutils::etf_env$price_s
sub_prices <- price_s["2014-10-15/2015-01-10", 1:4]
first(sub_prices)
last(sub_prices)
# Subset Nov 2014 using a date string
sub_prices <- price_s["2014-11", 1:4]
first(sub_prices)
last(sub_prices)
# Subset all data after Nov 2014
sub_prices <- price_s["2014-11/", 1:4]
first(sub_prices)
last(sub_prices)
# Comma after date range not necessary
all.equal(price_s["2014-11", ], price_s["2014-11"])
# .subset_xts() is faster than the bracket []
library(microbenchmark)
summary(microbenchmark(
  bracket=price_s[10:20, ],
  subset=xts::.subset_xts(price_s, 10:20),
  times=10))[, c(1, 4, 5)]
# Specify string representing a date
dat_e <- "2014-10-15"
# Subset price_s in two different ways
price_s <- rutils::etf_env$price_s
all.equal(price_s[zoo::index(price_s) >= dat_e],
    price_s[paste0(dat_e, "/")])
# Boolean subsetting is slower because coercing string into date
library(microbenchmark)
summary(microbenchmark(
  boolean=(price_s[zoo::index(price_s) >= dat_e]),
  date=(price_s[paste0(dat_e, "/")]),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Coerce string into a date
dat_e <- as.Date("2014-10-15")
# Boolean subsetting is faster than using date string
summary(microbenchmark(
  boolean=(price_s[zoo::index(price_s) >= dat_e]),
  date=(price_s[paste0(dat_e, "/")]),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
price_s <- HighFreq::SPY["2012-04"]
# Subset recurring time interval using "T notation",
price_s <- price_s["T10:30:00/T15:00:00"]
first(price_s["2012-04-16"])  # First element of day
last(price_s["2012-04-16"])  # Last element of day
# Suppress timezone warning messages
options(xts_check_tz=FALSE)
# Create time series with overlapping time indices
vti1 <- rutils::etf_env$VTI["/2015"]
vti2 <- rutils::etf_env$VTI["2014/"]
dates1 <- index(vti1)
dates2 <- index(vti2)
# Join by rows
vt_i <- rbind(vti1, vti2)
date_s <- index(vt_i)
sum(duplicated(date_s))
vt_i <- vt_i[!duplicated(date_s), ]
all.equal(vt_i, rutils::etf_env$VTI)
# Alternative method - slightly slower
vt_i <- rbind(vti1, vti2[!(index(vti2) %in% index(vti1))])
all.equal(vt_i, rutils::etf_env$VTI)
# Remove duplicates starting from the end
vt_i <- rbind(vti1, vti2)
vt_i <- vt_i[!duplicated(date_s), ]
vti_fl <- vt_i[!duplicated(date_s, fromLast=TRUE), ]
all.equal(vt_i, vti_fl)
price_s <- rutils::etf_env$price_s[, c("VTI", "IEF")]
price_s <- na.omit(price_s)
str(price_s)  # Display structure of xts
# Subsetting zoo to single column drops dim attribute
zoo_prices <- as.zoo(price_s)
dim(zoo_prices)
dim(zoo_prices[, 1])
# zoo with single column are vectors not matrices
c(is.matrix(zoo_prices), is.matrix(zoo_prices[, 1]))
# xts always have a dim attribute
rbind(base=dim(price_s), subs=dim(price_s[, 1]))
c(is.matrix(price_s), is.matrix(price_s[, 1]))
# Lag of zoo shortens it by one row
rbind(base=dim(zoo_prices), lag=dim(lag(zoo_prices)))
# Lag of xts doesn't shorten it
rbind(base=dim(price_s), lag=dim(lag(price_s)))
# Lag of zoo is in opposite direction from xts
head(lag(zoo_prices, -1), 4)
head(lag(price_s), 4)
# library(rutils)  # Load package rutils
# Indices of last observations in each hour
end_p <- xts::endpoints(price_s, on="hours")
head(end_p)
# Extract the last observations in each hour
head(price_s[end_p, ])
# Lower the periodicity to months
xts_monthly <- to.period(x=price_s,
             period="months", name="MSFT")
# Convert colnames to standard OHLC format
colnames(xts_monthly)
colnames(xts_monthly) <- sapply(
  strsplit(colnames(xts_monthly), split=".", fixed=TRUE),
  function(na_me) na_me[-1]
  )  # end sapply
head(xts_monthly, 3)
# Lower the periodicity to years
xts_yearly <- to.period(x=xts_monthly,
             period="years", name="MSFT")
colnames(xts_yearly) <- sapply(
  strsplit(colnames(xts_yearly), split=".", fixed=TRUE),
  function(na_me) na_me[-1]
  )  # end sapply
head(xts_yearly)
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
load(file="/Users/jerzy/Develop/lecture_slides/data/zoo_data.RData")
library(xts)  # Load package xts
# as.xts() coerces zoo series into xts series
st_ox <- as.xts(zoo_prices)
# Subset xts using a date
stox_sub <- st_ox["2014-11", 1:4]
# Plot OHLC using plot.xts method
xts::plot.xts(stox_sub, type="candles", main="")
title(main="MSFT Prices")  # Add title
load(file="/Users/jerzy/Develop/lecture_slides/data/zoo_data.RData")
ts_stx <- as.ts(zoo_stx)
class(ts_stx)
tail(ts_stx[, 1:4])
library(xts)
st_ox <- as.xts(zoo_stx)
class(st_ox)
tail(st_ox[, 1:4])
