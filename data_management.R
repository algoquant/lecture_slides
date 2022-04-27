






cat("Enter\ttab")  # Cat() interretsp backslash escape sequences
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
input <- readline("Enter a number: ")
class(input)
# Coerce to numeric
input <- as.numeric(input)

# Read text from file and display in editor:
# file.show("mytext.txt")
# file.show("mytext.txt", pager="")

setwd("/Users/jerzy/Develop/lecture_slides/data")
data_frame <- data.frame(type=c("rose", "daisy", "tulip"),
  color=c("red", "white", "yellow"),
  price=c(1.5, 0.5, 1.0),
  row.names=c("flower1", "flower2", "flower3"))  # end data.frame
matrixv <- matrix(sample(1:12), ncol=3,
  dimnames=list(NULL, c("col1", "col2", "col3")))
rownames(matrixv) <- paste("row", 1:NROW(matrixv), sep="")
# Write data frame to text file, and then read it back
write.table(data_frame, file="florist.txt")
data_read <- read.table(file="florist.txt")
data_read  # A data frame

# Write matrix to text file, and then read it back
write.table(matrixv, file="matrix.txt")
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
data10 <- read.csv(con_read, nrows=10)
# Read another 10 rows
data20 <- read.csv(con_read, nrows=10, header=FALSE)
colnames(data20) <- colnames(data10)
# Close the connection to the file
close(con_read)
# Open a read connection to a file
con_read = file("/Users/jerzy/Develop/lecture_slides/data/etf_prices_crsp.csv", "r")
# Read the first 1000 rows
data10 <- read.csv(con_read, nrows=1e3)
colnamev <- colnames(data10)
# Write to a file
countv <- 1
write.csv(data10, paste0("/Users/jerzy/Develop/data/temp/etf_prices_", countv, ".csv"))
# Read remaining rows in a loop 10 rows at a time
# Can produce error without getting to end of file
while (isOpen(con_read)) {
  datav <- read.csv(con_read, nrows=1e3)
  colnames(datav) <- colnamev
  write.csv(datav, paste0("/Users/jerzy/Develop/data/temp/etf_prices_", countv, ".csv"))
  countv <- countv + 1
}  # end while

# Write matrix to csv file, and then read it back
write.csv(matrixv, file="matrix.csv")
mat_read <- read.csv(file="matrix.csv", row.names=1)
mat_read  # Read.csv() reads matrix as data frame
class(mat_read)
mat_read <- as.matrix(mat_read)  # Coerce to matrix
identical(matrixv, mat_read)
write.csv(matrixv, row.names=FALSE,
    file="matrix_ex_rows.csv")
mat_read <- read.csv(file="matrix_ex_rows.csv")
mat_read <- as.matrix(mat_read)
mat_read  # A matrix without row names

setwd("/Users/jerzy/Develop/lecture_slides/data")
library(MASS)  # Load package "MASS"
# Write to CSV file by row - it's very SLOW!!!
MASS::write.matrix(matrixv, file="matrix.csv", sep=",")
# Read using scan() and skip first line with colnames
mat_read <- scan(file="matrix.csv", sep=",", skip=1,
  what=numeric())
# Read colnames
colnamev <- readLines(con="matrix.csv", n=1)
colnamev  # this is a string!
# Convert to char vector
colnamev <- strsplit(colnamev, split=",")[[1]]
mat_read  # mat_read is a vector, not matrix!
# Coerce by row to matrix
mat_read <- matrix(mat_read, ncol=NROW(colnamev), byrow=TRUE)
# Restore colnames
colnames(mat_read) <- colnamev
mat_read
# Scan() is a little faster than read.csv()
library(microbenchmark)
summary(microbenchmark(
  read_csv=read.csv("matrix.csv"),
  scan=scan(file="matrix.csv", sep=",",
    skip=1, what=numeric()),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# Read data from a csv file, including row names
matrixv <- read.csv(file="matrix_bad.csv", row.names=1)
matrixv
class(matrixv)
# Columns with bad data are character or factor
sapply(matrixv, class)
# Copy row names
row_names <- row.names(matrixv)
# sapply loop over columns and coerce to numeric
matrixv <- sapply(matrixv, as.numeric)
# Restore row names
row.names(matrixv) <- row_names
# Replace NAs with zero
matrixv[is.na(matrixv)] <- 0
# matrix without NAs
matrixv

setwd("/Users/jerzy/Develop/lecture_slides/data")
rm(list=ls())
set.seed(1121)  # Reset random number generator
library(zoo)  # Load package zoo
# Create zoo with Date index
dates <- seq(from=as.Date("2013-06-15"), by="day",
        length.out=100)
zoo_series <- zoo(rnorm(NROW(dates)), order.by=dates)
head(zoo_series, 3)
# Write zoo series to text file, and then read it back
write.zoo(zoo_series, file="zoo_series.txt")
zoo_read <- read.zoo("zoo_series.txt")  # Read it back
all.equal(zoo_read, zoo_series)
# Perform the same using write.table() and read.table()
# First coerce zoo_series into data frame
data_frame <- as.data.frame(zoo_series)
data_frame <- cbind(dates, data_frame)
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
dates <- seq(from=as.POSIXct("2013-06-15"),
        by="hour", length.out=100)
zoo_series <- zoo(rnorm(NROW(dates)), order.by=dates)
head(zoo_series, 3)
# Write zoo series to CSV file, and then read it back
write.zoo(zoo_series, file="zoo_series.csv",
    sep=",", col.names=TRUE)
# Read from CSV file using read.csv.zoo()
zoo_read <- read.csv.zoo(file="zoo_series.csv")
all.equal(zoo_series, zoo_read)
# Coerce to xts series
xtes <- xts::as.xts(zoo_read)
class(xtes); head(xtes, 3)
# Coerce zoo series into data frame with custom date format
data_frame <- as.data.frame(zoo_series)
data_frame <- cbind(format(dates, "%m-%d-%Y %H:%M:%S"),
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
sapply(load_ed, function(symbol) {
  assign(symbol, runif(1), envir=globalenv())
})  # end sapply
ls()  # List objects
# Assign new values to objects using for loop
for (symbol in load_ed) {
  assign(symbol, runif(1))
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
# Print option "datatable.print.n_rows"
getOption("datatable.print.n_rows")
options(datatable.print.n_rows=10)
getOption("datatable.print.n_rows")
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
data_table[1:7, .SD, by=.(origin, dest),
     .SDcols=c("weather_delay", "aircraft_delay")]
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

# Select rows with dest to MIA
data_table[.(unique(origin), "MIA")]
# Select carrier_delay for all flights from JFK to MIA
data_table[.("JFK", "MIA"), carrier_delay]
data_table[.("JFK", "MIA"), .(carrier_delay)]
data_table[.("JFK", "MIA"), .(carrier, carrier_delay)]
# Calculate longest carrier_delay from JFK to MIA
data_table[.("JFK", "MIA"), max(carrier_delay)]
# Chain commands to sort the carrier_delay
data_table[.("JFK", "MIA"), .(carrier, carrier_delay)][order(-carrier_delay)]
data_table[.("JFK", "MIA"), .(carrier, carrier_delay)][carrier_delay>0][order(-carrier_delay)]
# Calculate carrier with longest carrier_delay from JFK to MIA
data_table[.("JFK", "MIA"), .(carrier, carrier_delay)][carrier_delay == max(carrier_delay)]
# Calculate longest carrier_delay from JFK to every dest
data_table["JFK", .(max_delay=max(carrier_delay)), keyby=.(dest)]
# Calculate longest carrier_delay for every carrier, from JFK to every dest
data_table["JFK", .(max_delay=max(carrier_delay)), keyby=.(dest, carrier)]
# Calculate carriers with longest carrier_delay from JFK to every dest
# doesn't work
data_table["JFK"][carrier_delay == max(carrier_delay), .(carrier, carrier_delay), by=.(dest)]
data_table["JFK",
     lapply(.SD, function(x) x[max(carrier_delay)]),
     by=.(dest),
     .SDcols=c("dest", "carrier_delay")]
# Set carrier_delay to longest carrier_delay from JFK to MIA
data_table[.("JFK", "MIA", carrier_delay == max(carrier_delay)), carrier_delay := carrier_delay]
# Show the modified row (record)
data_table[.("JFK", "MIA")][carrier_delay == max(carrier_delay)]

# Select using multiple logical clauses
jfk_flights <- data_table[origin=="JFK" & month==6]
dim(data_table); dim(jfk_flights)
# Select first five rows
jfk_flights[1:5]
# Sort data table by "origin" column in ascending order, then by "dest" in descending order
data_table <- data_table[order(origin, -dest)]
# fsort() is much slower than sort() !
datav <- runif(1e3)
all.equal(sort(datav), data.table::fsort(datav))
library(microbenchmark)
summary(microbenchmark(
  pure_r=sort(datav),
  dt=data.table::fsort(datav),
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
prices <- rutils::etfenv$VTI
class(prices); head(prices)
prices <- as.data.frame(prices)
class(prices); head(prices)
# Coerce data frame to a data table
data.table::setDT(prices, keep.rownames=TRUE)
class(prices); head(prices)
# Dates are coerced to strings
sapply(prices, class)
# Coerce xts directly to a data table
data_table <- as.data.table(rutils::etfenv$VTI,
  keep.rownames=TRUE)
class(data_table); head(data_table)
# Dates are not coerced to strings
sapply(data_table, class)
all.equal(prices, data_table, check.attributes=FALSE)

# Coerce xts to a data frame
prices <- rutils::etfenv$VTI
class(prices); head(prices)
prices <- as.data.frame(prices)
class(prices); head(prices)
# Coerce data frame to a data table
data.table::setDT(prices, keep.rownames=TRUE)
class(prices); head(prices)
# Dates are coerced to strings
sapply(prices, class)
# Coerce xts directly to a data table
data_table <- as.data.table(rutils::etfenv$VTI,
  keep.rownames=TRUE)
class(data_table); head(data_table)
# Dates are not coerced to strings
sapply(data_table, class)
all.equal(prices, data_table, check.attributes=FALSE)

# Coerce xts to a data frame
prices <- rutils::etfenv$VTI
class(prices); head(prices)
prices <- as.data.frame(prices)
class(prices); head(prices)
# Coerce data frame to a data table
data.table::setDT(prices, keep.rownames=TRUE)
class(prices); head(prices)
# Dates are coerced to strings
sapply(prices, class)
# Coerce xts directly to a data table
data_table <- as.data.table(rutils::etfenv$VTI,
  keep.rownames=TRUE)
class(data_table); head(data_table)
# Dates are not coerced to strings
sapply(data_table, class)
all.equal(prices, data_table, check.attributes=FALSE)

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
fst::write_fst(t_aq, path="/Users/jerzy/Develop/data/taq.fst")
# Create reference to .fst file similar to a data frame
fs_t <- fst::fst("/Users/jerzy/Develop/data/taq.fst")
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

load(file="/Users/jerzy/Develop/lecture_slides/data/zoo_data.RData")
library(tseries)  # Load package tseries
# Download MSFT data in ts format
stxts <- suppressWarnings(
  get.hist.quote(
    instrument="MSFT",
    start=Sys.Date()-3*365,
    end=Sys.Date(),
    retclass="ts",
    quote=c("Open","High","Low","Close",
      "AdjClose","Volume"),
    origin="1970-01-01")
)  # end suppressWarnings
# Calculate price adjustment vector
ratio <- as.numeric(stxts[, "AdjClose"]/stxts[, "Close"])
# Adjust OHLC prices
stxts_adj <- stxts
stxts_adj[, c("Open","High","Low","Close")] <-
  ratio*stxts[, c("Open","High","Low","Close")]
# Inspect the data
tsp(stxts_adj)  # frequency=1
head(time(stxts_adj))
head(stxts_adj)
tail(stxts_adj)

library(tseries)  # Load package tseries
# Download MSFT data
zoo_stx <- suppressWarnings(
  get.hist.quote(
    instrument="MSFT",
    start=Sys.Date()-3*365,
    end=Sys.Date(),
    quote=c("Open","High","Low","Close",
      "AdjClose","Volume"),
    origin="1970-01-01")
)  # end suppressWarnings

load(file="/Users/jerzy/Develop/lecture_slides/data/zoo_data.RData")
class(zoo_stx)
dim(zoo_stx)
head(zoo_stx, 4)

library(tseries)  # Load package tseries
load(file="/Users/jerzy/Develop/lecture_slides/data/zoo_data.RData")
# Calculate price adjustment vector
ratio <- as.numeric(zoo_stx[, "AdjClose"]/zoo_stx[, "Close"])
head(ratio, 5)
tail(ratio, 5)
# Adjust OHLC prices
zoo_stx_adj <- zoo_stx
zoo_stx_adj[, c("Open","High","Low","Close")] <-
  ratio*zoo_stx[, c("Open","High","Low","Close")]
head(zoo_stx_adj)
tail(zoo_stx_adj)

library(tseries)  # Load package tseries
# Download EUR/USD data
zoo_eurusd <- suppressWarnings(
  get.hist.quote(
    instrument="EUR/USD",
    provider="oanda",
    start=Sys.Date()-3*365,
    end=Sys.Date(),
    origin="1970-01-01")
)  # end suppressWarnings
# Bind and scrub data
zoo_stxeur <- cbind(zoo_eurusd,
               zoo_stx[, "AdjClose"])
colnames(zoo_stxeur) <- c("EURUSD", "MSFT")
zoo_stxeur <-
  zoo_stxeur[complete.cases(zoo_stxeur),]
save(zoo_stx, zoo_stx_adj,
     stxts, stxts_adj,
     zoo_eurusd, zoo_stxeur,
     file="/Users/jerzy/Develop/lecture_slides/data/zoo_data.RData")

load(file="/Users/jerzy/Develop/lecture_slides/data/zoo_data.RData")
# Inspect the data
class(zoo_eurusd)
head(zoo_eurusd, 4)

library(tseries)  # Load package tseries
# Download price and volume data for symbolv into list of zoo objects
zoo_series <- suppressWarnings(
  lapply(symbolv, # Loop for loading data
   get.hist.quote,
   quote=c("AdjClose", "Volume"),
   start=Sys.Date()-3650,
   end=Sys.Date(),
   origin="1970-01-01")  # end lapply
)  # end suppressWarnings
# Flatten list of zoo objects into a single zoo object
zoo_series <- rutils::do_call(cbind, zoo_series)
# Or
# zoo_series <- do.call(cbind, zoo_series)
# Assign names in format "symbol.Close", "symbol.Volume"
names(zoo_series) <- as.numeric(sapply(symbolv,
    paste, c("Close", "Volume"), sep="."))
# Save zoo_series to a comma-separated CSV file
write.zoo(zoo_series, file="zoo_series.csv", sep=",")
# Save zoo_series to a binary .RData file
save(zoo_series, file="zoo_series.RData")

# Select ETF symbols for asset allocation
symbolv <- c("VTI", "VEU", "EEM", "XLY", "XLP", "XLE", "XLF",
  "XLV", "XLI", "XLB", "XLK", "XLU", "VYM", "IVW", "IWB", "IWD",
  "IWF", "IEF", "TLT", "VNQ", "DBC", "GLD", "USO", "VXX", "SVXY",
  "MTUM", "IVE", "VLUE", "QUAL", "VTV", "USMV")
library(rutils)  # Load package rutils
etfenv <- new.env()  # New environment for data
# Boolean vector of symbols already downloaded
isdownloaded <- symbolv %in% ls(etfenv)
# Download data for symbolv using single command - creates pacing error
getSymbols.av(symbolv, adjust=TRUE, env=etfenv,
  output.size="full", api.key="T7JPW54ES8G75310")
# Download data from Alpha Vantage using while loop
nattempts <- 0  # number of download attempts
while (((sum(!isdownloaded)) > 0) & (nattempts<10)) {
  # Download data and copy it into environment
  nattempts <- nattempts + 1
  cat("Download attempt = ", nattempts, "\n")
  for (symbol in na.omit(symbolv[!isdownloaded][1:5])) {
    cat("Processing: ", symbol, "\n")
    tryCatch(  # With error handler
quantmod::getSymbols.av(symbol, adjust=TRUE, env=etfenv, auto.assign=TRUE, output.size="full", api.key="T7JPW54ES8G75310"),
# Error handler captures error condition
error=function(error_cond) {
  print(paste("error handler: ", error_cond))
},  # end error handler
finally=print(paste("symbol=", symbol))
    )  # end tryCatch
  }  # end for
  # Update vector of symbols already downloaded
  isdownloaded <- symbolv %in% ls(etfenv)
  cat("Pausing 1 minute to avoid pacing...\n")
  Sys.sleep(65)
}  # end while
# Download all symbolv using single command - creates pacing error
# quantmod::getSymbols.av(symbolv, env=etfenv, adjust=TRUE, from="2005-01-03", output.size="full", api.key="T7NHW54ES8GG501C")

ls(etfenv)  # List files in etfenv
# Get class of object in etfenv
class(get(x=symbolv[1], envir=etfenv))
# Another way
class(etfenv$VTI)
colnames(etfenv$VTI)
# Get first 3 rows of data
head(etfenv$VTI, 3)
# Get last 11 rows of data
tail(etfenv$VTI, 11)
# Get class of all objects in etfenv
eapply(etfenv, class)
# Get class of all objects in R workspace
lapply(ls(), function(ob_ject) class(get(ob_ject)))
# Get end dates of all objects in etfenv
as.Date(sapply(etfenv, end))

library(rutils)  # Load package rutils
# Check of object is an OHLC time series
is.OHLC(etfenv$VTI)
# Adjust single OHLC object using its name
etfenv$VTI <- adjustOHLC(etfenv$VTI, use.Adjusted=TRUE)

# Adjust OHLC object using string as name
assign(symbolv[1], adjustOHLC(
    get(x=symbolv[1], envir=etfenv), use.Adjusted=TRUE),
  envir=etfenv)

# Adjust objects in environment using vector of strings
for (symbol in ls(etfenv)) {
  assign(symbol,
   adjustOHLC(get(symbol, envir=etfenv), use.Adjusted=TRUE),
   envir=etfenv)
}  # end for

library(rutils)  # Load package rutils
# Define ETF symbols
symbolv <- c("VTI", "VEU", "IEF", "VNQ")
# Extract symbolv from rutils::etfenv
prices <- mget(symbolv, envir=rutils::etfenv)
# prices is a list of xts series
class(prices)
class(prices[[1]])
tail(prices[[1]])
# Extract Close prices
prices <- lapply(prices, quantmod::Cl)
# Collapse list into time series the hard way
xts1 <- cbind(prices[[1]], prices[[2]],
         prices[[3]], prices[[4]])
class(xts1)
dim(xts1)
# Collapse list into time series using do.call()
prices <- do.call(cbind, prices)
all.equal(xts1, prices)
class(prices)
dim(prices)
# Or extract and cbind in single step
prices <- do.call(cbind, lapply(
  mget(symbolv, envir=rutils::etfenv), quantmod::Cl))
# Or extract and bind all data, subset by symbolv
prices <- lapply(symbolv, function(symbol) {
    quantmod::Cl(get(symbol, envir=rutils::etfenv))
})  # end lapply
# Or loop over etfenv without anonymous function
prices <- do.call(cbind,
  lapply(as.list(rutils::etfenv)[symbolv], quantmod::Cl))
# Same, but works only for OHLC series - produces error
prices <- do.call(cbind,
  eapply(rutils::etfenv, quantmod::Cl)[symbolv])

# Column names end with ".Close"
colnames(prices)
strsplit(colnames(prices), split="[.]")
do.call(rbind, strsplit(colnames(prices), split="[.]"))
do.call(rbind, strsplit(colnames(prices), split="[.]"))[, 1]
# Drop ".Close" from colnames
colnames(prices) <- rutils::get_name(colnames(prices))
# Or
# colnames(prices) <- do.call(rbind,
#   strsplit(colnames(prices), split="[.]"))[, 1]
tail(prices, 3)
# Which objects in global environment are class xts?
unlist(eapply(globalenv(), is.xts))
# Save xts to csv file
write.zoo(prices,
  file="/Users/jerzy/Develop/lecture_slides/data/etf_series.csv", sep=",")
# Copy prices into etfenv
etfenv$etf_list <- etf_list
# Or
assign("prices", prices, envir=etfenv)
# Save to .RData file
save(etfenv, file="etf_data.RData")

# Extract VTI prices
vti <- etfenv$prices[ ,"VTI"]
vti <- na.omit(vti)
# Calculate percentage returns "by hand"
vti_lag <- as.numeric(vti)
vti_lag <- c(vti_lag[1], vti_lag[-NROW(vti_lag)])
vti_lag <- xts(vti_lag, zoo::index(vti))
vti_returns <- (vti-vti_lag)/vti_lag
# Calculate percentage returns using dailyReturn()
daily_returns <- quantmod::dailyReturn(vti)
head(cbind(daily_returns, vti_returns))
all.equal(daily_returns, vti_returns, check.attributes=FALSE)
# Calculate returns for all prices in etfenv$prices
returns <- lapply(etfenv$prices, function(xtes) {
  daily_returns <- quantmod::dailyReturn(na.omit(xtes))
  colnames(daily_returns) <- names(xtes)
  daily_returns
})  # end lapply
# "returns" is a list of xts
class(returns)
class(returns[[1]])
# Flatten list of xts into a single xts
returns <- do.call(cbind, returns)
class(returns)
dim(returns)
# Copy returns into etfenv and save to .RData file
# assign("returns", returns, envir=etfenv)
etfenv$returns <- returns
save(etfenv, file="/Users/jerzy/Develop/lecture_slides/data/etf_data.RData")

library(quantmod)
startd <- "2012-05-10"; endd <- "2013-11-20"
# Select all objects in environment and return as environment
new_env <- as.environment(eapply(etfenv, "[",
            paste(startd, endd, sep="/")))
# Select only symbolv in environment and return as environment
new_env <- as.environment(
  lapply(as.list(etfenv)[symbolv], "[",
   paste(startd, endd, sep="/")))
# Extract and cbind Close prices and return to environment
assign("prices", rutils::do_call(cbind,
         lapply(ls(etfenv), function(symbol) {
           xtes <- quantmod::Cl(get(symbol, etfenv))
           colnames(xtes) <- symbol
           xtes
         })), envir=new_env)
# Get sizes of OHLC xts series in etfenv
sapply(mget(symbolv, envir=etfenv), object.size)
# Extract and cbind adjusted prices and return to environment
colname <- function(xtes)
  strsplit(colnames(xtes), split="[.]")[[1]][1]
assign("prices", rutils::do_call(cbind,
         lapply(mget(etfenv$symbolv, envir=etfenv),
                function(xtes) {
                  xtes <- Ad(xtes)
                  colnames(xtes) <- colname(xtes)
                  xtes
         })), envir=new_env)

# Load data frame of S&P500 constituents from CSV file
sp500 <- read.csv(file="/Users/jerzy/Develop/lecture_slides/data/sp500_WRDS_08-30-17.csv")
# Inspect data frame of S&P500 constituents
dim(sp500)
colnames(sp500)
# Extract tickers from the column co_tic
symbolv <- sp500$co_tic
# Get duplicate tickers
tablev <- table(symbolv)
duplicates <- tablev[tablev>1]
duplicates <- names(duplicates)
# Get duplicate records (rows) of sp500
sp500[symbolv %in% duplicates, ]
# Get unique tickers
symbolv <- unique(symbolv)
# Find index of ticker "BRK.B"
which(symbolv=="BRK.B")
# Remove "BRK.B" and later download it separately
symbolv <- symbolv[-which(symbolv=="BRK.B")]

# Load package rutils
library(rutils)
# Create new environment for data
sp500env <- new.env()
# Boolean vector of symbols already downloaded
isdownloaded <- symbolv %in% ls(sp500env)
# Download in while loop from Tiingo and copy into environment
nattempts <- 0  # Number of download attempts
while (((sum(!isdownloaded)) > 0) & (nattempts<5)) {
  # Download data and copy it into environment
  nattempts <- nattempts + 1
  cat("Download attempt = ", nattempts, "\n")
  for (symbol in symbolv[!isdownloaded]) {
    cat("processing: ", symbol, "\n")
    tryCatch(  # With error handler
quantmod::getSymbols(symbol, src="tiingo", adjust=TRUE, auto.assign=TRUE,
           from="1990-01-01", env=sp500env, api.key="j84ac2b9c5bde2d68e33034f65d838092c6c9f10"),
# Error handler captures error condition
error=function(error_cond) {
  print(paste("error handler: ", error_cond))
},  # end error handler
finally=print(paste("symbol=", symbol))
    )  # end tryCatch
  }  # end for
  # Update vector of symbols already downloaded
  isdownloaded <- symbolv %in% ls(sp500env)
  Sys.sleep(2)  # Wait 2 seconds until next attempt
}  # end while
class(sp500env$AAPL)
class(zoo::index(sp500env$AAPL))
tail(sp500env$AAPL)

# "LOW.Low" is a bad column name
colnames(sp500env$LOW)
strsplit(colnames(sp500env$LOW), split="[.]")
do.call(cbind, strsplit(colnames(sp500env$LOW), split="[.]"))
do.call(cbind, strsplit(colnames(sp500env$LOW), split="[.]"))[2, ]
# Extract proper names from column names
namesv <- rutils::get_name(colnames(sp500env$LOW), field=2)
# Or
# namesv <- do.call(rbind, strsplit(colnames(sp500env$LOW),
#                                   split="[.]"))[, 2]
# Rename "LOW" colnames to "LOWES"
colnames(sp500env$LOW) <- paste("LO_WES", namesv, sep=".")
sp500env$LOWES <- sp500env$LOW
rm(LOW, envir=sp500env)
# Rename BF-B colnames to "BFB"
colnames(sp500env$"BF-B") <- paste("BFB", namesv, sep=".")
sp500env$BFB <- sp500env$"BF-B"
rm("BF-B", envir=sp500env)
# Rename BRK-B colnames
sp500env$BRKB <- sp500env$`BRK-B`
rm(`BRK-B`, envir=sp500env)
colnames(sp500env$BRKB) <- gsub("BRK-B", "BRKB", colnames(sp500env$BRKB))
# Save OHLC prices to .RData file
save(sp500env, file="/Users/jerzy/Develop/lecture_slides/data/sp500.RData")
# Download "BRK.B" separately with auto.assign=FALSE
# BRKB <- quantmod::getSymbols("BRK-B", auto.assign=FALSE, src="tiingo", adjust=TRUE, from="1990-01-01", api.key="j84ac2b9c5bde2d68e33034f65d838092c6c9f10")
# colnames(BRKB) <- paste("BRKB", namesv, sep=".")
# sp500env$BRKB <- BRKB

# Plot OHLC candlestick chart for LOWES
chart_Series(x=sp500env$LOWES["2019-12/"],
  TA="add_Vo()", name="LOWES OHLC Stock Prices")
# Plot dygraph
dygraphs::dygraph(sp500env$LOWES["2019-12/", -5], main="LOWES OHLC Stock Prices") %>%
  dyCandlestick()

class(sp500env$AAPL)
# The date-time index is class POSIXct not Date
class(zoo::index(sp500env$AAPL))
# Coerce time indices from class POSIXct to class Date
for (symbol in ls(sp500env)) {
  xtes <- get(symbol, envir=sp500env)
  zoo::index(xtes) <- as.Date(zoo::index(xtes))
  assign(symbol, xtes, envir=sp500env)
}  # end for
class(zoo::index(sp500env$AAPL))
# Save the environment to compressed .RData file
dir_name <- "/Users/jerzy/Develop/lecture_slides/data/"
save(sp500env, file=paste0(dir_name, "sp500.RData"))
# Save the ETF prices into CSV files
dir_name <- "/Users/jerzy/Develop/lecture_slides/data/SP500/"
for (symbol in ls(sp500env)) {
  zoo::write.zoo(sp500env$symbol, file=paste0(dir_name, symbol, ".csv"))
}  # end for
# Or using lapply()
file_names <- lapply(ls(sp500env), function(symbol) {
  xtes <- get(symbol, envir=sp500env)
  zoo::write.zoo(xtes, file=paste0(dir_name, symbol, ".csv"))
  symbol
})  # end lapply
unlist(file_names)
# Or using eapply() and data.table::fwrite()
file_names <- eapply(sp500env , function(xtes) {
  file_name <- rutils::get_name(colnames(xtes)[1])
  data.table::fwrite(data.table::as.data.table(xtes), file=paste0(dir_name, file_name, ".csv"))
  file_name
})  # end eapply
unlist(file_names)

# Load the environment from compressed .RData file
dir_name <- "/Users/jerzy/Develop/lecture_slides/data/"
load(file=paste0(dir_name, "sp500.RData"))
# Get all the .csv file names in the directory
dir_name <- "/Users/jerzy/Develop/lecture_slides/data/SP500/"
file_names <- Sys.glob(paste0(dir_name, "*.csv"))
# Create new environment for data
sp500env <- new.env()
for (file_name in file_names) {
  xtes <- xts::as.xts(zoo::read.csv.zoo(file_name))
  symbol <- rutils::get_name(colnames(xtes)[1])
  # symbol <- strsplit(colnames(xtes), split="[.]")[[1]][1]
  assign(symbol, xtes, envir=sp500env)
}  # end for
# Or using fread()
for (file_name in file_names) {
  xtes <- data.table::fread(file_name)
  data.table::setDF(xtes)
  xtes <- xts::xts(xtes[, -1], as.Date(xtes[, 1]))
  symbol <- rutils::get_name(colnames(xtes)[1])
  assign(symbol, xtes, envir=sp500env)
}  # end for

# Remove all files from environment(if necessary)
rm(list=ls(sp500env), envir=sp500env)
# Download in while loop from Alpha Vantage and copy into environment
isdownloaded <- symbolv %in% ls(sp500env)
nattempts <- 0
while (((sum(!isdownloaded)) > 0) & (nattempts<10)) {
  # Download data and copy it into environment
  nattempts <- nattempts + 1
  for (symbol in symbolv[!isdownloaded]) {
    cat("processing: ", symbol, "\n")
    tryCatch(  # With error handler
quantmod::getSymbols(symbol, src="av", adjust=TRUE, auto.assign=TRUE, env=sp500env,
           output.size="full", api.key="T7JPW54ES8G75310"),
# error handler captures error condition
error=function(error_cond) {
  print(paste("error handler: ", error_cond))
},  # end error handler
finally=print(paste("symbol=", symbol))
    )  # end tryCatch
  }  # end for
  # Update vector of symbols already downloaded
  isdownloaded <- symbolv %in% ls(sp500env)
  Sys.sleep(2)  # Wait 2 seconds until next attempt
}  # end while
# Adjust all OHLC prices in environment
for (symbol in ls(sp500env)) {
  assign(symbol,
    adjustOHLC(get(x=symbol, envir=sp500env), use.Adjusted=TRUE),
    envir=sp500env)
}  # end for

library(rutils)  # Load package rutils
# Assign name SP500 to ^GSPC symbol
setSymbolLookup(SP500=list(name="^GSPC", src="yahoo"))
getSymbolLookup()
# view and clear options
options("getSymbols.sources")
options(getSymbols.sources=NULL)
# Download S&P500 prices into etfenv
quantmod::getSymbols("SP500", env=etfenv,
    adjust=TRUE, auto.assign=TRUE, from="1990-01-01")
chart_Series(x=etfenv$SP500["2016/"],
       TA="add_Vo()", name="S&P500 index")

library(rutils)  # Load package rutils
# Assign name DJIA to ^DJI symbol
setSymbolLookup(DJIA=list(name="^DJI", src="yahoo"))
getSymbolLookup()
# view and clear options
options("getSymbols.sources")
options(getSymbols.sources=NULL)
# Download DJIA prices into etfenv
quantmod::getSymbols("DJIA", env=etfenv,
    adjust=TRUE, auto.assign=TRUE, from="1990-01-01")
chart_Series(x=etfenv$DJIA["2016/"],
       TA="add_Vo()", name="DJIA index")

# Calculate prices from OHLC data of the S&P500 stocks
prices <- eapply(sp500env, quantmod::Cl)
prices <- rutils::do_call(cbind, prices)
# Carry forward non-NA prices
prices <- zoo::na.locf(prices, na.rm=FALSE)
# Get first column name
colnames(prices[, 1])
rutils::get_name(colnames(prices[, 1]))
# Modify column names
colnames(prices) <- rutils::get_name(colnames(prices))
# Or
# colnames(prices) <- do.call(rbind,
#   strsplit(colnames(prices), split="[.]"))[, 1]
# Calculate percentage returns
returns <- xts::diff.xts(prices)/
  rutils::lagit(prices, pad_zeros=FALSE)
# Select a random sample of 100 prices and returns
set.seed(1121)
samplev <- sample(NCOL(returns), s=100, replace=FALSE)
prices100 <- prices[, samplev]
returns100 <- returns[, samplev]
# Save the data into binary files
save(prices, prices100,
     file="/Users/jerzy/Develop/lecture_slides/data/sp500_prices.RData")
save(returns, returns100,
     file="/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")

# Setup code
symbol <- "SPY"
startd <- as.Date("1990-01-01")
todayd <- Sys.Date()
tspan <- "day"
# Replace below your own Polygon API key
apikey <- "SEpnsBpiRyONMJdl48r6dOo0_pjmCu5r"
# Create url for download
urll <- paste0("https://api.polygon.io/v2/aggs/ticker/", symbol, "/range/1/", tspan, "/", startd, "/", todayd, "?adjusted=true&sort=asc&limit=50000&apiKey=", apikey)
# Download SPY OHLC prices in JSON format from Polygon
ohlc <- jsonlite::read_json(urll)
class(ohlc)
NROW(ohlc)
names(ohlc)
# Extract list of prices from json object
ohlc <- ohlc$results
# Coerce from list to matrix
ohlc <- lapply(ohlc, unlist)
ohlc <- do.call(rbind, ohlc)
# Coerce time from milliseconds to dates
dates <- ohlc[, "t"]/1e3
dates <- as.POSIXct(dates, origin="1970-01-01")
dates <- as.Date(dates)
tail(dates)
# Coerce from matrix to xts
ohlc <- ohlc[, c("o","h","l","c","v","vw")]
colnames(ohlc) <- c("Open", "High", "Low", "Close", "Volume", "VWAP")
ohlc <- xts::xts(ohlc, order.by=dates)
tail(ohlc)
# Save the xts time series to compressed RData file
save(ohlc, file="/Users/jerzy/Data/spy_daily.RData")
# Candlestick plot of SPY OHLC prices
dygraphs::dygraph(ohlc[, 1:4], main=paste("Candlestick Plot of", symbol, "OHLC prices")) %>%
  dygraphs::dyCandlestick()

# Select ETF symbols for asset allocation
symbolv <- c("VTI", "VEU", "EEM", "XLY", "XLP", "XLE", "XLF",
  "XLV", "XLI", "XLB", "XLK", "XLU", "VYM", "IVW", "IWB", "IWD",
  "IWF", "IEF", "TLT", "VNQ", "DBC", "GLD", "USO", "VXX", "SVXY",
  "MTUM", "IVE", "VLUE", "QUAL", "VTV", "USMV")
# Setup code
etfenv <- new.env()  # New environment for data
# Boolean vector of symbols already downloaded
isdownloaded <- symbolv %in% ls(etfenv)

# Download data from Polygon using while loop
while (sum(!isdownloaded) > 0) {
  for (symbol in symbolv[!isdownloaded]) {
    cat("Processing:", symbol, "\n")
    tryCatch({  # With error handler
# Download OHLC bars from Polygon into JSON format file
urll <- paste0("https://api.polygon.io/v2/aggs/ticker/", symbol, "/range/1/", tspan, "/", startd, "/", todayd, "?adjusted=true&sort=asc&limit=50000&apiKey=", apikey)
ohlc <- jsonlite::read_json(urll)
# Extract list of prices from json object
ohlc <- ohlc$results
# Coerce from list to matrix
ohlc <- lapply(ohlc, unlist)
ohlc <- do.call(rbind, ohlc)
# Coerce time from milliseconds to dates
dates <- ohlc[, "t"]/1e3
dates <- as.POSIXct(dates, origin="1970-01-01")
dates <- as.Date(dates)
# Coerce from matrix to xts
ohlc <- ohlc[, c("o","h","l","c","v","vw")]
colnames(ohlc) <- paste0(symbol, ".", c("Open", "High", "Low", "Close", "Volume", "VWAP"))
ohlc <- xts::xts(ohlc, order.by=dates)
# Save to environment
assign(symbol, ohlc, envir=etfenv)
Sys.sleep(1)
},
    error={function(error_cond) print(paste("Error handler:", error_cond))},
    finally=print(paste0("symbol=", symbol))
    )  # end tryCatch
  }  # end for
  # Update vector of symbols already downloaded
  isdownloaded <- symbolv %in% ls(etfenv)
}  # end while
save(etfenv, file="/Users/jerzy/Develop/lecture_slides/data/etf_data.RData")

# Extract Close prices
prices <- eapply(etfenv, quantmod::Cl)
prices <- do.call(cbind, prices)
# Drop ".Close" from colnames
colnames(prices) <- do.call(rbind, strsplit(colnames(prices), split="[.]"))[, 1]
# Calculate the log returns
returns <- xts::diff.xts(log(prices))
# Copy prices and returns into etfenv
etfenv$prices <- prices
etfenv$returns <- returns
# Copy symbolv into etfenv
etfenv$symbolv <- symbolv
# Calculate the risk-return statistics
riskstats <- PerformanceAnalytics::table.Stats(returns)
# Transpose the data frame
riskstats <- as.data.frame(t(riskstats))
# Add Name column
riskstats$Name <- rownames(riskstats)
# Copy riskstats into etfenv
etfenv$riskstats <- riskstats
# Calculate the beta, alpha, Treynor ratio, and other performance statistics
capmstats <- PerformanceAnalytics::table.CAPM(Ra=returns[, symbolv],
                                         Rb=returns[, "VTI"], scale=252)
colnamev <- strsplit(colnames(capmstats), split=" ")
colnamev <- do.call(cbind, colnamev)[1, ]
colnames(capmstats) <- colnamev
capmstats <- t(capmstats)
capmstats <- capmstats[, -1]
colnamev <- colnames(capmstats)
whichv <- match(c("Annualized Alpha", "Information Ratio", "Treynor Ratio"), colnamev)
colnamev[whichv] <- c("Alpha", "Information", "Treynor")
colnames(capmstats) <- colnamev
capmstats <- capmstats[order(capmstats[, "Alpha"], decreasing=TRUE), ]
# Copy capmstats into etfenv
etfenv$capmstats <- capmstats
save(etfenv, file="/Users/jerzy/Develop/lecture_slides/data/etf_data.RData")

library(rutils)  # Load package rutils
# Create name corresponding to "^GSPC" symbol
setSymbolLookup(SP500=list(name="^GSPC", src="yahoo"))
getSymbolLookup()
# view and clear options
options("getSymbols.sources")
options(getSymbols.sources=NULL)
# Download S&P500 prices into etfenv
quantmod::getSymbols("SP500", env=etfenv,
    adjust=TRUE, auto.assign=TRUE, from="1990-01-01")
chart_Series(x=etfenv$SP500["2016/"],
       TA="add_Vo()", name="S&P500 index")

library(rutils)  # Load package rutils
library(RCurl)  # Load package RCurl
library(XML)  # Load package XML
# Download text data from URL
sp500 <- getURL(
  "https://en.wikipedia.org/wiki/List_of_S%26P500_companies")
# Extract tables from the text data
sp500 <- readHTMLTable(sp500)
str(sp500)
# Extract colnames of data frames
lapply(sp500, colnames)
# Extract S&P500 constituents
sp500 <- sp500[[1]]
head(sp500)
# Create valid R names from symbols containing "-" or "."characters
sp500$namesv <- gsub("-", "_", sp500$Ticker)
sp500$namesv <- gsub("[.]", "_", sp500$names)
# Write data frame of S&P500 constituents to CSV file
write.csv(sp500,
  file="/Users/jerzy/Develop/lecture_slides/data/sp500_Yahoo.csv",
  row.names=FALSE)

library(rutils)  # Load package rutils
# Load data frame of S&P500 constituents from CSV file
sp500 <- read.csv(file="/Users/jerzy/Develop/lecture_slides/data/sp500_Yahoo.csv")
# Register symbols corresponding to R names
for (indeks in 1:NROW(sp500)) {
  cat("processing: ", sp500$Ticker[indeks], "\n")
  setSymbolLookup(structure(
    list(list(name=sp500$Ticker[indeks])),
    names=sp500$names[indeks]))
}  # end for
sp500env <- new.env()  # new environment for data
# Remove all files (if necessary)
rm(list=ls(sp500env), envir=sp500env)
# Download data and copy it into environment
rutils::get_data(sp500$names,
   env_out=sp500env, startd="1990-01-01")
# Or download in loop
for (symbol in sp500$names) {
  cat("processing: ", symbol, "\n")
  rutils::get_data(symbol,
   env_out=sp500env, startd="1990-01-01")
}  # end for
save(sp500env, file="/Users/jerzy/Develop/lecture_slides/data/sp500.RData")
chart_Series(x=sp500env$BRKB["2016/"],
       TA="add_Vo()", name="BRK-B stock")

library(quantmod)
# Download U.S. unemployment rate data
unemp_rate <- quantmod::getSymbols("UNRATE",
            auto.assign=FALSE, src="FRED")
# Plot U.S. unemployment rate data
chart_Series(unemp_rate["1990/"],
      name="U.S. unemployment rate")

library(rutils)  # Load package rutils
install.packages("devtools")
library(devtools)
# Install package Quandl from github
install_github("quandl/R-package")
library(Quandl)  # Load package Quandl
# Register Quandl API key
Quandl.api_key("pVJi9Nv3V8CD3Js5s7Qx")
# Get short description
packageDescription("Quandl")
# Load help page
help(package="Quandl")
# Remove Quandl from search path
detach("package:Quandl")

library(rutils)  # Load package rutils
# Download EOD AAPL prices from WIKI free database
prices <- Quandl(code="WIKI/AAPL",
            type="xts", startd="1990-01-01")
x11(width=14, height=7)
chart_Series(prices["2016", 1:4], name="AAPL OHLC prices")
# Add trade volume in extra panel
add_TA(prices["2016", 5])
# Download euro currency rates
prices <- Quandl(code="BNP/USDEUR",
    startd="2013-01-01",
    endd="2013-12-01", type="xts")
# Download multiple time series
prices <- Quandl(code=c("NSE/OIL", "WIKI/AAPL"),
    startd="2013-01-01", type="xts")
# Download AAPL gross profits
prof_it <- Quandl("RAYMOND/AAPL_GROSS_PROFIT_Q", type="xts")
chart_Series(prof_it, name="AAPL gross profits")
# Download Hurst time series
prices <- Quandl(code="PE/AAPL_HURST",
    startd="2013-01-01", type="xts")
chart_Series(prices["2016/", 1], name="AAPL Hurst")

library(rutils)  # Load package rutils
# Load S&P500 stock Quandl codes
sp500 <- read.csv(
  file="/Users/jerzy/Develop/lecture_slides/data/sp500_quandl.csv")
# Replace "-" with "_" in symbols
sp500$free_code <- gsub("-", "_", sp500$free_code)
head(sp500)
# vector of symbols in sp500 frame
tickers <- gsub("-", "_", sp500$ticker)
# Or
tickers <- matrix(unlist(
  strsplit(sp500$free_code, split="/"),
  use.names=FALSE), ncol=2, byrow=TRUE)[, 2]
# Or
tickers <- do_call_rbind(
  strsplit(sp500$free_code, split="/"))[, 2]

library(rutils)  # Load package rutils
sp500env <- new.env()  # new environment for data
# Remove all files (if necessary)
rm(list=ls(sp500env), envir=sp500env)
# Boolean vector of symbols already downloaded
isdownloaded <- tickers %in% ls(sp500env)
# Download data and copy it into environment
for (ticker in tickers[!isdownloaded]) {
  cat("processing: ", ticker, "\n")
  datav <- Quandl(code=paste0("WIKI/", ticker),
            startd="1990-01-01", type="xts")[, -(1:7)]
  colnames(datav) <- paste(ticker,
    c("Open", "High", "Low", "Close", "Volume"), sep=".")
  assign(ticker, datav, envir=sp500env)
}  # end for
save(sp500env, file="/Users/jerzy/Develop/lecture_slides/data/sp500.RData")
chart_Series(x=sp500env$XOM["2016/"], TA="add_Vo()", name="XOM stock")

library(rutils)
library(Quandl)
# Register Quandl API key
Quandl.api_key("pVJi9Nv3V8CD3Js5s7Qx")
# Download E-mini S&P500 futures prices
prices <- Quandl(code="CHRIS/CME_ES1",
  type="xts", startd="1990-01-01")
prices <- prices[, c("Open", "High", "Low", "Last", "Volume")]
colnames(prices)[4] <- "Close"
# Plot the prices
x11(width=5, height=4)  # Open x11 for plotting
chart_Series(x=prices["2008-06/2009-06"],
       TA="add_Vo()", name="S&P500 Futures")
# Plot dygraph
dygraphs::dygraph(prices["2008-06/2009-06", -5],
  main="S&P500 Futures") %>%
  dyCandlestick()

# Read CBOE futures expiration dates
dates <- read.csv(file="/Users/jerzy/Develop/lecture_slides/data/futures_expiration_dates_codes.csv",
  row.names=1)
dir_name <- "/Users/jerzy/Develop/data/vix_data"
dir.create(dir_name)
symbolv <- rownames(dates)
file_names <- file.path(dir_name, paste0(symbolv, ".csv"))
log_file <- file.path(dir_name, "log_file.txt")
cboe_url <- "https://markets.cboe.com/us/futures/market_statistics/historical_data/products/csv/VX/"
urls <- paste0(cboe_url, dates[, 1])
# Download files in loop
for (it in seq_along(urls)) {
    tryCatch(  # Warning and error handler
  download.file(urls[it],
          destfile=file_names[it], quiet=TRUE),
# Warning handler captures warning condition
warning=function(warning_cond) {
  cat(paste("warning handler: ", warning_cond, "\n"), file=log_file, append=TRUE)
},  # end warning handler
# Error handler captures error condition
error=function(error_cond) {
  cat(paste("error handler: ", error_cond, "\n"), append=TRUE)
},  # end error handler
finally=cat(paste("Processing file name =", file_names[it], "\n"), append=TRUE)
    )  # end tryCatch
}  # end for

# Create new environment for data
vix_env <- new.env()
# Download VIX data for the months 6, 7, and 8 in 2018
library(qmao)
quantmod::getSymbols("VX", Months=1:12,
  Years=2018, src="cfe", auto.assign=TRUE, env=vix_env)
# Or
qmao::getSymbols.cfe(Symbols="VX",
  Months=6:8, Years=2018, env=vix_env,
  verbose=FALSE, auto.assign=TRUE)
# Calculate the classes of all the objects
# In the environment vix_env
unlist(eapply(vix_env, function(x) {class(x)[1]}))
class(vix_env$VX_M18)
colnames(vix_env$VX_M18)
# Save the data to a binary file called "vix_cboe.RData".
save(vix_env,
  file="/Users/jerzy/Develop/data/vix_data/vix_cboe.RData")

# Install and load package readxl
install.packages("readxl")
library(readxl)
dir_name <- "/Users/jerzy/Develop/lecture_slides/data"
filev <- file.path(dir_name, "multi_tabs.xlsx")
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
xtes <- xts::xts(do.call(cbind, listv)[, -1], listv[[1]])
class(xtes); dim(xtes)
# Replace NA values with the most recent non-NA values
sum(is.na(xtes))
xtes <- zoo::na.locf(xtes, na.rm=FALSE)
xtes <- zoo::na.locf(xtes, fromLast=TRUE)

# Read names of all the sheets in an Excel spreadsheet
namesv <- readxl::excel_sheets(filev)
# Read all the sheets from an Excel spreadsheet
sheets <- lapply(namesv, read_xlsx, path=filev)
names(sheets) <- namesv
# sheets is a list of tibbles
sapply(sheets, class)
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
class(sheets)
sheets <- lapply(sheets, to_xts)
sapply(sheets, class)
# Replace NA values with the most recent non-NA values
sapply(sheets, function(xtes) sum(is.na(xtes)))
sheets <- lapply(sheets, zoo::na.locf, na.rm=FALSE)
sheets <- lapply(sheets, zoo::na.locf, fromLast=TRUE)

#Perform calculations in R,
#And export to CSV files
setwd("/Users/jerzy/Develop/lecture_slides/data")
# Read data frame, with row names from first column
data_read <- read.csv(file="florist.csv", row.names=1)
# Subset data frame
data_read <- data_read[data_read[, "type"]=="daisy", ]
# Write data frame to CSV file, with row names
write.csv(data_read, file="daisies.csv")

#Perform calculations in R,
#And export to CSV files
setwd("/Users/jerzy/Develop/lecture_slides/data")
# Read data frame, with row names from first column
data_read <- read.csv(file="florist.csv", row.names=1)
# Subset data frame
data_read <- data_read[data_read[, "type"]=="daisy", ]
# Write data frame to CSV file, with row names
write.csv(data_read, file="daisies.csv")

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
google_sheet <- gs_title("my_data")
# view sheet summary
google_sheet
# List tab names in sheet
tab_s <- gs_ws_ls(google_sheet)
# Set curl options
library(httr)
httr::set_config(config(ssl_verifypeer=0L))
# Read data from sheet
gs_read(google_sheet)
# Read data from single tab of sheet
gs_read(google_sheet, ws=tab_s[1])
gs_read_csv(google_sheet, ws=tab_s[1])
# Or using dplyr pipes
google_sheet %>% gs_read(ws=tab_s[1])
# Download data from sheet into file
gs_download(google_sheet, ws=tab_s[1],
      to="/Users/jerzy/Develop/lecture_slides/data/google_sheet.csv")
# Open sheet in internet browser
gs_browse(google_sheet)

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
google_sheet <- gs_title("my_data")
# view sheet summary
google_sheet
# List tab names in sheet
tab_s <- gs_ws_ls(google_sheet)
# Set curl options
library(httr)
httr::set_config(config(ssl_verifypeer=0L))
# Read data from sheet
gs_read(google_sheet)
# Read data from single tab of sheet
gs_read(google_sheet, ws=tab_s[1])
gs_read_csv(google_sheet, ws=tab_s[1])
# Or using dplyr pipes
google_sheet %>% gs_read(ws=tab_s[1])
# Download data from sheet into file
gs_download(google_sheet, ws=tab_s[1],
      to="/Users/jerzy/Develop/lecture_slides/data/google_sheet.csv")
# Open sheet in internet browser
gs_browse(google_sheet)
