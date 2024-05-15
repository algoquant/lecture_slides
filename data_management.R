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
write_clip <- function(data, row.names=FALSE, col.names=TRUE, ...) {
  write.table(x=data, file="clipboard", sep="\t",
      row.names=row.names, col.names=col.names, ...)
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
colnamev <- colnames(data10)
# Write to a file
countv <- 1
write.csv(data10, paste0("/Users/jerzy/Develop/data/temp/etf_prices_", countv, ".csv"))
# Read remaining rows in a loop 10 rows at a time
# Can produce error without getting to end of file
while (isOpen(filecon)) {
  datav <- read.csv(filecon, nrows=1e3)
  colnames(datav) <- colnamev
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
colnamev <- readLines(con="matrix.csv", n=1)
colnamev  # this is a string!
# Convert to char vector
colnamev <- strsplit(colnamev, split=",")[[1]]
readmat  # readmat is a vector, not matrix!
# Coerce by row to matrix
readmat <- matrix(readmat, ncol=NROW(colnamev), byrow=TRUE)
# Restore colnames
colnames(readmat) <- colnamev
readmat
# Scan() is a little faster than read.csv()
library(microbenchmark)
summary(microbenchmark(
  read_csv=read.csv("matrix.csv"),
  scan=scan(file="matrix.csv", sep=",",
    skip=1, what=numeric()),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Read data from a csv file, including row names
matv <- read.csv(file="matrix_bad.csv", row.names=1)
matv
class(matv)
# Columns with bad data are character or factor
sapply(matv, class)
# Coerce character column to numeric
matv$col2 <- as.numeric(matv$col2)
# Or
# Copy row names
rownames <- row.names(matv)
# sapply loop over columns and coerce to numeric
matv <- sapply(matv, as.numeric)
# Restore row names
row.names(matv) <- rownames
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
    setDF(data.table::fread(filen)))
# fread() is much faster than read.csv()
library(microbenchmark)
summary(microbenchmark(
  rcode=read.csv(filen),
  fread=setDF(data.table::fread(filen)),
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
dtable[, .(orig=origin, mon=month)]
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
# Select rows with dest to MIA
dtable[.(unique(origin), "MIA")]
# Select carrier_delay for all flights from JFK to MIA
dtable[.("JFK", "MIA"), carrier_delay]
dtable[.("JFK", "MIA"), .(carrier_delay)]
dtable[.("JFK", "MIA"), .(carrier, carrier_delay)]
# Calculate longest carrier_delay from JFK to MIA
dtable[.("JFK", "MIA"), max(carrier_delay)]
# Chain commands to sort the carrier_delay
dtable[.("JFK", "MIA"), .(carrier, carrier_delay)][order(-carrier_delay)]
dtable[.("JFK", "MIA"), .(carrier, carrier_delay)][carrier_delay>0][order(-carrier_delay)]
# Calculate carrier with longest carrier_delay from JFK to MIA
dtable[.("JFK", "MIA"), .(carrier, carrier_delay)][carrier_delay == max(carrier_delay)]
# Calculate longest carrier_delay from JFK to every dest
dtable["JFK", .(max_delay=max(carrier_delay)), keyby=.(dest)]
# Calculate longest carrier_delay for every carrier, from JFK to every dest
dtable["JFK", .(max_delay=max(carrier_delay)), keyby=.(dest, carrier)]
# Calculate carriers with longest carrier_delay from JFK to every dest
# doesn't work
dtable["JFK"][carrier_delay == max(carrier_delay), .(carrier, carrier_delay), by=.(dest)]
dtable["JFK",
     lapply(.SD, function(x) x[max(carrier_delay)]),
     by=.(dest),
     .SDcols=c("dest", "carrier_delay")]
# Set carrier_delay to longest carrier_delay from JFK to MIA
dtable[.("JFK", "MIA", carrier_delay == max(carrier_delay)), carrier_delay := carrier_delay]
# Show the modified row (record)
dtable[.("JFK", "MIA")][carrier_delay == max(carrier_delay)]
# Select using multiple logical clauses
jfkf <- dtable[origin=="JFK" & month==6]
dim(dtable); dim(jfkf)
# Select first five rows
jfkf[1:5]
# Sort data table by "origin" column in ascending order, then by "dest" in descending order
dtable <- dtable[order(origin, -dest)]
# fsort() is much slower than sort() !
datav <- runif(1e3)
all.equal(sort(datav), data.table::fsort(datav))
library(microbenchmark)
summary(microbenchmark(
  rcode=sort(datav),
  dt=data.table::fsort(datav),
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
load(file="/Users/jerzy/Develop/lecture_slides/data/zoo_data.RData")
library(tseries)  # Load package tseries
# Download MSFT data in ts format
pricev <- suppressWarnings(
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
ratio <- as.numeric(pricev[, "AdjClose"]/pricev[, "Close"])
# Adjust OHLC prices
pricadj <- pricev
pricadj[, c("Open","High","Low","Close")] <-
  ratio*pricev[, c("Open","High","Low","Close")]
# Inspect the data
tsp(pricadj)  # frequency=1
head(time(pricadj))
head(pricadj)
tail(pricadj)
library(tseries)  # Load package tseries
# Download MSFT data
pricezoo <- suppressWarnings(
  get.hist.quote(
    instrument="MSFT",
    start=Sys.Date()-3*365,
    end=Sys.Date(),
    quote=c("Open","High","Low","Close",
      "AdjClose","Volume"),
    origin="1970-01-01")
)  # end suppressWarnings
load(file="/Users/jerzy/Develop/lecture_slides/data/zoo_data.RData")
class(pricezoo)
dim(pricezoo)
head(pricezoo, 4)
library(tseries)  # Load package tseries
load(file="/Users/jerzy/Develop/lecture_slides/data/zoo_data.RData")
# Calculate price adjustment vector
ratio <- as.numeric(pricezoo[, "AdjClose"]/pricezoo[, "Close"])
head(ratio, 5)
tail(ratio, 5)
# Adjust OHLC prices
pricedj <- pricezoo
pricedj[, c("Open","High","Low","Close")] <-
  ratio*pricezoo[, c("Open","High","Low","Close")]
head(pricedj)
tail(pricedj)
library(tseries)  # Load package tseries
# Download EUR/USD data
priceur <- suppressWarnings(
  get.hist.quote(
    instrument="EUR/USD",
    provider="oanda",
    start=Sys.Date()-3*365,
    end=Sys.Date(),
    origin="1970-01-01")
)  # end suppressWarnings
# Bind and scrub data
pricecombo <- cbind(priceur, pricezoo[, "AdjClose"])
colnames(pricecombo) <- c("EURUSD", "MSFT")
pricecombo <- pricecombo[complete.cases(pricecombo),]
save(pricezoo, pricedj,
     pricev, pricadj,
     priceur, pricecombo,
     file="/Users/jerzy/Develop/lecture_slides/data/zoo_data.RData")
load(file="/Users/jerzy/Develop/lecture_slides/data/zoo_data.RData")
# Inspect the data
class(priceur)
head(priceur, 4)
library(tseries)  # Load package tseries
# Download price and volume data for symbolv into list of zoo objects
pricev <- suppressWarnings(
  lapply(symbolv, # Loop for loading data
   get.hist.quote,
   quote=c("AdjClose", "Volume"),
   start=Sys.Date()-3650,
   end=Sys.Date(),
   origin="1970-01-01")  # end lapply
)  # end suppressWarnings
# Flatten list of zoo objects into a single zoo object
pricev <- rutils::do_call(cbind, pricev)
# Or
# pricev <- do.call(cbind, pricev)
# Assign names in format "symboln.Close", "symboln.Volume"
names(pricev) <- as.numeric(sapply(symbolv,
    paste, c("Close", "Volume"), sep="."))
# Save pricev to a comma-separated CSV file
write.zoo(pricev, file="zooseries.csv", sep=",")
# Save pricev to a binary .RData file
save(pricev, file="pricev.RData")
# Select ETF symbols for asset allocation
symbolv <- c("VTI", "VEU", "EEM", "XLY", "XLP", "XLE", "XLF",
 "XLV", "XLI", "XLB", "XLK", "XLU", "VYM", "IVW", "IWB", "IWD",
 "IWF", "IEF", "TLT", "VNQ", "DBC", "GLD", "USO", "VXX", "SVXY",
 "MTUM", "IVE", "VLUE", "QUAL", "VTV", "USMV", "AIEQ", "QQQ")
# Read etf database into data frame
etflist <- read.csv(file="/Users/jerzy/Develop/lecture_slides/data/etf_list.csv")
rownames(etflist) <- etflist$Symbol
# Select from etflist only those ETF's in symbolv
etflist <- etflist[symbolv, ]
# Shorten names
etfnames <- sapply(etflist$Name, function(name) {
  namesplit <- strsplit(name, split=" ")[[1]]
  namesplit <- namesplit[c(-1, -NROW(namesplit))]
  name_match <- match("Select", namesplit)
  if (!is.na(name_match))
    namesplit <- namesplit[-name_match]
  paste(namesplit, collapse=" ")
})  # end sapply
etflist$Name <- etfnames
etflist["IEF", "Name"] <- "10 year Treasury Bond Fund"
etflist["TLT", "Name"] <- "20 plus year Treasury Bond Fund"
etflist["XLY", "Name"] <- "Consumer Discr. Sector Fund"
etflist["EEM", "Name"] <- "Emerging Market Stock Fund"
etflist["MTUM", "Name"] <- "Momentum Factor Fund"
etflist["SVXY", "Name"] <- "Short VIX Futures"
etflist["VXX", "Name"] <- "Long VIX Futures"
etflist["DBC", "Name"] <- "Commodity Futures Fund"
etflist["USO", "Name"] <- "WTI Oil Futures Fund"
etflist["GLD", "Name"] <- "Physical Gold Fund"
print(xtable::xtable(etflist), comment=FALSE, size="tiny", include.rownames=FALSE)
# Select ETF symbols for asset allocation
symbolv <- c("VTI", "VEU", "EEM", "XLY", "XLP", "XLE", "XLF",
 "XLV", "XLI", "XLB", "XLK", "XLU", "VYM", "IVW", "IWB", "IWD",
 "IWF", "IEF", "TLT", "VNQ", "DBC", "GLD", "USO", "VXX", "SVXY",
 "MTUM", "IVE", "VLUE", "QUAL", "VTV", "USMV", "AIEQ", "QQQ")
library(rutils)  # Load package rutils
etfenv <- new.env()  # New environment for data
# Boolean vector of symbols already downloaded
isdown <- symbolv %in% ls(etfenv)
# Download data for symbolv using single command - creates pacing error
getSymbols.av(symbolv, adjust=TRUE, env=etfenv,
  output.size="full", api.key="T7JPW54ES8G75310")
# Download data from Alpha Vantage using while loop
nattempts <- 0  # number of download attempts
while ((sum(!isdown) > 0) & (nattempts < 10)) {
  # Download data and copy it into environment
  nattempts <- nattempts + 1
  cat("Download attempt = ", nattempts, "\n")
  for (symboln in na.omit(symbolv[!isdown][1:5])) {
    cat("Processing: ", symboln, "\n")
    tryCatch(  # With error handler
quantmod::getSymbols.av(symboln, adjust=TRUE, env=etfenv, auto.assign=TRUE, output.size="full", api.key="T7JPW54ES8G75310"),
# Error handler captures error condition
error=function(msg) {
  print(paste0("Error handler: ", msg))
},  # end error handler
finally=print(paste0("Symbol = ", symboln))
    )  # end tryCatch
  }  # end for
  # Update vector of symbols already downloaded
  isdown <- symbolv %in% ls(etfenv)
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
lapply(ls(), function(namev) class(get(namev)))
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
for (symboln in ls(etfenv)) {
  assign(symboln,
   adjustOHLC(get(symboln, envir=etfenv), use.Adjusted=TRUE),
   envir=etfenv)
}  # end for
library(rutils)  # Load package rutils
# Define ETF symbols
symbolv <- c("VTI", "VEU", "IEF", "VNQ")
# Extract symbolv from rutils::etfenv
pricev <- mget(symbolv, envir=rutils::etfenv)
# pricev is a list of xts series
class(pricev)
class(pricev[[1]])
tail(pricev[[1]])
# Extract close prices
pricev <- lapply(pricev, quantmod::Cl)
# Collapse list into time series the hard way
prices2 <- cbind(pricev[[1]], pricev[[2]], pricev[[3]], pricev[[4]])
class(pricev2)
dim(pricev2)
# Collapse list into time series using do.call()
pricev <- do.call(cbind, pricev)
all.equal(pricev2, pricev)
class(pricev)
dim(pricev)
# Or extract and cbind in single step
pricev <- do.call(cbind, lapply(
  mget(symbolv, envir=rutils::etfenv), quantmod::Cl))
# Or extract and bind all data, subset by symbolv
pricev <- lapply(symbolv, function(symboln) {
    quantmod::Cl(get(symboln, envir=rutils::etfenv))
})  # end lapply
# Or loop over etfenv without anonymous function
pricev <- do.call(cbind,
  lapply(as.list(rutils::etfenv)[symbolv], quantmod::Cl))
# Same, but works only for OHLC series - produces error
pricev <- do.call(cbind,
  eapply(rutils::etfenv, quantmod::Cl)[symbolv])
# Column names end with ".Close"
colnames(pricev)
strsplit(colnames(pricev), split="[.]")
do.call(rbind, strsplit(colnames(pricev), split="[.]"))
do.call(rbind, strsplit(colnames(pricev), split="[.]"))[, 1]
# Drop ".Close" from colnames
colnames(pricev) <- rutils::get_name(colnames(pricev))
# Or
# colnames(pricev) <- do.call(rbind,
#   strsplit(colnames(pricev), split="[.]"))[, 1]
tail(pricev, 3)
# Which objects in global environment are class xts?
unlist(eapply(globalenv(), is.xts))
# Save xts to csv file
write.zoo(pricev,
  file="/Users/jerzy/Develop/lecture_slides/data/etf_series.csv", sep=",")
# Copy prices into etfenv
etfenv$prices <- pricev
# Or
assign("pricev", pricev, envir=etfenv)
# Save to .RData file
save(etfenv, file="etf_data.RData")
# Extract VTI prices
pricev <- etfenv$prices[ ,"VTI"]
pricev <- na.omit(pricev)
# Calculate percentage returns "by hand"
pricel <- as.numeric(pricev)
pricel <- c(pricel[1], pricel[-NROW(pricel)])
pricel <- xts(pricel, zoo::index(pricev))
retp <- (pricev-pricel)/pricel
# Calculate percentage returns using dailyReturn()
retd <- quantmod::dailyReturn(pricev)
head(cbind(retd, retp))
all.equal(retd, retp, check.attributes=FALSE)
# Calculate returns for all prices in etfenv$prices
retp <- lapply(etfenv$prices, function(xtsv) {
  retd <- quantmod::dailyReturn(na.omit(xtsv))
  colnames(retd) <- names(xtsv)
  retd
})  # end lapply
# "retp" is a list of xts
class(retp)
class(retp[[1]])
# Flatten list of xts into a single xts
retp <- do.call(cbind, retp)
class(retp)
dim(retp)
# Copy retp into etfenv and save to .RData file
# assign("retp", retp, envir=etfenv)
etfenv$retp <- retp
save(etfenv, file="/Users/jerzy/Develop/lecture_slides/data/etf_data.RData")
library(rutils)
startd <- "2012-05-10"; endd <- "2013-11-20"
# Select all objects in environment and return as environment
newenv <- as.environment(eapply(etfenv, "[",
            paste(startd, endd, sep="/")))
# Select only symbolv in environment and return as environment
newenv <- as.environment(
  lapply(as.list(etfenv)[symbolv], "[",
   paste(startd, endd, sep="/")))
# Extract and cbind Close prices and return to environment
assign("prices", rutils::do_call(cbind,
  lapply(ls(etfenv), function(symboln) {
    xtsv <- quantmod::Cl(get(symboln, etfenv))
    colnames(xtsv) <- symboln
    xtsv
  })), envir=newenv)
# Get sizes of OHLC xts series in etfenv
sapply(mget(symbolv, envir=etfenv), object.size)
# Extract and cbind adjusted prices and return to environment
colname <- function(xtsv)
  strsplit(colnames(xtsv), split="[.]")[[1]][1]
assign("prices", rutils::do_call(cbind,
         lapply(mget(etfenv$symbolv, envir=etfenv),
                function(xtsv) {
                  xtsv <- Ad(xtsv)
                  colnames(xtsv) <- colname(xtsv)
                  xtsv
         })), envir=newenv)
# Load data frame of S&P500 constituents from CSV file
sp500 <- read.csv(file="/Users/jerzy/Develop/lecture_slides/data/sp500_constituents.csv")
# Inspect data frame of S&P500 constituents
dim(sp500)
colnames(sp500)
# Extract tickers from the column Ticker
symbolv <- sp500$Ticker
# Get duplicate tickers
tablev <- table(symbolv)
duplicatv <- tablev[tablev > 1]
duplicatv <- names(duplicatv)
# Get duplicate records (rows) of sp500
sp500[symbolv %in% duplicatv, ]
# Get unique tickers
symbolv <- unique(symbolv)
# Find index of ticker "BRK.B"
which(symbolv=="BRK.B")
# Rename "BRK.B" to "BRK-B" and "BF.B" to "BF-B"
symbolv[which(symbolv=="BRK.B")] <- "BRK-B"
symbolv[which(symbolv=="BF.B")] <- "BF-B"
# Load package rutils
library(rutils)
# Create new environment for data
sp500env <- new.env()
# Boolean vector of symbols already downloaded
isdown <- symbolv %in% ls(sp500env)
# Download in while loop from Tiingo and copy into environment
nattempts <- 0  # Number of download attempts
while ((sum(!isdown) > 0) & (nattempts<3)) {
  # Download data and copy it into environment
  nattempts <- nattempts + 1
  cat("Download attempt = ", nattempts, "\n")
  for (symboln in symbolv[!isdown]) {
    cat("processing: ", symboln, "\n")
    tryCatch(  # With error handler
quantmod::getSymbols(symboln, src="tiingo", adjust=TRUE, auto.assign=TRUE,
           from="1990-01-01", env=sp500env, api.key="j84ac2b9c5bde2d68e33034f65d838092c6c9f10"),
# Error handler captures error condition
error=function(msg) {
  print(paste0("Error handler: ", msg))
},  # end error handler
finally=print(paste0("Symbol = ", symboln))
    )  # end tryCatch
  }  # end for
  # Update vector of symbols already downloaded
  isdown <- symbolv %in% ls(sp500env)
  Sys.sleep(2)  # Wait 2 seconds until next attempt
}  # end while
class(sp500env$AAPL)
class(zoo::index(sp500env$AAPL))
tail(sp500env$AAPL)
symbolv[!isdown]
# The date-time index of AAPL is POSIXct
class(zoo::index(sp500env$AAPL))
# Coerce the date-time index of AAPL to Date
zoo::index(sp500env$AAPL) <- as.Date(zoo::index(sp500env$AAPL))
# Coerce all the date-time indices to Date
for (symboln in ls(sp500env)) {
  ohlc <- get(symboln, envir=sp500env)
  zoo::index(ohlc) <- as.Date(zoo::index(ohlc))
  assign(symboln, ohlc, envir=sp500env)
}  # end for
# "LOW.Low" is a bad column name
colnames(sp500env$LOW)
strsplit(colnames(sp500env$LOW), split="[.]")
do.call(cbind, strsplit(colnames(sp500env$LOW), split="[.]"))
do.call(cbind, strsplit(colnames(sp500env$LOW), split="[.]"))[2, ]
# Extract proper names from column names
namev <- rutils::get_name(colnames(sp500env$LOW), field=2)
# Or
# namev <- do.call(rbind, strsplit(colnames(sp500env$LOW),
#                                   split="[.]"))[, 2]
# Rename "LOW" colnames to "LOWES"
colnames(sp500env$LOW) <- paste("LOVES", namev, sep=".")
sp500env$LOWES <- sp500env$LOW
rm(LOW, envir=sp500env)
# Rename BF-B colnames to "BFB"
colnames(sp500env$"BF-B") <- paste("BFB", namev, sep=".")
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
# colnames(BRKB) <- paste("BRKB", namev, sep=".")
# sp500env$BRKB <- BRKB
# Plot OHLC candlestick chart for LOWES
chart_Series(x=sp500env$LOWES["2019-12/"],
  TA="add_Vo()", name="LOWES OHLC Stock Prices")
# Plot dygraph
dygraphs::dygraph(sp500env$LOWES["2019-12/", -5], main="LOWES OHLC Stock Prices") %>%
  dyCandlestick()
# Load S&P500 constituent stock prices
load("/Users/jerzy/Develop/lecture_slides/data/sp500.RData")
pricev <- eapply(sp500env, quantmod::Cl)
pricev <- rutils::do_call(cbind, pricev)
# Carry forward non-NA prices
pricev <- zoo::na.locf(pricev, na.rm=FALSE)
# Drop ".Close" from column names
colnames(pricev)
colnames(pricev) <- rutils::get_name(colnames(pricev))
# Or
# colnames(pricev) <- do.call(rbind,
#   strsplit(colnames(pricev), split="[.]"))[, 1]
# Calculate percentage returns of the S&P500 constituent stocks
# retp <- xts::diff.xts(log(pricev))
retp <- xts::diff.xts(pricev)/
  rutils::lagit(pricev, pad_zeros=FALSE)
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
samplev <- sample(NCOL(retp), s=100, replace=FALSE)
prices100 <- pricev[, samplev]
returns100 <- retp[, samplev]
save(pricev, prices100,
  file="/Users/jerzy/Develop/lecture_slides/data/sp500_prices.RData")
save(retp, returns100,
  file="/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
# Calculate number of constituents without prices
datav <- rowSums(is.na(pricev))
datav <- xts::xts(datav, order.by=zoo::index(pricev))
dygraphs::dygraph(datav, main="Number of S&P500 Constituents Without Prices") %>%
  dyOptions(colors="blue", strokeWidth=2)
# Calculate price weighted index of constituent
ncols <- NCOL(pricev)
pricev <- zoo::na.locf(pricev, fromLast=TRUE)
indeks <- xts(rowSums(pricev)/ncols, zoo::index(pricev))
colnames(indeks) <- "index"
# Combine index with VTI
datav <- cbind(indeks[zoo::index(etfenv$VTI)], etfenv$VTI[, 4])
colnamev <- c("index", "VTI")
colnames(datav) <- colnamev
# Plot index with VTI
endd <- rutils::calc_endpoints(datav, interval="weeks")
dygraphs::dygraph(log(datav)[endd],
  main="S&P 500 Price-weighted Index and VTI") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="red") %>%
  dySeries(name=colnamev[2], axis="y2", col="blue")
# Save the environment to compressed .RData file
dirn <- "/Users/jerzy/Develop/lecture_slides/data/"
save(sp500env, file=paste0(dirn, "sp500.RData"))
# Save the ETF prices into CSV files
dirn <- "/Users/jerzy/Develop/lecture_slides/data/SP500/"
for (symboln in ls(sp500env)) {
  zoo::write.zoo(sp500env$symbol, file=paste0(dirn, symboln, ".csv"))
}  # end for
# Or using lapply()
filens <- lapply(ls(sp500env), function(symboln) {
  xtsv <- get(symboln, envir=sp500env)
  zoo::write.zoo(xtsv, file=paste0(dirn, symboln, ".csv"))
  symboln
})  # end lapply
unlist(filens)
# Or using eapply() and data.table::fwrite()
filens <- eapply(sp500env , function(xtsv) {
  filen <- rutils::get_name(colnames(xtsv)[1])
  data.table::fwrite(data.table::as.data.table(xtsv), file=paste0(dirn, filen, ".csv"))
  filen
})  # end eapply
unlist(filens)
# Load the environment from compressed .RData file
dirn <- "/Users/jerzy/Develop/lecture_slides/data/"
load(file=paste0(dirn, "sp500.RData"))
# Get all the .csv file names in the directory
dirn <- "/Users/jerzy/Develop/lecture_slides/data/SP500/"
filens <- Sys.glob(paste0(dirn, "*.csv"))
# Create new environment for data
sp500env <- new.env()
for (filen in filens) {
  xtsv <- xts::as.xts(zoo::read.csv.zoo(filen))
  symboln <- rutils::get_name(colnames(xtsv)[1])
  # symboln <- strsplit(colnames(xtsv), split="[.]")[[1]][1]
  assign(symboln, xtsv, envir=sp500env)
}  # end for
# Or using fread()
for (filen in filens) {
  xtsv <- data.table::fread(filen)
  data.table::setDF(xtsv)
  xtsv <- xts::xts(xtsv[, -1], as.Date(xtsv[, 1]))
  symboln <- rutils::get_name(colnames(xtsv)[1])
  assign(symboln, xtsv, envir=sp500env)
}  # end for
# Remove all files from environment(if necessary)
rm(list=ls(sp500env), envir=sp500env)
# Download in while loop from Alpha Vantage and copy into environment
isdown <- symbolv %in% ls(sp500env)
nattempts <- 0
while ((sum(!isdown) > 0) & (nattempts < 10)) {
  # Download data and copy it into environment
  nattempts <- nattempts + 1
  for (symboln in symbolv[!isdown]) {
    cat("processing: ", symboln, "\n")
    tryCatch(  # With error handler
quantmod::getSymbols(symboln, src="av", adjust=TRUE, auto.assign=TRUE, env=sp500env,
           output.size="full", api.key="T7JPW54ES8G75310"),
# error handler captures error condition
error=function(msg) {
  print(paste0("Error handler: ", msg))
},  # end error handler
finally=print(paste0("Symbol = ", symboln))
    )  # end tryCatch
  }  # end for
  # Update vector of symbols already downloaded
  isdown <- symbolv %in% ls(sp500env)
  Sys.sleep(2)  # Wait 2 seconds until next attempt
}  # end while
# Adjust all OHLC prices in environment
for (symboln in ls(sp500env)) {
  assign(symboln,
    adjustOHLC(get(x=symboln, envir=sp500env), use.Adjusted=TRUE),
    envir=sp500env)
}  # end for
library(rutils)  # Load package rutils
# Assign name SP500 to ^GSPC symbol
quantmod::setSymbolLookup(SP500=list(name="^GSPC", src="yahoo"))
quantmod::getSymbolLookup()
# View and clear options
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
pricev <- eapply(sp500env, quantmod::Cl)
pricev <- rutils::do_call(cbind, pricev)
# Carry forward non-NA prices
pricev <- zoo::na.locf(pricev, na.rm=FALSE)
# Get first column name
colnames(pricev[, 1])
rutils::get_name(colnames(pricev[, 1]))
# Modify column names
colnames(pricev) <- rutils::get_name(colnames(pricev))
# Or
# colnames(pricev) <- do.call(rbind,
#   strsplit(colnames(pricev), split="[.]"))[, 1]
# Calculate percentage returns
retp <- xts::diff.xts(pricev)/
  rutils::lagit(pricev, pad_zeros=FALSE)
# Select a random sample of 100 prices and returns
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
samplev <- sample(NCOL(retp), s=100, replace=FALSE)
prices100 <- pricev[, samplev]
returns100 <- retp[, samplev]
# Save the data into binary files
save(pricev, prices100,
     file="/Users/jerzy/Develop/lecture_slides/data/sp500_prices.RData")
save(retp, returns100,
     file="/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
# Setup code
symboln <- "SPY"
startd <- as.Date("1990-01-01")
todayd <- Sys.Date()
tspan <- "day"
# Replace below your own Polygon API key
apikey <- "SEpnsBpiRyONMJdl48r6dOo0_pjmCu5r"
# Create url for download
urll <- paste0("https://api.polygon.io/v2/aggs/ticker/", symboln, "/range/1/", tspan, "/", startd, "/", todayd, "?adjusted=true&sort=asc&limit=50000&apiKey=", apikey)
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
datev <- ohlc[, "t"]/1e3
datev <- as.POSIXct(datev, origin="1970-01-01")
datev <- as.Date(datev)
tail(datev)
# Coerce from matrix to xts
ohlc <- ohlc[, c("o","h","l","c","v","vw")]
colnames(ohlc) <- c("Open", "High", "Low", "Close", "Volume", "VWAP")
ohlc <- xts::xts(ohlc, order.by=datev)
tail(ohlc)
# Save the xts time series to compressed RData file
save(ohlc, file="/Users/jerzy/Data/spy_daily.RData")
# Candlestick plot of SPY OHLC prices
dygraphs::dygraph(ohlc[, 1:4], main=paste("Candlestick Plot of", symboln, "OHLC prices")) %>%
  dygraphs::dyCandlestick()
# Select ETF symbols for asset allocation
symbolv <- c("VTI", "VEU", "EEM", "XLY", "XLP", "XLE", "XLF",
 "XLV", "XLI", "XLB", "XLK", "XLU", "VYM", "IVW", "IWB", "IWD",
 "IWF", "IEF", "TLT", "VNQ", "DBC", "GLD", "USO", "VXX", "SVXY",
 "MTUM", "IVE", "VLUE", "QUAL", "VTV", "USMV", "AIEQ", "QQQ")
# Setup code
etfenv <- new.env()  # New environment for data
# Boolean vector of symbols already downloaded
isdown <- symbolv %in% ls(etfenv)
# Download data from Polygon using while loop
while (sum(!isdown) > 0) {
  for (symboln in symbolv[!isdown]) {
    cat("Processing:", symboln, "\n")
    tryCatch({  # With error handler
# Download OHLC bars from Polygon into JSON format file
urll <- paste0("https://api.polygon.io/v2/aggs/ticker/", symboln, "/range/1/", tspan, "/", startd, "/", todayd, "?adjusted=true&sort=asc&limit=50000&apiKey=", apikey)
ohlc <- jsonlite::read_json(urll)
# Extract list of prices from json object
ohlc <- ohlc$results
# Coerce from list to matrix
ohlc <- lapply(ohlc, unlist)
ohlc <- do.call(rbind, ohlc)
# Coerce time from milliseconds to dates
datev <- ohlc[, "t"]/1e3
datev <- as.POSIXct(datev, origin="1970-01-01")
datev <- as.Date(datev)
# Coerce from matrix to xts
ohlc <- ohlc[, c("o","h","l","c","v","vw")]
colnames(ohlc) <- paste0(symboln, ".", c("Open", "High", "Low", "Close", "Volume", "VWAP"))
ohlc <- xts::xts(ohlc, order.by=datev)
# Save to environment
assign(symboln, ohlc, envir=etfenv)
Sys.sleep(1)
},
    error={function(msg) print(paste0("Error handler: ", msg))},
    finally=print(paste0("Symbol = ", symboln))
    )  # end tryCatch
  }  # end for
  # Update vector of symbols already downloaded
  isdown <- symbolv %in% ls(etfenv)
}  # end while
save(etfenv, file="/Users/jerzy/Develop/lecture_slides/data/etf_data.RData")
# Extract Close prices
prices <- eapply(etfenv, quantmod::Cl)
prices <- do.call(cbind, prices)
# Drop ".Close" from colnames
colnames(prices) <- do.call(rbind, strsplit(colnames(prices), split="[.]"))[, 1]
# Calculate the log returns
retp <- xts::diff.xts(log(prices))
# Copy prices and returns into etfenv
etfenv$prices <- prices
etfenv$retp <- retp
# Copy symbolv into etfenv
etfenv$symbolv <- symbolv
# Calculate the risk-return statistics
riskstats <- PerformanceAnalytics::table.Stats(retp)
# Transpose the data frame
riskstats <- as.data.frame(t(riskstats))
# Add Name column
riskstats$Name <- rownames(riskstats)
# Copy riskstats into etfenv
etfenv$riskstats <- riskstats
# Calculate the beta, alpha, Treynor ratio, and other performance statistics
capmstats <- PerformanceAnalytics::table.CAPM(Ra=retp[, symbolv],
                                         Rb=retp[, "VTI"], scale=252)
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
sp500$namev <- gsub("-", "_", sp500$Ticker)
sp500$namev <- gsub("[.]", "_", sp500$names)
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
for (symboln in sp500$names) {
  cat("processing: ", symboln, "\n")
  rutils::get_data(symboln,
   env_out=sp500env, startd="1990-01-01")
}  # end for
save(sp500env, file="/Users/jerzy/Develop/lecture_slides/data/sp500.RData")
chart_Series(x=sp500env$BRKB["2016/"],
       TA="add_Vo()", name="BRK-B stock")
# Download U.S. unemployment rate data
unrate <- quantmod::getSymbols("UNRATE",
   auto.assign=FALSE, src="FRED")
# Plot U.S. unemployment rate data
dygraphs::dygraph(unrate["1990/"], main="U.S. Unemployment Rate") %>%
  dyOptions(colors="blue", strokeWidth=2)
# Or
quantmod::chart_Series(unrate["1990/"], name="U.S. Unemployment Rate")
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
pricev <- Quandl(code="WIKI/AAPL",
  type="xts", startd="1990-01-01")
x11(width=14, height=7)
chart_Series(pricev["2016", 1:4], name="AAPL OHLC prices")
# Add trade volume in extra panel
add_TA(pricev["2016", 5])
# Download euro currency rates
pricev <- Quandl(code="BNP/USDEUR",
    startd="2013-01-01",
    endd="2013-12-01", type="xts")
# Download multiple time series
pricev <- Quandl(code=c("NSE/OIL", "WIKI/AAPL"),
    startd="2013-01-01", type="xts")
# Download AAPL gross profits
prof_it <- Quandl("RAYMOND/AAPL_GROSS_PROFIT_Q", type="xts")
chart_Series(prof_it, name="AAPL gross profits")
# Download Hurst time series
pricev <- Quandl(code="PE/AAPL_HURST",
    startd="2013-01-01", type="xts")
chart_Series(pricev["2016/", 1], name="AAPL Hurst")
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
isdown <- tickers %in% ls(sp500env)
# Download data and copy it into environment
for (ticker in tickers[!isdown]) {
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
pricev <- Quandl(code="CHRIS/CME_ES1",
  type="xts", startd="1990-01-01")
pricev <- pricev[, c("Open", "High", "Low", "Last", "Volume")]
colnames(pricev)[4] <- "Close"
# Plot the prices
x11(width=5, height=4)  # Open x11 for plotting
chart_Series(x=pricev["2008-06/2009-06"],
       TA="add_Vo()", name="S&P500 Futures")
# Plot dygraph
dygraphs::dygraph(pricev["2008-06/2009-06", -5],
  main="S&P500 Futures") %>%
  dyCandlestick()
# Read CBOE futures expiration dates
datev <- read.csv(file="/Users/jerzy/Develop/lecture_slides/data/futures_expiration_dates_codes.csv",
  row.names=1)
dirn <- "/Users/jerzy/Develop/data/vix_data"
dir.create(dirn)
symbolv <- rownames(datev)
filens <- file.path(dirn, paste0(symbolv, ".csv"))
log_file <- file.path(dirn, "log_file.txt")
cboe_url <- "https://markets.cboe.com/us/futures/market_statistics/historical_data/products/csv/VX/"
urls <- paste0(cboe_url, datev[, 1])
# Download files in loop
for (it in seq_along(urls)) {
    tryCatch(  # Warning and error handler
  download.file(urls[it],
          destfile=filens[it], quiet=TRUE),
# Warning handler captures warning condition
warning=function(msg) {
  cat(paste0("Warning handler: ", msg, "\n"), file=log_file, append=TRUE)
},  # end warning handler
# Error handler captures error condition
error=function(msg) {
  cat(paste0("Error handler: ", msg, "\n"), append=TRUE)
},  # end error handler
finally=cat(paste0("Processing file name = ", filens[it], "\n"), append=TRUE)
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
#Perform calculations in R,
#And export to CSV files
setwd("/Users/jerzy/Develop/lecture_slides/data")
# Read data frame, with row names from first column
readf <- read.csv(file="florist.csv", row.names=1)
# Subset data frame
readf <- readf[readf[, "type"]=="daisy", ]
all.equal(readf, dframe)
# Write data frame to CSV file, with row names
write.csv(readf, file="daisies.csv")
#Perform calculations in R,
#And export to CSV files
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
