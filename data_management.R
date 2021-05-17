library(knitr)
opts_chunk$set(prompt=TRUE, eval=FALSE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='tiny', fig.width=6, fig.height=5)
options(width=80, dev='pdf')
options(digits=3)
thm <- knit_theme$get("acid")
knit_theme$set(thm)

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

setwd("C:/Develop/lecture_slides/data")
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

setwd("C:/Develop/lecture_slides/data")
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

setwd("C:/Develop/lecture_slides/data")
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

setwd("C:/Develop/lecture_slides/data")
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
zoo_read <- read.table(file="C:/Develop/lecture_slides/data/es_ohlc.csv",
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

pdf("Rgraph.pdf", width=7, height=4)  # Redirect graphics to pdf file

cat("Redirect data from R into pdf file\n")
my_var <- seq(-2*pi, 2*pi, len=100)
plot(x=my_var, y=sin(my_var), main="Sine wave",
   xlab="", ylab="", type="l", lwd=2, col="red")
cat("\nEnd data\nbye\n")

dev.off()  # turn pdf output off

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
# column referenced without quotes
data_table[, col2]
# row referenced without a following comma
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
dir_name <- "C:/Develop/lecture_slides/data/"
file_name <- file.path(dir_name, "weather_delays14.csv")
data_table <- data.table::fread(file_name)
class(data_table); dim(data_table)
data_table
# fread() reads the same data as read.csv()
all.equal(read.csv(file_name),
    setDF(data.table::fread(file_name)))
# fread() is much faster than read.csv()
summary(microbenchmark(
  pure_r=read.csv(file_name),
  fread=setDF(data.table::fread(file_name)),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Write data table to file in different ways
data.table::fwrite(data_table, file="data_table.csv")
write.csv(data_table, file="data_table2.csv")
cat(unlist(data_table), file="data_table3.csv")
# microbenchmark speed of data.table::fwrite()
library(microbenchmark)
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
da_ta <- runif(1e3)
all.equal(sort(da_ta), data.table::fsort(da_ta))
library(microbenchmark)
summary(microbenchmark(
  pure_r=sort(da_ta),
  dt=data.table::fsort(da_ta),
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
dir_name <- "C:/Develop/lecture_slides/data/"
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
# reference to .fst can be treated similar to a data table
dim(t_aq); dim(fs_t)
fst:::print.fst_table(fs_t)
# Subset reference to .fst just like a data table
fs_t[1e4:(1e4+5), ]

load(file="C:/Develop/lecture_slides/data/zoo_data.RData")
library(tseries)  # Load package tseries
# Download MSFT data in ts format
ts_stx <- suppressWarnings(
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
fac_tor <- as.numeric(ts_stx[, "AdjClose"]/ts_stx[, "Close"])
# Adjust OHLC prices
ts_stx_adj <- ts_stx
ts_stx_adj[, c("Open","High","Low","Close")] <-
  fac_tor*ts_stx[, c("Open","High","Low","Close")]
# Inspect the data
tsp(ts_stx_adj)  # frequency=1
head(time(ts_stx_adj))
head(ts_stx_adj)
tail(ts_stx_adj)

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

load(file="C:/Develop/lecture_slides/data/zoo_data.RData")
class(zoo_stx)
dim(zoo_stx)
head(zoo_stx, 4)

library(tseries)  # Load package tseries
load(file="C:/Develop/lecture_slides/data/zoo_data.RData")
# Calculate price adjustment vector
fac_tor <- as.numeric(zoo_stx[, "AdjClose"]/zoo_stx[, "Close"])
head(fac_tor, 5)
tail(fac_tor, 5)
# Adjust OHLC prices
zoo_stx_adj <- zoo_stx
zoo_stx_adj[, c("Open","High","Low","Close")] <-
  fac_tor*zoo_stx[, c("Open","High","Low","Close")]
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
     ts_stx, ts_stx_adj,
     zoo_eurusd, zoo_stxeur,
     file="C:/Develop/lecture_slides/data/zoo_data.RData")

load(file="C:/Develop/lecture_slides/data/zoo_data.RData")
# Inspect the data
class(zoo_eurusd)
head(zoo_eurusd, 4)

library(tseries)  # Load package tseries
# Download price and volume data for sym_bols into list of zoo objects
zoo_series <- suppressWarnings(
  lapply(sym_bols, # Loop for loading data
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
names(zoo_series) <- as.numeric(sapply(sym_bols,
    paste, c("Close", "Volume"), sep="."))
# Save zoo_series to a comma-separated CSV file
write.zoo(zoo_series, file="zoo_series.csv", sep=",")
# Save zoo_series to a binary .RData file
save(zoo_series, file="zoo_series.RData")

# Select ETF symbols for asset allocation
sym_bols <- c("VTI", "VEU", "EEM", "XLY", "XLP", "XLE", "XLF",
  "XLV", "XLI", "XLB", "XLK", "XLU", "VYM", "IVW", "IWB", "IWD",
  "IWF", "IEF", "TLT", "VNQ", "DBC", "GLD", "USO", "VXX", "SVXY",
  "MTUM", "IVE", "VLUE", "QUAL", "VTV", "USMV")
library(rutils)  # Load package rutils
etf_env <- new.env()  # New environment for data
# Boolean vector of symbols already downloaded
down_loaded <- sym_bols %in% ls(etf_env)
# Download data for sym_bols using single command - creates pacing error
getSymbols.av(sym_bols, adjust=TRUE, env=etf_env,
  output.size="full", api.key="T7JPW54ES8G75310")
# Download data from Alpha Vantage using while loop
at_tempt <- 0  # number of download attempts
while (((sum(!down_loaded)) > 0) & (at_tempt<10)) {
  # Download data and copy it into environment
  at_tempt <- at_tempt + 1
  cat("Download attempt = ", at_tempt, "\n")
  for (sym_bol in na.omit(sym_bols[!down_loaded][1:5])) {
    cat("Processing: ", sym_bol, "\n")
    tryCatch(  # With error handler
quantmod::getSymbols.av(sym_bol, adjust=TRUE, env=etf_env, auto.assign=TRUE, output.size="full", api.key="T7JPW54ES8G75310"),
# Error handler captures error condition
error=function(error_cond) {
  print(paste("error handler: ", error_cond))
},  # end error handler
finally=print(paste("sym_bol=", sym_bol))
    )  # end tryCatch
  }  # end for
  # Update vector of symbols already downloaded
  down_loaded <- sym_bols %in% ls(etf_env)
  cat("Pausing 1 minute to avoid pacing...\n")
  Sys.sleep(65)
}  # end while
# Download all sym_bols using single command - creates pacing error
# quantmod::getSymbols.av(sym_bols, env=etf_env, adjust=TRUE, from="2005-01-03", output.size="full", api.key="T7NHW54ES8GG501C")

ls(etf_env)  # List files in etf_env
# Get class of object in etf_env
class(get(x=sym_bols[1], envir=etf_env))
# Another way
class(etf_env$VTI)
colnames(etf_env$VTI)
# Get first 3 rows of data
head(etf_env$VTI, 3)
# Get last 11 rows of data
tail(etf_env$VTI, 11)
# Get class of all objects in etf_env
eapply(etf_env, class)
# Get class of all objects in R workspace
lapply(ls(), function(ob_ject) class(get(ob_ject)))
# Get end dates of all objects in etf_env
as.Date(sapply(etf_env, end))

library(rutils)  # Load package rutils
# Check of object is an OHLC time series
is.OHLC(etf_env$VTI)
# Adjust single OHLC object using its name
etf_env$VTI <- adjustOHLC(etf_env$VTI, use.Adjusted=TRUE)

# Adjust OHLC object using string as name
assign(sym_bols[1], adjustOHLC(
    get(x=sym_bols[1], envir=etf_env), use.Adjusted=TRUE),
  envir=etf_env)

# Adjust objects in environment using vector of strings
for (sym_bol in ls(etf_env)) {
  assign(sym_bol,
   adjustOHLC(get(sym_bol, envir=etf_env), use.Adjusted=TRUE),
   envir=etf_env)
}  # end for

library(rutils)  # Load package rutils
# Define ETF symbols
sym_bols <- c("VTI", "VEU", "IEF", "VNQ")
# Extract sym_bols from rutils::etf_env
price_s <- mget(sym_bols, envir=rutils::etf_env)
# price_s is a list of xts series
class(price_s)
class(price_s[[1]])
tail(price_s[[1]])
# Extract Close prices
price_s <- lapply(price_s, quantmod::Cl)
# Collapse list into time series the hard way
xts_1 <- cbind(price_s[[1]], price_s[[2]],
         price_s[[3]], price_s[[4]])
class(xts_1)
dim(xts_1)
# Collapse list into time series using do.call()
price_s <- do.call(cbind, price_s)
all.equal(xts_1, price_s)
class(price_s)
dim(price_s)
# Or extract and cbind in single step
price_s <- do.call(cbind, lapply(
  mget(sym_bols, envir=rutils::etf_env), quantmod::Cl))
# Or extract and bind all data, subset by sym_bols
price_s <- lapply(sym_bols, function(sym_bol) {
    quantmod::Cl(get(sym_bol, envir=rutils::etf_env))
})  # end lapply
# Or loop over etf_env without anonymous function
price_s <- do.call(cbind,
  lapply(as.list(rutils::etf_env)[sym_bols], quantmod::Cl))
# Same, but works only for OHLC series - produces error
price_s <- do.call(cbind,
  eapply(rutils::etf_env, quantmod::Cl)[sym_bols])

# Column names end with ".Close"
colnames(price_s)
strsplit(colnames(price_s), split="[.]")
do.call(rbind, strsplit(colnames(price_s), split="[.]"))
do.call(rbind, strsplit(colnames(price_s), split="[.]"))[, 1]
# Drop ".Close" from colnames
colnames(price_s) <- rutils::get_name(colnames(price_s))
# Or
# colnames(price_s) <- do.call(rbind,
#   strsplit(colnames(price_s), split="[.]"))[, 1]
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

# Extract VTI prices
vt_i <- etf_env$price_s[ ,"VTI"]
vt_i <- na.omit(vt_i)
# Calculate percentage returns "by hand"
vti_lag <- as.numeric(vt_i)
vti_lag <- c(vti_lag[1], vti_lag[-NROW(vti_lag)])
vti_lag <- xts(vti_lag, index(vt_i))
vti_returns <- (vt_i-vti_lag)/vti_lag
# Calculate percentage returns using dailyReturn()
daily_returns <- quantmod::dailyReturn(vt_i)
head(cbind(daily_returns, vti_returns))
all.equal(daily_returns, vti_returns, check.attributes=FALSE)
# Calculate returns for all prices in etf_env$price_s
re_turns <- lapply(etf_env$price_s, function(x_ts) {
  daily_returns <- quantmod::dailyReturn(na.omit(x_ts))
  colnames(daily_returns) <- names(x_ts)
  daily_returns
})  # end lapply
# "re_turns" is a list of xts
class(re_turns)
class(re_turns[[1]])
# Flatten list of xts into a single xts
re_turns <- do.call(cbind, re_turns)
class(re_turns)
dim(re_turns)
# Copy re_turns into etf_env and save to .RData file
# assign("re_turns", re_turns, envir=etf_env)
etf_env$re_turns <- re_turns
save(etf_env, file="C:/Develop/lecture_slides/data/etf_data.RData")

library(quantmod)
start_date <- "2012-05-10"; end_date <- "2013-11-20"
# Select all objects in environment and return as environment
new_env <- as.environment(eapply(etf_env, "[",
            paste(start_date, end_date, sep="/")))
# Select only sym_bols in environment and return as environment
new_env <- as.environment(
  lapply(as.list(etf_env)[sym_bols], "[",
   paste(start_date, end_date, sep="/")))
# Extract and cbind Close prices and return to environment
assign("price_s", rutils::do_call(cbind,
         lapply(ls(etf_env), function(sym_bol) {
           x_ts <- quantmod::Cl(get(sym_bol, etf_env))
           colnames(x_ts) <- sym_bol
           x_ts
         })), envir=new_env)
# Get sizes of OHLC xts series in etf_env
sapply(mget(sym_bols, envir=etf_env), object.size)
# Extract and cbind adjusted prices and return to environment
col_name <- function(x_ts)
  strsplit(colnames(x_ts), split="[.]")[[1]][1]
assign("price_s", rutils::do_call(cbind,
         lapply(mget(etf_env$sym_bols, envir=etf_env),
                function(x_ts) {
                  x_ts <- Ad(x_ts)
                  colnames(x_ts) <- col_name(x_ts)
                  x_ts
         })), envir=new_env)

# Load data frame of S&P500 constituents from CSV file
sp_500 <- read.csv(file="C:/Develop/lecture_slides/data/sp500_WRDS_08-30-17.csv")
# Inspect data frame of S&P500 constituents
dim(sp_500)
colnames(sp_500)
# Extract tickers from the column co_tic
sym_bols <- sp_500$co_tic
# Get duplicate tickers
ta_ble <- table(sym_bols)
dupli_cate <- ta_ble[ta_ble>1]
dupli_cate <- names(dupli_cate)
# Get duplicate records (rows) of sp_500
sp_500[sym_bols %in% dupli_cate, ]
# Get unique tickers
sym_bols <- unique(sym_bols)
# Find index of ticker "BRK.B"
which(sym_bols=="BRK.B")
# Remove "BRK.B" and later download it separately
sym_bols <- sym_bols[-which(sym_bols=="BRK.B")]

# Load package rutils
library(rutils)
# Create new environment for data
sp500_env <- new.env()
# Boolean vector of symbols already downloaded
down_loaded <- sym_bols %in% ls(sp500_env)
# Download in while loop from Tiingo and copy into environment
at_tempt <- 0  # number of download attempts
while (((sum(!down_loaded)) > 0) & (at_tempt<5)) {
  # Download data and copy it into environment
  at_tempt <- at_tempt + 1
  cat("Download attempt = ", at_tempt, "\n")
  for (sym_bol in sym_bols[!down_loaded]) {
    cat("processing: ", sym_bol, "\n")
    tryCatch(  # With error handler
quantmod::getSymbols(sym_bol, src="tiingo", adjust=TRUE, auto.assign=TRUE,
           from="1990-01-01", env=sp500_env, api.key="j84ac2b9c5bde2d68e33034f65d838092c6c9f10"),
# Error handler captures error condition
error=function(error_cond) {
  print(paste("error handler: ", error_cond))
},  # end error handler
finally=print(paste("sym_bol=", sym_bol))
    )  # end tryCatch
  }  # end for
  # Update vector of symbols already downloaded
  down_loaded <- sym_bols %in% ls(sp500_env)
  Sys.sleep(10)  # Wait 10 seconds until next attempt
}  # end while
class(sp500_env$AAPL)
class(index(sp500_env$AAPL))
tail(sp500_env$AAPL)

# "LOW.Low" is a bad column name
colnames(sp500_env$LOW)
strsplit(colnames(sp500_env$LOW), split="[.]")
do.call(cbind, strsplit(colnames(sp500_env$LOW), split="[.]"))
do.call(cbind, strsplit(colnames(sp500_env$LOW), split="[.]"))[2, ]
# Extract proper names from column names
name_s <- rutils::get_name(colnames(sp500_env$LOW), field=2)
# Or
# name_s <- do.call(rbind, strsplit(colnames(sp500_env$LOW),
#                                   split="[.]"))[, 2]
# Rename "LOW" colnames to "LOWES"
colnames(sp500_env$LOW) <- paste("LO_WES", name_s, sep=".")
sp500_env$LOWES <- sp500_env$LOW
rm(LOW, envir=sp500_env)
# Download "BRK.B" separately with auto.assign=FALSE
BRKB <- quantmod::getSymbols("BRK-B", auto.assign=FALSE, src="tiingo", adjust=TRUE, from="1990-01-01", api.key="j84ac2b9c5bde2d68e33034f65d838092c6c9f10")
colnames(BRKB) <- paste("BRKB", name_s, sep=".")
sp500_env$BRKB <- BRKB
# Rename "BF-B" colnames to "BFB"
colnames(sp500_env$"BF-B") <- paste("BFB", name_s, sep=".")
sp500_env$BFB <- sp500_env$"BF-B"
rm("BF-B", envir=sp500_env)

# Plot OHLC candlestick chart for LOWES
chart_Series(x=sp500_env$LOWES["2019-12/"],
  TA="add_Vo()", name="LOWES OHLC Stock Prices")
# Plot dygraph
dygraphs::dygraph(sp500_env$LOWES["2019-12/", -5], main="LOWES OHLC Stock Prices") %>%
  dyCandlestick()

class(sp500_env$AAPL)
# The date-time index is class POSIXct not Date
class(index(sp500_env$AAPL))
# Coerce time indices from class POSIXct to class Date
for (sym_bol in ls(sp500_env)) {
  x_ts <- get(sym_bol, envir=sp500_env)
  index(x_ts) <- as.Date(index(x_ts))
  assign(sym_bol, x_ts, envir=sp500_env)
}  # end for
class(index(sp500_env$AAPL))
# Save the environment to compressed .RData file
dir_name <- "C:/Develop/lecture_slides/data/"
save(sp500_env, file=paste0(dir_name, "sp500.RData"))
# Save the ETF prices into CSV files
dir_name <- "C:/Develop/lecture_slides/data/SP500/"
for (sym_bol in ls(sp500_env)) {
  zoo::write.zoo(sp500_env$sym_bol, file=paste0(dir_name, sym_bol, ".csv"))
}  # end for
# Or using lapply()
file_names <- lapply(ls(sp500_env), function(sym_bol) {
  x_ts <- get(sym_bol, envir=sp500_env)
  zoo::write.zoo(x_ts, file=paste0(dir_name, sym_bol, ".csv"))
  sym_bol
})  # end lapply
unlist(file_names)
# Or using eapply() and data.table::fwrite()
file_names <- eapply(sp500_env , function(x_ts) {
  file_name <- rutils::get_name(colnames(x_ts)[1])
  data.table::fwrite(data.table::as.data.table(x_ts), file=paste0(dir_name, file_name, ".csv"))
  file_name
})  # end eapply
unlist(file_names)

# Load the environment from compressed .RData file
dir_name <- "C:/Develop/lecture_slides/data/"
load(file=paste0(dir_name, "sp500.RData"))
# Get all the .csv file names in the directory
dir_name <- "C:/Develop/lecture_slides/data/SP500/"
file_names <- Sys.glob(paste0(dir_name, "*.csv"))
# Create new environment for data
sp500_env <- new.env()
for (file_name in file_names) {
  x_ts <- xts::as.xts(zoo::read.csv.zoo(file_name))
  sym_bol <- rutils::get_name(colnames(x_ts)[1])
  # sym_bol <- strsplit(colnames(x_ts), split="[.]")[[1]][1]
  assign(sym_bol, x_ts, envir=sp500_env)
}  # end for
# Or using fread()
for (file_name in file_names) {
  x_ts <- data.table::fread(file_name)
  data.table::setDF(x_ts)
  x_ts <- xts::xts(x_ts[, -1], as.Date(x_ts[, 1]))
  sym_bol <- rutils::get_name(colnames(x_ts)[1])
  assign(sym_bol, x_ts, envir=sp500_env)
}  # end for

# Remove all files from environment(if necessary)
rm(list=ls(sp500_env), envir=sp500_env)
# Download in while loop from Alpha Vantage and copy into environment
down_loaded <- sym_bols %in% ls(sp500_env)
at_tempt <- 0
while (((sum(!down_loaded)) > 0) & (at_tempt<10)) {
  # Download data and copy it into environment
  at_tempt <- at_tempt + 1
  for (sym_bol in sym_bols[!down_loaded]) {
    cat("processing: ", sym_bol, "\n")
    tryCatch(  # With error handler
quantmod::getSymbols(sym_bol, src="av", adjust=TRUE, auto.assign=TRUE, env=sp500_env,
           output.size="full", api.key="T7JPW54ES8G75310"),
# error handler captures error condition
error=function(error_cond) {
  print(paste("error handler: ", error_cond))
},  # end error handler
finally=print(paste("sym_bol=", sym_bol))
    )  # end tryCatch
  }  # end for
  # Update vector of symbols already downloaded
  down_loaded <- sym_bols %in% ls(sp500_env)
  Sys.sleep(10)  # Wait 10 seconds until next attempt
}  # end while
# Adjust all OHLC prices in environment
for (sym_bol in ls(sp500_env)) {
  assign(sym_bol,
    adjustOHLC(get(x=sym_bol, envir=sp500_env), use.Adjusted=TRUE),
    envir=sp500_env)
}  # end for

library(rutils)  # Load package rutils
# Assign name SP500 to ^GSPC symbol
setSymbolLookup(SP500=list(name="^GSPC", src="yahoo"))
getSymbolLookup()
# view and clear options
options("getSymbols.sources")
options(getSymbols.sources=NULL)
# Download S&P500 prices into etf_env
quantmod::getSymbols("SP500", env=etf_env,
    adjust=TRUE, auto.assign=TRUE, from="1990-01-01")
chart_Series(x=etf_env$SP500["2016/"],
       TA="add_Vo()", name="S&P500 index")

library(rutils)  # Load package rutils
# Assign name DJIA to ^DJI symbol
setSymbolLookup(DJIA=list(name="^DJI", src="yahoo"))
getSymbolLookup()
# view and clear options
options("getSymbols.sources")
options(getSymbols.sources=NULL)
# Download DJIA prices into etf_env
quantmod::getSymbols("DJIA", env=etf_env,
    adjust=TRUE, auto.assign=TRUE, from="1990-01-01")
chart_Series(x=etf_env$DJIA["2016/"],
       TA="add_Vo()", name="DJIA index")

# Calculate prices from OHLC data of the S&P500 stocks
price_s <- eapply(sp500_env, quantmod::Cl)
price_s <- rutils::do_call(cbind, price_s)
# Carry forward non-NA prices
price_s <- zoo::na.locf(price_s, na.rm=FALSE)
# Get first column name
colnames(price_s[, 1])
rutils::get_name(colnames(price_s[, 1]))
# Modify column names
colnames(price_s) <- rutils::get_name(colnames(price_s))
# Or
# colnames(price_s) <- do.call(rbind,
#   strsplit(colnames(price_s), split="[.]"))[, 1]
# Calculate percentage returns
re_turns <- xts::diff.xts(price_s)/
  rutils::lag_it(price_s, pad_zeros=FALSE)
# Select a random sample of 100 prices and returns
set.seed(1121)
sam_ple <- sample(NCOL(re_turns), s=100, replace=FALSE)
prices_100 <- price_s[, sam_ple]
returns_100 <- re_turns[, sam_ple]
# Save the data into binary files
save(price_s, prices_100,
     file="C:/Develop/lecture_slides/data/sp500_prices.RData")
save(re_turns, returns_100,
     file="C:/Develop/lecture_slides/data/sp500_returns.RData")

library(rutils)  # Load package rutils
# Create name corresponding to "^GSPC" symbol
setSymbolLookup(SP500=list(name="^GSPC", src="yahoo"))
getSymbolLookup()
# view and clear options
options("getSymbols.sources")
options(getSymbols.sources=NULL)
# Download S&P500 prices into etf_env
quantmod::getSymbols("SP500", env=etf_env,
    adjust=TRUE, auto.assign=TRUE, from="1990-01-01")
chart_Series(x=etf_env$SP500["2016/"],
       TA="add_Vo()", name="S&P500 index")

library(rutils)  # Load package rutils
library(RCurl)  # Load package RCurl
library(XML)  # Load package XML
# Download text data from URL
sp_500 <- getURL(
  "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies")
# Extract tables from the text data
sp_500 <- readHTMLTable(sp_500)
str(sp_500)
# Extract colnames of data frames
lapply(sp_500, colnames)
# Extract S&P500 constituents
sp_500 <- sp_500[[1]]
head(sp_500)
# Create valid R names from symbols containing "-" or "."characters
sp_500$names <- gsub("-", "_", sp_500$Ticker)
sp_500$names <- gsub("[.]", "_", sp_500$names)
# Write data frame of S&P500 constituents to CSV file
write.csv(sp_500,
  file="C:/Develop/lecture_slides/data/sp500_Yahoo.csv",
  row.names=FALSE)

library(rutils)  # Load package rutils
# Load data frame of S&P500 constituents from CSV file
sp_500 <- read.csv(file="C:/Develop/lecture_slides/data/sp500_Yahoo.csv")
# Register symbols corresponding to R names
for (in_dex in 1:NROW(sp_500)) {
  cat("processing: ", sp_500$Ticker[in_dex], "\n")
  setSymbolLookup(structure(
    list(list(name=sp_500$Ticker[in_dex])),
    names=sp_500$names[in_dex]))
}  # end for
sp500_env <- new.env()  # new environment for data
# Remove all files (if necessary)
rm(list=ls(sp500_env), envir=sp500_env)
# Download data and copy it into environment
rutils::get_data(sp_500$names,
   env_out=sp500_env, start_date="1990-01-01")
# Or download in loop
for (sym_bol in sp_500$names) {
  cat("processing: ", sym_bol, "\n")
  rutils::get_data(sym_bol,
   env_out=sp500_env, start_date="1990-01-01")
}  # end for
save(sp500_env, file="C:/Develop/lecture_slides/data/sp500.RData")
chart_Series(x=sp500_env$BRKB["2016/"],
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
price_s <- Quandl(code="WIKI/AAPL",
            type="xts", start_date="1990-01-01")
x11(width=14, height=7)
chart_Series(price_s["2016", 1:4], name="AAPL OHLC prices")
# Add trade volume in extra panel
add_TA(price_s["2016", 5])
# Download euro currency rates
price_s <- Quandl(code="BNP/USDEUR",
    start_date="2013-01-01",
    end_date="2013-12-01", type="xts")
# Download multiple time series
price_s <- Quandl(code=c("NSE/OIL", "WIKI/AAPL"),
    start_date="2013-01-01", type="xts")
# Download AAPL gross profits
prof_it <- Quandl("RAYMOND/AAPL_GROSS_PROFIT_Q", type="xts")
chart_Series(prof_it, name="AAPL gross profits")
# Download Hurst time series
price_s <- Quandl(code="PE/AAPL_HURST",
    start_date="2013-01-01", type="xts")
chart_Series(price_s["2016/", 1], name="AAPL Hurst")

library(rutils)  # Load package rutils
# Load S&P500 stock Quandl codes
sp_500 <- read.csv(
  file="C:/Develop/lecture_slides/data/sp500_quandl.csv")
# Replace "-" with "_" in symbols
sp_500$free_code <- gsub("-", "_", sp_500$free_code)
head(sp_500)
# vector of symbols in sp_500 frame
tick_ers <- gsub("-", "_", sp_500$ticker)
# Or
tick_ers <- matrix(unlist(
  strsplit(sp_500$free_code, split="/"),
  use.names=FALSE), ncol=2, byrow=TRUE)[, 2]
# Or
tick_ers <- do_call_rbind(
  strsplit(sp_500$free_code, split="/"))[, 2]

library(rutils)  # Load package rutils
sp500_env <- new.env()  # new environment for data
# Remove all files (if necessary)
rm(list=ls(sp500_env), envir=sp500_env)
# Boolean vector of symbols already downloaded
down_loaded <- tick_ers %in% ls(sp500_env)
# Download data and copy it into environment
for (tick_er in tick_ers[!down_loaded]) {
  cat("processing: ", tick_er, "\n")
  da_ta <- Quandl(code=paste0("WIKI/", tick_er),
            start_date="1990-01-01", type="xts")[, -(1:7)]
  colnames(da_ta) <- paste(tick_er,
    c("Open", "High", "Low", "Close", "Volume"), sep=".")
  assign(tick_er, da_ta, envir=sp500_env)
}  # end for
save(sp500_env, file="C:/Develop/lecture_slides/data/sp500.RData")
chart_Series(x=sp500_env$XOM["2016/"], TA="add_Vo()", name="XOM stock")

library(rutils)
library(Quandl)
# Register Quandl API key
Quandl.api_key("pVJi9Nv3V8CD3Js5s7Qx")
# Download E-mini S&P500 futures prices
price_s <- Quandl(code="CHRIS/CME_ES1",
  type="xts", start_date="1990-01-01")
price_s <- price_s[, c("Open", "High", "Low", "Last", "Volume")]
colnames(price_s)[4] <- "Close"
# Plot the prices
x11(width=5, height=4)  # Open x11 for plotting
chart_Series(x=price_s["2008-06/2009-06"],
       TA="add_Vo()", name="S&P500 Futures")
# Plot dygraph
dygraphs::dygraph(price_s["2008-06/2009-06", -5],
  main="S&P500 Futures") %>%
  dyCandlestick()

# Read CBOE futures expiration dates
date_s <- read.csv(file="C:/Develop/lecture_slides/data/futures_expiration_dates_codes.csv",
  row.names=1)
dir_name <- "C:/Develop/data/vix_data"
dir.create(dir_name)
sym_bols <- rownames(date_s)
file_names <- file.path(dir_name, paste0(sym_bols, ".csv"))
log_file <- file.path(dir_name, "log_file.txt")
cboe_url <- "https://markets.cboe.com/us/futures/market_statistics/historical_data/products/csv/VX/"
url_s <- paste0(cboe_url, date_s[, 1])
# Download files in loop
for (it in seq_along(url_s)) {
    tryCatch(  # Warning and error handler
  download.file(url_s[it],
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
  file="C:/Develop/data/vix_data/vix_cboe.RData")

# Install and load package readxl
install.packages("readxl")
library(readxl)
dir_name <- "C:/Develop/lecture_slides/data"
fil_e <- file.path(dir_name, "multi_tabs.xlsx")
# Read a time series from first sheet of xlsx file
tib_ble <- readxl::read_xlsx(fil_e)
class(tib_ble)
# Coerce POSIXct dates into Date class
class(tib_ble$Dates)
tib_ble$Dates <- as.Date(tib_ble$Dates)
# Some columns are character strings
sapply(tib_ble, class)
sapply(tib_ble, is.character)
# Coerce columns with strings to numeric
lis_t <- lapply(tib_ble, function(x) {
  if (is.character(x))
    as.numeric(x)
  else
    x
})  # end lapply
# Coerce list into xts time series
x_ts <- xts::xts(do.call(cbind, lis_t)[, -1], lis_t[[1]])
class(x_ts); dim(x_ts)
# Replace NA values with the most recent non-NA values
sum(is.na(x_ts))
x_ts <- zoo::na.locf(x_ts, na.rm=FALSE)
x_ts <- zoo::na.locf(x_ts, fromLast=TRUE)

# Read names of all the sheets in an Excel spreadsheet
name_s <- readxl::excel_sheets(fil_e)
# Read all the sheets from an Excel spreadsheet
sheet_s <- lapply(name_s, read_xlsx, path=fil_e)
names(sheet_s) <- name_s
# sheet_s is a list of tibbles
sapply(sheet_s, class)
# Create function to coerce tibble to xts
to_xts <- function(tib_ble) {
  tib_ble$Dates <- as.Date(tib_ble$Dates)
  # Coerce columns with strings to numeric
  lis_t <- lapply(tib_ble, function(x) {
    if (is.character(x))
      as.numeric(x)
    else
      x
  })  # end lapply
  # Coerce list into xts series
  xts::xts(do.call(cbind, lis_t)[, -1], lis_t$Dates)
}  # end to_xts
# Coerce list of tibbles to list of xts
class(sheet_s)
sheet_s <- lapply(sheet_s, to_xts)
sapply(sheet_s, class)
# Replace NA values with the most recent non-NA values
sapply(sheet_s, function(x_ts) sum(is.na(x_ts)))
sheet_s <- lapply(sheet_s, zoo::na.locf, na.rm=FALSE)
sheet_s <- lapply(sheet_s, zoo::na.locf, fromLast=TRUE)

#Perform calculations in R,
#And export to CSV files
setwd("C:/Develop/lecture_slides/data")
# Read data frame, with row names from first column
data_read <- read.csv(file="florist.csv", row.names=1)
# Subset data frame
data_read <- data_read[data_read[, "type"]=="daisy", ]
# Write data frame to CSV file, with row names
write.csv(data_read, file="daisies.csv")

#Perform calculations in R,
#And export to CSV files
setwd("C:/Develop/lecture_slides/data")
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
      to="C:/Develop/lecture_slides/data/google_sheet.csv")
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
      to="C:/Develop/lecture_slides/data/google_sheet.csv")
# Open sheet in internet browser
gs_browse(google_sheet)
