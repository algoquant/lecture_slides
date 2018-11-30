library(knitr)
opts_chunk$set(prompt=TRUE, eval=FALSE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=6, fig.height=5)
options(width=60, dev='pdf')
options(digits=3)
thm <- knit_theme$get("acid")
knit_theme$set(thm)
cat("Enter\ttab")  # Cat() interprets backslash escape sequences
print("Enter\ttab")

my_text <- print("hello")
my_text  # print() returns its argument

# Create string
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
setwd("C:/Develop/R/lecture_slides/data")
data_frame <- data.frame(type=c("rose", "daisy", "tulip"), color=c("red", "white", "yellow"), price=c(1.5, 0.5, 1.0), row.names=c("flower1", "flower2", "flower3"))  # end data.frame
mat_rix <- matrix(sample(1:12), ncol=3, dimnames=list(NULL, c("col1", "col2", "col3")))
rownames(mat_rix) <- paste("row", 1:NROW(mat_rix), sep="")
# write data frame to text file, and then read it back
write.table(data_frame, file="florist.txt")
data_read <- read.table(file="florist.txt")
data_read  # A data frame

# write matrix to text file, and then read it back
write.table(mat_rix, file="matrix.txt")
mat_read <- read.table(file="matrix.txt")
mat_read  # write.table() coerced matrix to data frame
class(mat_read)
# Coerce from data frame back to matrix
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

# Launch spreadsheet-style data editor
data_frame <- edit(data_frame)
# write data frame to CSV file, and then read it back
write.csv(data_frame, file="florist.csv")
data_read <- read.csv(file="florist.csv",
                 stringsAsFactors=FALSE)
data_read  # the row names are read in as extra column
# Restore row names
rownames(data_read) <- data_read[, 1]
data_read <- data_read[, -1]  # Remove extra column
data_read
# Read data frame, with row names from first column
data_read <- read.csv(file="florist.csv", row.names=1)
data_read
# write data frame to CSV file, without row names
write.csv(data_frame, row.names=FALSE, file="florist.csv")
data_read <- read.csv(file="florist.csv")
data_read  # A data frame without row names
# write matrix to csv file, and then read it back
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
setwd("C:/Develop/R/lecture_slides/data")
library(MASS)  # Load package "MASS"
# write to CSV file by row - it's very SLOW!!!
MASS::write.matrix(mat_rix,
  file="matrix.csv", sep=",")
# Read using scan() and skip first line with colnames
mat_read <- scan(file="matrix.csv",
  sep=",", skip=1, what=numeric())
# Read colnames
col_names <- readLines(con="matrix.csv", n=1)
col_names  # this is a string!
# Convert to char vector
col_names <- strsplit(col_names,
  s=",")[[1]]
mat_read  # mat_read is a vector, not matrix!
# Coerce by row to matrix
mat_read <- matrix(mat_read,
  ncol=NROW(col_names), byrow=TRUE)
# Restore colnames
colnames(mat_read) <- col_names
mat_read
# scan() is a little faster than read.csv()
library(microbenchmark)
summary(microbenchmark(
  read_csv=read.csv("matrix.csv"),
  scan=scan(file="matrix.csv", sep=",",
    skip=1, what=numeric()),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Read data from a csv file, including row names
mat_rix <- read.csv(file="matrix_bad.csv",
  row.names=1, stringsAsFactors=FALSE)
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
setwd("C:/Develop/R/lecture_slides/data")
rm(list=ls())
set.seed(1121)  # Reset random number generator
library(zoo)  # Load package zoo
# Create zoo with Date index
in_dex <- seq(from=as.Date("2013-06-15"),
            by="day", length.out=100)
zoo_series <- zoo(cumsum(rnorm(NROW(in_dex))),
            order.by=in_dex)
tail(zoo_series, 3)
# write zoo series to text file, and then read it back
write.zoo(zoo_series, file="zoo_series.txt")
zoo_series_read <- read.zoo("zoo_series.txt")  # Read it back
tail(zoo_series, 3)
all.equal(zoo_series_read, zoo_series)
# Perform the same using write.table() and read.table()
# first coerce zoo_series into data frame
data_frame <- as.data.frame(zoo_series)
data_frame <- cbind(in_dex, data_frame)
# write zoo_series to text file using write.table
write.table(data_frame, file="zoo_series.txt",
      row.names=FALSE, col.names=FALSE)
# Read data frame from file
zoo_series_read <- read.table(file="zoo_series.txt",
                        stringsAsFactors=FALSE)
sapply(zoo_series_read, class)  # A data frame
# Coerce data frame into zoo_series
zoo_series_read <- zoo::zoo(
  drop(as.matrix(zoo_series_read[, -1])),
  order.by=as.Date(zoo_series_read[, 1]))
all.equal(zoo_series_read, zoo_series)
set.seed(1121)  # Reset random number generator
library(zoo)  # Load package zoo
# Create zoo with POSIXct date-time index
in_dex <- seq(from=as.POSIXct("2013-06-15"),
            by="hour", length.out=1000)
zoo_series <- zoo(cumsum(rnorm(NROW(in_dex))),
            order.by=in_dex)
# write zoo series to text file, and then read it back
write.zoo(zoo_series, file="zoo_series.txt")
zoo_series_read <- read.zoo("zoo_series.txt")  # Read it back
# time field was read as a separate column
tail(zoo_series_read, 3)
# Read and specify that second column is time field
zoo_series_read <- read.zoo(file="zoo_series.txt",
                 index.column=list(1,2),
                 tz="America/New_York")
all.equal(zoo_series_read, zoo_series)
# Perform the same using write.table() and read.table()
# first coerce zoo_series into data frame
data_frame <- as.data.frame(zoo_series)
data_frame <- cbind(in_dex, data_frame)
# write zoo_series to text file using write.table
write.table(data_frame, file="zoo_series.txt",
      row.names=FALSE, col.names=FALSE)
# Read data frame from file
zoo_series_read <- read.table(file="zoo_series.txt",
                        stringsAsFactors=FALSE)
sapply(zoo_series_read, class)  # A data frame
# paste first two date columns together and coerce into as.POSIXct
date_col <- 1:2
in_dex <- do.call(paste, zoo_series_read[, date_col])
in_dex <- as.POSIXct(in_dex, tz="America/New_York")
# Coerce data frame into zoo_series
zoo_series_read <- zoo::zoo(
  drop(as.matrix(zoo_series_read[, -date_col])),
  order.by=in_dex)
all.equal(zoo_series_read, zoo_series)
library(zoo)  # Load package zoo
# write zoo series to CSV file, and then read it back
write.zoo(zoo_series, file="zoo_series.csv",
    sep=",", col.names=TRUE)
zoo_series <- read.zoo(file="zoo_series.csv",
            header=TRUE, sep=",",
            drop=FALSE,
            FUN=as.POSIXct, tz="America/New_York")
tail(zoo_series, 3)
# Read zoo from CSV file, with custom date-time format
zoo_frame <- read.table(file="zoo_series2.csv",
                  sep=",")
tail(zoo_frame, 3)  # Date-time format mm/dd/yyyy hh:mm
zoo_series <- read.zoo(file="zoo_series2.csv",
            header=TRUE, sep=",",
            drop=FALSE,
            FUN=as.POSIXct,
            tz="America/New_York",
            format="%m/%d/%Y %H:%M")
tail(zoo_series, 3)
# Read zoo from CSV file, with numeric date-time
zoo_series <- read.csv(file="es_ohlc.csv",
  sep=",")
zoo_series <- zoo::zoo(zoo_series[, -1],
  order.by=as.POSIXct.numeric(zoo_series[, 1],
    tz="America/New_York",
    origin="1970-01-01"))
head(zoo_series, 11)
rm(list=ls())  # Remove all objects
var1 <- 1; var2 <- 2
ls()  # List all objects
ls()[1]  # List first object
args(save)  # List arguments of save function
# save "var1" to a binary file using string argument
save("var1", file="my_data.RData")
# save "var1" to a binary file using object name
save(var1, file="my_data.RData")
# save multiple objects
save(var1, var2, file="my_data.RData")
# save first object in list by passing to "..." argument
# Ls()[1] is not evaluated
save(ls()[1], file="my_data.RData")
# save first object in list by passing to "list" argument
save(list=ls()[1], file="my_data.RData")
# save whole list by passing it to the "list" argument
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
# save vector of objects
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
# get documentation for package data.table
# get short description
packageDescription("data.table")
# Load help page
help(package="data.table")
# List all datasets in "data.table"
data(package="data.table")
# List all objects in "data.table"
ls("package:data.table")
# Remove data.table from search path
detach("package:data.table")
data_table <- data.table::data.table(
  type=c('rose', 'daisy', 'tulip'),
  color=c('red', 'white', 'yellow'),
  price=c(1.5, 0.5, 1.0)
)  # end data.frame
sapply(data_table, class)
# Read a data table from CSV file
setwd("C:/Develop/R/lecture_slides/data")
flight_s <- data.table::fread("flights14.csv")
# fread() reads the same data as read.csv()
all.equal(read.csv("weather_delays14.csv", stringsAsFactors=FALSE),
    data.table::setDF(data.table::fread("weather_delays14.csv")))
# fread() is much faster than read.csv()
library(microbenchmark)
summary(microbenchmark(
  pure_r=read.csv("weather_delays14.csv"),
  data.table=data.table:::setDF(data.table::fread("weather_delays14.csv")),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# subset using multiple logical clauses
jfk_flights <- flight_s[origin == "JFK" & month == 6]
# subset first five rows
jfk_flights[1:5]
# sort flight_s by "origin"column in ascending order, then by "dest" in descending order
flight_s <- flight_s[order(origin, -dest)]

# fsort() is much slower than sort() !
foo <- flight_s[, dest]
foo <- runif(1e3)
all.equal(sort(foo), data.table::fsort(foo))
library(microbenchmark)
summary(microbenchmark(
  pure_r=sort(foo),
  data.table=data.table::fsort(foo),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary


# write data frame to CSV file, and then read it back
write.csv(data_frame, file="florist.csv")
data_read <- read.csv(file="florist.csv",
                 stringsAsFactors=FALSE)
data_read  # the row names are read in as extra column
# Restore row names
rownames(data_read) <- data_read[, 1]
data_read <- data_read[, -1]  # Remove extra column
data_read
# Read data frame, with row names from first column
data_read <- read.csv(file="florist.csv", row.names=1)
data_read
# Read a data table from CSV file
flight_s <- data.table::fread("weather_delays14.csv")
class(flight_s)
# Coerce flight_s into data frame
data.table::setDF(flight_s)
class(flight_s)
# Or
flight_s <- data.table:::as.data.frame.data.table(flight_s)
# setDF() is much faster than as.data.frame()
library(microbenchmark)
summary(microbenchmark(
  as.data.frame=data.table:::as.data.frame.data.table(flight_s),
  setDF=data.table::setDF(flight_s),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
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
load(file="C:/Develop/R/lecture_slides/data/zoo_data.RData")
# Calculate price adjustment vector
adj_vector <-
  as.vector(ts_stx[, "AdjClose"] / ts_stx[, "Close"])
# Adjust OHLC prices
ts_stx_adj <- ts_stx
ts_stx_adj[, c("Open","High","Low","Close")] <-
  adj_vector * ts_stx[, c("Open","High","Low","Close")]
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
load(file="C:/Develop/R/lecture_slides/data/zoo_data.RData")
class(zoo_stx)
dim(zoo_stx)
head(zoo_stx, 4)
library(tseries)  # Load package tseries
load(file="C:/Develop/R/lecture_slides/data/zoo_data.RData")
# Calculate price adjustment vector
adj_vector <-
  as.vector(zoo_stx[, "AdjClose"] / zoo_stx[, "Close"])
head(adj_vector, 5)
tail(adj_vector, 5)
# Adjust OHLC prices
zoo_stx_adj <- zoo_stx
zoo_stx_adj[, c("Open","High","Low","Close")] <-
  adj_vector * zoo_stx[, c("Open","High","Low","Close")]
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
# bind and scrub data
zoo_stxeur <- cbind(zoo_eurusd,
               zoo_stx[, "AdjClose"])
colnames(zoo_stxeur) <- c("EURUSD", "MSFT")
zoo_stxeur <-
  zoo_stxeur[complete.cases(zoo_stxeur),]
save(zoo_stx, zoo_stx_adj,
     ts_stx, ts_stx_adj,
     zoo_eurusd, zoo_stxeur,
     file="C:/Develop/R/lecture_slides/data/zoo_data.RData")
load(file="C:/Develop/R/lecture_slides/data/zoo_data.RData")
# Inspect the data
class(zoo_eurusd)
tail(zoo_eurusd, 4)
library(xtable)
# Define ETF symbols for asset allocation
sym_bols <- c("VTI", "VEU", "IEF", "VNQ",
  "DBC", "XLY", "XLP", "XLE", "XLF", "XLV",
  "XLI", "XLB", "XLK", "XLU", "VYM", "IVW", 
  "IWB", "IWD", "IWF", "VXX", "SVXY")
# Read etf database into data frame
etf_list <- read.csv(
  file='C:/Develop/R/lecture_slides/data/etf_list.csv', 
         stringsAsFactors=FALSE)
rownames(etf_list) <- etf_list$Symbol
# subset etf_list only those ETF's in sym_bols
etf_list <- etf_list[sym_bols, ]
# shorten names
etf_names <- sapply(etf_list$Name,
              function(name) {
  name_split <- strsplit(name, split=" ")[[1]]
  name_split <-
    name_split[c(-1, -NROW(name_split))]
  name_match <- match("Select", name_split)
  if (!is.na(name_match))
    name_split <- name_split[-name_match]
  paste(name_split, collapse=" ")
})  # end sapply
etf_list$Name <- etf_names
etf_list["IEF", "Name"] <- "Treasury Bond Fund"
etf_list["XLY", "Name"] <- "Consumer Discr. Sector Fund"
etf_list["SVXY", "Name"] <- "Short VIX Futures"
print(xtable(etf_list), comment=FALSE, size="tiny", include.rownames=FALSE)
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
# flatten list of zoo objects into a single zoo object
zoo_series <- rutils::do_call(cbind, zoo_series)
# Or
# zoo_series <- do.call(cbind, zoo_series)
# Assign names in format "symbol.Close", "symbol.Volume"
names(zoo_series) <-
  as.vector(sapply(sym_bols,
    paste, c("Close", "Volume"), sep="."))
# save zoo_series to a comma-separated CSV file
write.zoo(zoo_series, file='zoo_series.csv', sep=",")
# save zoo_series to a binary .RData file
save(zoo_series, file='zoo_series.RData')
library(HighFreq)  # Load package HighFreq
etf_env <- new.env()  # new environment for data
# Download data for sym_bols into etf_env from Alpha Vantage
getSymbols.av(sym_bols, adjust=TRUE, env=etf_env,
  output.size="full", api.key="T7JPW54ES8G75310")
# getSymbols(sym_bols, env=etf_env, adjust=TRUE, from="2005-01-03")
library(HighFreq)  # Load package HighFreq
ls(etf_env)  # List files in etf_env
# get class of object in etf_env
class(get(x=sym_bols[1], envir=etf_env))
# Another way
class(etf_env$VTI)
colnames(etf_env$VTI)
head(etf_env$VTI, 3)
# get class of all objects in etf_env
eapply(etf_env, class)
# get class of all objects in R workspace
lapply(ls(), function(ob_ject) class(get(ob_ject)))
library(HighFreq)  # Load package HighFreq
# Check of object is an OHLC time series
is.OHLC(etf_env$VTI)
# Adjust single OHLC object using its name
etf_env$VTI <- adjustOHLC(etf_env$VTI,
                    use.Adjusted=TRUE)

# Adjust OHLC object using string as name
assign(sym_bols[1], adjustOHLC(
    get(x=sym_bols[1], envir=etf_env),
    use.Adjusted=TRUE),
  envir=etf_env)

# Adjust objects in environment using vector of strings
for (sym_bol in ls(etf_env)) {
  assign(sym_bol,
   adjustOHLC(get(sym_bol, envir=etf_env),
              use.Adjusted=TRUE),
   envir=etf_env)
}  # end for
library(HighFreq)  # Load package HighFreq
# extract and cbind all data, subset by symbols
price_s <- rutils::do_call(cbind,
  as.list(etf_env)[sym_bols])
# Or
# price_s <- do.call(cbind,
#   as.list(etf_env)[sym_bols])
# extract and cbind adjusted prices, subset by symbols
price_s <- rutils::do_call(cbind,
  lapply(as.list(etf_env)[sym_bols], Ad))
# same, but works only for OHLC series
price_s <- rutils::do_call(cbind,
  eapply(etf_env, Ad)[sym_bols])
# Drop ".Adjusted" from colnames
colnames(price_s) <-
  sapply(colnames(price_s),
    function(col_name)
strsplit(col_name, split="[.]")[[1]],
    USE.NAMES=FALSE)[1, ]
tail(price_s[, 1:2], 3)
# which objects in global environment are class xts?
unlist(eapply(globalenv(), is.xts))
# save xts to csv file
write.zoo(price_s,
  file='etf_series.csv', sep=",")
# Copy price_s into etf_env and save to .RData file
assign("price_s", price_s, envir=etf_env)
save(etf_env, file='etf_data.RData')
# extract VTI prices
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
  daily_returns <- quantmod::dailyReturn(x_ts)
  colnames(daily_returns) <- names(x_ts)
  daily_returns
})  # end lapply
# "re_turns" is a list of xts
class(re_turns)
class(re_turns[[1]])
# flatten list of xts into a single xts
re_turns <- rutils::do_call(cbind, re_turns)
class(re_turns)
dim(re_turns)
# Copy re_turns into etf_env and save to .RData file
assign("re_turns", re_turns, envir=etf_env)
save(etf_env, file='etf_data.RData')
library(quantmod)
start_date <- "2012-05-10"; end_date <- "2013-11-20"
# subset all objects in environment and return as environment
new_env <- as.environment(eapply(etf_env, "[",
            paste(start_date, end_date, sep="/")))
# subset only sym_bols in environment and return as environment
new_env <- as.environment(
  lapply(as.list(etf_env)[sym_bols], "[",
   paste(start_date, end_date, sep="/")))
# extract and cbind adjusted prices and return to environment
assign("price_s", rutils::do_call(cbind,
         lapply(ls(etf_env), function(sym_bol) {
           x_ts <- Ad(get(sym_bol, etf_env))
           colnames(x_ts) <- sym_bol
           x_ts
         })), envir=new_env)
# get sizes of OHLC xts series in etf_env
sapply(mget(sym_bols, envir=etf_env), object.size)
# extract and cbind adjusted prices and return to environment
col_name <- function(x_ts)
  strsplit(colnames(x_ts), split="[.]")[[1]][1]
assign("price_s", rutils::do_call(cbind,
         lapply(mget(etf_env$sym_bols, envir=etf_env),
                function(x_ts) {
                  x_ts <- Ad(x_ts)
                  colnames(x_ts) <- col_name(x_ts)
                  x_ts
         })), envir=new_env)
library(HighFreq)  # Load package HighFreq
# Load data frame of S&P500 constituents from CSV file
sp_500 <- read.csv(file="C:/Develop/R/lecture_slides/data/sp500_WRDS_08-30-17.csv", stringsAsFactors=FALSE)
sym_bols <- sp_500$co_tic
# Get duplicate sym_bols
ta_ble <- table(sym_bols)
ta_ble[ta_ble>1]
sym_bols <- unique(sym_bols)
# Remove "BRK.B" and later download separately
sym_bols <- sym_bols[-which(sym_bols=="BRK.B")]
env_sp500 <- new.env()  # new environment for data
# Download in while loop from Tiingo and copy into environment
down_loaded <- sym_bols %in% ls(env_sp500)
it_er <- 0
while (((sum(!down_loaded)) > 0) & (it_er<10)) {
  # Boolean vector of symbols already downloaded
  down_loaded <- sym_bols %in% ls(env_sp500)
  # Download data and copy it into environment
  for (sym_bol in sym_bols[!down_loaded]) {
    cat("processing: ", sym_bol, "\n")
    tryCatch(  # with error handler
getSymbols(sym_bol, src="tiingo", adjust=TRUE,
  from="1990-01-01", env=env_sp500, api.key="j84ac2b9c5bde2d68e33034f65d838092c6c9f10"),
# error handler captures error condition
error=function(error_cond) {
  print(paste("error handler: ", error_cond))
},  # end error handler
finally=print(paste("sym_bol=", sym_bol))
    )  # end tryCatch
  }  # end for
  it_er <- it_er + 1
  Sys.sleep(2*60)
}  # end while
# Rename "LOW" colnames to "LO_WES"
colnames(env_sp500$LOW) <-
  sapply(colnames(env_sp500$LOW),
    function(col_name) {
col_name <- strsplit(col_name, split="[.]")[[1]]
paste("LO_WES", col_name[2], sep=".")
    })
env_sp500$LO_WES <- env_sp500$LOW[, unique(colnames(env_sp500$LOW))]
rm(LOW, envir=env_sp500)
# Download "BRK.B" separately
BRK_B <- getSymbols("BRK-B", auto.assign=FALSE, src="tiingo", adjust=TRUE, from="1990-01-01", api.key="j84ac2b9c5bde2d68e33034f65d838092c6c9f10")
colnames(BRK_B) <- paste("BRK_B", sapply(strsplit(colnames(BRK_B), split="[.]"), function(x) x[2]), sep=".")
env_sp500$BRK_B <- BRK_B
# Rename "BF-B" colnames to "BF_B"
colnames(env_sp500$"BF-B") <-
  sapply(colnames(env_sp500$"BF-B"),
    function(col_name) {
col_name <- strsplit(col_name, split="[.]")[[1]]
paste("BF_B", col_name[2], sep=".")
    })
env_sp500$BF_B <- env_sp500$"BF-B"
names(colnames(env_sp500$BF_B)) <- NULL
rm("BF-B", envir=env_sp500)
# Coerce the time indices from class POSIXct to class Date
for (sym_bol in ls(env_sp500)) {
  x_ts <- get(sym_bol, envir=env_sp500)
  index(x_ts) <- as.Date(index(x_ts))
  assign(sym_bol, x_ts, envir=env_sp500)
}  # end for
save(env_sp500,
  file="C:/Develop/R/lecture_slides/data/sp500.RData")
chart_Series(x=env_sp500$LO_WES["2017-06/"],
  TA="add_Vo()", name="LOWES stock")
# Remove all files from environment(if necessary)
rm(list=ls(env_sp500), envir=env_sp500)
# Download in while loop from Alpha Vantage and copy into environment
down_loaded <- sym_bols %in% ls(env_sp500)
it_er <- 0
while (((sum(!down_loaded)) > 0) & (it_er<10)) {
  # Boolean vector of symbols already downloaded
  down_loaded <- sym_bols %in% ls(env_sp500)
  # Download data and copy it into environment
  for (sym_bol in sym_bols[!down_loaded]) {
    cat("processing: ", sym_bol, "\n")
    tryCatch(  # with error handler
getSymbols(sym_bol, src="av", adjust=TRUE, env=env_sp500,
           output.size="full", api.key="T7JPW54ES8G75310"),
# error handler captures error condition
error=function(error_cond) {
  print(paste("error handler: ", error_cond))
},  # end error handler
finally=print(paste("sym_bol=", sym_bol))
    )  # end tryCatch
  }  # end for
  it_er <- it_er + 1
  Sys.sleep(2*60)
}  # end while
# Adjust all OHLC prices in environment
for (sym_bol in ls(env_sp500)) {
  assign(sym_bol,
    adjustOHLC(get(x=sym_bol, envir=env_sp500), use.Adjusted=TRUE),
    envir=env_sp500)
}  # end for
library(HighFreq)  # Load package HighFreq
# Assign name SP500 to ^GSPC symbol
setSymbolLookup(
  SP500=list(name="^GSPC", src="yahoo"))
getSymbolLookup()
# view and clear options
options("getSymbols.sources")
options(getSymbols.sources=NULL)
# Download S&P500 prices into etf_env
getSymbols("SP500", env=etf_env,
    adjust=TRUE, from="1990-01-01")
chart_Series(x=etf_env$SP500["2016/"],
       TA="add_Vo()",
       name="S&P500 index")
library(HighFreq)  # Load package HighFreq
# Assign name DJIA to ^DJI symbol
setSymbolLookup(
  DJIA=list(name="^DJI", src="yahoo"))
getSymbolLookup()
# view and clear options
options("getSymbols.sources")
options(getSymbols.sources=NULL)
# Download DJIA prices into etf_env
getSymbols("DJIA", env=etf_env,
    adjust=TRUE, from="1990-01-01")
chart_Series(x=etf_env$DJIA["2016/"],
       TA="add_Vo()",
       name="DJIA index")
library(HighFreq)  # Load package HighFreq
# Create name corresponding to "^GSPC" symbol
setSymbolLookup(
  SP500=list(name="^GSPC", src="yahoo"))
getSymbolLookup()
# view and clear options
options("getSymbols.sources")
options(getSymbols.sources=NULL)
# Download S&P500 prices into etf_env
getSymbols("SP500", env=etf_env,
    adjust=TRUE, from="1990-01-01")
chart_Series(x=etf_env$SP500["2016/"],
       TA="add_Vo()",
       name="S&P500 index")
library(HighFreq)  # Load package HighFreq
library(RCurl)  # Load package RCurl
library(XML)  # Load package XML
# Download text data from URL
sp_500 <- getURL(
  "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies")
# extract tables from the text data
sp_500 <- readHTMLTable(sp_500,
              stringsAsFactors=FALSE)
str(sp_500)
# extract colnames of data frames
lapply(sp_500, colnames)
# extract S&P500 constituents
sp_500 <- sp_500[[1]]
head(sp_500)
# Create valid R names from symbols containing "-" or "."characters
sp_500$names <- gsub("-", "_", sp_500$Ticker)
sp_500$names <- gsub("[.]", "_", sp_500$names)
# write data frame of S&P500 constituents to CSV file
write.csv(sp_500,
  file="C:/Develop/R/lecture_slides/data/sp500_Yahoo.csv",
  row.names=FALSE)
library(HighFreq)  # Load package HighFreq
# Load data frame of S&P500 constituents from CSV file
sp_500 <- read.csv(file="C:/Develop/R/lecture_slides/data/sp500_Yahoo.csv",
     stringsAsFactors=FALSE)
# Register symbols corresponding to R names
for (in_dex in 1:NROW(sp_500)) {
  cat("processing: ", sp_500$Ticker[in_dex], "\n")
  setSymbolLookup(structure(
    list(list(name=sp_500$Ticker[in_dex])),
    names=sp_500$names[in_dex]))
}  # end for
env_sp500 <- new.env()  # new environment for data
# Remove all files (if necessary)
rm(list=ls(env_sp500), envir=env_sp500)
# Download data and copy it into environment
rutils::get_symbols(sp_500$names,
   env_out=env_sp500, start_date="1990-01-01")
# Or download in loop
for (sym_bol in sp_500$names) {
  cat("processing: ", sym_bol, "\n")
  rutils::get_symbols(sym_bol,
   env_out=env_sp500, start_date="1990-01-01")
}  # end for
save(env_sp500, file="C:/Develop/R/lecture_slides/data/sp500.RData")
chart_Series(x=env_sp500$BRK_B["2016/"], TA="add_Vo()",
       name="BRK-B stock")
library(quantmod)
# Download U.S. unemployment rate data
unemp_rate <- getSymbols("UNRATE",
            auto.assign=FALSE,
            src="FRED")
# plot U.S. unemployment rate data
chart_Series(unemp_rate["1990/"],
      name="U.S. unemployment rate")
library(HighFreq)  # Load package HighFreq
install.packages("devtools")
library(devtools)
# Install package Quandl from github
install_github("quandl/R-package")
library(Quandl)  # Load package Quandl
# Register Quandl API key
Quandl.api_key("pVJi9Nv3V8CD3Js5s7Qx")
# get short description
packageDescription("Quandl")
# Load help page
help(package="Quandl")
# Remove Quandl from search path
detach("package:Quandl")
library(HighFreq)  # Load package HighFreq
# Download EOD AAPL prices from WIKI free database
price_s <- Quandl(code="WIKI/AAPL",
            type="xts", start_date="1990-01-01")
x11(width=14, height=7)
chart_Series(price_s["2016", 1:4],
    name="AAPL OHLC prices")
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
prof_it <- Quandl("RAYMOND/AAPL_GROSS_PROFIT_Q",
    type="xts")
chart_Series(prof_it, name="AAPL gross profits")
# Download Hurst time series
price_s <- Quandl(code="PE/AAPL_HURST",
    start_date="2013-01-01", type="xts")
chart_Series(price_s["2016/", 1],
       name="AAPL Hurst")
library(HighFreq)  # Load package HighFreq
# Load S&P500 stock Quandl codes
sp_500 <- read.csv(
  file="C:/Develop/R/lecture_slides/data/sp500_quandl.csv",
  stringsAsFactors=FALSE)
# Replace "-" with "_" in symbols
sp_500$free_code <-
  gsub("-", "_", sp_500$free_code)
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
library(HighFreq)  # Load package HighFreq
env_sp500 <- new.env()  # new environment for data
# Remove all files (if necessary)
rm(list=ls(env_sp500), envir=env_sp500)
# Boolean vector of symbols already downloaded
down_loaded <- tick_ers %in% ls(env_sp500)
# Download data and copy it into environment
for (tick_er in tick_ers[!down_loaded]) {
  cat("processing: ", tick_er, "\n")
  da_ta <- Quandl(code=paste0("WIKI/", tick_er),
            start_date="1990-01-01",
            type="xts")[, -(1:7)]
  colnames(da_ta) <- paste(tick_er,
    c("Open", "High", "Low", "Close", "Volume"), sep=".")
  assign(tick_er, da_ta, envir=env_sp500)
}  # end for
save(env_sp500, file="C:/Develop/R/lecture_slides/data/sp500.RData")
chart_Series(x=env_sp500$XOM["2016/"], TA="add_Vo()",
       name="XOM stock")
library(HighFreq)
library(Quandl)
# Register Quandl API key
Quandl.api_key("pVJi9Nv3V8CD3Js5s7Qx")
# Download E-mini S&P500 futures prices
price_s <- Quandl(code="CHRIS/CME_ES1",
  type="xts", start_date="1990-01-01")
price_s <- price_s[, c("Open", "High", "Low", "Last", "Volume")]
colnames(price_s)[4] <- "Close"
# plot the prices
x11(width=5, height=4)  # Open x11 for plotting
chart_Series(x=price_s["2008-06/2009-06"],
       TA="add_Vo()",
       name="S&P500 Futures")
# plot dygraph
dygraphs::dygraph(price_s["2008-06/2009-06", -5],
  main="S&P500 Futures") %>%
  dyCandlestick()
# Read CBOE futures expiration dates
date_s <- read.csv(file="C:/Develop/R/lecture_slides/data/futures_expiration_dates_codes.csv",
  stringsAsFactors=FALSE, row.names=1)
data_dir <- "C:/Develop/data/vix_data"
dir.create(data_dir)
sym_bols <- rownames(date_s)
file_names <- file.path(data_dir, paste0(sym_bols, ".csv"))
log_file <- file.path(data_dir, "log_file.txt")
cboe_url <- "https://markets.cboe.com/us/futures/market_statistics/historical_data/products/csv/VX/"
url_s <- paste0(cboe_url, date_s[, 1])
# Download files in loop
for (it in seq_along(url_s)) {
    tryCatch(  # warning and error handler
  download.file(url_s[it],
          destfile=file_names[it], quiet=TRUE),
# warning handler captures warning condition
warning=function(warning_cond) {
  cat(paste("warning handler: ", warning_cond, "\n"), file=log_file, append=TRUE)
},  # end warning handler
# error handler captures error condition
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
  Years=2018, src="cfe", env=vix_env)
# Or
qmao::getSymbols.cfe(Symbols="VX",
  Months=6:8, Years=2018, env=vix_env,
  verbose=FALSE, auto.assign=TRUE)
# Calculate the classes of all the objects
# In the environment vix_env
unlist(eapply(vix_env,
  function(x) {class(x)[1]}))
class(vix_env$VX_M18)
colnames(vix_env$VX_M18)
# Save the data to a binary file called "vix_cboe.RData".
save(vix_env,
  file="C:/Develop/data/vix_data/vix_cboe.RData")
# futures contracts codes
future_s <- rbind(c("S&P500 index", "ES"), 
              c("10yr Treasury", "ZN"),
              c("VIX index", "VX"),
              c("Gold", "GC"),
              c("Oil", "CL"),
              c("Euro FX", "EC"),
              c("Swiss franc", "SF"),
              c("Japanese Yen", "JY"))
colnames(future_s) <- c("Futures contract", "Code")
print(xtable::xtable(future_s), comment=FALSE, size="scriptsize", include.rownames=FALSE, latex.environments="flushleft")
# monthly futures contract codes
month_codes <- cbind(c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"), 
                     c("F", "G", "H", "J", "K", "M", "N", "Q", "U", "V", "X", "Z"))
colnames(month_codes) <- c("Month", "Code")
print(xtable::xtable(month_codes), comment=FALSE, size="scriptsize", include.rownames=FALSE, latex.environments="flushright")
# futures contracts codes
future_s <- rbind(c("S&P500 index", "SP", "ES"), 
              c("10yr Treasury", "ZN", "ZN"),
              c("VIX index", "VX", "delisted"),
              c("Gold", "GC", "YG"),
              c("Oil", "CL", "QM"),
              c("Euro FX", "EC", "E7"),
              c("Swiss franc", "SF", "MSF"),
              c("Japanese Yen", "JY", "J7"))
colnames(future_s) <- c("Futures contract", "Standard", "E-mini")
print(xtable::xtable(future_s), comment=FALSE, size="scriptsize", include.rownames=FALSE, latex.environments="flushleft")
# Load data for S&P Emini futures December 2018 contract
sym_bol <- "ES"
data_dir <- "C:/Develop/data/ib_data"
file_name <- file.path(data_dir, paste0(sym_bol, ".csv"))
# Read a data table from CSV file
price_s <- data.table::fread(file_name)
# Coerce price_s into data frame
data.table::setDF(price_s)
# Or
# price_s <- data.table:::as.data.frame.data.table(
#   data.table::fread(file_name))
# first column of price_s is a numeric date-time
tail(price_s)
# Coerce price_s into xts series
price_s <- xts::xts(price_s[, 2:6],
  order.by=as.Date(as.POSIXct.numeric(price_s[, 1],
    tz="America/New_York",
    origin="1970-01-01")))
colnames(price_s) <- c("Open", "High", "Low", "Close", "Volume")
tail(price_s)
# plot OHLC data in x11 window
x11(width=5, height=4)  # Open x11 for plotting
par(mar=c(5, 5, 2, 1), oma=c(0, 0, 0, 0))
chart_Series(x=price_s, TA="add_Vo()",
  name="S&P500 futures")
# plot dygraph
dygraphs::dygraph(price_s[, 1:4], main="OHLC prices") %>%
  dyCandlestick()
# Load ESU8 data
data_dir <- "C:/Develop/data/ib_data"
file_name <- file.path(data_dir, "ESU8.csv")
ES_U8 <- data.table::fread(file_name)
data.table::setDF(ES_U8)
ES_U8 <- xts::xts(ES_U8[, 2:6],
  order.by=as.Date(as.POSIXct.numeric(ES_U8[, 1],
    tz="America/New_York", origin="1970-01-01")))
colnames(ES_U8) <- c("Open", "High", "Low", "Close", "Volume")
# Load ESM8 data
file_name <- file.path(data_dir, "ESM8.csv")
ES_M8 <- data.table::fread(file_name)
data.table::setDF(ES_M8)
ES_M8 <- xts::xts(ES_M8[, 2:6],
  order.by=as.Date(as.POSIXct.numeric(ES_M8[, 1],
    tz="America/New_York", origin="1970-01-01")))
colnames(ES_M8) <- c("Open", "High", "Low", "Close", "Volume")
x11(width=6, height=5)  # Open x11 for plotting
# plot last month of ESU8 and ESM8 volume data
en_d <- end(ES_M8)
star_t <- (en_d - 30*24*60^2)
vol_ume <- cbind(Vo(ES_U8),
  Vo(ES_M8))[paste0(star_t, "/", en_d)]
colnames(vol_ume) <- c("ESU8", "ESM8")
col_ors <- c("blue", "green")
plot(vol_ume, col=col_ors, lwd=3, major.ticks="days",
     format.labels="%b-%d", observation.based=TRUE,
     main="Volumes of ESU8 and ESM8 futures")
legend("topleft", legend=colnames(vol_ume), col=col_ors,
 title=NULL, bty="n", lty=1, lwd=6, inset=0.1, cex=0.7)
# find date when ESU8 volume exceeds ESM8
exceed_s <- (vol_ume[, "ESU8"] > vol_ume[, "ESM8"])
in_deks <- min(which(exceed_s))
# In_deks <- match(TRUE, exceed_s)
# scale the ES_M8 prices
in_deks <- index(exceed_s[in_deks])
fac_tor <- as.numeric(Cl(ES_U8[in_deks])/Cl(ES_M8[in_deks]))
ES_M8[, 1:4] <- fac_tor*ES_M8[, 1:4]
# Calculate continuous contract prices
chain_ed <- rbind(ES_M8[index(ES_M8) < in_deks],
            ES_U8[index(ES_U8) >= in_deks])
# Or
# Chain_ed <- rbind(ES_M8[paste0("/", in_deks-1)],
#                   ES_U8[paste0(in_deks, "/")])
# plot continuous contract prices
chart_Series(x=chain_ed["2018"], TA="add_Vo()",
  name="S&P500 chained futures")
# Download VIX index data from CBOE
vix_index <- data.table::fread("http://www.cboe.com/publish/scheduledtask/mktdata/datahouse/vixcurrent.csv", skip=1)
class(vix_index)
dim(vix_index)
tail(vix_index)
sapply(vix_index, class)
vix_index <- xts(vix_index[, -1],
  order.by=as.Date(vix_index$Date, format="%m/%d/%Y"))
colnames(vix_index) <- c("Open", "High", "Low", "Close")
# Save the VIX data to binary file
load(file="C:/Develop/data/ib_data/vix_cboe.RData")
ls(vix_env)
vix_env$vix_index <- vix_index
ls(vix_env)
save(vix_env, file="C:/Develop/data/ib_data/vix_cboe.RData")
# plot OHLC data in x11 window
chart_Series(x=vix_index["2018"], name="VIX Index")
# plot dygraph
dygraphs::dygraph(vix_index, main="VIX Index") %>%
  dyCandlestick()
# Read CBOE monthly futures expiration dates
date_s <- read.csv(
  file="C:/Develop/R/lecture_slides/data/futures_expiration_dates.csv",
  stringsAsFactors=FALSE)
date_s <- as.Date(date_s[, 1])
year_s <- format(date_s, format="%Y")
year_s <- substring(year_s, 4)
# monthly futures contract codes
month_codes <- c("F", "G", "H", "J", "K", "M", "N", "Q", "U", "V", "X", "Z")
sym_bols <- paste0("VX", month_codes, year_s)
date_s <- as.data.frame(date_s)
colnames(date_s) <- "monthly_expiration_dates"
rownames(date_s) <- sym_bols
# write dates to CSV file, with row names
write.csv(date_s, row.names=TRUE,
  file="C:/Develop/R/lecture_slides/data/futures_expiration_dates_codes.csv")
# Read back CBOE futures expiration dates
date_s <- read.csv(file="C:/Develop/R/lecture_slides/data/futures_expiration_dates_codes.csv",
  stringsAsFactors=FALSE, row.names=1)
date_s[, 1] <- as.Date(date_s[, 1])
# Load VIX futures data from binary file
load(file="C:/Develop/data/vix_data/vix_cboe.RData")
# Get all VIX futures for 2018 except January
sym_bols <- ls(vix_env)
sym_bols <- sym_bols[grep("*8", sym_bols)]
sym_bols <- sym_bols[2:9]
# Specify dates for curves
low_vol <- as.Date("2018-01-11")
hi_vol <- as.Date("2018-02-05")
# Extract all VIX futures prices on the dates
curve_s <- lapply(sym_bols, function(sym_bol) {
  x_ts <- get(x=sym_bol, envir=vix_env)
  Cl(x_ts[c(low_vol, hi_vol)])
})  # end lapply
curve_s <- rutils::do_call(cbind, curve_s)
colnames(curve_s) <- sym_bols
curve_s <- t(coredata(curve_s))
colnames(curve_s) <- c("Contango 01/11/2018",
                 "Backwardation 02/05/2018")
x11(width=7, height=5)
par(mar=c(3, 2, 1, 1), oma=c(0, 0, 0, 0))
plot(curve_s[, 1], type="l", lty=1, col="blue", lwd=3,
     xaxt="n", xlab="", ylab="", ylim=range(curve_s),
     main="VIX Futures Curves")
axis(1, at=(1:NROW(curve_s)), labels=rownames(curve_s))
lines(curve_s[, 2], lty=1, lwd=3, col="red")
legend(x="topright", legend=colnames(curve_s),
 inset=0.05, cex=1.0, bty="n",
 col=c("blue", "red"), lwd=6, lty=1)
# Read CBOE futures expiration dates
date_s <- read.csv(file="C:/Develop/R/lecture_slides/data/futures_expiration_dates_codes.csv",
  stringsAsFactors=FALSE, row.names=1)
sym_bols <- rownames(date_s)
expiration_dates <- as.Date(date_s[, 1])
to_day <- as.Date("2018-05-07")
maturi_ty <- to_day + 30
# Find neighboring futures contracts
in_deks <- match(TRUE, expiration_dates > maturi_ty)
# In_deks <- min(which(expiration_dates > to_day))
expiration_dates[in_deks-1]
expiration_dates[in_deks]
front_symbol <- sym_bols[in_deks-1]
back_symbol <- sym_bols[in_deks]
front_date <- expiration_dates[in_deks-1]
back_date <- expiration_dates[in_deks]
# Load VIX futures data from binary file
load(file="C:/Develop/data/vix_data/vix_cboe.RData")
front_price <- get(x=front_symbol, envir=vix_env)
# front_price <- vix_env$front_symbol
front_price <- as.numeric(Cl(front_price[to_day]))
back_price <- get(x=back_symbol, envir=vix_env)
back_price <- as.numeric(Cl(back_price[to_day]))
# Calculate the constant maturity 30-day futures price
fra_c <- as.numeric(maturi_ty - front_date) /
  as.numeric(back_date - front_date)
pric_e <- (fra_c*back_price +
  (1-fra_c)*front_price)
library(HighFreq)
x11(width=5, height=3)  # Open x11 for plotting
# Load VIX futures data from binary file
load(file="C:/Develop/data/vix_data/vix_cboe.RData")
# plot VIX and SVXY data in x11 window
plot_theme <- chart_theme()
plot_theme$col$line.col <- "blue"
chart_Series(x=Cl(vix_env$vix_index["2007/"]),
       theme=plot_theme, name="VIX Index")
chart_Series(x=Cl(rutils::etf_env$VTI["2007/"]),
       theme=plot_theme, name="VTI ETF")
chart_Series(x=Cl(vix_env$vix_index["2017/2018"]),
       theme=plot_theme, name="VIX Index")
chart_Series(x=Cl(rutils::etf_env$SVXY["2017/2018"]),
       theme=plot_theme, name="SVXY ETF")
library(xtable)
# Read etf database into data frame
fundamental_data <-
  read.csv(file='C:/Develop/R/lecture_slides/data/fundamental_stock_data.csv',
               stringsAsFactors=FALSE)
print(xtable(fundamental_data), comment=FALSE, size="scriptsize", include.rownames=FALSE)
library(xtable)
# Read etf database into data frame
fundamental_data <-
  read.csv(file='C:/Develop/R/lecture_slides/data/fundamental_stock_data.csv',
               stringsAsFactors=FALSE)
print(xtable(fundamental_data), comment=FALSE, size="scriptsize", include.rownames=FALSE)
library(Quandl)  # Load package Quandl
# Register Quandl API key
Quandl.api_key("pVJi9Nv3V8CD3Js5s7Qx")

# Quandl stock market data
# https://blog.quandl.com/stock-market-data-ultimate-guide-part-1
# https://blog.quandl.com/stock-market-data-the-ultimate-guide-part-2

# Download RAYMOND metadata
# https://www.quandl.com/data/RAYMOND-Raymond/documentation/metadata

# Download S&P500 Index sonstituents
# https://s3.amazonaws.com/static.quandl.com/tickers/SP500.csv

# Download AAPL gross profits from RAYMOND
prof_it <-
  Quandl("RAYMOND/AAPL_GROSS_PROFIT_Q", type="xts")
chart_Series(prof_it, name="AAPL gross profits")

# Download multiple time series
price_s <- Quandl(code=c("NSE/OIL", "WIKI/AAPL"),
   start_date="2013-01-01", type="xts")

# Download datasets for AAPL
# https://www.quandl.com/api/v3/datasets/WIKI/AAPL.json

# Download metadata for AAPL
price_s <- Quandl(code=c("NSE/OIL", "WIKI/AAPL"),
   start_date="2013-01-01", type="xts")
# https://www.quandl.com/api/v3/datasets/WIKI/AAPL/metadata.json

# scrape fundamental data from Google using quantmod - doesn't work
funda_mentals <- getFinancials("HPQ", src="google", auto.assign=FALSE)
# view quarterly fundamentals
viewFinancials(funda_mentals,  period="Q")
viewFinancials(funda_mentals)

# scrape fundamental data from Yahoo using quantmod
# table of Yahoo data fields
# http://www.financialwisdomforum.org/gummy-stuff/Yahoo-data.htm

met_rics <- yahooQF(c("Price/Sales",
                "P/E Ratio",
                "Price/EPS Estimate Next Year",
                "PEG Ratio",
                "Dividend Yield",
                "Market Capitalization"))


sym_bols <- c("AAPL", "IBM", "MSFT")
# Not all the metrics are returned by Yahoo.
funda_mentals <- getQuote(paste(sym_bols, sep="", collapse=";"), src="yahoo", what=met_rics)
viewFinancials(funda_mentals,  period="Q")

funda_mentals <- getFinancials("HPQ", src="yahoo", auto.assign=FALSE)
viewFinancials(funda_mentals)


library(HighFreq)  # Load package HighFreq
install.packages("devtools")
library(devtools)
# Install package WRDS from github
install_github("WRDS/R-package")
library(WRDS)  # Load package WRDS
# Register WRDS API key
WRDS.api_key("pVJi9Nv3V8CD3Js5s7Qx")
# get short description
packageDescription("WRDS")
# Load help page
help(package="WRDS")
# Remove WRDS from search path
detach("package:WRDS")
library(HighFreq)  # Load package HighFreq
env_sp500 <- new.env()  # new environment for data
# Remove all files (if necessary)
rm(list=ls(env_sp500), envir=env_sp500)
# Boolean vector of symbols already downloaded
down_loaded <- tick_ers %in% ls(env_sp500)
# Download data and copy it into environment
for (tick_er in tick_ers[!down_loaded]) {
  cat("processing: ", tick_er, "\n")
  da_ta <- Quandl(code=paste0("WIKI/", tick_er),
            start_date="1990-01-01",
            type="xts")[, -(1:7)]
  colnames(da_ta) <- paste(tick_er,
    c("Open", "High", "Low", "Close", "Volume"), sep=".")
  assign(tick_er, da_ta, envir=env_sp500)
}  # end for
save(env_sp500, file="C:/Develop/R/lecture_slides/data/sp500.RData")
chart_Series(x=env_sp500$XOM["2016/"], TA="add_Vo()",
       name="XOM stock")
library(HighFreq)  # Load package HighFreq
# Download Fama-French factors from KFRENCH database
fac_tors <- Quandl(code="KFRENCH/FACTORS_D",
  start_date="2001-01-01", type="xts")
dim(fac_tors)
head(fac_tors)
tail(fac_tors)
chart_Series(cumsum(fac_tors["2001/", 1]/100),
  name="Fama-French factors")
# Load package HighFreq
library(HighFreq)
head(SPY_TAQ)
# Load package HighFreq
library(HighFreq)
head(SPY)
# Install package HighFreq from github
devtools::install_github(repo="algoquant/HighFreq")
# Load package HighFreq
library(HighFreq)
# get documentation for package HighFreq
# get short description
packageDescription("HighFreq")
# Load help page
help(package="HighFreq")
# List all datasets in "HighFreq"
data(package="HighFreq")
# List all objects in "HighFreq"
ls("package:HighFreq")
# Remove HighFreq from search path
detach("package:HighFreq")
# Load package HighFreq
library(HighFreq)
# you can see SPY when listing objects in HighFreq
ls("package:HighFreq")
# you can see SPY when listing datasets in HighFreq
data(package="HighFreq")
# but the SPY dataset isn't listed in the workspace
ls()
# HighFreq datasets are lazy loaded and available when needed
head(SPY)
# Load all the datasets in package HighFreq
data(hf_data)
# HighFreq datasets are now loaded and in the workspace
head(SPY)
# Library(xts)  # Load package xts
# Load package "PerformanceAnalytics"
library(PerformanceAnalytics)
data(managers)  # Load "managers" data set
ham_1 <- managers[, c("HAM1", "EDHEC LS EQ",
                "SP500 TR")]

chart.CumReturns(ham_1, lwd=2, ylab="",
  legend.loc="topleft", main="")
# Add title
title(main="Managers cumulative returns",
line=-1)
# Install package IBrokers
install.packages("IBrokers")
# Load package IBrokers
library(IBrokers)
# get documentation for package IBrokers
# get short description
packageDescription("IBrokers")
# Load help page
help(package="IBrokers")
# List all datasets in "IBrokers"
data(package="IBrokers")
# List all objects in "IBrokers"
ls("package:IBrokers")
# Remove IBrokers from search path
detach("package:IBrokers")
# Connect to Interactive Brokers TWS
ib_connect <- IBrokers::twsConnect(port=7497)
# Or connect to IB Gateway
# Ib_connect <- ibgConnect(port=4002)
# Check connection
IBrokers::isConnected(ib_connect)
# Download Interactive Brokers account information
ib_account <- IBrokers::reqAccountUpdates(conn=ib_connect, acctCode="DI1207807")
foo <- ib_account[[1]]
foo$AvailableFunds

ib_account <- IBrokers::.reqAccountUpdates(conn=ib_connect,  subscribe=TRUE, acctCode="DI1207807")


IBrokers::twsPortfolioValue(ib_account)
# Close the Interactive Brokers API connection
IBrokers::twsDisconnect(ib_connect)
# Define AAPL stock contract (object)
con_tract <- IBrokers::twsEquity("AAPL")
# Define CHF currency contract
con_tract <- IBrokers::twsCurrency("CHF", currency="USD")
# Define S&P Emini future December 2018 contract
con_tract <- IBrokers::twsFuture(symbol="ES",
  exch="GLOBEX", expiry="201812")
# Define 10yr Treasury future December 2018 contract
con_tract <- IBrokers::twsFuture(symbol="ZN",
  exch="ECBOT", expiry="201812")
# Define euro currency future October 2018 contract
con_tract <- IBrokers::twsFuture(symbol="EUR",
  exch="GLOBEX", expiry="201810")
# Define Gold future July 2018 contract
con_tract <- IBrokers::twsFuture(symbol="GC",
  exch="NYMEX", expiry="201807")
# test if contract object is correct
IBrokers::is.twsContract(con_tract)
# Download list with instrument information
IBrokers::reqContractDetails(conn=ib_connect, Contract=con_tract)
# Install the package twsInstrument
install.packages("twsInstrument", repos="http://r-forge.r-project.org")
# Define euro future using getContract() and Conid
con_tract <- twsInstrument::getContract("317631411")
# Download list with instrument information
IBrokers::reqContractDetails(conn=ib_connect, Contract=con_tract)
# Define VIX monthly and weekly futures October 2018 contract
sym_bol <- "VIX"
con_tract <- IBrokers::twsFuture(symbol=sym_bol,
  exch="CFE", expiry="201810")
# Define VIX monthly futures October 2018 contract
con_tract <- IBrokers::twsFuture(symbol=sym_bol,
  local="VXV8", exch="CFE", expiry="201810")
# Define VIX weekly futures October 3rd 2018 contract
con_tract <- IBrokers::twsFuture(symbol=sym_bol,
  local="VX40V8", exch="CFE", expiry="201810")
# Download list with instrument information
IBrokers::reqContractDetails(conn=ib_connect,
  Contract=con_tract)
# Define S&P Emini futures December 2018 contract
sym_bol <- "ES"
con_tract <- IBrokers::twsFuture(symbol=sym_bol,
  exch="GLOBEX", expiry="201812")
# Open file for data download
data_dir <- "C:/Develop/data/ib_data"
dir.create(data_dir)
file_name <- file.path(data_dir, paste0(sym_bol, "_201812.csv"))
file_connect <- file(file_name, open="w")
# Connect to Interactive Brokers TWS
ib_connect <- IBrokers::twsConnect(port=7497)
# Download historical data to file
IBrokers::reqHistoricalData(conn=ib_connect,
  Contract=con_tract,
  # whatToShow="MIDPOINT",
  # endDateTime=ib_time,
  barSize="1 day", duration="6 M",
  file=file_connect)
# Close data file
close(file_connect)
# Close the Interactive Brokers API connection
IBrokers::twsDisconnect(ib_connect)
# Define S&P Emini futures June 2018 contract
sym_bol <- "ES"
con_tract <- IBrokers::twsFuture(symbol=sym_bol,
  include_expired="1",
  exch="GLOBEX", expiry="201806")
# Open file for ESM8 data download
file_name <- file.path(data_dir, paste0(sym_bol, "M8.csv"))
file_connect <- file(file_name, open="w")
# Connect to Interactive Brokers TWS
ib_connect <- IBrokers::twsConnect(port=7497)
# Download historical data to file
IBrokers::reqHistoricalData(conn=ib_connect,
  Contract=con_tract,
  barSize="1 day", duration="2 Y",
  file=file_connect)
# Close data file
close(file_connect)
# Close the Interactive Brokers API connection
IBrokers::twsDisconnect(ib_connect)
# Load OHLC data and coerce it into xts series
price_s <- data.table::fread(file_name)
data.table::setDF(price_s)
price_s <- xts::xts(price_s[, 2:6],
  order.by=as.Date(as.POSIXct.numeric(price_s[, 1],
    tz="America/New_York", origin="1970-01-01")))
colnames(price_s) <- c("Open", "High", "Low", "Close", "Volume")
# plot OHLC data in x11 window
chart_Series(x=price_s, TA="add_Vo()",
  name="S&P500 ESM8 futures")
# plot dygraph
dygraphs::dygraph(price_s[, 1:4], main="S&P500 ESM8 futures") %>%
  dyCandlestick()
# Define S&P Emini futures June 2018 contract
sym_bol <- "ES"
con_tract <- IBrokers::twsFuture(symbol=sym_bol,
  include_expired="1",
  exch="GLOBEX", expiry="201806")
# Open file for data download
data_dir <- "C:/Develop/data/ib_data"
dir.create(data_dir)
file_name <- file.path(data_dir, paste0(sym_bol, ".csv"))
file_connect <- file(file_name, open="w")
# Connect to Interactive Brokers TWS
ib_connect <- IBrokers::twsConnect(port=7497)
# Download historical data to file
IBrokers::reqHistoricalData(conn=ib_connect,
  Contract=con_tract,
  barSize="1 day", duration="6 M",
  file=file_connect)
# Close data file
close(file_connect)
# Close the Interactive Brokers API connection
IBrokers::twsDisconnect(ib_connect)
# Define S&P Emini futures December 2018 contract
sym_bol <- "ES"
con_tract <- IBrokers::twsFuture(symbol=sym_bol,
  exch="GLOBEX", expiry="201812")
# Open file for data download
data_dir <- "C:/Develop/data/ib_data"
# Dir.create(data_dir)
file_name <- file.path(data_dir, paste0(sym_bol, "_taq_live.csv"))
file_connect <- file(file_name, open="w")
# Connect to Interactive Brokers TWS
ib_connect <- IBrokers::twsConnect(port=7497)
# Download live data to file
IBrokers::reqMktData(conn=ib_connect,
     Contract=con_tract,
     eventWrapper=eWrapper.MktData.CSV(1),
     file=file_connect)
# Close data file
close(file_connect)
# Close the Interactive Brokers API connection
IBrokers::twsDisconnect(ib_connect)
# Define S&P Emini futures December 2018 contract
sym_bol <- "ES"
con_tract <- IBrokers::twsFuture(symbol=sym_bol,
  exch="GLOBEX", expiry="201812")
# Open file for data download
data_dir <- "C:/Develop/data/ib_data"
# Dir.create(data_dir)
file_name <- file.path(data_dir, paste0(sym_bol, "_ohlc_live.csv"))
file_connect <- file(file_name, open="w")
# Connect to Interactive Brokers TWS
ib_connect <- IBrokers::twsConnect(port=7497)
# Download live data to file
IBrokers::reqRealTimeBars(conn=ib_connect,
     Contract=con_tract, barSize="1",
     eventWrapper=eWrapper.RealTimeBars.CSV(1),
     file=file_connect)
# Close data file
close(file_connect)
# Close the Interactive Brokers API connection
IBrokers::twsDisconnect(ib_connect)
# Load OHLC data and coerce it into xts series
price_s <- data.table::fread(file_name)
data.table::setDF(price_s)
price_s <- xts::xts(price_s[, 2:6],
  order.by=as.POSIXct.numeric(price_s[, 1],
    tz="America/New_York", origin="1970-01-01"))
colnames(price_s) <- c("Open", "High", "Low", "Close", "Volume")
# Plot OHLC data in x11 window
chart_Series(x=price_s, TA="add_Vo()",
  name="S&P500 ESZ8 futures")
# Plot dygraph
dygraphs::dygraph(price_s[, 1:4], main="S&P500 ESZ8 futures") %>%
  dyCandlestick()
# Define S&P Emini future December 2018 contract
con_tract <- IBrokers::twsFuture(symbol="ES", exch="GLOBEX", expiry="201812")
# Define euro currency contract EUR.USD
con_tract <- IBrokers::twsCurrency("EUR", currency="USD")
# Define euro currency E-mini futures December 2018 contract E7Z8
con_tract <- IBrokers::twsFuture(symbol="E7", exch="GLOBEX", expiry="201812")
# Define Japanese yen currency contract JPY.USD
con_tract <- IBrokers::twsCurrency("JPY", currency="USD")
# Define Japanese yen currency E-mini futures December 2018 contract J7Z8
con_tract <- IBrokers::twsFuture(symbol="J7", exch="GLOBEX", expiry="201812")
# Define Japanese yen currency futures December 2018 contract 6JZ8
con_tract <- IBrokers::twsFuture(symbol="JPY", exch="GLOBEX", expiry="201812")
# Connect to Interactive Brokers TWS
ib_connect <- IBrokers::twsConnect(port=7497)
IBrokers::reqContractDetails(conn=ib_connect, Contract=con_tract)
# Request trade order ID
order_id <- IBrokers::reqIds(ib_connect)
# Create buy market order object
ib_order <- IBrokers::twsOrder(order_id, orderType="MKT",
  action="BUY", totalQuantity=1)
# Place trade order
IBrokers::placeOrder(ib_connect, con_tract, ib_order)
# Execute sell market order
order_id <- IBrokers::reqIds(ib_connect)
ib_order <- IBrokers::twsOrder(order_id, orderType="MKT",
  action="SELL", totalQuantity=1)
IBrokers::placeOrder(ib_connect, con_tract, ib_order)
# Execute buy market order
order_id <- IBrokers::reqIds(ib_connect)
ib_order <- IBrokers::twsOrder(order_id, orderType="MKT",
  action="BUY", totalQuantity=1)
IBrokers::placeOrder(ib_connect, con_tract, ib_order)
# Request trade order ID
order_id <- IBrokers::reqIds(ib_connect)
# Create buy limit order object
ib_order <- IBrokers::twsOrder(order_id, orderType="LMT",
  lmtPrice="1.1511", action="BUY", totalQuantity=1)
# Place trade order
IBrokers::placeOrder(ib_connect, con_tract, ib_order)
# Cancel trade order
IBrokers::cancelOrder(ib_connect, order_id)
# Execute sell limit order
order_id <- IBrokers::reqIds(ib_connect)
ib_order <- IBrokers::twsOrder(order_id, orderType="LMT",
  lmtPrice="1.1512", action="SELL", totalQuantity=1)
IBrokers::placeOrder(ib_connect, con_tract, ib_order)
# Cancel trade order
IBrokers::cancelOrder(ib_connect, order_id)
# Close the Interactive Brokers API connection
IBrokers::twsDisconnect(ib_connect)
eWrapper_realtimebars <- function (n = 1) {
  eW <- eWrapper_new(NULL)
  # eW <- IBrokers::eWrapper(NULL)
  eW$assign.Data("data", rep(list(structure(.xts(matrix(rep(NA_real_, 7), ncol = 7), 0), .Dimnames = list(NULL, c("Open", "High", "Low", "Close", "Volume", "WAP", "Count")))), n))
  eW$realtimeBars <- function(curMsg, msg, timestamp, file, ...) {
    id <- as.numeric(msg[2])
    file <- file[[id]]
    data <- eW$get.Data("data")
    attr(data[[id]], "index") <- as.numeric(msg[3])
    nr.data <- NROW(data[[id]])
    # write to file
    cat(paste(msg[3], msg[4], msg[5], msg[6], msg[7], msg[8], msg[9], msg[10], sep = ","), "\n", file = file, append = TRUE)
    # write to console
    # eW$count_er <- eW$count_er + 1
    eW$assign.Data("count_er", eW$get.Data("count_er")+1)
    cat(paste0("count_er=", eW$get.Data("count_er"), "\tOpen=", msg[4], "\tHigh=", msg[5], "\tLow=", msg[6], "\tClose=", msg[7], "\tVolume=", msg[8]), "\n")
    # cat(paste0("Open=", msg[4], "\tHigh=", msg[5], "\tLow=", msg[6], "\tClose=", msg[7], "\tVolume=", msg[8]), "\n")
    #Trade
    # Cancel previous trade orders
    buy_id <- eW$get.Data("buy_id")
    sell_id <- eW$get.Data("sell_id")
    if (buy_id>0) IBrokers::cancelOrder(ib_connect, buy_id)
    if (sell_id>0) IBrokers::cancelOrder(ib_connect, sell_id)
    # Execute buy limit order
    buy_id <- IBrokers::reqIds(ib_connect)
    buy_order <- IBrokers::twsOrder(buy_id, orderType="LMT",
                              lmtPrice=msg[6]-0.25, action="BUY", totalQuantity=1)
    IBrokers::placeOrder(ib_connect, con_tract, buy_order)
    # Execute sell limit order
    sell_id <- IBrokers::reqIds(ib_connect)
    sell_order <- IBrokers::twsOrder(sell_id, orderType="LMT",
                               lmtPrice=msg[5]+0.25, action="SELL", totalQuantity=1)
    IBrokers::placeOrder(ib_connect, con_tract, sell_order)
    # Copy new trade orders
    eW$assign.Data("buy_id", buy_id)
    eW$assign.Data("sell_id", sell_id)
    #Trade finished
    data[[id]][nr.data, 1:7] <- as.numeric(msg[4:10])
    eW$assign.Data("data", data)
    c(curMsg, msg)
  }  # end eW$realtimeBars
  return(eW)
}  # end eWrapper_realtimebars
# Define S&P Emini futures December 2018 contract
snp_contract <- IBrokers::twsFuture(symbol="ES",
  exch="GLOBEX", expiry="201812")
# Define VIX futures December 2018 contract
vix_contract <- IBrokers::twsFuture(symbol="VIX",
  local="VXZ8", exch="CFE", expiry="201812")
# Define 10yr Treasury futures December 2018 contract
trs_contract <- IBrokers::twsFuture(symbol="ZN",
  exch="ECBOT", expiry="201812")
# Define Emini gold futures December 2018 contract
gold_contract <- IBrokers::twsFuture(symbol="YG",
  exch="NYSELIFFE", expiry="201812")
# Define euro currency future December 2018 contract
euro_contract <- IBrokers::twsFuture(symbol="EUR",
  exch="GLOBEX", expiry="201812")
IBrokers::reqContractDetails(conn=ib_connect, Contract=euro_contract)

# Define data directory
data_dir <- "C:/Develop/data/ib_data"
# Dir.create(data_dir)

# Open file for error messages
file_root <- "replay"
file_name <- file.path(data_dir, paste0(file_root, "_error.csv"))
error_connect <- file(file_name, open="w")

# Open file for raw data
file_name <- file.path(data_dir, paste0(file_root, "_raw.csv"))
raw_connect <- file(file_name, open="w")

# Create empty eWrapper to redirect error messages to error file
error_ewrapper <- eWrapper(debug=NULL, errfile=error_connect)

# Create eWrapper for raw data
raw_ewrapper <- eWrapper(debug=TRUE)

# Redirect error messages to error eWrapper (error_ewrapper),
# by replacing handler function errorMessage() in raw_ewrapper
raw_ewrapper$errorMessage <- error_ewrapper$errorMessage

# Connect to Interactive Brokers TWS
ib_connect <- IBrokers::twsConnect(port=7497)

# Download raw data for multiple contracts for replay
IBrokers::reqMktData(ib_connect,
  list(snp_contract, vix_contract, trs_contract, gold_contract, euro_contract),
  eventWrapper=raw_ewrapper, file=raw_connect)

# Close the Interactive Brokers API connection
IBrokers::twsDisconnect(ib_connect)

# Close data files
close(raw_connect)
close(error_connect)

Replay the raw data

# Open file with raw data
file_name <- file.path(data_dir, paste0(file_root, "_raw.csv"))
raw_connect <- IBrokers::twsConnect(file_name)
class(raw_connect) <- c("twsPlayback", class(raw_connect))
# Replay the raw data
IBrokers::reqMktData(raw_connect, list(snp_contract, vix_contract))

# Open file for data
file_connect <- file(file.path(data_dir, "temp.csv"), open="w")
# Download TAQ data to file
IBrokers::reqMktData(conn=raw_connect,
     Contract=snp_contract,
     eventWrapper=eWrapper.MktData.CSV(1),
     file=file_connect)

# Close file for TAQ data
close(file_connect)
# Close file with raw data
IBrokers::twsDisconnect(raw_connect)

# Define AAPL stock contract (object)
con_tract <- IBrokers::twsEquity("AAPL")
# Define CHF currency contract
con_tract <- IBrokers::twsCurrency("CHF", currency="USD")
# Define S&P Emini future December 2018 contract
con_tract <- IBrokers::twsFuture(symbol="ES",
  exch="GLOBEX", expiry="201812")
# Define 10yr Treasury future December 2018 contract
con_tract <- IBrokers::twsFuture(symbol="ZN",
  exch="ECBOT", expiry="201812")
# Define euro currency future October 2018 contract
con_tract <- IBrokers::twsFuture(symbol="EUR",
  exch="GLOBEX", expiry="201810")
# Define Gold future July 2018 contract
con_tract <- IBrokers::twsFuture(symbol="GC",
  exch="NYMEX", expiry="201807")
# test if contract object is correct
IBrokers::is.twsContract(con_tract)
# Download list with instrument information
IBrokers::reqContractDetails(conn=ib_connect, Contract=con_tract)
# Install the package twsInstrument
install.packages("twsInstrument", repos="http://r-forge.r-project.org")
# Define euro future using getContract() and Conid
con_tract <- twsInstrument::getContract("317631411")
# Download list with instrument information
IBrokers::reqContractDetails(conn=ib_connect, Contract=con_tract)
# Define AAPL stock contract (object)
con_tract <- IBrokers::twsEquity("AAPL")
# Define CHF currency contract
con_tract <- IBrokers::twsCurrency("CHF", currency="USD")
# Define S&P Emini future December 2018 contract
con_tract <- IBrokers::twsFuture(symbol="ES",
  exch="GLOBEX", expiry="201812")
# Define 10yr Treasury future December 2018 contract
con_tract <- IBrokers::twsFuture(symbol="ZN",
  exch="ECBOT", expiry="201812")
# Define euro currency future October 2018 contract
con_tract <- IBrokers::twsFuture(symbol="EUR",
  exch="GLOBEX", expiry="201810")
# Define Gold future July 2018 contract
con_tract <- IBrokers::twsFuture(symbol="GC",
  exch="NYMEX", expiry="201807")
# test if contract object is correct
IBrokers::is.twsContract(con_tract)
# Download list with instrument information
IBrokers::reqContractDetails(conn=ib_connect, Contract=con_tract)
# Install the package twsInstrument
install.packages("twsInstrument", repos="http://r-forge.r-project.org")
# Define euro future using getContract() and Conid
con_tract <- twsInstrument::getContract("317631411")
# Download list with instrument information
IBrokers::reqContractDetails(conn=ib_connect, Contract=con_tract)
