# Select ETF symbols for asset allocation
symbolv <- c("SPY", "VTI", "QQQ", "VEU", "EEM", "XLY", "XLP", 
"XLE", "XLF", "XLV", "XLI", "XLB", "XLK", "XLU", "VYM", "IVW", 
"IWB", "IWD", "IWF", "IEF", "TLT", "VNQ", "DBC", "GLD", "USO", 
"VXX", "SVXY", "MTUM", "IVE", "VLUE", "QUAL", "VTV", "USMV", "AIEQ")
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
symbolv <- c("SPY", "VTI", "QQQ", "VEU", "EEM", "XLY", "XLP",
"XLE", "XLF", "XLV", "XLI", "XLB", "XLK", "XLU", "VYM", "IVW",
"IWB", "IWD", "IWF", "IEF", "TLT", "VNQ", "DBC", "GLD", "USO",
"VXX", "SVXY", "MTUM", "IVE", "VLUE", "QUAL", "VTV", "USMV", "AIEQ")
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
class(price2)
dim(price2)
# Collapse list into time series using do.call()
pricev <- do.call(cbind, pricev)
all.equal(price2, pricev)
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
colv <- c("index", "VTI")
colnames(datav) <- colv
# Plot index with VTI
endd <- rutils::calc_endpoints(datav, interval="weeks")
dygraphs::dygraph(log(datav)[endd],
  main="S&P 500 Price-weighted Index and VTI") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", col="red") %>%
  dySeries(name=colv[2], axis="y2", col="blue")

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
symbolv <- c("SPY", "VTI", "QQQ", "VEU", "EEM", "XLY", "XLP",
"XLE", "XLF", "XLV", "XLI", "XLB", "XLK", "XLU", "VYM", "IVW",
"IWB", "IWD", "IWF", "IEF", "TLT", "VNQ", "DBC", "GLD", "USO",
"VXX", "SVXY", "MTUM", "IVE", "VLUE", "QUAL", "VTV", "USMV", "AIEQ")
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
colv <- strsplit(colnames(capmstats), split=" ")
colv <- do.call(cbind, colv)[1, ]
colnames(capmstats) <- colv
capmstats <- t(capmstats)
capmstats <- capmstats[, -1]
colv <- colnames(capmstats)
whichv <- match(c("Annualized Alpha", "Information Ratio", "Treynor Ratio"), colv)
colv[whichv] <- c("Alpha", "Information", "Treynor")
colnames(capmstats) <- colv
capmstats <- capmstats[order(capmstats[, "Alpha"], decreasing=TRUE), ]
# Copy capmstats into etfenv
etfenv$capmstats <- capmstats
save(etfenv, file="/Users/jerzy/Develop/lecture_slides/data/etf_data.RData")

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

library(rutils)  # Load package rutils
# Calculate VTI percentage returns
retp <- rutils::etfenv$returns$VTI
retp <- drop(coredata(na.omit(retp)))
nrows <- NROW(retp)
# Mean and standard deviation of returns
c(mean(retp), sd(retp))
# Calculate the smoothing bandwidth as the MAD of returns 10 points apart
retp <- sort(retp)
bwidth <- 10*mad(rutils::diffit(retp, lagg=10))
# Calculate the kernel density using a loop
dens1 <- sapply(1:nrows, function(it) {
  sum(dnorm(retp-retp[it], sd=bwidth))
})/nrows  # end sapply

# Plot the kernel density
madv <- mad(retp)
plot(retp, dens1, xlim=c(-5*madv, 5*madv),
     t="l", col="blue", lwd=3,
     xlab="returns", ylab="density",
     main="Density of VTI Returns")

# Calculate the kernel density using density()
densv <- density(retp, bw=bwidth)
NROW(densv$y)
plot(densv, xlim=c(-5*madv, 5*madv),
     xlab="returns", ylab="density",
     col="blue", lwd=3, main="Density of VTI Returns")
# Interpolate the densv vector into returns
densv <- approx(densv$x, densv$y, xout=retp)
all.equal(densv$x, retp)

# Plot the two density estimates
plot(retp, dens1, xlim=c(-5*madv, 5*madv),
     xlab="returns", ylab="density",
     t="l", col="blue", lwd=1,
     main="Density of VTI Returns")
lines(retp, densv$y, col="red")
# Add legend
legend("topright", inset=0.05, cex=0.8, title=NULL,
 leg=c("density", "densfun"), bty="n", y.intersp=0.4,
 lwd=6, bg="white", col=c("blue", "red"))

# Plot histogram
histp <- hist(retp, breaks=100, freq=FALSE,
  xlim=c(-5*madv, 5*madv), xlab="", ylab="",
  main="VTI Return Distribution")
# Draw kernel density of histogram
lines(densv, col="red", lwd=2)
# Add density of normal distribution
curve(expr=dnorm(x, mean=mean(retp), sd=sd(retp)),
add=TRUE, lwd=2, col="blue")
# Add legend
legend("topright", inset=0.05, cex=0.8, title=NULL,
 leg=c("VTI", "Normal"), bty="n", y.intersp=0.4,
 lwd=6, bg="white", col=c("red", "blue"))

# Create normal Q-Q plot
qqnorm(retp, ylim=c(-0.1, 0.1), main="VTI Q-Q Plot",
 xlab="Normal Quantiles")
# Fit a line to the normal quantiles
qqline(retp, col="red", lwd=2)
# Perform Shapiro-Wilk test
shapiro.test(retp)

# Boxplot method for formula
boxplot(formula=mpg ~ cyl, data=mtcars,
  main="Mileage by number of cylinders",
  xlab="Cylinders", ylab="Miles per gallon")
# Boxplot method for data frame of EuStockMarkets percentage returns
boxplot(x=diff(log(EuStockMarkets)))

# Calculate VTI percentage returns
retp <- na.omit(rutils::etfenv$returns$VTI)
# Number of observations
nrows <- NROW(retp)
# Mean of VTI returns
retm <- mean(retp)
# Standard deviation of VTI returns
stdev <- sd(retp)
# Skewness of VTI returns
nrows/((nrows-1)*(nrows-2))*sum(((retp - retm)/stdev)^3)
# Kurtosis of VTI returns
nrows*(nrows+1)/((nrows-1)^3)*sum(((retp - retm)/stdev)^4)
# Random normal returns
retp <- rnorm(nrows, sd=stdev)
# Mean and standard deviation of random normal returns
retm <- mean(retp)
stdev <- sd(retp)
# Skewness of random normal returns
nrows/((nrows-1)*(nrows-2))*sum(((retp - retm)/stdev)^3)
# Kurtosis of random normal returns
nrows*(nrows+1)/((nrows-1)^3)*sum(((retp - retm)/stdev)^4)

# calc_skew() calculates skew of returns
calc_skew <- function(retp) {
  retp <- na.omit(retp)
  sum(((retp - mean(retp))/sd(retp))^3)/NROW(retp)
}  # end calc_skew
# calc_kurt() calculates kurtosis of returns
calc_kurt <- function(retp) {
  retp <- na.omit(retp)
  sum(((retp - mean(retp))/sd(retp))^4)/NROW(retp)
}  # end calc_kurt
# Calculate skew and kurtosis of VTI returns
calc_skew(retp)
calc_kurt(retp)
# calc_mom() calculates the moments of returns
calc_mom <- function(retp, moment=3) {
  retp <- na.omit(retp)
  sum(((retp - mean(retp))/sd(retp))^moment)/NROW(retp)
}  # end calc_mom
# Calculate skew and kurtosis of VTI returns
calc_mom(retp, moment=3)
calc_mom(retp, moment=4)

# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
# Sample from Standard Normal Distribution
nrows <- 1000
datav <- rnorm(nrows)
# Sample mean
mean(datav)
# Sample standard deviation
sd(datav)
# Standard error of sample mean
sd(datav)/sqrt(nrows)

xvar <- seq(-5, 7, length=100)
yvar <- dnorm(xvar, mean=1.0, sd=2.0)
plot(xvar, yvar, type="l", lty="solid", xlab="", ylab="")
title(main="Normal Density Function", line=0.5)
startp <- 3; endd <- 5  # Set lower and upper bounds
# Set polygon base
subv <- ((xvar >= startp) & (xvar <= endd))
polygon(c(startp, xvar[subv], endd),  # Draw polygon
  c(-1, yvar[subv], -1), col="red")

par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
sigmavs <- c(0.5, 1, 1.5, 2)  # Sigma values
# Create plot colors
colorv <- c("red", "black", "blue", "green")
# Create legend labels
labelv <- paste("sigma", sigmavs, sep="=")
for (it in 1:4) {  # Plot four curves
  curve(expr=dnorm(x, sd=sigmavs[it]),
  xlim=c(-4, 4), xlab="", ylab="", lwd=2,
  col=colorv[it], add=as.logical(it-1))
}  # end for
# Add title
title(main="Normal Distributions", line=0.5)
# Add legend
legend("topright", inset=0.05, title="Sigmas", y.intersp=0.4,
 labelv, cex=0.8, lwd=2, lty=1, bty="n", col=colorv)

x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
degf <- c(3, 6, 9)  # Df values
colorv <- c("black", "red", "blue", "green")
labelv <- c("normal", paste("df", degf, sep="="))
# Plot a Normal probability distribution
curve(expr=dnorm, xlim=c(-4, 4), xlab="", ylab="", lwd=2)
for (it in 1:3) {  # Plot three t-distributions
  curve(expr=dt(x, df=degf[it]), xlab="", ylab="",
lwd=2, col=colorv[it+1], add=TRUE)
}  # end for

# Add title
title(main="t-distributions", line=0.5)
# Add legend
legend("topright", inset=0.05, bty="n", y.intersp=0.4,
       title="Degrees\n of freedom", labelv,
       cex=0.8, lwd=6, lty=1, col=colorv)

# Mixture of two normal distributions with sd=1 and sd=2
nrows <- 1e5
retp <- c(rnorm(nrows/2), 2*rnorm(nrows/2))
retp <- (retp-mean(retp))/sd(retp)
# Kurtosis of normal
calc_kurt(rnorm(nrows))
# Kurtosis of mixture
calc_kurt(retp)
# Or
nrows*sum(retp^4)/(nrows-1)^2

x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
# Plot the distributions
plot(density(retp), xlab="", ylab="",
  main="Mixture of Normal Returns",
  xlim=c(-3, 3), type="l", lwd=3, col="red")
curve(expr=dnorm, lwd=2, col="blue", add=TRUE)
curve(expr=dt(x, df=3), lwd=2, col="green", add=TRUE)
# Add legend
legend("topright", inset=0.05, lty=1, lwd=6, bty="n",
  legend=c("Mixture", "Normal", "t-distribution"), y.intersp=0.4,
  col=c("red", "blue", "green"))

dev.new(width=6, height=5, noRStudioGD=TRUE)
# x11(width=6, height=5)
# Define density of non-standard t-distribution
tdistr <- function(x, dfree, locv=0, scalev=1) {
  dt((x-locv)/scalev, df=dfree)/scalev
}  # end tdistr
# Or
tdistr <- function(x, dfree, locv=0, scalev=1) {
  gamma((dfree+1)/2)/(sqrt(pi*dfree)*gamma(dfree/2)*scalev)*
    (1+((x-locv)/scalev)^2/dfree)^(-(dfree+1)/2)
}  # end tdistr
# Calculate vector of scale values
scalev <- c(0.5, 1.0, 2.0)
colorv <- c("blue", "black", "red")
labelv <- paste("scale", format(scalev, digits=2), sep="=")
# Plot three t-distributions
for (it in 1:3) {
  curve(expr=tdistr(x, dfree=3, scalev=scalev[it]), xlim=c(-3, 3),
xlab="", ylab="", lwd=2, col=colorv[it], add=(it>1))
}  # end for

# Add title
title(main="t-distributions with Different Scale Parameters", line=0.5)
# Add legend
legend("topright", inset=0.05, bty="n", title="Scale Parameters", labelv,
       cex=0.8, lwd=6, lty=1, col=colorv, y.intersp=0.4)

# Calculate VTI percentage returns
library(rutils)
retp <- as.numeric(na.omit(rutils::etfenv$returns$VTI))[1:499]
# Reduce number of output digits
ndigits <- options(digits=5)
# Shapiro-Wilk test for normal distribution
nrows <- NROW(retp)
shapiro.test(rnorm(nrows))
# Shapiro-Wilk test for VTI returns
shapiro.test(retp)
# Shapiro-Wilk test for uniform distribution
shapiro.test(runif(nrows))
# Restore output digits
options(digits=ndigits$digits)

library(tseries)  # Load package tseries
# Jarque-Bera test for normal distribution
jarque.bera.test(rnorm(nrows))
# Jarque-Bera test for VTI returns
jarque.bera.test(retp)
# Jarque-Bera test for uniform distribution
jarque.bera.test(runif(NROW(retp)))

# KS test for normal distribution
ks_test <- ks.test(rnorm(100), pnorm)
ks_test$p.value
# KS test for uniform distribution
ks.test(runif(100), pnorm)
# KS test for two shifted normal distributions
ks.test(rnorm(100), rnorm(100, mean=0.1))
ks.test(rnorm(100), rnorm(100, mean=1.0))
# KS test for two different normal distributions
ks.test(rnorm(100), rnorm(100, sd=2.0))
# KS test for VTI returns vs normal distribution
retp <- as.numeric(na.omit(rutils::etfenv$returns$VTI))
retp <- (retp - mean(retp))/sd(retp)
ks.test(retp, pnorm)

x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
# Degrees of freedom
degf <- c(2, 5, 8, 11)
# Plot four curves in loop
colorv <- c("red", "black", "blue", "green")
for (it in 1:4) {
  curve(expr=dchisq(x, df=degf[it]),
  xlim=c(0, 20), ylim=c(0, 0.3),
  xlab="", ylab="", col=colorv[it],
  lwd=2, add=as.logical(it-1))
}  # end for

# Add title
title(main="Chi-squared Distributions", line=0.5)
# Add legend
labelv <- paste("df", degf, sep="=")
legend("topright", inset=0.05, bty="n", y.intersp=0.4,
       title="Degrees of freedom", labelv,
       cex=0.8, lwd=6, lty=1, col=colorv)

# Observed frequencies from random normal data
histp <- hist(rnorm(1e3, mean=0), breaks=100, plot=FALSE)
countsn <- histp$counts
# Theoretical frequencies
countst <- rutils::diffit(pnorm(histp$breaks))
# Perform Chi-squared test for normal data
chisq.test(x=countsn, p=countst, rescale.p=TRUE, simulate.p.value=TRUE)
# Return p-value
chisqtest <- chisq.test(x=countsn, p=countst, rescale.p=TRUE, simulate.p.value=TRUE)
chisqtest$p.value
# Observed frequencies from shifted normal data
histp <- hist(rnorm(1e3, mean=2), breaks=100, plot=FALSE)
countsn <- histp$counts/sum(histp$counts)
# Theoretical frequencies
countst <- rutils::diffit(pnorm(histp$breaks))
# Perform Chi-squared test for shifted normal data
chisq.test(x=countsn, p=countst, rescale.p=TRUE, simulate.p.value=TRUE)
# Calculate histogram of VTI returns
histp <- hist(retp, breaks=100, plot=FALSE)
countsn <- histp$counts
# Calculate cumulative probabilities and then difference them
countst <- pt((histp$breaks-locv)/scalev, df=2)
countst <- rutils::diffit(countst)
# Perform Chi-squared test for VTI returns
chisq.test(x=countsn, p=countst, rescale.p=TRUE, simulate.p.value=TRUE)

# Objective function from function dt()
likefun <- function(par, dfree, datav) {
  -sum(log(dt(x=(datav-par[1])/par[2], df=dfree)/par[2]))
}  # end likefun
# Demonstrate equivalence with log(dt())
likefun(c(1, 0.5), 2, 2:5)
-sum(log(dt(x=(2:5-1)/0.5, df=2)/0.5))
# Objective function is negative log-likelihood
likefun <- function(par, dfree, datav) {
  sum(-log(gamma((dfree+1)/2)/(sqrt(pi*dfree)*gamma(dfree/2))) +
    log(par[2]) + (dfree+1)/2*log(1+((datav-par[1])/par[2])^2/dfree))
}  # end likefun

# Calculate VTI percentage returns
retp <- as.numeric(na.omit(rutils::etfenv$returns$VTI))
# Fit VTI returns using MASS::fitdistr()
fitobj <- MASS::fitdistr(retp, densfun="t", df=3)
summary(fitobj)
# Fitted parameters
fitobj$estimate
locv <- fitobj$estimate[1]
scalev <- fitobj$estimate[2]
locv; scalev
# Standard errors of parameters
fitobj$sd
# Log-likelihood value
fitobj$value
# Fit distribution using optim()
initp <- c(mean=0, scale=0.01)  # Initial parameters
fitobj <- optim(par=initp,
  fn=likefun, # Log-likelihood function
  datav=retp,
  dfree=3, # Degrees of freedom
  method="L-BFGS-B", # Quasi-Newton method
  upper=c(1, 0.1), # Upper constraint
  lower=c(-1, 1e-7)) # Lower constraint
# Optimal parameters
locv <- fitobj$par["mean"]
scalev <- fitobj$par["scale"]
locv; scalev

dev.new(width=6, height=5, noRStudioGD=TRUE)
# x11(width=6, height=5)
# Plot histogram of VTI returns
madv <- mad(retp)
histp <- hist(retp, col="lightgrey",
  xlab="returns", breaks=100, xlim=c(-5*madv, 5*madv),
  ylab="frequency", freq=FALSE, main="Histogram of VTI Returns")
lines(density(retp, adjust=1.5), lwd=3, col="blue")
# Plot the Normal probability distribution
curve(expr=dnorm(x, mean=mean(retp),
  sd=sd(retp)), add=TRUE, lwd=3, col="green")
# Define non-standard t-distribution
tdistr <- function(x, dfree, locv=0, scalev=1) {
  dt((x-locv)/scalev, df=dfree)/scalev
}  # end tdistr
# Plot t-distribution function
curve(expr=tdistr(x, dfree=3, locv=locv, scalev=scalev), col="red", lwd=3, add=TRUE)
# Add legend
legend("topright", inset=0.05, bty="n", y.intersp=0.4,
  leg=c("density", "t-distr", "normal"),
  lwd=6, lty=1, col=c("blue", "red", "green"))

# Calculate sample from non-standard t-distribution with df=3
datat <- locv + scalev*rt(NROW(retp), df=3)
# Q-Q plot of VTI Returns vs non-standard t-distribution
qqplot(datat, retp, xlab="t-Dist Quantiles", ylab="VTI Quantiles",
       main="Q-Q plot of VTI Returns vs Student's t-distribution")
# Calculate quartiles of the distributions
probs <- c(0.25, 0.75)
qrets <- quantile(retp, probs)
qtdata <- quantile(datat, probs)
# Calculate slope and plot line connecting quartiles
slope <- diff(qrets)/diff(qtdata)
intercept <- qrets[1]-slope*qtdata[1]
abline(intercept, slope, lwd=2, col="red")

# KS test for VTI returns vs t-distribution data
ks.test(retp, datat)
# Define cumulative distribution of non-standard t-distribution
ptdistr <- function(x, dfree, locv=0, scalev=1) {
  pt((x-locv)/scalev, df=dfree)
}  # end ptdistr
# KS test for VTI returns vs cumulative t-distribution
ks.test(sample(retp, replace=TRUE), ptdistr, dfree=3, locv=locv, scalev=scalev)

# Plot log density of VTI returns
plot(density(retp, adjust=4), xlab="VTI Returns", ylab="Density",
     main="Fat Left Tail of VTI Returns (density in log scale)",
     type="l", lwd=3, col="blue", xlim=c(min(retp), -0.02), log="y")
# Plot t-distribution function
curve(expr=dt((x-locv)/scalev, df=3)/scalev, lwd=3, col="red", add=TRUE, log="y")
# Plot the Normal probability distribution
curve(expr=dnorm(x, mean=mean(retp), sd=sd(retp)), lwd=3, col="green", add=TRUE, log="y")
# Add legend
legend("topleft", inset=0.01, bty="n", y.intersp=c(0.25, 0.25, 0.25),
  legend=c("density", "t-distr", "normal"), y.intersp=0.4,
  lwd=6, lty=1, col=c("blue", "red", "green"))

# Calculate VTI returns and trading volumes
ohlc <- rutils::etfenv$VTI
closep <- drop(coredata(quantmod::Cl(ohlc)))
retp <- rutils::diffit(log(closep))
volumv <- coredata(quantmod::Vo(ohlc))
# Calculate trailing variance
lookb <- 121
varv <- HighFreq::roll_var_ohlc(log(ohlc), method="close", lookb=lookb, scale=FALSE)
varv[1:lookb, ] <- varv[lookb+1, ]
# Calculate trailing average volume
volumr <- HighFreq::roll_sum(volumv, lookb=lookb)/lookb
# dygraph plot of VTI variance and trading volumes
datav <- xts::xts(cbind(varv, volumr), zoo::index(ohlc))
colv <- c("variance", "volume")
colnames(datav) <- colv
dygraphs::dygraph(datav, main="VTI Variance and Trading Volumes") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], strokeWidth=2, axis="y", col="blue") %>%
  dySeries(name=colv[2], strokeWidth=2, axis="y2", col="red") %>%
  dyLegend(show="always", width=500)

# Scale the returns using volume clock to trading time
retsc <- ifelse(volumv > 0, sqrt(volumr)*retp/sqrt(volumv), 0)
retsc <- sd(retp)*retsc/sd(retsc)
# retsc <- ifelse(volumv > 1e4, retp/volumv, 0)
# Calculate moments of scaled returns
nrows <- NROW(retp)
sapply(list(retp=retp, retsc=retsc),
  function(rets) {sapply(c(skew=3, kurt=4),
     function(x) sum((rets/sd(rets))^x)/nrows)
})  # end sapply

# x11(width=6, height=5)
dev.new(width=6, height=5, noRStudioGD=TRUE)
par(mar=c(3, 3, 2, 1), oma=c(1, 1, 1, 1))
# Plot densities of SPY returns
madv <- mad(retp)
# bwidth <- mad(rutils::diffit(retp))
plot(density(retp, bw=madv/10), xlim=c(-5*madv, 5*madv),
     lwd=3, mgp=c(2, 1, 0), col="blue",
     xlab="returns (standardized)", ylab="frequency",
     main="Density of Volume-scaled VTI Returns")
lines(density(retsc, bw=madv/10), lwd=3, col="red")
curve(expr=dnorm(x, mean=mean(retp), sd=sd(retp)),
add=TRUE, lwd=3, col="green")
# Add legend
legend("topright", inset=0.05, bty="n", y.intersp=0.4,
  leg=c("unscaled", "scaled", "normal"),
  lwd=6, lty=1, col=c("blue", "red", "green"))
quartz.save("figure/vti_scaled.png", type="png", width=6, height=5)

# Load package PerformanceAnalytics
library(PerformanceAnalytics)
# Get documentation for package PerformanceAnalytics
# Get short description
packageDescription("PerformanceAnalytics")
# Load help page
help(package="PerformanceAnalytics")
# List all objects in PerformanceAnalytics
ls("package:PerformanceAnalytics")
# List all datasets in PerformanceAnalytics
data(package="PerformanceAnalytics")
# Remove PerformanceAnalytics from search path
detach("package:PerformanceAnalytics")

perfstats <- unclass(data(
    package="PerformanceAnalytics"))$results[, -(1:2)]
apply(perfstats, 1, paste, collapse=" - ")
# Load "managers" data set
data(managers)
class(managers)
dim(managers)
head(managers, 3)

# Load package "PerformanceAnalytics"
library(PerformanceAnalytics)
# Calculate ETF returns
retp <- rutils::etfenv$returns[, c("VTI", "DBC", "IEF")]
retp <- na.omit(retp)
# Plot cumulative ETF returns
x11(width=6, height=5)
chart.CumReturns(retp, lwd=2, ylab="",
  legend.loc="topleft", main="ETF Cumulative Returns")

retp <- na.omit(rutils::etfenv$returns$VTI)
chart.Histogram(retp, xlim=c(-0.04, 0.04),
  colorset = c("lightgray", "red", "blue"), lwd=3,
  main=paste("Distribution of", colnames(retp), "Returns"),
  methods = c("add.density", "add.normal"))
legend("topright", inset=0.05, bty="n", y.intersp=0.4,
 leg=c("VTI Density", "Normal"),
 lwd=6, lty=1, col=c("red", "blue"))

retp <- rutils::etfenv$returns[,
  c("VTI", "IEF", "IVW", "VYM", "IWB", "DBC", "VXX")]
x11(width=6, height=5)
chart.Boxplot(names=FALSE, retp)
par(cex.lab=0.8, cex.axis=0.8)
axis(side=2, at=(1:NCOL(retp))/7.5-0.05,labels=colnames(retp))

# Simulate normally distributed data
nrows <- 1000
datav <- rnorm(nrows)
sd(datav)
mad(datav)
median(abs(datav - median(datav)))
median(abs(datav - median(datav)))/qnorm(0.75)
# Bootstrap of sd and mad estimators
bootd <- sapply(1:10000, function(x) {
  samplev <- datav[sample.int(nrows, replace=TRUE)]
  c(sd=sd(samplev), mad=mad(samplev))
})  # end sapply
bootd <- t(bootd)
# Analyze bootstrapped variance
head(bootd)
sum(is.na(bootd))
# Means and standard errors from bootstrap
apply(bootd, MARGIN=2, function(x)
  c(mean=mean(x), stderror=sd(x)))
# Parallel bootstrap under Windows
library(parallel)  # Load package parallel
ncores <- detectCores() - 1  # Number of cores
compclust <- makeCluster(ncores)  # Initialize compute cluster
bootd <- parLapply(compclust, 1:10000,
  function(x, datav) {
    samplev <- datav[sample.int(nrows, replace=TRUE)]
    c(sd=sd(samplev), mad=mad(samplev))
  }, datav=datav)  # end parLapply
# Parallel bootstrap under Mac-OSX or Linux
bootd <- mclapply(1:10000, function(x) {
    samplev <- datav[sample.int(nrows, replace=TRUE)]
    c(sd=sd(samplev), mad=mad(samplev))
  }, mc.cores=ncores)  # end mclapply
stopCluster(compclust)  # Stop R processes over cluster
bootd <- rutils::do_call(rbind, bootd)
# Means and standard errors from bootstrap
apply(bootd, MARGIN=2, function(x)
  c(mean=mean(x), stderror=sd(x)))

# Calculate VTI returns
retp <- na.omit(rutils::etfenv$returns$VTI)
nrows <- NROW(retp)
sd(retp)
mad(retp)
# Bootstrap of sd and mad estimators
bootd <- sapply(1:10000, function(x) {
  samplev <- retp[sample.int(nrows, replace=TRUE)]
  c(sd=sd(samplev), mad=mad(samplev))
})  # end sapply
bootd <- t(bootd)
# Means and standard errors from bootstrap
100*apply(bootd, MARGIN=2, function(x)
  c(mean=mean(x), stderror=sd(x)))
# Parallel bootstrap under Windows
library(parallel)  # Load package parallel
ncores <- detectCores() - 1  # Number of cores
compclust <- makeCluster(ncores)  # Initialize compute cluster
clusterExport(compclust, c("nrows", "returns"))
bootd <- parLapply(compclust, 1:10000,
  function(x) {
    samplev <- retp[sample.int(nrows, replace=TRUE)]
    c(sd=sd(samplev), mad=mad(samplev))
  })  # end parLapply
# Parallel bootstrap under Mac-OSX or Linux
bootd <- mclapply(1:10000, function(x) {
    samplev <- retp[sample.int(nrows, replace=TRUE)]
    c(sd=sd(samplev), mad=mad(samplev))
  }, mc.cores=ncores)  # end mclapply
stopCluster(compclust)  # Stop R processes over cluster
bootd <- rutils::do_call(rbind, bootd)
# Means and standard errors from bootstrap
apply(bootd, MARGIN=2, function(x)
  c(mean=mean(x), stderror=sd(x)))

library(PerformanceAnalytics)
# Define target rate of return of 50 bps
targetr <- 0.005
# Calculate the full downside returns
retsub <- (retp - targetr)
retsub <- ifelse(retsub < 0, retsub, 0)
nrows <- NROW(retsub)
# Calculate the downside deviation
all.equal(sqrt(sum(retsub^2)/nrows),
  drop(DownsideDeviation(retp, MAR=targetr, method="full")))
# Calculate the subset downside returns
retsub <- (retp - targetr)
retsub <- retsub[retsub < 0]
nrows <- NROW(retsub)
# Calculate the downside deviation
all.equal(sqrt(sum(retsub^2)/nrows),
  drop(DownsideDeviation(retp, MAR=targetr, method="subset")))

# Calculate time series of VTI drawdowns
closep <- log(quantmod::Cl(rutils::etfenv$VTI))
drawdns <- (closep - cummax(closep))
# Extract the date index from the time series closep
datev <- zoo::index(closep)
# Calculate the maximum drawdown date and depth
indexmin <- which.min(drawdns)
datemin <- datev[indexmin]
maxdd <- drawdns[datemin]
# Calculate the drawdown start and end dates
startd <- max(datev[(datev < datemin) & (drawdns == 0)])
endd <- min(datev[(datev > datemin) & (drawdns == 0)])
# dygraph plot of VTI drawdowns
datav <- cbind(closep, drawdns)
colv <- c("VTI", "Drawdowns")
colnames(datav) <- colv
dygraphs::dygraph(datav, main="VTI Drawdowns") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2],
   valueRange=(1.2*range(drawdns)+0.1), independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", col="blue") %>%
  dySeries(name=colv[2], axis="y2", col="red") %>%
  dyEvent(startd, "start drawdown", col="blue") %>%
  dyEvent(datemin, "max drawdown", col="red") %>%
  dyEvent(endd, "end drawdown", col="green")

# Plot VTI drawdowns using package quantmod
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("blue")
x11(width=6, height=5)
quantmod::chart_Series(x=closep, name="VTI Drawdowns", theme=plot_theme)
xval <- match(startd, datev)
yval <- max(closep)
abline(v=xval, col="blue")
text(x=xval, y=0.95*yval, "start drawdown", col="blue", cex=0.9)
xval <- match(datemin, datev)
abline(v=xval, col="red")
text(x=xval, y=0.9*yval, "max drawdown", col="red", cex=0.9)
xval <- match(endd, datev)
abline(v=xval, col="green")
text(x=xval, y=0.85*yval, "end drawdown", col="green", cex=0.9)

library(xtable)
library(PerformanceAnalytics)
closep <- log(quantmod::Cl(rutils::etfenv$VTI))
retp <- rutils::diffit(closep)
# Calculate table of VTI drawdowns
tablev <- PerformanceAnalytics::table.Drawdowns(retp, geometric=FALSE)
# Convert dates to strings
tablev <- cbind(sapply(tablev[, 1:3], as.character), tablev[, 4:7])
# Print table of VTI drawdowns
print(xtable(tablev), comment=FALSE, size="tiny", include.rownames=FALSE)
library(xtable)
library(PerformanceAnalytics)
closep <- log(quantmod::Cl(rutils::etfenv$VTI))
retp <- rutils::diffit(closep)
# Calculate table of VTI drawdowns
tablev <- PerformanceAnalytics::table.Drawdowns(retp, geometric=FALSE)
# Convert dates to strings
tablev <- cbind(sapply(tablev[, 1:3], as.character), tablev[, 4:7])
# Print table of VTI drawdowns
print(xtable(tablev), comment=FALSE, size="tiny", include.rownames=FALSE)

# Load "managers" data set
data(managers)
charts.PerformanceSummary(ham1,
  main="", lwd=2, ylog=TRUE)

# Calculate VTI percentage returns
retp <- na.omit(rutils::etfenv$returns$VTI)
confl <- 0.1
varisk <- quantile(retp, confl)
cvar <- mean(retp[retp <= varisk])
# Plot histogram of VTI returns
x11(width=6, height=5)
par(mar=c(3, 2, 1, 0), oma=c(0, 0, 0, 0))
histp <- hist(retp, col="lightgrey",
  xlab="returns", ylab="frequency", breaks=100,
  xlim=c(-0.05, 0.01), freq=FALSE, main="VTI Returns Histogram")
# Calculate density
densv <- density(retp, adjust=1.5)

# Plot density
lines(densv, lwd=3, col="blue")
# Plot line for VaR
abline(v=varisk, col="red", lwd=3)
text(x=varisk, y=25, labels="VaR", lwd=2, pos=2)
# Plot polygon shading for CVaR
text(x=1.5*varisk, y=10, labels="CVaR", lwd=2, pos=2)
varmax <- -0.06
rangev <- (densv$x < varisk) &  (densv$x > varmax)
polygon(c(varmax, densv$x[rangev], varisk),
  c(0, densv$y[rangev], 0), col=rgb(1, 0, 0,0.5), border=NA)

# Calculate VTI percentage returns
retp <- na.omit(rutils::etfenv$returns$VTI)
nrows <- NROW(retp)
confl <- 0.05
# Calculate VaR approximately by sorting
sortv <- sort(as.numeric(retp))
cutoff <- round(confl*nrows)
varisk <- sortv[cutoff]
# Calculate VaR as quantile
varisk <- quantile(retp, probs=confl)
# PerformanceAnalytics VaR
PerformanceAnalytics::VaR(retp, p=(1-confl), method="historical")
all.equal(unname(varisk),
  as.numeric(PerformanceAnalytics::VaR(retp,
  p=(1-confl), method="historical")))

# Calculate VaR as quantile
varisk <- quantile(retp, confl)
# Calculate CVaR as expected loss
cvar <- mean(retp[retp <= varisk])
# PerformanceAnalytics VaR
PerformanceAnalytics::ETL(retp, p=(1-confl), method="historical")
all.equal(unname(cvar),
  as.numeric(PerformanceAnalytics::ETL(retp,
    p=(1-confl), method="historical")))

# Calculate the risk-return statistics
riskstats <-
  PerformanceAnalytics::table.Stats(rutils::etfenv$returns)
class(riskstats)
# Transpose the data frame
riskstats <- as.data.frame(t(riskstats))
# Add Name column
riskstats$Name <- rownames(riskstats)
# Add Sharpe ratio column
riskstats$"Arithmetic Mean" <-
  sapply(rutils::etfenv$returns, mean, na.rm=TRUE)
riskstats$Sharpe <-
  sqrt(252)*riskstats$"Arithmetic Mean"/riskstats$Stdev
# Sort on Sharpe ratio
riskstats <- riskstats[order(riskstats$Sharpe, decreasing=TRUE), ]

# Copy from rutils to save time
riskstats <- rutils::etfenv$riskstats
# Add Sharpe ratio column
# riskstats$Sharpe <- riskstats$"Arithmetic Mean"/riskstats$Stdev
# Sort on Sharpe ratio
riskstats <- riskstats[order(riskstats$Sharpe, decreasing=TRUE), ]
# Print data frame
knitr::kable(riskstats[, c("Sharpe", "Skewness", "Kurtosis")])

# Print data frame
knitr::kable(riskstats[c("VXX", "SVXY"), c("Sharpe", "Skewness", "Kurtosis")])

# dygraph plot of VXX versus SVXY
pricev <- na.omit(rutils::etfenv$prices[, c("VXX", "SVXY")])
pricev <- pricev["2017/"]
colv <- c("VXX", "SVXY")
colnames(pricev) <- colv
dygraphs::dygraph(pricev, main="Prices of VXX and SVXY") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=colv[2], axis="y2", strokeWidth=2, col="green") %>%
  dyLegend(show="always", width=300) %>% dyLegend(show="always", width=300) %>%
  dyLegend(show="always", width=300)

# Remove VIX volatility ETF data
riskstats <- riskstats[-match(c("VXX", "SVXY"), riskstats$Name), ]
# Plot scatterplot of Sharpe vs Skewness
plot(Sharpe ~ Skewness, data=riskstats,
     ylim=1.1*range(riskstats$Sharpe),
     main="Sharpe vs Skewness")
# Add labels
text(x=riskstats$Skewness, y=riskstats$Sharpe,
    labels=riskstats$Name, pos=3, cex=0.8)
# Plot scatterplot of Kurtosis vs Skewness
x11(width=6, height=5)
par(mar=c(4, 4, 2, 1), oma=c(0, 0, 0, 0))
plot(Kurtosis ~ Skewness, data=riskstats,
     ylim=c(1, max(riskstats$Kurtosis)),
     main="Kurtosis vs Skewness")
# Add labels
text(x=riskstats$Skewness, y=riskstats$Kurtosis,
    labels=riskstats$Name, pos=1, cex=0.5)

library(PerformanceAnalytics)
retp <- rutils::etfenv$returns[, c("VTI", "IEF")]
retp <- na.omit(retp)
# Calculate the Sharpe ratio
confl <- 0.05
PerformanceAnalytics::SharpeRatio(retp, p=(1-confl),
  method="historical")
# Calculate the Sortino ratio
PerformanceAnalytics::SortinoRatio(retp)
# Calculate the Calmar ratio
PerformanceAnalytics::CalmarRatio(retp)
# Calculate the Dowd ratio
PerformanceAnalytics::SharpeRatio(retp, FUN="VaR",
  p=(1-confl), method="historical")
# Calculate the Dowd ratio from scratch
varisk <- sapply(retp, quantile, probs=confl)
-sapply(retp, mean)/varisk
# Calculate the Conditional Dowd ratio
PerformanceAnalytics::SharpeRatio(retp, FUN="ES",
  p=(1-confl), method="historical")
# Calculate the Conditional Dowd ratio from scratch
cvar <- sapply(retp, function(x) {
  mean(x[x < quantile(x, confl)])
})
-sapply(retp, mean)/cvar

# Calculate VTI daily percentage returns
retp <- na.omit(rutils::etfenv$returns$VTI)
nrows <- NROW(retp)
# Bootstrap aggregated monthly VTI returns
holdp <- 22
reta <- sqrt(holdp)*sapply(1:nrows, function(x) {
    mean(retp[sample.int(nrows, size=holdp, replace=TRUE)])
})  # end sapply
# Calculate mean, standard deviation, skewness, and kurtosis
datav <- cbind(retp, reta)
colnames(datav) <- c("VTI", "Agg")
sapply(datav, function(x) {
  # Standardize the returns
  meanv <- mean(x); stdev <- sd(x); x <- (x - meanv)/stdev
  c(mean=meanv, stdev=stdev, skew=mean(x^3), kurt=mean(x^4))
})  # end sapply
# Calculate the Sharpe and Dowd ratios
confl <- 0.02
ratiom <- sapply(datav, function(x) {
  stdev <- sd(x)
  varisk <- unname(quantile(x, probs=confl))
  cvar <- mean(x[x < varisk])
  mean(x)/c(Sharpe=stdev, Dowd=-varisk, DowdC=-cvar)
})  # end sapply
# Annualize the daily risk
ratiom[, 1] <- sqrt(22)*ratiom[, 1]
ratiom

# Plot the densities of returns
plot(density(retp), t="l", lwd=3, col="blue",
     xlab="returns", ylab="density", xlim=c(-0.04, 0.04),
     main="Distribution of Aggregated Stock Returns")
lines(density(reta), t="l", col="red", lwd=3)
curve(expr=dnorm(x, mean=mean(reta), sd=sd(reta)), col="green", lwd=3, add=TRUE)
legend("topright", legend=c("VTI Daily", "Aggregated", "Normal"), y.intersp=0.4,
 inset=-0.1, bg="white", lty=1, lwd=6, col=c("blue", "red", "green"), bty="n")
