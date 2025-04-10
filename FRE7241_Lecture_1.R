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
etfenv$pricev <- pricev
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
colnames(sp500env$LOW) <- paste("LOWES", namev, sep=".")
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
retp <- xts::diff.xts(pricev)/rutils::lagit(pricev, pad_zeros=FALSE)
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
filev <- lapply(ls(sp500env), function(symboln) {
  xtsv <- get(symboln, envir=sp500env)
  zoo::write.zoo(xtsv, file=paste0(dirn, symboln, ".csv"))
  symboln
})  # end lapply
unlist(filev)
# Or using eapply() and data.table::fwrite()
filev <- eapply(sp500env , function(xtsv) {
  filen <- rutils::get_name(colnames(xtsv)[1])
  data.table::fwrite(data.table::as.data.table(xtsv), file=paste0(dirn, filen, ".csv"))
  filen
})  # end eapply
unlist(filev)

# Load the environment from compressed .RData file
dirn <- "/Users/jerzy/Develop/lecture_slides/data/"
load(file=paste0(dirn, "sp500.RData"))
# Get all the .csv file names in the directory
dirn <- "/Users/jerzy/Develop/lecture_slides/data/SP500/"
filev <- Sys.glob(paste0(dirn, "*.csv"))
# Create new environment for data
sp500env <- new.env()
for (filen in filev) {
  xtsv <- xts::as.xts(zoo::read.csv.zoo(filen))
  symboln <- rutils::get_name(colnames(xtsv)[1])
  # symboln <- strsplit(colnames(xtsv), split="[.]")[[1]][1]
  assign(symboln, xtsv, envir=sp500env)
}  # end for
# Or using fread()
for (filen in filev) {
  xtsv <- data.table::fread(filen)
  data.table::setDF(xtsv)
  xtsv <- xts::xts(xtsv[, -1], as.Date(xtsv[, 1]))
  symboln <- rutils::get_name(colnames(xtsv)[1])
  assign(symboln, xtsv, envir=sp500env)
}  # end for

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
retp <- xts::diff.xts(pricev)/rutils::lagit(pricev, pad_zeros=FALSE)
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

# Extract Close prices
pricev <- eapply(etfenv, quantmod::Cl)
pricev <- do.call(cbind, pricev)
# Drop ".Close" from colnames
colnames(pricev) <- do.call(rbind, strsplit(colnames(pricev), split="[.]"))[, 1]
# Calculate the log returns
retp <- xts::diff.xts(log(pricev))
# Copy prices and returns into etfenv
etfenv$pricev <- pricev
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

# Read CBOE futures expiration dates
datev <- read.csv(file="/Users/jerzy/Develop/lecture_slides/data/futures_expiration_dates_codes.csv",
  row.names=1)
# Create directory for data
dirn <- "/Users/jerzy/Develop/data/vix_data"
dir.create(dirn)
namev <- rownames(datev)
filev <- file.path(dirn, paste0(namev, ".csv"))
filelog <- file.path(dirn, "log_file.txt")
urlcboe <- "https://markets.cboe.com/us/futures/market_statistics/historical_data/products/csv/VX/"
urlv <- paste0(urlcboe, datev[, 1])
# Download files in loop
for (it in seq_along(urlv)) {
    tryCatch(  # Warning and error handler
  download.file(urlv[it], destfile=filev[it], quiet=TRUE),
# Warning handler captures warning condition
warning=function(msg) {
  cat(paste0("Warning handler: ", msg, "\n"), file=filelog, append=TRUE)
},  # end warning handler
# Error handler captures error condition
error=function(msg) {
  cat(paste0("Error handler: ", msg, "\n"), append=TRUE)
},  # end error handler
finally=cat(paste0("Processing file name = ", filev[it], "\n"), append=TRUE)
    )  # end tryCatch
}  # end for

# Create new environment for data
vixenv <- new.env()
# Download VIX data for the months 6, 7, and 8 in 2018
library(qmao)
quantmod::getSymbols("VX", Months=1:12,
  Years=2018, src="cfe", auto.assign=TRUE, env=vixenv)
# Or
qmao::getSymbols.cfe(Symbols="VX",
  Months=6:8, Years=2018, env=vixenv,
  verbose=FALSE, auto.assign=TRUE)
# Calculate the classes of all the objects
# In the environment vixenv
unlist(eapply(vixenv, function(x) {class(x)[1]}))
class(vixenv$VX_M18)
colnames(vixenv$VX_M18)
# Save the data to a binary file called "vix_cboe.RData".
save(vixenv,
  file="/Users/jerzy/Develop/data/vix_data/vix_cboe.RData")

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

# Calculate VTI daily log returns
pricev <- log(drop(coredata(na.omit(rutils::etfenv$prices$VTI))))
retp <- rutils::diffit(pricev)
nrows <- NROW(retp)
# Calculate VTI monthly log returns
holdp <- 22 # Holding period in days
pricem <- pricev[rutils::calc_endpoints(pricev, holdp)]
retm <- rutils::diffit(pricem)
retm <- retm[-1] # Drop the first zero return
# Calculate the mean, standard deviation, skewness, and kurtosis
datav <- list(retp, retm)
names(datav) <- c("Daily", "Monthly")
do.call(cbind, lapply(datav, function(x) {
  # Standardize the returns
  meanv <- mean(x); stdev <- sd(x); x <- (x - meanv)/stdev
  c(mean=meanv, stdev=stdev, skew=mean(x^3), kurt=mean(x^4))
}))  # end lapply

# Calculate the Sharpe and Dowd ratios
do.call(cbind, lapply(datav, function(x) {
  meanv <- mean(x); stdev <- sd(x)
  varisk <- unname(quantile(x, probs=0.02))
  cvar <- mean(x[x < varisk])
  # Annualize the ratios
  sqrt(252*NROW(x)/nrows)*mean(x)/c(Sharpe=stdev, Dowd=-varisk, DowdC=-cvar)
}))  # end lapply
# Plot the density of monthly returns
plot(density(retm), t="l", lwd=3, col="blue",
     xlab="returns", ylab="density", xlim=c(-4*mad(retm), 4*mad(retm)),
     main="Distribution of Monthly VTI Returns")
curve(expr=dnorm(x, mean=mean(retm), sd=sd(retm)), col="green", lwd=3, add=TRUE)
legend("topright", legend=c("Monthly", "Normal"), y.intersp=0.4, cex=1.1,
 inset=0.0, bg="white", lty=1, lwd=6, col=c("blue", "green"), bty="n")

# Create a design matrix of IEF and VTI returns
desm <- na.omit(rutils::etfenv$returns[, c("IEF", "VTI")])
retvti <- desm$VTI
# Add returns with perfect timing skill
desm <- cbind(desm, 0.5*(retvti+abs(retvti)), retvti^2)
colnames(desm)[3:4] <- c("merton", "treynor")
# Perform Merton-Henriksson test regression
regmod <- lm(IEF ~ VTI + merton, data=desm); summary(regmod)
# Perform Treynor-Mazuy test regression
regmod <- lm(IEF ~ VTI + treynor, data=desm); summary(regmod)

# Plot residual scatterplot
resids <- (desm$IEF - regmod$coeff["VTI"]*retvti)
plot.default(x=retvti, y=resids, xlab="VTI", ylab="IEF")
title(main="Treynor-Mazuy Market Timing Test\n for IEF vs VTI", line=0.5)
# Plot fitted (predicted) response values
coefreg <- summary(regmod)$coeff
fitv <- regmod$fitted.values - coefreg["VTI", "Estimate"]*retvti
tvalue <- round(coefreg["treynor", "t value"], 2)
points.default(x=retvti, y=fitv, pch=16, col="red")
text(x=0.0, y=0.8*max(resids), paste("Treynor test t-value =", tvalue))

library(rutils)
# Extract the ETF prices from rutils::etfenv$prices
pricev <- rutils::etfenv$prices
pricev <- zoo::na.locf(pricev, na.rm=FALSE)
pricev <- zoo::na.locf(pricev, fromLast=TRUE)
datev <- zoo::index(pricev)
# Calculate the dollar returns
retd <- rutils::diffit(pricev)
# Or
# retd <- lapply(pricev, rutils::diffit)
# retd <- rutils::do_call(cbind, retd)
# Calculate the percentage returns
retp <- retd/rutils::lagit(pricev, lagg=1, pad_zeros=FALSE)
# Calculate the log returns
retl <- rutils::diffit(log(pricev))

# Set the initial dollar returns
retd[1, ] <- pricev[1, ]
# Calculate the prices from dollar returns
pricen <- cumsum(retd)
all.equal(pricen, pricev)
# Compound the percentage returns
pricen <- cumprod(1 + retp)
# Set the initial prices
prici <- as.numeric(pricev[1, ])
pricen <- lapply(1:NCOL(pricen), function (i) prici[i]*pricen[, i])
pricen <- rutils::do_call(cbind, pricen)
# pricen <- t(t(pricen)*prici)
all.equal(pricen, pricev, check.attributes=FALSE)

# Plot log VTI prices
endd <- rutils::calc_endpoints(rutils::etfenv$VTI, interval="weeks")
dygraphs::dygraph(log(quantmod::Cl(rutils::etfenv$VTI)[endd]),
  main="Logarithm of VTI Prices") %>%
  dyOptions(colors="blue", strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Calculate the percentage VTI returns
pricev <- rutils::etfenv$prices$VTI
pricev <- na.omit(pricev)
retp <- rutils::diffit(pricev)/rutils::lagit(pricev, lagg=1, pad_zeros=FALSE)

# Funding rate per day
ratef <- 0.01/252
# Margin account value
marginv <- cumsum(retp)
# Cumulative funding costs
costf <- cumsum(ratef*marginv)
# Add funding costs to margin account
marginv <- (marginv + costf)
# dygraph plot of margin and funding costs
datav <- cbind(marginv, costf)
colv <- c("Margin", "Cumulative Funding")
colnames(datav) <- colv
endd <- rutils::calc_endpoints(datav, interval="weeks")
dygraphs::dygraph(datav[endd], main="VTI Margin Funding Costs") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", col="blue") %>%
  dySeries(name=colv[2], axis="y2", col="red", strokeWidth=3) %>%
  dyLegend(show="always", width=300)

# The bid-ask spread is equal to 1 bp for liquid ETFs
bidask <- 0.001
# Cumulative transaction costs
costv <- bidask*cumsum(abs(retp))/2
# Subtract transaction costs from margin account
marginv <- cumsum(retp)
marginv <- (marginv - costv)
# dygraph plot of margin and transaction costs
datav <- cbind(marginv, costv)
colv <- c("Margin", "Transaction Costs")
colnames(datav) <- colv
dygraphs::dygraph(datav[endd], main="VTI Transaction Costs") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", col="blue") %>%
  dySeries(name=colv[2], axis="y2", col="red", strokeWidth=3) %>%
  dyLegend(show="always", width=300)

# Calculate the VTI and IEF dollar returns
pricev <- rutils::etfenv$prices[, c("VTI", "IEF")]
pricev <- na.omit(pricev)
retd <- rutils::diffit(pricev)
datev <- zoo::index(pricev)
# Calculate the VTI and IEF percentage returns
retp <- retd/rutils::lagit(pricev, lagg=1, pad_zeros=FALSE)
# Wealth of fixed shares equal to $0.5 each at start (without rebalancing)
weightv <- c(0.5, 0.5)  # dollar weights
wealthfs <- drop(cumprod(1 + retp) %*% weightv)
# Or using the dollar returns
prici <- as.numeric(pricev[1, ])
retd[1, ] <- pricev[1, ]
wealthfs2 <- cumsum(retd %*% (weightv/prici))
all.equal(wealthfs, drop(wealthfs2))

# Wealth of fixed dollars equal to $0.5 each (with rebalancing)
wealthfd <- cumsum(retp %*% weightv)
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(log(wealthfs), wealthfd)
wealthv <- xts::xts(wealthv, datev)
colnames(wealthv) <- c("Fixed shares", "Fixed dollars")
sqrt(252)*sapply(rutils::diffit(wealthv), function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot the log wealth
colv <- colnames(wealthv)
endd <- rutils::calc_endpoints(retp, interval="weeks")
dygraphs::dygraph(wealthv[endd], main="Wealth of Weighted Portfolios") %>%
  dySeries(name=colv[1], col="blue", strokeWidth=2) %>%
  dySeries(name=colv[2], col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Margin account for fixed dollars (with rebalancing)
marginv <- cumsum(retp %*% weightv)
# Cumulative transaction costs
costv <- bidask*cumsum(abs(retp) %*% weightv)/2
# Subtract transaction costs from margin account
marginv <- (marginv - costv)
# dygraph plot of margin and transaction costs
datav <- cbind(marginv, costv)
datav <- xts::xts(datav, datev)
colv <- c("Margin", "Transaction Costs")
colnames(datav) <- colv
dygraphs::dygraph(datav[endd], main="Fixed Dollar Portfolio Transaction Costs") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", col="blue") %>%
  dySeries(name=colv[2], axis="y2", col="red", strokeWidth=3) %>%
  dyLegend(show="always", width=300)

# Wealth of fixed shares (without rebalancing)
wealthfs <- cumsum(retd %*% (weightv/prici))
# Or compound the percentage returns
wealthfs <- drop(cumprod(1 + retp) %*% weightv)
# Wealth of proportional wealth strategy (with rebalancing)
wealthpr <- cumprod(1 + retp %*% weightv)
wealthv <- cbind(wealthfs, wealthpr)
wealthv <- xts::xts(wealthv, datev)
colnames(wealthv) <- c("Fixed shares", "Prop wealth")
wealthv <- log(wealthv)
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(rutils::diffit(wealthv), function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot the log wealth
dygraphs::dygraph(wealthv[endd],
  main="Wealth of Proportional Wealth Allocations") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Returns in excess of weighted returns
retw <- retp %*% weightv
retx <- lapply(retp, function(x) (x - retw))
retx <- do.call(cbind, retx)
sum(retx %*% weightv)
# Calculate the weighted sum of absolute excess returns
retx <- abs(retx) %*% weightv
# Total dollar amount of stocks that need to be traded
retx <- retx*rutils::lagit(wealthpr)
# Cumulative transaction costs
costv <- bidask*cumsum(retx)/2
# Subtract transaction costs from wealth
wealthpr <- (wealthpr - costv)

# dygraph plot of wealth and transaction costs
wealthv <- cbind(wealthpr, costv)
wealthv <- xts::xts(wealthv, datev)
colv <- c("Wealth", "Transaction Costs")
colnames(wealthv) <- colv
dygraphs::dygraph(wealthv[endd],
  main="Transaction Costs With Equal Wealths") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", col="blue") %>%
  dySeries(name=colv[2], axis="y2", col="red", strokeWidth=3) %>%
  dyLegend(show="always", width=300)

# Wealth of fixed shares (without rebalancing)
wealthfs <- drop(cumprod(1 + retp) %*% weightv)-1
# Wealth of proportional wealth (with rebalancing)
wealthpr <- cumprod(1 + retp %*% weightv) - 1
# Wealth of proportional target allocation (with rebalancing)
retp <- zoo::coredata(retp)
threshv <- 0.05
wealthv <- matrix(nrow=NROW(retp), ncol=2)
colnames(wealthv) <- colnames(retp)
wealthv[1, ] <- weightv
for (it in 2:NROW(retp)) {
  # Accrue wealth without rebalancing
  wealthv[it, ] <- wealthv[it-1, ]*(1 + retp[it, ])
  # Rebalance if wealth allocations differ from weights
  if (sum(abs(wealthv[it, ] - sum(wealthv[it, ])*weightv))/sum(wealthv[it, ]) > threshv) {
    # cat("Rebalance at:", it, "\n")
    wealthv[it, ] <- sum(wealthv[it, ])*weightv
  } # end if
} # end for
wealthv <- rowSums(wealthv) - 1
wealthv <- cbind(wealthpr, wealthv)
wealthv <- xts::xts(wealthv, datev)
colnames(wealthv) <- c("Equal Wealths", "Proportional Target")
dygraphs::dygraph(wealthv, main="Wealth of Proportional Target Allocations") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Calculate the stock and bond returns
retp <- na.omit(rutils::etfenv$returns[, c("VTI", "IEF")])
weightv <- c(0.4, 0.6)
retp <- cbind(retp, retp %*% weightv)
colnames(retp)[3] <- "Combined"
# Calculate the correlations
cor(retp)
# Calculate the Sharpe ratios
sqrt(252)*sapply(retp, function(x) mean(x)/sd(x))
# Calculate the standard deviation, skewness, and kurtosis
sapply(retp, function(x) {
  # Calculate the standard deviation
  stdev <- sd(x)
  # Standardize the returns
  x <- (x - mean(x))/stdev
  c(stdev=stdev, skew=mean(x^3), kurt=mean(x^4))
})  # end sapply

# Wealth of equal wealth strategy
wealthv <- cumsum(retp)
# Calculate the a vector of monthly end points
endd <- rutils::calc_endpoints(retp, interval="weeks")
# Plot cumulative log wealth
dygraphs::dygraph(wealthv[endd],
  main="Stocks and Bonds With Equal Wealths") %>%
  dyOptions(colors=c("blue", "green", "blue", "red")) %>%
  dySeries("Combined", color="red", strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Calculate the Sharpe ratios
sqrt(252)*sapply(retp, function(x) mean(x)/sd(x))
# Calculate the Sharpe ratios for vector of weights
weightv <- seq(0.05, 0.95, 0.05)
sharpev <- sqrt(252)*sapply(weightv, function(weight) {
  weightv <- c(weight, 1-weight)
  retp <- (retp[, 1:2] %*% weightv)
  mean(retp)/sd(retp)
})  # end sapply
# Calculate the optimal VTI weight
weightm <- weightv[which.max(sharpev)]
# Calculate the optimal weight using optimization
calc_sharpe <- function(weight) {
  weightv <- c(weight, 1-weight)
  retp <- (retp[, 1:2] %*% weightv)
  -mean(retp)/sd(retp)
}  # end calc_sharpe
optv <- optimize(calc_sharpe, interval=c(0, 1))
weightm <- optv$minimum

# Plot Sharpe ratios
plot(x=weightv, y=sharpev,
     main="Sharpe Ratio as Function of VTI Weight",
     xlab="VTI weight", ylab="Sharpe Ratio",
     t="l", lwd=3, col="blue")
abline(v=weightm, lty="dashed", lwd=1, col="blue")
text(x=weightm, y=0.7*max(sharpev), pos=4, cex=1.2,
     labels=paste("optimal VTI weight =", round(weightm, 2)))

# Coerce the log prices from xts time series to matrix
pricev <- na.omit(rutils::etfenv$prices[, c("VTI", "IEF")])
pricev <- log(zoo::coredata(pricev))
nrows <- NROW(pricev)
holdp <- 10*252 # Holding period of 10 years in days
# Sample the start dates for the bootstrap
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
startd <- sample.int(nrows-holdp, 1e3, replace=TRUE)
# Bootstrap the wealth
wealthv <- sapply(startd, function(x) {
  pricev[x+holdp-1, ] - pricev[x, ]
})  # end sapply
dim(wealthv)

# Calculate the means and standard deviations of the terminal wealths
apply(wealthv, 1, mean)
apply(wealthv, 1, sd)
# Extract the terminal wealths of VTI and IEF
vtiw <- wealthv["VTI", ]
iefw <- wealthv["IEF", ]

# Plot the densities of the terminal wealths of VTI and IEF
vtim <- mean(vtiw); iefm <- mean(iefw)
vtid <- density(vtiw); iefd <- density(iefw)
plot(vtid, col="blue", lwd=3, xlab="wealth",
     xlim=c(0, 2*max(iefd$x)), ylim=c(0, max(iefd$y)),
     main="Terminal Wealth Distributions of VTI and IEF")
lines(iefd, col="green", lwd=3)
abline(v=vtim, col="blue", lwd=2, lty="dashed")
text(x=vtim, y=0.5, labels="VTI mean", pos=4, cex=0.8)
abline(v=iefm, col="green", lwd=2, lty="dashed")
text(x=iefm, y=0.5, labels="IEF mean", pos=4, cex=0.8)
legend(x="topright", legend=c("VTI", "IEF"),
 inset=0.1, cex=1.0, bg="white", bty="n", y.intersp=0.5,
 lwd=6, lty=1, col=c("blue", "green"))

# Calculate the distributions of stock wealth
holdv <- nrows*seq(0.1, 0.5, 0.1)
wealthm <- sapply(holdv, function(holdp) {
  startd <- sample.int(nrows-holdp, 1e3, replace=TRUE)
  wealthv <- sapply(startd, function(x) {
    pricev[x+holdp-1, "VTI"] - pricev[x, "VTI"]
  })  # end sapply
})  # end sapply
dim(wealthm)
colnames(wealthm) <- paste0(round(holdv/252), "years")
# Calculate the skewness and kurtosis of the stock wealth distributions
apply(wealthm, 2, function(x) {
  # Standardize the returns
  x <- (x - mean(x))/sd(x)
  c(skew=mean(x^3), kurt=mean(x^4))
}) # end apply

# Plot the stock wealth for long and short holding periods
wealth1 <- wealthm[, 5]
wealth2 <- wealthm[, 1]
mean1 <- mean(wealth1); mean2 <- mean(wealth2)
dens1 <- density(wealth1); dens2 <- density(wealth2)
plot(dens1, col="blue", lwd=3, xlab="wealth",
     xlim=c(0, 2.5*max(dens2$x)), ylim=c(0, max(dens2$y)),
     main="Wealth Distributions for Long and Short Holding Periods")
lines(dens2, col="green", lwd=3)
abline(v=mean1, col="blue", lwd=2, lty="dashed")
text(x=mean1, y=0.5, labels="Long", pos=4, cex=0.8)
abline(v=mean2, col="green", lwd=2, lty="dashed")
text(x=mean2, y=0.5, labels="Short", pos=4, cex=0.8)
legend(x="topright", legend=c("Long", "Short"),
 inset=0.0, cex=1.0, bg="white", bty="n", y.intersp=0.5,
 lwd=6, lty=1, col=c("blue", "green"))

# Define the risk-adjusted wealth measure
riskretfun <- function(wealthv) {
  mean(wealthv)/sd(wealthv)
}  # end riskretfun
# Calculate the stock wealth risk-return ratios
riskrets <- apply(wealthm, 2, riskretfun)

# Plot the stock wealth risk-return ratios
plot(x=holdv, y=riskrets,
     main="Stock Risk-Return Ratio as Function of Holding Period",
     xlab="Holding Period", ylab="Ratio",
     t="l", lwd=3, col="blue")

# Calculate the distributions of portfolio wealth for different weights of VTI
weightv <- seq(0.05, 0.95, 0.05)
wealthm <- sapply(weightv, function(weight) {
  wealthv <- sapply(startd, function(x) {
    (pricev[x+holdp-1, ] - pricev[x, ]) %*% c(weight, 1-weight)
  })  # end sapply
})  # end sapply
dim(wealthm)
# Calculate the portfolio risk-return ratios
riskrets <- apply(wealthm, 2, riskretfun)
# Calculate the optimal VTI weight
weightm <- weightv[which.max(riskrets)]

# Plot the portfolio risk-return ratios
plot(x=weightv, y=riskrets,
     main="Portfolio Risk-Return Ratio as Function of VTI Weight",
     xlab="VTI weight", ylab="Ratio",
     t="l", lwd=3, col="blue")
abline(v=weightm, lty="dashed", lwd=1, col="blue")
text(x=weightm, y=0.5*max(riskrets), pos=3, cex=1.2,
     labels=paste("optimal VTI weight =", round(weightm, 2)))

# Extract the ETF returns
symbolv <- c("VTI", "IEF", "DBC")
retp <- na.omit(rutils::etfenv$returns[, symbolv])
# Calculate the all-weather portfolio wealth
weightaw <- c(0.30, 0.55, 0.15)
retp <- cbind(retp, retp %*% weightaw)
colnames(retp)[4] <- "All Weather"
# Calculate the Sharpe ratios
sqrt(252)*sapply(retp, function(x) mean(x)/sd(x))

# Calculate the cumulative wealth from returns
wealthv <- cumsum(retp)
# Calculate the a vector of monthly end points
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
# dygraph all-weather wealth
dygraphs::dygraph(wealthv[endd], main="All-Weather Portfolio") %>%
  dyOptions(colors=c("blue", "green", "orange", "red")) %>%
  dySeries("All Weather", color="red", strokeWidth=2) %>%
  dyLegend(show="always", width=400)
# Plot all-weather wealth
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue", "green", "red")
quantmod::chart_Series(wealthv, theme=plot_theme, lwd=c(2, 2, 2, 4),
       name="All-Weather Portfolio")
legend("topleft", legend=colnames(wealthv),
  inset=0.1, bg="white", lty=1, lwd=6, y.intersp=0.5,
  col=plot_theme$col$line.col, bty="n")

# Calculate the VTI returns
retp <- na.omit(rutils::etfenv$returns$VTI)
nrows <- NROW(retp)
# Bond floor
bfloor <- 60
# CPPI multiplier
coeff <- 2
# Portfolio market values
portfv <- numeric(nrows)
# Initial principal
portfv[1] <- 100
# Stock investment
stocki <- numeric(nrows)
stocki[1] <- max(coeff*(portfv[1] - bfloor), 0)
# Stock value
stockv <- numeric(nrows)
stockv[1] <- stocki[1]
# Bond value
bondv <- numeric(nrows)
bondv[1] <- max(portfv[1] - stocki[1], 0)
# Margin account
margv <- numeric(nrows)
# Simulate the CPPI strategy
for (t in 2:nrows) {
  # Update the portfolio value
  stocki[t] <- (1 + retp[t])*stocki[t-1]
  stockv[t] <- stocki[t] - margv[t-1]
  portfv[t] <- stockv[t] + bondv[t-1]
  # Update the CPPI leverage
  stockt <- max(coeff*(portfv[t] - bfloor), 0)
  bondv[t] <- max(portfv[t] - stockt, 0)
  margv[t] <- (bondv[t] - bondv[t-1]) + (stockt - stocki[t]) + margv[t-1]
  stocki[t] <- stockt
}  # end for

pricev <- 100*cumprod(1 + retp)
datav <- cbind(stockv, bondv, portfv, pricev)["2008/2009"]
colnames(datav) <- c("stocks", "bonds", "CPPI", "VTI")
endd <- rutils::calc_endpoints(datav, interval="weeks")
dygraphs::dygraph(datav[endd], main="CPPI Strategy") %>%
  dyOptions(colors=c("red", "green", "blue", "black"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Plot the CPPI margin account
margv <- cbind(pricev, margv)
colnames(margv)[2] <- "margin"
endd <- rutils::calc_endpoints(margv, interval="weeks")
dygraphs::dygraph(margv[endd], main="CPPI Margin and VTI") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Calculate the Sharpe of CPPI wealth
wealthv <- cbind(pricev, portfv)
colnames(wealthv)[2] <- "CPPI"
sqrt(252)*sapply(rutils::diffit(wealthv), function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot the CPPI wealth
dygraphs::dygraph(log(wealthv[endd]), main="Wealth of CPPI and VTI") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Calculate the dollar and percentage returns of VTI and IEF
pricev <- na.omit(rutils::etfenv$prices[, c("VTI", "IEF")])
datev <- zoo::index(pricev)
retd <- rutils::diffit(pricev)
retd[1, ] <- retd[2, ]
retp <- retd/rutils::lagit(pricev, lagg=1, pad_zeros=FALSE)
# Calculate the risk parity weights
weightv <- 1/sapply(retd, sd)
weightv <- weightv/sum(weightv)
# Wealth of risk parity (fixed shares)
wealthrp <- drop(pricev %*% weightv)
# Wealth of equal dollar (fixed shares)
weightv <- 1/as.numeric(pricev[1, ])
weightv <- weightv/sum(weightv)
wealthed <- (pricev %*% weightv)
# Scale the wealth to start equal to wealthrp
wealthed <- wealthrp[1]*wealthed/wealthed[1]

# Calculate the Sharpe and Sortino ratios
wealthv <- xts::xts(cbind(wealthed, wealthrp), datev)
colnames(wealthv) <- c("Equal dollar", "Risk parity")
sqrt(252)*sapply(rutils::diffit(wealthv), function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot the log wealth
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(log(wealthv[endd]),
  main="Wealth of Equal Dollar And Risk parity") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Calculate the trailing dollar volatilities
lambdav <- 0.99
vold <- HighFreq::run_var(retd, lambda=lambdav)
vold <- sqrt(vold[, 3:4])
# Calculate the rolling risk parity weights
weightv <- ifelse(vold > 0, 1/vold, 1)
weightv <- weightv/rowSums(weightv)
# Calculate the risk parity allocations
pricerp <- pricev*weightv
# Plot the risk parity allocations
colnames(pricerp) <- c("Stocks", "Bonds")
dygraph(log(pricerp[endd]), main="Risk Parity Allocations") %>%
  dyOptions(colors=c("red", "blue"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Calculate the dollar returns of risk parity
retrp <- retp*rutils::lagit(pricerp)
retrp[1, ] <- pricerp[1, ]
# Calculate the wealth of risk parity
wealthrp <- cumsum(rowSums(retrp))
# Wealth of proportional wealth (with rebalancing)
wealthpr <- cumprod(1 + rowMeans(retp))
wealthpr <- wealthpr*wealthrp[1]

# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(wealthpr, wealthrp)
wealthv <- xts::xts(wealthv, datev)
colnames(wealthv) <- c("PropWealth", "Risk Parity")
sqrt(252)*sapply(rutils::diffit(wealthv), function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot a dygraph of the log wealths
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(log(wealthv[endd]),
  main="Log of Proportional Wealth vs Risk Parity") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Test risk parity market timing of VTI using Treynor-Mazuy test
retrp <- rutils::diffit(wealthv)
retvti <- retp$VTI
desm <- cbind(retrp, retvti, retvti^2)
colnames(desm)[1:2] <- c("prop", "riskp")
colnames(desm)[4] <- "Treynor"
regmod <- lm(riskp ~ VTI + Treynor, data=desm)
summary(regmod)
# Plot residual scatterplot
resids <- regmod$residuals
plot.default(x=retvti, y=resids, xlab="VTI", ylab="residuals")
title(main="Treynor-Mazuy Market Timing Test\n for Risk Parity vs VTI", line=0.5)

# Plot fitted (predicted) response values
coefreg <- summary(regmod)$coeff
fitv <- regmod$fitted.values - coefreg["VTI", "Estimate"]*retvti
tvalue <- round(coefreg["Treynor", "t value"], 2)
points.default(x=retvti, y=fitv, pch=16, col="red")
text(x=0.0, y=0.9*max(resids), paste("Risk Parity t-value =", tvalue))
# Test for equal wealth strategy market timing of VTI using Treynor-Mazuy test
regmod <- lm(prop ~ VTI + Treynor, data=desm)
summary(regmod)
# Plot fitted (predicted) response values
coefreg <- summary(regmod)$coeff
fitv <- regmod$fitted.values - coefreg["VTI", "Estimate"]*retvti
points.default(x=retvti, y=fitv, pch=16, col="blue")
text(x=0.0, y=0.7*max(resids), paste("Prop Wealth t-value =", round(coefreg["Treynor", "t value"], 2)))

# Total dollar amount of stocks that need to be traded
notx <- rutils::diffit(pricerp)
# The bid-ask spread is equal to 1 bp for liquid ETFs
bidask <- 0.001
# Calculate the cumulative transaction costs
costv <- 0.5*bidask*cumsum(rowSums(abs(notx)))

# dygraph plot of wealth and transaction costs
wealthv <- cbind(wealthrp, wealthrp-costv)
wealthv <- xts::xts(wealthv, datev)
colv <- c("Risk Parity", "With Costs")
colnames(wealthv) <- colv
dygraphs::dygraph(wealthv[endd], main="Risk Parity Including Transaction Costs") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
