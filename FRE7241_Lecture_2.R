# Select ETF symbols for asset allocation
symbolv <- c("VTI", "VEU", "EEM", "XLY", "XLP", "XLE", "XLF",
  "XLV", "XLI", "XLB", "XLK", "XLU", "VYM", "IVW", "IWB", "IWD",
  "IWF", "IEF", "TLT", "VNQ", "DBC", "GLD", "USO", "VXX", "SVXY",
  "MTUM", "IVE", "VLUE", "QUAL", "VTV", "USMV")
# Read etf database into data frame
etflist <- read.csv(file="/Users/jerzy/Develop/lecture_slides/data/etf_list.csv")
rownames(etflist) <- etflist$Symbol
# Select from etflist only those ETF's in symbolv
etflist <- etflist[symbolv, ]
# Shorten names
etfnames <- sapply(etflist$Name, function(name) {
  namesvplit <- strsplit(name, split=" ")[[1]]
  namesvplit <- namesvplit[c(-1, -NROW(namesvplit))]
  name_match <- match("Select", namesvplit)
  if (!is.na(name_match))
    namesvplit <- namesvplit[-name_match]
  paste(namesvplit, collapse=" ")
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
pricev <- mget(symbolv, envir=rutils::etfenv)
# pricev is a list of xts series
class(pricev)
class(pricev[[1]])
tail(pricev[[1]])
# Extract close prices
pricev <- lapply(pricev, quantmod::Cl)
# Collapse list into time series the hard way
prices2 <- cbind(pricev[[1]], pricev[[2]], pricev[[3]], pricev[[4]])
class(prices2)
dim(prices2)
# Collapse list into time series using do.call()
pricev <- do.call(cbind, pricev)
all.equal(prices2, pricev)
class(pricev)
dim(pricev)
# Or extract and cbind in single step
pricev <- do.call(cbind, lapply(
  mget(symbolv, envir=rutils::etfenv), quantmod::Cl))
# Or extract and bind all data, subset by symbolv
pricev <- lapply(symbolv, function(symbol) {
    quantmod::Cl(get(symbol, envir=rutils::etfenv))
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
retsp <- (pricev-pricel)/pricel
# Calculate percentage returns using dailyReturn()
retsd <- quantmod::dailyReturn(pricev)
head(cbind(retsd, retsp))
all.equal(retsd, retsp, check.attributes=FALSE)
# Calculate returns for all prices in etfenv$prices
retsp <- lapply(etfenv$prices, function(xtsv) {
  retsd <- quantmod::dailyReturn(na.omit(xtsv))
  colnames(retsd) <- names(xtsv)
  retsd
})  # end lapply
# "retsp" is a list of xts
class(retsp)
class(retsp[[1]])
# Flatten list of xts into a single xts
retsp <- do.call(cbind, retsp)
class(retsp)
dim(retsp)
# Copy retsp into etfenv and save to .RData file
# assign("retsp", retsp, envir=etfenv)
etfenv$retsp <- retsp
save(etfenv, file="/Users/jerzy/Develop/lecture_slides/data/etf_data.RData")

library(rutils)
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
           xtsv <- quantmod::Cl(get(symbol, etfenv))
           colnames(xtsv) <- symbol
           xtsv
         })), envir=new_env)
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
         })), envir=new_env)

# Load data frame of S&P500 constituents from CSV file
sp500 <- read.csv(file="/Users/jerzy/Develop/lecture_slides/data/sp500_constituents.csv")
# Inspect data frame of S&P500 constituents
dim(sp500)
colnames(sp500)
# Extract tickers from the column Ticker
symbolv <- sp500$Ticker
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
# Rename "BRK.B" to "BRK-B" and "BF.B" to "BF-B"
symbolv[which(symbolv=="BRK.B")] <- "BRK-B"
symbolv[which(symbolv=="BF.B")] <- "BF-B"

# Load package rutils
library(rutils)
# Create new environment for data
sp500env <- new.env()
# Boolean vector of symbols already downloaded
isdownloaded <- symbolv %in% ls(sp500env)
# Download in while loop from Tiingo and copy into environment
nattempts <- 0  # Number of download attempts
while (((sum(!isdownloaded)) > 0) & (nattempts<3)) {
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
symbolv[!isdownloaded]

# The date-time index of AAPL is POSIXct
class(zoo::index(sp500env$AAPL))
# Coerce the date-time index of AAPL to Date
zoo::index(sp500env$AAPL) <- as.Date(zoo::index(sp500env$AAPL))
# Coerce all the date-time indices to Date
for (symbol in ls(sp500env)) {
  ohlc <- get(symbol, envir=sp500env)
  zoo::index(ohlc) <- as.Date(zoo::index(ohlc))
  assign(symbol, ohlc, envir=sp500env)
}  # end for

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

# Load S&P500 constituent stock prices
load("/Users/jerzy/Develop/lecture_slides/data/sp500.RData")
pricev <- eapply(sp500env, quantmod::Cl)
pricev <- rutils::do_call(cbind, pricev)
# Carry forward non-NA prices
pricev <- zoo::na.locf(pricev, na.rm=FALSE)
# Drop ".Close" from column names
colnames(pricev[, 1:4])
colnames(pricev) <- rutils::get_name(colnames(pricev))
# Or
# colnames(pricev) <- do.call(rbind,
#   strsplit(colnames(pricev), split="[.]"))[, 1]
# Calculate percentage returns of the S&P500 constituent stocks
# retsp <- xts::diff.xts(log(pricev))
retsp <- xts::diff.xts(pricev)/
  rutils::lagit(pricev, pad_zeros=FALSE)
set.seed(1121)
samplev <- sample(NCOL(retsp), s=100, replace=FALSE)
prices100 <- pricev[, samplev]
returns100 <- retsp[, samplev]
save(pricev, prices100,
  file="/Users/jerzy/Develop/lecture_slides/data/sp500_prices.RData")
save(retsp, returns100,
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
dygraphs::dygraph(datav,
  main="S&P 500 Price-weighted Index and VTI") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="red") %>%
  dySeries(name=colnamev[2], axis="y2", col="blue")

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
  xtsv <- get(symbol, envir=sp500env)
  zoo::write.zoo(xtsv, file=paste0(dir_name, symbol, ".csv"))
  symbol
})  # end lapply
unlist(file_names)
# Or using eapply() and data.table::fwrite()
file_names <- eapply(sp500env , function(xtsv) {
  file_name <- rutils::get_name(colnames(xtsv)[1])
  data.table::fwrite(data.table::as.data.table(xtsv), file=paste0(dir_name, file_name, ".csv"))
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
  xtsv <- xts::as.xts(zoo::read.csv.zoo(file_name))
  symbol <- rutils::get_name(colnames(xtsv)[1])
  # symbol <- strsplit(colnames(xtsv), split="[.]")[[1]][1]
  assign(symbol, xtsv, envir=sp500env)
}  # end for
# Or using fread()
for (file_name in file_names) {
  xtsv <- data.table::fread(file_name)
  data.table::setDF(xtsv)
  xtsv <- xts::xts(xtsv[, -1], as.Date(xtsv[, 1]))
  symbol <- rutils::get_name(colnames(xtsv)[1])
  assign(symbol, xtsv, envir=sp500env)
}  # end for

library(rutils)  # Load package rutils
# Calculate VTI percentage returns
retsp <- rutils::etfenv$returns$VTI
retsp <- drop(coredata(na.omit(retsp)))
nrows <- NROW(retsp)
# Mean and standard deviation of returns
c(mean(retsp), sd(retsp))
# Calculate the smoothing bandwidth as the MAD of returns 10 points apart
retsp <- sort(retsp)
bwidth <- 10*mad(rutils::diffit(retsp, lagg=10))
# Calculate the kernel density
densityv <- sapply(1:nrows, function(it) {
  sum(dnorm(retsp-retsp[it], sd=bwidth))
})  # end sapply
madv <- mad(retsp)
plot(retsp, densityv, xlim=c(-5*madv, 5*madv),
     t="l", col="blue", lwd=3,
     xlab="returns", ylab="density",
     main="Density of VTI Returns")
# Calculate the kernel density using density()
densityv <- density(retsp, bw=bwidth)
NROW(densityv$y)
x11(width=6, height=5)
plot(densityv, xlim=c(-5*madv, 5*madv),
     xlab="returns", ylab="density",
     col="blue", lwd=3, main="Density of VTI Returns")
# Interpolate the densityv vector into returns
densityv <- approx(densityv$x, densityv$y, xout=retsp)
all.equal(densityv$x, retsp)
plot(densityv, xlim=c(-5*madv, 5*madv),
     xlab="returns", ylab="density",
     t="l", col="blue", lwd=3,
     main="Density of VTI Returns")

# Plot histogram
histp <- hist(retsp, breaks=100, freq=FALSE,
  xlim=c(-5*madv, 5*madv), xlab="", ylab="",
  main="VTI Return Distribution")
# Draw kernel density of histogram
lines(densityv, col="red", lwd=2)
# Add density of normal distribution
curve(expr=dnorm(x, mean=mean(retsp), sd=sd(retsp)),
add=TRUE, lwd=2, col="blue")
# Add legend
legend("topright", inset=0.05, cex=0.8, title=NULL,
 leg=c("VTI", "Normal"), bty="n",
 lwd=6, bg="white", col=c("red", "blue"))

# Create normal Q-Q plot
qqnorm(retsp, ylim=c(-0.1, 0.1), main="VTI Q-Q Plot",
 xlab="Normal Quantiles")
# Fit a line to the normal quantiles
qqline(retsp, col="red", lwd=2)
# Perform Shapiro-Wilk test
shapiro.test(retsp)

# Boxplot method for formula
boxplot(formula=mpg ~ cyl, data=mtcars,
  main="Mileage by number of cylinders",
  xlab="Cylinders", ylab="Miles per gallon")
# Boxplot method for data frame of EuStockMarkets percentage returns
boxplot(x=diff(log(EuStockMarkets)))

# Calculate VTI percentage returns
retsp <- na.omit(rutils::etfenv$returns$VTI)
# Number of observations
nrows <- NROW(retsp)
# Mean of VTI returns
retsm <- mean(retsp)
# Standard deviation of VTI returns
sdrets <- sd(retsp)
# Skewness of VTI returns
nrows/((nrows-1)*(nrows-2))*
  sum(((retsp - retsm)/sdrets)^3)
# Kurtosis of VTI returns
nrows*(nrows+1)/((nrows-1)^3)*
  sum(((retsp - retsm)/sdrets)^4)
# Random normal returns
retsp <- rnorm(nrows, sd=sdrets)
# Mean and standard deviation of random normal returns
retsm <- mean(retsp)
sdrets <- sd(retsp)
# Skewness of random normal returns
nrows/((nrows-1)*(nrows-2))*
  sum(((retsp - retsm)/sdrets)^3)
# Kurtosis of random normal returns
nrows*(nrows+1)/((nrows-1)^3)*
  sum(((retsp - retsm)/sdrets)^4)

# calc_skew() calculates skew of returns
calc_skew <- function(retsp) {
  retsp <- na.omit(retsp)
  sum(((retsp - mean(retsp))/sd(retsp))^3)/NROW(retsp)
}  # end calc_skew
# calc_kurt() calculates kurtosis of returns
calc_kurt <- function(retsp) {
  retsp <- na.omit(retsp)
  sum(((retsp - mean(retsp))/sd(retsp))^4)/NROW(retsp)
}  # end calc_kurt
# Calculate skew and kurtosis of VTI returns
calc_skew(retsp)
calc_kurt(retsp)
# calcmom() calculates the moments of returns
calcmom <- function(retsp, moment=3) {
  retsp <- na.omit(retsp)
  sum(((retsp - mean(retsp))/sd(retsp))^moment)/NROW(retsp)
}  # end calcmom
# Calculate skew and kurtosis of VTI returns
calcmom(retsp, moment=3)
calcmom(retsp, moment=4)

set.seed(1121)  # Reset random number generator
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
startp <- 3; endp <- 5  # Set lower and upper bounds
# Set polygon base
subv <- ((xvar >= startp) & (xvar <= endp))
polygon(c(startp, xvar[subv], endp),  # Draw polygon
  c(-1, yvar[subv], -1), col="red")

par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
sigmavs <- c(0.5, 1, 1.5, 2)  # Sigma values
# Create plot colors
colors <- c("red", "black", "blue", "green")
# Create legend labels
labelv <- paste("sigma", sigmavs, sep="=")
for (it in 1:4) {  # Plot four curves
  curve(expr=dnorm(x, sd=sigmavs[it]),
  xlim=c(-4, 4), xlab="", ylab="", lwd=2,
  col=colors[it], add=as.logical(it-1))
}  # end for
# Add title
title(main="Normal Distributions", line=0.5)
# Add legend
legend("topright", inset=0.05, title="Sigmas",
 labelv, cex=0.8, lwd=2, lty=1, bty="n", col=colors)

x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
degf <- c(3, 6, 9)  # Df values
colors <- c("black", "red", "blue", "green")
labelv <- c("normal", paste("df", degf, sep="="))
# Plot a Normal probability distribution
curve(expr=dnorm, xlim=c(-4, 4), xlab="", ylab="", lwd=2)
for (it in 1:3) {  # Plot three t-distributions
  curve(expr=dt(x, df=degf[it]), xlab="", ylab="",
lwd=2, col=colors[it+1], add=TRUE)
}  # end for

# Add title
title(main="t-distributions", line=0.5)
# Add legend
legend("topright", inset=0.05, bty="n",
       title="Degrees\n of freedom", labelv,
       cex=0.8, lwd=6, lty=1, col=colors)

# Mixture of two normal distributions with sd=1 and sd=2
nrows <- 1e5
retsp <- c(rnorm(nrows/2), 2*rnorm(nrows/2))
retsp <- (retsp-mean(retsp))/sd(retsp)
# Kurtosis of normal
calc_kurt(rnorm(nrows))
# Kurtosis of mixture
calc_kurt(retsp)
# Or
nrows*sum(retsp^4)/(nrows-1)^2

x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
# Plot the distributions
plot(density(retsp), xlab="", ylab="",
  main="Mixture of Normal Returns",
  xlim=c(-3, 3), type="l", lwd=3, col="red")
curve(expr=dnorm, lwd=2, col="blue", add=TRUE)
curve(expr=dt(x, df=3), lwd=2, col="green", add=TRUE)
# Add legend
legend("topright", inset=0.05, lty=1, lwd=6, bty="n",
  legend=c("Mixture", "Normal", "t-distribution"),
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
colors <- c("blue", "black", "red")
labelv <- paste("scale", format(scalev, digits=2), sep="=")
# Plot three t-distributions
for (it in 1:3) {
  curve(expr=tdistr(x, dfree=3, scalev=scalev[it]), xlim=c(-3, 3),
xlab="", ylab="", lwd=2, col=colors[it], add=(it>1))
}  # end for

# Add title
title(main="t-distributions with Different Scale Parameters", line=0.5)
# Add legend
legend("topright", inset=0.05, bty="n", title="Scale Parameters", labelv,
       cex=0.8, lwd=6, lty=1, col=colors)

# Calculate VTI percentage returns
library(rutils)
retsp <- as.numeric(na.omit(rutils::etfenv$returns$VTI))[1:4999]
# Reduce number of output digits
ndigits <- options(digits=5)
# Shapiro-Wilk test for normal distribution
nrows <- NROW(retsp)
shapiro.test(rnorm(nrows))
# Shapiro-Wilk test for VTI returns
shapiro.test(retsp)
# Shapiro-Wilk test for uniform distribution
shapiro.test(runif(nrows))
# Restore output digits
options(digits=ndigits$digits)

library(tseries)  # Load package tseries
# Jarque-Bera test for normal distribution
jarque.bera.test(rnorm(nrows))
# Jarque-Bera test for VTI returns
jarque.bera.test(retsp)
# Jarque-Bera test for uniform distribution
jarque.bera.test(runif(NROW(retsp)))

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
retsp <- as.numeric(na.omit(rutils::etfenv$returns$VTI))
retsp <- (retsp - mean(retsp))/sd(retsp)
ks.test(retsp, pnorm)

x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
# Degrees of freedom
degf <- c(2, 5, 8, 11)
# Plot four curves in loop
colors <- c("red", "black", "blue", "green")
for (it in 1:4) {
  curve(expr=dchisq(x, df=degf[it]),
  xlim=c(0, 20), ylim=c(0, 0.3),
  xlab="", ylab="", col=colors[it],
  lwd=2, add=as.logical(it-1))
}  # end for

# Add title
title(main="Chi-squared Distributions", line=0.5)
# Add legend
labelv <- paste("df", degf, sep="=")
legend("topright", inset=0.05, bty="n",
       title="Degrees of freedom", labelv,
       cex=0.8, lwd=6, lty=1, col=colors)

# Observed frequencies from random normal data
histp <- hist(rnorm(1e3, mean=0), breaks=100, plot=FALSE)
countsn <- histp$counts
# Theoretical frequencies
countst <- rutils::diffit(pnorm(histp$breaks))
# Perform Chi-squared test for normal data
chisq.test(x=countsn, p=countst, rescale.p=TRUE, simulate.p.value=TRUE)
# Return p-value
chisq_test <- chisq.test(x=countsn, p=countst, rescale.p=TRUE, simulate.p.value=TRUE)
chisq_test$p.value
# Observed frequencies from shifted normal data
histp <- hist(rnorm(1e3, mean=2), breaks=100, plot=FALSE)
countsn <- histp$counts/sum(histp$counts)
# Theoretical frequencies
countst <- rutils::diffit(pnorm(histp$breaks))
# Perform Chi-squared test for shifted normal data
chisq.test(x=countsn, p=countst, rescale.p=TRUE, simulate.p.value=TRUE)
# Calculate histogram of VTI returns
histp <- hist(retsp, breaks=100, plot=FALSE)
countsn <- histp$counts
# Calculate cumulative probabilities and then difference them
countst <- pt((histp$breaks-locv)/scalev, df=2)
countst <- rutils::diffit(countst)
# Perform Chi-squared test for VTI returns
chisq.test(x=countsn, p=countst, rescale.p=TRUE, simulate.p.value=TRUE)

# Objective function from function dt()
likefun <- function(par, dfree, data) {
  -sum(log(dt(x=(data-par[1])/par[2], df=dfree)/par[2]))
}  # end likefun
# Demonstrate equivalence with log(dt())
likefun(c(1, 0.5), 2, 2:5)
-sum(log(dt(x=(2:5-1)/0.5, df=2)/0.5))
# Objective function is negative log-likelihood
likefun <- function(par, dfree, data) {
  sum(-log(gamma((dfree+1)/2)/(sqrt(pi*dfree)*gamma(dfree/2))) +
    log(par[2]) + (dfree+1)/2*log(1+((data-par[1])/par[2])^2/dfree))
}  # end likefun

# Calculate VTI percentage returns
retsp <- as.numeric(na.omit(rutils::etfenv$returns$VTI))
# Fit VTI returns using MASS::fitdistr()
fitobj <- MASS::fitdistr(retsp, densfun="t", df=3)
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
  data=retsp,
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
madv <- mad(retsp)
histp <- hist(retsp, col="lightgrey",
  xlab="returns", breaks=100, xlim=c(-5*madv, 5*madv),
  ylab="frequency", freq=FALSE, main="Histogram of VTI Returns")
lines(density(retsp, adjust=1.5), lwd=3, col="blue")
# Plot the Normal probability distribution
curve(expr=dnorm(x, mean=mean(retsp),
  sd=sd(retsp)), add=TRUE, lwd=3, col="green")
# Define non-standard t-distribution
tdistr <- function(x, dfree, locv=0, scalev=1) {
  dt((x-locv)/scalev, df=dfree)/scalev
}  # end tdistr
# Plot t-distribution function
curve(expr=tdistr(x, dfree=3, locv=locv, scalev=scalev), col="red", lwd=3, add=TRUE)
# Add legend
legend("topright", inset=0.05, bty="n",
  leg=c("density", "t-distr", "normal"),
  lwd=6, lty=1, col=c("blue", "red", "green"))

# Calculate sample from non-standard t-distribution with df=3
tdata <- scalev*rt(NROW(retsp), df=3) + locv
# Q-Q plot of VTI Returns vs non-standard t-distribution
qqplot(tdata, retsp, xlab="t-Dist Quantiles", ylab="VTI Quantiles",
       main="Q-Q plot of VTI Returns vs Student's t-distribution")
# Calculate quartiles of the distributions
probs <- c(0.25, 0.75)
qrets <- quantile(retsp, probs)
qtdata <- quantile(tdata, probs)
# Calculate slope and plot line connecting quartiles
slope <- diff(qrets)/diff(qtdata)
intercept <- qrets[1]-slope*qtdata[1]
abline(intercept, slope, lwd=2, col="red")

# KS test for VTI returns vs t-distribution data
ks.test(retsp, tdata)
# Define cumulative distribution of non-standard t-distribution
ptdistr <- function(x, dfree, locv=0, scalev=1) {
  pt((x-locv)/scalev, df=dfree)
}  # end ptdistr
# KS test for VTI returns vs cumulative t-distribution
ks.test(sample(retsp, replace=TRUE), ptdistr, dfree=3, locv=locv, scalev=scalev)

x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
# Plot histogram of VTI returns
histp <- hist(retsp, breaks=100, plot=FALSE)
plot(histp, xlab="retsp", ylab="frequency",
     col="lightgrey", freq=FALSE, main="VTI Left Tail Returns Histogram",
     xlim=c(min(retsp), -0.02),
     ylim=c(0.0, histp$density[findInterval(-0.02, histp$breaks)]))
lines(density(retsp, adjust=1.5), lwd=4, col="blue")
# Plot t-distribution function
curve(expr=dt((x-locv)/scalev, df=2)/scalev, type="l", lwd=4, col="red", add=TRUE)
# Plot the Normal probability distribution
curve(expr=dnorm(x, mean=mean(retsp), sd=sd(retsp)), add=TRUE, lwd=4, col="green")
# Add legend
legend("topleft", inset=0.05, bty="n",
  leg=c("density", "t-distr", "normal"),
  lwd=6, lty=1, col=c("blue", "red", "green"))

# Calculate VTI returns and trading volumes
ohlc <- rutils::etfenv$VTI
closep <- drop(coredata(quantmod::Cl(ohlc)))
retsp <- rutils::diffit(log(closep))
volumes <- coredata(quantmod::Vo(ohlc))
# Calculate rolling variance
look_back <- 121
variance <- HighFreq::roll_var_ohlc(log(ohlc), method="close", look_back=look_back, scale=FALSE)
variance[1:look_back, ] <- variance[look_back+1, ]
# Calculate rolling average volume
volume_roll <- HighFreq::roll_vec(volumes, look_back=look_back)/look_back
# dygraph plot of VTI variance and trading volumes
datav <- xts::xts(cbind(variance, volume_roll), zoo::index(ohlc))
colnamev <- c("variance", "volume")
colnames(datav) <- colnamev
dygraphs::dygraph(datav, main="VTI Variance and Trading Volumes") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], strokeWidth=2, axis="y", col="blue") %>%
  dySeries(name=colnamev[2], strokeWidth=2, axis="y2", col="red")

# Scale returns using volume (volume clock)
rets_scaled <- ifelse(volumes > 0,
  sqrt(volume_roll)*retsp/sqrt(volumes), 0)
rets_scaled <- sd(retsp)*rets_scaled/sd(rets_scaled)
# rets_scaled <- ifelse(volumes > 1e4, retsp/volumes, 0)
# Calculate moments of scaled returns
nrows <- NROW(retsp)
sapply(list(retsp=retsp, rets_scaled=rets_scaled),
  function(rets) {sapply(c(skew=3, kurt=4),
     function(x) sum((rets/sd(rets))^x)/nrows)
})  # end sapply

# x11(width=6, height=5)
dev.new(width=6, height=5, noRStudioGD=TRUE)
par(mar=c(3, 3, 2, 1), oma=c(1, 1, 1, 1))
# Plot densities of SPY returns
madv <- mad(retsp)
# bwidth <- mad(rutils::diffit(retsp))
plot(density(retsp, bw=madv/10), xlim=c(-5*madv, 5*madv),
     lwd=3, mgp=c(2, 1, 0), col="blue",
     xlab="returns (standardized)", ylab="frequency",
     main="Density of Volume-scaled VTI Returns")
lines(density(rets_scaled, bw=madv/10), lwd=3, col="red")
curve(expr=dnorm(x, mean=mean(retsp), sd=sd(retsp)),
add=TRUE, lwd=3, col="green")
# Add legend
legend("topright", inset=0.05, bty="n",
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

perf_data <- unclass(data(
    package="PerformanceAnalytics"))$results[, -(1:2)]
apply(perf_data, 1, paste, collapse=" - ")
# Load "managers" data set
data(managers)
class(managers)
dim(managers)
head(managers, 3)

# Load package "PerformanceAnalytics"
library(PerformanceAnalytics)
# Calculate ETF returns
retsp <- rutils::etfenv$returns[, c("VTI", "DBC", "IEF")]
retsp <- na.omit(retsp)
# Plot cumulative ETF returns
x11(width=6, height=5)
chart.CumReturns(retsp, lwd=2, ylab="",
  legend.loc="topleft", main="ETF Cumulative Returns")

retsp <- rutils::etfenv$returns$VTI
retsp <- na.omit(retsp)
x11(width=6, height=5)
chart.Histogram(retsp, xlim=c(-0.04, 0.04),
  colorset = c("lightgray", "red", "blue"), lwd=3,
  main=paste("Distribution of", colnames(retsp), "Returns"),
  methods = c("add.density", "add.normal"))
legend("topright", inset=0.05, bty="n",
 leg=c("VTI Density", "Normal"),
 lwd=6, lty=1, col=c("red", "blue"))

retsp <- rutils::etfenv$returns[,
  c("VTI", "IEF", "IVW", "VYM", "IWB", "DBC", "VXX")]
x11(width=6, height=5)
chart.Boxplot(names=FALSE, retsp)
par(cex.lab=0.8, cex.axis=0.8)
axis(side=2, at=(1:NCOL(retsp))/7.5-0.05,labels=colnames(retsp))

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
cluster <- makeCluster(ncores)  # Initialize compute cluster
bootd <- parLapply(cluster, 1:10000,
  function(x, datav) {
    samplev <- datav[sample.int(nrows, replace=TRUE)]
    c(sd=sd(samplev), mad=mad(samplev))
  }, datav=datav)  # end parLapply
# Parallel bootstrap under Mac-OSX or Linux
bootd <- mclapply(1:10000, function(x) {
    samplev <- datav[sample.int(nrows, replace=TRUE)]
    c(sd=sd(samplev), mad=mad(samplev))
  }, mc.cores=ncores)  # end mclapply
stopCluster(cluster)  # Stop R processes over cluster
bootd <- rutils::do_call(rbind, bootd)
# Means and standard errors from bootstrap
apply(bootd, MARGIN=2, function(x)
  c(mean=mean(x), stderror=sd(x)))

# Calculate VTI returns
retsp <- rutils::etfenv$returns$VTI
retsp <- na.omit(retsp)
nrows <- NROW(retsp)
sd(retsp)
mad(retsp)
# Bootstrap of sd and mad estimators
bootd <- sapply(1:10000, function(x) {
  samplev <- retsp[sample.int(nrows, replace=TRUE)]
  c(sd=sd(samplev), mad=mad(samplev))
})  # end sapply
bootd <- t(bootd)
# Means and standard errors from bootstrap
100*apply(bootd, MARGIN=2, function(x)
  c(mean=mean(x), stderror=sd(x)))
# Parallel bootstrap under Windows
library(parallel)  # Load package parallel
ncores <- detectCores() - 1  # Number of cores
cluster <- makeCluster(ncores)  # Initialize compute cluster
clusterExport(cluster, c("nrows", "returns"))
bootd <- parLapply(cluster, 1:10000,
  function(x) {
    samplev <- retsp[sample.int(nrows, replace=TRUE)]
    c(sd=sd(samplev), mad=mad(samplev))
  })  # end parLapply
# Parallel bootstrap under Mac-OSX or Linux
bootd <- mclapply(1:10000, function(x) {
    samplev <- retsp[sample.int(nrows, replace=TRUE)]
    c(sd=sd(samplev), mad=mad(samplev))
  }, mc.cores=ncores)  # end mclapply
stopCluster(cluster)  # Stop R processes over cluster
bootd <- rutils::do_call(rbind, bootd)
# Means and standard errors from bootstrap
apply(bootd, MARGIN=2, function(x)
  c(mean=mean(x), stderror=sd(x)))

library(PerformanceAnalytics)
# Define target rate of return of 50 bps
targetr <- 0.005
# Calculate the full downside returns
returns_sub <- (retsp - targetr)
returns_sub <- ifelse(returns_sub < 0, returns_sub, 0)
nrows <- NROW(returns_sub)
# Calculate the downside deviation
all.equal(sqrt(sum(returns_sub^2)/nrows),
  drop(DownsideDeviation(retsp, MAR=targetr, method="full")))
# Calculate the subset downside returns
returns_sub <- (retsp - targetr)
returns_sub <- returns_sub[returns_sub < 0]
nrows <- NROW(returns_sub)
# Calculate the downside deviation
all.equal(sqrt(sum(returns_sub^2)/nrows),
  drop(DownsideDeviation(retsp, MAR=targetr, method="subset")))

# Calculate time series of VTI drawdowns
closep <- log(quantmod::Cl(rutils::etfenv$VTI))
drawdns <- (closep - cummax(closep))
# Extract the date index from the time series closep
dates <- zoo::index(closep)
# Calculate the maximum drawdown date and depth
indexmin <- which.min(drawdns)
datemin <- dates[indexmin]
maxdd <- drawdns[datemin]
# Calculate the drawdown start and end dates
startd <- max(dates[(dates < datemin) & (drawdns == 0)])
endd <- min(dates[(dates > datemin) & (drawdns == 0)])
# dygraph plot of VTI drawdowns
datav <- cbind(closep, drawdns)
colnamev <- c("VTI", "Drawdowns")
colnames(datav) <- colnamev
dygraphs::dygraph(datav, main="VTI Drawdowns") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2],
   valueRange=(1.2*range(drawdns)+0.1), independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", col="red") %>%
  dyEvent(startd, "start drawdown", col="blue") %>%
  dyEvent(datemin, "max drawdown", col="red") %>%
  dyEvent(endd, "end drawdown", col="green")

# Plot VTI drawdowns using package quantmod
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("blue")
x11(width=6, height=5)
quantmod::chart_Series(x=closep, name="VTI Drawdowns", theme=plot_theme)
xval <- match(startd, dates)
yval <- max(closep)
abline(v=xval, col="blue")
text(x=xval, y=0.95*yval, "start drawdown", col="blue", cex=0.9)
xval <- match(datemin, dates)
abline(v=xval, col="red")
text(x=xval, y=0.9*yval, "max drawdown", col="red", cex=0.9)
xval <- match(endd, dates)
abline(v=xval, col="green")
text(x=xval, y=0.85*yval, "end drawdown", col="green", cex=0.9)

library(xtable)
library(PerformanceAnalytics)
closep <- log(quantmod::Cl(rutils::etfenv$VTI))
retsp <- rutils::diffit(closep)
# Calculate table of VTI drawdowns
tablev <- PerformanceAnalytics::table.Drawdowns(retsp, geometric=FALSE)
# Convert dates to strings
tablev <- cbind(sapply(tablev[, 1:3], as.character), tablev[, 4:7])
# Print table of VTI drawdowns
print(xtable(tablev), comment=FALSE, size="tiny", include.rownames=FALSE)
library(xtable)
library(PerformanceAnalytics)
closep <- log(quantmod::Cl(rutils::etfenv$VTI))
retsp <- rutils::diffit(closep)
# Calculate table of VTI drawdowns
tablev <- PerformanceAnalytics::table.Drawdowns(retsp, geometric=FALSE)
# Convert dates to strings
tablev <- cbind(sapply(tablev[, 1:3], as.character), tablev[, 4:7])
# Print table of VTI drawdowns
print(xtable(tablev), comment=FALSE, size="tiny", include.rownames=FALSE)

# Load "managers" data set
data(managers)
charts.PerformanceSummary(ham1,
  main="", lwd=2, ylog=TRUE)

# Calculate VTI percentage returns
retsp <- na.omit(rutils::etfenv$returns$VTI)
confl <- 0.1
varisk <- quantile(retsp, confl)
cvar <- mean(retsp[retsp < varisk])
# Plot histogram of VTI returns
x11(width=6, height=5)
par(mar=c(3, 2, 1, 0), oma=c(0, 0, 0, 0))
histp <- hist(retsp, col="lightgrey",
  xlab="returns", ylab="frequency", breaks=100,
  xlim=c(-0.05, 0.01), freq=FALSE, main="VTI Returns Histogram")
# Calculate density
densv <- density(retsp, adjust=1.5)

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
retsp <- na.omit(rutils::etfenv$returns$VTI)
confl <- 0.05
# Calculate VaR as quantile
varisk <- quantile(retsp, probs=confl)
# Or by sorting
sortv <- sort(as.numeric(retsp))
indeks <- round(confl*NROW(retsp))
varisk <- sortv[indeks]
# PerformanceAnalytics VaR
PerformanceAnalytics::VaR(retsp,
  p=(1-confl), method="historical")
all.equal(unname(varisk),
  as.numeric(PerformanceAnalytics::VaR(retsp,
  p=(1-confl), method="historical")))

x11(width=6, height=5)
par(mar=c(3, 2, 1, 0), oma=c(0, 0, 0, 0))
# Calculate VaR as quantile
varisk <- quantile(retsp, confl)
# Calculate CVaR as expected loss
cvar <- mean(retsp[retsp < varisk])
# Or by sorting
sortv <- sort(as.numeric(retsp))
indeks <- round(confl*NROW(retsp))
varisk <- sortv[indeks]
cvar <- mean(sortv[1:indeks])
# PerformanceAnalytics VaR
PerformanceAnalytics::ETL(retsp,
  p=(1-confl), method="historical")
all.equal(cvar,
  as.numeric(PerformanceAnalytics::ETL(retsp,
  p=(1-confl), method="historical")))

# Calculate the risk-return statistics
risk_ret <-
  PerformanceAnalytics::table.Stats(rutils::etfenv$returns)
class(risk_ret)
# Transpose the data frame
risk_ret <- as.data.frame(t(risk_ret))
# Add Name column
risk_ret$Name <- rownames(risk_ret)
# Add Sharpe ratio column
risk_ret$Sharpe <- risk_ret$"Arithmetic Mean"/risk_ret$Stdev
# Sort on Sharpe ratio
risk_ret <- risk_ret[order(risk_ret$Sharpe, decreasing=TRUE), ]

# Copy from rutils to save time
risk_ret <- rutils::etfenv$riskstats
# Add Sharpe ratio column
risk_ret$Sharpe <- risk_ret$"Arithmetic Mean"/risk_ret$Stdev
# Sort on Sharpe ratio
risk_ret <- risk_ret[order(risk_ret$Sharpe, decreasing=TRUE), ]
# Print data frame
knitr::kable(risk_ret[, c("Sharpe", "Skewness", "Kurtosis")])

# Print data frame
knitr::kable(risk_ret[c("VXX", "SVXY"), c("Sharpe", "Skewness", "Kurtosis")])

# dygraph plot of VXX versus SVXY
prices <- na.omit(rutils::etfenv$prices[, c("VXX", "SVXY")])
prices <- prices["2017/"]
colnamev <- c("VXX", "SVXY")
colnames(prices) <- colnamev
dygraphs::dygraph(prices, main="Prices of VXX and SVXY") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", strokeWidth=2, col="green") %>%
  dyLegend(show="always", width=500)

# Remove VIX volatility ETF data
risk_ret <- risk_ret[-match(c("VXX", "SVXY"), risk_ret$Name), ]
# Plot scatterplot of Sharpe vs Skewness
plot(Sharpe ~ Skewness, data=risk_ret,
     ylim=1.1*range(risk_ret$Sharpe),
     main="Sharpe vs Skewness")
# Add labels
text(x=risk_ret$Skewness, y=risk_ret$Sharpe,
    labels=risk_ret$Name, pos=3, cex=0.8)
# Plot scatterplot of Kurtosis vs Skewness
x11(width=6, height=5)
par(mar=c(4, 4, 2, 1), oma=c(0, 0, 0, 0))
plot(Kurtosis ~ Skewness, data=risk_ret,
     ylim=c(1, max(risk_ret$Kurtosis)),
     main="Kurtosis vs Skewness")
# Add labels
text(x=risk_ret$Skewness, y=risk_ret$Kurtosis,
    labels=risk_ret$Name, pos=1, cex=0.8)

library(PerformanceAnalytics)
retsp <- rutils::etfenv$returns[, c("VTI", "IEF")]
retsp <- na.omit(retsp)
# Calculate the Sharpe ratio
confl <- 0.05
PerformanceAnalytics::SharpeRatio(retsp, p=(1-confl),
  method="historical")
# Calculate the Sortino ratio
PerformanceAnalytics::SortinoRatio(retsp)
# Calculate the Calmar ratio
PerformanceAnalytics::CalmarRatio(retsp)
# Calculate the Dowd ratio
PerformanceAnalytics::SharpeRatio(retsp, FUN="VaR",
  p=(1-confl), method="historical")
# Calculate the Dowd ratio from scratch
varisk <- sapply(retsp, quantile, probs=confl)
-sapply(retsp, mean)/varisk
# Calculate the Conditional Dowd ratio
PerformanceAnalytics::SharpeRatio(retsp, FUN="ES",
  p=(1-confl), method="historical")
# Calculate the Conditional Dowd ratio from scratch
cvar <- sapply(retsp, function(x) {
  mean(x[x < quantile(x, confl)])
})
-sapply(retsp, mean)/cvar

# Calculate VTI percentage returns
retsp <- na.omit(rutils::etfenv$returns$VTI)
retsp <- drop(zoo::coredata(retsp))
nrows <- NROW(retsp)
# Calculate compounded VTI returns
holdp <- 252
retc <- sqrt(holdp)*sapply(1:nrows, function(x) {
    mean(retsp[sample.int(nrows, size=holdp, replace=TRUE)])
})  # end sapply
# Calculate mean, standard deviation, skewness, and kurtosis
datav <- cbind(retsp, retc)
colnames(datav) <- c("VTI", "Agg")
apply(datav, MARGIN=2, function(x) {
  # Standardize the returns
  meanval <- mean(x); stddev <- sd(x); x <- (x - meanval)/stddev
  c(mean=meanval, stddev=stddev, skew=mean(x^3), kurt=mean(x^4))
})  # end sapply
# Calculate the Sharpe and Dowd ratios
confl <- 0.05
sapply(colnames(datav), function(name) {
  x <- datav[, name]; stddev <- sd(x)
  varisk <- unname(quantile(x, probs=confl))
  cvar <- mean(x[x < varisk])
  ratio <- 1
  if (name == colnames(datav)[2]) {ratio <- holdp}
  sqrt(252/ratio)*mean(x)/c(Sharpe=stddev, Dowd=-varisk, DowdC=-cvar)
})  # end sapply

# Plot the densities of returns
x11(width=6, height=5)
par(mar=c(4, 4, 3, 1), oma=c(0, 0, 0, 0))
plot(density(retsp), t="l", lwd=3, col="blue",
     xlab="returns", ylab="density", xlim=c(-0.04, 0.04),
     main="Distribution of Compounded Stock Returns")
lines(density(retc), t="l", col="red", lwd=3)
curve(expr=dnorm(x, mean=mean(retc), sd=sd(retc)), col="green", lwd=3, add=TRUE)
legend("topright", legend=c("VTI Daily", "Compounded", "Normal"),
 inset=-0.1, bg="white", lty=1, lwd=6, col=c("blue", "red", "green"), bty="n")

library(rutils)
# Extract ETF prices from rutils::etfenv$prices
pricev <- rutils::etfenv$prices
pricev <- zoo::na.locf(pricev, na.rm=FALSE)
pricev <- zoo::na.locf(pricev, fromLast=TRUE)
dates <- zoo::index(pricev)
# Calculate simple dollar returns
retsd <- rutils::diffit(pricev)
# Or
# retsd <- lapply(pricev, rutils::diffit)
# retsd <- rutils::do_call(cbind, retsd)
# Calculate percentage returns
retsp <- retsd/rutils::lagit(pricev, lagg=1, pad_zeros=FALSE)
# Calculate log returns
retsl <- rutils::diffit(log(pricev))

# Set the initial dollar returns
retsd[1, ] <- pricev[1, ]
# Calculate prices from dollar returns
pricesn <- cumsum(retsd)
all.equal(pricesn, pricev)
# Compound the percentage returns
pricesn <- cumprod(1+retsp)
# Set the initial prices
pricesi <- as.numeric(pricev[1, ])
pricesn <- lapply(1:NCOL(pricesn), function (i)
    pricesi[i]*pricesn[, i])
pricesn <- rutils::do_call(cbind, pricesn)
# Or
# pricesn <- t(t(pricesn)*pricesi)
all.equal(pricesn, pricev, check.attributes=FALSE)
# Plot log VTI prices
endp <- rutils::calc_endpoints(rutils::etfenv$VTI, interval="months")
dygraphs::dygraph(log(quantmod::Cl(rutils::etfenv$VTI)[endp]),
  main="Logarithm of VTI Prices") %>%
  dyOptions(colors="blue", strokeWidth=2) %>%
  dyLegend(show="always", width=500)

# Calculate percentage VTI returns
pricev <- rutils::etfenv$prices$VTI
pricev <- na.omit(pricev)
retsp <- rutils::diffit(pricev)/
  rutils::lagit(pricev, lagg=1, pad_zeros=FALSE)

# Funding rate per day
frate <- 0.01/252
# Margin account
margin <- cumsum(retsp)
# Cumulative funding costs
fcosts <- cumsum(frate*margin)
# Add funding costs to margin account
margin <- (margin + fcosts)
# dygraph plot of margin and funding costs
datav <- cbind(margin, fcosts)
colnamev <- c("Margin", "Cumulative Funding")
colnames(datav) <- colnamev
endp <- rutils::calc_endpoints(datav, interval="months")
dygraphs::dygraph(datav[endp], main="VTI Margin Funding Costs") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", col="red", strokeWidth=3) %>%
  dyLegend(show="always", width=500)

# bid_offer equal to 10 bps for liquid ETFs
bid_offer <- 0.001
# Cumulative transaction costs
costs <- bid_offer*cumsum(abs(retsp))/2
# Subtract transaction costs from margin account
margin <- cumsum(retsp)
margin <- (margin - costs)
# dygraph plot of margin and transaction costs
datav <- cbind(margin, costs)
colnamev <- c("Margin", "Cumulative Transaction Costs")
colnames(datav) <- colnamev
dygraphs::dygraph(datav[endp], main="VTI Transaction Costs") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", col="red", strokeWidth=3) %>%
  dyLegend(show="always", width=500)

# Calculate VTI and IEF dollar returns
pricev <- rutils::etfenv$prices[, c("VTI", "IEF")]
pricev <- na.omit(pricev)
retsd <- rutils::diffit(pricev)
# Calculate VTI and IEF percentage returns
retsp <- retsd/rutils::lagit(pricev, lagg=1, pad_zeros=FALSE)
# Set the initial dollar returns
retsd[1, ] <- pricev[1, ]
# Wealth of fixed shares equal to $0.5 each (without rebalancing)
weightv <- c(0.5, 0.5)  # dollar weights
# Scale the dollar returns using the dollar weights
pricesi <- as.numeric(pricev[1, ])
wealth_fsa <- cumsum(retsd %*% (weightv/pricesi))
# Or using percentage returns
wealth_fsa2 <- cumprod(1+retsp) %*% weightv
all.equal(wealth_fsa, drop(wealth_fsa2))

# Wealth of fixed dollars (with rebalancing)
wealth_fda <- cumsum(retsp %*% weightv)
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(log(wealth_fsa), wealth_fda)
wealthv <- xts::xts(wealthv, zoo::index(pricev))
colnames(wealthv) <- c("Fixed shares", "Fixed dollars")
sqrt(252)*sapply(rutils::diffit(wealthv),
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot the log wealth
colnamev <- colnames(wealthv)
endp <- rutils::calc_endpoints(retsp, interval="months")
dygraphs::dygraph(wealthv[endp], main="Wealth of Weighted Portfolios") %>%
  dySeries(name=colnamev[1], col="blue", strokeWidth=2) %>%
  dySeries(name=colnamev[2], col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)

# Margin account for fixed dollars (with rebalancing)
margin <- cumsum(retsp %*% weightv)
# Cumulative transaction costs
costs <- bid_offer*cumsum(abs(retsp) %*% weightv)/2
# Subtract transaction costs from margin account
margin <- (margin - costs)
# dygraph plot of margin and transaction costs
datav <- cbind(margin, costs)
datav <- xts::xts(datav, zoo::index(pricev))
colnamev <- c("Margin", "Cumulative Transaction Costs")
colnames(datav) <- colnamev
dygraphs::dygraph(datav[endp], main="Fixed Dollar Portfolio Transaction Costs") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", col="red", strokeWidth=3) %>%
  dyLegend(show="always", width=500)

# Wealth of fixed shares (without rebalancing)
wealth_fsa <- cumsum(retsd %*% (weightv/pricesi))
# Or compound the percentage returns
wealth_fsa <- cumprod(1+retsp) %*% weightv
# Wealth of proportional allocations (with rebalancing)
wealth_pda <- cumprod(1 + retsp %*% weightv)
wealthv <- cbind(wealth_fsa, wealth_pda)
wealthv <- xts::xts(wealthv, zoo::index(pricev))
colnames(wealthv) <- c("Fixed shares", "Prop dollars")
wealthv <- log(wealthv)
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(rutils::diffit(wealthv),
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot the log wealth
dygraphs::dygraph(wealthv[endp],
  main="Wealth of Proportional Dollar Allocations") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)

# Returns in excess of weighted returns
retsw <- retsp %*% weightv
retsx <- lapply(retsp, function(x) (retsw - x))
retsx <- do.call(cbind, retsx)
sum(retsx %*% weightv)
# Calculate weighted sum of absolute excess returns
retsx <- abs(retsx) %*% weightv
# Total dollar amount of stocks that need to be traded
retsx <- retsx*rutils::lagit(wealth_pda)
# Cumulative transaction costs
costs <- bid_offer*cumsum(retsx)/2
# Subtract transaction costs from wealth
wealth_pda <- (wealth_pda - costs)

# dygraph plot of wealth and transaction costs
wealthv <- cbind(wealth_pda, costs)
wealthv <- xts::xts(wealthv, zoo::index(pricev))
colnamev <- c("Wealth", "Cumulative Transaction Costs")
colnames(wealthv) <- colnamev
dygraphs::dygraph(wealthv[endp],
  main="Transaction Costs With Proportional Allocations") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", col="red", strokeWidth=3) %>%
  dyLegend(show="always", width=500)

# Wealth of fixed shares (without rebalancing)
wealth_fsa <- drop(apply(retsp, 2, function(x) cumprod(1+x)) %*% weightv)-1
# Wealth of proportional dollar allocations (with rebalancing)
wealth_pda <- cumprod(1 + retsp %*% weightv) - 1
# Wealth of proportional target allocation (with rebalancing)
retsp <- zoo::coredata(retsp)
threshold <- 0.05
wealthv <- matrix(nrow=NROW(retsp), ncol=2)
colnames(wealthv) <- colnames(retsp)
wealthv[1, ] <- weightv
for (it in 2:NROW(retsp)) {
  # Accrue wealth without rebalancing
  wealthv[it, ] <- wealthv[it-1, ]*(1 + retsp[it, ])
  # Rebalance if wealth allocations differ from weights
  if (sum(abs(wealthv[it, ] - sum(wealthv[it, ])*weightv))/sum(wealthv[it, ]) > threshold) {
    # cat("Rebalance at:", it, "\n")
    wealthv[it, ] <- sum(wealthv[it, ])*weightv
  } # end if
} # end for
wealthv <- rowSums(wealthv) - 1
wealthv <- cbind(wealth_pda, wealthv)
wealthv <- xts::xts(wealthv, zoo::index(pricev))
colnames(wealthv) <- c("Proportional Allocations", "Proportional Target")
dygraphs::dygraph(wealthv, main="Wealth of Proportional Target Allocations") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)

# Calculate stock and bond returns
retsp <- na.omit(rutils::etfenv$returns[, c("VTI", "IEF")])
weightv <- c(0.4, 0.6)
retsp <- cbind(retsp, retsp %*% weightv)
colnames(retsp)[3] <- "Combined"
# Calculate correlations
cor(retsp)
# Calculate Sharpe ratios
sqrt(252)*sapply(retsp, function(x) mean(x)/sd(x))
# Calculate standard deviation, skewness, and kurtosis
sapply(retsp, function(x) {
  # Calculate standard deviation
  stddev <- sd(x)
  # Standardize the returns
  x <- (x - mean(x))/stddev
  c(stddev=stddev, skew=mean(x^3), kurt=mean(x^4))
})  # end sapply

# Wealth of proportional allocations
wealthv <- cumprod(1 + retsp)
# Calculate a vector of monthly end points
endp <- rutils::calc_endpoints(retsp, interval="months")
# Plot cumulative log wealth
dygraphs::dygraph(log(wealthv[endp]),
  main="Stocks and Bonds With Proportional Allocations") %>%
  dyOptions(colors=c("blue", "green", "blue", "red")) %>%
  dySeries("Combined", color="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)

# Calculate the Sharpe ratios
sqrt(252)*sapply(retsp, function(x) mean(x)/sd(x))
# Calculate the Sharpe ratios for vector of weights
weightv <- seq(0.05, 0.95, 0.05)
sharpev <- sqrt(252)*sapply(weightv, function(weight) {
  weightv <- c(weight, 1-weight)
  retsp <- (retsp[, 1:2] %*% weightv)
  mean(retsp)/sd(retsp)
})  # end sapply
# Calculate the optimal VTI weight
weightm <- weightv[which.max(sharpev)]
# Calculate the optimal weight using optimization
calc_sharpe <- function(weight) {
  weightv <- c(weight, 1-weight)
  retsp <- (retsp[, 1:2] %*% weightv)
  -mean(retsp)/sd(retsp)
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

# Coerce the returns from xts time series to matrix
retsp <- zoo::coredata(retsp[, 1:2])
nrows <- NROW(retsp)
# Bootstrap the returns and calculate a list of random returns
nboot <- 1e4
library(parallel)  # Load package parallel
ncores <- detectCores() - 1  # Number of cores
# Perform parallel bootstrap under Windows
cluster <- makeCluster(ncores)  # Initialize compute cluster under Windows
clusterSetRNGStream(cluster, 1121)  # Reset random number generator in all cores
clusterExport(cluster, c("retsp", "nrows"))
bootd <- parLapply(cluster, 1:nboot, function(x) {
  retsp[sample.int(nrows, replace=TRUE), ]
})  # end parLapply
# Stop R processes over cluster under Windows.
stopCluster(cluster)
# Perform parallel bootstrap under Mac-OSX or Linux
set.seed(1121)
bootd <- mclapply(1:nboot, function(x) {
  retsp[sample.int(nrows, replace=TRUE), ]
}, mc.cores=ncores)  # end mclapply
is.list(bootd); NROW(bootd); dim(bootd[[1]])
# Calculate the distribution of terminal wealths of VTI and IEF
wealthv <- sapply(bootd, function(retsp) {
  apply(retsp, 2, function(x) prod(1+x))
})  # end sapply
wealthv <- t(wealthv)
class(wealthv); dim(wealthv); tail(wealthv)
# Calculate the means and standard deviations of the terminal wealths
apply(wealthv, 2, mean)
apply(wealthv, 2, sd)

# Plot the densities of the terminal wealths of VTI and IEF
wealthvti <- wealthv[, "VTI"]
wealthief <- wealthv[, "IEF"]
meanvti <- mean(wealthvti); meanief <- mean(wealthief)
densvti <- density(wealthvti); densief <- density(wealthief)
plot(densvti, col="blue", lwd=3, xlab="wealth",
     xlim=c(0, 2*max(densief$x)), ylim=c(0, max(densief$y)),
     main="Terminal Wealth Distributions of VTI and IEF")
lines(densief, col="green", lwd=3)
abline(v=meanvti, col="blue", lwd=2, lty="dashed")
text(x=meanvti, y=0.5, labels="VTI mean", pos=4, cex=0.8)
abline(v=meanief, col="green", lwd=2, lty="dashed")
text(x=meanief, y=0.5, labels="IEF mean", pos=4, cex=0.8)
legend(x="topright", legend=c("VTI", "IEF"),
 inset=0.1, cex=1.0, bg="white", bty="n",
 lwd=6, lty=1, col=c("blue", "green"))

# Calculate the distributions of portfolio wealth
holdv <- nrows*seq(0.1, 1.0, 0.1)
wealthm <- sapply(bootd, function(retsp) {
  sapply(holdv, function(holdp) {
    prod(1 + retsp[1:holdp, "VTI"])
  })  # end sapply
})  # end sapply
wealthm <- t(wealthm)
dim(wealthm)
# Define the risk-adjusted wealth measure
riskretfun <- function(wealthv) {
  riskv <- min(wealthv)
  if (min(wealthv) < 1)
    riskv <- mean((1-wealthv)[wealthv<1])
  mean(wealthv)/riskv
}  # end riskretfun
# Calculate the stock wealth risk-return ratios
riskrets <- apply(wealthm, 2, riskretfun)
# Plot the stock wealth risk-return ratios
plot(x=holdv, y=riskrets,
     main="Stock Risk-Return Ratio as Function of Holding Period",
     xlab="Holding Period", ylab="Ratio",
     t="l", lwd=3, col="blue")

# Plot the stock wealth for long and short holding periods
wealth1 <- wealthm[, 9]
wealth2 <- wealthm[, 1]
mean1 <- mean(wealth1); mean2 <- mean(wealth2)
dens1 <- density(wealth1); dens2 <- density(wealth2)
plot(dens1, col="blue", lwd=3, xlab="wealth",
     xlim=c(0, 2*max(dens2$x)), ylim=c(0, max(dens2$y)),
     main="Wealth Distributions for Long and Short Holding Periods")
lines(dens2, col="green", lwd=3)
abline(v=mean1, col="blue", lwd=2, lty="dashed")
text(x=mean1, y=0.5, labels="Long", pos=4, cex=0.8)
abline(v=mean2, col="green", lwd=2, lty="dashed")
text(x=mean2, y=0.5, labels="Short", pos=4, cex=0.8)
legend(x="top", legend=c("Long", "Short"),
 inset=0.1, cex=1.0, bg="white", bty="n",
 lwd=6, lty=1, col=c("blue", "green"))

# Calculate the distributions of portfolio wealth
weightv <- seq(0.05, 0.95, 0.05)
wealthm <- sapply(bootd, function(retsp) {
  sapply(weightv, function(weight) {
    prod(1 + retsp %*% c(weight, 1-weight))
  })  # end sapply
})  # end sapply
wealthm <- t(wealthm)
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
text(x=weightm, y=0.7*max(riskrets), pos=4, cex=1.2,
     labels=paste("optimal VTI weight =", round(weightm, 2)))

# Extract ETF returns
symbolv <- c("VTI", "IEF", "DBC")
retsp <- na.omit(rutils::etfenv$returns[, symbolv])
# Calculate all-weather portfolio wealth
weightsaw <- c(0.30, 0.55, 0.15)
retsp <- cbind(retsp, retsp %*% weightsaw)
colnames(retsp)[4] <- "All Weather"
# Calculate Sharpe ratios
sqrt(252)*sapply(retsp, function(x) mean(x)/sd(x))

# Calculate cumulative wealth from returns
wealthv <- cumprod(1+retsp)
# Calculate a vector of monthly end points
endp <- rutils::calc_endpoints(wealthv, interval="months")
# dygraph all-weather wealth
dygraphs::dygraph(wealthv[endp], main="All-Weather Portfolio") %>%
  dyOptions(colors=c("blue", "green", "orange", "red")) %>%
  dySeries("All Weather", color="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Plot all-weather wealth
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue", "green", "red")
quantmod::chart_Series(wealthv, theme=plot_theme, lwd=c(2, 2, 2, 4),
       name="All-Weather Portfolio")
legend("topleft", legend=colnames(wealthv),
  inset=0.1, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")

# Calculate VTI returns
retsp <- na.omit(rutils::etfenv$returns$VTI["2008/2009"])
dates <- zoo::index(retsp)
nrows <- NROW(retsp)
retsp <- drop(zoo::coredata(retsp))
# Bond floor
bfloor <- 60
# CPPI multiplier
coeff <- 2
# Portfolio market values
portfv <- numeric(nrows)
# Initial principal
portfv[1] <- 100
# Stock allocation
stockv <- numeric(nrows)
stockv[1] <- min(coeff*(portfv[1] - bfloor), portfv[1])
# Bond allocation
bondv <- numeric(nrows)
bondv[1] <- (portfv[1] - stockv[1])

# Simulate CPPI strategy
for (t in 2:nrows) {
  portfv[t] <- portfv[t-1] + stockv[t-1]*retsp[t]
  stockv[t] <- min(coeff*(portfv[t] - bfloor), portfv[t])
  bondv[t] <- (portfv[t] - stockv[t])
}  # end for
# dygraph plot of CPPI strategy
pricevti <- 100*cumprod(1+retsp)
datav <- xts::xts(cbind(stockv, bondv, portfv, pricevti), dates)
colnames(datav) <- c("stocks", "bonds", "CPPI", "VTI")
endp <- rutils::calc_endpoints(datav, interval="weeks")
dygraphs::dygraph(datav[endp], main="CPPI strategy") %>%
  dyOptions(colors=c("red", "green", "blue", "orange"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Calculate dollar and percentage returns for VTI and IEF.
pricev <- rutils::etfenv$prices[, c("VTI", "IEF")]
pricev <- na.omit(pricev)
retsd <- rutils::diffit(pricev)
retsp <- retsd/rutils::lagit(pricev, lagg=1, pad_zeros=FALSE)
# Calculate wealth of proportional allocations.
weightv <- c(0.5, 0.5)
retsw <- retsp %*% weightv
wealth_pda <- cumprod(1 + retsw)
# Calculate rolling percentage volatility.
look_back <- 21
volat <- HighFreq::roll_var(retsp, look_back=look_back)
iszero <- (rowSums(volat) == 0)
volat[iszero, ] <- 1
# Calculate the risk parity portfolio allocations.
alloc <- lapply(1:NCOL(pricev),
  function(x) weightv[x]/volat[, x])
alloc <- do.call(cbind, alloc)
# Scale allocations to 1 dollar total.
alloc <- alloc/rowSums(alloc)
# Lag the allocations
alloc <- rutils::lagit(alloc)
# Calculate wealth of risk parity.
retsw <- rowSums(retsp*alloc)
wealth_risk_parity <- cumprod(1 + retsw)

# Calculate the log wealths.
wealthv <- log(cbind(wealth_pda, wealth_risk_parity))
wealthv <- xts::xts(wealthv, zoo::index(pricev))
colnames(wealthv) <- c("Fixed Ratio", "Risk Parity")
# Calculate the Sharpe ratios.
sqrt(252)*sapply(rutils::diffit(wealthv), function (x) mean(x)/sd(x))
# Plot a dygraph of the log wealths.
endp <- rutils::calc_endpoints(wealthv, interval="months")
dygraphs::dygraph(wealthv[endp],
  main="Log Wealth of Risk Parity vs Proportional Allocations") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)

# Test risk parity market timing of VTI using Treynor-Mazuy test
retsrp <- rutils::diffit(wealthv)
vti <- retsp$VTI
design <- cbind(retsrp, vti, vti^2)
design <- na.omit(design)
colnames(design)[1:2] <- c("fixed", "risk_parity")
colnames(design)[4] <- "treynor"
model <- lm(risk_parity ~ VTI + treynor, data=design)
summary(model)
# Plot residual scatterplot
residuals <- (design$risk_parity - model$coeff[2]*vti)
residuals <- model$residuals
x11(width=6, height=5)
plot.default(x=vti, y=residuals, xlab="VTI", ylab="residuals")
title(main="Treynor-Mazuy Market Timing Test\n for Risk Parity vs VTI", line=0.5)
# Plot fitted (predicted) response values
fittedv <- (model$coeff["(Intercept)"] + model$coeff["treynor"]*vti^2)
points.default(x=vti, y=fittedv, pch=16, col="red")
text(x=0.05, y=0.8*max(residuals), paste("Risk Parity t-value =", round(summary(model)$coeff["treynor", "t value"], 2)))

# Test for fixed ratio market timing of VTI using Treynor-Mazuy test
model <- lm(fixed ~ VTI + treynor, data=design)
summary(model)
# Plot fitted (predicted) response values
fittedv <- (model$coeff["(Intercept)"] + model$coeff["treynor"]*vti^2)
points.default(x=vti, y=fittedv, pch=16, col="blue")
text(x=0.05, y=0.6*max(residuals), paste("Fixed Ratio t-value =", round(summary(model)$coeff["treynor", "t value"], 2)))

# Calculate positions
retsp <- na.omit(rutils::etfenv$returns$VTI)
posit <- rep(NA_integer_, NROW(retsp))
dates <- zoo::index(retsp)
dates <- format(dates, "%m-%d")
posit[dates == "05-01"] <- 0
posit[dates == "05-03"] <- 0
posit[dates == "11-01"] <- 1
posit[dates == "11-03"] <- 1
# Carry forward and backward non-NA posit
posit <- zoo::na.locf(posit, na.rm=FALSE)
posit <- zoo::na.locf(posit, fromLast=TRUE)
# Calculate strategy returns
sell_inmay <- posit*retsp
wealthv <- cbind(retsp, sell_inmay)
colnames(wealthv) <- c("VTI", "sell_in_may")
# Calculate Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))

# Plot wealth of Sell in May strategy
endp <- rutils::calc_endpoints(wealthv, interval="months")
dygraphs::dygraph(cumsum(wealthv)[endp], main="Sell in May Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# OR: Open x11 for plotting
x11(width=6, height=5)
par(mar=c(4, 4, 3, 1), oma=c(0, 0, 0, 0))
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("blue", "red")
quantmod::chart_Series(wealthv, theme=plot_theme, name="Sell in May Strategy")
legend("topleft", legend=colnames(wealthv),
  inset=0.1, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")

# Test if Sell in May strategy can time VTI
design <- cbind(wealth$sell_in_may, 0.5*(retsp+abs(retsp)), retsp^2)
colnames(design) <- c("VTI", "merton", "treynor")
# Perform Merton-Henriksson test
model <- lm(sell_inmay ~ VTI + merton, data=design)
summary(model)
# Perform Treynor-Mazuy test
model <- lm(sell_inmay ~ VTI + treynor, data=design)
summary(model)
# Plot Treynor-Mazuy residual scatterplot
residuals <- (sell_inmay - model$coeff[2]*retsp)
plot.default(x=retsp, y=residuals, xlab="VTI", ylab="residuals")
title(main="Treynor-Mazuy Market Timing Test\n for Sell in May vs VTI", line=0.5)
# Plot fitted (predicted) response values
fittedv <- (model$coeff["(Intercept)"] +
        model$coeff["treynor"]*retsp^2)
points.default(x=retsp, y=fittedv, pch=16, col="red")
text(x=0.05, y=0.8*max(residuals), paste("Treynor test t-value =", round(summary(model)$coeff["treynor", "t value"], 2)))

# Calculate the log of OHLC VTI prices
ohlc <- log(rutils::etfenv$VTI)
openp <- quantmod::Op(ohlc)
highp <- quantmod::Hi(ohlc)
lowp <- quantmod::Lo(ohlc)
closep <- quantmod::Cl(ohlc)
# Calculate the close-to-close log returns, the intraday
# open-to-close returns and the overnight close-to-open returns.
close_close <- rutils::diffit(closep)
colnames(close_close) <- "close_close"
open_close <- (closep - openp)
colnames(open_close) <- "open_close"
close_open <- (openp - rutils::lagit(closep, lagg=1, pad_zeros=FALSE))
colnames(close_open) <- "close_open"

# Calculate Sharpe and Sortino ratios
wealthv <- cbind(close_close, close_open, open_close)
sqrt(252)*sapply(wealthv,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot log wealth
endp <- rutils::calc_endpoints(wealthv, interval="months")
dygraphs::dygraph(cumsum(wealthv)[endp],
  main="Wealth of Close-to-Close, Close-to-Open, and Open-to-Close Strategies") %>%
  dySeries(name="close_close", label="Close-to-Close (static)", strokeWidth=2, col="blue") %>%
  dySeries(name="close_open", label="Close-to-Open (overnight)", strokeWidth=2, col="red") %>%
  dySeries(name="open_close", label="Open-to-Close (daytime)", strokeWidth=2, col="green") %>%
  dyLegend(width=600)

# Calculate the VTI returns
retsp <- na.omit(rutils::etfenv$returns$VTI)
dates <- zoo::index(retsp)
# Calculate first business day of every month
dayv <- as.numeric(format(dates, "%d"))
indeks <- which(rutils::diffit(dayv) < 0)
dates[head(indeks)]
# Calculate Turn of the Month dates
indeks <- lapply((-1):2, function(x) indeks + x)
indeks <- do.call(c, indeks)
sum(indeks > NROW(dates))
indeks <- sort(indeks)
dates[head(indeks, 11)]
# Calculate Turn of the Month pnls
pnls <- numeric(NROW(retsp))
pnls[indeks] <- retsp[indeks, ]

# Combine data
wealthv <- cbind(retsp, pnls)
colnamev <- c("VTI", "Strategy")
colnames(wealthv) <- colnamev
# Calculate Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# dygraph plot VTI Turn of the Month strategy
endp <- rutils::calc_endpoints(wealthv, interval="months")
dygraphs::dygraph(cumsum(wealthv)[endp],
  main="Turn of the Month Strategy") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", strokeWidth=2, col="red")

# Calculate the VTI returns
retsp <- na.omit(rutils::etfenv$returns$VTI)
dates <- zoo::index(retsp)
retsp <- drop(coredata(retsp))
nrows <- NROW(retsp)
# Simulate stop-loss strategy
stopl <- 0.05
maxp <- 0.0
retc <- 0.0
pnls <- retsp
for (i in 1:(nrows-1)) {
# Calculate drawdown
  retc <- retc + retsp[i]
  maxp <- max(maxp, retc)
  dd <- (retc - maxp)
# Check for stop-loss
  if (dd < -stopl*maxp)
    pnls[i+1] <- 0
}  # end for
# Same but without using explicit loops
cumsumv <- cumsum(retsp)
cummaxv <- cummax(cumsumv)
dd <- (cumsumv - cummaxv)
pnls2 <- retsp
isdd <- rutils::lagit(dd < -stopl*cummaxv)
pnls2 <- ifelse(isdd, 0, pnls2)
all.equal(pnls, pnls2)

# Combine data
wealthv <- xts::xts(cbind(retsp, pnls), dates)
colnamev <- c("VTI", "Strategy")
colnames(wealthv) <- colnamev
# Calculate Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# dygraph plot VTI stop-loss strategy
endp <- rutils::calc_endpoints(wealthv, interval="months")
dygraphs::dygraph(cumsum(wealthv)[endp],
  main="VTI Stop-loss Strategy") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", strokeWidth=2, col="red")

# Simulate multiple stop-loss strategies
cumsumv <- cumsum(retsp)
cummaxv <- cummax(cumsumv)
dd <- (cumsumv - cummaxv)
cum_pnls <- sapply(0.01*(1:20), function(stopl) {
  pnls <- retsp
  isdd <- rutils::lagit(dd < -stopl*cummaxv)
  pnls <- ifelse(isdd, 0, pnls)
  sum(pnls)
})  # end sapply

# Plot cumulative pnls for stop-loss strategies
plot(x=0.01*(1:20), y=cum_pnls,
     main="Cumulative PnLs for Stop-loss Strategies",
     xlab="stop-loss level", ylab="cumulative pnl",
     t="l", lwd=3, col="blue")
