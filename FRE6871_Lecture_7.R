# Load package HighFreq
library(HighFreq)
# Or load the high frequency data file directly:
# symbolv <- load("/Users/jerzy/Develop/R/HighFreq/data/hf_data.RData")
head(HighFreq::SPY_TAQ)
head(HighFreq::SPY)
tail(HighFreq::SPY)
library(HighFreq)
# Read TAQ trade data from csv file
taq <- data.table::fread(file="/Users/jerzy/Develop/lecture_slides/data/xlk_tick_trades_20200316.csv")
# Inspect the TAQ data in data.table format
taq
class(taq)
colnames(taq)
sapply(taq, class)
symboln <- taq$SYM_ROOT[1]
# Create date-time index
datev <- paste(taq$DATE, taq$TIME_M)
# Coerce date-time index to POSIXlt
datev <- strptime(datev, "%Y%m%d %H:%M:%OS")
class(datev)
# Display more significant digits
# options("digits")
options(digits=20, digits.secs=10)
last(datev)
unclass(last(datev))
as.numeric(last(datev))
# Coerce date-time index to POSIXct
datev <- as.POSIXct(datev)
class(datev)
last(datev)
unclass(last(datev))
as.numeric(last(datev))
# Calculate the number of seconds
as.numeric(last(datev)) - as.numeric(first(datev))
# Calculate the number of ticks per second
NROW(taq)/(6.5*3600)
# Select TAQ data columns
taq <- taq[, .(price=PRICE, volume=SIZE)]
# Coerce trade ticks to xts series
xlk <- xts::xts(taq[, .(price, volume)], datev)
colnames(xlk) <- c("price", "volume")
save(xlk, file="/Users/jerzy/Develop/data/xlk_tick_trades_20200316.RData")
# Plot histogram of the trading volumes
hist(xlk$volume, main="Histogram of XLK Trading Volumes",
     breaks=1e5, xlim=c(1, 400), xlab="number of shares")
# Plot dygraph
dygraphs::dygraph(xlk$price, main="XLK Intraday Prices for 2020-03-16") %>%
  dyOptions(colors="blue", strokeWidth=1)
# Plot in x11 window
x11(width=6, height=5)
quantmod::chart_Series(x=xlk$price, name="XLK Intraday Prices for 2020-03-16")
pricev <- read.zoo(file="/Users/jerzy/Develop/lecture_slides/data/bid_ask_bounce.csv",
  header=TRUE, sep=",")
pricev <- as.xts(pricev)
dygraphs::dygraph(pricev$Close,
  main="S&P500 Futures Prices Bid-Ask Bounce") %>%
  dyOptions(colors="blue", strokeWidth=2)
# Plot dygraph of trade prices of at least 100 shares
dygraphs::dygraph(xlk$price[xlk$volume >= 100, ],
  main="XLK Prices for Trades of At Least 100 Shares") %>%
  dyOptions(colors="blue", strokeWidth=1)
# Select the large trade lots of at least 100 shares
dim(taq)
tickb <- taq[taq$volume >= 100]
dim(tickb)
# Number of large lot ticks per second
NROW(tickb)/(6.5*3600)
# Plot histogram of the trading volumes
hist(tickb$volume, main="Histogram of XLK Trading Volumes",
     breaks=100000, xlim=c(1, 400), xlab="number of shares")
# Save trade ticks with large lots
data.table::fwrite(tickb, file="/Users/jerzy/Develop/data/xlk_tick_trades_20200316_biglots.csv")
# Coerce trade prices to xts
xlkb <- xts::xts(tickb[, .(price, volume)], tickb$index)
colnames(xlkb) <- c("price", "volume")
# Plot dygraph of the large lots
dygraphs::dygraph(xlkb$price,
  main="XLK Prices for Trades of At Least 100 Shares") %>%
  dyOptions(colors="blue", strokeWidth=1)
# Plot the large lots
x11(width=6, height=5)
quantmod::chart_Series(x=xlk$price,
  name="XLK Trade Ticks for 2020-03-16 (large lots only)")
# Calculate the centered Hampel filter to remove bad prices
lookb <- 71 # Look-back interval
halfb <- lookb %/% 2 # Half-back interval
pricev <- xlk$price
# Calculate the trailing median and MAD
medianv <- HighFreq::roll_mean(pricev, lookb=lookb, method="nonparametric")
colnames(medianv) <- c("median")
madv <- HighFreq::roll_var(pricev, lookb=lookb, method="nonparametric")
# madv <- TTR::runMAD(pricev, n=lookb)
# Center the median and the MAD
medianv <- rutils::lagit(medianv, lagg=(-halfb), pad_zeros=FALSE)
madv <- rutils::lagit(madv, lagg=(-halfb), pad_zeros=FALSE)
# Calculate the Z-scores
zscores <- ifelse(madv > 0, (pricev - medianv)/madv, 0)
# Z-scores have very fat tails
range(zscores); mad(zscores)
madz <- mad(zscores[abs(zscores) > 0])
hist(zscores, breaks=50000, xlim=c(-2*madz, 2*madz))
# Define discrimination threshold value
threshv <- 6*madz
# Identify good prices with small z-scores
isgood <- (abs(zscores) < threshv)
# Calculate the number of bad prices
sum(!isgood)
# Scrub bad prices by replacing them with previous good prices
priceg <- pricev
priceg[!isgood] <- NA
priceg <- zoo::na.locf(priceg)
# Plot dygraph of the scrubbed prices
dygraphs::dygraph(priceg, main="Scrubbed XLK Intraday Prices") %>%
  dyOptions(colors="blue", strokeWidth=1)
# Plot using chart_Series()
x11(width=6, height=5)
quantmod::chart_Series(x=priceg,
  name="Clean XLK Intraday Prices for 2020-03-16")
# Add 200 random price spikes to the clean prices
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
nspikes <- 200
nrows <- NROW(priceg)
ispike <- logical(nrows)
ispike[sample(x=nrows, size=nspikes)] <- TRUE
priceb <- priceg
priceb[ispike] <- priceb[ispike]*
  sample(c(0.999, 1.001), size=nspikes, replace=TRUE)
# Plot the bad prices and their medians
medianv <- HighFreq::roll_mean(priceb, lookb=lookb, method="nonparametric")
pricem <- cbind(priceb, medianv)
colnames(pricem) <- c("prices with spikes", "median")
dygraphs::dygraph(pricem, main="XLK Prices With Spikes") %>%
  dyOptions(colors=c("red", "blue"))
# Calculate the z-scores
madv <- HighFreq::roll_var(priceb, lookb=lookb, method="nonparametric")
zscores <- ifelse(madv > 0, (priceb - medianv)/madv, 0)
# Z-scores have very fat tails
range(zscores); mad(zscores)
madz <- mad(zscores[abs(zscores) > 0])
hist(zscores, breaks=10000, xlim=c(-4*madz, 4*madz))
# Identify good prices with small z-scores
threshv <- 3*madz
isgood <- (abs(zscores) < threshv)
# Calculate the number of bad prices
sum(!isgood)
# Calculate the confusion matrix
table(actual=!ispike, forecast=isgood)
sum(!isgood)
# FALSE positive (type I error)
sum(!ispike & !isgood)
# FALSE negative (type II error)
sum(ispike & isgood)
# Confusion matrix as function of threshold
confun <- function(actualv, zscores, threshv) {
    confmat <- table(actualv, (abs(zscores) < threshv))
    confmat <- confmat / rowSums(confmat)
    c(typeI=confmat[2, 1], typeII=confmat[1, 2])
}  # end confun
confun(!ispike, zscores, threshv=threshv)
# Define vector of discrimination thresholds
threshv <- madz*seq(from=0.1, to=3.0, by=0.05)/2
# Calculate the error rates
errorr <- sapply(threshv, confun, actualv=!ispike, zscores=zscores)
errorr <- t(errorr)
rownames(errorr) <- threshv
errorr <- rbind(c(1, 0), errorr)
errorr <- rbind(errorr, c(0, 1))
# Calculate the area under the ROC curve (AUC)
truepos <- (1 - errorr[, "typeII"])
truepos <- (truepos + rutils::lagit(truepos))/2
falsepos <- rutils::diffit(errorr[, "typeI"])
abs(sum(truepos*falsepos))
# Plot ROC curve for Hampel classifier
plot(x=errorr[, "typeI"], y=1-errorr[, "typeII"],
     xlab="FALSE positive rate", ylab="TRUE positive rate",
     xlim=c(0, 1), ylim=c(0, 1),
     main="ROC Curve for Hampel Classifier",
     type="l", lwd=3, col="blue")
abline(a=0.0, b=1.0, lwd=3, col="orange")
# Load log VXX prices
load("/Users/jerzy/Develop/lecture_slides/data/pricevxx.RData")
nrows <- NROW(pricev)
# Calculate the centered Hampel filter for VXX
lookb <- 7 # Look-back interval
halfb <- lookb %/% 2 # Half-back interval
medianv <- HighFreq::roll_mean(pricev, lookb=lookb, method="nonparametric")
medianv <- rutils::lagit(medianv, lagg=(-halfb), pad_zeros=FALSE)
madv <- HighFreq::roll_var(pricev, lookb=lookb, method="nonparametric")
madv <- rutils::lagit(madv, lagg=(-halfb), pad_zeros=FALSE)
zscores <- ifelse(madv > 0, (pricev - medianv)/madv, 0)
range(zscores); mad(zscores)
madz <- mad(zscores[abs(zscores) > 0])
hist(zscores, breaks=100, xlim=c(-3*madz, 3*madz))
# Define discrimination threshold value
threshv <- 9*madz
# Calculate the good prices
isgood <- (abs(zscores) < threshv)
sum(!isgood)
# Dates of the bad prices
zoo::index(pricev[!isgood])
# Calculate the false positives
falsep <- !isgood
falsep[which(zoo::index(pricev) == as.Date("2010-11-08"))] <- FALSE
# Plot dygraph of the prices with bad prices
datam <- cbind(pricev, zscores)
colnames(datam)[2] <- "ZScores"
colnamev <- colnames(datam)
dygraphs::dygraph(datam, main="VXX Prices With Z-Scores and False Positives") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=1, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=1, col="red") %>%
  dyEvent(zoo::index(pricev[falsep]), label=rep("false", sum(falsep)), strokePattern="solid", color="red") %>%
  dyEvent(zoo::index(pricev["2010-11-08"]), label="true", strokePattern="solid", color="green")
# Replace bad stock prices with the previous good prices
priceg <- pricev
priceg[!isgood] <- NA
priceg <- zoo::na.locf(priceg)
# Calculate the Z-scores
medianv <- HighFreq::roll_mean(priceg, lookb=lookb, method="nonparametric")
medianv <- rutils::lagit(medianv, lagg=(-halfb), pad_zeros=FALSE)
madv <- HighFreq::roll_var(priceg, lookb=lookb, method="nonparametric")
madv <- rutils::lagit(madv, lagg=(-halfb), pad_zeros=FALSE)
zscores <- ifelse(madv > 0, (priceg - medianv)/madv, 0)
madz <- mad(zscores[abs(zscores) > 0])
# Calculate the number of bad prices
threshv <- 9*madz
isgood <- (abs(zscores) < threshv)
sum(!isgood)
zoo::index(priceg[!isgood])
# Calculate the false positives
falsep <- !isgood
falsep[which(zoo::index(pricev) == as.Date("2010-11-08"))] <- FALSE
# Plot dygraph of the prices with bad prices
dygraphs::dygraph(priceg, main="Scrubbed VXX Prices With False Positives") %>%
  dyEvent(zoo::index(priceg[falsep]), label=rep("false", sum(falsep)), strokePattern="solid", color="red") %>%
  dyOptions(colors="blue", strokeWidth=1)
# Add 200 random price spikes to the clean prices
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
nspikes <- 200
ispike <- logical(nrows)
ispike[sample(x=nrows, size=nspikes)] <- TRUE
priceb <- priceg
priceb[ispike] <- priceb[ispike]*
  sample(c(0.99, 1.01), size=nspikes, replace=TRUE)
# Calculate the Z-scores
medianv <- HighFreq::roll_mean(priceb, lookb=lookb, method="nonparametric")
medianv <- rutils::lagit(medianv, lagg=(-halfb), pad_zeros=FALSE)
madv <- HighFreq::roll_var(priceb, lookb=lookb, method="nonparametric")
madv <- rutils::lagit(madv, lagg=(-halfb), pad_zeros=FALSE)
zscores <- ifelse(madv > 0, (priceb - medianv)/madv, 0)
madz <- mad(zscores[abs(zscores) > 0])
# Define vector of discrimination thresholds
threshv <- madz*seq(from=0.1, to=3.0, by=0.05)/2
# Calculate the error rates
errorr <- sapply(threshv, confun, actualv=!ispike, zscores=zscores)
errorr <- t(errorr)
rownames(errorr) <- threshv
errorr <- rbind(c(1, 0), errorr)
errorr <- rbind(errorr, c(0, 1))
# Calculate the area under the ROC curve (AUC)
truepos <- (1 - errorr[, "typeII"])
truepos <- (truepos + rutils::lagit(truepos))/2
falsepos <- rutils::diffit(errorr[, "typeI"])
abs(sum(truepos*falsepos))
# Plot ROC curve for Hampel classifier
plot(x=errorr[, "typeI"], y=1-errorr[, "typeII"],
     xlab="FALSE positive rate", ylab="TRUE positive rate",
     xlim=c(0, 1), ylim=c(0, 1),
     main="ROC Curve for Daily Hampel Classifier",
     type="l", lwd=3, col="blue")
abline(a=0.0, b=1.0, lwd=3, col="orange")
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
sheets <- lapply(namev, read_xlsx, path=filev)
names(sheets) <- namev
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
sapply(sheets, function(xtsv) sum(is.na(xtsv)))
sheets <- lapply(sheets, zoo::na.locf, na.rm=FALSE)
sheets <- lapply(sheets, zoo::na.locf, fromLast=TRUE)
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
# Verify that Rtools or XCode are working properly:
devtools::find_rtools()  # Under Windows
devtools::has_devel()
# Install the packages Rcpp and RcppArmadillo
install.packages(c("Rcpp", "RcppArmadillo"))
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
int xsize = x.size();
int ysize = y.size();
if (xsize != ysize) {
    return 0;
  } else {
    double total = 0;
    for(int i = 0; i < xsize; ++i) {
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
double inner_sugar(NumericVector x, NumericVector y) {
  return sum(x * y);
}")  # end cppFunction
# Run Rcpp Sugar function
inner_sugar(1:3, 6:4)
inner_sugar(1:3, 6:3)
# Define R function with loop
inner_multr <- function(x, y) {
    sumv <- 0
    for(i in 1:NROW(x)) {
sumv <- sumv + x[i] * y[i]
    }
    sumv
}  # end inner_multr
# Run R function
inner_multr(1:3, 6:4)
inner_multr(1:3, 6:3)
# Compare speed of Rcpp and R
library(microbenchmark)
summary(microbenchmark(
  rcode=inner_multr(1:10000, 1:10000),
  innerp=1:10000 %*% 1:10000,
  Rcpp=inner_mult(1:10000, 1:10000),
  sugar=inner_sugar(1:10000, 1:10000),
  times=10))[, c(1, 4, 5)]
# Define Ornstein-Uhlenbeck function in R
sim_our <- function(nrows=1000, eq_price=5.0,
              volat=0.01, theta=0.01) {
  retp <- numeric(nrows)
  pricev <- numeric(nrows)
  pricev[1] <- eq_price
  for (i in 2:nrows) {
    retp[i] <- theta*(eq_price - pricev[i-1]) + volat*rnorm(1)
    pricev[i] <- pricev[i-1] + retp[i]
  }  # end for
  pricev
}  # end sim_our
# Simulate Ornstein-Uhlenbeck process in R
eq_price <- 5.0; sigmav <- 0.01
thetav <- 0.01; nrows <- 1000
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")  # Reset random numbers
ousim <- sim_our(nrows, eq_price=eq_price, volat=sigmav, theta=thetav)
# Define Ornstein-Uhlenbeck function in Rcpp
Rcpp::cppFunction("
NumericVector sim_oucpp(double eq_price,
                  double volat,
                  double thetav,
                  NumericVector innov) {
  int nrows = innov.size();
  NumericVector pricev(nrows);
  NumericVector retv(nrows);
  pricev[0] = eq_price;
  for (int it = 1; it < nrows; it++) {
    retv[it] = thetav*(eq_price - pricev[it-1]) + volat*innov[it-1];
    pricev[it] = pricev[it-1] + retv[it];
  }  // end for
  return pricev;
}")  # end cppFunction
# Simulate Ornstein-Uhlenbeck process in Rcpp
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")  # Reset random numbers
oucpp <- sim_oucpp(eq_price=eq_price,
  volat=sigmav, theta=thetav, innov=rnorm(nrows))
all.equal(ousim, oucpp)
# Compare speed of Rcpp and R
library(microbenchmark)
summary(microbenchmark(
  rcode=sim_our(nrows, eq_price=eq_price, volat=sigmav, theta=thetav),
  Rcpp=sim_oucpp(eq_price=eq_price, volat=sigmav, theta=thetav, innov=rnorm(nrows)),
  times=10))[, c(1, 4, 5)]
# Source Rcpp function for Ornstein-Uhlenbeck process from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/sim_ou.cpp")
# Simulate Ornstein-Uhlenbeck process in Rcpp
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")  # Reset random numbers
oucpp <- sim_oucpp(eq_price=eq_price,
  volat=sigmav,
  theta=thetav,
  innov=rnorm(nrows))
all.equal(ousim, oucpp)
# Compare speed of Rcpp and R
library(microbenchmark)
summary(microbenchmark(
  rcode=sim_our(nrows, eq_price=eq_price, volat=sigmav, theta=thetav),
  Rcpp=sim_oucpp(eq_price=eq_price, volat=sigmav, theta=thetav, innov=rnorm(nrows)),
  times=10))[, c(1, 4, 5)]
# Calculate uniformly distributed pseudo-random sequence
unifun <- function(seedv, nrows=10) {
  datav <- numeric(nrows)
  datav[1] <- seedv
  for (i in 2:nrows) {
    datav[i] <- 4*datav[i-1]*(1-datav[i-1])
  }  # end for
  acos(1-2*datav)/pi
}  # end unifun
# Source Rcpp functions from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/unifun.cpp")
# Microbenchmark Rcpp code
library(microbenchmark)
summary(microbenchmark(
  rcode=runif(1e5),
  rloop=unifun(0.3, 1e5),
  Rcpp=unifuncpp(0.3, 1e5),
  times=10))[, c(1, 4, 5)]
library(RcppArmadillo)
# Source Rcpp functions from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/armadillo_functions.cpp")
vec1 <- runif(1e5)
vec2 <- runif(1e5)
inner_vec(vec1, vec2)
vec1 %*% vec2
# Microbenchmark \emph{RcppArmadillo} code
summary(microbenchmark(
  rcpp = inner_vec(vec1, vec2),
  rcode = (vec1 %*% vec2),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
# Microbenchmark shows:
# inner_vec() is several times faster than %*%, especially for longer vectors.
#     expr     mean   median
# 1 inner_vec 110.7067 110.4530
# 2 rcode 585.5127 591.3575
# Source Rcpp functions from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/sim_arima.cpp")
# Define AR(2) coefficients
coeff <- c(0.9, 0.09)
nrows <- 1e4
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
innov <- rnorm(nrows)
# Simulate ARIMA using filter()
arimar <- filter(x=innov, filter=coeff, method="recursive")
# Simulate ARIMA using sim_ar()
innov <- matrix(innov)
coeff <- matrix(coeff)
arimav <- sim_ar(coeff, innov)
all.equal(drop(arimav), as.numeric(arimar))
# Microbenchmark \emph{RcppArmadillo} code
summary(microbenchmark(
  rcpp = sim_ar(coeff, innov),
  filter = filter(x=innov, filter=coeff, method="recursive"),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
library(RcppArmadillo)
# Source Rcpp functions from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/armadillo_functions.cpp")
matv <- matrix(runif(1e5), nc=1e3)
# Center matrix columns using apply()
matd <- apply(matv, 2, function(x) (x-mean(x)))
# Center matrix columns in place using Rcpp demeanr()
demeanr(matv)
all.equal(matd, matv)
# Microbenchmark \emph{RcppArmadillo} code
library(microbenchmark)
summary(microbenchmark(
  rcode = (apply(matv, 2, mean)),
  rcpp = demeanr(matv),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
# Perform matrix inversion
# Create random positive semi-definite matrix
matv <- matrix(runif(25), nc=5)
matv <- t(matv) %*% matv
# Invert the matrix
matrixinv <- solve(matv)
inv_mat(matv)
all.equal(matrixinv, matv)
# Microbenchmark \emph{RcppArmadillo} code
summary(microbenchmark(
  rcode = solve(matv),
  rcpp = inv_mat(matv),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
library(RcppArmadillo)
# Source Rcpp functions from file
Rcpp::sourceCpp("/Users/jerzy/Develop/lecture_slides/scripts/HighFreq.cpp")
# Calculate matrix of random returns
matv <- matrix(rnorm(300), nc=5)
# Regularized inverse of correlation matrix
dimax <- 4
cormat <- cor(matv)
eigend <- eigen(cormat)
invmat <- eigend$vectors[, 1:dimax] %*%
  (t(eigend$vectors[, 1:dimax]) / eigend$values[1:dimax])
# Regularized inverse using \emph{RcppArmadillo}
invarma <- calc_inv(cormat, dimax=dimax)
all.equal(invmat, invarma)
# Microbenchmark \emph{RcppArmadillo} code
library(microbenchmark)
summary(microbenchmark(
  rcode = {eigend <- eigen(cormat)
eigend$vectors[, 1:dimax] %*% (t(eigend$vectors[, 1:dimax]) / eigend$values[1:dimax])},
  rcpp = calc_inv(cormat, dimax=dimax),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
# Install package reticulate
install.packages("reticulate")
# Start Python session
reticulate::repl_python()
# Exit Python session
exit
