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

# Round time index to seconds
tickg[, zoo::index := as.POSIXct(round.POSIXt(index, "secs"))]
# Aggregate to OHLC by seconds
ohlc <- tickg[, .(open=first(price), high=max(price), low=min(price), close=last(price), volume=sum(volume)), by=index]
# Round time index to minutes
tickg[, zoo::index := as.POSIXct(round.POSIXt(index, "mins"))]
# Aggregate to OHLC by minutes
ohlc <- tickg[, .(open=first(price), high=max(price), low=min(price), close=last(price), volume=sum(volume)), by=index]

# Coerce OHLC prices to xts
ohlc <- xts::xts(ohlc[, -"index"], ohlc$index)
# Plot dygraph of the OHLC prices
dygraphs::dygraph(ohlc[, -5], main="XLK Trade Ticks for 2020-03-16 (OHLC)") %>%
  dyCandlestick()
# Plot the OHLC prices
x11(width=6, height=5)
quantmod::chart_Series(x=ohlc, TA="add_Vo()",
  name="XLK Trade Ticks for 2020-03-16 (OHLC)")

Load package HighFreq
library(HighFreq)
head(HighFreq::SPY)

# Load package HighFreq
library(HighFreq)
# Define symbol
symboln <- "SPY"
# Load OHLC data
dirout <- "/Users/jerzy/Develop/data/minutes/"
symboln <- load(file.path(dirout, paste0(symboln, ".RData")))
interval <-"2013-11-11 09:30:00/2013-11-11 10:30:00"
chart_Series(SPY[interval], name=symboln)

# Install package HighFreq from github
devtools::install_github(repo="algoquant/HighFreq")
# Load package HighFreq
library(HighFreq)
# Get documentation for package HighFreq
# Get short description
packageDescription(HighFreq)
# Load help page
help(package=HighFreq)
# List all datasets in HighFreq
data(package=HighFreq)
# List all objects in HighFreq
ls("package:HighFreq")
# Remove HighFreq from search path
detach("package:HighFreq")

# Load package HighFreq
library(HighFreq)
# You can see SPY when listing objects in HighFreq
ls("package:HighFreq")
# You can see SPY when listing datasets in HighFreq
data(package=HighFreq)
# But the SPY dataset isn't listed in the workspace
ls()
# HighFreq datasets are lazy loaded and available when needed
head(HighFreq::SPY)
# Load all the datasets in package HighFreq
data(hf_data)
# HighFreq datasets are now loaded and in the workspace
head(HighFreq::SPY)

library(rutils)  # Load package rutils
# Calculate SPY percentage returns
ohlc <- HighFreq::SPY
nrows <- NROW(ohlc)
closep <- log(quantmod::Cl(ohlc))
retp <- rutils::diffit(closep)
colnames(retp) <- "SPY"
# Standardize raw returns to make later comparisons
retp <- (retp - mean(retp))/sd(retp)
# Calculate moments and perform normality test
sapply(c(var=2, skew=3, kurt=4), function(x) sum(retp^x)/nrows)
tseries::jarque.bera.test(retp)
# Fit SPY returns using MASS::fitdistr()
optiml <- MASS::fitdistr(retp, densfun="t", df=2)
loc <- optiml$estimate[1]
scalev <- optiml$estimate[2]

x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(1, 1, 1, 1))
# Plot histogram of SPY returns
histp <- hist(retp, col="lightgrey", mgp=c(2, 1, 0),
  xlab="returns (standardized)", ylab="frequency", xlim=c(-3, 3),
  breaks=1e3, freq=FALSE, main="Distribution of High Frequency SPY Returns")
# lines(density(retp, bw=0.2), lwd=3, col="blue")
# Plot t-distribution function
curve(expr=dt((x-loc)/scalev, df=2)/scalev,
type="l", lwd=3, col="red", add=TRUE)
# Plot the Normal probability distribution
curve(expr=dnorm(x, mean=mean(retp),
  sd=sd(retp)), add=TRUE, lwd=3, col="blue")
# Add legend
legend("topright", inset=0.05, bty="n",
  leg=c("t-distr", "normal"), y.intersp=0.5,
  lwd=6, lty=1, col=c("red", "blue"))

# Hourly SPY percentage returns
closep <- log(Cl(xts::to.period(x=ohlc, period="hours")))
retsh <- rutils::diffit(closep)
retsh <- (retsh - mean(retsh))/sd(retsh)
# Daily SPY percentage returns
closep <- log(Cl(xts::to.period(x=ohlc, period="days")))
retd <- rutils::diffit(closep)
retd <- (retd - mean(retd))/sd(retd)
# Calculate moments
sapply(list(minutely=retp, hourly=retsh, daily=retd),
 function(rets) {sapply(c(var=2, skew=3, kurt=4),
          function(x) mean(rets^x))
})  # end sapply

x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(1, 1, 1, 1))
# Plot densities of SPY returns
plot(density(retp, bw=0.4), xlim=c(-3, 3),
     lwd=3, mgp=c(2, 1, 0), col="blue",
     xlab="returns (standardized)", ylab="frequency",
     main="Density of High Frequency SPY Returns")
lines(density(retsh, bw=0.4), lwd=3, col="green")
lines(density(retd, bw=0.4), lwd=3, col="red")
# Add legend
legend("topright", inset=0.05, bty="n",
  leg=c("minutely", "hourly", "daily"), y.intersp=0.5,
  lwd=6, lty=1, col=c("blue", "green", "red"))

# Calculate rolling volatility of SPY returns
ret2013 <- retp["2013-11-11/2013-11-15"]
# Calculate rolling volatility
lookb <- 11 # Look-back interval
endd <- seq_along(ret2013)
startp <- c(rep_len(1, lookb),
  endd[1:(NROW(endd)-lookb)])
endd[endd < lookb] <- lookb
vol_rolling <- sapply(seq_along(endd),
  function(it) sd(ret2013[startp[it]:endd[it]]))
vol_rolling <- xts::xts(vol_rolling, zoo::index(ret2013))
# Extract time intervals of SPY returns
indeks <- c(60, diff(xts::.index(ret2013)))
head(indeks)
table(indeks)
# Scale SPY returns by time intervals
ret2013 <- 60*ret2013/indeks
# Calculate scaled rolling volatility
vol_scaled <- sapply(seq_along(endd),
  function(it) sd(ret2013[startp[it]:endd[it]]))
vol_rolling <- cbind(vol_rolling, vol_scaled)
vol_rolling <- na.omit(vol_rolling)
sum(is.na(vol_rolling))
sapply(vol_rolling, range)

# Plot rolling volatility
x11(width=6, height=5)
themev <- chart_theme()
themev$col$line.col <- c("blue", "red")
chart_Series(vol_rolling, theme=themev,
     name="Rolling Volatility with Overnight Spikes")
legend("topright", legend=colnames(vol_rolling),
  inset=0.1, bg="white", lty=1, lwd=6, y.intersp=0.5,
  col=themev$col$line.col, bty="n")

# Volatility of SPY
sqrt(HighFreq::calc_var_ohlc(ohlc))
# Daily SPY volatility and volume
volatd <- sqrt(xts::apply.daily(ohlc, FUN=calc_var_ohlc))
colnames(volatd) <- ("SPY_volatility")
volumv <- quantmod::Vo(ohlc)
volumd <- xts::apply.daily(volumv, FUN=sum)
colnames(volumd) <- ("SPY_volume")
# Plot SPY volatility and volume
datav <- cbind(volatd, volumd)["2008/2009"]
colv <- colnames(datav)
dygraphs::dygraph(datav,
  main="SPY Daily Volatility and Trading Volume") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", col="red", strokeWidth=3) %>%
  dySeries(name=colv[2], axis="y2", col="blue", strokeWidth=3)

# Regress log of daily volume vs volatility
datav <- log(cbind(volumd, volatd))
colv <- colnames(datav)
dframe <- as.data.frame(datav)
formulav <- as.formula(paste(colv, collapse="~"))
regmod <- lm(formulav, data=dframe)
# Durbin-Watson test for autocorrelation of residuals
lmtest::dwtest(regmod)
# Regress diff log of daily volume vs volatility
dframe <- as.data.frame(rutils::diffit(datav))
regmod <- lm(formulav, data=dframe)
lmtest::dwtest(regmod)
summary(regmod)
plot(formulav, data=dframe, main="SPY Daily Trading Volume vs Volatility (log scale)")
abline(regmod, lwd=3, col="red")
mtext(paste("beta =", round(coef(regmod)[2], 3)), cex=1.2, lwd=3, side=2, las=2, adj=(-0.5), padj=(-7))

# 60 minutes of data in lookb interval
lookb <- 60 # Look-back interval
vol2013 <- volumv["2013"]
ret2013 <- retp["2013"]
# Define end points with beginning stub
nrows <- NROW(ret2013)
nagg <- nrows %/% lookb
endd <- nrows-lookb*nagg + (0:nagg)*lookb
startp <- c(1, endd[1:(NROW(endd)-1)])
# Calculate SPY volatility and volume
datav <- sapply(seq_along(endd), function(it) {
  endp <- startp[it]:endd[it]
  c(volume=sum(vol2013[endp]),
    volatility=sd(ret2013[endp]))
})  # end sapply
datav <- t(datav)
datav <- rutils::diffit(log(datav))
dframe <- as.data.frame(datav)

formulav <- as.formula(paste(colnames(datav), collapse="~"))
regmod <- lm(formulav, data=dframe)
lmtest::dwtest(regmod)
summary(regmod)
plot(formulav, data=dframe,
     main="SPY Hourly Trading Volume vs Volatility (log scale)")
abline(regmod, lwd=3, col="red")
mtext(paste("beta =", round(coef(regmod)[2], 3)), cex=1.2, lwd=3, side=2, las=2, adj=(-0.5), padj=(-7))

# Scale returns using volume (volume clock)
retsc <- ifelse(volumv > 1e4, retp/sqrt(volumv), 0)
retsc <- retsc/sd(retsc)
# Calculate moments of scaled returns
nrows <- NROW(retp)
sapply(list(retp=retp, retsc=retsc),
  function(rets) {sapply(c(skew=3, kurt=4),
     function(x) sum((rets/sd(rets))^x)/nrows)
})  # end sapply

x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(1, 1, 1, 1))
# Plot densities of SPY returns
plot(density(retp), xlim=c(-3, 3),
     lwd=3, mgp=c(2, 1, 0), col="blue",
     xlab="returns (standardized)", ylab="frequency",
     main="Density of Volume-scaled High Frequency SPY Returns")
lines(density(retsc, bw=0.4), lwd=3, col="red")
curve(expr=dnorm, add=TRUE, lwd=3, col="green")
# Add legend
legend("topright", inset=0.05, bty="n", y.intersp=0.5,
  leg=c("minutely", "scaled", "normal"),
  lwd=6, lty=1, col=c("blue", "red", "green"))

# Ljung-Box test for minutely SPY returns
Box.test(retp, lag=10, type="Ljung")
# Ljung-Box test for daily SPY returns
Box.test(retd, lag=10, type="Ljung")
# Ljung-Box test statistics for scaled SPY returns
sapply(list(retp=retp, retsc=retsc),
  function(rets) {
    Box.test(rets, lag=10, type="Ljung")$statistic
})  # end sapply
# Ljung-Box test statistics for aggregated SPY returns
sapply(list(minutely=retp, hourly=retsh, daily=retd),
  function(rets) {
    Box.test(rets, lag=10, type="Ljung")$statistic
})  # end sapply

# Set plot parameters
x11(width=6, height=8)
par(mar=c(4, 4, 2, 1), oma=c(0, 0, 0, 0))
layout(matrix(c(1, 2), ncol=1), widths=c(6, 6), heights=c(4, 4))
# Plot the partial autocorrelations of minutely SPY returns
pacfl <- pacf(as.numeric(retp), lag=10,
     xlab="lag", ylab="partial autocorrelation", main="")
title("Partial Autocorrelations of Minutely SPY Returns", line=1)
# Plot the partial autocorrelations of scaled SPY returns
pacfs <- pacf(as.numeric(retsc), lag=10,
     xlab="lag", ylab="partial autocorrelation", main="")
title("Partial Autocorrelations of Scaled SPY Returns", line=1)
# Calculate the sums of partial autocorrelations
sum(pacfl$acf)
sum(pacfs$acf)

# Calculate market illiquidity
liquidv <- sqrt(volumd)/volatd
# Plot market illiquidity
x11(width=6, height=7) ; par(mfrow=c(2, 1))
themev <- chart_theme()
themev$col$line.col <- c("blue")
chart_Series(liquidv["2010"], theme=themev,
  name="SPY Liquidity in 2010", plot=FALSE)
themev$col$line.col <- c("red")
chart_Series(volatd["2010"],
  theme=themev, name="SPY Volatility in 2010")

# Calculate intraday time index with hours and minutes
datev <- format(zoo::index(retp), "%H:%M")
# Aggregate the mean volume
volumagg <- tapply(X=volumv, INDEX=datev, FUN=mean)
volumagg <- drop(volumagg)
# Aggregate the mean volatility
volagg <- tapply(X=retp^2, INDEX=datev, FUN=mean)
volagg <- sqrt(drop(volagg))
# Coerce to xts
datev <- as.POSIXct(paste(Sys.Date(), names(volumagg)))
volumagg <- xts::xts(volumagg, datev)
volagg <- xts::xts(volagg, datev)
# Plot seasonality of volume and volatility
x11(width=6, height=7) ; par(mfrow=c(2, 1))
themev <- chart_theme()
themev$col$line.col <- c("blue")
chart_Series(volumagg[c(-1, -NROW(volumagg))], theme=themev,
  name="Intraday Seasonality of SPY Volume", plot=FALSE)
themev$col$line.col <- c("red")
chart_Series(volagg[c(-1, -NROW(volagg))], theme=themev,
  name="Intraday Seasonality of SPY Volatility")

# Calculate market liquidity
liquidv <- sqrt(volumagg)/volagg
# Plot intraday seasonality of market liquidity
x11(width=6, height=7) ; par(mfrow=c(2, 1))
themev <- chart_theme()
themev$col$line.col <- c("blue")
chart_Series(liquidv[c(-1, -NROW(liquidv))], theme=themev,
  name="Intraday Seasonality of SPY Liquidity", plot=FALSE)
themev$col$line.col <- c("red")
chart_Series(volagg[c(-1, -NROW(volagg))], theme=themev,
  name="Intraday Seasonality of SPY Volatility")

# Create list of vectors
listv <- lapply(1:3, function(x) sample(6))
# Bind list elements into matrix - doesn't work
rbind(listv)
# Bind list elements into matrix - tedious
rbind(listv[[1]], listv[[2]], listv[[3]])
# Bind list elements into matrix - works!
do.call(rbind, listv)
# Create numeric list
listv <- list(1, 2, 3, 4)
do.call(rbind, listv)  # Returns single column matrix
do.call(cbind, listv)  # Returns single row matrix
# Recycling rule applied
do.call(cbind, list(1:2, 3:5))
# NULL element is skipped
do.call(cbind, list(1, NULL, 3, 4))
# NA element isn't skipped
do.call(cbind, list(1, NA, 3, 4))

library(microbenchmark)
listv <- lapply(1:5, rnorm, n=10)
matv <- do.call(rbind, listv)
dim(matv)
do_call_rbind <- function(listv) {
  while (NROW(listv) > 1) {
# Index of odd list elements
    odd_index <- seq(from=1, to=NROW(listv), by=2)
# Bind odd and even elements, and divide listv by half
    listv <- lapply(odd_index, function(indeks) {
if (indeks==NROW(listv)) return(listv[[indeks]])
rbind(listv[[indeks]], listv[[indeks+1]])
    })  # end lapply
  }  # end while
# listv has only one element - return it
  listv[[1]]
}  # end do_call_rbind
all.equal(matv, do_call_rbind(listv))

library(microbenchmark)
airquality[(airquality$Solar.R > 320 & !is.na(airquality$Solar.R)), ]
subset(x=airquality, subset=(Solar.R > 320))
summary(microbenchmark(
    subset=subset(x=airquality, subset=(Solar.R > 320)),
    brackets=airquality[(airquality$Solar.R > 320 &
            !is.na(airquality$Solar.R)), ],
times=10))[, c(1, 4, 5)]  # end microbenchmark summary

unique(iris$Species)  # Species has three distinct values
# Split into separate data frames by hand
setosa <- iris[iris$Species=="setosa", ]
versi <- iris[iris$Species=="versicolor", ]
virgin <- iris[iris$Species=="virginica", ]
dim(setosa)
head(setosa, 2)
# Split iris into list based on Species
irisplit <- split(iris, iris$Species)
str(irisplit, max.confl=1)
names(irisplit)
dim(irisplit$setosa)
head(irisplit$setosa, 2)
all.equal(setosa, irisplit$setosa)

unique(mtcars$cyl)  # cyl has three unique values
# Split mpg column based on number of cylinders
split(mtcars$mpg, mtcars$cyl)
# Split mtcars data frame based on number of cylinders
carsplit <- split(mtcars, mtcars$cyl)
str(carsplit, max.confl=1)
names(carsplit)
# Aggregate the mean mpg over split mtcars data frame
sapply(carsplit, function(x) mean(x$mpg))
# Or: split mpg column and aggregate the mean
sapply(split(mtcars$mpg, mtcars$cyl), mean)
# Same but using with()
with(mtcars, sapply(split(mpg, cyl), mean))
# Or: aggregate() using formula syntax
aggregate(x=(mpg ~ cyl), data=mtcars, FUN=mean)
# Or: aggregate() using data frame syntax
aggregate(x=mtcars$mpg, by=list(cyl=mtcars$cyl), FUN=mean)
# Or: using name for mpg
aggregate(x=list(mpg=mtcars$mpg), by=list(cyl=mtcars$cyl), FUN=mean)
# Aggregate() all columns
aggregate(x=mtcars, by=list(cyl=mtcars$cyl), FUN=mean)
# Aggregate multiple columns using formula syntax
aggregate(x=(cbind(mpg, hp) ~ cyl), data=mtcars, FUN=mean)

# Mean mpg for each cylinder group
tapply(X=mtcars$mpg, INDEX=mtcars$cyl, FUN=mean)
# using with() environment
with(mtcars, tapply(X=mpg, INDEX=cyl, FUN=mean))
# Function sapply() instead of tapply()
with(mtcars, sapply(sort(unique(cyl)), function(x) {
  structure(mean(mpg[x==cyl]), names=x)
}))  # end with
# Function by() instead of tapply()
with(mtcars, by(data=mpg, INDICES=cyl, FUN=mean))

# Get several mpg stats for each cylinder group
cardata <- sapply(carsplit, function(x) {
  c(mean=mean(x$mpg), max=max(x$mpg), min=min(x$mpg))
}  # end anonymous function
)  # end sapply
cardata  # sapply() produces a matrix
# Now same using lapply
cardata <- lapply(carsplit, function(x) {
  c(mean=mean(x$mpg), max=max(x$mpg), min=min(x$mpg))
}  # end anonymous function
)  # end sapply
is.list(cardata)  # lapply produces a list
# do.call flattens list into a matrix
do.call(cbind, cardata)

# Download CRSPpanel.txt from the NYU share drive
# Read the file using read.table() with header and sep arguments
paneld <- read.table(file="/Users/jerzy/Develop/lecture_slides/data/CRSPpanel.txt",
                   header=TRUE, sep="\t")
paneld[1:5, 1:5]
attach(paneld)
# Split paneld based on Industry column
panelds <- split(paneld, Industry)
# Number of companies in each Industry
sapply(panelds, NROW)
# Number of Sectors that each Industry belongs to
sapply(panelds, function(x) {
  NROW(unique(x$Sector))
})  # end sapply
# Or
aggregate(x=(Sector ~ Industry),
  data=paneld, FUN=function(x) NROW(unique(x)))
# Industries and the Sector to which they belong
aggregate(x=(Sector ~ Industry), data=paneld, FUN=unique)
# Or
aggregate(x=Sector, by=list(Industry), FUN=unique)
# Or
sapply(unique(Industry), function(x) {
  Sector[match(x, Industry)]
})  # end sapply

# Split paneld based on Sector column
panelds <- split(paneld, Sector)
# Number of companies in each Sector
sapply(panelds, NROW)
# Industries belonging to each Sector (jagged array)
secind <- sapply(panelds, function(x) unique(x$Industry))
# Or use aggregate() (returns a data frame)
secind2 <- aggregate(x=(Industry ~ Sector),
  data=paneld, FUN=function(x) unique(x))
# Or use aggregate() with "by" argument
secind2 <- aggregate(x=Industry, by=list(Sector),
    FUN=function(x) as.vector(unique(x)))
# Coerce secind2 into a jagged array
namev <- secind2[, 1]
secind2 <- secind2[, 2]
names(secind2) <- namev
all.equal(secind2, secind)
# Or use tapply() (returns an array)
secind2 <- tapply(X=Industry, INDEX=Sector, FUN=unique)
# Coerce secind2 into a jagged array
secind2 <- drop(as.matrix(secind2))
all.equal(secind2, secind)

# Average ROE in each Industry
sapply(split(ROE, Industry), mean)
# Average, min, and max ROE in each Industry
t(sapply(split(ROE, Industry), FUN=function(x)
  c(mean=mean(x), max=max(x), min=min(x))))
# Split paneld based on Industry column
panelds <- split(paneld, Industry)
# Average ROE and EPS in each Industry
t(sapply(panelds, FUN=function(x)
  c(mean_roe=mean(x$ROE),
    mean_eps=mean(x$EPS.EXCLUDE.EI))))
# Or: split paneld based on Industry column
panelds <- split(paneld[, c("ROE", "EPS.EXCLUDE.EI")],
  paneld$Industry)
# Average ROE and EPS in each Industry
t(sapply(panelds, FUN=function(x) sapply(x, mean)))
# Average ROE and EPS using aggregate()
aggregate(x=paneld[, c("ROE", "EPS.EXCLUDE.EI")],
  by=list(paneld$Industry), FUN=mean)

Sys.Date()  # Get today's date
as.Date(1e3)  # Coerce numeric into date object
datetime <- as.Date("2014-07-14")  # "%Y-%m-%d" or "%Y/%m/%d"
datetime
class(datetime)  # Date object
as.Date("07-14-2014", "%m-%d-%Y")  # Specify format
datetime + 20  # Add 20 days
# Extract internal representation to integer
as.numeric(datetime)
datep <- as.Date("07/14/2013", "%m/%d/%Y")
datep
# Difference between dates
difftime(datetime, datep, units="weeks")
weekdays(datetime)  # Get day of the week
# Coerce numeric into date-times
datetime <- 0
attributes(datetime) <- list(class="Date")
datetime  # "Date" object
structure(0, class="Date")  # "Date" object
structure(10000.25, class="Date")

datetime <- Sys.time()  # Get today's date and time
datetime
class(datetime)  # POSIXct object
# POSIXct stored as integer moment of time
as.numeric(datetime)
# Parse character string "%Y-%m-%d %H:%M:%S" to POSIXct object
datetime <- as.POSIXct("2014-07-14 13:30:10")
# Different time zones can have same clock time
as.POSIXct("2014-07-14 13:30:10", tz="America/New_York")
as.POSIXct("2014-07-14 13:30:10", tz="UTC")
# Format argument allows parsing different date-time string formats
as.POSIXct("07/14/2014 13:30:10", format="%m/%d/%Y %H:%M:%S",
     tz="America/New_York")

# Same moment of time corresponds to different clock times
timeny <- as.POSIXct("2014-07-14 13:30:10", tz="America/New_York")
timeldn <- as.POSIXct("2014-07-14 13:30:10", tz="UTC")
# Add five hours to POSIXct
timeny + 5*60*60
# Subtract POSIXct
timeny - timeldn
class(timeny - timeldn)
# Compare POSIXct
timeny > timeldn
# Create vector of POSIXct times during trading hours
timev <- seq(
  from=as.POSIXct("2014-07-14 09:30:00", tz="America/New_York"),
  to=as.POSIXct("2014-07-14 16:00:00", tz="America/New_York"),
  by="10 min")
head(timev, 3)
tail(timev, 3)

# POSIXct is stored as integer moment of time
datetimen <- as.numeric(datetime)
# Same moment of time corresponds to different clock times
as.POSIXct(datetimen, origin="1970-01-01", tz="America/New_York")
as.POSIXct(datetimen, origin="1970-01-01", tz="UTC")
# Same clock time corresponds to different moments of time
as.POSIXct("2014-07-14 13:30:10", tz="America/New_York") -
  as.POSIXct("2014-07-14 13:30:10", tz="UTC")
# Add 20 seconds to POSIXct
datetime + 20

datetime  # POSIXct date and time
# Parse POSIXct to string representing the clock time
format(datetime)
class(format(datetime))  # Character string
# Get clock times in different time zones
format(datetime, tz="America/New_York")
format(datetime, tz="UTC")
# Format with custom format strings
format(datetime, "%m/%Y")
format(datetime, "%m-%d-%Y %H hours")
# Trunc to hour
format(datetime, "%m-%d-%Y %H:00:00")
# Date converted to midnight UTC moment of time
as.POSIXct(Sys.Date())
as.POSIXct(as.numeric(as.POSIXct(Sys.Date())),
     origin="1970-01-01",
     tz="UTC")

# Parse character string "%Y-%m-%d %H:%M:%S" to POSIXlt object
datetime <- as.POSIXlt("2014-07-14 18:30:10")
datetime
class(datetime)  # POSIXlt object
as.POSIXct(datetime)  # Coerce to POSIXct object
# Extract internal list representation to vector
unlist(datetime)
datetime + 20  # Add 20 seconds
class(datetime + 20)  # Implicit coercion to POSIXct
trunc(datetime, units="hours")  # Truncate to closest hour
trunc(datetime, units="days")  # Truncate to closest day
methods(trunc)  # Trunc methods
trunc.POSIXt

# Set time-zone to UTC
Sys.setenv(TZ="UTC")
Sys.timezone()  # Get time-zone
Sys.time()  # Today's date and time
# Set time-zone back to New York
Sys.setenv(TZ="America/New_York")
Sys.time()  # Today's date and time
# Standard Time in effect
as.POSIXct("2013-03-09 11:00:00", tz="America/New_York")
# Daylight Savings Time in effect
as.POSIXct("2013-03-10 11:00:00", tz="America/New_York")
datetime <- Sys.time()  # Today's date and time
# Convert to character in different TZ
format(datetime, tz="America/New_York")
format(datetime, tz="UTC")
# Parse back to POSIXct
as.POSIXct(format(datetime, tz="America/New_York"))
# Difference between New_York time and UTC
as.POSIXct(format(Sys.time(), tz="UTC")) -
  as.POSIXct(format(Sys.time(), tz="America/New_York"))

library(lubridate)  # Load lubridate
# Parse strings into date-times
as.POSIXct("07-14-2014", format="%m-%d-%Y", tz="America/New_York")
datetime <- lubridate::mdy("07-14-2014", tz="America/New_York")
datetime
class(datetime)  # POSIXct object
lubridate::dmy("14.07.2014", tz="America/New_York")

# Parse numeric into date-times
as.POSIXct(as.character(14072014), format="%d%m%Y",
                  tz="America/New_York")
lubridate::dmy(14072014, tz="America/New_York")

# Parse decimal to date-times
lubridate::decimal_date(datetime)
lubridate::date_decimal(2014.25, tz="America/New_York")
date_decimal(decimal_date(datetime), tz="America/New_York")

library(lubridate)  # Load lubridate
datetime <- lubridate::ymd_hms(20140714142010,
               tz="America/New_York")
datetime
# Get same moment of time in "UTC" time zone
lubridate::with_tz(datetime, "UTC")
as.POSIXct(format(datetime, tz="UTC"), tz="UTC")
# Get same clock time in "UTC" time zone
lubridate::force_tz(datetime, "UTC")
as.POSIXct(format(datetime, tz="America/New_York"),
     tz="UTC")
# Same moment of time
datetime - with_tz(datetime, "UTC")
# Different moments of time
datetime - force_tz(datetime, "UTC")

library(lubridate)  # Load lubridate
# Daylight Savings Time handling periods vs durations
datetime <- as.POSIXct("2013-03-09 11:00:00", tz="America/New_York")
datetime
datetime + lubridate::ddays(1)  # Add duration
datetime + lubridate::days(1)  # Add period

leap_year(2012)  # Leap year
datetime <- lubridate::dmy(01012012, tz="America/New_York")
datetime
datetime + lubridate::dyears(1)  # Add duration
datetime + lubridate::years(1)  # Add period

library(lubridate)  # Load lubridate
datetime <- lubridate::ymd_hms(20140714142010, tz="America/New_York")
datetime
# Add periods to a date-time
c(datetime + lubridate::seconds(1), datetime + lubridate::minutes(1),
  datetime + lubridate::days(1), datetime + period(months=1))

# Create vectors of dates
datetime <- lubridate::ymd(20140714, tz="America/New_York")
datetime + 0:2 * period(months=1)  # Monthly dates
datetime + period(months=0:2)
datetime + 0:2 * period(months=2)  # bi-monthly dates
datetime + seq(0, 5, by=2) * period(months=1)
seq(datetime, length=3, by="2 months")

library(lubridate)  # Load lubridate
# Adding monthly periods can create invalid dates
datetime <- lubridate::ymd(20120131, tz="America/New_York")
datetime + 0:2 * period(months=1)
datetime + period(months=1)
datetime + period(months=2)
# Create vector of end-of-month dates
datetime %m-% months(13:1)

library(zoo)  # Load zoo
library(RQuantLib)  # Load RQuantLib

Create daily date series of class "Date"
datev <- Sys.Date() + -5:2
datev

Create Boolean vector of business days
Use RQuantLib calendar
isbusday <- RQuantLib::isBusinessDay(
  calendar="UnitedStates/GovernmentBond", datev)

Create daily series of business days
datev[isbusday]

library(zoo)  # Load package zoo
datetime <- Sys.Date()  # Create date series of class "Date"
datev <- datetime + 0:365  # Daily series over one year
head(datev, 4)  # Print first few dates
format(head(datev, 4), "%m/%d/%Y")  # Print first few dates
Create daily date-time series of class "POSIXct"
datev <- seq(Sys.time(), by="days", length.out=365)
head(datev, 4)  # Print first few dates
format(head(datev, 4), "%m/%d/%Y %H:%M:%S")  # Print first few dates
Create series of monthly dates of class "zoo"
monthv <- yearmon(2010+0:36/12)
head(monthv, 4)  # Print first few dates
Create series of quarterly dates of class "zoo"
qrtv <- yearqtr(2010+0:16/4)
head(qrtv, 4)  # Print first few dates
Parse quarterly "zoo" dates to POSIXct
Sys.setenv(TZ="UTC")
as.POSIXct(head(qrtv, 4))

library(lubridate)  # Load lubridate
# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
# Create daily time series ending today
startd <- decimal_date(Sys.Date()-6)
endd <- decimal_date(Sys.Date())
# Create vector of geometric Brownian motion
datav <- exp(cumsum(rnorm(6)/100))
tstep <- NROW(datav)/(endd-startd)
timeser <- ts(data=datav, start=startd, frequency=tstep)
timeser  # Display time series
# Display index dates
as.Date(date_decimal(zoo::coredata(time(timeser))))
# bi-monthly geometric Brownian motion starting mid-1990
timeser <- ts(data=exp(cumsum(rnorm(96)/100)),
       frequency=6, start=1990.5)

# Show some methods for class "ts"
matrix(methods(class="ts")[3:8], ncol=2)
# "tsp" attribute specifies the date-time index
attributes(timeser)
# Extract the index
tail(time(timeser), 11)
# The index is equally spaced
diff(tail(time(timeser), 11))
# Subset the time series
window(timeser, start=1992, end=1992.25)

# Create plot
plot(timeser, type="l", col="red", lty="solid",
     xlab="", ylab="")
title(main="Brownian Motion", line=1)  # Add title

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

Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
library(zoo)  # Load package zoo
Create zoo time series of random returns
datev <- Sys.Date() + 0:11
zoots <- zoo(rnorm(NROW(datev)), order.by=datev)
zoots
attributes(zoots)
class(zoots)  # Class "zoo"
tail(zoots, 3)  # Get last few elements

library(zoo)  # Load package zoo
zoo::coredata(zoots)  # Extract coredata
zoo::index(zoots)  # Extract time index
start(zoots)  # First date
end(zoots)  # Last date
zoots[start(zoots)]  # First element
zoots[end(zoots)]  # Last element
zoo::coredata(zoots) <- rep(1, NROW(zoots))  # Replace coredata
cumsum(zoots)  # Cumulative sum
cummax(cumsum(zoots))
cummin(cumsum(zoots))

library(zoo)  # Load package zoo
zoots <- zoo(matrix(cumsum(rnorm(10)), nc=1),
  order.by=seq(from=as.Date("2013-06-15"), by="day", len=10))
colnames(zoots) <- "zoots"
tail(zoots)
dim(zoots)
attributes(zoots)

library(zoo)  # Load package zoo
zoo::coredata(zoots) <- (1:10)^2  # Replace coredata
zoots
lag(zoots)  # One day lag
lag(zoots, 2)  # Two day lag
lag(zoots, k=-1)  # Proper one day lag
diff(zoots)  # Diff with one day lag
Proper lag and original length
lag(zoots, -2, na.pad=TRUE)

# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
library(zoo)  # Load package zoo
# Create index of daily dates
datev <- seq(from=as.Date("2014-07-14"), by="day", length.out=1000)
# Create vector of geometric Brownian motion
datav <- exp(cumsum(rnorm(NROW(datev))/100))
# Create zoo series of geometric Brownian motion
zoots <- zoo(x=datav, order.by=datev)

# Plot using method plot.zoo()
plot.zoo(zoots, xlab="", ylab="")
title(main="Brownian Motion", line=1)  # Add title

library(zoo)  # Load package zoo
# Subset zoo as matrix
zoots[459:463, 1]
# Subset zoo using window()
window(zoots,
 start=as.Date("2014-10-15"),
 end=as.Date("2014-10-19"))
# Subset zoo using Date object
zoots[as.Date("2014-10-15")]

# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
library(zoo)  # Load package zoo
# Create daily date series of class "Date"
tday <- Sys.Date()
index1 <- seq(tday-2*365, by="days", length.out=365)
# Create zoo time series of random returns
zoo1 <- zoo(rnorm(NROW(index1)), order.by=index1)
# Create another zoo time series of random returns
index2 <- seq(tday-360, by="days", length.out=365)
zoo2 <- zoo(rnorm(NROW(index2)), order.by=index2)
# rbind the two time series - ts1 supersedes ts2
zooub2 <- zoo2[zoo::index(zoo2) > end(zoo1)]
zoo3 <- rbind(zoo1, zooub2)
# Plot zoo time series of geometric Brownian motion
plot(exp(cumsum(zoo3)/100), xlab="", ylab="")
# Add vertical lines at stitch point
abline(v=end(zoo1), col="blue", lty="dashed")
abline(v=start(zoo2), col="red", lty="dashed")
title(main="Brownian Motions Stitched Together", line=1)  # Add title

# Create daily date series of class "Date"
index1 <- Sys.Date() + -3:1
# Create zoo time series of random returns
zoo1 <- zoo(rnorm(NROW(index1)), order.by=index1)
# Create another zoo time series of random returns
index2 <- Sys.Date() + -1:3
zoo2 <- zoo(rnorm(NROW(index2)), order.by=index2)
merge(zoo1, zoo2)  # union of dates
# Intersection of dates
merge(zoo1, zoo2, all=FALSE)

# Create matrix containing NA values
matv <- sample(18)
matv[sample(NROW(matv), 4)] <- NA
matv <- matrix(matv, nc=3)
# Replace NA values with most recent non-NA values
zoo::na.locf(matv)
# Get time series of prices
pricev <- mget(c("VTI", "VXX"), envir=rutils::etfenv)
pricev <- lapply(pricev, quantmod::Cl)
pricev <- rutils::do_call(cbind, pricev)
sum(is.na(pricev))
# Carry forward and backward non-NA prices
pricev <- zoo::na.locf(pricev, na.rm=FALSE)
pricev <- zoo::na.locf(pricev, na.rm=FALSE, fromLast=TRUE)
sum(is.na(pricev))
# Remove whole rows containing NA returns
retp <- rutils::etfenv$returns
sum(is.na(retp))
retp <- na.omit(retp)
# Or carry forward non-NA returns (preferred)
retp <- rutils::etfenv$returns
retp[1, is.na(retp[1, ])] <- 0
retp <- zoo::na.locf(retp, na.rm=FALSE)
sum(is.na(retp))

# Replace NAs in xts time series
pricev <- rutils::etfenv$prices[, 1]
head(pricev)
sum(is.na(pricev))
library(quantmod)
pricezoo <- zoo::na.locf(pricev, na.rm=FALSE, fromLast=TRUE)
pricexts <- xts:::na.locf.xts(pricev, fromLast=TRUE)
all.equal(pricezoo, pricexts, check.attributes=FALSE)
library(microbenchmark)
summary(microbenchmark(
  zoo=zoo::na.locf(pricev, fromLast=TRUE),
  xts=xts:::na.locf.xts(pricev, fromLast=TRUE),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

library(lubridate)  # Load lubridate
library(zoo)  # Load package zoo
# methods(as.zoo)  # Many methods of coercing into zoo
class(EuStockMarkets)  # Multiple ts object
# Coerce mts object into zoo
zoots <- as.zoo(EuStockMarkets)
class(zoo::index(zoots))  # Index is numeric
head(zoots, 3)
# Approximately convert index into class "Date"
zoo::index(zoots) <- as.Date(365*(zoo::index(zoots)-1970))
head(zoots, 3)
# Convert index into class "POSIXct"
zoots <- as.zoo(EuStockMarkets)
zoo::index(zoots) <- date_decimal(zoo::index(zoots))
head(zoots, 3)

library(lubridate)  # Load lubridate
library(zoo)  # Load package zoo
# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
# Create index of daily dates
datev <- seq(from=as.Date("2014-07-14"), by="day", length.out=1000)
# Create vector of geometric Brownian motion
datav <- exp(cumsum(rnorm(NROW(datev))/100))
# Create zoo time series of geometric Brownian motion
zoots <- zoo(x=datav, order.by=datev)
head(zoots, 3)  # zoo object
# as.ts() creates ts object with frequency=1
timeser <- as.ts(zoots)
tsp(timeser)  # Frequency=1
# Get start and end dates of zoots
startd <- decimal_date(start(zoots))
endd <- decimal_date(end(zoots))
# Calculate frequency of zoots
tstep <- NROW(zoots)/(endd-startd)
datav <- zoo::coredata(zoots)  # Extract data from zoots
# Create ts object using ts()
timeser <- ts(data=datav, start=startd, frequency=tstep)
# Display start of time series
window(timeser, start=start(timeser), end=start(timeser)+4/365)
head(time(timeser))  # Display index dates
head(as.Date(date_decimal(zoo::coredata(time(timeser)))))

library(lubridate)  # Load lubridate
library(zoo)  # Load package zoo
# Create weekday Boolean vector
wkdays <- weekdays(zoo::index(zoots))
wkdayl <- !((wkdays == "Saturday") | (wkdays == "Sunday"))
# Remove weekends from zoo time series
zoots <- zoots[wkdayl, ]
head(zoots, 7)  # zoo object
# as.ts() creates NA values
timeser <- as.ts(zoots)
head(timeser, 7)
# Create vector of regular dates, including weekends
datev <- seq(from=start(zoots), by="day", length.out=NROW(zoots))
zoo::index(zoots) <- datev
timeser <- as.ts(zoots)
head(timeser, 7)

# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
library(xts)  # Load package xts
# Create xts time series of random returns
datev <- Sys.Date() + 0:3
xtsv <- xts(rnorm(NROW(datev)), order.by=datev)
names(xtsv) <- "random"
xtsv
tail(xtsv, 3)  # Get last few elements
first(xtsv)  # Get first element
last(xtsv)  # Get last element
class(xtsv)  # Class "xts"
attributes(xtsv)
# Get the time zone of an xts object
tzone(xtsv)

load(file="/Users/jerzy/Develop/lecture_slides/data/zoo_data.RData")
class(zoo_stx)
# as.xts() coerces zoo series into xts series
library(xts)  # Load package xts
pricexts <- as.xts(zoo_stx)
dim(pricexts)
head(pricexts[, 1:4], 4)
# Plot using plot.xts method
xts::plot.xts(pricexts[, "Close"], xlab="", ylab="", main="")
title(main="Stock Prices")  # Add title

library(xts)  # Load xts
library(lubridate)  # Load lubridate
# Coerce EuStockMarkets into class xts
xtsv <- xts(zoo::coredata(EuStockMarkets),
      order.by=date_decimal(zoo::index(EuStockMarkets)))
# Plot all columns in single panel: xts v.0.9-8
colorv <- rainbow(NCOL(xtsv))
plot(xtsv, main="EuStockMarkets using xts",
     col=colorv, major.ticks="years",
     minor.ticks=FALSE)
legend("topleft", legend=colnames(EuStockMarkets),
 inset=0.2, cex=0.7, , lty=rep(1, NCOL(xtsv)),
 lwd=3, col=colorv, bg="white")
# Plot only first column: xts v.0.9-7
plot(xtsv[, 1], main="EuStockMarkets using xts",
     col=colorv[1], major.ticks="years",
     minor.ticks=FALSE)
# Plot remaining columns
for (colnum in 2:NCOL(xtsv))
  lines(xtsv[, colnum], col=colorv[colnum])
# Plot using quantmod
library(quantmod)
plotheme <- chart_theme()
plotheme$col$line.col <- colors
chart_Series(x=xtsv, theme=plotheme,
       name="EuStockMarkets using quantmod")
legend("topleft", legend=colnames(EuStockMarkets),
 inset=0.2, cex=0.7, , lty=rep(1, NCOL(xtsv)),
 lwd=3, col=colorv, bg="white")

library(rutils)
library(ggplot2)
pricev <- rutils::etfenv$prices[, 1]
pricev <- na.omit(pricev)
# Create ggplot object
plotobj <- qplot(x=zoo::index(pricev),
          y=as.numeric(pricev),
          geom="line",
          main=names(pricev)) +
  xlab("") + ylab("") +
  theme(  # Add legend and title
    legend.position=c(0.1, 0.5),
    plot.title=element_text(vjust=-2.0),
    plot.background=element_blank()
  )  # end theme
# Render ggplot object
plotobj

library(rutils)  # Load xts time series data
library(reshape2)
library(ggplot2)
pricev <- rutils::etfenv$prices[, c("VTI", "IEF")]
pricev <- na.omit(pricev)
# Create data frame of time series
dframe <- data.frame(datev=zoo::index(pricev), zoo::coredata(pricev))
# reshape data into a single column
dframe <- reshape2::melt(dframe, id="dates")
x11(width=6, height=5)  # Open plot window
# ggplot the melted dframe
ggplot(data=dframe,
 mapping=aes(x=datev, y=value, colour=variable)) +
 geom_line() +
  xlab("") + ylab("") +
  ggtitle("VTI and IEF") +
  theme(  # Add legend and title
    legend.position=c(0.2, 0.8),
    plot.title=element_text(vjust=-2.0)
  )  # end theme

# Load rutils which contains etfenv dataset
library(rutils)
library(dygraphs)
pricev <- rutils::etfenv$prices[, c("VTI", "IEF")]
pricev <- na.omit(pricev)
# Plot dygraph with date range selector
dygraph(pricev, main="VTI and IEF prices") %>%
  dyOptions(colors=c("blue","green")) %>%
  dyRangeSelector()

# Load rutils which contains etfenv dataset
library(rutils)
library(plotly)
pricev <- rutils::etfenv$prices[, c("VTI", "IEF")]
pricev <- na.omit(pricev)
# Create data frame of time series
dframe <- data.frame(datev=zoo::index(pricev),
    zoo::coredata(pricev))
# Plotly syntax using pipes
dframe %>%
  plot_ly(x=~datev, y=~VTI, type="scatter", mode="lines", name="VTI") %>%
  add_trace(x=~datev, y=~IEF, type="scatter", mode="lines", name="IEF") %>%
  layout(title="VTI and IEF prices",
   xaxis=list(title="Time"),
   yaxis=list(title="Stock Prices"),
   legend=list(x=0.1, y=0.9))
# Or use standard plotly syntax
plotobj <- plot_ly(data=dframe, x=~datev, y=~VTI, type="scatter", mode="lines", name="VTI")
plotobj <- add_trace(p=plotobj, x=~datev, y=~IEF, type="scatter", mode="lines", name="IEF")
plotobj <- layout(p=plotobj, title="VTI and IEF prices", xaxis=list(title="Time"), yaxis=list(title="Stock Prices"), legend=list(x=0.1, y=0.9))
plotobj

# Subset xts using a date range string
pricev <- rutils::etfenv$prices
pricesub <- pricev["2014-10-15/2015-01-10", 1:4]
first(pricesub)
last(pricesub)
# Subset Nov 2014 using a date string
pricesub <- pricev["2014-11", 1:4]
first(pricesub)
last(pricesub)
# Subset all data after Nov 2014
pricesub <- pricev["2014-11/", 1:4]
first(pricesub)
last(pricesub)
# Comma after date range not necessary
all.equal(pricev["2014-11", ], pricev["2014-11"])
# .subset_xts() is faster than the bracket []
library(microbenchmark)
summary(microbenchmark(
  bracket=pricev[10:20, ],
  subset=xts::.subset_xts(pricev, 10:20),
  times=10))[, c(1, 4, 5)]

# Specify string representing a date
datev <- "2014-10-15"
# Subset prices in two different ways
pricev <- rutils::etfenv$prices
all.equal(pricev[zoo::index(pricev) >= datev],
    pricev[paste0(datev, "/")])
# Boolean subsetting is slower because coercing string into date
library(microbenchmark)
summary(microbenchmark(
  boolean=(pricev[zoo::index(pricev) >= datev]),
  date=(pricev[paste0(datev, "/")]),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Coerce string into a date
datev <- as.Date("2014-10-15")
# Boolean subsetting is faster than using date string
summary(microbenchmark(
  boolean=(pricev[zoo::index(pricev) >= datev]),
  date=(pricev[paste0(datev, "/")]),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

pricev <- HighFreq::SPY["2012-04"]
# Subset recurring time interval using "T notation",
pricev <- pricev["T10:30:00/T15:00:00"]
first(pricev["2012-04-16"])  # First element of day
last(pricev["2012-04-16"])  # Last element of day
# Suppress timezone warning messages
options(xts_check_tz=FALSE)

# Create time series with overlapping time indices
vti1 <- rutils::etfenv$VTI["/2015"]
vti2 <- rutils::etfenv$VTI["2014/"]
dates1 <- zoo::index(vti1)
dates2 <- zoo::index(vti2)
# Join by rows
vti <- rbind(vti1, vti2)
datev <- zoo::index(vti)
sum(duplicated(datev))
vti <- vti[!duplicated(datev), ]
all.equal(vti, rutils::etfenv$VTI)
# Alternative method - slightly slower
vti <- rbind(vti1, vti2[!(zoo::index(vti2) %in% zoo::index(vti1))])
all.equal(vti, rutils::etfenv$VTI)
# Remove duplicates starting from the end
vti <- rbind(vti1, vti2)
vti <- vti[!duplicated(datev), ]
vtifl <- vti[!duplicated(datev, fromLast=TRUE), ]
all.equal(vti, vtifl)

pricev <- rutils::etfenv$prices[, c("VTI", "IEF")]
pricev <- na.omit(pricev)
str(pricev)  # Display structure of xts
# Subsetting zoo to single column drops dim attribute
pricezoo <- as.zoo(pricev)
dim(pricezoo)
dim(pricezoo[, 1])
# zoo with single column are vectors not matrices
c(is.matrix(pricezoo), is.matrix(pricezoo[, 1]))
# xts always have a dim attribute
rbind(base=dim(pricev), subs=dim(pricev[, 1]))
c(is.matrix(pricev), is.matrix(pricev[, 1]))

# Lag of zoo shortens it by one row
rbind(base=dim(pricezoo), lag=dim(lag(pricezoo)))
# Lag of xts doesn't shorten it
rbind(base=dim(pricev), lag=dim(lag(pricev)))
# Lag of zoo is in opposite direction from xts
head(lag(pricezoo, -1), 4)
head(lag(pricev), 4)

# library(rutils)  # Load package rutils
# Indices of last observations in each hour
endd <- xts::endpoints(pricev, on="hours")
head(endd)
# Extract the last observations in each hour
head(pricev[endd, ])

# Lower the periodicity to months
pricem <- to.period(x=pricev, period="months", name="MSFT")
# Convert colnames to standard OHLC format
colnames(pricem)
colnames(pricem) <- sapply(
  strsplit(colnames(pricem), split=".", fixed=TRUE),
  function(namev) namev[-1]
  )  # end sapply
head(pricem, 3)
# Lower the periodicity to years
pricey <- to.period(x=pricem, period="years", name="MSFT")
colnames(pricey) <- sapply(
  strsplit(colnames(pricey), split=".", fixed=TRUE),
  function(namev) namev[-1]
  )  # end sapply
head(pricey)

load(file="/Users/jerzy/Develop/lecture_slides/data/zoo_data.RData")
library(quantmod)  # Load package quantmod
# as.xts() coerces zoo series into xts series
class(zoo_stx)
pricexts <- as.xts(zoo_stx)
dim(pricexts)
head(pricexts[, 1:4], 4)
# OHLC candlechart
plotheme <- chart_theme()
plotheme$col$up.col <- c("green")
plotheme$col$dn.col <- c("red")
chart_Series(x=pricexts["2016-05/2016-06", 1:4], theme=plotheme,
  name="Candlestick Plot of OHLC Stock Prices")

library(dygraphs)
# Create dygraphs object
dyplot <- dygraphs::dygraph(pricexts["2016-05/2016-06", 1:4])
# Convert dygraphs object to candlestick plot
dyplot <- dygraphs::dyCandlestick(dyplot)
# Render candlestick plot
dyplot
# Candlestick plot using pipes syntax
dygraphs::dygraph(pricexts["2016-05/2016-06", 1:4]) %>%
  dyCandlestick() %>%
  dyOptions(colors="red", strokeWidth=3)
# Candlestick plot without using pipes syntax
dygraphs::dyCandlestick(dygraphs::dyOptions(
  dygraphs::dygraph(pricexts["2016-05/2016-06", 1:4]),
  colors="red", strokeWidth=3))

# Create zoo time series
datev <- seq(from=as.Date("2014-07-14"), by="day", length.out=10)
timeser <- zoo(x=sample(10), order.by=datev)
class(timeser)
timeser
library(xts)
# Coerce zoo time series to class xts
pricexts <- as.xts(timeser)
class(xtseries)
xtseries
