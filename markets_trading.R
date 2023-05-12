# Symbols for constant maturity Treasury rates
symbolv <- c("DGS1", "DGS2", "DGS5", "DGS10", "DGS20", "DGS30")
# Create new environment for time series
ratesenv <- new.env()
# Download time series for symbolv into ratesenv
quantmod::getSymbols(symbolv, env=ratesenv, src="FRED")
# List files in ratesenv
ls(ratesenv)
# Get class of all objects in ratesenv
sapply(ratesenv, class)
# Get class of all objects in R workspace
sapply(ls(), function(name) class(get(name)))
# Save the time series environment into a binary .RData file
save(ratesenv, file="/Users/jerzy/Develop/lecture_slides/data/rates_data.RData")
# Get class of time series object DGS10
class(get(x="DGS10", envir=ratesenv))
# Another way
class(ratesenv$DGS10)
# Get first 6 rows of time series
head(ratesenv$DGS10)
# Plot dygraphs of 10-year Treasury rate
dygraphs::dygraph(ratesenv$DGS10, main="10-year Treasury Rate") %>%
  dyOptions(colors="blue", strokeWidth=2)
# Plot 10-year constant maturity Treasury rate
x11(width=6, height=5)
par(mar=c(2, 2, 0, 0), oma=c(0, 0, 0, 0))
chart_Series(ratesenv$DGS10["1990/"], name="10-year Treasury Rate")
# Load constant maturity Treasury rates
load(file="/Users/jerzy/Develop/lecture_slides/data/rates_data.RData")
# Get most recent yield curve
ycnow <- eapply(ratesenv, xts::last)
class(ycnow)
ycnow <- do.call(cbind, ycnow)
# Check if 2020-03-25 is not a holiday
date2020 <- as.Date("2020-03-25")
weekdays(date2020)
# Get yield curve from 2020-03-25
yc2020 <- eapply(ratesenv, function(x) x[date2020])
yc2020 <- do.call(cbind, yc2020)
# Combine the yield curves
ycurves <- c(yc2020, ycnow)
# Rename columns and rows, sort columns, and transpose into matrix
colnames(ycurves) <- substr(colnames(ycurves), start=4, stop=11)
ycurves <- ycurves[, order(as.numeric(colnames(ycurves)))]
colnames(ycurves) <- paste0(colnames(ycurves), "yr")
ycurves <- t(ycurves)
colnames(ycurves) <- substr(colnames(ycurves), start=1, stop=4)
x11(width=6, height=5)
par(mar=c(3, 3, 2, 0), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
# Plot using matplot()
colorv <- c("blue", "red")
matplot(ycurves, main="Yield Curves in 2020 and 2023", xaxt="n", lwd=3, lty=1,
  type="l", xlab="maturity", ylab="yield", col=colorv)
# Add x-axis
axis(1, seq_along(rownames(ycurves)), rownames(ycurves))
# Add legend
legend("topleft", legend=colnames(ycurves), y.intersp=0.1,
 bty="n", col=colorv, lty=1, lwd=6, inset=0.05, cex=1.0)
x11(width=6, height=5)
par(mar=c(3, 3, 2, 0), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
# Load constant maturity Treasury rates
load(file="/Users/jerzy/Develop/lecture_slides/data/rates_data.RData")
# Get end-of-year dates since 2006
datev <- xts::endpoints(ratesenv$DGS1["2006/"], on="years")
datev <- zoo::index(ratesenv$DGS1["2006/"][dates])
# Create time series of end-of-year rates
ycurves <- eapply(ratesenv, function(ratev) ratev[dates])
ycurves <- rutils::do_call(cbind, ycurves)
# Rename columns and rows, sort columns, and transpose into matrix
colnames(ycurves) <- substr(colnames(ycurves), start=4, stop=11)
ycurves <- ycurves[, order(as.numeric(colnames(ycurves)))]
colnames(ycurves) <- paste0(colnames(ycurves), "yr")
ycurves <- t(ycurves)
colnames(ycurves) <- substr(colnames(ycurves), start=1, stop=4)
# Plot matrix using plot.zoo()
colorv <- colorRampPalette(c("red", "blue"))(NCOL(ycurves))
plot.zoo(ycurves, main="Yield curve since 2006", lwd=3, xaxt="n",
   plot.type="single", xlab="maturity", ylab="yield", col=colorv)
# Add x-axis
axis(1, seq_along(rownames(ycurves)), rownames(ycurves))
# Add legend
legend("topleft", legend=colnames(ycurves), y.intersp=0.1,
 bty="n", col=colorv, lty=1, lwd=4, inset=0.05, cex=0.8)
# Alternative plot using matplot()
matplot(ycurves, main="Yield curve since 2006", xaxt="n", lwd=3, lty=1,
  type="l", xlab="maturity", ylab="yield", col=colorv)
# Add x-axis
axis(1, seq_along(rownames(ycurves)), rownames(ycurves))
# Add legend
legend("topleft", legend=colnames(ycurves), y.intersp=0.1,
 bty="n", col=colorv, lty=1, lwd=4, inset=0.05, cex=0.8)
# Extract rates from ratesenv
symbolv <- c("DGS1", "DGS2", "DGS5", "DGS10", "DGS20")
ratem <- mget(symbolv, envir=ratesenv)
ratem <- rutils::do_call(cbind, ratem)
ratem <- zoo::na.locf(ratem, na.rm=FALSE)
ratem <- zoo::na.locf(ratem, fromLast=TRUE)
# Calculate daily percentage rates changes
retp <- rutils::diffit(log(ratem))
# De-mean the returns
retp <- lapply(retp, function(x) {x - mean(x)})
retp <- rutils::do_call(cbind, retp)
sapply(retp, mean)
# Covariance and Correlation matrices of Treasury rates
covmat <- cov(retp)
cormat <- cor(retp)
# Reorder correlation matrix based on clusters
library(corrplot)
ordern <- corrMatOrder(cormat, order="hclust",
  hclust.method="complete")
cormat <- cormat[ordern, ordern]
# Plot the correlation matrix
x11(width=6, height=6)
colorv <- colorRampPalette(c("red", "white", "blue"))
corrplot(cormat, title=NA, tl.col="black",
    method="square", col=colorv(NCOL(cormat)), tl.cex=0.8,
    cl.offset=0.75, cl.cex=0.7, cl.align.text="l", cl.ratio=0.25)
title("Correlation of Treasury Rates", line=1)
# Draw rectangles on the correlation matrix plot
corrRect.hclust(cormat, k=NROW(cormat) %/% 2,
  method="complete", col="red")
# Create initial vector of portfolio weights
nweights <- NROW(symbolv)
weightv <- rep(1/sqrt(nweights), nweights)
names(weightv) <- symbolv
# Objective function equal to minus portfolio variance
objfun <- function(weightv, retp) {
  retp <- retp %*% weightv
  -1e7*var(retp) + 1e7*(1 - sum(weightv*weightv))^2
}  # end objfun
# Objective function for equal weight portfolio
objfun(weightv, retp)
# Compare speed of vector multiplication methods
library(microbenchmark)
summary(microbenchmark(
  transp=t(retp) %*% retp,
  sumv=sum(retp*retp),
  times=10))[, c(1, 4, 5)]
# Find weights with maximum variance
optiml <- optim(par=weightv,
  fn=objfun,
  retp=retp,
  method="L-BFGS-B",
  upper=rep(5.0, nweights),
  lower=rep(-5.0, nweights))
# Optimal weights and maximum variance
weights1 <- optiml$par
objfun(weights1, retp)
# Plot first principal component loadings
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
barplot(weights1, names.arg=names(weights1),
  xlab="", ylab="", main="First Principal Component Loadings")
# pc1 weights and returns
pc1 <- drop(retp %*% weights1)
# Redefine objective function
objfun <- function(weightv, retp) {
  retp <- retp %*% weightv
  -1e7*var(retp) + 1e7*(1 - sum(weightv^2))^2 +
    1e7*sum(weights1*weightv)^2
}  # end objfun
# Find second principal component weights
optiml <- optim(par=weightv,
             fn=objfun,
             retp=retp,
             method="L-BFGS-B",
             upper=rep(5.0, nweights),
             lower=rep(-5.0, nweights))
# pc2 weights and returns
weights2 <- optiml$par
pc2 <- drop(retp %*% weights2)
sum(pc1*pc2)
# Plot second principal component loadings
barplot(weights2, names.arg=names(weights2),
  xlab="", ylab="", main="Second Principal Component Loadings")
eigend <- eigen(covmat)
eigend$vectors
# Compare with optimization
all.equal(sum(diag(covmat)), sum(eigend$values))
all.equal(abs(eigend$vectors[, 1]), abs(weights1), check.attributes=FALSE)
all.equal(abs(eigend$vectors[, 2]), abs(weights2), check.attributes=FALSE)
all.equal(eigend$values[1], var(pc1), check.attributes=FALSE)
all.equal(eigend$values[2], var(pc2), check.attributes=FALSE)
# Eigenvalue equations are satisfied approximately
(covmat %*% weights1) / weights1 / var(pc1)
(covmat %*% weights2) / weights2 / var(pc2)
# Plot eigenvalues
barplot(eigend$values, names.arg=paste0("PC", 1:nweights),
  las=3, xlab="", ylab="", main="Principal Component Variances")
# Eigen decomposition of correlation matrix
eigend <- eigen(cormat)
# Perform PCA with scaling
pcad <- prcomp(retp, scale=TRUE)
# Compare outputs
all.equal(eigend$values, pcad$sdev^2)
all.equal(abs(eigend$vectors), abs(pcad$rotation),
    check.attributes=FALSE)
# Eigen decomposition of covariance matrix
eigend <- eigen(covmat)
# Perform PCA without scaling
pcad <- prcomp(retp, scale=FALSE)
# Compare outputs
all.equal(eigend$values, pcad$sdev^2)
all.equal(abs(eigend$vectors), abs(pcad$rotation),
    check.attributes=FALSE)
# Perform principal component analysis PCA
pcad <- prcomp(retp, scale=TRUE)
# Plot standard deviations
barplot(pcad$sdev, names.arg=colnames(pcad$rotation),
  las=3, xlab="", ylab="",
  main="Scree Plot: Volatilities of Principal Components
  of Treasury rates")
x11(width=6, height=7)
# Calculate principal component loadings (weights)
pcad$rotation
# Plot loading barplots in multiple panels
par(mfrow=c(3,2))
par(mar=c(3.5, 2, 2, 1), oma=c(0, 0, 0, 0))
for (ordern in 1:NCOL(pcad$rotation)) {
  barplot(pcad$rotation[, ordern], las=3, xlab="", ylab="", main="")
  title(paste0("PC", ordern), line=-2.0, col.main="red")
}  # end for
# Standardize (de-mean and scale) the returns
retp <- lapply(retp, function(x) {(x - mean(x))/sd(x)})
retp <- rutils::do_call(cbind, retp)
sapply(retp, mean)
sapply(retp, sd)
# Calculate principal component time series
pcacum <- retp %*% pcad$rotation
all.equal(pcad$x, pcacum, check.attributes=FALSE)
# Calculate products of principal component time series
round(t(pcacum) %*% pcacum, 2)
# Coerce to xts time series
pcacum <- xts(pcacum, order.by=zoo::index(retp))
pcacum <- cumsum(pcacum)
# Plot principal component time series in multiple panels
par(mfrow=c(3,2))
par(mar=c(2, 2, 0, 1), oma=c(0, 0, 0, 0))
rangev <- range(pcacum)
for (ordern in 1:NCOL(pcacum)) {
  plot.zoo(pcacum[, ordern], ylim=rangev, xlab="", ylab="")
  title(paste0("PC", ordern), line=-1, col.main="red")
}  # end for
# Invert all the principal component time series
retpca <- retp %*% pcad$rotation
solved <- retpca %*% solve(pcad$rotation)
all.equal(coredata(retp), solved)
# Invert first 3 principal component time series
solved <- retpca[, 1:3] %*% solve(pcad$rotation)[1:3, ]
solved <- xts::xts(solved, zoo::index(retp))
solved <- cumsum(solved)
retc <- cumsum(retp)
# Plot the solved returns
par(mfrow=c(3,2))
par(mar=c(2, 2, 0, 1), oma=c(0, 0, 0, 0))
for (symbol in symbolv) {
  plot.zoo(cbind(retc[, symbol], solved[, symbol]),
    plot.type="single", col=c("black", "blue"), xlab="", ylab="")
  legend(x="topleft", bty="n", y.intersp=0.1,
   legend=paste0(symbol, c("", " solved")),
   title=NULL, inset=0.0, cex=1.0, lwd=6,
   lty=1, col=c("black", "blue"))
}  # end for
library(quantmod)  # Load quantmod
library(RQuantLib)  # Load RQuantLib
# Specify curve parameters
curve_params <- list(tradeDate=as.Date("2018-01-17"),
               settleDate=as.Date("2018-01-19"),
               dt=0.25,
               interpWhat="discount",
               interpHow="loglinear")
# Specify market data: prices of FI instruments
market_data <- list(d3m=0.0363,
              fut1=96.2875,
              fut2=96.7875,
              fut3=96.9875,
              fut4=96.6875,
              s5y=0.0443,
              s10y=0.05165,
              s15y=0.055175)
# Specify dates for calculating the zero rates
disc_dates <- seq(0, 10, 0.25)
# Specify the evaluation (as of) date
setEvaluationDate(as.Date("2018-01-17"))
# Calculate the zero rates
disc_curves <- DiscountCurve(params=curve_params,
                       tsQuotes=market_data,
                       times=disc_dates)
# Plot the zero rates
x11()
plot(x=disc_curves$zerorates, t="l", main="zerorates")
# Futures contracts codes
futures <- rbind(c("S&P500 index", "ES"),
              c("10yr Treasury", "ZN"),
              c("VIX index", "VX"),
              c("Gold", "GC"),
              c("Oil", "CL"),
              c("Euro FX", "EC"),
              c("Swiss franc", "SF"),
              c("Japanese Yen", "JY"))
colnames(futures) <- c("Futures contract", "Code")
print(xtable::xtable(futures), comment=FALSE, size="scriptsize", include.rownames=FALSE, latex.environments="flushleft")
# Monthly futures contract codes
codes <- cbind(c("January", "February", "September", "April", "May", "June", "July", "August", "September", "October", "November", "December"),
                     c("F", "G", "H", "J", "K", "M", "N", "Q", "U", "V", "X", "Z"))
colnames(codes) <- c("Month", "Code")
print(xtable::xtable(codes), comment=FALSE, size="scriptsize", include.rownames=FALSE, latex.environments="flushright")
# Futures contracts codes
futures <- rbind(c("S&P500 index", "SP", "ES"),
              c("10yr Treasury", "ZN", "ZN"),
              c("VIX index", "VX", "delisted"),
              c("Gold", "GC", "YG"),
              c("Oil", "CL", "QM"),
              c("Euro FX", "EC", "E7"),
              c("Swiss franc", "SF", "MSF"),
              c("Japanese Yen", "JY", "J7"))
colnames(futures) <- c("Futures contract", "Standard", "E-mini")
print(xtable::xtable(futures), comment=FALSE, size="scriptsize", include.rownames=FALSE, latex.environments="flushleft")
# Load data for S&P Emini futures June 2019 contract
dir_name <- "/Users/jerzy/Develop/data/ib_data"
file_name <- file.path(dir_name, "ESohlc.csv")
# Read a data table from CSV file
pricev <- data.table::fread(file_name)
class(pricev)
# Coerce first column from string to date-time
unlist(sapply(pricev, class))
tail(pricev)
prices$Index <- as.POSIXct(prices$Index,
  tz="America/New_York", origin="1970-01-01")
# Coerce prices into xts series
pricev <- data.table::as.xts.data.table(pricev)
class(pricev)
tail(pricev)
colnames(pricev)[1:5] <- c("Open", "High", "Low", "Close", "Volume")
tail(pricev)
# Plot OHLC data in x11 window
x11(width=5, height=4)  # Open x11 for plotting
par(mar=c(5, 5, 2, 1), oma=c(0, 0, 0, 0))
chart_Series(x=pricev, TA="add_Vo()",
  name="S&P500 futures")
# Plot dygraph
dygraphs::dygraph(pricev[, 1:4], main="OHLC prices") %>%
  dyCandlestick()
# Load ESU8 data
dir_name <- "/Users/jerzy/Develop/data/ib_data"
file_name <- file.path(dir_name, "ESU8.csv")
ESU8 <- data.table::fread(file_name)
# Coerce ESU8 into xts series
ESU8$V1 <- as.Date(as.POSIXct.numeric(ESU8$V1,
    tz="America/New_York", origin="1970-01-01"))
ESU8 <- data.table::as.xts.data.table(ESU8)
colnames(ESU8)[1:5] <- c("Open", "High", "Low", "Close", "Volume")
# Load ESM8 data
file_name <- file.path(dir_name, "ESM8.csv")
ESM8 <- data.table::fread(file_name)
# Coerce ESM8 into xts series
ESM8$V1 <- as.Date(as.POSIXct.numeric(ESM8$V1,
    tz="America/New_York", origin="1970-01-01"))
ESM8 <- data.table::as.xts.data.table(ESM8)
colnames(ESM8)[1:5] <- c("Open", "High", "Low", "Close", "Volume")
x11(width=6, height=5)  # Open x11 for plotting
# Plot last month of ESU8 and ESM8 volume data
endd <- end(ESM8)
startd <- (endd - 30)
volumm <- cbind(Vo(ESU8), Vo(ESM8))[paste0(startd, "/", endd)]
colnames(volumm) <- c("ESU8", "ESM8")
colorv <- c("blue", "green")
plot(volumm, col=colorv, lwd=3, major.ticks="days",
     format.labels="%b-%d", observation.based=TRUE,
     main="Volumes of ESU8 and ESM8 futures")
legend("topleft", legend=colnames(volumm), col=colorv, y.intersp=0.1,
 title=NULL, bty="n", lty=1, lwd=6, inset=0.1, cex=0.7)
# Find date when ESU8 volume exceeds ESM8
exceeds <- (volumm[, "ESU8"] > volumm[, "ESM8"])
indeks <- match(TRUE, exceeds)
# indeks <- min(which(exceeds))
# Scale the ESM8 prices
indeks <- zoo::index(exceeds[indeks])
ratio <- as.numeric(Cl(ESU8[indeks])/Cl(ESM8[indeks]))
ESM8[, 1:4] <- ratio*ESM8[, 1:4]
# Calculate continuous contract prices
chain_ed <- rbind(ESM8[zoo::index(ESM8) < indeks],
            ESU8[zoo::index(ESU8) >= indeks])
# Or
# Chain_ed <- rbind(ESM8[paste0("/", indeks-1)],
#                   ESU8[paste0(indeks, "/")])
# Plot continuous contract prices
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
load(file="/Users/jerzy/Develop/data/ib_data/vix_cboe.RData")
ls(vixenv)
vixenv$vix_index <- vix_index
save(vixenv, file="/Users/jerzy/Develop/data/ib_data/vix_cboe.RData")
# Plot VIX OHLC data in x11 window
chart_Series(x=vix_index["2018"], name="VIX Index")
# Plot dygraph
dygraphs::dygraph(vix_index, main="VIX Index") %>%
  dyCandlestick()
# Read CBOE monthly futures expiration dates
datev <- read.csv(file="/Users/jerzy/Develop/data/vix_data/vix_dates.csv")
datev <- as.Date(datev[, 1])
yearv <- format(datev, format="%Y")
yearv <- substring(yearv, 4)
# Monthly futures contract codes
codes <-
  c("F", "G", "H", "J", "K", "M",
    "N", "Q", "U", "V", "X", "Z")
symbolv <- paste0("VX", codes, yearv)
datev <- as.data.frame(datev)
colnames(datev) <- "exp_dates"
rownames(datev) <- symbolv
# Write dates to CSV file, with row names
write.csv(datev, row.names=TRUE,
  file="/Users/jerzy/Develop/data/vix_data/vix_futures.csv")
# Read back CBOE futures expiration dates
datev <- read.csv(file="/Users/jerzy/Develop/data/vix_data/vix_futures.csv",
  row.names=1)
datev[, 1] <- as.Date(datev[, 1])
# Load VIX futures data from binary file
load(file="/Users/jerzy/Develop/data/vix_data/vix_cboe.RData")
# Get all VIX futures for 2018 except January
symbolv <- ls(vixenv)
symbolv <- symbolv[grep(glob2rx("*8"), symbolv)]
symbolv <- symbolv[2:9]
# Specify dates for curves
startd <- as.Date("2018-01-11")
endd <- as.Date("2018-02-05")
# Extract all VIX futures prices on the dates
futcurves <- lapply(symbolv, function(symbol) {
  xtsv <- get(x=symbol, envir=vixenv)
  Cl(xtsv[c(startd, endd)])
})  # end lapply
futcurves <- rutils::do_call(cbind, futcurves)
colnames(futcurves) <- symbolv
futcurves <- t(coredata(futcurves))
colnames(futcurves) <- c("Contango 01/11/2018",
                 "Backwardation 02/05/2018")
x11(width=7, height=5)
par(mar=c(3, 2, 1, 1), oma=c(0, 0, 0, 0))
plot(futcurves[, 1], type="l", lty=1, col="blue", lwd=3,
     xaxt="n", xlab="", ylab="", ylim=range(futcurves),
     main="VIX Futures Curves")
axis(1, at=(1:NROW(futcurves)), labels=rownames(futcurves))
lines(futcurves[, 2], lty=1, lwd=3, col="red")
legend(x="topright", legend=colnames(futcurves),
 inset=0.05, cex=1.0, bty="n", y.intersp=0.1,
 col=c("blue", "red"), lwd=6, lty=1)
# Load VIX futures data from binary file
load(file="/Users/jerzy/Develop/data/vix_data/vix_cboe.RData")
# Read CBOE futures expiration dates
datev <- read.csv(file="/Users/jerzy/Develop/data/vix_data/vix_futures.csv",
  row.names=1)
symbolv <- rownames(datev)
datev <- as.Date(datev[, 1])
tday <- as.Date("2018-05-07")
maturd <- (tday + 30)
# Find neighboring futures contracts
indeks <- match(TRUE, dates > maturd)
frontd <- datev[indeks-1]
backd <- datev[indeks]
symbolf <- symbolv[indeks-1]
symbolb <- symbolv[indeks]
pricef <- get(x=symbolf, envir=vixenv)
pricef <- as.numeric(Cl(pricef[tday]))
priceb <- get(x=symbolb, envir=vixenv)
priceb <- as.numeric(Cl(priceb[tday]))
# Calculate the constant maturity 30-day futures price
ratiop <- as.numeric(maturd - frontd)/as.numeric(backd - frontd)
pricec <- (ratiop*priceb + (1-ratiop)*pricef)
x11(width=5, height=3)  # Open x11 for plotting
# Load VIX futures data from binary file
load(file="/Users/jerzy/Develop/data/vix_data/vix_cboe.RData")
# Plot VIX and SVXY data in x11 window
plot_theme <- chart_theme()
plot_theme$col$line.col <- "blue"
chart_Series(x=Cl(vixenv$vix_index["2007/"]),
       theme=plot_theme, name="VIX Index")
chart_Series(x=Cl(rutils::etfenv$VTI["2007/"]),
       theme=plot_theme, name="VTI ETF")
chart_Series(x=Cl(vixenv$vix_index["2017/2018"]),
       theme=plot_theme, name="VIX Index")
chart_Series(x=Cl(rutils::etfenv$SVXY["2017/2018"]),
       theme=plot_theme, name="SVXY ETF")
library(xtable)
# Read table of fundamental data into data frame
fundamental_data <-
  read.csv(file="/Users/jerzy/Develop/lecture_slides/data/fundamental_stock_data.csv")
print(xtable(fundamental_data), comment=FALSE, size="scriptsize", include.rownames=FALSE)
library(xtable)
# Read table of fundamental data into data frame
fundamental_data <-
  read.csv(file="/Users/jerzy/Develop/lecture_slides/data/fundamental_stock_data.csv")
print(xtable(fundamental_data), comment=FALSE, size="scriptsize", include.rownames=FALSE)
# Install package "rwrds"
devtools::install_github("davidsovich/rwrds")
# Load package "rwrds"
library(rwrds)
# Get documentation for package "rwrds"
packageDescription("rwrds")
# Load help page
help(package="rwrds")
# List all datasets in "rwrds"
data(package="rwrds")
# List all objects in "rwrds"
ls("package:rwrds")
# Remove rwrds from search path
detach("package:rwrds")
library(rwrds)
library(dplyr)
# Establish connection to WRDS
wrds_con <- rwrds::wrds_connect(username="jp3900", password="RipvanWinkle20")
# Download Compustat names table as dplyr object
namesv_table <- rwrds::compustat_names(wrds=wrds_con, subset=FALSE, dl=TRUE)
dim(namesv_table)
# Save names table as csv file
write.csv(namesv_table, file="/Users/jerzy/Develop/lecture_slides/data/compustat_table.csv", row.names=FALSE)
# rm(namesv_table)
# Read names table from csv file
namesv_table <- read.csv(file="/Users/jerzy/Develop/lecture_slides/data/compustat_table.csv")
# symbol <- "VTI"
# match(symbol, namesv_table$tic)
# Create ETF symbols (tickers)
symbolv <- c("VTI", "VEU", "EEM")
# Get cusips of symbolv
indeks <- match(symbolv, namesv_table$tic)
names(indeks) <- symbolv
etf_cusips <- namesv_table$cusip[indeks]
names(etf_cusips) <- symbolv
# Save cusips into text file
cat(etf_cusips, file="/Users/jerzy/Develop/lecture_slides/data/etf_cusips.txt", sep="\n")
# Save gvkeys into text file
etf_gvkeys <- namesv_table$gvkey[indeks]
names(etf_gvkeys) <- symbolv
cat(etf_gvkeys, file="/Users/jerzy/Develop/lecture_slides/data/etf_gvkeys.txt", sep="\n")
# Read .csv file with S&P500 constituents
sp500table <- read.csv(file="/Users/jerzy/Develop/lecture_slides/data/sp500_constituents.csv")
class(sp500table)
dim(sp500table)
head(sp500table)
# Select unique sp500 tickers and save them into text file
sp500_tickers <- unique(sp500table$co_tic)
cat(sp500_tickers, file="/Users/jerzy/Develop/lecture_slides/data/sp500_tickers.txt", sep="\n")
# Some gvkeys are duplicates
duplicates <- table(sp500table$gvkey)
duplicates <- duplicates[duplicates > 1]
duplicates <- sp500table[match(as.numeric(names(duplicates)), sp500table$gvkey), ]
# Select unique gvkeys
sp500_gvkeys <- unique(sp500table$gvkey)
# foo <- sp500table[match(sp500_gvkeys, sp500table$gvkey), ]
# Save gvkeys into text file
cat(sp500_gvkeys, file="/Users/jerzy/Develop/lecture_slides/data/sp500_gvkeys.txt", sep="\n")
# Select unique cusips and save into text file
sp500_cusips <- unique(sp500table$co_cusip)
# Remove empty cusips
which(sp500_cusips == "")
sp500_cusips <- sp500_cusips[-which(sp500_cusips == "")]
cat(sp500_cusips, file="/Users/jerzy/Develop/lecture_slides/data/sp500_cusips.txt", sep="\n")
# Find the rows corresponding to the sp500_cusips
rows_cusips <- sp500table[match(sp500_cusips, sp500table$co_cusip), ]
# Find the rows corresponding to duplicate gvkeys
duplicates <- table(rows_cusips$gvkey)
duplicates <- duplicates[duplicates > 1]
duplicates <- rows_cusips[rows_cusips$gvkey %in% as.numeric(names(duplicates)), ]
# Read .csv file with S&P500 constituents
sp500table <- read.csv(file="/Users/jerzy/Develop/lecture_slides/data/sp500_constituents.csv")
class(sp500table)
dim(sp500table)
head(sp500table)
library(rutils)  # Load package rutils
# Read .csv file with TAP OHLC prices
ohlc <- read.csv(file="/Users/jerzy/Develop/lecture_slides/data/TAP.csv")
# ohlc contains cusips not in sp500_cusips
cusips <- unique(ohlc$cusip)
cusips %in% sp500_cusips
# Select data only for sp500_cusips
ohlc <- ohlc[ohlc$cusip %in% sp500_cusips, ]
# ohlc contains tickers not in sp500_tickers
tickers <- unique(ohlc$tic)
tickers %in% sp500_tickers
# Select data only for sp500_tickers
ohlc <- ohlc[ohlc$tic %in% sp500_tickers, ]
# Select ticker from sp500table
symbol <- sp500table$co_tic[match(ohlc$gvkey[1], sp500table$gvkey)]
# Adjustment factor and total return factor
factadj <- drop(ohlc[, "ajexdi"])
factr <- drop(ohlc[, "trfd"])
# Extract index of dates
datev <- drop(ohlc[, "datadate"])
datev <- lubridate::ymd(datev)
# Select only OHLCV data columns
ohlc <- ohlc[, c("prcod", "prchd", "prcld", "prccd", "cshtrd")]
colnames(ohlc) <- paste(symbol, c("Open", "High", "Low", "Close", "Volume"), sep=".")
# Coerce to xts series
ohlc <- xts::xts(ohlc, datev)
# Fill the missing (NA) Open prices
isna <- is.na(ohlc[, 1])
ohlc[isna, 1] <- (ohlc[isna, 2] + ohlc[isna, 3])/2
sum(is.na(ohlc))
# Adjust all the prices
ohlc[, 1:4] <- factr*ohlc[, 1:4]/factadj/factr[NROW(factr)]
ohlc <- na.omit(ohlc)
plot(quantmod::Cl(ohlc), main="TAP Stock")
# Define formatting function for OHLC prices
format_ohlc <- function(ohlc, environ_ment) {
  # Select ticker from sp500table
  symbol <- sp500table$co_tic[match(ohlc$gvkey[1], sp500table$gvkey)]
  # Split adjustment and total return factors
  factadj <- drop(ohlc[, c("ajexdi")])
  factr <- drop(ohlc[, "trfd"])
  factr <- ifelse(is.na(factr), 1, factr)
  # Extract dates index
  datev <- drop(ohlc[, "datadate"])
  datev <- lubridate::ymd(datev)
  # Select only OHLCV data
  ohlc <- ohlc[, c("prcod", "prchd", "prcld", "prccd", "cshtrd")]
  colnames(ohlc) <- paste(symbol, c("Open", "High", "Low", "Close", "Volume"), sep=".")
  # Coerce to xts series
  ohlc <- xts::xts(ohlc, datev)
  # Fill NA prices
  isna <- is.na(ohlc[, 1])
  ohlc[isna, 1] <- (ohlc[isna, 2] + ohlc[isna, 3])/2
  # Adjust the prices
  ohlc[, 1:4] <- factr*ohlc[, 1:4]/factadj/factr[NROW(factr)]
  # Copy the OHLCV data to environ_ment
  ohlc <- na.omit(ohlc)
  assign(x=symbol, value=ohlc, envir=environ_ment)
  symbol
}  # end format_ohlc
# Load OHLC prices from .csv file downloaded from WRDS by cusip
sp500_pricev <- read.csv(file="/Users/jerzy/Develop/lecture_slides/data/sp500_prices_bycusip.csv")
# sp500_prices contains cusips not in sp500_cusips
cusips <- unique(sp500_prices$cusip)
NROW(sp500_cusips); NROW(cusips)
# Select data only for sp500_cusips
sp500_pricev <- sp500_pricev[sp500_prices$cusip %in% sp500_cusips, ]
# sp500_prices contains tickers not in sp500_tickers
tickers <- unique(sp500_prices$tic)
NROW(sp500_tickers); NROW(tickers)
# Select data only for sp500_tickers
sp500_pricev <- sp500_pricev[sp500_prices$tic %in% sp500_tickers, ]
# Create new data environment
sp500env <- new.env()
# Perform OHLC aggregations by cusip column
sp500_pricev <- split(sp500_pricev, sp500_prices$cusip)
process_ed <- lapply(sp500_pricev, format_ohlc,
               environ_ment=sp500env)
# Get end dates of series in sp500env
endds <- eapply(sp500env, end)
endds <- unlist(endds)
endds <- as.Date(endds)
# Remove elements with short end dates
ishort <- (endds < max(endds))
rm(list=names(sp500env)[ishort], envir=sp500env)
# Rename element "BRK.B" to "BRKB"
sp500env$BRKB <- sp500env$BRK.B
rm(BRK.B, envir=sp500env)
names(sp500env$BRKB) <- paste("BRKB",
  c("Open", "High", "Low", "Close", "Volume"), sep=".")
# Rename element "LOW" to "LOWES"
sp500env$LOWES <- sp500env$LOW
rm(LOW, envir=sp500env)
names(sp500env$LOWES) <- paste("LOVES",
  c("Open", "High", "Low", "Close", "Volume"), sep=".")
# Rename element "BF.B" to "BFB"
sp500env$BFB <- sp500env$BF.B
rm(BF.B, envir=sp500env)
names(sp500env$BFB) <- paste("BFB",
  c("Open", "High", "Low", "Close", "Volume"), sep=".")
# Save OHLC prices to .RData file
save(sp500env, file="/Users/jerzy/Develop/lecture_slides/data/sp500.RData")
plot(quantmod::Cl(sp500env$MSFT))
library(Quandl)  # Load package Quandl
# Register Quandl API key
Quandl.api_key("pVJi9Nv3V8CD3Js5s7Qx")
# Quandl stock market data
# https://blog.quandl.com/stock-market-data-ultimate-guide-part-1
# https://blog.quandl.com/stock-market-data-the-ultimate-guide-part-2
# Download RAYMOND metadata
# https://www.quandl.com/data/RAYMOND-Raymond/documentation/metadata
# Download S&P500 Index constituents
# https://s3.amazonaws.com/static.quandl.com/tickers/SP500.csv
# Download AAPL gross profits from RAYMOND
profitaapl <- Quandl("RAYMOND/AAPL_GROSS_PROFIT_Q", type="xts")
chart_Series(profitaapl, name="AAPL gross profits")
# Download multiple time series
pricev <- Quandl(code=c("NSE/OIL", "WIKI/AAPL"),
   startd="2013-01-01", type="xts")
# Download datasets for AAPL
# https://www.quandl.com/api/v3/datasets/WIKI/AAPL.json
# Download metadata for AAPL
pricev <- Quandl(code=c("NSE/OIL", "WIKI/AAPL"),
   startd="2013-01-01", type="xts")
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
symbolv <- c("AAPL", "IBM", "MSFT")
# Not all the metrics are returned by Yahoo.
funda_mentals <- getQuote(paste(symbolv, sep="", collapse=";"), src="yahoo", what=met_rics)
viewFinancials(funda_mentals,  period="Q")
funda_mentals <- getFinancials("HPQ", src="yahoo", auto.assign=FALSE)
viewFinancials(funda_mentals)
library(rutils)  # Load package rutils
# Specify class for column "TICKER" so that "F" doesn't become FALSE
col_class <- "character"
names(col_class) <- "TICKER"
# Read .csv file with Ford OHLC prices
ohlc <- read.csv(file="/Users/jerzy/Develop/lecture_slides/data/F_CRSP.csv",
  colClasses=col_class)
symbol <- ohlc[1, "TICKER"]
# Adjustment factor
factadj <- drop(ohlc[, "CFACPR"])
# Extract dates index
datev <- drop(ohlc[, "date"])
datev <- lubridate::ymd(datev)
# Select only OHLCV data
ohlc <- ohlc[, c("OPENPRC", "ASKHI", "BIDLO", "PRC", "VOL")]
colnames(ohlc) <- paste(symbol, c("Open", "High", "Low", "Close", "Volume"), sep=".")
# Coerce to xts series
ohlc <- xts::xts(ohlc, datev)
# Fill missing Open NA prices
isna <- is.na(ohlc[, 1])
ohlc[isna, 1] <- (ohlc[isna, 2] + ohlc[isna, 3])/2
# Adjust all the prices
ohlc[, 1:4] <- ohlc[, 1:4]/factadj/factr[NROW(factr)]
plot(quantmod::Cl(ohlc), main="Ford Stock")
crsp_compustat <- read.csv(file="/Users/jerzy/Develop/lecture_slides/data/crsp_compustat_indices.csv")
colnames(crsp_compustat) <- crsp_compustat[1, ]
crsp_compustat <- crsp_compustat[-1, ]
print(xtable(crsp_compustat), comment=FALSE, size="tiny", include.rownames=FALSE)
library(rutils)  # Load package rutils
# Read .csv file with TAP OHLC prices
ohlc <- read.csv(file="/Users/jerzy/Develop/lecture_slides/data/TAP.csv")
symbol <- ohlc[1, "tic"]
# Adjustment factor and total return factor
factadj <- drop(ohlc[, "ajexdi"])
factr <- drop(ohlc[, "trfd"])
# Extract dates index
datev <- drop(ohlc[, "datadate"])
datev <- lubridate::ymd(datev)
# Select only OHLCV data
ohlc <- ohlc[, c("prcod", "prchd", "prcld", "prccd", "cshtrd")]
colnames(ohlc) <- paste(symbol, c("Open", "High", "Low", "Close", "Volume"), sep=".")
# Coerce to xts series
ohlc <- xts::xts(ohlc, datev)
# Fill missing Open NA prices
isna <- is.na(ohlc[, 1])
ohlc[isna, 1] <- (ohlc[isna, 2] + ohlc[isna, 3])/2
sum(is.na(ohlc))
# Adjust all the prices
ohlc[, 1:4] <- factr*ohlc[, 1:4]/factadj/factr[NROW(factr)]
plot(quantmod::Cl(ohlc), main="TAP Stock")
library(rutils)  # Load package rutils
# Download Fama-French factors from KFRENCH database
factors <- Quandl(code="KFRENCH/FACTORS_D",
  startd="2001-01-01", type="xts")
dim(factors)
head(factors)
tail(factors)
chart_Series(cumsum(factors["2001/", 1]/100),
  name="Fama-French factors")
options(width=200)
# Load package HighFreq
library(HighFreq)
# Or load the high frequency data file directly:
# symbolv <- load("/Users/jerzy/Develop/R/HighFreq/data/hf_data.RData")
head(HighFreq::SPY_TAQ)
head(HighFreq::SPY)
tail(HighFreq::SPY)
library(rutils)
# Read TAQ trade data from csv file
taq <- data.table::fread(file="/Users/jerzy/Develop/data/xlk_tick_trades2020_0316.csv")
# Inspect the TAQ data
taq
class(taq)
colnames(taq)
sapply(taq, class)
symbol <- taq$SYM_ROOT[1]
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
# Calculate the number of ticks per second
n_secs <- as.numeric(last(datev)) - as.numeric(first(datev))
NROW(taq)/(6.5*3600)
# Select TAQ data columns
taq <- taq[, .(price=PRICE, volume=SIZE)]
# Add date-time index
taq <- cbind(index=datev, taq)
# Coerce trade ticks to xts series
xtsv <- xts::xts(taq[, .(price, volume)], taq$index)
colnames(xtsv) <- paste(symbol, c("Close", "Volume"), sep=".")
save(xtsv, file="/Users/jerzy/Develop/data/xlk_tick_trades2020_0316.RData")
# save(xtsv, file="/Users/jerzy/Develop/data/xlk_tick_trades2020_0316.RData")
# Plot dygraph
dygraphs::dygraph(xtsv$XLK.Close,
  main="XLK Trade Ticks for 2020-03-16")
# Plot in x11 window
x11(width=6, height=5)
quantmod::chart_Series(x=xtsv$XLK.Close,
  name="XLK Trade Ticks for 2020-03-16")
# Select the large lots greater than 100
dim(taq)
big_ticks <- taq[taq$volume > 100]
dim(big_ticks)
# Number of large lot ticks per second
NROW(big_ticks)/(6.5*3600)
# Save trade ticks with large lots
data.table::fwrite(big_ticks, file="/Users/jerzy/Develop/data/xlk_tick_trades2020_0316_biglots.csv")
# Coerce trade prices to xts
xtsv <- xts::xts(big_ticks[, .(price, volume)], big_ticks$index)
colnames(xtsv) <- c("XLK.Close", "XLK.Volume")
# Plot dygraph of the large lots
dygraphs::dygraph(xtsv$XLK.Close,
  main="XLK Trade Ticks for 2020-03-16 (large lots only)")
# Plot the large lots
x11(width=6, height=5)
quantmod::chart_Series(x=xtsv$XLK.Close,
  name="XLK Trade Ticks for 2020-03-16 (large lots only)")
# Round time index to seconds
good_ticks[, zoo::index := as.POSIXct(round.POSIXt(index, "secs"))]
# Aggregate to OHLC by seconds
ohlc <- good_ticks[, .(open=first(price), high=max(price), low=min(price), close=last(price), volume=sum(volume)), by=index]
# Round time index to minutes
good_ticks[, zoo::index := as.POSIXct(round.POSIXt(index, "mins"))]
# Aggregate to OHLC by minutes
ohlc <- good_ticks[, .(open=first(price), high=max(price), low=min(price), close=last(price), volume=sum(volume)), by=index]
# Coerce OHLC prices to xts
xtsv <- xts::xts(ohlc[, -"index"], ohlc$index)
# Plot dygraph of the OHLC prices
dygraphs::dygraph(xtsv[, -5], main="XLK Trade Ticks for 2020-03-16 (OHLC)") %>%
  dyCandlestick()
# Plot the OHLC prices
x11(width=6, height=5)
quantmod::chart_Series(x=xtsv, TA="add_Vo()",
  name="XLK Trade Ticks for 2020-03-16 (OHLC)")
options(width=200)
# Load package HighFreq
library(HighFreq)
# Or load the high frequency data file directly:
# symbolv <- load("/Users/jerzy/Develop/R/HighFreq/data/hf_data.RData")
head(HighFreq::SPY_TAQ)
head(HighFreq::SPY)
tail(HighFreq::SPY)
# Load package HighFreq
library(HighFreq)
head(HighFreq::SPY)
# Load package HighFreq
library(HighFreq)
# Define symbol
symbol <- "SPY"
# Load OHLC data
output_dir <- "/Users/jerzy/Develop/data/hfreq/scrub/"
symbol <- load(file.path(output_dir, paste0(symbol, ".RData")))
interval <-"2013-11-11 09:30:00/2013-11-11 10:30:00"
chart_Series(SPY[interval], name=symbol)
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
# install package HighFreq from github
install.packages("devtools")
library(devtools)
install_github(repo="algoquant/HighFreq")
# Load package HighFreq
library(HighFreq)
# set data directories
data_dir <- "/Users/jerzy/Develop/data/hfreq/src/"
output_dir <- "/Users/jerzy/Develop/data/hfreq/scrub/"
# Define symbol
symbol <- "SPY"
# Load a single day of TAQ data
symbol <- load(file.path(data_dir, paste0(symbol, "/2014.05.02.", symbol, ".RData")))
# scrub, aggregate single day of TAQ data to OHLC
ohlc_data <- scrub_agg(taq_data=get(symbol))
# Aggregate TAQ data for symbol, save to file
save_scrub_agg(symbol,
         data_dir=data_dir,
         output_dir=output_dir,
         period="minutes")
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
  leg=c("t-distr", "normal"), y.intersp=0.1,
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
  leg=c("minutely", "hourly", "daily"), y.intersp=0.1,
  lwd=6, lty=1, col=c("blue", "green", "red"))
# Calculate rolling volatility of SPY returns
ret2013 <- retp["2013-11-11/2013-11-15"]
# Calculate rolling volatility
look_back <- 11
endd <- seq_along(ret2013)
startp <- c(rep_len(1, look_back),
  endd[1:(NROW(endd)-look_back)])
endd[endd < look_back] <- look_back
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
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("blue", "red")
chart_Series(vol_rolling, theme=plot_theme,
     name="Rolling Volatility with Overnight Spikes")
legend("topright", legend=colnames(vol_rolling),
  inset=0.1, bg="white", lty=1, lwd=6, y.intersp=0.1,
  col=plot_theme$col$line.col, bty="n")
pricev <- read.zoo(file="/Users/jerzy/Develop/lecture_slides/data/bid_ask_bounce.csv",
  header=TRUE, sep=",")
pricev <- as.xts(pricev)
x11(width=6, height=4)
par(mar=c(2, 2, 0, 0), oma=c(1, 1, 0, 0))
chart_Series(x=pricev, name="S&P500 Futures Bid-Ask Bounce")
# Volatility of SPY
sqrt(HighFreq::calcvar_ohlc(ohlc))
# Daily SPY volatility and volume
volatd <- sqrt(xts::apply.daily(ohlc, FUN=calcvar_ohlc))
colnames(volatd) <- ("SPY_volatility")
volumv <- quantmod::Vo(ohlc)
volumd <- xts::apply.daily(volumv, FUN=sum)
colnames(volumd) <- ("SPY_volume")
# Plot SPY volatility and volume
datav <- cbind(volatd, volumd)["2008/2009"]
colnamev <- colnames(datav)
dygraphs::dygraph(datav,
  main="SPY Daily Volatility and Trading Volume") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="red", strokeWidth=3) %>%
  dySeries(name=colnamev[2], axis="y2", col="blue", strokeWidth=3)
# Regress log of daily volume vs volatility
datav <- log(cbind(volumd, volatd))
colnamev <- colnames(datav)
dframe <- as.data.frame(datav)
formulav <- as.formula(paste(colnamev, collapse="~"))
model <- lm(formulav, data=dframe)
# Durbin-Watson test for autocorrelation of residuals
lmtest::dwtest(model)
# Regress diff log of daily volume vs volatility
dframe <- as.data.frame(rutils::diffit(datav))
model <- lm(formulav, data=dframe)
lmtest::dwtest(model)
summary(model)
plot(formulav, data=dframe, main="SPY Daily Trading Volume vs Volatility (log scale)")
abline(model, lwd=3, col="red")
mtext(paste("beta =", round(coef(model)[2], 3)), cex=1.2, lwd=3, side=2, las=2, adj=(-0.5), padj=(-7))
# 60 minutes of data in look_back interval
look_back <- 60
vol2013 <- volumv["2013"]
ret2013 <- retp["2013"]
# Define end points with beginning stub
nrows <- NROW(ret2013)
nagg <- nrows %/% look_back
endd <- nrows-look_back*nagg + (0:nagg)*look_back
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
model <- lm(formulav, data=dframe)
lmtest::dwtest(model)
summary(model)
plot(formulav, data=dframe,
     main="SPY Hourly Trading Volume vs Volatility (log scale)")
abline(model, lwd=3, col="red")
mtext(paste("beta =", round(coef(model)[2], 3)), cex=1.2, lwd=3, side=2, las=2, adj=(-0.5), padj=(-7))
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
legend("topright", inset=0.05, bty="n", y.intersp=0.1,
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
pacfv <- pacf(as.numeric(retp), lag=10,
     xlab="lag", ylab="partial autocorrelation", main="")
title("Partial Autocorrelations of Minutely SPY Returns", line=1)
# Plot the partial autocorrelations of scaled SPY returns
pacfs <- pacf(as.numeric(retsc), lag=10,
     xlab="lag", ylab="partial autocorrelation", main="")
title("Partial Autocorrelations of Scaled SPY Returns", line=1)
# Calculate the sums of partial autocorrelations
sum(pacfv$acf)
sum(pacfs$acf)
# Calculate market illiquidity
liquidv <- sqrt(volumd)/volatd
# Plot market illiquidity
x11(width=6, height=7) ; par(mfrow=c(2, 1))
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("blue")
chart_Series(liquidv["2010"], theme=plot_theme,
  name="SPY Liquidity in 2010", plot=FALSE)
plot_theme$col$line.col <- c("red")
chart_Series(volatd["2010"],
  theme=plot_theme, name="SPY Volatility in 2010")
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
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("blue")
chart_Series(volumagg[c(-1, -NROW(volumagg))], theme=plot_theme,
  name="Daily Seasonality of SPY Volume", plot=FALSE)
plot_theme$col$line.col <- c("red")
chart_Series(volagg[c(-1, -NROW(volagg))], theme=plot_theme,
  name="Daily Seasonality of SPY Volatility")
# Calculate market liquidity
liquidv <- sqrt(volumagg)/volagg
# Plot daily seasonality of market liquidity
x11(width=6, height=7) ; par(mfrow=c(2, 1))
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("blue")
chart_Series(liquidv[c(-1, -NROW(liquidv))], theme=plot_theme,
  name="Daily Seasonality of SPY Liquidity", plot=FALSE)
plot_theme$col$line.col <- c("red")
chart_Series(volagg[c(-1, -NROW(volagg))], theme=plot_theme,
  name="Daily Seasonality of SPY Volatility")
par(mfrow=c(2,1))  # set plot panels
library(rutils)  # Load package rutils
chart_Series(roll_sum(volatd, 10)[-(1:10)]/10,
       name=paste(symbol, "variance"))
chart_Series(roll_sum(hurst_daily, 10)[-(1:10)]/10,
       name=paste(symbol, "Hurst"))
abline(h=0.5, col="blue", lwd=2)
par(mfrow=c(2,1))  # set plot panels
library(rutils)  # Load package rutils
# Daily seasonality of Hurst exponent
interval <- "2013"
season_hurst <- season_ality(hurst_ohlc(ohlc=SPY[interval, 1:4]))
season_hurst <- season_hurst[-(nrow(season_hurst))]
colnames(season_hurst) <- paste0(colname(get(symbol)), ".season_hurst")
plot_theme <- chart_theme()
plot_theme$format.labels <- "%H:%M"
chobj <- chart_Series(x=season_hurst,
  name=paste(colnames(season_hurst),
  "daily seasonality"), theme=plot_theme,
  plot=FALSE)
ylim <- chobj$get_ylim()
ylim[[2]] <- structure(c(ylim[[2]][1],
        ylim[[2]][2]), fixed=TRUE)
chobj$set_ylim(ylim)
plot(chobj)
abline(h=0.5, col="blue", lwd=2)
# Daily seasonality of volatility
season_var <- season_ality(vol_ohlc(ohlc=SPY))
par(mfrow=c(2,1))  # set plot panels
library(rutils)  # Load package rutils
# Rolling variance
var_iance <-
  roll_agg_ohlc(ohlc=SPY, agg_fun="vol_ohlc")
# Rolling skew
skew <-
  roll_agg_ohlc(ohlc=SPY, agg_fun="skew_ohlc")
skew <- skew/(var_iance)^(1.5)
skew[1, ] <- 0
skew <- na.locf(skew)
interval <- "2013-11-11/2013-11-15"
chart_Series(var_iance[interval],
      name=paste(symbol, "variance"))
chart_Series(skew[interval],
      name=paste(symbol, "Skew"),
      ylim=c(-1, 1))
par(mfrow=c(2,1))  # set plot panels
library(rutils)  # Load package rutils
# Daily variance and skew
volatd <- xts::apply.daily(x=HighFreq::SPY, FUN=agg_ohlc,
                  agg_fun="vol_ohlc")
colnames(volatd) <- paste0(symbol, ".var")
daily_skew <- xts::apply.daily(x=HighFreq::SPY, FUN=agg_ohlc,
                  agg_fun="skew_ohlc")
daily_skew <- daily_skew/(volatd)^(1.5)
colnames(daily_skew) <- paste0(symbol, ".skew")
interval <- "2013-06-01/"
chart_Series(volatd[interval],
       name=paste(symbol, "variance"))
chart_Series(daily_skew[interval],
       name=paste(symbol, "skew"))
# skew scatterplot
retp <- calc_rets(xts_data=SPY)
skew <- skew_ohlc(log_ohlc=log(SPY[, -5]))
colnames(skew) <- paste0(symbol, ".skew")
lag_skew <- rutils::lag_it(skew)
lag_skew[1, ] <- 0
datav <- cbind(retp[, 1], sign(lag_skew))
formulav <- as.formula(paste(colnames(datav)[1],
    paste(paste(colnames(datav)[-1],
      collapse=" + "), "- 1"), sep="~"))
formulav
model <- lm(formulav, data=datav)
summary(model)$coef
summary(lm(formulav, data=datav["/2011-01-01"]))$coef
summary(lm(formulav, data=datav["2011-01-01/"]))$coef
interval <- "2013-12-01/"
plot(formulav, data=datav[interval],
     xlim=c(-2e-09, 2e-09),
     cex=0.6, xlab="skew", ylab="rets")
abline(model, col="blue", lwd=2)
# Contrarian skew trading strategy
# Lag the skew to get positions
posit <- -sign(lag_skew)
posit[1, ] <- 0
# Cumulative PnL
pnl <- cumsum(posit*retp[, 1])
# Calculate frequency of trades
50*sum(abs(sign(skew)-sign(lag_skew)))/nrow(skew)
# Calculate transaction costs
bid_offer <- 0.001  # 10 bps for liquid ETFs
bid_offer*sum(abs(sign(skew)-sign(lag_skew)))
chart_Series(pnl[endpoints(pnl, on="hours"), ],
  name=paste(symbol, "contrarian skew strategy pnl"))
# vwap plot
vwap_short <- vwapv(xtsv=SPY, look_back=70)
vwap_long <- vwapv(xtsv=SPY, look_back=225)
vwap_diff <- vwap_short - vwap_long
colnames(vwap_diff) <- paste0(symbol, ".vwap")
interval <- "2010-05-05/2010-05-07"
invisible(chart_Series(x=Cl(SPY[interval]),
         name=paste(symbol, "plus VWAP")))
invisible(add_TA(vwap_short[interval],
   on=1, col="red", lwd=2))
invisible(add_TA(vwap_long[interval],
   on=1, col="blue", lwd=2))
invisible(add_TA(vwap_diff[interval] > 0, on=-1,
   col="lightgreen", border="lightgreen"))
add_TA(vwap_diff[interval] < 0, on=-1,
 col="lightgrey", border="lightgrey")
# vwap scatterplot
# retp <- calc_rets(xts_data=SPY)
vwap_short <- vwapv(xtsv=SPY, look_back=70)
vwap_long <- vwapv(xtsv=SPY, look_back=225)
vwap_diff <- vwap_short - vwap_long
colnames(vwap_diff) <- paste0(symbol, ".vwap")
lag_vwap <- rutils::lag_it(vwap_diff)
lag_vwap[1, ] <- 0
datav <- cbind(retp[, 1], sign(lag_vwap))
formulav <- as.formula(paste(colnames(datav)[1],
    paste(paste(colnames(datav)[-1],
      collapse=" + "), "- 1"), sep="~"))
formulav
model <- lm(formulav, data=datav)
summary(model)$coef
summary(lm(formulav, data=datav["/2011-01-01"]))$coef
summary(lm(formulav, data=datav["2011-01-01/"]))$coef
interval <- "2013-12-01/"
plot(formulav, data=cbind(retp[, 1], lag_vwap)[interval],
     cex=0.6, xlab="skew", ylab="rets")
abline(model, col="blue", lwd=2)
# Trend following trading strategy
# Cumulative PnL
pnl <- cumsum(sign(lag_vwap)*retp[, 1])
# Calculate frequency of trades
50*sum(abs(sign(vwap_diff)-sign(lag_vwap)))/nrow(vwap_diff)
# Calculate transaction costs
bid_offer <- 0.001  # 10 bps for liquid ETFs
bid_offer*sum(abs(sign(vwap_diff)-sign(lag_vwap)))
chart_Series(
  pnl[endpoints(pnl, on="hours"), ],
  name=paste(symbol, "VWAP Trend Following Strategy PnL"))
library(rutils)  # Load package rutils
# Daily Hurst exponents
hurst_daily <- xts::apply.daily(x=HighFreq::SPY,
                     FUN=agg_ohlc,
                     agg_fun="hurst_ohlc")
colnames(hurst_daily) <-
  paste(colname(get(symbol)), ".Hurst")
chart_Series(roll_sum(hurst_daily, 10)[-(1:10)]/10,
       name=paste(symbol, "Hurst"))
abline(h=0.5, col="blue", lwd=2)
# Install package IBrokers
install.packages("IBrokers")
# Load package IBrokers
library(IBrokers)
# Get documentation for package IBrokers
# Get short description
packageDescription("IBrokers")
# Load help page
help(package="IBrokers")
# List all datasets in "IBrokers"
data(package="IBrokers")
# List all objects in "IBrokers"
ls("package:IBrokers")
# Remove IBrokers from search path
detach("package:IBrokers")
# Install package IBrokers2
devtools::install_github(repo="algoquant/IBrokers2")
# Connect to Interactive Brokers TWS
ib_connect <- IBrokers::twsConnect(port=7497)
# Or connect to IB Gateway
# Ib_connect <- ibgConnect(port=4002)
# Check connection
IBrokers::isConnected(ib_connect)
# Close the Interactive Brokers API connection
IBrokers::twsDisconnect(ib_connect)
# Connect to Interactive Brokers TWS
ib_connect <- IBrokers::twsConnect(port=7497)
# Or connect to IB Gateway
# Ib_connect <- ibgConnect(port=4002)
# Download account information from IB
ac_count <- "DU1215081"
ib_account <- IBrokers::reqAccountUpdates(conn=ib_connect,
                                    acctCode=ac_count)
# Extract account balances
balance_s <- ib_account[[1]]
balance_s$AvailableFunds
# Extract contract names, net positions, and profits and losses
IBrokers::twsPortfolioValue(ib_account)
# Close the Interactive Brokers API connection
IBrokers::twsDisconnect(ib_connect)
# Define AAPL stock contract (object)
contractobj <- IBrokers::twsEquity("AAPL", primary="SMART")
# Define CHF currency contract
contractobj <- IBrokers::twsCurrency("CHF", currency="USD")
# Define S&P Emini future June 2019 contract
contractobj <- IBrokers::twsFuture(symbol="ES",
  exch="GLOBEX", expiry="201906")
# Define 10yr Treasury future June 2019 contract
contractobj <- IBrokers::twsFuture(symbol="ZN",
  exch="ECBOT", expiry="201906")
# Define euro currency future June 2019 contract
contractobj <- IBrokers::twsFuture(symbol="EUR",
  exch="GLOBEX", expiry="201906")
# Define Gold future June 2019 contract
contractobj <- IBrokers::twsFuture(symbol="GC",
  exch="NYMEX", expiry="201906")
# Define Oil future January 2019 contract
contractobj <- IBrokers::twsFuture(symbol="QM",
  exch="NYMEX", expiry="201901")
# Test if contract object is correct
IBrokers::is.twsContract(contractobj)
# Get list with instrument information
IBrokers::reqContractDetails(conn=ib_connect, Contract=contractobj)
# Install the package twsInstrument
install.packages("twsInstrument", repos="http://r-forge.r-project.org")
# Define euro future using getContract() and Conid
contractobj <- twsInstrument::getContract("317631411")
# Get list with instrument information
IBrokers::reqContractDetails(conn=ib_connect, Contract=contractobj)
# Define VIX monthly and weekly futures June 2019 contract
symbol <- "VIX"
contractobj <- IBrokers::twsFuture(symbol=symbol,
  exch="CFE", expiry="201906")
# Define VIX monthly futures June 2019 contract
contractobj <- IBrokers::twsFuture(symbol=symbol,
  local="VXV8", exch="CFE", expiry="201906")
# Define VIX weekly futures October 3rd 2018 contract
contractobj <- IBrokers::twsFuture(symbol=symbol,
  local="VX40V8", exch="CFE", expiry="201906")
# Get list with instrument information
IBrokers::reqContractDetails(conn=ib_connect,
  Contract=contractobj)
# Define S&P Emini futures June 2019 contract
symbol <- "ES"
contractobj <- IBrokers::twsFuture(symbol=symbol,
  exch="GLOBEX", expiry="201906")
# Open file for data download
dir_name <- "/Users/jerzy/Develop/data/ib_data"
dir.create(dir_name)
file_name <- file.path(dir_name, paste0(symbol, "201906.csv"))
file_connect <- file(file_name, open="w")
# Connect to Interactive Brokers TWS
ib_connect <- IBrokers::twsConnect(port=7497)
# Write header to file
cat(paste(paste(symbol, c("Index", "Open", "High", "Low", "Close", "Volume", "WAP", "Count"), sep="."), collapse=","), "\n", file=file_connect)
# Download historical data to file
IBrokers::reqHistoricalData(conn=ib_connect,
  Contract=contractobj,
  barSize="1 day", duration="6 M",
  file=file_connect)
# Close data file
close(file_connect)
# Close the Interactive Brokers API connection
IBrokers::twsDisconnect(ib_connect)
# Define IB contract objects for stock symbols
symbolv <- c("AAPL", "F", "MSFT")
contractv <- lapply(symbolv, IBrokers::twsEquity, primary="SMART")
names(contractv) <- symbolv
# Open file connections for data download
dir_name <- "/Users/jerzy/Develop/data/ib_data"
file_names <- file.path(dir_name, paste0(symbolv, format(Sys.time(), format="_%m_%d_%Y_%H_%M"), ".csv"))
file_connects <- lapply(file_names, function(file_name) file(file_name, open="w"))
# Connect to Interactive Brokers TWS
ib_connect <- IBrokers::twsConnect(port=7497)
# Download historical 1-minute bar data to files
for (it in 1:NROW(symbolv)) {
  symbol <- symbolv[it]
  file_connect <- file_connects[[it]]
  contractobj <- contractv[[it]]
  cat("Downloading data for: ", symbol, "\n")
  # Write header to file
  cat(paste(paste(symbol, c("Index", "Open", "High", "Low", "Close", "Volume", "WAP", "XTRA", "Count"), sep="."), collapse=","), "\n", file=file_connect)
  IBrokers::reqHistoricalData(conn=ib_connect,
                         Contract=contractobj,
                         barSize="1 min", duration="2 D",
                         file=file_connect)
  Sys.sleep(10) # 10s pause to avoid IB pacing violation
}  # end for
# Close data files
for (file_connect in file_connects) close(file_connect)
# Close the Interactive Brokers API connection
IBrokers::twsDisconnect(ib_connect)
# Define S&P Emini futures June 2018 contract
symbol <- "ES"
contractobj <- IBrokers::twsFuture(symbol=symbol,
  include_expired="1",
  exch="GLOBEX", expiry="201806")
# Open file connection for ESM8 data download
file_name <- file.path(dir_name, paste0(symbol, "M8.csv"))
file_connect <- file(file_name, open="w")
# Connect to Interactive Brokers TWS
ib_connect <- IBrokers::twsConnect(port=7497)
# Download historical data to file
IBrokers::reqHistoricalData(conn=ib_connect,
  Contract=contractobj,
  barSize="1 day", duration="2 Y",
  file=file_connect)
# Close data file
close(file_connect)
# Close the Interactive Brokers API connection
IBrokers::twsDisconnect(ib_connect)
# Load OHLC data and coerce it into xts series
pricev <- data.table::fread(file_name)
data.table::setDF(pricev)
pricev <- xts::xts(pricev[, 2:6],
  order.by=as.Date(as.POSIXct.numeric(pricev[, 1],
    tz="America/New_York", origin="1970-01-01")))
colnames(pricev) <- c("Open", "High", "Low", "Close", "Volume")
# Plot OHLC data in x11 window
chart_Series(x=pricev, TA="add_Vo()",
  name="S&P500 ESM8 futures")
# Plot dygraph
dygraphs::dygraph(pricev[, 1:4], main="S&P500 ESM8 futures") %>%
  dyCandlestick()
# Define S&P Emini futures June 2018 contract
symbol <- "ES"
contractobj <- IBrokers::twsFuture(symbol=symbol,
  include_expired="1",
  exch="GLOBEX", expiry="201806")
# Open file connection for data download
dir_name <- "/Users/jerzy/Develop/data/ib_data"
dir.create(dir_name)
file_name <- file.path(dir_name, paste0(symbol, ".csv"))
file_connect <- file(file_name, open="w")
# Connect to Interactive Brokers TWS
ib_connect <- IBrokers::twsConnect(port=7497)
# Download historical data to file
IBrokers::reqHistoricalData(conn=ib_connect,
  Contract=contractobj,
  barSize="1 day", duration="6 M",
  file=file_connect)
# Close data file
close(file_connect)
# Close the Interactive Brokers API connection
IBrokers::twsDisconnect(ib_connect)
# Define S&P Emini futures June 2019 contract
symbol <- "ES"
contractobj <- IBrokers::twsFuture(symbol=symbol,
  exch="GLOBEX", expiry="201906")
# Open file connection for data download
dir_name <- "/Users/jerzy/Develop/data/ib_data"
# Dir.create(dir_name)
file_name <- file.path(dir_name, paste0(symbol, "_taq_live.csv"))
file_connect <- file(file_name, open="w")
# Connect to Interactive Brokers TWS
ib_connect <- IBrokers::twsConnect(port=7497)
# Download live data to file
IBrokers::reqMktData(conn=ib_connect,
     Contract=contractobj,
     eventWrapper=eWrapper.MktData.CSV(1),
     file=file_connect)
# Close data file
close(file_connect)
# Close the Interactive Brokers API connection
IBrokers::twsDisconnect(ib_connect)
# Define S&P Emini futures June 2019 contract
symbol <- "ES"
contractobj <- IBrokers::twsFuture(symbol=symbol,
  exch="GLOBEX", expiry="201906")
# Open file connection for data download
dir_name <- "/Users/jerzy/Develop/data/ib_data"
# Dir.create(dir_name)
file_name <- file.path(dir_name, paste0(symbol, "_ohlc_live.csv"))
file_connect <- file(file_name, open="w")
# Connect to Interactive Brokers TWS
ib_connect <- IBrokers::twsConnect(port=7497)
# Download live data to file
IBrokers::reqRealTimeBars(conn=ib_connect,
     Contract=contractobj, barSize="1",
     eventWrapper=eWrapper.RealTimeBars.CSV(1),
     file=file_connect)
# Close the Interactive Brokers API connection
IBrokers::twsDisconnect(ib_connect)
# Close data file
close(file_connect)
# Load OHLC data and coerce it into xts series
library(data.table)
pricev <- data.table::fread(file_name)
pricev <- xts::xts(pricev[, paste0("V", 2:6)],
  as.POSIXct.numeric(as.numeric(pricev[, V1]), tz="America/New_York", origin="1970-01-01"))
colnames(pricev) <- c("Open", "High", "Low", "Close", "Volume")
# Plot OHLC data in x11 window
x11()
chart_Series(x=pricev, TA="add_Vo()",
       name="S&P500 ESM9 futures")
# Plot dygraph
library(dygraphs)
dygraphs::dygraph(pricev[, 1:4], main="S&P500 ESM9 futures") %>%
  dyCandlestick()
library(IBrokers)
# Define list of S&P futures and 10yr Treasury contracts
contractv <- list(ES=IBrokers::twsFuture(symbol="ES", exch="GLOBEX", expiry="201906"),
             ZN=IBrokers::twsFuture(symbol="ZN", exch="ECBOT", expiry="201906"))
# Open the file connection for storing the bar data
dir_name <- "/Users/jerzy/Develop/data/ib_data"
file_names <- file.path(dir_name, paste0(c("ES", "ZN_"), format(Sys.time(), format="%m_%d_%Y_%H_%M"), ".csv"))
file_connects <- lapply(file_names, function(file_name) file(file_name, open="w"))
# Connect to Interactive Brokers TWS
ib_connect <- IBrokers::twsConnect(port=7497)
# Download live data to file
IBrokers::reqRealTimeBars(conn=ib_connect,
                    Contract=contractv,
                    barSize="1", useRTH=FALSE,
                    eventWrapper=eWrapper.RealTimeBars.CSV(NROW(contractv)),
                    file=file_connects)
# Close the Interactive Brokers API connection
IBrokers::twsDisconnect(ib_connect)
# Close data files
for (file_connect in file_connects)
  close(file_connect)
library(data.table)
# Load ES futures June 2019 contract and coerce it into xts series
pricev <- data.table::fread(file_names[1])
pricev <- xts::xts(pricev[, paste0("V", 2:6)],
  as.POSIXct.numeric(as.numeric(pricev[, V1]), tz="America/New_York", origin="1970-01-01"))
colnames(pricev) <- c("Open", "High", "Low", "Close", "Volume")
# Plot dygraph
library(dygraphs)
dygraphs::dygraph(pricev[, 1:4], main="S&P500 ESM9 futures") %>%
  dyCandlestick()
# Load ZN 10yr Treasury futures June 2019 contract
pricev <- data.table::fread(file_names[2])
pricev <- xts::xts(pricev[, paste0("V", 2:6)],
  as.POSIXct.numeric(as.numeric(pricev[, V1]), tz="America/New_York", origin="1970-01-01"))
colnames(pricev) <- c("Open", "High", "Low", "Close", "Volume")
# Plot dygraph
dygraphs::dygraph(pricev[, 1:4], main="ZN 10yr Treasury futures") %>%
  dyCandlestick()
# Define S&P Emini future June 2019 contract
contractobj <- IBrokers::twsFuture(symbol="ES", exch="GLOBEX", expiry="201906")
# Define euro currency contract EUR.USD
contractobj <- IBrokers::twsCurrency("EUR", currency="USD")
# Define euro currency E-mini futures June 2019 contract E7Z8
contractobj <- IBrokers::twsFuture(symbol="E7", exch="GLOBEX", expiry="201906")
# Define Japanese yen currency contract JPY.USD
contractobj <- IBrokers::twsCurrency("JPY", currency="USD")
# Define Japanese yen currency E-mini futures June 2019 contract J7Z8
contractobj <- IBrokers::twsFuture(symbol="J7", exch="GLOBEX", expiry="201906")
# Define Japanese yen currency futures June 2019 contract 6JZ8
contractobj <- IBrokers::twsFuture(symbol="JPY", exch="GLOBEX", expiry="201906")
# Connect to Interactive Brokers TWS
ib_connect <- IBrokers::twsConnect(port=7497)
IBrokers::reqContractDetails(conn=ib_connect, Contract=contractobj)
# Request trade order ID
order_id <- IBrokers::reqIds(ib_connect)
# Create buy market order object
ib_order <- IBrokers::twsOrder(order_id,
  orderType="MKT", action="BUY", totalQuantity=1)
# Place trade order
IBrokers::placeOrder(ib_connect, contractobj, ib_order)
# Execute sell market order
order_id <- IBrokers::reqIds(ib_connect)
ib_order <- IBrokers::twsOrder(order_id,
  orderType="MKT", action="SELL", totalQuantity=1)
IBrokers::placeOrder(ib_connect, contractobj, ib_order)
# Execute buy market order
order_id <- IBrokers::reqIds(ib_connect)
ib_order <- IBrokers::twsOrder(order_id,
  orderType="MKT", action="BUY", totalQuantity=1)
IBrokers::placeOrder(ib_connect, contractobj, ib_order)
# Request trade order ID
order_id <- IBrokers::reqIds(ib_connect)
# Create buy limit order object
ib_order <- IBrokers::twsOrder(order_id, orderType="LMT",
  lmtPrice="1.1511", action="BUY", totalQuantity=1)
# Place trade order
IBrokers::placeOrder(ib_connect, contractobj, ib_order)
# Cancel trade order
IBrokers::cancelOrder(ib_connect, order_id)
# Execute sell limit order
order_id <- IBrokers::reqIds(ib_connect)
ib_order <- IBrokers::twsOrder(order_id, orderType="LMT",
  lmtPrice="1.1512", action="SELL", totalQuantity=1)
IBrokers::placeOrder(ib_connect, contractobj, ib_order)
# Cancel trade order
IBrokers::cancelOrder(ib_connect, order_id)
# Close the Interactive Brokers API connection
IBrokers::twsDisconnect(ib_connect)
eWrapper_realtimebars <- function(n = 1) {
  eW <- eWrapper_new(NULL)
  # eW <- IBrokers::eWrapper(NULL)
  eW$assign.Data("data", rep(list(structure(.xts(matrix(rep(NA_real_, 7), ncol = 7), 0), .Dimnames = list(NULL, c("Open", "High", "Low", "Close", "Volume", "WAP", "Count")))), n))
  eW$realtimeBars <- function(curMsg, msg, timestamp, file, ...) {
    id <- as.numeric(msg[2])
    file <- file[[id]]
    data <- eW$get.Data("data")
    attr(data[[id]], "index") <- as.numeric(msg[3])
    nr.data <- NROW(data[[id]])
    # Write to file
    cat(paste(msg[3], msg[4], msg[5], msg[6], msg[7], msg[8], msg[9], msg[10], sep = ","), "\n", file = file, append = TRUE)
    # Write to console
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
    IBrokers::placeOrder(ib_connect, contractobj, buy_order)
    # Execute sell limit order
    sell_id <- IBrokers::reqIds(ib_connect)
    sell_order <- IBrokers::twsOrder(sell_id, orderType="LMT",
                               lmtPrice=msg[5]+0.25, action="SELL", totalQuantity=1)
    IBrokers::placeOrder(ib_connect, contractobj, sell_order)
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
eWrapper_realtimebars <- function(n = 1) {
  eW <- eWrapper_new(NULL)
  # eW <- IBrokers::eWrapper(NULL)
  eW$assign.Data("data", rep(list(structure(.xts(matrix(rep(NA_real_, 7), ncol = 7), 0), .Dimnames = list(NULL, c("Open", "High", "Low", "Close", "Volume", "WAP", "Count")))), n))
  eW$realtimeBars <- function(curMsg, msg, timestamp, file, ...) {
    id <- as.numeric(msg[2])
    file <- file[[id]]
    data <- eW$get.Data("data")
    attr(data[[id]], "index") <- as.numeric(msg[3])
    nr.data <- NROW(data[[id]])
    # Write to file
    cat(paste(msg[3], msg[4], msg[5], msg[6], msg[7], msg[8], msg[9], msg[10], sep = ","), "\n", file = file, append = TRUE)
    # Write to console
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
    IBrokers::placeOrder(ib_connect, contractobj, buy_order)
    # Execute sell limit order
    sell_id <- IBrokers::reqIds(ib_connect)
    sell_order <- IBrokers::twsOrder(sell_id, orderType="LMT",
                               lmtPrice=msg[5]+0.25, action="SELL", totalQuantity=1)
    IBrokers::placeOrder(ib_connect, contractobj, sell_order)
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
# Define S&P Emini futures June 2019 contract
snp_contract <- IBrokers::twsFuture(symbol="ES",
  exch="GLOBEX", expiry="201906")
# Define VIX futures June 2019 contract
vix_contract <- IBrokers::twsFuture(symbol="VIX",
  local="VXZ8", exch="CFE", expiry="201906")
# Define 10yr Treasury futures June 2019 contract
trs_contract <- IBrokers::twsFuture(symbol="ZN",
  exch="ECBOT", expiry="201906")
# Define Emini gold futures June 2019 contract
gold_contract <- IBrokers::twsFuture(symbol="YG",
  exch="NYSELIFFE", expiry="201906")
# Define euro currency future June 2019 contract
euro_contract <- IBrokers::twsFuture(symbol="EUR",
  exch="GLOBEX", expiry="201906")
IBrokers::reqContractDetails(conn=ib_connect, Contract=euro_contract)
# Define data directory
dir_name <- "/Users/jerzy/Develop/data/ib_data"
# Dir.create(dir_name)
# Open file for error messages
file_root <- "replay"
file_name <- file.path(dir_name, paste0(file_root, "_error.csv"))
error_connect <- file(file_name, open="w")
# Open file for raw data
file_name <- file.path(dir_name, paste0(file_root, "_raw.csv"))
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
file_name <- file.path(dir_name, paste0(file_root, "_raw.csv"))
raw_connect <- IBrokers::twsConnect(file_name)
class(raw_connect) <- c("twsPlayback", class(raw_connect))
# Replay the raw data
IBrokers::reqMktData(raw_connect, list(snp_contract, vix_contract))
# Open file for data
file_connect <- file(file.path(dir_name, "temp.csv"), open="w")
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
contractobj <- IBrokers::twsEquity("AAPL", primary="SMART")
# Define CHF currency contract
contractobj <- IBrokers::twsCurrency("CHF", currency="USD")
# Define S&P Emini future June 2019 contract
contractobj <- IBrokers::twsFuture(symbol="ES",
  exch="GLOBEX", expiry="201906")
# Define 10yr Treasury future June 2019 contract
contractobj <- IBrokers::twsFuture(symbol="ZN",
  exch="ECBOT", expiry="201906")
# Define euro currency future June 2019 contract
contractobj <- IBrokers::twsFuture(symbol="EUR",
  exch="GLOBEX", expiry="201906")
# Define Gold future June 2019 contract
contractobj <- IBrokers::twsFuture(symbol="GC",
  exch="NYMEX", expiry="201906")
# Test if contract object is correct
IBrokers::is.twsContract(contractobj)
# Get list with instrument information
IBrokers::reqContractDetails(conn=ib_connect, Contract=contractobj)
# Install the package twsInstrument
install.packages("twsInstrument", repos="http://r-forge.r-project.org")
# Define euro future using getContract() and Conid
contractobj <- twsInstrument::getContract("317631411")
# Get list with instrument information
IBrokers::reqContractDetails(conn=ib_connect, Contract=contractobj)
# Define AAPL stock contract (object)
contractobj <- IBrokers::twsEquity("AAPL", primary="SMART")
# Define CHF currency contract
contractobj <- IBrokers::twsCurrency("CHF", currency="USD")
# Define S&P Emini future June 2019 contract
contractobj <- IBrokers::twsFuture(symbol="ES",
  exch="GLOBEX", expiry="201906")
# Define 10yr Treasury future June 2019 contract
contractobj <- IBrokers::twsFuture(symbol="ZN",
  exch="ECBOT", expiry="201906")
# Define euro currency future June 2019 contract
contractobj <- IBrokers::twsFuture(symbol="EUR",
  exch="GLOBEX", expiry="201906")
# Define Gold future June 2019 contract
contractobj <- IBrokers::twsFuture(symbol="GC",
  exch="NYMEX", expiry="201906")
# Test if contract object is correct
IBrokers::is.twsContract(contractobj)
# Get list with instrument information
IBrokers::reqContractDetails(conn=ib_connect, Contract=contractobj)
# Install the package twsInstrument
install.packages("twsInstrument", repos="http://r-forge.r-project.org")
# Define euro future using getContract() and Conid
contractobj <- twsInstrument::getContract("317631411")
# Get list with instrument information
IBrokers::reqContractDetails(conn=ib_connect, Contract=contractobj)
