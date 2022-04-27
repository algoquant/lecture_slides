






# Load S&P500 constituent stock prices
load("/Users/jerzy/Develop/lecture_slides/data/sp500.RData")
prices <- eapply(sp500env, quantmod::Cl)
prices <- rutils::do_call(cbind, prices)
# Carry forward non-NA prices
prices <- zoo::na.locf(prices, na.rm=FALSE)
# Drop ".Close" from column names
colnames(prices[, 1:4])
colnames(prices) <- rutils::get_name(colnames(prices))
# Or
# colnames(prices) <- do.call(rbind,
#   strsplit(colnames(prices), split="[.]"))[, 1]
# Calculate percentage returns of the S&P500 constituent stocks
# returns <- xts::diff.xts(log(prices))
returns <- xts::diff.xts(prices)/
  rutils::lagit(prices, pad_zeros=FALSE)
set.seed(1121)
samplev <- sample(NCOL(returns), s=100, replace=FALSE)
prices100 <- prices[, samplev]
returns100 <- returns[, samplev]
save(prices, prices100,
  file="/Users/jerzy/Develop/lecture_slides/data/sp500_prices.RData")
save(returns, returns100,
  file="/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")

# Calculate number of constituents without prices
datav <- rowSums(is.na(prices))
datav <- xts::xts(datav, order.by=zoo::index(prices))
dygraphs::dygraph(datav, main="Number of S&P500 Constituents Without Prices") %>%
  dyOptions(colors="blue", strokeWidth=2) %>%
  dyAxis("y", valueRange=c(0, 300))

# Calculate price weighted index of constituent
ncols <- NCOL(prices)
prices <- zoo::na.locf(prices, fromLast=TRUE)
indeks <- xts(rowSums(prices)/ncols, zoo::index(prices))
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

# Select ETF symbols for asset allocation
symbolv <- c("VTI", "VEU", "EEM", "XLY", "XLP", "XLE", "XLF",
  "XLV", "XLI", "XLB", "XLK", "XLU", "VYM", "IVW", "IWB", "IWD",
  "IWF", "IEF", "TLT", "VNQ", "DBC", "GLD", "USO", "VXX", "SVXY",
  "MTUM", "IVE", "VLUE", "QUAL", "VTV", "USMV")
# Read etf database into data frame
etf_list <- read.csv(file="/Users/jerzy/Develop/lecture_slides/data/etf_list.csv")
rownames(etf_list) <- etf_list$Symbol
# Select from etf_list only those ETF's in symbolv
etf_list <- etf_list[symbolv, ]
# Shorten names
etf_names <- sapply(etf_list$Name, function(name) {
  namesvplit <- strsplit(name, split=" ")[[1]]
  namesvplit <- namesvplit[c(-1, -NROW(namesvplit))]
  name_match <- match("Select", namesvplit)
  if (!is.na(name_match))
    namesvplit <- namesvplit[-name_match]
  paste(namesvplit, collapse=" ")
})  # end sapply
etf_list$Name <- etf_names
etf_list["IEF", "Name"] <- "10 year Treasury Bond Fund"
etf_list["TLT", "Name"] <- "20 plus year Treasury Bond Fund"
etf_list["XLY", "Name"] <- "Consumer Discr. Sector Fund"
etf_list["EEM", "Name"] <- "Emerging Market Stock Fund"
etf_list["MTUM", "Name"] <- "Momentum Factor Fund"
etf_list["SVXY", "Name"] <- "Short VIX Futures"
etf_list["VXX", "Name"] <- "Long VIX Futures"
etf_list["DBC", "Name"] <- "Commodity Futures Fund"
etf_list["USO", "Name"] <- "WTI Oil Futures Fund"
etf_list["GLD", "Name"] <- "Physical Gold Fund"

print(xtable::xtable(etf_list), comment=FALSE, size="tiny", include.rownames=FALSE)

# Symbols for constant maturity Treasury rates
symbolv <- c("DGS1", "DGS2", "DGS5", "DGS10", "DGS20", "DGS30")
# Create new environment for time series
rates_env <- new.env()
# Download time series for symbolv into rates_env
quantmod::getSymbols(symbolv, env=rates_env, src="FRED")
# List files in rates_env
ls(rates_env)
# Get class of all objects in rates_env
sapply(rates_env, class)
# Get class of all objects in R workspace
sapply(ls(), function(name) class(get(name)))
# Save the time series environment into a binary .RData file
save(rates_env, file="/Users/jerzy/Develop/lecture_slides/data/rates_data.RData")

# Get class of time series object DGS10
class(get(x="DGS10", envir=rates_env))
# Another way
class(rates_env$DGS10)
# Get first 6 rows of time series
head(rates_env$DGS10)
# Plot dygraphs of 10-year Treasury rate
dygraphs::dygraph(rates_env$DGS10, main="10-year Treasury Rate") %>%
  dyOptions(colors="blue", strokeWidth=2)
# Plot 10-year constant maturity Treasury rate
x11(width=6, height=5)
par(mar=c(2, 2, 0, 0), oma=c(0, 0, 0, 0))
chart_Series(rates_env$DGS10["1990/"], name="10-year Treasury Rate")

# Load constant maturity Treasury rates
load(file="/Users/jerzy/Develop/lecture_slides/data/rates_data.RData")
# Get most recent yield curve
yc2021 <- eapply(rates_env, xts::last)
class(yc2021)
yc2021 <- do.call(cbind, yc2021)
# Check if 2020-03-25 is not a holiday
day2020 <- as.Date("2020-03-25")
weekdays(day2020)
# Get yield curve from 2020-03-25
yc2020 <- eapply(rates_env, function(x) x[day2020])
yc2020 <- do.call(cbind, yc2020)
# Combine the yield curves
rates <- c(yc2020, yc2021)
# Rename columns and rows, sort columns, and transpose into matrix
colnames(rates) <- substr(colnames(rates), start=4, stop=11)
rates <- rates[, order(as.numeric(colnames(rates)))]
colnames(rates) <- paste0(colnames(rates), "yr")
rates <- t(rates)
colnames(rates) <- substr(colnames(rates), start=1, stop=4)

x11(width=6, height=5)
par(mar=c(3, 3, 2, 0), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
# Plot using matplot()
colors <- c("blue", "red")
matplot(rates, main="Yield Curves in 2020 and 2021", xaxt="n", lwd=3, lty=1,
  type="l", xlab="maturity", ylab="yield", col=colors)
# Add x-axis
axis(1, seq_along(rownames(rates)), rownames(rates))
# Add legend
legend("topleft", legend=colnames(rates),
 col=colors, lty=1, lwd=6, inset=0.05, cex=1.0)

x11(width=6, height=5)
par(mar=c(3, 3, 2, 0), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
# Load constant maturity Treasury rates
load(file="/Users/jerzy/Develop/lecture_slides/data/rates_data.RData")
# Get end-of-year dates since 2006
dates <- xts::endpoints(rates_env$DGS1["2006/"], on="years")
dates <- zoo::index(rates_env$DGS1["2006/"][dates])
# Create time series of end-of-year rates
rates <- eapply(rates_env, function(ra_te) ra_te[dates])
rates <- rutils::do_call(cbind, rates)
# Rename columns and rows, sort columns, and transpose into matrix
colnames(rates) <- substr(colnames(rates), start=4, stop=11)
rates <- rates[, order(as.numeric(colnames(rates)))]
colnames(rates) <- paste0(colnames(rates), "yr")
rates <- t(rates)
colnames(rates) <- substr(colnames(rates), start=1, stop=4)
# Plot matrix using plot.zoo()
colors <- colorRampPalette(c("red", "blue"))(NCOL(rates))
plot.zoo(rates, main="Yield curve since 2006", lwd=3, xaxt="n",
   plot.type="single", xlab="maturity", ylab="yield", col=colors)
# Add x-axis
axis(1, seq_along(rownames(rates)), rownames(rates))
# Add legend
legend("topleft", legend=colnames(rates),
 col=colors, lty=1, lwd=4, inset=0.05, cex=0.8)

# Alternative plot using matplot()
matplot(rates, main="Yield curve since 2006", xaxt="n", lwd=3, lty=1,
  type="l", xlab="maturity", ylab="yield", col=colors)
# Add x-axis
axis(1, seq_along(rownames(rates)), rownames(rates))
# Add legend
legend("topleft", legend=colnames(rates),
 col=colors, lty=1, lwd=4, inset=0.05, cex=0.8)

# Extract rates from rates_env
symbolv <- c("DGS1", "DGS2", "DGS5", "DGS10", "DGS20")
rates <- mget(symbolv, envir=rates_env)
rates <- rutils::do_call(cbind, rates)
rates <- zoo::na.locf(rates, na.rm=FALSE)
rates <- zoo::na.locf(rates, fromLast=TRUE)
# Calculate daily percentage rates changes
returns <- rutils::diffit(log(rates))
# De-mean the returns
returns <- lapply(returns, function(x) {x - mean(x)})
returns <- rutils::do_call(cbind, returns)
sapply(returns, mean)
# Covariance and Correlation matrices of Treasury rates
covmat <- cov(returns)
cormat <- cor(returns)
# Reorder correlation matrix based on clusters
library(corrplot)
ordern <- corrMatOrder(cormat, order="hclust",
  hclust.method="complete")
cormat <- cormat[ordern, ordern]

# Plot the correlation matrix
x11(width=6, height=6)
colors <- colorRampPalette(c("red", "white", "blue"))
corrplot(cormat, title=NA, tl.col="black",
    method="square", col=colors(NCOL(cormat)), tl.cex=0.8,
    cl.offset=0.75, cl.cex=0.7, cl.align.text="l", cl.ratio=0.25)
title("Correlation of Treasury Rates", line=1)
# Draw rectangles on the correlation matrix plot
corrRect.hclust(cormat, k=NROW(cormat) %/% 2,
  method="complete", col="red")

# Create initial vector of portfolio weights
nweights <- NROW(symbolv)
weights <- rep(1/sqrt(nweights), nweights)
names(weights) <- symbolv
# Objective function equal to minus portfolio variance
object <- function(weights, returns) {
  retsp <- returns %*% weights
  -1e7*var(retsp) + 1e7*(1 - sum(weights*weights))^2
}  # end object
# Objective function for equal weight portfolio
object(weights, returns)
# Compare speed of vector multiplication methods
library(microbenchmark)
summary(microbenchmark(
  transp=t(returns) %*% returns,
  sumv=sum(returns*returns),
  times=10))[, c(1, 4, 5)]

# Find weights with maximum variance
optiml <- optim(par=weights,
  fn=object,
  returns=returns,
  method="L-BFGS-B",
  upper=rep(5.0, nweights),
  lower=rep(-5.0, nweights))
# Optimal weights and maximum variance
weights1 <- optiml$par
object(weights1, returns)
# Plot first principal component loadings
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
barplot(weights1, names.arg=names(weights1),
  xlab="", ylab="", main="First Principal Component Loadings")

# pc1 weights and returns
pc1 <- drop(returns %*% weights1)
# Redefine objective function
object <- function(weights, returns) {
  retsp <- returns %*% weights
  -1e7*var(retsp) + 1e7*(1 - sum(weights^2))^2 +
    1e7*sum(weights1*weights)^2
}  # end object
# Find second principal component weights
optiml <- optim(par=weights,
             fn=object,
             returns=returns,
             method="L-BFGS-B",
             upper=rep(5.0, nweights),
             lower=rep(-5.0, nweights))

# pc2 weights and returns
weights2 <- optiml$par
pc2 <- drop(returns %*% weights2)
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
pcad <- prcomp(returns, scale=TRUE)
# Compare outputs
all.equal(eigend$values, pcad$sdev^2)
all.equal(abs(eigend$vectors), abs(pcad$rotation),
    check.attributes=FALSE)
# Eigen decomposition of covariance matrix
eigend <- eigen(covmat)
# Perform PCA without scaling
pcad <- prcomp(returns, scale=FALSE)
# Compare outputs
all.equal(eigend$values, pcad$sdev^2)
all.equal(abs(eigend$vectors), abs(pcad$rotation),
    check.attributes=FALSE)

# Perform principal component analysis PCA
pcad <- prcomp(returns, scale=TRUE)
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
returns <- lapply(returns, function(x) {(x - mean(x))/sd(x)})
returns <- rutils::do_call(cbind, returns)
sapply(returns, mean)
sapply(returns, sd)
# Calculate principal component time series
pcats <- returns %*% pcad$rotation
all.equal(pcad$x, pcats, check.attributes=FALSE)
# Calculate products of principal component time series
round(t(pcats) %*% pcats, 2)
# Coerce to xts time series
pcats <- xts(pcats, order.by=zoo::index(returns))
pcats <- cumsum(pcats)
# Plot principal component time series in multiple panels
par(mfrow=c(3,2))
par(mar=c(2, 2, 0, 1), oma=c(0, 0, 0, 0))
rangev <- range(pcats)
for (ordern in 1:NCOL(pcats)) {
  plot.zoo(pcats[, ordern], ylim=rangev, xlab="", ylab="")
  title(paste0("PC", ordern), line=-1, col.main="red")
}  # end for

# Invert all the principal component time series
pcarets <- returns %*% pcad$rotation
solved <- pcarets %*% solve(pcad$rotation)
all.equal(coredata(returns), solved)

# Invert first 3 principal component time series
solved <- pcarets[, 1:3] %*% solve(pcad$rotation)[1:3, ]
solved <- xts::xts(solved, zoo::index(returns))
solved <- cumsum(solved)
cumrets <- cumsum(returns)
# Plot the solved returns
par(mfrow=c(3,2))
par(mar=c(2, 2, 0, 1), oma=c(0, 0, 0, 0))
for (symbol in symbolv) {
  plot.zoo(cbind(cumrets[, symbol], solved[, symbol]),
    plot.type="single", col=c("black", "blue"), xlab="", ylab="")
  legend(x="topleft", bty="n",
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
codes <- cbind(c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"),
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
prices <- data.table::fread(file_name)
class(prices)
# Coerce first column from string to date-time
unlist(sapply(prices, class))
tail(prices)
prices$Index <- as.POSIXct(prices$Index,
  tz="America/New_York", origin="1970-01-01")
# Coerce prices into xts series
prices <- data.table::as.xts.data.table(prices)
class(prices)
tail(prices)
colnames(prices)[1:5] <- c("Open", "High", "Low", "Close", "Volume")
tail(prices)

# Plot OHLC data in x11 window
x11(width=5, height=4)  # Open x11 for plotting
par(mar=c(5, 5, 2, 1), oma=c(0, 0, 0, 0))
chart_Series(x=prices, TA="add_Vo()",
  name="S&P500 futures")
# Plot dygraph
dygraphs::dygraph(prices[, 1:4], main="OHLC prices") %>%
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
volumes <- cbind(Vo(ESU8),
  Vo(ESM8))[paste0(startd, "/", endd)]
colnames(volumes) <- c("ESU8", "ESM8")
colors <- c("blue", "green")
plot(volumes, col=colors, lwd=3, major.ticks="days",
     format.labels="%b-%d", observation.based=TRUE,
     main="Volumes of ESU8 and ESM8 futures")
legend("topleft", legend=colnames(volumes), col=colors,
 title=NULL, bty="n", lty=1, lwd=6, inset=0.1, cex=0.7)

# Find date when ESU8 volume exceeds ESM8
exceeds <- (volumes[, "ESU8"] > volumes[, "ESM8"])
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
ls(vix_env)
vix_env$vix_index <- vix_index
save(vix_env, file="/Users/jerzy/Develop/data/ib_data/vix_cboe.RData")

# Plot VIX OHLC data in x11 window
chart_Series(x=vix_index["2018"], name="VIX Index")
# Plot dygraph
dygraphs::dygraph(vix_index, main="VIX Index") %>%
  dyCandlestick()

# Read CBOE monthly futures expiration dates
dates <- read.csv(
  file="/Users/jerzy/Develop/data/vix_data/vix_dates.csv")
dates <- as.Date(dates[, 1])
years <- format(dates, format="%Y")
years <- substring(years, 4)
# Monthly futures contract codes
codes <-
  c("F", "G", "H", "J", "K", "M",
    "N", "Q", "U", "V", "X", "Z")
symbolv <- paste0("VX", codes, years)
dates <- as.data.frame(dates)
colnames(dates) <- "exp_dates"
rownames(dates) <- symbolv
# Write dates to CSV file, with row names
write.csv(dates, row.names=TRUE,
  file="/Users/jerzy/Develop/data/vix_data/vix_futures.csv")
# Read back CBOE futures expiration dates
dates <- read.csv(file="/Users/jerzy/Develop/data/vix_data/vix_futures.csv",
  row.names=1)
dates[, 1] <- as.Date(dates[, 1])

# Load VIX futures data from binary file
load(file="/Users/jerzy/Develop/data/vix_data/vix_cboe.RData")
# Get all VIX futures for 2018 except January
symbolv <- ls(vix_env)
symbolv <- symbolv[grep("*8", symbolv)]
symbolv <- symbolv[2:9]
# Specify dates for curves
low_vol <- as.Date("2018-01-11")
hi_vol <- as.Date("2018-02-05")
# Extract all VIX futures prices on the dates
curve_s <- lapply(symbolv, function(symbol) {
  xtes <- get(x=symbol, envir=vix_env)
  Cl(xtes[c(low_vol, hi_vol)])
})  # end lapply
curve_s <- rutils::do_call(cbind, curve_s)
colnames(curve_s) <- symbolv
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

# Load VIX futures data from binary file
load(file="/Users/jerzy/Develop/data/vix_data/vix_cboe.RData")
# Read CBOE futures expiration dates
dates <- read.csv(file="/Users/jerzy/Develop/data/vix_data/vix_futures.csv",
  row.names=1)
symbolv <- rownames(dates)
dates <- as.Date(dates[, 1])
todayd <- as.Date("2018-05-07")
maturi_ty <- (todayd + 30)
# Find neighboring futures contracts
indeks <- match(TRUE, dates > maturi_ty)
front_date <- dates[indeks-1]
back_date <- dates[indeks]
front_symbol <- symbolv[indeks-1]
back_symbol <- symbolv[indeks]
front_price <- get(x=front_symbol, envir=vix_env)
front_price <- as.numeric(Cl(front_price[todayd]))
back_price <- get(x=back_symbol, envir=vix_env)
back_price <- as.numeric(Cl(back_price[todayd]))
# Calculate the constant maturity 30-day futures price
ra_tio <- as.numeric(maturi_ty - front_date) /
  as.numeric(back_date - front_date)
pric_e <- (ra_tio*back_price + (1-ra_tio)*front_price)

x11(width=5, height=3)  # Open x11 for plotting
# Load VIX futures data from binary file
load(file="/Users/jerzy/Develop/data/vix_data/vix_cboe.RData")
# Plot VIX and SVXY data in x11 window
plot_theme <- chart_theme()
plot_theme$col$line.col <- "blue"
chart_Series(x=Cl(vix_env$vix_index["2007/"]),
       theme=plot_theme, name="VIX Index")
chart_Series(x=Cl(rutils::etfenv$VTI["2007/"]),
       theme=plot_theme, name="VTI ETF")

chart_Series(x=Cl(vix_env$vix_index["2017/2018"]),
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
sp500table <- read.csv(file="/Users/jerzy/Develop/lecture_slides/data/sp500_constituents2020.csv")
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
adj_fact <- drop(ohlc[, "ajexdi"])
tr_fact <- drop(ohlc[, "trfd"])
# Extract index of dates
dates <- drop(ohlc[, "datadate"])
dates <- lubridate::ymd(dates)
# Select only OHLCV data columns
ohlc <- ohlc[, c("prcod", "prchd", "prcld", "prccd", "cshtrd")]
colnames(ohlc) <- paste(symbol, c("Open", "High", "Low", "Close", "Volume"), sep=".")
# Coerce to xts series
ohlc <- xts::xts(ohlc, dates)
# Fill the missing (NA) Open prices
is_na <- is.na(ohlc[, 1])
ohlc[is_na, 1] <- (ohlc[is_na, 2] + ohlc[is_na, 3])/2
sum(is.na(ohlc))
# Adjust all the prices
ohlc[, 1:4] <- tr_fact*ohlc[, 1:4]/adj_fact/tr_fact[NROW(tr_fact)]
ohlc <- na.omit(ohlc)
plot(quantmod::Cl(ohlc), main="TAP Stock")

# Define formatting function for OHLC prices
format_ohlc <- function(ohlc, environ_ment) {
  # Select ticker from sp500table
  symbol <- sp500table$co_tic[match(ohlc$gvkey[1], sp500table$gvkey)]
  # Split adjustment and total return factors
  adj_fact <- drop(ohlc[, c("ajexdi")])
  tr_fact <- drop(ohlc[, "trfd"])
  tr_fact <- ifelse(is.na(tr_fact), 1, tr_fact)
  # Extract dates index
  dates <- drop(ohlc[, "datadate"])
  dates <- lubridate::ymd(dates)
  # Select only OHLCV data
  ohlc <- ohlc[, c("prcod", "prchd", "prcld", "prccd", "cshtrd")]
  colnames(ohlc) <- paste(symbol, c("Open", "High", "Low", "Close", "Volume"), sep=".")
  # Coerce to xts series
  ohlc <- xts::xts(ohlc, dates)
  # Fill NA prices
  is_na <- is.na(ohlc[, 1])
  ohlc[is_na, 1] <- (ohlc[is_na, 2] + ohlc[is_na, 3])/2
  # Adjust the prices
  ohlc[, 1:4] <- tr_fact*ohlc[, 1:4]/adj_fact/tr_fact[NROW(tr_fact)]
  # Copy the OHLCV data to environ_ment
  ohlc <- na.omit(ohlc)
  assign(x=symbol, value=ohlc, envir=environ_ment)
  symbol
}  # end format_ohlc

# Load OHLC prices from .csv file downloaded from WRDS by cusip
sp500_prices <- read.csv(file="/Users/jerzy/Develop/lecture_slides/data/sp500_prices_bycusip.csv")
# sp500_prices contains cusips not in sp500_cusips
cusips <- unique(sp500_prices$cusip)
NROW(sp500_cusips); NROW(cusips)
# Select data only for sp500_cusips
sp500_prices <- sp500_prices[sp500_prices$cusip %in% sp500_cusips, ]
# sp500_prices contains tickers not in sp500_tickers
tickers <- unique(sp500_prices$tic)
NROW(sp500_tickers); NROW(tickers)
# Select data only for sp500_tickers
sp500_prices <- sp500_prices[sp500_prices$tic %in% sp500_tickers, ]
# Create new data environment
sp500env <- new.env()
# Perform OHLC aggregations by cusip column
sp500_prices <- split(sp500_prices, sp500_prices$cusip)
process_ed <- lapply(sp500_prices, format_ohlc,
               environ_ment=sp500env)

# Get end dates of series in sp500env
endds <- eapply(sp500env, end)
endds <- unlist(endds)
endds <- as.Date(endds)
# Remove elements with short end dates
se_lect <- (endds < max(endds))
rm(list=names(sp500env)[se_lect], envir=sp500env)
# Rename element "BRK.B" to "BRKB"
sp500env$BRKB <- sp500env$BRK.B
rm(BRK.B, envir=sp500env)
names(sp500env$BRKB) <- paste("BRKB",
  c("Open", "High", "Low", "Close", "Volume"), sep=".")
# Rename element "LOW" to "LOWES"
sp500env$LOWES <- sp500env$LOW
names(sp500env$LOWES) <- paste("LO_WES",
  c("Open", "High", "Low", "Close", "Volume"), sep=".")
rm(LOW, envir=sp500env)
# Rename element "BF.B" to "BFB"
sp500env$BFB <- sp500env$BF.B
names(sp500env$BFB) <- paste("BFB",
  c("Open", "High", "Low", "Close", "Volume"), sep=".")
rm(BF.B, envir=sp500env)
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
prof_it <-
  Quandl("RAYMOND/AAPL_GROSS_PROFIT_Q", type="xts")
chart_Series(prof_it, name="AAPL gross profits")

# Download multiple time series
prices <- Quandl(code=c("NSE/OIL", "WIKI/AAPL"),
   startd="2013-01-01", type="xts")

# Download datasets for AAPL
# https://www.quandl.com/api/v3/datasets/WIKI/AAPL.json

# Download metadata for AAPL
prices <- Quandl(code=c("NSE/OIL", "WIKI/AAPL"),
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
adj_fact <- drop(ohlc[, "CFACPR"])
# Extract dates index
dates <- drop(ohlc[, "date"])
dates <- lubridate::ymd(dates)
# Select only OHLCV data
ohlc <- ohlc[, c("OPENPRC", "ASKHI", "BIDLO", "PRC", "VOL")]
colnames(ohlc) <- paste(symbol, c("Open", "High", "Low", "Close", "Volume"), sep=".")
# Coerce to xts series
ohlc <- xts::xts(ohlc, dates)
# Fill missing Open NA prices
is_na <- is.na(ohlc[, 1])
ohlc[is_na, 1] <- (ohlc[is_na, 2] + ohlc[is_na, 3])/2
# Adjust all the prices
ohlc[, 1:4] <- ohlc[, 1:4]/adj_fact/tr_fact[NROW(tr_fact)]
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
adj_fact <- drop(ohlc[, "ajexdi"])
tr_fact <- drop(ohlc[, "trfd"])
# Extract dates index
dates <- drop(ohlc[, "datadate"])
dates <- lubridate::ymd(dates)
# Select only OHLCV data
ohlc <- ohlc[, c("prcod", "prchd", "prcld", "prccd", "cshtrd")]
colnames(ohlc) <- paste(symbol, c("Open", "High", "Low", "Close", "Volume"), sep=".")
# Coerce to xts series
ohlc <- xts::xts(ohlc, dates)
# Fill missing Open NA prices
is_na <- is.na(ohlc[, 1])
ohlc[is_na, 1] <- (ohlc[is_na, 2] + ohlc[is_na, 3])/2
sum(is.na(ohlc))
# Adjust all the prices
ohlc[, 1:4] <- tr_fact*ohlc[, 1:4]/adj_fact/tr_fact[NROW(tr_fact)]
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
dates <- paste(taq$DATE, taq$TIME_M)
# Coerce date-time index to POSIXlt
dates <- strptime(dates, "%Y%m%d %H:%M:%OS")
class(dates)
# Display more significant digits
# options("digits")
options(digits=20, digits.secs=10)
last(dates)
unclass(last(dates))
as.numeric(last(dates))
# Coerce date-time index to POSIXct
dates <- as.POSIXct(dates)
class(dates)
last(dates)
unclass(last(dates))
as.numeric(last(dates))
# Calculate the number of ticks per second
n_secs <- as.numeric(last(dates)) - as.numeric(first(dates))
NROW(taq)/(6.5*3600)
# Select TAQ data columns
taq <- taq[, .(price=PRICE, volume=SIZE)]
# Add date-time index
taq <- cbind(index=dates, taq)

# Coerce trade ticks to xts series
xtes <- xts::xts(taq[, .(price, volume)], taq$index)
colnames(xtes) <- paste(symbol, c("Close", "Volume"), sep=".")
save(xtes, file="/Users/jerzy/Develop/data/xlk_tick_trades2020_0316.RData")
# save(xtes, file="/Users/jerzy/Develop/data/xlk_tick_trades2020_0316.RData")
# Plot dygraph
dygraphs::dygraph(xtes$XLK.Close,
  main="XLK Trade Ticks for 2020-03-16")
# Plot in x11 window
x11(width=6, height=5)
quantmod::chart_Series(x=xtes$XLK.Close,
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
xtes <- xts::xts(big_ticks[, .(price, volume)], big_ticks$index)
colnames(xtes) <- c("XLK.Close", "XLK.Volume")

# Plot dygraph of the large lots
dygraphs::dygraph(xtes$XLK.Close,
  main="XLK Trade Ticks for 2020-03-16 (large lots only)")
# Plot the large lots
x11(width=6, height=5)
quantmod::chart_Series(x=xtes$XLK.Close,
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
xtes <- xts::xts(ohlc[, -"index"], ohlc$index)
# Plot dygraph of the OHLC prices
dygraphs::dygraph(xtes[, -5], main="XLK Trade Ticks for 2020-03-16 (OHLC)") %>%
  dyCandlestick()
# Plot the OHLC prices
x11(width=6, height=5)
quantmod::chart_Series(x=xtes, TA="add_Vo()",
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
# SPY percentage returns
ohlc <- HighFreq::SPY
nrows <- NROW(ohlc)
closep <- log(quantmod::Cl(ohlc))
returns <- rutils::diffit(closep)
colnames(returns) <- "SPY"
# Standardize raw returns to make later comparisons
returns <- (returns - mean(returns))/sd(returns)
# Calculate moments and perform normality test
sapply(c(var=2, skew=3, kurt=4), function(x) sum(returns^x)/nrows)
tseries::jarque.bera.test(returns)
# Fit SPY returns using MASS::fitdistr()
optim_fit <- MASS::fitdistr(returns, densfun="t", df=2)
loc <- optim_fit$estimate[1]
scalev <- optim_fit$estimate[2]

x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(1, 1, 1, 1))
# Plot histogram of SPY returns
histp <- hist(returns, col="lightgrey", mgp=c(2, 1, 0),
  xlab="returns (standardized)", ylab="frequency", xlim=c(-3, 3),
  breaks=1e3, freq=FALSE, main="Distribution of High Frequency SPY Returns")
# lines(density(returns, bw=0.2), lwd=3, col="blue")
# Plot t-distribution function
curve(expr=dt((x-loc)/scalev, df=2)/scalev,
type="l", lwd=3, col="red", add=TRUE)
# Plot the Normal probability distribution
curve(expr=dnorm(x, mean=mean(returns),
  sd=sd(returns)), add=TRUE, lwd=3, col="blue")
# Add legend
legend("topright", inset=0.05, bty="n",
  leg=c("t-distr", "normal"),
  lwd=6, lty=1, col=c("red", "blue"))

# Hourly SPY percentage returns
closep <- log(Cl(xts::to.period(x=ohlc, period="hours")))
hourlly <- rutils::diffit(closep)
hourlly <- (hourlly - mean(hourlly))/sd(hourlly)
# Daily SPY percentage returns
closep <- log(Cl(xts::to.period(x=ohlc, period="days")))
dai_ly <- rutils::diffit(closep)
dai_ly <- (dai_ly - mean(dai_ly))/sd(dai_ly)
# Calculate moments
sapply(list(minutely=returns, hourly=hourlly, daily=dai_ly),
 function(rets) {sapply(c(var=2, skew=3, kurt=4),
          function(x) mean(rets^x))
})  # end sapply

x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(1, 1, 1, 1))
# Plot densities of SPY returns
plot(density(returns, bw=0.4), xlim=c(-3, 3),
     lwd=3, mgp=c(2, 1, 0), col="blue",
     xlab="returns (standardized)", ylab="frequency",
     main="Density of High Frequency SPY Returns")
lines(density(hourlly, bw=0.4), lwd=3, col="green")
lines(density(dai_ly, bw=0.4), lwd=3, col="red")
# Add legend
legend("topright", inset=0.05, bty="n",
  leg=c("minutely", "hourly", "daily"),
  lwd=6, lty=1, col=c("blue", "green", "red"))

# Calculate rolling volatility of SPY returns
ret2013 <- returns["2013-11-11/2013-11-15"]
# Calculate rolling volatility
look_back <- 11
endp <- seq_along(ret2013)
startp <- c(rep_len(1, look_back-1),
  endp[1:(NROW(endp)-look_back+1)])
endp[endp < look_back] <- look_back
vol_rolling <- sapply(seq_along(endp),
  function(it) sd(ret2013[startp[it]:endp[it]]))
vol_rolling <- xts::xts(vol_rolling, zoo::index(ret2013))
# Extract time intervals of SPY returns
indeks <- c(60, diff(xts::.zoo::index(ret2013)))
head(indeks)
table(indeks)
# Scale SPY returns by time intervals
ret2013 <- 60*ret2013/indeks
# Calculate scaled rolling volatility
vol_scaled <- sapply(seq_along(endp),
  function(it) sd(ret2013[startp[it]:endp[it]]))
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
  inset=0.1, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")

prices <- read.zoo(file="/Users/jerzy/Develop/lecture_slides/data/bid_ask_bounce.csv",
  header=TRUE, sep=",")
prices <- as.xts(prices)
x11(width=6, height=4)
par(mar=c(2, 2, 0, 0), oma=c(1, 1, 0, 0))
chart_Series(x=prices, name="S&P500 Futures Bid-Ask Bounce")

# Volatility of SPY
sqrt(HighFreq::calcvar_ohlc(ohlc))
# Daily SPY volatility and volume
vol_daily <- sqrt(xts::apply.daily(ohlc, FUN=calcvar_ohlc))
colnames(vol_daily) <- ("SPY_volatility")
volumes <- quantmod::Vo(ohlc)
volume_daily <- xts::apply.daily(volumes, FUN=sum)
colnames(volume_daily) <- ("SPY_volume")
# Plot SPY volatility and volume
datav <- cbind(vol_daily, volume_daily)["2008/2009"]
colnamev <- colnames(datav)
dygraphs::dygraph(datav,
  main="SPY Daily Volatility and Trading Volume") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="red", strokeWidth=3) %>%
  dySeries(name=colnamev[2], axis="y2", col="blue", strokeWidth=3)

# Regress log of daily volume vs volatility
datav <- log(cbind(volume_daily, vol_daily))
colnamev <- colnames(datav)
data_frame <- as.data.frame(datav)
formulav <- as.formula(paste(colnamev, collapse="~"))
model <- lm(formulav, data=data_frame)
# Durbin-Watson test for autocorrelation of residuals
lmtest::dwtest(model)
# Regress diff log of daily volume vs volatility
data_frame <- as.data.frame(rutils::diffit(datav))
model <- lm(formulav, data=data_frame)
lmtest::dwtest(model)
summary(model)
plot(formulav, data=data_frame, main="SPY Daily Trading Volume vs Volatility (log scale)")
abline(model, lwd=3, col="red")
mtext(paste("beta =", round(coef(model)[2], 3)), cex=1.2, lwd=3, side=2, las=2, adj=(-0.5), padj=(-7))

# 60 minutes of data in look_back interval
look_back <- 60
vol2013 <- volumes["2013"]
ret2013 <- returns["2013"]
# Define end points with beginning stub
nrows <- NROW(ret2013)
nagg <- nrows %/% look_back
endp <- nrows-look_back*nagg + (0:nagg)*look_back
startp <- c(1, endp[1:(NROW(endp)-1)])
# Calculate SPY volatility and volume
datav <- sapply(seq_along(endp), function(it) {
  point_s <- startp[it]:endp[it]
  c(volume=sum(vol2013[point_s]),
    volatility=sd(ret2013[point_s]))
})  # end sapply
datav <- t(datav)
datav <- rutils::diffit(log(datav))
data_frame <- as.data.frame(datav)

formulav <- as.formula(paste(colnames(datav), collapse="~"))
model <- lm(formulav, data=data_frame)
lmtest::dwtest(model)
summary(model)
plot(formulav, data=data_frame,
     main="SPY Hourly Trading Volume vs Volatility (log scale)")
abline(model, lwd=3, col="red")
mtext(paste("beta =", round(coef(model)[2], 3)), cex=1.2, lwd=3, side=2, las=2, adj=(-0.5), padj=(-7))

# Scale returns using volume (volume clock)
rets_scaled <- ifelse(volumes > 1e4, returns/volumes, 0)
rets_scaled <- rets_scaled/sd(rets_scaled)
# Calculate moments of scaled returns
nrows <- NROW(returns)
sapply(list(returns=returns, rets_scaled=rets_scaled),
  function(rets) {sapply(c(skew=3, kurt=4),
     function(x) sum((rets/sd(rets))^x)/nrows)
})  # end sapply

x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(1, 1, 1, 1))
# Plot densities of SPY returns
plot(density(returns), xlim=c(-3, 3),
     lwd=3, mgp=c(2, 1, 0), col="blue",
     xlab="returns (standardized)", ylab="frequency",
     main="Density of Volume-scaled High Frequency SPY Returns")
lines(density(rets_scaled, bw=0.4), lwd=3, col="red")
curve(expr=dnorm, add=TRUE, lwd=3, col="green")
# Add legend
legend("topright", inset=0.05, bty="n",
  leg=c("minutely", "scaled", "normal"),
  lwd=6, lty=1, col=c("blue", "red", "green"))

# Ljung-Box test for minutely SPY returns
Box.test(returns, lag=10, type="Ljung")
# Ljung-Box test for daily SPY returns
Box.test(dai_ly, lag=10, type="Ljung")
# Ljung-Box test statistics for scaled SPY returns
sapply(list(returns=returns, rets_scaled=rets_scaled),
  function(rets) {
    Box.test(rets, lag=10, type="Ljung")$statistic
})  # end sapply
# Ljung-Box test statistics for aggregated SPY returns
sapply(list(minutely=returns, hourly=hourlly, daily=dai_ly),
  function(rets) {
    Box.test(rets, lag=10, type="Ljung")$statistic
})  # end sapply

# Set plot parameters
x11(width=6, height=8)
par(mar=c(4, 4, 2, 1), oma=c(0, 0, 0, 0))
layout(matrix(c(1, 2), ncol=1), widths=c(6, 6), heights=c(4, 4))
# Plot the partial autocorrelations of minutely SPY returns
pa_cf <- pacf(as.numeric(returns), lag=10,
     xlab="lag", ylab="partial autocorrelation", main="")
title("Partial Autocorrelations of Minutely SPY Returns", line=1)
# Plot the partial autocorrelations of scaled SPY returns
pacf_scaled <- pacf(as.numeric(rets_scaled), lag=10,
     xlab="lag", ylab="partial autocorrelation", main="")
title("Partial Autocorrelations of Scaled SPY Returns", line=1)
# Calculate the sums of partial autocorrelations
sum(pa_cf$acf)
sum(pacf_scaled$acf)

# Calculate market illiquidity
liquidi_ty <- sqrt(volume_daily)/vol_daily
# Plot market illiquidity
x11(width=6, height=7) ; par(mfrow=c(2, 1))
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("blue")
chart_Series(liquidi_ty["2010"], theme=plot_theme,
  name="SPY Liquidity in 2010", plot=FALSE)
plot_theme$col$line.col <- c("red")
chart_Series(vol_daily["2010"],
  theme=plot_theme, name="SPY Volatility in 2010")

# Calculate intraday time index with hours and minutes
dates <- format(zoo::index(returns), "%H:%M")
# Aggregate the mean volume
volume_agg <- tapply(X=volumes, INDEX=dates, FUN=mean)
volume_agg <- drop(volume_agg)
# Aggregate the mean volatility
vol_agg <- tapply(X=returns^2, INDEX=dates, FUN=mean)
vol_agg <- sqrt(drop(vol_agg))
# Coerce to xts
intra_day <- as.POSIXct(paste(Sys.Date(), names(volume_agg)))
volume_agg <- xts::xts(volume_agg, intra_day)
vol_agg <- xts::xts(vol_agg, intra_day)
# Plot seasonality of volume and volatility
x11(width=6, height=7) ; par(mfrow=c(2, 1))
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("blue")
chart_Series(volume_agg[c(-1, -NROW(volume_agg))], theme=plot_theme,
  name="Daily Seasonality of SPY Volume", plot=FALSE)
plot_theme$col$line.col <- c("red")
chart_Series(vol_agg[c(-1, -NROW(vol_agg))], theme=plot_theme,
  name="Daily Seasonality of SPY Volatility")

# Calculate market liquidity
liquidi_ty <- sqrt(volume_agg)/vol_agg
# Plot daily seasonality of market liquidity
x11(width=6, height=7) ; par(mfrow=c(2, 1))
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("blue")
chart_Series(liquidi_ty[c(-1, -NROW(liquidi_ty))], theme=plot_theme,
  name="Daily Seasonality of SPY Liquidity", plot=FALSE)
plot_theme$col$line.col <- c("red")
chart_Series(vol_agg[c(-1, -NROW(vol_agg))], theme=plot_theme,
  name="Daily Seasonality of SPY Volatility")

par(mfrow=c(2,1))  # set plot panels
library(rutils)  # Load package rutils
chart_Series(roll_sum(vol_daily, 10)[-(1:10)]/10,
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
vol_daily <- xts::apply.daily(x=HighFreq::SPY, FUN=agg_ohlc,
                  agg_fun="vol_ohlc")
colnames(vol_daily) <- paste0(symbol, ".var")
daily_skew <- xts::apply.daily(x=HighFreq::SPY, FUN=agg_ohlc,
                  agg_fun="skew_ohlc")
daily_skew <- daily_skew/(vol_daily)^(1.5)
colnames(daily_skew) <- paste0(symbol, ".skew")
interval <- "2013-06-01/"
chart_Series(vol_daily[interval],
       name=paste(symbol, "variance"))
chart_Series(daily_skew[interval],
       name=paste(symbol, "skew"))

# skew scatterplot
returns <- calc_rets(xts_data=SPY)
skew <- skew_ohlc(log_ohlc=log(SPY[, -5]))
colnames(skew) <- paste0(symbol, ".skew")
lag_skew <- lag(skew)
lag_skew[1, ] <- 0
datav <- cbind(returns[, 1], sign(lag_skew))
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
position_s <- -sign(lag_skew)
position_s[1, ] <- 0
# Cumulative PnL
cumu_pnl <- cumsum(position_s*returns[, 1])
# Calculate frequency of trades
50*sum(abs(sign(skew)-sign(lag_skew)))/nrow(skew)
# Calculate transaction costs
bid_offer <- 0.001  # 10 bps for liquid ETFs
bid_offer*sum(abs(sign(skew)-sign(lag_skew)))

chart_Series(
  cumu_pnl[endpoints(cumu_pnl, on="hours"), ],
  name=paste(symbol, "contrarian skew strategy pnl"))

# vwap plot
vwap_short <- vwapv(xtes=SPY, look_back=70)
vwap_long <- vwapv(xtes=SPY, look_back=225)
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
# returns <- calc_rets(xts_data=SPY)
vwap_short <- vwapv(xtes=SPY, look_back=70)
vwap_long <- vwapv(xtes=SPY, look_back=225)
vwap_diff <- vwap_short - vwap_long
colnames(vwap_diff) <- paste0(symbol, ".vwap")
lag_vwap <- lag(vwap_diff)
lag_vwap[1, ] <- 0
datav <- cbind(returns[, 1], sign(lag_vwap))
formulav <- as.formula(paste(colnames(datav)[1],
    paste(paste(colnames(datav)[-1],
      collapse=" + "), "- 1"), sep="~"))
formulav
model <- lm(formulav, data=datav)
summary(model)$coef
summary(lm(formulav, data=datav["/2011-01-01"]))$coef
summary(lm(formulav, data=datav["2011-01-01/"]))$coef

interval <- "2013-12-01/"
plot(formulav, data=cbind(returns[, 1], lag_vwap)[interval],
     cex=0.6, xlab="skew", ylab="rets")
abline(model, col="blue", lwd=2)

# Trend following trading strategy
# Cumulative PnL
cumu_pnl <- cumsum(sign(lag_vwap)*returns[, 1])
# Calculate frequency of trades
50*sum(abs(sign(vwap_diff)-sign(lag_vwap)))/nrow(vwap_diff)
# Calculate transaction costs
bid_offer <- 0.001  # 10 bps for liquid ETFs
bid_offer*sum(abs(sign(vwap_diff)-sign(lag_vwap)))

chart_Series(
  cumu_pnl[endpoints(cumu_pnl, on="hours"), ],
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
con_tract <- IBrokers::twsEquity("AAPL", primary="SMART")
# Define CHF currency contract
con_tract <- IBrokers::twsCurrency("CHF", currency="USD")
# Define S&P Emini future June 2019 contract
con_tract <- IBrokers::twsFuture(symbol="ES",
  exch="GLOBEX", expiry="201906")
# Define 10yr Treasury future June 2019 contract
con_tract <- IBrokers::twsFuture(symbol="ZN",
  exch="ECBOT", expiry="201906")
# Define euro currency future June 2019 contract
con_tract <- IBrokers::twsFuture(symbol="EUR",
  exch="GLOBEX", expiry="201906")
# Define Gold future June 2019 contract
con_tract <- IBrokers::twsFuture(symbol="GC",
  exch="NYMEX", expiry="201906")
# Define Oil future January 2019 contract
con_tract <- IBrokers::twsFuture(symbol="QM",
  exch="NYMEX", expiry="201901")
# Test if contract object is correct
IBrokers::is.twsContract(con_tract)
# Get list with instrument information
IBrokers::reqContractDetails(conn=ib_connect, Contract=con_tract)
# Install the package twsInstrument
install.packages("twsInstrument", repos="http://r-forge.r-project.org")
# Define euro future using getContract() and Conid
con_tract <- twsInstrument::getContract("317631411")
# Get list with instrument information
IBrokers::reqContractDetails(conn=ib_connect, Contract=con_tract)

# Define VIX monthly and weekly futures June 2019 contract
symbol <- "VIX"
con_tract <- IBrokers::twsFuture(symbol=symbol,
  exch="CFE", expiry="201906")
# Define VIX monthly futures June 2019 contract
con_tract <- IBrokers::twsFuture(symbol=symbol,
  local="VXV8", exch="CFE", expiry="201906")
# Define VIX weekly futures October 3rd 2018 contract
con_tract <- IBrokers::twsFuture(symbol=symbol,
  local="VX40V8", exch="CFE", expiry="201906")
# Get list with instrument information
IBrokers::reqContractDetails(conn=ib_connect,
  Contract=con_tract)

# Define S&P Emini futures June 2019 contract
symbol <- "ES"
con_tract <- IBrokers::twsFuture(symbol=symbol,
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
  Contract=con_tract,
  barSize="1 day", duration="6 M",
  file=file_connect)
# Close data file
close(file_connect)
# Close the Interactive Brokers API connection
IBrokers::twsDisconnect(ib_connect)

# Define IB contract objects for stock symbols
symbolv <- c("AAPL", "F", "MSFT")
con_tracts <- lapply(symbolv, IBrokers::twsEquity, primary="SMART")
names(con_tracts) <- symbolv
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
  con_tract <- con_tracts[[it]]
  cat("Downloading data for: ", symbol, "\n")
  # Write header to file
  cat(paste(paste(symbol, c("Index", "Open", "High", "Low", "Close", "Volume", "WAP", "XTRA", "Count"), sep="."), collapse=","), "\n", file=file_connect)
  IBrokers::reqHistoricalData(conn=ib_connect,
                         Contract=con_tract,
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
con_tract <- IBrokers::twsFuture(symbol=symbol,
  include_expired="1",
  exch="GLOBEX", expiry="201806")
# Open file connection for ESM8 data download
file_name <- file.path(dir_name, paste0(symbol, "M8.csv"))
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
prices <- data.table::fread(file_name)
data.table::setDF(prices)
prices <- xts::xts(prices[, 2:6],
  order.by=as.Date(as.POSIXct.numeric(prices[, 1],
    tz="America/New_York", origin="1970-01-01")))
colnames(prices) <- c("Open", "High", "Low", "Close", "Volume")
# Plot OHLC data in x11 window
chart_Series(x=prices, TA="add_Vo()",
  name="S&P500 ESM8 futures")
# Plot dygraph
dygraphs::dygraph(prices[, 1:4], main="S&P500 ESM8 futures") %>%
  dyCandlestick()

# Define S&P Emini futures June 2018 contract
symbol <- "ES"
con_tract <- IBrokers::twsFuture(symbol=symbol,
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
  Contract=con_tract,
  barSize="1 day", duration="6 M",
  file=file_connect)
# Close data file
close(file_connect)
# Close the Interactive Brokers API connection
IBrokers::twsDisconnect(ib_connect)

# Define S&P Emini futures June 2019 contract
symbol <- "ES"
con_tract <- IBrokers::twsFuture(symbol=symbol,
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
     Contract=con_tract,
     eventWrapper=eWrapper.MktData.CSV(1),
     file=file_connect)
# Close data file
close(file_connect)
# Close the Interactive Brokers API connection
IBrokers::twsDisconnect(ib_connect)

# Define S&P Emini futures June 2019 contract
symbol <- "ES"
con_tract <- IBrokers::twsFuture(symbol=symbol,
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
     Contract=con_tract, barSize="1",
     eventWrapper=eWrapper.RealTimeBars.CSV(1),
     file=file_connect)
# Close the Interactive Brokers API connection
IBrokers::twsDisconnect(ib_connect)
# Close data file
close(file_connect)
# Load OHLC data and coerce it into xts series
library(data.table)
prices <- data.table::fread(file_name)
prices <- xts::xts(prices[, paste0("V", 2:6)],
  as.POSIXct.numeric(as.numeric(prices[, V1]), tz="America/New_York", origin="1970-01-01"))
colnames(prices) <- c("Open", "High", "Low", "Close", "Volume")
# Plot OHLC data in x11 window
x11()
chart_Series(x=prices, TA="add_Vo()",
       name="S&P500 ESM9 futures")
# Plot dygraph
library(dygraphs)
dygraphs::dygraph(prices[, 1:4], main="S&P500 ESM9 futures") %>%
  dyCandlestick()

library(IBrokers)
# Define list of S&P futures and 10yr Treasury contracts
con_tracts <- list(ES=IBrokers::twsFuture(symbol="ES", exch="GLOBEX", expiry="201906"),
             ZN=IBrokers::twsFuture(symbol="ZN", exch="ECBOT", expiry="201906"))
# Open the file connection for storing the bar data
dir_name <- "/Users/jerzy/Develop/data/ib_data"
file_names <- file.path(dir_name, paste0(c("ES", "ZN_"), format(Sys.time(), format="%m_%d_%Y_%H_%M"), ".csv"))
file_connects <- lapply(file_names, function(file_name) file(file_name, open="w"))
# Connect to Interactive Brokers TWS
ib_connect <- IBrokers::twsConnect(port=7497)
# Download live data to file
IBrokers::reqRealTimeBars(conn=ib_connect,
                    Contract=con_tracts,
                    barSize="1", useRTH=FALSE,
                    eventWrapper=eWrapper.RealTimeBars.CSV(NROW(con_tracts)),
                    file=file_connects)
# Close the Interactive Brokers API connection
IBrokers::twsDisconnect(ib_connect)
# Close data files
for (file_connect in file_connects)
  close(file_connect)
library(data.table)
# Load ES futures June 2019 contract and coerce it into xts series
prices <- data.table::fread(file_names[1])
prices <- xts::xts(prices[, paste0("V", 2:6)],
  as.POSIXct.numeric(as.numeric(prices[, V1]), tz="America/New_York", origin="1970-01-01"))
colnames(prices) <- c("Open", "High", "Low", "Close", "Volume")
# Plot dygraph
library(dygraphs)
dygraphs::dygraph(prices[, 1:4], main="S&P500 ESM9 futures") %>%
  dyCandlestick()
# Load ZN 10yr Treasury futures June 2019 contract
prices <- data.table::fread(file_names[2])
prices <- xts::xts(prices[, paste0("V", 2:6)],
  as.POSIXct.numeric(as.numeric(prices[, V1]), tz="America/New_York", origin="1970-01-01"))
colnames(prices) <- c("Open", "High", "Low", "Close", "Volume")
# Plot dygraph
dygraphs::dygraph(prices[, 1:4], main="ZN 10yr Treasury futures") %>%
  dyCandlestick()

# Define S&P Emini future June 2019 contract
con_tract <- IBrokers::twsFuture(symbol="ES", exch="GLOBEX", expiry="201906")
# Define euro currency contract EUR.USD
con_tract <- IBrokers::twsCurrency("EUR", currency="USD")
# Define euro currency E-mini futures June 2019 contract E7Z8
con_tract <- IBrokers::twsFuture(symbol="E7", exch="GLOBEX", expiry="201906")
# Define Japanese yen currency contract JPY.USD
con_tract <- IBrokers::twsCurrency("JPY", currency="USD")
# Define Japanese yen currency E-mini futures June 2019 contract J7Z8
con_tract <- IBrokers::twsFuture(symbol="J7", exch="GLOBEX", expiry="201906")
# Define Japanese yen currency futures June 2019 contract 6JZ8
con_tract <- IBrokers::twsFuture(symbol="JPY", exch="GLOBEX", expiry="201906")
# Connect to Interactive Brokers TWS
ib_connect <- IBrokers::twsConnect(port=7497)
IBrokers::reqContractDetails(conn=ib_connect, Contract=con_tract)
# Request trade order ID
order_id <- IBrokers::reqIds(ib_connect)
# Create buy market order object
ib_order <- IBrokers::twsOrder(order_id,
  orderType="MKT", action="BUY", totalQuantity=1)
# Place trade order
IBrokers::placeOrder(ib_connect, con_tract, ib_order)
# Execute sell market order
order_id <- IBrokers::reqIds(ib_connect)
ib_order <- IBrokers::twsOrder(order_id,
  orderType="MKT", action="SELL", totalQuantity=1)
IBrokers::placeOrder(ib_connect, con_tract, ib_order)
# Execute buy market order
order_id <- IBrokers::reqIds(ib_connect)
ib_order <- IBrokers::twsOrder(order_id,
  orderType="MKT", action="BUY", totalQuantity=1)
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
con_tract <- IBrokers::twsEquity("AAPL", primary="SMART")
# Define CHF currency contract
con_tract <- IBrokers::twsCurrency("CHF", currency="USD")
# Define S&P Emini future June 2019 contract
con_tract <- IBrokers::twsFuture(symbol="ES",
  exch="GLOBEX", expiry="201906")
# Define 10yr Treasury future June 2019 contract
con_tract <- IBrokers::twsFuture(symbol="ZN",
  exch="ECBOT", expiry="201906")
# Define euro currency future June 2019 contract
con_tract <- IBrokers::twsFuture(symbol="EUR",
  exch="GLOBEX", expiry="201906")
# Define Gold future June 2019 contract
con_tract <- IBrokers::twsFuture(symbol="GC",
  exch="NYMEX", expiry="201906")
# Test if contract object is correct
IBrokers::is.twsContract(con_tract)
# Get list with instrument information
IBrokers::reqContractDetails(conn=ib_connect, Contract=con_tract)
# Install the package twsInstrument
install.packages("twsInstrument", repos="http://r-forge.r-project.org")
# Define euro future using getContract() and Conid
con_tract <- twsInstrument::getContract("317631411")
# Get list with instrument information
IBrokers::reqContractDetails(conn=ib_connect, Contract=con_tract)

# Define AAPL stock contract (object)
con_tract <- IBrokers::twsEquity("AAPL", primary="SMART")
# Define CHF currency contract
con_tract <- IBrokers::twsCurrency("CHF", currency="USD")
# Define S&P Emini future June 2019 contract
con_tract <- IBrokers::twsFuture(symbol="ES",
  exch="GLOBEX", expiry="201906")
# Define 10yr Treasury future June 2019 contract
con_tract <- IBrokers::twsFuture(symbol="ZN",
  exch="ECBOT", expiry="201906")
# Define euro currency future June 2019 contract
con_tract <- IBrokers::twsFuture(symbol="EUR",
  exch="GLOBEX", expiry="201906")
# Define Gold future June 2019 contract
con_tract <- IBrokers::twsFuture(symbol="GC",
  exch="NYMEX", expiry="201906")
# Test if contract object is correct
IBrokers::is.twsContract(con_tract)
# Get list with instrument information
IBrokers::reqContractDetails(conn=ib_connect, Contract=con_tract)
# Install the package twsInstrument
install.packages("twsInstrument", repos="http://r-forge.r-project.org")
# Define euro future using getContract() and Conid
con_tract <- twsInstrument::getContract("317631411")
# Get list with instrument information
IBrokers::reqContractDetails(conn=ib_connect, Contract=con_tract)
