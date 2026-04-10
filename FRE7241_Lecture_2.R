# Calculate the positions
retp <- na.omit(rutils::etfenv$returns$VTI)
posv <- rep(NA_integer_, NROW(retp))
datev <- zoo::index(retp)
datev <- format(datev, "%m-%d")
posv[datev == "05-01"] <- 0
posv[datev == "05-03"] <- 0
posv[datev == "11-01"] <- 1
posv[datev == "11-03"] <- 1
# Carry forward and backward non-NA posv
posv <- zoo::na.locf(posv, na.rm=FALSE)
posv <- zoo::na.locf(posv, fromLast=TRUE)
# Calculate the strategy returns
pnlmay <- posv*retp
wealthv <- cbind(retp, pnlmay)
colnames(wealthv) <- c("VTI", "sellmay")
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))

# Plot wealth of Sell in May strategy
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw], main="Sell in May Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# OR: Open x11 for plotting
x11(width=6, height=5)
par(mar=c(4, 4, 3, 1), oma=c(0, 0, 0, 0))
themev <- chart_theme()
themev$col$line.col <- c("blue", "red")
quantmod::chart_Series(wealthv, theme=themev, name="Sell in May Strategy")
legend("topleft", legend=colnames(wealthv),
  inset=0.1, bg="white", lty=1, lwd=6, y.intersp=0.5,
  col=themev$col$line.col, bty="n")

# Test if Sell in May strategy can time VTI
desm <- cbind(wealthv, 0.5*(retp+abs(retp)), retp^2)
colnames(desm) <- c(colnames(wealthv), "Merton", "Treynor")
# Perform Merton-Henriksson test
regmod <- lm(sellmay ~ VTI + Merton, data=desm)
summary(regmod)
# Perform Treynor-Mazuy test
regmod <- lm(sellmay ~ VTI + Treynor, data=desm)
summary(regmod)
# Plot Treynor-Mazuy residual scatterplot
resids <- (desm$sellmay - regmod$coeff["VTI"]*retp)
plot.default(x=retp, y=resids, xlab="VTI", ylab="residuals")
title(main="Treynor-Mazuy Market Timing Test\n for Sell in May vs VTI", line=0.5)
# Plot fitted (predicted) response values
coefreg <- summary(regmod)$coeff
fitv <- regmod$fitted.values - coefreg["VTI", "Estimate"]*retp
tvalue <- round(coefreg["Treynor", "t value"], 2)
points.default(x=retp, y=fitv, pch=16, col="red")
text(x=0.0, y=0.8*max(resids), paste("Treynor test t-value =", tvalue))

# Calculate the log of OHLC VTI prices
ohlc <- log(rutils::etfenv$VTI)
openp <- quantmod::Op(ohlc)
highp <- quantmod::Hi(ohlc)
lowp <- quantmod::Lo(ohlc)
closep <- quantmod::Cl(ohlc)
# Calculate the close-to-close log returns,
# the daytime open-to-close returns
# and the overnight close-to-open returns.
retp <- rutils::diffit(closep)
colnames(retp) <- "daily"
retd <- (closep - openp)
colnames(retd) <- "daytime"
reton <- (openp - rutils::lagit(closep, lagg=1, pad_zeros=FALSE))
colnames(reton) <- "overnight"

# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retp, reton, retd)
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot the log wealth
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Wealth of Close-to-Close, Overnight, and Daytime Strategies") %>%
  dySeries(name="daily", strokeWidth=2, col="blue") %>%
  dySeries(name="overnight", strokeWidth=2, col="red") %>%
  dySeries(name="daytime", strokeWidth=2, col="green") %>%
  dyLegend(width=500)

# Calculate the VTI returns
retp <- na.omit(rutils::etfenv$returns$VTI)
datev <- zoo::index(retp)
# Calculate the first business day of every month
dayv <- as.numeric(format(datev, "%d"))
indeks <- which(rutils::diffit(dayv) < 0)
datev[head(indeks)]
# Calculate the Turn of the Month dates
indeks <- lapply((-1):2, function(x) indeks + x)
indeks <- do.call(c, indeks)
sum(indeks > NROW(datev))
indeks <- sort(indeks)
datev[head(indeks, 11)]
# Calculate the Turn of the Month pnls
pnls <- numeric(NROW(retp))
pnls[indeks] <- retp[indeks, ]

# Combine data
wealthv <- cbind(retp, pnls)
colv <- c("VTI", "TOM Strategy")
colnames(wealthv) <- colv
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# dygraph plot VTI Turn of the Month strategy
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Turn of the Month Strategy") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=colv[2], axis="y2", strokeWidth=2, col="red")

# Calculate the log VTI prices and percentage returns
pricev <- log(na.omit(rutils::etfenv$prices$VTI))
nrows <- NROW(pricev)
datev <- zoo::index(pricev)
retp <- rutils::diffit(pricev)
# Simulate stop-loss strategy
stopl <- 0.05 # Stop-loss percentage
pricem <- cummax(pricev) # Trailing maximum prices
# Calculate the drawdown
dd <- (pricev - pricem)
pnls <- retp # Initialize PnLs
for (i in 1:(nrows-1)) {
# Check for stop-loss
  if (dd[i] < -stopl*pricem[i])
    pnls[i+1] <- 0 # Set PnLs = 0 if in stop-loss
}  # end for
# Same but without using loops in R
pnl2 <- retp
insl <- rutils::lagit(dd < -stopl*pricem)
pnl2 <- ifelse(insl, 0, pnl2)
all.equal(pnls, pnl2, check.attributes=FALSE)

# Combine the data
wealthv <- cbind(retp, pnls)
colv <- c("VTI", "Strategy")
colnames(wealthv) <- colv
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# dygraph plot the stop-loss strategy
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="VTI Stop-loss Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Plot the stop-loss strategy with shading
indic <- (rutils::diffit(insl) != 0) # Indices of stop-loss
crossd <- c(datev[indic], datev[nrows]) # Dates of stop-loss
shadev <- ifelse(insl[indic] == 1, "antiquewhite", "lightgreen")
# Create dygraph object without plotting it
dyplot <- dygraphs::dygraph(cumsum(wealthv),
  main="VTI Stop-loss Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Add shading to dygraph object
for (i in 1:NROW(shadev)) {
  dyplot <- dyplot %>% dyShading(from=crossd[i], to=crossd[i+1], color=shadev[i])
}  # end for
# Plot the dygraph object
dyplot

# Simulate multiple stop-loss strategies
dd <- (pricev - pricem)
stopv <- 0.01*(1:30)
pnlc <- sapply(stopv, function(stopl) {
  pnls <- retp
  insl <- rutils::lagit(dd < -stopl*pricem)
  pnls <- ifelse(insl, 0, pnls)
  sum(pnls)
})  # end sapply

# Plot cumulative PnLs of stop-loss strategies
plot(x=stopv, y=pnlc,
   main="Cumulative PnLs of Stop-loss Strategies",
   xlab="stop-loss percent", ylab="cumulative pnl",
   t="l", lwd=3, col="blue")

# Function for simulating the stop-start strategy
stopstart <- function(stopl) {
  maxp <- pricev[1] # Trailing maximum price
  minp <- pricev[1] # Trailing minimum price
  insl <- FALSE # Is in stop-loss?
  insg <- FALSE # Is in start-gain?
  pnls <- retp # Initialize PnLs
  for (i in 1:nrows) {
    if (insl) { # In stop-loss
pnls[i] <- 0 # Set PnLs = 0 if in stop-loss
minp <- min(minp, pricev[i]) # Update minimum price to current price
if (pricev[i] > (1 + stopl)*minp) { # Check for start-gain
  insg <- TRUE # Is in start-gain?
  insl <- FALSE # Is in stop-loss?
  maxp <- pricev[i] # Reset trailing maximum price
}  # end if
    } else if (insg) { # In start-gain
maxp <- max(maxp, pricev[i]) # Update maximum price to current price
if (pricev[i] < (1 - stopl)*maxp) { # Check for stop-loss
  insl <- TRUE # Is in stop-loss?
  insg <- FALSE # Is in start-gain?
  minp <- pricev[i] # Reset trailing minimum price
}  # end if
    } else { # Warmup period
# Update the maximum and minimum prices
maxp <- max(maxp, pricev[i])
minp <- min(minp, pricev[i])
# Update the stop-loss and start-gain indicators
insl <- (pricev[i] < (1 - stopl)*maxp) # Is in stop-loss?
insg <- (pricev[i] > (1 + stopl)*minp) # Is in start-gain?
    }  # end if
  }  # end for
  return(pnls)
} # end stopstart

# Simulate the stop-start strategy
pnls <- stopstart(0.07)
# Combine the data
wealthv <- cbind(retp, pnls)
colv <- c("VTI", "Strategy")
colnames(wealthv) <- colv
# Has low correlation to VTI
cor(wealthv)[1, 2]
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# dygraph plot the stop-loss strategy
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="VTI Stop-Start Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Plot the stop-start strategy with shading
insl <- (pnls == 0) # Is in stop-loss?
indic <- (rutils::diffit(insl) != 0) # Indices of crosses
crossd <- c(datev[indic], datev[nrows]) # Dates of crosses
shadev <- ifelse(insl[indic] == 1, "antiquewhite", "lightgreen")
# Create dygraph object without plotting it
dyplot <- dygraphs::dygraph(cumsum(wealthv),
  main="VTI Stop-Start Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Add shading to dygraph object
for (i in 1:NROW(shadev)) {
  dyplot <- dyplot %>% dyShading(from=crossd[i], to=crossd[i+1], color=shadev[i])
}  # end for
# Plot the dygraph object
dyplot

# Simulate multiple stop-loss strategies in a loop (slow)
stopv <- 0.01*(1:30)
# pnlc <- sapply(stopv, function(stopl) {
#   sum(stopstart(stopl))
# })  # end sapply
# Simulate stop-loss strategies in a parallel loop (faster)
library(parallel)
ncores <- detectCores()-1
pnlc <- unlist(mclapply(stopv, function(stopl) {
  sum(stopstart(stopl))
}, mc.cores=ncores))  # end mclapply
# Find optimal stop-loss level
stopl <- stopv[which.max(pnlc)]

# Plot cumulative PnLs of stop-loss strategies
plot(x=stopv, y=pnlc,
   main="Cumulative PnLs of Stop-Start Strategies",
   xlab="stop-loss percent", ylab="cumulative pnl",
   t="l", lwd=3, col="blue")

# Calculate the USO prices and returns
pricev <- log(na.omit(rutils::etfenv$prices$USO))
nrows <- NROW(pricev)
datev <- zoo::index(pricev)
retp <- rutils::diffit(pricev)
# Simulate multiple stop-start strategies
stopv <- 0.01*(1:30)
pnlc <- unlist(mclapply(stopv, function(stopl) {
  sum(stopstart(stopl))
}, mc.cores=ncores))  # end mclapply

# Plot cumulative PnLs of stop-start strategies
plot(x=stopv, y=pnlc,
   main="Cumulative PnLs of USO Stop-Start Strategies",
   xlab="stop-loss percent", ylab="cumulative pnl",
   t="l", lwd=3, col="blue")

# Simulate optimal stop-start strategy for USO
stopl <- stopv[which.max(pnlc)]
pnls <- stopstart(stopl)
# Combine the data
wealthv <- cbind(retp, pnls)
colv <- c("USO", "Strategy")
colnames(wealthv) <- colv
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# dygraph plot the stop-start strategy
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="USO Stop-Start Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Plot the stop-start strategy with shading
insl <- (pnls == 0) # Is in stop-loss?
indic <- (rutils::diffit(insl) != 0) # Indices of crosses
crossd <- c(datev[indic], datev[nrows]) # Dates of crosses
shadev <- ifelse(insl[indic] == 1, "antiquewhite", "lightgreen")
# Create dygraph object without plotting it
dyplot <- dygraphs::dygraph(cumsum(wealthv),
  main="USO Stop-Start Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Add shading to dygraph object
for (i in 1:NROW(shadev)) {
  dyplot <- dyplot %>% dyShading(from=crossd[i], to=crossd[i+1], color=shadev[i])
}  # end for
# Plot the dygraph object
dyplot

# Function for simulating the trend-flip strategy
trend_flip <- function(stopl = 0.1) {
  pricev <- coredata(pricev)
  nrows <- NROW(pricev)
  pricec <- pricev[1]
  pricemax <- pricec
  pricemin <- pricec
  drawp <- 0
  gainp <- 0
  posv <- integer(nrows)
  pnlv <- numeric(nrows)
  for (it in 2:(nrows - 1)) {
    pricec <- pricev[it]
    pnlv[it] <- posv[it] * (pricec - pricev[it - 1])
    pricemax <- max(pricemax, pricec)
    pricemin <- min(pricemin, pricec)
    drawp <- min(pricec - pricemax, 0)
    gainp <- max(pricec - pricemin, 0)
    if ((drawp < -stopl) & (posv[it] >= 0)) {
posv[it + 1] <- (-1)
pricemin <- pricec
    } else if ((gainp > stopl) & (posv[it] <= 0)) {
posv[it + 1] <- 1
pricemax <- pricec
    } else { # Do nothing
posv[it + 1] <- posv[it]
    }
  }
  return(cbind(pnlv, posv))
} # end trend_flip

# Simulate the trend-flip strategy
pnls <- trend_flip(0.09)
# Combine the data
wealthv <- cbind(retp, pnls[, "pnlv"])
colv <- c("VTI", "Strategy")
colnames(wealthv) <- colv
# Has low correlation to VTI
cor(wealthv)[1, 2]
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# dygraph plot the stop-loss strategy
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="VTI Trend-Flip Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Plot the trend-flip strategy with shading
posv <- pnls[, "posv"]
indic <- (rutils::diffit(posv) != 0) # Indices of crosses
crossd <- c(datev[indic], datev[nrows]) # Dates of crosses
shadev <- ifelse(posv[indic] == 1, "lightgreen", "antiquewhite")
# Create dygraph object without plotting it
dyplot <- dygraphs::dygraph(cumsum(wealthv),
  main="VTI Trend-Flip Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Add shading to dygraph object
for (i in 1:NROW(shadev)) {
  dyplot <- dyplot %>% dyShading(from=crossd[i], to=crossd[i+1], color=shadev[i])
}  # end for
# Plot the dygraph object
dyplot

# Simulate stop-loss strategies in a parallel loop
stopv <- 0.01*(1:30)
library(parallel)
ncores <- detectCores()-1
pnlc <- unlist(mclapply(stopv, function(stopl) {
  sum(trend_flip(stopl)[, "pnlv"])
}, mc.cores=ncores))  # end mclapply
# Find optimal stop-loss level
stopl <- stopv[which.max(pnlc)]

# Plot cumulative PnLs of trend-flip strategies
plot(x=stopv, y=pnlc,
   main="Cumulative PnLs of Trend-Flip Strategies",
   xlab="stop-loss percent", ylab="cumulative pnl",
   t="l", lwd=3, col="blue")

# Calculate the USO prices and returns
pricev <- log(na.omit(rutils::etfenv$prices$USO))
nrows <- NROW(pricev)
datev <- zoo::index(pricev)
retp <- rutils::diffit(pricev)
# Simulate multiple trend-flip strategies
stopv <- 0.01*(1:30)
pnlc <- unlist(mclapply(stopv, function(stopl) {
  sum(trend_flip(stopl)[, "pnlv"])
}, mc.cores=ncores))  # end mclapply

# Plot cumulative PnLs of trend-flip strategies
plot(x=stopv, y=pnlc,
   main="Cumulative PnLs of USO Trend-Flip Strategies",
   xlab="stop-loss percent", ylab="cumulative pnl",
   t="l", lwd=3, col="blue")

# Simulate optimal trend-flip strategy for USO
stopl <- stopv[which.max(pnlc)]
pnls <- trend_flip(stopl)
# Combine the data
wealthv <- cbind(retp, pnls[, "pnlv"])
colv <- c("USO", "Strategy")
colnames(wealthv) <- colv
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# dygraph plot the trend-flip strategy
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="USO Trend-Flip Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Plot the trend-flip strategy with shading
posv <- pnls[, "posv"]
indic <- (rutils::diffit(posv) != 0) # Indices of crosses
crossd <- c(datev[indic], datev[nrows]) # Dates of crosses
shadev <- ifelse(posv[indic] == 1, "lightgreen", "antiquewhite")
# Create dygraph object without plotting it
dyplot <- dygraphs::dygraph(cumsum(wealthv),
  main="USO Trend-Flip Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Add shading to dygraph object
for (i in 1:NROW(shadev)) {
  dyplot <- dyplot %>% dyShading(from=crossd[i], to=crossd[i+1], color=shadev[i])
}  # end for
# Plot the dygraph object
dyplot

# Extract the log VTI prices
ohlc <- log(rutils::etfenv$VTI)
closep <- quantmod::Cl(ohlc)
colnames(closep) <- "VTI"
nrows <- NROW(closep)
# Calculate the EMA weights
lookb <- 111
lambdaf <- 0.9
weightv <- lambdaf^(0:lookb)
weightv <- weightv/sum(weightv)
# Calculate the EMA prices as a convolution
pricema <- HighFreq::roll_sumw(closep, weightv=weightv)
pricev <- cbind(closep, pricema)
colnames(pricev) <- c("VTI", "VTI EMA")

# Dygraphs plot with custom line colors
colv <- colnames(pricev)
dygraphs::dygraph(pricev["2008/2009"], main="VTI EMA Prices") %>%
  dySeries(name=colv[1], strokeWidth=1, col="blue") %>%
  dySeries(name=colv[2], strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)
# Standard plot of  EMA prices with custom line colors
x11(width=6, height=5)
themev <- chart_theme()
colorv <- c("blue", "red")
themev$col$line.col <- colorv
quantmod::chart_Series(pricev["2009"], theme=themev,
       lwd=2, name="VTI EMA Prices")
legend("topleft", legend=colnames(pricev), y.intersp=0.5,
 inset=0.1, bg="white", lty=1, lwd=6, cex=0.8,
 col=themev$col$line.col, bty="n")

# Calculate the EMA prices recursively using C++ code
emar <- .Call(stats:::C_rfilter, closep, lambdaf, c(as.numeric(closep[1])/(1-lambdaf), double(NROW(closep))))[-1]
# Or R code
# emar <- filter(closep, filter=lambdaf, init=as.numeric(closep[1, 1])/(1-lambdaf), method="recursive")
emar <- (1-lambdaf)*emar
# Calculate the EMA prices recursively using RcppArmadillo
pricema <- HighFreq::run_mean(closep, lambda=lambdaf)
all.equal(drop(pricema), emar)
# Compare the speed of C++ code with RcppArmadillo
library(microbenchmark)
summary(microbenchmark(
  Rcpp=HighFreq::run_mean(closep, lambda=lambdaf),
  rfilter=.Call(stats:::C_rfilter, closep, lambdaf, c(as.numeric(closep[1])/(1-lambdaf), double(NROW(closep)))),
  times=10))[, c(1, 4, 5)]

# Dygraphs plot with custom line colors
pricev <- cbind(closep, pricema)
colnames(pricev) <- c("VTI", "VTI EMA")
colv <- colnames(pricev)
dygraphs::dygraph(pricev["2008/2009"], main="Recursive VTI EMA Prices") %>%
  dySeries(name=colv[1], strokeWidth=1, col="blue") %>%
  dySeries(name=colv[2], strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)
# Standard plot of  EMA prices with custom line colors
themev <- chart_theme()
colorv <- c("blue", "red")
themev$col$line.col <- colorv
quantmod::chart_Series(pricev["2009"], theme=themev,
       lwd=2, name="VTI EMA Prices")
legend("topleft", legend=colnames(pricev), y.intersp=0.5,
 inset=0.1, bg="white", lty=1, lwd=6, cex=0.8,
 col=themev$col$line.col, bty="n")

# Calculate the EMA prices recursively using C++ code
lambdaf <- 0.984
pricema <- HighFreq::run_mean(closep, lambda=lambdaf)
pricev <- cbind(closep, pricema)
colnames(pricev) <- c("VTI", "VTI EMA")
colv <- colnames(pricev)
# Calculate the positions, either: -1, 0, or 1
indic <- sign(closep - pricema)
posv <- rutils::lagit(indic, lagg=1)
# Create colors for background shading
crossd <- (rutils::diffit(posv) != 0)
shadev <- posv[crossd]
crossd <- c(zoo::index(shadev), end(posv))
shadev <- ifelse(drop(zoo::coredata(shadev)) == 1, "lightgreen", "antiquewhite")
# Create dygraph object without plotting it
dyplot <- dygraphs::dygraph(pricev, main="VTI EMA Prices") %>%
  dySeries(name=colv[1], strokeWidth=1, col="blue") %>%
  dySeries(name=colv[2], strokeWidth=3, col="red") %>%
  dyLegend(show="always", width=300)


# Add shading to dygraph object
for (i in 1:NROW(shadev)) {
  dyplot <- dyplot %>% dyShading(from=crossd[i], to=crossd[i+1], color=shadev[i])
}  # end for
# Plot the dygraph object
dyplot
# Standard plot of EMA prices with position shading
quantmod::chart_Series(pricev, theme=themev,
       lwd=2, name="VTI EMA Prices")
add_TA(posv > 0, on=-1, col="lightgreen", border="lightgreen")
add_TA(posv < 0, on=-1, col="lightgrey", border="lightgrey")
legend("topleft", legend=colnames(pricev),
 inset=0.1, bg="white", lty=1, lwd=6, y.intersp=0.5,
 col=themev$col$line.col, bty="n")

# Calculate the daily profits and losses of crossover strategy
retp <- rutils::diffit(closep)  # VTI returns
pnls <- retp*posv
colnames(pnls) <- "EMA"
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "EMA PnL")
# Annualized Sharpe ratio of crossover strategy
sqrt(252)*sapply(wealthv, function (x) mean(x)/sd(x))
# The crossover strategy has a negative correlation to VTI
cor(wealthv)[1, 2]
# Plot dygraph of crossover strategy wealth
# Create dygraph object without plotting it
colorv <- c("blue", "red")
dyplot <- dygraphs::dygraph(cumsum(wealthv), main="Performance of Crossover Strategy") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Add shading to dygraph object
for (i in 1:NROW(shadev)) {
  dyplot <- dyplot %>%
    dyShading(from=crossd[i], to=crossd[i+1], color=shadev[i])
}  # end for
# Plot the dygraph object
dyplot

# Standard plot of crossover strategy wealth
x11(width=6, height=5)
themev <- chart_theme()
themev$col$line.col <- colorv
quantmod::chart_Series(cumsum(wealthv), theme=themev,
       name="Performance of Crossover Strategy")
add_TA(posv > 0, on=-1, col="lightgreen", border="lightgreen")
add_TA(posv < 0, on=-1, col="lightgrey", border="lightgrey")
legend("top", legend=colnames(wealthv), y.intersp=0.5,
 inset=0.05, bg="white", lty=1, lwd=6,
 col=themev$col$line.col, bty="n")

# Test EMA crossover market timing of VTI using Treynor-Mazuy test
desm <- cbind(pnls, retp, retp^2)
desm <- na.omit(desm)
colnames(desm) <- c("EMA", "VTI", "Treynor")
regmod <- lm(EMA ~ VTI + Treynor, data=desm)
summary(regmod)
# Plot residual scatterplot
resids <- (desm$EMA - regmod$coeff["VTI"]*retp)
resids <- regmod$residuals
plot.default(x=retp, y=resids, xlab="VTI", ylab="residuals")
title(main="Treynor-Mazuy Market Timing Test\n for EMA Crossover vs VTI", line=0.5)
# Plot fitted (predicted) response values
coefreg <- summary(regmod)$coeff
fitv <- regmod$fitted.values - coefreg["VTI", "Estimate"]*retp
tvalue <- round(coefreg["Treynor", "t value"], 2)
points.default(x=retp, y=fitv, pch=16, col="red")
text(x=0.0, y=0.8*max(resids), paste("Treynor test t-value =", tvalue))

# Determine the trade dates right after EMA has crossed prices
indic <- sign(closep - pricema)
# Calculate the positions from lagged indicator
lagg <- 2
indic <- HighFreq::roll_sum(indic, lagg)
# Calculate the positions, either: -1, 0, or 1
posv <- rep(NA_integer_, nrows)
posv[1] <- 0
posv <- ifelse(indic == lagg, 1, posv)
posv <- ifelse(indic == (-lagg), -1, posv)
posv <- zoo::na.locf(posv, na.rm=FALSE)
posv <- xts::xts(posv, order.by=zoo::index(closep))
# Lag the positions to trade in next period
posv <- rutils::lagit(posv, lagg=1)
# Calculate the PnLs of lagged strategy
pnlslag <- retp*posv
colnames(pnlslag) <- "Lagged Strategy"

wealthv <- cbind(pnls, pnlslag)
colnames(wealthv) <- c("EMA", "Lagged")
# Annualized Sharpe ratios of crossover strategies
sharper <- sqrt(252)*sapply(wealthv, function (x) mean(x)/sd(x))
# Plot both strategies
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw], main=paste("EMA Crossover Strategy", paste(paste(names(sharper), round(sharper, 3), sep="="), collapse=", "))) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Calculate the positions, either: -1, 0, or 1
indic <- sign(closep - pricema)
posv <- rutils::lagit(indic, lagg=1)
# Calculate the daily pnl for days without trades
pnls <- retp*posv
# Determine the trade dates right after EMA has crossed prices
crossd <- which(rutils::diffit(posv) != 0)
# Calculate the realized pnl for days with trades
openp <- quantmod::Op(ohlc)
closelag <- rutils::lagit(closep)
poslag <- rutils::lagit(posv)
pnls[crossd] <- poslag[crossd]*(openp[crossd] - closelag[crossd])
# Calculate the unrealized pnl for days with trades
pnls[crossd] <- pnls[crossd] +
  posv[crossd]*(closep[crossd] - openp[crossd])
# Calculate the wealth
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "EMA PnL")
# Annualized Sharpe ratio of crossover strategy
sqrt(252)*sapply(wealthv, function (x) mean(x)/sd(x))
# The crossover strategy has a negative correlation to VTI
cor(wealthv)[1, 2]

# Plot dygraph of crossover strategy wealth
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw], main="Crossover Strategy Trading at the Open Price") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Standard plot of crossover strategy wealth
quantmod::chart_Series(cumsum(wealthv)[endw], theme=themev,
       name="Crossover Strategy Trading at the Open Price")
legend("top", legend=colnames(wealthv),
 inset=0.05, bg="white", lty=1, lwd=6,
 col=themev$col$line.col, bty="n")

# The bid-ask spread is equal to 1 bp for liquid ETFs
bidask <- 0.001
# Calculate the transaction costs
costv <- 0.5*bidask*abs(poslag - posv)
# Plot strategy with transaction costs
wealthv <- cbind(pnls, pnls - costv)
colnames(wealthv) <- c("EMA", "EMA w Costs")
colorv <- c("blue", "red")
dygraphs::dygraph(cumsum(wealthv)[endw], main="Crossover Strategy With Transaction Costs") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=300)

sim_ema <- function(closep, lambdaf=0.9, bidask=0.001, trend=1, lagg=1) {
  retp <- rutils::diffit(closep)
  nrows <- NROW(closep)
  # Calculate the EMA prices
  pricema <- HighFreq::run_mean(closep, lambda=lambdaf)
  # Calculate the indicator
  indic <- trend*sign(closep - pricema)
  if (lagg > 1) {
    indic <- HighFreq::roll_sum(indic, lagg)
    indic[1:lagg] <- 0
  }  # end if
  # Calculate the positions, either: -1, 0, or 1
  posv <- rep(NA_integer_, nrows)
  posv[1] <- 0
  posv <- ifelse(indic == lagg, 1, posv)
  posv <- ifelse(indic == (-lagg), -1, posv)
  posv <- zoo::na.locf(posv, na.rm=FALSE)
  posv <- xts::xts(posv, order.by=zoo::index(closep))
  # Lag the positions to trade on next day
  posv <- rutils::lagit(posv, lagg=1)
  # Calculate the PnLs of strategy
  pnls <- retp*posv
  costv <- 0.5*bidask*abs(rutils::diffit(posv))
  pnls <- (pnls - costv)
  # Calculate the strategy returns
  pnls <- cbind(posv, pnls)
  colnames(pnls) <- c("positions", "pnls")
  pnls
}  # end sim_ema

lambdav <- seq(from=0.97, to=0.99, by=0.004)
# Perform lapply() loop over lambdav
pnltrend <- lapply(lambdav, function(lambdaf) {
  # Simulate crossover strategy and calculate the returns
  sim_ema(closep=closep, lambdaf=lambdaf, bidask=0, lagg=2)[, "pnls"]
})  # end lapply
pnltrend <- do.call(cbind, pnltrend)
colnames(pnltrend) <- paste0("lambda=", lambdav)

# Plot dygraph of multiple crossover strategies
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnltrend))
endw <- rutils::calc_endpoints(pnltrend, interval="weeks")
dygraphs::dygraph(cumsum(pnltrend)[endw], main="Cumulative Returns of Trend Following Crossover Strategies") %>%
  dyOptions(colors=colorv, strokeWidth=1) %>%
  dyLegend(show="always", width=400)
# Plot crossover strategies with custom line colors
themev <- chart_theme()
themev$col$line.col <- colorv
quantmod::chart_Series(cumsum(pnltrend), theme=themev,
  name="Cumulative Returns of Crossover Strategies")
legend("topleft", legend=colnames(pnltrend), inset=0.1,
  bg="white", cex=0.8, lwd=rep(6, NCOL(pnltrend)),
  col=themev$col$line.col, bty="n")

# Initialize compute cluster under Windows
library(parallel)
ncores <- detectCores() - 1  # Number of cores
compclust <- makeCluster(detectCores()-1)
clusterExport(compclust,
  varlist=c("ohlc", "lookb", "sim_ema"))
# Perform parallel loop over lambdav under Windows
pnltrend <- parLapply(compclust, lambdav, function(lambdaf) {
  library(quantmod)
  # Simulate crossover strategy and calculate the returns
  sim_ema(closep=closep, lambdaf=lambdaf, bidask=0, lagg=2)[, "pnls"]
})  # end parLapply
stopCluster(compclust)  # Stop R processes over cluster under Windows
# Perform parallel loop over lambdav under Mac-OSX or Linux
pnltrend <- mclapply(lambdav, function(lambdaf) {
  library(quantmod)
  # Simulate crossover strategy and calculate the returns
  sim_ema(closep=closep, lambdaf=lambdaf, bidask=0, lagg=2)[, "pnls"]
}, mc.cores=ncores)  # end mclapply
pnltrend <- do.call(cbind, pnltrend)
colnames(pnltrend) <- paste0("lambda=", lambdav)

# Calculate the annualized Sharpe ratios of strategy returns
sharpetrend <- sqrt(252)*sapply(pnltrend, function(xtsv) {
  mean(xtsv)/sd(xtsv)
})  # end sapply
# Plot Sharpe ratios
dev.new(width=6, height=5, noRStudioGD=TRUE)
plot(x=lambdav, y=sharpetrend, t="l",
     xlab="lambda", ylab="Sharpe",
     main="Performance of EMA Trend Following Strategies
     as Function of the Decay Factor Lambda")

# Calculate the optimal lambda
lambdaf <- lambdav[which.max(sharpetrend)]
# Simulate best performing strategy
ematrend <- sim_ema(closep=closep, lambdaf=lambdaf, bidask=0, lagg=2)
posv <- ematrend[, "positions"]
trendopt <- ematrend[, "pnls"]
wealthv <- cbind(retp, trendopt)
colnames(wealthv) <- c("VTI", "EMA PnL")
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
cor(wealthv)[1, 2]
# Plot dygraph of crossover strategy wealth
dygraphs::dygraph(cumsum(wealthv)[endw], main="Performance of Optimal Trend Following Crossover Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Plot EMA PnL with position shading
# Standard plot of crossover strategy wealth
x11(width=6, height=5)
themev <- chart_theme()
themev$col$line.col <- colorv
quantmod::chart_Series(cumsum(wealthv), theme=themev,
       name="Performance of Crossover Strategy")
add_TA(posv > 0, on=-1, col="lightgreen", border="lightgreen")
add_TA(posv < 0, on=-1, col="lightgrey", border="lightgrey")
legend("top", legend=colnames(wealthv),
 inset=0.05, bg="white", lty=1, lwd=6,
 col=themev$col$line.col, bty="n")

lambdav <- seq(0.6, 0.7, 0.01)
# Perform lapply() loop over lambdav
pnlrevert <- lapply(lambdav, function(lambdaf) {
  # Simulate crossover strategy and calculate the returns
  sim_ema(closep=closep, lambdaf=lambdaf, bidask=0, trend=(-1))[, "pnls"]
})  # end lapply
pnlrevert <- do.call(cbind, pnlrevert)
colnames(pnlrevert) <- paste0("lambda=", lambdav)
# Plot dygraph of mean reverting crossover strategies
colorv <- colorRampPalette(c("blue", "red"))(NROW(lambdav))
dygraphs::dygraph(cumsum(pnlrevert)[endw], main="Returns of Mean Reverting Crossover Strategies (No costv)") %>%
  dyOptions(colors=colorv, strokeWidth=1) %>%
  dyLegend(show="always", width=400)
# Plot crossover strategies with custom line colors
x11(width=6, height=5)
themev <- chart_theme()
themev$col$line.col <- colorv
quantmod::chart_Series(pnlrevert,
  theme=themev, name="Cumulative Returns of Mean Reverting Crossover Strategies")
legend("topleft", legend=colnames(pnlrevert),
  inset=0.1, bg="white", cex=0.8, lwd=6,
  col=themev$col$line.col, bty="n")

# Calculate the Sharpe ratios of strategy returns
sharperevert <- sqrt(252)*sapply(pnlrevert, function(xtsv) {
  mean(xtsv)/sd(xtsv)
})  # end sapply
plot(x=lambdav, y=sharperevert, t="l",
     xlab="lambda", ylab="Sharpe",
     main="Performance of EMA Mean Reverting Strategies
     as Function of the Decay Factor Lambda")

# Calculate the optimal lambda
lambdaf <- lambdav[which.max(sharperevert)]
# Simulate best performing strategy
emarevert <- sim_ema(closep=closep, bidask=0.0,
  lambdaf=lambdaf, trend=(-1))
posv <- emarevert[, "positions"]
revertopt <- emarevert[, "pnls"]
wealthv <- cbind(retp, revertopt)
colnames(wealthv) <- c("VTI", "EMA PnL")
# Plot dygraph of crossover strategy wealth
colorv <- c("blue", "red")
dygraphs::dygraph(cumsum(wealthv)[endw], main="Optimal Mean Reverting Crossover Strategy (No costv)") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Standard plot of crossover strategy wealth
x11(width=6, height=5)
themev <- chart_theme()
themev$col$line.col <- colorv
quantmod::chart_Series(cumsum(wealthv), theme=themev,
       name="Optimal Mean Reverting Crossover Strategy")
add_TA(posv > 0, on=-1, col="lightgreen", border="lightgreen")
add_TA(posv < 0, on=-1, col="lightgrey", border="lightgrey")
legend("top", legend=colnames(wealthv),
 inset=0.05, bg="white", lty=1, lwd=6,
 col=themev$col$line.col, bty="n")

# Calculate the correlation between trend following and mean reverting strategies
trendopt <- ematrend[, "pnls"]
colnames(trendopt) <- "trend"
revertopt <- emarevert[, "pnls"]
colnames(revertopt) <- "revert"
cor(cbind(retp, trendopt, revertopt))
# Calculate the combined strategy
combstrat <- (retp + trendopt + revertopt)/3
colnames(combstrat) <- "combined"
# Calculate the annualized Sharpe ratio of strategy returns
retc <- cbind(retp, trendopt, revertopt, combstrat)
colnames(retc) <- c("VTI", "Trending", "Reverting", "Combined")
sqrt(252)*sapply(retc, function(xtsv) mean(xtsv)/sd(xtsv))

# Plot dygraph of crossover strategy wealth
colorv <- c("blue", "red", "green", "purple")
dygraphs::dygraph(cumsum(retc)[endw], main="Performance of Combined Crossover Strategies") %>%
  dyOptions(colors=colorv, strokeWidth=1) %>%
  dySeries(name="Combined", strokeWidth=3) %>%
  dyLegend(show="always", width=300)
# Standard plot of crossover strategy wealth
themev <- chart_theme()
themev$col$line.col <- colorv
quantmod::chart_Series(pnls, theme=themev,
  name="Performance of Combined Crossover Strategies")
legend("topleft", legend=colnames(pnls),
  inset=0.05, bg="white", lty=1, lwd=6,
  col=themev$col$line.col, bty="n")

# Calculate the weights proportional to Sharpe ratios
weightv <- c(sharpetrend, sharperevert)
weightv[weightv < 0] <- 0
weightv <- weightv/sum(weightv)
retc <- cbind(pnltrend, pnlrevert)
retc <- retc %*% weightv
retc <- cbind(retp, retc)
colnames(retc) <- c("VTI", "EMA PnL")
# Plot dygraph of crossover strategy wealth
colorv <- c("blue", "red")
dygraphs::dygraph(cumsum(retc)[endw], main="Performance of Ensemble of Crossover Strategies") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Standard plot of crossover strategy wealth
themev <- chart_theme()
themev$col$line.col <- colorv
quantmod::chart_Series(cumsum(retc), theme=themev,
       name="Performance of Ensemble of Crossover Strategies")
legend("topleft", legend=colnames(pnls),
 inset=0.05, bg="white", lty=1, lwd=6,
 col=themev$col$line.col, bty="n")

# Extract the log VTI prices
ohlc <- log(rutils::etfenv$VTI)
datev <- zoo::index(ohlc)
nrows <- NROW(ohlc)
closep <- quantmod::Cl(ohlc)
colnames(closep) <- "VTI"
retp <- rutils::diffit(closep)  # VTI returns
# Calculate the fast and slow EMAs
lambdafa <- 0.89
lambdasl <- 0.95
# Calculate the EMA prices
emaf <- HighFreq::run_mean(closep, lambda=lambdafa)
emas <- HighFreq::run_mean(closep, lambda=lambdasl)
pricev <- cbind(closep, emaf, emas)
colnames(pricev) <- c("VTI", "EMA fast", "EMA slow")
# Determine the positions and the trade dates right after the EMAs have crossed
indic <- sign(emaf - emas)
posv <- rutils::lagit(indic)
crossd <- (rutils::diffit(posv) != 0)
# Create colors for background shading
shadev <- posv[crossd]
shadev <- ifelse(shadev == 1, "lightgreen", "antiquewhite")
crossd <- c(datev[crossd], end(closep))

# Plot dygraph
colv <- colnames(pricev)
dyplot <- dygraphs::dygraph(pricev, main="VTI Dual EMA Prices") %>%
  dySeries(name=colv[1], strokeWidth=1, col="blue") %>%
  dySeries(name=colv[2], strokeWidth=2, col="red") %>%
  dySeries(name=colv[3], strokeWidth=2, col="purple") %>%
  dyLegend(show="always", width=300)
for (i in 1:NROW(shadev)) {
  dyplot <- dyplot %>% dyShading(from=crossd[i], to=crossd[i+1], color=shadev[i])
}  # end for
dyplot

# Calculate the daily profits and losses of strategy
pnls <- retp*posv
colnames(pnls) <- "Strategy"
wealthv <- cbind(retp, pnls)
# Annualized Sharpe ratio of dual crossover strategy
sharper <- sqrt(252)*sapply(wealthv, function (x) mean(x)/sd(x))
# The crossover strategy has a negative correlation to VTI
cor(wealthv)[1, 2]

# Plot Dual crossover strategy
dyplot <- dygraphs::dygraph(cumsum(wealthv),
  main=paste("EMA Dual Crossover Strategy, Sharpe", paste(paste(names(sharper), round(sharper, 3), sep="="), collapse=", "))) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2)
# Add shading to dygraph object
for (i in 1:NROW(shadev)) {
  dyplot <- dyplot %>% dyShading(from=crossd[i], to=crossd[i+1], color=shadev[i])
}  # end for
# Plot the dygraph object
dyplot

sim_ema2 <- function(closep, lambdafa=0.1, lambdasl=0.01,
               bidask=0.001, trend=1, lagg=1) {
  if (lambdafa >= lambdasl) return(NA)
  retp <- rutils::diffit(closep)
  nrows <- NROW(closep)
  # Calculate the EMA prices
  emaf <- HighFreq::run_mean(closep, lambda=lambdafa)
  emas <- HighFreq::run_mean(closep, lambda=lambdasl)
  # Calculate the positions, either: -1, 0, or 1
  indic <- sign(emaf - emas)
  if (lagg > 1) {
    indic <- HighFreq::roll_sum(indic, lagg)
    indic[1:lagg] <- 0
  }  # end if
  posv <- rep(NA_integer_, nrows)
  posv[1] <- 0
  posv <- ifelse(indic == lagg, 1, posv)
  posv <- ifelse(indic == (-lagg), -1, posv)
  posv <- zoo::na.locf(posv, na.rm=FALSE)
  posv <- xts::xts(posv, order.by=zoo::index(closep))
  # Lag the positions to trade on next day
  posv <- rutils::lagit(posv, lagg=1)
  # Calculate the PnLs of strategy
  pnls <- retp*posv
  costv <- 0.5*bidask*abs(rutils::diffit(posv))
  pnls <- (pnls - costv)
  # Calculate the strategy returns
  pnls <- cbind(posv, pnls)
  colnames(pnls) <- c("positions", "pnls")
  pnls
}  # end sim_ema2

lambdafv <- seq(from=0.85, to=0.99, by=0.01)
lambdasv <- seq(from=0.85, to=0.99, by=0.01)
# Calculate the Sharpe ratio of dual crossover strategy
calc_sharpe <- function(closep, lambdafa, lambdasl, bidask, trend, lagg) {
  if (lambdafa >= lambdasl) return(NA)
  pnls <- sim_ema2(closep=closep, lambdafa=lambdafa, lambdasl=lambdasl,
    bidask=bidask, trend=trend, lagg=lagg)[, "pnls"]
  sqrt(252)*mean(pnls)/sd(pnls)
}  # end calc_sharpe
# Vectorize calc_sharpe with respect to lambdafa and lambdasl
calc_sharpe <- Vectorize(FUN=calc_sharpe,
  vectorize.args=c("lambdafa", "lambdasl"))
# Calculate the matrix of PnLs
sharpem <- outer(lambdafv, lambdasv, FUN=calc_sharpe,
           closep=closep, bidask=0.0, trend=1, lagg=1)
# Or perform two sapply() loops over lambda vectors
sharpem <- sapply(lambdasv, function(lambdasl) {
  sapply(lambdafv, function(lambdafa) {
    if (lambdafa >= lambdasl) return(NA)
    calc_sharpe(closep=closep, lambdafa=lambdafa,
      lambdasl=lambdasl, bidask=0.0, trend=1, lagg=1)
  })  # end sapply
})  # end sapply
colnames(sharpem) <- lambdasv
rownames(sharpem) <- lambdafv

# Calculate the PnLs of the optimal crossover strategy
whichv <- which(sharpem == max(sharpem, na.rm=TRUE), arr.ind=TRUE)
lambdafa <- lambdafv[whichv[1]]
lambdasl <- lambdasv[whichv[2]]
crossopt <- sim_ema2(closep=closep, lambdafa=lambdafa, lambdasl=lambdasl,
  bidask=0.0, trend=1, lagg=1)
pnls <- crossopt[, "pnls"]
wealthv <- cbind(retp, pnls)
colnames(wealthv)[2] <- "EMA"
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Annualized Sharpe ratio of dual crossover strategy
sharper <- sqrt(252)*sapply(wealthv, function (x) mean(x)/sd(x))
# The crossover strategy has a negative correlation to VTI
cor(wealthv)[1, 2]

# Create colors for background shading
posv <- crossopt[, "positions"]
crossd <- (rutils::diffit(posv) != 0)
shadev <- posv[crossd]
crossd <- c(zoo::index(shadev), end(posv))
shadev <- ifelse(drop(zoo::coredata(shadev)) == 1, "lightgreen", "antiquewhite")
# Plot Optimal Dual crossover strategy
dyplot <- dygraphs::dygraph(cumsum(wealthv), main=paste("Optimal Dual Crossover Strategy, Sharpe", paste(paste(names(sharper), round(sharper, 3), sep="="), collapse=", "))) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2)
# Add shading to dygraph object
for (i in 1:NROW(shadev)) {
  dyplot <- dyplot %>% dyShading(from=crossd[i], to=crossd[i+1], color=shadev[i])
}  # end for
# Plot the dygraph object
dyplot

# Define in-sample and out-of-sample intervals
insample <- 1:(nrows %/% 2)
outsample <- (nrows %/% 2 + 1):nrows
# Calculate the matrix of PnLs
sharpem <- outer(lambdafv, lambdasv,
           FUN=calc_sharpe, closep=closep[insample, ],
           bidask=0.0, trend=1, lagg=1)
colnames(sharpem) <- lambdasv
rownames(sharpem) <- lambdafv
# Calculate the PnLs of the optimal strategy
whichv <- which(sharpem == max(sharpem, na.rm=TRUE), arr.ind=TRUE)
lambdafa <- lambdafv[whichv[1]]
lambdasl <- lambdasv[whichv[2]]
pnls <- sim_ema2(closep=closep, lambdafa=lambdafa, lambdasl=lambdasl,
           bidask=0.0, trend=1, lagg=1)[, "pnls"]
wealthv <- cbind(retp, pnls)
colnames(wealthv)[2] <- "EMA"
# Calculate the Sharpe and Sortino ratios in-sample and out-of-sample
sqrt(252)*sapply(wealthv[insample, ], function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
sqrt(252)*sapply(wealthv[outsample, ], function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))

# Dygraphs plot with custom line colors
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw], main="Dual Crossover Strategy Out-of-Sample") %>%
  dyEvent(zoo::index(wealthv[last(insample)]), label="in-sample", strokePattern="solid", color="green") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2)

# Calculate the log OHLC prices and volumes
ohlc <- rutils::etfenv$VTI
closep <- log(quantmod::Cl(ohlc))
colnames(closep) <- "VTI"
volumv <- quantmod::Vo(ohlc)
colnames(volumv) <- "Volume"
nrows <- NROW(closep)
# Calculate the VWAP prices
lookb <- 21
vwap <- HighFreq::roll_sum(closep, lookb=lookb, weightv=volumv)
colnames(vwap) <- "VWAP"
pricev <- cbind(closep, vwap)

# Dygraphs plot with custom line colors
colorv <- c("blue", "red")
dygraphs::dygraph(pricev["2009"], main="VTI VWAP Prices") %>%
  dyOptions(colors=colorv, strokeWidth=2)
# Plot VWAP prices with custom line colors
x11(width=6, height=5)
themev <- chart_theme()
themev$col$line.col <- colors
quantmod::chart_Series(pricev["2009"], theme=themev,
       lwd=2, name="VTI VWAP Prices")
legend("bottomright", legend=colnames(pricev),
 inset=0.1, bg="white", lty=1, lwd=6, cex=0.8,
 col=themev$col$line.col, bty="n")

# Calculate the VWAP prices recursively using C++ code
lambdaf <- 0.9
volumer <- .Call(stats:::C_rfilter, volumv, lambdaf, c(as.numeric(volumv[1])/(1-lambdaf), double(NROW(volumv))))[-1]
pricer <- .Call(stats:::C_rfilter, volumv*closep, lambdaf, c(as.numeric(volumv[1]*closep[1])/(1-lambdaf), double(NROW(closep))))[-1]
vwapr <- pricer/volumer
# Calculate the VWAP prices recursively using RcppArmadillo
vwapc <- HighFreq::run_mean(closep, lambda=lambdaf, weightv=volumv)
all.equal(vwapr, drop(vwapc))
# Dygraphs plot the VWAP prices
pricev <- xts(cbind(vwap, vwapr), zoo::index(ohlc))
colnames(pricev) <- c("VWAP rolling", "VWAP recursive")
dygraphs::dygraph(pricev["2009"], main="VWAP Prices") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Calculate the VWAP prices recursively using RcppArmadillo
lambdaf <- 0.99
vwapc <- HighFreq::run_mean(closep, lambda=lambdaf, weightv=volumv)
# Calculate the positions from lagged indicator
indic <- sign(closep - vwapc)
lagg <- 2
indic <- HighFreq::roll_sum(indic, lagg)
# Calculate the positions, either: -1, 0, or 1
posv <- rep(NA_integer_, nrows)
posv[1] <- 0
posv <- ifelse(indic == lagg, 1, posv)
posv <- ifelse(indic == (-lagg), -1, posv)
posv <- zoo::na.locf(posv, na.rm=FALSE)
posv <- xts::xts(posv, order.by=zoo::index(closep))
# Lag the positions to trade in next period
posv <- rutils::lagit(posv, lagg=1)
# Calculate the PnLs of VWAP strategy
retp <- rutils::diffit(closep)  # VTI returns
pnls <- retp*posv
colnames(pnls) <- "VWAP"
wealthv <- cbind(retp, pnls)
colv <- colnames(wealthv)

# Annualized Sharpe ratios of VTI and VWAP strategy
sharper <- sqrt(252)*sapply(wealthv, function (x) mean(x)/sd(x))
# Create colors for background shading
crossd <- (rutils::diffit(posv) != 0)
shadev <- posv[crossd]
crossd <- c(zoo::index(shadev), end(posv))
shadev <- ifelse(drop(zoo::coredata(shadev)) == 1, "lightgreen", "antiquewhite")
# Plot dygraph of VWAP strategy
# Create dygraph object without plotting it
dyplot <- dygraphs::dygraph(cumsum(wealthv),
  main=paste("VWAP Crossover Strategy, Sharpe", paste(paste(names(sharper), round(sharper, 3), sep="="), collapse=", "))) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Add shading to dygraph object
for (i in 1:NROW(shadev)) {
  dyplot <- dyplot %>% dyShading(from=crossd[i], to=crossd[i+1], color=shadev[i])
}  # end for
# Plot the dygraph object
dyplot

# Calculate the correlation of VWAP strategy with VTI
cor(retp, pnls)
# Combine VWAP strategy with VTI
wealthv <- cbind(retp, pnls, 0.5*(retp+pnls))
colnames(wealthv) <- c("VTI", "VWAP", "Combined")
sharper <- sqrt(252)*sapply(wealthv, function (x) mean(x)/sd(x))

# Plot dygraph of VWAP strategy combined with VTI
colorv <- c("blue", "red", "purple")
dygraphs::dygraph(cumsum(wealthv)[endw],
  paste("VWAP Strategy Sharpe", paste(paste(names(sharper), round(sharper, 3), sep="="), collapse=", "))) %>%
  dyOptions(colors=colorv, strokeWidth=1) %>%
  dySeries(name="Combined", strokeWidth=3) %>%
  dyLegend(show="always", width=300)

# Test VWAP crossover market timing of VTI using Treynor-Mazuy test
desm <- cbind(pnls, retp, retp^2)
desm <- na.omit(desm)
colnames(desm) <- c("VWAP", "VTI", "Treynor")
regmod <- lm(VWAP ~ VTI + Treynor, data=desm)
summary(regmod)
# Plot residual scatterplot
resids <- (desm$VWAP - regmod$coeff["VTI"]*retp)
resids <- regmod$residuals
# x11(width=6, height=6)
plot.default(x=retp, y=resids, xlab="VTI", ylab="residuals")
title(main="Treynor-Mazuy Market Timing Test\n for VWAP Crossover vs VTI", line=0.5)
# Plot fitted (predicted) response values
coefreg <- summary(regmod)$coeff
fitv <- regmod$fitted.values - coefreg["VTI", "Estimate"]*retp
tvalue <- round(coefreg["Treynor", "t value"], 2)
points.default(x=retp, y=fitv, pch=16, col="red")
text(x=0.0, y=0.8*max(resids), paste("Treynor test t-value =", tvalue))

sim_vwap <- function(ohlc, lambdaf=0.9, bidask=0.001, trend=1, lagg=1) {
  closep <- log(quantmod::Cl(ohlc))
  volumv <- quantmod::Vo(ohlc)
  retp <- rutils::diffit(closep)
  nrows <- NROW(ohlc)
  # Calculate the VWAP prices
  vwap <- HighFreq::run_mean(closep, lambda=lambdaf, weightv=volumv)
  # Calculate the indicator
  indic <- trend*sign(closep - vwap)
  if (lagg > 1) {
    indic <- HighFreq::roll_sum(indic, lagg)
    indic[1:lagg] <- 0
  }  # end if
  # Calculate the positions, either: -1, 0, or 1
  posv <- rep(NA_integer_, nrows)
  posv[1] <- 0
  posv <- ifelse(indic == lagg, 1, posv)
  posv <- ifelse(indic == (-lagg), -1, posv)
  posv <- zoo::na.locf(posv, na.rm=FALSE)
  posv <- xts::xts(posv, order.by=zoo::index(closep))
  # Lag the positions to trade on next day
  posv <- rutils::lagit(posv, lagg=1)
  # Calculate the PnLs of strategy
  pnls <- retp*posv
  costv <- 0.5*bidask*abs(rutils::diffit(posv))
  pnls <- (pnls - costv)
  # Calculate the strategy returns
  pnls <- cbind(posv, pnls)
  colnames(pnls) <- c("positions", "pnls")
  pnls
}  # end sim_vwap

lambdav <- seq(from=0.97, to=0.995, by=0.004)
# Perform lapply() loop over lambdav
pnls <- lapply(lambdav, function(lambdaf) {
  # Simulate VWAP strategy and Calculate the returns
  sim_vwap(ohlc=ohlc, lambdaf=lambdaf, bidask=0, lagg=2)[, "pnls"]
})  # end lapply
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("lambda=", lambdav)

# Plot dygraph of multiple VWAP strategies
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls)[endw], main="Cumulative Returns of Trend Following VWAP Strategies") %>%
  dyOptions(colors=colorv, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Plot VWAP strategies with custom line colors
x11(width=6, height=5)
themev <- chart_theme()
themev$col$line.col <- colorv
quantmod::chart_Series(cumsum(pnls), theme=themev,
  name="Cumulative Returns of VWAP Strategies")
legend("topleft", legend=colnames(pnls), inset=0.1,
  bg="white", cex=0.8, lwd=rep(6, NCOL(pnls)),
  col=themev$col$line.col, bty="n")

# Dygraphs plot with custom line colors
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw], main="Dual Crossover Strategy Out-of-Sample") %>%
  dyEvent(zoo::index(wealthv[last(insample)]), label="in-sample", strokePattern="solid", color="green") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2)

# Calculate the trailing mean prices and volatilities
pricev <- log(na.omit(rutils::etfenv$prices$VTI))
lambdaf <- 0.9
volp <- HighFreq::run_var(pricev, lambda=lambdaf)
pricema <- volp[, 1]
volp <- sqrt(volp[, 2])
# Dygraphs plot of Bollinger bands
priceb <- cbind(pricev, pricema, pricema+volp, pricema-volp)
colnames(priceb)[2:4] <- c("mean", "upper", "lower")
colv <- colnames(priceb)
dygraphs::dygraph(priceb["2008-09/2009-09"], main="VTI Prices and Bollinger Bands") %>%
  dySeries(name=colv[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colv[2], strokeWidth=2, col="purple") %>%
  dySeries(name=colv[3], strokeWidth=2, strokePattern="dashed", col="green") %>%
  dySeries(name=colv[4], strokeWidth=2, strokePattern="dashed", col="red") %>%
  dyLegend(show="always", width=300)

NA

# Center the prices
pricec <- pricev - pricema
# Dygraphs plot of Bollinger bands
priceb <- cbind(pricec, volp, -volp)
colnames(priceb) <- c("price", "upper", "lower")
colv <- colnames(priceb)
dygraphs::dygraph(priceb["2008-09/2009-09"],
  main="Centered VTI Prices and Bollinger Bands") %>%
  dySeries(name=colv[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colv[2], strokeWidth=2, strokePattern="dashed", col="green") %>%
  dySeries(name=colv[3], strokeWidth=2, strokePattern="dashed", col="red") %>%
  dyLegend(show="always", width=300)

# Calculate the EMA prices and volatilities
lambdaf <- 0.1
volp <- HighFreq::run_var(pricev, lambda=lambdaf)
pricema <- volp[, 1]
volp <- sqrt(volp[, 2])
# Prepare the simulation parameters
pricen <- as.numeric(pricev) # Numeric price
pricec <- pricen - pricema # Centered price
threshv <- volp
nrows <- NROW(pricev)
posv <- integer(nrows) # Stock positions
posv[1] <- 0 # Initial position
# Calculate the positions from Bollinger bands
for (it in 2:nrows) {
  if (pricec[it-1] > threshv[it-1]) {
    # Enter short
    posv[it] <- (-1)
  } else if (pricec[it-1] < (-threshv[it-1])) {
    # Enter long
    posv[it] <- 1
  } else if ((posv[it-1] < 0) && (pricec[it-1] < 0)) {
    # Unwind short
    posv[it] <- 0
  } else if ((posv[it-1] > 0) && (pricec[it-1] > 0)) {
    # Unwind long
    posv[it] <- 0
  } else {
    # Do nothing
    posv[it] <- posv[it-1]
  }  # end if
}  # end for
# Calculate the number of trades
ntrades <- sum(abs(rutils::diffit(posv)) > 0)
# Calculate the pnls
retp <- rutils::diffit(pricev)
pnls <- retp*posv

# Calculate the Sharpe ratios
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
sharper <- sqrt(252)*sapply(wealthv, function(x) mean(x)/sd(x[x<0]))
sharper <- round(sharper, 3)
# Dygraphs plot of Bollinger strategy
colv <- colnames(wealthv)
captiont <- paste("Bollinger Strategy", "/ \n",
  paste0(paste(colv[1:2], "Sharpe =", sharper), collapse=" / "), "/ \n",
  "Number trades =", ntrades)
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw], main=captiont) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Simulate the patient Bollinger Strategy
posv <- integer(nrows) # Stock positions
posv[1] <- 0 # Initial position
for (it in 2:nrows) {
  if (pricec[it-1] > threshv[it-1]) {
    # Enter short
    posv[it] <- (-1)
  } else if (pricec[it-1] < (-threshv[it-1])) {
    # Enter long
    posv[it] <- 1
  } else {
    # Do nothing
    posv[it] <- posv[it-1]
  }  # end if
}  # end for
# Calculate the PnLs
pnl2 <- retp*posv

# Calculate the Sharpe ratios
wealthv <- cbind(pnls, pnl2)
colnames(wealthv) <- c("Bollinger", "Patient")
sharper <- sqrt(252)*sapply(wealthv, function(x) mean(x)/sd(x[x<0]))
sharper <- round(sharper, 3)
# Dygraphs plot of Bollinger strategy
colv <- colnames(wealthv)
captiont <- paste("Bollinger Strategy", "/ \n",
  paste0(paste(colv[1:2], "Sharpe =", sharper), collapse=" / "), "/ \n",
  "Number trades =", ntrades)
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw], main=captiont) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Simulate the patient Bollinger Strategy quickly
posf <- rep(NA_integer_, nrows)
posf[1] <- 0
posf <- ifelse(pricec > threshv, -1, posf)
posf <- ifelse(pricec < -threshv, 1, posf)
posf <- zoo::na.locf(posf)
# Lag the positions to trade in the next period
posf <- rutils::lagit(posf, lagg=1)
# Compare the positions
all.equal(posv, posf)

# Calculate the daytime open-to-close VTI returns
ohlc <- log(rutils::etfenv$VTI)
nrows <- NROW(ohlc)
openp <- quantmod::Op(ohlc)
highp <- quantmod::Hi(ohlc)
lowp <- quantmod::Lo(ohlc)
closep <- quantmod::Cl(ohlc)
retd <- (closep - openp)
# Calculate the cumulative daytime VTI returns
priced <- cumsum(retd)
lambdaf <- 0.1
volp <- HighFreq::run_var(priced, lambda=lambdaf)
pricema <- volp[, 1]
volp <- sqrt(volp[, 2])
# Calculate the positions from Bollinger bands
threshv <- volp
pricec <- zoo::coredata(priced - pricema)
posv <- rep(NA_integer_, nrows)
posv[1] <- 0
posv <- ifelse(pricec > threshv, -1, posv)
posv <- ifelse(pricec < -threshv, 1, posv)
posv <- zoo::na.locf(posv)
# Lag the positions to trade in the next period
posv <- rutils::lagit(posv, lagg=1)
# Calculate the number of trades and the PnLs
ntrades <- sum(abs(rutils::diffit(posv)) > 0)
pnls <- retd*posv

# Calculate the Sharpe ratios
wealthv <- cbind(retd, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
nyears <- as.numeric(end(priced)-start(priced))/365
sharper <- sqrt(nrows/nyears)*sapply(wealthv, function(x) mean(x)/sd(x[x<0]))
sharper <- round(sharper, 3)
# Dygraphs plot of Bollinger strategy
colv <- colnames(wealthv)
captiont <- paste("Bollinger Strategy for Daytime VTI", "/ \n",
  paste0(paste(colv[1:2], "Sharpe =", sharper), collapse=" / "), "/ \n",
  "Number trades =", ntrades)
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw], main=captiont) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Calculate the trailing mean prices and volatilities of SPY
pricev <- log(quantmod::Cl(HighFreq::SPY))
nrows <- NROW(pricev)
lambdaf <- 0.1
volp <- HighFreq::run_var(pricev, lambda=lambdaf)
pricema <- volp[, 1]
volp <- sqrt(volp[, 2])
# Calculate the positions from Bollinger bands
threshv <- volp
pricec <- zoo::coredata(pricev - pricema)
posv <- rep(NA_integer_, nrows)
posv[1] <- 0
posv <- ifelse(pricec > threshv, -1, posv)
posv <- ifelse(pricec < -threshv, 1, posv)
posv <- zoo::na.locf(posv)
# Lag the positions to trade in the next period
posv <- rutils::lagit(posv, lagg=1)
# Calculate the number of trades and the PnLs
ntrades <- sum(abs(rutils::diffit(posv)) > 0)
retp <- rutils::diffit(pricev)
pnls <- retp*posv
# Subtract transaction costs from the pnls
bidask <- 0.0001 # Bid-ask spread equal to 1 basis point
costv <- 0.5*bidask*abs(rutils::diffit(posv))
pnls <- (pnls - costv)

# Calculate the Sharpe ratios
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("SPY", "Strategy")
nyears <- as.numeric(end(pricev)-start(pricev))/365
sharper <- sqrt(nrows/nyears)*sapply(wealthv, function(x) mean(x)/sd(x[x<0]))
sharper <- round(sharper, 3)
# Dygraphs plot of Bollinger strategy
colv <- colnames(wealthv)
captiont <- paste("Bollinger Strategy for Minute SPY", "/ \n",
  paste0(paste(colv[1:2], "Sharpe =", sharper), collapse=" / "), "/ \n",
  "Number trades =", ntrades)
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw], main=captiont) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=100)

# Extract time series of VTI log prices
pricev <- log(na.omit(rutils::etfenv$prices$VTI))
nrows <- NROW(pricev)
# Define look-back window
lookb <- 11
# Calculate time series of trailing medians
medianv <- HighFreq::roll_mean(pricev, lookb, method="nonparametric")
# medianv <- TTR::runMedian(pricev, n=lookb)
# Calculate time series of MAD
madv <- HighFreq::roll_var(pricev, lookb=lookb, method="nonparametric")
# madv <- TTR::runMAD(pricev, n=lookb)
# Calculate time series of z-scores
zscores <- ifelse(madv > 0, (pricev - medianv)/madv, 0)
zscores[1:lookb, ] <- 0
tail(zscores, lookb)
range(zscores)

# Plot histogram of z-scores
histp <- hist(zscores, col="lightgrey",
  xlab="z-scores", breaks=50, xlim=c(-4, 4),
  ylab="frequency", freq=FALSE, main="Hampel Z-Scores histogram")
lines(density(zscores, adjust=1.5), lwd=3, col="blue")
# Dygraphs plot of Hampel bands
priceb <- cbind(pricev, medianv, medianv+madv, medianv-madv)
colnames(priceb)[2:4] <- c("median", "upper", "lower")
colv <- colnames(priceb)
dygraphs::dygraph(priceb["2008-09/2009-09"], main="VTI Prices and Hampel Bands") %>%
  dySeries(name=colv[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colv[2], strokeWidth=2, col="green") %>%
  dySeries(name=colv[3], strokeWidth=2, strokePattern="dashed", col="red") %>%
  dySeries(name=colv[4], strokeWidth=2, strokePattern="dashed", col="red") %>%
  dyLegend(show="always", width=300)

# Calculate the time series of trailing medians and MAD
lookb <- 3
medianv <- HighFreq::roll_mean(pricev, lookb, method="nonparametric")
madv <- HighFreq::roll_var(pricev, lookb=lookb, method="nonparametric")
# Calculate the time series of z-scores
zscores <- ifelse(madv > 0, (pricev - medianv)/madv, 0)
zscores[1:lookb, ] <- 0
range(zscores)
# Calculate the positions
threshv <- 1
posv <- rep(NA_integer_, nrows)
posv[1] <- 0
posv[zscores > threshv] <- (-1)
posv[zscores < -threshv] <- 1
posv <- zoo::na.locf(posv)
posv <- rutils::lagit(posv)
# Calculate the number of trades and the PnLs
ntrades <- sum(abs(rutils::diffit(posv)) > 0)
retp <- rutils::diffit(pricev)
pnls <- retp*posv

# Calculate the Sharpe ratios
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
sharper <- sqrt(252)*sapply(wealthv, function(x) mean(x)/sd(x[x<0]))
sharper <- round(sharper, 3)
# Dygraphs plot of Hampel strategy
captiont <- paste("Hampel Strategy", "/ \n",
  paste0(paste(colv[1:2], "Sharpe =", sharper), collapse=" / "), "/ \n",
  "Number trades =", ntrades)
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
colv <- colnames(wealthv)
dygraphs::dygraph(cumsum(wealthv)[endw], main=captiont) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=100)

# Calculate the trailing mean prices and volatilities of SPY
pricev <- log(quantmod::Cl(HighFreq::SPY))
nrows <- NROW(pricev)
# Calculate the price medians and MAD
lookb <- 3
medianv <- HighFreq::roll_mean(pricev, lookb, method="nonparametric")
madv <- HighFreq::roll_var(pricev, lookb=lookb, method="nonparametric")
# Calculate the time series of z-scores
zscores <- ifelse(madv > 0, (pricev - medianv)/madv, 0)
zscores[1:lookb, ] <- 0
# Calculate the positions
threshv <- 1
posv <- rep(NA_integer_, nrows)
posv[1] <- 0
posv[zscores < -threshv] <- 1
posv[zscores > threshv] <- (-1)
posv <- zoo::na.locf(posv)
posv <- rutils::lagit(posv)
# Calculate the number of trades and the PnLs
ntrades <- sum(abs(rutils::diffit(posv)) > 0)
retp <- rutils::diffit(pricev)
pnls <- retp*posv
# Subtract transaction costs from the pnls
costv <- 0.5*bidask*abs(rutils::diffit(posv))
pnls <- (pnls - costv)

# Calculate the Sharpe ratios
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("SPY", "Strategy")
nyears <- as.numeric(end(pricev)-start(pricev))/365
sharper <- sqrt(nrows/nyears)*sapply(wealthv, function(x) mean(x)/sd(x[x<0]))
sharper <- round(sharper, 3)
# Dygraphs plot of Hampel strategy
colv <- colnames(wealthv)
captiont <- paste("Hampel Strategy for Minute SPY", "/ \n",
  paste0(paste(colv[1:2], "Sharpe =", sharper), collapse=" / "), "/ \n",
  "Number trades =", ntrades)
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw], main=captiont) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=100)

# Create a plotting expression
expv <- quote({
  degf <- 2:20
  rangev <- (1:NROW(degf))
  indeks <- 4
  # Plot a curve
  curve(expr=dchisq(x, df=degf[indeks]),
xlim=c(0, 30), ylim=c(0, 0.2),
xlab="", ylab="", lwd=3, col="red")
  # Add grey lines to plot
  for (it in rangev[-indeks]) {
    curve(expr=dchisq(x, df=degf[it]),
  xlim=c(0, 30), ylim=c(0, 0.2),
  xlab="", ylab="", lwd=2, col="grey80", add=TRUE)
  }  # end for
  # Add title
  title(main="Chi-squared Distributions", line=-1.5, cex.main=1.5)
  # Add legend
  text(x=20, y=0.15, labels=paste0("Degrees of freedom=",
      degf[indeks]), pos=1, cex=1.3)
})  # end quote

# View the plotting expression
expv
# Create plot by evaluating the plotting expression
x11(width=6, height=4)
eval(expv)

library(animation)
# Create an expression for creating multiple plots
expv <- quote({
  degf <- 2:20
  rangev <- (1:NROW(degf))
  # Set image refesh interval
  animation::ani.options(interval=0.5)
  # Create multiple plots with curves
  for (indeks in rangev) {
    curve(expr=dchisq(x, df=degf[indeks]),
  xlim=c(0, 30), ylim=c(0, 0.2),
  xlab="", ylab="", lwd=3, col="red")
    # Add grey lines to plot
    for (it in rangev[-indeks]) {
      curve(expr=dchisq(x, df=degf[it]),
    xlim=c(0, 30), ylim=c(0, 0.2),
    xlab="", ylab="", lwd=2, col="grey80", add=TRUE)
    }  # end for
    # Add title
    title(main="Chi-squared Distributions", line=-1.5, cex.main=1.5)
    # Add legend
    text(x=20, y=0.15, labels=paste0("Degrees of freedom=",
      degf[indeks]), pos=1, cex=1.3)
  }  # end for
})  # end quote

# Create plot by evaluating the plotting expression
x11(width=6, height=4)
eval(expv)
# Create gif with animated plot
animation::saveGIF(expr=eval(expv),
  movie.name="chi_squared.gif",
  img.name="chi_squared")
# Create html with animated plot
animation::saveHTML(expr=eval(expv),
  img.name="chi_squared",
  htmlfile="chi_squared.html",
  description="Chi-squared Distributions")  # end saveHTML

NA

#App setup code that runs only once at startup.
ndata <- 1e4
stdev <- 1.0

#Define the user interface
uiface <- shiny::fluidPage(
  # Create numeric input for the number of data points.
  numericInput("ndata", "Number of data points:", value=ndata),
  # Create slider input for the standard deviation parameter.
  sliderInput("stdev", label="Standard deviation:",
        min=0.1, max=3.0, value=stdev, step=0.1),
  # Render plot in a panel.
  plotOutput("plotobj", height=300, width=500)
)  # end user interface

#Define the server function
servfun <- function(input, output) {
  output$plotobj <- shiny::renderPlot({
    # Simulate the data
    datav <- rnorm(input$ndata, sd=input$stdev)
    # Plot the data
    par(mar=c(2, 4, 4, 0), oma=c(0, 0, 0, 0))
    hist(datav, xlim=c(-4, 4), main="Histogram of Random Data")
  })  # end renderPlot
}  # end servfun

# Return a Shiny app object
shiny::shinyApp(ui=uiface, server=servfun)

#Create elements of the user interface
uiface <- shiny::fluidPage(
  titlePanel("VWAP Moving Average"),
  # Create single row of widgets with two slider inputs
  fluidRow(
    # Input stock symbol
    column(width=3, selectInput("symbol", label="Symbol",
                          choices=symbolv, selected=symbol)),
    # Input look-back interval
    column(width=3, sliderInput("lookb", label="Lookback interval",
                          min=1, max=150, value=11, step=1))
  ),  # end fluidRow
  # Create output plot panel
  mainPanel(dygraphs::dygraphOutput("dyplot"), width=12)
)  # end fluidPage interface

#Define the server function
servfun <- shiny::shinyServer(function(input, output) {
  # Get the close and volume data in a reactive environment
  closep <- shiny::reactive({
    # Get the data
    ohlc <- get(input$symbol, data_env)
    closep <- log(quantmod::Cl(ohlc))
    volum <- quantmod::Vo(ohlc)
    # Return the data
    cbind(closep, volum)
  })  # end reactive code

  # Calculate the VWAP indicator in a reactive environment
  vwapv <- shiny::reactive({
    # Get model parameters from input argument
    lookb <- input$lookb
    # Calculate the VWAP indicator
    closep <- closep()[, 1]
    volum <- closep()[, 2]
    vwapv <- HighFreq::roll_sum(tseries=closep*volum, lookb=lookb)
    volumroll <- HighFreq::roll_sum(tseries=volum, lookb=lookb)
    vwapv <- vwapv/volumroll
    vwapv[is.na(vwapv)] <- 0
    # Return the plot data
    datav <- cbind(closep, vwapv)
    colnames(datav) <- c(input$symbol, "VWAP")
    datav
  })  # end reactive code

  # Return the dygraph plot to output argument
  output$dyplot <- dygraphs::renderDygraph({
    colv <- colnames(vwapv())
    dygraphs::dygraph(vwapv(), main=paste(colv[1], "VWAP")) %>%
dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
dySeries(name=colv[1], axis="y", label=colv[1], strokeWidth=2, col="blue") %>%
dySeries(name=colv[2], axis="y2", label=colv[2], strokeWidth=2, col="red")
  })  # end output plot
})  # end server code

#Return a Shiny app object
shiny::shinyApp(ui=uiface, server=servfun)

#Define the server function
servfun <- shiny::shinyServer(function(input, output) {

  # Create an empty list of reactive values.
  value_s <- reactiveValues()

  # Get input parameters from the user interface.
  nrows <- reactive({
    # Add nrows to list of reactive values.
    value_s*nrows <- input$nrows
    input$nrows
  })  # end reactive code

  # Broadcast a message to the console when the button is pressed.
  observeEvent(eventExpr=input$button, handlerExpr={
    cat("Input button pressed\n")
  })  # end observeEvent

  # Send the data when the button is pressed.
  datav <- eventReactive(eventExpr=input$button, valueExpr={
    # eventReactive() executes on input$button, but not on nrows() or input$nrows.
    cat("Sending", nrows(), "rows of data\n")
    datav <- head(mtcars, input$nrows)
    value_s$mpg <- mean(datav$mpg)
    datav
  })  # end eventReactive
  #   datav

  # Draw table of the data when the button is pressed.
  observeEvent(eventExpr=input$button, handlerExpr={
    datav <- datav()
    cat("Received", value_s*nrows, "rows of data\n")
    cat("Average mpg = ", value_s$mpg, "\n")
    cat("Drawing table\n")
    output$tablev <- renderTable(datav)
  })  # end observeEvent

})  # end server code

#Return a Shiny app object
shiny::shinyApp(ui=uiface, server=servfun)
