# Test if IEF can time VTI
returns <- na.omit(rutils::etfenv$returns[, c("IEF", "VTI")])
vti <- returns$VTI
design <- cbind(returns, 0.5*(vti+abs(vti)), vti^2)
colnames(design)[3:4] <- c("merton", "treynor")

# Merton-Henriksson test
model <- lm(IEF ~ VTI + merton, data=design); summary(model)
# Treynor-Mazuy test
model <- lm(IEF ~ VTI + treynor, data=design); summary(model)
# Plot residual scatterplot
x11(width=6, height=5)
residuals <- (design$IEF - model$coeff[2]*design$VTI)
plot.default(x=design$VTI, y=residuals, xlab="VTI", ylab="IEF")
title(main="Treynor-Mazuy Market Timing Test\n for IEF vs VTI", line=0.5)
# Plot fitted (predicted) response values
fittedv <- (model$coeff["(Intercept)"] +
        model$coeff["treynor"]*vti^2)
points.default(x=design$VTI, y=fittedv, pch=16, col="red")
text(x=0.05, y=0.8*max(residuals), paste("Treynor test t-value =", round(summary(model)$coeff["treynor", "t value"], 2)))

library(rutils)
# Extract ETF prices from rutils::etfenv$prices
prices <- rutils::etfenv$prices
prices <- zoo::na.locf(prices, na.rm=FALSE)
prices <- zoo::na.locf(prices, fromLast=TRUE)
dates <- index(prices)
# Calculate simple dollar returns
retsd <- rutils::diffit(prices)
# Or
# retsd <- lapply(prices, rutils::diffit)
# retsd <- rutils::do_call(cbind, retsd)
# Calculate percentage returns
retsp <- retsd/rutils::lagit(prices, lagg=1, pad_zeros=FALSE)
# Calculate log returns
retsl <- rutils::diffit(log(prices))

# Set the initial dollar returns
retsd[1, ] <- prices[1, ]
# Calculate prices from dollar returns
pricesn <- cumsum(retsd)
all.equal(pricesn, prices)
# Compound the percentage returns
pricesn <- cumprod(1+retsp)
# Set the initial prices
pricesi <- as.numeric(prices[1, ])
pricesn <- lapply(1:NCOL(pricesn), function (i)
    pricesi[i]*pricesn[, i])
pricesn <- rutils::do_call(cbind, pricesn)
# Or
# pricesn <- t(t(pricesn)*pricesi)
all.equal(pricesn, prices, check.attributes=FALSE)
# Plot log VTI prices
dygraphs::dygraph(log(quantmod::Cl(rutils::etfenv$VTI)),
  main="Logarithm of VTI Prices") %>%
  dyOptions(colors="blue", strokeWidth=2) %>%
  dyLegend(show="always", width=500)

# Calculate percentage VTI returns
prices <- rutils::etfenv$prices$VTI
prices <- na.omit(prices)
returns <- rutils::diffit(prices)/
  rutils::lagit(prices, lagg=1, pad_zeros=FALSE)

# Funding rate per day
frate <- 0.01/252
# Margin account
margin <- cumsum(returns)
# Cumulative funding costs
fcosts <- cumsum(frate*margin)
# Add funding costs to margin account
margin <- (margin + fcosts)
# dygraph plot of margin and funding costs
datav <- cbind(margin, fcosts)
colnamev <- c("Margin", "Cumulative Funding")
colnames(datav) <- colnamev
dygraphs::dygraph(datav, main="VTI Margin Funding Costs") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", col="red", strokeWidth=3) %>%
  dyLegend(show="always", width=500)

# bid_offer equal to 10 bps for liquid ETFs
bid_offer <- 0.001
# Cumulative transaction costs
costs <- bid_offer*cumsum(abs(returns))/2
# Subtract transaction costs from margin account
margin <- cumsum(returns)
margin <- (margin - costs)
# dygraph plot of margin and transaction costs
datav <- cbind(margin, costs)
colnamev <- c("Margin", "Cumulative Transaction Costs")
colnames(datav) <- colnamev
dygraphs::dygraph(datav, main="VTI Transaction Costs") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", col="red", strokeWidth=3) %>%
  dyLegend(show="always", width=500)

# Calculate VTI and IEF dollar returns
prices <- rutils::etfenv$prices[, c("VTI", "IEF")]
prices <- na.omit(prices)
retsd <- rutils::diffit(prices)
# Calculate VTI and IEF percentage returns
retsp <- retsd/rutils::lagit(prices, lagg=1, pad_zeros=FALSE)
# Set the initial dollar returns
retsd[1, ] <- prices[1, ]

# Wealth of fixed shares equal to $0.5 each (without rebalancing)
weights <- c(0.5, 0.5)  # dollar weights
# Scale the dollar returns using the dollar weights
pricesi <- as.numeric(prices[1, ])
wealth_fsa <- cumsum(retsd %*% (weights/pricesi))
# Or using percentage returns
wealth_fsa2 <- drop(cumprod(1+retsp) %*% weights)
all.equal(wealth_fsa, wealth_fsa2)
# Wealth of fixed dollars (with rebalancing)
wealth_fda <- cumsum(retsp %*% weights)
# Calculate the Sharpe and Sortino ratios
wealth <- cbind(wealth_fda, wealth_fsa)
wealth <- xts::xts(wealth, zoo::index(prices))
colnames(wealth) <- c("Fixed dollars", "Fixed shares")
sqrt(252)*sapply(rutils::diffit(wealth),
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot the log wealth
colnamev <- colnames(wealth)
dygraphs::dygraph(wealth, main="Wealth of Weighted Portfolios") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="red", strokeWidth=2) %>%
  dySeries(name=colnamev[2], axis="y2", col="blue", strokeWidth=2) %>%
  dyLegend(show="always", width=500)

# Margin account for fixed dollars (with rebalancing)
margin <- cumsum(retsp %*% weights)
# Cumulative transaction costs
costs <- bid_offer*cumsum(abs(retsp) %*% weights)/2
# Subtract transaction costs from margin account
margin <- (margin - costs)
# dygraph plot of margin and transaction costs
datav <- cbind(margin, costs)
datav <- xts::xts(datav, zoo::index(prices))
colnamev <- c("Margin", "Cumulative Transaction Costs")
colnames(datav) <- colnamev
dygraphs::dygraph(datav, main="Fixed Dollar Portfolio Transaction Costs") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", col="red", strokeWidth=3) %>%
  dyLegend(show="always", width=500)

# Wealth of fixed shares (without rebalancing)
wealth_fsa <- cumsum(retsd %*% (weights/pricesi))
# Or compound the percentage returns
wealth_fsa <- drop(apply(retsp, 2,
  function(x) cumprod(1+x)) %*% weights)-1
# Wealth of proportional allocations (with rebalancing)
wealth_pda <- cumprod(1 + retsp %*% weights) - 1
# Plot log wealth
wealth <- cbind(wealth_fsa, wealth_pda)
wealth <- xts::xts(wealth, zoo::index(prices))
colnames(wealth) <- c("Fixed Shares", "Proportional Allocations")
dygraphs::dygraph(wealth, main="Wealth of Proportional Allocations") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)

# Returns in excess of weighted returns
rets_weighted <- retsp %*% weights
excess <- lapply(retsp, function(x) (rets_weighted - x))
excess <- do.call(cbind, excess)
sum(excess %*% weights)
# Calculate weighted sum of absolute excess returns
excess <- abs(excess) %*% weights
# Total dollar amount of stocks that need to be traded
excess <- excess*rutils::lagit(wealth_pda)
# Cumulative transaction costs
costs <- bid_offer*cumsum(excess)/2
# Subtract transaction costs from wealth
wealth_pda <- (wealth_pda - costs)

# dygraph plot of wealth and transaction costs
wealth <- cbind(wealth_pda, costs)
wealth <- xts::xts(wealth, zoo::index(prices))
colnamev <- c("Wealth", "Cumulative Transaction Costs")
colnames(wealth) <- colnamev
dygraphs::dygraph(wealth, main="Transaction Costs With Proportional Allocations") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", col="red", strokeWidth=3) %>%
  dyLegend(show="always", width=500)

# Wealth of fixed shares (without rebalancing)
wealth_fsa <- drop(apply(retsp, 2, function(x) cumprod(1+x)) %*% weights)-1
# Wealth of proportional dollar allocations (with rebalancing)
wealth_pda <- cumprod(1 + retsp %*% weights) - 1
# Wealth of proportional target allocation (with rebalancing)
retsp <- zoo::coredata(retsp)
threshold <- 0.05
wealth <- matrix(nrow=NROW(retsp), ncol=2)
colnames(wealth) <- colnames(retsp)
wealth[1, ] <- weights
for (it in 2:NROW(retsp)) {
  # Accrue wealth without rebalancing
  wealth[it, ] <- wealth[it-1, ]*(1 + retsp[it, ])
  # Rebalance if wealth allocations differ from weights
  if (sum(abs(wealth[it, ] - sum(wealth[it, ])*weights))/sum(wealth[it, ]) > threshold) {
    # cat("Rebalance at:", it, "\n")
    wealth[it, ] <- sum(wealth[it, ])*weights
  } # end if
} # end for
wealth <- rowSums(wealth) - 1
wealth <- cbind(wealth_pda, wealth)
wealth <- xts::xts(wealth, zoo::index(prices))
colnames(wealth) <- c("Proportional Allocations", "Proportional Target")
dygraphs::dygraph(wealth, main="Wealth of Proportional Target Allocations") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)

# Calculate stock and bond returns
returns <- na.omit(rutils::etfenv$returns[, c("VTI", "IEF")])
weights <- c(0.4, 0.6)
returns <- cbind(returns, returns %*% weights)
colnames(returns)[3] <- "Combined"
# Calculate correlations
cor(returns)
# Calculate Sharpe ratios
sqrt(252)*sapply(returns, function(x) mean(x)/sd(x))
# Calculate standard deviation, skewness, and kurtosis
sapply(returns, function(x) {
  # Calculate standard deviation
  stddev <- sd(x)
  # Standardize the returns
  x <- (x - mean(x))/stddev
  c(stddev=stddev, skew=mean(x^3), kurt=mean(x^4))
})  # end sapply

# Wealth of proportional allocations
wealth <- cumprod(1 + returns)
# Plot cumulative wealth
dygraphs::dygraph(log(wealth), main="Stocks and Bonds With Proportional Allocations") %>%
  dyOptions(colors=c("blue", "green", "blue", "red")) %>%
  dySeries("Combined", color="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)

# Extract ETF returns
symbolv <- c("VTI", "IEF", "DBC")
returns <- na.omit(rutils::etfenv$returns[, symbolv])
# Calculate all-weather portfolio wealth
weightsaw <- c(0.30, 0.55, 0.15)
returns <- cbind(returns, returns %*% weightsaw)
colnames(returns)[4] <- "All Weather"
# Calculate Sharpe ratios
sqrt(252)*sapply(returns, function(x) mean(x)/sd(x))

# Calculate cumulative wealth from returns
wealth <- cumprod(1+returns)
# dygraph all-weather wealth
dygraphs::dygraph(wealth, main="All-Weather Portfolio") %>%
  dyOptions(colors=c("blue", "green", "orange", "red")) %>%
  dySeries("All Weather", color="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Plot all-weather wealth
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue", "green", "red")
quantmod::chart_Series(wealth, theme=plot_theme, lwd=c(2, 2, 2, 4),
       name="All-Weather Portfolio")
legend("topleft", legend=colnames(wealth),
  inset=0.1, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")

# Calculate VTI returns
returns <- na.omit(rutils::etfenv$returns$VTI["2008/2009"])
dates <- index(returns)
nrows <- NROW(returns)
returns <- drop(zoo::coredata(returns))
bfloor <- 60  # bond floor
coeff <- 2  # multiplier
portf_value <- numeric(nrows)
portf_value[1] <- 100  # principal
stock_value <- numeric(nrows)
stock_value[1] <- coeff*(portf_value[1] - bfloor)
bond_value <- numeric(nrows)
bond_value[1] <- (portf_value[1] - stock_value[1])

# Simulate CPPI strategy
for (t in 2:nrows) {
  portf_value[t] <- portf_value[t-1] + stock_value[t-1]*returns[t]
  stock_value[t] <- min(coeff*(portf_value[t] - bfloor), portf_value[t])
  bond_value[t] <- (portf_value[t] - stock_value[t])
}  # end for
# dygraph plot of CPPI strategy
vti <- 100*cumprod(1+returns)
datav <- xts::xts(cbind(stock_value, bond_value, portf_value, vti), dates)
colnames(datav) <- c("stocks", "bonds", "CPPI", "VTI")
dygraphs::dygraph(datav, main="CPPI strategy") %>%
  dyOptions(colors=c("red", "green", "blue", "orange"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Calculate dollar and percentage returns for VTI and IEF.
prices <- rutils::etfenv$prices[, c("VTI", "IEF")]
prices <- na.omit(prices)
retsd <- rutils::diffit(prices)
retsp <- retsd/rutils::lagit(prices, lagg=1, pad_zeros=FALSE)
# Calculate wealth of proportional allocations.
weights <- c(0.5, 0.5)
rets_weighted <- retsp %*% weights
wealth_pda <- cumprod(1 + rets_weighted)
# Calculate rolling percentage volatility.
look_back <- 21
volat <- roll::roll_sd(retsp, width=look_back)
volat <- zoo::na.locf(volat, na.rm=FALSE)
volat <- zoo::na.locf(volat, fromLast=TRUE)
# Calculate the risk parity portfolio allocations.
alloc <- lapply(1:NCOL(prices),
  function(x) weights[x]/volat[, x])
alloc <- do.call(cbind, alloc)
# Scale allocations to 1 dollar total.
alloc <- alloc/rowSums(alloc)
# Lag the allocations
alloc <- rutils::lagit(alloc)
# Calculate wealth of risk parity.
rets_weighted <- rowSums(retsp*alloc)
wealth_risk_parity <- cumprod(1 + rets_weighted)

# Calculate the log wealths.
wealth <- log(cbind(wealth_pda, wealth_risk_parity))
wealth <- xts::xts(wealth, zoo::index(prices))
colnames(wealth) <- c("Fixed Ratio", "Risk Parity")
# Calculate the Sharpe ratios.
sqrt(252)*sapply(rutils::diffit(wealth), function (x) mean(x)/sd(x))
# Plot a dygraph of the log wealths.
dygraphs::dygraph(wealth, main="Log Wealth of Risk Parity vs Proportional Allocations") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)

# Test risk parity market timing of VTI using Treynor-Mazuy test
returns <- rutils::diffit(wealth)
vti <- retsp$VTI
design <- cbind(returns, vti, vti^2)
design <- na.omit(design)
colnames(design)[1:2] <- c("fixed", "risk_parity")
colnames(design)[4] <- "treynor"
model <- lm(risk_parity ~ VTI + treynor, data=design)
summary(model)
# Plot residual scatterplot
residuals <- (design$risk_parity - model$coeff[2]*design$VTI)
residuals <- model$residuals
x11(width=6, height=5)
plot.default(x=design$VTI, y=residuals, xlab="VTI", ylab="residuals")
title(main="Treynor-Mazuy Market Timing Test\n for Risk Parity vs VTI", line=0.5)
# Plot fitted (predicted) response values
fittedv <- (model$coeff["(Intercept)"] + model$coeff["treynor"]*vti^2)
points.default(x=design$VTI, y=fittedv, pch=16, col="red")
text(x=0.05, y=0.8*max(residuals), paste("Risk Parity t-value =", round(summary(model)$coeff["treynor", "t value"], 2)))

# Test for fixed ratio market timing of VTI using Treynor-Mazuy test
model <- lm(fixed ~ VTI + treynor, data=design)
summary(model)
# Plot fitted (predicted) response values
fittedv <- (model$coeff["(Intercept)"] + model$coeff["treynor"]*vti^2)
points.default(x=design$VTI, y=fittedv, pch=16, col="blue")
text(x=0.05, y=0.8*max(residuals), paste("Fixed Ratio t-value =", round(summary(model)$coeff["treynor", "t value"], 2)))

# Calculate positions
vti <- na.omit(rutils::etfenv$returns$VTI)
posit <- rep(NA_integer_, NROW(vti))
dates <- index(vti)
dates <- format(dates, "%m-%d")
posit[dates == "05-01"] <- 0
posit[dates == "05-03"] <- 0
posit[dates == "11-01"] <- 1
posit[dates == "11-03"] <- 1
# Carry forward and backward non-NA posit
posit <- zoo::na.locf(posit, na.rm=FALSE)
posit <- zoo::na.locf(posit, fromLast=TRUE)
# Calculate strategy returns
sell_inmay <- posit*vti
wealth <- cbind(vti, sell_inmay)
colnames(wealth) <- c("VTI", "sell_in_may")
# Calculate Sharpe and Sortino ratios
sqrt(252)*sapply(wealth,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))

# Plot wealth of Sell in May strategy
dygraphs::dygraph(cumsum(wealth), main="Sell in May Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# OR: Open x11 for plotting
x11(width=6, height=5)
par(mar=c(4, 4, 3, 1), oma=c(0, 0, 0, 0))
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("blue", "red")
quantmod::chart_Series(wealth, theme=plot_theme, name="Sell in May Strategy")
legend("topleft", legend=colnames(wealth),
  inset=0.1, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")

# Test if Sell in May strategy can time VTI
design <- cbind(vti, 0.5*(vti+abs(vti)), vti^2)
colnames(design) <- c("VTI", "merton", "treynor")
# Perform Merton-Henriksson test
model <- lm(sell_inmay ~ VTI + merton, data=design)
summary(model)
# Perform Treynor-Mazuy test
model <- lm(sell_inmay ~ VTI + treynor, data=design)
summary(model)
# Plot Treynor-Mazuy residual scatterplot
residuals <- (sell_inmay - model$coeff[2]*vti)
plot.default(x=vti, y=residuals, xlab="VTI", ylab="residuals")
title(main="Treynor-Mazuy Market Timing Test\n for Sell in May vs VTI", line=0.5)
# Plot fitted (predicted) response values
fittedv <- (model$coeff["(Intercept)"] +
        model$coeff["treynor"]*vti^2)
points.default(x=vti, y=fittedv, pch=16, col="red")
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
wealth <- cbind(close_close, close_open, open_close)
sqrt(252)*sapply(wealth,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot log wealth
dygraphs::dygraph(cumsum(wealth),
  main="Wealth of Close-to-Close, Close-to-Open, and Open-to-Close Strategies") %>%
  dySeries(name="close_close", label="Close-to-Close (static)", strokeWidth=2, col="blue") %>%
  dySeries(name="close_open", label="Close-to-Open (overnight)", strokeWidth=2, col="red") %>%
  dySeries(name="open_close", label="Open-to-Close (daytime)", strokeWidth=2, col="green") %>%
  dyLegend(width=600)

# Calculate the VTI returns
vti <- na.omit(rutils::etfenv$returns$VTI)
dates <- zoo::index(vti)
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
pnls <- numeric(NROW(vti))
pnls[indeks] <- vti[indeks, ]

# Combine data
wealth <- cbind(vti, pnls)
colnamev <- c("VTI", "Strategy")
colnames(wealth) <- colnamev
# Calculate Sharpe and Sortino ratios
sqrt(252)*sapply(wealth,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# dygraph plot VTI Turn of the Month strategy
dygraphs::dygraph(cumsum(wealth), main="Turn of the Month Strategy") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", strokeWidth=2, col="red")

# Calculate the VTI returns
vti <- na.omit(rutils::etfenv$returns$VTI)
dates <- zoo::index(vti)
vti <- drop(coredata(vti))
nrows <- NROW(vti)
# Simulate stop-loss strategy
stopl <- 0.05
maxp <- 0.0
cumrets <- 0.0
pnls <- vti
for (i in 1:nrows) {
# Calculate drawdown
  cumrets <- cumrets + vti[i]
  maxp <- max(maxp, cumrets)
  dd <- (cumrets - maxp)
# Check for stop-loss
  if (dd < -stopl*maxp)
    pnls[i+1] <- 0
}  # end for
# Same but without using explicit loops
cumsumv <- cumsum(vti)
cummaxv <- cummax(cumsum(vti))
dd <- (cumsumv - cummaxv)
pnls2 <- vti
isdd <- rutils::lagit(dd < -stopl*cummaxv)
pnls2 <- ifelse(isdd, 0, pnls2)
all.equal(pnls, pnls2)

# Combine data
wealth <- xts::xts(cbind(vti, pnls), dates)
colnamev <- c("VTI", "Strategy")
colnames(wealth) <- colnamev
# Calculate Sharpe and Sortino ratios
sqrt(252)*sapply(wealth,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# dygraph plot VTI stop-loss strategy
dygraphs::dygraph(cumsum(wealth), main="VTI Stop-loss Strategy") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", strokeWidth=2, col="red")

# Simulate multiple stop-loss strategies
cumsumv <- cumsum(vti)
cummaxv <- cummax(cumsum(vti))
dd <- (cumsumv - cummaxv)
cum_pnls <- sapply(0.01*(1:20), function(stopl) {
  pnls <- vti
  isdd <- rutils::lagit(dd < -stopl*cummaxv)
  pnls <- ifelse(isdd, 0, pnls)
  sum(pnls)
})  # end sapply

# Plot cumulative pnls for stop-loss strategies
plot(x=0.01*(1:20), y=cum_pnls,
     main="Cumulative PnLs for Stop-loss Strategies",
     xlab="stop-loss level", ylab="cumulative pnl",
     t="l", lwd=3, col="blue")

NA

App setup code that runs only once at startup.
ndata <- 1e4
stdev <- 1.0

Define the user interface
uiface <- shiny::fluidPage(
  # Create numeric input for the number of data points.
  numericInput("ndata", "Number of data points:", value=ndata),
  # Create slider input for the standard deviation parameter.
  sliderInput("stdev", label="Standard deviation:",
        min=0.1, max=3.0, value=stdev, step=0.1),
  # Render plot in a panel.
  plotOutput("plotobj", height=300, width=500)
)  # end user interface

Define the server function
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

Create elements of the user interface
uiface <- shiny::fluidPage(
  titlePanel("VWAP Moving Average"),
  # Create single row of widgets with two slider inputs
  fluidRow(
    # Input stock symbol
    column(width=3, selectInput("symbol", label="Symbol",
                          choices=symbolv, selected=symbol)),
    # Input look-back interval
    column(width=3, sliderInput("look_back", label="Lookback interval",
                          min=1, max=150, value=11, step=1))
  ),  # end fluidRow
  # Create output plot panel
  mainPanel(dygraphs::dygraphOutput("dyplot"), width=12)
)  # end fluidPage interface

Define the server function
servfun <- shiny::shinyServer(function(input, output) {
  # Get the close and volume data in a reactive environment
  closep <- shiny::reactive({
    # Get the data
    ohlc <- get(input$symbol, data_env)
    closep <- log(quantmod::Cl(ohlc))
    volumes <- quantmod::Vo(ohlc)
    # Return the data
    cbind(closep, volumes)
  })  # end reactive code

  # Calculate the VWAP indicator in a reactive environment
  vwapv <- shiny::reactive({
    # Get model parameters from input argument
    look_back <- input$look_back
    # Calculate the VWAP indicator
    closep <- closep()[, 1]
    volumes <- closep()[, 2]
    vwapv <- HighFreq::roll_sum(se_ries=closep*volumes, look_back=look_back)
    volume_rolling <- HighFreq::roll_sum(se_ries=volumes, look_back=look_back)
    vwapv <- vwapv/volume_rolling
    vwapv[is.na(vwapv)] <- 0
    # Return the plot data
    datav <- cbind(closep, vwapv)
    colnames(datav) <- c(input$symbol, "VWAP")
    datav
  })  # end reactive code

  # Return the dygraph plot to output argument
  output$dyplot <- dygraphs::renderDygraph({
    colnamev <- colnames(vwapv())
    dygraphs::dygraph(vwapv(), main=paste(colnamev[1], "VWAP")) %>%
dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col="blue") %>%
dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=2, col="red")
  })  # end output plot
})  # end server code

Return a Shiny app object
shiny::shinyApp(ui=uiface, server=servfun)

set.seed(1121)  # Reset random number generator
# Sample from Standard Normal Distribution
nrows <- 1000
datav <- rnorm(nrows)
# Sample mean - MC estimate
mean(datav)
# Sample standard deviation - MC estimate
sd(datav)
# Monte Carlo estimate of cumulative probability
pnorm(1)
sum(datav < 1)/nrows
# Monte Carlo estimate of quantile
confl <- 0.98
qnorm(confl)  # Exact value
cutoff <- confl*nrows
datav <- sort(datav)
datav[cutoff]  # Naive Monte Carlo value
quantile(datav, probs=confl)
# Analyze the source code of quantile()
stats:::quantile.default
# Microbenchmark quantile
library(microbenchmark)
summary(microbenchmark(
  monte_carlo = datav[cutoff],
  quantilev = quantile(datav, probs=confl),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary

# Define daily volatility and growth rate
sigmav <- 0.01; drift <- 0.0; nrows <- 1000
# Simulate geometric Brownian motion
returns <- sigmav*rnorm(nrows) + drift - sigmav^2/2
prices <- exp(cumsum(returns))
plot(prices, type="l", xlab="time", ylab="prices",
     main="geometric Brownian motion")

# Simulate geometric Brownian motion
sigmav <- 0.01/sqrt(48)
drift <- 0.0
nrows <- 1e4
dates <- seq(from=as.POSIXct(paste(Sys.Date()-250, "09:30:00")),
  length.out=nrows, by="30 min")
prices <- exp(cumsum(sigmav*rnorm(nrows) + drift - sigmav^2/2))
prices <- xts(prices, order.by=dates)
prices <- cbind(prices,
  volume=sample(x=10*(2:18), size=nrows, replace=TRUE))
# Aggregate to daily OHLC data
ohlc <- xts::to.daily(prices)
quantmod::chart_Series(ohlc, name="random prices")
# dygraphs candlestick plot using pipes syntax
library(dygraphs)
dygraphs::dygraph(ohlc[, 1:4]) %>% dyCandlestick()
# dygraphs candlestick plot without using pipes syntax
dygraphs::dyCandlestick(dygraphs::dygraph(ohlc[, 1:4]))

# Standard deviations of log-normal distribution
sigmavs <- c(0.5, 1, 1.5)
# Create plot colors
colors <- c("black", "red", "blue")
# Plot all curves
for (indeks in 1:NROW(sigmavs)) {
  curve(expr=dlnorm(x, sdlog=sigmavs[indeks]),
  type="l", lwd=2, xlim=c(0, 3),
  xlab="", ylab="", col=colors[indeks],
  add=as.logical(indeks-1))
}  # end for

# Add title and legend
title(main="Log-normal Distributions", line=0.5)
legend("topright", inset=0.05, title="Sigmas",
 paste("sigma", sigmavs, sep="="),
 cex=0.8, lwd=2, lty=rep(1, NROW(sigmavs)),
 col=colors)

x11(width=6, height=5)
par(mar=c(4, 4, 3, 1))
# Return volatility of VTI etf
sigmav <- sd(rutils::diffit(log(rutils::etfenv$VTI[, 4])))
sigma2 <- sigmav^2
nrows <- NROW(rutils::etfenv$VTI)
# Standard deviation of log-normal prices
sqrt(nrows)*sigmav

# Skewness of log-normal prices
skew_ness <- function(t) {
  ex_p <- exp(t*sigma2)
  (ex_p + 2)*sqrt(ex_p - 1)
}  # end skew_ness
curve(expr=skew_ness, xlim=c(1, nrows), lwd=3,
xlab="Number of days", ylab="Skewness", col="blue",
main="Skewness of Log-normal Prices
as a Function of Time")

# Probability that random log-normal price will be lower than the mean price
curve(expr=pnorm(sigmav*sqrt(x)/2),
xlim=c(1, nrows), lwd=3,
xlab="Number of days", ylab="Probability", col="blue",
main="Probability That Random Log-normal Price
Will be Lower Than the Mean Price")

# Define daily volatility and growth rate
sigmav <- 0.01; drift <- 0.0; nrows <- 5000
paths <- 10
# Simulate multiple paths of geometric Brownian motion
prices <- matrix(rnorm(paths*nrows, sd=sigmav) +
    drift - sigmav^2/2, nc=paths)
prices <- exp(matrixStats::colCumsums(prices))
# Create xts time series
prices <- xts(prices, order.by=seq.Date(Sys.Date()-NROW(prices)+1, Sys.Date(), by=1))
# Plot xts time series
colors <- colorRampPalette(c("red", "blue"))(NCOL(prices))
colors <- colors[order(order(prices[NROW(prices), ]))]
par(mar=c(3, 3, 2, 2), oma=c(0, 0, 0, 0))
plot.zoo(prices, main="Multiple paths of geometric Brownian motion",
   xlab=NA, ylab=NA, plot.type="single", col=colors)

# Define daily volatility and growth rate
sigmav <- 0.01; drift <- 0.0; nrows <- 10000
paths <- 100
# Simulate multiple paths of geometric Brownian motion
prices <- matrix(rnorm(paths*nrows, sd=sigmav) +
    drift - sigmav^2/2, nc=paths)
prices <- exp(matrixStats::colCumsums(prices))
# Calculate fraction of paths below the expected value
fractv <- rowSums(prices < 1.0) / paths
# Create xts time series of percentage of paths below the expected value
fractv <- xts(fractv, order.by=seq.Date(Sys.Date()-NROW(fractv)+1, Sys.Date(), by=1))
# Plot xts time series of percentage of paths below the expected value
par(mar=c(3, 3, 2, 2), oma=c(0, 0, 0, 0))
plot.zoo(fractv, main="Percentage of GBM paths below mean",
   xlab=NA, ylab=NA, col="blue")

# Load S&P500 stock prices
load("/Users/jerzy/Develop/lecture_slides/data/sp500.RData")
ls(sp500env)
# Extract closing prices
prices <- eapply(sp500env, quantmod::Cl)
# Flatten prices into a single xts series
prices <- rutils::do_call(cbind, prices)
# Carry forward and backward non-NA prices
prices <- zoo::na.locf(prices, na.rm=FALSE)
prices <- zoo::na.locf(prices, fromLast=TRUE)
sum(is.na(prices))
# Drop ".Close" from column names
colnames(prices[, 1:4])
colnames(prices) <- rutils::get_name(colnames(prices))
# Or
# colnames(prices) <- do.call(rbind,
#   strsplit(colnames(prices), split="[.]"))[, 1]
# Normalize the columns so that prices start at 1
pricesn <- lapply(prices, function(x) x/as.numeric(x[1]))
pricesn <- rutils::do_call(cbind, pricesn)
# Calculate permutation index for sorting on the final prices
nrows <- NROW(pricesn)
ordern <- order(pricesn[nrows, ])
# Sort the symbols according to the final prices
symbolv <- colnames(pricesn)[ordern]
# Select 20 symbols
symbolv <- symbolv[seq.int(from=1, to=NROW(symbolv), length.out=20)]

# Plot xts time series of prices
colors <- colorRampPalette(c("red", "blue"))(NROW(symbolv))
colors <- colors[order(order(pricesn[nrows, symbolv]))]
plot.zoo(pricesn["2000/", symbolv], main="20 S&P500 Stock Prices (normalized)",
   xlab=NA, ylab=NA, plot.type="single", col=colors)
legend(x="topleft", inset=0.02, cex=0.6,
 legend=rev(symbolv), col=rev(colors), lwd=6, lty=1)

# Calculate average of valid stock prices
validp <- (pricesn != 1)  # Valid stocks
nstocks <- rowSums(validp)
nstocks[1] <- NCOL(pricesn)
indeks <- rowSums(pricesn*validp)/nstocks
# Calculate fraction of stock prices below the average price
fractv <- rowSums((pricesn < indeks) & validp)/nstocks
# Create xts time series of average stock prices
indeks <- xts(indeks, order.by=zoo::index(pricesn))

dev.new(width=6, height=5, noRStudioGD=TRUE)
# x11(width=6, height=4)
# Plot xts time series of average stock prices
plot.zoo(indeks, main="Average S&P500 Stock Prices (normalized from 1990)",
   xlab=NA, ylab=NA, col="blue")
# Create xts time series of percentage of stock prices below the average price
fractv <- xts(fractv, order.by=zoo::index(pricesn))
# Plot percentage of stock prices below the average price
plot.zoo(fractv[-(1:2),],
   main="Percentage of S&P500 Stock Prices
   Below the Average Price",
   xlab=NA, ylab=NA, col="blue")

# Load the S&P500 stock prices
library(rutils)
load("/Users/jerzy/Develop/lecture_slides/data/sp500_prices.RData")
# Subset (select) the prices after the start date of VTI
vti <- quantmod::Cl(rutils::etfenv$VTI)
vti <- rutils::diffit(vti)/rutils::lagit(vti, lagg=1, pad_zeros=FALSE)
colnames(vti) <- "VTI"
startd <- start(vti)
prices <- prices[startd <= zoo::index(prices)]
# Copy over NA prices using the function zoo::na.locf().
sum(is.na(prices))
prices <- zoo::na.locf(prices, na.rm=FALSE)
prices <- prices[, !is.na(prices[1, ])]
sum(is.na(prices))
dates <- zoo::index(prices)
vti <- vti[dates]
nrows <- NROW(prices)
ncols <- NCOL(prices)
# Calculate percentage returns
retsp <- rutils::diffit(prices)/rutils::lagit(prices, lagg=1, pad_zeros=FALSE)
# Normalize the prices so that they start at 1
pricesn <- lapply(prices, function(x) x/as.numeric(x[1]))
pricesn <- rutils::do_call(cbind, pricesn)
head(pricesn[, 1:5])
# Calculate the equal dollar-weighted average of all stock prices
indeks <- rowMeans(pricesn)
indeks <- xts::xts(indeks, order.by=dates)
colnames(indeks) <- "Index"

# Select a random, equal dollar-weighted portfolio of 5 stocks
nstocks <- 5
set.seed(1121)
samplev <- sample.int(n=ncols, size=nstocks, replace=FALSE)
portf <- pricesn[, samplev]
portf <- rowMeans(portf)
portf <- xts::xts(portf, order.by=dates)
colnames(portf) <- "Random"
# Plot dygraph of stock index and random portfolio
wealth <- cbind(indeks, portf)
colors <- c("blue", "red")
dygraphs::dygraph(wealth, main="Stock Index and Random Portfolio") %>%
  dyOptions(colors=colors, strokeWidth=2) %>%
  dyLegend(show="always", width=500)

# Select 10 random equal dollar-weighted sub-portfolios
set.seed(1121)
nportf <- 10
portfs <- sapply(1:nportf, function(x) {
  prices <- pricesn[, sample.int(n=ncols, size=nstocks, replace=FALSE)]
  rowMeans(prices)
})  # end sapply
portfs <- xts::xts(portfs, order.by=dates)
colnames(portfs) <- paste0("portf", 1:nportf)
round(head(portfs[, 1:4]), 3)
round(tail(portfs[, 1:4]), 3)

# Plot dygraph of stock index and random portfolios
colors <- colorRampPalette(c("red", "blue"))(nportf)
colors <- colors[order(order(portfs[NROW(portfs), ]))]
combined <- cbind(indeks, portfs)
colnames(combined)[1] <- "Index"
colors <- c("green", colors)
dygraphs::dygraph(combined, main="Stock Index and Random Portfolios") %>%
  dyOptions(colors=colors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)

# Define in-sample and out-of-sample intervals
cutoff <- nrows %/% 2
dates[cutoff]
# Calculate the 10 best performing stocks in-sample
perfstat <- sort(drop(coredata(pricesn[cutoff, ])), decreasing=TRUE)
symbolv <- names(head(perfstat, 10))
# Calculate the in-sample portfolio
pricis <- pricesn[1:cutoff, symbolv]
wealth <- sum(pricis[cutoff, ])
# Normalize the prices so that they are 1 at cutoff+1
pricesn <- lapply(prices, function(x) x/as.numeric(x[cutoff+1]))
pricesn <- rutils::do_call(cbind, pricesn)
# Calculate the out-of-sample portfolio
pricos <- pricesn[(cutoff+1):nrows, symbolv]
pricos <- wealth*pricos/sum(pricos[1, ])
wealth <- rbind(pricis, pricos)
wealth <- xts::xts(rowMeans(wealth), dates)

# Plot out-of-sample stock portfolio returns
wealth <- cbind(indeks, wealth)
colnames(wealth)[2] <- "Portfolio"
dygraphs::dygraph(log(wealth), main="Out-of-sample Log Prices of Stock Portfolio") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyEvent(dates[cutoff], label="in-sample", strokePattern="solid", color="green") %>%
  dyLegend(width=500)

backtestmom <- function(returns,
                       objfun=function(returns) (prod(1+returns)/sd(returns)),
                       look_back=12, rfreq="months", nstocks=10, bid_offer=0.001,
                       endp=rutils::calc_endpoints(returns, interval=rfreq), ...) {
  # Define look-back and look-forward intervals
  npts <- NROW(endp)
  # Perform loop over end points
  pnls <- lapply(2:(npts-1), function(it) {
    # Select look-back returns
    startp <- endp[max(1, it-look_back)]
    retslb <- returns[startp:endp[it], ]
    # Calculate the best performing stocks in-sample
    perfstat <- sapply(retslb, objfun)
    # perfstat <- sapply(1+retslb, prod)
    perfstat <- na.omit(perfstat)
    perfstat <- perfstat[is.finite(perfstat)]
    perfstat <- sort(perfstat, decreasing=TRUE)
    symbolv <- names(head(perfstat, nstocks))
    # Calculate the in-sample portfolio volatility and beta
    retst <- rowMeans(cumprod(1+retslb))
    retst <- rutils::diffit(retst)/rutils::lagit(retst, lagg=1, pad_zeros=FALSE)
    retsportf <- retslb[, symbolv]
    retsportf <- rowMeans(cumprod(1+retsportf))
    retsportf <- rutils::diffit(retsportf)/rutils::lagit(retsportf, lagg=1, pad_zeros=FALSE)
    scalef <- sd(retst)/sd(retsportf)
    # Calculate the out-of-sample portfolio returns
    retsos <- returns[(endp[it]+1):endp[it+1], symbolv]
    retsos <- rowMeans(cumprod(1+retsos))
    retsos <- rutils::diffit(retsos)/rutils::lagit(retsos, lagg=1, pad_zeros=FALSE)
    # Scale the out-of-sample portfolio returns
    scalef*retsos
  })  # end lapply
  pnls <- rutils::do_call(c, pnls)
  pnls
}  # end backtestmom

# Calculate a vector of monthly end points
endp <- rutils::calc_endpoints(prices, interval="months")
endp[2] <- 11
dates[endp]
npts <- NROW(endp)
look_back <- 8
# Select 10 stocks for equal dollar-weighted portfolio
nstocks <- 10
# weights <- rep(1/nstocks, nstocks)
pnls <- backtestmom(retsp, look_back=look_back, endp=endp,
  nstocks=nstocks, objfun=function(returns) (prod(1+returns)))
# Add initial startup interval
retsu <- retsp[endp[1]:endp[2], ]
retsu <- rowMeans(cumprod(1+retsu))
retsu <- rutils::diffit(retsu)/
  rutils::lagit(retsu, lagg=1, pad_zeros=FALSE)
pnls <- c(retsu, pnls)
pnls <- cumprod(1+pnls)
pnls <- xts::xts(pnls, order.by=dates)
colnames(pnls) <- "Strategy"
# Calculate the Sharpe and Sortino ratios
wealth <- cbind(indeks, pnls)
sqrt(252)*sapply(rutils::diffit(wealth),
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))

# Plot dygraph of stock index and momentum strategy
colors <- c("blue", "red")
dygraphs::dygraph(log(wealth), main="Log Stock Index and Momentum Strategy") %>%
  dyOptions(colors=colors, strokeWidth=2) %>%
  dyLegend(show="always", width=500)
