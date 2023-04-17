# Open x11 for plotting
x11(width=5, height=4)
# Set plot parameters to reduce whitespace around plot
par(mar=c(4, 4, 2, 1), oma=c(0, 0, 0, 0))
# Define logarithmic utility
utilfun <- function(frac, p=0.3, a=20, b=1) {
  p*log(1+frac*a) + (1-p)*log(1-frac*b)
}  # end utilfun
# Plot utility
curve(expr=utilfun, xlim=c(0, 1),
ylim=c(-0.5, 0.4), xlab="betting fraction",
ylab="utility", main="", lwd=2)
title(main="Logarithmic Utility", line=0.5)

# Open x11 for plotting
x11(width=5, height=4)
# Set plot parameters to reduce whitespace around plot
par(mar=c(4, 4, 2, 1), oma=c(0, 0, 0, 0))
# Define and plot Kelly fraction
kelly_frac <- function(a, p=0.5, b=1) {
  p/b - (1-p)/a
}  # end kelly_frac
curve(expr=kelly_frac, xlim=c(0, 5),
ylim=c(-2, 1), xlab="betting odds",
ylab="kelly fraction", main="", lwd=2)
abline(h=0.5, lwd=2, col="red")
text(x=1.5, y=0.5, pos=3, cex=0.8, labels="max Kelly fraction=0.5")
title(main="Kelly fraction", line=-0.8)

# Open x11 for plotting
x11(width=5, height=4)
# Set plot parameters to reduce whitespace around plot
par(mar=c(4, 4, 2, 1), oma=c(0, 0, 0, 0))
# Plot several Kelly curves
curve(expr=kelly_frac(x, b=1), xlim=c(0, 5),
ylim=c(-1, 1.5), xlab="betting odds",
ylab="kelly fraction", main="", lwd=2)
abline(h=0.5, lwd=2, col="red")
text(x=1.5, y=0.5, pos=3, cex=0.8, labels="b=1.0; max fraction=0.5")
curve(expr=kelly_frac(x, b=0.5), add=TRUE, main="", lwd=2)
abline(h=1.0, lwd=2, col="red")
text(x=1.5, y=1.0, pos=3, cex=0.8, labels="b=0.5; max fraction=1.0")
title(main="Kelly fraction", line=-0.8)

# Open x11 for plotting
x11(width=5, height=4)
# Set plot parameters to reduce whitespace around plot
par(mar=c(4, 4, 2, 1), oma=c(0, 0, 0, 0))
# Wealth of multiperiod binary betting
wealthv <- function(f, a=0.8, b=0.1, n=1e3, i=150) {
  (1+f*a)^i * (1-f*b)^(n-i)
}  # end wealth
curve(expr=wealthv, xlim=c(0, 1),
xlab="betting fraction",
ylab="wealth", main="", lwd=2)
title(main="Wealth of Multiperiod Betting", line=0.1)

# Open x11 for plotting
x11(width=5, height=4)
# Set plot parameters to reduce whitespace around plot
par(mar=c(4, 4, 2, 1), oma=c(0, 0, 0, 0))
set.seed(1121)  # Reset random number generator
# Simulate asset prices
calc_pricev <- function(x) cumprod(1 + rnorm(1e3, sd=0.01))
price_paths <- sapply(1:3, calc_pricev)
plot(price_paths[, 1], type="l", lwd=3,
     main="Simulated Asset Prices",
     ylim=range(price_paths),
     lty="solid", xlab="time", ylab="price")
lines(price_paths[, 2], col="blue", lwd=3)
lines(price_paths[, 3], col="orange", lwd=3)
abline(h=0.5, col="red", lwd=3)
text(x=200, y=0.5, pos=3, labels="liquidation threshold")

# Calculate the VTI returns
retp <- rutils::etfenv$returns$VTI
retp <- na.omit(retp)
c(mean=mean(retp), std=sd(retp))
range(retp)

# Open x11 for plotting
x11(width=5, height=4)
# Set plot parameters to reduce whitespace around plot
par(mar=c(4, 4, 2, 1), oma=c(0, 0, 0, 0))
# Define vectorized logarithmic utility function
utilfun <- function(kellyfrac, retp) {
  sapply(kellyfrac, function(x) sum(log(1 + x*retp)))
}  # end utilfun
utilfun(1, retp)
utilfun(c(1, 4), retp)
# Plot the logarithmic utility
curve(expr=utilfun(x, retp=retp),
xlim=c(0.1, 5), xlab="leverage", ylab="utility",
main="Utility of Asset Returns", lwd=2)

# Approximate Kelly leverage
mean(retp)/var(retp)
PerformanceAnalytics::KellyRatio(R=retp, method="full")
# Kelly leverage
unlist(optimize(
  f=function(x) -utilfun(x, retp),
  interval=c(1, 4)))

# Calculate the VTI returns
retp <- rutils::etfenv$returns$VTI
retp <- na.omit(retp)
# Calculate the wealth paths
kelly_ratio <- drop(mean(retp)/var(retp))
kelly_wealthv <- cumprod(1 + kelly_ratio*retp)
hyper_kelly <- cumprod(1 + (kelly_ratio+2)*retp)
sub_kelly <- cumprod(1 + (kelly_ratio-2)*retp)
kelly_paths <- cbind(kelly_wealth, hyper_kelly, sub_kelly)
colnames(kelly_paths) <- c("kelly", "hyper-kelly", "sub-kelly")

# Plot wealth paths
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("black", "orange", "blue")
quantmod::chart_Series(kelly_paths, theme=plot_theme, name="Wealth Paths")
legend("topleft", legend=colnames(kelly_paths),
 inset=0.1, bg="white", lty=1, lwd=6, y.intersp=0.5,
 col=plot_theme$col$line.col, bty="n")

# bid_offer equal to 10 bps for liquid ETFs
bid_offer <- 0.001
# Calculate the wealth paths
kelly_ratio <- drop(mean(retp)/var(retp))
wealthv <- cumprod(1 + kelly_ratio*retp)
wealth_trans <- cumprod(1 + kelly_ratio*retp -
  0.5*bid_offer*kelly_ratio*(kelly_ratio-1)*abs(retp))
# Calculate the compounded wealth from returns
wealthv <- cbind(wealthv, wealth_trans)
colnames(wealthv) <- c("Kelly", "Including bid-offer")
# Plot compounded wealth
dygraphs::dygraph(wealthv, main="Kelly Strategy With Transaction Costs") %>%
  dyOptions(colors=c("green", "blue"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Open x11 for plotting
x11(width=5, height=4)
# Set plot parameters to reduce whitespace around plot
par(mar=c(4, 4, 2, 1), oma=c(0, 0, 0, 0))
# Plot several Kelly curves
curve(expr=kelly_frac(x, b=1), xlim=c(0, 5),
ylim=c(-1, 1.5), xlab="betting odds",
ylab="kelly fraction", main="", lwd=2)
abline(h=0.5, lwd=2, col="red")
text(x=1.5, y=0.5, pos=3, cex=0.8, labels="b=1.0; max fraction=0.5")
curve(expr=kelly_frac(x, b=0.5), add=TRUE, main="", lwd=2)
abline(h=1.0, lwd=2, col="red")
text(x=1.5, y=1.0, pos=3, cex=0.8, labels="b=0.5; max fraction=1.0")
title(main="Kelly fraction", line=-0.8)

# Open x11 for plotting
x11(width=5, height=4)
# Set plot parameters to reduce whitespace around plot
par(mar=c(4, 4, 2, 1), oma=c(0, 0, 0, 0))
# Plot logarithmic utility function
curve(expr=log, lwd=3, col="blue", xlim=c(0.5, 5),
xlab="wealth", ylab="utility",
main="Logarithmic Utility")

# Open x11 for plotting
x11(width=5, height=4)
# Set plot parameters to reduce whitespace around plot
par(mar=c(4, 4, 2, 1), oma=c(0, 0, 0, 0))
# Define CRRA utility
cr_ra <- function(w, ra) {
  (w^(1-ra) - 1)/(1-ra)
}  # end cr_ra
# Plot utility functions
curve(expr=cr_ra(x, ra=0.7), xlim=c(0.5, 5), lwd=3,
xlab="wealth", ylab="utility", main="", col="blue")
curve(expr=log, add=TRUE, lwd=3)
curve(expr=cr_ra(x, ra=1.3), add=TRUE, lwd=3, col="red")
# Add title and legend
title(main="CRRA Utility", line=0.5)
legend(x="topleft", legend=c("risk seeking", "logarithmic", "risk averse"),
 title="Risk Aversion", inset=0.05, cex=0.8, bg="white", y.intersp=0.5,
 lwd=6, lty=1, bty="n", col=c("blue", "black", "red"))

# Open x11 for plotting
x11(width=5, height=4)
# Set plot parameters to reduce whitespace around plot
par(mar=c(4, 4, 2, 1), oma=c(0, 0, 0, 0))
# Define CRRA utility
cr_ra <- function(w, ra) {
  (w^(1-ra) - 1)/(1-ra)
}  # end cr_ra
# Plot utility functions
curve(expr=cr_ra(x, ra=0.7), xlim=c(0.5, 5), lwd=3,
xlab="wealth", ylab="utility", main="", col="blue")
curve(expr=log, add=TRUE, lwd=3)
curve(expr=cr_ra(x, ra=1.3), add=TRUE, lwd=3, col="red")
# Add title and legend
title(main="CRRA Utility", line=0.5)
legend(x="topleft", legend=c("risk seeking", "logarithmic", "risk averse"),
 title="Risk Aversion", inset=0.05, cex=0.8, bg="white", y.intersp=0.5,
 lwd=6, lty=1, bty="n", col=c("blue", "black", "red"))

# Calculate the VTI returns
retp <- rutils::etfenv$returns$VTI
retp <- na.omit(retp)
# Calculate the wealth paths
kelly_ratio <- drop(mean(retp)/var(retp))
kelly_wealthv <- cumprod(1 + kelly_ratio*retp)
hyper_kelly <- cumprod(1 + (kelly_ratio+2)*retp)
sub_kelly <- cumprod(1 + (kelly_ratio-2)*retp)
kelly_paths <- cbind(kelly_wealth, hyper_kelly, sub_kelly)
colnames(kelly_paths) <- c("kelly", "hyper-kelly", "sub-kelly")

# Plot wealth paths
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("black", "orange", "blue")
quantmod::chart_Series(kelly_paths, theme=plot_theme,
       name="Wealth Paths")
legend("topleft", legend=colnames(kelly_paths),
 inset=0.1, bg="white", lty=1, lwd=6, y.intersp=0.5,
 col=plot_theme$col$line.col, bty="n")

# Calculate the VTI returns
retp <- rutils::etfenv$returns$VTI
retp <- na.omit(retp)
# Calculate the higher moments of VTI returns
c(mean=sum(retp),
  variance=sum(retp^2),
  mom3=sum(retp^3),
  mom4=sum(retp^4))/NROW(retp)
# Calculate the higher moments of minutely SPY returns
spy <- HighFreq::SPY[, 4]
spy <- na.omit(spy)
spy <- rutils::diffit(log(spy))
c(mean=sum(spy),
  variance=sum(spy^2),
  mom3=sum(spy^3),
  mom4=sum(spy^4))/NROW(spy)

retp <- na.omit(rutils::etfenv$returns[, c("VTI", "IEF")])
# Logarithmic utility of stock and bond portfolio
utilfun <- function(stocku, bondu) {
  -sum(log(1 + stocku*retp$VTI + bondu*retp$IEF))
}  # end utilfun
# Create matrix of utility values
stocku <- seq(from=3, to=7, by=0.2)
bondu <- seq(from=12, to=20, by=0.2)
utilm <- sapply(bondu, function(y) sapply(stocku,
  function(x) utilfun(x, y)))
# Set rgl options and load package rgl
options(rgl.useNULL=TRUE)
library(rgl)
# Draw 3d surface plot of utility
rgl::persp3d(stocku, bondu, utilm, col="green",
  xlab="stocks", ylab="bonds", zlab="utility")
# Render the surface plot
rgl::rglwidget(elementId="plot3drgl")
# Save the surface plot to png file
rgl::rgl.snapshot("utility_surface.png")

# Approximate Kelly weights
weightv <- sapply(retp, function(x) mean(x)/var(x))
# Kelly weight for stocks
unlist(optimize(f=function(x) utilfun(x, bondu=0), interval=c(1, 4)))
# Kelly weight for bonds
unlist(optimize(f=function(x) utilfun(x, stocku=0), interval=c(1, 14)))
# Vectorized utility of stock and bond portfolio
utility_vec <- function(weightv) {
  utilfun(weightv[1], weightv[2])
}  # end utility_vec
# Optimize with respect to vector argument
optiml <- optim(fn=utility_vec, par=c(3, 10),
          method="L-BFGS-B",
          upper=c(8, 20), lower=c(2, 5))
# Exact Kelly weights
optiml$par

# Approximate Kelly weights
retsport <- (retp %*% weightv)
drop(mean(retsport)/var(retsport))*weightv
# Exact Kelly weights
optiml$par

# Quarter-Kelly sub-optimal weights
weightv <- optiml$par/4
# Plot Kelly optimal portfolio
retp <- cbind(retp, weightv[1]*retp$VTI + weightv[2]*retp$IEF)
colnames(retp)[3] <- "Kelly_sub_optimal"
# Calculate the compounded wealth from returns
wealthv <- cumprod(1 + retp)
# Plot compounded wealth
dygraphs::dygraph(wealthv, main="Stock and Bond Portfolio") %>%
  dyOptions(colors=c("green", "blue", "green")) %>%
  dySeries("Kelly_sub_optimal", color="red", strokeWidth=2) %>%
  dyLegend(show="always", width=300)

retp <- na.omit(rutils::etfenv$returns[, c("VTI", "IEF")])
# Calculate the rolling returns and variance
look_back <- 200
var_rolling <- HighFreq::roll_var(retp, look_back)
weightv <- HighFreq::roll_sum(retp, look_back)/look_back
weightv <- weightv/var_rolling
weightv[1, ] <- 1/NCOL(weightv)
weightv <- zoo::na.locf(weightv)
sum(is.na(weightv))
range(weightv)

# Plot the weights
x11(width=6, height=5)
par(mar=c(4, 4, 3, 1), oma=c(0, 0, 0, 0))
plot(density(retp$IEF), t="l", lwd=3, col="red",
     xlab="weights", ylab="density",
     ylim=c(0, max(density(retp$VTI)$y)),
     main="Kelly Weight Distributions")
lines(density(retp$VTI), t="l", col="blue", lwd=3)
legend("topright", legend=c("VTI", "IEF"),
 inset=0.1, bg="white", lty=1, lwd=6, y.intersp=0.5,
 col=c("blue", "red"), bty="n")

# Scale and lag the Kelly weights
weightv <- lapply(weightv,
  function(x) 10*x/sum(abs(range(x))))
weightv <- do.call(cbind, weightv)
weightv <- rutils::lagit(weightv)
# Calculate the compounded Kelly wealth and VTI
wealthv <- cbind(cumprod(1 + weightv$VTI*retp$VTI), cumprod(1 + retp$VTI))
colnames(wealthv) <- c("Kelly Strategy", "VTI")
dygraphs::dygraph(wealthv, main="VTI Strategy Using Rolling Kelly Weight") %>%
  dyAxis("y", label="Kelly Strategy", independentTicks=TRUE) %>%
  dyAxis("y2", label="VTI", independentTicks=TRUE) %>%
  dySeries(name="Kelly Strategy", axis="y", label="Kelly Strategy", strokeWidth=1, col="red") %>%
  dySeries(name="VTI", axis="y2", label="VTI", strokeWidth=1, col="blue")

# bid_offer equal to 10 bps for liquid ETFs
bid_offer <- 0.001
# Calculate the compounded Kelly wealth and margin
wealthv <- cumprod(1 + weightv$VTI*retp$VTI)
margin <- (retp$VTI - 1)*wealthv + 1
# Calculate the transaction costs
costs <- bid_offer*drop(rutils::diffit(margin))/2
wealth_diff <- drop(rutils::diffit(wealthv))
costs_rel <- ifelse(wealth_diff>0, costs/wealth_diff, 0)
range(costs_rel)
hist(costs_rel, breaks=10000, xlim=c(-0.02, 0.02))
# Scale and lag the transaction costs
costs <- rutils::lagit(abs(costs)/wealthv)
# ReCalculate the compounded Kelly wealth
wealth_trans <- cumprod(1 + retp$VTI*retp$VTI - costs)
# Plot compounded wealth
wealthv <- cbind(wealthv, wealth_trans)
colnames(wealthv) <- c("Kelly", "Including bid-offer")
dygraphs::dygraph(wealthv, main="Kelly Strategy With Transaction Costs") %>%
  dyOptions(colors=c("green", "blue"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Calculate the compounded wealth from returns
wealthv <- cumprod(1 + rowSums(weightv*retp))
wealthv <- xts::xts(wealthv, zoo::index(retp))
quantmod::chart_Series(wealthv, name="Rolling Kelly Strategy For VTI and IEF")
# Calculate the compounded Kelly wealth and VTI
wealthv <- cbind(wealthv, cumprod(1 + 0.6*retp$IEF + 0.4*retp$VTI))
colnames(wealthv) <- c("Kelly Strategy", "VTI plus IEF")
dygraphs::dygraph(wealthv, main="Rolling Kelly Strategy For VTI and IEF") %>%
  dyAxis("y", label="Kelly Strategy", independentTicks=TRUE) %>%
  dyAxis("y2", label="VTI plus IEF", independentTicks=TRUE) %>%
  dySeries(name="Kelly Strategy", axis="y", label="Kelly Strategy", strokeWidth=1, col="red") %>%
  dySeries(name="VTI plus IEF", axis="y2", label="VTI plus IEF", strokeWidth=1, col="blue")

# Test if IEF can time VTI
retp <- na.omit(rutils::etfenv$returns[, c("IEF", "VTI")])
retvti <- retp$VTI
desv <- cbind(retp, 0.5*(retvti+abs(retvti)), retvti^2)
colnames(desv)[3:4] <- c("merton", "treynor")
# Merton-Henriksson test
regmod <- lm(IEF ~ VTI + merton, data=desv); summary(regmod)

# Treynor-Mazuy test
regmod <- lm(IEF ~ VTI + treynor, data=desv); summary(regmod)
# Plot residual scatterplot
x11(width=6, height=5)
resids <- (desv$IEF - regmod$coeff["VTI"]*retvti)
plot.default(x=retvti, y=resids, xlab="VTI", ylab="IEF")
title(main="Treynor-Mazuy Market Timing Test\n for IEF vs VTI", line=0.5)
# Plot fitted (predicted) response values
coefreg <- summary(regmod)$coeff
fittedv <- regmod$fitted.values - coefreg["VTI", "Estimate"]*retvti
tvalue <- round(coefreg["treynor", "t value"], 2)
points.default(x=retvti, y=fittedv, pch=16, col="red")
text(x=0.0, y=0.8*max(resids), paste("Treynor test t-value =", tvalue))

library(xtable)
gamblev <- data.frame(win=c("p", "a"), lose=c("q = 1 - p", "-b"))
rownames(gamblev) <- c("probability", "payout")
# print(xtable(gamblev), comment=FALSE, size="tiny")
print(xtable(gamblev), comment=FALSE)

library(rutils)
# Extract the ETF prices from rutils::etfenv$prices
pricev <- rutils::etfenv$prices
pricev <- zoo::na.locf(pricev, na.rm=FALSE)
pricev <- zoo::na.locf(pricev, fromLast=TRUE)
datev <- zoo::index(pricev)
# Calculate the simple dollar returns
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
pricen <- cumprod(1+retp)
# Set the initial prices
pricesi <- as.numeric(pricev[1, ])
pricen <- lapply(1:NCOL(pricen), function (i) pricesi[i]*pricen[, i])
pricen <- rutils::do_call(cbind, pricen)
# pricen <- t(t(pricen)*pricesi)
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
frate <- 0.01/252
# Margin account
margin <- cumsum(retp)
# Cumulative funding costs
fcosts <- cumsum(frate*margin)
# Add funding costs to margin account
margin <- (margin + fcosts)
# dygraph plot of margin and funding costs
datav <- cbind(margin, fcosts)
colnamev <- c("Margin", "Cumulative Funding")
colnames(datav) <- colnamev
endd <- rutils::calc_endpoints(datav, interval="weeks")
dygraphs::dygraph(datav[endd], main="VTI Margin Funding Costs") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", col="red", strokeWidth=3) %>%
  dyLegend(show="always", width=300)

# bid_offer equal to 10 bps for liquid ETFs
bid_offer <- 0.001
# Cumulative transaction costs
costs <- bid_offer*cumsum(abs(retp))/2
# Subtract transaction costs from margin account
margin <- cumsum(retp)
margin <- (margin - costs)
# dygraph plot of margin and transaction costs
datav <- cbind(margin, costs)
colnamev <- c("Margin", "Cumulative Transaction Costs")
colnames(datav) <- colnamev
dygraphs::dygraph(datav[endd], main="VTI Transaction Costs") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", col="red", strokeWidth=3) %>%
  dyLegend(show="always", width=300)

# Calculate the VTI and IEF dollar returns
pricev <- rutils::etfenv$prices[, c("VTI", "IEF")]
pricev <- na.omit(pricev)
retd <- rutils::diffit(pricev)
datev <- zoo::index(pricev)
# Calculate the VTI and IEF percentage returns
retp <- retd/rutils::lagit(pricev, lagg=1, pad_zeros=FALSE)
# Wealth of fixed shares equal to $0.5 each (without rebalancing)
weightv <- c(0.5, 0.5)  # dollar weights
wealth_tsa <- drop(cumprod(1+retp) %*% weightv)
# Or using the dollar returns
pricesi <- as.numeric(pricev[1, ])
retd[1, ] <- pricev[1, ]
wealth_tsa2 <- cumsum(retd %*% (weightv/pricesi))
all.equal(wealth_tsa, drop(wealth_tsa2))

# Wealth of fixed dollars (with rebalancing)
wealth_tda <- cumsum(retp %*% weightv)
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(log(wealth_tsa), wealth_tda)
wealthv <- xts::xts(wealthv, datev)
colnames(wealthv) <- c("Fixed shares", "Fixed dollars")
sqrt(252)*sapply(rutils::diffit(wealthv),
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot the log wealth
colnamev <- colnames(wealthv)
endd <- rutils::calc_endpoints(retp, interval="weeks")
dygraphs::dygraph(wealthv[endd], main="Wealth of Weighted Portfolios") %>%
  dySeries(name=colnamev[1], col="blue", strokeWidth=2) %>%
  dySeries(name=colnamev[2], col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Margin account for fixed dollars (with rebalancing)
margin <- cumsum(retp %*% weightv)
# Cumulative transaction costs
costs <- bid_offer*cumsum(abs(retp) %*% weightv)/2
# Subtract transaction costs from margin account
margin <- (margin - costs)
# dygraph plot of margin and transaction costs
datav <- cbind(margin, costs)
datav <- xts::xts(datav, datev)
colnamev <- c("Margin", "Cumulative Transaction Costs")
colnames(datav) <- colnamev
dygraphs::dygraph(datav[endd], main="Fixed Dollar Portfolio Transaction Costs") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", col="red", strokeWidth=3) %>%
  dyLegend(show="always", width=300)

# Wealth of fixed shares (without rebalancing)
wealth_tsa <- cumsum(retd %*% (weightv/pricesi))
# Or compound the percentage returns
wealth_tsa <- cumprod(1+retp) %*% weightv
# Wealth of proportional allocations (with rebalancing)
wealth_pda <- cumprod(1 + retp %*% weightv)
wealthv <- cbind(wealth_tsa, wealth_pda)
wealthv <- xts::xts(wealthv, datev)
colnames(wealthv) <- c("Fixed shares", "Prop dollars")
wealthv <- log(wealthv)
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(rutils::diffit(wealthv),
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot the log wealth
dygraphs::dygraph(wealthv[endd],
  main="Wealth of Proportional Dollar Allocations") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Returns in excess of weighted returns
retw <- retp %*% weightv
retx <- lapply(retp, function(x) (retw - x))
retx <- do.call(cbind, retx)
sum(retx %*% weightv)
# Calculate the weighted sum of absolute excess returns
retx <- abs(retx) %*% weightv
# Total dollar amount of stocks that need to be traded
retx <- retx*rutils::lagit(wealth_pda)
# Cumulative transaction costs
costs <- bid_offer*cumsum(retx)/2
# Subtract transaction costs from wealth
wealth_pda <- (wealth_pda - costs)

# dygraph plot of wealth and transaction costs
wealthv <- cbind(wealth_pda, costs)
wealthv <- xts::xts(wealthv, datev)
colnamev <- c("Wealth", "Cumulative Transaction Costs")
colnames(wealthv) <- colnamev
dygraphs::dygraph(wealthv[endd],
  main="Transaction Costs With Proportional Allocations") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", col="red", strokeWidth=3) %>%
  dyLegend(show="always", width=300)

# Wealth of fixed shares (without rebalancing)
wealth_tsa <- drop(apply(retp, 2, function(x) cumprod(1+x)) %*% weightv)-1
# Wealth of proportional dollar allocations (with rebalancing)
wealth_pda <- cumprod(1 + retp %*% weightv) - 1
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
wealthv <- cbind(wealth_pda, wealthv)
wealthv <- xts::xts(wealthv, datev)
colnames(wealthv) <- c("Proportional Allocations", "Proportional Target")
dygraphs::dygraph(wealthv, main="Wealth of Proportional Target Allocations") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

library(rutils)  # Load package rutils
# Create name corresponding to "^GSPC" symbol
setSymbolLookup(
  SP500=list(name="^GSPC", src="yahoo"))
getSymbolLookup()
# view and clear options
options("getSymbols.sources")
options(getSymbols.sources=NULL)
# Download S&P500 prices into etfenv
quantmod::getSymbols("SP500", env=etfenv,
    adjust=TRUE, auto.assign=TRUE, from="1990-01-01")
quantmod::chart_Series(x=etfenv$SP500["2016/"],
       TA="add_Vo()",
       name="S&P500 index")

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

# Wealth of proportional allocations
wealthv <- cumprod(1 + retp)
# Calculate the a vector of monthly end points
endd <- rutils::calc_endpoints(retp, interval="weeks")
# Plot cumulative log wealth
dygraphs::dygraph(log(wealthv[endd]),
  main="Stocks and Bonds With Proportional Allocations") %>%
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

# Coerce the returns from xts time series to matrix
retp <- zoo::coredata(retp[, 1:2])
nrows <- NROW(retp)
# Bootstrap the returns and Calculate the a list of random returns
nboot <- 1e4
library(parallel)  # Load package parallel
ncores <- detectCores() - 1  # Number of cores
# Perform parallel bootstrap under Windows
cluster <- makeCluster(ncores)  # Initialize compute cluster under Windows
clusterSetRNGStream(cluster, 1121)  # Reset random number generator in all cores
clusterExport(cluster, c("retp", "nrows"))
bootd <- parLapply(cluster, 1:nboot, function(x) {
  retp[sample.int(nrows, replace=TRUE), ]
})  # end parLapply
# Perform parallel bootstrap under Mac-OSX or Linux
set.seed(1121)
bootd <- mclapply(1:nboot, function(x) {
  retp[sample.int(nrows, replace=TRUE), ]
}, mc.cores=ncores)  # end mclapply
is.list(bootd); NROW(bootd); dim(bootd[[1]])

# Calculate the distribution of terminal wealths under Windows
wealthv <- parLapply(cluster, bootd, function(retp) {
  apply(retp, 2, function(x) prod(1+x))
})  # end parLapply
# Calculate the distribution of terminal wealths under Mac-OSX or Linux
wealthv <- mclapply(bootd, function(retp) {
  apply(retp, 2, function(x) prod(1+x))
}, mc.cores=ncores)  # end mclapply
wealthv <- do.call(rbind, wealthv)
class(wealthv); dim(wealthv); tail(wealthv)
# Calculate the means and standard deviations of the terminal wealths
apply(wealthv, 2, mean)
apply(wealthv, 2, sd)
# Extract the terminal wealths of VTI and IEF
wealthvti <- wealthv[, "VTI"]
wealthief <- wealthv[, "IEF"]

# Plot the densities of the terminal wealths of VTI and IEF
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
 inset=0.1, cex=1.0, bg="white", bty="n", y.intersp=0.5,
 lwd=6, lty=1, col=c("blue", "green"))

# Calculate the distributions of stock wealth
holdv <- nrows*seq(0.1, 1.0, 0.1)
wealthm <- mclapply(bootd, function(retp) {
  sapply(holdv, function(holdp) {
    prod(1 + retp[1:holdp, "VTI"])
  })  # end sapply
}, mc.cores=ncores)  # end mclapply
wealthm <- do.call(rbind, wealthm)
dim(wealthm)
# Define the risk-adjusted wealth measure
riskretfun <- function(wealthv) {
  riskv <- 0.01
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
 inset=0.1, cex=1.0, bg="white", bty="n", y.intersp=0.5,
 lwd=6, lty=1, col=c("blue", "green"))

# Calculate the distributions of portfolio wealth
weightv <- seq(0.05, 0.95, 0.05)
wealthm <- mclapply(bootd, function(retp) {
  sapply(weightv, function(weight) {
    prod(1 + retp %*% c(weight, 1-weight))
  })  # end sapply
}, mc.cores=ncores)  # end mclapply
wealthm <- do.call(rbind, wealthm)
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

# Extract the ETF returns
symbolv <- c("VTI", "IEF", "DBC")
retp <- na.omit(rutils::etfenv$returns[, symbolv])
# Calculate the all-weather portfolio wealth
weightsaw <- c(0.30, 0.55, 0.15)
retp <- cbind(retp, retp %*% weightsaw)
colnames(retp)[4] <- "All Weather"
# Calculate the Sharpe ratios
sqrt(252)*sapply(retp, function(x) mean(x)/sd(x))

# Calculate the cumulative wealth from returns
wealthv <- cumprod(1+retp)
# Calculate the a vector of monthly end points
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
# dygraph all-weather wealth
dygraphs::dygraph(wealthv[endd], main="All-Weather Portfolio") %>%
  dyOptions(colors=c("blue", "green", "orange", "red")) %>%
  dySeries("All Weather", color="red", strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Plot all-weather wealth
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue", "green", "red")
quantmod::chart_Series(wealthv, theme=plot_theme, lwd=c(2, 2, 2, 4),
       name="All-Weather Portfolio")
legend("topleft", legend=colnames(wealthv),
  inset=0.1, bg="white", lty=1, lwd=6, y.intersp=0.5,
  col=plot_theme$col$line.col, bty="n")

# Calculate the VTI returns
retp <- na.omit(rutils::etfenv$returns$VTI["2008/2009"])
datev <- zoo::index(retp)
nrows <- NROW(retp)
retp <- drop(zoo::coredata(retp))
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
  portfv[t] <- portfv[t-1] + stockv[t-1]*retp[t]
  stockv[t] <- min(coeff*(portfv[t] - bfloor), portfv[t])
  bondv[t] <- (portfv[t] - stockv[t])
}  # end for
# dygraph plot of CPPI strategy
pricev <- 100*cumprod(1+retp)
datav <- xts::xts(cbind(stockv, bondv, portfv, pricev), datev)
colnames(datav) <- c("stocks", "bonds", "CPPI", "VTI")
endd <- rutils::calc_endpoints(datav, interval="weeks")
dygraphs::dygraph(datav[endd], main="CPPI strategy") %>%
  dyOptions(colors=c("red", "green", "blue", "orange"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Calculate the dollar and percentage returns for VTI and IEF
pricev <- rutils::etfenv$prices[, c("VTI", "IEF")]
pricev <- na.omit(pricev)
retd <- rutils::diffit(pricev)
retp <- retd/rutils::lagit(pricev, lagg=1, pad_zeros=FALSE)
# Calculate the standardized simple dollar returns
retstd <- lapply(retd, function(x) x/sd(x))
retstd <- do.call(cbind, retstd)
sapply(retstd, sd)
# Wealth of fixed number of shares (without rebalancing)
weightv <- c(0.5, 0.5)
pricesi <- as.numeric(pricev[1, ])
wealth_tsa <- cumsum(retd %*% (weightv/pricesi))
# Calculate the standardized percentage returns
retstp <- lapply(retp, function(x) x/sd(x))
retstp <- do.call(cbind, retstp)
sapply(retstp, sd)
# Wealth of target dollar allocation of shares (with rebalancing)
wealth_tda <- cumsum(retstp %*% weightv)
# Plot log wealth
wealthv <- cbind(wealth_tda, log(wealth_tsa))
# wealthv <- xts::xts(wealthv, datev)
colnames(wealthv) <- c("With rebalancing", "Without rebalancing")
dygraphs::dygraph(wealthv, main="Wealth of Equal Dollar Amount of Shares") %>%
  dyOptions(colors=c("green", "blue"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Calculate the dollar and percentage returns for VTI and IEF
pricev <- rutils::etfenv$prices[, c("VTI", "IEF")]
pricev <- na.omit(pricev)
retd <- rutils::diffit(pricev)
retp <- retd/rutils::lagit(pricev, lagg=1, pad_zeros=FALSE)
# Calculate the wealth of proportional allocations
weightv <- c(0.5, 0.5)
retw <- retp %*% weightv
wealth_pda <- cumprod(1 + retw)
# Calculate the rolling percentage volatility
look_back <- 21
volat <- HighFreq::roll_var(retp, look_back=look_back)
iszero <- (rowSums(volat) == 0)
volat[iszero, ] <- 1
# Calculate the risk parity portfolio allocations
alloc <- lapply(1:NCOL(pricev),
  function(x) weightv[x]/volat[, x])
alloc <- do.call(cbind, alloc)
# Scale allocations to 1 dollar total
alloc <- alloc/rowSums(alloc)
# Lag the allocations
alloc <- rutils::lagit(alloc)
# Calculate the wealth of risk parity
retw <- rowSums(retp*alloc)
wealth_rp <- cumprod(1 + retw)

# Calculate the log wealths
datev <- zoo::index(pricev)
wealthv <- log(cbind(wealth_pda, wealth_rp))
wealthv <- xts::xts(wealthv, datev)
colnames(wealthv) <- c("Fixed Ratio", "Risk Parity")
# Calculate the Sharpe ratios
sqrt(252)*sapply(rutils::diffit(wealthv), function (x) mean(x)/sd(x))
# Plot a dygraph of the log wealths
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(wealthv[endd],
  main="Log Wealth of Risk Parity vs Proportional Allocations") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Test risk parity market timing of VTI using Treynor-Mazuy test
retrp <- rutils::diffit(wealthv)
retvti <- retp$VTI
desv <- cbind(retrp, retvti, retvti^2)
desv <- na.omit(desv)
colnames(desv)[1:2] <- c("fixed", "risk_parity")
colnames(desv)[4] <- "treynor"
regmod <- lm(risk_parity ~ VTI + treynor, data=desv)
summary(regmod)
# Plot residual scatterplot
resids <- regmod$residuals
plot.default(x=retvti, y=resids, xlab="VTI", ylab="residuals")
title(main="Treynor-Mazuy Market Timing Test\n for Risk Parity vs VTI", line=0.5)
# Plot fitted (predicted) response values
coefreg <- summary(regmod)$coeff
fittedv <- regmod$fitted.values - coefreg["VTI", "Estimate"]*retvti
tvalue <- round(coefreg["treynor", "t value"], 2)
points.default(x=retvti, y=fittedv, pch=16, col="red")
text(x=0.0, y=0.8*max(resids), paste("Treynor test t-value =", tvalue))

# Test for fixed ratio market timing of VTI using Treynor-Mazuy test
regmod <- lm(fixed ~ VTI + treynor, data=desv)
summary(regmod)
# Plot fitted (predicted) response values
coefreg <- summary(regmod)$coeff
fittedv <- regmod$fitted.values - coefreg["VTI", "Estimate"]*retvti
points.default(x=retvti, y=fittedv, pch=16, col="blue")
text(x=0.0, y=0.6*max(resids), paste("Fixed Ratio t-value =", round(coefreg["treynor", "t value"], 2)))

# Returns in excess of weighted returns
retx <- lapply(retp, function(x) (retw - x))
retx <- do.call(cbind, retx)
sum(retx %*% weightv)
# Calculate the weighted sum of absolute excess returns
retx <- abs(retx) %*% weightv
# Total dollar amount of stocks that need to be traded
retx <- retx*rutils::lagit(wealth_pda)
# Cumulative transaction costs
costs <- bid_offer*cumsum(retx)/2
# Subtract transaction costs from wealth
wealth_pda <- (wealth_pda - costs)

# dygraph plot of wealth and transaction costs
wealthv <- cbind(wealth_pda, costs)
wealthv <- xts::xts(wealthv, datev)
colnamev <- c("Wealth", "Cumulative Transaction Costs")
colnames(wealthv) <- colnamev
dygraphs::dygraph(wealthv, main="Transaction Costs With Proportional Allocations") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", col="red", strokeWidth=3) %>%
  dyLegend(show="always", width=300)

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
sell_inmay <- posv*retp
wealthv <- cbind(retp, sell_inmay)
colnames(wealthv) <- c("VTI", "sell_in_may")
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))

# Plot wealth of Sell in May strategy
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd], main="Sell in May Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# OR: Open x11 for plotting
x11(width=6, height=5)
par(mar=c(4, 4, 3, 1), oma=c(0, 0, 0, 0))
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("blue", "red")
quantmod::chart_Series(wealthv, theme=plot_theme, name="Sell in May Strategy")
legend("topleft", legend=colnames(wealthv),
  inset=0.1, bg="white", lty=1, lwd=6, y.intersp=0.5,
  col=plot_theme$col$line.col, bty="n")

# Test if Sell in May strategy can time VTI
desv <- cbind(wealth$sell_in_may, 0.5*(retp+abs(retp)), retp^2)
colnames(desv) <- c("VTI", "merton", "treynor")
# Perform Merton-Henriksson test
regmod <- lm(sell_inmay ~ VTI + merton, data=desv)
summary(regmod)
# Perform Treynor-Mazuy test
regmod <- lm(sell_inmay ~ VTI + treynor, data=desv)
summary(regmod)
# Plot Treynor-Mazuy residual scatterplot
resids <- (sell_inmay - regmod$coeff["VTI"]*retp)
plot.default(x=retp, y=resids, xlab="VTI", ylab="residuals")
title(main="Treynor-Mazuy Market Timing Test\n for Sell in May vs VTI", line=0.5)
# Plot fitted (predicted) response values
coefreg <- summary(regmod)$coeff
fittedv <- regmod$fitted.values - coefreg["VTI", "Estimate"]*retp
tvalue <- round(coefreg["treynor", "t value"], 2)
points.default(x=retp, y=fittedv, pch=16, col="red")
text(x=0.0, y=0.8*max(resids), paste("Treynor test t-value =", tvalue))

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

# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(close_close, close_open, open_close)
sqrt(252)*sapply(wealthv,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot log wealth
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Wealth of Close-to-Close, Close-to-Open, and Open-to-Close Strategies") %>%
  dySeries(name="close_close", label="Close-to-Close (static)", strokeWidth=2, col="blue") %>%
  dySeries(name="close_open", label="Close-to-Open (overnight)", strokeWidth=2, col="red") %>%
  dySeries(name="open_close", label="Open-to-Close (daytime)", strokeWidth=2, col="green") %>%
  dyLegend(width=600)

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
colnamev <- c("VTI", "Strategy")
colnames(wealthv) <- colnamev
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# dygraph plot VTI Turn of the Month strategy
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Turn of the Month Strategy") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", strokeWidth=2, col="red")

# Calculate the VTI returns
retp <- na.omit(rutils::etfenv$returns$VTI)
datev <- zoo::index(retp)
retp <- drop(zoo::coredata(retp))
nrows <- NROW(retp)
# Simulate stop-loss strategy
stopl <- 0.05
maxp <- 0.0
retc <- 0.0
pnls <- retp
for (i in 1:(nrows-1)) {
# Calculate the drawdown
  retc <- retc + retp[i]
  maxp <- max(maxp, retc)
  dd <- (retc - maxp)
# Check for stop-loss
  if (dd < -stopl*maxp)
    pnls[i+1] <- 0
}  # end for
# Same but without using explicit loops
cumsumv <- cumsum(retp)
cummaxv <- cummax(cumsumv)
dd <- (cumsumv - cummaxv)
pnls2 <- retp
isdd <- rutils::lagit(dd < -stopl*cummaxv)
pnls2 <- ifelse(isdd, 0, pnls2)
all.equal(pnls, pnls2)

# Combine data
wealthv <- xts::xts(cbind(retp, pnls), datev)
colnamev <- c("VTI", "Strategy")
colnames(wealthv) <- colnamev
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# dygraph plot VTI stop-loss strategy
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="VTI Stop-loss Strategy") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", strokeWidth=2, col="red")

# Simulate multiple stop-loss strategies
cumsumv <- cumsum(retp)
cummaxv <- cummax(cumsumv)
dd <- (cumsumv - cummaxv)
stopv <- 0.01*(1:30)
cum_pnls <- sapply(stopv, function(stopl) {
  pnls <- retp
  isdd <- rutils::lagit(dd < -stopl*cummaxv)
  pnls <- ifelse(isdd, 0, pnls)
  sum(pnls)
})  # end sapply

# Plot cumulative pnls for stop-loss strategies
plot(x=stopv, y=cum_pnls,
     main="Cumulative PnLs for Stop-loss Strategies",
     xlab="stop-loss level", ylab="cumulative pnl",
     t="l", lwd=3, col="blue")

# Simulate stop-loss strategy
stopl <- 0.05
maxp <- 0.0
minp <- 0.0
retc <- 0.0
pnls <- retp
for (i in 1:(nrows-1)) {
# Calculate the drawdown
  retc <- retc + retp[i]
  maxp <- max(maxp, retc)
  dd <- (retc - maxp)
# Check for stop-loss
  if (dd < -stopl*maxp) {
    pnls[i+1] <- 0
    minp <- min(minp, retc)
    du <- (retc - minp)
# Check for gain
    if (du > stopl*minp) {
pnls[i+1] <- retp[i+1]
    }  # end if
  } else {
    minp <- retc
  }  # end if
}  # end for

# Combine data
wealthv <- xts::xts(cbind(retp, pnls), datev)
colnamev <- c("VTI", "Strategy")
colnames(wealthv) <- colnamev
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# dygraph plot VTI stop-loss strategy
dygraphs::dygraph(cumsum(wealthv), main="VTI Stop-loss Strategy") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", strokeWidth=2, col="red")

# Extract the log VTI prices
ohlc <- rutils::etfenv$VTI
closep <- log(quantmod::Cl(ohlc))
colnames(closep) <- "VTI"
nrows <- NROW(closep)
# Calculate the EWMA weights
look_back <- 111
lambda <- 0.9
weightv <- lambda^(0:look_back)
weightv <- weightv/sum(weightv)
# Calculate the EWMA prices as the convolution
ewmacpp <- HighFreq::roll_sumw(closep, weightv=weightv)
pricev <- cbind(closep, ewmacpp)
colnames(pricev) <- c("VTI", "VTI EWMA")

# Dygraphs plot with custom line colors
colnamev <- colnames(pricev)
dygraphs::dygraph(pricev["2008/2009"], main="VTI EWMA Prices") %>%
  dySeries(name=colnamev[1], strokeWidth=1, col="blue") %>%
  dySeries(name=colnamev[2], strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)
# Standard plot of  EWMA prices with custom line colors
x11(width=6, height=5)
plot_theme <- chart_theme()
colorv <- c("blue", "red")
plot_theme$col$line.col <- colorv
quantmod::chart_Series(pricev["2009"], theme=plot_theme,
       lwd=2, name="VTI EWMA Prices")
legend("topleft", legend=colnames(pricev), y.intersp=0.5,
 inset=0.1, bg="white", lty=1, lwd=6, cex=0.8,
 col=plot_theme$col$line.col, bty="n")

# Calculate the EWMA prices recursively using C++ code
ewmar <- .Call(stats:::C_rfilter, closep, lambda, c(as.numeric(closep[1])/(1-lambda), double(NROW(closep))))[-1]
# Or R code
# ewmar <- filter(closep, filter=lambda, init=as.numeric(closep[1, 1])/(1-lambda), method="recursive")
ewmar <- (1-lambda)*ewmar
# Calculate the EWMA prices recursively using RcppArmadillo
ewmacpp <- HighFreq::run_mean(closep, lambda=lambda)
all.equal(drop(ewmacpp), ewmar)
# Compare the speed of C++ code with RcppArmadillo
library(microbenchmark)
summary(microbenchmark(
  Rcpp=HighFreq::run_mean(closep, lambda=lambda),
  rfilter=.Call(stats:::C_rfilter, closep, lambda, c(as.numeric(closep[1])/(1-lambda), double(NROW(closep)))),
  times=10))[, c(1, 4, 5)]

# Dygraphs plot with custom line colors
pricev <- cbind(closep, ewmacpp)
colnames(pricev) <- c("VTI", "VTI EWMA")
colnamev <- colnames(pricev)
dygraphs::dygraph(pricev["2008/2009"], main="Recursive VTI EWMA Prices") %>%
  dySeries(name=colnamev[1], strokeWidth=1, col="blue") %>%
  dySeries(name=colnamev[2], strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)
# Standard plot of  EWMA prices with custom line colors
plot_theme <- chart_theme()
colorv <- c("blue", "red")
plot_theme$col$line.col <- colorv
quantmod::chart_Series(pricev["2009"], theme=plot_theme,
       lwd=2, name="VTI EWMA Prices")
legend("topleft", legend=colnames(pricev), y.intersp=0.5,
 inset=0.1, bg="white", lty=1, lwd=6, cex=0.8,
 col=plot_theme$col$line.col, bty="n")

# Calculate the EWMA prices recursively using C++ code
lambda <- 0.984
ewmacpp <- HighFreq::run_mean(closep, lambda=lambda)
# Calculate the positions, either: -1, 0, or 1
indic <- sign(closep - ewmacpp)
posv <- rutils::lagit(indic, lagg=1)
# Create colors for background shading
crossd <- (rutils::diffit(posv) != 0)
shadev <- posv[crossd]
crossd <- c(zoo::index(shadev), end(posv))
shadev <- ifelse(drop(zoo::coredata(shadev)) == 1, "lightgreen", "antiquewhite")
# Create dygraph object without plotting it
dyplot <- dygraphs::dygraph(pricev, main="VTI EWMA Prices") %>%
  dySeries(name=colnamev[1], strokeWidth=1, col="blue") %>%
  dySeries(name=colnamev[2], strokeWidth=3, col="red") %>%
  dyLegend(show="always", width=300)


# Add shading to dygraph object
for (i in 1:NROW(shadev)) {
    dyplot <- dyplot %>% dyShading(from=crossd[i], to=crossd[i+1], color=shadev[i])
}  # end for
# Plot the dygraph object
dyplot
# Standard plot of EWMA prices with position shading
quantmod::chart_Series(pricev, theme=plot_theme,
       lwd=2, name="VTI EWMA Prices")
add_TA(posv > 0, on=-1, col="lightgreen", border="lightgreen")
add_TA(posv < 0, on=-1, col="lightgrey", border="lightgrey")
legend("topleft", legend=colnames(pricev),
 inset=0.1, bg="white", lty=1, lwd=6, y.intersp=0.5,
 col=plot_theme$col$line.col, bty="n")

# Calculate the daily profits and losses of EWMA strategy
retp <- rutils::diffit(closep)  # VTI returns
pnls <- retp*posv
colnames(pnls) <- "EWMA"
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "EWMA PnL")
# Annualized Sharpe ratio of EWMA strategy
sqrt(252)*sapply(wealthv, function (x) mean(x)/sd(x))
# The crossover strategy has a negative correlation to VTI
cor(wealthv)[1, 2]
# Plot dygraph of EWMA strategy wealth
# Create dygraph object without plotting it
colorv <- c("blue", "red")
dyplot <- dygraphs::dygraph(cumsum(wealthv), main="Performance of EWMA Strategy") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Add shading to dygraph object
for (i in 1:NROW(shadev)) {
    dyplot <- dyplot %>%
dyShading(from=crossd[i], to=crossd[i+1], color=shadev[i])
}  # end for
# Plot the dygraph object
dyplot

# Standard plot of EWMA strategy wealth
x11(width=6, height=5)
plot_theme <- chart_theme()
plot_theme$col$line.col <- colorv
quantmod::chart_Series(cumsum(wealthv), theme=plot_theme,
       name="Performance of EWMA Strategy")
add_TA(posv > 0, on=-1, col="lightgreen", border="lightgreen")
add_TA(posv < 0, on=-1, col="lightgrey", border="lightgrey")
legend("top", legend=colnames(wealthv), y.intersp=0.5,
 inset=0.05, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")

# Test EWMA crossover market timing of VTI using Treynor-Mazuy test
desv <- cbind(pnls, retp, retp^2)
desv <- na.omit(desv)
colnames(desv) <- c("EWMA", "VTI", "treynor")
regmod <- lm(EWMA ~ VTI + treynor, data=desv)
summary(regmod)
# Plot residual scatterplot
resids <- (desv$EWMA - regmod$coeff["VTI"]*retp)
resids <- regmod$residuals
plot.default(x=retp, y=resids, xlab="VTI", ylab="residuals")
title(main="Treynor-Mazuy Market Timing Test\n for EWMA Crossover vs VTI", line=0.5)
# Plot fitted (predicted) response values
coefreg <- summary(regmod)$coeff
fittedv <- regmod$fitted.values - coefreg["VTI", "Estimate"]*retp
tvalue <- round(coefreg["treynor", "t value"], 2)
points.default(x=retp, y=fittedv, pch=16, col="red")
text(x=0.0, y=0.8*max(resids), paste("Treynor test t-value =", tvalue))

# Determine trade dates right after EWMA has crossed prices
indic <- sign(closep - ewmacpp)
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
colnames(wealthv) <- c("EWMA", "Lagged")
# Annualized Sharpe ratios of EWMA strategies
sharper <- sqrt(252)*sapply(wealthv, function (x) mean(x)/sd(x))
# Plot both strategies
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd], main=paste("EWMA Crossover Strategy", paste(paste(names(sharper), round(sharper, 3), sep="="), collapse=", "))) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Calculate the positions, either: -1, 0, or 1
indic <- sign(closep - ewmacpp)
posv <- rutils::lagit(indic, lagg=1)
# Calculate the daily pnl for days without trades
pnls <- retp*posv
# Determine trade dates right after EWMA has crossed prices
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
colnames(wealthv) <- c("VTI", "EWMA PnL")
# Annualized Sharpe ratio of EWMA strategy
sqrt(252)*sapply(wealthv, function (x) mean(x)/sd(x))
# The crossover strategy has a negative correlation to VTI
cor(wealthv)[1, 2]

# Plot dygraph of EWMA strategy wealth
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd], main="EWMA Strategy Trading at the Open Price") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Standard plot of EWMA strategy wealth
quantmod::chart_Series(cumsum(wealthv)[endd], theme=plot_theme,
       name="EWMA Strategy Trading at the Open Price")
legend("top", legend=colnames(wealthv),
 inset=0.05, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")

# bid_offer equal to 10 bps for liquid ETFs
bid_offer <- 0.001
# Calculate the transaction costs
costs <- 0.5*bid_offer*abs(poslag - posv)
# Plot strategy with transaction costs
wealthv <- cbind(pnls, pnls - costs)
colnames(wealthv) <- c("EWMA", "EWMA w Costs")
colorv <- c("blue", "red")
dygraphs::dygraph(cumsum(wealthv)[endd], main="EWMA Strategy With Transaction Costs") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=300)

sim_ewma <- function(ohlc, lambda=0.9, look_back=333, bid_offer=0.001,
                trend=1, lagg=1) {
  closep <- quantmod::Cl(ohlc)
  retp <- rutils::diffit(closep)
  nrows <- NROW(ohlc)
  # Calculate the EWMA prices
  ewmacpp <- HighFreq::run_mean(closep, lambda=lambda)
  # Calculate the indicator
  indic <- trend*sign(closep - ewmacpp)
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
  costs <- 0.5*bid_offer*abs(rutils::diffit(posv))
  pnls <- (pnls - costs)
  # Calculate the strategy returns
  pnls <- cbind(posv, pnls)
  colnames(pnls) <- c("positions", "pnls")
  pnls
}  # end sim_ewma

source("/Users/jerzy/Develop/lecture_slides/scripts/ewma_model.R")
lambdav <- seq(from=0.97, to=0.99, by=0.004)
# Perform lapply() loop over lambdav
pnltrend <- lapply(lambdav, function(lambda) {
  # Simulate EWMA strategy and Calculate the returns
  sim_ewma(ohlc=ohlc, lambda=lambda, look_back=look_back, bid_offer=0, lagg=2)[, "pnls"]
})  # end lapply
pnltrend <- do.call(cbind, pnltrend)
colnames(pnltrend) <- paste0("lambda=", lambdav)

# Plot dygraph of multiple EWMA strategies
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnltrend))
endd <- rutils::calc_endpoints(pnltrend, interval="weeks")
dygraphs::dygraph(cumsum(pnltrend)[endd], main="Cumulative Returns of Trend Following EWMA Strategies") %>%
  dyOptions(colors=colorv, strokeWidth=1) %>%
  dyLegend(show="always", width=400)
# Plot EWMA strategies with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- colorv
quantmod::chart_Series(cumsum(pnltrend), theme=plot_theme,
  name="Cumulative Returns of EWMA Strategies")
legend("topleft", legend=colnames(pnltrend), inset=0.1,
  bg="white", cex=0.8, lwd=rep(6, NCOL(pnltrend)),
  col=plot_theme$col$line.col, bty="n")

# Initialize compute cluster under Windows
library(parallel)
ncores <- detectCores() - 1  # Number of cores
cluster <- makeCluster(detectCores()-1)
clusterExport(cluster,
  varlist=c("ohlc", "look_back", "sim_ewma"))
# Perform parallel loop over lambdav under Windows
pnltrend <- parLapply(cluster, lambdav, function(lambda) {
  library(quantmod)
  # Simulate EWMA strategy and Calculate the returns
  sim_ewma(ohlc=ohlc, lambda=lambda, look_back=look_back)[, "pnls"]
})  # end parLapply
stopCluster(cluster)  # Stop R processes over cluster under Windows
# Perform parallel loop over lambdav under Mac-OSX or Linux
pnltrend <- mclapply(lambdav, function(lambda) {
  library(quantmod)
  # Simulate EWMA strategy and Calculate the returns
  sim_ewma(ohlc=ohlc, lambda=lambda, look_back=look_back)[, "pnls"]
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
     main="Performance of EWMA Trend Following Strategies
     as Function of the Decay Parameter Lambda")

# Calculate the optimal lambda
lambda <- lambdav[which.max(sharpetrend)]
# Simulate best performing strategy
ewmatrend <- sim_ewma(ohlc=ohlc, lambda=lambda, bid_offer=0, lagg=2)
posv <- ewmatrend[, "positions"]
trendopt <- ewmatrend[, "pnls"]
wealthv <- cbind(retp, trendopt)
colnames(wealthv) <- c("VTI", "EWMA PnL")
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
cor(wealthv)[1, 2]
# Plot dygraph of EWMA strategy wealth
dygraphs::dygraph(cumsum(wealthv)[endd], main="Performance of Optimal Trend Following EWMA Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Plot EWMA PnL with position shading
# Standard plot of EWMA strategy wealth
x11(width=6, height=5)
plot_theme <- chart_theme()
plot_theme$col$line.col <- colorv
quantmod::chart_Series(cumsum(wealthv), theme=plot_theme,
       name="Performance of EWMA Strategy")
add_TA(posv > 0, on=-1, col="lightgreen", border="lightgreen")
add_TA(posv < 0, on=-1, col="lightgrey", border="lightgrey")
legend("top", legend=colnames(wealthv),
 inset=0.05, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")

source("/Users/jerzy/Develop/lecture_slides/scripts/ewma_model.R")
lambdav <- seq(0.6, 0.7, 0.01)
# Perform lapply() loop over lambdav
pnlrevert <- lapply(lambdav, function(lambda) {
  # Simulate EWMA strategy and Calculate the returns
  sim_ewma(ohlc=ohlc, lambda=lambda, bid_offer=0, trend=(-1))[, "pnls"]
})  # end lapply
pnlrevert <- do.call(cbind, pnlrevert)
colnames(pnlrevert) <- paste0("lambda=", lambdav)
# Plot dygraph of mean reverting EWMA strategies
colorv <- colorRampPalette(c("blue", "red"))(NROW(lambdav))
dygraphs::dygraph(cumsum(pnlrevert)[endd], main="Returns of Mean Reverting EWMA Strategies (No Costs)") %>%
  dyOptions(colors=colorv, strokeWidth=1) %>%
  dyLegend(show="always", width=400)
# Plot EWMA strategies with custom line colors
x11(width=6, height=5)
plot_theme <- chart_theme()
plot_theme$col$line.col <- colorv
quantmod::chart_Series(pnlrevert,
  theme=plot_theme, name="Cumulative Returns of Mean Reverting EWMA Strategies")
legend("topleft", legend=colnames(pnlrevert),
  inset=0.1, bg="white", cex=0.8, lwd=6,
  col=plot_theme$col$line.col, bty="n")

# Calculate the Sharpe ratios of strategy returns
sharperevert <- sqrt(252)*sapply(pnlrevert, function(xtsv) {
  mean(xtsv)/sd(xtsv)
})  # end sapply
plot(x=lambdav, y=sharperevert, t="l",
     xlab="lambda", ylab="Sharpe",
     main="Performance of EWMA Mean Reverting Strategies
     as Function of the Decay Parameter Lambda")

# Calculate the optimal lambda
lambda <- lambdav[which.max(sharperevert)]
# Simulate best performing strategy
ewmarevert <- sim_ewma(ohlc=ohlc, bid_offer=0.0,
  lambda=lambda, trend=(-1))
posv <- ewmarevert[, "positions"]
revertopt <- ewmarevert[, "pnls"]
wealthv <- cbind(retp, revertopt)
colnames(wealthv) <- c("VTI", "EWMA PnL")
# Plot dygraph of EWMA strategy wealth
colorv <- c("blue", "red")
dygraphs::dygraph(cumsum(wealthv)[endd], main="Optimal Mean Reverting EWMA Strategy (No Costs)") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Standard plot of EWMA strategy wealth
x11(width=6, height=5)
plot_theme <- chart_theme()
plot_theme$col$line.col <- colorv
quantmod::chart_Series(cumsum(wealthv), theme=plot_theme,
       name="Optimal Mean Reverting EWMA Strategy")
add_TA(posv > 0, on=-1, col="lightgreen", border="lightgreen")
add_TA(posv < 0, on=-1, col="lightgrey", border="lightgrey")
legend("top", legend=colnames(wealthv),
 inset=0.05, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")

# Calculate the correlation between trend following and mean reverting strategies
trendopt <- ewmatrend[, "pnls"]
colnames(trendopt) <- "trend"
revertopt <- ewmarevert[, "pnls"]
colnames(revertopt) <- "revert"
cor(cbind(retp, trendopt, revertopt))
# Calculate the combined strategy
combstrat <- (retp + trendopt + revertopt)/3
colnames(combstrat) <- "combined"
# Calculate the annualized Sharpe ratio of strategy returns
retc <- cbind(retp, trendopt, revertopt, combstrat)
colnames(retc) <- c("VTI", "Trending", "Reverting", "Combined")
sqrt(252)*sapply(retc, function(xtsv) mean(xtsv)/sd(xtsv))

# Plot dygraph of EWMA strategy wealth
colorv <- c("blue", "red", "green", "purple")
dygraphs::dygraph(cumsum(retc)[endd], main="Performance of Combined EWMA Strategies") %>%
  dySeries("VTI", color="blue", strokeWidth=1) %>%
  dySeries("Trending", color="red", strokeWidth=1) %>%
  dySeries("Reverting", color="green", strokeWidth=1) %>%
  dySeries("Combined", color="purple", strokeWidth=3) %>%
  dyLegend(show="always", width=400)
# Standard plot of EWMA strategy wealth
plot_theme <- chart_theme()
plot_theme$col$line.col <- colorv
quantmod::chart_Series(pnls, theme=plot_theme,
       name="Performance of Combined EWMA Strategies")
legend("topleft", legend=colnames(pnls),
 inset=0.05, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")

# Calculate the weights proportional to Sharpe ratios
weightv <- c(sharpetrend, sharperevert)
weightv[weightv<0] <- 0
weightv <- weightv/sum(weightv)
retc <- cbind(pnltrend, pnlrevert)
retc <- retc %*% weightv
retc <- cbind(retp, retc)
colnames(retc) <- c("VTI", "EWMA PnL")
# Plot dygraph of EWMA strategy wealth
colorv <- c("blue", "red")
dygraphs::dygraph(cumsum(retc)[endd], main="Performance of Ensemble of EWMA Strategies") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Standard plot of EWMA strategy wealth
plot_theme <- chart_theme()
plot_theme$col$line.col <- colorv
quantmod::chart_Series(cumsum(retc), theme=plot_theme,
       name="Performance of Ensemble of EWMA Strategies")
legend("topleft", legend=colnames(pnls),
 inset=0.05, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")

# Calculate the fast and slow EWMAs
lambdaf <- 0.89
lambdas <- 0.95
# Calculate the EWMA prices
ewmaf <- HighFreq::run_mean(closep, lambda=lambdaf)
ewmas <- HighFreq::run_mean(closep, lambda=lambdas)
# Calculate the EWMA prices
pricev <- cbind(closep, ewmaf, ewmas)
colnames(pricev) <- c("VTI", "EWMA fast", "EWMA slow")
# Calculate the positions, either: -1, 0, or 1
indic <- sign(ewmaf - ewmas)
lagg <- 2
indic <- HighFreq::roll_sum(indic, lagg)
posv <- rep(NA_integer_, nrows)
posv[1] <- 0
posv <- ifelse(indic == lagg, 1, posv)
posv <- ifelse(indic == (-lagg), -1, posv)
posv <- zoo::na.locf(posv, na.rm=FALSE)
posv <- xts::xts(posv, order.by=zoo::index(closep))
posv <- rutils::lagit(posv, lagg=1)

# Create colors for background shading
crossd <- (rutils::diffit(posv) != 0)
shadev <- posv[crossd]
crossd <- c(zoo::index(shadev), end(posv))
shadev <- ifelse(drop(zoo::coredata(shadev)) == 1, "lightgreen", "antiquewhite")
# Plot dygraph
colnamev <- colnames(pricev)
dyplot <- dygraphs::dygraph(pricev[endd], main="VTI Dual EWMA Prices") %>%
  dySeries(name=colnamev[1], strokeWidth=1, col="blue") %>%
  dySeries(name=colnamev[2], strokeWidth=2, col="red") %>%
  dySeries(name=colnamev[3], strokeWidth=2, col="purple") %>%
  dyLegend(show="always", width=300)
for (i in 1:NROW(shadev)) {
    dyplot <- dyplot %>% dyShading(from=crossd[i], to=crossd[i+1], color=shadev[i])
}  # end for
dyplot

# Calculate the daily profits and losses of strategy
pnls <- retp*posv
colnames(pnls) <- "Strategy"
wealthv <- cbind(retp, pnls)
# Annualized Sharpe ratio of Dual EWMA strategy
sharper <- sqrt(252)*sapply(wealthv, function (x) mean(x)/sd(x))
# The crossover strategy has a negative correlation to VTI
cor(wealthv)[1, 2]

# Plot Dual EWMA strategy
dyplot <- dygraphs::dygraph(cumsum(wealthv)[endd], main=paste("EWMA Dual Crossover Strategy, Sharpe", paste(paste(names(sharper), round(sharper, 3), sep="="), collapse=", "))) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2)
# Add shading to dygraph object
for (i in 1:NROW(shadev)) {
    dyplot <- dyplot %>% dyShading(from=crossd[i], to=crossd[i+1], color=shadev[i])
}  # end for
# Plot the dygraph object
dyplot

sim_ewma2 <- function(ohlc, lambdaf=0.1, lambdas=0.01, look_back=333,
                bid_offer=0.001, trend=1, lagg=1) {
  if (lambdaf >= lambdas) return(NA)
  closep <- quantmod::Cl(ohlc)
  retp <- rutils::diffit(closep)
  nrows <- NROW(ohlc)
  # Calculate the EWMA prices
  ewmaf <- HighFreq::run_mean(closep, lambda=lambdaf)
  ewmas <- HighFreq::run_mean(closep, lambda=lambdas)
  # Calculate the positions, either: -1, 0, or 1
  indic <- sign(ewmaf - ewmas)
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
  costs <- 0.5*bid_offer*abs(rutils::diffit(posv))
  pnls <- (pnls - costs)
  # Calculate the strategy returns
  pnls <- cbind(posv, pnls)
  colnames(pnls) <- c("positions", "pnls")
  pnls
}  # end sim_ewma2

source("/Users/jerzy/Develop/lecture_slides/scripts/ewma_model.R")
lambdafv <- seq(from=0.85, to=0.99, by=0.01)
lambdasv <- seq(from=0.85, to=0.99, by=0.01)
# Calculate the Sharpe ratio of dual EWMA strategy
calc_sharpe <- function(ohlc, lambdaf, lambdas, look_back, bid_offer, trend, lagg) {
  if (lambdaf >= lambdas) return(NA)
  pnls <- sim_ewma2(ohlc=ohlc, lambdaf=lambdaf, lambdas=lambdas, look_back=look_back,
    bid_offer=bid_offer, trend=trend, lagg=lagg)[, "pnls"]
  sqrt(252)*mean(pnls)/sd(pnls)
}  # end calc_sharpe
# Vectorize calc_sharpe with respect to lambdaf and lambdas
calc_sharpe <- Vectorize(FUN=calc_sharpe,
  vectorize.args=c("lambdaf", "lambdas"))
# Calculate the matrix of PnLs
sharpem <- outer(lambdafv, lambdasv, FUN=calc_sharpe, ohlc=ohlc,
           look_back=look_back, bid_offer=0.0, trend=1, lagg=2)
# Or perform two sapply() loops over lambda vectors
sharpem <- sapply(lambdasv, function(lambdas) {
  sapply(lambdafv, function(lambdaf) {
    if (lambdaf >= lambdas) return(NA)
    calc_sharpe(ohlc=ohlc, lambdaf=lambdaf, lambdas=lambdas,
          look_back=look_back, bid_offer=0.0, trend=1, lagg=2)
  })  # end sapply
})  # end sapply
colnames(sharpem) <- lambdasv
rownames(sharpem) <- lambdafv

# Calculate the PnLs for the optimal strategy
whichv <- which(sharpem == max(sharpem, na.rm=TRUE), arr.ind=TRUE)
lambdaf <- lambdafv[whichv[1]]
lambdas <- lambdasv[whichv[2]]
ewma_opt <- sim_ewma2(ohlc=ohlc, lambdaf=lambdaf, lambdas=lambdas,
  look_back=look_back, bid_offer=0.0, trend=1, lagg=2)
pnls <- ewma_opt[, "pnls"]
wealthv <- cbind(retp, pnls)
colnames(wealthv)[2] <- "EWMA"
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Annualized Sharpe ratio of Dual EWMA strategy
sharper <- sqrt(252)*sapply(wealthv, function (x) mean(x)/sd(x))
# The crossover strategy has a negative correlation to VTI
cor(wealthv)[1, 2]

# Create colors for background shading
posv <- ewma_opt[, "positions"]
crossd <- (rutils::diffit(posv) != 0)
shadev <- posv[crossd]
crossd <- c(zoo::index(shadev), end(posv))
shadev <- ifelse(drop(zoo::coredata(shadev)) == 1, "lightgreen", "antiquewhite")
# Plot Optimal Dual EWMA strategy
dyplot <- dygraphs::dygraph(cumsum(wealthv), main=paste("Optimal Dual EWMA Strategy, Sharpe", paste(paste(names(sharper), round(sharper, 3), sep="="), collapse=", "))) %>%
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
           FUN=calc_sharpe, ohlc=ohlc[insample, ],
           look_back=look_back, bid_offer=0.0, trend=1, lagg=2)
colnames(sharpem) <- lambdasv
rownames(sharpem) <- lambdafv
# Calculate the PnLs for the optimal strategy
whichv <- which(sharpem == max(sharpem, na.rm=TRUE), arr.ind=TRUE)
lambdaf <- lambdafv[whichv[1]]
lambdas <- lambdasv[whichv[2]]
pnls <- sim_ewma2(ohlc=ohlc, lambdaf=lambdaf, lambdas=lambdas,
            look_back=look_back, bid_offer=0.0, trend=1, lagg=2)[, "pnls"]
wealthv <- cbind(retp, pnls)
colnames(wealthv)[2] <- "EWMA"
# Calculate the Sharpe and Sortino ratios in-sample and out-of-sample
sqrt(252)*sapply(wealthv[insample, ],
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
sqrt(252)*sapply(wealthv[outsample, ],
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))

# Dygraphs plot with custom line colors
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd], main="Dual EWMA Strategy Out-of-Sample") %>%
  dyEvent(zoo::index(wealthv[last(insample)]), label="in-sample", strokePattern="solid", color="green") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2)

# Calculate the log OHLC prices and volumes
ohlc <- rutils::etfenv$VTI
closep <- log(quantmod::Cl(ohlc))
colnames(closep) <- "VTI"
volum <- quantmod::Vo(ohlc)
colnames(volum) <- "Volume"
nrows <- NROW(closep)
# Calculate the VWAP prices
look_back <- 21
vwap <- HighFreq::roll_sum(closep*volum, look_back)
volumr <- HighFreq::roll_sum(volum, look_back)
vwap <- vwap/volumr
colnames(vwap) <- "VWAP"
pricev <- cbind(closep, vwap)

# Dygraphs plot with custom line colors
colorv <- c("blue", "red")
dygraphs::dygraph(pricev["2009"], main="VTI VWAP Prices") %>%
  dyOptions(colors=colorv, strokeWidth=2)
# Plot VWAP prices with custom line colors
x11(width=6, height=5)
plot_theme <- chart_theme()
plot_theme$col$line.col <- colors
quantmod::chart_Series(pricev["2009"], theme=plot_theme,
       lwd=2, name="VTI VWAP Prices")
legend("bottomright", legend=colnames(pricev),
 inset=0.1, bg="white", lty=1, lwd=6, cex=0.8,
 col=plot_theme$col$line.col, bty="n")

# Calculate the VWAP prices recursively using C++ code
lambda <- 0.995
volumer <- .Call(stats:::C_rfilter, volum, lambda, c(as.numeric(volum[1])/(1-lambda), double(NROW(volum))))[-1]
pricer <- .Call(stats:::C_rfilter, volum*closep, lambda, c(as.numeric(volum[1]*closep[1])/(1-lambda), double(NROW(closep))))[-1]
vwapr <- pricer/volumer
# Calculate the VWAP prices recursively using RcppArmadillo
vwapcpp <- HighFreq::run_mean(closep, lambda=lambda, weightv=volum)
all.equal(vwapr, drop(vwapcpp))
# Dygraphs plot the VWAP prices
pricev <- xts(cbind(vwap, vwapr), zoo::index(ohlc))
colnames(pricev) <- c("VWAP rolling", "VWAP recursive")
dygraphs::dygraph(pricev["2009"], main="VWAP Prices") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Calculate the VWAP prices recursively using RcppArmadillo
lambda <- 0.99
vwapcpp <- HighFreq::run_mean(closep, lambda=lambda, weightv=volum)
# Calculate the positions from lagged indicator
indic <- sign(closep - vwapcpp)
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
colnamev <- colnames(wealthv)

# Annualized Sharpe ratios of VTI and VWAP strategy
sharper <- sqrt(252)*sapply(wealthv, function (x) mean(x)/sd(x))
# Create colors for background shading
crossd <- (rutils::diffit(posv) != 0)
shadev <- posv[crossd]
crossd <- c(zoo::index(shadev), end(posv))
shadev <- ifelse(drop(zoo::coredata(shadev)) == 1, "lightgreen", "antiquewhite")
# Plot dygraph of VWAP strategy
# Create dygraph object without plotting it
dyplot <- dygraphs::dygraph(cumsum(wealthv), main=paste("VWAP Crossover Strategy, Sharpe", paste(paste(names(sharper), round(sharper, 3), sep="="), collapse=", "))) %>%
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
colnamev <- colnames(wealthv)
dygraphs::dygraph(cumsum(wealthv)[endd], paste("VWAP Strategy Sharpe", paste(paste(names(sharper), round(sharper, 3), sep="="), collapse=", "))) %>%
  dySeries(name=colnamev[1], col="blue", strokeWidth=1) %>%
  dySeries(name=colnamev[2], col="red", strokeWidth=1) %>%
  dySeries(name=colnamev[3], col="purple", strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Or
dygraphs::dygraph(cumsum(wealthv)[endd],
  main=paste("VWAP Strategy Sharpe", paste(paste(names(sharper), round(sharper, 3), sep="="), collapse=", "))) %>%
  dyOptions(colors=c("blue", "red", "purple"), strokeWidth=1) %>%
  dyLegend(show="always", width=300)

# Test VWAP crossover market timing of VTI using Treynor-Mazuy test
desv <- cbind(pnls, retp, retp^2)
desv <- na.omit(desv)
colnames(desv) <- c("VWAP", "VTI", "treynor")
regmod <- lm(VWAP ~ VTI + treynor, data=desv)
summary(regmod)
# Plot residual scatterplot
resids <- (desv$VWAP - regmod$coeff["VTI"]*retp)
resids <- regmod$residuals
# x11(width=6, height=6)
plot.default(x=retp, y=resids, xlab="VTI", ylab="residuals")
title(main="Treynor-Mazuy Market Timing Test\n for VWAP Crossover vs VTI", line=0.5)
# Plot fitted (predicted) response values
coefreg <- summary(regmod)$coeff
fittedv <- regmod$fitted.values - coefreg["VTI", "Estimate"]*retp
tvalue <- round(coefreg["treynor", "t value"], 2)
points.default(x=retp, y=fittedv, pch=16, col="red")
text(x=0.0, y=0.8*max(resids), paste("Treynor test t-value =", tvalue))

sim_vwap <- function(ohlc, lambda=0.9, bid_offer=0.001, trend=1, lagg=1) {
  closep <- log(quantmod::Cl(ohlc))
  volum <- quantmod::Vo(ohlc)
  retp <- rutils::diffit(closep)
  nrows <- NROW(ohlc)
  # Calculate the VWAP prices
  vwap <- HighFreq::run_mean(closep, lambda=lambda, weightv=volum)
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
  costs <- 0.5*bid_offer*abs(rutils::diffit(posv))
  pnls <- (pnls - costs)
  # Calculate the strategy returns
  pnls <- cbind(posv, pnls)
  colnames(pnls) <- c("positions", "pnls")
  pnls
}  # end sim_vwap

source("/Users/jerzy/Develop/lecture_slides/scripts/ewma_model.R")
lambdav <- seq(from=0.97, to=0.995, by=0.004)
# Perform lapply() loop over lambdav
pnls <- lapply(lambdav, function(lambda) {
  # Simulate VWAP strategy and Calculate the returns
  sim_vwap(ohlc=ohlc, lambda=lambda, bid_offer=0, lagg=2)[, "pnls"]
})  # end lapply
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("lambda=", lambdav)

# Plot dygraph of multiple VWAP strategies
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls)[endd], main="Cumulative Returns of Trend Following VWAP Strategies") %>%
  dyOptions(colors=colorv, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Plot VWAP strategies with custom line colors
x11(width=6, height=5)
plot_theme <- chart_theme()
plot_theme$col$line.col <- colorv
quantmod::chart_Series(cumsum(pnls), theme=plot_theme,
  name="Cumulative Returns of VWAP Strategies")
legend("topleft", legend=colnames(pnls), inset=0.1,
  bg="white", cex=0.8, lwd=rep(6, NCOL(pnls)),
  col=plot_theme$col$line.col, bty="n")

# Extract the log VTI prices
pricev <- log(na.omit(rutils::etfenv$prices$VTI))
nrows <- NROW(pricev)
# Calculate the trailing mean prices
lambda <- 0.9
meanv <- HighFreq::run_mean(pricev, lambda=lambda)
# Calculate the trailing volatilities
volat <- HighFreq::run_var(pricev, lambda=lambda)
volat <- sqrt(volat)

# Dygraphs plot of Bollinger bands
priceb <- cbind(pricev, meanv, meanv+volat, meanv-volat)
colnames(priceb)[2:4] <- c("mean", "upper", "lower")
colnamev <- colnames(priceb)
dygraphs::dygraph(priceb["2008-09/2009-09"], main="VTI Prices and Bollinger Bands") %>%
  dySeries(name=colnamev[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], strokeWidth=2, col="green") %>%
  dySeries(name=colnamev[3], strokeWidth=2, strokePattern="dashed", col="red") %>%
  dySeries(name=colnamev[4], strokeWidth=2, strokePattern="dashed", col="red") %>%
  dyLegend(show="always", width=300)

# Calculate the trailing mean prices and volatilities
lambda <- 0.1
meanv <- HighFreq::run_mean(pricev, lambda=lambda)
volat <- HighFreq::run_var(pricev, lambda=lambda)
volat <- sqrt(volat)
# Prepare the simulation parameters
pricen <- as.numeric(pricev) # Numeric price
pricem <- pricen - meanv # De-meaned price
threshv <- volat
posv <- integer(nrows) # Stock positions
posv[1] <- 0 # Initial position
# Calculate the positions from Bollinger bands
for (it in 2:nrows) {
  if (pricem[it-1] > threshv[it-1]) {
    # Enter short
    posv[it] <- (-1)
  } else if (pricem[it-1] < (-threshv[it-1])) {
    # Enter long
    posv[it] <- 1
  } else if ((posv[it-1] < 0) && (pricem[it-1] < 0)) {
    # Unwind short
    posv[it] <- 0
  } else if ((posv[it-1] > 0) && (pricem[it-1] > 0)) {
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
retv <- rutils::diffit(pricev)
pnls <- retv*posv

# Calculate the Sharpe ratios
wealthv <- cbind(retv, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
sharper <- sqrt(252)*sapply(wealthv, function(x) mean(x)/sd(x[x<0]))
sharper <- round(sharper, 3)
# Dygraphs plot of Bollinger strategy
colnamev <- colnames(wealthv)
captiont <- paste("Bollinger Strategy", "/ \n",
  paste0(paste(colnamev[1:2], "Sharpe =", sharper), collapse=" / "), "/ \n",
  "Number of trades=", ntrades)
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd], main=captiont) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Simulate the modified Bollinger strategy
posv <- integer(nrows) # Stock positions
posv[1] <- 0 # Initial position
for (it in 2:nrows) {
  if (pricem[it-1] > threshv[it-1]) {
    # Enter short
    posv[it] <- (-1)
  } else if (pricem[it-1] < (-threshv[it-1])) {
    # Enter long
    posv[it] <- 1
  } else {
    # Do nothing
    posv[it] <- posv[it-1]
  }  # end if
}  # end for
# Calculate the PnLs
pnls2 <- retv*posv

# Calculate the Sharpe ratios
wealthv <- cbind(pnls, pnls2)
colnames(wealthv) <- c("Bollinger", "Modified")
sharper <- sqrt(252)*sapply(wealthv, function(x) mean(x)/sd(x[x<0]))
sharper <- round(sharper, 3)
# Dygraphs plot of Bollinger strategy
colnamev <- colnames(wealthv)
captiont <- paste("Bollinger Strategy", "/ \n",
  paste0(paste(colnamev[1:2], "Sharpe =", sharper), collapse=" / "), "/ \n",
  "Number of trades=", ntrades)
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd], main=captiont) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Simulate the modified Bollinger strategy quickly
posf <- rep(NA_integer_, nrows)
posf[1] <- 0
posf <- ifelse(pricem > threshv, -1, posf)
posf <- ifelse(pricem < -threshv, 1, posf)
posf <- zoo::na.locf(posf)
# Lag the positions to trade in the next period
posf <- rutils::lagit(posf, lagg=1)
# Compare the positions
all.equal(posv, posf)

# Calculate the trailing mean prices and volatilities of SPY
pricev <- log(quantmod::Cl(HighFreq::SPY))
nrows <- NROW(pricev)
lambda <- 0.1
meanv <- HighFreq::run_mean(pricev, lambda=lambda)
volat <- HighFreq::run_var(pricev, lambda=lambda)
volat <- sqrt(volat)
# Calculate the positions from Bollinger bands
threshv <- volat
pricem <- zoo::coredata(pricev - meanv)
posv <- rep(NA_integer_, nrows)
posv[1] <- 0
posv <- ifelse(pricem > threshv, -1, posv)
posv <- ifelse(pricem < -threshv, 1, posv)
posv <- zoo::na.locf(posv)
# Lag the positions to trade in the next period
posv <- rutils::lagit(posv, lagg=1)
# Calculate the number of trades and the PnLs
ntrades <- sum(abs(rutils::diffit(posv)) > 0)
retv <- rutils::diffit(pricev)
pnls <- retv*posv

# Calculate the Sharpe ratios
wealthv <- cbind(retv, pnls)
colnames(wealthv) <- c("SPY", "Strategy")
nyears <- as.numeric(end(pricev)-start(pricev))/365
sharper <- sqrt(nrows/nyears)*sapply(wealthv, function(x) mean(x)/sd(x[x<0]))
sharper <- round(sharper, 3)
# Dygraphs plot of Bollinger strategy
colnamev <- colnames(wealthv)
captiont <- paste("Bollinger Strategy for Minute SPY", "/ \n",
  paste0(paste(colnamev[1:2], "Sharpe =", sharper), collapse=" / "), "/ \n",
  "Number of trades=", ntrades)
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd], main=captiont) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Extract time series of VTI log prices
pricev <- log(na.omit(rutils::etfenv$prices$VTI))
nrows <- NROW(pricev)
# Define look-back window
look_back <- 11
# Calculate time series of trailing medians
medianv <- HighFreq::roll_mean(pricev, look_back, method="nonparametric")
# medianv <- TTR::runMedian(pricev, n=look_back)
# Calculate time series of MAD
madv <- HighFreq::roll_var(pricev, look_back=look_back, method="nonparametric")
# madv <- TTR::runMAD(pricev, n=look_back)
# Calculate time series of z-scores
zscores <- ifelse(madv > 0, (pricev - medianv)/madv, 0)
zscores[1:look_back, ] <- 0
tail(zscores, look_back)
range(zscores)

# Plot histogram of z-scores
histp <- hist(zscores, col="lightgrey",
  xlab="z-scores", breaks=50, xlim=c(-4, 4),
  ylab="frequency", freq=FALSE, main="Hampel Z-Scores histogram")
lines(density(zscores, adjust=1.5), lwd=3, col="blue")
# Dygraphs plot of Hampel bands
priceb <- cbind(pricev, medianv, medianv+madv, medianv-madv)
colnames(priceb)[2:4] <- c("median", "upper", "lower")
colnamev <- colnames(priceb)
dygraphs::dygraph(priceb["2008-09/2009-09"], main="VTI Prices and Hampel Bands") %>%
  dySeries(name=colnamev[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], strokeWidth=2, col="green") %>%
  dySeries(name=colnamev[3], strokeWidth=2, strokePattern="dashed", col="red") %>%
  dySeries(name=colnamev[4], strokeWidth=2, strokePattern="dashed", col="red") %>%
  dyLegend(show="always", width=300)

# Calculate the time series of trailing medians and MAD
look_back <- 3
medianv <- HighFreq::roll_mean(pricev, look_back, method="nonparametric")
madv <- HighFreq::roll_var(pricev, look_back=look_back, method="nonparametric")
# Calculate the time series of z-scores
zscores <- ifelse(madv > 0, (pricev - medianv)/madv, 0)
zscores[1:look_back, ] <- 0
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
retv <- rutils::diffit(pricev)
pnls <- retv*posv

# Calculate the Sharpe ratios
wealthv <- cbind(retv, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
sharper <- sqrt(252)*sapply(wealthv, function(x) mean(x)/sd(x[x<0]))
sharper <- round(sharper, 3)
# Dygraphs plot of Hampel strategy
captiont <- paste("Hampel Strategy", "/ \n",
  paste0(paste(colnamev[1:2], "Sharpe =", sharper), collapse=" / "), "/ \n",
  "Number of trades=", ntrades)
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
colnamev <- colnames(wealthv)
dygraphs::dygraph(cumsum(wealthv)[endd], main=captiont) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Calculate the trailing mean prices and volatilities of SPY
pricev <- log(quantmod::Cl(HighFreq::SPY))
nrows <- NROW(pricev)
# Calculate the price medians and MAD
look_back <- 3
medianv <- HighFreq::roll_mean(pricev, look_back, method="nonparametric")
madv <- HighFreq::roll_var(pricev, look_back=look_back, method="nonparametric")
# Calculate the time series of z-scores
zscores <- ifelse(madv > 0, (pricev - medianv)/madv, 0)
zscores[1:look_back, ] <- 0
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
retv <- rutils::diffit(pricev)
pnls <- retv*posv

# Calculate the Sharpe ratios
wealthv <- cbind(retv, pnls)
colnames(wealthv) <- c("SPY", "Strategy")
nyears <- as.numeric(end(pricev)-start(pricev))/365
sharper <- sqrt(nrows/nyears)*sapply(wealthv, function(x) mean(x)/sd(x[x<0]))
sharper <- round(sharper, 3)
# Dygraphs plot of Hampel strategy
colnamev <- colnames(wealthv)
captiont <- paste("Hampel Strategy for Minute SPY", "/ \n",
  paste0(paste(colnamev[1:2], "Sharpe =", sharper), collapse=" / "), "/ \n",
  "Number of trades=", ntrades)
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd], main=captiont) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Extract the VTI log OHLC prices
ohlc <- log(rutils::etfenv$VTI)
nrows <- NROW(ohlc)
closep <- quantmod::Cl(ohlc)
retp <- rutils::diffit(closep)
# Calculate the centered volatility
look_back <- 7
half_back <- look_back %/% 2
stdev <- sqrt(HighFreq::roll_var(retp, look_back))
stdev <- rutils::lagit(stdev, lagg=(-half_back))
# Calculate the z-scores of prices
pricez <- (2*closep -
  rutils::lagit(closep, half_back, pad_zeros=FALSE) -
  rutils::lagit(closep, -half_back, pad_zeros=FALSE))
pricez <- ifelse(stdev > 0, pricez/stdev, 0)

# Plot dygraph of z-scores of VTI prices
pricev <- cbind(closep, pricez)
colnames(pricev) <- c("VTI", "Z-scores")
colnamev <- colnames(pricev)
dygraphs::dygraph(pricev["2009"], main="VTI Price Z-Scores") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", strokeWidth=2, col="red")

# Calculate the thresholds for labeling tops and bottoms
confl <- c(0.2, 0.8)
threshv <- quantile(pricez, confl)
# Calculate the vectors of tops and bottoms
tops <- zoo::coredata(pricez > threshv[2])
bottoms <- zoo::coredata(pricez < threshv[1])
# Simulate in-sample VTI strategy
posv <- rep(NA_integer_, nrows)
posv[1] <- 0
posv[tops] <- (-1)
posv[bottoms] <- 1
posv <- zoo::na.locf(posv)
posv <- rutils::lagit(posv)
pnls <- retp*posv

# Plot dygraph of in-sample VTI strategy
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Price Tops and Bottoms Strategy In-Sample") %>%
  dyAxis("y", label="VTI", independentTicks=TRUE) %>%
  dyAxis("y2", label="Strategy", independentTicks=TRUE) %>%
  dySeries(name="VTI", axis="y", label="VTI", strokeWidth=2, col="blue") %>%
  dySeries(name="Strategy", axis="y2", label="Strategy", strokeWidth=2, col="red")

# Calculate the volatility z-scores
volat <- HighFreq::roll_var_ohlc(ohlc=ohlc, look_back=look_back, scale=FALSE)
volatm <- HighFreq::roll_mean(volat, look_back)
volatsd <- sqrt(HighFreq::roll_var(rutils::diffit(volat), look_back))
volatsd[1] <- 0
volatz <- ifelse(volatsd > 0, (volat - volatm)/volatsd, 0)
colnames(volatz) <- "volat"
# Calculate the volume z-scores
volum <- quantmod::Vo(ohlc)
volumean <- HighFreq::roll_mean(volum, look_back)
volumsd <- sqrt(HighFreq::roll_var(rutils::diffit(volum), look_back))
volumsd[1] <- 0
volumz <- ifelse(volumsd > 0, (volum - volumean)/volumsd, 0)
colnames(volumz) <- "volume"

# Calculate the trailing price regression z-scores
datev <- matrix(zoo::index(closep))
look_back <- 21
controlv <- HighFreq::param_reg()
regz <- HighFreq::roll_reg(respv=closep, predv=datev, look_back=look_back, controlv=controlv)
regz <- drop(regz[, NCOL(regz)])
regz[1:look_back] <- 0

# Plot dygraph of z-scores of VTI prices
pricev <- cbind(closep, regz)
colnames(pricev) <- c("VTI", "Z-scores")
colnamev <- colnames(pricev)
dygraphs::dygraph(pricev["2009"], main="VTI Price Z-Scores") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", strokeWidth=2, col="red")

# Define predictor for tops including intercept column
predv <- cbind(volatz, volumz, regz)
predv[1, ] <- 0
predv <- rutils::lagit(predv)
# Fit in-sample logistic regression for tops
logmod <- glm(tops ~ predv, family=binomial(logit))
summary(logmod)
coeff <- logmod$coefficients
fcast <- drop(cbind(rep(1, nrows), predv) %*% coeff)
ordern <- order(fcast)
# Calculate the in-sample forecasts from logistic regression model
fcast <- 1/(1+exp(-fcast))
all.equal(logmod$fitted.values, fcast, check.attributes=FALSE)
hist(fcast)

plot(x=fcast[ordern], y=tops[ordern],
     main="Logistic Regression of Stock Tops",
     col="orange", xlab="predictor", ylab="top")
lines(x=fcast[ordern], y=logmod$fitted.values[ordern], col="blue", lwd=3)
legend(x="topleft", inset=0.1, bty="n", lwd=6,
 legend=c("tops", "logit fitted values"), y.intersp=0.5,
 col=c("orange", "blue"), lty=c(NA, 1), pch=c(1, NA))

# Define discrimination threshold value
threshv <- quantile(fcast, confl[2])
# Calculate the confusion matrix in-sample
confmat <- table(actual=!tops, forecast=(fcast < threshv))
confmat
# Calculate the FALSE positive (type I error)
sum(tops & (fcast < threshv))
# Calculate the FALSE negative (type II error)
sum(!tops & (fcast > threshv))

# Calculate the FALSE positive and FALSE negative rates
confmat <- confmat / rowSums(confmat)
c(typeI=confmat[2, 1], typeII=confmat[1, 2])
# Below is an unsuccessful attempt to draw confusion matrix using xtable
confusion_matrix <- matrix(c("| true positive \\\\ (sensitivity)", "| false negative \\\\ (type II error)", "| false positive \\\\ (type I error)", "| true negative \\\\ (specificity)"), nc=2)
dimnames(confusion_matrix) <- list(forecast=c("FALSE", "TRUE"),
                             actual=c("FALSE", "TRUE"))
print(xtable::xtable(confusion_matrix,
caption="Confusion Matrix"),
caption.placement="top",
comment=FALSE, size="scriptsize",
include.rownames=TRUE,
include.colnames=TRUE)
# end unsuccessful attempt to draw confusion table using xtable

# Confusion matrix as function of threshold
confun <- function(actual, fcast, threshv) {
  forb <- (fcast < threshv)
  conf <- matrix(c(sum(!actual & !forb), sum(actual & !forb),
             sum(!actual & forb), sum(actual & forb)), ncol=2)
  conf <- conf / rowSums(conf)
  c(typeI=conf[2, 1], typeII=conf[1, 2])
}  # end confun
confun(!tops, fcast, threshv=threshv)
# Define vector of discrimination thresholds
threshv <- quantile(fcast, seq(0.01, 0.99, by=0.01))
# Calculate the error rates
error_rates <- sapply(threshv, confun,
  actual=!tops, fcast=fcast)  # end sapply
error_rates <- t(error_rates)
rownames(error_rates) <- threshv
# Calculate the informedness
informv <- 2 - rowSums(error_rates)
plot(threshv, informv, t="l", main="Informedness")
# Find the threshold corresponding to highest informedness
threshm <- threshv[which.max(informv)]
forecastops <- (fcast > threshm)

# Calculate the area under ROC curve (AUC)
error_rates <- rbind(c(1, 0), error_rates)
error_rates <- rbind(error_rates, c(0, 1))
truepos <- (1 - error_rates[, "typeII"])
truepos <- (truepos + rutils::lagit(truepos))/2
falsepos <- rutils::diffit(error_rates[, "typeI"])
abs(sum(truepos*falsepos))
# Plot ROC Curve for stock tops
plot(x=error_rates[, "typeI"], y=1-error_rates[, "typeII"],
     xlab="FALSE positive rate", ylab="TRUE positive rate",
     main="ROC Curve for Stock Tops", type="l", lwd=3, col="blue")
abline(a=0.0, b=1.0, lwd=3, col="orange")

# Fit in-sample logistic regression for bottoms
logmod <- glm(bottoms ~ predv, family=binomial(logit))
summary(logmod)
# Calculate the in-sample forecast from logistic regression model
coeff <- logmod$coefficients
fcast <- drop(cbind(rep(1, nrows), predv) %*% coeff)
fcast <- 1/(1+exp(-fcast))
# Calculate the error rates
error_rates <- sapply(threshv, confun,
  actual=!bottoms, fcast=fcast)  # end sapply
error_rates <- t(error_rates)
rownames(error_rates) <- threshv
# Calculate the informedness
informv <- 2 - rowSums(error_rates)
plot(threshv, informv, t="l", main="Informedness")
# Find the threshold corresponding to highest informedness
threshm <- threshv[which.max(informv)]
forecastbot <- (fcast > threshm)

# Calculate the area under ROC curve (AUC)
error_rates <- rbind(c(1, 0), error_rates)
error_rates <- rbind(error_rates, c(0, 1))
truepos <- (1 - error_rates[, "typeII"])
truepos <- (truepos + rutils::lagit(truepos))/2
falsepos <- rutils::diffit(error_rates[, "typeI"])
abs(sum(truepos*falsepos))
# Plot ROC Curve for stock tops
plot(x=error_rates[, "typeI"], y=1-error_rates[, "typeII"],
     xlab="FALSE positive rate", ylab="TRUE positive rate",
     main="ROC Curve for Stock Bottoms", type="l", lwd=3, col="blue")
abline(a=0.0, b=1.0, lwd=3, col="orange")

# Average the signals over time
topsav <- HighFreq::roll_sum(matrix(forecastops), 5)/5
botsav <- HighFreq::roll_sum(matrix(forecastbot), 5)/5
# Simulate in-sample VTI strategy
posv <- (botsav - topsav)
# Standard strategy
# posv <- rep(NA_integer_, NROW(retp))
# posv[1] <- 0
# posv[forecastops] <- (-1)
# posv[forecastbot] <- 1
# posv <- zoo::na.locf(posv)
posv <- rutils::lagit(posv)
pnls <- retp*posv

# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
sqrt(252)*sapply(wealthv,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of in-sample VTI strategy
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Logistic Top and Bottom Strategy In-Sample") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Define in-sample and out-of-sample intervals
insample <- 1:(nrows %/% 2)
outsample <- (nrows %/% 2 + 1):nrows
# Fit in-sample logistic regression for tops
logmod <- glm(tops[insample] ~ predv[insample, ], family=binomial(logit))
fittedv <- logmod$fitted.values
coefftop <- logmod$coefficients
# Calculate the error rates and best threshold value
error_rates <- sapply(threshv, confun,
  actual=!tops[insample], fcast=fittedv)  # end sapply
error_rates <- t(error_rates)
informv <- 2 - rowSums(error_rates)
threshtop <- threshv[which.max(informv)]
# Fit in-sample logistic regression for bottoms
logmod <- glm(bottoms[insample] ~ predv[insample, ], family=binomial(logit))
fittedv <- logmod$fitted.values
coeffbot <- logmod$coefficients
# Calculate the error rates and best threshold value
error_rates <- sapply(threshv, confun,
  actual=!bottoms[insample], fcast=fittedv)  # end sapply
error_rates <- t(error_rates)
informv <- 2 - rowSums(error_rates)
threshbot <- threshv[which.max(informv)]
# Calculate the out-of-sample forecasts from logistic regression model
predictout <- cbind(rep(1, NROW(outsample)), predv[outsample, ])
fcast <- drop(predictout %*% coefftop)
fcast <- 1/(1+exp(-fcast))
forecastops <- (fcast > threshtop)
fcast <- drop(predictout %*% coeffbot)
fcast <- 1/(1+exp(-fcast))
forecastbot <- (fcast > threshbot)

# Simulate in-sample VTI strategy
topsav <- HighFreq::roll_sum(matrix(forecastops), 5)/5
botsav <- HighFreq::roll_sum(matrix(forecastbot), 5)/5
posv <- (botsav - topsav)
posv <- rutils::lagit(posv)
pnls <- retp[outsample, ]*posv
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retp[outsample, ], pnls)
colnames(wealthv) <- c("VTI", "Strategy")
sqrt(252)*sapply(wealthv,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of in-sample VTI strategy
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Logistic Strategy Out-of-Sample") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Fit logistic regression over training data
set.seed(1121)  # Reset random number generator
nrows <- NROW(Default)
samplev <- sample.int(n=nrows, size=nrows/2)
trainset <- Default[samplev, ]
logmod <- glm(formulav, data=trainset, family=binomial(logit))
# Forecast over test data out-of-sample
testset <- Default[-samplev, ]
fcast <- predict(logmod, newdata=testset, type="response")
# Calculate the confusion matrix out-of-sample
table(actual=!testset$default,
forecast=(fcast < threshv))

# Define response as the multi-day returns
lagg <- 5
retsf <- rutils::diffit(closep, lagg=5)
retsf <- drop(coredata(retsf))
# Fit in-sample logistic regression for positive returns
retos <- (retsf > 0)
logmod <- glm(retspos ~ predv - 1, family=binomial(logit))
summary(logmod)
coeff <- logmod$coefficients
fcast <- drop(predv %*% coeff)
fcast <- 1/(1+exp(-fcast))
# Calculate the error rates
threshv <- quantile(fcast, seq(0.01, 0.99, by=0.01))
error_rates <- sapply(threshv, confun,
  actual=!retspos, fcast=fcast)  # end sapply
error_rates <- t(error_rates)
# Calculate the threshold corresponding to highest informedness
informv <- 2 - rowSums(error_rates)
plot(threshv, informv, t="l", main="Informedness")
threshm <- threshv[which.max(informv)]
forecastpos <- (fcast > threshm)
# Fit in-sample logistic regression for negative returns
retsneg <- (retsf < 0)
logmod <- glm(retsneg ~ predv - 1, family=binomial(logit))
summary(logmod)
coeff <- logmod$coefficients
fcast <- drop(predv %*% coeff)
fcast <- 1/(1+exp(-fcast))
# Calculate the error rates
error_rates <- sapply(threshv, confun,
  actual=!retsneg, fcast=fcast)  # end sapply
error_rates <- t(error_rates)
# Calculate the threshold corresponding to highest informedness
informv <- 2 - rowSums(error_rates)
plot(threshv, informv, t="l", main="Informedness")
threshm <- threshv[which.max(informv)]
forecastneg <- (fcast > threshm)

# Simulate in-sample VTI strategy
negav <- HighFreq::roll_sum(matrix(forecastneg), lagg)/lagg
posav <- HighFreq::roll_sum(matrix(forecastpos), lagg)/lagg
posv <- (negav - posav)
# posv <- ifelse(forecastpos, 1, 0)
# posv <- ifelse(forecastneg, -1, posv)
posv <- rutils::lagit(posv)
pnls <- retp*posv
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
sqrt(252)*sapply(wealthv,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))

# Plot dygraph of in-sample VTI strategy
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Logistic Forecasting Returns") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Define in-sample and out-of-sample intervals
insample <- 1:(nrows %/% 2)
outsample <- (nrows %/% 2 + 1):nrows
# Fit in-sample logistic regression for tops
logmod <- glm(tops[insample] ~ predv[insample, ], family=binomial(logit))
fittedv <- logmod$fitted.values
coefftop <- logmod$coefficients
# Calculate the error rates and best threshold value
error_rates <- sapply(threshv, confun,
  actual=!tops[insample], fcast=fittedv)  # end sapply
error_rates <- t(error_rates)
informv <- 2 - rowSums(error_rates)
threshtop <- threshv[which.max(informv)]
# Fit in-sample logistic regression for bottoms
logmod <- glm(bottoms[insample] ~ predv[insample, ], family=binomial(logit))
fittedv <- logmod$fitted.values
coeffbot <- logmod$coefficients
# Calculate the error rates and best threshold value
error_rates <- sapply(threshv, confun,
  actual=!bottoms[insample], fcast=fittedv)  # end sapply
error_rates <- t(error_rates)
informv <- 2 - rowSums(error_rates)
threshbot <- threshv[which.max(informv)]
# Calculate the out-of-sample forecasts from logistic regression model
predictout <- cbind(rep(1, NROW(outsample)), predv[outsample, ])
fcast <- drop(predictout %*% coefftop)
fcast <- 1/(1+exp(-fcast))
forecastops <- (fcast > threshtop)
fcast <- drop(predictout %*% coeffbot)
fcast <- 1/(1+exp(-fcast))
forecastbot <- (fcast > threshbot)

# Simulate out-of-sample VTI strategy
posv <- rep(NA_integer_, NROW(outsample))
posv[1] <- 0
posv[forecastops] <- (-1)
posv[forecastbot] <- 1
posv <- zoo::na.locf(posv)
posv <- rutils::lagit(posv)
pnls <- retp[outsample, ]*posv
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retp[outsample, ], pnls)
colnames(wealthv) <- c("VTI", "Strategy")
sqrt(252)*sapply(wealthv,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of in-sample VTI strategy
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Logistic Strategy Out-of-Sample") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

lambdav <- c(0.5, 1, 1.5)
colorv <- c("red", "blue", "green")
# Define the leverage function
leverage <- function(p, lambda) tanh(lambda*p)
# Plot three curves in loop
for (indeks in 1:3) {
  curve(expr=leverage(x, lambda=lambdav[indeks]),
xlim=c(-4, 4), type="l", lwd=4,
xlab="predictor", ylab="dollar amount",
col=colorv[indeks], add=(indeks>1))
}  # end for
# Add title
title(main="Leverage function", line=0.5)
# Add legend
legend("topleft", title="Leverage parameters",
       paste("lambda", lambdav, sep="="),
       inset=0.05, cex=0.8, lwd=6, bty="n", y.intersp=0.5,
       lty=1, col=colorv)

# Calculate the a vector of daily VTI percentage returns
retp <- na.omit(rutils::etfenv$returns$VTI)
datev <- zoo::index(retp)
retp <- as.numeric(retp)
nrows <- NROW(retp)
# Define the response and predictor matrices
respv <- retp
orderp <- 5
predv <- sapply(1:orderp, rutils::lagit, input=respv)
predv <- cbind(rep(1, nrows), predv)
colnames(predv) <- paste0("pred", 1:NCOL(predv))
predinv <- MASS::ginv(predv)
coeff <- drop(predinv %*% respv)
# Calculate the in-sample forecasts of VTI
fcast <- drop(predv %*% coeff)

# Calculate the residuals (forecast errors)
resids <- drop(fcast - retp)
# Calculate the variance of the residuals
vares <- sum(resids^2)/(nrows-NROW(coeff))
# Calculate the predictor matrix squared
predv2 <- crossprod(predv)
# Calculate the covariance matrix of the AR coefficients
covar <- vares*MASS::ginv(predv2)
coeffsd <- sqrt(diag(covar))
# Calculate the t-values of the AR coefficients
coefftv <- coeff/coeffsd
# Plot the t-values of the AR coefficients
barplot(coefftv, xlab="lag", ylab="t-value",
  main="Coefficient t-values of AR Forecasting Model")

# Define predictor matrix for forecasting
ordmax <- 5
predv <- sapply(1:ordmax, rutils::lagit, input=respv)
predv <- cbind(rep(1, nrows), predv)
colnames(predv) <- paste0("pred", 1:NCOL(predv))
# Calculate the forecasts as function of the AR order
fcast <- lapply(2:NCOL(predv), function(ordern) {
  # Calculate the fitted coefficients
  predinv <- MASS::ginv(predv[, 1:ordern])
  coeff <- drop(predinv %*% respv)
  # Calculate the in-sample forecasts of VTI
  drop(predv[, 1:ordern] %*% coeff)
})  # end lapply
names(fcast) <- paste0("n=", 2:NCOL(predv))

# Calculate the mean squared errors
mse <- sapply(fcast, function(x) {
  c(mse=mean((respv - x)^2), cor=cor(respv, x))
})  # end sapply
mse <- t(mse)
rownames(mse) <- names(fcast)
# Plot forecasting MSE
plot(x=2:NCOL(predv), y=mse[, 1],
  xlab="AR(n) order", ylab="MSE", type="l", lwd=2,
  main="MSE of In-sample AR(n) Forecasting Model for VTI")

# Define in-sample and out-of-sample intervals
insample <- 1:(nrows %/% 2)
outsample <- (nrows %/% 2 + 1):nrows
# Calculate the forecasts as function of the AR order
fcast <- lapply(2:NCOL(predv), function(ordern) {
  # Calculate the fitted coefficients
  predinv <- MASS::ginv(predv[insample, 1:ordern])
  coeff <- drop(predinv %*% respv[insample])
  # Calculate the out-of-sample forecasts of VTI
  drop(predv[outsample, 1:ordern] %*% coeff)
})  # end lapply
names(fcast) <- paste0("n=", 2:NCOL(predv))

# Calculate the mean squared errors
mse <- sapply(fcast, function(x) {
  c(mse=mean((respv[outsample] - x)^2), cor=cor(respv[outsample], x))
})  # end sapply
mse <- t(mse)
rownames(mse) <- names(fcast)
# Plot forecasting MSE
plot(x=2:NCOL(predv), y=mse[, 1],
  xlab="AR(n) order", ylab="MSE", type="l", lwd=2,
  main="MSE of Out-of-sample AR(n) Forecasting Model for VTI")

# Calculate the out-of-sample PnLs
pnls <- sapply(fcast, function(x) {
  cumsum(sign(x)*retp[outsample])
})  # end sapply
colnames(pnls) <- names(fcast)
pnls <- xts::xts(pnls, datev[outsample])

# Plot dygraph of out-of-sample PnLs
colorv <- colorRampPalette(c("red", "blue"))(NCOL(pnls))
colnamev <- colnames(pnls)
endd <- rutils::calc_endpoints(pnls, interval="weeks")
dygraphs::dygraph(pnls[endd],
  main="Autoregressive Strategies With Different Order Parameters") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(width=300)

# Define predictor as a rolling mean
nagg <- 5
predv <- HighFreq::roll_mean(matrix(retp), nagg)
# Define predictor matrix for forecasting
predv <- sapply(1+nagg*(0:ordmax), rutils::lagit, input=predv)
predv <- cbind(rep(1, nrows), predv)
# Calculate the forecasts as function of the AR order
fcast <- lapply(2:NCOL(predv), function(ordern) {
  predinv <- MASS::ginv(predv[insample, 1:ordern])
  coeff <- drop(predinv %*% respv[insample])
  drop(predv[outsample, 1:ordern] %*% coeff)
})  # end lapply
names(fcast) <- paste0("n=", 2:NCOL(predv))

# Calculate the out-of-sample PnLs
pnls <- sapply(fcast, function(x) {
  cumsum(sign(x)*retp[outsample])
})  # end sapply
colnames(pnls) <- names(fcast)
pnls <- xts::xts(pnls, datev[outsample])
# Plot dygraph of out-of-sample PnLs
colorv <- colorRampPalette(c("red", "blue"))(NCOL(pnls))
dygraphs::dygraph(pnls[endd],
  main="Autoregressive Strategies Using Rolling Average Predictor") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(width=300)

# Calculate the PnLs using the average of past forecasts
nagg <- 5
pnls <- sapply(fcast, function(x) {
  x <- HighFreq::roll_mean(matrix(x), nagg)
  cumsum(sign(x)*retp[outsample])
})  # end sapply
colnames(pnls) <- names(fcast)
pnls <- xts::xts(pnls, datev[outsample])

# Plot dygraph of out-of-sample PnLs
dygraphs::dygraph(pnls[endd],
  main="Autoregressive Strategies Using Rolling Average Forecasts") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(width=300)

library(rutils)
# Calculate the a vector of daily VTI log returns
pricev <- log(quantmod::Cl(rutils::etfenv$VTI))
retp <- rutils::diffit(pricev)
retp <- as.numeric(retp)
nrows <- NROW(retp)
# Define predictor matrix for forecasting
ordmax <- 5
desv <- sapply(1:ordmax, rutils::lagit, input=respv)
colnames(desv) <- paste0("pred", 1:NCOL(desv))
# Add response equal to VTI
desv <- cbind(retp, desv)
colnames(desv)[1] <- "response"
# Specify length of look-back interval
look_back <- 100
# Invert the predictor matrix
rangev <- (nrows-look_back):(nrows-1)
desvinv <- MASS::ginv(desv[rangev, -1])
# Calculate the fitted coefficients
coeff <- drop(desvinv %*% desv[rangev, 1])
# Calculate the forecast of VTI for nrows
drop(desv[nrows, -1] %*% coeff)
# Compare with actual value
desv[nrows, 1]

# Define predictor as a rolling mean
nagg <- 5
predv <- HighFreq::roll_mean(matrix(retp), nagg)
# Define predictor matrix for forecasting
ordmax <- 5
predv <- sapply(1+nagg*(0:ordmax), rutils::lagit, input=predv)
predv <- cbind(rep(1, nrows), predv)
# Perform rolling forecasting
look_back <- 100
fcast <- sapply((look_back+1):nrows, function(endd) {
  # Define rolling look-back range
  startp <- max(1, endd-look_back)
  # Or expanding look-back range
  # startp <- 1
  rangev <- startp:(endd-1)
  # Invert the predictor matrix
  desvinv <- MASS::ginv(predv[rangev, ])
  # Calculate the fitted coefficients
  coeff <- drop(desvinv %*% retp[rangev])
  # Calculate the forecast
  drop(predv[endd, ] %*% coeff)
})  # end sapply
# Add warmup period
fcast <- c(rep(0, look_back), fcast)

# Calculate the correlation between forecasts and returns
cor(fcast, retp)
# Calculate the forecasting errors
errorf <- (fcast - retp)
# Mean squared error
mean(errorf^2)
# Calculate correlation between forecast errors and returns
cor(errorf, retp)

# Plot forecasting series with legend
plot(retp[(nrows-5*look_back):nrows], col="blue",
     xlab="", ylab="", type="l", lwd=2,
     main="Rolling Forecasting Using AR Model")
lines(fcast[(nrows-5*look_back):nrows], col="red", lwd=2)
legend(x="topleft", legend=c("returns", "forecasts"),
 col=c("blue", "red"), lty=1, lwd=6, y.intersp=0.3,
 cex=1.0, bg="white", bty="n")

# Define backtesting function
sim_fcasts <- function(respv, nagg=5, ordern=5,
                 look_back=100, rollp=TRUE) {
  nrows <- NROW(respv)
  # Define predictor as a rolling sum
  predv <- rutils::roll_sum(respv, look_back=nagg)
  # Define predictor matrix for forecasting
  predv <- sapply(1+nagg*(0:ordern), rutils::lagit,
                 input=predv)
  predv <- cbind(rep(1, nrows), predv)
  # Perform rolling forecasting
  fcast <- sapply((look_back+1):nrows, function(endd) {
    # Define rolling look-back range
    if (rollp)
startp <- max(1, endd-look_back)
    else
    # Or expanding look-back range
    startp <- 1
    rangev <- startp:(endd-1)
    # Invert the predictor matrix
    desvinv <- MASS::ginv(predv[rangev, ])
    # Calculate the fitted coefficients
    coeff <- drop(desvinv %*% respv[rangev])
    # Calculate the forecast
    drop(predv[endd, ] %*% coeff)
  })  # end sapply
  # Add warmup period
  fcast <- c(rep(0, look_back), fcast)
  # Aggregate the forecasts
  rutils::roll_sum(fcast, look_back=nagg)
}  # end sim_fcasts
# Simulate the rolling autoregressive forecasts
fcast <- sim_fcasts(respv=retp, ordern=5, look_back=100)
c(mse=mean((fcast - retp)^2), cor=cor(retp, fcast))

library(parallel)  # Load package parallel
# Calculate the number of available cores
ncores <- detectCores() - 1
# Initialize compute cluster under Windows
cluster <- makeCluster(ncores)
# Perform parallel loop under Windows
look_backs <- seq(20, 600, 40)
fcast <- parLapply(cluster, look_backs, sim_fcasts,
  response=retp, nagg=5, ordern=5)
# Perform parallel bootstrap under Mac-OSX or Linux
fcast <- mclapply(look_backs, sim_fcasts, response=retp,
  nagg=5, ordern=5, mc.cores=ncores)

# Calculate the mean squared errors
mse <- sapply(fcast, function(x) {
  c(mse=mean((retp - x)^2), cor=cor(retp, x))
})  # end sapply
mse <- t(mse)
rownames(mse) <- look_backs
# Select optimal look_back interval
look_back <- look_backs[which.min(mse[, 1])]
# Plot forecasting MSE
plot(x=look_backs, y=mse[, 1],
  xlab="look-back", ylab="MSE", type="l", lwd=2,
  main="MSE of AR Forecasting Model As Function of Look-back")

library(parallel)  # Load package parallel
# Calculate the number of available cores
ncores <- detectCores() - 1
# Initialize compute cluster under Windows
cluster <- makeCluster(ncores)
# Perform parallel loop under Windows
orderv <- 2:6
fcast <- parLapply(cluster, orderv, sim_fcasts, response=retp,
  nagg=5, look_back=look_back)
stopCluster(cluster)  # Stop R processes over cluster under Windows
# Perform parallel bootstrap under Mac-OSX or Linux
fcast <- mclapply(orderv, sim_fcasts, response=retp,
  nagg=5, look_back=look_back, mc.cores=ncores)

# Calculate the mean squared errors
mse <- sapply(fcast, function(x) {
  c(mse=mean((retp - x)^2), cor=cor(retp, x))
})  # end sapply
mse <- t(mse)
rownames(mse) <- orderv
# Select optimal order parameter
ordern <- orderv[which.min(mse[, 1])]
# Plot forecasting MSE
plot(x=orderv, y=mse[, 1],
  xlab="AR order", ylab="MSE", type="l", lwd=2,
  main="MSE of Forecasting Model As Function of AR Order")

# Simulate the rolling autoregressive forecasts
fcast <- sim_fcasts(retp, ordern=ordern, look_back=look_back)
# Calculate the strategy PnLs
pnls <- sign(fcast)*retp
pnls <- cbind(retp, pnls, (retp+pnls)/2)
colnames(pnls) <- c("VTI", "AR_Strategy", "Combined")
cor(pnls)
# Annualized Sharpe ratios of VTI and AR strategy
pnls <- xts::xts(pnls, datev)
sqrt(252)*sapply(pnls, function (x) mean(x)/sd(x))

# Plot the cumulative strategy PnLs
endd <- rutils::calc_endpoints(pnls, interval="weeks")
dygraphs::dygraph(cumsum(pnls)[endd], main="Rolling Autoregressive Strategy") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Dygraphs plot with custom line colors
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd], main="Dual Crossover Strategy Out-of-Sample") %>%
  dyEvent(zoo::index(wealthv[last(insample)]), label="in-sample", strokePattern="solid", color="green") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2)

# Calculate the PnLs for ordern=5
fcast <- sim_fcasts(retp, ordern=ordern, look_back=look_back)
pnls5 <- cumsum(sign(fcast)*retp)
# Calculate the PnLs for ordern=3
fcast <- sim_fcasts(retp, ordern=ordern, look_back=look_back)
pnls3 <- cumsum(sign(fcast)*retp)

# Plot the cumulative strategy returns
wealthv <- cbind(pnls5, pnls3)
wealthv <- xts::xts(wealthv, datev)
colnamev <- c("AR(5)_Strategy", "AR(3)_Strategy")
colnames(wealthv) <- colnamev
dygraphs::dygraph(wealthv, main="Autoregressive Strategies for Different Order Parameters") %>%
  dySeries(name=colnamev[1], col="blue", strokeWidth=2) %>%
  dySeries(name=colnamev[2], col="red", strokeWidth=2) %>%
  dyLegend(width=300)

# Calculate the PnLs for rolling look-back
fcast <- sim_fcasts(retp, ordern=ordern, look_back=look_back,
                     rollp=TRUE)
pnls_roll <- cumsum(sign(fcast)*retp)
# Calculate the PnLs for expanding look-back
fcast <- sim_fcasts(retp, ordern=ordern, look_back=look_back,
                     rollp=FALSE)
pnls_expand <- cumsum(sign(fcast)*retp)

# Plot the cumulative strategy returns
wealthv <- cbind(pnls_roll, pnls_expand)
wealthv <- xts::xts(wealthv, datev)
colnamev <- c("Rolling", "Expanding")
colnames(wealthv) <- colnamev
dygraphs::dygraph(wealthv[endd], main="Autoregressive Strategies for Expanding Look-back Interval") %>%
  dySeries(name=colnamev[1], col="blue", strokeWidth=2) %>%
  dySeries(name=colnamev[2], col="red", strokeWidth=2) %>%
  dyLegend(width=300)

# Cap the VTI returns
cutoff <- 0.03
capped <- ifelse(retp > cutoff, cutoff, retp)
capped <- ifelse(capped < (-cutoff), -cutoff, capped)
# Calculate the PnLs for VTI
fcast <- sim_fcasts(retp, ordern=3, look_back=look_back, rollp=FALSE)
pnls <- cumsum(sign(fcast)*retp)
# Calculate the PnLs for capped VTI returns
fcast <- sim_fcasts(capped, ordern=3, look_back=look_back, rollp=FALSE)
pnls_capped <- cumsum(sign(fcast)*retp)

# Plot the cumulative strategy returns
wealthv <- cbind(pnls, pnls_capped)
wealthv <- xts::xts(wealthv, datev)
colnamev <- c("AR(3)_Rolling", "AR(3)_Expanding")
colnamev <- c("AR_Strategy", "AR_Strategy_Capped")
colnames(wealthv) <- colnamev
dygraphs::dygraph(wealthv[endd], main="Improved Autoregressive Strategies") %>%
  dySeries(name=colnamev[1], col="blue", strokeWidth=2) %>%
  dySeries(name=colnamev[2], col="red", strokeWidth=2) %>%
  dyLegend(width=300)
