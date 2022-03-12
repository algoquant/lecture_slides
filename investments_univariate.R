library(xtable)
gambl_e <- data.frame(win=c("p", "a", "1 + a"), lose=c("q = 1 - p", "-b", "1 - b"))
rownames(gambl_e) <- c("probability", "payout", "terminal wealth")
# print(xtable(gambl_e), comment=FALSE, size="tiny")
print(xtable(gambl_e), comment=FALSE)
# Open x11 for plotting
x11(width=5, height=4)
# Set plot parameters to reduce whitespace around plot
par(mar=c(4, 4, 2, 1), oma=c(0, 0, 0, 0))
# Define logarithmic utility
utili_ty <- function(frac, p=0.3, a=20, b=1) {
  p*log(1+frac*a) + (1-p)*log(1-frac*b)
}  # end utili_ty
# Plot utility
curve(expr=utili_ty, xlim=c(0, 1),
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
wealth <- function(f, a=0.8, b=0.1, n=1e3, i=150) {
  (1+f*a)^i * (1-f*b)^(n-i)
}  # end wealth
curve(expr=wealth, xlim=c(0, 1),
xlab="betting fraction",
ylab="wealth", main="", lwd=2)
title(main="Wealth of Multiperiod Betting", line=0.1)
# Open x11 for plotting
x11(width=5, height=4)
# Set plot parameters to reduce whitespace around plot
par(mar=c(4, 4, 2, 1), oma=c(0, 0, 0, 0))
set.seed(1121)  # Reset random number generator
# Simulate asset prices
calc_prices <- function(x) cumprod(1 + rnorm(1e3, sd=0.01))
price_paths <- sapply(1:3, calc_prices)
plot(price_paths[, 1], type="l", lwd=3,
     main="Simulated Asset Prices",
     ylim=range(price_paths),
     lty="solid", xlab="time", ylab="price")
lines(price_paths[, 2], col="blue", lwd=3)
lines(price_paths[, 3], col="orange", lwd=3)
abline(h=0.5, col="red", lwd=3)
text(x=200, y=0.5, pos=3, labels="liquidation threshold")
library(rutils)
# Calculate the VTI returns
vtis <- rutils::etfenv$returns$VTI
vtis <- na.omit(vtis)
c(mean=mean(vtis), std=sd(vtis))
range(vtis)
# Open x11 for plotting
x11(width=5, height=4)
# Set plot parameters to reduce whitespace around plot
par(mar=c(4, 4, 2, 1), oma=c(0, 0, 0, 0))
# Define vectorized logarithmic utility function
utili_ty <- function(kell_y, returns) {
  sapply(kell_y, function(x)
    sum(log(1 + x*returns)))
}  # end utili_ty
utili_ty(1, vtis)
utili_ty(c(1, 4), vtis)
# Plot the logarithmic utility
curve(expr=utili_ty(x, returns=vtis),
xlim=c(0.1, 5), xlab="leverage", ylab="utility",
main="Utility of Asset Returns", lwd=2)
# Approximate Kelly leverage
mean(vtis)/var(vtis)
PerformanceAnalytics::KellyRatio(R=vtis, method="full")
# Kelly leverage
unlist(optimize(
  f=function(x) -utili_ty(x, vtis),
  interval=c(1, 4)))
# Calculate the VTI returns
vtis <- rutils::etfenv$returns$VTI
vtis <- na.omit(vtis)
# Calculate wealth paths
kelly_ratio <- drop(mean(vtis)/var(vtis))
kelly_wealth <- cumprod(1 + kelly_ratio*vtis)
hyper_kelly <- cumprod(1 + (kelly_ratio+2)*vtis)
sub_kelly <- cumprod(1 + (kelly_ratio-2)*vtis)
kelly_paths <- cbind(kelly_wealth, hyper_kelly, sub_kelly)
colnames(kelly_paths) <- c("kelly", "hyper-kelly", "sub-kelly")
# Plot wealth paths
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("black", "orange", "blue")
quantmod::chart_Series(kelly_paths, theme=plot_theme, name="Wealth Paths")
legend("topleft", legend=colnames(kelly_paths),
 inset=0.1, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")
# bid_offer equal to 10 bps for liquid ETFs
bid_offer <- 0.001
# Calculate wealth paths
kelly_ratio <- drop(mean(vtis)/var(vtis))
wealth <- cumprod(1 + kelly_ratio*vtis)
wealth_trans <- cumprod(1 + kelly_ratio*vtis -
  0.5*bid_offer*kelly_ratio*(kelly_ratio-1)*abs(vtis))
# Calculate compounded wealth from returns
wealth <- cbind(wealth, wealth_trans)
colnames(wealth) <- c("Kelly", "Including bid-offer")
# Plot compounded wealth
dygraphs::dygraph(wealth, main="Kelly Strategy With Transaction Costs") %>%
  dyOptions(colors=c("green", "blue"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
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
 title="Risk Aversion", inset=0.05, cex=0.8, bg="white",
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
 title="Risk Aversion", inset=0.05, cex=0.8, bg="white",
 lwd=6, lty=1, bty="n", col=c("blue", "black", "red"))
# Calculate the VTI returns
vtis <- rutils::etfenv$returns$VTI
vtis <- na.omit(vtis)
# Calculate wealth paths
kelly_ratio <- drop(mean(vtis)/var(vtis))
kelly_wealth <- cumprod(1 + kelly_ratio*vtis)
hyper_kelly <- cumprod(1 + (kelly_ratio+2)*vtis)
sub_kelly <- cumprod(1 + (kelly_ratio-2)*vtis)
kelly_paths <- cbind(kelly_wealth, hyper_kelly, sub_kelly)
colnames(kelly_paths) <- c("kelly", "hyper-kelly", "sub-kelly")
# Plot wealth paths
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("black", "orange", "blue")
quantmod::chart_Series(kelly_paths, theme=plot_theme,
       name="Wealth Paths")
legend("topleft", legend=colnames(kelly_paths),
 inset=0.1, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")
# Calculate the VTI returns
vtis <- rutils::etfenv$vtis$VTI
vtis <- na.omit(vtis)
# Calculate higher moments of VTI returns
c(mean=sum(vtis),
  variance=sum(vtis^2),
  mom3=sum(vtis^3),
  mom4=sum(vtis^4))/NROW(vtis)
# Calculate higher moments of minutely SPY returns
sp_y <- HighFreq::SPY[, 4]
sp_y <- na.omit(sp_y)
sp_y <- HighFreq::diffit(log(sp_y))
c(mean=sum(sp_y),
  variance=sum(sp_y^2),
  mom3=sum(sp_y^3),
  mom4=sum(sp_y^4))/NROW(sp_y)
returns <- na.omit(rutils::etfenv$returns[, c("VTI", "IEF")])
# Logarithmic utility of stock and bond portfolio
utili_ty <- function(w_s, w_b) {
  -sum(log(1 + w_s*returns$VTI + w_b*returns$IEF))
}  # end utili_ty
# Create matrix of utility values
w_s <- seq(from=3, to=7, by=0.2)
w_b <- seq(from=12, to=20, by=0.2)
utility_mat <- sapply(w_b, function(y) sapply(w_s,
  function(x) utili_ty(x, y)))
# Set rgl options and load package rgl
options(rgl.useNULL=TRUE)
library(rgl)
# Draw 3d surface plot of utility
rgl::persp3d(w_s, w_b, utility_mat, col="green",
  xlab="stocks", ylab="bonds", zlab="utility")
# Render the surface plot
rgl::rglwidget(elementId="plot3drgl")
# Save the surface plot to png file
rgl::rgl.snapshot("utility_surface.png")
# Approximate Kelly weights
weightv <- sapply(returns, function(x) mean(x)/var(x))
# Kelly weight for stocks
unlist(optimize(f=function(x) utili_ty(x, w_b=0), interval=c(1, 4)))
# Kelly weight for bonds
unlist(optimize(f=function(x) utili_ty(x, w_s=0), interval=c(1, 14)))
# Vectorized utility of stock and bond portfolio
utility_vec <- function(weightv) {
  utili_ty(weightv[1], weightv[2])
}  # end utility_vec
# Optimize with respect to vector argument
optimd <- optim(fn=utility_vec, par=c(3, 10),
          method="L-BFGS-B",
          upper=c(8, 20), lower=c(2, 5))
# Exact Kelly weights
optimd$par
# Approximate Kelly weights
p_rets <- (returns %*% weightv)
drop(mean(p_rets)/var(p_rets))*weightv
# Exact Kelly weights
optimd$par
# Quarter-Kelly sub-optimal weights
weightv <- optimd$par/4
# Plot Kelly optimal portfolio
returns <- cbind(returns,
  weightv[1]*returns$VTI + weightv[2]*returns$IEF)
colnames(returns)[3] <- "Kelly_sub_optimal"
# Calculate compounded wealth from returns
wealth <- cumprod(1 + returns)
# Plot compounded wealth
dygraphs::dygraph(wealth, main="Stock and Bond Portfolio") %>%
  dyOptions(colors=c("green", "blue", "green")) %>%
  dySeries("Kelly_sub_optimal", color="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
returns <- na.omit(rutils::etfenv$returns[, c("VTI", "IEF")])
# Calculate rolling returns and variance
look_back <- 200
var_rolling <- roll::roll_var(returns, width=look_back)
weightv <- roll::roll_sum(returns, width=look_back)/look_back
weightv <- weightv/var_rolling
weightv[1, ] <- 1/NCOL(weightv)
weightv <- zoo::na.locf(weightv)
sum(is.na(weightv))
range(weightv)
# Plot the weightv
x11(width=6, height=5)
par(mar=c(4, 4, 3, 1), oma=c(0, 0, 0, 0))
plot(density(returns$IEF), t="l", lwd=3, col="red",
     xlab="weights", ylab="density",
     ylim=c(0, max(density(returns$VTI)$y)),
     main="Kelly Weight Distributions")
lines(density(returns$VTI), t="l", col="blue", lwd=3)
legend("topright", legend=c("VTI", "IEF"),
 inset=0.1, bg="white", lty=1, lwd=6,
 col=c("blue", "red"), bty="n")
# Scale and lag the Kelly weights
weightv <- lapply(weightv,
  function(x) 10*x/sum(abs(range(x))))
weightv <- do.call(cbind, weightv)
weightv <- rutils::lagit(weightv)
# Calculate the compounded Kelly wealth and VTI
wealth <- cbind(cumprod(1 + weightv$VTI*returns$VTI),
           cumprod(1 + returns$VTI))
colnames(wealth) <- c("Kelly Strategy", "VTI")
dygraphs::dygraph(wealth, main="VTI Strategy Using Rolling Kelly Weight") %>%
  dyAxis("y", label="Kelly Strategy", independentTicks=TRUE) %>%
  dyAxis("y2", label="VTI", independentTicks=TRUE) %>%
  dySeries(name="Kelly Strategy", axis="y", label="Kelly Strategy", strokeWidth=1, col="red") %>%
  dySeries(name="VTI", axis="y2", label="VTI", strokeWidth=1, col="blue")
# bid_offer equal to 10 bps for liquid ETFs
bid_offer <- 0.001
# Calculate the compounded Kelly wealth and margin
wealth <- cumprod(1 + weightv$VTI*returns$VTI)
mar_gin <- (returns$VTI - 1)*wealth + 1
# Calculate the transaction costs
costs <- bid_offer*drop(rutils::diffit(mar_gin))/2
wealth_diff <- drop(rutils::diffit(wealth))
costs_rel <- ifelse(wealth_diff>0, costs/wealth_diff, 0)
range(costs_rel)
hist(costs_rel, breaks=10000, xlim=c(-0.02, 0.02))
# Scale and lag the transaction costs
costs <- rutils::lagit(abs(costs)/wealth)
# Recalculate the compounded Kelly wealth
wealth_trans <- cumprod(1 + returns$VTI*returns$VTI - costs)
# Plot compounded wealth
wealth <- cbind(wealth, wealth_trans)
colnames(wealth) <- c("Kelly", "Including bid-offer")
dygraphs::dygraph(wealth, main="Kelly Strategy With Transaction Costs") %>%
  dyOptions(colors=c("green", "blue"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Calculate compounded wealth from returns
wealth <- cumprod(1 + rowSums(weightv*returns))
wealth <- xts::xts(wealth, index(returns))
quantmod::chart_Series(wealth, name="Rolling Kelly Strategy For VTI and IEF")
# Calculate the compounded Kelly wealth and VTI
wealth <- cbind(wealth,
  cumprod(1 + 0.6*returns$IEF + 0.4*returns$VTI))
colnames(wealth) <- c("Kelly Strategy", "VTI plus IEF")
dygraphs::dygraph(wealth, main="Rolling Kelly Strategy For VTI and IEF") %>%
  dyAxis("y", label="Kelly Strategy", independentTicks=TRUE) %>%
  dyAxis("y2", label="VTI plus IEF", independentTicks=TRUE) %>%
  dySeries(name="Kelly Strategy", axis="y", label="Kelly Strategy", strokeWidth=1, col="red") %>%
  dySeries(name="VTI plus IEF", axis="y2", label="VTI plus IEF", strokeWidth=1, col="blue")
# Test if IEF can time VTI
returns <- na.omit(rutils::etfenv$returns[, c("IEF", "VTI")])
vtis <- returns$VTI
design <- cbind(returns, 0.5*(vtis+abs(vtis)), vtis^2)
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
fit_ted <- (model$coeff["(Intercept)"] +
        model$coeff["treynor"]*vtis^2)
points.default(x=design$VTI, y=fit_ted, pch=16, col="red")
text(x=0.05, y=0.8*max(residuals), paste("Treynor test t-value =", round(summary(model)$coeff["treynor", "t value"], 2)))
library(xtable)
gambl_e <- data.frame(win=c("p", "a"), lose=c("q = 1 - p", "-b"))
rownames(gambl_e) <- c("probability", "payout")
# print(xtable(gambl_e), comment=FALSE, size="tiny")
print(xtable(gambl_e), comment=FALSE)
library(rutils)
# Extract ETF prices from rutils::etfenv$prices
prices <- rutils::etfenv$prices
prices <- zoo::na.locf(prices, na.rm=FALSE)
prices <- zoo::na.locf(prices, fromLast=TRUE)
# Calculate simple dollar returns
rets_dollar <- rutils::diffit(prices)
# Or
# rets_dollar <- lapply(prices, rutils::diffit)
# rets_dollar <- rutils::do_call(cbind, rets_dollar)
# Calculate log returns
rets_log <- rutils::diffit(log(prices))
# Calculate percentage returns
rets_percent <- rets_dollar/
  rutils::lagit(prices, lagg=1, pad_zeros=FALSE)
# Calculate prices from simple dollar returns
rets_dollar[1, ] <- prices[1, ]
new_prices <- cumsum(rets_dollar)
all.equal(new_prices, prices)
# Compound the percentage returns
new_prices <- cumprod(1+ rets_percent)
new_prices <- lapply(1:NCOL(new_prices), function (i)
    init_prices[i]*new_prices[, i])
new_prices <- rutils::do_call(cbind, new_prices)
all.equal(new_prices, prices)
# Sum the percentage returns
new_prices <- cumsum(rets_percent)
methods(cumsum)
new_prices <- lapply(1:NCOL(new_prices), function (i)
    new_prices[, i] + log(init_prices[i]))
new_prices <- rutils::do_call(cbind, new_prices)
# Only approximately equal
all.equal(new_prices, log(prices))
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
f_rate <- 0.01/252
# Margin account
mar_gin <- cumsum(returns)
# Cumulative funding costs
f_costs <- cumsum(f_rate*mar_gin)
# Add funding costs to margin account
mar_gin <- (mar_gin + f_costs)
# dygraph plot of margin and funding costs
datav <- cbind(mar_gin, f_costs)
colnames <- c("Margin", "Cumulative Funding")
colnames(datav) <- colnames
dygraphs::dygraph(datav, main="VTI Margin Funding Costs") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", col="blue") %>%
  dySeries(name=colnames[2], axis="y2", col="red", strokeWidth=3) %>%
  dyLegend(show="always", width=500)
# bid_offer equal to 10 bps for liquid ETFs
bid_offer <- 0.001
# Cumulative transaction costs
costs <- bid_offer*cumsum(abs(returns))/2
# Subtract transaction costs from margin account
mar_gin <- cumsum(returns)
mar_gin <- (mar_gin - costs)
# dygraph plot of margin and transaction costs
datav <- cbind(mar_gin, costs)
colnames <- c("Margin", "Cumulative Transaction Costs")
colnames(datav) <- colnames
dygraphs::dygraph(datav, main="VTI Transaction Costs") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", col="blue") %>%
  dySeries(name=colnames[2], axis="y2", col="red", strokeWidth=3) %>%
  dyLegend(show="always", width=500)
# Calculate VTI and IEF dollar returns
prices <- rutils::etfenv$prices[, c("VTI", "IEF")]
prices <- na.omit(prices)
dates <- index(prices)
rets_dollar <- rutils::diffit(prices)
# Calculate VTI and IEF percentage returns
rets_percent <- rets_dollar/
  rutils::lagit(prices, lagg=1, pad_zeros=FALSE)
# Wealth of fixed shares (without rebalancing)
weightv <- c(0.5, 0.5)
rets_dollar[1, ] <- prices[1, ]
wealth_fsa <- cumsum(rets_dollar %*% weightv)
# Wealth of fixed dollars (with rebalancing)
wealth_fda <- cumsum(rets_percent %*% weightv)
# Plot log wealth
wealth <- cbind(wealth_fda, log(wealth_fsa))
wealth <- xts::xts(wealth, index(prices))
colnames(wealth) <- c("Fixed dollars", "Fixed shares (log)")
colnames <- colnames(wealth)
dygraphs::dygraph(wealth, main="Wealth of Weighted Portfolios") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", col="red", strokeWidth=2) %>%
  dySeries(name=colnames[2], axis="y2", col="blue", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Margin account for fixed dollars (with rebalancing)
mar_gin <- cumsum(rets_percent %*% weightv)
# Cumulative transaction costs
costs <- bid_offer*cumsum(abs(rets_percent) %*% weightv)/2
# Subtract transaction costs from margin account
mar_gin <- (mar_gin - costs)
# dygraph plot of margin and transaction costs
datav <- cbind(mar_gin, costs)
datav <- xts::xts(datav, index(prices))
colnames <- c("Margin", "Cumulative Transaction Costs")
colnames(datav) <- colnames
dygraphs::dygraph(datav, main="Fixed Dollar Portfolio Transaction Costs") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", col="blue") %>%
  dySeries(name=colnames[2], axis="y2", col="red", strokeWidth=3) %>%
  dyLegend(show="always", width=500)
# Wealth of fixed shares (without rebalancing)
wealth_fsa <- cumsum(rets_dollar %*% weightv)
# Calculate weighted percentage returns
rets_weighted <- rets_percent %*% weightv
# Wealth of fixed ratio of dollar amounts (with rebalancing)
wealth_cda <- cumprod(1 + rets_weighted)
wealth_cda <- wealth_fsa[1]*wealth_cda
# Plot log wealth
wealth <- log(cbind(wealth_fsa, wealth_cda))
wealth <- xts::xts(wealth, index(prices))
colnames(wealth) <- c("Fixed Shares", "Fixed Ratio")
dygraphs::dygraph(wealth, main="Log Wealth of Fixed Dollar Ratios") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Returns in excess of weighted returns
excess <- lapply(rets_percent, function(x) (rets_weighted - x))
excess <- do.call(cbind, excess)
sum(excess %*% weightv)
# Calculate weighted sum of absolute excess returns
excess <- abs(excess) %*% weightv
# Total dollar amount of stocks that need to be traded
excess <- excess*rutils::lagit(wealth_cda)
# Cumulative transaction costs
costs <- bid_offer*cumsum(excess)/2
# Subtract transaction costs from wealth
wealth_cda <- (wealth_cda - costs)
# dygraph plot of wealth and transaction costs
wealth <- cbind(wealth_cda, costs)
wealth <- xts::xts(wealth, index(prices))
colnames <- c("Wealth", "Cumulative Transaction Costs")
colnames(wealth) <- colnames
dygraphs::dygraph(wealth, main="Transaction Costs With Fixed Dollar Ratios") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", col="blue") %>%
  dySeries(name=colnames[2], axis="y2", col="red", strokeWidth=3) %>%
  dyLegend(show="always", width=500)
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
# Calculate stock and bond returns
returns <- na.omit(rutils::etfenv$returns[, c("VTI", "IEF")])
weightv <- c(0.4, 0.6)
returns <- cbind(returns, returns %*% weightv)
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
# Wealth of fixed ratio of dollar amounts
wealth <- cumprod(1 + returns)
# Plot cumulative wealth
dygraphs::dygraph(log(wealth), main="Stock and Bond Portfolio") %>%
  dyOptions(colors=c("blue", "green", "blue", "red")) %>%
  dySeries("Combined", color="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
library(rutils)  # Load package rutils
# Calculate ETF returns
returns <- na.omit(rutils::etfenv$returns[, c("IEF", "VTI")])
returns <- cbind(returns, 0.6*returns$IEF+0.4*returns$VTI)
colnames(returns)[3] <- "combined"
# Calculate correlations
cor(returns)
# Calculate Sharpe ratios
sqrt(252)*sapply(returns, function(x) mean(x)/sd(x))
# Calculate skewness and kurtosis
sapply(returns, sd)
# Calculate skewness and kurtosis
t(sapply(c(skew=3, kurt=4), function(x)
  moments::moment(returns, order=x, central=TRUE)))
# Calculate prices from returns
prices <- lapply(returns, function(x) exp(cumsum(x)))
prices <- do.call(cbind, prices)
# Plot prices
dygraphs::dygraph(prices, main="Stock and Bond Portfolio") %>%
  dyOptions(colors=c("green", "blue", "green")) %>%
  dySeries("combined", color="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Extract ETF returns
symbols <- c("VTI", "IEF", "DBC")
returns <- na.omit(rutils::etfenv$returns[, symbols])
# Calculate all-weather portfolio wealth
weightsaw <- c(0.30, 0.55, 0.15)
returns <- cbind(returns, returns %*% weightsaw)
colnames(returns)[4] <- "All Weather"
# Calculate cumulative wealth from returns
wealth <- cumsum(returns)
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
# Calculate standardized simple dollar returns
rets_dollar_std <- lapply(rets_dollar, function(x) x/sd(x))
rets_dollar_std <- do.call(cbind, rets_dollar_std)
sapply(rets_dollar_std, sd)
# Wealth of fixed number of shares (without rebalancing)
weightv <- c(0.5, 0.5)
wealth_fsa <- cumsum(rets_dollar %*% weightv)
# Calculate standardized percentage returns
rets_percent_std <- lapply(rets_percent, function(x) x/sd(x))
rets_percent_std <- do.call(cbind, rets_percent_std)
sapply(rets_percent_std, sd)
# Wealth of fixed dollar amount of shares (with rebalancing)
wealth_fda <- cumsum(rets_percent_std %*% weightv)
# Plot log wealth
wealth <- cbind(wealth_fda, log(wealth_fsa))
# wealth <- xts::xts(wealth, index(prices))
colnames(wealth) <- c("With rebalancing", "Without rebalancing")
dygraphs::dygraph(wealth, main="Wealth of Equal Dollar Amount of Shares") %>%
  dyOptions(colors=c("green", "blue"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
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
vtis <- 100*cumprod(1+returns)
datav <- xts::xts(cbind(stock_value, bond_value, portf_value, vtis), dates)
colnames(datav) <- c("stocks", "bonds", "CPPI", "VTI")
dygraphs::dygraph(datav, main="CPPI strategy") %>%
  dyOptions(colors=c("red", "green", "blue", "orange"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Calculate dollar and percentage returns for VTI and IEF.
prices <- rutils::etfenv$prices[, c("VTI", "IEF")]
prices <- na.omit(prices)
rets_dollar <- rutils::diffit(prices)
rets_percent <- rets_dollar/rutils::lagit(prices, lagg=1, pad_zeros=FALSE)
# Calculate wealth of fixed ratio of dollar amounts.
weightv <- c(0.5, 0.5)
rets_weighted <- rets_percent %*% weightv
wealth_cda <- cumprod(1 + rets_weighted)
# Calculate rolling percentage volatility.
look_back <- 21
vo_l <- roll::roll_sd(rets_percent, width=look_back)
vo_l <- zoo::na.locf(vo_l, na.rm=FALSE)
vo_l <- zoo::na.locf(vo_l, fromLast=TRUE)
# Calculate the risk parity portfolio allocations.
allocation_s <- lapply(1:NCOL(prices),
  function(x) weightv[x]/vo_l[, x])
allocation_s <- do.call(cbind, allocation_s)
# Scale allocations to 1 dollar total.
allocation_s <- allocation_s/rowSums(allocation_s)
# Lag the allocations
allocation_s <- rutils::lagit(allocation_s)
# Calculate wealth of risk parity.
rets_weighted <- rowSums(rets_percent*allocation_s)
wealth_risk_parity <- cumprod(1 + rets_weighted)
# Calculate the log wealths.
wealth <- log(cbind(wealth_cda, wealth_risk_parity))
wealth <- xts::xts(wealth, index(prices))
colnames(wealth) <- c("Fixed Ratio", "Risk Parity")
# Calculate the Sharpe ratios.
sqrt(252)*sapply(rutils::diffit(wealth), function (x) mean(x)/sd(x))
# Plot a dygraph of the log wealths.
dygraphs::dygraph(wealth, main="Log Wealth of Risk Parity vs Fixed Dollar Ratios") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Test risk parity market timing of VTI using Treynor-Mazuy test
returns <- rutils::diffit(wealth)
vtis <- rets_percent$VTI
design <- cbind(returns, vtis, vtis^2)
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
fit_ted <- (model$coeff["(Intercept)"] + model$coeff["treynor"]*vtis^2)
points.default(x=design$VTI, y=fit_ted, pch=16, col="red")
text(x=0.05, y=0.8*max(residuals), paste("Risk Parity t-value =", round(summary(model)$coeff["treynor", "t value"], 2)))
# Test for fixed ratio market timing of VTI using Treynor-Mazuy test
model <- lm(fixed ~ VTI + treynor, data=design)
summary(model)
# Plot fitted (predicted) response values
fit_ted <- (model$coeff["(Intercept)"] + model$coeff["treynor"]*vtis^2)
points.default(x=design$VTI, y=fit_ted, pch=16, col="blue")
text(x=0.05, y=0.8*max(residuals), paste("Fixed Ratio t-value =", round(summary(model)$coeff["treynor", "t value"], 2)))
# Returns in excess of weighted returns
excess <- lapply(rets_percent, function(x) (rets_weighted - x))
excess <- do.call(cbind, excess)
sum(excess %*% weightv)
# Calculate weighted sum of absolute excess returns
excess <- abs(excess) %*% weightv
# Total dollar amount of stocks that need to be traded
excess <- excess*rutils::lagit(wealth_cda)
# Cumulative transaction costs
costs <- bid_offer*cumsum(excess)/2
# Subtract transaction costs from wealth
wealth_cda <- (wealth_cda - costs)
# dygraph plot of wealth and transaction costs
wealth <- cbind(wealth_cda, costs)
wealth <- xts::xts(wealth, index(prices))
colnames <- c("Wealth", "Cumulative Transaction Costs")
colnames(wealth) <- colnames
dygraphs::dygraph(wealth, main="Transaction Costs With Fixed Dollar Ratios") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", col="blue") %>%
  dySeries(name=colnames[2], axis="y2", col="red", strokeWidth=3) %>%
  dyLegend(show="always", width=500)
# Calculate positions
vtis <- na.omit(rutils::etfenv$returns$VTI)
po_s <- rep(NA_integer_, NROW(vtis))
dates <- index(vtis)
dates <- format(dates, "%m-%d")
po_s[dates == "05-01"] <- 0
po_s[dates == "05-03"] <- 0
po_s[dates == "11-01"] <- 1
po_s[dates == "11-03"] <- 1
# Carry forward and backward non-NA po_s
po_s <- zoo::na.locf(po_s, na.rm=FALSE)
po_s <- zoo::na.locf(po_s, fromLast=TRUE)
# Calculate strategy returns
sell_inmay <- po_s*vtis
wealth <- cbind(vtis, sell_inmay)
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
design <- cbind(vtis, 0.5*(vtis+abs(vtis)), vtis^2)
colnames(design) <- c("VTI", "merton", "treynor")
# Perform Merton-Henriksson test
model <- lm(sell_inmay ~ VTI + merton, data=design)
summary(model)
# Perform Treynor-Mazuy test
model <- lm(sell_inmay ~ VTI + treynor, data=design)
summary(model)
# Plot Treynor-Mazuy residual scatterplot
residuals <- (sell_inmay - model$coeff[2]*vtis)
plot.default(x=vtis, y=residuals, xlab="VTI", ylab="residuals")
title(main="Treynor-Mazuy Market Timing Test\n for Sell in May vs VTI", line=0.5)
# Plot fitted (predicted) response values
fit_ted <- (model$coeff["(Intercept)"] +
        model$coeff["treynor"]*vtis^2)
points.default(x=vtis, y=fit_ted, pch=16, col="red")
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
vtis <- na.omit(rutils::etfenv$returns$VTI)
dates <- zoo::index(vtis)
# Calculate first business day of every month
day_s <- as.numeric(format(dates, "%d"))
indeks <- which(rutils::diffit(day_s) < 0)
dates[head(indeks)]
# Calculate Turn of the Month dates
indeks <- lapply((-1):2, function(x) indeks + x)
indeks <- do.call(c, indeks)
sum(indeks > NROW(dates))
indeks <- sort(indeks)
dates[head(indeks, 11)]
# Calculate Turn of the Month pnls
pnls <- numeric(NROW(vtis))
pnls[indeks] <- vtis[indeks, ]
# Combine data
wealth <- cbind(vtis, pnls)
colnames <- c("VTI", "Strategy")
colnames(wealth) <- colnames
# Calculate Sharpe and Sortino ratios
sqrt(252)*sapply(wealth,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# dygraph plot VTI Turn of the Month strategy
dygraphs::dygraph(cumsum(wealth), main="Turn of the Month Strategy") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=colnames[2], axis="y2", strokeWidth=2, col="red")
# Calculate the VTI returns
vtis <- na.omit(rutils::etfenv$returns$VTI)
dates <- zoo::index(vtis)
vtis <- drop(coredata(vtis))
nrows <- NROW(vtis)
# Simulate stop-loss strategy
sto_p <- 0.05
ma_x <- 0.0
cum_ret <- 0.0
pnls <- vtis
for (i in 1:nrows) {
# Calculate drawdown
  cum_ret <- cum_ret + vtis[i]
  ma_x <- max(ma_x, cum_ret)
  dd <- (cum_ret - ma_x)
# Check for stop-loss
  if (dd < -sto_p*ma_x)
    pnls[i+1] <- 0
}  # end for
# Same but without using explicit loops
cumsumv <- cumsum(vtis)
cum_max <- cummax(cumsum(vtis))
dd <- (cumsumv - cum_max)
pnls2 <- vtis
is_dd <- rutils::lagit(dd < -sto_p*cum_max)
pnls2 <- ifelse(is_dd, 0, pnls2)
all.equal(pnls, pnls2)
# Combine data
wealth <- xts::xts(cbind(vtis, pnls), dates)
colnames <- c("VTI", "Strategy")
colnames(wealth) <- colnames
# Calculate Sharpe and Sortino ratios
sqrt(252)*sapply(wealth,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# dygraph plot VTI stop-loss strategy
dygraphs::dygraph(cumsum(wealth), main="VTI Stop-loss Strategy") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=colnames[2], axis="y2", strokeWidth=2, col="red")
# Simulate multiple stop-loss strategies
cumsumv <- cumsum(vtis)
cum_max <- cummax(cumsum(vtis))
dd <- (cumsumv - cum_max)
cum_pnls <- sapply(0.01*(1:20), function(sto_p) {
  pnls <- vtis
  is_dd <- rutils::lagit(dd < -sto_p*cum_max)
  pnls <- ifelse(is_dd, 0, pnls)
  sum(pnls)
})  # end sapply
# Plot cumulative pnls for stop-loss strategies
plot(x=0.01*(1:20), y=cum_pnls,
     main="Cumulative PnLs for Stop-loss Strategies",
     xlab="stop-loss level", ylab="cumulative pnl",
     t="l", lwd=3, col="blue")
# Simulate stop-loss strategy
sto_p <- 0.05
ma_x <- 0.0
mi_n <- 0.0
cum_ret <- 0.0
pnls <- vtis
for (i in 1:(nrows-1)) {
# Calculate drawdown
  cum_ret <- cum_ret + vtis[i]
  ma_x <- max(ma_x, cum_ret)
  dd <- (cum_ret - ma_x)
# Check for stop-loss
  if (dd < -sto_p*ma_x) {
    pnls[i+1] <- 0
    mi_n <- min(mi_n, cum_ret)
    du <- (cum_ret - mi_n)
# Check for gain
    if (du > sto_p*mi_n) {
pnls[i+1] <- vtis[i+1]
    }  # end if
  } else {
    mi_n <- cum_ret
  }  # end if
}  # end for
# Combine data
wealth <- xts::xts(cbind(vtis, pnls), dates)
colnames <- c("VTI", "Strategy")
colnames(wealth) <- colnames
# Calculate Sharpe and Sortino ratios
sqrt(252)*sapply(wealth,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# dygraph plot VTI stop-loss strategy
dygraphs::dygraph(cumsum(wealth), main="VTI Stop-loss Strategy") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=colnames[2], axis="y2", strokeWidth=2, col="red")
# Extract VTI log OHLC prices
ohlc <- log(rutils::etfenv$VTI)
closep <- quantmod::Cl(ohlc)
returns <- rutils::diffit(closep)
# Calculate the centered volatility
look_back <- 21
half_back <- look_back %/% 2
volat <- roll::roll_sd(returns, width=look_back, min_obs=1)
volat <- rutils::lagit(volat, lagg=(-half_back))
# Calculate the z-scores of prices
pricescores <- (2*closep -
  rutils::lagit(closep, half_back, pad_zeros=FALSE) -
  rutils::lagit(closep, -half_back, pad_zeros=FALSE))
pricescores <- ifelse(volat > 0, pricescores/volat, 0)
# Plot dygraph of z-scores of VTI prices
prices <- cbind(closep, pricescores)
colnames(prices) <- c("VTI", "Z-scores")
colnames <- colnames(prices)
dygraphs::dygraph(prices["2009"], main="VTI Price Z-Scores") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", label=colnames[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnames[2], axis="y2", label=colnames[2], strokeWidth=2, col="red")
# Calculate thresholds for labeling tops and bottoms
threshold_s <- quantile(pricescores, c(0.1, 0.9))
# Calculate the vectors of tops and bottoms
top_s <- zoo::coredata(pricescores > threshold_s[2])
colnames(top_s) <- "tops"
bottom_s <- zoo::coredata(pricescores < threshold_s[1])
colnames(bottom_s) <- "bottoms"
# Simulate in-sample VTI strategy
po_s <- rep(NA_integer_, NROW(returns))
po_s[1] <- 0
po_s[top_s] <- (-1)
po_s[bottom_s] <- 1
po_s <- zoo::na.locf(po_s)
po_s <- rutils::lagit(po_s)
pnls <- returns*po_s
# Plot dygraph of in-sample VTI strategy
wealth <- cbind(returns, pnls)
colnames(wealth) <- c("VTI", "Strategy")
dygraphs::dygraph(cumsum(wealth), main="VTI Strategy Using In-sample Labels") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Calculate volatility z-scores
volat <- HighFreq::roll_var_ohlc(ohlc=ohlc, look_back=look_back, scale=FALSE)
volat_mean <- roll::roll_mean(volat, width=look_back, min_obs=1)
volat_sd <- roll::roll_sd(rutils::diffit(volat), width=look_back, min_obs=1)
volat_sd[1] <- 0
volat_scores <- ifelse(volat_sd > 0, (volat - volat_mean)/volat_sd, 0)
colnames(volat_scores) <- "volat"
# Calculate volume z-scores
volumes <- quantmod::Vo(ohlc)
volume_mean <- roll::roll_mean(volumes, width=look_back, min_obs=1)
volume_sd <- roll::roll_sd(rutils::diffit(volumes), width=look_back, min_obs=1)
volume_sd[1] <- 0
volume_scores <- ifelse(volume_sd > 0, (volumes - volume_mean)/volume_sd, 0)
colnames(volume_scores) <- "volume"
# Define design matrix for tops including intercept column
design <- cbind(top_s, intercept=rep(1, NROW(top_s)),
           volat_scores, volume_scores)
# Define regression formula
colnames <- colnames(design)
formulav <- as.formula(paste(paste(colnames[1],
  paste(colnames[-1], collapse="+"), sep=" ~ "), "-1"))
# Fit in-sample logistic regression for tops
glmod <- glm(formulav, data=design, family=binomial(logit))
summary(glmod)
coeff <- glmod$coefficients
predictv <- drop(design[, -1] %*% coeff)
ordern <- order(predictv)
# Calculate in-sample forecasts from logistic regression model
forecasts <- 1/(1+exp(-predictv))
all.equal(glmod$fitted.values, forecasts, check.attributes=FALSE)
hist(forecasts)
x11(width=6, height=5)
plot(x=predictv[ordern], y=top_s[ordern],
     main="Logistic Regression of Stock Tops",
     col="orange", xlab="predictor", ylab="top")
lines(x=predictv[ordern], y=glmod$fitted.values[ordern], col="blue", lwd=3)
legend(x="topleft", inset=0.1, bty="n", lwd=6,
 legend=c("tops", "logit fitted values"),
 col=c("orange", "blue"), lty=c(NA, 1), pch=c(1, NA))
# Define discrimination threshold value
threshold <- quantile(forecasts, 0.95)
# Calculate confusion matrix in-sample
confu_sion <- table(actual=!top_s, forecast=(forecasts < threshold))
confu_sion
# Calculate FALSE positive (type I error)
sum(!top_s & (forecasts > threshold))
# Calculate FALSE negative (type II error)
sum(top_s & (forecasts < threshold))
# Calculate FALSE positive and FALSE negative rates
confu_sion <- confu_sion / rowSums(confu_sion)
c(typeI=confu_sion[2, 1], typeII=confu_sion[1, 2])
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
con_fuse <- function(actual, forecasts, threshold) {
    conf <- table(actual, (forecasts < threshold))
    conf <- conf / rowSums(conf)
    c(typeI=conf[2, 1], typeII=conf[1, 2])
  }  # end con_fuse
con_fuse(!top_s, forecasts, threshold=threshold)
# Define vector of discrimination thresholds
threshold_s <- quantile(forecasts, seq(0.1, 0.99, by=0.01))
# Calculate error rates
error_rates <- sapply(threshold_s, con_fuse,
  actual=!top_s, forecasts=forecasts)  # end sapply
error_rates <- t(error_rates)
rownames(error_rates) <- threshold_s
# Calculate the informedness
inform_ed <- 2 - rowSums(error_rates[, c("typeI", "typeII")])
plot(threshold_s, inform_ed, t="l", main="Informedness")
# Find the threshold corresponding to highest informedness
threshold_top <- threshold_s[which.max(inform_ed)]
tops_forecast <- (forecasts > threshold_top)
# Calculate area under ROC curve (AUC)
error_rates <- rbind(c(1, 0), error_rates)
error_rates <- rbind(error_rates, c(0, 1))
true_pos <- (1 - error_rates[, "typeII"])
true_pos <- (true_pos + rutils::lagit(true_pos))/2
false_pos <- rutils::diffit(error_rates[, "typeI"])
abs(sum(true_pos*false_pos))
# Plot ROC Curve for stock tops
x11(width=5, height=5)
plot(x=error_rates[, "typeI"], y=1-error_rates[, "typeII"],
     xlab="FALSE positive rate", ylab="TRUE positive rate",
     main="ROC Curve for Stock Tops", type="l", lwd=3, col="blue")
abline(a=0.0, b=1.0, lwd=3, col="orange")
# Define design matrix for tops including intercept column
design <- cbind(bottom_s, intercept=rep(1, NROW(bottom_s)),
           volat_scores, volume_scores)
# Define regression formula
colnames <- colnames(design)
formulav <- as.formula(paste(paste(colnames[1],
  paste(colnames[-1], collapse="+"), sep=" ~ "), "-1"))
# Fit in-sample logistic regression for tops
glmod <- glm(formulav, data=design, family=binomial(logit))
summary(glmod)
# Calculate in-sample forecast from logistic regression model
predictv <- drop(design[, -1] %*% glmod$coefficients)
forecasts <- 1/(1+exp(-predictv))
# Calculate error rates
error_rates <- sapply(threshold_s, con_fuse,
  actual=!bottom_s, forecasts=forecasts)  # end sapply
error_rates <- t(error_rates)
rownames(error_rates) <- threshold_s
# Calculate the informedness
inform_ed <- 2 - rowSums(error_rates[, c("typeI", "typeII")])
plot(threshold_s, inform_ed, t="l", main="Informedness")
# Find the threshold corresponding to highest informedness
threshold_bottom <- threshold_s[which.max(inform_ed)]
bottoms_forecast <- (forecasts > threshold_bottom)
# Calculate area under ROC curve (AUC)
error_rates <- rbind(c(1, 0), error_rates)
error_rates <- rbind(error_rates, c(0, 1))
true_pos <- (1 - error_rates[, "typeII"])
true_pos <- (true_pos + rutils::lagit(true_pos))/2
false_pos <- rutils::diffit(error_rates[, "typeI"])
abs(sum(true_pos*false_pos))
# Plot ROC Curve for stock tops
x11(width=5, height=5)
plot(x=error_rates[, "typeI"], y=1-error_rates[, "typeII"],
     xlab="FALSE positive rate", ylab="TRUE positive rate",
     main="ROC Curve for Stock Bottoms", type="l", lwd=3, col="blue")
abline(a=0.0, b=1.0, lwd=3, col="orange")
# Simulate in-sample VTI strategy
po_s <- rep(NA_integer_, NROW(returns))
po_s[1] <- 0
po_s[tops_forecast] <- (-1)
po_s[bottoms_forecast] <- 1
po_s <- zoo::na.locf(po_s)
po_s <- rutils::lagit(po_s)
pnls <- returns*po_s
# Plot dygraph of in-sample VTI strategy
wealth <- cbind(returns, pnls)
colnames(wealth) <- c("VTI", "Strategy")
dygraphs::dygraph(cumsum(wealth), main="Logistic Strategy Using Top and Bottom Labels") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Fit logistic regression over training data
set.seed(1121)  # Reset random number generator
nrows <- NROW(Default)
samplev <- sample.int(n=nrows, size=nrows/2)
traindata <- Default[samplev, ]
glmod <- glm(formulav, data=traindata, family=binomial(logit))
# Forecast over test data out-of-sample
test_data <- Default[-samplev, ]
forecasts <- predict(glmod, newdata=test_data, type="response")
# Calculate confusion matrix out-of-sample
table(actual=!test_data$de_fault,
forecast=(forecasts < threshold))
# Calculate VTI percentage returns
closep <- log(na.omit(rutils::etfenv$prices$VTI))
returns <- rutils::diffit(closep)
# Define look-back window
look_back <- 11
# Calculate time series of medians
medi_an <- roll::roll_median(closep, width=look_back)
# medi_an <- TTR::runMedian(closep, n=look_back)
# Calculate time series of MAD
madv <- HighFreq::roll_var(closep, look_back=look_back, method="nonparametric")
# madv <- TTR::runMAD(closep, n=look_back)
# Calculate time series of z-scores
zscores <- (closep - medi_an)/madv
zscores[1:look_back, ] <- 0
tail(zscores, look_back)
range(zscores)
# Define threshold value
threshold <- sum(abs(range(zscores)))/8
# Simulate VTI strategy
position_s <- rep(NA_integer_, NROW(closep))
position_s[1] <- 0
position_s[zscores < -threshold] <- 1
position_s[zscores > threshold] <- (-1)
position_s <- zoo::na.locf(position_s)
position_s <- rutils::lagit(position_s)
pnls <- returns*position_s
# Plot dygraph of Hampel strategy pnls
wealth <- cbind(returns, pnls)
colnames(wealth) <- c("VTI", "Strategy")
dygraphs::dygraph(cumsum(wealth), main="VTI Hampel Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
lambdas <- c(0.5, 1, 1.5)
colors <- c("red", "blue", "green")
# Define the leverage function
leverage <- function(p, lambda) tanh(lambda*p)
# Plot three curves in loop
for (indeks in 1:3) {
  curve(expr=leverage(x, lambda=lambdas[indeks]),
xlim=c(-4, 4), type="l", lwd=4,
xlab="predictor", ylab="dollar amount",
col=colors[indeks], add=(indeks>1))
}  # end for
# Add title
title(main="Leverage function", line=0.5)
# Add legend
legend("topleft", title="Leverage parameters",
       paste("lambda", lambdas, sep="="),
       inset=0.05, cex=0.8, lwd=6, bty="n",
       lty=1, col=colors)
# Extract log VTI prices
ohlc <- rutils::etfenv$VTI
closep <- log(quantmod::Cl(ohlc))
colnames(closep) <- "VTI"
nrows <- NROW(closep)
# Calculate EWMA weights
look_back <- 333
lambda <- 0.004
weightv <- exp(-lambda*(1:look_back))
weightv <- weightv/sum(weightv)
# Calculate EWMA prices
ew_ma <- HighFreq::roll_wsum(closep, weights=weightv)
# Copy over NA values
ew_ma <- zoo::na.locf(ew_ma, fromLast=TRUE)
prices <- cbind(closep, ew_ma)
colnames(prices) <- c("VTI", "VTI EWMA")
# Dygraphs plot with custom line colors
colnames <- colnames(prices)
dygraphs::dygraph(prices["2007/"], main="VTI EWMA Prices") %>%
  dySeries(name=colnames[1], label=colnames[1], strokeWidth=1, col="blue") %>%
  dySeries(name=colnames[2], label=colnames[2], strokeWidth=4, col="red") %>%
  dyLegend(show="always", width=500)
# Standard plot of  EWMA prices with custom line colors
x11(width=6, height=5)
plot_theme <- chart_theme()
colors <- c("blue", "red")
plot_theme$col$line.col <- colors
quantmod::chart_Series(prices["2007/"], theme=plot_theme,
       lwd=2, name="VTI EWMA Prices")
legend("topleft", legend=colnames(prices),
 inset=0.1, bg="white", lty=1, lwd=6, cex=0.8,
 col=plot_theme$col$line.col, bty="n")
# Calculate positions, either: -1, 0, or 1
indic <- sign(closep - ew_ma)
po_s <- rutils::lagit(indic, lagg=1)
# Create colors for background shading
dates <- (rutils::diffit(po_s) != 0)
shad_e <- po_s[dates]
dates <- c(index(shad_e), end(po_s))
shad_e <- ifelse(drop(zoo::coredata(shad_e)) == 1, "lightgreen", "antiquewhite")
# Create dygraph object without plotting it
dyplot <- dygraphs::dygraph(prices["2007/"], main="VTI EWMA Prices") %>%
  dySeries(name=colnames[1], label=colnames[1], strokeWidth=1, col="blue") %>%
  dySeries(name=colnames[2], label=colnames[2], strokeWidth=4, col="red") %>%
  dyLegend(show="always", width=500)
# Add shading to dygraph object
for (i in 1:NROW(shad_e)) {
    dyplot <- dyplot %>% dyShading(from=dates[i], to=dates[i+1], color=shad_e[i])
}  # end for
# Plot the dygraph object
dyplot
# Equivalent code to the above
# Determine trade dates right after EWMA has crossed prices
indic <- sign(closep - ew_ma)
dates <- (rutils::diffit(indic) != 0)
dates <- which(dates) + 1
dates <- dates[dates < nrows]
# Calculate positions, either: -1, 0, or 1
po_s <- rep(NA_integer_, nrows)
po_s[1] <- 0
po_s[dates] <- indic[dates-1]
po_s <- zoo::na.locf(po_s, na.rm=FALSE)
po_s <- xts::xts(po_s, order.by=index(closep))
# Create indicator for background shading
shad_e <- po_s[dates]
dates <- index(shad_e)
dates <- c(dates, end(po_s))
shad_e <- ifelse(drop(zoo::coredata(shad_e)) == 1, "lightgreen", "antiquewhite")
# Standard plot of EWMA prices with position shading
x11(width=6, height=5)
quantmod::chart_Series(prices["2007/"], theme=plot_theme,
       lwd=2, name="VTI EWMA Prices")
add_TA(po_s > 0, on=-1, col="lightgreen", border="lightgreen")
add_TA(po_s < 0, on=-1, col="lightgrey", border="lightgrey")
legend("topleft", legend=colnames(prices),
 inset=0.1, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")
# Calculate daily profits and losses of EWMA strategy
vtis <- rutils::diffit(closep)  # VTI returns
pnls <- vtis*po_s
colnames(pnls) <- "EWMA"
wealth <- cbind(vtis, pnls)
colnames(wealth) <- c("VTI", "EWMA PnL")
# Annualized Sharpe ratio of EWMA strategy
sqrt(252)*sapply(wealth, function (x) mean(x)/sd(x))
# The crossover strategy has a negative correlation to VTI
cor(wealth)
# Plot dygraph of EWMA strategy wealth
# Create dygraph object without plotting it
colors <- c("blue", "red")
dyplot <- dygraphs::dygraph(cumsum(wealth["2007/"]), main="Performance of EWMA Strategy") %>%
  dyOptions(colors=colors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Add shading to dygraph object
for (i in 1:NROW(shad_e)) {
    dyplot <- dyplot %>%
dyShading(from=dates[i], to=dates[i+1], color=shad_e[i])
}  # end for
# Plot the dygraph object
dyplot
# Standard plot of EWMA strategy wealth
x11(width=6, height=5)
plot_theme <- chart_theme()
plot_theme$col$line.col <- colors
quantmod::chart_Series(cumsum(wealth["2007/"]), theme=plot_theme,
       name="Performance of EWMA Strategy")
add_TA(po_s > 0, on=-1, col="lightgreen", border="lightgreen")
add_TA(po_s < 0, on=-1, col="lightgrey", border="lightgrey")
legend("top", legend=colnames(wealth),
 inset=0.05, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")
# Test EWMA crossover market timing of VTI using Treynor-Mazuy test
design <- cbind(pnls, vtis, vtis^2)
design <- na.omit(design)
colnames(design) <- c("EWMA", "VTI", "treynor")
model <- lm(EWMA ~ VTI + treynor, data=design)
summary(model)
# Plot residual scatterplot
residuals <- (design$EWMA - model$coeff[2]*design$VTI)
residuals <- model$residuals
x11(width=6, height=6)
plot.default(x=design$VTI, y=residuals, xlab="VTI", ylab="residuals")
title(main="Treynor-Mazuy Market Timing Test\n for EWMA Crossover vs VTI", line=0.5)
# Plot fitted (predicted) response values
fit_ted <- (model$coeff["(Intercept)"] +
        model$coeff["treynor"]*vtis^2)
points.default(x=design$VTI, y=fit_ted, pch=16, col="red")
text(x=0.05, y=0.8*max(residuals), paste("EWMA crossover t-value =", round(summary(model)$coeff["treynor", "t value"], 2)))
# Determine trade dates right after EWMA has crossed prices
indic <- sign(closep - ew_ma)
# Calculate positions from lagged indicator
lagg <- 2
indic <- roll::roll_sum(indic, width=lagg, min_obs=1)
# Calculate positions, either: -1, 0, or 1
po_s <- rep(NA_integer_, nrows)
po_s[1] <- 0
po_s <- ifelse(indic == lagg, 1, po_s)
po_s <- ifelse(indic == (-lagg), -1, po_s)
po_s <- zoo::na.locf(po_s, na.rm=FALSE)
po_s <- xts::xts(po_s, order.by=index(closep))
# Lag the positions to trade in next period
po_s <- rutils::lagit(po_s, lagg=1)
# Calculate PnLs of lagged strategy
pnls_lag <- vtis*po_s
colnames(pnls_lag) <- "Lagged Strategy"
wealth <- cbind(pnls, pnls_lag)
colnames(wealth) <- c("EWMA Strategy", "Lagged Strategy")
# Annualized Sharpe ratios of EWMA strategies
sharp_e <- sqrt(252)*sapply(wealth, function (x) mean(x)/sd(x))
# Plot both strategies
dygraphs::dygraph(cumsum(wealth["2007/"]), main=paste("EWMA Crossover Strategy, Sharpe", paste(paste(names(sharp_e), round(sharp_e, 3), sep="="), collapse=", "))) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Calculate positions, either: -1, 0, or 1
indic <- sign(closep - ew_ma)
po_s <- rutils::lagit(indic, lagg=1)
# Calculate daily pnl for days without trades
pnls_lag <- vtis*po_s
# Determine trade dates right after EWMA has crossed prices
dates <- which(rutils::diffit(po_s) != 0)
# Calculate realized pnl for days with trades
openp <- log(quantmod::Op(ohlc))
close_lag <- rutils::lagit(closep)
pos_lag <- rutils::lagit(po_s)
pnls_lag[dates] <- pos_lag[dates]*
  (openp[dates] - close_lag[dates])
# Calculate unrealized pnl for days with trades
pnls_lag[dates] <- pnls_lag[dates] +
  po_s[dates]*(closep[dates] - openp[dates])
# Calculate the wealth
wealth <- cbind(vtis, pnls_lag)
colnames(wealth) <- c("VTI", "EWMA PnL")
# Annualized Sharpe ratio of EWMA strategy
sqrt(252)*sapply(wealth, function (x) mean(x)/sd(x))
# The crossover strategy has a negative correlation to VTI
cor(wealth)
# Plot dygraph of EWMA strategy wealth
dygraphs::dygraph(cumsum(wealth["2007/"]), main="EWMA Strategy Trading at the Open Price") %>%
  dyOptions(colors=colors, strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Standard plot of EWMA strategy wealth
quantmod::chart_Series(wealth, theme=plot_theme,
       name="EWMA Strategy Trading at the Open Price")
legend("top", legend=colnames(wealth),
 inset=0.05, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")
# bid_offer equal to 10 bps for liquid ETFs
bid_offer <- 0.001
# Calculate transaction costs
costs <- 0.5*bid_offer*abs(pos_lag - po_s)*closep
# Plot strategy with transaction costs
wealth <- cbind(pnls, pnls - costs)
colnames(wealth) <- c("EWMA", "EWMA w Costs")
colors <- c("blue", "red")
dygraphs::dygraph(cumsum(wealth["2007/"]), main="EWMA Strategy With Transaction Costs") %>%
  dyOptions(colors=colors, strokeWidth=2) %>%
  dyLegend(show="always", width=500)
sim_ewma <- function(ohlc, lambda=0.01, look_back=333, bid_offer=0.001,
                trend=1, lagg=1) {
  close <- log(quantmod::Cl(ohlc))
  returns <- rutils::diffit(close)
  nrows <- NROW(ohlc)
  # Calculate EWMA prices
  weights <- exp(-lambda*(1:look_back))
  weights <- weights/sum(weights)
  ewma <- HighFreq::roll_wsum(close, weights=weights)
  # Calculate the indicator
  indic <- trend*sign(close - ewma)
  if (lagg > 1) {
    indic <- roll::roll_sum(indic, width=lagg, min_obs=1)
    indic[1:lagg] <- 0
  }  # end if
  # Calculate positions, either: -1, 0, or 1
  pos <- rep(NA_integer_, nrows)
  pos[1] <- 0
  pos <- ifelse(indic == lagg, 1, pos)
  pos <- ifelse(indic == (-lagg), -1, pos)
  pos <- zoo::na.locf(pos, na.rm=FALSE)
  pos <- xts::xts(pos, order.by=index(close))
  # Lag the positions to trade on next day
  pos <- rutils::lagit(pos, lagg=1)
  # Calculate PnLs of strategy
  pnls <- returns*pos
  costs <- 0.5*bid_offer*abs(rutils::diffit(pos))*close
  pnls <- (pnls - costs)
  # Calculate strategy returns
  pnls <- cbind(pos, pnls)
  colnames(pnls) <- c("positions", "pnls")
  pnls
}  # end sim_ewma
source("/Users/jerzy/Develop/lecture_slides/scripts/ewma_model.R")
lambdas <- seq(from=0.001, to=0.008, by=0.001)
# Perform lapply() loop over lambdas
pnls <- lapply(lambdas, function(lambda) {
  # Simulate EWMA strategy and calculate returns
  sim_ewma(ohlc=ohlc, lambda=lambda, look_back=look_back, bid_offer=0, lagg=2)[, "pnls"]
})  # end lapply
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("lambda=", lambdas)
# Plot dygraph of multiple EWMA strategies
colors <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls["2007/"]), main="Cumulative Returns of Trend Following EWMA Strategies") %>%
  dyOptions(colors=colors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Plot EWMA strategies with custom line colors
x11(width=6, height=5)
plot_theme <- chart_theme()
plot_theme$col$line.col <- colors
quantmod::chart_Series(cumsum(pnls), theme=plot_theme,
  name="Cumulative Returns of EWMA Strategies")
legend("topleft", legend=colnames(pnls), inset=0.1,
  bg="white", cex=0.8, lwd=rep(6, NCOL(pnls)),
  col=plot_theme$col$line.col, bty="n")
# Initialize compute cluster under Windows
library(parallel)
cluster <- makeCluster(detectCores()-1)
clusterExport(cluster,
  varlist=c("ohlc", "look_back", "sim_ewma"))
# Perform parallel loop over lambdas under Windows
pnls <- parLapply(cluster, lambdas, function(lambda) {
  library(quantmod)
  # Simulate EWMA strategy and calculate returns
  sim_ewma(ohlc=ohlc, lambda=lambda, look_back=look_back)[, "pnls"]
})  # end parLapply
stopCluster(cluster)  # Stop R processes over cluster under Windows
# Perform parallel loop over lambdas under Mac-OSX or Linux
pnls <- mclapply(lambdas, function(lambda) {
  library(quantmod)
  # Simulate EWMA strategy and calculate returns
  sim_ewma(ohlc=ohlc, lambda=lambda, look_back=look_back)[, "pnls"]
})  # end mclapply
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("lambda=", lambdas)
# Calculate annualized Sharpe ratios of strategy returns
sharper <- sqrt(252)*sapply(pnls, function(xtes) {
  mean(xtes)/sd(xtes)
})  # end sapply
# Plot Sharpe ratios
dev.new(width=6, height=5, noRStudioGD=TRUE)
plot(x=lambdas, y=sharper, t="l",
     xlab="lambda", ylab="Sharpe",
     main="Performance of EWMA Trend Following Strategies
     as Function of the Decay Parameter Lambda")
# Find optimal lambda
lambda <- lambdas[which.max(sharper)]
# Plot optimal weights
weightv <- exp(-lambda*(1:look_back))
weightv <- weightv/sum(weightv)
plot(weightv, t="l", xlab="days", ylab="weights",
     main="Optimal Weights of EWMA Trend Following Strategy")
trend_returns <- pnls
trend_sharpe <- sharper
# Simulate best performing strategy
ewma_trend <- sim_ewma(ohlc=ohlc, lambda=lambda, look_back=look_back, bid_offer=0, lagg=2)
po_s <- ewma_trend[, "positions"]
pnls <- ewma_trend[, "pnls"]
wealth <- cbind(vtis, pnls)
colnames(wealth) <- c("VTI", "EWMA PnL")
# Create colors for background shading
dates <- (rutils::diffit(po_s) != 0)
shad_e <- po_s[dates]
dates <- c(index(shad_e), end(po_s))
shad_e <- ifelse(drop(zoo::coredata(shad_e)) == 1, "lightgreen", "antiquewhite")
colors <- c("blue", "red")
# Plot dygraph of EWMA strategy wealth
# Create dygraph object without plotting it
dyplot <- dygraphs::dygraph(cumsum(wealth["2007/"]), main="Performance of Optimal Trend Following EWMA Strategy") %>%
  dyOptions(colors=colors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Add shading to dygraph object
for (i in 1:NROW(shad_e)) {
    dyplot <- dyplot %>%
dyShading(from=dates[i], to=dates[i+1], color=shad_e[i])
}  # end for
# Plot the dygraph object
dyplot
# Plot EWMA PnL with position shading
# Standard plot of EWMA strategy wealth
x11(width=6, height=5)
plot_theme <- chart_theme()
plot_theme$col$line.col <- colors
quantmod::chart_Series(cumsum(wealth["2007/"]), theme=plot_theme,
       name="Performance of EWMA Strategy")
add_TA(po_s > 0, on=-1, col="lightgreen", border="lightgreen")
add_TA(po_s < 0, on=-1, col="lightgrey", border="lightgrey")
legend("top", legend=colnames(wealth),
 inset=0.05, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")
source("/Users/jerzy/Develop/lecture_slides/scripts/ewma_model.R")
lambdas <- seq(0.05, 1.0, 0.05)
# Perform lapply() loop over lambdas
pnls <- lapply(lambdas, function(lambda) {
  # Simulate EWMA strategy and calculate returns
  sim_ewma(ohlc=ohlc, lambda=lambda, look_back=look_back, trend=(-1))[, "pnls"]
})  # end lapply
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("lambda=", lambdas)
# Plot dygraph of mean reverting EWMA strategies
column_s <- seq(1, NCOL(pnls), by=4)
colors <- colorRampPalette(c("blue", "red"))(NROW(column_s))
dygraphs::dygraph(cumsum(pnls["2007/", column_s]), main="Cumulative Returns of Mean Reverting EWMA Strategies") %>%
  dyOptions(colors=colors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Plot EWMA strategies with custom line colors
x11(width=6, height=5)
plot_theme <- chart_theme()
plot_theme$col$line.col <- colors
quantmod::chart_Series(pnls[, column_s],
  theme=plot_theme, name="Cumulative Returns of Mean Reverting EWMA Strategies")
legend("topleft", legend=colnames(pnls[, column_s]),
  inset=0.1, bg="white", cex=0.8, lwd=6,
  col=plot_theme$col$line.col, bty="n")
# Calculate Sharpe ratios of strategy returns
sharper <- sqrt(252)*sapply(pnls, function(xtes) {
  mean(xtes)/sd(xtes)
})  # end sapply
plot(x=lambdas, y=sharper, t="l",
     xlab="lambda", ylab="Sharpe",
     main="Performance of EWMA Mean Reverting Strategies
     as Function of the Decay Parameter Lambda")
revert_returns <- pnls
revert_sharpe <- sharper
# Find optimal lambda
lambda <- lambdas[which.max(sharper)]
# Simulate best performing strategy
ewma_revert <- sim_ewma(ohlc=ohlc, bid_offer=0.0,
  lambda=lambda, look_back=look_back, trend=(-1))
po_s <- ewma_revert[, "positions"]
pnls <- ewma_revert[, "pnls"]
wealth <- cbind(vtis, pnls)
colnames(wealth) <- c("VTI", "EWMA PnL")
# Plot dygraph of EWMA strategy wealth
colors <- c("blue", "red")
dygraphs::dygraph(cumsum(wealth["2007/"]), main="Optimal Mean Reverting EWMA Strategy") %>%
  dyOptions(colors=colors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Standard plot of EWMA strategy wealth
x11(width=6, height=5)
plot_theme <- chart_theme()
plot_theme$col$line.col <- colors
quantmod::chart_Series(cumsum(wealth["2007/"]), theme=plot_theme,
       name="Optimal Mean Reverting EWMA Strategy")
add_TA(po_s > 0, on=-1, col="lightgreen", border="lightgreen")
add_TA(po_s < 0, on=-1, col="lightgrey", border="lightgrey")
legend("top", legend=colnames(wealth),
 inset=0.05, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")
# Calculate correlation between trend following and mean reverting strategies
trend_ing <- ewma_trend[, "pnls"]
colnames(trend_ing) <- "trend"
revert_ing <- ewma_revert[, "pnls"]
colnames(revert_ing) <- "revert"
cor(cbind(vtis, trend_ing, revert_ing))
# Calculate combined strategy
com_bined <- (vtis + trend_ing + revert_ing)/3
colnames(com_bined) <- "combined"
# Calculate annualized Sharpe ratio of strategy returns
returns <- cbind(vtis, trend_ing, revert_ing, com_bined)
colnames(returns) <- c("VTI", "Trending", "Reverting", "EWMA combined")
sqrt(252)*sapply(returns, function(xtes) mean(xtes)/sd(xtes))
# Plot dygraph of EWMA strategy wealth
colors <- c("blue", "red", "green", "purple")
dygraphs::dygraph(cumsum(returns["2007/"]), main="Performance of Combined EWMA Strategies") %>%
  dyOptions(colors=colors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Standard plot of EWMA strategy wealth
plot_theme <- chart_theme()
plot_theme$col$line.col <- colors
quantmod::chart_Series(pnls, theme=plot_theme,
       name="Performance of Combined EWMA Strategies")
legend("topleft", legend=colnames(pnls),
 inset=0.05, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")
weightv <- c(trend_sharpe, revert_sharpe)
weightv[weightv<0] <- 0
weightv <- weightv/sum(weightv)
returns <- cbind(trend_returns, revert_returns)
returns <- returns %*% weightv
returns <- xts::xts(returns, order.by=index(vtis))
returns <- cbind(vtis, returns)
colnames(returns) <- c("VTI", "EWMA PnL")
# Plot dygraph of EWMA strategy wealth
colors <- c("blue", "red")
dygraphs::dygraph(cumsum(returns["2007/"]), main="Performance of Ensemble of EWMA Strategies") %>%
  dyOptions(colors=colors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Standard plot of EWMA strategy wealth
plot_theme <- chart_theme()
plot_theme$col$line.col <- colors
quantmod::chart_Series(cumsum(returns["2007/"]), theme=plot_theme,
       name="Performance of Ensemble of EWMA Strategies")
legend("topleft", legend=colnames(pnls),
 inset=0.05, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")
# Calculate fast and slow EWMAs
look_back <- 333
lambda1 <- 0.04
lambda2 <- 0.004
weightv <- exp(-lambda1*(1:look_back))
weightv <- weightv/sum(weightv)
ewma1 <- HighFreq::roll_wsum(closep, weights=weightv)
weightv <- exp(-lambda2*(1:look_back))
weightv <- weightv/sum(weightv)
ewma2 <- HighFreq::roll_wsum(closep, weights=weightv)
# Calculate EWMA prices
prices <- cbind(closep, ewma1, ewma2)
colnames(prices) <- c("VTI", "EWMA fast", "EWMA slow")
# Calculate positions, either: -1, 0, or 1
indic <- sign(ewma1 - ewma2)
lagg <- 2
indic <- roll::roll_sum(indic, width=lagg, min_obs=1)
po_s <- rep(NA_integer_, nrows)
po_s[1] <- 0
po_s <- ifelse(indic == lagg, 1, po_s)
po_s <- ifelse(indic == (-lagg), -1, po_s)
po_s <- zoo::na.locf(po_s, na.rm=FALSE)
po_s <- xts::xts(po_s, order.by=index(closep))
po_s <- rutils::lagit(po_s, lagg=1)
# Create colors for background shading
dates <- (rutils::diffit(po_s) != 0)
shad_e <- po_s[dates]
dates <- c(index(shad_e), end(po_s))
shad_e <- ifelse(drop(zoo::coredata(shad_e)) == 1, "lightgreen", "antiquewhite")
# Plot dygraph
colnames <- colnames(prices)
dyplot <- dygraphs::dygraph(prices["2007/"], main="VTI Dual EWMA Prices") %>%
  dySeries(name=colnames[1], label=colnames[1], strokeWidth=1, col="blue") %>%
  dySeries(name=colnames[2], label=colnames[2], strokeWidth=4, col="red") %>%
  dySeries(name=colnames[3], label=colnames[3], strokeWidth=4, col="purple") %>%
  dyLegend(show="always", width=500)
for (i in 1:NROW(shad_e)) {
    dyplot <- dyplot %>% dyShading(from=dates[i], to=dates[i+1], color=shad_e[i])
}  # end for
dyplot
# Calculate daily profits and losses of strategy
pnls <- vtis*po_s
colnames(pnls) <- "Strategy"
wealth <- cbind(vtis, pnls)
# Annualized Sharpe ratio of Dual EWMA strategy
sharp_e <- sqrt(252)*sapply(wealth, function (x) mean(x)/sd(x))
# The crossover strategy has a negative correlation to VTI
cor(wealth)
# Plot Dual EWMA strategy
dyplot <- dygraphs::dygraph(cumsum(wealth["2007/"]), main=paste("EWMA Dual Crossover Strategy, Sharpe", paste(paste(names(sharp_e), round(sharp_e, 3), sep="="), collapse=", "))) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=1)
# Add shading to dygraph object
for (i in 1:NROW(shad_e)) {
    dyplot <- dyplot %>% dyShading(from=dates[i], to=dates[i+1], color=shad_e[i])
}  # end for
# Plot the dygraph object
dyplot
sim_ewma2 <- function(ohlc, lambda1=0.1, lambda2=0.01, look_back=333,
                bid_offer=0.001, trend=1, lagg=1) {
  close <- log(quantmod::Cl(ohlc))
  returns <- rutils::diffit(close)
  nrows <- NROW(ohlc)
  # Calculate EWMA prices
  weights <- exp(-lambda1*(1:look_back))
  weights <- weights/sum(weights)
  ewma1 <- HighFreq::roll_wsum(closep, weights=weights)
  weights <- exp(-lambda2*(1:look_back))
  weights <- weights/sum(weights)
  ewma2 <- HighFreq::roll_wsum(closep, weights=weights)
  # Calculate positions, either: -1, 0, or 1
  indic <- sign(ewma1 - ewma2)
  if (lagg > 1) {
    indic <- roll::roll_sum(indic, width=lagg, min_obs=1)
    indic[1:lagg] <- 0
  }  # end if
  pos <- rep(NA_integer_, nrows)
  pos[1] <- 0
  pos <- ifelse(indic == lagg, 1, pos)
  pos <- ifelse(indic == (-lagg), -1, pos)
  pos <- zoo::na.locf(pos, na.rm=FALSE)
  pos <- xts::xts(pos, order.by=index(close))
  # Lag the positions to trade on next day
  pos <- rutils::lagit(pos, lagg=1)
  # Calculate PnLs of strategy
  pnls <- returns*pos
  costs <- 0.5*bid_offer*abs(rutils::diffit(pos))*close
  pnls <- (pnls - costs)
  # Calculate strategy returns
  pnls <- cbind(pos, pnls)
  colnames(pnls) <- c("positions", "pnls")
  pnls
}  # end sim_ewma2
source("/Users/jerzy/Develop/lecture_slides/scripts/ewma_model.R")
lambdas1 <- seq(from=0.05, to=0.15, by=0.01)
lambdas2 <- seq(from=0.03, to=0.1, by=0.01)
# Perform sapply() loops over lambdas
sharper <- sapply(lambdas1, function(lambda1) {
  sapply(lambdas2, function(lambda2) {
    if (lambda1 > lambda2) {
# Simulate Dual EWMA strategy
pnls <- sim_ewma2(ohlc=ohlc, lambda1=lambda1, lambda2=lambda2,
                    look_back=look_back, bid_offer=0.0, trend=1, lagg=2)[, "pnls"]
sqrt(252)*mean(pnls)/sd(pnls)
    } else NA
  })  # end sapply
})  # end sapply
colnames(sharper) <- lambdas1
rownames(sharper) <- lambdas2
# Calculate the PnLs for the optimal strategy
whichv <- which(sharper == max(sharper, na.rm=TRUE), arr.ind=TRUE)
lambda1 <- lambdas1[whichv[2]]
lambda2 <- lambdas2[whichv[1]]
pnls <- sim_ewma2(ohlc=ohlc, lambda1=lambda1, lambda2=lambda2,
              look_back=look_back, bid_offer=0.0, trend=1, lagg=2)[, "pnls"]
wealth <- cbind(vtis, pnls)
# Annualized Sharpe ratio of Dual EWMA strategy
sharp_e <- sqrt(252)*sapply(wealth, function (x) mean(x)/sd(x))
# The crossover strategy has a negative correlation to VTI
cor(wealth)
# Plot Optimal Dual EWMA strategy
dyplot <- dygraphs::dygraph(cumsum(wealth["2007/"]), main=paste("Optimal EWMA Dual Crossover Strategy, Sharpe", paste(paste(names(sharp_e), round(sharp_e, 3), sep="="), collapse=", "))) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=1)
# Add shading to dygraph object
for (i in 1:NROW(shad_e)) {
    dyplot <- dyplot %>% dyShading(from=dates[i], to=dates[i+1], color=shad_e[i])
}  # end for
# Plot the dygraph object
dyplot
# Calculate log OHLC prices and volumes
ohlc <- rutils::etfenv$VTI
closep <- log(quantmod::Cl(ohlc))
colnames(closep) <- "VTI"
volumes <- quantmod::Vo(ohlc)
colnames(volumes) <- "Volume"
nrows <- NROW(closep)
# Calculate the VWAP prices
look_back <- 170
vwap <- roll::roll_sum(closep*volumes, width=look_back, min_obs=1)
volume_roll <- roll::roll_sum(volumes, width=look_back, min_obs=1)
vwap <- vwap/volume_roll
colnames(vwap) <- "VWAP"
prices <- cbind(closep, vwap)
# Dygraphs plot with custom line colors
colnames <- colnames(prices)
dygraphs::dygraph(prices["2007/"], main="VTI VWAP Prices") %>%
  dySeries(name=colnames[1], label=colnames[1], strokeWidth=1, col="blue") %>%
  dySeries(name=colnames[2], label=colnames[2], strokeWidth=4, col="red") %>%
  dyLegend(show="always", width=500)
# Plot VWAP prices with custom line colors
x11(width=6, height=5)
colors <- c("blue", "red")
plot_theme <- chart_theme()
plot_theme$col$line.col <- colors
quantmod::chart_Series(prices["2009"], theme=plot_theme,
       lwd=2, name="VTI VWAP Prices")
legend("bottomright", legend=colnames(prices),
 inset=0.1, bg="white", lty=1, lwd=6, cex=0.8,
 col=plot_theme$col$line.col, bty="n")
# Calculate positions from lagged indicator
indic <- sign(closep - vwap)
lagg <- 2
indic <- roll::roll_sum(indic, width=lagg, min_obs=1)
# Calculate positions, either: -1, 0, or 1
po_s <- rep(NA_integer_, nrows)
po_s[1] <- 0
po_s <- ifelse(indic == lagg, 1, po_s)
po_s <- ifelse(indic == (-lagg), -1, po_s)
po_s <- zoo::na.locf(po_s, na.rm=FALSE)
po_s <- xts::xts(po_s, order.by=index(closep))
# Lag the positions to trade in next period
po_s <- rutils::lagit(po_s, lagg=1)
# Calculate PnLs of VWAP strategy
vtis <- rutils::diffit(closep)  # VTI returns
pnls <- vtis*po_s
colnames(pnls) <- "VWAP Strategy"
wealth <- cbind(vtis, pnls)
colnames(wealth) <- c("VTI", "VWAP Strategy")
colnames <- colnames(wealth)
# Annualized Sharpe ratios of VTI and VWAP strategy
sharp_e <- sqrt(252)*sapply(wealth, function (x) mean(x)/sd(x))
# Create colors for background shading
dates <- (rutils::diffit(po_s) != 0)
shad_e <- po_s[dates]
dates <- c(index(shad_e), end(po_s))
shad_e <- ifelse(drop(zoo::coredata(shad_e)) == 1, "lightgreen", "antiquewhite")
# Plot dygraph of VWAP strategy
# Create dygraph object without plotting it
dyplot <- dygraphs::dygraph(cumsum(wealth["2007/"]), main=paste("VWAP Crossover Strategy, Sharpe", paste(paste(names(sharp_e), round(sharp_e, 3), sep="="), collapse=", "))) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Add shading to dygraph object
for (i in 1:NROW(shad_e)) {
    dyplot <- dyplot %>% dyShading(from=dates[i], to=dates[i+1], color=shad_e[i])
}  # end for
# Plot the dygraph object
dyplot
# Calculate correlation of VWAP strategy with VTI
cor(vtis, pnls)
# Combine VWAP strategy with VTI
wealth <- cbind(vtis, pnls, 0.5*(vtis+pnls))
colnames(wealth) <- c("VTI", "VWAP", "Combined")
sharp_e <- sqrt(252)*sapply(wealth, function (x) mean(x)/sd(x))
# Plot dygraph of VWAP strategy combined with VTI
# wippp
colnames <- colnames(wealth)
dygraphs::dygraph(cumsum(wealth), paste("VWAP Crossover Strategy, Sharpe", paste(paste(names(sharp_e), round(sharp_e, 3), sep="="), collapse=", "))) %>%
  dySeries(name=colnames[1], label=colnames[1], col="blue", strokeWidth=1) %>%
  dySeries(name=colnames[2], label=colnames[2], col="red", strokeWidth=1) %>%
  dySeries(name=colnames[3], label=colnames[3], col="purple", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Or
dygraphs::dygraph(cumsum(wealth),
  main=paste("VWAP Crossover Strategy, Sharpe", paste(paste(names(sharp_e), round(sharp_e, 3), sep="="), collapse=", "))) %>%
  dyOptions(colors=c("blue", "red", "purple"), strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Test VWAP crossover market timing of VTI using Treynor-Mazuy test
design <- cbind(pnls, vtis, vtis^2)
design <- na.omit(design)
colnames(design) <- c("VWAP", "VTI", "treynor")
model <- lm(VWAP ~ VTI + treynor, data=design)
summary(model)
# Plot residual scatterplot
residuals <- (design$VWAP - model$coeff[2]*design$VTI)
residuals <- model$residuals
x11(width=6, height=6)
plot.default(x=design$VTI, y=residuals, xlab="VTI", ylab="residuals")
title(main="Treynor-Mazuy Market Timing Test\n for VWAP Crossover vs VTI", line=0.5)
# Plot fitted (predicted) response values
fit_ted <- (model$coeff["(Intercept)"] + model$coeff["treynor"]*vtis^2)
points.default(x=design$VTI, y=fit_ted, pch=16, col="red")
text(x=0.05, y=0.8*max(residuals), paste("VWAP crossover t-value =", round(summary(model)$coeff["treynor", "t value"], 2)))
sim_vwap <- function(ohlc, look_back=333, bid_offer=0.001, trend=1, lagg=1) {
  close <- log(quantmod::Cl(ohlc))
  volumes <- quantmod::Vo(ohlc)
  returns <- rutils::diffit(close)
  nrows <- NROW(ohlc)
  # Calculate VWAP prices
  vwap <- roll::roll_sum(closep*volumes, width=look_back, min_obs=1)
  volume_roll <- roll::roll_sum(volumes, width=look_back, min_obs=1)
  vwap <- vwap/volume_roll
  # Calculate the indicator
  indic <- trend*sign(close - vwap)
  if (lagg > 1) {
    indic <- roll::roll_sum(indic, width=lagg, min_obs=1)
    indic[1:lagg] <- 0
  }  # end if
  # Calculate positions, either: -1, 0, or 1
  pos <- rep(NA_integer_, nrows)
  pos[1] <- 0
  pos <- ifelse(indic == lagg, 1, pos)
  pos <- ifelse(indic == (-lagg), -1, pos)
  pos <- zoo::na.locf(pos, na.rm=FALSE)
  pos <- xts::xts(pos, order.by=index(close))
  # Lag the positions to trade on next day
  pos <- rutils::lagit(pos, lagg=1)
  # Calculate PnLs of strategy
  pnls <- returns*pos
  costs <- 0.5*bid_offer*abs(rutils::diffit(pos))*close
  pnls <- (pnls - costs)
  # Calculate strategy returns
  pnls <- cbind(pos, pnls)
  colnames(pnls) <- c("positions", "pnls")
  pnls
}  # end sim_vwap
source("/Users/jerzy/Develop/lecture_slides/scripts/ewma_model.R")
look_backs <- seq(70, 200, 10)
# Perform lapply() loop over lambdas
pnls <- lapply(look_backs, function(look_back) {
  # Simulate VWAP strategy and calculate returns
  sim_vwap(ohlc=ohlc, look_back=look_back, bid_offer=0, lagg=2)[, "pnls"]
})  # end lapply
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("look_back=", look_backs)
# Plot dygraph of multiple VWAP strategies
colors <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls["2007/"]), main="Cumulative Returns of Trend Following VWAP Strategies") %>%
  dyOptions(colors=colors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Plot VWAP strategies with custom line colors
x11(width=6, height=5)
plot_theme <- chart_theme()
plot_theme$col$line.col <- colors
quantmod::chart_Series(cumsum(pnls), theme=plot_theme,
  name="Cumulative Returns of VWAP Strategies")
legend("topleft", legend=colnames(pnls), inset=0.1,
  bg="white", cex=0.8, lwd=rep(6, NCOL(pnls)),
  col=plot_theme$col$line.col, bty="n")
# Calculate a vector of daily VTI log returns
vtis <- na.omit(rutils::etfenv$returns$VTI)
dates <- index(vtis)
vtis <- as.numeric(vtis)
nrows <- NROW(vtis)
# Define predictor matrix for forecasting
order_max <- 5
predictor <- sapply(1:order_max, rutils::lagit, input=vtis)
predictor <- cbind(rep(1, nrows), predictor)
colnames(predictor) <- paste0("pred_", 1:NCOL(predictor))
response <- vtis
# Calculate forecasts as function of the AR order
forecasts <- lapply(2:NCOL(predictor), function(ordern) {
  # Calculate fitted coefficients
  inverse <- MASS::ginv(predictor[, 1:ordern])
  coeff <- drop(inverse %*% response)
  # Calculate in-sample forecasts of vtis
  drop(predictor[, 1:ordern] %*% coeff)
})  # end lapply
names(forecasts) <- paste0("n=", 2:NCOL(predictor))
# Calculate mean squared errors
mse <- sapply(forecasts, function(x) {
  c(mse=mean((vtis - x)^2), cor=cor(vtis, x))
})  # end sapply
mse <- t(mse)
rownames(mse) <- names(forecasts)
# Plot forecasting MSE
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
plot(x=2:NCOL(predictor), y=mse[, 1],
  xlab="AR(n) order", ylab="MSE", type="l", lwd=2,
  main="MSE of In-sample AR(n) Forecasting Model for VTI")
in_sample <- 1:(nrows %/% 2)
out_sample <- (nrows %/% 2 + 1):nrows
# Calculate forecasts as function of the AR order
forecasts <- lapply(2:NCOL(predictor), function(ordern) {
  # Calculate fitted coefficients
  inverse <- MASS::ginv(predictor[in_sample, 1:ordern])
  coeff <- drop(inverse %*% response[in_sample])
  # Calculate out-of-sample forecasts of vtis
  drop(predictor[out_sample, 1:ordern] %*% coeff)
})  # end lapply
names(forecasts) <- paste0("n=", 2:NCOL(predictor))
# Calculate mean squared errors
mse <- sapply(forecasts, function(x) {
  c(mse=mean((vtis[out_sample] - x)^2), cor=cor(vtis[out_sample], x))
})  # end sapply
mse <- t(mse)
rownames(mse) <- names(forecasts)
# Plot forecasting MSE
plot(x=2:NCOL(predictor), y=mse[, 1],
  xlab="AR(n) order", ylab="MSE", type="l", lwd=2,
  main="MSE of Out-of-sample AR(n) Forecasting Model for VTI")
# Calculate out-of-sample PnLs
pnls <- sapply(forecasts, function(x) {
  cumsum(sign(x)*vtis[out_sample])
})  # end sapply
colnames(pnls) <- names(forecasts)
pnls <- xts::xts(pnls, dates[out_sample])
# Plot dygraph of out-of-sample PnLs
colorv <- colorRampPalette(c("red", "blue"))(NCOL(pnls[, 1:4]))
colnames <- colnames(pnls[, 1:4])
dygraphs::dygraph(pnls[, 1:4],
  main="Autoregressive Strategies Performance With Different Order Parameters") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(width=500)
# Define predictor as a rolling mean
nagg <- 5
predictor <- roll::roll_mean(vtis, width=nagg, min_obs=1)
response <- vtis
# Define predictor matrix for forecasting
predictor <- sapply(1+nagg*(0:order_max), rutils::lagit,
               input=predictor)
predictor <- cbind(rep(1, nrows), predictor)
# Calculate forecasts as function of the AR order
forecasts <- lapply(2:NCOL(predictor), function(ordern) {
  inverse <- MASS::ginv(predictor[in_sample, 1:ordern])
  coeff <- drop(inverse %*% response[in_sample])
  drop(predictor[out_sample, 1:ordern] %*% coeff)
})  # end lapply
names(forecasts) <- paste0("n=", 2:NCOL(predictor))
# Calculate out-of-sample PnLs
pnls <- sapply(forecasts, function(x) {
  cumsum(sign(x)*vtis[out_sample])
})  # end sapply
colnames(pnls) <- names(forecasts)
pnls <- xts::xts(pnls, dates[out_sample])
# Plot dygraph of out-of-sample PnLs
dygraphs::dygraph(pnls[, 1:4],
  main="Autoregressive Strategies Performance Using Rolling Average Predictor") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(width=500)
# Calculate PnLs using the average of past forecasts
nagg <- 5
pnls <- sapply(forecasts, function(x) {
  x <- roll::roll_mean(x, width=nagg, min_obs=1)
  cumsum(sign(x)*vtis[out_sample])
})  # end sapply
colnames(pnls) <- names(forecasts)
pnls <- xts::xts(pnls, dates[out_sample])
# Plot dygraph of out-of-sample PnLs
dygraphs::dygraph(pnls[, 1:4],
  main="Autoregressive Strategies Performance Using Rolling Average Forecasts") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(width=500)
library(rutils)
# Calculate a vector of daily VTI log returns
prices <- log(quantmod::Cl(rutils::etfenv$VTI))
vtis <- rutils::diffit(prices)
vtis <- as.numeric(vtis)
nrows <- NROW(vtis)
# Define predictor matrix for forecasting
order_max <- 5
design <- sapply(1:order_max, rutils::lagit, input=vtis)
colnames(design) <- paste0("pred_", 1:NCOL(design))
# Add response equal to vtis
design <- cbind(vtis, design)
colnames(design)[1] <- "response"
# Specify length of look-back interval
look_back <- 100
# Invert the predictor matrix
rangev <- (nrows-look_back):(nrows-1)
design_inv <- MASS::ginv(design[rangev, -1])
# Calculate fitted coefficients
coeff <- drop(design_inv %*% design[rangev, 1])
# Calculate forecast of vtis for nrows
drop(design[nrows, -1] %*% coeff)
# Compare with actual value
design[nrows, 1]
# Calculate a vector of daily VTI log returns
vtis <- na.omit(rutils::etfenv$returns$VTI)
dates <- index(vtis)
vtis <- as.numeric(vtis)
nrows <- NROW(vtis)
# Define response equal to vtis
response <- vtis
# Define predictor as a rolling sum
nagg <- 5
predictor <- rutils::roll_sum(vtis, look_back=nagg)
# Define predictor matrix for forecasting
order_max <- 5
predictor <- sapply(1+nagg*(0:order_max), rutils::lagit,
               input=predictor)
predictor <- cbind(rep(1, nrows), predictor)
# Perform rolling forecasting
look_back <- 100
forecasts <- sapply((look_back+1):nrows, function(endp) {
  # Define rolling look-back range
  startp <- max(1, endp-look_back)
  # Or expanding look-back range
  # startp <- 1
  rangev <- startp:(endp-1)
  # Invert the predictor matrix
  design_inv <- MASS::ginv(predictor[rangev, ])
  # Calculate fitted coefficients
  coeff <- drop(design_inv %*% response[rangev])
  # Calculate forecast
  drop(predictor[endp, ] %*% coeff)
})  # end sapply
# Add warmup period
forecasts <- c(rep(0, look_back), forecasts)
# Mean squared error
mean((vtis - forecasts)^2)
# Correlation
cor(forecasts, vtis)
# Plot forecasting series with legend
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0))
plot(vtis[(nrows-look_back):nrows], col="blue",
     xlab="", ylab="", type="l", lwd=2,
     main="Rolling Forecasting Using AR Model")
lines(forecasts[(nrows-look_back):nrows], col="red", lwd=2)
legend(x="top", legend=c("returns", "forecasts"),
 col=c("blue", "red"), lty=1, lwd=6,
 cex=0.9, bg="white", bty="n")
# Define backtesting function
sim_forecasts <- function(response, nagg=5,
                    ordern=5, look_back=100) {
  nrows <- NROW(response)
  # Define predictor as a rolling sum
  predictor <- rutils::roll_sum(response, look_back=nagg)
  # Define predictor matrix for forecasting
  predictor <- sapply(1+nagg*(0:ordern), rutils::lagit,
                 input=predictor)
  predictor <- cbind(rep(1, nrows), predictor)
  # Perform rolling forecasting
  forecasts <- sapply((look_back+1):nrows, function(endp) {
    # Define rolling look-back range
    startp <- max(1, endp-look_back)
    # Or expanding look-back range
    # startp <- 1
    rangev <- startp:(endp-1)
    # Invert the predictor matrix
    design_inv <- MASS::ginv(predictor[rangev, ])
    # Calculate fitted coefficients
    coeff <- drop(design_inv %*% response[rangev])
    # Calculate forecast
    drop(predictor[endp, ] %*% coeff)
  })  # end sapply
  # Add warmup period
  forecasts <- c(rep(0, look_back), forecasts)
  # Aggregate the forecasts
  rutils::roll_sum(forecasts, look_back=nagg)
}  # end sim_forecasts
# Simulate the rolling autoregressive forecasts
forecasts <- sim_forecasts(response=vtis, ordern=5, look_back=100)
c(mse=mean((vtis - forecasts)^2), cor=cor(vtis, forecasts))
library(parallel)  # Load package parallel
# Calculate number of available cores
ncores <- detectCores() - 1
# Initialize compute cluster under Windows
cluster <- makeCluster(ncores)
# clusterExport(cluster, c("startd", "barp"))
# Perform parallel loop under Windows
look_backs <- seq(20, 600, 40)
forecasts <- parLapply(cluster, look_backs, sim_forecasts,
  response=vtis, nagg=5, ordern=5)
# Perform parallel bootstrap under Mac-OSX or Linux
forecasts <- mclapply(look_backs, sim_forecasts, response=vtis,
  nagg=5, ordern=5, mc.cores=ncores)
# Calculate mean squared errors
mse <- sapply(forecasts, function(x) {
  c(mse=mean((vtis - x)^2), cor=cor(vtis, x))
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
# Calculate number of available cores
ncores <- detectCores() - 1
# Initialize compute cluster under Windows
cluster <- makeCluster(ncores)
# clusterExport(cluster, c("startd", "barp"))
# Perform parallel loop under Windows
forecasts <- parLapply(cluster, orders, sim_forecasts, response=vtis,
  nagg=5, look_back=look_back)
stopCluster(cluster)  # Stop R processes over cluster under Windows
# Perform parallel bootstrap under Mac-OSX or Linux
orders <- 2:6
forecasts <- mclapply(orders, sim_forecasts, response=vtis,
  nagg=5, look_back=look_back, mc.cores=ncores)
# Calculate mean squared errors
mse <- sapply(forecasts, function(x) {
  c(mse=mean((vtis - x)^2), cor=cor(vtis, x))
})  # end sapply
mse <- t(mse)
rownames(mse) <- orders
# Select optimal order parameter
ordern <- orders[which.min(mse[, 1])]
# Plot forecasting MSE
plot(x=orders, y=mse[, 1],
  xlab="ordern", ylab="MSE", type="l", lwd=2,
  main="MSE of Forecasting Model As Function of AR Order")
# Simulate the rolling autoregressive forecasts
forecasts <- sim_forecasts(vtis, ordern=ordern, look_back=look_back)
# Calculate strategy PnLs
pnls <- sign(forecasts)*vtis
pnls <- cbind(vtis, pnls, (vtis+pnls)/2)
colnames(pnls) <- c("VTI", "AR_Strategy", "Combined")
cor(pnls)
# Annualized Sharpe ratios of VTI and AR strategy
pnls <- xts::xts(pnls, dates)
sqrt(252)*sapply(pnls, function (x) mean(x)/sd(x))
# Plot the cumulative strategy PnLs
dygraphs::dygraph(cumsum(pnls), main="Rolling Autoregressive Strategy") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Calculate PnLs for ordern=5
forecasts <- sim_forecasts(vtis, ordern=5, look_back=look_back)
pnls5 <- cumsum(sign(forecasts)*vtis)
# Calculate PnLs for ordern=3
forecasts <- sim_forecasts(vtis, ordern=3, look_back=look_back)
pnls3 <- cumsum(sign(forecasts)*vtis)
# Plot the cumulative strategy returns
wealth <- cbind(pnls5, pnls3)
wealth <- xts::xts(wealth, dates)
colnames <- c("AR(5)_Strategy", "AR(3)_Strategy")
colnames(wealth) <- colnames
dygraphs::dygraph(wealth, main="Autoregressive Strategies for Different Order Parameters") %>%
  dySeries(name=colnames[1], label=colnames[1], col="blue", strokeWidth=2) %>%
  dySeries(name=colnames[2], label=colnames[2], col="red", strokeWidth=2) %>%
  dyLegend(width=500)
# Calculate PnLs for rolling look-back
forecasts <- sim_forecasts(vtis, ordern=3, look_back=look_back, is_rolling=TRUE)
pnls_rolling <- cumsum(sign(forecasts)*vtis)
# Calculate PnLs for expanding look-back
forecasts <- sim_forecasts(vtis, ordern=3, look_back=look_back, is_rolling=FALSE)
pnls_expanding <- cumsum(sign(forecasts)*vtis)
# Plot the cumulative strategy returns
wealth <- cbind(pnls_rolling, pnls_expanding)
wealth <- xts::xts(wealth, dates)
colnames <- c("AR(3)_Rolling", "AR(3)_Expanding")
colnames(wealth) <- colnames
dygraphs::dygraph(wealth, main="Autoregressive Strategies for Expanding Look-back Interval") %>%
  dySeries(name=colnames[1], label=colnames[1], col="blue", strokeWidth=2) %>%
  dySeries(name=colnames[2], label=colnames[2], col="red", strokeWidth=2) %>%
  dyLegend(width=500)
# Cap the VTI returns
cutoff <- 0.03
capped <- ifelse(vtis > cutoff, cutoff, vtis)
capped <- ifelse(capped < (-cutoff), -cutoff, capped)
# Calculate PnLs for vtis
forecasts <- sim_forecasts(vtis, ordern=3, look_back=look_back, is_rolling=FALSE)
pnls <- cumsum(sign(forecasts)*vtis)
# Calculate PnLs for capped VTI returns
forecasts <- sim_forecasts(capped, ordern=3, look_back=look_back, is_rolling=FALSE)
pnls_capped <- cumsum(sign(forecasts)*vtis)
# Plot the cumulative strategy returns
wealth <- cbind(pnls, pnls_capped)
wealth <- xts::xts(wealth, dates)
colnames <- c("AR(3)_Rolling", "AR(3)_Expanding")
colnames <- c("AR_Strategy", "AR_Strategy_Capped")
colnames(wealth) <- colnames
dygraphs::dygraph(wealth, main="Improved Autoregressive Strategies") %>%
  dySeries(name=colnames[1], label=colnames[1], col="blue", strokeWidth=2) %>%
  dySeries(name=colnames[2], label=colnames[2], col="red", strokeWidth=2) %>%
  dyLegend(width=500)
