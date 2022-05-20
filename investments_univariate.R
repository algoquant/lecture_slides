






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
vti <- rutils::etfenv$returns$VTI
vti <- na.omit(vti)
c(mean=mean(vti), std=sd(vti))
range(vti)

# Open x11 for plotting
x11(width=5, height=4)
# Set plot parameters to reduce whitespace around plot
par(mar=c(4, 4, 2, 1), oma=c(0, 0, 0, 0))
# Define vectorized logarithmic utility function
utili_ty <- function(kell_y, returns) {
  sapply(kell_y, function(x)
    sum(log(1 + x*returns)))
}  # end utili_ty
utili_ty(1, vti)
utili_ty(c(1, 4), vti)
# Plot the logarithmic utility
curve(expr=utili_ty(x, returns=vti),
xlim=c(0.1, 5), xlab="leverage", ylab="utility",
main="Utility of Asset Returns", lwd=2)

# Approximate Kelly leverage
mean(vti)/var(vti)
PerformanceAnalytics::KellyRatio(R=vti, method="full")
# Kelly leverage
unlist(optimize(
  f=function(x) -utili_ty(x, vti),
  interval=c(1, 4)))

# Calculate the VTI returns
vti <- rutils::etfenv$returns$VTI
vti <- na.omit(vti)
# Calculate wealth paths
kelly_ratio <- drop(mean(vti)/var(vti))
kelly_wealth <- cumprod(1 + kelly_ratio*vti)
hyper_kelly <- cumprod(1 + (kelly_ratio+2)*vti)
sub_kelly <- cumprod(1 + (kelly_ratio-2)*vti)
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
kelly_ratio <- drop(mean(vti)/var(vti))
wealth <- cumprod(1 + kelly_ratio*vti)
wealth_trans <- cumprod(1 + kelly_ratio*vti -
  0.5*bid_offer*kelly_ratio*(kelly_ratio-1)*abs(vti))
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
vti <- rutils::etfenv$returns$VTI
vti <- na.omit(vti)
# Calculate wealth paths
kelly_ratio <- drop(mean(vti)/var(vti))
kelly_wealth <- cumprod(1 + kelly_ratio*vti)
hyper_kelly <- cumprod(1 + (kelly_ratio+2)*vti)
sub_kelly <- cumprod(1 + (kelly_ratio-2)*vti)
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
vti <- rutils::etfenv$vti$VTI
vti <- na.omit(vti)
# Calculate higher moments of VTI returns
c(mean=sum(vti),
  variance=sum(vti^2),
  mom3=sum(vti^3),
  mom4=sum(vti^4))/NROW(vti)
# Calculate higher moments of minutely SPY returns
spy <- HighFreq::SPY[, 4]
spy <- na.omit(spy)
spy <- HighFreq::diffit(log(spy))
c(mean=sum(spy),
  variance=sum(spy^2),
  mom3=sum(spy^3),
  mom4=sum(spy^4))/NROW(spy)

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
weights <- sapply(returns, function(x) mean(x)/var(x))
# Kelly weight for stocks
unlist(optimize(f=function(x) utili_ty(x, w_b=0), interval=c(1, 4)))
# Kelly weight for bonds
unlist(optimize(f=function(x) utili_ty(x, w_s=0), interval=c(1, 14)))
# Vectorized utility of stock and bond portfolio
utility_vec <- function(weights) {
  utili_ty(weights[1], weights[2])
}  # end utility_vec
# Optimize with respect to vector argument
optiml <- optim(fn=utility_vec, par=c(3, 10),
          method="L-BFGS-B",
          upper=c(8, 20), lower=c(2, 5))
# Exact Kelly weights
optiml$par

# Approximate Kelly weights
p_rets <- (returns %*% weights)
drop(mean(p_rets)/var(p_rets))*weights
# Exact Kelly weights
optiml$par

# Quarter-Kelly sub-optimal weights
weights <- optiml$par/4
# Plot Kelly optimal portfolio
returns <- cbind(returns,
  weights[1]*returns$VTI + weights[2]*returns$IEF)
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
weights <- roll::roll_sum(returns, width=look_back)/look_back
weights <- weights/var_rolling
weights[1, ] <- 1/NCOL(weights)
weights <- zoo::na.locf(weights)
sum(is.na(weights))
range(weights)

# Plot the weights
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
weights <- lapply(weights,
  function(x) 10*x/sum(abs(range(x))))
weights <- do.call(cbind, weights)
weights <- rutils::lagit(weights)
# Calculate the compounded Kelly wealth and VTI
wealth <- cbind(cumprod(1 + weights$VTI*returns$VTI),
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
wealth <- cumprod(1 + weights$VTI*returns$VTI)
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
wealth <- cumprod(1 + rowSums(weights*returns))
wealth <- xts::xts(wealth, zoo::index(returns))
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

library(xtable)
gambl_e <- data.frame(win=c("p", "a"), lose=c("q = 1 - p", "-b"))
rownames(gambl_e) <- c("probability", "payout")
# print(xtable(gambl_e), comment=FALSE, size="tiny")
print(xtable(gambl_e), comment=FALSE)

library(HighFreq)
# Extract ETF prices from rutils::etfenv$prices
prices <- rutils::etfenv$prices
prices <- zoo::na.locf(prices, na.rm=FALSE)
prices <- zoo::na.locf(prices, fromLast=TRUE)
# Calculate simple dollar returns
rets_dollar <- rutils::diffit(prices)
# Or
# rets_dollar <- lapply(prices, rutils::diffit)
# rets_dollar <- rutils::do_call(cbind, rets_dollar)
# Calculate percentage returns
rets_percent <- rets_dollar/
  rutils::lagit(prices, lagg=1, pad_zeros=FALSE)
# Calculate log returns
rets_log <- rutils::diffit(log(prices))

# Set the initial dollar returns
rets_dollar[1, ] <- prices[1, ]
# Calculate prices from dollar returns
new_prices <- cumsum(rets_dollar)
all.equal(new_prices, prices)
# Compound the percentage returns
new_prices <- cumprod(1+rets_percent)
# Set the initial prices
init_prices <- as.numeric(prices[1, ])
new_prices <- lapply(1:NCOL(new_prices), function (i)
    init_prices[i]*new_prices[, i])
new_prices <- rutils::do_call(cbind, new_prices)
# Or
# new_prices <- t(t(new_prices)*init_prices)
all.equal(new_prices, prices, check.attributes=FALSE)
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
mar_gin <- cumsum(returns)
mar_gin <- (mar_gin - costs)
# dygraph plot of margin and transaction costs
datav <- cbind(mar_gin, costs)
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
rets_dollar <- rutils::diffit(prices)
# Calculate VTI and IEF percentage returns
rets_percent <- rets_dollar/
  rutils::lagit(prices, lagg=1, pad_zeros=FALSE)

# Wealth of fixed shares (without rebalancing)
weights <- c(0.5, 0.5)
init_prices <- as.numeric(prices[1, ])
wealth_fsa <- cumsum(rets_dollar %*% (weights/init_prices))
# Wealth of fixed dollars (with rebalancing)
wealth_fda <- cumsum(rets_percent %*% weights)
# Plot log wealth
wealth <- cbind(wealth_fda, wealth_fsa)
wealth <- xts::xts(wealth, zoo::index(prices))
colnames(wealth) <- c("Fixed dollars", "Fixed shares")
# Calculate Sharpe and Sortino ratios
sqrt(252)*sapply(rutils::diffit(wealth),
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
colnamev <- colnames(wealth)
dygraphs::dygraph(wealth, main="Wealth of Weighted Portfolios") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="red", strokeWidth=2) %>%
  dySeries(name=colnamev[2], axis="y2", col="blue", strokeWidth=2) %>%
  dyLegend(show="always", width=500)

# Margin account for fixed dollars (with rebalancing)
mar_gin <- cumsum(rets_percent %*% weights)
# Cumulative transaction costs
costs <- bid_offer*cumsum(abs(rets_percent) %*% weights)/2
# Subtract transaction costs from margin account
mar_gin <- (mar_gin - costs)
# dygraph plot of margin and transaction costs
datav <- cbind(mar_gin, costs)
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
wealth_fsa <- cumsum(rets_dollar %*% (weights/init_prices))
# Or compound the percentage returns
wealth_fsa <- drop(apply(rets_percent, 2, function(x) cumprod(1+x)) %*% weights)-1
# Wealth of proportional allocations (with rebalancing)
wealth_pda <- cumprod(1 + rets_percent %*% weights) - 1
# Plot log wealth
wealth <- cbind(wealth_fsa, wealth_pda)
wealth <- xts::xts(wealth, zoo::index(prices))
colnames(wealth) <- c("Fixed Shares", "Proportional Allocations")
dygraphs::dygraph(wealth, main="Wealth of Proportional Allocations") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)

# Returns in excess of weighted returns
rets_weighted <- rets_percent %*% weights
excess <- lapply(rets_percent, function(x) (rets_weighted - x))
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
wealth_fsa <- drop(apply(rets_percent, 2, function(x) cumprod(1+x)) %*% weights)-1
# Wealth of proportional dollar allocations (with rebalancing)
wealth_pda <- cumprod(1 + rets_percent %*% weights) - 1
# Wealth of proportional target allocation (with rebalancing)
rets_percent <- zoo::coredata(rets_percent)
threshold <- 0.05
wealth <- matrix(nrow=NROW(rets_percent), ncol=2)
colnames(wealth) <- colnames(rets_percent)
wealth[1, ] <- weights
for (it in 2:NROW(rets_percent)) {
  # Accrue wealth without rebalancing
  wealth[it, ] <- wealth[it-1, ]*(1 + rets_percent[it, ])
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

# Calculate standardized simple dollar returns
rets_dollar_std <- lapply(rets_dollar, function(x) x/sd(x))
rets_dollar_std <- do.call(cbind, rets_dollar_std)
sapply(rets_dollar_std, sd)
# Wealth of fixed number of shares (without rebalancing)
weights <- c(0.5, 0.5)
wealth_fsa <- cumsum(rets_dollar %*% (weights/init_prices))
# Calculate standardized percentage returns
rets_percent_std <- lapply(rets_percent, function(x) x/sd(x))
rets_percent_std <- do.call(cbind, rets_percent_std)
sapply(rets_percent_std, sd)
# Wealth of fixed dollar amount of shares (with rebalancing)
wealth_fda <- cumsum(rets_percent_std %*% weights)
# Plot log wealth
wealth <- cbind(wealth_fda, log(wealth_fsa))
# wealth <- xts::xts(wealth, zoo::index(prices))
colnames(wealth) <- c("With rebalancing", "Without rebalancing")
dygraphs::dygraph(wealth, main="Wealth of Equal Dollar Amount of Shares") %>%
  dyOptions(colors=c("green", "blue"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)

# Calculate VTI returns
returns <- na.omit(rutils::etfenv$returns$VTI["2008/2009"])
dates <- zoo::index(returns)
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
rets_dollar <- rutils::diffit(prices)
rets_percent <- rets_dollar/rutils::lagit(prices, lagg=1, pad_zeros=FALSE)
# Calculate wealth of proportional allocations.
weights <- c(0.5, 0.5)
rets_weighted <- rets_percent %*% weights
wealth_pda <- cumprod(1 + rets_weighted)
# Calculate rolling percentage volatility.
look_back <- 21
volat <- roll::roll_sd(rets_percent, width=look_back)
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
rets_weighted <- rowSums(rets_percent*alloc)
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
vti <- rets_percent$VTI
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

# Returns in excess of weighted returns
excess <- lapply(rets_percent, function(x) (rets_weighted - x))
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

# Calculate positions
vti <- na.omit(rutils::etfenv$returns$VTI)
posit <- rep(NA_integer_, NROW(vti))
dates <- zoo::index(vti)
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

# Simulate stop-loss strategy
stopl <- 0.05
maxp <- 0.0
minp <- 0.0
cumrets <- 0.0
pnls <- vti
for (i in 1:(nrows-1)) {
# Calculate drawdown
  cumrets <- cumrets + vti[i]
  maxp <- max(maxp, cumrets)
  dd <- (cumrets - maxp)
# Check for stop-loss
  if (dd < -stopl*maxp) {
    pnls[i+1] <- 0
    minp <- min(minp, cumrets)
    du <- (cumrets - minp)
# Check for gain
    if (du > stopl*minp) {
pnls[i+1] <- vti[i+1]
    }  # end if
  } else {
    minp <- cumrets
  }  # end if
}  # end for

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
colnamev <- colnames(prices)
dygraphs::dygraph(prices["2009"], main="VTI Price Z-Scores") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=2, col="red")

# Calculate thresholds for labeling tops and bottoms
threshv <- quantile(pricescores, c(0.1, 0.9))
# Calculate the vectors of tops and bottoms
tops <- zoo::coredata(pricescores > threshv[2])
colnames(tops) <- "tops"
bottoms <- zoo::coredata(pricescores < threshv[1])
colnames(bottoms) <- "bottoms"
# Simulate in-sample VTI strategy
posit <- rep(NA_integer_, NROW(returns))
posit[1] <- 0
posit[tops] <- (-1)
posit[bottoms] <- 1
posit <- zoo::na.locf(posit)
posit <- rutils::lagit(posit)
pnls <- returns*posit

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
design <- cbind(tops, intercept=rep(1, NROW(tops)),
           volat_scores, volume_scores)
# Define regression formula
colnamev <- colnames(design)
formulav <- as.formula(paste(paste(colnamev[1],
  paste(colnamev[-1], collapse="+"), sep=" ~ "), "-1"))
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
plot(x=predictv[ordern], y=tops[ordern],
     main="Logistic Regression of Stock Tops",
     col="orange", xlab="predictor", ylab="top")
lines(x=predictv[ordern], y=glmod$fitted.values[ordern], col="blue", lwd=3)
legend(x="topleft", inset=0.1, bty="n", lwd=6,
 legend=c("tops", "logit fitted values"),
 col=c("orange", "blue"), lty=c(NA, 1), pch=c(1, NA))

# Define discrimination threshold value
threshold <- quantile(forecasts, 0.95)
# Calculate confusion matrix in-sample
confmat <- table(actual=!tops, forecast=(forecasts < threshold))
confmat
# Calculate FALSE positive (type I error)
sum(!tops & (forecasts > threshold))
# Calculate FALSE negative (type II error)
sum(tops & (forecasts < threshold))

# Calculate FALSE positive and FALSE negative rates
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
confun <- function(actual, forecasts, threshold) {
    conf <- table(actual, (forecasts < threshold))
    conf <- conf / rowSums(conf)
    c(typeI=conf[2, 1], typeII=conf[1, 2])
  }  # end confun
confun(!tops, forecasts, threshold=threshold)
# Define vector of discrimination thresholds
threshv <- quantile(forecasts, seq(0.1, 0.99, by=0.01))
# Calculate error rates
error_rates <- sapply(threshv, confun,
  actual=!tops, forecasts=forecasts)  # end sapply
error_rates <- t(error_rates)
rownames(error_rates) <- threshv
# Calculate the informedness
inform_ed <- 2 - rowSums(error_rates[, c("typeI", "typeII")])
plot(threshv, inform_ed, t="l", main="Informedness")
# Find the threshold corresponding to highest informedness
threshold_top <- threshv[which.max(inform_ed)]
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
design <- cbind(bottoms, intercept=rep(1, NROW(bottoms)),
           volat_scores, volume_scores)
# Define regression formula
colnamev <- colnames(design)
formulav <- as.formula(paste(paste(colnamev[1],
  paste(colnamev[-1], collapse="+"), sep=" ~ "), "-1"))
# Fit in-sample logistic regression for tops
glmod <- glm(formulav, data=design, family=binomial(logit))
summary(glmod)
# Calculate in-sample forecast from logistic regression model
predictv <- drop(design[, -1] %*% glmod$coefficients)
forecasts <- 1/(1+exp(-predictv))
# Calculate error rates
error_rates <- sapply(threshv, confun,
  actual=!bottoms, forecasts=forecasts)  # end sapply
error_rates <- t(error_rates)
rownames(error_rates) <- threshv
# Calculate the informedness
inform_ed <- 2 - rowSums(error_rates[, c("typeI", "typeII")])
plot(threshv, inform_ed, t="l", main="Informedness")
# Find the threshold corresponding to highest informedness
threshold_bottom <- threshv[which.max(inform_ed)]
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
posit <- rep(NA_integer_, NROW(returns))
posit[1] <- 0
posit[tops_forecast] <- (-1)
posit[bottoms_forecast] <- 1
posit <- zoo::na.locf(posit)
posit <- rutils::lagit(posit)
pnls <- returns*posit

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
medianv <- roll::roll_median(closep, width=look_back)
# medianv <- TTR::runMedian(closep, n=look_back)
# Calculate time series of MAD
madv <- HighFreq::roll_var(closep, look_back=look_back, method="nonparametric")
# madv <- TTR::runMAD(closep, n=look_back)
# Calculate time series of z-scores
zscores <- (closep - medianv)/madv
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
ohlc <- log(rutils::etfenv$VTI)
closep <- quantmod::Cl(ohlc)
colnames(closep) <- "VTI"
nrows <- NROW(closep)
# Calculate EWMA weights
look_back <- 333
lambda <- 0.9
weights <- lambda^(1:look_back)
weights <- weights/sum(weights)
# Calculate EWMA prices as the convolution
ewmap <- HighFreq::roll_wsum(closep, weights=weights)
prices <- cbind(closep, ewmap)
colnames(prices) <- c("VTI", "VTI EWMA")

# Dygraphs plot with custom line colors
colnamev <- colnames(prices)
dygraphs::dygraph(prices["2009"], main="VTI EWMA Prices") %>%
  dySeries(name=colnamev[1], label=colnamev[1], strokeWidth=1, col="blue") %>%
  dySeries(name=colnamev[2], label=colnamev[2], strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=500)
# Standard plot of  EWMA prices with custom line colors
x11(width=6, height=5)
plot_theme <- chart_theme()
colors <- c("blue", "red")
plot_theme$col$line.col <- colors
quantmod::chart_Series(prices["2009"], theme=plot_theme,
       lwd=2, name="VTI EWMA Prices")
legend("topleft", legend=colnames(prices),
 inset=0.1, bg="white", lty=1, lwd=6, cex=0.8,
 col=plot_theme$col$line.col, bty="n")

# Calculate EWMA prices recursively using C++ code
ewma_rfilter <- .Call(stats:::C_rfilter, closep, lambda, c(as.numeric(closep[1])/(1-lambda), double(NROW(closep))))[-1]
# Or R code
# ewma_rfilter <- filter(closep, filter=lambda, init=as.numeric(closep[1, 1])/(1-lambda), method="recursive")
ewma_rfilter <- (1-lambda)*ewma_rfilter
# Calculate EWMA prices recursively using RcppArmadillo
ewmap <- HighFreq::run_mean(closep, lambda=lambda)
all.equal(drop(ewmap), ewma_rfilter)
# Compare the speed of C++ code with RcppArmadillo
library(microbenchmark)
summary(microbenchmark(
  run_mean=HighFreq::run_mean(closep, lambda=lambda),
  rfilter=.Call(stats:::C_rfilter, closep, lambda, c(as.numeric(closep[1])/(1-lambda), double(NROW(closep)))),
  times=10))[, c(1, 4, 5)]

# Dygraphs plot with custom line colors
prices <- cbind(closep, ewmap)
colnames(prices) <- c("VTI", "VTI EWMA")
colnamev <- colnames(prices)
dygraphs::dygraph(prices["2009"], main="Recursive VTI EWMA Prices") %>%
  dySeries(name=colnamev[1], label=colnamev[1], strokeWidth=1, col="blue") %>%
  dySeries(name=colnamev[2], label=colnamev[2], strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=500)
# Standard plot of  EWMA prices with custom line colors
x11(width=6, height=5)
plot_theme <- chart_theme()
colors <- c("blue", "red")
plot_theme$col$line.col <- colors
quantmod::chart_Series(prices["2009"], theme=plot_theme,
       lwd=2, name="VTI EWMA Prices")
legend("topleft", legend=colnames(prices),
 inset=0.1, bg="white", lty=1, lwd=6, cex=0.8,
 col=plot_theme$col$line.col, bty="n")

# Calculate positions, either: -1, 0, or 1
indic <- sign(closep - ewmap)
posit <- rutils::lagit(indic, lagg=1)
# Create colors for background shading
dates <- (rutils::diffit(posit) != 0)
shad_e <- posit[dates]
dates <- c(zoo::index(shad_e), end(posit))
shad_e <- ifelse(drop(zoo::coredata(shad_e)) == 1, "lightgreen", "antiquewhite")
# Create dygraph object without plotting it
dyplot <- dygraphs::dygraph(prices["2007/"], main="VTI EWMA Prices") %>%
  dySeries(name=colnamev[1], label=colnamev[1], strokeWidth=1, col="blue") %>%
  dySeries(name=colnamev[2], label=colnamev[2], strokeWidth=4, col="red") %>%
  dyLegend(show="always", width=500)
# Add shading to dygraph object
for (i in 1:NROW(shad_e)) {
    dyplot <- dyplot %>% dyShading(from=dates[i], to=dates[i+1], color=shad_e[i])
}  # end for
# Plot the dygraph object
dyplot
# Equivalent code to the above
# Determine trade dates right after EWMA has crossed prices
indic <- sign(closep - ewmap)
dates <- (rutils::diffit(indic) != 0)
dates <- which(dates) + 1
dates <- dates[dates <.n_rows]
# Calculate positions, either: -1, 0, or 1
posit <- rep(NA_integer_, nrows)
posit[1] <- 0
posit[dates] <- indic[dates-1]
posit <- zoo::na.locf(posit, na.rm=FALSE)
posit <- xts::xts(posit, order.by=zoo::index(closep))
# Create indicator for background shading
shad_e <- posit[dates]
dates <- zoo::index(shad_e)
dates <- c(dates, end(posit))
shad_e <- ifelse(drop(zoo::coredata(shad_e)) == 1, "lightgreen", "antiquewhite")

# Standard plot of EWMA prices with position shading
x11(width=6, height=5)
quantmod::chart_Series(prices["2007/"], theme=plot_theme,
       lwd=2, name="VTI EWMA Prices")
add_TA(posit > 0, on=-1, col="lightgreen", border="lightgreen")
add_TA(posit < 0, on=-1, col="lightgrey", border="lightgrey")
legend("topleft", legend=colnames(prices),
 inset=0.1, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")

# Calculate daily profits and losses of EWMA strategy
vti <- rutils::diffit(closep)  # VTI returns
pnls <- vti*posit
colnames(pnls) <- "EWMA"
wealth <- cbind(vti, pnls)
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
add_TA(posit > 0, on=-1, col="lightgreen", border="lightgreen")
add_TA(posit < 0, on=-1, col="lightgrey", border="lightgrey")
legend("top", legend=colnames(wealth),
 inset=0.05, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")

# Test EWMA crossover market timing of VTI using Treynor-Mazuy test
design <- cbind(pnls, vti, vti^2)
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
fittedv <- (model$coeff["(Intercept)"] +
        model$coeff["treynor"]*vti^2)
points.default(x=design$VTI, y=fittedv, pch=16, col="red")
text(x=0.05, y=0.8*max(residuals), paste("EWMA crossover t-value =", round(summary(model)$coeff["treynor", "t value"], 2)))

# Determine trade dates right after EWMA has crossed prices
indic <- sign(closep - ewmap)
# Calculate positions from lagged indicator
lagg <- 2
indic <- roll::roll_sum(indic, width=lagg, min_obs=1)
# Calculate positions, either: -1, 0, or 1
posit <- rep(NA_integer_, nrows)
posit[1] <- 0
posit <- ifelse(indic == lagg, 1, posit)
posit <- ifelse(indic == (-lagg), -1, posit)
posit <- zoo::na.locf(posit, na.rm=FALSE)
posit <- xts::xts(posit, order.by=zoo::index(closep))
# Lag the positions to trade in next period
posit <- rutils::lagit(posit, lagg=1)
# Calculate PnLs of lagged strategy
pnls_lag <- vti*posit
colnames(pnls_lag) <- "Lagged Strategy"

wealth <- cbind(pnls, pnls_lag)
colnames(wealth) <- c("EWMA Strategy", "Lagged Strategy")
# Annualized Sharpe ratios of EWMA strategies
sharper <- sqrt(252)*sapply(wealth, function (x) mean(x)/sd(x))
# Plot both strategies
dygraphs::dygraph(cumsum(wealth["2007/"]), main=paste("EWMA Crossover Strategy, Sharpe", paste(paste(names(sharper), round(sharper, 3), sep="="), collapse=", "))) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=1) %>%
  dyLegend(show="always", width=500)

# Calculate positions, either: -1, 0, or 1
indic <- sign(closep - ewmap)
posit <- rutils::lagit(indic, lagg=1)
# Calculate daily pnl for days without trades
pnls_lag <- vti*posit
# Determine trade dates right after EWMA has crossed prices
dates <- which(rutils::diffit(posit) != 0)
# Calculate realized pnl for days with trades
openp <- quantmod::Op(ohlc)
close_lag <- rutils::lagit(closep)
pos_lag <- rutils::lagit(posit)
pnls_lag[dates] <- pos_lag[dates]*
  (openp[dates] - close_lag[dates])
# Calculate unrealized pnl for days with trades
pnls_lag[dates] <- pnls_lag[dates] +
  posit[dates]*(closep[dates] - openp[dates])
# Calculate the wealth
wealth <- cbind(vti, pnls_lag)
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
costs <- 0.5*bid_offer*abs(pos_lag - posit)*closep
# Plot strategy with transaction costs
wealth <- cbind(pnls, pnls - costs)
colnames(wealth) <- c("EWMA", "EWMA w Costs")
colors <- c("blue", "red")
dygraphs::dygraph(cumsum(wealth["2007/"]), main="EWMA Strategy With Transaction Costs") %>%
  dyOptions(colors=colors, strokeWidth=2) %>%
  dyLegend(show="always", width=500)

sim_ewma <- function(ohlc, lambda=0.01, look_back=333, bid_offer=0.001,
                trend=1, lagg=1) {
  close <- quantmod::Cl(ohlc)
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
  pos <- xts::xts(pos, order.by=zoo::index(close))
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
weights <- exp(-lambda*(1:look_back))
weights <- weights/sum(weights)
plot(weights, t="l", xlab="days", ylab="weights",
     main="Optimal Weights of EWMA Trend Following Strategy")
trend_returns <- pnls
trend_sharpe <- sharper

# Simulate best performing strategy
ewma_trend <- sim_ewma(ohlc=ohlc, lambda=lambda, look_back=look_back, bid_offer=0, lagg=2)
posit <- ewma_trend[, "positions"]
pnls <- ewma_trend[, "pnls"]
wealth <- cbind(vti, pnls)
colnames(wealth) <- c("VTI", "EWMA PnL")
# Create colors for background shading
dates <- (rutils::diffit(posit) != 0)
shad_e <- posit[dates]
dates <- c(zoo::index(shad_e), end(posit))
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
add_TA(posit > 0, on=-1, col="lightgreen", border="lightgreen")
add_TA(posit < 0, on=-1, col="lightgrey", border="lightgrey")
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
columnv <- seq(1, NCOL(pnls), by=4)
colors <- colorRampPalette(c("blue", "red"))(NROW(columnv))
dygraphs::dygraph(cumsum(pnls["2007/", columnv]), main="Cumulative Returns of Mean Reverting EWMA Strategies") %>%
  dyOptions(colors=colors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Plot EWMA strategies with custom line colors
x11(width=6, height=5)
plot_theme <- chart_theme()
plot_theme$col$line.col <- colors
quantmod::chart_Series(pnls[, columnv],
  theme=plot_theme, name="Cumulative Returns of Mean Reverting EWMA Strategies")
legend("topleft", legend=colnames(pnls[, columnv]),
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
posit <- ewma_revert[, "positions"]
pnls <- ewma_revert[, "pnls"]
wealth <- cbind(vti, pnls)
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
add_TA(posit > 0, on=-1, col="lightgreen", border="lightgreen")
add_TA(posit < 0, on=-1, col="lightgrey", border="lightgrey")
legend("top", legend=colnames(wealth),
 inset=0.05, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")

# Calculate correlation between trend following and mean reverting strategies
trend_ing <- ewma_trend[, "pnls"]
colnames(trend_ing) <- "trend"
revert_ing <- ewma_revert[, "pnls"]
colnames(revert_ing) <- "revert"
cor(cbind(vti, trend_ing, revert_ing))
# Calculate combined strategy
com_bined <- (vti + trend_ing + revert_ing)/3
colnames(com_bined) <- "combined"
# Calculate annualized Sharpe ratio of strategy returns
returns <- cbind(vti, trend_ing, revert_ing, com_bined)
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

weights <- c(trend_sharpe, revert_sharpe)
weights[weights<0] <- 0
weights <- weights/sum(weights)
returns <- cbind(trend_returns, revert_returns)
returns <- returns %*% weights
returns <- xts::xts(returns, order.by=zoo::index(vti))
returns <- cbind(vti, returns)
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
weights <- exp(-lambda1*(1:look_back))
weights <- weights/sum(weights)
ewma1 <- HighFreq::roll_wsum(closep, weights=weights)
weights <- exp(-lambda2*(1:look_back))
weights <- weights/sum(weights)
ewma2 <- HighFreq::roll_wsum(closep, weights=weights)
# Calculate EWMA prices
prices <- cbind(closep, ewma1, ewma2)
colnames(prices) <- c("VTI", "EWMA fast", "EWMA slow")
# Calculate positions, either: -1, 0, or 1
indic <- sign(ewma1 - ewma2)
lagg <- 2
indic <- roll::roll_sum(indic, width=lagg, min_obs=1)
posit <- rep(NA_integer_, nrows)
posit[1] <- 0
posit <- ifelse(indic == lagg, 1, posit)
posit <- ifelse(indic == (-lagg), -1, posit)
posit <- zoo::na.locf(posit, na.rm=FALSE)
posit <- xts::xts(posit, order.by=zoo::index(closep))
posit <- rutils::lagit(posit, lagg=1)

# Create colors for background shading
dates <- (rutils::diffit(posit) != 0)
shad_e <- posit[dates]
dates <- c(zoo::index(shad_e), end(posit))
shad_e <- ifelse(drop(zoo::coredata(shad_e)) == 1, "lightgreen", "antiquewhite")
# Plot dygraph
colnamev <- colnames(prices)
dyplot <- dygraphs::dygraph(prices["2007/"], main="VTI Dual EWMA Prices") %>%
  dySeries(name=colnamev[1], label=colnamev[1], strokeWidth=1, col="blue") %>%
  dySeries(name=colnamev[2], label=colnamev[2], strokeWidth=4, col="red") %>%
  dySeries(name=colnamev[3], label=colnamev[3], strokeWidth=4, col="purple") %>%
  dyLegend(show="always", width=500)
for (i in 1:NROW(shad_e)) {
    dyplot <- dyplot %>% dyShading(from=dates[i], to=dates[i+1], color=shad_e[i])
}  # end for
dyplot

# Calculate daily profits and losses of strategy
pnls <- vti*posit
colnames(pnls) <- "Strategy"
wealth <- cbind(vti, pnls)
# Annualized Sharpe ratio of Dual EWMA strategy
sharper <- sqrt(252)*sapply(wealth, function (x) mean(x)/sd(x))
# The crossover strategy has a negative correlation to VTI
cor(wealth)

# Plot Dual EWMA strategy
dyplot <- dygraphs::dygraph(cumsum(wealth["2007/"]), main=paste("EWMA Dual Crossover Strategy, Sharpe", paste(paste(names(sharper), round(sharper, 3), sep="="), collapse=", "))) %>%
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
  pos <- xts::xts(pos, order.by=zoo::index(close))
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
wealth <- cbind(vti, pnls)

# Annualized Sharpe ratio of Dual EWMA strategy
sharper <- sqrt(252)*sapply(wealth, function (x) mean(x)/sd(x))
# The crossover strategy has a negative correlation to VTI
cor(wealth)
# Plot Optimal Dual EWMA strategy
dyplot <- dygraphs::dygraph(cumsum(wealth["2007/"]), main=paste("Optimal EWMA Dual Crossover Strategy, Sharpe", paste(paste(names(sharper), round(sharper, 3), sep="="), collapse=", "))) %>%
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
look_back <- 21
vwap <- roll::roll_sum(closep*volumes, width=look_back, min_obs=1)
volume_roll <- roll::roll_sum(volumes, width=look_back, min_obs=1)
vwap <- vwap/volume_roll
colnames(vwap) <- "VWAP"
prices <- cbind(closep, vwap)

# Dygraphs plot with custom line colors
colors <- c("blue", "red")
dygraphs::dygraph(prices["2009"], main="VTI VWAP Prices") %>%
  dyOptions(colors=colors, strokeWidth=2)
# Plot VWAP prices with custom line colors
x11(width=6, height=5)
plot_theme <- chart_theme()
plot_theme$col$line.col <- colors
quantmod::chart_Series(prices["2009"], theme=plot_theme,
       lwd=2, name="VTI VWAP Prices")
legend("bottomright", legend=colnames(prices),
 inset=0.1, bg="white", lty=1, lwd=6, cex=0.8,
 col=plot_theme$col$line.col, bty="n")

# Calculate VWAP prices recursively using C++ code
volume_rec <- .Call(stats:::C_rfilter, volumes, lambda, c(as.numeric(volumes[1])/(1-lambda), double(NROW(volumes))))[-1]
price_rec <- .Call(stats:::C_rfilter, volumes*closep, lambda, c(as.numeric(volumes[1]*closep[1])/(1-lambda), double(NROW(closep))))[-1]
vwap_rec <- price_rec/volume_rec
# Calculate VWAP prices recursively using RcppArmadillo
vwap_arma <- HighFreq::run_mean(closep, lambda=lambda, weights=volumes)
all.equal(vwap_rec, drop(vwap_arma))
# Dygraphs plot the VWAP prices
prices <- xts(cbind(vwap, vwap_arma), zoo::index(ohlc))
colnames(prices) <- c("VWAP rolling", "VWAP running")
dygraphs::dygraph(prices["2009"], main="VWAP Prices") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)

# Calculate positions from lagged indicator
indic <- sign(closep - vwap)
lagg <- 2
indic <- roll::roll_sum(indic, width=lagg, min_obs=1)
# Calculate positions, either: -1, 0, or 1
posit <- rep(NA_integer_, nrows)
posit[1] <- 0
posit <- ifelse(indic == lagg, 1, posit)
posit <- ifelse(indic == (-lagg), -1, posit)
posit <- zoo::na.locf(posit, na.rm=FALSE)
posit <- xts::xts(posit, order.by=zoo::index(closep))
# Lag the positions to trade in next period
posit <- rutils::lagit(posit, lagg=1)
# Calculate PnLs of VWAP strategy
vti <- rutils::diffit(closep)  # VTI returns
pnls <- vti*posit
colnames(pnls) <- "VWAP Strategy"
wealth <- cbind(vti, pnls)
colnames(wealth) <- c("VTI", "VWAP Strategy")
colnamev <- colnames(wealth)
# Annualized Sharpe ratios of VTI and VWAP strategy
sharper <- sqrt(252)*sapply(wealth, function (x) mean(x)/sd(x))

# Create colors for background shading
dates <- (rutils::diffit(posit) != 0)
shad_e <- posit[dates]
dates <- c(zoo::index(shad_e), end(posit))
shad_e <- ifelse(drop(zoo::coredata(shad_e)) == 1, "lightgreen", "antiquewhite")
# Plot dygraph of VWAP strategy
# Create dygraph object without plotting it
dyplot <- dygraphs::dygraph(cumsum(wealth["2007/"]), main=paste("VWAP Crossover Strategy, Sharpe", paste(paste(names(sharper), round(sharper, 3), sep="="), collapse=", "))) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Add shading to dygraph object
for (i in 1:NROW(shad_e)) {
    dyplot <- dyplot %>% dyShading(from=dates[i], to=dates[i+1], color=shad_e[i])
}  # end for
# Plot the dygraph object
dyplot

# Calculate correlation of VWAP strategy with VTI
cor(vti, pnls)
# Combine VWAP strategy with VTI
wealth <- cbind(vti, pnls, 0.5*(vti+pnls))
colnames(wealth) <- c("VTI", "VWAP", "Combined")
sharper <- sqrt(252)*sapply(wealth, function (x) mean(x)/sd(x))

# Plot dygraph of VWAP strategy combined with VTI
# wippp
colnamev <- colnames(wealth)
dygraphs::dygraph(cumsum(wealth), paste("VWAP Crossover Strategy, Sharpe", paste(paste(names(sharper), round(sharper, 3), sep="="), collapse=", "))) %>%
  dySeries(name=colnamev[1], label=colnamev[1], col="blue", strokeWidth=1) %>%
  dySeries(name=colnamev[2], label=colnamev[2], col="red", strokeWidth=1) %>%
  dySeries(name=colnamev[3], label=colnamev[3], col="purple", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Or
dygraphs::dygraph(cumsum(wealth),
  main=paste("VWAP Crossover Strategy, Sharpe", paste(paste(names(sharper), round(sharper, 3), sep="="), collapse=", "))) %>%
  dyOptions(colors=c("blue", "red", "purple"), strokeWidth=1) %>%
  dyLegend(show="always", width=500)

# Test VWAP crossover market timing of VTI using Treynor-Mazuy test
design <- cbind(pnls, vti, vti^2)
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
fittedv <- (model$coeff["(Intercept)"] + model$coeff["treynor"]*vti^2)
points.default(x=design$VTI, y=fittedv, pch=16, col="red")
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
  pos <- xts::xts(pos, order.by=zoo::index(close))
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
vti <- na.omit(rutils::etfenv$returns$VTI)
dates <- zoo::index(vti)
vti <- as.numeric(vti)
nrows <- NROW(vti)
# Define predictor matrix for forecasting
order_max <- 5
predictor <- sapply(1:order_max, rutils::lagit, input=vti)
predictor <- cbind(rep(1, nrows), predictor)
colnames(predictor) <- paste0("pred_", 1:NCOL(predictor))
response <- vti
# Calculate forecasts as function of the AR order
forecasts <- lapply(2:NCOL(predictor), function(ordern) {
  # Calculate fitted coefficients
  inverse <- MASS::ginv(predictor[, 1:ordern])
  coeff <- drop(inverse %*% response)
  # Calculate in-sample forecasts of vti
  drop(predictor[, 1:ordern] %*% coeff)
})  # end lapply
names(forecasts) <- paste0("n=", 2:NCOL(predictor))

# Calculate mean squared errors
mse <- sapply(forecasts, function(x) {
  c(mse=mean((vti - x)^2), cor=cor(vti, x))
})  # end sapply
mse <- t(mse)
rownames(mse) <- names(forecasts)
# Plot forecasting MSE
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
plot(x=2:NCOL(predictor), y=mse[, 1],
  xlab="AR(n) order", ylab="MSE", type="l", lwd=2,
  main="MSE of In-sample AR(n) Forecasting Model for VTI")

insample <- 1:(nrows %/% 2)
outsample <- (nrows %/% 2 + 1):nrows
# Calculate forecasts as function of the AR order
forecasts <- lapply(2:NCOL(predictor), function(ordern) {
  # Calculate fitted coefficients
  inverse <- MASS::ginv(predictor[insample, 1:ordern])
  coeff <- drop(inverse %*% response[insample])
  # Calculate out-of-sample forecasts of vti
  drop(predictor[outsample, 1:ordern] %*% coeff)
})  # end lapply
names(forecasts) <- paste0("n=", 2:NCOL(predictor))

# Calculate mean squared errors
mse <- sapply(forecasts, function(x) {
  c(mse=mean((vti[outsample] - x)^2), cor=cor(vti[outsample], x))
})  # end sapply
mse <- t(mse)
rownames(mse) <- names(forecasts)
# Plot forecasting MSE
plot(x=2:NCOL(predictor), y=mse[, 1],
  xlab="AR(n) order", ylab="MSE", type="l", lwd=2,
  main="MSE of Out-of-sample AR(n) Forecasting Model for VTI")

# Calculate out-of-sample PnLs
pnls <- sapply(forecasts, function(x) {
  cumsum(sign(x)*vti[outsample])
})  # end sapply
colnames(pnls) <- names(forecasts)
pnls <- xts::xts(pnls, dates[outsample])

# Plot dygraph of out-of-sample PnLs
colorv <- colorRampPalette(c("red", "blue"))(NCOL(pnls[, 1:4]))
colnamev <- colnames(pnls[, 1:4])
dygraphs::dygraph(pnls[, 1:4],
  main="Autoregressive Strategies Performance With Different Order Parameters") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(width=500)

# Define predictor as a rolling mean
nagg <- 5
predictor <- roll::roll_mean(vti, width=nagg, min_obs=1)
response <- vti
# Define predictor matrix for forecasting
predictor <- sapply(1+nagg*(0:order_max), rutils::lagit,
               input=predictor)
predictor <- cbind(rep(1, nrows), predictor)
# Calculate forecasts as function of the AR order
forecasts <- lapply(2:NCOL(predictor), function(ordern) {
  inverse <- MASS::ginv(predictor[insample, 1:ordern])
  coeff <- drop(inverse %*% response[insample])
  drop(predictor[outsample, 1:ordern] %*% coeff)
})  # end lapply
names(forecasts) <- paste0("n=", 2:NCOL(predictor))

# Calculate out-of-sample PnLs
pnls <- sapply(forecasts, function(x) {
  cumsum(sign(x)*vti[outsample])
})  # end sapply
colnames(pnls) <- names(forecasts)
pnls <- xts::xts(pnls, dates[outsample])
# Plot dygraph of out-of-sample PnLs
dygraphs::dygraph(pnls[, 1:4],
  main="Autoregressive Strategies Performance Using Rolling Average Predictor") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(width=500)

# Calculate PnLs using the average of past forecasts
nagg <- 5
pnls <- sapply(forecasts, function(x) {
  x <- roll::roll_mean(x, width=nagg, min_obs=1)
  cumsum(sign(x)*vti[outsample])
})  # end sapply
colnames(pnls) <- names(forecasts)
pnls <- xts::xts(pnls, dates[outsample])

# Plot dygraph of out-of-sample PnLs
dygraphs::dygraph(pnls[, 1:4],
  main="Autoregressive Strategies Performance Using Rolling Average Forecasts") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(width=500)

library(rutils)
# Calculate a vector of daily VTI log returns
prices <- log(quantmod::Cl(rutils::etfenv$VTI))
vti <- rutils::diffit(prices)
vti <- as.numeric(vti)
nrows <- NROW(vti)
# Define predictor matrix for forecasting
order_max <- 5
design <- sapply(1:order_max, rutils::lagit, input=vti)
colnames(design) <- paste0("pred_", 1:NCOL(design))
# Add response equal to vti
design <- cbind(vti, design)
colnames(design)[1] <- "response"
# Specify length of look-back interval
look_back <- 100
# Invert the predictor matrix
rangev <- (nrows-look_back):(nrows-1)
designinv <- MASS::ginv(design[rangev, -1])
# Calculate fitted coefficients
coeff <- drop(designinv %*% design[rangev, 1])
# Calculate forecast of vti for.n_rows
drop(design[nrows, -1] %*% coeff)
# Compare with actual value
design[nrows, 1]

# Calculate a vector of daily VTI log returns
vti <- na.omit(rutils::etfenv$returns$VTI)
dates <- zoo::index(vti)
vti <- as.numeric(vti)
nrows <- NROW(vti)
# Define response equal to vti
response <- vti
# Define predictor as a rolling sum
nagg <- 5
predictor <- rutils::roll_sum(vti, look_back=nagg)
# Define predictor matrix for forecasting
order_max <- 5
predictor <- sapply(1+nagg*(0:order_max), rutils::lagit,
               input=predictor)
predictor <- cbind(rep(1, nrows), predictor)
# Perform rolling forecasting
look_back <- 100
forecasts <- sapply((look_back+1)/nrows, function(endp) {
  # Define rolling look-back range
  startp <- max(1, endp-look_back)
  # Or expanding look-back range
  # startp <- 1
  rangev <- startp:(endp-1)
  # Invert the predictor matrix
  designinv <- MASS::ginv(predictor[rangev, ])
  # Calculate fitted coefficients
  coeff <- drop(designinv %*% response[rangev])
  # Calculate forecast
  drop(predictor[endp, ] %*% coeff)
})  # end sapply
# Add warmup period
forecasts <- c(rep(0, look_back), forecasts)

# Mean squared error
mean((vti - forecasts)^2)
# Correlation
cor(forecasts, vti)

# Plot forecasting series with legend
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0))
plot(vti[(nrows-look_back):nrows], col="blue",
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
  forecasts <- sapply((look_back+1)/nrows, function(endp) {
    # Define rolling look-back range
    startp <- max(1, endp-look_back)
    # Or expanding look-back range
    # startp <- 1
    rangev <- startp:(endp-1)
    # Invert the predictor matrix
    designinv <- MASS::ginv(predictor[rangev, ])
    # Calculate fitted coefficients
    coeff <- drop(designinv %*% response[rangev])
    # Calculate forecast
    drop(predictor[endp, ] %*% coeff)
  })  # end sapply
  # Add warmup period
  forecasts <- c(rep(0, look_back), forecasts)
  # Aggregate the forecasts
  rutils::roll_sum(forecasts, look_back=nagg)
}  # end sim_forecasts
# Simulate the rolling autoregressive forecasts
forecasts <- sim_forecasts(response=vti, ordern=5, look_back=100)
c(mse=mean((vti - forecasts)^2), cor=cor(vti, forecasts))

library(parallel)  # Load package parallel
# Calculate number of available cores
ncores <- detectCores() - 1
# Initialize compute cluster under Windows
cluster <- makeCluster(ncores)
# clusterExport(cluster, c("startd", "barp"))
# Perform parallel loop under Windows
look_backs <- seq(20, 600, 40)
forecasts <- parLapply(cluster, look_backs, sim_forecasts,
  response=vti, nagg=5, ordern=5)
# Perform parallel bootstrap under Mac-OSX or Linux
forecasts <- mclapply(look_backs, sim_forecasts, response=vti,
  nagg=5, ordern=5, mc.cores=ncores)

# Calculate mean squared errors
mse <- sapply(forecasts, function(x) {
  c(mse=mean((vti - x)^2), cor=cor(vti, x))
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
forecasts <- parLapply(cluster, orders, sim_forecasts, response=vti,
  nagg=5, look_back=look_back)
stopCluster(cluster)  # Stop R processes over cluster under Windows
# Perform parallel bootstrap under Mac-OSX or Linux
orders <- 2:6
forecasts <- mclapply(orders, sim_forecasts, response=vti,
  nagg=5, look_back=look_back, mc.cores=ncores)

# Calculate mean squared errors
mse <- sapply(forecasts, function(x) {
  c(mse=mean((vti - x)^2), cor=cor(vti, x))
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
forecasts <- sim_forecasts(vti, ordern=ordern, look_back=look_back)
# Calculate strategy PnLs
pnls <- sign(forecasts)*vti
pnls <- cbind(vti, pnls, (vti+pnls)/2)
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
forecasts <- sim_forecasts(vti, ordern=5, look_back=look_back)
pnls5 <- cumsum(sign(forecasts)*vti)
# Calculate PnLs for ordern=3
forecasts <- sim_forecasts(vti, ordern=3, look_back=look_back)
pnls3 <- cumsum(sign(forecasts)*vti)

# Plot the cumulative strategy returns
wealth <- cbind(pnls5, pnls3)
wealth <- xts::xts(wealth, dates)
colnamev <- c("AR(5)_Strategy", "AR(3)_Strategy")
colnames(wealth) <- colnamev
dygraphs::dygraph(wealth, main="Autoregressive Strategies for Different Order Parameters") %>%
  dySeries(name=colnamev[1], label=colnamev[1], col="blue", strokeWidth=2) %>%
  dySeries(name=colnamev[2], label=colnamev[2], col="red", strokeWidth=2) %>%
  dyLegend(width=500)

# Calculate PnLs for rolling look-back
forecasts <- sim_forecasts(vti, ordern=3, look_back=look_back, is_rolling=TRUE)
pnls_rolling <- cumsum(sign(forecasts)*vti)
# Calculate PnLs for expanding look-back
forecasts <- sim_forecasts(vti, ordern=3, look_back=look_back, is_rolling=FALSE)
pnls_expanding <- cumsum(sign(forecasts)*vti)

# Plot the cumulative strategy returns
wealth <- cbind(pnls_rolling, pnls_expanding)
wealth <- xts::xts(wealth, dates)
colnamev <- c("AR(3)_Rolling", "AR(3)_Expanding")
colnames(wealth) <- colnamev
dygraphs::dygraph(wealth, main="Autoregressive Strategies for Expanding Look-back Interval") %>%
  dySeries(name=colnamev[1], label=colnamev[1], col="blue", strokeWidth=2) %>%
  dySeries(name=colnamev[2], label=colnamev[2], col="red", strokeWidth=2) %>%
  dyLegend(width=500)

# Cap the VTI returns
cutoff <- 0.03
capped <- ifelse(vti > cutoff, cutoff, vti)
capped <- ifelse(capped < (-cutoff), -cutoff, capped)
# Calculate PnLs for vti
forecasts <- sim_forecasts(vti, ordern=3, look_back=look_back, is_rolling=FALSE)
pnls <- cumsum(sign(forecasts)*vti)
# Calculate PnLs for capped VTI returns
forecasts <- sim_forecasts(capped, ordern=3, look_back=look_back, is_rolling=FALSE)
pnls_capped <- cumsum(sign(forecasts)*vti)

# Plot the cumulative strategy returns
wealth <- cbind(pnls, pnls_capped)
wealth <- xts::xts(wealth, dates)
colnamev <- c("AR(3)_Rolling", "AR(3)_Expanding")
colnamev <- c("AR_Strategy", "AR_Strategy_Capped")
colnames(wealth) <- colnamev
dygraphs::dygraph(wealth, main="Improved Autoregressive Strategies") %>%
  dySeries(name=colnamev[1], label=colnamev[1], col="blue", strokeWidth=2) %>%
  dySeries(name=colnamev[2], label=colnamev[2], col="red", strokeWidth=2) %>%
  dyLegend(width=500)
