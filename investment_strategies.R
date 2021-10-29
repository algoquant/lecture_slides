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
weal_th <- function(f, a=0.8, b=0.1, n=1e3, i=150) {
  (1+f*a)^i * (1-f*b)^(n-i)
}  # end weal_th
curve(expr=weal_th, xlim=c(0, 1),
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
vt_i <- rutils::etf_env$re_turns$VTI
vt_i <- na.omit(vt_i)
c(mean=mean(vt_i), std=sd(vt_i))
range(vt_i)
# Open x11 for plotting
x11(width=5, height=4)
# Set plot parameters to reduce whitespace around plot
par(mar=c(4, 4, 2, 1), oma=c(0, 0, 0, 0))
# Define vectorized logarithmic utility function
utili_ty <- function(kell_y, re_turns) {
  sapply(kell_y, function(x)
    sum(log(1 + x*re_turns)))
}  # end utili_ty
utili_ty(1, vt_i)
utili_ty(c(1, 4), vt_i)
# Plot the logarithmic utility
curve(expr=utili_ty(x, re_turns=vt_i),
xlim=c(0.1, 5), xlab="leverage", ylab="utility",
main="Utility of Asset Returns", lwd=2)
# Approximate Kelly leverage
mean(vt_i)/var(vt_i)
PerformanceAnalytics::KellyRatio(R=vt_i, method="full")
# Kelly leverage
unlist(optimize(
  f=function(x) -utili_ty(x, vt_i),
  interval=c(1, 4)))
# Calculate the VTI returns
vt_i <- rutils::etf_env$re_turns$VTI
vt_i <- na.omit(vt_i)
# Calculate wealth paths
kelly_ratio <- drop(mean(vt_i)/var(vt_i))
kelly_wealth <- cumprod(1 + kelly_ratio*vt_i)
hyper_kelly <- cumprod(1 + (kelly_ratio+2)*vt_i)
sub_kelly <- cumprod(1 + (kelly_ratio-2)*vt_i)
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
kelly_ratio <- drop(mean(vt_i)/var(vt_i))
weal_th <- cumprod(1 + kelly_ratio*vt_i)
wealth_trans <- cumprod(1 + kelly_ratio*vt_i -
  0.5*bid_offer*kelly_ratio*(kelly_ratio-1)*abs(vt_i))
# Calculate compounded wealth from returns
weal_th <- cbind(weal_th, wealth_trans)
colnames(weal_th) <- c("Kelly", "Including bid-offer")
# Plot compounded wealth
dygraphs::dygraph(weal_th, main="Kelly Strategy With Transaction Costs") %>%
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
vt_i <- rutils::etf_env$re_turns$VTI
vt_i <- na.omit(vt_i)
# Calculate wealth paths
kelly_ratio <- drop(mean(vt_i)/var(vt_i))
kelly_wealth <- cumprod(1 + kelly_ratio*vt_i)
hyper_kelly <- cumprod(1 + (kelly_ratio+2)*vt_i)
sub_kelly <- cumprod(1 + (kelly_ratio-2)*vt_i)
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
vt_i <- rutils::etf_env$vt_i$VTI
vt_i <- na.omit(vt_i)
# Calculate higher moments of VTI returns
c(mean=sum(vt_i),
  variance=sum(vt_i^2),
  mom3=sum(vt_i^3),
  mom4=sum(vt_i^4))/NROW(vt_i)
# Calculate higher moments of minutely SPY returns
sp_y <- HighFreq::SPY[, 4]
sp_y <- na.omit(sp_y)
sp_y <- HighFreq::diff_it(log(sp_y))
c(mean=sum(sp_y),
  variance=sum(sp_y^2),
  mom3=sum(sp_y^3),
  mom4=sum(sp_y^4))/NROW(sp_y)
re_turns <- na.omit(rutils::etf_env$re_turns[, c("VTI", "IEF")])
# Logarithmic utility of stock and bond portfolio
utili_ty <- function(w_s, w_b) {
  -sum(log(1 + w_s*re_turns$VTI + w_b*re_turns$IEF))
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
weight_s <- sapply(re_turns, function(x) mean(x)/var(x))
# Kelly weight for stocks
unlist(optimize(f=function(x) utili_ty(x, w_b=0), interval=c(1, 4)))
# Kelly weight for bonds
unlist(optimize(f=function(x) utili_ty(x, w_s=0), interval=c(1, 14)))
# Vectorized utility of stock and bond portfolio
utility_vec <- function(weight_s) {
  utili_ty(weight_s[1], weight_s[2])
}  # end utility_vec
# Optimize with respect to vector argument
op_tim <- optim(fn=utility_vec, par=c(3, 10),
          method="L-BFGS-B",
          upper=c(8, 20), lower=c(2, 5))
# Exact Kelly weights
op_tim$par
# Approximate Kelly weights
p_rets <- (re_turns %*% weight_s)
drop(mean(p_rets)/var(p_rets))*weight_s
# Exact Kelly weights
op_tim$par
# Quarter-Kelly sub-optimal weights
weight_s <- op_tim$par/4
# Plot Kelly optimal portfolio
re_turns <- cbind(re_turns,
  weight_s[1]*re_turns$VTI + weight_s[2]*re_turns$IEF)
colnames(re_turns)[3] <- "Kelly_sub_optimal"
# Calculate compounded wealth from returns
weal_th <- cumprod(1 + re_turns)
# Plot compounded wealth
dygraphs::dygraph(weal_th, main="Stock and Bond Portfolio") %>%
  dyOptions(colors=c("green", "blue", "green")) %>%
  dySeries("Kelly_sub_optimal", color="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
re_turns <- na.omit(rutils::etf_env$re_turns[, c("VTI", "IEF")])
# Calculate rolling returns and variance
look_back <- 200
var_rolling <- roll::roll_var(re_turns, width=look_back)
weight_s <- roll::roll_sum(re_turns, width=look_back)/look_back
weight_s <- weight_s/var_rolling
weight_s[1, ] <- 1/NCOL(weight_s)
weight_s <- zoo::na.locf(weight_s)
sum(is.na(weight_s))
range(weight_s)
# Plot the weight_s
x11(width=6, height=5)
par(mar=c(4, 4, 3, 1), oma=c(0, 0, 0, 0))
plot(density(re_turns$IEF), t="l", lwd=3, col="red",
     xlab="weights", ylab="density",
     ylim=c(0, max(density(re_turns$VTI)$y)),
     main="Kelly Weight Distributions")
lines(density(re_turns$VTI), t="l", col="blue", lwd=3)
legend("topright", legend=c("VTI", "IEF"),
 inset=0.1, bg="white", lty=1, lwd=6,
 col=c("blue", "red"), bty="n")
# Scale and lag the Kelly weights
weight_s <- lapply(weight_s,
  function(x) 10*x/sum(abs(range(x))))
weight_s <- do.call(cbind, weight_s)
weight_s <- rutils::lag_it(weight_s)
# Calculate the compounded Kelly wealth and VTI
weal_th <- cbind(cumprod(1 + weight_s$VTI*re_turns$VTI),
           cumprod(1 + re_turns$VTI))
colnames(weal_th) <- c("Kelly Strategy", "VTI")
dygraphs::dygraph(weal_th, main="VTI Strategy Using Rolling Kelly Weight") %>%
  dyAxis("y", label="Kelly Strategy", independentTicks=TRUE) %>%
  dyAxis("y2", label="VTI", independentTicks=TRUE) %>%
  dySeries(name="Kelly Strategy", axis="y", label="Kelly Strategy", strokeWidth=1, col="red") %>%
  dySeries(name="VTI", axis="y2", label="VTI", strokeWidth=1, col="blue")
# bid_offer equal to 10 bps for liquid ETFs
bid_offer <- 0.001
# Calculate the compounded Kelly wealth and margin
weal_th <- cumprod(1 + weight_s$VTI*re_turns$VTI)
mar_gin <- (re_turns$VTI - 1)*weal_th + 1
# Calculate the transaction costs
cost_s <- bid_offer*drop(rutils::diff_it(mar_gin))/2
wealth_diff <- drop(rutils::diff_it(weal_th))
costs_rel <- ifelse(wealth_diff>0, cost_s/wealth_diff, 0)
range(costs_rel)
hist(costs_rel, breaks=10000, xlim=c(-0.02, 0.02))
# Scale and lag the transaction costs
cost_s <- rutils::lag_it(abs(cost_s)/weal_th)
# Recalculate the compounded Kelly wealth
wealth_trans <- cumprod(1 + re_turns$VTI*re_turns$VTI - cost_s)
# Plot compounded wealth
weal_th <- cbind(weal_th, wealth_trans)
colnames(weal_th) <- c("Kelly", "Including bid-offer")
dygraphs::dygraph(weal_th, main="Kelly Strategy With Transaction Costs") %>%
  dyOptions(colors=c("green", "blue"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Calculate compounded wealth from returns
weal_th <- cumprod(1 + rowSums(weight_s*re_turns))
weal_th <- xts::xts(weal_th, index(re_turns))
quantmod::chart_Series(weal_th, name="Rolling Kelly Strategy For VTI and IEF")
# Calculate the compounded Kelly wealth and VTI
weal_th <- cbind(weal_th,
  cumprod(1 + 0.6*re_turns$IEF + 0.4*re_turns$VTI))
colnames(weal_th) <- c("Kelly Strategy", "VTI plus IEF")
dygraphs::dygraph(weal_th, main="Rolling Kelly Strategy For VTI and IEF") %>%
  dyAxis("y", label="Kelly Strategy", independentTicks=TRUE) %>%
  dyAxis("y2", label="VTI plus IEF", independentTicks=TRUE) %>%
  dySeries(name="Kelly Strategy", axis="y", label="Kelly Strategy", strokeWidth=1, col="red") %>%
  dySeries(name="VTI plus IEF", axis="y2", label="VTI plus IEF", strokeWidth=1, col="blue")
# Test if IEF can time VTI
re_turns <- na.omit(rutils::etf_env$re_turns[, c("IEF", "VTI")])
vt_i <- re_turns$VTI
de_sign <- cbind(re_turns, 0.5*(vt_i+abs(vt_i)), vt_i^2)
colnames(de_sign)[3:4] <- c("merton", "treynor")
# Merton-Henriksson test
mod_el <- lm(IEF ~ VTI + merton, data=de_sign); summary(mod_el)
# Treynor-Mazuy test
mod_el <- lm(IEF ~ VTI + treynor, data=de_sign); summary(mod_el)
# Plot residual scatterplot
x11(width=6, height=5)
residual_s <- (de_sign$IEF - mod_el$coeff[2]*de_sign$VTI)
plot.default(x=de_sign$VTI, y=residual_s, xlab="VTI", ylab="IEF")
title(main="Treynor-Mazuy Market Timing Test\n for IEF vs VTI", line=0.5)
# Plot fitted (predicted) response values
fit_ted <- (mod_el$coeff["(Intercept)"] +
        mod_el$coeff["treynor"]*vt_i^2)
points.default(x=de_sign$VTI, y=fit_ted, pch=16, col="red")
text(x=0.05, y=0.8*max(residual_s), paste("Treynor test t-value =", round(summary(mod_el)$coeff["treynor", "t value"], 2)))
library(xtable)
gambl_e <- data.frame(win=c("p", "a"), lose=c("q = 1 - p", "-b"))
rownames(gambl_e) <- c("probability", "payout")
# print(xtable(gambl_e), comment=FALSE, size="tiny")
print(xtable(gambl_e), comment=FALSE)
library(rutils)
# Extract ETF prices from rutils::etf_env$price_s
price_s <- rutils::etf_env$price_s
price_s <- zoo::na.locf(price_s, na.rm=FALSE)
price_s <- zoo::na.locf(price_s, fromLast=TRUE)
# Calculate simple dollar returns
rets_dollar <- rutils::diff_it(price_s)
# Or
# rets_dollar <- lapply(price_s, rutils::diff_it)
# rets_dollar <- rutils::do_call(cbind, rets_dollar)
# Calculate log returns
rets_log <- rutils::diff_it(log(price_s))
# Calculate percentage returns
rets_percent <- rets_dollar/
  rutils::lag_it(price_s, lagg=1, pad_zeros=FALSE)
# Calculate prices from simple dollar returns
rets_dollar[1, ] <- price_s[1, ]
new_prices <- cumsum(rets_dollar)
all.equal(new_prices, price_s)
# Compound the percentage returns
new_prices <- cumprod(1+ rets_percent)
new_prices <- lapply(1:NCOL(new_prices), function (i)
    init_prices[i]*new_prices[, i])
new_prices <- rutils::do_call(cbind, new_prices)
all.equal(new_prices, price_s)
# Sum the percentage returns
new_prices <- cumsum(rets_percent)
methods(cumsum)
new_prices <- lapply(1:NCOL(new_prices), function (i)
    new_prices[, i] + log(init_prices[i]))
new_prices <- rutils::do_call(cbind, new_prices)
# Only approximately equal
all.equal(new_prices, log(price_s))
# Plot log VTI prices
dygraphs::dygraph(log(quantmod::Cl(rutils::etf_env$VTI)),
  main="Logarithm of VTI Prices") %>%
  dyOptions(colors="blue", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Calculate percentage VTI returns
price_s <- rutils::etf_env$price_s$VTI
price_s <- na.omit(price_s)
re_turns <- rutils::diff_it(price_s)/
  rutils::lag_it(price_s, lagg=1, pad_zeros=FALSE)
# Funding rate per day
f_rate <- 0.01/252
# Margin account
mar_gin <- cumsum(re_turns)
# Cumulative funding costs
f_costs <- cumsum(f_rate*mar_gin)
# Add funding costs to margin account
mar_gin <- (mar_gin + f_costs)
# dygraph plot of margin and funding costs
da_ta <- cbind(mar_gin, f_costs)
col_names <- c("Margin", "Cumulative Funding")
colnames(da_ta) <- col_names
dygraphs::dygraph(da_ta, main="VTI Margin Funding Costs") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="blue") %>%
  dySeries(name=col_names[2], axis="y2", col="red", strokeWidth=3) %>%
  dyLegend(show="always", width=500)
# bid_offer equal to 10 bps for liquid ETFs
bid_offer <- 0.001
# Cumulative transaction costs
cost_s <- bid_offer*cumsum(abs(re_turns))/2
# Subtract transaction costs from margin account
mar_gin <- cumsum(re_turns)
mar_gin <- (mar_gin - cost_s)
# dygraph plot of margin and transaction costs
da_ta <- cbind(mar_gin, cost_s)
col_names <- c("Margin", "Cumulative Transaction Costs")
colnames(da_ta) <- col_names
dygraphs::dygraph(da_ta, main="VTI Transaction Costs") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="blue") %>%
  dySeries(name=col_names[2], axis="y2", col="red", strokeWidth=3) %>%
  dyLegend(show="always", width=500)
# Calculate VTI and IEF dollar returns
price_s <- rutils::etf_env$price_s[, c("VTI", "IEF")]
price_s <- na.omit(price_s)
date_s <- index(price_s)
rets_dollar <- rutils::diff_it(price_s)
# Calculate VTI and IEF percentage returns
rets_percent <- rets_dollar/
  rutils::lag_it(price_s, lagg=1, pad_zeros=FALSE)
# Wealth of fixed shares (without rebalancing)
weight_s <- c(0.5, 0.5)
rets_dollar[1, ] <- price_s[1, ]
wealth_fsa <- cumsum(rets_dollar %*% weight_s)
# Wealth of fixed dollars (with rebalancing)
wealth_fda <- cumsum(rets_percent %*% weight_s)
# Plot log wealth
weal_th <- cbind(wealth_fda, log(wealth_fsa))
weal_th <- xts::xts(weal_th, index(price_s))
colnames(weal_th) <- c("Fixed dollars", "Fixed shares (log)")
col_names <- colnames(weal_th)
dygraphs::dygraph(weal_th, main="Wealth of Weighted Portfolios") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="red", strokeWidth=2) %>%
  dySeries(name=col_names[2], axis="y2", col="blue", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Margin account for fixed dollars (with rebalancing)
mar_gin <- cumsum(rets_percent %*% weight_s)
# Cumulative transaction costs
cost_s <- bid_offer*cumsum(abs(rets_percent) %*% weight_s)/2
# Subtract transaction costs from margin account
mar_gin <- (mar_gin - cost_s)
# dygraph plot of margin and transaction costs
da_ta <- cbind(mar_gin, cost_s)
da_ta <- xts::xts(da_ta, index(price_s))
col_names <- c("Margin", "Cumulative Transaction Costs")
colnames(da_ta) <- col_names
dygraphs::dygraph(da_ta, main="Fixed Dollar Portfolio Transaction Costs") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="blue") %>%
  dySeries(name=col_names[2], axis="y2", col="red", strokeWidth=3) %>%
  dyLegend(show="always", width=500)
# Wealth of fixed shares (without rebalancing)
wealth_fsa <- cumsum(rets_dollar %*% weight_s)
# Calculate weighted percentage returns
rets_weighted <- rets_percent %*% weight_s
# Wealth of fixed ratio of dollar amounts (with rebalancing)
wealth_cda <- cumprod(1 + rets_weighted)
wealth_cda <- wealth_fsa[1]*wealth_cda
# Plot log wealth
weal_th <- log(cbind(wealth_fsa, wealth_cda))
weal_th <- xts::xts(weal_th, index(price_s))
colnames(weal_th) <- c("Fixed Shares", "Fixed Ratio")
dygraphs::dygraph(weal_th, main="Log Wealth of Fixed Dollar Ratios") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Returns in excess of weighted returns
ex_cess <- lapply(rets_percent, function(x) (rets_weighted - x))
ex_cess <- do.call(cbind, ex_cess)
sum(ex_cess %*% weight_s)
# Calculate weighted sum of absolute excess returns
ex_cess <- abs(ex_cess) %*% weight_s
# Total dollar amount of stocks that need to be traded
ex_cess <- ex_cess*rutils::lag_it(wealth_cda)
# Cumulative transaction costs
cost_s <- bid_offer*cumsum(ex_cess)/2
# Subtract transaction costs from wealth
wealth_cda <- (wealth_cda - cost_s)
# dygraph plot of wealth and transaction costs
weal_th <- cbind(wealth_cda, cost_s)
weal_th <- xts::xts(weal_th, index(price_s))
col_names <- c("Wealth", "Cumulative Transaction Costs")
colnames(weal_th) <- col_names
dygraphs::dygraph(weal_th, main="Transaction Costs With Fixed Dollar Ratios") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="blue") %>%
  dySeries(name=col_names[2], axis="y2", col="red", strokeWidth=3) %>%
  dyLegend(show="always", width=500)
library(rutils)  # Load package rutils
# Create name corresponding to "^GSPC" symbol
setSymbolLookup(
  SP500=list(name="^GSPC", src="yahoo"))
getSymbolLookup()
# view and clear options
options("getSymbols.sources")
options(getSymbols.sources=NULL)
# Download S&P500 prices into etf_env
quantmod::getSymbols("SP500", env=etf_env,
    adjust=TRUE, auto.assign=TRUE, from="1990-01-01")
quantmod::chart_Series(x=etf_env$SP500["2016/"],
       TA="add_Vo()",
       name="S&P500 index")
# Calculate stock and bond returns
re_turns <- na.omit(rutils::etf_env$re_turns[, c("VTI", "IEF")])
weight_s <- c(0.4, 0.6)
re_turns <- cbind(re_turns, re_turns %*% weight_s)
colnames(re_turns)[3] <- "Combined"
# Calculate correlations
cor(re_turns)
# Calculate Sharpe ratios
sqrt(252)*sapply(re_turns, function(x) mean(x)/sd(x))
# Calculate standard deviation, skewness, and kurtosis
sapply(re_turns, function(x) {
  # Calculate standard deviation
  stddev <- sd(x)
  # Standardize the returns
  x <- (x - mean(x))/stddev
  c(stddev=stddev, skew=mean(x^3), kurt=mean(x^4))
})  # end sapply
# Wealth of fixed ratio of dollar amounts
weal_th <- cumprod(1 + re_turns)
# Plot cumulative wealth
dygraphs::dygraph(log(weal_th), main="Stock and Bond Portfolio") %>%
  dyOptions(colors=c("blue", "green", "blue", "red")) %>%
  dySeries("Combined", color="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
library(rutils)  # Load package rutils
# Calculate ETF returns
re_turns <- na.omit(rutils::etf_env$re_turns[, c("IEF", "VTI")])
re_turns <- cbind(re_turns, 0.6*re_turns$IEF+0.4*re_turns$VTI)
colnames(re_turns)[3] <- "combined"
# Calculate correlations
cor(re_turns)
# Calculate Sharpe ratios
sqrt(252)*sapply(re_turns, function(x) mean(x)/sd(x))
# Calculate skewness and kurtosis
sapply(re_turns, sd)
# Calculate skewness and kurtosis
t(sapply(c(skew=3, kurt=4), function(x)
  moments::moment(re_turns, order=x, central=TRUE)))
# Calculate prices from returns
price_s <- lapply(re_turns, function(x) exp(cumsum(x)))
price_s <- do.call(cbind, price_s)
# Plot prices
dygraphs::dygraph(price_s, main="Stock and Bond Portfolio") %>%
  dyOptions(colors=c("green", "blue", "green")) %>%
  dySeries("combined", color="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Extract ETF returns
sym_bols <- c("VTI", "IEF", "DBC")
re_turns <- na.omit(rutils::etf_env$re_turns[, sym_bols])
# Calculate all-weather portfolio wealth
weights_aw <- c(0.30, 0.55, 0.15)
re_turns <- cbind(re_turns, re_turns %*% weights_aw)
colnames(re_turns)[4] <- "All Weather"
# Calculate cumulative wealth from returns
weal_th <- cumsum(re_turns)
# dygraph all-weather wealth
dygraphs::dygraph(weal_th, main="All-Weather Portfolio") %>%
  dyOptions(colors=c("blue", "green", "orange", "red")) %>%
  dySeries("All Weather", color="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Plot all-weather wealth
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue", "green", "red")
quantmod::chart_Series(weal_th, theme=plot_theme, lwd=c(2, 2, 2, 4),
       name="All-Weather Portfolio")
legend("topleft", legend=colnames(weal_th),
  inset=0.1, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")
# Calculate standardized simple dollar returns
rets_dollar_std <- lapply(rets_dollar, function(x) x/sd(x))
rets_dollar_std <- do.call(cbind, rets_dollar_std)
sapply(rets_dollar_std, sd)
# Wealth of fixed number of shares (without rebalancing)
weight_s <- c(0.5, 0.5)
wealth_fsa <- cumsum(rets_dollar %*% weight_s)
# Calculate standardized percentage returns
rets_percent_std <- lapply(rets_percent, function(x) x/sd(x))
rets_percent_std <- do.call(cbind, rets_percent_std)
sapply(rets_percent_std, sd)
# Wealth of fixed dollar amount of shares (with rebalancing)
wealth_fda <- cumsum(rets_percent_std %*% weight_s)
# Plot log wealth
weal_th <- cbind(wealth_fda, log(wealth_fsa))
# weal_th <- xts::xts(weal_th, index(price_s))
colnames(weal_th) <- c("With rebalancing", "Without rebalancing")
dygraphs::dygraph(weal_th, main="Wealth of Equal Dollar Amount of Shares") %>%
  dyOptions(colors=c("green", "blue"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Calculate VTI returns
re_turns <- na.omit(rutils::etf_env$re_turns$VTI["2008/2009"])
date_s <- index(re_turns)
n_rows <- NROW(re_turns)
re_turns <- drop(zoo::coredata(re_turns))
bfloor <- 60  # bond floor
co_eff <- 2  # multiplier
portf_value <- numeric(n_rows)
portf_value[1] <- 100  # principal
stock_value <- numeric(n_rows)
stock_value[1] <- co_eff*(portf_value[1] - bfloor)
bond_value <- numeric(n_rows)
bond_value[1] <- (portf_value[1] - stock_value[1])
# Simulate CPPI strategy
for (t in 2:n_rows) {
  portf_value[t] <- portf_value[t-1] + stock_value[t-1]*re_turns[t]
  stock_value[t] <- min(co_eff*(portf_value[t] - bfloor), portf_value[t])
  bond_value[t] <- (portf_value[t] - stock_value[t])
}  # end for
# dygraph plot of CPPI strategy
vt_i <- 100*cumprod(1+re_turns)
da_ta <- xts::xts(cbind(stock_value, bond_value, portf_value, vt_i), date_s)
colnames(da_ta) <- c("stocks", "bonds", "CPPI", "VTI")
dygraphs::dygraph(da_ta, main="CPPI strategy") %>%
  dyOptions(colors=c("red", "green", "blue", "orange"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Calculate dollar and percentage returns for VTI and IEF.
price_s <- rutils::etf_env$price_s[, c("VTI", "IEF")]
price_s <- na.omit(price_s)
rets_dollar <- rutils::diff_it(price_s)
rets_percent <- rets_dollar/rutils::lag_it(price_s, lagg=1, pad_zeros=FALSE)
# Calculate wealth of fixed ratio of dollar amounts.
weight_s <- c(0.5, 0.5)
rets_weighted <- rets_percent %*% weight_s
wealth_cda <- cumprod(1 + rets_weighted)
# Calculate rolling percentage volatility.
look_back <- 21
vo_l <- roll::roll_sd(rets_percent, width=look_back)
vo_l <- zoo::na.locf(vo_l, na.rm=FALSE)
vo_l <- zoo::na.locf(vo_l, fromLast=TRUE)
# Calculate the risk parity portfolio allocations.
allocation_s <- lapply(1:NCOL(price_s),
  function(x) weight_s[x]/vo_l[, x])
allocation_s <- do.call(cbind, allocation_s)
# Scale allocations to 1 dollar total.
allocation_s <- allocation_s/rowSums(allocation_s)
# Lag the allocations
allocation_s <- rutils::lag_it(allocation_s)
# Calculate wealth of risk parity.
rets_weighted <- rowSums(rets_percent*allocation_s)
wealth_risk_parity <- cumprod(1 + rets_weighted)
# Calculate the log wealths.
weal_th <- log(cbind(wealth_cda, wealth_risk_parity))
weal_th <- xts::xts(weal_th, index(price_s))
colnames(weal_th) <- c("Fixed Ratio", "Risk Parity")
# Calculate the Sharpe ratios.
sqrt(252)*sapply(rutils::diff_it(weal_th), function (x) mean(x)/sd(x))
# Plot a dygraph of the log wealths.
dygraphs::dygraph(weal_th, main="Log Wealth of Risk Parity vs Fixed Dollar Ratios") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Test risk parity market timing of VTI using Treynor-Mazuy test
re_turns <- rutils::diff_it(weal_th)
vt_i <- rets_percent$VTI
de_sign <- cbind(re_turns, vt_i, vt_i^2)
de_sign <- na.omit(de_sign)
colnames(de_sign)[1:2] <- c("fixed", "risk_parity")
colnames(de_sign)[4] <- "treynor"
mod_el <- lm(risk_parity ~ VTI + treynor, data=de_sign)
summary(mod_el)
# Plot residual scatterplot
residual_s <- (de_sign$risk_parity - mod_el$coeff[2]*de_sign$VTI)
residual_s <- mod_el$residuals
x11(width=6, height=5)
plot.default(x=de_sign$VTI, y=residual_s, xlab="VTI", ylab="residuals")
title(main="Treynor-Mazuy Market Timing Test\n for Risk Parity vs VTI", line=0.5)
# Plot fitted (predicted) response values
fit_ted <- (mod_el$coeff["(Intercept)"] + mod_el$coeff["treynor"]*vt_i^2)
points.default(x=de_sign$VTI, y=fit_ted, pch=16, col="red")
text(x=0.05, y=0.8*max(residual_s), paste("Risk Parity t-value =", round(summary(mod_el)$coeff["treynor", "t value"], 2)))
# Test for fixed ratio market timing of VTI using Treynor-Mazuy test
mod_el <- lm(fixed ~ VTI + treynor, data=de_sign)
summary(mod_el)
# Plot fitted (predicted) response values
fit_ted <- (mod_el$coeff["(Intercept)"] + mod_el$coeff["treynor"]*vt_i^2)
points.default(x=de_sign$VTI, y=fit_ted, pch=16, col="blue")
text(x=0.05, y=0.8*max(residual_s), paste("Fixed Ratio t-value =", round(summary(mod_el)$coeff["treynor", "t value"], 2)))
# Returns in excess of weighted returns
ex_cess <- lapply(rets_percent, function(x) (rets_weighted - x))
ex_cess <- do.call(cbind, ex_cess)
sum(ex_cess %*% weight_s)
# Calculate weighted sum of absolute excess returns
ex_cess <- abs(ex_cess) %*% weight_s
# Total dollar amount of stocks that need to be traded
ex_cess <- ex_cess*rutils::lag_it(wealth_cda)
# Cumulative transaction costs
cost_s <- bid_offer*cumsum(ex_cess)/2
# Subtract transaction costs from wealth
wealth_cda <- (wealth_cda - cost_s)
# dygraph plot of wealth and transaction costs
weal_th <- cbind(wealth_cda, cost_s)
weal_th <- xts::xts(weal_th, index(price_s))
col_names <- c("Wealth", "Cumulative Transaction Costs")
colnames(weal_th) <- col_names
dygraphs::dygraph(weal_th, main="Transaction Costs With Fixed Dollar Ratios") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="blue") %>%
  dySeries(name=col_names[2], axis="y2", col="red", strokeWidth=3) %>%
  dyLegend(show="always", width=500)
# Calculate positions
vt_i <- na.omit(rutils::etf_env$re_turns$VTI)
po_s <- rep(NA_integer_, NROW(vt_i))
date_s <- index(vt_i)
date_s <- format(date_s, "%m-%d")
po_s[date_s == "05-01"] <- 0
po_s[date_s == "05-03"] <- 0
po_s[date_s == "11-01"] <- 1
po_s[date_s == "11-03"] <- 1
# Carry forward and backward non-NA po_s
po_s <- zoo::na.locf(po_s, na.rm=FALSE)
po_s <- zoo::na.locf(po_s, fromLast=TRUE)
# Calculate strategy returns
sell_inmay <- po_s*vt_i
weal_th <- cbind(vt_i, sell_inmay)
colnames(weal_th) <- c("VTI", "sell_in_may")
# Calculate Sharpe and Sortino ratios
sqrt(252)*sapply(weal_th,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot wealth of Sell in May strategy
dygraphs::dygraph(cumsum(weal_th), main="Sell in May Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# OR: Open x11 for plotting
x11(width=6, height=5)
par(mar=c(4, 4, 3, 1), oma=c(0, 0, 0, 0))
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("blue", "red")
quantmod::chart_Series(weal_th, theme=plot_theme, name="Sell in May Strategy")
legend("topleft", legend=colnames(weal_th),
  inset=0.1, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")
# Test if Sell in May strategy can time VTI
de_sign <- cbind(vt_i, 0.5*(vt_i+abs(vt_i)), vt_i^2)
colnames(de_sign) <- c("VTI", "merton", "treynor")
# Perform Merton-Henriksson test
mod_el <- lm(sell_inmay ~ VTI + merton, data=de_sign)
summary(mod_el)
# Perform Treynor-Mazuy test
mod_el <- lm(sell_inmay ~ VTI + treynor, data=de_sign)
summary(mod_el)
# Plot Treynor-Mazuy residual scatterplot
residual_s <- (sell_inmay - mod_el$coeff[2]*vt_i)
plot.default(x=vt_i, y=residual_s, xlab="VTI", ylab="residuals")
title(main="Treynor-Mazuy Market Timing Test\n for Sell in May vs VTI", line=0.5)
# Plot fitted (predicted) response values
fit_ted <- (mod_el$coeff["(Intercept)"] +
        mod_el$coeff["treynor"]*vt_i^2)
points.default(x=vt_i, y=fit_ted, pch=16, col="red")
text(x=0.05, y=0.8*max(residual_s), paste("Treynor test t-value =", round(summary(mod_el)$coeff["treynor", "t value"], 2)))
# Calculate the log of OHLC VTI prices
oh_lc <- log(rutils::etf_env$VTI)
op_en <- quantmod::Op(oh_lc)
hi_gh <- quantmod::Hi(oh_lc)
lo_w <- quantmod::Lo(oh_lc)
clos_e <- quantmod::Cl(oh_lc)
# Calculate the close-to-close log returns, the intraday
# open-to-close returns and the overnight close-to-open returns.
close_close <- rutils::diff_it(clos_e)
colnames(close_close) <- "close_close"
open_close <- (clos_e - op_en)
colnames(open_close) <- "open_close"
close_open <- (op_en - rutils::lag_it(clos_e, lagg=1, pad_zeros=FALSE))
colnames(close_open) <- "close_open"
# Calculate Sharpe and Sortino ratios
weal_th <- cbind(close_close, close_open, open_close)
sqrt(252)*sapply(weal_th,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot log wealth
dygraphs::dygraph(cumsum(weal_th),
  main="Wealth of Close-to-Close, Close-to-Open, and Open-to-Close Strategies") %>%
  dySeries(name="close_close", label="Close-to-Close (static)", strokeWidth=2, col="blue") %>%
  dySeries(name="close_open", label="Close-to-Open (overnight)", strokeWidth=2, col="red") %>%
  dySeries(name="open_close", label="Open-to-Close (daytime)", strokeWidth=2, col="green") %>%
  dyLegend(width=600)
# Calculate the VTI returns
vt_i <- na.omit(rutils::etf_env$re_turns$VTI)
date_s <- zoo::index(vt_i)
# Calculate first business day of every month
day_s <- as.numeric(format(date_s, "%d"))
indeks <- which(rutils::diff_it(day_s) < 0)
date_s[head(indeks)]
# Calculate Turn of the Month dates
indeks <- lapply((-1):2, function(x) indeks + x)
indeks <- do.call(c, indeks)
sum(indeks > NROW(date_s))
indeks <- sort(indeks)
date_s[head(indeks, 11)]
# Calculate Turn of the Month pnls
pnl_s <- numeric(NROW(vt_i))
pnl_s[indeks] <- vt_i[indeks, ]
# Combine data
weal_th <- cbind(vt_i, pnl_s)
col_names <- c("VTI", "Strategy")
colnames(weal_th) <- col_names
# Calculate Sharpe and Sortino ratios
sqrt(252)*sapply(weal_th,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# dygraph plot VTI Turn of the Month strategy
dygraphs::dygraph(cumsum(weal_th), main="Turn of the Month Strategy") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=col_names[2], axis="y2", strokeWidth=2, col="red")
# Calculate the VTI returns
vt_i <- na.omit(rutils::etf_env$re_turns$VTI)
date_s <- zoo::index(vt_i)
vt_i <- drop(coredata(vt_i))
n_rows <- NROW(vt_i)
# Simulate stop-loss strategy
sto_p <- 0.05
ma_x <- 0.0
cum_ret <- 0.0
pnl_s <- vt_i
for (i in 1:n_rows) {
# Calculate drawdown
  cum_ret <- cum_ret + vt_i[i]
  ma_x <- max(ma_x, cum_ret)
  dd <- (cum_ret - ma_x)
# Check for stop-loss
  if (dd < -sto_p*ma_x)
    pnl_s[i+1] <- 0
}  # end for
# Same but without using explicit loops
cum_sum <- cumsum(vt_i)
cum_max <- cummax(cumsum(vt_i))
dd <- (cum_sum - cum_max)
pnls2 <- vt_i
is_dd <- rutils::lag_it(dd < -sto_p*cum_max)
pnls2 <- ifelse(is_dd, 0, pnls2)
all.equal(pnl_s, pnls2)
# Combine data
weal_th <- xts::xts(cbind(vt_i, pnl_s), date_s)
col_names <- c("VTI", "Strategy")
colnames(weal_th) <- col_names
# Calculate Sharpe and Sortino ratios
sqrt(252)*sapply(weal_th,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# dygraph plot VTI stop-loss strategy
dygraphs::dygraph(cumsum(weal_th), main="VTI Stop-loss Strategy") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=col_names[2], axis="y2", strokeWidth=2, col="red")
# Simulate multiple stop-loss strategies
cum_sum <- cumsum(vt_i)
cum_max <- cummax(cumsum(vt_i))
dd <- (cum_sum - cum_max)
cum_pnls <- sapply(0.01*(1:20), function(sto_p) {
  pnl_s <- vt_i
  is_dd <- rutils::lag_it(dd < -sto_p*cum_max)
  pnl_s <- ifelse(is_dd, 0, pnl_s)
  sum(pnl_s)
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
pnl_s <- vt_i
for (i in 1:(n_rows-1)) {
# Calculate drawdown
  cum_ret <- cum_ret + vt_i[i]
  ma_x <- max(ma_x, cum_ret)
  dd <- (cum_ret - ma_x)
# Check for stop-loss
  if (dd < -sto_p*ma_x) {
    pnl_s[i+1] <- 0
    mi_n <- min(mi_n, cum_ret)
    du <- (cum_ret - mi_n)
# Check for gain
    if (du > sto_p*mi_n) {
pnl_s[i+1] <- vt_i[i+1]
    }  # end if
  } else {
    mi_n <- cum_ret
  }  # end if
}  # end for
# Combine data
weal_th <- xts::xts(cbind(vt_i, pnl_s), date_s)
col_names <- c("VTI", "Strategy")
colnames(weal_th) <- col_names
# Calculate Sharpe and Sortino ratios
sqrt(252)*sapply(weal_th,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# dygraph plot VTI stop-loss strategy
dygraphs::dygraph(cumsum(weal_th), main="VTI Stop-loss Strategy") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=col_names[2], axis="y2", strokeWidth=2, col="red")
# Extract VTI log OHLC prices
oh_lc <- log(rutils::etf_env$VTI)
clos_e <- quantmod::Cl(oh_lc)
re_turns <- rutils::diff_it(clos_e)
# Calculate the centered volatility
look_back <- 21
half_back <- look_back %/% 2
vol_at <- roll::roll_sd(re_turns, width=look_back, min_obs=1)
vol_at <- rutils::lag_it(vol_at, lagg=(-half_back))
# Calculate the z-scores of prices
price_scores <- (2*clos_e -
  rutils::lag_it(clos_e, half_back, pad_zeros=FALSE) -
  rutils::lag_it(clos_e, -half_back, pad_zeros=FALSE))
price_scores <- ifelse(vol_at > 0, price_scores/vol_at, 0)
# Plot dygraph of z-scores of VTI prices
price_s <- cbind(clos_e, price_scores)
colnames(price_s) <- c("VTI", "Z-scores")
col_names <- colnames(price_s)
dygraphs::dygraph(price_s["2009"], main="VTI Price Z-Scores") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=2, col="blue") %>%
  dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=2, col="red")
# Calculate thresholds for labeling tops and bottoms
threshold_s <- quantile(price_scores, c(0.1, 0.9))
# Calculate the vectors of tops and bottoms
top_s <- zoo::coredata(price_scores > threshold_s[2])
colnames(top_s) <- "tops"
bottom_s <- zoo::coredata(price_scores < threshold_s[1])
colnames(bottom_s) <- "bottoms"
# Simulate in-sample VTI strategy
po_s <- rep(NA_integer_, NROW(re_turns))
po_s[1] <- 0
po_s[top_s] <- (-1)
po_s[bottom_s] <- 1
po_s <- zoo::na.locf(po_s)
po_s <- rutils::lag_it(po_s)
pnl_s <- re_turns*po_s
# Plot dygraph of in-sample VTI strategy
weal_th <- cbind(re_turns, pnl_s)
colnames(weal_th) <- c("VTI", "Strategy")
dygraphs::dygraph(cumsum(weal_th), main="VTI Strategy Using In-sample Labels") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Calculate volatility z-scores
vol_at <- HighFreq::roll_var_ohlc(ohlc=oh_lc, look_back=look_back, scale=FALSE)
volat_mean <- roll::roll_mean(vol_at, width=look_back, min_obs=1)
volat_sd <- roll::roll_sd(rutils::diff_it(vol_at), width=look_back, min_obs=1)
volat_sd[1] <- 0
volat_scores <- ifelse(volat_sd > 0, (vol_at - volat_mean)/volat_sd, 0)
colnames(volat_scores) <- "volat"
# Calculate volume z-scores
vol_ume <- quantmod::Vo(oh_lc)
volume_mean <- roll::roll_mean(vol_ume, width=look_back, min_obs=1)
volume_sd <- roll::roll_sd(rutils::diff_it(vol_ume), width=look_back, min_obs=1)
volume_sd[1] <- 0
volume_scores <- ifelse(volume_sd > 0, (vol_ume - volume_mean)/volume_sd, 0)
colnames(volume_scores) <- "volume"
# Define design matrix for tops including intercept column
de_sign <- cbind(top_s, intercept=rep(1, NROW(top_s)),
           volat_scores, volume_scores)
# Define regression formula
col_names <- colnames(de_sign)
for_mula <- as.formula(paste(paste(col_names[1],
  paste(col_names[-1], collapse="+"), sep=" ~ "), "-1"))
# Fit in-sample logistic regression for tops
g_lm <- glm(for_mula, data=de_sign, family=binomial(logit))
summary(g_lm)
co_eff <- g_lm$coefficients
pre_dict <- drop(de_sign[, -1] %*% co_eff)
or_der <- order(pre_dict)
# Calculate in-sample forecasts from logistic regression model
forecast_s <- 1/(1+exp(-pre_dict))
all.equal(g_lm$fitted.values, forecast_s, check.attributes=FALSE)
hist(forecast_s)
x11(width=6, height=5)
plot(x=pre_dict[or_der], y=top_s[or_der],
     main="Logistic Regression of Stock Tops",
     col="orange", xlab="predictor", ylab="top")
lines(x=pre_dict[or_der], y=g_lm$fitted.values[or_der], col="blue", lwd=3)
legend(x="topleft", inset=0.1, bty="n", lwd=6,
 legend=c("tops", "logit fitted values"),
 col=c("orange", "blue"), lty=c(NA, 1), pch=c(1, NA))
# Define discrimination threshold value
thresh_old <- quantile(forecast_s, 0.95)
# Calculate confusion matrix in-sample
confu_sion <- table(actual=!top_s, forecast=(forecast_s < thresh_old))
confu_sion
# Calculate FALSE positive (type I error)
sum(!top_s & (forecast_s > thresh_old))
# Calculate FALSE negative (type II error)
sum(top_s & (forecast_s < thresh_old))
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
con_fuse(!top_s, forecast_s, threshold=thresh_old)
# Define vector of discrimination thresholds
threshold_s <- quantile(forecast_s, seq(0.1, 0.99, by=0.01))
# Calculate error rates
error_rates <- sapply(threshold_s, con_fuse,
  actual=!top_s, forecasts=forecast_s)  # end sapply
error_rates <- t(error_rates)
rownames(error_rates) <- threshold_s
# Calculate the informedness
inform_ed <- 2 - rowSums(error_rates[, c("typeI", "typeII")])
plot(threshold_s, inform_ed, t="l", main="Informedness")
# Find the threshold corresponding to highest informedness
threshold_top <- threshold_s[which.max(inform_ed)]
tops_forecast <- (forecast_s > threshold_top)
# Calculate area under ROC curve (AUC)
error_rates <- rbind(c(1, 0), error_rates)
error_rates <- rbind(error_rates, c(0, 1))
true_pos <- (1 - error_rates[, "typeII"])
true_pos <- (true_pos + rutils::lag_it(true_pos))/2
false_pos <- rutils::diff_it(error_rates[, "typeI"])
abs(sum(true_pos*false_pos))
# Plot ROC Curve for stock tops
x11(width=5, height=5)
plot(x=error_rates[, "typeI"], y=1-error_rates[, "typeII"],
     xlab="FALSE positive rate", ylab="TRUE positive rate",
     main="ROC Curve for Stock Tops", type="l", lwd=3, col="blue")
abline(a=0.0, b=1.0, lwd=3, col="orange")
# Define design matrix for tops including intercept column
de_sign <- cbind(bottom_s, intercept=rep(1, NROW(bottom_s)),
           volat_scores, volume_scores)
# Define regression formula
col_names <- colnames(de_sign)
for_mula <- as.formula(paste(paste(col_names[1],
  paste(col_names[-1], collapse="+"), sep=" ~ "), "-1"))
# Fit in-sample logistic regression for tops
g_lm <- glm(for_mula, data=de_sign, family=binomial(logit))
summary(g_lm)
# Calculate in-sample forecast from logistic regression model
pre_dict <- drop(de_sign[, -1] %*% g_lm$coefficients)
forecast_s <- 1/(1+exp(-pre_dict))
# Calculate error rates
error_rates <- sapply(threshold_s, con_fuse,
  actual=!bottom_s, forecasts=forecast_s)  # end sapply
error_rates <- t(error_rates)
rownames(error_rates) <- threshold_s
# Calculate the informedness
inform_ed <- 2 - rowSums(error_rates[, c("typeI", "typeII")])
plot(threshold_s, inform_ed, t="l", main="Informedness")
# Find the threshold corresponding to highest informedness
threshold_bottom <- threshold_s[which.max(inform_ed)]
bottoms_forecast <- (forecast_s > threshold_bottom)
# Calculate area under ROC curve (AUC)
error_rates <- rbind(c(1, 0), error_rates)
error_rates <- rbind(error_rates, c(0, 1))
true_pos <- (1 - error_rates[, "typeII"])
true_pos <- (true_pos + rutils::lag_it(true_pos))/2
false_pos <- rutils::diff_it(error_rates[, "typeI"])
abs(sum(true_pos*false_pos))
# Plot ROC Curve for stock tops
x11(width=5, height=5)
plot(x=error_rates[, "typeI"], y=1-error_rates[, "typeII"],
     xlab="FALSE positive rate", ylab="TRUE positive rate",
     main="ROC Curve for Stock Bottoms", type="l", lwd=3, col="blue")
abline(a=0.0, b=1.0, lwd=3, col="orange")
# Simulate in-sample VTI strategy
po_s <- rep(NA_integer_, NROW(re_turns))
po_s[1] <- 0
po_s[tops_forecast] <- (-1)
po_s[bottoms_forecast] <- 1
po_s <- zoo::na.locf(po_s)
po_s <- rutils::lag_it(po_s)
pnl_s <- re_turns*po_s
# Plot dygraph of in-sample VTI strategy
weal_th <- cbind(re_turns, pnl_s)
colnames(weal_th) <- c("VTI", "Strategy")
dygraphs::dygraph(cumsum(weal_th), main="Logistic Strategy Using Top and Bottom Labels") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Fit logistic regression over training data
set.seed(1121)  # Reset random number generator
n_rows <- NROW(Default)
sampl_e <- sample.int(n=n_rows, size=n_rows/2)
train_data <- Default[sampl_e, ]
g_lm <- glm(for_mula, data=train_data, family=binomial(logit))
# Forecast over test data out-of-sample
test_data <- Default[-sampl_e, ]
forecast_s <- predict(g_lm, newdata=test_data, type="response")
# Calculate confusion matrix out-of-sample
table(actual=!test_data$de_fault,
forecast=(forecast_s < thresh_old))
# Calculate VTI percentage returns
clos_e <- log(na.omit(rutils::etf_env$price_s$VTI))
re_turns <- rutils::diff_it(clos_e)
# Define look-back window
look_back <- 11
# Calculate time series of medians
medi_an <- roll::roll_median(clos_e, width=look_back)
# medi_an <- TTR::runMedian(clos_e, n=look_back)
# Calculate time series of MAD
ma_d <- HighFreq::roll_var(clos_e, look_back=look_back, method="nonparametric")
# ma_d <- TTR::runMAD(clos_e, n=look_back)
# Calculate time series of z-scores
z_scores <- (clos_e - medi_an)/ma_d
z_scores[1:look_back, ] <- 0
tail(z_scores, look_back)
range(z_scores)
# Define threshold value
thresh_old <- sum(abs(range(z_scores)))/8
# Simulate VTI strategy
position_s <- rep(NA_integer_, NROW(clos_e))
position_s[1] <- 0
position_s[z_scores < -thresh_old] <- 1
position_s[z_scores > thresh_old] <- (-1)
position_s <- zoo::na.locf(position_s)
position_s <- rutils::lag_it(position_s)
pnl_s <- re_turns*position_s
# Plot dygraph of Hampel strategy pnl_s
weal_th <- cbind(re_turns, pnl_s)
colnames(weal_th) <- c("VTI", "Strategy")
dygraphs::dygraph(cumsum(weal_th), main="VTI Hampel Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Extract log VTI prices
oh_lc <- rutils::etf_env$VTI
clos_e <- log(quantmod::Cl(oh_lc))
colnames(clos_e) <- "VTI"
n_rows <- NROW(clos_e)
# Calculate EWMA weights
look_back <- 333
lamb_da <- 0.004
weight_s <- exp(-lamb_da*(1:look_back))
weight_s <- weight_s/sum(weight_s)
# Calculate EWMA prices
ew_ma <- HighFreq::roll_wsum(clos_e, weights=weight_s)
# Copy over NA values
ew_ma <- zoo::na.locf(ew_ma, fromLast=TRUE)
price_s <- cbind(clos_e, ew_ma)
colnames(price_s) <- c("VTI", "VTI EWMA")
# Dygraphs plot with custom line colors
col_names <- colnames(price_s)
dygraphs::dygraph(price_s["2007/"], main="VTI EWMA Prices") %>%
  dySeries(name=col_names[1], label=col_names[1], strokeWidth=1, col="blue") %>%
  dySeries(name=col_names[2], label=col_names[2], strokeWidth=4, col="red") %>%
  dyLegend(show="always", width=500)
# Standard plot of  EWMA prices with custom line colors
x11(width=6, height=5)
plot_theme <- chart_theme()
col_ors <- c("blue", "red")
plot_theme$col$line.col <- col_ors
quantmod::chart_Series(price_s["2007/"], theme=plot_theme,
       lwd=2, name="VTI EWMA Prices")
legend("topleft", legend=colnames(price_s),
 inset=0.1, bg="white", lty=1, lwd=6, cex=0.8,
 col=plot_theme$col$line.col, bty="n")
# Calculate positions, either: -1, 0, or 1
in_dic <- sign(clos_e - ew_ma)
po_s <- rutils::lag_it(in_dic, lagg=1)
# Create colors for background shading
date_s <- (rutils::diff_it(po_s) != 0)
shad_e <- po_s[date_s]
date_s <- c(index(shad_e), end(po_s))
shad_e <- ifelse(drop(zoo::coredata(shad_e)) == 1, "lightgreen", "antiquewhite")
# Create dygraph object without plotting it
dy_graph <- dygraphs::dygraph(price_s["2007/"], main="VTI EWMA Prices") %>%
  dySeries(name=col_names[1], label=col_names[1], strokeWidth=1, col="blue") %>%
  dySeries(name=col_names[2], label=col_names[2], strokeWidth=4, col="red") %>%
  dyLegend(show="always", width=500)
# Add shading to dygraph object
for (i in 1:NROW(shad_e)) {
    dy_graph <- dy_graph %>% dyShading(from=date_s[i], to=date_s[i+1], color=shad_e[i])
}  # end for
# Plot the dygraph object
dy_graph
# Equivalent code to the above
# Determine trade dates right after EWMA has crossed prices
in_dic <- sign(clos_e - ew_ma)
date_s <- (rutils::diff_it(in_dic) != 0)
date_s <- which(date_s) + 1
date_s <- date_s[date_s < n_rows]
# Calculate positions, either: -1, 0, or 1
po_s <- rep(NA_integer_, n_rows)
po_s[1] <- 0
po_s[date_s] <- in_dic[date_s-1]
po_s <- zoo::na.locf(po_s, na.rm=FALSE)
po_s <- xts::xts(po_s, order.by=index(clos_e))
# Create indicator for background shading
shad_e <- po_s[date_s]
date_s <- index(shad_e)
date_s <- c(date_s, end(po_s))
shad_e <- ifelse(drop(zoo::coredata(shad_e)) == 1, "lightgreen", "antiquewhite")
# Standard plot of EWMA prices with position shading
x11(width=6, height=5)
quantmod::chart_Series(price_s["2007/"], theme=plot_theme,
       lwd=2, name="VTI EWMA Prices")
add_TA(po_s > 0, on=-1, col="lightgreen", border="lightgreen")
add_TA(po_s < 0, on=-1, col="lightgrey", border="lightgrey")
legend("topleft", legend=colnames(price_s),
 inset=0.1, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")
# Calculate daily profits and losses of EWMA strategy
vt_i <- rutils::diff_it(clos_e)  # VTI returns
pnl_s <- vt_i*po_s
colnames(pnl_s) <- "EWMA"
weal_th <- cbind(vt_i, pnl_s)
colnames(weal_th) <- c("VTI", "EWMA PnL")
# Annualized Sharpe ratio of EWMA strategy
sqrt(252)*sapply(weal_th, function (x) mean(x)/sd(x))
# The crossover strategy has a negative correlation to VTI
cor(weal_th)
# Plot dygraph of EWMA strategy wealth
# Create dygraph object without plotting it
col_ors <- c("blue", "red")
dy_graph <- dygraphs::dygraph(cumsum(weal_th["2007/"]), main="Performance of EWMA Strategy") %>%
  dyOptions(colors=col_ors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Add shading to dygraph object
for (i in 1:NROW(shad_e)) {
    dy_graph <- dy_graph %>%
dyShading(from=date_s[i], to=date_s[i+1], color=shad_e[i])
}  # end for
# Plot the dygraph object
dy_graph
# Standard plot of EWMA strategy wealth
x11(width=6, height=5)
plot_theme <- chart_theme()
plot_theme$col$line.col <- col_ors
quantmod::chart_Series(cumsum(weal_th["2007/"]), theme=plot_theme,
       name="Performance of EWMA Strategy")
add_TA(po_s > 0, on=-1, col="lightgreen", border="lightgreen")
add_TA(po_s < 0, on=-1, col="lightgrey", border="lightgrey")
legend("top", legend=colnames(weal_th),
 inset=0.05, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")
# Test EWMA crossover market timing of VTI using Treynor-Mazuy test
de_sign <- cbind(pnl_s, vt_i, vt_i^2)
de_sign <- na.omit(de_sign)
colnames(de_sign) <- c("EWMA", "VTI", "treynor")
mod_el <- lm(EWMA ~ VTI + treynor, data=de_sign)
summary(mod_el)
# Plot residual scatterplot
residual_s <- (de_sign$EWMA - mod_el$coeff[2]*de_sign$VTI)
residual_s <- mod_el$residuals
x11(width=6, height=6)
plot.default(x=de_sign$VTI, y=residual_s, xlab="VTI", ylab="residuals")
title(main="Treynor-Mazuy Market Timing Test\n for EWMA Crossover vs VTI", line=0.5)
# Plot fitted (predicted) response values
fit_ted <- (mod_el$coeff["(Intercept)"] +
        mod_el$coeff["treynor"]*vt_i^2)
points.default(x=de_sign$VTI, y=fit_ted, pch=16, col="red")
text(x=0.05, y=0.8*max(residual_s), paste("EWMA crossover t-value =", round(summary(mod_el)$coeff["treynor", "t value"], 2)))
# Determine trade dates right after EWMA has crossed prices
in_dic <- sign(clos_e - ew_ma)
# Calculate positions from lagged indicator
lagg <- 2
in_dic <- roll::roll_sum(in_dic, width=lagg, min_obs=1)
# Calculate positions, either: -1, 0, or 1
po_s <- rep(NA_integer_, n_rows)
po_s[1] <- 0
po_s <- ifelse(in_dic == lagg, 1, po_s)
po_s <- ifelse(in_dic == (-lagg), -1, po_s)
po_s <- zoo::na.locf(po_s, na.rm=FALSE)
po_s <- xts::xts(po_s, order.by=index(clos_e))
# Lag the positions to trade in next period
po_s <- rutils::lag_it(po_s, lagg=1)
# Calculate PnLs of lagged strategy
pnls_lag <- vt_i*po_s
colnames(pnls_lag) <- "Lagged Strategy"
weal_th <- cbind(pnl_s, pnls_lag)
colnames(weal_th) <- c("EWMA Strategy", "Lagged Strategy")
# Annualized Sharpe ratios of EWMA strategies
sharp_e <- sqrt(252)*sapply(weal_th, function (x) mean(x)/sd(x))
# Plot both strategies
dygraphs::dygraph(cumsum(weal_th["2007/"]), main=paste("EWMA Crossover Strategy, Sharpe", paste(paste(names(sharp_e), round(sharp_e, 3), sep="="), collapse=", "))) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Calculate positions, either: -1, 0, or 1
in_dic <- sign(clos_e - ew_ma)
po_s <- rutils::lag_it(in_dic, lagg=1)
# Calculate daily pnl for days without trades
pnls_lag <- vt_i*po_s
# Determine trade dates right after EWMA has crossed prices
date_s <- which(rutils::diff_it(po_s) != 0)
# Calculate realized pnl for days with trades
op_en <- log(quantmod::Op(oh_lc))
close_lag <- rutils::lag_it(clos_e)
pos_lag <- rutils::lag_it(po_s)
pnls_lag[date_s] <- pos_lag[date_s]*
  (op_en[date_s] - close_lag[date_s])
# Calculate unrealized pnl for days with trades
pnls_lag[date_s] <- pnls_lag[date_s] +
  po_s[date_s]*(clos_e[date_s] - op_en[date_s])
# Calculate the wealth
weal_th <- cbind(vt_i, pnls_lag)
colnames(weal_th) <- c("VTI", "EWMA PnL")
# Annualized Sharpe ratio of EWMA strategy
sqrt(252)*sapply(weal_th, function (x) mean(x)/sd(x))
# The crossover strategy has a negative correlation to VTI
cor(weal_th)
# Plot dygraph of EWMA strategy wealth
dygraphs::dygraph(cumsum(weal_th["2007/"]), main="EWMA Strategy Trading at the Open Price") %>%
  dyOptions(colors=col_ors, strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Standard plot of EWMA strategy wealth
quantmod::chart_Series(weal_th, theme=plot_theme,
       name="EWMA Strategy Trading at the Open Price")
legend("top", legend=colnames(weal_th),
 inset=0.05, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")
# bid_offer equal to 10 bps for liquid ETFs
bid_offer <- 0.001
# Calculate transaction costs
cost_s <- 0.5*bid_offer*abs(pos_lag - po_s)*clos_e
# Plot strategy with transaction costs
weal_th <- cbind(pnl_s, pnl_s - cost_s)
colnames(weal_th) <- c("EWMA", "EWMA w Costs")
col_ors <- c("blue", "red")
dygraphs::dygraph(cumsum(weal_th["2007/"]), main="EWMA Strategy With Transaction Costs") %>%
  dyOptions(colors=col_ors, strokeWidth=2) %>%
  dyLegend(show="always", width=500)
sim_ewma <- function(ohlc, lambda=0.01, look_back=333, bid_offer=0.001,
                trend=1, lagg=1) {
  close <- log(quantmod::Cl(ohlc))
  returns <- rutils::diff_it(close)
  n_rows <- NROW(ohlc)
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
  pos <- rep(NA_integer_, n_rows)
  pos[1] <- 0
  pos <- ifelse(indic == lagg, 1, pos)
  pos <- ifelse(indic == (-lagg), -1, pos)
  pos <- zoo::na.locf(pos, na.rm=FALSE)
  pos <- xts::xts(pos, order.by=index(close))
  # Lag the positions to trade on next day
  pos <- rutils::lag_it(pos, lagg=1)
  # Calculate PnLs of strategy
  pnls <- returns*pos
  costs <- 0.5*bid_offer*abs(rutils::diff_it(pos))*close
  pnls <- (pnls - costs)
  # Calculate strategy returns
  pnls <- cbind(pos, pnls)
  colnames(pnls) <- c("positions", "pnls")
  pnls
}  # end sim_ewma
source("/Users/jerzy/Develop/lecture_slides/scripts/ewma_model.R")
lamb_das <- seq(from=0.001, to=0.008, by=0.001)
# Perform lapply() loop over lamb_das
pnl_s <- lapply(lamb_das, function(lamb_da) {
  # Simulate EWMA strategy and calculate returns
  sim_ewma(ohlc=oh_lc, lambda=lamb_da, look_back=look_back, bid_offer=0, lagg=2)[, "pnls"]
})  # end lapply
pnl_s <- do.call(cbind, pnl_s)
colnames(pnl_s) <- paste0("lambda=", lamb_das)
# Plot dygraph of multiple EWMA strategies
col_ors <- colorRampPalette(c("blue", "red"))(NCOL(pnl_s))
dygraphs::dygraph(cumsum(pnl_s["2007/"]), main="Cumulative Returns of Trend Following EWMA Strategies") %>%
  dyOptions(colors=col_ors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Plot EWMA strategies with custom line colors
x11(width=6, height=5)
plot_theme <- chart_theme()
plot_theme$col$line.col <- col_ors
quantmod::chart_Series(cumsum(pnl_s), theme=plot_theme,
  name="Cumulative Returns of EWMA Strategies")
legend("topleft", legend=colnames(pnl_s), inset=0.1,
  bg="white", cex=0.8, lwd=rep(6, NCOL(pnl_s)),
  col=plot_theme$col$line.col, bty="n")
# Initialize compute cluster under Windows
library(parallel)
clus_ter <- makeCluster(detectCores()-1)
clusterExport(clus_ter,
  varlist=c("oh_lc", "look_back", "sim_ewma"))
# Perform parallel loop over lamb_das under Windows
pnl_s <- parLapply(clus_ter, lamb_das, function(lamb_da) {
  library(quantmod)
  # Simulate EWMA strategy and calculate returns
  sim_ewma(ohlc=oh_lc, lambda=lamb_da, look_back=look_back)[, "pnls"]
})  # end parLapply
stopCluster(clus_ter)  # Stop R processes over cluster under Windows
# Perform parallel loop over lamb_das under Mac-OSX or Linux
pnl_s <- mclapply(lamb_das, function(lamb_da) {
  library(quantmod)
  # Simulate EWMA strategy and calculate returns
  sim_ewma(ohlc=oh_lc, lambda=lamb_da, look_back=look_back)[, "pnls"]
})  # end mclapply
pnl_s <- do.call(cbind, pnl_s)
colnames(pnl_s) <- paste0("lambda=", lamb_das)
# Calculate annualized Sharpe ratios of strategy returns
sharpe_ratios <- sqrt(252)*sapply(pnl_s, function(x_ts) {
  mean(x_ts)/sd(x_ts)
})  # end sapply
# Plot Sharpe ratios
dev.new(width=6, height=5, noRStudioGD=TRUE)
plot(x=lamb_das, y=sharpe_ratios, t="l",
     xlab="lambda", ylab="Sharpe",
     main="Performance of EWMA Trend Following Strategies
     as Function of the Decay Parameter Lambda")
# Find optimal lambda
lamb_da <- lamb_das[which.max(sharpe_ratios)]
# Plot optimal weights
weight_s <- exp(-lamb_da*(1:look_back))
weight_s <- weight_s/sum(weight_s)
plot(weight_s, t="l", xlab="days", ylab="weights",
     main="Optimal Weights of EWMA Trend Following Strategy")
trend_returns <- pnl_s
trend_sharpe <- sharpe_ratios
# Simulate best performing strategy
ewma_trend <- sim_ewma(ohlc=oh_lc, lambda=lamb_da, look_back=look_back, bid_offer=0, lagg=2)
po_s <- ewma_trend[, "positions"]
pnl_s <- ewma_trend[, "pnls"]
weal_th <- cbind(vt_i, pnl_s)
colnames(weal_th) <- c("VTI", "EWMA PnL")
# Create colors for background shading
date_s <- (rutils::diff_it(po_s) != 0)
shad_e <- po_s[date_s]
date_s <- c(index(shad_e), end(po_s))
shad_e <- ifelse(drop(zoo::coredata(shad_e)) == 1, "lightgreen", "antiquewhite")
col_ors <- c("blue", "red")
# Plot dygraph of EWMA strategy wealth
# Create dygraph object without plotting it
dy_graph <- dygraphs::dygraph(cumsum(weal_th["2007/"]), main="Performance of Optimal Trend Following EWMA Strategy") %>%
  dyOptions(colors=col_ors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Add shading to dygraph object
for (i in 1:NROW(shad_e)) {
    dy_graph <- dy_graph %>%
dyShading(from=date_s[i], to=date_s[i+1], color=shad_e[i])
}  # end for
# Plot the dygraph object
dy_graph
# Plot EWMA PnL with position shading
# Standard plot of EWMA strategy wealth
x11(width=6, height=5)
plot_theme <- chart_theme()
plot_theme$col$line.col <- col_ors
quantmod::chart_Series(cumsum(weal_th["2007/"]), theme=plot_theme,
       name="Performance of EWMA Strategy")
add_TA(po_s > 0, on=-1, col="lightgreen", border="lightgreen")
add_TA(po_s < 0, on=-1, col="lightgrey", border="lightgrey")
legend("top", legend=colnames(weal_th),
 inset=0.05, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")
source("/Users/jerzy/Develop/lecture_slides/scripts/ewma_model.R")
lamb_das <- seq(0.05, 1.0, 0.05)
# Perform lapply() loop over lamb_das
pnl_s <- lapply(lamb_das, function(lamb_da) {
  # Simulate EWMA strategy and calculate returns
  sim_ewma(ohlc=oh_lc, lambda=lamb_da, look_back=look_back, trend=(-1))[, "pnls"]
})  # end lapply
pnl_s <- do.call(cbind, pnl_s)
colnames(pnl_s) <- paste0("lambda=", lamb_das)
# Plot dygraph of mean reverting EWMA strategies
column_s <- seq(1, NCOL(pnl_s), by=4)
col_ors <- colorRampPalette(c("blue", "red"))(NROW(column_s))
dygraphs::dygraph(cumsum(pnl_s["2007/", column_s]), main="Cumulative Returns of Mean Reverting EWMA Strategies") %>%
  dyOptions(colors=col_ors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Plot EWMA strategies with custom line colors
x11(width=6, height=5)
plot_theme <- chart_theme()
plot_theme$col$line.col <- col_ors
quantmod::chart_Series(pnl_s[, column_s],
  theme=plot_theme, name="Cumulative Returns of Mean Reverting EWMA Strategies")
legend("topleft", legend=colnames(pnl_s[, column_s]),
  inset=0.1, bg="white", cex=0.8, lwd=6,
  col=plot_theme$col$line.col, bty="n")
# Calculate Sharpe ratios of strategy returns
sharpe_ratios <- sqrt(252)*sapply(pnl_s, function(x_ts) {
  mean(x_ts)/sd(x_ts)
})  # end sapply
plot(x=lamb_das, y=sharpe_ratios, t="l",
     xlab="lambda", ylab="Sharpe",
     main="Performance of EWMA Mean Reverting Strategies
     as Function of the Decay Parameter Lambda")
revert_returns <- pnl_s
revert_sharpe <- sharpe_ratios
# Find optimal lambda
lamb_da <- lamb_das[which.max(sharpe_ratios)]
# Simulate best performing strategy
ewma_revert <- sim_ewma(ohlc=oh_lc, bid_offer=0.0,
  lambda=lamb_da, look_back=look_back, trend=(-1))
po_s <- ewma_revert[, "positions"]
pnl_s <- ewma_revert[, "pnls"]
weal_th <- cbind(vt_i, pnl_s)
colnames(weal_th) <- c("VTI", "EWMA PnL")
# Plot dygraph of EWMA strategy wealth
col_ors <- c("blue", "red")
dygraphs::dygraph(cumsum(weal_th["2007/"]), main="Optimal Mean Reverting EWMA Strategy") %>%
  dyOptions(colors=col_ors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Standard plot of EWMA strategy wealth
x11(width=6, height=5)
plot_theme <- chart_theme()
plot_theme$col$line.col <- col_ors
quantmod::chart_Series(cumsum(weal_th["2007/"]), theme=plot_theme,
       name="Optimal Mean Reverting EWMA Strategy")
add_TA(po_s > 0, on=-1, col="lightgreen", border="lightgreen")
add_TA(po_s < 0, on=-1, col="lightgrey", border="lightgrey")
legend("top", legend=colnames(weal_th),
 inset=0.05, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")
# Calculate correlation between trend following and mean reverting strategies
trend_ing <- ewma_trend[, "pnls"]
colnames(trend_ing) <- "trend"
revert_ing <- ewma_revert[, "pnls"]
colnames(revert_ing) <- "revert"
cor(cbind(vt_i, trend_ing, revert_ing))
# Calculate combined strategy
com_bined <- (vt_i + trend_ing + revert_ing)/3
colnames(com_bined) <- "combined"
# Calculate annualized Sharpe ratio of strategy returns
re_turns <- cbind(vt_i, trend_ing, revert_ing, com_bined)
colnames(re_turns) <- c("VTI", "Trending", "Reverting", "EWMA combined")
sqrt(252)*sapply(re_turns, function(x_ts) mean(x_ts)/sd(x_ts))
# Plot dygraph of EWMA strategy wealth
col_ors <- c("blue", "red", "green", "purple")
dygraphs::dygraph(cumsum(re_turns["2007/"]), main="Performance of Combined EWMA Strategies") %>%
  dyOptions(colors=col_ors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Standard plot of EWMA strategy wealth
plot_theme <- chart_theme()
plot_theme$col$line.col <- col_ors
quantmod::chart_Series(pnl_s, theme=plot_theme,
       name="Performance of Combined EWMA Strategies")
legend("topleft", legend=colnames(pnl_s),
 inset=0.05, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")
weight_s <- c(trend_sharpe, revert_sharpe)
weight_s[weight_s<0] <- 0
weight_s <- weight_s/sum(weight_s)
re_turns <- cbind(trend_returns, revert_returns)
re_turns <- re_turns %*% weight_s
re_turns <- xts::xts(re_turns, order.by=index(vt_i))
re_turns <- cbind(vt_i, re_turns)
colnames(re_turns) <- c("VTI", "EWMA PnL")
# Plot dygraph of EWMA strategy wealth
col_ors <- c("blue", "red")
dygraphs::dygraph(cumsum(re_turns["2007/"]), main="Performance of Ensemble of EWMA Strategies") %>%
  dyOptions(colors=col_ors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Standard plot of EWMA strategy wealth
plot_theme <- chart_theme()
plot_theme$col$line.col <- col_ors
quantmod::chart_Series(cumsum(re_turns["2007/"]), theme=plot_theme,
       name="Performance of Ensemble of EWMA Strategies")
legend("topleft", legend=colnames(pnl_s),
 inset=0.05, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")
# Calculate fast and slow EWMAs
look_back <- 333
lambda1 <- 0.04
lambda2 <- 0.004
weight_s <- exp(-lambda1*(1:look_back))
weight_s <- weight_s/sum(weight_s)
ewma1 <- HighFreq::roll_wsum(clos_e, weights=weight_s)
weight_s <- exp(-lambda2*(1:look_back))
weight_s <- weight_s/sum(weight_s)
ewma2 <- HighFreq::roll_wsum(clos_e, weights=weight_s)
# Calculate EWMA prices
price_s <- cbind(clos_e, ewma1, ewma2)
colnames(price_s) <- c("VTI", "EWMA fast", "EWMA slow")
# Calculate positions, either: -1, 0, or 1
in_dic <- sign(ewma1 - ewma2)
lagg <- 2
in_dic <- roll::roll_sum(in_dic, width=lagg, min_obs=1)
po_s <- rep(NA_integer_, n_rows)
po_s[1] <- 0
po_s <- ifelse(in_dic == lagg, 1, po_s)
po_s <- ifelse(in_dic == (-lagg), -1, po_s)
po_s <- zoo::na.locf(po_s, na.rm=FALSE)
po_s <- xts::xts(po_s, order.by=index(clos_e))
po_s <- rutils::lag_it(po_s, lagg=1)
# Create colors for background shading
date_s <- (rutils::diff_it(po_s) != 0)
shad_e <- po_s[date_s]
date_s <- c(index(shad_e), end(po_s))
shad_e <- ifelse(drop(zoo::coredata(shad_e)) == 1, "lightgreen", "antiquewhite")
# Plot dygraph
col_names <- colnames(price_s)
dy_graph <- dygraphs::dygraph(price_s["2007/"], main="VTI Dual EWMA Prices") %>%
  dySeries(name=col_names[1], label=col_names[1], strokeWidth=1, col="blue") %>%
  dySeries(name=col_names[2], label=col_names[2], strokeWidth=4, col="red") %>%
  dySeries(name=col_names[3], label=col_names[3], strokeWidth=4, col="purple") %>%
  dyLegend(show="always", width=500)
for (i in 1:NROW(shad_e)) {
    dy_graph <- dy_graph %>% dyShading(from=date_s[i], to=date_s[i+1], color=shad_e[i])
}  # end for
dy_graph
# Calculate daily profits and losses of strategy
pnl_s <- vt_i*po_s
colnames(pnl_s) <- "Strategy"
weal_th <- cbind(vt_i, pnl_s)
# Annualized Sharpe ratio of Dual EWMA strategy
sharp_e <- sqrt(252)*sapply(weal_th, function (x) mean(x)/sd(x))
# The crossover strategy has a negative correlation to VTI
cor(weal_th)
# Plot Dual EWMA strategy
dy_graph <- dygraphs::dygraph(cumsum(weal_th["2007/"]), main=paste("EWMA Dual Crossover Strategy, Sharpe", paste(paste(names(sharp_e), round(sharp_e, 3), sep="="), collapse=", "))) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=1)
# Add shading to dygraph object
for (i in 1:NROW(shad_e)) {
    dy_graph <- dy_graph %>% dyShading(from=date_s[i], to=date_s[i+1], color=shad_e[i])
}  # end for
# Plot the dygraph object
dy_graph
sim_ewma2 <- function(ohlc, lambda1=0.1, lambda2=0.01, look_back=333,
                bid_offer=0.001, trend=1, lagg=1) {
  close <- log(quantmod::Cl(ohlc))
  returns <- rutils::diff_it(close)
  n_rows <- NROW(ohlc)
  # Calculate EWMA prices
  weights <- exp(-lambda1*(1:look_back))
  weights <- weights/sum(weights)
  ewma1 <- HighFreq::roll_wsum(clos_e, weights=weights)
  weights <- exp(-lambda2*(1:look_back))
  weights <- weights/sum(weights)
  ewma2 <- HighFreq::roll_wsum(clos_e, weights=weights)
  # Calculate positions, either: -1, 0, or 1
  indic <- sign(ewma1 - ewma2)
  if (lagg > 1) {
    indic <- roll::roll_sum(indic, width=lagg, min_obs=1)
    indic[1:lagg] <- 0
  }  # end if
  pos <- rep(NA_integer_, n_rows)
  pos[1] <- 0
  pos <- ifelse(indic == lagg, 1, pos)
  pos <- ifelse(indic == (-lagg), -1, pos)
  pos <- zoo::na.locf(pos, na.rm=FALSE)
  pos <- xts::xts(pos, order.by=index(close))
  # Lag the positions to trade on next day
  pos <- rutils::lag_it(pos, lagg=1)
  # Calculate PnLs of strategy
  pnls <- returns*pos
  costs <- 0.5*bid_offer*abs(rutils::diff_it(pos))*close
  pnls <- (pnls - costs)
  # Calculate strategy returns
  pnls <- cbind(pos, pnls)
  colnames(pnls) <- c("positions", "pnls")
  pnls
}  # end sim_ewma2
source("/Users/jerzy/Develop/lecture_slides/scripts/ewma_model.R")
lamb_das1 <- seq(from=0.05, to=0.15, by=0.01)
lamb_das2 <- seq(from=0.03, to=0.1, by=0.01)
# Perform sapply() loops over lambdas
sharpe_ratios <- sapply(lamb_das1, function(lambda1) {
  sapply(lamb_das2, function(lambda2) {
    if (lambda1 > lambda2) {
# Simulate Dual EWMA strategy
pnl_s <- sim_ewma2(ohlc=oh_lc, lambda1=lambda1, lambda2=lambda2,
                    look_back=look_back, bid_offer=0.0, trend=1, lagg=2)[, "pnls"]
sqrt(252)*mean(pnl_s)/sd(pnl_s)
    } else NA
  })  # end sapply
})  # end sapply
colnames(sharpe_ratios) <- lamb_das1
rownames(sharpe_ratios) <- lamb_das2
# Calculate the PnLs for the optimal strategy
whi_ch <- which(sharpe_ratios == max(sharpe_ratios, na.rm=TRUE), arr.ind=TRUE)
lambda1 <- lamb_das1[whi_ch[2]]
lambda2 <- lamb_das2[whi_ch[1]]
pnl_s <- sim_ewma2(ohlc=oh_lc, lambda1=lambda1, lambda2=lambda2,
              look_back=look_back, bid_offer=0.0, trend=1, lagg=2)[, "pnls"]
weal_th <- cbind(vt_i, pnl_s)
# Annualized Sharpe ratio of Dual EWMA strategy
sharp_e <- sqrt(252)*sapply(weal_th, function (x) mean(x)/sd(x))
# The crossover strategy has a negative correlation to VTI
cor(weal_th)
# Plot Optimal Dual EWMA strategy
dy_graph <- dygraphs::dygraph(cumsum(weal_th["2007/"]), main=paste("Optimal EWMA Dual Crossover Strategy, Sharpe", paste(paste(names(sharp_e), round(sharp_e, 3), sep="="), collapse=", "))) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=1)
# Add shading to dygraph object
for (i in 1:NROW(shad_e)) {
    dy_graph <- dy_graph %>% dyShading(from=date_s[i], to=date_s[i+1], color=shad_e[i])
}  # end for
# Plot the dygraph object
dy_graph
# Calculate log OHLC prices and volumes
oh_lc <- rutils::etf_env$VTI
clos_e <- log(quantmod::Cl(oh_lc))
colnames(clos_e) <- "VTI"
vol_ume <- quantmod::Vo(oh_lc)
colnames(vol_ume) <- "Volume"
n_rows <- NROW(clos_e)
# Calculate the VWAP prices
look_back <- 170
vwap <- roll::roll_sum(clos_e*vol_ume, width=look_back, min_obs=1)
volume_roll <- roll::roll_sum(vol_ume, width=look_back, min_obs=1)
vwap <- vwap/volume_roll
colnames(vwap) <- "VWAP"
price_s <- cbind(clos_e, vwap)
# Dygraphs plot with custom line colors
col_names <- colnames(price_s)
dygraphs::dygraph(price_s["2007/"], main="VTI VWAP Prices") %>%
  dySeries(name=col_names[1], label=col_names[1], strokeWidth=1, col="blue") %>%
  dySeries(name=col_names[2], label=col_names[2], strokeWidth=4, col="red") %>%
  dyLegend(show="always", width=500)
# Plot VWAP prices with custom line colors
x11(width=6, height=5)
col_ors <- c("blue", "red")
plot_theme <- chart_theme()
plot_theme$col$line.col <- col_ors
quantmod::chart_Series(price_s["2009"], theme=plot_theme,
       lwd=2, name="VTI VWAP Prices")
legend("bottomright", legend=colnames(price_s),
 inset=0.1, bg="white", lty=1, lwd=6, cex=0.8,
 col=plot_theme$col$line.col, bty="n")
# Calculate positions from lagged indicator
in_dic <- sign(clos_e - vwap)
lagg <- 2
in_dic <- roll::roll_sum(in_dic, width=lagg, min_obs=1)
# Calculate positions, either: -1, 0, or 1
po_s <- rep(NA_integer_, n_rows)
po_s[1] <- 0
po_s <- ifelse(in_dic == lagg, 1, po_s)
po_s <- ifelse(in_dic == (-lagg), -1, po_s)
po_s <- zoo::na.locf(po_s, na.rm=FALSE)
po_s <- xts::xts(po_s, order.by=index(clos_e))
# Lag the positions to trade in next period
po_s <- rutils::lag_it(po_s, lagg=1)
# Calculate PnLs of VWAP strategy
vt_i <- rutils::diff_it(clos_e)  # VTI returns
pnl_s <- vt_i*po_s
colnames(pnl_s) <- "VWAP Strategy"
weal_th <- cbind(vt_i, pnl_s)
colnames(weal_th) <- c("VTI", "VWAP Strategy")
col_names <- colnames(weal_th)
# Annualized Sharpe ratios of VTI and VWAP strategy
sharp_e <- sqrt(252)*sapply(weal_th, function (x) mean(x)/sd(x))
# Create colors for background shading
date_s <- (rutils::diff_it(po_s) != 0)
shad_e <- po_s[date_s]
date_s <- c(index(shad_e), end(po_s))
shad_e <- ifelse(drop(zoo::coredata(shad_e)) == 1, "lightgreen", "antiquewhite")
# Plot dygraph of VWAP strategy
# Create dygraph object without plotting it
dy_graph <- dygraphs::dygraph(cumsum(weal_th["2007/"]), main=paste("VWAP Crossover Strategy, Sharpe", paste(paste(names(sharp_e), round(sharp_e, 3), sep="="), collapse=", "))) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Add shading to dygraph object
for (i in 1:NROW(shad_e)) {
    dy_graph <- dy_graph %>% dyShading(from=date_s[i], to=date_s[i+1], color=shad_e[i])
}  # end for
# Plot the dygraph object
dy_graph
# Calculate correlation of VWAP strategy with VTI
cor(vt_i, pnl_s)
# Combine VWAP strategy with VTI
weal_th <- cbind(vt_i, pnl_s, 0.5*(vt_i+pnl_s))
colnames(weal_th) <- c("VTI", "VWAP", "Combined")
sharp_e <- sqrt(252)*sapply(weal_th, function (x) mean(x)/sd(x))
# Plot dygraph of VWAP strategy combined with VTI
# wippp
col_names <- colnames(weal_th)
dygraphs::dygraph(cumsum(weal_th), paste("VWAP Crossover Strategy, Sharpe", paste(paste(names(sharp_e), round(sharp_e, 3), sep="="), collapse=", "))) %>%
  dySeries(name=col_names[1], label=col_names[1], col="blue", strokeWidth=1) %>%
  dySeries(name=col_names[2], label=col_names[2], col="red", strokeWidth=1) %>%
  dySeries(name=col_names[3], label=col_names[3], col="purple", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Or
dygraphs::dygraph(cumsum(weal_th),
  main=paste("VWAP Crossover Strategy, Sharpe", paste(paste(names(sharp_e), round(sharp_e, 3), sep="="), collapse=", "))) %>%
  dyOptions(colors=c("blue", "red", "purple"), strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Test VWAP crossover market timing of VTI using Treynor-Mazuy test
de_sign <- cbind(pnl_s, vt_i, vt_i^2)
de_sign <- na.omit(de_sign)
colnames(de_sign) <- c("VWAP", "VTI", "treynor")
mod_el <- lm(VWAP ~ VTI + treynor, data=de_sign)
summary(mod_el)
# Plot residual scatterplot
residual_s <- (de_sign$VWAP - mod_el$coeff[2]*de_sign$VTI)
residual_s <- mod_el$residuals
x11(width=6, height=6)
plot.default(x=de_sign$VTI, y=residual_s, xlab="VTI", ylab="residuals")
title(main="Treynor-Mazuy Market Timing Test\n for VWAP Crossover vs VTI", line=0.5)
# Plot fitted (predicted) response values
fit_ted <- (mod_el$coeff["(Intercept)"] + mod_el$coeff["treynor"]*vt_i^2)
points.default(x=de_sign$VTI, y=fit_ted, pch=16, col="red")
text(x=0.05, y=0.8*max(residual_s), paste("VWAP crossover t-value =", round(summary(mod_el)$coeff["treynor", "t value"], 2)))
sim_vwap <- function(ohlc, look_back=333, bid_offer=0.001, trend=1, lagg=1) {
  close <- log(quantmod::Cl(ohlc))
  vol_ume <- quantmod::Vo(oh_lc)
  returns <- rutils::diff_it(close)
  n_rows <- NROW(ohlc)
  # Calculate VWAP prices
  vwap <- roll::roll_sum(clos_e*vol_ume, width=look_back, min_obs=1)
  volume_roll <- roll::roll_sum(vol_ume, width=look_back, min_obs=1)
  vwap <- vwap/volume_roll
  # Calculate the indicator
  indic <- trend*sign(close - vwap)
  if (lagg > 1) {
    indic <- roll::roll_sum(indic, width=lagg, min_obs=1)
    indic[1:lagg] <- 0
  }  # end if
  # Calculate positions, either: -1, 0, or 1
  pos <- rep(NA_integer_, n_rows)
  pos[1] <- 0
  pos <- ifelse(indic == lagg, 1, pos)
  pos <- ifelse(indic == (-lagg), -1, pos)
  pos <- zoo::na.locf(pos, na.rm=FALSE)
  pos <- xts::xts(pos, order.by=index(close))
  # Lag the positions to trade on next day
  pos <- rutils::lag_it(pos, lagg=1)
  # Calculate PnLs of strategy
  pnls <- returns*pos
  costs <- 0.5*bid_offer*abs(rutils::diff_it(pos))*close
  pnls <- (pnls - costs)
  # Calculate strategy returns
  pnls <- cbind(pos, pnls)
  colnames(pnls) <- c("positions", "pnls")
  pnls
}  # end sim_vwap
source("/Users/jerzy/Develop/lecture_slides/scripts/ewma_model.R")
look_backs <- seq(70, 200, 10)
# Perform lapply() loop over lamb_das
pnl_s <- lapply(look_backs, function(look_back) {
  # Simulate VWAP strategy and calculate returns
  sim_vwap(ohlc=oh_lc, look_back=look_back, bid_offer=0, lagg=2)[, "pnls"]
})  # end lapply
pnl_s <- do.call(cbind, pnl_s)
colnames(pnl_s) <- paste0("look_back=", look_backs)
# Plot dygraph of multiple VWAP strategies
col_ors <- colorRampPalette(c("blue", "red"))(NCOL(pnl_s))
dygraphs::dygraph(cumsum(pnl_s["2007/"]), main="Cumulative Returns of Trend Following VWAP Strategies") %>%
  dyOptions(colors=col_ors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Plot VWAP strategies with custom line colors
x11(width=6, height=5)
plot_theme <- chart_theme()
plot_theme$col$line.col <- col_ors
quantmod::chart_Series(cumsum(pnl_s), theme=plot_theme,
  name="Cumulative Returns of VWAP Strategies")
legend("topleft", legend=colnames(pnl_s), inset=0.1,
  bg="white", cex=0.8, lwd=rep(6, NCOL(pnl_s)),
  col=plot_theme$col$line.col, bty="n")
# Calculate a vector of daily VTI log returns
vt_i <- na.omit(rutils::etf_env$re_turns$VTI)
date_s <- index(vt_i)
vt_i <- as.numeric(vt_i)
n_rows <- NROW(vt_i)
# Define predictor matrix for forecasting
order_max <- 5
predic_tor <- sapply(1:order_max, rutils::lag_it, in_put=vt_i)
predic_tor <- cbind(rep(1, n_rows), predic_tor)
colnames(predic_tor) <- paste0("pred_", 1:NCOL(predic_tor))
res_ponse <- vt_i
# Calculate forecasts as function of the AR order
forecast_s <- lapply(2:NCOL(predic_tor), function(or_der) {
  # Calculate fitted coefficients
  in_verse <- MASS::ginv(predic_tor[, 1:or_der])
  co_eff <- drop(in_verse %*% res_ponse)
  # Calculate in-sample forecasts of vt_i
  drop(predic_tor[, 1:or_der] %*% co_eff)
})  # end lapply
names(forecast_s) <- paste0("n=", 2:NCOL(predic_tor))
# Calculate mean squared errors
ms_e <- sapply(forecast_s, function(x) {
  c(mse=mean((vt_i - x)^2), cor=cor(vt_i, x))
})  # end sapply
ms_e <- t(ms_e)
rownames(ms_e) <- names(forecast_s)
# Plot forecasting MSE
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
plot(x=2:NCOL(predic_tor), y=ms_e[, 1],
  xlab="AR(n) order", ylab="MSE", type="l", lwd=2,
  main="MSE of In-sample AR(n) Forecasting Model for VTI")
in_sample <- 1:(n_rows %/% 2)
out_sample <- (n_rows %/% 2 + 1):n_rows
# Calculate forecasts as function of the AR order
forecast_s <- lapply(2:NCOL(predic_tor), function(or_der) {
  # Calculate fitted coefficients
  in_verse <- MASS::ginv(predic_tor[in_sample, 1:or_der])
  co_eff <- drop(in_verse %*% res_ponse[in_sample])
  # Calculate out-of-sample forecasts of vt_i
  drop(predic_tor[out_sample, 1:or_der] %*% co_eff)
})  # end lapply
names(forecast_s) <- paste0("n=", 2:NCOL(predic_tor))
# Calculate mean squared errors
ms_e <- sapply(forecast_s, function(x) {
  c(mse=mean((vt_i[out_sample] - x)^2), cor=cor(vt_i[out_sample], x))
})  # end sapply
ms_e <- t(ms_e)
rownames(ms_e) <- names(forecast_s)
# Plot forecasting MSE
plot(x=2:NCOL(predic_tor), y=ms_e[, 1],
  xlab="AR(n) order", ylab="MSE", type="l", lwd=2,
  main="MSE of Out-of-sample AR(n) Forecasting Model for VTI")
# Calculate out-of-sample PnLs
pnl_s <- sapply(forecast_s, function(x) {
  cumsum(sign(x)*vt_i[out_sample])
})  # end sapply
colnames(pnl_s) <- names(forecast_s)
pnl_s <- xts::xts(pnl_s, date_s[out_sample])
# Plot dygraph of out-of-sample PnLs
color_s <- colorRampPalette(c("red", "blue"))(NCOL(pnl_s[, 1:4]))
col_names <- colnames(pnl_s[, 1:4])
dygraphs::dygraph(pnl_s[, 1:4],
  main="Autoregressive Strategies Performance With Different Order Parameters") %>%
  dyOptions(colors=color_s, strokeWidth=2) %>%
  dyLegend(width=500)
# Define predictor as a rolling mean
n_agg <- 5
predic_tor <- roll::roll_mean(vt_i, width=n_agg, min_obs=1)
res_ponse <- vt_i
# Define predictor matrix for forecasting
predic_tor <- sapply(1+n_agg*(0:order_max), rutils::lag_it,
               in_put=predic_tor)
predic_tor <- cbind(rep(1, n_rows), predic_tor)
# Calculate forecasts as function of the AR order
forecast_s <- lapply(2:NCOL(predic_tor), function(or_der) {
  in_verse <- MASS::ginv(predic_tor[in_sample, 1:or_der])
  co_eff <- drop(in_verse %*% res_ponse[in_sample])
  drop(predic_tor[out_sample, 1:or_der] %*% co_eff)
})  # end lapply
names(forecast_s) <- paste0("n=", 2:NCOL(predic_tor))
# Calculate out-of-sample PnLs
pnl_s <- sapply(forecast_s, function(x) {
  cumsum(sign(x)*vt_i[out_sample])
})  # end sapply
colnames(pnl_s) <- names(forecast_s)
pnl_s <- xts::xts(pnl_s, date_s[out_sample])
# Plot dygraph of out-of-sample PnLs
dygraphs::dygraph(pnl_s[, 1:4],
  main="Autoregressive Strategies Performance Using Rolling Average Predictor") %>%
  dyOptions(colors=color_s, strokeWidth=2) %>%
  dyLegend(width=500)
# Calculate PnLs using the average of past forecasts
n_agg <- 5
pnl_s <- sapply(forecast_s, function(x) {
  x <- roll::roll_mean(x, width=n_agg, min_obs=1)
  cumsum(sign(x)*vt_i[out_sample])
})  # end sapply
colnames(pnl_s) <- names(forecast_s)
pnl_s <- xts::xts(pnl_s, date_s[out_sample])
# Plot dygraph of out-of-sample PnLs
dygraphs::dygraph(pnl_s[, 1:4],
  main="Autoregressive Strategies Performance Using Rolling Average Forecasts") %>%
  dyOptions(colors=color_s, strokeWidth=2) %>%
  dyLegend(width=500)
library(rutils)
# Calculate a vector of daily VTI log returns
price_s <- log(quantmod::Cl(rutils::etf_env$VTI))
vt_i <- rutils::diff_it(price_s)
vt_i <- as.numeric(vt_i)
n_rows <- NROW(vt_i)
# Define predictor matrix for forecasting
order_max <- 5
de_sign <- sapply(1:order_max, rutils::lag_it, in_put=vt_i)
colnames(de_sign) <- paste0("pred_", 1:NCOL(de_sign))
# Add response equal to vt_i
de_sign <- cbind(vt_i, de_sign)
colnames(de_sign)[1] <- "response"
# Specify length of look-back interval
look_back <- 100
# Invert the predictor matrix
rang_e <- (n_rows-look_back):(n_rows-1)
design_inv <- MASS::ginv(de_sign[rang_e, -1])
# Calculate fitted coefficients
co_eff <- drop(design_inv %*% de_sign[rang_e, 1])
# Calculate forecast of vt_i for n_rows
drop(de_sign[n_rows, -1] %*% co_eff)
# Compare with actual value
de_sign[n_rows, 1]
# Calculate a vector of daily VTI log returns
vt_i <- na.omit(rutils::etf_env$re_turns$VTI)
date_s <- index(vt_i)
vt_i <- as.numeric(vt_i)
n_rows <- NROW(vt_i)
# Define response equal to vt_i
res_ponse <- vt_i
# Define predictor as a rolling sum
n_agg <- 5
predic_tor <- rutils::roll_sum(vt_i, look_back=n_agg)
# Define predictor matrix for forecasting
order_max <- 5
predic_tor <- sapply(1+n_agg*(0:order_max), rutils::lag_it,
               in_put=predic_tor)
predic_tor <- cbind(rep(1, n_rows), predic_tor)
# Perform rolling forecasting
look_back <- 100
forecast_s <- sapply((look_back+1):n_rows, function(end_p) {
  # Define rolling look-back range
  start_p <- max(1, end_p-look_back)
  # Or expanding look-back range
  # start_p <- 1
  rang_e <- start_p:(end_p-1)
  # Invert the predictor matrix
  design_inv <- MASS::ginv(predic_tor[rang_e, ])
  # Calculate fitted coefficients
  co_eff <- drop(design_inv %*% res_ponse[rang_e])
  # Calculate forecast
  drop(predic_tor[end_p, ] %*% co_eff)
})  # end sapply
# Add warmup period
forecast_s <- c(rep(0, look_back), forecast_s)
# Mean squared error
mean((vt_i - forecast_s)^2)
# Correlation
cor(forecast_s, vt_i)
# Plot forecasting series with legend
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0))
plot(vt_i[(n_rows-look_back):n_rows], col="blue",
     xlab="", ylab="", type="l", lwd=2,
     main="Rolling Forecasting Using AR Model")
lines(forecast_s[(n_rows-look_back):n_rows], col="red", lwd=2)
legend(x="top", legend=c("returns", "forecasts"),
 col=c("blue", "red"), lty=1, lwd=6,
 cex=0.9, bg="white", bty="n")
# Define backtesting function
sim_forecasts <- function(response, n_agg=5,
                    or_der=5, look_back=100) {
  n_rows <- NROW(response)
  # Define predictor as a rolling sum
  predictor <- rutils::roll_sum(response, look_back=n_agg)
  # Define predictor matrix for forecasting
  predictor <- sapply(1+n_agg*(0:or_der), rutils::lag_it,
                 in_put=predictor)
  predictor <- cbind(rep(1, n_rows), predictor)
  # Perform rolling forecasting
  forecast_s <- sapply((look_back+1):n_rows, function(end_p) {
    # Define rolling look-back range
    start_p <- max(1, end_p-look_back)
    # Or expanding look-back range
    # start_p <- 1
    rang_e <- start_p:(end_p-1)
    # Invert the predictor matrix
    design_inv <- MASS::ginv(predictor[rang_e, ])
    # Calculate fitted coefficients
    co_eff <- drop(design_inv %*% response[rang_e])
    # Calculate forecast
    drop(predictor[end_p, ] %*% co_eff)
  })  # end sapply
  # Add warmup period
  forecast_s <- c(rep(0, look_back), forecast_s)
  # Aggregate the forecasts
  rutils::roll_sum(forecast_s, look_back=n_agg)
}  # end sim_forecasts
# Simulate the rolling autoregressive forecasts
forecast_s <- sim_forecasts(response=vt_i, or_der=5, look_back=100)
c(mse=mean((vt_i - forecast_s)^2), cor=cor(vt_i, forecast_s))
library(parallel)  # Load package parallel
# Calculate number of available cores
n_cores <- detectCores() - 1
# Initialize compute cluster under Windows
clus_ter <- makeCluster(n_cores)
# clusterExport(clus_ter, c("star_t", "bar_rier"))
# Perform parallel loop under Windows
look_backs <- seq(20, 600, 40)
forecast_s <- parLapply(clus_ter, look_backs, sim_forecasts,
  response=vt_i, n_agg=5, or_der=5)
# Perform parallel bootstrap under Mac-OSX or Linux
forecast_s <- mclapply(look_backs, sim_forecasts, response=vt_i,
  n_agg=5, or_der=5, mc.cores=n_cores)
# Calculate mean squared errors
ms_e <- sapply(forecast_s, function(x) {
  c(mse=mean((vt_i - x)^2), cor=cor(vt_i, x))
})  # end sapply
ms_e <- t(ms_e)
rownames(ms_e) <- look_backs
# Select optimal look_back interval
look_back <- look_backs[which.min(ms_e[, 1])]
# Plot forecasting MSE
plot(x=look_backs, y=ms_e[, 1],
  xlab="look-back", ylab="MSE", type="l", lwd=2,
  main="MSE of AR Forecasting Model As Function of Look-back")
library(parallel)  # Load package parallel
# Calculate number of available cores
n_cores <- detectCores() - 1
# Initialize compute cluster under Windows
clus_ter <- makeCluster(n_cores)
# clusterExport(clus_ter, c("star_t", "bar_rier"))
# Perform parallel loop under Windows
forecast_s <- parLapply(clus_ter, order_s, sim_forecasts, response=vt_i,
  n_agg=5, look_back=look_back)
stopCluster(clus_ter)  # Stop R processes over cluster under Windows
# Perform parallel bootstrap under Mac-OSX or Linux
order_s <- 2:6
forecast_s <- mclapply(order_s, sim_forecasts, response=vt_i,
  n_agg=5, look_back=look_back, mc.cores=n_cores)
# Calculate mean squared errors
ms_e <- sapply(forecast_s, function(x) {
  c(mse=mean((vt_i - x)^2), cor=cor(vt_i, x))
})  # end sapply
ms_e <- t(ms_e)
rownames(ms_e) <- order_s
# Select optimal order parameter
or_der <- order_s[which.min(ms_e[, 1])]
# Plot forecasting MSE
plot(x=order_s, y=ms_e[, 1],
  xlab="or_der", ylab="MSE", type="l", lwd=2,
  main="MSE of Forecasting Model As Function of AR Order")
# Simulate the rolling autoregressive forecasts
forecast_s <- sim_forecasts(vt_i, or_der=or_der, look_back=look_back)
# Calculate strategy PnLs
pnl_s <- sign(forecast_s)*vt_i
pnl_s <- cbind(vt_i, pnl_s, (vt_i+pnl_s)/2)
colnames(pnl_s) <- c("VTI", "AR_Strategy", "Combined")
cor(pnl_s)
# Annualized Sharpe ratios of VTI and AR strategy
pnl_s <- xts::xts(pnl_s, date_s)
sqrt(252)*sapply(pnl_s, function (x) mean(x)/sd(x))
# Plot the cumulative strategy PnLs
dygraphs::dygraph(cumsum(pnl_s), main="Rolling Autoregressive Strategy") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Calculate PnLs for or_der=5
forecast_s <- sim_forecasts(vt_i, or_der=5, look_back=look_back)
pnls_5 <- cumsum(sign(forecast_s)*vt_i)
# Calculate PnLs for or_der=3
forecast_s <- sim_forecasts(vt_i, or_der=3, look_back=look_back)
pnls_3 <- cumsum(sign(forecast_s)*vt_i)
# Plot the cumulative strategy returns
weal_th <- cbind(pnls_5, pnls_3)
weal_th <- xts::xts(weal_th, date_s)
col_names <- c("AR(5)_Strategy", "AR(3)_Strategy")
colnames(weal_th) <- col_names
dygraphs::dygraph(weal_th, main="Autoregressive Strategies for Different Order Parameters") %>%
  dySeries(name=col_names[1], label=col_names[1], col="blue", strokeWidth=2) %>%
  dySeries(name=col_names[2], label=col_names[2], col="red", strokeWidth=2) %>%
  dyLegend(width=500)
# Calculate PnLs for rolling look-back
forecast_s <- sim_forecasts(vt_i, or_der=3, look_back=look_back, is_rolling=TRUE)
pnls_rolling <- cumsum(sign(forecast_s)*vt_i)
# Calculate PnLs for expanding look-back
forecast_s <- sim_forecasts(vt_i, or_der=3, look_back=look_back, is_rolling=FALSE)
pnls_expanding <- cumsum(sign(forecast_s)*vt_i)
# Plot the cumulative strategy returns
weal_th <- cbind(pnls_rolling, pnls_expanding)
weal_th <- xts::xts(weal_th, date_s)
col_names <- c("AR(3)_Rolling", "AR(3)_Expanding")
colnames(weal_th) <- col_names
dygraphs::dygraph(weal_th, main="Autoregressive Strategies for Expanding Look-back Interval") %>%
  dySeries(name=col_names[1], label=col_names[1], col="blue", strokeWidth=2) %>%
  dySeries(name=col_names[2], label=col_names[2], col="red", strokeWidth=2) %>%
  dyLegend(width=500)
# Cap the VTI returns
cut_off <- 0.03
capp_ed <- ifelse(vt_i > cut_off, cut_off, vt_i)
capp_ed <- ifelse(capp_ed < (-cut_off), -cut_off, capp_ed)
# Calculate PnLs for vt_i
forecast_s <- sim_forecasts(vt_i, or_der=3, look_back=look_back, is_rolling=FALSE)
pnl_s <- cumsum(sign(forecast_s)*vt_i)
# Calculate PnLs for capped VTI returns
forecast_s <- sim_forecasts(capp_ed, or_der=3, look_back=look_back, is_rolling=FALSE)
pnls_capped <- cumsum(sign(forecast_s)*vt_i)
# Plot the cumulative strategy returns
weal_th <- cbind(pnl_s, pnls_capped)
weal_th <- xts::xts(weal_th, date_s)
col_names <- c("AR(3)_Rolling", "AR(3)_Expanding")
col_names <- c("AR_Strategy", "AR_Strategy_Capped")
colnames(weal_th) <- col_names
dygraphs::dygraph(weal_th, main="Improved Autoregressive Strategies") %>%
  dySeries(name=col_names[1], label=col_names[1], col="blue", strokeWidth=2) %>%
  dySeries(name=col_names[2], label=col_names[2], col="red", strokeWidth=2) %>%
  dyLegend(width=500)
# Load constant maturity Treasury rates
load(file="/Users/jerzy/Develop/lecture_slides/data/rates_data.RData")
# Combine rates into single xts series
rate_s <- do.call(cbind, as.list(rates_env))
# Sort the columns of rate_s according bond maturity
name_s <- colnames(rate_s)
name_s <- substr(name_s, start=4, stop=10)
name_s <- as.numeric(name_s)
indeks <- order(name_s)
rate_s <- rate_s[, indeks]
# Align rates dates with VTI prices
clos_e <- log(quantmod::Cl(rutils::etf_env$VTI))
colnames(clos_e) <- "VTI"
n_rows <- NROW(clos_e)
date_s <- zoo::index(clos_e)
rate_s <- na.omit(rate_s[date_s])
clos_e <- clos_e[zoo::index(rate_s)]
date_s <- zoo::index(clos_e)
# Calculate VTI returns and IR changes
re_turns <- rutils::diff_it(clos_e)
rates_diff <- rutils::diff_it(log(rate_s))
# Regress VTI returns versus the lagged rate differences
predic_tor <- rutils::lag_it(rates_diff)
mod_el <- lm(re_turns ~ predic_tor)
summary(mod_el)
# Regress VTI returns before and after 2012
summary(lm(re_turns["/2012"] ~ predic_tor["/2012"]))
summary(lm(re_turns["2012/"] ~ predic_tor["2012/"]))
# Calculate PCA of rates correlation matrix
ei_gen <- eigen(cor(rates_diff))
rates_pca <- -rates_diff %*% ei_gen$vectors
colnames(rates_pca) <- paste0("PC", 1:6)
# Define predictor as the YC PCAs
predic_tor <- rutils::lag_it(rates_pca)
mod_el <- lm(re_turns ~ predic_tor)
summary(mod_el)
# Plot YC steepener principal component with VTI
da_ta <- cbind(re_turns, rates_pca[, 2])
colnames(da_ta) <- c("VTI", "Steepener")
col_names <- colnames(da_ta)
dygraphs::dygraph(cumsum(da_ta), main="VTI and Yield Curve Steepener") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=1, col="blue") %>%
  dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=1, col="red")
# Define predictor with intercept term
predic_tor <- rutils::lag_it(rates_diff)
predic_tor <- cbind(rep(1, NROW(predic_tor)), predic_tor)
colnames(predic_tor)[1] <- "intercept"
# Calculate inverse of predictor
in_verse <- MASS::ginv(predic_tor)
# Calculate coefficients from response and inverse of predictor
res_ponse <- re_turns
co_eff <- drop(in_verse %*% res_ponse)
# Calculate forecasts and pnls in-sample
forecast_s <- (predic_tor %*% co_eff)
pnl_s <- sign(forecast_s)*re_turns
# Calculate in-sample factors
factor_s <- (predic_tor * co_eff)
apply(factor_s, 2, sd)
# Plot dygraph of in-sample IR strategy
weal_th <- cbind(re_turns, pnl_s)
colnames(weal_th) <- c("VTI", "Strategy")
col_names <- colnames(weal_th)
dygraphs::dygraph(cumsum(weal_th), main="Yield Curve Strategy In-sample") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=col_names[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Define in-sample and out-of-sample intervals
in_sample <- (date_s < as.Date("2020-01-01"))
out_sample <- (date_s >= as.Date("2020-01-01"))
# Calculate inverse of predictor in-sample
in_verse <- MASS::ginv(predic_tor[in_sample, ])
# Calculate coefficients in-sample
co_eff <- drop(in_verse %*% res_ponse[in_sample, ])
# Calculate forecasts and pnls out-of-sample
forecast_s <- (predic_tor[out_sample, ] %*% co_eff)
pnl_s <- sign(forecast_s)*re_turns[out_sample, ]
# Plot dygraph of out-of-sample IR PCA strategy
weal_th <- cbind(re_turns[out_sample, ], pnl_s)
colnames(weal_th) <- c("VTI", "Strategy")
col_names <- colnames(weal_th)
dygraphs::dygraph(cumsum(weal_th), main="Yield Curve Strategy Out-of-Sample") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=col_names[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Define yearly dates
format(date_s[1], "%Y")
year_s <- paste0(seq(2001, 2022, 1), "-01-01")
year_s <- as.Date(year_s)
# Perform loop over yearly dates
pnl_s <- lapply(3:(NROW(year_s)-1), function(i) {
  # Define in-sample and out-of-sample intervals
  in_sample <- (date_s > year_s[i-1]) & (date_s < year_s[i])
  out_sample <- (date_s >= year_s[i]) & (date_s < year_s[i+1])
  # Calculate coefficients in-sample
  in_verse <- MASS::ginv(predic_tor[in_sample, ])
  co_eff <- drop(in_verse %*% res_ponse[in_sample, ])
  # Calculate forecasts and pnls out-of-sample
  forecast_s <- (predic_tor[out_sample, ] %*% co_eff)
  sign(forecast_s)*re_turns[out_sample, ]
})  # end lapply
pnl_s <- do.call(rbind, pnl_s)
# Plot dygraph of rolling yearly IR strategy
vt_i <- rutils::diff_it(clos_e[zoo::index(pnl_s),])
weal_th <- cbind(vt_i, pnl_s)
colnames(weal_th) <- c("VTI", "Strategy")
col_names <- colnames(weal_th)
dygraphs::dygraph(cumsum(weal_th), main="Rolling Yearly Yield Curve Strategy") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=col_names[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Define monthly dates
format(date_s[1], "%m-%Y")
format(date_s[NROW(date_s)], "%m-%Y")
month_s <- seq.Date(from=as.Date("2001-05-01"), to=as.Date("2021-04-01"), by="month")
# Perform loop over monthly dates
pnl_s <- lapply(12:(NROW(month_s)-1), function(i) {
  # Define in-sample and out-of-sample intervals
  in_sample <- (date_s > month_s[i-11]) & (date_s < month_s[i])
  out_sample <- (date_s > month_s[i]) & (date_s < month_s[i+1])
  # Calculate forecasts and pnls out-of-sample
  in_verse <- MASS::ginv(predic_tor[in_sample, ])
  co_eff <- drop(in_verse %*% res_ponse[in_sample, ])
  forecast_s <- (predic_tor[out_sample, ] %*% co_eff)
  sign(forecast_s)*re_turns[out_sample, ]
})  # end lapply
pnl_s <- do.call(rbind, pnl_s)
# Plot dygraph of rolling monthly IR strategy
vt_i <- rutils::diff_it(clos_e[zoo::index(pnl_s),])
weal_th <- cbind(vt_i, pnl_s)
colnames(weal_th) <- c("VTI", "Strategy")
col_names <- colnames(weal_th)
dygraphs::dygraph(cumsum(weal_th), main="Rolling Monthly Yield Curve Strategy") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=col_names[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Define weekly dates
week_s <- seq.Date(from=as.Date("2001-05-01"), to=as.Date("2021-04-01"), by="weeks")
# Perform loop over weekly dates
pnl_s <- lapply(51:(NROW(week_s)-1), function(i) {
  # Define in-sample and out-of-sample intervals
  in_sample <- (date_s > week_s[i-10]) & (date_s < week_s[i])
  out_sample <- (date_s > week_s[i]) & (date_s < week_s[i+1])
  # Calculate forecasts and pnls out-of-sample
  in_verse <- MASS::ginv(predic_tor[in_sample, ])
  co_eff <- drop(in_verse %*% res_ponse[in_sample, ])
  forecast_s <- (predic_tor[out_sample, ] %*% co_eff)
  sign(forecast_s)*re_turns[out_sample, ]
})  # end lapply
pnl_s <- do.call(rbind, pnl_s)
# Plot dygraph of rolling weekly IR strategy
vt_i <- rutils::diff_it(clos_e[zoo::index(pnl_s),])
weal_th <- cbind(vt_i, pnl_s)
colnames(weal_th) <- c("VTI", "Strategy")
col_names <- colnames(weal_th)
dygraphs::dygraph(cumsum(weal_th), main="Rolling Weekly Yield Curve Strategy") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=col_names[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Calculate singular value decomposition of the predictor matrix
s_vd <- svd(predic_tor)
barplot(s_vd$d, main="Singular Values of YC Predictor Matrix")
# Calculate generalized inverse from SVD
in_verse <- s_vd$v %*% (t(s_vd$u) / s_vd$d)
# Verify inverse property of in_verse
all.equal(zoo::coredata(predic_tor),
    predic_tor %*% in_verse %*% predic_tor)
# Calculate generalized inverse using MASS::ginv()
inverse_ginv <- MASS::ginv(predic_tor)
all.equal(inverse_ginv, in_verse)
# Set tolerance for determining zero singular values
to_l <- sqrt(.Machine$double.eps)
# Check for zero singular values
round(s_vd$d, 12)
not_zero <- (s_vd$d > (to_l * s_vd$d[1]))
# Calculate regularized inverse from SVD
inv_reg <- s_vd$v[, not_zero] %*%
  (t(s_vd$u[, not_zero]) / s_vd$d[not_zero])
# Verify inverse property of inv_reg
all.equal(zoo::coredata(predic_tor),
    predic_tor %*% inv_reg %*% predic_tor)
all.equal(inv_reg, in_verse)
# Calculate shrinkage inverse from SVD
eigen_max <- 3
inv_shrink <- s_vd$v[, 1:eigen_max] %*%
  (t(s_vd$u[, 1:eigen_max]) / s_vd$d[1:eigen_max])
# Inverse property fails for inv_shrink
all.equal(zoo::coredata(predic_tor),
    predic_tor %*% inv_shrink %*% predic_tor)
# Calculate shrinkage inverse using RcppArmadillo
inverse_rcpp <- HighFreq::calc_inv(predic_tor, eigen_max=eigen_max)
all.equal(inv_shrink, inverse_rcpp, check.attributes=FALSE)
# Calculate in-sample pnls for different eigen_max values
eigen_maxs <- 2:7
pnl_s <- lapply(eigen_maxs, function(eigen_max) {
  in_verse <- HighFreq::calc_inv(predic_tor, eigen_max=eigen_max)
  co_eff <- drop(in_verse %*% res_ponse)
  forecast_s <- (predic_tor %*% co_eff)
  sign(forecast_s)*re_turns
})
pnl_s <- do.call(cbind, pnl_s)
colnames(pnl_s) <- paste0("eigen", eigen_maxs)
# Plot dygraph of in-sample pnls
col_ors <- colorRampPalette(c("blue", "red"))(NCOL(pnl_s))
dygraphs::dygraph(cumsum(pnl_s), main="In-Sample Returns of Shrinkage YC Strategies") %>%
  dyOptions(colors=col_ors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Define in-sample and out-of-sample intervals
in_sample <- (date_s < as.Date("2020-01-01"))
out_sample <- (date_s >= as.Date("2020-01-01"))
# Calculate in-sample pnls for different eigen_max values
eigen_maxs <- 2:7
pnl_s <- lapply(eigen_maxs, function(x) {
  in_verse <- HighFreq::calc_inv(predic_tor[in_sample, ], eigen_max=x)
  co_eff <- drop(in_verse %*% res_ponse[in_sample, ])
  forecast_s <- (predic_tor[out_sample, ] %*% co_eff)
  sign(forecast_s)*re_turns[out_sample, ]
})
pnl_s <- do.call(cbind, pnl_s)
colnames(pnl_s) <- paste0("eigen", eigen_maxs)
# Plot dygraph of out-of-sample pnls
col_ors <- colorRampPalette(c("blue", "red"))(NCOL(pnl_s))
dygraphs::dygraph(cumsum(pnl_s), main="Out-of-Sample Returns of Shrinkage YC Strategies") %>%
  dyOptions(colors=col_ors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Define monthly dates
format(date_s[1], "%m-%Y")
format(date_s[NROW(date_s)], "%m-%Y")
month_s <- seq.Date(from=as.Date("2001-05-01"), to=as.Date("2021-04-01"), by="month")
# Perform loop over monthly dates
look_back <- 6
eigen_max <- 3
pnl_s <- lapply(12:(NROW(month_s)-1), function(i) {
  # Define in-sample and out-of-sample intervals
  in_sample <- (date_s > month_s[i-look_back]) & (date_s < month_s[i])
  out_sample <- (date_s > month_s[i]) & (date_s < month_s[i+1])
  # Calculate forecasts and pnls out-of-sample
  in_verse <- HighFreq::calc_inv(predic_tor[in_sample, ], eigen_max=eigen_max)
  co_eff <- drop(in_verse %*% res_ponse[in_sample, ])
  forecast_s <- (predic_tor[out_sample, ] %*% co_eff)
  sign(forecast_s)*re_turns[out_sample, ]
})  # end lapply
pnl_s <- do.call(rbind, pnl_s)
# Plot dygraph of rolling monthly IR strategy
vt_i <- rutils::diff_it(clos_e[zoo::index(pnl_s),])
weal_th <- cbind(vt_i, pnl_s)
colnames(weal_th) <- c("VTI", "Strategy")
col_names <- colnames(weal_th)
dygraphs::dygraph(cumsum(weal_th), main="Rolling Monthly Shrinkage YC Strategy") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=col_names[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Define weekly dates
week_s <- seq.Date(from=as.Date("2001-05-01"), to=as.Date("2021-04-01"), by="weeks")
# Perform loop over weekly dates
look_back <- 4
eigen_max <- 4
pnl_s <- lapply(51:(NROW(week_s)-1), function(i) {
  # Define in-sample and out-of-sample intervals
  in_sample <- (date_s > week_s[i-look_back]) & (date_s < week_s[i])
  out_sample <- (date_s > week_s[i]) & (date_s < week_s[i+1])
  # Calculate forecasts and pnls out-of-sample
  in_verse <- HighFreq::calc_inv(predic_tor[in_sample, ], eigen_max=eigen_max)
  co_eff <- drop(in_verse %*% res_ponse[in_sample, ])
  forecast_s <- (predic_tor[out_sample, ] %*% co_eff)
  sign(forecast_s)*re_turns[out_sample, ]
})  # end lapply
pnl_s <- do.call(rbind, pnl_s)
# Plot dygraph of rolling weekly IR strategy
vt_i <- rutils::diff_it(clos_e[zoo::index(pnl_s),])
weal_th <- cbind(vt_i, pnl_s)
colnames(weal_th) <- c("VTI", "Strategy")
col_names <- colnames(weal_th)
dygraphs::dygraph(cumsum(weal_th), main="Rolling Weekly Shrinkage YC Strategy") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=col_names[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Load the yield curve data
load(file="/Users/jerzy/Develop/lecture_slides/data/rates_data.RData")
rate_s <- do.call(cbind, as.list(rates_env))
name_s <- colnames(rate_s)
name_s <- substr(name_s, start=4, stop=10)
name_s <- as.numeric(name_s)
indeks <- order(name_s)
rate_s <- rate_s[, indeks]
clos_e <- log(quantmod::Cl(rutils::etf_env$VTI))
colnames(clos_e) <- "VTI"
n_rows <- NROW(clos_e)
date_s <- zoo::index(clos_e)
rate_s <- na.omit(rate_s[date_s])
clos_e <- clos_e[zoo::index(rate_s)]
date_s <- zoo::index(clos_e)
re_turns <- rutils::diff_it(clos_e)
rates_diff <- rutils::diff_it(log(rate_s))
# Create a combined predictor matrix
order_max <- 5
predic_tor <- sapply(1:order_max, rutils::lag_it, in_put=as.numeric(re_turns))
colnames(predic_tor) <- paste0("retslag", 1:NCOL(predic_tor))
predic_tor <- cbind(predic_tor, rutils::lag_it(rates_diff))
predic_tor <- cbind(rep(1, NROW(predic_tor)), predic_tor)
colnames(predic_tor)[1] <- "intercept"
res_ponse <- re_turns
# Calculate in-sample pnls for different eigen_max values
eigen_maxs <- 2:11
pnl_s <- lapply(eigen_maxs, function(eigen_max) {
  in_verse <- HighFreq::calc_inv(predic_tor, eigen_max=eigen_max)
  co_eff <- drop(in_verse %*% res_ponse)
  forecast_s <- (predic_tor %*% co_eff)
  sign(forecast_s)*re_turns
})
pnl_s <- do.call(cbind, pnl_s)
colnames(pnl_s) <- paste0("eigen", eigen_maxs)
# Plot dygraph of in-sample pnls
col_ors <- colorRampPalette(c("blue", "red"))(NCOL(pnl_s))
dygraphs::dygraph(cumsum(pnl_s), main="In-Sample Returns of Combined Strategies With Shrinkage") %>%
  dyOptions(colors=col_ors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Define in-sample and out-of-sample intervals
in_sample <- (date_s < as.Date("2020-01-01"))
out_sample <- (date_s >= as.Date("2020-01-01"))
# Calculate in-sample pnls for different eigen_max values
eigen_maxs <- 2:11
pnl_s <- lapply(eigen_maxs, function(x) {
  in_verse <- HighFreq::calc_inv(predic_tor[in_sample, ], eigen_max=x)
  co_eff <- drop(in_verse %*% res_ponse[in_sample, ])
  forecast_s <- (predic_tor[out_sample, ] %*% co_eff)
  sign(forecast_s)*re_turns[out_sample, ]
})
pnl_s <- do.call(cbind, pnl_s)
colnames(pnl_s) <- paste0("eigen", eigen_maxs)
# Plot dygraph of out-of-sample pnls
col_ors <- colorRampPalette(c("blue", "red"))(NCOL(pnl_s))
dygraphs::dygraph(cumsum(pnl_s), main="Out-of-Sample Returns of Combined Strategies With Shrinkage") %>%
  dyOptions(colors=col_ors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Define monthly dates
format(date_s[1], "%m-%Y")
format(date_s[NROW(date_s)], "%m-%Y")
month_s <- seq.Date(from=as.Date("2001-05-01"), to=as.Date("2021-04-01"), by="month")
# Perform loop over monthly dates
look_back <- 6
eigen_max <- 3
pnl_s <- lapply(12:(NROW(month_s)-1), function(i) {
  # Define in-sample and out-of-sample intervals
  in_sample <- (date_s > month_s[i-look_back]) & (date_s < month_s[i])
  out_sample <- (date_s > month_s[i]) & (date_s < month_s[i+1])
  # Calculate forecasts and pnls out-of-sample
  in_verse <- HighFreq::calc_inv(predic_tor[in_sample, ], eigen_max=eigen_max)
  co_eff <- drop(in_verse %*% res_ponse[in_sample, ])
  forecast_s <- (predic_tor[out_sample, ] %*% co_eff)
  sign(forecast_s)*re_turns[out_sample, ]
})  # end lapply
pnl_s <- do.call(rbind, pnl_s)
# Plot dygraph of rolling monthly IR strategy
vt_i <- rutils::diff_it(clos_e[zoo::index(pnl_s),])
weal_th <- cbind(vt_i, pnl_s)
colnames(weal_th) <- c("VTI", "Strategy")
col_names <- colnames(weal_th)
dygraphs::dygraph(cumsum(weal_th), main="Rolling Monthly Shrinkage YC Strategy") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=col_names[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Define weekly dates
week_s <- seq.Date(from=as.Date("2001-05-01"), to=as.Date("2021-04-01"), by="weeks")
# Perform loop over weekly dates
look_back <- 8
eigen_max <- 4
pnl_s <- lapply(51:(NROW(week_s)-1), function(i) {
  # Define in-sample and out-of-sample intervals
  in_sample <- (date_s > week_s[i-look_back]) & (date_s < week_s[i])
  out_sample <- (date_s > week_s[i]) & (date_s < week_s[i+1])
  # Calculate forecasts and pnls out-of-sample
  in_verse <- HighFreq::calc_inv(predic_tor[in_sample, ], eigen_max=eigen_max)
  co_eff <- drop(in_verse %*% res_ponse[in_sample, ])
  forecast_s <- (predic_tor[out_sample, ] %*% co_eff)
  sign(forecast_s)*re_turns[out_sample, ]
})  # end lapply
pnl_s <- do.call(rbind, pnl_s)
# Plot dygraph of rolling weekly IR strategy
vt_i <- rutils::diff_it(clos_e[zoo::index(pnl_s),])
weal_th <- cbind(vt_i, pnl_s)
colnames(weal_th) <- c("VTI", "Strategy")
col_names <- colnames(weal_th)
dygraphs::dygraph(cumsum(weal_th), main="Rolling Weekly Shrinkage YC Strategy") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=col_names[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Find optimal n_agg for predictor
n_aggs <- 5:100
tvalues <- sapply(n_aggs, function(n_agg) {
  predic_tor <- roll::roll_mean(rates_diff, width=n_agg, min_obs=1)
  predic_tor <- cbind(rep(1, NROW(predic_tor)), predic_tor)
  predic_tor <- rutils::lag_it(predic_tor)
  mod_el <- lm(res_ponse ~ predic_tor - 1)
  model_sum <- summary(mod_el)
  max(abs(model_sum$coefficients[, 3][-1]))
})  # end sapply
n_aggs[which.max(tvalues)]
plot(n_aggs, tvalues, t="l", col="blue", lwd=2)
# Calculate aggregated predictor
n_agg <- 53
predic_tor <- roll::roll_mean(rates_diff, width=n_agg, min_obs=1)
predic_tor <- rutils::lag_it(predic_tor)
predic_tor <- cbind(rep(1, NROW(predic_tor)), predic_tor)
mod_el <- lm(res_ponse ~ predic_tor - 1)
summary(mod_el)
# Calculate forecasts and pnls in-sample
in_verse <- MASS::ginv(predic_tor)
co_eff <- drop(in_verse %*% res_ponse)
forecast_s <- (predic_tor %*% co_eff)
pnl_s <- sign(forecast_s)*re_turns
# Plot dygraph of in-sample IR strategy
weal_th <- cbind(re_turns, pnl_s)
colnames(weal_th) <- c("VTI", "Strategy")
col_names <- colnames(weal_th)
dygraphs::dygraph(cumsum(weal_th), main="Aggregated YC Strategy In-sample") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=col_names[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Define in-sample and out-of-sample intervals
in_sample <- (date_s < as.Date("2020-01-01"))
out_sample <- (date_s >= as.Date("2020-01-01"))
# Calculate forecasts and pnls out-of-sample
in_verse <- MASS::ginv(predic_tor[in_sample, ])
co_eff <- drop(in_verse %*% res_ponse[in_sample, ])
forecast_s <- (predic_tor[out_sample, ] %*% co_eff)
pnl_s <- sign(forecast_s)*re_turns[out_sample, ]
# Plot dygraph of out-of-sample YC strategy
weal_th <- cbind(re_turns[out_sample, ], pnl_s)
colnames(weal_th) <- c("VTI", "Strategy")
col_names <- colnames(weal_th)
dygraphs::dygraph(cumsum(weal_th), main="Aggregated YC Strategy Out-of-Sample") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=col_names[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Extract ETF returns
sym_bols <- c("VTI", "IEF", "DBC")
re_turns <- rutils::etf_env$re_turns[, sym_bols]
re_turns <- na.omit(re_turns)
# Or, select rows with IEF data
# re_turns <- re_turns[index(rutils::etf_env$IEF)]
# Copy over NA values
# re_turns[1, is.na(re_turns[1, ])] <- 0
# re_turns <- zoo::na.locf(re_turns, na.rm=FALSE)
# Define end of month end points
end_p <- rutils::calc_endpoints(re_turns, inter_val="months")
end_p <- end_p[-1]
n_rows <- NROW(end_p)
date_s <- zoo::index(re_turns)[end_p]
# Start points equal end points lagged by 12-month look-back interval
look_back <- 12
start_p <- c(rep_len(1, look_back-1),
  end_p[1:(n_rows - look_back + 1)])
# Calculate matrix of look-back intervals
look_backs <- cbind(start_p, end_p)
colnames(look_backs) <- c("start", "end")
# Calculate matrix of look-forward intervals
look_fwds <- cbind(end_p + 1, rutils::lag_it(end_p, -1))
look_fwds[n_rows, ] <- end_p[n_rows]
colnames(look_fwds) <- c("start", "end")
# Inspect the intervals
head(cbind(look_backs, look_fwds))
tail(cbind(look_backs, look_fwds))
# Define performance function as Sharpe ratio
perform_ance <- function(re_turns) sum(re_turns)/sd(re_turns)
# Calculate past performance over look-back intervals
pas_t <- apply(look_backs, 1, function(ep) {
  sapply(re_turns[ep[1]:ep[2]], perform_ance)
})  # end sapply
pas_t <- t(pas_t)
pas_t[is.na(pas_t)] <- 0
# Weights are proportional to past performance
weight_s <- pas_t
# weight_s[weight_s < 0] <- 0
# Scale weight_s so sum of squares is equal to 1.
weight_s <- weight_s/sqrt(rowSums(weight_s^2))
# Or scale weight_s so sum is equal to 1
# weight_s <- weight_s/rowSums(weight_s)
# Set NA values to zero
weight_s[is.na(weight_s)] <- 0
sum(is.na(weight_s))
# Calculate future out-of-sample performance
fu_ture <- apply(look_fwds, 1, function(ep) {
  sapply(re_turns[ep[1]:ep[2]], sum)
})  # end sapply
fu_ture <- t(fu_ture)
fu_ture[is.na(fu_ture)] <- 0
tail(fu_ture)
# Calculate the momentum pnls
pnl_s <- rowSums(weight_s*fu_ture)
# Lag the future and momentum returns to proper dates
fu_ture <- rutils::lag_it(fu_ture)
pnl_s <- rutils::lag_it(pnl_s)
# The momentum strategy has low correlation to stocks
cor(pnl_s, fu_ture)
# Define all-weather benchmark
weights_aw <- c(0.30, 0.55, 0.15)
all_weather <- fu_ture %*% weights_aw
# Calculate the wealth of momentum returns
weal_th <- xts::xts(cbind(all_weather, pnl_s), order.by=date_s)
colnames(weal_th) <- c("All-Weather", "Momentum")
cor(weal_th)
# Plot dygraph of the momentum strategy returns
dygraphs::dygraph(cumsum(weal_th), main="Monthly Momentum Strategy vs All-Weather") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Calculate momentum profits and losses (returns)
pnl_s <- rowSums(weight_s*fu_ture)
# Lag the momentum returns and weights
# to correspond with end of future interval
pnl_s <- rutils::lag_it(pnl_s)
weight_s <- rutils::lag_it(weight_s)
# bid_offer equal to 10 bps for liquid ETFs
bid_offer <- 0.001
# Calculate transaction costs
weal_th <- cumsum(pnl_s)
cost_s <- 0.5*bid_offer*weal_th*rowSums(abs(rutils::diff_it(weight_s)))
weal_th <- cumsum(pnl_s - cost_s)
date_s <- index(re_turns[end_p])
weal_th <- xts::xts(weal_th, date_s)
# Define all-weather benchmark
weights_aw <- c(0.30, 0.55, 0.15)
ret_aw <- re_turns %*% weights_aw
wealth_aw <- cumsum(ret_aw)
wealth_aw <- xts::xts(wealth_aw[end_p], date_s)
# Plot the Momentum strategy and benchmark
weal_th <- cbind(weal_th, wealth_aw)
colnames(weal_th) <- c("Momentum Strategy", "Benchmark")
dygraphs::dygraph(weal_th, main="Momentum Strategy") %>%
  dyAxis("y", label="Benchmark", independentTicks=TRUE) %>%
  dyAxis("y2", label="Momentum Strategy", independentTicks=TRUE) %>%
  dySeries(name="Momentum Strategy", axis="y2", label="Momentum Strategy", strokeWidth=2, col="red") %>%
  dySeries(name="Benchmark", axis="y", label="Benchmark", strokeWidth=2, col="blue")
# Define backtest functional
backtest_momentum <- function(returns,
                perform_ance=function(re_turns) (sum(re_turns)/sd(re_turns)),
                look_back=12, re_balance="months", bid_offer=0.001,
                endp=rutils::calc_endpoints(re_turns, inter_val=re_balance)[-1],
                with_weights=FALSE, ...) {
  stopifnot("package:rutils" %in% search() || require("rutils", quietly=TRUE))
  # Define look-back and look-forward intervals
  n_rows <- NROW(end_p)
  start_p <- c(rep_len(1, look_back-1), end_p[1:(n_rows-look_back+1)])
  # Calculate look-back intervals
  look_backs <- cbind(start_p, end_p)
  # Calculate look-forward intervals
  look_fwds <- cbind(end_p + 1, rutils::lag_it(end_p, -1))
  look_fwds[n_rows, ] <- end_p[n_rows]
  # Calculate past performance over look-back intervals
  pas_t <- t(apply(look_backs, 1, function(ep) sapply(re_turns[ep[1]:ep[2]], perform_ance)))
  pas_t[is.na(pas_t)] <- 0
  # Calculate future performance
  fu_ture <- t(apply(look_fwds, 1, function(ep) sapply(re_turns[ep[1]:ep[2]], sum)))
  fu_ture[is.na(fu_ture)] <- 0
  # Scale weight_s so sum of squares is equal to 1
  weight_s <- pas_t
  weight_s <- weight_s/sqrt(rowSums(weight_s^2))
  weight_s[is.na(weight_s)] <- 0  # Set NA values to zero
  # Calculate momentum profits and losses
  pnl_s <- rowSums(weight_s*fu_ture)
  # Calculate transaction costs
  cost_s <- 0.5*bid_offer*cumprod(1 + pnl_s)*rowSums(abs(rutils::diff_it(weight_s)))
  pnl_s <- (pnl_s - cost_s)
  if (with_weights)
    rutils::lag_it(cbind(pnl_s, weight_s))
  else
    rutils::lag_it(pnl_s)
}  # end backtest_momentum
source("/Users/jerzy/Develop/lecture_slides/scripts/back_test.R")
look_backs <- seq(3, 15, by=1)
perform_ance <- function(re_turns) sum(re_turns)/sd(re_turns)
pro_file <- sapply(look_backs, function(look_back) {
  pnl_s <- backtest_momentum(returns=re_turns, endp=end_p,
    look_back=look_back, perform_ance=perform_ance)
  sum(pnl_s)
})  # end sapply
# Plot momemntum PnLs
x11(width=6, height=5)
plot(x=look_backs, y=pro_file, t="l",
  main="Momemntum PnL as function of look_back",
  xlab="look_back (months)", ylab="pnl")
# Optimal look_back
look_back <- look_backs[which.max(pro_file)]
pnl_s <- backtest_momentum(returns=re_turns,
  look_back=look_back, endp=end_p,
  perform_ance=perform_ance, with_weights=TRUE)
tail(pnl_s)
# Calculate the wealth of momentum returns
ret_mom <- pnl_s[, 1]
weal_th <- xts::xts(cbind(all_weather, ret_mom), order.by=date_s)
colnames(weal_th) <- c("All-Weather", "Momentum")
cor(weal_th)
# Plot dygraph of the momentum strategy returns
dygraphs::dygraph(cumsum(weal_th), main="Monthly Momentum Strategy vs All-Weather") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Or
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue")
quantmod::chart_Series(cumsum(weal_th), theme=plot_theme, lwd=2,
       name="Momentum PnL")
legend("topleft", legend=colnames(weal_th),
  inset=0.1, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")
# Plot the momentum portfolio weights
weight_s <- pnl_s[, -1]
vt_i <- log(quantmod::Cl(rutils::etf_env$VTI[date_s]))
colnames(vt_i) <- "VTI"
da_ta <- cbind(vt_i, weight_s)
da_ta <- na.omit(da_ta)
colnames(da_ta)[2:NCOL(pnl_s)] <- paste0(colnames(weight_s), "_weight")
zoo::plot.zoo(da_ta, xlab=NULL, main="Momentum Weights")
# Calculate ETF betas
betas_etf <- sapply(re_turns, function(x)
  cov(re_turns$VTI, x)/var(x))
# Momentum beta is equal weights times ETF betas
beta_s <- weight_s %*% betas_etf
beta_s <- xts::xts(beta_s, order.by=date_s)
colnames(beta_s) <- "momentum_beta"
da_ta <- cbind(beta_s, vt_i)
zoo::plot.zoo(da_ta,
  oma = c(3, 1, 3, 0), mar = c(0, 4, 0, 1),
  main="Momentum Beta & VTI Price", xlab="")
# Open x11 for plotting and set parameters to reduce whitespace around plot
x11(width=6, height=5)
par(mar=c(4, 4, 3, 1), oma=c(0, 0, 0, 0))
# Merton-Henriksson test
vt_i <- rutils::diff_it(vt_i)
de_sign <- cbind(VTI=vt_i, 0.5*(vt_i+abs(vt_i)), vt_i^2)
colnames(de_sign)[2:3] <- c("merton", "treynor")
mod_el <- lm(ret_mom ~ VTI + merton, data=de_sign); summary(mod_el)
# Treynor-Mazuy test
mod_el <- lm(ret_mom ~ VTI + treynor, data=de_sign); summary(mod_el)
# Plot residual scatterplot
plot.default(x=vt_i, y=ret_mom, xlab="VTI", ylab="momentum")
title(main="Treynor-Mazuy market timing test\n for Momentum vs VTI", line=0.5)
# Plot fitted (predicted) response values
points.default(x=vt_i, y=mod_el$fitted.values, pch=16, col="red")
residual_s <- mod_el$residuals
text(x=0.0, y=max(residual_s), paste("Treynor test t-value =", round(summary(mod_el)$coeff["treynor", "t value"], 2)))
# Standardize the returns
ret_mom_std <- (ret_mom-mean(ret_mom))/sd(ret_mom)
vt_i <- (vt_i-mean(vt_i))/sd(vt_i)
# Calculate skewness and kurtosis
apply(cbind(ret_mom_std, vt_i), 2, function(x)
  sapply(c(skew=3, kurt=4),
    function(e) sum(x^e)))/n_rows
# Plot histogram
hist(ret_mom_std, breaks=30,
  main="Momentum and VTI Return Distributions (standardized",
  xlim=c(-4, 4),
  xlab="", ylab="", freq=FALSE)
# Draw kernel density of histogram
lines(density(ret_mom_std), col='red', lwd=2)
lines(density(vt_i), col='blue', lwd=2)
# Add legend
legend("topright", inset=0.05, cex=0.8, title=NULL,
 leg=c("Momentum", "VTI"),
 lwd=6, bg="white", col=c("red", "blue"))
# Combine momentum strategy with all-weather
all_weather <- sd(ret_mom)*all_weather/sd(all_weather)
weal_th <- cbind(ret_mom, all_weather, 0.5*(ret_mom + all_weather))
colnames(weal_th) <- c("momentum", "all_weather", "combined")
# Calculate strategy annualized Sharpe ratios
apply(weal_th, MARGIN=2, function(x) {
  sqrt(12)*sum(x)/sd(x)/NROW(x)
})  # end apply
# Calculate strategy correlations
cor(weal_th)
# Calculate cumulative wealth
weal_th <- xts::xts(weal_th, date_s)
# Plot ETF momentum strategy combined with All-Weather
dygraphs::dygraph(cumsum(weal_th), main="ETF Momentum Strategy Combined with All-Weather") %>%
  dyOptions(colors=c("red", "blue", "green"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Or
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("green", "blue", "red")
quantmod::chart_Series(weal_th, theme=plot_theme,
       name="ETF Momentum Strategy Combined with All-Weather")
legend("topleft", legend=colnames(weal_th),
  inset=0.1, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")
# Calculate rolling variance
look_back <- 252
vari_ance <- roll::roll_var(re_turns, width=look_back, min_obs=1)
vari_ance[1, ] <- 1
# Calculate rolling Sharpe
pas_t <- roll::roll_mean(re_turns, width=look_back, min_obs=1)
weight_s <- pas_t/sqrt(vari_ance)
weight_s <- weight_s/sqrt(rowSums(weight_s^2))
weight_s <- rutils::lag_it(weight_s)
sum(is.na(weight_s))
# Calculate momentum profits and losses
pnl_s <- rowMeans(weight_s*re_turns)
# Calculate transaction costs
bid_offer <- 0.001
cost_s <- 0.5*bid_offer*rowSums(abs(rutils::diff_it(weight_s)))
pnl_s <- (pnl_s - cost_s)
# Define all-weather benchmark
weights_aw <- c(0.30, 0.55, 0.15)
all_weather <- re_turns %*% weights_aw
# Calculate the wealth of momentum returns
weal_th <- xts::xts(cbind(all_weather, pnl_s), order.by=index(re_turns))
colnames(weal_th) <- c("All-Weather", "Momentum")
cor(weal_th)
# Plot dygraph of the momentum strategy returns
dygraphs::dygraph(cumsum(weal_th)[date_s], main="Daily Momentum Strategy vs All-Weather") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Define backtest functional for daily momentum strategy
# If trend=(-1) then it backtests a mean reverting strategy
momentum_daily <- function(returns, look_back=252, bid_offer=0.001, trend=1, ...) {
  stopifnot("package:quantmod" %in% search() || require("quantmod", quietly=TRUE))
  # Calculate rolling variance
  vari_ance <- roll::roll_var(returns, width=look_back, min_obs=1)
  vari_ance[1, ] <- 1
  vari_ance[vari_ance <= 0] <- 1
# Calculate rolling Sharpe
  pas_t <- roll::roll_mean(returns, width=look_back, min_obs=1)
  weight_s <- pas_t/sqrt(vari_ance)
  weight_s <- weight_s/sqrt(rowSums(weight_s^2))
  weight_s <- rutils::lag_it(weight_s)
  # Calculate momentum profits and losses
  pnl_s <- trend*rowMeans(weight_s*returns)
  # Calculate transaction costs
  cost_s <- 0.5*bid_offer*rowSums(abs(rutils::diff_it(weight_s)))
  (pnl_s - cost_s)
}  # end momentum_daily
# Simulate a daily ETF momentum strategy
source("/Users/jerzy/Develop/lecture_slides/scripts/back_test.R")
pnl_s <- momentum_daily(returns=re_turns, look_back=252,
  bid_offer=bid_offer)
# Perform sapply loop over look_backs
look_backs <- seq(50, 300, by=50)
pnl_s <- sapply(look_backs, momentum_daily,
  returns=re_turns, bid_offer=bid_offer)
colnames(pnl_s) <- paste0("look_back=", look_backs)
pnl_s <- xts::xts(pnl_s, index(re_turns))
tail(pnl_s)
# Plot dygraph of daily ETF momentum strategies
col_ors <- colorRampPalette(c("blue", "red"))(NCOL(pnl_s))
dygraphs::dygraph(cumsum(pnl_s), main="Daily ETF Momentum Strategies") %>%
  dyOptions(colors=col_ors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Plot EWMA strategies with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(pnl_s))
quantmod::chart_Series(cumsum(pnl_s),
  theme=plot_theme, name="Cumulative Returns of Daily ETF Momentum Strategies")
legend("bottomleft", legend=colnames(pnl_s),
  inset=0.02, bg="white", cex=0.7, lwd=rep(6, NCOL(re_turns)),
  col=plot_theme$col$line.col, bty="n")
# Define backtest functional for daily momentum strategy
# If trend=(-1) then it backtests a mean reverting strategy
momentum_daily <- function(returns, look_back=252, hold_period=5, bid_offer=0.001, trend=1, ...) {
  stopifnot("package:quantmod" %in% search() || require("quantmod", quietly=TRUE))
  # Calculate rolling variance
  vari_ance <- roll::roll_var(returns, width=look_back, min_obs=1)
  vari_ance[1, ] <- 1
  vari_ance[vari_ance <= 0] <- 1
  # Calculate rolling Sharpe
  pas_t <- roll::roll_mean(returns, width=look_back, min_obs=1)
  weight_s <- pas_t/sqrt(vari_ance)
  weight_s <- weight_s/sqrt(rowSums(weight_s^2))
  weight_s <- rutils::lag_it(weight_s)
  # Average the weights over holding period
  weight_s <- roll::roll_mean(weight_s, width=hold_period, min_obs=1)
  # Calculate momentum profits and losses
  pnl_s <- trend*rowMeans(weight_s*returns)
  # Calculate transaction costs
  cost_s <- 0.5*bid_offer*rowSums(abs(rutils::diff_it(weight_s)))
  (pnl_s - cost_s)
}  # end momentum_daily
# Perform sapply loop over holding periods
hold_periods <- seq(2, 11, by=2)
pnl_s <- sapply(hold_periods, momentum_daily, look_back=120,
            returns=re_turns, bid_offer=bid_offer)
colnames(pnl_s) <- paste0("holding=", hold_periods)
pnl_s <- xts::xts(pnl_s, index(re_turns))
# Plot dygraph of daily ETF momentum strategies
col_ors <- colorRampPalette(c("blue", "red"))(NCOL(pnl_s))
dygraphs::dygraph(cumsum(pnl_s), main="Daily ETF Momentum Strategies with Holding Period") %>%
  dyOptions(colors=col_ors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Plot EWMA strategies with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(pnl_s))
quantmod::chart_Series(cumsum(pnl_s),
  theme=plot_theme, name="Cumulative Returns of Daily ETF Momentum Strategies")
legend("bottomleft", legend=colnames(pnl_s),
  inset=0.02, bg="white", cex=0.7, lwd=rep(6, NCOL(re_turns)),
  col=plot_theme$col$line.col, bty="n")
# Load daily S&P500 percentage stock returns.
load(file="/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
# Overwrite NA values in returns_100
returns_100 <- returns_100["2000/"]
returns_100[1, is.na(returns_100[1, ])] <- 0
returns_100 <- zoo::na.locf(returns_100, na.rm=FALSE)
# Simulate a daily S&P500 momentum strategy.
# Perform sapply loop over look_backs
look_backs <- seq(100, 300, by=20)
pnl_s <- sapply(look_backs, momentum_daily,
  hold_period=5, returns=returns_100, bid_offer=0)
colnames(pnl_s) <- paste0("look_back=", look_backs)
pnl_s <- xts::xts(pnl_s, index(returns_100))
# Plot dygraph of daily ETF momentum strategies
col_ors <- colorRampPalette(c("blue", "red"))(NCOL(pnl_s))
dygraphs::dygraph(cumsum(pnl_s), main="Daily S&P500 Momentum Strategies") %>%
  dyOptions(colors=col_ors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Plot daily S&P500 momentum strategies with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- colorRampPalette(c("blue", "red"))(NCOL(pnl_s))
quantmod::chart_Series(cumsum(pnl_s),
  theme=plot_theme, name="Daily S&P500 Momentum Strategies")
legend("bottomleft", legend=colnames(pnl_s),
  inset=0.02, bg="white", cex=0.7, lwd=rep(6, NCOL(re_turns)),
  col=plot_theme$col$line.col, bty="n")
# Perform sapply loop over look_backs
look_backs <- seq(3, 20, by=2)
pnl_s <- sapply(look_backs, momentum_daily,
  hold_period=5, returns=returns_100, bid_offer=0, trend=(-1))
colnames(pnl_s) <- paste0("look_back=", look_backs)
pnl_s <- xts::xts(pnl_s, index(returns_100))
# Plot dygraph of daily ETF momentum strategies
col_ors <- colorRampPalette(c("blue", "red"))(NCOL(pnl_s))
dygraphs::dygraph(cumsum(pnl_s), main="Daily S&P500 Momentum Strategies") %>%
  dyOptions(colors=col_ors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Plot EWMA strategies with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- colorRampPalette(c("blue", "red"))(NCOL(pnl_s))
quantmod::chart_Series(cumsum(pnl_s),
  theme=plot_theme, name="Cumulative Returns of S&P500 Mean Reverting Strategies")
legend("topleft", legend=colnames(pnl_s),
  inset=0.05, bg="white", cex=0.7, lwd=rep(6, NCOL(re_turns)),
  col=plot_theme$col$line.col, bty="n")
# Plot cumulative returns of VTI vs MTUM ETF
weal_th <- log(na.omit(rutils::etf_env$price_s[, c("VTI", "MTUM")]))
colnames(weal_th) <- c("VTI", "MTUM")
weal_th <- rutils::diff_it(weal_th)
dygraphs::dygraph(cumsum(weal_th), main="VTI vs MTUM ETF") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(width=500)
# Select all the ETF symbols except "VXX", "SVXY" and "MTUM"
sym_bols <- colnames(rutils::etf_env$re_turns)
sym_bols <- sym_bols[!(sym_bols %in% c("VXX", "SVXY", "MTUM", "QUAL", "VLUE", "USMV"))]
# Extract columns of rutils::etf_env$re_turns and overwrite NA values
re_turns <- rutils::etf_env$re_turns[, sym_bols]
n_assets <- NCOL(re_turns)
# re_turns <- na.omit(re_turns)
re_turns[1, is.na(re_turns[1, ])] <- 0
re_turns <- zoo::na.locf(re_turns, na.rm=FALSE)
# Returns in excess of risk-free rate
risk_free <- 0.03/252
ex_cess <- (re_turns - risk_free)
# Maximum Sharpe weights in-sample interval
rets_is <- re_turns["/2014"]
in_verse <- MASS::ginv(cov(rets_is))
weight_s <- in_verse %*% colMeans(ex_cess["/2014"])
weight_s <- drop(weight_s/sqrt(sum(weight_s^2)))
names(weight_s) <- colnames(re_turns)
# Plot portfolio weights
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
barplot(sort(weight_s), main="Maximum Sharpe Weights", cex.names=0.7)
# Calculate portfolio returns
portf_is <- xts::xts(rets_is %*% weight_s, index(rets_is))
in_dex <- xts::xts(rowSums(rets_is)/sqrt(n_assets), index(rets_is))
portf_is <- portf_is*sd(in_dex)/sd(portf_is)
# Plot cumulative portfolio returns
pnl_s <- cumsum(cbind(portf_is, in_dex))
colnames(pnl_s) <- c("Optimal Portfolio", "Equal Weight Portfolio")
dygraphs::dygraph(pnl_s, main="In-sample Optimal Portfolio Returns") %>%
  dyOptions(colors=c("red", "blue"), strokeWidth=2) %>%
  dyLegend(width=500)
# Out-of-sample portfolio returns
rets_os <- re_turns["2015/"]
portf_os <- xts::xts(rets_os %*% weight_s, index(rets_os))
in_dex <- xts::xts(rowSums(rets_os)/sqrt(n_assets), index(rets_os))
portf_os <- portf_os*sd(in_dex)/sd(portf_os)
pnl_s <- cbind(portf_os, in_dex, (portf_os + in_dex)/2)
colnames(pnl_s) <- c("Optimal", "Equal Weight", "Combined")
sapply(pnl_s, function(x) mean(x)/sd(x))
# Plot cumulative portfolio returns
dygraphs::dygraph(cumsum(pnl_s), main="Out-of-sample Optimal Portfolio Returns") %>%
  dyOptions(colors=c("red", "blue", "green"), strokeWidth=2) %>%
  dyLegend(width=500)
load("/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
# Overwrite NA values in re_turns
re_turns <- re_turns["2000/"]
n_assets <- NCOL(re_turns)
re_turns[1, is.na(re_turns[1, ])] <- 0
re_turns <- zoo::na.locf(re_turns, na.rm=FALSE)
risk_free <- 0.03/252
ex_cess <- (re_turns - risk_free)
rets_is <- re_turns["/2010"]
rets_os <- re_turns["2011/"]
# Maximum Sharpe weights in-sample interval
cov_mat <- cov(rets_is)
in_verse <- MASS::ginv(cov_mat)
weight_s <- in_verse %*% colMeans(ex_cess["/2010"])
weight_s <- drop(weight_s/sqrt(sum(weight_s^2)))
names(weight_s) <- colnames(re_turns)
# Calculate portfolio returns
portf_is <- xts::xts(rets_is %*% weight_s, index(rets_is))
portf_os <- xts::xts(rets_os %*% weight_s, index(rets_os))
in_dex <- xts::xts(rowSums(re_turns)/sqrt(n_assets), index(re_turns))
# Plot cumulative portfolio returns
pnl_s <- rbind(portf_is, portf_os)
pnl_s <- pnl_s*sd(in_dex)/sd(pnl_s)
pnl_s <- cumsum(cbind(pnl_s, in_dex))
colnames(pnl_s) <- c("Optimal Portfolio", "Equal Weight Portfolio")
dygraphs::dygraph(pnl_s, main="Out-of-sample Optimal Portfolio Returns for Stocks") %>%
  dyOptions(colors=c("red", "blue"), strokeWidth=2) %>%
  dyEvent(index(last(rets_is[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=500)
# Create rectangular matrix with collinear columns
ran_dom <- matrix(rnorm(10*8), nc=10)
# Calculate covariance matrix
cov_mat <- cov(ran_dom)
# Calculate inverse of cov_mat - error
in_verse <- solve(cov_mat)
# Perform eigen decomposition
ei_gen <- eigen(cov_mat)
eigen_vec <- ei_gen$vectors
eigen_val <- ei_gen$values
# Set tolerance for determining zero singular values
to_l <- sqrt(.Machine$double.eps)
# Calculate regularized inverse matrix
not_zero <- (eigen_val > (to_l * eigen_val[1]))
inv_reg <- eigen_vec[, not_zero] %*%
  (t(eigen_vec[, not_zero]) / eigen_val[not_zero])
# Verify inverse property of inv_reg
all.equal(cov_mat, cov_mat %*% inv_reg %*% cov_mat)
# Calculate regularized inverse of cov_mat
in_verse <- MASS::ginv(cov_mat)
# Verify inverse property of mat_rix
all.equal(in_verse, inv_reg)
# Calculate in-sample covariance matrix
cov_mat <- cov(rets_is)
ei_gen <- eigen(cov_mat)
eigen_vec <- ei_gen$vectors
eigen_val <- ei_gen$values
# Calculate shrinkage inverse of covariance matrix
eigen_max <- 21
in_verse <- eigen_vec[, 1:eigen_max] %*%
  (t(eigen_vec[, 1:eigen_max]) / ei_gen$values[1:eigen_max])
# Calculate portfolio weights
weight_s <- in_verse %*% colMeans(ex_cess["/2010"])
weight_s <- drop(weight_s/sqrt(sum(weight_s^2)))
names(weight_s) <- colnames(re_turns)
# Calculate portfolio returns
portf_is <- xts::xts(rets_is %*% weight_s, index(rets_is))
portf_os <- xts::xts(rets_os %*% weight_s, index(rets_os))
in_dex <- xts::xts(rowSums(re_turns)/sqrt(n_assets), index(re_turns))
# Plot cumulative portfolio returns
pnl_s <- rbind(portf_is, portf_os)
pnl_s <- pnl_s*sd(in_dex)/sd(pnl_s)
pnl_s <- cumsum(cbind(pnl_s, in_dex))
colnames(pnl_s) <- c("Optimal Portfolio", "Equal Weight Portfolio")
dygraphs::dygraph(pnl_s, main="Regularized Out-of-sample Optimal Portfolio Returns for Stocks") %>%
  dyOptions(colors=c("red", "blue"), strokeWidth=2) %>%
  dyEvent(index(last(rets_is[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=500)
# Shrink the in-sample returns to their mean
al_pha <- 0.7
excess_mean <- rowMeans(ex_cess["/2010"])
excess_is <- (1 - al_pha)*ex_cess["/2010"] + al_pha*excess_mean
# Calculate portfolio weights
weight_s <- in_verse %*% colMeans(excess_is)
weight_s <- drop(weight_s/sqrt(sum(weight_s^2)))
# Calculate portfolio returns
portf_is <- xts::xts(rets_is %*% weight_s, index(rets_is))
portf_os <- xts::xts(rets_os %*% weight_s, index(rets_os))
# Plot cumulative portfolio returns
pnl_s <- rbind(portf_is, portf_os)
pnl_s <- pnl_s*sd(in_dex)/sd(pnl_s)
pnl_s <- cumsum(cbind(pnl_s, in_dex))
colnames(pnl_s) <- c("Optimal Portfolio", "Equal Weight Portfolio")
dygraphs::dygraph(pnl_s, main="Out-of-sample Returns for Stocks With Regularization and Shrinkage") %>%
  dyOptions(colors=c("red", "blue"), strokeWidth=2) %>%
  dyEvent(index(last(rets_is[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=500)
library(RcppArmadillo)
# Source Rcpp functions from file
Rcpp::sourceCpp("/Users/jerzy/Develop/lecture_slides/scripts/back_test.cpp")
# Create random matrix of returns
mat_rix <- matrix(rnorm(300), nc=5)
# Regularized inverse of covariance matrix
eigen_max <- 4
ei_gen <- eigen(cov(mat_rix))
cov_inv <- ei_gen$vectors[, 1:eigen_max] %*%
  (t(ei_gen$vectors[, 1:eigen_max]) / ei_gen$values[1:eigen_max])
# Regularized inverse using RcppArmadillo
cov_inv_arma <- calc_inv(mat_rix, eigen_max)
all.equal(cov_inv, cov_inv_arma)
# Microbenchmark RcppArmadillo code
library(microbenchmark)
summary(microbenchmark(
  pure_r={ei_gen <- eigen(cov(mat_rix))
    ei_gen$vectors[, 1:eigen_max] %*%
(t(ei_gen$vectors[, 1:eigen_max]) / ei_gen$values[1:eigen_max])
  },
  r_cpp=calc_inv(mat_rix, eigen_max),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
# Overwrite NA values in returns_100
returns_100[1, is.na(returns_100[1, ])] <- 0
returns_100 <- zoo::na.locf(returns_100, na.rm=FALSE)
ex_cess <- (returns_100 - risk_free)
n_cols <- NCOL(returns_100) ; date_s <- index(returns_100)
n_assets <- NCOL(returns_100)
# Define monthly end points
end_p <- rutils::calc_endpoints(returns_100, inter_val="months")
end_p <- end_p[end_p > (n_cols+1)]
n_rows <- NROW(end_p) ; look_back <- 12
start_p <- c(rep_len(0, look_back-1), end_p[1:(n_rows-look_back+1)])
# Perform loop over end points
pnl_s <- lapply(2:n_rows, function(i) {
    # Subset the ex_cess returns
    ex_cess <- ex_cess[start_p[i-1]:end_p[i-1], ]
    in_verse <- MASS::ginv(cov(ex_cess))
    # Calculate the maximum Sharpe ratio portfolio weights
    weight_s <- in_verse %*% colMeans(ex_cess)
    weight_s <- drop(weight_s/sqrt(sum(weight_s^2)))
    # Calculate the out-of-sample portfolio returns
    re_turns <- returns_100[(end_p[i-1]+1):end_p[i], ]
    xts::xts(re_turns %*% weight_s, index(re_turns))
})  # end lapply
pnl_s <- rutils::do_call(rbind, pnl_s)
# Calculate returns of equal weight portfolio
in_dex <- xts::xts(rowMeans(returns_100), date_s)
# Plot cumulative strategy returns
weal_th <- na.omit(cbind(pnl_s, in_dex*sd(rets_portf)/sd(in_dex)))
colnames(weal_th) <- c("Rolling Strategy", "Equal Weight")
dygraphs::dygraph(cumsum(weal_th), main="Rolling Portfolio Optimization Strategy for S&P500 Stocks") %>%
  dyOptions(colors=c("red", "blue"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Shift end points to C++ convention
end_p <- (end_p - 1)
end_p[end_p < 0] <- 0
start_p <- (start_p - 1)
start_p[start_p < 0] <- 0
# Specify shrinkage intensity
al_pha <- 0.7
eigen_max <- 21
# Perform backtest in Rcpp
pnl_s <- HighFreq::back_test(excess=ex_cess, returns=returns_100,
  startp=start_p, endp=end_p, alpha=al_pha, eigen_max=eigen_max, method="max_sharpe")
# Plot cumulative strategy returns
weal_th <- cbind(pnl_s, in_dex, (pnl_s+in_dex)/2)
weal_th <- cumsum(na.omit(weal_th))
col_names <- c("Strategy", "Index", "Combined")
colnames(weal_th) <- col_names
dygraphs::dygraph(weal_th[end_p], main="Rolling S&P500 Portfolio Optimization Strategy With Shrinkage") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="red", strokeWidth=1) %>%
  dySeries(name=col_names[2], axis="y2", col="blue", strokeWidth=1) %>%
  dySeries(name=col_names[3], axis="y2", col="green", strokeWidth=2)
# Perform backtest over alphas
alpha_s <- seq(from=0.01, to=0.91, by=0.1)
pnl_s <- lapply(alpha_s, function(al_pha) {
  HighFreq::back_test(excess=ex_cess, returns=returns_100,
  startp=start_p, endp=end_p, alpha=al_pha, eigen_max=eigen_max, method="max_sharpe")
})  # end lapply
pro_file <- sapply(pnl_s, sum)
plot(x=alpha_s, y=pro_file, t="l", main="Strategy PnL as Function of Shrinkage Intensity Alpha",
  xlab="Shrinkage Intensity Alpha", ylab="pnl")
al_pha <- alpha_s[which.max(pro_file)]
pnl_s <- pnl_s[[which.max(pro_file)]]
# Perform backtest over eigen_maxs
eigen_maxs <- seq(from=3, to=40, by=2)
pnl_s <- lapply(eigen_maxs, function(eigen_max) {
  HighFreq::back_test(excess=ex_cess, returns=returns_100,
    startp=start_p, endp=end_p, alpha=al_pha, eigen_max=eigen_max, method="max_sharpe")
})  # end lapply
pro_file <- sapply(pnl_s, sum)
plot(x=eigen_maxs, y=pro_file, t="l", main="Strategy PnL as Function of eigen_max",
  xlab="eigen_max", ylab="pnl")
eigen_max <- eigen_maxs[which.max(pro_file)]
pnl_s <- pnl_s[[which.max(pro_file)]]
# Plot cumulative strategy returns
weal_th <- cbind(pnl_s, in_dex, (pnl_s+in_dex)/2)
weal_th <- cumsum(na.omit(weal_th))
col_names <- c("Strategy", "Index", "Combined")
colnames(weal_th) <- col_names
dygraphs::dygraph(weal_th[end_p], main="Optimal Rolling S&P500 Portfolio Strategy") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="red", strokeWidth=1) %>%
  dySeries(name=col_names[2], axis="y2", col="blue", strokeWidth=1) %>%
  dySeries(name=col_names[3], axis="y2", col="green", strokeWidth=2)
# Perform backtest over look-backs
look_backs <- seq(from=3, to=24, by=1)
pnl_s <- lapply(look_backs, function(look_back) {
  start_p <- c(rep_len(0, look_back-1), end_p[1:(n_rows-look_back+1)])
  start_p <- (start_p - 1)
  start_p[start_p < 0] <- 0
  HighFreq::back_test(excess=ex_cess, returns=returns_100,
    startp=start_p, endp=end_p, alpha=al_pha, eigen_max=eigen_max, method="max_sharpe")
})  # end lapply
pro_file <- sapply(pnl_s, sum)
plot(x=look_backs, y=pro_file, t="l", main="Strategy PnL as Function of Look-back Interval",
  xlab="Look-back Interval", ylab="pnl")
look_back <- look_backs[which.max(pro_file)]
pnl_s <- pnl_s[[which.max(pro_file)]]
# Plot cumulative strategy returns
weal_th <- cbind(pnl_s, in_dex, (pnl_s+in_dex)/2)
weal_th <- cumsum(na.omit(weal_th))
col_names <- c("Strategy", "Index", "Combined")
colnames(weal_th) <- col_names
dygraphs::dygraph(weal_th[end_p], main="Optimal Rolling S&P500 Portfolio Strategy") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="red", strokeWidth=1) %>%
  dySeries(name=col_names[2], axis="y2", col="blue", strokeWidth=1) %>%
  dySeries(name=col_names[3], axis="y2", col="green", strokeWidth=2)
