library(knitr)
opts_chunk$set(prompt=TRUE, eval=FALSE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
options(digits=3)
thm <- knit_theme$get("acid")
knit_theme$set(thm)

# Open x11 for plotting
x11(width=6, height=4)
# Set plot parameters to reduce whitespace around plot
par(mar=c(4, 4, 3, 1), oma=c(0, 0, 0, 0))
# Test if IEF can time VTI
re_turns <- na.omit(rutils::etf_env$re_turns[, c("IEF", "VTI")])
vt_i <- re_turns$VTI
re_turns <- cbind(re_turns, 0.5*(vt_i+abs(vt_i)), vt_i^2)
colnames(re_turns)[3:4] <- c("merton", "treynor")
# Merton-Henriksson test
mod_el <- lm(IEF ~ VTI + merton, data=re_turns); summary(mod_el)
# Treynor-Mazuy test
mod_el <- lm(IEF ~ VTI + treynor, data=re_turns); summary(mod_el)
# Plot residual scatterplot
residual_s <- (re_turns$IEF - mod_el$coefficients[2]*re_turns$VTI)
plot.default(x=re_turns$VTI, y=residual_s, xlab="VTI", ylab="IEF")
title(main="Treynor-Mazuy market timing test\n for IEF vs VTI", line=0.5)
# Plot fitted (predicted) response values
fit_ted <- (mod_el$fitted.values - mod_el$coefficients[2]*re_turns$VTI)
points.default(x=re_turns$VTI, y=fit_ted, pch=16, col="red")
text(x=0.05, y=0.03, paste("Treynor test t-value =", round(summary(mod_el)$coefficients["treynor", "t value"], 2)))

# Calculate positions
re_turns <- na.omit(rutils::etf_env$re_turns$VTI)
position_s <- rep(NA_integer_, NROW(re_turns))
in_dex <- index(re_turns)
in_dex <- format(in_dex, "%m-%d")
position_s[in_dex == "05-01"] <- 0
position_s[in_dex == "05-03"] <- 0
position_s[in_dex == "11-01"] <- 1
position_s[in_dex == "11-03"] <- 1
# Carry forward and backward non-NA position_s
position_s <- zoo::na.locf(position_s, na.rm=FALSE)
position_s <- zoo::na.locf(position_s, fromLast=TRUE)
# Calculate strategy returns
sell_inmay <- position_s*re_turns
weal_th <- cbind(cumprod(1 + re_turns),
           cumprod(1 + sell_inmay))
colnames(weal_th) <- c("vti", "sell_in_may")

# Plot Sell in May strategy
dygraphs::dygraph(weal_th, main="Sell in May Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always")
# OR: Open x11 for plotting
x11(width=6, height=4)
par(mar=c(4, 4, 3, 1), oma=c(0, 0, 0, 0))
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("blue", "red")
chart_Series(weal_th, theme=plot_theme,
       name="Sell in May Strategy")
legend("topleft", legend=colnames(weal_th),
  inset=0.1, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")

# Test if Sell in May strategy can time VTI
vt_i <- re_turns$VTI
re_turns <- cbind(re_turns, 0.5*(vt_i+abs(vt_i)), vt_i^2)
colnames(re_turns)[2:3] <- c("merton", "treynor")
# Merton-Henriksson test
mod_el <- lm(sell_inmay ~ VTI + merton, data=re_turns); summary(mod_el)
# Treynor-Mazuy test
mod_el <- lm(sell_inmay ~ VTI + treynor, data=re_turns); summary(mod_el)
# Plot residual scatterplot
residual_s <- (sell_inmay - mod_el$coefficients[2]*vt_i)
plot.default(x=vt_i, y=residual_s, xlab="VTI", ylab="sell_inmay")
title(main="Treynor-Mazuy market timing test\n for Sell in May vs VTI", line=0.5)
# Plot fitted (predicted) response values
fit_ted <- (mod_el$fitted.values - mod_el$coefficients[2]*vt_i)
points.default(x=vt_i, y=fit_ted, pch=16, col="red")
text(x=0.05, y=0.03, paste("Treynor test t-value =", round(summary(mod_el)$coefficients["treynor", "t value"], 2)))

library(xtable)
gambl_e <- data.frame(win=c("p", "a"), lose=c("q = 1 - p", "-b"))
rownames(gambl_e) <- c("probability", "payout")
# print(xtable(gambl_e), comment=FALSE, size="tiny")
print(xtable(gambl_e), comment=FALSE)

# Load S&P500 constituent stock prices
load("C:/Develop/lecture_slides/data/sp500.RData")
price_s <- eapply(env_sp500, quantmod::Cl)
price_s <- rutils::do_call(cbind, price_s)
# Carry forward and backward non-NA prices
price_s <- zoo::na.locf(price_s, na.rm=FALSE)
price_s <- zoo::na.locf(price_s, fromLast=TRUE)
colnames(price_s) <- unname(sapply(colnames(price_s),
  function(col_name) strsplit(col_name, split="[.]")[[1]][1]))
# Calculate percentage returns of the S&P500 constituent stocks
# re_turns <- rutils::diff_it(log(price_s))
re_turns <- lapply(price_s, function(x)
  rutils::diff_it(x)/rutils::lag_it(x))
re_turns <- rutils::do_call(cbind, re_turns)
set.seed(1121)
sam_ple <- sample(NCOL(re_turns), s=100, replace=FALSE)
returns_100 <- re_turns[, sam_ple]
save(price_s, re_turns, returns_100,
  file="C:/Develop/lecture_slides/data/sp500_prices.RData")
# Calculate number of constituents without prices
da_ta <- rowSums(rutils::roll_sum(re_turns, 4)==0)
da_ta <- xts::xts(da_ta, order.by=index(re_turns))
dygraphs::dygraph(da_ta, main="Number of S&P 500 Constituents Without Prices") %>%
  dyAxis("y", valueRange=c(0, 300))

# Calculate price weighted index of constituent
n_cols <- NCOL(price_s)
in_dex <- xts(rowSums(price_s)/n_cols, index(price_s))
colnames(in_dex) <- "index"
# Combine index with VTI
da_ta <- cbind(in_dex[index(etf_env$VTI)], etf_env$VTI[, 4])
col_names <- c("index", "VTI")
colnames(da_ta) <- col_names
# Plot index with VTI
dygraphs::dygraph(da_ta,
  main="S&P 500 Price-weighted Index and VTI") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="red") %>%
  dySeries(name=col_names[2], axis="y2", col="blue")

library(xtable)
# Select ETF symbols for asset allocation
sym_bols <- c("VTI", "VEU", "IEF", "VNQ",
  "DBC", "XLY", "XLP", "XLE", "XLF", "XLV",
  "XLI", "XLB", "XLK", "XLU", "VYM", "IVW",
  "IWB", "IWD", "IWF", "MTUM", "VXX", "SVXY")
# Read etf database into data frame
etf_list <- read.csv(
  file="C:/Develop/lecture_slides/data/etf_list.csv",
         stringsAsFactors=FALSE)
rownames(etf_list) <- etf_list$Symbol
# Select from etf_list only those ETF's in sym_bols
etf_list <- etf_list[sym_bols, ]
# Shorten names
etf_names <- sapply(etf_list$Name,
              function(name) {
  name_split <- strsplit(name, split=" ")[[1]]
  name_split <-
    name_split[c(-1, -NROW(name_split))]
  name_match <- match("Select", name_split)
  if (!is.na(name_match))
    name_split <- name_split[-name_match]
  paste(name_split, collapse=" ")
})  # end sapply
etf_list$Name <- etf_names
etf_list["IEF", "Name"] <- "Treasury Bond Fund"
etf_list["XLY", "Name"] <- "Consumer Discr. Sector Fund"
etf_list["MTUM", "Name"] <- "Momentum Factor"
etf_list["SVXY", "Name"] <- "Short VIX Futures"
etf_list["VXX", "Name"] <- "Long VIX Futures"

print(xtable(etf_list), comment=FALSE, size="tiny", include.rownames=FALSE)

library(rutils)
# Extract ETF prices from rutils::etf_env$price_s
price_s <- rutils::etf_env$price_s
price_s <- zoo::na.locf(price_s, na.rm=FALSE)
price_s <- zoo::na.locf(price_s, fromLast=TRUE)
# Calculate simple dollar returns
sim_ple <- rutils::diff_it(price_s)
# Or 
sim_ple <- lapply(price_s, rutils::diff_it)
sim_ple <- rutils::do_call(cbind, sim_ple)
# Calculate percentage returns
per_cent <- sim_ple/rutils::lag_it(price_s, lagg=1)
# Calculate log returns
log_rets <- rutils::diff_it(log(price_s))
# Calculate prices from simple dollar returns
sim_ple[1, ] <- price_s[1, ]
new_prices <- cumsum(sim_ple)
all.equal(new_prices, price_s)
# Calculate prices from percentage returns
new_prices <- cumprod(1 + per_cent)
new_prices <- lapply(1:NCOL(new_prices), function (i)
  as.numeric(price_s[1, i])*new_prices[, i])
new_prices <- rutils::do_call(cbind, new_prices)
all.equal(new_prices, price_s)

# Wealth for equal and fixed number of shares (no rebalancing)
weight_s <- c(0.5, 0.5)
sim_ple <- sim_ple[, c("VTI", "IEF")]
re_turns <- sim_ple %*% weight_s
weal_th <- cumsum(re_turns)
# Wealth for equal dollar amount of shares (with rebalancing)
per_cent <- per_cent[, c("VTI", "IEF")]
re_turns <- (per_cent %*% weight_s)
weal_th <- sum(weight_s)*cumprod(1 + re_turns)
# Wealth for equal dollar amount of shares (without rebalancing)
wealth_nreb <- cumprod(1 + per_cent) %*% weight_s
# Plot compounded wealth
weal_th <- cbind(weal_th, wealth_nreb)
weal_th <- xts::xts(weal_th, index(per_cent))
colnames(weal_th) <- c("With rebalancing", "Without rebalancing")
dygraphs::dygraph(weal_th, main="Wealth for Equal Dollar Amount of Shares") %>%
  dyOptions(colors=c("green","blue"), strokeWidth=2) %>%
  dyLegend(show="always")

# Extra amount of stocks
ex_tra <- lapply(per_cent, function(x)
  rutils::lag_it(weal_th)*(re_turns - x))
ex_tra <- lapply(seq_along(ex_tra), function(i)
  weight_s[i]*ex_tra[[i]])
ex_tra <- do.call(cbind, ex_tra)
# ex_tra <- t(weight_s*t(ex_tra))
# bid_offer equal to 10 bps for liquid ETFs
bid_offer <- 0.001
# Calculate the transaction costs
cost_s <- bid_offer*rowSums(abs(ex_tra))/2

# Calculate standardized simple dollar returns
sim_ple <- lapply(sim_ple, function(x) {
  x <- (x - mean(x))
  x/sd(x)})
sim_ple <- do.call(cbind, sim_ple)
sapply(sim_ple, sd)
# Wealth for equal and fixed number of shares (no rebalancing)
re_turns <- sim_ple %*% weight_s
weal_th <- cumsum(re_turns)
# Calculate standardized percentage returns
per_cent <- lapply(per_cent, function(x) {
  x <- (x - mean(x))
  x/sd(x)})
per_cent <- do.call(cbind, per_cent)
sapply(per_cent, sd)
# Wealth for equal dollar amount of shares (with rebalancing)
re_turns <- (per_cent %*% weight_s)
weal_th <- sum(weight_s)*cumprod(1 + re_turns)
# Wealth for equal dollar amount of shares (without rebalancing)
wealth_nreb <- cumprod(1 + per_cent) %*% weight_s
# Plot compounded wealth
weal_th <- cbind(weal_th, wealth_nreb)
weal_th <- xts::xts(weal_th, index(per_cent))
colnames(weal_th) <- c("With rebalancing", "Without rebalancing")
dygraphs::dygraph(weal_th, main="Wealth for Equal Dollar Amount of Shares") %>%
  dyOptions(colors=c("green","blue"), strokeWidth=2) %>%
  dyLegend(show="always")

library(HighFreq)  # Load package HighFreq
# Calculate stock and bond returns
re_turns <- na.omit(
  rutils::etf_env$re_turns[, c("VTI", "IEF")])
weight_s <- c(0.4, 0.6)
re_turns <- cbind(re_turns, re_turns %*% weight_s)
colnames(re_turns)[3] <- "combined"
# Calculate compounded wealth from returns
weal_th <- lapply(re_turns,
  function(x) cumprod(1 + x))
weal_th <- do.call(cbind, weal_th)
# Plot compounded wealth
dygraphs::dygraph(weal_th, main="Stock and Bond Portfolio") %>%
  dyOptions(colors=c("green","blue","green")) %>%
  dySeries("combined", color="red", strokeWidth=2) %>%
  dyLegend(show="always")

# Calculate correlations
cor(re_turns)
# Calculate Sharpe ratios
sqrt(252)*sapply(re_turns, function(x) mean(x)/sd(x))
# Calculate standard deviations
sapply(re_turns, sd)
# Calculate standardized returns
re_turns <- lapply(re_turns, function(x) {
  x <- (x - mean(x))
  x/sd(x)})
re_turns <- do.call(cbind, re_turns)
sapply(re_turns, sd)
# Calculate skewness and kurtosis
t(sapply(re_turns, function(x) {
  c(skew=mean(x^3), kurt=mean(x^4))
}))
# Or
sapply(c(skew=3, kurt=4), function(x)
  moments::moment(re_turns, order=x, central=TRUE))

# Extract ETF returns
sym_bols <- c("VTI", "IEF", "DBC")
re_turns <- na.omit(rutils::etf_env$re_turns[, sym_bols])
# Calculate all-weather portfolio wealth
weights_aw <- c(0.30, 0.55, 0.15)
re_turns <- cbind(re_turns, re_turns %*% weights_aw)
colnames(re_turns)[4] <- "all_weather"

# Calculate compounded wealth from returns
weal_th <- lapply(re_turns,
  function(x) cumprod(1 + x))
weal_th <- do.call(cbind, weal_th)
# dygraph all-weather wealth
dygraphs::dygraph(weal_th, main="All-Weather Portfolio") %>%
  dyOptions(colors=c("orange", "blue", "green", "red")) %>%
  dySeries("all_weather", color="red", strokeWidth=2) %>%
  dyLegend(show="always")
# Plot all-weather wealth
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue", "green", "red")
chart_Series(weal_th, theme=plot_theme, lwd=c(2, 2, 2, 4),
       name="All-Weather Portfolio")
legend("topleft", legend=colnames(weal_th),
  inset=0.1, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")

library(xtable)
gambl_e <- data.frame(win=c("p", "a", "1 + a"), lose=c("q = 1 - p", "-b", "1 - b"))
rownames(gambl_e) <- c("probability", "payout", "terminal wealth")
# print(xtable(gambl_e), comment=FALSE, size="tiny")
print(xtable(gambl_e), comment=FALSE)

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
re_turns <- rutils::etf_env$re_turns$VTI
re_turns <- na.omit(re_turns)
c(mean=mean(re_turns), std=sd(re_turns))
range(re_turns)

# Open x11 for plotting
x11(width=5, height=4)
# Set plot parameters to reduce whitespace around plot
par(mar=c(4, 4, 2, 1), oma=c(0, 0, 0, 0))
# Define vectorized logarithmic utility function
utili_ty <- function(kell_y, re_turns) {
  sapply(kell_y, function(x)
    sum(log(1 + x*re_turns)))
}  # end utili_ty
utili_ty(1, re_turns)
utili_ty(c(1, 4), re_turns)
# Plot the logarithmic utility
curve(expr=utili_ty(x, re_turns=re_turns),
xlim=c(0.1, 5), xlab="leverage", ylab="utility",
main="Utility of Asset Returns", lwd=2)

# Approximate Kelly leverage
mean(re_turns)/var(re_turns)
PerformanceAnalytics::KellyRatio(R=re_turns, method="full")
# Kelly leverage
unlist(optimize(
  f=function(x) -utili_ty(x, re_turns),
  interval=c(1, 4)))

# Calculate the VTI returns
re_turns <- rutils::etf_env$re_turns$VTI
re_turns <- na.omit(re_turns)
# Calculate wealth paths
kelly_ratio <- drop(mean(re_turns)/var(re_turns))
kelly_wealth <- cumprod(1 + kelly_ratio*re_turns)
hyper_kelly <- cumprod(1 + (kelly_ratio+2)*re_turns)
sub_kelly <- cumprod(1 + (kelly_ratio-2)*re_turns)
kelly_paths <- cbind(kelly_wealth, hyper_kelly, sub_kelly)
colnames(kelly_paths) <- c("kelly", "hyper-kelly", "sub-kelly")

# Plot wealth paths
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("black", "orange", "blue")
chart_Series(kelly_paths, theme=plot_theme,
       name="Wealth Paths")
legend("topleft", legend=colnames(kelly_paths),
 inset=0.1, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")

# bid_offer equal to 10 bps for liquid ETFs
bid_offer <- 0.001
# Calculate wealth paths
kelly_ratio <- drop(mean(re_turns)/var(re_turns))
weal_th <- cumprod(1 + kelly_ratio*re_turns)
wealth_trans <- cumprod(1 + kelly_ratio*re_turns - 0.5*bid_offer*kelly_ratio*(kelly_ratio-1)*abs(re_turns))
# Calculate compounded wealth from returns
weal_th <- cbind(weal_th, wealth_trans)
colnames(weal_th) <- c("Kelly", "Including bid-offer")
# Plot compounded wealth
dygraphs::dygraph(weal_th, main="Kelly Strategy With Transaction Costs") %>%
  dyOptions(colors=c("green","blue"), strokeWidth=2) %>%
  dyLegend(show="always")

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
re_turns <- rutils::etf_env$re_turns$VTI
re_turns <- na.omit(re_turns)
# Calculate wealth paths
kelly_ratio <- drop(mean(re_turns)/var(re_turns))
kelly_wealth <- cumprod(1 + kelly_ratio*re_turns)
hyper_kelly <- cumprod(1 + (kelly_ratio+2)*re_turns)
sub_kelly <- cumprod(1 + (kelly_ratio-2)*re_turns)
kelly_paths <- cbind(kelly_wealth, hyper_kelly, sub_kelly)
colnames(kelly_paths) <- c("kelly", "hyper-kelly", "sub-kelly")

# Plot wealth paths
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("black", "orange", "blue")
chart_Series(kelly_paths, theme=plot_theme,
       name="Wealth Paths")
legend("topleft", legend=colnames(kelly_paths),
 inset=0.1, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")

# Calculate higher moments of VTI returns
c(mean=sum(re_turns),
  variance=sum(re_turns^2),
  mom3=sum(re_turns^3),
  mom4=sum(re_turns^4))/NROW(re_turns)
# Calculate higher moments of minutely SPY returns
re_turns <- HighFreq::SPY[, 4]
re_turns <- na.omit(re_turns)
re_turns <- HighFreq::diff_it(log(re_turns))
c(mean=sum(re_turns),
  variance=sum(re_turns^2),
  mom3=sum(re_turns^3),
  mom4=sum(re_turns^4))/NROW(re_turns)

re_turns <- na.omit(
  rutils::etf_env$re_turns[, c("VTI", "IEF")])
# Logarithmic utility of stock and bond portfolio
utili_ty <- function(w_s, w_b) {
  -sum(log(1 + w_s*re_turns$VTI + w_b*re_turns$IEF))
}  # end utili_ty
# Draw 3d surface plot of utility
library(rgl)  # Load rgl
w_s <- seq(from=3, to=8, by=0.2)
w_b <- seq(from=10, to=20, by=0.2)
utility_mat <- sapply(w_b, function(y) sapply(w_s,
  function(x) utili_ty(x, y)))
rgl::persp3d(w_s, w_b, utility_mat, col="green",
  xlab="stocks", ylab="bonds", zlab="utility")
rgl::rgl.snapshot("utility_surface.png")

# Approximate Kelly weights
weight_s <- sapply(re_turns,
function(x) mean(x)/var(x))
# Kelly weight for stocks
unlist(optimize(f=function(x)
  utili_ty(x, w_b=0), interval=c(1, 4)))
# Kelly weight for bonds
unlist(optimize(f=function(x)
  utili_ty(x, w_s=0), interval=c(1, 14)))
# Vectorized utility of stock and bond portfolio
utility_vec <- function(weight_s) {
  utili_ty(weight_s[1], weight_s[2])
}  # end utility_vec
# Optimize with respect to vector argument
op_tim <- optim(fn=utility_vec, par=c(3, 10),
          method="L-BFGS-B",
          upper=c(8, 20),
          lower=c(2, 5))
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
  weight_s[1]*re_turns$VTI +
    weight_s[2]*re_turns$IEF)
colnames(re_turns)[3] <- "Kelly_sub_optimal"
# Calculate compounded wealth from returns
weal_th <- lapply(re_turns,
  function(x) cumprod(1 + x))
weal_th <- do.call(cbind, weal_th)
# Plot compounded wealth
dygraphs::dygraph(weal_th, main="Stock and Bond Portfolio") %>%
  dyOptions(colors=c("green","blue","green")) %>%
  dySeries("Kelly_sub_optimal", color="red", strokeWidth=2) %>%
  dyLegend(show="always")

re_turns <- na.omit(
  rutils::etf_env$re_turns[, c("VTI", "IEF")])
# Calculate rolling returns and variance
look_back <- 200
var_rolling <- roll::roll_var(re_turns, width=look_back)
weight_s <- roll::roll_sum(re_turns, width=look_back)/look_back
weight_s <- weight_s/var_rolling
weight_s <- zoo::na.locf(weight_s, fromLast=TRUE)
sum(is.na(weight_s))
range(weight_s)

# Plot the weight_s
x11(width=6, height=4)
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
  dyOptions(colors=c("green","blue"), strokeWidth=2) %>%
  dyLegend(show="always")

# Calculate compounded wealth from returns
weal_th <- cumprod(1 + rowSums(weight_s*re_turns))
weal_th <- xts(weal_th, index(re_turns))
chart_Series(weal_th, name="Rolling Kelly Strategy For VTI and IEF")
# Calculate the compounded Kelly wealth and VTI
weal_th <- cbind(weal_th,
  cumprod(1 + 0.6*re_turns$IEF + 0.4*re_turns$VTI))
colnames(weal_th) <- c("Kelly Strategy", "VTI plus IEF")
dygraphs::dygraph(weal_th, main="Rolling Kelly Strategy For VTI and IEF") %>%
  dyAxis("y", label="Kelly Strategy", independentTicks=TRUE) %>%
  dyAxis("y2", label="VTI plus IEF", independentTicks=TRUE) %>%
  dySeries(name="Kelly Strategy", axis="y", label="Kelly Strategy", strokeWidth=1, col="red") %>%
  dySeries(name="VTI plus IEF", axis="y2", label="VTI plus IEF", strokeWidth=1, col="blue")

library(rutils)  # Load package rutils
# Calculate ETF returns
re_turns <- na.omit(
  rutils::etf_env$re_turns[, c("IEF", "VTI")])
re_turns <- cbind(re_turns,
  0.6*re_turns$IEF+0.4*re_turns$VTI)
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
price_s <- lapply(re_turns,
  function(x) exp(cumsum(x)))
price_s <- do.call(cbind, price_s)
# Plot prices
dygraphs::dygraph(price_s, main="Stock and Bond Portfolio") %>%
  dyOptions(colors=c("green","blue","green")) %>%
  dySeries("combined", color="red", strokeWidth=2) %>%
  dyLegend(show="always")

# Get close prices and calculate close-to-close returns
# price_s <- quantmod::Cl(rutils::etf_env$VTI)
price_s <- quantmod::Cl(HighFreq::SPY)
colnames(price_s) <- rutils::get_name(colnames(price_s))
re_turns <- TTR::ROC(price_s)
re_turns[1] <- 0
# Calculate the RSI indicator
r_si <- TTR::RSI(price_s, 2)
# Calculate the long (up) and short (dn) signals
sig_up <- ifelse(r_si < 10, 1, 0)
sig_dn <- ifelse(r_si > 90, -1, 0)
# Lag signals by one period
sig_up <- rutils::lag_it(sig_up, 1)
sig_dn <- rutils::lag_it(sig_dn, 1)
# Replace NA signals with zero position
sig_up[is.na(sig_up)] <- 0
sig_dn[is.na(sig_dn)] <- 0
# Combine up and down signals into one
sig_nals <- sig_up + sig_dn
# Calculate cumulative returns
eq_up <- exp(cumsum(sig_up*re_turns))
eq_dn <- exp(cumsum(-1*sig_dn*re_turns))
eq_all <- exp(cumsum(sig_nals*re_turns))

# Plot daily cumulative returns in panels
end_points <- endpoints(re_turns, on="days")
plot.zoo(cbind(eq_all, eq_up, eq_dn)[end_points], lwd=c(2, 2, 2),
  ylab=c("Total","Long","Short"), col=c("red","green","blue"),
  main=paste("RSI(2) strategy for", colnames(price_s), "from",
       format(start(re_turns), "%B %Y"), "to",
       format(end(re_turns), "%B %Y")))

# Calculate open, close, and lagged prices
sym_bol <- "VTI"
# oh_lc <- rutils::etf_env$VTI
oh_lc <- get(sym_bol, rutils::etf_env)
op_en <- quantmod::Op(oh_lc)
cl_ose <- quantmod::Cl(oh_lc)
vol_ume <- quantmod::Vo(oh_lc)
star_t <- as.numeric(cl_ose[1])
# Define aggregation interval and calculate VWAP
look_back <- 200
v_wap <- rutils::roll_sum(x_ts=cl_ose*vol_ume,
  look_back=look_back)
volume_roll <- rutils::roll_sum(x_ts=vol_ume,
  look_back=look_back)
v_wap <- v_wap/volume_roll
v_wap[is.na(v_wap)] <- 0

# Plot prices and VWAP
x11(width=6, height=5)
chart_Series(x=cl_ose,
  name="VTI prices and VWAP", col="orange")
add_TA(v_wap, on=1, lwd=2, col="blue")
legend("top", legend=c("VTI", "VWAP"),
  bg="white", lty=1, lwd=6,
  col=c("orange", "blue"), bty="n")

# Calculate VWAP indicator
indica_tor <- sign(cl_ose - v_wap)
# Calculate positions as lagged indicator
position_s <- rutils::lag_it(indica_tor)
# Calculate daily profits and losses of strategy
re_turns <- rutils::diff_it(cl_ose)
pnl_s <- re_turns*position_s
cum_pnls <- star_t + cumsum(pnl_s)
# Annualized Sharpe ratio of VWAP strategy
sqrt(252)*sum(pnl_s)/sd(pnl_s)/NROW(pnl_s)
# Annualized Sharpe ratio of VTI
sqrt(252)*sum(re_turns)/sd(re_turns)/NROW(pnl_s)
# Plot prices and VWAP
chart_Series(x=cl_ose, name="VWAP Crossover Strategy for VTI", col="orange")
add_TA(cum_pnls, on=1, lwd=2, col="blue")
add_TA(position_s > 0, on=-1,
 col="lightgreen", border="lightgreen")
add_TA(position_s < 0, on=-1,
 col="lightgrey", border="lightgrey")
legend("top", legend=c(sym_bol, "VWAP strategy"),
 inset=0.1, bg="white", lty=1, lwd=6,
 col=c("orange", "blue"), bty="n")

# Plot prices and VWAP
da_ta <- cbind(cl_ose, cum_pnls, v_wap)
colnames(da_ta) <- c(sym_bol, "strategy", "vwap")
dygraphs::dygraph(da_ta, main=paste(sym_bol, "VWAP Strategy")) %>%
  dyAxis("y", label=sym_bol, independentTicks=TRUE) %>%
  dyAxis("y2", label="strategy", independentTicks=TRUE) %>%
  dySeries(name=sym_bol, axis="y", label=sym_bol, strokeWidth=2, col="red") %>%
  dySeries(name="vwap", axis="y", label="vwap", strokeWidth=2, col="green") %>%
  dySeries(name="strategy", axis="y2", label="strategy", strokeWidth=2, col="blue")

# Calculate positions from lagged indicator
lagg <- 2
indic_sum <- roll::roll_sum(indica_tor, width=lagg)
indic_sum[1:lagg] <- 0
position_s <- rep(NA_integer_, NROW(cl_ose))
position_s[1] <- 0
position_s <- ifelse(indic_sum == lagg, 1, position_s)
position_s <- ifelse(indic_sum == (-lagg), -1, position_s)
position_s <- zoo::na.locf(position_s, na.rm=FALSE)
# Lag the positions to trade in next period
position_s <- rutils::lag_it(position_s, lagg=1)

# Calculate PnLs of lagged strategy
pnl_s <- re_turns*position_s
cum_pnls_lag <- star_t + cumsum(pnl_s)
# Plot both strategies
da_ta <- cbind(cum_pnls_lag, cum_pnls)
colnames(da_ta) <- c("lag_strategy", "strategy")
dygraphs::dygraph(da_ta, main=paste(sym_bol, "VWAP Strategy With Lag")) %>%
  dySeries(name="lag_strategy", label="Strategy With Lag", strokeWidth=2, col="green") %>%
  dySeries(name="strategy", label="Strategy", strokeWidth=2, col="blue")

# Determine dates right after VWAP has crossed prices
trade_dates <- (rutils::diff_it(indica_tor) != 0)
trade_dates <- which(trade_dates) + 1
# Calculate positions, either: -1, 0, or 1
position_s <- rep(NA_integer_, NROW(oh_lc))
position_s[1] <- 0
position_s[trade_dates] <- indica_tor[trade_dates-1]
position_s <- zoo::na.locf(position_s, na.rm=FALSE)
position_s <- xts(position_s, order.by=index(oh_lc))
pos_lagged <- rutils::lag_it(position_s)
# Calculate pnl for days without trade
pnl_s <- rutils::diff_it(cl_ose)*position_s
# Calculate realized pnl for days with trade
close_lag <- rutils::lag_it(cl_ose)
pnl_s[trade_dates] <- pos_lagged[trade_dates]*
  (op_en[trade_dates] - close_lag[trade_dates])
# Calculate unrealized pnl for days with trade
pnl_s[trade_dates] <- pnl_s[trade_dates] +
  position_s[trade_dates]*
  (cl_ose[trade_dates] - op_en[trade_dates])
cum_pnls_open <- star_t + cumsum(pnl_s)
# Annualized Sharpe ratio of VWAP strategy
sqrt(252)*sum(pnl_s)/sd(pnl_s)/NROW(pnl_s)
# Annualized Sharpe ratio of VTI
sqrt(252)*sum(re_turns)/sd(re_turns)/NROW(pnl_s)

# Plot both strategies
da_ta <- cbind(cum_pnls_open, cum_pnls)
colnames(da_ta) <- c("strategy_open", "strategy")
dygraphs::dygraph(da_ta, main=paste(sym_bol, "VWAP Strategy on Open")) %>%
  dySeries(name="strategy_open", label="Strategy on Open", strokeWidth=2, col="green") %>%
  dySeries(name="strategy", label="Strategy", strokeWidth=2, col="blue")
# Plot VTI and VWAP strategy
chart_Series(x=cl_ose, name="VWAP Crossover Strategy for VTI Trade at Open Price", col="orange")
add_TA(cum_pnls, on=1, lwd=2, col="blue")
add_TA(position_s > 0, on=-1,
 col="lightgreen", border="lightgreen")
add_TA(position_s < 0, on=-1,
 col="lightgrey", border="lightgrey")
legend("top", legend=c(sym_bol, "VWAP strategy"),
 inset=0.1, bg="white", lty=1, lwd=6,
 col=c("orange", "blue"), bty="n")

# Define length for weights and decay parameter
wid_th <- 352
lamb_da <- 0.01
# Calculate EWMA prices
weight_s <- exp(-lamb_da*1:wid_th)
weight_s <- weight_s/sum(weight_s)
ew_ma <- stats::filter(cl_ose, filter=weight_s,
  sides=1, method="convolution")
ew_ma[1:(wid_th-1)] <- ew_ma[wid_th]
ew_ma <- cbind(cl_ose, as.numeric(ew_ma))
colnames(ew_ma) <- c("VTI", "VTI EWMA")

# Plot EWMA prices with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue")
chart_Series(ew_ma["2007/2010"], theme=plot_theme,
       name="EWMA prices")
legend("bottomleft", legend=colnames(ew_ma),
 inset=0.1, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")

# Determine dates right after VWAP has crossed prices
indica_tor <- sign(cl_ose - ew_ma[, 2])
trade_dates <- (rutils::diff_it(indica_tor) != 0)
trade_dates <- which(trade_dates) + 1
# Calculate positions, either: -1, 0, or 1
position_s <- rep(NA_integer_, NROW(oh_lc))
position_s[1] <- 0
position_s[trade_dates] <- indica_tor[trade_dates-1]
position_s <- zoo::na.locf(position_s, na.rm=FALSE)
position_s <- xts(position_s, order.by=index(oh_lc))

# Plot EWMA prices with position shading
chart_Series(ew_ma["2007/2010"], theme=plot_theme,
       name="EWMA prices")
add_TA(position_s > 0, on=-1,
 col="lightgreen", border="lightgreen")
add_TA(position_s < 0, on=-1,
 col="lightgrey", border="lightgrey")
legend("bottomleft", legend=colnames(ew_ma),
 inset=0.1, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")

# Calculate daily profits and losses
# Calculate pnl for days without trade
pnl_s <- rutils::diff_it(cl_ose)*position_s
# Calculate realized pnl for days with trade
close_lag <- rutils::lag_it(cl_ose)
pos_lagged <- rutils::lag_it(position_s)
pnl_s[trade_dates] <- pos_lagged[trade_dates]*
  (op_en[trade_dates] - close_lag[trade_dates])
# Calculate unrealized pnl for days with trade
pnl_s[trade_dates] <- pnl_s[trade_dates] +
  position_s[trade_dates]*
  (cl_ose[trade_dates] - op_en[trade_dates])
# Annualized Sharpe ratio of EWMA strategy
sqrt(252)*sum(pnl_s)/sd(pnl_s)/NROW(pnl_s)
# Cumulative pnls
cum_pnls <- star_t + cumsum(pnl_s)
cum_pnls <- cbind(cl_ose, cum_pnls)
colnames(cum_pnls) <- c("VTI", "EWMA PnL")

# Plot EWMA PnL with position shading
chart_Series(cum_pnls, theme=plot_theme,
       name="Performance of EWMA Strategy")
add_TA(position_s > 0, on=-1,
 col="lightgreen", border="lightgreen")
add_TA(position_s < 0, on=-1,
 col="lightgrey", border="lightgrey")
legend("top", legend=colnames(cum_pnls),
 inset=0.05, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")

# bid_offer equal to 10 bps for liquid ETFs
bid_offer <- 0.001
# Calculate transaction costs
cost_s <- 0.5*bid_offer*abs(pos_lagged - position_s)*cl_ose
# pnl_s <- (pnl_s - cost_s)
# Plot strategy with transaction costs
cum_pnls <- star_t + cumsum(pnl_s)
cum_pnls <- cbind(cum_pnls, cum_pnls - cumsum(cost_s))
colnames(cum_pnls) <- c(sym_bol, "costs")
dygraphs::dygraph(cum_pnls, main=paste(sym_bol, "EWMA Strategy With Transaction Costs")) %>%
  dySeries(name="costs", label="Strategy With Transaction Costs", strokeWidth=2, col="green") %>%
  dySeries(name=sym_bol, label="EWMA Strategy", strokeWidth=2, col="blue")

simu_ewma <- function(oh_lc, lamb_da=0.01, wid_th=251, bid_offer=0.001, tre_nd=1) {
  # Calculate EWMA prices
  weight_s <- exp(-lamb_da*1:wid_th)
  weight_s <- weight_s/sum(weight_s)
  cl_ose <- quantmod::Cl(oh_lc)
  ew_ma <- stats::filter(cl_ose, filter=weight_s, sides=1, method="convolution")
  ew_ma[1:(wid_th-1)] <- ew_ma[wid_th]
  # Determine dates right after EWMA has crossed prices
  indica_tor <- tre_nd*sign(cl_ose - as.numeric(ew_ma))
  trade_dates <- (rutils::diff_it(indica_tor) != 0)
  trade_dates <- which(trade_dates) + 1
  trade_dates <- trade_dates[trade_dates < NROW(oh_lc)]
  # Calculate positions, either: -1, 0, or 1
  position_s <- rep(NA_integer_, NROW(oh_lc))
  position_s[1] <- 0
  position_s[trade_dates] <- indica_tor[trade_dates-1]
  position_s <- zoo::na.locf(position_s, na.rm=FALSE)
  op_en <- quantmod::Op(oh_lc)
  close_lag <- rutils::lag_it(cl_ose)
  pos_lagged <- rutils::lag_it(position_s)
  # Calculate daily profits and losses
  pnl_s <- rutils::diff_it(cl_ose)*position_s
  pnl_s[trade_dates] <- pos_lagged[trade_dates]*
    (op_en[trade_dates] - close_lag[trade_dates])
  pnl_s[trade_dates] <- pnl_s[trade_dates] +
    position_s[trade_dates]*
    (cl_ose[trade_dates] - op_en[trade_dates])
  # Calculate transaction costs
  cost_s <- 0.5*bid_offer*abs(pos_lagged - position_s)*cl_ose
  pnl_s <- (pnl_s - cost_s)
  # Calculate strategy returns
  pnl_s <- cbind(position_s, pnl_s)
  colnames(pnl_s) <- c("positions", "pnls")
  pnl_s
}  # end simu_ewma

source("C:/Develop/lecture_slides/scripts/ewma_model.R")
lamb_das <- seq(from=1e-5, to=0.05, by=0.01)
# Perform lapply() loop over lamb_das
pnl_s <- lapply(lamb_das, function(lamb_da) {
  # Simulate EWMA strategy and calculate re_turns
  star_t + cumsum(simu_ewma(oh_lc=oh_lc,
    lamb_da=lamb_da, wid_th=wid_th)[, "pnls"])
})  # end lapply
pnl_s <- do.call(cbind, pnl_s)
colnames(pnl_s) <- paste0("lambda=", lamb_das)

# Plot EWMA strategies with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(pnl_s))
chart_Series(pnl_s, theme=plot_theme,
  name="Cumulative Returns of EWMA Strategies")
legend("topleft", legend=colnames(pnl_s), inset=0.1,
  bg="white", cex=0.8, lwd=rep(6, NCOL(pnl_s)),
  col=plot_theme$col$line.col, bty="n")

# Initialize compute cluster under Windows
library(parallel)
clus_ter <- makeCluster(detectCores()-1)
clusterExport(clus_ter,
  varlist=c("oh_lc", "wid_th", "simu_ewma"))
# Perform parallel loop over lamb_das under Windows
pnl_s <- parLapply(clus_ter, lamb_das, function(lamb_da) {
  library(quantmod)
  # Simulate EWMA strategy and calculate re_turns
  star_t + cumsum(simu_ewma(oh_lc=oh_lc,
    lamb_da=lamb_da, wid_th=wid_th)[, "pnls"])
})  # end parLapply
# Perform parallel loop over lamb_das under Mac-OSX or Linux
re_turns <- mclapply(lamb_das, function(lamb_da) {
  library(quantmod)
  # Simulate EWMA strategy and calculate re_turns
  star_t + cumsum(simu_ewma(oh_lc=oh_lc,
    lamb_da=lamb_da, wid_th=wid_th)[, "pnls"])
})  # end mclapply
stopCluster(clus_ter)  # Stop R processes over cluster under Windows
pnl_s <- do.call(cbind, pnl_s)
colnames(pnl_s) <- paste0("lambda=", lamb_das)

sharpe_ratios <- sqrt(252)*sapply(pnl_s, function(x_ts) {
  # Calculate annualized Sharpe ratio of strategy returns
  x_ts <- rutils::diff_it(x_ts)
  sum(x_ts)/sd(x_ts)
})/NROW(pnl_s)  # end sapply
plot(x=lamb_das, y=sharpe_ratios, t="l",
     main="Performance of EWMA trend following strategies
     as function of the decay parameter lambda")
trend_returns <- rutils::diff_it(pnl_s)
trend_sharpe <- sharpe_ratios

# Simulate best performing strategy
ewma_trend <- simu_ewma(oh_lc=oh_lc,
  lamb_da=lamb_das[which.max(sharpe_ratios)],
  wid_th=wid_th)
position_s <- ewma_trend[, "positions"]
pnl_s <- star_t + cumsum(ewma_trend[, "pnls"])
pnl_s <- cbind(cl_ose, pnl_s)
colnames(pnl_s) <- c("VTI", "EWMA PnL")
# Plot EWMA PnL with position shading
plot_theme$col$line.col <- c("orange", "blue")
chart_Series(pnl_s, theme=plot_theme,
       name="Performance of Trend Following EWMA Strategy")
add_TA(position_s > 0, on=-1,
 col="lightgreen", border="lightgreen")
add_TA(position_s < 0, on=-1,
 col="lightgrey", border="lightgrey")
legend("top", legend=colnames(pnl_s),
  inset=0.05, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")

source("C:/Develop/lecture_slides/scripts/ewma_model.R")
lamb_das <- seq(0.05, 1.0, 0.05)
# Perform lapply() loop over lamb_das
pnl_s <- lapply(lamb_das, function(lamb_da) {
  # Backtest EWMA strategy and calculate re_turns
  star_t + cumsum(simu_ewma(
    oh_lc=oh_lc, lamb_da=lamb_da, wid_th=wid_th, tre_nd=(-1))[, "pnls"])
})  # end lapply
pnl_s <- do.call(cbind, pnl_s)
colnames(pnl_s) <- paste0("lambda=", lamb_das)
# Plot EWMA strategies with custom line colors
column_s <- seq(1, NCOL(pnl_s), by=4)
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NROW(column_s))
chart_Series(pnl_s[, column_s],
  theme=plot_theme, name="Cumulative Returns of Mean Reverting EWMA Strategies")
legend("topleft", legend=colnames(pnl_s[, column_s]),
  inset=0.1, bg="white", cex=0.8, lwd=6,
  col=plot_theme$col$line.col, bty="n")

sharpe_ratios <- sqrt(252)*sapply(pnl_s, function(x_ts) {
  # Calculate annualized Sharpe ratio of strategy returns
  x_ts <- rutils::diff_it(x_ts)
  sum(x_ts)/sd(x_ts)
})/NROW(pnl_s)  # end sapply
plot(x=lamb_das, y=sharpe_ratios, t="l",
     main="Performance of EWMA mean reverting strategies
     as function of the decay parameter lambda")
revert_returns <- rutils::diff_it(pnl_s)
revert_sharpe <- sharpe_ratios

# Backtest best performing strategy
ewma_revert <- simu_ewma(oh_lc=oh_lc, bid_offer=0.0,
  lamb_da=lamb_das[which.max(sharpe_ratios)],
  wid_th=wid_th, tre_nd=(-1))
position_s <- ewma_revert[, "positions"]
pnl_s <- star_t + cumsum(ewma_revert[, "pnls"])
pnl_s <- cbind(cl_ose, pnl_s)
colnames(pnl_s) <- c("VTI", "EWMA PnL")

# Plot EWMA PnL with position shading
plot_theme$col$line.col <- c("orange", "blue")
chart_Series(pnl_s, theme=plot_theme,
       name="Performance of Mean Reverting EWMA Strategy")
add_TA(position_s > 0, on=-1,
 col="lightgreen", border="lightgreen")
add_TA(position_s < 0, on=-1,
 col="lightgrey", border="lightgrey")
legend("top", legend=colnames(pnl_s),
  inset=0.05, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")

# Calculate correlation between trend following and mean reverting strategies
trend_ing <- ewma_trend[, "pnls"]
colnames(trend_ing) <- "trend"
revert_ing <- ewma_revert[, "pnls"]
colnames(revert_ing) <- "revert"
close_rets <- rutils::diff_it(cl_ose)
cor(cbind(trend_ing, revert_ing, close_rets))
# Calculate combined strategy
com_bined <- trend_ing + revert_ing
colnames(com_bined) <- "combined"
# Calculate annualized Sharpe ratio of strategy returns
re_turns <- cbind(close_rets, trend_ing, revert_ing, com_bined)
sqrt(252)*sapply(re_turns, function(x_ts)
  sum(x_ts)/sd(x_ts))/NROW(com_bined)
pnl_s <- lapply(re_turns, function(x_ts) {star_t + cumsum(x_ts)})
pnl_s <- do.call(cbind, pnl_s)
colnames(pnl_s) <- c("VTI", "trending", "reverting", "EWMA combined PnL")

plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue", "green", "magenta2")
chart_Series(pnl_s, theme=plot_theme,
       name="Performance of Combined EWMA Strategies")
legend("topleft", legend=colnames(pnl_s),
 inset=0.05, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")

weight_s <- c(trend_sharpe, revert_sharpe)
weight_s[weight_s<0] <- 0
weight_s <- weight_s/sum(weight_s)
re_turns <- cbind(trend_returns, revert_returns)
avg_returns <- re_turns %*% weight_s
avg_returns <- xts(avg_returns, order.by=index(re_turns))
pnl_s <- (star_t + cumsum(avg_returns))
pnl_s <- cbind(cl_ose, pnl_s)
colnames(pnl_s) <- c("VTI", "EWMA PnL")
# Plot EWMA PnL without position shading
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue")
chart_Series(pnl_s, theme=plot_theme,
  name="Performance of Ensemble EWMA Strategy")
legend("top", legend=colnames(pnl_s),
  inset=0.05, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")

# Extract ETF returns
sym_bols <- c("VTI", "IEF", "DBC")
re_turns <- rutils::etf_env$re_turns[, sym_bols]
# Select rows with IEF data
re_turns <- re_turns[index(rutils::etf_env$IEF)]
# Copy over NA values
re_turns[1, is.na(re_turns[1, ])] <- 0
re_turns <- zoo::na.locf(re_turns, na.rm=FALSE)

# Calculate backtest returns
pnl_s <- rowSums(weight_s*fu_ture)
pnl_s <- xts(pnl_s, order.by=in_dex)
colnames(pnl_s) <- "ewma momentum"
close_rets <- rutils::diff_it(cl_ose[in_dex])
cor(cbind(pnl_s, close_rets))
pnl_s <- star_t + cumsum(pnl_s)

# Plot the backtest
chart_Series(x=cl_ose[end_points[-n_rows]],
  name="backtest of EWMA strategies", col="orange")
add_TA(pnl_s, on=1, lwd=2, col="blue")
legend("top", legend=c("VTI", "EWMA"),
 inset=0.1, bg="white", lty=1, lwd=6,
 col=c("orange", "blue"), bty="n")
# shad_e <- xts(index(pnl_s) < as.Date("2008-01-31"), order.by=index(pnl_s))
# add_TA(shad_e, on=-1, col="lightgrey", border="lightgrey")
# text(x=7, y=0, labels="warmup period")

# Define end of month end_points
end_points <- rutils::calc_endpoints(re_turns,
          inter_val="months")
n_rows <- NROW(end_points)
# Start_points equal end_points lagged by 12-month look-back interval
look_back <- 12
start_points <- c(rep_len(1, look_back-1),
  end_points[1:(n_rows-look_back+1)])
# Calculate look-back intervals
look_backs <- cbind(start_points, end_points)
# Calculate look-forward intervals
look_fwds <- cbind(end_points + 1,
  rutils::lag_it(end_points, -1))
look_fwds[n_rows, 1] <- end_points[n_rows]
# Inspect the intervals
tail(cbind(look_backs, look_fwds))

# Define performance function as Sharpe ratio
perform_ance <- function(re_turns)
  sum(re_turns)/sd(re_turns)
# Calculate past performance over look-back intervals
pas_t <- apply(look_backs, 1, function(ep) {
  sapply(re_turns[ep[1]:ep[2]], perform_ance)
})  # end sapply
pas_t <- t(pas_t)
pas_t[is.na(pas_t)] <- 0
# Calculate future performance
fu_ture <- apply(look_fwds, 1, function(ep) {
  sapply(re_turns[ep[1]:ep[2]], sum)
})  # end sapply
fu_ture <- t(fu_ture)
fu_ture[is.na(fu_ture)] <- 0
# Weights proportional to past performance
weight_s <- pas_t
# weight_s[weight_s < 0] <- 0
# Scale weight_s so sum is equal to 1
# weight_s <- weight_s/rowSums(weight_s)
# Scale weight_s so sum of squares is equal to 1
weight_s <- weight_s/sqrt(rowSums(weight_s^2))
# Set NA values to zero
weight_s[is.na(weight_s)] <- 0
sum(is.na(weight_s))

# Calculate momentum profits and losses (returns)
pnl_s <- rowSums(weight_s*fu_ture)
# Lag the momentum returns and weights
# to correspond with end of future interval
pnl_s <- rutils::lag_it(pnl_s)
weight_s <- rutils::lag_it(weight_s)
# bid_offer equal to 10 bps for liquid ETFs
bid_offer <- 0.001
# Calculate transaction costs
weal_th <- cumprod(1 + pnl_s)
cost_s <- 0.5*bid_offer*weal_th*
  rowSums(abs(rutils::diff_it(weight_s)))
weal_th <- cumprod(1 + pnl_s - cost_s)
in_dex <- index(re_turns[end_points])
weal_th <- xts::xts(weal_th, in_dex)

# Define all-weather benchmark
weights_aw <- c(0.30, 0.55, 0.15)
ret_aw <- re_turns %*% weights_aw
wealth_aw <- cumprod(1 + ret_aw)
wealth_aw <- xts::xts(wealth_aw[end_points], in_dex)
# Plot the Momentum strategy and benchmark
da_ta <- cbind(weal_th, wealth_aw)
colnames(da_ta) <- c("Momentum Strategy", "Benchmark")
dygraphs::dygraph(da_ta, main="Momentum Strategy") %>%
  dyAxis("y", label="Benchmark", independentTicks=TRUE) %>%
  dyAxis("y2", label="Momentum Strategy", independentTicks=TRUE) %>%
  dySeries(name="Momentum Strategy", axis="y2", label="Momentum Strategy", strokeWidth=2, col="red") %>%
  dySeries(name="Benchmark", axis="y", label="Benchmark", strokeWidth=2, col="blue")

# Define backtest functional
backtest_momentum <- function(re_turns,
                perform_ance=function(re_turns) (sum(re_turns)/sd(re_turns)),
                look_back=12, re_balance="months", bid_offer=0.001,
                end_points=rutils::calc_endpoints(re_turns, inter_val=re_balance),
                with_weights=FALSE, ...) {
  stopifnot("package:rutils" %in% search() || require("rutils", quietly=TRUE))
  # Define look-back and look-forward intervals
  n_rows <- NROW(end_points)
  start_points <- c(rep_len(1, look_back-1), end_points[1:(n_rows-look_back+1)])
  # Calculate look-back intervals
  look_backs <- cbind(start_points, end_points)
  # Calculate look-forward intervals
  look_fwds <- cbind(end_points + 1, rutils::lag_it(end_points, -1))
  look_fwds[n_rows, 1] <- end_points[n_rows]
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

source("C:/Develop/lecture_slides/scripts/back_test.R")
look_backs <- seq(3, 15, by=1)
perform_ance <- function(re_turns) sum(re_turns)/sd(re_turns)
pro_files <- sapply(look_backs, function(look_back) {
  pnl_s <- backtest_momentum(re_turns=re_turns, end_points=end_points,
    look_back=look_back, perform_ance=perform_ance)
  last(cumprod(1 + pnl_s))
})  # end sapply
x11(width=6, height=4)
plot(x=look_backs, y=pro_files, t="l",
  main="Strategy PnL as function of look_back",
  xlab="look_back (months)", ylab="pnl")

look_back <- look_backs[which.max(pro_files)]
pnl_s <- backtest_momentum(re_turns=re_turns,
  look_back=look_back, end_points=end_points,
  perform_ance=perform_ance, with_weights=TRUE)
tail(pnl_s)
ret_mom <- as.numeric(pnl_s[, 1])
weal_th <- cumprod(1 + ret_mom)
da_ta <- cbind(weal_th, wealth_aw)
colnames(da_ta) <- c("Momentum Strategy", "All_weather")

# Plot the Momentum strategy and benchmark
dygraphs::dygraph(da_ta, main="Momentum Strategy") %>%
  dyAxis("y", label="All_weather", independentTicks=TRUE) %>%
  dyAxis("y2", label="Momentum Strategy", independentTicks=TRUE) %>%
  dySeries(name="Momentum Strategy", axis="y2", label="Momentum Strategy", strokeWidth=2, col="red") %>%
  dySeries(name="All_weather", axis="y", label="All_weather", strokeWidth=2, col="blue")
# Or
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue")
chart_Series(da_ta, theme=plot_theme, lwd=2,
       name="Momentum PnL")
legend("topleft", legend=colnames(da_ta),
  inset=0.1, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")

# Plot the momentum portfolio weights
weight_s <- pnl_s[, -1]
vt_i <- rutils::etf_env$price_s$VTI[in_dex]
da_ta <- cbind(vt_i, weight_s)
da_ta <- na.omit(da_ta)
colnames(da_ta)[2:NCOL(pnl_s)] <- paste0(colnames(weight_s), "_weight")
zoo::plot.zoo(da_ta, xlab=NULL, main="Momentum Weights")

# Calculate ETF betas
betas_etf <- sapply(re_turns, function(x)
  cov(re_turns$VTI, x)/var(x))
# Betas equal weights times ETF betas
beta_s <- weight_s %*% betas_etf
beta_s <- xts(beta_s, order.by=in_dex)
colnames(beta_s) <- "momentum_beta"
da_ta <- cbind(beta_s, rutils::etf_env$VTI[in_dex, 4])
zoo::plot.zoo(da_ta,
  oma = c(3, 1, 3, 0), mar = c(0, 4, 0, 1),
  main="betas & VTI", xlab="")

# Open x11 for plotting and set parameters to reduce whitespace around plot
x11(width=6, height=4)
par(mar=c(4, 4, 3, 1), oma=c(0, 0, 0, 0))
# Merton-Henriksson test
vt_i <- rutils::diff_it(vt_i)/rutils::lag_it(vt_i)
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
text(x=0.05, y=0.15, paste("Treynor test t-value =", round(summary(mod_el)$coefficients["treynor", "t value"], 2)))

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
ret_aw <- rutils::diff_it(wealth_aw)/rutils::lag_it(wealth_aw)
ret_aw <- sd(ret_mom)*ret_aw/sd(ret_aw)
da_ta <- cbind(ret_mom, ret_aw, 0.5*(ret_mom + ret_aw))
colnames(da_ta) <- c("momentum", "all_weather", "combined")
# Calculate strategy annualized Sharpe ratios
apply(da_ta, MARGIN=2, function(x) {
  sqrt(12)*sum(x)/sd(x)/NROW(x)
})  # end apply
# Calculate strategy correlations
cor(da_ta)
# Calculate cumulative wealth
weal_th <- apply(da_ta, MARGIN=2,
  function(x) {cumprod(1 + x)}
)  # end apply
weal_th <- xts::xts(weal_th, in_dex)

# Plot ETF momentum strategy combined with All-Weather
dygraphs::dygraph(weal_th, main="ETF Momentum Strategy Combined with All-Weather") %>%
  dyOptions(colors=c("green", "blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always")
# Or
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("green", "blue", "red")
chart_Series(da_ta, theme=plot_theme,
       name="ETF Momentum Strategy Combined with All-Weather")
legend("topleft", legend=colnames(da_ta),
  inset=0.1, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")

# Calculate rolling variance
look_back <- 252
vari_ance <- roll::roll_var(re_turns, width=look_back)
vari_ance <- zoo::na.locf(vari_ance, na.rm=FALSE)
vari_ance[is.na(vari_ance)] <- 0
# Calculate rolling Sharpe
pas_t <- roll::roll_mean(re_turns, width=look_back)
weight_s <- pas_t/sqrt(vari_ance)
weight_s[vari_ance == 0] <- 0
weight_s[1:look_back, ] <- 1
weight_s <- weight_s/sqrt(rowSums(weight_s^2))
weight_s[is.na(weight_s)] <- 0
weight_s <- rutils::lag_it(weight_s)
sum(is.na(weight_s))
# Calculate momentum profits and losses
pnl_s <- rowMeans(weight_s*re_turns)

# Calculate transaction costs
bid_offer <- 0.001
cost_s <- 0.5*bid_offer*rowSums(abs(rutils::diff_it(weight_s)))
weal_th <- cumprod(1 + pnl_s - cost_s)
weal_th <- xts(weal_th, order.by=index(re_turns))
# Plot momentum and VTI
wealth_aw <- cumprod(1 + re_turns %*% weights_aw)
da_ta <- cbind(wealth_aw, weal_th)
colnames(da_ta) <- c("all_weather", "momentum")
col_names <- colnames(da_ta)
dygraphs::dygraph(da_ta, main="Momentum vs All-Weather") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="blue") %>%
  dySeries(name=col_names[2], axis="y2", col="red")

# Define backtest functional for daily momentum strategy
# If tre_nd=(-1) then it backtests a mean reverting strategy
momentum_daily <- function(re_turns, look_back=252, bid_offer=0.001, tre_nd=1, ...) {
  stopifnot("package:quantmod" %in% search() || require("quantmod", quietly=TRUE))
  # Calculate rolling variance
  vari_ance <- roll::roll_var(re_turns, width=look_back)
  vari_ance <- zoo::na.locf(vari_ance, na.rm=FALSE)
  vari_ance[is.na(vari_ance)] <- 0
  # Calculate rolling Sharpe
  pas_t <- roll::roll_mean(re_turns, width=look_back)
  weight_s <- pas_t/sqrt(vari_ance)
  weight_s[vari_ance == 0] <- 0
  weight_s[1:look_back, ] <- 1
  weight_s <- weight_s/sqrt(rowSums(weight_s^2))
  weight_s[is.na(weight_s)] <- 0
  weight_s <- rutils::lag_it(weight_s)
  # Calculate momentum profits and losses
  pnl_s <- tre_nd*rowMeans(weight_s*re_turns)
  # Calculate transaction costs
  cost_s <- 0.5*bid_offer*rowSums(abs(rutils::diff_it(weight_s)))
  cumprod(1 + pnl_s - cost_s)
}  # end momentum_daily

# Backtest a daily ETF momentum strategy
source("C:/Develop/lecture_slides/scripts/back_test.R")
weal_th <- momentum_daily(look_back=252,
  re_turns=re_turns, bid_offer=bid_offer)
# Perform sapply loop over look_backs
look_backs <- seq(50, 300, by=50)
weal_th <- sapply(look_backs, momentum_daily,
  re_turns=re_turns, bid_offer=bid_offer)
colnames(weal_th) <- paste0("look_back=", look_backs)
weal_th <- xts(weal_th, index(re_turns))
tail(weal_th)

# Plot EWMA strategies with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(weal_th))
chart_Series(weal_th,
  theme=plot_theme, name="Cumulative Returns of Daily ETF Momentum Strategies")
legend("bottomleft", legend=colnames(weal_th),
  inset=0.02, bg="white", cex=0.7, lwd=rep(6, NCOL(re_turns)),
  col=plot_theme$col$line.col, bty="n")

# Backtest a daily S&P500 momentum strategy
source("C:/Develop/lecture_slides/scripts/back_test.R")
load("C:/Develop/lecture_slides/data/sp500_prices.RData")
# Perform sapply loop over look_backs
look_backs <- seq(50, 300, by=50)
weal_th <- sapply(look_backs, momentum_daily,
  re_turns=returns_100, bid_offer=0)
colnames(weal_th) <- paste0("look_back=", look_backs)
weal_th <- xts(weal_th, index(re_turns))

# Plot daily S&P500 momentum strategies with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(weal_th))
chart_Series(weal_th,
  theme=plot_theme, name="Cumulative Returns of S&P500 Momentum Strategies")
legend("bottomleft", legend=colnames(weal_th),
  inset=0.02, bg="white", cex=0.7, lwd=rep(6, NCOL(re_turns)),
  col=plot_theme$col$line.col, bty="n")

# Perform sapply loop over look_backs
look_backs <- seq(5, 50, by=5)
weal_th <- sapply(look_backs, momentum_daily,
  re_turns=returns_100, bid_offer=0, tre_nd=(-1))
colnames(weal_th) <- paste0("look_back=", look_backs)
weal_th <- xts(weal_th, index(price_s))

# Plot EWMA strategies with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(weal_th))
chart_Series(weal_th,
  theme=plot_theme, name="Cumulative Returns of S&P500 Mean Reverting Strategies")
legend("topleft", legend=colnames(weal_th),
  inset=0.05, bg="white", cex=0.7, lwd=rep(6, NCOL(re_turns)),
  col=plot_theme$col$line.col, bty="n")

# sym_bols contains all the symbols in rutils::etf_env$re_turns except "VXX" and "SVXY"
sym_bols <- c("DBC", "IEF", "VTI", "XLE", "XLF", "XLI", "XLK", "XLP", "XLU", "XLV", "XLY")
# sym_bols <- colnames(rutils::etf_env$re_turns)
# sym_bols <- sym_bols[!(sym_bols %in% c("VXX", "SVXY", "MTUM"))]
# Extract columns of rutils::etf_env$re_turns and remove NA values
re_turns <- rutils::etf_env$re_turns[, sym_bols]
re_turns <- na.omit(re_turns)
# re_turns[1, is.na(re_turns[1, ])] <- 0
# re_turns <- zoo::na.locf(re_turns, na.rm=FALSE)
# Calculate vector of monthly end points and start points
look_back <- 12
end_points <- rutils::calc_endpoints(re_turns, inter_val="months")
end_points[end_points < 2*NCOL(re_turns)] <- 2*NCOL(re_turns)

n_rows <- NROW(end_points)
# Sliding window
start_points <- c(rep_len(1, look_back-1), end_points[1:(n_rows-look_back+1)])
# OR expanding window
# Start_points <- rep_len(1, n_rows)
# risk_free is the daily risk-free rate
risk_free <- 0.03/252
# Calculate daily excess returns
ex_cess <- re_turns - risk_free
# Perform loop over end_points
portf_rets <- lapply(2:n_rows,
  function(i) {
    # Subset the ex_cess returns
    ex_cess <- ex_cess[start_points[i-1]:end_points[i-1], ]
    in_verse <- solve(cov(ex_cess))
    # Calculate the maximum Sharpe ratio portfolio weights
    weight_s <- in_verse %*% colMeans(ex_cess)
    weight_s <- drop(weight_s/sqrt(sum(weight_s^2)))
    # Subset the re_turns
    re_turns <- re_turns[(end_points[i-1]+1):end_points[i], ]
    # Calculate the out-of-sample portfolio returns
    xts(re_turns %*% weight_s, index(re_turns))
  }  # end anonymous function
)  # end lapply
portf_rets <- rutils::do_call(rbind, portf_rets)
colnames(portf_rets) <- "portf_rets"
# Calculate compounded cumulative portfolio wealth
weal_th <- cumprod(1 + portf_rets)
quantmod::chart_Series(weal_th,
  name="Cumulative Returns of Max Sharpe Portfolio Strategy")

# sym_bols contains all the symbols in rutils::etf_env$re_turns except "VXX" and "SVXY"
sym_bols <- c("DBC", "IEF", "VTI", "XLE", "XLF", "XLI", "XLK", "XLP", "XLU", "XLV", "XLY")
# sym_bols <- colnames(rutils::etf_env$re_turns)
# sym_bols <- sym_bols[!(sym_bols %in% c("VXX", "SVXY", "MTUM"))]
# Extract columns of rutils::etf_env$re_turns and remove NA values
re_turns <- rutils::etf_env$re_turns[, sym_bols]
re_turns <- na.omit(re_turns)
# re_turns[1, is.na(re_turns[1, ])] <- 0
# re_turns <- zoo::na.locf(re_turns, na.rm=FALSE)
# Calculate vector of monthly end points and start points
look_back <- 12
end_points <- rutils::calc_endpoints(re_turns, inter_val="months")
# Lag the initial end_points in warmpup period
end_points[end_points < 2*NCOL(re_turns)] <- 2*NCOL(re_turns)

n_rows <- NROW(end_points)
# Sliding window
start_points <- c(rep_len(1, look_back-1), end_points[1:(n_rows-look_back+1)])
# OR expanding window
# Start_points <- rep_len(1, n_rows)
# risk_free is the daily risk-free rate
risk_free <- 0.03/252
# Calculate daily excess returns
ex_cess <- re_turns - risk_free
# Perform loop over end_points
portf_rets <- lapply(2:n_rows,
  function(ep) {
    # Subset the ex_cess returns
    ex_cess <- ex_cess[start_points[ep-1]:end_points[ep-1], ]
    in_verse <- solve(cov(ex_cess))
    # Calculate the maximum Sharpe ratio portfolio weights
    weight_s <- in_verse %*% colMeans(ex_cess)
    weight_s <- drop(weight_s/sqrt(sum(weight_s^2)))
    # Subset the re_turns to out-of-sample returns
    re_turns <- re_turns[(end_points[ep-1]+1):end_points[ep], ]
    # Calculate the out-of-sample portfolio returns
    xts::xts(re_turns %*% weight_s, index(re_turns))
  }  # end anonymous function
)  # end lapply
portf_rets <- do.call(rbind, portf_rets)
colnames(portf_rets) <- "portf_rets"
# Calculate compounded cumulative portfolio wealth
weal_th <- cumprod(1 + portf_rets)
quantmod::chart_Series(weal_th,
  name="Cumulative Returns of Max Sharpe Portfolio Strategy")

load("C:/Develop/lecture_slides/data/sp500_prices.RData")
n_cols <- NCOL(re_turns) ; date_s <- index(re_turns)
# Calculate returns on equal weight portfolio
in_dex <- xts(cumsum(re_turns %*% rep(1/n_cols, n_cols)), index(re_turns))
# Define monthly end points
end_points <- rutils::calc_endpoints(re_turns, inter_val="months")
end_points <- end_points[end_points > (n_cols+1)]
n_rows <- NROW(end_points) ; look_back <- 12
start_points <- c(rep_len(1, look_back-1), end_points[1:(n_rows-look_back+1)])
# Perform backtest
al_pha <- 0.01 ; max_eigen <- 3
pnl_s <- HighFreq::back_test(ex_cess=re_turns,
                       re_turns=re_turns,
                       start_points=start_points-1,
                       end_points=end_points-1,
                       al_pha=al_pha,
                       max_eigen=max_eigen)

# Plot strategy in log scale
weal_th <- cumprod(1 + pnl_s)
weal_th <- cbind(weal_th, in_dex, (weal_th+in_dex)/2)
col_names <- c("Strategy", "Index", "Average")
colnames(weal_th) <- col_names
dygraphs::dygraph(weal_th[end_points], main="Rolling S&P500 Portfolio Optimization Strategy") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="red", strokeWidth=1) %>%
  dySeries(name=col_names[2], axis="y2", col="blue", strokeWidth=1) %>%
  dySeries(name=col_names[3], axis="y2", col="green", strokeWidth=2)

# library(rutils)  # Load package rutils
# Define time interval for end points
re_balance <- "weeks"
# Look-back interval is multiple of re_balance
look_back <- 41
# Create index of rebalancing period end points
end_points <- xts::endpoints(rutils::etf_env$re_turns,
                       on=re_balance)
# Start_points are multi-period lag of end_points
n_rows <- NROW(end_points)
start_points <- c(rep_len(1, look_back-1),
  end_points[1:(n_rows-look_back+1)])
# Create list of look-back intervals
look_backs <- lapply(2:n_rows,
    function(ep) {
start_points[ep]:end_points[ep]
  })  # end lapply

# library(rutils)  # Load package rutils
# Create vector of symbols for model
sym_bols <- c("VTI", "IEF", "DBC")

# Calculate risk&ret stats for some symbols, over a range of dates
# Perform lapply() loop over look_backs
risk_stats <- lapply(look_backs,
  function(look_back) {
    x_ts <-
rutils::etf_env$re_turns[look_back, sym_bols]
    t(sapply(x_ts,
function(col_umn)
  c(return=mean(col_umn), risk=mad(col_umn))
))  # end sapply
    })  # end lapply
# rbind list into single xts or matrix
# risk_stats <- do.call_rbind(risk_stats)
# head(risk_stats)
# Calculate non-overlapping returns in interval
re_turns <-sapply(2:n_rows,
    function(ep) {
    sapply(rutils::etf_env$re_turns[
(end_points[ep-1]+1):end_points[ep],
sym_bols], sum)
  })  # end sapply
re_turns <- t(re_turns)
