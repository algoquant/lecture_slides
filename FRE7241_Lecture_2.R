# Load S&P500 constituent stock prices
load("C:/Develop/lecture_slides/data/sp500.RData")
price_s <- eapply(sp500_env, quantmod::Cl)
price_s <- rutils::do_call(cbind, price_s)
# Carry forward and backward non-NA prices
price_s <- zoo::na.locf(price_s, na.rm=FALSE)
price_s <- zoo::na.locf(price_s, fromLast=TRUE)
colnames(price_s) <- unname(sapply(colnames(price_s),
  function(col_name) strsplit(col_name, split="[.]")[[1]][1]))
# Calculate percentage returns of the S&P500 constituent stocks
# re_turns <- rutils::diff_it(log(price_s))
re_turns <- lapply(price_s, function(x)
  rutils::diff_it(x)/rutils::lag_it(x, pad_zeros=FALSE))
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

# Select ETF symbols for asset allocation
sym_bols <- c("VTI", "VEU", "EEM",
  "XLY", "XLP", "XLE", "XLF", "XLV",
  "XLI", "XLB", "XLK", "XLU", "VYM", "IVW",
  "IWB", "IWD", "IWF", "IEF", "TLT", "VNQ",
  "DBC", "GLD", "USO", "VXX", "SVXY", "MTUM")
# Read etf database into data frame
etf_list <- read.csv(file="C:/Develop/lecture_slides/data/etf_list.csv", stringsAsFactors=FALSE)
rownames(etf_list) <- etf_list$Symbol
# Select from etf_list only those ETF's in sym_bols
etf_list <- etf_list[sym_bols, ]
# Shorten names
etf_names <- sapply(etf_list$Name, function(name) {
  name_split <- strsplit(name, split=" ")[[1]]
  name_split <- name_split[c(-1, -NROW(name_split))]
  name_match <- match("Select", name_split)
  if (!is.na(name_match))
    name_split <- name_split[-name_match]
  paste(name_split, collapse=" ")
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

library(rutils)
# Extract ETF prices from rutils::etf_env$price_s
price_s <- rutils::etf_env$price_s
price_s <- zoo::na.locf(price_s, na.rm=FALSE)
price_s <- zoo::na.locf(price_s, fromLast=TRUE)
# Calculate simple dollar returns
rets_dollar <- rutils::diff_it(price_s)
# Or
rets_dollar <- lapply(price_s, rutils::diff_it)
rets_dollar <- rutils::do_call(cbind, rets_dollar)
# Calculate log returns
log_rets <- rutils::diff_it(log(price_s))
# Calculate percentage returns
rets_percent <- rets_dollar/
  rutils::lag_it(price_s, lagg=1, pad_zeros=FALSE)

# Calculate prices from simple dollar returns
init_prices <- as.numeric(price_s[1, ])
new_prices <- cumsum(rets_dollar)
new_prices <- lapply(1:NCOL(new_prices), function (i)
    new_prices[, i] + init_prices[i])
new_prices <- rutils::do_call(cbind, new_prices)
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
dim(new_prices)
all.equal(new_prices, log(price_s))
# Plot log VTI prices
dygraphs::dygraph(log(quantmod::Cl(rutils::etf_env$VTI)),
  main="Logarithm of VTI Prices") %>%
  dyOptions(colors="blue", strokeWidth=2) %>%
  dyLegend(show="always")

# Calculate percentage VTI returns
price_s <- quantmod::Cl(rutils::etf_env$VTI)
re_turns <- rutils::diff_it(price_s)/
  rutils::lag_it(price_s, pad_zeros=FALSE)
# Funding rate per day
f_rate <- 0.01/252
# Margin account
mar_gin <- cumsum(re_turns)
# Calculate the cumulative funding costs
cost_s <- f_rate*mar_gin
# Add funding costs to margin account
mar_gin <- (mar_gin + cost_s)
# dygraph plot of margin and funding costs
da_ta <- cbind(mar_gin, cumsum(cost_s))
col_names <- c("Margin", "Cumulative Funding")
colnames(da_ta) <- col_names
dygraphs::dygraph(da_ta, main="VTI Margin Funding Costs") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="blue") %>%
  dySeries(name=col_names[2], axis="y2", col="red", strokeWidth=3)

# bid_offer equal to 10 bps for liquid ETFs
bid_offer <- 0.001
# Calculate the transaction costs
cost_s <- bid_offer*abs(re_turns)/2
# Subtract transaction costs from margin account
mar_gin <- cumsum(re_turns)
mar_gin <- (mar_gin - cost_s)
# dygraph plot of margin and transaction costs
da_ta <- cbind(mar_gin, cumsum(cost_s))
col_names <- c("Margin", "Cumulative Transaction Costs")
colnames(da_ta) <- col_names
dygraphs::dygraph(da_ta, main="VTI Transaction Costs") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="blue") %>%
  dySeries(name=col_names[2], axis="y2", col="red", strokeWidth=3)

# Calculate VTI and IEF prices
price_s <- rutils::etf_env$price_s[, c("VTI", "IEF")]
price_s <- na.omit(price_s)
# Wealth for fixed shares (no rebalancing)
weight_s <- c(0.5, 0.5)
wealth_fixed_shares <- price_s %*% weight_s
wealth_fixed_shares <- wealth_fixed_shares/wealth_fixed_shares[1, ]
# Calculate VTI and IEF percentage returns
re_turns <- lapply(price_s, function(x)
  rutils::diff_it(x)/rutils::lag_it(x, pad_zeros=FALSE))
re_turns <- do.call(cbind, re_turns)
# Wealth for fixed dollars (with rebalancing)
wealth_fixed_dollars <- cumsum(re_turns %*% weight_s)
# Plot log wealth
weal_th <- cbind(wealth_fixed_dollars, log(wealth_fixed_shares))
weal_th <- xts::xts(weal_th, index(price_s))
colnames(weal_th) <- c("Fixed dollars", "Fixed shares")
dygraphs::dygraph(weal_th, main="Log Wealth of Weighted Portfolios") %>%
  dyOptions(colors=c("red","blue"), strokeWidth=2) %>%
  dyLegend(show="always")

# Margin account for fixed dollars (with rebalancing)
mar_gin <- cumsum(re_turns %*% weight_s)
# Calculate the transaction costs
cost_s <- bid_offer*abs(re_turns %*% weight_s)/2
# Subtract transaction costs from margin account
mar_gin <- (mar_gin - cost_s)
# dygraph plot of margin and transaction costs
da_ta <- cbind(mar_gin, cumsum(cost_s))
da_ta <- xts::xts(da_ta, index(re_turns))
col_names <- c("Margin", "Cumulative Transaction Costs")
colnames(da_ta) <- col_names
dygraphs::dygraph(da_ta, main="Fixed Dollar Portfolio Transaction Costs") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="blue") %>%
  dySeries(name=col_names[2], axis="y2", col="red", strokeWidth=3)

# Calculate standardized simple dollar returns
rets_dollar <- lapply(rets_dollar, function(x) {
  x <- (x - mean(x))
  x/sd(x)})
rets_dollar <- do.call(cbind, rets_dollar)
sapply(rets_dollar, sd)
# Wealth for equal and fixed number of shares (no rebalancing)
re_turns <- rets_dollar[, c("VTI", "IEF")] %*% weight_s
weal_th <- cumsum(re_turns)
# Calculate standardized percentage returns
rets_percent <- lapply(rets_percent, function(x) {
  x <- (x - mean(x))
  x/sd(x)})
rets_percent <- do.call(cbind, rets_percent)
sapply(rets_percent, sd)
# Wealth for equal dollar amount of shares (with rebalancing)
re_turns <- (rets_percent[, c("VTI", "IEF")] %*% weight_s)
weal_th <- sum(weight_s)*cumsum(re_turns)
# Wealth for equal dollar amount of shares (without rebalancing)
wealth_nreb <- cumsum(rets_percent) %*% weight_s
# Plot compounded wealth
weal_th <- cbind(weal_th, wealth_nreb)
weal_th <- xts::xts(weal_th, index(rets_percent))
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
# Calculate cumulative wealth from returns
weal_th <- cumsum(re_turns)
# Plot cumulative wealth
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

# Calculate cumulative wealth from returns
weal_th <- cumsum(re_turns)
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

# VTI percentage returns
re_turns <- na.omit(rutils::etf_env$re_turns$VTI)
# Number of observations
n_rows <- NROW(re_turns)
# Mean of VTI returns
mean_rets <- mean(re_turns)
# Standard deviation of VTI returns
sd_rets <- sd(re_turns)
# Skewness of VTI returns
n_rows/((n_rows-1)*(n_rows-2))*
  sum(((re_turns - mean_rets)/sd_rets)^3)
# Kurtosis of VTI returns
n_rows*(n_rows+1)/((n_rows-1)^3)*
  sum(((re_turns - mean_rets)/sd_rets)^4)
# Random normal returns
re_turns <- rnorm(n_rows, sd=sd_rets)
# Mean and standard deviation of random normal returns
mean_rets <- mean(re_turns)
sd_rets <- sd(re_turns)
# Skewness of random normal returns
n_rows/((n_rows-1)*(n_rows-2))*
  sum(((re_turns - mean_rets)/sd_rets)^3)
# Kurtosis of random normal returns
n_rows*(n_rows+1)/((n_rows-1)^3)*
  sum(((re_turns - mean_rets)/sd_rets)^4)

# calc_skew() calculates skew of returns
calc_skew <- function(re_turns) {
  # Standardize re_turns
  re_turns <-
    (re_turns - mean(re_turns))/sd(re_turns)
  # Calculate skew
  n_rows <- NROW(re_turns)
  n_rows*sum(re_turns^3)/((n_rows-1)*(n_rows-2))
}  # end calc_skew
# calc_kurt() calculates kurtosis of returns
calc_kurt <- function(re_turns) {
  # Standardize re_turns
  re_turns <-
    (re_turns - mean(re_turns))/sd(re_turns)
  # Calculate skew
  n_rows <- NROW(re_turns)
  n_rows*(n_rows+1)*sum(re_turns^4)/((n_rows-1)*(n_rows-2)*(n_rows-3))
}  # end calc_kurt
# Calculate skew and kurtosis of VTI returns
calc_skew(re_turns)
calc_kurt(re_turns)

set.seed(1121)  # Reset random number generator
# Sample from Standard Normal Distribution
n_rows <- 1000
da_ta <- rnorm(n_rows)
# Sample mean
mean(da_ta)
# Sample standard deviation
sd(da_ta)
# Standard error of sample mean
sd(da_ta)/sqrt(n_rows)

x_var <- seq(-5, 7, length=100)
y_var <- dnorm(x_var, mean=1.0, sd=2.0)
plot(x_var, y_var, type="l", lty="solid",
     xlab="", ylab="")
title(main="Normal Density Function", line=0.5)
star_t <- 3; fin_ish <- 5  # Set lower and upper bounds
# Plot polygon area
are_a <- ((x_var >= star_t) & (x_var <= fin_ish))
polygon(c(star_t, x_var[are_a], fin_ish),
  c(-1, y_var[are_a], -1), col="red")

par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
sig_mas <- c(0.5, 1, 1.5, 2)  # Sigma values
# Create plot colors
col_ors <- c("red", "black", "blue", "green")
# Create legend labels
lab_els <- paste("sigma", sig_mas, sep="=")
for (in_dex in 1:4) {  # Plot four curves
curve(expr=dnorm(x, sd=sig_mas[in_dex]),
type="l", xlim=c(-4, 4),
xlab="", ylab="", lwd=2,
col=col_ors[in_dex],
add=as.logical(in_dex-1))
}  # end for
# Add title
title(main="Normal Distributions", line=0.5)
# Add legend
legend("topright", inset=0.05, title="Sigmas",
 lab_els, cex=0.8, lwd=2, lty=1, bty="n",
 col=col_ors)

x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
deg_free <- c(3, 6, 9)  # Df values
col_ors <- c("black", "red", "blue", "green")
lab_els <- c("normal", paste("df", deg_free, sep="="))
# Plot a Normal probability distribution
curve(expr=dnorm, xlim=c(-4, 4),
      xlab="", ylab="", lwd=2)
for (in_dex in 1:3) {  # Plot three t-distributions
curve(expr=dt(x, df=deg_free[in_dex]),
      xlab="", ylab="", lwd=2,
      col=col_ors[in_dex+1], add=TRUE)
}  # end for

# Add title
title(main="t-distributions", line=0.5)
# Add legend
legend("topright", inset=0.05, bty="n",
       title="Degrees\n of freedom", lab_els,
       cex=0.8, lwd=6, lty=1, col=col_ors)

# Mixture of two normal distributions with sd=1 and sd=2
n_rows <- 1e5
re_turns <- c(rnorm(n_rows/2), 2*rnorm(n_rows/2))
re_turns <- (re_turns-mean(re_turns))/sd(re_turns)
# Kurtosis of normal
calc_kurt(rnorm(n_rows))
# Kurtosis of mixture
calc_kurt(re_turns)
# Or
n_rows*sum(re_turns^4)/(n_rows-1)^2

x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
# Plot the distributions
plot(density(re_turns), xlab="", ylab="",
  main="Mixture of Normal Returns",
  xlim=c(-3, 3), type="l", lwd=3, col="red")
curve(expr=dnorm, lwd=2, col="blue", add=TRUE)
curve(expr=dt(x, df=3), lwd=2, col="green", add=TRUE)
# Add legend
legend("topright", inset=0.05, lty=1, lwd=6, bty="n",
  legend=c("Mixture", "Normal", "t-distribution"),
  col=c("red", "blue", "green"))

# Objective function is log-likelihood
likeli_hood <- function(pa_r, free_dom, da_ta) {
  sum(
    -log(gamma((free_dom+1)/2) /
      (sqrt(pi*free_dom) * gamma(free_dom/2))) +
    log(pa_r[2]) +
    (free_dom+1)/2 * log(1 + ((da_ta - pa_r[1])/
                    pa_r[2])^2/free_dom))
}  # end likeli_hood
# Demonstrate equivalence with log(dt())
likeli_hood(c(1, 0.5), 2, 2:5)
-sum(log(dt(x=(2:5-1)/0.5, df=2)/0.5))
# Simpler objective function
likeli_hood <- function(pa_r, free_dom, da_ta) {
  -sum(log(dt(x=(da_ta-pa_r[1])/pa_r[2],
      df=free_dom)/pa_r[2]))
}  # end likeli_hood

# VTI percentage returns
re_turns <- na.omit(rutils::etf_env$re_turns$VTI)
# Initial parameters
par_init <- c(mean=0, scale=0.01)
# Fit distribution using optim()
optim_fit <- optim(par=par_init,
  fn=likeli_hood, # Log-likelihood function
  da_ta=re_turns,
  free_dom=2, # Degrees of freedom
  method="L-BFGS-B", # quasi-Newton method
  upper=c(1, 0.1), # upper constraint
  lower=c(-1, 1e-7)) # Lower constraint
# optimal parameters
lo_cation <- optim_fit$par["mean"]
scal_e <- optim_fit$par["scale"]
# Fit VTI returns using MASS::fitdistr()
optim_fit <- MASS::fitdistr(re_turns,
  densfun="t", df=2)
optim_fit$estimate
optim_fit$sd
lo_cation <- optim_fit$estimate[1]
scal_e <- optim_fit$estimate[2]
summary(optim_fit)

x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
# Plot histogram of VTI returns
histo_gram <- hist(re_turns, col="lightgrey",
  xlab="returns", breaks=100, xlim=c(-0.05, 0.05),
  ylab="frequency", freq=FALSE,
  main="VTI Returns Histogram")
lines(density(re_turns, adjust=1.5),
lwd=3, col="blue")
# Plot the Normal probability distribution
curve(expr=dnorm(x, mean=mean(re_turns),
  sd=sd(re_turns)), add=TRUE, lwd=3, col="green")
# Plot t-distribution function
curve(expr=dt((x-lo_cation)/scal_e, df=2)/scal_e,
type="l", lwd=3, col="red", add=TRUE)
# Add legend
legend("topright", inset=0.05, bty="n",
  leg=c("density", "t-distr", "normal"),
  lwd=6, lty=1,
  col=c("blue", "red", "green"))

x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
# Plot histogram of VTI returns
histo_gram <- hist(re_turns, breaks=100, plot=FALSE)
plot(histo_gram, col="lightgrey",
  xlab="returns", ylab="frequency", freq=FALSE,
  xlim=c(min(re_turns), -0.02),
  ylim=c(0.0, histo_gram$density[findInterval(-0.02, histo_gram$breaks)]),
  main="VTI Left Tail Returns Histogram")
lines(density(re_turns, adjust=1.5),
lwd=4, col="blue")
# Plot t-distribution function
curve(expr=dt((x-lo_cation)/scal_e, df=2)/scal_e,
type="l", lwd=4, col="red", add=TRUE)
# Plot the Normal probability distribution
curve(expr=dnorm(x, mean=mean(re_turns),
  sd=sd(re_turns)), add=TRUE, lwd=4, col="green")
# Add legend
legend("topleft", inset=0.05, bty="n",
  leg=c("density", "t-distr", "normal"),
  lwd=6, lty=1, col=c("blue", "red", "green"))

x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
# Degrees of freedom
deg_free <- c(2, 5, 8, 11)
# Plot four curves in loop
col_ors <- c("red", "black", "blue", "green")
for (in_dex in 1:4) {
curve(expr=dchisq(x, df=deg_free[in_dex]),
xlim=c(0, 20), ylim=c(0, 0.3),
xlab="", ylab="", col=col_ors[in_dex],
lwd=2, add=as.logical(in_dex-1))
}  # end for

# Add title
title(main="Chi-squared Distributions", line=0.5)
# Add legend
lab_els <- paste("df", deg_free, sep="=")
legend("topright", inset=0.05, bty="n",
       title="Degrees of freedom", lab_els,
       cex=0.8, lwd=6, lty=1, col=col_ors)

# KS test for normal distribution
ks.test(rnorm(100), pnorm)
# KS test for uniform distribution
ks.test(runif(100), pnorm)
# KS test for two similar normal distributions
ks.test(rnorm(100), rnorm(100, mean=0.1))
# KS test for two different normal distributions
ks.test(rnorm(100), rnorm(100, mean=1.0))
# Fit t-dist into VTI returns
re_turns <- na.omit(rutils::etf_env$re_turns$VTI)
optim_fit <- MASS::fitdistr(re_turns, densfun="t", df=2)
lo_cation <- optim_fit$estimate[1]
scal_e <- optim_fit$estimate[2]
# Perform Kolmogorov-Smirnov test on VTI returns
da_ta <- lo_cation + scal_e*rt(NROW(re_turns), df=2)
ks.test(as.numeric(re_turns), da_ta)

# Observed frequencies from random normal data
histo_gram <- hist(rnorm(1e3, mean=0), breaks=100, plot=FALSE)
freq_o <- histo_gram$counts
# Theoretical frequencies
freq_t <- rutils::diff_it(pnorm(histo_gram$breaks))
# Perform Chi-squared test for normal data
chisq.test(x=freq_o, p=freq_t, rescale.p=TRUE, simulate.p.value=TRUE)
# Return p-value
chisq_test <- chisq.test(x=freq_o, p=freq_t, rescale.p=TRUE, simulate.p.value=TRUE)
chisq_test$p.value
# Observed frequencies from shifted normal data
histo_gram <- hist(rnorm(1e3, mean=2), breaks=100, plot=FALSE)
freq_o <- histo_gram$counts/sum(histo_gram$counts)
# Theoretical frequencies
freq_t <- rutils::diff_it(pnorm(histo_gram$breaks))
# Perform Chi-squared test for shifted normal data
chisq.test(x=freq_o, p=freq_t, rescale.p=TRUE, simulate.p.value=TRUE)
# Calculate histogram of VTI returns
histo_gram <- hist(re_turns, breaks=100, plot=FALSE)
freq_o <- histo_gram$counts
# Calculate cumulative probabilities and then difference them
freq_t <- pt((histo_gram$breaks-lo_cation)/scal_e, df=2)
freq_t <- rutils::diff_it(freq_t)
# Perform Chi-squared test for VTI returns
chisq.test(x=freq_o, p=freq_t, rescale.p=TRUE, simulate.p.value=TRUE)

# Load package PerformanceAnalytics
library(PerformanceAnalytics)
# Get documentation for package PerformanceAnalytics
# Get short description
packageDescription("PerformanceAnalytics")
# Load help page
help(package="PerformanceAnalytics")
# List all objects in PerformanceAnalytics
ls("package:PerformanceAnalytics")
# List all datasets in PerformanceAnalytics
data(package="PerformanceAnalytics")
# Remove PerformanceAnalytics from search path
detach("package:PerformanceAnalytics")

perf_data <- unclass(data(
    package="PerformanceAnalytics"))$results[, -(1:2)]
apply(perf_data, 1, paste, collapse=" - ")
# Load "managers" data set
data(managers)
class(managers)
dim(managers)
head(managers, 3)

# Load package "PerformanceAnalytics"
library(PerformanceAnalytics)
# Calculate ETF returns
re_turns <- rutils::etf_env$re_turns[,
  c("VTI", "DBC", "IEF")]
re_turns <- na.omit(re_turns)
# Plot cumulative ETF returns
x11(width=6, height=5)
chart.CumReturns(re_turns, lwd=2, ylab="",
  legend.loc="topleft",
  main="ETF Cumulative Returns")

re_turns <- rutils::etf_env$re_turns$VTI
re_turns <- na.omit(re_turns)
x11(width=6, height=5)
chart.Histogram(re_turns, xlim=c(-0.04, 0.04),
  colorset = c("lightgray", "red", "blue"), lwd=3,
  main=paste("Distribution of", colnames(re_turns), "Returns"),
  methods = c("add.density", "add.normal"))
legend("topright", inset=0.05, bty="n",
 leg=c("VTI Density", "Normal"),
 lwd=6, lty=1, col=c("red", "blue"))

re_turns <- rutils::etf_env$re_turns[,
  c("VTI", "IEF", "IVW", "VYM", "IWB", "DBC", "VXX")]
x11(width=6, height=5)
chart.Boxplot(names=FALSE, re_turns)
par(cex.lab=0.8, cex.axis=0.8)
axis(side=2, at=(1:NCOL(re_turns))/7.5-0.05,labels=colnames(re_turns))

# Simulate normally distributed data
n_rows <- 1000
da_ta <- rnorm(n_rows)
sd(da_ta)
mad(da_ta)
median(abs(da_ta - median(da_ta)))
median(abs(da_ta - median(da_ta)))/qnorm(0.75)
# Bootstrap of sd and mad estimators
boot_data <- sapply(1:10000, function(x) {
  sampl_e <- da_ta[sample.int(n_rows, replace=TRUE)]
  c(sd=sd(sampl_e), mad=mad(sampl_e))
})  # end sapply
boot_data <- t(boot_data)
# Analyze bootstrapped variance
head(boot_data)
sum(is.na(boot_data))
# Means and standard errors from bootstrap
apply(boot_data, MARGIN=2, function(x)
  c(mean=mean(x), std_error=sd(x)))
# Parallel bootstrap under Windows
library(parallel)  # Load package parallel
n_cores <- detectCores() - 1  # Number of cores
clus_ter <- makeCluster(n_cores)  # Initialize compute cluster
boot_data <- parLapply(clus_ter, 1:10000,
  function(x, da_ta) {
    sampl_e <- da_ta[sample.int(n_rows, replace=TRUE)]
    c(sd=sd(sampl_e), mad=mad(sampl_e))
  }, da_ta=da_ta)  # end parLapply
# Parallel bootstrap under Mac-OSX or Linux
boot_data <- mclapply(1:10000, function(x) {
    sampl_e <- da_ta[sample.int(n_rows, replace=TRUE)]
    c(sd=sd(sampl_e), mad=mad(sampl_e))
  }, mc.cores=n_cores)  # end mclapply
stopCluster(clus_ter)  # Stop R processes over cluster
boot_data <- rutils::do_call(rbind, boot_data)
# Means and standard errors from bootstrap
apply(boot_data, MARGIN=2, function(x)
  c(mean=mean(x), std_error=sd(x)))

# VTI returns
re_turns <- rutils::etf_env$re_turns$VTI
re_turns <- na.omit(re_turns)
n_rows <- NROW(re_turns)
sd(re_turns)
mad(re_turns)
# Bootstrap of sd and mad estimators
boot_data <- sapply(1:10000, function(x) {
  sampl_e <- re_turns[sample.int(n_rows, replace=TRUE)]
  c(sd=sd(sampl_e), mad=mad(sampl_e))
})  # end sapply
boot_data <- t(boot_data)
# Means and standard errors from bootstrap
100*apply(boot_data, MARGIN=2, function(x)
  c(mean=mean(x), std_error=sd(x)))
# Parallel bootstrap under Windows
library(parallel)  # Load package parallel
n_cores <- detectCores() - 1  # Number of cores
clus_ter <- makeCluster(n_cores)  # Initialize compute cluster
clusterExport(clus_ter, c("n_rows", "re_turns"))
boot_data <- parLapply(clus_ter, 1:10000,
  function(x) {
    sampl_e <- re_turns[sample.int(n_rows, replace=TRUE)]
    c(sd=sd(sampl_e), mad=mad(sampl_e))
  })  # end parLapply
# Parallel bootstrap under Mac-OSX or Linux
boot_data <- mclapply(1:10000, function(x) {
    sampl_e <- re_turns[sample.int(n_rows, replace=TRUE)]
    c(sd=sd(sampl_e), mad=mad(sampl_e))
  }, mc.cores=n_cores)  # end mclapply
stopCluster(clus_ter)  # Stop R processes over cluster
boot_data <- rutils::do_call(rbind, boot_data)
# Means and standard errors from bootstrap
apply(boot_data, MARGIN=2, function(x)
  c(mean=mean(x), std_error=sd(x)))

library(PerformanceAnalytics)
# Define target rate of return of 50 bps
tar_get <- 0.005
# Calculate the full downside returns
returns_sub <- (re_turns - tar_get)
returns_sub <- ifelse(returns_sub < 0, returns_sub, 0)
n_rows <- NROW(returns_sub)
# Calculate the downside deviation
all.equal(sqrt(sum(returns_sub^2)/n_rows),
  drop(DownsideDeviation(re_turns, MAR=tar_get, method="full")))
# Calculate the subset downside returns
returns_sub <- (re_turns - tar_get)
returns_sub <- returns_sub[returns_sub < 0]
n_rows <- NROW(returns_sub)
# Calculate the downside deviation
all.equal(sqrt(sum(returns_sub^2)/n_rows),
  DownsideDeviation(re_turns, MAR=tar_get, method="subset"))

# Calculate time series of VTI drawdowns
price_s <- quantmod::Cl(rutils::etf_env$VTI)
draw_downs <- (price_s - cummax(price_s))
# PerformanceAnalytics plot of VTI drawdowns
re_turns <- rutils::diff_it(log(price_s))
PerformanceAnalytics::chart.Drawdown(re_turns,
  ylab="", main="VTI Drawdowns")
# PerformanceAnalytics table of VTI drawdowns
PerformanceAnalytics::table.Drawdowns(re_turns)
# dygraph plot of VTI drawdowns
da_ta <- cbind(price_s, draw_downs)
col_names <- c("VTI", "Drawdowns")
colnames(da_ta) <- col_names
dygraphs::dygraph(da_ta, main="VTI Drawdowns") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], valueRange=c(min(da_ta[, "Drawdowns"]), 5), independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="blue") %>%
  dySeries(name=col_names[2], axis="y2", col="red")
# Plot VTI drawdowns
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("blue")
x11(width=6, height=5)
quantmod::chart_Series(x=draw_downs, name="VTI Drawdowns", theme=plot_theme)

library(xtable)
library(PerformanceAnalytics)
price_s <- quantmod::Cl(rutils::etf_env$VTI)
re_turns <- rutils::diff_it(log(price_s))
# Calculate table of VTI drawdowns
tabl_e <- PerformanceAnalytics::table.Drawdowns(re_turns)
# Convert dates to strings
tabl_e <- cbind(sapply(tabl_e[, 1:3], as.character), tabl_e[, 4:7])
# Print table of VTI drawdowns
print(xtable(tabl_e), comment=FALSE, size="tiny", include.rownames=FALSE)

# Load "managers" data set
data(managers)
charts.PerformanceSummary(ham_1,
  main="", lwd=2, ylog=TRUE)

x11(width=6, height=4)
par(mar=c(3, 2, 1, 0), oma=c(0, 0, 0, 0))
# VTI percentage returns
re_turns <- na.omit(rutils::etf_env$re_turns$VTI)
conf_level <- 0.1
va_r <- quantile(re_turns, conf_level)
c_var <- mean(re_turns[re_turns < va_r])
# Plot histogram of VTI returns
histo_gram <- hist(re_turns, col="lightgrey",
  xlab="returns", ylab="frequency", breaks=100,
  xlim=c(-0.05, 0.01), freq=FALSE, main="VTI Returns Histogram")
# Calculate density
densi_ty <- density(re_turns, adjust=1.5)

# Plot density
lines(densi_ty, lwd=3, col="blue")
# Plot line for VaR
abline(v=va_r, col="red", lwd=3)
text(x=va_r, y=20, labels="VaR",
     lwd=2, srt=90, pos=2)
# Plot polygon shading for CVaR
var_max <- -0.06
rang_e <- (densi_ty$x < va_r) &
  (densi_ty$x > var_max)
polygon(c(var_max, densi_ty$x[rang_e], va_r),
  c(0, densi_ty$y[rang_e], 0),
  col=rgb(1, 0, 0,0.5), border=NA)
text(x=va_r, y=3, labels="CVaR", lwd=2, pos=2)

# VTI percentage returns
re_turns <- na.omit(rutils::etf_env$re_turns$VTI)
conf_level <- 0.02
# Calculate VaR as quantile
va_r <- quantile(re_turns, conf_level)
# Or by sorting
sort_ed <- sort(as.numeric(re_turns))
in_dex <- round(conf_level*NROW(re_turns))
va_r <- sort_ed[in_dex]
# PerformanceAnalytics VaR
PerformanceAnalytics::VaR(re_turns,
  p=(1-conf_level), method="historical")
all.equal(unname(va_r),
  as.numeric(PerformanceAnalytics::VaR(re_turns,
  p=(1-conf_level), method="historical")))

x11(width=6, height=4)
par(mar=c(3, 2, 1, 0), oma=c(0, 0, 0, 0))
# Calculate VaR as quantile
va_r <- quantile(re_turns, conf_level)
# Calculate CVaR as expected loss
c_var <- mean(re_turns[re_turns < va_r])
# Or by sorting
sort_ed <- sort(as.numeric(re_turns))
in_dex <- round(conf_level*NROW(re_turns))
va_r <- sort_ed[in_dex]
c_var <- mean(sort_ed[1:in_dex])
# PerformanceAnalytics VaR
PerformanceAnalytics::ETL(re_turns,
  p=(1-conf_level), method="historical")
all.equal(c_var,
  as.numeric(PerformanceAnalytics::ETL(re_turns,
  p=(1-conf_level), method="historical")))

# Calculate the Sharpe ratio
risk_return <- table.Stats(rutils::etf_env$re_turns)
class(risk_return)
all.equal(risk_return, rutils::etf_env$risk_return)
# Transpose the data frame
risk_return <- as.data.frame(t(risk_return))
# Remove VIX etf data
risk_return <- risk_return[-match(c("VXX", "SVXY"), rownames(risk_return)), ]
# Plot scatterplot
plot(Kurtosis ~ Skewness, data=risk_return,
     ylim=c(1, max(risk_return$Kurtosis)),
     main="Kurtosis vs Skewness")
# Add labels
text(x=risk_return$Skewness, y=risk_return$Kurtosis,
    labels=rownames(risk_return),
    pos=1, cex=0.8)

risk_return <- rutils::etf_env$risk_return
risk_return <- as.data.frame(t(risk_return))
# Add skew_kurt column
risk_return$skew_kurt <-
  risk_return$Skewness/risk_return$Kurtosis
# Sort on skew_kurt
risk_return <- risk_return[
  order(risk_return$skew_kurt,
  decreasing=TRUE), ]

# Add names column
risk_return$Name <- rutils::etf_env$etf_list[rownames(risk_return), ]$Name
knitr::kable(risk_return[, c("Name", "Skewness", "Kurtosis")])

library(PerformanceAnalytics)
re_turns <- rutils::etf_env$re_turns[, c("VTI", "IEF")]
re_turns <- na.omit(re_turns)
# Calculate the Sharpe ratio
PerformanceAnalytics::SharpeRatio(re_turns)
# Calculate the Sortino ratio
PerformanceAnalytics::SortinoRatio(re_turns)
# Calculate the Calmar ratio
PerformanceAnalytics::CalmarRatio(re_turns)
# Calculate the returns statistics
tail(PerformanceAnalytics::table.Stats(re_turns), 4)
