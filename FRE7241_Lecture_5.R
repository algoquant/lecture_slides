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
residual_s <- (sell_inmay - mod_el$coefficients[2]*re_turns$VTI)
plot.default(x=re_turns$VTI, y=residual_s, xlab="VTI", ylab="sell_inmay")
title(main="Treynor-Mazuy market timing test\n for Sell in May vs VTI", line=0.5)
# Plot fitted (predicted) response values
fit_ted <- (mod_el$fitted.values - mod_el$coefficients[2]*re_turns$VTI)
points.default(x=re_turns$VTI, y=fit_ted, pch=16, col="red")
text(x=0.05, y=0.03, paste("Treynor test t-value =", round(summary(mod_el)$coefficients["treynor", "t value"], 2)))

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
sim_ple <- do.call(cbind, sim_ple)
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
new_prices <- do.call(cbind, new_prices)
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

# Extract ETF returns
sym_bols <- c("VTI", "IEF", "DBC")
re_turns <- rutils::etf_env$re_turns[, sym_bols]
# Calculate the positions of the first non-NA values
in_dex <- sapply(re_turns, function(x_ts) {
  match(TRUE, !is.na(x_ts))
})  # end sapply
# Remove rows with missing IEF data
re_turns <- re_turns[-(1:in_dex["IEF"]), ]
# Copy over NA values
re_turns <- zoo::na.locf(re_turns, na.rm=FALSE)
re_turns <- zoo::na.locf(re_turns, fromLast=TRUE)

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
all_weather <- re_turns %*% weights_aw
all_weather <- cumprod(1 + all_weather)
all_weather <- xts::xts(all_weather[end_points], in_dex)
# Plot the Momentum strategy and benchmark
da_ta <- cbind(weal_th, all_weather)
colnames(da_ta) <- c("Momentum Strategy", "Benchmark")
dygraphs::dygraph(da_ta, main="Momentum Strategy") %>%
  dyAxis("y", label="Benchmark", independentTicks=TRUE) %>%
  dyAxis("y2", label="Momentum Strategy", independentTicks=TRUE) %>%
  dySeries(name="Momentum Strategy", axis="y2", label="Momentum Strategy", strokeWidth=2, col="red") %>%
  dySeries(name="Benchmark", axis="y", label="Benchmark", strokeWidth=2, col="blue")

# Define backtest functional
back_test_momentum <- function(re_turns,
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
}  # end back_test_momentum

source("C:/Develop/lecture_slides/scripts/back_test.R")
look_backs <- seq(3, 15, by=1)
perform_ance <- function(re_turns) sum(re_turns)/sd(re_turns)
end_points <- rutils::calc_endpoints(re_turns, inter_val="months")
pro_files <- sapply(look_backs, function(look_back) {
  pnl_s <- back_test_momentum(re_turns=re_turns, end_points=end_points,
    look_back=look_back, perform_ance=perform_ance)
  last(cumprod(1 + pnl_s))
})  # end sapply
x11(width=6, height=4)
plot(x=look_backs, y=pro_files, t="l",
  main="Strategy PnL as function of look_back",
  xlab="look_back (months)", ylab="pnl")

look_back <- look_backs[which.max(pro_files)]
pnl_s <- back_test_momentum(re_turns=re_turns, price_s=price_s,
  look_back=look_back, end_points=end_points,
  perform_ance=perform_ance, with_weights=TRUE)
tail(pnl_s)
weal_th <- cumprod(1 + pnl_s[, 1])
in_dex <- zoo::index(re_turns[end_points])
weal_th <- xts::xts(weal_th, in_dex)
da_ta <- cbind(weal_th, all_weather)
colnames(da_ta) <- c("Momentum Strategy", "Benchmark")

# Plot the Momentum strategy and benchmark
dygraphs::dygraph(da_ta, main="Momentum Strategy") %>%
  dyAxis("y", label="Benchmark", independentTicks=TRUE) %>%
  dyAxis("y2", label="Momentum Strategy", independentTicks=TRUE) %>%
  dySeries(name="Momentum Strategy", axis="y2", label="Momentum Strategy", strokeWidth=2, col="red") %>%
  dySeries(name="Benchmark", axis="y", label="Benchmark", strokeWidth=2, col="blue")
# Or
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue")
chart_Series(da_ta, theme=plot_theme, lwd=2,
       name="Momentum PnL")
legend("topleft", legend=colnames(da_ta),
  inset=0.1, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")

# Plot the momentum portfolio weights
da_ta <- cbind(rutils::etf_env$price_s$VTI[in_dex],
         weight_s)
da_ta <- na.omit(da_ta)
colnames(da_ta)[2:4] <- paste0(colnames(weight_s), "_weight")
x11(width=6, height=7)
zoo::plot.zoo(da_ta, xlab=NULL, main="Momentum Weights")

# Calculate ETF betas
betas_etf <- sapply(re_turns, function(x)
  cov(re_turns$VTI, x)/var(x))
# Betas equal weights times ETF betas
beta_s <- pnl_s[, 2:4] %*% betas_etf
beta_s <- xts(beta_s, order.by=in_dex)
colnames(beta_s) <- "momentum_beta"
da_ta <- cbind(beta_s, rutils::etf_env$VTI[in_dex, 4])
zoo::plot.zoo(da_ta,
  oma = c(3, 1, 3, 0), mar = c(0, 4, 0, 1),
  main="betas & VTI", xlab="")

# Open x11 for plotting
x11(width=6, height=4)
# Set plot parameters to reduce whitespace around plot
par(mar=c(4, 4, 3, 1), oma=c(0, 0, 0, 0))
pnl_s <- as.numeric(pnl_s[, 1])
vt_i <- as.numeric(rutils::diff_it(log(rutils::etf_env$price_s$VTI[in_dex])))
# Merton-Henriksson test
de_sign <- cbind(VTI=vt_i, skill=0.5*(vt_i+abs(vt_i)))
mod_el <- lm(pnl_s ~ de_sign); summary(mod_el)
# Treynor-Mazuy test
de_sign <- cbind(VTI=vt_i, skill=vt_i^2)
mod_el <- lm(pnl_s ~ de_sign); summary(mod_el)
# Plot residual scatterplot
plot(x=vt_i, y=pnl_s, xlab="VTI", ylab="momentum")
title(main="Treynor-Mazuy market timing test\n for Momentum vs VTI", line=0.5)
# Plot fitted (predicted) response values
points(x=vt_i, y=mod_el$fitted.values, pch=16, col="red")

# Standardize the returns
momen_tum <- (pnl_s-mean(pnl_s))/sd(pnl_s)
vt_i <- (vt_i-mean(vt_i))/sd(vt_i)
# Calculate skewness and kurtosis
apply(cbind(momen_tum, vt_i), 2, function(x)
  sapply(c(skew=3, kurt=4),
    function(e) sum(x^e)))/n_rows

# Plot histogram
hist(momen_tum, breaks=30,
  main="Momentum and VTI Return Distributions",
  xlim=c(-4, 4),
  xlab="", ylab="", freq=FALSE)
# Draw kernel density of histogram
lines(density(momen_tum), col='red', lwd=2)
lines(density(vt_i), col='blue', lwd=2)
# Add legend
legend("topright", inset=0.05, cex=0.8, title=NULL,
 leg=c("Momentum", "VTI"),
 lwd=6, bg="white", col=c("red", "blue"))

# Combine momentum strategy with all-weather
all_weather <- (re_turns %*% weights_aw)[end_points]
all_weather <- sd(pnl_s)*all_weather/sd(all_weather)
da_ta <- cbind(pnl_s, all_weather, 0.5*(pnl_s + all_weather))
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
  dyOptions(colors=c("red", "blue", "green"), strokeWidth=2) %>%
  dyLegend(show="always")
# Or
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("red", "blue", "green")
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
weight_s <- pas_t/sqrt(look_back*vari_ance)
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
da_ta <- cbind(all_weather, weal_th)
colnames(da_ta) <- c("all_weather", "momentum")
col_names <- colnames(da_ta)
dygraphs::dygraph(da_ta, main="Momentum vs All-Weather") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="blue") %>%
  dySeries(name=col_names[2], axis="y2", col="red")

# Define backtest functional for daily momentum strategy
# If tre_nd=(-1) then it backtests a mean reverting strategy
backtest_rolling <- function(re_turns, look_back=252, bid_offer=0.001, tre_nd=1, ...) {
  stopifnot("package:quantmod" %in% search() || require("quantmod", quietly=TRUE))
  # Calculate rolling variance
  vari_ance <- roll::roll_var(re_turns, width=look_back)
  vari_ance <- zoo::na.locf(vari_ance, na.rm=FALSE)
  vari_ance[is.na(vari_ance)] <- 0
  # Calculate rolling Sharpe
  pas_t <- roll::roll_mean(re_turns, width=look_back)
  weight_s <- pas_t/sqrt(look_back*vari_ance)
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
}  # end backtest_rolling

# Backtest a daily ETF momentum strategy
source("C:/Develop/lecture_slides/scripts/back_test.R")
weal_th <- backtest_rolling(look_back=252,
  re_turns=re_turns, bid_offer=bid_offer)
# Perform sapply loop over look_backs
look_backs <- seq(50, 300, by=50)
weal_th <- sapply(look_backs, backtest_rolling,
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
weal_th <- sapply(look_backs, backtest_rolling,
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
weal_th <- sapply(look_backs, backtest_rolling,
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

# Create random real symmetric matrix
mat_rix <- matrix(runif(25), nc=5)
mat_rix <- mat_rix + t(mat_rix)
# Calculate eigenvectors and eigenvalues
ei_gen <- eigen(mat_rix)
eigen_vec <- ei_gen$vectors
dim(eigen_vec)
# Plot eigenvalues
barplot(ei_gen$values,
  xlab="", ylab="", las=3,
  names.arg=paste0("ev", 1:NROW(ei_gen$values)),
  main="Eigenvalues of a real symmetric matrix")

# eigenvectors form an orthonormal basis
round(t(eigen_vec) %*% eigen_vec,
  digits=4)
# Diagonalize matrix using eigenvector matrix
round(t(eigen_vec) %*% (mat_rix %*% eigen_vec),
  digits=4)
ei_gen$values
# eigen decomposition of matrix by rotating the diagonal matrix
de_comp <- eigen_vec %*% (ei_gen$values * t(eigen_vec))
# Create diagonal matrix of eigenvalues
# diago_nal <- diag(ei_gen$values)
# de_comp <- eigen_vec %*% (diago_nal %*% t(eigen_vec))
all.equal(mat_rix, de_comp)

# Create random positive semi-definite matrix
mat_rix <- matrix(runif(25), nc=5)
mat_rix <- t(mat_rix) %*% mat_rix
# Calculate eigenvectors and eigenvalues
ei_gen <- eigen(mat_rix)
ei_gen$values
# Plot eigenvalues
barplot(ei_gen$values, las=3,
  xlab="", ylab="",
  names.arg=paste0("ev", 1:NROW(ei_gen$values)),
  main="Eigenvalues of positive semi-definite matrix")

# Perform singular value decomposition
mat_rix <- matrix(rnorm(50), nc=5)
s_vd <- svd(mat_rix)
# Recompose mat_rix from SVD mat_rices
all.equal(mat_rix,
  s_vd$u %*% (s_vd$d*t(s_vd$v)))
# Columns of U and V are orthonormal
round(t(s_vd$u) %*% s_vd$u, 4)
round(t(s_vd$v) %*% s_vd$v, 4)

# Dimensions of left and right matrices
n_left <- 6 ; n_right <- 4
# Calculate left matrix
left_mat <- matrix(runif(n_left^2), nc=n_left)
ei_gen <- eigen(crossprod(left_mat))
left_mat <- ei_gen$vectors[, 1:n_right]
# Calculate right matrix and singular values
right_mat <- matrix(runif(n_right^2), nc=n_right)
ei_gen <- eigen(crossprod(right_mat))
right_mat <- ei_gen$vectors
sing_values <- sort(runif(n_right, min=1, max=5), decreasing=TRUE)
# Compose rectangular matrix
mat_rix <- left_mat %*% (sing_values * t(right_mat))
# Perform singular value decomposition
s_vd <- svd(mat_rix)
# Recompose mat_rix from SVD
all.equal(mat_rix, s_vd$u %*% (s_vd$d*t(s_vd$v)))
# Compare SVD with mat_rix components
all.equal(abs(s_vd$u), abs(left_mat))
all.equal(abs(s_vd$v), abs(right_mat))
all.equal(s_vd$d, sing_values)
# Eigen decomposition of mat_rix squared
square_d <- mat_rix %*% t(mat_rix)
ei_gen <- eigen(square_d)
all.equal(ei_gen$values[1:n_right], sing_values^2)
all.equal(abs(ei_gen$vectors[, 1:n_right]), abs(left_mat))
# Eigen decomposition of mat_rix squared
square_d <- t(mat_rix) %*% mat_rix
ei_gen <- eigen(square_d)
all.equal(ei_gen$values, sing_values^2)
all.equal(abs(ei_gen$vectors), abs(right_mat))

# Create random positive semi-definite matrix
mat_rix <- matrix(runif(25), nc=5)
mat_rix <- t(mat_rix) %*% mat_rix
# Calculate the inverse of mat_rix
in_verse <- solve(a=mat_rix)
# Multiply inverse with matrix
round(in_verse %*% mat_rix, 4)
round(mat_rix %*% in_verse, 4)

# Calculate eigenvectors and eigenvalues
ei_gen <- eigen(mat_rix)
eigen_vec <- ei_gen$vectors

# Perform eigen decomposition of inverse
eigen_inverse <-
  eigen_vec %*% (t(eigen_vec) / ei_gen$values)
all.equal(in_verse, eigen_inverse)
# Decompose diagonal matrix with inverse of eigenvalues
# diago_nal <- diag(1/ei_gen$values)
# eigen_inverse <-
#   eigen_vec %*% (diago_nal %*% t(eigen_vec))

# Random rectangular matrix: n_left > n_right
n_left <- 6 ; n_right <- 4
mat_rix <- matrix(runif(n_left*n_right),
  nc=n_right)
# Calculate generalized inverse of mat_rix
in_verse <- MASS::ginv(mat_rix)
round(in_verse %*% mat_rix, 4)
all.equal(mat_rix,
  mat_rix %*% in_verse %*% mat_rix)
# Random rectangular matrix: n_left < n_right
n_left <- 4 ; n_right <- 6
mat_rix <- matrix(runif(n_left*n_right),
  nc=n_right)
# Calculate generalized inverse of mat_rix
in_verse <- MASS::ginv(mat_rix)
all.equal(mat_rix, mat_rix %*% in_verse %*% mat_rix)
round(mat_rix %*% in_verse, 4)
round(in_verse %*% mat_rix, 4)
# Perform singular value decomposition
s_vd <- svd(mat_rix)
# Calculate generalized inverse from SVD
svd_inverse <- s_vd$v %*% (t(s_vd$u) / s_vd$d)
all.equal(svd_inverse, in_verse)
# Calculate Moore-Penrose pseudo-inverse
mp_inverse <-
  MASS::ginv(t(mat_rix) %*% mat_rix) %*% t(mat_rix)
all.equal(mp_inverse, in_verse)

# Create random singular matrix
# more columns than rows: n_right > n_left
n_left <- 4 ; n_right <- 6
mat_rix <- matrix(runif(n_left*n_right), nc=n_right)
mat_rix <- t(mat_rix) %*% mat_rix
# Perform singular value decomposition
s_vd <- svd(mat_rix)
# Incorrect inverse from SVD because of zero singular values
svd_inverse <- s_vd$v %*% (t(s_vd$u) / s_vd$d)
# Verify inverse property of mat_rix
all.equal(mat_rix,
  mat_rix %*% svd_inverse %*% mat_rix)

# Set tolerance for determining zero singular values
to_l <- sqrt(.Machine$double.eps)
# Check for zero singular values
round(s_vd$d, 12)
not_zero <- (s_vd$d > (to_l * s_vd$d[1]))
# Calculate generalized inverse from SVD
svd_inverse <-
  s_vd$v[, not_zero] %*%
  (t(s_vd$u[, not_zero]) / s_vd$d[not_zero])
# Verify inverse property of mat_rix
all.equal(mat_rix,
  mat_rix %*% svd_inverse %*% mat_rix)
# Calculate generalized inverse using MASS::ginv()
in_verse <- MASS::ginv(mat_rix)
all.equal(svd_inverse, in_verse)
# Calculate Moore-Penrose pseudo-inverse
mp_inverse <-
  MASS::ginv(t(mat_rix) %*% mat_rix) %*% t(mat_rix)
all.equal(mp_inverse, in_verse)

# Diagonalize the "unit" matrix
uni_t <- mat_rix %*% in_verse
round(uni_t, 4)
round(mat_rix %*% in_verse, 4)
round(t(s_vd$u) %*% uni_t %*% s_vd$v, 4)

# Define a square matrix
mat_rix <- matrix(c(1, 2, -1, 2), nc=2)
vec_tor <- c(2, 1)
# Calculate the inverse of mat_rix
in_verse <- solve(a=mat_rix)
in_verse %*% mat_rix
# Calculate solution using inverse of mat_rix
solu_tion <- in_verse %*% vec_tor
mat_rix %*% solu_tion
# Calculate solution of linear system
solu_tion <- solve(a=mat_rix, b=vec_tor)
mat_rix %*% solu_tion

# Create large random positive semi-definite matrix
mat_rix <- matrix(runif(1e4), nc=100)
mat_rix <- t(mat_rix) %*% mat_rix
# Calculate eigen decomposition
ei_gen <- eigen(mat_rix)
eigen_val <- ei_gen$values
eigen_vec <- ei_gen$vectors
# Set tolerance for determining zero singular values
to_l <- sqrt(.Machine$double.eps)
# If needed convert to positive definite matrix
not_zero <- (eigen_val > (to_l * eigen_val[1]))
if (sum(!not_zero) > 0) {
  eigen_val[!not_zero] <- 2*to_l
  mat_rix <- eigen_vec %*%
    (eigen_val * t(eigen_vec))
}  # end if
# Calculate the Cholesky mat_rix
choles_ky <- chol(mat_rix)
choles_ky[1:5, 1:5]
all.equal(mat_rix, t(choles_ky) %*% choles_ky)
# Calculate inverse from Cholesky
chol_inverse <- chol2inv(choles_ky)
all.equal(solve(mat_rix), chol_inverse)
# Compare speed of Cholesky inversion
library(microbenchmark)
summary(microbenchmark(
  sol_ve=solve(mat_rix),
  choles_ky=chol2inv(chol(mat_rix)),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# Calculate random covariance matrix
cov_mat <- matrix(runif(25), nc=5)
cov_mat <- t(cov_mat) %*% cov_mat
# Calculate the Cholesky mat_rix
choles_ky <- chol(cov_mat)
choles_ky
# Simulate random uncorrelated returns
n_assets <- 5
n_rows <- 10000
re_turns <- matrix(rnorm(n_assets*n_rows), nc=n_assets)
# Calculate correlated returns by applying Cholesky
corr_returns <- re_turns %*% choles_ky
# Calculate covariance matrix
cov_returns <- crossprod(corr_returns) / (n_rows-1)
all.equal(cov_mat, cov_returns)

# Simulate random portfolio returns
n_assets <- 10
n_rows <- 100
set.seed(1121)  # Initialize random number generator
re_turns <- matrix(rnorm(n_assets*n_rows), nc=n_assets)
# Calculate de-meaned re_turns matrix
re_turns <- t(t(re_turns) - colMeans(re_turns))
# Or
re_turns <- apply(re_turns, MARGIN=2, function(x) (x-mean(x)))
# Calculate covariance matrix
cov_mat <- crossprod(re_turns) / (n_rows-1)
# Calculate eigenvectors and eigenvalues
ei_gen <- eigen(cov_mat)
ei_gen$values
barplot(ei_gen$values, # Plot eigenvalues
  xlab="", ylab="", las=3,
  names.arg=paste0("ev", 1:NROW(ei_gen$values)),
  main="Eigenvalues of covariance matrix")

# Calculate eigenvectors and eigenvalues
# as function of number of returns
n_data <- ((n_assets/2):(2*n_assets))
e_values <- sapply(n_data, function(x) {
  re_turns <- re_turns[1:x, ]
  re_turns <- apply(re_turns, MARGIN=2,
    function(y) (y-mean(y)))
  cov_mat <- crossprod(re_turns) / (x-1)
  min(eigen(cov_mat)$values)
})  # end sapply
plot(y=e_values, x=n_data, t="l",
  xlab="", ylab="", lwd=3, col="blue",
  main="Smallest eigenvalue of covariance matrix\nas function of number of returns")

# Create rectangular matrix with collinear columns
se_ries <- matrix(rnorm(10*8), nc=10)
# Calculate covariance matrix
cov_mat <- cov(se_ries)
# Calculate inverse of cov_mat - error
in_verse <- solve(cov_mat)
# Calculate regularized inverse of cov_mat
in_verse <- MASS::ginv(cov_mat)
# Verify inverse property of mat_rix
all.equal(cov_mat,
  cov_mat %*% in_verse %*% cov_mat)
# Perform eigen decomposition
ei_gen <- eigen(cov_mat)
eigen_vec <- ei_gen$vectors
eigen_val <- ei_gen$values
# Set tolerance for determining zero singular values
to_l <- sqrt(.Machine$double.eps)
# Calculate regularized inverse matrix
not_zero <- (eigen_val > (to_l * eigen_val[1]))
reg_inverse <- eigen_vec[, not_zero] %*%
  (t(eigen_vec[, not_zero]) / eigen_val[not_zero])
# Verify inverse property of mat_rix
all.equal(in_verse, reg_inverse)
# Create random covariance matrix
set.seed(1121)
mat_rix <- matrix(rnorm(5e2), nc=5)
cov_mat <- cov(mat_rix)
# Perform eigen decomposition
ei_gen <- eigen(cov_mat)
eigen_vec <- ei_gen$vectors
# Calculate regularized inverse matrix
max_eigen <- 2
in_verse <- eigen_vec[, 1:max_eigen] %*%
  (t(eigen_vec[, 1:max_eigen]) / ei_gen$values[1:max_eigen])

# Create random covariance matrix
set.seed(1121)
mat_rix <- matrix(rnorm(5e2), nc=5)
cov_mat <- cov(mat_rix)
cor_mat <- cor(mat_rix)
std_dev <- sqrt(diag(cov_mat))
# Calculate target matrix
cor_mean <- mean(cor_mat[upper.tri(cor_mat)])
tar_get <- matrix(cor_mean, nr=NROW(cov_mat), nc=NCOL(cov_mat))
diag(tar_get) <- 1
tar_get <- t(t(tar_get * std_dev) * std_dev)
# Calculate shrinkage covariance matrix
al_pha <- 0.5
cov_shrink <- (1-al_pha)*cov_mat + al_pha*tar_get
# Calculate inverse matrix
in_verse <- solve(cov_shrink)

# Formula of linear model with zero intercept
for_mula <- z ~ x + y - 1
for_mula

# Collapse vector of strings into single text string
paste0("x", 1:5)
paste(paste0("x", 1:5), collapse="+")

# Create formula from text string
for_mula <- as.formula(
  # Coerce text strings to formula
  paste("z ~ ",
  paste(paste0("x", 1:5), collapse="+")
  )  # end paste
)  # end as.formula
class(for_mula)
for_mula
# Modify the formula using "update"
update(for_mula, log(.) ~ . + beta)

# Define explanatory (design) variable
len_gth <- 100
set.seed(1121)  # initialize random number generator
de_sign <- runif(len_gth)
noise <- rnorm(len_gth)
# Response equals linear form plus random noise
res_ponse <- (1 + de_sign + noise)

# Calculate de-meaned explanatory (design) and response vectors
design_zm <- de_sign - mean(de_sign)
response_zm <- res_ponse - mean(res_ponse)
# Calculate the regression beta
be_ta <- sum(design_zm*response_zm)/sum(design_zm^2)
# Calculate the regression alpha
al_pha <- mean(res_ponse) - be_ta*mean(de_sign)

# Specify regression formula
for_mula <- res_ponse ~ de_sign
mod_el <- lm(for_mula)  # Perform regression
class(mod_el)  # Regressions have class lm
attributes(mod_el)
eval(mod_el$call$formula)  # Regression formula
mod_el$coeff  # Regression coefficients
all.equal(coef(mod_el), c(al_pha, be_ta), 
  check.attributes=FALSE)

fit_ted <- (al_pha + be_ta*de_sign)
all.equal(fit_ted, mod_el$fitted.values, check.attributes=FALSE)
x11(width=5, height=4)  # Open x11 for plotting
# Set plot parameters to reduce whitespace around plot
par(mar=c(5, 5, 2, 1), oma=c(0, 0, 0, 0))
# Plot scatterplot using formula
plot(for_mula, xlab="design", ylab="response")
title(main="Simple Regression", line=0.5)
# Add regression line
abline(mod_el, lwd=3, col="blue")
# Plot fitted (predicted) response values
points(x=de_sign, y=mod_el$fitted.values,
       pch=16, col="blue")

# Plot response without noise
lines(x=de_sign, y=(res_ponse-noise),
      col="red", lwd=3)
legend(x="topleft", # Add legend
       legend=c("response without noise", "fitted values"),
       title=NULL, inset=0.08, cex=0.8, lwd=6,
       lty=1, col=c("red", "blue"))

# Calculate the residuals
fit_ted <- (al_pha + be_ta*de_sign)
resid_uals <- (res_ponse - fit_ted)
all.equal(resid_uals, mod_el$residuals, check.attributes=FALSE)
# Residuals are orthogonal to the de_sign
all.equal(sum(resid_uals*de_sign), target=0)
# Residuals are orthogonal to the fitted values
all.equal(sum(resid_uals*fit_ted), target=0)
# Sum of residuals is equal to zero
all.equal(mean(resid_uals), target=0)

x11(width=6, height=5)  # Open x11 for plotting
# Set plot parameters to reduce whitespace around plot
par(mar=c(5, 5, 1, 1), oma=c(0, 0, 0, 0))
# Extract residuals
da_ta <- cbind(de_sign, mod_el$residuals)
colnames(da_ta) <- c("design", "residuals")
# Plot residuals
plot(da_ta)
title(main="Residuals of the Linear Regression", line=-1)
abline(h=0, lwd=3, col="red")

# Degrees of freedom of residuals
deg_free <- mod_el$df.residual
# Standard deviation of residuals
resid_stddev <- 
  sqrt(sum(resid_uals^2)/deg_free)
# Standard error of beta
beta_stderror <- 
  resid_stddev/sqrt(sum(design_zm^2))
# Standard error of alpha
alpha_stderror <- resid_stddev*
  sqrt(1/len_gth + mean(de_sign)^2/sum(design_zm^2))

model_sum <- summary(mod_el)  # Copy regression summary
model_sum  # Print the summary to console
attributes(model_sum)$names  # get summary elements

model_sum$coeff
# Standard errors
model_sum$coefficients[2, "Std. Error"]
all.equal(c(alpha_stderror, beta_stderror), 
  model_sum$coefficients[, "Std. Error"], check.attributes=FALSE)
# R-squared
model_sum$r.squared
model_sum$adj.r.squared
# F-statistic and ANOVA
model_sum$fstatistic
anova(mod_el)

set.seed(1121)  # initialize random number generator
# High noise compared to coefficient
res_ponse <- (1 + de_sign + rnorm(len_gth, sd=8))
mod_el <- lm(for_mula)  # Perform regression
# Values of regression coefficients are not
# Statistically significant
summary(mod_el)

par(oma=c(1, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=1.0, cex.axis=1.0, cex.main=1.0, cex.sub=1.0)
reg_stats <- function(std_dev) {  # Noisy regression
  set.seed(1121)  # initialize number generator
# Define explanatory (design) and response variables
  de_sign <- rnorm(100, mean=2)
  res_ponse <- (1 + 0.2*de_sign +
    rnorm(NROW(de_sign), sd=std_dev))
# Specify regression formula
  for_mula <- res_ponse ~ de_sign
# Perform regression and get summary
  model_sum <- summary(lm(for_mula))
# Extract regression statistics
  with(model_sum, c(pval=coefficients[2, 4],
   adj_rsquared=adj.r.squared,
   fstat=fstatistic[1]))
}  # end reg_stats
# Apply reg_stats() to vector of std dev values
vec_sd <- seq(from=0.1, to=0.5, by=0.1)
names(vec_sd) <- paste0("sd=", vec_sd)
mat_stats <- t(sapply(vec_sd, reg_stats))
# Plot in loop
par(mfrow=c(NCOL(mat_stats), 1))
for (in_dex in 1:NCOL(mat_stats)) {
  plot(mat_stats[, in_dex], type="l",
 xaxt="n", xlab="", ylab="", main="")
  title(main=colnames(mat_stats)[in_dex], line=-1.0)
  axis(1, at=1:(NROW(mat_stats)),
 labels=rownames(mat_stats))
}  # end for

reg_stats <- function(da_ta) {  # get regression
# Perform regression and get summary
  col_names <- colnames(da_ta)
  for_mula <-
    paste(col_names[2], col_names[1], sep="~")
  model_sum <- summary(lm(for_mula,
                        data=da_ta))
# Extract regression statistics
  with(model_sum, c(pval=coefficients[2, 4],
   adj_rsquared=adj.r.squared,
   fstat=fstatistic[1]))
}  # end reg_stats
# Apply reg_stats() to vector of std dev values
vec_sd <- seq(from=0.1, to=0.5, by=0.1)
names(vec_sd) <- paste0("sd=", vec_sd)
mat_stats <-
  t(sapply(vec_sd, function(std_dev) {
    set.seed(1121)  # initialize number generator
# Define explanatory (design) and response variables
    de_sign <- rnorm(100, mean=2)
    res_ponse <- (1 + 0.2*de_sign +
rnorm(NROW(de_sign), sd=std_dev))
    reg_stats(data.frame(de_sign, res_ponse))
    }))
# Plot in loop
par(mfrow=c(NCOL(mat_stats), 1))
for (in_dex in 1:NCOL(mat_stats)) {
  plot(mat_stats[, in_dex], type="l",
 xaxt="n", xlab="", ylab="", main="")
  title(main=colnames(mat_stats)[in_dex], line=-1.0)
  axis(1, at=1:(NROW(mat_stats)),
 labels=rownames(mat_stats))
}  # end for

# Set plot paramaters - margins and font scale
par(oma=c(1,0,1,0), mgp=c(2,1,0), mar=c(2,1,2,1), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2, 2))  # Plot 2x2 panels
plot(mod_el)  # Plot diagnostic scatterplots
plot(mod_el, which=2)  # Plot just Q-Q

library(lmtest)  # Load lmtest
# Perform Durbin-Watson test
lmtest::dwtest(mod_el)

x11(width=6, height=5)  # Open x11 for plotting
# Set plot parameters to reduce whitespace around plot
par(mar=c(5, 5, 2, 1), oma=c(0, 0, 0, 0))
# Add unit column to the design matrix
de_sign <- cbind(rep(1, NROW(de_sign)), de_sign)
# Calculate generalized inverse of the design matrix
design_inv <- MASS::ginv(de_sign)
# Calculate the influence matrix
influ_ence <- de_sign %*% design_inv
# Plot the leverage vector
or_der <- order(de_sign[, 2])
plot(x=de_sign[or_der, 2], y=diag(influ_ence)[or_der],
     type="l", lwd=3, col="blue",
     xlab="predictor", ylab="leverage",
     main="Leverage as Function of Predictor")

# Calculate the influence matrix
influ_ence <- de_sign %*% design_inv
# The influence matrix is idempotent
all.equal(influ_ence, influ_ence %*% influ_ence)

# Calculate covariance and standard deviations of fitted values
beta_s <- design_inv %*% res_ponse
fit_ted <- drop(de_sign %*% beta_s)
resid_uals <- drop(res_ponse - fit_ted)
deg_free <- (NROW(de_sign) - NCOL(de_sign))
var_resid <- sqrt(sum(resid_uals^2)/deg_free)
fit_covar <- var_resid*influ_ence
fit_sd <- sqrt(diag(fit_covar))
# Plot the standard deviations
fit_sd <- cbind(fitted=fit_ted, stddev=fit_sd)
fit_sd <- fit_sd[order(fit_ted), ]
plot(fit_sd, type="l", lwd=3, col="blue",
     xlab="Fitted Value", ylab="Standard Deviation",
     main="Standard Deviations of Fitted Values\nin Univariate Regression")

# Calculate response without random noise for univariate regression,
# equal to weighted sum over columns of de_sign
weight_s <- c(-1, 1)
res_ponse <- de_sign %*% weight_s
# Perform loop over different realizations of random noise
fit_ted <- lapply(1:50, function(it) {
  # Add random noise to response
  res_ponse <- res_ponse + rnorm(len_gth, sd=1.0)
  # Calculate fitted values using influence matrix
  influ_ence %*% res_ponse
})  # end lapply
fit_ted <- rutils::do_call(cbind, fit_ted)

x11(width=5, height=4)  # Open x11 for plotting
# Set plot parameters to reduce whitespace around plot
par(mar=c(5, 5, 2, 1), oma=c(0, 0, 0, 0))
# Plot fitted values
matplot(x=de_sign[,2], y=fit_ted,
type="l", lty="solid", lwd=1, col="blue",
xlab="predictor", ylab="fitted",
main="Fitted Values for Different Realizations
of Random Noise")
lines(x=de_sign[,2], y=res_ponse, col="red", lwd=4)
legend(x="topleft", # Add legend
       legend=c("response without noise", "fitted values"),
       title=NULL, inset=0.05, cex=0.8, lwd=6,
       lty=1, col=c("red", "blue"))

# Inverse of design matrix squared
design_2 <- MASS::ginv(crossprod(de_sign))
# Define new predictors
new_data <- (max(de_sign[, 2]) + 10*(1:5)/len_gth)
# Calculate the predicted values and standard errors
design_new <- cbind(rep(1, NROW(new_data)), new_data)
predic_tions <- cbind(
  prediction=drop(design_new %*% beta_s),
  stddev=diag(var_resid*sqrt(design_new %*% design_2 %*% t(design_new))))
# OR: Perform loop over new_data
predic_tions <- sapply(new_data, function(predic_tor) {
  predic_tor <- cbind(1, predic_tor)
  # Calculate predicted values
  predic_tion <- predic_tor %*% beta_s
  # Calculate standard deviation
  predict_sd <- var_resid*sqrt(predic_tor %*% design_2 %*% t(predic_tor))
  c(prediction=predic_tion, stddev=predict_sd)
})  # end sapply
predic_tions <- t(predic_tions)

# Prepare plot data
x_data <- c(de_sign[,2], new_data)
x_lim <- range(x_data)
y_data <- c(fit_ted, predic_tions[, 1])
# Calculate t-quantile
t_quant <- qt(pnorm(2), df=deg_free)
predict_low <- predic_tions[, 1]-t_quant*predic_tions[, 2]
predict_high <- predic_tions[, 1]+t_quant*predic_tions[, 2]
y_lim <- range(c(res_ponse, y_data, predict_low, predict_high))
# Plot the regression predictions
plot(x=x_data, y=y_data,
     xlim=x_lim, ylim=y_lim,
     type="l", lwd=3, col="blue",
     xlab="predictor", ylab="fitted or predicted",
     main="Predictions from Linear Regression")
points(x=de_sign[,2], y=res_ponse, col="blue")
points(x=new_data, y=predic_tions[, 1], pch=16, col="blue")
lines(x=new_data, y=predict_high, lwd=3, col="red")
lines(x=new_data, y=predict_low, lwd=3, col="green")
legend(x="topleft", # Add legend
       legend=c("predictions", "+2SD", "-2SD"),
       title=NULL, inset=0.05, cex=0.8, lwd=6,
       lty=1, col=c("blue", "red", "green"))

# Perform univariate regression
predic_tor <- de_sign[, 2]
mod_el <- lm(res_ponse ~ predic_tor)
# Perform prediction from regression
new_data <- data.frame(predic_tor=new_data)
predict_lm <- predict(object=mod_el,
  newdata=new_data, level=1-2*(1-pnorm(2)),
  interval="confidence")
predict_lm <- as.data.frame(predict_lm)
all.equal(predict_lm$fit, predic_tions[, 1])
all.equal(predict_lm$lwr, predict_low)
all.equal(predict_lm$upr, predict_high)
plot(res_ponse ~ predic_tor,
     xlim=range(predic_tor, new_data),
     ylim=range(res_ponse, predict_lm),
     xlab="predictor", ylab="fitted or predicted",
     main="Predictions from lm() Regression")

abline(mod_el, col="blue", lwd=3)
with(predict_lm, {
  points(x=new_data$predic_tor, y=fit, pch=16, col="blue")
  lines(x=new_data$predic_tor, y=lwr, lwd=3, col="green")
  lines(x=new_data$predic_tor, y=upr, lwd=3, col="red")
})  # end with
legend(x="topleft", # Add legend
       legend=c("predictions", "+2SD", "-2SD"),
       title=NULL, inset=0.05, cex=0.8, lwd=6,
       lty=1, col=c("blue", "red", "green"))

set.seed(1121)
library(lmtest)
# Spurious regression in unit root time series
de_sign <- cumsum(rnorm(100))  # Unit root time series
res_ponse <- cumsum(rnorm(100))
for_mula <- res_ponse ~ de_sign
mod_el <- lm(for_mula)  # Perform regression
# Summary indicates statistically significant regression
model_sum <- summary(mod_el)
model_sum$coeff
model_sum$r.squared
# Durbin-Watson test shows residuals are autocorrelated
dw_test <- dwtest(mod_el)
c(dw_test$statistic[[1]], dw_test$p.value)

par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # Set plot panels
plot(for_mula, xlab="", ylab="")  # Plot scatterplot using formula
title(main="Spurious Regression", line=-1)
# Add regression line
abline(mod_el, lwd=2, col="red")
plot(mod_el, which=2, ask=FALSE)  # Plot just Q-Q

set.seed(1121)  # initialize random number generator
# Define design matrix
n_rows <- 100
n_cols <- 5
de_sign <- sapply(rep(n_rows, n_cols), rnorm)
# Add column names
colnames(de_sign) <- paste0("col", 1:n_cols)
# Plot design matrix
# matplot(de_sign, type="l", lty="solid", lwd=3)
# Define the design weights
weight_s <- sample(3:(n_cols+2))
# Response equals linear form plus random noise
noise <- rnorm(n_rows, sd=5)
res_ponse <- (-1 + de_sign %*% weight_s + noise)

# Perform multivariate regression using lm()
mod_el <- lm(res_ponse ~ de_sign)
# Solve multivariate regression using matrix algebra
# Calculate de-meaned design matrix and response vector
design_zm <- t(t(de_sign) - colMeans(de_sign))
# de_sign <- apply(de_sign, 2, function(x) (x-mean(x)))
response_zm <- res_ponse - mean(res_ponse)
# Calculate the regression coefficients
beta_s <- MASS::ginv(design_zm) %*% response_zm
# Calculate the regression alpha
al_pha <- mean(res_ponse) - 
  sum(colSums(de_sign)*drop(beta_s))/n_rows
# Compare with coefficients from lm()
all.equal(coef(mod_el), c(al_pha, beta_s), check.attributes=FALSE)
# Compare with actual coefficients
all.equal(c(-1, weight_s), c(al_pha, beta_s), check.attributes=FALSE)

# Add intercept column to design matrix
de_sign <- cbind(rep(1, NROW(de_sign)), de_sign)
n_cols <- NCOL(de_sign)
# Add column name
colnames(de_sign)[1] <- "intercept"
# Calculate generalized inverse of the design matrix
design_inv <- MASS::ginv(de_sign)
# Calculate the regression coefficients
beta_s <- design_inv %*% res_ponse
# Perform multivariate regression without intercept term
mod_el <- lm(res_ponse ~ de_sign - 1)
all.equal(drop(beta_s), coef(mod_el), check.attributes=FALSE)

# Calculate fitted values from regression coefficients
fit_ted <- drop(de_sign %*% beta_s)
all.equal(fit_ted, mod_el$fitted.values, check.attributes=FALSE)
# Calculate the residuals
resid_uals <- drop(res_ponse - fit_ted)
all.equal(resid_uals, mod_el$residuals, check.attributes=FALSE)
# Residuals are orthogonal to de_sign columns (predictors)
sapply(resid_uals %*% de_sign, 
       all.equal, target=0)
# Residuals are orthogonal to the fitted values
all.equal(sum(resid_uals*fit_ted), target=0)
# Sum of residuals is equal to zero
all.equal(sum(resid_uals), target=0)

# Calculate the influence matrix
influ_ence <- de_sign %*% design_inv
# The influence matrix is idempotent
all.equal(influ_ence, influ_ence %*% influ_ence)
# Calculate fitted values using influence matrix
fit_ted <- drop(influ_ence %*% res_ponse)
all.equal(fit_ted, mod_el$fitted.values, check.attributes=FALSE)
# Calculate fitted values from regression coefficients
fit_ted <- drop(de_sign %*% beta_s)
all.equal(fit_ted, mod_el$fitted.values, check.attributes=FALSE)

# Calculate zero mean fitted values
design_zm <- t(t(de_sign) - colMeans(de_sign))
fitted_zm <- drop(design_zm %*% beta_s)
all.equal(fitted_zm, 
  mod_el$fitted.values - mean(res_ponse), 
  check.attributes=FALSE)
# Calculate the residuals
response_zm <- res_ponse - mean(res_ponse)
resid_uals <- drop(response_zm - fitted_zm)
all.equal(resid_uals, mod_el$residuals, 
  check.attributes=FALSE)
# Calculate the influence matrix
influence_zm <- design_zm %*% MASS::ginv(design_zm)
# Compare the fitted values
all.equal(fitted_zm, 
  drop(influence_zm %*% response_zm), 
  check.attributes=FALSE)

library(lmtest)  # Load lmtest
de_sign <- data.frame(  # Design matrix
  de_sign=1:30, omit_var=sin(0.2*1:30))
# Response depends on both predictors
res_ponse <- with(de_sign,
  0.2*de_sign + omit_var + 0.2*rnorm(30))
# Mis-specified regression only one predictor
mod_el <- lm(res_ponse ~ de_sign,
        data=de_sign)
model_sum <- summary(mod_el)
model_sum$coeff
model_sum$r.squared
# Durbin-Watson test shows residuals are autocorrelated
dwtest(mod_el)$p.value

par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # Set plot panels
plot(for_mula, data=de_sign)
abline(mod_el, lwd=2, col="red")
title(main="OVB Regression", line=-1)
plot(mod_el, which=2, ask=FALSE)  # Plot just Q-Q

# Regression model summary
model_sum <- summary(mod_el)
# Degrees of freedom of residuals
n_rows <- NROW(de_sign)
n_cols <- NCOL(de_sign)
deg_free <- (n_rows - n_cols)
all.equal(deg_free, model_sum$df[2])
# Variance of residuals
var_resid <- sum(resid_uals^2)/deg_free

# Inverse of design matrix squared
design_2 <- MASS::ginv(crossprod(de_sign))
# design_2 <- t(de_sign) %*% de_sign
# Variance of residuals
var_resid <- sum(resid_uals^2)/deg_free
# Calculate covariance matrix of betas
beta_covar <- var_resid*design_2
# Round(beta_covar, 3)
beta_sd <- sqrt(diag(beta_covar))
all.equal(beta_sd, model_sum$coeff[, 2], check.attributes=FALSE)
# Calculate t-values of betas
beta_tvals <- drop(beta_s)/beta_sd
all.equal(beta_tvals, model_sum$coeff[, 3], check.attributes=FALSE)
# Calculate two-sided p-values of betas
beta_pvals <- 2*pt(-abs(beta_tvals), df=deg_free)
all.equal(beta_pvals, model_sum$coeff[, 4], check.attributes=FALSE)
# The square of the generalized inverse is equal 
# to the inverse of the square
all.equal(MASS::ginv(crossprod(de_sign)), 
  design_inv %*% t(design_inv))

# Calculate the influence matrix
influ_ence <- de_sign %*% design_inv
# The influence matrix is idempotent
all.equal(influ_ence, influ_ence %*% influ_ence)

# Calculate covariance and standard deviations of fitted values
fit_covar <- var_resid*influ_ence
fit_sd <- sqrt(diag(fit_covar))
# Sort the standard deviations
fit_sd <- cbind(fitted=fit_ted, stddev=fit_sd)
fit_sd <- fit_sd[order(fit_ted), ]
# Plot the standard deviations
plot(fit_sd, type="l", lwd=3, col="blue",
     xlab="Fitted Value", ylab="Standard Deviation",
     main="Standard Deviations of Fitted Values\nin Multivariate Regression")

# Load time series of ETF percentage returns
re_turns <- rutils::etf_env$re_turns[, c("XLF", "XLE")]
re_turns <- na.omit(re_turns)
n_rows <- NROW(re_turns)
head(re_turns)
# Define regression formula
for_mula <- paste(colnames(re_turns)[1],
  paste(colnames(re_turns)[-1], collapse="+"),
  sep=" ~ ")
# Standard regression
mod_el <- lm(for_mula, data=re_turns)
model_sum <- summary(mod_el)
# Bootstrap of regression
set.seed(1121)  # initialize random number generator
boot_data <- sapply(1:100, function(x) {
  boot_sample <- sample.int(n_rows, replace=TRUE)
  mod_el <- lm(for_mula,
         data=re_turns[boot_sample, ])
  mod_el$coefficients
})  # end sapply
# Means and standard errors from regression
model_sum$coefficients
# Means and standard errors from bootstrap
dim(boot_data)
t(apply(boot_data, MARGIN=1,
function(x) c(mean=mean(x), std_error=sd(x))))

# New data predictor is a data frame or row vector
set.seed(1121)
new_data <- data.frame(matrix(c(1, rnorm(5)), nr=1))
col_names <- colnames(de_sign)
colnames(new_data) <- col_names
new_datav <- as.matrix(new_data)
predic_tion <- drop(new_datav %*% beta_s)
std_dev <- drop(sqrt(
    new_datav %*% beta_covar %*% t(new_datav)))

# Create formula from text string
for_mula <- paste0("res_ponse ~ ", 
  paste(colnames(de_sign), collapse=" + "), " - 1")
# Specify multivariate regression using formula
mod_el <- lm(for_mula, 
     data=data.frame(cbind(res_ponse, de_sign)))
model_sum <- summary(mod_el)
# Predict from lm object
predict_lm <- predict.lm(object=mod_el, newdata=new_data, 
   interval="confidence", level=1-2*(1-pnorm(2)))
# Calculate t-quantile
t_quant <- qt(pnorm(2), df=deg_free)
predict_high <- (predic_tion + t_quant*std_dev)
predict_low <- (predic_tion - t_quant*std_dev)
# Compare with matrix calculations
all.equal(predict_lm[1, "fit"], predic_tion)
all.equal(predict_lm[1, "lwr"], predict_low)
all.equal(predict_lm[1, "upr"], predict_high)

# TSS = ESS + RSS
t_ss <- sum((res_ponse-mean(res_ponse))^2)
e_ss <- sum((fit_ted-mean(fit_ted))^2)
r_ss <- sum(resid_uals^2)
all.equal(t_ss, e_ss + r_ss)

# Set regression attribute for intercept
attributes(mod_el$terms)$intercept <- 1
# Regression summary
model_sum <- summary(mod_el)
# Regression R-squared
r_squared <- e_ss/t_ss
all.equal(r_squared, model_sum$r.squared)
# Correlation between response and fitted values
cor_fitted <- drop(cor(res_ponse, fit_ted))
# Squared correlation between response and fitted values
all.equal(cor_fitted^2, r_squared)

n_rows <- NROW(de_sign)
n_cols <- NCOL(de_sign)
# Degrees of freedom of residuals
deg_free <- (n_rows - n_cols)
# Adjusted R-squared
r_squared_adj <- 
  (1-sum(resid_uals^2)/deg_free/var(res_ponse))
# Compare adjusted R-squared from lm()
all.equal(drop(r_squared_adj), 
  model_sum$adj.r.squared)

x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
# Plot three curves in loop
deg_free <- c(3, 5, 9)  # Degrees of freedom
col_ors <- c("black", "red", "blue", "green")
for (in_dex in 1:NROW(deg_free)) {
curve(expr=df(x, df1=deg_free[in_dex], df2=3),
xlim=c(0, 4), xlab="", ylab="", lwd=2,
col=col_ors[in_dex], add=as.logical(in_dex-1))
}  # end for

# Add title
title(main="F-Distributions", line=0.5)
# Add legend
lab_els <- paste("df", deg_free, sep="=")
legend("topright", inset=0.05, title="degrees of freedom",
       lab_els, cex=0.8, lwd=2, lty=1,
       col=col_ors)

sigma_x <- var(rnorm(n_rows))
sigma_y <- var(rnorm(n_rows))
f_ratio <- sigma_x/sigma_y
# Cumulative probability for q = f_ratio
pf(f_ratio, n_rows-1, n_rows-1)
# p-value for f_ratios
1-pf((10:20)/10, n_rows-1, n_rows-1)

# F-statistic from lm()
model_sum$fstatistic
# Degrees of freedom of residuals
deg_free <- (n_rows - n_cols)
# F-statistic from ESS and RSS
f_stat <- (e_ss/(n_cols-1))/(r_ss/deg_free)
all.equal(f_stat, model_sum$fstatistic[1], check.attributes=FALSE)
# p-value of F-statistic
1-pf(q=f_stat, df1=n_cols-1, df2=n_rows-n_cols)

library(HighFreq)
# Select ETF symbols
sym_bols <- c("IEF", "DBC", "XLU", "XLF", "XLP", "XLI")
# Calculate ETF prices and simple returns (not percentage)
price_s <- rutils::etf_env$price_s[, sym_bols]
price_s <- xts:::na.locf.xts(price_s)
price_s <- xts:::na.locf.xts(price_s, fromLast=TRUE)
date_s <- index(price_s)
re_turns <- rutils::diff_it(price_s)
# Center (de-mean) and scale the returns
re_turns <- t(t(re_turns) - colMeans(re_turns))
re_turns <- t(t(re_turns) / sqrt(colSums(re_turns^2)/(NROW(re_turns)-1)))
re_turns <- xts(re_turns, date_s)
# Alternative center (de-mean) and scale the returns
# re_turns <- scale(re_turns, center=TRUE, scale=TRUE)
# re_turns <- xts(re_turns, date_s)
# or
# re_turns <- lapply(re_turns, function(x) {x - sum(x)/NROW(re_turns)})
# re_turns <- rutils::do_call(cbind, re_turns)
# re_turns <- apply(re_turns, 2, scale)
# Covariance matrix and variance vector of returns
cov_mat <- cov(re_turns)
vari_ance <- diag(cov_mat)
cor_mat <- cor(re_turns)
# cov_mat <- crossprod(re_turns) / (NROW(re_turns)-1)
# cor_mat <- cov_mat / sqrt(vari_ance)
# cor_mat <- t(t(cor_mat) / sqrt(vari_ance))
# Reorder correlation matrix based on clusters
library(corrplot)
or_der <- corrMatOrder(cor_mat,
        order="hclust",
        hclust.method="complete")
cor_mat <- cor_mat[or_der, or_der]
# Plot the correlation matrix
col_ors <- colorRampPalette(c("red", "white", "blue"))
corrplot(cor_mat, title="ETF Correlation Matrix",
    tl.col="black", tl.cex=0.8, mar=c(0,0,1,0),
    method="square", col=col_ors(8),
    cl.offset=0.75, cl.cex=0.7,
    cl.align.text="l", cl.ratio=0.25)
# draw rectangles on the correlation matrix plot
corrRect.hclust(cor_mat, k=NROW(cor_mat) %/% 2,
          method="complete", col="red")

# Plot the correlation matrix
col_ors <- colorRampPalette(c("red", "white", "blue"))
corrplot(cor_mat, title="Correlation Matrix",
    tl.col="black", tl.cex=0.8, mar = c(0,0,1,0),
    method="square", col=col_ors(NCOL(cor_mat)),
    cl.offset=0.75, cl.cex=0.7,
    cl.align.text="l", cl.ratio=0.25)
# draw rectangles on the correlation matrix plot
corrRect.hclust(cor_mat, k=NCOL(cor_mat) %/% 2,
    method="complete", col="red")

# create initial vector of portfolio weights
n_weights <- NROW(sym_bols)
weight_s <- rep(1/sqrt(n_weights), n_weights)
names(weight_s) <- sym_bols
# objective function equal to minus portfolio variance
object_ive <- function(weight_s, re_turns) {
  portf_rets <- re_turns %*% weight_s
  -sum(portf_rets^2) +
    1e7*(1 - sum(weight_s^2))^2
}  # end object_ive
# objective for equal weight portfolio
object_ive(weight_s, re_turns)
# Compare speed of vector multiplication methods
summary(microbenchmark(
  trans_pose=(t(re_turns[, 1]) %*% re_turns[, 1]),
  s_um=sum(re_turns[, 1]^2),
  times=10))[, c(1, 4, 5)]

# Find weights with maximum variance
optim_run <- optim(par=weight_s,
  fn=object_ive,
  re_turns=re_turns,
  method="L-BFGS-B",
  upper=rep(10.0, n_weights),
  lower=rep(-10.0, n_weights))
# optimal weights and maximum variance
weight_s <- optim_run$par
-object_ive(weight_s, re_turns)
# Plot first principal component weights
barplot(weight_s, names.arg=names(weight_s),
  xlab="", ylab="",
  main="First Principal Component Weights")

# pc1 weights and returns
weights_1 <- weight_s
pc_1 <- re_turns %*% weights_1
# Redefine objective function
object_ive <- function(weight_s, re_turns) {
  portf_rets <- re_turns %*% weight_s
  -sum(portf_rets^2) +
    1e7*(1 - sum(weight_s^2))^2 +
    1e7*(sum(weights_1*weight_s))^2
}  # end object_ive
# Find second PC weights using parallel DEoptim
optim_run <- DEoptim::DEoptim(fn=object_ive,
  upper=rep(10, NCOL(re_turns)),
  lower=rep(-10, NCOL(re_turns)),
  re_turns=re_turns, control=list(parVar="weights_1",
    trace=FALSE, itermax=1000, parallelType=1))

# pc2 weights and returns
weights_2 <- optim_run$optim$bestmem
names(weights_2) <- colnames(re_turns)
sum(weights_2^2)
sum(weights_1*weights_2)
# Plot second principal component loadings
barplot(weights_2, names.arg=names(weights_2),
  xlab="", ylab="",
  main="Second Principal Component Loadings")

# Calculate eigenvectors and eigenvalues
ei_gen <- eigen(cov_mat)
ei_gen$vectors
weights_1
weights_2
ei_gen$values[1]
var(pc_1)
(cov_mat %*% weights_1) / weights_1
ei_gen$values[2]
var(pc_2)
(cov_mat %*% weights_2) / weights_2
sum(vari_ance)
sum(ei_gen$values)
barplot(ei_gen$values, # Plot eigenvalues
  names.arg=paste0("PC", 1:n_weights),
  las=3, xlab="", ylab="", main="Principal Component Variances")

# Eigen decomposition of covariance matrix
re_turns <- rutils::diff_it(price_s)
cov_mat <- cov(re_turns)
ei_gen <- eigen(cov_mat)
# Perform PCA without scaling
pc_a <- prcomp(re_turns, scale=FALSE)
# Compare outputs
all.equal(ei_gen$values, pc_a$sdev^2)
all.equal(abs(ei_gen$vectors), abs(pc_a$rotation),
    check.attributes=FALSE)
# Eigen decomposition of correlation matrix
cor_mat <- cor(re_turns)
ei_gen <- eigen(cor_mat)
# Perform PCA with scaling
pc_a <- prcomp(re_turns, scale=TRUE)
# Compare outputs
all.equal(ei_gen$values, pc_a$sdev^2)
all.equal(abs(ei_gen$vectors), abs(pc_a$rotation),
    check.attributes=FALSE)

# Redefine objective function to minimize variance
object_ive <- function(weight_s, re_turns) {
  portf_rets <- re_turns %*% weight_s
  sum(portf_rets^2) +
    1e7*(1 - sum(weight_s^2))^2
}  # end object_ive
# Find highest order PC weights using parallel DEoptim
optim_run <- DEoptim::DEoptim(fn=object_ive,
  upper=rep(10, NCOL(re_turns)),
  lower=rep(-10, NCOL(re_turns)),
  re_turns=re_turns, control=list(trace=FALSE,
    itermax=1000, parallelType=1))
# pc6 weights and returns
weights_6 <- optim_run$optim$bestmem
names(weights_6) <- colnames(re_turns)
sum(weights_6^2)
sum(weights_1*weights_6)
# Calculate objective function
object_ive(weights_6, re_turns)
object_ive(ei_gen$vectors[, 6], re_turns)

# Plot highest order principal component loadings
barplot(weights_6, names.arg=names(weights_2),
  xlab="", ylab="",
  main="Highest Order Principal Component Loadings")

# Perform principal component analysis PCA
pc_a <- prcomp(re_turns, scale=TRUE)
# Plot standard deviations of principal components
barplot(pc_a$sdev,
  names.arg=colnames(pc_a$rotation),
  las=3, xlab="", ylab="",
  main="Scree Plot: Volatilities of Principal Components
  of Stock Returns")

# Calculate principal component loadings (weights)
pc_a$rotation
# Plot barplots with PCA weights in multiple panels
par(mfrow=c(n_weights/2, 2))
par(mar=c(2, 2, 2, 1), oma=c(0, 0, 0, 0))
for (or_der in 1:n_weights) {
  barplot(pc_a$rotation[, or_der],
  las=3, xlab="", ylab="", main="")
  title(paste0("PC", or_der), line=-2.0,
  col.main="red")
}  # end for

# Calculate products of principal component time series
round(t(pc_a$x) %*% pc_a$x, 2)
# Calculate principal component time series from re_turns
pca_rets <- xts(re_turns %*% pc_a$rotation,
          order.by=date_s)
round(cov(pca_rets), 3)
all.equal(coredata(pca_rets), pc_a$x, check.attributes=FALSE)
pca_ts <- xts:::cumsum.xts(pca_rets)
# Plot principal component time series in multiple panels
par(mfrow=c(n_weights/2, 2))
par(mar=c(2, 2, 0, 1), oma=c(0, 0, 0, 0))
ra_nge <- range(pca_ts)
for (or_der in 1:n_weights) {
  plot.zoo(pca_ts[, or_der],
     ylim=ra_nge,
     xlab="", ylab="")
  title(paste0("PC", or_der), line=-2.0)
}  # end for

par(mfrow=c(n_weights/2, 2))
par(mar=c(2, 2, 0, 1), oma=c(0, 0, 0, 0))
# Invert all the principal component time series
pca_rets <- re_turns %*% pc_a$rotation
sol_ved <- pca_rets %*% solve(pc_a$rotation)
all.equal(coredata(re_turns), sol_ved)
# Invert first 3 principal component time series
sol_ved <- pca_rets[, 1:3] %*% solve(pc_a$rotation)[1:3, ]
sol_ved <- xts::xts(sol_ved, date_s)
sol_ved <- xts:::cumsum.xts(sol_ved)
cum_returns <- xts:::cumsum.xts(re_turns)
# Plot the solved returns
for (sym_bol in sym_bols) {
  plot.zoo(
    cbind(cum_returns[, sym_bol], sol_ved[, sym_bol]),
    plot.type="single", col=c("black", "blue"), xlab="", ylab="")
  legend(x="topleft", bty="n",
   legend=paste0(sym_bol, c("", " solved")),
   title=NULL, inset=0.0, cex=1.0, lwd=6,
   lty=1, col=c("black", "blue"))
}  # end for
