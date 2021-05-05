library(dygraphs)
# Calculate volume-weighted average price
oh_lc <- rutils::etf_env$VTI
v_wap <- TTR::VWAP(price=quantmod::Cl(oh_lc),
    volume=quantmod::Vo(oh_lc), n=20)
# Add VWAP to OHLC data
da_ta <- cbind(oh_lc[, 1:4], v_wap)["2009-01/2009-04"]
# Create dygraphs object
dy_graph <- dygraphs::dygraph(da_ta)
# Increase line width and color
dy_graph <- dygraphs::dyOptions(dy_graph,
  colors="red", strokeWidth=3)
# Convert dygraphs object to candlestick plot
dy_graph <- dygraphs::dyCandlestick(dy_graph)
# Render candlestick plot
dy_graph
# Candlestick plot using pipes syntax
dygraphs::dygraph(da_ta) %>% dyCandlestick() %>%
  dyOptions(colors="red", strokeWidth=3)
# Candlestick plot without using pipes syntax
dygraphs::dyCandlestick(dygraphs::dyOptions(dygraphs::dygraph(da_ta),
  colors="red", strokeWidth=3))

# Create candlestick plot with background shading
in_dic <- (Cl(da_ta) > da_ta[, "VWAP"])
whi_ch <- which(rutils::diff_it(in_dic) != 0)
in_dic <- rbind(first(in_dic), in_dic[whi_ch, ], last(in_dic))
date_s <- index(in_dic)
in_dic <- ifelse(drop(coredata(in_dic)), "lightgreen", "antiquewhite")
# Create dygraph object without rendering it
dy_graph <- dygraphs::dygraph(da_ta) %>% dyCandlestick() %>%
  dyOptions(colors="red", strokeWidth=3)
# Add shading
for (i in 1:(NROW(in_dic)-1)) {
    dy_graph <- dy_graph %>%
dyShading(from=date_s[i], to=date_s[i+1], color=in_dic[i])
}  # end for
# Render the dygraph object
dy_graph

library(dygraphs)
# Prepare VTI and IEF prices
price_s <- cbind(Cl(rutils::etf_env$VTI), Cl(rutils::etf_env$IEF))
price_s <- na.omit(price_s)
col_names <- rutils::get_name(colnames(price_s))
colnames(price_s) <- col_names
# dygraphs plot with two y-axes
library(dygraphs)
dygraphs::dygraph(price_s, main=paste(col_names, collapse=" and ")) %>%
  dyAxis(name="y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis(name="y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", strokeWidth=2, col="red") %>%
  dySeries(name=col_names[2], axis="y2", strokeWidth=2, col="blue")

# Calculate log OHLC prices
sym_bol <- "VTI"
# oh_lc <- rutils::etf_env$VTI
oh_lc <- get(sym_bol, rutils::etf_env)
oh_lc[, 1:4] <- log(oh_lc[, 1:4])
n_rows <- NROW(oh_lc)
op_en <- quantmod::Op(oh_lc)
clos_e <- quantmod::Cl(oh_lc)
colnames(clos_e) <- sym_bol
vol_ume <- quantmod::Vo(oh_lc)
re_turns <- rutils::diff_it(clos_e)
cum_rets <- cumsum(re_turns)
# Define aggregation interval and calculate VWAP
look_back <- 200
v_wap <- rutils::roll_sum(x_ts=cum_rets*vol_ume, look_back=look_back)
volume_roll <- rutils::roll_sum(x_ts=vol_ume, look_back=look_back)
v_wap <- v_wap/volume_roll
v_wap[is.na(v_wap)] <- 0

# Plot VTI and VWAP using dygraphs
weal_th <- cbind(cum_rets, v_wap)
colnames(weal_th) <- c(sym_bol, "VWAP")
dygraphs::dygraph(weal_th, main="VTI and VWAP") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=3)
# Plot VTI and VWAP using quantmod
x11(width=6, height=5)
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("blue", "red")
quantmod::chart_Series(x=weal_th, name="VTI and VWAP",
       theme=plot_theme)
legend("top", legend=c("VTI", "VWAP"), lty=1, lwd=6, cex=0.9,
 bg="white", col=c("blue", "red"), bty="n")

# Calculate VWAP indicator
in_dic <- sign(cum_rets - v_wap)
# Calculate positions as lagged indicator
position_s <- rutils::lag_it(in_dic)
# Calculate daily profits and losses of strategy
pnl_s <- re_turns*position_s
colnames(pnl_s) <- "Strategy"
cum_pnls <- cumsum(pnl_s)
weal_th <- cbind(cum_rets, cum_pnls, v_wap)
colnames(weal_th) <- c(sym_bol, "Strategy", "VWAP")
# Annualized Sharpe ratios of VTI and VWAP strategy
sharp_e <- sqrt(252)*sapply(cbind(re_turns, pnl_s), function (x) mean(x)/sd(x))
# Calculate index for background shading
in_dic <- (cum_rets > v_wap)
whi_ch <- which(rutils::diff_it(in_dic) != 0)
in_dic <- rbind(first(in_dic), in_dic[whi_ch, ], last(in_dic))
date_s <- index(in_dic)
in_dic <- ifelse(drop(coredata(in_dic)), "lightgreen", "antiquewhite")

# Create dygraph object without rendering it
dy_graph <- dygraphs::dygraph(weal_th, main=paste("VWAP Crossover Strategy, Sharpe", paste(paste(names(sharp_e), round(sharp_e, 3), sep="="), collapse=", "))) %>%
  dyOptions(colors=c("blue", "red", "purple"), strokeWidth=2)
# Add shading
for (i in 1:(NROW(in_dic)-1)) {
    dy_graph <- dy_graph %>%
dyShading(from=date_s[i], to=date_s[i+1], color=in_dic[i])
}  # end for
# Render the dygraph object
dy_graph
# Plot VTI and VWAP strategy using quantmod
quantmod::chart_Series(x=cbind(cum_rets, cum_pnls),
  name="VWAP Crossover Strategy for VTI", theme=plot_theme)
add_TA(position_s > 0, on=-1, col="lightgreen", border="lightgreen")
add_TA(position_s < 0, on=-1, col="lightgrey", border="lightgrey")
legend("top", legend=c(sym_bol, "VWAP strategy"), lty=1, lwd=6,
 cex=0.9, inset=0.1, bg="white", col=c("blue", "red"), bty="n")

# Calculate positions from lagged indicator
lagg <- 2
in_dic <- sign(cum_rets - v_wap)
indic_sum <- roll::roll_sum(in_dic, width=lagg)
indic_sum[1:lagg] <- 0
position_s <- rep(NA_integer_, NROW(clos_e))
position_s[1] <- 0
position_s <- ifelse(indic_sum == lagg, 1, position_s)
position_s <- ifelse(indic_sum == (-lagg), -1, position_s)
position_s <- zoo::na.locf(position_s, na.rm=FALSE)
# Lag the positions to trade in next period
position_s <- rutils::lag_it(position_s, lagg=1)
# Calculate PnLs of lagged strategy
pnl_s <- re_turns*position_s
colnames(pnl_s) <- "Strategy"

cum_pnls_lag <- cumsum(pnl_s)
weal_th <- cbind(cum_pnls, cum_pnls_lag)
colnames(weal_th) <- c("Strategy", "Strategy_lag")
# Annualized Sharpe ratios of VWAP strategies
sharp_e <- sqrt(252)*sapply(cbind(re_turns, pnl_s),
  function (x) mean(x)/sd(x))
# Plot both strategies
dygraphs::dygraph(weal_th, main=paste("VWAP Crossover Strategy, Sharpe", paste(paste(names(sharp_e), round(sharp_e, 3), sep="="), collapse=", "))) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=3)

# Calculate fast and slow VWAPs
vwap_fast <- TTR::VWAP(cum_rets, volume=vol_ume, n=20)
vwap_fast[1:20] <- 0
vwap_slow <- TTR::VWAP(cum_rets, volume=vol_ume, n=200)
vwap_slow[1:200] <- 0
# Calculate VWAP indicator
in_dic <- sign(vwap_fast - vwap_slow)
# Calculate positions as lagged indicator
position_s <- rutils::lag_it(in_dic)
# Calculate daily profits and losses of strategy
pnl_s <- re_turns*position_s
colnames(pnl_s) <- "Strategy"
cum_pnls <- cumsum(pnl_s)
weal_th <- cbind(cum_rets, cum_pnls, vwap_fast, vwap_slow)
colnames(weal_th) <- c(sym_bol, "Strategy", "VWAP_fast", "VWAP_slow")
# Annualized Sharpe ratios of VTI and VWAP strategy
sharp_e <- sqrt(252)*sapply(cbind(re_turns, pnl_s),
  function (x) mean(x)/sd(x))
# Calculate index for background shading
in_dic <- (vwap_fast > vwap_slow)
whi_ch <- which(rutils::diff_it(in_dic) != 0)
in_dic <- rbind(first(in_dic), in_dic[whi_ch, ], last(in_dic))
date_s <- index(in_dic)
in_dic <- ifelse(drop(coredata(in_dic)), "lightgreen", "antiquewhite")

# Create dygraph object without rendering it
dy_graph <- dygraphs::dygraph(weal_th, main=paste("VWAP Crossover Strategy, Sharpe", paste(paste(names(sharp_e), round(sharp_e, 3), sep="="), collapse=", "))) %>%
  dyOptions(colors=c("blue", "red", "purple", "lightpurple"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Add shading
for (i in 1:(NROW(in_dic)-1)) {
    dy_graph <- dy_graph %>%
dyShading(from=date_s[i], to=date_s[i+1], color=in_dic[i])
}  # end for
# Render the dygraph object
dy_graph

# VTI percentage returns
re_turns <- na.omit(rutils::etf_env$re_turns$VTI)
n_rows <- NROW(re_turns)
# Define end points
end_p <- 1:NROW(re_turns)
# Start points are multi-period lag of end_p
look_back <- 11
start_p <- c(rep_len(0, look_back-1), end_p[1:(n_rows-look_back+1)])
# Calculate rolling variance in sapply() loop - takes very long
vari_ance <- sapply(1:n_rows, function(in_dex) {
  ret_s <- re_turns[start_p[in_dex]:end_p[in_dex]]
  sum((ret_s - mean(ret_s))^2)
}) / (look_back-1)  # end sapply
# Use only vectorized functions
rolling_rets <- cumsum(re_turns)
rolling_rets <- (rolling_rets -
  c(rep_len(0, look_back), rolling_rets[1:(n_rows-look_back)]))
rolling_rets2 <- cumsum(re_turns^2)
rolling_rets2 <- (rolling_rets2 -
  c(rep_len(0, look_back), rolling_rets2[1:(n_rows-look_back)]))
vari_ance2 <- (rolling_rets2 - rolling_rets^2/look_back)/(look_back-1)
all.equal(vari_ance[-(1:look_back)], as.numeric(vari_ance2)[-(1:look_back)])
# Same, using package rutils
rolling_rets <- rutils::roll_sum(re_turns, look_back=look_back)
rolling_rets2 <- rutils::roll_sum(re_turns^2, look_back=look_back)
vari_ance2 <- (rolling_rets2 - rolling_rets^2/look_back)/(look_back-1)
# Coerce vari_ance into xts
tail(vari_ance)
class(vari_ance)
vari_ance <- xts(vari_ance, order.by=index(re_turns))
colnames(vari_ance) <- "VTI.variance"
head(vari_ance)

# Calculate rolling VTI variance using package roll
library(roll)  # Load roll
vari_ance <- roll::roll_var(re_turns, width=look_back)
colnames(vari_ance) <- "VTI.variance"
head(vari_ance)
sum(is.na(vari_ance))
vari_ance[1:(look_back-1)] <- 0
# Benchmark calculation of rolling variance
library(microbenchmark)
summary(microbenchmark(
  roll_sapply=sapply(2:n_rows, function(in_dex) {
    ret_s <- re_turns[start_p[in_dex]:end_p[in_dex]]
    sum((ret_s - mean(ret_s))^2)
  }),
  ro_ll=roll::roll_var(re_turns, width=look_back),
  times=10))[, c(1, 4, 5)]

# Calculate EWMA VTI variance using filter()
look_back <- 51
weight_s <- exp(-0.1*1:look_back)
weight_s <- weight_s/sum(weight_s)
vari_ance <- stats::filter(re_turns^2, filter=weight_s, sides=1)
vari_ance[1:(look_back-1)] <- vari_ance[look_back]
class(vari_ance)
vari_ance <- as.numeric(vari_ance)
x_ts <- xts:::xts(sqrt(vari_ance), order.by=index(re_turns))
# Plot EWMA standard deviation
chart_Series(x_ts, name="EWMA standard deviation")
dygraphs::dygraph(x_ts, main="EWMA standard deviation")

# Calculate rolling VTI variance using package roll
library(roll)  # Load roll
vari_ance <- roll::roll_var(re_turns,
  weights=rev(weight_s), width=look_back)
colnames(vari_ance) <- "VTI.variance"
class(vari_ance)
head(vari_ance)
sum(is.na(vari_ance))
vari_ance[1:(look_back-1)] <- 0

x11(width=6, height=5)
# VTI percentage returns
re_turns <- na.omit(rutils::etf_env$re_turns$VTI)
# Calculate rolling VTI variance using package roll
look_back <- 22
vari_ance <- roll::roll_var(re_turns, width=look_back)
vari_ance[1:(look_back-1)] <- 0
colnames(vari_ance) <- "VTI.variance"
# Number of look_backs that fit over re_turns
n_rows <- NROW(re_turns)
n_agg <- n_rows %/% look_back
# Define end_p with beginning stub
end_p <- c(0, n_rows-look_back*n_agg + (0:n_agg)*look_back)
n_rows <- NROW(end_p)
# Subset vari_ance to end_p
vari_ance <- vari_ance[end_p]
# Plot autocorrelation function
rutils::plot_acf(vari_ance, lag=10, main="ACF of Variance")
# Plot partial autocorrelation
pacf(vari_ance, lag=10, main="PACF of Variance", ylab=NA)

# Define GARCH parameters
om_ega <- 0.01 ; al_pha <- 0.2
be_ta <- 0.79 ; n_rows <- 1000
re_turns <- numeric(n_rows)
vari_ance <- numeric(n_rows)
vari_ance[1] <- om_ega/(1-al_pha-be_ta)
re_turns[1] <- rnorm(1, sd=sqrt(vari_ance[1]))
# Simulate GARCH model
set.seed(1121)  # Reset random numbers
for (i in 2:n_rows) {
  re_turns[i] <- rnorm(n=1, sd=sqrt(vari_ance[i-1]))
  vari_ance[i] <- om_ega + al_pha*re_turns[i]^2 +
    be_ta*vari_ance[i-1]
}  # end for

x11(width=5, height=3.5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0))
# Plot GARCH cumulative returns
plot(cumsum(re_turns/100), t="l",
  lwd=2, col="blue", xlab="", ylab="",
  main="GARCH cumulative returns")
# Plot dygraphs GARCH standard deviation
date_s <- seq.Date(from=Sys.Date()-n_rows+1,
  to=Sys.Date(), length.out=n_rows)
x_ts <- xts:::xts(cumsum(re_turns/100), order.by=date_s)
dygraphs::dygraph(x_ts, main="GARCH cumulative returns")
# Plot GARCH standard deviation
plot(sqrt(vari_ance), t="l",
  col="blue", xlab="", ylab="",
  main="GARCH standard deviation")
# Plot dygraphsGARCH standard deviation
x_ts <- xts:::xts(sqrt(vari_ance), order.by=date_s)
dygraphs::dygraph(x_ts, main="GARCH standard deviation")

# Define GARCH parameters
om_ega <- 0.0001 ; al_pha <- 0.5
be_ta <- 0.1 ; n_rows <- 10000
re_turns <- numeric(n_rows)
vari_ance <- numeric(n_rows)
vari_ance[1] <- om_ega/(1-al_pha-be_ta)
re_turns[1] <- rnorm(1, sd=sqrt(vari_ance[1]))
# Simulate GARCH model
set.seed(1121)  # Reset random numbers
for (i in 2:n_rows) {
  re_turns[i] <- rnorm(n=1, sd=sqrt(vari_ance[i-1]))
  vari_ance[i] <- om_ega + al_pha*re_turns[i]^2 +
    be_ta*vari_ance[i-1]
}  # end for
# Calculate kurtosis of GARCH returns
moments::moment(re_turns, order=4) /
  moments::moment(re_turns, order=2)^2
# Perform Jarque-Bera test of normality
tseries::jarque.bera.test(re_turns)

# Plot histogram of GARCH returns
histo_gram <- hist(re_turns, col="lightgrey",
  xlab="returns", breaks=200, xlim=c(-0.05, 0.05),
  ylab="frequency", freq=FALSE,
  main="GARCH returns histogram")
lines(density(re_turns, adjust=1.5),
lwd=3, col="blue")
optim_fit <- MASS::fitdistr(re_turns,
  densfun="t", df=2, lower=c(-1, 1e-7))
lo_cation <- optim_fit$estimate[1]
scal_e <- optim_fit$estimate[2]
curve(expr=dt((x-lo_cation)/scal_e, df=2)/scal_e,
  type="l", xlab="", ylab="", lwd=3,
  col="red", add=TRUE)
legend("topright", inset=0.05, bty="n",
 leg=c("density", "t-distr w/ 2 dof"),
 lwd=6, lty=1, col=c("blue", "red"))

# use fixed notation instead of exponential notation
options(scipen=999)
library(fGarch)
# Fit returns into GARCH
garch_fit <- fGarch::garchFit(data=re_turns)
# Fitted GARCH parameters
round(garch_fit@fit$coef, 5)
# Actual GARCH parameters
round(c(mu=mean(re_turns), omega=om_ega,
  alpha=al_pha, beta=be_ta), 5)

# Plot GARCH fitted standard deviation
plot(sqrt(garch_fit@fit$series$h), t="l",
  col="blue", xlab="", ylab="",
  main="GARCH fitted standard deviation")

# Specify GARCH model
garch_spec <- fGarch::garchSpec(
  model=list(omega=om_ega, alpha=al_pha, beta=be_ta))
# Simulate GARCH model
garch_sim <-
  fGarch::garchSim(spec=garch_spec, n=n_rows)
re_turns <- as.numeric(garch_sim)
# Calculate kurtosis of GARCH returns
moments::moment(re_turns, order=4) /
  moments::moment(re_turns, order=2)^2
# Perform Jarque-Bera test of normality
tseries::jarque.bera.test(re_turns)
# Plot histogram of GARCH returns
histo_gram <- hist(re_turns, col="lightgrey",
  xlab="returns", breaks=200, xlim=c(-0.05, 0.05),
  ylab="frequency", freq=FALSE,
  main="GARCH returns histogram")
lines(density(re_turns, adjust=1.5),
lwd=3, col="blue")

# Fit t-distribution into GARCH returns
optim_fit <- MASS::fitdistr(re_turns,
  densfun="t", df=2, lower=c(-1, 1e-7))
lo_cation <- optim_fit$estimate[1]
scal_e <- optim_fit$estimate[2]
curve(expr=dt((x-lo_cation)/scal_e, df=2)/scal_e,
  type="l", xlab="", ylab="", lwd=3,
  col="red", add=TRUE)
legend("topright", inset=0.05, bty="n",
 leg=c("density", "t-distr w/ 2 dof"),
 lwd=6, lty=1, col=c("blue", "red"))

library(HighFreq)  # Load HighFreq
# Minutely SPY returns (unit per minute) single day
# Minutely SPY volatility (unit per minute)
re_turns <- rutils::diff_it(log(SPY["2012-02-13", 4]))
sd(re_turns)
# SPY returns multiple days (includes overnight jumps)
re_turns <- rutils::diff_it(log(SPY[, 4]))
sd(re_turns)
# Table of time intervals - 60 second is most frequent
in_dex <- rutils::diff_it(.index(SPY))
table(in_dex)
# SPY returns divided by the overnight time intervals (unit per second)
re_turns <- re_turns / in_dex
re_turns[1] <- 0
# Minutely SPY volatility scaled to unit per minute
60*sd(re_turns)

library(HighFreq)  # Load HighFreq
# Minutely OHLC SPY prices aggregated to daily prices
SPY_daily <- rutils::to_period(oh_lc=HighFreq::SPY, period="days")
# Daily SPY volatility from daily returns
sd(rutils::diff_it(log(SPY_daily[, 4])))
# Minutely SPY returns (unit per minute)
re_turns <- rutils::diff_it(log(SPY[, 4]))
# Minutely SPY volatility scaled to daily interval
sqrt(6.5*60)*sd(re_turns)
# Minutely SPY returns with overnight scaling (unit per second)
re_turns <- rutils::diff_it(log(SPY[, 4]))
in_dex <- rutils::diff_it(.index(SPY))
re_turns <- re_turns / in_dex
re_turns[1] <- 0
# Daily SPY volatility from minutely returns
sqrt(6.5*60)*60*sd(re_turns)
# Daily SPY volatility
# Including extra time over weekends and holidays
24*60*60*sd(rutils::diff_it(log(SPY_daily[, 4]))[-1] /
    rutils::diff_it(.index(SPY_daily))[-1])

# Calculate SPY prices adjusted for overnight jumps
price_s <- log(as.numeric(Cl(HighFreq::SPY[, 4])))
re_turns <- rutils::diff_it(price_s) /
  rutils::diff_it(.index(HighFreq::SPY))
re_turns[1] <- 0
price_s <- cumsum(re_turns)
# Calculate volatilities for vector of aggregation intervals
interval_s <- round(seq.int(from=3, to=30, length.out=9)^2)
vol_s <- sapply(interval_s, function(inter_val) {
  end_p <- rutils::calc_endpoints(price_s, inter_val=inter_val)
  sd(rutils::diff_it(price_s[end_p]))
})  # end sapply
vol_log <- log(vol_s)
vol_log <- vol_log - mean(vol_log)
inter_log <- log(interval_s)
inter_log <- inter_log - mean(inter_log)
mod_el <- lm(vol_log ~ inter_log)
hurs_t <- summary(mod_el)$coeff[2, 1]
# Or directly from formula
hurst_form <- sum(vol_log*inter_log)/sum(inter_log^2)
all.equal(hurst_form, hurs_t)

x11(width=6, height=5)
par(mar=c(4, 4, 2, 1), oma=c(1, 1, 1, 1))
plot(vol_log ~ inter_log, lwd=6, col="red",
     xlab="aggregation intervals (log)",
     ylab="volatility (log)",
     main="Hurst Exponent for SPY From Volatilities")
abline(mod_el, lwd=3, col="blue")
text(-2, 0.5, paste0("Hurst = ", round(hurs_t, 4)))

# Calculate the rescaled range
inter_val <- 500
end_p <- rutils::calc_endpoints(price_s,
  inter_val=inter_val)
r_s <- sapply(2:NROW(end_p), function(ep) {
  in_dex <- end_p[ep-1]:end_p[ep]
  diff(range(price_s[in_dex]))/sd(re_turns[in_dex])
})  # end sapply
mean(r_s)

# Calculate rescaled range for vector of aggregation intervals
r_s <- sapply(interval_s, function(inter_val) {
  end_p <- rutils::calc_endpoints(price_s,
    inter_val=inter_val)
  r_s <- sapply(2:NROW(end_p), function(ep) {
    in_dex <- end_p[ep-1]:end_p[ep]
    diff(range(price_s[in_dex]))/sd(re_turns[in_dex])
  })  # end sapply
  mean(na.omit(r_s))
})  # end sapply
rs_log <- log(r_s)
rs_log <- rs_log - mean(rs_log)
inter_log <- log(interval_s)
inter_log <- inter_log - mean(inter_log)
mod_el <- lm(rs_log ~ inter_log)
hurs_t <- summary(mod_el)$coeff[2, 1]
# Or directly from formula
hurst_form <- sum(rs_log*inter_log)/sum(inter_log^2)
all.equal(hurst_form, hurs_t)

x11(width=6, height=5)
par(mar=c(4, 4, 2, 1), oma=c(1, 1, 1, 1))
plot(rs_log ~ inter_log, lwd=6, col="red",
     xlab="aggregation intervals (log)",
     ylab="rescaled range (log)",
     main="Rescaled Range Analysis for SPY")
abline(mod_el, lwd=3, col="blue")
text(-2, 0.5, paste0("Hurst = ", round(hurs_t, 4)))

library(HighFreq)  # Load HighFreq
# Daily SPY volatility from minutely prices using package TTR
library(TTR)
sqrt((6.5*60)*mean(na.omit(
  TTR::volatility(HighFreq::SPY, N=1,
          calc="yang.zhang"))^2))
# SPY volatility using package HighFreq
60*sqrt((6.5*60)*agg_regate(oh_lc=HighFreq::SPY,
    weight_ed=FALSE, mo_ment="run_variance",
    calc_method="yang_zhang"))

oh_lc <- rutils::etf_env$VTI
# Number of data points
n_rows <- NROW(oh_lc["2018-06/"])
# Define end_p at each point in time
end_p <- 0:n_rows
# Number of data points in look_back interval
look_back <- 22
# start_p are end_p lagged by look_back
start_p <- c(rep_len(0, look_back - 1), end_p[1:(NROW(end_p)- look_back + 1)])
head(start_p, 33)

# Number of data points
price_s <- quantmod::Cl(oh_lc["2018/"])
n_rows <- NROW(price_s)
# Number of periods between endpoints
n_points <- 21
# Number of n_points that fit over n_rows
n_agg <- n_rows %/% n_points
# If n_rows==n_points*n_agg then whole number
end_p <- (0:n_agg)*n_points
# Stub interval at beginning
end_p <- c(0, n_rows-n_points*n_agg + (0:n_agg)*n_points)
# Else stub interval at end
end_p <- c((0:n_agg)*n_points, n_rows)
# Or use xts::endpoints()
end_p <- xts::endpoints(price_s, on="months")

# Plot data and endpoints as vertical lines
plot.xts(price_s, col="blue", lwd=2, xlab="", ylab="",
   main="Prices with Endpoints as Vertical Lines")
addEventLines(xts(rep("endpoint", NROW(end_p)-1), index(price_s)[end_p]),
        col="red", lwd=2, pos=4)
# Or
plot_theme <- chart_theme()
plot_theme$col$line.col <- "blue"
chart_Series(price_s, theme=plot_theme,
  name="prices with endpoints as vertical lines")
abline(v=end_p, col="red", lwd=2)

# Number of data points
n_rows <- NROW(rutils::etf_env$VTI["2019/"])
# Number of n_points that fit over n_rows
n_points <- 22
n_agg <- n_rows %/% n_points
# Stub interval at beginning
end_p <- c(0, n_rows-n_points*n_agg +
            (0:n_agg)*n_points)

# look_back defined as number of data points
look_back <- 252
# start_p are end_p lagged by look_back
start_p <- (end_p - look_back + 1)
start_p <- ifelse(start_p < 0, 0, start_p)
# look_back defined as number of end_p
look_back <- 12
start_p <- c(rep_len(0, look_back-1), end_p[1:(NROW(end_p)- look_back + 1)])
# Bind start_p with end_p
cbind(start_p, end_p)

# Number of data points
n_rows <- NROW(rutils::etf_env$VTI["2019/"])
# Number of data points per interval
n_points <- 22
# Number of n_pointss that fit over n_rows
n_agg <- n_rows %/% n_points
# Define end_p with beginning stub
end_p <- c(0, n_rows-n_points*n_agg +
            (0:n_agg)*n_points)
# Define contiguous start_p
start_p <- c(0, end_p[1:(NROW(end_p)-1)])
# Define exclusive start_p
start_p <- c(0, end_p[1:(NROW(end_p)-1)]+1)

price_s <- quantmod::Cl(rutils::etf_env$VTI)
end_p <- 0:NROW(price_s)  # End points at each point
n_rows <- NROW(end_p)
look_back <- 22  # Number of data points per look-back interval
# start_p are multi-period lag of end_p
start_p <- c(rep_len(0, look_back - 1), end_p[1:(n_rows - look_back + 1)])
# Define list of look-back intervals for aggregations over past
look_backs <- lapply(2:n_rows, function(in_dex) {
    start_p[in_dex]:end_p[in_dex]
})  # end lapply
# Define aggregation function
agg_regate <- function(x_ts) c(max=max(x_ts), min=min(x_ts))
# Perform aggregations over look_backs list
agg_regations <- sapply(look_backs,
    function(look_back) agg_regate(price_s[look_back])
)  # end sapply
# Coerce agg_regations into matrix and transpose it
if (is.vector(agg_regations))
  agg_regations <- t(agg_regations)
agg_regations <- t(agg_regations)
# Coerce agg_regations into xts series
agg_regations <- xts(agg_regations, order.by=index(price_s[end_p]))

library(rutils)  # Load package rutils
# Perform aggregations over look_backs list
agg_regations <- lapply(look_backs,
    function(look_back) agg_regate(price_s[look_back])
)  # end lapply
# rbind list into single xts or matrix
agg_regations <- rutils::do_call(rbind, agg_regations)
# Convert into xts
agg_regations <- xts::xts(agg_regations,
    order.by=index(price_s))
agg_regations <- cbind(agg_regations, price_s)
# Plot aggregations with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("black", "red", "green")
x11(width=6, height=5)
chart_Series(agg_regations, theme=plot_theme,
       name="price aggregations")
legend("top", legend=colnames(agg_regations),
  bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")

# library(rutils)  # Load package rutils
# Define functional for rolling aggregations
roll_agg <- function(x_ts, look_back, FUN, ...) {
# Define end points at every period
  end_p <- 0:NROW(x_ts)
  n_rows <- NROW(end_p)
# Define starting points as lag of end_p
  start_p <- c(rep_len(0, look_back - 1),
    end_p[1:(n_rows- look_back + 1)])
# Perform aggregations over look_backs list
  agg_regations <- lapply(2:n_rows, function(in_dex)
    FUN(x_ts[start_p[in_dex]:end_p[in_dex]], ...)
  )  # end lapply
# rbind list into single xts or matrix
  agg_regations <- rutils::do_call(rbind, agg_regations)
# Coerce agg_regations into xts series
  if (!is.xts(agg_regations))
    agg_regations <- xts(agg_regations, order.by=index(x_ts))
  agg_regations
}  # end roll_agg
# Define aggregation function
agg_regate <- function(x_ts)
  c(max=max(x_ts), min=min(x_ts))
# Perform aggregations over rolling interval
agg_regations <- roll_agg(price_s, look_back=look_back,
              FUN=agg_regate)
class(agg_regations)
dim(agg_regations)

# library(rutils)  # Load package rutils
# Define aggregation function that returns a vector
agg_vector <- function(x_ts)
  c(max=max(x_ts), min=min(x_ts))
# Define aggregation function that returns an xts
agg_xts <- function(x_ts)
  xts(t(c(max=max(x_ts), min=min(x_ts))), order.by=end(x_ts))
# Benchmark the speed of aggregation functions
library(microbenchmark)
summary(microbenchmark(
  agg_vector=roll_agg(price_s, look_back=look_back, FUN=agg_vector),
  agg_xts=roll_agg(price_s, look_back=look_back, FUN=agg_xts),
  times=10))[, c(1, 4, 5)]

# library(rutils)  # Load package rutils
# Define aggregation function that returns a single value
agg_regate <- function(x_ts)  max(x_ts)
# Perform aggregations over a rolling interval
agg_regations <- xts:::rollapply.xts(price_s, width=look_back,
              FUN=agg_regate, align="right")
# Perform aggregations over a rolling interval
library(PerformanceAnalytics)  # Load package PerformanceAnalytics
agg_regations <- apply.rolling(price_s,
              width=look_back, FUN=agg_regate)
# Benchmark the speed of the functionals
library(microbenchmark)
summary(microbenchmark(
  roll_agg=roll_agg(price_s, look_back=look_back, FUN=max),
  roll_xts=xts:::rollapply.xts(price_s, width=look_back, FUN=max, align="right"),
  apply_rolling=apply.rolling(price_s, width=look_back, FUN=max),
  times=10))[, c(1, 4, 5)]

# library(rutils)  # Load package rutils
# Rolling sum using cumsum()
roll_sum <- function(x_ts, look_back) {
  cum_sum <- cumsum(na.omit(x_ts))
  out_put <- cum_sum - lag(x=cum_sum, k=look_back)
  out_put[1:look_back, ] <- cum_sum[1:look_back, ]
  colnames(out_put) <- paste0(colnames(x_ts), "_stdev")
  out_put
}  # end roll_sum
agg_regations <- roll_sum(price_s, look_back=look_back)
# Perform rolling aggregations using lapply loop
agg_regations <- lapply(2:n_rows, function(in_dex)
    sum(price_s[start_p[in_dex]:end_p[in_dex]])
)  # end lapply
# rbind list into single xts or matrix
agg_regations <- rutils::do_call(rbind, agg_regations)
head(agg_regations)
tail(agg_regations)
# Benchmark the speed of both methods
library(microbenchmark)
summary(microbenchmark(
  roll_sum=roll_sum(price_s, look_back=look_back),
  s_apply=sapply(look_backs,
    function(look_back) sum(price_s[look_back])),
  times=10))[, c(1, 4, 5)]

# Extract time series of VTI prices
price_s <- quantmod::Cl(rutils::etf_env$VTI)
# Calculate EWMA prices using filter()
look_back <- 21
weight_s <- exp(-0.1*1:look_back)
weight_s <- weight_s/sum(weight_s)
filter_ed <- stats::filter(price_s, filter=weight_s,
                   method="convolution", sides=1)
# filter() returns time series of class "ts"
class(filter_ed)
# Filter using compiled C++ function directly
getAnywhere(C_cfilter)
str(stats:::C_cfilter)
filter_fast <- .Call(stats:::C_cfilter, price_s, filter=weight_s, sides=1, circular=FALSE)
all.equal(as.numeric(filter_ed), filter_fast, check.attributes=FALSE)
# Calculate EWMA prices using roll::roll_sum()
weights_rev <- rev(weight_s)
roll_ed <- roll::roll_sum(price_s, width=look_back, weights=weights_rev)
all.equal(filter_ed[-(1:look_back)],
    as.numeric(roll_ed)[-(1:look_back)],
    check.attributes=FALSE)
# Benchmark speed of rolling calculations
library(microbenchmark)
summary(microbenchmark(
  filter=filter(price_s, filter=weight_s, method="convolution", sides=1),
  filter_fast=.Call(stats:::C_cfilter, price_s, filter=weight_s, sides=1, circular=FALSE),
  cum_sum=cumsum(price_s),
  roll=roll::roll_sum(price_s, width=look_back, weights=weights_rev)
  ), times=10)[, c(1, 4, 5)]

# Calculate the rolling maximum and minimum over a vector of data
roll_maxminr <- function(vec_tor, look_back) {
  n_rows <- NROW(vec_tor)
  max_min <- matrix(numeric(2*n_rows), nc=2)
  # Loop over periods
  for (it in 1:n_rows) {
    sub_vec <- vec_tor[max(1, it-look_back+1):it]
    max_min[it, 1] <- max(sub_vec)
    max_min[it, 2] <- min(sub_vec)
  }  # end for
  return(max_min)
}  # end roll_maxminr
max_minr <- roll_maxminr(price_s, look_back)
max_minr <- xts::xts(max_minr, index(price_s))
library(TTR)  # Load package TTR
max_min <- cbind(TTR::runMax(x=price_s, n=look_back),
           TTR::runMin(x=price_s, n=look_back))
all.equal(max_min[-(1:look_back), ], max_minr[-(1:look_back), ], check.attributes=FALSE)
# Benchmark the speed of TTR::runMax
library(microbenchmark)
summary(microbenchmark(
  pure_r=roll_maxminr(price_s, look_back),
  ttr=TTR::runMax(price_s, n=look_back),
  times=10))[, c(1, 4, 5)]
# Benchmark the speed of TTR::runSum
summary(microbenchmark(
  vector_r=cumsum(coredata(price_s)),
  rutils=rutils::roll_sum(price_s, look_back=look_back),
  ttr=TTR::runSum(price_s, n=look_back),
  times=10))[, c(1, 4, 5)]

library(rutils)
# Calculate rolling VTI variance using package roll
library(roll)  # Load roll
re_turns <- na.omit(rutils::etf_env$re_turns[, "VTI"])
look_back <- 22
# Calculate rolling sum using RcppRoll
sum_roll <- roll::roll_sum(re_turns, width=look_back)
# Calculate rolling sum using rutils
sum_rutils <- rutils::roll_sum(re_turns, look_back=look_back)
all.equal(sum_roll[-(1:look_back), ], sum_rutils[-(1:look_back), ], check.attributes=FALSE)
# Benchmark speed of rolling calculations
library(microbenchmark)
summary(microbenchmark(
  cum_sum=cumsum(re_turns),
  roll=roll::roll_sum(re_turns, width=look_back),
  RcppRoll=RcppRoll::roll_sum(re_turns, n=look_back),
  rutils=rutils::roll_sum(re_turns, look_back=look_back),
  times=10))[, c(1, 4, 5)]

library(RcppRoll)  # Load package RcppRoll
# Calculate rolling sum using RcppRoll
sum_roll <- RcppRoll::roll_sum(re_turns, align="right", n=look_back)
# Calculate rolling sum using rutils
sum_rutils <- rutils::roll_sum(re_turns, look_back=look_back)
all.equal(sum_roll, coredata(sum_rutils[-(1:(look_back-1))]), check.attributes=FALSE)
# Benchmark speed of rolling calculations
library(microbenchmark)
summary(microbenchmark(
  cum_sum=cumsum(re_turns),
  RcppRoll=RcppRoll::roll_sum(re_turns, n=look_back),
  rutils=rutils::roll_sum(re_turns, look_back=look_back),
  times=10))[, c(1, 4, 5)]
# Calculate EWMA prices using RcppRoll
price_s <- na.omit(rutils::etf_env$VTI[, 4])
weight_s <- exp(0.1*1:look_back)
prices_ewma <- RcppRoll::roll_mean(price_s,
align="right", n=look_back, weights=weight_s)
prices_ewma <- cbind(price_s,
  rbind(coredata(price_s[1:(look_back-1), ]), prices_ewma))
colnames(prices_ewma) <- c("VTI", "VTI EWMA")
# Plot an interactive dygraph plot
dygraphs::dygraph(prices_ewma)
# Or static plot of EWMA prices with custom line colors
x11(width=6, height=5)
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("black", "red")
chart_Series(prices_ewma, theme=plot_theme,
       name="EWMA prices")
legend("top", legend=colnames(prices_ewma),
 bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")

# library(rutils)  # Load package rutils
library(caTools)  # Load package "caTools"
# Get documentation for package "caTools"
packageDescription("caTools")  # Get short description
help(package="caTools")  # Load help page
data(package="caTools")  # List all datasets in "caTools"
ls("package:caTools")  # List all objects in "caTools"
detach("package:caTools")  # Remove caTools from search path
# Median filter
look_back <- 2
price_s <- quantmod::Cl(HighFreq::SPY["2012-02-01/2012-04-01"])
med_ian <- runmed(x=price_s, k=look_back)
# Vector of rolling volatility
sigma_r <- runsd(x=price_s, k=look_back,
          endrule="constant", align="center")
# Vector of rolling quantiles
quan_tiles <- runquantile(x=price_s, k=look_back,
  probs=0.9, endrule="constant", align="center")

library(rutils)  # Load package rutils
# Indices of last observations in each hour
end_p <- xts::endpoints(price_s, on="hours")
head(end_p)
# extract the last observations in each hour
head(price_s[end_p, ])

price_s <- quantmod::Cl(rutils::etf_env$VTI)
# Number of data points
n_rows <- NROW(price_s)
# Number of data points per interval
look_back <- 22
# Number of look_backs that fit over n_rows
n_agg <- n_rows %/% look_back
# Define end_p with beginning stub
end_p <- c(0, n_rows-look_back*n_agg + (0:n_agg)*look_back)
# Define contiguous start_p
start_p <- c(0, end_p[1:(NROW(end_p)-1)])
# Define list of look-back intervals for aggregations over past
look_backs <- lapply(2:NROW(end_p), function(in_dex) {
    start_p[in_dex]:end_p[in_dex]
})  # end lapply
look_backs[[1]]
look_backs[[2]]
# Perform sapply() loop over look_backs list
agg_regations <- sapply(look_backs,
    function(look_back) {
x_ts <- price_s[look_back]
c(max=max(x_ts), min=min(x_ts))
  })  # end sapply
# Coerce agg_regations into matrix and transpose it
if (is.vector(agg_regations))
  agg_regations <- t(agg_regations)
agg_regations <- t(agg_regations)
# Coerce agg_regations into xts series
agg_regations <- xts(agg_regations,
    order.by=index(price_s[end_p]))
head(agg_regations)
# Plot aggregations with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("red", "green")
chart_Series(agg_regations, theme=plot_theme,
       name="price aggregations")
legend("top", legend=colnames(agg_regations),
  bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")

# library(rutils)  # Load package rutils
# Perform lapply() loop over look_backs list
agg_regations <- lapply(look_backs,
    function(look_back) {
x_ts <- price_s[look_back]
c(max=max(x_ts), min=min(x_ts))
    })  # end lapply
# rbind list into single xts or matrix
agg_regations <- rutils::do_call(rbind, agg_regations)
# Coerce agg_regations into xts series
agg_regations <- xts(agg_regations,
    order.by=index(price_s[end_p]))
head(agg_regations)
# Plot aggregations with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("red", "green")
chart_Series(agg_regations, theme=plot_theme,
       name="price aggregations")
legend("top", legend=colnames(agg_regations),
  bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")

# library(rutils)  # Load package rutils
# Define functional for rolling aggregations over end_p
roll_agg <- function(x_ts, end_p, FUN, ...) {
  n_rows <- NROW(end_p)
# start_p are single-period lag of end_p
  start_p <- c(1, end_p[1:(n_rows-1)])
# Perform aggregations over look_backs list
  agg_regations <- lapply(look_backs,
    function(look_back) FUN(x_ts[look_back], ...))  # end lapply
# rbind list into single xts or matrix
  agg_regations <- rutils::do_call(rbind, agg_regations)
  if (!is.xts(agg_regations))
    agg_regations <-  # Coerce agg_regations into xts series
    xts(agg_regations, order.by=index(x_ts[end_p]))
  agg_regations
}  # end roll_agg
# Apply sum() over end_p
agg_regations <-
  roll_agg(price_s, end_p=end_p, FUN=sum)
agg_regations <-
  period.apply(price_s, INDEX=end_p, FUN=sum)
# Benchmark the speed of aggregation functions
summary(microbenchmark(
  roll_agg=roll_agg(price_s, end_p=end_p, FUN=sum),
  period_apply=period.apply(price_s, INDEX=end_p, FUN=sum),
  times=10))[, c(1, 4, 5)]
agg_regations <- period.sum(price_s, INDEX=end_p)
head(agg_regations)

# library(rutils)  # Load package rutils
# Load package HighFreq
library(HighFreq)
# Extract closing minutely prices
price_s <- quantmod::Cl(rutils::etf_env$VTI["2019"])
# Apply "mean" over daily periods
agg_regations <- apply.daily(price_s, FUN=sum)
head(agg_regations)

# Define end_p with beginning stub
n_points <- 5
n_rows <- NROW(price_s)
n_agg <- n_rows %/% n_points
end_p <- c(0, n_rows-n_points*n_agg + (0:n_agg)*n_points)
# Number of data points in look_back interval
look_back <- 22
# start_p are end_p lagged by look_back
start_p <- (end_p - look_back + 1)
start_p <- ifelse(start_p < 0, 0, start_p)
# Perform lapply() loop over look_backs list
agg_regations <- lapply(2:NROW(end_p), function(in_dex) {
x_ts <- price_s[start_p[in_dex]:end_p[in_dex]]
c(max=max(x_ts), min=min(x_ts))
})  # end lapply
# rbind list into single xts or matrix
agg_regations <- rutils::do_call(rbind, agg_regations)
# Coerce agg_regations into xts series
agg_regations <- xts(agg_regations,
    order.by=index(price_s[end_p]))
# Plot aggregations with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("red", "green")
chart_Series(agg_regations, theme=plot_theme,
       name="price aggregations")
legend("top", legend=colnames(agg_regations),
  bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")

agg_regations <- cbind(price_s, agg_regations)
tail(agg_regations)
agg_regations <- na.omit(xts:::na.locf.xts(agg_regations))
# Plot aggregations with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("black", "red", "green")
chart_Series(agg_regations, theme=plot_theme,
       name="price aggregations")
legend("top", legend=colnames(agg_regations),
  bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")

set.seed(1121)  # Reset random number generator
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
library(zoo)  # Load package zoo
# Create zoo time series of random returns
date_s <- Sys.Date() + 0:365
zoo_series <- zoo(rnorm(NROW(date_s)), order.by=date_s)
# Create monthly dates
dates_agg <- as.Date(as.yearmon(index(zoo_series)))
# Perform monthly mean aggregation
zoo_agg <- aggregate(zoo_series, by=dates_agg, FUN=mean)
# Merge with original zoo - union of dates
zoo_agg <- cbind(zoo_series, zoo_agg)
# Replace NA's using locf
zoo_agg <- na.locf(zoo_agg, na.rm=FALSE)
# Extract aggregated zoo
zoo_agg <- zoo_agg[index(zoo_series), 2]

# library(rutils)  # Load package rutils
# Plot original and aggregated cumulative returns
plot(cumsum(zoo_series), xlab="", ylab="")
lines(cumsum(zoo_agg), lwd=2, col="red")
# Add legend
legend("topright", inset=0.05, cex=0.8, bty="n",
 title="Aggregated Prices",
 leg=c("orig prices", "agg prices"),
 lwd=2, bg="white", col=c("black", "red"))

par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# Perform monthly mean aggregation
zoo_agg <- aggregate(zoo_series, by=dates_agg, FUN=mean)
# Merge with original zoo - union of dates
zoo_agg <- cbind(zoo_series, zoo_agg)
# Replace NA's using linear interpolation
zoo_agg <- na.approx(zoo_agg)
# Extract interpolated zoo
zoo_agg <- zoo_agg[index(zoo_series), 2]
# Plot original and interpolated zoo
plot(cumsum(zoo_series), xlab="", ylab="")
lines(cumsum(zoo_agg), lwd=2, col="red")
# Add legend
legend("topright", inset=0.05, cex=0.8, title="Interpolated Prices",
 leg=c("orig prices", "interpol prices"), lwd=2, bg="white",
 col=c("black", "red"), bty="n")

par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# "mean" aggregation over interval with width=11
zoo_mean <- rollapply(zoo_series, width=11,
                FUN=mean, align="right")
# Merge with original zoo - union of dates
zoo_mean <- cbind(zoo_series, zoo_mean)
# Replace NA's using na.locf
zoo_mean <- na.locf(zoo_mean, na.rm=FALSE, fromLast=TRUE)
# Extract mean zoo
zoo_mean <- zoo_mean[index(zoo_series), 2]
# Plot original and interpolated zoo
plot(cumsum(zoo_series), xlab="", ylab="")
lines(cumsum(zoo_mean), lwd=2, col="red")
# Add legend
legend("topright", inset=0.05, cex=0.8, title="Mean Prices",
 leg=c("orig prices", "mean prices"), lwd=2, bg="white",
 col=c("black", "red"), bty="n")

x11(width=6, height=5)
par(mar=c(3, 2, 1, 1), oma=c(0, 0, 0, 0), mgp=c(2, 0.5, 0))
re_turns <- as.numeric(na.omit(rutils::etf_env$re_turns$VTI))
# Plot autocorrelations using stats::acf()
stats::acf(re_turns, lag=10, xlab="lag", main="")
title(main="ACF of VTI Returns", line=-1)
# Two-tailed 95% confidence interval
qnorm(0.975)/sqrt(NROW(re_turns))

# Ljung-Box test for VTI returns
# 'lag' is the number of autocorrelation coefficients
Box.test(re_turns, lag=10, type="Ljung")
library(Ecdat)  # Load Ecdat
macro_zoo <- as.zoo(Macrodat[, c("lhur", "fygm3")])
colnames(macro_zoo) <- c("unemprate", "3mTbill")
macro_diff <- na.omit(diff(macro_zoo))
# Changes in 3 month T-bill rate are autocorrelated
Box.test(macro_diff[, "3mTbill"], lag=10, type="Ljung")
# Changes in unemployment rate are autocorrelated
Box.test(macro_diff[, "unemprate"], lag=10, type="Ljung")

# Get the ACF data returned invisibly
acf_data <- acf(re_turns, plot=FALSE)
summary(acf_data)
# Print the ACF data
print(acf_data)
dim(acf_data$acf)
dim(acf_data$lag)
head(acf_data$acf)

plot_acf <- function(x_ts, lagg=10,
               plo_t=TRUE,
               xlab="Lag", ylab="",
               main="", ...) {
  # Calculate the ACF without a plot
  acf_data <- acf(x=x_ts, lag.max=lagg, plot=FALSE, ...)
  # Remove first element of ACF data
  acf_data$acf <- array(data=acf_data$acf[-1],
    dim=c((dim(acf_data$acf)[1]-1), 1, 1))
  acf_data$lag <- array(data=acf_data$lag[-1],
    dim=c((dim(acf_data$lag)[1]-1), 1, 1))
  # Plot ACF
  if (plo_t) {
    ci <- qnorm((1+0.95)/2)*sqrt(1/NROW(x_ts))
    ylim <- c(min(-ci, range(acf_data$acf[-1])),
        max(ci, range(acf_data$acf[-1])))
    plot(acf_data, xlab=xlab, ylab=ylab,
   ylim=ylim, main="", ci=0)
    title(main=main, line=0.5)
    abline(h=c(-ci, ci), col="blue", lty=2)
  }
  # Return the ACF data invisibly
  invisible(acf_data)
}  # end plot_acf

# Improved autocorrelation function
rutils::plot_acf(re_turns, lag=10, main="")
title(main="ACF of VTI returns", line=-1)
# Ljung-Box test for VTI returns
Box.test(re_turns, lag=10, type="Ljung")

x11(width=6, height=7)
par(mfrow=c(2,1))  # Set plot panels
par(mar=c(3, 3, 2, 2), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
# Autocorrelation of squared random returns
rutils::plot_acf(rnorm(NROW(re_turns))^2, lag=10, main="")
title(main="ACF of Squared Random Returns", line=-1)
# Autocorrelation of squared VTI returns
rutils::plot_acf(re_turns^2, lag=10, main="")
title(main="ACF of Squared VTI Returns", line=-1)
# Ljung-Box test for squared VTI returns
Box.test(re_turns^2, lag=10, type="Ljung")

library(rutils)  # Load package rutils
library(Ecdat)  # Load Ecdat
colnames(Macrodat)  # United States Macroeconomic Time Series
macro_zoo <- as.zoo(  # Coerce to "zoo"
    Macrodat[, c("lhur", "fygm3")])
colnames(macro_zoo) <- c("unemprate", "3mTbill")
# ggplot2 in multiple panes
autoplot(  # Generic ggplot2 for "zoo"
  object=macro_zoo, main="US Macro",
  facets=Series ~ .) + # end autoplot
  xlab("") +
theme(  # modify plot theme
  legend.position=c(0.1, 0.5),
  plot.title=element_text(vjust=-2.0),
  plot.margin=unit(c(-0.5, 0.0, -0.5, 0.0), "cm"),
  plot.background=element_blank(),
  axis.text.y=element_blank()
)  # end theme

par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # Set plot panels
macro_diff <- na.omit(diff(macro_zoo))
rutils::plot_acf(coredata(macro_diff[, "unemprate"]),
  lag=10, main="quarterly unemployment rate")
rutils::plot_acf(coredata(macro_diff[, "3mTbill"]),
  lag=10, main="3 month T-bill EOQ")

# Extract time series of VTI prices
price_s <- na.omit(rutils::etf_env$price_s$VTI)
n_rows <- NROW(price_s)
# Apply convolution filter over past values (sides=1)
co_eff <- c(0.1, 0.39, 0.5); n_coeff <- NROW(co_eff)
filter_ed <- filter(price_s, filter=co_eff,
              method="convolution", sides=1)

# Inspect the R code of the function filter()
filter
# Get information about C_cfilter()
getAnywhere(C_cfilter)
# Filter over past values (sides=1)
filter_ed <- filter(price_s, filter=co_eff,
              method="convolution", sides=1)
# Filter using C_cfilter() compiled C++ function directly
filter_fast <- .Call(stats:::C_cfilter, price_s, filter=co_eff,
               sides=1, circular=FALSE)
all.equal(as.numeric(filter_ed), filter_fast,
    check.attributes=FALSE)
# Benchmark speed of the two methods
library(microbenchmark)
summary(microbenchmark(
  fast_filter=.Call(stats:::C_cfilter, price_s, filter=co_eff, sides=1, circular=FALSE),
  R_filter=filter(price_s, filter=co_eff, method="convolution", sides=1)
  ), times=10)[, c(1, 4, 5)]
# Simulate AR process using filter()
in_nov <- rnorm(n_rows)
ari_ma <- filter(x=in_nov, filter=co_eff, method="recursive")
# Get information about C_rfilter()
getAnywhere(C_rfilter)
# Filter using C_rfilter() compiled C++ function directly
arima_fast <- .Call(stats:::C_rfilter, in_nov, co_eff,
              double(n_coeff + n_rows))
all.equal(as.numeric(ari_ma), arima_fast[-(1:n_coeff)], check.attributes=FALSE)
# Benchmark speed of the two methods
summary(microbenchmark(
  fast_filter=.Call(stats:::C_rfilter, in_nov, co_eff, double(NROW(co_eff) + NROW(in_nov))),
  R_filter=filter(x=in_nov, filter=co_eff, method="recursive")
  ), times=10)[, c(1, 4, 5)]

library(rutils)  # Load package rutils
library(ggplot2)  # Load ggplot2
library(gridExtra)  # Load gridExtra
# Coerce to zoo and merge the time series
filter_ed <- cbind(price_s, as.numeric(filter_ed))
colnames(filter_ed) <- c("VTI", "VTI filtered")
# Plot ggplot2
autoplot(filter_ed["2008/2010"],
    main="Filtered VTI", facets=NULL) +  # end autoplot
xlab("") + ylab("") +
theme(  # modify plot theme
    legend.position=c(0.1, 0.5),
    plot.title=element_text(vjust=-2.0),
    plot.margin=unit(c(-0.5, 0.0, -0.5, 0.0), "cm"),
    plot.background=element_blank(),
    axis.text.y=element_blank()
    )  # end theme
# end ggplot2

# Open plot window
x11(width=6, height=7)
# Set plot parameters
par(oma=c(1, 1, 0, 1), mar=c(1, 1, 1, 1), mgp=c(0, 0.5, 0),
    cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# Set two plot panels
par(mfrow=c(2,1))
# Calculate VTI returns
re_turns <- na.omit(diff(log(filter_ed)))
# Plot ACF of VTI returns
rutils::plot_acf(re_turns[, 1], lag=10, xlab="")
title(main="ACF of VTI Returns", line=-1)
# Plot ACF of VTI returns
rutils::plot_acf(re_turns[, 2], lag=10, xlab="")
title(main="ACF of VTI Filtered Returns", line=-1)

# Simulate AR processes
set.seed(1121)  # Reset random numbers
date_s <- Sys.Date() + 0:728  # Two year daily series
ari_ma <- xts(  # AR time series of returns
  x=arima.sim(n=NROW(date_s), model=list(ar=0.2)),
  order.by=date_s)
ari_ma <- cbind(ari_ma, cumsum(ari_ma))
colnames(ari_ma) <- c("AR returns", "AR prices")

library(ggplot2)  # Load ggplot2
library(gridExtra)  # Load gridExtra
autoplot(object=ari_ma, # ggplot AR process
 facets="Series ~ .",
 main="Autoregressive process (phi=0.2)") +
  facet_grid("Series ~ .", scales="free_y") +
  xlab("") + ylab("") +
theme(legend.position=c(0.1, 0.5),
  plot.background=element_blank(),
  axis.text.y=element_blank())

ar_coeff <- c(-0.9, 0.01, 0.9)  # AR coefficients
# Create three AR time series
ari_ma <- sapply(ar_coeff, function(phi) {
  set.seed(1121)  # Reset random numbers
  arima.sim(n=NROW(date_s), model=list(ar=phi))
})  # end sapply
colnames(ari_ma) <- paste("autocorr", ar_coeff)
plot.zoo(ari_ma, main="AR(1) prices", xlab=NA)
# Or plot using ggplot
ari_ma <- xts(x=ari_ma, order.by=date_s)
library(ggplot)
autoplot(ari_ma, main="AR(1) prices",
   facets=Series ~ .) +
    facet_grid(Series ~ ., scales="free_y") +
xlab("") +
theme(
  legend.position=c(0.1, 0.5),
  plot.title=element_text(vjust=-2.0),
  plot.margin=unit(c(-0.5, 0.0, -0.5, 0.0), "cm"),
  plot.background=element_blank(),
  axis.text.y=element_blank())

# Define AR(3) coefficients and innovations
co_eff <- c(0.1, 0.39, 0.5)
n_rows <- 1e2
set.seed(1121); in_nov <- rnorm(n_rows)
# Simulate AR process using recursive loop in R
ari_ma <- numeric(NROW(in_nov))
ari_ma[1] <- in_nov[1]
ari_ma[2] <- co_eff[1]*ari_ma[1] + in_nov[2]
ari_ma[3] <- co_eff[1]*ari_ma[2] + co_eff[2]*ari_ma[1] + in_nov[3]
for (it in 4:NROW(ari_ma)) {
  ari_ma[it] <- ari_ma[(it-1):(it-3)] %*% co_eff + in_nov[it]
}  # End for
# Simulate AR process using filter()
arima_faster <- filter(x=in_nov,
  filter=co_eff, method="recursive")
class(arima_faster)
all.equal(ari_ma, as.numeric(arima_faster))
# Fast simulation of AR process using C_rfilter()
arima_fastest <- .Call(stats:::C_rfilter, in_nov, co_eff,
                 double(NROW(co_eff) + NROW(in_nov)))[-(1:3)]
all.equal(ari_ma, arima_fastest)

# Calculate modulus of roots of characteristic equation
root_s <- Mod(polyroot(c(1, -co_eff)))
# Calculate warmup period
warm_up <- NROW(co_eff) + ceiling(6/log(min(root_s)))
set.seed(1121)
n_rows <- 1e4
in_nov <- rnorm(n_rows + warm_up)
# Simulate AR process using arima.sim()
ari_ma <- arima.sim(n=n_rows,
  model=list(ar=co_eff),
  start.innov=in_nov[1:warm_up],
  innov=in_nov[(warm_up+1):NROW(in_nov)])
# Simulate AR process using filter()
arima_fast <- filter(x=in_nov,
  filter=co_eff, method="recursive")
all.equal(arima_fast[-(1:warm_up)],
  as.numeric(ari_ma))
# Benchmark the speed of the three methods of simulating AR process
library(microbenchmark)
summary(microbenchmark(
  filter=filter(x=in_nov, filter=co_eff, method="recursive"),
  arima_sim=arima.sim(n=n_rows,
                  model=list(ar=co_eff),
                  start.innov=in_nov[1:warm_up],
                  innov=in_nov[(warm_up+1):NROW(in_nov)]),
  arima_loop={for (it in 4:NROW(ari_ma)) {
  ari_ma[it] <- ari_ma[(it-1):(it-3)] %*% co_eff + in_nov[it]}}
  ), times=10)[, c(1, 4, 5)]

library(rutils)  # Load rutils
library(ggplot2)  # Load ggplot2
set.seed(1121)  # Initialize random number generator
rand_walk <- cumsum(zoo(matrix(rnorm(3*100), ncol=3),
            order.by=(Sys.Date()+0:99)))
colnames(rand_walk) <- paste("rand_walk", 1:3, sep="_")
plot.zoo(rand_walk, main="Random walks",
     xlab="", ylab="", plot.type="single",
     col=c("black", "red", "blue"))
# Add legend
legend(x="topleft", legend=colnames(rand_walk),
 col=c("black", "red", "blue"), lty=1)

# Simulate arima with large AR coefficient
set.seed(1121)
ari_ma <- arima.sim(n=n_rows, model=list(ar=0.99))
tseries::adf.test(ari_ma)
# Integrated series has unit root
tseries::adf.test(cumsum(ari_ma))
# Simulate arima with negative AR coefficient
set.seed(1121)
ari_ma <- arima.sim(n=n_rows, model=list(ar=-0.99))
tseries::adf.test(ari_ma)
# Integrated series has unit root
tseries::adf.test(cumsum(ari_ma))

set.seed(1121); vol_at <- 0.01; in_nov <- vol_at*rnorm(1e4)
# Simulate AR(1) process with coefficient=1, with unit root
ari_ma <- filter(x=in_nov, filter=1.0, method="recursive")
x11(); plot(ari_ma, t="l", main="AR(1) coefficient = 1.0")
# Perform ADF test with lag = 1
tseries::adf.test(ari_ma, k=1)
# Perform standard Dickey-Fuller test
tseries::adf.test(ari_ma, k=0)
# Simulate AR(1) with coefficient close to 1, without unit root
ari_ma <- filter(x=in_nov, filter=0.99, method="recursive")
x11(); plot(ari_ma, t="l", main="AR(1) coefficient = 0.99")
tseries::adf.test(ari_ma, k=1)
# Simulate Ornstein-Uhlenbeck OU process with mean reversion
eq_price <- 0.0; the_ta <- 0.001
price_s <- HighFreq::sim_ou(eq_price=eq_price, vol_at=1.0, the_ta=the_ta, in_nov=in_nov)
x11(); plot(price_s, t="l", main=paste("OU coefficient =", the_ta))
tseries::adf.test(price_s, k=1)
# Simulate Ornstein-Uhlenbeck OU process with zero reversion
the_ta <- 0.0
price_s <- HighFreq::sim_ou(eq_price=eq_price, vol_at=1.0, the_ta=the_ta, in_nov=in_nov)
x11(); plot(price_s, t="l", main=paste("OU coefficient =", the_ta))
tseries::adf.test(price_s, k=1)

# Calculate XLB and XLE prices
price_s <- na.omit(rutils::etf_env$price_s[, c("XLB", "XLE")])
xl_b <- drop(zoo::coredata(price_s$XLB))
xl_e <- drop(zoo::coredata(price_s$XLE))
# Calculate regression coefficients of XLB ~ XLE
be_ta <- cov(xl_b, xl_e)/var(xl_e)
al_pha <- (mean(xl_b) - be_ta*mean(xl_e))
# Calculate regression residuals
fit_ted <- (al_pha + be_ta*xl_e)
residual_s <- (xl_b - fit_ted)
# Perform ADF test on residuals
tseries::adf.test(residual_s, k=1)

# Simulate random walks using apply() loops
set.seed(1121)  # Initialize random number generator
rand_walks <- matrix(rnorm(1000*100), ncol=1000)
rand_walks <- apply(rand_walks, 2, cumsum)
vari_ance <- apply(rand_walks, 1, var)
# Simulate random walks using vectorized functions
set.seed(1121)  # Initialize random number generator
rand_walks <- matrixStats::colCumsums(matrix(rnorm(1000*100), ncol=1000))
vari_ance <- matrixStats::rowVars(rand_walks)
par(mar=c(5, 3, 2, 2), oma=c(0, 0, 0, 0))
plot(vari_ance, xlab="time steps", ylab="",
     t="l", col="blue", lwd=2,
     main="Variance of Random Walk")

x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0))
# Simulate AR(1) process
ari_ma <- arima.sim(n=1e3, model=list(ar=0.8))
# ACF of AR(1) process
ac_f <- rutils::plot_acf(ari_ma, lag=10, xlab="", ylab="",
  main="Autocorrelations of AR(1) process")
ac_f$acf[1:5]

# PACF of AR(1) process
pac_f <- pacf(ari_ma, lag=10, xlab="", ylab="", main="")
title("Partial autocorrelations of AR(1) process", line=1)
pac_f <- drop(pac_f$acf)
pac_f[1:5]

# Compute pacf recursively from acf
ac_f <- rutils::plot_acf(ari_ma, lag=10, plo_t=FALSE)
ac_f <- drop(ac_f$acf)
pac_f <- numeric(3)
pac_f[1] <- ac_f[1]
pac_f[2] <- ac_f[2] - ac_f[1]^2
pac_f[3] <- ac_f[3] - pac_f[2]*ac_f[1] - ac_f[2]*pac_f[1]
# Compute pacf recursively in a loop
pac_f <- numeric(NROW(ac_f))
pac_f[1] <- ac_f[1]
for (it in 2:NROW(pac_f)) {
  pac_f[it] <- ac_f[it] - pac_f[1:(it-1)] %*% ac_f[(it-1):1]
}  # end for

par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # Set plot panels
# Simulate AR process of returns
ari_ma <- arima.sim(n=1e3, model=list(ar=c(0.1, 0.5, 0.1)))
# ACF of AR(3) process
rutils::plot_acf(ari_ma, lag=10, xlab="", ylab="",
   main="ACF of AR(3) process")
# PACF of AR(3) process
pacf(ari_ma, lag=10, xlab="", ylab="",
     main="PACF of AR(3) process")

# Specify AR process parameters
n_rows <- 1e3
co_eff <- c(0.1, 0.39, 0.5); n_coeff <- NROW(co_eff)
set.seed(1121); in_nov <- rnorm(n_rows)
# ari_ma <- filter(x=in_nov, filter=co_eff, method="recursive")
# Simulate AR process using C_rfilter()
ari_ma <- .Call(stats:::C_rfilter, in_nov, co_eff,
  double(n_rows + n_coeff))[-(1:n_coeff)]
# Fit AR model using ar.ols()
ar_fit <- ar.ols(ari_ma, order.max=n_coeff, aic=FALSE)
class(ar_fit)
is.list(ar_fit)
drop(ar_fit$ar)
# Define design matrix with intercept column
de_sign <- sapply(1:n_coeff, function(lagg) {
  rutils::lag_it(ari_ma, lagg=lagg)
})  # end sapply
de_sign <- cbind(rep(1, n_rows), de_sign)
# Fit AR model using regression
design_inv <- MASS::ginv(de_sign)
coeff_fit <- drop(design_inv %*% ari_ma)
all.equal(drop(ar_fit$ar), coeff_fit[-1], check.attributes=FALSE)
# Define design matrix without intercept column
de_sign <- sapply(1:n_coeff, function(lagg) {
  rutils::lag_it(ari_ma, lagg=lagg)
})  # end sapply
# Fit AR model using regression
design_inv <- MASS::ginv(de_sign)
coeff_fit <- drop(design_inv %*% ari_ma)
all.equal(drop(ar_fit$ar), coeff_fit, check.attributes=FALSE)

# Calculate the regression residuals
fit_ted <- drop(de_sign %*% coeff_fit)
residual_s <- drop(ari_ma - fit_ted)
# Variance of residuals
var_resid <- sum(residual_s^2)/(n_rows - NROW(coeff_fit))
# Design matrix squared
design_2 <- crossprod(de_sign)
# Calculate covariance matrix of AR coefficients
co_var <- var_resid*MASS::ginv(design_2)
coeff_fitd <- sqrt(diag(co_var))
# Calculate t-values of AR coefficients
coeff_tvals <- drop(coeff_fit)/coeff_fitd

# Fit AR(5) model into AR(3) process
de_sign <- sapply(1:5, function(lagg) {
  rutils::lag_it(ari_ma, lagg=lagg)
})  # end sapply
de_sign <- cbind(rep(1, n_rows), de_sign)
design_inv <- MASS::ginv(de_sign)
coeff_fit <- drop(design_inv %*% ari_ma)
# Calculate t-values of AR(5) coefficients
residual_s <- drop(ari_ma - drop(de_sign %*% coeff_fit))
var_resid <- sum(residual_s^2)/(n_rows - NROW(coeff_fit))
co_var <- var_resid*MASS::ginv(crossprod(de_sign))
coeff_fitd <- sqrt(diag(co_var))
coeff_tvals <- drop(coeff_fit)/coeff_fitd
# Fit AR(5) model using arima()
arima_fit <- arima(ari_ma, order=c(5, 0, 0), include.mean=FALSE)
arima_fit$coef
# Fit AR(5) model using auto.arima()
library(forecast)  # Load forecast
arima_fit <- forecast::auto.arima(ari_ma, max.p=5, max.q=0, max.d=0)
# Fit AR(5) model into VTI returns
re_turns <- drop(zoo::coredata(na.omit(rutils::etf_env$re_turns$VTI)))
de_sign <- sapply(1:5, function(lagg) {
  rutils::lag_it(re_turns, lagg=lagg)
})  # end sapply
de_sign <- cbind(rep(1, NROW(re_turns)), de_sign)
design_inv <- MASS::ginv(de_sign)
coeff_fit <- drop(design_inv %*% re_turns)
# Calculate t-values of AR(5) coefficients
residual_s <- drop(re_turns - drop(de_sign %*% coeff_fit))
var_resid <- sum(residual_s^2)/(n_rows - NROW(coeff_fit))
co_var <- var_resid*MASS::ginv(crossprod(de_sign))
coeff_fitd <- sqrt(diag(co_var))
coeff_tvals <- drop(coeff_fit)/coeff_fitd

# Compute autocorrelation coefficients
ac_f <- acf(ari_ma, lag=10, plot=FALSE)
ac_f <- drop(ac_f$acf)
acf1 <- ac_f[-NROW(ac_f)]
# Define Yule-Walker matrix
yule_walker <- sapply(2:9, function(lagg) {
  c(acf1[lagg:1], acf1[2:(NROW(acf1)-lagg+1)])
})  # end sapply
yule_walker <- cbind(acf1, yule_walker, rev(acf1))
# Generalized inverse of Yule-Walker matrix
yule_walker_inv <- MASS::ginv(yule_walker)
# Solve Yule-Walker equations
coeff_yw <- drop(yule_walker_inv %*% ac_f[-1])
round(coeff_yw, 5)
coeff_fit

n_rows <- 1e2
co_eff <- c(0.1, 0.39, 0.5); n_coeff <- NROW(co_eff)
set.seed(1121); in_nov <- rnorm(n_rows)
# Simulate AR process using filter()
ari_ma <- filter(x=in_nov, filter=co_eff, method="recursive")
ari_ma <- as.numeric(ari_ma)
# Simulate AR process using C_rfilter()
arima_fast <- .Call(stats:::C_rfilter, in_nov, co_eff,
  double(n_rows + n_coeff))
all.equal(ari_ma, arima_fast[-(1:n_coeff)],
  check.attributes=FALSE)

# Forecast AR(3) process using loop in R
forecast_s <- numeric(NROW(ari_ma)+1)
forecast_s[1] <- 0
forecast_s[2] <- co_eff[1]*ari_ma[1]
forecast_s[3] <- co_eff[1]*ari_ma[2] + co_eff[2]*ari_ma[1]
for (it in 4:NROW(forecast_s)) {
  forecast_s[it] <- ari_ma[(it-1):(it-3)] %*% co_eff
}  # end for
# Plot with legend
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
plot(ari_ma, main="Forecasting Using AR(3) Model",
  xlab="", ylab="", type="l")
lines(forecast_s, col="orange", lwd=3)
legend(x="topright", legend=c("series", "forecasts"),
 col=c("black", "orange"), lty=1, lwd=6,
 cex=0.9, bg="white", bty="n")

# Forecast using filter()
filter_fast <- filter(x=ari_ma, sides=1,
  filter=co_eff, method="convolution")
filter_fast <- as.numeric(filter_fast)
# Compare excluding warmup period
all.equal(forecast_s[-(1:n_coeff)], filter_fast[-(1:(n_coeff-1))],
    check.attributes=FALSE)
# Filter using C_cfilter() compiled C++ function directly
filter_fast <- .Call(stats:::C_cfilter, ari_ma, filter=co_eff,
               sides=1, circular=FALSE)
# Compare excluding warmup period
all.equal(forecast_s[-(1:n_coeff)], filter_fast[-(1:(n_coeff-1))],
    check.attributes=FALSE)
# Define predictor matrix for forecasting
predic_tor <- sapply(0:(n_coeff-1), function(lagg) {
  rutils::lag_it(ari_ma, lagg=lagg)
})  # end sapply
# Forecast using predictor matrix
filter_fast <- c(0, drop(predic_tor %*% co_eff))
# Compare with loop in R
all.equal(forecast_s, filter_fast, check.attributes=FALSE)

# Fit ARIMA model using arima()
arima_fit <- arima(ari_ma, order=c(3,0,0), include.mean=FALSE)
arima_fit$coef
co_eff
# One-step-ahead forecast using predict.Arima()
pre_dict <- predict(arima_fit, n.ahead=1)
# Or directly call predict.Arima()
# pre_dict <- predict.Arima(arima_fit, n.ahead=1)
# Inspect the prediction object
class(pre_dict)
names(pre_dict)
class(pre_dict$pred)
unlist(pre_dict)
# One-step-ahead forecast using matrix algebra
fore_cast <- drop(ari_ma[n_rows:(n_rows-2)] %*% arima_fit$coef)
# Compare one-step-ahead forecasts
all.equal(pre_dict$pred[[1]], fore_cast)
# Get information about predict.Arima()
?stats:::predict.Arima

# Calculate the in-sample forecasting residuals
residual_s <- (ari_ma - forecast_s[-NROW(forecast_s)])
# Compare residuals with innovations
all.equal(in_nov, residual_s, check.attributes=FALSE)
plot(residual_s, t="l", lwd=3, xlab="", ylab="",
     main="ARIMA Forecast Errors")

# Define AR process parameters
n_rows <- 1e3
co_eff <- c(0.5, 0.0, 0.0); n_coeff <- NROW(co_eff)
set.seed(1121); in_nov <- rnorm(n_rows)
# Simulate AR process using C_rfilter()
ari_ma <- .Call(stats:::C_rfilter, in_nov, co_eff,
  double(n_rows + n_coeff))[-(1:n_coeff)]
# Define order of the AR(p) forecasting model
or_der <- 5
# Define predictor matrix for forecasting
de_sign <- sapply(1:or_der, rutils::lag_it, in_put=ari_ma)
de_sign <- cbind(rep(1, n_rows), de_sign)
colnames(de_sign) <- paste0("pred_", 1:NCOL(de_sign))
# Add response equal to series
de_sign <- cbind(ari_ma, de_sign)
colnames(de_sign)[1] <- "response"
# Specify length of look-back interval
look_back <- 100
# Invert the predictor matrix
rang_e <- (n_rows-look_back):(n_rows-1)
design_inv <- MASS::ginv(de_sign[rang_e, -1])
# Calculate fitted coefficients
coeff_fit <- drop(design_inv %*% de_sign[rang_e, 1])
# Calculate forecast
drop(de_sign[n_rows, -1] %*% coeff_fit)

# Perform rolling forecasting
forecast_s <- sapply(look_back:n_rows, function(now) {
  # Calculate look-back range
  star_t <- max(1, now-look_back)
  rang_e <- star_t:(now-1)
  # Invert the predictor matrix
  design_inv <- MASS::ginv(de_sign[rang_e, -1])
  # Calculate fitted coefficients
  coeff_fit <- drop(design_inv %*% de_sign[rang_e, 1])
  # Calculate forecast
  drop(de_sign[now, -1] %*% coeff_fit)
})  # end sapply
# Add warmup period
forecast_s <- c(numeric(look_back-1), forecast_s)
forecast_s[1:(look_back-1)] <- ari_ma[1:(look_back-1)]

# Mean squared error
mean((ari_ma - forecast_s)^2)
# Correlation
cor(forecast_s, ari_ma)
# Plot forecasting series with legend
x11(width=6, height=5)
par(mar=c(3, 3, 2, 0), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
plot(ari_ma[(n_rows-look_back):n_rows], xlab="", ylab="", type="l", lwd=2,
  main="Rolling Forecasting Using AR(5) Model")
lines(forecast_s[(n_rows-look_back):n_rows], col="orange", lwd=2)
legend(x="topright", legend=c("series", "forecasts"),
 col=c("black", "orange"), lty=1, lwd=6,
 cex=0.9, bg="white", bty="n")

# Define backtesting function
back_test <- function(se_ries, or_der=5, look_back=100) {
  n_rows <- NROW(se_ries)
  # Define predictor matrix for forecasting
  de_sign <- sapply(1:or_der, rutils::lag_it, in_put=se_ries)
  de_sign <- cbind(rep(1, n_rows), de_sign)
  # Add response equal to series
  de_sign <- cbind(se_ries, de_sign)
  # Perform rolling forecasting
  forecast_s <- sapply(look_back:n_rows, function(now) {
    # Calculate look-back range
    star_t <- max(1, now-look_back)
    rang_e <- star_t:(now-1)
    # Invert the predictor matrix
    design_inv <- MASS::ginv(de_sign[rang_e, -1])
    # Calculate fitted coefficients
    coeff_fit <- drop(design_inv %*% de_sign[rang_e, 1])
    # Calculate forecast
    drop(de_sign[now, -1] %*% coeff_fit)
  })  # end sapply
  # Add warmup period
  forecast_s <- c(numeric(look_back-1), forecast_s)
  c(mse=mean((se_ries - forecast_s)^2), cor=cor(forecast_s, se_ries))
}  # end back_test
# Apply the backtesting function
back_test(ari_ma, or_der=5, look_back=100)

look_backs <- seq(20, 200, 20)
back_tests <- sapply(look_backs, back_test,
        se_ries=ari_ma, or_der=or_der)
back_tests <- t(back_tests)
rownames(back_tests) <- look_backs
# Plot forecasting series with legend
plot(x=look_backs, y=back_tests[, 1],
  xlab="look-back", ylab="MSE", type="l", lwd=2,
  main="MSE of AR(5) Forecasting Model")

# Define Ornstein-Uhlenbeck parameters
eq_price <- 1.0; sigma_r <- 0.02
the_ta <- 0.01; n_rows <- 1000
drif_t <- the_ta*eq_price
theta_1 <- 1-the_ta
# Simulate Ornstein-Uhlenbeck process
in_nov <- rnorm(n_rows)
re_turns <- numeric(n_rows)
price_s <- numeric(n_rows)
price_s[1] <- sigma_r*in_nov[1]
for (i in 2:n_rows) {
  re_turns[i] <- the_ta*(eq_price - price_s[i-1]) + sigma_r*in_nov[i]
  price_s[i] <- price_s[i-1] + re_turns[i]
}  # end for

plot(price_s, type="l",
     xlab="time", ylab="prices",
     main="Ornstein-Uhlenbeck Process")
legend("topright",
 title=paste(c(paste0("sigma_r = ", sigma_r),
               paste0("eq_price = ", eq_price),
               paste0("the_ta = ", the_ta)),
             collapse="\n"),
 legend="", cex=0.8, inset=0.1, bg="white", bty="n")
abline(h=eq_price, col='red', lwd=2)

re_turns <- rutils::diff_it(price_s)
lag_prices <- rutils::lag_it(price_s)
for_mula <- re_turns ~ lag_prices
l_m <- lm(for_mula)
summary(l_m)
# Plot regression
plot(for_mula, main="OU Returns Versus Lagged Prices")
abline(l_m, lwd=2, col="red")

# Calculate volatility parameter
c(volatility=sigma_r, estimate=sd(re_turns))
# Extract OU parameters from regression
co_eff <- summary(l_m)$coefficients
# Calculate regression alpha and beta directly
be_ta <- cov(re_turns, lag_prices)/var(lag_prices)
al_pha <- (mean(re_turns) - be_ta*mean(lag_prices))
cbind(direct=c(alpha=al_pha, beta=be_ta), lm=co_eff[, 1])
all.equal(c(alpha=al_pha, beta=be_ta), co_eff[, 1],
    check.attributes=FALSE)
# Calculate regression standard errors directly
beta_s <- c(alpha=al_pha, beta=be_ta)
fit_ted <- (al_pha + be_ta*lag_prices)
residual_s <- (re_turns - fit_ted)
prices_squared <- sum((lag_prices - mean(lag_prices))^2)
beta_sd <- sqrt(sum(residual_s^2)/prices_squared/(n_rows-2))
alpha_sd <- sqrt(sum(residual_s^2)/(n_rows-2)*(1/n_rows + mean(lag_prices)^2/prices_squared))
cbind(direct=c(alpha_sd=alpha_sd, beta_sd=beta_sd), lm=co_eff[, 2])
all.equal(c(alpha_sd=alpha_sd, beta_sd=beta_sd), co_eff[, 2],
    check.attributes=FALSE)
# Compare mean reversion parameter theta
c(theta=(-the_ta), round(co_eff[2, ], 3))
# Compare equilibrium price mu
c(eq_price=eq_price, estimate=-co_eff[1, 1]/co_eff[2, 1])
# Compare actual and estimated parameters
co_eff <- cbind(c(the_ta*eq_price, -the_ta), co_eff[, 1:2])
rownames(co_eff) <- c("drift", "theta")
colnames(co_eff)[1] <- "actual"
round(co_eff, 4)

# Simulate Schwartz process
re_turns <- numeric(n_rows)
price_s <- numeric(n_rows)
price_s[1] <- eq_price
set.seed(1121)  # Reset random numbers
for (i in 2:n_rows) {
  re_turns[i] <- the_ta*(eq_price - price_s[i-1]) + sigma_r*rnorm(1)
  price_s[i] <- price_s[i-1]*exp(re_turns[i])
}  # end for

plot(price_s, type="l", xlab="time", ylab="prices",
     main="Schwartz Process")
legend("topright",
 title=paste(c(paste0("sigma_r = ", sigma_r),
               paste0("eq_price = ", eq_price),
               paste0("the_ta = ", the_ta)),
             collapse="\n"),
 legend="", cex=0.8, inset=0.12, bg="white", bty="n")
abline(h=eq_price, col='red', lwd=2)
