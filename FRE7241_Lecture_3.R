price_s <- rutils::etf_env$VTI[, 4]
# Define look-back window and a half window
win_dow <- 11
# Calculate time series of medians
medi_an <- TTR::runMedian(price_s, n=win_dow)
# Calculate time series of z-scores
ma_d <- TTR::runMAD(price_s, n=win_dow)
z_scores <- (price_s - medi_an)/ma_d
z_scores[1:win_dow, ] <- 0
tail(z_scores, win_dow)
range(z_scores)

x11(width=6, height=5)
# Plot prices and medians
dygraphs::dygraph(cbind(price_s, medi_an), main="VTI median") %>%
  dyOptions(colors=c("black", "red"))
# Plot histogram of z-scores
histo_gram <- hist(z_scores, col="lightgrey",
  xlab="z-scores", breaks=50, xlim=c(-4, 4),
  ylab="frequency", freq=FALSE, main="Z-scores histogram")
lines(density(z_scores, adjust=1.5), lwd=3, col="blue")

# Calculate one-sided Hampel z-scores
medi_an <- TTR::runMedian(price_s, n=win_dow)
ma_d <- TTR::runMAD(price_s, n=win_dow)
z_scores <- (price_s - medi_an)/ma_d
z_scores[1:win_dow, ] <- 0
tail(z_scores, win_dow)
range(z_scores)
# Calculate two-sided Hampel z-scores
half_window <- win_dow %/% 2
medi_an <- rutils::lag_it(medi_an, lagg=-half_window)
ma_d <- rutils::lag_it(ma_d, lagg=-half_window)
z_scores <- (price_s - medi_an)/ma_d
z_scores[1:win_dow, ] <- 0
tail(z_scores, win_dow)
range(z_scores)

# Define threshold value
thresh_old <- 3
# Calculate number of prices classified as bad data
ba_d <- (abs(z_scores) > thresh_old)
sum(ba_d)
# Add 200 random price jumps into price_s
set.seed(1121)
n_bad <- 200
jump_s <- logical(NROW(price_s))
jump_s[sample(x=NROW(jump_s), size=n_bad)] <- TRUE
price_s[jump_s] <- price_s[jump_s]*
  sample(c(0.95, 1.05), size=n_bad, replace=TRUE)
# Plot prices and medians
dygraphs::dygraph(cbind(price_s, medi_an), main="VTI median") %>%
  dyOptions(colors=c("black", "red"))
# Calculate time series of z-scores
medi_an <- TTR::runMedian(price_s, n=win_dow)
ma_d <- TTR::runMAD(price_s, n=win_dow)
z_scores <- (price_s - medi_an)/ma_d
z_scores[1:win_dow, ] <- 0
# Calculate number of prices classified as bad data
ba_d <- (abs(z_scores) > thresh_old)
sum(ba_d)

# Calculate confusion matrix
table(jump_s, ba_d)
sum(ba_d)
# FALSE positive (type I error)
sum(jump_s & !ba_d)
# FALSE negative (type II error)
sum(!jump_s & ba_d)

# Confusion matrix as function of thresh_old
con_fuse <- function(res_ponse, z_scores, thresh_old) {
    confu_sion <- table(res_ponse, (abs(z_scores) > thresh_old))
    confu_sion <- confu_sion / rowSums(confu_sion)
    c(typeI=confu_sion[2, 1], typeII=confu_sion[1, 2])
  }  # end con_fuse
con_fuse(jump_s, z_scores, thresh_old=thresh_old)
# Define vector of thresholds
threshold_s <- seq(from=0.2, to=5.0, by=0.2)
# Calculate error rates
error_rates <- sapply(threshold_s, con_fuse,
  res_ponse=jump_s,
  z_scores=z_scores)  # end sapply
error_rates <- t(error_rates)
error_rates <- rbind(c(0, 1), error_rates)
error_rates <- rbind(error_rates, c(1, 0))
# Calculate area under ROC curve (AUC)
true_pos <- (1 - error_rates[, "typeII"])
true_pos <- (true_pos + rutils::lag_it(true_pos))/2
false_pos <- rutils::diff_it(error_rates[, "typeI"])
abs(sum(true_pos*false_pos))

# Plot ROC curve for Hampel classifier
plot(x=error_rates[, "typeI"],
     y=1-error_rates[, "typeII"],
     xlab="FALSE positive rate",
     ylab="TRUE positive rate",
     xlim=c(0, 1), ylim=c(0, 1),
     main="ROC Curve for Hampel Classifier",
     type="l", lwd=3, col="blue")
abline(a=0.0, b=1.0, lwd=3, col="orange")

# Define daily volatility and growth rate
sigma_r <- 0.01; dri_ft <- 0.0; n_rows <- 1000
# Simulate geometric Brownian motion
re_turns <- sigma_r*rnorm(n_rows) +
  dri_ft - sigma_r^2/2
price_s <- exp(cumsum(re_turns))
plot(price_s, type="l",
     xlab="periods", ylab="prices",
     main="geometric Brownian motion")

# Simulate geometric Brownian motion
sigma_r <- 0.01/sqrt(48)
dri_ft <- 0.0
n_rows <- 1e4
in_dex <- seq(from=as.POSIXct(paste(Sys.Date()-250, "09:30:00")),
  length.out=n_rows, by="30 min")
price_s <- xts(exp(cumsum(sigma_r*rnorm(n_rows) + dri_ft - sigma_r^2/2)),
  order.by=in_dex)
price_s <- cbind(price_s,
  volume=sample(x=10*(2:18), size=n_rows, replace=TRUE))
# Aggregate to daily OHLC data
oh_lc <- xts::to.daily(price_s)
quantmod::chart_Series(oh_lc, name="random prices")
# dygraphs candlestick plot using pipes syntax
library(dygraphs)
dygraphs::dygraph(oh_lc[, 1:4]) %>%
  dyCandlestick()
# dygraphs candlestick plot without using pipes syntax
dygraphs::dyCandlestick(dygraphs::dygraph(oh_lc[, 1:4]))

# Standard deviations of log-normal distribution
sig_mas <- c(0.5, 1, 1.5)
# Create plot colors
col_ors <- c("black", "red", "blue")
# Plot all curves
for (in_dex in 1:NROW(sig_mas)) {
  curve(expr=dlnorm(x, sdlog=sig_mas[in_dex]),
  type="l", xlim=c(0, 3), lwd=2,
  xlab="", ylab="", col=col_ors[in_dex],
  add=as.logical(in_dex-1))
}  # end for

# Add title and legend
title(main="Log-normal Distributions", line=0.5)
legend("topright", inset=0.05, title="Sigmas",
 paste("sigma", sig_mas, sep="="),
 cex=0.8, lwd=2, lty=rep(1, NROW(sig_mas)),
 col=col_ors)

x11(width=6, height=5)
par(mar=c(4, 4, 3, 1))
# Return volatility of VTI ETF
sigma_r <- sd(rutils::diff_it(log(rutils::etf_env$VTI[, 4])))
sigmar_2 <- sigma_r^2
n_rows <- NROW(rutils::etf_env$VTI)
# Standard deviation of log-normal prices
sqrt(n_rows)*sigma_r

# Skewness of log-normal prices
skew_ness <- function(t) {
  ex_p <- exp(t*sigmar_2)
  (ex_p + 2)*sqrt(ex_p - 1)
}  # end skew_ness
curve(expr=skew_ness, xlim=c(1, n_rows), lwd=3,
xlab="Number of days", ylab="Skewness", col="blue",
main="Skewness of Log-normal Prices
as a Function of Time")

# Probability that random log-normal price will be lower than the mean price
curve(expr=pnorm(sigma_r*sqrt(x)/2),
xlim=c(1, n_rows), lwd=3,
xlab="Number of days", ylab="Probability", col="blue",
main="Probability That Random Log-normal Price
Will be Lower Than the Mean Price")

# Define daily volatility and growth rate
sigma_r <- 0.01; dri_ft <- 0.0; n_rows <- 5000
path_s <- 10
# Simulate multiple paths of geometric Brownian motion
price_s <- matrix(sigma_r*rnorm(path_s*n_rows) +
    dri_ft - sigma_r^2/2, nc=path_s)
price_s <- exp(matrixStats::colCumsums(price_s))
# Create xts time series
price_s <- xts(price_s, order.by=seq.Date(Sys.Date()-NROW(price_s)+1, Sys.Date(), by=1))
# Plot xts time series
col_ors <- colorRampPalette(c("red", "blue"))(NCOL(price_s))
col_ors <- col_ors[order(order(price_s[NROW(price_s), ]))]
par(mar=c(3, 3, 2, 2), oma=c(0, 0, 0, 0))
plot.zoo(price_s, main="Multiple paths of geometric Brownian motion",
   xlab=NA, ylab=NA, plot.type="single", col=col_ors)

# Define daily volatility and growth rate
sigma_r <- 0.01; dri_ft <- 0.0; n_rows <- 10000
path_s <- 100
# Simulate multiple paths of geometric Brownian motion
price_s <- matrix(sigma_r*rnorm(path_s*n_rows) +
    dri_ft - sigma_r^2/2, nc=path_s)
price_s <- exp(matrixStats::colCumsums(price_s))
# Calculate percentage of paths below the expected value
per_centage <- rowSums(price_s < 1.0) / path_s
# Create xts time series of percentage of paths below the expected value
per_centage <- xts(per_centage, order.by=seq.Date(Sys.Date()-NROW(per_centage)+1, Sys.Date(), by=1))
# Plot xts time series of percentage of paths below the expected value
par(mar=c(3, 3, 2, 2), oma=c(0, 0, 0, 0))
plot.zoo(per_centage, main="Percentage of GBM paths below mean",
   xlab=NA, ylab=NA, col="blue")

# Load S&P500 stock prices
load("C:/Develop/lecture_slides/data/sp500.RData")
ls(sp500_env)
# Extract closing prices
price_s <- eapply(sp500_env, quantmod::Cl)
# Flatten price_s into a single xts series
price_s <- rutils::do_call(cbind, price_s)
# Carry forward and backward non-NA prices
price_s <- xts:::na.locf.xts(price_s)
price_s <- xts:::na.locf.xts(price_s, fromLast=TRUE)
sum(is.na(price_s))
# Rename and normalize columns
colnames(price_s) <- sapply(colnames(price_s),
  function(col_name) strsplit(col_name, split="[.]")[[1]][1])
price_s <- xts(t(t(price_s) / as.numeric(price_s[1, ])),
         order.by=index(price_s))
# Calculate permution index for sorting the lowest to highest final price_s
or_der <- order(price_s[NROW(price_s), ])
# Select a few symbols
sym_bols <- colnames(price_s)[or_der]
sym_bols <- sym_bols[seq.int(from=1, to=(NROW(sym_bols)-1), length.out=20)]

# Plot xts time series of price_s
col_ors <- colorRampPalette(c("red", "blue"))(NROW(sym_bols))
col_ors <- col_ors[order(order(price_s[NROW(price_s), sym_bols]))]
par(mar=c(3, 3, 2, 2), oma=c(0, 0, 0, 0))
plot.zoo(price_s[, sym_bols], main="20 S&P500 stock prices (normalized)",
   xlab=NA, ylab=NA, plot.type="single", col=col_ors)
legend(x="topleft", inset=0.05, cex=0.8,
 legend=rev(sym_bols), col=rev(col_ors), lwd=6, lty=1)

# Calculate average of valid stock prices
val_id <- (price_s != 1)  # valid stocks
n_stocks <- rowSums(val_id)
n_stocks[1] <- NCOL(price_s)
in_dex <- rowSums(price_s * val_id) / n_stocks
# Calculate percentage of stock prices below the average price
per_centage <- rowSums((price_s < in_dex) & val_id) / n_stocks
# Create xts time series of average stock prices
in_dex <- xts(in_dex, order.by=index(price_s))

x11(width=6, height=4)
par(mar=c(3, 3, 2, 2), oma=c(0, 0, 0, 0))
# Plot xts time series of average stock prices
plot.zoo(in_dex, main="Average S&P500 stock prices (normalized from 1990)",
   xlab=NA, ylab=NA, col="blue")
# Create xts time series of percentage of stock prices below the average price
per_centage <- xts(per_centage, order.by=index(price_s))
# Plot percentage of stock prices below the average price
plot.zoo(per_centage[-(1:2),],
   main="Percentage of S&P500 stock prices below the average price",
   xlab=NA, ylab=NA, col="blue")

x11(width=6, height=4)
par(mar=c(3, 3, 1, 3), oma=c(0, 0, 0, 0), mgp=c(2, 0.5, 0))
re_turns <- diff(log(as.numeric(EuStockMarkets[, 1])))
# Plot autocorrelations using stats::acf()
stats::acf(re_turns, lag=10, main="")
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

library(rutils)  # Load package rutils
par(mar=c(5,0,1,2), oma=c(1,2,1,0), mgp=c(2,1,0), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
# Improved autocorrelation function
rutils::plot_acf(re_turns, lag=10, main="")
title(main="acf of VTI returns", line=-1)
# Ljung-Box test for VTI returns
Box.test(re_turns, lag=10, type="Ljung")

x11(width=6, height=7)
par(mar=c(1, 1, 1, 1), oma=c(1, 1, 0, 1), mgp=c(0, 0.5, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # Set plot panels
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
# Apply convolution filter over past values (sides=1)
co_eff <- c(0.1, 0.39, 0.5)
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
# Simulate ARIMA using filter()
in_nov <- rnorm(NROW(price_s))
ari_ma <- filter(x=in_nov, filter=co_eff, method="recursive")
# Get information about C_rfilter()
getAnywhere(C_rfilter)
# Filter using C_rfilter() compiled C++ function directly
arima_fast <- .Call(stats:::C_rfilter, in_nov, co_eff,
              double(NROW(co_eff) + NROW(in_nov)))
all.equal(as.numeric(ari_ma), arima_fast[-(1:3)], check.attributes=FALSE)
# Benchmark speed of the two methods
summary(microbenchmark(
  fast_filter=.Call(stats:::C_rfilter, in_nov, co_eff, double(NROW(co_eff) + NROW(in_nov))),
  R_filter=filter(x=in_nov, filter=co_eff, method="recursive")
  ), times=10)[, c(1, 4, 5)]

library(rutils)  # Load package rutils
library(ggplot2)  # Load ggplot2
library(gridExtra)  # Load gridExtra
# Coerce to zoo and merge the time series
filter_ed <- cbind(as.zoo(price_s),
            as.zoo(filter_ed))
colnames(filter_ed) <- c("VTI", "VTI filtered")
autoplot(  # Plot ggplot2
    window(filter_ed, start=1997, end=1998),
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
title(main="ACF of VTI filtered Returns", line=-1)
# Plot ACF of VTI returns
rutils::plot_acf(re_turns[, 2], lag=10, xlab="")
title(main="ACF of VTI Filtered", line=-1)

# Simulate AR processes
set.seed(1121)  # Reset random numbers
in_dex <- Sys.Date() + 0:728  # Two year daily series
ari_ma <- xts(  # AR time series of returns
  x=arima.sim(n=NROW(in_dex), model=list(ar=0.2)),
  order.by=in_dex)
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
  arima.sim(n=NROW(in_dex), model=list(ar=phi))
})  # end sapply
colnames(ari_ma) <- paste("autocorr", ar_coeff)
plot.zoo(ari_ma, main="AR(1) prices", xlab=NA)
# Or plot using ggplot
ari_ma <- xts(x=ari_ma, order.by=in_dex)
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
# Simulate ARIMA using arima.sim()
ari_ma <- arima.sim(n=n_rows,
  model=list(ar=co_eff),
  start.innov=in_nov[1:warm_up],
  innov=in_nov[(warm_up+1):NROW(in_nov)])
# Simulate AR process using filter()
arima_fast <- filter(x=in_nov,
  filter=co_eff, method="recursive")
all.equal(arima_fast[-(1:warm_up)],
  as.numeric(ari_ma))
# Benchmark the speed of the three methods of simulating ARIMA
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

n_rows <- 1e3
# Perform ADF test for AR(1) with small coefficient
set.seed(1121)
ari_ma <- arima.sim(n=n_rows, model=list(ar=0.01))
tseries::adf.test(ari_ma)
# Perform ADF test for AR(1) with large coefficient
set.seed(1121)
ari_ma <- arima.sim(n=n_rows, model=list(ar=0.99))
tseries::adf.test(ari_ma)
# Perform ADF test with lag = 1
tseries::adf.test(ari_ma, k=1)
# Perform Dickey-Fuller test
tseries::adf.test(ari_ma, k=0)

# Simulate AR(1) process with different coefficients
coeff_s <- seq(0.99, 0.999, 0.001)
in_nov <- as.numeric(na.omit(rutils::etf_env$re_turns$VTI))
adf_test <- sapply(coeff_s, function(co_eff) {
  ari_ma <- filter(x=in_nov, filter=co_eff, method="recursive")
  ad_f <- suppressWarnings(tseries::adf.test(ari_ma))
  c(adf_stat=unname(ad_f$statistic), pval=ad_f$p.value)
})  # end sapply
x11(width=6, height=5)
par(mar=c(4, 4, 2, 2), oma=c(0, 0, 0, 0), mgp=c(2.5, 1, 0))
plot(x=coeff_s, y=adf_test["pval", ], main="ADF p-val Versus AR Coefficient",
     xlab="AR coefficient", ylab="ADF pval", t="l", col="blue", lwd=2)
plot(x=coeff_s, y=adf_test["adf_stat", ], main="ADF Stat Versus AR Coefficient",
     xlab="AR coefficient", ylab="ADF stat", t="l", col="blue", lwd=2)

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
