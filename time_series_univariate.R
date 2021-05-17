library(knitr)
opts_chunk$set(prompt=TRUE, eval=FALSE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='tiny', fig.width=4, fig.height=4)
options(width=80, dev='pdf')
options(digits=3)
thm <- knit_theme$get("acid")
knit_theme$set(thm)

# Get documentation for package tseries
packageDescription("tseries")  # Get short description

help(package="tseries")  # Load help page

library(tseries)  # Load package tseries

data(package="tseries")  # List all datasets in "tseries"

ls("package:tseries")  # List all objects in "tseries"

detach("package:tseries")  # Remove tseries from search path

par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
load(file="C:/Develop/lecture_slides/data/zoo_data.RData")
# Get start and end dates
date_s <- time(ts_stx_adj)
e_nd <- date_s[NROW(date_s)]
st_art <- round((4*e_nd + date_s[1])/5)
# Plot using plotOHLC
plotOHLC(window(ts_stx_adj,
          start=st_art,
          end=e_nd)[, 1:4],
   xlab="", ylab="")
title(main="MSFT OHLC Prices")

par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
library(lubridate)  # Load lubridate
# Get start and end dates of ms_ft
start_date <- decimal_date(start(ms_ft))
end_date <- decimal_date(end(ms_ft))
# Calculate frequency of ms_ft
fre_quency <-
  NROW(ms_ft)/(end_date-start_date)
# Extract data from ms_ft
da_ta <- zoo::coredata(
  window(ms_ft, start=as.Date("2015-01-01"),
   end=end(ms_ft)))
# Create ts object using ts()
ts_stx <- ts(data=da_ta,
  start=decimal_date(as.Date("2015-01-01")),
  frequency=fre_quency)
seqplot.ts(x=ts_stx[, 1], y=ts_stx[, 4], xlab="", ylab="")
title(main="MSFT Open and Close Prices", line=-1)

library(tseries)  # Load package tseries
# Calculate maximum drawdown
maxdrawdown(msft_adj[, "AdjClose"])
max_drawd <- maxdrawdown(msft_adj[, "AdjClose"])
index(msft_adj)[max_drawd$from]
index(msft_adj)[max_drawd$to]
# Calculate Sharpe ratio
sharpe(msft_adj[, "AdjClose"])
# Calculate Sterling ratio
sterling(as.numeric(msft_adj[, "AdjClose"]))

ms_ft <- suppressWarnings(  # Load MSFT data
  get.hist.quote(instrument="MSFT",
           start=Sys.Date()-365,
           end=Sys.Date(),
           origin="1970-01-01")
)  # end suppressWarnings
class(ms_ft)
dim(ms_ft)
tail(ms_ft, 4)

# Calculate Sharpe ratio
sharpe(ms_ft[, "Close"], r=0.01)
# Add title
plot(ms_ft[, "Close"], xlab="", ylab="")
title(main="MSFT Close Prices", line=-1)

library(tseries)  # Load package tseries
ms_ft <- suppressWarnings(  # Load MSFT data
  get.hist.quote(instrument="MSFT",
           start=Sys.Date()-365,
           end=Sys.Date(),
           origin="1970-01-01")
)  # end suppressWarnings
class(ms_ft)
dim(ms_ft)
tail(ms_ft, 4)

# Calculate Sharpe ratio
sharpe(ms_ft[, "Close"], r=0.01)
# Add title
plot(ms_ft[, "Close"], xlab="", ylab="")
title(main="MSFT Close Prices", line=-1)

library(tseries)  # Load package tseries
ms_ft <- suppressWarnings(  # Load MSFT data
  get.hist.quote(instrument="MSFT",
           start=Sys.Date()-365,
           end=Sys.Date(),
           origin="1970-01-01")
)  # end suppressWarnings
class(ms_ft)
dim(ms_ft)
tail(ms_ft, 4)

# Calculate Sharpe ratio
sharpe(ms_ft[, "Close"], r=0.01)
# Add title
plot(ms_ft[, "Close"], xlab="", ylab="")
title(main="MSFT Close Prices", line=-1)

# Load package quantmod
library(quantmod)
# Get documentation for package quantmod
# Get short description
packageDescription("quantmod")
# Load help page
help(package="quantmod")
# List all datasets in "quantmod"
data(package="quantmod")
# List all objects in "quantmod"
ls("package:quantmod")
# Remove quantmod from search path
detach("package:quantmod")

library(quantmod)
# Plot OHLC candlechart with volume
chartSeries(etf_env$VTI["2014-11"],
      name="VTI",
      theme=chartTheme("white"))
# Plot OHLC bar chart with volume
chartSeries(etf_env$VTI["2014-11"],
      type="bars",
      name="VTI",
      theme=chartTheme("white"))

library(quantmod)
# Plot OHLC candlechart with volume
chartSeries(etf_env$VTI["2008-11/2009-04"], name="VTI")
# Redraw plot only for Feb-2009, with white theme
reChart(subset="2009-02", theme=chartTheme("white"))

library(quantmod)
# Candlechart with Bollinger Bands
chartSeries(etf_env$VTI["2014"],
      TA="addBBands(): addBBands(draw='percent'): addVo()",
      name="VTI with Bollinger Bands",
      theme=chartTheme("white"))
# Candlechart with two Moving Averages
chartSeries(etf_env$VTI["2014"],
      TA="addVo(): addEMA(10): addEMA(30)",
      name="VTI with Moving Averages",
      theme=chartTheme("white"))
# Candlechart with Commodity Channel Index
chartSeries(etf_env$VTI["2014"],
      TA="addVo(): addBBands(): addCCI()",
      name="VTI with Technical Indicators",
      theme=chartTheme("white"))

library(quantmod)
library(TTR)
oh_lc <- rutils::etf_env$VTI["2009-02/2009-03"]
VTI_close <- quantmod::Cl(oh_lc)
VTI_vol <- quantmod::Vo(oh_lc)
# Calculate volume-weighted average price
v_wap <- TTR::VWAP(price=VTI_close, volume=VTI_vol, n=10)
# Plot OHLC candlechart with volume
chartSeries(oh_lc, name="VTI plus VWAP", theme=chartTheme("white"))
# Add VWAP to main plot
addTA(ta=v_wap, on=1, col='red')
# Add price minus VWAP in extra panel
addTA(ta=(VTI_close-v_wap), col='red')

library(quantmod)
library(TTR)
oh_lc <- rutils::etf_env$VTI
VTI_close <- quantmod::Cl(oh_lc)
VTI_vol <- quantmod::Vo(oh_lc)
v_wap <- TTR::VWAP(price=VTI_close, volume=VTI_vol, n=10)
VTI_close <- VTI_close["2009-02/2009-03"]
oh_lc <- oh_lc["2009-02/2009-03"]
v_wap <- v_wap["2009-02/2009-03"]
# Plot OHLC candlechart with volume
chartSeries(oh_lc, name="VTI plus VWAP shaded",
      theme=chartTheme("white"))
# Add VWAP to main plot
addTA(ta=v_wap, on=1, col='red')
# Add price minus VWAP in extra panel
addTA(ta=(VTI_close-v_wap), col='red')
# Add background shading of areas
addTA((VTI_close-v_wap) > 0, on=-1,
col="lightgreen", border="lightgreen")
addTA((VTI_adj-v_wap) < 0, on=-1,
col="lightgrey", border="lightgrey")
# Add vertical and horizontal lines at v_wap minimum
addLines(v=which.min(v_wap), col='red')
addLines(h=min(v_wap), col='red')

library(quantmod)
library(TTR)
oh_lc <- rutils::etf_env$VTI
VTI_adj <- quantmod::Cl(oh_lc)
VTI_vol <- quantmod::Vo(oh_lc)
v_wap <- TTR::VWAP(price=VTI_adj, volume=VTI_vol, n=10)
VTI_adj <- VTI_adj["2009-02/2009-03"]
oh_lc <- oh_lc["2009-02/2009-03"]
v_wap <- v_wap["2009-02/2009-03"]
# OHLC candlechart VWAP in main plot,
chart_Series(x=oh_lc, # Volume in extra panel
       TA="add_Vo(); add_TA(v_wap, on=1)",
       name="VTI plus VWAP shaded")
# Add price minus VWAP in extra panel
add_TA(VTI_adj-v_wap, col='red')
# Add background shading of areas
add_TA((VTI_adj-v_wap) > 0, on=-1,
col="lightgreen", border="lightgreen")
add_TA((VTI_adj-v_wap) < 0, on=-1,
col="lightgrey", border="lightgrey")
# Add vertical and horizontal lines
abline(v=which.min(v_wap), col='red')
abline(h=min(v_wap), col='red')

library(quantmod)
oh_lc <- rutils::etf_env$VTI["2009-02/2009-03"]
# Extract plot object
ch_ob <- chart_Series(x=oh_lc, plot=FALSE)
class(ch_ob)
ls(ch_ob)
class(ch_ob$get_ylim)
class(ch_ob$set_ylim)
# ls(ch_ob$Env)
class(ch_ob$Env$actions)
plot_theme <- chart_theme()
class(plot_theme)
ls(plot_theme)

library(quantmod)
oh_lc <- rutils::etf_env$VTI["2010-04/2010-05"]
# Extract, modify theme, format tick marks "%b %d"
plot_theme <- chart_theme()
plot_theme$format.labels <- "%b %d"
# Create plot object
ch_ob <- chart_Series(x=oh_lc, theme=plot_theme, plot=FALSE)
# Extract ylim using accessor function
y_lim <- ch_ob$get_ylim()
y_lim[[2]] <- structure(range(quantmod::Cl(oh_lc)) + c(-1, 1),
  fixed=TRUE)
# Modify plot object to reduce y-axis range
ch_ob$set_ylim(y_lim)  # use setter function
# Render the plot
plot(ch_ob)

library(rutils)
# Calculate VTI and XLF volume-weighted average price
v_wap <- TTR::VWAP(price=quantmod::Cl(rutils::etf_env$VTI),
      volume=quantmod::Vo(rutils::etf_env$VTI), n=10)
XLF_vwap <- TTR::VWAP(price=quantmod::Cl(rutils::etf_env$XLF),
      volume=quantmod::Vo(rutils::etf_env$XLF), n=10)
# Open graphics device, and define
# Plot area with two horizontal panels
x11(); par(mfrow=c(2, 1))
ch_ob <- chart_Series(  # Plot in top panel
  x=etf_env$VTI["2009-02/2009-04"],
  name="VTI", plot=FALSE)
add_TA(v_wap["2009-02/2009-04"], lwd=2, on=1, col='blue')
# Plot in bottom panel
ch_ob <- chart_Series(x=etf_env$XLF["2009-02/2009-04"],
  name="XLF", plot=FALSE)
add_TA(XLF_vwap["2009-02/2009-04"], lwd=2, on=1, col='blue')

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

# Open plot window and set plot margins
x11(width=6, height=5)
par(mar=c(2, 2, 2, 2), oma=c(1, 1, 1, 1))
# Plot first time series without x-axis
zoo::plot.zoo(price_s[, 1], lwd=2, col="orange",
        xlab=NA, ylab=NA, xaxt="n")
# Create X-axis date labels and add X-axis
date_s <- pretty(index(price_s))
axis(side=1, at=date_s, labels=format(date_s, "%b-%d-%y"))
# Plot second time series without y-axis
par(new=TRUE)  # Allow new line on same plot
zoo::plot.zoo(price_s[, 2], xlab=NA, ylab=NA,
        lwd=2, yaxt="n", col="blue", xaxt="n")
# Plot second y-axis on right
axis(side=4, lwd=2, col="blue")
# Add axis labels
mtext(col_names[1], cex=1.2, lwd=3, side=2, las=2, adj=(-0.5), padj=(-5), col="orange")
mtext(col_names[2], cex=1.2, lwd=3, side=4, las=2, adj=1.5, padj=(-5), col="blue")
# Add title and legend
title(main=paste(col_names, collapse=" and "), line=0.5)
legend("top", legend=col_names, cex=1.0, bg="white",
 lty=1, lwd=6, col=c("orange", "blue"), bty="n")

# Load package qmao
library(qmao)
# Get documentation for package qmao
# Get short description
packageDescription("qmao")
# Load help page
help(package="qmao")
# List all datasets in "qmao"
data(package="qmao")
# List all objects in "qmao"
ls("package:qmao")
# Remove qmao from search path
detach("package:qmao")

set.seed(1121)  # Reset random number generator
# Sample from Standard Normal Distribution
n_rows <- 1000
da_ta <- rnorm(n_rows)
# Sample mean - MC estimate
mean(da_ta)
# Sample standard deviation - MC estimate
sd(da_ta)
# Monte Carlo estimate of cumulative probability
pnorm(1)
sum(da_ta < 1)/n_rows
# Monte Carlo estimate of quantile
conf_level <- 0.99
qnorm(conf_level)
cut_off <- conf_level*n_rows
da_ta <- sort(da_ta)
da_ta[cut_off]
quantile(da_ta, probs=conf_level)
# Analyze the source code of quantile()
stats:::quantile.default
# Microbenchmark quantile
library(microbenchmark)
summary(microbenchmark(
  monte_carlo=da_ta[cut_off],
  quan_tile=quantile(da_ta, probs=conf_level),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary

set.seed(1121)  # Reset random number generator
bar_rier <- 20  # Barrier level
n_rows <- 1000  # Number of simulation steps
pa_th <- numeric(n_rows)  # Allocate path vector
pa_th[1] <- 0  # Initialize path
in_dex <- 2  # Initialize simulation index
while ((in_dex <= n_rows) && (pa_th[in_dex - 1] < bar_rier)) {
# Simulate next step
  pa_th[in_dex] <- pa_th[in_dex - 1] + rnorm(1)
  in_dex <- in_dex + 1  # Advance in_dex
}  # end while
# Fill remaining pa_th after it crosses bar_rier
if (in_dex <= n_rows)
  pa_th[in_dex:n_rows] <- pa_th[in_dex - 1]
# Plot the Brownian motion
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(1, 1, 1, 1))
plot(pa_th, type="l", col="black",
     lty="solid", lwd=2, xlab="", ylab="")
abline(h=bar_rier, lwd=3, col="red")
title(main="Brownian Motion Crossing a Barrier Level", line=0.5)

set.seed(1121)  # Reset random number generator
bar_rier <- 20  # Barrier level
n_rows <- 1000  # Number of simulation steps
# Simulate path of Brownian motion
pa_th <- cumsum(rnorm(n_rows))
# Find index when pa_th crosses bar_rier
cro_ss <- which(pa_th > bar_rier)
# Fill remaining pa_th after it crosses bar_rier
if (NROW(cro_ss)>0) {
  pa_th[(cro_ss[1]+1):n_rows] <- pa_th[cro_ss[1]]
}  # end if
# Plot the Brownian motion
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(1, 1, 1, 1))
plot(pa_th, type="l", col="black",
     lty="solid", lwd=2, xlab="", ylab="")
abline(h=bar_rier, lwd=3, col="red")
title(main="Brownian Motion Crossing a Barrier Level", line=0.5)

# Define daily volatility and growth rate
sigma_r <- 0.01; dri_ft <- 0.0; n_rows <- 1000
# Simulate geometric Brownian motion
re_turns <- sigma_r*rnorm(n_rows) + dri_ft - sigma_r^2/2
price_s <- exp(cumsum(re_turns))
plot(price_s, type="l", xlab="time", ylab="prices",
     main="geometric Brownian motion")

# Simulate geometric Brownian motion
sigma_r <- 0.01/sqrt(48)
dri_ft <- 0.0
n_rows <- 1e4
date_s <- seq(from=as.POSIXct(paste(Sys.Date()-250, "09:30:00")),
  length.out=n_rows, by="30 min")
price_s <- exp(cumsum(sigma_r*rnorm(n_rows) + dri_ft - sigma_r^2/2))
price_s <- xts(price_s, order.by=date_s)
price_s <- cbind(price_s,
  volume=sample(x=10*(2:18), size=n_rows, replace=TRUE))
# Aggregate to daily OHLC data
oh_lc <- xts::to.daily(price_s)
quantmod::chart_Series(oh_lc, name="random prices")
# dygraphs candlestick plot using pipes syntax
library(dygraphs)
dygraphs::dygraph(oh_lc[, 1:4]) %>% dyCandlestick()
# dygraphs candlestick plot without using pipes syntax
dygraphs::dyCandlestick(dygraphs::dygraph(oh_lc[, 1:4]))

# Standard deviations of log-normal distribution
sig_mas <- c(0.5, 1, 1.5)
# Create plot colors
col_ors <- c("black", "red", "blue")
# Plot all curves
for (in_dex in 1:NROW(sig_mas)) {
  curve(expr=dlnorm(x, sdlog=sig_mas[in_dex]),
  type="l", lwd=2, xlim=c(0, 3),
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
price_s <- zoo::na.locf(price_s, na.rm=FALSE)
price_s <- zoo::na.locf(price_s, fromLast=TRUE)
sum(is.na(price_s))
# Drop ".Close" from column names
colnames(price_s[, 1:4])
colnames(price_s) <- rutils::get_name(colnames(price_s))
# Or
# colnames(price_s) <- do.call(rbind,
#   strsplit(colnames(price_s), split="[.]"))[, 1]
# Normalize columns
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
val_id <- (price_s != 1)  # Valid stocks
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

x11(width=6, height=5)
par(mar=c(3, 2, 1, 1), oma=c(1, 0, 0, 0))
re_turns <- na.omit(rutils::etf_env$re_turns$VTI)
# Plot autocorrelations using stats::acf()
stats::acf(re_turns, lag=10, xlab="lag", main="")
title(main="ACF of VTI Returns", line=-1)
# Two-tailed 95% confidence interval
qnorm(0.975)/sqrt(NROW(re_turns))

# Calculate VTI and XLF percentage returns
re_turns <- rutils::etf_env$re_turns[, c("VTI", "XLF")]
re_turns <- na.omit(re_turns)
n_rows <- NROW(re_turns)
# De-mean (center) and scale the returns
re_turns <- apply(re_turns, MARGIN=2,
  function(x) (x-mean(x))/sd(x))
apply(re_turns, MARGIN=2, sd)
# Calculate the correlation
drop(re_turns[, "VTI"] %*% re_turns[, "XLF"])/(n_rows-1)
co_r <- cor(re_turns[, "VTI"], re_turns[, "XLF"])
# Test statistical significance of correlation
cor_test <- cor.test(re_turns[, "VTI"], re_turns[, "XLF"])
conf_int <- qnorm((1+0.95)/2)/sqrt(n_rows)
co_r*c(1-conf_int, 1+conf_int)

# Get source code
stats:::cor.test.default

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

plot_acf <- function(x_ts, lagg=10, plo_t=TRUE,
               xlab="Lag", ylab="", main="", ...) {
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
  }  # end if
  # Return the ACF data invisibly
  invisible(acf_data)
}  # end plot_acf

# Improved autocorrelation function
x11(width=6, height=5)
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
# Coerce to "zoo"
macro_zoo <- as.zoo(Macrodat[, c("lhur", "fygm3")])
colnames(macro_zoo) <- c("unemprate", "3mTbill")
# ggplot2 in multiple panes
autoplot(  # Generic ggplot2 for "zoo"
  object=macro_zoo, main="US Macro",
  facets=Series ~ .) + # end autoplot
  xlab("") +
theme(  # Modify plot theme
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
theme(  # Modify plot theme
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
# AR time series of returns
ari_ma <- xts(x=arima.sim(n=NROW(date_s), model=list(ar=0.2)),
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
arima_fast <- filter(x=in_nov, filter=co_eff, method="recursive")
all.equal(arima_fast[-(1:warm_up)], as.numeric(ari_ma))
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
re_turns <- as.numeric(na.omit(rutils::etf_env$re_turns$VTI))
adf_test <- sapply(coeff_s, function(co_eff) {
  ari_ma <- filter(x=re_turns, filter=co_eff, method="recursive")
  ad_f <- suppressWarnings(tseries::adf.test(ari_ma))
  c(adf_stat=unname(ad_f$statistic), pval=ad_f$p.value)
})  # end sapply
x11(width=6, height=5)
par(mar=c(4, 4, 2, 2), oma=c(0, 0, 0, 0), mgp=c(2.5, 1, 0))
plot(x=coeff_s, y=adf_test["pval", ], main="ADF p-val Versus AR Coefficient",
     xlab="AR coefficient", ylab="ADF pval", t="l", col="blue", lwd=2)
plot(x=coeff_s, y=adf_test["adf_stat", ], main="ADF Stat Versus AR Coefficient",
     xlab="AR coefficient", ylab="ADF stat", t="l", col="blue", lwd=2)

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
pacf(ari_ma, lag=10, xlab="", ylab="", main="PACF of AR(3) process")

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
# Define design matrix without intercept column
de_sign <- sapply(1:n_coeff, rutils::lag_it, in_put=ari_ma)
# Fit AR model using regression
design_inv <- MASS::ginv(de_sign)
coeff_fit <- drop(design_inv %*% ari_ma)
all.equal(drop(ar_fit$ar), coeff_fit, check.attributes=FALSE)

# Specify AR process parameters
n_rows <- 1e3
co_eff <- c(0.1, 0.39, 0.5); n_coeff <- NROW(co_eff)
# Simulate AR process using C_rfilter()
set.seed(1121); in_nov <- rnorm(n_rows)
ari_ma <- .Call(stats:::C_rfilter, in_nov, co_eff,
  double(n_rows + n_coeff))[-(1:n_coeff)]


# wippp
# Calibrate ARIMA model using regression
# Define design matrix
ari_ma <- (ari_ma - mean(ari_ma))
de_sign <- sapply(1:3, rutils::lag_it, in_put=ari_ma)
# Calculate de-meaned re_turns matrix
de_sign <- t(t(de_sign) - colMeans(de_sign))
design_inv <- MASS::ginv(de_sign)
# Regression coefficients with response equal to ari_ma
coeff_fit <- drop(design_inv %*% ari_ma)

all.equal(arima_fit$coef, coeff_fit, check.attributes=FALSE)

# Calculate the regression residuals
fit_ted <- drop(de_sign %*% coeff_fit)
residual_s <- drop(ari_ma - fit_ted)
# Variance of residuals
var_resid <- sum(residual_s^2)/(n_rows-NROW(coeff_fit))
# Design matrix squared
design_2 <- crossprod(de_sign)
# Calculate covariance matrix of AR coefficients
co_var <- var_resid*MASS::ginv(design_2)
coeff_fitd <- sqrt(diag(co_var))
# Calculate t-values of AR coefficients
coeff_tvals <- drop(coeff_fit)/coeff_fitd

# Fit AR(5) model into AR(3) process
de_sign <- sapply(1:5, rutils::lag_it, in_put=ari_ma)
design_inv <- MASS::ginv(de_sign)
coeff_fit <- drop(design_inv %*% ari_ma)
# Calculate t-values of AR(5) coefficients
residual_s <- drop(ari_ma - drop(de_sign %*% coeff_fit))
var_resid <- sum(residual_s^2)/(n_rows-NROW(coeff_fit))
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
de_sign <- sapply(1:5, rutils::lag_it, in_put=re_turns)
design_inv <- MASS::ginv(de_sign)
coeff_fit <- drop(design_inv %*% re_turns)
# Calculate t-values of AR(5) coefficients
residual_s <- drop(re_turns - drop(de_sign %*% coeff_fit))
var_resid <- sum(residual_s^2)/(n_rows-NROW(coeff_fit))
co_var <- var_resid*MASS::ginv(crossprod(de_sign))
coeff_fitd <- sqrt(diag(co_var))
coeff_tvals <- drop(coeff_fit)/coeff_fitd

# Calibrate ARIMA model using auto.arima()
# library(forecast)  # Load forecast
forecast::auto.arima(ari_ma, max.p=3, max.q=0, max.d=0)
# Calibrate ARIMA model using arima()
arima_fit <- arima(ari_ma, order=c(3,0,0), include.mean=FALSE)
arima_fit$coef
# Calibrate ARIMA model using auto.arima()
# library(forecast)  # Load forecast
forecast::auto.arima(ari_ma, max.p=3, max.q=0, max.d=0)
# Calibrate ARIMA model using regression
ari_ma <- as.numeric(ari_ma)
# Define design matrix for ari_ma
de_sign <- sapply(1:3, rutils::lag_it, in_put=ari_ma)
# Generalized inverse of design matrix
design_inv <- MASS::ginv(de_sign)
# Regression coefficients with response equal to ari_ma
co_eff <- drop(design_inv %*% ari_ma)
all.equal(arima_fit$coef, co_eff, check.attributes=FALSE)

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

# Simulate AR process using filter()
n_rows <- 1e2
co_eff <- c(0.1, 0.39, 0.5); n_coeff <- NROW(co_eff)
set.seed(1121)
ari_ma <- filter(x=rnorm(n_rows), filter=co_eff, method="recursive")
ari_ma <- as.numeric(ari_ma)
# Forecast AR(3) process
forecast_s <- numeric(NROW(ari_ma))
forecast_s[2] <- co_eff[1]*ari_ma[1]
forecast_s[3] <- co_eff[1]*ari_ma[2] + co_eff[2]*ari_ma[1]
for (it in 4:NROW(forecast_s)) {
  forecast_s[it] <- ari_ma[(it-1):(it-3)] %*% co_eff
}  # end for
# Forecast using filter()
forecasts_filter <- filter(x=ari_ma, sides=1,
  filter=co_eff, method="convolution")
class(forecasts_filter)
all.equal(forecast_s[-(1:4)],
  forecasts_filter[-c(1:3, NROW(forecasts_filter))],
  check.attributes=FALSE)
# Compare residuals with innovations
residual_s <- (ari_ma-forecast_s)
tail(cbind(in_nov, residual_s))


# ari_ma <- as.numeric(lh)
# n_rows <- NROW(ari_ma)
# Compare one-step-ahead forecasts
# arima_fit <- arima(ari_ma, order=c(3,0,0), method="ML", include.mean=FALSE)

# Compare many one-step-ahead forecasts
forecast_s <- sapply(31:n_rows, function(x) {
  cat("len = ", x, "\n")
  # ari_ma <- filter(x=rnorm(n_rows+1), filter=co_eff, method="recursive")
  arima_fit <- arima(ari_ma[1:x], order=c(3,0,0), include.mean=FALSE)
  pre_dict <- predict(arima_fit, n.ahead=1)
  fore_cast <- drop(ari_ma[x:(x-2)] %*% arima_fit$coef)
  c(actual=ari_ma[x+1], forecast=fore_cast, predict=as.numeric(pre_dict$pred))
})  # end sapply
foo <- t(foo)
# hist(foo[, 1], breaks=30,
#   main="", ylim=c(0, 60), xlim=c(-0.04, 0.04),
#   xlab="", ylab="", freq=FALSE)

hist(foo[, 1], ylim=c(0, 0.15), freq=FALSE)
lines(density(foo[, 1]), col='blue', lwd=3)
lines(density(foo[, 2]), col='green', lwd=3)
lines(density(foo[, 3]), col='red', lwd=3)


# Forecast AR(3) process
forecast_s <- numeric(NROW(ari_ma))
forecast_s[2] <- co_eff[1]*ari_ma[1]
forecast_s[3] <- co_eff[1]*ari_ma[2] + co_eff[2]*ari_ma[1]
for (it in 4:NROW(forecast_s)) {
  forecast_s[it] <- ari_ma[(it-1):(it-3)] %*% co_eff
}  # end for
# Forecast using filter()
forecasts_filter <- filter(x=ari_ma, sides=1,
  filter=co_eff, method="convolution")
class(forecasts_filter)
all.equal(forecast_s[-(1:4)],
  forecasts_filter[-c(1:3, NROW(forecasts_filter))],
  check.attributes=FALSE)
# Compare residuals with innovations
residual_s <- (ari_ma-forecast_s)
tail(cbind(in_nov, residual_s))

# Plot with legend
plot(ari_ma, main="Forecasting Using AR(3) Model",
  xlab="", ylab="", type="l")
lines(forecast_s, col="orange", lwd=3)
legend(x="topright", legend=c("series", "forecasts"),
 col=c("black", "orange"), lty=1, lwd=6,
 cex=0.9, bg="white", bty="n")

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

# Calculate a vector of daily VTI log returns
re_turns <- na.omit(rutils::etf_env$re_turns$VTI)
date_s <- index(re_turns)
re_turns <- as.numeric(re_turns)
n_rows <- NROW(re_turns)
# Define predictor as a rolling sum
n_agg <- 5
predic_tor <- rutils::roll_sum(re_turns, look_back=n_agg)
# Shift the res_ponse forward out-of-sample
res_ponse <- rutils::lag_it(predic_tor, lagg=(-n_agg))
# Define predictor matrix for forecasting
order_max <- 5
predic_tor <- sapply(1+n_agg*(0:order_max), rutils::lag_it,
               in_put=predic_tor)
predic_tor <- cbind(rep(1, n_rows), predic_tor)
# Define de_sign matrix
de_sign <- cbind(res_ponse, predic_tor)
# Perform rolling forecasting
look_back <- 100
forecast_s <- sapply((look_back+1):n_rows, function(end_p) {
  # Define rolling look-back range
  start_p <- max(1, end_p-look_back)
  # Or expanding look-back range
  # start_p <- 1
  rang_e <- start_p:(end_p-1)
  # Invert the predictor matrix
  design_inv <- MASS::ginv(de_sign[rang_e, -1])
  # Calculate fitted coefficients
  coeff_fit <- drop(design_inv %*% de_sign[rang_e, 1])
  # Calculate forecast
  drop(de_sign[end_p, -1] %*% coeff_fit)
})  # end sapply
# Add warmup period
forecast_s <- c(rep(0, look_back), forecast_s)

# Mean squared error
mean((re_turns - forecast_s)^2)
# Correlation
cor(forecast_s, re_turns)
# Plot forecasting series with legend
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0))
plot(forecast_s[(n_rows-look_back):n_rows], col="red",
     xlab="", ylab="", type="l", lwd=2,
     main="Rolling Forecasting Using AR Model")
lines(re_turns[(n_rows-look_back):n_rows], col="blue", lwd=2)
legend(x="top", legend=c("re_turns", "forecasts"),
 col=c("blue", "red"), lty=1, lwd=6,
 cex=0.9, bg="white", bty="n")

# Define backtesting function
sim_forecasts <- function(res_ponse, predic_tor=res_ponse, n_agg=5,
                or_der=5, look_back=100) {
  n_rows <- NROW(res_ponse)
  # Define predictor as a rolling sum
  predic_tor <- rutils::roll_sum(res_ponse, look_back=n_agg)
  # Shift the res_ponse forward out-of-sample
  res_ponse <- rutils::lag_it(predic_tor, lagg=(-n_agg))
  # Define predictor matrix for forecasting
  predic_tor <- sapply(1+n_agg*(0:or_der), rutils::lag_it,
                 in_put=predic_tor)
  predic_tor <- cbind(rep(1, n_rows), predic_tor)
  # Define de_sign matrix
  de_sign <- cbind(res_ponse, predic_tor)
  # Perform rolling forecasting
  forecast_s <- sapply((look_back+1):n_rows, function(end_p) {
    # Define rolling look-back range
    start_p <- max(1, end_p-look_back)
    # Or expanding look-back range
    # start_p <- 1
    rang_e <- start_p:(end_p-1)
    # Invert the predictor matrix
    design_inv <- MASS::ginv(de_sign[rang_e, -1])
    # Calculate fitted coefficients
    coeff_fit <- drop(design_inv %*% de_sign[rang_e, 1])
    # Calculate forecast
    drop(de_sign[end_p, -1] %*% coeff_fit)
  })  # end sapply
  # Add warmup period
  forecast_s <- c(rep(0, look_back), forecast_s)
  rutils::roll_sum(forecast_s, look_back=n_agg)
}  # end sim_forecasts
# Simulate the rolling autoregressive forecasts
forecast_s <- sim_forecasts(re_turns, or_der=5, look_back=100)
c(mse=mean((re_turns - forecast_s)^2), cor=cor(re_turns, forecast_s))

look_backs <- seq(20, 200, 20)
back_tests <- sapply(look_backs, back_test, se_ries=ari_ma, or_der=or_der)
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
