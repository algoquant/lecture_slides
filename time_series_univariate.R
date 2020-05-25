library(knitr)
opts_chunk$set(prompt=TRUE, eval=FALSE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
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
in_dex <- time(ts_stx_adj)
e_nd <- in_dex[NROW(in_dex)]
st_art <- round((4*e_nd + in_dex[1])/5)
# Plot using plotOHLC
plotOHLC(window(ts_stx_adj,
          start=st_art,
          end=e_nd)[, 1:4],
   xlab="", ylab="")
title(main="MSFT OHLC Prices")

par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
library(lubridate)  # Load lubridate
# Get start and end dates of zoo_series
start_date <- decimal_date(start(zoo_stx))
end_date <- decimal_date(end(zoo_stx))
# Calculate frequency of zoo_stx
fre_quency <-
  NROW(zoo_stx)/(end_date-start_date)
# Extract data from zoo_stx
da_ta <- zoo::coredata(
  window(zoo_stx, start=as.Date("2015-01-01"),
   end=end(zoo_stx)))
# Create ts object using ts()
ts_stx <- ts(data=da_ta,
  start=decimal_date(as.Date("2015-01-01")),
  frequency=fre_quency)
seqplot.ts(x=ts_stx[, 1], y=ts_stx[, 4], xlab="", ylab="")
title(main="MSFT Open and Close Prices", line=-1)

library(tseries)  # Load package tseries
# Calculate maximum drawdown
maxdrawdown(zoo_stx_adj[, "AdjClose"])
max_drawd <- maxdrawdown(zoo_stx_adj[, "AdjClose"])
index(zoo_stx_adj)[max_drawd$from]
index(zoo_stx_adj)[max_drawd$to]
# Calculate Sharpe ratio
sharpe(zoo_stx_adj[, "AdjClose"])
# Calculate Sterling ratio
sterling(as.numeric(zoo_stx_adj[, "AdjClose"]))

zoo_stx <- suppressWarnings(  # Load MSFT data
  get.hist.quote(instrument="MSFT",
           start=Sys.Date()-365,
           end=Sys.Date(),
           origin="1970-01-01")
)  # end suppressWarnings
class(zoo_stx)
dim(zoo_stx)
tail(zoo_stx, 4)

# Calculate Sharpe ratio
sharpe(zoo_stx[, "Close"], r=0.01)
# Add title
plot(zoo_stx[, "Close"], xlab="", ylab="")
title(main="MSFT Close Prices", line=-1)

library(tseries)  # Load package tseries
zoo_stx <- suppressWarnings(  # Load MSFT data
  get.hist.quote(instrument="MSFT",
           start=Sys.Date()-365,
           end=Sys.Date(),
           origin="1970-01-01")
)  # end suppressWarnings
class(zoo_stx)
dim(zoo_stx)
tail(zoo_stx, 4)

# Calculate Sharpe ratio
sharpe(zoo_stx[, "Close"], r=0.01)
# Add title
plot(zoo_stx[, "Close"], xlab="", ylab="")
title(main="MSFT Close Prices", line=-1)

library(tseries)  # Load package tseries
zoo_stx <- suppressWarnings(  # Load MSFT data
  get.hist.quote(instrument="MSFT",
           start=Sys.Date()-365,
           end=Sys.Date(),
           origin="1970-01-01")
)  # end suppressWarnings
class(zoo_stx)
dim(zoo_stx)
tail(zoo_stx, 4)

# Calculate Sharpe ratio
sharpe(zoo_stx[, "Close"], r=0.01)
# Add title
plot(zoo_stx[, "Close"], xlab="", ylab="")
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
chartSeries(etf_env$VTI["2008-11/2009-04"],
      name="VTI")
# Redraw plot only for Feb-2009, with white theme
reChart(subset="2009-02",
  theme=chartTheme("white"))

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
VTI_adj <- Ad(oh_lc); VTI_vol <- Vo(oh_lc)
# Calculate volume-weighted average price
v_wap <- TTR::VWAP(price=VTI_adj,
volume=VTI_vol, n=10)
# Plot OHLC candlechart with volume
chartSeries(oh_lc, name="VTI plus VWAP",
      theme=chartTheme("white"))
# Add VWAP to main plot
addTA(ta=v_wap, on=1, col='red')
# Add price minus VWAP in extra panel
addTA(ta=(VTI_adj-v_wap), col='red')

library(quantmod)
library(TTR)
oh_lc <- rutils::etf_env$VTI
VTI_adj <- Ad(oh_lc)
VTI_vol <- Vo(oh_lc)
v_wap <- TTR::VWAP(price=VTI_adj, volume=VTI_vol, n=10)
VTI_adj <- VTI_adj["2009-02/2009-03"]
oh_lc <- oh_lc["2009-02/2009-03"]
v_wap <- v_wap["2009-02/2009-03"]
# Plot OHLC candlechart with volume
chartSeries(oh_lc, name="VTI plus VWAP shaded",
      theme=chartTheme("white"))
# Add VWAP to main plot
addTA(ta=v_wap, on=1, col='red')
# Add price minus VWAP in extra panel
addTA(ta=(VTI_adj-v_wap), col='red')
# Add background shading of areas
addTA((VTI_adj-v_wap) > 0, on=-1,
col="lightgreen", border="lightgreen")
addTA((VTI_adj-v_wap) < 0, on=-1,
col="lightgrey", border="lightgrey")
# Add vertical and horizontal lines at v_wap minimum
addLines(v=which.min(v_wap), col='red')
addLines(h=min(v_wap), col='red')

library(quantmod)
library(TTR)
oh_lc <- rutils::etf_env$VTI
VTI_adj <- Ad(oh_lc)
VTI_vol <- Vo(oh_lc)
v_wap <- TTR::VWAP(price=VTI_adj, volume=VTI_vol, n=10)
VTI_adj <- VTI_adj["2009-02/2009-03"]
oh_lc <- oh_lc["2009-02/2009-03"]
v_wap <- v_wap["2009-02/2009-03"]
# OHLC candlechart VWAP in main plot,
chart_Series(x=oh_lc, # volume in extra panel
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
ch_ob <- chart_Series(x=oh_lc,
                theme=plot_theme, plot=FALSE)
# Extract ylim using accessor function
y_lim <- ch_ob$get_ylim()
y_lim[[2]] <- structure(
  range(Ad(oh_lc)) + c(-1, 1),
  fixed=TRUE)
# modify plot object to reduce y-axis range
ch_ob$set_ylim(y_lim)  # use setter function
# Render the plot
plot(ch_ob)

library(rutils)
# Calculate VTI and XLF volume-weighted average price
v_wap <-
  TTR::VWAP(price=Ad(rutils::etf_env$VTI),
      volume=Vo(rutils::etf_env$VTI), n=10)
XLF_vwap <-
  TTR::VWAP(price=Ad(rutils::etf_env$XLF),
      volume=Vo(rutils::etf_env$XLF), n=10)
# open graphics device, and define
# Plot area with two horizontal panels
x11(); par(mfrow=c(2, 1))
ch_ob <- chart_Series(  # Plot in top panel
  x=etf_env$VTI["2009-02/2009-04"],
  name="VTI", plot=FALSE)
add_TA(v_wap["2009-02/2009-04"],
 lwd=2, on=1, col='blue')
ch_ob <- chart_Series(  # Plot in bottom panel
  x=etf_env$XLF["2009-02/2009-04"],
  name="XLF", plot=FALSE)
add_TA(XLF_vwap["2009-02/2009-04"],
 lwd=2, on=1, col='blue')

library(dygraphs)
# Calculate volume-weighted average price
oh_lc <- rutils::etf_env$VTI
v_wap <- TTR::VWAP(price=quantmod::Ad(oh_lc),
    volume=quantmod::Vo(oh_lc), n=20)
# Add VWAP to OHLC  data
oh_lc <- cbind(oh_lc[, c(1:3, 6)],
         v_wap)["2009-02/2009-04"]
# Create dygraphs object
dy_graph <- dygraphs::dygraph(oh_lc)
# Convert dygraphs object to candlestick plot
dy_graph <- dygraphs::dyCandlestick(dy_graph)
# Render candlestick plot
dy_graph
# Candlestick plot using pipes syntax
dygraphs::dygraph(oh_lc) %>% dyCandlestick()
# Candlestick plot without using pipes syntax
dygraphs::dyCandlestick(dygraphs::dygraph(oh_lc))

# Create candlestick plot with background shading
in_dex <- index(oh_lc)
in_dic <-
  rutils::diff_it(oh_lc[, 4] > oh_lc[, "VWAP"])
in_dic <- rbind(cbind(which(in_dic==1), 1),
  cbind(which(in_dic==(-1)), -1))
in_dic <- in_dic[order(in_dic[, 1]), ]
in_dic <- rbind(c(1, -in_dic[1, 2]), in_dic,
  c(NROW(oh_lc), -in_dic[NROW(in_dic), 2]))
in_dic <-
  data.frame(in_dex[in_dic[, 1]], in_dic[, 2])
# Create dygraphs object
dy_graph <- dygraphs::dygraph(oh_lc) %>%
  dyCandlestick()
# Add shading
for (i in 1:(NROW(in_dic)-1)) {
  if (in_dic[i, 2] == 1)
    dy_graph <- dy_graph %>% dyShading(from=in_dic[i, 1], to=in_dic[i+1, 1], color="lightgreen")
  else
    dy_graph <- dy_graph %>% dyShading(from=in_dic[i, 1], to=in_dic[i+1, 1], color="antiquewhite")
}  # end for
# Render plot
dy_graph

library(dygraphs)
# Prepare VTI and IEF prices
price_s <- cbind(Ad(rutils::etf_env$VTI),
           Ad(rutils::etf_env$IEF))
price_s <- na.omit(price_s)
col_names <- rutils::get_name(colnames(price_s))
colnames(price_s) <- col_names
# dygraphs plot with two y-axes
library(dygraphs)
dygraphs::dygraph(price_s, main=paste(col_names, collapse=" and ")) %>%
  dyAxis(name="y", label="VTI", independentTicks=TRUE) %>%
  dyAxis(name="y2", label="IEF", independentTicks=TRUE) %>%
  dySeries(name="VTI", axis="y", strokeWidth=2, col="red") %>%
  dySeries(name="IEF", axis="y2", strokeWidth=2, col="blue")

# Open plot window and set plot margins
x11(width=6, height=5)
par(mar=c(2, 2, 2, 2), oma=c(1, 1, 1, 1))
# Plot first time series without x-axis
zoo::plot.zoo(price_s[, 1], lwd=2, col="orange",
        xlab=NA, ylab=NA, xaxt="n")
# Create X-axis date labels and add X-axis
in_dex <- pretty(index(price_s))
axis(side=1, at=in_dex, labels=format(in_dex, "%b-%d-%y"))
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
da_ta <- sort(da_ta)
pnorm(1)
sum(da_ta<1)/n_rows
# Monte Carlo estimate of quantile
conf_level <- 0.99
qnorm(conf_level)
cut_off <- conf_level*n_rows
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

x11(width=6, height=5)
par(oma=c(1, 1, 1, 1), mar=c(2, 2, 2, 1), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
set.seed(1121)  # Reset random number generator
bar_rier <- 20  # Barrier level
n_rows <- 1000  # Number of simulation steps
pa_th <- numeric(n_rows)  # Allocate path vector
pa_th[1] <- 0  # Initialize path
in_dex <- 2  # Initialize simulation index
while ((in_dex <= n_rows) &&
 (pa_th[in_dex - 1] < bar_rier)) {
# Simulate next step
  pa_th[in_dex] <-
    pa_th[in_dex - 1] + rnorm(1)
  in_dex <- in_dex + 1  # Advance in_dex
}  # end while
# Fill remaining pa_th after it crosses bar_rier
if (in_dex <= n_rows)
  pa_th[in_dex:n_rows] <- pa_th[in_dex - 1]
# Create daily time series starting 2011
ts_path <- ts(data=pa_th, frequency=365, start=c(2011, 1))
plot(ts_path, type="l", col="black",
     lty="solid", lwd=2, xlab="", ylab="")
abline(h=bar_rier, lwd=2, col="red")
title(main="Brownian motion crossing a barrier level",
      line=0.5)

x11(width=6, height=5)
par(oma=c(1, 1, 1, 1), mar=c(2, 2, 2, 1), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
set.seed(1121)  # Reset random number generator
bar_rier <- 20  # Barrier level
n_rows <- 1000  # Number of simulation steps
# Simulate path of Brownian motion
pa_th <- cumsum(rnorm(n_rows))
# Find index when pa_th crosses bar_rier
cro_ss <- which(pa_th > bar_rier)
# Fill remaining pa_th after it crosses bar_rier
if (NROW(cro_ss)>0) {
  pa_th[(cro_ss[1]+1):n_rows] <-
    pa_th[cro_ss[1]]
}  # end if
# Create daily time series starting 2011
ts_path <- ts(data=pa_th, frequency=365,
     start=c(2011, 1))
# Create plot with horizontal line
plot(ts_path, type="l", col="black",
     lty="solid", lwd=2, xlab="", ylab="")
abline(h=bar_rier, lwd=2, col="red")
title(main="Brownian motion crossing a barrier level",
      line=0.5)

# Define daily volatility and growth rate
sigma_r <- 0.01; dri_ft <- 0.0; len_gth <- 1000
# Simulate geometric Brownian motion
re_turns <- sigma_r*rnorm(len_gth) +
  dri_ft - sigma_r^2/2
price_s <- exp(cumsum(re_turns))
plot(price_s, type="l",
     xlab="periods", ylab="prices",
     main="geometric Brownian motion")

# Simulate geometric Brownian motion
sigma_r <- 0.01/sqrt(48)
dri_ft <- 0.0
len_gth <- 1e4
in_dex <- seq(from=as.POSIXct(paste(Sys.Date()-250, "09:30:00")),
  length.out=len_gth, by="30 min")
price_s <- xts(exp(cumsum(sigma_r*rnorm(len_gth) + dri_ft - sigma_r^2/2)),
  order.by=in_dex)
price_s <- cbind(price_s,
  volume=sample(x=10*(2:18), size=len_gth, replace=TRUE))
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
sigma_r <- 0.01; dri_ft <- 0.0; len_gth <- 5000
path_s <- 10
# Simulate multiple paths of geometric Brownian motion
price_s <- matrix(sigma_r*rnorm(path_s*len_gth) +
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
sigma_r <- 0.01; dri_ft <- 0.0; len_gth <- 10000
path_s <- 100
# Simulate multiple paths of geometric Brownian motion
price_s <- matrix(sigma_r*rnorm(path_s*len_gth) +
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
ls(env_sp500)
# Extract closing prices
price_s <- eapply(env_sp500, quantmod::Cl)
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
par(mar=c(4, 3, 1, 1), oma=c(0, 0, 0, 0))
library(zoo)
re_turns <-
  diff(log(as.numeric(EuStockMarkets[, 1])))
# acf() autocorrelation from package stats
acf(re_turns, lag=10, main="")
title(main="acf of DAX returns", line=-1)

# Ljung-Box test for DAX returns
# 'lag' is the number of autocorrelation coefficients
Box.test(re_turns, lag=10, type="Ljung")
library(Ecdat)  # Load Ecdat
macro_zoo <- as.zoo(Macrodat[, c("lhur", "fygm3")])
colnames(macro_zoo) <- c("unemprate", "3mTbill")
macro_diff <- na.omit(diff(macro_zoo))
# Changes in 3 month T-bill rate are autocorrelated
Box.test(macro_diff[, "3mTbill"],
   lag=10, type="Ljung")
# Changes in unemployment rate are autocorrelated
Box.test(macro_diff[, "unemprate"],
   lag=10, type="Ljung")

library(zoo)  # Load package zoo
dax_acf <- acf(re_turns, plot=FALSE)
summary(dax_acf)  # Get the structure of the "acf" object
# Print(dax_acf)  # Print acf data
dim(dax_acf$acf)
dim(dax_acf$lag)
head(dax_acf$acf)

acf_plus <- function(ts_data, plo_t=TRUE,
                xlab="Lag", ylab="",
                main="", ...) {
  acf_data <- acf(x=ts_data, plot=FALSE, ...)
# Remove first element of acf data
  acf_data$acf <-  array(data=acf_data$acf[-1],
    dim=c((dim(acf_data$acf)[1]-1), 1, 1))
  acf_data$lag <-  array(data=acf_data$lag[-1],
    dim=c((dim(acf_data$lag)[1]-1), 1, 1))
  if (plo_t) {
    ci <- qnorm((1+0.95)/2)*sqrt(1/NROW(ts_data))
    ylim <- c(min(-ci, range(acf_data$acf[-1])),
        max(ci, range(acf_data$acf[-1])))
    plot(acf_data, xlab=xlab, ylab=ylab,
   ylim=ylim, main="", ci=0)
    title(main=main, line=0.5)
    abline(h=c(-ci, ci), col="blue", lty=2)
  }
  invisible(acf_data)  # Return invisibly
}  # end acf_plus

par(mar=c(5,0,1,2), oma=c(1,2,1,0), mgp=c(2,1,0), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
library(zoo)  # Load package zoo
# improved autocorrelation function
acf_plus(re_turns, lag=10, main="")
title(main="acf of DAX returns", line=-1)
# Ljung-Box test for DAX returns
Box.test(re_turns, lag=10, type="Ljung")

par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # Set plot panels
# Autocorrelation of squared DAX returns
acf_plus(re_turns^2, lag=10, main="")
title(main="acf of squared DAX returns",
line=-1)
# Autocorrelation of squared random returns
acf_plus(rnorm(NROW(re_turns))^2,
   lag=10, main="")
title(main="acf of squared random returns",
line=-1)
# Ljung-Box test for squared DAX returns
Box.test(re_turns^2, lag=10, type="Ljung")

library(zoo)  # Load package zoo
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
acf_plus(coredata(macro_diff[, "unemprate"]),
  lag=10, main="quarterly unemployment rate")
acf_plus(coredata(macro_diff[, "3mTbill"]),
  lag=10, main="3 month T-bill EOQ")

# Extract DAX time series
se_ries <- EuStockMarkets[, 1]
# Filter only over past values (sides=1)
co_eff <- c(0.1, 0.39, 0.5)
filter_ed <- filter(se_ries, filter=co_eff,
             method="convolution", sides=1)
# filter() returns a time series of class "ts"
class(filter_ed)
# Filter using compiled C++ function directly
getAnywhere(C_cfilter)
str(stats:::C_cfilter)
filter_fast <- .Call(stats:::C_cfilter, se_ries, filter=co_eff, sides=1, circular=FALSE)
all.equal(as.numeric(filter_ed), filter_fast, check.attributes=FALSE)
# Benchmark speed of the two methods
library(microbenchmark)
summary(microbenchmark(
  filter=filter(se_ries, filter=co_eff, method="convolution", sides=1),
  filter_fast=.Call(stats:::C_cfilter, se_ries, filter=co_eff, sides=1, circular=FALSE)
  ), times=10)[, c(1, 4, 5)]
# Simulate ARIMA using filter()
in_nov <- rnorm(NROW(EuStockMarkets))
filter_ed <- filter(x=in_nov,
  filter=co_eff, method="recursive")

library(zoo)  # Load zoo
library(ggplot2)  # Load ggplot2
library(gridExtra)  # Load gridExtra
# Coerce to zoo and merge the time series
filter_ed <- cbind(as.zoo(se_ries),
            as.zoo(filter_ed))
colnames(filter_ed) <- c("DAX", "DAX filtered")
filter_ed <- window(filter_ed,
             start=1997, end=1998)
autoplot(  # Plot ggplot2
    filter_ed, main="Filtered DAX",
    facets=NULL) +  # end autoplot
xlab("") + ylab("") +
theme(  # modify plot theme
    legend.position=c(0.1, 0.5),
    plot.title=element_text(vjust=-2.0),
    plot.margin=unit(c(-0.5, 0.0, -0.5, 0.0), "cm"),
    plot.background=element_blank(),
    axis.text.y=element_blank()
    )  # end theme
# end ggplot2

par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
re_turns <- na.omit(diff(log(filter_ed)))
par(mfrow=c(2,1))  # Set plot panels

acf_plus(coredata(re_turns[, 1]), lag=10,
   xlab="")
title(main="DAX", line=-1)

acf_plus(coredata(re_turns[, 2]), lag=10,
   xlab="")
title(main="DAX filtered", line=-1)

# Simulate AR processes
set.seed(1121)  # Reset random numbers
in_dex <- Sys.Date() + 0:728  # two year daily series
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

library(zoo)  # Load zoo
library(ggplot2)  # Load ggplot2
set.seed(1121)  # initialize random number generator
rand_walk <- cumsum(zoo(matrix(rnorm(3*100), ncol=3),
            order.by=(Sys.Date()+0:99)))
colnames(rand_walk) <-
  paste("rand_walk", 1:3, sep="_")
plot.zoo(rand_walk, main="Random walks",
     xlab="", ylab="", plot.type="single",
     col=c("black", "red", "blue"))
# Add legend
legend(x="topleft",
 legend=colnames(rand_walk),
 col=c("black", "red", "blue"), lty=1)

# Define AR(3) coefficients and innovations
co_eff <- c(0.1, 0.39, 0.5)
len_gth <- 1e2
set.seed(1121); in_nov <- rnorm(len_gth)
# Simulate AR process using recursive loop in R
ari_ma <- numeric(NROW(in_nov))
ari_ma[1] <- in_nov[1]
ari_ma[2] <- co_eff[1]*ari_ma[1] + in_nov[2]
ari_ma[3] <- co_eff[1]*ari_ma[2] +
  co_eff[2]*ari_ma[1] + in_nov[3]
for (it in 4:NROW(ari_ma)) {
  ari_ma[it] <-
    ari_ma[(it-1):(it-3)] %*% co_eff + in_nov[it]
}  # End for
# Simulate AR process using filter()
filter_ed <- filter(x=in_nov,
  filter=co_eff, method="recursive")
class(filter_ed)
all.equal(ari_ma, as.numeric(filter_ed))

# Calculate modulus of roots of characteristic equation
root_s <- Mod(polyroot(c(1, -co_eff)))
# Calculate warmup period
warm_up <- NROW(co_eff) + ceiling(6/log(min(root_s)))
set.seed(1121)
len_gth <- 1e4
in_nov <- rnorm(len_gth + warm_up)
# Simulate ARIMA using arima.sim()
ari_ma <- arima.sim(n=len_gth,
  model=list(ar=co_eff),
  start.innov=in_nov[1:warm_up],
  innov=in_nov[(warm_up+1):NROW(in_nov)])
# Simulate AR process using filter()
filter_ed <- filter(x=in_nov,
  filter=co_eff, method="recursive")
all.equal(filter_ed[-(1:warm_up)],
  as.numeric(ari_ma))
# Benchmark the speed of the three methods of simulating ARIMA
library(microbenchmark)
summary(microbenchmark(
  filter_ed=filter(x=in_nov, filter=co_eff, method="recursive"),
  arima_sim=arima.sim(n=len_gth,
                  model=list(ar=co_eff),
                  start.innov=in_nov[1:warm_up],
                  innov=in_nov[(warm_up+1):NROW(in_nov)]),
  arima_loop={for (it in 4:NROW(ari_ma)) {
  ari_ma[it] <- ari_ma[(it-1):(it-3)] %*% co_eff + in_nov[it]}}
  ), times=10)[, c(1, 4, 5)]

# Simulate random walks using apply() loops
set.seed(1121)  # initialize random number generator
rand_walks <- matrix(rnorm(1000*100), ncol=1000)
rand_walks <- apply(rand_walks, 2, cumsum)
vari_ance <- apply(rand_walks, 1, var)
# Simulate random walks using vectorized functions
set.seed(1121)  # initialize random number generator
rand_walks <- matrixStats::colCumsums(matrix(rnorm(1000*100), ncol=1000))
vari_ance <- matrixStats::rowVars(rand_walks)
par(mar=c(5, 3, 2, 2), oma=c(0, 0, 0, 0))
plot(vari_ance, xlab="time steps", ylab="",
     t="l", col="blue", lwd=2,
     main="Variance of Random Walk")

len_gth <- 1e4
# Simulate arima with small AR coefficient
set.seed(1121)
ari_ma <- arima.sim(n=len_gth, model=list(ar=0.01))
tseries::adf.test(ari_ma)
# Simulate arima with large AR coefficient
set.seed(1121)
ari_ma <- arima.sim(n=len_gth, model=list(ar=0.99))
tseries::adf.test(ari_ma)
# Simulate arima with different AR coefficients
coeff_s <- seq(0.99, 1.0, 0.001) - 0.001
set.seed(1121)
in_nov <- rnorm(len_gth)
adf_test <- sapply(coeff_s, function(co_eff) {
  ari_ma <- filter(x=in_nov, filter=co_eff, method="recursive")
  ad_f <- suppressWarnings(tseries::adf.test(ari_ma))
  c(adf_stat=unname(ad_f$statistic), pval=ad_f$p.value)
})  # end sapply
plot(x=coeff_s, y=adf_test["pval", ], main="ADF Pval versus AR coefficient",
     xlab="AR coefficient", ylab="ADF pval", t="l", col="blue", lwd=2)
plot(x=coeff_s, y=adf_test["adf_stat", ], main="ADF Stat versus AR coefficient",
     xlab="AR coefficient", ylab="ADF stat", t="l", col="blue", lwd=2)

# Simulate arima with large AR coefficient
set.seed(1121)
ari_ma <- arima.sim(n=len_gth, model=list(ar=0.99))
tseries::adf.test(ari_ma)
# integrated series has unit root
tseries::adf.test(cumsum(ari_ma))
# Simulate arima with negative AR coefficient
set.seed(1121)
ari_ma <- arima.sim(n=len_gth, model=list(ar=-0.99))
tseries::adf.test(ari_ma)
# integrated series has unit root
tseries::adf.test(cumsum(ari_ma))

x11(width=5, height=3.5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0))
# Simulate AR(1) process
ari_ma <- arima.sim(n=729, model=list(ar=0.8))
# ACF of AR(1) process
ac_f <- acf_plus(ari_ma, lag=10,
  xlab="", ylab="",
  main="Autocorrelations of AR(1) process")
ac_f$acf[1:5]

# PACF of AR(1) process
pac_f <- pacf(ari_ma, lag=10,
  xlab="", ylab="", main="")
title("Partial autocorrelations of AR(1) process",
  line=1)
pac_f <- drop(pac_f$acf)
pac_f[1:5]

# Compute pacf recursively from acf
ac_f <- acf_plus(ari_ma, lag=10, plo_t=FALSE)
ac_f <- drop(ac_f$acf)
pac_f <- numeric(3)
pac_f[1] <- ac_f[1]
pac_f[2] <- ac_f[2] - ac_f[1]^2
pac_f[3] <- ac_f[3] -
  pac_f[2]*ac_f[1] - ac_f[2]*pac_f[1]
# Compute pacf recursively in a loop
pac_f <- numeric(NROW(ac_f))
pac_f[1] <- ac_f[1]
for (it in 2:NROW(pac_f)) {
  pac_f[it] <- ac_f[it] -
    pac_f[1:(it-1)] %*% ac_f[(it-1):1]
}  # end for

par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # Set plot panels
# Simulate AR process of returns
ari_ma <- arima.sim(n=729,
  model=list(ar=c(0.1, 0.5, 0.1)))
# ACF of AR(3) process
acf_plus(ari_ma, lag=10, xlab="", ylab="",
   main="ACF of AR(3) process")
# PACF of AR(3) process
pacf(ari_ma, lag=10, xlab="", ylab="",
     main="PACF of AR(3) process")

# Simulate AR process using filter()
len_gth <- 1e3
co_eff <- c(0.1, 0.39, 0.5)
set.seed(1121); in_nov <- rnorm(len_gth)
ari_ma <- as.numeric(filter(x=in_nov,
  filter=co_eff, method="recursive"))
# Calibrate AR model using ar.ols()
ar_fit <- ar.ols(ari_ma)
class(ar_fit)
is.list(ar_fit)
drop(ar_fit$ar)

# wippp
# Calibrate AR model using regression
# Define design matrix with intercept column
de_sign <- sapply(1:3, function(lagg) {
  rutils::lag_it(ari_ma, lagg=lagg)
})  # end sapply
de_sign <- cbind(rep(1, len_gth), de_sign)
# Regression coefficients with response equal to ari_ma
design_inv <- MASS::ginv(de_sign)
coeff_reg <- drop(design_inv %*% ari_ma)
all.equal(drop(ar_fit$ar), coeff_reg[-1], check.attributes=FALSE)

# Simulate AR process using filter()
len_gth <- 1e3
co_eff <- c(0.1, 0.39, 0.5)
set.seed(1121); in_nov <- rnorm(len_gth)
ari_ma <- as.numeric(filter(x=in_nov,
  filter=co_eff, method="recursive"))
# Calibrate ARIMA model using arima()
arima_fit <- arima(ari_ma,
  order=c(3,0,0), include.mean=FALSE)

# wippp
# Calibrate ARIMA model using regression
# Define design matrix
ari_ma <- (ari_ma - mean(ari_ma))
de_sign <- sapply(1:3, function(lagg) {
  rutils::lag_it(ari_ma, lagg=lagg)
})  # end sapply
# Calculate de-meaned re_turns matrix
de_sign <- t(t(de_sign) - colMeans(de_sign))
design_inv <- MASS::ginv(de_sign)
# Regression coefficients with response equal to ari_ma
coeff_reg <- drop(design_inv %*% ari_ma)

all.equal(arima_fit$coef, coeff_reg, check.attributes=FALSE)

# Calculate fitted values from regression coefficients
fit_ted <- drop(de_sign %*% coeff_reg)
all.equal(fit_ted, mod_el$fitted.values, check.attributes=FALSE)
# Calculate the residuals
resid_uals <- drop(ari_ma - fit_ted)
all.equal(resid_uals, mod_el$residuals, check.attributes=FALSE)
# The residuals have zero mean
all.equal(sum(resid_uals), target=0)
# The residuals are orthogonal to the de_sign columns (predictors)
sapply(resid_uals %*% de_sign,
 all.equal, target=0)
# The residuals are orthogonal to the fitted values
all.equal(sum(resid_uals*fit_ted), target=0)

# Variance of residuals
var_resid <- sum(resid_uals^2)/(len_gth - NROW(coeff_reg))

# Design matrix squared
design_2 <- crossprod(de_sign)
# design_2 <- t(de_sign) %*% de_sign
# Calculate covariance matrix of betas
co_var <- var_resid*MASS::ginv(design_2)
# Round(co_var, 3)
coeff_regd <- sqrt(diag(co_var))
all.equal(coeff_regd, arima_fit$var.coef, check.attributes=FALSE)

# Calculate t-values of betas
beta_tvals <- drop(coeff_reg)/coeff_regd




# Simulate AR process using filter()
len_gth <- 1e3
co_eff <- c(0.1, 0.39, 0.5)
set.seed(1121); in_nov <- rnorm(len_gth)
ari_ma <- as.numeric(filter(x=in_nov,
  filter=co_eff, method="recursive"))
# wippp
# Calibrate ARIMA model using regression
# Define design matrix
de_sign <- sapply(1:3, function(lagg) {
  rutils::lag_it(ari_ma, lagg=lagg)
})  # end sapply
design_inv <- MASS::ginv(de_sign)
# Regression coefficients with response equal to ari_ma
coeff_reg <- drop(design_inv %*% ari_ma)
# Calibrate ARIMA model using arima()
arima_fit <- arima(ari_ma,
  order=c(3,0,0), include.mean=FALSE)
arima_fit$coef
all.equal(arima_fit$coef, coeff_reg, check.attributes=FALSE)

# Calibrate ARIMA model using auto.arima()
# library(forecast)  # Load forecast
forecast::auto.arima(ari_ma, max.p=3, max.q=0, max.d=0)

# Calibrate ARIMA model using arima()
arima_fit <- arima(ari_ma,
  order=c(3,0,0), include.mean=FALSE)
arima_fit$coef
# Calibrate ARIMA model using auto.arima()
# library(forecast)  # Load forecast
forecast::auto.arima(ari_ma, max.p=3, max.q=0, max.d=0)
# Calibrate ARIMA model using regression
ari_ma <- as.numeric(ari_ma)
# Define design matrix
de_sign <- sapply(1:3, function(lagg) {
  rutils::lag_it(ari_ma, lagg=lagg)
})  # end sapply
# Generalized inverse of design matrix
design_inv <- MASS::ginv(de_sign)
# Regression coefficients with response equal to ari_ma
co_eff <- drop(design_inv %*% ari_ma)
all.equal(arima_fit$coef, co_eff, check.attributes=FALSE)

# Compute autocorrelation coefficients
ac_f <- acf_plus(ari_ma, lag=10, plo_t=FALSE)
ac_f <- drop(ac_f$acf)
# Define Yule-Walker matrix
acf_1 <- c(1, ac_f[-10])
yule_walker <- sapply(1:9, function(lagg) {
  col_umn <- rutils::lag_it(acf_1, lagg=lagg)
  col_umn[1:lagg] <- acf_1[(lagg+1):2]
  col_umn
})  # end sapply
yule_walker <- cbind(acf_1, yule_walker)
# Generalized inverse of Yule-Walker matrix
yule_walker_inv <- MASS::ginv(yule_walker)
# Solve Yule-Walker equations
co_eff <- drop(yule_walker_inv %*% ac_f)

# Simulate AR process using filter()
len_gth <- 1e2
co_eff <- c(0.1, 0.39, 0.5)
set.seed(1121); in_nov <- rnorm(len_gth)
ari_ma <- filter(x=in_nov,
  filter=co_eff, method="recursive")
ari_ma <- as.numeric(ari_ma)

# Forecast AR(3) process using loop in R
forecast_s <- numeric(NROW(ari_ma))
forecast_s[1] <- 0
forecast_s[2] <- co_eff[1]*ari_ma[1]
forecast_s[3] <- co_eff[1]*ari_ma[2] +
  co_eff[2]*ari_ma[1]
for (it in 4:NROW(forecast_s)) {
  forecast_s[it] <- ari_ma[(it-1):(it-3)] %*% co_eff
}  # end for

# Plot with legend
plot(ari_ma,
  main="Forecasting Using AR(3) Model",
  xlab="", ylab="", type="l")
lines(forecast_s, col="orange", lwd=3)
legend(x="topright", legend=c("series","forecasts"),
 col=c("black", "orange"), lty=1, lwd=6,
 cex=0.9, bg="white", bty="n")

# Forecast using filter()
forecasts_filter <- filter(x=ari_ma, sides=1,
  filter=co_eff, method="convolution")
forecasts_filter <- as.numeric(forecasts_filter)
# Compare with loop in R
all.equal(forecast_s[-(1:3)],
  forecasts_filter[-c(1:2, NROW(forecasts_filter))],
  check.attributes=FALSE)

# Calibrate ARIMA model using arima()
arima_fit <- arima(ari_ma, order=c(3,0,0),
             include.mean=FALSE)
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
fore_cast <- drop(
  ari_ma[len_gth:(len_gth-2)] %*% arima_fit$coef)
# Compare one-step-ahead forecasts
all.equal(pre_dict$pred[[1]], fore_cast)
# Get information about predict.Arima()
?stats:::predict.Arima

# Compare residuals with innovations
residual_s <- (ari_ma - forecast_s)
all.equal(in_nov, residual_s,
    check.attributes=FALSE)
plot(residual_s, t="l", lwd=3, main="ARIMA Forecast Errors")
# Calibrate ARIMA model using arima()
arima_fit <- arima(ari_ma, order=c(3,0,0),
             include.mean=FALSE)
coef_fit <- arima_fit$coef
# Forecast using from fitted coefficients
forecast_s <- numeric(NROW(ari_ma))
forecast_s[1] <- 0
forecast_s[2] <- coef_fit[1]*ari_ma[1]
forecast_s[3] <- coef_fit[1]*ari_ma[2] +
  coef_fit[2]*ari_ma[1]
for (it in 4:NROW(forecast_s)) {
  forecast_s[it] <-
    ari_ma[(it-1):(it-3)] %*% coef_fit
}  # end for
# Calculate the forecasting residuals
residual_s <- (ari_ma - forecast_s)
all.equal(in_nov, residual_s,
    check.attributes=FALSE)
tail(cbind(in_nov, residual_s))

# Simulate AR process using filter()
len_gth <- 1e2
co_eff <- c(0.1, 0.39, 0.5)
set.seed(1121)
ari_ma <- filter(x=rnorm(len_gth),
  filter=co_eff, method="recursive")
ari_ma <- as.numeric(ari_ma)
# Forecast AR(3) process
forecast_s <- numeric(NROW(ari_ma))
forecast_s[2] <- co_eff[1]*ari_ma[1]
forecast_s[3] <- co_eff[1]*ari_ma[2] +
  co_eff[2]*ari_ma[1]
for (it in 4:NROW(forecast_s)) {
  forecast_s[it] <-
    ari_ma[(it-1):(it-3)] %*% co_eff
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
# len_gth <- NROW(ari_ma)
# Compare one-step-ahead forecasts
# arima_fit <- arima(ari_ma, order=c(3,0,0), method="ML", include.mean=FALSE)

# Compare many one-step-ahead forecasts
forecast_s <- sapply(31:len_gth, function(x) {
  cat("len = ", x, "\n")
  # ari_ma <- filter(x=rnorm(len_gth+1), filter=co_eff, method="recursive")
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
forecast_s[3] <- co_eff[1]*ari_ma[2] +
  co_eff[2]*ari_ma[1]
for (it in 4:NROW(forecast_s)) {
  forecast_s[it] <-
    ari_ma[(it-1):(it-3)] %*% co_eff
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
plot(ari_ma,
  main="Forecasting Using AR(3) Model",
  xlab="", ylab="", type="l")
lines(forecast_s, col="orange", lwd=3)
legend(x="topright", legend=c("series","forecasts"),
 col=c("black", "orange"), lty=1, lwd=6,
 cex=0.9, bg="white", bty="n")

# Simulate AR process using filter()
co_eff <- c(0.1, 0.39, 0.5); n_coeff <- NROW(co_eff)
len_gth <- 1e3; set.seed(1121)
ari_ma <- as.numeric(filter(x=rnorm(len_gth),
  filter=co_eff, method="recursive"))
# Define design matrix
de_sign <- sapply(1:n_coeff, function(lagg) {
  rutils::lag_it(ari_ma, lagg=lagg)
})  # end sapply
de_sign <- cbind(rep(1, len_gth), de_sign)
de_sign <- cbind(ari_ma, de_sign)
# Perform rolling forecasting
look_back <- 100 # Length of look-back interval
forecast_s <- sapply(n_coeff:len_gth, function(now) {
  # Subset the design matrix
  star_t <- max(1, now-look_back+1)
  de_sign <- de_sign[star_t:now, ]
  # Calculate AR(3) coefficients
  design_inv <- MASS::ginv(de_sign[, -1])
  co_eff <- drop(design_inv %*% de_sign[, 1])
  # Calculate forecast
  de_sign[NROW(de_sign):(NROW(de_sign)-n_coeff+1), 1] %*% co_eff
})  # end sapply
forecast_s <- c(rep(forecast_s[1], n_coeff-1), forecast_s)
# Lag the forecasts to push them out-of-sample
forecast_s <- rutils::lag_it(forecast_s)

# Mean squared error
mean((ari_ma - forecast_s)^2)
# Correlation
sum(forecast_s*ari_ma)
# Plot forecasting series with legend
plot(ari_ma, xlab="", ylab="", type="l",
  main="Rolling Forecasting Using AR(3) Model")
lines(forecast_s, col="orange", lwd=1)
legend(x="topright", legend=c("series","forecasts"),
 col=c("black", "orange"), lty=1, lwd=6,
 cex=0.9, bg="white", bty="n")

# Define backtesting function
back_test <- function(ari_ma, n_coeff=2, look_back=11) {
  # Define design matrix
  de_sign <- sapply(1:n_coeff, function(lagg) {
    rutils::lag_it(ari_ma, lagg=lagg)
  })  # end sapply
  de_sign <- cbind(rep(1, len_gth), de_sign)
  de_sign <- cbind(ari_ma, de_sign)
  # Perform rolling forecasting
  forecast_s <- sapply(n_coeff:NROW(ari_ma),
    function(now) {
# Subset the design matrix
star_t <- max(1, now-look_back+1)
de_sign <- de_sign[star_t:now, ]
n_rows <- NROW(de_sign)
# Calculate AR coefficients
design_inv <- MASS::ginv(de_sign[, -1])
co_eff <- drop(design_inv %*% de_sign[, 1])
# Calculate forecast
de_sign[n_rows:(n_rows-n_coeff+1), 1] %*% co_eff
    })  # end sapply
  forecast_s <- c(rep(forecast_s[1], n_coeff-1), forecast_s)
  # Lag the forecasts to push them out-of-sample
  forecast_s <- rutils::lag_it(forecast_s)
  sum(forecast_s*ari_ma)
}  # end back_test

# Simulate AR process using filter()
len_gth <- 1e3
co_eff <- c(0.1, 0.39, 0.5)
n_coeff <- NROW(co_eff)
set.seed(1121)
ari_ma <- as.numeric(filter(x=rnorm(len_gth),
  filter=co_eff, method="recursive"))

foo <- sapply(2*(1:11), back_test, ari_ma=ari_ma, look_back=look_back)


# Plot forecasting series with legend
plot(foo, xlab="", ylab="", type="l",
  main="Rolling Forecasting Using AR(3) Model")
lines(forecast_s, col="orange", lwd=1)
legend(x="topright", legend=c("series","forecasts"),
 col=c("black", "orange"), lty=1, lwd=6,
 cex=0.9, bg="white", bty="n")

# Define Ornstein-Uhlenbeck parameters
eq_price <- 1.0; sigma_r <- 0.02
the_ta <- 0.01; len_gth <- 1000
drif_t <- the_ta*eq_price
theta_1 <- 1-the_ta
# Simulate Ornstein-Uhlenbeck process
in_nov <- sigma_r*rnorm(len_gth)
price_s <- numeric(len_gth)
price_s[1] <- in_nov[1]
for (i in 2:len_gth) {
  price_s[i] <- theta_1*price_s[i-1] +
    in_nov[i] + drif_t
}  # end for

plot(price_s, type="l",
     xlab="periods", ylab="prices",
     main="Ornstein-Uhlenbeck process")
legend("topright",
 title=paste(c(paste0("sigma_r = ", sigma_r),
               paste0("eq_price = ", eq_price),
               paste0("the_ta = ", the_ta)),
             collapse="\n"),
 legend="", cex=0.8,
 inset=0.1, bg="white", bty="n")
abline(h=eq_price, col='red', lwd=2)

re_turns <- rutils::diff_it(price_s)
lag_price <- rutils::lag_it(price_s)
for_mula <- re_turns ~ lag_price
l_m <- lm(for_mula)
summary(l_m)
# Plot regression
plot(for_mula, main="OU Returns Versus Lagged Prices")
abline(l_m, lwd=2, col="red")

# volatility parameter
c(sigma_r, sd(re_turns))
# Extract OU parameters from regression
co_eff <- summary(l_m)$coefficients
# theta strength of mean reversion
round(co_eff[2, ], 3)
# Equilibrium price
co_eff[1, 1]/co_eff[2, 1]
# Parameter and t-values
co_eff <- cbind(c(the_ta*eq_price, the_ta),
  co_eff[, 1:2])
rownames(co_eff) <- c("drift", "theta")
round(co_eff, 3)

# Simulate Schwartz process
re_turns <- numeric(len_gth)
price_s <- numeric(len_gth)
price_s[1] <- eq_price
set.seed(1121)  # Reset random numbers
for (i in 2:len_gth) {
  re_turns[i] <- the_ta*(eq_price - price_s[i-1]) +
    sigma_r*rnorm(1)
  price_s[i] <- price_s[i-1] * exp(re_turns[i])
}  # end for

plot(price_s, type="l",
     xlab="periods", ylab="prices",
     main="Log-normal Ornstein-Uhlenbeck process")
legend("topright",
 title=paste(c(paste0("sigma_r = ", sigma_r),
               paste0("eq_price = ", eq_price),
               paste0("the_ta = ", the_ta)),
             collapse="\n"),
 legend="", cex=0.8,
 inset=0.12, bg="white", bty="n")
abline(h=eq_price, col='red', lwd=2)

# Define Ornstein-Uhlenbeck parameters
eq_price <- 5.0; sigma_r <- 0.01
the_ta <- 0.01; len_gth <- 1000
# Simulate Ornstein-Uhlenbeck process
re_turns <- numeric(len_gth)
price_s <- numeric(len_gth)
price_s[1] <- 5.0
set.seed(1121)  # Reset random numbers
for (i in 2:len_gth) {
  re_turns[i] <- the_ta*(eq_price - price_s[i-1]) +
    sigma_r*rnorm(1)
  price_s[i] <- price_s[i-1] * exp(re_turns[i])
}  # end for

plot(price_s, type="l",
     xlab="periods", ylab="prices",
     main="Ornstein-Uhlenbeck process")
legend("topright",
 title=paste(c(paste0("sigma_r = ", sigma_r),
               paste0("eq_price = ", eq_price),
               paste0("the_ta = ", the_ta)),
             collapse="\n"),
 legend="", cex=0.8,
 inset=0.1, bg="white", bty="n")
