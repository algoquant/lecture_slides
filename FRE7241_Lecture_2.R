library(HighFreq)  # Load package HighFreq
# Calculate ETF returns
re_turns <- na.omit(
  rutils::etf_env$re_turns[, c("VTI", "IEF")])
re_turns <- cbind(re_turns,
  0.6*re_turns[, "IEF"]+0.4*re_turns[, "VTI"])
colnames(re_turns)[3] <- "combined"
# Calculate prices from returns
price_s <- lapply(re_turns,
  function(x) cumprod(1 + x))
price_s <- do.call(cbind, price_s)
# Plot prices
dygraphs::dygraph(price_s, main="Stock and Bond Portfolio") %>%
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

re_turns <- na.omit(rutils::etf_env$re_turns[, c("VTI", "IEF")])
# Logarithmic utility of stock and bond portfolio
utili_ty <- function(w_s, w_b) {
  -sum(log(1 + w_s*re_turns[, "VTI"] + w_b*re_turns[, "IEF"]))
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

# Kelly optimal leverage for stocks
unlist(optimize(f=function(x) utili_ty(x, w_b=0), interval=c(1, 4)))
# Approximate Kelly leverage
mean(re_turns[, "VTI"])/var(re_turns[, "VTI"])
# Kelly optimal leverage for bonds
unlist(optimize(f=function(x) utili_ty(x, w_s=0), interval=c(1, 14)))
# Approximate Kelly leverage
mean(re_turns[, "IEF"])/var(re_turns[, "IEF"])
# Vectorized utility of stock and bond portfolio
utility_vec <- function(weight_s) {
  utili_ty(weight_s[1], weight_s[2])
}  # end utility_vec
# Optimize with respect to vector argument
op_tim <- optim(fn=utility_vec, par=c(3, 10),
          method="L-BFGS-B",
          upper=c(8, 20),
          lower=c(2, 5))
# Kelly sub-optimal weights
weight_s <- op_tim$par/4

# Plot Kelly optimal portfolio
re_turns <- cbind(re_turns,
  weight_s[1]*re_turns[, "VTI"] +
    weight_s[2]*re_turns[, "IEF"])
colnames(re_turns)[3] <- "Kelly_sub_optimal"
# Calculate prices from returns
price_s <- lapply(re_turns,
  function(x) cumprod(1 + x))
price_s <- do.call(cbind, price_s)
# Plot prices
dygraphs::dygraph(price_s, main="Stock and Bond Portfolio") %>%
  dyOptions(colors=c("green","blue","green")) %>%
  dySeries("Kelly_sub_optimal", color="red", strokeWidth=2) %>%
  dyLegend(show="always")

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

library(HighFreq)
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

oh_lc <- rutils::etf_env$VTI
# Number of data points
n_rows <- NROW(oh_lc["2018-06/"])
# Define end_points at each point in time
end_points <- 1:n_rows
# Number of data points in look_back interval
look_back <- 22
# start_points are end_points lagged by look_back
start_points <- c(rep_len(1, look_back-1),
    end_points[1:(n_rows-look_back+1)])
# Or
start_points <- (end_points-look_back+1)
start_points <- ifelse(start_points > 0,
  start_points, 1)

# Number of data points
price_s <- quantmod::Cl(oh_lc["2018/"])
n_rows <- NROW(price_s)
# Number of periods between endpoints
n_points <- 22
# Number of n_points that fit over n_rows
n_agg <- n_rows %/% n_points
# if n_rows==n_points*n_agg then whole number
end_points <- (1:n_agg)*n_points
# else stub interval at beginning
end_points <-
  n_rows-n_points*n_agg + (0:n_agg)*n_points
# else stub interval at end
end_points <- c((1:n_agg)*n_points, n_rows)
# Or use xts::endpoints()
end_points <- xts::endpoints(price_s, on="months")

# Plot data and endpoints as vertical lines
plot.xts(price_s, col="blue", lwd=2, xlab="", ylab="",
   main="Prices with Endpoints as Vertical Lines")
addEventLines(xts(rep("endpoint", NROW(end_points)), index(price_s)[end_points]),
        col="red", lwd=2, pos=4)
# Or
plot_theme <- chart_theme()
plot_theme$col$line.col <- "blue"
chart_Series(price_s, theme=plot_theme,
  name="prices with endpoints as vertical lines")
abline(v=end_points, col="red", lwd=2)

# Number of data points
n_rows <- NROW(rutils::etf_env$VTI["2017/"])
# Number of n_points that fit over n_rows
n_points <- 22
n_agg <- n_rows %/% n_points
# end_points with stub interval at beginning
end_points <-
  n_rows-n_points*n_agg + (0:n_agg)*n_points

# look_back defined as number of data points
look_back <- 252
# start_points are end_points lagged by look_back
start_points <- (end_points-look_back+1)
start_points <- ifelse(start_points > 0,
  start_points, 1)
cbind(start_points, end_points)
# look_back defined as number of end_points
look_back <- 12
start_points <- c(rep_len(1, look_back-1),
    end_points[1:(NROW(end_points)-look_back+1)])

# Number of data points
n_rows <- NROW(rutils::etf_env$VTI["2017/"])
# Number of data points per interval
look_back <- 22
# Number of look_backs that fit over n_rows
n_agg <- n_rows %/% look_back
# Define end_points with beginning stub
end_points <-
  n_rows-look_back*n_agg + (0:n_agg)*look_back
# Define contiguous start_points
start_points <- c(1, end_points[1:(NROW(end_points)-1)])
# Define exclusive start_points
start_points <- c(1, end_points[1:(NROW(end_points)-1)]+1)

price_s <- quantmod::Cl(rutils::etf_env$VTI)
end_points <- seq_along(price_s)  # Define end points
n_rows <- NROW(end_points)
look_back <- 22  # Number of data points per look-back interval
# start_points are multi-period lag of end_points
start_points <- c(rep_len(1, look_back-1),
    end_points[1:(n_rows-look_back+1)])
# Define list of look-back intervals for aggregations over past
look_backs <- lapply(seq_along(end_points),
  function(in_dex) {
    start_points[in_dex]:end_points[in_dex]
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
agg_regations <- xts(agg_regations,
               order.by=index(price_s[end_points]))

library(HighFreq)  # load package HighFreq
# Perform aggregations over look_backs list
agg_regations <- lapply(look_backs,
    function(look_back) agg_regate(price_s[look_back])
)  # end lapply
# rbind list into single xts or matrix
agg_regations <- rutils::do_call_rbind(agg_regations)
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

# library(HighFreq)  # load package HighFreq
# Define functional for rolling aggregations
roll_agg <- function(x_ts, look_back, FUN, ...) {
# Define end points at every period
  end_points <- seq_along(x_ts)
  n_rows <- NROW(end_points)
# Define starting points as lag of end_points
  start_points <- c(rep_len(1, look_back-1),
    end_points[1:(n_rows-look_back+1)])
# Define list of look-back intervals for aggregations over past
look_backs <- lapply(seq_along(end_points),
  function(in_dex) {
    start_points[in_dex]:end_points[in_dex]
})  # end lapply
# Perform aggregations over look_backs list
  agg_regations <- lapply(look_backs,
    function(look_back) FUN(x_ts[look_back], ...)
  )  # end lapply
# rbind list into single xts or matrix
  agg_regations <- rutils::do_call_rbind(agg_regations)
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

# library(HighFreq)  # load package HighFreq
# Define aggregation function that returns a vector
agg_vector <- function(x_ts)
  c(max=max(x_ts), min=min(x_ts))
# Define aggregation function that returns an xts
agg_xts <- function(x_ts)
  xts(t(c(max=max(x_ts), min=min(x_ts))),
order.by=end(x_ts))
# Benchmark the speed of aggregation functions
library(microbenchmark)
summary(microbenchmark(
  agg_vector=roll_agg(price_s, look_back=look_back,
              FUN=agg_vector),
  agg_xts=roll_agg(price_s, look_back=look_back,
              FUN=agg_xts),
  times=10))[, c(1, 4, 5)]

# library(HighFreq)  # load package HighFreq
# Define aggregation function that returns a single value
agg_regate <- function(x_ts)  max(x_ts)
# Perform aggregations over a rolling interval
agg_regations <- xts:::rollapply.xts(price_s, width=look_back,
              FUN=agg_regate, align="right")
# Perform aggregations over a rolling interval
library(PerformanceAnalytics)  # load package PerformanceAnalytics
agg_regations <- apply.rolling(price_s,
              width=look_back, FUN=agg_regate)
# Benchmark the speed of the functionals
library(microbenchmark)
summary(microbenchmark(
  roll_agg=roll_agg(price_s, look_back=look_back,
              FUN=max),
  roll_xts=xts:::rollapply.xts(price_s, width=look_back,
                 FUN=max, align="right"),
  apply_rolling=apply.rolling(price_s,
                        width=look_back, FUN=max),
  times=10))[, c(1, 4, 5)]

# library(HighFreq)  # load package HighFreq
# rolling sum using cumsum()
roll_sum <- function(x_ts, look_back) {
  cum_sum <- cumsum(na.omit(x_ts))
  out_put <- cum_sum - lag(x=cum_sum, k=look_back)
  out_put[1:look_back, ] <- cum_sum[1:look_back, ]
  colnames(out_put) <- paste0(colnames(x_ts), "_stdev")
  out_put
}  # end roll_sum
agg_regations <- roll_sum(price_s, look_back=look_back)
# Define list of look-back intervals for aggregations over past
look_backs <- lapply(seq_along(end_points),
  function(in_dex) {
    start_points[in_dex]:end_points[in_dex]
})  # end lapply
# Perform rolling aggregations using apply loop
agg_regations <- sapply(look_backs,
    function(look_back) sum(price_s[look_back])
)  # end sapply
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
  # loop over periods
  for (it in 1:n_rows) {
    sub_vec <- vec_tor[max(1, it-look_back+1):it]
    max_min[it, 1] <- max(sub_vec)
    max_min[it, 2] <- min(sub_vec)
  }  # end for
  return(max_min)
}  # end roll_maxminr
max_minr <- roll_maxminr(price_s, look_back)
max_minr <- xts::xts(max_minr, index(price_s))
library(TTR)  # load package TTR
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
library(roll)  # load roll
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

library(RcppRoll)  # load package RcppRoll
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

# library(HighFreq)  # load package HighFreq
library(caTools)  # load package "caTools"
# get documentation for package "caTools"
packageDescription("caTools")  # get short description
help(package="caTools")  # load help page
data(package="caTools")  # list all datasets in "caTools"
ls("package:caTools")  # list all objects in "caTools"
detach("package:caTools")  # remove caTools from search path
# median filter
look_back <- 2
price_s <- quantmod::Cl(HighFreq::SPY["2012-02-01/2012-04-01"])
med_ian <- runmed(x=price_s, k=look_back)
# Vector of rolling volatility
sigma_r <- runsd(x=price_s, k=look_back,
          endrule="constant", align="center")
# Vector of rolling quantiles
quan_tiles <- runquantile(x=price_s,
            k=look_back, probs=0.9,
            endrule="constant",
            align="center")

# Compile Rcpp functions
Rcpp::sourceCpp(file="C:/Develop/R/Rcpp/roll_maxmin.cpp")
max_minarma <- roll_maxmin(price_s, look_back)
max_minarma <- xts::xts(max_minr, index(price_s))
max_min <- cbind(TTR::runMax(x=price_s, n=look_back),
           TTR::runMin(x=price_s, n=look_back))
all.equal(max_min[-(1:look_back), ], max_minarma[-(1:look_back), ], check.attributes=FALSE)
# Benchmark the speed of TTR::runMax
library(microbenchmark)
summary(microbenchmark(
  arma=roll_maxmin(price_s, look_back),
  ttr=TTR::runMax(price_s, n=look_back),
  times=10))[, c(1, 4, 5)]
# Dygraphs plot with max_min lines
da_ta <- cbind(price_s, max_minarma)
colnames(da_ta)[2:3] <- c("max", "min")
col_ors <- c("blue", "red", "green")
dygraphs::dygraph(da_ta, main=paste(colnames(price_s), "max and min lines")) %>%
  dyOptions(colors=col_ors)
# standard plot with max_min lines
plot_theme <- chart_theme()
plot_theme$col$line.col <- col_ors
quantmod::chart_Series(da_ta["2008/2009"], theme=plot_theme,
  name=paste(colnames(price_s), "max and min lines"))
legend(x="topright", title=NULL, legend=colnames(da_ta),
 inset=0.1, cex=0.9, bg="white", bty="n",
 lwd=6, lty=1, col=col_ors)

library(HighFreq)  # load package HighFreq
# indices of last observations in each hour
end_points <- xts::endpoints(price_s, on="hours")
head(end_points)
# extract the last observations in each hour
head(price_s[end_points, ])

price_s <- quantmod::Cl(rutils::etf_env$VTI)
# Number of data points
n_rows <- NROW(price_s)
# Number of data points per interval
look_back <- 22
# Number of look_backs that fit over n_rows
n_agg <- n_rows %/% look_back
# Define end_points with beginning stub
end_points <-
  n_rows-look_back*n_agg + (0:n_agg)*look_back
# Define contiguous start_points
start_points <- c(1, end_points[1:(NROW(end_points)-1)])
# Define list of look-back intervals for aggregations over past
look_backs <- lapply(seq_along(end_points),
  function(in_dex) {
    start_points[in_dex]:end_points[in_dex]
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
    order.by=index(price_s[end_points]))
head(agg_regations)
# Plot aggregations with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("red", "green")
chart_Series(agg_regations, theme=plot_theme,
       name="price aggregations")
legend("top", legend=colnames(agg_regations),
  bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")

# library(HighFreq)  # load package HighFreq
# Perform lapply() loop over look_backs list
agg_regations <- lapply(look_backs,
    function(look_back) {
x_ts <- price_s[look_back]
c(max=max(x_ts), min=min(x_ts))
    })  # end lapply
# rbind list into single xts or matrix
agg_regations <- rutils::do_call_rbind(agg_regations)
# Coerce agg_regations into xts series
agg_regations <- xts(agg_regations,
    order.by=index(price_s[end_points]))
head(agg_regations)
# Plot aggregations with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("red", "green")
chart_Series(agg_regations, theme=plot_theme,
       name="price aggregations")
legend("top", legend=colnames(agg_regations),
  bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")

# library(HighFreq)  # load package HighFreq
# Define functional for rolling aggregations over end_points
roll_agg <- function(x_ts, end_points, FUN, ...) {
  n_rows <- NROW(end_points)
# start_points are single-period lag of end_points
  start_points <- c(1, end_points[1:(n_rows-1)])
# Perform aggregations over look_backs list
  agg_regations <- lapply(look_backs,
    function(look_back) FUN(x_ts[look_back], ...))  # end lapply
# rbind list into single xts or matrix
  agg_regations <- rutils::do_call_rbind(agg_regations)
  if (!is.xts(agg_regations))
    agg_regations <-  # Coerce agg_regations into xts series
    xts(agg_regations, order.by=index(x_ts[end_points]))
  agg_regations
}  # end roll_agg
# Apply sum() over end_points
agg_regations <-
  roll_agg(price_s, end_points=end_points, FUN=sum)
agg_regations <-
  period.apply(price_s, INDEX=end_points, FUN=sum)
# Benchmark the speed of aggregation functions
summary(microbenchmark(
  roll_agg=roll_agg(price_s, end_points=end_points, FUN=sum),
  period_apply=period.apply(price_s, INDEX=end_points, FUN=sum),
  times=10))[, c(1, 4, 5)]
agg_regations <- period.sum(price_s, INDEX=end_points)
head(agg_regations)

# library(HighFreq)  # load package HighFreq
# load package HighFreq
library(HighFreq)
# extract closing minutely prices
price_s <- quantmod::Cl(HighFreq::SPY["2012-02-01/2012-04-01"])
# Apply "mean" over daily periods
agg_regations <- apply.daily(price_s, FUN=sum)
head(agg_regations)

# Number of n_points that fit over n_rows
n_points <- 22
n_agg <- n_rows %/% n_points
# end_points with stub interval at beginning
end_points <- # Define end_points with beginning stub
  n_rows-n_points*n_agg + (0:n_agg)*n_points
# Number of data points in look_back interval
look_back <- 252
# start_points are end_points lagged by look_back
start_points <- (end_points-look_back+1)
start_points <- ifelse(start_points > 0,
  start_points, 1)
# Define list of look-back intervals for aggregations over past
look_backs <- lapply(seq_along(end_points),
  function(in_dex) {
    start_points[in_dex]:end_points[in_dex]
})  # end lapply
# Perform lapply() loop over look_backs list
agg_regations <- lapply(look_backs,
    function(look_back) {
x_ts <- price_s[look_back]
c(max=max(x_ts), min=min(x_ts))
    })  # end lapply
# rbind list into single xts or matrix
agg_regations <- rutils::do_call_rbind(agg_regations)
# Coerce agg_regations into xts series
agg_regations <- xts(agg_regations,
    order.by=index(price_s[end_points]))
# Plot aggregations with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("red", "green")
chart_Series(agg_regations, theme=plot_theme,
       name="price aggregations")
legend("top", legend=colnames(agg_regations),
  bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")

library(HighFreq)  # load package HighFreq
agg_regations <- cbind(price_s, agg_regations)
tail(agg_regations, 22)
agg_regations <- na.omit(xts:::na.locf.xts(agg_regations))
# Plot aggregations with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("black", "red", "green")
chart_Series(agg_regations, theme=plot_theme,
       name="price aggregations")
legend("top", legend=colnames(agg_regations),
  bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")

set.seed(1121)  # reset random number generator
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
library(zoo)  # load package zoo
# Create zoo time series of random returns
in_dex <- Sys.Date() + 0:365
zoo_series <-
  zoo(rnorm(NROW(in_dex)), order.by=in_dex)
# Create monthly dates
dates_agg <- as.Date(as.yearmon(index(zoo_series)))
# Perform monthly mean aggregation
zoo_agg <- aggregate(zoo_series, by=dates_agg,
               FUN=mean)
# merge with original zoo - union of dates
zoo_agg <- cbind(zoo_series, zoo_agg)
# replace NA's using locf
zoo_agg <- na.locf(zoo_agg, na.rm=FALSE)
# extract aggregated zoo
zoo_agg <- zoo_agg[index(zoo_series), 2]

# library(HighFreq)  # load package HighFreq
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
zoo_agg <- aggregate(zoo_series, by=dates_agg,
               FUN=mean)
# merge with original zoo - union of dates
zoo_agg <- cbind(zoo_series, zoo_agg)
# replace NA's using linear interpolation
zoo_agg <- na.approx(zoo_agg)
# extract interpolated zoo
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
# merge with original zoo - union of dates
zoo_mean <- cbind(zoo_series, zoo_mean)
# replace NA's using na.locf
zoo_mean <- na.locf(zoo_mean, na.rm=FALSE, fromLast=TRUE)
# extract mean zoo
zoo_mean <- zoo_mean[index(zoo_series), 2]
# Plot original and interpolated zoo
plot(cumsum(zoo_series), xlab="", ylab="")
lines(cumsum(zoo_mean), lwd=2, col="red")
# Add legend
legend("topright", inset=0.05, cex=0.8, title="Mean Prices",
 leg=c("orig prices", "mean prices"), lwd=2, bg="white",
 col=c("black", "red"), bty="n")

n_rows <- 1000
r_norm <- rnorm(n_rows)
sd(r_norm)
mad(r_norm)
median(abs(r_norm - median(r_norm)))
median(abs(r_norm - median(r_norm)))/qnorm(0.75)
# Bootstrap of sd and mad estimators
boot_data <- sapply(1:10000, function(x) {
  boot_sample <-
    r_norm[sample.int(n_rows, replace=TRUE)]
  c(sd=sd(boot_sample), mad=mad(boot_sample))
})  # end sapply
boot_data <- t(boot_data)
# Analyze bootstrapped variance
head(boot_data)
sum(is.na(boot_data))
# Means and standard errors from bootstrap
apply(boot_data, MARGIN=2,
function(x) c(mean=mean(x), std_error=sd(x)))
# Parallel bootstrap under Windows
library(parallel)  # load package parallel
n_cores <- detectCores() - 1  # Number of cores
clus_ter <- makeCluster(n_cores)  # initialize compute cluster
boot_data <- parLapply(clus_ter, 1:10000,
  function(x, r_norm) {
    boot_sample <-
r_norm[sample.int(n_rows, replace=TRUE)]
    c(sd=sd(boot_sample), mad=mad(boot_sample))
  }, r_norm=r_norm)  # end parLapply
# Parallel bootstrap under Mac-OSX or Linux
boot_data <- mclapply(1:10000,
  function(x) {
    boot_sample <-
r_norm[sample.int(n_rows, replace=TRUE)]
    c(sd=sd(boot_sample), mad=mad(boot_sample))
  }, mc.cores=n_cores)  # end mclapply
stopCluster(clus_ter)  # Stop R processes over cluster
boot_data <- rutils::do_call(rbind, boot_data)
# Means and standard errors from bootstrap
apply(boot_data, MARGIN=2,
function(x) c(mean=mean(x), std_error=sd(x)))

price_s <- rutils::etf_env$VTI[, 4]
# Define look-back window and a half window
win_dow <- 11
# Calculate time series of medians
medi_an <- TTR::runMedian(price_s, n=win_dow)
# Plot prices and medians
dygraphs::dygraph(cbind(price_s, medi_an), main="VTI median") %>%
  dyOptions(colors=c("black", "red"))
# Calculate time series of z-scores
ma_d <- TTR::runMAD(price_s, n=win_dow)
z_scores <- (price_s - medi_an)/ma_d
z_scores[1:win_dow, ] <- 0
tail(z_scores, win_dow)
range(z_scores)

# Plot histogram of z-scores
x11(width=6, height=5)
histo_gram <- hist(z_scores, col="lightgrey",
  xlab="z-scores", breaks=50, xlim=c(-4, 4),
  ylab="frequency", freq=FALSE,
  main="Z-scores histogram")
lines(density(z_scores, adjust=1.5),
lwd=3, col="blue")

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

# Plot ROC Curve for Defaults
plot(x=error_rates[, "typeI"],
     y=1-error_rates[, "typeII"],
     xlab="FALSE positive rate",
     ylab="TRUE positive rate",
     xlim=c(0, 1), ylim=c(0, 1),
     main="ROC Curve for Hampel Classifier",
     type="l", lwd=3, col="blue")
abline(a=0.0, b=1.0, lwd=3, col="orange")
