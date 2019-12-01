library(knitr)
opts_chunk$set(prompt=TRUE, eval=FALSE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=6, fig.height=5)
options(width=60, dev='pdf')
options(digits=3)
thm <- knit_theme$get("acid")
knit_theme$set(thm)

# Symbols for constant maturity Treasury rates
sym_bols <- c("DGS1", "DGS2", "DGS5", "DGS10", "DGS20", "DGS30")
library(quantmod)  # Load package quantmod
rates_env <- new.env()  # new environment for data
# Download data for sym_bols into rates_env
getSymbols(sym_bols, env=rates_env, src="FRED")
ls(rates_env)  # list files in rates_env
# Get class of object in rates_env
class(get(x=sym_bols[1], envir=rates_env))
# Another way
class(rates_env$DGS20)
colnames(rates_env$DGS20)
save(rates_env, file="C:/Develop/R/lecture_slides/data/rates_data.RData")

x11(width=6, height=4)
par(mar=c(2, 2, 0, 0), oma=c(0, 0, 0, 0))
head(rates_env$DGS20, 3)
# Get class of all objects in rates_env
sapply(rates_env, class)
# Get class of all objects in R workspace
sapply(ls(), function(ob_ject) class(get(ob_ject)))
# Plot 20-year constant maturity Treasury rate
chart_Series(rates_env$DGS20["1990/"],
  name="20-year constant maturity Treasury rate")

par(mar=c(3, 3, 2, 0), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
# Load constant maturity Treasury rates
load(file="C:/Develop/R/lecture_slides/data/rates_data.RData")
# Get end-of-year dates since 2006
date_s <- xts::endpoints(rates_env$DGS1["2006/"], on="years")
date_s <- zoo::index(rates_env$DGS1["2006/"])[date_s]
# Create time series of end-of-year rates
rate_s <- eapply(rates_env, function(ra_te) ra_te[date_s])
rate_s <- rutils::do_call(cbind, rate_s)
# Rename columns and rows, sort columns, and transpose into matrix
colnames(rate_s) <- substr(colnames(rate_s), start=4, stop=11)
rate_s <- rate_s[, order(as.numeric(colnames(rate_s)))]
colnames(rate_s) <- paste0(colnames(rate_s), "yr")
rate_s <- t(rate_s)
colnames(rate_s) <- substr(colnames(rate_s), start=1, stop=4)
# Plot matrix using plot.zoo()
col_ors <- colorRampPalette(c("red", "blue"))(NCOL(rate_s))
plot.zoo(rate_s, main="Yield curve since 2006", lwd=3, xaxt="n",
   plot.type="single", xlab="maturity", ylab="yield", col=col_ors)
# Add x-axis
axis(1, seq_along(rownames(rate_s)), rownames(rate_s))
# Add legend
legend("topleft", legend=colnames(rate_s),
 col=col_ors, lty=1, lwd=4, inset=0.05, cex=0.8)

# Alternative plot using matplot()
matplot(rate_s, main="Yield curve since 2006", xaxt="n", lwd=3, lty=1,
  type="l", xlab="maturity", ylab="yield", col=col_ors)
# Add x-axis
axis(1, seq_along(rownames(rate_s)), rownames(rate_s))
# Add legend
legend("bottomright", legend=colnames(rate_s),
 col=col_ors, lty=1, lwd=4, inset=0.05, cex=0.8)

x11(width=6, height=4)
par(mar=c(0, 0, 0, 0), oma=c(0, 0, 0, 0), mgp=c(0, 0, 0))
# Load constant maturity Treasury rates
load(file="C:/Develop/R/lecture_slides/data/rates_data.RData")
# Symbols for constant maturity Treasury rates
sym_bols <- c("DGS1", "DGS2", "DGS5", "DGS20")
# Calculate daily rates changes
rate_s <- xts:::na.locf.xts(rutils::do_call(cbind,
    as.list(rates_env)[sym_bols]))
rate_s <- xts:::na.locf.xts(rate_s)
rate_s <- xts:::na.locf.xts(rate_s, fromLast=TRUE)
re_turns <- rutils::diff_it(rate_s)
date_s <- index(re_turns)
# De-mean (center) and scale the returns
re_turns <- t(t(re_turns) - colMeans(re_turns))
re_turns <- t(t(re_turns) / sqrt(colSums(re_turns^2)/(NROW(re_turns)-1)))
re_turns <- xts(re_turns, date_s)
# Correlation matrix of Treasury rates
cor_mat <- cor(re_turns)
# reorder correlation matrix based on clusters
library(corrplot)
or_der <- corrMatOrder(cor_mat, order="hclust",
  hclust.method="complete")
cor_mat <- cor_mat[or_der, or_der]

# Plot the correlation matrix
col_ors <- colorRampPalette(c("red", "white", "blue"))
corrplot(cor_mat, title=NA, tl.col="black",
  tl.cex=0.8, mar=c(0,0,0,0), method="square",
  col=col_ors(8), cl.offset=0.75, cl.cex=0.7,
  cl.align.text="l", cl.ratio=0.25)
title("Correlation of Treasury rates", line=-1)
# Draw rectangles on the correlation matrix plot
corrRect.hclust(cor_mat, k=NROW(cor_mat) %/% 2,
  method="complete", col="red")

# Create initial vector of portfolio weights
n_weights <- NROW(sym_bols)
weight_s <- rep(1/sqrt(n_weights), n_weights)
names(weight_s) <- sym_bols
# objective function equal to minus portfolio variance
object_ive <- function(weight_s, re_turns) {
  portf_rets <- re_turns %*% weight_s
  -sum(portf_rets*portf_rets) +
    1e7*(1 - sum(weight_s*weight_s))^2
}  # end object_ive
# objective for equal weight portfolio
object_ive(weight_s, re_turns)
# Compare speed of vector multiplication methods
summary(microbenchmark(
  trans_pose=t(re_turns) %*% re_turns,
  s_um=sum(re_turns*re_turns),
  times=10))[, c(1, 4, 5)]

# find weights with maximum variance
optim_run <- optim(par=weight_s,
  fn=object_ive,
  re_turns=re_turns,
  method="L-BFGS-B",
  upper=rep(1.0, n_weights),
  lower=rep(-1.0, n_weights))
# optimal weights and maximum variance
weight_s <- optim_run$par
-object_ive(weight_s, re_turns)
# Plot first principal component loadings
barplot(weight_s, names.arg=names(weight_s),
  xlab="", ylab="",
  main="first principal component loadings")

# pc1 weights and returns
weights_1 <- weight_s
pc_1 <- re_turns %*% weights_1
# redefine objective function
object_ive <- function(weight_s, re_turns) {
  portf_rets <- re_turns %*% weight_s
  -sum(portf_rets*portf_rets) +
    1e7*(1 - sum(weight_s*weight_s))^2 +
    1e7*sum(pc_1*portf_rets)^2
}  # end object_ive
# find second principal component weights
optim_run <- optim(par=weight_s,
             fn=object_ive,
             re_turns=re_turns,
             method="L-BFGS-B",
             upper=rep(1.0, n_weights),
             lower=rep(-1.0, n_weights))

# pc2 weights and returns
weights_2 <- optim_run$par
pc_2 <- re_turns %*% weights_2
sum(pc_1*pc_2)
# Plot second principal component loadings
barplot(weights_2, names.arg=names(weights_2),
  xlab="", ylab="",
  main="second principal component loadings")

# Covariance matrix and variance vector of returns
cov_mat <- cov(re_turns)
vari_ance <- diag(cov_mat)
cor_mat <- cor(re_turns)
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

# eigen decomposition of covariance matrix
cov_mat <- cov(re_turns)
ei_gen <- eigen(cov_mat)
# Perform PCA without scaling
pc_a <- prcomp(re_turns, scale=FALSE)
# compare outputs
all.equal(ei_gen$values, pc_a$sdev^2)
all.equal(abs(ei_gen$vectors), abs(pc_a$rotation),
    check.attributes=FALSE)
# eigen decomposition of correlation matrix
cor_mat <- cor(re_turns)
ei_gen <- eigen(cor_mat)
# Perform PCA with scaling
pc_a <- prcomp(re_turns, scale=TRUE)
# compare outputs
all.equal(ei_gen$values, pc_a$sdev^2)
all.equal(abs(ei_gen$vectors), abs(pc_a$rotation),
    check.attributes=FALSE)

# Perform principal component analysis PCA
pc_a <- prcomp(re_turns, scale=TRUE)
# Plot standard deviations
barplot(pc_a$sdev,
  names.arg=colnames(pc_a$rotation),
  las=3, xlab="", ylab="",
  main="Scree Plot: Volatilities of Principal Components
  of Treasury rates")

x11(width=6, height=7)
# Calculate principal component loadings (weights)
pc_a$rotation
# Plot loading barplots in multiple panels
par(mfrow=c(3,2))
par(mar=c(2, 2, 2, 1), oma=c(0, 0, 0, 0))
for (or_der in 1:NCOL(pc_a$rotation)) {
  barplot(pc_a$rotation[, or_der],
  las=3, xlab="", ylab="", main="")
  title(paste0("PC", or_der), line=-2.0,
  col.main="red")
}  # end for

# Calculate products of principal component time series
round(t(pc_a$x) %*% pc_a$x, 2)
# Calculate principal component time series
pca_ts <- xts(re_turns %*% pc_a$rotation,
          order.by=index(re_turns))
pca_ts <- cumsum(pca_ts)
# Plot principal component time series in multiple panels
par(mfrow=c(3,2))
par(mar=c(2, 2, 0, 1), oma=c(0, 0, 0, 0))
ra_nge <- range(pca_ts)
for (or_der in 1:NCOL(pca_ts)) {
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

library(quantmod)  # Load quantmod
library(RQuantLib)  # Load RQuantLib
# Specify curve parameters
curve_params <- list(tradeDate=as.Date("2018-01-17"),
               settleDate=as.Date("2018-01-19"),
               dt=0.25,
               interpWhat="discount",
               interpHow="loglinear")
# Specify market data: prices of FI instruments
market_data <- list(d3m=0.0363,
              fut1=96.2875,
              fut2=96.7875,
              fut3=96.9875,
              fut4=96.6875,
              s5y=0.0443,
              s10y=0.05165,
              s15y=0.055175)
# Specify dates for calculating the zero rates
disc_dates <- seq(0, 10, 0.25)
# Specify the evaluation (as of) date
setEvaluationDate(as.Date("2018-01-17"))
# Calculate the zero rates
disc_curves <- DiscountCurve(params=curve_params,
                       tsQuotes=market_data,
                       times=disc_dates)
# Plot the zero rates
x11()
plot(x=disc_curves$zerorates, t="l", main="zerorates")

# futures contracts codes
future_s <- rbind(c("S&P500 index", "ES"),
              c("10yr Treasury", "ZN"),
              c("VIX index", "VX"),
              c("Gold", "GC"),
              c("Oil", "CL"),
              c("Euro FX", "EC"),
              c("Swiss franc", "SF"),
              c("Japanese Yen", "JY"))
colnames(future_s) <- c("Futures contract", "Code")
print(xtable::xtable(future_s), comment=FALSE, size="scriptsize", include.rownames=FALSE, latex.environments="flushleft")

# Monthly futures contract codes
month_codes <- cbind(c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"),
                     c("F", "G", "H", "J", "K", "M", "N", "Q", "U", "V", "X", "Z"))
colnames(month_codes) <- c("Month", "Code")
print(xtable::xtable(month_codes), comment=FALSE, size="scriptsize", include.rownames=FALSE, latex.environments="flushright")

# futures contracts codes
future_s <- rbind(c("S&P500 index", "SP", "ES"),
              c("10yr Treasury", "ZN", "ZN"),
              c("VIX index", "VX", "delisted"),
              c("Gold", "GC", "YG"),
              c("Oil", "CL", "QM"),
              c("Euro FX", "EC", "E7"),
              c("Swiss franc", "SF", "MSF"),
              c("Japanese Yen", "JY", "J7"))
colnames(future_s) <- c("Futures contract", "Standard", "E-mini")
print(xtable::xtable(future_s), comment=FALSE, size="scriptsize", include.rownames=FALSE, latex.environments="flushleft")

# Load data for S&P Emini futures June 2019 contract
sym_bol <- "ES"
dir_name <- "C:/Develop/data/ib_data"
file_name <- file.path(dir_name, paste0(sym_bol, ".csv"))
# Read a data table from CSV file
price_s <- data.table::fread(file_name)
# Coerce price_s into data frame
data.table::setDF(price_s)
# Or
# price_s <- data.table:::as.data.frame.data.table(
#   data.table::fread(file_name))
# first column of price_s is a numeric date-time
tail(price_s)
# Coerce price_s into xts series
price_s <- xts::xts(price_s[, 2:6],
  order.by=as.Date(as.POSIXct.numeric(price_s[, 1],
    tz="America/New_York",
    origin="1970-01-01")))
colnames(price_s) <- c("Open", "High", "Low", "Close", "Volume")
tail(price_s)

# Plot OHLC data in x11 window
x11(width=5, height=4)  # Open x11 for plotting
par(mar=c(5, 5, 2, 1), oma=c(0, 0, 0, 0))
chart_Series(x=price_s, TA="add_Vo()",
  name="S&P500 futures")
# Plot dygraph
dygraphs::dygraph(price_s[, 1:4], main="OHLC prices") %>%
  dyCandlestick()

# Load ESU8 data
dir_name <- "C:/Develop/data/ib_data"
file_name <- file.path(dir_name, "ESU8.csv")
ES_U8 <- data.table::fread(file_name)
data.table::setDF(ES_U8)
ES_U8 <- xts::xts(ES_U8[, 2:6],
  order.by=as.Date(as.POSIXct.numeric(ES_U8[, 1],
    tz="America/New_York", origin="1970-01-01")))
colnames(ES_U8) <- c("Open", "High", "Low", "Close", "Volume")
# Load ESM8 data
file_name <- file.path(dir_name, "ESM8.csv")
ES_M8 <- data.table::fread(file_name)
data.table::setDF(ES_M8)
ES_M8 <- xts::xts(ES_M8[, 2:6],
  order.by=as.Date(as.POSIXct.numeric(ES_M8[, 1],
    tz="America/New_York", origin="1970-01-01")))
colnames(ES_M8) <- c("Open", "High", "Low", "Close", "Volume")

x11(width=6, height=5)  # Open x11 for plotting
# Plot last month of ESU8 and ESM8 volume data
en_d <- end(ES_M8)
star_t <- (en_d - 30*24*60^2)
vol_ume <- cbind(Vo(ES_U8),
  Vo(ES_M8))[paste0(star_t, "/", en_d)]
colnames(vol_ume) <- c("ESU8", "ESM8")
col_ors <- c("blue", "green")
plot(vol_ume, col=col_ors, lwd=3, major.ticks="days",
     format.labels="%b-%d", observation.based=TRUE,
     main="Volumes of ESU8 and ESM8 futures")
legend("topleft", legend=colnames(vol_ume), col=col_ors,
 title=NULL, bty="n", lty=1, lwd=6, inset=0.1, cex=0.7)

# Find date when ESU8 volume exceeds ESM8
exceed_s <- (vol_ume[, "ESU8"] > vol_ume[, "ESM8"])
in_dex <- match(TRUE, exceed_s)
# in_dex <- min(which(exceed_s))
# Scale the ES_M8 prices
in_dex <- index(exceed_s[in_dex])
fac_tor <- as.numeric(Cl(ES_U8[in_dex])/Cl(ES_M8[in_dex]))
ES_M8[, 1:4] <- fac_tor*ES_M8[, 1:4]
# Calculate continuous contract prices
chain_ed <- rbind(ES_M8[index(ES_M8) < in_dex],
            ES_U8[index(ES_U8) >= in_dex])
# Or
# Chain_ed <- rbind(ES_M8[paste0("/", in_dex-1)],
#                   ES_U8[paste0(in_dex, "/")])
# Plot continuous contract prices
chart_Series(x=chain_ed["2018"], TA="add_Vo()",
  name="S&P500 chained futures")

# Download VIX index data from CBOE
vix_index <- data.table::fread("http://www.cboe.com/publish/scheduledtask/mktdata/datahouse/vixcurrent.csv", skip=1)
class(vix_index)
dim(vix_index)
tail(vix_index)
sapply(vix_index, class)
vix_index <- xts(vix_index[, -1],
  order.by=as.Date(vix_index$Date, format="%m/%d/%Y"))
colnames(vix_index) <- c("Open", "High", "Low", "Close")
# Save the VIX data to binary file
load(file="C:/Develop/data/ib_data/vix_cboe.RData")
ls(vix_env)
vix_env$vix_index <- vix_index
ls(vix_env)
save(vix_env, file="C:/Develop/data/ib_data/vix_cboe.RData")
# Plot OHLC data in x11 window
chart_Series(x=vix_index["2018"], name="VIX Index")
# Plot dygraph
dygraphs::dygraph(vix_index, main="VIX Index") %>%
  dyCandlestick()

# Read CBOE monthly futures expiration dates
date_s <- read.csv(
  file="C:/Develop/data/vix_data/vix_dates.csv",
  stringsAsFactors=FALSE)
date_s <- as.Date(date_s[, 1])
year_s <- format(date_s, format="%Y")
year_s <- substring(year_s, 4)
# Monthly futures contract codes
month_codes <-
  c("F", "G", "H", "J", "K", "M",
    "N", "Q", "U", "V", "X", "Z")
sym_bols <- paste0("VX", month_codes, year_s)
date_s <- as.data.frame(date_s)
colnames(date_s) <- "exp_dates"
rownames(date_s) <- sym_bols
# Write dates to CSV file, with row names
write.csv(date_s, row.names=TRUE,
  file="C:/Develop/data/vix_data/vix_futures.csv")
# Read back CBOE futures expiration dates
date_s <- read.csv(file="C:/Develop/data/vix_data/vix_futures.csv",
  stringsAsFactors=FALSE, row.names=1)
date_s[, 1] <- as.Date(date_s[, 1])

# Load VIX futures data from binary file
load(file="C:/Develop/data/vix_data/vix_cboe.RData")
# Get all VIX futures for 2018 except January
sym_bols <- ls(vix_env)
sym_bols <- sym_bols[grep("*8", sym_bols)]
sym_bols <- sym_bols[2:9]
# Specify dates for curves
low_vol <- as.Date("2018-01-11")
hi_vol <- as.Date("2018-02-05")
# Extract all VIX futures prices on the dates
curve_s <- lapply(sym_bols, function(sym_bol) {
  x_ts <- get(x=sym_bol, envir=vix_env)
  Cl(x_ts[c(low_vol, hi_vol)])
})  # end lapply
curve_s <- rutils::do_call(cbind, curve_s)
colnames(curve_s) <- sym_bols
curve_s <- t(coredata(curve_s))
colnames(curve_s) <- c("Contango 01/11/2018",
                 "Backwardation 02/05/2018")

x11(width=7, height=5)
par(mar=c(3, 2, 1, 1), oma=c(0, 0, 0, 0))
plot(curve_s[, 1], type="l", lty=1, col="blue", lwd=3,
     xaxt="n", xlab="", ylab="", ylim=range(curve_s),
     main="VIX Futures Curves")
axis(1, at=(1:NROW(curve_s)), labels=rownames(curve_s))
lines(curve_s[, 2], lty=1, lwd=3, col="red")
legend(x="topright", legend=colnames(curve_s),
 inset=0.05, cex=1.0, bty="n",
 col=c("blue", "red"), lwd=6, lty=1)

# Load VIX futures data from binary file
load(file="C:/Develop/data/vix_data/vix_cboe.RData")
# Read CBOE futures expiration dates
date_s <- read.csv(file="C:/Develop/data/vix_data/vix_futures.csv",
  stringsAsFactors=FALSE, row.names=1)
sym_bols <- rownames(date_s)
date_s <- as.Date(date_s[, 1])
to_day <- as.Date("2018-05-07")
maturi_ty <- (to_day + 30)
# Find neighboring futures contracts
in_dex <- match(TRUE, date_s > maturi_ty)
front_date <- date_s[in_dex-1]
back_date <- date_s[in_dex]
front_symbol <- sym_bols[in_dex-1]
back_symbol <- sym_bols[in_dex]
front_price <- get(x=front_symbol, envir=vix_env)
front_price <- as.numeric(Cl(front_price[to_day]))
back_price <- get(x=back_symbol, envir=vix_env)
back_price <- as.numeric(Cl(back_price[to_day]))
# Calculate the constant maturity 30-day futures price
ra_tio <- as.numeric(maturi_ty - front_date) /
  as.numeric(back_date - front_date)
pric_e <- (ra_tio*back_price + (1-ra_tio)*front_price)

library(HighFreq)
x11(width=5, height=3)  # Open x11 for plotting
# Load VIX futures data from binary file
load(file="C:/Develop/data/vix_data/vix_cboe.RData")
# Plot VIX and SVXY data in x11 window
plot_theme <- chart_theme()
plot_theme$col$line.col <- "blue"
chart_Series(x=Cl(vix_env$vix_index["2007/"]),
       theme=plot_theme, name="VIX Index")
chart_Series(x=Cl(rutils::etf_env$VTI["2007/"]),
       theme=plot_theme, name="VTI ETF")

chart_Series(x=Cl(vix_env$vix_index["2017/2018"]),
       theme=plot_theme, name="VIX Index")
chart_Series(x=Cl(rutils::etf_env$SVXY["2017/2018"]),
       theme=plot_theme, name="SVXY ETF")

library(xtable)
# Read table of fundamental data into data frame
fundamental_data <-
  read.csv(file="C:/Develop/R/lecture_slides/data/fundamental_stock_data.csv",
               stringsAsFactors=FALSE)

print(xtable(fundamental_data), comment=FALSE, size="scriptsize", include.rownames=FALSE)

library(xtable)
# Read table of fundamental data into data frame
fundamental_data <-
  read.csv(file="C:/Develop/R/lecture_slides/data/fundamental_stock_data.csv",
               stringsAsFactors=FALSE)

print(xtable(fundamental_data), comment=FALSE, size="scriptsize", include.rownames=FALSE)

library(Quandl)  # Load package Quandl
# Register Quandl API key
Quandl.api_key("pVJi9Nv3V8CD3Js5s7Qx")

# Quandl stock market data
# https://blog.quandl.com/stock-market-data-ultimate-guide-part-1
# https://blog.quandl.com/stock-market-data-the-ultimate-guide-part-2

# Download RAYMOND metadata
# https://www.quandl.com/data/RAYMOND-Raymond/documentation/metadata

# Download S&P500 Index constituents
# https://s3.amazonaws.com/static.quandl.com/tickers/SP500.csv

# Download AAPL gross profits from RAYMOND
prof_it <-
  Quandl("RAYMOND/AAPL_GROSS_PROFIT_Q", type="xts")
chart_Series(prof_it, name="AAPL gross profits")

# Download multiple time series
price_s <- Quandl(code=c("NSE/OIL", "WIKI/AAPL"),
   start_date="2013-01-01", type="xts")

# Download datasets for AAPL
# https://www.quandl.com/api/v3/datasets/WIKI/AAPL.json

# Download metadata for AAPL
price_s <- Quandl(code=c("NSE/OIL", "WIKI/AAPL"),
   start_date="2013-01-01", type="xts")
# https://www.quandl.com/api/v3/datasets/WIKI/AAPL/metadata.json

# scrape fundamental data from Google using quantmod - doesn't work
funda_mentals <- getFinancials("HPQ", src="google", auto.assign=FALSE)
# view quarterly fundamentals
viewFinancials(funda_mentals,  period="Q")
viewFinancials(funda_mentals)

# scrape fundamental data from Yahoo using quantmod
# table of Yahoo data fields
# http://www.financialwisdomforum.org/gummy-stuff/Yahoo-data.htm

met_rics <- yahooQF(c("Price/Sales",
                "P/E Ratio",
                "Price/EPS Estimate Next Year",
                "PEG Ratio",
                "Dividend Yield",
                "Market Capitalization"))


sym_bols <- c("AAPL", "IBM", "MSFT")
# Not all the metrics are returned by Yahoo.
funda_mentals <- getQuote(paste(sym_bols, sep="", collapse=";"), src="yahoo", what=met_rics)
viewFinancials(funda_mentals,  period="Q")

funda_mentals <- getFinancials("HPQ", src="yahoo", auto.assign=FALSE)
viewFinancials(funda_mentals)



library(HighFreq)  # Load package HighFreq
install.packages("devtools")
library(devtools)
# Install package WRDS from github
install_github("WRDS/R-package")
library(WRDS)  # Load package WRDS
# Register WRDS API key
WRDS.api_key("pVJi9Nv3V8CD3Js5s7Qx")
# Get short description
packageDescription("WRDS")
# Load help page
help(package="WRDS")
# Remove WRDS from search path
detach("package:WRDS")

library(HighFreq)  # Load package HighFreq
env_sp500 <- new.env()  # new environment for data
# Remove all files (if necessary)
rm(list=ls(env_sp500), envir=env_sp500)
# Boolean vector of symbols already downloaded
down_loaded <- tick_ers %in% ls(env_sp500)
# Download data and copy it into environment
for (tick_er in tick_ers[!down_loaded]) {
  cat("processing: ", tick_er, "\n")
  da_ta <- Quandl(code=paste0("WIKI/", tick_er),
            start_date="1990-01-01",
            type="xts")[, -(1:7)]
  colnames(da_ta) <- paste(tick_er,
    c("Open", "High", "Low", "Close", "Volume"), sep=".")
  assign(tick_er, da_ta, envir=env_sp500)
}  # end for
save(env_sp500, file="C:/Develop/R/lecture_slides/data/sp500.RData")
chart_Series(x=env_sp500$XOM["2016/"], TA="add_Vo()",
       name="XOM stock")

library(HighFreq)  # Load package HighFreq
# Download Fama-French factors from KFRENCH database
fac_tors <- Quandl(code="KFRENCH/FACTORS_D",
  start_date="2001-01-01", type="xts")
dim(fac_tors)
head(fac_tors)
tail(fac_tors)
chart_Series(cumsum(fac_tors["2001/", 1]/100),
  name="Fama-French factors")

# Load package HighFreq
library(HighFreq)
# Or load the high frequency data file directly:
symbol_s <- load("C:/Develop/R/HighFreq/data/hf_data.RData")
head(SPY_TAQ)
head(SPY)
tail(SPY)

# Load package HighFreq
library(HighFreq)
head(SPY)

# Install package HighFreq from github
devtools::install_github(repo="algoquant/HighFreq")
# Load package HighFreq
library(HighFreq)
# Get documentation for package HighFreq
# Get short description
packageDescription("HighFreq")
# Load help page
help(package="HighFreq")
# List all datasets in "HighFreq"
data(package="HighFreq")
# List all objects in "HighFreq"
ls("package:HighFreq")
# Remove HighFreq from search path
detach("package:HighFreq")

# Load package HighFreq
library(HighFreq)
# you can see SPY when listing objects in HighFreq
ls("package:HighFreq")
# you can see SPY when listing datasets in HighFreq
data(package="HighFreq")
# but the SPY dataset isn't listed in the workspace
ls()
# HighFreq datasets are lazy loaded and available when needed
head(SPY)
# Load all the datasets in package HighFreq
data(hf_data)
# HighFreq datasets are now loaded and in the workspace
head(SPY)

# Install package IBrokers
install.packages("IBrokers")
# Load package IBrokers
library(IBrokers)
# Get documentation for package IBrokers
# Get short description
packageDescription("IBrokers")
# Load help page
help(package="IBrokers")
# List all datasets in "IBrokers"
data(package="IBrokers")
# List all objects in "IBrokers"
ls("package:IBrokers")
# Remove IBrokers from search path
detach("package:IBrokers")
# Install package IBrokers2
devtools::install_github(repo="algoquant/IBrokers2")

# Connect to Interactive Brokers TWS
ib_connect <- IBrokers::twsConnect(port=7497)
# Or connect to IB Gateway
# Ib_connect <- ibgConnect(port=4002)
# Check connection
IBrokers::isConnected(ib_connect)
# Close the Interactive Brokers API connection
IBrokers::twsDisconnect(ib_connect)

# Connect to Interactive Brokers TWS
ib_connect <- IBrokers::twsConnect(port=7497)
# Or connect to IB Gateway
# Ib_connect <- ibgConnect(port=4002)
# Download account information from IB
ac_count <- "DU1215081"
ib_account <- IBrokers::reqAccountUpdates(conn=ib_connect,
                                    acctCode=ac_count)
# Extract account balances
balance_s <- ib_account[[1]]
balance_s$AvailableFunds
# Extract contract names, net positions, and profits and losses
IBrokers::twsPortfolioValue(ib_account)
# Close the Interactive Brokers API connection
IBrokers::twsDisconnect(ib_connect)

# Define AAPL stock contract (object)
con_tract <- IBrokers::twsEquity("AAPL", primary="SMART")
# Define CHF currency contract
con_tract <- IBrokers::twsCurrency("CHF", currency="USD")
# Define S&P Emini future June 2019 contract
con_tract <- IBrokers::twsFuture(symbol="ES",
  exch="GLOBEX", expiry="201906")
# Define 10yr Treasury future June 2019 contract
con_tract <- IBrokers::twsFuture(symbol="ZN",
  exch="ECBOT", expiry="201906")
# Define euro currency future June 2019 contract
con_tract <- IBrokers::twsFuture(symbol="EUR",
  exch="GLOBEX", expiry="201906")
# Define Gold future June 2019 contract
con_tract <- IBrokers::twsFuture(symbol="GC",
  exch="NYMEX", expiry="201906")
# Define Oil future January 2019 contract
con_tract <- IBrokers::twsFuture(symbol="QM",
  exch="NYMEX", expiry="201901")
# Test if contract object is correct
IBrokers::is.twsContract(con_tract)
# Get list with instrument information
IBrokers::reqContractDetails(conn=ib_connect, Contract=con_tract)
# Install the package twsInstrument
install.packages("twsInstrument", repos="http://r-forge.r-project.org")
# Define euro future using getContract() and Conid
con_tract <- twsInstrument::getContract("317631411")
# Get list with instrument information
IBrokers::reqContractDetails(conn=ib_connect, Contract=con_tract)

# Define VIX monthly and weekly futures June 2019 contract
sym_bol <- "VIX"
con_tract <- IBrokers::twsFuture(symbol=sym_bol,
  exch="CFE", expiry="201906")
# Define VIX monthly futures June 2019 contract
con_tract <- IBrokers::twsFuture(symbol=sym_bol,
  local="VXV8", exch="CFE", expiry="201906")
# Define VIX weekly futures October 3rd 2018 contract
con_tract <- IBrokers::twsFuture(symbol=sym_bol,
  local="VX40V8", exch="CFE", expiry="201906")
# Get list with instrument information
IBrokers::reqContractDetails(conn=ib_connect,
  Contract=con_tract)

# Define S&P Emini futures June 2019 contract
sym_bol <- "ES"
con_tract <- IBrokers::twsFuture(symbol=sym_bol,
  exch="GLOBEX", expiry="201906")
# Open file for data download
dir_name <- "C:/Develop/data/ib_data"
dir.create(dir_name)
file_name <- file.path(dir_name, paste0(sym_bol, "_201906.csv"))
file_connect <- file(file_name, open="w")
# Connect to Interactive Brokers TWS
ib_connect <- IBrokers::twsConnect(port=7497)
# Write header to file
cat(paste(paste(sym_bol, c("Index", "Open", "High", "Low", "Close", "Volume", "WAP", "Count"), sep="."), collapse=","), "\n", file=file_connect)
# Download historical data to file
IBrokers::reqHistoricalData(conn=ib_connect,
  Contract=con_tract,
  barSize="1 day", duration="6 M",
  file=file_connect)
# Close data file
close(file_connect)
# Close the Interactive Brokers API connection
IBrokers::twsDisconnect(ib_connect)

# Define IB contract objects for stock symbols
sym_bols <- c("AAPL", "F", "MSFT")
con_tracts <- lapply(sym_bols, IBrokers::twsEquity, primary="SMART")
names(con_tracts) <- sym_bols
# Open file connections for data download
dir_name <- "C:/Develop/data/ib_data"
file_names <- file.path(dir_name, paste0(sym_bols, format(Sys.time(), format="_%m_%d_%Y_%H_%M"), ".csv"))
file_connects <- lapply(file_names, function(file_name) file(file_name, open="w"))
# Connect to Interactive Brokers TWS
ib_connect <- IBrokers::twsConnect(port=7497)
# Download historical 1-minute bar data to files
for (it in 1:NROW(sym_bols)) {
  sym_bol <- sym_bols[it]
  file_connect <- file_connects[[it]]
  con_tract <- con_tracts[[it]]
  cat("Downloading data for: ", sym_bol, "\n")
  # Write header to file
  cat(paste(paste(sym_bol, c("Index", "Open", "High", "Low", "Close", "Volume", "WAP", "XTRA", "Count"), sep="."), collapse=","), "\n", file=file_connect)
  IBrokers::reqHistoricalData(conn=ib_connect,
                         Contract=con_tract,
                         barSize="1 min", duration="2 D",
                         file=file_connect)
  Sys.sleep(10) # 10s pause to avoid IB pacing violation
}  # end for
# Close data files
for (file_connect in file_connects) close(file_connect)
# Close the Interactive Brokers API connection
IBrokers::twsDisconnect(ib_connect)

# Define S&P Emini futures June 2018 contract
sym_bol <- "ES"
con_tract <- IBrokers::twsFuture(symbol=sym_bol,
  include_expired="1",
  exch="GLOBEX", expiry="201806")
# Open file connection for ESM8 data download
file_name <- file.path(dir_name, paste0(sym_bol, "M8.csv"))
file_connect <- file(file_name, open="w")
# Connect to Interactive Brokers TWS
ib_connect <- IBrokers::twsConnect(port=7497)
# Download historical data to file
IBrokers::reqHistoricalData(conn=ib_connect,
  Contract=con_tract,
  barSize="1 day", duration="2 Y",
  file=file_connect)
# Close data file
close(file_connect)
# Close the Interactive Brokers API connection
IBrokers::twsDisconnect(ib_connect)

# Load OHLC data and coerce it into xts series
price_s <- data.table::fread(file_name)
data.table::setDF(price_s)
price_s <- xts::xts(price_s[, 2:6],
  order.by=as.Date(as.POSIXct.numeric(price_s[, 1],
    tz="America/New_York", origin="1970-01-01")))
colnames(price_s) <- c("Open", "High", "Low", "Close", "Volume")
# Plot OHLC data in x11 window
chart_Series(x=price_s, TA="add_Vo()",
  name="S&P500 ESM8 futures")
# Plot dygraph
dygraphs::dygraph(price_s[, 1:4], main="S&P500 ESM8 futures") %>%
  dyCandlestick()

# Define S&P Emini futures June 2018 contract
sym_bol <- "ES"
con_tract <- IBrokers::twsFuture(symbol=sym_bol,
  include_expired="1",
  exch="GLOBEX", expiry="201806")
# Open file connection for data download
dir_name <- "C:/Develop/data/ib_data"
dir.create(dir_name)
file_name <- file.path(dir_name, paste0(sym_bol, ".csv"))
file_connect <- file(file_name, open="w")
# Connect to Interactive Brokers TWS
ib_connect <- IBrokers::twsConnect(port=7497)
# Download historical data to file
IBrokers::reqHistoricalData(conn=ib_connect,
  Contract=con_tract,
  barSize="1 day", duration="6 M",
  file=file_connect)
# Close data file
close(file_connect)
# Close the Interactive Brokers API connection
IBrokers::twsDisconnect(ib_connect)

# Define S&P Emini futures June 2019 contract
sym_bol <- "ES"
con_tract <- IBrokers::twsFuture(symbol=sym_bol,
  exch="GLOBEX", expiry="201906")
# Open file connection for data download
dir_name <- "C:/Develop/data/ib_data"
# Dir.create(dir_name)
file_name <- file.path(dir_name, paste0(sym_bol, "_taq_live.csv"))
file_connect <- file(file_name, open="w")
# Connect to Interactive Brokers TWS
ib_connect <- IBrokers::twsConnect(port=7497)
# Download live data to file
IBrokers::reqMktData(conn=ib_connect,
     Contract=con_tract,
     eventWrapper=eWrapper.MktData.CSV(1),
     file=file_connect)
# Close data file
close(file_connect)
# Close the Interactive Brokers API connection
IBrokers::twsDisconnect(ib_connect)

# Define S&P Emini futures June 2019 contract
sym_bol <- "ES"
con_tract <- IBrokers::twsFuture(symbol=sym_bol,
  exch="GLOBEX", expiry="201906")
# Open file connection for data download
dir_name <- "C:/Develop/data/ib_data"
# Dir.create(dir_name)
file_name <- file.path(dir_name, paste0(sym_bol, "_ohlc_live.csv"))
file_connect <- file(file_name, open="w")
# Connect to Interactive Brokers TWS
ib_connect <- IBrokers::twsConnect(port=7497)
# Download live data to file
IBrokers::reqRealTimeBars(conn=ib_connect,
     Contract=con_tract, barSize="1",
     eventWrapper=eWrapper.RealTimeBars.CSV(1),
     file=file_connect)
# Close the Interactive Brokers API connection
IBrokers::twsDisconnect(ib_connect)
# Close data file
close(file_connect)
# Load OHLC data and coerce it into xts series
library(data.table)
price_s <- data.table::fread(file_name)
price_s <- xts::xts(price_s[, paste0("V", 2:6)],
  as.POSIXct.numeric(as.numeric(price_s[, V1]), tz="America/New_York", origin="1970-01-01"))
colnames(price_s) <- c("Open", "High", "Low", "Close", "Volume")
# Plot OHLC data in x11 window
x11()
chart_Series(x=price_s, TA="add_Vo()",
       name="S&P500 ESM9 futures")
# Plot dygraph
library(dygraphs)
dygraphs::dygraph(price_s[, 1:4], main="S&P500 ESM9 futures") %>%
  dyCandlestick()

library(IBrokers)
# Define list of S&P futures and 10yr Treasury contracts
con_tracts <- list(ES=IBrokers::twsFuture(symbol="ES", exch="GLOBEX", expiry="201906"),
             ZN=IBrokers::twsFuture(symbol="ZN", exch="ECBOT", expiry="201906"))
# Open the file connection for storing the bar data
dir_name <- "C:/Develop/data/ib_data"
file_names <- file.path(dir_name, paste0(c("ES_", "ZN_"), format(Sys.time(), format="%m_%d_%Y_%H_%M"), ".csv"))
file_connects <- lapply(file_names, function(file_name) file(file_name, open="w"))
# Connect to Interactive Brokers TWS
ib_connect <- IBrokers::twsConnect(port=7497)
# Download live data to file
IBrokers::reqRealTimeBars(conn=ib_connect,
                    Contract=con_tracts,
                    barSize="1", useRTH=FALSE,
                    eventWrapper=eWrapper.RealTimeBars.CSV(NROW(con_tracts)),
                    file=file_connects)
# Close the Interactive Brokers API connection
IBrokers::twsDisconnect(ib_connect)
# Close data files
for (file_connect in file_connects)
  close(file_connect)
library(data.table)
# Load ES futures June 2019 contract and coerce it into xts series
price_s <- data.table::fread(file_names[1])
price_s <- xts::xts(price_s[, paste0("V", 2:6)],
  as.POSIXct.numeric(as.numeric(price_s[, V1]), tz="America/New_York", origin="1970-01-01"))
colnames(price_s) <- c("Open", "High", "Low", "Close", "Volume")
# Plot dygraph
library(dygraphs)
dygraphs::dygraph(price_s[, 1:4], main="S&P500 ESM9 futures") %>%
  dyCandlestick()
# Load ZN 10yr Treasury futures June 2019 contract
price_s <- data.table::fread(file_names[2])
price_s <- xts::xts(price_s[, paste0("V", 2:6)],
  as.POSIXct.numeric(as.numeric(price_s[, V1]), tz="America/New_York", origin="1970-01-01"))
colnames(price_s) <- c("Open", "High", "Low", "Close", "Volume")
# Plot dygraph
dygraphs::dygraph(price_s[, 1:4], main="ZN 10yr Treasury futures") %>%
  dyCandlestick()

# Define S&P Emini future June 2019 contract
con_tract <- IBrokers::twsFuture(symbol="ES", exch="GLOBEX", expiry="201906")
# Define euro currency contract EUR.USD
con_tract <- IBrokers::twsCurrency("EUR", currency="USD")
# Define euro currency E-mini futures June 2019 contract E7Z8
con_tract <- IBrokers::twsFuture(symbol="E7", exch="GLOBEX", expiry="201906")
# Define Japanese yen currency contract JPY.USD
con_tract <- IBrokers::twsCurrency("JPY", currency="USD")
# Define Japanese yen currency E-mini futures June 2019 contract J7Z8
con_tract <- IBrokers::twsFuture(symbol="J7", exch="GLOBEX", expiry="201906")
# Define Japanese yen currency futures June 2019 contract 6JZ8
con_tract <- IBrokers::twsFuture(symbol="JPY", exch="GLOBEX", expiry="201906")
# Connect to Interactive Brokers TWS
ib_connect <- IBrokers::twsConnect(port=7497)
IBrokers::reqContractDetails(conn=ib_connect, Contract=con_tract)
# Request trade order ID
order_id <- IBrokers::reqIds(ib_connect)
# Create buy market order object
ib_order <- IBrokers::twsOrder(order_id,
  orderType="MKT", action="BUY", totalQuantity=1)
# Place trade order
IBrokers::placeOrder(ib_connect, con_tract, ib_order)
# Execute sell market order
order_id <- IBrokers::reqIds(ib_connect)
ib_order <- IBrokers::twsOrder(order_id,
  orderType="MKT", action="SELL", totalQuantity=1)
IBrokers::placeOrder(ib_connect, con_tract, ib_order)
# Execute buy market order
order_id <- IBrokers::reqIds(ib_connect)
ib_order <- IBrokers::twsOrder(order_id,
  orderType="MKT", action="BUY", totalQuantity=1)
IBrokers::placeOrder(ib_connect, con_tract, ib_order)

# Request trade order ID
order_id <- IBrokers::reqIds(ib_connect)
# Create buy limit order object
ib_order <- IBrokers::twsOrder(order_id, orderType="LMT",
  lmtPrice="1.1511", action="BUY", totalQuantity=1)
# Place trade order
IBrokers::placeOrder(ib_connect, con_tract, ib_order)
# Cancel trade order
IBrokers::cancelOrder(ib_connect, order_id)
# Execute sell limit order
order_id <- IBrokers::reqIds(ib_connect)
ib_order <- IBrokers::twsOrder(order_id, orderType="LMT",
  lmtPrice="1.1512", action="SELL", totalQuantity=1)
IBrokers::placeOrder(ib_connect, con_tract, ib_order)
# Cancel trade order
IBrokers::cancelOrder(ib_connect, order_id)
# Close the Interactive Brokers API connection
IBrokers::twsDisconnect(ib_connect)

eWrapper_realtimebars <- function(n = 1) {
  eW <- eWrapper_new(NULL)
  # eW <- IBrokers::eWrapper(NULL)
  eW$assign.Data("data", rep(list(structure(.xts(matrix(rep(NA_real_, 7), ncol = 7), 0), .Dimnames = list(NULL, c("Open", "High", "Low", "Close", "Volume", "WAP", "Count")))), n))
  eW$realtimeBars <- function(curMsg, msg, timestamp, file, ...) {
    id <- as.numeric(msg[2])
    file <- file[[id]]
    data <- eW$get.Data("data")
    attr(data[[id]], "index") <- as.numeric(msg[3])
    nr.data <- NROW(data[[id]])
    # Write to file
    cat(paste(msg[3], msg[4], msg[5], msg[6], msg[7], msg[8], msg[9], msg[10], sep = ","), "\n", file = file, append = TRUE)
    # Write to console
    # eW$count_er <- eW$count_er + 1
    eW$assign.Data("count_er", eW$get.Data("count_er")+1)
    cat(paste0("count_er=", eW$get.Data("count_er"), "\tOpen=", msg[4], "\tHigh=", msg[5], "\tLow=", msg[6], "\tClose=", msg[7], "\tVolume=", msg[8]), "\n")
    # cat(paste0("Open=", msg[4], "\tHigh=", msg[5], "\tLow=", msg[6], "\tClose=", msg[7], "\tVolume=", msg[8]), "\n")
    #Trade
    # Cancel previous trade orders
    buy_id <- eW$get.Data("buy_id")
    sell_id <- eW$get.Data("sell_id")
    if (buy_id>0) IBrokers::cancelOrder(ib_connect, buy_id)
    if (sell_id>0) IBrokers::cancelOrder(ib_connect, sell_id)
    # Execute buy limit order
    buy_id <- IBrokers::reqIds(ib_connect)
    buy_order <- IBrokers::twsOrder(buy_id, orderType="LMT",
                              lmtPrice=msg[6]-0.25, action="BUY", totalQuantity=1)
    IBrokers::placeOrder(ib_connect, con_tract, buy_order)
    # Execute sell limit order
    sell_id <- IBrokers::reqIds(ib_connect)
    sell_order <- IBrokers::twsOrder(sell_id, orderType="LMT",
                               lmtPrice=msg[5]+0.25, action="SELL", totalQuantity=1)
    IBrokers::placeOrder(ib_connect, con_tract, sell_order)
    # Copy new trade orders
    eW$assign.Data("buy_id", buy_id)
    eW$assign.Data("sell_id", sell_id)
    #Trade finished
    data[[id]][nr.data, 1:7] <- as.numeric(msg[4:10])
    eW$assign.Data("data", data)
    c(curMsg, msg)
  }  # end eW$realtimeBars
  return(eW)
}  # end eWrapper_realtimebars

eWrapper_realtimebars <- function(n = 1) {
  eW <- eWrapper_new(NULL)
  # eW <- IBrokers::eWrapper(NULL)
  eW$assign.Data("data", rep(list(structure(.xts(matrix(rep(NA_real_, 7), ncol = 7), 0), .Dimnames = list(NULL, c("Open", "High", "Low", "Close", "Volume", "WAP", "Count")))), n))
  eW$realtimeBars <- function(curMsg, msg, timestamp, file, ...) {
    id <- as.numeric(msg[2])
    file <- file[[id]]
    data <- eW$get.Data("data")
    attr(data[[id]], "index") <- as.numeric(msg[3])
    nr.data <- NROW(data[[id]])
    # Write to file
    cat(paste(msg[3], msg[4], msg[5], msg[6], msg[7], msg[8], msg[9], msg[10], sep = ","), "\n", file = file, append = TRUE)
    # Write to console
    # eW$count_er <- eW$count_er + 1
    eW$assign.Data("count_er", eW$get.Data("count_er")+1)
    cat(paste0("count_er=", eW$get.Data("count_er"), "\tOpen=", msg[4], "\tHigh=", msg[5], "\tLow=", msg[6], "\tClose=", msg[7], "\tVolume=", msg[8]), "\n")
    # cat(paste0("Open=", msg[4], "\tHigh=", msg[5], "\tLow=", msg[6], "\tClose=", msg[7], "\tVolume=", msg[8]), "\n")
    #Trade
    # Cancel previous trade orders
    buy_id <- eW$get.Data("buy_id")
    sell_id <- eW$get.Data("sell_id")
    if (buy_id>0) IBrokers::cancelOrder(ib_connect, buy_id)
    if (sell_id>0) IBrokers::cancelOrder(ib_connect, sell_id)
    # Execute buy limit order
    buy_id <- IBrokers::reqIds(ib_connect)
    buy_order <- IBrokers::twsOrder(buy_id, orderType="LMT",
                              lmtPrice=msg[6]-0.25, action="BUY", totalQuantity=1)
    IBrokers::placeOrder(ib_connect, con_tract, buy_order)
    # Execute sell limit order
    sell_id <- IBrokers::reqIds(ib_connect)
    sell_order <- IBrokers::twsOrder(sell_id, orderType="LMT",
                               lmtPrice=msg[5]+0.25, action="SELL", totalQuantity=1)
    IBrokers::placeOrder(ib_connect, con_tract, sell_order)
    # Copy new trade orders
    eW$assign.Data("buy_id", buy_id)
    eW$assign.Data("sell_id", sell_id)
    #Trade finished
    data[[id]][nr.data, 1:7] <- as.numeric(msg[4:10])
    eW$assign.Data("data", data)
    c(curMsg, msg)
  }  # end eW$realtimeBars
  return(eW)
}  # end eWrapper_realtimebars

# Define S&P Emini futures June 2019 contract
snp_contract <- IBrokers::twsFuture(symbol="ES",
  exch="GLOBEX", expiry="201906")
# Define VIX futures June 2019 contract
vix_contract <- IBrokers::twsFuture(symbol="VIX",
  local="VXZ8", exch="CFE", expiry="201906")
# Define 10yr Treasury futures June 2019 contract
trs_contract <- IBrokers::twsFuture(symbol="ZN",
  exch="ECBOT", expiry="201906")
# Define Emini gold futures June 2019 contract
gold_contract <- IBrokers::twsFuture(symbol="YG",
  exch="NYSELIFFE", expiry="201906")
# Define euro currency future June 2019 contract
euro_contract <- IBrokers::twsFuture(symbol="EUR",
  exch="GLOBEX", expiry="201906")
IBrokers::reqContractDetails(conn=ib_connect, Contract=euro_contract)

# Define data directory
dir_name <- "C:/Develop/data/ib_data"
# Dir.create(dir_name)

# Open file for error messages
file_root <- "replay"
file_name <- file.path(dir_name, paste0(file_root, "_error.csv"))
error_connect <- file(file_name, open="w")

# Open file for raw data
file_name <- file.path(dir_name, paste0(file_root, "_raw.csv"))
raw_connect <- file(file_name, open="w")

# Create empty eWrapper to redirect error messages to error file
error_ewrapper <- eWrapper(debug=NULL, errfile=error_connect)

# Create eWrapper for raw data
raw_ewrapper <- eWrapper(debug=TRUE)

# Redirect error messages to error eWrapper (error_ewrapper),
# by replacing handler function errorMessage() in raw_ewrapper
raw_ewrapper$errorMessage <- error_ewrapper$errorMessage

# Connect to Interactive Brokers TWS
ib_connect <- IBrokers::twsConnect(port=7497)

# Download raw data for multiple contracts for replay
IBrokers::reqMktData(ib_connect,
  list(snp_contract, vix_contract, trs_contract, gold_contract, euro_contract),
  eventWrapper=raw_ewrapper, file=raw_connect)

# Close the Interactive Brokers API connection
IBrokers::twsDisconnect(ib_connect)

# Close data files
close(raw_connect)
close(error_connect)

Replay the raw data

# Open file with raw data
file_name <- file.path(dir_name, paste0(file_root, "_raw.csv"))
raw_connect <- IBrokers::twsConnect(file_name)
class(raw_connect) <- c("twsPlayback", class(raw_connect))
# Replay the raw data
IBrokers::reqMktData(raw_connect, list(snp_contract, vix_contract))

# Open file for data
file_connect <- file(file.path(dir_name, "temp.csv"), open="w")
# Download TAQ data to file
IBrokers::reqMktData(conn=raw_connect,
     Contract=snp_contract,
     eventWrapper=eWrapper.MktData.CSV(1),
     file=file_connect)

# Close file for TAQ data
close(file_connect)
# Close file with raw data
IBrokers::twsDisconnect(raw_connect)


# Define AAPL stock contract (object)
con_tract <- IBrokers::twsEquity("AAPL", primary="SMART")
# Define CHF currency contract
con_tract <- IBrokers::twsCurrency("CHF", currency="USD")
# Define S&P Emini future June 2019 contract
con_tract <- IBrokers::twsFuture(symbol="ES",
  exch="GLOBEX", expiry="201906")
# Define 10yr Treasury future June 2019 contract
con_tract <- IBrokers::twsFuture(symbol="ZN",
  exch="ECBOT", expiry="201906")
# Define euro currency future June 2019 contract
con_tract <- IBrokers::twsFuture(symbol="EUR",
  exch="GLOBEX", expiry="201906")
# Define Gold future June 2019 contract
con_tract <- IBrokers::twsFuture(symbol="GC",
  exch="NYMEX", expiry="201906")
# Test if contract object is correct
IBrokers::is.twsContract(con_tract)
# Get list with instrument information
IBrokers::reqContractDetails(conn=ib_connect, Contract=con_tract)
# Install the package twsInstrument
install.packages("twsInstrument", repos="http://r-forge.r-project.org")
# Define euro future using getContract() and Conid
con_tract <- twsInstrument::getContract("317631411")
# Get list with instrument information
IBrokers::reqContractDetails(conn=ib_connect, Contract=con_tract)

# Define AAPL stock contract (object)
con_tract <- IBrokers::twsEquity("AAPL", primary="SMART")
# Define CHF currency contract
con_tract <- IBrokers::twsCurrency("CHF", currency="USD")
# Define S&P Emini future June 2019 contract
con_tract <- IBrokers::twsFuture(symbol="ES",
  exch="GLOBEX", expiry="201906")
# Define 10yr Treasury future June 2019 contract
con_tract <- IBrokers::twsFuture(symbol="ZN",
  exch="ECBOT", expiry="201906")
# Define euro currency future June 2019 contract
con_tract <- IBrokers::twsFuture(symbol="EUR",
  exch="GLOBEX", expiry="201906")
# Define Gold future June 2019 contract
con_tract <- IBrokers::twsFuture(symbol="GC",
  exch="NYMEX", expiry="201906")
# Test if contract object is correct
IBrokers::is.twsContract(con_tract)
# Get list with instrument information
IBrokers::reqContractDetails(conn=ib_connect, Contract=con_tract)
# Install the package twsInstrument
install.packages("twsInstrument", repos="http://r-forge.r-project.org")
# Define euro future using getContract() and Conid
con_tract <- twsInstrument::getContract("317631411")
# Get list with instrument information
IBrokers::reqContractDetails(conn=ib_connect, Contract=con_tract)
