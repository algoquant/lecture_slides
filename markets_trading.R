library(knitr)
opts_chunk$set(prompt=TRUE, eval=FALSE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='tiny', fig.width=6, fig.height=5)
options(width=80, dev='pdf')
options(digits=3)
thm <- knit_theme$get("acid")
knit_theme$set(thm)

# Load S&P500 constituent stock prices
load("C:/Develop/lecture_slides/data/sp500.RData")
price_s <- eapply(sp500_env, quantmod::Cl)
price_s <- rutils::do_call(cbind, price_s)
# Carry forward non-NA prices
price_s <- zoo::na.locf(price_s, na.rm=FALSE)
# Drop ".Close" from column names
colnames(price_s[, 1:4])
colnames(price_s) <- rutils::get_name(colnames(price_s))
# Or
# colnames(price_s) <- do.call(rbind,
#   strsplit(colnames(price_s), split="[.]"))[, 1]
# Calculate percentage returns of the S&P500 constituent stocks
# re_turns <- xts::diff.xts(log(price_s))
re_turns <- xts::diff.xts(price_s)/
  rutils::lag_it(price_s, pad_zeros=FALSE)
set.seed(1121)
sam_ple <- sample(NCOL(re_turns), s=100, replace=FALSE)
prices_100 <- price_s[, sam_ple]
returns_100 <- re_turns[, sam_ple]
save(price_s, prices_100,
  file="C:/Develop/lecture_slides/data/sp500_prices.RData")
save(re_turns, returns_100,
  file="C:/Develop/lecture_slides/data/sp500_returns.RData")

# Calculate number of constituents without prices
da_ta <- rowSums(is.na(price_s))
da_ta <- xts::xts(da_ta, order.by=index(price_s))
dygraphs::dygraph(da_ta, main="Number of S&P 500 Constituents Without Prices") %>%
  dyOptions(colors="blue", strokeWidth=2) %>%
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
sym_bols <- c("VTI", "VEU", "EEM", "XLY", "XLP", "XLE", "XLF",
  "XLV", "XLI", "XLB", "XLK", "XLU", "VYM", "IVW", "IWB", "IWD",
  "IWF", "IEF", "TLT", "VNQ", "DBC", "GLD", "USO", "VXX", "SVXY",
  "MTUM", "IVE", "VLUE", "QUAL", "VTV", "USMV")
# Read etf database into data frame
etf_list <- read.csv(file="C:/Develop/lecture_slides/data/etf_list.csv")
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

# Symbols for constant maturity Treasury rates
sym_bols <- c("DGS1", "DGS2", "DGS5", "DGS10", "DGS20", "DGS30")
# Create new environment for time series
rates_env <- new.env()
# Download time series for sym_bols into rates_env
quantmod::getSymbols(sym_bols, env=rates_env, src="FRED")
# List files in rates_env
ls(rates_env)
# Get class of all objects in rates_env
sapply(rates_env, class)
# Get class of all objects in R workspace
sapply(ls(), function(nam_e) class(get(nam_e)))
# Save the time series environment into a binary .RData file
save(rates_env, file="C:/Develop/lecture_slides/data/rates_data.RData")

# Get class of time series object DGS10
class(get(x="DGS10", envir=rates_env))
# Another way
class(rates_env$DGS10)
# Get first 6 rows of time series
head(rates_env$DGS10)
# Plot dygraphs of 10-year Treasury rate
dygraphs::dygraph(rates_env$DGS10, main="10-year Treasury Rate") %>%
  dyOptions(colors="blue", strokeWidth=2)
# Plot 10-year constant maturity Treasury rate
x11(width=6, height=5)
par(mar=c(2, 2, 0, 0), oma=c(0, 0, 0, 0))
chart_Series(rates_env$DGS10["1990/"], name="10-year Treasury Rate")

# Load constant maturity Treasury rates
load(file="C:/Develop/lecture_slides/data/rates_data.RData")
# Get most recent yield curve
yc_2021 <- eapply(rates_env, xts::last)
class(yc_2021)
yc_2021 <- do.call(cbind, yc_2021)
# Check if 2020-03-25 is not a holiday
weekdays(as.Date("2020-03-25"))
# Get yield curve from 2020-03-25
yc_2020 <- eapply(rates_env, function(x) x[as.Date("2020-03-25")])
yc_2020 <- do.call(cbind, yc_2020)
# Combine the yield curves
rate_s <- c(yc_2020, yc_2021)
# Rename columns and rows, sort columns, and transpose into matrix
colnames(rate_s) <- substr(colnames(rate_s), start=4, stop=11)
rate_s <- rate_s[, order(as.numeric(colnames(rate_s)))]
colnames(rate_s) <- paste0(colnames(rate_s), "yr")
rate_s <- t(rate_s)
colnames(rate_s) <- substr(colnames(rate_s), start=1, stop=4)

x11(width=6, height=5)
par(mar=c(3, 3, 2, 0), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
# Plot using matplot()
col_ors <- c("blue", "red")
matplot(rate_s, main="Yield Curves in 2020 and 2021", xaxt="n", lwd=3, lty=1,
  type="l", xlab="maturity", ylab="yield", col=col_ors)
# Add x-axis
axis(1, seq_along(rownames(rate_s)), rownames(rate_s))
# Add legend
legend("topleft", legend=colnames(rate_s),
 col=col_ors, lty=1, lwd=6, inset=0.05, cex=1.0)

x11(width=6, height=5)
par(mar=c(3, 3, 2, 0), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
# Load constant maturity Treasury rates
load(file="C:/Develop/lecture_slides/data/rates_data.RData")
# Get end-of-year dates since 2006
date_s <- xts::endpoints(rates_env$DGS1["2006/"], on="years")
date_s <- zoo::index(rates_env$DGS1["2006/"][date_s])
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
legend("topleft", legend=colnames(rate_s),
 col=col_ors, lty=1, lwd=4, inset=0.05, cex=0.8)

# Extract rates from rates_env
sym_bols <- c("DGS1", "DGS2", "DGS5", "DGS10", "DGS20")
rate_s <- mget(sym_bols, envir=rates_env)
rate_s <- rutils::do_call(cbind, rate_s)
rate_s <- zoo::na.locf(rate_s, na.rm=FALSE)
rate_s <- zoo::na.locf(rate_s, fromLast=TRUE)
# Calculate daily percentage rates changes
re_turns <- rutils::diff_it(log(rate_s))
# De-mean the returns
re_turns <- lapply(re_turns, function(x) {x - mean(x)})
re_turns <- rutils::do_call(cbind, re_turns)
sapply(re_turns, mean)
# Covariance and Correlation matrices of Treasury rates
cov_mat <- cov(re_turns)
cor_mat <- cor(re_turns)
# Reorder correlation matrix based on clusters
library(corrplot)
or_der <- corrMatOrder(cor_mat, order="hclust",
  hclust.method="complete")
cor_mat <- cor_mat[or_der, or_der]

# Plot the correlation matrix
x11(width=6, height=6)
col_ors <- colorRampPalette(c("red", "white", "blue"))
corrplot(cor_mat, title=NA, tl.col="black",
    method="square", col=col_ors(NCOL(cor_mat)), tl.cex=0.8,
    cl.offset=0.75, cl.cex=0.7, cl.align.text="l", cl.ratio=0.25)
title("Correlation of Treasury Rates", line=1)
# Draw rectangles on the correlation matrix plot
corrRect.hclust(cor_mat, k=NROW(cor_mat) %/% 2,
  method="complete", col="red")

# Create initial vector of portfolio weights
n_weights <- NROW(sym_bols)
weight_s <- rep(1/sqrt(n_weights), n_weights)
names(weight_s) <- sym_bols
# Objective function equal to minus portfolio variance
object_ive <- function(weight_s, re_turns) {
  portf_rets <- re_turns %*% weight_s
  -1e7*var(portf_rets) + 1e7*(1 - sum(weight_s*weight_s))^2
}  # end object_ive
# Objective for equal weight portfolio
object_ive(weight_s, re_turns)
# Compare speed of vector multiplication methods
library(microbenchmark)
summary(microbenchmark(
  trans_pose=t(re_turns) %*% re_turns,
  s_um=sum(re_turns*re_turns),
  times=10))[, c(1, 4, 5)]

# Find weights with maximum variance
op_tim <- optim(par=weight_s,
  fn=object_ive,
  re_turns=re_turns,
  method="L-BFGS-B",
  upper=rep(5.0, n_weights),
  lower=rep(-5.0, n_weights))
# Optimal weights and maximum variance
weights_1 <- op_tim$par
object_ive(weights_1, re_turns)
# Plot first principal component loadings
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
barplot(weights_1, names.arg=names(weights_1),
  xlab="", ylab="", main="First Principal Component Loadings")

# pc1 weights and returns
pc_1 <- drop(re_turns %*% weights_1)
# Redefine objective function
object_ive <- function(weight_s, re_turns) {
  portf_rets <- re_turns %*% weight_s
  -1e7*var(portf_rets) + 1e7*(1 - sum(weight_s^2))^2 +
    1e7*sum(weights_1*weight_s)^2
}  # end object_ive
# Find second principal component weights
op_tim <- optim(par=weight_s,
             fn=object_ive,
             re_turns=re_turns,
             method="L-BFGS-B",
             upper=rep(5.0, n_weights),
             lower=rep(-5.0, n_weights))

# pc2 weights and returns
weights_2 <- op_tim$par
pc_2 <- drop(re_turns %*% weights_2)
sum(pc_1*pc_2)
# Plot second principal component loadings
barplot(weights_2, names.arg=names(weights_2),
  xlab="", ylab="", main="Second Principal Component Loadings")

ei_gen <- eigen(cov_mat)
ei_gen$vectors
# Compare with optimization
all.equal(sum(diag(cov_mat)), sum(ei_gen$values))
all.equal(abs(ei_gen$vectors[, 1]), abs(weights_1), check.attributes=FALSE)
all.equal(abs(ei_gen$vectors[, 2]), abs(weights_2), check.attributes=FALSE)
all.equal(ei_gen$values[1], var(pc_1), check.attributes=FALSE)
all.equal(ei_gen$values[2], var(pc_2), check.attributes=FALSE)
# Eigenvalue equations are satisfied approximately
(cov_mat %*% weights_1) / weights_1 / var(pc_1)
(cov_mat %*% weights_2) / weights_2 / var(pc_2)
# Plot eigenvalues
barplot(ei_gen$values, names.arg=paste0("PC", 1:n_weights),
  las=3, xlab="", ylab="", main="Principal Component Variances")

# Eigen decomposition of correlation matrix
ei_gen <- eigen(cor_mat)
# Perform PCA with scaling
pc_a <- prcomp(re_turns, scale=TRUE)
# Compare outputs
all.equal(ei_gen$values, pc_a$sdev^2)
all.equal(abs(ei_gen$vectors), abs(pc_a$rotation),
    check.attributes=FALSE)
# Eigen decomposition of covariance matrix
ei_gen <- eigen(cov_mat)
# Perform PCA without scaling
pc_a <- prcomp(re_turns, scale=FALSE)
# Compare outputs
all.equal(ei_gen$values, pc_a$sdev^2)
all.equal(abs(ei_gen$vectors), abs(pc_a$rotation),
    check.attributes=FALSE)

# Perform principal component analysis PCA
pc_a <- prcomp(re_turns, scale=TRUE)
# Plot standard deviations
barplot(pc_a$sdev, names.arg=colnames(pc_a$rotation),
  las=3, xlab="", ylab="",
  main="Scree Plot: Volatilities of Principal Components
  of Treasury rates")

x11(width=6, height=7)
# Calculate principal component loadings (weights)
pc_a$rotation
# Plot loading barplots in multiple panels
par(mfrow=c(3,2))
par(mar=c(3.5, 2, 2, 1), oma=c(0, 0, 0, 0))
for (or_der in 1:NCOL(pc_a$rotation)) {
  barplot(pc_a$rotation[, or_der], las=3, xlab="", ylab="", main="")
  title(paste0("PC", or_der), line=-2.0, col.main="red")
}  # end for

# Standardize (de-mean and scale) the returns
re_turns <- lapply(re_turns, function(x) {(x - mean(x))/sd(x)})
re_turns <- rutils::do_call(cbind, re_turns)
sapply(re_turns, mean)
sapply(re_turns, sd)
# Calculate principal component time series
pca_ts <- re_turns %*% pc_a$rotation
all.equal(pc_a$x, pca_ts, check.attributes=FALSE)
# Calculate products of principal component time series
round(t(pca_ts) %*% pca_ts, 2)
# Coerce to xts time series
pca_ts <- xts(pca_ts, order.by=index(re_turns))
pca_ts <- cumsum(pca_ts)
# Plot principal component time series in multiple panels
par(mfrow=c(3,2))
par(mar=c(2, 2, 0, 1), oma=c(0, 0, 0, 0))
rang_e <- range(pca_ts)
for (or_der in 1:NCOL(pca_ts)) {
  plot.zoo(pca_ts[, or_der], ylim=rang_e, xlab="", ylab="")
  title(paste0("PC", or_der), line=-1, col.main="red")
}  # end for

# Invert all the principal component time series
pca_rets <- re_turns %*% pc_a$rotation
sol_ved <- pca_rets %*% solve(pc_a$rotation)
all.equal(coredata(re_turns), sol_ved)

# Invert first 3 principal component time series
sol_ved <- pca_rets[, 1:3] %*% solve(pc_a$rotation)[1:3, ]
sol_ved <- xts::xts(sol_ved, zoo::index(re_turns))
sol_ved <- cumsum(sol_ved)
cum_returns <- cumsum(re_turns)
# Plot the solved returns
par(mfrow=c(3,2))
par(mar=c(2, 2, 0, 1), oma=c(0, 0, 0, 0))
for (sym_bol in sym_bols) {
  plot.zoo(cbind(cum_returns[, sym_bol], sol_ved[, sym_bol]),
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

# Futures contracts codes
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

# Futures contracts codes
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
dir_name <- "C:/Develop/data/ib_data"
file_name <- file.path(dir_name, "ES_ohlc.csv")
# Read a data table from CSV file
price_s <- data.table::fread(file_name)
class(price_s)
# Coerce first column from string to date-time
unlist(sapply(price_s, class))
tail(price_s)
price_s$Index <- as.POSIXct(price_s$Index,
  tz="America/New_York", origin="1970-01-01")
# Coerce price_s into xts series
price_s <- data.table::as.xts.data.table(price_s)
class(price_s)
tail(price_s)
colnames(price_s)[1:5] <- c("Open", "High", "Low", "Close", "Volume")
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
# Coerce ES_U8 into xts series
ES_U8$V1 <- as.Date(as.POSIXct.numeric(ES_U8$V1,
    tz="America/New_York", origin="1970-01-01"))
ES_U8 <- data.table::as.xts.data.table(ES_U8)
colnames(ES_U8)[1:5] <- c("Open", "High", "Low", "Close", "Volume")
# Load ESM8 data
file_name <- file.path(dir_name, "ESM8.csv")
ES_M8 <- data.table::fread(file_name)
# Coerce ES_M8 into xts series
ES_M8$V1 <- as.Date(as.POSIXct.numeric(ES_M8$V1,
    tz="America/New_York", origin="1970-01-01"))
ES_M8 <- data.table::as.xts.data.table(ES_M8)
colnames(ES_M8)[1:5] <- c("Open", "High", "Low", "Close", "Volume")

x11(width=6, height=5)  # Open x11 for plotting
# Plot last month of ESU8 and ESM8 volume data
en_d <- end(ES_M8)
star_t <- (en_d - 30)
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
  file="C:/Develop/data/vix_data/vix_dates.csv")
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
  row.names=1)
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
  row.names=1)
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
  read.csv(file="C:/Develop/lecture_slides/data/fundamental_stock_data.csv")

print(xtable(fundamental_data), comment=FALSE, size="scriptsize", include.rownames=FALSE)

library(xtable)
# Read table of fundamental data into data frame
fundamental_data <-
  read.csv(file="C:/Develop/lecture_slides/data/fundamental_stock_data.csv")

print(xtable(fundamental_data), comment=FALSE, size="scriptsize", include.rownames=FALSE)

# Install package "rwrds"
devtools::install_github("davidsovich/rwrds")
# Load package "rwrds"
library(rwrds)
# Get documentation for package "rwrds"
packageDescription("rwrds")
# Load help page
help(package="rwrds")
# List all datasets in "rwrds"
data(package="rwrds")
# List all objects in "rwrds"
ls("package:rwrds")
# Remove rwrds from search path
detach("package:rwrds")

library(rwrds)
library(dplyr)
# Establish connection to WRDS
wrds_con <- rwrds::wrds_connect(username="jp3900", password="RipvanWinkle20")
# Download Compustat names table as dplyr object
names_table <- rwrds::compustat_names(wrds=wrds_con, subset=FALSE, dl=TRUE)
dim(names_table)
# Save names table as csv file
write.csv(names_table, file="C:/Develop/lecture_slides/data/compustat_table.csv", row.names=FALSE)
# rm(names_table)
# Read names table from csv file
names_table <- read.csv(file="C:/Develop/lecture_slides/data/compustat_table.csv")
# sym_bol <- "VTI"
# match(sym_bol, names_table$tic)
# Create ETF symbols (tickers)
sym_bols <- c("VTI", "VEU", "EEM")
# Get cusips of sym_bols
in_dex <- match(sym_bols, names_table$tic)
names(in_dex) <- sym_bols
etf_cusips <- names_table$cusip[in_dex]
names(etf_cusips) <- sym_bols
# Save cusips into text file
cat(etf_cusips, file="C:/Develop/lecture_slides/data/etf_cusips.txt", sep="\n")
# Save gvkeys into text file
etf_gvkeys <- names_table$gvkey[in_dex]
names(etf_gvkeys) <- sym_bols
cat(etf_gvkeys, file="C:/Develop/lecture_slides/data/etf_gvkeys.txt", sep="\n")

# Read .csv file with S&P500 constituents
sp500_table <- read.csv(file="C:/Develop/lecture_slides/data/sp500_constituents.csv")
class(sp500_table)
# Select unique sp500 tickers and save them into text file
sp500_tickers <- unique(sp500_table$co_tic)
cat(sp500_tickers, file="C:/Develop/lecture_slides/data/sp500_tickers.txt", sep="\n")
# Some gvkeys are duplicates
duplicate_s <- table(sp500_table$gvkey)
duplicate_s <- duplicate_s[duplicate_s > 1]
duplicate_s <- sp500_table[match(as.numeric(names(duplicate_s)), sp500_table$gvkey), ]
# Select unique gvkeys
sp500_gvkeys <- unique(sp500_table$gvkey)
# foo <- sp500_table[match(sp500_gvkeys, sp500_table$gvkey), ]
# Save gvkeys into text file
cat(sp500_gvkeys, file="C:/Develop/lecture_slides/data/sp500_gvkeys.txt", sep="\n")
# Select unique cusips and save into text file
sp500_cusips <- unique(sp500_table$co_cusip)
# Remove empty cusips
which(sp500_cusips == "")
sp500_cusips <- sp500_cusips[-which(sp500_cusips == "")]
cat(sp500_cusips, file="C:/Develop/lecture_slides/data/sp500_cusips.txt", sep="\n")
# Find the rows corresponding to the sp500_cusips
rows_cusips <- sp500_table[match(sp500_cusips, sp500_table$co_cusip), ]
# Find the rows corresponding to duplicate gvkeys
duplicate_s <- table(rows_cusips$gvkey)
duplicate_s <- duplicate_s[duplicate_s > 1]
duplicate_s <- rows_cusips[rows_cusips$gvkey %in% as.numeric(names(duplicate_s)), ]

library(rutils)  # Load package rutils
# Read .csv file with TAP OHLC prices
oh_lc <- read.csv(file="C:/Develop/lecture_slides/data/TAP.csv")
# oh_lc contains cusips not in sp500_cusips
cusip_s <- unique(oh_lc$cusip)
cusip_s %in% sp500_cusips
# Select data only for sp500_cusips
oh_lc <- oh_lc[oh_lc$cusip %in% sp500_cusips, ]
# oh_lc contains tickers not in sp500_tickers
ticker_s <- unique(oh_lc$tic)
ticker_s %in% sp500_tickers
# Select data only for sp500_tickers
oh_lc <- oh_lc[oh_lc$tic %in% sp500_tickers, ]
# Select ticker from sp500_table
sym_bol <- sp500_table$co_tic[match(oh_lc$gvkey[1], sp500_table$gvkey)]
# Adjustment factor and total return factor
adj_fact <- drop(oh_lc[, "ajexdi"])
tr_fact <- drop(oh_lc[, "trfd"])
# Extract index of dates
date_s <- drop(oh_lc[, "datadate"])
date_s <- lubridate::ymd(date_s)
# Select only OHLCV data columns
oh_lc <- oh_lc[, c("prcod", "prchd", "prcld", "prccd", "cshtrd")]
colnames(oh_lc) <- paste(sym_bol, c("Open", "High", "Low", "Close", "Volume"), sep=".")
# Coerce to xts series
oh_lc <- xts::xts(oh_lc, date_s)
# Fill the missing (NA) Open prices
is_na <- is.na(oh_lc[, 1])
oh_lc[is_na, 1] <- (oh_lc[is_na, 2] + oh_lc[is_na, 3])/2
sum(is.na(oh_lc))
# Adjust all the prices
oh_lc[, 1:4] <- tr_fact*oh_lc[, 1:4]/adj_fact/tr_fact[NROW(tr_fact)]
oh_lc <- na.omit(oh_lc)
plot(quantmod::Cl(oh_lc), main="TAP Stock")

# Define formatting function for OHLC prices
format_ohlc <- function(oh_lc, environ_ment) {
  # Select ticker from sp500_table
  sym_bol <- sp500_table$co_tic[match(oh_lc$gvkey[1], sp500_table$gvkey)]
  # Split adjustment and total return factors
  adj_fact <- drop(oh_lc[, c("ajexdi")])
  tr_fact <- drop(oh_lc[, "trfd"])
  tr_fact <- ifelse(is.na(tr_fact), 1, tr_fact)
  # Extract dates index
  date_s <- drop(oh_lc[, "datadate"])
  date_s <- lubridate::ymd(date_s)
  # Select only OHLCV data
  oh_lc <- oh_lc[, c("prcod", "prchd", "prcld", "prccd", "cshtrd")]
  colnames(oh_lc) <- paste(sym_bol, c("Open", "High", "Low", "Close", "Volume"), sep=".")
  # Coerce to xts series
  oh_lc <- xts::xts(oh_lc, date_s)
  # Fill NA prices
  is_na <- is.na(oh_lc[, 1])
  oh_lc[is_na, 1] <- (oh_lc[is_na, 2] + oh_lc[is_na, 3])/2
  # Adjust the prices
  oh_lc[, 1:4] <- tr_fact*oh_lc[, 1:4]/adj_fact/tr_fact[NROW(tr_fact)]
  # Copy the OHLCV data to environ_ment
  oh_lc <- na.omit(oh_lc)
  assign(x=sym_bol, value=oh_lc, envir=environ_ment)
  sym_bol
}  # end format_ohlc

# Load OHLC prices from .csv file downloaded from WRDS by cusip
sp500_prices <- read.csv(file="C:/Develop/lecture_slides/data/sp500_prices_bycusip.csv")
# sp500_prices contains cusips not in sp500_cusips
cusip_s <- unique(sp500_prices$cusip)
NROW(sp500_cusips); NROW(cusip_s)
# Select data only for sp500_cusips
sp500_prices <- sp500_prices[sp500_prices$cusip %in% sp500_cusips, ]
# sp500_prices contains tickers not in sp500_tickers
ticker_s <- unique(sp500_prices$tic)
NROW(sp500_tickers); NROW(ticker_s)
# Select data only for sp500_tickers
sp500_prices <- sp500_prices[sp500_prices$tic %in% sp500_tickers, ]
# Create new data environment
sp500_env <- new.env()
# Perform OHLC aggregations by cusip column
sp500_prices <- split(sp500_prices, sp500_prices$cusip)
process_ed <- lapply(sp500_prices, format_ohlc,
               environ_ment=sp500_env)

# Get end dates of series in sp500_env
end_dates <- eapply(sp500_env, end)
end_dates <- unlist(end_dates)
end_dates <- as.Date(end_dates)
# Remove elements with short end dates
se_lect <- (end_dates < max(end_dates))
rm(list=names(sp500_env)[se_lect], envir=sp500_env)
# Rename element "BRK.B" to "BRKB"
sp500_env$BRKB <- sp500_env$BRK.B
rm(BRK.B, envir=sp500_env)
names(sp500_env$BRKB) <- paste("BRKB",
  c("Open", "High", "Low", "Close", "Volume"), sep=".")
# Rename element "LOW" to "LOWES"
sp500_env$LOWES <- sp500_env$LOW
names(sp500_env$LOWES) <- paste("LO_WES",
  c("Open", "High", "Low", "Close", "Volume"), sep=".")
rm(LOW, envir=sp500_env)
# Rename element "BF.B" to "BFB"
sp500_env$BFB <- sp500_env$BF.B
names(sp500_env$BFB) <- paste("BFB",
  c("Open", "High", "Low", "Close", "Volume"), sep=".")
rm(BF.B, envir=sp500_env)
# Save OHLC prices to .RData file
save(sp500_env, file="C:/Develop/lecture_slides/data/sp500.RData")
plot(quantmod::Cl(sp500_env$MSFT))

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

library(rutils)  # Load package rutils
# Specify class for column "TICKER" so that "F" doesn't become FALSE
col_class <- "character"
names(col_class) <- "TICKER"
# Read .csv file with Ford OHLC prices
oh_lc <- read.csv(file="C:/Develop/lecture_slides/data/F_CRSP.csv",
  colClasses=col_class)
sym_bol <- oh_lc[1, "TICKER"]
# Adjustment factor
adj_fact <- drop(oh_lc[, "CFACPR"])
# Extract dates index
date_s <- drop(oh_lc[, "date"])
date_s <- lubridate::ymd(date_s)
# Select only OHLCV data
oh_lc <- oh_lc[, c("OPENPRC", "ASKHI", "BIDLO", "PRC", "VOL")]
colnames(oh_lc) <- paste(sym_bol, c("Open", "High", "Low", "Close", "Volume"), sep=".")
# Coerce to xts series
oh_lc <- xts::xts(oh_lc, date_s)
# Fill missing Open NA prices
is_na <- is.na(oh_lc[, 1])
oh_lc[is_na, 1] <- (oh_lc[is_na, 2] + oh_lc[is_na, 3])/2
# Adjust all the prices
oh_lc[, 1:4] <- oh_lc[, 1:4]/adj_fact/tr_fact[NROW(tr_fact)]
plot(quantmod::Cl(oh_lc), main="Ford Stock")

crsp_compustat <- read.csv(file="C:/Develop/lecture_slides/data/crsp_compustat_indices.csv")
colnames(crsp_compustat) <- crsp_compustat[1, ]
crsp_compustat <- crsp_compustat[-1, ]
print(xtable(crsp_compustat), comment=FALSE, size="tiny", include.rownames=FALSE)

library(rutils)  # Load package rutils
# Read .csv file with TAP OHLC prices
oh_lc <- read.csv(file="C:/Develop/lecture_slides/data/TAP.csv")
sym_bol <- oh_lc[1, "tic"]
# Adjustment factor and total return factor
adj_fact <- drop(oh_lc[, "ajexdi"])
tr_fact <- drop(oh_lc[, "trfd"])
# Extract dates index
date_s <- drop(oh_lc[, "datadate"])
date_s <- lubridate::ymd(date_s)
# Select only OHLCV data
oh_lc <- oh_lc[, c("prcod", "prchd", "prcld", "prccd", "cshtrd")]
colnames(oh_lc) <- paste(sym_bol, c("Open", "High", "Low", "Close", "Volume"), sep=".")
# Coerce to xts series
oh_lc <- xts::xts(oh_lc, date_s)
# Fill missing Open NA prices
is_na <- is.na(oh_lc[, 1])
oh_lc[is_na, 1] <- (oh_lc[is_na, 2] + oh_lc[is_na, 3])/2
sum(is.na(oh_lc))
# Adjust all the prices
oh_lc[, 1:4] <- tr_fact*oh_lc[, 1:4]/adj_fact/tr_fact[NROW(tr_fact)]
plot(quantmod::Cl(oh_lc), main="TAP Stock")

library(rutils)  # Load package rutils
# Download Fama-French factors from KFRENCH database
fac_tors <- Quandl(code="KFRENCH/FACTORS_D",
  start_date="2001-01-01", type="xts")
dim(fac_tors)
head(fac_tors)
tail(fac_tors)
chart_Series(cumsum(fac_tors["2001/", 1]/100),
  name="Fama-French factors")

options(width=200)
# Load package HighFreq
library(HighFreq)
# Or load the high frequency data file directly:
symbol_s <- load("C:/Develop/R/HighFreq/data/hf_data.RData")
head(SPY_TAQ)
head(SPY)
tail(SPY)

library(rutils)
# Read TAQ trade data from csv file
ta_q <- data.table::fread(file="C:/Develop/data/xlk_tick_trades_2020_03_16.csv")
# Inspect the TAQ data
ta_q
class(ta_q)
colnames(ta_q)
sapply(ta_q, class)
sym_bol <- ta_q$SYM_ROOT[1]
# Create date-time index
date_s <- paste(ta_q$DATE, ta_q$TIME_M)
# Coerce date-time index to POSIXlt
date_s <- strptime(date_s, "%Y%m%d %H:%M:%OS")
class(date_s)
# Display more significant digits
# options("digits")
options(digits=20, digits.secs=10)
last(date_s)
unclass(last(date_s))
as.numeric(last(date_s))
# Coerce date-time index to POSIXct
date_s <- as.POSIXct(date_s)
class(date_s)
last(date_s)
unclass(last(date_s))
as.numeric(last(date_s))
# Calculate the number of ticks per second
n_secs <- as.numeric(last(date_s)) - as.numeric(first(date_s))
NROW(ta_q)/(6.5*3600)
# Select TAQ data columns
ta_q <- ta_q[, .(price=PRICE, volume=SIZE)]
# Add date-time index
ta_q <- cbind(index=date_s, ta_q)

# Coerce trade ticks to xts series
x_ts <- xts::xts(ta_q[, .(price, volume)], ta_q$index)
colnames(x_ts) <- paste(sym_bol, c("Close", "Volume"), sep=".")
save(x_ts, file="C:/Develop/data/xlk_tick_trades_2020_03_16.RData")
# Plot dygraph
dygraphs::dygraph(x_ts$XLK.Close,
  main="XLK Trade Ticks for 2020-03-16")
# Plot in x11 window
x11(width=6, height=5)
quantmod::chart_Series(x=x_ts$XLK.Close,
  name="XLK Trade Ticks for 2020-03-16")

# Select the large lots greater than 100
dim(ta_q)
big_ticks <- ta_q[ta_q$volume > 100]
dim(big_ticks)
# Number of large lot ticks per second
NROW(big_ticks)/(6.5*3600)
# Save trade ticks with large lots
data.table::fwrite(big_ticks, file="C:/Develop/data/xlk_tick_trades_2020_03_16_biglots.csv")
# Coerce trade prices to xts
x_ts <- xts::xts(big_ticks[, .(price, volume)], big_ticks$index)
colnames(x_ts) <- c("XLK.Close", "XLK.Volume")

# Plot dygraph of the large lots
dygraphs::dygraph(x_ts$XLK.Close,
  main="XLK Trade Ticks for 2020-03-16 (large lots only)")
# Plot the large lots
x11(width=6, height=5)
quantmod::chart_Series(x=x_ts$XLK.Close,
  name="XLK Trade Ticks for 2020-03-16 (large lots only)")

# Apply centered Hampel filter to remove price jumps
win_dow <- 111
half_window <- win_dow %/% 2
medi_an <- TTR::runMedian(ta_q$price, n=win_dow)
medi_an <- rutils::lag_it(medi_an, lagg=-half_window, pad_zeros=FALSE)
ma_d <- TTR::runMAD(ta_q$price, n=win_dow)
ma_d <- rutils::lag_it(ma_d, lagg=-half_window, pad_zeros=FALSE)
ma_d[1:half_window] <- 1
ma_d[ma_d == 0] <- 1
# Calculate Z-scores
z_scores <- (ta_q$price - medi_an)/ma_d
z_scores[is.na(z_scores)] <- 0
z_scores[!is.finite(z_scores)] <- 0
sum(is.na(z_scores))
sum(!is.finite(z_scores))
range(z_scores)
mad(z_scores)
hist(z_scores, breaks=2000, xlim=c(-5*mad(z_scores), 5*mad(z_scores)))

# Remove price jumps with large z-scores
thresh_old <- 3
bad_ticks <- (abs(z_scores) > thresh_old)
good_ticks <- ta_q[!bad_ticks]
# Calculate number of price jumps
sum(bad_ticks)/NROW(z_scores)
# Coerce trade prices to xts
x_ts <- xts::xts(good_ticks[, .(price, volume)], good_ticks$index)
colnames(x_ts) <- c("XLK.Close", "XLK.Volume")
# Plot dygraph of the clean lots
dygraphs::dygraph(x_ts$XLK.Close,
  main="XLK Trade Ticks for 2020-03-16 (Hampel filtered)")
# Plot the large lots
x11(width=6, height=5)
quantmod::chart_Series(x=x_ts$XLK.Close,
  name="XLK Trade Ticks for 2020-03-16 (Hampel filtered)")

# Round time index to seconds
good_ticks[, index := as.POSIXct(round.POSIXt(index, "secs"))]
# Aggregate to OHLC by seconds
oh_lc <- good_ticks[, .(open=first(price), high=max(price), low=min(price), close=last(price), volume=sum(volume)), by=index]
# Round time index to minutes
good_ticks[, index := as.POSIXct(round.POSIXt(index, "mins"))]
# Aggregate to OHLC by minutes
oh_lc <- good_ticks[, .(open=first(price), high=max(price), low=min(price), close=last(price), volume=sum(volume)), by=index]

# Coerce OHLC prices to xts
x_ts <- xts::xts(oh_lc[, -"index"], oh_lc$index)
# Plot dygraph of the OHLC prices
dygraphs::dygraph(x_ts[, -5], main="XLK Trade Ticks for 2020-03-16 (OHLC)") %>%
  dyCandlestick()
# Plot the OHLC prices
x11(width=6, height=5)
quantmod::chart_Series(x=x_ts, TA="add_Vo()",
  name="XLK Trade Ticks for 2020-03-16 (OHLC)")

options(width=200)
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

# Load package HighFreq
library(HighFreq)
# Define sym_bol
sym_bol <- "SPY"
# Load OHLC data
output_dir <- "C:/Develop/data/hfreq/scrub/"
sym_bol <- load(
  file.path(output_dir,
      paste0(sym_bol, ".RData")))
inter_val <-
  "2013-11-11 09:30:00/2013-11-11 10:30:00"
chart_Series(SPY[inter_val], name=sym_bol)

# Install package HighFreq from github
devtools::install_github(repo="algoquant/HighFreq")
# Load package HighFreq
library(HighFreq)
# Get documentation for package HighFreq
# Get short description
packageDescription(HighFreq)
# Load help page
help(package=HighFreq)
# List all datasets in HighFreq
data(package=HighFreq)
# List all objects in HighFreq
ls("package:HighFreq")
# Remove HighFreq from search path
detach("package:HighFreq")

# install package HighFreq from github
install.packages("devtools")
library(devtools)
install_github(repo="algoquant/HighFreq")
# Load package HighFreq
library(HighFreq)
# set data directories
data_dir <- "C:/Develop/data/hfreq/src/"
output_dir <- "C:/Develop/data/hfreq/scrub/"
# Define sym_bol
sym_bol <- "SPY"
# Load a single day of TAQ data
sym_bol <- load(
  file.path(data_dir,
      paste0(sym_bol, "/2014.05.02.",
             sym_bol, ".RData")))
# scrub, aggregate single day of TAQ data to OHLC
ohlc_data <- scrub_agg(taq_data=get(sym_bol))
# Aggregate TAQ data for symbol, save to file
save_scrub_agg(sym_bol,
         data_dir=data_dir,
         output_dir=output_dir,
         period="minutes")

# Load package HighFreq
library(HighFreq)
# You can see SPY when listing objects in HighFreq
ls("package:HighFreq")
# You can see SPY when listing datasets in HighFreq
data(package=HighFreq)
# But the SPY dataset isn't listed in the workspace
ls()
# HighFreq datasets are lazy loaded and available when needed
head(SPY)
# Load all the datasets in package HighFreq
data(hf_data)
# HighFreq datasets are now loaded and in the workspace
head(SPY)

library(rutils)  # Load package rutils
# SPY percentage returns
oh_lc <- HighFreq::SPY
n_rows <- NROW(oh_lc)
clos_e <- log(quantmod::Cl(oh_lc))
re_turns <- rutils::diff_it(clos_e)
colnames(re_turns) <- "SPY"
# Standardize raw returns to make later comparisons
re_turns <- (re_turns - mean(re_turns))/sd(re_turns)
# Calculate moments and perform normality test
sapply(c(var=2, skew=3, kurt=4),
  function(x) sum(re_turns^x)/n_rows)
tseries::jarque.bera.test(re_turns)
# Fit SPY returns using MASS::fitdistr()
optim_fit <- MASS::fitdistr(re_turns, densfun="t", df=2)
lo_cation <- optim_fit$estimate[1]
scal_e <- optim_fit$estimate[2]

x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(1, 1, 1, 1))
# Plot histogram of SPY returns
histo_gram <- hist(re_turns, col="lightgrey", mgp=c(2, 1, 0),
  xlab="returns (standardized)", ylab="frequency", xlim=c(-3, 3),
  breaks=1e3, freq=FALSE, main="Distribution of High Frequency SPY Returns")
# lines(density(re_turns, bw=0.2), lwd=3, col="blue")
# Plot t-distribution function
curve(expr=dt((x-lo_cation)/scal_e, df=2)/scal_e,
type="l", lwd=3, col="red", add=TRUE)
# Plot the Normal probability distribution
curve(expr=dnorm(x, mean=mean(re_turns),
  sd=sd(re_turns)), add=TRUE, lwd=3, col="blue")
# Add legend
legend("topright", inset=0.05, bty="n",
  leg=c("t-distr", "normal"),
  lwd=6, lty=1, col=c("red", "blue"))

# Hourly SPY percentage returns
clos_e <- log(Cl(xts::to.period(x=oh_lc, period="hours")))
hour_ly <- rutils::diff_it(clos_e)
hour_ly <- (hour_ly - mean(hour_ly))/sd(hour_ly)
# Daily SPY percentage returns
clos_e <- log(Cl(xts::to.period(x=oh_lc, period="days")))
dai_ly <- rutils::diff_it(clos_e)
dai_ly <- (dai_ly - mean(dai_ly))/sd(dai_ly)
# Calculate moments
sapply(list(minutely=re_turns, hourly=hour_ly, daily=dai_ly),
 function(rets) {
   sapply(c(var=2, skew=3, kurt=4),
          function(x) mean(rets^x))
})  # end sapply

x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(1, 1, 1, 1))
# Plot densities of SPY returns
plot(density(re_turns, bw=0.4), xlim=c(-3, 3),
     lwd=3, mgp=c(2, 1, 0), col="blue",
     xlab="returns (standardized)", ylab="frequency",
     main="Density of High Frequency SPY Returns")
lines(density(hour_ly, bw=0.4), lwd=3, col="green")
lines(density(dai_ly, bw=0.4), lwd=3, col="red")
# Add legend
legend("topright", inset=0.05, bty="n",
  leg=c("minutely", "hourly", "daily"),
  lwd=6, lty=1, col=c("blue", "green", "red"))

# Calculate rolling volatility of SPY returns
ret_2013 <- re_turns["2013-11-11/2013-11-15"]
# Calculate rolling volatility
look_back <- 11
end_p <- seq_along(ret_2013)
start_p <- c(rep_len(1, look_back-1),
  end_p[1:(NROW(end_p)-look_back+1)])
end_p[end_p < look_back] <- look_back
vol_rolling <- sapply(seq_along(end_p),
  function(it) sd(ret_2013[start_p[it]:end_p[it]]))
vol_rolling <- xts::xts(vol_rolling, index(ret_2013))
# Extract time intervals of SPY returns
in_dex <- c(60, diff(xts::.index(ret_2013)))
head(in_dex)
table(in_dex)
# Scale SPY returns by time intervals
ret_2013 <- 60*ret_2013/in_dex
# Calculate scaled rolling volatility
vol_scaled <- sapply(seq_along(end_p),
  function(it) sd(ret_2013[start_p[it]:end_p[it]]))
vol_rolling <- cbind(vol_rolling, vol_scaled)
vol_rolling <- na.omit(vol_rolling)
sum(is.na(vol_rolling))
sapply(vol_rolling, range)

# Plot rolling volatility
x11(width=6, height=5)
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("blue", "red")
chart_Series(vol_rolling, theme=plot_theme,
     name="Rolling Volatility with Overnight Spikes")
legend("topright", legend=colnames(vol_rolling),
  inset=0.1, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")

price_s <- read.zoo(file="C:/Develop/lecture_slides/data/bid_ask_bounce.csv",
  header=TRUE, sep=",")
price_s <- as.xts(price_s)
x11(width=6, height=4)
par(mar=c(2, 2, 0, 0), oma=c(1, 1, 0, 0))
chart_Series(x=price_s, name="S&P500 Futures Bid-Ask Bounce")

# Volatility of SPY
sqrt(HighFreq::calc_var_ohlc(oh_lc))
# Daily SPY volatility and volume
vol_daily <- sqrt(xts::apply.daily(oh_lc, FUN=calc_var_ohlc))
colnames(vol_daily) <- ("SPY_volatility")
vol_ume <- quantmod::Vo(oh_lc)
volume_daily <- xts::apply.daily(vol_ume, FUN=sum)
colnames(volume_daily) <- ("SPY_volume")
# Plot SPY volatility and volume
da_ta <- cbind(vol_daily, volume_daily)["2008/2009"]
col_names <- colnames(da_ta)
dygraphs::dygraph(da_ta,
  main="SPY Daily Volatility and Trading Volume") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="red", strokeWidth=3) %>%
  dySeries(name=col_names[2], axis="y2", col="blue", strokeWidth=3)

# Regress log of daily volume vs volatility
da_ta <- log(cbind(volume_daily, vol_daily))
col_names <- colnames(da_ta)
data_frame <- as.data.frame(da_ta)
for_mula <- as.formula(paste(col_names, collapse="~"))
mod_el <- lm(for_mula, data=data_frame)
# Durbin-Watson test for autocorrelation of residuals
lmtest::dwtest(mod_el)
# Regress diff log of daily volume vs volatility
data_frame <- as.data.frame(rutils::diff_it(da_ta))
mod_el <- lm(for_mula, data=data_frame)
lmtest::dwtest(mod_el)
summary(mod_el)
plot(for_mula, data=data_frame, main="SPY Daily Trading Volume vs Volatility (log scale)")
abline(mod_el, lwd=3, col="red")
mtext(paste("beta =", round(coef(mod_el)[2], 3)), cex=1.2, lwd=3, side=2, las=2, adj=(-0.5), padj=(-7))

# 60 minutes of data in look_back interval
look_back <- 60
vol_2013 <- vol_ume["2013"]
ret_2013 <- re_turns["2013"]
# Define end points with beginning stub
n_rows <- NROW(ret_2013)
n_agg <- n_rows %/% look_back
end_p <- n_rows-look_back*n_agg + (0:n_agg)*look_back
start_p <- c(1, end_p[1:(NROW(end_p)-1)])
# Calculate SPY volatility and volume
da_ta <- sapply(seq_along(end_p), function(it) {
  point_s <- start_p[it]:end_p[it]
  c(volume=sum(vol_2013[point_s]),
    volatility=sd(ret_2013[point_s]))
})  # end sapply
da_ta <- t(da_ta)
da_ta <- rutils::diff_it(log(da_ta))
data_frame <- as.data.frame(da_ta)

for_mula <- as.formula(paste(colnames(da_ta), collapse="~"))
mod_el <- lm(for_mula, data=data_frame)
lmtest::dwtest(mod_el)
summary(mod_el)
plot(for_mula, data=data_frame,
     main="SPY Hourly Trading Volume vs Volatility (log scale)")
abline(mod_el, lwd=3, col="red")
mtext(paste("beta =", round(coef(mod_el)[2], 3)), cex=1.2, lwd=3, side=2, las=2, adj=(-0.5), padj=(-7))

# Scale returns using volume (volume clock)
rets_scaled <- ifelse(vol_ume > 1e4, re_turns/vol_ume, 0)
rets_scaled <- rets_scaled/sd(rets_scaled)
# Calculate moments of scaled returns
n_rows <- NROW(re_turns)
sapply(list(re_turns=re_turns, rets_scaled=rets_scaled),
  function(rets) {sapply(c(skew=3, kurt=4),
     function(x) sum((rets/sd(rets))^x)/n_rows)
})  # end sapply

x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(1, 1, 1, 1))
# Plot densities of SPY returns
plot(density(re_turns), xlim=c(-3, 3),
     lwd=3, mgp=c(2, 1, 0), col="blue",
     xlab="returns (standardized)", ylab="frequency",
     main="Density of Volume-scaled High Frequency SPY Returns")
lines(density(rets_scaled, bw=0.4), lwd=3, col="red")
curve(expr=dnorm, add=TRUE, lwd=3, col="green")
# Add legend
legend("topright", inset=0.05, bty="n",
  leg=c("minutely", "scaled", "normal"),
  lwd=6, lty=1, col=c("blue", "red", "green"))

# Ljung-Box test for minutely SPY returns
Box.test(re_turns, lag=10, type="Ljung")
# Ljung-Box test for daily SPY returns
Box.test(dai_ly, lag=10, type="Ljung")
# Ljung-Box test statistics for scaled SPY returns
sapply(list(re_turns=re_turns, rets_scaled=rets_scaled),
  function(rets) {
    Box.test(rets, lag=10, type="Ljung")$statistic
})  # end sapply
# Ljung-Box test statistics for aggregated SPY returns
sapply(list(minutely=re_turns, hourly=hour_ly, daily=dai_ly),
  function(rets) {
    Box.test(rets, lag=10, type="Ljung")$statistic
})  # end sapply

# Set plot parameters
x11(width=6, height=8)
par(mar=c(4, 4, 2, 1), oma=c(0, 0, 0, 0))
layout(matrix(c(1, 2), ncol=1), widths=c(6, 6), heights=c(4, 4))
# Plot the partial autocorrelations of minutely SPY returns
pa_cf <- pacf(as.numeric(re_turns), lag=10,
     xlab="lag", ylab="partial autocorrelation", main="")
title("Partial Autocorrelations of Minutely SPY Returns", line=1)
# Plot the partial autocorrelations of scaled SPY returns
pacf_scaled <- pacf(as.numeric(rets_scaled), lag=10,
     xlab="lag", ylab="partial autocorrelation", main="")
title("Partial Autocorrelations of Scaled SPY Returns", line=1)
# Calculate the sums of partial autocorrelations
sum(pa_cf$acf)
sum(pacf_scaled$acf)

# Calculate market illiquidity
liquidi_ty <- sqrt(volume_daily)/vol_daily
# Plot market illiquidity
x11(width=6, height=7) ; par(mfrow=c(2, 1))
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("blue")
chart_Series(liquidi_ty["2010"], theme=plot_theme,
  name="SPY Liquidity in 2010", plot=FALSE)
plot_theme$col$line.col <- c("red")
chart_Series(vol_daily["2010"],
  theme=plot_theme, name="SPY Volatility in 2010")

# Calculate intraday time index with hours and minutes
date_s <- format(zoo::index(re_turns), "%H:%M")
# Aggregate the mean volume
volume_agg <- tapply(X=vol_ume, INDEX=date_s, FUN=mean)
volume_agg <- drop(volume_agg)
# Aggregate the mean volatility
vol_agg <- tapply(X=re_turns^2, INDEX=date_s, FUN=mean)
vol_agg <- sqrt(drop(vol_agg))
# Coerce to xts
intra_day <- as.POSIXct(paste(Sys.Date(), names(volume_agg)))
volume_agg <- xts::xts(volume_agg, intra_day)
vol_agg <- xts::xts(vol_agg, intra_day)
# Plot seasonality of volume and volatility
x11(width=6, height=7) ; par(mfrow=c(2, 1))
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("blue")
chart_Series(volume_agg[c(-1, -NROW(volume_agg))], theme=plot_theme,
  name="Daily Seasonality of SPY Volume", plot=FALSE)
plot_theme$col$line.col <- c("red")
chart_Series(vol_agg[c(-1, -NROW(vol_agg))], theme=plot_theme,
  name="Daily Seasonality of SPY Volatility")

# Calculate market liquidity
liquidi_ty <- sqrt(volume_agg)/vol_agg
# Plot daily seasonality of market liquidity
x11(width=6, height=7) ; par(mfrow=c(2, 1))
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("blue")
chart_Series(liquidi_ty[c(-1, -NROW(liquidi_ty))], theme=plot_theme,
  name="Daily Seasonality of SPY Liquidity", plot=FALSE)
plot_theme$col$line.col <- c("red")
chart_Series(vol_agg[c(-1, -NROW(vol_agg))], theme=plot_theme,
  name="Daily Seasonality of SPY Volatility")

par(mfrow=c(2,1))  # set plot panels
library(rutils)  # Load package rutils
chart_Series(roll_sum(vol_daily, 10)[-(1:10)]/10,
       name=paste(sym_bol, "variance"))
chart_Series(roll_sum(hurst_daily, 10)[-(1:10)]/10,
       name=paste(sym_bol, "Hurst"))
abline(h=0.5, col="blue", lwd=2)

par(mfrow=c(2,1))  # set plot panels
library(rutils)  # Load package rutils
# Daily seasonality of Hurst exponent
inter_val <- "2013"
season_hurst <- season_ality(hurst_ohlc(ohlc=SPY[inter_val, 1:4]))
season_hurst <- season_hurst[-(nrow(season_hurst))]
colnames(season_hurst) <- paste0(col_name(get(sym_bol)), ".season_hurst")
plot_theme <- chart_theme()
plot_theme$format.labels <- "%H:%M"
ch_ob <- chart_Series(x=season_hurst,
  name=paste(colnames(season_hurst),
  "daily seasonality"), theme=plot_theme,
  plot=FALSE)
y_lim <- ch_ob$get_ylim()
y_lim[[2]] <- structure(c(y_lim[[2]][1],
        y_lim[[2]][2]), fixed=TRUE)
ch_ob$set_ylim(y_lim)
plot(ch_ob)
abline(h=0.5, col="blue", lwd=2)
# Daily seasonality of volatility
season_var <- season_ality(vol_ohlc(ohlc=SPY))

par(mfrow=c(2,1))  # set plot panels
library(rutils)  # Load package rutils
# Rolling variance
var_iance <-
  roll_agg_ohlc(ohlc=SPY, agg_fun="vol_ohlc")
# Rolling skew
sk_ew <-
  roll_agg_ohlc(ohlc=SPY, agg_fun="skew_ohlc")
sk_ew <- sk_ew/(var_iance)^(1.5)
sk_ew[1, ] <- 0
sk_ew <- na.locf(sk_ew)
inter_val <- "2013-11-11/2013-11-15"
chart_Series(var_iance[inter_val],
      name=paste(sym_bol, "variance"))
chart_Series(sk_ew[inter_val],
      name=paste(sym_bol, "Skew"),
      ylim=c(-1, 1))

par(mfrow=c(2,1))  # set plot panels
library(rutils)  # Load package rutils
# Daily variance and skew
vol_daily <- xts::apply.daily(x=HighFreq::SPY, FUN=agg_ohlc,
                  agg_fun="vol_ohlc")
colnames(vol_daily) <- paste0(sym_bol, ".var")
daily_skew <- xts::apply.daily(x=HighFreq::SPY, FUN=agg_ohlc,
                  agg_fun="skew_ohlc")
daily_skew <- daily_skew/(vol_daily)^(1.5)
colnames(daily_skew) <- paste0(sym_bol, ".skew")
inter_val <- "2013-06-01/"
chart_Series(vol_daily[inter_val],
       name=paste(sym_bol, "variance"))
chart_Series(daily_skew[inter_val],
       name=paste(sym_bol, "skew"))

# skew scatterplot
re_turns <- calc_rets(xts_data=SPY)
sk_ew <- skew_ohlc(log_ohlc=log(SPY[, -5]))
colnames(sk_ew) <- paste0(sym_bol, ".skew")
lag_skew <- lag(sk_ew)
lag_skew[1, ] <- 0
da_ta <- cbind(re_turns[, 1], sign(lag_skew))
for_mula <- as.formula(paste(colnames(da_ta)[1],
    paste(paste(colnames(da_ta)[-1],
      collapse=" + "), "- 1"), sep="~"))
for_mula
mod_el <- lm(for_mula, data=da_ta)
summary(mod_el)$coef
summary(lm(for_mula, data=da_ta["/2011-01-01"]))$coef
summary(lm(for_mula, data=da_ta["2011-01-01/"]))$coef

inter_val <- "2013-12-01/"
plot(for_mula, data=da_ta[inter_val],
     xlim=c(-2e-09, 2e-09),
     cex=0.6, xlab="skew", ylab="rets")
abline(mod_el, col="blue", lwd=2)

# Contrarian skew trading strategy
# Lag the skew to get positions
position_s <- -sign(lag_skew)
position_s[1, ] <- 0
# Cumulative PnL
cumu_pnl <- cumsum(position_s*re_turns[, 1])
# Calculate frequency of trades
50*sum(abs(sign(sk_ew)-sign(lag_skew)))/nrow(sk_ew)
# Calculate transaction costs
bid_offer <- 0.001  # 10 bps for liquid ETFs
bid_offer*sum(abs(sign(sk_ew)-sign(lag_skew)))

chart_Series(
  cumu_pnl[endpoints(cumu_pnl, on="hours"), ],
  name=paste(sym_bol, "contrarian skew strategy pnl"))

# vwap plot
vwap_short <- v_wap(x_ts=SPY, win_dow=70)
vwap_long <- v_wap(x_ts=SPY, win_dow=225)
vwap_diff <- vwap_short - vwap_long
colnames(vwap_diff) <- paste0(sym_bol, ".vwap")
inter_val <- "2010-05-05/2010-05-07"
invisible(chart_Series(x=Cl(SPY[inter_val]),
         name=paste(sym_bol, "plus VWAP")))
invisible(add_TA(vwap_short[inter_val],
   on=1, col="red", lwd=2))
invisible(add_TA(vwap_long[inter_val],
   on=1, col="blue", lwd=2))
invisible(add_TA(vwap_diff[inter_val] > 0, on=-1,
   col="lightgreen", border="lightgreen"))
add_TA(vwap_diff[inter_val] < 0, on=-1,
 col="lightgrey", border="lightgrey")

# vwap scatterplot
# re_turns <- calc_rets(xts_data=SPY)
vwap_short <- v_wap(x_ts=SPY, win_dow=70)
vwap_long <- v_wap(x_ts=SPY, win_dow=225)
vwap_diff <- vwap_short - vwap_long
colnames(vwap_diff) <- paste0(sym_bol, ".vwap")
lag_vwap <- lag(vwap_diff)
lag_vwap[1, ] <- 0
da_ta <- cbind(re_turns[, 1], sign(lag_vwap))
for_mula <- as.formula(paste(colnames(da_ta)[1],
    paste(paste(colnames(da_ta)[-1],
      collapse=" + "), "- 1"), sep="~"))
for_mula
mod_el <- lm(for_mula, data=da_ta)
summary(mod_el)$coef
summary(lm(for_mula, data=da_ta["/2011-01-01"]))$coef
summary(lm(for_mula, data=da_ta["2011-01-01/"]))$coef

inter_val <- "2013-12-01/"
plot(for_mula, data=cbind(re_turns[, 1], lag_vwap)[inter_val],
     cex=0.6, xlab="skew", ylab="rets")
abline(mod_el, col="blue", lwd=2)

# Trend following trading strategy
# Cumulative PnL
cumu_pnl <- cumsum(sign(lag_vwap)*re_turns[, 1])
# Calculate frequency of trades
50*sum(abs(sign(vwap_diff)-sign(lag_vwap)))/nrow(vwap_diff)
# Calculate transaction costs
bid_offer <- 0.001  # 10 bps for liquid ETFs
bid_offer*sum(abs(sign(vwap_diff)-sign(lag_vwap)))

chart_Series(
  cumu_pnl[endpoints(cumu_pnl, on="hours"), ],
  name=paste(sym_bol, "VWAP Trend Following Strategy PnL"))

library(rutils)  # Load package rutils
# Daily Hurst exponents
hurst_daily <- xts::apply.daily(x=HighFreq::SPY,
                     FUN=agg_ohlc,
                     agg_fun="hurst_ohlc")
colnames(hurst_daily) <-
  paste(col_name(get(sym_bol)), ".Hurst")
chart_Series(roll_sum(hurst_daily, 10)[-(1:10)]/10,
       name=paste(sym_bol, "Hurst"))
abline(h=0.5, col="blue", lwd=2)

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
