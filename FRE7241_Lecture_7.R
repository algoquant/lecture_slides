# Perform regression using formula
retp <- na.omit(rutils::etfenv$returns[, c("XLP", "VTI")])
raterf <- 0.03/252
retp <- (retp - raterf)
regmod <- lm(XLP ~ VTI, data=retp)
regsum <- summary(regmod)
# Get regression coefficients
coef(regsum)
# Get alpha and beta
coef(regsum)[, 1]

# Plot scatterplot of returns with aspect ratio 1
plot(XLP ~ VTI, data=rutils::etfenv$returns, main="Regression XLP ~ VTI",
     xlim=c(-0.1, 0.1), ylim=c(-0.1, 0.1), pch=1, col="blue", asp=1)
# Add regression line and perpendicular line
abline(regmod, lwd=2, col="red")
abline(a=0, b=-1/coef(regsum)[2, 1], lwd=2, col="blue")

# Get regression coefficients
coef(regsum)
# Calculate regression coefficients from scratch
betac <- drop(cov(retp$XLP, retp$VTI)/var(retp$VTI))
alphac <- drop(mean(retp$XLP) - betac*mean(retp$VTI))
c(alphac, betac)
# Calculate the residuals
residuals <- (retp$XLP - (alphac + betac*retp$VTI))
# Calculate the standard deviation of residuals
nrows <- NROW(residuals)
residsd <- sqrt(sum(residuals^2)/(nrows - 2))
Calculate the standard errors of beta and alpha
sum2 <- sum((retp$VTI - mean(retp$VTI))^2)
betasd <- residsd/sqrt(sum2)
alphasd <- residsd*sqrt(1/nrows + mean(retp$VTI)^2/sum2)
c(alphasd, betasd)
Perform the Durbin-Watson test of autocorrelation of residuals
lmtest::dwtest(regmod)

retp <- rutils::etfenv$returns
symbolv <- colnames(retp)
symbolv <- symbolv[symbolv != "VTI"]
Perform regressions and collect statistics
betam <- sapply(symbolv, function(symbol) {
Specify regression formula
  formulav <- as.formula(paste(symbol, "~ VTI"))
Perform regression
  regmod <- lm(formulav, data=retp)
Get regression summary
  regsum <- summary(regmod)
Collect regression statistics
  with(regsum, 
    c(beta=coefficients[2, 1], 
pbeta=coefficients[2, 4],
alpha=coefficients[1, 1], 
palpha=coefficients[1, 4], 
pdw=lmtest::dwtest(regmod)$p.value))
})  # end sapply
betam <- t(betam)
Sort by palpha
betam <- betam[order(betam[, "palpha"]), ]

betam

library(PerformanceAnalytics)
Calculate XLP beta
PerformanceAnalytics::CAPM.beta(Ra=retp$XLP, Rb=retp$VTI)
Or
retxlp <- na.omit(retp[, c("XLP", "VTI")])
betac <- drop(cov(retxlp$XLP, retxlp$VTI)/var(retxlp$VTI))
betac
Calculate XLP alpha
PerformanceAnalytics::CAPM.alpha(Ra=retp$XLP, Rb=retp$VTI)
Or
mean(retp$XLP - betac*retp$VTI)
Calculate XLP bull beta
PerformanceAnalytics::CAPM.beta.bull(Ra=retp$XLP, Rb=retp$VTI)
Calculate XLP bear beta
PerformanceAnalytics::CAPM.beta.bear(Ra=retp$XLP, Rb=retp$VTI)

symbolv <- rownames(betam)
betac <- betam[-match(c("VXX", "SVXY", "MTUM", "USMV", "QUAL"), symbolv), 1]
betac <- c(1, betac)
names(betac)[1] <- "VTI"
retsann <- sapply(retp[, names(betac)], PerformanceAnalytics::Return.annualized)
# Plot scatterplot of returns vs betas
minrets <- min(retsann)
plot(retsann ~ betac, xlab="betas", ylab="returns",
     ylim=c(minrets, -minrets), main="Security Market Line for ETFs")
retvti <- retsann["VTI"]
points(x=1, y=retvti, col="red", lwd=3, pch=21)
# Plot Security Market Line
raterf <- 0.01
abline(a=raterf, b=(retvti-raterf), col="green", lwd=2)

# Add labels
text(x=betac, y=retsann, labels=names(betac), pos=2, cex=0.8)
# Find optimal risk-free rate by minimizing residuals
rss <- function(raterf) {
  sum((retsann - raterf - betac*(retvti-raterf))^2)
}  # end rss
optimrss <- optimize(rss, c(-1, 1))
raterf <- optimrss$minimum
# Or simply
retsadj <- (retsann - retvti*betac)
betadj <- (1-betac)
raterf <- sum(retsadj*betadj)/sum(betadj^2)
abline(a=raterf, b=(retvti-raterf), col="blue", lwd=2)
legend(x="topleft", bty="n", title="Security Market Line",
 legend=c("optimal fit", "raterf=0.01"),
 y.intersp=0.5, cex=1.0, lwd=6, lty=1, col=c("blue", "green"))

# Load S&P500 constituent stock returns
load("/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
retvti <- na.omit(rutils::etfenv$returns$VTI)
retp <- retstock[index(retvti), ]
nrows <- NROW(retp)
# Calculate stock betas
betac <- sapply(retp, function(x) {
  retp <- na.omit(cbind(x, retvti))
  drop(cov(retp[, 1], retp[, 2])/var(retp[, 2]))
})  # end sapply
mean(betac)
# Calculate annual stock returns
retsann <- retp
retsann[1, ] <- 0
retsann <- zoo::na.locf(retsann, na.rm=FALSE)
retsann <- 252*sapply(retsann, sum)/nrows
# Remove stocks with zero returns
sum(retsann == 0)
betac <- betac[retsann > 0]
retsann <- retsann[retsann > 0]
retvti <- 252*mean(retvti)
# Plot scatterplot of returns vs betas
plot(retsann ~ betac, xlab="betas", ylab="returns",
     main="Security Market Line for Stocks")
points(x=1, y=retvti, col="red", lwd=3, pch=21)
# Plot Security Market Line
raterf <- 0.01
abline(a=raterf, b=(retvti-raterf), col="green", lwd=2)

# Find optimal risk-free rate by minimizing residuals
retsadj <- (retsann - retvti*betac)
betadj <- (1-betac)
raterf <- sum(retsadj*betadj)/sum(betadj^2)
abline(a=raterf, b=(retvti-raterf), col="blue", lwd=2)
legend(x="topleft", bty="n", title="Security Market Line",
 legend=c("optimal fit", "raterf=0.01"),
 y.intersp=0.5, cex=1.0, lwd=6, lty=1, col=c("blue", "green"))

library(PerformanceAnalytics)
Calculate XLP Treynor ratio
TreynorRatio(Ra=retp$XLP, Rb=retp$VTI)
Calculate XLP Information ratio
InformationRatio(Ra=retp$XLP, Rb=retp$VTI)

PerformanceAnalytics::table.CAPM(Ra=retp[, c("XLP", "XLF")], 
                           Rb=retp$VTI, scale=252)

capmstats <- table.CAPM(Ra=retp[, symbolv],
        Rb=retp$VTI, scale=252)
colv <- strsplit(colnames(capmstats), split=" ")
colv <- do.call(cbind, colv)[1, ]
colnames(capmstats) <- colv
capmstats <- t(capmstats)
capmstats <- capmstats[, -1]
colv <- colnames(capmstats)
whichv <- match(c("Annualized Alpha", "Information Ratio", "Treynor Ratio"), colv)
colv[whichv] <- c("Alpha", "Information", "Treynor")
colnames(capmstats) <- colv
capmstats <- capmstats[order(capmstats[, "Alpha"], decreasing=TRUE), ]
# Copy capmstats into etfenv and save to .RData file
etfenv <- rutils::etfenv
etfenv$capmstats <- capmstats
save(etfenv, file="/Users/jerzy/Develop/lecture_slides/data/etf_data.RData")

rutils::etfenv$capmstats[, c("Beta", "Alpha", "Information", "Treynor")]

# Calculate XLP and VTI returns
retp <- na.omit(rutils::etfenv$returns[, c("XLP", "VTI")])
# Calculate monthly end points
endd <- xts::endpoints(retp, on="months")[-1]
# Calculate start points from look-back interval
lookb <- 12  # Look back 12 months
startp <- c(rep(1, lookb), endd[1:(NROW(endd)-lookb)])
head(cbind(endd, startp), lookb+2)
# Calculate trailing beta regressions every month in R
formulav <- XLP ~ VTI  # Specify regression formula
betar <- sapply(1:NROW(endd), FUN=function(tday) {
    datav <- retp[startp[tday]:endd[tday], ]
    # coef(lm(formulav, data=datav))[2]
    drop(cov(datav$XLP, datav$VTI)/var(datav$VTI))
  })  # end sapply
# Calculate trailing betas using RcppArmadillo
controll <- HighFreq::param_reg()
reg_stats <- HighFreq::roll_reg(respv=retp$XLP, predm=retp$VTI,
  startp=(startp-1), endp=(endd-1), controll=controll)
betac <- reg_stats[, 2]
all.equal(betac, betar)
# Compare the speed of RcppArmadillo with R code
library(microbenchmark)
summary(microbenchmark(
  Rcpp=HighFreq::roll_reg(respv=retp$XLP, predm=retp$VTI, startp=(startp-1), endp=(endd-1), controll=controll),
  Rcode=sapply(1:NROW(endd), FUN=function(tday) {
    datav <- retp[startp[tday]:endd[tday], ]
    drop(cov(datav$XLP, datav$VTI)/var(datav$VTI))
  }),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# dygraph plot of trailing XLP beta and VTI prices
datev <- zoo::index(retp[endd, ])
pricev <- rutils::etfenv$prices$VTI[datev]
datav <- cbind(pricev, betac)
colnames(datav)[2] <- "beta"
colv <- colnames(datav)
dygraphs::dygraph(datav, main="XLP Trailing 12-month Beta and VTI Prices") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", col="blue") %>%
  dySeries(name=colv[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)

# Calculate XLP and VTI returns
retp <- na.omit(rutils::etfenv$returns[, c("XLP", "VTI")])
# Calculate monthly end points
endd <- rutils::calc_endpoints(retp, interval="months")[-1]
# Calculate start points from look-back interval
lookb <- 12  # Look back 12 months
startp <- c(rep(1, lookb), endd[1:(NROW(endd)-lookb)])
head(cbind(endd, startp), lookb+2)
# Calculate trailing beta regressions every month in R
formulav <- XLP ~ VTI  # Specify regression formula
betar <- sapply(1:NROW(endd), FUN=function(tday) {
    datav <- retp[startp[tday]:endd[tday], ]
    # coef(lm(formulav, data=datav))[2]
    drop(cov(datav$XLP, datav$VTI)/var(datav$VTI))
  })  # end sapply
# Calculate trailing betas using RcppArmadillo
controll <- HighFreq::param_reg()
reg_stats <- HighFreq::roll_reg(respv=retp$XLP, predm=retp$VTI,
  startp=(startp-1), endp=(endd-1), controll=controll)
betac <- reg_stats[, 2]
all.equal(betac, betar)
# Compare the speed of RcppArmadillo with R code
library(microbenchmark)
summary(microbenchmark(
  Rcpp=HighFreq::roll_reg(respv=retp$XLP, predm=retp$VTI, startp=(startp-1), endp=(endd-1), controll=controll),
  Rcode=sapply(1:NROW(endd), FUN=function(tday) {
    datav <- retp[startp[tday]:endd[tday], ]
    drop(cov(datav$XLP, datav$VTI)/var(datav$VTI))
  }),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# dygraph plot of trailing XLP beta and VTI prices
datev <- zoo::index(retp[endd, ])
pricev <- log(rutils::etfenv$prices$VTI[datev])
datav <- cbind(pricev, betac)
colnames(datav)[2] <- "beta"
colv <- colnames(datav)
dygraphs::dygraph(datav, main="XLP Trailing 12-month Beta and VTI Prices") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=colv[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)

# Calculate the trailing betas
lambdaf <- 0.99
covarv <- HighFreq::run_covar(retp, lambdaf)
betac <- covarv[, 1]/covarv[, 3]

# dygraph plot of trailing XLP beta and VTI prices
datav <- cbind(pricev, betac[endd])[-(1:11)] # Remove warmup period
colnames(datav)[2] <- "beta"
colv <- colnames(datav)
dygraphs::dygraph(datav, main="XLP Trailing 12-month Beta and VTI Prices") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=colv[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)

# Load S&P500 constituent stock prices
load("/Users/jerzy/Develop/lecture_slides/data/sp500_prices.RData")
# Calculate stock prices and percentage returns
pricets <- zoo::na.locf(pricets, na.rm=FALSE)
pricets <- zoo::na.locf(pricets, fromLast=TRUE)
retp <- rutils::diffit(log(pricev))
# Standardize (center and scale) the returns
retp <- lapply(retp, function(x) {(x - mean(x))/sd(x)})
retp <- rutils::do_call(cbind, retp)
# Perform principal component analysis PCA
pcad <- prcomp(retp, scale=TRUE)
# Find number of components with variance greater than 2
ncomp <- which(pcad$sdev^2 < 2)[1]

# Plot standard deviations of principal components
barplot(pcad$sdev[1:ncomp],
  names.arg=colnames(pcad$rotation[, 1:ncomp]),
  las=3, xlab="", ylab="",
  main="Volatilities of S&P500 Principal Components")

# Calculate principal component loadings (weights)
# Plot barplots with PCA weights in multiple panels
ncomps <- 6
par(mfrow=c(ncomps/2, 2))
par(mar=c(4, 2, 2, 1), oma=c(0, 0, 0, 0))
# First principal component weights
weightv <- sort(pcad$rotation[, 1], decreasing=TRUE)
barplot(weightv[1:6], las=3, xlab="", ylab="", main="")
title(paste0("PC", 1), line=-2.0, col.main="red")
for (ordern in 2:ncomps) {
  weightv <- sort(pcad$rotation[, ordern], decreasing=TRUE)
  barplot(weightv[c(1:3, 498:500)], las=3, xlab="", ylab="", main="")
  title(paste0("PC", ordern), line=-2.0, col.main="red")
}  # end for

# Calculate principal component time series
retpca <- xts(retp %*% pcad$rotation[, 1:ncomps], order.by=datev)
round(cov(retpca), 3)
retpcac <- cumsum(retpca)
# Plot principal component time series in multiple panels
par(mfrow=c(ncomps/2, 2))
par(mar=c(2, 2, 0, 1), oma=c(0, 0, 0, 0))
rangev <- range(retpcac)
for (ordern in 1:ncomps) {
  plot.zoo(retpcac[, ordern], ylim=rangev, xlab="", ylab="")
  title(paste0("PC", ordern), line=-2.0)
}  # end for

par(mfrow=c(ncomps/2, 2))
par(mar=c(2, 2, 0, 1), oma=c(0, 0, 0, 0))
# Invert principal component time series
pcinv <- solve(pcad$rotation)
all.equal(pcinv, t(pcad$rotation))
solved <- retpca %*% pcinv[1:ncomps, ]
solved <- xts::xts(solved, datev)
solved <- cumsum(solved)
retc <- cumsum(retp)
# Plot the solved returns
symbolv <- c("MSFT", "XOM", "JPM", "AAPL", "BRK_B", "JNJ")
for (symbol in symbolv) {
  plot.zoo(cbind(retc[, symbol], solved[, symbol]),
    plot.type="single", col=c("black", "blue"), xlab="", ylab="")
  legend(x="topleft", bty="n",
   legend=paste0(symbol, c("", " solved")),
   title=NULL, inset=0.05, cex=1.0, lwd=6,
   lty=1, col=c("black", "blue"))
}  # end for

par(mfrow=c(ncomps/2, 2))
par(mar=c(2, 2, 0, 1), oma=c(0, 0, 0, 0))
# Perform ADF unit root tests on original series and residuals
sapply(symbolv, function(symbol) {
  c(series=tseries::adf.test(retc[, symbol])$p.value,
    resid=tseries::adf.test(retc[, symbol] - solved[, symbol])$p.value)
})  # end sapply
# Plot the residuals
for (symbol in symbolv) {
  plot.zoo(retc[, symbol] - solved[, symbol],
    plot.type="single", col="blue", xlab="", ylab="")
  legend(x="topleft", bty="n", legend=paste(symbol, "residuals"),
   title=NULL, inset=0.05, cex=1.0, lwd=6, lty=1, col="blue")
}  # end for
# Perform ADF unit root test on principal component time series
retpca <- xts(retp %*% pcad$rotation, order.by=datev)
retpcac <- cumsum(retpca)
adf_pvalues <- sapply(1:NCOL(retpcac), function(ordern)
  tseries::adf.test(retpcac[, ordern])$p.value)
# AdF unit root test on stationary time series
tseries::adf.test(rnorm(1e5))

library(quantmod)
##Perform pair-wise correlation analysis
# Calculate correlation matrix
cormat <- cor(retp)
colnames(cormat) <- colnames(retp)
rownames(cormat) <- colnames(retp)
# Reorder correlation matrix based on clusters
# Calculate permutation vector
library(corrplot)
ordern <- corrMatOrder(cormat, order="hclust",
        hclust.method="complete")
# Apply permutation vector
cormat <- cormat[ordern, ordern]
# Plot the correlation matrix
colorv <- colorRampPalette(c("red", "white", "blue"))
corrplot(cormat, tl.col="black", tl.cex=0.8,
    method="square", col=colorv(8),
    cl.offset=0.75, cl.cex=0.7,
    cl.align.text="l", cl.ratio=0.25)
# draw rectangles on the correlation matrix plot
corrRect.hclust(cormat, k=NROW(cormat) %/% 2,
          method="complete", col="red")

# Convert correlation matrix into distance object
distancev <- as.dist(1-cormat)
# Perform hierarchical clustering analysis
compclust <- hclust(distancev)
plot(compclust, ann=FALSE, xlab="", ylab="")
title("Dendrogram representing hierarchical clustering
\nwith dissimilarity = 1-correlation", line=-0.5)

# Simulate AR processes
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")  # Reset random numbers
datev <- Sys.Date() + 0:728  # Two year daily series
# AR time series of returns
arimav <- xts(x=arima.sim(n=NROW(datev), model=list(ar=0.2)),
          order.by=datev)
arimav <- cbind(arimav, cumsum(arimav))
colnames(arimav) <- c("AR returns", "AR prices")

library(ggplot2)  # Load ggplot2
library(gridExtra)  # Load gridExtra
autoplot(object=arimav, # ggplot AR process
 facets="Series ~ .",
 main="Autoregressive process (phi=0.2)") +
  facet_grid("Series ~ .", scales="free_y") +
  xlab("") + ylab("") +
theme(legend.position=c(0.1, 0.5),
  plot.background=element_blank(),
  axis.text.y=element_blank())

coeff <- c(-0.9, 0.01, 0.9)  # AR coefficients
# Create three AR time series
arimav <- sapply(coeff, function(phi) {
  set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")  # Reset random numbers
  arima.sim(n=NROW(datev), model=list(ar=phi))
})  # end sapply
colnames(arimav) <- paste("autocorr", coeff)
plot.zoo(arimav, main="AR(1) prices", xlab=NA)
# Or plot using ggplot
arimav <- xts(x=arimav, order.by=datev)
library(ggplot)
autoplot(arimav, main="AR(1) prices",
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
coeff <- c(0.1, 0.39, 0.5)
nrows <- 1e2
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection"); innov <- rnorm(nrows)
# Simulate AR process using recursive loop in R
arimav <- numeric(nrows)
arimav[1] <- innov[1]
arimav[2] <- coeff[1]*arimav[1] + innov[2]
arimav[3] <- coeff[1]*arimav[2] + coeff[2]*arimav[1] + innov[3]
for (it in 4:NROW(arimav)) {
  arimav[it] <- arimav[(it-1):(it-3)] %*% coeff + innov[it]
}  # end for
# Simulate AR process using filter()
arimaf <- filter(x=innov, filter=coeff, method="recursive")
class(arimaf)
all.equal(arimav, as.numeric(arimaf))
# Fast simulation of AR process using C_rfilter()
arimacpp <- .Call(stats:::C_rfilter, innov, coeff,
     double(NROW(coeff) + NROW(innov)))[-(1:3)]
all.equal(arimav, arimacpp)
# Fastest simulation of AR process using HighFreq::sim_ar()
arimav <- HighFreq::sim_ar(coeff=matrix(coeff), innov=matrix(innov))
arimav <- drop(arimav)
all.equal(arimav, arimacpp)
# Benchmark the speed of the three methods of simulating AR process
library(microbenchmark)
summary(microbenchmark(
  Rloop={for (it in 4:NROW(arimav)) {
    arimav[it] <- arimav[(it-1):(it-3)] %*% coeff + innov[it]
  }},
  filter=filter(x=innov, filter=coeff, method="recursive"),
  cpp=HighFreq::sim_ar(coeff=matrix(coeff), innov=matrix(innov))
  ), times=10)[, c(1, 4, 5)]

# Calculate modulus of roots of characteristic equation
rootv <- Mod(polyroot(c(1, -coeff)))
# Calculate warmup period
warmup <- NROW(coeff) + ceiling(6/log(min(rootv)))
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
nrows <- 1e4
innov <- rnorm(nrows + warmup)
# Simulate AR process using arima.sim()
arimav <- arima.sim(n=nrows,
  model=list(ar=coeff),
  start.innov=innov[1:warmup],
  innov=innov[(warmup+1):NROW(innov)])
# Simulate AR process using filter()
arimaf <- filter(x=innov, filter=coeff, method="recursive")
all.equal(arimaf[-(1:warmup)], as.numeric(arimav))
# Benchmark the speed of the three methods of simulating AR process
library(microbenchmark)
summary(microbenchmark(
  filter=filter(x=innov, filter=coeff, method="recursive"),
  arima_sim=arima.sim(n=nrows,
                  model=list(ar=coeff),
                  start.innov=innov[1:warmup],
                  innov=innov[(warmup+1):NROW(innov)]),
  arima_loop={for (it in 4:NROW(arimav)) {
  arimav[it] <- arimav[(it-1):(it-3)] %*% coeff + innov[it]}}
  ), times=10)[, c(1, 4, 5)]

x11(width=6, height=4)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0))
# Simulate AR(1) process
arimav <- arima.sim(n=1e3, model=list(ar=0.8))
# ACF of AR(1) process
acfl <- rutils::plot_acf(arimav, lag=10, xlab="", ylab="",
  main="Autocorrelations of AR(1) process")
acfl$acf[1:5]

# PACF of AR(1) process
pacfl <- pacf(arimav, lag=10, xlab="", ylab="", main="")
title("Partial autocorrelations of AR(1) process", line=1)
pacfl <- as.numeric(pacfl$acf)
pacfl[1:5]

library(rutils)  # Load rutils
library(ggplot2)  # Load ggplot2
# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
randw <- cumsum(zoo(matrix(rnorm(3*100), ncol=3),
order.by=(Sys.Date()+0:99)))
colnames(randw) <- paste("randw", 1:3, sep="_")
plot.zoo(randw, main="Random walks",
     xlab="", ylab="", plot.type="single",
     col=c("black", "red", "blue"))
# Add legend
legend(x="topleft", legend=colnames(randw),
 col=c("black", "red", "blue"), lty=1)

# Simulate arima with large AR coefficient
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
nrows <- 1e4
arimav <- arima.sim(n=nrows, model=list(ar=0.99))
tseries::adf.test(arimav)
# Integrated series has unit root
tseries::adf.test(cumsum(arimav))
# Simulate arima with negative AR coefficient
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
arimav <- arima.sim(n=nrows, model=list(ar=-0.99))
tseries::adf.test(arimav)
# Integrated series has unit root
tseries::adf.test(cumsum(arimav))

# Simulate random walks using apply() loops
# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
randws <- matrix(rnorm(1000*100), ncol=1000)
randws <- apply(randws, 2, cumsum)
varv <- apply(randws, 1, var)
# Simulate random walks using vectorized functions
# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
randws <- matrixStats::colCumsums(matrix(rnorm(1000*100), ncol=1000))
varv <- matrixStats::rowVars(randws)
par(mar=c(5, 3, 2, 2), oma=c(0, 0, 0, 0))
plot(varv, xlab="time steps", ylab="",
     t="l", col="blue", lwd=2,
     main="Variance of Random Walk")

# Define Brownian Motion parameters
nrows <- 1000; sigmav <- 0.01
# Simulate 5 paths of Brownian motion
pricev <- matrix(rnorm(5*nrows, sd=sigmav), nc=5)
pricev <- matrixStats::colCumsums(pricev)
# Plot 5 paths of Brownian motion
matplot(y=pricev, main="Brownian Motion Paths",
  xlab="time", ylab="path",
  type="l", lty="solid", lwd=1, col="blue")
# Save plot to png file on Mac
quartz.save("figure/brown_paths.png", type="png", width=6, height=4)

# Define Ornstein-Uhlenbeck parameters
prici <- 0.0; priceq <- 1.0;
sigmav <- 0.02; thetav <- 0.01; nrows <- 1000
# Initialize the data
innov <- rnorm(nrows)
retp <- numeric(nrows)
pricev <- numeric(nrows)
retp[1] <- sigmav*innov[1]
pricev[1] <- prici
# Simulate Ornstein-Uhlenbeck process in R
for (i in 2:nrows) {
  retp[i] <- thetav*(priceq - pricev[i-1]) + sigmav*innov[i]
  pricev[i] <- pricev[i-1] + retp[i]
}  # end for
# Simulate Ornstein-Uhlenbeck process in Rcpp
pricecpp <- HighFreq::sim_ou(prici=prici, priceq=priceq,
  theta=thetav, innov=matrix(sigmav*innov))
all.equal(pricev, drop(pricecpp))
# Compare the speed of R code with Rcpp
library(microbenchmark)
summary(microbenchmark(
  Rcode={for (i in 2:nrows) {
    retp[i] <- thetav*(priceq - pricev[i-1]) + sigmav*innov[i]
    pricev[i] <- pricev[i-1] + retp[i]}},
  Rcpp=HighFreq::sim_ou(prici=prici, priceq=priceq,
    theta=thetav, innov=matrix(sigmav*innov)),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

plot(pricev, type="l", xlab="time", ylab="prices",
     main="Ornstein-Uhlenbeck Process")
legend("topright", title=paste(c(paste0("sigmav = ", sigmav),
     paste0("priceq = ", ),
     paste0("thetav = ", thetav)),
   collapse="\n"),
 legend="", cex=0.8, inset=0.1, bg="white", bty="n")
abline(h=, col='red', lwd=2)

retp <- rutils::diffit(pricev)
pricelag <- rutils::lagit(pricev)
formulav <- retp ~ pricelag
regmod <- lm(formulav)
summary(regmod)
# Plot regression
plot(formulav, main="OU Returns Versus Lagged Prices")
abline(regmod, lwd=2, col="red")

# Calculate volatility parameter
c(volatility=sigmav, estimate=sd(retp))
# Extract OU parameters from regression
coeff <- summary(regmod)$coefficients
# Calculate regression alpha and beta directly
betac <- cov(retp, pricelag)/var(pricelag)
alphac <- (mean(retp) - betac*mean(pricelag))
cbind(direct=c(alpha=alphac, beta=betac), lm=coeff[, 1])
all.equal(c(alpha=alphac, beta=betac), coeff[, 1],
    check.attributes=FALSE)
# Calculate regression standard errors directly
betac <- c(alpha=alphac, beta=betac)
fitv <- (alphac + betac*pricelag)
resids <- (retp - fitv)
price2 <- sum((pricelag - mean(pricelag))^2)
betasd <- sqrt(sum(resids^2)/price2/(nrows-2))
alphasd <- sqrt(sum(resids^2)/(nrows-2)*(1:nrows + mean(pricelag)^2/price2))
cbind(direct=c(alphasd=alphasd, betasd=betasd), lm=coeff[, 2])
all.equal(c(alphasd=alphasd, betasd=betasd), coeff[, 2],
    check.attributes=FALSE)
# Compare mean reversion parameter theta
c(theta=(-thetav), round(coeff[2, ], 3))
# Compare equilibrium price mu
c(priceq=priceq, estimate=-coeff[1, 1]/coeff[2, 1])
# Compare actual and estimated parameters
coeff <- cbind(c(thetav*priceq, -thetav), coeff[, 1:2])
rownames(coeff) <- c("drift", "theta")
colnames(coeff)[1] <- "actual"
round(coeff, 4)

# Simulate Schwartz process
retp <- numeric(nrows)
pricev <- numeric(nrows)
pricev[1] <- exp(sigmav*innov[1])
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")  # Reset random numbers
for (i in 2:nrows) {
  retp[i] <- thetav*(priceq - pricev[i-1]) + sigmav*innov[i]
  pricev[i] <- pricev[i-1]*exp(retp[i])
}  # end for

plot(pricev, type="l", xlab="time", ylab="prices",
     main="Schwartz Process")
legend("topright",
 title=paste(c(paste0("sigmav = ", sigmav),
     paste0("priceq = ", priceq),
     paste0("thetav = ", thetav)),
   collapse="\n"),
 legend="", cex=0.8, inset=0.12, bg="white", bty="n")
abline(h=priceq, col='red', lwd=2)

# Define Dickey-Fuller parameters
prici <- 0.0;  priceq <- 1.0
thetav <- 0.01;  nrows <- 1000
coeff <- c(0.1, 0.39, 0.5)
# Initialize the data
innov <- rnorm(nrows, sd=0.01)
retp <- numeric(nrows)
pricev <- numeric(nrows)
# Simulate Dickey-Fuller process using recursive loop in R
retp[1] <- innov[1]
pricev[1] <- prici
retp[2] <- thetav*(priceq - pricev[1]) + coeff[1]*retp[1] +
  innov[2]
pricev[2] <- pricev[1] + retp[2]
retp[3] <- thetav*(priceq - pricev[2]) + coeff[1]*retp[2] +
  coeff[2]*retp[1] + innov[3]
pricev[3] <- pricev[2] + retp[3]
for (it in 4:nrows) {
  retp[it] <- thetav*(priceq - pricev[it-1]) +
    retp[(it-1):(it-3)] %*% coeff + innov[it]
  pricev[it] <- pricev[it-1] + retp[it]
}  # end for
# Simulate Dickey-Fuller process in Rcpp
pricecpp <- HighFreq::sim_df(prici=prici, priceq=priceq,
   theta=thetav, coeff=matrix(coeff), innov=matrix(innov))
# Compare prices
all.equal(pricev, drop(pricecpp))
# Compare the speed of R code with Rcpp
library(microbenchmark)
summary(microbenchmark(
  Rcode={for (it in 4:nrows) {
  retp[it] <- thetav*(priceq - pricev[it-1]) + retp[(it-1):(it-3)] %*% coeff + innov[it]
  pricev[it] <- pricev[it-1] + retp[it]
  }},
  Rcpp=HighFreq::sim_df(prici=prici, priceq=priceq, theta=thetav, coeff=matrix(coeff), innov=matrix(innov)),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# Simulate AR(1) process with coefficient=1, with unit root
innov <- matrix(rnorm(1e4, sd=0.01))
arimav <- HighFreq::sim_ar(coeff=matrix(1), innov=innov)
plot(arimav, t="l", main="Brownian Motion")
# Perform ADF test with lag = 1
tseries::adf.test(arimav, k=1)
# Perform standard Dickey-Fuller test
tseries::adf.test(arimav, k=0)
# Simulate AR(1) with coefficient close to 1, without unit root
arimav <- HighFreq::sim_ar(coeff=matrix(0.99), innov=innov)
plot(arimav, t="l", main="AR(1) coefficient = 0.99")
tseries::adf.test(arimav, k=1)
# Simulate Ornstein-Uhlenbeck OU process with mean reversion
prici <- 0.0; priceq <- 0.0; thetav <- 0.1
pricev <- HighFreq::sim_ou(prici=prici, priceq=priceq,
  theta=thetav, innov=innov)
plot(pricev, t="l", main=paste("OU coefficient =", thetav))
tseries::adf.test(pricev, k=1)
# Simulate Ornstein-Uhlenbeck OU process with zero reversion
thetav <- 0.0
pricev <- HighFreq::sim_ou(prici=prici, priceq=priceq,
  theta=thetav, innov=innov)
plot(pricev, t="l", main=paste("OU coefficient =", thetav))
tseries::adf.test(pricev, k=1)

# Simulate AR(1) process with different coefficients
coeffv <- seq(0.99, 0.999, 0.001)
retp <- as.numeric(na.omit(rutils::etfenv$returns$VTI))
adft <- sapply(coeffv, function(coeff) {
  arimav <- filter(x=retp, filter=coeff, method="recursive")
  adft <- suppressWarnings(tseries::adf.test(arimav))
  c(adfstat=unname(adft$statistic), pval=adft$p.value)
})  # end sapply
dev.new(width=6, height=4, noRStudioGD=TRUE)
# x11(width=6, height=4)
plot(x=coeffv, y=adft["pval", ], main="ADF p-val Versus AR Coefficient",
     xlab="AR coefficient", ylab="ADF pval", t="l", col="blue", lwd=2)
plot(x=coeffv, y=adft["adfstat", ], main="ADF Stat Versus AR Coefficient",
     xlab="AR coefficient", ylab="ADF stat", t="l", col="blue", lwd=2)

# Specify AR process parameters
nrows <- 1e3
coeff <- matrix(c(0.1, 0.39, 0.5)); ncoeff <- NROW(coeff)
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection"); innov <- matrix(rnorm(nrows))
# Simulate AR process using HighFreq::sim_ar()
arimav <- HighFreq::sim_ar(coeff=coeff, innov=innov)
# Fit AR model using ar.ols()
arfit <- ar.ols(arimav, order.max=ncoeff, aic=FALSE)
class(arfit)
is.list(arfit)
drop(arfit$ar); drop(coeff)
# Define predictor matrix without intercept column
predm <- sapply(1:ncoeff, rutils::lagit, input=arimav)
# Fit AR model using regression
predinv <- MASS::ginv(predm)
coeff <- drop(predinv %*% arimav)
all.equal(drop(arfit$ar), coeff, check.attributes=FALSE)

# Calculate the model residuals
fitv <- drop(predm %*% coeff)
resids <- drop(arimav - fitv)
# Variance of residuals
residsd <- sum(resids^2)/(nrows-NROW(coeff))
# Inverse of predictor matrix squared
pred2 <- MASS::ginv(crossprod(predm))
# Calculate covariance matrix of AR coefficients
covmat <- residsd*pred2
coefsd <- sqrt(diag(covmat))
# Calculate t-values of AR coefficients
coefft <- drop(coeff)/coefsd
# Plot the t-values of the AR coefficients
barplot(coefft, xlab="lag", ylab="t-value",
  main="Coefficient t-values of AR Forecasting Model")

# Fit AR(5) model into AR(3) process
predm <- sapply(1:5, rutils::lagit, input=arimav)
predinv <- MASS::ginv(predm)
coeff <- drop(predinv %*% arimav)
# Calculate t-values of AR(5) coefficients
resids <- drop(arimav - drop(predm %*% coeff))
residsd <- sum(resids^2)/(nrows-NROW(coeff))
covmat <- residsd*MASS::ginv(crossprod(predm))
coefsd <- sqrt(diag(covmat))
coefft <- drop(coeff)/coefsd
# Fit AR(5) model using arima()
arfit <- arima(arimav, order=c(5, 0, 0), include.mean=FALSE)
arfit$coef
# Fit AR(5) model using auto.arima()
library(forecast)  # Load forecast
arfit <- forecast::auto.arima(arimav, max.p=5, max.q=0, max.d=0)
# Fit AR(5) model into VTI returns
retp <- drop(zoo::coredata(na.omit(rutils::etfenv$returns$VTI)))
predm <- sapply(1:5, rutils::lagit, input=retp)
predinv <- MASS::ginv(predm)
coeff <- drop(predinv %*% retp)
# Calculate t-values of AR(5) coefficients
resids <- drop(retp - drop(predm %*% coeff))
residsd <- sum(resids^2)/(nrows-NROW(coeff))
covmat <- residsd*MASS::ginv(crossprod(predm))
coefsd <- sqrt(diag(covmat))
coefft <- drop(coeff)/coefsd

# Compute autocorrelation coefficients
acfl <- rutils::plot_acf(arimav, lag=10, plot=FALSE)
acfl <- drop(acfl$acf)
nrows <- NROW(acfl)
acf1 <- c(1, acfl[-nrows])
# Define Yule-Walker matrix
ywmat <- sapply(1:nrows, function(lagg) {
  if (lagg < nrows)
    c(acf1[lagg:1], acf1[2:(nrows-lagg+1)])
  else
    acf1[lagg:1]
})  # end sapply
# Generalized inverse of Yule-Walker matrix
ywmatinv <- MASS::ginv(ywmat)
# Solve Yule-Walker equations
ywcoeff <- drop(ywmatinv %*% acfl)
round(ywcoeff, 5)
coeff

# Calculate PACF from acf using Durbin-Levinson algorithm
acfl <- rutils::plot_acf(arimav, lag=10, plotobj=FALSE)
acfl <- drop(acfl$acf)
nrows <- NROW(acfl)
pacfl <- numeric(2)
pacfl[1] <- acfl[1]
pacfl[2] <- (acfl[2] - acfl[1]^2)/(1 - acfl[1]^2)
# Calculate PACF recursively in a loop using Durbin-Levinson algorithm
pacfll <- matrix(numeric(nrows*nrows), nc=nrows)
pacfll[1, 1] <- acfl[1]
for (it in 2:nrows) {
  pacfll[it, it] <- (acfl[it] - pacfll[it-1, 1:(it-1)] %*% acfl[(it-1):1])/(1 - pacfll[it-1, 1:(it-1)] %*% acfl[1:(it-1)])
  for (it2 in 1:(it-1)) {
    pacfll[it, it2] <- pacfll[it-1, it2] - pacfll[it, it] %*% pacfll[it-1, it-it2]
  }  # end for
}  # end for
pacfll <- diag(pacfll)
# Compare with the PACF without loop
all.equal(pacfl, pacfll[1:2])
# Calculate PACF using pacf()
pacfl <- pacf(arimav, lag=10, plot=FALSE)
pacfl <- drop(pacfl$acf)
all.equal(pacfl, pacfll)

# Load package rutils
library(rutils)
# Create new environment for data
sp500env <- new.env()
# Boolean vector of symbols already downloaded
isdown <- symbolv %in% ls(sp500env)
# Download in while loop from Tiingo and copy into environment
nattempts <- 0  # Number of download attempts
while ((sum(!isdown) > 0) & (nattempts<3)) {
  # Download data and copy it into environment
  nattempts <- nattempts + 1
  cat("Download attempt = ", nattempts, "\n")
  for (symboln in symbolv[!isdown]) {
    cat("processing: ", symboln, "\n")
    tryCatch(  # With error handler
quantmod::getSymbols(symboln, src="tiingo", adjust=TRUE, auto.assign=TRUE,
           from="1990-01-01", env=sp500env, api.key="j84ac2b9c5bde2d68e33034f65d838092c6c9f10"),
# Error handler captures error condition
error=function(msg) {
  print(paste0("Error handler: ", msg))
},  # end error handler
finally=print(paste0("Symbol = ", symboln))
    )  # end tryCatch
  }  # end for
  # Update vector of symbols already downloaded
  isdown <- symbolv %in% ls(sp500env)
  Sys.sleep(2)  # Wait 2 seconds until next attempt
}  # end while
class(sp500env$AAPL)
class(zoo::index(sp500env$AAPL))
tail(sp500env$AAPL)
symbolv[!isdown]

# The date-time index of AAPL is POSIXct
class(zoo::index(sp500env$AAPL))
# Coerce the date-time index of AAPL to Date
zoo::index(sp500env$AAPL) <- as.Date(zoo::index(sp500env$AAPL))
# Coerce all the date-time indices to Date
for (symboln in ls(sp500env)) {
  ohlc <- get(symboln, envir=sp500env)
  zoo::index(ohlc) <- as.Date(zoo::index(ohlc))
  assign(symboln, ohlc, envir=sp500env)
}  # end for

# "LOW.Low" is a bad column name
colnames(sp500env$LOW)
strsplit(colnames(sp500env$LOW), split="[.]")
do.call(cbind, strsplit(colnames(sp500env$LOW), split="[.]"))
do.call(cbind, strsplit(colnames(sp500env$LOW), split="[.]"))[2, ]
# Extract proper names from column names
namev <- rutils::get_name(colnames(sp500env$LOW), field=2)
# Or
# namev <- do.call(rbind, strsplit(colnames(sp500env$LOW),
#                                   split="[.]"))[, 2]
# Rename "LOW" colnames to "LOWES"
colnames(sp500env$LOW) <- paste("LOWES", namev, sep=".")
sp500env$LOWES <- sp500env$LOW
rm(LOW, envir=sp500env)
# Rename BF-B colnames to "BFB"
colnames(sp500env$"BF-B") <- paste("BFB", namev, sep=".")
sp500env$BFB <- sp500env$"BF-B"
rm("BF-B", envir=sp500env)
# Rename BRK-B colnames
sp500env$BRKB <- sp500env$`BRK-B`
rm(`BRK-B`, envir=sp500env)
colnames(sp500env$BRKB) <- gsub("BRK-B", "BRKB", colnames(sp500env$BRKB))
# Save OHLC prices to .RData file
save(sp500env, file="/Users/jerzy/Develop/lecture_slides/data/sp500.RData")
# Download "BRK.B" separately with auto.assign=FALSE
# BRKB <- quantmod::getSymbols("BRK-B", auto.assign=FALSE, src="tiingo", adjust=TRUE, from="1990-01-01", api.key="j84ac2b9c5bde2d68e33034f65d838092c6c9f10")
# colnames(BRKB) <- paste("BRKB", namev, sep=".")
# sp500env$BRKB <- BRKB

# Plot OHLC candlestick chart for LOWES
chart_Series(x=sp500env$LOWES["2019-12/"],
  TA="add_Vo()", name="LOWES OHLC Stock Prices")
# Plot dygraph
dygraphs::dygraph(sp500env$LOWES["2019-12/", -5], main="LOWES OHLC Stock Prices") %>%
  dyCandlestick()

# Load S&P500 constituent stock prices
load("/Users/jerzy/Develop/lecture_slides/data/sp500.RData")
pricev <- eapply(sp500env, quantmod::Cl)
pricev <- rutils::do_call(cbind, pricev)
# Carry forward non-NA prices
pricev <- zoo::na.locf(pricev, na.rm=FALSE)
# Drop ".Close" from column names
colnames(pricev)
colnames(pricev) <- rutils::get_name(colnames(pricev))
# Or
# colnames(pricev) <- do.call(rbind,
#   strsplit(colnames(pricev), split="[.]"))[, 1]
# Calculate percentage returns of the S&P500 constituent stocks
# retp <- xts::diff.xts(log(pricev))
retp <- xts::diff.xts(pricev)/rutils::lagit(pricev, pad_zeros=FALSE)
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
samplev <- sample(NCOL(retp), s=100, replace=FALSE)
prices100 <- pricev[, samplev]
returns100 <- retp[, samplev]
save(pricev, prices100,
  file="/Users/jerzy/Develop/lecture_slides/data/sp500_prices.RData")
save(retp, returns100,
  file="/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")

# Calculate number of constituents without prices
datav <- rowSums(is.na(pricev))
datav <- xts::xts(datav, order.by=zoo::index(pricev))
dygraphs::dygraph(datav, main="Number of S&P500 Constituents Without Prices") %>%
  dyOptions(colors="blue", strokeWidth=2)

# Calculate price weighted index of constituent
ncols <- NCOL(pricev)
pricev <- zoo::na.locf(pricev, fromLast=TRUE)
indeks <- xts(rowSums(pricev)/ncols, zoo::index(pricev))
colnames(indeks) <- "index"
# Combine index with VTI
datav <- cbind(indeks[zoo::index(etfenv$VTI)], etfenv$VTI[, 4])
colv <- c("index", "VTI")
colnames(datav) <- colv
# Plot index with VTI
endw <- rutils::calc_endpoints(datav, interval="weeks")
dygraphs::dygraph(log(datav)[endw],
  main="S&P 500 Price-weighted Index and VTI") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", col="red") %>%
  dySeries(name=colv[2], axis="y2", col="blue")

# Save the environment to compressed .RData file
dirn <- "/Users/jerzy/Develop/lecture_slides/data/"
save(sp500env, file=paste0(dirn, "sp500.RData"))
# Save the ETF prices into CSV files
dirn <- "/Users/jerzy/Develop/lecture_slides/data/SP500/"
for (symboln in ls(sp500env)) {
  zoo::write.zoo(sp500env$symbol, file=paste0(dirn, symboln, ".csv"))
}  # end for
# Or using lapply()
filev <- lapply(ls(sp500env), function(symboln) {
  xtsv <- get(symboln, envir=sp500env)
  zoo::write.zoo(xtsv, file=paste0(dirn, symboln, ".csv"))
  symboln
})  # end lapply
unlist(filev)
# Or using eapply() and data.table::fwrite()
filev <- eapply(sp500env , function(xtsv) {
  filen <- rutils::get_name(colnames(xtsv)[1])
  data.table::fwrite(data.table::as.data.table(xtsv), file=paste0(dirn, filen, ".csv"))
  filen
})  # end eapply
unlist(filev)

# Load the environment from compressed .RData file
dirn <- "/Users/jerzy/Develop/lecture_slides/data/"
load(file=paste0(dirn, "sp500.RData"))
# Get all the .csv file names in the directory
dirn <- "/Users/jerzy/Develop/lecture_slides/data/SP500/"
filev <- Sys.glob(paste0(dirn, "*.csv"))
# Create new environment for data
sp500env <- new.env()
for (filen in filev) {
  xtsv <- xts::as.xts(zoo::read.csv.zoo(filen))
  symboln <- rutils::get_name(colnames(xtsv)[1])
  # symboln <- strsplit(colnames(xtsv), split="[.]")[[1]][1]
  assign(symboln, xtsv, envir=sp500env)
}  # end for
# Or using fread()
for (filen in filev) {
  xtsv <- data.table::fread(filen)
  data.table::setDF(xtsv)
  xtsv <- xts::xts(xtsv[, -1], as.Date(xtsv[, 1]))
  symboln <- rutils::get_name(colnames(xtsv)[1])
  assign(symboln, xtsv, envir=sp500env)
}  # end for

# Remove all files from environment(if necessary)
rm(list=ls(sp500env), envir=sp500env)
# Download in while loop from Alpha Vantage and copy into environment
isdown <- symbolv %in% ls(sp500env)
nattempts <- 0
while ((sum(!isdown) > 0) & (nattempts < 10)) {
  # Download data and copy it into environment
  nattempts <- nattempts + 1
  for (symboln in symbolv[!isdown]) {
    cat("processing: ", symboln, "\n")
    tryCatch(  # With error handler
quantmod::getSymbols(symboln, src="av", adjust=TRUE, auto.assign=TRUE, env=sp500env,
           output.size="full", api.key="T7JPW54ES8G75310"),
# error handler captures error condition
error=function(msg) {
  print(paste0("Error handler: ", msg))
},  # end error handler
finally=print(paste0("Symbol = ", symboln))
    )  # end tryCatch
  }  # end for
  # Update vector of symbols already downloaded
  isdown <- symbolv %in% ls(sp500env)
  Sys.sleep(2)  # Wait 2 seconds until next attempt
}  # end while
# Adjust all OHLC prices in environment
for (symboln in ls(sp500env)) {
  assign(symboln,
    adjustOHLC(get(x=symboln, envir=sp500env), use.Adjusted=TRUE),
    envir=sp500env)
}  # end for

library(rutils)  # Load package rutils
# Assign name SP500 to ^GSPC symbol
quantmod::setSymbolLookup(SP500=list(name="^GSPC", src="yahoo"))
quantmod::getSymbolLookup()
# View and clear options
options("getSymbols.sources")
options(getSymbols.sources=NULL)
# Download S&P500 prices into etfenv
quantmod::getSymbols("SP500", env=etfenv,
    adjust=TRUE, auto.assign=TRUE, from="1990-01-01")

chart_Series(x=etfenv$SP500["2016/"],
       TA="add_Vo()", name="S&P500 index")

library(rutils)  # Load package rutils
# Assign name DJIA to ^DJI symbol
setSymbolLookup(DJIA=list(name="^DJI", src="yahoo"))
getSymbolLookup()
# view and clear options
options("getSymbols.sources")
options(getSymbols.sources=NULL)
# Download DJIA prices into etfenv
quantmod::getSymbols("DJIA", env=etfenv,
    adjust=TRUE, auto.assign=TRUE, from="1990-01-01")
chart_Series(x=etfenv$DJIA["2016/"],
       TA="add_Vo()", name="DJIA index")

# Calculate prices from OHLC data of the S&P500 stocks
pricev <- eapply(sp500env, quantmod::Cl)
pricev <- rutils::do_call(cbind, pricev)
# Carry forward non-NA prices
pricev <- zoo::na.locf(pricev, na.rm=FALSE)
# Get first column name
colnames(pricev[, 1])
rutils::get_name(colnames(pricev[, 1]))
# Modify column names
colnames(pricev) <- rutils::get_name(colnames(pricev))
# Or
# colnames(pricev) <- do.call(rbind,
#   strsplit(colnames(pricev), split="[.]"))[, 1]
# Calculate percentage returns
retp <- xts::diff.xts(pricev)/rutils::lagit(pricev, pad_zeros=FALSE)
# Select a random sample of 100 prices and returns
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
samplev <- sample(NCOL(retp), s=100, replace=FALSE)
prices100 <- pricev[, samplev]
returns100 <- retp[, samplev]
# Save the data into binary files
save(pricev, prices100,
     file="/Users/jerzy/Develop/lecture_slides/data/sp500_prices.RData")
save(retp, returns100,
     file="/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")

# Setup code
symboln <- "SPY"
startd <- as.Date("1990-01-01")
todayd <- Sys.Date()
tspan <- "day"
# Replace below your own Polygon API key
apikey <- "SEpnsBpiRyONMJdl48r6dOo0_pjmCu5r"
# Create url for download
urll <- paste0("https://api.polygon.io/v2/aggs/ticker/", symboln, "/range/1/", tspan, "/", startd, "/", todayd, "?adjusted=true&sort=asc&limit=50000&apiKey=", apikey)
# Download SPY OHLC prices in JSON format from Polygon
ohlc <- jsonlite::read_json(urll)
class(ohlc)
NROW(ohlc)
names(ohlc)
# Extract list of prices from json object
ohlc <- ohlc$results
# Coerce from list to matrix
ohlc <- lapply(ohlc, unlist)
ohlc <- do.call(rbind, ohlc)
# Coerce time from milliseconds to dates
datev <- ohlc[, "t"]/1e3
datev <- as.POSIXct(datev, origin="1970-01-01")
datev <- as.Date(datev)
tail(datev)
# Coerce from matrix to xts
ohlc <- ohlc[, c("o","h","l","c","v","vw")]
colnames(ohlc) <- c("Open", "High", "Low", "Close", "Volume", "VWAP")
ohlc <- xts::xts(ohlc, order.by=datev)
tail(ohlc)
# Save the xts time series to compressed RData file
save(ohlc, file="/Users/jerzy/Data/spy_daily.RData")
# Candlestick plot of SPY OHLC prices
dygraphs::dygraph(ohlc[, 1:4], main=paste("Candlestick Plot of", symboln, "OHLC prices")) %>%
  dygraphs::dyCandlestick()

# Select ETF symbols for asset allocation
symbolv <- c("SPY", "VTI", "QQQ", "VEU", "EEM", "XLY", "XLP",
"XLE", "XLF", "XLV", "XLI", "XLB", "XLK", "XLU", "VYM", "IVW",
"IWB", "IWD", "IWF", "IEF", "TLT", "VNQ", "DBC", "GLD", "USO",
"VXX", "SVXY", "MTUM", "IVE", "VLUE", "QUAL", "VTV", "USMV", "AIEQ")
# Setup code
etfenv <- new.env()  # New environment for data
# Boolean vector of symbols already downloaded
isdown <- symbolv %in% ls(etfenv)

# Download data from Polygon using while loop
while (sum(!isdown) > 0) {
  for (symboln in symbolv[!isdown]) {
    cat("Processing:", symboln, "\n")
    tryCatch({  # With error handler
# Download OHLC bars from Polygon into JSON format file
urll <- paste0("https://api.polygon.io/v2/aggs/ticker/", symboln, "/range/1/", tspan, "/", startd, "/", todayd, "?adjusted=true&sort=asc&limit=50000&apiKey=", apikey)
ohlc <- jsonlite::read_json(urll)
# Extract list of prices from json object
ohlc <- ohlc$results
# Coerce from list to matrix
ohlc <- lapply(ohlc, unlist)
ohlc <- do.call(rbind, ohlc)
# Coerce time from milliseconds to dates
datev <- ohlc[, "t"]/1e3
datev <- as.POSIXct(datev, origin="1970-01-01")
datev <- as.Date(datev)
# Coerce from matrix to xts
ohlc <- ohlc[, c("o","h","l","c","v","vw")]
colnames(ohlc) <- paste0(symboln, ".", c("Open", "High", "Low", "Close", "Volume", "VWAP"))
ohlc <- xts::xts(ohlc, order.by=datev)
# Save to environment
assign(symboln, ohlc, envir=etfenv)
Sys.sleep(1)
},
    error={function(msg) print(paste0("Error handler: ", msg))},
    finally=print(paste0("Symbol = ", symboln))
    )  # end tryCatch
  }  # end for
  # Update vector of symbols already downloaded
  isdown <- symbolv %in% ls(etfenv)
}  # end while
save(etfenv, file="/Users/jerzy/Develop/lecture_slides/data/etf_data.RData")

# Extract Close prices
pricev <- eapply(etfenv, quantmod::Cl)
pricev <- do.call(cbind, pricev)
# Drop ".Close" from colnames
colnames(pricev) <- do.call(rbind, strsplit(colnames(pricev), split="[.]"))[, 1]
# Calculate the log returns
retp <- xts::diff.xts(log(pricev))
# Copy prices and returns into etfenv
etfenv$pricev <- pricev
etfenv$retp <- retp
# Copy symbolv into etfenv
etfenv$symbolv <- symbolv
# Calculate the risk-return statistics
riskstats <- PerformanceAnalytics::table.Stats(retp)
# Transpose the data frame
riskstats <- as.data.frame(t(riskstats))
# Add Name column
riskstats$Name <- rownames(riskstats)
# Copy riskstats into etfenv
etfenv$riskstats <- riskstats
# Calculate the beta, alpha, Treynor ratio, and other performance statistics
capmstats <- PerformanceAnalytics::table.CAPM(Ra=retp[, symbolv],
                                         Rb=retp[, "VTI"], scale=252)
colv <- strsplit(colnames(capmstats), split=" ")
colv <- do.call(cbind, colv)[1, ]
colnames(capmstats) <- colv
capmstats <- t(capmstats)
capmstats <- capmstats[, -1]
colv <- colnames(capmstats)
whichv <- match(c("Annualized Alpha", "Information Ratio", "Treynor Ratio"), colv)
colv[whichv] <- c("Alpha", "Information", "Treynor")
colnames(capmstats) <- colv
capmstats <- capmstats[order(capmstats[, "Alpha"], decreasing=TRUE), ]
# Copy capmstats into etfenv
etfenv$capmstats <- capmstats
save(etfenv, file="/Users/jerzy/Develop/lecture_slides/data/etf_data.RData")

library(rutils)  # Load package rutils
library(RCurl)  # Load package RCurl
library(XML)  # Load package XML
# Download text data from URL
sp500 <- getURL(
  "https://en.wikipedia.org/wiki/List_of_S%26P500_companies")
# Extract tables from the text data
sp500 <- readHTMLTable(sp500)
str(sp500)
# Extract colnames of data frames
lapply(sp500, colnames)
# Extract S&P500 constituents
sp500 <- sp500[[1]]
head(sp500)
# Create valid R names from symbols containing "-" or "."characters
sp500$namev <- gsub("-", "_", sp500$Ticker)
sp500$namev <- gsub("[.]", "_", sp500$names)
# Write data frame of S&P500 constituents to CSV file
write.csv(sp500,
  file="/Users/jerzy/Develop/lecture_slides/data/sp500_Yahoo.csv",
  row.names=FALSE)

library(rutils)  # Load package rutils
# Load data frame of S&P500 constituents from CSV file
sp500 <- read.csv(file="/Users/jerzy/Develop/lecture_slides/data/sp500_Yahoo.csv")
# Register symbols corresponding to R names
for (indeks in 1:NROW(sp500)) {
  cat("processing: ", sp500$Ticker[indeks], "\n")
  setSymbolLookup(structure(
    list(list(name=sp500$Ticker[indeks])),
    names=sp500$names[indeks]))
}  # end for
sp500env <- new.env()  # new environment for data
# Remove all files (if necessary)
rm(list=ls(sp500env), envir=sp500env)
# Download data and copy it into environment
rutils::get_data(sp500$names,
   env_out=sp500env, startd="1990-01-01")
# Or download in loop
for (symboln in sp500$names) {
  cat("processing: ", symboln, "\n")
  rutils::get_data(symboln,
   env_out=sp500env, startd="1990-01-01")
}  # end for
save(sp500env, file="/Users/jerzy/Develop/lecture_slides/data/sp500.RData")
chart_Series(x=sp500env$BRKB["2016/"],
       TA="add_Vo()", name="BRK-B stock")

# Download U.S. unemployment rate data
unrate <- quantmod::getSymbols("UNRATE",
   auto.assign=FALSE, src="FRED")
# Plot U.S. unemployment rate data
dygraphs::dygraph(unrate["1990/"], main="U.S. Unemployment Rate") %>%
  dyOptions(colors="blue", strokeWidth=2)
# Or
quantmod::chart_Series(unrate["1990/"], name="U.S. Unemployment Rate")

library(rutils)  # Load package rutils
install.packages("devtools")
library(devtools)
# Install package Quandl from github
install_github("quandl/R-package")
library(Quandl)  # Load package Quandl
# Register Quandl API key
Quandl.api_key("pVJi9Nv3V8CD3Js5s7Qx")
# Get short description
packageDescription("Quandl")
# Load help page
help(package="Quandl")
# Remove Quandl from search path
detach("package:Quandl")

library(rutils)  # Load package rutils
# Download EOD AAPL prices from WIKI free database
pricev <- Quandl(code="WIKI/AAPL",
  type="xts", startd="1990-01-01")
x11(width=14, height=7)
chart_Series(pricev["2016", 1:4], name="AAPL OHLC prices")
# Add trade volume in extra panel
add_TA(pricev["2016", 5])
# Download euro currency rates
pricev <- Quandl(code="BNP/USDEUR",
    startd="2013-01-01",
    endd="2013-12-01", type="xts")
# Download multiple time series
pricev <- Quandl(code=c("NSE/OIL", "WIKI/AAPL"),
    startd="2013-01-01", type="xts")
# Download AAPL gross profits
prof_it <- Quandl("RAYMOND/AAPL_GROSS_PROFIT_Q", type="xts")
chart_Series(prof_it, name="AAPL gross profits")
# Download Hurst time series
pricev <- Quandl(code="PE/AAPL_HURST",
    startd="2013-01-01", type="xts")
chart_Series(pricev["2016/", 1], name="AAPL Hurst")

library(rutils)  # Load package rutils
# Load S&P500 stock Quandl codes
sp500 <- read.csv(
  file="/Users/jerzy/Develop/lecture_slides/data/sp500_quandl.csv")
# Replace "-" with "_" in symbols
sp500$free_code <- gsub("-", "_", sp500$free_code)
head(sp500)
# vector of symbols in sp500 frame
tickers <- gsub("-", "_", sp500$ticker)
# Or
tickers <- matrix(unlist(
  strsplit(sp500$free_code, split="/"),
  use.names=FALSE), ncol=2, byrow=TRUE)[, 2]
# Or
tickers <- do_call_rbind(
  strsplit(sp500$free_code, split="/"))[, 2]

library(rutils)  # Load package rutils
sp500env <- new.env()  # new environment for data
# Remove all files (if necessary)
rm(list=ls(sp500env), envir=sp500env)
# Boolean vector of symbols already downloaded
isdown <- tickers %in% ls(sp500env)
# Download data and copy it into environment
for (ticker in tickers[!isdown]) {
  cat("processing: ", ticker, "\n")
  datav <- Quandl(code=paste0("WIKI/", ticker),
            startd="1990-01-01", type="xts")[, -(1:7)]
  colnames(datav) <- paste(ticker,
    c("Open", "High", "Low", "Close", "Volume"), sep=".")
  assign(ticker, datav, envir=sp500env)
}  # end for
save(sp500env, file="/Users/jerzy/Develop/lecture_slides/data/sp500.RData")
chart_Series(x=sp500env$XOM["2016/"], TA="add_Vo()", name="XOM stock")

library(rutils)
library(Quandl)
# Register Quandl API key
Quandl.api_key("pVJi9Nv3V8CD3Js5s7Qx")
# Download E-mini S&P500 futures prices
pricev <- Quandl(code="CHRIS/CME_ES1",
  type="xts", startd="1990-01-01")
pricev <- pricev[, c("Open", "High", "Low", "Last", "Volume")]
colnames(pricev)[4] <- "Close"
# Plot the prices
x11(width=5, height=4)  # Open x11 for plotting
chart_Series(x=pricev["2008-06/2009-06"],
       TA="add_Vo()", name="S&P500 Futures")
# Plot dygraph
dygraphs::dygraph(pricev["2008-06/2009-06", -5],
  main="S&P500 Futures") %>%
  dyCandlestick()

# Read CBOE futures expiration dates
datev <- read.csv(file="/Users/jerzy/Develop/lecture_slides/data/futures_expiration_dates_codes.csv",
  row.names=1)
# Create directory for data
dirn <- "/Users/jerzy/Develop/data/vix_data"
dir.create(dirn)
namev <- rownames(datev)
filev <- file.path(dirn, paste0(namev, ".csv"))
filelog <- file.path(dirn, "log_file.txt")
urlcboe <- "https://markets.cboe.com/us/futures/market_statistics/historical_data/products/csv/VX/"
urlv <- paste0(urlcboe, datev[, 1])
# Download files in loop
for (it in seq_along(urlv)) {
    tryCatch(  # Warning and error handler
  download.file(urlv[it], destfile=filev[it], quiet=TRUE),
# Warning handler captures warning condition
warning=function(msg) {
  cat(paste0("Warning handler: ", msg, "\n"), file=filelog, append=TRUE)
},  # end warning handler
# Error handler captures error condition
error=function(msg) {
  cat(paste0("Error handler: ", msg, "\n"), append=TRUE)
},  # end error handler
finally=cat(paste0("Processing file name = ", filev[it], "\n"), append=TRUE)
    )  # end tryCatch
}  # end for

# Create new environment for data
vixenv <- new.env()
# Download VIX data for the months 6, 7, and 8 in 2018
library(qmao)
quantmod::getSymbols("VX", Months=1:12,
  Years=2018, src="cfe", auto.assign=TRUE, env=vixenv)
# Or
qmao::getSymbols.cfe(Symbols="VX",
  Months=6:8, Years=2018, env=vixenv,
  verbose=FALSE, auto.assign=TRUE)
# Calculate the classes of all the objects
# In the environment vixenv
unlist(eapply(vixenv, function(x) {class(x)[1]}))
class(vixenv$VX_M18)
colnames(vixenv$VX_M18)
# Save the data to a binary file called "vix_cboe.RData".
save(vixenv,
  file="/Users/jerzy/Develop/data/vix_data/vix_cboe.RData")
