# Perform regression using formula
retp <- na.omit(rutils::etfenv$returns[, c("XLP", "VTI")])
riskfree <- 0.03/252
retp <- (retp - riskfree)
regmod <- lm(XLP ~ VTI, data=retp)
regmodsum <- summary(regmod)
# Get regression coefficients
coef(regmodsum)
# Get alpha and beta
coef(regmodsum)[, 1]
# Plot scatterplot of returns with aspect ratio 1
plot(XLP ~ VTI, data=rutils::etfenv$returns, main="Regression XLP ~ VTI",
     xlim=c(-0.1, 0.1), ylim=c(-0.1, 0.1), pch=1, col="blue", asp=1)
# Add regression line and perpendicular line
abline(regmod, lwd=2, col="red")
abline(a=0, b=-1/coef(regmodsum)[2, 1], lwd=2, col="blue")
# Get regression coefficients
coef(regmodsum)
# Calculate regression coefficients from scratch
betac <- drop(cov(retp$XLP, retp$VTI)/var(retp$VTI))
alphac <- drop(mean(retp$XLP) - betac*mean(retp$VTI))
c(alphac, betac)
# Calculate the residuals
residuals <- (retp$XLP - (alphac + betac*retp$VTI))
# Calculate the standard deviation of residuals
nrows <- NROW(residuals)
residsd <- sqrt(sum(residuals^2)/(nrows - 2))
# Calculate the standard errors of beta and alpha
sum2 <- sum((retp$VTI - mean(retp$VTI))^2)
betasd <- residsd/sqrt(sum2)
alphasd <- residsd*sqrt(1/nrows + mean(retp$VTI)^2/sum2)
c(alphasd, betasd)
# Perform the Durbin-Watson test of autocorrelation of residuals
lmtest::dwtest(regmod)
retp <- rutils::etfenv$returns
symbolv <- colnames(retp)
symbolv <- symbolv[symbolv != "VTI"]
# Perform regressions and collect statistics
betam <- sapply(symbolv, function(symbol) {
# Specify regression formula
  formulav <- as.formula(paste(symbol, "~ VTI"))
# Perform regression
  regmod <- lm(formulav, data=retp)
# Get regression summary
  regmodsum <- summary(regmod)
# Collect regression statistics
  with(regmodsum, 
    c(beta=coefficients[2, 1], 
pbeta=coefficients[2, 4],
alpha=coefficients[1, 1], 
palpha=coefficients[1, 4], 
pdw=lmtest::dwtest(regmod)$p.value))
})  # end sapply
betam <- t(betam)
# Sort by palpha
betam <- betam[order(betam[, "palpha"]), ]
betam
library(PerformanceAnalytics)
# Calculate XLP beta
PerformanceAnalytics::CAPM.beta(Ra=retp$XLP, Rb=retp$VTI)
# Or
retxlp <- na.omit(retp[, c("XLP", "VTI")])
betac <- drop(cov(retxlp$XLP, retxlp$VTI)/var(retxlp$VTI))
betac
# Calculate XLP alpha
PerformanceAnalytics::CAPM.alpha(Ra=retp$XLP, Rb=retp$VTI)
# Or
mean(retp$XLP - betac*retp$VTI)
# Calculate XLP bull beta
PerformanceAnalytics::CAPM.beta.bull(Ra=retp$XLP, Rb=retp$VTI)
# Calculate XLP bear beta
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
riskfree <- 0.01
abline(a=riskfree, b=(retvti-riskfree), col="green", lwd=2)
# Add labels
text(x=betac, y=retsann, labels=names(betac), pos=2, cex=0.8)
# Find optimal risk-free rate by minimizing residuals
rss <- function(riskfree) {
  sum((retsann - riskfree - betac*(retvti-riskfree))^2)
}  # end rss
optimrss <- optimize(rss, c(-1, 1))
riskfree <- optimrss$minimum
# Or simply
retsadj <- (retsann - retvti*betac)
betadj <- (1-betac)
riskfree <- sum(retsadj*betadj)/sum(betadj^2)
abline(a=riskfree, b=(retvti-riskfree), col="blue", lwd=2)
legend(x="topleft", bty="n", title="Security Market Line",
 legend=c("optimal fit", "riskfree=0.01"),
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
riskfree <- 0.01
abline(a=riskfree, b=(retvti-riskfree), col="green", lwd=2)
# Find optimal risk-free rate by minimizing residuals
retsadj <- (retsann - retvti*betac)
betadj <- (1-betac)
riskfree <- sum(retsadj*betadj)/sum(betadj^2)
abline(a=riskfree, b=(retvti-riskfree), col="blue", lwd=2)
legend(x="topleft", bty="n", title="Security Market Line",
 legend=c("optimal fit", "riskfree=0.01"),
 y.intersp=0.5, cex=1.0, lwd=6, lty=1, col=c("blue", "green"))
library(PerformanceAnalytics)
# Calculate XLP Treynor ratio
TreynorRatio(Ra=retp$XLP, Rb=retp$VTI)
# Calculate XLP Information ratio
InformationRatio(Ra=retp$XLP, Rb=retp$VTI)
PerformanceAnalytics::table.CAPM(Ra=retp[, c("XLP", "XLF")], 
                           Rb=retp$VTI, scale=252)
capmstats <- table.CAPM(Ra=retp[, symbolv],
        Rb=retp$VTI, scale=252)
colnamev <- strsplit(colnames(capmstats), split=" ")
colnamev <- do.call(cbind, colnamev)[1, ]
colnames(capmstats) <- colnamev
capmstats <- t(capmstats)
capmstats <- capmstats[, -1]
colnamev <- colnames(capmstats)
whichv <- match(c("Annualized Alpha", "Information Ratio", "Treynor Ratio"), colnamev)
colnamev[whichv] <- c("Alpha", "Information", "Treynor")
colnames(capmstats) <- colnamev
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
controlv <- HighFreq::param_reg()
reg_stats <- HighFreq::roll_reg(respv=retp$XLP, predm=retp$VTI,
  startp=(startp-1), endp=(endd-1), controlv=controlv)
betac <- reg_stats[, 2]
all.equal(betac, betar)
# Compare the speed of RcppArmadillo with R code
library(microbenchmark)
summary(microbenchmark(
  Rcpp=HighFreq::roll_reg(respv=retp$XLP, predm=retp$VTI, startp=(startp-1), endp=(endd-1), controlv=controlv),
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
colnamev <- colnames(datav)
dygraphs::dygraph(datav, main="XLP Trailing 12-month Beta and VTI Prices") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", col="red", strokeWidth=2) %>%
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
controlv <- HighFreq::param_reg()
reg_stats <- HighFreq::roll_reg(respv=retp$XLP, predm=retp$VTI,
  startp=(startp-1), endp=(endd-1), controlv=controlv)
betac <- reg_stats[, 2]
all.equal(betac, betar)
# Compare the speed of RcppArmadillo with R code
library(microbenchmark)
summary(microbenchmark(
  Rcpp=HighFreq::roll_reg(respv=retp$XLP, predm=retp$VTI, startp=(startp-1), endp=(endd-1), controlv=controlv),
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
colnamev <- colnames(datav)
dygraphs::dygraph(datav, main="XLP Trailing 12-month Beta and VTI Prices") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=colnamev[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Calculate the trailing betas
lambdaf <- 0.99
covarv <- HighFreq::run_covar(retp, lambdaf)
betac <- covarv[, 1]/covarv[, 3]
# dygraph plot of trailing XLP beta and VTI prices
datav <- cbind(pricev, betac[endd])[-(1:11)] # Remove warmup period
colnames(datav)[2] <- "beta"
colnamev <- colnames(datav)
dygraphs::dygraph(datav, main="XLP Trailing 12-month Beta and VTI Prices") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=colnamev[2], axis="y2", col="red", strokeWidth=2) %>%
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
#Perform pair-wise correlation analysis
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
library(PerformanceAnalytics)  # Load package "PerformanceAnalytics"
# PC returns from rotation and scaled returns
retsc <- apply(retp, 2, scale)
retpca <- retsc %*% pcad$rotation
# "x" matrix contains time series of PC returns
dim(pcad$x)
class(pcad$x)
head(pcad$x[, 1:3], 3)
# Convert PC matrix to xts and rescale to decimals
retpca <- xts(pcad$x/100, order.by=zoo::index(retp))
library(PerformanceAnalytics)  # Load package "PerformanceAnalytics"
chart.CumReturns(
  retpca[, 1:3], lwd=2, ylab="",
  legend.loc="topright", main="")
# Add title
title(main="ETF cumulative returns", line=-1)
library(PerformanceAnalytics)
# Calculate PC correlation matrix
cormat <- cor(retpca)
colnames(cormat) <- colnames(retpca)
rownames(cormat) <- colnames(retpca)
cormat[1:3, 1:3]
table.CAPM(Ra=retpca[, 1:3], Rb=retp$VTI, scale=252)
par(oma=c(1,0,1,0), mgp=c(2,1,0), mar=c(2,1,2,1), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # Set plot panels
#Perform principal component analysis PCA
retp <- na.omit(rutils::etfenv$returns)
pcad <- prcomp(retp, center=TRUE, scale=TRUE)
barplot(pcad$sdev[1:10],
  names.arg=colnames(pcad$rotation)[1:10],
  las=3, ylab="STDEV", xlab="PCVec",
  main="PCA Explain VAR")
# Show first three principal component loadings
head(pcad$rotation[,1:3], 3)
# Permute second principal component loadings by size
pca2 <- as.matrix(
  pcad$rotation[order(pcad$rotation[, 2],
  decreasing=TRUE), 2])
colnames(pca2) <- "pca2"
head(pca2, 3)
# The option las=3 rotates the names.arg labels
barplot(as.vector(pca2),
  names.arg=rownames(pca2),
  las=3, ylab="Loadings",
  xlab="Symbol", main="Loadings pca2")
par(oma=c(1,0,1,0), mgp=c(2,1,0), mar=c(2,1,2,1), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(3,1))  # Set plot panels
# Get list of principal component vectors
pca_vecs <- lapply(1:3, function(ordern) {
  pca_vec <- as.matrix(
    pcad$rotation[
    order(pcad$rotation[, ordern],
    decreasing=TRUE), ordern])
  colnames(pca_vec) <- paste0("pca", ordern)
  pca_vec
})  # end lapply
names(pca_vecs) <- c("pca1", "pca2", "pca3")
# The option las=3 rotates the names.arg labels
for (ordern in 1:3) {
  barplot(as.vector(pca_vecs[[ordern]]),
  names.arg=rownames(pca_vecs[[ordern]]),
  las=3, xlab="", ylab="",
  main=paste("Loadings",
    colnames(pca_vecs[[ordern]])))
}  # end for
library(factorAnalytics)  # Load package "factorAnalytics"
# Get documentation for package "factorAnalytics"
packageDescription("factorAnalytics")  # Get short description
help(package="factorAnalytics")  # Load help page
options(width=50)
library(factorAnalytics)  # Load package "factorAnalytics"
# List all objects in "factorAnalytics"
ls("package:factorAnalytics")
# List all datasets in "factorAnalytics"
# data(package="factorAnalytics")
# Remove factorAnalytics from search path
detach("package:factorAnalytics")
library(factorAnalytics)
# Fit a three-factor model using PCA
factpca <- fitSfm(rutils::etfenv$returns, k=3)
head(factpca$loadings, 3)  # Factor loadings
# Factor realizations (time series)
head(factpca$factors)
# Residuals from regression
factpca$residuals[1:3, 1:3]
library(factorAnalytics)
factpca$alpha  # Estimated alphas
factpca$r2  # R-squared regression
# Covariance matrix estimated by factor model
factpca$Omega[1:3, 4:6]
library(factorAnalytics)
# load(file="/Users/jerzy/Develop/lecture_slides/data/portf_optim.RData")
plot(factpca, which.plot.group=3, n.max=30, loop=FALSE)
# ?plot.sfm
library(PortfolioAnalytics)
# Plot factor cumulative returns
chart.CumReturns(factpca$factors,
    lwd=2, ylab="", legend.loc="topleft", main="")
# Plot time series of factor returns
# Plot(factpca, which.plot.group=2,
#   loop=FALSE)
# Asset correlations "hclust" hierarchical clustering order
plot(factpca, which.plot.group=7, loop=FALSE,
     order="hclust", method="ellipse")
library(PortfolioAnalytics)
# Plot residual cumulative returns
chart.CumReturns(factpca$residuals[, c("IEF", "DBC", "XLF")],
  lwd=2, ylab="", legend.loc="topleft", main="")
library(PortfolioAnalytics)
# Plot residual histogram with normal curve
plot(factpca, asset.name="VTI",
     which.plot.single=8,
     plot.single=TRUE, loop=FALSE,
     xlim=c(-0.007, 0.007))
library(PortfolioAnalytics)
# Residual Q-Q plot
plot(factpca, asset.name="VTI",
     which.plot.single=9,
     plot.single=TRUE, loop=FALSE)
# SACF and PACF of residuals
plot(factpca, asset.name="VTI",
     which.plot.single=5,
     plot.single=TRUE, loop=FALSE)
