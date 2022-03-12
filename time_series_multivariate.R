# Perform regression using formula
model <- lm(XLP ~ VTI, data=rutils::etfenv$returns)
# Get regression coefficients
coef(summary(model))
# Get alpha and beta
coef(summary(model))[, 1]
# Plot scatterplot of returns with aspect ratio 1
plot(formulav, data=rutils::etfenv$returns,
     xlim=c(-0.1, 0.1), ylim=c(-0.1, 0.1),
     asp=1, main="Regression XLP ~ VTI")
# Add regression line and perpendicular line
abline(model, lwd=2, col="red")
abline(a=0, b=-1/coef(summary(model))[2, 1],
 lwd=2, col="blue")
# Get regression coefficients
coef(summary(model))
# Calculate regression coefficients from scratch
design <- na.omit(rutils::etfenv$returns[, c("XLP", "VTI")])
betav <- drop(cov(design$XLP, design$VTI)/var(design$VTI))
alpha <- drop(mean(design$XLP) - betav*mean(design$VTI))
c(alpha, betav)
# Calculate the residuals
residuals <- (design$XLP - (alpha + betav*design$VTI))
# Calculate the standard deviation of residuals
nrows <- NROW(residuals)
resid_stddev <- sqrt(sum(residuals^2)/(nrows - 2))
# Calculate the standard errors of beta and alpha
sum2 <- sum((design$VTI - mean(design$VTI))^2)
betastderror <- resid_stddev/sqrt(sum2)
alpha_stderror <- resid_stddev*sqrt(1/nrows + mean(design$VTI)^2/sum2)
c(alpha_stderror, betastderror)
# Perform the Durbin-Watson test of autocorrelation of residuals
lmtest::dwtest(model)
library(rutils)  # Load rutils
returns <- rutils::etfenv$returns
symbolv <- colnames(returns)
symbolv <- symbolv[symbolv != "VTI"]
# Perform regressions and collect statistics
etf_betas <- sapply(symbolv, function(symbol) {
# Specify regression formula
  formulav <- as.formula(paste(symbol, "~ VTI"))
# Perform regression
  model <- lm(formulav, data=returns)
# Get regression summary
  model_sum <- summary(model)
# Collect regression statistics
  with(model_sum, 
    c(beta=coefficients[2, 1], 
p_beta=coefficients[2, 4],
alpha=coefficients[1, 1], 
p_alpha=coefficients[1, 4], 
p_dw=lmtest::dwtest(model)$p.value))
})  # end sapply
etf_betas <- t(etf_betas)
# Sort by p_alpha
etf_betas <- etf_betas[order(etf_betas[, "p_alpha"]), ]
etf_betas
library(PerformanceAnalytics)
# Calculate XLP beta
CAPM.beta(Ra=returns[, "XLP"], Rb=returns[, "VTI"])
# Calculate XLP bull beta
CAPM.beta.bull(Ra=returns[, "XLP"], Rb=returns[, "VTI"])
# Calculate XLP bear beta
CAPM.beta.bear(Ra=returns[, "XLP"], Rb=returns[, "VTI"])
# Calculate XLP alpha
CAPM.alpha(Ra=returns[, "XLP"], Rb=returns[, "VTI"])
library(PerformanceAnalytics)
etf_betas <- sapply(returns[, colnames(returns)!="VXX"],
  CAPM.beta, Rb=returns[, "VTI"])
etf_annrets <- sapply(returns[, colnames(returns)!="VXX"],
  Return.annualized)
# Plot scatterplot
plot(etf_annrets ~ etf_betas, xlab="betas",
      ylab="ann. rets", xlim=c(-0.25, 1.6))
points(x=1, y=etf_annrets["VTI"], col="red", lwd=3, pch=21)
abline(a=0, b=etf_annrets["VTI"])
label_names <- rownames(etf_betas)[1:13]
# Add labels
text(x=1, y=etf_annrets["VTI"], labels="VTI", pos=2)
text(x=etf_betas[label_names], y=etf_annrets[label_names],
     labels=label_names, pos=2, cex=0.8)
library(PerformanceAnalytics)
etf_betas <- sapply(
  returns[, colnames(returns)!="VXX"],
  CAPM.beta, Rb=returns[, "VTI"])
etf_annrets <- sapply(
  returns[, colnames(returns)!="VXX"],
  Return.annualized)
# Plot scatterplot
plot(etf_annrets ~ etf_betas, xlab="betas",
      ylab="ann. rets", xlim=c(-0.25, 1.6))
points(x=1, y=etf_annrets["VTI"], col="red",
 lwd=3, pch=21)
abline(a=0, b=etf_annrets["VTI"])
label_names <- rownames(etf_betas)[1:13]
# Add labels
text(x=1, y=etf_annrets["VTI"], labels="VTI",
     pos=2)
text(x=etf_betas[label_names],
     y=etf_annrets[label_names],
     labels=label_names, pos=2, cex=0.8)
library(PerformanceAnalytics)
# Calculate XLP Treynor ratio
TreynorRatio(Ra=returns[, "XLP"], Rb=returns[, "VTI"])
# Calculate XLP Information ratio
InformationRatio(Ra=returns[, "XLP"], Rb=returns[, "VTI"])
PerformanceAnalytics::table.CAPM(Ra=returns[, c("XLP", "XLF")], 
                           Rb=returns[, "VTI"], scale=252)
capmstats <- table.CAPM(Ra=returns[, symbolv],
        Rb=returns[, "VTI"], scale=252)
colnames <- strsplit(colnames(capmstats), split=" ")
colnames <- do.call(cbind, colnames)[1, ]
colnames(capmstats) <- colnames
capmstats <- t(capmstats)
capmstats <- capmstats[, -1]
colnames <- colnames(capmstats)
whichv <- match(c("Annualized Alpha", "Information Ratio", "Treynor Ratio"), colnames)
colnames[whichv] <- c("Alpha", "Information", "Treynor")
colnames(capmstats) <- colnames
capmstats <- capmstats[order(capmstats[, "Alpha"], decreasing=TRUE), ]
# Copy capmstats into etfenv and save to .RData file
etfenv <- rutils::etfenv
etfenv$capmstats <- capmstats
save(etfenv, file="/Users/jerzy/Develop/lecture_slides/data/etf_data.RData")
rutils::etfenv$capmstats[, c("Beta", "Alpha", "Information", "Treynor")]
# Calculate XLP and VTI returns
returns <- na.omit(rutils::etfenv$returns[, c("XLP", "VTI")])
# Calculate monthly end points
endp <- xts::endpoints(returns, on="months")[-1]
# Calculate start points from look-back interval
look_back <- 12  # Look back 12 months
startp <- c(rep(1, look_back), endp[1:(NROW(endp)-look_back)])
head(cbind(endp, startp), look_back+2)
# Calculate rolling beta regressions every month in R
formulav <- XLP ~ VTI  # Specify regression formula
betas_r <- sapply(1:NROW(endp), FUN=function(ep) {
    datav <- returns[startp[ep]:endp[ep], ]
    # coef(lm(formulav, data=datav))[2]
    drop(cov(datav[, 1], datav[, 2])/var(datav[, 2]))
  })  # end sapply
# Calculate rolling betas using RcppArmadillo
reg_stats <- HighFreq::roll_reg(response=returns[, 1], design=returns[, 2], endp=(endp-1), startp=(startp-1))
betas <- reg_stats[, 2]
all.equal(betas, betas_r)
# Compare the speed of RcppArmadillo with R code
library(microbenchmark)
summary(microbenchmark(
  Rcpp=HighFreq::roll_reg(response=returns[, 1], design=returns[, 2], endp=(endp-1), startp=(startp-1)),
  Rcode=sapply(1:NROW(endp), FUN=function(ep) {
    datav <- returns[startp[ep]:endp[ep], ]
    drop(cov(datav[, 1], datav[, 2])/var(datav[, 2]))
  }),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# dygraph plot of rolling XLP beta and VTI prices
dates <- zoo::index(returns[endp, ])
prices <- rutils::etfenv$prices$VTI[dates]
datav <- cbind(prices, betas)
colnames <- colnames(datav)
dygraphs::dygraph(datav, main="XLP Rolling 12-month Beta and VTI Prices") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", col="blue") %>%
  dySeries(name=colnames[2], axis="y2", col="red", strokeWidth=3) %>%
  dyLegend(show="always", width=500)
# Calculate XLB and XLE prices
prices <- na.omit(rutils::etfenv$prices[, c("XLB", "XLE")])
cor(rutils::diffit(log(prices)))
xl_b <- drop(zoo::coredata(prices$XLB))
xl_e <- drop(zoo::coredata(prices$XLE))
# Calculate regression coefficients of XLB ~ XLE
betav <- cov(xl_b, xl_e)/var(xl_e)
alpha <- (mean(xl_b) - betav*mean(xl_e))
# Calculate regression residuals
fit_ted <- (alpha + betav*xl_e)
residuals <- (xl_b - fit_ted)
# Perform ADF test on residuals
tseries::adf.test(residuals, k=1)
# Plot prices
dygraphs::dygraph(prices, main="XLB and XLE Prices") %>%
  dyOptions(colors=c("blue", "red"))
# Plot cointegration residuals
residuals <- xts::xts(residuals, index(prices))
dygraphs::dygraph(residuals, main="XLB and XLE Cointegration Residuals")
# Load S&P500 constituent stock prices
load("/Users/jerzy/Develop/lecture_slides/data/sp500_prices.RData")
# Calculate stock prices and percentage returns
prices <- zoo::na.locf(prices, na.rm=FALSE)
prices <- zoo::na.locf(prices, fromLast=TRUE)
returns <- rutils::diffit(log(prices))
# Standardize (de-mean and scale) the returns
returns <- lapply(returns, function(x) {(x - mean(x))/sd(x)})
returns <- rutils::do_call(cbind, returns)
# Perform principal component analysis PCA
pcad <- prcomp(returns, scale=TRUE)
# Find number of components with variance greater than 2
n_comp <- which(pcad$sdev^2 < 2)[1]
# Plot standard deviations of principal components
barplot(pcad$sdev[1:n_comp],
  names.arg=colnames(pcad$rotation[, 1:n_comp]),
  las=3, xlab="", ylab="",
  main="Volatilities of S&P500 Principal Components")
# Calculate principal component loadings (weights)
# Plot barplots with PCA weights in multiple panels
n_comps <- 6
par(mfrow=c(n_comps/2, 2))
par(mar=c(4, 2, 2, 1), oma=c(0, 0, 0, 0))
# First principal component weights
weightv <- sort(pcad$rotation[, 1], decreasing=TRUE)
barplot(weightv[1:6], las=3, xlab="", ylab="", main="")
title(paste0("PC", 1), line=-2.0, col.main="red")
for (ordern in 2:n_comps) {
  weightv <- sort(pcad$rotation[, ordern], decreasing=TRUE)
  barplot(weightv[c(1:3, 498:500)], las=3, xlab="", ylab="", main="")
  title(paste0("PC", ordern), line=-2.0, col.main="red")
}  # end for
# Calculate principal component time series
pcarets <- xts(returns %*% pcad$rotation[, 1:n_comps],
          order.by=dates)
round(cov(pcarets), 3)
pcats <- cumsum(pcarets)
# Plot principal component time series in multiple panels
par(mfrow=c(n_comps/2, 2))
par(mar=c(2, 2, 0, 1), oma=c(0, 0, 0, 0))
rangev <- range(pcats)
for (ordern in 1:n_comps) {
  plot.zoo(pcats[, ordern], ylim=rangev, xlab="", ylab="")
  title(paste0("PC", ordern), line=-2.0)
}  # end for
par(mfrow=c(n_comps/2, 2))
par(mar=c(2, 2, 0, 1), oma=c(0, 0, 0, 0))
# Invert principal component time series
inv_rotation <- solve(pcad$rotation)
all.equal(inv_rotation, t(pcad$rotation))
solved <- pcarets %*% inv_rotation[1:n_comps, ]
solved <- xts::xts(solved, dates)
solved <- cumsum(solved)
cum_returns <- cumsum(returns)
# Plot the solved returns
symbols <- c("MSFT", "XOM", "JPM", "AAPL", "BRK_B", "JNJ")
for (symbol in symbols) {
  plot.zoo(cbind(cum_returns[, symbol], solved[, symbol]),
    plot.type="single", col=c("black", "blue"), xlab="", ylab="")
  legend(x="topleft", bty="n",
   legend=paste0(symbol, c("", " solved")),
   title=NULL, inset=0.05, cex=1.0, lwd=6,
   lty=1, col=c("black", "blue"))
}  # end for
par(mfrow=c(n_comps/2, 2))
par(mar=c(2, 2, 0, 1), oma=c(0, 0, 0, 0))
# Perform ADF unit root tests on original series and residuals
sapply(symbols, function(symbol) {
  c(series=tseries::adf.test(cum_returns[, symbol])$p.value,
    resid=tseries::adf.test(cum_returns[, symbol] - solved[, symbol])$p.value)
})  # end sapply
# Plot the residuals
for (symbol in symbols) {
  plot.zoo(cum_returns[, symbol] - solved[, symbol],
    plot.type="single", col="blue", xlab="", ylab="")
  legend(x="topleft", bty="n", legend=paste(symbol, "residuals"),
   title=NULL, inset=0.05, cex=1.0, lwd=6, lty=1, col="blue")
}  # end for
# Perform ADF unit root test on principal component time series
pcarets <- xts(returns %*% pcad$rotation, order.by=dates)
pcats <- cumsum(pcarets)
adf_pvalues <- sapply(1:NCOL(pcats), function(ordern)
  tseries::adf.test(pcats[, ordern])$p.value)
# AdF unit root test on stationary time series
tseries::adf.test(rnorm(1e5))
library(quantmod)
#Perform pair-wise correlation analysis
# Calculate correlation matrix
cormat <- cor(returns)
colnames(cormat) <- colnames(returns)
rownames(cormat) <- colnames(returns)
# Reorder correlation matrix based on clusters
# Calculate permutation vector
library(corrplot)
ordern <- corrMatOrder(cormat, order="hclust",
        hclust.method="complete")
# Apply permutation vector
cormat <- cormat[ordern, ordern]
# Plot the correlation matrix
colors <- colorRampPalette(c("red", "white", "blue"))
corrplot(cormat, tl.col="black", tl.cex=0.8,
    method="square", col=colors(8),
    cl.offset=0.75, cl.cex=0.7,
    cl.align.text="l", cl.ratio=0.25)
# draw rectangles on the correlation matrix plot
corrRect.hclust(cormat, k=NROW(cormat) %/% 2,
          method="complete", col="red")
# Convert correlation matrix into distance object
dis_tance <- as.dist(1-cormat)
# Perform hierarchical clustering analysis
cluster <- hclust(dis_tance)
plot(cluster, ann=FALSE, xlab="", ylab="")
title("Dendrogram representing hierarchical clustering
\nwith dissimilarity = 1-correlation", line=-0.5)
library(PerformanceAnalytics)  # Load package "PerformanceAnalytics"
# PC returns from rotation and scaled returns
returns_scaled <- apply(returns, 2, scale)
pcarets <- returns_scaled %*% pcad$rotation
# "x" matrix contains time series of PC returns
dim(pcad$x)
class(pcad$x)
head(pcad$x[, 1:3], 3)
# Convert PC matrix to xts and rescale to decimals
pcarets <- xts(pcad$x/100,
    order.by=index(returns))
library(PerformanceAnalytics)  # Load package "PerformanceAnalytics"
chart.CumReturns(
  pcarets[, 1:3], lwd=2, ylab="",
  legend.loc="topright", main="")
# Add title
title(main="ETF cumulative returns", line=-1)
library(PerformanceAnalytics)
# Calculate PC correlation matrix
cormat <- cor(pcarets)
colnames(cormat) <- colnames(pcarets)
rownames(cormat) <- colnames(pcarets)
cormat[1:3, 1:3]
table.CAPM(Ra=pcarets[, 1:3],
    Rb=returns[, "VTI"], scale=252)
par(oma=c(1,0,1,0), mgp=c(2,1,0), mar=c(2,1,2,1), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # Set plot panels
#Perform principal component analysis PCA
returns <- na.omit(rutils::etfenv$returns)
pcad <- prcomp(returns, center=TRUE, scale=TRUE)
barplot(pcad$sdev[1:10],
  names.arg=colnames(pcad$rotation)[1:10],
  las=3, ylab="STDEV", xlab="PCVec",
  main="PCA Explain VAR")
# Show first three principal component loadings
head(pcad$rotation[,1:3], 3)
# Permute second principal component loadings by size
pca_vec2 <- as.matrix(
  pcad$rotation[order(pcad$rotation[, 2],
  decreasing=TRUE), 2])
colnames(pca_vec2) <- "pca2"
head(pca_vec2, 3)
# The option las=3 rotates the names.arg labels
barplot(as.vector(pca_vec2),
  names.arg=rownames(pca_vec2),
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
factor_pca <- fitSfm(rutils::etfenv$returns, k=3)
head(factor_pca$loadings, 3)  # Factor loadings
# Factor realizations (time series)
head(factor_pca$factors)
# Residuals from regression
factor_pca$residuals[1:3, 1:3]
library(factorAnalytics)
factor_pca$alpha  # Estimated alphas
factor_pca$r2  # R-squared regression
# Covariance matrix estimated by factor model
factor_pca$Omega[1:3, 4:6]
library(factorAnalytics)
# load(file="/Users/jerzy/Develop/lecture_slides/data/portf_optim.RData")
plot(factor_pca, which.plot.group=3,
     n.max=30, loop=FALSE)
# ?plot.sfm
library(PortfolioAnalytics)
# Plot factor cumulative returns
chart.CumReturns(factor_pca$factors,
    lwd=2, ylab="", legend.loc="topleft",
    main="")
# Plot time series of factor returns
# Plot(factor_pca, which.plot.group=2,
#   loop=FALSE)
# Asset correlations "hclust" hierarchical clustering order
plot(factor_pca, which.plot.group=7,
     loop=FALSE, order="hclust",
     method="ellipse")
library(PortfolioAnalytics)
# Plot residual cumulative returns
chart.CumReturns(
  factor_pca$residuals[, c("IEF",
            "DBC", "XLF")],
  lwd=2, ylab="", legend.loc="topleft",
  main="")
library(PortfolioAnalytics)
# Plot residual histogram with normal curve
plot(factor_pca, asset.name="VTI",
     which.plot.single=8,
     plot.single=TRUE, loop=FALSE,
     xlim=c(-0.007, 0.007))
library(PortfolioAnalytics)
# Residual Q-Q plot
plot(factor_pca, asset.name="VTI",
     which.plot.single=9,
     plot.single=TRUE, loop=FALSE)
# SACF and PACF of residuals
plot(factor_pca, asset.name="VTI",
     which.plot.single=5,
     plot.single=TRUE, loop=FALSE)
