# Select ETF symbols
symbolv <- c("IEF", "DBC", "XLU", "XLF", "XLP", "XLI")
# Calculate ETF prices and log returns
pricev <- rutils::etfenv$prices[, symbolv]
# Applying zoo::na.locf() can produce bias of the correlations
# pricev <- zoo::na.locf(pricev, na.rm=FALSE)
# pricev <- zoo::na.locf(pricev, fromLast=TRUE)
pricev <- na.omit(pricev)
retp <- rutils::diffit(log(pricev))
# Calculate covariance matrix
covmat <- cov(retp)
# Standardize (de-mean and scale) the returns
retp <- lapply(retp, function(x) {(x - mean(x))/sd(x)})
retp <- rutils::do_call(cbind, retp)
round(sapply(retp, mean), 6)
sapply(retp, sd)
# Alternative (much slower) center (de-mean) and scale the returns
# retp <- apply(retp, 2, scale)
# retp <- xts::xts(retp, zoo::index(pricev))
# Alternative (much slower) center (de-mean) and scale the returns
# retp <- scale(retp, center=TRUE, scale=TRUE)
# retp <- xts::xts(retp, zoo::index(pricev))
# Alternative (much slower) center (de-mean) and scale the returns
# retp <- t(retp) - colMeans(retp)
# retp <- retp/sqrt(rowSums(retp^2)/(NCOL(retp)-1))
# retp <- t(retp)
# retp <- xts::xts(retp, zoo::index(pricev))
# Calculate correlation matrix
cormat <- cor(retp)
# Reorder correlation matrix based on clusters
library(corrplot)
ordern <- corrMatOrder(cormat, order="hclust",
  hclust.method="complete")
cormat <- cormat[ordern, ordern]
# Plot the correlation matrix
colorv <- colorRampPalette(c("red", "white", "blue"))
# x11(width=6, height=6)
corrplot(cormat, title=NA, tl.col="black", mar=c(0,0,0,0),
    method="square", col=colorv(NCOL(cormat)), tl.cex=0.8,
    cl.offset=0.75, cl.cex=0.7, cl.align.text="l", cl.ratio=0.25)
title("ETF Correlation Matrix", line=2)
# Draw rectangles on the correlation matrix plot
corrRect.hclust(cormat, k=NROW(cormat) %/% 2,
  method="complete", col="red")
# Create initial vector of portfolio weights
nweights <- NROW(symbolv)
weightv <- rep(1/sqrt(nweights), nweights)
names(weightv) <- symbolv
# Objective function equal to minus portfolio variance
objfun <- function(weightv, retp) {
  retp <- retp %*% weightv
  -sum(retp^2) + 1e4*(1 - sum(weightv^2))^2
}  # end objfun
# Objective for equal weight portfolio
objfun(weightv, retp)
# Compare speed of vector multiplication methods
summary(microbenchmark(
  transp=(t(retp[, 1]) %*% retp[, 1]),
  sumv=sum(retp[, 1]^2),
  times=10))[, c(1, 4, 5)]
# Find weights with maximum variance
optiml <- optim(par=weightv,
  fn=objfun,
  retp=retp,
  method="L-BFGS-B",
  upper=rep(10.0, nweights),
  lower=rep(-10.0, nweights))
# Optimal weights and maximum variance
weights1 <- optiml$par
-objfun(weights1, retp)
# Plot first principal component weights
barplot(weights1, names.arg=names(weights1), xlab="", ylab="",
  main="First Principal Component Weights")
# PC1 returns
pc1 <- drop(retp %*% weights1)
# Redefine objective function
objfun <- function(weightv, retp) {
  retp <- retp %*% weightv
  -sum(retp^2) + 1e4*(1 - sum(weightv^2))^2 +
    1e4*(sum(weights1*weightv))^2
}  # end objfun
# Find second PC weights using parallel DEoptim
optiml <- DEoptim::DEoptim(fn=objfun,
  upper=rep(10, NCOL(retp)),
  lower=rep(-10, NCOL(retp)),
  retp=retp, control=list(parVar="weights1",
    trace=FALSE, itermax=1000, parallelType=1))
# PC2 weights
weights2 <- optiml$optim$bestmem
names(weights2) <- colnames(retp)
sum(weights2^2)
sum(weights1*weights2)
# PC2 returns
pc2 <- drop(retp %*% weights2)
# Plot second principal component loadings
barplot(weights2, names.arg=names(weights2), xlab="", ylab="",
  main="Second Principal Component Loadings")
# Calculate the eigenvalues and eigenvectors
eigend <- eigen(cormat)
eigend$vectors
# Compare with optimization
all.equal(sum(diag(cormat)), sum(eigend$values))
all.equal(abs(eigend$vectors[, 1]), abs(weights1), check.attributes=FALSE)
all.equal(abs(eigend$vectors[, 2]), abs(weights2), check.attributes=FALSE)
all.equal(eigend$values[1], var(pc1), check.attributes=FALSE)
all.equal(eigend$values[2], var(pc2), check.attributes=FALSE)
# Eigenvalue equations
(cormat %*% weights1) / weights1 / var(pc1)
(cormat %*% weights2) / weights2 / var(pc2)
# Plot eigenvalues
barplot(eigend$values, names.arg=paste0("PC", 1:nweights),
  las=3, xlab="", ylab="", main="Principal Component Variances")
# Calculate the eigen decomposition of the correlation matrix
eigend <- eigen(cormat)
# Perform PCA with scaling
pcad <- prcomp(retp, scale=TRUE)
# Compare outputs
all.equal(eigend$values, pcad$sdev^2)
all.equal(abs(eigend$vectors), abs(pcad$rotation),
    check.attributes=FALSE)
# Eigen decomposition of covariance matrix
eigend <- eigen(covmat)
# Perform PCA without scaling
pcad <- prcomp(retp, scale=FALSE)
# Compare outputs
all.equal(eigend$values, pcad$sdev^2)
all.equal(abs(eigend$vectors), abs(pcad$rotation),
    check.attributes=FALSE)
# Redefine objective function to minimize variance
objfun <- function(weightv, retp) {
  retp <- retp %*% weightv
  sum(retp^2) + 1e4*(1 - sum(weightv^2))^2
}  # end objfun
# Find highest order PC weights using parallel DEoptim
optiml <- DEoptim::DEoptim(fn=objfun,
  upper=rep(10, NCOL(retp)),
  lower=rep(-10, NCOL(retp)),
  retp=retp, control=list(trace=FALSE,
    itermax=1000, parallelType=1))
# PC6 weights and returns
weights6 <- optiml$optim$bestmem
names(weights6) <- colnames(retp)
sum(weights6^2)
sum(weights1*weights6)
# Compare with eigend vector
weights6
eigend$vectors[, 6]
# Calculate objective function
objfun(weights6, retp)
objfun(eigend$vectors[, 6], retp)
# Plot highest order principal component loadings
weights6 <- eigend$vectors[, 6]
names(weights6) <- colnames(retp)
barplot(weights6, names.arg=names(weights6), xlab="", ylab="",
  main="Highest Order Principal Component Loadings")
# Perform principal component analysis PCA
pcad <- prcomp(retp, scale=TRUE)
# Plot standard deviations of principal components
barplot(pcad$sdev, names.arg=colnames(pcad$rotation),
  las=3, xlab="", ylab="",
  main="Scree Plot: Volatilities of Principal Components \n of ETF Returns")
# Calculate the number of principal components which sum up to at least 80% of the total variance
pcavar <- pcad$sdev^2
which(cumsum(pcavar)/sum(pcavar) > 0.8)[1]
# Plot barplots with PCA loadings (weights) in multiple panels
pcad$rotation
# x11(width=6, height=7)
par(mfrow=c(nweights/2, 2))
par(mar=c(3, 2, 2, 1), oma=c(0, 0, 0, 0))
for (ordern in 1:nweights) {
  barplot(pcad$rotation[, ordern], las=3, xlab="", ylab="", main="")
  title(paste0("PC", ordern), line=-1, col.main="red")
}  # end for
# Calculate products of principal component time series
round(t(pcad$x) %*% pcad$x, 2)
# Calculate principal component time series from returns
datev <- zoo::index(pricev)
retpca <- xts::xts(retp %*% pcad$rotation, order.by=datev)
round(cov(retpca), 3)
all.equal(coredata(retpca), pcad$x, check.attributes=FALSE)
pcacum <- cumsum(retpca)
# Plot principal component time series in multiple panels
rangev <- range(pcacum)
for (ordern in 1:nweights) {
  plot.zoo(pcacum[, ordern], ylim=rangev, xlab="", ylab="")
  title(paste0("PC", ordern), line=-1, col.main="red")
}  # end for
# Invert all the principal component time series
retpca <- retp %*% pcad$rotation
solved <- retpca %*% solve(pcad$rotation)
all.equal(coredata(retp), solved)
# Invert first 3 principal component time series
solved <- retpca[, 1:3] %*% solve(pcad$rotation)[1:3, ]
solved <- xts::xts(solved, datev)
solved <- cumsum(solved)
retc <- cumsum(retp)
# Plot the solved returns
for (symbol in symbolv) {
  plot.zoo(cbind(retc[, symbol], solved[, symbol]),
    plot.type="single", col=c("black", "blue"), xlab="", ylab="")
  legend(x="topleft", bty="n", legend=paste0(symbol, c("", " solved")), y.intersp=0.4,
   title=NULL, inset=0.0, cex=1.0, lwd=6, lty=1, col=c("black", "blue"))
}  # end for
# Create a matrix with low correlation
ndata <- 10
cormat <- matrix(rep(0.1, ndata^2), nc=ndata)
diag(cormat) <- rep(1, ndata)
# Calculate the condition number
eigend <- eigen(cormat)
eigenval <- eigend$values
max(eigenval)/min(eigenval)
# Create a matrix with high correlation
cormat <- matrix(rep(0.9, ndata^2), nc=ndata)
diag(cormat) <- rep(1, ndata)
# Calculate the condition number
eigend <- eigen(cormat)
eigenval <- eigend$values
max(eigenval)/min(eigenval)
# Calculate the condition numbers as function correlation
corvec <- seq(0.1, 0.9, 0.1)
condvec <- sapply(corvec, function(corv) {
  cormat <- matrix(rep(corv, ndata^2), nc=ndata)
  diag(cormat) <- rep(1, ndata)
  eigend <- eigen(cormat)
  eigenval <- eigend$values
  max(eigenval)/min(eigenval)
})  # end sapply
# Plot the condition numbers
plot(x=corvec, y=condvec, t="l",
  main="Condition Number as Function of Correlation",
  xlab="correlation", ylab="condition number")
# Simulate uncorrelated stock returns
nstocks <- 10
nrows <- 100
set.seed(1121)  # Initialize random number generator
retp <- matrix(rnorm(nstocks*nrows), nc=nstocks)
# Calculate the condition numbers as function of number of observations
obsvec <- seq(20, nrows, 10)
condvec <- sapply(obsvec, function(nobs) {
  cormat <- cor(retp[1:nobs, ])
  eigend <- eigen(cormat)
  eigenval <- eigend$values
  max(eigenval)/min(eigenval)
})  # end sapply
# Plot the condition numbers
plot(x=obsvec, y=condvec, t="l",
  main="Condition Number as Function of Number of Observations",
  xlab="number of observations", ylab="condition number")
# Load daily S&P500 log percentage stock returns
load(file="/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
# Calculate the number of NA values in returns
retp <- returns
colSums(is.na(retp))
# Calculate the correlations ignoring NA values
cor(retp$DAL, retp$FOXA, use="pairwise.complete.obs")
cor(na.omit(retp[, c("DAL", "FOXA")]))[2]
cormat <- cor(retp, use="pairwise.complete.obs")
sum(is.na(cormat))
cormat[is.na(cormat)] <- 0
# Perform principal component analysis PCA - produces error
pcad <- prcomp(retp, scale=TRUE)
# Calculate the eigen decomposition of the correlation matrix
eigend <- eigen(cormat)
# Calculate the eigenvalues and eigenvectors
eigenval <- eigend$values
eigenvec <- eigend$vectors
# Calculate the number of negative eigenvalues
sum(eigenval<0)
# Calculate the condition number
max(eigenval)/min(abs(eigenval))
# Calculate the number of eigenvalues which sum up to at least 80% of the total variance
which(cumsum(eigenval)/sum(eigenval) > 0.8)[1]
# Plot the eigenvalues
barplot(eigenval, xlab="", ylab="", las=3,
  names.arg=paste0("ev", 1:NROW(eigenval)),
  main="Eigenvalues of Stock Correlation Matrix")
# Calculate the stock variance
varv <- sapply(retp, var, na.rm=TRUE)
# Calculate the returns of low and high volatility stocks
nstocks <- NCOL(retp)
medianv <- median(varv)
retlow <- retp[, varv <= medianv]
rethigh <- retp[, varv > medianv]
# Calculate the correlations of low volatility stocks
cormat <- cor(retlow, use="pairwise.complete.obs")
cormat[is.na(cormat)] <- 0
# Calculate the mean correlations
mean(cormat[upper.tri(cormat)])
# Calculate the eigen decomposition of the correlation matrix
eigend <- eigen(cormat)
eigenval <- eigend$values
# Calculate the number of negative eigenvalues
sum(eigenval < 0)
# Calculate the number of eigenvalues which sum up to at least 80% of the total variance
which(cumsum(eigenval)/sum(eigenval) > 0.8)[1]
# Calculate the condition number
max(eigenval)/min(abs(eigenval))
# Calculate the correlations of high volatility stocks
cormat <- cor(rethigh, use="pairwise.complete.obs")
cormat[is.na(cormat)] <- 0
# Calculate the mean correlations
mean(cormat[upper.tri(cormat)])
# Calculate the eigen decomposition of the correlation matrix
eigend <- eigen(cormat)
eigenval <- eigend$values
# Calculate the number of negative eigenvalues
sum(eigenval < 0)
# Calculate the number of eigenvalues which sum up to at least 80% of the total variance
which(cumsum(eigenval)/sum(eigenval) > 0.8)[1]
# Calculate the condition number
max(eigenval)/min(abs(eigenval))
# Subset (select) the stock returns after the start date of VTI
retvti <- na.omit(rutils::etfenv$returns$VTI)
colnames(retvti) <- "VTI"
retp <- returns[zoo::index(retvti)]
datev <- zoo::index(retp)
retvti <- retvti[datev]
nrows <- NROW(retp)
nstocks <- NCOL(retp)
head(retp[, 1:5])
# Calculate the monthly end points
endd <- rutils::calc_endpoints(retvti, interval="months")
retvti[head(endd)]
retvti[tail(endd)]
# Remove stub interval at the end
endd <- endd[-NROW(endd)]
npts <- NROW(endd)
# Calculate the monthly stock volatilities and correlations
stdcor <- sapply(2:npts, function(endp) {
  # cat("endp = ", endp, "\n")
  retp <- retp[endd[endp-1]:endd[endp]]
  cormat <- cor(retp, use="pairwise.complete.obs")
  cormat[is.na(cormat)] <- 0
  c(std=sd(retvti[endd[endp-1]:endd[endp]]),
    cor=mean(cormat[upper.tri(cormat)]))
})  # end sapply
stdcor <- t(stdcor)
# Scatterplot of stock volatilities and correlations
plot(x=stdcor[, "std"], y=stdcor[, "cor"],
 xlab="volatility", ylab="correlation",
 main="Monthly Stock Volatilities and Correlations")
# Plot stock volatilities and correlations
colnamev <- colnames(stdcor)
stdcor <- xts(stdcor, zoo::index(retvti[endd]))
dygraphs::dygraph(stdcor,
  main="Monthly Stock Volatilities and Correlations") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)
# Calculate the median VTI volatility
medianv <- median(stdcor[, "std"])
# Calculate the stock returns of low volatility intervals
retlow <- lapply(2:npts, function(endp) {
  if (stdcor[endp-1, "std"] <= medianv)
    retp[endd[endp-1]:endd[endp]]
})  # end lapply
retlow <- rutils::do_call(rbind, retlow)
# Calculate the stock returns of high volatility intervals
rethigh <- lapply(2:npts, function(endp) {
  if (stdcor[endp-1, "std"] > medianv)
    retp[endd[endp-1]:endd[endp]]
})  # end lapply
rethigh <- rutils::do_call(rbind, rethigh)
# Calculate the correlations of low volatility intervals
cormat <- cor(retlow, use="pairwise.complete.obs")
cormat[is.na(cormat)] <- 0
mean(cormat[upper.tri(cormat)])
# Calculate the eigen decomposition of the correlation matrix
eigend <- eigen(cormat)
eigenval <- eigend$values
sum(eigenval < 0)
# Calculate the number of eigenvalues which sum up to at least 80% of the total variance
which(cumsum(eigenval)/sum(eigenval) > 0.8)[1]
# Calculate the condition number
max(eigenval)/min(abs(eigenval))
# Calculate the correlations of high volatility intervals
cormat <- cor(rethigh, use="pairwise.complete.obs")
cormat[is.na(cormat)] <- 0
mean(cormat[upper.tri(cormat)])
# Calculate the eigen decomposition of the correlation matrix
eigend <- eigen(cormat)
eigenval <- eigend$values
sum(eigenval < 0)
# Calculate the number of eigenvalues which sum up to at least 80% of the total variance
which(cumsum(eigenval)/sum(eigenval) > 0.8)[1]
# Calculate the condition number
max(eigenval)/min(abs(eigenval))
# Calculate AAPL and XLK returns
retp <- na.omit(cbind(returns$AAPL, rutils::etfenv$returns$XLK))
# Calculate the trailing correlations
lambda <- 0.99
covarv <- HighFreq::run_covar(retp, lambda)
correlv <- covarv[, 1, drop=FALSE]/sqrt(covarv[, 2]*covarv[, 3])
# Plot dygraph of XLK returns and AAPL correlations
datav <- cbind(cumsum(retp$XLK), correlv)
colnames(datav)[2] <- "correlation"
colnamev <- colnames(datav)
endd <- rutils::calc_endpoints(retp, interval="weeks")
dygraphs::dygraph(datav[endd], main="AAPL Correlations With XLK") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)
# Scatterplot of trailing stock volatilities and correlations
volv <- sqrt(covarv[, 2])
plot(x=volv[endd], y=correlv[endd, ], pch=1, col="blue",
 xlab="AAPL volatility", ylab="Correlation",
 main="Trailing Volatilities and Correlations of AAPL vs XLK")
# Interactive scatterplot of trailing stock volatilities and correlations
datev <- zoo::index(retp[endd])
datav <- data.frame(datev, volv[endd], correlv[endd, ])
colnames(datav) <- c("date", "volatility", "correlation")
library(plotly)
plotly::plot_ly(data=datav, x=~volatility, y=~correlation,
  type="scatter", mode="markers", text=datev) %>%
  layout(title="Trailing Volatilities and Correlations of AAPL vs XLK")
# Plot trailing stock volatilities and correlations
datav <- xts(cbind(volv, correlv), zoo::index(retp))
colnames(datav) <- c("volatility", "correlation")
colnamev <- colnames(datav)
dygraphs::dygraph(datav[endd], main="AAPL Trailing Stock Volatility and Correlation") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)
# Calculate portfolio returns
retvti <- na.omit(rutils::etfenv$returns$VTI)
colnames(retvti) <- "VTI"
datev <- zoo::index(retvti)
retp <- returns100
retp[is.na(retp)] <- 0
retp <- retp[datev]
nrows <- NROW(retp)
nstocks <- NCOL(retp)
head(retp[, 1:5])
# Calculate the average trailing portfolio correlations
lambda <- 0.9
correlv <- sapply(retp, function(retp) {
  covarv <- HighFreq::run_covar(cbind(retvti, retp), lambda)
  covarv[, 1, drop=FALSE]/sqrt(covarv[, 2]*covarv[, 3])
})  # end sapply
correlv[is.na(correlv)] <- 0
correlp <- rowMeans(correlv)
# Scatterplot of trailing stock volatilities and correlations
volvti <- sqrt(HighFreq::run_var(retvti, lambda))
endd <- rutils::calc_endpoints(retvti, interval="weeks")
plot(x=volvti[endd], y=correlp[endd],
 xlab="volatility", ylab="correlation",
 main="Trailing Stock Volatilities and Correlations")
# Plot trailing stock volatilities and correlations
datav <- xts(cbind(volvti, correlp), datev)
colnames(datav) <- c("volatility", "correlation")
colnamev <- colnames(datav)
dygraphs::dygraph(datav[endd],
  main="Trailing Stock Volatilities and Correlations") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)
# Calculate the VTI daily percentage returns
retp <- na.omit(rutils::etfenv$returns$VTI)
# Calculate the autocorrelations of VTI daily returns
rutils::plot_acf(retp)
# Simulate mean reverting strategy
posv <- rutils::lagit(sign(retp), lagg=1)
pnls <- (-retp*posv)
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of mean reverting strategy
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="VTI Daily Mean Reverting Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Simulate mean reverting strategy with two day holding period
posv <- rutils::lagit(rutils::roll_sum(sign(retp), look_back=2))/2
pnls <- (-retp*posv)
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of mean reverting strategy
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Daily Mean Reverting Strategy With Two Day Holding Period") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Load daily S&P500 stock returns
load(file="/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
retp <- na.omit(returns$MSFT)
rutils::plot_acf(retp)
# Simulate mean reverting strategy with two day holding period
posv <- rutils::lagit(rutils::roll_sum(sign(retp), look_back=2))/2
pnls <- (-retp*posv)
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("MSFT", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of mean reverting strategy
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Daily Mean Reverting Strategy For MSFT") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Calculate the average returns of all S&P500 stocks
datev <- zoo::index(returns)
retp <- returns
retp[is.na(retp)] <- 0
retp <- rowMeans(retp)
# Simulate mean reverting strategy for all S&P500 stocks
pnls <- lapply(returns, function(retp) {
  retp <- na.omit(retp)
  posv <- rutils::roll_sum(sign(retp), look_back=2)/2
  posv <- rutils::lagit(posv)
  pnls <- (-retp*posv)
  pnls
}) # end lapply
pnls <- do.call(cbind, pnls)
pnls[is.na(pnls)] <- 0
pnls <- rowMeans(pnls)
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retp, pnls)
wealthv <- xts::xts(wealthv, datev)
colnames(wealthv) <- c("All Stocks", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of mean reverting strategy
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Daily Mean Reverting Strategy For All Stocks") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Calculate the VTI daily percentage returns
retp <- na.omit(rutils::etfenv$returns$VTI)
nrows <- NROW(retp)
respv <- retp
# Define the response and predictor matrices
orderp <- 5
predm <- lapply(1:orderp, rutils::lagit, input=respv)
predm <- rutils::do_call(cbind, predm)
predm <- cbind(rep(1, nrows), predm)
colnames(predm) <- paste0("pred", 1:NCOL(predm))
# Calculate the fitted autoregressive coefficients
predinv <- MASS::ginv(predm)
coeff <- predinv %*% respv
# Calculate the in-sample forecasts of VTI (fitted values)
fcast <- predm %*% coeff
range(fcast)
# Calculate the residuals (forecast errors)
resids <- (fcast - retp)
# The residuals are orthogonal to the forecasts
cor(resids, fcast)
# Calculate the variance of the residuals
vares <- sum(resids^2)/(nrows-NROW(coeff))
# Calculate the predictor matrix squared
predm2 <- crossprod(predm)
# Calculate the covariance matrix of the AR coefficients
covar <- vares*MASS::ginv(predm2)
coeffsd <- sqrt(diag(covar))
# Calculate the t-values of the AR coefficients
coefft <- drop(coeff/coeffsd)
# Plot the t-values of the AR coefficients
lagv <- paste0("lag=", 0:5)
barplot(coefft ~ lagv, xlab="", ylab="t-value",
  main="Coefficient t-values of AR Forecasting Model")
# Calculate the trailing volatility of the residuals
residv <- sqrt(HighFreq::run_var(resids, lambda=0.9))
# Plot dygraph of volatility of residuals
datav <- cbind(cumsum(retp), residv)
colnames(datav) <- c("VTI", "residual vol")
endd <- rutils::calc_endpoints(datav, interval="weeks")
dygraphs::dygraph(datav[endd], main="Volatility of Residuals") %>%
  dyAxis("y", label="VTI", independentTicks=TRUE) %>%
  dyAxis("y2", label="residual vol", independentTicks=TRUE) %>%
  dySeries(name="VTI", axis="y", label="VTI", strokeWidth=2, col="blue") %>%
  dySeries(name="residual vol", axis="y2", label="residual vol", strokeWidth=2, col="red")
# Simulate autoregressive strategy in-sample
pnls <- retp*fcast
# Scale the PnL volatility to that of VTI
pnls <- pnls*sd(retp)/sd(pnls)
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of autoregressive strategy
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Autoregressive Strategy In-Sample") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Calculate the high volatility AR coefficients
respv <- retp["2008/2011"]
predm <- lapply(1:orderp, rutils::lagit, input=respv)
predm <- rutils::do_call(cbind, predm)
predm <- cbind(rep(1, NROW(predm)), predm)
predinv <- MASS::ginv(predm)
coeffh <- drop(predinv %*% respv)
lagv <- paste0("lag=", 0:5)
barplot(coeffh ~ lagv, main="High Volatility AR Coefficients",
  xlab="", ylab="coefficient", ylim=c(-0.1, 0.05))
# Calculate the low volatility AR coefficients
respv <- retp["2012/2019"]
predm <- lapply(1:orderp, rutils::lagit, input=respv)
predm <- rutils::do_call(cbind, predm)
predm <- cbind(rep(1, NROW(predm)), predm)
predinv <- MASS::ginv(predm)
coeffl <- drop(predinv %*% respv)
barplot(coeffl ~ lagv, main="Low Volatility AR Coefficients",
  xlab="", ylab="coefficient", ylim=c(-0.1, 0.05))
NA
lambdav <- c(0.5, 1, 1.5)
colorv <- c("red", "blue", "green")
# Define the winsor function
winsorfun <- function(retp, lambda) tanh(lambda*retp)
# Plot three curves in loop
for (indeks in 1:3) {
  curve(expr=winsorfun(x, lambda=lambdav[indeks]),
xlim=c(-4, 4), type="l", lwd=4,
xlab="model weight", ylab="dollar amount",
col=colorv[indeks], add=(indeks>1))
}  # end for
# Add title and legend
title(main="Winsor function", line=0.5)
legend("topleft", title="scale parameters\n",
   paste("lambda", lambdav, sep="="), inset=0.0, cex=1.0,
   lwd=6, bty="n", y.intersp=0.3, lty=1, col=colorv)
# Winsorize the VTI returns
retw <- winsorfun(retp/0.01, lambda=0.1)
# Define the response and predictor matrices
predm <- lapply(1:orderp, rutils::lagit, input=retw)
predm <- rutils::do_call(cbind, predm)
predm <- cbind(rep(1, nrows), predm)
colnames(predm) <- paste0("pred", 1:NCOL(predm))
predinv <- MASS::ginv(predm)
coeff <- predinv %*% retw
# Calculate the in-sample forecasts of VTI
fcast <- predm %*% coeff
# Winsorize the forecasts
# fcast <- winsorfun(fcast/mad(fcast), lambda=1.5)
# Simulate autoregressive strategy in-sample
pnls <- retp*fcast
# Scale the PnL volatility to that of VTI
pnls <- pnls*sd(retp)/sd(pnls)
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of autoregressive strategy
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Winsorized Autoregressive Strategy In-Sample") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Scale the returns by their standard deviation
varv <- HighFreq::run_var(retp, lambda=0.99)
retsc <- ifelse(varv > 0, retp/sqrt(varv), 0)
# Calculate the AR coefficients
predm <- lapply(1:orderp, rutils::lagit, input=retsc)
predm <- rutils::do_call(cbind, predm)
predm <- cbind(rep(1, nrows), predm)
colnames(predm) <- paste0("pred", 1:NCOL(predm))
predinv <- MASS::ginv(predm)
coeff <- predinv %*% retsc
# Calculate the in-sample forecasts of VTI
fcast <- predm %*% coeff
# Simulate autoregressive strategy in-sample
pnls <- retp*fcast
# Scale the PnL volatility to that of VTI
pnls <- pnls*sd(retp)/sd(pnls)
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of autoregressive strategy
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Autoregressive Strategy With Returns Scaled By Volatility") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Calculate VTI returns and trading volumes
ohlc <- rutils::etfenv$VTI
datev <- zoo::index(ohlc)
nrows <- NROW(ohlc)
closep <- quantmod::Cl(ohlc)
retp <- rutils::diffit(log(closep))
volumv <- quantmod::Vo(ohlc)
# Calculate trailing average volume
volumr <- HighFreq::run_mean(volumv, lambda=0.7)
# Scale the returns using volume clock to trading time
retsc <- ifelse(volumv > 0, volumr*retp/volumv, 0)
# Calculate the AR coefficients
respv <- retsc
predm <- lapply(1:orderp, rutils::lagit, input=respv)
predm <- rutils::do_call(cbind, predm)
predm <- cbind(rep(1, nrows), predm)
colnames(predm) <- paste0("pred", 1:NCOL(predm))
predinv <- MASS::ginv(predm)
coeff <- predinv %*% respv
# Calculate the in-sample forecasts of VTI
fcasts <- predm %*% coeff
# Simulate autoregressive strategy in-sample
pnls <- retp*fcasts
pnls <- pnls*sd(retp)/sd(pnls)
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of autoregressive strategy
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Autoregressive Strategy With Returns Scaled By Volume") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Calculate the correlation between forecasts and returns
cor(fcasts, retp)
# Calculate the forecasting errors
errorf <- (fcasts - retp)
# Mean squared error
mean(errorf^2)
# Plot the forecasts
datav <- cbind(retp, fcasts)["2020-01/2020-06"]
colnames(datav) <- c("returns", "forecasts")
dygraphs::dygraph(datav,
  main="VTI Returns And Forecasts") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Calculate the forecasts as function of the AR order
fcasts <- lapply(2:NCOL(predm), function(ordern) {
  # Calculate the fitted AR coefficients
  predinv <- MASS::ginv(predm[, 1:ordern])
  coeff <- predinv %*% respv
  # Calculate the in-sample forecasts of VTI
  drop(predm[, 1:ordern] %*% coeff)
})  # end lapply
names(fcasts) <- paste0("n=", 2:NCOL(predm))
# Calculate the mean squared errors
mse <- sapply(fcasts, function(x) {
  c(mse=mean((respv - x)^2), cor=cor(respv, x))
})  # end sapply
mse <- t(mse)
rownames(mse) <- names(fcasts)
# Plot forecasting MSE
plot(x=2:NCOL(predm), y=mse[, 1],
  xlab="AR(n) order", ylab="MSE", type="l", lwd=2,
  main="MSE of In-sample AR(n) Forecasting Model for VTI")
# Define in-sample and out-of-sample intervals
nrows <- NROW(retp)
insample <- 1:(nrows %/% 2)
outsample <- (nrows %/% 2 + 1):nrows
# Calculate the forecasts as function of the AR order
fcasts <- lapply(2:NCOL(predm), function(ordern) {
  # Calculate the fitted AR coefficients
  predinv <- MASS::ginv(predm[insample, 1:ordern])
  coeff <- predinv %*% respv[insample]
  # Calculate the out-of-sample forecasts of VTI
  drop(predm[outsample, 1:ordern] %*% coeff)
})  # end lapply
names(fcasts) <- paste0("n=", 2:NCOL(predm))
# Calculate the mean squared errors
mse <- sapply(fcasts, function(x) {
  c(mse=mean((respv[outsample] - x)^2), cor=cor(respv[outsample], x))
})  # end sapply
mse <- t(mse)
rownames(mse) <- names(fcasts)
# Plot forecasting MSE
plot(x=2:NCOL(predm), y=mse[, 1],
  xlab="AR(n) order", ylab="MSE", type="l", lwd=2,
  main="MSE of Out-of-sample AR(n) Forecasting Model for VTI")
# Calculate the optimal AR coefficients
predinv <- MASS::ginv(predm[insample, 1:2])
coeff <- drop(predinv %*% respv[insample])
# Calculate the out-of-sample PnLs
pnls <- lapply(fcasts, function(fcast) {
  cumsum(fcast*retp[outsample])
})  # end lapply
pnls <- rutils::do_call(cbind, pnls)
colnames(pnls) <- names(fcasts)
# Plot dygraph of out-of-sample PnLs
colorv <- colorRampPalette(c("red", "blue"))(NCOL(pnls))
colnamev <- colnames(pnls)
endd <- rutils::calc_endpoints(pnls, interval="weeks")
dygraphs::dygraph(pnls[endd],
  main="Autoregressive Strategies Out-of-sample") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(width=300)
# Perform rolling forecasting
look_back <- 100
fcasts <- sapply((look_back+1):nrows, function(tday) {
  # Define rolling look-back range
  startp <- max(1, tday-look_back)
  # Or expanding look-back range
  # startp <- 1
  rangev <- startp:(tday-1) # In-sample range
  # Invert the predictor matrix
  predinv <- MASS::ginv(predm[rangev, ])
  # Calculate the fitted AR coefficients
  coeff <- predinv %*% respv[rangev]
  # Calculate the out-of-sample forecast
  predm[tday, ] %*% coeff
})  # end sapply
# Add warmup period
fcasts <- c(rep(0, look_back), fcasts)
# Define backtesting function
sim_fcasts <- function(look_back=100, ordern=5, fixedlb=TRUE) {
  # Perform rolling forecasting
  fcasts <- sapply((look_back+1):nrows, function(tday) {
    # Define rolling look-back range
    if (fixedlb)
startp <- max(1, tday-look_back) # Fixed look-back
    else
startp <- 1 # Expanding look-back
    rangev <- startp:(tday-1) # In-sample range
    # Invert the predictor matrix
    predinv <- MASS::ginv(predm[rangev, 1:ordern])
    # Calculate the fitted AR coefficients
    coeff <- predinv %*% respv[rangev]
    # Calculate the out-of-sample forecast
    predm[tday, 1:ordern] %*% coeff
  })  # end sapply
  # Add warmup period
  fcasts <- c(rep(0, look_back), fcasts)
}  # end sim_fcasts
# Simulate the rolling autoregressive forecasts
fcasts <- sim_fcasts(look_back=100, ordern=5)
c(mse=mean((fcasts - retp)^2), cor=cor(retp, fcasts))
library(parallel)  # Load package parallel
# Calculate the number of available cores
ncores <- detectCores() - 1
# Initialize compute cluster under Windows
cluster <- makeCluster(ncores)
# Perform parallel loop under Windows
look_backs <- seq(20, 600, 40)
fcasts <- parLapply(cluster, look_backs, sim_fcasts, ordern=6)
# Perform parallel bootstrap under Mac-OSX or Linux
fcasts <- mclapply(look_backs, sim_fcasts, ordern=6, mc.cores=ncores)
# Calculate the mean squared errors
mse <- sapply(fcasts, function(x) {
  c(mse=mean((retp - x)^2), cor=cor(retp, x))
})  # end sapply
mse <- t(mse)
rownames(mse) <- look_backs
# Select optimal look_back interval
look_back <- look_backs[which.min(mse[, 1])]
# Plot forecasting MSE
plot(x=look_backs, y=mse[, 1],
  xlab="look-back", ylab="MSE", type="l", lwd=2,
  main="MSE of AR Forecasting Model As Function of Look-back")
library(parallel)  # Load package parallel
# Calculate the number of available cores
ncores <- detectCores() - 1
# Initialize compute cluster under Windows
cluster <- makeCluster(ncores)
# Perform parallel loop under Windows
orderv <- 2:6
fcasts <- parLapply(cluster, orderv, sim_fcasts,
  look_back=look_back)
stopCluster(cluster)  # Stop R processes over cluster under Windows
# Perform parallel bootstrap under Mac-OSX or Linux
fcasts <- mclapply(orderv, sim_fcasts,
  look_back=look_back, mc.cores=ncores)
# Calculate the mean squared errors
mse <- sapply(fcasts, function(x) {
  c(mse=mean((retp - x)^2), cor=cor(retp, x))
})  # end sapply
mse <- t(mse)
rownames(mse) <- orderv
# Select optimal order parameter
ordern <- orderv[which.min(mse[, 1])]
# Plot forecasting MSE
plot(x=orderv, y=mse[, 1],
  xlab="AR order", ylab="MSE", type="l", lwd=2,
  main="MSE of Forecasting Model As Function of AR Order")
# Simulate the rolling autoregressive forecasts
fcasts <- sim_fcasts(look_back=look_back, ordern=ordern)
# Calculate the strategy PnLs
pnls <- fcasts*retp
# Scale the PnL volatility to that of VTI
pnls <- pnls*sd(retp)/sd(pnls)
wealthv <- cbind(retp, pnls, (retp+pnls)/2)
colnames(wealthv) <- c("VTI", "AR_Strategy", "Combined")
cor(wealthv)
# Annualized Sharpe ratios of VTI and AR strategy
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of AR strategy combined with VTI
colorv <- c("blue", "red", "green")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Autoregressive Strategy Fixed Look-back") %>%
  dyOptions(colors=colorv, strokeWidth=1) %>%
  dySeries(name="Combined", label="Combined", strokeWidth=3) %>%
  dyLegend(show="always", width=300)
library(parallel)  # Load package parallel
# Calculate the number of available cores
ncores <- detectCores() - 1
# Initialize compute cluster under Windows
cluster <- makeCluster(ncores)
# Perform parallel loop under Windows
orderv <- 2:6
fcasts <- parLapply(cluster, orderv, sim_fcasts,
  look_back=look_back, fixedlb=FALSE)
stopCluster(cluster)  # Stop R processes over cluster under Windows
# Perform parallel bootstrap under Mac-OSX or Linux
fcasts <- mclapply(orderv, sim_fcasts,
  look_back=look_back, fixedlb=FALSE, mc.cores=ncores)
# Calculate the mean squared errors
mse <- sapply(fcasts, function(x) {
  c(mse=mean((retp - x)^2), cor=cor(retp, x))
})  # end sapply
mse <- t(mse)
rownames(mse) <- orderv
# Select optimal order parameter
ordern <- orderv[which.min(mse[, 1])]
# Plot forecasting MSE
plot(x=orderv, y=mse[, 1],
  xlab="AR order", ylab="MSE", type="l", lwd=2,
  main="MSE With Expanding Look-back As Function of AR Order")
# Simulate the autoregressive forecasts with expanding look-back
fcasts <- sim_fcasts(look_back=look_back, ordern=ordern, fixedlb=FALSE)
# Calculate the strategy PnLs
pnls <- fcasts*retp
pnls <- pnls*sd(retp)/sd(pnls)
wealthv <- cbind(retp, pnls, (retp+pnls)/2)
colnames(wealthv) <- c("VTI", "AR_Strategy", "Combined")
cor(wealthv)
# Annualized Sharpe ratios of VTI and AR strategy
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of AR strategy combined with VTI
colorv <- c("blue", "red", "green")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Autoregressive Strategy Expanding Look-back") %>%
  dyOptions(colors=colorv, strokeWidth=1) %>%
  dySeries(name="Combined", label="Combined", strokeWidth=3) %>%
  dyLegend(show="always", width=300)
# Load daily S&P500 percentage stock returns
load(file="/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
# Overwrite NA values in returns
retp <- returns100
retp[is.na(retp)] <- 0
# Remove stocks with very little data
datev <- zoo::index(retp) # dates
nrows <- NROW(retp) # number of rows
nzeros <- colSums(retp == 0)
sum(nzeros > nrows/2)
retp <- retp[, nzeros < nrows/2]
nstocks <- NCOL(retp) # number of stocks
# Objective function equal to Kelly ratio
objfun <- function(retp) {
  varv <- var(retp)
  if (varv > 0) mean(retp)/varv else 0
}  # end objfun
# Calculate performance statistics for all stocks
perfstat <- sapply(retp, objfun)
perfstat[!is.finite(perfstat)] <- 0
sum(is.na(perfstat))
sum(!is.finite(perfstat))
# Calculate weights proportional to performance statistic
weightm <- perfstat
hist(weightm)
# Center the weights
weightv <- weightm - mean(weightm)
sum(weightv)
sort(weightv)
# Quadratic constraint
weightv <- weightm/sqrt(sum(weightm^2))
sum(weightv^2)
sum(weightv)
weightv
# Apply the linear constraint
weightv <- weightm/sum(weightm)
sum(weightv)
weightv
# Calculate in-sample portfolio volatility
volis <- sd(drop(retp %*% weightm))
# Calculate equal weight portfolio volatility
volew <- sd(rowMeans(retp))
# Apply the volatility constraint
weightv <- volew*weightm/volis
sqrt(var(drop(retp %*% weightv)))
# Apply the volatility target constraint
volt <- 1e-2
weightv <- volt*weightm/volis
sqrt(var(drop(retp %*% weightv)))
# Box constraints
weightv[weightv > 1] <- 1
weightv[weightv < 0] <- 0
weightv
# Objective function equal to sum of returns
objfun <- function(retp) sum(retp)
# Objective function equal to Sharpe ratio
objfun <- function(retp) mean(retp)/max(sd(retp), 1e-4)
# Objective function equal to Kelly ratio
objfun <- function(retp) {
  varv <- var(retp)
  if (varv > 0) mean(retp)/varv else 0
}  # end objfun
# Calculate performance statistics for all stocks
perfstat <- sapply(retp, objfun)
perfstat[!is.finite(perfstat)] <- 0
sum(is.na(perfstat))
# Calculate the best and worst performing stocks
perfstat <- sort(perfstat, decreasing=TRUE)
topstocks <- 10
symbolb <- names(head(perfstat, topstocks))
symbolw <- names(tail(perfstat, topstocks))
# Calculate equal weights for the best and worst performing stocks
weightv <- numeric(NCOL(retp))
names(weightv) <- colnames(retp)
weightv[symbolb] <- 1
weightv[symbolw] <- (-1)
# Calculate weights proportional to performance statistic
weightv <- perfstat
# Center weights so sum is equal to 0
weightv <- weightv - mean(weightv)
# Scale weights so sum of squares is equal to 1
weightv <- weightv/sqrt(sum(weightv^2))
# Calculate the strategy returns
retportf <- retp %*% weightv
# Scale weights so in-sample portfolio volatility is same as equal weight
scalev <- sd(rowMeans(retp))/sd(retportf)
weightv <- scalev*weightv
# Objective function equal to Kelly ratio
objfun <- function(retp) {
  varv <- var(retp)
  if (varv > 0) mean(retp)/varv else 0
}  # end objfun
# Calculate a vector of monthly end points
endd <- rutils::calc_endpoints(retp, interval="months")
npts <- NROW(endd)
# Perform loop over the end points
look_back <- 8
pnls <- lapply(2:(npts-1), function(ep) {
  # Select the look-back returns
  startp <- endd[max(1, ep-look_back)]
  retsis <- retp[startp:endd[ep], ]
  # Calculate the best and worst performing stocks in-sample
  perfstat <- sapply(retsis, objfun)
  perfstat[!is.finite(perfstat)] <- 0
  perfstat <- sort(perfstat, decreasing=TRUE)
  symbolb <- names(head(perfstat, topstocks))
  symbolw <- names(tail(perfstat, topstocks))
  # Calculate the momentum weights
  weightv <- numeric(NCOL(retp))
  names(weightv) <- colnames(retp)
  weightv[symbolb] <- 1
  weightv[symbolw] <- (-1)
  # Calculate the in-sample portfolio returns
  retportf <- retsis %*% weightv
  # Scale weights so in-sample portfolio volatility is same as equal weight
  weightv <- weightv*sd(rowMeans(retsis))/sd(retportf)
  # Calculate the out-of-sample momentum returns
  drop(retp[(endd[ep]+1):endd[ep+1], ] %*% weightv)
})  # end lapply
pnls <- rutils::do_call(c, pnls)
# Calculate the average of all stock returns
indeks <- rowMeans(retp)
indeks <- xts::xts(indeks, order.by=datev)
colnames(indeks) <- "Index"
# Add initial startup interval to the momentum returns
pnls <- c(rowMeans(retp[endd[1]:endd[2], ]), pnls)
pnls <- xts::xts(pnls, order.by=datev)
colnames(pnls) <- "Strategy"
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(indeks, pnls)
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of stock index and momentum strategy
colorv <- c("blue", "red")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Log Stock Index and Momentum Strategy") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=300)
btmomtop <- function(retp, objfun, look_back=12, rebalf="months", topstocks=10,
  bid_offer=0.0, endd=rutils::calc_endpoints(retp, interval=rebalf), ...) {
  # Perform loop over end points
  npts <- NROW(endd)
  pnls <- lapply(2:(npts-1), function(ep) {
    # Select the look-back returns
    startp <- endd[max(1, ep-look_back)]
    retsis <- retp[startp:endd[ep], ]
    # Calculate the best and worst performing stocks in-sample
    perfstat <- sapply(retsis, objfun)
    perfstat[!is.finite(perfstat)] <- 0
    perfstat <- sort(perfstat, decreasing=TRUE)
    symbolb <- names(head(perfstat, topstocks))
    symbolw <- names(tail(perfstat, topstocks))
    # Calculate the momentum weights
    weightv <- numeric(NCOL(retp))
    names(weightv) <- colnames(retp)
    weightv[symbolb] <- 1
    weightv[symbolw] <- (-1)
    # Calculate the in-sample portfolio returns
    retportf <- retsis %*% weightv
    # Scale weights so in-sample portfolio volatility is same as equal weight
    weightv <- weightv*sd(rowMeans(retsis))/sd(retportf)
    # Calculate the out-of-sample momentum returns
    drop(retp[(endd[ep]+1):endd[ep+1], ] %*% weightv)
  })  # end lapply
  pnls <- rutils::do_call(c, pnls)
  pnls
}  # end btmomtop
# Perform backtests for vector of look-back intervals
look_backs <- seq(3, 15, by=1)
endd <- rutils::calc_endpoints(retp, interval="months")
# Warning - takes very long
pnll <- lapply(look_backs, btmomtop, retp=retp, endd=endd, objfun=objfun)
# Perform parallel loop under Mac-OSX or Linux
library(parallel)  # Load package parallel
ncores <- detectCores() - 1
pnll <- mclapply(look_backs, btmomtop, retp=retp, endd=endd, objfun=objfun, mc.cores=ncores)
sharper <- sqrt(252)*sapply(pnll, function(pnl) mean(pnl)/sd(pnl))
# Plot Sharpe ratios of momentum strategies
plot(x=look_backs, y=sharper, t="l",
  main="Momentum Sharpe as Function of Look-back Interval",
  xlab="look-back (months)", ylab="Sharpe")
# Calculate best pnls of momentum strategy
whichmax <- which.max(sharper)
look_backs[whichmax]
pnls <- pnll[[whichmax]]
# Add stub period
pnls <- c(rowMeans(retp[endd[1]:endd[2], ]), pnls)
pnls <- xts::xts(pnls, order.by=datev)
colnames(pnls) <- "Strategy"
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(indeks, pnls)
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of stock index and momentum strategy
colorv <- c("blue", "red")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Optimal Momentum Strategy for Stocks") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=300)
btmomweight <- function(retp, objfun, look_back=12, rebalf="months",
  bid_offer=0.0, endd=rutils::calc_endpoints(retp, interval=rebalf), ...) {
  # Perform loop over end points
  npts <- NROW(endd)
  pnls <- lapply(2:(npts-1), function(ep) {
    # Select the look-back returns
    startp <- endd[max(1, ep-look_back)]
    retsis <- retp[startp:endd[ep], ]
    # Calculate weights proportional to performance
    perfstat <- sapply(retsis, objfun)
    perfstat[!is.finite(perfstat)] <- 0
    weightv <- perfstat
    # Calculate the in-sample portfolio returns
    retportf <- retsis %*% weightv
    # Scale weights so in-sample portfolio volatility is same as equal weight
    weightv <- weightv*sd(rowMeans(retsis))/sd(retportf)
    # Calculate the out-of-sample momentum returns
    retp[(endd[ep]+1):endd[ep+1], ] %*% weightv
  })  # end lapply
  rutils::do_call(c, pnls)
}  # end btmomweight
# Perform backtests for vector of look-back intervals
look_backs <- seq(3, 15, by=1)
pnll <- lapply(look_backs, btmomweight, retp=retp, endd=endd, objfun=objfun)
# Or perform parallel loop under Mac-OSX or Linux
library(parallel)  # Load package parallel
ncores <- detectCores() - 1
pnll <- mclapply(look_backs, btmomweight, retp=retp, endd=endd, objfun=objfun, mc.cores=ncores)
sharper <- sqrt(252)*sapply(pnll, function(pnl) mean(pnl)/sd(pnl))
# Plot Sharpe ratios of momentum strategies
plot(x=look_backs, y=sharper, t="l",
  main="Momentum Sharpe as Function of Look-back Interval",
  xlab="look-back (months)", ylab="Sharpe")
# Calculate best pnls of momentum strategy
whichmax <- which.max(sharper)
look_backs[whichmax]
pnls <- pnll[[whichmax]]
pnls <- c(rowMeans(retp[endd[1]:endd[2], ]), pnls)
pnls <- xts::xts(pnls, order.by=datev)
colnames(pnls) <- "Strategy"
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(indeks, pnls, 0.5*(indeks+pnls))
colnames(wealthv)[3] <- "Combined"
cor(wealthv)
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of stock index and momentum strategy
colorv <- c("blue", "red", "green")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Optimal Weighted Momentum Strategy for Stocks") %>%
  dyOptions(colors=colorv, strokeWidth=1) %>%
  dySeries(name="Combined", label="Combined", strokeWidth=3) %>%
  dyLegend(show="always", width=300)
# Calculate the trailing average returns and variance using C++ code
lambda <- 0.99
meanm <- HighFreq::run_mean(retp, lambda=lambda)
varm <- HighFreq::run_var(retp, lambda=lambda)
# Calculate the trailing Kelly ratio
weightv <- ifelse(varm > 0, meanm/varm, 0)
weightv[1, ] <- 1
sum(is.na(weightv))
weightv <- weightv/sqrt(rowSums(weightv^2))
weightv <- rutils::lagit(weightv)
# Calculate the momentum profits and losses
pnls <- rowSums(weightv*retp)
# Calculate the transaction costs
bid_offer <- 0.0
costs <- 0.5*bid_offer*rowSums(abs(rutils::diffit(weightv)))
pnls <- (pnls - costs)
# Scale the momentum volatility to the equal weight index
indeksd <- sd(indeks)
pnls <- indeksd*pnls/sd(pnls)
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(indeks, pnls, 0.5*(indeks+pnls))
colnames(wealthv)[2:3] <- c("Momentum", "Combined")
cor(wealthv)
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of stock index and momentum strategy
colorv <- c("blue", "red", "green")
endd <- rutils::calc_endpoints(retp, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Daily Momentum Strategy for Stocks") %>%
  dyOptions(colors=colorv, strokeWidth=1) %>%
  dySeries(name="Combined", label="Combined", strokeWidth=3) %>%
  dyLegend(show="always", width=300)
# Define backtest functional for daily momentum strategy
# If trend=(-1) then it backtests a mean reverting strategy
btmomdaily <- function(retp, lambda=0.9, trend=1, bid_offer=0.0, ...) {
  stopifnot("package:quantmod" %in% search() || require("quantmod", quietly=TRUE))
  # Calculate the trailing Kelly ratio
  meanm <- HighFreq::run_mean(retp, lambda=lambda)
  varm <- HighFreq::run_var(retp, lambda=lambda)
  weightv <- ifelse(varm > 0, meanm/varm, 0)
  weightv[1, ] <- 1
  weightv <- weightv/sqrt(rowSums(weightv^2))
  weightv <- rutils::lagit(weightv)
  # Calculate the momentum profits and losses
  pnls <- trend*rowSums(weightv*retp)
  # Calculate the transaction costs
  costs <- 0.5*bid_offer*rowSums(abs(rutils::diffit(weightv)))
  (pnls - costs)
}  # end btmomdaily
# Simulate multiple daily stock momentum strategies
lambdas <- seq(0.99, 0.999, 0.002)
pnls <- sapply(lambdas, btmomdaily, retp=retp)
# Scale the momentum volatility to the equal weight index
pnls <- apply(pnls, MARGIN=2, function(pnl) indeksd*pnl/sd(pnl))
colnames(pnls) <- paste0("lambda=", lambdas)
pnls <- xts::xts(pnls, datev)
tail(pnls)
# Plot dygraph of daily stock momentum strategies
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls)[endd],
  main="Daily Stock Momentum Strategies") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Plot daily stock momentum strategies using quantmod
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(pnls))
quantmod::chart_Series(cumsum(pnls)[endd],
  theme=plot_theme, name="Daily Stock Momentum Strategies")
legend("bottomleft", legend=colnames(pnls),
  inset=0.02, bg="white", cex=0.7, lwd=rep(6, NCOL(retp)),
  col=plot_theme$col$line.col, bty="n")
# Define backtest functional for daily momentum strategy
# If trend=(-1) then it backtests a mean reverting strategy
btmomdailyhold <- function(retp, lambda=0.9, trend=1, bid_offer=0.0, ...) {
  stopifnot("package:quantmod" %in% search() || require("quantmod", quietly=TRUE))
  # Calculate the trailing Kelly ratio
  meanm <- HighFreq::run_mean(retp, lambda=lambda)
  varm <- HighFreq::run_var(retp, lambda=lambda)
  weightv <- ifelse(varm > 0, meanm/varm, 0)
  weightv[1, ] <- 1
  weightv <- weightv/sqrt(rowSums(weightv^2))
  # Average the past weights
  weightv <- HighFreq::run_mean(weightv, lambda=lambda)
  weightv <- rutils::lagit(weightv)
  # Calculate the momentum profits and losses
  pnls <- trend*rowSums(weightv*retp)
  # Calculate the transaction costs
  costs <- 0.5*bid_offer*rowSums(abs(rutils::diffit(weightv)))
  (pnls - costs)
}  # end btmomdailyhold
# Simulate multiple daily stock momentum strategies with holding periods
lambdas <- seq(0.99, 0.999, 0.002)
pnls <- sapply(lambdas, btmomdailyhold, retp=retp)
# Scale the momentum volatility to the equal weight index
pnls <- apply(pnls, MARGIN=2, function(pnl) indeksd*pnl/sd(pnl))
colnames(pnls) <- paste0("lambda=", lambdas)
pnls <- xts::xts(pnls, datev)
# dygraph of daily stock momentum strategies with holding period
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls)[endd],
  main="Daily Stock Momentum Strategies with Holding Period") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Plot of daily stock momentum strategies with holding period
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(pnls))
quantmod::chart_Series(cumsum(pnls)[endd],
  theme=plot_theme, name="Daily Stock Momentum Strategies with Holding Period")
legend("bottomleft", legend=colnames(pnls),
  inset=0.02, bg="white", cex=0.7, lwd=rep(6, NCOL(retp)),
  col=plot_theme$col$line.col, bty="n")
# Calculate best pnls of momentum strategy
sharper <- sqrt(252)*sapply(pnls, function(pnl) mean(pnl)/sd(pnl))
whichmax <- which.max(sharper)
lambdas[whichmax]
pnls <- pnls[, whichmax]
colnames(pnls) <- "Strategy"
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(indeks, pnls, 0.5*(indeks+pnls))
colnames(wealthv)[2:3] <- c("Momentum", "Combined")
cor(wealthv)
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of stock index and momentum strategy
colorv <- c("blue", "red", "green")
endd <- rutils::calc_endpoints(retp, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Optimal Daily Momentum Strategy for Stocks") %>%
  dyOptions(colors=colorv, strokeWidth=1) %>%
  dySeries(name="Combined", label="Combined", strokeWidth=3) %>%
  dyLegend(show="always", width=300)
# Perform sapply loop over look_backs
lambdas <- seq(0.2, 0.7, 0.1)
pnls <- sapply(lambdas, btmomdaily, retp=retp, trend=(-1))
# Scale the momentum volatility to the equal weight index
pnls <- apply(pnls, MARGIN=2, function(pnl) indeksd*pnl/sd(pnl))
colnames(pnls) <- paste0("lambda=", lambdas)
pnls <- xts::xts(pnls, datev)
# Plot dygraph of mean reverting daily stock momentum strategies
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls)[endd],
  main="Mean Reverting Daily Stock Momentum Strategies") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=400)
# Plot mean reverting daily stock momentum strategies using quantmod
plot_theme <- chart_theme()
plot_theme$col$line.col <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
quantmod::chart_Series(cumsum(pnls)[endd],
  theme=plot_theme, name="Mean Reverting Daily Stock Momentum Strategies")
legend("topleft", legend=colnames(pnls),
  inset=0.05, bg="white", cex=0.7, lwd=rep(6, NCOL(retp)),
  col=plot_theme$col$line.col, bty="n")
# Calculate the scaled prices of VTI vs MTUM ETF
wealthv <- na.omit(rutils::etfenv$returns[, c("VTI", "MTUM")])
colnames(wealthv) <- c("VTI", "MTUM")
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of scaled prices of VTI vs MTUM ETF
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="VTI vs MTUM ETF") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(width=300)
# Calculate the PCA weights for standardized returns
retsc <- lapply(retp, function(x) (x - mean(x))/sd(x))
retsc <- do.call(cbind, retsc)
covmat <- cov(retsc)
pcad <- eigen(covmat)
pcaw <- pcad$vectors
rownames(pcaw) <- colnames(retp)
sort(-pcaw[, 1], decreasing=TRUE)
sort(pcaw[, 2], decreasing=TRUE)
round((t(pcaw) %*% pcaw)[1:5, 1:5], 4)
# Calculate the PCA time series from stock returns using PCA weights
retpca <- retsc %*% pcaw
round((t(retpca) %*% retpca)[1:5, 1:5], 4)
# Calculate the PCA using prcomp()
pcad <- prcomp(retsc, center=FALSE, scale=FALSE)
all.equal(abs(pcad$x), abs(retpca), check.attributes=FALSE)
retpca <- xts::xts(retpca, order.by=datev)
# Simulate daily PCA momentum strategies for multiple lambda parameters
dimax <- 21
lambdas <- seq(0.98, 0.99, 0.003)
pnls <- mclapply(lambdas, btmomdailyhold, retp=retpca[, 1:dimax], mc.cores=ncores)
pnls <- lapply(pnls, function(pnl) indeksd*pnl/sd(pnl))
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("lambda=", lambdas)
pnls <- xts::xts(pnls, datev)
# Plot Sharpe ratios of momentum strategies
sharper <- sqrt(252)*sapply(pnls, function(pnl) mean(pnl)/sd(pnl))
plot(x=lambdas, y=sharper, t="l",
  main="PCA Momentum Sharpe as Function of Decay Parameter",
  xlab="lambda", ylab="Sharpe")
# Plot dygraph of daily PCA momentum strategies
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
endd <- rutils::calc_endpoints(retpca, interval="weeks")
dygraphs::dygraph(cumsum(pnls)[endd],
  main="Daily PCA Momentum Strategies") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=400)
# Calculate best pnls of PCA momentum strategy
whichmax <- which.max(sharper)
lambdas[whichmax]
pnls <- pnls[, whichmax]
colnames(pnls) <- "Strategy"
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(indeks, pnls, 0.5*(indeks+pnls))
colnames(wealthv)[2:3] <- c("Momentum", "Combined")
cor(wealthv)
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of stock index and PCA momentum strategy
colorv <- c("blue", "red", "green")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Optimal Daily Momentum Strategy for Stocks") %>%
  dyOptions(colors=colorv, strokeWidth=1) %>%
  dySeries(name="Combined", label="Combined", strokeWidth=3) %>%
  dyLegend(show="always", width=300)
# Simulate daily PCA momentum strategies for multiple lambda parameters
lambdas <- seq(0.6, 0.9, 0.1)
pnls <- mclapply(lambdas, btmomdailyhold, retp=retpca[, (dimax+1):NCOL(retpca)],
   trend=(-1), mc.cores=ncores)
pnls <- lapply(pnls, function(pnl) indeksd*pnl/sd(pnl))
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("lambda=", lambdas)
pnls <- xts::xts(pnls, datev)
# Plot Sharpe ratios of momentum strategies
sharper <- sqrt(252)*sapply(pnls, function(pnl) mean(pnl)/sd(pnl))
plot(x=lambdas, y=sharper, t="l",
  main="PCA Momentum Sharpe as Function of Decay Parameter",
  xlab="lambda", ylab="Sharpe")
# Plot dygraph of daily PCA momentum strategies
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls)[endd],
  main="Mean Reverting Daily PCA Momentum Strategies") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=400)
# Define in-sample and out-of-sample intervals
cutoff <- nrows %/% 2
datev[cutoff]
insample <- 1:cutoff
outsample <- (cutoff + 1):nrows
# Calculate the PCA weights in-sample
pcad <- prcomp(retp[insample], center=FALSE, scale=TRUE)
# Calculate the out-of-sample PCA time series
retsc <- lapply(retp, function(x) x[outsample]/sd(x[insample]))
retsc <- do.call(cbind, retsc)
retpca <- xts::xts(retsc %*% pcad$rotation, order.by=datev[outsample])
# Simulate daily PCA momentum strategies for multiple lambda parameters
lambdas <- seq(0.99, 0.999, 0.003)
pnls <- mclapply(lambdas, btmomdailyhold, retp=retpca[, 1:dimax], mc.cores=ncores)
pnls <- lapply(pnls, function(pnl) indeksd*pnl/sd(pnl))
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("lambda=", lambdas)
pnls <- xts::xts(pnls, datev[outsample])
# Plot Sharpe ratios of momentum strategies
sharper <- sqrt(252)*sapply(pnls, function(pnl) mean(pnl)/sd(pnl))
plot(x=lambdas, y=sharper, t="l",
  main="PCA Momentum Sharpe as Function of Decay Parameter",
  xlab="lambda", ylab="Sharpe")
# Calculate a vector of weekly end points
endd <- rutils::calc_endpoints(retpca, interval="weeks")
# Plot dygraph of daily out-of-sample PCA momentum strategies
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls)[endd],
  main="Daily Out-of-Sample PCA Momentum Strategies") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Simulate daily PCA momentum strategies for multiple lambda parameters
lambdas <- seq(0.5, 0.9, 0.1)
pnls <- mclapply(lambdas, btmomdailyhold, retp=retpca[, (dimax+1):NCOL(retpca)],
   trend=(-1), mc.cores=ncores)
pnls <- lapply(pnls, function(pnl) indeksd*pnl/sd(pnl))
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("lambda=", lambdas)
pnls <- xts::xts(pnls, datev[outsample])
# Plot Sharpe ratios of momentum strategies
sharper <- sqrt(252)*sapply(pnls, function(pnl) mean(pnl)/sd(pnl))
plot(x=lambdas, y=sharper, t="l",
  main="PCA Momentum Sharpe as Function of Decay Parameter",
  xlab="lambda", ylab="Sharpe")
# Calculate a vector of weekly end points
endd <- rutils::calc_endpoints(retpca, interval="weeks")
# Plot dygraph of daily S&P500 momentum strategies
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls)[endd],
  main="Mean Reverting Daily Out-of-Sample PCA Momentum Strategies") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Extract ETF returns
symbolv <- c("VTI", "IEF", "DBC")
retp <- na.omit(rutils::etfenv$returns[, symbolv])
datev <- zoo::index(retp)
# Calculate a vector of monthly end points
endd <- rutils::calc_endpoints(retp, interval="months")
npts <- NROW(endd)
# Perform backtests for vector of look-back intervals
look_backs <- seq(3, 12, by=1)
pnll <- lapply(look_backs, btmomweight, retp=retp, endd=endd, objfun=objfun)
sharper <- sqrt(252)*sapply(pnll, function(pnl) mean(pnl)/sd(pnl))
# Plot Sharpe ratios of momentum strategies
plot(x=look_backs, y=sharper, t="l",
  main="Momentum Sharpe as Function of Look-back Interval",
  xlab="look-back (months)", ylab="Sharpe")
# Calculate best pnls of momentum strategy
whichmax <- which.max(sharper)
look_backs[whichmax]
pnls <- pnll[[whichmax]]
pnls <- c(rowMeans(retp[endd[1]:endd[2], ]), pnls)
# Define all-weather benchmark
weightvaw <- c(0.30, 0.55, 0.15)
all_weather <- retp %*% weightvaw
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(all_weather, pnls, 0.5*(all_weather+pnls))
colnames(wealthv) <- c("All-weather", "Strategy", "Combined")
cor(wealthv)
wealthv <- xts::xts(wealthv, order.by=datev)
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of stock index and momentum strategy
colorv <- c("blue", "red", "green")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Optimal Momentum Strategy and All-weather for ETFs") %>%
  dyOptions(colors=colorv, strokeWidth=1) %>%
  dySeries(name="Combined", label="Combined", strokeWidth=3) %>%
  dyLegend(show="always", width=300)
# Calculate the momentum weights
look_back <- look_backs[whichmax]
weightv <- lapply(2:npts, function(ep) {
  # Select the look-back returns
  startp <- endd[max(1, ep-look_back)]
  retsis <- retp[startp:endd[ep], ]
  # Calculate weights proportional to performance
  perfstat <- sapply(retsis, objfun)
  weightv <- drop(perfstat)
  # Scale weights so in-sample portfolio volatility is same as equal weight
  retportf <- retsis %*% weightv
  weightv*sd(rowMeans(retsis))/sd(retportf)
})  # end lapply
weightv <- rutils::do_call(rbind, weightv)
# Plot of momentum weights
retvti <- cumsum(retp$VTI)
datav <- cbind(retvti[endd], weightv)
colnames(datav) <- c("VTI", paste0(colnames(retp), "_weight"))
zoo::plot.zoo(datav, xlab=NULL, main="Momentum Weights")
# Calculate ETF betas
betasetf <- sapply(retp, function(x)
  cov(retp$VTI, x)/var(retp$VTI))
# Momentum beta is equal weights times ETF betas
betav <- weightv %*% betasetf
betav <- xts::xts(betav, order.by=datev[endd])
colnames(betav) <- "momentum_beta"
datav <- cbind(retvti[endd], betav)
zoo::plot.zoo(datav, main="Momentum Beta & VTI Price", xlab="")
# Merton-Henriksson test
retvti <- retp$VTI
predm <- cbind(VTI=retvti, 0.5*(retvti+abs(retvti)), retvti^2)
colnames(predm)[2:3] <- c("merton", "treynor")
regmod <- lm(pnls ~ VTI + merton, data=predm); summary(regmod)
# Treynor-Mazuy test
regmod <- lm(pnls ~ VTI + treynor, data=predm); summary(regmod)
# Plot residual scatterplot
resids <- regmod$residuals
plot.default(x=retvti, y=resids, xlab="VTI", ylab="momentum")
title(main="Treynor-Mazuy Market Timing Test\n for Momentum vs VTI", line=0.5)
# Plot fitted (predicted) response values
coefreg <- summary(regmod)$coeff
fitv <- regmod$fitted.values - coefreg["VTI", "Estimate"]*retvti
tvalue <- round(coefreg["treynor", "t value"], 2)
points.default(x=retvti, y=fitv, pch=16, col="red")
text(x=0.0, y=max(resids), paste("Treynor test t-value =", tvalue))
# Standardize the returns
pnlsd <- (pnls-mean(pnls))/sd(pnls)
retvti <- (retvti-mean(retvti))/sd(retvti)
# Calculate skewness and kurtosis
apply(cbind(pnlsd, retvti), 2, function(x)
  sapply(c(skew=3, kurt=4),
    function(e) sum(x^e)))/NROW(retvti)
# Calculate kernel density of VTI
densvti <- density(retvti)
# Plot histogram of momentum returns
hist(pnlsd, breaks=80,
  main="Momentum and VTI Return Distributions (standardized)",
  xlim=c(-4, 4), ylim=range(densvti$y), xlab="", ylab="", freq=FALSE)
# Draw kernel density of histogram
lines(density(pnlsd), col='red', lwd=2)
lines(densvti, col='blue', lwd=2)
# Add legend
legend("topright", inset=0.0, cex=1.0, title=NULL,
 leg=c("Momentum", "VTI"), bty="n", y.intersp=0.5,
 lwd=6, bg="white", col=c("red", "blue"))
# Combine momentum strategy with all-weather
wealthv <- cbind(pnls, all_weather, 0.5*(pnls + all_weather))
colnames(wealthv) <- c("Momentum", "All_weather", "Combined")
wealthv <- xts::xts(wealthv, datev)
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Calculate strategy correlations
cor(wealthv)
# Plot ETF momentum strategy combined with All-Weather
colorv <- c("blue", "red", "green")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Optimal Momentum Strategy and All-weather for ETFs") %>%
  dyOptions(colors=colorv, strokeWidth=1) %>%
  dySeries(name="Combined", label="Combined", strokeWidth=3) %>%
  dyLegend(show="always", width=300)
# Calculate the trailing variance
look_back <- 152
varm <- HighFreq::roll_var(retp, look_back=look_back)
# Calculate the trailing Kelly ratio
meanv <- HighFreq::roll_mean(retp, look_back=look_back)
weightv <- ifelse(varm > 0, meanv/varm, 0)
sum(is.na(weightv))
weightv <- weightv/sqrt(rowSums(weightv^2))
weightv <- rutils::lagit(weightv)
# Calculate the momentum profits and losses
pnls <- rowSums(weightv*retp)
# Calculate the transaction costs
bid_offer <- 0.0
costs <- 0.5*bid_offer*rowSums(abs(rutils::diffit(weightv)))
pnls <- (pnls - costs)
# Scale the momentum volatility to all_weather
pnls <- sd(all_weather)*pnls/sd(pnls)
# Calculate the wealth of momentum returns
wealthv <- cbind(pnls, all_weather, 0.5*(pnls + all_weather))
colnames(wealthv) <- c("Momentum", "All_weather", "Combined")
wealthv <- xts::xts(wealthv, datev)
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
cor(wealthv)
# Plot dygraph of the momentum strategy returns
colorv <- c("blue", "red", "green")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Daily Momentum Strategy for ETFs vs All-Weather") %>%
  dyOptions(colors=colorv, strokeWidth=1) %>%
  dySeries(name="Combined", label="Combined", strokeWidth=3) %>%
  dyLegend(show="always", width=300)
