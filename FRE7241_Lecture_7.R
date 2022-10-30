# Select all the ETF symbols except "VXX", "SVXY" "MTUM", "QUAL", "VLUE", and "USMV"
symbolv <- colnames(rutils::etfenv$returns)
symbolv <- symbolv[!(symbolv %in% c("VXX", "SVXY", "MTUM", "QUAL", "VLUE", "USMV"))]
# Extract columns of rutils::etfenv$returns and overwrite NA values
retsp <- rutils::etfenv$returns[, symbolv]
nstocks <- NCOL(retsp)
# retsp <- na.omit(retsp)
retsp[1, is.na(retsp[1, ])] <- 0
retsp <- zoo::na.locf(retsp, na.rm=FALSE)
datev <- zoo::index(retsp)
# Returns in excess of risk-free rate
riskf <- 0.03/252
retsx <- (retsp - riskf)

# Maximum Sharpe weights in-sample interval
retsis <- retsp["/2014"]
invmat <- MASS::ginv(cov(retsis))
weightv <- invmat %*% colMeans(retsx["/2014"])
weightv <- drop(weightv/sqrt(sum(weightv^2)))
names(weightv) <- colnames(retsp)
# Plot portfolio weights
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
barplot(sort(weightv), main="Maximum Sharpe Weights", cex.names=0.7)

# Calculate in-sample portfolio returns
insample <- xts::xts(retsis %*% weightv, zoo::index(retsis))
indeks <- xts::xts(rowMeans(retsis), zoo::index(retsis))
insample <- insample*sd(indeks)/sd(insample)

# Plot cumulative portfolio returns
pnls <- cbind(indeks, insample)
colnames(pnls) <- c("Equal Weight", "Optimal")
endp <- rutils::calc_endpoints(pnls, interval="months")
dygraphs::dygraph(cumsum(pnls)[endp], main="In-sample Optimal Portfolio Returns") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(width=500)

# Calculate out-of-sample portfolio returns
retsos <- retsp["2015/"]
outsample <- xts::xts(retsos %*% weightv, zoo::index(retsos))
indeks <- xts::xts(rowMeans(retsos), zoo::index(retsos))
outsample <- outsample*sd(indeks)/sd(outsample)
pnls <- cbind(indeks, outsample, (outsample + indeks)/2)
colnames(pnls) <- c("Equal Weight", "Optimal", "Combined")
sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x))

# Plot cumulative portfolio returns
endp <- rutils::calc_endpoints(pnls, interval="months")
dygraphs::dygraph(cumsum(pnls)[endp], main="Out-of-sample Optimal Portfolio Returns") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=2) %>%
  dyLegend(width=500)

# Maximum Sharpe weights in-sample interval
invmat <- MASS::ginv(cov(retsis))
weightv <- invmat %*% colMeans(retsx["/2014"])
weightv <- drop(weightv/sqrt(sum(weightv^2)))
names(weightv) <- colnames(retsp)
# Calculate in-sample portfolio returns
insample <- xts::xts(retsis %*% weightv, zoo::index(retsis))
# Calculate out-of-sample portfolio returns
retsos <- retsp["2015/"]
outsample <- xts::xts(retsos %*% weightv, zoo::index(retsos))

# Plot cumulative portfolio returns
pnls <- rbind(insample, outsample)
indeks <- xts::xts(rowMeans(retsp), datev)
pnls <- pnls*sd(indeks)/sd(pnls)
pnls <- cbind(indeks, pnls)
colnames(pnls) <- c("Equal Weight", "Optimal")
endp <- rutils::calc_endpoints(pnls, interval="months")
dygraphs::dygraph(cumsum(pnls)[endp], main="Out-of-sample Optimal Portfolio Returns for ETFs") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyEvent(zoo::index(last(retsis[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=500)

# Create rectangular matrix with collinear columns
matrixv <- matrix(rnorm(10*8), nc=10)
# Calculate covariance matrix
covmat <- cov(matrixv)
# Calculate inverse of covmat - error
invmat <- solve(covmat)
# Perform eigen decomposition
eigend <- eigen(covmat)
eigenvec <- eigend$vectors
eigenval <- eigend$values
# Set tolerance for determining zero singular values
precv <- sqrt(.Machine$double.eps)
# Calculate regularized inverse matrix
notzero <- (eigenval > (precv*eigenval[1]))
invreg <- eigenvec[, notzero] %*%
  (t(eigenvec[, notzero]) / eigenval[notzero])
# Verify inverse property of invreg
all.equal(covmat, covmat %*% invreg %*% covmat)
# Calculate regularized inverse of covmat
invmat <- MASS::ginv(covmat)
# Verify that invmat is same as invreg
all.equal(invmat, invreg)

# Calculate in-sample covariance matrix
covmat <- cov(retsis)
eigend <- eigen(covmat)
eigenvec <- eigend$vectors
eigenval <- eigend$values
# Calculate dimension reduction inverse of covariance matrix
dimax <- 3
covinv <- eigenvec[, 1:dimax] %*%
  (t(eigenvec[, 1:dimax]) / eigenval[1:dimax])
# Verify inverse property of inverse
all.equal(covmat, covmat %*% covinv %*% covmat)

# Calculate portfolio weights
weightv <- invmat %*% colMeans(retsis)
weightv <- drop(weightv/sqrt(sum(weightv^2)))
names(weightv) <- colnames(retsp)
# Calculate portfolio returns
insample <- xts::xts(retsis %*% weightv, zoo::index(retsis))
outsample <- xts::xts(retsos %*% weightv, zoo::index(retsos))

# Plot cumulative portfolio returns
pnls <- rbind(insample, outsample)
pnls <- pnls*sd(indeks)/sd(pnls)
pnls <- cbind(indeks, pnls)
colnames(pnls) <- c("Equal Weight", "Optimal")
dygraphs::dygraph(cumsum(pnls)[endp], main="Optimal Portfolio Returns With Dimension Reduction") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyEvent(zoo::index(last(retsis[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=500)

# Shrink the in-sample returns to their mean
alpha <- 0.7
retsxm <- rowMeans(retsx["/2014"])
retsxis <- (1-alpha)*retsx["/2014"] + alpha*retsxm
# Calculate portfolio weights
weightv <- invmat %*% colMeans(retsxis)
weightv <- drop(weightv/sqrt(sum(weightv^2)))
# Calculate portfolio returns
insample <- xts::xts(retsis %*% weightv, zoo::index(retsis))
outsample <- xts::xts(retsos %*% weightv, zoo::index(retsos))

# Plot cumulative portfolio returns
pnls <- rbind(insample, outsample)
pnls <- pnls*sd(indeks)/sd(pnls)
pnls <- cbind(indeks, pnls)
colnames(pnls) <- c("Equal Weight", "Optimal")
dygraphs::dygraph(cumsum(pnls)[endp], main="Optimal Portfolio Returns With Eigen and Return Shrinkage") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyEvent(zoo::index(last(retsis[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=500)

# Define monthly end points
endp <- rutils::calc_endpoints(retsp, interval="months")
endp <- endp[endp > (nstocks+1)]
npts <- NROW(endp)
look_back <- 3
startp <- c(rep_len(0, look_back), endp[1:(npts-look_back)])
# Perform loop over end points
pnls <- lapply(2:npts, function(ep) {
    # Calculate the portfolio weights
    insample <- retsx[startp[ep-1]:endp[ep-1], ]
    invmat <- MASS::ginv(cov(insample))
    weightv <- invmat %*% colMeans(insample)
    weightv <- drop(weightv/sqrt(sum(weightv^2)))
    # Calculate the out-of-sample portfolio returns
    outsample <- retsp[(endp[ep-1]+1):endp[ep], ]
    xts::xts(outsample %*% weightv, zoo::index(outsample))
})  # end lapply
pnls <- do.call(rbind, pnls)

# Plot dygraph of rolling ETF portfolio strategy
pnls <- pnls*sd(indeks)/sd(pnls)
pnls <- rbind(indeks[paste0("/", start(pnls)-1)], pnls)
wealthv <- cbind(indeks, pnls, (pnls+indeks)/2)
colnames(wealthv) <- c("Index", "Strategy", "Combined")
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
dygraphs::dygraph(cumsum(wealthv)[endp], main="Monthly ETF Rolling Portfolio Strategy") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)

# Define monthly end points
look_back <- 3; dimax <- 9
startp <- c(rep_len(0, look_back), endp[1:(npts-look_back)])
# Perform loop over end points
pnls <- lapply(2:npts, function(ep) {
    # Calculate regularized inverse of covariance matrix
    insample <- retsx[startp[ep-1]:endp[ep-1], ]
    eigend <- eigen(cov(insample))
    eigenvec <- eigend$vectors
    eigenval <- eigend$values
    invmat <- eigenvec[, 1:dimax] %*%
(t(eigenvec[, 1:dimax]) / eigenval[1:dimax])
    # Calculate the maximum Sharpe ratio portfolio weights
    weightv <- invmat %*% colMeans(insample)
    weightv <- drop(weightv/sqrt(sum(weightv^2)))
    # Calculate the out-of-sample portfolio returns
    outsample <- retsp[(endp[ep-1]+1):endp[ep], ]
    xts::xts(outsample %*% weightv, zoo::index(outsample))
})  # end lapply
pnls <- do.call(rbind, pnls)

# Plot dygraph of rolling ETF portfolio strategy
pnls <- pnls*sd(indeks)/sd(pnls)
pnls <- rbind(indeks[paste0("/", start(pnls)-1)], pnls)
wealthv <- cbind(indeks, pnls, (pnls+indeks)/2)
colnames(wealthv) <- c("Index", "Strategy", "Combined")
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
dygraphs::dygraph(cumsum(wealthv)[endp], main="Rolling Portfolio Strategy With Dimension Reduction") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)

# Define the return shrinkage intensity
alpha <- 0.7
# Perform loop over end points
pnls <- lapply(2:npts, function(ep) {
    # Calculate regularized inverse of covariance matrix
    insample <- retsx[startp[ep-1]:endp[ep-1], ]
    eigend <- eigen(cov(insample))
    eigenvec <- eigend$vectors
    eigenval <- eigend$values
    invmat <- eigenvec[, 1:dimax] %*%
(t(eigenvec[, 1:dimax]) / eigenval[1:dimax])
    # Shrink the in-sample returns to their mean
    insample <- (1-alpha)*insample + alpha*rowMeans(insample)
    # Calculate the maximum Sharpe ratio portfolio weights
    weightv <- invmat %*% colMeans(insample)
    weightv <- drop(weightv/sqrt(sum(weightv^2)))
    # Calculate the out-of-sample portfolio returns
    outsample <- retsp[(endp[ep-1]+1):endp[ep], ]
    xts::xts(outsample %*% weightv, zoo::index(outsample))
})  # end lapply
pnls <- do.call(rbind, pnls)

# Plot dygraph of rolling ETF portfolio strategy
pnls <- pnls*sd(indeks)/sd(pnls)
pnls <- rbind(indeks[paste0("/", start(pnls)-1)], pnls)
wealthv <- cbind(indeks, pnls, (pnls+indeks)/2)
colnames(wealthv) <- c("Index", "Strategy", "Combined")
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
dygraphs::dygraph(cumsum(wealthv)[endp], main="Rolling Portfolio Strategy With Return Shrinkage") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)

# Define backtest functional for rolling portfolio strategy
roll_portf <- function(excess, # Excess returns
                 returns, # Stock returns
                 endp, # End points
                 look_back=12, # Look-back interval
                 dimax=3, # Dimension reduction intensity
                 alpha=0.0, # Return shrinkage intensity
                 bid_offer=0.0, # Bid-offer spread
                 ...) {
  npts <- NROW(endp)
  startp <- c(rep_len(0, look_back), endp[1:(npts-look_back)])
  pnls <- lapply(2:npts, function(ep) {
    # Calculate regularized inverse of covariance matrix
    insample <- excess[startp[ep-1]:endp[ep-1], ]
    eigend <- eigen(cov(insample))
    eigenvec <- eigend$vectors
    eigenval <- eigend$values
    invmat <- eigenvec[, 1:dimax] %*%
(t(eigenvec[, 1:dimax]) / eigenval[1:dimax])
    # Shrink the in-sample returns to their mean
    insample <- (1-alpha)*insample + alpha*rowMeans(insample)
    # Calculate the maximum Sharpe ratio portfolio weights
    weightv <- invmat %*% colMeans(insample)
    weightv <- drop(weightv/sqrt(sum(weightv^2)))
    # Calculate the out-of-sample portfolio returns
    outsample <- returns[(endp[ep-1]+1):endp[ep], ]
    xts::xts(outsample %*% weightv, zoo::index(outsample))
  })  # end lapply
  pnls <- do.call(rbind, pnls)
  # Add warmup period to pnls
  rbind(indeks[paste0("/", start(pnls)-1)], pnls)
}  # end roll_portf

# Simulate a monthly ETF momentum strategy
pnls <- roll_portf(excess=retsx, returns=retsp, endp=endp,
  look_back=look_back, dimax=dimax)
# Perform sapply loop over look_backs
look_backs <- seq(2, 15, by=1)
pnls <- lapply(look_backs, roll_portf,
  returns=retsp, excess=retsx, endp=endp, dimax=dimax)
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("look_back=", look_backs)
pnlsums <- sapply(pnls, sum)
look_back <- look_backs[which.max(pnlsums)]

# Plot dygraph of daily ETF momentum strategies
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls)[endp], main="Rolling Portfolio Strategies") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Plot EWMA strategies with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(pnls))
quantmod::chart_Series(cumsum(pnls),
  theme=plot_theme, name="Rolling Portfolio Strategies")
legend("bottomleft", legend=colnames(pnls),
  inset=0.02, bg="white", cex=0.7, lwd=rep(6, NCOL(retsp)),
  col=plot_theme$col$line.col, bty="n")

# Perform backtest for different dimax values
eigenvals <- 2:11
pnls <- lapply(eigenvals, roll_portf, excess=retsx,
  returns=retsp, endp=endp, look_back=look_back)
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("eigenval=", eigenvals)
pnlsums <- sapply(pnls, sum)
dimax <- eigenvals[which.max(pnlsums)]

# Plot dygraph of daily ETF momentum strategies
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls)[endp], main="Rolling Portfolio Strategies With Dimension Reduction") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Plot EWMA strategies with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(pnls))
quantmod::chart_Series(cumsum(pnls),
  theme=plot_theme, name="Rolling Portfolio Strategies")
legend("bottomleft", legend=colnames(pnls),
  inset=0.02, bg="white", cex=0.7, lwd=rep(6, NCOL(retsp)),
  col=plot_theme$col$line.col, bty="n")

# Perform backtest over vector of return shrinkage intensities
alphav <- seq(from=0.0, to=0.9, by=0.1)
pnls <- lapply(alphav, roll_portf, excess=retsx,
  returns=retsp, endp=endp, look_back=look_back, dimax=dimax)
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("alpha=", alphav)
pnlsums <- sapply(pnls, sum)
alpha <- alphav[which.max(pnlsums)]

# Plot dygraph of daily ETF momentum strategies
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls)[endp], main="Rolling Portfolio Strategies With Return Shrinkage") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Plot EWMA strategies with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(pnls))
quantmod::chart_Series(cumsum(pnls),
  theme=plot_theme, name="Rolling Portfolio Strategies")
legend("bottomleft", legend=colnames(pnls),
  inset=0.02, bg="white", cex=0.7, lwd=rep(6, NCOL(retsp)),
  col=plot_theme$col$line.col, bty="n")

load("/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
# Overwrite NA values in returns
retsp <- returns["2000/"]
nstocks <- NCOL(retsp)
retsp[1, is.na(retsp[1, ])] <- 0
retsp <- zoo::na.locf(retsp, na.rm=FALSE)
datev <- zoo::index(retsp)
riskf <- 0.03/252
retsx <- (retsp - riskf)
retsis <- retsp["/2010"]
retsos <- retsp["2011/"]
# Maximum Sharpe weights in-sample interval
covmat <- cov(retsis)
invmat <- MASS::ginv(covmat)
weightv <- invmat %*% colMeans(retsx["/2010"])
weightv <- drop(weightv/sqrt(sum(weightv^2)))
names(weightv) <- colnames(retsp)
# Calculate portfolio returns
insample <- xts::xts(retsis %*% weightv, zoo::index(retsis))
outsample <- xts::xts(retsos %*% weightv, zoo::index(retsos))
indeks <- xts::xts(rowMeans(retsp), datev)

# Combine in-sample and out-of-sample returns
pnls <- rbind(insample, outsample)
pnls <- pnls*sd(indeks)/sd(pnls)
pnls <- cbind(indeks, pnls)
colnames(pnls) <- c("Equal Weight", "Optimal")
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(pnls[index(outsample)],
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot the cumulative portfolio returns
endp <- rutils::calc_endpoints(pnls, interval="months")
dygraphs::dygraph(cumsum(pnls)[endp], main="Out-of-sample Optimal Portfolio Returns for Stocks") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyEvent(zoo::index(last(retsis[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=500)

# Calculate regularized inverse of covariance matrix
look_back <- 8; dimax <- 21
eigend <- eigen(cov(retsis))
eigenvec <- eigend$vectors
eigenval <- eigend$values
invmat <- eigenvec[, 1:dimax] %*%
  (t(eigenvec[, 1:dimax]) / eigenval[1:dimax])
# Calculate portfolio weights
weightv <- invmat %*% colMeans(retsx["/2010"])
weightv <- drop(weightv/sqrt(sum(weightv^2)))
names(weightv) <- colnames(retsp)
# Calculate portfolio returns
insample <- xts::xts(retsis %*% weightv, zoo::index(retsis))
outsample <- xts::xts(retsos %*% weightv, zoo::index(retsos))
indeks <- xts::xts(rowMeans(retsp), datev)

# Combine in-sample and out-of-sample returns
pnls <- rbind(insample, outsample)
pnls <- pnls*sd(indeks)/sd(pnls)
pnls <- cbind(indeks, pnls)
colnames(pnls) <- c("Equal Weight", "Optimal")
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(pnls[index(outsample)],
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot the cumulative portfolio returns
endp <- rutils::calc_endpoints(pnls, interval="months")
dygraphs::dygraph(cumsum(pnls)[endp], main="Out-of-sample Returns for Stocks with Dimension Reduction") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyEvent(zoo::index(last(retsis[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=500)

# Shrink the in-sample returns to their mean
alpha <- 0.7
retsxm <- rowMeans(retsx["/2010"])
retsxis <- (1-alpha)*retsx["/2010"] + alpha*retsxm
# Calculate portfolio weights
weightv <- invmat %*% colMeans(retsxis)
weightv <- drop(weightv/sqrt(sum(weightv^2)))
# Calculate portfolio returns
insample <- xts::xts(retsis %*% weightv, zoo::index(retsis))
outsample <- xts::xts(retsos %*% weightv, zoo::index(retsos))

# Combine in-sample and out-of-sample returns
pnls <- rbind(insample, outsample)
pnls <- pnls*sd(indeks)/sd(pnls)
pnls <- cbind(indeks, pnls)
colnames(pnls) <- c("Equal Weight", "Optimal")
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(pnls[index(outsample)],
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot the cumulative portfolio returns
dygraphs::dygraph(cumsum(pnls)[endp], main="Out-of-sample Returns for Stocks with Return Shrinkage") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyEvent(zoo::index(last(retsis[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=500)

library(RcppArmadillo)
# Source Rcpp functions from file
Rcpp::sourceCpp("/Users/jerzy/Develop/lecture_slides/scripts/back_test.cpp")
# Create random matrix of returns
matrixv <- matrix(rnorm(300), nc=5)
# Regularized inverse of covariance matrix
dimax <- 4
eigend <- eigen(cov(matrixv))
covinv <- eigend$vectors[, 1:dimax] %*%
  (t(eigend$vectors[, 1:dimax]) / eigend$values[1:dimax])
# Regularized inverse using RcppArmadillo
covinv_arma <- calc_inv(matrixv, dimax)
all.equal(covinv, covinv_arma)
# Microbenchmark RcppArmadillo code
library(microbenchmark)
summary(microbenchmark(
  rcode={eigend <- eigen(cov(matrixv))
    eigend$vectors[, 1:dimax] %*%
(t(eigend$vectors[, 1:dimax]) / eigend$values[1:dimax])
  },
  cppcode=calc_inv(matrixv, dimax),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary

# Overwrite NA values in returns100
retsp <- returns100
retsp[1, is.na(retsp[1, ])] <- 0
retsp <- zoo::na.locf(retsp, na.rm=FALSE)
retsx <- (retsp - riskf)
nstocks <- NCOL(retsp) ; datev <- zoo::index(retsp)
# Define monthly end points
endp <- rutils::calc_endpoints(retsp, interval="months")
endp <- endp[endp > (nstocks+1)]
npts <- NROW(endp) ; look_back <- 12
startp <- c(rep_len(0, look_back), endp[1:(npts-look_back)])
# Perform loop over end points - takes very long !!!
pnls <- lapply(2:npts, function(ep) {
    # Subset the excess returns
    insample <- retsx[startp[ep-1]:endp[ep-1], ]
    invmat <- MASS::ginv(cov(insample))
    # Calculate the maximum Sharpe ratio portfolio weights
    weightv <- invmat %*% colMeans(insample)
    weightv <- drop(weightv/sqrt(sum(weightv^2)))
    # Calculate the out-of-sample portfolio returns
    outsample <- retsp[(endp[ep-1]+1):endp[ep], ]
    xts::xts(outsample %*% weightv, zoo::index(outsample))
})  # end lapply
pnls <- rutils::do_call(rbind, pnls)

# Calculate returns of equal weight portfolio
indeks <- xts::xts(rowMeans(retsp), datev)
pnls <- rbind(indeks[paste0("/", start(pnls)-1)], pnls*sd(indeks)/sd(pnls))
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(indeks, pnls)
colnames(wealthv) <- c("Equal Weight", "Strategy")
sqrt(252)*sapply(wealthv,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot cumulative strategy returns
dygraphs::dygraph(cumsum(wealthv)[endp], main="Rolling Portfolio Optimization Strategy for S&P500 Stocks") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)

# Shift end points to C++ convention
endp <- (endp - 1)
endp[endp < 0] <- 0
startp <- (startp - 1)
startp[startp < 0] <- 0
# Specify dimension reduction and return shrinkage using list of portfolio optimization parameters
controlv <- HighFreq::param_portf(method="maxsharpe", dimax=21, alpha=0.7)
# Perform backtest in Rcpp
pnls <- HighFreq::back_test(excess=retsx, returns=retsp,
  startp=startp, endp=endp, controlv=controlv)
pnls <- pnls*sd(indeks)/sd(pnls)

# Plot cumulative strategy returns
wealthv <- cbind(indeks, pnls, (pnls+indeks)/2)
colnames(wealthv) <- c("Index", "Strategy", "Combined")
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
dygraphs::dygraph(cumsum(wealthv)[endp], main="Rolling S&P500 Portfolio Optimization Strategy With Shrinkage") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)

# Perform backtest over vector of return shrinkage intensities
alphav <- seq(from=0.01, to=0.91, by=0.1)
pnls <- lapply(alphav, function(alpha) {
  HighFreq::back_test(excess=retsx, returns=retsp,
  startp=startp, endp=endp, controlv=controlv)
})  # end lapply
profilev <- sapply(pnls, sum)
plot(x=alphav, y=profilev, t="l", main="Rolling Strategy as Function of Return Shrinkage",
  xlab="Shrinkage Intensity Alpha", ylab="pnl")
whichmax <- which.max(profilev)
alpha <- alphav[whichmax]
pnls <- pnls[[whichmax]]
# Perform backtest over vector of dimension reduction eigenvals
eigenvals <- seq(from=3, to=40, by=2)
pnls <- lapply(eigenvals, function(dimax) {
  HighFreq::back_test(excess=retsx, returns=retsp,
    startp=startp, endp=endp, controlv=controlv)
})  # end lapply
profilev <- sapply(pnls, sum)
plot(x=eigenvals, y=profilev, t="l", main="Strategy PnL as Function of dimax",
  xlab="dimax", ylab="pnl")
whichmax <- which.max(profilev)
dimax <- eigenvals[whichmax]
pnls <- pnls[[whichmax]]
pnls <- pnls*sd(indeks)/sd(pnls)

# Plot cumulative strategy returns
wealthv <- cbind(indeks, pnls, (pnls+indeks)/2)
colnames(wealthv) <- c("Index", "Strategy", "Combined")
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
dygraphs::dygraph(cumsum(wealthv)[endp], main="Optimal Rolling S&P500 Portfolio Strategy") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)

# Perform backtest over look-backs
look_backs <- seq(from=3, to=12, by=1)
pnls <- lapply(look_backs, function(look_back) {
  startp <- c(rep_len(0, look_back), endp[1:(npts-look_back)])
  startp <- (startp - 1)
  startp[startp < 0] <- 0
  HighFreq::back_test(excess=retsx, returns=retsp,
    startp=startp, endp=endp, controlv=controlv)
})  # end lapply
profilev <- sapply(pnls, sum)
plot(x=look_backs, y=profilev, t="l", main="Strategy PnL as Function of Look-back Interval",
  xlab="Look-back Interval", ylab="pnl")
whichmax <- which.max(profilev)
look_back <- look_backs[whichmax]
pnls <- pnls[[whichmax]]
pnls <- pnls*sd(indeks)/sd(pnls)

# Plot cumulative strategy returns
wealthv <- cbind(indeks, pnls, (pnls+indeks)/2)
colnames(wealthv) <- c("Index", "Strategy", "Combined")
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
dygraphs::dygraph(cumsum(wealthv)[endp], main="Optimal Rolling S&P500 Portfolio Strategy") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)

# Linear constraint
weightv <- weightv/sum(weightv)
# Quadratic constraint
weightv <- weightv/sqrt(sum(weightv^2))
# Box constraints
weightv[weightv > 1] <- 1
weightv[weightv < 0] <- 0

library(rutils)
library(Rglpk)
# Vector of symbol names
symbolv <- c("VTI", "IEF", "DBC")
nstocks <- NROW(symbolv)
# Calculate mean returns
retsp <- na.omit(rutils::etfenv$returns[, symbolv])
retsm <- colMeans(retsp)
# Specify linear constraint coefficients
lincon <- matrix(c(rep(1, nstocks), 1, 1, 0),
                 nc=nstocks, byrow=TRUE)
directs <- c("==", "<=")
rhs <- c(1, 0)
# Specify box constraints (-1, 1) (default is c(0, Inf))
boxc <- list(lower=list(ind=1:nstocks, val=rep(-1, nstocks)),
         upper=list(ind=1:nstocks, val=rep(1, nstocks)))
# Perform optimization
optiml <- Rglpk::Rglpk_solve_LP(
  obj=retsm,
  mat=lincon,
  dir=directs,
  rhs=rhs,
  bounds=boxc,
  max=TRUE)
unlist(optiml[1:2])

# Calculate covariance matrix of returns and its inverse
covmat <- cov(retsp)
covinv <- solve(a=covmat)
unitv <- rep(1, NCOL(covmat))
# Minimum variance weights with constraint
# weightv <- solve(a=covmat, b=unitv)
weightv <- covinv %*% unitv
weightv <- weightv/drop(t(unitv) %*% weightv)
# Minimum variance
t(weightv) %*% covmat %*% weightv
1/(t(unitv) %*% covinv %*% unitv)

# Calculate vector of mean returns
retsm <- colMeans(retsp)
# Specify the target return
rett <- 1.5*mean(retsp)
# Products of inverse with mean returns and unit vector
fmat <- matrix(c(
  t(unitv) %*% covinv %*% unitv,
  t(unitv) %*% covinv %*% retsm,
  t(retsm) %*% covinv %*% unitv,
  t(retsm) %*% covinv %*% retsm), nc=2)
# Solve for the Lagrange multipliers
lagm <- solve(a=fmat, b=c(2, 2*rett))
# Calculate weights
weightv <- drop(0.5*covinv %*% cbind(unitv, retsm) %*% lagm)
# Calculate constraints
all.equal(1, sum(weightv))
all.equal(rett, sum(retsm*weightv))

# Calculate portfolio return and standard deviation
retsp <- drop(retsp %*% weightv)
c(return=mean(retsp), sd=sd(retsp))
all.equal(mean(retsp), rett)
# Calculate portfolio variance
uu <- c(1, rett)
finv <- solve(fmat)
all.equal(var(retsp), drop(t(uu) %*% finv %*% uu))
# Calculate vertex of variance parabola
weightv <- drop(covinv %*% unitv /
  drop(t(unitv) %*% covinv %*% unitv))
retsp <- drop(retsp %*% weightv)
retsv <- drop(t(unitv) %*% covinv %*% retsm /
  t(unitv) %*% covinv %*% unitv)
all.equal(mean(retsp), retsv)
varmin <- drop(1/t(unitv) %*% covinv %*% unitv)
all.equal(var(retsp), varmin)

# Calculate efficient frontier from target returns
retst <- retsv*(1+seq(from=(-1), to=1, by=0.1))
effront <- sapply(retst, function(rett) {
  uu <- c(1, rett)
  sqrt(drop(t(uu) %*% finv %*% uu))
})  # end sapply
# Plot efficient frontier
x11(width=6, height=5)
plot(x=effront, y=retst, t="l", col="blue", lwd=2,
     main="Efficient Frontier and Minimum Variance Portfolio",
     xlab="standard deviation", ylab="return")
points(x=sqrt(varmin), y=retsv, col="green", lwd=6)
text(x=sqrt(varmin), y=retsv, labels="minimum \nvariance",
     pos=4, cex=0.8)

# Calculate portfolio standard deviation
stdev <- sqrt(drop(t(uu) %*% finv %*% uu))
# Calculate the slope of the tangent line
slopev <- (stdev*det(fmat))/(fmat[1, 1]*rett-fmat[1, 2])
# Calculate the risk-free rate as intercept of the tangent line
riskf <- rett - slopev*stdev
# Calculate the risk-free rate from target return
riskf <- (rett*fmat[1, 2]-fmat[2, 2]) /
  (rett*fmat[1, 1]-fmat[1, 2])

# Plot efficient frontier
plot(x=effront, y=retst, t="l", col="blue", lwd=2,
     xlim=c(0.0, max(effront)),
     main="Efficient Frontier and Tangency Portfolio",
     xlab="standard deviation", ylab="return")
# Plot minimum variance
points(x=sqrt(varmin), y=retsv, col="green", lwd=6)
text(x=sqrt(varmin), y=retsv, labels="minimum \nvariance",
     pos=4, cex=0.8)
# Plot tangent point
points(x=stdev, y=rett, col="red", lwd=6)
text(x=stdev, y=rett, labels="tangency\nportfolio", pos=2, cex=0.8)
# Plot risk-free point
points(x=0, y=riskf, col="red", lwd=6)
text(x=0, y=riskf, labels="risk-free", pos=4, cex=0.8)
# Plot tangent line
abline(a=riskf, b=slopev, lwd=2, col="green")

# Calculate excess returns
riskf <- 0.03/252
retsx <- (retsp - riskf)
# Calculate covariance and inverse matrix
covmat <- cov(retsp)
unitv <- rep(1, NCOL(covmat))
covinv <- solve(a=covmat)
# Calculate mean excess returns
retsx <- sapply(retsx, mean)
# Weights of maximum Sharpe portfolio
# weightv <- solve(a=covmat, b=returns)
weightv <- covinv %*% retsx
weightv <- weightv/drop(t(unitv) %*% weightv)
# Sharpe ratios
sqrt(252)*sum(weightv*retsx) /
  sqrt(drop(weightv %*% covmat %*% weightv))
sapply(retsp - riskf, function(x) sqrt(252)*mean(x)/sd(x))
maxsharpe <- weightv

library(rutils)
# Calculate minimum variance weights
weightv <- covinv %*% unitv
minvar <- weightv/drop(t(unitv) %*% weightv)
# Calculate optimal portfolio returns
retsoptim <- xts(
  x=cbind(exp(cumsum(retsp %*% maxsharpe)),
    exp(cumsum(retsp %*% minvar))),
  order.by=zoo::index(retsp))
colnames(retsoptim) <- c("maxsharpe", "minvar")
# Plot optimal portfolio returns, with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "green")
x11(width=6, height=5)
chart_Series(retsoptim, theme=plot_theme,
  name="Maximum Sharpe and
  Minimum Variance portfolios")
legend("top", legend=colnames(retsoptim), cex=0.8,
 inset=0.1, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")

x11(widthp <- 6, heightp <- 6)
# Calculate minimum variance weights
weightv <- covinv %*% unitv
weightv <- weightv/drop(t(unitv) %*% weightv)
# Minimum standard deviation and return
stdev <- sqrt(252*drop(weightv %*% covmat %*% weightv))
retsp <- 252*sum(weightv*retsm)
# Calculate maximum Sharpe portfolios
riskf <- (retsp * seq(-10, 10, by=0.1)^3)/252
effront <- sapply(riskf, function(riskf) {
  weightv <- covinv %*% (retsm - riskf)
  weightv <- weightv/drop(t(unitv) %*% weightv)
  # Portfolio return and standard deviation
  c(return=252*sum(weightv*retsm),
    stddev=sqrt(252*drop(weightv %*% covmat %*% weightv)))
})  # end sapply
effront <- cbind(252*riskf, t(effront))
colnames(effront)[1] <- "risk-free"
effront <- effront[is.finite(effront[, "stddev"]), ]
effront <- effront[order(effront[, "return"]), ]
# Plot maximum Sharpe portfolios
plot(x=effront[, "stddev"],
     y=effront[, "return"], t="l",
     xlim=c(0.0*stdev, 3.0*stdev),
     ylim=c(0.0*retsp, 2.0*retsp),
     main="Efficient Frontier and Capital Market Line",
     xlab="standard deviation", ylab="return")
points(x=effront[, "stddev"], y=effront[, "return"],
 col="red", lwd=3)

# Plot minimum variance portfolio
points(x=stdev, y=retsp, col="green", lwd=6)
text(stdev, retsp, labels="minimum \nvariance",
     pos=4, cex=0.8)
# Draw Capital Market Line
sortv <- sort(effront[, 1])
riskf <- sortv[findInterval(x=0.5*retsp, vec=sortv)]
points(x=0, y=riskf, col="blue", lwd=6)
text(x=0, y=riskf, labels="risk-free",
     pos=4, cex=0.8)
marketp <- match(riskf, effront[, 1])
points(x=effront[marketp, "stddev"],
 y=effront[marketp, "return"],
 col="blue", lwd=6)
text(x=effront[marketp, "stddev"],
     y=effront[marketp, "return"],
     labels="market portfolio",
     pos=2, cex=0.8)
sharper <- (effront[marketp, "return"]-riskf)/
  effront[marketp, "stddev"]
abline(a=riskf, b=sharper, col="blue", lwd=2)
text(x=0.7*effront[marketp, "stddev"],
     y=0.7*effront[marketp, "return"]+0.01,
     labels="Capital Market Line", pos=2, cex=0.8,
     srt=45*atan(sharper*heightp/widthp)/(0.25*pi))

# Calculate random portfolios
nportf <- 1000
randportf <- sapply(1:nportf, function(it) {
  weightv <- runif(nstocks-1, min=-0.25, max=1.0)
  weightv <- c(weightv, 1-sum(weightv))
  # Portfolio return and standard deviation
  c(return=252*sum(weightv*retsm),
    stddev=sqrt(252*drop(weightv %*% covmat %*% weightv)))
})  # end sapply
# Plot scatterplot of random portfolios
x11(widthp <- 6, heightp <- 6)
plot(x=randportf["stddev", ], y=randportf["return", ],
     main="Efficient Frontier and Random Portfolios",
     xlim=c(0.5*stdev, 0.8*max(randportf["stddev", ])),
     xlab="standard deviation", ylab="return")
# Plot maximum Sharpe portfolios
lines(x=effront[, "stddev"],
     y=effront[, "return"], lwd=2)
points(x=effront[, "stddev"], y=effront[, "return"],
 col="red", lwd=3)
# Plot minimum variance portfolio
points(x=stdev, y=retsp, col="green", lwd=6)
text(stdev, retsp, labels="minimum\nvariance",
     pos=2, cex=0.8)
# Plot market portfolio
points(x=effront[marketp, "stddev"],
 y=effront[marketp, "return"], col="green", lwd=6)
text(x=effront[marketp, "stddev"],
     y=effront[marketp, "return"],
     labels="market\nportfolio",
     pos=2, cex=0.8)

# Plot individual assets
points(x=sqrt(252*diag(covmat)),
 y=252*retsm, col="blue", lwd=6)
text(x=sqrt(252*diag(covmat)), y=252*retsm,
     labels=names(retsm),
     col="blue", pos=1, cex=0.8)

riskf <- 0.03
retsp <- c(asset1=0.05, asset2=0.06)
stdevs <- c(asset1=0.4, asset2=0.5)
corrp <- 0.6
covmat <- matrix(c(1, corrp, corrp, 1), nc=2)
covmat <- t(t(stdevs*covmat)*stdevs)
weightv <- seq(from=(-1), to=2, length.out=31)
weightv <- cbind(weightv, 1-weightv)
retsp <- weightv %*% retsp
portfsd <- sqrt(rowSums(weightv*(weightv %*% covmat)))
sharper <- (retsp-riskf)/portfsd
whichmax <- which.max(sharper)
sharpem <- max(sharper)
# Plot efficient frontier
x11(widthp <- 6, heightp <- 5)
par(mar=c(3,3,2,1)+0.1, oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
plot(portfsd, retsp, t="l",
 main=paste0("Efficient frontier and CML for two assets\ncorrelation = ", 100*corrp, "%"),
 xlab="standard deviation", ylab="return",
 lwd=2, col="orange",
 xlim=c(0, max(portfsd)),
 ylim=c(0.02, max(retsp)))
# Add Market Portfolio (maximum Sharpe ratio portfolio)
points(portfsd[whichmax], retsp[whichmax],
 col="blue", lwd=3)
text(x=portfsd[whichmax], y=retsp[whichmax],
     labels=paste(c("market portfolio\n",
 structure(c(weightv[whichmax], 1-weightv[whichmax]),
         names=names(retsp))), collapse=" "),
     pos=2, cex=0.8)

# Plot individual assets
points(stdevs, retsp, col="green", lwd=3)
text(stdevs, retsp, labels=names(retsp), pos=4, cex=0.8)
# Add point at risk-free rate and draw Capital Market Line
points(x=0, y=riskf, col="blue", lwd=3)
text(0, riskf, labels="risk-free\nrate", pos=4, cex=0.8)
abline(a=riskf, b=sharpem, lwd=2, col="blue")
range_s <- par("usr")
text(portfsd[whichmax]/2, (retsp[whichmax]+riskf)/2,
     labels="Capital Market Line", cex=0.8, , pos=3,
     srt=45*atan(sharpem*(range_s[2]-range_s[1])/
             (range_s[4]-range_s[3])*
             heightp/widthp)/(0.25*pi))

# Plot portfolios in x11() window
x11(widthp <- 6, heightp <- 5)
par(oma=c(0, 0, 0, 0), mar=c(3,3,2,1)+0.1, mgp=c(2, 1, 0), cex.lab=1.0, cex.axis=1.0, cex.main=1.0, cex.sub=1.0)
# Vector of symbol names
symbolv <- c("VTI", "IEF")
# Matrix of portfolio weights
weightv <- seq(from=(-1), to=2, length.out=31)
weightv <- cbind(weightv, 1-weightv)
# Calculate portfolio returns and volatilities
retsp <- rutils::etfenv$returns[, symbolv]
retsp <- retsp %*% t(weightv)
portfv <- cbind(252*colMeans(retsp),
  sqrt(252)*matrixStats::colSds(retsp))
colnames(portfv) <- c("returns", "stddev")
riskf <- 0.06
portfv <- cbind(portfv,
  (portfv[, "returns"]-riskf)/portfv[, "stddev"])
colnames(portfv)[3] <- "Sharpe"
whichmax <- which.max(portfv[, "Sharpe"])
sharpem <- portfv[whichmax, "Sharpe"]
plot(x=portfv[, "stddev"], y=portfv[, "returns"],
     main="Stock and Bond portfolios", t="l",
     xlim=c(0, 0.7*max(portfv[, "stddev"])), ylim=c(0, max(portfv[, "returns"])),
     xlab="standard deviation", ylab="return")
# Add blue point for market portfolio
points(x=portfv[whichmax, "stddev"], y=portfv[whichmax, "returns"], col="blue", lwd=6)
text(x=portfv[whichmax, "stddev"], y=portfv[whichmax, "returns"],
     labels=paste(c("market portfolio\n",
  structure(c(weightv[whichmax, 1], weightv[whichmax, 2]), names=symbolv)), collapse=" "),
     pos=3, cex=0.8)

# Plot individual assets
retsm <- 252*sapply(retsp, mean)
stdevs <- sqrt(252)*sapply(retsp, sd)
points(stdevs, retsm, col="green", lwd=6)
text(stdevs, retsm, labels=names(retsp), pos=2, cex=0.8)
# Add point at risk-free rate and draw Capital Market Line
points(x=0, y=riskf, col="blue", lwd=6)
text(0, riskf, labels="risk-free", pos=4, cex=0.8)
abline(a=riskf, b=sharpem, col="blue", lwd=2)
range_s <- par("usr")
text(max(portfv[, "stddev"])/3, 0.75*max(portfv[, "returns"]),
     labels="Capital Market Line", cex=0.8, , pos=3,
     srt=45*atan(sharpem*(range_s[2]-range_s[1])/
             (range_s[4]-range_s[3])*
             heightp/widthp)/(0.25*pi))

# Plot portfolios in x11() window
x11(widthp <- 6, heightp <- 5)
# Calculate cumulative returns of VTI and IEF
retsoptim <- lapply(retsp,
  function(retsp) exp(cumsum(retsp)))
retsoptim <- rutils::do_call(cbind, retsoptim)
# Calculate market portfolio returns
retsoptim <- cbind(exp(cumsum(retsp %*%
    c(weightv[whichmax], 1-weightv[whichmax]))),
  retsoptim)
colnames(retsoptim)[1] <- "market"
# Plot market portfolio with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue", "green")
chart_Series(retsoptim, theme=plot_theme,
       name="Market portfolio for stocks and bonds")
legend("top", legend=colnames(retsoptim),
 cex=0.8, inset=0.1, bg="white", lty=1,
 lwd=6, col=plot_theme$col$line.col, bty="n")

x11(width=6, height=4)
par(mar=c(3, 2, 1, 0), oma=c(0, 0, 0, 0))
# VTI percentage returns
retsp <- rutils::diffit(log(quantmod::Cl(rutils::etfenv$VTI)))
confl <- 0.1
varisk <- quantile(retsp, confl)
cvar <- mean(retsp[retsp < varisk])
# Or
sortv <- sort(as.numeric(retsp))
varind <- round(confl*NROW(retsp))
varisk <- sortv[varind]
cvar <- mean(sortv[1:varind])
# Plot histogram of VTI returns
varmin <- (-0.05)
histp <- hist(retsp, col="lightgrey",
  xlab="returns", breaks=100, xlim=c(varmin, 0.01),
  ylab="frequency", freq=FALSE, main="VTI Returns Histogram")

# Plot density of losses
densv <- density(retsp, adjust=1.5)
lines(densv, lwd=3, col="blue")
# Add line for VaR
abline(v=varisk, col="red", lwd=3)
ymax <- max(densv$y)
text(x=varisk, y=2*ymax/3, labels="VaR", lwd=2, pos=2)
# Add shading for CVaR
rangev <- (densv$x < varisk) & (densv$x > varmin)
polygon(
  c(varmin, densv$x[rangev], varisk),
  c(0, densv$y[rangev], 0),
  col=rgb(1, 0, 0,0.5), border=NA)
text(x=1.5*varisk, y=ymax/7, labels="CVaR", lwd=2, pos=2)

library(rutils)  # Load rutils
library(Rglpk)
# Vector of symbol names and returns
symbolv <- c("VTI", "IEF", "DBC")
nstocks <- NROW(symbolv)
retsp <- na.omit(rutils::etfenv$returns[, symbolv])
retsm <- colMeans(retsp)
confl <- 0.05
rmin <- 0 ; wmin <- 0 ; wmax <- 1
weightsum <- 1
ncols <- NCOL(retsp) # number of assets
nrows <- NROW(retsp) # number of rows
# Create objective vector
objvec <- c(numeric(ncols), rep(-1/(confl/nrows), nrows), -1)
# Specify linear constraint coefficients
lincon <- rbind(cbind(rbind(1, retsm),
                matrix(data=0, nrow=2, ncol=(nrows+1))),
          cbind(coredata(retsp), diag(nrows), 1))
rhs <- c(weightsum, rmin, rep(0, nrows))
directs <- c("==", ">=", rep(">=", nrows))
# Specify box constraints (wmin, wmax) (default is c(0, Inf))
boxc <- list(lower=list(ind=1:ncols, val=rep(wmin, ncols)),
         upper=list(ind=1:ncols, val=rep(wmax, ncols)))
# Perform optimization
optiml <- Rglpk_solve_LP(obj=objvec, mat=lincon, dir=directs, rhs=rhs, types=rep("C", NROW(objvec)), max=T, bounds=boxc)
optiml$solution
lincon %*% optiml$solution
objvec %*% optiml$solution
as.numeric(optiml$solution[1:ncols])

# Calculate daily percentage returns
symbolv <- c("VTI", "IEF", "DBC")
retsp <- rutils::etfenv$returns[, symbolv]
# Create initial vector of portfolio weights
weightv <- rep(1, NROW(symbolv))
names(weightv) <- symbolv
# Objective equal to minus Sharpe ratio
objfun <- function(weightv, retsp) {
  retsp <- retsp %*% weightv
  if (sd(retsp) == 0)
    return(0)
  else
    -return(mean(retsp)/sd(retsp))
}  # end objfun
# Objective for equal weight portfolio
objfun(weightv, retsp=retsp)
optiml <- unlist(optimize(
  f=function(weight)
    objfun(c(1, 1, weight), retsp=retsp),
  interval=c(-4, 1)))
# Vectorize objective function with respect to third weight
objvec <- function(weightv) sapply(weightv,
  function(weight) objfun(c(1, 1, weight),
    retsp=retsp))
# Or
objvec <- Vectorize(FUN=function(weight)
    objfun(c(1, 1, weight), retsp=retsp),
  vectorize.args="weight")  # end Vectorize
objvec(1)
objvec(1:3)

x11(width=6, height=5)
par(oma=c(1, 1, 1, 1), mgp=c(2, 1, 0), mar=c(3, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# Plot objective function with respect to third weight
curve(expr=objvec,
      type="l", xlim=c(-4.0, 1.0),
      xlab=paste("weight of", names(weightv[3])),
      ylab="", lwd=2)
title(main="Objective Function", line=(-1))  # Add title
points(x=optiml[1], y=optiml[2], col="green", lwd=6)
text(x=optiml[1], y=optiml[2],
     labels="minimum objective", pos=4, cex=0.8)

#below is simplified code for plotting objective function
# Create vector of DBC weights
weightv <- seq(from=-4, to=1, by=0.1)
obj_val <- sapply(weightv,
  function(weight) objfun(c(1, 1, weight)))
plot(x=weightv, y=obj_val, t="l",
      xlab="weight of DBC", ylab="", lwd=2)
title(main="Objective Function", line=(-1))  # Add title
points(x=optiml[1], y=optiml[2], col="green", lwd=6)
text(x=optiml[1], y=optiml[2],
     labels="minimum objective", pos=4, cex=0.8)

# Vectorize function with respect to all weights
objvec <- Vectorize(
  FUN=function(w1, w2, w3) objfun(c(w1, w2, w3)),
  vectorize.args=c("w2", "w3"))  # end Vectorize
# Calculate objective on 2-d (w2 x w3) parameter grid
w2 <- seq(-3, 7, length=50)
w3 <- seq(-5, 5, length=50)
grid_object <- outer(w2, w3, FUN=objvec, w1=1)
rownames(grid_object) <- round(w2, 2)
colnames(grid_object) <- round(w3, 2)
# Perspective plot of objective function
persp(w2, w3, -grid_object,
theta=45, phi=30, shade=0.5,
col=rainbow(50), border="green",
main="objective function")

# Interactive perspective plot of objective function
library(rgl)
rgl::persp3d(z=-grid_object, zlab="objective",
  col="green", main="objective function")
rgl::persp3d(
  x=function(w2, w3) {-objvec(w1=1, w2, w3)},
  xlim=c(-3, 7), ylim=c(-5, 5),
  col="green", axes=FALSE)

# Optimization to find weights with maximum Sharpe ratio
optiml <- optim(par=weightv,
             fn=objfun,
             retsp=retsp,
             method="L-BFGS-B",
             upper=c(1.1, 10, 10),
             lower=c(0.9, -10, -10))
# Optimal parameters
optiml$par
optiml$par <- optiml$par/sum(optiml$par)
# Optimal Sharpe ratio
-objfun(optiml$par)

x11(width=6, height=5)
par(oma=c(1, 1, 1, 0), mgp=c(2, 1, 0), mar=c(2, 1, 2, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# Plot in two vertical panels
layout(matrix(c(1,2), 2),
 widths=c(1,1), heights=c(1,3))
# barplot of optimal portfolio weights
barplot(optiml$par, col=c("red", "green", "blue"),
  main="Optimized portfolio weights")
# Calculate cumulative returns of VTI, IEF, DBC
retc <- lapply(retsp,
  function(retsp) exp(cumsum(retsp)))
retc <- rutils::do_call(cbind, retc)
# Calculate optimal portfolio returns with VTI, IEF, DBC
retsoptim <- cbind(
  exp(cumsum(retsp %*% optiml$par)),
  retc)
colnames(retsoptim)[1] <- "retsoptim"
# Plot optimal returns with VTI, IEF, DBC
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("black", "red", "green", "blue")
chart_Series(retsoptim, theme=plot_theme,
       name="Optimized portfolio performance")
legend("top", legend=colnames(retsoptim), cex=0.8,
 inset=0.1, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")
# Or plot non-compounded (simple) cumulative returns
PerformanceAnalytics::chart.CumReturns(
  cbind(retsp %*% optiml$par, retsp),
  lwd=2, ylab="", legend.loc="topleft", main="")

riskf <- 0.03
retsp <- c(asset1=0.05, asset2=0.06)
stdevs <- c(asset1=0.4, asset2=0.5)
corrp <- 0.6
covmat <- matrix(c(1, corrp, corrp, 1), nc=2)
covmat <- t(t(stdevs*covmat)*stdevs)
library(quadprog)
# Minimum variance weights without constraints
optiml <- solve.QP(Dmat=2*covmat,
            dvec=rep(0, 2),
            Amat=matrix(0, nr=2, nc=1),
            bvec=0)
# Minimum variance weights sum equal to 1
optiml <- solve.QP(Dmat=2*covmat,
            dvec=rep(0, 2),
            Amat=matrix(1, nr=2, nc=1),
            bvec=1)
# Optimal value of objective function
t(optiml$solution) %*% covmat %*% optiml$solution
Perform simple optimization for reference
# Objective function for simple optimization
objfun <- function(x) {
  x <- c(x, 1-x)
  t(x) %*% covmat %*% x
}  # end objfun
unlist(optimize(f=objfun, interval=c(-1, 2)))

# Calculate daily percentage returns
symbolv <- c("VTI", "IEF", "DBC")
retsp <- rutils::etfenv$returns[, symbolv]
# Calculate the covariance matrix
covmat <- cov(retsp)
# Minimum variance weights, with sum equal to 1
optiml <- quadprog::solve.QP(Dmat=2*covmat,
            dvec=numeric(3),
            Amat=matrix(1, nr=3, nc=1),
            bvec=1)
# Minimum variance, maximum returns
optiml <- quadprog::solve.QP(Dmat=2*covmat,
            dvec=apply(0.1*retsp, 2, mean),
            Amat=matrix(1, nr=3, nc=1),
            bvec=1)
# Minimum variance positive weights, sum equal to 1
a_mat <- cbind(matrix(1, nr=3, nc=1),
       diag(3), -diag(3))
b_vec <- c(1, rep(0, 3), rep(-1, 3))
optiml <- quadprog::solve.QP(Dmat=2*covmat,
            dvec=numeric(3),
            Amat=a_mat,
            bvec=b_vec,
            meq=1)

# Rastrigin function with vector argument for optimization
rastrigin <- function(vectorv, param=25){
  sum(vectorv^2 - param*cos(vectorv))
}  # end rastrigin
vectorv <- c(pi/6, pi/6)
rastrigin(vectorv=vectorv)
library(DEoptim)
# Optimize rastrigin using DEoptim
optiml <-  DEoptim(rastrigin,
  upper=c(6, 6), lower=c(-6, -6),
  DEoptim.control(trace=FALSE, itermax=50))
# Optimal parameters and value
optiml$optim$bestmem
rastrigin(optiml$optim$bestmem)
summary(optiml)
plot(optiml)

# Calculate daily percentage returns
retsp <- rutils::etfenv$returns[, symbolv]
# Objective equal to minus Sharpe ratio
objfun <- function(weightv, retsp) {
  retsp <- retsp %*% weightv
  if (sd(retsp) == 0)
    return(0)
  else
    -return(mean(retsp)/sd(retsp))
}  # end objfun
# Perform optimization using DEoptim
optiml <- DEoptim::DEoptim(fn=objfun,
  upper=rep(10, NCOL(retsp)),
  lower=rep(-10, NCOL(retsp)),
  retsp=retsp,
  control=list(trace=FALSE, itermax=100, parallelType=1))
weightv <- optiml$optim$bestmem/sum(abs(optiml$optim$bestmem))
names(weightv) <- colnames(retsp)

# Objective with shrinkage penalty
objfun <- function(weightv, retsp, lambda, alpha) {
  retsp <- retsp %*% weightv
  if (sd(retsp) == 0)
    return(0)
  else {
    penaltyv <- lambda*((1-alpha)*sum(weightv^2) +
alpha*sum(abs(weightv)))
    -return(mean(retsp)/sd(retsp) + penaltyv)
  }
}  # end objfun
# Objective for equal weight portfolio
weightv <- rep(1, NROW(symbolv))
names(weightv) <- symbolv
lambda <- 0.5 ; alpha <- 0.5
objfun(weightv, retsp=retsp, lambda=lambda, alpha=alpha)
# Perform optimization using DEoptim
optiml <- DEoptim::DEoptim(fn=objfun,
  upper=rep(10, NCOL(retsp)),
  lower=rep(-10, NCOL(retsp)),
  retsp=retsp,
  lambda=lambda,
  alpha=alpha,
  control=list(trace=FALSE, itermax=100, parallelType=1))
weightv <- optiml$optim$bestmem/sum(abs(optiml$optim$bestmem))
names(weightv) <- colnames(retsp)
