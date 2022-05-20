# Select all the ETF symbols except "VXX", "SVXY" "MTUM", "QUAL", "VLUE", and "USMV"
symbolv <- colnames(rutils::etfenv$returns)
symbolv <- symbolv[!(symbolv %in% c("VXX", "SVXY", "MTUM", "QUAL", "VLUE", "USMV"))]
# Extract columns of rutils::etfenv$returns and overwrite NA values
retsp <- rutils::etfenv$returns[, symbolv]
nstocks <- NCOL(retsp)
# retsp <- na.omit(retsp)
retsp[1, is.na(retsp[1, ])] <- 0
retsp <- zoo::na.locf(retsp, na.rm=FALSE)
dates <- zoo::index(retsp)
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
indeks <- xts::xts(rowMeans(retsp), dates)
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
precision <- sqrt(.Machine$double.eps)
# Calculate regularized inverse matrix
not_zero <- (eigenval > (precision * eigenval[1]))
invreg <- eigenvec[, not_zero] %*%
  (t(eigenvec[, not_zero]) / eigenval[not_zero])
# Verify inverse property of invreg
all.equal(covmat, covmat %*% invreg %*% covmat)
# Calculate regularized inverse of covmat
invmat <- MASS::ginv(covmat)
# Verify inverse property of matrixv
all.equal(invmat, invreg)

# Calculate in-sample covariance matrix
covmat <- cov(retsis)
eigend <- eigen(covmat)
eigenvec <- eigend$vectors
eigenval <- eigend$values
# Calculate shrinkage inverse of covariance matrix
eigen_max <- 3
invmat <- eigenvec[, 1:eigen_max] %*%
  (t(eigenvec[, 1:eigen_max]) / eigenval[1:eigen_max])
# Verify inverse property of inverse
all.equal(covmat, covmat %*% invmat %*% covmat)

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
dygraphs::dygraph(cumsum(pnls)[endp], main="Optimal Portfolio Returns With Eigen Shrinkage") %>%
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
pnls <- rbind(indeks[paste0("/", start(pnls)-1)], pnls*sd(indeks)/sd(pnls))
wealth <- cbind(indeks, pnls, (pnls+indeks)/2)
colnames(wealth) <- c("Index", "Strategy", "Combined")
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealth,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
dygraphs::dygraph(cumsum(wealth)[endp], main="Monthly ETF Rolling Portfolio Strategy") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)

# Define monthly end points
look_back <- 3; eigen_max <- 9
startp <- c(rep_len(0, look_back), endp[1:(npts-look_back)])
# Perform loop over end points
pnls <- lapply(2:npts, function(ep) {
    # Calculate regularized inverse of covariance matrix
    insample <- retsx[startp[ep-1]:endp[ep-1], ]
    eigend <- eigen(cov(insample))
    eigenvec <- eigend$vectors
    eigenval <- eigend$values
    invmat <- eigenvec[, 1:eigen_max] %*%
(t(eigenvec[, 1:eigen_max]) / eigenval[1:eigen_max])
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
pnls <- rbind(indeks[paste0("/", start(pnls)-1)], pnls*sd(indeks)/sd(pnls))
wealth <- cbind(indeks, pnls, (pnls+indeks)/2)
colnames(wealth) <- c("Index", "Strategy", "Combined")
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealth,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
dygraphs::dygraph(cumsum(wealth)[endp], main="Rolling Portfolio Strategy With Eigen Shrinkage") %>%
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
    invmat <- eigenvec[, 1:eigen_max] %*%
(t(eigenvec[, 1:eigen_max]) / eigenval[1:eigen_max])
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
pnls <- rbind(indeks[paste0("/", start(pnls)-1)], pnls*sd(indeks)/sd(pnls))
wealth <- cbind(indeks, pnls, (pnls+indeks)/2)
colnames(wealth) <- c("Index", "Strategy", "Combined")
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealth,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
dygraphs::dygraph(cumsum(wealth)[endp], main="Rolling Portfolio Strategy With Return Shrinkage") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)

# Define backtest functional for rolling portfolio strategy
roll_portf <- function(excess, # Excess returns
                 returns, # Stock returns
                 endp, # End points
                 look_back=12, # Look-back interval
                 eigen_max=3, # Eigen shrinkage intensity
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
    invmat <- eigenvec[, 1:eigen_max] %*%
(t(eigenvec[, 1:eigen_max]) / eigenval[1:eigen_max])
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
  look_back=look_back, eigen_max=eigen_max)
# Perform sapply loop over look_backs
look_backs <- seq(2, 15, by=1)
pnls <- lapply(look_backs, roll_portf,
  returns=retsp, excess=retsx, endp=endp, eigen_max=eigen_max)
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("look_back=", look_backs)
pnlsums <- sapply(pnls, sum)
look_back <- look_backs[which.max(pnlsums)]

# Plot dygraph of daily ETF momentum strategies
colors <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls)[endp], main="Rolling Portfolio Strategies") %>%
  dyOptions(colors=colors, strokeWidth=2) %>%
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

# Perform backtest for different eigen_max values
eigenvals <- 2:11
pnls <- lapply(eigenvals, roll_portf, excess=retsx,
  returns=retsp, endp=endp, look_back=look_back)
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("eigenval=", eigenvals)
pnlsums <- sapply(pnls, sum)
eigen_max <- eigenvals[which.max(pnlsums)]

# Plot dygraph of daily ETF momentum strategies
colors <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls)[endp], main="Rolling Portfolio Strategies With Eigen Shrinkage") %>%
  dyOptions(colors=colors, strokeWidth=2) %>%
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
  returns=retsp, endp=endp, look_back=look_back, eigen_max=eigen_max)
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("alpha=", alphav)
pnlsums <- sapply(pnls, sum)
alpha <- alphav[which.max(pnlsums)]

# Plot dygraph of daily ETF momentum strategies
colors <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls)[endp], main="Rolling Portfolio Strategies With Return Shrinkage") %>%
  dyOptions(colors=colors, strokeWidth=2) %>%
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
dates <- zoo::index(retsp)
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
indeks <- xts::xts(rowMeans(retsp), dates)

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
look_back <- 8; eigen_max <- 21
eigend <- eigen(cov(retsis))
eigenvec <- eigend$vectors
eigenval <- eigend$values
invmat <- eigenvec[, 1:eigen_max] %*%
  (t(eigenvec[, 1:eigen_max]) / eigenval[1:eigen_max])
# Calculate portfolio weights
weightv <- invmat %*% colMeans(retsx["/2010"])
weightv <- drop(weightv/sqrt(sum(weightv^2)))
names(weightv) <- colnames(retsp)
# Calculate portfolio returns
insample <- xts::xts(retsis %*% weightv, zoo::index(retsis))
outsample <- xts::xts(retsos %*% weightv, zoo::index(retsos))
indeks <- xts::xts(rowMeans(retsp), dates)

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
dygraphs::dygraph(cumsum(pnls)[endp], main="Out-of-sample Returns for Stocks with Eigen Shrinkage") %>%
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
eigen_max <- 4
eigend <- eigen(cov(matrixv))
covinv <- eigend$vectors[, 1:eigen_max] %*%
  (t(eigend$vectors[, 1:eigen_max]) / eigend$values[1:eigen_max])
# Regularized inverse using RcppArmadillo
covinv_arma <- calc_inv(matrixv, eigen_max)
all.equal(covinv, covinv_arma)
# Microbenchmark RcppArmadillo code
library(microbenchmark)
summary(microbenchmark(
  pure_r={eigend <- eigen(cov(matrixv))
    eigend$vectors[, 1:eigen_max] %*%
(t(eigend$vectors[, 1:eigen_max]) / eigend$values[1:eigen_max])
  },
  r_cpp=calc_inv(matrixv, eigen_max),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary

# Overwrite NA values in returns100
retsp <- returns100
retsp[1, is.na(retsp[1, ])] <- 0
retsp <- zoo::na.locf(retsp, na.rm=FALSE)
retsx <- (retsp - riskf)
nstocks <- NCOL(retsp) ; dates <- zoo::index(retsp)
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
indeks <- xts::xts(rowMeans(retsp), dates)
pnls <- rbind(indeks[paste0("/", start(pnls)-1)], pnls*sd(indeks)/sd(pnls))
# Calculate the Sharpe and Sortino ratios
wealth <- cbind(indeks, pnls)
colnames(wealth) <- c("Equal Weight", "Strategy")
sqrt(252)*sapply(wealth,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot cumulative strategy returns
dygraphs::dygraph(cumsum(wealth)[endp], main="Rolling Portfolio Optimization Strategy for S&P500 Stocks") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)

# Shift end points to C++ convention
endp <- (endp - 1)
endp[endp < 0] <- 0
startp <- (startp - 1)
startp[startp < 0] <- 0
# Specify eigen shrinkage and return shrinkage
alpha <- 0.7
eigen_max <- 21
# Perform backtest in Rcpp
pnls <- HighFreq::back_test(excess=retsx, returns=retsp,
  startp=startp, endp=endp, alpha=alpha, eigen_max=eigen_max, method="max_sharpe")
pnls <- pnls*sd(indeks)/sd(pnls)

# Plot cumulative strategy returns
wealth <- cbind(indeks, pnls, (pnls+indeks)/2)
colnames(wealth) <- c("Index", "Strategy", "Combined")
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealth,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
dygraphs::dygraph(cumsum(wealth)[endp], main="Rolling S&P500 Portfolio Optimization Strategy With Shrinkage") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)

# Perform backtest over vector of return shrinkage intensities
alphav <- seq(from=0.01, to=0.91, by=0.1)
pnls <- lapply(alphav, function(alpha) {
  HighFreq::back_test(excess=retsx, returns=retsp,
  startp=startp, endp=endp, alpha=alpha, eigen_max=eigen_max, method="max_sharpe")
})  # end lapply
profilev <- sapply(pnls, sum)
plot(x=alphav, y=profilev, t="l", main="Rolling Strategy as Function of Return Shrinkage",
  xlab="Shrinkage Intensity Alpha", ylab="pnl")
whichmax <- which.max(profilev)
alpha <- alphav[whichmax]
pnls <- pnls[[whichmax]]
# Perform backtest over vector of eigen shrinkage eigenvals
eigenvals <- seq(from=3, to=40, by=2)
pnls <- lapply(eigenvals, function(eigen_max) {
  HighFreq::back_test(excess=retsx, returns=retsp,
    startp=startp, endp=endp, alpha=alpha, eigen_max=eigen_max, method="max_sharpe")
})  # end lapply
profilev <- sapply(pnls, sum)
plot(x=eigenvals, y=profilev, t="l", main="Strategy PnL as Function of eigen_max",
  xlab="eigen_max", ylab="pnl")
whichmax <- which.max(profilev)
eigen_max <- eigenvals[whichmax]
pnls <- pnls[[whichmax]]
pnls <- pnls*sd(indeks)/sd(pnls)

# Plot cumulative strategy returns
wealth <- cbind(indeks, pnls, (pnls+indeks)/2)
colnames(wealth) <- c("Index", "Strategy", "Combined")
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealth,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
dygraphs::dygraph(cumsum(wealth)[endp], main="Optimal Rolling S&P500 Portfolio Strategy") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)

# Perform backtest over look-backs
look_backs <- seq(from=3, to=12, by=1)
pnls <- lapply(look_backs, function(look_back) {
  startp <- c(rep_len(0, look_back), endp[1:(npts-look_back)])
  startp <- (startp - 1)
  startp[startp < 0] <- 0
  HighFreq::back_test(excess=retsx, returns=retsp,
    startp=startp, endp=endp, alpha=alpha, eigen_max=eigen_max, method="max_sharpe")
})  # end lapply
profilev <- sapply(pnls, sum)
plot(x=look_backs, y=profilev, t="l", main="Strategy PnL as Function of Look-back Interval",
  xlab="Look-back Interval", ylab="pnl")
whichmax <- which.max(profilev)
look_back <- look_backs[whichmax]
pnls <- pnls[[whichmax]]
pnls <- pnls*sd(indeks)/sd(pnls)

# Plot cumulative strategy returns
wealth <- cbind(indeks, pnls, (pnls+indeks)/2)
colnames(wealth) <- c("Index", "Strategy", "Combined")
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealth,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
dygraphs::dygraph(cumsum(wealth)[endp], main="Optimal Rolling S&P500 Portfolio Strategy") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)

# Linear constraint
weightv <- weightv/sum(weightv)
# Quadratic constraint
weightv <- weightv/sqrt(sum(weightv^2))
# Box constraints
weightv[weightv > 1] <- 1
weightv[weightv < 0] <- 0

library(quantmod)
library(Rglpk)
# Vector of symbol names
symbolv <- c("VTI", "IEF", "DBC")
nweights <- NROW(symbolv)
# Calculate mean returns
returns <- rutils::etfenv$returns[, symbolv]
returns <- zoo::na.locf(returns, na.rm=FALSE)
returns <- na.omit(returns)
retsm <- colMeans(returns)
# Specify weight constraints
constr <- matrix(c(rep(1, nweights), 1, 1, 0),
                 nc=nweights, byrow=TRUE)
directs <- c("==", "<=")
rhs <- c(1, 0)
# Specify weight bounds (-1, 1) (default is c(0, Inf))
bounds <- list(lower=list(ind=1:nweights, val=rep(-1, nweights)),
         upper=list(ind=1:nweights, val=rep(1, nweights)))
# Perform optimization
optiml <- Rglpk::Rglpk_solve_LP(
  obj=retsm,
  mat=constr,
  dir=directs,
  rhs=rhs,
  bounds=bounds,
  max=TRUE)
unlist(optiml[1:2])

# Calculate covariance matrix of returns and its inverse
covmat <- cov(returns)
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
retsm <- colMeans(returns)
# Specify the target return
rett <- 1.5*mean(returns)
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
retsp <- drop(returns %*% weightv)
c(return=mean(retsp), sd=sd(retsp))
all.equal(mean(retsp), rett)
# Calculate portfolio variance
uu <- c(1, rett)
finv <- solve(fmat)
all.equal(var(retsp), drop(t(uu) %*% finv %*% uu))
# Calculate vertex of variance parabola
weightv <- drop(covinv %*% unitv /
  drop(t(unitv) %*% covinv %*% unitv))
retsp <- drop(returns %*% weightv)
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
retexc <- returns - riskf
# Calculate covariance and inverse matrix
covmat <- cov(returns)
unitv <- rep(1, NCOL(covmat))
covinv <- solve(a=covmat)
# Calculate mean excess returns
retexc <- sapply(retexc, mean)
# Weights of maximum Sharpe portfolio
# weightv <- solve(a=covmat, b=returns)
weightv <- covinv %*% retexc
weightv <- weightv/drop(t(unitv) %*% weightv)
# Sharpe ratios
sqrt(252)*sum(weightv*retexc) /
  sqrt(drop(weightv %*% covmat %*% weightv))
sapply(returns - riskf,
  function(x) sqrt(252)*mean(x)/sd(x))
weights_maxsharpe <- weightv

library(quantmod)
# Calculate minimum variance weights
weightv <- covinv %*% unitv
weights_minvar <- weightv/drop(t(unitv) %*% weightv)
# Calculate optimal portfolio returns
optim_rets <- xts(
  x=cbind(exp(cumsum(returns %*% weights_maxsharpe)),
    exp(cumsum(returns %*% weights_minvar))),
  order.by=zoo::index(returns))
colnames(optim_rets) <- c("maxsharpe", "minvar")
# Plot optimal portfolio returns, with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "green")
x11(width=6, height=5)
chart_Series(optim_rets, theme=plot_theme,
  name="Maximum Sharpe and
  Minimum Variance portfolios")
legend("top", legend=colnames(optim_rets), cex=0.8,
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
  weightv <- runif(nweights-1, min=-0.25, max=1.0)
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
returns <- c(asset1=0.05, asset2=0.06)
stdevs <- c(asset1=0.4, asset2=0.5)
corrp <- 0.6
covmat <- matrix(c(1, corrp, corrp, 1), nc=2)
covmat <- t(t(stdevs*covmat)*stdevs)
weightv <- seq(from=(-1), to=2, length.out=31)
weightv <- cbind(weightv, 1-weightv)
retsp <- weightv %*% returns
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
         names=names(returns))), collapse=" "),
     pos=2, cex=0.8)

# Plot individual assets
points(stdevs, returns, col="green", lwd=3)
text(stdevs, returns, labels=names(returns), pos=4, cex=0.8)
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
returns <- rutils::etfenv$returns[, symbolv]
retsp <- returns %*% t(weightv)
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
retsm <- 252*sapply(returns, mean)
stdevs <- sqrt(252)*sapply(returns, sd)
points(stdevs, retsm, col="green", lwd=6)
text(stdevs, retsm, labels=names(returns), pos=2, cex=0.8)
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
optim_rets <- lapply(returns,
  function(returns) exp(cumsum(returns)))
optim_rets <- rutils::do_call(cbind, optim_rets)
# Calculate market portfolio returns
optim_rets <- cbind(exp(cumsum(returns %*%
    c(weightv[whichmax], 1-weightv[whichmax]))),
  optim_rets)
colnames(optim_rets)[1] <- "market"
# Plot market portfolio with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue", "green")
chart_Series(optim_rets, theme=plot_theme,
       name="Market portfolio for stocks and bonds")
legend("top", legend=colnames(optim_rets),
 cex=0.8, inset=0.1, bg="white", lty=1,
 lwd=6, col=plot_theme$col$line.col, bty="n")

# Calculate VTI percentage returns
retsp <- na.omit(rutils::etfenv$returns$VTI)
nrows <- NROW(retsp)
# Define end points
endp <- 1:NROW(retsp)
# Start points are multi-period lag of endp
look_back <- 11
startp <- c(rep_len(0, look_back), endp[1:(nrows-look_back)])
# Calculate rolling variance in sapply() loop - takes long
variance <- sapply(1:nrows, function(indeks) {
  ret_s <- retsp[startp[indeks]:endp[indeks]]
  sum((ret_s - mean(ret_s))^2)
}) / (look_back)  # end sapply
# Use only vectorized functions
cumrets <- cumsum(retsp)
cumrets <- (cumrets -
  c(rep_len(0, look_back), cumrets[1:(nrows-look_back)]))
cumrets2 <- cumsum(retsp^2)
cumrets2 <- (cumrets2 -
  c(rep_len(0, look_back), cumrets2[1:(nrows-look_back)]))
variance2 <- (cumrets2 - cumrets^2/look_back)/(look_back)
all.equal(variance[-(1:look_back)], as.numeric(variance2)[-(1:look_back)])
# Same, using package rutils
cumrets <- rutils::roll_sum(retsp, look_back=look_back, min_obs=1)
cumrets2 <- rutils::roll_sum(retsp^2, look_back=look_back, min_obs=1)
variance2 <- (cumrets2 - cumrets^2/look_back)/(look_back)
# Coerce variance into xts
tail(variance)
class(variance)
variance <- xts(variance, order.by=zoo::index(retsp))
colnames(variance) <- "VTI.variance"
head(variance)

# Calculate rolling VTI variance using package roll
library(roll)  # Load roll
variance <- roll::roll_var(retsp, width=look_back)
colnames(variance) <- "VTI.variance"
head(variance)
sum(is.na(variance))
variance[1:(look_back-1)] <- 0
# Benchmark calculation of rolling variance
library(microbenchmark)
summary(microbenchmark(
  roll_sapply=sapply(2:nrows, function(indeks) {
    ret_s <- retsp[startp[indeks]:endp[indeks]]
    sum((ret_s - mean(ret_s))^2)
  }),
  ro_ll=roll::roll_var(retsp, width=look_back),
  times=10))[, c(1, 4, 5)]

# Calculate EWMA VTI variance using compiled C++ function
look_back <- 51
weights <- exp(-0.1*1:look_back)
weights <- weights/sum(weights)
variance <- .Call(stats:::C_cfilter, retsp^2,
  filter=weights, sides=1, circular=FALSE)
variance[1:(look_back-1)] <- variance[look_back]
# Plot EWMA volatility
variance <- xts:::xts(sqrt(variance), order.by=zoo::index(retsp))
dygraphs::dygraph(variance, main="VTI EWMA Volatility") %>%
  dyOptions(colors="blue")
quantmod::chart_Series(xtes, name="VTI EWMA Volatility")

# Calculate rolling VTI variance using package roll
library(roll)  # Load roll
variance <- roll::roll_var(retsp,
  weights=rev(weights), width=look_back)
colnames(variance) <- "VTI.variance"
class(variance)
head(variance)
sum(is.na(variance))
variance[1:(look_back-1)] <- 0

library(HighFreq)  # Load HighFreq
# Minutely SPY returns (unit per minute) single day
# Minutely SPY volatility (unit per minute)
retsp <- rutils::diffit(log(SPY["2012-02-13", 4]))
sd(retsp)
# SPY returns multiple days (includes overnight jumps)
retsp <- rutils::diffit(log(SPY[, 4]))
sd(retsp)
# Table of time intervals - 60 second is most frequent
indeks <- rutils::diffit(.zoo::index(SPY))
table(indeks)
# SPY returns divided by the overnight time intervals (unit per second)
retsp <- retsp / indeks
retsp[1] <- 0
# Minutely SPY volatility scaled to unit per minute
60*sd(retsp)

library(HighFreq)  # Load HighFreq
spy <- HighFreq::SPY["2009"]
# Calculate daily SPY volatility using package HighFreq
sqrt(6.5*60*HighFreq::calcvar_ohlc(log(spy),
  method="yang_zhang"))
# Calculate daily SPY volatility from minutely prices using package TTR
sqrt((6.5*60)*mean(na.omit(
  TTR::volatility(spy, N=1, calc="yang.zhang"))^2))
# Calculate rolling SPY variance using package HighFreq
variance <- HighFreq::roll_var_ohlc(log(spy), method="yang_zhang",
  look_back=look_back)
# Plot range volatility
variance <- xts:::xts(sqrt(variance), order.by=zoo::index(spy))
dygraphs::dygraph(variance["2009-02"],
  main="SPY Rolling Range Volatility") %>%
  dyOptions(colors="blue")
# Benchmark the speed of HighFreq vs TTR
library(microbenchmark)
summary(microbenchmark(
  ttr=TTR::volatility(rutils::etfenv$VTI, N=1, calc="yang.zhang"),
  highfreq=HighFreq::calcvar_ohlc(log(rutils::etfenv$VTI), method="yang_zhang"),
  times=2))[, c(1, 4, 5)]

# Calculate VXX log prices
vxx <- na.omit(rutils::etfenv$prices$VXX)
dates <- zoo::index(vxx)
look_back <- 41
vxx <- log(vxx)
# Calculate rolling VTI volatility
closep <- get("VTI", rutils::etfenv)[dates]
closep <- log(closep)
volat <- sqrt(HighFreq::roll_var_ohlc(ohlc=closep, look_back=look_back, scalev=FALSE))
volat[1:look_back] <- volat[look_back+1]

# Plot dygraph of VXX and VTI volatility
datav <- cbind(vxx, volat)
colnames(datav)[2] <- "VTI Volatility"
colnamev <- colnames(datav)
cap_tion <- "VXX and VTI Volatility"
dygraphs::dygraph(datav[, 1:2], main=cap_tion) %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=1, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=1, col="red")

x11(width=6, height=5)
par(mar=c(4, 3, 1, 1), oma=c(0, 0, 0, 0))
# Calculate VTI percentage returns
retsp <- na.omit(rutils::etfenv$returns$VTI)
# Calculate rolling VTI variance using package roll
look_back <- 22
variance <- roll::roll_var(retsp, width=look_back)
variance[1:(look_back-1)] <- 0
colnames(variance) <- "VTI.variance"
# Number of look_backs that fit over returns
nrows <- NROW(retsp)
nagg <- nrows %/% look_back
# Define endp with beginning stub
endp <- c(0, nrows-look_back*nagg + (0:nagg)*look_back)
nrows <- NROW(endp)
# Subset variance to endp
variance <- variance[endp]
# Plot autocorrelation function
rutils::plot_acf(variance, lag=10, main="ACF of Variance")
# Plot partial autocorrelation
pacf(variance, lag=10, main="PACF of Variance", ylab=NA)

# Define GARCH parameters
alpha <- 0.3; betav <- 0.5;
om_ega <- 1e-4*(1-alpha-betav)
nrows <- 1000
# Calculate matrix of standard normal innovations
set.seed(1121)  # Reset random numbers
innov <- rnorm(nrows)
retsp <- numeric(nrows)
variance <- numeric(nrows)
variance[1] <- om_ega/(1-alpha-betav)
retsp[1] <- sqrt(variance[1])*innov[1]
# Simulate GARCH model
for (i in 2:nrows) {
  retsp[i] <- sqrt(variance[i-1])*innov[i]
  variance[i] <- om_ega + alpha*retsp[i]^2 +
    betav*variance[i-1]
}  # end for
# Simulate the GARCH process using Rcpp
garch_data <- HighFreq::sim_garch(omega=om_ega, alpha=alpha,
  beta=betav, innov=matrix(innov))
all.equal(garch_data, cbind(retsp, variance),
  check.attributes=FALSE)

# Open plot window on Mac
dev.new(width=6, height=5, noRStudioGD=TRUE)
# Set plot parameters to reduce whitespace around plot
par(mar=c(2, 2, 3, 1), oma=c(0, 0, 0, 0))
# Plot GARCH cumulative returns
plot(cumsum(retsp), t="l", col="blue", xlab="", ylab="",
  main="GARCH Cumulative Returns")
quartz.save("figure/garch_returns.png", type="png",
  width=6, height=5)
# Plot GARCH volatility
plot(sqrt(variance), t="l", col="blue", xlab="", ylab="",
  main="GARCH Volatility")
quartz.save("figure/garch_volat.png", type="png",
  width=6, height=5)

# Calculate kurtosis of GARCH returns
mean(((retsp-mean(retsp))/sd(retsp))^4)
# Perform Jarque-Bera test of normality
tseries::jarque.bera.test(retsp)
# Fit t-distribution into GARCH returns
optim_fit <- MASS::fitdistr(retsp, densfun="t", df=2)
loc <- optim_fit$estimate[1]
scalev <- optim_fit$estimate[2]

# Plot histogram of GARCH returns
histp <- hist(retsp, col="lightgrey",
  xlab="returns", breaks=200, xlim=c(-0.03, 0.03),
  ylab="frequency", freq=FALSE, main="GARCH Returns Histogram")
lines(density(retsp, adjust=1.5), lwd=2, col="blue")
curve(expr=dt((x-loc)/scalev, df=2)/scalev,
  type="l", xlab="", ylab="", lwd=2,
  col="red", add=TRUE)
legend("topright", inset=-0, bty="n",
 leg=c("density", "t-distr w/ 2 dof"),
 lwd=6, lty=1, col=c("blue", "red"))
quartz.save("figure/garch_hist.png", type="png", width=6, height=5)

# Specify GARCH model
garch_spec <- fGarch::garchSpec(model=list(ar=c(0, 0), omega=om_ega,
  alpha=alpha, beta=betav))
# Simulate GARCH model
garch_sim <- fGarch::garchSim(spec=garch_spec, n=nrows)
retsp <- as.numeric(garch_sim)
# Calculate kurtosis of GARCH returns
moments::moment(retsp, order=4) /
  moments::moment(retsp, order=2)^2
# Perform Jarque-Bera test of normality
tseries::jarque.bera.test(retsp)
# Plot histogram of GARCH returns
histp <- hist(retsp, col="lightgrey",
  xlab="returns", breaks=200, xlim=c(-0.05, 0.05),
  ylab="frequency", freq=FALSE,
  main="GARCH Returns Histogram")
lines(density(retsp, adjust=1.5), lwd=3, col="blue")

# Fit t-distribution into GARCH returns
optim_fit <- MASS::fitdistr(retsp, densfun="t", df=2, lower=c(-1, 1e-7))
loc <- optim_fit$estimate[1]
scalev <- optim_fit$estimate[2]
curve(expr=dt((x-loc)/scalev, df=2)/scalev,
  type="l", xlab="", ylab="", lwd=3,
  col="red", add=TRUE)
legend("topright", inset=0.05, bty="n",
 leg=c("density", "t-distr w/ 2 dof"),
 lwd=6, lty=1, col=c("blue", "red"))

# Calculate variance of GARCH returns
var(retsp)
# Calculate expected value of variance
om_ega/(1-alpha-betav)
# Calculate kurtosis of GARCH returns
mean(((retsp-mean(retsp))/sd(retsp))^4)
# Calculate expected value of kurtosis
3 + 6*alpha^2/(1-2*alpha^2-(alpha+betav)^2)

# Calculate the distribution of GARCH kurtosis
kurt <- sapply(1:1e4, function(x) {
  garch_data <- HighFreq::sim_garch(omega=om_ega, alpha=alpha,
    beta=betav, innov=matrix(rnorm(nrows)))
  retsp <- garch_data[, 1]
  c(var(retsp), mean(((retsp-mean(retsp))/sd(retsp))^4))
})  # end sapply
kurt <- t(kurt)
apply(kurt, 2, mean)
# Plot the distribution of GARCH kurtosis
dev.new(width=6, height=5, noRStudioGD=TRUE)
par(mar=c(2, 2, 3, 1), oma=c(0, 0, 0, 0))
histp <- hist(kurt[, 2], breaks=500, col="lightgrey",
  xlim=c(2, 8), xlab="returns", ylab="frequency", freq=FALSE,
  main="Distribution of GARCH Kurtosis")
lines(density(kurt[, 2], adjust=1.5), lwd=3, col="blue")
abline(v=(3 + 6*alpha^2/(1-2*alpha^2-(alpha+betav)^2)), lwd=3, col="red")
text(x=7.0, y=0.4, "Expected Kurtosis")
quartz.save("figure/garch_kurtosis.png", type="png", width=6, height=5)

# Simulate the GARCH process using Rcpp
garch_data <- HighFreq::sim_garch(omega=om_ega, alpha=alpha,
  beta=betav, innov=matrix(innov))
# Extract the returns
retsp <- garch_data[, 1]
# Estimate the rolling variance from the returns
variance <- numeric(nrows)
variance[1] <- om_ega/(1-alpha-betav)
for (i in 2:nrows) {
  variance[i] <- om_ega + alpha*returns[i]^2 +
    betav*variance[i-1]
}  # end for
all.equal(garch_data[, 2], variance, check.attributes=FALSE)

library(fGarch)
# Fit returns into GARCH
garch_fit <- fGarch::garchFit(data=retsp)
# Fitted GARCH parameters
garch_fit@fit$coef
# Actual GARCH parameters
c(mu=mean(retsp), omega=om_ega,alpha=alpha, beta=betav)
# Plot GARCH fitted volatility
plot(sqrt(garch_fit@fit$series$h), t="l",
  col="blue", xlab="", ylab="",
  main="GARCH Fitted Volatility")
quartz.save("figure/garch_fGarch_fitted.png",
  type="png", width=6, height=5)

# Define likelihood function
likefun <- function(om_ega, alpha, betav) {
  # Estimate the rolling variance from the returns
  variance <- numeric(nrows)
  variance[1] <- om_ega/(1-alpha-betav)
  for (i in 2:nrows) {
    variance[i] <- om_ega + alpha*returns[i]^2 + betav*variance[i-1]
  }  # end for
  variance <- ifelse(variance > 0, variance, 0.000001)
  # Lag the variance
  variance <- rutils::lagit(variance, pad_zeros=FALSE)
  # Calculate the likelihood
  -sum(retsp^2/variance + log(variance))
}  # end likefun
# Calculate the likelihood in R
likefun(om_ega, alpha, betav)
# Calculate the likelihood in Rcpp
HighFreq::lik_garch(omega=om_ega, alpha=alpha,
  beta=betav, returns=matrix(retsp))
# Benchmark speed of likelihood calculations
library(microbenchmark)
summary(microbenchmark(
  Rcode=likefun(om_ega, alpha, betav),
  Rcpp=HighFreq::lik_garch(omega=om_ega, alpha=alpha, beta=betav, returns=matrix(retsp))
  ), times=10)[, c(1, 4, 5)]

# Calculate the variance of returns
retsp <- garch_data[, 1, drop=FALSE]
variance <- var(retsp)
retsp <- (retsp - mean(retsp))
# Calculate likelihood as function of alpha and betav parameters
likefun <- function(alpha, betav) {
  om_ega <- variance*(1 - alpha - betav)
  -HighFreq::lik_garch(omega=om_ega, alpha=alpha, beta=betav, returns=retsp)
}  # end likefun
# Calculate matrix of likelihood values
alphas <- seq(from=0.15, to=0.35, len=50)
betas <- seq(from=0.35, to=0.5, len=50)
lik_mat <- sapply(alphas, function(alpha) sapply(betas,
  function(betav) likefun(alpha, betav)))

# Set rgl options and load package rgl
options(rgl.useNULL=TRUE); library(rgl)
# Draw and render 3d surface plot of likelihood function
n_col <- 100
color <- rainbow(n_col, start=2/6, end=4/6)
z_col <- cut(lik_mat, n_col)
rgl::persp3d(alphas, betas, lik_mat, col=color[z_col],
  xlab="alpha", ylab="beta", zlab="likelihood")
rgl::rglwidget(elementId="plot3drgl", width=700, height=700)
# Perform grid search
coord <- which(lik_mat == min(lik_mat), arr.ind=TRUE)
c(alphas[coord[2]], betas[coord[1]])
lik_mat[coord]
likefun(alphas[coord[2]], betas[coord[1]])
# Optimal and actual parameters
options(scipen=2)  # Use fixed not scientific notation
cbind(actual=c(alpha=alpha, beta=betav, omega=om_ega),
  optimal=c(alphas[coord[2]], betas[coord[1]], variance*(1 - sum(alphas[coord[2]], betas[coord[1]]))))

# Define vectorized likelihood function
likefun <- function(x, retsp) {
  alpha <- x[1]; betav <- x[2]; om_ega <- x[3]
  -HighFreq::lik_garch(omega=om_ega, alpha=alpha, beta=betav, returns=retsp)
}  # end likefun
# Initial parameters
initp <- c(alpha=0.2, beta=0.4, omega=variance/0.2)
# Find max likelihood parameters using steepest descent optimizer
optim_fit <- optim(par=initp,
  fn=likefun, # Log-likelihood function
  method="L-BFGS-B", # Quasi-Newton method
  returns=retsp,
  upper=c(0.35, 0.55, variance), # Upper constraint
  lower=c(0.15, 0.35, variance/100)) # Lower constraint
# Optimal and actual parameters
cbind(actual=c(alpha=alpha, beta=betav, omega=om_ega),
optimal=c(optim_fit$par["alpha"], optim_fit$par["beta"], optim_fit$par["omega"]))
# Find max likelihood parameters using DEoptim
optiml <- DEoptim::DEoptim(fn=likefun,
  upper=c(0.35, 0.55, variance), # Upper constraint
  lower=c(0.15, 0.35, variance/100), # Lower constraint
  returns=retsp,
  control=list(trace=FALSE, itermax=1000, parallelType=1))
# Optimal and actual parameters
cbind(actual=c(alpha=alpha, beta=betav, omega=om_ega),
optimal=c(optiml$optim$bestmem[1], optiml$optim$bestmem[2], optiml$optim$bestmem[3]))

# Calculate VTI returns
retsp <- rutils::diffit(log(quantmod::Cl(rutils::etfenv$VTI)))
# Find max likelihood parameters using DEoptim
optiml <- DEoptim::DEoptim(fn=likefun,
  upper=c(0.4, 0.9, variance), # Upper constraint
  lower=c(0.1, 0.5, variance/100), # Lower constraint
  returns=retsp,
  control=list(trace=FALSE, itermax=1000, parallelType=1))
# Optimal parameters
par_am <- unname(optiml$optim$bestmem)
alpha <- par_am[1]; betav <- par_am[2]; om_ega <- par_am[3]
c(alpha, betav, om_ega)
# Equilibrium GARCH variance
om_ega/(1-alpha-betav)
drop(var(retsp))

# Estimate the GARCH volatility of VTI returns
nrows <- NROW(retsp)
variance <- numeric(nrows)
variance[1] <- om_ega/(1-alpha-betav)
for (i in 2:nrows) {
  variance[i] <- om_ega + alpha*retsp[i]^2 + betav*variance[i-1]
}  # end for
# Estimate the GARCH volatility using Rcpp
garch_data <- HighFreq::sim_garch(omega=om_ega, alpha=alpha,
  beta=betav, innov=retsp, is_random=FALSE)
all.equal(garch_data[, 2], variance, check.attributes=FALSE)
# Plot dygraph of the estimated GARCH volatility
dygraphs::dygraph(xts::xts(sqrt(variance), zoo::index(retsp)),
  main="Estimated GARCH Volatility of VTI") %>%
  dyOptions(colors="blue")

# Simulate GARCH model
garch_data <- HighFreq::sim_garch(omega=om_ega, alpha=alpha,
  beta=betav, innov=matrix(innov))
variance <- garch_data[, 2]
# Calculate the equilibrium variance
var_eq <- om_ega/(1-alpha-betav)
# Calculate the variance forecasts
varcasts <- numeric(10)
varcasts[1] <- var_eq +
  (alpha + betav)*(xts::last(variance) - var_eq)
for (i in 2:10) {
  varcasts[i] <- var_eq + (alpha + betav)*(varcasts[i-1] - var_eq)
}  # end for

# Open plot window on Mac
dev.new(width=6, height=5, noRStudioGD=TRUE)
par(mar=c(2, 2, 3, 1), oma=c(0, 0, 0, 0))
# Plot GARCH variance forecasts
plot(tail(variance, 30), t="l", col="blue", xlab="", ylab="",
  xlim=c(1, 40), ylim=c(0, max(tail(variance, 30))),
  main="GARCH Variance Forecasts")
text(x=15, y=0.5*var_eq, "realized variance")
lines(x=30:40, y=c(xts::last(variance), varcasts), col="red", lwd=3)
text(x=35, y=0.6*var_eq, "variance forecasts")
abline(h=var_eq, lwd=3, col="red")
text(x=10, y=1.1*var_eq, "Equilibrium variance")
quartz.save("figure/garch_forecast.png", type="png",
  width=6, height=5)

library(HighFreq)  # Load HighFreq
# Minutely SPY returns (unit per minute) single day
retsp <- rutils::diffit(log(SPY["2012-02-13", 4]))
# Minutely SPY volatility (unit per minute)
sd(retsp)
# Divide minutely SPY returns by time intervals (unit per second)
retsp <- retsp / rutils::diffit(.zoo::index(SPY["2012-02-13"]))
retsp[1] <- 0
# Minutely SPY volatility scaled to unit per minute
60*sd(retsp)
# SPY returns multiple days
retsp <- rutils::diffit(log(SPY[, 4]))
# Minutely SPY volatility (includes overnight jumps)
sd(retsp)
# Table of intervals - 60 second is most frequent
indeks <- rutils::diffit(.zoo::index(SPY))
table(indeks)
# hist(indeks)
# SPY returns with overnight scaling (unit per second)
retsp <- retsp / indeks
retsp[1] <- 0
# Minutely SPY volatility scaled to unit per minute
60*sd(retsp)

library(HighFreq)  # Load HighFreq
# Minutely OHLC SPY prices aggregated to daily prices
SPY_daily <- rutils::to_period(ohlc=HighFreq::SPY, period="days")
# Daily SPY volatility from daily returns
sd(rutils::diffit(log(SPY_daily[, 4])))
# Minutely SPY returns (unit per minute)
retsp <- rutils::diffit(log(SPY[, 4]))
# Minutely SPY volatility scaled to daily interval
sqrt(6.5*60)*sd(retsp)
# Minutely SPY returns with overnight scaling (unit per second)
retsp <- rutils::diffit(log(SPY[, 4]))
indeks <- rutils::diffit(.zoo::index(SPY))
retsp <- retsp / indeks
retsp[1] <- 0
# Daily SPY volatility from minutely returns
sqrt(6.5*60)*60*sd(retsp)
# Daily SPY volatility
# Scale by extra time over weekends and holidays
24*60*60*sd(rutils::diffit(log(SPY_daily[, 4]))[-1] /
    rutils::diffit(.zoo::index(SPY_daily))[-1])

# Calculate SPY returns adjusted for overnight jumps
closep <- log(as.numeric(Cl(HighFreq::SPY[, 4])))
retsp <- rutils::diffit(closep) /
  rutils::diffit(.zoo::index(HighFreq::SPY))
retsp[1] <- 0
closep <- cumsum(retsp)
nrows <- NROW(closep)
# Calculate volatilities for vector of aggregation intervals
interval_s <- seq.int(from=3, to=35, length.out=9)^2
vol_s <- sapply(interval_s, function(interval) {
  num_agg <- nrows %/% interval
  endp <- c(0, nrows - num_agg*interval + (0:num_agg)*interval)
  # endp <- rutils::calc_endpoints(closep, interval=interval)
  sd(rutils::diffit(closep[endp]))
})  # end sapply
# Calculate Hurst as regression slope using formula
vol_log <- log(vol_s)
inter_log <- log(interval_s)
hurs_t <- cov(vol_log, inter_log)/var(inter_log)
# Or using function lm()
model <- lm(vol_log ~ inter_log)
coef(model)[2]

# Calculate Hurst from single data point
(last(vol_log) - log(sd(retsp)))/last(inter_log)
# Plot the volatilities
x11(width=6, height=5)
par(mar=c(4, 4, 2, 1), oma=c(1, 1, 1, 1))
plot(vol_log ~ inter_log, lwd=6, col="red",
     xlab="aggregation intervals (log)", ylab="volatility (log)",
     main="Hurst Exponent for SPY From Volatilities")
abline(model, lwd=3, col="blue")
text(inter_log[2], vol_log[NROW(vol_log)-1],
     paste0("Hurst = ", round(hurs_t, 4)))

# Calculate the rescaled range
interval <- 500
nrows <- NROW(closep); num_agg <- nrows %/% interval
endp <- c(0, nrows - num_agg*interval + (0:num_agg)*interval)
# Or
# endp <- rutils::calc_endpoints(closep, interval=interval)
r_s <- sapply(2:NROW(endp), function(ep) {
  indeks <- endp[ep-1]:endp[ep]
  diff(range(closep[indeks]))/sd(retsp[indeks])
})  # end sapply
mean(r_s)
# Calculate Hurst from single data point
log(mean(r_s))/log(interval)

# Calculate rescaled range for vector of aggregation intervals
nrows <- NROW(closep)
r_s <- sapply(interval_s, function(interval) {
# Calculate end points
  num_agg <- nrows %/% interval
  endp <- c(0, nrows - num_agg*interval + (0:num_agg)*interval)
# Calculate rescaled ranges
  r_s <- sapply(2:NROW(endp), function(ep) {
    indeks <- endp[ep-1]:endp[ep]
    diff(range(closep[indeks]))/sd(retsp[indeks])
  })  # end sapply
  mean(na.omit(r_s))
})  # end sapply
# Calculate Hurst as regression slope using formula
rs_log <- log(r_s)
inter_log <- log(interval_s)
hurs_t <- cov(rs_log, inter_log)/var(inter_log)
# Or using function lm()
model <- lm(rs_log ~ inter_log)
coef(model)[2]

x11(width=6, height=5)
par(mar=c(4, 4, 2, 1), oma=c(1, 1, 1, 1))
plot(rs_log ~ inter_log, lwd=6, col="red",
     xlab="aggregation intervals (log)",
     ylab="rescaled range (log)",
     main="Rescaled Range Analysis for SPY")
abline(model, lwd=3, col="blue")
text(inter_log[2], rs_log[NROW(rs_log)-1],
     paste0("Hurst = ", round(hurs_t, 4)))

options(width=200)
# Load package HighFreq
library(HighFreq)
# Or load the high frequency data file directly:
# symbolv <- load("/Users/jerzy/Develop/R/HighFreq/data/hf_data.RData")
head(HighFreq::SPY_TAQ)
head(HighFreq::SPY)
tail(HighFreq::SPY)

library(rutils)
# Read TAQ trade data from csv file
taq <- data.table::fread(file="/Users/jerzy/Develop/data/xlk_tick_trades2020_0316.csv")
# Inspect the TAQ data
taq
class(taq)
colnames(taq)
sapply(taq, class)
symbol <- taq$SYM_ROOT[1]
# Create date-time index
dates <- paste(taq$DATE, taq$TIME_M)
# Coerce date-time index to POSIXlt
dates <- strptime(dates, "%Y%m%d %H:%M:%OS")
class(dates)
# Display more significant digits
# options("digits")
options(digits=20, digits.secs=10)
last(dates)
unclass(last(dates))
as.numeric(last(dates))
# Coerce date-time index to POSIXct
dates <- as.POSIXct(dates)
class(dates)
last(dates)
unclass(last(dates))
as.numeric(last(dates))
# Calculate the number of ticks per second
n_secs <- as.numeric(last(dates)) - as.numeric(first(dates))
NROW(taq)/(6.5*3600)
# Select TAQ data columns
taq <- taq[, .(price=PRICE, volume=SIZE)]
# Add date-time index
taq <- cbind(index=dates, taq)

# Coerce trade ticks to xts series
xtes <- xts::xts(taq[, .(price, volume)], taq$index)
colnames(xtes) <- paste(symbol, c("Close", "Volume"), sep=".")
save(xtes, file="/Users/jerzy/Develop/data/xlk_tick_trades2020_0316.RData")
# save(xtes, file="/Users/jerzy/Develop/data/xlk_tick_trades2020_0316.RData")
# Plot dygraph
dygraphs::dygraph(xtes$XLK.Close,
  main="XLK Trade Ticks for 2020-03-16")
# Plot in x11 window
x11(width=6, height=5)
quantmod::chart_Series(x=xtes$XLK.Close,
  name="XLK Trade Ticks for 2020-03-16")

# Select the large lots greater than 100
dim(taq)
big_ticks <- taq[taq$volume > 100]
dim(big_ticks)
# Number of large lot ticks per second
NROW(big_ticks)/(6.5*3600)
# Save trade ticks with large lots
data.table::fwrite(big_ticks, file="/Users/jerzy/Develop/data/xlk_tick_trades2020_0316_biglots.csv")
# Coerce trade prices to xts
xtes <- xts::xts(big_ticks[, .(price, volume)], big_ticks$index)
colnames(xtes) <- c("XLK.Close", "XLK.Volume")

# Plot dygraph of the large lots
dygraphs::dygraph(xtes$XLK.Close,
  main="XLK Trade Ticks for 2020-03-16 (large lots only)")
# Plot the large lots
x11(width=6, height=5)
quantmod::chart_Series(x=xtes$XLK.Close,
  name="XLK Trade Ticks for 2020-03-16 (large lots only)")

# Round time index to seconds
good_ticks[, zoo::index := as.POSIXct(round.POSIXt(index, "secs"))]
# Aggregate to OHLC by seconds
ohlc <- good_ticks[, .(open=first(price), high=max(price), low=min(price), close=last(price), volume=sum(volume)), by=index]
# Round time index to minutes
good_ticks[, zoo::index := as.POSIXct(round.POSIXt(index, "mins"))]
# Aggregate to OHLC by minutes
ohlc <- good_ticks[, .(open=first(price), high=max(price), low=min(price), close=last(price), volume=sum(volume)), by=index]

# Coerce OHLC prices to xts
xtes <- xts::xts(ohlc[, -"index"], ohlc$index)
# Plot dygraph of the OHLC prices
dygraphs::dygraph(xtes[, -5], main="XLK Trade Ticks for 2020-03-16 (OHLC)") %>%
  dyCandlestick()
# Plot the OHLC prices
x11(width=6, height=5)
quantmod::chart_Series(x=xtes, TA="add_Vo()",
  name="XLK Trade Ticks for 2020-03-16 (OHLC)")

# Load package HighFreq
library(HighFreq)
head(HighFreq::SPY)

# Load package HighFreq
library(HighFreq)
# Define symbol
symbol <- "SPY"
# Load OHLC data
output_dir <- "/Users/jerzy/Develop/data/hfreq/scrub/"
symbol <- load(file.path(output_dir, paste0(symbol, ".RData")))
interval <-"2013-11-11 09:30:00/2013-11-11 10:30:00"
chart_Series(SPY[interval], name=symbol)

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

# Load package HighFreq
library(HighFreq)
# You can see SPY when listing objects in HighFreq
ls("package:HighFreq")
# You can see SPY when listing datasets in HighFreq
data(package=HighFreq)
# But the SPY dataset isn't listed in the workspace
ls()
# HighFreq datasets are lazy loaded and available when needed
head(HighFreq::SPY)
# Load all the datasets in package HighFreq
data(hf_data)
# HighFreq datasets are now loaded and in the workspace
head(HighFreq::SPY)

library(rutils)  # Load package rutils
# SPY percentage returns
ohlc <- HighFreq::SPY
nrows <- NROW(ohlc)
closep <- log(quantmod::Cl(ohlc))
returns <- rutils::diffit(closep)
colnames(returns) <- "SPY"
# Standardize raw returns to make later comparisons
returns <- (returns - mean(returns))/sd(returns)
# Calculate moments and perform normality test
sapply(c(var=2, skew=3, kurt=4), function(x) sum(returns^x)/nrows)
tseries::jarque.bera.test(returns)
# Fit SPY returns using MASS::fitdistr()
optim_fit <- MASS::fitdistr(returns, densfun="t", df=2)
loc <- optim_fit$estimate[1]
scalev <- optim_fit$estimate[2]

x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(1, 1, 1, 1))
# Plot histogram of SPY returns
histp <- hist(returns, col="lightgrey", mgp=c(2, 1, 0),
  xlab="returns (standardized)", ylab="frequency", xlim=c(-3, 3),
  breaks=1e3, freq=FALSE, main="Distribution of High Frequency SPY Returns")
# lines(density(returns, bw=0.2), lwd=3, col="blue")
# Plot t-distribution function
curve(expr=dt((x-loc)/scalev, df=2)/scalev,
type="l", lwd=3, col="red", add=TRUE)
# Plot the Normal probability distribution
curve(expr=dnorm(x, mean=mean(returns),
  sd=sd(returns)), add=TRUE, lwd=3, col="blue")
# Add legend
legend("topright", inset=0.05, bty="n",
  leg=c("t-distr", "normal"),
  lwd=6, lty=1, col=c("red", "blue"))

# Hourly SPY percentage returns
closep <- log(Cl(xts::to.period(x=ohlc, period="hours")))
retsh <- rutils::diffit(closep)
retsh <- (retsh - mean(retsh))/sd(retsh)
# Daily SPY percentage returns
closep <- log(Cl(xts::to.period(x=ohlc, period="days")))
retsd <- rutils::diffit(closep)
retsd <- (retsd - mean(retsd))/sd(retsd)
# Calculate moments
sapply(list(minutely=returns, hourly=retsh, daily=retsd),
 function(rets) {sapply(c(var=2, skew=3, kurt=4),
          function(x) mean(rets^x))
})  # end sapply

x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(1, 1, 1, 1))
# Plot densities of SPY returns
plot(density(returns, bw=0.4), xlim=c(-3, 3),
     lwd=3, mgp=c(2, 1, 0), col="blue",
     xlab="returns (standardized)", ylab="frequency",
     main="Density of High Frequency SPY Returns")
lines(density(retsh, bw=0.4), lwd=3, col="green")
lines(density(retsd, bw=0.4), lwd=3, col="red")
# Add legend
legend("topright", inset=0.05, bty="n",
  leg=c("minutely", "hourly", "daily"),
  lwd=6, lty=1, col=c("blue", "green", "red"))

# Calculate rolling volatility of SPY returns
ret2013 <- returns["2013-11-11/2013-11-15"]
# Calculate rolling volatility
look_back <- 11
endp <- seq_along(ret2013)
startp <- c(rep_len(1, look_back),
  endp[1:(NROW(endp)-look_back)])
endp[endp < look_back] <- look_back
vol_rolling <- sapply(seq_along(endp),
  function(it) sd(ret2013[startp[it]:endp[it]]))
vol_rolling <- xts::xts(vol_rolling, zoo::index(ret2013))
# Extract time intervals of SPY returns
indeks <- c(60, diff(xts::.index(ret2013)))
head(indeks)
table(indeks)
# Scale SPY returns by time intervals
ret2013 <- 60*ret2013/indeks
# Calculate scaled rolling volatility
vol_scaled <- sapply(seq_along(endp),
  function(it) sd(ret2013[startp[it]:endp[it]]))
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

prices <- read.zoo(file="/Users/jerzy/Develop/lecture_slides/data/bid_ask_bounce.csv",
  header=TRUE, sep=",")
prices <- as.xts(prices)
x11(width=6, height=4)
par(mar=c(2, 2, 0, 0), oma=c(1, 1, 0, 0))
chart_Series(x=prices, name="S&P500 Futures Bid-Ask Bounce")

# Volatility of SPY
sqrt(HighFreq::calcvar_ohlc(ohlc))
# Daily SPY volatility and volume
vol_daily <- sqrt(xts::apply.daily(ohlc, FUN=calcvar_ohlc))
colnames(vol_daily) <- ("SPY_volatility")
volumes <- quantmod::Vo(ohlc)
volume_daily <- xts::apply.daily(volumes, FUN=sum)
colnames(volume_daily) <- ("SPY_volume")
# Plot SPY volatility and volume
datav <- cbind(vol_daily, volume_daily)["2008/2009"]
colnamev <- colnames(datav)
dygraphs::dygraph(datav,
  main="SPY Daily Volatility and Trading Volume") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="red", strokeWidth=3) %>%
  dySeries(name=colnamev[2], axis="y2", col="blue", strokeWidth=3)

# Regress log of daily volume vs volatility
datav <- log(cbind(volume_daily, vol_daily))
colnamev <- colnames(datav)
data_frame <- as.data.frame(datav)
formulav <- as.formula(paste(colnamev, collapse="~"))
model <- lm(formulav, data=data_frame)
# Durbin-Watson test for autocorrelation of residuals
lmtest::dwtest(model)
# Regress diff log of daily volume vs volatility
data_frame <- as.data.frame(rutils::diffit(datav))
model <- lm(formulav, data=data_frame)
lmtest::dwtest(model)
summary(model)
plot(formulav, data=data_frame, main="SPY Daily Trading Volume vs Volatility (log scale)")
abline(model, lwd=3, col="red")
mtext(paste("beta =", round(coef(model)[2], 3)), cex=1.2, lwd=3, side=2, las=2, adj=(-0.5), padj=(-7))

# 60 minutes of data in look_back interval
look_back <- 60
vol2013 <- volumes["2013"]
ret2013 <- returns["2013"]
# Define end points with beginning stub
nrows <- NROW(ret2013)
nagg <- nrows %/% look_back
endp <- nrows-look_back*nagg + (0:nagg)*look_back
startp <- c(1, endp[1:(NROW(endp)-1)])
# Calculate SPY volatility and volume
datav <- sapply(seq_along(endp), function(it) {
  point_s <- startp[it]:endp[it]
  c(volume=sum(vol2013[point_s]),
    volatility=sd(ret2013[point_s]))
})  # end sapply
datav <- t(datav)
datav <- rutils::diffit(log(datav))
data_frame <- as.data.frame(datav)

formulav <- as.formula(paste(colnames(datav), collapse="~"))
model <- lm(formulav, data=data_frame)
lmtest::dwtest(model)
summary(model)
plot(formulav, data=data_frame,
     main="SPY Hourly Trading Volume vs Volatility (log scale)")
abline(model, lwd=3, col="red")
mtext(paste("beta =", round(coef(model)[2], 3)), cex=1.2, lwd=3, side=2, las=2, adj=(-0.5), padj=(-7))

# Scale returns using volume (volume clock)
rets_scaled <- ifelse(volumes > 1e4, returns/volumes, 0)
rets_scaled <- rets_scaled/sd(rets_scaled)
# Calculate moments of scaled returns
nrows <- NROW(returns)
sapply(list(returns=returns, rets_scaled=rets_scaled),
  function(rets) {sapply(c(skew=3, kurt=4),
     function(x) sum((rets/sd(rets))^x)/nrows)
})  # end sapply

x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(1, 1, 1, 1))
# Plot densities of SPY returns
plot(density(returns), xlim=c(-3, 3),
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
Box.test(returns, lag=10, type="Ljung")
# Ljung-Box test for daily SPY returns
Box.test(retsd, lag=10, type="Ljung")
# Ljung-Box test statistics for scaled SPY returns
sapply(list(returns=returns, rets_scaled=rets_scaled),
  function(rets) {
    Box.test(rets, lag=10, type="Ljung")$statistic
})  # end sapply
# Ljung-Box test statistics for aggregated SPY returns
sapply(list(minutely=returns, hourly=retsh, daily=retsd),
  function(rets) {
    Box.test(rets, lag=10, type="Ljung")$statistic
})  # end sapply

# Set plot parameters
x11(width=6, height=8)
par(mar=c(4, 4, 2, 1), oma=c(0, 0, 0, 0))
layout(matrix(c(1, 2), ncol=1), widths=c(6, 6), heights=c(4, 4))
# Plot the partial autocorrelations of minutely SPY returns
pa_cf <- pacf(as.numeric(returns), lag=10,
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
dates <- format(zoo::index(returns), "%H:%M")
# Aggregate the mean volume
volume_agg <- tapply(X=volumes, INDEX=dates, FUN=mean)
volume_agg <- drop(volume_agg)
# Aggregate the mean volatility
vol_agg <- tapply(X=returns^2, INDEX=dates, FUN=mean)
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

# Futures contracts codes
futures <- rbind(c("S&P500 index", "ES"),
              c("10yr Treasury", "ZN"),
              c("VIX index", "VX"),
              c("Gold", "GC"),
              c("Oil", "CL"),
              c("Euro FX", "EC"),
              c("Swiss franc", "SF"),
              c("Japanese Yen", "JY"))
colnames(futures) <- c("Futures contract", "Code")
print(xtable::xtable(futures), comment=FALSE, size="scriptsize", include.rownames=FALSE, latex.environments="flushleft")

# Monthly futures contract codes
codes <- cbind(c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"),
                     c("F", "G", "H", "J", "K", "M", "N", "Q", "U", "V", "X", "Z"))
colnames(codes) <- c("Month", "Code")
print(xtable::xtable(codes), comment=FALSE, size="scriptsize", include.rownames=FALSE, latex.environments="flushright")

# Futures contracts codes
futures <- rbind(c("S&P500 index", "SP", "ES"),
              c("10yr Treasury", "ZN", "ZN"),
              c("VIX index", "VX", "delisted"),
              c("Gold", "GC", "YG"),
              c("Oil", "CL", "QM"),
              c("Euro FX", "EC", "E7"),
              c("Swiss franc", "SF", "MSF"),
              c("Japanese Yen", "JY", "J7"))
colnames(futures) <- c("Futures contract", "Standard", "E-mini")
print(xtable::xtable(futures), comment=FALSE, size="scriptsize", include.rownames=FALSE, latex.environments="flushleft")

# Load data for S&P Emini futures June 2019 contract
dir_name <- "/Users/jerzy/Develop/data/ib_data"
file_name <- file.path(dir_name, "ESohlc.csv")
# Read a data table from CSV file
prices <- data.table::fread(file_name)
class(prices)
# Coerce first column from string to date-time
unlist(sapply(prices, class))
tail(prices)
prices$Index <- as.POSIXct(prices$Index,
  tz="America/New_York", origin="1970-01-01")
# Coerce prices into xts series
prices <- data.table::as.xts.data.table(prices)
class(prices)
tail(prices)
colnames(prices)[1:5] <- c("Open", "High", "Low", "Close", "Volume")
tail(prices)

# Plot OHLC data in x11 window
x11(width=5, height=4)  # Open x11 for plotting
par(mar=c(5, 5, 2, 1), oma=c(0, 0, 0, 0))
chart_Series(x=prices, TA="add_Vo()",
  name="S&P500 futures")
# Plot dygraph
dygraphs::dygraph(prices[, 1:4], main="OHLC prices") %>%
  dyCandlestick()

# Load ESU8 data
dir_name <- "/Users/jerzy/Develop/data/ib_data"
file_name <- file.path(dir_name, "ESU8.csv")
ESU8 <- data.table::fread(file_name)
# Coerce ESU8 into xts series
ESU8$V1 <- as.Date(as.POSIXct.numeric(ESU8$V1,
    tz="America/New_York", origin="1970-01-01"))
ESU8 <- data.table::as.xts.data.table(ESU8)
colnames(ESU8)[1:5] <- c("Open", "High", "Low", "Close", "Volume")
# Load ESM8 data
file_name <- file.path(dir_name, "ESM8.csv")
ESM8 <- data.table::fread(file_name)
# Coerce ESM8 into xts series
ESM8$V1 <- as.Date(as.POSIXct.numeric(ESM8$V1,
    tz="America/New_York", origin="1970-01-01"))
ESM8 <- data.table::as.xts.data.table(ESM8)
colnames(ESM8)[1:5] <- c("Open", "High", "Low", "Close", "Volume")

x11(width=6, height=5)  # Open x11 for plotting
# Plot last month of ESU8 and ESM8 volume data
endd <- end(ESM8)
startd <- (endd - 30)
volumes <- cbind(Vo(ESU8),
  Vo(ESM8))[paste0(startd, "/", endd)]
colnames(volumes) <- c("ESU8", "ESM8")
colors <- c("blue", "green")
plot(volumes, col=colors, lwd=3, major.ticks="days",
     format.labels="%b-%d", observation.based=TRUE,
     main="Volumes of ESU8 and ESM8 futures")
legend("topleft", legend=colnames(volumes), col=colors,
 title=NULL, bty="n", lty=1, lwd=6, inset=0.1, cex=0.7)

# Find date when ESU8 volume exceeds ESM8
exceeds <- (volumes[, "ESU8"] > volumes[, "ESM8"])
indeks <- match(TRUE, exceeds)
# indeks <- min(which(exceeds))
# Scale the ESM8 prices
indeks <- zoo::index(exceeds[indeks])
ratio <- as.numeric(Cl(ESU8[indeks])/Cl(ESM8[indeks]))
ESM8[, 1:4] <- ratio*ESM8[, 1:4]
# Calculate continuous contract prices
chain_ed <- rbind(ESM8[zoo::index(ESM8) < indeks],
            ESU8[zoo::index(ESU8) >= indeks])
# Or
# Chain_ed <- rbind(ESM8[paste0("/", indeks-1)],
#                   ESU8[paste0(indeks, "/")])
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
load(file="/Users/jerzy/Develop/data/ib_data/vix_cboe.RData")
ls(vix_env)
vix_env$vix_index <- vix_index
save(vix_env, file="/Users/jerzy/Develop/data/ib_data/vix_cboe.RData")

# Plot VIX OHLC data in x11 window
chart_Series(x=vix_index["2018"], name="VIX Index")
# Plot dygraph
dygraphs::dygraph(vix_index, main="VIX Index") %>%
  dyCandlestick()

# Read CBOE monthly futures expiration dates
dates <- read.csv(
  file="/Users/jerzy/Develop/data/vix_data/vix_dates.csv")
dates <- as.Date(dates[, 1])
years <- format(dates, format="%Y")
years <- substring(years, 4)
# Monthly futures contract codes
codes <-
  c("F", "G", "H", "J", "K", "M",
    "N", "Q", "U", "V", "X", "Z")
symbolv <- paste0("VX", codes, years)
dates <- as.data.frame(dates)
colnames(dates) <- "exp_dates"
rownames(dates) <- symbolv
# Write dates to CSV file, with row names
write.csv(dates, row.names=TRUE,
  file="/Users/jerzy/Develop/data/vix_data/vix_futures.csv")
# Read back CBOE futures expiration dates
dates <- read.csv(file="/Users/jerzy/Develop/data/vix_data/vix_futures.csv",
  row.names=1)
dates[, 1] <- as.Date(dates[, 1])

# Load VIX futures data from binary file
load(file="/Users/jerzy/Develop/data/vix_data/vix_cboe.RData")
# Get all VIX futures for 2018 except January
symbolv <- ls(vix_env)
symbolv <- symbolv[grep("*8", symbolv)]
symbolv <- symbolv[2:9]
# Specify dates for curves
low_vol <- as.Date("2018-01-11")
hi_vol <- as.Date("2018-02-05")
# Extract all VIX futures prices on the dates
curve_s <- lapply(symbolv, function(symbol) {
  xtes <- get(x=symbol, envir=vix_env)
  Cl(xtes[c(low_vol, hi_vol)])
})  # end lapply
curve_s <- rutils::do_call(cbind, curve_s)
colnames(curve_s) <- symbolv
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
load(file="/Users/jerzy/Develop/data/vix_data/vix_cboe.RData")
# Read CBOE futures expiration dates
dates <- read.csv(file="/Users/jerzy/Develop/data/vix_data/vix_futures.csv",
  row.names=1)
symbolv <- rownames(dates)
dates <- as.Date(dates[, 1])
todayd <- as.Date("2018-05-07")
maturi_ty <- (todayd + 30)
# Find neighboring futures contracts
indeks <- match(TRUE, dates > maturi_ty)
front_date <- dates[indeks-1]
back_date <- dates[indeks]
front_symbol <- symbolv[indeks-1]
back_symbol <- symbolv[indeks]
front_price <- get(x=front_symbol, envir=vix_env)
front_price <- as.numeric(Cl(front_price[todayd]))
back_price <- get(x=back_symbol, envir=vix_env)
back_price <- as.numeric(Cl(back_price[todayd]))
# Calculate the constant maturity 30-day futures price
ra_tio <- as.numeric(maturi_ty - front_date) /
  as.numeric(back_date - front_date)
pric_e <- (ra_tio*back_price + (1-ra_tio)*front_price)

x11(width=5, height=3)  # Open x11 for plotting
# Load VIX futures data from binary file
load(file="/Users/jerzy/Develop/data/vix_data/vix_cboe.RData")
# Plot VIX and SVXY data in x11 window
plot_theme <- chart_theme()
plot_theme$col$line.col <- "blue"
chart_Series(x=Cl(vix_env$vix_index["2007/"]),
       theme=plot_theme, name="VIX Index")
chart_Series(x=Cl(rutils::etfenv$VTI["2007/"]),
       theme=plot_theme, name="VTI ETF")

chart_Series(x=Cl(vix_env$vix_index["2017/2018"]),
       theme=plot_theme, name="VIX Index")
chart_Series(x=Cl(rutils::etfenv$SVXY["2017/2018"]),
       theme=plot_theme, name="SVXY ETF")
