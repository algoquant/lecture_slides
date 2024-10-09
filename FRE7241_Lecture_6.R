# Select all the ETF symbols except "VXX", "SVXY" "MTUM", "QUAL", "VLUE", "USMV", "AIEQ", and "VYM"
symbolv <- colnames(rutils::etfenv$returns)
symbolv <- symbolv[!(symbolv %in% c("VXX", "SVXY", "MTUM", "QUAL", "VLUE", "USMV", "AIEQ", "VYM"))]
# Extract columns of rutils::etfenv$returns and overwrite NA values
retp <- rutils::etfenv$returns["1999/", symbolv]
sum(is.na(retp)) # Number of NA values
nstocks <- NCOL(retp)
datev <- zoo::index(retp)
# Calculate the covariance ignoring NA values
covmat <- cov(retp, use="pairwise.complete.obs")
sum(is.na(covmat))
# Calculate the inverse of covmat
invmat <- solve(covmat)
round(invmat %*% covmat, digits=5)
# Calculate the generalized inverse of covmat
invreg <- MASS::ginv(covmat)
all.equal(unname(invmat), invreg)

# Create rectangular matrix with collinear columns
matv <- matrix(rnorm(10*8), nc=10)
# Calculate covariance matrix
covmat <- cov(matv)
# Calculate inverse of covmat - error
invmat <- solve(covmat)
# Perform eigen decomposition
eigend <- eigen(covmat)
eigenvec <- eigend$vectors
eigenval <- eigend$values
# Set tolerance for determining zero singular values
precv <- sqrt(.Machine$double.eps)
# Calculate generalized inverse from the eigen decomposition
notzero <- (eigenval > (precv*eigenval[1]))
inveigen <- eigenvec[, notzero] %*%
  (t(eigenvec[, notzero]) / eigenval[notzero])
# Inverse property of invreg doesn't hold
all.equal(covmat, inveigen %*% covmat)
# Generalized inverse property of invreg holds
all.equal(covmat, covmat %*% inveigen %*% covmat)
# Calculate generalized inverse using MASS::ginv()
invreg <- MASS::ginv(covmat)
# Verify that inveigen is the same as invreg
all.equal(inveigen, invreg)

# Returns in excess of risk-free rate
raterf <- 0.03/252
retx <- (retp - raterf)
# Maximum Sharpe weights in-sample interval
retis <- retp["/2014"]
invreg <- MASS::ginv(cov(retis, use="pairwise.complete.obs"))
weightv <- drop(invreg %*% colMeans(retx["/2014"], na.rm=TRUE))
weightv <- weightv/sqrt(sum(weightv^2))
names(weightv) <- colnames(retp)

# Plot portfolio weights
barplot(sort(weightv), main="Maximum Sharpe Weights", cex.names=0.7)

# Calculate the equal weight index
retew <- xts::xts(rowMeans(retp, na.rm=TRUE), datev)
# Calculate the in-sample weighted returns using Rcpp
pnlis <- HighFreq::mult_mat(weightv, retis)
# Or using the transpose
# pnlis <- unname(t(t(retis)*weightv))
pnlis <- rowMeans(pnlis, na.rm=TRUE)
pnlis <- pnlis*sd(retew["/2014"])/sd(pnlis)

# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retew["/2014"], pnlis, (pnlis + retew["/2014"])/2)
colnames(wealthv) <- c("Equal Weight", "Max Sharpe", "Combined")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Dygraph cumulative wealth
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="In-Sample Optimal Portfolio Returns") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", strokeWidth=3) %>%
  dyLegend(width=300)

# Calculate the equal weight index
retos <- retp["2015/"]
# Calculate out-of-sample portfolio returns
pnlos <- HighFreq::mult_mat(weightv, retos)
pnlos <- rowMeans(pnlos, na.rm=TRUE)
pnlos <- pnlos*sd(retew["2015/"])/sd(pnlos)
wealthv <- cbind(retew["2015/"], pnlos, (pnlos + retew["2015/"])/2)
colnames(wealthv) <- c("Equal Weight", "Max Sharpe", "Combined")
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))

# Dygraph cumulative wealth
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Out-of-Sample Optimal Portfolio Returns") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", strokeWidth=3) %>%
  dyLegend(width=300)

# Maximum Sharpe weights in-sample interval
invreg <- MASS::ginv(cov(retis, use="pairwise.complete.obs"))
weightv <- invreg %*% colMeans(retx["/2014"], na.rm=TRUE)
names(weightv) <- colnames(retp)
# Calculate cumulative wealth
pnls <- HighFreq::mult_mat(weightv, retp)
pnls <- rowMeans(pnls, na.rm=TRUE)
pnls <- pnls*sd(retew)/sd(pnls)
wealthv <- cbind(retew, pnls, (pnls + retew)/2)
colnames(wealthv) <- c("Equal Weight", "Optimal", "Combined")
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))

# Dygraph cumulative wealth
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Out-of-Sample Optimal Portfolio Returns for ETFs") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", strokeWidth=3) %>%
  dyEvent(zoo::index(last(retis[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=400)

# Calculate in-sample covariance matrix
covmat <- cov(retis, use="pairwise.complete.obs")
eigend <- eigen(covmat)
eigenvec <- eigend$vectors
eigenval <- eigend$values
# Plot the eigenvalues
barplot(eigenval, main="ETF Covariance Eigenvalues", cex.names=0.7)
# Calculate reduced inverse of covariance matrix
dimax <- 9
invred <- eigenvec[, 1:dimax] %*%
  (t(eigenvec[, 1:dimax]) / eigenval[1:dimax])
# Reduced inverse does not satisfy matrix inverse property
all.equal(covmat, covmat %*% invred %*% covmat)

# Calculate portfolio weights
weightv <- drop(invred %*% colMeans(retis, na.rm=TRUE))
names(weightv) <- colnames(retp)
# Calculate cumulative wealth
pnls <- HighFreq::mult_mat(weightv, retp)
pnls <- rowMeans(pnls, na.rm=TRUE)
pnls <- pnls*sd(retew)/sd(pnls)
wealthv <- cbind(retew, pnls, (pnls + retew)/2)
colnames(wealthv) <- c("Equal Weight", "DimReduction", "Combined")
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))

# Dygraph cumulative wealth
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Optimal Portfolio Returns With Dimension Reduction") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", strokeWidth=3) %>%
  dyEvent(zoo::index(last(retis[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=400)

# Shrink the in-sample returns to their mean
alphac <- 0.7
retxm <- rowMeans(retx["/2014"], na.rm=TRUE)
retxis <- (1-alphac)*retx["/2014"] + alphac*retxm
# Calculate portfolio weights
weightv <- invred %*% colMeans(retxis, na.rm=TRUE)
# Calculate cumulative wealth
pnls <- HighFreq::mult_mat(weightv, retp)
pnls <- rowMeans(pnls, na.rm=TRUE)
pnls <- pnls*sd(retew)/sd(pnls)
wealthv <- cbind(retew, pnls, (pnls + retew)/2)
colnames(wealthv) <- c("Equal Weight", "Optimal", "Combined")

# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Dygraph cumulative wealth
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Optimal Portfolio With Dimension Reduction and Return Shrinkage") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", strokeWidth=3) %>%
  dyEvent(zoo::index(last(retis[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=300)

# Define monthly end points
endd <- rutils::calc_endpoints(retp, interval="months")
endd <- endd[endd > (nstocks+1)]
npts <- NROW(endd)
lookb <- 3
startp <- c(rep_len(0, lookb), endd[1:(npts-lookb)])
# Perform loop over end points
pnls <- lapply(1:(npts-1), function(tday) {
    # Calculate the portfolio weights
    retis <- retx[startp[tday]:endd[tday], ]
    covmat <- cov(retis, use="pairwise.complete.obs")
    covmat[is.na(covmat)] <- 0
    invreg <- MASS::ginv(covmat)
    colm <- colMeans(retis, na.rm=TRUE)
    colm[is.na(colm)] <- 0
    weightv <- invreg %*% colm
    # Calculate the in-sample portfolio returns
    pnlis <- HighFreq::mult_mat(weightv, retis)
    pnlis <- rowMeans(pnlis, na.rm=TRUE)
    # Calculate the out-of-sample portfolio returns
    retos <- retp[(endd[tday]+1):endd[tday+1], ]
    pnlos <- HighFreq::mult_mat(weightv, retos)
    pnlos <- rowMeans(pnlos, na.rm=TRUE)
    xts::xts(pnlos, zoo::index(retos))
})  # end lapply
pnls <- do.call(rbind, pnls)
pnls <- rbind(retew[paste0("/", start(pnls)-1)], pnls)

# Calculate the Sharpe and Sortino ratios
pnls <- pnls*sd(retew)/sd(pnls)
wealthv <- cbind(retew, pnls, (pnls + retew)/2)
colnames(wealthv) <- c("Index", "PortfStrat", "Combined")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))

# Dygraph cumulative wealth
dygraphs::dygraph(cumsum(wealthv)[endd], main="Monthly ETF Rolling Portfolio Strategy") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", strokeWidth=3) %>%
  dyLegend(show="always", width=300)

# Perform loop over end points
dimax <- 9
pnls <- lapply(1:(npts-1), function(tday) {
    # Calculate the portfolio weights
    retis <- retx[startp[tday]:endd[tday], ]
    covmat <- cov(retis, use="pairwise.complete.obs")
    covmat[is.na(covmat)] <- 0
    eigend <- eigen(covmat)
    eigenvec <- eigend$vectors
    eigenval <- eigend$values
    invred <- eigenvec[, 1:dimax] %*%
(t(eigenvec[, 1:dimax]) / eigenval[1:dimax])
    colm <- colMeans(retis, na.rm=TRUE)
    colm[is.na(colm)] <- 0
    weightv <- invred %*% colm
    # Calculate the in-sample portfolio returns
    pnlis <- HighFreq::mult_mat(weightv, retis)
    pnlis <- rowMeans(pnlis, na.rm=TRUE)
    weightv <- weightv*0.01/sd(pnlis)
    # Calculate the out-of-sample portfolio returns
    retos <- retp[(endd[tday]+1):endd[tday+1], ]
    pnlos <- HighFreq::mult_mat(weightv, retos)
    pnlos <- rowMeans(pnlos, na.rm=TRUE)
    xts::xts(pnlos, zoo::index(retos))
})  # end lapply
pnls <- do.call(rbind, pnls)
pnls <- rbind(retew[paste0("/", start(pnls)-1)], pnls)

# Calculate the Sharpe and Sortino ratios
pnls <- pnls*sd(retew)/sd(pnls)
wealthv <- cbind(retew, pnls, (pnls + retew)/2)
colnames(wealthv) <- c("Index", "PortfStrat", "Combined")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Dygraph cumulative wealth
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Rolling Portfolio Strategy With Dimension Reduction") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", strokeWidth=3) %>%
  dyLegend(show="always", width=300)

alphac <- 0.7 # Return shrinkage intensity
# Perform loop over end points
pnls <- lapply(1:(npts-1), function(tday) {
    # Shrink the in-sample returns to their mean
    retis <- retx[startp[tday]:endd[tday], ]
    rowm <- rowMeans(retis, na.rm=TRUE)
    rowm[is.na(rowm)] <- 0
    retis <- (1-alphac)*retis + alphac*rowm
    # Calculate the portfolio weights
    covmat <- cov(retis, use="pairwise.complete.obs")
    covmat[is.na(covmat)] <- 0
    eigend <- eigen(covmat)
    eigenvec <- eigend$vectors
    eigenval <- eigend$values
    invred <- eigenvec[, 1:dimax] %*%
(t(eigenvec[, 1:dimax]) / eigenval[1:dimax])
    colm <- colMeans(retis, na.rm=TRUE)
    colm[is.na(colm)] <- 0
    weightv <- invred %*% colm
    # Scale the weights to volatility target
    pnlis <- HighFreq::mult_mat(weightv, retis)
    pnlis <- rowMeans(pnlis, na.rm=TRUE)
    weightv <- weightv*0.01/sd(pnlis)
    # Calculate the out-of-sample portfolio returns
    retos <- retp[(endd[tday]+1):endd[tday+1], ]
    pnlos <- HighFreq::mult_mat(weightv, retos)
    pnlos <- rowMeans(pnlos, na.rm=TRUE)
    xts::xts(pnlos, zoo::index(retos))
})  # end lapply
pnls <- do.call(rbind, pnls)
pnls <- rbind(retew[paste0("/", start(pnls)-1)], pnls)

# Calculate the Sharpe and Sortino ratios
pnls <- pnls*sd(retew)/sd(pnls)
wealthv <- cbind(retew, pnls, (pnls + retew)/2)
colnames(wealthv) <- c("Index", "PortfStrat", "Combined")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Dygraph cumulative wealth
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Rolling Portfolio Strategy With Return Shrinkage") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", strokeWidth=3) %>%
  dyLegend(show="always", width=300)

# Define backtest functional for rolling portfolio strategy
roll_portf <- function(retx, # Excess returns
                 retp, # Stock returns
                 endd, # End points
                 lookb=12, # Look-back interval
                 dimax=3, # Dimension reduction parameter
                 alphac=0.0, # Return shrinkage intensity
                 bidask=0.0, # Bid-offer spread
                 ...) {
  npts <- NROW(endd)
  startp <- c(rep_len(0, lookb), endd[1:(npts-lookb)])
  pnls <- lapply(1:(npts-1), function(tday) {
    retis <- retx[startp[tday]:endd[tday], ]
    # Shrink the in-sample returns to their mean
    if (alphac > 0) {
rowm <- rowMeans(retis, na.rm=TRUE)
rowm[is.na(rowm)] <- 0
retis <- (1-alphac)*retis + alphac*rowm
    } # end if
    # Calculate the portfolio weights
    covmat <- cov(retis, use="pairwise.complete.obs")
    covmat[is.na(covmat)] <- 0
    eigend <- eigen(covmat)
    eigenvec <- eigend$vectors
    eigenval <- eigend$values
    invred <- eigenvec[, 1:dimax] %*% (t(eigenvec[, 1:dimax]) / eigenval[1:dimax])
    colm <- colMeans(retis, na.rm=TRUE)
    colm[is.na(colm)] <- 0
    weightv <- invred %*% colm
    # Scale the weights to volatility target
    pnlis <- HighFreq::mult_mat(weightv, retis)
    pnlis <- rowMeans(pnlis, na.rm=TRUE)
    weightv <- weightv*0.01/sd(pnlis)
    # Calculate the out-of-sample portfolio returns
    retos <- retp[(endd[tday]+1):endd[tday+1], ]
    pnlos <- HighFreq::mult_mat(weightv, retos)
    pnlos <- rowMeans(pnlos, na.rm=TRUE)
    xts::xts(pnlos, zoo::index(retos))
  })  # end lapply
  pnls <- do.call(rbind, pnls)
  # Add warmup period to pnls
  rbind(retew[paste0("/", start(pnls)-1)], pnls)
}  # end roll_portf

# Simulate a monthly ETF portfolio strategy
pnls <- roll_portf(retx=retx, retp=retp, endd=endd,
  lookb=lookb, dimax=dimax)
# Perform sapply loop over lookbv
lookbv <- seq(2, 15, by=1)
library(parallel)  # Load package parallel
ncores <- detectCores() - 1
pnls <- mclapply(lookbv, roll_portf, retp=retp, retx=retx,
  endd=endd, dimax=dimax, mc.cores=ncores)
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("lookb=", lookbv)
pnlsums <- sapply(pnls, sum)
lookb <- lookbv[which.max(pnlsums)]

# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(pnls, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of monthly ETF portfolio strategies
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls)[endd], main="Rolling Portfolio Strategies") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=600)
# Plot portfolio strategies using quantmod
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(pnls))
quantmod::chart_Series(cumsum(pnls),
  theme=plot_theme, name="Rolling Portfolio Strategies")
legend("bottomleft", legend=colnames(pnls),
  inset=0.02, bg="white", cex=0.7, lwd=rep(6, NCOL(retp)),
  col=plot_theme$col$line.col, bty="n")

# Perform backtest for different dimax values
dimv <- 2:11
pnls <- mclapply(dimv, roll_portf, retp=retp, retx=retx,
  endd=endd, lookb=lookb, mc.zcores=ncores)
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("dimax=", dimv)
pnlsums <- sapply(pnls, sum)
dimax <- dimv[which.max(pnlsums)]

# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(pnls, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of monthly ETF portfolio strategies
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls)[endd],
  main="Rolling Portfolio Strategies With Dimension Reduction") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Plot portfolio strategies using quantmod
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(pnls))
quantmod::chart_Series(cumsum(pnls),
  theme=plot_theme, name="Rolling Portfolio Strategies")
legend("bottomleft", legend=colnames(pnls),
  inset=0.02, bg="white", cex=0.7, lwd=rep(6, NCOL(retp)),
  col=plot_theme$col$line.col, bty="n")

load("/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
# Overwrite NA values in returns
which(colnames(retstock) == "WRK")
retstock <- retstock[, -which(colnames(retstock) == "WRK")]
retp <- retstock["2000/"]
retp[is.na(retp)] <- 0
nstocks <- NCOL(retp)
datev <- zoo::index(retp)
raterf <- 0.03/252
retx <- (retp - raterf)
retis <- retp["/2014"]
# Maximum Sharpe weights in-sample interval
covmat <- cov(retis)
invreg <- MASS::ginv(covmat)
colmeanv <- colMeans(retx["/2014"], na.rm=TRUE)
weightv <- invreg %*% colmeanv
rownames(weightv) <- colnames(retp)
# Calculate the portfolio returns
pnls <- HighFreq::mult_mat(weightv, retp)
pnls <- rowMeans(pnls, na.rm=TRUE)
retew <- xts::xts(rowMeans(retp, na.rm=TRUE), datev)
pnls <- pnls*sd(retew)/sd(pnls)
wealthv <- cbind(retew, pnls)
colnames(wealthv) <- c("Equal Weight", "Optimal")

# Calculate the in-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv["/2014"],
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv["2015/"],
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of cumulative portfolio returns
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Out-of-Sample Optimal Portfolio Returns for Stocks") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyEvent(zoo::index(last(retis[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=300)

# Calculate reduced inverse of covariance matrix
dimax <- 5
eigend <- eigen(covmat)
eigenvec <- eigend$vectors
eigenval <- eigend$values
invred <- eigenvec[, 1:dimax] %*%
  (t(eigenvec[, 1:dimax]) / eigenval[1:dimax])
# Calculate portfolio weights and returns
weightv <- invred %*% colmeanv
rownames(weightv) <- colnames(retp)
pnls <- HighFreq::mult_mat(weightv, retp)
pnls <- rowMeans(pnls, na.rm=TRUE)
pnls <- pnls*sd(retew)/sd(pnls)

# Combine with equal weight
wealthv <- cbind(retew, pnls)
colnames(wealthv) <- c("Equal Weight", "Optimal")
# Calculate the in-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv["/2014"],
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv["2015/"],
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of cumulative portfolio returns
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Out-of-Sample Returns for Stocks with Dimension Reduction") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyEvent(zoo::index(last(retis[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=300)

# Shrink the in-sample returns to their mean
alphac <- 0.7
retxm <- rowMeans(retx["/2014"], na.rm=TRUE)
retxis <- (1-alphac)*retx["/2014"] + alphac*retxm
# Calculate portfolio weights and returns
weightv <- invred %*% colMeans(retxis, na.rm=TRUE)
rownames(weightv) <- colnames(retp)
pnls <- HighFreq::mult_mat(weightv, retp)
pnls <- rowMeans(pnls, na.rm=TRUE)
pnls <- pnls*sd(retew)/sd(pnls)

# Combine with equal weight
wealthv <- cbind(retew, pnls)
colnames(wealthv) <- c("Equal Weight", "Optimal")
# Calculate the in-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv["/2014"],
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv["2015/"],
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of cumulative portfolio returns
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Out-of-Sample Returns for Stocks with Return Shrinkage") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyEvent(zoo::index(last(retis[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=300)

# Define monthly end points
endd <- rutils::calc_endpoints(retp, interval="months")
endd <- endd[endd > (nstocks+1)]
npts <- NROW(endd) ; lookb <- 12
startp <- c(rep_len(0, lookb), endd[1:(npts-lookb)])
# !!! Perform parallel loop over end points - takes very long!!!
library(parallel)  # Load package parallel
ncores <- detectCores() - 1
pnls <- mclapply(1:(npts-1), function(tday) {
    # Subset the excess returns
    retis <- retx[startp[tday]:endd[tday], ]
    invreg <- MASS::ginv(cov(retis, use="pairwise.complete.obs"))
    # Calculate the maximum Sharpe ratio portfolio weights
    weightv <- invreg %*% colMeans(retis, na.rm=TRUE)
    # Zero weights if sparse data
    zerov <- sapply(retis, function(x) (sum(x == 0) > 5))
    weightv[zerov] <- 0
    # Calculate in-sample portfolio returns
    pnlis <- (retis %*% weightv)
    # Scale the weights to volatility target
    weightv <- weightv*0.01/sd(pnlis)
    # Calculate the out-of-sample portfolio returns
    retos <- retp[(endd[tday]+1):endd[tday+1], ]
    xts::xts(retos %*% weightv, zoo::index(retos))
})  # end lapply
pnls <- rutils::do_call(rbind, pnls)
pnls <- rbind(retew[paste0("/", start(pnls)-1)], pnls*sd(retew)/sd(pnls))

# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retew, pnls)
colnames(wealthv) <- c("Equal Weight", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))

# Plot cumulative strategy returns
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Rolling Portfolio Strategy for S&P500 Stocks") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Create random matrix of returns
matv <- matrix(rnorm(300), nc=5)
# Reduced inverse of covariance matrix
dimax <- 3
eigend <- eigen(covmat)
invred <- eigend$vectors[, 1:dimax] %*%
  (t(eigend$vectors[, 1:dimax]) / eigend$values[1:dimax])
# Reduced inverse using RcppArmadillo
invarma <- HighFreq::calc_inv(covmat, dimax)
all.equal(invred, invarma)
# Microbenchmark RcppArmadillo code
library(microbenchmark)
summary(microbenchmark(
  rcode={eigend <- eigen(covmat)
    eigend$vectors[, 1:dimax] %*%
(t(eigend$vectors[, 1:dimax]) / eigend$values[1:dimax])
  },
  rcpp=calc_inv(covmat, dimax),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# Shift end points to C++ convention
endd <- (endd - 1)
endd[endd < 0] <- 0
startp <- (startp - 1)
startp[startp < 0] <- 0
# Specify dimension reduction and return shrinkage using list of portfolio optimization parameters
dimax <- 9
alphac <- 0.7
controlv <- HighFreq::param_portf(method="maxsharpe",
  dimax=dimax, alpha=alphac)
# Perform backtest in Rcpp - takes long!!!
pnls <- HighFreq::back_test(retx=retx, retp=retp,
  startp=startp, endd=endd, controlv=controlv)
pnls <- pnls*sd(retew)/sd(pnls)

# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot cumulative strategy returns
wealthv <- cbind(retew, pnls, (pnls + retew)/2)
colnames(wealthv) <- c("Index", "PortfStrat", "Combined")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Rolling S&P500 Portfolio Strategy With Shrinkage") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", label="Combined", strokeWidth=3) %>%
  dyLegend(show="always", width=300)

# Perform backtest over vector of dimension reduction parameters
dimv <- seq(from=3, to=40, by=2)
pnls <- mclapply(dimv, function(dimax) {
  controlv <- HighFreq::param_portf(method="maxsharpe",
    dimax=dimax, alpha=alphac)
  HighFreq::back_test(retx=retx, retp=retp,
    startp=startp, endd=endd, controlv=controlv)
})  # end lapply
profilev <- sapply(pnls, sum)
whichmax <- which.max(profilev)
dimax <- dimv[whichmax]

plot(x=dimv, y=profilev, t="l", xlab="dimax", ylab="pnl",
  main="Rolling Strategy PnL as Function of dimax")

# Perform backtest over vector of return shrinkage intensities
alphav <- seq(from=0.5, to=0.9, by=0.1)
pnls <- mclapply(alphav, function(alphac) {
  controlv <- HighFreq::param_portf(method="maxsharpe",
      dimax=dimax, alpha=alphac)
  HighFreq::back_test(retx=retx, retp=retp,
      startp=startp, endd=endd, controlv=controlv)
})  # end lapply
profilev <- sapply(pnls, sum)
whichmax <- which.max(profilev)
alphac <- alphav[whichmax]

plot(x=alphav, y=profilev, t="l",
  main="Rolling Strategy PnL as Function of Return Shrinkage",
  xlab="Shrinkage Intensity Alpha", ylab="pnl")

# Create list of model parameters
controlv <- HighFreq::param_portf(method="maxsharpe",
      dimax=dimax, alpha=alphac)
# Perform backtest over look-backs
lookbv <- seq(from=5, to=16, by=1)
pnls <- mclapply(lookbv, function(lookb) {
  startp <- c(rep_len(0, lookb), endd[1:(npts-lookb)])
  startp <- (startp - 1) ; startp[startp < 0] <- 0
  HighFreq::back_test(retx=retx, retp=retp,
    startp=startp, endd=endd, controlv=controlv)
})  # end lapply
profilev <- sapply(pnls, sum)
plot(x=lookbv, y=profilev, t="l", main="Strategy PnL as Function of Look-back Interval",
  xlab="Look-back Interval", ylab="pnl")

# Calculate the out-of-sample Sharpe and Sortino ratios
whichmax <- which.max(profilev)
lookb <- lookbv[whichmax]
pnls <- pnls[[whichmax]]
pnls <- pnls*sd(retew)/sd(pnls)
wealthv <- cbind(retew, pnls, (pnls + retew)/2)
colnames(wealthv) <- c("Index", "PortfStrat", "Combined")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Dygraph the cumulative wealth
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Optimal Rolling S&P500 Portfolio Strategy") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", label="Combined", strokeWidth=3) %>%
  dyLegend(show="always", width=300)
