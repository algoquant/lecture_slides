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
# Inverse property of inveigen isn't satisfied
all.equal(inveigen %*% covmat, diag(NROW(covmat)))
# Generalized inverse property of inveigen is satisfied
all.equal(covmat %*% inveigen %*% covmat, covmat)
# Calculate generalized inverse using MASS::ginv()
invreg <- MASS::ginv(covmat)
# Verify that inveigen is the same as invreg
all.equal(inveigen, invreg)

# Maximum Sharpe weights in-sample interval
retis <- retp["/2014"]
raterf <- 0.03/252
retx <- (retis - raterf)
invreg <- MASS::ginv(cov(retis, use="pairwise.complete.obs"))
weightv <- drop(invreg %*% colMeans(retx, na.rm=TRUE))
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
colnames(wealthv) <- c("EqualWeight", "Max Sharpe", "Combined")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Dygraph cumulative wealth
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="In-Sample Optimal Portfolio Returns") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", strokeWidth=2) %>%
  dyLegend(width=300)

# Calculate the equal weight index
retos <- retp["2015/"]
# Calculate out-of-sample portfolio returns
pnlos <- HighFreq::mult_mat(weightv, retos)
pnlos <- rowMeans(pnlos, na.rm=TRUE)
pnlos <- pnlos*sd(retew["2015/"])/sd(pnlos)
wealthv <- cbind(retew["2015/"], pnlos, (pnlos + retew["2015/"])/2)
colnames(wealthv) <- c("EqualWeight", "Max Sharpe", "Combined")
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))

# Dygraph cumulative wealth
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Out-of-Sample Optimal Portfolio Returns") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", strokeWidth=2) %>%
  dyLegend(width=300)

# Maximum Sharpe weights in-sample interval
invreg <- MASS::ginv(cov(retis, use="pairwise.complete.obs"))
weightv <- invreg %*% colMeans(retx, na.rm=TRUE)
names(weightv) <- colnames(retp)
# Calculate cumulative wealth
pnls <- HighFreq::mult_mat(weightv, retp)
pnls <- rowMeans(pnls, na.rm=TRUE)
pnls <- pnls*sd(retew)/sd(pnls)
wealthv <- cbind(retew, pnls, (pnls + retew)/2)
colnames(wealthv) <- c("EqualWeight", "Optimal", "Combined")
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))

# Dygraph cumulative wealth
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Out-of-Sample Optimal Portfolio Returns for ETFs") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", strokeWidth=2) %>%
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
all.equal(covmat %*% invred %*% covmat, covmat)

# Calculate portfolio weights
weightv <- drop(invred %*% colMeans(retis, na.rm=TRUE))
names(weightv) <- colnames(retp)
# Calculate cumulative wealth
pnls <- HighFreq::mult_mat(weightv, retp)
pnls <- rowMeans(pnls, na.rm=TRUE)
pnls <- pnls*sd(retew)/sd(pnls)
wealthv <- cbind(retew, pnls, (pnls + retew)/2)
colnames(wealthv) <- c("EqualWeight", "DimReduction", "Combined")
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))

# Dygraph cumulative wealth
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Optimal Portfolio Returns With Dimension Reduction") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", strokeWidth=2) %>%
  dyEvent(zoo::index(last(retis[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=400)

# Shrink the in-sample returns to their mean
alphac <- 0.7
retxm <- rowMeans(retx, na.rm=TRUE)
retxis <- (1-alphac)*retx + alphac*retxm
# Calculate portfolio weights
weightv <- invred %*% colMeans(retxis, na.rm=TRUE)
# Calculate cumulative wealth
pnls <- HighFreq::mult_mat(weightv, retp)
pnls <- rowMeans(pnls, na.rm=TRUE)
pnls <- pnls*sd(retew)/sd(pnls)
wealthv <- cbind(retew, pnls, (pnls + retew)/2)
colnames(wealthv) <- c("EqualWeight", "Optimal", "Combined")

# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Dygraph cumulative wealth
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Optimal Portfolio With Dimension Reduction and Return Shrinkage") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", strokeWidth=2) %>%
  dyEvent(zoo::index(last(retis[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=300)

# Define monthly end and start points
retx <- (retp - raterf)
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
colnames(wealthv) <- c("EqualWeight", "PortfStrat", "Combined")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))

# Dygraph cumulative wealth
dygraphs::dygraph(cumsum(wealthv)[endw], main="Monthly ETF Rolling Portfolio Strategy") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", strokeWidth=2) %>%
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
colnames(wealthv) <- c("EqualWeight", "PortfStrat", "Combined")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Dygraph cumulative wealth
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Rolling Portfolio Strategy With Dimension Reduction") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", strokeWidth=2) %>%
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
colnames(wealthv) <- c("EqualWeight", "PortfStrat", "Combined")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Dygraph cumulative wealth
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Rolling Portfolio Strategy With Return Shrinkage") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", strokeWidth=2) %>%
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
lookbv <- seq(6, 13, by=1)
library(parallel)  # Load package parallel
ncores <- detectCores() - 1
pnls <- mclapply(lookbv, roll_portf, retp=retp, retx=retx,
  endd=endd, dimax=dimax, mc.cores=ncores)
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("lookb=", lookbv)
profilev <- sapply(pnls, sum)
lookb <- lookbv[which.max(profilev)]

# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(pnls, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of monthly ETF portfolio strategies
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls)[endw], main="Rolling Portfolio Strategies") %>%
  dyOptions(colors=colorv, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Plot portfolio strategies using quantmod
themev <- chart_theme()
themev$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(pnls))
quantmod::chart_Series(cumsum(pnls),
  theme=themev, name="Rolling Portfolio Strategies")
legend("bottomleft", legend=colnames(pnls),
  inset=0.02, bg="white", cex=0.7, lwd=rep(6, NCOL(retp)),
  col=themev$col$line.col, bty="n")

# Perform backtest for different dimax values
dimv <- 3:9
pnls <- mclapply(dimv, roll_portf, retp=retp, retx=retx,
  endd=endd, lookb=lookb, mc.cores=ncores)
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("dim=", dimv)
profilev <- sapply(pnls, sum)
dimax <- dimv[which.max(profilev)]

# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(pnls, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of monthly ETF portfolio strategies
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls)[endw],
  main="Rolling Portfolio Strategies With Dimension Reduction") %>%
  dyOptions(colors=colorv, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Plot portfolio strategies using quantmod
themev <- chart_theme()
themev$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(pnls))
quantmod::chart_Series(cumsum(pnls),
  theme=themev, name="Rolling Portfolio Strategies")
legend("bottomleft", legend=colnames(pnls),
  inset=0.02, bg="white", cex=0.7, lwd=rep(6, NCOL(retp)),
  col=themev$col$line.col, bty="n")

# Load stock returns
load("/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
datev <- zoo::index(na.omit(retstock$GOOGL))
retp <- retstock[datev] # Subset the returns to GOOGL
# Remove the stocks with any NA values
numna <- sapply(retp, function(x) sum(is.na(x)))
retp <- retp[, numna==0]
nstocks <- NCOL(retp)
datev <- zoo::index(retp)
symbolv <- colnames(retp)
retis <- retp["/2014"] # In-sample returns
raterf <- 0.03/252
retx <- (retis - raterf) # Excess returns
# Maximum Sharpe weights in-sample interval
colmeanv <- colMeans(retx, na.rm=TRUE)
covmat <- cov(retis, use="pairwise.complete.obs")
invreg <- MASS::ginv(covmat)
weightv <- drop(invreg %*% colmeanv)
names(weightv) <- symbolv
head(sort(weightv))
tail(sort(weightv))
# Calculate the portfolio returns
pnls <- HighFreq::mult_mat(weightv, retp)
pnls <- rowMeans(pnls, na.rm=TRUE)
pnls <- xts::xts(pnls, datev)
retew <- xts::xts(rowMeans(retp, na.rm=TRUE), datev)
pnls <- pnls*sd(retew["/2014"])/sd(pnls["/2014"])
wealthv <- cbind(retew, pnls)
colnames(wealthv) <- c("EqualWeight", "MaxSharpe")

# Calculate the in-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv["/2014"], function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv["2015/"], function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of cumulative portfolio returns
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Out-of-Sample Maximum Sharpe Stock Portfolio") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyEvent(zoo::index(last(retis[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=300)

# Calculate reduced inverse of covariance matrix
dimax <- 10
eigend <- eigen(covmat)
eigenvec <- eigend$vectors
eigenval <- eigend$values
invred <- eigenvec[, 1:dimax] %*%
  (t(eigenvec[, 1:dimax]) / eigenval[1:dimax])
# Calculate portfolio weights and PnLs
colmeanv <- colMeans(retx["/2014"], na.rm=TRUE)
weightv <- invred %*% colmeanv
pnls <- HighFreq::mult_mat(weightv, retp)
pnls <- rowMeans(pnls, na.rm=TRUE)
pnls <- xts::xts(pnls, datev)
pnls <- pnls*sd(retew["/2014"])/sd(pnls["/2014"])

# Combine with equal weight
wealthv <- cbind(retew, pnls)
colnames(wealthv) <- c("EqualWeight", "MaxSharpe")
# Calculate the in-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv["/2014"], function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv["2015/"], function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of cumulative portfolio returns
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Maximum Sharpe With Dimension Reduction") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyEvent(zoo::index(last(retis[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=300)

# Perform loop over vector of dimension reduction parameters
dimv <- seq(from=5, to=11)
pnls <- mclapply(dimv, function(dimax) {
  invred <- eigenvec[, 1:dimax] %*%
    (t(eigenvec[, 1:dimax]) / eigenval[1:dimax])
  # Calculate portfolio weights and PnLs
  weightv <- invred %*% colmeanv
  pnls <- HighFreq::mult_mat(weightv, retp)
  pnls <- rowMeans(pnls, na.rm=TRUE)
  pnls <- xts::xts(pnls, datev)
  pnls*sd(retew["/2014"])/sd(pnls["/2014"])
}, mc.cores=ncores)  # end mclapply
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("dim=", dimv)
profilev <- sapply(pnls["2015/"], sum)
dimax <- dimv[which.max(profilev)]

# Plot of cumulative portfolio returns
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
endw <- rutils::calc_endpoints(pnls["2015/"], interval="weeks")
dygraphs::dygraph(cumsum(pnls["2015/"])[endw],
  main="Out-of-Sample PnLs Wih Dimension Reduction") %>%
  dyOptions(colors=colorv, strokeWidth=1) %>%
  dyLegend(show="always", width=300)

# Shrink the in-sample returns to their mean
alphac <- 0.3
retxm <- rowMeans(retx, na.rm=TRUE)
retxis <- (1-alphac)*retx + alphac*retxm
# Calculate portfolio weights and PnLs
weightv <- drop(invred %*% colMeans(retxis, na.rm=TRUE))
names(weightv) <- symbolv
pnls <- HighFreq::mult_mat(weightv, retp)
pnls <- rowMeans(pnls, na.rm=TRUE)
pnls <- xts::xts(pnls, datev)
pnls <- pnls*sd(retew["/2014"])/sd(pnls["/2014"])

# Combine with equal weight
wealthv <- cbind(retew, pnls)
colnames(wealthv) <- c("EqualWeight", "MaxSharpe")
# Calculate the in-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv["/2014"], function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv["2015/"], function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of cumulative portfolio returns
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Maximum Sharpe With Return Shrinkage") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyEvent(zoo::index(last(retis[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=300)

# Perform loop over vector of shrinkage intensities
alphav <- seq(from=0.5, to=0.9, by=0.1)
pnls <- mclapply(alphav, function(alphac) {
  retxis <- (1-alphac)*retx + alphac*retxm
  weightv <- drop(invred %*% colMeans(retxis, na.rm=TRUE))
  pnls <- HighFreq::mult_mat(weightv, retp)
  pnls <- rowMeans(pnls, na.rm=TRUE)
  pnls <- xts::xts(pnls, datev)
  pnls*sd(retew["/2014"])/sd(pnls["/2014"])
}, mc.cores=ncores)  # end mclapply
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("alpha=", alphav)
profilev <- sapply(pnls["2015/"], sum)
alphac <- alphav[which.max(profilev)]

# Plot of cumulative portfolio returns
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
endw <- rutils::calc_endpoints(pnls["2015/"], interval="weeks")
dygraphs::dygraph(cumsum(pnls["2015/"])[endw],
  main="Out-of-Sample PnLs Wih Shrinkage") %>%
  dyOptions(colors=colorv, strokeWidth=1) %>%
  dyLegend(show="always", width=300)

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
    retis <- retp[startp[tday]:endd[tday], ]
    retis[is.na(retis)] <- 0
    invreg <- MASS::ginv(cov(retis, use="pairwise.complete.obs"))
    # Calculate the maximum Sharpe ratio portfolio weights
    retm <- colMeans(retis, na.rm=TRUE)
    weightv <- invreg %*% retm
    # Zero weights if sparse data
    zerov <- sapply(retis, function(x) (sum(x == 0) > 5))
    weightv[zerov] <- 0
    # Calculate in-sample portfolio returns
    pnlis <- (retis %*% weightv)
    # Scale the weights to the in-sample volatility
    weightv <- weightv*sd(retm)/sd(pnlis)
    # Calculate the out-of-sample portfolio returns
    retos <- retp[(endd[tday]+1):endd[tday+1], ]
    pnls <- HighFreq::mult_mat(weightv, retos)
    pnls <- rowMeans(pnls, na.rm=TRUE)
    xts::xts(pnls, zoo::index(retos))
}, mc.cores=ncores)  # end mclapply
pnls <- rutils::do_call(rbind, pnls)
pnls <- rbind(retew[paste0("/", start(pnls)-1)], pnls*sd(retew)/sd(pnls))

# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retew, pnls)
colnames(wealthv) <- c("EqualWeight", "MaxSharpe")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))

# Plot cumulative strategy returns
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Rolling Maximum Sharpe Portfolio Strategy") %>%
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

# Shift the end points to C++ convention
endd <- (endd - 1)
endd[endd < 0] <- 0
startp <- (startp - 1)
startp[startp < 0] <- 0
# Specify dimension reduction and return shrinkage using list of portfolio optimization parameters
dimax <- 9
alphac <- 0.8
controll <- HighFreq::param_portf(method="maxsharpe",
  dimax=dimax, alpha=alphac)
# Perform backtest in Rcpp - takes very long!!!
retx <- (retp - raterf)
pnls <- HighFreq::roll_portf(retx=retx, retp=retp,
  startp=startp, endd=endd, controll=controll)
pnls <- pnls*sd(retew)/sd(pnls)

# Calculate the out-of-sample Sharpe and Sortino ratios
wealthv <- cbind(retew, pnls, (pnls + retew)/2)
colnames(wealthv) <- c("EqualWeight", "MaxSharpe", "Combined")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot cumulative strategy returns
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Rolling S&P500 Portfolio Strategy With Shrinkage") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", label="Combined", strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Perform backtest over vector of dimension reduction parameters
dimv <- seq(from=3, to=21, by=2)
pnls <- mclapply(dimv, function(dimax) {
  controll <- HighFreq::param_portf(method="maxsharpe",
    dimax=dimax, alpha=alphac)
  HighFreq::roll_portf(retx=retx, retp=retp,
    startp=startp, endd=endd, controll=controll)
}, mc.cores=ncores)  # end mclapply
profilev <- sapply(pnls, sum)
dimax <- dimv[which.max(profilev)]

# Plot of rolling strategy PnL as a function of dimax
plot(x=dimv, y=profilev, t="l", xlab="dimax", ylab="pnl",
  main="Rolling Strategy PnL as Function of dimax")

# Perform backtest over vector of shrinkage intensities
alphav <- seq(from=0.5, to=0.9, by=0.1)
pnls <- mclapply(alphav, function(alphac) {
  controll <- HighFreq::param_portf(method="maxsharpe",
      dimax=dimax, alpha=alphac)
  HighFreq::roll_portf(retx=retx, retp=retp,
      startp=startp, endd=endd, controll=controll)
}, mc.cores=ncores)  # end mclapply
profilev <- sapply(pnls, sum)
alphac <- alphav[which.max(profilev)]

# Plot of rolling strategy PnL as a function of shrinkage intensity
plot(x=alphav, y=profilev, t="l",
  main="Rolling Strategy PnL as Function of Return Shrinkage",
  xlab="Shrinkage Intensity Alpha", ylab="pnl")

# Create list of model parameters
controll <- HighFreq::param_portf(method="maxsharpe",
      dimax=dimax, alpha=alphac)
# Perform backtest over look-backs
lookbv <- seq(from=5, to=16, by=1)
pnls <- mclapply(lookbv, function(lookb) {
  startp <- c(rep_len(0, lookb), endd[1:(npts-lookb)])
  startp <- (startp - 1) ; startp[startp < 0] <- 0
  HighFreq::roll_portf(retx=retx, retp=retp,
    startp=startp, endd=endd, controll=controll)
}, mc.cores=ncores)  # end mclapply
profilev <- sapply(pnls, sum)
lookb <- lookbv[which.max(profilev)]

# Plot of rolling strategy PnL as a function of look-back interval
plot(x=lookbv, y=profilev, t="l", main="MaxSharpe PnL as Function of Look-back Interval",
  xlab="Look-back Interval", ylab="pnl")

# Calculate the out-of-sample Sharpe and Sortino ratios
pnls <- pnls[[whichmax]]
pnls <- pnls*sd(retew)/sd(pnls)
wealthv <- cbind(retew, pnls, (pnls + retew)/2)
colnames(wealthv) <- c("EqualWeight", "MaxSharpe", "Combined")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Dygraph the cumulative wealth
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Optimal Maximum Sharpe Portfolio Strategy") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", label="Combined", strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Verify that Rtools or XCode are working properly:
devtools::find_rtools()  # Under Windows
devtools::has_devel()
# Install the packages Rcpp and RcppArmadillo
install.packages(c("Rcpp", "RcppArmadillo"))
# Load package Rcpp
library(Rcpp)
# Get documentation for package Rcpp
# Get short description
packageDescription("Rcpp")
# Load help page
help(package="Rcpp")
# List all datasets in "Rcpp"
data(package="Rcpp")
# List all objects in "Rcpp"
ls("package:Rcpp")
# Remove Rcpp from search path
detach("package:Rcpp")

# Define Rcpp function
Rcpp::cppFunction("
  int times_two(int x)
    { return 2 * x;}
  ")  # end cppFunction
# Run Rcpp function
times_two(3)
# Source Rcpp functions from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/mult_rcpp.cpp")
# Multiply two numbers
mult_rcpp(2, 3)
mult_rcpp(1:3, 6:4)
# Multiply two vectors
mult_vec_rcpp(2, 3)
mult_vec_rcpp(1:3, 6:4)

# Define Rcpp function with loop
Rcpp::cppFunction("
double inner_mult(NumericVector x, NumericVector y) {
int xsize = x.size();
int ysize = y.size();
if (xsize != ysize) {
    return 0;
  } else {
    double total = 0;
    for(int i = 0; i < xsize; ++i) {
total += x[i] * y[i];
  }
  return total;
  }
}")  # end cppFunction
# Run Rcpp function
inner_mult(1:3, 6:4)
inner_mult(1:3, 6:3)
# Define Rcpp Sugar function with loop
Rcpp::cppFunction("
double inner_sugar(NumericVector x, NumericVector y) {
  return sum(x * y);
}")  # end cppFunction
# Run Rcpp Sugar function
inner_sugar(1:3, 6:4)
inner_sugar(1:3, 6:3)

# Define R function with loop
inner_multr <- function(x, y) {
    sumv <- 0
    for(i in 1:NROW(x)) {
sumv <- sumv + x[i] * y[i]
    }
    sumv
}  # end inner_multr
# Run R function
inner_multr(1:3, 6:4)
inner_multr(1:3, 6:3)
# Compare speed of Rcpp and R
library(microbenchmark)
summary(microbenchmark(
  rcode=inner_multr(1:10000, 1:10000),
  innerp=1:10000 %*% 1:10000,
  Rcpp=inner_mult(1:10000, 1:10000),
  sugar=inner_sugar(1:10000, 1:10000),
  times=10))[, c(1, 4, 5)]

# Define Ornstein-Uhlenbeck function in R
sim_our <- function(nrows=1000, priceq=5.0,
              volat=0.01, theta=0.01) {
  retp <- numeric(nrows)
  pricev <- numeric(nrows)
  pricev[1] <- priceq
  for (i in 2:nrows) {
    retp[i] <- theta*(priceq - pricev[i-1]) + volat*rnorm(1)
    pricev[i] <- pricev[i-1] + retp[i]
  }  # end for
  pricev
}  # end sim_our
# Simulate Ornstein-Uhlenbeck process in R
priceq <- 5.0; sigmav <- 0.01
thetav <- 0.01; nrows <- 1000
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")  # Reset random numbers
ousim <- sim_our(nrows, priceq=priceq, volat=sigmav, theta=thetav)

# Define Ornstein-Uhlenbeck function in Rcpp
Rcpp::cppFunction("
NumericVector sim_oucpp(double priceq,
                  double volat,
                  double thetav,
                  NumericVector innov) {
  int nrows = innov.size();
  NumericVector pricev(nrows);
  NumericVector retv(nrows);
  pricev[0] = priceq;
  for (int it = 1; it < nrows; it++) {
    retv[it] = thetav*(priceq - pricev[it-1]) + volat*innov[it-1];
    pricev[it] = pricev[it-1] + retv[it];
  }  // end for
  return pricev;
}")  # end cppFunction
# Simulate Ornstein-Uhlenbeck process in Rcpp
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")  # Reset random numbers
oucpp <- sim_oucpp(priceq=priceq,
  volat=sigmav, theta=thetav, innov=rnorm(nrows))
all.equal(ousim, oucpp)
# Compare speed of Rcpp and R
library(microbenchmark)
summary(microbenchmark(
  rcode=sim_our(nrows, priceq=priceq, volat=sigmav, theta=thetav),
  Rcpp=sim_oucpp(priceq=priceq, volat=sigmav, theta=thetav, innov=rnorm(nrows)),
  times=10))[, c(1, 4, 5)]

# Source Rcpp function for Ornstein-Uhlenbeck process from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/sim_ou.cpp")
# Simulate Ornstein-Uhlenbeck process in Rcpp
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")  # Reset random numbers
oucpp <- sim_oucpp(priceq=priceq,
  volat=sigmav,
  theta=thetav,
  innov=rnorm(nrows))
all.equal(ousim, oucpp)
# Compare speed of Rcpp and R
library(microbenchmark)
summary(microbenchmark(
  rcode=sim_our(nrows, priceq=priceq, volat=sigmav, theta=thetav),
  Rcpp=sim_oucpp(priceq=priceq, volat=sigmav, theta=thetav, innov=rnorm(nrows)),
  times=10))[, c(1, 4, 5)]

# Calculate uniformly distributed pseudo-random sequence
unifun <- function(seedv, nrows=10) {
  datav <- numeric(nrows)
  datav[1] <- seedv
  for (i in 2:nrows) {
    datav[i] <- 4*datav[i-1]*(1-datav[i-1])
  }  # end for
  acos(1-2*datav)/pi
}  # end unifun

# Source Rcpp functions from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/unifun.cpp")
# Microbenchmark Rcpp code
library(microbenchmark)
summary(microbenchmark(
  rcode=runif(1e5),
  rloop=unifun(0.3, 1e5),
  Rcpp=unifuncpp(0.3, 1e5),
  times=10))[, c(1, 4, 5)]

library(RcppArmadillo)
# Source Rcpp functions from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/armadillo_functions.cpp")
vec1 <- runif(1e5)
vec2 <- runif(1e5)
inner_vec(vec1, vec2)
vec1 %*% vec2

# Microbenchmark \emph{RcppArmadillo} code
summary(microbenchmark(
  rcpp = inner_vec(vec1, vec2),
  rcode = (vec1 %*% vec2),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
# Microbenchmark shows:
# inner_vec() is several times faster than %*%, especially for longer vectors.
#     expr     mean   median
# 1 inner_vec 110.7067 110.4530
# 2 rcode 585.5127 591.3575

# Source Rcpp functions from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/sim_arima.cpp")
# Define AR(2) coefficients
coeff <- c(0.9, 0.09)
nrows <- 1e4
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
innov <- rnorm(nrows)
# Simulate ARIMA using filter()
arimar <- filter(x=innov, filter=coeff, method="recursive")
# Simulate ARIMA using sim_ar()
innov <- matrix(innov)
coeff <- matrix(coeff)
arimav <- sim_ar(coeff, innov)
all.equal(drop(arimav), as.numeric(arimar))
# Microbenchmark \emph{RcppArmadillo} code
summary(microbenchmark(
  rcpp = sim_ar(coeff, innov),
  filter = filter(x=innov, filter=coeff, method="recursive"),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary

library(RcppArmadillo)
# Source Rcpp functions from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/armadillo_functions.cpp")
matv <- matrix(runif(1e5), nc=1e3)
# Center matrix columns using apply()
matd <- apply(matv, 2, function(x) (x-mean(x)))
# Center matrix columns in place using Rcpp demeanr()
demeanr(matv)
all.equal(matd, matv)
# Microbenchmark \emph{RcppArmadillo} code
library(microbenchmark)
summary(microbenchmark(
  rcode = (apply(matv, 2, mean)),
  rcpp = demeanr(matv),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
# Perform matrix inversion
# Create random positive semi-definite matrix
matv <- matrix(runif(25), nc=5)
matv <- t(matv) %*% matv
# Invert the matrix
matrixinv <- solve(matv)
inv_mat(matv)
all.equal(matrixinv, matv)
# Microbenchmark \emph{RcppArmadillo} code
summary(microbenchmark(
  rcode = solve(matv),
  rcpp = inv_mat(matv),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary

library(RcppArmadillo)
# Source Rcpp functions from file
Rcpp::sourceCpp("/Users/jerzy/Develop/lecture_slides/scripts/HighFreq.cpp")
# Calculate matrix of random returns
matv <- matrix(rnorm(300), nc=5)
# Reduced inverse of correlation matrix
dimax <- 4
cormat <- cor(matv)
eigend <- eigen(cormat)
invmat <- eigend$vectors[, 1:dimax] %*%
  (t(eigend$vectors[, 1:dimax]) / eigend$values[1:dimax])
# Reduced inverse using \emph{RcppArmadillo}
invarma <- calc_inv(cormat, dimax=dimax)
all.equal(invmat, invarma)
# Microbenchmark \emph{RcppArmadillo} code
library(microbenchmark)
summary(microbenchmark(
  rcode = {eigend <- eigen(cormat)
eigend$vectors[, 1:dimax] %*% (t(eigend$vectors[, 1:dimax]) / eigend$values[1:dimax])},
  rcpp = calc_inv(cormat, dimax=dimax),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary

# Install package reticulate
install.packages("reticulate")
# Start Python session
reticulate::repl_python()
# Exit Python session
exit
