# Select all the ETF symbols except "VXX", "SVXY" "MTUM", "QUAL", "VLUE", "USMV", and "AIEQ"
symbolv <- colnames(rutils::etfenv$returns)
# VYM has bad data in 2006
symbolv <- symbolv[!(symbolv %in% c("VXX", "SVXY", "MTUM", "QUAL", "VLUE", "USMV", "AIEQ", "VYM"))]
# Extract columns of rutils::etfenv$returns and overwrite NA values
retp <- rutils::etfenv$returns[, symbolv]
retp[1, ] <- 0.01
nstocks <- NCOL(retp)
datev <- zoo::index(retp)
# Calculate the covariance ignoring NA values
covmat <- cov(retp, use="pairwise.complete.obs")
sum(is.na(covmat))
# Calculate the inverse of covmat
invmat <- solve(covmat)
# Calculate the generalized inverse of covmat
invreg <- MASS::ginv(covmat)
all.equal(unname(invmat), invreg)
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
# Calculate generalized inverse from the eigen decomposition
notzero <- (eigenval > (precv*eigenval[1]))
inveigen <- eigenvec[, notzero] %*%
  (t(eigenvec[, notzero]) / eigenval[notzero])
# Verify inverse property of invreg
all.equal(covmat, inveigen %*% covmat)
# Verify generalized inverse property of invreg
all.equal(covmat, covmat %*% inveigen %*% covmat)
# Calculate generalized inverse of covmat
invreg <- MASS::ginv(covmat)
# Verify that inveigen is the same as invreg
all.equal(inveigen, invreg)
# Returns in excess of risk-free rate
riskf <- 0.03/252
retx <- (retp - riskf)
# Maximum Sharpe weights in-sample interval
retis <- retp["/2014"]
invreg <- MASS::ginv(cov(retis, use="pairwise.complete.obs"))
weightv <- invreg %*% colMeans(retx["/2014"], na.rm=TRUE)
weightv <- drop(weightv/sqrt(sum(weightv^2)))
names(weightv) <- colnames(retp)
# Plot portfolio weights
barplot(sort(weightv), main="Maximum Sharpe Weights", cex.names=0.7)
# Calculate the equal weight index
indeks <- xts::xts(rowMeans(retis, na.rm=TRUE), zoo::index(retis))
# Calculate the in-sample weighted returns using transpose
pnlis <- unname(t(t(retis)*weightv))
# Or using Rcpp
# pnlis <- HighFreq::mult_mat(weightv, retis)
pnlis <- rowMeans(pnlis, na.rm=TRUE)
pnlis <- pnlis*sd(indeks)/sd(pnlis)
# Dygraph cumulative wealth
wealthv <- cbind(indeks, pnlis, (pnlis + indeks)/2)
colnames(wealthv) <- c("Equal Weight", "Optimal", "Combined")
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Dygraph cumulative wealth
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="In-Sample Optimal Portfolio Returns") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", label="Combined", strokeWidth=3) %>%
  dyLegend(width=300)
# Calculate the equal weight index
retos <- retp["2015/"]
indeks <- xts::xts(rowMeans(retos, na.rm=TRUE), zoo::index(retos))
# Calculate out-of-sample portfolio returns
pnlos <- HighFreq::mult_mat(weightv, retos)
pnlos <- rowMeans(pnlos, na.rm=TRUE)
pnlos <- pnlos*sd(indeks)/sd(pnlos)
wealthv <- cbind(indeks, pnlos, (pnlos + indeks)/2)
colnames(wealthv) <- c("Equal Weight", "Optimal", "Combined")
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Dygraph cumulative wealth
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Out-of-Sample Optimal Portfolio Returns") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", label="Combined", strokeWidth=3) %>%
  dyLegend(width=300)
# Maximum Sharpe weights in-sample interval
invreg <- MASS::ginv(cov(retis, use="pairwise.complete.obs"))
weightv <- invreg %*% colMeans(retx["/2014"], na.rm=TRUE)
weightv <- drop(weightv/sqrt(sum(weightv^2)))
names(weightv) <- colnames(retp)
# Calculate in-sample portfolio returns
pnlis <- HighFreq::mult_mat(weightv, retis)
pnlis <- rowMeans(pnlis, na.rm=TRUE)
# Calculate out-of-sample portfolio returns
pnlos <- HighFreq::mult_mat(weightv, retos)
pnlos <- rowMeans(pnlos, na.rm=TRUE)
# Calculate cumulative wealth
pnls <- c(pnlis, pnlos)
pnls <- pnls*sd(indeks)/sd(pnls)
indeks <- xts::xts(rowMeans(retp, na.rm=TRUE), datev)
wealthv <- cbind(indeks, pnls, (pnls + indeks)/2)
colnames(wealthv) <- c("Equal Weight", "Optimal", "Combined")
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Dygraph cumulative wealth
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Out-of-Sample Optimal Portfolio Returns for ETFs") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", label="Combined", strokeWidth=3) %>%
  dyEvent(zoo::index(last(retis[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=300)
# Calculate in-sample covariance matrix
covmat <- cov(retis, use="pairwise.complete.obs")
eigend <- eigen(covmat)
eigenvec <- eigend$vectors
eigenval <- eigend$values
# Negative eigenvalues
eigenval
# Calculate reduced inverse of covariance matrix
dimax <- 11
invred <- eigenvec[, 1:dimax] %*%
  (t(eigenvec[, 1:dimax]) / eigenval[1:dimax])
# Verify inverse property of inverse
all.equal(covmat, covmat %*% invred %*% covmat)
# Calculate portfolio weights
weightv <- invred %*% colMeans(retis, na.rm=TRUE)
weightv <- drop(weightv/sqrt(sum(weightv^2)))
names(weightv) <- colnames(retp)
# Calculate portfolio returns
pnlis <- HighFreq::mult_mat(weightv, retis)
pnlis <- rowMeans(pnlis, na.rm=TRUE)
pnlos <- HighFreq::mult_mat(weightv, retos)
pnlos <- rowMeans(pnlos, na.rm=TRUE)
pnls <- c(pnlis, pnlos)
pnls <- pnls*sd(indeks)/sd(pnls)
wealthv <- cbind(indeks, pnls, (pnls + indeks)/2)
colnames(wealthv) <- c("Equal Weight", "DimReduction", "Combined")
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Dygraph cumulative wealth
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Optimal Portfolio Returns With Dimension Reduction") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", label="Combined", strokeWidth=3) %>%
  dyEvent(zoo::index(last(retis[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=300)
# Shrink the in-sample returns to their mean
alpha <- 0.7
retxm <- rowMeans(retx["/2014"], na.rm=TRUE)
retxis <- (1-alpha)*retx["/2014"] + alpha*retxm
# Calculate portfolio weights
weightv <- invred %*% colMeans(retxis, na.rm=TRUE)
weightv <- drop(weightv/sqrt(sum(weightv^2)))
# Calculate portfolio returns
pnlis <- HighFreq::mult_mat(weightv, retis)
pnlis <- rowMeans(pnlis, na.rm=TRUE)
pnlos <- HighFreq::mult_mat(weightv, retos)
pnlos <- rowMeans(pnlos, na.rm=TRUE)
pnls <- c(pnlis, pnlos)
pnls <- pnls*sd(indeks)/sd(pnls)
wealthv <- cbind(indeks, pnls, (pnls + indeks)/2)
colnames(wealthv) <- c("Equal Weight", "Optimal", "Combined")
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Dygraph cumulative wealth
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Optimal Portfolio With Dimension Reduction and Return Shrinkage") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", label="Combined", strokeWidth=3) %>%
  dyEvent(zoo::index(last(retis[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=300)
# Define monthly end points
endd <- rutils::calc_endpoints(retp, interval="months")
endd <- endd[endd > (nstocks+1)]
npts <- NROW(endd)
look_back <- 3
startp <- c(rep_len(0, look_back), endd[1:(npts-look_back)])
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
    weightv <- drop(weightv/sqrt(sum(weightv^2)))
    # Calculate the out-of-sample portfolio returns
    retos <- retp[(endd[tday]+1):endd[tday+1], ]
    pnlos <- HighFreq::mult_mat(weightv, retos)
    pnlos <- rowMeans(pnlos, na.rm=TRUE)
    pnlos <- pnlos*0.01/sd(pnlos)
    xts::xts(pnlos, zoo::index(retos))
})  # end lapply
pnls <- do.call(rbind, pnls)
pnls <- rbind(indeks[paste0("/", start(pnls)-1)], pnls)
# Calculate the Sharpe and Sortino ratios
pnls <- pnls*sd(indeks)/sd(pnls)
wealthv <- cbind(indeks, pnls, (pnls+indeks)/2)
colnames(wealthv) <- c("Index", "PortfStrat", "Combined")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Dygraph cumulative wealth
dygraphs::dygraph(cumsum(wealthv)[endd], main="Monthly ETF Rolling Portfolio Strategy") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", label="Combined", strokeWidth=3) %>%
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
    invred <- eigenvec[, 1:dimax] %*% (t(eigenvec[, 1:dimax]) / eigenval[1:dimax])
    colm <- colMeans(retis, na.rm=TRUE)
    colm[is.na(colm)] <- 0
    weightv <- invred %*% colm
    weightv <- drop(weightv/sqrt(sum(weightv^2)))
    # Calculate the out-of-sample portfolio returns
    retos <- retp[(endd[tday]+1):endd[tday+1], ]
    pnlos <- HighFreq::mult_mat(weightv, retos)
    pnlos <- rowMeans(pnlos, na.rm=TRUE)
    pnlos <- pnlos*0.01/sd(pnlos)
    xts::xts(pnlos, zoo::index(retos))
})  # end lapply
pnls <- do.call(rbind, pnls)
pnls <- rbind(indeks[paste0("/", start(pnls)-1)], pnls)
# Calculate the Sharpe and Sortino ratios
pnls <- pnls*sd(indeks)/sd(pnls)
wealthv <- cbind(indeks, pnls, (pnls+indeks)/2)
colnames(wealthv) <- c("Index", "PortfStrat", "Combined")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Dygraph cumulative wealth
dygraphs::dygraph(cumsum(wealthv)[endd], main="Rolling Portfolio Strategy With Dimension Reduction") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", label="Combined", strokeWidth=3) %>%
  dyLegend(show="always", width=300)
alpha <- 0.7 # Return shrinkage intensity
# Perform loop over end points
pnls <- lapply(1:(npts-1), function(tday) {
    # Shrink the in-sample returns to their mean
    retis <- retx[startp[tday]:endd[tday], ]
    rowm <- rowMeans(retis, na.rm=TRUE)
    rowm[is.na(rowm)] <- 0
    retis <- (1-alpha)*retis + alpha*rowm
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
    weightv <- drop(weightv/sqrt(sum(weightv^2)))
    # Calculate the in-sample portfolio returns
    pnlis <- HighFreq::mult_mat(weightv, retis)
    pnlis <- rowMeans(pnlis, na.rm=TRUE)
    # Calculate the out-of-sample portfolio returns
    retos <- retp[(endd[tday]+1):endd[tday+1], ]
    pnlos <- HighFreq::mult_mat(weightv, retos)
    pnlos <- rowMeans(pnlos, na.rm=TRUE)
    pnlos <- pnlos*0.01/sd(pnlos)
    xts::xts(pnlos, zoo::index(retos))
})  # end lapply
pnls <- do.call(rbind, pnls)
pnls <- rbind(indeks[paste0("/", start(pnls)-1)], pnls)
# Calculate the Sharpe and Sortino ratios
pnls <- pnls*sd(indeks)/sd(pnls)
wealthv <- cbind(indeks, pnls, (pnls+indeks)/2)
colnames(wealthv) <- c("Index", "PortfStrat", "Combined")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Dygraph cumulative wealth
dygraphs::dygraph(cumsum(wealthv)[endd], main="Rolling Portfolio Strategy With Return Shrinkage") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", label="Combined", strokeWidth=3) %>%
  dyLegend(show="always", width=300)
# Define backtest functional for rolling portfolio strategy
roll_portf <- function(retx, # Excess returns
                 retp, # Stock returns
                 endd, # End points
                 look_back=12, # Look-back interval
                 dimax=3, # Dimension reduction parameter
                 alpha=0.0, # Return shrinkage intensity
                 bid_offer=0.0, # Bid-offer spread
                 ...) {
  npts <- NROW(endd)
  startp <- c(rep_len(0, look_back), endd[1:(npts-look_back)])
  pnls <- lapply(1:(npts-1), function(tday) {
    retis <- retx[startp[tday]:endd[tday], ]
    # Shrink the in-sample returns to their mean
    if (alpha > 0) {
rowm <- rowMeans(retis, na.rm=TRUE)
rowm[is.na(rowm)] <- 0
retis <- (1-alpha)*retis + alpha*rowm
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
    weightv <- drop(weightv/sqrt(sum(weightv^2)))
    # Calculate the out-of-sample portfolio returns
    retos <- retp[(endd[tday]+1):endd[tday+1], ]
    pnlos <- HighFreq::mult_mat(weightv, retos)
    pnlos <- rowMeans(pnlos, na.rm=TRUE)
    pnlos <- pnlos*0.01/sd(pnlos)
    xts::xts(pnlos, zoo::index(retos))
  })  # end lapply
  pnls <- do.call(rbind, pnls)
  # Add warmup period to pnls
  rbind(indeks[paste0("/", start(pnls)-1)], pnls)
}  # end roll_portf
# Simulate a monthly ETF portfolio strategy
pnls <- roll_portf(retx=retx, retp=retp, endd=endd,
  look_back=look_back, dimax=dimax)
# Perform sapply loop over look_backs
look_backs <- seq(2, 15, by=1)
pnls <- lapply(look_backs, roll_portf,
  retp=retp, retx=retx, endd=endd, dimax=dimax)
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("lookb=", look_backs)
pnlsums <- sapply(pnls, sum)
look_back <- look_backs[which.max(pnlsums)]
# Plot dygraph of monthly ETF portfolio strategies
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls)[endd], main="Rolling Portfolio Strategies") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=600)
# Plot EWMA strategies using quantmod
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(pnls))
quantmod::chart_Series(cumsum(pnls),
  theme=plot_theme, name="Rolling Portfolio Strategies")
legend("bottomleft", legend=colnames(pnls),
  inset=0.02, bg="white", cex=0.7, lwd=rep(6, NCOL(retp)),
  col=plot_theme$col$line.col, bty="n")
# Perform backtest for different dimax values
dimaxs <- 2:11
pnls <- lapply(dimaxs, roll_portf, retx=retx,
  retp=retp, endd=endd, look_back=look_back)
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("dimax=", dimaxs)
pnlsums <- sapply(pnls, sum)
dimax <- dimaxs[which.max(pnlsums)]
# Plot dygraph of monthly ETF portfolio strategies
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls)[endd],
  main="Rolling Portfolio Strategies With Dimension Reduction") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Plot EWMA strategies using quantmod
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(pnls))
quantmod::chart_Series(cumsum(pnls),
  theme=plot_theme, name="Rolling Portfolio Strategies")
legend("bottomleft", legend=colnames(pnls),
  inset=0.02, bg="white", cex=0.7, lwd=rep(6, NCOL(retp)),
  col=plot_theme$col$line.col, bty="n")
# Perform backtest over vector of return shrinkage intensities
alphav <- seq(from=0.0, to=0.9, by=0.1)
pnls <- lapply(alphav, roll_portf, retx=retx,
  retp=retp, endd=endd, look_back=look_back, dimax=dimax)
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("alpha=", alphav)
pnlsums <- sapply(pnls, sum)
alpha <- alphav[which.max(pnlsums)]
# Plot dygraph of monthly ETF portfolio strategies
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls)[endd],
  main="Rolling Portfolio Strategies With Return Shrinkage") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Plot EWMA strategies using quantmod
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
retp <- returns
nstocks <- NCOL(retp)
retp[is.na(retp)] <- 0
sum(is.na(retp))
datev <- zoo::index(retp)
riskf <- 0.03/252
retx <- (retp - riskf)
retis <- retp["/2010"]
retos <- retp["2011/"]
# Maximum Sharpe weights in-sample interval
covmat <- cov(retis, use="pairwise.complete.obs")
invreg <- MASS::ginv(covmat)
weightv <- invreg %*% colMeans(retx["/2010"], na.rm=TRUE)
weightv <- drop(weightv/sqrt(sum(weightv^2)))
names(weightv) <- colnames(retp)
# Calculate portfolio returns
pnlis <- (retis %*% weightv)
pnlos <- (retos %*% weightv)
indeks <- xts::xts(rowMeans(retp), datev)
# Combine in-sample and out-of-sample returns
pnls <- c(pnlis, pnlos)
pnls <- pnls*sd(indeks)/sd(pnls)
wealthv <- cbind(indeks, pnls)
colnames(wealthv) <- c("Equal Weight", "Optimal")
# Calculate the in-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv[index(retis)],
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv[index(retos)],
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of cumulative portfolio returns
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Out-of-Sample Optimal Portfolio Returns for Stocks") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyEvent(zoo::index(last(retis[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=300)
# Calculate reduced inverse of covariance matrix
dimax <- 11
eigend <- eigen(cov(retis, use="pairwise.complete.obs"))
eigenvec <- eigend$vectors
eigenval <- eigend$values
invred <- eigenvec[, 1:dimax] %*%
  (t(eigenvec[, 1:dimax]) / eigenval[1:dimax])
# Calculate portfolio weights
weightv <- invred %*% colMeans(retx["/2010"], na.rm=TRUE)
weightv <- drop(weightv/sqrt(sum(weightv^2)))
names(weightv) <- colnames(retp)
# Calculate portfolio returns
pnlis <- (retis %*% weightv)
pnlos <- (retos %*% weightv)
# Combine in-sample and out-of-sample returns
pnls <- c(pnlis, pnlos)
pnls <- pnls*sd(indeks)/sd(pnls)
wealthv <- cbind(indeks, pnls)
colnames(wealthv) <- c("Equal Weight", "Optimal")
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv[index(retos)],
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of cumulative portfolio returns
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Out-of-Sample Returns for Stocks with Dimension Reduction") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyEvent(zoo::index(last(retis[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=300)
# Shrink the in-sample returns to their mean
alpha <- 0.7
retxm <- rowMeans(retx["/2010"])
retxis <- (1-alpha)*retx["/2010"] + alpha*retxm
# Calculate portfolio weights
weightv <- invred %*% colMeans(retxis, na.rm=TRUE)
weightv <- drop(weightv/sqrt(sum(weightv^2)))
# Calculate portfolio returns
pnlis <- (retis %*% weightv)
pnlos <- (retos %*% weightv)
# Combine in-sample and out-of-sample returns
pnls <- c(pnlis, pnlos)
pnls <- pnls*sd(indeks)/sd(pnls)
wealthv <- cbind(indeks, pnls)
colnames(wealthv) <- c("Equal Weight", "Optimal")
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv[index(retos)],
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of cumulative portfolio returns
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Out-of-Sample Returns for Stocks with Return Shrinkage") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyEvent(zoo::index(last(retis[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=300)
library(RcppArmadillo)
# Source Rcpp functions from file
Rcpp::sourceCpp("/Users/jerzy/Develop/lecture_slides/scripts/back_test.cpp")
# Create random matrix of returns
matrixv <- matrix(rnorm(300), nc=5)
# Reduced inverse of covariance matrix
dimax <- 11
eigend <- eigen(covmat)
invred <- eigend$vectors[, 1:dimax] %*%
  (t(eigend$vectors[, 1:dimax]) / eigend$values[1:dimax])
# Reduced inverse using RcppArmadillo
invarma <- calc_inv(covmat, dimax)
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
# Overwrite NA values in returns
retp <- returns100
retp[is.na(retp)] <- 0
retx <- (retp - riskf)
nstocks <- NCOL(retp) ; datev <- zoo::index(retp)
# Define monthly end points
endd <- rutils::calc_endpoints(retp, interval="months")
endd <- endd[endd > (nstocks+1)]
npts <- NROW(endd) ; look_back <- 12
startp <- c(rep_len(0, look_back), endd[1:(npts-look_back)])
# Perform loop over end points - takes long
pnls <- lapply(1:(npts-1), function(tday) {
    # Subset the excess returns
    retis <- retx[startp[tday]:endd[tday], ]
    invreg <- MASS::ginv(cov(retis, use="pairwise.complete.obs"))
    # Calculate the maximum Sharpe ratio portfolio weights
    weightv <- invreg %*% colMeans(retis, na.rm=TRUE)
    weightv <- drop(weightv/sqrt(sum(weightv^2)))
    # Calculate the out-of-sample portfolio returns
    retos <- retp[(endd[tday]+1):endd[tday+1], ]
    xts::xts(retos %*% weightv, zoo::index(retos))
})  # end lapply
pnls <- rutils::do_call(rbind, pnls)
# Calculate returns of equal weight portfolio
indeks <- xts::xts(rowMeans(retp), datev)
pnls <- rbind(indeks[paste0("/", start(pnls)-1)], pnls*sd(indeks)/sd(pnls))
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(indeks, pnls)
colnames(wealthv) <- c("Equal Weight", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot cumulative strategy returns
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Rolling Portfolio Strategy for S&P500 Stocks") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Shift end points to C++ convention
endd <- (endd - 1)
endd[endd < 0] <- 0
startp <- (startp - 1)
startp[startp < 0] <- 0
# Specify dimension reduction and return shrinkage using list of portfolio optimization parameters
controlv <- HighFreq::param_portf(method="maxsharpe", dimax=dimax, alpha=0.7)
# Perform backtest in Rcpp
pnls <- HighFreq::back_test(retx=retx, retp=retp,
  startp=startp, endd=endd, controlv=controlv)
pnls <- pnls*sd(indeks)/sd(pnls)
# Plot cumulative strategy returns
wealthv <- cbind(indeks, pnls, (pnls+indeks)/2)
colnames(wealthv) <- c("Index", "PortfStrat", "Combined")
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Rolling S&P500 Portfolio Strategy With Shrinkage") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", label="Combined", strokeWidth=3) %>%
  dyLegend(show="always", width=300)
# Perform backtest over vector of return shrinkage intensities
alphav <- seq(from=0.01, to=0.91, by=0.1)
pnls <- lapply(alphav, function(alpha) {
  controlv <- HighFreq::param_portf(method="maxsharpe",
      dimax=dimax, alpha=alpha)
  HighFreq::back_test(retx=retx, retp=retp,
      startp=startp, endd=endd, controlv=controlv)
})  # end lapply
profilev <- sapply(pnls, sum)
plot(x=alphav, y=profilev, t="l",
  main="Rolling Strategy as Function of Return Shrinkage",
  xlab="Shrinkage Intensity Alpha", ylab="pnl")
whichmax <- which.max(profilev)
alpha <- alphav[whichmax]
pnls <- pnls[[whichmax]]
# Perform backtest over vector of dimension reduction parameters
dimaxs <- seq(from=3, to=40, by=2)
pnls <- lapply(dimaxs, function(dimax) {
  controlv <- HighFreq::param_portf(method="maxsharpe",
    dimax=dimax, alpha=alpha)
  HighFreq::back_test(retx=retx, retp=retp,
    startp=startp, endd=endd, controlv=controlv)
})  # end lapply
profilev <- sapply(pnls, sum)
plot(x=dimaxs, y=profilev, t="l", xlab="dimax", ylab="pnl",
  main="Strategy PnL as Function of dimax")
whichmax <- which.max(profilev)
dimax <- dimaxs[whichmax]
pnls <- pnls[[whichmax]]
pnls <- pnls*sd(indeks)/sd(pnls)
# Plot cumulative strategy returns
wealthv <- cbind(indeks, pnls, (pnls+indeks)/2)
colnames(wealthv) <- c("Index", "PortfStrat", "Combined")
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Optimal Rolling S&P500 Stock Portfolio Strategy") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", label="Combined", strokeWidth=3) %>%
  dyLegend(show="always", width=300)
# Create list of model parameters
controlv <- HighFreq::param_portf(method="maxsharpe",
      dimax=dimax, alpha=alpha)
# Perform backtest over look-backs
look_backs <- seq(from=5, to=16, by=1)
pnls <- lapply(look_backs, function(look_back) {
  startp <- c(rep_len(0, look_back), endd[1:(npts-look_back)])
  startp <- (startp - 1)
  startp[startp < 0] <- 0
  HighFreq::back_test(retx=retx, retp=retp,
    startp=startp, endd=endd, controlv=controlv)
})  # end lapply
profilev <- sapply(pnls, sum)
plot(x=look_backs, y=profilev, t="l", main="Strategy PnL as Function of Look-back Interval",
  xlab="Look-back Interval", ylab="pnl")
whichmax <- which.max(profilev)
look_back <- look_backs[whichmax]
pnls <- pnls[[whichmax]]
pnls <- pnls*sd(indeks)/sd(pnls)
# Calculate the out-of-sample Sharpe and Sortino ratios
wealthv <- cbind(indeks, pnls, (pnls+indeks)/2)
colnames(wealthv) <- c("Index", "PortfStrat", "Combined")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Dygraph the cumulative wealth
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Optimal Rolling S&P500 Portfolio Strategy") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", label="Combined", strokeWidth=3) %>%
  dyLegend(show="always", width=300)
# Calculate daily stock returns
symbolv <- c("VTI", "IEF", "DBC")
nstocks <- NROW(symbolv)
retp <- na.omit(rutils::etfenv$returns[, symbolv])
# Calculate covariance matrix of returns and its inverse
covmat <- cov(retp)
covinv <- solve(a=covmat)
unitv <- rep(1, nstocks)
# Calculate the minimum variance weights
c11 <- drop(t(unitv) %*% covinv %*% unitv)
weightmv <- drop(covinv %*% unitv/c11)
# Calculate the daily minvar portfolio returns in two ways
retmv <- (retp %*% weightmv)
all.equal(retmv, (retp %*% covinv %*% unitv)/c11)
# Calculate the minimum variance in three ways
all.equal(var(retmv),
  t(weightmv) %*% covmat %*% weightmv,
  1/(t(unitv) %*% covinv %*% unitv))
# Calculate vector of mean returns
retm <- colMeans(retp)
# Specify the target return
retarget <- 1.5*mean(retp)
# Products of inverse with mean returns and unit vector
c11 <- drop(t(unitv) %*% covinv %*% unitv)
cr1 <- drop(t(unitv) %*% covinv %*% retm)
crr <- drop(t(retm) %*% covinv %*% retm)
fmat <- matrix(c(c11, cr1, cr1, crr), nc=2)
# Solve for the Lagrange multipliers
lagm <- solve(a=fmat, b=c(2, 2*retarget))
# Calculate the efficient portfolio weights
weightv <- 0.5*drop(covinv %*% cbind(unitv, retm) %*% lagm)
# Calculate constraints
all.equal(1, sum(weights))
all.equal(retarget, sum(retm*weightv))
# Calculate the efficient portfolio returns
reteff <- drop(retp %*% weightv)
reteffm <- mean(reteff)
all.equal(reteffm, retarget)
# Calculate the efficient portfolio variance in three ways
uu <- c(1, retarget)
finv <- solve(fmat)
detf <- (c11*crr-cr1^2)  # det(fmat)
all.equal(var(reteff),
  drop(t(uu) %*% finv %*% uu),
  (c11*reteffm^2-2*cr1*reteffm+crr)/detf)
# Calculate the daily and mean minvar portfolio returns
c11 <- drop(t(unitv) %*% covinv %*% unitv)
weightv <- drop(covinv %*% unitv/c11)
retmv <- (retp %*% weightv)
retmvm <- sum(weightv*retm)
# Calculate the minimum variance
varmv <- 1/c11
stdevmv <- sqrt(varmv)
# Calculate efficient frontier from target returns
targetv <- retmvm*(1+seq(from=(-1), to=1, by=0.1))
stdevs <- sapply(targetv, function(rett) {
  uu <- c(1, rett)
  sqrt(drop(t(uu) %*% finv %*% uu))
})  # end sapply
# Plot the efficient frontier
plot(x=stdevs, y=targetv, t="l", col="blue", lwd=2,
     main="Efficient Frontier and Minimum Variance Portfolio",
     xlab="standard deviation", ylab="return")
points(x=stdevmv, y=retmvm, col="green", lwd=6)
text(x=stdevmv, y=retmvm, labels="minimum \nvariance",
     pos=4, cex=0.8)
# Calculate standard deviation of efficient portfolio
uu <- c(1, retarget)
stdeveff <- sqrt(drop(t(uu) %*% finv %*% uu))
# Calculate the slope of the tangent line
detf <- (c11*crr-cr1^2)  # det(fmat)
sharper <- (stdeveff*detf)/(c11*retarget-cr1)
# Calculate the risk-free rate as intercept of the tangent line
riskf <- retarget - sharper*stdeveff
# Calculate the risk-free rate from target return
all.equal(riskf,
  (retarget*cr1-crr)/(retarget*c11-cr1))
# Plot efficient frontier
aspratio <- 1.0*max(stdevs)/diff(range(targetv))
plot(x=stdevs, y=targetv, t="l", col="blue", lwd=2, asp=aspratio,
     xlim=c(0.4, 0.6)*max(stdevs), ylim=c(0.2, 0.9)*max(targetv),
     main="Efficient Frontier and Capital Market Line",
     xlab="standard deviation", ylab="return")
# Plot the minimum variance portfolio
points(x=stdevmv, y=retmvm, col="green", lwd=6)
text(x=stdevmv, y=retmvm, labels="minimum \nvariance",
     pos=4, cex=0.8)
# Plot the tangent portfolio
points(x=stdeveff, y=retarget, col="red", lwd=6)
text(x=stdeveff, y=retarget, labels="tangency\nportfolio", pos=2, cex=0.8)
# Plot the risk-free point
points(x=0, y=riskf, col="red", lwd=6)
text(x=0, y=riskf, labels="risk-free", pos=4, cex=0.8)
# Plot the tangent line
abline(a=riskf, b=sharper, lwd=2, col="green")
text(x=0.6*stdev, y=0.8*retarget,
     labels="Capital Market Line", pos=2, cex=0.8,
     srt=180/pi*atan(aspratio*sharper))
# Calculate the mean excess returns
riskf <- retarget - sharper*stdeveff
retx <- (retm - riskf)
# Calculate the efficient portfolio weights
weightv <- 0.5*drop(covinv %*% cbind(unitv, retm) %*% lagm)
# Calculate the maximum Sharpe weights
weightms <- drop(covinv %*% retx)/sum(covinv %*% retx)
all.equal(weightv, weightms)
# Calculate the maximum Sharpe mean return in two ways
all.equal(sum(retm*weightv),
  (cr1*riskf-crr)/(c11*riskf-cr1))
# Calculate the maximum Sharpe daily returns
retd <- (retp %*% weightms)
# Calculate the maximum Sharpe variance in four ways
detf <- (c11*crr-cr1^2)  # det(fmat)
all.equal(var(retd),
  t(weights) %*% covmat %*% weightv,
  (t(retx) %*% covinv %*% retx)/sum(covinv %*% retx)^2,
  (c11*retarget^2-2*cr1*retarget+crr)/detf)
# Calculate the maximum Sharpe ratio
sqrt(252)*sum(weightv*retx)/
  sqrt(drop(t(weights) %*% covmat %*% weightv))
# Calculate the stock Sharpe ratios
sqrt(252)*sapply((retp - riskf), function(x) mean(x)/sd(x))
# Calculate optimal portfolio returns
wealthv <- cbind(retp %*% weightms, retp %*% weightmv)
wealthv <- xts::xts(wealthv, zoo::index(retp))
colnames(wealthv) <- c("MaxSharpe", "MinVar")
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv,
  function(x) c(Sharpe=(mean(x)-riskf)/sd(x), Sortino=(mean(x)-riskf)/sd(x[x<0])))
# Plot the log wealth
endd <- rutils::calc_endpoints(retp, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Maximum Sharpe and Minimum Variance Portfolios") %>%
  dyOptions(colors=c("blue", "green"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Calculate the maximum Sharpe portfolios for different risk-free rates
detf <- (c11*crr-cr1^2)  # det(fmat)
riskfv <- retmvm*seq(from=1.3, to=20, by=0.1)
riskfv <- c(riskfv, retmvm*seq(from=(-20), to=0.7, by=0.1))
effront <- sapply(riskfv, function(riskf) {
  # Calculate the maximum Sharpe mean return
  reteffm <- (cr1*riskf-crr)/(c11*riskf-cr1)
  # Calculate the maximum Sharpe standard deviation
  stdev <- sqrt((c11*reteffm^2-2*cr1*reteffm+crr)/detf)
  c(return=reteffm, stdev=stdev)
})  # end sapply
effront <- effront[, order(effront["return", ])]
# Plot the efficient frontier
reteffv <- effront["return", ]
stdevs <- effront["stdev", ]
aspratio <- 0.6*max(stdevs)/diff(range(reteffv))
plot(x=stdevs, y=reteffv, t="l", col="blue", lwd=2, asp=aspratio,
  main="Maximum Sharpe Portfolio and Efficient Frontier",
  xlim=c(0.0, max(stdevs)), xlab="standard deviation", ylab="return")
# Plot the minimum variance portfolio
points(x=stdevmv, y=retmvm, col="green", lwd=6)
text(x=stdevmv, y=retmvm, labels="minimum \nvariance", pos=4, cex=0.8)
# Calculate the maximum Sharpe return and standard deviation
riskf <- min(reteffv)
retmax <- (cr1*riskf-crr)/(c11*riskf-cr1)
stdevmax <- sqrt((c11*retmax^2-2*cr1*retmax+crr)/detf)
# Plot the maximum Sharpe portfolio
points(x=stdevmax, y=retmax, col="red", lwd=6)
text(x=stdevmax, y=retmax, labels="Max Sharpe\nportfolio", pos=2, cex=0.8)
# Plot the risk-free point
points(x=0, y=riskf, col="red", lwd=6)
text(x=0, y=riskf, labels="risk-free", pos=4, cex=0.8)
# Plot the tangent line
sharper <- (stdevmax*detf)/(c11*retmax-cr1)
abline(a=riskf, b=sharper, lwd=2, col="green")
text(x=0.6*stdevmax, y=0.8*retmax, labels="Capital Market Line",
     pos=2, cex=0.8, srt=180/pi*atan(aspratio*sharper))
# Plot the efficient frontier
reteffv <- effront["return", ]
stdevs <- effront["stdev", ]
plot(x=stdevs, y=reteffv, t="l", col="blue", lwd=2,
  xlim=c(0.0, max(stdevs)),
  main="Efficient Frontier and Tangent Lines",
  xlab="standard deviation", ylab="return")
# Calculate vector of mean returns
reteffv <- min(reteffv) + diff(range(reteffv))*c(0.2, 0.4, 0.6, 0.8)
# Plot the tangent lines
for (reteffm in reteffv) {
  # Calculate the maximum Sharpe standard deviation
  stdev <- sqrt((c11*reteffm^2-2*cr1*reteffm+crr)/detf)
  # Calculate the slope of the tangent line
  sharper <- (stdev*detf)/(c11*reteffm-cr1)
  # Calculate the risk-free rate as intercept of the tangent line
  riskf <- reteffm - sharper*stdev
  # Plot the tangent portfolio
  points(x=stdev, y=reteffm, col="red", lwd=3)
  # Plot the tangent line
  abline(a=riskf, b=sharper, lwd=2, col="green")
} # end for
# Calculate random portfolios
nportf <- 1000
randportf <- sapply(1:nportf, function(it) {
  weightv <- runif(nstocks-1, min=-0.25, max=1.0)
  weightv <- c(weightv, 1-sum(weights))
  # Portfolio returns and standard deviation
  c(return=252*sum(weightv*retm),
    stdev=sqrt(252*drop(weightv %*% covmat %*% weightv)))
})  # end sapply
# Plot scatterplot of random portfolios
x11(widthp <- 6, heightp <- 6)
plot(x=randportf["stdev", ], y=randportf["return", ],
     main="Efficient Frontier and Random Portfolios",
     xlim=c(0.5*stdev, 0.8*max(randportf["stdev", ])),
     xlab="standard deviation", ylab="return")
# Plot maximum Sharpe portfolios
lines(x=effront[, "stdev"], y=effront[, "return"], lwd=2)
points(x=effront[, "stdev"], y=effront[, "return"],
 col="red", lwd=3)
# Plot the minimum variance portfolio
points(x=stdev, y=retp, col="green", lwd=6)
text(stdev, retp, labels="minimum\nvariance", pos=2, cex=0.8)
# Plot efficient portfolio
points(x=effront[marketp, "stdev"],
 y=effront[marketp, "return"], col="green", lwd=6)
text(x=effront[marketp, "stdev"], y=effront[marketp, "return"],
     labels="market\nportfolio", pos=2, cex=0.8)
# Plot individual assets
points(x=sqrt(252*diag(covmat)),
 y=252*retm, col="blue", lwd=6)
text(x=sqrt(252*diag(covmat)), y=252*retm,
     labels=names(retm),
     col="blue", pos=1, cex=0.8)
riskf <- 0.03
retp <- c(asset1=0.05, asset2=0.06)
stdevs <- c(asset1=0.4, asset2=0.5)
corrp <- 0.6
covmat <- matrix(c(1, corrp, corrp, 1), nc=2)
covmat <- t(t(stdevs*covmat)*stdevs)
weightv <- seq(from=(-1), to=2, length.out=31)
weightv <- cbind(weightv, 1-weightv)
retp <- weightv %*% retp
portfsd <- sqrt(rowSums(weightv*(weightv %*% covmat)))
sharper <- (retp-riskf)/portfsd
whichmax <- which.max(sharper)
sharpem <- max(sharper)
# Plot efficient frontier
x11(widthp <- 6, heightp <- 5)
par(mar=c(3,3,2,1)+0.1, oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
plot(portfsd, retp, t="l",
 main=paste0("Efficient frontier and CML for two assets\ncorrelation = ", 100*corrp, "%"),
 xlab="standard deviation", ylab="return",
 lwd=2, col="orange",
 xlim=c(0, max(portfsd)),
 ylim=c(0.02, max(retp)))
# Add efficient portfolio (maximum Sharpe ratio portfolio)
points(portfsd[whichmax], retp[whichmax],
 col="blue", lwd=3)
text(x=portfsd[whichmax], y=retp[whichmax],
     labels=paste(c("efficient portfolio\n",
 structure(c(weightv[whichmax], 1-weightv[whichmax]),
         names=names(retp))), collapse=" "),
     pos=2, cex=0.8)
# Plot individual assets
points(stdevs, retp, col="green", lwd=3)
text(stdevs, retp, labels=names(retp), pos=4, cex=0.8)
# Add point at risk-free rate and draw Capital Market Line
points(x=0, y=riskf, col="blue", lwd=3)
text(0, riskf, labels="risk-free\nrate", pos=4, cex=0.8)
abline(a=riskf, b=sharpem, lwd=2, col="blue")
rangev <- par("usr")
text(portfsd[whichmax]/2, (retp[whichmax]+riskf)/2,
     labels="Capital Market Line", cex=0.8, , pos=3,
     srt=45*atan(sharpem*(rangev[2]-rangev[1])/
             (rangev[4]-rangev[3])*
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
retp <- na.omit(rutils::etfenv$returns[, symbolv])
retp <- retp %*% t(weights)
portfv <- cbind(252*colMeans(retp),
  sqrt(252)*matrixStats::colSds(retp))
colnames(portfv) <- c("returns", "stdev")
riskf <- 0.06
portfv <- cbind(portfv,
  (portfv[, "returns"]-riskf)/portfv[, "stdev"])
colnames(portfv)[3] <- "Sharpe"
whichmax <- which.max(portfv[, "Sharpe"])
sharpem <- portfv[whichmax, "Sharpe"]
plot(x=portfv[, "stdev"], y=portfv[, "returns"],
     main="Stock and Bond portfolios", t="l",
     xlim=c(0, 0.7*max(portfv[, "stdev"])), ylim=c(0, max(portfv[, "returns"])),
     xlab="standard deviation", ylab="return")
# Add blue point for efficient portfolio
points(x=portfv[whichmax, "stdev"], y=portfv[whichmax, "returns"], col="blue", lwd=6)
text(x=portfv[whichmax, "stdev"], y=portfv[whichmax, "returns"],
     labels=paste(c("efficient portfolio\n",
  structure(c(weightv[whichmax, 1], weightv[whichmax, 2]), names=symbolv)), collapse=" "),
     pos=3, cex=0.8)
# Plot individual assets
retm <- 252*sapply(retp, mean)
stdevs <- sqrt(252)*sapply(retp, sd)
points(stdevs, retm, col="green", lwd=6)
text(stdevs, retm, labels=names(retp), pos=2, cex=0.8)
# Add point at risk-free rate and draw Capital Market Line
points(x=0, y=riskf, col="blue", lwd=6)
text(0, riskf, labels="risk-free", pos=4, cex=0.8)
abline(a=riskf, b=sharpem, col="blue", lwd=2)
rangev <- par("usr")
text(max(portfv[, "stdev"])/3, 0.75*max(portfv[, "returns"]),
     labels="Capital Market Line", cex=0.8, , pos=3,
     srt=45*atan(sharpem*(rangev[2]-rangev[1])/
             (rangev[4]-rangev[3])*
             heightp/widthp)/(0.25*pi))
# Plot portfolios in x11() window
x11(widthp <- 6, heightp <- 5)
# Calculate cumulative returns of VTI and IEF
retsoptim <- lapply(retp,
  function(retp) exp(cumsum(retp)))
retsoptim <- rutils::do_call(cbind, retsoptim)
# Calculate the efficient portfolio returns
retsoptim <- cbind(exp(cumsum(retp %*%
    c(weightv[whichmax], 1-weightv[whichmax]))),
  retsoptim)
colnames(retsoptim)[1] <- "efficient"
# Plot efficient portfolio with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue", "green")
chart_Series(retsoptim, theme=plot_theme,
   name="Efficient Portfolio for Stocks and Bonds")
legend("top", legend=colnames(retsoptim),
   cex=0.8, inset=0.1, bg="white", lty=1,
   lwd=6, col=plot_theme$col$line.col, bty="n")
library(rutils)
library(Rglpk)
# Vector of symbol names
symbolv <- c("VTI", "IEF", "DBC")
nstocks <- NROW(symbolv)
# Calculate the objective vector - the mean returns
retp <- na.omit(rutils::etfenv$returns[, symbolv])
objvec <- colMeans(retp)
# Specify matrix of linear constraint coefficients
coeffm <- matrix(c(rep(1, nstocks), 1, 1, 0),
           nc=nstocks, byrow=TRUE)
# Specify the logical constraint operators
logop <- c("==", "<=")
# Specify the vector of constraints
consv <- c(1, 0)
# Specify box constraints (-1, 1) (default is c(0, Inf))
boxc <- list(lower=list(ind=1:nstocks, val=rep(-1, nstocks)),
       upper=list(ind=1:nstocks, val=rep(1, nstocks)))
# Perform optimization
optiml <- Rglpk::Rglpk_solve_LP(
  obj=objvec,
  mat=coeffm,
  dir=logop,
  rhs=consv,
  bounds=boxc,
  max=TRUE)
all.equal(optiml$optimum, sum(objvec*optiml$solution))
optiml$solution
coeffm %*% optiml$solution
# Calculate the VTI percentage returns
retp <- na.omit(rutils::etfenv$returns$VTI)
confl <- 0.1
varisk <- quantile(retp, confl)
cvar <- mean(retp[retp < varisk])
# Or
sortv <- sort(as.numeric(retp))
varind <- round(confl*NROW(retp))
varisk <- sortv[varind]
cvar <- mean(sortv[1:varind])
# Plot histogram of VTI returns
varmin <- (-0.05)
histp <- hist(retp, col="lightgrey",
  xlab="returns", breaks=100, xlim=c(varmin, 0.01),
  ylab="frequency", freq=FALSE, main="VTI Returns Histogram")
# Plot density of losses
densv <- density(retp, adjust=1.5)
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
retp <- na.omit(rutils::etfenv$returns[, symbolv])
retm <- colMeans(retp)
confl <- 0.05
rmin <- 0 ; wmin <- 0 ; wmax <- 1
weightsum <- 1
ncols <- NCOL(retp) # number of assets
nrows <- NROW(retp) # number of rows
# Create objective vector
objvec <- c(numeric(ncols), rep(-1/(confl/nrows), nrows), -1)
# Specify matrix of linear constraint coefficients
coeffm <- rbind(cbind(rbind(1, retm),
                matrix(data=0, nrow=2, ncol=(nrows+1))),
          cbind(coredata(retp), diag(nrows), 1))
# Specify the logical constraint operators
logop <- c("==", ">=", rep(">=", nrows))
# Specify the vector of constraints
consv <- c(weightvum, rmin, rep(0, nrows))
# Specify box constraints (wmin, wmax) (default is c(0, Inf))
boxc <- list(lower=list(ind=1:ncols, val=rep(wmin, ncols)),
       upper=list(ind=1:ncols, val=rep(wmax, ncols)))
# Perform optimization
optiml <- Rglpk_solve_LP(obj=objvec, mat=coeffm, dir=logop, rhs=consv, types=rep("C", NROW(objvec)), max=T, bounds=boxc)
all.equal(optiml$optimum, sum(objvec*optiml$solution))
coeffm %*% optiml$solution
as.numeric(optiml$solution[1:ncols])
# Calculate daily percentage returns
symbolv <- c("VTI", "IEF", "DBC")
nstocks <- NROW(symbolv)
retp <- na.omit(rutils::etfenv$returns[, symbolv])
# Create initial vector of portfolio weights
weightv <- rep(1, NROW(symbolv))
names(weights) <- symbolv
# Objective equal to minus Sharpe ratio
objfun <- function(weightv, retp) {
  retp <- retp %*% weightv
  if (sd(retp) == 0)
    return(0)
  else
    -return(mean(retp)/sd(retp))
}  # end objfun
# Objective for equal weight portfolio
objfun(weightv, retp=retp)
optiml <- unlist(optimize(
  f=function(weight)
    objfun(c(1, 1, weight), retp=retp),
  interval=c(-4, 1)))
# Vectorize objective function with respect to third weight
objvec <- function(weights) sapply(weightv,
  function(weight) objfun(c(1, 1, weight),
    retp=retp))
# Or
objvec <- Vectorize(FUN=function(weight)
    objfun(c(1, 1, weight), retp=retp),
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
             retp=retp,
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
retc <- lapply(retp,
  function(retp) exp(cumsum(retp)))
retc <- rutils::do_call(cbind, retc)
# Calculate optimal portfolio returns with VTI, IEF, DBC
retsoptim <- cbind(
  exp(cumsum(retp %*% optiml$par)),
  retc)
colnames(retsoptim)[1] <- "retsoptim"
# Plot optimal returns with VTI, IEF, DBC
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("black", "red", "green", "blue")
chart_Series(retsoptim, theme=plot_theme,
       name="Optimized portfolio performance")
legend("top", legend=colnames(retsoptim), cex=1.0,
   inset=0.1, bg="white", lty=1, lwd=6,
   col=plot_theme$col$line.col, bty="n")
# Or plot non-compounded (simple) cumulative returns
PerformanceAnalytics::chart.CumReturns(
  cbind(retp %*% optiml$par, retp),
  lwd=2, ylab="", legend.loc="topleft", main="")
riskf <- 0.03
retp <- c(asset1=0.05, asset2=0.06)
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
nstocks <- NROW(symbolv)
retp <- na.omit(rutils::etfenv$returns[, symbolv])
# Calculate the covariance matrix
covmat <- cov(retp)
# Minimum variance weights, with sum equal to 1
optiml <- quadprog::solve.QP(Dmat=2*covmat,
            dvec=numeric(3),
            Amat=matrix(1, nr=3, nc=1),
            bvec=1)
# Minimum variance, maximum returns
optiml <- quadprog::solve.QP(Dmat=2*covmat,
            dvec=apply(0.1*retp, 2, mean),
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
symbolv <- c("VTI", "IEF", "DBC")
nstocks <- NROW(symbolv)
retp <- na.omit(rutils::etfenv$returns[, symbolv])
# Objective equal to minus Sharpe ratio
objfun <- function(weightv, retp) {
  retp <- retp %*% weightv
  if (sd(retp) == 0)
    return(0)
  else
    -return(mean(retp)/sd(retp))
}  # end objfun
# Perform optimization using DEoptim
optiml <- DEoptim::DEoptim(fn=objfun,
  upper=rep(10, NCOL(retp)),
  lower=rep(-10, NCOL(retp)),
  retp=retp,
  control=list(trace=FALSE, itermax=100, parallelType=1))
weightv <- optiml$optim$bestmem/sum(abs(optiml$optim$bestmem))
names(weights) <- colnames(retp)
# Objective with shrinkage penalty
objfun <- function(weightv, retp, lambda, alpha) {
  retp <- retp %*% weightv
  if (sd(retp) == 0)
    return(0)
  else {
    penaltyv <- lambda*((1-alpha)*sum(weightv^2) +
alpha*sum(abs(weights)))
    -return(mean(retp)/sd(retp) + penaltyv)
  }
}  # end objfun
# Objective for equal weight portfolio
weightv <- rep(1, NROW(symbolv))
names(weights) <- symbolv
lambda <- 0.5 ; alpha <- 0.5
objfun(weightv, retp=retp, lambda=lambda, alpha=alpha)
# Perform optimization using DEoptim
optiml <- DEoptim::DEoptim(fn=objfun,
  upper=rep(10, NCOL(retp)),
  lower=rep(-10, NCOL(retp)),
  retp=retp,
  lambda=lambda,
  alpha=alpha,
  control=list(trace=FALSE, itermax=100, parallelType=1))
weightv <- optiml$optim$bestmem/sum(abs(optiml$optim$bestmem))
names(weights) <- colnames(retp)
