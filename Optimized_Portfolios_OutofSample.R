






# # Load stock returns
# load("/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
# datev <- zoo::index(na.omit(retstock$GOOGL))
# retp <- retstock[datev]  Subset the returns to GOOGL
# # Remove the stocks with any NA values
# numna <- sapply(retp, function(x) sum(is.na(x)))
# retp <- retp[, numna==0]
# nstocks <- NCOL(retp)
# datev <- zoo::index(retp)
# symbolv <- colnames(retp)
# retis <- retp["/2014"]  In-sample returns
# raterf <- 0.03/252
# retx <- (retis - raterf)  Excess returns
# # Calculate the covariance ignoring NA values
# covmat <- cov(retp, use="pairwise.complete.obs")
# sum(is.na(covmat))
# # Calculate the inverse of covmat
# invmat <- solve(covmat)
# round(invmat %*% covmat, digits=5)
# # Calculate the generalized inverse of covmat
# invreg <- MASS::ginv(covmat)
# all.equal(unname(invmat), invreg)

# # Create rectangular matrix with collinear columns
# matv <- matrix(rnorm(10*8), nc=10)
# # Calculate covariance matrix
# covmat <- cov(matv)
# # Calculate inverse of covmat - error
# invmat <- solve(covmat)
# # Perform eigen decomposition
# eigend <- eigen(covmat)
# eigenvec <- eigend$vectors
# eigenval <- eigend$values
# # Set tolerance for determining zero singular values
# precv <- sqrt(.Machine$double.eps)
# # Calculate generalized inverse from the eigen decomposition
# notzero <- (eigenval > (precv*eigenval[1]))
# inveigen <- eigenvec[, notzero] %*%
#   (t(eigenvec[, notzero]) / eigenval[notzero])
# # Inverse property of inveigen isn't satisfied
# all.equal(inveigen %*% covmat, diag(NROW(covmat)))
# # Generalized inverse property of inveigen is satisfied
# all.equal(covmat %*% inveigen %*% covmat, covmat)
# # Calculate generalized inverse using MASS::ginv()
# invreg <- MASS::ginv(covmat)
# # Verify that inveigen is the same as invreg
# all.equal(inveigen, invreg)

# # Maximum Sharpe weights in-sample interval
# retis <- retp["/2014"]
# raterf <- 0.03/252
# retx <- (retis - raterf)
# invreg <- MASS::ginv(cov(retis, use="pairwise.complete.obs"))
# weightv <- drop(invreg %*% colMeans(retx, na.rm=TRUE))
# weightv <- weightv/sqrt(sum(weightv^2))
# names(weightv) <- colnames(retp)

# # Plot portfolio weights
# barplot(sort(weightv), main="Maximum Sharpe Weights", cex.names=0.7)

# # Maximum Sharpe weights in-sample interval
# colmeanv <- colMeans(retx, na.rm=TRUE)
# covmat <- cov(retis, use="pairwise.complete.obs")
# invreg <- MASS::ginv(covmat)
# weightv <- drop(invreg %*% colmeanv)
# names(weightv) <- symbolv
# head(sort(weightv))
# tail(sort(weightv))
# # Calculate the portfolio returns
# pnls <- HighFreq::mult_mat(weightv, retp)
# pnls <- rowMeans(pnls, na.rm=TRUE)
# pnls <- xts::xts(pnls, datev)
# retew <- xts::xts(rowMeans(retp, na.rm=TRUE), datev)
# pnls <- pnls*sd(retew["/2014"])/sd(pnls["/2014"])
# wealthv <- cbind(retew, pnls)
# colnames(wealthv) <- c("EqualWeight", "MaxSharpe")

# # Calculate the in-sample Sharpe and Sortino ratios
# sqrt(252)*sapply(wealthv["/2014"], function(x)
#   c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# # Calculate the out-of-sample Sharpe and Sortino ratios
# sqrt(252)*sapply(wealthv["2015/"], function(x)
#   c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# # Plot of cumulative portfolio returns
# endw <- rutils::calc_endpoints(wealthv, interval="weeks")
# dygraphs::dygraph(cumsum(wealthv)[endw],
#   main="Out-of-Sample Maximum Sharpe Stock Portfolio") %>%
#   dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
#   dyEvent(zoo::index(last(retis[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
#   dyLegend(width=300)

# # Objective function equal to the sum of returns
# objfun <- function(retp) sum(na.omit(retp))
# # Objective function equal to the Sharpe ratio
# objfun <- function(retp) {
#   retp <- na.omit(retp)
#   if (NROW(retp) > 12) {
#     stdev <- sd(retp)
#     if (stdev > 0) mean(retp)/stdev else 0
#   } else 0
# }   end objfun
# # Objective function equal to the Kelly ratio
# objfun <- function(retp) {
#   retp <- na.omit(retp)
#   if (NROW(retp) > 12) {
#     varv <- var(retp)
#     if (varv > 0) mean(retp)/varv else 0
#   } else 0
# }   end objfun

# # Calculate in-sample covariance matrix
# covmat <- cov(retis, use="pairwise.complete.obs")
# eigend <- eigen(covmat)
# eigenvec <- eigend$vectors
# eigenval <- eigend$values
# # Plot the eigenvalues
# barplot(eigenval, main="ETF Covariance Eigenvalues", cex.names=0.7)
# # Calculate reduced inverse of covariance matrix
# dimax <- 9
# invred <- eigenvec[, 1:dimax] %*%
#   (t(eigenvec[, 1:dimax]) / eigenval[1:dimax])
# # Reduced inverse does not satisfy matrix inverse property
# all.equal(covmat %*% invred %*% covmat, covmat)

# # Calculate reduced inverse of covariance matrix
# dimax <- 10
# eigend <- eigen(covmat)
# eigenvec <- eigend$vectors
# eigenval <- eigend$values
# invred <- eigenvec[, 1:dimax] %*%
#   (t(eigenvec[, 1:dimax]) / eigenval[1:dimax])
# # Calculate portfolio weights and PnLs
# colmeanv <- colMeans(retx["/2014"], na.rm=TRUE)
# weightv <- invred %*% colmeanv
# pnls <- HighFreq::mult_mat(weightv, retp)
# pnls <- rowMeans(pnls, na.rm=TRUE)
# pnls <- xts::xts(pnls, datev)
# pnls <- pnls*sd(retew["/2014"])/sd(pnls["/2014"])

# # Combine with equal weight
# wealthv <- cbind(retew, pnls)
# colnames(wealthv) <- c("EqualWeight", "MaxSharpe")
# # Calculate the in-sample Sharpe and Sortino ratios
# sqrt(252)*sapply(wealthv["/2014"], function(x)
#   c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# # Calculate the out-of-sample Sharpe and Sortino ratios
# sqrt(252)*sapply(wealthv["2015/"], function(x)
#   c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# # Plot of cumulative portfolio returns
# dygraphs::dygraph(cumsum(wealthv)[endw],
#   main="Maximum Sharpe With Dimension Reduction") %>%
#   dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
#   dyEvent(zoo::index(last(retis[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
#   dyLegend(width=300)

# # Perform loop over vector of dimension reduction parameters
# dimv <- seq(from=5, to=11)
# pnls <- mclapply(dimv, function(dimax) {
#   invred <- eigenvec[, 1:dimax] %*%
#     (t(eigenvec[, 1:dimax]) / eigenval[1:dimax])
#    Calculate portfolio weights and PnLs
#   weightv <- invred %*% colmeanv
#   pnls <- HighFreq::mult_mat(weightv, retp)
#   pnls <- rowMeans(pnls, na.rm=TRUE)
#   pnls <- xts::xts(pnls, datev)
#   pnls*sd(retew["/2014"])/sd(pnls["/2014"])
# }, mc.cores=ncores)   end mclapply
# pnls <- do.call(cbind, pnls)
# colnames(pnls) <- paste0("dim=", dimv)
# profilev <- sapply(pnls["2015/"], sum)
# dimax <- dimv[which.max(profilev)]

# # Plot of cumulative portfolio returns
# colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
# endw <- rutils::calc_endpoints(pnls["2015/"], interval="weeks")
# dygraphs::dygraph(cumsum(pnls["2015/"])[endw],
#   main="Out-of-Sample PnLs Wih Dimension Reduction") %>%
#   dyOptions(colors=colorv, strokeWidth=1) %>%
#   dyLegend(show="always", width=300)

# # Shrink the in-sample returns to their mean
# alphac <- 0.3
# retxm <- rowMeans(retx, na.rm=TRUE)
# retxis <- (1-alphac)*retx + alphac*retxm
# # Calculate portfolio weights and PnLs
# weightv <- drop(invred %*% colMeans(retxis, na.rm=TRUE))
# names(weightv) <- symbolv
# pnls <- HighFreq::mult_mat(weightv, retp)
# pnls <- rowMeans(pnls, na.rm=TRUE)
# pnls <- xts::xts(pnls, datev)
# pnls <- pnls*sd(retew["/2014"])/sd(pnls["/2014"])

# # Combine with equal weight
# wealthv <- cbind(retew, pnls)
# colnames(wealthv) <- c("EqualWeight", "MaxSharpe")
# # Calculate the in-sample Sharpe and Sortino ratios
# sqrt(252)*sapply(wealthv["/2014"], function(x)
#   c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# # Calculate the out-of-sample Sharpe and Sortino ratios
# sqrt(252)*sapply(wealthv["2015/"], function(x)
#   c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# # Plot of cumulative portfolio returns
# dygraphs::dygraph(cumsum(wealthv)[endw],
#   main="Maximum Sharpe With Return Shrinkage") %>%
#   dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
#   dyEvent(zoo::index(last(retis[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
#   dyLegend(width=300)

# # Perform loop over vector of shrinkage intensities
# alphav <- seq(from=0.5, to=0.9, by=0.1)
# pnls <- mclapply(alphav, function(alphac) {
#   retxis <- (1-alphac)*retx + alphac*retxm
#   weightv <- drop(invred %*% colMeans(retxis, na.rm=TRUE))
#   pnls <- HighFreq::mult_mat(weightv, retp)
#   pnls <- rowMeans(pnls, na.rm=TRUE)
#   pnls <- xts::xts(pnls, datev)
#   pnls*sd(retew["/2014"])/sd(pnls["/2014"])
# }, mc.cores=ncores)   end mclapply
# pnls <- do.call(cbind, pnls)
# colnames(pnls) <- paste0("alpha=", alphav)
# profilev <- sapply(pnls["2015/"], sum)
# alphac <- alphav[which.max(profilev)]

# # Plot of cumulative portfolio returns
# colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
# endw <- rutils::calc_endpoints(pnls["2015/"], interval="weeks")
# dygraphs::dygraph(cumsum(pnls["2015/"])[endw],
#   main="Out-of-Sample PnLs Wih Shrinkage") %>%
#   dyOptions(colors=colorv, strokeWidth=1) %>%
#   dyLegend(show="always", width=300)

# # Define monthly end points
# endd <- rutils::calc_endpoints(retp, interval="months")
# endd <- endd[endd > (nstocks+1)]
# npts <- NROW(endd) ; lookb <- 12
# startp <- c(rep_len(0, lookb), endd[1:(npts-lookb)])
# # !!! Perform parallel loop over end points - takes very long!!!
# library(parallel)   Load package parallel
# ncores <- detectCores() - 1
# pnls <- mclapply(1:(npts-1), function(tday) {
#      Subset the excess returns
#     retis <- retp[startp[tday]:endd[tday], ]
#     retis[is.na(retis)] <- 0
#     invreg <- MASS::ginv(cov(retis, use="pairwise.complete.obs"))
#      Calculate the maximum Sharpe ratio portfolio weights
#     retm <- colMeans(retis, na.rm=TRUE)
#     weightv <- invreg %*% retm
#      Zero weights if sparse data
#     zerov <- sapply(retis, function(x) (sum(x == 0) > 5))
#     weightv[zerov] <- 0
#      Calculate in-sample portfolio returns
#     pnlis <- (retis %*% weightv)
#      Scale the weights to the in-sample volatility
#     weightv <- weightv*sd(retm)/sd(pnlis)
#      Calculate the out-of-sample portfolio returns
#     retos <- retp[(endd[tday]+1):endd[tday+1], ]
#     pnls <- HighFreq::mult_mat(weightv, retos)
#     pnls <- rowMeans(pnls, na.rm=TRUE)
#     xts::xts(pnls, zoo::index(retos))
# }, mc.cores=ncores)   end mclapply
# pnls <- rutils::do_call(rbind, pnls)
# pnls <- rbind(retew[paste0("/", start(pnls)-1)], pnls*sd(retew)/sd(pnls))

# # Calculate the Sharpe and Sortino ratios
# wealthv <- cbind(retew, pnls)
# colnames(wealthv) <- c("EqualWeight", "MaxSharpe")
# sqrt(252)*sapply(wealthv, function(x)
#   c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))

# # Plot cumulative strategy returns
# dygraphs::dygraph(cumsum(wealthv)[endw],
#   main="Rolling Maximum Sharpe Portfolio Strategy") %>%
#   dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
#   dyLegend(show="always", width=300)

# # Create random matrix of returns
# matv <- matrix(rnorm(300), nc=5)
# # Reduced inverse of covariance matrix
# dimax <- 3
# eigend <- eigen(covmat)
# invred <- eigend$vectors[, 1:dimax] %*%
#   (t(eigend$vectors[, 1:dimax]) / eigend$values[1:dimax])
# # Reduced inverse using RcppArmadillo
# invarma <- HighFreq::calc_inv(covmat, dimax)
# all.equal(invred, invarma)
# # Microbenchmark RcppArmadillo code
# library(microbenchmark)
# summary(microbenchmark(
#   rcode={eigend <- eigen(covmat)
#     eigend$vectors[, 1:dimax] %*%
# (t(eigend$vectors[, 1:dimax]) / eigend$values[1:dimax])
#   },
#   rcpp=calc_inv(covmat, dimax),
#   times=10))[, c(1, 4, 5)]   end microbenchmark summary

# # Shift the end points to C++ convention
# endd <- (endd - 1)
# endd[endd < 0] <- 0
# startp <- (startp - 1)
# startp[startp < 0] <- 0
# # Specify dimension reduction and return shrinkage using list of portfolio optimization parameters
# dimax <- 9
# alphac <- 0.8
# controll <- HighFreq::param_portf(method="maxsharpe",
#   dimax=dimax, alpha=alphac)
# # Perform backtest in Rcpp - takes very long!!!
# retx <- (retp - raterf)
# pnls <- HighFreq::roll_portf(retx=retx, retp=retp,
#   startp=startp, endd=endd, controll=controll)
# pnls <- pnls*sd(retew)/sd(pnls)

# # Calculate the out-of-sample Sharpe and Sortino ratios
# wealthv <- cbind(retew, pnls, (pnls + retew)/2)
# colnames(wealthv) <- c("EqualWeight", "MaxSharpe", "Combined")
# sqrt(252)*sapply(wealthv, function(x)
#   c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# # Plot cumulative strategy returns
# dygraphs::dygraph(cumsum(wealthv)[endw],
#   main="Rolling S&P500 Portfolio Strategy With Shrinkage") %>%
#   dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
#   dySeries(name="Combined", label="Combined", strokeWidth=2) %>%
#   dyLegend(show="always", width=300)

# # Perform backtest over vector of dimension reduction parameters
# dimv <- seq(from=3, to=21, by=2)
# pnls <- mclapply(dimv, function(dimax) {
#   controll <- HighFreq::param_portf(method="maxsharpe",
#     dimax=dimax, alpha=alphac)
#   HighFreq::roll_portf(retx=retx, retp=retp,
#     startp=startp, endd=endd, controll=controll)
# }, mc.cores=ncores)   end mclapply
# profilev <- sapply(pnls, sum)
# dimax <- dimv[which.max(profilev)]

# # Plot of rolling strategy PnL as a function of dimax
# plot(x=dimv, y=profilev, t="l", xlab="dimax", ylab="pnl",
#   main="Rolling Strategy PnL as Function of dimax")

# # Perform backtest over vector of shrinkage intensities
# alphav <- seq(from=0.5, to=0.9, by=0.1)
# pnls <- mclapply(alphav, function(alphac) {
#   controll <- HighFreq::param_portf(method="maxsharpe",
#       dimax=dimax, alpha=alphac)
#   HighFreq::roll_portf(retx=retx, retp=retp,
#       startp=startp, endd=endd, controll=controll)
# }, mc.cores=ncores)   end mclapply
# profilev <- sapply(pnls, sum)
# alphac <- alphav[which.max(profilev)]

# # Plot of rolling strategy PnL as a function of shrinkage intensity
# plot(x=alphav, y=profilev, t="l",
#   main="Rolling Strategy PnL as Function of Return Shrinkage",
#   xlab="Shrinkage Intensity Alpha", ylab="pnl")

# # Create list of model parameters
# controll <- HighFreq::param_portf(method="maxsharpe",
#       dimax=dimax, alpha=alphac)
# # Perform backtest over look-backs
# lookbv <- seq(from=5, to=16, by=1)
# pnls <- mclapply(lookbv, function(lookb) {
#   startp <- c(rep_len(0, lookb), endd[1:(npts-lookb)])
#   startp <- (startp - 1) ; startp[startp < 0] <- 0
#   HighFreq::roll_portf(retx=retx, retp=retp,
#     startp=startp, endd=endd, controll=controll)
# }, mc.cores=ncores)   end mclapply
# profilev <- sapply(pnls, sum)
# lookb <- lookbv[which.max(profilev)]

# # Plot of rolling strategy PnL as a function of look-back interval
# plot(x=lookbv, y=profilev, t="l", main="MaxSharpe PnL as Function of Look-back Interval",
#   xlab="Look-back Interval", ylab="pnl")

# # Calculate the out-of-sample Sharpe and Sortino ratios
# pnls <- pnls[[whichmax]]
# pnls <- pnls*sd(retew)/sd(pnls)
# wealthv <- cbind(retew, pnls, (pnls + retew)/2)
# colnames(wealthv) <- c("EqualWeight", "MaxSharpe", "Combined")
# sqrt(252)*sapply(wealthv, function(x)
#   c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# # Dygraph the cumulative wealth
# dygraphs::dygraph(cumsum(wealthv)[endw],
#   main="Optimal Maximum Sharpe Portfolio Strategy") %>%
#   dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
#   dySeries(name="Combined", label="Combined", strokeWidth=2) %>%
#   dyLegend(show="always", width=300)

# # Calculate the autocorrelations of the PCA time series
# pacv <- apply(retpca[, 1:100], 2, function(x)
#   sum(pacf(x, lag=10, plot=FALSE)$acf))
# plot(pacv, type="h", main="PCA Autocorrelations",
#      xlab="PC", ylab="PACF")

# # Simulate daily PCA momentum strategies for multiple lambdaf parameters
# dimax <- 30
# lambdav <- seq(0.97, 0.99, 0.005)
# pnls <- mclapply(lambdav, btmomdailyhold, retp=retpca[, 1:dimax], mc.cores=ncores)
# pnls <- lapply(pnls, function(pnl) volew*pnl/sd(pnl))
# pnls <- do.call(cbind, pnls)
# colnames(pnls) <- paste0("lambda=", lambdav)
# pnls <- xts::xts(pnls, datev)

# # Plot Sharpe ratios of momentum strategies
# sharper <- sqrt(252)*sapply(pnls, function(pnl) mean(pnl)/sd(pnl))
# plot(x=lambdav, y=sharper, t="l",
#   main="PCA Momentum Sharpe as Function of Decay Factor",
#   xlab="lambdaf", ylab="Sharpe")
# # Plot dygraph of daily PCA momentum strategies
# colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
# endw <- rutils::calc_endpoints(pnls, interval="weeks")
# dygraphs::dygraph(cumsum(pnls)[endw],
#   main="Daily PCA Momentum Strategies") %>%
#   dyOptions(colors=colorv, strokeWidth=1) %>%
#   dyLegend(show="always", width=400)

# # Calculate best pnls of PCA momentum strategy
# whichmax <- which.max(sharper)
# lambdav[whichmax]
# pnls <- pnls[, whichmax]
# # Calculate the Sharpe and Sortino ratios
# wealthv <- cbind(retew, pnls, 0.5*(retew + pnls))
# colnames(wealthv) <- c("EqualWeight", "Momentum", "Combined")
# cor(wealthv)
# sqrt(252)*sapply(wealthv, function(x)
#   c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))

# # Plot dygraph of stock index and PCA momentum strategy
# dygraphs::dygraph(cumsum(wealthv)[endw],
#   main="Optimal Daily Momentum Strategy for Stocks") %>%
#   dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
#   dySeries(name="Combined", strokeWidth=2) %>%
#   dyLegend(show="always", width=300)
