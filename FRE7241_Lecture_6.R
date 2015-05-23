library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
thm <- knit_theme$get("acid")
knit_theme$set(thm)
library(PerformanceAnalytics)
load(file="C:/Develop/data/etf_data.Rdata")
# rebalancing period
re_balance <- "weeks"
# look-back period in number of re_balance
win_dow <- 40

# create index of rebalancing period end points
end_points <- endpoints(etf_rets, 
                on=re_balance)
end_points[1] <- 1

# create index of rebalancing periods
periods <- lapply(win_dow:(length(end_points)-1),
    function(point)
list(
  back=end_points[point-win_dow+1]:(end_points[point]-1),
  fwd=end_points[point]:end_points[point+1])  # end list
    )  # end lapply
# calculate risk&ret stats for some symbols, over a range of dates
risk_ret_stats <- function(x_ts=etf_rets,  # daily returns
                 sym_bols=colnames(x_ts),  # names
                 range=index(x_ts),  # date range
                 ret="mean",  # return stat
                 risk="mad") {  # risk stat
  ret <- match.fun(ret)  # match to function
  risk <- match.fun(risk)  # match to function
  stats <- sapply(x_ts[range, sym_bols], 
  function(ts)
    c(ret=ret(ts), risk=risk(ts))
  )  # end sapply
  t(stats)
}  # end risk_ret_stats

# example
head(risk_ret_stats(range=
        periods[[1]]$back))
# calculate stats over overlaping period date windows
period_stats <- lapply(periods,
   function(point)
     cbind(risk_ret_stats(range=point$back),
      fut_ret=sapply(etf_rets[point$fwd, ], sum))
)  # end lapply
head(period_stats[[1]])
## # calculate pnl for a given period
## pnl_period <- function(period_stat, de_mean=FALSE) {
##   weights <- period_stat[, "ret"]/
##     period_stat[, "risk"]
##   weights <- weights - de_mean*mean(weights)
##   weights <- weights/sum(abs(weights))
## c(sum(period_stat[, "fut_ret"]*weights), weights)
## }  # end pnl_period
## 
## # calculate pnls over all windows
## pnl_xts <- t(sapply(period_stats, pnl_period))
## pnl_xts <- xts(pnl_xts,
##      order.by=index(etf_rets)
##  [end_points[win_dow:(length(end_points)-1)]]
##  )  # end xts
## colnames(pnl_xts)[1] <- "pnl"
## 
## # calculate transaction costs
## bid_offer <- 0.001  # 10 bps for liquid ETFs
## co_sts <- bid_offer*abs(diff(pnl_xts[, -1]))
## co_sts[1, ] <- 0
## co_sts <- rowSums(co_sts)
## pnl_xts[, "pnl"] <- pnl_xts[, "pnl"] - co_sts
## # plot cumulative pnl of strategy
## plot(cumsum(pnl_xts[, "pnl"]),
##   main=colnames(pnl_xts[, "pnl"]))
## portf_names <- c("VTI", "IEF", "DBC", "XLF", "VNQ", "XLP", "XLV", "XLU", "XLB", "XLE")
## # plot portfolio weights
## plot.zoo(pnl_xts[, portf_names], main="")
## # calculate xts of net beta
## betas <- c(1, etf_reg_stats[, 3])
## names(betas)[1] <- colnames(pnl_xts[, 2])
## # weights times betas
## betas <- pnl_xts[, -1] * betas
## betas <- xts(rowSums(betas),
##     order.by=index(pnl_xts))
## colnames(betas) <- "betas"
## plot.zoo(cbind(betas,
##     cumsum(etf_rets[, 1])[index(betas)]),
##     main="betas & VTI", xlab="")
## # create trading function
## tot_pnl <- function(win_dow) {
##   periods <- lapply(win_dow:(length(end_points)-1),
##     function(point)
## list(back=
## end_points[point-win_dow+1]:(end_points[point]-1),
##    fwd=end_points[point]:end_points[point+1])
##   )  # end sapply
##   period_stats <- lapply(periods,
##  function(point)
##    cbind(risk_ret_stats(range=point$back),
##      fut_ret=sapply(etf_rets[point$fwd, ], sum))
##   )  # end lapply
##   pnl_xts <- t(sapply(period_stats, pnl_period))
##   co_sts <- bid_offer*abs(diff(pnl_xts[, -1]))
##   co_sts <- rowSums(co_sts)
##   co_sts <- c(0, co_sts)
##   pnl_xts[, 1] <- pnl_xts[, 1] - co_sts
##   sum(pnl_xts[, 1])
## }  # end tot_pnl
## strat_profile <- sapply(4*(5:15), tot_pnl)
## plot(cbind(4*(5:15), strat_profile), t="l")
## # load ETF returns
## load(file="C:/Develop/data/etf_data.Rdata")
## library(quantmod)
## ### Perform pair-wise correlation analysis
## # Calculate correlation matrix
## corr_matrix <- cor(etf_rets)
## colnames(corr_matrix) <- colnames(etf_rets)
## rownames(corr_matrix) <- colnames(etf_rets)
## # Reorder the correlation matrix based on clusters
## # Calculate permutation vector
## library(corrplot)
## corr_order <- corrMatOrder(corr_matrix,
##         order="hclust",
##         hclust.method="complete")
## # Apply permutation vector
## corr_matrix_ordered <-
##   corr_matrix[corr_order, corr_order]
## # Plot the correlation matrix
## col3 <- colorRampPalette(c("red", "white", "blue"))
## corrplot(corr_matrix_ordered,
##     tl.col="black", tl.cex=0.8,
##     method="square", col=col3(8),
##     cl.offset=0.75, cl.cex=0.7,
##     cl.align.text="l", cl.ratio=0.25)
## # Draw rectangles on the correlation matrix plot
## corrRect.hclust(corr_matrix_ordered,
## k=13, method="complete", col="red")
## # Perform hierarchical clustering analysis
## data_dist <- as.dist(1-corr_matrix_ordered)
## data_cluster <- hclust(data_dist)
## plot(data_cluster,
##      main="Dissimilarity = 1-Correlation",
##      xlab="", ylab="")
par(oma=c(1, 0, 1, 0), mgp=c(2, 1, 0), mar=c(2, 1, 2, 1), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # set plot panels
# load ETF returns
load(file="C:/Develop/data/etf_data.Rdata")
### Perform principal component analysis PCA
etf_pca <- prcomp(etf_rets, center=TRUE, scale=TRUE)
barplot(etf_pca$sdev[1:10], 
  names.arg=colnames(etf_pca$rotation)[1:10], 
  las=3, ylab="STDEV", xlab="PCVec", 
  main="PCA Explain VAR")
# Show first three principal component loadings
head(etf_pca$rotation[,1:3], 3)
# Permute second principal component loadings by size
pca_vec2 <- as.matrix(
  etf_pca$rotation[order(etf_pca$rotation[, 2], 
  decreasing=TRUE), 2])
colnames(pca_vec2) <- "pca2"
head(pca_vec2, 3)
# The option las=3 rotates the names.arg labels
barplot(as.vector(pca_vec2), 
  names.arg=rownames(pca_vec2), 
  las=3, ylab="Loadings", 
  xlab="Symbol", main="Loadings pca2")
## par(oma=c(1, 0, 1, 0), mgp=c(2, 1, 0), mar=c(2, 1, 2, 1), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
## par(mfrow=c(3,1))  # set plot panels
## # get list of principal component vectors
## pca_vecs <- lapply(1:3, function(in_dex) {
##   pca_vec <- as.matrix(
##     etf_pca$rotation[
##     order(etf_pca$rotation[, in_dex],
##     decreasing=TRUE), in_dex])
##   colnames(pca_vec) <- paste0("pca", in_dex)
##   pca_vec
## })  # end sapply
## names(pca_vecs) <- c("pca1", "pca2", "pca3")
## # The option las=3 rotates the names.arg labels
## for (in_dex in 1:3) {
##   barplot(as.vector(pca_vecs[[in_dex]]),
##   names.arg=rownames(pca_vecs[[in_dex]]),
##   las=3, ylab="", xlab="",
##   main=paste("Loadings",
##     colnames(pca_vecs[[in_dex]])))
## }  # end for
library(PerformanceAnalytics)  # load package "PerformanceAnalytics"
# PC returns from rotation and scaled etf_rets
etf_rets_scaled <- apply(etf_rets, 2, scale)
pca_rets <- etf_rets_scaled %*% etf_pca$rotation
# "x" matrix contains time series of PC returns
dim(etf_pca$x)
class(etf_pca$x)
head(etf_pca$x[, 1:3], 3)
# convert PC matrix to xts and rescale to decimals
pca_rets <- xts(etf_pca$x/100, 
    order.by=index(etf_rets))
## library(PerformanceAnalytics)  # load package "PerformanceAnalytics"
## chart.CumReturns(
##   pca_rets[, 1:3], lwd=2, ylab="",
##   legend.loc="topright", main="")
## # add title
## title(main="ETF cumulative returns", line=-1)
library(PerformanceAnalytics)
# Calculate PC correlation matrix
corr_matrix <- cor(pca_rets)
colnames(corr_matrix) <- colnames(pca_rets)
rownames(corr_matrix) <- colnames(pca_rets)
corr_matrix[1:3, 1:3]
table.CAPM(Ra=pca_rets[, 1:3], 
    Rb=etf_rets[, "VTI"], scale=252)
## library(factorAnalytics)  # load package "factorAnalytics"
## # get documentation for package "factorAnalytics"
## packageDescription("factorAnalytics")  # get short description
## help(package="factorAnalytics")  # load help page
options(width=50)
library(factorAnalytics)  # load package "factorAnalytics"
# list all objects in "factorAnalytics"
ls("package:factorAnalytics")

# list all datasets in "factorAnalytics"
# data(package="factorAnalytics")

# remove factorAnalytics from search path
detach("package:factorAnalytics")
library(factorAnalytics)
# load ETF returns
load(file="C:/Develop/data/etf_data.Rdata")
# fit a three-factor model using PCA
factor_pca <- fitSfm(etf_rets, k=3)
head(factor_pca$loadings, 3)  # factor loadings
# factor realizations (time series)
head(factor_pca$factors)
# residuals from regression
factor_pca$residuals[1:3, 1:3]
library(factorAnalytics)
factor_pca$alpha  # estimated alphas
factor_pca$r2  # R-squared regression
# covariance matrix estimated by factor model
factor_pca$Omega[1:3, 4:6]
## library(factorAnalytics)
## load(file="C:/Develop/data/portf_optim.RData")
## plot(factor_pca, which.plot.group=3,
##      n.max=30, loop=FALSE)
## # ?plot.sfm
## library(PortfolioAnalytics)
## # plot factor cumulative returns
## chart.CumReturns(factor_pca$factors,
##     lwd=2, ylab="", legend.loc="topleft",
##     main="")
## 
## # plot time series of factor returns
## # plot(factor_pca, which.plot.group=2,
## #   loop=FALSE)
## # asset correlations "hclust" hierarchical clustering order
## plot(factor_pca, which.plot.group=7,
##      loop=FALSE, order="hclust",
##      method="ellipse")
## library(PortfolioAnalytics)
## # plot residual cumulative returns
## chart.CumReturns(
##   factor_pca$residuals[, c("IEF",
##             "DBC", "XLF")],
##   lwd=2, ylab="", legend.loc="topleft",
##   main="")
## library(PortfolioAnalytics)
## # plot residual histogram with normal curve
## plot(factor_pca, asset.name="VTI",
##      which.plot.single=8,
##      plot.single=TRUE, loop=FALSE,
##      xlim=c(-0.007, 0.007))
library(PortfolioAnalytics)
# residual Q-Q plot
plot(factor_pca, asset.name="VTI", 
     which.plot.single=9, 
     plot.single=TRUE, loop=FALSE)
## # SACF and PACF of residuals
## plot(factor_pca, asset.name="VTI",
##      which.plot.single=5,
##      plot.single=TRUE, loop=FALSE)
