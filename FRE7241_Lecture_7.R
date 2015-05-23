library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
thm <- knit_theme$get("acid")
knit_theme$set(thm)
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
## library(PortfolioAnalytics)  # load package "PortfolioAnalytics"
## # get documentation for package "PortfolioAnalytics"
## packageDescription("PortfolioAnalytics")  # get short description
## 
## help(package="PortfolioAnalytics")  # load help page
## 
## data(package="PortfolioAnalytics")  # list all datasets in "PortfolioAnalytics"
## 
## ls("package:PortfolioAnalytics")  # list all objects in "PortfolioAnalytics"
## 
## detach("package:PortfolioAnalytics")  # remove PortfolioAnalytics from search path
library(PortfolioAnalytics)
# load ETF returns
load(file="C:/Develop/data/etf_data.Rdata")
portf_names <- c("VTI", "IEF", "DBC", "XLF", 
  "VNQ", "XLP", "XLV", "XLU", "XLB", "XLE")
# initial portfolio to equal weights
portf_init <- rep(1/length(portf_names), 
            length(portf_names))
# named vector
names(portf_init) <- portf_names
# create portfolio object
portf_init <- portfolio.spec(
  assets=portf_init)
library(PortfolioAnalytics)
# add constraints
portf_maxSR <- add.constraint(
  portfolio=portf_init,  # initial portfolio
  type="weight_sum",  # constraint sum weights
  min_sum=0.9, max_sum=1.1)
# add constraints
portf_maxSR <- add.constraint(
  portfolio=portf_maxSR, 
  type="long_only")  # box constraint min=0, max=1
# add objectives
portf_maxSR <- add.objective(
  portfolio=portf_maxSR, 
  type="return",  # maximize mean return
  name="mean")
# add objectives
portf_maxSR <- add.objective(
  portfolio=portf_maxSR, 
  type="risk",  # minimize StdDev
  name="StdDev")
## load(file="C:/Develop/data/portf_optim.RData")
## library(PortfolioAnalytics)
## # perform optimization of weights
## maxSR_DEOpt <- optimize.portfolio(
##   R=etf_rets[, portf_names],  # specify returns
##   portfolio=portf_maxSR,  # specify portfolio
##   optimize_method="DEoptim", # use DEoptim
##   maxSR=TRUE,  # maximize Sharpe
##   trace=TRUE, traceDE=0)
## # plot optimization
## chart.RiskReward(maxSR_DEOpt,
##   risk.col="StdDev",
##   return.col="mean")
library(PortfolioAnalytics)
load(file="C:/Develop/data/portf_optim.RData")
maxSR_DEOpt$weights
maxSR_DEOpt$objective_measures$mean[1]
maxSR_DEOpt$objective_measures$StdDev[[1]]
## library(PortfolioAnalytics)
## # plot optimization
## chart.RiskReward(maxSR_DEOpt,
##   risk.col="StdDev",
##   return.col="mean")
## 
## # plot risk/ret points in portfolio scatterplot
## risk_ret_points <- function(rets=etf_rets,
##   risk=c("sd", "ETL"), sym_bols=c("VTI", "IEF")) {
##   risk <- match.arg(risk)  # match to arg list
##   if (risk=="ETL") {
##     stopifnot(
## "package:PerformanceAnalytics" %in% search() ||
## require("PerformanceAnalytics", quietly=TRUE))
##   }  # end if
##   risk <- match.fun(risk)  # match to function
##   risk_ret <- t(sapply(rets[, sym_bols],
##      function(x_ts)
##  c(ret=mean(x_ts), risk=abs(risk(x_ts)))))
##   points(x=risk_ret[, "risk"], y=risk_ret[, "ret"],
##    col="red", lwd=3, pch=21)
##   text(x=risk_ret[, "risk"], y=risk_ret[, "ret"],
##  labels=rownames(risk_ret), col="red",
##  lwd=2, pos=4)
## }  # end risk_ret_points
## 
## risk_ret_points()
## library(PortfolioAnalytics)
## plot_portf <- function(portfolio,
##       rets_data=etf_rets) {
##   portf_weights <- portfolio$weights
##   portf_names <- names(portf_weights)
##   # calculate xts of portfolio
##   portf_max <- xts(
##     rets_data[, portf_names] %*% portf_weights,
##     order.by=index(rets_data))
##   colnames(portf_max) <-
##     deparse(substitute(portfolio))
##   graph_params <- par(oma=c(1, 0, 1, 0),
##     mgp=c(2, 1, 0), mar=c(2, 1, 2, 1),
##     cex.lab=0.8, cex.axis=1.0,
##     cex.main=0.8, cex.sub=0.5)
##   layout(matrix(c(1,2), 2),
##     widths=c(1,1), heights=c(1,3))
##   barplot(portf_weights, names.arg=portf_names,
##     las=3, ylab="", xlab="Symbol", main="")
##   title(main=paste("Loadings",
##           colnames(portf_max)), line=-1)
##   chart.CumReturns(
##     cbind(portf_max, rets_data[, c("IEF", "VTI")]),
##     lwd=2, ylab="", legend.loc="topleft", main="")
##   title(main=paste0(colnames(portf_max),
##               ", IEF, VTI"), line=-1)
##   par(graph_params)  # restore original parameters
##   invisible(portf_max)
## }  # end plot_portf
## maxSR_DEOpt_xts <- plot_portf(portfolio=maxSR_DEOpt)
## library(PortfolioAnalytics)
## # add leverage constraint abs(weight_sum)
## portf_maxSRN <- add.constraint(
##   portfolio=portf_init, type="leverage",
##   min_sum=0.9, max_sum=1.1)
## # add box constraint long/short
## portf_maxSRN <- add.constraint(
##   portfolio=portf_maxSRN,
##   type="box", min=-0.2, max=0.2)
## 
## # add objectives
## portf_maxSRN <- add.objective(
##   portfolio=portf_maxSRN,
##   type="return",  # maximize mean return
##   name="mean")
## # add objectives
## portf_maxSRN <- add.objective(
##   portfolio=portf_maxSRN,
##   type="risk",  # minimize StdDev
##   name="StdDev")
## load(file="C:/Develop/data/portf_optim.RData")
## library(PortfolioAnalytics)
## # perform optimization of weights
## maxSRN_DEOpt <- optimize.portfolio(
##   R=etf_rets[, portf_names],  # specify returns
##   portfolio=portf_maxSRN,  # specify portfolio
##   optimize_method="DEoptim", # use DEoptim
##   maxSR=TRUE,  # maximize Sharpe
##   trace=TRUE, traceDE=0)
## # plot optimization
## chart.RiskReward(maxSRN_DEOpt,
##   risk.col="StdDev",
##   return.col="mean",
##   xlim=c(
##     maxSR_DEOpt$objective_measures$StdDev[[1]]-0.001,
##     0.016))
##   points(x=maxSR_DEOpt$objective_measures$StdDev[[1]],
##    y=maxSR_DEOpt$objective_measures$mean[1],
##    col="green", lwd=3, pch=21)
##   text(x=maxSR_DEOpt$objective_measures$StdDev[[1]],
##    y=maxSR_DEOpt$objective_measures$mean[1],
##  labels="maxSR", col="green",
##  lwd=2, pos=4)
## # plot risk/ret points in portfolio scatterplot
## risk_ret_points()
library(PortfolioAnalytics)
load(file="C:/Develop/data/portf_optim.RData")
maxSRN_DEOpt$weights
maxSRN_DEOpt$objective_measures$mean[1]
maxSRN_DEOpt$objective_measures$StdDev[[1]]
## library(PortfolioAnalytics)
## maxSRN_DEOpt_xts <-
##   plot_portf(portfolio=maxSRN_DEOpt)
## library(PerformanceAnalytics)
## load(file="C:/Develop/data/portf_optim.RData")
## chart.CumReturns(
##   cbind(maxSR_DEOpt_xts, maxSRN_DEOpt_xts),
##   lwd=2, ylab="",
##   legend.loc="topleft", main="")
library(PerformanceAnalytics)
load(file="C:/Develop/data/portf_optim.RData")
rbind(maxSR_DEOpt$weights, maxSRN_DEOpt$weights)
c(maxSR_DEOpt$objective_measures$mean,
maxSRN_DEOpt$objective_measures$mean)
c(maxSR_DEOpt$objective_measures$StdDev[[1]],
maxSRN_DEOpt$objective_measures$StdDev[[1]])
## library(PortfolioAnalytics)
## # add constraints
## portf_maxSTARR <- add.constraint(
##   portfolio=portf_init,  # initial portfolio
##   type="weight_sum",  # constraint sum weights
##   min_sum=0.9, max_sum=1.1)
## # add constraints
## portf_maxSTARR <- add.constraint(
##   portfolio=portf_maxSTARR,
##   type="long_only")  # box constraint min=0, max=1
## # add objectives
## portf_maxSTARR <- add.objective(
##   portfolio=portf_maxSTARR,
##   type="return",  # maximize mean return
##   name="mean")
## # add objectives
## portf_maxSTARR <- add.objective(
##   portfolio=portf_maxSTARR,
##   type="risk",  # minimize StdDev
##   name="ES")
## load(file="C:/Develop/data/portf_optim.RData")
## library(PortfolioAnalytics)
## # perform optimization of weights
## maxSTARR_DEOpt <- optimize.portfolio(
##   R=etf_rets[, portf_names],  # specify returns
##   portfolio=portf_maxSTARR,  # specify portfolio
##   optimize_method="DEoptim", # use DEoptim
##   maxSTARR=TRUE,  # maximize STARR
##   trace=TRUE, traceDE=0)
## 
## # plot optimization
## chart.RiskReward(maxSTARR_DEOpt,
##   risk.col="ES",
##   return.col="mean")
## # plot risk/ret points in portfolio scatterplot
## risk_ret_points(risk="ETL")
library(PortfolioAnalytics)
load(file="C:/Develop/data/portf_optim.RData")
maxSTARR_DEOpt$weights
maxSTARR_DEOpt$objective_measures$mean[1]
maxSTARR_DEOpt$objective_measures$ES[[1]]
## library(PortfolioAnalytics)
## maxSTARR_DEOpt_xts <-
##   plot_portf(portfolio=maxSTARR_DEOpt)
## library(PerformanceAnalytics)
## load(file="C:/Develop/data/portf_optim.RData")
## chart.CumReturns(
##   cbind(maxSR_DEOpt_xts, maxSTARR_DEOpt_xts),
##   lwd=2, ylab="",
##   legend.loc="topleft", main="")
library(PerformanceAnalytics)
load(file="C:/Develop/data/portf_optim.RData")
rbind(maxSR_DEOpt$weights, maxSTARR_DEOpt$weights)
c(maxSR_DEOpt$objective_measures$mean,
maxSTARR_DEOpt$objective_measures$mean)
c(maxSR_DEOpt$objective_measures$StdDev[[1]],
maxSTARR_DEOpt$objective_measures$ES[[1]])
## library(PortfolioAnalytics)
## # plot the efficient frontier
## chart.EfficientFrontier(maxSR_DEOpt,
##           match.col="StdDev",
##           n.portfolios=15, type="l")
## points(x=maxSRN_DEOpt$objective_measures$StdDev[[1]],
##    y=maxSRN_DEOpt$objective_measures$mean[1],
##    col="green", lwd=3, pch=21)
## text(x=maxSRN_DEOpt$objective_measures$StdDev[[1]],
##    y=maxSRN_DEOpt$objective_measures$mean[1],
##  labels="maxSRN", col="green",
##  lwd=2, pos=4)
## library(PortfolioAnalytics)
## # add constraints
## portf_minES <- add.constraint(
##   portfolio=portf_init,  # initial portfolio
##   type="weight_sum",  # constraint sum weights
##   min_sum=0.9, max_sum=1.1)
## # add constraints
## portf_minES <- add.constraint(
##   portfolio=portf_minES,
##   type="long_only")  # box constraint min=0, max=1
## # add objectives
## portf_minES <- add.objective(
##   portfolio=portf_minES,
##   type="risk",  # minimize ES
##   name="ES")
## load(file="C:/Develop/data/portf_optim.RData")
## library(PortfolioAnalytics)
## # perform optimization of weights
## minES_ROI <- optimize.portfolio(
##   R=etf_rets[, portf_names],  # specify returns
##   portfolio=portf_minES,  # specify portfolio
##   optimize_method="ROI", # use ROI
##   trace=TRUE, traceDE=0)
## 
## # plot optimization
## chart.RiskReward(maxSTARR_DEOpt,
##   risk.col="ES",
##   return.col="mean")
##   points(x=minES_ROI$objective_measures$ES[[1]],
##    y=mean(minES_ROI_xts),
##    col="green", lwd=3, pch=21)
##   text(x=minES_ROI$objective_measures$ES[[1]],
##    y=mean(minES_ROI_xts),
##  labels="minES", col="green",
##  lwd=2, pos=4)
## # plot risk/ret points in portfolio scatterplot
## risk_ret_points(risk="ETL")
library(PortfolioAnalytics)
load(file="C:/Develop/data/portf_optim.RData")
minES_ROI$weights
minES_ROI$objective_measures$ES[[1]]
## library(PortfolioAnalytics)
## minES_ROI_xts <-
##   plot_portf(portfolio=minES_ROI)
## library(PerformanceAnalytics)
## load(file="C:/Develop/data/portf_optim.RData")
## chart.CumReturns(
##   cbind(maxSR_DEOpt_xts, minES_ROI_xts),
##   lwd=2, ylab="",
##   legend.loc="topleft", main="")
library(PerformanceAnalytics)
load(file="C:/Develop/data/portf_optim.RData")
rbind(maxSR_DEOpt$weights, minES_ROI$weights)
c(maxSR_DEOpt$objective_measures$mean,
minES_ROI$objective_measures$mean)
c(maxSR_DEOpt$objective_measures$StdDev[[1]],
minES_ROI$objective_measures$ES[[1]])
## load(file="C:/Develop/data/portf_optim.RData")
## library(PortfolioAnalytics)
## # perform optimization of weights
## maxSR_DEOpt <- optimize.portfolio(
##   R=etf_rets["/2011", portf_names],  # specify returns
##   portfolio=portf_maxSR,  # specify portfolio
##   optimize_method="DEoptim", # use DEoptim
##   maxSR=TRUE,  # maximize Sharpe
##   trace=TRUE, traceDE=0)
## weights_1h <- maxSR_DEOpt$weights
## 
## # plot optimization
## maxSR_DEOpt_xts <-
##   plot_portf(portfolio=maxSR_DEOpt)
## load(file="C:/Develop/data/portf_optim.RData")
## library(PortfolioAnalytics)
## # perform optimization of weights
## maxSR_DEOpt <- optimize.portfolio(
##   R=etf_rets["2011/", portf_names],  # specify returns
##   portfolio=portf_maxSR,  # specify portfolio
##   optimize_method="DEoptim", # use DEoptim
##   maxSR=TRUE,  # maximize Sharpe
##   trace=TRUE, traceDE=0)
## weights_2h <- maxSR_DEOpt$weights
## 
## # plot optimization
## maxSR_DEOpt_xts <-
##   plot_portf(portfolio=maxSR_DEOpt)
options(width=50)
par(oma=c(1, 0, 1, 0), mgp=c(2, 1, 0), mar=c(2, 1, 2, 1), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # set plot panels
weights_1h
weights_2h
weights_1h - weights_2h
barplot(weights_1h, 
  names.arg=names(weights_1h), 
  las=3, ylab="", xlab="", 
  main="Portfolio Weights First Half")
barplot(weights_2h, 
  names.arg=names(weights_2h), 
  las=3, ylab="", xlab="", 
  main="Portfolio Weights Second Half")
