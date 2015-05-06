

library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=600, dev='pdf')
thm <- knit_theme$get("acid")
knit_theme$set(thm)



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



load(file="C:/Develop/data/etf_data.Rdata")
load(file="C:/Develop/data/portf_optim.RData")
library(PortfolioAnalytics)
# perform optimization of weights
maxSR_DEOpt <- optimize.portfolio(
  R=etf_rets["/2011", portf_names],  # specify returns
  portfolio=portf_maxSR,  # specify portfolio
  optimize_method="DEoptim", # use DEoptim
  maxSR=TRUE,  # maximize Sharpe
  trace=TRUE, traceDE=0)
weights_1h <- maxSR_DEOpt$weights

# plot optimization
maxSR_DEOpt_xts <- 
  plot_portf(portfolio=maxSR_DEOpt)



load(file="C:/Develop/data/portf_optim.RData")
library(PortfolioAnalytics)
# perform optimization of weights
maxSR_DEOpt <- optimize.portfolio(
  R=etf_rets["2011/", portf_names],  # specify returns
  portfolio=portf_maxSR,  # specify portfolio
  optimize_method="DEoptim", # use DEoptim
  maxSR=TRUE,  # maximize Sharpe
  trace=TRUE, traceDE=0)
weights_2h <- maxSR_DEOpt$weights

# plot optimization
maxSR_DEOpt_xts <- 
  plot_portf(portfolio=maxSR_DEOpt)



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


