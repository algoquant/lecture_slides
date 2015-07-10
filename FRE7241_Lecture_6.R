library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
thm <- knit_theme$get("acid")
knit_theme$set(thm)
options(width=50, dev='pdf')
str(optimize)
# objective function with multiple minima
object_ive <- function(in_put, param1=0.01) {
  sin(0.25*pi*in_put) + param1*(in_put-1)^2
}  # end object_ive
unlist(optimize(f=object_ive, interval=c(-4, 2)))
unlist(optimize(f=object_ive, interval=c(0, 8)))
options(width=60, dev='pdf')
## par(oma=c(1, 1, 1, 1), mgp=c(2, 1, 0), mar=c(5, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
## # plot objective function
## curve(expr=object_ive, type="l", xlim=c(-8, 9),
## xlab="", ylab="", lwd=2)
## title(main="Objective Function", line=-1)  # add title
# target vector of normal variables
target_vector <- rnorm(1000, mean=4, sd=2)
# objective function is log-likelihood
object_ive <- function(parm, target) {
  sum(2*log(parm[2]) + 
    ((target - parm[1])/parm[2])^2)
}  # end object_ive
# vectorize objective function
vec_objective <- Vectorize(
  FUN=function(mean, sd, target)
    object_ive(c(mean, sd), target),
  vectorize.args=c("mean", "sd")
)  # end Vectorize
# objective function on parameter grid
par_mean <- seq(1, 6, length=50)
par_sd <- seq(0.5, 3.0, length=50)
objective_grid <- outer(par_mean, par_sd, 
    vec_objective, target=target_vector)
rownames(objective_grid) <- round(par_mean, 2)
colnames(objective_grid) <- round(par_sd, 2)
objective_min <- which(
  objective_grid==min(objective_grid), 
  arr.ind=TRUE)
objective_min
objective_grid[objective_min]
objective_grid[(objective_min[, 1] + -1:1), 
         (objective_min[, 2] + -1:1)]
## # perspective plot of objective function
## persp(par_mean, par_sd, -objective_grid,
## theta=45, phi=30,
## shade=0.5,
## col=rainbow(50),
## border="green",
## main="objective function")
# initial parameters
par_init <- c(mean=0, sd=1)
# perform optimization quasi-Newton method
optim_run <- optim(par=par_init, 
       fn=object_ive, 
       target=target_vector,
       method="L-BFGS-B",
       upper=c(10, 10),
       lower=c(-10, 0.1))
# optimal parameters
optim_run$par
## # plot histogram
## histo_gram <- hist(target_vector, plot=FALSE)
## plot(histo_gram, freq=FALSE,
##      main="histogram of target vector")
## curve(expr=dnorm(x, mean=optim_run$par["mean"],
##            sd=optim_run$par["sd"]),
## add=TRUE, type="l", lwd=2, col="red")
## legend("topright", inset=0.0, cex=0.8, title=NULL,
##  leg="optimal parameters",
##  lwd=2, bg="white", col="red")
# target vector is mixture of normal distributions
target_vector <- c(rnorm(100, sd=1.0), 
             rnorm(100, mean=4, sd=1.0))
# objective function is log-likelihood
object_ive <- function(parm, target) {
  likelihood <- parm[1]/parm[3] * 
  dnorm((target-parm[2])/parm[3]) +
  (1-parm[1])/parm[5]*dnorm((target-parm[4])/parm[5])
  if(any(likelihood <= 0)) Inf else
    -sum(log(likelihood))
}  # end object_ive
# vectorize objective function
vec_objective <- Vectorize(
  FUN=function(mean, sd, w, m1, s1, target)
    object_ive(c(w, m1, s1, mean, sd), target),
  vectorize.args=c("mean", "sd")
)  # end Vectorize
# objective function on parameter grid
par_mean <- seq(3, 5, length=50)
par_sd <- seq(0.5, 1.5, length=50)
objective_grid <- outer(par_mean, par_sd, 
    vec_objective, target=target_vector,
    w=0.5, m1=2.0, s1=2.0)
rownames(objective_grid) <- round(par_mean, 2)
colnames(objective_grid) <- round(par_sd, 2)
objective_min <- which(objective_grid==
  min(objective_grid), arr.ind=TRUE)
objective_min
objective_grid[objective_min]
objective_grid[(objective_min[, 1] + -1:1), 
         (objective_min[, 2] + -1:1)]
## # perspective plot of objective function
## persp(par_mean, par_sd, -objective_grid,
## theta=45, phi=30,
## shade=0.5,
## col=rainbow(50),
## border="green",
## main="objective function")
# initial parameters
par_init <- c(weight=0.5, m1=0, s1=1, m2=2, s2=1)
# perform optimization
optim_run <- optim(par=par_init, 
      fn=object_ive, 
      target=target_vector,
      method="L-BFGS-B",
      upper=c(1,10,10,10,10),
      lower=c(0,-10,0.2,-10,0.2))
optim_run$par
## # plot histogram
## histo_gram <- hist(target_vector, plot=FALSE)
## plot(histo_gram, freq=FALSE,
##      main="histogram of target vector")
## fit_func <- function(x, parm) {
##   parm["weight"] * dnorm(x, mean=parm["m1"], sd=parm["s1"]) +
##     (1-parm["weight"]) * dnorm(x, mean=parm["m2"], sd=parm["s2"])
## }  # end fit_func
## curve(expr=fit_func(x, parm=optim_run$par), add=TRUE,
## type="l", lwd=2, col="red")
## legend("topright", inset=0.0, cex=0.8, title=NULL,
##  leg="optimal parameters",
##  lwd=2, bg="white", col="red")
load(file="C:/Develop/data/etf_data.Rdata")
# create list of symbols for optimized portfolio
sym_bols <- c("VTI", "VNQ", "DBC")
# create initial vector of portfolio weights
portf_weights <- rep(1, length(sym_bols))
names(portf_weights) <- sym_bols
# objective equal to minus Sharpe ratio
object_ive <- function(weights) {
  portf_ts <- etf_rets[, sym_bols] %*% weights
  -mean(portf_ts)/sd(portf_ts)
}  # end object_ive
# objective for equal weight portfolio
object_ive(portf_weights)
## par(oma=c(1, 1, 1, 1), mgp=c(2, 1, 0), mar=c(5, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
## # vectorize objective function with respect to third weight
## vec_objective <- Vectorize(
##   FUN=function(weight) object_ive(c(1, 1, weight)),
##   vectorize.args="weight"
## )  # end Vectorize
## # plot objective function with respect to third weight
## curve(expr=vec_objective,
##       type="l", xlim=c(-4.0, 1.0),
##       xlab=paste("weight of", names(portf_weights[3])),
##       ylab="", lwd=2)
## title(main="Objective Function", line=-1)  # add title
## # vectorize objective function
## vec_objective <- Vectorize(
##   FUN=function(w1, w2, w3)
##     object_ive(c(w1, w2, w3)),
##   vectorize.args=c("w2", "w3")
## )  # end Vectorize
## # create vectors of weights
## w2 <- seq(-5, 5, length=50)
## w3 <- seq(-5, 5, length=50)
## # calculate objective on 2-d (w2 x w3) parameter grid
## objective_grid <- outer(w2, w3, vec_objective, w1=1)
## rownames(objective_grid) <- round(w2, 2)
## colnames(objective_grid) <- round(w3, 2)
## # perspective plot of objective function
## persp(w2, w3, -objective_grid,
## theta=45, phi=30,
## shade=0.5,
## col=rainbow(50),
## border="green",
## main="objective function")
# optimization to find weights with maximum Sharpe ratio
optim_run <- optim(par=portf_weights, 
             fn=object_ive, 
             method="L-BFGS-B",
             upper=c(1.1, 10, 10),
             lower=c(0.9, -10, -10))
# optimal parameters
optim_run$par
# optimal Sharpe ratio
-object_ive(optim_run$par)
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
