########################
### contains scripts for lectures
########################


rm(list=ls())  # remove all

options(max.print=80)
options(digits=3)

par(new=TRUE)  # allow new plot on same chart
par(las=1)  # set text printing to "horizontal"

library(zoo)
# good package loading script inside functions
stopifnot("package:xts" %in% search() || require("xts", quietly=TRUE))




### seed_random() returns the pseudo-random generating function random_generator
# this version is with for loop instead of recursion
# the formal argument 'seed' persists in the evaluation environment of seed_random
seed_random <- function(seed) {  # seed must be an integer
  random_number <- as.numeric(paste0('0.', seed))  # initialize
  # anon function returns a vector of pseudo-random numbers of length length_rand
  function(length_rand=1) {
    rand_vector <- numeric(length_rand)
    for (inter in 1:length_rand) {
      # logistic map
      random_number <<- 4*random_number*(1 - random_number)
      rand_vector[inter] <- random_number
    }  # end for
    rand_vector
  }  # end anon
}  # end seed_random

# create a random number generating function and set seed
make_random <- seed_random(88)
make_random(10)  #  calculate vector of 10 pseudo-random numbers
ls(environment(make_random))  # list objects in scope of make_random

###



########################
### dates and time series



########################
### regression




########################
### plotting ggplot2

library(car)
# qqPlot with t-quantiles
qqPlot(dax_rets, distribution="t", df=5, ylab="DAX Returns", envelope=FALSE)
# Box Plots


### autoplot
autoplot(object=ar_zoo,  # plot AR returns
         main="Autoregressive process (phi=0.2)", 
         facets=Series ~ .) + facet_grid(Series ~ ., scales="free_y") +
  xlab("") + ylab("") + 
  theme(legend.position=c(0.1, 0.5), 
        plot.title=element_text(vjust=-2.0), 
        plot.margin=unit(c(-0.5, 0.0, -0.5, 0.0), "cm"), 
        plot.background=element_blank(),
        axis.text.y=element_blank())

autoplot(object=ar_zoo, 
         facets="Series ~ .", 
         main="Autoregressive process (phi=0.2)") + 
  facet_grid("Series ~ .", scales="free_y") +
  xlab("") + ylab("") + 
  theme(
    legend.position="none", 
    #  plot.title=element_text(vjust=-1.0), 
    #  plot.margin=unit(c(-0.5, 0.0, -0.5, 0.0), "cm"), 
    plot.background=element_blank(),
    axis.text.y=element_blank())


ar_data_frame <- as.data.frame(ar_zoo)
ggplot(data=ar_zoo, mapping=aes(x=index(ar_zoo), y=ar_zoo[, 2])) + geom_line()
ggplot(data=ar_data_frame, mapping=aes(x=rownames(ar_data_frame), y=ar_data_frame[, 2])) + geom_line()



###

# autoplot.zoo with some examples
# http://www.inside-r.org/packages/cran/zoo/docs/autoplot.zoo

x.Date <- as.Date(paste(2003, 02, c(1, 3, 7, 9, 14), sep="-"))
x <- zoo(rnorm(5), x.Date)
xlow <- x - runif(5)
xhigh <- x + runif(5)
z <- cbind(x, xlow, xhigh)

# univariate plot
autoplot(x)

# multivariate plotting in multiple or single panels
# multiple panels + no legend
autoplot(z)
# by rows + legend + series-dependent color/linetype
autoplot(z, facets="Series ~ .")
# by rows + no legend
autoplot(z, facets="Series ~ .") + theme(legend.position="none")
# by columns + no legend
autoplot(z, facets=". ~ Series") + theme(legend.position="none")
# single panel + no legend
autoplot(z, facets=NULL) + theme(legend.position="none")

autoplot(as.zoo(EuStockMarkets))

# point plot
autoplot(z, geom="point")
# plot with connected points
autoplot(z, facets=NULL) + geom_point()
# b&w plot
autoplot(z, facets=NULL) + scale_colour_grey() + theme_bw()

autoplot(z) +
  aes(colour = NULL, linetype = NULL) + 
  facet_grid("Series ~ .", scales = "free_y")



########################
### functions



########################
### numerical methods



########################
### optimization examples



########################
### optimization generic

### single variable optimization
# ?optimize
### generic optimization
# ?optim


### optimization one-dim example: fit normal variables

# target vector is histogram of normal distribution
histo_gram <- hist(rnorm(100, mean=4, sd=2), 
                   main="histogram of normal variables")  # end hist
target_vector <- histo_gram$density
target_vector <- rnorm(100, mean=4, sd=2)
# objective function is log-likelihood
object_ive <- function(parm, target) {
  #  cat(c(parm[1], parm[2]), "\n")
  #  -sum(log(max(dnorm(target, mean=parm[1], sd=parm[2]), 0.01)))
  sum(2*log(parm[2]) + ((target - parm[1])/parm[2])^2)
}  # end object_ive

objective_grid <- (-20:60)/10
plot(x=objective_grid, 
     y=sapply(objective_grid, function(parm)
       object_ive(c(parm, 0.1), 
                  target_vector)),
     type="l")
objective_grid <- (10:40)/10
plot(x=objective_grid, 
     y=sapply(objective_grid, function(parm)
       object_ive(c(4.0, parm), 
                  target_vector)),
     type="l")

# initial parameters
par_init <- c(m=0, s=2)

# perform optimization
optim_run <- optim(par=par_init, 
                   fn=object_ive, 
                   target=target_vector,
                   method="L-BFGS-B",
                   upper=c(10,10),
                   lower=c(-10,1))
str(optim_run)
optim_run$par
optim_run$convergence


### optimization one-dim example: fit mixture of normal variables

# target vector is histogram of mixture of normal distributions
target_vector <- c(rnorm(100, sd=1.0), rnorm(100, mean=4, sd=1.0))
histo_gram <- hist(target_vector, 
                   main="mixture of normal distributions")  # end hist
target_vector <- histo_gram$density
# objective function is log-likelihood of mixture
object_ive <- function(parm, target) {
  #     parm[1]*sum(2*log(parm[3]) + ((target - parm[2])/parm[3])^2) +
  #     (1-parm[1])*sum(2*log(parm[5]) + ((target - parm[4])/parm[5])^2)
  #   likelihood <- parm[1]/parm[3] * dnorm(target, mean=parm[2], sd=parm[3]) +
  #     (1-parm[1])/parm[5] * dnorm(target, mean=parm[4], sd=parm[5])
  likelihood <- parm[1]/parm[3] * dnorm((target - parm[2])/parm[3]) +
    (1-parm[1])/parm[5] * dnorm((target - parm[4])/parm[5])
  if(any(likelihood <= 0))
    Inf
  else
    -sum(log(likelihood))
}  # end object_ive

objective_grid <- (1:9)/10
plot(x=objective_grid, 
     y=sapply(objective_grid, function(parm)
       object_ive(c(parm, 0, 1.0, 4, 1.0), 
                  target_vector)),
     type="l")
objective_grid <- (-20:20)/10
objective_grid <- (5:40)/10
plot(x=objective_grid, 
     y=sapply(objective_grid, function(parm)
       object_ive(c(0.5, 1, parm, 4, 1), 
                  target_vector)),
     type="l")

# initial parameters
par_init <- c(weight=0.5, m1=0, s1=1, m2=4, s2=1)

# perform optimization
optim_run <- optim(par=par_init, 
                   fn=object_ive, 
                   target=target_vector,
                   method="L-BFGS-B",
                   upper=c(1,2,2,10,2),
                   lower=c(0,-2,0.2,-1,0.2))
str(optim_run)
optim_run$par
optim_run$convergence


fit_func <- function(x, parm) {
  parm["weight"] * dnorm(x, mean=parm["m1"], sd=parm["s1"]) + 
    (1 - parm["weight"]) * dnorm(x, mean=parm["m2"], sd=parm["s2"])
}  # end fit_func

curve(expr=fit_func(x, parm=optim_run$par), add=TRUE,
      xlim=c(-5, 7), type="l", lwd=2, col="red")




########################
### portfolio optimization

### PortfolioAnalytics

library(PerformanceAnalytics)
library(TTR)
require(xts)
library(PortfolioAnalytics)
library(DEoptim)

# load ETF returns
load(file="C:/Develop/data/etf_data.Rdata")


### portfolio setup
portf_names <- c("VTI", "IEF", "DBC", "XLF", "VNQ", "XLP", "XLV", "XLU", "XLB", "XLE")
# portfolio equal weights
portf_init <- rep(0.1, length(portf_names))
names(portf_init) <- portf_names  # named vector
portf_init <- portfolio.spec(assets=portf_init)
# args(portfolio.spec)
str(portf_init)
portf_init$assets



########################
### portfolio visualization

plot_portf <- function(portfolio, rets_data=etf_rets) {
  portf_weights <- portfolio$weights
  portf_names <- names(portf_weights)
  # calculate xts of portfolio
  portf_max <- xts(rets_data[, portf_names] %*% portf_weights, 
                   order.by=index(rets_data))
  colnames(portf_max) <- "best portf"
  graph_params <- par(oma=c(1, 0, 1, 0), mgp=c(2, 1, 0), mar=c(2, 1, 2, 1), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
  layout(matrix(c(1,2), 2), widths=c(1,1), heights=c(1,3))
  barplot(portf_weights, 
          names.arg=portf_names, 
          las=3, ylab="", 
          xlab="Symbol", main="")
  title(main="Loadings best portf", line=-1)
  chart.CumReturns(cbind(portf_max, rets_data[, c("IEF", "VTI")]), 
                   lwd=2, ylab="", legend.loc="topleft", 
                   main="")
  title(main="best portf, IEF, VTI", line=-1)
  par(graph_params)  # restore original parameters
  invisible(portf_max)
}  # end plot_portf



########################
### rbr presentation

### add constraints

# add constraint such that the portfolio weights sum to 0*
portf_init <- add.constraint(portf_init, type="weight_sum",
                             min_sum=-0.01, max_sum=0.01)
# add box constraint such that no asset can have a weight of greater than
# 20% or less than -20%
portf_init <- add.constraint(portf_init, type="box", min=-0.2, max=0.2)
# add constraint such that we have at most 20 positions
portf_init <- add.constraint(portf_init, type="position_limit", max_pos=20)
# add constraint such that the portfolio beta is between -0.25 and 0.25
betas <- t(CAPM.beta(etf_rets, market, Rf))
portf_init <- add.constraint(portf_init, type="factor_exposure", B=betas,
                             lower=-0.25, upper=0.25)

### add objectives

# add objective to maximize portfolio return with a target of 0.0015
portf_objective <- add.objective(portf_init, type="return", name="mean",
                                 target=0.0015)
# add objective to minimize portfolio StdDev with a target of 0.02
portf_objective <- add.objective(portf_objective, type="risk", name="StdDev",
                                 target=0.001)


########################
### optimize.portfolio using random portfolios

# generate random portfolios
portf_rand <- random_portfolios(portf_init, 10000, "sample")

# perform optimization using random portfolios
portf_optim <- optimize.portfolio(R=etf_rets[, portf_names], portfolio=portf_objective,
                                  optimize_method="random", rp=portf_rand,
                                  trace=TRUE)

plot(portf_optim, main="Dollar Neutral Portfolio", risk.col="StdDev", neighbors=10)

chart.EfficientFrontier(portf_optim, match.col="StdDev", n.portfolios=25, type="l")



########################
### optimize.portfolio maxSR

# Maximizing Sharpe Ratio can be formulated as a quadratic programming 
# problem and solved very quickly using optimize_method="ROI". Although "StdDev"
# was specified as an objective, the quadratic programming problem uses the 
# variance-covariance matrix in the objective function.

# The default action if "mean" and "StdDev" are specified as objectives with
# optimize_method="ROI" is to maximize quadratic utility. If we want to maximize
# Sharpe Ratio, we need to pass in maxSR=TRUE to optimize.portfolio.

# add constraints
portf_maxSR <- add.constraint(portfolio=portf_init, type="full_investment")
portf_maxSR <- add.constraint(portfolio=portf_maxSR, type="long_only")

# add objectives
portf_maxSR <- add.objective(portfolio=portf_maxSR, type="return", name="mean")
portf_maxSR <- add.objective(portfolio=portf_maxSR, type="risk", name="StdDev")


maxSR_ROI <- optimize.portfolio(
  R=etf_rets[, portf_names], 
  portfolio=portf_maxSR, 
  optimize_method="ROI", 
  maxSR=TRUE, 
  trace=TRUE)


# visualize
plot_portf(portfolio=maxSR_ROI)
chart.RiskReward(maxSR_ROI, risk.col="StdDev", return.col="mean")



########################
### optimize.portfolio maxSTARR

# add constraints
portf_maxSTARR <- add.constraint(portfolio=portf_init, type="full_investment")
portf_maxSTARR <- add.constraint(portfolio=portf_maxSTARR, type="long_only")

# add objectives
portf_maxSTARR <- add.objective(portfolio=portf_maxSTARR, type="return", name="mean")
portf_maxSTARR <- add.objective(portfolio=portf_maxSTARR, type="risk", name="ES",
                                arguments=list(p=0.925))

# perform optimization using ROI
maxSTARR_ROI <- optimize.portfolio(R=etf_rets[, portf_names], portfolio=portf_maxSTARR, 
                                   optimize_method="ROI",
                                   trace=TRUE)
maxSTARR_ROI

# visualize
plot_portf(portfolio=maxSTARR_ROI)
chart.RiskReward(maxSTARR_ROI, risk.col="ES", return.col="mean")


# for random portfolios and DEoptim, the leverage constraints should be relaxed slightly
portf_maxSTARR$constraints[[1]]$min_sum=0.9
portf_maxSTARR$constraints[[1]]$max_sum=1.1

# random portfolios optimization
maxSTARR_RP <- optimize.portfolio(R=etf_rets[, portf_names], portfolio=portf_maxSTARR, 
                                  optimize_method="random",
                                  search_size=2000,
                                  trace=TRUE)
maxSTARR_RP
# visualize
plot_portf(portfolio=maxSTARR_RP)
chart.RiskReward(maxSTARR_RP, risk.col="ES", return.col="mean")

# Use DEoptim to run the optimization.
maxSTARR_DEOpt <- optimize.portfolio(R=etf_rets[, portf_names], portfolio=portf_maxSTARR, 
                                     optimize_method="DEoptim",
                                     search_size=2000,
                                     trace=TRUE)
maxSTARR_DEOpt
chart.RiskReward(maxSTARR_DEOpt, risk.col="ES", return.col="mean")



########################
### efficient frontier (without optimize.portfolio)


### this doesn't produce good scatterplot

portf_comb <- combine.portfolios(
  list(maxSR=portf_maxSR, maxSRN=portf_maxSRN))

maxSR_comb <- optimize.portfolio(
  R=etf_rets[, portf_names],  # specify returns
  portfolio=portf_comb,  # specify portfolio
  optimize_method="DEoptim", # use DEoptim
  maxSR=TRUE,  # maximize Sharpe
  trace=TRUE, traceDE=0)

chart.RiskReward(
  maxSR_comb,
  risk.col="StdDev",
  return.col="mean")



########################
### optimize.portfolio

# doesn't work
# produces something - but no way to get it out
portfolio_rolling <- optimize.portfolio.rebalancing(
  R=etf_rets[, portf_names],
  portfolio=portf_maxSR,
  optimize_method="DEoptim",
  rebalance_on="months",
  trailing_periods=12,
  training_period=12,
  trace=TRUE,
  traceDE=0
)



########################
### efficient frontier (without optimize.portfolio)

# add constraints
portf_eff <- add.constraint(portfolio=portf_init, type="weight_sum", min_sum=0.9, max_sum=1.1)
portf_eff <- add.constraint(portfolio=portf_eff, type="box", min=-0.3, max=0.3)

# add objectives for mean-variance portfolio
portf_eff <- add.objective(portfolio=portf_eff, type="risk", name="var", risk_aversion=10)
portf_eff <- add.objective(portfolio=portf_eff, type="return", name="mean")

# Compute the mean-variance efficient frontier
efficient_front <- create.EfficientFrontier(R=etf_rets[, portf_names], portfolio=portf_eff, type="mean-StdDev")
chart.EfficientFrontier(efficient_front, match.col="StdDev", n.portfolios=5, type="l")

efficient_front
summary(efficient_front, digits=2)
# efficient_front$frontier

# calculate best portfolio on efficient frontier
mean_var <- efficient_front$frontier[, 1:2]
mean_var <- cbind(mean_var, mean_var[, 1]/mean_var[, 2])
colnames(mean_var) <- c(colnames(mean_var)[1:2], "SR")
portf_best <- which.max(mean_var[, "SR"])

# calculate xts of best portfolio
portf_weights <- efficient_front$frontier[portf_best, -(1:3)]
portf_max <- etf_rets[, portf_names] %*% portf_weights
portf_max <- xts(portf_max, order.by=index(etf_rets))
colnames(portf_max) <- "best portf"

barplot(portf_weights, 
        names.arg=portf_names, 
        las=3, ylab="Loadings", 
        xlab="Symbol", main="Loadings efficient frontier")

chart.CumReturns(cbind(portf_max, etf_rets[, c("IEF", "VTI")]), lwd=2, ylab="", legend.loc="topleft", main="best portf, IEF, VTI")


### charts of efficient frontier

# The RAR.text argument can be used for the risk-adjusted-return name on the 
# legend, by default it is 'Modified Sharpe Ratio'.
chart.EfficientFrontier(efficient_front, match.col="StdDev", type="l", 
                        RAR.text="Sharpe Ratio", pch=4)

# The tangency portfolio and line are plotted by default, these can be 
# ommitted by setting rf=NULL.
chart.EfficientFrontier(efficient_front, match.col="StdDev", type="b", rf=NULL)

# The tangency line can be omitted with tangent.line=FALSE. The tangent 
# portfolio, risk-free rate and Sharpe Ratio are still included in the plot.
chart.EfficientFrontier(efficient_front, match.col="StdDev", type="l", tangent.line=FALSE)

# The assets can be omitted with chart.assets=FALSE.
chart.EfficientFrontier(efficient_front, match.col="StdDev", type="l", 
                        tangent.line=FALSE, chart.assets=FALSE)

# Just the names of the assets can be omitted with labels.assets=FALSE and the 
# plotting character can be changed with pch.assets.
chart.EfficientFrontier(efficient_front, match.col="StdDev", type="l", 
                        tangent.line=FALSE, labels.assets=FALSE, pch.assets=1)

# Chart the asset weights along the efficient frontier.
chart.Weights.EF(efficient_front, colorset=bluemono, match.col="StdDev")

# Chart the group weights along the efficient frontier.
chart.Weights.EF(efficient_front, colorset=bluemono, by.groups=TRUE, match.col="StdDev")

# The labels for Mean, Weight, and StdDev can be increased or decreased with
# the cex.lab argument. The default is cex.lab=0.8.
chart.Weights.EF(efficient_front, colorset=bluemono, match.col="StdDev", main="", cex.lab=1)



### older

# Set the MAR parameter
MAR <- 0.005 # ~6%/year

# Example 1 maximize Sortino Ratio
SortinoConstr <- constraint(assets = colnames(indexes[,1:4]), min = 0.05, max = 1, min_sum=.99, max_sum=1.01, weight_seq = generatesequence(by=.001))
SortinoConstr <- add.objective(constraints=SortinoConstr, type="return", name="SortinoRatio",  enabled=TRUE, arguments = list(MAR=MAR))
SortinoConstr <- add.objective(constraints=SortinoConstr, type="return", name="mean",  enabled=TRUE, multiplier=0) # multiplier 0 makes it availble for plotting, but not affect optimization

# Use random portfolio engine
SortinoResult <- optimize.portfolio(R=indexes[,1:4], constraints=SortinoConstr, optimize_method='random', search_size=2000, trace=TRUE, verbose=TRUE)
plot(SortinoResult, risk.col='SortinoRatio')

# Alternately, Use DEoptim engine
#SortinoResultDE <- optimize.portfolio(R=indexes[,1:4], constraints=SortinoConstr, optimize_method='DEoptim', search_size=2000, trace=TRUE, verbose=FALSE,strategy=6, parallel=TRUE) #itermax=55, CR=0.99, F=0.5,
#plot(SortinoResultDE, risk.col='SortinoRatio')

# Now rebalance quarterly
SortinoRebalance <- optimize.portfolio.rebalancing(R=indexes[,1:4], constraints=SortinoConstr, optimize_method="random", trace=TRUE, rebalance_on='quarters', trailing_periods=NULL, training_period=36, search_size=2000)



########################
### backtest strategy


### calculate risk_ret_stats for some symbols, over a range of dates
risk_ret_stats <- function(x_ts=etf_rets,  # daily returns
                           sym_bols=colnames(x_ts),  # names
                           range=index(x_ts),  # date range
                           ret="sum",  # return stat
                           risk="mad"  # risk stat
) {
  ret <- match.fun(ret)  # match to function
  risk <- match.fun(risk)  # match to function
  stats <- sapply(x_ts[range, sym_bols], function(ts) {
    c(ret=ret(ts), risk=risk(ts))
  })
  t(stats)
}  # end risk_ret_stats

# example
head(risk_ret_stats(range=
                      periods[[1]]$back))




### calculate weights and pnl for a given period
pnl_period <- function(period_stat, de_mean=FALSE) {
  weights <- period_stat[, "ret"]/period_stat[, "risk"]
  weights <- weights - de_mean*mean(weights)
  weights <- weights/sum(abs(weights))
  c(sum(period_stat[, "fut_ret"] * weights), weights)
}  # end pnl_period



### set up parameters

# rebalancing period
re_balance <- "weeks"
# create index of rebalancing period end points
end_points <- endpoints(etf_rets, on=re_balance)
end_points[1] <- 1
# look-back period in number of re_balance
win_dow <- 5
# create index of rebalancing periods
periods <- lapply(win_dow:(length(end_points)-1),
                  function(point)
                    list(back=end_points[point-win_dow+1]:(end_points[point]-1),
                         fwd=end_points[point]:end_points[point+1])
)  # end sapply



### main loop over windows

# calculate stats over overlaping period date windows
period_stats <- lapply(periods,
                       function(point)
                         cbind(risk_ret_stats(range=point$back),
                               fut_ret=sapply(etf_rets[point$fwd, ], sum))
)  # end lapply
head(period_stats[[1]])


### calculate weights and pnls

pnl_xts <- t(sapply(period_stats, pnl_period))
pnl_xts <- xts(pnl_xts,
               order.by=index(etf_rets)[end_points[win_dow:(length(end_points)-1)]])
colnames(pnl_xts)[1] <- "pnl"


# calculate transaction costs
bid_offer <- 0.001  # 10 bps for liquid ETFs
co_sts <- bid_offer*abs(diff(pnl_xts[, -1]))
co_sts[1, ] <- 0
co_sts <- rowSums(co_sts)
pnl_xts[, "pnl"] <- pnl_xts[, "pnl"] - co_sts
co_sts <- xts(co_sts,
              order.by=index(etf_rets)[end_points[win_dow:(length(end_points)-1)]])
colnames(co_sts) <- "costs"
plot(cumsum(co_sts))
sum(co_sts)



library(xtsExtra)
# plot cumulative pnl
plot(cumsum(pnl_xts[, "pnl"]), main=colnames(pnl_xts[, "pnl"]))
# plot weights
plot.zoo(pnl_xts[, portf_names], main="")
# calculate xts of net beta
betas <- c(1, etf_reg_stats[, 3])
names(betas)[1] <- colnames(pnl_xts[, 2])  # "VTI"
# weights times betas
betas <- pnl_xts[, -1] * betas
betas <- xts(rowSums(betas), order.by=index(pnl_xts))
colnames(betas) <- "betas"
plot(betas, t="l")
plot.zoo(cbind(betas, cumsum(etf_rets[, 1])[index(betas)]),
         main="betas & VTI", xlab="")
plot(cbind(betas, cumsum(etf_rets[, 1])[index(betas)]),
     main="betas & VTI", xlab="")



### create trading function

tot_pnl <- function(win_dow) {
  periods <- lapply(win_dow:(length(end_points)-1),
                    function(point)
                      list(back=end_points[point-win_dow+1]:(end_points[point]-1),
                           fwd=end_points[point]:end_points[point+1])
  )  # end sapply
  period_stats <- lapply(periods,
                         function(point)
                           cbind(risk_ret_stats(range=point$back),
                                 fut_ret=sapply(etf_rets[point$fwd, ], sum))
  )  # end lapply
  pnl_xts <- t(sapply(period_stats, pnl_period))
  co_sts <- bid_offer*abs(diff(pnl_xts[, -1]))
  co_sts <- rowSums(co_sts)
  co_sts <- c(0, co_sts)
  pnl_xts[, 1] <- pnl_xts[, 1] - co_sts
  sum(pnl_xts[, 1])
}  # end tot_pnl
tot_pnl(4)
strat_profile <- sapply(3:10, tot_pnl)
plot(cbind(3:10, strat_profile), t="l")



### simplified version for single asset

periods_back <- sapply(win_dow:(length(end_points)-1),
                       function(point)
                         end_points[point-win_dow+1]:(end_points[point]-1)
)

periods_fwd <- sapply(win_dow:(length(end_points)-1),
                      function(point)
                        end_points[point]:end_points[point+1]
)

rets_xts <- etf_rets[, "VTI"]
risk_ret <- sapply(periods_back, function(period) c(ret=sum(rets_xts[period]), risk=sd(rets_xts[period])))
risk_ret <- t(risk_ret)
risk_ret <- cbind(risk_ret, risk_ret[, 1]/risk_ret[, 2])
colnames(risk_ret)[3] <- "sr"

fut_ret <- sapply(periods_fwd, function(period) sum(rets_xts[period]))
# calculate pnl
risk_ret <- cbind(risk_ret, risk_ret[, 3]*fut_ret)
colnames(risk_ret)[4] <- "pnl"
plot(cumsum(risk_ret[, 4]), t="l")

# calculate transaction costs
co_sts <- abs(diff(risk_ret[, "sr"]))
co_sts[1] <- 0
sum(bid_offer*co_sts)


### create histogram of risk and return stats

# flatten list into data.frame
period_stats <- do.call(rbind, period_stats)
period_stats <- as.data.frame(period_stats)


# create vectors of risk and return bins (for histogram)
ret_vec <- sort(period_stats[, "ret"])
ret_vec <- c(first(ret_vec), 
             ret_vec[(length(ret_vec) %/% 10) * (1:9)], 
             last(ret_vec))
# ret_vec <- range(period_stats[, "ret"])
# ret_vec <- seq(ret_vec[1], ret_vec[2], length.out=11)
# sapply(1:10, function(ret_ind) sum(
#   ret_vec[ret_ind]<period_stats[, "ret"] &
#     ret_vec[ret_ind+1]>period_stats[, "ret"]))
# sapply(1:10, function(ret_ind) with(period_stats,
#   mean(period_stats[ret_vec[ret_ind]<ret &
#     ret_vec[ret_ind+1]>ret, "fut_ret"]))
#   )  # end sapply


risk_vec <- sort(period_stats[, "risk"])
risk_vec <- c(first(risk_vec), 
              risk_vec[(length(risk_vec) %/% 10) * (1:9)], 
              last(risk_vec))
# risk_vec <- range(period_stats[, "risk"])
# risk_vec <- seq(risk_vec[1], risk_vec[2], length.out=11)

# calculate histogram of future returns
fut_ret_mat <- matrix(numeric(100), ncol=10)
for (ret_ind in 1:10) {
  for (risk_ind in 1:10) {
    fut_ret_mat[ret_ind, risk_ind] <- with(period_stats,
           mean(
             period_stats[(ret>ret_vec[ret_ind] & 
                             ret<ret_vec[ret_ind+1] & 
                             risk>risk_vec[risk_ind] & 
                             risk<risk_vec[risk_ind+1]), "fut_ret"]
           )  # end mean
      )  # end with
  }  # end for risk_ind
}  # end for ret_ind


# plot histogram
persp(ret_vec[1:10], risk_vec[1:10], fut_ret_mat,
      theta = 45, phi = 30,
      shade = 0.5,
      col = rainbow(50),
      border = "green",
      main = "fut_ret")



### mostly unnecessary code
# but it's cute, so keep it around for reuse

# create rebalancing period dates
period_dates <- endpoints(etf_rets, on=re_balance)
period_dates <- index(etf_rets)[period_dates]
period_dates <- c(index(etf_rets[1, ]), period_dates)

# create overlaping period date windows
period_windows <- lapply(win_dow:(length(period_dates)-1),
                         function(end_point)
                           c(look_back=paste(
                             period_dates[end_point-win_dow], 
                             "/", 
                             period_dates[end_point], sep=""),
                             look_fwd=paste(
                               period_dates[end_point], 
                               "/", 
                               period_dates[end_point+1], sep=""))
)  # end lapply
period_windows <- do.call(rbind, period_windows)
period_windows <- xts(period_windows, 
                      order.by=period_dates[win_dow:(length(period_dates)-1)])

### end unnecessary code - period date windows



########################
### factorAnalytics

load(file="C:/Develop/data/etf_data.Rdata")
load(file="C:/Develop/data/portf_optim.RData")
library(factorAnalytics)

# fit a two-factor model using principal component analysis
factor_pca <- fitSfm(etf_rets, k=3)
# factor loadings
head(factor_pca$loadings)
# factor realizations (time series)
head(factor_pca$factors)
# residuals from regression
factor_pca$residuals[1:3, 1:3]
# estimated alphas
factor_pca$alpha
# R-squared from regression
factor_pca$r2
# covariance matrix estimated by factor model
factor_pca$Omega[1:3, 4:6]


?plot.sfm
# screeplot of eigenvalues
plot(factor_pca, which.plot.group=1, loop=FALSE, eig.max=0.9, cex.names=0.9, cex.axis=0.9, cex.main=0.8)

# time series plot of estimated factors
library(PortfolioAnalytics)
plot(factor_pca, which.plot.group=2, loop=FALSE)
chart.CumReturns(factor_pca$factors, lwd=2, ylab="", legend.loc="topleft", main="")

# factor loadings
plot(factor_pca, which.plot.group=3, n.max=30, loop=FALSE)

# R-squared histogram
plot(factor_pca, which.plot.group=4, loop=FALSE)

# residual volatility histogram
plot(factor_pca, which.plot.group=5, loop=FALSE)

# residual correlations
plot(factor_pca, which.plot.group=6, loop=FALSE)

# asset correlations - "hclust" for hierarchical clustering order
plot(factor_pca, which.plot.group=7, loop=FALSE, order="hclust", method="ellipse")
plot(factor_pca, which.plot.group=7, n.max=30, loop=FALSE, order="hclust", method="ellipse")

# factor contribution to SD
plot(factor_pca, which.plot.group=8, loop=FALSE)

# factor contribution to ES
plot(factor_pca, which.plot.group=9, loop=FALSE)

# factor contribution to VAR
plot(factor_pca, which.plot.group=10, loop=FALSE)


### individual asset plots

# cumulative residuals
chart.CumReturns(factor_pca$residuals[, c("IEF", "DBC", "XLF")], lwd=2, ylab="", legend.loc="topleft", main="")

# actual and fitted returns
plot(factor_pca, asset.name="VTI", which.plot.single=1, plot.single=TRUE, loop=FALSE)

# residuals with standard error bands
plot(factor_pca, asset.name="VTI", which.plot.single=2, plot.single=TRUE, loop=FALSE)

# squared residuals
plot(factor_pca, asset.name="VTI", which.plot.single=3, plot.single=TRUE, loop=FALSE)

# absolute residuals
plot(factor_pca, asset.name="VTI", which.plot.single=4, plot.single=TRUE, loop=FALSE)

# SACF and PACF of residuals
plot(factor_pca, asset.name="VTI", which.plot.single=5, plot.single=TRUE, loop=FALSE)

# SACF and PACF of squared residuals
plot(factor_pca, asset.name="VTI", which.plot.single=6, plot.single=TRUE, loop=FALSE)

# SACF and PACF of absolute residuals
plot(factor_pca, asset.name="VTI", which.plot.single=7, plot.single=TRUE, loop=FALSE)

# residual histogram with normal curve 
plot(factor_pca, asset.name="VTI", which.plot.single=8, plot.single=TRUE, loop=FALSE, xlim=c(-0.007, 0.007))

# residual qq-plot
plot(factor_pca, asset.name="VTI", which.plot.single=9, plot.single=TRUE, loop=FALSE)

# 
plot(factor_pca, asset.name="VTI", which.plot.single=10, plot.single=TRUE, loop=FALSE)

# 
plot(factor_pca, asset.name="VTI", which.plot.single=11, plot.single=TRUE, loop=FALSE)

# 
plot(factor_pca, asset.name="VTI", which.plot.single=12, plot.single=TRUE, loop=FALSE)


# 
plot(factor_pca, asset.name="VTI", which.plot.single=9, plot.single=TRUE, loop=FALSE)



########################
### data download

### scrape ETF ticker table using XML, qmao packages - from 2012, doesn't work now
# http://stackoverflow.com/questions/5246843/how-to-get-a-complete-list-of-ticker-symbols-from-yahoo-finance

# first find out how many ETFs there are, then construct a URL
etf_list <- readLines("http://finance.yahoo.com/etf/browser/mkt")
# Sorry for the ugly regex
etf_num <- gsub("^(\\w+)\\s?(.*)$", "\\1", 
                gsub("(.*)(Showing 1 - 20 of )(.*)", "\\3",
                     etf_list[grep("Showing 1 - 20", etf_list)]))
etf_url <- paste0("http://finance.yahoo.com/etf/browser/mkt?c=0&k=5&f=0&o=d&cs=1&ce=", etf_num)

library(XML)
etf_tbl <- readHTMLTable(etf_url, stringsAsFactors=FALSE)
etf_dat <- etf_tbl[[tail(grep("Ticker", etf_tbl), 1)]][-1, ]
colnames(etf_dat) <- etf_dat[1, ]
etf_dat <- etf_dat[-1, ]
etfs <- etf_dat$Ticker # All ETF tickers from yahoo
length(etfs)
head(etfs)

### end scrape ETF ticker table


### download database of stock tickers using stockSymbols() from package TTR - no ETFs
library(TTR)
# stock_table <- stockSymbols("NYSE")
stock_table <- stockSymbols()
write.csv(stock_table, file='stock_table.csv')


### ETF symbols - tickers for Tactical Asset Allocation System by Mebane Faber
sym_bols <- c("VTI", "VEU", "IEF", "VNQ", "DBC", "XLY", "XLP", "XLE", "XLF", "XLV", "XLI", "XLB", "XLK", "XLU", "IWB", "IWD", "IWF", "IWM", "IWN", "IWO", "IWP", "IWR", "IWS", "IWV", "IUSV", "IUSG")

# read etf database into data frame
etf_list <- read.csv(file='etf_list.csv')
sym_bols %in% etf_list$Symbol
# subset etf_list to include only those ETF's in sym_bols
etf_list <- etf_list[etf_list$Symbol %in% sym_bols, ]


### download time series of "AdjClose" and "Volume" for single symbol
library(tseries)
sym_bol <- "MSFT"
field_names <- c("AdjClose", "Volume")
zoo_series <- suppressWarnings(  # load MSFT data
  get.hist.quote(instrument=sym_bol, 
                 quote=field_names,
                 start=Sys.Date()-365, 
                 end=Sys.Date(), 
                 origin="1970-01-01")
)  # end suppressWarnings


# download data for list of symbols
sym_bols <- c("YHOO", "MSFT")
zoo_series <- suppressWarnings(
  lapply(sym_bols, # loop for loading data
         get.hist.quote,
         quote=field_names,
         start=Sys.Date()-365, 
         end=Sys.Date(), 
         origin="1970-01-01")
)  # end suppressWarnings
names(zoo_series) <- sym_bols

# flatten into a single zoo
zoo_series <- do.call(merge, zoo_series)
names(zoo_series) <- as.vector(sapply(sym_bols, paste, c("Close", "Volume"), sep="."))
head(zoo_series)

# write zoo to CSV file
write.zoo(zoo_series, file='zoo_series.csv', sep=",")
# save to binary file
save(zoo_series, file='zoo_series.Rdata')

### plot

etf_gg <- autoplot(zoo_series[, "VTI.Close"], 
                   main="Vanguard Total Stock Market ETF") + 
  xlab("") + ylab("") + 
  theme(
    #  legend.position="none", 
    legend.position=c(0.1, 0.5), 
    plot.title=element_text(vjust=-2.0), 
    #  plot.margin=unit(c(-0.0,0.0,-0.5,0.0),"cm"), 
    #  axis.text.y=element_blank(),
    plot.background=element_blank()
  )  # end theme
# render ggplot
etf_gg


###

tick_data <- read.csv("http://ichart.finance.yahoo.com/table.csv?s=BAC&a=fM&b=fD&c=fY&d=M&e=D&f=Y", 
                      header=FALSE)

# version with anon function
zoo_series <- suppressWarnings(  # load MSFT data
  sapply(sym_bols, function(sym_bol, ...) {
    coredata(get.hist.quote(sym_bol, ...))
  },
  quote=field_names,
  start=Sys.Date()-365, 
  end=Sys.Date(), 
  origin="1970-01-01")
)  # end suppressWarnings



########################
### extra unused code


# contingency table doesn't return zero for bins with missing values (hist does)
pois_table <- table(random_var)  # calculate contingency table
pois_table <- pois_table/sum(pois_table)  # calculate frequency table
pois_table
names(pois_table)  # get names of table

# open Windows graphics device
x11(width=11, height=7, title="function plot")
1
# create barplot
barplot(pois_table, col="blue", ylab="Frequency of events", xlab="No. of events", main="Poisson distribution")
x_var <- 0:max(random_var)
lines(x=x_var, y=dpois(x_var, lambda=4), lwd=2, col="red")

graphics.off()  # close all graphics devices

# combines together first two values
hist(random_var, freq=FALSE, col="grey", breaks=x_var)

hist(random_var, freq=FALSE, col="grey", breaks="FD")

# doesn't work
curve(expr=dpois(x, lambda=6), xlim=c(0, 11), ylab="", 
      lwd=2, col="red")
curve(expr=dpois(x, lambda=4), add=TRUE, xlim=c(0, 11), ylab="", 
      lwd=2, col="blue")
# doesn't work
plot(x=dpois(x, lambda=6), type="l", xlim=c(0, 11), ylab="", lwd=2, col="red")
plot(x=dnorm, type="l", xlim=c(-2, 2), ylab="", lwd=2, col="red")
plot(x=dnorm(x, mean=1), type="l", xlim=c(-1, 3), ylab="", lwd=2, col="red")
curve(expr=dnorm(x, mean=1), type="l", xlim=c(-1, 3), ylab="", lwd=2, col="red")


# add title
title(main="sine and cosine functions", cex=1.5, line=0.1)
# add legend
legend(x="topright", legend=c("sine", "cosine"),
       title="legend", inset=0.05, cex=1.0, bg="white",
       lwd=2, lty=c(1, 1), col=c("red", "blue"))



########################
### misc stuff for deletion

