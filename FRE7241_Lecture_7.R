library(knitr)
opts_chunk$set(prompt=TRUE, eval=FALSE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
options(digits=3)
thm <- knit_theme$get("acid")
knit_theme$set(thm)
# define aggregation functional
roll_agg <- function(re_turns, agg_fun=sum,
    look_back=12, re_balance="months",
    end_points=xts::endpoints(re_turns, on=re_balance), ...) {
# define start_points and forward (future) endpoints
  len_gth <- NROW(end_points)
  start_points <- end_points[c(rep_len(1, look_back-1), 1:(len_gth-look_back+1))]
  fwd_points <- end_points[c(2:len_gth, len_gth)]
# Perform loop over end_points and calculate aggregations
  agg_s <- sapply(2:(len_gth-1), function(in_dex) {
    c(sapply(re_turns[start_points[in_dex]:end_points[in_dex]], agg_fun, ...),  # end sapply
    sapply(re_turns[(end_points[in_dex]+1):fwd_points[in_dex]], sum))  # end sapply
  })  # end sapply
  agg_s <- t(agg_s)
#  colnames(agg_s) <- c(paste0("past_", colnames(re_turns)), paste0("fwd_", colnames(re_turns)))
  xts::xts(agg_s,
     order.by=index(re_turns[end_points[2:(len_gth-1)]]))
}  # end roll_agg
agg_fun <- function(re_turns) sum(re_turns)/sd(re_turns)
agg_s <- roll_agg(re_turns, agg_fun=agg_fun,
            look_back=12, re_balance="months")
# define agg_fun() equal to the Sharpe ratio
agg_fun <- function(re_turns) sum(re_turns)/sd(re_turns)
# Define vector of symbols for the model:
sym_bols <- c("VTI", "IEF", "DBC")
# apply roll_agg() to ETF returns
agg_s <- roll_agg(rutils::env_etf$re_turns[, sym_bols],
            agg_fun=agg_fun,
            look_back=52,
            re_balance="weeks")
# select aggregations over look-back intervals
past_aggs <- agg_s[, seq_along(sym_bols)]
# select aggregations over look-forward intervals
fwd_rets <- agg_s[, NROW(sym_bols) + seq_along(sym_bols)]
# calculate the portfolio weights proportional to past_aggs
weight_s <- past_aggs
# scale weight_s so that their sum of squares is equal to 1
weight_s <- weight_s/sqrt(rowSums(weight_s^2))
# plot the weights in multiple verticle panels
zoo::plot.zoo(weight_s, xlab=NULL)
# calculate betas
beta_s <- c(1, rutils::env_etf$capm_stats[
  match(sym_bols[-1],
  rownames(rutils::env_etf$capm_stats)),
  "Beta"])
names(beta_s)[1] <- sym_bols[1]
# weights times betas
beta_s <- weight_s %*% beta_s
beta_s <- xts(beta_s, order.by=index(weight_s))
colnames(beta_s) <- "portf_beta"
x11()
plot.zoo(cbind(beta_s,
  rutils::env_etf$VTI[, 4])[index(beta_s)],
  main="betas & VTI", xlab="")
# calculate portfolio profits and losses
pnl_s <- rowSums(weight_s * fwd_rets)
pnl_s <- xts(pnl_s, order.by=index(fwd_rets))
colnames(pnl_s) <- "pnl_s"
# calculate vector of transaction costs
# bid_offer is equal to 10 bps for liquid ETFs
bid_offer <- 0.001
cost_s <- bid_offer*abs(rutils::diff_xts(weight_s))
cost_s <- rowSums(cost_s)
# subtract the cost_s from pnl_s
pnl_s <- pnl_s - cost_s
# plot the cumulative strategy pnl_s
chart_Series(x=cumsum(pnl_s), name="Strategy PnL")
# define back-test functional
back_test <- function(re_turns, agg_fun=sum, look_back=12,
                re_balance="months", bid_offer=0.001) {
  stopifnot("package:quantmod" %in% search() || require("quantmod", quietly=TRUE))
  # Calculate aggregations.
  agg_s <- roll_agg(re_turns, agg_fun=agg_fun,
              look_back=look_back, re_balance=re_balance)
  # Select aggregations over look-back and look-forward intervals.
  past_aggs <- agg_s[, 1:NCOL(re_turns)]
  fwd_rets <- agg_s[, NCOL(re_turns)+1:NCOL(re_turns)]
  # Calculate portfolio weights.
  weight_s <- past_aggs
  weight_s <- weight_s/sqrt(rowSums(weight_s^2))
  # Calculate portfolio profits and losses.
  pnl_s <- rowSums(weight_s * fwd_rets)
  pnl_s <- xts(pnl_s, order.by=index(fwd_rets))
  colnames(pnl_s) <- "pnl_s"
  # Calculate transaction costs.
  cost_s <- bid_offer*abs(rutils::diff_xts(weight_s))
  cost_s <- rowSums(cost_s)
  pnl_s - cost_s
}  # end back_test
# define parameters
re_turns <- rutils::env_etf$re_turns[, sym_bols]
re_balance <- "weeks"
bid_offer <- 0.001
look_backs <- 10*(1:15)
# initialize compute cluster
library(parallel)
clus_ter <- makeCluster(detectCores()-1)
clusterExport(clus_ter,
  varlist=c("back_test", "roll_agg", "agg_fun", "re_turns", "re_balance", "bid_offer"))
# perform parallel loop over look_backs under Windows
pro_files <- parSapply(clus_ter, look_backs,
  function(look_back) {
  # perform back-test
    sum(back_test(re_turns=re_turns,
            agg_fun=agg_fun,
            look_back=look_back,
            re_balance=re_balance,
            bid_offer=bid_offer))
  })  # end parSapply
# perform parallel loop over look_backs under Mac-OSX or Linux
pro_files <- mclapply(look_backs, function(look_back) {
  # perform back-test
  sum(back_test(re_turns=re_turns,
    agg_fun=agg_fun,
    look_back=look_back,
    re_balance=re_balance,
    bid_offer=bid_offer))
})  # end mclapply
# stop R processes over cluster under Windows
stopCluster(clus_ter)
# non-parallel loop - for reference only
pro_files <- sapply(look_backs,
  function(look_back, ...)
    sum(back_test(look_back=look_back, ...)),
  re_turns=rutils::env_etf$re_turns[, sym_bols],
  agg_fun=agg_fun,
  re_balance="weeks",
  bid_offer=0.001)
plot(cbind(look_backs, pro_files), t="l",
     main="Strategy PnL as function of look_back",
     xlab="look_back (weeks)", ylab="pnl")
# verify that rtools are working properly:
devtools::find_rtools()
devtools::has_devel()

# load package Rcpp
library(Rcpp)
# get documentation for package Rcpp
# get short description
packageDescription("Rcpp")
# load help page
help(package="Rcpp")
# list all datasets in "Rcpp"
data(package="Rcpp")
# list all objects in "Rcpp"
ls("package:Rcpp")
# remove Rcpp from search path
detach("package:Rcpp")
# define Rcpp function
Rcpp::cppFunction("
  int times_two(int x)
    { return 2 * x;}
  ")  # end cppFunction
# run Rcpp function
times_two(3)
# source Rcpp functions from file
Rcpp::sourceCpp(file="C:/Develop/R/scripts/rcpp_mult.cpp")
# multiply two numbers
rcpp_mult(2, 3)
rcpp_mult(1:3, 6:4)
# multiply two vectors
rcpp_mult_vec(2, 3)
rcpp_mult_vec(1:3, 6:4)
# define Rcpp function with loop
Rcpp::cppFunction("
double inner_mult(NumericVector x, NumericVector y) {
int x_size = x.size();
int y_size = y.size();
if (x_size != y_size) {
    return 0;
  } else {
    double total = 0;
    for(int i = 0; i < x_size; ++i) {
total += x[i] * y[i];
  }
  return total;
  }
}")  # end cppFunction
# run Rcpp function
inner_mult(1:3, 6:4)
inner_mult(1:3, 6:3)
# define Rcpp Sugar function with loop
Rcpp::cppFunction("
double inner_mult_sugar(NumericVector x, NumericVector y) {
  return sum(x * y);
}")  # end cppFunction
# run Rcpp Sugar function
inner_mult_sugar(1:3, 6:4)
inner_mult_sugar(1:3, 6:3)
# define R function with loop
inner_mult_r <- function(x, y) {
    to_tal <- 0
    for(i in 1:NROW(x)) {
to_tal <- to_tal + x[i] * y[i]
    }
    to_tal
}  # end inner_mult_r
# run R function
inner_mult_r(1:3, 6:4)
inner_mult_r(1:3, 6:3)
# compare speed of Rcpp and R
library(microbenchmark)
summary(microbenchmark(
  pure_r=inner_mult_r(1:10000, 1:10000),
  inner_r=1:10000 %*% 1:10000,
  r_cpp=inner_mult(1:10000, 1:10000),
  r_cpp_sugar=inner_mult_sugar(1:10000, 1:10000),
  times=10))[, c(1, 4, 5)]
# define Ornstein-Uhlenbeck function in R
ou_proc <- function(len_gth=1000, eq_price=5.0,
              vol_at=0.01, the_ta=0.01) {
  re_turns <- numeric(len_gth)
  price_s <- numeric(len_gth)
  price_s[1] <- eq_price
  for (i in 2:len_gth) {
    re_turns[i] <- the_ta*(eq_price - price_s[i-1]) + vol_at*rnorm(1)
    price_s[i] <- price_s[i-1] * exp(re_turns[i])
  }  # end for
  price_s
}  # end ou_proc
# simulate Ornstein-Uhlenbeck process
eq_price <- 5.0; vol_at <- 0.01
the_ta <- 0.01; len_gth <- 1000
set.seed(1121)  # reset random numbers
price_s <- ou_proc(len_gth=len_gth, eq_price=eq_price, vol_at=vol_at, the_ta=the_ta)
# define Ornstein-Uhlenbeck function in Rcpp
Rcpp::cppFunction("
NumericVector rcpp_ou_proc(int len_gth, double eq_price, double vol_at, double the_ta, NumericVector r_norm) {
  NumericVector price_s(len_gth);
  NumericVector re_turns(len_gth);
  price_s[0] = eq_price;
  for (int i = 1; i < len_gth; ++i) {
    re_turns[i] = the_ta*(eq_price - price_s[i-1]) + vol_at*r_norm[i-1];
    price_s[i] = price_s[i-1] * exp(re_turns[i]);
  }
  return price_s;
}")  # end cppFunction
set.seed(1121)  # reset random numbers
price_s <- rcpp_ou_proc(len_gth=len_gth, eq_price=eq_price, vol_at=vol_at, the_ta=the_ta, r_norm=rnorm(len_gth))
# compare speed of Rcpp and R
library(microbenchmark)
summary(microbenchmark(
  pure_r=ou_proc(len_gth=len_gth, eq_price=eq_price, vol_at=vol_at, the_ta=the_ta),
  r_cpp=rcpp_ou_proc(len_gth=len_gth, eq_price=eq_price, vol_at=vol_at, the_ta=the_ta, r_norm=rnorm(len_gth)),
  times=10))[, c(1, 4, 5)]
library(PortfolioAnalytics)  # load package "PortfolioAnalytics"
# get documentation for package "PortfolioAnalytics"
packageDescription("PortfolioAnalytics")  # get short description

help(package="PortfolioAnalytics")  # load help page

data(package="PortfolioAnalytics")  # list all datasets in "PortfolioAnalytics"

ls("package:PortfolioAnalytics")  # list all objects in "PortfolioAnalytics"

detach("package:PortfolioAnalytics")  # remove PortfolioAnalytics from search path
library(PortfolioAnalytics)
# use ETF returns from package HighFreq
library(HighFreq)
portf_names <- c("VTI", "IEF", "DBC", "XLF",
  "VNQ", "XLP", "XLV", "XLU", "XLB", "XLE")
# initial portfolio to equal weights
portf_init <- rep(1/NROW(portf_names),
            NROW(portf_names))
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
load(file="C:/Develop/data/portf_optim.RData")
library(PortfolioAnalytics)
# perform optimization of weights
maxSR_DEOpt <- optimize.portfolio(
  R=env_etf$re_turns[, portf_names],  # specify returns
  portfolio=portf_maxSR,  # specify portfolio
  optimize_method="DEoptim", # use DEoptim
  maxSR=TRUE,  # maximize Sharpe
  trace=TRUE, traceDE=0)
# plot optimization
chart.RiskReward(maxSR_DEOpt,
  risk.col="StdDev",
  return.col="mean")
options(width=50)
library(PortfolioAnalytics)
load(file="C:/Develop/data/portf_optim.RData")
maxSR_DEOpt$weights
maxSR_DEOpt$objective_measures$mean[1]
maxSR_DEOpt$objective_measures$StdDev[[1]]
library(PortfolioAnalytics)
# plot optimization
chart.RiskReward(maxSR_DEOpt,
  risk.col="StdDev",
  return.col="mean")

# plot risk/ret points in portfolio scatterplot
risk_ret_points <- function(rets=env_etf$re_turns,
  risk=c("sd", "ETL"), sym_bols=c("VTI", "IEF")) {
  risk <- match.arg(risk)  # match to arg list
  if (risk=="ETL") {
    stopifnot(
"package:PerformanceAnalytics" %in% search() ||
require("PerformanceAnalytics", quietly=TRUE))
  }  # end if
  risk <- match.fun(risk)  # match to function
  risk_ret <- t(sapply(rets[, sym_bols],
     function(x_ts)
 c(ret=mean(x_ts), risk=abs(risk(x_ts)))))
  points(x=risk_ret[, "risk"], y=risk_ret[, "ret"],
   col="red", lwd=3)
  text(x=risk_ret[, "risk"], y=risk_ret[, "ret"],
 labels=rownames(risk_ret), col="red",
 lwd=2, pos=4)
}  # end risk_ret_points

risk_ret_points()
library(PortfolioAnalytics)
plot_portf <- function(portfolio,
      rets_data=env_etf$re_turns) {
  weight_s <- portfolio$weights
  portf_names <- names(weight_s)
  # calculate xts of portfolio
  portf_max <- xts(
    rets_data[, portf_names] %*% weight_s,
    order.by=index(rets_data))
  colnames(portf_max) <-
    deparse(substitute(portfolio))
  graph_params <- par(oma=c(1, 0, 1, 0),
    mgp=c(2, 1, 0), mar=c(2, 1, 2, 1),
    cex.lab=0.8, cex.axis=1.0,
    cex.main=0.8, cex.sub=0.5)
  layout(matrix(c(1,2), 2),
    widths=c(1,1), heights=c(1,3))
  barplot(weight_s, names.arg=portf_names,
    las=3, ylab="", xlab="Symbol", main="")
  title(main=paste("Loadings",
          colnames(portf_max)), line=-1)
  chart.CumReturns(
    cbind(portf_max, rets_data[, c("IEF", "VTI")]),
    lwd=2, ylab="", legend.loc="topleft", main="")
  title(main=paste0(colnames(portf_max),
              ", IEF, VTI"), line=-1)
  par(graph_params)  # restore original parameters
  invisible(portf_max)
}  # end plot_portf
maxSR_DEOpt_xts <- plot_portf(portfolio=maxSR_DEOpt)
library(PortfolioAnalytics)
# add leverage constraint abs(weight_sum)
portf_maxSRN <- add.constraint(
  portfolio=portf_init, type="leverage",
  min_sum=0.9, max_sum=1.1)
# add box constraint long/short
portf_maxSRN <- add.constraint(
  portfolio=portf_maxSRN,
  type="box", min=-0.2, max=0.2)

# add objectives
portf_maxSRN <- add.objective(
  portfolio=portf_maxSRN,
  type="return",  # maximize mean return
  name="mean")
# add objectives
portf_maxSRN <- add.objective(
  portfolio=portf_maxSRN,
  type="risk",  # minimize StdDev
  name="StdDev")
load(file="C:/Develop/data/portf_optim.RData")
library(PortfolioAnalytics)
# perform optimization of weights
maxSRN_DEOpt <- optimize.portfolio(
  R=env_etf$re_turns[, portf_names],  # specify returns
  portfolio=portf_maxSRN,  # specify portfolio
  optimize_method="DEoptim", # use DEoptim
  maxSR=TRUE,  # maximize Sharpe
  trace=TRUE, traceDE=0)
# plot optimization
chart.RiskReward(maxSRN_DEOpt,
  risk.col="StdDev",
  return.col="mean",
  xlim=c(
    maxSR_DEOpt$objective_measures$StdDev[[1]]-0.001,
    0.016))
  points(x=maxSR_DEOpt$objective_measures$StdDev[[1]],
   y=maxSR_DEOpt$objective_measures$mean[1],
   col="green", lwd=3)
  text(x=maxSR_DEOpt$objective_measures$StdDev[[1]],
   y=maxSR_DEOpt$objective_measures$mean[1],
 labels="maxSR", col="green",
 lwd=2, pos=4)
# plot risk/ret points in portfolio scatterplot
risk_ret_points()
library(PortfolioAnalytics)
load(file="C:/Develop/data/portf_optim.RData")
maxSRN_DEOpt$weights
maxSRN_DEOpt$objective_measures$mean[1]
maxSRN_DEOpt$objective_measures$StdDev[[1]]
library(PortfolioAnalytics)
maxSRN_DEOpt_xts <-
  plot_portf(portfolio=maxSRN_DEOpt)
library(PerformanceAnalytics)
load(file="C:/Develop/data/portf_optim.RData")
chart.CumReturns(
  cbind(maxSR_DEOpt_xts, maxSRN_DEOpt_xts),
  lwd=2, ylab="",
  legend.loc="topleft", main="")
options(width=50)
library(PerformanceAnalytics)
load(file="C:/Develop/data/portf_optim.RData")
rbind(maxSR_DEOpt$weights, maxSRN_DEOpt$weights)
c(maxSR_DEOpt$objective_measures$mean,
maxSRN_DEOpt$objective_measures$mean)
c(maxSR_DEOpt$objective_measures$StdDev[[1]],
maxSRN_DEOpt$objective_measures$StdDev[[1]])
library(PortfolioAnalytics)
# add constraints
portf_maxSTARR <- add.constraint(
  portfolio=portf_init,  # initial portfolio
  type="weight_sum",  # constraint sum weights
  min_sum=0.9, max_sum=1.1)
# add constraints
portf_maxSTARR <- add.constraint(
  portfolio=portf_maxSTARR,
  type="long_only")  # box constraint min=0, max=1
# add objectives
portf_maxSTARR <- add.objective(
  portfolio=portf_maxSTARR,
  type="return",  # maximize mean return
  name="mean")
# add objectives
portf_maxSTARR <- add.objective(
  portfolio=portf_maxSTARR,
  type="risk",  # minimize Expected Shortfall
  name="ES")
load(file="C:/Develop/data/portf_optim.RData")
library(PortfolioAnalytics)
# perform optimization of weights
maxSTARR_DEOpt <- optimize.portfolio(
  R=env_etf$re_turns[, portf_names],  # specify returns
  portfolio=portf_maxSTARR,  # specify portfolio
  optimize_method="DEoptim", # use DEoptim
  maxSTARR=TRUE,  # maximize STARR
  trace=TRUE, traceDE=0)

# plot optimization
chart.RiskReward(maxSTARR_DEOpt,
  risk.col="ES",
  return.col="mean")
# plot risk/ret points in portfolio scatterplot
risk_ret_points(risk="ETL")
options(width=50)
library(PortfolioAnalytics)
load(file="C:/Develop/data/portf_optim.RData")
maxSTARR_DEOpt$weights
maxSTARR_DEOpt$objective_measures$mean[1]
maxSTARR_DEOpt$objective_measures$ES[[1]]
library(PortfolioAnalytics)
maxSTARR_DEOpt_xts <-
  plot_portf(portfolio=maxSTARR_DEOpt)
library(PerformanceAnalytics)
load(file="C:/Develop/data/portf_optim.RData")
chart.CumReturns(
  cbind(maxSR_DEOpt_xts, maxSTARR_DEOpt_xts),
  lwd=2, ylab="",
  legend.loc="topleft", main="")
options(width=50)
library(PerformanceAnalytics)
load(file="C:/Develop/data/portf_optim.RData")
rbind(maxSR_DEOpt$weights, maxSTARR_DEOpt$weights)
c(maxSR_DEOpt$objective_measures$mean,
maxSTARR_DEOpt$objective_measures$mean)
c(maxSR_DEOpt$objective_measures$StdDev[[1]],
maxSTARR_DEOpt$objective_measures$ES[[1]])
library(PortfolioAnalytics)
# plot the efficient frontier
chart.EfficientFrontier(maxSR_DEOpt,
          match.col="StdDev",
          n.portfolios=15, type="l")
points(x=maxSRN_DEOpt$objective_measures$StdDev[[1]],
   y=maxSRN_DEOpt$objective_measures$mean[1],
   col="green", lwd=3)
text(x=maxSRN_DEOpt$objective_measures$StdDev[[1]],
   y=maxSRN_DEOpt$objective_measures$mean[1],
 labels="maxSRN", col="green",
 lwd=2, pos=4)
library(PortfolioAnalytics)
# add constraints
portf_minES <- add.constraint(
  portfolio=portf_init,  # initial portfolio
  type="weight_sum",  # constraint sum weights
  min_sum=0.9, max_sum=1.1)
# add constraints
portf_minES <- add.constraint(
  portfolio=portf_minES,
  type="long_only")  # box constraint min=0, max=1
# add objectives
portf_minES <- add.objective(
  portfolio=portf_minES,
  type="risk",  # minimize ES
  name="ES")
load(file="C:/Develop/data/portf_optim.RData")
library(PortfolioAnalytics)
# perform optimization of weights
minES_ROI <- optimize.portfolio(
  R=env_etf$re_turns[, portf_names],  # specify returns
  portfolio=portf_minES,  # specify portfolio
  optimize_method="ROI", # use ROI
  trace=TRUE, traceDE=0)

# plot optimization
chart.RiskReward(maxSTARR_DEOpt,
  risk.col="ES",
  return.col="mean")
  points(x=minES_ROI$objective_measures$ES[[1]],
   y=mean(minES_ROI_xts),
   col="green", lwd=3)
  text(x=minES_ROI$objective_measures$ES[[1]],
   y=mean(minES_ROI_xts),
 labels="minES", col="green",
 lwd=2, pos=4)
# plot risk/ret points in portfolio scatterplot
risk_ret_points(risk="ETL")
options(width=50)
library(PortfolioAnalytics)
load(file="C:/Develop/data/portf_optim.RData")
minES_ROI$weights
minES_ROI$objective_measures$ES[[1]]
library(PortfolioAnalytics)
minES_ROI_xts <-
  plot_portf(portfolio=minES_ROI)
library(PerformanceAnalytics)
load(file="C:/Develop/data/portf_optim.RData")
chart.CumReturns(
  cbind(maxSR_DEOpt_xts, minES_ROI_xts),
  lwd=2, ylab="",
  legend.loc="topleft", main="")
options(width=50)
library(PerformanceAnalytics)
load(file="C:/Develop/data/portf_optim.RData")
rbind(maxSR_DEOpt$weights, minES_ROI$weights)
c(maxSR_DEOpt$objective_measures$mean,
minES_ROI$objective_measures$mean)
c(maxSR_DEOpt$objective_measures$StdDev[[1]],
minES_ROI$objective_measures$ES[[1]])
load(file="C:/Develop/data/portf_optim.RData")
library(PortfolioAnalytics)
options(width=50)
# perform optimization of weights
maxSR_DEOpt <- optimize.portfolio(
  R=env_etf$re_turns["/2011", portf_names],
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
options(width=50)
# perform optimization of weights
maxSR_DEOpt <- optimize.portfolio(
  R=env_etf$re_turns["2011/", portf_names],
  portfolio=portf_maxSR,  # specify portfolio
  optimize_method="DEoptim", # use DEoptim
  maxSR=TRUE,  # maximize Sharpe
  trace=TRUE, traceDE=0)
weights_2h <- maxSR_DEOpt$weights

# plot optimization
maxSR_DEOpt_xts <-
  plot_portf(portfolio=maxSR_DEOpt)
options(width=50)
weights_1h
weights_2h
weights_1h - weights_2h
par(oma=c(1,0,1,0), mgp=c(2,1,0), mar=c(2,1,2,1), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # set plot panels
barplot(weights_1h,
  names.arg=names(weights_1h),
  las=3, ylab="", xlab="",
  main="Portfolio Weights First Half")
barplot(weights_2h,
  names.arg=names(weights_2h),
  las=3, ylab="", xlab="",
  main="Portfolio Weights Second Half")
library(quantmod)
#perform pair-wise correlation analysis
# calculate correlation matrix
cor_mat <- cor(re_turns)
colnames(cor_mat) <- colnames(re_turns)
rownames(cor_mat) <- colnames(re_turns)
# reorder correlation matrix based on clusters
# calculate permutation vector
library(corrplot)
or_der <- corrMatOrder(cor_mat,
        order="hclust",
        hclust.method="complete")
# apply permutation vector
cor_mat <- cor_mat[or_der, or_der]
# plot the correlation matrix
color_ramp <- colorRampPalette(c("red", "white", "blue"))
corrplot(cor_mat,
    tl.col="black", tl.cex=0.8,
    method="square", col=color_ramp(8),
    cl.offset=0.75, cl.cex=0.7,
    cl.align.text="l", cl.ratio=0.25)
# draw rectangles on the correlation matrix plot
corrRect.hclust(cor_mat, k=NROW(cor_mat) %/% 2,
          method="complete", col="red")
# convert correlation matrix into distance object
dis_tance <- as.dist(1-cor_mat)
# Perform hierarchical clustering analysis
clus_ter <- hclust(dis_tance)
plot(clus_ter, ann=FALSE, xlab="", ylab="")
title("Dissimilarity = 1-Correlation",
line=-0.5)
