library(knitr)
opts_chunk$set(prompt=TRUE, eval=FALSE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(digits=3)
options(width=60, dev='pdf')
thm <- knit_theme$get("acid")
knit_theme$set(thm)
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
library(quantmod)
library(Rglpk)
# vector of symbol names
sym_bols <- c("VTI", "IEF", "DBC")
n_weights <- NROW(sym_bols)
# calculate mean returns
re_turns <- rutils::env_etf$re_turns[, sym_bols]
mean_rets <- sapply(re_turns, mean)
# specify weight constraints
constraint_s <- matrix(c(rep(1, n_weights),
                 1, 1, 0),
                 nc=n_weights, byrow=TRUE)
direction_s <- c("==", "<=")
rh_s <- c(1, 0)
# specify weight bounds (-1, 1) (default is c(0, Inf))
bound_s <-
  list(lower=list(ind=1:n_weights, val=rep(-1, n_weights)),
 upper=list(ind=1:n_weights, val=rep(1, n_weights)))
# perform optimization
op_tim <- Rglpk::Rglpk_solve_LP(
  obj=mean_rets,
  mat=constraint_s,
  dir=direction_s,
  rhs=rh_s,
  bounds=bound_s,
  max=TRUE)
unlist(op_tim[1:2])
# define a covariance matrix
std_devs <- c(asset1=0.3, asset2=0.6)
cor_rel <- 0.8
co_var <- matrix(c(1, cor_rel, cor_rel, 1),
           nc=2)
co_var <- t(t(std_devs*co_var)*std_devs)
# calculate inverse of covariance mat_rix
in_verse <- solve(a=co_var)
u_nit <- rep(1, NCOL(co_var))
# minimum variance weights with constraint
# weight_s <- solve(a=co_var, b=u_nit)
weight_s <- in_verse %*% u_nit
weight_s <- weight_s / drop(t(u_nit) %*% weight_s)
# minimum variance
weight_s %*% co_var %*% weight_s
1/(t(u_nit) %*% in_verse %*% u_nit)
# calculate excess re_turns
risk_free <- 0.03/252
ex_cess <- re_turns - risk_free
# calculate covariance and inverse matrix
co_var <- cov(re_turns)
u_nit <- rep(1, NCOL(co_var))
in_verse <- solve(a=co_var)
# calculate mean excess returns
ex_cess <- sapply(ex_cess, mean)
# weights of maximum Sharpe portfolio
# weight_s <- solve(a=co_var, b=re_turns)
weight_s <- in_verse %*% ex_cess
weight_s <- weight_s/drop(t(u_nit) %*% weight_s)
# Sharpe ratios
sqrt(252)*sum(weight_s * ex_cess) /
  sqrt(drop(weight_s %*% co_var %*% weight_s))
sapply(re_turns - risk_free,
  function(x) sqrt(252)*mean(x)/sd(x))
weights_maxsharpe <- weight_s
library(quantmod)
# calculate minimum variance weights
weight_s <- in_verse %*% u_nit
weights_minvar <-
  weight_s / drop(t(u_nit) %*% weight_s)
# calculate optimal portfolio returns
optim_rets <- xts(
  x=cbind(exp(cumsum(re_turns %*% weights_maxsharpe)),
    exp(cumsum(re_turns %*% weights_minvar))),
  order.by=index(re_turns))
colnames(optim_rets) <- c("maxsharpe", "minvar")
# plot optimal portfolio returns, with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "green")
x11(width=6, height=5)
chart_Series(optim_rets, theme=plot_theme,
       name="Maximum Sharpe and \nMinimum Variance portfolios")
legend("top", legend=colnames(optim_rets), cex=0.8,
 inset=0.1, bg="white", lty=c(1, 1), lwd=c(6, 6),
 col=plot_theme$col$line.col, bty="n")
x11(wid_th <- 6, hei_ght <- 6)
# calculate minimum variance weights
weight_s <- in_verse %*% u_nit
weight_s <- weight_s / drop(t(u_nit) %*% weight_s)
# minimum standard deviation and return
std_dev <- sqrt(252*drop(weight_s %*% co_var %*% weight_s))
min_ret <- 252*sum(weight_s * mean_rets)
# calculate maximum Sharpe portfolios
risk_free <- (min_ret * seq(-10, 10, by=0.1)^3)/252
eff_front <- sapply(risk_free, function(risk_free) {
  weight_s <- in_verse %*% (mean_rets - risk_free)
  weight_s <- weight_s/drop(t(u_nit) %*% weight_s)
  # portfolio return and standard deviation
  c(return=252*sum(weight_s * mean_rets),
    stddev=sqrt(252*drop(weight_s %*% co_var %*% weight_s)))
})  # end sapply
eff_front <- cbind(252*risk_free, t(eff_front))
colnames(eff_front)[1] <- "risk-free"
eff_front <- eff_front[is.finite(eff_front[, "stddev"]), ]
eff_front <- eff_front[order(eff_front[, "return"]), ]
# plot maximum Sharpe portfolios
plot(x=eff_front[, "stddev"],
     y=eff_front[, "return"], t="l",
     xlim=c(0.0*std_dev, 3.0*std_dev),
     ylim=c(0.0*min_ret, 2.0*min_ret),
     main="Efficient Frontier and Capital Market Line",
     xlab="standard deviation", ylab="return")
points(x=eff_front[, "stddev"], y=eff_front[, "return"],
 col="red", lwd=3)
# plot minimum variance portfolio
points(x=std_dev, y=min_ret, col="green", lwd=6)
text(std_dev, min_ret, labels="minimum \nvariance",
     pos=4, cex=0.8)
# draw Capital Market Line
sor_ted <- sort(eff_front[, 1])
risk_free <-
  sor_ted[findInterval(x=0.5*min_ret, vec=sor_ted)]
points(x=0, y=risk_free, col="blue", lwd=6)
text(x=0, y=risk_free, labels="risk-free",
     pos=4, cex=0.8)
in_dex <- match(risk_free, eff_front[, 1])
points(x=eff_front[in_dex, "stddev"],
 y=eff_front[in_dex, "return"],
 col="blue", lwd=6)
text(x=eff_front[in_dex, "stddev"],
     y=eff_front[in_dex, "return"],
     labels="market portfolio",
     pos=2, cex=0.8)
sharp_e <- (eff_front[in_dex, "return"]-risk_free)/
  eff_front[in_dex, "stddev"]
abline(a=risk_free, b=sharp_e, col="blue", lwd=2)
text(x=0.7*eff_front[in_dex, "stddev"],
     y=0.7*eff_front[in_dex, "return"]+0.01,
     labels="Capital Market Line", pos=2, cex=0.8,
     srt=45*atan(sharp_e*hei_ght/wid_th)/(0.25*pi))
# calculate random portfolios
n_portf <- 1000
ret_sd <- sapply(1:n_portf, function(in_dex) {
  weight_s <- runif(n_weights-1, min=-0.25, max=1.0)
  weight_s <- c(weight_s, 1-sum(weight_s))
  # portfolio return and standard deviation
  c(return=252*sum(weight_s * mean_rets),
    stddev=sqrt(252*drop(weight_s %*% co_var %*% weight_s)))
})  # end sapply
# plot scatterplot of random portfolios
x11(wid_th <- 6, hei_ght <- 6)
plot(x=ret_sd["stddev", ], y=ret_sd["return", ],
     main="Efficient Frontier and Random Portfolios",
     xlim=c(0.5*std_dev, 0.8*max(ret_sd["stddev", ])),
     xlab="standard deviation", ylab="return")
# plot maximum Sharpe portfolios
lines(x=eff_front[, "stddev"],
     y=eff_front[, "return"], lwd=2)
points(x=eff_front[, "stddev"], y=eff_front[, "return"],
 col="red", lwd=3)
# plot minimum variance portfolio
points(x=std_dev, y=min_ret, col="green", lwd=6)
text(std_dev, min_ret, labels="minimum\nvariance",
     pos=2, cex=0.8)
# plot market portfolio
points(x=eff_front[in_dex, "stddev"],
 y=eff_front[in_dex, "return"], col="green", lwd=6)
text(x=eff_front[in_dex, "stddev"],
     y=eff_front[in_dex, "return"],
     labels="market\nportfolio",
     pos=2, cex=0.8)
# plot individual assets
points(x=sqrt(252*diag(co_var)),
 y=252*mean_rets, col="blue", lwd=6)
text(x=sqrt(252*diag(co_var)), y=252*mean_rets,
     labels=names(mean_rets),
     col="blue", pos=1, cex=0.8)
# vector of symbol names
sym_bols <- c("VTI", "IEF", "DBC")
n_weights <- NROW(sym_bols)
# calculate random portfolios
n_portf <- 1000
ret_sd <- sapply(1:n_portf, function(in_dex) {
  weight_s <- runif(n_weights, min=0, max=10)
  weight_s <- weight_s/sum(weight_s)
  portf_rets <- env_etf$re_turns[, sym_bols] %*% weight_s
  100*c(ret=mean(portf_rets), sd=sd(portf_rets))
})  # end sapply
# plot scatterplot of random portfolios
x11(width=(wid_th <- 6), height=(hei_ght <- 5))
plot(x=ret_sd[2, ], y=ret_sd[1, ], xlim=c(0, max(ret_sd[2, ])),
     main="Random portfolios",
     ylim=c(min(0, min(ret_sd[1, ])), max(ret_sd[1, ])),
     xlab=rownames(ret_sd)[2], ylab=rownames(ret_sd)[1])
# plot individual assets
points(x=sqrt(252*diag(co_var)),
 y=252*mean_rets, col="blue", lwd=6)
text(x=sqrt(252*diag(co_var)), y=252*mean_rets,
     labels=names(mean_rets),
     col="blue", pos=1, cex=0.8)
risk_free <- 0.03
re_turns <- c(asset1=0.05, asset2=0.06)
std_devs <- c(asset1=0.4, asset2=0.5)
cor_rel <- 0.6
co_var <- matrix(c(1, cor_rel, cor_rel, 1), nc=2)
co_var <- t(t(std_devs*co_var)*std_devs)
weight_s <- seq(from=-1, to=2, length.out=31)
weight_s <- cbind(weight_s, 1-weight_s)
portf_rets <- weight_s %*% re_turns
portf_sd <-
  sqrt(rowSums(weight_s * (weight_s %*% co_var)))
sharpe_ratios <- (portf_rets-risk_free)/portf_sd
in_dex <- which.max(sharpe_ratios)
max_Sharpe <- max(sharpe_ratios)
# plot efficient frontier
x11(wid_th <- 6, hei_ght <- 5)
par(mar=c(3,3,2,1)+0.1, oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
plot(portf_sd, portf_rets, t="l",
 main=paste0("Efficient frontier and CML for two assets\ncorrelation = ", 100*cor_rel, "%"),
 xlab="standard deviation", ylab="return",
 lwd=2, col="orange",
 xlim=c(0, max(portf_sd)),
 ylim=c(0.02, max(portf_rets)))
# add Market Portfolio (maximum Sharpe ratio portfolio)
points(portf_sd[in_dex], portf_rets[in_dex],
 col="blue", lwd=3)
text(x=portf_sd[in_dex], y=portf_rets[in_dex],
     labels=paste(c("market portfolio\n",
 structure(c(weight_s[in_dex], 1-weight_s[in_dex]),
         names=names(re_turns))), collapse=" "),
     pos=2, cex=0.8)
# plot individual assets
points(std_devs, re_turns, col="green", lwd=3)
text(std_devs, re_turns, labels=names(re_turns), pos=4, cex=0.8)
# add point at risk-free rate and draw Capital Market Line
points(x=0, y=risk_free, col="blue", lwd=3)
text(0, risk_free, labels="risk-free\nrate", pos=4, cex=0.8)
abline(a=risk_free, b=max_Sharpe, lwd=2, col="blue")
range_s <- par("usr")
text(portf_sd[in_dex]/2, (portf_rets[in_dex]+risk_free)/2,
     labels="Capital Market Line", cex=0.8, , pos=3,
     srt=45*atan(max_Sharpe*(range_s[2]-range_s[1])/
             (range_s[4]-range_s[3])*
             hei_ght/wid_th)/(0.25*pi))
# plot portfolios in x11() window
x11(wid_th <- 6, hei_ght <- 5)
par(oma=c(0, 0, 0, 0), mar=c(3,3,2,1)+0.1, mgp=c(2, 1, 0), cex.lab=1.0, cex.axis=1.0, cex.main=1.0, cex.sub=1.0)
# vector of symbol names
sym_bols <- c("VTI", "IEF")
# matrix of portfolio weights
weight_s <- seq(from=-1, to=2, length.out=31)
weight_s <- cbind(weight_s, 1-weight_s)
# calculate portfolio returns and volatilities
re_turns <- rutils::env_etf$re_turns[, sym_bols]
ret_sd <- re_turns %*% t(weight_s)
ret_sd <- cbind(252*colMeans(ret_sd),
  sqrt(252)*matrixStats::colSds(ret_sd))
colnames(ret_sd) <- c("returns", "stddev")
risk_free <- 0.06
ret_sd <- cbind(ret_sd,
  (ret_sd[, "returns"]-risk_free)/ret_sd[, "stddev"])
colnames(ret_sd)[3] <- "Sharpe"
in_dex <- which.max(ret_sd[, "Sharpe"])
max_Sharpe <- ret_sd[in_dex, "Sharpe"]
plot(x=ret_sd[, "stddev"], y=ret_sd[, "returns"],
     main="Stock and Bond portfolios", t="l",
     xlim=c(0, 0.7*max(ret_sd[, "stddev"])), ylim=c(0, max(ret_sd[, "returns"])),
     xlab="standard deviation", ylab="return")
# add blue point for market portfolio
points(x=ret_sd[in_dex, "stddev"], y=ret_sd[in_dex, "returns"], col="blue", lwd=6)
text(x=ret_sd[in_dex, "stddev"], y=ret_sd[in_dex, "returns"],
     labels=paste(c("market portfolio\n", structure(c(weight_s[in_dex, 1], weight_s[in_dex, 2]), names=sym_bols)), collapse=" "),
     pos=3, cex=0.8)
# plot individual assets
mean_rets <- 252*sapply(re_turns, mean)
std_devs <- sqrt(252)*sapply(re_turns, sd)
points(std_devs, mean_rets, col="green", lwd=6)
text(std_devs, mean_rets, labels=names(re_turns), pos=2, cex=0.8)
# add point at risk-free rate and draw Capital Market Line
points(x=0, y=risk_free, col="blue", lwd=6)
text(0, risk_free, labels="risk-free", pos=4, cex=0.8)
abline(a=risk_free, b=max_Sharpe, col="blue", lwd=2)
range_s <- par("usr")
text(max(ret_sd[, "stddev"])/3, 0.75*max(ret_sd[, "returns"]),
     labels="Capital Market Line", cex=0.8, , pos=3,
     srt=45*atan(max_Sharpe*(range_s[2]-range_s[1])/
             (range_s[4]-range_s[3])*
             hei_ght/wid_th)/(0.25*pi))
# plot portfolios in x11() window
x11(wid_th <- 6, hei_ght <- 5)
# calculate cumulative returns of VTI and IEF
optim_rets <- lapply(re_turns,
  function(re_turns) exp(cumsum(re_turns)))
optim_rets <- rutils::do_call(cbind, optim_rets)
# calculate market portfolio returns
optim_rets <- cbind(
  exp(cumsum(re_turns %*%
    c(weight_s[in_dex], 1-weight_s[in_dex]))),
  optim_rets)
colnames(optim_rets)[1] <- "market"
# plot market portfolio with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue", "green")
chart_Series(optim_rets, theme=plot_theme,
       name="Market portfolio for stocks and bonds")
legend("top", legend=colnames(optim_rets),
 cex=0.8, inset=0.1, bg="white", lty=c(1, 1),
 lwd=c(6, 6), col=plot_theme$col$line.col, bty="n")
x11(width=6, height=4)
par(mar=c(3, 2, 1, 0), oma=c(0, 0, 0, 0))
# VTI percentage returns
re_turns <- rutils::diff_xts(log(Ad(rutils::env_etf$VTI)))
conf_level <- 0.1
va_r <- quantile(re_turns, conf_level)
c_var <- mean(re_turns[re_turns < va_r])
# or
sort_ed <- sort(as.numeric(re_turns))
in_dex <- round(conf_level*NROW(re_turns))
va_r <- sort_ed[in_dex]
c_var <- mean(sort_ed[1:in_dex])
# plot histogram of VTI returns
histo_gram <- hist(re_turns, col="lightgrey",
  xlab="returns", breaks=100, xlim=c(-0.05, 0.01),
  ylab="frequency", freq=FALSE,
  main="VTI returns histogram")
dens_ity <- density(re_turns, adjust=1.5)
lines(dens_ity, lwd=3, col="blue")
# add line for VaR
abline(v=va_r, col="red", lwd=3)
text(x=va_r, y=20, labels="VaR",
     lwd=2, srt=90, pos=2)
# add shading for CVaR
var_max <- -0.06
rang_e <- (dens_ity$x < va_r) & (dens_ity$x > var_max)
polygon(
  c(var_max, dens_ity$x[rang_e], va_r),
  c(0, dens_ity$y[rang_e], 0),
  col=rgb(1, 0, 0,0.5), border=NA)
text(x=va_r, y=3, labels="CVaR", lwd=2, pos=2)
library(HighFreq)
library(Rglpk)
# vector of symbol names and returns
sym_bols <- c("VTI", "IEF", "DBC")
n_weights <- NROW(sym_bols)
re_turns <- rutils::env_etf$re_turns[((NROW(re_turns)-6):NROW(re_turns)), sym_bols]
mean_rets <- colMeans(re_turns)
conf_level <- 0.05
r_min <- 0 ; w_min <- 0 ; w_max <- 1
weight_sum <- 1
n_col <- NCOL(re_turns) # number of assets
n_row <- NROW(re_turns) # number of rows
# creat objective vector
obj_vector <- c(numeric(n_col), rep(-1/(conf_level*n_row), n_row), -1)
# specify weight constraints
constraint_s <- rbind(
  cbind(rbind(1, mean_rets),
  matrix(data=0, nrow=2, ncol=(n_row+1))),
  cbind(coredata(re_turns), diag(n_row), 1))
rh_s <- c(weight_sum, r_min, rep(0, n_row))
direction_s <- c("==", ">=", rep(">=", n_row))
# specify weight bounds
bound_s <- list(
  lower=list(ind=1:n_col, val=rep(w_min, n_col)),
  upper=list(ind=1:n_col, val=rep(w_max, n_col)))
# perform optimization
op_tim <- Rglpk_solve_LP(obj=obj_vector, mat=constraint_s, dir=direction_s, rhs=rh_s, types=rep("C", NROW(obj_vector)), max=T, bounds=bound_s)
op_tim$solution
constraint_s %*% op_tim$solution
obj_vector %*% op_tim$solution
as.numeric(op_tim$solution[1:n_col])
# calculate daily percentage re_turns
sym_bols <- c("VTI", "IEF", "DBC")
re_turns <- rutils::env_etf$re_turns[, sym_bols]
# create initial vector of portfolio weights
weight_s <- rep(1, NROW(sym_bols))
names(weight_s) <- sym_bols
# objective equal to minus Sharpe ratio
object_ive <- function(weight_s, re_turns) {
  portf_rets <- re_turns %*% weight_s
  if (sd(portf_rets) == 0)
    return(0)
  else
    return(-mean(portf_rets)/sd(portf_rets))
}  # end object_ive
# objective for equal weight portfolio
object_ive(weight_s, re_turns=re_turns)
op_tim <- unlist(optimize(
  f=function(weight)
    object_ive(c(1, 1, weight), re_turns=re_turns),
  interval=c(-4, 1)))
# vectorize objective function with respect to third weight
vec_object <- function(weights) sapply(weights,
  function(weight) object_ive(c(1, 1, weight),
    re_turns=re_turns))
# or
vec_object <- Vectorize(FUN=function(weight)
    object_ive(c(1, 1, weight), re_turns=re_turns),
  vectorize.args="weight")  # end Vectorize
vec_object(1)
vec_object(1:3)
x11(width=6, height=5)
par(oma=c(1, 1, 1, 1), mgp=c(2, 1, 0), mar=c(3, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# plot objective function with respect to third weight
curve(expr=vec_object,
      type="l", xlim=c(-4.0, 1.0),
      xlab=paste("weight of", names(weight_s[3])),
      ylab="", lwd=2)
title(main="Objective Function", line=-1)  # add title
points(x=op_tim[1], y=op_tim[2], col="green", lwd=6)
text(x=op_tim[1], y=op_tim[2],
     labels="minimum objective", pos=4, cex=0.8)

#below is simplified code for plotting objective function
# create vector of DBC weights
weight_s <- seq(from=-4, to=1, by=0.1)
obj_val <- sapply(weight_s,
  function(weight) object_ive(c(1, 1, weight)))
plot(x=weight_s, y=obj_val, t="l",
      xlab="weight of DBC", ylab="", lwd=2)
title(main="Objective Function", line=-1)  # add title
points(x=op_tim[1], y=op_tim[2], col="green", lwd=6)
text(x=op_tim[1], y=op_tim[2],
     labels="minimum objective", pos=4, cex=0.8)
# vectorize function with respect to all weights
vec_object <- Vectorize(
  FUN=function(w1, w2, w3)
    object_ive(c(w1, w2, w3)),
  vectorize.args=c("w2", "w3"))  # end Vectorize
# calculate objective on 2-d (w2 x w3) parameter grid
w2 <- seq(-3, 7, length=50)
w3 <- seq(-5, 5, length=50)
grid_object <- outer(w2, w3, FUN=vec_object, w1=1)
rownames(grid_object) <- round(w2, 2)
colnames(grid_object) <- round(w3, 2)
# perspective plot of objective function
persp(w2, w3, -grid_object,
theta=45, phi=30, shade=0.5,
col=rainbow(50), border="green",
main="objective function")
# interactive perspective plot of objective function
library(rgl)
rgl::persp3d(z=-grid_object, zlab="objective",
  col="green", main="objective function")
rgl::persp3d(
  x=function(w2, w3)
    -vec_object(w1=1, w2, w3),
  xlim=c(-3, 7), ylim=c(-5, 5),
  col="green", axes=FALSE)
# vector of initial portfolio weights equal to 1
weight_s <- rep(1, n_weights)
names(weight_s) <- sym_bols
# objective function equal to standard deviation of returns
object_ive <- function(weight_s) {
  portf_rets <- re_turns %*% weight_s
  sd(portf_rets)/sum(weight_s)
}  # end object_ive
# object_ive() for equal weight portfolio
object_ive(weight_s)
object_ive(2*weight_s)
# perform portfolio optimization
op_tim <- optim(par=weight_s,
          fn=object_ive,
          method="L-BFGS-B",
          upper=rep(10, n_weights),
          lower=rep(-10, n_weights))
# Rescale the optimal weights
weight_s <- op_tim$par/sum(op_tim$par)
# minimum variance portfolio returns
optim_rets <- xts(x=re_turns %*% weight_s,
            order.by=index(re_turns))
chart_Series(x=exp(cumsum(optim_rets)), name="minvar portfolio")
# add green point for minimum variance portfolio
optim_sd <- 100*sd(optim_rets)
optim_ret <- 100*mean(optim_rets)
points(x=optim_sd, y=optim_ret, col="green", lwd=6)
text(x=optim_sd, y=optim_ret, labels="minvar", pos=2, cex=0.8)


# objective function equal to minus Sharpe ratio
risk_free <- 0.03
object_ive <- function(weight_s) {
  portf_rets <- 100*env_etf$re_turns[, names(weight_s)] %*% weight_s / sum(weight_s)
  -mean(portf_rets-risk_free)/sd(portf_rets)
}  # end object_ive
# perform portfolio optimization
op_tim <- optim(par=weight_s,
             fn=object_ive,
             method="L-BFGS-B",
             upper=rep(10, n_weights),
             lower=rep(-10, n_weights))
# maximum Sharpe ratio portfolio returns
weight_s <- op_tim$par/sum(op_tim$par)
optim_rets <- xts(x=re_turns %*% weight_s,
            order.by=index(re_turns))
chart_Series(x=exp(cumsum(optim_rets)), name="maxSR portfolio")
optim_sd <- 100*sd(optim_rets)
optim_ret <- 100*mean(optim_rets)
points(x=optim_sd, y=optim_ret,
 col="blue", lwd=3)
text(x=optim_sd, y=optim_ret,
     labels="maxSR", pos=2, cex=0.8)
max_Sharpe <- (optim_ret-risk_free)/optim_sd
# plot individual assets
mean_rets <- 100*sapply(re_turns, mean)
std_devs <- 100*sapply(re_turns, sd)
points(std_devs, mean_rets, col="green", lwd=3)
text(std_devs, mean_rets, labels=names(mean_rets), pos=2, cex=0.8)
# add point at risk-free rate and draw Capital Market Line
points(x=0, y=risk_free)
text(0, risk_free, labels="risk-free", pos=4, cex=0.8)
abline(a=risk_free, b=max_Sharpe, col="blue")
range_s <- par("usr")
text(optim_sd/3, (optim_ret+risk_free)/2.5,
     labels="Capital Market Line", cex=0.8, , pos=3,
     srt=45*atan(max_Sharpe*(range_s[2]-range_s[1])/
             (range_s[4]-range_s[3])*
             hei_ght/wid_th)/(0.25*pi))
# optimization to find weights with maximum Sharpe ratio
op_tim <- optim(par=weight_s,
             fn=object_ive,
             method="L-BFGS-B",
             upper=c(1.1, 10, 10),
             lower=c(0.9, -10, -10))
# optimal parameters
op_tim$par
op_tim$par <- op_tim$par/sum(op_tim$par)
# optimal Sharpe ratio
-object_ive(op_tim$par)
x11(width=6, height=5)
par(oma=c(1, 1, 1, 0), mgp=c(2, 1, 0), mar=c(2, 1, 2, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# plot in two vertical panels
layout(matrix(c(1,2), 2),
 widths=c(1,1), heights=c(1,3))
# barplot of optimal portfolio weights
barplot(op_tim$par, col=c("red", "green", "blue"),
  main="Optimized portfolio weights")
# calculate cumulative returns of VTI, IEF, DBC
cum_rets <- lapply(re_turns,
  function(re_turns) exp(cumsum(re_turns)))
cum_rets <- rutils::do_call(cbind, cum_rets)
# calculate optimal portfolio returns with VTI, IEF, DBC
optim_rets <- cbind(
  exp(cumsum(re_turns %*% op_tim$par)),
  cum_rets)
colnames(optim_rets)[1] <- "optim_rets"
# plot optimal returns with VTI, IEF, DBC
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("black", "red", "green", "blue")
chart_Series(optim_rets, theme=plot_theme,
       name="Optimized portfolio performance")
legend("top", legend=colnames(optim_rets), cex=0.8,
 inset=0.1, bg="white", lty=c(1, 1), lwd=c(6, 6),
 col=plot_theme$col$line.col, bty="n")
# or plot non-compounded (simple) cumulative returns
PerformanceAnalytics::chart.CumReturns(
  cbind(re_turns %*% op_tim$par, re_turns),
  lwd=2, ylab="", legend.loc="topleft", main="")
risk_free <- 0.03
re_turns <- c(asset1=0.05, asset2=0.06)
std_devs <- c(asset1=0.4, asset2=0.5)
cor_rel <- 0.6
co_var <- matrix(c(1, cor_rel, cor_rel, 1), nc=2)
co_var <- t(t(std_devs*co_var)*std_devs)
library(quadprog)
# minimum variance weights without constraints
op_tim <- solve.QP(Dmat=2*co_var,
            dvec=rep(0, 2),
            Amat=matrix(0, nr=2, nc=1),
            bvec=0)
# minimum variance weights sum equal to 1
op_tim <- solve.QP(Dmat=2*co_var,
            dvec=rep(0, 2),
            Amat=matrix(1, nr=2, nc=1),
            bvec=1)
# optimal value of objective function
t(op_tim$solution) %*% co_var %*% op_tim$solution
perform simple optimization for reference
# objective function for simple optimization
object_ive <- function(x) {
  x <- c(x, 1-x)
  t(x) %*% co_var %*% x
}  # end object_ive
unlist(optimize(f=object_ive, interval=c(-1, 2)))
# calculate daily percentage re_turns
sym_bols <- c("VTI", "IEF", "DBC")
re_turns <- rutils::env_etf$re_turns[, sym_bols]
# calculate the covariance matrix
co_var <- cov(re_turns)
# minimum variance weights, with sum equal to 1
op_tim <- quadprog::solve.QP(Dmat=2*co_var,
            dvec=numeric(3),
            Amat=matrix(1, nr=3, nc=1),
            bvec=1)
# minimum variance, maximum returns
op_tim <- quadprog::solve.QP(Dmat=2*co_var,
            dvec=apply(0.1*re_turns, 2, mean),
            Amat=matrix(1, nr=3, nc=1),
            bvec=1)
# minimum variance positive weights, sum equal to 1
a_mat <- cbind(matrix(1, nr=3, nc=1),
       diag(3), -diag(3))
b_vec <- c(1, rep(0, 3), rep(-1, 3))
op_tim <- quadprog::solve.QP(Dmat=2*co_var,
            dvec=numeric(3),
            Amat=a_mat,
            bvec=b_vec,
            meq=1)
# calculate daily percentage re_turns
re_turns <- rutils::env_etf$re_turns[, sym_bols]
# objective equal to minus Sharpe ratio
object_ive <- function(weight_s, re_turns) {
  portf_rets <- re_turns %*% weight_s
  if (sd(portf_rets) == 0)
    return(0)
  else
    return(-mean(portf_rets)/sd(portf_rets))
}  # end object_ive
# perform optimization using DEoptim
op_tim <- DEoptim::DEoptim(fn=object_ive,
  upper=rep(10, NCOL(re_turns)),
  lower=rep(-10, NCOL(re_turns)),
  re_turns=re_turns,
  control=list(trace=FALSE, itermax=100, parallelType=1))
weight_s <- op_tim$optim$bestmem/sum(abs(op_tim$optim$bestmem))
names(weight_s) <- colnames(re_turns)
# objective with shrinkage penalty
object_ive <- function(weight_s, re_turns, lamb_da, al_pha) {
  portf_rets <- re_turns %*% weight_s
  if (sd(portf_rets) == 0)
    return(0)
  else {
    penal_ty <- lamb_da*((1-al_pha)*sum(weight_s^2) +
al_pha*sum(abs(weight_s)))
    return(-mean(portf_rets)/sd(portf_rets) + penal_ty)
  }
}  # end object_ive
# objective for equal weight portfolio
weight_s <- rep(1, NROW(sym_bols))
names(weight_s) <- sym_bols
lamb_da <- 0.5 ; al_pha <- 0.5
object_ive(weight_s, re_turns=re_turns,
  lamb_da=lamb_da, al_pha=al_pha)
# perform optimization using DEoptim
op_tim <- DEoptim::DEoptim(fn=object_ive,
  upper=rep(10, NCOL(re_turns)),
  lower=rep(-10, NCOL(re_turns)),
  re_turns=re_turns,
  lamb_da=lamb_da,
  al_pha=al_pha,
  control=list(trace=FALSE, itermax=100, parallelType=1))
weight_s <-
  op_tim$optim$bestmem/sum(abs(op_tim$optim$bestmem))
names(weight_s) <- colnames(re_turns)
load(file="C:/Develop/R/lecture_slides/data/zoo_data.RData")
ts_stx <- as.ts(zoo_stx)
class(ts_stx)
tail(ts_stx[, 1:4])
library(xts)
st_ox <- as.xts(zoo_stx)
class(st_ox)
tail(st_ox[, 1:4])
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
load(file="C:/Develop/R/lecture_slides/data/portf_optim.RData")
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
  risk.col="stddev",
  return.col="mean")
options(width=50)
library(PortfolioAnalytics)
load(file="C:/Develop/R/lecture_slides/data/portf_optim.RData")
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
load(file="C:/Develop/R/lecture_slides/data/portf_optim.RData")
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
load(file="C:/Develop/R/lecture_slides/data/portf_optim.RData")
maxSRN_DEOpt$weights
maxSRN_DEOpt$objective_measures$mean[1]
maxSRN_DEOpt$objective_measures$StdDev[[1]]
library(PortfolioAnalytics)
maxSRN_DEOpt_xts <-
  plot_portf(portfolio=maxSRN_DEOpt)
library(PerformanceAnalytics)
load(file="C:/Develop/R/lecture_slides/data/portf_optim.RData")
chart.CumReturns(
  cbind(maxSR_DEOpt_xts, maxSRN_DEOpt_xts),
  lwd=2, ylab="",
  legend.loc="topleft", main="")
options(width=50)
library(PerformanceAnalytics)
load(file="C:/Develop/R/lecture_slides/data/portf_optim.RData")
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
load(file="C:/Develop/R/lecture_slides/data/portf_optim.RData")
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
load(file="C:/Develop/R/lecture_slides/data/portf_optim.RData")
maxSTARR_DEOpt$weights
maxSTARR_DEOpt$objective_measures$mean[1]
maxSTARR_DEOpt$objective_measures$ES[[1]]
library(PortfolioAnalytics)
maxSTARR_DEOpt_xts <-
  plot_portf(portfolio=maxSTARR_DEOpt)
library(PerformanceAnalytics)
load(file="C:/Develop/R/lecture_slides/data/portf_optim.RData")
chart.CumReturns(
  cbind(maxSR_DEOpt_xts, maxSTARR_DEOpt_xts),
  lwd=2, ylab="",
  legend.loc="topleft", main="")
options(width=50)
library(PerformanceAnalytics)
load(file="C:/Develop/R/lecture_slides/data/portf_optim.RData")
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
load(file="C:/Develop/R/lecture_slides/data/portf_optim.RData")
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
load(file="C:/Develop/R/lecture_slides/data/portf_optim.RData")
minES_ROI$weights
minES_ROI$objective_measures$ES[[1]]
library(PortfolioAnalytics)
minES_ROI_xts <-
  plot_portf(portfolio=minES_ROI)
library(PerformanceAnalytics)
load(file="C:/Develop/R/lecture_slides/data/portf_optim.RData")
chart.CumReturns(
  cbind(maxSR_DEOpt_xts, minES_ROI_xts),
  lwd=2, ylab="",
  legend.loc="topleft", main="")
options(width=50)
library(PerformanceAnalytics)
load(file="C:/Develop/R/lecture_slides/data/portf_optim.RData")
rbind(maxSR_DEOpt$weights, minES_ROI$weights)
c(maxSR_DEOpt$objective_measures$mean,
minES_ROI$objective_measures$mean)
c(maxSR_DEOpt$objective_measures$StdDev[[1]],
minES_ROI$objective_measures$ES[[1]])
load(file="C:/Develop/R/lecture_slides/data/portf_optim.RData")
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
load(file="C:/Develop/R/lecture_slides/data/portf_optim.RData")
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
# symbols for constant maturity Treasury rates
sym_bols <- c("DGS1", "DGS2", "DGS5", "DGS10", "DGS20", "DGS30")
library(quantmod)  # load package quantmod
env_rates <- new.env()  # new environment for data
# download data for sym_bols into env_rates
getSymbols(sym_bols, env=env_rates, src="FRED")
ls(env_rates)  # list files in env_rates
# get class of object in env_rates
class(get(x=sym_bols[1], envir=env_rates))
# another way
class(env_rates$DGS10)
colnames(env_rates$DGS10)
save(env_rates, file="C:/Develop/R/lecture_slides/data/rates_data.RData")
x11(width=6, height=4)
par(mar=c(2, 2, 0, 0), oma=c(0, 0, 0, 0))
head(env_rates$DGS10, 3)
# get class of all objects in env_rates
eapply(env_rates, class)
# get class of all objects in R workspace
lapply(ls(), function(ob_ject) class(get(ob_ject)))
# plot 10-year constant maturity Treasury rate
chart_Series(env_rates$DGS10["1990/"],
  name="10-year constant maturity Treasury rate")
par(mar=c(3, 3, 2, 0), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
# load constant maturity Treasury rates
load(file="C:/Develop/R/lecture_slides/data/rates_data.RData")
# get end-of-year dates since 2006
date_s <- xts::endpoints(env_rates$DGS1["2006/"], on="years")
date_s <- zoo::index(env_rates$DGS1["2006/"])[date_s]
# create time series of end-of-year rates
rate_s <- eapply(env_rates, function(ra_te) ra_te[date_s])
rate_s <- rutils::do_call(cbind, rate_s)
# rename columns and rows, sort columns, and transpose into matrix
colnames(rate_s) <- substr(colnames(rate_s), start=4, stop=11)
rate_s <- rate_s[, order(as.numeric(colnames(rate_s)))]
colnames(rate_s) <- paste0(colnames(rate_s), "yr")
rate_s <- t(rate_s)
colnames(rate_s) <- substr(colnames(rate_s), start=1, stop=4)
# plot matrix using plot.zoo()
col_ors <- colorRampPalette(c("red", "blue"))(NCOL(rate_s))
plot.zoo(rate_s, main="Yield curve since 2006", lwd=3, xaxt="n",
   plot.type="single", xlab="maturity", ylab="yield", col=col_ors)
# add x-axis
axis(1, seq_along(rownames(rate_s)), rownames(rate_s))
# add legend
legend("bottomright", legend=colnames(rate_s),
 col=col_ors, lty=1, lwd=4, inset=0.05, cex=0.8)
# alternative plot using matplot()
matplot(rate_s, main="Yield curve since 2006", xaxt="n", lwd=3, lty=1,
  type="l", xlab="maturity", ylab="yield", col=col_ors)
# add x-axis
axis(1, seq_along(rownames(rate_s)), rownames(rate_s))
# add legend
legend("bottomright", legend=colnames(rate_s),
 col=col_ors, lty=1, lwd=4, inset=0.05, cex=0.8)
x11(width=6, height=4)
par(mar=c(0, 0, 0, 0), oma=c(0, 0, 0, 0), mgp=c(0, 0, 0))
# load constant maturity Treasury rates
load(file="C:/Develop/R/lecture_slides/data/rates_data.RData")
# symbols for constant maturity Treasury rates
sym_bols <- c("DGS1", "DGS2", "DGS5", "DGS10", "DGS20")
# calculate daily rates changes
rate_s <- zoo::na.locf(rutils::do_call(cbind,
    as.list(env_rates)[sym_bols]))
re_turns <- rutils::diff_xts(rate_s)
# correlation matrix of Treasury rates
cor_mat <- cor(re_turns)
# reorder correlation matrix based on clusters
library(corrplot)
or_der <- corrMatOrder(cor_mat, order="hclust",
  hclust.method="complete")
cor_mat <- cor_mat[or_der, or_der]
# plot the correlation matrix
col_ors <- colorRampPalette(c("red", "white", "blue"))
corrplot(cor_mat, title=NA, tl.col="black",
  tl.cex=0.8, mar=c(0,0,0,0), method="square",
  col=col_ors(8), cl.offset=0.75, cl.cex=0.7,
  cl.align.text="l", cl.ratio=0.25)
title("Correlation of Treasury rates", line=-1)
# draw rectangles on the correlation matrix plot
corrRect.hclust(cor_mat, k=NROW(cor_mat) %/% 2,
  method="complete", col="red")
# create initial vector of portfolio weights
n_weights <- NROW(sym_bols)
weight_s <- rep(1/sqrt(n_weights), n_weights)
names(weight_s) <- sym_bols
# objective function equal to minus portfolio variance
object_ive <- function(weight_s, re_turns) {
  portf_rets <- re_turns %*% weight_s
  -sum(portf_rets*portf_rets) +
    1000*(1 - sum(weight_s*weight_s))^2
}  # end object_ive
# objective for equal weight portfolio
object_ive(weight_s, re_turns)
# compare speed of vector multiplication methods
summary(microbenchmark(
  trans_pose=t(portf_rets) %*% portf_rets,
  s_um=sum(portf_rets*portf_rets),
  times=10))[, c(1, 4, 5)]
# find weights with maximum variance
optim_run <- optim(par=weight_s,
             fn=object_ive,
             re_turns=re_turns,
             method="L-BFGS-B",
             upper=rep(1.0, n_weights),
             lower=rep(-1.0, n_weights))
# optimal weights and maximum variance
weight_s <- optim_run$par
-object_ive(weight_s, re_turns)
# plot first principal component loadings
barplot(weight_s, names.arg=names(weight_s),
  xlab="", ylab="",
  main="first principal component loadings")
# pc1 weights and returns
weights_1 <- weight_s
pc_1 <- re_turns %*% weights_1
# redefine objective function
object_ive <- function(weight_s, re_turns) {
  portf_rets <- re_turns %*% weight_s
  -sum(portf_rets*portf_rets) +
    1000*(1 - sum(weight_s*weight_s))^2 +
    1000*sum(pc_1*portf_rets)^2
}  # end object_ive
# find second principal component weights
optim_run <- optim(par=weight_s,
             fn=object_ive,
             re_turns=re_turns,
             method="L-BFGS-B",
             upper=rep(1.0, n_weights),
             lower=rep(-1.0, n_weights))
# pc2 weights and returns
weights_2 <- optim_run$par
pc_2 <- re_turns %*% weights_2
sum(pc_1*pc_2)
# plot second principal component loadings
barplot(weights_2, names.arg=names(weights_2),
  xlab="", ylab="",
  main="second principal component loadings")
# perform principal component analysis PCA
pc_a <- prcomp(re_turns, scale=TRUE)
# plot standard deviations
barplot(pc_a$sdev,
  names.arg=colnames(pc_a$rotation),
  las=3, xlab="", ylab="",
  main="Scree Plot: Volatilities of Principal Components
  of Treasury rates")
x11(width=6, height=7)
# principal component loadings (weights)
pc_a$rotation
# plot loading barplots in multiple panels
par(mfrow=c(3,2))
par(mar=c(2, 2, 2, 1), oma=c(0, 0, 0, 0))
for (or_der in 1:NCOL(pc_a$rotation)) {
  barplot(pc_a$rotation[, or_der],
  las=3, xlab="", ylab="", main="")
  title(paste0("PC", or_der), line=-2.0,
  col.main="red")
}  # end for
# principal component time series
pca_ts <- xts(re_turns %*% pc_a$rotation,
          order.by=index(re_turns))
pca_ts <- cumsum(pca_ts)
# plot principal component time series in multiple panels
par(mfrow=c(3,2))
par(mar=c(2, 2, 0, 1), oma=c(0, 0, 0, 0))
ra_nge <- range(pca_ts)
for (or_der in 1:NCOL(pca_ts)) {
  plot.zoo(pca_ts[, or_der],
     ylim=ra_nge,
     xlab="", ylab="")
  title(paste0("PC", or_der), line=-2.0)
}  # end for
library(quantmod)  # load quantmod
library(RQuantLib)  # load RQuantLib
# specify curve parameters
curve_params <- list(tradeDate=as.Date("2017-01-17"),
               settleDate=as.Date("2017-01-19"),
               dt=0.25,
               interpWhat="discount",
               interpHow="loglinear")
# specify market data: prices of FI instruments
market_data <- list(d3m=0.0363,
              fut1=96.2875,
              fut2=96.7875,
              fut3=96.9875,
              fut4=96.6875,
              s5y=0.0443,
              s10y=0.05165,
              s15y=0.055175)
# specify dates for calculating the zero rates
disc_dates <- seq(0, 10, 0.25)
# specify the evaluation (as of) date
setEvaluationDate(as.Date("2017-01-17"))
# calculate the zero rates
disc_curves <- DiscountCurve(params=curve_params,
                       tsQuotes=market_data,
                       times=disc_dates)
# plot the zero rates
x11()
plot(x=disc_curves$zerorates, t="l", main="zerorates")
# calculate random default probabilities
num_assets <- 100
default_probs <- runif(num_assets, max=0.2)
mean(default_probs)
# calculate number of defaults
uni_form <- runif(num_assets)
sum(uni_form < default_probs)
# simulate average number of defaults
num_simu <- 1000
de_faults <- numeric(num_simu)
# simulate using for() loop (inefficient way)
for (i in 1:num_simu) {  # perform loop
  uni_form <- runif(num_assets)
  de_faults[i] <- sum(uni_form < default_probs)
}  # end for
# calculate average number of defaults
mean(de_faults)
# simulate using vectorized functions  (efficient way)
uni_form <- matrix(runif(num_simu*num_assets),
             ncol=num_simu)
sum(uni_form < default_probs)/num_simu
# plot Standard Normal distribution
curve(expr=dnorm(x),
type="l", xlim=c(-4, 4),
xlab="asset value", ylab="", lwd=2,
col="blue", main="Distribution of Asset Values")
abline(v=qnorm(0.025), col="red", lwd=2)
text(x=qnorm(0.025)-0.1, y=0.15,
 labels="default threshold",
 lwd=2, srt=90, pos=3)
# define correlation parameters
rh_o <- 0.2
rho_sqrt <- sqrt(rh_o) ; rho_sqrtm <- sqrt(1-rh_o)
num_assets <- 5 ; num_simu <- 10000
# calculate vector of systematic factors
system_atic <- rnorm(num_simu)
# simulate asset values using vectorized functions (efficient way)
asset_values <- rho_sqrt*system_atic +
  rho_sqrtm*rnorm(num_simu*num_assets)
dim(asset_values) <- c(num_simu, num_assets)
# calculate correlations between asset values
cor(asset_values)
# simulate asset values using for() loop (inefficient way)
# allocate matrix of assets
asset_values <- matrix(nr=num_simu, nc=num_assets)
# simulate asset values using for() loop
for (i in 1:num_simu) {  # perform loop
  asset_values[i, ] <-
    rho_sqrt*system_atic[i] +
    rho_sqrtm*rnorm(num_assets)
}  # end for
cor(asset_values)
# benchmark the speed of the two methods
library(microbenchmark)
summary(microbenchmark(
  for_loop={for (i in 1:num_simu) {
    rho_sqrt*system_atic[i] +
    rho_sqrtm*rnorm(num_assets)}},
  vector_ized={rho_sqrt*system_atic +
        rho_sqrtm*rnorm(num_simu*num_assets)},
  times=10))[, c(1, 4, 5)]
# calculate random default probabilities
num_assets <- 5
default_probs <- runif(num_assets, max=0.2)
mean(default_probs)
# calculate default thresholds
default_thresh <- qnorm(default_probs)
# calculate number of defaults using vectorized functions (efficient way)
# calculate vector of number of defaults
de_faults <-
  colSums(t(t(asset_values) < default_thresh))
de_faults / num_simu
default_probs
# calculate number of defaults using for() loop (inefficient way)
# allocate matrix of de_faults
de_faults <- matrix(nr=num_simu, nc=num_assets)
# simulate asset values using for() loop
for (i in 1:num_simu) {  # perform loop
  de_faults[i, ] <-
    (asset_values[i, ] < default_thresh)
}  # end for
colSums(de_faults) / num_simu
default_probs
# calculate correlations between defaults
cor(de_faults)
# define default probabilities
num_assets <- 2
default_prob <- 0.2
default_thresh <- qnorm(default_prob)
# define correlation parameters
rh_o <- 0.2
rho_sqrt <- sqrt(rh_o) ; rho_sqrtm <- sqrt(1-rh_o)
# calculate vector of systematic factors
num_simu <- 1000
system_atic <- rnorm(num_simu)
# simulate asset values using vectorized functions
asset_values <- rho_sqrt*system_atic +
  rho_sqrtm*rnorm(num_simu*num_assets)
dim(asset_values) <- c(num_simu, num_assets)
# calculate number of defaults using vectorized functions
de_faults <- t(t(asset_values) < default_thresh)
# calculate correlations between defaults
cor(de_faults)
# calculate averaage number of defaults and compare to default_prob
colSums(de_faults) / num_simu
default_prob
# define cumulative default probability function
def_prob <- function(x, def_thresh=qnorm(0.1), rh_o=0.1)
  pnorm((sqrt(1-rh_o)*qnorm(x) - def_thresh)/sqrt(rh_o))
def_prob(x=0.2, def_thresh=qnorm(0.2), rh_o=0.2)
# plot cumulative default probability function
curve(expr=def_prob(x, def_thresh=qnorm(0.4), rh_o=0.05),
xlim=c(0, 0.999), lwd=3,
xlab="percent default", ylab="probability",
col="green", main="Cumulative Default Probabilities")
# plot default distribution with higher correlation
curve(expr=def_prob(x, def_thresh=qnorm(0.4), rh_o=0.2),
xlim=c(0, 0.999), add=TRUE, lwd=3,
col="blue", main="")
# add legend
legend(x="topleft",
 legend=c("high correlation", "low correlation"),
 title=NULL, inset=0.05, cex=0.8, bg="white",
 bty="n", lwd=6, lty=c(1, 1), col=c("blue", "green"))
# add unconditional default probability
abline(v=0.4, col="red", lwd=3)
text(x=0.4, y=0.0,
 labels="default probability",
 lwd=2, srt=90, pos=4)
# define default probability density function
vasi_cek <- function(x, def_thresh=-2, rh_o=0.1)
  sqrt((1-rh_o)/rh_o)*exp(-(sqrt(1-rh_o)*qnorm(x) -
  def_thresh)^2/(2*rh_o) + qnorm(x)^2/2)
vasi_cek(0.03, def_thresh=qnorm(0.025), rh_o=0.1)
# plot probability distribution of defaults
curve(expr=vasi_cek(x, def_thresh=qnorm(0.025), rh_o=0.02),
xlim=c(0, 0.1), lwd=3,
xlab="percentage of defaults", ylab="density",
col="green", main="Distribution of Defaults")
# plot default distribution with higher correlation
curve(expr=vasi_cek(x, def_thresh=qnorm(0.025), rh_o=0.1),
xlab="default percentage", ylab="",
add=TRUE, lwd=3, col="blue", main="")
# add legend
legend(x="topright",
 legend=c("high correlation", "low correlation"),
 title=NULL, inset=0.05, cex=0.8, bg="white",
 bty="n", lwd=6, lty=c(1, 1), col=c("blue", "green"))
# add unconditional default probability
abline(v=0.025, col="red", lwd=3)
text(x=0.023, y=8,
 labels="default probability",
 lwd=2, srt=90, pos=3)
# plot default distribution with low correlation
curve(expr=vasi_cek(x, def_thresh=qnorm(0.1), rh_o=0.01),
xlab="default percentage", ylab="", lwd=2,
col="green", main="Distribution of Defaults")
# plot default distribution with high correlation
curve(expr=vasi_cek(x, def_thresh=qnorm(0.1), rh_o=0.99),
xlab="percentage of defaults", ylab="density",
add=TRUE, lwd=2, n=10001, col="blue", main="")
# add legend
legend(x="top",
 legend=c("high correlation", "low correlation"),
 title=NULL, inset=0.1, cex=0.8, bg="white",
 bty="n", lwd=6, lty=c(1, 1), col=c("blue", "green"))
# add unconditional default probability
abline(v=0.1, col="red", lwd=2)
text(x=0.1, y=10, lwd=2, pos=4,
 labels="default probability")
# define Vasicek loss distribution density function
portf_loss <- function(x, def_thresh=-2, rh_o=0.1, l_gd=0.4)
  sqrt((1-rh_o)/rh_o)*exp(-(sqrt(1-rh_o)*qnorm(x/l_gd) - def_thresh)^2/(2*rh_o) + qnorm(x/l_gd)^2/2)/l_gd
integrate(portf_loss, low=0, up=0.3,
  def_thresh=-2, rh_o=0.1, l_gd=0.4)
# plot probability distribution of losses
curve(expr=portf_loss(x, def_thresh=qnorm(0.06), rh_o=0.1),
type="l", xlim=c(0, 0.06),
xlab="loss percentage", ylab="density", lwd=3,
col="orange", main="Distribution of Losses")
# add line for expected loss
abline(v=0.02, col="red", lwd=3)
text(x=0.02-0.001, y=10, labels="expected loss",
 lwd=2, srt=90, pos=3)
# add lines for unexpected loss
abline(v=0.04, col="blue", lwd=3)
arrows(x0=0.02, y0=35, x1=0.04, y1=35,
 code=3, lwd=3, cex=0.5)
text(x=0.03, y=36, labels="unexpected loss",
     lwd=2, pos=3)
# add lines for VaR
abline(v=0.055, col="red", lwd=3)
arrows(x0=0.0, y0=25, x1=0.055, y1=25,
 code=3, lwd=3, cex=0.5)
text(x=0.03, y=26, labels="VaR", lwd=2, pos=3)
text(x=0.055-0.001, y=10, labels="VaR",
 lwd=2, srt=90, pos=3)
# plot probability distribution of losses
curve(expr=portf_loss(x, def_thresh=qnorm(0.1), rh_o=0.1),
type="l", xlim=c(0, 0.06),
xlab="loss percentage", ylab="density", lwd=3,
col="orange", main="Conditional Value at Risk")
# add line for expected loss
abline(v=0.02, col="red", lwd=3)
text(x=0.02-0.001, y=10, labels="expected loss",
 lwd=2, srt=90, pos=3)
# add lines for VaR
abline(v=0.04, col="red", lwd=3)
text(x=0.04-0.001, y=10, labels="VaR",
 lwd=2, srt=90, pos=3)
# add shading for CVaR
va_r <- 0.04; var_max <- 0.07
var_s <- seq(va_r, var_max, length=100)
dens_ity <- sapply(var_s, portf_loss,
  def_thresh=qnorm(0.1), rh_o=0.1)
# draw shaded polygon
polygon(c(va_r, var_s, var_max),
  c(-1, dens_ity, -1), col="red", border=NA)
text(x=0.045, y=0, labels="CVaR", lwd=2, pos=3)
# VaR (quantile of the loss distribution)
var_func <- function(x, def_thresh=qnorm(0.1), rh_o=0.1, l_gd=0.4)
  l_gd*pnorm((sqrt(rh_o)*qnorm(x) + def_thresh)/sqrt(1-rh_o))
var_func(x=0.99, def_thresh=qnorm(0.1), rh_o=0.2, l_gd=0.4)
# plot VaR
curve(expr=var_func(x, def_thresh=qnorm(0.1), rh_o=0.1, l_gd=0.4),
type="l", xlim=c(0, 0.999),
xlab="confidence level", ylab="VaR", lwd=3,
col="orange", main="VaR versus Confidence Level")
# add line for expected loss
abline(h=0.04, col="red", lwd=3)
text(x=0.2, y=0.04, labels="expected loss",
     lwd=2, pos=3)
# integrate portf_loss() over full range
integrate(portf_loss, low=0.0, up=0.3,
    def_thresh=qnorm(0.1), rh_o=0.1, l_gd=0.4)
# calculate expected losses using portf_loss()
integrate(function(x, ...) x*portf_loss(x, ...),
    low=0.0, up=0.3,
    def_thresh=qnorm(0.1), rh_o=0.1, l_gd=0.4)
# calculate confidence levels corresponding to VaR values
var_s <- seq(0.07, 0.12, 0.001)
conf_levels <- sapply(var_s, function(va_r, ...) {
  integrate(portf_loss, low=va_r, up=0.3, ...)
}, def_thresh=qnorm(0.1), rh_o=0.1, l_gd=0.4)  # end sapply
conf_levels <- cbind(as.numeric(t(conf_levels)[, 1]), var_s)
colnames(conf_levels) <- c("conf_levels", "VaRs")
# calculate 95% confidence level VaR value
conf_levels[
  match(TRUE, conf_levels[, "conf_levels"] < 0.05), "VaRs"]
plot(x=1-conf_levels[, "conf_levels"],
     y=conf_levels[, "VaRs"], lwd=2,
     xlab="conf_levels", ylab="VaRs",
     t="l", main="VaR values and confidence levels")
# calculate CVaR values
cvar_s <- sapply(var_s, function(va_r, ...) {
  integrate(function(x, ...) x*portf_loss(x, ...),
      low=va_r, up=0.3, ...)
}, def_thresh=qnorm(0.1), rh_o=0.1, l_gd=0.4)  # end sapply
conf_levels <- cbind(conf_levels, as.numeric(t(cvar_s)[, 1]))
colnames(conf_levels)[3] <- "CVaRs"
# divide CVaR by confidence level
conf_levels[, "CVaRs"] <-
  conf_levels[, "CVaRs"]/conf_levels[, "conf_levels"]
# calculate 95% confidence level CVaR value
conf_levels[match(TRUE,
  conf_levels[, "conf_levels"] < 0.05), "CVaRs"]
# plot CVaRs
plot(x=1-conf_levels[, "conf_levels"],
     y=conf_levels[, "CVaRs"],
     t="l", col="red", lwd=2,
     ylim=range(conf_levels[, c("VaRs", "CVaRs")]),
     xlab="conf_levels", ylab="CVaRs",
     main="CVaR values and confidence levels")
# add VaRs
lines(x=1-conf_levels[, "conf_levels"],
y=conf_levels[, "VaRs"], lwd=2)
# add legend
legend(x="topleft", legend=c("CVaRs", "VaRs"),
 title="default probability = 10%
correlation = 10%
loss given default = 40%",
 inset=0.1, cex=0.8, bg="white", bty="n",
 lwd=6, lty=c(1, 1), col=c("red", "black"))
# Define model parameters
num_assets <- 300
num_simu <- 1000
l_gd <- 0.4
# define correlation parameters
rh_o <- 0.2
rho_sqrt <- sqrt(rh_o)
rho_sqrtm <- sqrt(1-rh_o)
# calculate default probabilities and thresholds
set.seed(1121)
default_probs <- runif(num_assets, max=0.2)
default_thresh <- qnorm(default_probs)
# calculate vector of systematic factors
system_atic <- rnorm(num_simu)
# simulate losses under Vasicek model
asset_values <- matrix(rnorm(num_simu*num_assets), ncol=num_simu)
asset_values <- t(rho_sqrt*system_atic + t(rho_sqrtm*asset_values))
loss_es <-
  l_gd*colSums(asset_values < default_thresh)/num_assets
# calculate VaRs
conf_levels <- seq(0.93, 0.99, 0.01)
var_s <- quantile(loss_es, probs=conf_levels)
plot(x=conf_levels, y=var_s, t="l", lwd=2,
     main="Simulated VaR and confidence levels")
# calculate CVaRs
cvar_s <- sapply(var_s, function(va_r) {
  mean(loss_es[loss_es>va_r])
})  # end sapply
cvar_s <- cbind(cvar_s, var_s)
# alternative CVaR calculation using frequency table
# first calculate frequency table of loss_es
table_losses <- table(loss_es)/num_simu
# calculate CVaRs from frequency table
cvar_s <- sapply(var_s, function(va_r) {
  tai_l <- table_losses[names(table_losses) > va_r]
  tai_l %*% as.numeric(names(tai_l)) / sum(tai_l)
})  # end sapply
# plot CVaRs
plot(x=rownames(cvar_s), y=cvar_s[, "cvar_s"],
     t="l", col="red", lwd=2,
     ylim=range(cvar_s),
     xlab="conf_levels", ylab="CVaRs",
     main="Simulated CVaR and confidence levels")
# add VaRs
lines(x=rownames(cvar_s), y=cvar_s[, "var_s"], lwd=2)
# add legend
legend(x="topleft", legend=c("CVaRs", "VaRs"), bty="n",
 title=NULL, inset=0.05, cex=0.8, bg="white",
 lwd=6, lty=c(1, 1), col=c("red", "black"))
calc_var <- function(default_thresh,
               l_gd=0.6,
               rho_sqrt,
               rho_sqrtm,
               num_simu=1000,
               conf_levels=seq(0.93, 0.99, 0.01)) {
  # Define model parameters
  num_assets <- NROW(default_thresh)
  # Simulate losses under Vasicek model
  system_atic <- rnorm(num_simu)
  asset_values <- matrix(rnorm(num_simu*num_assets), ncol=num_simu)
  asset_values <- t(rho_sqrt*system_atic + t(rho_sqrtm*asset_values))
  loss_es <- l_gd*colSums(asset_values < default_thresh)/num_assets
  # Calculate VaRs and CVaRs
  var_s <- quantile(loss_es, probs=conf_levels)
  cvar_s <- sapply(var_s, function(va_r) {
    mean(loss_es[loss_es>va_r])
  })  # end sapply
  names(cvar_s) <- names(var_s)
  c(var_s, cvar_s)
}  # end calc_var
# define number of bootstrap simulations
num_boot <- 500
num_assets <- NROW(default_probs)
# perform bootstrap of calc_var
set.seed(1121)
boot_strap <- sapply(rep(l_gd, num_boot),
  calc_var,
  default_thresh=qnorm(default_probs),
  rho_sqrt=rho_sqrt,
  rho_sqrtm=rho_sqrtm,
  num_simu=num_simu,
  conf_levels=conf_levels)  # end sapply
boot_strap <- t(boot_strap)
# calculate vectors of standard errors of VaR and CVaR from boot_strap data
std_error_var <- apply(boot_strap[, 1:7], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
std_error_cvar <- apply(boot_strap[, 8:14], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
# scale the standard errors of VaRs and CVaRs
std_error_var[2, ] <- std_error_var[2, ]/std_error_var[1, ]
std_error_cvar[2, ] <- std_error_cvar[2, ]/std_error_cvar[1, ]
# plot the standard errors of VaRs and CVaRs
plot(x=colnames(std_error_cvar),
  y=std_error_cvar[2, ], t="l", col="red", lwd=2,
  ylim=range(c(std_error_var[2, ], std_error_cvar[2, ])),
  xlab="conf_levels", ylab="CVaRs",
  main="Scaled standard errors of CVaR and VaR")
lines(x=colnames(std_error_var), y=std_error_var[2, ], lwd=2)
legend(x="topleft", legend=c("CVaRs", "VaRs"), bty="n",
 title=NULL, inset=0.05, cex=0.8, bg="white",
 lwd=6, lty=c(1, 1), col=c("red", "black"))
library(parallel)  # load package parallel
num_cores <- detectCores() - 1  # number of cores
clus_ter <- makeCluster(num_cores)  # initialize compute cluster
# perform bootstrap of calc_var for Windows
set.seed(1121)
boot_strap <- parLapply(clus_ter, rep(l_gd, num_boot),
  fun=calc_var, default_probs=default_probs,
  rh_o=rh_o, num_simu=num_simu,
  conf_levels=conf_levels)  # end parLapply
# bootstrap under Mac-OSX or Linux
boot_strap <- mclapply(rep(l_gd, num_boot),
  FUN=calc_var, default_probs=default_probs,
  rh_o=rh_o, num_simu=num_simu,
  conf_levels=conf_levels)  # end mclapply
boot_strap <- rutils::do_call(rbind, boot_strap)
stopCluster(clus_ter)  # stop R processes over cluster
# calculate vectors of standard errors of VaR and CVaR from boot_strap data
std_error_var <- apply(boot_strap[, 1:7], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
std_error_cvar <- apply(boot_strap[, 8:14], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
# scale the standard errors of VaRs and CVaRs
std_error_var[2, ] <- std_error_var[2, ]/std_error_var[1, ]
std_error_cvar[2, ] <- std_error_cvar[2, ]/std_error_cvar[1, ]
# plot the standard errors of VaRs and CVaRs
plot(x=colnames(std_error_cvar),
  y=std_error_cvar[2, ], t="l", col="red", lwd=2,
  ylim=range(c(std_error_var[2, ], std_error_cvar[2, ])),
  xlab="conf_levels", ylab="CVaRs",
  main="Scaled standard errors of CVaR and VaR")
lines(x=colnames(std_error_var), y=std_error_var[2, ], lwd=2)
legend(x="topleft", legend=c("CVaRs", "VaRs"), bty="n",
 title=NULL, inset=0.05, cex=0.8, bg="white",
 lwd=6, lty=c(1, 1), col=c("red", "black"))
