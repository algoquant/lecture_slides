# Linear constraint
weight_s <- weight_s/sum(weight_s)
# Quadratic constraint
weight_s <- weight_s/sqrt(sum(weight_s^2))
# Box constraints
weight_s[weight_s > 1] <- 1
weight_s[weight_s < 0] <- 0

library(quantmod)
library(Rglpk)
# Vector of symbol names
sym_bols <- c("VTI", "IEF", "DBC")
n_weights <- NROW(sym_bols)
# Calculate mean returns
re_turns <- rutils::etf_env$re_turns[, sym_bols]
re_turns <- xts:::na.locf.xts(re_turns)
re_turns <- na.omit(re_turns)
mean_rets <- colMeans(re_turns)
# Specify weight constraints
constraint_s <- matrix(c(rep(1, n_weights),
                 1, 1, 0),
                 nc=n_weights, byrow=TRUE)
direction_s <- c("==", "<=")
rh_s <- c(1, 0)
# Specify weight bounds (-1, 1) (default is c(0, Inf))
bound_s <-
  list(lower=list(ind=1:n_weights, val=rep(-1, n_weights)),
 upper=list(ind=1:n_weights, val=rep(1, n_weights)))
# Perform optimization
op_tim <- Rglpk::Rglpk_solve_LP(
  obj=mean_rets,
  mat=constraint_s,
  dir=direction_s,
  rhs=rh_s,
  bounds=bound_s,
  max=TRUE)
unlist(op_tim[1:2])

# Calculate covariance matrix of returns and its inverse
cov_mat <- cov(re_turns)
cov_inv <- solve(a=cov_mat)
u_nit <- rep(1, NCOL(cov_mat))
# Minimum variance weights with constraint
# weight_s <- solve(a=cov_mat, b=u_nit)
weight_s <- cov_inv %*% u_nit
weight_s <- weight_s / drop(t(u_nit) %*% weight_s)
# Minimum variance
t(weight_s) %*% cov_mat %*% weight_s
1/(t(u_nit) %*% cov_inv %*% u_nit)

# Calculate vector of mean returns
mean_rets <- colMeans(re_turns)
# Specify the target return
tar_get <- 1.5*mean(re_turns)
# Products of inverse with mean returns and unit vector
f_mat <- matrix(c(
  t(u_nit) %*% cov_inv %*% u_nit,
  t(u_nit) %*% cov_inv %*% mean_rets,
  t(mean_rets) %*% cov_inv %*% u_nit,
  t(mean_rets) %*% cov_inv %*% mean_rets), nc=2)
# Solve for the Lagrange multipliers
multipli_ers <-
  solve(a=f_mat, b=c(2, 2*tar_get))
# Calculate weights
weight_s <- drop(0.5*cov_inv %*%
  cbind(u_nit, mean_rets) %*% multipli_ers)
# Calculate constraints
all.equal(1, sum(weight_s))
all.equal(tar_get, sum(mean_rets*weight_s))

# Calculate portfolio return and standard deviation
portf_rets <- drop(re_turns %*% weight_s)
c(return=mean(portf_rets), sd=sd(portf_rets))
all.equal(mean(portf_rets), tar_get)
# Calculate portfolio variance
uu <- c(1, tar_get)
f_inv <- solve(f_mat)
all.equal(var(portf_rets), drop(t(uu) %*% f_inv %*% uu))
# Calculate vertex of variance parabola
weight_s <- drop(cov_inv %*% u_nit /
  drop(t(u_nit) %*% cov_inv %*% u_nit))
portf_rets <- drop(re_turns %*% weight_s)
v_rets <-
  drop(t(u_nit) %*% cov_inv %*% mean_rets /
  t(u_nit) %*% cov_inv %*% u_nit)
all.equal(mean(portf_rets), v_rets)
var_min <-
  drop(1/t(u_nit) %*% cov_inv %*% u_nit)
all.equal(var(portf_rets), var_min)

# Calculate efficient frontier
target_s <- v_rets*(1+seq(from=-1, to=1, by=0.1))
eff_front <- sapply(target_s, function(tar_get) {
  uu <- c(1, tar_get)
  sqrt(drop(t(uu) %*% f_inv %*% uu))
})  # end sapply
# Plot efficient frontier
x11(width=6, height=5)
plot(x=eff_front, y=target_s, t="l", col="blue", lwd=2,
     main="Efficient Frontier and Minimum Variance Portfolio",
     xlab="standard deviation", ylab="return")
points(x=sqrt(var_min), y=v_rets, col="green", lwd=6)
text(x=sqrt(var_min), y=v_rets, labels="minimum \nvariance",
     pos=4, cex=0.8)

# Calculate portfolio standard deviation
std_dev <- sqrt(drop(t(uu) %*% f_inv %*% uu))
# Calculate the slope of the tangent line
slop_e <- (std_dev*det(f_mat))/(f_mat[1, 1]*tar_get-f_mat[1, 2])
# Calculate the risk-free rate as intercept of the tangent line
risk_free <- tar_get - slop_e*std_dev
# Calculate the risk-free rate from target return
risk_free <- (tar_get*f_mat[1, 2]-f_mat[2, 2]) /
  (tar_get*f_mat[1, 1]-f_mat[1, 2])

# Plot efficient frontier
plot(x=eff_front, y=target_s, t="l", col="blue", lwd=2,
     xlim=c(0.0, max(eff_front)),
     main="Efficient Frontier and Tangency Portfolio",
     xlab="standard deviation", ylab="return")
# Plot minimum variance
points(x=sqrt(var_min), y=v_rets, col="green", lwd=6)
text(x=sqrt(var_min), y=v_rets, labels="minimum \nvariance",
     pos=4, cex=0.8)
# Plot tangent point
points(x=std_dev, y=tar_get, col="red", lwd=6)
text(x=std_dev, y=tar_get, labels="tangency\nportfolio", pos=2, cex=0.8)
# Plot risk-free point
points(x=0, y=risk_free, col="red", lwd=6)
text(x=0, y=risk_free, labels="risk-free", pos=4, cex=0.8)
# Plot tangent line
abline(a=risk_free, b=slop_e, lwd=2, col="green")

# Calculate excess re_turns
risk_free <- 0.03/252
ex_cess <- re_turns - risk_free
# Calculate covariance and inverse matrix
cov_mat <- cov(re_turns)
u_nit <- rep(1, NCOL(cov_mat))
cov_inv <- solve(a=cov_mat)
# Calculate mean excess returns
ex_cess <- sapply(ex_cess, mean)
# Weights of maximum Sharpe portfolio
# weight_s <- solve(a=cov_mat, b=re_turns)
weight_s <- cov_inv %*% ex_cess
weight_s <- weight_s/drop(t(u_nit) %*% weight_s)
# Sharpe ratios
sqrt(252)*sum(weight_s * ex_cess) /
  sqrt(drop(weight_s %*% cov_mat %*% weight_s))
sapply(re_turns - risk_free,
  function(x) sqrt(252)*mean(x)/sd(x))
weights_maxsharpe <- weight_s

library(quantmod)
# Calculate minimum variance weights
weight_s <- cov_inv %*% u_nit
weights_minvar <-
  weight_s / drop(t(u_nit) %*% weight_s)
# Calculate optimal portfolio returns
optim_rets <- xts(
  x=cbind(exp(cumsum(re_turns %*% weights_maxsharpe)),
    exp(cumsum(re_turns %*% weights_minvar))),
  order.by=index(re_turns))
colnames(optim_rets) <- c("maxsharpe", "minvar")
# Plot optimal portfolio returns, with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "green")
x11(width=6, height=5)
chart_Series(optim_rets, theme=plot_theme,
  name="Maximum Sharpe and
  Minimum Variance portfolios")
legend("top", legend=colnames(optim_rets), cex=0.8,
 inset=0.1, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")

x11(wid_th <- 6, hei_ght <- 6)
# Calculate minimum variance weights
weight_s <- cov_inv %*% u_nit
weight_s <- weight_s / drop(t(u_nit) %*% weight_s)
# Minimum standard deviation and return
std_dev <- sqrt(252*drop(weight_s %*% cov_mat %*% weight_s))
min_ret <- 252*sum(weight_s * mean_rets)
# Calculate maximum Sharpe portfolios
risk_free <- (min_ret * seq(-10, 10, by=0.1)^3)/252
eff_front <- sapply(risk_free, function(risk_free) {
  weight_s <- cov_inv %*% (mean_rets - risk_free)
  weight_s <- weight_s/drop(t(u_nit) %*% weight_s)
  # Portfolio return and standard deviation
  c(return=252*sum(weight_s * mean_rets),
    stddev=sqrt(252*drop(weight_s %*% cov_mat %*% weight_s)))
})  # end sapply
eff_front <- cbind(252*risk_free, t(eff_front))
colnames(eff_front)[1] <- "risk-free"
eff_front <- eff_front[is.finite(eff_front[, "stddev"]), ]
eff_front <- eff_front[order(eff_front[, "return"]), ]
# Plot maximum Sharpe portfolios
plot(x=eff_front[, "stddev"],
     y=eff_front[, "return"], t="l",
     xlim=c(0.0*std_dev, 3.0*std_dev),
     ylim=c(0.0*min_ret, 2.0*min_ret),
     main="Efficient Frontier and Capital Market Line",
     xlab="standard deviation", ylab="return")
points(x=eff_front[, "stddev"], y=eff_front[, "return"],
 col="red", lwd=3)

# Plot minimum variance portfolio
points(x=std_dev, y=min_ret, col="green", lwd=6)
text(std_dev, min_ret, labels="minimum \nvariance",
     pos=4, cex=0.8)
# Draw Capital Market Line
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

# Calculate random portfolios
n_portf <- 1000
ret_sd <- sapply(1:n_portf, function(in_dex) {
  weight_s <- runif(n_weights-1, min=-0.25, max=1.0)
  weight_s <- c(weight_s, 1-sum(weight_s))
  # Portfolio return and standard deviation
  c(return=252*sum(weight_s * mean_rets),
    stddev=sqrt(252*drop(weight_s %*% cov_mat %*% weight_s)))
})  # end sapply
# Plot scatterplot of random portfolios
x11(wid_th <- 6, hei_ght <- 6)
plot(x=ret_sd["stddev", ], y=ret_sd["return", ],
     main="Efficient Frontier and Random Portfolios",
     xlim=c(0.5*std_dev, 0.8*max(ret_sd["stddev", ])),
     xlab="standard deviation", ylab="return")
# Plot maximum Sharpe portfolios
lines(x=eff_front[, "stddev"],
     y=eff_front[, "return"], lwd=2)
points(x=eff_front[, "stddev"], y=eff_front[, "return"],
 col="red", lwd=3)
# Plot minimum variance portfolio
points(x=std_dev, y=min_ret, col="green", lwd=6)
text(std_dev, min_ret, labels="minimum\nvariance",
     pos=2, cex=0.8)
# Plot market portfolio
points(x=eff_front[in_dex, "stddev"],
 y=eff_front[in_dex, "return"], col="green", lwd=6)
text(x=eff_front[in_dex, "stddev"],
     y=eff_front[in_dex, "return"],
     labels="market\nportfolio",
     pos=2, cex=0.8)

# Plot individual assets
points(x=sqrt(252*diag(cov_mat)),
 y=252*mean_rets, col="blue", lwd=6)
text(x=sqrt(252*diag(cov_mat)), y=252*mean_rets,
     labels=names(mean_rets),
     col="blue", pos=1, cex=0.8)

risk_free <- 0.03
re_turns <- c(asset1=0.05, asset2=0.06)
std_devs <- c(asset1=0.4, asset2=0.5)
cor_rel <- 0.6
cov_mat <- matrix(c(1, cor_rel, cor_rel, 1), nc=2)
cov_mat <- t(t(std_devs*cov_mat)*std_devs)
weight_s <- seq(from=-1, to=2, length.out=31)
weight_s <- cbind(weight_s, 1-weight_s)
portf_rets <- weight_s %*% re_turns
portf_sd <-
  sqrt(rowSums(weight_s * (weight_s %*% cov_mat)))
sharpe_ratios <- (portf_rets-risk_free)/portf_sd
in_dex <- which.max(sharpe_ratios)
max_Sharpe <- max(sharpe_ratios)
# Plot efficient frontier
x11(wid_th <- 6, hei_ght <- 5)
par(mar=c(3,3,2,1)+0.1, oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
plot(portf_sd, portf_rets, t="l",
 main=paste0("Efficient frontier and CML for two assets\ncorrelation = ", 100*cor_rel, "%"),
 xlab="standard deviation", ylab="return",
 lwd=2, col="orange",
 xlim=c(0, max(portf_sd)),
 ylim=c(0.02, max(portf_rets)))
# Add Market Portfolio (maximum Sharpe ratio portfolio)
points(portf_sd[in_dex], portf_rets[in_dex],
 col="blue", lwd=3)
text(x=portf_sd[in_dex], y=portf_rets[in_dex],
     labels=paste(c("market portfolio\n",
 structure(c(weight_s[in_dex], 1-weight_s[in_dex]),
         names=names(re_turns))), collapse=" "),
     pos=2, cex=0.8)

# Plot individual assets
points(std_devs, re_turns, col="green", lwd=3)
text(std_devs, re_turns, labels=names(re_turns), pos=4, cex=0.8)
# Add point at risk-free rate and draw Capital Market Line
points(x=0, y=risk_free, col="blue", lwd=3)
text(0, risk_free, labels="risk-free\nrate", pos=4, cex=0.8)
abline(a=risk_free, b=max_Sharpe, lwd=2, col="blue")
range_s <- par("usr")
text(portf_sd[in_dex]/2, (portf_rets[in_dex]+risk_free)/2,
     labels="Capital Market Line", cex=0.8, , pos=3,
     srt=45*atan(max_Sharpe*(range_s[2]-range_s[1])/
             (range_s[4]-range_s[3])*
             hei_ght/wid_th)/(0.25*pi))

# Plot portfolios in x11() window
x11(wid_th <- 6, hei_ght <- 5)
par(oma=c(0, 0, 0, 0), mar=c(3,3,2,1)+0.1, mgp=c(2, 1, 0), cex.lab=1.0, cex.axis=1.0, cex.main=1.0, cex.sub=1.0)
# Vector of symbol names
sym_bols <- c("VTI", "IEF")
# Matrix of portfolio weights
weight_s <- seq(from=-1, to=2, length.out=31)
weight_s <- cbind(weight_s, 1-weight_s)
# Calculate portfolio returns and volatilities
re_turns <- rutils::etf_env$re_turns[, sym_bols]
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
# Add blue point for market portfolio
points(x=ret_sd[in_dex, "stddev"], y=ret_sd[in_dex, "returns"], col="blue", lwd=6)
text(x=ret_sd[in_dex, "stddev"], y=ret_sd[in_dex, "returns"],
     labels=paste(c("market portfolio\n", structure(c(weight_s[in_dex, 1], weight_s[in_dex, 2]), names=sym_bols)), collapse=" "),
     pos=3, cex=0.8)

# Plot individual assets
mean_rets <- 252*sapply(re_turns, mean)
std_devs <- sqrt(252)*sapply(re_turns, sd)
points(std_devs, mean_rets, col="green", lwd=6)
text(std_devs, mean_rets, labels=names(re_turns), pos=2, cex=0.8)
# Add point at risk-free rate and draw Capital Market Line
points(x=0, y=risk_free, col="blue", lwd=6)
text(0, risk_free, labels="risk-free", pos=4, cex=0.8)
abline(a=risk_free, b=max_Sharpe, col="blue", lwd=2)
range_s <- par("usr")
text(max(ret_sd[, "stddev"])/3, 0.75*max(ret_sd[, "returns"]),
     labels="Capital Market Line", cex=0.8, , pos=3,
     srt=45*atan(max_Sharpe*(range_s[2]-range_s[1])/
             (range_s[4]-range_s[3])*
             hei_ght/wid_th)/(0.25*pi))

# Plot portfolios in x11() window
x11(wid_th <- 6, hei_ght <- 5)
# Calculate cumulative returns of VTI and IEF
optim_rets <- lapply(re_turns,
  function(re_turns) exp(cumsum(re_turns)))
optim_rets <- rutils::do_call(cbind, optim_rets)
# Calculate market portfolio returns
optim_rets <- cbind(
  exp(cumsum(re_turns %*%
    c(weight_s[in_dex], 1-weight_s[in_dex]))),
  optim_rets)
colnames(optim_rets)[1] <- "market"
# Plot market portfolio with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue", "green")
chart_Series(optim_rets, theme=plot_theme,
       name="Market portfolio for stocks and bonds")
legend("top", legend=colnames(optim_rets),
 cex=0.8, inset=0.1, bg="white", lty=1,
 lwd=6, col=plot_theme$col$line.col, bty="n")

x11(width=6, height=4)
par(mar=c(3, 2, 1, 0), oma=c(0, 0, 0, 0))
# VTI percentage returns
re_turns <- rutils::diff_it(log(Ad(rutils::etf_env$VTI)))
conf_level <- 0.1
va_r <- quantile(re_turns, conf_level)
c_var <- mean(re_turns[re_turns < va_r])
# Or
sort_ed <- sort(as.numeric(re_turns))
in_dex <- round(conf_level*NROW(re_turns))
va_r <- sort_ed[in_dex]
c_var <- mean(sort_ed[1:in_dex])
# Plot histogram of VTI returns
histo_gram <- hist(re_turns, col="lightgrey",
  xlab="returns", breaks=100, xlim=c(-0.05, 0.01),
  ylab="frequency", freq=FALSE,
  main="VTI returns histogram")
densi_ty <- density(re_turns, adjust=1.5)
lines(densi_ty, lwd=3, col="blue")

# Add line for VaR
abline(v=va_r, col="red", lwd=3)
text(x=va_r, y=20, labels="VaR",
     lwd=2, srt=90, pos=2)
# Add shading for CVaR
var_max <- -0.06
rang_e <- (densi_ty$x < va_r) & (densi_ty$x > var_max)
polygon(
  c(var_max, densi_ty$x[rang_e], va_r),
  c(0, densi_ty$y[rang_e], 0),
  col=rgb(1, 0, 0,0.5), border=NA)
text(x=va_r, y=3, labels="CVaR", lwd=2, pos=2)

library(HighFreq)
library(Rglpk)
# Vector of symbol names and returns
sym_bols <- c("VTI", "IEF", "DBC")
n_weights <- NROW(sym_bols)
re_turns <- rutils::etf_env$re_turns[((NROW(re_turns)-6):NROW(re_turns)), sym_bols]
mean_rets <- colMeans(re_turns)
conf_level <- 0.05
r_min <- 0 ; w_min <- 0 ; w_max <- 1
weight_sum <- 1
n_cols <- NCOL(re_turns) # number of assets
n_rows <- NROW(re_turns) # number of rows
# Creat objective vector
obj_vector <- c(numeric(n_cols), rep(-1/(conf_level*n_rows), n_rows), -1)
# Specify weight constraints
constraint_s <- rbind(
  cbind(rbind(1, mean_rets),
  matrix(data=0, nrow=2, ncol=(n_rows+1))),
  cbind(coredata(re_turns), diag(n_rows), 1))
rh_s <- c(weight_sum, r_min, rep(0, n_rows))
direction_s <- c("==", ">=", rep(">=", n_rows))
# Specify weight bounds
bound_s <- list(
  lower=list(ind=1:n_cols, val=rep(w_min, n_cols)),
  upper=list(ind=1:n_cols, val=rep(w_max, n_cols)))
# Perform optimization
op_tim <- Rglpk_solve_LP(obj=obj_vector, mat=constraint_s, dir=direction_s, rhs=rh_s, types=rep("C", NROW(obj_vector)), max=T, bounds=bound_s)
op_tim$solution
constraint_s %*% op_tim$solution
obj_vector %*% op_tim$solution
as.numeric(op_tim$solution[1:n_cols])

# Calculate daily percentage re_turns
sym_bols <- c("VTI", "IEF", "DBC")
re_turns <- rutils::etf_env$re_turns[, sym_bols]
# Create initial vector of portfolio weights
weight_s <- rep(1, NROW(sym_bols))
names(weight_s) <- sym_bols
# Objective equal to minus Sharpe ratio
object_ive <- function(weight_s, re_turns) {
  portf_rets <- re_turns %*% weight_s
  if (sd(portf_rets) == 0)
    return(0)
  else
    return(-mean(portf_rets)/sd(portf_rets))
}  # end object_ive
# Objective for equal weight portfolio
object_ive(weight_s, re_turns=re_turns)
op_tim <- unlist(optimize(
  f=function(weight)
    object_ive(c(1, 1, weight), re_turns=re_turns),
  interval=c(-4, 1)))
# Vectorize objective function with respect to third weight
vec_object <- function(weights) sapply(weights,
  function(weight) object_ive(c(1, 1, weight),
    re_turns=re_turns))
# Or
vec_object <- Vectorize(FUN=function(weight)
    object_ive(c(1, 1, weight), re_turns=re_turns),
  vectorize.args="weight")  # end Vectorize
vec_object(1)
vec_object(1:3)

x11(width=6, height=5)
par(oma=c(1, 1, 1, 1), mgp=c(2, 1, 0), mar=c(3, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# Plot objective function with respect to third weight
curve(expr=vec_object,
      type="l", xlim=c(-4.0, 1.0),
      xlab=paste("weight of", names(weight_s[3])),
      ylab="", lwd=2)
title(main="Objective Function", line=-1)  # Add title
points(x=op_tim[1], y=op_tim[2], col="green", lwd=6)
text(x=op_tim[1], y=op_tim[2],
     labels="minimum objective", pos=4, cex=0.8)

#below is simplified code for plotting objective function
# Create vector of DBC weights
weight_s <- seq(from=-4, to=1, by=0.1)
obj_val <- sapply(weight_s,
  function(weight) object_ive(c(1, 1, weight)))
plot(x=weight_s, y=obj_val, t="l",
      xlab="weight of DBC", ylab="", lwd=2)
title(main="Objective Function", line=-1)  # Add title
points(x=op_tim[1], y=op_tim[2], col="green", lwd=6)
text(x=op_tim[1], y=op_tim[2],
     labels="minimum objective", pos=4, cex=0.8)

# Vectorize function with respect to all weights
vec_object <- Vectorize(
  FUN=function(w1, w2, w3)
    object_ive(c(w1, w2, w3)),
  vectorize.args=c("w2", "w3"))  # end Vectorize
# Calculate objective on 2-d (w2 x w3) parameter grid
w2 <- seq(-3, 7, length=50)
w3 <- seq(-5, 5, length=50)
grid_object <- outer(w2, w3, FUN=vec_object, w1=1)
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
  x=function(w2, w3)
    -vec_object(w1=1, w2, w3),
  xlim=c(-3, 7), ylim=c(-5, 5),
  col="green", axes=FALSE)

# Optimization to find weights with maximum Sharpe ratio
op_tim <- optim(par=weight_s,
             fn=object_ive,
             method="L-BFGS-B",
             re_turns=re_turns,
             upper=c(1.1, 10, 10),
             lower=c(0.9, -10, -10))
# Optimal parameters
op_tim$par
op_tim$par <- op_tim$par/sum(op_tim$par)
# Optimal Sharpe ratio
-object_ive(op_tim$par)

x11(width=6, height=5)
par(oma=c(1, 1, 1, 0), mgp=c(2, 1, 0), mar=c(2, 1, 2, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# Plot in two vertical panels
layout(matrix(c(1,2), 2),
 widths=c(1,1), heights=c(1,3))
# barplot of optimal portfolio weights
barplot(op_tim$par, col=c("red", "green", "blue"),
  main="Optimized portfolio weights")
# Calculate cumulative returns of VTI, IEF, DBC
cum_rets <- lapply(re_turns,
  function(re_turns) exp(cumsum(re_turns)))
cum_rets <- rutils::do_call(cbind, cum_rets)
# Calculate optimal portfolio returns with VTI, IEF, DBC
optim_rets <- cbind(
  exp(cumsum(re_turns %*% op_tim$par)),
  cum_rets)
colnames(optim_rets)[1] <- "optim_rets"
# Plot optimal returns with VTI, IEF, DBC
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("black", "red", "green", "blue")
chart_Series(optim_rets, theme=plot_theme,
       name="Optimized portfolio performance")
legend("top", legend=colnames(optim_rets), cex=0.8,
 inset=0.1, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")
# Or plot non-compounded (simple) cumulative returns
PerformanceAnalytics::chart.CumReturns(
  cbind(re_turns %*% op_tim$par, re_turns),
  lwd=2, ylab="", legend.loc="topleft", main="")

risk_free <- 0.03
re_turns <- c(asset1=0.05, asset2=0.06)
std_devs <- c(asset1=0.4, asset2=0.5)
cor_rel <- 0.6
cov_mat <- matrix(c(1, cor_rel, cor_rel, 1), nc=2)
cov_mat <- t(t(std_devs*cov_mat)*std_devs)
library(quadprog)
# Minimum variance weights without constraints
op_tim <- solve.QP(Dmat=2*cov_mat,
            dvec=rep(0, 2),
            Amat=matrix(0, nr=2, nc=1),
            bvec=0)
# Minimum variance weights sum equal to 1
op_tim <- solve.QP(Dmat=2*cov_mat,
            dvec=rep(0, 2),
            Amat=matrix(1, nr=2, nc=1),
            bvec=1)
# Optimal value of objective function
t(op_tim$solution) %*% cov_mat %*% op_tim$solution
Perform simple optimization for reference
# Objective function for simple optimization
object_ive <- function(x) {
  x <- c(x, 1-x)
  t(x) %*% cov_mat %*% x
}  # end object_ive
unlist(optimize(f=object_ive, interval=c(-1, 2)))

# Calculate daily percentage re_turns
sym_bols <- c("VTI", "IEF", "DBC")
re_turns <- rutils::etf_env$re_turns[, sym_bols]
# Calculate the covariance matrix
cov_mat <- cov(re_turns)
# Minimum variance weights, with sum equal to 1
op_tim <- quadprog::solve.QP(Dmat=2*cov_mat,
            dvec=numeric(3),
            Amat=matrix(1, nr=3, nc=1),
            bvec=1)
# Minimum variance, maximum returns
op_tim <- quadprog::solve.QP(Dmat=2*cov_mat,
            dvec=apply(0.1*re_turns, 2, mean),
            Amat=matrix(1, nr=3, nc=1),
            bvec=1)
# Minimum variance positive weights, sum equal to 1
a_mat <- cbind(matrix(1, nr=3, nc=1),
       diag(3), -diag(3))
b_vec <- c(1, rep(0, 3), rep(-1, 3))
op_tim <- quadprog::solve.QP(Dmat=2*cov_mat,
            dvec=numeric(3),
            Amat=a_mat,
            bvec=b_vec,
            meq=1)

# Calculate daily percentage re_turns
re_turns <- rutils::etf_env$re_turns[, sym_bols]
# Objective equal to minus Sharpe ratio
object_ive <- function(weight_s, re_turns) {
  portf_rets <- re_turns %*% weight_s
  if (sd(portf_rets) == 0)
    return(0)
  else
    return(-mean(portf_rets)/sd(portf_rets))
}  # end object_ive
# Perform optimization using DEoptim
op_tim <- DEoptim::DEoptim(fn=object_ive,
  upper=rep(10, NCOL(re_turns)),
  lower=rep(-10, NCOL(re_turns)),
  re_turns=re_turns,
  control=list(trace=FALSE, itermax=100, parallelType=1))
weight_s <- op_tim$optim$bestmem/sum(abs(op_tim$optim$bestmem))
names(weight_s) <- colnames(re_turns)

# Objective with shrinkage penalty
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
# Objective for equal weight portfolio
weight_s <- rep(1, NROW(sym_bols))
names(weight_s) <- sym_bols
lamb_da <- 0.5 ; al_pha <- 0.5
object_ive(weight_s, re_turns=re_turns,
  lamb_da=lamb_da, al_pha=al_pha)
# Perform optimization using DEoptim
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

# sym_bols contains all the symbols in rutils::etf_env$re_turns except "VXX" and "SVXY"
sym_bols <- c("DBC", "IEF", "VTI", "XLE", "XLF", "XLI", "XLK", "XLP", "XLU", "XLV", "XLY")
# sym_bols <- colnames(rutils::etf_env$re_turns)
# sym_bols <- sym_bols[!(sym_bols %in% c("VXX", "SVXY", "MTUM"))]
# Extract columns of rutils::etf_env$re_turns and remove NA values
re_turns <- rutils::etf_env$re_turns[, sym_bols]
re_turns <- na.omit(re_turns)
# re_turns <- zoo::na.locf(re_turns, na.rm=FALSE)
# re_turns <- zoo::na.locf(re_turns, fromLast=TRUE)
# Calculate vector of monthly end points and start points
look_back <- 12
end_points <- rutils::calc_endpoints(re_turns, inter_val="months")
end_points[end_points < 2*NCOL(re_turns)] <- 2*NCOL(re_turns)

n_rows <- NROW(end_points)
# Sliding window
start_points <- c(rep_len(1, look_back-1), end_points[1:(n_rows-look_back+1)])
# OR expanding window
# Start_points <- rep_len(1, n_rows)
# risk_free is the daily risk-free rate
risk_free <- 0.03/252
# Calculate daily excess returns
ex_cess <- re_turns - risk_free
# Perform loop over end_points
portf_rets <- lapply(2:n_rows,
  function(i) {
    # Subset the ex_cess returns
    ex_cess <- ex_cess[start_points[i-1]:end_points[i-1], ]
    in_verse <- solve(cov(ex_cess))
    # Calculate the maximum Sharpe ratio portfolio weights
    weight_s <- in_verse %*% colMeans(ex_cess)
    weight_s <- drop(weight_s/sqrt(sum(weight_s^2)))
    # Subset the re_turns
    re_turns <- re_turns[(end_points[i-1]+1):end_points[i], ]
    # Calculate the out-of-sample portfolio returns
    xts(re_turns %*% weight_s, index(re_turns))
  }  # end anonymous function
)  # end lapply
portf_rets <- do.call(rbind, portf_rets)
colnames(portf_rets) <- "portf_rets"
# Calculate compounded cumulative portfolio returns
portf_rets <- cumprod(1 + portf_rets)
quantmod::chart_Series(portf_rets,
  name="Cumulative Returns of Max Sharpe Portfolio Strategy")

load("C:/Develop/lecture_slides/data/sp500_prices.RData")
n_cols <- NCOL(re_turns) ; date_s <- index(re_turns)
# Calculate returns on equal weight portfolio
in_dex <- xts(cumsum(re_turns %*% rep(1/n_cols, n_cols)), index(re_turns))
# Define monthly end points
end_points <- rutils::calc_endpoints(re_turns, inter_val="months")
end_points <- end_points[end_points > (n_cols+1)]
n_rows <- NROW(end_points) ; look_back <- 12
start_points <- c(rep_len(1, look_back-1), end_points[1:(n_rows-look_back+1)])
# Perform backtest
al_pha <- 0.01 ; max_eigen <- 3
pnl_s <- HighFreq::back_test(ex_cess=re_turns,
                       re_turns=re_turns,
                       start_points=start_points-1,
                       end_points=end_points-1,
                       al_pha=al_pha,
                       max_eigen=max_eigen)

# Plot strategy in log scale
pnl_s <- cumprod(1 + pnl_s)
pnl_s <- cbind(pnl_s, in_dex, (pnl_s+in_dex)/2)
col_names <- c("Strategy", "Index", "Average")
colnames(pnl_s) <- col_names
dygraphs::dygraph(pnl_s[end_points], main="Rolling S&P500 Portfolio Optimization Strategy") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="red", strokeWidth=1) %>%
  dySeries(name=col_names[2], axis="y2", col="blue", strokeWidth=1) %>%
  dySeries(name=col_names[3], axis="y2", col="green", strokeWidth=2)

# t-test for single sample
t.test(rnorm(100))
# t-test for two samples
t.test(rnorm(100),
       rnorm(100, mean=1))
# Plot the normal and t-distribution densities
x11(width=6, height=5)
par(mar=c(3, 3, 3, 1), oma=c(0, 0, 0, 0))
curve(expr=dnorm, xlim=c(-4, 4),
      xlab="", ylab="", lwd=3)
curve(expr=dt(x, df=3),
      xlab="", ylab="", lwd=3,
      col="red", add=TRUE)
# Add title
title(main="Normal and t-distribution densities", line=0.5)
# Add legend
legend("topright", inset=0.05, bty="n",
       title=NULL, c("normal", "t-dist"),
       cex=0.8, lwd=6, lty=1,
       col=c("black", "red"))

# KS-test for normal distribution
ks.test(rnorm(100), pnorm)
# KS-test for uniform distribution
ks.test(runif(100), pnorm)
# KS-test for two similar normal distributions
ks.test(rnorm(100), rnorm(100, mean=0.1))
# KS-test for two different normal distributions
ks.test(rnorm(100), rnorm(100, mean=1.0))

# Calculate DAX percentage returns
dax_rets <- diff(log(EuStockMarkets[, 1]))
# Shapiro-Wilk test for normal distribution
shapiro.test(rnorm(NROW(dax_rets)))
# Shapiro-Wilk test for DAX returns
shapiro.test(dax_rets)
# Shapiro-Wilk test for uniform distribution
shapiro.test(runif(NROW(dax_rets)))

library(tseries)  # Load package tseries
# Jarque-Bera test for normal distribution
jarque.bera.test(rnorm(NROW(dax_rets)))
# Jarque-Bera test for DAX returns
jarque.bera.test(dax_rets)
# Jarque-Bera test for uniform distribution
jarque.bera.test(runif(NROW(dax_rets)))

set.seed(1121)  # Reset random number generator
# Sample from Standard Normal Distribution
n_rows <- 1000
da_ta <- rnorm(n_rows)
# Sample mean - MC estimate
mean(da_ta)
# Sample standard deviation - MC estimate
sd(da_ta)
# Monte Carlo estimate of cumulative probability
da_ta <- sort(da_ta)
pnorm(1)
sum(da_ta<1)/n_rows
# Monte Carlo estimate of quantile
conf_level <- 0.99
qnorm(conf_level)
cut_off <- conf_level*n_rows
da_ta[cut_off]
quantile(da_ta, probs=conf_level)
# Analyze the source code of quantile()
stats:::quantile.default
# Microbenchmark quantile
library(microbenchmark)
summary(microbenchmark(
  monte_carlo=da_ta[cut_off],
  quan_tile=quantile(da_ta, probs=conf_level),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary

x11(width=6, height=5)
par(oma=c(1, 1, 1, 1), mar=c(2, 2, 2, 1), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
set.seed(1121)  # Reset random number generator
bar_rier <- 20  # Barrier level
n_rows <- 1000  # Number of simulation steps
pa_th <- numeric(n_rows)  # Allocate path vector
pa_th[1] <- 0  # Initialize path
in_dex <- 2  # Initialize simulation index
while ((in_dex <= n_rows) &&
 (pa_th[in_dex - 1] < bar_rier)) {
# Simulate next step
  pa_th[in_dex] <-
    pa_th[in_dex - 1] + rnorm(1)
  in_dex <- in_dex + 1  # Advance in_dex
}  # end while
# Fill remaining pa_th after it crosses bar_rier
if (in_dex <= n_rows)
  pa_th[in_dex:n_rows] <- pa_th[in_dex - 1]
# Create daily time series starting 2011
ts_path <- ts(data=pa_th, frequency=365, start=c(2011, 1))
plot(ts_path, type="l", col="black",
     lty="solid", lwd=2, xlab="", ylab="")
abline(h=bar_rier, lwd=2, col="red")
title(main="Brownian motion crossing a barrier level",
      line=0.5)

x11(width=6, height=5)
par(oma=c(1, 1, 1, 1), mar=c(2, 2, 2, 1), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
set.seed(1121)  # Reset random number generator
bar_rier <- 20  # Barrier level
n_rows <- 1000  # Number of simulation steps
# Simulate path of Brownian motion
pa_th <- cumsum(rnorm(n_rows))
# Find index when pa_th crosses bar_rier
cro_ss <- which(pa_th > bar_rier)
# Fill remaining pa_th after it crosses bar_rier
if (NROW(cro_ss)>0) {
  pa_th[(cro_ss[1]+1):n_rows] <-
    pa_th[cro_ss[1]]
}  # end if
# Create daily time series starting 2011
ts_path <- ts(data=pa_th, frequency=365,
     start=c(2011, 1))
# Create plot with horizontal line
plot(ts_path, type="l", col="black",
     lty="solid", lwd=2, xlab="", ylab="")
abline(h=bar_rier, lwd=2, col="red")
title(main="Brownian motion crossing a barrier level",
      line=0.5)

# Define daily volatility and growth rate
sigma_r <- 0.01; dri_ft <- 0.0; len_gth <- 1000
# Simulate geometric Brownian motion
re_turns <- sigma_r*rnorm(len_gth) +
  dri_ft - sigma_r^2/2
price_s <- exp(cumsum(re_turns))
plot(price_s, type="l",
     xlab="periods", ylab="prices",
     main="geometric Brownian motion")

# Simulate geometric Brownian motion
sigma_r <- 0.01/sqrt(48)
dri_ft <- 0.0
len_gth <- 1e4
in_dex <- seq(from=as.POSIXct(paste(Sys.Date()-250, "09:30:00")),
  length.out=len_gth, by="30 min")
price_s <- xts(exp(cumsum(sigma_r*rnorm(len_gth) + dri_ft - sigma_r^2/2)),
  order.by=in_dex)
price_s <- cbind(price_s,
  volume=sample(x=10*(2:18), size=len_gth, replace=TRUE))
# Aggregate to daily OHLC data
oh_lc <- xts::to.daily(price_s)
quantmod::chart_Series(oh_lc, name="random prices")
# dygraphs candlestick plot using pipes syntax
library(dygraphs)
dygraphs::dygraph(oh_lc[, 1:4]) %>%
  dyCandlestick()
# dygraphs candlestick plot without using pipes syntax
dygraphs::dyCandlestick(dygraphs::dygraph(oh_lc[, 1:4]))

# Standard deviations of log-normal distribution
sig_mas <- c(0.5, 1, 1.5)
# Create plot colors
col_ors <- c("black", "red", "blue")
# Plot all curves
for (in_dex in 1:NROW(sig_mas)) {
  curve(expr=dlnorm(x, sdlog=sig_mas[in_dex]),
  type="l", xlim=c(0, 3), lwd=2,
  xlab="", ylab="", col=col_ors[in_dex],
  add=as.logical(in_dex-1))
}  # end for

# Add title and legend
title(main="Log-normal Distributions", line=0.5)
legend("topright", inset=0.05, title="Sigmas",
 paste("sigma", sig_mas, sep="="),
 cex=0.8, lwd=2, lty=rep(1, NROW(sig_mas)),
 col=col_ors)

x11(width=6, height=5)
par(mar=c(4, 4, 3, 1))
# Return volatility of VTI ETF
sigma_r <- sd(rutils::diff_it(log(rutils::etf_env$VTI[, 4])))
sigmar_2 <- sigma_r^2
n_rows <- NROW(rutils::etf_env$VTI)
# Standard deviation of log-normal prices
sqrt(n_rows)*sigma_r

# Skewness of log-normal prices
skew_ness <- function(t) {
  ex_p <- exp(t*sigmar_2)
  (ex_p + 2)*sqrt(ex_p - 1)
}  # end skew_ness
curve(expr=skew_ness, xlim=c(1, n_rows), lwd=3,
xlab="Number of days", ylab="Skewness", col="blue",
main="Skewness of Log-normal Prices
as a Function of Time")

# Probability that random log-normal price will be lower than the mean price
curve(expr=pnorm(sigma_r*sqrt(x)/2),
xlim=c(1, n_rows), lwd=3,
xlab="Number of days", ylab="Probability", col="blue",
main="Probability That Random Log-normal Price
Will be Lower Than the Mean Price")

# Define daily volatility and growth rate
sigma_r <- 0.01; dri_ft <- 0.0; len_gth <- 5000
path_s <- 10
# Simulate multiple paths of geometric Brownian motion
price_s <- matrix(sigma_r*rnorm(path_s*len_gth) +
    dri_ft - sigma_r^2/2, nc=path_s)
price_s <- exp(matrixStats::colCumsums(price_s))
# Create xts time series
price_s <- xts(price_s, order.by=seq.Date(Sys.Date()-NROW(price_s)+1, Sys.Date(), by=1))
# Plot xts time series
col_ors <- colorRampPalette(c("red", "blue"))(NCOL(price_s))
col_ors <- col_ors[order(order(price_s[NROW(price_s), ]))]
par(mar=c(3, 3, 2, 2), oma=c(0, 0, 0, 0))
plot.zoo(price_s, main="Multiple paths of geometric Brownian motion",
   xlab=NA, ylab=NA, plot.type="single", col=col_ors)

# Define daily volatility and growth rate
sigma_r <- 0.01; dri_ft <- 0.0; len_gth <- 10000
path_s <- 100
# Simulate multiple paths of geometric Brownian motion
price_s <- matrix(sigma_r*rnorm(path_s*len_gth) +
    dri_ft - sigma_r^2/2, nc=path_s)
price_s <- exp(matrixStats::colCumsums(price_s))
# Calculate percentage of paths below the expected value
per_centage <- rowSums(price_s < 1.0) / path_s
# Create xts time series of percentage of paths below the expected value
per_centage <- xts(per_centage, order.by=seq.Date(Sys.Date()-NROW(per_centage)+1, Sys.Date(), by=1))
# Plot xts time series of percentage of paths below the expected value
par(mar=c(3, 3, 2, 2), oma=c(0, 0, 0, 0))
plot.zoo(per_centage, main="Percentage of GBM paths below mean",
   xlab=NA, ylab=NA, col="blue")

# Load S&P500 stock prices
load("C:/Develop/lecture_slides/data/sp500.RData")
ls(env_sp500)
# Extract closing prices
price_s <- eapply(env_sp500, quantmod::Cl)
# Flatten price_s into a single xts series
price_s <- rutils::do_call(cbind, price_s)
# Carry forward and backward non-NA prices
price_s <- xts:::na.locf.xts(price_s)
price_s <- xts:::na.locf.xts(price_s, fromLast=TRUE)
sum(is.na(price_s))
# Rename and normalize columns
colnames(price_s) <- sapply(colnames(price_s),
  function(col_name) strsplit(col_name, split="[.]")[[1]][1])
price_s <- xts(t(t(price_s) / as.numeric(price_s[1, ])),
         order.by=index(price_s))
# Calculate permution index for sorting the lowest to highest final price_s
or_der <- order(price_s[NROW(price_s), ])
# Select a few symbols
sym_bols <- colnames(price_s)[or_der]
sym_bols <- sym_bols[seq.int(from=1, to=(NROW(sym_bols)-1), length.out=20)]

# Plot xts time series of price_s
col_ors <- colorRampPalette(c("red", "blue"))(NROW(sym_bols))
col_ors <- col_ors[order(order(price_s[NROW(price_s), sym_bols]))]
par(mar=c(3, 3, 2, 2), oma=c(0, 0, 0, 0))
plot.zoo(price_s[, sym_bols], main="20 S&P500 stock prices (normalized)",
   xlab=NA, ylab=NA, plot.type="single", col=col_ors)
legend(x="topleft", inset=0.05, cex=0.8,
 legend=rev(sym_bols), col=rev(col_ors), lwd=6, lty=1)

# Calculate average of valid stock prices
val_id <- (price_s != 1)  # valid stocks
n_stocks <- rowSums(val_id)
n_stocks[1] <- NCOL(price_s)
in_dex <- rowSums(price_s * val_id) / n_stocks
# Calculate percentage of stock prices below the average price
per_centage <- rowSums((price_s < in_dex) & val_id) / n_stocks
# Create xts time series of average stock prices
in_dex <- xts(in_dex, order.by=index(price_s))

x11(width=6, height=4)
par(mar=c(3, 3, 2, 2), oma=c(0, 0, 0, 0))
# Plot xts time series of average stock prices
plot.zoo(in_dex, main="Average S&P500 stock prices (normalized from 1990)",
   xlab=NA, ylab=NA, col="blue")
# Create xts time series of percentage of stock prices below the average price
per_centage <- xts(per_centage, order.by=index(price_s))
# Plot percentage of stock prices below the average price
plot.zoo(per_centage[-(1:2),],
   main="Percentage of S&P500 stock prices below the average price",
   xlab=NA, ylab=NA, col="blue")

x11(width=6, height=4)
par(mar=c(4, 3, 1, 1), oma=c(0, 0, 0, 0))
library(zoo)
re_turns <-
  diff(log(as.numeric(EuStockMarkets[, 1])))
# acf() autocorrelation from package stats
acf(re_turns, lag=10, main="")
title(main="acf of DAX returns", line=-1)

# Ljung-Box test for DAX returns
# 'lag' is the number of autocorrelation coefficients
Box.test(re_turns, lag=10, type="Ljung")
library(Ecdat)  # Load Ecdat
macro_zoo <- as.zoo(Macrodat[, c("lhur", "fygm3")])
colnames(macro_zoo) <- c("unemprate", "3mTbill")
macro_diff <- na.omit(diff(macro_zoo))
# Changes in 3 month T-bill rate are autocorrelated
Box.test(macro_diff[, "3mTbill"],
   lag=10, type="Ljung")
# Changes in unemployment rate are autocorrelated
Box.test(macro_diff[, "unemprate"],
   lag=10, type="Ljung")

library(zoo)  # Load package zoo
dax_acf <- acf(re_turns, plot=FALSE)
summary(dax_acf)  # Get the structure of the "acf" object
# Print(dax_acf)  # Print acf data
dim(dax_acf$acf)
dim(dax_acf$lag)
head(dax_acf$acf)

acf_plus <- function(ts_data, plo_t=TRUE,
                xlab="Lag", ylab="",
                main="", ...) {
  acf_data <- acf(x=ts_data, plot=FALSE, ...)
# Remove first element of acf data
  acf_data$acf <-  array(data=acf_data$acf[-1],
    dim=c((dim(acf_data$acf)[1]-1), 1, 1))
  acf_data$lag <-  array(data=acf_data$lag[-1],
    dim=c((dim(acf_data$lag)[1]-1), 1, 1))
  if (plo_t) {
    ci <- qnorm((1+0.95)/2)*sqrt(1/NROW(ts_data))
    ylim <- c(min(-ci, range(acf_data$acf[-1])),
        max(ci, range(acf_data$acf[-1])))
    plot(acf_data, xlab=xlab, ylab=ylab,
   ylim=ylim, main="", ci=0)
    title(main=main, line=0.5)
    abline(h=c(-ci, ci), col="blue", lty=2)
  }
  invisible(acf_data)  # Return invisibly
}  # end acf_plus

par(mar=c(5,0,1,2), oma=c(1,2,1,0), mgp=c(2,1,0), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
library(zoo)  # Load package zoo
# improved autocorrelation function
acf_plus(re_turns, lag=10, main="")
title(main="acf of DAX returns", line=-1)
# Ljung-Box test for DAX returns
Box.test(re_turns, lag=10, type="Ljung")

par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # Set plot panels
# Autocorrelation of squared DAX returns
acf_plus(re_turns^2, lag=10, main="")
title(main="acf of squared DAX returns",
line=-1)
# Autocorrelation of squared random returns
acf_plus(rnorm(NROW(re_turns))^2,
   lag=10, main="")
title(main="acf of squared random returns",
line=-1)
# Ljung-Box test for squared DAX returns
Box.test(re_turns^2, lag=10, type="Ljung")

library(zoo)  # Load package zoo
library(Ecdat)  # Load Ecdat
colnames(Macrodat)  # United States Macroeconomic Time Series
macro_zoo <- as.zoo(  # Coerce to "zoo"
    Macrodat[, c("lhur", "fygm3")])
colnames(macro_zoo) <- c("unemprate", "3mTbill")
# ggplot2 in multiple panes
autoplot(  # Generic ggplot2 for "zoo"
  object=macro_zoo, main="US Macro",
  facets=Series ~ .) + # end autoplot
  xlab("") +
theme(  # modify plot theme
  legend.position=c(0.1, 0.5),
  plot.title=element_text(vjust=-2.0),
  plot.margin=unit(c(-0.5, 0.0, -0.5, 0.0), "cm"),
  plot.background=element_blank(),
  axis.text.y=element_blank()
)  # end theme

par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # Set plot panels
macro_diff <- na.omit(diff(macro_zoo))
acf_plus(coredata(macro_diff[, "unemprate"]),
  lag=10, main="quarterly unemployment rate")
acf_plus(coredata(macro_diff[, "3mTbill"]),
  lag=10, main="3 month T-bill EOQ")

# Extract DAX time series
se_ries <- EuStockMarkets[, 1]
# Filter only over past values (sides=1)
co_eff <- c(0.1, 0.39, 0.5)
filter_ed <- filter(se_ries, filter=co_eff,
             method="convolution", sides=1)
# filter() returns a time series of class "ts"
class(filter_ed)
# Filter using compiled C++ function directly
getAnywhere(C_cfilter)
str(stats:::C_cfilter)
filter_fast <- .Call(stats:::C_cfilter, se_ries, filter=co_eff, sides=1, circular=FALSE)
all.equal(as.numeric(filter_ed), filter_fast, check.attributes=FALSE)
# Benchmark speed of the two methods
library(microbenchmark)
summary(microbenchmark(
  filter=filter(se_ries, filter=co_eff, method="convolution", sides=1),
  filter_fast=.Call(stats:::C_cfilter, se_ries, filter=co_eff, sides=1, circular=FALSE)
  ), times=10)[, c(1, 4, 5)]
# Simulate ARIMA using filter()
in_nov <- rnorm(NROW(EuStockMarkets))
filter_ed <- filter(x=in_nov,
  filter=co_eff, method="recursive")

library(zoo)  # Load zoo
library(ggplot2)  # Load ggplot2
library(gridExtra)  # Load gridExtra
# Coerce to zoo and merge the time series
filter_ed <- cbind(as.zoo(se_ries),
            as.zoo(filter_ed))
colnames(filter_ed) <- c("DAX", "DAX filtered")
filter_ed <- window(filter_ed,
             start=1997, end=1998)
autoplot(  # Plot ggplot2
    filter_ed, main="Filtered DAX",
    facets=NULL) +  # end autoplot
xlab("") + ylab("") +
theme(  # modify plot theme
    legend.position=c(0.1, 0.5),
    plot.title=element_text(vjust=-2.0),
    plot.margin=unit(c(-0.5, 0.0, -0.5, 0.0), "cm"),
    plot.background=element_blank(),
    axis.text.y=element_blank()
    )  # end theme
# end ggplot2

par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
re_turns <- na.omit(diff(log(filter_ed)))
par(mfrow=c(2,1))  # Set plot panels

acf_plus(coredata(re_turns[, 1]), lag=10,
   xlab="")
title(main="DAX", line=-1)

acf_plus(coredata(re_turns[, 2]), lag=10,
   xlab="")
title(main="DAX filtered", line=-1)

# Simulate AR processes
set.seed(1121)  # Reset random numbers
in_dex <- Sys.Date() + 0:728  # two year daily series
ari_ma <- xts(  # AR time series of returns
  x=arima.sim(n=NROW(in_dex), model=list(ar=0.2)),
  order.by=in_dex)
ari_ma <- cbind(ari_ma, cumsum(ari_ma))
colnames(ari_ma) <- c("AR returns", "AR prices")

library(ggplot2)  # Load ggplot2
library(gridExtra)  # Load gridExtra
autoplot(object=ari_ma, # ggplot AR process
 facets="Series ~ .",
 main="Autoregressive process (phi=0.2)") +
  facet_grid("Series ~ .", scales="free_y") +
  xlab("") + ylab("") +
theme(legend.position=c(0.1, 0.5),
  plot.background=element_blank(),
  axis.text.y=element_blank())

ar_coeff <- c(-0.9, 0.01, 0.9)  # AR coefficients
# Create three AR time series
ari_ma <- sapply(ar_coeff, function(phi) {
  set.seed(1121)  # Reset random numbers
  arima.sim(n=NROW(in_dex), model=list(ar=phi))
})  # end sapply
colnames(ari_ma) <- paste("autocorr", ar_coeff)
plot.zoo(ari_ma, main="AR(1) prices", xlab=NA)
# Or plot using ggplot
ari_ma <- xts(x=ari_ma, order.by=in_dex)
library(ggplot)
autoplot(ari_ma, main="AR(1) prices",
   facets=Series ~ .) +
    facet_grid(Series ~ ., scales="free_y") +
xlab("") +
theme(
  legend.position=c(0.1, 0.5),
  plot.title=element_text(vjust=-2.0),
  plot.margin=unit(c(-0.5, 0.0, -0.5, 0.0), "cm"),
  plot.background=element_blank(),
  axis.text.y=element_blank())

library(zoo)  # Load zoo
library(ggplot2)  # Load ggplot2
set.seed(1121)  # initialize random number generator
rand_walk <- cumsum(zoo(matrix(rnorm(3*100), ncol=3),
            order.by=(Sys.Date()+0:99)))
colnames(rand_walk) <-
  paste("rand_walk", 1:3, sep="_")
plot.zoo(rand_walk, main="Random walks",
     xlab="", ylab="", plot.type="single",
     col=c("black", "red", "blue"))
# Add legend
legend(x="topleft",
 legend=colnames(rand_walk),
 col=c("black", "red", "blue"), lty=1)

# Define AR(3) coefficients and innovations
co_eff <- c(0.1, 0.39, 0.5)
len_gth <- 1e2
set.seed(1121); in_nov <- rnorm(len_gth)
# Simulate AR process using recursive loop in R
ari_ma <- numeric(NROW(in_nov))
ari_ma[1] <- in_nov[1]
ari_ma[2] <- co_eff[1]*ari_ma[1] + in_nov[2]
ari_ma[3] <- co_eff[1]*ari_ma[2] +
  co_eff[2]*ari_ma[1] + in_nov[3]
for (it in 4:NROW(ari_ma)) {
  ari_ma[it] <-
    ari_ma[(it-1):(it-3)] %*% co_eff + in_nov[it]
}  # End for
# Simulate AR process using filter()
filter_ed <- filter(x=in_nov,
  filter=co_eff, method="recursive")
class(filter_ed)
all.equal(ari_ma, as.numeric(filter_ed))

# Calculate modulus of roots of characteristic equation
root_s <- Mod(polyroot(c(1, -co_eff)))
# Calculate warmup period
warm_up <- NROW(co_eff) + ceiling(6/log(min(root_s)))
set.seed(1121)
len_gth <- 1e4
in_nov <- rnorm(len_gth + warm_up)
# Simulate ARIMA using arima.sim()
ari_ma <- arima.sim(n=len_gth,
  model=list(ar=co_eff),
  start.innov=in_nov[1:warm_up],
  innov=in_nov[(warm_up+1):NROW(in_nov)])
# Simulate AR process using filter()
filter_ed <- filter(x=in_nov,
  filter=co_eff, method="recursive")
all.equal(filter_ed[-(1:warm_up)],
  as.numeric(ari_ma))
# Benchmark the speed of the three methods of simulating ARIMA
library(microbenchmark)
summary(microbenchmark(
  filter_ed=filter(x=in_nov, filter=co_eff, method="recursive"),
  arima_sim=arima.sim(n=len_gth,
                  model=list(ar=co_eff),
                  start.innov=in_nov[1:warm_up],
                  innov=in_nov[(warm_up+1):NROW(in_nov)]),
  arima_loop={for (it in 4:NROW(ari_ma)) {
  ari_ma[it] <- ari_ma[(it-1):(it-3)] %*% co_eff + in_nov[it]}}
  ), times=10)[, c(1, 4, 5)]

# Simulate random walks using apply() loops
set.seed(1121)  # initialize random number generator
rand_walks <- matrix(rnorm(1000*100), ncol=1000)
rand_walks <- apply(rand_walks, 2, cumsum)
vari_ance <- apply(rand_walks, 1, var)
# Simulate random walks using vectorized functions
set.seed(1121)  # initialize random number generator
rand_walks <- matrixStats::colCumsums(matrix(rnorm(1000*100), ncol=1000))
vari_ance <- matrixStats::rowVars(rand_walks)
par(mar=c(5, 3, 2, 2), oma=c(0, 0, 0, 0))
plot(vari_ance, xlab="time steps", ylab="",
     t="l", col="blue", lwd=2,
     main="Variance of Random Walk")

len_gth <- 1e4
# Simulate arima with small AR coefficient
set.seed(1121)
ari_ma <- arima.sim(n=len_gth, model=list(ar=0.01))
tseries::adf.test(ari_ma)
# Simulate arima with large AR coefficient
set.seed(1121)
ari_ma <- arima.sim(n=len_gth, model=list(ar=0.99))
tseries::adf.test(ari_ma)
# Simulate arima with different AR coefficients
coeff_s <- seq(0.99, 1.0, 0.001) - 0.001
set.seed(1121)
in_nov <- rnorm(len_gth)
adf_test <- sapply(coeff_s, function(co_eff) {
  ari_ma <- filter(x=in_nov, filter=co_eff, method="recursive")
  ad_f <- suppressWarnings(tseries::adf.test(ari_ma))
  c(adf_stat=unname(ad_f$statistic), pval=ad_f$p.value)
})  # end sapply
plot(x=coeff_s, y=adf_test["pval", ], main="ADF Pval versus AR coefficient",
     xlab="AR coefficient", ylab="ADF pval", t="l", col="blue", lwd=2)
plot(x=coeff_s, y=adf_test["adf_stat", ], main="ADF Stat versus AR coefficient",
     xlab="AR coefficient", ylab="ADF stat", t="l", col="blue", lwd=2)

# Simulate arima with large AR coefficient
set.seed(1121)
ari_ma <- arima.sim(n=len_gth, model=list(ar=0.99))
tseries::adf.test(ari_ma)
# integrated series has unit root
tseries::adf.test(cumsum(ari_ma))
# Simulate arima with negative AR coefficient
set.seed(1121)
ari_ma <- arima.sim(n=len_gth, model=list(ar=-0.99))
tseries::adf.test(ari_ma)
# integrated series has unit root
tseries::adf.test(cumsum(ari_ma))

x11(width=5, height=3.5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0))
# Simulate AR(1) process
ari_ma <- arima.sim(n=729, model=list(ar=0.8))
# ACF of AR(1) process
ac_f <- acf_plus(ari_ma, lag=10,
  xlab="", ylab="",
  main="Autocorrelations of AR(1) process")
ac_f$acf[1:5]

# PACF of AR(1) process
pac_f <- pacf(ari_ma, lag=10,
  xlab="", ylab="", main="")
title("Partial autocorrelations of AR(1) process",
  line=1)
pac_f <- drop(pac_f$acf)
pac_f[1:5]

# Compute pacf recursively from acf
ac_f <- acf_plus(ari_ma, lag=10, plo_t=FALSE)
ac_f <- drop(ac_f$acf)
pac_f <- numeric(3)
pac_f[1] <- ac_f[1]
pac_f[2] <- ac_f[2] - ac_f[1]^2
pac_f[3] <- ac_f[3] -
  pac_f[2]*ac_f[1] - ac_f[2]*pac_f[1]
# Compute pacf recursively in a loop
pac_f <- numeric(NROW(ac_f))
pac_f[1] <- ac_f[1]
for (it in 2:NROW(pac_f)) {
  pac_f[it] <- ac_f[it] -
    pac_f[1:(it-1)] %*% ac_f[(it-1):1]
}  # end for

par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # Set plot panels
# Simulate AR process of returns
ari_ma <- arima.sim(n=729,
  model=list(ar=c(0.1, 0.5, 0.1)))
# ACF of AR(3) process
acf_plus(ari_ma, lag=10, xlab="", ylab="",
   main="ACF of AR(3) process")
# PACF of AR(3) process
pacf(ari_ma, lag=10, xlab="", ylab="",
     main="PACF of AR(3) process")

# Compute autocorrelation coefficients
ac_f <- acf_plus(ari_ma, lag=10, plo_t=FALSE)
ac_f <- drop(ac_f$acf)
# Define Yule-Walker matrix
acf_1 <- c(1, ac_f[-10])
yule_walker <- sapply(1:9, function(lagg) {
  col_umn <- rutils::lag_it(acf_1, lagg=lagg)
  col_umn[1:lagg] <- acf_1[(lagg+1):2]
  col_umn
})  # end sapply
yule_walker <- cbind(acf_1, yule_walker)
# Generalized inverse of Yule-Walker matrix
yule_walker_inv <- MASS::ginv(yule_walker)
# Solve Yule-Walker equations
co_eff <- drop(yule_walker_inv %*% ac_f)

# Simulate AR process using filter()
len_gth <- 1e2
co_eff <- c(0.1, 0.39, 0.5)
set.seed(1121); in_nov <- rnorm(len_gth)
ari_ma <- filter(x=in_nov,
  filter=co_eff, method="recursive")
ari_ma <- as.numeric(ari_ma)

# Forecast AR(3) process using loop in R
forecast_s <- numeric(NROW(ari_ma))
forecast_s[1] <- 0
forecast_s[2] <- co_eff[1]*ari_ma[1]
forecast_s[3] <- co_eff[1]*ari_ma[2] +
  co_eff[2]*ari_ma[1]
for (it in 4:NROW(forecast_s)) {
  forecast_s[it] <- ari_ma[(it-1):(it-3)] %*% co_eff
}  # end for

# Plot with legend
plot(ari_ma,
  main="Forecasting Using AR(3) Model",
  xlab="", ylab="", type="l")
lines(forecast_s, col="orange", lwd=3)
legend(x="topright", legend=c("series","forecasts"),
 col=c("black", "orange"), lty=1, lwd=6,
 cex=0.9, bg="white", bty="n")

# Forecast using filter()
forecasts_filter <- filter(x=ari_ma, sides=1,
  filter=co_eff, method="convolution")
forecasts_filter <- as.numeric(forecasts_filter)
# Compare with loop in R
all.equal(forecast_s[-(1:3)],
  forecasts_filter[-c(1:2, NROW(forecasts_filter))],
  check.attributes=FALSE)

# Calibrate ARIMA model using arima()
arima_fit <- arima(ari_ma, order=c(3,0,0),
             include.mean=FALSE)
# One-step-ahead forecast using predict.Arima()
pre_dict <- predict(arima_fit, n.ahead=1)
# Or directly call predict.Arima()
# pre_dict <- predict.Arima(arima_fit, n.ahead=1)
# Inspect the prediction object
class(pre_dict)
names(pre_dict)
class(pre_dict$pred)
unlist(pre_dict)
# One-step-ahead forecast using matrix algebra
fore_cast <- drop(
  ari_ma[len_gth:(len_gth-2)] %*% arima_fit$coef)
# Compare one-step-ahead forecasts
all.equal(pre_dict$pred[[1]], fore_cast)
# Get information about predict.Arima()
?stats:::predict.Arima

# Compare residuals with innovations
residual_s <- (ari_ma - forecast_s)
all.equal(in_nov, residual_s,
    check.attributes=FALSE)
plot(residual_s, t="l", lwd=3, main="ARIMA Forecast Errors")
# Calibrate ARIMA model using arima()
arima_fit <- arima(ari_ma, order=c(3,0,0),
             include.mean=FALSE)
coef_fit <- arima_fit$coef
# Forecast using from fitted coefficients
forecast_s <- numeric(NROW(ari_ma))
forecast_s[1] <- 0
forecast_s[2] <- coef_fit[1]*ari_ma[1]
forecast_s[3] <- coef_fit[1]*ari_ma[2] +
  coef_fit[2]*ari_ma[1]
for (it in 4:NROW(forecast_s)) {
  forecast_s[it] <-
    ari_ma[(it-1):(it-3)] %*% coef_fit
}  # end for
# Calculate the forecasting residuals
residual_s <- (ari_ma - forecast_s)
all.equal(in_nov, residual_s,
    check.attributes=FALSE)
tail(cbind(in_nov, residual_s))

# Simulate AR process using filter()
co_eff <- c(0.1, 0.39, 0.5); n_coeff <- NROW(co_eff)
len_gth <- 1e3; set.seed(1121)
ari_ma <- as.numeric(filter(x=rnorm(len_gth),
  filter=co_eff, method="recursive"))
# Define design matrix
de_sign <- sapply(1:n_coeff, function(lagg) {
  rutils::lag_it(ari_ma, lagg=lagg)
})  # end sapply
de_sign <- cbind(rep(1, len_gth), de_sign)
de_sign <- cbind(ari_ma, de_sign)
# Perform rolling forecasting
look_back <- 100 # Length of look-back interval
forecast_s <- sapply(n_coeff:len_gth, function(now) {
  # Subset the design matrix
  star_t <- max(1, now-look_back+1)
  de_sign <- de_sign[star_t:now, ]
  # Calculate AR(3) coefficients
  design_inv <- MASS::ginv(de_sign[, -1])
  co_eff <- drop(design_inv %*% de_sign[, 1])
  # Calculate forecast
  de_sign[NROW(de_sign):(NROW(de_sign)-n_coeff+1), 1] %*% co_eff
})  # end sapply
forecast_s <- c(rep(forecast_s[1], n_coeff-1), forecast_s)
# Lag the forecasts to push them out-of-sample
forecast_s <- rutils::lag_it(forecast_s)

# Mean squared error
mean((ari_ma - forecast_s)^2)
# Correlation
sum(forecast_s*ari_ma)
# Plot forecasting series with legend
plot(ari_ma, xlab="", ylab="", type="l",
  main="Rolling Forecasting Using AR(3) Model")
lines(forecast_s, col="orange", lwd=1)
legend(x="topright", legend=c("series","forecasts"),
 col=c("black", "orange"), lty=1, lwd=6,
 cex=0.9, bg="white", bty="n")

# Define backtesting function
back_test <- function(ari_ma, n_coeff=2, look_back=11) {
  # Define design matrix
  de_sign <- sapply(1:n_coeff, function(lagg) {
    rutils::lag_it(ari_ma, lagg=lagg)
  })  # end sapply
  de_sign <- cbind(rep(1, len_gth), de_sign)
  de_sign <- cbind(ari_ma, de_sign)
  # Perform rolling forecasting
  forecast_s <- sapply(n_coeff:NROW(ari_ma),
    function(now) {
# Subset the design matrix
star_t <- max(1, now-look_back+1)
de_sign <- de_sign[star_t:now, ]
n_rows <- NROW(de_sign)
# Calculate AR coefficients
design_inv <- MASS::ginv(de_sign[, -1])
co_eff <- drop(design_inv %*% de_sign[, 1])
# Calculate forecast
de_sign[n_rows:(n_rows-n_coeff+1), 1] %*% co_eff
    })  # end sapply
  forecast_s <- c(rep(forecast_s[1], n_coeff-1), forecast_s)
  # Lag the forecasts to push them out-of-sample
  forecast_s <- rutils::lag_it(forecast_s)
  sum(forecast_s*ari_ma)
}  # end back_test

# Define Ornstein-Uhlenbeck parameters
eq_price <- 1.0; sigma_r <- 0.02
the_ta <- 0.01; len_gth <- 1000
drif_t <- the_ta*eq_price
theta_1 <- 1-the_ta
# Simulate Ornstein-Uhlenbeck process
in_nov <- sigma_r*rnorm(len_gth)
price_s <- numeric(len_gth)
price_s[1] <- in_nov[1]
for (i in 2:len_gth) {
  price_s[i] <- theta_1*price_s[i-1] +
    in_nov[i] + drif_t
}  # end for

plot(price_s, type="l",
     xlab="periods", ylab="prices",
     main="Ornstein-Uhlenbeck process")
legend("topright",
 title=paste(c(paste0("sigma_r = ", sigma_r),
               paste0("eq_price = ", eq_price),
               paste0("the_ta = ", the_ta)),
             collapse="\n"),
 legend="", cex=0.8,
 inset=0.1, bg="white", bty="n")
abline(h=eq_price, col='red', lwd=2)

re_turns <- rutils::diff_it(price_s)
lag_price <- rutils::lag_it(price_s)
for_mula <- re_turns ~ lag_price
l_m <- lm(for_mula)
summary(l_m)
# Plot regression
plot(for_mula, main="OU Returns Versus Lagged Prices")
abline(l_m, lwd=2, col="red")

# volatility parameter
c(sigma_r, sd(re_turns))
# Extract OU parameters from regression
co_eff <- summary(l_m)$coefficients
# theta strength of mean reversion
round(co_eff[2, ], 3)
# Equilibrium price
co_eff[1, 1]/co_eff[2, 1]
# Parameter and t-values
co_eff <- cbind(c(the_ta*eq_price, the_ta),
  co_eff[, 1:2])
rownames(co_eff) <- c("drift", "theta")
round(co_eff, 3)

# Simulate Schwartz process
re_turns <- numeric(len_gth)
price_s <- numeric(len_gth)
price_s[1] <- eq_price
set.seed(1121)  # Reset random numbers
for (i in 2:len_gth) {
  re_turns[i] <- the_ta*(eq_price - price_s[i-1]) +
    sigma_r*rnorm(1)
  price_s[i] <- price_s[i-1] * exp(re_turns[i])
}  # end for

plot(price_s, type="l",
     xlab="periods", ylab="prices",
     main="Log-normal Ornstein-Uhlenbeck process")
legend("topright",
 title=paste(c(paste0("sigma_r = ", sigma_r),
               paste0("eq_price = ", eq_price),
               paste0("the_ta = ", the_ta)),
             collapse="\n"),
 legend="", cex=0.8,
 inset=0.12, bg="white", bty="n")
abline(h=eq_price, col='red', lwd=2)

options(width=200)
# Load package HighFreq
library(HighFreq)
# Or load the high frequency data file directly:
symbol_s <- load("C:/Develop/R/HighFreq/data/hf_data.RData")
head(SPY_TAQ)
head(SPY)
tail(SPY)

# Load package HighFreq
library(HighFreq)
head(SPY)

# Load package HighFreq
library(HighFreq)
# Define sym_bol
sym_bol <- "SPY"
# Load OHLC data
output_dir <- "C:/Develop/data/hfreq/scrub/"
sym_bol <- load(
  file.path(output_dir,
      paste0(sym_bol, ".RData")))
inter_val <-
  "2013-11-11 09:30:00/2013-11-11 10:30:00"
chart_Series(SPY[inter_val], name=sym_bol)

# Install package HighFreq from github
devtools::install_github(repo="algoquant/HighFreq")
# Load package HighFreq
library(HighFreq)
# Get documentation for package HighFreq
# Get short description
packageDescription(HighFreq)
# Load help page
help(package=HighFreq)
# List all datasets in HighFreq
data(package=HighFreq)
# List all objects in HighFreq
ls("package:HighFreq")
# Remove HighFreq from search path
detach("package:HighFreq")

# Load package HighFreq
library(HighFreq)
# You can see SPY when listing objects in HighFreq
ls("package:HighFreq")
# You can see SPY when listing datasets in HighFreq
data(package=HighFreq)
# But the SPY dataset isn't listed in the workspace
ls()
# HighFreq datasets are lazy loaded and available when needed
head(SPY)
# Load all the datasets in package HighFreq
data(hf_data)
# HighFreq datasets are now loaded and in the workspace
head(SPY)

library(HighFreq)  # Load package HighFreq
# SPY percentage returns
oh_lc <- HighFreq::SPY
clo_se <- quantmod::Cl(oh_lc)
re_turns <- rutils::diff_it(clo_se)/rutils::lag_it(clo_se)
colnames(re_turns) <- "SPY"
# Standardize raw returns to make later comparisons
ret_std <- re_turns/sd(re_turns)
# Calculate moments and perform normality test
sapply(c(sd=2, skew=3, kurt=4),
  function(x) sum(ret_std^x)/NROW(ret_std))
tseries::jarque.bera.test(re_turns)
# Fit SPY returns using MASS::fitdistr()
optim_fit <- MASS::fitdistr(ret_std, densfun="t", df=2)
lo_cation <- optim_fit$estimate[1]
scal_e <- optim_fit$estimate[2]

x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(1, 1, 1, 1))
# Plot histogram of SPY returns
histo_gram <- hist(ret_std, col="lightgrey", mgp=c(2, 1, 0),
  xlab="returns (standardized)", ylab="frequency", xlim=c(-3, 3),
  breaks=1e3, freq=FALSE, main="Distribution of High Frequency SPY Returns")
# lines(density(ret_std, bw=0.2), lwd=3, col="blue")
# Plot t-distribution function
curve(expr=dt((x-lo_cation)/scal_e, df=2)/scal_e,
type="l", lwd=3, col="red", add=TRUE)
# Plot the Normal probability distribution
curve(expr=dnorm(x, mean=mean(ret_std),
  sd=sd(ret_std)), add=TRUE, lwd=3, col="blue")
# Add legend
legend("topright", inset=0.05, bty="n",
  leg=c("t-distr", "normal"),
  lwd=6, lty=1, col=c("red", "blue"))

# Hourly SPY percentage returns
price_s <- Cl(xts::to.period(x=oh_lc, period="hours"))
returns_hourly <- rutils::diff_it(price_s)/rutils::lag_it(price_s)
returns_hourly <- returns_hourly/sd(returns_hourly)
# Daily SPY percentage returns
price_s <- Cl(xts::to.period(x=oh_lc, period="days"))
returns_daily <- rutils::diff_it(price_s)/rutils::lag_it(price_s)
returns_daily <- returns_daily/sd(returns_daily)
# Calculate moments
sapply(list(minutely=ret_std, hourly=returns_hourly, daily=returns_daily),
 function(rets) {
   sapply(c(sd=2, skew=3, kurt=4),
          function(x) sum(rets^x)/NROW(rets))
})  # end sapply

x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(1, 1, 1, 1))
# Plot densities of SPY returns
plot(density(ret_std, bw=0.4), xlim=c(-3, 3), 
     lwd=3, mgp=c(2, 1, 0), col="blue",
     xlab="returns (standardized)", ylab="frequency",
     main="Density of High Frequency SPY Returns")
lines(density(returns_hourly, bw=0.4), lwd=3, col="green")
lines(density(returns_daily, bw=0.4), lwd=3, col="red")
# Add legend
legend("topright", inset=0.05, bty="n",
  leg=c("minutely", "hourly", "daily"),
  lwd=6, lty=1, col=c("blue", "green", "red"))

# Calculate rolling volatility of SPY returns
ret_2013 <- re_turns["2013-11-11/2013-11-15"]
# Calculate rolling volatility
look_back <- 10
end_p <- seq_along(ret_2013)
start_p <- c(rep_len(1, look_back-1),
  end_p[1:(NROW(end_p)-look_back+1)])
vol_rolling <- sapply(seq_along(end_p),
  function(it) sd(ret_2013[start_p[it]:end_p[it]]))
vol_rolling <- xts::xts(vol_rolling, index(ret_2013))
# Extract time intervals of SPY returns
interval_s <- c(60, diff(xts::.index(re_turns)))
head(interval_s)
table(interval_s)
# Scale SPY returns by time intervals
re_turns <- 60*re_turns/interval_s
ret_2013 <- re_turns["2013-11-11/2013-11-15"]
# Calculate scaled rolling volatility
vol_scaled <- sapply(seq_along(end_p),
  function(it) sd(ret_2013[start_p[it]:end_p[it]]))
vol_rolling <- cbind(vol_rolling, vol_scaled)
vol_rolling <- na.omit(vol_rolling)

# Plot rolling volatility
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("blue", "red")
chart_Series(vol_rolling, theme=plot_theme,
     name="Rolling Volatility with Overnight Spikes")
legend("topright", legend=colnames(vol_rolling),
  inset=0.1, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")

price_s <- read.zoo(file="C:/Develop/lecture_slides/data/bid_ask_bounce.csv",
  header=TRUE, sep=",")
price_s <- as.xts(price_s)
x11(width=6, height=4)
par(mar=c(2, 2, 0, 0), oma=c(1, 1, 0, 0))
chart_Series(x=price_s, name="S&P500 Futures Bid-Ask Bounce")

# Volatility of SPY
sqrt(HighFreq::calc_var_ohlc(oh_lc))
# Daily SPY volatility and volume
vol_daily <- sqrt(xts::apply.daily(oh_lc, FUN=calc_var_ohlc))
colnames(vol_daily) <- ("SPY_volatility")
vol_ume <- quantmod::Vo(oh_lc)
volume_daily <- xts::apply.daily(vol_ume, FUN=sum)
colnames(volume_daily) <- ("SPY_volume")
# Plot SPY volatility and volume
da_ta <- cbind(vol_daily, volume_daily)["2008/2009"]
col_names <- colnames(da_ta)
dygraphs::dygraph(da_ta,
  main="SPY Daily Volatility and Trading Volume") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="red", strokeWidth=3) %>%
  dySeries(name=col_names[2], axis="y2", col="blue", strokeWidth=3)

# Regress log of daily volume vs volatility
da_ta <- log(cbind(volume_daily, vol_daily))
col_names <- colnames(da_ta)
data_frame <- as.data.frame(da_ta)
for_mula <- as.formula(paste(col_names, collapse="~"))
mod_el <- lm(for_mula, data=data_frame)
# Durbin-Watson test for autocorrelation of residuals
lmtest::dwtest(mod_el)
# Regress diff log of daily volume vs volatility
data_frame <- as.data.frame(rutils::diff_it(da_ta))
mod_el <- lm(for_mula, data=data_frame)
lmtest::dwtest(mod_el)
summary(mod_el)
plot(for_mula, data=data_frame, main="SPY Daily Trading Volume vs Volatility (log scale)")
abline(mod_el, lwd=3, col="red")
mtext(paste("beta =", round(coef(mod_el)[2], 3)), cex=1.2, lwd=3, side=2, las=2, adj=(-0.5), padj=(-7))

# 60 minutes of data in look_back interval
look_back <- 60
vol_2013 <- vol_ume["2013"]
ret_2013 <- re_turns["2013"]
# Define end_points with beginning stub
n_rows <- NROW(ret_2013)
n_agg <- n_rows %/% look_back
end_p <- n_rows-look_back*n_agg + (0:n_agg)*look_back
start_p <- c(1, end_p[1:(NROW(end_p)-1)])
# Calculate SPY volatility and volume
da_ta <- sapply(seq_along(end_p), function(it) {
  point_s <- start_p[it]:end_p[it]
  c(volume=sum(vol_2013[point_s]),
    volatility=sd(ret_2013[point_s]))
})  # end sapply
da_ta <- t(da_ta)
da_ta <- rutils::diff_it(log(da_ta))
data_frame <- as.data.frame(da_ta)

for_mula <- as.formula(paste(colnames(da_ta), collapse="~"))
mod_el <- lm(for_mula, data=data_frame)
lmtest::dwtest(mod_el)
summary(mod_el)
plot(for_mula, data=data_frame,
     main="SPY Hourly Trading Volume vs Volatility (log scale)")
abline(mod_el, lwd=3, col="red")
mtext(paste("beta =", round(coef(mod_el)[2], 3)), cex=1.2, lwd=3, side=2, las=2, adj=(-0.5), padj=(-7))

# Scale returns using volume (volume clock)
ret_scaled <- ifelse(vol_ume>0,
  re_turns/vol_ume^0.5, 0)
# Calculate moments
sapply(list(ret_std=ret_std, ret_scaled=ret_scaled),
  function(rets) {
    sapply(c(skew=3, kurt=4),
     function(x) sum((rets/sd(rets))^x)/NROW(rets))
})  # end sapply

x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(1, 1, 1, 1))
# Plot densities of SPY returns
plot(density(ret_std), xlim=c(-3, 3),
     lwd=3, mgp=c(2, 1, 0), col="blue",
     xlab="returns (standardized)", ylab="frequency",
     main="Density of Volume-scaled High Frequency SPY Returns")
lines(density(ret_scaled/sd(ret_scaled), bw=0.4), lwd=3, col="red")
curve(expr=dnorm, add=TRUE, lwd=3, col="green")
# Add legend
legend("topright", inset=0.05, bty="n",
  leg=c("minutely", "scaled", "normal"),
  lwd=6, lty=1, col=c("blue", "red", "green"))
# Plot the partial autocorrelations
x11(width=6, height=5)
par(mar=c(4, 4, 2, 1), oma=c(1, 1, 1, 1))
pacf(ret_scaled, lag=10, mgp=c(2, 1, 0),
  xlab="lag", ylab="partial autocorrelation", main="")
title("Partial Autocorrelations of Volume-scaled High Frequency SPY Returns", line=1)

# Ljung-Box test for minutely SPY returns
Box.test(re_turns, lag=10, type="Ljung")
# Ljung-Box test for daily SPY returns
Box.test(returns_daily, lag=10, type="Ljung")
# Ljung-Box test statistics for scaled SPY returns
sapply(list(ret_std=ret_std, ret_scaled=ret_scaled),
  function(rets) {
    Box.test(rets, lag=10, type="Ljung")$statistic
})  # end sapply
# Ljung-Box test statistics for aggregated SPY returns
sapply(list(minutely=ret_std, hourly=returns_hourly, daily=returns_daily),
  function(rets) {
    Box.test(rets, lag=10, type="Ljung")$statistic
})  # end sapply

# Set plot parameters
x11(width=6, height=8)
par(mar=c(4, 4, 2, 1), oma=c(0, 0, 0, 0))
layout(matrix(c(1, 2), ncol=1), widths=c(6, 6), heights=c(4, 4))
# Plot the partial autocorrelations of minutely SPY returns
pacf(as.numeric(re_turns), lag=10,
     xlab="lag", ylab="partial autocorrelation", main="")
title("Partial Autocorrelations of Minutely SPY Returns", line=1)
# Plot the partial autocorrelations of scaled SPY returns
pacf(as.numeric(ret_scaled), lag=10,
     xlab="lag", ylab="partial autocorrelation", main="")
title("Partial Autocorrelations of Scaled SPY Returns", line=1)

# Calculate market illiquidity
liquidi_ty <- sqrt(volume_daily)/vol_daily
# Plot market illiquidity
x11(width=6, height=7) ; par(mfrow=c(2, 1))
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("blue")
chart_Series(liquidi_ty["2010"], theme=plot_theme,
  name="SPY Liquidity in 2010", plot=FALSE)
plot_theme$col$line.col <- c("red")
chart_Series(vol_daily["2010"],
  theme=plot_theme, name="SPY Volatility in 2010")

# Calculate intraday time index with hours and minutes
in_dex <- format(zoo::index(re_turns), "%H:%M")
# Aggregate the mean volume
volume_agg <- tapply(X=vol_ume, INDEX=in_dex, FUN=mean)
volume_agg <- drop(volume_agg)
# Aggregate the mean volatility
vol_agg <- tapply(X=re_turns^2, INDEX=in_dex, FUN=mean)
vol_agg <- sqrt(drop(vol_agg))
# Coerce to xts
intra_day <- as.POSIXct(paste(Sys.Date(), names(volume_agg)))
volume_agg <- xts::xts(volume_agg, intra_day)
vol_agg <- xts::xts(vol_agg, intra_day)
# Plot seasonality of volume and volatility
x11(width=6, height=7) ; par(mfrow=c(2, 1))
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("blue")
chart_Series(volume_agg[c(-1, -NROW(volume_agg))], theme=plot_theme,
  name="Daily Seasonality of SPY Volume", plot=FALSE)
plot_theme$col$line.col <- c("red")
chart_Series(vol_agg[c(-1, -NROW(vol_agg))], theme=plot_theme,
  name="Daily Seasonality of SPY Volatility")

# Calculate market liquidity
liquidi_ty <- sqrt(volume_agg)/vol_agg
# Plot daily seasonality of market liquidity
x11(width=6, height=7) ; par(mfrow=c(2, 1))
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("blue")
chart_Series(liquidi_ty[c(-1, -NROW(liquidi_ty))], theme=plot_theme,
  name="Daily Seasonality of SPY Liquidity", plot=FALSE)
plot_theme$col$line.col <- c("red")
chart_Series(vol_agg[c(-1, -NROW(vol_agg))], theme=plot_theme,
  name="Daily Seasonality of SPY Volatility")
