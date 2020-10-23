library(knitr)
opts_chunk$set(prompt=TRUE, eval=FALSE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='tiny', fig.width=4, fig.height=4)
options(digits=3)
options(width=80, dev='pdf')
thm <- knit_theme$get("acid")
knit_theme$set(thm)

library(PortfolioAnalytics)
# Use ETF returns from package rutils
library(rutils)
portf_names <- c("VTI", "IEF", "DBC", "XLF",
  "VNQ", "XLP", "XLV", "XLU", "XLB", "XLE")
# Initial portfolio to equal weights
portf_init <- rep(1/NROW(portf_names),
            NROW(portf_names))
# named vector
names(portf_init) <- portf_names
# Create portfolio object
portf_init <- portfolio.spec(
  assets=portf_init)

library(PortfolioAnalytics)
# Add constraints
portf_maxSR <- add.constraint(
  portfolio=portf_init,  # Initial portfolio
  type="weight_sum",  # Constraint sum weights
  min_sum=0.9, max_sum=1.1)
# Add constraints
portf_maxSR <- add.constraint(
  portfolio=portf_maxSR,
  type="long_only")  # box constraint min=0, max=1
# Add objectives
portf_maxSR <- add.objective(
  portfolio=portf_maxSR,
  type="return",  # Maximize mean return
  name="mean")
# Add objectives
portf_maxSR <- add.objective(
  portfolio=portf_maxSR,
  type="risk",  # Minimize StdDev
  name="StdDev")

library(PortfolioAnalytics)
# Use ETF returns from package rutils
library(rutils)
portf_names <- c("VTI", "IEF", "DBC", "XLF",
  "VNQ", "XLP", "XLV", "XLU", "XLB", "XLE")
# Initial portfolio to equal weights
portf_init <- rep(1/NROW(portf_names),
            NROW(portf_names))
# named vector
names(portf_init) <- portf_names
# Create portfolio object
portf_init <- portfolio.spec(
  assets=portf_init)

library(PortfolioAnalytics)
# Add constraints
portf_maxSR <- add.constraint(
  portfolio=portf_init,  # Initial portfolio
  type="weight_sum",  # Constraint sum weights
  min_sum=0.9, max_sum=1.1)
# Add constraints
portf_maxSR <- add.constraint(
  portfolio=portf_maxSR,
  type="long_only")  # box constraint min=0, max=1
# Add objectives
portf_maxSR <- add.objective(
  portfolio=portf_maxSR,
  type="return",  # Maximize mean return
  name="mean")
# Add objectives
portf_maxSR <- add.objective(
  portfolio=portf_maxSR,
  type="risk",  # Minimize StdDev
  name="StdDev")

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
re_turns <- zoo::na.locf(re_turns, na.rm=FALSE)
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

# Vector of symbol names
sym_bols <- c("VTI", "IEF", "DBC")
n_weights <- NROW(sym_bols)
# Calculate random portfolios
n_portf <- 1000
ret_sd <- sapply(1:n_portf, function(in_dex) {
  weight_s <- runif(n_weights, min=0, max=10)
  weight_s <- weight_s/sum(weight_s)
  portf_rets <- etf_env$re_turns[, sym_bols] %*% weight_s
  100*c(ret=mean(portf_rets), sd=sd(portf_rets))
})  # end sapply
# Plot scatterplot of random portfolios
x11(width=(wid_th <- 6), height=(hei_ght <- 5))
plot(x=ret_sd[2, ], y=ret_sd[1, ], xlim=c(0, max(ret_sd[2, ])),
     main="Random portfolios",
     ylim=c(min(0, min(ret_sd[1, ])), max(ret_sd[1, ])),
     xlab=rownames(ret_sd)[2], ylab=rownames(ret_sd)[1])

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
optim_rets <- cbind(exp(cumsum(re_turns %*%
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
re_turns <- rutils::diff_it(log(quantmod::Cl(rutils::etf_env$VTI)))
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

library(rutils)  # Load rutils
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

# Vector of initial portfolio weights equal to 1
weight_s <- rep(1, n_weights)
names(weight_s) <- sym_bols
# Objective function equal to standard deviation of returns
object_ive <- function(weight_s) {
  portf_rets <- re_turns %*% weight_s
  sd(portf_rets)/sum(weight_s)
}  # end object_ive
# Object_ive() for equal weight portfolio
object_ive(weight_s)
object_ive(2*weight_s)
# Perform portfolio optimization
op_tim <- optim(par=weight_s,
          fn=object_ive,
          method="L-BFGS-B",
          upper=rep(10, n_weights),
          lower=rep(-10, n_weights))
# Rescale the optimal weights
weight_s <- op_tim$par/sum(op_tim$par)
# Minimum variance portfolio returns
optim_rets <- xts(x=re_turns %*% weight_s,
            order.by=index(re_turns))
chart_Series(x=exp(cumsum(optim_rets)), name="minvar portfolio")
# Add green point for minimum variance portfolio
optim_sd <- 100*sd(optim_rets)
optim_ret <- 100*mean(optim_rets)
points(x=optim_sd, y=optim_ret, col="green", lwd=6)
text(x=optim_sd, y=optim_ret, labels="minvar", pos=2, cex=0.8)


# Objective function equal to minus Sharpe ratio
risk_free <- 0.03
object_ive <- function(weight_s) {
  portf_rets <- 100*etf_env$re_turns[, names(weight_s)] %*% weight_s / sum(weight_s)
  -mean(portf_rets-risk_free)/sd(portf_rets)
}  # end object_ive
# Perform portfolio optimization
op_tim <- optim(par=weight_s,
             fn=object_ive,
             method="L-BFGS-B",
             upper=rep(10, n_weights),
             lower=rep(-10, n_weights))
# Maximum Sharpe ratio portfolio returns
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
# Plot individual assets
mean_rets <- 100*sapply(re_turns, mean)
std_devs <- 100*sapply(re_turns, sd)
points(std_devs, mean_rets, col="green", lwd=3)
text(std_devs, mean_rets, labels=names(mean_rets), pos=2, cex=0.8)
# Add point at risk-free rate and draw Capital Market Line
points(x=0, y=risk_free)
text(0, risk_free, labels="risk-free", pos=4, cex=0.8)
abline(a=risk_free, b=max_Sharpe, col="blue")
range_s <- par("usr")
text(optim_sd/3, (optim_ret+risk_free)/2.5,
     labels="Capital Market Line", cex=0.8, , pos=3,
     srt=45*atan(max_Sharpe*(range_s[2]-range_s[1])/
             (range_s[4]-range_s[3])*
             hei_ght/wid_th)/(0.25*pi))

# Optimization to find weights with maximum Sharpe ratio
op_tim <- optim(par=weight_s,
             fn=object_ive,
             re_turns=re_turns,
             method="L-BFGS-B",
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

# Rastrigin function with vector argument for optimization
rastri_gin <- function(vec_tor, pa_ram=25){
  sum(vec_tor^2 - pa_ram*cos(vec_tor))
}  # end rastri_gin
vec_tor <- c(pi/6, pi/6)
rastri_gin(vec_tor=vec_tor)
library(DEoptim)
Optimize rastri_gin using DEoptim
op_tim <-  DEoptim(rastri_gin,
  upper=c(6, 6), lower=c(-6, -6),
  DEoptim.control(trace=FALSE, itermax=50))
# Optimal parameters and value
op_tim$optim$bestmem
rastri_gin(op_tim$optim$bestmem)
summary(op_tim)
plot(op_tim)

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

load(file="C:/Develop/lecture_slides/data/zoo_data.RData")
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
# Use ETF returns from package rutils
library(rutils)
portf_names <- c("VTI", "IEF", "DBC", "XLF",
  "VNQ", "XLP", "XLV", "XLU", "XLB", "XLE")
# Initial portfolio to equal weights
portf_init <- rep(1/NROW(portf_names),
            NROW(portf_names))
# named vector
names(portf_init) <- portf_names
# Create portfolio object
portf_init <- portfolio.spec(
  assets=portf_init)

library(PortfolioAnalytics)
# Add constraints
portf_maxSR <- add.constraint(
  portfolio=portf_init,  # Initial portfolio
  type="weight_sum",  # Constraint sum weights
  min_sum=0.9, max_sum=1.1)
# Add constraints
portf_maxSR <- add.constraint(
  portfolio=portf_maxSR,
  type="long_only")  # box constraint min=0, max=1
# Add objectives
portf_maxSR <- add.objective(
  portfolio=portf_maxSR,
  type="return",  # Maximize mean return
  name="mean")
# Add objectives
portf_maxSR <- add.objective(
  portfolio=portf_maxSR,
  type="risk",  # Minimize StdDev
  name="StdDev")

load(file="C:/Develop/lecture_slides/data/portf_optim.RData")
library(PortfolioAnalytics)
# Perform optimization of weights
maxSR_DEOpt <- optimize.portfolio(
  R=etf_env$re_turns[, portf_names],  # Specify returns
  portfolio=portf_maxSR,  # Specify portfolio
  optimize_method="DEoptim", # Use DEoptim
  maxSR=TRUE,  # Maximize Sharpe
  trace=TRUE, traceDE=0)
# Plot optimization
chart.RiskReward(maxSR_DEOpt,
  risk.col="stddev",
  return.col="mean")

options(width=50)
library(PortfolioAnalytics)
load(file="C:/Develop/lecture_slides/data/portf_optim.RData")
maxSR_DEOpt$weights
maxSR_DEOpt$objective_measures$mean[1]
maxSR_DEOpt$objective_measures$StdDev[[1]]

library(PortfolioAnalytics)
# Plot optimization
chart.RiskReward(maxSR_DEOpt,
  risk.col="StdDev",
  return.col="mean")

# Plot risk/ret points in portfolio scatterplot
risk_ret_points <- function(rets=etf_env$re_turns,
  risk=c("sd", "ETL"), sym_bols=c("VTI", "IEF")) {
  risk <- match.arg(risk)  # Match to arg list
  if (risk=="ETL") {
    stopifnot(
"package:PerformanceAnalytics" %in% search() ||
require("PerformanceAnalytics", quietly=TRUE))
  }  # end if
  risk <- match.fun(risk)  # Match to function
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
      rets_data=etf_env$re_turns) {
  weight_s <- portfolio$weights
  portf_names <- names(weight_s)
  # Calculate xts of portfolio
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
# Add leverage constraint abs(weight_sum)
portf_maxSRN <- add.constraint(
  portfolio=portf_init, type="leverage",
  min_sum=0.9, max_sum=1.1)
# Add box constraint long/short
portf_maxSRN <- add.constraint(
  portfolio=portf_maxSRN,
  type="box", min=-0.2, max=0.2)

# Add objectives
portf_maxSRN <- add.objective(
  portfolio=portf_maxSRN,
  type="return",  # Maximize mean return
  name="mean")
# Add objectives
portf_maxSRN <- add.objective(
  portfolio=portf_maxSRN,
  type="risk",  # Minimize StdDev
  name="StdDev")

load(file="C:/Develop/lecture_slides/data/portf_optim.RData")
library(PortfolioAnalytics)
# Perform optimization of weights
maxSRN_DEOpt <- optimize.portfolio(
  R=etf_env$re_turns[, portf_names],  # Specify returns
  portfolio=portf_maxSRN,  # Specify portfolio
  optimize_method="DEoptim", # Use DEoptim
  maxSR=TRUE,  # Maximize Sharpe
  trace=TRUE, traceDE=0)
# Plot optimization
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
# Plot risk/ret points in portfolio scatterplot
risk_ret_points()

library(PortfolioAnalytics)
load(file="C:/Develop/lecture_slides/data/portf_optim.RData")
maxSRN_DEOpt$weights
maxSRN_DEOpt$objective_measures$mean[1]
maxSRN_DEOpt$objective_measures$StdDev[[1]]

library(PortfolioAnalytics)
maxSRN_DEOpt_xts <-
  plot_portf(portfolio=maxSRN_DEOpt)

library(PerformanceAnalytics)
load(file="C:/Develop/lecture_slides/data/portf_optim.RData")
chart.CumReturns(
  cbind(maxSR_DEOpt_xts, maxSRN_DEOpt_xts),
  lwd=2, ylab="",
  legend.loc="topleft", main="")

options(width=50)
library(PerformanceAnalytics)
load(file="C:/Develop/lecture_slides/data/portf_optim.RData")
rbind(maxSR_DEOpt$weights, maxSRN_DEOpt$weights)
c(maxSR_DEOpt$objective_measures$mean,
maxSRN_DEOpt$objective_measures$mean)
c(maxSR_DEOpt$objective_measures$StdDev[[1]],
maxSRN_DEOpt$objective_measures$StdDev[[1]])

library(PortfolioAnalytics)
# Add constraints
portf_maxSTARR <- add.constraint(
  portfolio=portf_init,  # Initial portfolio
  type="weight_sum",  # Constraint sum weights
  min_sum=0.9, max_sum=1.1)
# Add constraints
portf_maxSTARR <- add.constraint(
  portfolio=portf_maxSTARR,
  type="long_only")  # box constraint min=0, max=1
# Add objectives
portf_maxSTARR <- add.objective(
  portfolio=portf_maxSTARR,
  type="return",  # Maximize mean return
  name="mean")
# Add objectives
portf_maxSTARR <- add.objective(
  portfolio=portf_maxSTARR,
  type="risk",  # Minimize Expected Shortfall
  name="ES")

load(file="C:/Develop/lecture_slides/data/portf_optim.RData")
library(PortfolioAnalytics)
# Perform optimization of weights
maxSTARR_DEOpt <- optimize.portfolio(
  R=etf_env$re_turns[, portf_names],  # Specify returns
  portfolio=portf_maxSTARR,  # Specify portfolio
  optimize_method="DEoptim", # Use DEoptim
  maxSTARR=TRUE,  # Maximize STARR
  trace=TRUE, traceDE=0)

# Plot optimization
chart.RiskReward(maxSTARR_DEOpt,
  risk.col="ES",
  return.col="mean")
# Plot risk/ret points in portfolio scatterplot
risk_ret_points(risk="ETL")

options(width=50)
library(PortfolioAnalytics)
load(file="C:/Develop/lecture_slides/data/portf_optim.RData")
maxSTARR_DEOpt$weights
maxSTARR_DEOpt$objective_measures$mean[1]
maxSTARR_DEOpt$objective_measures$ES[[1]]

library(PortfolioAnalytics)
maxSTARR_DEOpt_xts <-
  plot_portf(portfolio=maxSTARR_DEOpt)

library(PerformanceAnalytics)
load(file="C:/Develop/lecture_slides/data/portf_optim.RData")
chart.CumReturns(
  cbind(maxSR_DEOpt_xts, maxSTARR_DEOpt_xts),
  lwd=2, ylab="",
  legend.loc="topleft", main="")

options(width=50)
library(PerformanceAnalytics)
load(file="C:/Develop/lecture_slides/data/portf_optim.RData")
rbind(maxSR_DEOpt$weights, maxSTARR_DEOpt$weights)
c(maxSR_DEOpt$objective_measures$mean,
maxSTARR_DEOpt$objective_measures$mean)
c(maxSR_DEOpt$objective_measures$StdDev[[1]],
maxSTARR_DEOpt$objective_measures$ES[[1]])

library(PortfolioAnalytics)
# Plot the efficient frontier
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
# Add constraints
portf_minES <- add.constraint(
  portfolio=portf_init,  # Initial portfolio
  type="weight_sum",  # Constraint sum weights
  min_sum=0.9, max_sum=1.1)
# Add constraints
portf_minES <- add.constraint(
  portfolio=portf_minES,
  type="long_only")  # box constraint min=0, max=1
# Add objectives
portf_minES <- add.objective(
  portfolio=portf_minES,
  type="risk",  # Minimize ES
  name="ES")

load(file="C:/Develop/lecture_slides/data/portf_optim.RData")
library(PortfolioAnalytics)
# Perform optimization of weights
minES_ROI <- optimize.portfolio(
  R=etf_env$re_turns[, portf_names],  # Specify returns
  portfolio=portf_minES,  # Specify portfolio
  optimize_method="ROI", # Use ROI
  trace=TRUE, traceDE=0)

# Plot optimization
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
# Plot risk/ret points in portfolio scatterplot
risk_ret_points(risk="ETL")

options(width=50)
library(PortfolioAnalytics)
load(file="C:/Develop/lecture_slides/data/portf_optim.RData")
minES_ROI$weights
minES_ROI$objective_measures$ES[[1]]

library(PortfolioAnalytics)
minES_ROI_xts <-
  plot_portf(portfolio=minES_ROI)

library(PerformanceAnalytics)
load(file="C:/Develop/lecture_slides/data/portf_optim.RData")
chart.CumReturns(
  cbind(maxSR_DEOpt_xts, minES_ROI_xts),
  lwd=2, ylab="",
  legend.loc="topleft", main="")

options(width=50)
library(PerformanceAnalytics)
load(file="C:/Develop/lecture_slides/data/portf_optim.RData")
rbind(maxSR_DEOpt$weights, minES_ROI$weights)
c(maxSR_DEOpt$objective_measures$mean,
minES_ROI$objective_measures$mean)
c(maxSR_DEOpt$objective_measures$StdDev[[1]],
minES_ROI$objective_measures$ES[[1]])

load(file="C:/Develop/lecture_slides/data/portf_optim.RData")
library(PortfolioAnalytics)
options(width=50)
# Perform optimization of weights
maxSR_DEOpt <- optimize.portfolio(
  R=etf_env$re_turns["/2011", portf_names],
  portfolio=portf_maxSR,  # Specify portfolio
  optimize_method="DEoptim", # Use DEoptim
  maxSR=TRUE,  # Maximize Sharpe
  trace=TRUE, traceDE=0)
weights_1h <- maxSR_DEOpt$weights

# Plot optimization
maxSR_DEOpt_xts <-
  plot_portf(portfolio=maxSR_DEOpt)

load(file="C:/Develop/lecture_slides/data/portf_optim.RData")
library(PortfolioAnalytics)
options(width=50)
# Perform optimization of weights
maxSR_DEOpt <- optimize.portfolio(
  R=etf_env$re_turns["2011/", portf_names],
  portfolio=portf_maxSR,  # Specify portfolio
  optimize_method="DEoptim", # Use DEoptim
  maxSR=TRUE,  # Maximize Sharpe
  trace=TRUE, traceDE=0)
weights_2h <- maxSR_DEOpt$weights

# Plot optimization
maxSR_DEOpt_xts <-
  plot_portf(portfolio=maxSR_DEOpt)

options(width=50)
weights_1h
weights_2h
weights_1h - weights_2h

par(oma=c(1,0,1,0), mgp=c(2,1,0), mar=c(2,1,2,1), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # Set plot panels
barplot(weights_1h,
  names.arg=names(weights_1h),
  las=3, ylab="", xlab="",
  main="Portfolio Weights First Half")
barplot(weights_2h,
  names.arg=names(weights_2h),
  las=3, ylab="", xlab="",
  main="Portfolio Weights Second Half")

# Calculate random default probabilities
set.seed(1121)
n_assets <- 100
def_probs <- runif(n_assets, max=0.2)
mean(def_probs)
# Simulate number of defaults
uni_form <- runif(n_assets)
sum(uni_form < def_probs)
# Simulate average number of defaults using for() loop (inefficient way)
n_simu <- 1000
set.seed(1121)
de_faults <- numeric(n_simu)
for (i in 1:n_simu) {  # Perform loop
  uni_form <- runif(n_assets)
  de_faults[i] <- sum(uni_form < def_probs)
}  # end for
# Calculate average number of defaults
mean(de_faults)
# Simulate using vectorized functions  (efficient way)
set.seed(1121)
uni_form <- matrix(runif(n_simu*n_assets), ncol=n_simu)
de_faults <- colSums(uni_form < def_probs)
mean(de_faults)
# Plot the distribution of defaults
x11(width=6, height=5)
plot(density(de_faults), main="Distribution of Defaults",
     xlab="number of defaults", ylab="frequqncy")
abline(v=mean(de_faults), lwd=3, col="red")

# Calculate default thresholds and asset values
def_thresh <- qnorm(def_probs)
asset_s <- qnorm(uni_form)
# Simulate defaults
de_faults <- colSums(asset_s < def_thresh)
mean(de_faults)

# Plot Standard Normal distribution
x11(width=6, height=5)
x_lim <- 4; def_thresh <- qnorm(0.025)
curve(expr=dnorm(x), type="l", xlim=c(-x_lim, x_lim),
xlab="asset value", ylab="", lwd=3,
col="blue", main="Distribution of Asset Values")
abline(v=def_thresh, col="red", lwd=3)
text(x=def_thresh-0.1, y=0.15, labels="default threshold",
 lwd=2, srt=90, pos=3)
# Plot polygon area
x_var <- seq(-x_lim, x_lim, length=100)
y_var <- dnorm(x_var)
are_a <- ((x_var >= (-x_lim)) & (x_var <= def_thresh))
polygon(c(x_lim, x_var[are_a], def_thresh),
  c(-1, y_var[are_a], -1), col="red")

# Define correlation parameters
rh_o <- 0.2
rho_sqrt <- sqrt(rh_o) ; rho_sqrtm <- sqrt(1-rh_o)
n_assets <- 5 ; n_simu <- 10000
# Calculate vector of systematic and idiosyncratic factors
system_atic <- rnorm(n_simu)
idio_syncratic <- rnorm(n_simu*n_assets)
# Simulate asset values using vectorized functions (efficient way)
asset_s <- rho_sqrt*system_atic + rho_sqrtm*idio_syncratic
dim(asset_s) <- c(n_simu, n_assets)
# Asset values are standard normally distributed
apply(asset_s, MARGIN=2, function(x) c(mean=mean(x), sd=sd(x)))
# Calculate correlations between asset values
cor(asset_s)
# Simulate asset values using for() loop (inefficient way)
# Allocate matrix of assets
asset_s <- matrix(nr=n_simu, nc=n_assets)
# Simulate asset values using for() loop
for (i in 1:n_simu) {  # Perform loop
  asset_s[i, ] <- rho_sqrt*system_atic[i] + rho_sqrtm*rnorm(n_assets)
}  # end for
cor(asset_s)
# benchmark the speed of the two methods
library(microbenchmark)
summary(microbenchmark(
  for_loop={for (i in 1:n_simu) {
    rho_sqrt*system_atic[i] + rho_sqrtm*rnorm(n_assets)}},
  vector_ized={rho_sqrt*system_atic + rho_sqrtm*rnorm(n_simu*n_assets)},
  times=10))[, c(1, 4, 5)]

# Calculate random default probabilities
n_assets <- 5
def_probs <- runif(n_assets, max=0.2)
mean(def_probs)
# Calculate default thresholds
def_thresh <- qnorm(def_probs)
# Calculate number of defaults using vectorized functions (efficient way)
# Calculate vector of number of defaults
rowMeans(t(asset_s) < def_thresh)
def_probs
# Calculate number of defaults using for() loop (inefficient way)
# allocate matrix of de_faults
de_faults <- matrix(nr=n_simu, nc=n_assets)
# Simulate asset values using for() loop
for (i in 1:n_simu) {  # Perform loop
  de_faults[i, ] <- (asset_s[i, ] < def_thresh)
}  # end for
colSums(de_faults) / n_simu
def_probs
# Calculate correlations between defaults
cor(de_faults)

# Define default probabilities
n_assets <- 2
def_prob <- 0.2
def_thresh <- qnorm(def_prob)
# Define correlation parameters
rh_o <- 0.2
rho_sqrt <- sqrt(rh_o) ; rho_sqrtm <- sqrt(1-rh_o)
# Calculate vector of systematic factors
n_simu <- 1000
system_atic <- rnorm(n_simu)
# Simulate asset values using vectorized functions
asset_s <- rho_sqrt*system_atic + rho_sqrtm*rnorm(n_simu*n_assets)
dim(asset_s) <- c(n_simu, n_assets)
# Calculate number of defaults using vectorized functions
de_faults <- t(t(asset_s) < def_thresh)
# Calculate correlations between defaults
cor(de_faults)
# Calculate average number of defaults and compare to def_prob
colSums(de_faults) / n_simu
def_prob

# Define cumulative default probability function
def_cumdistr <- function(x, def_thresh=(-2), rh_o=0.2)
  pnorm((sqrt(1-rh_o)*qnorm(x) - def_thresh)/sqrt(rh_o))
def_cumdistr(x=0.2, def_thresh=qnorm(def_prob), rh_o=rh_o)
# Plot cumulative default probability function
def_prob <- 0.4; def_thresh <- qnorm(def_prob)
curve(expr=def_cumdistr(x, def_thresh=def_thresh, rh_o=0.05),
xlim=c(0, 0.999), lwd=3,
xlab="percent default", ylab="probability",
col="green", main="Cumulative Default Probabilities")

# Plot default distribution with higher correlation
curve(expr=def_cumdistr(x, def_thresh=def_thresh, rh_o=0.2),
xlim=c(0, 0.999), add=TRUE, lwd=3,
col="blue", main="")
# Add legend
legend(x="topleft",
 legend=c("high correlation", "low correlation"),
 title=NULL, inset=0.05, cex=0.8, bg="white",
 bty="n", lwd=6, lty=1, col=c("blue", "green"))
# Add unconditional default probability
abline(v=def_prob, col="red", lwd=3)
text(x=def_prob, y=0.0,
 labels="default probability",
 lwd=2, srt=90, pos=4)

# Define default probability density function
def_distr <- function(x, def_thresh=(-2), rh_o=0.2)
  sqrt((1-rh_o)/rh_o)*exp(-(sqrt(1-rh_o)*qnorm(x) -
  def_thresh)^2/(2*rh_o) + qnorm(x)^2/2)
# Define parameters
rh_o <- 0.2 ; rho_sqrt <- sqrt(rh_o) ; rho_sqrtm <- sqrt(1-rh_o)
def_prob <- 0.3; def_thresh <- qnorm(def_prob)
def_distr(0.03, def_thresh=def_thresh, rh_o=rh_o)
# Plot probability distribution of defaults
curve(expr=def_distr(x, def_thresh=def_thresh, rh_o=0.1),
xlim=c(0, 1.0), lwd=3,
xlab="percentage of defaults", ylab="density",
col="green", main="Distribution of Defaults")

# Plot default distribution with higher correlation
curve(expr=def_distr(x, def_thresh=def_thresh, rh_o=0.3),
xlab="default percentage", ylab="",
add=TRUE, lwd=3, col="blue", main="")
# Add legend
legend(x="topright",
 legend=c("high correlation", "low correlation"),
 title=NULL, inset=0.05, cex=0.8, bg="white",
 bty="n", lwd=6, lty=1, col=c("blue", "green"))
# Add unconditional default probability
abline(v=def_prob, col="red", lwd=3)
text(x=def_prob, y=2,
 labels="default probability",
 lwd=2, srt=90, pos=2)

# Plot default distribution with low correlation
curve(expr=def_distr(x, def_thresh=def_thresh, rh_o=0.01),
xlab="default percentage", ylab="", lwd=2,
col="green", main="Distribution of Defaults")
# Plot default distribution with high correlation
curve(expr=def_distr(x, def_thresh=def_thresh, rh_o=0.99),
xlab="percentage of defaults", ylab="density",
add=TRUE, lwd=2, n=10001, col="blue", main="")

# Add legend
legend(x="top",
 legend=c("high correlation", "low correlation"),
 title=NULL, inset=0.1, cex=0.8, bg="white",
 bty="n", lwd=6, lty=1, col=c("blue", "green"))
# Add unconditional default probability
abline(v=0.1, col="red", lwd=2)
text(x=0.1, y=10, lwd=2, pos=4,
 labels="default probability")

# Get help for integrate()
?integrate
# Calculate slowly converging integral
func_tion <- function(x) {1/((x+1)*sqrt(x))}
integrate(func_tion, lower=0, upper=10)
integrate(func_tion, lower=0, upper=Inf)
# Integrate function with parameter lamb_da
func_tion <- function(x, lamb_da=1) {
  exp(-x*lamb_da)
}  # end func_tion
integrate(func_tion, lower=0, upper=Inf)
integrate(func_tion, lower=0, upper=Inf, lamb_da=2)
# Cumulative probability over normal distribution
pnorm(-2)
integrate(dnorm, low=2, up=Inf)
str(dnorm)
pnorm(-1)
integrate(dnorm, low=2, up=Inf, mean=1)
# Expected value over normal distribution
integrate(function(x) x*dnorm(x), low=2, up=Inf)

rh_o <- 0.1; l_gd <- 0.4
# Define Vasicek loss distribution function
loss_distr <- function(x, def_thresh=(-2), rh_o=0.2, l_gd=0.4)
  sqrt((1-rh_o)/rh_o)*exp(-(sqrt(1-rh_o)*qnorm(x/l_gd) - def_thresh)^2/(2*rh_o) + qnorm(x/l_gd)^2/2)/l_gd
integrate(loss_distr, low=0, up=l_gd,
  def_thresh=(-2), rh_o=rh_o, l_gd=l_gd)

# Plot probability distribution of losses
def_prob <- 0.05; def_thresh <- qnorm(def_prob)
curve(expr=loss_distr(x, def_thresh=def_thresh, rh_o=rh_o),
type="l", xlim=c(0, 0.06),
xlab="loss percentage", ylab="density", lwd=3,
col="orange", main="Distribution of Losses")
# Add line for expected loss
abline(v=l_gd*def_prob, col="red", lwd=3)
text(x=l_gd*def_prob-0.001, y=10, labels="expected loss",
 lwd=2, srt=90, pos=3)

# Define cumulative default probability function
cum_loss <- function(x, def_thresh=(-2), rh_o=0.2, l_gd=0.4)
  pnorm((sqrt(1-rh_o)*qnorm(x/l_gd) - def_thresh)/sqrt(rh_o))
# Define Vasicek loss distribution function
# (vectorized version with error handling for x)
loss_distr <- function(x, def_thresh=-2, rh_o=0.1, l_gd=0.4) {
  q_norm <- ifelse(x/l_gd < 0.999, qnorm(x/l_gd), 3.1)
  sqrt((1-rh_o)/rh_o)*exp(-(sqrt(1-rh_o)*q_norm - def_thresh)^2/(2*rh_o) + q_norm^2/2)/l_gd
}  # end loss_distr

def_prob <- 0.2; def_thresh <- qnorm(def_prob)
rh_o <- 0.1; l_gd <- 0.4
at_tach <- 0.15; de_tach <- 0.2
# Expected tranche loss is sum of two terms
tranche_loss <-
  # Loss between at_tach and de_tach
  integrate(function(x, at_tach) (x-at_tach)*loss_distr(x,
def_thresh=def_thresh, rh_o=rh_o, l_gd=l_gd),
low=at_tach, up=de_tach, at_tach=at_tach)$value / (de_tach-at_tach) +
  # Loss in excess of de_tach
  (1-cum_loss(x=de_tach, def_thresh=def_thresh, rh_o=rh_o, l_gd=l_gd))
# Plot probability distribution of losses
curve(expr=loss_distr(x, def_thresh=def_thresh, rh_o=rh_o),
type="l", xlim=c(0, 3*l_gd*def_prob),
xlab="loss percentage", ylab="density", lwd=3,
col="orange", main="CDO Tranche Losses")
# Add line for expected loss
abline(v=l_gd*def_prob, col="red", lwd=3)
text(x=l_gd*def_prob-0.001, y=4, labels="expected loss",
 lwd=2, srt=90, pos=3)
# Add lines for attach and detach
abline(v=at_tach, col="blue", lwd=3)
text(x=at_tach-0.001, y=4, labels="attach",
 lwd=2, srt=90, pos=3)
abline(v=de_tach, col="green", lwd=3)
text(x=de_tach-0.001, y=4, labels="detach",
 lwd=2, srt=90, pos=3)
# Add shading for CDO tranche
var_s <- seq(at_tach, de_tach, length=100)
densi_ty <- sapply(var_s, loss_distr,
  def_thresh=def_thresh, rh_o=rh_o)
# Draw shaded polygon
polygon(c(at_tach, var_s, de_tach), density=20,
  c(-1, densi_ty, -1), col="red", border=NA)
text(x=0.5*(at_tach+de_tach), y=0, labels="CDO tranche", cex=0.9, lwd=2, pos=3)

# Add lines for unexpected loss
abline(v=0.04, col="blue", lwd=3)
arrows(x0=0.02, y0=35, x1=0.04, y1=35,
 code=3, lwd=3, cex=0.5)
text(x=0.03, y=36, labels="unexpected loss",
     lwd=2, pos=3)
# Add lines for VaR
abline(v=0.055, col="red", lwd=3)
arrows(x0=0.0, y0=25, x1=0.055, y1=25,
 code=3, lwd=3, cex=0.5)
text(x=0.03, y=26, labels="VaR", lwd=2, pos=3)
text(x=0.055-0.001, y=10, labels="VaR",
 lwd=2, srt=90, pos=3)

va_r <- 0.04; var_max <- 4*l_gd*def_prob
# Calculate CVaR
c_var <- integrate(function(x, ...) x*loss_distr(x, ...),
  low=va_r, up=l_gd, def_thresh=def_thresh, rh_o=rh_o, l_gd=l_gd)$value
c_var <- c_var/integrate(loss_distr, low=va_r, up=l_gd, def_thresh=def_thresh, rh_o=rh_o, l_gd=l_gd)$value
# Plot probability distribution of losses
curve(expr=loss_distr(x, def_thresh=def_thresh, rh_o=rh_o),
type="l", xlim=c(0, 0.06),
xlab="loss percentage", ylab="density", lwd=3,
col="orange", main="Conditional Value at Risk")
# Add line for expected loss
abline(v=l_gd*def_prob, col="red", lwd=3)
text(x=l_gd*def_prob-0.001, y=10, labels="expected loss", lwd=2, srt=90, pos=3)

# Add lines for VaR
abline(v=va_r, col="red", lwd=3)
text(x=va_r-0.001, y=10, labels="VaR",
 lwd=2, srt=90, pos=3)
# Add shading for CVaR
var_s <- seq(va_r, var_max, length=100)
densi_ty <- sapply(var_s, loss_distr,
  def_thresh=def_thresh, rh_o=rh_o)
# Draw shaded polygon
polygon(c(va_r, var_s, var_max), density=20,
  c(-1, densi_ty, -1), col="red", border=NA)
text(x=va_r+0.005, y=0, labels="CVaR", lwd=2, pos=3)

# VaR (quantile of the loss distribution)
var_func <- function(x, def_thresh=qnorm(0.1), rh_o=0.1, l_gd=0.4)
  l_gd*pnorm((sqrt(rh_o)*qnorm(x) + def_thresh)/sqrt(1-rh_o))
var_func(x=0.99, def_thresh=def_thresh, rh_o=rh_o, l_gd=l_gd)
# Plot VaR
curve(expr=var_func(x, def_thresh=def_thresh, rh_o=rh_o, l_gd=l_gd),
type="l", xlim=c(0, 0.999), xlab="confidence level", ylab="VaR", lwd=3,
col="orange", main="VaR versus Confidence Level")
# Add line for expected loss
abline(h=l_gd*def_prob, col="red", lwd=3)
text(x=0.2, y=l_gd*def_prob, labels="expected loss", lwd=2, pos=3)

# Integrate loss_distr() over full range
integrate(loss_distr, low=0.0, up=l_gd,
    def_thresh=def_thresh, rh_o=rh_o, l_gd=l_gd)
# Calculate expected losses using loss_distr()
integrate(function(x, ...) x*loss_distr(x, ...),
    low=0.0, up=l_gd,
    def_thresh=def_thresh, rh_o=rh_o, l_gd=l_gd)
# Calculate confidence levels corresponding to VaR values
var_s <- seq(0.07, 0.12, 0.001)
level_s <- sapply(var_s, function(va_r, ...) {
  integrate(loss_distr, low=va_r, up=l_gd, ...)
}, def_thresh=def_thresh, rh_o=rh_o, l_gd=l_gd)  # end sapply
level_s <- cbind(as.numeric(t(level_s)[, 1]), var_s)
colnames(level_s) <- c("level_s", "VaRs")
# Calculate 95% confidence level VaR value
level_s[match(TRUE, level_s[, "level_s"] < 0.05), "VaRs"]
plot(x=1-level_s[, "level_s"],
     y=level_s[, "VaRs"], lwd=2,
     xlab="confidence level", ylab="VaRs",
     t="l", main="VaR Values and Confidence Levels")

# Calculate CVaR values
cvar_s <- sapply(var_s, function(va_r, ...) {
  integrate(function(x, ...) x*loss_distr(x, ...),
      low=va_r, up=l_gd, ...)
}, def_thresh=def_thresh, rh_o=rh_o, l_gd=l_gd)  # end sapply
level_s <- cbind(level_s, as.numeric(t(cvar_s)[, 1]))
colnames(level_s)[3] <- "CVaRs"
# Divide CVaR by confidence level
level_s[, "CVaRs"] <- level_s[, "CVaRs"]/level_s[, "level_s"]
# Calculate 95% confidence level CVaR value
level_s[match(TRUE,
  level_s[, "level_s"] < 0.05), "CVaRs"]
# Plot CVaRs
plot(x=1-level_s[, "level_s"],
     y=level_s[, "CVaRs"],
     t="l", col="red", lwd=2,
     ylim=range(level_s[, c("VaRs", "CVaRs")]),
     xlab="confidence level", ylab="CVaRs",
     main="CVaR Values and Confidence Levels")

# Add VaRs
lines(x=1-level_s[, "level_s"], y=level_s[, "VaRs"], lwd=2)
# Add legend
legend(x="topleft", legend=c("CVaRs", "VaRs"),
 title="default probability = 5%
correlation = 10%
loss given default = 40%",
 inset=0.1, cex=0.8, bg="white", bty="n",
 lwd=6, lty=1, col=c("red", "black"))

# Define model parameters
n_assets <- 300; n_simu <- 1000; l_gd <- 0.4
# Define correlation parameters
rh_o <- 0.2; rho_sqrt <- sqrt(rh_o); rho_sqrtm <- sqrt(1-rh_o)
# Calculate default probabilities and thresholds
set.seed(1121)
def_probs <- runif(n_assets, max=0.2)
def_thresh <- qnorm(def_probs)
# Calculate vector of systematic factors
system_atic <- rnorm(n_simu)
# Simulate losses under Vasicek model
asset_s <- matrix(rnorm(n_simu*n_assets), ncol=n_simu)
asset_s <- t(rho_sqrt*system_atic + t(rho_sqrtm*asset_s))
loss_es <- l_gd*colSums(asset_s < def_thresh)/n_assets

# Calculate VaRs
level_s <- seq(0.93, 0.99, 0.01)
var_s <- quantile(loss_es, probs=level_s)
plot(x=level_s, y=var_s, t="l", lwd=2,
     xlab="confidence level", ylab="VaRs",
     main="Simulated VaR and Confidence Levels")

# Calculate CVaRs
cvar_s <- sapply(var_s, function(va_r) {
  mean(loss_es[loss_es >= va_r])
})  # end sapply
cvar_s <- cbind(cvar_s, var_s)
# Alternative CVaR calculation using frequency table
# first calculate frequency table of loss_es
# ta_ble <- table(loss_es)/n_simu
# Calculate CVaRs from frequency table
# Cvar_s <- sapply(var_s, function(va_r) {
#   tai_l <- ta_ble[names(ta_ble) > va_r]
#   tai_l %*% as.numeric(names(tai_l)) / sum(tai_l)
# })  # end sapply
# Plot CVaRs
plot(x=level_s, y=cvar_s[, "cvar_s"],
     t="l", col="red", lwd=2,
     ylim=range(cvar_s),
     xlab="confidence level", ylab="CVaRs",
     main="Simulated CVaR and Confidence Levels")

# Add VaRs
lines(x=level_s, y=cvar_s[, "var_s"], lwd=2)
# Add legend
legend(x="topleft", legend=c("CVaRs", "VaRs"), bty="n",
 title=NULL, inset=0.05, cex=0.8, bg="white",
 lwd=6, lty=1, col=c("red", "black"))

calc_var <- function(def_thresh, # Default thresholds
               l_gd=0.6, # loss given default
               rho_sqrt, rho_sqrtm, # asset correlation
               n_simu=1000, # number of simulations
               level_s=seq(0.93, 0.99, 0.01) # Confidence levels
               ) {
  # Define model parameters
  n_assets <- NROW(def_thresh)
  # Simulate losses under Vasicek model
  system_atic <- rnorm(n_simu)
  asset_s <- matrix(rnorm(n_simu*n_assets), ncol=n_simu)
  asset_s <- t(rho_sqrt*system_atic + t(rho_sqrtm*asset_s))
  loss_es <- l_gd*colSums(asset_s < def_thresh)/n_assets
  # Calculate VaRs and CVaRs
  var_s <- quantile(loss_es, probs=level_s)
  cvar_s <- sapply(var_s, function(va_r) {
    mean(loss_es[loss_es >= va_r])
  })  # end sapply
  names(var_s) <- level_s
  names(cvar_s) <- level_s
  c(var_s, cvar_s)
}  # end calc_var

# Define model parameters
n_assets <- 300; n_simu <- 1000; l_gd <- 0.4
rh_o <- 0.2; rho_sqrt <- sqrt(rh_o); rho_sqrtm <- sqrt(1-rh_o)
# Calculate default probabilities and thresholds
set.seed(1121)
def_probs <- runif(n_assets, max=0.2)
def_thresh <- qnorm(def_probs)
# Define number of bootstrap simulations
n_boot <- 500
# Perform bootstrap of calc_var
set.seed(1121)
boot_data <- sapply(rep(l_gd, n_boot), calc_var,
  def_thresh=def_thresh,
  rho_sqrt=rho_sqrt, rho_sqrtm=rho_sqrtm,
  n_simu=n_simu, level_s=level_s)  # end sapply
boot_data <- t(boot_data)
# Calculate vectors of standard errors of VaR and CVaR from boot_data data
std_error_var <- apply(boot_data[, 1:7], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
std_error_cvar <- apply(boot_data[, 8:14], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
# Scale the standard errors of VaRs and CVaRs
std_error_var[2, ] <- std_error_var[2, ]/std_error_var[1, ]
std_error_cvar[2, ] <- std_error_cvar[2, ]/std_error_cvar[1, ]

# Plot the standard errors of VaRs and CVaRs
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
plot(x=colnames(std_error_cvar), y=std_error_cvar[2, ],
  t="l", col="red", lwd=2,
  ylim=range(c(std_error_var[2, ], std_error_cvar[2, ])),
  xlab="confidence level", ylab="standard error",
  main="Scaled standard errors of CVaR and VaR")
lines(x=colnames(std_error_var), y=std_error_var[2, ], lwd=2)
legend(x="topleft", legend=c("CVaRs", "VaRs"), bty="n",
 title=NULL, inset=0.05, cex=0.8, bg="white",
 lwd=6, lty=1, col=c("red", "black"))

library(parallel)  # load package parallel
n_cores <- detectCores() - 1  # number of cores
clus_ter <- makeCluster(n_cores)  # Initialize compute cluster
# Perform bootstrap of calc_var for Windows
clusterSetRNGStream(clus_ter, 1121)
boot_data <- parLapply(clus_ter, rep(l_gd, n_boot),
  fun=calc_var, def_thresh=def_thresh,
  rho_sqrt=rho_sqrt, rho_sqrtm=rho_sqrtm,
  n_simu=n_simu, level_s=level_s)  # end parLapply
# Bootstrap under Mac-OSX or Linux
boot_data <- mclapply(rep(l_gd, n_boot),
  FUN=calc_var, def_thresh=def_thresh,
  rho_sqrt=rho_sqrt, rho_sqrtm=rho_sqrtm,
  n_simu=n_simu, level_s=level_s)  # end mclapply
boot_data <- rutils::do_call(rbind, boot_data)
stopCluster(clus_ter)  # Stop R processes over cluster
# Calculate vectors of standard errors of VaR and CVaR from boot_data data
std_error_var <- apply(boot_data[, 1:7], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
std_error_cvar <- apply(boot_data[, 8:14], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
# Scale the standard errors of VaRs and CVaRs
std_error_var[2, ] <- std_error_var[2, ]/std_error_var[1, ]
std_error_cvar[2, ] <- std_error_cvar[2, ]/std_error_cvar[1, ]

# Plot the standard errors of VaRs and CVaRs
plot(x=colnames(std_error_cvar),
  y=std_error_cvar[2, ], t="l", col="red", lwd=2,
  ylim=range(c(std_error_var[2, ], std_error_cvar[2, ])),
  xlab="confidence level", ylab="standard error",
  main="Scaled standard errors of CVaR and VaR")
lines(x=colnames(std_error_var), y=std_error_var[2, ], lwd=2)
legend(x="topleft", legend=c("CVaRs", "VaRs"), bty="n",
 title=NULL, inset=0.05, cex=0.8, bg="white",
 lwd=6, lty=1, col=c("red", "black"))

calc_var <- function(def_probs, # Default probabilities
               l_gd=0.6, # loss given default
               rho_sqrt, rho_sqrtm, # asset correlation
               n_simu=1000, # number of simulations
               level_s=seq(0.93, 0.99, 0.01) # Confidence levels
               ) {
  # Calculate random default thresholds
  def_thresh <- qnorm(runif(1, min=0.5, max=1.5)*def_probs)
  # Simulate losses under Vasicek model
  n_assets <- NROW(def_probs)
  system_atic <- rnorm(n_simu)
  asset_s <- matrix(rnorm(n_simu*n_assets), ncol=n_simu)
  asset_s <- t(rho_sqrt*system_atic + t(rho_sqrtm*asset_s))
  loss_es <- l_gd*colSums(asset_s < def_thresh)/n_assets
  # Calculate VaRs and CVaRs
  var_s <- quantile(loss_es, probs=level_s)
  cvar_s <- sapply(var_s, function(va_r) {
    mean(loss_es[loss_es >= va_r])
  })  # end sapply
  names(var_s) <- level_s
  names(cvar_s) <- level_s
  c(var_s, cvar_s)
}  # end calc_var

library(parallel)  # load package parallel
n_cores <- detectCores() - 1  # number of cores
clus_ter <- makeCluster(n_cores)  # Initialize compute cluster
# Perform bootstrap of calc_var for Windows
clusterSetRNGStream(clus_ter, 1121)
boot_data <- parLapply(clus_ter, rep(l_gd, n_boot),
  fun=calc_var, def_probs=def_probs,
  rho_sqrt=rho_sqrt, rho_sqrtm=rho_sqrtm,
  n_simu=n_simu, level_s=level_s)  # end parLapply
# Bootstrap under Mac-OSX or Linux
boot_data <- mclapply(rep(l_gd, n_boot),
  FUN=calc_var, def_probs=def_probs,
  rho_sqrt=rho_sqrt, rho_sqrtm=rho_sqrtm,
  n_simu=n_simu, level_s=level_s)  # end mclapply
boot_data <- rutils::do_call(rbind, boot_data)
stopCluster(clus_ter)  # Stop R processes over cluster
# Calculate vectors of standard errors of VaR and CVaR from boot_data data
std_error_var <- apply(boot_data[, 1:7], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
std_error_cvar <- apply(boot_data[, 8:14], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
# Scale the standard errors of VaRs and CVaRs
std_error_var[2, ] <- std_error_var[2, ]/std_error_var[1, ]
std_error_cvar[2, ] <- std_error_cvar[2, ]/std_error_cvar[1, ]

# Plot the standard errors of VaRs and CVaRs
plot(x=colnames(std_error_cvar),
  y=std_error_cvar[2, ], t="l", col="red", lwd=2,
  ylim=range(c(std_error_var[2, ], std_error_cvar[2, ])),
  xlab="confidence level", ylab="standard error",
  main="Standard Errors of CVaR and VaR
  with Uncertain Default Probabilities")
lines(x=colnames(std_error_var), y=std_error_var[2, ], lwd=2)
legend(x="topright", legend=c("CVaRs", "VaRs"), bty="n",
 title=NULL, inset=0.05, cex=1.0, bg="white",
 lwd=6, lty=1, col=c("red", "black"))

# Define model parameters
n_assets <- 300; n_simu <- 1000; l_gd <- 0.4
# Define correlation parameters
rh_o <- 0.2; rho_sqrt <- sqrt(rh_o); rho_sqrtm <- sqrt(1-rh_o)
# Calculate default probabilities and thresholds
set.seed(1121)
def_probs <- runif(n_assets, max=0.2)
def_thresh <- qnorm(def_probs)
# Calculate vector of systematic factors
system_atic <- rnorm(n_simu)
# Calculate vector of idiosyncratic factors
idio_syncratic <-
  matrix(rnorm(n_simu*n_assets), ncol=n_simu)
# Simulate losses under Vasicek model
asset_s <-
  t(rho_sqrt*system_atic + t(rho_sqrtm*idio_syncratic))
loss_es <-
  l_gd*colSums(asset_s < def_thresh)/n_assets
# Calculate VaRs
level_s <- seq(0.93, 0.99, 0.01)
var_s <- quantile(loss_es, probs=level_s)

# Importance sampling losses
lamb_da <- 3
asset_s <-
  t(rho_sqrt*system_atic + t(rho_sqrtm*idio_syncratic))

cond_thresh <- outer(rho_sqrtm*def_thresh, -rho_sqrt*system_atic, FUN="+")

cond_probs <- pnorm(cond_thresh)

tilt_probs <- lamb_da*cond_probs/(1 + cond_probs*(lamb_da - 1))

weight_s <- (1 + tilt_probs*(lamb_da - 1))/lamb_da

tilt_thresh <- qnorm(tilt_probs)

loss_es <-
  l_gd*colSums(weight_s*(asset_s < tilt_thresh))/n_assets

var_s <- quantile(loss_es, probs=level_s)


foo <- (uni_form < def_probs)

def_thresh <- qnorm(def_probs)





asset_s <- t(rho_sqrt*(system_atic - lamb_da) +
t(rho_sqrtm*idio_syncratic))
loss_es <-
  l_gd*colSums(asset_s < def_thresh)/n_assets
# Calculate VaRs
var_s <- quantile(loss_es, probs=level_s)


asset_s <- t(rho_sqrt*(system_atic - lamb_da) +
t(rho_sqrtm*idio_syncratic))
loss_es <-
  l_gd*colSums(asset_s < def_thresh)/n_assets
# Calculate VaRs
var_s <- quantile(loss_es, probs=level_s)



plot(x=level_s, y=var_s, t="l", lwd=2,
     xlab="confidence level", ylab="VaRs",
     main="Simulated VaR and Confidence Levels")

# Define model parameters
n_assets <- 300; n_simu <- 1000; l_gd <- 0.4
rh_o <- 0.2; rho_sqrt <- sqrt(rh_o); rho_sqrtm <- sqrt(1-rh_o)
# Calculate default probabilities and thresholds
set.seed(1121)
def_probs <- runif(n_assets, max=0.2)
def_thresh <- qnorm(def_probs)
# Define number of bootstrap simulations
n_boot <- 500
# Perform bootstrap of calc_var
set.seed(1121)
boot_data <- sapply(rep(l_gd, n_boot),
  calc_var,
  def_thresh=def_thresh,
  rho_sqrt=rho_sqrt, rho_sqrtm=rho_sqrtm,
  n_simu=n_simu, level_s=level_s)  # end sapply
boot_data <- t(boot_data)
# Calculate vectors of standard errors of VaR and CVaR from boot_data data
std_error_var <- apply(boot_data[, 1:7], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
std_error_cvar <- apply(boot_data[, 8:14], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
# Scale the standard errors of VaRs and CVaRs
std_error_var[2, ] <- std_error_var[2, ]/std_error_var[1, ]
std_error_cvar[2, ] <- std_error_cvar[2, ]/std_error_cvar[1, ]
