# sym_bols contains all the symbols in rutils::env_etf$re_turns except for "VXX"
sym_bols <- colnames(rutils::env_etf$re_turns)
sym_bols <- sym_bols[!(sym_bols=="VXX")]
# Extract columns of rutils::env_etf$re_turns and remove NA values
re_turns <- rutils::env_etf$re_turns[, sym_bols]
re_turns <- zoo::na.locf(re_turns)
re_turns <- na.omit(re_turns)
# Calculate vector of monthly end points and start points
look_back <- 12
end_points <- rutils::calc_endpoints(re_turns, inter_val="months")
end_points[end_points<2*NCOL(re_turns)] <- 2*NCOL(re_turns)
len_gth <- NROW(end_points)
# sliding window
start_points <- c(rep_len(1, look_back-1), end_points[1:(len_gth-look_back+1)])
# expanding window
start_points <- rep_len(1, NROW(end_points))
# risk_free is the daily risk-free rate
risk_free <- 0.03/260
# Calculate daily excess returns
ex_cess <- re_turns - risk_free
# Perform loop over end_points
portf_rets <- lapply(2:NROW(end_points),
  function(i) {
    # subset the ex_cess returns
    ex_cess <- ex_cess[start_points[i-1]:end_points[i-1], ]
    in_verse <- solve(cov(ex_cess))
    # calculate the maximum Sharpe ratio portfolio weights.
    weight_s <- in_verse %*% colMeans(ex_cess)
    weight_s <- drop(weight_s/sum(weight_s^2))
    # subset the re_turns
    re_turns <- re_turns[(end_points[i-1]+1):end_points[i], ]
    # calculate the out-of-sample portfolio returns
    xts(re_turns %*% weight_s, index(re_turns))
  }  # end anonymous function
)  # end lapply
portf_rets <- rutils::do_call(rbind, portf_rets)
colnames(portf_rets) <- "portf_rets"
# Calculate compounded cumulative portfolio returns
portf_rets <- cumsum(portf_rets)
quantmod::chart_Series(portf_rets,
  name="Cumulative Returns of Max Sharpe Portfolio Strategy")
# create random covariance matrix
set.seed(1121)
mat_rix <- matrix(runif(5e2), nc=5)
cov_mat <- cov(mat_rix)
cor_mat <- cor(mat_rix)
std_dev <- sqrt(diag(cov_mat))
# calculate target matrix
cor_mean <- mean(cor_mat[upper.tri(cor_mat)])
tar_get <- matrix(cor_mean, nr=NROW(cov_mat), nc=NCOL(cov_mat))
diag(tar_get) <- 1
tar_get <- t(t(tar_get * std_dev) * std_dev)
# calculate shrinkage covariance matrix
al_pha <- 0.5
cov_shrink <- (1-al_pha)*cov_mat + al_pha*tar_get
# calculate inverse matrix
in_verse <- solve(cov_shrink)
# create random covariance matrix
set.seed(1121)
mat_rix <- matrix(runif(5e2), nc=5)
cov_mat <- cov(mat_rix)
# perform eigen decomposition
ei_gen <- eigen(cov_mat)
eigen_vec <- ei_gen$vectors
# calculate regularized inverse matrix
max_eigen <- 2
in_verse <- eigen_vec[, 1:max_eigen] %*%
  (t(eigen_vec[, 1:max_eigen]) / ei_gen$values[1:max_eigen])
# VTI percentage returns
re_turns <- rutils::diff_xts(log(quantmod::Cl(rutils::env_etf$VTI)))
# define end points
end_points <- seq_along(re_turns)
len_gth <- NROW(end_points)
look_back <- 51
# start_points are multi-period lag of end_points
start_points <- c(rep_len(1, look_back-1),
    end_points[1:(len_gth-look_back+1)])
# define list of look-back intervals for aggregations over past
look_backs <- lapply(seq_along(end_points),
  function(in_dex) {
    start_points[in_dex]:end_points[in_dex]
})  # end lapply
# calculate realized VTI variance in sapply() loop
vari_ance <- sapply(look_backs,
  function(look_back) {
    ret_s <- re_turns[look_back]
    sum((ret_s - mean(ret_s))^2)
}) / (look_back-1)  # end sapply
tail(vari_ance)
class(vari_ance)
# coerce vari_ance into xts
vari_ance <- xts(vari_ance, order.by=index(re_turns))
colnames(vari_ance) <- "VTI.variance"
head(vari_ance)
# calculate VTI variance using package roll
library(roll)  # load roll
vari_ance <-
  roll::roll_var(re_turns, width=look_back)
colnames(vari_ance) <- "VTI.variance"
head(vari_ance)
sum(is.na(vari_ance))
vari_ance[1:(look_back-1)] <- 0
# benchmark calculation of rolling variance
library(microbenchmark)
summary(microbenchmark(
  roll_sapply=sapply(look_backs, function(look_back) {
    ret_s <- re_turns[look_back]
    sum((ret_s - mean(ret_s))^2)
  }),
  ro_ll=roll::roll_var(re_turns, width=look_back),
  times=10))[, c(1, 4, 5)]
# calculate EWMA VTI variance using filter()
wid_th <- 51
weight_s <- exp(-0.1*1:wid_th)
weight_s <- weight_s/sum(weight_s)
vari_ance <- stats::filter(re_turns^2,
    filter=weight_s, sides=1)
vari_ance[1:(wid_th-1)] <- vari_ance[wid_th]
class(vari_ance)
vari_ance <- as.numeric(vari_ance)
x_ts <- xts:::xts(sqrt(vari_ance), order.by=index(re_turns))
# plot EWMA standard deviation
chart_Series(x_ts,
  name="EWMA standard deviation")
dygraphs::dygraph(x_ts, main="EWMA standard deviation")
# calculate VTI variance using package roll
library(roll)  # load roll
vari_ance <- roll::roll_var(re_turns,
  weights=rev(weight_s), width=wid_th)
colnames(vari_ance) <- "VTI.variance"
class(vari_ance)
head(vari_ance)
sum(is.na(vari_ance))
vari_ance[1:(wid_th-1)] <- 0
x11(width=6, height=4)
par(mar=c(4, 3, 1, 1), oma=c(0, 0, 0, 0))
# VTI percentage returns
re_turns <- rutils::diff_xts(log(quantmod::Cl(rutils::env_etf$VTI)))
# calculate VTI variance using package roll
look_back <- 22
vari_ance <-
  roll::roll_var(re_turns, width=look_back)
vari_ance[1:(look_back-1)] <- 0
colnames(vari_ance) <- "VTI.variance"
# number of look_backs that fit over re_turns
n_row <- NROW(re_turns)
num_agg <- n_row %/% look_back
end_points <- # define end_points with beginning stub
  n_row-look_back*num_agg + (0:num_agg)*look_back
len_gth <- NROW(end_points)
# subset vari_ance to end_points
vari_ance <- vari_ance[end_points]
# improved autocorrelation function
acf_plus(coredata(vari_ance), lag=10, main="")
title(main="acf of variance", line=-1)
# partial autocorrelation
pacf(coredata(vari_ance), lag=10, main="", ylab=NA)
title(main="pacf of variance", line=-1)
# define GARCH parameters
om_ega <- 0.01 ; al_pha <- 0.2
be_ta <- 0.2 ; len_gth <- 1000
re_turns <- numeric(len_gth)
vari_ance <- numeric(len_gth)
vari_ance[1] <- om_ega/(1-al_pha-be_ta)
re_turns[1] <- rnorm(1, sd=sqrt(vari_ance[1]))
# simulate GARCH model
set.seed(1121)  # reset random numbers
for (i in 2:len_gth) {
  re_turns[i] <- rnorm(n=1, sd=sqrt(vari_ance[i-1]))
  vari_ance[i] <- om_ega + al_pha*re_turns[i]^2 +
    be_ta*vari_ance[i-1]
}  # end for
x11(width=6, height=4)
par(mar=c(3, 3, 1, 1), oma=c(0, 0, 0, 0))
# plot GARCH cumulative returns
plot(cumsum(re_turns/100), t="l",
  lwd=2, col="orange", xlab="", ylab="",
  main="GARCH cumulative returns")
date_s <- seq.Date(from=Sys.Date()-len_gth+1,
  to=Sys.Date(), length.out=len_gth)
x_ts <- xts:::xts(cumsum(re_turns/100), order.by=date_s)
dygraphs::dygraph(x_ts, main="GARCH cumulative returns")
# plot GARCH standard deviation
plot(sqrt(vari_ance), t="l",
  col="orange", xlab="", ylab="",
  main="GARCH standard deviation")
x_ts <- xts:::xts(sqrt(vari_ance), order.by=date_s)
dygraphs::dygraph(x_ts, main="GARCH standard deviation")
# define GARCH parameters
om_ega <- 0.0001 ; al_pha <- 0.5
be_ta <- 0.1 ; len_gth <- 10000
re_turns <- numeric(len_gth)
vari_ance <- numeric(len_gth)
vari_ance[1] <- om_ega/(1-al_pha-be_ta)
re_turns[1] <- rnorm(1, sd=sqrt(vari_ance[1]))
# simulate GARCH model
set.seed(1121)  # reset random numbers
for (i in 2:len_gth) {
  re_turns[i] <- rnorm(n=1, sd=sqrt(vari_ance[i-1]))
  vari_ance[i] <- om_ega + al_pha*re_turns[i]^2 +
    be_ta*vari_ance[i-1]
}  # end for
# calculate kurtosis of GARCH returns
moments::moment(re_turns, order=4) /
  moments::moment(re_turns, order=2)^2
# perform Jarque-Bera test of normality
tseries::jarque.bera.test(re_turns)
# plot histogram of GARCH returns
histo_gram <- hist(re_turns, col="lightgrey",
  xlab="returns", breaks=200, xlim=c(-0.05, 0.05),
  ylab="frequency", freq=FALSE,
  main="GARCH returns histogram")
lines(density(re_turns, adjust=1.5),
lwd=3, col="blue")
optim_fit <- MASS::fitdistr(re_turns,
  densfun="t", df=2, lower=c(-1, 1e-7))
lo_cation <- optim_fit$estimate[1]
sc_ale <- optim_fit$estimate[2]
curve(expr=dt((x-lo_cation)/sc_ale, df=2)/sc_ale,
  type="l", xlab="", ylab="", lwd=3,
  col="red", add=TRUE)
legend("topright", inset=0.05,
 leg=c("density", "t-distr w/ 2 dof"),
 lwd=6, lty=c(1, 1),
 col=c("blue", "red"))
# use fixed notation instead of exponential notation
options(scipen=999)
library(fGarch)
# fit returns into GARCH
garch_fit <- fGarch::garchFit(data=re_turns)
# fitted GARCH parameters
round(garch_fit@fit$coef, 5)
# actual GARCH parameters
round(c(mu=mean(re_turns), omega=om_ega,
  alpha=al_pha, beta=be_ta), 5)
# plot GARCH fitted standard deviation
plot.zoo(sqrt(garch_fit@fit$series$h), t="l",
  col="orange", xlab="", ylab="",
  main="GARCH fitted standard deviation")
# specify GARCH model
garch_spec <- fGarch::garchSpec(
  model=list(omega=om_ega, alpha=al_pha, beta=be_ta))
# simulate GARCH model
garch_sim <-
  fGarch::garchSim(spec=garch_spec, n=len_gth)
re_turns <- as.numeric(garch_sim)
# calculate kurtosis of GARCH returns
moments::moment(re_turns, order=4) /
  moments::moment(re_turns, order=2)^2
# perform Jarque-Bera test of normality
tseries::jarque.bera.test(re_turns)
# plot histogram of GARCH returns
histo_gram <- hist(re_turns, col="lightgrey",
  xlab="returns", breaks=200, xlim=c(-0.05, 0.05),
  ylab="frequency", freq=FALSE,
  main="GARCH returns histogram")
lines(density(re_turns, adjust=1.5),
lwd=3, col="blue")
# fir t-distribution into GARCH returns
optim_fit <- MASS::fitdistr(re_turns,
  densfun="t", df=2, lower=c(-1, 1e-7))
lo_cation <- optim_fit$estimate[1]
sc_ale <- optim_fit$estimate[2]
curve(expr=dt((x-lo_cation)/sc_ale, df=2)/sc_ale,
  type="l", xlab="", ylab="", lwd=3,
  col="red", add=TRUE)
legend("topright", inset=0.05,
 leg=c("density", "t-distr w/ 2 dof"),
 lwd=6, lty=c(1, 1),
 col=c("blue", "red"))
# load package HighFreq
library(HighFreq)
head(SPY_TAQ)
# load package HighFreq
library(HighFreq)
head(SPY)
# install package HighFreq from github
devtools::install_github(repo="algoquant/HighFreq")
# load package HighFreq
library(HighFreq)
# get documentation for package HighFreq
# get short description
packageDescription("HighFreq")
# load help page
help(package="HighFreq")
# list all datasets in "HighFreq"
data(package="HighFreq")
# list all objects in "HighFreq"
ls("package:HighFreq")
# remove HighFreq from search path
detach("package:HighFreq")
# load package HighFreq
library(HighFreq)
# you can see SPY when listing objects in HighFreq
ls("package:HighFreq")
# you can see SPY when listing datasets in HighFreq
data(package="HighFreq")
# but the SPY dataset isn't listed in the workspace
ls()
# HighFreq datasets are lazy loaded and available when needed
head(SPY)
# load all the datasets in package HighFreq
data(hf_data)
# HighFreq datasets are now loaded and in the workspace
head(SPY)
library(HighFreq)  # load HighFreq
# minutely SPY returns (unit per minute) single day
re_turns <- rutils::diff_xts(log(SPY["2012-02-13", 4]))
# minutely SPY volatility (unit per minute)
sd(re_turns)
# minutely SPY returns (unit per second)
re_turns <- rutils::diff_xts(log(SPY["2012-02-13", 4])) / 
  c(1, diff(.index(SPY["2012-02-13"])))
# minutely SPY volatility scaled to unit per minute
60*sd(re_turns)
# minutely SPY returns multiple days no overnight scaling
re_turns <- rutils::diff_xts(log(SPY[, 4]))
# minutely SPY volatility (unit per minute)
sd(re_turns)
# minutely SPY returns (unit per second)
re_turns <- rutils::diff_xts(log(SPY[, 4])) / 
  c(1, diff(.index(SPY)))
# minutely SPY volatility scaled to unit per minute
60*sd(re_turns)
table(c(1, diff(.index(SPY))))
library(HighFreq)  # load HighFreq
# daily OHLC SPY prices
SPY_daily <- 
  rutils::to_period(oh_lc=SPY, period="days")
# daily SPY returns and volatility
sd(rutils::diff_xts(log(SPY_daily[, 4])))
# minutely SPY returns (unit per minute)
re_turns <- rutils::diff_xts(log(SPY[, 4]))
# minutely SPY volatility scaled to daily interval
sqrt(6.5*60)*sd(re_turns)

# minutely SPY returns (unit per second)
re_turns <- rutils::diff_xts(log(SPY[, 4])) / 
  c(1, diff(.index(SPY)))
# minutely SPY volatility scaled to daily aggregation interval
60*sqrt(6.5*60)*sd(re_turns)

# daily SPY volatility
# including extra time over weekends and holidays
24*60*60*sd(rutils::diff_xts(log(SPY_daily[, 4])) / 
    c(1, diff(.index(SPY_daily))))
table(c(1, diff(.index(SPY_daily))))
library(HighFreq)  # load HighFreq
# daily SPY volatility from minutely prices using package TTR
library(TTR)
sqrt((6.5*60)*mean(na.omit(
  TTR::volatility(SPY, N=1,
          calc="yang.zhang"))^2))
# SPY volatility using package HighFreq
60*sqrt((6.5*60)*agg_regate(oh_lc=SPY,
    weight_ed=FALSE, mo_ment="run_variance",
    calc_method="yang_zhang"))
library(HighFreq)  # load HighFreq
# calculate variance
var_close <-
  HighFreq::run_variance(oh_lc=env_etf$VTI,
                   calc_method="close")
var_yang_zhang <-
  HighFreq::run_variance(oh_lc=env_etf$VTI)
vari_ance <-
  252*(24*60*60)^2*cbind(var_close, var_yang_zhang)
colnames(vari_ance) <-
  c("close var", "Yang-Zhang var")
# plot
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("black", "red")
x11()
chart_Series(vari_ance["2011-06/2011-12"],
  theme=plot_theme, name="Close and YZ variances")
legend("top", legend=colnames(vari_ance),
 bg="white", lty=c(1, 1), lwd=c(6, 6),
 col=plot_theme$col$line.col, bty="n")
# R startup chunk
# ```{r setup, include=FALSE}
library(shiny)
library(quantmod)
inter_val <- 31
cl_ose <- quantmod::Cl(rutils::env_etf$VTI)
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue")
# ```
#end R startup chunk
inputPanel(
  sliderInput("lamb_da", label="lambda:",
    min=0.01, max=0.2, value=0.1, step=0.01)
)  # end inputPanel
renderPlot({
  # calculate EWMA prices
  lamb_da <- input$lamb_da
  weight_s <- exp(-lamb_da*1:inter_val)
  weight_s <- weight_s/sum(weight_s)
  ew_ma <- filter(cl_ose, filter=weight_s, sides=1)
  ew_ma[1:(inter_val-1)] <- ew_ma[inter_val]
  ew_ma <- xts(cbind(cl_ose, ew_ma), order.by=index(cl_ose))
  colnames(ew_ma) <- c("VTI", "VTI EWMA")
  # plot EWMA prices
  ch_ob <- chart_Series(ew_ma, theme=plot_theme, name="EWMA prices")
  plot(ch_ob)
  legend("top", legend=colnames(ew_ma),
   inset=0.1, bg="white", lty=c(1, 1), lwd=c(2, 2),
   col=plot_theme$col$line.col, bty="n")
})  # end renderPlot
