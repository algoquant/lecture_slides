# Calculate ETF prices and simple returns
sym_bols <- c("VTI", "IEF", "DBC")
price_s <- rutils::env_etf$price_s[, sym_bols]
price_s <- zoo::na.locf(price_s)
price_s <- na.omit(price_s)
re_turns <- rutils::diff_it(price_s)
# Define look-back and look-forward intervals
end_points <- rutils::calc_endpoints(re_turns,
  inter_val="months")
n_col <- NCOL(re_turns)
len_gth <- NROW(end_points)
look_back <- 12
start_points <- c(rep_len(1, look_back-1),
  end_points[1:(len_gth-look_back+1)])
fwd_points <- end_points[c(2:len_gth, len_gth)]
# Perform loop over end-points and calculate aggregations
agg_fun <-
  function(re_turns) sum(re_turns)/sd(re_turns)
agg_s <- sapply(1:(len_gth-1), function(it_er) {
  c(back_aggs=sapply(re_turns[start_points[it_er]:end_points[it_er]], agg_fun),
    fwd_rets=sapply(re_turns[(end_points[it_er]+1):fwd_points[it_er]], sum))
})  # end sapply
agg_s <- t(agg_s)
# Select look-back and look-forward aggregations
back_aggs <- agg_s[, 1:n_col]
fwd_rets <- agg_s[, n_col+1:n_col]
# Calculate portfolio weights equal to number of shares
end_prices <- price_s[end_points[-len_gth]]
weight_s <-
  back_aggs/rowSums(abs(back_aggs))/end_prices
weight_s[is.na(weight_s)] <- 0
colnames(weight_s) <- colnames(re_turns)
# Calculate profits and losses
pnl_s <- rowSums(weight_s*fwd_rets)
pnl_s <- xts(pnl_s, index(end_prices))
colnames(pnl_s) <- "pnls"
# Calculate transaction costs
bid_offer <- 0.001
cost_s <-
  0.5*bid_offer*end_prices*abs(rutils::diff_it(weight_s))
cost_s <- rowSums(cost_s)
pnl_s <- (pnl_s - cost_s)
pnl_s <- cumsum(pnl_s)
# plot momentum strategy with VTI
cl_ose <- Cl(rutils::env_etf$VTI[index(end_prices)])
zoo::plot.zoo(cbind(cl_ose, pnl_s, weight_s),
  oma = c(3, 1, 3, 0), mar = c(0, 4, 0, 1), nc=1,
  xlab=NULL, main="ETF Momentum Strategy")
# define back-test functional
back_test_ep <- function(re_turns, price_s, agg_fun=sum,
    look_back=12, re_balance="months", bid_offer=0.001,
    end_points=rutils::calc_endpoints(re_turns, inter_val=re_balance),
    with_weights=FALSE, ...) {
  stopifnot("package:quantmod" %in% search() || require("quantmod", quietly=TRUE))
  # Define look-back and look-forward intervals
  n_col <- NCOL(re_turns)
  len_gth <- NROW(end_points)
  start_points <- c(rep_len(1, look_back-1), end_points[1:(len_gth-look_back+1)])
  fwd_points <- end_points[c(2:len_gth, len_gth)]
  # Perform loop over end-points and calculate aggregations
  agg_s <- sapply(1:(len_gth-1), function(it_er) {
    c(back_aggs=sapply(re_turns[start_points[it_er]:end_points[it_er]], agg_fun, ...),  # end sapply
    fwd_rets=sapply(re_turns[(end_points[it_er]+1):fwd_points[it_er]], sum))  # end sapply
  })  # end sapply
  agg_s <- t(agg_s)
  # Select look-back and look-forward aggregations
  back_aggs <- agg_s[, 1:n_col]
  fwd_rets <- agg_s[, n_col+1:n_col]
  # Calculate portfolio weights equal to number of shares
  end_prices <- price_s[end_points[-len_gth]]
  weight_s <- back_aggs/rowSums(abs(back_aggs))/end_prices
  weight_s[is.na(weight_s)] <- 0
  colnames(weight_s) <- colnames(re_turns)
  # Calculate profits and losses
  pnl_s <- rowSums(weight_s*fwd_rets)
  pnl_s <- xts(pnl_s, index(end_prices))
  colnames(pnl_s) <- "pnls"
  # Calculate transaction costs
  cost_s <- 0.5*bid_offer*end_prices*abs(rutils::diff_it(weight_s))
  cost_s <- rowSums(cost_s)
  pnl_s <- (pnl_s - cost_s)
  pnl_s <- cumsum(pnl_s)
  if (with_weights)
    cbind(pnl_s, weight_s)
  else
    pnl_s
}  # end back_test_ep
source("C:/Develop/R/lecture_slides/scripts/back_test.R")
look_backs <- seq(5, 60, by=5)
agg_fun <- function(re_turns) sum(re_turns)/sd(re_turns)
pro_files <- sapply(look_backs, function(x) {
  last(back_test_ep(re_turns=re_turns, price_s=price_s,
    re_balance="weeks", look_back=x, agg_fun=agg_fun))
})  # end sapply
plot(x=look_backs, y=pro_files, t="l",
  main="Strategy PnL as function of look_back",
  xlab="look_back (weeks)", ylab="pnl")
look_back <- look_backs[which.max(pro_files)]
pnl_s <- back_test_ep(re_turns=re_turns, price_s=price_s,
  re_balance="weeks", look_back=look_back, agg_fun=agg_fun,
  with_weights=TRUE)
cl_ose <- Cl(rutils::env_etf$VTI[index(pnl_s)])
# bind model returns with VTI
da_ta <- as.numeric(cl_ose[1, ])
da_ta <- cbind(cl_ose, da_ta*pnl_s[, 1]+da_ta)
colnames(da_ta) <- c("VTI", "momentum")
# plot momentum strategy with VTI
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue")
chart_Series(da_ta, theme=plot_theme, lwd=2,
       name="Momentum PnL")
legend("topleft", legend=colnames(da_ta),
  inset=0.1, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")
# combine momentum strategy with static
da_ta <- cbind(da_ta, 0.5* (da_ta[, "VTI"] + da_ta[, "momentum"]))
colnames(da_ta) <- c("VTI", "momentum", "combined")
# calculate strategy annualized Sharpe ratios
sapply(da_ta, function(cumu_lative) {
  x_ts <- na.omit(diff(log(cumu_lative)))
  sqrt(52)*sum(x_ts)/sd(x_ts)/NROW(x_ts)
})  # end sapply
# calculate strategy correlations
cor(na.omit(diff(log(da_ta))))
# plot momentum strategy combined with VTI
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue", "green")
chart_Series(da_ta, theme=plot_theme,
       name="Momentum strategy combined with VTI")
legend("topleft", legend=colnames(da_ta),
  inset=0.1, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")
# calculate betas
beta_s <- c(1, rutils::env_etf$capm_stats[
  match(sym_bols[-1],
  rownames(rutils::env_etf$capm_stats)),
  "Beta"])
names(beta_s)[1] <- sym_bols[1]
# weights times betas
weight_s <- price_s[index(pnl_s)]*pnl_s[, -1]
beta_s <- weight_s %*% beta_s
beta_s <- xts(beta_s, order.by=index(weight_s))
colnames(beta_s) <- "portf_beta"
zoo::plot.zoo(cbind(beta_s, cl_ose),
  oma = c(3, 1, 3, 0), mar = c(0, 4, 0, 1),
  main="betas & VTI", xlab="")
momentum_rets <- as.numeric(rutils::diff_it(pnl_s[, 1]))
vti_rets <- as.numeric(rutils::diff_it(cl_ose)/100)
# Merton-Henriksson test
vti_ <- cbind(vti_rets, vti_rets+abs(vti_rets))
colnames(vti_) <- c("rets", "sign")
reg_model <- lm(momentum_rets ~ vti_)
summary(reg_model)
# open x11 for plotting
x11(width=6, height=4)
# set plot parameters to reduce whitespace around plot
par(mar=c(4, 4, 3, 1), oma=c(0, 0, 0, 0))
# Treynor-Mazuy test
vti_ <- cbind(vti_rets, vti_rets^2)
colnames(vti_) <- c("rets", "squared")
reg_model <- lm(momentum_rets ~ vti_)
summary(reg_model)
# plot scatterplot
plot(x=vti_rets, y=momentum_rets,
     xlab="VTI", ylab="momentum")
title(main="Treynor-Mazuy market timing test\n for Momentum vs VTI", line=0.5)
# plot fitted (predicted) response values
points(x=vti_rets, y=reg_model$fitted.values,
 pch=16, col="red")
# Normalize the returns
momentum_rets <-
  (momentum_rets-mean(momentum_rets))
momentum_rets <-
  sd(vti_rets)*momentum_rets/sd(momentum_rets)
vti_rets <- (vti_rets-mean(vti_rets))
# calculate ratios of moments
sapply(2:4, FUN=moments::moment, x=vti_rets)/
  sapply(2:4, FUN=moments::moment, x=momentum_rets)
# plot histogram
x_lim <- 4*sd(momentum_rets)
hist(momentum_rets, breaks=30,
  main="Momentum and VTI Return Distributions",
  xlim=c(-x_lim, x_lim),
  xlab="", ylab="", freq=FALSE)
# draw kernel density of histogram
lines(density(momentum_rets), col='red', lwd=2)
lines(density(vti_rets), col='blue', lwd=2)
# add legend
legend("topright", inset=0.05, cex=0.8, title=NULL,
 leg=c("Momentum", "VTI"),
 lwd=6, bg="white", col=c("red", "blue"))
# load S&P500 constituent stock prices
load("C:/Develop/R/lecture_slides/data/sp500.RData")
price_s <- eapply(env_sp500, quantmod::Cl)
price_s <- rutils::do_call(cbind, price_s)
# carry forward and backward non-NA prices
price_s <- zoo::na.locf(price_s)
price_s <- zoo::na.locf(price_s, fromLast=TRUE)
colnames(price_s) <- sapply(colnames(price_s),
  function(col_name) strsplit(col_name, split="[.]")[[1]][1])
# calculate the percentage (log) returns of the S&P500 constituent stocks
re_turns <- rutils::diff_it(price_s)
da_ta <- rowSums(re_turns==0)
da_ta <- xts::xts(da_ta, order.by=index(price_s))
dygraphs::dygraph(da_ta, main="Number of S&P 500 Constituents Without Prices")
# select data after 2007
price_s <- price_s["2007/"]
re_turns <- re_turns["2007/"]
n_col <- NCOL(price_s)
save(price_s,
  file="C:/Develop/R/lecture_slides/data/sp500_prices.RData")
# calculate price weighted index of constituent
in_dex <- rowSums(price_s)/n_col
# rescale to VTI
in_dex <- as.numeric(env_etf$VTI[index(price_s[1]), 4])*in_dex/in_dex[1]
in_dex <- xts::xts(in_dex, order.by=index(price_s))
colnames(in_dex) <- "index"
da_ta <- cbind(in_dex, env_etf$VTI[index(price_s), 4])
col_names <- c("index", "VTI")
colnames(da_ta) <- col_names
# plot with VTI
dygraphs::dygraph(da_ta,
  main="S&P 500 Price-weighted Index and VTI") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(col_names[2], axis="y2", col=c("orange", "blue"))
# calculate rolling variance of S&P500 portfolio
wid_th <- 252
vari_ance <- roll::roll_var(re_turns, width=wid_th)
vari_ance <- zoo::na.locf(vari_ance)
vari_ance[is.na(vari_ance)] <- 0
# calculate rolling Sharpe of S&P500 portfolio
returns_width <- rutils::diff_it(price_s, lagg=wid_th)
weight_s <- returns_width/sqrt(wid_th*vari_ance)
weight_s[vari_ance==0] <- 0
weight_s[1:wid_th, ] <- 1
weight_s[is.na(weight_s)] <- 0
weight_s <- weight_s/rowSums(abs(weight_s))/price_s
weight_s[is.na(weight_s)] <- 0
weight_s <- rutils::lag_it(weight_s)
sum(is.na(weight_s))
# calculate portfolio profits and losses
pnl_s <- rowSums(weight_s*re_turns)
# Calculate transaction costs
bid_offer <- 0.001
cost_s <- 0.5*bid_offer*price_s*abs(rutils::diff_it(weight_s))
cost_s <- rowSums(cost_s)
pnl_s <- (pnl_s - cost_s)
pnl_s <- cumsum(pnl_s)
pnl_s <- xts(pnl_s, order.by=index(price_s))
pnl_s <- cbind(rutils::env_etf$VTI[, 4], pnl_s)
pnl_s <- na.omit(pnl_s)
colnames(pnl_s) <- c("VTI", "momentum")
col_names <- colnames(pnl_s)
# plot momentum and VTI
dygraphs::dygraph(pnl_s, main=paste(col_names, collapse=" and ")) %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(col_names[2], axis="y2", col=c("red", "blue"))
# define back-test functional
back_test_rolling <- function(re_turns, price_s,
    wid_th=252, bid_offer=0.001, tre_nd=1, ...) {
  stopifnot("package:quantmod" %in% search() || require("quantmod", quietly=TRUE))
  # Define look-back and look-forward intervals
  n_col <- NCOL(re_turns)
  vari_ance <- roll::roll_var(re_turns, width=wid_th)
  vari_ance <- zoo::na.locf(vari_ance)
  vari_ance[is.na(vari_ance)] <- 0
  # calculate rolling Sharpe of S&P500 portfolio
  returns_width <- rutils::diff_it(price_s, lagg=wid_th)
  weight_s <- tre_nd*returns_width/sqrt(wid_th*vari_ance)
  weight_s[vari_ance==0] <- 0
  weight_s[1:wid_th, ] <- 1
  weight_s[is.na(weight_s)] <- 0
  weight_s <- weight_s/rowSums(abs(weight_s))/price_s
  weight_s[is.na(weight_s)] <- 0
  weight_s <- rutils::lag_it(weight_s)
  # calculate portfolio profits and losses
  pnl_s <- rowSums(weight_s*re_turns)
  # Calculate transaction costs
  bid_offer <- 0.001
  cost_s <- 0.5*bid_offer*price_s*abs(rutils::diff_it(weight_s))
  cost_s <- rowSums(cost_s)
  pnl_s <- (pnl_s - cost_s)
  pnl_s <- cumsum(pnl_s)
  pnl_s
}  # end back_test_rolling
source("C:/Develop/R/lecture_slides/scripts/back_test.R")
pnl_s <- back_test_rolling(wid_th=252, re_turns=re_turns,
  price_s=price_s, bid_offer=bid_offer)
width_s <- seq(50, 300, by=50)
# perform sapply loop over lamb_das
pro_files <- sapply(width_s, back_test_rolling, re_turns=re_turns,
  price_s=price_s, bid_offer=bid_offer)
colnames(pro_files) <- paste0("width=", width_s)
pro_files <- xts(pro_files, index(price_s))
# plot EWMA strategies with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(pro_files))
chart_Series(pro_files,
  theme=plot_theme, name="Cumulative Returns of S&P500 Momentum Strategies")
legend("bottomleft", legend=colnames(pro_files),
  inset=0.0, bg="white", cex=0.7, lwd=rep(6, NCOL(re_turns)),
  col=plot_theme$col$line.col, bty="n")
width_s <- seq(5, 50, by=5)
# perform sapply loop over lamb_das
pro_files <- sapply(width_s, back_test_rolling, re_turns=re_turns,
  price_s=price_s, bid_offer=bid_offer, tre_nd=(-1))
colnames(pro_files) <- paste0("width=", width_s)
pro_files <- xts(pro_files, index(price_s))
# plot EWMA strategies with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(pro_files))
chart_Series(pro_files,
  theme=plot_theme, name="Cumulative Returns of S&P500 Mean-reverting Strategies")
legend("bottomleft", legend=colnames(pro_files),
  inset=0.0, bg="white", cex=0.7, lwd=rep(6, NCOL(re_turns)),
  col=plot_theme$col$line.col, bty="n")
# create random real symmetric matrix
mat_rix <- matrix(runif(25), nc=5)
mat_rix <- mat_rix + t(mat_rix)
# calculate eigenvectors and eigenvalues
ei_gen <- eigen(mat_rix)
eigen_vec <- ei_gen$vectors
dim(eigen_vec)
# plot eigenvalues
barplot(ei_gen$values,
  xlab="", ylab="", las=3,
  names.arg=paste0("ev", 1:NROW(ei_gen$values)),
  main="Eigenvalues of a real symmetric matrix")
# eigenvectors form an orthonormal basis
round(t(eigen_vec) %*% eigen_vec,
  digits=4)
# diagonalize matrix using eigenvector matrix
round(t(eigen_vec) %*% (mat_rix %*% eigen_vec),
  digits=4)
ei_gen$values
# eigen decomposition of matrix by rotating the diagonal matrix
eigen_decomp <- eigen_vec %*% (ei_gen$values * t(eigen_vec))
# create diagonal matrix of eigenvalues
# diago_nal <- diag(ei_gen$values)
# eigen_decomp <- eigen_vec %*% (diago_nal %*% t(eigen_vec))
all.equal(mat_rix, eigen_decomp)
# create random positive semi-definite matrix
mat_rix <- matrix(runif(25), nc=5)
mat_rix <- t(mat_rix) %*% mat_rix
# calculate eigenvectors and eigenvalues
ei_gen <- eigen(mat_rix)
ei_gen$values
# plot eigenvalues
barplot(ei_gen$values, las=3,
  xlab="", ylab="",
  names.arg=paste0("ev", 1:NROW(ei_gen$values)),
  main="Eigenvalues of positive semi-definite matrix")
# dimensions of left and right matrices
n_left <- 6 ; n_right <- 4
# create random positive semi-definite matrix
left_mat <- matrix(runif(n_left^2), nc=n_left)
left_mat <- crossprod(left_mat)
# or
left_mat <- left_mat %*% t(left_mat)
# calculate left eigenvectors
ei_gen <- eigen(left_mat)
left_mat <- ei_gen$vectors[, 1:n_right]
# create random positive semi-definite matrix
right_mat <- matrix(runif(n_right^2), nc=n_right)
right_mat <- crossprod(right_mat)
# or
right_mat <- right_mat %*% t(right_mat)
# calculate right eigenvectors and singular values
ei_gen <- eigen(right_mat)
right_mat <- ei_gen$vectors
sing_values <- ei_gen$values
# compose rectangular matrix
mat_rix <-
  left_mat %*% (sing_values * t(right_mat))
# mat_rix <- left_mat %*% diag(sing_values) %*% t(right_mat)
# perform singular value decomposition
s_vd <- svd(mat_rix)
# compare SVD with inputs
all.equal(abs(s_vd$u), abs(left_mat))
all.equal(abs(s_vd$v), abs(right_mat))
all.equal(s_vd$d, ei_gen$values)
# create random positive semi-definite matrix
mat_rix <- matrix(runif(25), nc=5)
mat_rix <- t(mat_rix) %*% mat_rix
# calculate the inverse of mat_rix
in_verse <- solve(a=mat_rix)
# multiply inverse with matrix
round(in_verse %*% mat_rix, 4)
round(mat_rix %*% in_verse, 4)

# calculate eigenvectors and eigenvalues
ei_gen <- eigen(mat_rix)
eigen_vec <- ei_gen$vectors

# perform eigen decomposition of inverse
eigen_inverse <-
  eigen_vec %*% (t(eigen_vec) / ei_gen$values)
all.equal(in_verse, eigen_inverse)
# decompose diagonal matrix with inverse of eigenvalues
# diago_nal <- diag(1/ei_gen$values)
# eigen_inverse <-
#   eigen_vec %*% (diago_nal %*% t(eigen_vec))
# create random rectangular matrix
# case when: n_left > n_right
n_left <- 6 ; n_right <- 4
mat_rix <- matrix(runif(n_left*n_right),
  nc=n_right)
# calculate generalized inverse of mat_rix
in_verse <- MASS::ginv(mat_rix)
round(in_verse %*% mat_rix, 4)
all.equal(mat_rix,
    mat_rix %*% in_verse %*% mat_rix)
# create random rectangular matrix
# case when: n_left < n_right
n_left <- 4 ; n_right <- 6
mat_rix <- matrix(runif(n_left*n_right),
  nc=n_right)
# calculate generalized inverse of mat_rix
in_verse <- MASS::ginv(mat_rix)
round(mat_rix %*% in_verse, 4)
# perform singular value decomposition
s_vd <- svd(mat_rix)
# calculate generalized inverse from SVD
svd_inverse <- s_vd$v %*% (t(s_vd$u) / s_vd$d)
all.equal(svd_inverse, in_verse)
# calculate Moore-Penrose pseudo-inverse
mp_inverse <-
  MASS::ginv(t(mat_rix) %*% mat_rix) %*% t(mat_rix)
all.equal(mp_inverse, in_verse)
# create random singular matrix
n_left <- 4 ; n_right <- 6
mat_rix <- matrix(runif(n_left*n_right), nc=n_right)
mat_rix <- t(mat_rix) %*% mat_rix
# calculate generalized inverse of mat_rix
in_verse <- MASS::ginv(mat_rix)
# verify inverse of mat_rix
all.equal(mat_rix,
  mat_rix %*% in_verse %*% mat_rix)
# perform singular value decomposition
s_vd <- svd(mat_rix)
# set tolerance for determining zero singular values
to_l <- sqrt(.Machine$double.eps)
# check for zero singular values
s_vd$d
not_zero <- (s_vd$d > (to_l * s_vd$d[1]))
# calculate generalized inverse from SVD
svd_inverse <-
  s_vd$v[, not_zero] %*%
  (t(s_vd$u[, not_zero]) / s_vd$d[not_zero])
all.equal(svd_inverse, in_verse)
# calculate Moore-Penrose pseudo-inverse
mp_inverse <-
  MASS::ginv(t(mat_rix) %*% mat_rix) %*% t(mat_rix)
all.equal(mp_inverse, in_verse)
# diagonalize the "unit" matrix
uni_t <- mat_rix %*% in_verse
round(uni_t, 4)
round(mat_rix %*% in_verse, 4)
round(t(s_vd$u) %*% uni_t %*% s_vd$v, 4)
# define a square matrix
mat_rix <- matrix(c(1, 2, -1, 2), nc=2)
vec_tor <- c(2, 1)
# calculate the inverse of mat_rix
in_verse <- solve(a=mat_rix)
in_verse %*% mat_rix
# calculate solution using inverse of mat_rix
solu_tion <- in_verse %*% vec_tor
mat_rix %*% solu_tion
# calculate solution of linear system
solu_tion <- solve(a=mat_rix, b=vec_tor)
mat_rix %*% solu_tion
# create large random positive semi-definite matrix
mat_rix <- matrix(runif(1e4), nc=100)
mat_rix <- t(mat_rix) %*% mat_rix
# calculate eigen decomposition
ei_gen <- eigen(mat_rix)
eigen_values <- ei_gen$values
eigen_vec <- ei_gen$vectors
# set tolerance for determining zero singular values
to_l <- sqrt(.Machine$double.eps)
# if needed convert to positive definite matrix
not_zero <- (eigen_values > (to_l * eigen_values[1]))
if (sum(!not_zero) > 0) {
  eigen_values[!not_zero] <- 2*to_l
  mat_rix <- eigen_vec %*%
    (eigen_values * t(eigen_vec))
}  # end if
# calculate the Cholesky mat_rix
choles_ky <- chol(mat_rix)
choles_ky[1:5, 1:5]
all.equal(mat_rix, t(choles_ky) %*% choles_ky)
# calculate inverse from Cholesky
chol_inverse <- chol2inv(choles_ky)
all.equal(solve(mat_rix), chol_inverse)
# compare speed of Cholesky inversion
library(microbenchmark)
summary(microbenchmark(
  sol_ve=solve(mat_rix),
  choles_ky=chol2inv(chol(mat_rix)),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# calculate random covariance matrix
cov_mat <- matrix(runif(25), nc=5)
cov_mat <- t(cov_mat) %*% cov_mat
# calculate the Cholesky mat_rix
choles_ky <- chol(cov_mat)
choles_ky
# simulate random uncorrelated returns
n_assets <- 5
n_rows <- 10000
re_turns <- matrix(rnorm(n_assets*n_rows), nc=n_assets)
# calculate correlated returns by applying Cholesky
corr_returns <- re_turns %*% choles_ky
# calculate covariance matrix
cov_returns <- crossprod(corr_returns) / (n_rows-1)
all.equal(cov_mat, cov_returns)
# simulate random portfolio returns
n_assets <- 10
n_rows <- 100
re_turns <- matrix(rnorm(n_assets*n_rows), nc=n_assets)
# de-mean the returns
re_turns <- apply(re_turns, MARGIN=2, function(x) (x-mean(x)))
# calculate covariance matrix
cov_mat <- crossprod(re_turns) / (n_rows-1)
# calculate eigenvectors and eigenvalues
ei_gen <- eigen(cov_mat)
ei_gen$values
barplot(ei_gen$values, # plot eigenvalues
  xlab="", ylab="", las=3,
  names.arg=paste0("ev", 1:NROW(ei_gen$values)),
  main="Eigenvalues of covariance matrix")
# calculate eigenvectors and eigenvalues
# as function of number of returns
n_data <- ((n_assets/2):(2*n_assets))
e_values <- sapply(n_data, function(x) {
  re_turns <- re_turns[1:x, ]
  re_turns <- apply(re_turns, MARGIN=2,
    function(y) (y-mean(y)))
  cov_mat <- crossprod(re_turns) / (x-1)
  min(eigen(cov_mat)$values)
})  # end sapply
plot(y=e_values, x=n_data, t="l",
  xlab="", ylab="", lwd=3, col="blue",
  main="Smallest eigenvalue of covariance matrix\nas function of number of returns")
library(RcppArmadillo)
# source Rcpp functions from file
Rcpp::sourceCpp(file="C:/Develop/R/lecture_slides/scripts/armadillo_functions.cpp")
vec1 <- runif(1e5)
vec2 <- runif(1e5)
vec_in(vec1, vec2)
vec1 %*% vec2
# microbenchmark RcppArmadillo code
summary(microbenchmark(
  vec_in=vec_in(vec1, vec2),
  r_code=(vec1 %*% vec2),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
# microbenchmark shows:
# vec_in() is several times faster than %*%, especially for longer vectors.
#     expr     mean   median
# 1 vec_in 110.7067 110.4530
# 2 r_code 585.5127 591.3575
library(RcppArmadillo)
# source Rcpp functions from file
Rcpp::sourceCpp(file="C:/Develop/R/lecture_slides/scripts/armadillo_functions.cpp")
mat_rix <- matrix(runif(1e5), nc=1e3)
# de-mean using apply()
new_mat <- apply(mat_rix, 2,
  function(x) (x-mean(x)))
# de-mean using demean_mat()
demean_mat(mat_rix)
all.equal(new_mat, mat_rix)
# microbenchmark RcppArmadillo code
summary(microbenchmark(
  demean_mat=demean_mat(mat_rix),
  apply=(apply(mat_rix, 2, mean)),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
# microbenchmark shows:
# demean_mat() is over 70 times faster than apply()
#         expr       mean   median
# 1 demean_mat   127.7539  125.604
# 2      apply 10781.7534 9291.674

# perform matrix inversion
# create random positive semi-definite matrix
mat_rix <- matrix(runif(25), nc=5)
mat_rix <- t(mat_rix) %*% mat_rix
# invert the matrix
matrix_inv <- solve(mat_rix)
inv_mat(mat_rix)
all.equal(inv_mat, mat_rix)
# microbenchmark RcppArmadillo code
library(microbenchmark)
summary(microbenchmark(
  inv_mat=inv_mat(mat_rix),
  solve=solve(mat_rix),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
# microbenchmark shows:
# inv_mat() is over 10 times faster than solve()
#      expr     mean median
# 1 inv_mat  3.42669  2.933
# 2 solve   32.00254 31.280
# formula of linear model with zero intercept
lin_formula <- z ~ x + y - 1
lin_formula

# collapse vector of strings into single text string
paste0("x", 1:5)
paste(paste0("x", 1:5), collapse="+")

# create formula from text string
lin_formula <- as.formula(
  # coerce text strings to formula
  paste("z ~ ",
  paste(paste0("x", 1:5), collapse="+")
  )  # end paste
)  # end as.formula
class(lin_formula)
lin_formula
# modify the formula using "update"
update(lin_formula, log(.) ~ . + beta)
set.seed(1121)  # initialize random number generator
# define explanatory variable
len_gth <- 100
ex_plain <- rnorm(len_gth, mean=2)
noise <- rnorm(len_gth)
# response equals linear form plus error terms
res_ponse <- -3 + ex_plain + noise
# calculate de-meaned explanatory and response vectors
explain_zm <- ex_plain - mean(ex_plain)
response_zm <- res_ponse - mean(res_ponse)
# solve for regression beta
be_ta <- sum(explain_zm*response_zm) / sum(explain_zm^2)
# solve for regression alpha
al_pha <- mean(res_ponse) - be_ta*mean(ex_plain)
# specify regression formula
reg_formula <- res_ponse ~ ex_plain
reg_model <- lm(reg_formula)  # perform regression
class(reg_model)  # regressions have class lm
attributes(reg_model)
eval(reg_model$call$formula)  # regression formula
reg_model$coeff  # regression coefficients
coef(reg_model)
c(al_pha, be_ta)
x11(width=6, height=5)  # open x11 for plotting
# set plot parameters to reduce whitespace around plot
par(mar=c(5, 5, 1, 1), oma=c(0, 0, 0, 0))
# plot scatterplot using formula
plot(reg_formula)
title(main="Simple Regression", line=-1)
# add regression line
abline(reg_model, lwd=2, col="red")
# plot fitted (predicted) response values
points(x=ex_plain, y=reg_model$fitted.values,
       pch=16, col="blue")
# sum of residuals = 0
sum(reg_model$residuals)
x11(width=6, height=5)  # open x11 for plotting
# set plot parameters to reduce whitespace around plot
par(mar=c(5, 5, 1, 1), oma=c(0, 0, 0, 0))
# extract residuals
resi_duals <- cbind(ex_plain, reg_model$residuals)
colnames(resi_duals) <- c("explanatory variable", "residuals")
# plot residuals
plot(resi_duals)
title(main="Residuals of the Linear Regression", line=-1)
abline(h=0, lwd=2, col="red")
reg_model_sum <- summary(reg_model)  # copy regression summary
reg_model_sum  # print the summary to console
attributes(reg_model_sum)$names  # get summary elements
reg_model_sum$coeff
reg_model_sum$r.squared
reg_model_sum$adj.r.squared
reg_model_sum$fstatistic
# standard error of beta
reg_model_sum$
  coefficients["ex_plain", "Std. Error"]
sd(reg_model_sum$residuals)/sd(ex_plain)/
  sqrt(unname(reg_model_sum$fstatistic[3]))
anova(reg_model)
set.seed(1121)  # initialize random number generator
# high noise compared to coefficient
res_ponse <- 3 + ex_plain + rnorm(30, sd=8)
reg_model <- lm(reg_formula)  # perform regression
# values of regression coefficients are not
# statistically significant
summary(reg_model)
par(oma=c(1, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=1.0, cex.axis=1.0, cex.main=1.0, cex.sub=1.0)
reg_stats <- function(std_dev) {  # noisy regression
  set.seed(1121)  # initialize number generator
# create explanatory and response variables
  ex_plain <- rnorm(100, mean=2)
  res_ponse <- 3 + 0.2*ex_plain +
    rnorm(NROW(ex_plain), sd=std_dev)
# specify regression formula
  reg_formula <- res_ponse ~ ex_plain
# perform regression and get summary
  reg_model_sum <- summary(lm(reg_formula))
# extract regression statistics
  with(reg_model_sum, c(pval=coefficients[2, 4],
   adj_rsquared=adj.r.squared,
   fstat=fstatistic[1]))
}  # end reg_stats
# apply reg_stats() to vector of std dev values
vec_sd <- seq(from=0.1, to=0.5, by=0.1)
names(vec_sd) <- paste0("sd=", vec_sd)
mat_stats <- t(sapply(vec_sd, reg_stats))
# plot in loop
par(mfrow=c(NCOL(mat_stats), 1))
for (in_dex in 1:NCOL(mat_stats)) {
  plot(mat_stats[, in_dex], type="l",
 xaxt="n", xlab="", ylab="", main="")
  title(main=colnames(mat_stats)[in_dex], line=-1.0)
  axis(1, at=1:(NROW(mat_stats)),
 labels=rownames(mat_stats))
}  # end for
reg_stats <- function(da_ta) {  # get regression
# perform regression and get summary
  col_names <- colnames(da_ta)
  reg_formula <-
    paste(col_names[2], col_names[1], sep="~")
  reg_model_sum <- summary(lm(reg_formula,
                        data=da_ta))
# extract regression statistics
  with(reg_model_sum, c(pval=coefficients[2, 4],
   adj_rsquared=adj.r.squared,
   fstat=fstatistic[1]))
}  # end reg_stats
# apply reg_stats() to vector of std dev values
vec_sd <- seq(from=0.1, to=0.5, by=0.1)
names(vec_sd) <- paste0("sd=", vec_sd)
mat_stats <-
  t(sapply(vec_sd, function (std_dev) {
    set.seed(1121)  # initialize number generator
# create explanatory and response variables
    ex_plain <- rnorm(100, mean=2)
    res_ponse <- 3 + 0.2*ex_plain +
rnorm(NROW(ex_plain), sd=std_dev)
    reg_stats(data.frame(ex_plain, res_ponse))
    }))
# plot in loop
par(mfrow=c(NCOL(mat_stats), 1))
for (in_dex in 1:NCOL(mat_stats)) {
  plot(mat_stats[, in_dex], type="l",
 xaxt="n", xlab="", ylab="", main="")
  title(main=colnames(mat_stats)[in_dex], line=-1.0)
  axis(1, at=1:(NROW(mat_stats)),
 labels=rownames(mat_stats))
}  # end for
# set plot paramaters - margins and font scale
par(oma=c(1,0,1,0), mgp=c(2,1,0), mar=c(2,1,2,1), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2, 2))  # plot 2x2 panels
plot(reg_model)  # plot diagnostic scatterplots
plot(reg_model, which=2)  # plot just Q-Q
library(lmtest)  # load lmtest
# perform Durbin-Watson test
dwtest(reg_model)
set.seed(1121)  # initialize random number generator
# define explanatory variable
len_gth <- 100
n_var <- 5
ex_plain <- matrix(rnorm(n_var*len_gth), nc=n_var)
noise <- rnorm(len_gth, sd=1.0)
# response equals linear form plus error terms
weight_s <- rnorm(n_var)
res_ponse <- -3 + ex_plain %*% weight_s + noise
# calculate de-meaned explanatory matrix
explain_zm <- t(t(ex_plain) - colMeans(ex_plain))
# explain_zm <- apply(explain_zm, 2, function(x) (x-mean(x)))
# calculate de-meaned response vector
response_zm <- res_ponse - mean(res_ponse)
# solve for regression betas
beta_s <- MASS::ginv(explain_zm) %*% response_zm
# solve for regression alpha
al_pha <- mean(res_ponse) - 
  sum(colSums(ex_plain)*drop(beta_s))/len_gth
# multivariate regression using lm()
reg_model <- lm(res_ponse ~ ex_plain)
coef(reg_model)
c(al_pha, beta_s)
c(-3, weight_s)
sum(reg_model$residuals * reg_model$fitted.values)
# calculate fitted values
fit_ted <- drop(al_pha + ex_plain %*% beta_s)
all.equal(fit_ted, reg_model$fitted.values, check.attributes=FALSE)
# calculate residuals
resid_uals <- drop(res_ponse - fit_ted)
all.equal(resid_uals, reg_model$residuals, check.attributes=FALSE)
# residuals are orthogonal to fitted values
sum(resid_uals * fit_ted)
# TSS = ESS + RSS
t_ss <- (len_gth-1)*var(drop(res_ponse))
e_ss <- (len_gth-1)*var(fit_ted)
r_ss <- (len_gth-1)*var(resid_uals)
all.equal(t_ss, e_ss + r_ss)
# regression summary
reg_model_sum <- summary(reg_model)
# regression R-squared
r_squared <- e_ss/t_ss
all.equal(r_squared, reg_model_sum$r.squared)
# correlation between response and fitted values
cor_fitted <- drop(cor(res_ponse, fit_ted))
# squared correlation between response and fitted values
all.equal(cor_fitted^2, r_squared)
x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
deg_free <- c(3, 5, 9)  # df values
col_ors <- c("black", "red", "blue", "green")
lab_els <- paste0("df1=", deg_free, ", df2=3")
for (in_dex in 1:NROW(deg_free)) {  # plot four curves
curve(expr=df(x, df1=deg_free[in_dex], df2=3),
      type="l", xlim=c(0, 4),
      xlab="", ylab="", lwd=2,
      col=col_ors[in_dex],
      add=as.logical(in_dex-1))
}  # end for
# add title
title(main="F-Distributions", line=0.5)
# add legend
legend("topright", inset=0.05, title="degrees of freedom",
       lab_els, cex=0.8, lwd=2, lty=1,
       col=col_ors)
# F-statistic from lm()
reg_model_sum$fstatistic
# degrees of freedom of residuals
deg_free <- len_gth-n_var-1
# F-statistic from RSS
f_stat <- e_ss*deg_free/r_ss/n_var
all.equal(f_stat, reg_model_sum$fstatistic[1], check.attributes=FALSE)
# p-value of F-statistic
1-pf(q=f_stat, df1=len_gth-n_var-1, df2=n_var)
# add intercept column to explanatory
ex_plain <- cbind(rep(1, NROW(ex_plain)), ex_plain)
# solve for regression betas
beta_s <- MASS::ginv(ex_plain) %*% res_ponse
all.equal(drop(beta_s), coef(reg_model), check.attributes=FALSE)
# calculate fitted values
fit_ted <- drop(ex_plain %*% beta_s)
all.equal(fit_ted, reg_model$fitted.values, check.attributes=FALSE)
# calculate residuals
resid_uals <- drop(res_ponse - fit_ted)
all.equal(resid_uals, reg_model$residuals, check.attributes=FALSE)
# degrees of freedom of residuals
deg_free <- len_gth-NCOL(ex_plain)
# variance of residuals
resid_var <- sum(resid_uals^2)/deg_free
# explanatory matrix squared
explain_squared <- crossprod(ex_plain)
# explain_squared <- t(ex_plain) %*% ex_plain
# calculate covariance matrix of betas
beta_covar <- resid_var*MASS::ginv(explain_squared)
beta_sd <- sqrt(diag(beta_covar))
all.equal(beta_sd, reg_model_sum$coeff[, 2], check.attributes=FALSE)
# calculate t-values of betas
beta_tvals <- drop(beta_s)/beta_sd
all.equal(beta_tvals, reg_model_sum$coeff[, 3], check.attributes=FALSE)
# calculate two-sided p-values of betas
beta_pvals <- 2*pt(-abs(beta_tvals), df=deg_free)
all.equal(beta_pvals, reg_model_sum$coeff[, 4], check.attributes=FALSE)
library(lmtest)  # load lmtest
de_sign <- data.frame(  # design matrix
  ex_plain=1:30, omit_var=sin(0.2*1:30))
# response depends on both explanatory variables
res_ponse <- with(de_sign,
  0.2*ex_plain + omit_var + 0.2*rnorm(30))
# mis-specified regression only one explanatory
reg_model <- lm(res_ponse ~ ex_plain,
        data=de_sign)
reg_model_sum <- summary(reg_model)
reg_model_sum$coeff
reg_model_sum$r.squared
# Durbin-Watson test shows residuals are autocorrelated
dwtest(reg_model)$p.value
par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # set plot panels
plot(reg_formula, data=de_sign)
abline(reg_model, lwd=2, col="red")
title(main="OVB Regression", line=-1)
plot(reg_model, which=2, ask=FALSE)  # plot just Q-Q
set.seed(1121)
library(lmtest)
# spurious regression in unit root time series
ex_plain <- cumsum(rnorm(100))  # unit root time series
res_ponse <- cumsum(rnorm(100))
reg_formula <- res_ponse ~ ex_plain
reg_model <- lm(reg_formula)  # perform regression
# summary indicates statistically significant regression
reg_model_sum <- summary(reg_model)
reg_model_sum$coeff
reg_model_sum$r.squared
# Durbin-Watson test shows residuals are autocorrelated
dw_test <- dwtest(reg_model)
c(dw_test$statistic[[1]], dw_test$p.value)
par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # set plot panels
plot(reg_formula, xlab="", ylab="")  # plot scatterplot using formula
title(main="Spurious Regression", line=-1)
# add regression line
abline(reg_model, lwd=2, col="red")
plot(reg_model, which=2, ask=FALSE)  # plot just Q-Q
ex_plain <- seq(from=0.1, to=3.0, by=0.1)  # explanatory variable
res_ponse <- 3 + 2*ex_plain + rnorm(30)
reg_formula <- res_ponse ~ ex_plain
reg_model <- lm(reg_formula)  # perform regression
new_data <- data.frame(ex_plain=0.1*31:40)
predict_lm <- predict(object=reg_model,
              newdata=new_data, level=0.95,
              interval="confidence")
predict_lm <- as.data.frame(predict_lm)
head(predict_lm, 2)
plot(reg_formula, xlim=c(1.0, 4.0),
     ylim=range(res_ponse, predict_lm),
     main="Regression predictions")
abline(reg_model, col="red")
with(predict_lm, {
  points(x=new_data$ex_plain, y=fit, pch=16, col="blue")
  lines(x=new_data$ex_plain, y=lwr, lwd=2, col="red")
  lines(x=new_data$ex_plain, y=upr, lwd=2, col="red")
})  # end with
