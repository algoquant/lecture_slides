library(rutils)
# Calculate a vector of daily VTI log returns
price_s <- log(quantmod::Cl(rutils::etf_env$VTI))
re_turns <- rutils::diff_it(price_s)
re_turns <- as.numeric(re_turns)
n_rows <- NROW(re_turns)
# Define predictor matrix for forecasting
order_max <- 10
predic_tor <- sapply(1:order_max, rutils::lag_it, in_put=re_turns)
predic_tor <- cbind(rep(1, n_rows), predic_tor)
colnames(predic_tor) <- paste0("pred_", 1:NCOL(predic_tor))
res_ponse <- re_turns
# Calculate forecasts as function of or_der
forecast_s <- lapply(2:NCOL(predic_tor), function(or_der) {
  # Calculate fitted coefficients
  in_verse <- MASS::ginv(predic_tor[, 1:or_der])
  coeff_fit <- drop(in_verse %*% res_ponse)
  # Calculate in-sample forecasts of re_turns
  drop(predic_tor[, 1:or_der] %*% coeff_fit)
})  # end lapply
names(forecast_s) <- paste0("p=", 2:NCOL(predic_tor))

back_tests <- sapply(forecast_s, function(x) {
  c(mse=mean((re_turns - x)^2), cor=cor(re_turns, x))
})  # end sapply
back_tests <- t(back_tests)
rownames(back_tests) <- names(forecast_s)
# Plot forecasting series with legend
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
plot(x=2:NCOL(predic_tor), y=back_tests[, 1],
  xlab="AR(p) order", ylab="MSE", type="l", lwd=2,
  main="MSE of In-sample AR(p) Forecasting Model for VTI")

in_sample <- 1:(n_rows %/% 2)
out_of_sample <- (n_rows %/% 2 + 1):n_rows
# Calculate forecasts as function of or_der
forecast_s <- lapply(2:NCOL(predic_tor), function(or_der) {
  # Calculate fitted coefficients
  in_verse <- MASS::ginv(predic_tor[in_sample, 1:or_der])
  coeff_fit <- drop(in_verse %*% res_ponse[in_sample])
  # Calculate out-of-sample forecasts of re_turns
  drop(predic_tor[out_of_sample, 1:or_der] %*% coeff_fit)
})  # end lapply
names(forecast_s) <- paste0("p=", 2:NCOL(predic_tor))

back_tests <- sapply(forecast_s, function(x) {
  c(mse=mean((re_turns[out_of_sample] - x)^2), cor=cor(re_turns[out_of_sample], x))
})  # end sapply
back_tests <- t(back_tests)
rownames(back_tests) <- names(forecast_s)
# Plot forecasting series with legend
plot(x=2:NCOL(predic_tor), y=back_tests[, 1],
  xlab="AR(p) order", ylab="MSE", type="l", lwd=2,
  main="MSE of Out-of-sample AR(p) Forecasting Model for VTI")

# Define predictor matrix for forecasting
order_max <- 5  # Define maximum order parameter
predic_tor <- sapply(1:order_max, rutils::lag_it, in_put=re_turns)
predic_tor <- cbind(rep(1, n_rows), predic_tor)
colnames(predic_tor) <- paste0("pred_", 1:NCOL(predic_tor))
res_ponse <- re_turns
# Calculate forecasts as function of or_der
forecast_s <- lapply(2:NCOL(predic_tor), function(or_der) {
  # Calculate fitted coefficients
  in_verse <- MASS::ginv(predic_tor[in_sample, 1:or_der])
  coeff_fit <- drop(in_verse %*% res_ponse[in_sample])
  # Calculate out-of-sample forecasts of re_turns
  drop(predic_tor[out_of_sample, 1:or_der] %*% coeff_fit)
})  # end lapply
names(forecast_s) <- paste0("p=", 2:NCOL(predic_tor))

# Calculate out-of-sample PnLs
pnl_s <- sapply(forecast_s, function(x) {
  cumsum(sign(x)*re_turns[out_of_sample])
})  # end sapply
colnames(pnl_s) <- names(forecast_s)
pnl_s <- xts::xts(pnl_s, index(price_s[out_of_sample]))
# Create dygraph object
dy_graph <- dygraphs::dygraph(pnl_s,
  main="Autoregressive Strategies Performance With Different Order Parameters")
# Add time series
color_s <- colorRampPalette(c("blue", "red"))(NCOL(pnl_s))
col_names <- colnames(pnl_s)
for (i in 1:NROW(col_names)) {
  dy_graph <- dy_graph %>%
    dySeries(label=col_names[i], col=color_s[i], strokeWidth=2)
}  # end for
# Add legend
dy_graph <- dy_graph %>% dyLegend(width=500)
# Render dygraph
dy_graph

# Define predictor as a rolling sum
look_back <- 5
predic_tor <- rutils::roll_sum(re_turns, look_back=look_back)
# Shift the res_ponse forward into out-of-sample
res_ponse <- rutils::lag_it(predic_tor, lagg=(-look_back))
# Define predictor matrix for forecasting
predic_tor <- sapply(1+look_back*(0:order_max), rutils::lag_it,
               in_put=predic_tor)
predic_tor <- cbind(rep(1, n_rows), predic_tor)
colnames(predic_tor) <- paste0("pred_", 1:NCOL(predic_tor))
# Calculate forecasts as function of or_der
forecast_s <- lapply(2:NCOL(predic_tor), function(or_der) {
  # Calculate fitted coefficients
  in_verse <- MASS::ginv(predic_tor[in_sample, 1:or_der])
  coeff_fit <- drop(in_verse %*% res_ponse[in_sample])
  # Calculate out-of-sample forecasts of re_turns
  drop(predic_tor[out_of_sample, 1:or_der] %*% coeff_fit)
})  # end lapply
names(forecast_s) <- paste0("p=", 2:NCOL(predic_tor))

# Calculate out-of-sample PnLs
pnl_s <- sapply(forecast_s, function(x) {
  cumsum(sign(x)*re_turns[out_of_sample])
})  # end sapply
colnames(pnl_s) <- names(forecast_s)
pnl_s <- xts::xts(pnl_s, index(price_s[out_of_sample]))
# Create dygraph object
dy_graph <- dygraphs::dygraph(pnl_s,
  main="Autoregressive Strategies With Rolling Sum Predictor")
# Add time series
color_s <- colorRampPalette(c("blue", "red"))(NCOL(pnl_s))
col_names <- colnames(pnl_s)
for (i in 1:NROW(col_names)) {
  dy_graph <- dy_graph %>%
    dySeries(label=col_names[i], col=color_s[i], strokeWidth=2)
}  # end for
# Add legend
dy_graph <- dy_graph %>% dyLegend(width=500)
# Render dygraph
dy_graph

library(rutils)
# Select all the ETF symbols except "VXX", "SVXY" and "MTUM"
sym_bols <- colnames(rutils::etf_env$re_turns)
sym_bols <- sym_bols[!(sym_bols %in% c("VXX", "SVXY", "MTUM"))]
# Extract columns of rutils::etf_env$re_turns and overwrite NA values
re_turns <- rutils::etf_env$re_turns[, sym_bols]
n_assets <- NCOL(re_turns)
# re_turns <- na.omit(re_turns)
re_turns[1, is.na(re_turns[1, ])] <- 0
re_turns <- zoo::na.locf(re_turns, na.rm=FALSE)
# Returns in excess of risk-free rate
risk_free <- 0.03/252
ex_cess <- (re_turns - risk_free)
# Maximum Sharpe weights in-sample interval
in_verse <- MASS::ginv(cov(re_turns["/2014"]))
weight_s <- in_verse %*% colMeans(ex_cess["/2014"])
weight_s <- drop(weight_s/sqrt(sum(weight_s^2)))
names(weight_s) <- colnames(re_turns)
# Plot portfolio weights
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
barplot(sort(weight_s), main="Maximum Sharpe Weights", cex.names=0.7)
# Calculate portfolio returns
rets_is <- re_turns["/2014"]
portf_is <- xts(rets_is %*% weight_s, index(rets_is))
in_dex <- xts(rowSums(rets_is)/sqrt(n_assets), index(rets_is))
portf_is <- portf_is*sd(in_dex)/sd(portf_is)
# Plot cumulative portfolio returns
weal_th <- cumsum(cbind(portf_is, in_dex))
colnames(weal_th) <- c("Optimal Portfolio", "Equal Weight Portfolio")
dygraphs::dygraph(weal_th, main="In-sample Optimal Portfolio Returns") %>%
  dyOptions(colors=c("red","blue"), strokeWidth=2) %>%
  dyLegend(width=500)

# Out-of-sample portfolio returns
rets_os <- re_turns["2015/"]
portf_os <- xts(rets_os %*% weight_s, index(rets_os))
in_dex <- xts(rowSums(rets_os)/sqrt(n_assets), index(rets_os))
portf_os <- portf_os*sd(in_dex)/sd(portf_os)

# Plot cumulative portfolio returns
weal_th <- cumsum(cbind(portf_os, in_dex))
colnames(weal_th) <- c("Optimal Portfolio", "Equal Weight Portfolio")
dygraphs::dygraph(weal_th, main="Out-of-sample Optimal Portfolio Returns") %>%
  dyOptions(colors=c("red","blue"), strokeWidth=2) %>%
  dyLegend(width=500)

load("C:/Develop/lecture_slides/data/sp500_returns.RData")
# Overwrite NA values in re_turns
re_turns <- re_turns["2000/"]
n_assets <- NCOL(re_turns)
re_turns[1, is.na(re_turns[1, ])] <- 0
re_turns <- zoo::na.locf(re_turns, na.rm=FALSE)
risk_free <- 0.03/252
ex_cess <- (re_turns - risk_free)
rets_is <- re_turns["/2010"]
rets_os <- re_turns["2011/"]
# Maximum Sharpe weights in-sample interval
cov_mat <- cov(rets_is)
in_verse <- MASS::ginv(cov_mat)
weight_s <- in_verse %*% colMeans(ex_cess["/2010"])
weight_s <- drop(weight_s/sqrt(sum(weight_s^2)))
names(weight_s) <- colnames(re_turns)
# Calculate portfolio returns
portf_is <- xts(rets_is %*% weight_s, index(rets_is))
portf_os <- xts(rets_os %*% weight_s, index(rets_os))
in_dex <- xts(rowSums(re_turns)/sqrt(n_assets), index(re_turns))

# Plot cumulative portfolio returns
weal_th <- rbind(portf_is, portf_os)
weal_th <- weal_th*sd(in_dex)/sd(weal_th)
weal_th <- cumsum(cbind(weal_th, in_dex))
colnames(weal_th) <- c("Optimal Portfolio", "Equal Weight Portfolio")
dygraphs::dygraph(weal_th, main="Out-of-sample Optimal Portfolio Returns for Stocks") %>%
  dyOptions(colors=c("red","blue"), strokeWidth=2) %>%
  dyEvent(index(last(rets_is[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=500)

# Create rectangular matrix with collinear columns
ran_dom <- matrix(rnorm(10*8), nc=10)
# Calculate covariance matrix
cov_mat <- cov(ran_dom)
# Calculate inverse of cov_mat - error
in_verse <- solve(cov_mat)
# Calculate regularized inverse of cov_mat
in_verse <- MASS::ginv(cov_mat)
# Verify inverse property of mat_rix
all.equal(cov_mat, cov_mat %*% in_verse %*% cov_mat)
# Perform eigen decomposition
ei_gen <- eigen(cov_mat)
eigen_vec <- ei_gen$vectors
eigen_val <- ei_gen$values
# Set tolerance for determining zero singular values
to_l <- sqrt(.Machine$double.eps)
# Calculate regularized inverse matrix
not_zero <- (eigen_val > (to_l * eigen_val[1]))
reg_inverse <- eigen_vec[, not_zero] %*%
  (t(eigen_vec[, not_zero]) / eigen_val[not_zero])
# Verify inverse property of mat_rix
all.equal(in_verse, reg_inverse)

# Calculate in-sample covariance matrix
cov_mat <- cov(rets_is)
ei_gen <- eigen(cov_mat)
eigen_vec <- ei_gen$vectors
eigen_val <- ei_gen$values
# Calculate regularized inverse of covariance matrix
max_eigen <- 21
in_verse <- eigen_vec[, 1:max_eigen] %*%
  (t(eigen_vec[, 1:max_eigen]) / ei_gen$values[1:max_eigen])

# Calculate portfolio weights
weight_s <- in_verse %*% colMeans(ex_cess["/2010"])
weight_s <- drop(weight_s/sqrt(sum(weight_s^2)))
names(weight_s) <- colnames(re_turns)
# Calculate portfolio returns
portf_is <- xts(rets_is %*% weight_s, index(rets_is))
portf_os <- xts(rets_os %*% weight_s, index(rets_os))
in_dex <- xts(rowSums(re_turns)/sqrt(n_assets), index(re_turns))

# Plot cumulative portfolio returns
weal_th <- rbind(portf_is, portf_os)
weal_th <- weal_th*sd(in_dex)/sd(weal_th)
weal_th <- cumsum(cbind(weal_th, in_dex))
colnames(weal_th) <- c("Optimal Portfolio", "Equal Weight Portfolio")
dygraphs::dygraph(weal_th, main="Regularized Out-of-sample Optimal Portfolio Returns for Stocks") %>%
  dyOptions(colors=c("red","blue"), strokeWidth=2) %>%
  dyEvent(index(last(rets_is[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=500)

# Shrink the in-sample returns to their mean
rets_mean <- colMeans(rets_is) - risk_free
al_pha <- 0.7
rets_mean <- (1 - al_pha)*rets_mean + al_pha*mean(rets_mean)

# Calculate portfolio weights
weight_s <- in_verse %*% rets_mean
weight_s <- drop(weight_s/sqrt(sum(weight_s^2)))
# Calculate portfolio returns
portf_is <- xts(rets_is %*% weight_s, index(rets_is))
portf_os <- xts(rets_os %*% weight_s, index(rets_os))
# Plot cumulative portfolio returns
weal_th <- rbind(portf_is, portf_os)
weal_th <- weal_th*sd(in_dex)/sd(weal_th)
weal_th <- cumsum(cbind(weal_th, in_dex))
colnames(weal_th) <- c("Optimal Portfolio", "Equal Weight Portfolio")
dygraphs::dygraph(weal_th, main="Out-of-sample Returns for Stocks With Regularization and Shrinkage") %>%
  dyOptions(colors=c("red","blue"), strokeWidth=2) %>%
  dyEvent(index(last(rets_is[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=500)

library(RcppArmadillo)
# Source Rcpp functions from file
Rcpp::sourceCpp("C:/Develop/lecture_slides/scripts/calc_weights.cpp")
# Create random matrix of returns
mat_rix <- matrix(rnorm(300), nc=5)
# Regularized inverse of covariance matrix
max_eigen <- 4
ei_gen <- eigen(cov(mat_rix))
cov_inv <- ei_gen$vectors[, 1:max_eigen] %*%
  (t(ei_gen$vectors[, 1:max_eigen]) / ei_gen$values[1:max_eigen])
# Regularized inverse using RcppArmadillo
cov_inv_arma <- calc_inv(mat_rix, max_eigen)
all.equal(cov_inv, cov_inv_arma)
# Microbenchmark RcppArmadillo code
library(microbenchmark)
summary(microbenchmark(
  pure_r={
    ei_gen <- eigen(cov(mat_rix))
    ei_gen$vectors[, 1:max_eigen] %*%
(t(ei_gen$vectors[, 1:max_eigen]) / ei_gen$values[1:max_eigen])
  },
  r_cpp=calc_inv(mat_rix, max_eigen),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary

# Calculate vector of monthly end points and start points
end_p <- rutils::calc_endpoints(re_turns, inter_val="months")
end_p <- end_p[end_p > 2*NCOL(re_turns)]
n_rows <- NROW(end_p)
look_back <- 24
start_p <- c(rep_len(0, look_back-1),
       end_p[1:(n_rows-look_back+1)])

# Perform loop over end points
rets_portf <- lapply(2:n_rows, function(i) {
    # Subset the ex_cess returns
    ex_cess <- ex_cess[start_p[i-1]:end_p[i-1], ]
    in_verse <- MASS::ginv(cov(ex_cess))
    # Calculate the maximum Sharpe ratio portfolio weights
    weight_s <- in_verse %*% colMeans(ex_cess)
    weight_s <- drop(weight_s/sqrt(sum(weight_s^2)))
    # Calculate the out-of-sample portfolio returns
    re_turns <- re_turns[(end_p[i-1]+1):end_p[i], ]
    xts(re_turns %*% weight_s, index(re_turns))
})  # end lapply
rets_portf <- rutils::do_call(rbind, rets_portf)
# Plot cumulative strategy returns
in_dex <- xts(rowSums(re_turns)/sqrt(n_assets), index(re_turns))
weal_th <- cumsum(na.omit(cbind(rets_portf, in_dex*sd(rets_portf)/sd(in_dex))))
colnames(weal_th) <- c("Rolling Portfolio Strategy", "Equal Weight Portfolio")
dygraphs::dygraph(weal_th, main="Rolling Portfolio Optimization Strategy") %>%
  dyOptions(colors=c("red","blue"), strokeWidth=2) %>%
  dyLegend(show="always")

load("C:/Develop/lecture_slides/data/sp500_returns.RData")
# Overwrite NA values in re_turns
returns_100[1, is.na(returns_100[1, ])] <- 0
returns_100 <- zoo::na.locf(returns_100, na.rm=FALSE)
n_cols <- NCOL(returns_100) ; date_s <- index(returns_100)
# Define monthly end points
end_p <- rutils::calc_endpoints(returns_100, inter_val="months")
end_p <- end_p[end_p > (n_cols+1)]
n_rows <- NROW(end_p) ; look_back <- 12
start_p <- c(rep_len(0, look_back-1), end_p[1:(n_rows-look_back+1)])
end_p <- (end_p - 1)
start_p <- (start_p - 1)
start_p[start_p < 0] <- 0
al_pha <- 0.7 ; max_eigen <- 21

# Perform backtest in Rcpp
pnl_s <- HighFreq::back_test(typ_e="max_sharpe",
  ex_cess=returns_100, re_turns=returns_100,
  start_p=start_p, end_p=end_p,
  al_pha=al_pha, max_eigen=max_eigen)
# Calculate returns on equal weight portfolio
in_dex <- xts(rowMeans(returns_100), index(returns_100))
# Plot cumulative strategy returns
weal_th <- cbind(pnl_s, in_dex, (pnl_s+in_dex)/2)
weal_th <- cumsum(na.omit(weal_th))
col_names <- c("Strategy", "Index", "Average")
colnames(weal_th) <- col_names
dygraphs::dygraph(weal_th[end_p], main="Rolling S&P500 Portfolio Optimization Strategy") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="red", strokeWidth=1) %>%
  dySeries(name=col_names[2], axis="y2", col="blue", strokeWidth=1) %>%
  dySeries(name=col_names[3], axis="y2", col="green", strokeWidth=2)

# Perform backtest over alphas
alpha_s <- seq(from=0.01, to=0.91, by=0.1)
pnl_s <- lapply(alpha_s, function(al_pha) {
  HighFreq::back_test(typ_e="max_sharpe",
  ex_cess=returns_100, re_turns=returns_100,
  start_p=start_p, end_p=end_p,
  al_pha=al_pha, max_eigen=max_eigen)
})  # end lapply
pro_files <- sapply(pnl_s, sum)
plot(x=alpha_s, y=pro_files, t="l", main="Strategy PnL as Function of Shrinkage Intensity Alpha",
  xlab="Shrinkage Intensity Alpha", ylab="pnl")
al_pha <- alpha_s[which.max(pro_files)]
pnl_s <- pnl_s[[which.max(pro_files)]]
# Perform backtest over max_eigens
max_eigens <- seq(from=3, to=40, by=2)
pnl_s <- lapply(max_eigens, function(max_eigen) {
  HighFreq::back_test(typ_e="max_sharpe",
    ex_cess=returns_100, re_turns=returns_100,
    start_p=start_p, end_p=end_p,
    al_pha=al_pha, max_eigen=max_eigen)
})  # end lapply
pro_files <- sapply(pnl_s, sum)
plot(x=max_eigens, y=pro_files, t="l", main="Strategy PnL as Function of Max_eigen",
  xlab="Max_eigen", ylab="pnl")
max_eigen <- max_eigens[which.max(pro_files)]
pnl_s <- pnl_s[[which.max(pro_files)]]

# Plot cumulative strategy returns
weal_th <- cbind(pnl_s, in_dex, (pnl_s+in_dex)/2)
weal_th <- cumsum(na.omit(weal_th))
col_names <- c("Strategy", "Index", "Average")
colnames(weal_th) <- col_names
dygraphs::dygraph(weal_th[end_p], main="Optimal Rolling S&P500 Portfolio Strategy") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="red", strokeWidth=1) %>%
  dySeries(name=col_names[2], axis="y2", col="blue", strokeWidth=1) %>%
  dySeries(name=col_names[3], axis="y2", col="green", strokeWidth=2)

# Perform backtest over look-backs
look_backs <- seq(from=3, to=24, by=1)
pnl_s <- lapply(look_backs, function(look_back) {
  start_p <- c(rep_len(0, look_back-1), end_p[1:(n_rows-look_back+1)])
  start_p <- (start_p - 1)
  start_p[start_p < 0] <- 0
  HighFreq::back_test(typ_e="max_sharpe",
    ex_cess=returns_100, re_turns=returns_100,
    start_p=start_p, end_p=end_p,
    al_pha=al_pha, max_eigen=max_eigen)
})  # end lapply
pro_files <- sapply(pnl_s, sum)
plot(x=look_backs, y=pro_files, t="l", main="Strategy PnL as Function of Look-back Interval",
  xlab="Look-back Interval", ylab="pnl")
look_back <- look_backs[which.max(pro_files)]
pnl_s <- pnl_s[[which.max(pro_files)]]

# Plot cumulative strategy returns
weal_th <- cbind(pnl_s, in_dex, (pnl_s+in_dex)/2)
weal_th <- cumsum(na.omit(weal_th))
col_names <- c("Strategy", "Index", "Average")
colnames(weal_th) <- col_names
dygraphs::dygraph(weal_th[end_p], main="Optimal Rolling S&P500 Portfolio Strategy") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="red", strokeWidth=1) %>%
  dySeries(name=col_names[2], axis="y2", col="blue", strokeWidth=1) %>%
  dySeries(name=col_names[3], axis="y2", col="green", strokeWidth=2)

# Verify that rtools are working properly:
devtools::find_rtools()
devtools::has_devel()

# Load package Rcpp
library(Rcpp)
# Get documentation for package Rcpp
# Get short description
packageDescription("Rcpp")
# Load help page
help(package="Rcpp")
# list all datasets in "Rcpp"
data(package="Rcpp")
# list all objects in "Rcpp"
ls("package:Rcpp")
# Remove Rcpp from search path
detach("package:Rcpp")

# Define Rcpp function
Rcpp::cppFunction("
  int times_two(int x)
    { return 2 * x;}
  ")  # end cppFunction
# Run Rcpp function
times_two(3)
# Source Rcpp functions from file
Rcpp::sourceCpp(file="C:/Develop/lecture_slides/scripts/mult_rcpp.cpp")
# Multiply two numbers
mult_rcpp(2, 3)
mult_rcpp(1:3, 6:4)
# Multiply two vectors
mult_vec_rcpp(2, 3)
mult_vec_rcpp(1:3, 6:4)

# Define Rcpp function with loop
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
# Run Rcpp function
inner_mult(1:3, 6:4)
inner_mult(1:3, 6:3)
# Define Rcpp Sugar function with loop
Rcpp::cppFunction("
double inner_mult_sugar(NumericVector x, NumericVector y) {
  return sum(x * y);
}")  # end cppFunction
# Run Rcpp Sugar function
inner_mult_sugar(1:3, 6:4)
inner_mult_sugar(1:3, 6:3)

# Define R function with loop
inner_mult_r <- function(x, y) {
    to_tal <- 0
    for(i in 1:NROW(x)) {
to_tal <- to_tal + x[i] * y[i]
    }
    to_tal
}  # end inner_mult_r
# Run R function
inner_mult_r(1:3, 6:4)
inner_mult_r(1:3, 6:3)
# Compare speed of Rcpp and R
library(microbenchmark)
summary(microbenchmark(
  pure_r=inner_mult_r(1:10000, 1:10000),
  inner_r=1:10000 %*% 1:10000,
  r_cpp=inner_mult(1:10000, 1:10000),
  r_cpp_sugar=inner_mult_sugar(1:10000, 1:10000),
  times=10))[, c(1, 4, 5)]

# Define Ornstein-Uhlenbeck function in R
sim_ou <- function(n_rows=1000, eq_price=5.0,
              vol_at=0.01, the_ta=0.01) {
  re_turns <- numeric(n_rows)
  price_s <- numeric(n_rows)
  price_s[1] <- eq_price
  for (i in 2:n_rows) {
    re_turns[i] <- the_ta*(eq_price - price_s[i-1]) + vol_at*rnorm(1)
    price_s[i] <- price_s[i-1] * exp(re_turns[i])
  }  # end for
  price_s
}  # end sim_ou
# Simulate Ornstein-Uhlenbeck process in R
eq_price <- 5.0; sig_ma <- 0.01
the_ta <- 0.01; n_rows <- 1000
set.seed(1121)  # Reset random numbers
ou_sim <- sim_ou(n_rows=n_rows, eq_price=eq_price, vol_at=sig_ma, the_ta=the_ta)

# Define Ornstein-Uhlenbeck function in Rcpp
Rcpp::cppFunction("
NumericVector sim_ou_rcpp(double eq_price,
                double vol_at,
                double the_ta,
                NumericVector in_nov) {
  int n_rows = in_nov.size();
  NumericVector price_s(n_rows);
  NumericVector re_turns(n_rows);
  price_s[0] = eq_price;
  for (int it = 1; it < n_rows; it++) {
    re_turns[it] = the_ta*(eq_price - price_s[it-1]) + vol_at*in_nov[it-1];
    price_s[it] = price_s[it-1] * exp(re_turns[it]);
  }  // end for
  return price_s;
}")  # end cppFunction
# Simulate Ornstein-Uhlenbeck process in Rcpp
set.seed(1121)  # Reset random numbers
ou_sim_rcpp <- sim_ou_rcpp(eq_price=eq_price,
  vol_at=sig_ma,
  the_ta=the_ta,
  in_nov=rnorm(n_rows))
all.equal(ou_sim, ou_sim_rcpp)
# Compare speed of Rcpp and R
library(microbenchmark)
summary(microbenchmark(
  pure_r=sim_ou(n_rows=n_rows, eq_price=eq_price, vol_at=sig_ma, the_ta=the_ta),
  r_cpp=sim_ou_rcpp(eq_price=eq_price, vol_at=sig_ma, the_ta=the_ta, in_nov=rnorm(n_rows)),
  times=10))[, c(1, 4, 5)]

# Source Rcpp function for Ornstein-Uhlenbeck process from file
Rcpp::sourceCpp(file="C:/Develop/lecture_slides/scripts/sim_ou.cpp")
# Simulate Ornstein-Uhlenbeck process in Rcpp
set.seed(1121)  # Reset random numbers
ou_sim_rcpp <- sim_ou_rcpp(eq_price=eq_price,
  vol_at=sig_ma,
  the_ta=the_ta,
  in_nov=rnorm(n_rows))
all.equal(ou_sim, ou_sim_rcpp)
# Compare speed of Rcpp and R
library(microbenchmark)
summary(microbenchmark(
  pure_r=sim_ou(n_rows=n_rows, eq_price=eq_price, vol_at=sig_ma, the_ta=the_ta),
  r_cpp=sim_ou_rcpp(eq_price=eq_price, vol_at=sig_ma, the_ta=the_ta, in_nov=rnorm(n_rows)),
  times=10))[, c(1, 4, 5)]

# Calculate uniformly distributed pseudo-random sequence
uni_form <- function(see_d, n_rows=10) {
  out_put <- numeric(n_rows)
  out_put[1] <- see_d
  for (i in 2:n_rows) {
    out_put[i] <- 4*out_put[i-1]*(1-out_put[i-1])
  }  # end for
  acos(1-2*out_put)/pi
}  # end uni_form

# Source Rcpp functions from file
Rcpp::sourceCpp(file="C:/Develop/lecture_slides/scripts/uni_form.cpp")
# Microbenchmark Rcpp code
library(microbenchmark)
summary(microbenchmark(
  pure_r=runif(1e5),
  r_loop=uni_form(0.3, 1e5),
  r_cpp=uniform_rcpp(0.3, 1e5),
  times=10))[, c(1, 4, 5)]

library(RcppArmadillo)
# Source Rcpp functions from file
Rcpp::sourceCpp(file="C:/Develop/R/Rcpp/armadillo_functions.cpp")
vec1 <- runif(1e5)
vec2 <- runif(1e5)
vec_in(vec1, vec2)
vec1 %*% vec2

# Microbenchmark RcppArmadillo code
summary(microbenchmark(
  vec_in = vec_in(vec1, vec2),
  r_code = (vec1 %*% vec2),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
# Microbenchmark shows:
# vec_in() is several times faster than %*%, especially for longer vectors.
#     expr     mean   median
# 1 vec_in 110.7067 110.4530
# 2 r_code 585.5127 591.3575

# Source Rcpp functions from file
Rcpp::sourceCpp(file="C:/Develop/lecture_slides/scripts/sim_arima.cpp")
# Define AR(2) coefficients
co_eff <- c(0.9, 0.09)
n_rows <- 1e4
set.seed(1121)
in_nov <- rnorm(n_rows)
# Simulate ARIMA using filter()
arima_filter <- filter(x=in_nov,
  filter=co_eff, method="recursive")
# Simulate ARIMA using sim_arima()
ari_ma <- sim_arima(in_nov, rev(co_eff))
all.equal(drop(ari_ma),
  as.numeric(arima_filter))
# Microbenchmark RcppArmadillo code
summary(microbenchmark(
  sim_arima = sim_arima(in_nov, rev(co_eff)),
  filter = filter(x=in_nov, filter=co_eff, method="recursive"),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary

library(RcppArmadillo)
# Source Rcpp functions from file
Rcpp::sourceCpp(file="C:/Develop/R/Rcpp/armadillo_functions.cpp")
mat_rix <- matrix(runif(1e5), nc=1e3)
# De-mean using apply()
new_mat <- apply(mat_rix, 2,
  function(x) (x-mean(x)))
# De-mean using demean_mat()
demean_mat(mat_rix)
all.equal(new_mat, mat_rix)
# Microbenchmark RcppArmadillo code
summary(microbenchmark(
  apply = (apply(mat_rix, 2, mean)),
  demean_mat = demean_mat(mat_rix),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
# Microbenchmark shows:
# Demean_mat() is over 70 times faster than apply()
#         expr       mean   median
# 1 demean_mat   127.7539  125.604
# 2      apply 10781.7534 9291.674

# Perform matrix inversion
# Create random positive semi-definite matrix
mat_rix <- matrix(runif(25), nc=5)
mat_rix <- t(mat_rix) %*% mat_rix
# Invert the matrix
matrix_inv <- solve(mat_rix)
inv_mat(mat_rix)
all.equal(inv_mat, mat_rix)
# Microbenchmark RcppArmadillo code
library(microbenchmark)
summary(microbenchmark(
  solve = solve(mat_rix),
  inv_mat = inv_mat(mat_rix),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
# Microbenchmark shows:
# inv_mat() is over 10 times faster than solve()
#      expr     mean median
# 1 inv_mat  3.42669  2.933
# 2 solve   32.00254 31.280

library(RcppArmadillo)
# Source Rcpp functions from file
Rcpp::sourceCpp("C:/Develop/lecture_slides/scripts/calc_weights.cpp")
# Create random matrix of returns
mat_rix <- matrix(rnorm(300), nc=5)
# Regularized inverse of covariance matrix
max_eigen <- 4
ei_gen <- eigen(cov(mat_rix))
cov_inv <- ei_gen$vectors[, 1:max_eigen] %*%
  (t(ei_gen$vectors[, 1:max_eigen]) / ei_gen$values[1:max_eigen])
# Regularized inverse using RcppArmadillo
cov_inv_arma <- calc_inv(mat_rix, max_eigen)
all.equal(cov_inv, cov_inv_arma)
# Microbenchmark RcppArmadillo code
library(microbenchmark)
summary(microbenchmark(
  Rcode = {
    ei_gen <- eigen(cov(mat_rix))
    ei_gen$vectors[, 1:max_eigen] %*%
(t(ei_gen$vectors[, 1:max_eigen]) / ei_gen$values[1:max_eigen])
  },
  Rcpp = calc_inv(mat_rix, max_eigen),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary

# futures contracts codes
future_s <- rbind(c("S&P500 index", "ES"),
              c("10yr Treasury", "ZN"),
              c("VIX index", "VX"),
              c("Gold", "GC"),
              c("Oil", "CL"),
              c("Euro FX", "EC"),
              c("Swiss franc", "SF"),
              c("Japanese Yen", "JY"))
colnames(future_s) <- c("Futures contract", "Code")
print(xtable::xtable(future_s), comment=FALSE, size="scriptsize", include.rownames=FALSE, latex.environments="flushleft")

# Monthly futures contract codes
month_codes <- cbind(c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"),
                     c("F", "G", "H", "J", "K", "M", "N", "Q", "U", "V", "X", "Z"))
colnames(month_codes) <- c("Month", "Code")
print(xtable::xtable(month_codes), comment=FALSE, size="scriptsize", include.rownames=FALSE, latex.environments="flushright")

# futures contracts codes
future_s <- rbind(c("S&P500 index", "SP", "ES"),
              c("10yr Treasury", "ZN", "ZN"),
              c("VIX index", "VX", "delisted"),
              c("Gold", "GC", "YG"),
              c("Oil", "CL", "QM"),
              c("Euro FX", "EC", "E7"),
              c("Swiss franc", "SF", "MSF"),
              c("Japanese Yen", "JY", "J7"))
colnames(future_s) <- c("Futures contract", "Standard", "E-mini")
print(xtable::xtable(future_s), comment=FALSE, size="scriptsize", include.rownames=FALSE, latex.environments="flushleft")

# Load data for S&P Emini futures June 2019 contract
sym_bol <- "ES"
dir_name <- "C:/Develop/data/ib_data"
file_name <- file.path(dir_name, paste0(sym_bol, ".csv"))
# Read a data table from CSV file
price_s <- data.table::fread(file_name)
# Coerce price_s into data frame
data.table::setDF(price_s)
# Or
# price_s <- data.table:::as.data.frame.data.table(
#   data.table::fread(file_name))
# first column of price_s is a numeric date-time
tail(price_s)
# Coerce price_s into xts series
price_s <- xts::xts(price_s[, 2:6],
  order.by=as.Date(as.POSIXct.numeric(price_s[, 1],
    tz="America/New_York",
    origin="1970-01-01")))
colnames(price_s) <- c("Open", "High", "Low", "Close", "Volume")
tail(price_s)

# Plot OHLC data in x11 window
x11(width=5, height=4)  # Open x11 for plotting
par(mar=c(5, 5, 2, 1), oma=c(0, 0, 0, 0))
chart_Series(x=price_s, TA="add_Vo()",
  name="S&P500 futures")
# Plot dygraph
dygraphs::dygraph(price_s[, 1:4], main="OHLC prices") %>%
  dyCandlestick()

# Load ESU8 data
dir_name <- "C:/Develop/data/ib_data"
file_name <- file.path(dir_name, "ESU8.csv")
ES_U8 <- data.table::fread(file_name)
data.table::setDF(ES_U8)
ES_U8 <- xts::xts(ES_U8[, 2:6],
  order.by=as.Date(as.POSIXct.numeric(ES_U8[, 1],
    tz="America/New_York", origin="1970-01-01")))
colnames(ES_U8) <- c("Open", "High", "Low", "Close", "Volume")
# Load ESM8 data
file_name <- file.path(dir_name, "ESM8.csv")
ES_M8 <- data.table::fread(file_name)
data.table::setDF(ES_M8)
ES_M8 <- xts::xts(ES_M8[, 2:6],
  order.by=as.Date(as.POSIXct.numeric(ES_M8[, 1],
    tz="America/New_York", origin="1970-01-01")))
colnames(ES_M8) <- c("Open", "High", "Low", "Close", "Volume")

x11(width=6, height=5)  # Open x11 for plotting
# Plot last month of ESU8 and ESM8 volume data
en_d <- end(ES_M8)
star_t <- (en_d - 30*24*60^2)
vol_ume <- cbind(Vo(ES_U8),
  Vo(ES_M8))[paste0(star_t, "/", en_d)]
colnames(vol_ume) <- c("ESU8", "ESM8")
col_ors <- c("blue", "green")
plot(vol_ume, col=col_ors, lwd=3, major.ticks="days",
     format.labels="%b-%d", observation.based=TRUE,
     main="Volumes of ESU8 and ESM8 futures")
legend("topleft", legend=colnames(vol_ume), col=col_ors,
 title=NULL, bty="n", lty=1, lwd=6, inset=0.1, cex=0.7)

# Find date when ESU8 volume exceeds ESM8
exceed_s <- (vol_ume[, "ESU8"] > vol_ume[, "ESM8"])
in_dex <- match(TRUE, exceed_s)
# in_dex <- min(which(exceed_s))
# Scale the ES_M8 prices
in_dex <- index(exceed_s[in_dex])
fac_tor <- as.numeric(Cl(ES_U8[in_dex])/Cl(ES_M8[in_dex]))
ES_M8[, 1:4] <- fac_tor*ES_M8[, 1:4]
# Calculate continuous contract prices
chain_ed <- rbind(ES_M8[index(ES_M8) < in_dex],
            ES_U8[index(ES_U8) >= in_dex])
# Or
# Chain_ed <- rbind(ES_M8[paste0("/", in_dex-1)],
#                   ES_U8[paste0(in_dex, "/")])
# Plot continuous contract prices
chart_Series(x=chain_ed["2018"], TA="add_Vo()",
  name="S&P500 chained futures")

# Download VIX index data from CBOE
vix_index <- data.table::fread("http://www.cboe.com/publish/scheduledtask/mktdata/datahouse/vixcurrent.csv", skip=1)
class(vix_index)
dim(vix_index)
tail(vix_index)
sapply(vix_index, class)
vix_index <- xts(vix_index[, -1],
  order.by=as.Date(vix_index$Date, format="%m/%d/%Y"))
colnames(vix_index) <- c("Open", "High", "Low", "Close")
# Save the VIX data to binary file
load(file="C:/Develop/data/ib_data/vix_cboe.RData")
ls(vix_env)
vix_env$vix_index <- vix_index
ls(vix_env)
save(vix_env, file="C:/Develop/data/ib_data/vix_cboe.RData")
# Plot OHLC data in x11 window
chart_Series(x=vix_index["2018"], name="VIX Index")
# Plot dygraph
dygraphs::dygraph(vix_index, main="VIX Index") %>%
  dyCandlestick()

# Read CBOE monthly futures expiration dates
date_s <- read.csv(
  file="C:/Develop/data/vix_data/vix_dates.csv",
  stringsAsFactors=FALSE)
date_s <- as.Date(date_s[, 1])
year_s <- format(date_s, format="%Y")
year_s <- substring(year_s, 4)
# Monthly futures contract codes
month_codes <-
  c("F", "G", "H", "J", "K", "M",
    "N", "Q", "U", "V", "X", "Z")
sym_bols <- paste0("VX", month_codes, year_s)
date_s <- as.data.frame(date_s)
colnames(date_s) <- "exp_dates"
rownames(date_s) <- sym_bols
# Write dates to CSV file, with row names
write.csv(date_s, row.names=TRUE,
  file="C:/Develop/data/vix_data/vix_futures.csv")
# Read back CBOE futures expiration dates
date_s <- read.csv(file="C:/Develop/data/vix_data/vix_futures.csv",
  stringsAsFactors=FALSE, row.names=1)
date_s[, 1] <- as.Date(date_s[, 1])

# Load VIX futures data from binary file
load(file="C:/Develop/data/vix_data/vix_cboe.RData")
# Get all VIX futures for 2018 except January
sym_bols <- ls(vix_env)
sym_bols <- sym_bols[grep("*8", sym_bols)]
sym_bols <- sym_bols[2:9]
# Specify dates for curves
low_vol <- as.Date("2018-01-11")
hi_vol <- as.Date("2018-02-05")
# Extract all VIX futures prices on the dates
curve_s <- lapply(sym_bols, function(sym_bol) {
  x_ts <- get(x=sym_bol, envir=vix_env)
  Cl(x_ts[c(low_vol, hi_vol)])
})  # end lapply
curve_s <- rutils::do_call(cbind, curve_s)
colnames(curve_s) <- sym_bols
curve_s <- t(coredata(curve_s))
colnames(curve_s) <- c("Contango 01/11/2018",
                 "Backwardation 02/05/2018")

x11(width=7, height=5)
par(mar=c(3, 2, 1, 1), oma=c(0, 0, 0, 0))
plot(curve_s[, 1], type="l", lty=1, col="blue", lwd=3,
     xaxt="n", xlab="", ylab="", ylim=range(curve_s),
     main="VIX Futures Curves")
axis(1, at=(1:NROW(curve_s)), labels=rownames(curve_s))
lines(curve_s[, 2], lty=1, lwd=3, col="red")
legend(x="topright", legend=colnames(curve_s),
 inset=0.05, cex=1.0, bty="n",
 col=c("blue", "red"), lwd=6, lty=1)

# Load VIX futures data from binary file
load(file="C:/Develop/data/vix_data/vix_cboe.RData")
# Read CBOE futures expiration dates
date_s <- read.csv(file="C:/Develop/data/vix_data/vix_futures.csv",
  stringsAsFactors=FALSE, row.names=1)
sym_bols <- rownames(date_s)
date_s <- as.Date(date_s[, 1])
to_day <- as.Date("2018-05-07")
maturi_ty <- (to_day + 30)
# Find neighboring futures contracts
in_dex <- match(TRUE, date_s > maturi_ty)
front_date <- date_s[in_dex-1]
back_date <- date_s[in_dex]
front_symbol <- sym_bols[in_dex-1]
back_symbol <- sym_bols[in_dex]
front_price <- get(x=front_symbol, envir=vix_env)
front_price <- as.numeric(Cl(front_price[to_day]))
back_price <- get(x=back_symbol, envir=vix_env)
back_price <- as.numeric(Cl(back_price[to_day]))
# Calculate the constant maturity 30-day futures price
ra_tio <- as.numeric(maturi_ty - front_date) /
  as.numeric(back_date - front_date)
pric_e <- (ra_tio*back_price + (1-ra_tio)*front_price)

library(HighFreq)
x11(width=5, height=3)  # Open x11 for plotting
# Load VIX futures data from binary file
load(file="C:/Develop/data/vix_data/vix_cboe.RData")
# Plot VIX and SVXY data in x11 window
plot_theme <- chart_theme()
plot_theme$col$line.col <- "blue"
chart_Series(x=Cl(vix_env$vix_index["2007/"]),
       theme=plot_theme, name="VIX Index")
chart_Series(x=Cl(rutils::etf_env$VTI["2007/"]),
       theme=plot_theme, name="VTI ETF")

chart_Series(x=Cl(vix_env$vix_index["2017/2018"]),
       theme=plot_theme, name="VIX Index")
chart_Series(x=Cl(rutils::etf_env$SVXY["2017/2018"]),
       theme=plot_theme, name="SVXY ETF")

Sys.Date()  # Get today's date
as.Date(1e3)  # Coerce numeric into date object
date_time <- as.Date("2014-07-14")  # "%Y-%m-%d" or "%Y/%m/%d"
date_time
class(date_time)  # Date object
as.Date("07-14-2014", "%m-%d-%Y")  # Specify format
date_time + 20  # Add 20 days
# Extract internal representation to integer
as.numeric(date_time)
date_old <- as.Date("07/14/2013", "%m/%d/%Y")
date_old
# Difference between dates
difftime(date_time, date_old, units="weeks")
weekdays(date_time)  # Get day of the week
# Coerce numeric into date-times
date_time <- 0
attributes(date_time) <- list(class="Date")
date_time  # "Date" object
structure(0, class="Date")  # "Date" object
structure(10000.25, class="Date")

date_time <- Sys.time()  # Get today's date and time
date_time
class(date_time)  # POSIXct object
# POSIXct stored as integer moment of time
as.numeric(date_time)
# Parse character string "%Y-%m-%d %H:%M:%S" to POSIXct object
date_time <- as.POSIXct("2014-07-14 13:30:10")
# Different time zones can have same clock time
as.POSIXct("2014-07-14 13:30:10", tz="America/New_York")
as.POSIXct("2014-07-14 13:30:10", tz="UTC")
# Format argument allows parsing different date-time string formats
as.POSIXct("07/14/2014 13:30:10", format="%m/%d/%Y %H:%M:%S",
     tz="America/New_York")

# Same moment of time corresponds to different clock times
time_ny <- as.POSIXct("2014-07-14 13:30:10",
     tz="America/New_York")
time_ldn <- as.POSIXct("2014-07-14 13:30:10",
     tz="UTC")
# Add five hours to POSIXct
time_ny + 5*60*60
# subtract POSIXct
time_ny - time_ldn
class(time_ny - time_ldn)
# Compare POSIXct
time_ny > time_ldn
# Create vector of POSIXct times during trading hours
trading_times <- seq(
  from=as.POSIXct("2014-07-14 09:30:00", tz="America/New_York"),
  to=as.POSIXct("2014-07-14 16:00:00", tz="America/New_York"),
  by="10 min")
head(trading_times, 3)
tail(trading_times, 3)

# POSIXct is stored as integer moment of time
int_time <- as.numeric(date_time)
# Same moment of time corresponds to different clock times
as.POSIXct(int_time, origin="1970-01-01",
     tz="America/New_York")
as.POSIXct(int_time, origin="1970-01-01",
     tz="UTC")
# Same clock time corresponds to different moments of time
as.POSIXct("2014-07-14 13:30:10",
     tz="America/New_York") -
  as.POSIXct("2014-07-14 13:30:10", tz="UTC")
# Add 20 seconds to POSIXct
date_time + 20

date_time  # POSIXct date and time
# Parse POSIXct to string representing the clock time
format(date_time)
class(format(date_time))  # Character string
# Get clock times in different time zones
format(date_time, tz="America/New_York")
format(date_time, tz="UTC")
# Format with custom format strings
format(date_time, "%m/%Y")
format(date_time, "%m-%d-%Y %H hours")
# Trunc to hour
format(date_time, "%m-%d-%Y %H:00:00")
# Date converted to midnight UTC moment of time
as.POSIXct(Sys.Date())
as.POSIXct(as.numeric(as.POSIXct(Sys.Date())),
     origin="1970-01-01",
     tz="UTC")

# Parse character string "%Y-%m-%d %H:%M:%S" to POSIXlt object
date_time <- as.POSIXlt("2014-07-14 18:30:10")
date_time
class(date_time)  # POSIXlt object
as.POSIXct(date_time)  # Coerce to POSIXct object
# Extract internal list representation to vector
unlist(date_time)
date_time + 20  # Add 20 seconds
class(date_time + 20)  # Implicit coercion to POSIXct
trunc(date_time, units="hours")  # Truncate to closest hour
trunc(date_time, units="days")  # Truncate to closest day
methods(trunc)  # Trunc methods
trunc.POSIXt

Sys.timezone()  # Get time-zone
Sys.setenv(TZ="UTC")  # set time-zone to UTC
Sys.timezone()  # Get time-zone
# Standard Time in effect
as.POSIXct("2013-03-09 11:00:00", tz="America/New_York")
# Daylight Savings Time in effect
as.POSIXct("2013-03-10 11:00:00", tz="America/New_York")
date_time <- Sys.time()  # Today's date and time
# Convert to character in different TZ
format(date_time, tz="America/New_York")
format(date_time, tz="UTC")
# Parse back to POSIXct
as.POSIXct(format(date_time, tz="America/New_York"))
# Difference between New_York time and UTC
as.POSIXct(format(Sys.time(), tz="UTC")) -
  as.POSIXct(format(Sys.time(), tz="America/New_York"))
# set time-zone to New York
Sys.setenv(TZ="America/New_York")

library(lubridate)  # Load lubridate
# Parse strings into date-times
as.POSIXct("07-14-2014", format="%m-%d-%Y", tz="America/New_York")
date_time <- lubridate::mdy("07-14-2014", tz="America/New_York")
date_time
class(date_time)  # POSIXct object
lubridate::dmy("14.07.2014", tz="America/New_York")

# Parse numeric into date-times
as.POSIXct(as.character(14072014), format="%d%m%Y",
                  tz="America/New_York")
lubridate::dmy(14072014, tz="America/New_York")

# Parse decimal to date-times
lubridate::decimal_date(date_time)
lubridate::date_decimal(2014.25, tz="America/New_York")
date_decimal(decimal_date(date_time), tz="America/New_York")

library(lubridate)  # Load lubridate
date_time <- lubridate::ymd_hms(20140714142010,
               tz="America/New_York")
date_time
# Get same moment of time in "UTC" time zone
lubridate::with_tz(date_time, "UTC")
as.POSIXct(format(date_time, tz="UTC"), tz="UTC")
# Get same clock time in "UTC" time zone
lubridate::force_tz(date_time, "UTC")
as.POSIXct(format(date_time, tz="America/New_York"),
     tz="UTC")
# Same moment of time
date_time - with_tz(date_time, "UTC")
# Different moments of time
date_time - force_tz(date_time, "UTC")

library(lubridate)  # Load lubridate
# Daylight Savings Time handling periods vs durations
date_time <- as.POSIXct("2013-03-09 11:00:00",
                  tz="America/New_York")
date_time
date_time + lubridate::ddays(1)  # Add duration
date_time + lubridate::days(1)  # Add period

leap_year(2012)  # Leap year
date_time <- lubridate::dmy(01012012, tz="America/New_York")
date_time
date_time + lubridate::dyears(1)  # Add duration
date_time + lubridate::years(1)  # Add period

library(lubridate)  # Load lubridate
date_time <- lubridate::ymd_hms(20140714142010, tz="America/New_York")
date_time
# Add periods to a date-time
c(date_time + lubridate::seconds(1), date_time + lubridate::minutes(1),
  date_time + lubridate::days(1), date_time + lubridate::months(1))

# Create vectors of dates
date_time <- lubridate::ymd(20140714, tz="America/New_York")
date_time + 0:2 * lubridate::months(1)  # Monthly dates
date_time + lubridate::months(0:2)
date_time + 0:2 * lubridate::months(2)  # bi-monthly dates
date_time + seq(0, 5, by=2) * lubridate::months(1)
seq(date_time, length=3, by="2 months")

library(lubridate)  # Load lubridate
# Adding monthly periods can create invalid dates
date_time <- lubridate::ymd(20120131, tz="America/New_York")
date_time + 0:2 * lubridate::months(1)
date_time + lubridate::months(1)
date_time + lubridate::months(2)

# Create vector of end-of-month dates
date_time %m-% lubridate::months(13:1)

library(zoo)  # Load zoo
library(RQuantLib)  # Load RQuantLib

# Create daily date series of class "Date"
in_dex <- Sys.Date() + -5:2
in_dex

# Create Boolean vector of business days
is_busday <- isBusinessDay(  # RQuantLib calendar
  calendar="UnitedStates/GovernmentBond", in_dex)

# Create daily series of business days
bus_index <- in_dex[is_busday]
bus_index

library(zoo)  # Load package zoo
date_time <- Sys.Date()  # Create date series of class "Date"
in_dex <- date_time + 0:365  # Daily series over one year
head(in_dex, 4)  # Print first few dates
format(head(in_dex, 4), "%m/%d/%Y")  # Print first few dates
# Create daily date-time series of class "POSIXct"
in_dex <- seq(Sys.time(), by="days", length.out=365)
head(in_dex, 4)  # Print first few dates
format(head(in_dex, 4), "%m/%d/%Y %H:%M:%S")  # Print first few dates
# Create series of monthly dates of class "zoo"
monthly_index <- yearmon(2010+0:36/12)
head(monthly_index, 4)  # Print first few dates
# Create series of quarterly dates of class "zoo"
qrtly_index <- yearqtr(2010+0:16/4)
head(qrtly_index, 4)  # Print first few dates
# Parse quarterly "zoo" dates to POSIXct
Sys.setenv(TZ="UTC")
as.POSIXct(head(qrtly_index, 4))

library(lubridate)  # Load lubridate
set.seed(1121)  # Reset random number generator
# Create daily time series ending today
start_date <- decimal_date(Sys.Date()-6)
end_date <- decimal_date(Sys.Date())
# Create vector of geometric Brownian motion
da_ta <- exp(cumsum(rnorm(6)/100))
fre_quency <- NROW(da_ta)/(end_date-start_date)
ts_series <- ts(data=da_ta,
          start=start_date,
          frequency=fre_quency)
ts_series  # Display time series
# Display index dates
as.Date(date_decimal(zoo::coredata(time(ts_series))))
# bi-monthly geometric Brownian motion starting mid-1990
ts_series <- ts(data=exp(cumsum(rnorm(96)/100)),
       frequency=6, start=1990.5)

# show some methods for class "ts"
matrix(methods(class="ts")[3:8], ncol=2)
# "tsp" attribute specifies the date-time index
attributes(ts_series)
# Extract the index
tail(time(ts_series), 11)
# The index is equally spaced
diff(tail(time(ts_series), 11))
# Subset the time series
window(ts_series, start=1992, end=1992.25)

par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
plot(ts_series, type="l",  # Create plot
     col="red", lty="solid", xlab="", ylab="")
title(main="Random Prices", line=-1)  # Add title

class(EuStockMarkets)  # Multiple ts object
dim(EuStockMarkets)
head(EuStockMarkets, 3)  # Get first three rows
# EuStockMarkets index is equally spaced
diff(tail(time(EuStockMarkets), 11))

par(mar=c(1, 2, 1, 1), oma=c(0, 0, 0, 0))
# Plot all the columns in separate panels
plot(EuStockMarkets, main="EuStockMarkets", xlab="")

par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# Plot in single panel
plot(EuStockMarkets, main="EuStockMarkets",
     xlab="", ylab="", plot.type="single",
     col=c("black", "red", "blue", "green"))
# Add legend
legend(x=1992, y=8000,
 legend=colnames(EuStockMarkets),
 col=c("black", "red", "blue", "green"),
 lwd=6, lty=1)

set.seed(1121)  # Reset random number generator
library(zoo)  # Load package zoo
# Create zoo time series of random returns
in_dex <- Sys.Date() + 0:3
zoo_series <- zoo(rnorm(NROW(in_dex)),
         order.by=in_dex)
zoo_series
attributes(zoo_series)
class(zoo_series)  # Class "zoo"
tail(zoo_series, 3)  # Get last few elements

library(zoo)  # Load package zoo
zoo::coredata(zoo_series)  # Extract coredata
zoo::index(zoo_series)  # Extract time index
start(zoo_series)  # First date
end(zoo_series)  # Last date
zoo_series[start(zoo_series)]  # First element
zoo_series[end(zoo_series)]  # Last element
zoo::coredata(zoo_series) <- rep(1, 4)  # Replace coredata
cumsum(zoo_series)  # Cumulative sum
cummax(cumsum(zoo_series))
cummin(cumsum(zoo_series))

library(zoo)  # Load package zoo
zoo_series <-
  zoo(as.matrix(cumsum(rnorm(100)), nc=1),
order.by=seq(from=as.Date("2013-06-15"),
             by="day", length.out=100))
colnames(zoo_series) <- "zoo_series"
tail(zoo_series)
dim(zoo_series)
attributes(zoo_series)

library(zoo)  # Load package zoo
zoo::coredata(zoo_series) <- (1:4)^2  # Replace coredata
zoo_series
lag(zoo_series)  # One day lag
lag(zoo_series, 2)  # Two day lag
lag(zoo_series, k=-1)  # Proper one day lag
diff(zoo_series)  # Diff with one day lag
# Proper lag and original length
lag(zoo_series, -2, na.pad=TRUE)

set.seed(1121)  # Reset random number generator
library(zoo)  # Load package zoo
# Create index of daily dates
in_dex <- seq(from=as.Date("2014-07-14"),
            by="day", length.out=1000)
# Create vector of geometric Brownian motion
zoo_data <-
  exp(cumsum(rnorm(NROW(in_dex))/100))
# Create zoo series of geometric Brownian motion
zoo_series <- zoo(x=zoo_data, order.by=in_dex)

par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# Plot using plot.zoo method
plot(zoo_series, xlab="", ylab="")
title(main="Random Prices", line=-1)  # Add title

library(zoo)  # Load package zoo
# Subset zoo as matrix
zoo_series[459:463, 1]
# Subset zoo using window()
window(zoo_series,
 start=as.Date("2014-10-15"),
 end=as.Date("2014-10-19"))
# Subset zoo using Date object
zoo_series[as.Date("2014-10-15")]

set.seed(1121)  # Reset random number generator
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
library(zoo)  # Load package zoo
# Create daily date series of class "Date"
in_dex1 <- seq(Sys.Date(), by="days",
             length.out=365)
# Create zoo time series of random returns
zoo_series1 <- zoo(rnorm(NROW(in_dex1)),
           order.by=in_dex1)
# Create another zoo time series of random returns
in_dex2 <- seq(Sys.Date()+350, by="days",
             length.out=365)
zoo_series2 <- zoo(rnorm(NROW(in_dex2)),
           order.by=in_dex2)
# rbind the two time series - ts1 supersedes ts2
zoo_series3 <- rbind(zoo_series1,
           zoo_series2[zoo::index(zoo_series2) > end(zoo_series1)])
# Plot zoo time series of geometric Brownian motion
plot(exp(cumsum(zoo_series3)/100), xlab="", ylab="")
# Add vertical lines at stitch point
abline(v=end(zoo_series1), col="blue", lty="dashed")
abline(v=start(zoo_series2), col="red", lty="dashed")
title(main="Random Prices", line=-1)  # Add title

# Create daily date series of class "Date"
in_dex1 <- Sys.Date() + -3:1
# Create zoo time series of random returns
zoo_series1 <- zoo(rnorm(NROW(in_dex1)),
         order.by=in_dex1)
# Create another zoo time series of random returns
in_dex2 <- Sys.Date() + -1:3
zoo_series2 <- zoo(rnorm(NROW(in_dex2)),
         order.by=in_dex2)
merge(zoo_series1, zoo_series2)  # union of dates
# Intersection of dates
merge(zoo_series1, zoo_series2, all=FALSE)

# Create matrix containing NA values
mat_rix <- sample(18)
mat_rix[sample(NROW(mat_rix), 4)] <- NA
mat_rix <- matrix(mat_rix, nc=3)
# Replace NA values with most recent non-NA values
zoo::na.locf(mat_rix)
rutils::na_locf(mat_rix)
# Get time series of prices
price_s <- mget(c("VTI", "VXX"), envir=rutils::etf_env)
price_s <- lapply(price_s, quantmod::Cl)
price_s <- rutils::do_call(cbind, price_s)
sum(is.na(price_s))
# Carry forward and backward non-NA prices
price_s <- zoo::na.locf(price_s, na.rm=FALSE)
price_s <- zoo::na.locf(price_s, na.rm=FALSE, fromLast=TRUE)
sum(is.na(price_s))
# Remove whole rows containing NA returns
re_turns <- rutils::etf_env$re_turns
sum(is.na(re_turns))
re_turns <- na.omit(re_turns)
# Or carry forward non-NA returns (preferred)
re_turns <- rutils::etf_env$re_turns
re_turns[1, is.na(re_turns[1, ])] <- 0
re_turns <- zoo::na.locf(re_turns, na.rm=FALSE)
sum(is.na(re_turns))

# Replace NAs in xts time series
se_ries <- rutils::etf_env$price_s[, 1]
head(se_ries)
sum(is.na(se_ries))
library(quantmod)
series_zoo <- as.xts(zoo::na.locf(se_ries, na.rm=FALSE, fromLast=TRUE))
series_xts <- xts:::na.locf.xts(se_ries, fromLast=TRUE)
all.equal(series_zoo, series_xts, check.attributes=FALSE)
library(microbenchmark)
summary(microbenchmark(
  zoo=as.xts(zoo::na.locf(se_ries, fromLast=TRUE)),
  xts=xts:::na.locf.xts(se_ries, fromLast=TRUE),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

library(lubridate)  # Load lubridate
library(zoo)  # Load package zoo
# methods(as.zoo)  # Many methods of coercing into zoo
class(EuStockMarkets)  # Multiple ts object
# Coerce mts object into zoo
zoo_series <- as.zoo(EuStockMarkets)
class(zoo::index(zoo_series))  # Index is numeric
head(zoo_series, 3)
# Approximately convert index into class "Date"
zoo::index(zoo_series) <-
  as.Date(365*(zoo::index(zoo_series)-1970))
head(zoo_series, 3)
# Convert index into class "POSIXct"
zoo_series <- as.zoo(EuStockMarkets)
zoo::index(zoo_series) <- date_decimal(zoo::index(zoo_series))
head(zoo_series, 3)

library(lubridate)  # Load lubridate
library(zoo)  # Load package zoo
set.seed(1121)  # Reset random number generator
# Create index of daily dates
in_dex <- seq(from=as.Date("2014-07-14"),
            by="day", length.out=1000)
# Create vector of geometric Brownian motion
zoo_data <- exp(cumsum(rnorm(NROW(in_dex))/100))
# Create zoo time series of geometric Brownian motion
zoo_series <- zoo(x=zoo_data,
            order.by=in_dex)
head(zoo_series, 3)  # zoo object
# as.ts() creates ts object with frequency=1
ts_series <- as.ts(zoo_series)
tsp(ts_series)  # Frequency=1
# Get start and end dates of zoo_series
start_date <- decimal_date(start(zoo_series))
end_date <- decimal_date(end(zoo_series))
# Calculate frequency of zoo_series
fre_quency <- NROW(zoo_series)/(end_date-start_date)
da_ta <- zoo::coredata(zoo_series)  # Extract data from zoo_series
# Create ts object using ts()
ts_series <- ts(data=da_ta, start=start_date,
          frequency=fre_quency)
# Display start of time series
window(ts_series, start=start(ts_series),
 end=start(ts_series)+4/365)
head(time(ts_series))  # Display index dates
head(as.Date(date_decimal(zoo::coredata(time(ts_series)))))

library(lubridate)  # Load lubridate
library(zoo)  # Load package zoo
# Create weekday Boolean vector
week_days <- weekdays(zoo::index(zoo_series))
is_weekday <- !((week_days == "Saturday") |
  (week_days == "Sunday"))
# Remove weekends from zoo time series
zoo_series <- zoo_series[is_weekday, ]
head(zoo_series, 7)  # zoo object
# as.ts() creates NA values
ts_series <- as.ts(zoo_series)
head(ts_series, 7)
# Create vector of regular dates, including weekends
in_dex <- seq(from=start(zoo_series),
            by="day",
            length.out=NROW(zoo_series))
zoo::index(zoo_series) <- in_dex
ts_series <- as.ts(zoo_series)
head(ts_series, 7)

set.seed(1121)  # Reset random number generator
library(xts)  # Load package xts
# Create xts time series of random returns
in_dex <- Sys.Date() + 0:3
x_ts <- xts(rnorm(NROW(in_dex)),
         order.by=in_dex)
names(x_ts) <- "random"
x_ts
tail(x_ts, 3)  # Get last few elements
first(x_ts)  # Get first element
last(x_ts)  # Get last element
class(x_ts)  # Class "xts"
attributes(x_ts)
# Get the time zone of an xts object
indexTZ(x_ts)

load(file="C:/Develop/lecture_slides/data/zoo_data.RData")
library(xts)  # Load package xts
# as.xts() coerces zoo series into xts series
st_ox <- as.xts(zoo_stx)
dim(st_ox)
head(st_ox[, 1:4], 4)
# Plot using plot.xts method
xts::plot.xts(st_ox[, "Close"], xlab="", ylab="", main="")
title(main="MSFT Prices")  # Add title

library(xts)  # Load xts
library(lubridate)  # Load lubridate
# Coerce EuStockMarkets into class xts
x_ts <- xts(zoo::coredata(EuStockMarkets),
      order.by=date_decimal(zoo::index(EuStockMarkets)))
# Plot all columns in single panel: xts v.0.9-8
col_ors <- rainbow(NCOL(x_ts))
plot(x_ts, main="EuStockMarkets using xts",
     col=col_ors, major.ticks="years",
     minor.ticks=FALSE)
legend("topleft", legend=colnames(EuStockMarkets),
 inset=0.2, cex=0.7, , lty=rep(1, NCOL(x_ts)),
 lwd=3, col=col_ors, bg="white")
# Plot only first column: xts v.0.9-7
plot(x_ts[, 1], main="EuStockMarkets using xts",
     col=col_ors[1], major.ticks="years",
     minor.ticks=FALSE)
# Plot remaining columns
for (col_umn in 2:NCOL(x_ts))
  lines(x_ts[, col_umn], col=col_ors[col_umn])
# Plot using quantmod
library(quantmod)
plot_theme <- chart_theme()
plot_theme$col$line.col <- col_ors
chart_Series(x=x_ts, theme=plot_theme,
       name="EuStockMarkets using quantmod")
legend("topleft", legend=colnames(EuStockMarkets),
 inset=0.2, cex=0.7, , lty=rep(1, NCOL(x_ts)),
 lwd=3, col=col_ors, bg="white")

library(rutils)
library(ggplot2)
price_s <- rutils::etf_env$price_s[, 1]
price_s <- na.omit(price_s)
# Create ggplot object
etf_gg <- qplot(x=zoo::index(price_s),
          y=as.numeric(price_s),
          geom="line",
          main=names(price_s)) +
  xlab("") + ylab("") +
  theme(  # Add legend and title
    legend.position=c(0.1, 0.5),
    plot.title=element_text(vjust=-2.0),
    plot.background=element_blank()
  )  # end theme
# Render ggplot object
etf_gg

library(rutils)  # Load xts time series data
library(reshape2)
library(ggplot2)
price_s <- rutils::etf_env$price_s[, c("VTI", "IEF")]
price_s <- na.omit(price_s)
# Create data frame of time series
data_frame <- data.frame(dates=zoo::index(price_s),
    zoo::coredata(price_s))
# reshape data into a single column
data_frame <-
  reshape2::melt(data_frame, id="dates")
x11(width=6, height=5)  # Open plot window
# ggplot the melted data_frame
ggplot(data=data_frame,
 mapping=aes(x=dates, y=value, colour=variable)) +
 geom_line() +
  xlab("") + ylab("") +
  ggtitle("VTI and IEF") +
  theme(  # Add legend and title
    legend.position=c(0.2, 0.8),
    plot.title=element_text(vjust=-2.0)
  )  # end theme

# Load rutils which contains etf_env dataset
library(rutils)
library(dygraphs)
price_s <- rutils::etf_env$price_s[, c("VTI", "IEF")]
price_s <- na.omit(price_s)
# Plot dygraph with date range selector
dygraph(price_s, main="VTI and IEF prices") %>%
  dyOptions(colors=c("blue","green")) %>%
  dyRangeSelector()

# Load rutils which contains etf_env dataset
library(rutils)
library(plotly)
price_s <- rutils::etf_env$price_s[, c("VTI", "IEF")]
price_s <- na.omit(price_s)
# Create data frame of time series
data_frame <- data.frame(dates=zoo::index(price_s),
    zoo::coredata(price_s))
# Plotly syntax using pipes
data_frame %>%
  plot_ly(x=~dates, y=~VTI, type="scatter", mode="lines", name="VTI") %>%
  add_trace(x=~dates, y=~IEF, type="scatter", mode="lines", name="IEF") %>%
  layout(title="VTI and IEF prices",
   xaxis=list(title="Time"),
   yaxis=list(title="Stock Prices"),
   legend=list(x=0.1, y=0.9))
# Or use standard plotly syntax
p_lot <- plot_ly(data=data_frame, x=~dates, y=~VTI, type="scatter", mode="lines", name="VTI")
p_lot <- add_trace(p=p_lot, x=~dates, y=~IEF, type="scatter", mode="lines", name="IEF")
p_lot <- layout(p=p_lot, title="VTI and IEF prices", xaxis=list(title="Time"), yaxis=list(title="Stock Prices"), legend=list(x=0.1, y=0.9))
p_lot

# Subset xts using a date range string
price_s <- rutils::etf_env$price_s
sub_prices <- price_s["2014-10-15/2015-01-10", 1:4]
first(sub_prices)
last(sub_prices)
# Subset Nov 2014 using a date string
sub_prices <- price_s["2014-11", 1:4]
first(sub_prices)
last(sub_prices)
# Subset all data after Nov 2014
sub_prices <- price_s["2014-11/", 1:4]
first(sub_prices)
last(sub_prices)
# Comma after date range not necessary
all.equal(price_s["2014-11", ], price_s["2014-11"])
# .subset_xts() is faster than the bracket []
library(microbenchmark)
summary(microbenchmark(
  bracket=price_s[10:20, ],
  subset=xts::.subset_xts(price_s, 10:20),
  times=10))[, c(1, 4, 5)]

# Specify string representing a date
dat_e <- "2014-10-15"
# Subset price_s in two different ways
price_s <- rutils::etf_env$price_s
all.equal(price_s[zoo::index(price_s) >= dat_e],
    price_s[paste0(dat_e, "/")])
# Boolean subsetting is slower because coercing string into date
library(microbenchmark)
summary(microbenchmark(
  boolean=(price_s[zoo::index(price_s) >= dat_e]),
  date=(price_s[paste0(dat_e, "/")]),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Coerce string into a date
dat_e <- as.Date("2014-10-15")
# Boolean subsetting is faster than using date string
summary(microbenchmark(
  boolean=(price_s[zoo::index(price_s) >= dat_e]),
  date=(price_s[paste0(dat_e, "/")]),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

price_s <- HighFreq::SPY["2012-04"]
# Subset recurring time interval using "T notation",
price_s <- price_s["T10:30:00/T15:00:00"]
first(price_s["2012-04-16"])  # First element of day
last(price_s["2012-04-16"])  # Last element of day
# suppress timezone warning messages
options(xts_check_tz=FALSE)

price_s <- rutils::etf_env$price_s[, c("VTI", "IEF")]
price_s <- na.omit(price_s)
str(price_s)  # Display structure of xts
# Subsetting zoo to single column drops dim attribute
zoo_prices <- as.zoo(price_s)
dim(zoo_prices)
dim(zoo_prices[, 1])
# zoo with single column are vectors not matrices
c(is.matrix(zoo_prices), is.matrix(zoo_prices[, 1]))
# xts always have a dim attribute
rbind(base=dim(price_s), subs=dim(price_s[, 1]))
c(is.matrix(price_s), is.matrix(price_s[, 1]))

# Lag of zoo shortens it by one row
rbind(base=dim(zoo_prices), lag=dim(lag(zoo_prices)))
# Lag of xts doesn't shorten it
rbind(base=dim(price_s), lag=dim(lag(price_s)))
# Lag of zoo is in opposite direction from xts
head(lag(zoo_prices, -1), 4)
head(lag(price_s), 4)

# library(rutils)  # Load package rutils
# Indices of last observations in each hour
end_p <- xts::endpoints(price_s, on="hours")
head(end_p)
# Extract the last observations in each hour
head(price_s[end_p, ])

# Lower the periodicity to months
xts_monthly <- to.period(x=price_s,
             period="months", name="MSFT")
# Convert colnames to standard OHLC format
colnames(xts_monthly)
colnames(xts_monthly) <- sapply(
  strsplit(colnames(xts_monthly), split=".", fixed=TRUE),
  function(na_me) na_me[-1]
  )  # end sapply
head(xts_monthly, 3)
# Lower the periodicity to years
xts_yearly <- to.period(x=xts_monthly,
             period="years", name="MSFT")
colnames(xts_yearly) <- sapply(
  strsplit(colnames(xts_yearly), split=".", fixed=TRUE),
  function(na_me) na_me[-1]
  )  # end sapply
head(xts_yearly)

par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
load(file="C:/Develop/lecture_slides/data/zoo_data.RData")
library(xts)  # Load package xts
# as.xts() coerces zoo series into xts series
st_ox <- as.xts(zoo_prices)
# Subset xts using a date
stox_sub <- st_ox["2014-11", 1:4]

# Plot OHLC using plot.xts method
xts::plot.xts(stox_sub, type="candles", main="")
title(main="MSFT Prices")  # Add title

load(file="C:/Develop/lecture_slides/data/zoo_data.RData")
ts_stx <- as.ts(zoo_stx)
class(ts_stx)
tail(ts_stx[, 1:4])
library(xts)
st_ox <- as.xts(zoo_stx)
class(st_ox)
tail(st_ox[, 1:4])

# Install package IBrokers
install.packages("IBrokers")
# Load package IBrokers
library(IBrokers)
# Get documentation for package IBrokers
# Get short description
packageDescription("IBrokers")
# Load help page
help(package="IBrokers")
# List all datasets in "IBrokers"
data(package="IBrokers")
# List all objects in "IBrokers"
ls("package:IBrokers")
# Remove IBrokers from search path
detach("package:IBrokers")
# Install package IBrokers2
devtools::install_github(repo="algoquant/IBrokers2")

# Connect to Interactive Brokers TWS
ib_connect <- IBrokers::twsConnect(port=7497)
# Or connect to IB Gateway
# Ib_connect <- ibgConnect(port=4002)
# Check connection
IBrokers::isConnected(ib_connect)
# Close the Interactive Brokers API connection
IBrokers::twsDisconnect(ib_connect)

# Connect to Interactive Brokers TWS
ib_connect <- IBrokers::twsConnect(port=7497)
# Or connect to IB Gateway
# Ib_connect <- ibgConnect(port=4002)
# Download account information from IB
ac_count <- "DU1215081"
ib_account <- IBrokers::reqAccountUpdates(conn=ib_connect,
                                    acctCode=ac_count)
# Extract account balances
balance_s <- ib_account[[1]]
balance_s$AvailableFunds
# Extract contract names, net positions, and profits and losses
IBrokers::twsPortfolioValue(ib_account)
# Close the Interactive Brokers API connection
IBrokers::twsDisconnect(ib_connect)

# Define AAPL stock contract (object)
con_tract <- IBrokers::twsEquity("AAPL", primary="SMART")
# Define CHF currency contract
con_tract <- IBrokers::twsCurrency("CHF", currency="USD")
# Define S&P Emini future June 2019 contract
con_tract <- IBrokers::twsFuture(symbol="ES",
  exch="GLOBEX", expiry="201906")
# Define 10yr Treasury future June 2019 contract
con_tract <- IBrokers::twsFuture(symbol="ZN",
  exch="ECBOT", expiry="201906")
# Define euro currency future June 2019 contract
con_tract <- IBrokers::twsFuture(symbol="EUR",
  exch="GLOBEX", expiry="201906")
# Define Gold future June 2019 contract
con_tract <- IBrokers::twsFuture(symbol="GC",
  exch="NYMEX", expiry="201906")
# Define Oil future January 2019 contract
con_tract <- IBrokers::twsFuture(symbol="QM",
  exch="NYMEX", expiry="201901")
# Test if contract object is correct
IBrokers::is.twsContract(con_tract)
# Get list with instrument information
IBrokers::reqContractDetails(conn=ib_connect, Contract=con_tract)
# Install the package twsInstrument
install.packages("twsInstrument", repos="http://r-forge.r-project.org")
# Define euro future using getContract() and Conid
con_tract <- twsInstrument::getContract("317631411")
# Get list with instrument information
IBrokers::reqContractDetails(conn=ib_connect, Contract=con_tract)

# Define VIX monthly and weekly futures June 2019 contract
sym_bol <- "VIX"
con_tract <- IBrokers::twsFuture(symbol=sym_bol,
  exch="CFE", expiry="201906")
# Define VIX monthly futures June 2019 contract
con_tract <- IBrokers::twsFuture(symbol=sym_bol,
  local="VXV8", exch="CFE", expiry="201906")
# Define VIX weekly futures October 3rd 2018 contract
con_tract <- IBrokers::twsFuture(symbol=sym_bol,
  local="VX40V8", exch="CFE", expiry="201906")
# Get list with instrument information
IBrokers::reqContractDetails(conn=ib_connect,
  Contract=con_tract)

# Define S&P Emini futures June 2019 contract
sym_bol <- "ES"
con_tract <- IBrokers::twsFuture(symbol=sym_bol,
  exch="GLOBEX", expiry="201906")
# Open file for data download
dir_name <- "C:/Develop/data/ib_data"
dir.create(dir_name)
file_name <- file.path(dir_name, paste0(sym_bol, "_201906.csv"))
file_connect <- file(file_name, open="w")
# Connect to Interactive Brokers TWS
ib_connect <- IBrokers::twsConnect(port=7497)
# Write header to file
cat(paste(paste(sym_bol, c("Index", "Open", "High", "Low", "Close", "Volume", "WAP", "Count"), sep="."), collapse=","), "\n", file=file_connect)
# Download historical data to file
IBrokers::reqHistoricalData(conn=ib_connect,
  Contract=con_tract,
  barSize="1 day", duration="6 M",
  file=file_connect)
# Close data file
close(file_connect)
# Close the Interactive Brokers API connection
IBrokers::twsDisconnect(ib_connect)

# Define IB contract objects for stock symbols
sym_bols <- c("AAPL", "F", "MSFT")
con_tracts <- lapply(sym_bols, IBrokers::twsEquity, primary="SMART")
names(con_tracts) <- sym_bols
# Open file connections for data download
dir_name <- "C:/Develop/data/ib_data"
file_names <- file.path(dir_name, paste0(sym_bols, format(Sys.time(), format="_%m_%d_%Y_%H_%M"), ".csv"))
file_connects <- lapply(file_names, function(file_name) file(file_name, open="w"))
# Connect to Interactive Brokers TWS
ib_connect <- IBrokers::twsConnect(port=7497)
# Download historical 1-minute bar data to files
for (it in 1:NROW(sym_bols)) {
  sym_bol <- sym_bols[it]
  file_connect <- file_connects[[it]]
  con_tract <- con_tracts[[it]]
  cat("Downloading data for: ", sym_bol, "\n")
  # Write header to file
  cat(paste(paste(sym_bol, c("Index", "Open", "High", "Low", "Close", "Volume", "WAP", "XTRA", "Count"), sep="."), collapse=","), "\n", file=file_connect)
  IBrokers::reqHistoricalData(conn=ib_connect,
                         Contract=con_tract,
                         barSize="1 min", duration="2 D",
                         file=file_connect)
  Sys.sleep(10) # 10s pause to avoid IB pacing violation
}  # end for
# Close data files
for (file_connect in file_connects) close(file_connect)
# Close the Interactive Brokers API connection
IBrokers::twsDisconnect(ib_connect)

# Define S&P Emini futures June 2018 contract
sym_bol <- "ES"
con_tract <- IBrokers::twsFuture(symbol=sym_bol,
  include_expired="1",
  exch="GLOBEX", expiry="201806")
# Open file connection for ESM8 data download
file_name <- file.path(dir_name, paste0(sym_bol, "M8.csv"))
file_connect <- file(file_name, open="w")
# Connect to Interactive Brokers TWS
ib_connect <- IBrokers::twsConnect(port=7497)
# Download historical data to file
IBrokers::reqHistoricalData(conn=ib_connect,
  Contract=con_tract,
  barSize="1 day", duration="2 Y",
  file=file_connect)
# Close data file
close(file_connect)
# Close the Interactive Brokers API connection
IBrokers::twsDisconnect(ib_connect)

# Load OHLC data and coerce it into xts series
price_s <- data.table::fread(file_name)
data.table::setDF(price_s)
price_s <- xts::xts(price_s[, 2:6],
  order.by=as.Date(as.POSIXct.numeric(price_s[, 1],
    tz="America/New_York", origin="1970-01-01")))
colnames(price_s) <- c("Open", "High", "Low", "Close", "Volume")
# Plot OHLC data in x11 window
chart_Series(x=price_s, TA="add_Vo()",
  name="S&P500 ESM8 futures")
# Plot dygraph
dygraphs::dygraph(price_s[, 1:4], main="S&P500 ESM8 futures") %>%
  dyCandlestick()

# Define S&P Emini futures June 2019 contract
sym_bol <- "ES"
con_tract <- IBrokers::twsFuture(symbol=sym_bol,
  exch="GLOBEX", expiry="201906")
# Open file connection for data download
dir_name <- "C:/Develop/data/ib_data"
# Dir.create(dir_name)
file_name <- file.path(dir_name, paste0(sym_bol, "_taq_live.csv"))
file_connect <- file(file_name, open="w")
# Connect to Interactive Brokers TWS
ib_connect <- IBrokers::twsConnect(port=7497)
# Download live data to file
IBrokers::reqMktData(conn=ib_connect,
     Contract=con_tract,
     eventWrapper=eWrapper.MktData.CSV(1),
     file=file_connect)
# Close data file
close(file_connect)
# Close the Interactive Brokers API connection
IBrokers::twsDisconnect(ib_connect)

# Define S&P Emini futures June 2019 contract
sym_bol <- "ES"
con_tract <- IBrokers::twsFuture(symbol=sym_bol,
  exch="GLOBEX", expiry="201906")
# Open file connection for data download
dir_name <- "C:/Develop/data/ib_data"
# Dir.create(dir_name)
file_name <- file.path(dir_name, paste0(sym_bol, "_ohlc_live.csv"))
file_connect <- file(file_name, open="w")
# Connect to Interactive Brokers TWS
ib_connect <- IBrokers::twsConnect(port=7497)
# Download live data to file
IBrokers::reqRealTimeBars(conn=ib_connect,
     Contract=con_tract, barSize="1",
     eventWrapper=eWrapper.RealTimeBars.CSV(1),
     file=file_connect)
# Close the Interactive Brokers API connection
IBrokers::twsDisconnect(ib_connect)
# Close data file
close(file_connect)
# Load OHLC data and coerce it into xts series
library(data.table)
price_s <- data.table::fread(file_name)
price_s <- xts::xts(price_s[, paste0("V", 2:6)],
  as.POSIXct.numeric(as.numeric(price_s[, V1]), tz="America/New_York", origin="1970-01-01"))
colnames(price_s) <- c("Open", "High", "Low", "Close", "Volume")
# Plot OHLC data in x11 window
x11()
chart_Series(x=price_s, TA="add_Vo()",
       name="S&P500 ESM9 futures")
# Plot dygraph
library(dygraphs)
dygraphs::dygraph(price_s[, 1:4], main="S&P500 ESM9 futures") %>%
  dyCandlestick()

library(IBrokers)
# Define list of S&P futures and 10yr Treasury contracts
con_tracts <- list(ES=IBrokers::twsFuture(symbol="ES", exch="GLOBEX", expiry="201906"),
             ZN=IBrokers::twsFuture(symbol="ZN", exch="ECBOT", expiry="201906"))
# Open the file connection for storing the bar data
dir_name <- "C:/Develop/data/ib_data"
file_names <- file.path(dir_name, paste0(c("ES_", "ZN_"), format(Sys.time(), format="%m_%d_%Y_%H_%M"), ".csv"))
file_connects <- lapply(file_names, function(file_name) file(file_name, open="w"))
# Connect to Interactive Brokers TWS
ib_connect <- IBrokers::twsConnect(port=7497)
# Download live data to file
IBrokers::reqRealTimeBars(conn=ib_connect,
                    Contract=con_tracts,
                    barSize="1", useRTH=FALSE,
                    eventWrapper=eWrapper.RealTimeBars.CSV(NROW(con_tracts)),
                    file=file_connects)
# Close the Interactive Brokers API connection
IBrokers::twsDisconnect(ib_connect)
# Close data files
for (file_connect in file_connects)
  close(file_connect)
library(data.table)
# Load ES futures June 2019 contract and coerce it into xts series
price_s <- data.table::fread(file_names[1])
price_s <- xts::xts(price_s[, paste0("V", 2:6)],
  as.POSIXct.numeric(as.numeric(price_s[, V1]), tz="America/New_York", origin="1970-01-01"))
colnames(price_s) <- c("Open", "High", "Low", "Close", "Volume")
# Plot dygraph
library(dygraphs)
dygraphs::dygraph(price_s[, 1:4], main="S&P500 ESM9 futures") %>%
  dyCandlestick()
# Load ZN 10yr Treasury futures June 2019 contract
price_s <- data.table::fread(file_names[2])
price_s <- xts::xts(price_s[, paste0("V", 2:6)],
  as.POSIXct.numeric(as.numeric(price_s[, V1]), tz="America/New_York", origin="1970-01-01"))
colnames(price_s) <- c("Open", "High", "Low", "Close", "Volume")
# Plot dygraph
dygraphs::dygraph(price_s[, 1:4], main="ZN 10yr Treasury futures") %>%
  dyCandlestick()

# Define S&P Emini future June 2019 contract
con_tract <- IBrokers::twsFuture(symbol="ES", exch="GLOBEX", expiry="201906")
# Define euro currency contract EUR.USD
con_tract <- IBrokers::twsCurrency("EUR", currency="USD")
# Define euro currency E-mini futures June 2019 contract E7Z8
con_tract <- IBrokers::twsFuture(symbol="E7", exch="GLOBEX", expiry="201906")
# Define Japanese yen currency contract JPY.USD
con_tract <- IBrokers::twsCurrency("JPY", currency="USD")
# Define Japanese yen currency E-mini futures June 2019 contract J7Z8
con_tract <- IBrokers::twsFuture(symbol="J7", exch="GLOBEX", expiry="201906")
# Define Japanese yen currency futures June 2019 contract 6JZ8
con_tract <- IBrokers::twsFuture(symbol="JPY", exch="GLOBEX", expiry="201906")
# Connect to Interactive Brokers TWS
ib_connect <- IBrokers::twsConnect(port=7497)
IBrokers::reqContractDetails(conn=ib_connect, Contract=con_tract)
# Request trade order ID
order_id <- IBrokers::reqIds(ib_connect)
# Create buy market order object
ib_order <- IBrokers::twsOrder(order_id,
  orderType="MKT", action="BUY", totalQuantity=1)
# Place trade order
IBrokers::placeOrder(ib_connect, con_tract, ib_order)
# Execute sell market order
order_id <- IBrokers::reqIds(ib_connect)
ib_order <- IBrokers::twsOrder(order_id,
  orderType="MKT", action="SELL", totalQuantity=1)
IBrokers::placeOrder(ib_connect, con_tract, ib_order)
# Execute buy market order
order_id <- IBrokers::reqIds(ib_connect)
ib_order <- IBrokers::twsOrder(order_id,
  orderType="MKT", action="BUY", totalQuantity=1)
IBrokers::placeOrder(ib_connect, con_tract, ib_order)

# Request trade order ID
order_id <- IBrokers::reqIds(ib_connect)
# Create buy limit order object
ib_order <- IBrokers::twsOrder(order_id, orderType="LMT",
  lmtPrice="1.1511", action="BUY", totalQuantity=1)
# Place trade order
IBrokers::placeOrder(ib_connect, con_tract, ib_order)
# Cancel trade order
IBrokers::cancelOrder(ib_connect, order_id)
# Execute sell limit order
order_id <- IBrokers::reqIds(ib_connect)
ib_order <- IBrokers::twsOrder(order_id, orderType="LMT",
  lmtPrice="1.1512", action="SELL", totalQuantity=1)
IBrokers::placeOrder(ib_connect, con_tract, ib_order)
# Cancel trade order
IBrokers::cancelOrder(ib_connect, order_id)
# Close the Interactive Brokers API connection
IBrokers::twsDisconnect(ib_connect)

eWrapper_realtimebars <- function(n = 1) {
  eW <- eWrapper_new(NULL)
  # eW <- IBrokers::eWrapper(NULL)
  eW$assign.Data("data", rep(list(structure(.xts(matrix(rep(NA_real_, 7), ncol = 7), 0), .Dimnames = list(NULL, c("Open", "High", "Low", "Close", "Volume", "WAP", "Count")))), n))
  eW$realtimeBars <- function(curMsg, msg, timestamp, file, ...) {
    id <- as.numeric(msg[2])
    file <- file[[id]]
    data <- eW$get.Data("data")
    attr(data[[id]], "index") <- as.numeric(msg[3])
    nr.data <- NROW(data[[id]])
    # Write to file
    cat(paste(msg[3], msg[4], msg[5], msg[6], msg[7], msg[8], msg[9], msg[10], sep = ","), "\n", file = file, append = TRUE)
    # Write to console
    # eW$count_er <- eW$count_er + 1
    eW$assign.Data("count_er", eW$get.Data("count_er")+1)
    cat(paste0("count_er=", eW$get.Data("count_er"), "\tOpen=", msg[4], "\tHigh=", msg[5], "\tLow=", msg[6], "\tClose=", msg[7], "\tVolume=", msg[8]), "\n")
    # cat(paste0("Open=", msg[4], "\tHigh=", msg[5], "\tLow=", msg[6], "\tClose=", msg[7], "\tVolume=", msg[8]), "\n")
    #Trade
    # Cancel previous trade orders
    buy_id <- eW$get.Data("buy_id")
    sell_id <- eW$get.Data("sell_id")
    if (buy_id>0) IBrokers::cancelOrder(ib_connect, buy_id)
    if (sell_id>0) IBrokers::cancelOrder(ib_connect, sell_id)
    # Execute buy limit order
    buy_id <- IBrokers::reqIds(ib_connect)
    buy_order <- IBrokers::twsOrder(buy_id, orderType="LMT",
                              lmtPrice=msg[6]-0.25, action="BUY", totalQuantity=1)
    IBrokers::placeOrder(ib_connect, con_tract, buy_order)
    # Execute sell limit order
    sell_id <- IBrokers::reqIds(ib_connect)
    sell_order <- IBrokers::twsOrder(sell_id, orderType="LMT",
                               lmtPrice=msg[5]+0.25, action="SELL", totalQuantity=1)
    IBrokers::placeOrder(ib_connect, con_tract, sell_order)
    # Copy new trade orders
    eW$assign.Data("buy_id", buy_id)
    eW$assign.Data("sell_id", sell_id)
    #Trade finished
    data[[id]][nr.data, 1:7] <- as.numeric(msg[4:10])
    eW$assign.Data("data", data)
    c(curMsg, msg)
  }  # end eW$realtimeBars
  return(eW)
}  # end eWrapper_realtimebars

library(rutils)  # Load package rutils
etf_env <- new.env()  # new environment for data
# Download data for sym_bols into etf_env from Alpha Vantage
getSymbols.av(sym_bols, adjust=TRUE, env=etf_env,
  output.size="full", api.key="T7JPW54ES8G75310")
# getSymbols(sym_bols, env=etf_env, adjust=TRUE, from="2005-01-03")

library(rutils)  # Load package rutils
ls(etf_env)  # List files in etf_env
# get class of object in etf_env
class(get(x=sym_bols[1], envir=etf_env))
# Another way
class(etf_env$VTI)
colnames(etf_env$VTI)
head(etf_env$VTI, 3)
# get class of all objects in etf_env
eapply(etf_env, class)
# get class of all objects in R workspace
lapply(ls(), function(ob_ject) class(get(ob_ject)))

library(rutils)  # Load package rutils
# Check of object is an OHLC time series
is.OHLC(etf_env$VTI)
# Adjust single OHLC object using its name
etf_env$VTI <- adjustOHLC(etf_env$VTI,
                    use.Adjusted=TRUE)

# Adjust OHLC object using string as name
assign(sym_bols[1], adjustOHLC(
    get(x=sym_bols[1], envir=etf_env),
    use.Adjusted=TRUE),
  envir=etf_env)

# Adjust objects in environment using vector of strings
for (sym_bol in ls(etf_env)) {
  assign(sym_bol,
   adjustOHLC(get(sym_bol, envir=etf_env),
              use.Adjusted=TRUE),
   envir=etf_env)
}  # end for

library(rutils)  # Load package rutils
# Extract and cbind all data, subset by symbols
price_s <- rutils::do_call(cbind,
  as.list(etf_env)[sym_bols])
# Or
# price_s <- do.call(cbind,
#   as.list(etf_env)[sym_bols])
# Extract and cbind adjusted prices, subset by symbols
price_s <- rutils::do_call(cbind,
  lapply(as.list(etf_env)[sym_bols], Ad))
# Same, but works only for OHLC series
price_s <- rutils::do_call(cbind,
  eapply(etf_env, Ad)[sym_bols])
# Drop ".Adjusted" from colnames
colnames(price_s) <-
  sapply(colnames(price_s),
    function(col_name)
strsplit(col_name, split="[.]")[[1]],
    USE.NAMES=FALSE)[1, ]
head(price_s[, 1:2], 3)
# Which objects in global environment are class xts?
unlist(eapply(globalenv(), is.xts))
# Save xts to csv file
write.zoo(price_s,
  file="etf_series.csv", sep=",")
# Copy price_s into etf_env and save to .RData file
assign("price_s", price_s, envir=etf_env)
save(etf_env, file="etf_data.RData")

# Extract VTI prices
vt_i <- etf_env$price_s[ ,"VTI"]
vt_i <- na.omit(vt_i)
# Calculate percentage returns "by hand"
vti_lag <- as.numeric(vt_i)
vti_lag <- c(vti_lag[1], vti_lag[-NROW(vti_lag)])
vti_lag <- xts(vti_lag, index(vt_i))
vti_returns <- (vt_i-vti_lag)/vti_lag
# Calculate percentage returns using dailyReturn()
daily_returns <- quantmod::dailyReturn(vt_i)
head(cbind(daily_returns, vti_returns))
all.equal(daily_returns, vti_returns, check.attributes=FALSE)
# Calculate returns for all prices in etf_env$price_s
re_turns <- lapply(etf_env$price_s, function(x_ts) {
  daily_returns <- quantmod::dailyReturn(na.omit(x_ts))
  colnames(daily_returns) <- names(x_ts)
  daily_returns
})  # end lapply
# "re_turns" is a list of xts
class(re_turns)
class(re_turns[[1]])
# Flatten list of xts into a single xts
re_turns <- rutils::do_call(cbind, re_turns)
class(re_turns)
dim(re_turns)
# Copy re_turns into etf_env and save to .RData file
assign("re_turns", re_turns, envir=etf_env)
save(etf_env, file="etf_data.RData")

library(quantmod)
start_date <- "2012-05-10"; end_date <- "2013-11-20"
# Select all objects in environment and return as environment
new_env <- as.environment(eapply(etf_env, "[",
            paste(start_date, end_date, sep="/")))
# Select only sym_bols in environment and return as environment
new_env <- as.environment(
  lapply(as.list(etf_env)[sym_bols], "[",
   paste(start_date, end_date, sep="/")))
# Extract and cbind adjusted prices and return to environment
assign("price_s", rutils::do_call(cbind,
         lapply(ls(etf_env), function(sym_bol) {
           x_ts <- Ad(get(sym_bol, etf_env))
           colnames(x_ts) <- sym_bol
           x_ts
         })), envir=new_env)
# get sizes of OHLC xts series in etf_env
sapply(mget(sym_bols, envir=etf_env), object.size)
# Extract and cbind adjusted prices and return to environment
col_name <- function(x_ts)
  strsplit(colnames(x_ts), split="[.]")[[1]][1]
assign("price_s", rutils::do_call(cbind,
         lapply(mget(etf_env$sym_bols, envir=etf_env),
                function(x_ts) {
                  x_ts <- Ad(x_ts)
                  colnames(x_ts) <- col_name(x_ts)
                  x_ts
         })), envir=new_env)

# Load data frame of S&P500 constituents from CSV file
sp_500 <- read.csv(file="C:/Develop/lecture_slides/data/sp500_WRDS_08-30-17.csv", stringsAsFactors=FALSE)
# Inspect data frame of S&P500 constituents
dim(sp_500)
colnames(sp_500)
# Extract tickers from the column co_tic
sym_bols <- sp_500$co_tic
# Get duplicate tickers
ta_ble <- table(sym_bols)
dupli_cate <- ta_ble[ta_ble>1]
dupli_cate <- names(dupli_cate)
# Get duplicate records (rows) of sp_500
sp_500[sym_bols %in% dupli_cate, ]
# Get unique tickers
sym_bols <- unique(sym_bols)
# Find index of ticker "BRK.B"
which(sym_bols=="BRK.B")
# Remove "BRK.B" and later download it separately
sym_bols <- sym_bols[-which(sym_bols=="BRK.B")]

# Load package rutils
library(rutils)
# Create new environment for data
env_sp500 <- new.env()
# Boolean vector of symbols already downloaded
down_loaded <- sym_bols %in% ls(env_sp500)
# Download in while loop from Tiingo and copy into environment
at_tempt <- 0  # number of download attempts
while (((sum(!down_loaded)) > 0) & (at_tempt<5)) {
  # Download data and copy it into environment
  at_tempt <- at_tempt + 1
  cat("Download attempt = ", at_tempt, "\n")
  for (sym_bol in sym_bols[!down_loaded]) {
    cat("processing: ", sym_bol, "\n")
    tryCatch(  # With error handler
getSymbols(sym_bol, src="tiingo", adjust=TRUE, auto.assign=TRUE,
           from="1990-01-01", env=env_sp500, api.key="d84fc2a9c5bde2d68e33034f65a838092c6b9f10"),
# Error handler captures error condition
error=function(error_cond) {
  print(paste("error handler: ", error_cond))
},  # end error handler
finally=print(paste("sym_bol=", sym_bol))
    )  # end tryCatch
  }  # end for
  # Update vector of symbols already downloaded
  down_loaded <- sym_bols %in% ls(env_sp500)
  Sys.sleep(10)  # Wait 10 seconds until next attempt
}  # end while
class(env_sp500$AAPL)
class(index(env_sp500$AAPL))

library(quantmod)
# Rename "LOW" colnames to "LO_WES"
colnames(env_sp500$LOW) <- paste("LO_WES",
  sapply(strsplit(colnames(env_sp500$LOW), split="[.]"),
   function(col_name) col_name[2]), sep=".")
env_sp500$LO_WES <- env_sp500$LOW[, unique(colnames(env_sp500$LOW))]
rm(LOW, envir=env_sp500)
chart_Series(x=env_sp500$LO_WES["2017-06/"],
  TA="add_Vo()", name="LOWES stock")
# Download "BRK.B" separately with auto.assign=FALSE
BRK_B <- getSymbols("BRK-B", auto.assign=FALSE, src="tiingo", adjust=TRUE, from="1990-01-01", api.key="j84ac2b9c5bde2d68e33034f65d838092c6c9f10")
colnames(BRK_B) <- paste("BRK_B",
  sapply(strsplit(colnames(BRK_B), split="[.]"),
   function(col_name) col_name[2]), sep=".")
env_sp500$BRK_B <- BRK_B

# Rename "BF-B" colnames to "BF_B"
colnames(env_sp500$"BF-B") <- paste("BF_B",
  sapply(strsplit(colnames(env_sp500$"BF-B"), split="[.]"),
   function(col_name) col_name[2]), sep=".")
names(colnames(env_sp500$"BF-B")) <- NULL
env_sp500$BF_B <- env_sp500$"BF-B"
rm("BF-B", envir=env_sp500)

class(env_sp500$AAPL)
# The date-time index is class POSIXct not Date
class(index(env_sp500$AAPL))
# Coerce time indices from class POSIXct to class Date
for (sym_bol in ls(env_sp500)) {
  x_ts <- get(sym_bol, envir=env_sp500)
  index(x_ts) <- as.Date(index(x_ts))
  assign(sym_bol, x_ts, envir=env_sp500)
}  # end for
class(index(env_sp500$AAPL))
# Save the environment to compressed .RData file
dir_name <- "C:/Develop/lecture_slides/data/"
save(env_sp500, file=paste0(dir_name, "sp500.RData"))
# Save the ETF prices into CSV files
dir_name <- "C:/Develop/lecture_slides/data/SP500/"
for (sym_bol in ls(env_sp500)) {
  zoo::write.zoo(env_sp500$sym_bol, file=paste0(dir_name, sym_bol, ".csv"))
}  # end for
# Or using lapply()
file_names <- lapply(ls(env_sp500), function(sym_bol) {
  x_ts <- get(sym_bol, envir=env_sp500)
  zoo::write.zoo(x_ts, file=paste0(dir_name, sym_bol, ".csv"))
  sym_bol
})  # end lapply
unlist(file_names)
# Or using eapply() and data.table::fwrite()
file_names <- eapply(env_sp500 , function(x_ts) {
  file_name <- rutils::get_name(colnames(x_ts)[1])
  data.table::fwrite(data.table::as.data.table(x_ts), file=paste0(dir_name, file_name, ".csv"))
  file_name
})  # end eapply
unlist(file_names)

# Load the environment from compressed .RData file
dir_name <- "C:/Develop/lecture_slides/data/"
load(file=paste0(dir_name, "sp500.RData"))
# Get all the .csv file names in the directory
dir_name <- "C:/Develop/lecture_slides/data/SP500/"
file_names <- Sys.glob(paste0(dir_name, "*.csv"))
# Create new environment for data
env_sp500 <- new.env()
for (file_name in file_names) {
  x_ts <- xts::as.xts(zoo::read.csv.zoo(file_name))
  sym_bol <- strsplit(colnames(x_ts), split="[.]")[[1]][1]
  assign(sym_bol, x_ts, envir=env_sp500)
}  # end for
# Or using fread()
for (file_name in file_names) {
  x_ts <- data.table::fread(file_name)
  data.table::setDF(x_ts)
  x_ts <- xts::xts(x_ts[, -1], as.Date(x_ts[, 1]))
  sym_bol <- strsplit(colnames(x_ts), split="[.]")[[1]][1]
  assign(sym_bol, x_ts, envir=env_sp500)
}  # end for

# Remove all files from environment(if necessary)
rm(list=ls(env_sp500), envir=env_sp500)
# Download in while loop from Alpha Vantage and copy into environment
down_loaded <- sym_bols %in% ls(env_sp500)
at_tempt <- 0
while (((sum(!down_loaded)) > 0) & (at_tempt<10)) {
  # Download data and copy it into environment
  at_tempt <- at_tempt + 1
  for (sym_bol in sym_bols[!down_loaded]) {
    cat("processing: ", sym_bol, "\n")
    tryCatch(  # With error handler
getSymbols(sym_bol, src="av", adjust=TRUE, auto.assign=TRUE, env=env_sp500,
           output.size="full", api.key="T7JPW54ES8G75310"),
# error handler captures error condition
error=function(error_cond) {
  print(paste("error handler: ", error_cond))
},  # end error handler
finally=print(paste("sym_bol=", sym_bol))
    )  # end tryCatch
  }  # end for
  # Update vector of symbols already downloaded
  down_loaded <- sym_bols %in% ls(env_sp500)
  Sys.sleep(10)  # Wait 10 seconds until next attempt
}  # end while
# Adjust all OHLC prices in environment
for (sym_bol in ls(env_sp500)) {
  assign(sym_bol,
    adjustOHLC(get(x=sym_bol, envir=env_sp500), use.Adjusted=TRUE),
    envir=env_sp500)
}  # end for

library(rutils)  # Load package rutils
# Assign name SP500 to ^GSPC symbol
setSymbolLookup(
  SP500=list(name="^GSPC", src="yahoo"))
getSymbolLookup()
# view and clear options
options("getSymbols.sources")
options(getSymbols.sources=NULL)
# Download S&P500 prices into etf_env
getSymbols("SP500", env=etf_env,
    adjust=TRUE, auto.assign=TRUE, from="1990-01-01")
chart_Series(x=etf_env$SP500["2016/"],
       TA="add_Vo()",
       name="S&P500 index")

library(rutils)  # Load package rutils
# Assign name DJIA to ^DJI symbol
setSymbolLookup(
  DJIA=list(name="^DJI", src="yahoo"))
getSymbolLookup()
# view and clear options
options("getSymbols.sources")
options(getSymbols.sources=NULL)
# Download DJIA prices into etf_env
getSymbols("DJIA", env=etf_env,
    adjust=TRUE, auto.assign=TRUE, from="1990-01-01")
chart_Series(x=etf_env$DJIA["2016/"],
       TA="add_Vo()",
       name="DJIA index")

library(rutils)  # Load package rutils
library(RCurl)  # Load package RCurl
library(XML)  # Load package XML
# Download text data from URL
sp_500 <- getURL(
  "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies")
# Extract tables from the text data
sp_500 <- readHTMLTable(sp_500,
              stringsAsFactors=FALSE)
str(sp_500)
# Extract colnames of data frames
lapply(sp_500, colnames)
# Extract S&P500 constituents
sp_500 <- sp_500[[1]]
head(sp_500)
# Create valid R names from symbols containing "-" or "."characters
sp_500$names <- gsub("-", "_", sp_500$Ticker)
sp_500$names <- gsub("[.]", "_", sp_500$names)
# Write data frame of S&P500 constituents to CSV file
write.csv(sp_500,
  file="C:/Develop/lecture_slides/data/sp500_Yahoo.csv",
  row.names=FALSE)

library(rutils)  # Load package rutils
# Load data frame of S&P500 constituents from CSV file
sp_500 <- read.csv(file="C:/Develop/lecture_slides/data/sp500_Yahoo.csv",
     stringsAsFactors=FALSE)
# Register symbols corresponding to R names
for (in_dex in 1:NROW(sp_500)) {
  cat("processing: ", sp_500$Ticker[in_dex], "\n")
  setSymbolLookup(structure(
    list(list(name=sp_500$Ticker[in_dex])),
    names=sp_500$names[in_dex]))
}  # end for
env_sp500 <- new.env()  # new environment for data
# Remove all files (if necessary)
rm(list=ls(env_sp500), envir=env_sp500)
# Download data and copy it into environment
rutils::get_symbols(sp_500$names,
   env_out=env_sp500, start_date="1990-01-01")
# Or download in loop
for (sym_bol in sp_500$names) {
  cat("processing: ", sym_bol, "\n")
  rutils::get_symbols(sym_bol,
   env_out=env_sp500, start_date="1990-01-01")
}  # end for
save(env_sp500, file="C:/Develop/lecture_slides/data/sp500.RData")
chart_Series(x=env_sp500$BRK_B["2016/"], TA="add_Vo()",
       name="BRK-B stock")

library(quantmod)
# Download U.S. unemployment rate data
unemp_rate <- getSymbols("UNRATE",
            auto.assign=FALSE,
            src="FRED")
# Plot U.S. unemployment rate data
chart_Series(unemp_rate["1990/"],
      name="U.S. unemployment rate")

library(rutils)  # Load package rutils
install.packages("devtools")
library(devtools)
# Install package Quandl from github
install_github("quandl/R-package")
library(Quandl)  # Load package Quandl
# Register Quandl API key
Quandl.api_key("pVJi9Nv3V8CD3Js5s7Qx")
# get short description
packageDescription("Quandl")
# Load help page
help(package="Quandl")
# Remove Quandl from search path
detach("package:Quandl")

library(rutils)  # Load package rutils
# Download EOD AAPL prices from WIKI free database
price_s <- Quandl(code="WIKI/AAPL",
            type="xts", start_date="1990-01-01")
x11(width=14, height=7)
chart_Series(price_s["2016", 1:4],
    name="AAPL OHLC prices")
# Add trade volume in extra panel
add_TA(price_s["2016", 5])
# Download euro currency rates
price_s <- Quandl(code="BNP/USDEUR",
    start_date="2013-01-01",
    end_date="2013-12-01", type="xts")
# Download multiple time series
price_s <- Quandl(code=c("NSE/OIL", "WIKI/AAPL"),
    start_date="2013-01-01", type="xts")
# Download AAPL gross profits
prof_it <- Quandl("RAYMOND/AAPL_GROSS_PROFIT_Q",
    type="xts")
chart_Series(prof_it, name="AAPL gross profits")
# Download Hurst time series
price_s <- Quandl(code="PE/AAPL_HURST",
    start_date="2013-01-01", type="xts")
chart_Series(price_s["2016/", 1],
       name="AAPL Hurst")

library(rutils)  # Load package rutils
# Load S&P500 stock Quandl codes
sp_500 <- read.csv(
  file="C:/Develop/lecture_slides/data/sp500_quandl.csv",
  stringsAsFactors=FALSE)
# Replace "-" with "_" in symbols
sp_500$free_code <-
  gsub("-", "_", sp_500$free_code)
head(sp_500)
# vector of symbols in sp_500 frame
tick_ers <- gsub("-", "_", sp_500$ticker)
# Or
tick_ers <- matrix(unlist(
  strsplit(sp_500$free_code, split="/"),
  use.names=FALSE), ncol=2, byrow=TRUE)[, 2]
# Or
tick_ers <- do_call_rbind(
  strsplit(sp_500$free_code, split="/"))[, 2]

library(rutils)  # Load package rutils
env_sp500 <- new.env()  # new environment for data
# Remove all files (if necessary)
rm(list=ls(env_sp500), envir=env_sp500)
# Boolean vector of symbols already downloaded
down_loaded <- tick_ers %in% ls(env_sp500)
# Download data and copy it into environment
for (tick_er in tick_ers[!down_loaded]) {
  cat("processing: ", tick_er, "\n")
  da_ta <- Quandl(code=paste0("WIKI/", tick_er),
            start_date="1990-01-01",
            type="xts")[, -(1:7)]
  colnames(da_ta) <- paste(tick_er,
    c("Open", "High", "Low", "Close", "Volume"), sep=".")
  assign(tick_er, da_ta, envir=env_sp500)
}  # end for
save(env_sp500, file="C:/Develop/lecture_slides/data/sp500.RData")
chart_Series(x=env_sp500$XOM["2016/"], TA="add_Vo()",
       name="XOM stock")

library(rutils)
library(Quandl)
# Register Quandl API key
Quandl.api_key("pVJi9Nv3V8CD3Js5s7Qx")
# Download E-mini S&P500 futures prices
price_s <- Quandl(code="CHRIS/CME_ES1",
  type="xts", start_date="1990-01-01")
price_s <- price_s[, c("Open", "High", "Low", "Last", "Volume")]
colnames(price_s)[4] <- "Close"
# Plot the prices
x11(width=5, height=4)  # Open x11 for plotting
chart_Series(x=price_s["2008-06/2009-06"],
       TA="add_Vo()",
       name="S&P500 Futures")
# Plot dygraph
dygraphs::dygraph(price_s["2008-06/2009-06", -5],
  main="S&P500 Futures") %>%
  dyCandlestick()

# Read CBOE futures expiration dates
date_s <- read.csv(file="C:/Develop/lecture_slides/data/futures_expiration_dates_codes.csv",
  stringsAsFactors=FALSE, row.names=1)
dir_name <- "C:/Develop/data/vix_data"
dir.create(dir_name)
sym_bols <- rownames(date_s)
file_names <- file.path(dir_name, paste0(sym_bols, ".csv"))
log_file <- file.path(dir_name, "log_file.txt")
cboe_url <- "https://markets.cboe.com/us/futures/market_statistics/historical_data/products/csv/VX/"
url_s <- paste0(cboe_url, date_s[, 1])
# Download files in loop
for (it in seq_along(url_s)) {
    tryCatch(  # Warning and error handler
  download.file(url_s[it],
          destfile=file_names[it], quiet=TRUE),
# Warning handler captures warning condition
warning=function(warning_cond) {
  cat(paste("warning handler: ", warning_cond, "\n"), file=log_file, append=TRUE)
},  # end warning handler
# Error handler captures error condition
error=function(error_cond) {
  cat(paste("error handler: ", error_cond, "\n"), append=TRUE)
},  # end error handler
finally=cat(paste("Processing file name =", file_names[it], "\n"), append=TRUE)
    )  # end tryCatch
}  # end for

# Create new environment for data
vix_env <- new.env()
# Download VIX data for the months 6, 7, and 8 in 2018
library(qmao)
quantmod::getSymbols("VX", Months=1:12,
  Years=2018, src="cfe", auto.assign=TRUE, env=vix_env)
# Or
qmao::getSymbols.cfe(Symbols="VX",
  Months=6:8, Years=2018, env=vix_env,
  verbose=FALSE, auto.assign=TRUE)
# Calculate the classes of all the objects
# In the environment vix_env
unlist(eapply(vix_env,
  function(x) {class(x)[1]}))
class(vix_env$VX_M18)
colnames(vix_env$VX_M18)
# Save the data to a binary file called "vix_cboe.RData".
save(vix_env,
  file="C:/Develop/data/vix_data/vix_cboe.RData")
