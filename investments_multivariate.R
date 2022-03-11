# Load constant maturity Treasury rates
load(file="/Users/jerzy/Develop/lecture_slides/data/rates_data.RData")
# Combine rates into single xts series
rate_s <- do.call(cbind, as.list(rates_env))
# Sort the columns of rate_s according bond maturity
name_s <- colnames(rate_s)
name_s <- substr(name_s, start=4, stop=10)
name_s <- as.numeric(name_s)
indeks <- order(name_s)
rate_s <- rate_s[, indeks]
# Align rates dates with VTI prices
clos_e <- log(quantmod::Cl(rutils::etfenv$VTI))
colnames(clos_e) <- "VTI"
n_rows <- NROW(clos_e)
date_s <- zoo::index(clos_e)
rate_s <- na.omit(rate_s[date_s])
clos_e <- clos_e[zoo::index(rate_s)]
date_s <- zoo::index(clos_e)
# Calculate VTI returns and IR changes
re_turns <- rutils::diff_it(clos_e)
rates_diff <- rutils::diff_it(log(rate_s))
# Regress VTI returns versus the lagged rate differences
predic_tor <- rutils::lag_it(rates_diff)
mod_el <- lm(re_turns ~ predic_tor)
summary(mod_el)
# Regress VTI returns before and after 2012
summary(lm(re_turns["/2012"] ~ predic_tor["/2012"]))
summary(lm(re_turns["2012/"] ~ predic_tor["2012/"]))
# Calculate PCA of rates correlation matrix
ei_gen <- eigen(cor(rates_diff))
rates_pca <- -rates_diff %*% ei_gen$vectors
colnames(rates_pca) <- paste0("PC", 1:6)
# Define predictor as the YC PCAs
predic_tor <- rutils::lag_it(rates_pca)
mod_el <- lm(re_turns ~ predic_tor)
summary(mod_el)
# Plot YC steepener principal component with VTI
da_ta <- cbind(re_turns, rates_pca[, 2])
colnames(da_ta) <- c("VTI", "Steepener")
col_names <- colnames(da_ta)
dygraphs::dygraph(cumsum(da_ta), main="VTI and Yield Curve Steepener") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=1, col="blue") %>%
  dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=1, col="red")
# Define predictor with intercept term
predic_tor <- rutils::lag_it(rates_diff)
predic_tor <- cbind(rep(1, NROW(predic_tor)), predic_tor)
colnames(predic_tor)[1] <- "intercept"
# Calculate inverse of predictor
in_verse <- MASS::ginv(predic_tor)
# Calculate coefficients from response and inverse of predictor
res_ponse <- re_turns
co_eff <- drop(in_verse %*% res_ponse)
# Calculate forecasts and pnls in-sample
forecast_s <- (predic_tor %*% co_eff)
pnl_s <- sign(forecast_s)*res_ponse
# Calculate in-sample factors
factor_s <- (predic_tor * co_eff)
apply(factor_s, 2, sd)
# Plot dygraph of in-sample IR strategy
weal_th <- cbind(re_turns, pnl_s)
colnames(weal_th) <- c("VTI", "Strategy")
col_names <- colnames(weal_th)
dygraphs::dygraph(cumsum(weal_th), main="Yield Curve Strategy In-sample") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=col_names[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Define in-sample and out-of-sample intervals
in_sample <- (date_s < as.Date("2020-01-01"))
out_sample <- (date_s >= as.Date("2020-01-01"))
# Calculate inverse of predictor in-sample
in_verse <- MASS::ginv(predic_tor[in_sample, ])
# Calculate coefficients in-sample
co_eff <- drop(in_verse %*% res_ponse[in_sample, ])
# Calculate forecasts and pnls out-of-sample
forecast_s <- (predic_tor[out_sample, ] %*% co_eff)
pnl_s <- sign(forecast_s)*res_ponse[out_sample, ]
# Plot dygraph of out-of-sample IR PCA strategy
weal_th <- cbind(re_turns[out_sample, ], pnl_s)
colnames(weal_th) <- c("VTI", "Strategy")
col_names <- colnames(weal_th)
dygraphs::dygraph(cumsum(weal_th), main="Yield Curve Strategy Out-of-Sample") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=col_names[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Define yearly dates
format(date_s[1], "%Y")
year_s <- paste0(seq(2001, 2022, 1), "-01-01")
year_s <- as.Date(year_s)
# Perform loop over yearly dates
pnl_s <- lapply(3:(NROW(year_s)-1), function(i) {
  # Define in-sample and out-of-sample intervals
  in_sample <- (date_s > year_s[i-1]) & (date_s < year_s[i])
  out_sample <- (date_s >= year_s[i]) & (date_s < year_s[i+1])
  # Calculate coefficients in-sample
  in_verse <- MASS::ginv(predic_tor[in_sample, ])
  co_eff <- drop(in_verse %*% res_ponse[in_sample, ])
  # Calculate forecasts and pnls out-of-sample
  forecast_s <- (predic_tor[out_sample, ] %*% co_eff)
  sign(forecast_s)*res_ponse[out_sample, ]
})  # end lapply
pnl_s <- do.call(rbind, pnl_s)
# Plot dygraph of rolling yearly IR strategy
vt_i <- rutils::diff_it(clos_e[zoo::index(pnl_s),])
weal_th <- cbind(vt_i, pnl_s)
colnames(weal_th) <- c("VTI", "Strategy")
col_names <- colnames(weal_th)
dygraphs::dygraph(cumsum(weal_th), main="Rolling Yearly Yield Curve Strategy") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=col_names[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Define monthly dates
format(date_s[1], "%m-%Y")
format(date_s[NROW(date_s)], "%m-%Y")
month_s <- seq.Date(from=as.Date("2001-05-01"), to=as.Date("2021-04-01"), by="month")
# Perform loop over monthly dates
pnl_s <- lapply(12:(NROW(month_s)-1), function(i) {
  # Define in-sample and out-of-sample intervals
  in_sample <- (date_s > month_s[i-11]) & (date_s < month_s[i])
  out_sample <- (date_s > month_s[i]) & (date_s < month_s[i+1])
  # Calculate forecasts and pnls out-of-sample
  in_verse <- MASS::ginv(predic_tor[in_sample, ])
  co_eff <- drop(in_verse %*% res_ponse[in_sample, ])
  forecast_s <- (predic_tor[out_sample, ] %*% co_eff)
  sign(forecast_s)*res_ponse[out_sample, ]
})  # end lapply
pnl_s <- do.call(rbind, pnl_s)
# Plot dygraph of rolling monthly IR strategy
vt_i <- rutils::diff_it(clos_e[zoo::index(pnl_s),])
weal_th <- cbind(vt_i, pnl_s)
colnames(weal_th) <- c("VTI", "Strategy")
col_names <- colnames(weal_th)
dygraphs::dygraph(cumsum(weal_th), main="Rolling Monthly Yield Curve Strategy") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=col_names[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Define weekly dates
week_s <- seq.Date(from=as.Date("2001-05-01"), to=as.Date("2021-04-01"), by="weeks")
# Perform loop over weekly dates
pnl_s <- lapply(51:(NROW(week_s)-1), function(i) {
  # Define in-sample and out-of-sample intervals
  in_sample <- (date_s > week_s[i-10]) & (date_s < week_s[i])
  out_sample <- (date_s > week_s[i]) & (date_s < week_s[i+1])
  # Calculate forecasts and pnls out-of-sample
  in_verse <- MASS::ginv(predic_tor[in_sample, ])
  co_eff <- drop(in_verse %*% res_ponse[in_sample, ])
  forecast_s <- (predic_tor[out_sample, ] %*% co_eff)
  sign(forecast_s)*res_ponse[out_sample, ]
})  # end lapply
pnl_s <- do.call(rbind, pnl_s)
# Plot dygraph of rolling weekly IR strategy
vt_i <- rutils::diff_it(clos_e[zoo::index(pnl_s),])
weal_th <- cbind(vt_i, pnl_s)
colnames(weal_th) <- c("VTI", "Strategy")
col_names <- colnames(weal_th)
dygraphs::dygraph(cumsum(weal_th), main="Rolling Weekly Yield Curve Strategy") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=col_names[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Calculate singular value decomposition of the predictor matrix
s_vd <- svd(predic_tor)
barplot(s_vd$d, main="Singular Values of YC Predictor Matrix")
# Calculate generalized inverse from SVD
in_verse <- s_vd$v %*% (t(s_vd$u) / s_vd$d)
# Verify inverse property of in_verse
all.equal(zoo::coredata(predic_tor),
    predic_tor %*% in_verse %*% predic_tor)
# Calculate generalized inverse using MASS::ginv()
inverse_ginv <- MASS::ginv(predic_tor)
all.equal(inverse_ginv, in_verse)
# Set tolerance for determining zero singular values
to_l <- sqrt(.Machine$double.eps)
# Check for zero singular values
round(s_vd$d, 12)
not_zero <- (s_vd$d > (to_l * s_vd$d[1]))
# Calculate regularized inverse from SVD
inv_reg <- s_vd$v[, not_zero] %*%
  (t(s_vd$u[, not_zero]) / s_vd$d[not_zero])
# Verify inverse property of inv_reg
all.equal(zoo::coredata(predic_tor),
    predic_tor %*% inv_reg %*% predic_tor)
all.equal(inv_reg, in_verse)
# Calculate shrinkage inverse from SVD
eigen_max <- 3
inv_shrink <- s_vd$v[, 1:eigen_max] %*%
  (t(s_vd$u[, 1:eigen_max]) / s_vd$d[1:eigen_max])
# Inverse property fails for inv_shrink
all.equal(zoo::coredata(predic_tor),
    predic_tor %*% inv_shrink %*% predic_tor)
# Calculate shrinkage inverse using RcppArmadillo
inverse_rcpp <- HighFreq::calc_inv(predic_tor, eigen_max=eigen_max)
all.equal(inv_shrink, inverse_rcpp, check.attributes=FALSE)
# Calculate in-sample pnls for different eigen_max values
eigen_maxs <- 2:7
pnl_s <- lapply(eigen_maxs, function(eigen_max) {
  in_verse <- HighFreq::calc_inv(predic_tor, eigen_max=eigen_max)
  co_eff <- drop(in_verse %*% res_ponse)
  forecast_s <- (predic_tor %*% co_eff)
  sign(forecast_s)*res_ponse
})
pnl_s <- do.call(cbind, pnl_s)
colnames(pnl_s) <- paste0("eigen", eigen_maxs)
# Plot dygraph of in-sample pnls
col_ors <- colorRampPalette(c("blue", "red"))(NCOL(pnl_s))
dygraphs::dygraph(cumsum(pnl_s), main="In-Sample Returns of Shrinkage YC Strategies") %>%
  dyOptions(colors=col_ors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Define in-sample and out-of-sample intervals
in_sample <- (date_s < as.Date("2020-01-01"))
out_sample <- (date_s >= as.Date("2020-01-01"))
# Calculate in-sample pnls for different eigen_max values
eigen_maxs <- 2:7
pnl_s <- lapply(eigen_maxs, function(x) {
  in_verse <- HighFreq::calc_inv(predic_tor[in_sample, ], eigen_max=x)
  co_eff <- drop(in_verse %*% res_ponse[in_sample, ])
  forecast_s <- (predic_tor[out_sample, ] %*% co_eff)
  sign(forecast_s)*res_ponse[out_sample, ]
})
pnl_s <- do.call(cbind, pnl_s)
colnames(pnl_s) <- paste0("eigen", eigen_maxs)
# Plot dygraph of out-of-sample pnls
col_ors <- colorRampPalette(c("blue", "red"))(NCOL(pnl_s))
dygraphs::dygraph(cumsum(pnl_s), main="Out-of-Sample Returns of Shrinkage YC Strategies") %>%
  dyOptions(colors=col_ors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Define monthly dates
format(date_s[1], "%m-%Y")
format(date_s[NROW(date_s)], "%m-%Y")
month_s <- seq.Date(from=as.Date("2001-05-01"), to=as.Date("2021-04-01"), by="month")
# Perform loop over monthly dates
look_back <- 6
eigen_max <- 3
pnl_s <- lapply((look_back+1):(NROW(month_s)-1), function(i) {
  # Define in-sample and out-of-sample intervals
  in_sample <- (date_s > month_s[i-look_back]) & (date_s < month_s[i])
  out_sample <- (date_s > month_s[i]) & (date_s < month_s[i+1])
  # Calculate forecasts and pnls out-of-sample
  in_verse <- HighFreq::calc_inv(predic_tor[in_sample, ], eigen_max=eigen_max)
  co_eff <- drop(in_verse %*% res_ponse[in_sample, ])
  forecast_s <- (predic_tor[out_sample, ] %*% co_eff)
  sign(forecast_s)*res_ponse[out_sample, ]
})  # end lapply
pnl_s <- do.call(rbind, pnl_s)
# Plot dygraph of rolling monthly IR strategy
vt_i <- rutils::diff_it(clos_e[zoo::index(pnl_s),])
weal_th <- cbind(vt_i, pnl_s)
colnames(weal_th) <- c("VTI", "Strategy")
col_names <- colnames(weal_th)
dygraphs::dygraph(cumsum(weal_th), main="Rolling Monthly Shrinkage YC Strategy") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=col_names[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Define weekly dates
week_s <- seq.Date(from=as.Date("2001-05-01"), to=as.Date("2021-04-01"), by="weeks")
# Perform loop over weekly dates
look_back <- 4
eigen_max <- 4
pnl_s <- lapply((look_back+1):(NROW(week_s)-1), function(i) {
  # Define in-sample and out-of-sample intervals
  in_sample <- (date_s > week_s[i-look_back]) & (date_s < week_s[i])
  out_sample <- (date_s > week_s[i]) & (date_s < week_s[i+1])
  # Calculate forecasts and pnls out-of-sample
  in_verse <- HighFreq::calc_inv(predic_tor[in_sample, ], eigen_max=eigen_max)
  co_eff <- drop(in_verse %*% res_ponse[in_sample, ])
  forecast_s <- (predic_tor[out_sample, ] %*% co_eff)
  sign(forecast_s)*res_ponse[out_sample, ]
})  # end lapply
pnl_s <- do.call(rbind, pnl_s)
# Plot dygraph of rolling weekly IR strategy
vt_i <- rutils::diff_it(clos_e[zoo::index(pnl_s),])
weal_th <- cbind(vt_i, pnl_s)
colnames(weal_th) <- c("VTI", "Strategy")
col_names <- colnames(weal_th)
dygraphs::dygraph(cumsum(weal_th), main="Rolling Weekly Shrinkage YC Strategy") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=col_names[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Load the yield curve data
load(file="/Users/jerzy/Develop/lecture_slides/data/rates_data.RData")
rate_s <- do.call(cbind, as.list(rates_env))
name_s <- colnames(rate_s)
name_s <- substr(name_s, start=4, stop=10)
name_s <- as.numeric(name_s)
indeks <- order(name_s)
rate_s <- rate_s[, indeks]
clos_e <- log(quantmod::Cl(rutils::etfenv$VTI))
colnames(clos_e) <- "VTI"
n_rows <- NROW(clos_e)
date_s <- zoo::index(clos_e)
rate_s <- na.omit(rate_s[date_s])
clos_e <- clos_e[zoo::index(rate_s)]
date_s <- zoo::index(clos_e)
re_turns <- rutils::diff_it(clos_e)
rates_diff <- rutils::diff_it(log(rate_s))
# Create a combined predictor matrix
order_max <- 5
predic_tor <- sapply(1:order_max, rutils::lag_it, in_put=as.numeric(re_turns))
colnames(predic_tor) <- paste0("retslag", 1:NCOL(predic_tor))
predic_tor <- cbind(predic_tor, rutils::lag_it(rates_diff))
predic_tor <- cbind(rep(1, NROW(predic_tor)), predic_tor)
colnames(predic_tor)[1] <- "intercept"
res_ponse <- re_turns
# Calculate in-sample pnls for different eigen_max values
eigen_maxs <- 2:11
pnl_s <- lapply(eigen_maxs, function(eigen_max) {
  in_verse <- HighFreq::calc_inv(predic_tor, eigen_max=eigen_max)
  co_eff <- drop(in_verse %*% res_ponse)
  forecast_s <- (predic_tor %*% co_eff)
  sign(forecast_s)*res_ponse
})
pnl_s <- do.call(cbind, pnl_s)
colnames(pnl_s) <- paste0("eigen", eigen_maxs)
# Plot dygraph of in-sample pnls
col_ors <- colorRampPalette(c("blue", "red"))(NCOL(pnl_s))
dygraphs::dygraph(cumsum(pnl_s), main="In-Sample Returns of Combined Strategies With Shrinkage") %>%
  dyOptions(colors=col_ors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Define in-sample and out-of-sample intervals
in_sample <- (date_s < as.Date("2020-01-01"))
out_sample <- (date_s >= as.Date("2020-01-01"))
# Calculate in-sample pnls for different eigen_max values
eigen_maxs <- 2:11
pnl_s <- lapply(eigen_maxs, function(x) {
  in_verse <- HighFreq::calc_inv(predic_tor[in_sample, ], eigen_max=x)
  co_eff <- drop(in_verse %*% res_ponse[in_sample, ])
  forecast_s <- (predic_tor[out_sample, ] %*% co_eff)
  sign(forecast_s)*res_ponse[out_sample, ]
})
pnl_s <- do.call(cbind, pnl_s)
colnames(pnl_s) <- paste0("eigen", eigen_maxs)
# Plot dygraph of out-of-sample pnls
col_ors <- colorRampPalette(c("blue", "red"))(NCOL(pnl_s))
dygraphs::dygraph(cumsum(pnl_s), main="Out-of-Sample Returns of Combined Strategies With Shrinkage") %>%
  dyOptions(colors=col_ors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Define monthly dates
format(date_s[1], "%m-%Y")
format(date_s[NROW(date_s)], "%m-%Y")
month_s <- seq.Date(from=as.Date("2001-05-01"), to=as.Date("2021-04-01"), by="month")
# Perform loop over monthly dates
look_back <- 6
eigen_max <- 3
pnl_s <- lapply((look_back+1):(NROW(month_s)-1), function(i) {
  # Define in-sample and out-of-sample intervals
  in_sample <- (date_s > month_s[i-look_back]) & (date_s < month_s[i])
  out_sample <- (date_s > month_s[i]) & (date_s < month_s[i+1])
  # Calculate forecasts and pnls out-of-sample
  in_verse <- HighFreq::calc_inv(predic_tor[in_sample, ], eigen_max=eigen_max)
  co_eff <- drop(in_verse %*% res_ponse[in_sample, ])
  forecast_s <- (predic_tor[out_sample, ] %*% co_eff)
  sign(forecast_s)*res_ponse[out_sample, ]
})  # end lapply
pnl_s <- do.call(rbind, pnl_s)
# Plot dygraph of rolling monthly IR strategy
vt_i <- rutils::diff_it(clos_e[zoo::index(pnl_s),])
weal_th <- cbind(vt_i, pnl_s)
colnames(weal_th) <- c("VTI", "Strategy")
col_names <- colnames(weal_th)
dygraphs::dygraph(cumsum(weal_th), main="Rolling Monthly Shrinkage YC Strategy") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=col_names[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Define weekly dates
week_s <- seq.Date(from=as.Date("2001-05-01"), to=as.Date("2021-04-01"), by="weeks")
# Perform loop over weekly dates
look_back <- 8
eigen_max <- 4
pnl_s <- lapply((look_back+1):(NROW(week_s)-1), function(i) {
  # Define in-sample and out-of-sample intervals
  in_sample <- (date_s > week_s[i-look_back]) & (date_s < week_s[i])
  out_sample <- (date_s > week_s[i]) & (date_s < week_s[i+1])
  # Calculate forecasts and pnls out-of-sample
  in_verse <- HighFreq::calc_inv(predic_tor[in_sample, ], eigen_max=eigen_max)
  co_eff <- drop(in_verse %*% res_ponse[in_sample, ])
  forecast_s <- (predic_tor[out_sample, ] %*% co_eff)
  sign(forecast_s)*res_ponse[out_sample, ]
})  # end lapply
pnl_s <- do.call(rbind, pnl_s)
# Plot dygraph of rolling weekly IR strategy
vt_i <- rutils::diff_it(clos_e[zoo::index(pnl_s),])
weal_th <- cbind(vt_i, pnl_s)
colnames(weal_th) <- c("VTI", "Strategy")
col_names <- colnames(weal_th)
dygraphs::dygraph(cumsum(weal_th), main="Rolling Weekly Shrinkage YC Strategy") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=col_names[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Find optimal n_agg for predictor
n_aggs <- 5:100
tvalues <- sapply(n_aggs, function(n_agg) {
  predic_tor <- roll::roll_mean(rates_diff, width=n_agg, min_obs=1)
  predic_tor <- cbind(rep(1, NROW(predic_tor)), predic_tor)
  predic_tor <- rutils::lag_it(predic_tor)
  mod_el <- lm(res_ponse ~ predic_tor - 1)
  model_sum <- summary(mod_el)
  max(abs(model_sum$coefficients[, 3][-1]))
})  # end sapply
n_aggs[which.max(tvalues)]
plot(n_aggs, tvalues, t="l", col="blue", lwd=2)
# Calculate aggregated predictor
n_agg <- 53
predic_tor <- roll::roll_mean(rates_diff, width=n_agg, min_obs=1)
predic_tor <- rutils::lag_it(predic_tor)
predic_tor <- cbind(rep(1, NROW(predic_tor)), predic_tor)
mod_el <- lm(res_ponse ~ predic_tor - 1)
summary(mod_el)
# Calculate forecasts and pnls in-sample
in_verse <- MASS::ginv(predic_tor)
co_eff <- drop(in_verse %*% res_ponse)
forecast_s <- (predic_tor %*% co_eff)
pnl_s <- sign(forecast_s)*res_ponse
# Plot dygraph of in-sample IR strategy
weal_th <- cbind(re_turns, pnl_s)
colnames(weal_th) <- c("VTI", "Strategy")
col_names <- colnames(weal_th)
dygraphs::dygraph(cumsum(weal_th), main="Aggregated YC Strategy In-sample") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=col_names[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Define in-sample and out-of-sample intervals
in_sample <- (date_s < as.Date("2020-01-01"))
out_sample <- (date_s >= as.Date("2020-01-01"))
# Calculate forecasts and pnls out-of-sample
in_verse <- MASS::ginv(predic_tor[in_sample, ])
co_eff <- drop(in_verse %*% res_ponse[in_sample, ])
forecast_s <- (predic_tor[out_sample, ] %*% co_eff)
pnl_s <- sign(forecast_s)*res_ponse[out_sample, ]
# Plot dygraph of out-of-sample YC strategy
weal_th <- cbind(re_turns[out_sample, ], pnl_s)
colnames(weal_th) <- c("VTI", "Strategy")
col_names <- colnames(weal_th)
dygraphs::dygraph(cumsum(weal_th), main="Aggregated YC Strategy Out-of-Sample") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=col_names[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Extract ETF returns
sym_bols <- c("VTI", "IEF", "DBC")
re_turns <- rutils::etfenv$re_turns[, sym_bols]
re_turns <- na.omit(re_turns)
# Or, select rows with IEF data
# re_turns <- re_turns[index(rutils::etfenv$IEF)]
# Copy over NA values
# re_turns[1, is.na(re_turns[1, ])] <- 0
# re_turns <- zoo::na.locf(re_turns, na.rm=FALSE)
# Define end of month end points
end_p <- rutils::calc_endpoints(re_turns, inter_val="months")
end_p <- end_p[-1]
n_rows <- NROW(end_p)
date_s <- zoo::index(re_turns)[end_p]
# Start points equal end points lagged by 12-month look-back interval
look_back <- 12
start_p <- c(rep_len(1, look_back-1),
  end_p[1:(n_rows - look_back + 1)])
# Calculate matrix of look-back intervals
look_backs <- cbind(start_p, end_p)
colnames(look_backs) <- c("start", "end")
# Calculate matrix of look-forward intervals
look_fwds <- cbind(end_p + 1, rutils::lag_it(end_p, -1))
look_fwds[n_rows, ] <- end_p[n_rows]
colnames(look_fwds) <- c("start", "end")
# Inspect the intervals
head(cbind(look_backs, look_fwds))
tail(cbind(look_backs, look_fwds))
# Define performance function as Sharpe ratio
perform_ance <- function(re_turns) sum(re_turns)/sd(re_turns)
# Calculate past performance over look-back intervals
pas_t <- apply(look_backs, 1, function(ep) {
  sapply(re_turns[ep[1]:ep[2]], perform_ance)
})  # end sapply
pas_t <- t(pas_t)
pas_t[is.na(pas_t)] <- 0
# Weights are proportional to past performance
weight_s <- pas_t
# weight_s[weight_s < 0] <- 0
# Scale weight_s so sum of squares is equal to 1.
weight_s <- weight_s/sqrt(rowSums(weight_s^2))
# Or scale weight_s so sum is equal to 1
# weight_s <- weight_s/rowSums(weight_s)
# Set NA values to zero
weight_s[is.na(weight_s)] <- 0
sum(is.na(weight_s))
# Calculate future out-of-sample performance
fu_ture <- apply(look_fwds, 1, function(ep) {
  sapply(re_turns[ep[1]:ep[2]], sum)
})  # end sapply
fu_ture <- t(fu_ture)
fu_ture[is.na(fu_ture)] <- 0
tail(fu_ture)
# Calculate the momentum pnls
pnl_s <- rowSums(weight_s*fu_ture)
# Lag the future and momentum returns to proper dates
fu_ture <- rutils::lag_it(fu_ture)
pnl_s <- rutils::lag_it(pnl_s)
# The momentum strategy has low correlation to stocks
cor(pnl_s, fu_ture)
# Define all-weather benchmark
weights_aw <- c(0.30, 0.55, 0.15)
all_weather <- fu_ture %*% weights_aw
# Calculate the wealth of momentum returns
weal_th <- xts::xts(cbind(all_weather, pnl_s), order.by=date_s)
colnames(weal_th) <- c("All-Weather", "Momentum")
cor(weal_th)
# Plot dygraph of the momentum strategy returns
dygraphs::dygraph(cumsum(weal_th), main="Monthly Momentum Strategy vs All-Weather") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Calculate momentum profits and losses (returns)
pnl_s <- rowSums(weight_s*fu_ture)
# Lag the momentum returns and weights
# to correspond with end of future interval
pnl_s <- rutils::lag_it(pnl_s)
weight_s <- rutils::lag_it(weight_s)
# bid_offer equal to 10 bps for liquid ETFs
bid_offer <- 0.001
# Calculate transaction costs
weal_th <- cumsum(pnl_s)
cost_s <- 0.5*bid_offer*weal_th*rowSums(abs(rutils::diff_it(weight_s)))
weal_th <- cumsum(pnl_s - cost_s)
date_s <- index(re_turns[end_p])
weal_th <- xts::xts(weal_th, date_s)
# Define all-weather benchmark
weights_aw <- c(0.30, 0.55, 0.15)
ret_aw <- re_turns %*% weights_aw
wealth_aw <- cumsum(ret_aw)
wealth_aw <- xts::xts(wealth_aw[end_p], date_s)
# Plot the Momentum strategy and benchmark
weal_th <- cbind(weal_th, wealth_aw)
colnames(weal_th) <- c("Momentum Strategy", "Benchmark")
dygraphs::dygraph(weal_th, main="Momentum Strategy") %>%
  dyAxis("y", label="Benchmark", independentTicks=TRUE) %>%
  dyAxis("y2", label="Momentum Strategy", independentTicks=TRUE) %>%
  dySeries(name="Momentum Strategy", axis="y2", label="Momentum Strategy", strokeWidth=2, col="red") %>%
  dySeries(name="Benchmark", axis="y", label="Benchmark", strokeWidth=2, col="blue")
# Define backtest functional
backtest_momentum <- function(returns,
                perform_ance=function(re_turns) (sum(re_turns)/sd(re_turns)),
                look_back=12, re_balance="months", bid_offer=0.001,
                endp=rutils::calc_endpoints(re_turns, inter_val=re_balance)[-1],
                with_weights=FALSE, ...) {
  stopifnot("package:rutils" %in% search() || require("rutils", quietly=TRUE))
  # Define look-back and look-forward intervals
  n_rows <- NROW(end_p)
  start_p <- c(rep_len(1, look_back-1), end_p[1:(n_rows-look_back+1)])
  # Calculate look-back intervals
  look_backs <- cbind(start_p, end_p)
  # Calculate look-forward intervals
  look_fwds <- cbind(end_p + 1, rutils::lag_it(end_p, -1))
  look_fwds[n_rows, ] <- end_p[n_rows]
  # Calculate past performance over look-back intervals
  pas_t <- t(apply(look_backs, 1, function(ep) sapply(re_turns[ep[1]:ep[2]], perform_ance)))
  pas_t[is.na(pas_t)] <- 0
  # Calculate future performance
  fu_ture <- t(apply(look_fwds, 1, function(ep) sapply(re_turns[ep[1]:ep[2]], sum)))
  fu_ture[is.na(fu_ture)] <- 0
  # Scale weight_s so sum of squares is equal to 1
  weight_s <- pas_t
  weight_s <- weight_s/sqrt(rowSums(weight_s^2))
  weight_s[is.na(weight_s)] <- 0  # Set NA values to zero
  # Calculate momentum profits and losses
  pnl_s <- rowSums(weight_s*fu_ture)
  # Calculate transaction costs
  cost_s <- 0.5*bid_offer*cumprod(1 + pnl_s)*rowSums(abs(rutils::diff_it(weight_s)))
  pnl_s <- (pnl_s - cost_s)
  if (with_weights)
    rutils::lag_it(cbind(pnl_s, weight_s))
  else
    rutils::lag_it(pnl_s)
}  # end backtest_momentum
source("/Users/jerzy/Develop/lecture_slides/scripts/back_test.R")
look_backs <- seq(3, 15, by=1)
perform_ance <- function(re_turns) sum(re_turns)/sd(re_turns)
pro_file <- sapply(look_backs, function(look_back) {
  pnl_s <- backtest_momentum(returns=re_turns, endp=end_p,
    look_back=look_back, perform_ance=perform_ance)
  sum(pnl_s)
})  # end sapply
# Plot momemntum PnLs
x11(width=6, height=5)
plot(x=look_backs, y=pro_file, t="l",
  main="Momemntum PnL as function of look_back",
  xlab="look_back (months)", ylab="pnl")
# Optimal look_back
look_back <- look_backs[which.max(pro_file)]
pnl_s <- backtest_momentum(returns=re_turns,
  look_back=look_back, endp=end_p,
  perform_ance=perform_ance, with_weights=TRUE)
tail(pnl_s)
# Calculate the wealth of momentum returns
ret_mom <- pnl_s[, 1]
weal_th <- xts::xts(cbind(all_weather, ret_mom), order.by=date_s)
colnames(weal_th) <- c("All-Weather", "Momentum")
cor(weal_th)
# Plot dygraph of the momentum strategy returns
dygraphs::dygraph(cumsum(weal_th), main="Monthly Momentum Strategy vs All-Weather") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Or
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue")
quantmod::chart_Series(cumsum(weal_th), theme=plot_theme, lwd=2,
       name="Momentum PnL")
legend("topleft", legend=colnames(weal_th),
  inset=0.1, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")
# Plot the momentum portfolio weights
weight_s <- pnl_s[, -1]
vt_i <- log(quantmod::Cl(rutils::etfenv$VTI[date_s]))
colnames(vt_i) <- "VTI"
da_ta <- cbind(vt_i, weight_s)
da_ta <- na.omit(da_ta)
colnames(da_ta)[2:NCOL(pnl_s)] <- paste0(colnames(weight_s), "_weight")
zoo::plot.zoo(da_ta, xlab=NULL, main="Momentum Weights")
# Calculate ETF betas
betas_etf <- sapply(re_turns, function(x)
  cov(re_turns$VTI, x)/var(x))
# Momentum beta is equal weights times ETF betas
beta_s <- weight_s %*% betas_etf
beta_s <- xts::xts(beta_s, order.by=date_s)
colnames(beta_s) <- "momentum_beta"
da_ta <- cbind(beta_s, vt_i)
zoo::plot.zoo(da_ta,
  oma = c(3, 1, 3, 0), mar = c(0, 4, 0, 1),
  main="Momentum Beta & VTI Price", xlab="")
# Open x11 for plotting and set parameters to reduce whitespace around plot
x11(width=6, height=5)
par(mar=c(4, 4, 3, 1), oma=c(0, 0, 0, 0))
# Merton-Henriksson test
vt_i <- rutils::diff_it(vt_i)
de_sign <- cbind(VTI=vt_i, 0.5*(vt_i+abs(vt_i)), vt_i^2)
colnames(de_sign)[2:3] <- c("merton", "treynor")
mod_el <- lm(ret_mom ~ VTI + merton, data=de_sign); summary(mod_el)
# Treynor-Mazuy test
mod_el <- lm(ret_mom ~ VTI + treynor, data=de_sign); summary(mod_el)
# Plot residual scatterplot
plot.default(x=vt_i, y=ret_mom, xlab="VTI", ylab="momentum")
title(main="Treynor-Mazuy market timing test\n for Momentum vs VTI", line=0.5)
# Plot fitted (predicted) response values
points.default(x=vt_i, y=mod_el$fitted.values, pch=16, col="red")
residual_s <- mod_el$residuals
text(x=0.0, y=max(residual_s), paste("Treynor test t-value =", round(summary(mod_el)$coeff["treynor", "t value"], 2)))
# Standardize the returns
ret_mom_std <- (ret_mom-mean(ret_mom))/sd(ret_mom)
vt_i <- (vt_i-mean(vt_i))/sd(vt_i)
# Calculate skewness and kurtosis
apply(cbind(ret_mom_std, vt_i), 2, function(x)
  sapply(c(skew=3, kurt=4),
    function(e) sum(x^e)))/n_rows
# Plot histogram
hist(ret_mom_std, breaks=30,
  main="Momentum and VTI Return Distributions (standardized",
  xlim=c(-4, 4),
  xlab="", ylab="", freq=FALSE)
# Draw kernel density of histogram
lines(density(ret_mom_std), col='red', lwd=2)
lines(density(vt_i), col='blue', lwd=2)
# Add legend
legend("topright", inset=0.05, cex=0.8, title=NULL,
 leg=c("Momentum", "VTI"),
 lwd=6, bg="white", col=c("red", "blue"))
# Combine momentum strategy with all-weather
all_weather <- sd(ret_mom)*all_weather/sd(all_weather)
weal_th <- cbind(ret_mom, all_weather, 0.5*(ret_mom + all_weather))
colnames(weal_th) <- c("momentum", "all_weather", "combined")
# Calculate strategy annualized Sharpe ratios
apply(weal_th, MARGIN=2, function(x) {
  sqrt(12)*sum(x)/sd(x)/NROW(x)
})  # end apply
# Calculate strategy correlations
cor(weal_th)
# Calculate cumulative wealth
weal_th <- xts::xts(weal_th, date_s)
# Plot ETF momentum strategy combined with All-Weather
dygraphs::dygraph(cumsum(weal_th), main="ETF Momentum Strategy Combined with All-Weather") %>%
  dyOptions(colors=c("red", "blue", "green"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Or
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("green", "blue", "red")
quantmod::chart_Series(weal_th, theme=plot_theme,
       name="ETF Momentum Strategy Combined with All-Weather")
legend("topleft", legend=colnames(weal_th),
  inset=0.1, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")
# Calculate rolling variance
look_back <- 252
vari_ance <- roll::roll_var(re_turns, width=look_back, min_obs=1)
vari_ance[1, ] <- 1
# Calculate rolling Sharpe
pas_t <- roll::roll_mean(re_turns, width=look_back, min_obs=1)
weight_s <- pas_t/sqrt(vari_ance)
weight_s <- weight_s/sqrt(rowSums(weight_s^2))
weight_s <- rutils::lag_it(weight_s)
sum(is.na(weight_s))
# Calculate momentum profits and losses
pnl_s <- rowMeans(weight_s*res_ponse)
# Calculate transaction costs
bid_offer <- 0.001
cost_s <- 0.5*bid_offer*rowSums(abs(rutils::diff_it(weight_s)))
pnl_s <- (pnl_s - cost_s)
# Define all-weather benchmark
weights_aw <- c(0.30, 0.55, 0.15)
all_weather <- re_turns %*% weights_aw
# Calculate the wealth of momentum returns
weal_th <- xts::xts(cbind(all_weather, pnl_s), order.by=index(re_turns))
colnames(weal_th) <- c("All-Weather", "Momentum")
cor(weal_th)
# Plot dygraph of the momentum strategy returns
dygraphs::dygraph(cumsum(weal_th)[date_s], main="Daily Momentum Strategy vs All-Weather") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Define backtest functional for daily momentum strategy
# If trend=(-1) then it backtests a mean reverting strategy
momentum_daily <- function(returns, look_back=252, bid_offer=0.001, trend=1, ...) {
  stopifnot("package:quantmod" %in% search() || require("quantmod", quietly=TRUE))
  # Calculate rolling variance
  vari_ance <- roll::roll_var(returns, width=look_back, min_obs=1)
  vari_ance[1, ] <- 1
  vari_ance[vari_ance <= 0] <- 1
# Calculate rolling Sharpe
  pas_t <- roll::roll_mean(returns, width=look_back, min_obs=1)
  weight_s <- pas_t/sqrt(vari_ance)
  weight_s <- weight_s/sqrt(rowSums(weight_s^2))
  weight_s <- rutils::lag_it(weight_s)
  # Calculate momentum profits and losses
  pnl_s <- trend*rowMeans(weight_s*returns)
  # Calculate transaction costs
  cost_s <- 0.5*bid_offer*rowSums(abs(rutils::diff_it(weight_s)))
  (pnl_s - cost_s)
}  # end momentum_daily
# Simulate a daily ETF momentum strategy
source("/Users/jerzy/Develop/lecture_slides/scripts/back_test.R")
pnl_s <- momentum_daily(returns=re_turns, look_back=252,
  bid_offer=bid_offer)
# Perform sapply loop over look_backs
look_backs <- seq(50, 300, by=50)
pnl_s <- sapply(look_backs, momentum_daily,
  returns=re_turns, bid_offer=bid_offer)
colnames(pnl_s) <- paste0("look_back=", look_backs)
pnl_s <- xts::xts(pnl_s, index(re_turns))
tail(pnl_s)
# Plot dygraph of daily ETF momentum strategies
col_ors <- colorRampPalette(c("blue", "red"))(NCOL(pnl_s))
dygraphs::dygraph(cumsum(pnl_s), main="Daily ETF Momentum Strategies") %>%
  dyOptions(colors=col_ors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Plot EWMA strategies with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(pnl_s))
quantmod::chart_Series(cumsum(pnl_s),
  theme=plot_theme, name="Cumulative Returns of Daily ETF Momentum Strategies")
legend("bottomleft", legend=colnames(pnl_s),
  inset=0.02, bg="white", cex=0.7, lwd=rep(6, NCOL(re_turns)),
  col=plot_theme$col$line.col, bty="n")
# Define backtest functional for daily momentum strategy
# If trend=(-1) then it backtests a mean reverting strategy
momentum_daily <- function(returns, look_back=252, hold_period=5, bid_offer=0.001, trend=1, ...) {
  stopifnot("package:quantmod" %in% search() || require("quantmod", quietly=TRUE))
  # Calculate rolling variance
  vari_ance <- roll::roll_var(returns, width=look_back, min_obs=1)
  vari_ance[1, ] <- 1
  vari_ance[vari_ance <= 0] <- 1
  # Calculate rolling Sharpe
  pas_t <- roll::roll_mean(returns, width=look_back, min_obs=1)
  weight_s <- pas_t/sqrt(vari_ance)
  weight_s <- weight_s/sqrt(rowSums(weight_s^2))
  weight_s <- rutils::lag_it(weight_s)
  # Average the weights over holding period
  weight_s <- roll::roll_mean(weight_s, width=hold_period, min_obs=1)
  # Calculate momentum profits and losses
  pnl_s <- trend*rowMeans(weight_s*returns)
  # Calculate transaction costs
  cost_s <- 0.5*bid_offer*rowSums(abs(rutils::diff_it(weight_s)))
  (pnl_s - cost_s)
}  # end momentum_daily
# Perform sapply loop over holding periods
hold_periods <- seq(2, 11, by=2)
pnl_s <- sapply(hold_periods, momentum_daily, look_back=120,
            returns=re_turns, bid_offer=bid_offer)
colnames(pnl_s) <- paste0("holding=", hold_periods)
pnl_s <- xts::xts(pnl_s, index(re_turns))
# Plot dygraph of daily ETF momentum strategies
col_ors <- colorRampPalette(c("blue", "red"))(NCOL(pnl_s))
dygraphs::dygraph(cumsum(pnl_s), main="Daily ETF Momentum Strategies with Holding Period") %>%
  dyOptions(colors=col_ors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Plot EWMA strategies with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(pnl_s))
quantmod::chart_Series(cumsum(pnl_s),
  theme=plot_theme, name="Cumulative Returns of Daily ETF Momentum Strategies")
legend("bottomleft", legend=colnames(pnl_s),
  inset=0.02, bg="white", cex=0.7, lwd=rep(6, NCOL(re_turns)),
  col=plot_theme$col$line.col, bty="n")
# Define backtest functional for daily momentum strategy
# If trend=(-1) then it backtests a mean reverting strategy
momentum_daily <- function(returns, look_back=252, hold_period=5, bid_offer=0.001, trend=1, ...) {
  stopifnot("package:quantmod" %in% search() || require("quantmod", quietly=TRUE))
  position_s <- matrixStats::rowRanks(returns)
  position_s <- (position_s - rowMeans(position_s))
  position_s <- HighFreq::lag_it(position_s, lagg=1)
  trend*rowMeans(position_s*returns)
}  # end momentum_daily
# Load ETF data
sym_bols <- rutils::etfenv$sym_bols
sym_bols <- sym_bols[!(sym_bols %in% c("TLT", "IEF", "MTUM", "QUAL", "VLUE", "USMV"))]
re_turns <- rutils::etfenv$re_turns[, sym_bols]
re_turns[1, is.na(re_turns[1, ])] <- 0
re_turns <- zoo::na.locf(re_turns, na.rm=FALSE)
# Load S&P500 data
load("/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
re_turns <- re_turns["2000/"]
re_turns[1, is.na(re_turns[1, ])] <- 0
re_turns <- zoo::na.locf(re_turns, na.rm=FALSE)
n_cols <- NCOL(re_turns)
re_turns <- re_turns[, !(re_turns[n_cols %/% 10, ] == 0)]
pnl_s <- momentum_daily(returns=re_turns, trend=(-1))
pnl_s <- xts::xts(pnl_s, index(re_turns))
colnames(pnl_s) <- "PnL"
dygraphs::dygraph(cumsum(pnl_s), main="Daily Momentum Strategy") %>%
  dyOptions(colors="blue", strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Perform sapply loop over look_backs
look_backs <- seq(50, 300, by=50)
pnl_s <- sapply(look_backs, momentum_daily,
  returns=re_turns, bid_offer=bid_offer)
colnames(pnl_s) <- paste0("look_back=", look_backs)
pnl_s <- xts::xts(pnl_s, index(re_turns))
# Perform sapply loop over holding periods
hold_periods <- seq(2, 11, by=2)
pnl_s <- sapply(hold_periods, momentum_daily, look_back=120, returns=re_turns)
colnames(pnl_s) <- paste0("holding=", hold_periods)
pnl_s <- xts::xts(pnl_s, index(re_turns))
# Plot dygraph of daily ETF momentum strategies
col_ors <- colorRampPalette(c("blue", "red"))(NCOL(pnl_s))
dygraphs::dygraph(cumsum(pnl_s), main="Daily ETF Momentum Strategies with Holding Period") %>%
  dyOptions(colors=col_ors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Plot EWMA strategies with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(pnl_s))
quantmod::chart_Series(cumsum(pnl_s),
  theme=plot_theme, name="Cumulative Returns of Daily ETF Momentum Strategies")
legend("bottomleft", legend=colnames(pnl_s),
  inset=0.02, bg="white", cex=0.7, lwd=rep(6, NCOL(re_turns)),
  col=plot_theme$col$line.col, bty="n")
# Load daily S&P500 percentage stock returns.
load(file="/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
# Overwrite NA values in returns_100
returns_100 <- returns_100["2000/"]
returns_100[1, is.na(returns_100[1, ])] <- 0
returns_100 <- zoo::na.locf(returns_100, na.rm=FALSE)
# Simulate a daily S&P500 momentum strategy.
# Perform sapply loop over look_backs
look_backs <- seq(100, 300, by=20)
pnl_s <- sapply(look_backs, momentum_daily,
  hold_period=5, returns=returns_100, bid_offer=0)
colnames(pnl_s) <- paste0("look_back=", look_backs)
pnl_s <- xts::xts(pnl_s, index(returns_100))
# Plot dygraph of daily ETF momentum strategies
col_ors <- colorRampPalette(c("blue", "red"))(NCOL(pnl_s))
dygraphs::dygraph(cumsum(pnl_s), main="Daily S&P500 Momentum Strategies") %>%
  dyOptions(colors=col_ors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Plot daily S&P500 momentum strategies with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- colorRampPalette(c("blue", "red"))(NCOL(pnl_s))
quantmod::chart_Series(cumsum(pnl_s),
  theme=plot_theme, name="Daily S&P500 Momentum Strategies")
legend("bottomleft", legend=colnames(pnl_s),
  inset=0.02, bg="white", cex=0.7, lwd=rep(6, NCOL(re_turns)),
  col=plot_theme$col$line.col, bty="n")
# Perform sapply loop over look_backs
look_backs <- seq(3, 20, by=2)
pnl_s <- sapply(look_backs, momentum_daily,
  hold_period=5, returns=returns_100, bid_offer=0, trend=(-1))
colnames(pnl_s) <- paste0("look_back=", look_backs)
pnl_s <- xts::xts(pnl_s, index(returns_100))
# Plot dygraph of daily ETF momentum strategies
col_ors <- colorRampPalette(c("blue", "red"))(NCOL(pnl_s))
dygraphs::dygraph(cumsum(pnl_s), main="Daily S&P500 Momentum Strategies") %>%
  dyOptions(colors=col_ors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Plot EWMA strategies with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- colorRampPalette(c("blue", "red"))(NCOL(pnl_s))
quantmod::chart_Series(cumsum(pnl_s),
  theme=plot_theme, name="Cumulative Returns of S&P500 Mean Reverting Strategies")
legend("topleft", legend=colnames(pnl_s),
  inset=0.05, bg="white", cex=0.7, lwd=rep(6, NCOL(re_turns)),
  col=plot_theme$col$line.col, bty="n")
# Plot cumulative returns of VTI vs MTUM ETF
weal_th <- log(na.omit(rutils::etfenv$price_s[, c("VTI", "MTUM")]))
colnames(weal_th) <- c("VTI", "MTUM")
weal_th <- rutils::diff_it(weal_th)
dygraphs::dygraph(cumsum(weal_th), main="VTI vs MTUM ETF") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(width=500)
# Select all the ETF symbols except "VXX", "SVXY" "MTUM", "QUAL", "VLUE", and "USMV"
sym_bols <- colnames(rutils::etfenv$re_turns)
sym_bols <- sym_bols[!(sym_bols %in% c("VXX", "SVXY", "MTUM", "QUAL", "VLUE", "USMV"))]
# Extract columns of rutils::etfenv$re_turns and overwrite NA values
re_turns <- rutils::etfenv$re_turns[, sym_bols]
n_assets <- NCOL(re_turns)
# re_turns <- na.omit(re_turns)
re_turns[1, is.na(re_turns[1, ])] <- 0
re_turns <- zoo::na.locf(re_turns, na.rm=FALSE)
date_s <- zoo::index(re_turns)
# Returns in excess of risk-free rate
risk_free <- 0.03/252
ex_cess <- (re_turns - risk_free)
# Maximum Sharpe weights in-sample interval
rets_is <- re_turns["/2014"]
in_verse <- MASS::ginv(cov(rets_is))
weight_s <- in_verse %*% colMeans(ex_cess["/2014"])
weight_s <- drop(weight_s/sqrt(sum(weight_s^2)))
names(weight_s) <- colnames(re_turns)
# Plot portfolio weights
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
barplot(sort(weight_s), main="Maximum Sharpe Weights", cex.names=0.7)
# Calculate in-sample portfolio returns
portf_is <- xts::xts(rets_is %*% weight_s, index(rets_is))
in_dex <- xts::xts(rowSums(rets_is)/sqrt(n_assets), index(rets_is))
portf_is <- portf_is*sd(in_dex)/sd(portf_is)
# Plot cumulative portfolio returns
pnl_s <- cumsum(cbind(portf_is, in_dex))
colnames(pnl_s) <- c("Optimal Portfolio", "Equal Weight Portfolio")
dygraphs::dygraph(pnl_s, main="In-sample Optimal Portfolio Returns") %>%
  dyOptions(colors=c("red", "blue"), strokeWidth=2) %>%
  dyLegend(width=500)
# Calculate out-of-sample portfolio returns
rets_os <- re_turns["2015/"]
portf_os <- xts::xts(rets_os %*% weight_s, index(rets_os))
in_dex <- xts::xts(rowSums(rets_os)/sqrt(n_assets), index(rets_os))
portf_os <- portf_os*sd(in_dex)/sd(portf_os)
pnl_s <- cbind(portf_os, in_dex, (portf_os + in_dex)/2)
colnames(pnl_s) <- c("Optimal", "Equal Weight", "Combined")
sapply(pnl_s, function(x) mean(x)/sd(x))
# Plot cumulative portfolio returns
dygraphs::dygraph(cumsum(pnl_s), main="Out-of-sample Optimal Portfolio Returns") %>%
  dyOptions(colors=c("red", "blue", "green"), strokeWidth=2) %>%
  dyLegend(width=500)
# Maximum Sharpe weights in-sample interval
in_verse <- MASS::ginv(cov(rets_is))
weight_s <- in_verse %*% colMeans(ex_cess["/2014"])
weight_s <- drop(weight_s/sqrt(sum(weight_s^2)))
names(weight_s) <- colnames(re_turns)
# Calculate in-sample portfolio returns
portf_is <- xts::xts(rets_is %*% weight_s, index(rets_is))
# Calculate out-of-sample portfolio returns
rets_os <- re_turns["2015/"]
portf_os <- xts::xts(rets_os %*% weight_s, index(rets_os))
# Plot cumulative portfolio returns
pnl_s <- rbind(portf_is, portf_os)
in_dex <- xts::xts(rowSums(re_turns)/sqrt(n_assets), date_s)
pnl_s <- pnl_s*sd(in_dex)/sd(pnl_s)
pnl_s <- cumsum(cbind(pnl_s, in_dex))
colnames(pnl_s) <- c("Optimal Portfolio", "Equal Weight Portfolio")
dygraphs::dygraph(pnl_s, main="Out-of-sample Optimal Portfolio Returns for ETFs") %>%
  dyOptions(colors=c("red", "blue"), strokeWidth=2) %>%
  dyEvent(index(last(rets_is[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=500)
# Create rectangular matrix with collinear columns
ran_dom <- matrix(rnorm(10*8), nc=10)
# Calculate covariance matrix
cov_mat <- cov(ran_dom)
# Calculate inverse of cov_mat - error
in_verse <- solve(cov_mat)
# Perform eigen decomposition
ei_gen <- eigen(cov_mat)
eigen_vec <- ei_gen$vectors
eigen_val <- ei_gen$values
# Set tolerance for determining zero singular values
to_l <- sqrt(.Machine$double.eps)
# Calculate regularized inverse matrix
not_zero <- (eigen_val > (to_l * eigen_val[1]))
inv_reg <- eigen_vec[, not_zero] %*%
  (t(eigen_vec[, not_zero]) / eigen_val[not_zero])
# Verify inverse property of inv_reg
all.equal(cov_mat, cov_mat %*% inv_reg %*% cov_mat)
# Calculate regularized inverse of cov_mat
in_verse <- MASS::ginv(cov_mat)
# Verify inverse property of mat_rix
all.equal(in_verse, inv_reg)
# Calculate in-sample covariance matrix
cov_mat <- cov(rets_is)
ei_gen <- eigen(cov_mat)
eigen_vec <- ei_gen$vectors
eigen_val <- ei_gen$values
# Calculate shrinkage inverse of covariance matrix
eigen_max <- 3
in_verse <- eigen_vec[, 1:eigen_max] %*%
  (t(eigen_vec[, 1:eigen_max]) / ei_gen$values[1:eigen_max])
# Verify inverse property of in_verse
all.equal(cov_mat, cov_mat %*% in_verse %*% cov_mat)
# Calculate portfolio weights
weight_s <- in_verse %*% colMeans(ex_cess["/2014"])
weight_s <- drop(weight_s/sqrt(sum(weight_s^2)))
names(weight_s) <- colnames(re_turns)
# Calculate portfolio returns
portf_is <- xts::xts(rets_is %*% weight_s, index(rets_is))
portf_os <- xts::xts(rets_os %*% weight_s, index(rets_os))
# Plot cumulative portfolio returns
pnl_s <- rbind(portf_is, portf_os)
pnl_s <- pnl_s*sd(in_dex)/sd(pnl_s)
pnl_s <- cumsum(cbind(pnl_s, in_dex))
colnames(pnl_s) <- c("Optimal Portfolio", "Equal Weight Portfolio")
dygraphs::dygraph(pnl_s, main="Regularized Out-of-sample Optimal Portfolio Returns for ETFs") %>%
  dyOptions(colors=c("red", "blue"), strokeWidth=2) %>%
  dyEvent(index(last(rets_is[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=500)
# Shrink the in-sample returns to their mean
al_pha <- 0.7
excess_mean <- rowMeans(ex_cess["/2014"])
excess_is <- (1 - al_pha)*ex_cess["/2014"] + al_pha*excess_mean
# Calculate portfolio weights
weight_s <- in_verse %*% colMeans(excess_is)
weight_s <- drop(weight_s/sqrt(sum(weight_s^2)))
# Calculate portfolio returns
portf_is <- xts::xts(rets_is %*% weight_s, index(rets_is))
portf_os <- xts::xts(rets_os %*% weight_s, index(rets_os))
# Plot cumulative portfolio returns
pnl_s <- rbind(portf_is, portf_os)
pnl_s <- pnl_s*sd(in_dex)/sd(pnl_s)
pnl_s <- cumsum(cbind(pnl_s, in_dex))
colnames(pnl_s) <- c("Optimal Portfolio", "Equal Weight Portfolio")
dygraphs::dygraph(pnl_s, main="Out-of-sample Returns for ETFs With Regularization and Shrinkage") %>%
  dyOptions(colors=c("red", "blue"), strokeWidth=2) %>%
  dyEvent(index(last(rets_is[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=500)
# Define monthly dates
format(date_s[1], "%m-%Y")
format(date_s[NROW(date_s)], "%m-%Y")
month_s <- seq.Date(from=as.Date("2001-01-01"), to=as.Date("2021-04-01"), by="month")
# Perform loop over monthly dates
look_back <- 6
eigen_max <- 3
pnl_s <- lapply((look_back+1):(NROW(month_s)-1), function(i) {
  # Define in-sample and out-of-sample returns
  in_sample <- (date_s > month_s[i-look_back]) & (date_s < month_s[i])
  out_sample <- (date_s > month_s[i]) & (date_s < month_s[i+1])
  rets_is <- re_turns[in_sample]
  rets_os <- re_turns[out_sample]
  # Calculate shrinkage inverse of covariance matrix
  # in_verse <- MASS::ginv(cov(rets_is))  # if VXX and SVXY are included then no shrinkage is better
  ei_gen <- eigen(cov(rets_is))
  eigen_vec <- ei_gen$vectors
  eigen_val <- ei_gen$values
  in_verse <- eigen_vec[, 1:eigen_max] %*%
    (t(eigen_vec[, 1:eigen_max]) / ei_gen$values[1:eigen_max])
  weight_s <- in_verse %*% colMeans(rets_is - risk_free)
  weight_s <- drop(weight_s/sqrt(sum(weight_s^2)))
  # Calculate portfolio pnls out-of-sample
  xts::xts(rets_os %*% weight_s, index(rets_os))
})  # end lapply
pnl_s <- do.call(rbind, pnl_s)
# Plot dygraph of rolling monthly IR strategy
vt_i <- rutils::diff_it(cumsum(in_dex)[zoo::index(pnl_s),])
weal_th <- cbind(vt_i, pnl_s)
colnames(weal_th) <- c("Index", "Strategy")
col_names <- colnames(weal_th)
dygraphs::dygraph(cumsum(weal_th), main="Monthly ETF Rolling Portfolio Strategy With Shrinkage") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=col_names[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Define weekly dates
week_s <- seq.Date(from=as.Date("2001-01-01"), to=as.Date("2021-04-01"), by="weeks")
# Perform loop over monthly dates
look_back <- 21
eigen_max <- 3
pnl_s <- lapply((look_back+1):(NROW(week_s)-1), function(i) {
  # Define in-sample and out-of-sample returns
  in_sample <- (date_s > week_s[i-look_back]) & (date_s < week_s[i])
  out_sample <- (date_s > week_s[i]) & (date_s < week_s[i+1])
  rets_is <- re_turns[in_sample]
  rets_os <- re_turns[out_sample]
  # Calculate shrinkage inverse of covariance matrix
  # in_verse <- MASS::ginv(cov(rets_is))  # if VXX and SVXY are included then no shrinkage is better
  in_verse <- HighFreq::calc_inv(cov(rets_is), eigen_max=eigen_max)
  weight_s <- in_verse %*% colMeans(rets_is - risk_free)
  weight_s <- drop(weight_s/sqrt(sum(weight_s^2)))
  # Calculate portfolio pnls out-of-sample
  xts::xts(rets_os %*% weight_s, index(rets_os))
})  # end lapply
pnl_s <- do.call(rbind, pnl_s)
# Plot dygraph of rolling monthly IR strategy
vt_i <- rutils::diff_it(cumsum(in_dex)[zoo::index(pnl_s),])
weal_th <- cbind(vt_i, pnl_s)
colnames(weal_th) <- c("Index", "Strategy")
col_names <- colnames(weal_th)
dygraphs::dygraph(cumsum(weal_th), main="Weekly ETF Rolling Portfolio Strategy With Shrinkage") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=col_names[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Define backtest functional for daily momentum strategy
# If trend=(-1) then it backtests a mean reverting strategy
roll_portf <- function(returns, look_back=252, eigen_max=3, hold_period=5, bid_offer=0.0, trend=1, ...) {
  cat("look_back=", look_back, "\n")
  pnl_s <- lapply((look_back+1):(NROW(week_s)-1), function(i) {
    # Define in-sample and out-of-sample returns
    in_sample <- (date_s > week_s[i-look_back]) & (date_s < week_s[i])
    out_sample <- (date_s > week_s[i]) & (date_s < week_s[i+1])
    rets_is <- re_turns[in_sample]
    rets_os <- re_turns[out_sample]
    # Calculate shrinkage inverse of covariance matrix
    # in_verse <- MASS::ginv(cov(rets_is))  # if VXX and SVXY are included then no shrinkage is better
    # in_verse <- HighFreq::calc_inv(cov(rets_is), eigen_max=eigen_max)
    # weight_s <- in_verse %*% colMeans(rets_is - risk_free)
    weight_s <- HighFreq::calc_weights(rets_is, method="max_sharpe", eigen_max=eigen_max, scale=FALSE)
    weight_s <- drop(weight_s/sqrt(sum(weight_s^2)))
    # Calculate portfolio pnls out-of-sample
    xts::xts(rets_os %*% weight_s, index(rets_os))
  })  # end lapply
  do.call(rbind, pnl_s)
}  # end roll_portf
# Simulate a daily ETF momentum strategy
pnl_s <- roll_portf(returns=re_turns, look_back=41, eigen_max=eigen_max)
# Perform sapply loop over look_backs
look_backs <- seq(5, 50, by=2)
pnl_s <- lapply(look_backs, roll_portf,
  returns=re_turns, eigen_max=eigen_max)
pnl_s <- do.call(cbind, pnl_s)
colnames(pnl_s) <- paste0("look_back=", look_backs)
tail(pnl_s)
pnl_s[1, is.na(pnl_s[1, ])] <- 0
pnl_s <- zoo::na.locf(pnl_s)
foo <- sapply(pnl_s, sum)
look_backs[which.max(foo)]
# Plot dygraph of daily ETF momentum strategies
col_ors <- colorRampPalette(c("blue", "red"))(NCOL(pnl_s))
dygraphs::dygraph(cumsum(pnl_s), main="Daily ETF Momentum Strategies") %>%
  dyOptions(colors=col_ors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Plot EWMA strategies with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(pnl_s))
quantmod::chart_Series(cumsum(pnl_s),
  theme=plot_theme, name="Cumulative Returns of Daily ETF Momentum Strategies")
legend("bottomleft", legend=colnames(pnl_s),
  inset=0.02, bg="white", cex=0.7, lwd=rep(6, NCOL(re_turns)),
  col=plot_theme$col$line.col, bty="n")
load("/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
# Overwrite NA values in re_turns
re_turns <- re_turns["2000/"]
n_assets <- NCOL(re_turns)
re_turns[1, is.na(re_turns[1, ])] <- 0
re_turns <- zoo::na.locf(re_turns, na.rm=FALSE)
date_s <- zoo::index(re_turns)
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
portf_is <- xts::xts(rets_is %*% weight_s, index(rets_is))
portf_os <- xts::xts(rets_os %*% weight_s, index(rets_os))
in_dex <- xts::xts(rowSums(re_turns)/sqrt(n_assets), date_s)
# Plot cumulative portfolio returns
pnl_s <- rbind(portf_is, portf_os)
pnl_s <- pnl_s*sd(in_dex)/sd(pnl_s)
pnl_s <- cumsum(cbind(pnl_s, in_dex))
colnames(pnl_s) <- c("Optimal Portfolio", "Equal Weight Portfolio")
dygraphs::dygraph(pnl_s, main="Out-of-sample Optimal Portfolio Returns for Stocks") %>%
  dyOptions(colors=c("red", "blue"), strokeWidth=2) %>%
  dyEvent(index(last(rets_is[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=500)
# Calculate portfolio weights
weight_s <- in_verse %*% colMeans(ex_cess["/2010"])
weight_s <- drop(weight_s/sqrt(sum(weight_s^2)))
names(weight_s) <- colnames(re_turns)
# Calculate portfolio returns
portf_is <- xts::xts(rets_is %*% weight_s, index(rets_is))
portf_os <- xts::xts(rets_os %*% weight_s, index(rets_os))
in_dex <- xts::xts(rowSums(re_turns)/sqrt(n_assets), date_s)
# Plot cumulative portfolio returns
pnl_s <- rbind(portf_is, portf_os)
pnl_s <- pnl_s*sd(in_dex)/sd(pnl_s)
pnl_s <- cumsum(cbind(pnl_s, in_dex))
colnames(pnl_s) <- c("Optimal Portfolio", "Equal Weight Portfolio")
dygraphs::dygraph(pnl_s, main="Regularized Out-of-sample Optimal Portfolio Returns for Stocks") %>%
  dyOptions(colors=c("red", "blue"), strokeWidth=2) %>%
  dyEvent(index(last(rets_is[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=500)
# Shrink the in-sample returns to their mean
al_pha <- 0.7
excess_mean <- rowMeans(ex_cess["/2010"])
excess_is <- (1 - al_pha)*ex_cess["/2010"] + al_pha*excess_mean
# Calculate portfolio weights
weight_s <- in_verse %*% colMeans(excess_is)
weight_s <- drop(weight_s/sqrt(sum(weight_s^2)))
# Calculate portfolio returns
portf_is <- xts::xts(rets_is %*% weight_s, index(rets_is))
portf_os <- xts::xts(rets_os %*% weight_s, index(rets_os))
# Plot cumulative portfolio returns
pnl_s <- rbind(portf_is, portf_os)
pnl_s <- pnl_s*sd(in_dex)/sd(pnl_s)
pnl_s <- cumsum(cbind(pnl_s, in_dex))
colnames(pnl_s) <- c("Optimal Portfolio", "Equal Weight Portfolio")
dygraphs::dygraph(pnl_s, main="Out-of-sample Returns for Stocks With Regularization and Shrinkage") %>%
  dyOptions(colors=c("red", "blue"), strokeWidth=2) %>%
  dyEvent(index(last(rets_is[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=500)
library(RcppArmadillo)
# Source Rcpp functions from file
Rcpp::sourceCpp("/Users/jerzy/Develop/lecture_slides/scripts/back_test.cpp")
# Create random matrix of returns
mat_rix <- matrix(rnorm(300), nc=5)
# Regularized inverse of covariance matrix
eigen_max <- 4
ei_gen <- eigen(cov(mat_rix))
cov_inv <- ei_gen$vectors[, 1:eigen_max] %*%
  (t(ei_gen$vectors[, 1:eigen_max]) / ei_gen$values[1:eigen_max])
# Regularized inverse using RcppArmadillo
cov_inv_arma <- calc_inv(mat_rix, eigen_max)
all.equal(cov_inv, cov_inv_arma)
# Microbenchmark RcppArmadillo code
library(microbenchmark)
summary(microbenchmark(
  pure_r={ei_gen <- eigen(cov(mat_rix))
    ei_gen$vectors[, 1:eigen_max] %*%
(t(ei_gen$vectors[, 1:eigen_max]) / ei_gen$values[1:eigen_max])
  },
  r_cpp=calc_inv(mat_rix, eigen_max),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
# Overwrite NA values in returns_100
returns_100[1, is.na(returns_100[1, ])] <- 0
returns_100 <- zoo::na.locf(returns_100, na.rm=FALSE)
ex_cess <- (returns_100 - risk_free)
n_cols <- NCOL(returns_100) ; date_s <- index(returns_100)
n_assets <- NCOL(returns_100)
# Define monthly end points
end_p <- rutils::calc_endpoints(returns_100, inter_val="months")
end_p <- end_p[end_p > (n_cols+1)]
n_rows <- NROW(end_p) ; look_back <- 12
start_p <- c(rep_len(0, look_back-1), end_p[1:(n_rows-look_back+1)])
# Perform loop over end points
pnl_s <- lapply(2:n_rows, function(i) {
    # Subset the ex_cess returns
    ex_cess <- ex_cess[start_p[i-1]:end_p[i-1], ]
    in_verse <- MASS::ginv(cov(ex_cess))
    # Calculate the maximum Sharpe ratio portfolio weights
    weight_s <- in_verse %*% colMeans(ex_cess)
    weight_s <- drop(weight_s/sqrt(sum(weight_s^2)))
    # Calculate the out-of-sample portfolio returns
    re_turns <- returns_100[(end_p[i-1]+1):end_p[i], ]
    xts::xts(re_turns %*% weight_s, index(re_turns))
})  # end lapply
pnl_s <- rutils::do_call(rbind, pnl_s)
# Calculate returns of equal weight portfolio
in_dex <- xts::xts(rowMeans(returns_100), date_s)
# Plot cumulative strategy returns
weal_th <- na.omit(cbind(pnl_s, in_dex*sd(rets_portf)/sd(in_dex)))
colnames(weal_th) <- c("Rolling Strategy", "Equal Weight")
dygraphs::dygraph(cumsum(weal_th), main="Rolling Portfolio Optimization Strategy for S&P500 Stocks") %>%
  dyOptions(colors=c("red", "blue"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Shift end points to C++ convention
end_p <- (end_p - 1)
end_p[end_p < 0] <- 0
start_p <- (start_p - 1)
start_p[start_p < 0] <- 0
# Specify shrinkage intensity
al_pha <- 0.7
eigen_max <- 21
# Perform backtest in Rcpp
pnl_s <- HighFreq::back_test(excess=ex_cess, returns=returns_100,
  startp=start_p, endp=end_p, alpha=al_pha, eigen_max=eigen_max, method="max_sharpe")
# Plot cumulative strategy returns
weal_th <- cbind(pnl_s, in_dex, (pnl_s+in_dex)/2)
weal_th <- cumsum(na.omit(weal_th))
col_names <- c("Strategy", "Index", "Combined")
colnames(weal_th) <- col_names
dygraphs::dygraph(weal_th[end_p], main="Rolling S&P500 Portfolio Optimization Strategy With Shrinkage") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="red", strokeWidth=1) %>%
  dySeries(name=col_names[2], axis="y2", col="blue", strokeWidth=1) %>%
  dySeries(name=col_names[3], axis="y2", col="green", strokeWidth=2)
# Perform backtest over alphas
alpha_s <- seq(from=0.01, to=0.91, by=0.1)
pnl_s <- lapply(alpha_s, function(al_pha) {
  HighFreq::back_test(excess=ex_cess, returns=returns_100,
  startp=start_p, endp=end_p, alpha=al_pha, eigen_max=eigen_max, method="max_sharpe")
})  # end lapply
pro_file <- sapply(pnl_s, sum)
plot(x=alpha_s, y=pro_file, t="l", main="Strategy PnL as Function of Shrinkage Intensity Alpha",
  xlab="Shrinkage Intensity Alpha", ylab="pnl")
al_pha <- alpha_s[which.max(pro_file)]
pnl_s <- pnl_s[[which.max(pro_file)]]
# Perform backtest over eigen_maxs
eigen_maxs <- seq(from=3, to=40, by=2)
pnl_s <- lapply(eigen_maxs, function(eigen_max) {
  HighFreq::back_test(excess=ex_cess, returns=returns_100,
    startp=start_p, endp=end_p, alpha=al_pha, eigen_max=eigen_max, method="max_sharpe")
})  # end lapply
pro_file <- sapply(pnl_s, sum)
plot(x=eigen_maxs, y=pro_file, t="l", main="Strategy PnL as Function of eigen_max",
  xlab="eigen_max", ylab="pnl")
eigen_max <- eigen_maxs[which.max(pro_file)]
pnl_s <- pnl_s[[which.max(pro_file)]]
# Plot cumulative strategy returns
weal_th <- cbind(pnl_s, in_dex, (pnl_s+in_dex)/2)
weal_th <- cumsum(na.omit(weal_th))
col_names <- c("Strategy", "Index", "Combined")
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
  HighFreq::back_test(excess=ex_cess, returns=returns_100,
    startp=start_p, endp=end_p, alpha=al_pha, eigen_max=eigen_max, method="max_sharpe")
})  # end lapply
pro_file <- sapply(pnl_s, sum)
plot(x=look_backs, y=pro_file, t="l", main="Strategy PnL as Function of Look-back Interval",
  xlab="Look-back Interval", ylab="pnl")
look_back <- look_backs[which.max(pro_file)]
pnl_s <- pnl_s[[which.max(pro_file)]]
# Plot cumulative strategy returns
weal_th <- cbind(pnl_s, in_dex, (pnl_s+in_dex)/2)
weal_th <- cumsum(na.omit(weal_th))
col_names <- c("Strategy", "Index", "Combined")
colnames(weal_th) <- col_names
dygraphs::dygraph(weal_th[end_p], main="Optimal Rolling S&P500 Portfolio Strategy") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="red", strokeWidth=1) %>%
  dySeries(name=col_names[2], axis="y2", col="blue", strokeWidth=1) %>%
  dySeries(name=col_names[3], axis="y2", col="green", strokeWidth=2)
