# Extract VTI log OHLC prices
oh_lc <- log(rutils::etf_env$VTI)
clos_e <- quantmod::Cl(oh_lc)
re_turns <- rutils::diff_it(clos_e)
# Calculate the centered volatility
look_back <- 21
half_back <- look_back %/% 2
vol_at <- roll::roll_sd(re_turns, width=look_back, min_obs=1)
vol_at <- rutils::lag_it(vol_at, lagg=(-half_back))
# Calculate the z-scores of prices
price_scores <- (2*clos_e -
  rutils::lag_it(clos_e, half_back, pad_zeros=FALSE) -
  rutils::lag_it(clos_e, -half_back, pad_zeros=FALSE))
price_scores <- ifelse(vol_at > 0, price_scores/vol_at, 0)
# Plot dygraph of z-scores of VTI prices
price_s <- cbind(clos_e, price_scores)
colnames(price_s) <- c("VTI", "Z-scores")
col_names <- colnames(price_s)
dygraphs::dygraph(price_s["2009"], main="VTI Price Z-Scores") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=2, col="blue") %>%
  dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=2, col="red")
# Calculate thresholds for labeling tops and bottoms
threshold_s <- quantile(price_scores, c(0.1, 0.9))
# Calculate the vectors of tops and bottoms
top_s <- zoo::coredata(price_scores > threshold_s[2])
colnames(top_s) <- "tops"
bottom_s <- zoo::coredata(price_scores < threshold_s[1])
colnames(bottom_s) <- "bottoms"
# Simulate in-sample VTI strategy
position_s <- rep(NA_integer_, NROW(re_turns))
position_s[1] <- 0
position_s[top_s] <- (-1)
position_s[bottom_s] <- 1
position_s <- zoo::na.locf(position_s)
position_s <- rutils::lag_it(position_s)
pnl_s <- cumsum(re_turns*position_s)
# Plot dygraph of in-sample VTI strategy
price_s <- cbind(clos_e, pnl_s)
colnames(price_s) <- c("VTI", "Strategy")
col_names <- colnames(price_s)
dygraphs::dygraph(price_s, main="VTI Strategy Using In-sample Labels") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=2, col="blue") %>%
  dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=2, col="red")
# Calculate volatility z-scores
vol_at <- HighFreq::roll_var_ohlc(ohlc=oh_lc, look_back=look_back, scale=FALSE)
volat_mean <- roll::roll_mean(vol_at, width=look_back, min_obs=1)
volat_sd <- roll::roll_sd(rutils::diff_it(vol_at), width=look_back, min_obs=1)
volat_sd[1] <- 0
volat_scores <- ifelse(volat_sd > 0, (vol_at - volat_mean)/volat_sd, 0)
colnames(volat_scores) <- "volat"
# Calculate volume z-scores
vol_ume <- quantmod::Vo(oh_lc)
volume_mean <- roll::roll_mean(vol_ume, width=look_back, min_obs=1)
volume_sd <- roll::roll_sd(rutils::diff_it(vol_ume), width=look_back, min_obs=1)
volume_sd[1] <- 0
volume_scores <- ifelse(volume_sd > 0, (vol_ume - volume_mean)/volume_sd, 0)
colnames(volume_scores) <- "volume"
# Define design matrix for tops including intercept column
de_sign <- cbind(top_s, intercept=rep(1, NROW(top_s)),
           volat_scores, volume_scores)
# Define regression formula
col_names <- colnames(de_sign)
for_mula <- as.formula(paste(paste(col_names[1],
  paste(col_names[-1], collapse="+"), sep=" ~ "), "-1"))
# Fit in-sample logistic regression for tops
g_lm <- glm(for_mula, data=de_sign, family=binomial(logit))
summary(g_lm)
co_eff <- g_lm$coefficients
pre_dict <- drop(de_sign[, -1] %*% co_eff)
or_der <- order(pre_dict)
# Calculate in-sample forecasts from logistic regression model
forecast_s <- 1/(1+exp(-pre_dict))
all.equal(g_lm$fitted.values, forecast_s, check.attributes=FALSE)
hist(forecast_s)
x11(width=6, height=5)
plot(x=pre_dict[or_der], y=top_s[or_der],
     main="Logistic Regression of Stock Tops",
     col="orange", xlab="predictor", ylab="top")
lines(x=pre_dict[or_der], y=g_lm$fitted.values[or_der], col="blue", lwd=3)
legend(x="topleft", inset=0.1, bty="n", lwd=6,
 legend=c("tops", "logit fitted values"),
 col=c("orange", "blue"), lty=c(NA, 1), pch=c(1, NA))
# Define discrimination threshold value
thresh_old <- quantile(forecast_s, 0.95)
# Calculate confusion matrix in-sample
confu_sion <- table(actual=!top_s, forecast=(forecast_s < thresh_old))
confu_sion
# Calculate FALSE positive (type I error)
sum(!top_s & (forecast_s > thresh_old))
# Calculate FALSE negative (type II error)
sum(top_s & (forecast_s < thresh_old))
# Calculate FALSE positive and FALSE negative rates
confu_sion <- confu_sion / rowSums(confu_sion)
c(typeI=confu_sion[2, 1], typeII=confu_sion[1, 2])
# Below is an unsuccessful attempt to draw confusion matrix using xtable
confusion_matrix <- matrix(c("| true positive \\\\ (sensitivity)", "| false negative \\\\ (type II error)", "| false positive \\\\ (type I error)", "| true negative \\\\ (specificity)"), nc=2)
dimnames(confusion_matrix) <- list(forecast=c("FALSE", "TRUE"),
                             actual=c("FALSE", "TRUE"))
print(xtable::xtable(confusion_matrix,
caption="Confusion Matrix"),
caption.placement="top",
comment=FALSE, size="scriptsize",
include.rownames=TRUE,
include.colnames=TRUE)
# end unsuccessful attempt to draw confusion table using xtable
# Confusion matrix as function of thresh_old
con_fuse <- function(actual, forecasts, threshold) {
    conf <- table(actual, (forecasts < threshold))
    conf <- conf / rowSums(conf)
    c(typeI=conf[2, 1], typeII=conf[1, 2])
  }  # end con_fuse
con_fuse(!top_s, forecast_s, threshold=thresh_old)
# Define vector of discrimination thresholds
threshold_s <- quantile(forecast_s, seq(0.1, 0.99, by=0.01))
# Calculate error rates
error_rates <- sapply(threshold_s, con_fuse,
  actual=!top_s, forecasts=forecast_s)  # end sapply
error_rates <- t(error_rates)
rownames(error_rates) <- threshold_s
# Calculate the informedness
inform_ed <- 2 - rowSums(error_rates[, c("typeI", "typeII")])
plot(threshold_s, inform_ed, t="l", main="Informedness")
# Find the threshold corresponding to highest informedness
threshold_top <- threshold_s[which.max(inform_ed)]
tops_forecast <- (forecast_s > threshold_top)
# Calculate area under ROC curve (AUC)
error_rates <- rbind(c(1, 0), error_rates)
error_rates <- rbind(error_rates, c(0, 1))
true_pos <- (1 - error_rates[, "typeII"])
true_pos <- (true_pos + rutils::lag_it(true_pos))/2
false_pos <- rutils::diff_it(error_rates[, "typeI"])
abs(sum(true_pos*false_pos))
# Plot ROC Curve for stock tops
x11(width=5, height=5)
plot(x=error_rates[, "typeI"], y=1-error_rates[, "typeII"],
     xlab="FALSE positive rate", ylab="TRUE positive rate",
     main="ROC Curve for Stock Tops", type="l", lwd=3, col="blue")
abline(a=0.0, b=1.0, lwd=3, col="orange")
# Define design matrix for tops including intercept column
de_sign <- cbind(bottom_s, intercept=rep(1, NROW(bottom_s)),
           volat_scores, volume_scores)
# Define regression formula
col_names <- colnames(de_sign)
for_mula <- as.formula(paste(paste(col_names[1],
  paste(col_names[-1], collapse="+"), sep=" ~ "), "-1"))
# Fit in-sample logistic regression for tops
g_lm <- glm(for_mula, data=de_sign, family=binomial(logit))
summary(g_lm)
# Calculate in-sample forecast from logistic regression model
pre_dict <- drop(de_sign[, -1] %*% g_lm$coefficients)
forecast_s <- 1/(1+exp(-pre_dict))
# Calculate error rates
error_rates <- sapply(threshold_s, con_fuse,
  actual=!bottom_s, forecasts=forecast_s)  # end sapply
error_rates <- t(error_rates)
rownames(error_rates) <- threshold_s
# Calculate the informedness
inform_ed <- 2 - rowSums(error_rates[, c("typeI", "typeII")])
plot(threshold_s, inform_ed, t="l", main="Informedness")
# Find the threshold corresponding to highest informedness
threshold_bottom <- threshold_s[which.max(inform_ed)]
bottoms_forecast <- (forecast_s > threshold_bottom)
# Calculate area under ROC curve (AUC)
error_rates <- rbind(c(1, 0), error_rates)
error_rates <- rbind(error_rates, c(0, 1))
true_pos <- (1 - error_rates[, "typeII"])
true_pos <- (true_pos + rutils::lag_it(true_pos))/2
false_pos <- rutils::diff_it(error_rates[, "typeI"])
abs(sum(true_pos*false_pos))
# Plot ROC Curve for stock tops
x11(width=5, height=5)
plot(x=error_rates[, "typeI"], y=1-error_rates[, "typeII"],
     xlab="FALSE positive rate", ylab="TRUE positive rate",
     main="ROC Curve for Stock Bottoms", type="l", lwd=3, col="blue")
abline(a=0.0, b=1.0, lwd=3, col="orange")
# Simulate in-sample VTI strategy
position_s <- rep(NA_integer_, NROW(re_turns))
position_s[1] <- 0
position_s[tops_forecast] <- (-1)
position_s[bottoms_forecast] <- 1
position_s <- zoo::na.locf(position_s)
position_s <- rutils::lag_it(position_s)
pnl_s <- cumsum(re_turns*position_s)
# Plot dygraph of in-sample VTI strategy
price_s <- cbind(clos_e, pnl_s)
colnames(price_s) <- c("VTI", "Strategy")
col_names <- colnames(price_s)
dygraphs::dygraph(price_s, main="Logistic Strategy Using Top and Bottom Labels") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=2, col="blue") %>%
  dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=2, col="red")
# Determine trade dates right after EWMA has crossed prices
in_dic <- sign(clos_e - ew_ma)
trade_dates <- (rutils::diff_it(in_dic) != 0)
trade_dates <- which(trade_dates) + 1
trade_dates <- trade_dates[trade_dates < n_rows]
# Calculate positions, either: -1, 0, or 1
position_s <- rep(NA_integer_, n_rows)
position_s[1] <- 0
position_s[trade_dates] <- in_dic[trade_dates-1]
position_s <- zoo::na.locf(position_s, na.rm=FALSE)
position_s <- xts::xts(position_s, order.by=index(oh_lc))
# Plot EWMA prices with position shading
da_ta <- cbind(clos_e, ew_ma)
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("blue", "red")
quantmod::chart_Series(da_ta["2007/2010"], theme=plot_theme,
       name="EWMA prices")
add_TA(position_s > 0, on=-1,
 col="lightgreen", border="lightgreen")
add_TA(position_s < 0, on=-1,
 col="lightgrey", border="lightgrey")
legend("bottomleft", legend=colnames(ew_ma),
 inset=0.1, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")
op_en <- quantmod::Op(oh_lc)
hi_gh <- quantmod::Hi(oh_lc)
lo_w <- quantmod::Lo(oh_lc)
clos_e <- quantmod::Cl(oh_lc)
# Calculate daily profits and losses
# Calculate pnl for days without trade
pnl_s <- rutils::diff_it(clos_e)*position_s
# Calculate realized pnl for days with trade
close_lag <- rutils::lag_it(clos_e)
pos_lagged <- rutils::lag_it(position_s)
pnl_s[trade_dates] <- pos_lagged[trade_dates]*
  (op_en[trade_dates] - close_lag[trade_dates])
# Calculate unrealized pnl for days with trade
pnl_s[trade_dates] <- pnl_s[trade_dates] +
  position_s[trade_dates]*
  (clos_e[trade_dates] - op_en[trade_dates])
# Annualized Sharpe ratio of EWMA strategy
sqrt(252)*sum(pnl_s)/sd(pnl_s)/NROW(pnl_s)
# Cumulative pnls
star_t <- as.numeric(clos_e[1])
cum_pnls <- star_t + cumsum(pnl_s)
cum_pnls <- cbind(clos_e, cum_pnls)
colnames(cum_pnls) <- c("VTI", "EWMA PnL")
cum_pnls <- star_t + cumsum((pnl_s+re_turns)/2)
cum_pnls <- cbind(clos_e, cum_pnls)
colnames(cum_pnls) <- c("VTI", "EWMA PnL")
# Plot EWMA PnL with position shading
quantmod::chart_Series(cum_pnls, theme=plot_theme,
       name="Performance of EWMA Strategy")
add_TA(position_s > 0, on=-1,
 col="lightgreen", border="lightgreen")
add_TA(position_s < 0, on=-1,
 col="lightgrey", border="lightgrey")
legend("top", legend=colnames(cum_pnls),
 inset=0.05, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")
# bid_offer equal to 10 bps for liquid ETFs
bid_offer <- 0.001
# Calculate transaction costs
cost_s <- 0.5*bid_offer*abs(pos_lagged - position_s)*clos_e
# pnl_s <- (pnl_s - cost_s)
# Plot strategy with transaction costs
cum_pnls <- star_t + cumsum(pnl_s)
cum_pnls <- cbind(cum_pnls, cum_pnls - cumsum(cost_s))
colnames(cum_pnls) <- c(sym_bol, "costs")
dygraphs::dygraph(cum_pnls, main=paste(sym_bol, "EWMA Strategy With Transaction Costs")) %>%
  dySeries(name="costs", label="Strategy With Transaction Costs", strokeWidth=2, col="green") %>%
  dySeries(name=sym_bol, label="EWMA Strategy", strokeWidth=2, col="blue")
simu_ewma <- function(oh_lc, lamb_da=0.01, wid_th=351, bid_offer=0.001, tre_nd=1) {
  n_rows <- NROW(oh_lc)
  # Calculate EWMA prices
  weight_s <- exp(-lamb_da*1:wid_th)
  weight_s <- weight_s/sum(weight_s)
  clos_e <- quantmod::Cl(oh_lc)
  ew_ma <- .Call(stats:::C_cfilter, clos_e, filter=weight_s, sides=1, circular=FALSE)
  ew_ma[1:(wid_th-1)] <- ew_ma[wid_th]
  # Determine trade dates right after EWMA has crossed prices
  in_dic <- tre_nd*sign(clos_e - ew_ma)
  trade_dates <- (rutils::diff_it(in_dic) != 0)
  trade_dates <- which(trade_dates) + 1
  trade_dates <- trade_dates[trade_dates < n_rows]
  # Calculate positions, either: -1, 0, or 1
  position_s <- rep(NA_integer_, n_rows)
  position_s[1] <- 0
  position_s[trade_dates] <- in_dic[trade_dates-1]
  position_s <- zoo::na.locf(position_s, na.rm=FALSE)
  op_en <- quantmod::Op(oh_lc)
  close_lag <- rutils::lag_it(clos_e)
  pos_lagged <- rutils::lag_it(position_s)
  # Calculate daily profits and losses
  pnl_s <- rutils::diff_it(clos_e)*position_s
  pnl_s[trade_dates] <- pos_lagged[trade_dates]*
    (op_en[trade_dates] - close_lag[trade_dates])
  pnl_s[trade_dates] <- pnl_s[trade_dates] +
    position_s[trade_dates]*
    (clos_e[trade_dates] - op_en[trade_dates])
  # Calculate transaction costs
  cost_s <- 0.5*bid_offer*abs(pos_lagged - position_s)*clos_e
  pnl_s <- (pnl_s - cost_s)
  # Calculate strategy returns
  pnl_s <- cbind(position_s, pnl_s)
  colnames(pnl_s) <- c("positions", "pnls")
  pnl_s
}  # end simu_ewma
source("C:/Develop/lecture_slides/scripts/ewma_model.R")
lamb_das <- seq(from=1e-5, to=0.05, by=0.01)
# Perform lapply() loop over lamb_das
pnl_s <- lapply(lamb_das, function(lamb_da) {
  # Simulate EWMA strategy and calculate re_turns
  star_t + cumsum(simu_ewma(oh_lc=oh_lc,
    lamb_da=lamb_da, wid_th=wid_th)[, "pnls"])
})  # end lapply
pnl_s <- do.call(cbind, pnl_s)
colnames(pnl_s) <- paste0("lambda=", lamb_das)
# Plot EWMA strategies with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(pnl_s))
quantmod::chart_Series(pnl_s, theme=plot_theme,
  name="Cumulative Returns of EWMA Strategies")
legend("topleft", legend=colnames(pnl_s), inset=0.1,
  bg="white", cex=0.8, lwd=rep(6, NCOL(pnl_s)),
  col=plot_theme$col$line.col, bty="n")
# Initialize compute cluster under Windows
library(parallel)
clus_ter <- makeCluster(detectCores()-1)
clusterExport(clus_ter,
  varlist=c("oh_lc", "wid_th", "simu_ewma"))
# Perform parallel loop over lamb_das under Windows
pnl_s <- parLapply(clus_ter, lamb_das, function(lamb_da) {
  library(quantmod)
  # Simulate EWMA strategy and calculate re_turns
  star_t + cumsum(simu_ewma(oh_lc=oh_lc,
    lamb_da=lamb_da, wid_th=wid_th)[, "pnls"])
})  # end parLapply
# Perform parallel loop over lamb_das under Mac-OSX or Linux
re_turns <- mclapply(lamb_das, function(lamb_da) {
  library(quantmod)
  # Simulate EWMA strategy and calculate re_turns
  star_t + cumsum(simu_ewma(oh_lc=oh_lc,
    lamb_da=lamb_da, wid_th=wid_th)[, "pnls"])
})  # end mclapply
stopCluster(clus_ter)  # Stop R processes over cluster under Windows
pnl_s <- do.call(cbind, pnl_s)
colnames(pnl_s) <- paste0("lambda=", lamb_das)
sharpe_ratios <- sqrt(252)*sapply(pnl_s, function(x_ts) {
  # Calculate annualized Sharpe ratio of strategy returns
  x_ts <- rutils::diff_it(x_ts)
  sum(x_ts)/sd(x_ts)
})/NROW(pnl_s)  # end sapply
plot(x=lamb_das, y=sharpe_ratios, t="l",
     main="Performance of EWMA trend following strategies
     as function of the decay parameter lambda")
trend_returns <- rutils::diff_it(pnl_s)
trend_sharpe <- sharpe_ratios
# Simulate best performing strategy
ewma_trend <- simu_ewma(oh_lc=oh_lc,
  lamb_da=lamb_das[which.max(sharpe_ratios)],
  wid_th=wid_th)
position_s <- ewma_trend[, "positions"]
pnl_s <- star_t + cumsum(ewma_trend[, "pnls"])
pnl_s <- cbind(clos_e, pnl_s)
colnames(pnl_s) <- c("VTI", "EWMA PnL")
# Plot EWMA PnL with position shading
plot_theme$col$line.col <- c("orange", "blue")
quantmod::chart_Series(pnl_s, theme=plot_theme,
       name="Performance of Trend Following EWMA Strategy")
add_TA(position_s > 0, on=-1,
 col="lightgreen", border="lightgreen")
add_TA(position_s < 0, on=-1,
 col="lightgrey", border="lightgrey")
legend("top", legend=colnames(pnl_s),
  inset=0.05, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")
source("C:/Develop/lecture_slides/scripts/ewma_model.R")
lamb_das <- seq(0.05, 1.0, 0.05)
# Perform lapply() loop over lamb_das
pnl_s <- lapply(lamb_das, function(lamb_da) {
  # Backtest EWMA strategy and calculate re_turns
  star_t + cumsum(simu_ewma(
    oh_lc=oh_lc, lamb_da=lamb_da, wid_th=wid_th, tre_nd=(-1))[, "pnls"])
})  # end lapply
pnl_s <- do.call(cbind, pnl_s)
colnames(pnl_s) <- paste0("lambda=", lamb_das)
# Plot EWMA strategies with custom line colors
column_s <- seq(1, NCOL(pnl_s), by=4)
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NROW(column_s))
quantmod::chart_Series(pnl_s[, column_s],
  theme=plot_theme, name="Cumulative Returns of Mean Reverting EWMA Strategies")
legend("topleft", legend=colnames(pnl_s[, column_s]),
  inset=0.1, bg="white", cex=0.8, lwd=6,
  col=plot_theme$col$line.col, bty="n")
sharpe_ratios <- sqrt(252)*sapply(pnl_s, function(x_ts) {
  # Calculate annualized Sharpe ratio of strategy returns
  x_ts <- rutils::diff_it(x_ts)
  sum(x_ts)/sd(x_ts)
})/NROW(pnl_s)  # end sapply
plot(x=lamb_das, y=sharpe_ratios, t="l",
     main="Performance of EWMA mean reverting strategies
     as function of the decay parameter lambda")
revert_returns <- rutils::diff_it(pnl_s)
revert_sharpe <- sharpe_ratios
# Backtest best performing strategy
ewma_revert <- simu_ewma(oh_lc=oh_lc, bid_offer=0.0,
  lamb_da=lamb_das[which.max(sharpe_ratios)],
  wid_th=wid_th, tre_nd=(-1))
position_s <- ewma_revert[, "positions"]
pnl_s <- star_t + cumsum(ewma_revert[, "pnls"])
pnl_s <- cbind(clos_e, pnl_s)
colnames(pnl_s) <- c("VTI", "EWMA PnL")
# Plot EWMA PnL with position shading
plot_theme$col$line.col <- c("orange", "blue")
quantmod::chart_Series(pnl_s, theme=plot_theme,
       name="Performance of Mean Reverting EWMA Strategy")
add_TA(position_s > 0, on=-1,
 col="lightgreen", border="lightgreen")
add_TA(position_s < 0, on=-1,
 col="lightgrey", border="lightgrey")
legend("top", legend=colnames(pnl_s),
  inset=0.05, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")
# Calculate correlation between trend following and mean reverting strategies
trend_ing <- ewma_trend[, "pnls"]
colnames(trend_ing) <- "trend"
revert_ing <- ewma_revert[, "pnls"]
colnames(revert_ing) <- "revert"
close_rets <- rutils::diff_it(clos_e)
cor(cbind(trend_ing, revert_ing, close_rets))
# Calculate combined strategy
com_bined <- trend_ing + revert_ing
colnames(com_bined) <- "combined"
# Calculate annualized Sharpe ratio of strategy returns
re_turns <- cbind(close_rets, trend_ing, revert_ing, com_bined)
sqrt(252)*sapply(re_turns, function(x_ts)
  sum(x_ts)/sd(x_ts))/NROW(com_bined)
pnl_s <- lapply(re_turns, function(x_ts) {star_t + cumsum(x_ts)})
pnl_s <- do.call(cbind, pnl_s)
colnames(pnl_s) <- c("VTI", "trending", "reverting", "EWMA combined PnL")
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue", "green", "magenta2")
quantmod::chart_Series(pnl_s, theme=plot_theme,
       name="Performance of Combined EWMA Strategies")
legend("topleft", legend=colnames(pnl_s),
 inset=0.05, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")
weight_s <- c(trend_sharpe, revert_sharpe)
weight_s[weight_s<0] <- 0
weight_s <- weight_s/sum(weight_s)
re_turns <- cbind(trend_returns, revert_returns)
avg_returns <- re_turns %*% weight_s
avg_returns <- xts::xts(avg_returns, order.by=index(re_turns))
pnl_s <- (star_t + cumsum(avg_returns))
pnl_s <- cbind(clos_e, pnl_s)
colnames(pnl_s) <- c("VTI", "EWMA PnL")
# Plot EWMA PnL without position shading
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue")
quantmod::chart_Series(pnl_s, theme=plot_theme,
  name="Performance of Ensemble EWMA Strategy")
legend("top", legend=colnames(pnl_s),
  inset=0.05, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")
# Calculate VWAP positions
position_s <- sign(vwap_fast - vwap_slow)
# Lag the positions to avoid data snooping
position_s <- rutils::lag_it(position_s)
# Calculate daily profits and losses of strategy
pnl_s <- re_turns*position_s
colnames(pnl_s) <- "Strategy"
cum_pnls <- cumsum(pnl_s)
weal_th <- cbind(cum_rets, cum_pnls, v_wap)
colnames(weal_th) <- c(sym_bol, "Strategy", "VWAP")
# Annualized Sharpe ratios of VTI and VWAP strategy
sharp_e <- sqrt(252)*sapply(cbind(re_turns, pnl_s), function (x) mean(x)/sd(x))
# Calculate index for background shading
in_dic <- (cum_rets > v_wap)
whi_ch <- which(rutils::diff_it(in_dic) != 0)
in_dic <- rbind(first(in_dic), in_dic[whi_ch, ], last(in_dic))
date_s <- index(in_dic)
in_dic <- ifelse(drop(coredata(in_dic)), "lightgreen", "antiquewhite")
# Create dygraph object without rendering it
dy_graph <- dygraphs::dygraph(weal_th, main=paste("VWAP Crossover Strategy, Sharpe", paste(paste(names(sharp_e), round(sharp_e, 3), sep="="), collapse=", "))) %>%
  dyOptions(colors=c("blue", "red", "purple"), strokeWidth=2)
# Add shading
for (i in 1:(NROW(in_dic)-1)) {
    dy_graph <- dy_graph %>%
dyShading(from=date_s[i], to=date_s[i+1], color=in_dic[i])
}  # end for
# Render the dygraph object
dy_graph
# Plot VTI and VWAP strategy using quantmod
quantmod::chart_Series(x=cbind(cum_rets, cum_pnls),
  name="VWAP Crossover Strategy for VTI", theme=plot_theme)
add_TA(position_s > 0, on=-1, col="lightgreen", border="lightgreen")
add_TA(position_s < 0, on=-1, col="lightgrey", border="lightgrey")
legend("top", legend=c(sym_bol, "VWAP strategy"), lty=1, lwd=6,
 cex=0.9, inset=0.1, bg="white", col=c("blue", "red"), bty="n")
# Calculate positions from lagged indicator
lagg <- 2
in_dic <- sign(cum_rets - v_wap)
indic_sum <- roll::roll_sum(in_dic, width=lagg)
indic_sum[1:lagg] <- 0
position_s <- rep(NA_integer_, NROW(clos_e))
position_s[1] <- 0
position_s <- ifelse(indic_sum == lagg, 1, position_s)
position_s <- ifelse(indic_sum == (-lagg), -1, position_s)
position_s <- zoo::na.locf(position_s, na.rm=FALSE)
# Lag the positions to trade in next period
position_s <- rutils::lag_it(position_s, lagg=1)
# Calculate PnLs of lagged strategy
pnl_s <- re_turns*position_s
colnames(pnl_s) <- "Strategy"
cum_pnls_lag <- cumsum(pnl_s)
weal_th <- cbind(cum_pnls, cum_pnls_lag)
colnames(weal_th) <- c("Strategy", "Strategy_lag")
# Annualized Sharpe ratios of VWAP strategies
sharp_e <- sqrt(252)*sapply(cbind(re_turns, pnl_s),
  function (x) mean(x)/sd(x))
# Plot both strategies
dygraphs::dygraph(weal_th, main=paste("VWAP Crossover Strategy, Sharpe", paste(paste(names(sharp_e), round(sharp_e, 3), sep="="), collapse=", "))) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=3)
# Calculate fast and slow VWAPs
vwap_fast <- TTR::VWAP(cum_rets, volume=vol_ume, n=20)
vwap_fast[1:20] <- 0
vwap_slow <- TTR::VWAP(cum_rets, volume=vol_ume, n=200)
vwap_slow[1:200] <- 0
# Calculate VWAP positions
position_s <- sign(vwap_fast - vwap_slow)
# Lag the positions to avoid data snooping
position_s <- rutils::lag_it(position_s)
# Calculate daily profits and losses of strategy
pnl_s <- re_turns*position_s
colnames(pnl_s) <- "Strategy"
cum_pnls <- cumsum(pnl_s)
weal_th <- cbind(cum_rets, cum_pnls, vwap_fast, vwap_slow)
colnames(weal_th) <- c(sym_bol, "Strategy", "VWAP_fast", "VWAP_slow")
# Annualized Sharpe ratios of VTI and VWAP strategy
sharp_e <- sqrt(252)*sapply(cbind(re_turns, pnl_s),
  function (x) mean(x)/sd(x))
# Calculate index for background shading
in_dic <- (vwap_fast > vwap_slow)
whi_ch <- which(rutils::diff_it(in_dic) != 0)
in_dic <- rbind(first(in_dic), in_dic[whi_ch, ], last(in_dic))
date_s <- index(in_dic)
in_dic <- ifelse(drop(coredata(in_dic)), "lightgreen", "antiquewhite")
# Create dygraph object without rendering it
dy_graph <- dygraphs::dygraph(weal_th, main=paste("VWAP Crossover Strategy, Sharpe", paste(paste(names(sharp_e), round(sharp_e, 3), sep="="), collapse=", "))) %>%
  dyOptions(colors=c("blue", "red", "purple", "lightpurple"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Add shading
for (i in 1:(NROW(in_dic)-1)) {
    dy_graph <- dy_graph %>%
dyShading(from=date_s[i], to=date_s[i+1], color=in_dic[i])
}  # end for
# Render the dygraph object
dy_graph
# Calculate correlation of VWAP strategy with VTI
cor(pnl_s, re_turns)
# Combine VWAP strategy with VTI
weal_th <- cbind(re_turns, pnl_s, 0.5*(re_turns+pnl_s))
colnames(weal_th) <- c(sym_bol, "VWAP", "Combined")
sharp_e <- sqrt(252)*sapply(weal_th, function (x) mean(x)/sd(x))
# Plot dygraph of VWAP strategy combined with VTI
dygraphs::dygraph(cumsum(weal_th),
  main=paste("VWAP Crossover Strategy, Sharpe", paste(paste(names(sharp_e), round(sharp_e, 3), sep="="), collapse=", "))) %>%
  dyOptions(colors=c("blue", "purple", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Define Brownian Motion parameters
n_rows <- 1000; sig_ma <- 0.01
# Simulate 5 paths of Brownian motion
price_s <- matrix(rnorm(5*n_rows, sd=sig_ma), nc=5)
price_s <- matrixStats::colCumsums(price_s)
# Open plot window on Mac
dev.new(width=6, height=4, noRStudioGD=TRUE)
# Set plot parameters to reduce whitespace around plot
par(mar=c(2, 2, 3, 1), oma=c(0, 0, 0, 0))
# Plot 5 paths of Brownian motion
matplot(y=price_s, main="Brownian Motion Paths",
  xlab="", ylab="", type="l", lty="solid", lwd=1, col="blue")
# Save plot to png file on Mac
quartz.save("figure/brown_paths.png", type="png", width=6, height=4)
# Define Ornstein-Uhlenbeck parameters
eq_price <- 1.0; sig_ma <- 0.02
the_ta <- 0.01; n_rows <- 1000
# Initialize the data
in_nov <- rnorm(n_rows)
re_turns <- numeric(n_rows)
price_s <- numeric(n_rows)
# Simulate Ornstein-Uhlenbeck process in R
price_s[1] <- sig_ma*in_nov[1]
for (i in 2:n_rows) {
  re_turns[i] <- the_ta*(eq_price - price_s[i-1]) +
    sig_ma*in_nov[i]
  price_s[i] <- price_s[i-1] + re_turns[i]
}  # end for
# Simulate Ornstein-Uhlenbeck process in Rcpp
prices_cpp <- HighFreq::sim_ou(eq_price=eq_price, volat=sig_ma,
  theta=the_ta, innov=matrix(in_nov))
all.equal(price_s, drop(prices_cpp))
# Compare the speed of R code with Rcpp
library(microbenchmark)
summary(microbenchmark(
  Rcode={for (i in 2:n_rows) {
    re_turns[i] <- the_ta*(eq_price - price_s[i-1]) + sig_ma*in_nov[i]
    price_s[i] <- price_s[i-1] + re_turns[i]}},
  Rcpp=HighFreq::sim_ou(eq_price=eq_price, volat=sig_ma,
    theta=the_ta, innov=matrix(in_nov)),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
plot(price_s, type="l",
     xlab="time", ylab="prices",
     main="Ornstein-Uhlenbeck Process")
legend("topright",
 title=paste(c(paste0("sig_ma = ", sig_ma),
               paste0("eq_price = ", eq_price),
               paste0("the_ta = ", the_ta)),
             collapse="\n"),
 legend="", cex=0.8, inset=0.1, bg="white", bty="n")
abline(h=eq_price, col='red', lwd=2)
re_turns <- rutils::diff_it(price_s)
lag_prices <- rutils::lag_it(price_s)
for_mula <- re_turns ~ lag_prices
l_m <- lm(for_mula)
summary(l_m)
# Plot regression
plot(for_mula, main="OU Returns Versus Lagged Prices")
abline(l_m, lwd=2, col="red")
# Calculate volatility parameter
c(volatility=sig_ma, estimate=sd(re_turns))
# Extract OU parameters from regression
co_eff <- summary(l_m)$coefficients
# Calculate regression alpha and beta directly
be_ta <- cov(re_turns, lag_prices)/var(lag_prices)
al_pha <- (mean(re_turns) - be_ta*mean(lag_prices))
cbind(direct=c(alpha=al_pha, beta=be_ta), lm=co_eff[, 1])
all.equal(c(alpha=al_pha, beta=be_ta), co_eff[, 1],
    check.attributes=FALSE)
# Calculate regression standard errors directly
beta_s <- c(alpha=al_pha, beta=be_ta)
fit_ted <- (al_pha + be_ta*lag_prices)
residual_s <- (re_turns - fit_ted)
prices_squared <- sum((lag_prices - mean(lag_prices))^2)
beta_sd <- sqrt(sum(residual_s^2)/prices_squared/(n_rows-2))
alpha_sd <- sqrt(sum(residual_s^2)/(n_rows-2)*(1/n_rows + mean(lag_prices)^2/prices_squared))
cbind(direct=c(alpha_sd=alpha_sd, beta_sd=beta_sd), lm=co_eff[, 2])
all.equal(c(alpha_sd=alpha_sd, beta_sd=beta_sd), co_eff[, 2],
    check.attributes=FALSE)
# Compare mean reversion parameter theta
c(theta=(-the_ta), round(co_eff[2, ], 3))
# Compare equilibrium price mu
c(eq_price=eq_price, estimate=-co_eff[1, 1]/co_eff[2, 1])
# Compare actual and estimated parameters
co_eff <- cbind(c(the_ta*eq_price, -the_ta), co_eff[, 1:2])
rownames(co_eff) <- c("drift", "theta")
colnames(co_eff)[1] <- "actual"
round(co_eff, 4)
# Simulate Schwartz process
re_turns <- numeric(n_rows)
price_s <- numeric(n_rows)
price_s[1] <- exp(sig_ma*in_nov[1])
set.seed(1121)  # Reset random numbers
for (i in 2:n_rows) {
  re_turns[i] <- the_ta*(eq_price - price_s[i-1]) + sig_ma*in_nov[i]
  price_s[i] <- price_s[i-1]*exp(re_turns[i])
}  # end for
plot(price_s, type="l", xlab="time", ylab="prices",
     main="Schwartz Process")
legend("topright",
 title=paste(c(paste0("sig_ma = ", sig_ma),
               paste0("eq_price = ", eq_price),
               paste0("the_ta = ", the_ta)),
             collapse="\n"),
 legend="", cex=0.8, inset=0.12, bg="white", bty="n")
abline(h=eq_price, col='red', lwd=2)
x11(width=6, height=5)
par(mar=c(3, 2, 1, 1), oma=c(1, 0, 0, 0))
re_turns <- na.omit(rutils::etf_env$re_turns$VTI)
# Plot autocorrelations using stats::acf()
stats::acf(re_turns, lag=10, xlab="lag", main="")
title(main="ACF of VTI Returns", line=-1)
# Two-tailed 95% confidence interval
qnorm(0.975)/sqrt(NROW(re_turns))
# Ljung-Box test for VTI returns
# 'lag' is the number of autocorrelation coefficients
Box.test(re_turns, lag=10, type="Ljung")
library(Ecdat)  # Load Ecdat
macro_zoo <- as.zoo(Macrodat[, c("lhur", "fygm3")])
colnames(macro_zoo) <- c("unemprate", "3mTbill")
macro_diff <- na.omit(diff(macro_zoo))
# Changes in 3 month T-bill rate are autocorrelated
Box.test(macro_diff[, "3mTbill"], lag=10, type="Ljung")
# Changes in unemployment rate are autocorrelated
Box.test(macro_diff[, "unemprate"], lag=10, type="Ljung")
# Get the ACF data returned invisibly
acf_data <- acf(re_turns, plot=FALSE)
summary(acf_data)
# Print the ACF data
print(acf_data)
dim(acf_data$acf)
dim(acf_data$lag)
head(acf_data$acf)
plot_acf <- function(x_ts, lagg=10, plo_t=TRUE,
               xlab="Lag", ylab="", main="", ...) {
  # Calculate the ACF without a plot
  acf_data <- acf(x=x_ts, lag.max=lagg, plot=FALSE, ...)
  # Remove first element of ACF data
  acf_data$acf <- array(data=acf_data$acf[-1],
    dim=c((dim(acf_data$acf)[1]-1), 1, 1))
  acf_data$lag <- array(data=acf_data$lag[-1],
    dim=c((dim(acf_data$lag)[1]-1), 1, 1))
  # Plot ACF
  if (plo_t) {
    ci <- qnorm((1+0.95)/2)*sqrt(1/NROW(x_ts))
    ylim <- c(min(-ci, range(acf_data$acf[-1])),
        max(ci, range(acf_data$acf[-1])))
    plot(acf_data, xlab=xlab, ylab=ylab,
   ylim=ylim, main="", ci=0)
    title(main=main, line=0.5)
    abline(h=c(-ci, ci), col="blue", lty=2)
  }  # end if
  # Return the ACF data invisibly
  invisible(acf_data)
}  # end plot_acf
# Improved autocorrelation function
x11(width=6, height=5)
rutils::plot_acf(re_turns, lag=10, main="")
title(main="ACF of VTI returns", line=-1)
# Ljung-Box test for VTI returns
Box.test(re_turns, lag=10, type="Ljung")
x11(width=6, height=7)
par(mfrow=c(2,1))  # Set plot panels
par(mar=c(3, 3, 2, 2), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
# Autocorrelation of squared random returns
rutils::plot_acf(rnorm(NROW(re_turns))^2, lag=10, main="")
title(main="ACF of Squared Random Returns", line=-1)
# Autocorrelation of squared VTI returns
rutils::plot_acf(re_turns^2, lag=10, main="")
title(main="ACF of Squared VTI Returns", line=-1)
# Ljung-Box test for squared VTI returns
Box.test(re_turns^2, lag=10, type="Ljung")
library(rutils)  # Load package rutils
library(Ecdat)  # Load Ecdat
colnames(Macrodat)  # United States Macroeconomic Time Series
# Coerce to "zoo"
macro_zoo <- as.zoo(Macrodat[, c("lhur", "fygm3")])
colnames(macro_zoo) <- c("unemprate", "3mTbill")
# ggplot2 in multiple panes
autoplot(  # Generic ggplot2 for "zoo"
  object=macro_zoo, main="US Macro",
  facets=Series ~ .) + # end autoplot
  xlab("") +
theme(  # Modify plot theme
  legend.position=c(0.1, 0.5),
  plot.title=element_text(vjust=-2.0),
  plot.margin=unit(c(-0.5, 0.0, -0.5, 0.0), "cm"),
  plot.background=element_blank(),
  axis.text.y=element_blank()
)  # end theme
par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # Set plot panels
macro_diff <- na.omit(diff(macro_zoo))
rutils::plot_acf(coredata(macro_diff[, "unemprate"]),
  lag=10, main="quarterly unemployment rate")
rutils::plot_acf(coredata(macro_diff[, "3mTbill"]),
  lag=10, main="3 month T-bill EOQ")
# Simulate AR processes
set.seed(1121)  # Reset random numbers
date_s <- Sys.Date() + 0:728  # Two year daily series
# AR time series of returns
ari_ma <- xts(x=arima.sim(n=NROW(date_s), model=list(ar=0.2)),
          order.by=date_s)
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
  arima.sim(n=NROW(date_s), model=list(ar=phi))
})  # end sapply
colnames(ari_ma) <- paste("autocorr", ar_coeff)
plot.zoo(ari_ma, main="AR(1) prices", xlab=NA)
# Or plot using ggplot
ari_ma <- xts(x=ari_ma, order.by=date_s)
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
# Define AR(3) coefficients and innovations
co_eff <- c(0.1, 0.39, 0.5)
n_rows <- 1e2
set.seed(1121); in_nov <- rnorm(n_rows)
# Simulate AR process using recursive loop in R
ari_ma <- numeric(NROW(in_nov))
ari_ma[1] <- in_nov[1]
ari_ma[2] <- co_eff[1]*ari_ma[1] + in_nov[2]
ari_ma[3] <- co_eff[1]*ari_ma[2] + co_eff[2]*ari_ma[1] + in_nov[3]
for (it in 4:NROW(ari_ma)) {
  ari_ma[it] <- ari_ma[(it-1):(it-3)] %*% co_eff + in_nov[it]
}  # End for
# Simulate AR process using filter()
arima_faster <- filter(x=in_nov, filter=co_eff, method="recursive")
class(arima_faster)
all.equal(ari_ma, as.numeric(arima_faster))
# Fast simulation of AR process using C_rfilter()
arima_fastest <- .Call(stats:::C_rfilter, in_nov, co_eff,
                 double(NROW(co_eff) + NROW(in_nov)))[-(1:3)]
all.equal(ari_ma, arima_fastest)
# Calculate modulus of roots of characteristic equation
root_s <- Mod(polyroot(c(1, -co_eff)))
# Calculate warmup period
warm_up <- NROW(co_eff) + ceiling(6/log(min(root_s)))
set.seed(1121)
n_rows <- 1e4
in_nov <- rnorm(n_rows + warm_up)
# Simulate AR process using arima.sim()
ari_ma <- arima.sim(n=n_rows,
  model=list(ar=co_eff),
  start.innov=in_nov[1:warm_up],
  innov=in_nov[(warm_up+1):NROW(in_nov)])
# Simulate AR process using filter()
arima_fast <- filter(x=in_nov, filter=co_eff, method="recursive")
all.equal(arima_fast[-(1:warm_up)], as.numeric(ari_ma))
# Benchmark the speed of the three methods of simulating AR process
library(microbenchmark)
summary(microbenchmark(
  filter=filter(x=in_nov, filter=co_eff, method="recursive"),
  arima_sim=arima.sim(n=n_rows,
                  model=list(ar=co_eff),
                  start.innov=in_nov[1:warm_up],
                  innov=in_nov[(warm_up+1):NROW(in_nov)]),
  arima_loop={for (it in 4:NROW(ari_ma)) {
  ari_ma[it] <- ari_ma[(it-1):(it-3)] %*% co_eff + in_nov[it]}}
  ), times=10)[, c(1, 4, 5)]
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0))
# Simulate AR(1) process
ari_ma <- arima.sim(n=1e3, model=list(ar=0.8))
# ACF of AR(1) process
ac_f <- rutils::plot_acf(ari_ma, lag=10, xlab="", ylab="",
  main="Autocorrelations of AR(1) process")
ac_f$acf[1:5]
# PACF of AR(1) process
pac_f <- pacf(ari_ma, lag=10, xlab="", ylab="", main="")
title("Partial autocorrelations of AR(1) process", line=1)
pac_f <- drop(pac_f$acf)
pac_f[1:5]
# Compute pacf recursively from acf
ac_f <- rutils::plot_acf(ari_ma, lag=10, plo_t=FALSE)
ac_f <- drop(ac_f$acf)
pac_f <- numeric(3)
pac_f[1] <- ac_f[1]
pac_f[2] <- ac_f[2] - ac_f[1]^2
pac_f[3] <- ac_f[3] - pac_f[2]*ac_f[1] - ac_f[2]*pac_f[1]
# Compute pacf recursively in a loop
pac_f <- numeric(NROW(ac_f))
pac_f[1] <- ac_f[1]
for (it in 2:NROW(pac_f)) {
  pac_f[it] <- ac_f[it] - pac_f[1:(it-1)] %*% ac_f[(it-1):1]
}  # end for
par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # Set plot panels
# Simulate AR process of returns
ari_ma <- arima.sim(n=1e3, model=list(ar=c(0.1, 0.5, 0.1)))
# ACF of AR(3) process
rutils::plot_acf(ari_ma, lag=10, xlab="", ylab="",
   main="ACF of AR(3) process")
# PACF of AR(3) process
pacf(ari_ma, lag=10, xlab="", ylab="", main="PACF of AR(3) process")
library(rutils)  # Load rutils
library(ggplot2)  # Load ggplot2
set.seed(1121)  # Initialize random number generator
rand_walk <- cumsum(zoo(matrix(rnorm(3*100), ncol=3),
            order.by=(Sys.Date()+0:99)))
colnames(rand_walk) <- paste("rand_walk", 1:3, sep="_")
plot.zoo(rand_walk, main="Random walks",
     xlab="", ylab="", plot.type="single",
     col=c("black", "red", "blue"))
# Add legend
legend(x="topleft", legend=colnames(rand_walk),
 col=c("black", "red", "blue"), lty=1)
# Simulate arima with large AR coefficient
set.seed(1121)
ari_ma <- arima.sim(n=n_rows, model=list(ar=0.99))
tseries::adf.test(ari_ma)
# Integrated series has unit root
tseries::adf.test(cumsum(ari_ma))
# Simulate arima with negative AR coefficient
set.seed(1121)
ari_ma <- arima.sim(n=n_rows, model=list(ar=-0.99))
tseries::adf.test(ari_ma)
# Integrated series has unit root
tseries::adf.test(cumsum(ari_ma))
# Simulate random walks using apply() loops
set.seed(1121)  # Initialize random number generator
rand_walks <- matrix(rnorm(1000*100), ncol=1000)
rand_walks <- apply(rand_walks, 2, cumsum)
vari_ance <- apply(rand_walks, 1, var)
# Simulate random walks using vectorized functions
set.seed(1121)  # Initialize random number generator
rand_walks <- matrixStats::colCumsums(matrix(rnorm(1000*100), ncol=1000))
vari_ance <- matrixStats::rowVars(rand_walks)
par(mar=c(5, 3, 2, 2), oma=c(0, 0, 0, 0))
plot(vari_ance, xlab="time steps", ylab="",
     t="l", col="blue", lwd=2,
     main="Variance of Random Walk")
# Define Dickey-Fuller parameters
eq_price <- 1.0; sig_ma <- 0.02
the_ta <- 0.01; n_rows <- 1000
# Initialize the data
in_nov <- rnorm(n_rows)
re_turns <- numeric(n_rows)
price_s <- numeric(n_rows)
# Simulate Dickey-Fuller process in R
price_s[1] <- sig_ma*in_nov[1]
for (i in 2:n_rows) {
  re_turns[i] <- the_ta*(eq_price - price_s[i-1]) + sig_ma*in_nov[i]
  price_s[i] <- price_s[i-1] + re_turns[i]
}  # end for
# Simulate Dickey-Fuller process in Rcpp
prices_cpp <- HighFreq::sim_ou(eq_price=eq_price, volat=sig_ma,
    theta=the_ta, innov=matrix(in_nov))
all.equal(price_s, drop(prices_cpp))
# Compare the speed of R code with Rcpp
library(microbenchmark)
summary(microbenchmark(
  Rcode={for (i in 2:n_rows) {
    re_turns[i] <- the_ta*(eq_price - price_s[i-1]) + sig_ma*in_nov[i]
    price_s[i] <- price_s[i-1] + re_turns[i]}},
  Rcpp=HighFreq::sim_ou(eq_price=eq_price, volat=sig_ma, theta=the_ta, innov=matrix(in_nov)),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
set.seed(1121); in_nov <- rnorm(1e4, sd=0.01)
# Simulate AR(1) process with coefficient=1, with unit root
ari_ma <- filter(x=in_nov, filter=1.0, method="recursive")
x11(); plot(ari_ma, t="l", main="AR(1) coefficient = 1.0")
# Perform ADF test with lag = 1
tseries::adf.test(ari_ma, k=1)
# Perform standard Dickey-Fuller test
tseries::adf.test(ari_ma, k=0)
# Simulate AR(1) with coefficient close to 1, without unit root
ari_ma <- filter(x=in_nov, filter=0.99, method="recursive")
x11(); plot(ari_ma, t="l", main="AR(1) coefficient = 0.99")
tseries::adf.test(ari_ma, k=1)
# Simulate Ornstein-Uhlenbeck OU process with mean reversion
eq_price <- 0.0; the_ta <- 0.001
price_s <- HighFreq::sim_ou(eq_price=eq_price, volat=1.0,
  theta=the_ta, innov=in_nov)
x11(); plot(price_s, t="l", main=paste("OU coefficient =", the_ta))
tseries::adf.test(price_s, k=1)
# Simulate Ornstein-Uhlenbeck OU process with zero reversion
the_ta <- 0.0
price_s <- HighFreq::sim_ou(eq_price=eq_price, volat=1.0,
  theta=the_ta, innov=in_nov)
x11(); plot(price_s, t="l", main=paste("OU coefficient =", the_ta))
tseries::adf.test(price_s, k=1)
