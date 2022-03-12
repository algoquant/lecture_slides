# Extract VTI log OHLC prices
ohlc <- log(rutils::etfenv$VTI)
closep <- quantmod::Cl(ohlc)
returns <- rutils::diffit(closep)
# Calculate the centered volatility
look_back <- 21
half_back <- look_back %/% 2
volat <- roll::roll_sd(returns, width=look_back, min_obs=1)
volat <- rutils::lagit(volat, lagg=(-half_back))
# Calculate the z-scores of prices
pricescores <- (2*closep -
  rutils::lagit(closep, half_back, pad_zeros=FALSE) -
  rutils::lagit(closep, -half_back, pad_zeros=FALSE))
pricescores <- ifelse(volat > 0, pricescores/volat, 0)
# Plot dygraph of z-scores of VTI prices
prices <- cbind(closep, pricescores)
colnames(prices) <- c("VTI", "Z-scores")
colnames <- colnames(prices)
dygraphs::dygraph(prices["2009"], main="VTI Price Z-Scores") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", label=colnames[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnames[2], axis="y2", label=colnames[2], strokeWidth=2, col="red")
# Calculate thresholds for labeling tops and bottoms
threshold_s <- quantile(pricescores, c(0.1, 0.9))
# Calculate the vectors of tops and bottoms
top_s <- zoo::coredata(pricescores > threshold_s[2])
colnames(top_s) <- "tops"
bottom_s <- zoo::coredata(pricescores < threshold_s[1])
colnames(bottom_s) <- "bottoms"
# Simulate in-sample VTI strategy
position_s <- rep(NA_integer_, NROW(returns))
position_s[1] <- 0
position_s[top_s] <- (-1)
position_s[bottom_s] <- 1
position_s <- zoo::na.locf(position_s)
position_s <- rutils::lagit(position_s)
pnls <- cumsum(returns*position_s)
# Plot dygraph of in-sample VTI strategy
prices <- cbind(closep, pnls)
colnames(prices) <- c("VTI", "Strategy")
colnames <- colnames(prices)
dygraphs::dygraph(prices, main="VTI Strategy Using In-sample Labels") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", label=colnames[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnames[2], axis="y2", label=colnames[2], strokeWidth=2, col="red")
# Calculate volatility z-scores
volat <- HighFreq::roll_var_ohlc(ohlc=ohlc, look_back=look_back, scale=FALSE)
volat_mean <- roll::roll_mean(volat, width=look_back, min_obs=1)
volat_sd <- roll::roll_sd(rutils::diffit(volat), width=look_back, min_obs=1)
volat_sd[1] <- 0
volat_scores <- ifelse(volat_sd > 0, (volat - volat_mean)/volat_sd, 0)
colnames(volat_scores) <- "volat"
# Calculate volume z-scores
volumes <- quantmod::Vo(ohlc)
volume_mean <- roll::roll_mean(volumes, width=look_back, min_obs=1)
volume_sd <- roll::roll_sd(rutils::diffit(volumes), width=look_back, min_obs=1)
volume_sd[1] <- 0
volume_scores <- ifelse(volume_sd > 0, (volumes - volume_mean)/volume_sd, 0)
colnames(volume_scores) <- "volume"
# Define design matrix for tops including intercept column
design <- cbind(top_s, intercept=rep(1, NROW(top_s)),
           volat_scores, volume_scores)
# Define regression formula
colnames <- colnames(design)
formulav <- as.formula(paste(paste(colnames[1],
  paste(colnames[-1], collapse="+"), sep=" ~ "), "-1"))
# Fit in-sample logistic regression for tops
glmod <- glm(formulav, data=design, family=binomial(logit))
summary(glmod)
coeff <- glmod$coefficients
predictv <- drop(design[, -1] %*% coeff)
ordern <- order(predictv)
# Calculate in-sample forecasts from logistic regression model
forecasts <- 1/(1+exp(-predictv))
all.equal(glmod$fitted.values, forecasts, check.attributes=FALSE)
hist(forecasts)
x11(width=6, height=5)
plot(x=predictv[ordern], y=top_s[ordern],
     main="Logistic Regression of Stock Tops",
     col="orange", xlab="predictor", ylab="top")
lines(x=predictv[ordern], y=glmod$fitted.values[ordern], col="blue", lwd=3)
legend(x="topleft", inset=0.1, bty="n", lwd=6,
 legend=c("tops", "logit fitted values"),
 col=c("orange", "blue"), lty=c(NA, 1), pch=c(1, NA))
# Define discrimination threshold value
threshold <- quantile(forecasts, 0.95)
# Calculate confusion matrix in-sample
confu_sion <- table(actual=!top_s, forecast=(forecasts < threshold))
confu_sion
# Calculate FALSE positive (type I error)
sum(!top_s & (forecasts > threshold))
# Calculate FALSE negative (type II error)
sum(top_s & (forecasts < threshold))
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
# Confusion matrix as function of threshold
con_fuse <- function(actual, forecasts, threshold) {
    conf <- table(actual, (forecasts < threshold))
    conf <- conf / rowSums(conf)
    c(typeI=conf[2, 1], typeII=conf[1, 2])
  }  # end con_fuse
con_fuse(!top_s, forecasts, threshold=threshold)
# Define vector of discrimination thresholds
threshold_s <- quantile(forecasts, seq(0.1, 0.99, by=0.01))
# Calculate error rates
error_rates <- sapply(threshold_s, con_fuse,
  actual=!top_s, forecasts=forecasts)  # end sapply
error_rates <- t(error_rates)
rownames(error_rates) <- threshold_s
# Calculate the informedness
inform_ed <- 2 - rowSums(error_rates[, c("typeI", "typeII")])
plot(threshold_s, inform_ed, t="l", main="Informedness")
# Find the threshold corresponding to highest informedness
threshold_top <- threshold_s[which.max(inform_ed)]
tops_forecast <- (forecasts > threshold_top)
# Calculate area under ROC curve (AUC)
error_rates <- rbind(c(1, 0), error_rates)
error_rates <- rbind(error_rates, c(0, 1))
true_pos <- (1 - error_rates[, "typeII"])
true_pos <- (true_pos + rutils::lagit(true_pos))/2
false_pos <- rutils::diffit(error_rates[, "typeI"])
abs(sum(true_pos*false_pos))
# Plot ROC Curve for stock tops
x11(width=5, height=5)
plot(x=error_rates[, "typeI"], y=1-error_rates[, "typeII"],
     xlab="FALSE positive rate", ylab="TRUE positive rate",
     main="ROC Curve for Stock Tops", type="l", lwd=3, col="blue")
abline(a=0.0, b=1.0, lwd=3, col="orange")
# Define design matrix for tops including intercept column
design <- cbind(bottom_s, intercept=rep(1, NROW(bottom_s)),
           volat_scores, volume_scores)
# Define regression formula
colnames <- colnames(design)
formulav <- as.formula(paste(paste(colnames[1],
  paste(colnames[-1], collapse="+"), sep=" ~ "), "-1"))
# Fit in-sample logistic regression for tops
glmod <- glm(formulav, data=design, family=binomial(logit))
summary(glmod)
# Calculate in-sample forecast from logistic regression model
predictv <- drop(design[, -1] %*% glmod$coefficients)
forecasts <- 1/(1+exp(-predictv))
# Calculate error rates
error_rates <- sapply(threshold_s, con_fuse,
  actual=!bottom_s, forecasts=forecasts)  # end sapply
error_rates <- t(error_rates)
rownames(error_rates) <- threshold_s
# Calculate the informedness
inform_ed <- 2 - rowSums(error_rates[, c("typeI", "typeII")])
plot(threshold_s, inform_ed, t="l", main="Informedness")
# Find the threshold corresponding to highest informedness
threshold_bottom <- threshold_s[which.max(inform_ed)]
bottoms_forecast <- (forecasts > threshold_bottom)
# Calculate area under ROC curve (AUC)
error_rates <- rbind(c(1, 0), error_rates)
error_rates <- rbind(error_rates, c(0, 1))
true_pos <- (1 - error_rates[, "typeII"])
true_pos <- (true_pos + rutils::lagit(true_pos))/2
false_pos <- rutils::diffit(error_rates[, "typeI"])
abs(sum(true_pos*false_pos))
# Plot ROC Curve for stock tops
x11(width=5, height=5)
plot(x=error_rates[, "typeI"], y=1-error_rates[, "typeII"],
     xlab="FALSE positive rate", ylab="TRUE positive rate",
     main="ROC Curve for Stock Bottoms", type="l", lwd=3, col="blue")
abline(a=0.0, b=1.0, lwd=3, col="orange")
# Simulate in-sample VTI strategy
position_s <- rep(NA_integer_, NROW(returns))
position_s[1] <- 0
position_s[tops_forecast] <- (-1)
position_s[bottoms_forecast] <- 1
position_s <- zoo::na.locf(position_s)
position_s <- rutils::lagit(position_s)
pnls <- cumsum(returns*position_s)
# Plot dygraph of in-sample VTI strategy
prices <- cbind(closep, pnls)
colnames(prices) <- c("VTI", "Strategy")
colnames <- colnames(prices)
dygraphs::dygraph(prices, main="Logistic Strategy Using Top and Bottom Labels") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", label=colnames[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnames[2], axis="y2", label=colnames[2], strokeWidth=2, col="red")
# Determine trade dates right after EWMA has crossed prices
indic <- sign(closep - ew_ma)
trade_dates <- (rutils::diffit(indic) != 0)
trade_dates <- which(trade_dates) + 1
trade_dates <- trade_dates[trade_dates < nrows]
# Calculate positions, either: -1, 0, or 1
position_s <- rep(NA_integer_, nrows)
position_s[1] <- 0
position_s[trade_dates] <- indic[trade_dates-1]
position_s <- zoo::na.locf(position_s, na.rm=FALSE)
position_s <- xts::xts(position_s, order.by=index(ohlc))
# Plot EWMA prices with position shading
datav <- cbind(closep, ew_ma)
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("blue", "red")
quantmod::chart_Series(datav["2007/2010"], theme=plot_theme,
       name="EWMA prices")
add_TA(position_s > 0, on=-1,
 col="lightgreen", border="lightgreen")
add_TA(position_s < 0, on=-1,
 col="lightgrey", border="lightgrey")
legend("bottomleft", legend=colnames(ew_ma),
 inset=0.1, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")
openp <- quantmod::Op(ohlc)
highp <- quantmod::Hi(ohlc)
lowp <- quantmod::Lo(ohlc)
closep <- quantmod::Cl(ohlc)
# Calculate daily profits and losses
# Calculate pnl for days without trade
pnls <- rutils::diffit(closep)*position_s
# Calculate realized pnl for days with trade
close_lag <- rutils::lagit(closep)
pos_lagged <- rutils::lagit(position_s)
pnls[trade_dates] <- pos_lagged[trade_dates]*
  (openp[trade_dates] - close_lag[trade_dates])
# Calculate unrealized pnl for days with trade
pnls[trade_dates] <- pnls[trade_dates] +
  position_s[trade_dates]*
  (closep[trade_dates] - openp[trade_dates])
# Annualized Sharpe ratio of EWMA strategy
sqrt(252)*sum(pnls)/sd(pnls)/NROW(pnls)
# Cumulative pnls
startd <- as.numeric(closep[1])
cum_pnls <- startd + cumsum(pnls)
cum_pnls <- cbind(closep, cum_pnls)
colnames(cum_pnls) <- c("VTI", "EWMA PnL")
cum_pnls <- startd + cumsum((pnls+returns)/2)
cum_pnls <- cbind(closep, cum_pnls)
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
costs <- 0.5*bid_offer*abs(pos_lagged - position_s)*closep
# pnls <- (pnls - costs)
# Plot strategy with transaction costs
cum_pnls <- startd + cumsum(pnls)
cum_pnls <- cbind(cum_pnls, cum_pnls - cumsum(costs))
colnames(cum_pnls) <- c(symbol, "costs")
dygraphs::dygraph(cum_pnls, main=paste(symbol, "EWMA Strategy With Transaction Costs")) %>%
  dySeries(name="costs", label="Strategy With Transaction Costs", strokeWidth=2, col="green") %>%
  dySeries(name=symbol, label="EWMA Strategy", strokeWidth=2, col="blue")
simu_ewma <- function(ohlc, lambda=0.01, wid_th=351, bid_offer=0.001, tre_nd=1) {
  nrows <- NROW(ohlc)
  # Calculate EWMA prices
  weightv <- exp(-lambda*1:wid_th)
  weightv <- weightv/sum(weightv)
  closep <- quantmod::Cl(ohlc)
  ew_ma <- .Call(stats:::C_cfilter, closep, filter=weightv, sides=1, circular=FALSE)
  ew_ma[1:(wid_th-1)] <- ew_ma[wid_th]
  # Determine trade dates right after EWMA has crossed prices
  indic <- tre_nd*sign(closep - ew_ma)
  trade_dates <- (rutils::diffit(indic) != 0)
  trade_dates <- which(trade_dates) + 1
  trade_dates <- trade_dates[trade_dates < nrows]
  # Calculate positions, either: -1, 0, or 1
  position_s <- rep(NA_integer_, nrows)
  position_s[1] <- 0
  position_s[trade_dates] <- indic[trade_dates-1]
  position_s <- zoo::na.locf(position_s, na.rm=FALSE)
  openp <- quantmod::Op(ohlc)
  close_lag <- rutils::lagit(closep)
  pos_lagged <- rutils::lagit(position_s)
  # Calculate daily profits and losses
  pnls <- rutils::diffit(closep)*position_s
  pnls[trade_dates] <- pos_lagged[trade_dates]*
    (openp[trade_dates] - close_lag[trade_dates])
  pnls[trade_dates] <- pnls[trade_dates] +
    position_s[trade_dates]*
    (closep[trade_dates] - openp[trade_dates])
  # Calculate transaction costs
  costs <- 0.5*bid_offer*abs(pos_lagged - position_s)*closep
  pnls <- (pnls - costs)
  # Calculate strategy returns
  pnls <- cbind(position_s, pnls)
  colnames(pnls) <- c("positions", "pnls")
  pnls
}  # end simu_ewma
source("C:/Develop/lecture_slides/scripts/ewma_model.R")
lambdas <- seq(from=1e-5, to=0.05, by=0.01)
# Perform lapply() loop over lambdas
pnls <- lapply(lambdas, function(lambda) {
  # Simulate EWMA strategy and calculate returns
  startd + cumsum(simu_ewma(ohlc=ohlc,
    lambda=lambda, wid_th=wid_th)[, "pnls"])
})  # end lapply
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("lambda=", lambdas)
# Plot EWMA strategies with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(pnls))
quantmod::chart_Series(pnls, theme=plot_theme,
  name="Cumulative Returns of EWMA Strategies")
legend("topleft", legend=colnames(pnls), inset=0.1,
  bg="white", cex=0.8, lwd=rep(6, NCOL(pnls)),
  col=plot_theme$col$line.col, bty="n")
# Initialize compute cluster under Windows
library(parallel)
cluster <- makeCluster(detectCores()-1)
clusterExport(cluster,
  varlist=c("ohlc", "wid_th", "simu_ewma"))
# Perform parallel loop over lambdas under Windows
pnls <- parLapply(cluster, lambdas, function(lambda) {
  library(quantmod)
  # Simulate EWMA strategy and calculate returns
  startd + cumsum(simu_ewma(ohlc=ohlc,
    lambda=lambda, wid_th=wid_th)[, "pnls"])
})  # end parLapply
# Perform parallel loop over lambdas under Mac-OSX or Linux
returns <- mclapply(lambdas, function(lambda) {
  library(quantmod)
  # Simulate EWMA strategy and calculate returns
  startd + cumsum(simu_ewma(ohlc=ohlc,
    lambda=lambda, wid_th=wid_th)[, "pnls"])
})  # end mclapply
stopCluster(cluster)  # Stop R processes over cluster under Windows
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("lambda=", lambdas)
sharper <- sqrt(252)*sapply(pnls, function(xtes) {
  # Calculate annualized Sharpe ratio of strategy returns
  xtes <- rutils::diffit(xtes)
  sum(xtes)/sd(xtes)
})/NROW(pnls)  # end sapply
plot(x=lambdas, y=sharper, t="l",
     main="Performance of EWMA trend following strategies
     as function of the decay parameter lambda")
trend_returns <- rutils::diffit(pnls)
trend_sharpe <- sharper
# Simulate best performing strategy
ewma_trend <- simu_ewma(ohlc=ohlc,
  lambda=lambdas[which.max(sharper)],
  wid_th=wid_th)
position_s <- ewma_trend[, "positions"]
pnls <- startd + cumsum(ewma_trend[, "pnls"])
pnls <- cbind(closep, pnls)
colnames(pnls) <- c("VTI", "EWMA PnL")
# Plot EWMA PnL with position shading
plot_theme$col$line.col <- c("orange", "blue")
quantmod::chart_Series(pnls, theme=plot_theme,
       name="Performance of Trend Following EWMA Strategy")
add_TA(position_s > 0, on=-1,
 col="lightgreen", border="lightgreen")
add_TA(position_s < 0, on=-1,
 col="lightgrey", border="lightgrey")
legend("top", legend=colnames(pnls),
  inset=0.05, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")
source("C:/Develop/lecture_slides/scripts/ewma_model.R")
lambdas <- seq(0.05, 1.0, 0.05)
# Perform lapply() loop over lambdas
pnls <- lapply(lambdas, function(lambda) {
  # Backtest EWMA strategy and calculate returns
  startd + cumsum(simu_ewma(
    ohlc=ohlc, lambda=lambda, wid_th=wid_th, tre_nd=(-1))[, "pnls"])
})  # end lapply
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("lambda=", lambdas)
# Plot EWMA strategies with custom line colors
column_s <- seq(1, NCOL(pnls), by=4)
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NROW(column_s))
quantmod::chart_Series(pnls[, column_s],
  theme=plot_theme, name="Cumulative Returns of Mean Reverting EWMA Strategies")
legend("topleft", legend=colnames(pnls[, column_s]),
  inset=0.1, bg="white", cex=0.8, lwd=6,
  col=plot_theme$col$line.col, bty="n")
sharper <- sqrt(252)*sapply(pnls, function(xtes) {
  # Calculate annualized Sharpe ratio of strategy returns
  xtes <- rutils::diffit(xtes)
  sum(xtes)/sd(xtes)
})/NROW(pnls)  # end sapply
plot(x=lambdas, y=sharper, t="l",
     main="Performance of EWMA mean reverting strategies
     as function of the decay parameter lambda")
revert_returns <- rutils::diffit(pnls)
revert_sharpe <- sharper
# Backtest best performing strategy
ewma_revert <- simu_ewma(ohlc=ohlc, bid_offer=0.0,
  lambda=lambdas[which.max(sharper)],
  wid_th=wid_th, tre_nd=(-1))
position_s <- ewma_revert[, "positions"]
pnls <- startd + cumsum(ewma_revert[, "pnls"])
pnls <- cbind(closep, pnls)
colnames(pnls) <- c("VTI", "EWMA PnL")
# Plot EWMA PnL with position shading
plot_theme$col$line.col <- c("orange", "blue")
quantmod::chart_Series(pnls, theme=plot_theme,
       name="Performance of Mean Reverting EWMA Strategy")
add_TA(position_s > 0, on=-1,
 col="lightgreen", border="lightgreen")
add_TA(position_s < 0, on=-1,
 col="lightgrey", border="lightgrey")
legend("top", legend=colnames(pnls),
  inset=0.05, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")
# Calculate correlation between trend following and mean reverting strategies
trend_ing <- ewma_trend[, "pnls"]
colnames(trend_ing) <- "trend"
revert_ing <- ewma_revert[, "pnls"]
colnames(revert_ing) <- "revert"
close_rets <- rutils::diffit(closep)
cor(cbind(trend_ing, revert_ing, close_rets))
# Calculate combined strategy
com_bined <- trend_ing + revert_ing
colnames(com_bined) <- "combined"
# Calculate annualized Sharpe ratio of strategy returns
returns <- cbind(close_rets, trend_ing, revert_ing, com_bined)
sqrt(252)*sapply(returns, function(xtes)
  sum(xtes)/sd(xtes))/NROW(com_bined)
pnls <- lapply(returns, function(xtes) {startd + cumsum(xtes)})
pnls <- do.call(cbind, pnls)
colnames(pnls) <- c("VTI", "trending", "reverting", "EWMA combined PnL")
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue", "green", "magenta2")
quantmod::chart_Series(pnls, theme=plot_theme,
       name="Performance of Combined EWMA Strategies")
legend("topleft", legend=colnames(pnls),
 inset=0.05, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")
weightv <- c(trend_sharpe, revert_sharpe)
weightv[weightv<0] <- 0
weightv <- weightv/sum(weightv)
returns <- cbind(trend_returns, revert_returns)
avg_returns <- returns %*% weightv
avg_returns <- xts::xts(avg_returns, order.by=index(returns))
pnls <- (startd + cumsum(avg_returns))
pnls <- cbind(closep, pnls)
colnames(pnls) <- c("VTI", "EWMA PnL")
# Plot EWMA PnL without position shading
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue")
quantmod::chart_Series(pnls, theme=plot_theme,
  name="Performance of Ensemble EWMA Strategy")
legend("top", legend=colnames(pnls),
  inset=0.05, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")
# Calculate VWAP positions
position_s <- sign(vwap_fast - vwap_slow)
# Lag the positions to avoid data snooping
position_s <- rutils::lagit(position_s)
# Calculate daily profits and losses of strategy
pnls <- returns*position_s
colnames(pnls) <- "Strategy"
cum_pnls <- cumsum(pnls)
wealth <- cbind(cum_rets, cum_pnls, vwapv)
colnames(wealth) <- c(symbol, "Strategy", "VWAP")
# Annualized Sharpe ratios of VTI and VWAP strategy
sharp_e <- sqrt(252)*sapply(cbind(returns, pnls), function (x) mean(x)/sd(x))
# Calculate index for background shading
indic <- (cum_rets > vwapv)
whichv <- which(rutils::diffit(indic) != 0)
indic <- rbind(first(indic), indic[whichv, ], last(indic))
dates <- index(indic)
indic <- ifelse(drop(coredata(indic)), "lightgreen", "antiquewhite")
# Create dygraph object without rendering it
dyplot <- dygraphs::dygraph(wealth, main=paste("VWAP Crossover Strategy, Sharpe", paste(paste(names(sharp_e), round(sharp_e, 3), sep="="), collapse=", "))) %>%
  dyOptions(colors=c("blue", "red", "purple"), strokeWidth=2)
# Add shading
for (i in 1:(NROW(indic)-1)) {
    dyplot <- dyplot %>%
dyShading(from=dates[i], to=dates[i+1], color=indic[i])
}  # end for
# Render the dygraph object
dyplot
# Plot VTI and VWAP strategy using quantmod
quantmod::chart_Series(x=cbind(cum_rets, cum_pnls),
  name="VWAP Crossover Strategy for VTI", theme=plot_theme)
add_TA(position_s > 0, on=-1, col="lightgreen", border="lightgreen")
add_TA(position_s < 0, on=-1, col="lightgrey", border="lightgrey")
legend("top", legend=c(symbol, "VWAP strategy"), lty=1, lwd=6,
 cex=0.9, inset=0.1, bg="white", col=c("blue", "red"), bty="n")
# Calculate positions from lagged indicator
lagg <- 2
indic <- sign(cum_rets - vwapv)
indic_sum <- roll::roll_sum(indic, width=lagg)
indic_sum[1:lagg] <- 0
position_s <- rep(NA_integer_, NROW(closep))
position_s[1] <- 0
position_s <- ifelse(indic_sum == lagg, 1, position_s)
position_s <- ifelse(indic_sum == (-lagg), -1, position_s)
position_s <- zoo::na.locf(position_s, na.rm=FALSE)
# Lag the positions to trade in next period
position_s <- rutils::lagit(position_s, lagg=1)
# Calculate PnLs of lagged strategy
pnls <- returns*position_s
colnames(pnls) <- "Strategy"
cum_pnls_lag <- cumsum(pnls)
wealth <- cbind(cum_pnls, cum_pnls_lag)
colnames(wealth) <- c("Strategy", "Strategy_lag")
# Annualized Sharpe ratios of VWAP strategies
sharp_e <- sqrt(252)*sapply(cbind(returns, pnls),
  function (x) mean(x)/sd(x))
# Plot both strategies
dygraphs::dygraph(wealth, main=paste("VWAP Crossover Strategy, Sharpe", paste(paste(names(sharp_e), round(sharp_e, 3), sep="="), collapse=", "))) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=3)
# Calculate fast and slow VWAPs
vwap_fast <- TTR::VWAP(cum_rets, volume=volumes, n=20)
vwap_fast[1:20] <- 0
vwap_slow <- TTR::VWAP(cum_rets, volume=volumes, n=200)
vwap_slow[1:200] <- 0
# Calculate VWAP positions
position_s <- sign(vwap_fast - vwap_slow)
# Lag the positions to avoid data snooping
position_s <- rutils::lagit(position_s)
# Calculate daily profits and losses of strategy
pnls <- returns*position_s
colnames(pnls) <- "Strategy"
cum_pnls <- cumsum(pnls)
wealth <- cbind(cum_rets, cum_pnls, vwap_fast, vwap_slow)
colnames(wealth) <- c(symbol, "Strategy", "VWAP_fast", "VWAP_slow")
# Annualized Sharpe ratios of VTI and VWAP strategy
sharp_e <- sqrt(252)*sapply(cbind(returns, pnls),
  function (x) mean(x)/sd(x))
# Calculate index for background shading
indic <- (vwap_fast > vwap_slow)
whichv <- which(rutils::diffit(indic) != 0)
indic <- rbind(first(indic), indic[whichv, ], last(indic))
dates <- index(indic)
indic <- ifelse(drop(coredata(indic)), "lightgreen", "antiquewhite")
# Create dygraph object without rendering it
dyplot <- dygraphs::dygraph(wealth, main=paste("VWAP Crossover Strategy, Sharpe", paste(paste(names(sharp_e), round(sharp_e, 3), sep="="), collapse=", "))) %>%
  dyOptions(colors=c("blue", "red", "purple", "lightpurple"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Add shading
for (i in 1:(NROW(indic)-1)) {
    dyplot <- dyplot %>%
dyShading(from=dates[i], to=dates[i+1], color=indic[i])
}  # end for
# Render the dygraph object
dyplot
# Calculate correlation of VWAP strategy with VTI
cor(pnls, returns)
# Combine VWAP strategy with VTI
wealth <- cbind(returns, pnls, 0.5*(returns+pnls))
colnames(wealth) <- c(symbol, "VWAP", "Combined")
sharp_e <- sqrt(252)*sapply(wealth, function (x) mean(x)/sd(x))
# Plot dygraph of VWAP strategy combined with VTI
dygraphs::dygraph(cumsum(wealth),
  main=paste("VWAP Crossover Strategy, Sharpe", paste(paste(names(sharp_e), round(sharp_e, 3), sep="="), collapse=", "))) %>%
  dyOptions(colors=c("blue", "purple", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Define Brownian Motion parameters
nrows <- 1000; sigmav <- 0.01
# Simulate 5 paths of Brownian motion
prices <- matrix(rnorm(5*nrows, sd=sigmav), nc=5)
prices <- matrixStats::colCumsums(prices)
# Open plot window on Mac
dev.new(width=6, height=4, noRStudioGD=TRUE)
# Set plot parameters to reduce whitespace around plot
par(mar=c(2, 2, 3, 1), oma=c(0, 0, 0, 0))
# Plot 5 paths of Brownian motion
matplot(y=prices, main="Brownian Motion Paths",
  xlab="", ylab="", type="l", lty="solid", lwd=1, col="blue")
# Save plot to png file on Mac
quartz.save("figure/brown_paths.png", type="png", width=6, height=4)
# Define Ornstein-Uhlenbeck parameters
eq_price <- 1.0; sigmav <- 0.02
thetav <- 0.01; nrows <- 1000
# Initialize the data
innov <- rnorm(nrows)
returns <- numeric(nrows)
prices <- numeric(nrows)
# Simulate Ornstein-Uhlenbeck process in R
prices[1] <- sigmav*innov[1]
for (i in 2:nrows) {
  returns[i] <- thetav*(eq_price - prices[i-1]) +
    sigmav*innov[i]
  prices[i] <- prices[i-1] + returns[i]
}  # end for
# Simulate Ornstein-Uhlenbeck process in Rcpp
prices_cpp <- HighFreq::sim_ou(eq_price=eq_price, volat=sigmav,
  theta=thetav, innov=matrix(innov))
all.equal(prices, drop(prices_cpp))
# Compare the speed of R code with Rcpp
library(microbenchmark)
summary(microbenchmark(
  Rcode={for (i in 2:nrows) {
    returns[i] <- thetav*(eq_price - prices[i-1]) + sigmav*innov[i]
    prices[i] <- prices[i-1] + returns[i]}},
  Rcpp=HighFreq::sim_ou(eq_price=eq_price, volat=sigmav,
    theta=thetav, innov=matrix(innov)),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
plot(prices, type="l",
     xlab="time", ylab="prices",
     main="Ornstein-Uhlenbeck Process")
legend("topright",
 title=paste(c(paste0("sigmav = ", sigmav),
               paste0("eq_price = ", eq_price),
               paste0("thetav = ", thetav)),
             collapse="\n"),
 legend="", cex=0.8, inset=0.1, bg="white", bty="n")
abline(h=eq_price, col='red', lwd=2)
returns <- rutils::diffit(prices)
lag_prices <- rutils::lagit(prices)
formulav <- returns ~ lag_prices
l_m <- lm(formulav)
summary(l_m)
# Plot regression
plot(formulav, main="OU Returns Versus Lagged Prices")
abline(l_m, lwd=2, col="red")
# Calculate volatility parameter
c(volatility=sigmav, estimate=sd(returns))
# Extract OU parameters from regression
coeff <- summary(l_m)$coefficients
# Calculate regression alpha and beta directly
betav <- cov(returns, lag_prices)/var(lag_prices)
alpha <- (mean(returns) - betav*mean(lag_prices))
cbind(direct=c(alpha=alpha, beta=betav), lm=coeff[, 1])
all.equal(c(alpha=alpha, beta=betav), coeff[, 1],
    check.attributes=FALSE)
# Calculate regression standard errors directly
betas <- c(alpha=alpha, beta=betav)
fit_ted <- (alpha + betav*lag_prices)
residuals <- (returns - fit_ted)
prices_squared <- sum((lag_prices - mean(lag_prices))^2)
betasd <- sqrt(sum(residuals^2)/prices_squared/(nrows-2))
alpha_sd <- sqrt(sum(residuals^2)/(nrows-2)*(1/nrows + mean(lag_prices)^2/prices_squared))
cbind(direct=c(alpha_sd=alpha_sd, betasd=betasd), lm=coeff[, 2])
all.equal(c(alpha_sd=alpha_sd, betasd=betasd), coeff[, 2],
    check.attributes=FALSE)
# Compare mean reversion parameter theta
c(theta=(-thetav), round(coeff[2, ], 3))
# Compare equilibrium price mu
c(eq_price=eq_price, estimate=-coeff[1, 1]/coeff[2, 1])
# Compare actual and estimated parameters
coeff <- cbind(c(thetav*eq_price, -thetav), coeff[, 1:2])
rownames(coeff) <- c("drift", "theta")
colnames(coeff)[1] <- "actual"
round(coeff, 4)
# Simulate Schwartz process
returns <- numeric(nrows)
prices <- numeric(nrows)
prices[1] <- exp(sigmav*innov[1])
set.seed(1121)  # Reset random numbers
for (i in 2:nrows) {
  returns[i] <- thetav*(eq_price - prices[i-1]) + sigmav*innov[i]
  prices[i] <- prices[i-1]*exp(returns[i])
}  # end for
plot(prices, type="l", xlab="time", ylab="prices",
     main="Schwartz Process")
legend("topright",
 title=paste(c(paste0("sigmav = ", sigmav),
               paste0("eq_price = ", eq_price),
               paste0("thetav = ", thetav)),
             collapse="\n"),
 legend="", cex=0.8, inset=0.12, bg="white", bty="n")
abline(h=eq_price, col='red', lwd=2)
x11(width=6, height=5)
par(mar=c(3, 2, 1, 1), oma=c(1, 0, 0, 0))
returns <- na.omit(rutils::etfenv$returns$VTI)
# Plot autocorrelations using stats::acf()
stats::acf(returns, lag=10, xlab="lag", main="")
title(main="ACF of VTI Returns", line=-1)
# Two-tailed 95% confidence interval
qnorm(0.975)/sqrt(NROW(returns))
# Ljung-Box test for VTI returns
# 'lag' is the number of autocorrelation coefficients
Box.test(returns, lag=10, type="Ljung")
library(Ecdat)  # Load Ecdat
macro_zoo <- as.zoo(Macrodat[, c("lhur", "fygm3")])
colnames(macro_zoo) <- c("unemprate", "3mTbill")
macro_diff <- na.omit(diff(macro_zoo))
# Changes in 3 month T-bill rate are autocorrelated
Box.test(macro_diff[, "3mTbill"], lag=10, type="Ljung")
# Changes in unemployment rate are autocorrelated
Box.test(macro_diff[, "unemprate"], lag=10, type="Ljung")
# Get the ACF data returned invisibly
acf_data <- acf(returns, plot=FALSE)
summary(acf_data)
# Print the ACF data
print(acf_data)
dim(acf_data$acf)
dim(acf_data$lag)
head(acf_data$acf)
plot_acf <- function(xtes, lagg=10, plotobj=TRUE,
               xlab="Lag", ylab="", main="", ...) {
  # Calculate the ACF without a plot
  acf_data <- acf(x=xtes, lag.max=lagg, plot=FALSE, ...)
  # Remove first element of ACF data
  acf_data$acf <- array(data=acf_data$acf[-1],
    dim=c((dim(acf_data$acf)[1]-1), 1, 1))
  acf_data$lag <- array(data=acf_data$lag[-1],
    dim=c((dim(acf_data$lag)[1]-1), 1, 1))
  # Plot ACF
  if (plotobj) {
    ci <- qnorm((1+0.95)/2)*sqrt(1/NROW(xtes))
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
rutils::plot_acf(returns, lag=10, main="")
title(main="ACF of VTI returns", line=-1)
# Ljung-Box test for VTI returns
Box.test(returns, lag=10, type="Ljung")
x11(width=6, height=7)
par(mfrow=c(2,1))  # Set plot panels
par(mar=c(3, 3, 2, 2), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
# Autocorrelation of squared random returns
rutils::plot_acf(rnorm(NROW(returns))^2, lag=10, main="")
title(main="ACF of Squared Random Returns", line=-1)
# Autocorrelation of squared VTI returns
rutils::plot_acf(returns^2, lag=10, main="")
title(main="ACF of Squared VTI Returns", line=-1)
# Ljung-Box test for squared VTI returns
Box.test(returns^2, lag=10, type="Ljung")
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
dates <- Sys.Date() + 0:728  # Two year daily series
# AR time series of returns
arimav <- xts(x=arima.sim(n=NROW(dates), model=list(ar=0.2)),
          order.by=dates)
arimav <- cbind(arimav, cumsum(arimav))
colnames(arimav) <- c("AR returns", "AR prices")
library(ggplot2)  # Load ggplot2
library(gridExtra)  # Load gridExtra
autoplot(object=arimav, # ggplot AR process
 facets="Series ~ .",
 main="Autoregressive process (phi=0.2)") +
  facet_grid("Series ~ .", scales="free_y") +
  xlab("") + ylab("") +
theme(legend.position=c(0.1, 0.5),
  plot.background=element_blank(),
  axis.text.y=element_blank())
ar_coeff <- c(-0.9, 0.01, 0.9)  # AR coefficients
# Create three AR time series
arimav <- sapply(ar_coeff, function(phi) {
  set.seed(1121)  # Reset random numbers
  arima.sim(n=NROW(dates), model=list(ar=phi))
})  # end sapply
colnames(arimav) <- paste("autocorr", ar_coeff)
plot.zoo(arimav, main="AR(1) prices", xlab=NA)
# Or plot using ggplot
arimav <- xts(x=arimav, order.by=dates)
library(ggplot)
autoplot(arimav, main="AR(1) prices",
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
coeff <- c(0.1, 0.39, 0.5)
nrows <- 1e2
set.seed(1121); innov <- rnorm(nrows)
# Simulate AR process using recursive loop in R
arimav <- numeric(NROW(innov))
arimav[1] <- innov[1]
arimav[2] <- coeff[1]*arimav[1] + innov[2]
arimav[3] <- coeff[1]*arimav[2] + coeff[2]*arimav[1] + innov[3]
for (it in 4:NROW(arimav)) {
  arimav[it] <- arimav[(it-1):(it-3)] %*% coeff + innov[it]
}  # End for
# Simulate AR process using filter()
arima_faster <- filter(x=innov, filter=coeff, method="recursive")
class(arima_faster)
all.equal(arimav, as.numeric(arima_faster))
# Fast simulation of AR process using C_rfilter()
arima_fastest <- .Call(stats:::C_rfilter, innov, coeff,
                 double(NROW(coeff) + NROW(innov)))[-(1:3)]
all.equal(arimav, arima_fastest)
# Calculate modulus of roots of characteristic equation
root_s <- Mod(polyroot(c(1, -coeff)))
# Calculate warmup period
warm_up <- NROW(coeff) + ceiling(6/log(min(root_s)))
set.seed(1121)
nrows <- 1e4
innov <- rnorm(nrows + warm_up)
# Simulate AR process using arima.sim()
arimav <- arima.sim(n=nrows,
  model=list(ar=coeff),
  start.innov=innov[1:warm_up],
  innov=innov[(warm_up+1):NROW(innov)])
# Simulate AR process using filter()
arima_fast <- filter(x=innov, filter=coeff, method="recursive")
all.equal(arima_fast[-(1:warm_up)], as.numeric(arimav))
# Benchmark the speed of the three methods of simulating AR process
library(microbenchmark)
summary(microbenchmark(
  filter=filter(x=innov, filter=coeff, method="recursive"),
  arima_sim=arima.sim(n=nrows,
                  model=list(ar=coeff),
                  start.innov=innov[1:warm_up],
                  innov=innov[(warm_up+1):NROW(innov)]),
  arima_loop={for (it in 4:NROW(arimav)) {
  arimav[it] <- arimav[(it-1):(it-3)] %*% coeff + innov[it]}}
  ), times=10)[, c(1, 4, 5)]
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0))
# Simulate AR(1) process
arimav <- arima.sim(n=1e3, model=list(ar=0.8))
# ACF of AR(1) process
acfd <- rutils::plot_acf(arimav, lag=10, xlab="", ylab="",
  main="Autocorrelations of AR(1) process")
acfd$acf[1:5]
# PACF of AR(1) process
pacfd <- pacf(arimav, lag=10, xlab="", ylab="", main="")
title("Partial autocorrelations of AR(1) process", line=1)
pacfd <- drop(pacfd$acf)
pacfd[1:5]
# Compute pacf recursively from acf
acfd <- rutils::plot_acf(arimav, lag=10, plotobj=FALSE)
acfd <- drop(acfd$acf)
pacfd <- numeric(3)
pacfd[1] <- acfd[1]
pacfd[2] <- acfd[2] - acfd[1]^2
pacfd[3] <- acfd[3] - pacfd[2]*acfd[1] - acfd[2]*pacfd[1]
# Compute pacf recursively in a loop
pacfd <- numeric(NROW(acfd))
pacfd[1] <- acfd[1]
for (it in 2:NROW(pacfd)) {
  pacfd[it] <- acfd[it] - pacfd[1:(it-1)] %*% acfd[(it-1):1]
}  # end for
par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # Set plot panels
# Simulate AR process of returns
arimav <- arima.sim(n=1e3, model=list(ar=c(0.1, 0.5, 0.1)))
# ACF of AR(3) process
rutils::plot_acf(arimav, lag=10, xlab="", ylab="",
   main="ACF of AR(3) process")
# PACF of AR(3) process
pacf(arimav, lag=10, xlab="", ylab="", main="PACF of AR(3) process")
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
arimav <- arima.sim(n=nrows, model=list(ar=0.99))
tseries::adf.test(arimav)
# Integrated series has unit root
tseries::adf.test(cumsum(arimav))
# Simulate arima with negative AR coefficient
set.seed(1121)
arimav <- arima.sim(n=nrows, model=list(ar=-0.99))
tseries::adf.test(arimav)
# Integrated series has unit root
tseries::adf.test(cumsum(arimav))
# Simulate random walks using apply() loops
set.seed(1121)  # Initialize random number generator
rand_walks <- matrix(rnorm(1000*100), ncol=1000)
rand_walks <- apply(rand_walks, 2, cumsum)
variance <- apply(rand_walks, 1, var)
# Simulate random walks using vectorized functions
set.seed(1121)  # Initialize random number generator
rand_walks <- matrixStats::colCumsums(matrix(rnorm(1000*100), ncol=1000))
variance <- matrixStats::rowVars(rand_walks)
par(mar=c(5, 3, 2, 2), oma=c(0, 0, 0, 0))
plot(variance, xlab="time steps", ylab="",
     t="l", col="blue", lwd=2,
     main="Variance of Random Walk")
# Define Dickey-Fuller parameters
eq_price <- 1.0; sigmav <- 0.02
thetav <- 0.01; nrows <- 1000
# Initialize the data
innov <- rnorm(nrows)
returns <- numeric(nrows)
prices <- numeric(nrows)
# Simulate Dickey-Fuller process in R
prices[1] <- sigmav*innov[1]
for (i in 2:nrows) {
  returns[i] <- thetav*(eq_price - prices[i-1]) + sigmav*innov[i]
  prices[i] <- prices[i-1] + returns[i]
}  # end for
# Simulate Dickey-Fuller process in Rcpp
prices_cpp <- HighFreq::sim_ou(eq_price=eq_price, volat=sigmav,
    theta=thetav, innov=matrix(innov))
all.equal(prices, drop(prices_cpp))
# Compare the speed of R code with Rcpp
library(microbenchmark)
summary(microbenchmark(
  Rcode={for (i in 2:nrows) {
    returns[i] <- thetav*(eq_price - prices[i-1]) + sigmav*innov[i]
    prices[i] <- prices[i-1] + returns[i]}},
  Rcpp=HighFreq::sim_ou(eq_price=eq_price, volat=sigmav, theta=thetav, innov=matrix(innov)),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
set.seed(1121); innov <- rnorm(1e4, sd=0.01)
# Simulate AR(1) process with coefficient=1, with unit root
arimav <- filter(x=innov, filter=1.0, method="recursive")
x11(); plot(arimav, t="l", main="AR(1) coefficient = 1.0")
# Perform ADF test with lag = 1
tseries::adf.test(arimav, k=1)
# Perform standard Dickey-Fuller test
tseries::adf.test(arimav, k=0)
# Simulate AR(1) with coefficient close to 1, without unit root
arimav <- filter(x=innov, filter=0.99, method="recursive")
x11(); plot(arimav, t="l", main="AR(1) coefficient = 0.99")
tseries::adf.test(arimav, k=1)
# Simulate Ornstein-Uhlenbeck OU process with mean reversion
eq_price <- 0.0; thetav <- 0.001
prices <- HighFreq::sim_ou(eq_price=eq_price, volat=1.0,
  theta=thetav, innov=innov)
x11(); plot(prices, t="l", main=paste("OU coefficient =", thetav))
tseries::adf.test(prices, k=1)
# Simulate Ornstein-Uhlenbeck OU process with zero reversion
thetav <- 0.0
prices <- HighFreq::sim_ou(eq_price=eq_price, volat=1.0,
  theta=thetav, innov=innov)
x11(); plot(prices, t="l", main=paste("OU coefficient =", thetav))
tseries::adf.test(prices, k=1)
