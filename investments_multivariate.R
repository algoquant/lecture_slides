# Load constant maturity Treasury rates
load(file="/Users/jerzy/Develop/lecture_slides/data/rates_data.RData")
# Combine rates into single xts series
rates <- do.call(cbind, as.list(rates_env))
# Sort the columns of rates according bond maturity
namesv <- colnames(rates)
namesv <- substr(namesv, start=4, stop=10)
namesv <- as.numeric(namesv)
indeks <- order(namesv)
rates <- rates[, indeks]
# Align rates dates with VTI prices
closep <- log(quantmod::Cl(rutils::etfenv$VTI))
colnames(closep) <- "VTI"
nrows <- NROW(closep)
dates <- zoo::index(closep)
rates <- na.omit(rates[dates])
closep <- closep[zoo::index(rates)]
dates <- zoo::index(closep)
# Calculate VTI returns and IR changes
returns <- rutils::diffit(closep)
rates_diff <- rutils::diffit(log(rates))
# Regress VTI returns versus the lagged rate differences
predictor <- rutils::lagit(rates_diff)
model <- lm(returns ~ predictor)
summary(model)
# Regress VTI returns before and after 2012
summary(lm(returns["/2012"] ~ predictor["/2012"]))
summary(lm(returns["2012/"] ~ predictor["2012/"]))
# Calculate PCA of rates correlation matrix
eigend <- eigen(cor(rates_diff))
rates_pca <- -rates_diff %*% eigend$vectors
colnames(rates_pca) <- paste0("PC", 1:6)
# Define predictor as the YC PCAs
predictor <- rutils::lagit(rates_pca)
model <- lm(returns ~ predictor)
summary(model)
# Plot YC steepener principal component with VTI
datav <- cbind(returns, rates_pca[, 2])
colnames(datav) <- c("VTI", "Steepener")
colnames <- colnames(datav)
dygraphs::dygraph(cumsum(datav), main="VTI and Yield Curve Steepener") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", label=colnames[1], strokeWidth=1, col="blue") %>%
  dySeries(name=colnames[2], axis="y2", label=colnames[2], strokeWidth=1, col="red")
# Define predictor with intercept term
predictor <- rutils::lagit(rates_diff)
predictor <- cbind(rep(1, NROW(predictor)), predictor)
colnames(predictor)[1] <- "intercept"
# Calculate inverse of predictor
inverse <- MASS::ginv(predictor)
# Calculate coefficients from response and inverse of predictor
response <- returns
coeff <- drop(inverse %*% response)
# Calculate forecasts and pnls in-sample
forecasts <- (predictor %*% coeff)
pnls <- sign(forecasts)*response
# Calculate in-sample factors
factors <- (predictor * coeff)
apply(factors, 2, sd)
# Plot dygraph of in-sample IR strategy
wealth <- cbind(returns, pnls)
colnames(wealth) <- c("VTI", "Strategy")
colnames <- colnames(wealth)
dygraphs::dygraph(cumsum(wealth), main="Yield Curve Strategy In-sample") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=colnames[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Define in-sample and out-of-sample intervals
in_sample <- (dates < as.Date("2020-01-01"))
out_sample <- (dates >= as.Date("2020-01-01"))
# Calculate inverse of predictor in-sample
inverse <- MASS::ginv(predictor[in_sample, ])
# Calculate coefficients in-sample
coeff <- drop(inverse %*% response[in_sample, ])
# Calculate forecasts and pnls out-of-sample
forecasts <- (predictor[out_sample, ] %*% coeff)
pnls <- sign(forecasts)*response[out_sample, ]
# Plot dygraph of out-of-sample IR PCA strategy
wealth <- cbind(returns[out_sample, ], pnls)
colnames(wealth) <- c("VTI", "Strategy")
colnames <- colnames(wealth)
dygraphs::dygraph(cumsum(wealth), main="Yield Curve Strategy Out-of-Sample") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=colnames[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Define yearly dates
format(dates[1], "%Y")
years <- paste0(seq(2001, 2022, 1), "-01-01")
years <- as.Date(years)
# Perform loop over yearly dates
pnls <- lapply(3:(NROW(years)-1), function(i) {
  # Define in-sample and out-of-sample intervals
  in_sample <- (dates > years[i-1]) & (dates < years[i])
  out_sample <- (dates >= years[i]) & (dates < years[i+1])
  # Calculate coefficients in-sample
  inverse <- MASS::ginv(predictor[in_sample, ])
  coeff <- drop(inverse %*% response[in_sample, ])
  # Calculate forecasts and pnls out-of-sample
  forecasts <- (predictor[out_sample, ] %*% coeff)
  sign(forecasts)*response[out_sample, ]
})  # end lapply
pnls <- do.call(rbind, pnls)
# Plot dygraph of rolling yearly IR strategy
vtis <- rutils::diffit(closep[zoo::index(pnls),])
wealth <- cbind(vtis, pnls)
colnames(wealth) <- c("VTI", "Strategy")
colnames <- colnames(wealth)
dygraphs::dygraph(cumsum(wealth), main="Rolling Yearly Yield Curve Strategy") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=colnames[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Define monthly dates
format(dates[1], "%m-%Y")
format(dates[NROW(dates)], "%m-%Y")
months <- seq.Date(from=as.Date("2001-05-01"), to=as.Date("2021-04-01"), by="month")
# Perform loop over monthly dates
pnls <- lapply(12:(NROW(months)-1), function(i) {
  # Define in-sample and out-of-sample intervals
  in_sample <- (dates > months[i-11]) & (dates < months[i])
  out_sample <- (dates > months[i]) & (dates < months[i+1])
  # Calculate forecasts and pnls out-of-sample
  inverse <- MASS::ginv(predictor[in_sample, ])
  coeff <- drop(inverse %*% response[in_sample, ])
  forecasts <- (predictor[out_sample, ] %*% coeff)
  sign(forecasts)*response[out_sample, ]
})  # end lapply
pnls <- do.call(rbind, pnls)
# Plot dygraph of rolling monthly IR strategy
vtis <- rutils::diffit(closep[zoo::index(pnls),])
wealth <- cbind(vtis, pnls)
colnames(wealth) <- c("VTI", "Strategy")
colnames <- colnames(wealth)
dygraphs::dygraph(cumsum(wealth), main="Rolling Monthly Yield Curve Strategy") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=colnames[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Define weekly dates
weeks <- seq.Date(from=as.Date("2001-05-01"), to=as.Date("2021-04-01"), by="weeks")
# Perform loop over weekly dates
pnls <- lapply(51:(NROW(weeks)-1), function(i) {
  # Define in-sample and out-of-sample intervals
  in_sample <- (dates > weeks[i-10]) & (dates < weeks[i])
  out_sample <- (dates > weeks[i]) & (dates < weeks[i+1])
  # Calculate forecasts and pnls out-of-sample
  inverse <- MASS::ginv(predictor[in_sample, ])
  coeff <- drop(inverse %*% response[in_sample, ])
  forecasts <- (predictor[out_sample, ] %*% coeff)
  sign(forecasts)*response[out_sample, ]
})  # end lapply
pnls <- do.call(rbind, pnls)
# Plot dygraph of rolling weekly IR strategy
vtis <- rutils::diffit(closep[zoo::index(pnls),])
wealth <- cbind(vtis, pnls)
colnames(wealth) <- c("VTI", "Strategy")
colnames <- colnames(wealth)
dygraphs::dygraph(cumsum(wealth), main="Rolling Weekly Yield Curve Strategy") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=colnames[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Calculate singular value decomposition of the predictor matrix
svdec <- svd(predictor)
barplot(svdec$d, main="Singular Values of YC Predictor Matrix")
# Calculate generalized inverse from SVD
inverse <- svdec$v %*% (t(svdec$u) / svdec$d)
# Verify inverse property of inverse
all.equal(zoo::coredata(predictor),
    predictor %*% inverse %*% predictor)
# Calculate generalized inverse using MASS::ginv()
inverse_ginv <- MASS::ginv(predictor)
all.equal(inverse_ginv, inverse)
# Set tolerance for determining zero singular values
precision <- sqrt(.Machine$double.eps)
# Check for zero singular values
round(svdec$d, 12)
not_zero <- (svdec$d > (precision * svdec$d[1]))
# Calculate regularized inverse from SVD
inv_reg <- svdec$v[, not_zero] %*%
  (t(svdec$u[, not_zero]) / svdec$d[not_zero])
# Verify inverse property of inv_reg
all.equal(zoo::coredata(predictor),
    predictor %*% inv_reg %*% predictor)
all.equal(inv_reg, inverse)
# Calculate shrinkage inverse from SVD
eigen_max <- 3
inv_shrink <- svdec$v[, 1:eigen_max] %*%
  (t(svdec$u[, 1:eigen_max]) / svdec$d[1:eigen_max])
# Inverse property fails for inv_shrink
all.equal(zoo::coredata(predictor),
    predictor %*% inv_shrink %*% predictor)
# Calculate shrinkage inverse using RcppArmadillo
inverse_rcpp <- HighFreq::calc_inv(predictor, eigen_max=eigen_max)
all.equal(inv_shrink, inverse_rcpp, check.attributes=FALSE)
# Calculate in-sample pnls for different eigen_max values
eigen_maxs <- 2:7
pnls <- lapply(eigen_maxs, function(eigen_max) {
  inverse <- HighFreq::calc_inv(predictor, eigen_max=eigen_max)
  coeff <- drop(inverse %*% response)
  forecasts <- (predictor %*% coeff)
  sign(forecasts)*response
})
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("eigen", eigen_maxs)
# Plot dygraph of in-sample pnls
colors <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls), main="In-Sample Returns of Shrinkage YC Strategies") %>%
  dyOptions(colors=colors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Define in-sample and out-of-sample intervals
in_sample <- (dates < as.Date("2020-01-01"))
out_sample <- (dates >= as.Date("2020-01-01"))
# Calculate in-sample pnls for different eigen_max values
eigen_maxs <- 2:7
pnls <- lapply(eigen_maxs, function(x) {
  inverse <- HighFreq::calc_inv(predictor[in_sample, ], eigen_max=x)
  coeff <- drop(inverse %*% response[in_sample, ])
  forecasts <- (predictor[out_sample, ] %*% coeff)
  sign(forecasts)*response[out_sample, ]
})
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("eigen", eigen_maxs)
# Plot dygraph of out-of-sample pnls
colors <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls), main="Out-of-Sample Returns of Shrinkage YC Strategies") %>%
  dyOptions(colors=colors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Define monthly dates
format(dates[1], "%m-%Y")
format(dates[NROW(dates)], "%m-%Y")
months <- seq.Date(from=as.Date("2001-05-01"), to=as.Date("2021-04-01"), by="month")
# Perform loop over monthly dates
look_back <- 6
eigen_max <- 3
pnls <- lapply((look_back+1):(NROW(months)-1), function(i) {
  # Define in-sample and out-of-sample intervals
  in_sample <- (dates > months[i-look_back]) & (dates < months[i])
  out_sample <- (dates > months[i]) & (dates < months[i+1])
  # Calculate forecasts and pnls out-of-sample
  inverse <- HighFreq::calc_inv(predictor[in_sample, ], eigen_max=eigen_max)
  coeff <- drop(inverse %*% response[in_sample, ])
  forecasts <- (predictor[out_sample, ] %*% coeff)
  sign(forecasts)*response[out_sample, ]
})  # end lapply
pnls <- do.call(rbind, pnls)
# Plot dygraph of rolling monthly IR strategy
vtis <- rutils::diffit(closep[zoo::index(pnls),])
wealth <- cbind(vtis, pnls)
colnames(wealth) <- c("VTI", "Strategy")
colnames <- colnames(wealth)
dygraphs::dygraph(cumsum(wealth), main="Rolling Monthly Shrinkage YC Strategy") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=colnames[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Define weekly dates
weeks <- seq.Date(from=as.Date("2001-05-01"), to=as.Date("2021-04-01"), by="weeks")
# Perform loop over weekly dates
look_back <- 4
eigen_max <- 4
pnls <- lapply((look_back+1):(NROW(weeks)-1), function(i) {
  # Define in-sample and out-of-sample intervals
  in_sample <- (dates > weeks[i-look_back]) & (dates < weeks[i])
  out_sample <- (dates > weeks[i]) & (dates < weeks[i+1])
  # Calculate forecasts and pnls out-of-sample
  inverse <- HighFreq::calc_inv(predictor[in_sample, ], eigen_max=eigen_max)
  coeff <- drop(inverse %*% response[in_sample, ])
  forecasts <- (predictor[out_sample, ] %*% coeff)
  sign(forecasts)*response[out_sample, ]
})  # end lapply
pnls <- do.call(rbind, pnls)
# Plot dygraph of rolling weekly IR strategy
vtis <- rutils::diffit(closep[zoo::index(pnls),])
wealth <- cbind(vtis, pnls)
colnames(wealth) <- c("VTI", "Strategy")
colnames <- colnames(wealth)
dygraphs::dygraph(cumsum(wealth), main="Rolling Weekly Shrinkage YC Strategy") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=colnames[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Load the yield curve data
load(file="/Users/jerzy/Develop/lecture_slides/data/rates_data.RData")
rates <- do.call(cbind, as.list(rates_env))
namesv <- colnames(rates)
namesv <- substr(namesv, start=4, stop=10)
namesv <- as.numeric(namesv)
indeks <- order(namesv)
rates <- rates[, indeks]
closep <- log(quantmod::Cl(rutils::etfenv$VTI))
colnames(closep) <- "VTI"
nrows <- NROW(closep)
dates <- zoo::index(closep)
rates <- na.omit(rates[dates])
closep <- closep[zoo::index(rates)]
dates <- zoo::index(closep)
returns <- rutils::diffit(closep)
rates_diff <- rutils::diffit(log(rates))
# Create a combined predictor matrix
order_max <- 5
predictor <- sapply(1:order_max, rutils::lagit, input=as.numeric(returns))
colnames(predictor) <- paste0("retslag", 1:NCOL(predictor))
predictor <- cbind(predictor, rutils::lagit(rates_diff))
predictor <- cbind(rep(1, NROW(predictor)), predictor)
colnames(predictor)[1] <- "intercept"
response <- returns
# Calculate in-sample pnls for different eigen_max values
eigen_maxs <- 2:11
pnls <- lapply(eigen_maxs, function(eigen_max) {
  inverse <- HighFreq::calc_inv(predictor, eigen_max=eigen_max)
  coeff <- drop(inverse %*% response)
  forecasts <- (predictor %*% coeff)
  sign(forecasts)*response
})
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("eigen", eigen_maxs)
# Plot dygraph of in-sample pnls
colors <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls), main="In-Sample Returns of Combined Strategies With Shrinkage") %>%
  dyOptions(colors=colors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Define in-sample and out-of-sample intervals
in_sample <- (dates < as.Date("2020-01-01"))
out_sample <- (dates >= as.Date("2020-01-01"))
# Calculate in-sample pnls for different eigen_max values
eigen_maxs <- 2:11
pnls <- lapply(eigen_maxs, function(x) {
  inverse <- HighFreq::calc_inv(predictor[in_sample, ], eigen_max=x)
  coeff <- drop(inverse %*% response[in_sample, ])
  forecasts <- (predictor[out_sample, ] %*% coeff)
  sign(forecasts)*response[out_sample, ]
})
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("eigen", eigen_maxs)
# Plot dygraph of out-of-sample pnls
colors <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls), main="Out-of-Sample Returns of Combined Strategies With Shrinkage") %>%
  dyOptions(colors=colors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Define monthly dates
format(dates[1], "%m-%Y")
format(dates[NROW(dates)], "%m-%Y")
months <- seq.Date(from=as.Date("2001-05-01"), to=as.Date("2021-04-01"), by="month")
# Perform loop over monthly dates
look_back <- 6
eigen_max <- 3
pnls <- lapply((look_back+1):(NROW(months)-1), function(i) {
  # Define in-sample and out-of-sample intervals
  in_sample <- (dates > months[i-look_back]) & (dates < months[i])
  out_sample <- (dates > months[i]) & (dates < months[i+1])
  # Calculate forecasts and pnls out-of-sample
  inverse <- HighFreq::calc_inv(predictor[in_sample, ], eigen_max=eigen_max)
  coeff <- drop(inverse %*% response[in_sample, ])
  forecasts <- (predictor[out_sample, ] %*% coeff)
  sign(forecasts)*response[out_sample, ]
})  # end lapply
pnls <- do.call(rbind, pnls)
# Plot dygraph of rolling monthly IR strategy
vtis <- rutils::diffit(closep[zoo::index(pnls),])
wealth <- cbind(vtis, pnls)
colnames(wealth) <- c("VTI", "Strategy")
colnames <- colnames(wealth)
dygraphs::dygraph(cumsum(wealth), main="Rolling Monthly Shrinkage YC Strategy") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=colnames[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Define weekly dates
weeks <- seq.Date(from=as.Date("2001-05-01"), to=as.Date("2021-04-01"), by="weeks")
# Perform loop over weekly dates
look_back <- 8
eigen_max <- 4
pnls <- lapply((look_back+1):(NROW(weeks)-1), function(i) {
  # Define in-sample and out-of-sample intervals
  in_sample <- (dates > weeks[i-look_back]) & (dates < weeks[i])
  out_sample <- (dates > weeks[i]) & (dates < weeks[i+1])
  # Calculate forecasts and pnls out-of-sample
  inverse <- HighFreq::calc_inv(predictor[in_sample, ], eigen_max=eigen_max)
  coeff <- drop(inverse %*% response[in_sample, ])
  forecasts <- (predictor[out_sample, ] %*% coeff)
  sign(forecasts)*response[out_sample, ]
})  # end lapply
pnls <- do.call(rbind, pnls)
# Plot dygraph of rolling weekly IR strategy
vtis <- rutils::diffit(closep[zoo::index(pnls),])
wealth <- cbind(vtis, pnls)
colnames(wealth) <- c("VTI", "Strategy")
colnames <- colnames(wealth)
dygraphs::dygraph(cumsum(wealth), main="Rolling Weekly Shrinkage YC Strategy") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=colnames[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Find optimal nagg for predictor
naggs <- 5:100
tvalues <- sapply(naggs, function(nagg) {
  predictor <- roll::roll_mean(rates_diff, width=nagg, min_obs=1)
  predictor <- cbind(rep(1, NROW(predictor)), predictor)
  predictor <- rutils::lagit(predictor)
  model <- lm(response ~ predictor - 1)
  model_sum <- summary(model)
  max(abs(model_sum$coefficients[, 3][-1]))
})  # end sapply
naggs[which.max(tvalues)]
plot(naggs, tvalues, t="l", col="blue", lwd=2)
# Calculate aggregated predictor
nagg <- 53
predictor <- roll::roll_mean(rates_diff, width=nagg, min_obs=1)
predictor <- rutils::lagit(predictor)
predictor <- cbind(rep(1, NROW(predictor)), predictor)
model <- lm(response ~ predictor - 1)
summary(model)
# Calculate forecasts and pnls in-sample
inverse <- MASS::ginv(predictor)
coeff <- drop(inverse %*% response)
forecasts <- (predictor %*% coeff)
pnls <- sign(forecasts)*response
# Plot dygraph of in-sample IR strategy
wealth <- cbind(returns, pnls)
colnames(wealth) <- c("VTI", "Strategy")
colnames <- colnames(wealth)
dygraphs::dygraph(cumsum(wealth), main="Aggregated YC Strategy In-sample") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=colnames[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Define in-sample and out-of-sample intervals
in_sample <- (dates < as.Date("2020-01-01"))
out_sample <- (dates >= as.Date("2020-01-01"))
# Calculate forecasts and pnls out-of-sample
inverse <- MASS::ginv(predictor[in_sample, ])
coeff <- drop(inverse %*% response[in_sample, ])
forecasts <- (predictor[out_sample, ] %*% coeff)
pnls <- sign(forecasts)*response[out_sample, ]
# Plot dygraph of out-of-sample YC strategy
wealth <- cbind(returns[out_sample, ], pnls)
colnames(wealth) <- c("VTI", "Strategy")
colnames <- colnames(wealth)
dygraphs::dygraph(cumsum(wealth), main="Aggregated YC Strategy Out-of-Sample") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=colnames[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Extract ETF returns
symbols <- c("VTI", "IEF", "DBC")
returns <- rutils::etfenv$returns[, symbols]
returns <- na.omit(returns)
# Or, select rows with IEF data
# returns <- returns[index(rutils::etfenv$IEF)]
# Copy over NA values
# returns[1, is.na(returns[1, ])] <- 0
# returns <- zoo::na.locf(returns, na.rm=FALSE)
# Define end of month end points
endp <- rutils::calc_endpoints(returns, interval="months")
endp <- endp[-1]
nrows <- NROW(endp)
dates <- zoo::index(returns)[endp]
# Start points equal end points lagged by 12-month look-back interval
look_back <- 12
startp <- c(rep_len(1, look_back-1),
  endp[1:(nrows - look_back + 1)])
# Calculate matrix of look-back intervals
look_backs <- cbind(startp, endp)
colnames(look_backs) <- c("start", "end")
# Calculate matrix of look-forward intervals
look_fwds <- cbind(endp + 1, rutils::lagit(endp, -1))
look_fwds[nrows, ] <- endp[nrows]
colnames(look_fwds) <- c("start", "end")
# Inspect the intervals
head(cbind(look_backs, look_fwds))
tail(cbind(look_backs, look_fwds))
# Define performance function as Sharpe ratio
objfunc <- function(returns) sum(returns)/sd(returns)
# Calculate past performance over look-back intervals
past <- apply(look_backs, 1, function(ep) {
  sapply(returns[ep[1]:ep[2]], objfunc)
})  # end sapply
past <- t(past)
past[is.na(past)] <- 0
# Weights are proportional to past performance
weightv <- past
# weightv[weightv < 0] <- 0
# Scale weightv so sum of squares is equal to 1.
weightv <- weightv/sqrt(rowSums(weightv^2))
# Or scale weightv so sum is equal to 1
# weightv <- weightv/rowSums(weightv)
# Set NA values to zero
weightv[is.na(weightv)] <- 0
sum(is.na(weightv))
# Calculate future out-of-sample performance
future <- apply(look_fwds, 1, function(ep) {
  sapply(returns[ep[1]:ep[2]], sum)
})  # end sapply
future <- t(future)
future[is.na(future)] <- 0
tail(future)
# Calculate the momentum pnls
pnls <- rowSums(weightv*future)
# Lag the future and momentum returns to proper dates
future <- rutils::lagit(future)
pnls <- rutils::lagit(pnls)
# The momentum strategy has low correlation to stocks
cor(pnls, future)
# Define all-weather benchmark
weightsaw <- c(0.30, 0.55, 0.15)
all_weather <- future %*% weightsaw
# Calculate the wealth of momentum returns
wealth <- xts::xts(cbind(all_weather, pnls), order.by=dates)
colnames(wealth) <- c("All-Weather", "Momentum")
cor(wealth)
# Plot dygraph of the momentum strategy returns
dygraphs::dygraph(cumsum(wealth), main="Monthly Momentum Strategy vs All-Weather") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Calculate momentum profits and losses (returns)
pnls <- rowSums(weightv*future)
# Lag the momentum returns and weights
# to correspond with end of future interval
pnls <- rutils::lagit(pnls)
weightv <- rutils::lagit(weightv)
# bid_offer equal to 10 bps for liquid ETFs
bid_offer <- 0.001
# Calculate transaction costs
wealth <- cumsum(pnls)
costs <- 0.5*bid_offer*wealth*rowSums(abs(rutils::diffit(weightv)))
wealth <- cumsum(pnls - costs)
dates <- index(returns[endp])
wealth <- xts::xts(wealth, dates)
# Define all-weather benchmark
weightsaw <- c(0.30, 0.55, 0.15)
retaw <- returns %*% weightsaw
wealthaw <- cumsum(retaw)
wealthaw <- xts::xts(wealthaw[endp], dates)
# Plot the Momentum strategy and benchmark
wealth <- cbind(wealth, wealthaw)
colnames(wealth) <- c("Momentum Strategy", "Benchmark")
dygraphs::dygraph(wealth, main="Momentum Strategy") %>%
  dyAxis("y", label="Benchmark", independentTicks=TRUE) %>%
  dyAxis("y2", label="Momentum Strategy", independentTicks=TRUE) %>%
  dySeries(name="Momentum Strategy", axis="y2", label="Momentum Strategy", strokeWidth=2, col="red") %>%
  dySeries(name="Benchmark", axis="y", label="Benchmark", strokeWidth=2, col="blue")
# Define backtest functional
backtestmomentum <- function(returns,
                objfunc=function(returns) (sum(returns)/sd(returns)),
                look_back=12, re_balance="months", bid_offer=0.001,
                endp=rutils::calc_endpoints(returns, interval=re_balance)[-1],
                with_weights=FALSE, ...) {
  stopifnot("package:rutils" %in% search() || require("rutils", quietly=TRUE))
  # Define look-back and look-forward intervals
  nrows <- NROW(endp)
  startp <- c(rep_len(1, look_back-1), endp[1:(nrows-look_back+1)])
  # Calculate look-back intervals
  look_backs <- cbind(startp, endp)
  # Calculate look-forward intervals
  look_fwds <- cbind(endp + 1, rutils::lagit(endp, -1))
  look_fwds[nrows, ] <- endp[nrows]
  # Calculate past performance over look-back intervals
  past <- t(apply(look_backs, 1, function(ep) sapply(returns[ep[1]:ep[2]], objfunc)))
  past[is.na(past)] <- 0
  # Calculate future performance
  future <- t(apply(look_fwds, 1, function(ep) sapply(returns[ep[1]:ep[2]], sum)))
  future[is.na(future)] <- 0
  # Scale weightv so sum of squares is equal to 1
  weightv <- past
  weightv <- weightv/sqrt(rowSums(weightv^2))
  weightv[is.na(weightv)] <- 0  # Set NA values to zero
  # Calculate momentum profits and losses
  pnls <- rowSums(weightv*future)
  # Calculate transaction costs
  costs <- 0.5*bid_offer*cumprod(1 + pnls)*rowSums(abs(rutils::diffit(weightv)))
  pnls <- (pnls - costs)
  if (with_weights)
    rutils::lagit(cbind(pnls, weightv))
  else
    rutils::lagit(pnls)
}  # end backtestmomentum
source("/Users/jerzy/Develop/lecture_slides/scripts/back_test.R")
look_backs <- seq(3, 15, by=1)
objfunc <- function(returns) sum(returns)/sd(returns)
profilev <- sapply(look_backs, function(look_back) {
  pnls <- backtestmomentum(returns=returns, endp=endp,
    look_back=look_back, objfunc=objfunc)
  sum(pnls)
})  # end sapply
# Plot momemntum PnLs
x11(width=6, height=5)
plot(x=look_backs, y=profilev, t="l",
  main="Momemntum PnL as function of look_back",
  xlab="look_back (months)", ylab="pnl")
# Optimal look_back
look_back <- look_backs[which.max(profilev)]
pnls <- backtestmomentum(returns=returns,
  look_back=look_back, endp=endp,
  objfunc=objfunc, with_weights=TRUE)
tail(pnls)
# Calculate the wealth of momentum returns
retmom <- pnls[, 1]
wealth <- xts::xts(cbind(all_weather, retmom), order.by=dates)
colnames(wealth) <- c("All-Weather", "Momentum")
cor(wealth)
# Plot dygraph of the momentum strategy returns
dygraphs::dygraph(cumsum(wealth), main="Monthly Momentum Strategy vs All-Weather") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Or
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue")
quantmod::chart_Series(cumsum(wealth), theme=plot_theme, lwd=2,
       name="Momentum PnL")
legend("topleft", legend=colnames(wealth),
  inset=0.1, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")
# Plot the momentum portfolio weights
weightv <- pnls[, -1]
vtis <- log(quantmod::Cl(rutils::etfenv$VTI[dates]))
colnames(vtis) <- "VTI"
datav <- cbind(vtis, weightv)
datav <- na.omit(datav)
colnames(datav)[2:NCOL(pnls)] <- paste0(colnames(weightv), "_weight")
zoo::plot.zoo(datav, xlab=NULL, main="Momentum Weights")
# Calculate ETF betas
betas_etf <- sapply(returns, function(x)
  cov(returns$VTI, x)/var(x))
# Momentum beta is equal weights times ETF betas
betas <- weightv %*% betas_etf
betas <- xts::xts(betas, order.by=dates)
colnames(betas) <- "momentum_beta"
datav <- cbind(betas, vtis)
zoo::plot.zoo(datav,
  oma = c(3, 1, 3, 0), mar = c(0, 4, 0, 1),
  main="Momentum Beta & VTI Price", xlab="")
# Open x11 for plotting and set parameters to reduce whitespace around plot
x11(width=6, height=5)
par(mar=c(4, 4, 3, 1), oma=c(0, 0, 0, 0))
# Merton-Henriksson test
vtis <- rutils::diffit(vtis)
design <- cbind(VTI=vtis, 0.5*(vtis+abs(vtis)), vtis^2)
colnames(design)[2:3] <- c("merton", "treynor")
model <- lm(retmom ~ VTI + merton, data=design); summary(model)
# Treynor-Mazuy test
model <- lm(retmom ~ VTI + treynor, data=design); summary(model)
# Plot residual scatterplot
plot.default(x=vtis, y=retmom, xlab="VTI", ylab="momentum")
title(main="Treynor-Mazuy market timing test\n for Momentum vs VTI", line=0.5)
# Plot fitted (predicted) response values
points.default(x=vtis, y=model$fitted.values, pch=16, col="red")
residuals <- model$residuals
text(x=0.0, y=max(residuals), paste("Treynor test t-value =", round(summary(model)$coeff["treynor", "t value"], 2)))
# Standardize the returns
retmom_std <- (retmom-mean(retmom))/sd(retmom)
vtis <- (vtis-mean(vtis))/sd(vtis)
# Calculate skewness and kurtosis
apply(cbind(retmom_std, vtis), 2, function(x)
  sapply(c(skew=3, kurt=4),
    function(e) sum(x^e)))/nrows
# Plot histogram
hist(retmom_std, breaks=30,
  main="Momentum and VTI Return Distributions (standardized",
  xlim=c(-4, 4),
  xlab="", ylab="", freq=FALSE)
# Draw kernel density of histogram
lines(density(retmom_std), col='red', lwd=2)
lines(density(vtis), col='blue', lwd=2)
# Add legend
legend("topright", inset=0.05, cex=0.8, title=NULL,
 leg=c("Momentum", "VTI"),
 lwd=6, bg="white", col=c("red", "blue"))
# Combine momentum strategy with all-weather
all_weather <- sd(retmom)*all_weather/sd(all_weather)
wealth <- cbind(retmom, all_weather, 0.5*(retmom + all_weather))
colnames(wealth) <- c("momentum", "all_weather", "combined")
# Calculate strategy annualized Sharpe ratios
apply(wealth, MARGIN=2, function(x) {
  sqrt(12)*sum(x)/sd(x)/NROW(x)
})  # end apply
# Calculate strategy correlations
cor(wealth)
# Calculate cumulative wealth
wealth <- xts::xts(wealth, dates)
# Plot ETF momentum strategy combined with All-Weather
dygraphs::dygraph(cumsum(wealth), main="ETF Momentum Strategy Combined with All-Weather") %>%
  dyOptions(colors=c("red", "blue", "green"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Or
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("green", "blue", "red")
quantmod::chart_Series(wealth, theme=plot_theme,
       name="ETF Momentum Strategy Combined with All-Weather")
legend("topleft", legend=colnames(wealth),
  inset=0.1, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")
# Calculate rolling variance
look_back <- 252
variance <- roll::roll_var(returns, width=look_back, min_obs=1)
variance[1, ] <- 1
# Calculate rolling Sharpe
past <- roll::roll_mean(returns, width=look_back, min_obs=1)
weightv <- past/sqrt(variance)
weightv <- weightv/sqrt(rowSums(weightv^2))
weightv <- rutils::lagit(weightv)
sum(is.na(weightv))
# Calculate momentum profits and losses
pnls <- rowMeans(weightv*response)
# Calculate transaction costs
bid_offer <- 0.001
costs <- 0.5*bid_offer*rowSums(abs(rutils::diffit(weightv)))
pnls <- (pnls - costs)
# Define all-weather benchmark
weightsaw <- c(0.30, 0.55, 0.15)
all_weather <- returns %*% weightsaw
# Calculate the wealth of momentum returns
wealth <- xts::xts(cbind(all_weather, pnls), order.by=index(returns))
colnames(wealth) <- c("All-Weather", "Momentum")
cor(wealth)
# Plot dygraph of the momentum strategy returns
dygraphs::dygraph(cumsum(wealth)[dates], main="Daily Momentum Strategy vs All-Weather") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Define backtest functional for daily momentum strategy
# If trend=(-1) then it backtests a mean reverting strategy
momentum_daily <- function(returns, look_back=252, bid_offer=0.001, trend=1, ...) {
  stopifnot("package:quantmod" %in% search() || require("quantmod", quietly=TRUE))
  # Calculate rolling variance
  variance <- roll::roll_var(returns, width=look_back, min_obs=1)
  variance[1, ] <- 1
  variance[variance <= 0] <- 1
# Calculate rolling Sharpe
  past <- roll::roll_mean(returns, width=look_back, min_obs=1)
  weightv <- past/sqrt(variance)
  weightv <- weightv/sqrt(rowSums(weightv^2))
  weightv <- rutils::lagit(weightv)
  # Calculate momentum profits and losses
  pnls <- trend*rowMeans(weightv*returns)
  # Calculate transaction costs
  costs <- 0.5*bid_offer*rowSums(abs(rutils::diffit(weightv)))
  (pnls - costs)
}  # end momentum_daily
# Simulate a daily ETF momentum strategy
source("/Users/jerzy/Develop/lecture_slides/scripts/back_test.R")
pnls <- momentum_daily(returns=returns, look_back=252,
  bid_offer=bid_offer)
# Perform sapply loop over look_backs
look_backs <- seq(50, 300, by=50)
pnls <- sapply(look_backs, momentum_daily,
  returns=returns, bid_offer=bid_offer)
colnames(pnls) <- paste0("look_back=", look_backs)
pnls <- xts::xts(pnls, index(returns))
tail(pnls)
# Plot dygraph of daily ETF momentum strategies
colors <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls), main="Daily ETF Momentum Strategies") %>%
  dyOptions(colors=colors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Plot EWMA strategies with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(pnls))
quantmod::chart_Series(cumsum(pnls),
  theme=plot_theme, name="Cumulative Returns of Daily ETF Momentum Strategies")
legend("bottomleft", legend=colnames(pnls),
  inset=0.02, bg="white", cex=0.7, lwd=rep(6, NCOL(returns)),
  col=plot_theme$col$line.col, bty="n")
# Define backtest functional for daily momentum strategy
# If trend=(-1) then it backtests a mean reverting strategy
momentum_daily <- function(returns, look_back=252, hold_period=5, bid_offer=0.001, trend=1, ...) {
  stopifnot("package:quantmod" %in% search() || require("quantmod", quietly=TRUE))
  # Calculate rolling variance
  variance <- roll::roll_var(returns, width=look_back, min_obs=1)
  variance[1, ] <- 1
  variance[variance <= 0] <- 1
  # Calculate rolling Sharpe
  past <- roll::roll_mean(returns, width=look_back, min_obs=1)
  weightv <- past/sqrt(variance)
  weightv <- weightv/sqrt(rowSums(weightv^2))
  weightv <- rutils::lagit(weightv)
  # Average the weights over holding period
  weightv <- roll::roll_mean(weightv, width=hold_period, min_obs=1)
  # Calculate momentum profits and losses
  pnls <- trend*rowMeans(weightv*returns)
  # Calculate transaction costs
  costs <- 0.5*bid_offer*rowSums(abs(rutils::diffit(weightv)))
  (pnls - costs)
}  # end momentum_daily
# Perform sapply loop over holding periods
hold_periods <- seq(2, 11, by=2)
pnls <- sapply(hold_periods, momentum_daily, look_back=120,
            returns=returns, bid_offer=bid_offer)
colnames(pnls) <- paste0("holding=", hold_periods)
pnls <- xts::xts(pnls, index(returns))
# Plot dygraph of daily ETF momentum strategies
colors <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls), main="Daily ETF Momentum Strategies with Holding Period") %>%
  dyOptions(colors=colors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Plot EWMA strategies with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(pnls))
quantmod::chart_Series(cumsum(pnls),
  theme=plot_theme, name="Cumulative Returns of Daily ETF Momentum Strategies")
legend("bottomleft", legend=colnames(pnls),
  inset=0.02, bg="white", cex=0.7, lwd=rep(6, NCOL(returns)),
  col=plot_theme$col$line.col, bty="n")
# Define backtest functional for daily momentum strategy
# If trend=(-1) then it backtests a mean reverting strategy
momentum_daily <- function(returns, look_back=252, hold_period=5, bid_offer=0.001, trend=1, ...) {
  stopifnot("package:quantmod" %in% search() || require("quantmod", quietly=TRUE))
  position_s <- matrixStats::rowRanks(returns)
  position_s <- (position_s - rowMeans(position_s))
  position_s <- HighFreq::lagit(position_s, lagg=1)
  trend*rowMeans(position_s*returns)
}  # end momentum_daily
# Load ETF data
symbols <- rutils::etfenv$symbols
symbols <- symbols[!(symbols %in% c("TLT", "IEF", "MTUM", "QUAL", "VLUE", "USMV"))]
returns <- rutils::etfenv$returns[, symbols]
returns[1, is.na(returns[1, ])] <- 0
returns <- zoo::na.locf(returns, na.rm=FALSE)
# Load S&P500 data
load("/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
returns <- returns["2000/"]
returns[1, is.na(returns[1, ])] <- 0
returns <- zoo::na.locf(returns, na.rm=FALSE)
ncols <- NCOL(returns)
returns <- returns[, !(returns[ncols %/% 10, ] == 0)]
pnls <- momentum_daily(returns=returns, trend=(-1))
pnls <- xts::xts(pnls, index(returns))
colnames(pnls) <- "PnL"
dygraphs::dygraph(cumsum(pnls), main="Daily Momentum Strategy") %>%
  dyOptions(colors="blue", strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Perform sapply loop over look_backs
look_backs <- seq(50, 300, by=50)
pnls <- sapply(look_backs, momentum_daily,
  returns=returns, bid_offer=bid_offer)
colnames(pnls) <- paste0("look_back=", look_backs)
pnls <- xts::xts(pnls, index(returns))
# Perform sapply loop over holding periods
hold_periods <- seq(2, 11, by=2)
pnls <- sapply(hold_periods, momentum_daily, look_back=120, returns=returns)
colnames(pnls) <- paste0("holding=", hold_periods)
pnls <- xts::xts(pnls, index(returns))
# Plot dygraph of daily ETF momentum strategies
colors <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls), main="Daily ETF Momentum Strategies with Holding Period") %>%
  dyOptions(colors=colors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Plot EWMA strategies with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(pnls))
quantmod::chart_Series(cumsum(pnls),
  theme=plot_theme, name="Cumulative Returns of Daily ETF Momentum Strategies")
legend("bottomleft", legend=colnames(pnls),
  inset=0.02, bg="white", cex=0.7, lwd=rep(6, NCOL(returns)),
  col=plot_theme$col$line.col, bty="n")
# Load daily S&P500 percentage stock returns.
load(file="/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
# Overwrite NA values in returns100
returns100 <- returns100["2000/"]
returns100[1, is.na(returns100[1, ])] <- 0
returns100 <- zoo::na.locf(returns100, na.rm=FALSE)
# Simulate a daily S&P500 momentum strategy.
# Perform sapply loop over look_backs
look_backs <- seq(100, 300, by=20)
pnls <- sapply(look_backs, momentum_daily,
  hold_period=5, returns=returns100, bid_offer=0)
colnames(pnls) <- paste0("look_back=", look_backs)
pnls <- xts::xts(pnls, index(returns100))
# Plot dygraph of daily ETF momentum strategies
colors <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls), main="Daily S&P500 Momentum Strategies") %>%
  dyOptions(colors=colors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Plot daily S&P500 momentum strategies with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
quantmod::chart_Series(cumsum(pnls),
  theme=plot_theme, name="Daily S&P500 Momentum Strategies")
legend("bottomleft", legend=colnames(pnls),
  inset=0.02, bg="white", cex=0.7, lwd=rep(6, NCOL(returns)),
  col=plot_theme$col$line.col, bty="n")
# Perform sapply loop over look_backs
look_backs <- seq(3, 20, by=2)
pnls <- sapply(look_backs, momentum_daily,
  hold_period=5, returns=returns100, bid_offer=0, trend=(-1))
colnames(pnls) <- paste0("look_back=", look_backs)
pnls <- xts::xts(pnls, index(returns100))
# Plot dygraph of daily ETF momentum strategies
colors <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls), main="Daily S&P500 Momentum Strategies") %>%
  dyOptions(colors=colors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Plot EWMA strategies with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
quantmod::chart_Series(cumsum(pnls),
  theme=plot_theme, name="Cumulative Returns of S&P500 Mean Reverting Strategies")
legend("topleft", legend=colnames(pnls),
  inset=0.05, bg="white", cex=0.7, lwd=rep(6, NCOL(returns)),
  col=plot_theme$col$line.col, bty="n")
# Plot cumulative returns of VTI vs MTUM ETF
wealth <- log(na.omit(rutils::etfenv$prices[, c("VTI", "MTUM")]))
colnames(wealth) <- c("VTI", "MTUM")
wealth <- rutils::diffit(wealth)
dygraphs::dygraph(cumsum(wealth), main="VTI vs MTUM ETF") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(width=500)
# Select all the ETF symbols except "VXX", "SVXY" "MTUM", "QUAL", "VLUE", and "USMV"
symbols <- colnames(rutils::etfenv$returns)
symbols <- symbols[!(symbols %in% c("VXX", "SVXY", "MTUM", "QUAL", "VLUE", "USMV"))]
# Extract columns of rutils::etfenv$returns and overwrite NA values
returns <- rutils::etfenv$returns[, symbols]
nassets <- NCOL(returns)
# returns <- na.omit(returns)
returns[1, is.na(returns[1, ])] <- 0
returns <- zoo::na.locf(returns, na.rm=FALSE)
dates <- zoo::index(returns)
# Returns in excess of risk-free rate
riskf <- 0.03/252
excess <- (returns - riskf)
# Maximum Sharpe weights in-sample interval
rets_is <- returns["/2014"]
inverse <- MASS::ginv(cov(rets_is))
weightv <- inverse %*% colMeans(excess["/2014"])
weightv <- drop(weightv/sqrt(sum(weightv^2)))
names(weightv) <- colnames(returns)
# Plot portfolio weights
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
barplot(sort(weightv), main="Maximum Sharpe Weights", cex.names=0.7)
# Calculate in-sample portfolio returns
portf_is <- xts::xts(rets_is %*% weightv, index(rets_is))
indeks <- xts::xts(rowSums(rets_is)/sqrt(nassets), index(rets_is))
portf_is <- portf_is*sd(indeks)/sd(portf_is)
# Plot cumulative portfolio returns
pnls <- cumsum(cbind(portf_is, indeks))
colnames(pnls) <- c("Optimal Portfolio", "Equal Weight Portfolio")
dygraphs::dygraph(pnls, main="In-sample Optimal Portfolio Returns") %>%
  dyOptions(colors=c("red", "blue"), strokeWidth=2) %>%
  dyLegend(width=500)
# Calculate out-of-sample portfolio returns
rets_os <- returns["2015/"]
portf_os <- xts::xts(rets_os %*% weightv, index(rets_os))
indeks <- xts::xts(rowSums(rets_os)/sqrt(nassets), index(rets_os))
portf_os <- portf_os*sd(indeks)/sd(portf_os)
pnls <- cbind(portf_os, indeks, (portf_os + indeks)/2)
colnames(pnls) <- c("Optimal", "Equal Weight", "Combined")
sapply(pnls, function(x) mean(x)/sd(x))
# Plot cumulative portfolio returns
dygraphs::dygraph(cumsum(pnls), main="Out-of-sample Optimal Portfolio Returns") %>%
  dyOptions(colors=c("red", "blue", "green"), strokeWidth=2) %>%
  dyLegend(width=500)
# Maximum Sharpe weights in-sample interval
inverse <- MASS::ginv(cov(rets_is))
weightv <- inverse %*% colMeans(excess["/2014"])
weightv <- drop(weightv/sqrt(sum(weightv^2)))
names(weightv) <- colnames(returns)
# Calculate in-sample portfolio returns
portf_is <- xts::xts(rets_is %*% weightv, index(rets_is))
# Calculate out-of-sample portfolio returns
rets_os <- returns["2015/"]
portf_os <- xts::xts(rets_os %*% weightv, index(rets_os))
# Plot cumulative portfolio returns
pnls <- rbind(portf_is, portf_os)
indeks <- xts::xts(rowSums(returns)/sqrt(nassets), dates)
pnls <- pnls*sd(indeks)/sd(pnls)
pnls <- cumsum(cbind(pnls, indeks))
colnames(pnls) <- c("Optimal Portfolio", "Equal Weight Portfolio")
dygraphs::dygraph(pnls, main="Out-of-sample Optimal Portfolio Returns for ETFs") %>%
  dyOptions(colors=c("red", "blue"), strokeWidth=2) %>%
  dyEvent(index(last(rets_is[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=500)
# Create rectangular matrix with collinear columns
ran_dom <- matrix(rnorm(10*8), nc=10)
# Calculate covariance matrix
covmat <- cov(ran_dom)
# Calculate inverse of covmat - error
inverse <- solve(covmat)
# Perform eigen decomposition
eigend <- eigen(covmat)
eigen_vec <- eigend$vectors
eigen_val <- eigend$values
# Set tolerance for determining zero singular values
precision <- sqrt(.Machine$double.eps)
# Calculate regularized inverse matrix
not_zero <- (eigen_val > (precision * eigen_val[1]))
inv_reg <- eigen_vec[, not_zero] %*%
  (t(eigen_vec[, not_zero]) / eigen_val[not_zero])
# Verify inverse property of inv_reg
all.equal(covmat, covmat %*% inv_reg %*% covmat)
# Calculate regularized inverse of covmat
inverse <- MASS::ginv(covmat)
# Verify inverse property of matrixv
all.equal(inverse, inv_reg)
# Calculate in-sample covariance matrix
covmat <- cov(rets_is)
eigend <- eigen(covmat)
eigen_vec <- eigend$vectors
eigen_val <- eigend$values
# Calculate shrinkage inverse of covariance matrix
eigen_max <- 3
inverse <- eigen_vec[, 1:eigen_max] %*%
  (t(eigen_vec[, 1:eigen_max]) / eigend$values[1:eigen_max])
# Verify inverse property of inverse
all.equal(covmat, covmat %*% inverse %*% covmat)
# Calculate portfolio weights
weightv <- inverse %*% colMeans(excess["/2014"])
weightv <- drop(weightv/sqrt(sum(weightv^2)))
names(weightv) <- colnames(returns)
# Calculate portfolio returns
portf_is <- xts::xts(rets_is %*% weightv, index(rets_is))
portf_os <- xts::xts(rets_os %*% weightv, index(rets_os))
# Plot cumulative portfolio returns
pnls <- rbind(portf_is, portf_os)
pnls <- pnls*sd(indeks)/sd(pnls)
pnls <- cumsum(cbind(pnls, indeks))
colnames(pnls) <- c("Optimal Portfolio", "Equal Weight Portfolio")
dygraphs::dygraph(pnls, main="Regularized Out-of-sample Optimal Portfolio Returns for ETFs") %>%
  dyOptions(colors=c("red", "blue"), strokeWidth=2) %>%
  dyEvent(index(last(rets_is[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=500)
# Shrink the in-sample returns to their mean
alpha <- 0.7
excess_mean <- rowMeans(excess["/2014"])
excess_is <- (1 - alpha)*excess["/2014"] + alpha*excess_mean
# Calculate portfolio weights
weightv <- inverse %*% colMeans(excess_is)
weightv <- drop(weightv/sqrt(sum(weightv^2)))
# Calculate portfolio returns
portf_is <- xts::xts(rets_is %*% weightv, index(rets_is))
portf_os <- xts::xts(rets_os %*% weightv, index(rets_os))
# Plot cumulative portfolio returns
pnls <- rbind(portf_is, portf_os)
pnls <- pnls*sd(indeks)/sd(pnls)
pnls <- cumsum(cbind(pnls, indeks))
colnames(pnls) <- c("Optimal Portfolio", "Equal Weight Portfolio")
dygraphs::dygraph(pnls, main="Out-of-sample Returns for ETFs With Regularization and Shrinkage") %>%
  dyOptions(colors=c("red", "blue"), strokeWidth=2) %>%
  dyEvent(index(last(rets_is[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=500)
# Define monthly dates
format(dates[1], "%m-%Y")
format(dates[NROW(dates)], "%m-%Y")
months <- seq.Date(from=as.Date("2001-01-01"), to=as.Date("2021-04-01"), by="month")
# Perform loop over monthly dates
look_back <- 6
eigen_max <- 3
pnls <- lapply((look_back+1):(NROW(months)-1), function(i) {
  # Define in-sample and out-of-sample returns
  in_sample <- (dates > months[i-look_back]) & (dates < months[i])
  out_sample <- (dates > months[i]) & (dates < months[i+1])
  rets_is <- returns[in_sample]
  rets_os <- returns[out_sample]
  # Calculate shrinkage inverse of covariance matrix
  # inverse <- MASS::ginv(cov(rets_is))  # if VXX and SVXY are included then no shrinkage is better
  eigend <- eigen(cov(rets_is))
  eigen_vec <- eigend$vectors
  eigen_val <- eigend$values
  inverse <- eigen_vec[, 1:eigen_max] %*%
    (t(eigen_vec[, 1:eigen_max]) / eigend$values[1:eigen_max])
  weightv <- inverse %*% colMeans(rets_is - riskf)
  weightv <- drop(weightv/sqrt(sum(weightv^2)))
  # Calculate portfolio pnls out-of-sample
  xts::xts(rets_os %*% weightv, index(rets_os))
})  # end lapply
pnls <- do.call(rbind, pnls)
# Plot dygraph of rolling monthly IR strategy
vtis <- rutils::diffit(cumsum(indeks)[zoo::index(pnls),])
wealth <- cbind(vtis, pnls)
colnames(wealth) <- c("Index", "Strategy")
colnames <- colnames(wealth)
dygraphs::dygraph(cumsum(wealth), main="Monthly ETF Rolling Portfolio Strategy With Shrinkage") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=colnames[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Define weekly dates
weeks <- seq.Date(from=as.Date("2001-01-01"), to=as.Date("2021-04-01"), by="weeks")
# Perform loop over monthly dates
look_back <- 21
eigen_max <- 3
pnls <- lapply((look_back+1):(NROW(weeks)-1), function(i) {
  # Define in-sample and out-of-sample returns
  in_sample <- (dates > weeks[i-look_back]) & (dates < weeks[i])
  out_sample <- (dates > weeks[i]) & (dates < weeks[i+1])
  rets_is <- returns[in_sample]
  rets_os <- returns[out_sample]
  # Calculate shrinkage inverse of covariance matrix
  # inverse <- MASS::ginv(cov(rets_is))  # if VXX and SVXY are included then no shrinkage is better
  inverse <- HighFreq::calc_inv(cov(rets_is), eigen_max=eigen_max)
  weightv <- inverse %*% colMeans(rets_is - riskf)
  weightv <- drop(weightv/sqrt(sum(weightv^2)))
  # Calculate portfolio pnls out-of-sample
  xts::xts(rets_os %*% weightv, index(rets_os))
})  # end lapply
pnls <- do.call(rbind, pnls)
# Plot dygraph of rolling monthly IR strategy
vtis <- rutils::diffit(cumsum(indeks)[zoo::index(pnls),])
wealth <- cbind(vtis, pnls)
colnames(wealth) <- c("Index", "Strategy")
colnames <- colnames(wealth)
dygraphs::dygraph(cumsum(wealth), main="Weekly ETF Rolling Portfolio Strategy With Shrinkage") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=colnames[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Define backtest functional for daily momentum strategy
# If trend=(-1) then it backtests a mean reverting strategy
roll_portf <- function(returns, look_back=252, eigen_max=3, hold_period=5, bid_offer=0.0, trend=1, ...) {
  cat("look_back=", look_back, "\n")
  pnls <- lapply((look_back+1):(NROW(weeks)-1), function(i) {
    # Define in-sample and out-of-sample returns
    in_sample <- (dates > weeks[i-look_back]) & (dates < weeks[i])
    out_sample <- (dates > weeks[i]) & (dates < weeks[i+1])
    rets_is <- returns[in_sample]
    rets_os <- returns[out_sample]
    # Calculate shrinkage inverse of covariance matrix
    # inverse <- MASS::ginv(cov(rets_is))  # if VXX and SVXY are included then no shrinkage is better
    # inverse <- HighFreq::calc_inv(cov(rets_is), eigen_max=eigen_max)
    # weightv <- inverse %*% colMeans(rets_is - riskf)
    weightv <- HighFreq::calc_weights(rets_is, method="max_sharpe", eigen_max=eigen_max, scale=FALSE)
    weightv <- drop(weightv/sqrt(sum(weightv^2)))
    # Calculate portfolio pnls out-of-sample
    xts::xts(rets_os %*% weightv, index(rets_os))
  })  # end lapply
  do.call(rbind, pnls)
}  # end roll_portf
# Simulate a daily ETF momentum strategy
pnls <- roll_portf(returns=returns, look_back=41, eigen_max=eigen_max)
# Perform sapply loop over look_backs
look_backs <- seq(5, 50, by=2)
pnls <- lapply(look_backs, roll_portf,
  returns=returns, eigen_max=eigen_max)
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("look_back=", look_backs)
tail(pnls)
pnls[1, is.na(pnls[1, ])] <- 0
pnls <- zoo::na.locf(pnls)
foo <- sapply(pnls, sum)
look_backs[which.max(foo)]
# Plot dygraph of daily ETF momentum strategies
colors <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls), main="Daily ETF Momentum Strategies") %>%
  dyOptions(colors=colors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Plot EWMA strategies with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(pnls))
quantmod::chart_Series(cumsum(pnls),
  theme=plot_theme, name="Cumulative Returns of Daily ETF Momentum Strategies")
legend("bottomleft", legend=colnames(pnls),
  inset=0.02, bg="white", cex=0.7, lwd=rep(6, NCOL(returns)),
  col=plot_theme$col$line.col, bty="n")
load("/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
# Overwrite NA values in returns
returns <- returns["2000/"]
nassets <- NCOL(returns)
returns[1, is.na(returns[1, ])] <- 0
returns <- zoo::na.locf(returns, na.rm=FALSE)
dates <- zoo::index(returns)
riskf <- 0.03/252
excess <- (returns - riskf)
rets_is <- returns["/2010"]
rets_os <- returns["2011/"]
# Maximum Sharpe weights in-sample interval
covmat <- cov(rets_is)
inverse <- MASS::ginv(covmat)
weightv <- inverse %*% colMeans(excess["/2010"])
weightv <- drop(weightv/sqrt(sum(weightv^2)))
names(weightv) <- colnames(returns)
# Calculate portfolio returns
portf_is <- xts::xts(rets_is %*% weightv, index(rets_is))
portf_os <- xts::xts(rets_os %*% weightv, index(rets_os))
indeks <- xts::xts(rowSums(returns)/sqrt(nassets), dates)
# Plot cumulative portfolio returns
pnls <- rbind(portf_is, portf_os)
pnls <- pnls*sd(indeks)/sd(pnls)
pnls <- cumsum(cbind(pnls, indeks))
colnames(pnls) <- c("Optimal Portfolio", "Equal Weight Portfolio")
dygraphs::dygraph(pnls, main="Out-of-sample Optimal Portfolio Returns for Stocks") %>%
  dyOptions(colors=c("red", "blue"), strokeWidth=2) %>%
  dyEvent(index(last(rets_is[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=500)
# Calculate portfolio weights
weightv <- inverse %*% colMeans(excess["/2010"])
weightv <- drop(weightv/sqrt(sum(weightv^2)))
names(weightv) <- colnames(returns)
# Calculate portfolio returns
portf_is <- xts::xts(rets_is %*% weightv, index(rets_is))
portf_os <- xts::xts(rets_os %*% weightv, index(rets_os))
indeks <- xts::xts(rowSums(returns)/sqrt(nassets), dates)
# Plot cumulative portfolio returns
pnls <- rbind(portf_is, portf_os)
pnls <- pnls*sd(indeks)/sd(pnls)
pnls <- cumsum(cbind(pnls, indeks))
colnames(pnls) <- c("Optimal Portfolio", "Equal Weight Portfolio")
dygraphs::dygraph(pnls, main="Regularized Out-of-sample Optimal Portfolio Returns for Stocks") %>%
  dyOptions(colors=c("red", "blue"), strokeWidth=2) %>%
  dyEvent(index(last(rets_is[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=500)
# Shrink the in-sample returns to their mean
alpha <- 0.7
excess_mean <- rowMeans(excess["/2010"])
excess_is <- (1 - alpha)*excess["/2010"] + alpha*excess_mean
# Calculate portfolio weights
weightv <- inverse %*% colMeans(excess_is)
weightv <- drop(weightv/sqrt(sum(weightv^2)))
# Calculate portfolio returns
portf_is <- xts::xts(rets_is %*% weightv, index(rets_is))
portf_os <- xts::xts(rets_os %*% weightv, index(rets_os))
# Plot cumulative portfolio returns
pnls <- rbind(portf_is, portf_os)
pnls <- pnls*sd(indeks)/sd(pnls)
pnls <- cumsum(cbind(pnls, indeks))
colnames(pnls) <- c("Optimal Portfolio", "Equal Weight Portfolio")
dygraphs::dygraph(pnls, main="Out-of-sample Returns for Stocks With Regularization and Shrinkage") %>%
  dyOptions(colors=c("red", "blue"), strokeWidth=2) %>%
  dyEvent(index(last(rets_is[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=500)
library(RcppArmadillo)
# Source Rcpp functions from file
Rcpp::sourceCpp("/Users/jerzy/Develop/lecture_slides/scripts/back_test.cpp")
# Create random matrix of returns
matrixv <- matrix(rnorm(300), nc=5)
# Regularized inverse of covariance matrix
eigen_max <- 4
eigend <- eigen(cov(matrixv))
covinv <- eigend$vectors[, 1:eigen_max] %*%
  (t(eigend$vectors[, 1:eigen_max]) / eigend$values[1:eigen_max])
# Regularized inverse using RcppArmadillo
covinv_arma <- calc_inv(matrixv, eigen_max)
all.equal(covinv, covinv_arma)
# Microbenchmark RcppArmadillo code
library(microbenchmark)
summary(microbenchmark(
  pure_r={eigend <- eigen(cov(matrixv))
    eigend$vectors[, 1:eigen_max] %*%
(t(eigend$vectors[, 1:eigen_max]) / eigend$values[1:eigen_max])
  },
  r_cpp=calc_inv(matrixv, eigen_max),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
# Overwrite NA values in returns100
returns100[1, is.na(returns100[1, ])] <- 0
returns100 <- zoo::na.locf(returns100, na.rm=FALSE)
excess <- (returns100 - riskf)
ncols <- NCOL(returns100) ; dates <- index(returns100)
nassets <- NCOL(returns100)
# Define monthly end points
endp <- rutils::calc_endpoints(returns100, interval="months")
endp <- endp[endp > (ncols+1)]
nrows <- NROW(endp) ; look_back <- 12
startp <- c(rep_len(0, look_back-1), endp[1:(nrows-look_back+1)])
# Perform loop over end points
pnls <- lapply(2:nrows, function(i) {
    # Subset the excess returns
    excess <- excess[startp[i-1]:endp[i-1], ]
    inverse <- MASS::ginv(cov(excess))
    # Calculate the maximum Sharpe ratio portfolio weights
    weightv <- inverse %*% colMeans(excess)
    weightv <- drop(weightv/sqrt(sum(weightv^2)))
    # Calculate the out-of-sample portfolio returns
    returns <- returns100[(endp[i-1]+1):endp[i], ]
    xts::xts(returns %*% weightv, index(returns))
})  # end lapply
pnls <- rutils::do_call(rbind, pnls)
# Calculate returns of equal weight portfolio
indeks <- xts::xts(rowMeans(returns100), dates)
# Plot cumulative strategy returns
wealth <- na.omit(cbind(pnls, indeks*sd(rets_portf)/sd(indeks)))
colnames(wealth) <- c("Rolling Strategy", "Equal Weight")
dygraphs::dygraph(cumsum(wealth), main="Rolling Portfolio Optimization Strategy for S&P500 Stocks") %>%
  dyOptions(colors=c("red", "blue"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Shift end points to C++ convention
endp <- (endp - 1)
endp[endp < 0] <- 0
startp <- (startp - 1)
startp[startp < 0] <- 0
# Specify shrinkage intensity
alpha <- 0.7
eigen_max <- 21
# Perform backtest in Rcpp
pnls <- HighFreq::back_test(excess=excess, returns=returns100,
  startp=startp, endp=endp, alpha=alpha, eigen_max=eigen_max, method="max_sharpe")
# Plot cumulative strategy returns
wealth <- cbind(pnls, indeks, (pnls+indeks)/2)
wealth <- cumsum(na.omit(wealth))
colnames <- c("Strategy", "Index", "Combined")
colnames(wealth) <- colnames
dygraphs::dygraph(wealth[endp], main="Rolling S&P500 Portfolio Optimization Strategy With Shrinkage") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", col="red", strokeWidth=1) %>%
  dySeries(name=colnames[2], axis="y2", col="blue", strokeWidth=1) %>%
  dySeries(name=colnames[3], axis="y2", col="green", strokeWidth=2)
# Perform backtest over alphas
alpha_s <- seq(from=0.01, to=0.91, by=0.1)
pnls <- lapply(alpha_s, function(alpha) {
  HighFreq::back_test(excess=excess, returns=returns100,
  startp=startp, endp=endp, alpha=alpha, eigen_max=eigen_max, method="max_sharpe")
})  # end lapply
profilev <- sapply(pnls, sum)
plot(x=alpha_s, y=profilev, t="l", main="Strategy PnL as Function of Shrinkage Intensity Alpha",
  xlab="Shrinkage Intensity Alpha", ylab="pnl")
alpha <- alpha_s[which.max(profilev)]
pnls <- pnls[[which.max(profilev)]]
# Perform backtest over eigen_maxs
eigen_maxs <- seq(from=3, to=40, by=2)
pnls <- lapply(eigen_maxs, function(eigen_max) {
  HighFreq::back_test(excess=excess, returns=returns100,
    startp=startp, endp=endp, alpha=alpha, eigen_max=eigen_max, method="max_sharpe")
})  # end lapply
profilev <- sapply(pnls, sum)
plot(x=eigen_maxs, y=profilev, t="l", main="Strategy PnL as Function of eigen_max",
  xlab="eigen_max", ylab="pnl")
eigen_max <- eigen_maxs[which.max(profilev)]
pnls <- pnls[[which.max(profilev)]]
# Plot cumulative strategy returns
wealth <- cbind(pnls, indeks, (pnls+indeks)/2)
wealth <- cumsum(na.omit(wealth))
colnames <- c("Strategy", "Index", "Combined")
colnames(wealth) <- colnames
dygraphs::dygraph(wealth[endp], main="Optimal Rolling S&P500 Portfolio Strategy") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", col="red", strokeWidth=1) %>%
  dySeries(name=colnames[2], axis="y2", col="blue", strokeWidth=1) %>%
  dySeries(name=colnames[3], axis="y2", col="green", strokeWidth=2)
# Perform backtest over look-backs
look_backs <- seq(from=3, to=24, by=1)
pnls <- lapply(look_backs, function(look_back) {
  startp <- c(rep_len(0, look_back-1), endp[1:(nrows-look_back+1)])
  startp <- (startp - 1)
  startp[startp < 0] <- 0
  HighFreq::back_test(excess=excess, returns=returns100,
    startp=startp, endp=endp, alpha=alpha, eigen_max=eigen_max, method="max_sharpe")
})  # end lapply
profilev <- sapply(pnls, sum)
plot(x=look_backs, y=profilev, t="l", main="Strategy PnL as Function of Look-back Interval",
  xlab="Look-back Interval", ylab="pnl")
look_back <- look_backs[which.max(profilev)]
pnls <- pnls[[which.max(profilev)]]
# Plot cumulative strategy returns
wealth <- cbind(pnls, indeks, (pnls+indeks)/2)
wealth <- cumsum(na.omit(wealth))
colnames <- c("Strategy", "Index", "Combined")
colnames(wealth) <- colnames
dygraphs::dygraph(wealth[endp], main="Optimal Rolling S&P500 Portfolio Strategy") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", col="red", strokeWidth=1) %>%
  dySeries(name=colnames[2], axis="y2", col="blue", strokeWidth=1) %>%
  dySeries(name=colnames[3], axis="y2", col="green", strokeWidth=2)
