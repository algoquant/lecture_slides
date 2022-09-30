# Extract VTI log OHLC prices
ohlc <- log(rutils::etfenv$VTI)
nrows <- NROW(ohlc)
closep <- quantmod::Cl(ohlc)
retsp <- rutils::diffit(closep)
# Calculate the centered volatility
look_back <- 7
half_back <- look_back %/% 2
stdev <- roll::roll_sd(retsp, width=look_back, min_obs=1)
stdev <- rutils::lagit(stdev, lagg=(-half_back))
# Calculate the z-scores of prices
pricez <- (2*closep -
  rutils::lagit(closep, half_back, pad_zeros=FALSE) -
  rutils::lagit(closep, -half_back, pad_zeros=FALSE))
pricez <- ifelse(stdev > 0, pricez/stdev, 0)

# Plot dygraph of z-scores of VTI prices
pricets <- cbind(closep, pricez)
colnames(pricets) <- c("VTI", "Z-scores")
colnamev <- colnames(pricets)
dygraphs::dygraph(pricets["2009"], main="VTI Price Z-Scores") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=2, col="red")

# Calculate thresholds for labeling tops and bottoms
confl <- c(0.2, 0.8)
threshv <- quantile(pricez, confl)
# Calculate the vectors of tops and bottoms
tops <- zoo::coredata(pricez > threshv[2])
colnames(tops) <- "tops"
bottoms <- zoo::coredata(pricez < threshv[1])
colnames(bottoms) <- "bottoms"
# Simulate in-sample VTI strategy
posit <- rep(NA_integer_, nrows)
posit[1] <- 0
posit[tops] <- (-1)
posit[bottoms] <- 1
posit <- zoo::na.locf(posit)
posit <- rutils::lagit(posit)
pnls <- retsp*posit

# Plot dygraph of in-sample VTI strategy
wealthv <- cbind(retsp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
endp <- rutils::calc_endpoints(wealthv, interval="months")
dygraphs::dygraph(cumsum(wealthv)[endp],
  main="VTI Strategy Using In-sample Labels") %>%
  dyAxis("y", label="VTI", independentTicks=TRUE) %>%
  dyAxis("y2", label="Strategy", independentTicks=TRUE) %>%
  dySeries(name="VTI", axis="y", label="VTI", strokeWidth=2, col="blue") %>%
  dySeries(name="Strategy", axis="y2", label="Strategy", strokeWidth=2, col="red")

# Calculate volatility z-scores
volat <- HighFreq::roll_var_ohlc(ohlc=ohlc, look_back=look_back, scale=FALSE)
meanv <- roll::roll_mean(volat, width=look_back, min_obs=1)
stdev <- roll::roll_sd(rutils::diffit(volat), width=look_back, min_obs=1)
stdev[1] <- 0
volatz <- ifelse(stdev > 0, (volat - meanv)/stdev, 0)
colnames(volatz) <- "volat"
# Calculate volume z-scores
volumes <- quantmod::Vo(ohlc)
meanv <- roll::roll_mean(volumes, width=look_back, min_obs=1)
stdev <- roll::roll_sd(rutils::diffit(volumes), width=look_back, min_obs=1)
stdev[1] <- 0
volumez <- ifelse(stdev > 0, (volumes - meanv)/stdev, 0)
colnames(volumez) <- "volume"

# Define design matrix for tops including intercept column
predictor <- cbind(volatz, volumez)
predictor[1, ] <- 0
predictor <- rutils::lagit(predictor)
# Fit in-sample logistic regression for tops
logmod <- glm(tops ~ predictor, family=binomial(logit))
summary(logmod)
coeff <- logmod$coefficients
forecastv <- drop(cbind(rep(1, nrows), predictor) %*% coeff)
ordern <- order(forecastv)
# Calculate in-sample forecasts from logistic regression model
forecastv <- 1/(1+exp(-forecastv))
all.equal(logmod$fitted.values, forecastv, check.attributes=FALSE)
hist(forecastv)

x11(width=6, height=5)
plot(x=forecastv[ordern], y=tops[ordern],
     main="Logistic Regression of Stock Tops",
     col="orange", xlab="predictor", ylab="top")
lines(x=forecastv[ordern], y=logmod$fitted.values[ordern], col="blue", lwd=3)
legend(x="topleft", inset=0.1, bty="n", lwd=6,
 legend=c("tops", "logit fitted values"),
 col=c("orange", "blue"), lty=c(NA, 1), pch=c(1, NA))

# Define discrimination threshold value
threshold <- quantile(forecastv, confl[2])
# Calculate confusion matrix in-sample
confmat <- table(actual=!tops, forecast=(forecastv < threshold))
confmat
# Calculate FALSE positive (type I error)
sum(tops & (forecastv < threshold))
# Calculate FALSE negative (type II error)
sum(!tops & (forecastv > threshold))

# Calculate FALSE positive and FALSE negative rates
confmat <- confmat / rowSums(confmat)
c(typeI=confmat[2, 1], typeII=confmat[1, 2])
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
confun <- function(actual, forecastv, threshold) {
    conf <- table(actual, (forecastv < threshold))
    conf <- conf / rowSums(conf)
    c(typeI=conf[2, 1], typeII=conf[1, 2])
  }  # end confun
confun(!tops, forecastv, threshold=threshold)
# Define vector of discrimination thresholds
threshv <- quantile(forecastv, seq(0.01, 0.99, by=0.01))
# Calculate error rates
error_rates <- sapply(threshv, confun,
  actual=!tops, forecastv=forecastv)  # end sapply
error_rates <- t(error_rates)
rownames(error_rates) <- threshv
# Calculate the informedness
informv <- 2 - rowSums(error_rates)
plot(threshv, informv, t="l", main="Informedness")
# Find the threshold corresponding to highest informedness
threshm <- threshv[which.max(informv)]
forecastops <- (forecastv > threshm)

# Calculate area under ROC curve (AUC)
error_rates <- rbind(c(1, 0), error_rates)
error_rates <- rbind(error_rates, c(0, 1))
truepos <- (1 - error_rates[, "typeII"])
truepos <- (truepos + rutils::lagit(truepos))/2
falsepos <- rutils::diffit(error_rates[, "typeI"])
abs(sum(truepos*falsepos))
# Plot ROC Curve for stock tops
plot(x=error_rates[, "typeI"], y=1-error_rates[, "typeII"],
     xlab="FALSE positive rate", ylab="TRUE positive rate",
     main="ROC Curve for Stock Tops", type="l", lwd=3, col="blue")
abline(a=0.0, b=1.0, lwd=3, col="orange")

# Fit in-sample logistic regression for bottoms
logmod <- glm(bottoms ~ predictor, family=binomial(logit))
summary(logmod)
# Calculate in-sample forecast from logistic regression model
coeff <- logmod$coefficients
forecastv <- drop(cbind(rep(1, nrows), predictor) %*% coeff)
forecastv <- 1/(1+exp(-forecastv))
# Calculate error rates
error_rates <- sapply(threshv, confun,
  actual=!bottoms, forecastv=forecastv)  # end sapply
error_rates <- t(error_rates)
rownames(error_rates) <- threshv
# Calculate the informedness
informv <- 2 - rowSums(error_rates)
plot(threshv, informv, t="l", main="Informedness")
# Find the threshold corresponding to highest informedness
threshm <- threshv[which.max(informv)]
forecastbot <- (forecastv > threshm)

# Calculate area under ROC curve (AUC)
error_rates <- rbind(c(1, 0), error_rates)
error_rates <- rbind(error_rates, c(0, 1))
truepos <- (1 - error_rates[, "typeII"])
truepos <- (truepos + rutils::lagit(truepos))/2
falsepos <- rutils::diffit(error_rates[, "typeI"])
abs(sum(truepos*falsepos))
# Plot ROC Curve for stock tops
plot(x=error_rates[, "typeI"], y=1-error_rates[, "typeII"],
     xlab="FALSE positive rate", ylab="TRUE positive rate",
     main="ROC Curve for Stock Bottoms", type="l", lwd=3, col="blue")
abline(a=0.0, b=1.0, lwd=3, col="orange")

# Simulate in-sample VTI strategy
posit <- rep(NA_integer_, NROW(retsp))
posit[1] <- 0
posit[forecastops] <- (-1)
posit[forecastbot] <- 1
posit <- zoo::na.locf(posit)
posit <- rutils::lagit(posit)
pnls <- retsp*posit

# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retsp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
sqrt(252)*sapply(wealthv,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of in-sample VTI strategy
endp <- rutils::calc_endpoints(wealthv, interval="months")
dygraphs::dygraph(cumsum(wealthv)[endp],
  main="Logistic Strategy Using Top and Bottom Labels") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)

insample <- 1:(nrows %/% 2)
outsample <- (nrows %/% 2 + 1):nrows
# Fit in-sample logistic regression for tops
logmod <- glm(tops[insample] ~ predictor[insample, ], family=binomial(logit))
fittedv <- logmod$fitted.values
coefftop <- logmod$coefficients
# Calculate error rates and best threshold value
error_rates <- sapply(threshv, confun,
  actual=!tops[insample], forecastv=fittedv)  # end sapply
error_rates <- t(error_rates)
informv <- 2 - rowSums(error_rates)
threshtop <- threshv[which.max(informv)]
# Fit in-sample logistic regression for bottoms
logmod <- glm(bottoms[insample] ~ predictor[insample, ], family=binomial(logit))
fittedv <- logmod$fitted.values
coeffbot <- logmod$coefficients
# Calculate error rates and best threshold value
error_rates <- sapply(threshv, confun,
  actual=!bottoms[insample], forecastv=fittedv)  # end sapply
error_rates <- t(error_rates)
informv <- 2 - rowSums(error_rates)
threshbot <- threshv[which.max(informv)]
# Calculate out-of-sample forecasts from logistic regression model
predictout <- cbind(rep(1, NROW(outsample)), predictor[outsample, ])
forecastv <- drop(predictout %*% coefftop)
forecastv <- 1/(1+exp(-forecastv))
forecastops <- (forecastv > threshtop)
forecastv <- drop(predictout %*% coeffbot)
forecastv <- 1/(1+exp(-forecastv))
forecastbot <- (forecastv > threshbot)

# Simulate out-of-sample VTI strategy
posit <- rep(NA_integer_, NROW(outsample))
posit[1] <- 0
posit[forecastops] <- (-1)
posit[forecastbot] <- 1
posit <- zoo::na.locf(posit)
posit <- rutils::lagit(posit)
pnls <- retsp[outsample, ]*posit
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retsp[outsample, ], pnls)
colnames(wealthv) <- c("VTI", "Strategy")
sqrt(252)*sapply(wealthv,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of in-sample VTI strategy
endp <- rutils::calc_endpoints(wealthv, interval="months")
dygraphs::dygraph(cumsum(wealthv)[endp],
  main="Logistic Strategy Out-of-Sample") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)

# Fit logistic regression over training data
set.seed(1121)  # Reset random number generator
nrows <- NROW(Default)
samplev <- sample.int(n=nrows, size=nrows/2)
traindata <- Default[samplev, ]
logmod <- glm(formulav, data=traindata, family=binomial(logit))
# Forecast over test data out-of-sample
testdata <- Default[-samplev, ]
forecastv <- predict(logmod, newdata=testdata, type="response")
# Calculate confusion matrix out-of-sample
table(actual=!testdata$default,
forecast=(forecastv < threshold))

# Define response and design matrix
retsf <- rutils::diffit(closep, lagg=5)
retsf <- drop(coredata(retsf))
# Fit in-sample logistic regression for positive returns
retspos <- (retsf > 0)
logmod <- glm(retspos ~ predictor - 1, family=binomial(logit))
summary(logmod)
coeff <- logmod$coefficients
forecastv <- drop(predictor %*% coeff)
forecastv <- 1/(1+exp(-forecastv))
# Calculate error rates
threshv <- quantile(forecastv, seq(0.01, 0.99, by=0.01))
error_rates <- sapply(threshv, confun,
  actual=!retspos, forecastv=forecastv)  # end sapply
error_rates <- t(error_rates)
# Calculate the informedness
informv <- 2 - rowSums(error_rates)
plot(threshv, informv, t="l", main="Informedness")
# Find the threshold corresponding to highest informedness
threshm <- threshv[which.max(informv)]
forecastpos <- (forecastv > threshm)
# Fit in-sample logistic regression for negative returns
retsneg <- (retsf < 0)
logmod <- glm(retsneg ~ predictor - 1, family=binomial(logit))
summary(logmod)
coeff <- logmod$coefficients
forecastv <- drop(predictor %*% coeff)
forecastv <- 1/(1+exp(-forecastv))
# Calculate error rates
error_rates <- sapply(threshv, confun,
  actual=!retsneg, forecastv=forecastv)  # end sapply
error_rates <- t(error_rates)
# Calculate the informedness
informv <- 2 - rowSums(error_rates)
plot(threshv, informv, t="l", main="Informedness")
# Find the threshold corresponding to highest informedness
threshm <- threshv[which.max(informv)]
forecastneg <- (forecastv > threshm)

# Simulate in-sample VTI strategy
posit <- ifelse(forecastpos, 1, 0)
posit <- ifelse(forecastneg, -1, posit)
pnls <- retsp*posit
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retsp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
sqrt(252)*sapply(wealthv,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))

# Plot dygraph of in-sample VTI strategy
endp <- rutils::calc_endpoints(wealthv, interval="months")
dygraphs::dygraph(cumsum(wealthv)[endp],
  main="Logistic Forecasting Returns") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)

insample <- 1:(nrows %/% 2)
outsample <- (nrows %/% 2 + 1):nrows
# Fit in-sample logistic regression for tops
logmod <- glm(tops[insample] ~ predictor[insample, ], family=binomial(logit))
fittedv <- logmod$fitted.values
coefftop <- logmod$coefficients
# Calculate error rates and best threshold value
error_rates <- sapply(threshv, confun,
  actual=!tops[insample], forecastv=fittedv)  # end sapply
error_rates <- t(error_rates)
informv <- 2 - rowSums(error_rates)
threshtop <- threshv[which.max(informv)]
# Fit in-sample logistic regression for bottoms
logmod <- glm(bottoms[insample] ~ predictor[insample, ], family=binomial(logit))
fittedv <- logmod$fitted.values
coeffbot <- logmod$coefficients
# Calculate error rates and best threshold value
error_rates <- sapply(threshv, confun,
  actual=!bottoms[insample], forecastv=fittedv)  # end sapply
error_rates <- t(error_rates)
informv <- 2 - rowSums(error_rates)
threshbot <- threshv[which.max(informv)]
# Calculate out-of-sample forecasts from logistic regression model
predictout <- cbind(rep(1, NROW(outsample)), predictor[outsample, ])
forecastv <- drop(predictout %*% coefftop)
forecastv <- 1/(1+exp(-forecastv))
forecastops <- (forecastv > threshtop)
forecastv <- drop(predictout %*% coeffbot)
forecastv <- 1/(1+exp(-forecastv))
forecastbot <- (forecastv > threshbot)

# Simulate out-of-sample VTI strategy
posit <- rep(NA_integer_, NROW(outsample))
posit[1] <- 0
posit[forecastops] <- (-1)
posit[forecastbot] <- 1
posit <- zoo::na.locf(posit)
posit <- rutils::lagit(posit)
pnls <- retsp[outsample, ]*posit
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retsp[outsample, ], pnls)
colnames(wealthv) <- c("VTI", "Strategy")
sqrt(252)*sapply(wealthv,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of in-sample VTI strategy
endp <- rutils::calc_endpoints(wealthv, interval="months")
dygraphs::dygraph(cumsum(wealthv)[endp],
  main="Logistic Strategy Out-of-Sample") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)

# Calculate VTI percentage returns
closep <- log(na.omit(rutils::etfenv$prices$VTI))
retsp <- rutils::diffit(closep)
# Define look-back window
look_back <- 11
# Calculate time series of medians
medianv <- roll::roll_median(closep, width=look_back)
# medianv <- TTR::runMedian(closep, n=look_back)
# Calculate time series of MAD
madv <- HighFreq::roll_var(closep, look_back=look_back, method="nonparametric")
# madv <- TTR::runMAD(closep, n=look_back)
# Calculate time series of z-scores
zscores <- (closep - medianv)/madv
zscores[1:look_back, ] <- 0
tail(zscores, look_back)
range(zscores)
# Define threshold value
threshold <- sum(abs(range(zscores)))/8
# Simulate VTI strategy
posit <- rep(NA_integer_, NROW(closep))
posit[1] <- 0
posit[zscores < -threshold] <- 1
posit[zscores > threshold] <- (-1)
posit <- zoo::na.locf(posit)
posit <- rutils::lagit(posit)
pnls <- retsp*posit

# Plot dygraph of Hampel strategy pnls
wealthv <- cbind(retsp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
endp <- rutils::calc_endpoints(wealthv, interval="months")
dygraphs::dygraph(cumsum(wealthv)[endp],
  main="VTI Hampel Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)

# Create random real symmetric matrix
matrixv <- matrix(runif(25), nc=5)
matrixv <- matrixv + t(matrixv)
# Calculate eigenvectors and eigenvalues
eigend <- eigen(matrixv)
eigenvec <- eigend$vectors
dim(eigenvec)
# Plot eigenvalues
barplot(eigend$values, xlab="", ylab="", las=3,
  names.arg=paste0("ev", 1:NROW(eigend$values)),
  main="Eigenvalues of a real symmetric matrix")

# eigenvectors form an orthonormal basis
round(t(eigenvec) %*% eigenvec, digits=4)
# Diagonalize matrix using eigenvector matrix
round(t(eigenvec) %*% (matrixv %*% eigenvec), digits=4)
eigend$values
# eigen decomposition of matrix by rotating the diagonal matrix
matrixe <- eigenvec %*% (eigend$values * t(eigenvec))
# Create diagonal matrix of eigenvalues
# diagmat <- diag(eigend$values)
# matrixe <- eigenvec %*% (diagmat %*% t(eigenvec))
all.equal(matrixv, matrixe)

# Create random positive semi-definite matrix
matrixv <- matrix(runif(25), nc=5)
matrixv <- t(matrixv) %*% matrixv
# Calculate eigenvectors and eigenvalues
eigend <- eigen(matrixv)
eigend$values
# Plot eigenvalues
barplot(eigend$values, las=3, xlab="", ylab="",
  names.arg=paste0("ev", 1:NROW(eigend$values)),
  main="Eigenvalues of positive semi-definite matrix")

# Perform singular value decomposition
matrixv <- matrix(rnorm(50), nc=5)
svdec <- svd(matrixv)
# Recompose matrixv from SVD mat_rices
all.equal(matrixv, svdec$u %*% (svdec$d*t(svdec$v)))
# Columns of U and V are orthonormal
round(t(svdec$u) %*% svdec$u, 4)
round(t(svdec$v) %*% svdec$v, 4)

# Dimensions of left and right matrices
nleft <- 6 ; nright <- 4
# Calculate left matrix
leftmat <- matrix(runif(nleft^2), nc=nleft)
eigend <- eigen(crossprod(leftmat))
leftmat <- eigend$vectors[, 1:nright]
# Calculate right matrix and singular values
rightmat <- matrix(runif(nright^2), nc=nright)
eigend <- eigen(crossprod(rightmat))
rightmat <- eigend$vectors
singval <- sort(runif(nright, min=1, max=5), decreasing=TRUE)
# Compose rectangular matrix
matrixv <- leftmat %*% (singval * t(rightmat))
# Perform singular value decomposition
svdec <- svd(matrixv)
# Recompose matrixv from SVD
all.equal(matrixv, svdec$u %*% (svdec$d*t(svdec$v)))
# Compare SVD with matrixv components
all.equal(abs(svdec$u), abs(leftmat))
all.equal(abs(svdec$v), abs(rightmat))
all.equal(svdec$d, singval)
# Eigen decomposition of matrixv squared
retsq <- matrixv %*% t(matrixv)
eigend <- eigen(retsq)
all.equal(eigend$values[1:nright], singval^2)
all.equal(abs(eigend$vectors[, 1:nright]), abs(leftmat))
# Eigen decomposition of matrixv squared
retsq <- t(matrixv) %*% matrixv
eigend <- eigen(retsq)
all.equal(eigend$values, singval^2)
all.equal(abs(eigend$vectors), abs(rightmat))

# Create random positive semi-definite matrix
matrixv <- matrix(runif(25), nc=5)
matrixv <- t(matrixv) %*% matrixv
# Calculate the inverse of matrixv
invmat <- solve(a=matrixv)
# Multiply inverse with matrix
round(invmat %*% matrixv, 4)
round(matrixv %*% invmat, 4)

# Calculate eigenvectors and eigenvalues
eigend <- eigen(matrixv)
eigenvec <- eigend$vectors

# Perform eigen decomposition of inverse
inveigen <- eigenvec %*% (t(eigenvec) / eigend$values)
all.equal(invmat, inveigen)
# Decompose diagonal matrix with inverse of eigenvalues
# diagmat <- diag(1/eigend$values)
# inveigen <-
#   eigenvec %*% (diagmat %*% t(eigenvec))

# Random rectangular matrix: nleft > nright
nleft <- 6 ; nright <- 4
matrixv <- matrix(runif(nleft*nright), nc=nright)
# Calculate generalized inverse of matrixv
invmat <- MASS::ginv(matrixv)
round(invmat %*% matrixv, 4)
all.equal(matrixv, matrixv %*% invmat %*% matrixv)
# Random rectangular matrix: nleft < nright
nleft <- 4 ; nright <- 6
matrixv <- matrix(runif(nleft*nright), nc=nright)
# Calculate generalized inverse of matrixv
invmat <- MASS::ginv(matrixv)
all.equal(matrixv, matrixv %*% invmat %*% matrixv)
round(matrixv %*% invmat, 4)
round(invmat %*% matrixv, 4)
# Perform singular value decomposition
svdec <- svd(matrixv)
# Calculate generalized inverse from SVD
invsvd <- svdec$v %*% (t(svdec$u) / svdec$d)
all.equal(invsvd, invmat)
# Calculate Moore-Penrose pseudo-inverse
invmp <- MASS::ginv(t(matrixv) %*% matrixv) %*% t(matrixv)
all.equal(invmp, invmat)

# Create random singular matrix
# More columns than rows: nright > nleft
nleft <- 4 ; nright <- 6
matrixv <- matrix(runif(nleft*nright), nc=nright)
matrixv <- t(matrixv) %*% matrixv
# Perform singular value decomposition
svdec <- svd(matrixv)
# Incorrect inverse from SVD because of zero singular values
invsvd <- svdec$v %*% (t(svdec$u) / svdec$d)
# Inverse property doesn't hold
all.equal(matrixv, matrixv %*% invsvd %*% matrixv)

# Set tolerance for determining zero singular values
precv <- sqrt(.Machine$double.eps)
# Check for zero singular values
round(svdec$d, 12)
notzero <- (svdec$d > (precv * svdec$d[1]))
# Calculate regularized inverse from SVD
invsvd <- svdec$v[, notzero] %*%
  (t(svdec$u[, notzero]) / svdec$d[notzero])
# Verify inverse property of matrixv
all.equal(matrixv, matrixv %*% invsvd %*% matrixv)
# Calculate regularized inverse using MASS::ginv()
invmat <- MASS::ginv(matrixv)
all.equal(invsvd, invmat)
# Calculate Moore-Penrose pseudo-inverse
invmp <- MASS::ginv(t(matrixv) %*% matrixv) %*% t(matrixv)
all.equal(invmp, invmat)

# Diagonalize the unit matrix
unitmat <- matrixv %*% invmat
round(unitmat, 4)
round(matrixv %*% invmat, 4)
round(t(svdec$u) %*% unitmat %*% svdec$v, 4)

# Define a square matrix
matrixv <- matrix(c(1, 2, -1, 2), nc=2)
vectorv <- c(2, 1)
# Calculate the inverse of matrixv
invmat <- solve(a=matrixv)
invmat %*% matrixv
# Calculate solution using inverse of matrixv
solutionv <- invmat %*% vectorv
matrixv %*% solutionv
# Calculate solution of linear system
solutionv <- solve(a=matrixv, b=vectorv)
matrixv %*% solutionv

# Create large random positive semi-definite matrix
matrixv <- matrix(runif(1e4), nc=100)
matrixv <- t(matrixv) %*% matrixv
# Calculate eigen decomposition
eigend <- eigen(matrixv)
eigenval <- eigend$values
eigenvec <- eigend$vectors
# Set tolerance for determining zero singular values
precv <- sqrt(.Machine$double.eps)
# If needed convert to positive definite matrix
notzero <- (eigenval > (precv*eigenval[1]))
if (sum(!notzero) > 0) {
  eigenval[!notzero] <- 2*precv
  matrixv <- eigenvec %*% (eigenval * t(eigenvec))
}  # end if
# Calculate the Cholesky matrixv
cholmat <- chol(matrixv)
cholmat[1:5, 1:5]
all.equal(matrixv, t(cholmat) %*% cholmat)
# Calculate inverse from Cholesky
invchol <- chol2inv(cholmat)
all.equal(solve(matrixv), invchol)
# Compare speed of Cholesky inversion
library(microbenchmark)
summary(microbenchmark(
  solve=solve(matrixv),
  cholmat=chol2inv(chol(matrixv)),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# Calculate random covariance matrix
covmat <- matrix(runif(25), nc=5)
covmat <- t(covmat) %*% covmat
# Calculate the Cholesky matrix
cholmat <- chol(covmat)
cholmat
# Simulate random uncorrelated returns
nassets <- 5
nrows <- 10000
retsp <- matrix(rnorm(nassets*nrows), nc=nassets)
# Calculate correlated returns by applying Cholesky
retscorr <- retsp %*% cholmat
# Calculate covariance matrix
covmat2 <- crossprod(retscorr) /(nrows-1)
all.equal(covmat, covmat2)

# Simulate random portfolio returns
nassets <- 10
nrows <- 100
set.seed(1121)  # Initialize random number generator
retsp <- matrix(rnorm(nassets*nrows), nc=nassets)
# Calculate de-meaned returns matrix
retsp <- t(t(retsp) - colMeans(retsp))
# Or
retsp <- apply(retsp, MARGIN=2, function(x) (x-mean(x)))
# Calculate covariance matrix
covmat <- crossprod(retsp) /(nrows-1)
# Calculate eigenvectors and eigenvalues
eigend <- eigen(covmat)
eigend$values
barplot(eigend$values, # Plot eigenvalues
  xlab="", ylab="", las=3,
  names.arg=paste0("ev", 1:NROW(eigend$values)),
  main="Eigenvalues of covariance matrix")

# Calculate eigenvectors and eigenvalues
# as function of number of returns
ndata <- ((nassets/2):(2*nassets))
eigenval <- sapply(ndata, function(x) {
  retsp <- retsp[1:x, ]
  retsp <- apply(retsp, MARGIN=2, function(y) (y - mean(y)))
  covmat <- crossprod(retsp) / (x-1)
  min(eigen(covmat)$values)
})  # end sapply
plot(y=eigenval, x=ndata, t="l", xlab="", ylab="", lwd=3, col="blue",
  main="Smallest eigenvalue of covariance matrix
  as function of number of returns")

# Create rectangular matrix with collinear columns
matrixv <- matrix(rnorm(10*8), nc=10)
# Calculate covariance matrix
covmat <- cov(matrixv)
# Calculate inverse of covmat - error
invmat <- solve(covmat)
# Calculate regularized inverse of covmat
invmat <- MASS::ginv(covmat)
# Verify inverse property of matrixv
all.equal(covmat, covmat %*% invmat %*% covmat)
# Perform eigen decomposition
eigend <- eigen(covmat)
eigenvec <- eigend$vectors
eigenval <- eigend$values
# Set tolerance for determining zero singular values
precv <- sqrt(.Machine$double.eps)
# Calculate regularized inverse matrix
notzero <- (eigenval > (precv * eigenval[1]))
invreg <- eigenvec[, notzero] %*%
  (t(eigenvec[, notzero]) / eigenval[notzero])
# Verify that invmat is same as invreg
all.equal(invmat, invreg)

# Calculate regularized inverse matrix using cutoff
dimax <- 3
invmat <- eigenvec[, 1:dimax] %*%
  (t(eigenvec[, 1:dimax]) / eigend$values[1:dimax])
# Verify that invmat is same as invreg
all.equal(invmat, invreg)

# Create random covariance matrix
set.seed(1121)
matrixv <- matrix(rnorm(5e2), nc=5)
covmat <- cov(matrixv)
cormat <- cor(matrixv)
stdev <- sqrt(diag(covmat))
# Calculate target matrix
cormean <- mean(cormat[upper.tri(cormat)])
targetmat <- matrix(cormean, nr=NROW(covmat), nc=NCOL(covmat))
diag(targetmat) <- 1
targetmat <- t(t(targetmat * stdev) * stdev)
# Calculate shrinkage covariance matrix
alpha <- 0.5
covshrink <- (1-alpha)*covmat + alpha*targetmat
# Calculate inverse matrix
invmat <- solve(covshrink)

library(rutils)
# Select ETF symbols
symbolv <- c("IEF", "DBC", "XLU", "XLF", "XLP", "XLI")
# Calculate ETF prices and percentage returns
pricets <- rutils::etfenv$prices[, symbolv]
pricets <- zoo::na.locf(pricets, na.rm=FALSE)
pricets <- zoo::na.locf(pricets, fromLast=TRUE)
# Calculate log returns without standardizing
retsp <- rutils::diffit(log(pricets))
# Calculate covariance matrix
covmat <- cov(retsp)
# Standardize (de-mean and scale) the returns
retsp <- lapply(retsp, function(x) {(x - mean(x))/sd(x)})
retsp <- rutils::do_call(cbind, retsp)
round(sapply(retsp, mean), 6)
sapply(retsp, sd)
# Alternative (much slower) center (de-mean) and scale the returns
# retsp <- apply(retsp, 2, scale)
# retsp <- xts::xts(retsp, zoo::index(pricets))
# Alternative (much slower) center (de-mean) and scale the returns
# retsp <- scale(retsp, center=TRUE, scale=TRUE)
# retsp <- xts::xts(retsp, zoo::index(pricets))
# Alternative (much slower) center (de-mean) and scale the returns
# retsp <- t(retsp) - colMeans(retsp)
# retsp <- retsp/sqrt(rowSums(retsp^2)/(NCOL(retsp)-1))
# retsp <- t(retsp)
# retsp <- xts::xts(retsp, zoo::index(pricets))

# Calculate correlation matrix
cormat <- cor(retsp)
# Reorder correlation matrix based on clusters
library(corrplot)
ordern <- corrMatOrder(cormat, order="hclust",
  hclust.method="complete")
cormat <- cormat[ordern, ordern]
# Plot the correlation matrix
colors <- colorRampPalette(c("red", "white", "blue"))
x11(width=6, height=6)
corrplot(cormat, title=NA, tl.col="black", mar=c(0,0,0,0),
    method="square", col=colors(NCOL(cormat)), tl.cex=0.8,
    cl.offset=0.75, cl.cex=0.7, cl.align.text="l", cl.ratio=0.25)
title("ETF Correlation Matrix", line=1)
# Draw rectangles on the correlation matrix plot
corrRect.hclust(cormat, k=NROW(cormat) %/% 2,
  method="complete", col="red")

# create initial vector of portfolio weights
nweights <- NROW(symbolv)
weights <- rep(1/sqrt(nweights), nweights)
names(weights) <- symbolv
# Objective function equal to minus portfolio variance
objfun <- function(weights, retsp) {
  retsp <- retsp %*% weights
  -sum(retsp^2) + 1e7*(1 - sum(weights^2))^2
}  # end objfun
# Objective for equal weight portfolio
objfun(weights, retsp)
# Compare speed of vector multiplication methods
summary(microbenchmark(
  transp=(t(retsp[, 1]) %*% retsp[, 1]),
  sumv=sum(retsp[, 1]^2),
  times=10))[, c(1, 4, 5)]

# Find weights with maximum variance
optiml <- optim(par=weights,
  fn=objfun,
  retsp=retsp,
  method="L-BFGS-B",
  upper=rep(10.0, nweights),
  lower=rep(-10.0, nweights))
# Optimal weights and maximum variance
weights1 <- optiml$par
-objfun(weights1, retsp)
# Plot first principal component weights
barplot(weights1, names.arg=names(weights1), xlab="", ylab="",
  main="First Principal Component Weights")

# PC1 returns
pc1 <- drop(retsp %*% weights1)
# Redefine objective function
objfun <- function(weights, retsp) {
  retsp <- retsp %*% weights
  -sum(retsp^2) + 1e7*(1 - sum(weights^2))^2 +
    1e7*(sum(weights1*weights))^2
}  # end objfun
# Find second PC weights using parallel DEoptim
optiml <- DEoptim::DEoptim(fn=objfun,
  upper=rep(10, NCOL(retsp)),
  lower=rep(-10, NCOL(retsp)),
  retsp=retsp, control=list(parVar="weights1",
    trace=FALSE, itermax=1000, parallelType=1))

# PC2 weights
weights2 <- optiml$optim$bestmem
names(weights2) <- colnames(retsp)
sum(weights2^2)
sum(weights1*weights2)
# PC2 returns
pc2 <- drop(retsp %*% weights2)
# Plot second principal component loadings
barplot(weights2, names.arg=names(weights2), xlab="", ylab="",
  main="Second Principal Component Loadings")

# Calculate eigenvectors and eigenvalues
eigend <- eigen(cormat)
eigend$vectors
# Compare with optimization
all.equal(sum(diag(cormat)), sum(eigend$values))
all.equal(abs(eigend$vectors[, 1]), abs(weights1), check.attributes=FALSE)
all.equal(abs(eigend$vectors[, 2]), abs(weights2), check.attributes=FALSE)
all.equal(eigend$values[1], var(pc1), check.attributes=FALSE)
all.equal(eigend$values[2], var(pc2), check.attributes=FALSE)
# Eigenvalue equations
(cormat %*% weights1) / weights1 / var(pc1)
(cormat %*% weights2) / weights2 / var(pc2)
# Plot eigenvalues
barplot(eigend$values, names.arg=paste0("PC", 1:nweights),
  las=3, xlab="", ylab="", main="Principal Component Variances")

# Eigen decomposition of correlation matrix
eigend <- eigen(cormat)
# Perform PCA with scaling
pcad <- prcomp(retsp, scale=TRUE)
# Compare outputs
all.equal(eigend$values, pcad$sdev^2)
all.equal(abs(eigend$vectors), abs(pcad$rotation),
    check.attributes=FALSE)
# Eigen decomposition of covariance matrix
eigend <- eigen(covmat)
# Perform PCA without scaling
pcad <- prcomp(retsp, scale=FALSE)
# Compare outputs
all.equal(eigend$values, pcad$sdev^2)
all.equal(abs(eigend$vectors), abs(pcad$rotation),
    check.attributes=FALSE)

# Redefine objective function to minimize variance
objfun <- function(weights, retsp) {
  retsp <- retsp %*% weights
  sum(retsp^2) + 1e7*(1 - sum(weights^2))^2
}  # end objfun
# Find highest order PC weights using parallel DEoptim
optiml <- DEoptim::DEoptim(fn=objfun,
  upper=rep(10, NCOL(retsp)),
  lower=rep(-10, NCOL(retsp)),
  retsp=retsp, control=list(trace=FALSE,
    itermax=1000, parallelType=1))
# PC6 weights and returns
weights6 <- optiml$optim$bestmem
names(weights6) <- colnames(retsp)
sum(weights6^2)
sum(weights1*weights6)
# Compare with eigend vector
weights6
eigend$vectors[, 6]
# Calculate objective function
objfun(weights6, retsp)
objfun(eigend$vectors[, 6], retsp)

# Plot highest order principal component loadings
x11(width=6, height=5)
par(mar=c(2.5, 2, 2, 3), oma=c(0, 0, 0, 0), mgp=c(2, 0.5, 0))
barplot(weights6, names.arg=names(weights2), xlab="", ylab="",
  main="Highest Order Principal Component Loadings")

# Perform principal component analysis PCA
pcad <- prcomp(retsp, scale=TRUE)
# Plot standard deviations of principal components
barplot(pcad$sdev, names.arg=colnames(pcad$rotation),
  las=3, xlab="", ylab="",
  main="Scree Plot: Volatilities of Principal Components \n of Stock Returns")

# Calculate principal component loadings (weights)
pcad$rotation
# Plot barplots with PCA weights in multiple panels
x11(width=6, height=7)
par(mfrow=c(nweights/2, 2))
par(mar=c(3, 2, 2, 1), oma=c(0, 0, 0, 0))
for (ordern in 1:nweights) {
  barplot(pcad$rotation[, ordern], las=3, xlab="", ylab="", main="")
  title(paste0("PC", ordern), line=-1, col.main="red")
}  # end for

# Calculate products of principal component time series
round(t(pcad$x) %*% pcad$x, 2)
# Calculate principal component time series from returns
dates <- zoo::index(pricets)
retspca <- xts::xts(retsp %*% pcad$rotation, order.by=dates)
round(cov(retspca), 3)
all.equal(coredata(retspca), pcad$x, check.attributes=FALSE)
pcacum <- cumsum(retspca)
# Plot principal component time series in multiple panels
rangev <- range(pcacum)
for (ordern in 1:nweights) {
  plot.zoo(pcacum[, ordern], ylim=rangev, xlab="", ylab="")
  title(paste0("PC", ordern), line=-1, col.main="red")
}  # end for

# Invert all the principal component time series
retspca <- retsp %*% pcad$rotation
solved <- retspca %*% solve(pcad$rotation)
all.equal(coredata(retsp), solved)
# Invert first 3 principal component time series
solved <- retspca[, 1:3] %*% solve(pcad$rotation)[1:3, ]
solved <- xts::xts(solved, dates)
solved <- cumsum(solved)
retc <- cumsum(retsp)
# Plot the solved returns
for (symbol in symbolv) {
  plot.zoo(cbind(retc[, symbol], solved[, symbol]),
    plot.type="single", col=c("black", "blue"), xlab="", ylab="")
  legend(x="topleft", bty="n", legend=paste0(symbol, c("", " solved")),
   title=NULL, inset=0.0, cex=1.0, lwd=6, lty=1, col=c("black", "blue"))
}  # end for

# Formula of linear model with zero intercept
formulav <- z ~ x + y - 1
formulav

# Collapse vector of strings into single text string
paste0("x", 1:5)
paste(paste0("x", 1:5), collapse="+")

# Create formula from text string
formulav <- as.formula(
  # Coerce text strings to formula
  paste("z ~ ",
  paste(paste0("x", 1:5), collapse="+")
  )  # end paste
)  # end as.formula
class(formulav)
formulav
# Modify the formula using "update"
update(formulav, log(.) ~ . + beta)

# Define explanatory (predictor) variable
nrows <- 100
set.seed(1121)  # Initialize random number generator
predictor <- runif(nrows)
noise <- rnorm(nrows)
# Response equals linear form plus random noise
response <- (-3 + 2*predictor + noise)

# Calculate de-meaned explanatory (predictor) and response vectors
predictor_zm <- predictor - mean(predictor)
response_zm <- response - mean(response)
# Calculate the regression beta
betav <- cov(predictor, response)/var(predictor)
# Calculate the regression alpha
alpha <- mean(response) - betav*mean(predictor)

# Specify regression formula
formulav <- response ~ predictor
model <- lm(formulav)  # Perform regression
class(model)  # Regressions have class lm
attributes(model)
eval(model$call$formula)  # Regression formula
model$coeff  # Regression coefficients
all.equal(coef(model), c(alpha, betav),
  check.attributes=FALSE)

fittedv <- (alpha + betav*predictor)
all.equal(fittedv, model$fitted.values, check.attributes=FALSE)
x11(width=5, height=4)  # Open x11 for plotting
# Set plot parameters to reduce whitespace around plot
par(mar=c(5, 5, 2, 1), oma=c(0, 0, 0, 0))
# Plot scatterplot using formula
plot(formulav, xlab="predictor", ylab="response")
title(main="Simple Regression", line=0.5)
# Add regression line
abline(model, lwd=3, col="blue")
# Plot fitted (predicted) response values
points(x=predictor, y=model$fitted.values, pch=16, col="blue")

# Plot response without noise
lines(x=predictor, y=(response-noise), col="red", lwd=3)
legend(x="topleft", # Add legend
       legend=c("response without noise", "fitted values"),
       title=NULL, inset=0.08, cex=0.8, lwd=6,
       lty=1, col=c("red", "blue"))

# Calculate the residuals
fittedv <- (alpha + betav*predictor)
residuals <- (response - fittedv)
all.equal(residuals, model$residuals, check.attributes=FALSE)
# Residuals are orthogonal to the predictor
all.equal(sum(residuals*predictor), target=0)
# Residuals are orthogonal to the fitted values
all.equal(sum(residuals*fittedv), target=0)
# Sum of residuals is equal to zero
all.equal(mean(residuals), target=0)

x11(width=6, height=5)  # Open x11 for plotting
# Set plot parameters to reduce whitespace around plot
par(mar=c(5, 5, 1, 1), oma=c(0, 0, 0, 0))
# Extract residuals
datav <- cbind(predictor, model$residuals)
colnames(datav) <- c("predictor", "residuals")
# Plot residuals
plot(datav)
title(main="Residuals of the Linear Regression", line=-1)
abline(h=0, lwd=3, col="red")

# Degrees of freedom of residuals
degf <- model$df.residual
# Standard deviation of residuals
residsd <- sqrt(sum(residuals^2)/degf)
# Standard error of beta
betasd <- residsd/sqrt(sum(predictor_zm^2))
# Standard error of alpha
alphasd <- residsd*
  sqrt(1/nrows + mean(predictor)^2/sum(predictor_zm^2))

modelsum <- summary(model)  # Copy regression summary
modelsum  # Print the summary to console
attributes(modelsum)$names  # get summary elements

modelsum$coeff
# Standard errors
modelsum$coefficients[2, "Std. Error"]
all.equal(c(alphasd, betasd),
  modelsum$coefficients[, "Std. Error"], 
  check.attributes=FALSE)
# R-squared
modelsum$r.squared
modelsum$adj.r.squared
# F-statistic and ANOVA
modelsum$fstatistic
anova(model)

set.seed(1121)  # initialize random number generator
# High noise compared to coefficient
response <- (-3 + 2*predictor + rnorm(nrows, sd=8))
model <- lm(formulav)  # Perform regression
# Values of regression coefficients are not
# Statistically significant
summary(model)

par(oma=c(1, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=1.0, cex.axis=1.0, cex.main=1.0, cex.sub=1.0)
reg_stats <- function(stdev) {  # Noisy regression
  set.seed(1121)  # initialize number generator
# Define explanatory (predictor) and response variables
  predictor <- rnorm(100, mean=2)
  response <- (1 + 0.2*predictor +
  rnorm(NROW(predictor), sd=stdev))
# Specify regression formula
  formulav <- response ~ predictor
# Perform regression and get summary
  modelsum <- summary(lm(formulav))
# Extract regression statistics
  with(modelsum, c(pval=coefficients[2, 4],
   adj_rsquared=adj.r.squared,
   fstat=fstatistic[1]))
}  # end reg_stats
# Apply reg_stats() to vector of std dev values
vecsd <- seq(from=0.1, to=0.5, by=0.1)
names(vecsd) <- paste0("sd=", vecsd)
statsmat <- t(sapply(vecsd, reg_stats))
# Plot in loop
par(mfrow=c(NCOL(statsmat), 1))
for (it in 1:NCOL(statsmat)) {
  plot(statsmat[, it], type="l",
 xaxt="n", xlab="", ylab="", main="")
  title(main=colnames(statsmat)[it], line=-1.0)
  axis(1, at=1:(NROW(statsmat)), labels=rownames(statsmat))
}  # end for

reg_stats <- function(datav) {  # get regression
# Perform regression and get summary
  colnamev <- colnames(datav)
  formulav <- paste(colnamev[2], colnamev[1], sep="~")
  modelsum <- summary(lm(formulav, data=datav))
# Extract regression statistics
  with(modelsum, c(pval=coefficients[2, 4],
   adj_rsquared=adj.r.squared,
   fstat=fstatistic[1]))
}  # end reg_stats
# Apply reg_stats() to vector of std dev values
vecsd <- seq(from=0.1, to=0.5, by=0.1)
names(vecsd) <- paste0("sd=", vecsd)
statsmat <- t(sapply(vecsd, function(stdev) {
    set.seed(1121)  # initialize number generator
# Define explanatory (predictor) and response variables
    predictor <- rnorm(100, mean=2)
    response <- (1 + 0.2*predictor +
rnorm(NROW(predictor), sd=stdev))
    reg_stats(data.frame(predictor, response))
    }))
# Plot in loop
par(mfrow=c(NCOL(statsmat), 1))
for (it in 1:NCOL(statsmat)) {
  plot(statsmat[, it], type="l",
 xaxt="n", xlab="", ylab="", main="")
  title(main=colnames(statsmat)[it], line=-1.0)
  axis(1, at=1:(NROW(statsmat)),
 labels=rownames(statsmat))
}  # end for

# Set plot paramaters - margins and font scale
par(oma=c(1,0,1,0), mgp=c(2,1,0), mar=c(2,1,2,1), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2, 2))  # Plot 2x2 panels
plot(model)  # Plot diagnostic scatterplots
plot(model, which=2)  # Plot just Q-Q

library(lmtest)  # Load lmtest
# Perform Durbin-Watson test
lmtest::dwtest(model)

x11(width=6, height=5)  # Open x11 for plotting
# Set plot parameters to reduce whitespace around plot
par(mar=c(5, 5, 2, 1), oma=c(0, 0, 0, 0))
# Add unit column to the predictor matrix
predictor <- cbind(rep(1, nrows), predictor)
# Calculate generalized inverse of the predictor matrix
invpred <- MASS::ginv(predictor)
# Calculate the influence matrix
influencem <- predictor %*% invpred
# Plot the leverage vector
ordern <- order(predictor[, 2])
plot(x=predictor[ordern, 2], y=diag(influencem)[ordern],
     type="l", lwd=3, col="blue",
     xlab="predictor", ylab="leverage",
     main="Leverage as Function of Predictor")

# Calculate the influence matrix
influencem <- predictor %*% invpred
# The influence matrix is idempotent
all.equal(influencem, influencem %*% influencem)

# Calculate covariance and standard deviations of fitted values
betas <- invpred %*% response
fittedv <- drop(predictor %*% betas)
residuals <- drop(response - fittedv)
degf <- (NROW(predictor) - NCOL(predictor))
residvar <- sqrt(sum(residuals^2)/degf)
fitcovar <- residvar*influencem
fitsd <- sqrt(diag(fitcovar))
# Plot the standard deviations
fitsd <- cbind(fitted=fittedv, stddev=fitsd)
fitsd <- fitsd[order(fittedv), ]
plot(fitsd, type="l", lwd=3, col="blue",
     xlab="Fitted Value", ylab="Standard Deviation",
     main="Standard Deviations of Fitted Values\nin Univariate Regression")

# Calculate response without random noise for univariate regression,
# equal to weighted sum over columns of predictor.
betas <- c(-1, 1)
response <- predictor %*% betas
# Perform loop over different realizations of random noise
fittedv <- lapply(1:50, function(it) {
  # Add random noise to response
  response <- response + rnorm(nrows, sd=1.0)
  # Calculate fitted values using influence matrix
  influencem %*% response
})  # end lapply
fittedv <- rutils::do_call(cbind, fittedv)

x11(width=5, height=4)  # Open x11 for plotting
# Set plot parameters to reduce whitespace around plot
par(mar=c(5, 5, 2, 1), oma=c(0, 0, 0, 0))
# Plot fitted values
matplot(x=predictor[,2], y=fittedv,
type="l", lty="solid", lwd=1, col="blue",
xlab="predictor", ylab="fitted",
main="Fitted Values for Different Realizations
of Random Noise")
lines(x=predictor[,2], y=response, col="red", lwd=4)
legend(x="topleft", # Add legend
       legend=c("response without noise", "fitted values"),
       title=NULL, inset=0.05, cex=0.8, lwd=6,
       lty=1, col=c("red", "blue"))

# Inverse of predictor matrix squared
predictor2 <- MASS::ginv(crossprod(predictor))
# Define new predictors
newdata <- (max(predictor[, 2]) + 10*(1:5)/nrows)
# Calculate the predicted values and standard errors
predictorn <- cbind(rep(1, NROW(newdata)), newdata)
predsd <- sqrt(predictorn %*% predictor2 %*% t(predictorn))
predictv <- cbind(
  prediction=drop(predictorn %*% betas),
  stddev=diag(residvar*predsd))
# Or: Perform loop over predictorn
predictv <- apply(predictorn, MARGIN=1, function(predictor) {
  # Calculate predicted values
  prediction <- predictor %*% betas
  # Calculate standard deviation
  predsd <- sqrt(t(predictor) %*% predictor2 %*% predictor)
  predictsd <- residvar*predsd
  c(prediction=prediction, stddev=predictsd)
})  # end sapply
predictv <- t(predictv)

# Prepare plot data
xdata <- c(predictor[,2], newdata)
xlim <- range(xdata)
ydata <- c(fittedv, predictv[, 1])
# Calculate t-quantile
tquant <- qt(pnorm(2), df=degf)
predictlow <- predictv[, 1]-tquant*predictv[, 2]
predicthigh <- predictv[, 1]+tquant*predictv[, 2]
ylim <- range(c(response, ydata, predictlow, predicthigh))
# Plot the regression predictions
plot(x=xdata, y=ydata, xlim=xlim, ylim=ylim,
     type="l", lwd=3, col="blue",
     xlab="predictor", ylab="fitted or predicted",
     main="Predictions from Linear Regression")
points(x=predictor[,2], y=response, col="blue")
points(x=newdata, y=predictv[, 1], pch=16, col="blue")
lines(x=newdata, y=predicthigh, lwd=3, col="red")
lines(x=newdata, y=predictlow, lwd=3, col="green")
legend(x="topleft", # Add legend
       legend=c("predictions", "+2SD", "-2SD"),
       title=NULL, inset=0.05, cex=0.8, lwd=6,
       lty=1, col=c("blue", "red", "green"))

# Perform univariate regression
predictor <- predictor[, 2]
model <- lm(response ~ predictor)
# Perform prediction from regression
newdata <- data.frame(predictor=newdata)
predictlm <- predict(object=model,
  newdata=newdata, confl=1-2*(1-pnorm(2)),
  interval="confidence")
predictlm <- as.data.frame(predictlm)
all.equal(predictlm$fit, predictv[, 1])
all.equal(predictlm$lwr, predictlow)
all.equal(predictlm$upr, predicthigh)
plot(response ~ predictor,
     xlim=range(predictor, newdata),
     ylim=range(response, predictlm),
     xlab="predictor", ylab="fitted or predicted",
     main="Predictions from lm() Regression")

abline(model, col="blue", lwd=3)
with(predictlm, {
  points(x=newdata$predictor, y=fit, pch=16, col="blue")
  lines(x=newdata$predictor, y=lwr, lwd=3, col="green")
  lines(x=newdata$predictor, y=upr, lwd=3, col="red")
})  # end with
legend(x="topleft", # Add legend
       legend=c("predictions", "+2SD", "-2SD"),
       title=NULL, inset=0.05, cex=0.8, lwd=6,
       lty=1, col=c("blue", "red", "green"))

set.seed(1121)
library(lmtest)
# Spurious regression in unit root time series
predictor <- cumsum(rnorm(100))  # Unit root time series
response <- cumsum(rnorm(100))
formulav <- response ~ predictor
model <- lm(formulav)  # Perform regression
# Summary indicates statistically significant regression
modelsum <- summary(model)
modelsum$coeff
modelsum$r.squared
# Durbin-Watson test shows residuals are autocorrelated
dw_test <- lmtest::dwtest(model)
c(dw_test$statistic[[1]], dw_test$p.value)

par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # Set plot panels
plot(formulav, xlab="", ylab="")  # Plot scatterplot using formula
title(main="Spurious Regression", line=-1)
# Add regression line
abline(model, lwd=2, col="red")
plot(model, which=2, ask=FALSE)  # Plot just Q-Q

# Define predictor matrix
nrows <- 100
ncols <- 5
set.seed(1121)  # initialize random number generator
predictor <- matrix(rnorm(nrows*ncols), ncol=ncols)
# Add column names
colnames(predictor) <- paste0("col", 1:ncols)
# Define the predictor weights
weights <- sample(3:(ncols+2))
# Response equals weighted predictor plus random noise
noise <- rnorm(nrows, sd=5)
response <- (-3 + 2*predictor %*% weights + noise)

# Perform multivariate regression using lm()
model <- lm(response ~ predictor)
# Solve multivariate regression using matrix algebra
# Calculate de-meaned predictor matrix and response vector
predictor_zm <- t(t(predictor) - colMeans(predictor))
# predictor <- apply(predictor, 2, function(x) (x-mean(x)))
response_zm <- response - mean(response)
# Calculate the regression coefficients
betas <- drop(MASS::ginv(predictor_zm) %*% response_zm)
# Calculate the regression alpha
alpha <- mean(response) - sum(colSums(predictor)*betas)/nrows
# Compare with coefficients from lm()
all.equal(coef(model), c(alpha, betas), check.attributes=FALSE)
# Compare with actual coefficients
all.equal(c(-1, weights), c(alpha, betas), check.attributes=FALSE)

# Add intercept column to predictor matrix
predictor <- cbind(rep(1, NROW(predictor)), predictor)
ncols <- NCOL(predictor)
# Add column name
colnames(predictor)[1] <- "intercept"
# Calculate generalized inverse of the predictor matrix
invpred <- MASS::ginv(predictor)
# Calculate the regression coefficients
betas <- invpred %*% response
# Perform multivariate regression without intercept term
model <- lm(response ~ predictor - 1)
all.equal(drop(betas), coef(model), check.attributes=FALSE)

# Calculate fitted values from regression coefficients
fittedv <- drop(predictor %*% betas)
all.equal(fittedv, model$fitted.values, check.attributes=FALSE)
# Calculate the residuals
residuals <- drop(response - fittedv)
all.equal(residuals, model$residuals, check.attributes=FALSE)
# Residuals are orthogonal to predictor columns (predictors)
sapply(residuals %*% predictor, all.equal, target=0)
# Residuals are orthogonal to the fitted values
all.equal(sum(residuals*fittedv), target=0)
# Sum of residuals is equal to zero
all.equal(sum(residuals), target=0)

# Calculate the influence matrix
influencem <- predictor %*% invpred
# The influence matrix is idempotent
all.equal(influencem, influencem %*% influencem)
# Calculate fitted values using influence matrix
fittedv <- drop(influencem %*% response)
all.equal(fittedv, model$fitted.values, check.attributes=FALSE)
# Calculate fitted values from regression coefficients
fittedv <- drop(predictor %*% betas)
all.equal(fittedv, model$fitted.values, check.attributes=FALSE)

# Calculate zero mean fitted values
predictor_zm <- t(t(predictor) - colMeans(predictor))
fitted_zm <- drop(predictor_zm %*% betas)
all.equal(fitted_zm,
  model$fitted.values - mean(response),
  check.attributes=FALSE)
# Calculate the residuals
response_zm <- response - mean(response)
residuals <- drop(response_zm - fitted_zm)
all.equal(residuals, model$residuals,
  check.attributes=FALSE)
# Calculate the influence matrix
influence_zm <- predictor_zm %*% MASS::ginv(predictor_zm)
# Compare the fitted values
all.equal(fitted_zm,
  drop(influence_zm %*% response_zm),
  check.attributes=FALSE)

library(lmtest)  # Load lmtest
# Define predictor matrix
predictor <- 1:30
omitv <- sin(0.2*1:30)
# Response depends on both predictors
response <- 0.2*predictor + omitv + 0.2*rnorm(30)
# Mis-specified regression only one predictor
model_ovb <- lm(response ~ predictor)
modelsum <- summary(model_ovb)
modelsum$coeff
modelsum$r.squared
# Durbin-Watson test shows residuals are autocorrelated
lmtest::dwtest(model_ovb)
# Plot the regression diagnostic plots
x11(width=5, height=7)
par(mfrow=c(2,1))  # Set plot panels
par(mar=c(3, 2, 1, 1), oma=c(1, 0, 0, 0))
plot(response ~ predictor)
abline(model_ovb, lwd=2, col="red")
title(main="Omitted Variable Regression", line=-1)
plot(model_ovb, which=2, ask=FALSE)  # Plot just Q-Q

# Regression model summary
modelsum <- summary(model)
# Degrees of freedom of residuals
nrows <- NROW(predictor)
ncols <- NCOL(predictor)
degf <- (nrows - ncols)
all.equal(degf, modelsum$df[2])
# Variance of residuals
residvar <- sum(residuals^2)/degf

# Inverse of predictor matrix squared
predictor2 <- MASS::ginv(crossprod(predictor))
# predictor2 <- t(predictor) %*% predictor
# Variance of residuals
residvar <- sum(residuals^2)/degf
# Calculate covariance matrix of betas
beta_covar <- residvar*predictor2
# Round(beta_covar, 3)
betasd <- sqrt(diag(beta_covar))
all.equal(betasd, modelsum$coeff[, 2], check.attributes=FALSE)
# Calculate t-values of betas
beta_tvals <- drop(betas)/betasd
all.equal(beta_tvals, modelsum$coeff[, 3], check.attributes=FALSE)
# Calculate two-sided p-values of betas
beta_pvals <- 2*pt(-abs(beta_tvals), df=degf)
all.equal(beta_pvals, modelsum$coeff[, 4], check.attributes=FALSE)
# The square of the generalized inverse is equal
# to the inverse of the square
all.equal(MASS::ginv(crossprod(predictor)),
  invpred %*% t(invpred))

# Calculate the influence matrix
influencem <- predictor %*% invpred
# The influence matrix is idempotent
all.equal(influencem, influencem %*% influencem)

# Calculate covariance and standard deviations of fitted values
fitcovar <- residvar*influencem
fitsd <- sqrt(diag(fitcovar))
# Sort the standard deviations
fitsd <- cbind(fitted=fittedv, stddev=fitsd)
fitsd <- fitsd[order(fittedv), ]
# Plot the standard deviations
plot(fitsd, type="l", lwd=3, col="blue",
     xlab="Fitted Value", ylab="Standard Deviation",
     main="Standard Deviations of Fitted Values\nin Multivariate Regression")

# Load time series of ETF percentage returns
retsp <- rutils::etfenv$returns[, c("XLF", "XLE")]
retsp <- na.omit(retsp)
nrows <- NROW(retsp)
head(retsp)
# Define regression formula
formulav <- paste(colnames(retsp)[1],
  paste(colnames(retsp)[-1], collapse="+"),
  sep=" ~ ")
# Standard regression
model <- lm(formulav, data=retsp)
modelsum <- summary(model)
# Bootstrap of regression
set.seed(1121)  # initialize random number generator
bootd <- sapply(1:100, function(x) {
  samplev <- sample.int(nrows, replace=TRUE)
  model <- lm(formulav, data=retsp[samplev, ])
  model$coefficients
})  # end sapply
# Means and standard errors from regression
modelsum$coefficients
# Means and standard errors from bootstrap
dim(bootd)
t(apply(bootd, MARGIN=1,
function(x) c(mean=mean(x), stderror=sd(x))))

# New data predictor is a data frame or row vector
set.seed(1121)
newdata <- data.frame(matrix(c(1, rnorm(5)), nr=1))
colnamev <- colnames(predictor)
colnames(newdata) <- colnamev
newdatav <- as.matrix(newdata)
prediction <- drop(newdatav %*% betas)
predsd <- drop(sqrt(newdatav %*% beta_covar %*% t(newdatav)))

# Create formula from text string
formulav <- paste0("response ~ ",
  paste(colnames(predictor), collapse=" + "), " - 1")
# Specify multivariate regression using formula
model <- lm(formulav, data=data.frame(cbind(response, predictor)))
modelsum <- summary(model)
# Predict from lm object
predictlm <- predict.lm(object=model, newdata=newdata,
   interval="confidence", confl=1-2*(1-pnorm(2)))
# Calculate t-quantile
tquant <- qt(pnorm(2), df=degf)
predicthigh <- (prediction + tquant*predsd)
predictlow <- (prediction - tquant*predsd)
# Compare with matrix calculations
all.equal(predictlm[1, "fit"], prediction)
all.equal(predictlm[1, "lwr"], predictlow)
all.equal(predictlm[1, "upr"], predicthigh)

# TSS = ESS + RSS
tss <- sum((response-mean(response))^2)
ess <- sum((fittedv-mean(fittedv))^2)
rss <- sum(residuals^2)
all.equal(tss, ess + rss)

# Set regression attribute for intercept
attributes(model$terms)$intercept <- 1
# Regression summary
modelsum <- summary(model)
# Regression R-squared
rsquared <- ess/tss
all.equal(rsquared, modelsum$r.squared)
# Correlation between response and fitted values
cor_fitted <- drop(cor(response, fittedv))
# Squared correlation between response and fitted values
all.equal(cor_fitted^2, rsquared)

nrows <- NROW(predictor)
ncols <- NCOL(predictor)
# Degrees of freedom of residuals
degf <- (nrows - ncols)
# Adjusted R-squared
rsquared_adj <- (1-sum(residuals^2)/degf/var(response))
# Compare adjusted R-squared from lm()
all.equal(drop(rsquared_adj), modelsum$adj.r.squared)

x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
# Plot three curves in loop
degf <- c(3, 5, 9)  # Degrees of freedom
colors <- c("black", "red", "blue", "green")
for (it in 1:NROW(degf)) {
curve(expr=df(x, df1=degf[it], df2=3),
xlim=c(0, 4), xlab="", ylab="", lwd=2,
col=colors[it], add=as.logical(it-1))
}  # end for

# Add title
title(main="F-Distributions", line=0.5)
# Add legend
labelv <- paste("df", degf, sep="=")
legend("topright", inset=0.05, title="degrees of freedom",
       labelv, cex=0.8, lwd=2, lty=1, col=colors)

sigmax <- var(rnorm(nrows))
sigmay <- var(rnorm(nrows))
fratio <- sigmax/sigmay
# Cumulative probability for q = fratio
pf(fratio, nrows-1, nrows-1)
# p-value for fratios
1-pf((10:20)/10, nrows-1, nrows-1)

# F-statistic from lm()
modelsum$fstatistic
# Degrees of freedom of residuals
degf <- (nrows - ncols)
# F-statistic from ESS and RSS
fstat <- (ess/(ncols-1))/(rss/degf)
all.equal(fstat, modelsum$fstatistic[1], check.attributes=FALSE)
# p-value of F-statistic
1-pf(q=fstat, df1=(ncols-1), df2=(nrows-ncols))

# Calculate ETF returns
retsp <- na.omit(rutils::etfenv$returns)
# Perform singular value decomposition
svdec <- svd(retsp)
barplot(svdec$d, main="Singular Values of ETF Returns")

# Calculate generalized inverse from SVD
invmat <- svdec$v %*% (t(svdec$u) / svdec$d)
# Verify inverse property of inverse
all.equal(zoo::coredata(retsp), retsp %*% invmat %*% retsp)
# Calculate regularized inverse from SVD
dimax <- 1:3
invreg <- svdec$v[, dimax] %*%
  (t(svdec$u[, dimax]) / svdec$d[dimax])
# Calculate regularized inverse using RcppArmadillo
invcpp <- HighFreq::calc_inv(retsp, dimax=3)
all.equal(invreg, invcpp, check.attributes=FALSE)
# Calculate regularized inverse from Moore-Penrose pseudo-inverse
retsq <- t(retsp) %*% retsp
eigend <- eigen(retsq)
squared_inv <- eigend$vectors[, dimax] %*%
  (t(eigend$vectors[, dimax]) / eigend$values[dimax])
invmp <- squared_inv %*% t(retsp)
all.equal(invreg, invmp, check.attributes=FALSE)

# Define transformation matrix
trans_mat <- matrix(runif(ncols^2, min=(-1), max=1), ncol=ncols)
# Calculate linear combinations of predictor columns
predictor_trans <- predictor %*% trans_mat
# Calculate the influence matrix
influence_trans <- predictor_trans %*% MASS::ginv(predictor_trans)
# Compare the influence matrices
all.equal(influencem, influence_trans)
