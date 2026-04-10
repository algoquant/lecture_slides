# Define logarithmic utility
utilfun <- function(frac, p=0.3, a=20, b=1) {
  p*log(1+frac*a) + (1-p)*log(1-frac*b)
}  # end utilfun
# Plot utility
curve(expr=utilfun, xlim=c(0, 1),
ylim=c(-0.5, 0.4), xlab="betting fraction",
ylab="utility", main="", lwd=2)
title(main="Logarithmic Utility", line=0.5)

# Define and plot Kelly ratio
kelly_ratio <- function(a, b, p) {
  p/b - (1-p)/a
}  # end kelly_ratio
# Define and plot Kelly ratio
curve(expr=kelly_ratio(x, p=0.5, b=1), xlim=c(0, 5),
ylim=c(-2, 1), xlab="win amount",
ylab="Kelly ratio", main="", lwd=2)
abline(h=0.5, lwd=2, col="red")
text(x=1.5, y=0.5, pos=3, cex=0.8, labels="max Kelly ratio=0.5")
title(main="Kelly ratio", line=-0.8)

# Plot several Kelly curves
curve(expr=kelly_ratio(x, p=0.5, b=1), xlim=c(0, 5),
ylim=c(-0.5, 0.75), xlab="win amount",
ylab="Kelly ratio", main="", lwd=2)
abline(h=0.5, lwd=2, col="red")
text(x=1.5, y=0.5, pos=3, cex=1.0, labels="b=1.0; max ratio=0.5")
curve(expr=kelly_ratio(x, p=0.5, b=2), add=TRUE, main="", lwd=2)
abline(h=0.25, lwd=2, col="red")
text(x=1.5, y=0.25, pos=3, cex=1.0, labels="b=2.0; max ratio=0.25")
title(main="Kelly Ratios For Different Loss Amounts", line=-0.8)

# Wealth of multiperiod binary betting
wealthv <- function(f, a, b, n, p) {
  m <- n*p
  (1+f*a)^m * (1-f*b)^(n-m)
}  # end wealth
curve(expr=wealthv(x, a=0.8, b=0.1, n=1e3, p=0.15), xlim=c(0, 1),
xlab="betting fraction",
ylab="wealth", main="", lwd=2)
title(main="Wealth of Multiperiod Betting", line=0.1)

# Simulate stock prices
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
pricep <- sapply(1:3, function(x)
  cumprod(1 + rnorm(1e3, sd=0.01)))
colorv <- c("blue", "red", "green")
matplot(pricep, type="l", lwd=2,
  main="Simulated Stock Prices",
  ylim=range(pricep), col=colorv,
  lty="solid", xlab="time", ylab="price")
abline(h=0.5, col="purple", lwd=3)
text(x=200, y=0.5, pos=3, labels="liquidation threshold")

# Calculate the VTI returns
retp <- rutils::etfenv$returns$VTI
retp <- na.omit(retp)
c(mean=mean(retp), stdev=sd(retp))
range(retp)

# Define vectorized logarithmic utility function
utilfun <- function(fracv, retp) {
  sapply(fracv, function(x) sum(log(1 + x*retp)))
}  # end utilfun
utilfun(1, retp)
utilfun(c(1, 4), retp)
# Plot the logarithmic utility
curve(expr=utilfun(x, retp=retp),
xlim=c(0.1, 5), xlab="leverage", ylab="utility",
main="Utility of Asset Returns", lwd=2)

# Approximate Kelly ratio
mean(retp)/var(retp)
PerformanceAnalytics::KellyRatio(R=retp, method="full")
# Kelly leverage
unlist(optimize(
  f=function(x) -utilfun(x, retp),
  interval=c(1, 4)))

# Calculate the VTI returns
retp <- rutils::etfenv$returns$VTI
retp <- na.omit(retp)
# Calculate the wealth paths
kellyr <- drop(mean(retp)/var(retp))
wealthk <- cumprod(1 + kellyr*retp)
wealthyper <- cumprod(1 + (kellyr+2)*retp)
wealthsub <- cumprod(1 + (kellyr-2)*retp)
wealthp <- cbind(wealthk, wealthyper, wealthsub)
colnames(wealthp) <- c("kelly", "hyper-kelly", "sub-kelly")

# Dygraph plot o wealth paths
dygraphs::dygraph(wealthp, main="Wealth Paths") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dyLegend(show="always", width=300)

# bidask equal to 1 bp for liquid ETFs
bidask <- 0.001
# Calculate the wealth paths
kellyr <- drop(mean(retp)/var(retp))
wealthv <- cumprod(1 + kellyr*retp)
wealth_trans <- cumprod(1 + kellyr*retp -
  0.5*bidask*kellyr*(kellyr-1)*abs(retp))
# Calculate the compounded wealth from returns
wealthv <- cbind(wealthv, wealth_trans)
colnames(wealthv) <- c("Kelly", "Including bid-ask")
# Plot compounded wealth
dygraphs::dygraph(wealthv, main="Kelly Strategy With Transaction Costs") %>%
  dyOptions(colors=c("green", "blue"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Plot logarithmic utility function
curve(expr=log, lwd=3, col="blue", xlim=c(0.5, 5),
xlab="wealth", ylab="utility",
main="Logarithmic Utility")

# Define CRRA utility
crra_util <- function(w, ra) {
  (w^(1-ra) - 1)/(1-ra)
}  # end crra_util
# Plot utility functions
curve(expr=crra_util(x, ra=0.7), xlim=c(0.5, 5), lwd=3,
xlab="wealth", ylab="utility", main="", col="blue")
curve(expr=log, add=TRUE, lwd=3)
curve(expr=crra_util(x, ra=1.3), add=TRUE, lwd=3, col="red")
# Add title and legend
title(main="CRRA Utility", line=0.5)
legend(x="topleft", legend=c("risk seeking", "logarithmic", "risk averse"),
 title="Risk Aversion", inset=0.05, cex=0.8, bg="white", y.intersp=0.5,
 lwd=6, lty=1, bty="n", col=c("blue", "black", "red"))

# Calculate the VTI returns
retp <- rutils::etfenv$returns$VTI
retp <- na.omit(retp)
# Calculate the higher moments of VTI returns
c(mean=sum(retp),
  variance=sum(retp^2),
  mom3=sum(retp^3),
  mom4=sum(retp^4))/NROW(retp)
# Calculate the higher moments of minutely SPY returns
spy <- HighFreq::SPY[, 4]
spy <- na.omit(spy)
spy <- rutils::diffit(log(spy))
c(mean=sum(spy),
  variance=sum(spy^2),
  mom3=sum(spy^3),
  mom4=sum(spy^4))/NROW(spy)

retp <- na.omit(rutils::etfenv$returns[, c("VTI", "IEF")])
# Logarithmic utility of stock and bond portfolio
utilfun <- function(stocku, bondu) {
  -sum(log(1 + stocku*retp$VTI + bondu*retp$IEF))
}  # end utilfun
# Create matrix of utility values
stocku <- seq(from=3, to=7, by=0.2)
bondu <- seq(from=12, to=20, by=0.2)
utilm <- sapply(bondu, function(y) sapply(stocku,
  function(x) utilfun(x, y)))
# Set rgl options and load package rgl
options(rgl.useNULL=TRUE)
library(rgl)
# Draw 3d surface plot of utility
rgl::persp3d(stocku, bondu, utilm, col="green",
  xlab="stocks", ylab="bonds", zlab="utility")
# Render the surface plot
rgl::rglwidget(elementId="plot3drgl")
# Save the surface plot to png file
rgl::rgl.snapshot("utility_surface.png")

# Approximate Kelly weights
weightv <- sapply(retp, function(x) mean(x)/var(x))
# Kelly weight for stocks
unlist(optimize(f=function(x) utilfun(x, bondu=0), interval=c(1, 4)))
# Kelly weight for bonds
unlist(optimize(f=function(x) utilfun(x, stocku=0), interval=c(1, 14)))
# Vectorized utility of stock and bond portfolio
utility_vec <- function(weightv) {
  utilfun(weightv[1], weightv[2])
}  # end utility_vec
# Optimize with respect to vector argument
optiml <- optim(fn=utility_vec, par=c(3, 10),
          method="L-BFGS-B",
          upper=c(8, 20), lower=c(2, 5))
# Exact Kelly weights
optiml$par

# Approximate Kelly weights
retsport <- (retp %*% weightv)
drop(mean(retsport)/var(retsport))*weightv
# Exact Kelly weights
optiml$par

# Quarter-Kelly sub-optimal weights
weightv <- optiml$par/4
# Plot Kelly optimal portfolio
retp <- cbind(retp, weightv[1]*retp$VTI + weightv[2]*retp$IEF)
colnames(retp)[3] <- "Kelly_sub"
# Calculate the compounded wealth from returns
wealthv <- cumprod(1 + retp)
# Plot compounded wealth
dygraphs::dygraph(log(wealthv), main="Stock and Bond Portfolio") %>%
  dyOptions(colors=c("green", "blue", "green")) %>%
  dySeries("Kelly_sub", color="red", strokeWidth=2) %>%
  dyLegend(show="always", width=300)

retp <- na.omit(rutils::etfenv$returns[, c("VTI", "IEF")])
# Calculate the rolling returns and variance
lookb <- 200
var_rolling <- HighFreq::roll_var(retp, lookb)
weightv <- HighFreq::roll_sum(retp, lookb)/lookb
weightv <- weightv/var_rolling
weightv[1, ] <- 1/NCOL(weightv)
weightv <- zoo::na.locf(weightv)
sum(is.na(weightv))
range(weightv)

# Plot the weights
plot(density(retp$IEF), t="l", lwd=3, col="red",
     xlab="weights", ylab="density",
     ylim=c(0, max(density(retp$VTI)$y)),
     main="Kelly Weight Distributions")
lines(density(retp$VTI), t="l", col="blue", lwd=3)
legend("topright", legend=c("VTI", "IEF"),
 inset=0.1, bg="white", lty=1, lwd=6, y.intersp=0.5,
 col=c("blue", "red"), bty="n")

# Scale and lag the Kelly weights
weightv <- lapply(weightv, function(x) 10*x/sum(abs(range(x))))
weightv <- do.call(cbind, weightv)
weightv <- rutils::lagit(weightv)
# Calculate the compounded Kelly wealth and VTI
wealthv <- cbind(cumprod(1 + weightv$VTI*retp$VTI), cumprod(1 + retp$VTI))
colnames(wealthv) <- c("Kelly Strategy", "VTI")
dygraphs::dygraph(wealthv, main="VTI Strategy Using Rolling Kelly Weight") %>%
  dyAxis("y", label="Kelly Strategy", independentTicks=TRUE) %>%
  dyAxis("y2", label="VTI", independentTicks=TRUE) %>%
  dySeries(name="Kelly Strategy", axis="y", strokeWidth=1, col="red") %>%
  dySeries(name="VTI", axis="y2", strokeWidth=1, col="blue")

# bidask equal to 1 bp for liquid ETFs
bidask <- 0.001
# Calculate the compounded Kelly wealth and margin
wealthv <- cumprod(1 + weightv$VTI*retp$VTI)
marginv <- (retp$VTI - 1)*wealthv + 1
# Calculate the transaction costs
costs <- bidask*drop(rutils::diffit(marginv))/2
wealth_diff <- drop(rutils::diffit(wealthv))
costs_rel <- ifelse(wealth_diff>0, costs/wealth_diff, 0)
range(costs_rel)
hist(costs_rel, breaks=10000, xlim=c(-0.02, 0.02))
# Scale and lag the transaction costs
costs <- rutils::lagit(abs(costs)/wealthv)
# ReCalculate the compounded Kelly wealth
wealth_trans <- cumprod(1 + retp$VTI*retp$VTI - costs)
# Plot compounded wealth
wealthv <- cbind(wealthv, wealth_trans)
colnames(wealthv) <- c("Kelly", "Including bid-ask")
dygraphs::dygraph(wealthv, main="Kelly Strategy With Transaction Costs") %>%
  dyOptions(colors=c("green", "blue"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Calculate the compounded wealth from returns
wealthv <- cumprod(1 + rowSums(weightv*retp))
wealthv <- xts::xts(wealthv, zoo::index(retp))
quantmod::chart_Series(wealthv, name="Rolling Kelly Strategy For VTI and IEF")
# Calculate the compounded Kelly wealth and VTI
wealthv <- cbind(wealthv, cumprod(1 + 0.6*retp$IEF + 0.4*retp$VTI))
colnames(wealthv) <- c("Kelly Strategy", "VTI plus IEF")
dygraphs::dygraph(wealthv, main="Rolling Kelly Strategy For VTI and IEF") %>%
  dyAxis("y", label="Kelly Strategy", independentTicks=TRUE) %>%
  dyAxis("y2", label="VTI plus IEF", independentTicks=TRUE) %>%
  dySeries(name="Kelly Strategy", axis="y", strokeWidth=1, col="red") %>%
  dySeries(name="VTI plus IEF", axis="y2", strokeWidth=1, col="blue")

# Perform regression using formula
retp <- na.omit(rutils::etfenv$returns[, c("XLP", "VTI")])
raterf <- 0.03/252
retp <- (retp - raterf)
regmod <- lm(XLP ~ VTI, data=retp)
regsum <- summary(regmod)
# Get regression coefficients
coef(regsum)
# Get alpha and beta
coef(regsum)[, 1]
# Plot scatterplot of returns with aspect ratio 1
plot(XLP ~ VTI, data=rutils::etfenv$returns, main="Regression XLP ~ VTI",
     xlim=c(-0.1, 0.1), ylim=c(-0.1, 0.1), pch=1, col="blue", asp=1)
# Add regression line and perpendicular line
abline(regmod, lwd=2, col="red")
abline(a=0, b=-1/coef(regsum)[2, 1], lwd=2, col="blue")

library(PerformanceAnalytics)
# Calculate XLP beta
PerformanceAnalytics::CAPM.beta(Ra=retp$XLP, Rb=retp$VTI)
# Or
betac <- drop(cov(retp$XLP, retp$VTI)/var(retp$VTI))
betac
# Calculate XLP alpha
PerformanceAnalytics::CAPM.alpha(Ra=retp$XLP, Rb=retp$VTI)
# Or
alphac <- mean(retp$XLP - betac*retp$VTI)
# Calculate XLP bull beta
PerformanceAnalytics::CAPM.beta.bull(Ra=retp$XLP, Rb=retp$VTI)
# Calculate XLP bear beta
PerformanceAnalytics::CAPM.beta.bear(Ra=retp$XLP, Rb=retp$VTI)

# Plot the CML
rets <- raterf + betac*mean(retp$VTI)
stdev <- sd(retp$XLP)
plot(x=c(0, stdev), y=c(raterf, rets), col="red", lwd=2,
     xlim=c(0, 1.2*stdev), ylim=c(0, 1.2*rets),
     main="Capital Market Line",
     xlab="standard deviation", ylab="return")
text(x=0.0, y=raterf, labels="risk free", pos=4, cex=1.2)
text(x=stdev, y=rets, labels="stock", pos=1, cex=1.2)
abline(a=raterf, b=betac*mean(retp$VTI)/stdev, lwd=2, col="blue")

# Get regression coefficients
coef(regsum)
# Calculate regression coefficients from scratch
betac <- drop(cov(retp$XLP, retp$VTI)/var(retp$VTI))
alphac <- drop(mean(retp$XLP) - betac*mean(retp$VTI))
c(alphac, betac)
# Calculate the residuals
residuals <- (retp$XLP - (alphac + betac*retp$VTI))
# Calculate the standard deviation of residuals
nrows <- NROW(residuals)
residsd <- sqrt(sum(residuals^2)/(nrows - 2))
# Calculate the standard errors of beta and alpha
sum2 <- sum((retp$VTI - mean(retp$VTI))^2)
betasd <- residsd/sqrt(sum2)
alphasd <- residsd*sqrt(1/nrows + mean(retp$VTI)^2/sum2)
c(alphasd, betasd)
# Perform the Durbin-Watson test of autocorrelation of residuals
lmtest::dwtest(regmod)

retm <- rutils::etfenv$returns
symbolv <- colnames(retm)
symbolv <- symbolv[symbolv != "VTI"]
Perform regressions and collect statistics
betam <- sapply(symbolv, function(symbol) {
Specify regression formula
  formulav <- as.formula(paste(symbol, "~ VTI"))
Perform regression
  regmod <- lm(formulav, data=retm)
Get regression summary
  regsum <- summary(regmod)
Collect regression statistics
  with(regsum, 
    c(beta=coefficients[2, 1], 
betap=coefficients[2, 4],
alpha=coefficients[1, 1], 
alphap=coefficients[1, 4], 
dwp=lmtest::dwtest(regmod)$p.value))
})  # end sapply
betam <- t(betam)
Sort by alphap
betam <- betam[order(betam[, "alphap"]), ]

betam

symbolv <- rownames(betam)
betac <- betam[-match(c("VXX", "SVXY", "MTUM", "USMV", "QUAL"), symbolv), 1]
betac <- c(1, betac)
names(betac)[1] <- "VTI"
retsann <- sapply(retp[, names(betac)], PerformanceAnalytics::Return.annualized)
# Plot scatterplot of returns vs betas
minrets <- min(retsann)
plot(retsann ~ betac, xlab="betas", ylab="returns",
     ylim=c(minrets, -minrets), main="Security Market Line for ETFs")
retvti <- retsann["VTI"]
points(x=1, y=retvti, col="red", lwd=3, pch=21)
# Plot Security Market Line
raterf <- 0.01
abline(a=raterf, b=(retvti-raterf), col="green", lwd=2)

# Add labels
text(x=betac, y=retsann, labels=names(betac), pos=2, cex=0.8)
# Find optimal risk-free rate by minimizing residuals
rss <- function(raterf) {
  sum((retsann - raterf - betac*(retvti-raterf))^2)
}  # end rss
optimrss <- optimize(rss, c(-1, 1))
raterf <- optimrss$minimum
# Or simply
retsadj <- (retsann - retvti*betac)
betadj <- (1-betac)
raterf <- sum(retsadj*betadj)/sum(betadj^2)
abline(a=raterf, b=(retvti-raterf), col="blue", lwd=2)
legend(x="topleft", bty="n", title="Security Market Line",
 legend=c("optimal fit", "raterf=0.01"),
 y.intersp=0.5, cex=1.0, lwd=6, lty=1, col=c("blue", "green"))

# Load S&P500 constituent stock returns
load("/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
retvti <- na.omit(rutils::etfenv$returns$VTI)
retp <- retstock[index(retvti), ]
nrows <- NROW(retp)
# Calculate stock betas
betac <- sapply(retp, function(x) {
  retp <- na.omit(cbind(x, retvti))
  drop(cov(retp[, 1], retp[, 2])/var(retp[, 2]))
})  # end sapply
mean(betac)
# Calculate annual stock returns
retsann <- retp
retsann[1, ] <- 0
retsann <- zoo::na.locf(retsann, na.rm=FALSE)
retsann <- 252*sapply(retsann, sum)/nrows
# Remove stocks with zero returns
sum(retsann == 0)
betac <- betac[retsann > 0]
retsann <- retsann[retsann > 0]
retvti <- 252*mean(retvti)
# Plot scatterplot of returns vs betas
plot(retsann ~ betac, xlab="betas", ylab="returns",
     main="Security Market Line for Stocks")
points(x=1, y=retvti, col="red", lwd=3, pch=21)
# Plot Security Market Line
raterf <- 0.01
abline(a=raterf, b=(retvti-raterf), col="green", lwd=2)

# Find optimal risk-free rate by minimizing residuals
retsadj <- (retsann - retvti*betac)
betadj <- (1-betac)
raterf <- sum(retsadj*betadj)/sum(betadj^2)
abline(a=raterf, b=(retvti-raterf), col="blue", lwd=2)
legend(x="topleft", bty="n", title="Security Market Line",
 legend=c("optimal fit", "raterf=0.01"),
 y.intersp=0.5, cex=1.0, lwd=6, lty=1, col=c("blue", "green"))

retp <- na.omit(rutils::etfenv$returns[, c("XLP", "VTI")])
library(PerformanceAnalytics)
Calculate XLP Treynor ratio
TreynorRatio(Ra=retp$XLP, Rb=retp$VTI)
Calculate XLP Information ratio
InformationRatio(Ra=retp$XLP, Rb=retp$VTI)

PerformanceAnalytics::table.CAPM(Ra=retm[, c("XLP", "XLF")], 
                           Rb=retm$VTI, scale=252)

capmstats <- table.CAPM(Ra=retm[, symbolv], Rb=retm$VTI, scale=252)
colv <- strsplit(colnames(capmstats), split=" ")
colv <- do.call(cbind, colv)[1, ]
colnames(capmstats) <- colv
capmstats <- t(capmstats)
capmstats <- capmstats[, -1]
colv <- colnames(capmstats)
whichv <- match(c("Annualized Alpha", "Information Ratio", "Treynor Ratio"), colv)
colv[whichv] <- c("Alpha", "Information", "Treynor")
colnames(capmstats) <- colv
capmstats <- capmstats[order(capmstats[, "Alpha"], decreasing=TRUE), ]
# Copy capmstats into etfenv and save to .RData file
etfenv <- rutils::etfenv
etfenv$capmstats <- capmstats
save(etfenv, file="/Users/jerzy/Develop/lecture_slides/data/etf_data.RData")

rutils::etfenv$capmstats[, c("Beta", "Alpha", "Information", "Treynor")]

# Calculate XLP and VTI returns
retp <- na.omit(rutils::etfenv$returns[, c("XLP", "VTI")])
# Calculate monthly end points
endd <- rutils::calc_endpoints(retp, interval="months")[-1]
# Calculate start points from look-back interval
lookb <- 12  # Look back 12 months
startp <- c(rep(1, lookb), endd[1:(NROW(endd)-lookb)])
head(cbind(endd, startp), lookb+2)
# Calculate trailing beta regressions every month in R
formulav <- XLP ~ VTI  # Specify regression formula
betar <- sapply(1:NROW(endd), FUN=function(tday) {
    datav <- retp[startp[tday]:endd[tday], ]
    # coef(lm(formulav, data=datav))[2]
    drop(cov(datav$XLP, datav$VTI)/var(datav$VTI))
})  # end sapply
# Calculate trailing betas using RcppArmadillo
controll <- HighFreq::param_reg()
reg_stats <- HighFreq::roll_reg(respv=retp$XLP, predm=retp$VTI,
  startp=(startp-1), endd=(endd-1), controll=controll)
betac <- reg_stats[, 1]
all.equal(betac, betar)
# Compare the speed of RcppArmadillo with R code
library(microbenchmark)
summary(microbenchmark(
  Rcpp=HighFreq::roll_reg(respv=retp$XLP, predm=retp$VTI, startp=(startp-1), endd=(endd-1), controll=controll),
  Rcode=sapply(1:NROW(endd), FUN=function(tday) {
    datav <- retp[startp[tday]:endd[tday], ]
    drop(cov(datav$XLP, datav$VTI)/var(datav$VTI))
  }),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# dygraph plot of trailing XLP beta and VTI prices
datev <- zoo::index(retp[endd, ])
pricev <- log(rutils::etfenv$prices$VTI[datev])
datav <- cbind(pricev, betac)
colnames(datav)[2] <- "beta"
colv <- colnames(datav)
dygraphs::dygraph(datav, main="XLP Trailing 12-month Beta and VTI Prices") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=colv[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Calculate the trailing betas
lambdaf <- 0.99
covarv <- HighFreq::run_covar(retp, lambdaf)
betac <- covarv[, 1]/covarv[, 3]

# dygraph plot of trailing XLP beta and VTI prices
datav <- cbind(pricev, betac[endd])[-(1:11)] # Remove warmup period
colnames(datav)[2] <- "beta"
colv <- colnames(datav)
dygraphs::dygraph(datav, main="XLP Trailing EMA Beta and VTI Prices") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=colv[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Verify that Rtools or XCode are working properly:
devtools::find_rtools()  # Under Windows
devtools::has_devel()
# Install the packages Rcpp and RcppArmadillo
install.packages(c("Rcpp", "RcppArmadillo"))
# Load package Rcpp
library(Rcpp)
# Get documentation for package Rcpp
# Get short description
packageDescription("Rcpp")
# Load help page
help(package="Rcpp")
# List all datasets in "Rcpp"
data(package="Rcpp")
# List all objects in "Rcpp"
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
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/mult_rcpp.cpp")
# Multiply two numbers
mult_rcpp(2, 3)
mult_rcpp(1:3, 6:4)
# Multiply two vectors
mult_vec_rcpp(2, 3)
mult_vec_rcpp(1:3, 6:4)

# Define Rcpp function with loop
Rcpp::cppFunction("
double inner_mult(NumericVector x, NumericVector y) {
int xsize = x.size();
int ysize = y.size();
if (xsize != ysize) {
    return 0;
  } else {
    double total = 0;
    for(int i = 0; i < xsize; ++i) {
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
double inner_sugar(NumericVector x, NumericVector y) {
  return sum(x * y);
}")  # end cppFunction
# Run Rcpp Sugar function
inner_sugar(1:3, 6:4)
inner_sugar(1:3, 6:3)

# Define R function with loop
inner_multr <- function(x, y) {
    sumv <- 0
    for(i in 1:NROW(x)) {
sumv <- sumv + x[i] * y[i]
    }
    sumv
}  # end inner_multr
# Run R function
inner_multr(1:3, 6:4)
inner_multr(1:3, 6:3)
# Compare speed of Rcpp and R
library(microbenchmark)
summary(microbenchmark(
  rcode=inner_multr(1:10000, 1:10000),
  innerp=1:10000 %*% 1:10000,
  Rcpp=inner_mult(1:10000, 1:10000),
  sugar=inner_sugar(1:10000, 1:10000),
  times=10))[, c(1, 4, 5)]

# Define Ornstein-Uhlenbeck function in R
sim_our <- function(nrows=1000, priceq=5.0,
              volat=0.01, theta=0.01) {
  retp <- numeric(nrows)
  pricev <- numeric(nrows)
  pricev[1] <- priceq
  for (i in 2:nrows) {
    retp[i] <- theta*(priceq - pricev[i-1]) + volat*rnorm(1)
    pricev[i] <- pricev[i-1] + retp[i]
  }  # end for
  pricev
}  # end sim_our
# Simulate Ornstein-Uhlenbeck process in R
priceq <- 5.0; sigmav <- 0.01
thetav <- 0.01; nrows <- 1000
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")  # Reset random numbers
ousim <- sim_our(nrows, priceq=priceq, volat=sigmav, theta=thetav)

# Define Ornstein-Uhlenbeck function in Rcpp
Rcpp::cppFunction("
NumericVector sim_oucpp(double priceq,
                  double volat,
                  double thetav,
                  NumericVector innov) {
  int nrows = innov.size();
  NumericVector pricev(nrows);
  NumericVector retv(nrows);
  pricev[0] = priceq;
  for (int it = 1; it < nrows; it++) {
    retv[it] = thetav*(priceq - pricev[it-1]) + volat*innov[it-1];
    pricev[it] = pricev[it-1] + retv[it];
  }  // end for
  return pricev;
}")  # end cppFunction
# Simulate Ornstein-Uhlenbeck process in Rcpp
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")  # Reset random numbers
oucpp <- sim_oucpp(priceq=priceq,
  volat=sigmav, theta=thetav, innov=rnorm(nrows))
all.equal(ousim, oucpp)
# Compare speed of Rcpp and R
library(microbenchmark)
summary(microbenchmark(
  rcode=sim_our(nrows, priceq=priceq, volat=sigmav, theta=thetav),
  Rcpp=sim_oucpp(priceq=priceq, volat=sigmav, theta=thetav, innov=rnorm(nrows)),
  times=10))[, c(1, 4, 5)]

# Source Rcpp function for Ornstein-Uhlenbeck process from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/sim_ou.cpp")
# Simulate Ornstein-Uhlenbeck process in Rcpp
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")  # Reset random numbers
oucpp <- sim_oucpp(priceq=priceq,
  volat=sigmav,
  theta=thetav,
  innov=rnorm(nrows))
all.equal(ousim, oucpp)
# Compare speed of Rcpp and R
library(microbenchmark)
summary(microbenchmark(
  rcode=sim_our(nrows, priceq=priceq, volat=sigmav, theta=thetav),
  Rcpp=sim_oucpp(priceq=priceq, volat=sigmav, theta=thetav, innov=rnorm(nrows)),
  times=10))[, c(1, 4, 5)]

# Calculate uniformly distributed pseudo-random sequence
unifun <- function(seedv, nrows=10) {
  datav <- numeric(nrows)
  datav[1] <- seedv
  for (i in 2:nrows) {
    datav[i] <- 4*datav[i-1]*(1-datav[i-1])
  }  # end for
  acos(1-2*datav)/pi
}  # end unifun

# Source Rcpp functions from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/unifun.cpp")
# Microbenchmark Rcpp code
library(microbenchmark)
summary(microbenchmark(
  rcode=runif(1e5),
  rloop=unifun(0.3, 1e5),
  Rcpp=unifuncpp(0.3, 1e5),
  times=10))[, c(1, 4, 5)]

library(RcppArmadillo)
# Source Rcpp functions from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/armadillo_functions.cpp")
vec1 <- runif(1e5)
vec2 <- runif(1e5)
inner_vec(vec1, vec2)
vec1 %*% vec2

# Microbenchmark \emph{RcppArmadillo} code
summary(microbenchmark(
  rcpp = inner_vec(vec1, vec2),
  rcode = (vec1 %*% vec2),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
# Microbenchmark shows:
# inner_vec() is several times faster than %*%, especially for longer vectors.
#     expr     mean   median
# 1 inner_vec 110.7067 110.4530
# 2 rcode 585.5127 591.3575

# Source Rcpp functions from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/sim_arima.cpp")
# Define AR(2) coefficients
coeff <- c(0.9, 0.09)
nrows <- 1e4
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
innov <- rnorm(nrows)
# Simulate ARIMA using filter()
arimar <- filter(x=innov, filter=coeff, method="recursive")
# Simulate ARIMA using sim_ar()
innov <- matrix(innov)
coeff <- matrix(coeff)
arimav <- sim_ar(coeff, innov)
all.equal(drop(arimav), as.numeric(arimar))
# Microbenchmark \emph{RcppArmadillo} code
summary(microbenchmark(
  rcpp = sim_ar(coeff, innov),
  filter = filter(x=innov, filter=coeff, method="recursive"),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary

library(RcppArmadillo)
# Source Rcpp functions from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/armadillo_functions.cpp")
matv <- matrix(runif(1e5), nc=1e3)
# Center matrix columns using apply()
matd <- apply(matv, 2, function(x) (x-mean(x)))
# Center matrix columns in place using Rcpp demeanr()
demeanr(matv)
all.equal(matd, matv)
# Microbenchmark \emph{RcppArmadillo} code
library(microbenchmark)
summary(microbenchmark(
  rcode = (apply(matv, 2, mean)),
  rcpp = demeanr(matv),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
# Perform matrix inversion
# Create random positive semi-definite matrix
matv <- matrix(runif(25), nc=5)
matv <- t(matv) %*% matv
# Invert the matrix
matrixinv <- solve(matv)
inv_mat(matv)
all.equal(matrixinv, matv)
# Microbenchmark \emph{RcppArmadillo} code
summary(microbenchmark(
  rcode = solve(matv),
  rcpp = inv_mat(matv),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary

library(RcppArmadillo)
# Source Rcpp functions from file
Rcpp::sourceCpp("/Users/jerzy/Develop/lecture_slides/scripts/HighFreq.cpp")
# Calculate matrix of random returns
matv <- matrix(rnorm(300), nc=5)
# Reduced inverse of correlation matrix
dimax <- 4
cormat <- cor(matv)
eigend <- eigen(cormat)
invmat <- eigend$vectors[, 1:dimax] %*%
  (t(eigend$vectors[, 1:dimax]) / eigend$values[1:dimax])
# Reduced inverse using \emph{RcppArmadillo}
invarma <- calc_inv(cormat, dimax=dimax)
all.equal(invmat, invarma)
# Microbenchmark \emph{RcppArmadillo} code
library(microbenchmark)
summary(microbenchmark(
  rcode = {eigend <- eigen(cormat)
eigend$vectors[, 1:dimax] %*% (t(eigend$vectors[, 1:dimax]) / eigend$values[1:dimax])},
  rcpp = calc_inv(cormat, dimax=dimax),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary

# Install package reticulate
install.packages("reticulate")
# Start Python session
reticulate::repl_python()
# Exit Python session
exit
