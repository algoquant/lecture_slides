library(knitr)
opts_chunk$set(prompt=TRUE, eval=FALSE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(digits=3)
options(width=60, dev='pdf')
thm <- knit_theme$get("acid")
knit_theme$set(thm)
library(HighFreq)
# specify formula and perform regression
for_mula <- XLP ~ VTI
mod_el <- lm(for_mula, 
          data=rutils::etf_env$re_turns)
# get regression coefficients
coef(summary(mod_el))
# Durbin-Watson test of autocorrelation of residuals
lmtest::dwtest(mod_el)
# plot scatterplot of returns with aspect ratio 1
plot(for_mula, data=rutils::etf_env$re_turns,
     xlim=c(-0.1, 0.1), ylim=c(-0.1, 0.1),
     asp=1, main="Regression XLP ~ VTI")
# add regression line and perpendicular line
abline(mod_el, lwd=2, col="red")
abline(a=0, b=-1/coef(summary(mod_el))[2, 1],
 lwd=2, col="blue")
library(HighFreq)  # load HighFreq
re_turns <- na.omit(rutils::etf_env$re_turns)
# perform regressions and collect statistics
etf_reg_stats <- sapply(colnames(re_turns)[-1],
                  function(etf_name) {
# specify regression formula
  for_mula <- as.formula(
    paste(etf_name, "~ VTI"))
# perform regression
  mod_el <- lm(for_mula, data=re_turns)
# get regression summary
  model_sum <- summary(mod_el)
# collect regression statistics
  etf_reg_stats <- with(model_sum,
    c(alpha=coefficients[1, 1],
p_alpha=coefficients[1, 4],
beta=coefficients[2, 1],
p_beta=coefficients[2, 4]))
  etf_reg_stats <- c(etf_reg_stats,
         p_dw=lmtest::dwtest(mod_el)$p.value)
  etf_reg_stats
})  # end sapply
etf_reg_stats <- t(etf_reg_stats)
# sort by p_alpha
etf_reg_stats <- etf_reg_stats[
  order(etf_reg_stats[, "p_alpha"]), ]
etf_reg_stats[, 1:3]
library(HighFreq)
# specify regression formula
for_mula <- XLP ~ VTI
# perform rolling beta regressions every month
beta_s <- rollapply(rutils::etf_env$re_turns, width=252,
  FUN=function(de_sign)
  coef(lm(for_mula, data=de_sign))[2],
  by=22, by.column=FALSE, align="right")
beta_s <- na.omit(beta_s)
# plot beta_s in x11() window
x11(width=(wid_th <- 6), height=(hei_ght <- 4))
chart_Series(x=beta_s[, "VTI"],
  name=paste("rolling betas", format(for_mula)))
# perform daily rolling beta regressions in parallel
library(roll)
beta_s <- roll_lm(x=rutils::etf_env$re_turns[, "VTI"],
            y=rutils::etf_env$re_turns[, "XLP"],
            width=252)$coefficients
# compare speed of rollapply() versus roll_lm()
library(microbenchmark)
da_ta <- rutils::etf_env$re_turns["2012", c("VTI", "XLP")]
summary(microbenchmark(
  rollapply=rollapply(da_ta, width=22,
FUN=function(de_sign)
coef(lm(for_mula, data=de_sign))[2],
  by.column=FALSE, align="right"),
  roll_lm=roll_lm(x=da_ta[, "VTI"],
            y=da_ta[, "XLP"],
            width=22)$coefficients,
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
library(PerformanceAnalytics)
CAPM.beta(Ra=re_turns[, "XLP"],
    Rb=re_turns[, "VTI"])
CAPM.beta.bull(Ra=re_turns[, "XLP"],
  Rb=re_turns[, "VTI"])
CAPM.beta.bear(Ra=re_turns[, "XLP"],
  Rb=re_turns[, "VTI"])
CAPM.alpha(Ra=re_turns[, "XLP"],
     Rb=re_turns[, "VTI"])
library(PerformanceAnalytics)
etf_betas <- sapply(
  re_turns[, colnames(re_turns)!="VXX"],
  CAPM.beta, Rb=re_turns[, "VTI"])
etf_annrets <- sapply(
  re_turns[, colnames(re_turns)!="VXX"],
  Return.annualized)
# plot scatterplot
plot(etf_annrets ~ etf_betas, xlab="betas",
      ylab="ann. rets", xlim=c(-0.25, 1.6))
points(x=1, y=etf_annrets["VTI"], col="red",
 lwd=3, pch=21)
abline(a=0, b=etf_annrets["VTI"])
label_names <- rownames(etf_reg_stats)[1:13]
# add labels
text(x=1, y=etf_annrets["VTI"], labels="VTI",
     pos=2)
text(x=etf_betas[label_names],
     y=etf_annrets[label_names],
     labels=label_names, pos=2, cex=0.8)
library(PerformanceAnalytics)
TreynorRatio(Ra=re_turns[, "XLP"],
     Rb=re_turns[, "VTI"])

InformationRatio(Ra=re_turns[, "XLP"],
     Rb=re_turns[, "VTI"])
library(PerformanceAnalytics)
table.CAPM(Ra=re_turns[, c("XLP", "XLF")],
     Rb=re_turns[, "VTI"], scale=252)
library(PerformanceAnalytics)
capm_stats <- PerformanceAnalytics::table.CAPM(Ra=re_turns[, colnames(re_turns)!="VTI"],
        Rb=re_turns[, "VTI"], scale=252)
colnames(capm_stats) <-
  sapply(colnames(capm_stats),
  function(str) {strsplit(str, split=" ")[[1]][1]})
capm_stats <- as.matrix(capm_stats)
capm_stats <- t(capm_stats)
capm_stats <- capm_stats[
  order(capm_stats[, "Annualized Alpha"],
  decreasing=TRUE), ]
# copy capm_stats into etf_env and save to .RData file
assign("capm_stats", capm_stats, envir=etf_env)
save(etf_env, file='etf_data.RData')
capm_stats[, c("Information Ratio", "Annualized Alpha")]
re_turns <-
  scale(na.omit(rutils::etf_env$re_turns
          [, as.character(for_mula)[-1]]))
crossprod(re_turns) / NROW(re_turns)
w_1 <- sqrt(0.5); w_2 <- w_1
foo <- matrix(c(w_1, w_2, -w_2, w_1), nc=2)
t(foo) %*% foo
# bar <- re_turns %*% t(solve(foo))
(t(bar) %*% bar) / NROW(bar)

cov_mat <- function(re_turns, an_gle=0) {
  w_1 <- cos(an_gle)
  w_2 <- sin(an_gle)
  mat_rix <- matrix(c(w_1, -w_2, w_2, w_1), nc=2)
  compo_nents <- re_turns %*% t(mat_rix)
  (t(compo_nents) %*% compo_nents) / NROW(compo_nents)
}  # end cov_mat

bar <- cov_mat(re_turns, an_gle=pi/4)
crossprod(re_turns) / NROW(re_turns)
(t(bar) %*% bar) / NROW(bar)

angle_s <- seq(0, pi/2, by=pi/24)
cov_mat <- sapply(angle_s, function(an_gle)
  cov_mat(re_turns, an_gle=an_gle)[1, 1])
plot(x=angle_s, y=cov_mat, t="l")

op_tim <- optimize(
  f=function(an_gle)
    -cov_mat(re_turns, an_gle=an_gle)[1, 1],
  interval=range(angle_s))
an_gle <- op_tim$minimum
bar <- cov_mat(re_turns, an_gle=an_gle)
tan(an_gle)

w_1 <- cos(an_gle)
w_2 <- sin(an_gle)
mat_rix <- matrix(c(w_1, -w_2, w_2, w_1), nc=2)
compo_nents <- re_turns %*% t(mat_rix)
(t(compo_nents) %*% compo_nents) / NROW(compo_nents)

plot(x=compo_nents[, 1], y=compo_nents[, 2],
     xlim=c(-10, 10), ylim=c(-10, 10))

mod_el <- lm(for_mula, data=re_turns)
# get regression coefficients
coef(summary(mod_el))

foo <- cbind(rnorm(1000, sd=0.2), rnorm(1000)) %*% t(mat_rix)
(t(foo) %*% foo) / NROW(foo)
plot(x=foo[, 1], y=foo[, 2])
summary(lm(foo[, 1] ~ foo[, 2]))

op_tim <- optimize(
  f=function(an_gle)
    -cov_mat(foo, an_gle=an_gle)[1, 1],
  interval=range(angle_s))
an_gle <- op_tim$minimum
tan(an_gle)

###

w_1 <- cos(0.5)
s_1 <- 1
s_2 <- 2
foo <- matrix(c(s_1^2, s_1*s_2*w_1, s_1*s_2*w_1, s_2^2), nc=2)
eigen(foo)

# Durbin-Watson test of autocorrelation of residuals
lmtest::dwtest(mod_el)
# plot scatterplot of returns
plot(for_mula, data=rutils::etf_env$re_turns,
     main="Regression XLP ~ VTI")
# add regression line
abline(mod_el, lwd=2, col="red")
library(HighFreq)
# Select ETF symbols
sym_bols <- c("IEF", "DBC", "XLU", "XLF", "XLP", "XLI")
# calculate ETF prices and simple returns (not percentage)
price_s <- rutils::etf_env$price_s[, sym_bols]
price_s <- xts:::na.locf.xts(price_s)
price_s <- xts:::na.locf.xts(price_s, fromLast=TRUE)
date_s <- index(price_s)
re_turns <- rutils::diff_it(price_s)
# de-mean (center) and scale the returns
re_turns <- t(t(re_turns) - colMeans(re_turns))
re_turns <- t(t(re_turns) / sqrt(colSums(re_turns^2)/(NROW(re_turns)-1)))
re_turns <- xts(re_turns, date_s)
# alternative de-mean (center) and scale the returns
# re_turns <- scale(re_turns, center=TRUE, scale=TRUE)
# re_turns <- xts(re_turns, date_s)
# or
# re_turns <- lapply(re_turns, function(x) {x - sum(x)/NROW(re_turns)})
# re_turns <- rutils::do_call(cbind, re_turns)
# re_turns <- apply(re_turns, 2, scale)
# covariance matrix and variance vector of returns
cov_mat <- cov(re_turns)
vari_ance <- diag(cov_mat)
cor_mat <- cor(re_turns)
# cov_mat <- crossprod(re_turns) / (NROW(re_turns)-1)
# cor_mat <- cov_mat / sqrt(vari_ance)
# cor_mat <- t(t(cor_mat) / sqrt(vari_ance))
# reorder correlation matrix based on clusters
library(corrplot)
or_der <- corrMatOrder(cor_mat,
        order="hclust",
        hclust.method="complete")
cor_mat <- cor_mat[or_der, or_der]
# plot the correlation matrix
col_ors <- colorRampPalette(c("red", "white", "blue"))
corrplot(cor_mat, title="ETF Correlation Matrix",
    tl.col="black", tl.cex=0.8, mar=c(0,0,1,0),
    method="square", col=col_ors(8),
    cl.offset=0.75, cl.cex=0.7,
    cl.align.text="l", cl.ratio=0.25)
# draw rectangles on the correlation matrix plot
corrRect.hclust(cor_mat, k=NROW(cor_mat) %/% 2,
          method="complete", col="red")
# plot the correlation matrix
col_ors <- colorRampPalette(c("red", "white", "blue"))
corrplot(cor_mat, title="Correlation Matrix",
    tl.col="black", tl.cex=0.8, mar = c(0,0,1,0),
    method="square", col=col_ors(NCOL(cor_mat)),
    cl.offset=0.75, cl.cex=0.7,
    cl.align.text="l", cl.ratio=0.25)
# draw rectangles on the correlation matrix plot
corrRect.hclust(cor_mat, k=NCOL(cor_mat) %/% 2,
    method="complete", col="red")
# create initial vector of portfolio weights
n_weights <- NROW(sym_bols)
weight_s <- rep(1/sqrt(n_weights), n_weights)
names(weight_s) <- sym_bols
# objective function equal to minus portfolio variance
object_ive <- function(weight_s, re_turns) {
  portf_rets <- re_turns %*% weight_s
  -sum(portf_rets^2) +
    1e7*(1 - sum(weight_s^2))^2
}  # end object_ive
# objective for equal weight portfolio
object_ive(weight_s, re_turns)
# compare speed of vector multiplication methods
summary(microbenchmark(
  trans_pose=(t(re_turns[, 1]) %*% re_turns[, 1]),
  s_um=sum(re_turns[, 1]^2),
  times=10))[, c(1, 4, 5)]
# find weights with maximum variance
optim_run <- optim(par=weight_s,
  fn=object_ive,
  re_turns=re_turns,
  method="L-BFGS-B",
  upper=rep(10.0, n_weights),
  lower=rep(-10.0, n_weights))
# optimal weights and maximum variance
weight_s <- optim_run$par
-object_ive(weight_s, re_turns)
# plot first principal component weights
barplot(weight_s, names.arg=names(weight_s),
  xlab="", ylab="",
  main="First Principal Component Weights")
# pc1 weights and returns
weights_1 <- weight_s
pc_1 <- re_turns %*% weights_1
# redefine objective function
object_ive <- function(weight_s, re_turns) {
  portf_rets <- re_turns %*% weight_s
  -sum(portf_rets^2) +
    1e7*(1 - sum(weight_s^2))^2 +
    1e7*(sum(weights_1*weight_s))^2
}  # end object_ive
# find second PC weights using parallel DEoptim
optim_run <- DEoptim::DEoptim(fn=object_ive,
  upper=rep(10, NCOL(re_turns)),
  lower=rep(-10, NCOL(re_turns)),
  re_turns=re_turns, control=list(parVar="weights_1",
    trace=FALSE, itermax=1000, parallelType=1))
# pc2 weights and returns
weights_2 <- optim_run$optim$bestmem
names(weights_2) <- colnames(re_turns)
sum(weights_2^2)
sum(weights_1*weights_2)
# plot second principal component loadings
barplot(weights_2, names.arg=names(weights_2),
  xlab="", ylab="",
  main="Second Principal Component Loadings")
# calculate eigenvectors and eigenvalues
ei_gen <- eigen(cov_mat)
ei_gen$vectors
weights_1
weights_2
ei_gen$values[1]
var(pc_1)
(cov_mat %*% weights_1) / weights_1
ei_gen$values[2]
var(pc_2)
(cov_mat %*% weights_2) / weights_2
sum(vari_ance)
sum(ei_gen$values)
barplot(ei_gen$values, # plot eigenvalues
  names.arg=paste0("PC", 1:n_weights),
  las=3, xlab="", ylab="", main="Principal Component Variances")
# redefine objective function to minimize variance
object_ive <- function(weight_s, re_turns) {
  portf_rets <- re_turns %*% weight_s
  sum(portf_rets^2) +
    1e7*(1 - sum(weight_s^2))^2
}  # end object_ive
# find highest order PC weights using parallel DEoptim
optim_run <- DEoptim::DEoptim(fn=object_ive,
  upper=rep(10, NCOL(re_turns)),
  lower=rep(-10, NCOL(re_turns)),
  re_turns=re_turns, control=list(trace=FALSE,
    itermax=1000, parallelType=1))
# pc6 weights and returns
weights_6 <- optim_run$optim$bestmem
names(weights_6) <- colnames(re_turns)
sum(weights_6^2)
sum(weights_1*weights_6)
# calculate objective function
object_ive(weights_6, re_turns)
object_ive(ei_gen$vectors[, 6], re_turns)
# plot highest order principal component loadings
barplot(weights_6, names.arg=names(weights_2),
  xlab="", ylab="",
  main="Highest Order Principal Component Loadings")
# perform principal component analysis PCA
pc_a <- prcomp(re_turns, scale=TRUE)
# plot standard deviations of principal components
barplot(pc_a$sdev,
  names.arg=colnames(pc_a$rotation),
  las=3, xlab="", ylab="",
  main="Scree Plot: Volatilities of Principal Components
  of Stock Returns")
# principal component loadings (weights)
pc_a$rotation
# Plot barplots with PCA weights in multiple panels
par(mfrow=c(n_weights/2, 2))
par(mar=c(2, 2, 2, 1), oma=c(0, 0, 0, 0))
for (or_der in 1:n_weights) {
  barplot(pc_a$rotation[, or_der],
  las=3, xlab="", ylab="", main="")
  title(paste0("PC", or_der), line=-2.0,
  col.main="red")
}  # end for
# principal component time series
pca_rets <- xts(re_turns %*% pc_a$rotation,
          order.by=date_s)
round(cov(pca_rets), 3)
all.equal(coredata(pca_rets), pc_a$x, check.attributes=FALSE)
pca_ts <- xts:::cumsum.xts(pca_rets)
# plot principal component time series in multiple panels
par(mfrow=c(n_weights/2, 2))
par(mar=c(2, 2, 0, 1), oma=c(0, 0, 0, 0))
ra_nge <- range(pca_ts)
for (or_der in 1:n_weights) {
  plot.zoo(pca_ts[, or_der],
     ylim=ra_nge,
     xlab="", ylab="")
  title(paste0("PC", or_der), line=-2.0)
}  # end for
par(mfrow=c(n_weights/2, 2))
par(mar=c(2, 2, 0, 1), oma=c(0, 0, 0, 0))
# invert all the principal component time series
pca_rets <- re_turns %*% pc_a$rotation
sol_ved <- pca_rets %*% solve(pc_a$rotation)
all.equal(re_turns, sol_ved)
# invert first 3 principal component time series
sol_ved <- pca_rets[, 1:3] %*% solve(pc_a$rotation)[1:3, ]
sol_ved <- xts::xts(sol_ved, date_s)
sol_ved <- xts:::cumsum.xts(sol_ved)
cum_returns <- xts:::cumsum.xts(re_turns)
# plot the solved returns
for (sym_bol in sym_bols) {
  plot.zoo(
    cbind(cum_returns[, sym_bol], sol_ved[, sym_bol]),
    plot.type="single", col=c("black", "blue"), xlab="", ylab="")
  legend(x="topleft", bty="n",
   legend=paste0(sym_bol, c("", " solved")),
   title=NULL, inset=0.0, cex=1.0, lwd=6,
   lty=1, col=c("black", "blue"))
}  # end for
# eigen decomposition of covariance matrix
re_turns <- rutils::diff_it(price_s)
cov_mat <- cov(re_turns)
ei_gen <- eigen(cov_mat)
# perform PCA without scaling
pc_a <- prcomp(re_turns, scale=FALSE)
# compare outputs
all.equal(ei_gen$values, pc_a$sdev^2)
all.equal(abs(ei_gen$vectors), abs(pc_a$rotation),
    check.attributes=FALSE)
# eigen decomposition of correlation matrix
cor_mat <- cor(re_turns)
ei_gen <- eigen(cor_mat)
# perform PCA with scaling
pc_a <- prcomp(re_turns, scale=TRUE)
# compare outputs
all.equal(ei_gen$values, pc_a$sdev^2)
all.equal(abs(ei_gen$vectors), abs(pc_a$rotation),
    check.attributes=FALSE)
# load S&P500 constituent stock prices
load("C:/Develop/R/lecture_slides/data/sp500_prices.RData")
date_s <- index(price_s)
# calculate simple returns (not percentage)
re_turns <- rutils::diff_it(price_s)
# de-mean (center) and scale the returns
re_turns <- t(t(re_turns) - colMeans(re_turns))
re_turns <- t(t(re_turns) / sqrt(colSums(re_turns^2)/(NROW(re_turns)-1)))
re_turns <- xts(re_turns, date_s)
# perform principal component analysis PCA
pc_a <- prcomp(re_turns, scale=TRUE)
# find number of components with variance greater than 2
n_comp <- which(pc_a$sdev^2 < 2)[1]
# plot standard deviations of principal components
barplot(pc_a$sdev[1:n_comp],
  names.arg=colnames(pc_a$rotation[, 1:n_comp]),
  las=3, xlab="", ylab="",
  main="Volatilities of S&P500 Principal Components")
# Principal component loadings (weights)
# Plot barplots with PCA weights in multiple panels
n_comps <- 6
par(mfrow=c(n_comps/2, 2))
par(mar=c(4, 2, 2, 1), oma=c(0, 0, 0, 0))
# First principal component weights
weight_s <- sort(pc_a$rotation[, 1], decreasing=TRUE)
barplot(weight_s[1:6],
  las=3, xlab="", ylab="", main="")
title(paste0("PC", 1), line=-2.0,
col.main="red")
for (or_der in 2:n_comps) {
  weight_s <- sort(pc_a$rotation[, or_der], decreasing=TRUE)
  barplot(weight_s[c(1:3, 498:500)],
  las=3, xlab="", ylab="", main="")
  title(paste0("PC", or_der), line=-2.0,
  col.main="red")
}  # end for
# principal component time series
pca_rets <- xts(re_turns %*% pc_a$rotation[, 1:n_comps],
          order.by=date_s)
round(cov(pca_rets), 3)
pca_ts <- xts:::cumsum.xts(pca_rets)
# plot principal component time series in multiple panels
par(mfrow=c(n_comps/2, 2))
par(mar=c(2, 2, 0, 1), oma=c(0, 0, 0, 0))
ra_nge <- range(pca_ts)
for (or_der in 1:n_comps) {
  plot.zoo(pca_ts[, or_der],
     ylim=ra_nge,
     xlab="", ylab="")
  title(paste0("PC", or_der), line=-2.0)
}  # end for
par(mfrow=c(n_comps/2, 2))
par(mar=c(2, 2, 0, 1), oma=c(0, 0, 0, 0))
# invert principal component time series
inv_rotation <- solve(pc_a$rotation)
all.equal(inv_rotation, t(pc_a$rotation))
sol_ved <- pca_rets %*% inv_rotation[1:n_comps, ]
sol_ved <- xts::xts(sol_ved, date_s)
sol_ved <- xts:::cumsum.xts(sol_ved)
cum_returns <- xts:::cumsum.xts(re_turns)
# plot the solved returns
sym_bols <- c("MSFT", "XOM", "JPM", "AAPL", "BRK_B", "JNJ")
for (sym_bol in sym_bols) {
  plot.zoo(
    cbind(cum_returns[, sym_bol], sol_ved[, sym_bol]),
    plot.type="single", col=c("black", "blue"), xlab="", ylab="")
  legend(x="topleft", bty="n",
   legend=paste0(sym_bol, c("", " solved")),
   title=NULL, inset=0.05, cex=1.0, lwd=6,
   lty=1, col=c("black", "blue"))
}  # end for
par(mfrow=c(n_comps/2, 2))
par(mar=c(2, 2, 0, 1), oma=c(0, 0, 0, 0))
# perform ADF unit-root tests on original series and residuals
sapply(sym_bols, function(sym_bol) {
  c(series=tseries::adf.test(cum_returns[, sym_bol])$p.value,
    resid=tseries::adf.test(cum_returns[, sym_bol] - sol_ved[, sym_bol])$p.value)
})  # end sapply
# plot the residuals
for (sym_bol in sym_bols) {
  plot.zoo(cum_returns[, sym_bol] - sol_ved[, sym_bol],
    plot.type="single", col="blue", xlab="", ylab="")
  legend(x="topleft", bty="n",
   legend=paste0(sym_bol, " residuals"),
   title=NULL, inset=0.05, cex=1.0, lwd=6,
   lty=1, col="blue")
}  # end for
# perform ADF unit-root test on principal component time series
pca_rets <- xts(re_turns %*% pc_a$rotation,
          order.by=date_s)
pca_ts <- xts:::cumsum.xts(pca_rets)
adf_pvalues <- sapply(1:NCOL(pca_ts), function(or_der)
  tseries::adf.test(pca_ts[, or_der])$p.value)
# ADF unit-root test on stationary time series
tseries::adf.test(rnorm(1e5))
library(quantmod)
#perform pair-wise correlation analysis
# calculate correlation matrix
cor_mat <- cor(re_turns)
colnames(cor_mat) <- colnames(re_turns)
rownames(cor_mat) <- colnames(re_turns)
# reorder correlation matrix based on clusters
# calculate permutation vector
library(corrplot)
or_der <- corrMatOrder(cor_mat,
        order="hclust",
        hclust.method="complete")
# apply permutation vector
cor_mat <- cor_mat[or_der, or_der]
# plot the correlation matrix
col_ors <- colorRampPalette(c("red", "white", "blue"))
corrplot(cor_mat,
    tl.col="black", tl.cex=0.8,
    method="square", col=col_ors(8),
    cl.offset=0.75, cl.cex=0.7,
    cl.align.text="l", cl.ratio=0.25)
# draw rectangles on the correlation matrix plot
corrRect.hclust(cor_mat, k=NROW(cor_mat) %/% 2,
          method="complete", col="red")
# convert correlation matrix into distance object
dis_tance <- as.dist(1-cor_mat)
# perform hierarchical clustering analysis
clus_ter <- hclust(dis_tance)
plot(clus_ter, ann=FALSE, xlab="", ylab="")
title("Dendrogram representing hierarchical clustering
\nwith dissimilarity = 1-correlation", line=-0.5)
library(PerformanceAnalytics)  # load package "PerformanceAnalytics"
# PC returns from rotation and scaled re_turns
re_turns_scaled <- apply(re_turns, 2, scale)
pca_rets <- re_turns_scaled %*% pc_a$rotation
# "x" matrix contains time series of PC returns
dim(pc_a$x)
class(pc_a$x)
head(pc_a$x[, 1:3], 3)
# convert PC matrix to xts and rescale to decimals
pca_rets <- xts(pc_a$x/100,
    order.by=index(re_turns))
library(PerformanceAnalytics)  # load package "PerformanceAnalytics"
chart.CumReturns(
  pca_rets[, 1:3], lwd=2, ylab="",
  legend.loc="topright", main="")
# add title
title(main="ETF cumulative returns", line=-1)
library(PerformanceAnalytics)
# calculate PC correlation matrix
cor_mat <- cor(pca_rets)
colnames(cor_mat) <- colnames(pca_rets)
rownames(cor_mat) <- colnames(pca_rets)
cor_mat[1:3, 1:3]
table.CAPM(Ra=pca_rets[, 1:3],
    Rb=re_turns[, "VTI"], scale=252)
par(oma=c(1,0,1,0), mgp=c(2,1,0), mar=c(2,1,2,1), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # set plot panels
#Perform principal component analysis PCA
re_turns <- na.omit(rutils::etf_env$re_turns)
pc_a <- prcomp(re_turns, center=TRUE, scale=TRUE)
barplot(pc_a$sdev[1:10],
  names.arg=colnames(pc_a$rotation)[1:10],
  las=3, ylab="STDEV", xlab="PCVec",
  main="PCA Explain VAR")
# Show first three principal component loadings
head(pc_a$rotation[,1:3], 3)
# Permute second principal component loadings by size
pca_vec2 <- as.matrix(
  pc_a$rotation[order(pc_a$rotation[, 2],
  decreasing=TRUE), 2])
colnames(pca_vec2) <- "pca2"
head(pca_vec2, 3)
# The option las=3 rotates the names.arg labels
barplot(as.vector(pca_vec2),
  names.arg=rownames(pca_vec2),
  las=3, ylab="Loadings",
  xlab="Symbol", main="Loadings pca2")
par(oma=c(1,0,1,0), mgp=c(2,1,0), mar=c(2,1,2,1), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(3,1))  # set plot panels
# get list of principal component vectors
pca_vecs <- lapply(1:3, function(or_der) {
  pca_vec <- as.matrix(
    pc_a$rotation[
    order(pc_a$rotation[, or_der],
    decreasing=TRUE), or_der])
  colnames(pca_vec) <- paste0("pca", or_der)
  pca_vec
})  # end lapply
names(pca_vecs) <- c("pca1", "pca2", "pca3")
# The option las=3 rotates the names.arg labels
for (or_der in 1:3) {
  barplot(as.vector(pca_vecs[[or_der]]),
  names.arg=rownames(pca_vecs[[or_der]]),
  las=3, xlab="", ylab="",
  main=paste("Loadings",
    colnames(pca_vecs[[or_der]])))
}  # end for
library(factorAnalytics)  # load package "factorAnalytics"
# get documentation for package "factorAnalytics"
packageDescription("factorAnalytics")  # get short description
help(package="factorAnalytics")  # load help page
options(width=50)
library(factorAnalytics)  # load package "factorAnalytics"
# list all objects in "factorAnalytics"
ls("package:factorAnalytics")

# list all datasets in "factorAnalytics"
# data(package="factorAnalytics")

# remove factorAnalytics from search path
detach("package:factorAnalytics")
library(factorAnalytics)
# fit a three-factor model using PCA
factor_pca <- fitSfm(rutils::etf_env$re_turns, k=3)
head(factor_pca$loadings, 3)  # factor loadings
# factor realizations (time series)
head(factor_pca$factors)
# residuals from regression
factor_pca$residuals[1:3, 1:3]
library(factorAnalytics)
factor_pca$alpha  # estimated alphas
factor_pca$r2  # R-squared regression
# covariance matrix estimated by factor model
factor_pca$Omega[1:3, 4:6]
library(factorAnalytics)
# load(file="C:/Develop/R/lecture_slides/data/portf_optim.RData")
plot(factor_pca, which.plot.group=3,
     n.max=30, loop=FALSE)
# ?plot.sfm
library(PortfolioAnalytics)
# plot factor cumulative returns
chart.CumReturns(factor_pca$factors,
    lwd=2, ylab="", legend.loc="topleft",
    main="")

# plot time series of factor returns
# plot(factor_pca, which.plot.group=2,
#   loop=FALSE)
# asset correlations "hclust" hierarchical clustering order
plot(factor_pca, which.plot.group=7,
     loop=FALSE, order="hclust",
     method="ellipse")
library(PortfolioAnalytics)
# plot residual cumulative returns
chart.CumReturns(
  factor_pca$residuals[, c("IEF",
            "DBC", "XLF")],
  lwd=2, ylab="", legend.loc="topleft",
  main="")
library(PortfolioAnalytics)
# plot residual histogram with normal curve
plot(factor_pca, asset.name="VTI",
     which.plot.single=8,
     plot.single=TRUE, loop=FALSE,
     xlim=c(-0.007, 0.007))
library(PortfolioAnalytics)
# residual Q-Q plot
plot(factor_pca, asset.name="VTI",
     which.plot.single=9,
     plot.single=TRUE, loop=FALSE)
# SACF and PACF of residuals
plot(factor_pca, asset.name="VTI",
     which.plot.single=5,
     plot.single=TRUE, loop=FALSE)
