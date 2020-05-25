library(knitr)
opts_chunk$set(prompt=TRUE, eval=FALSE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(digits=3)
options(width=60, dev='pdf')
thm <- knit_theme$get("acid")
knit_theme$set(thm)

library(rutils)
# Specify formula and perform regression
mod_el <- lm(XLP ~ VTI, 
  data=rutils::etf_env$re_turns)
# Get regression coefficients
coef(summary(mod_el))
# Durbin-Watson test of autocorrelation of residuals
lmtest::dwtest(mod_el)

# Plot scatterplot of returns with aspect ratio 1
plot(for_mula, data=rutils::etf_env$re_turns,
     xlim=c(-0.1, 0.1), ylim=c(-0.1, 0.1),
     asp=1, main="Regression XLP ~ VTI")
# Add regression line and perpendicular line
abline(mod_el, lwd=2, col="red")
abline(a=0, b=-1/coef(summary(mod_el))[2, 1],
 lwd=2, col="blue")

library(rutils)  # load rutils
re_turns <- na.omit(rutils::etf_env$re_turns)
# Perform regressions and collect statistics
etf_reg_stats <- sapply(colnames(re_turns)[-1],
                  function(etf_name) {
# Specify regression formula
  for_mula <- as.formula(
    paste(etf_name, "~ VTI"))
# Perform regression
  mod_el <- lm(for_mula, data=re_turns)
# Get regression summary
  model_sum <- summary(mod_el)
# Collect regression statistics
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
# Sort by p_alpha
etf_reg_stats <- etf_reg_stats[
  order(etf_reg_stats[, "p_alpha"]), ]

etf_reg_stats[, 1:3]

library(rutils)
# Specify regression formula
for_mula <- XLP ~ VTI
# Perform rolling beta regressions every month
beta_s <- rollapply(rutils::etf_env$re_turns, width=252,
  FUN=function(de_sign)
  coef(lm(for_mula, data=de_sign))[2],
  by=22, by.column=FALSE, align="right")
beta_s <- na.omit(beta_s)
# Plot beta_s in x11() window
x11(width=(wid_th <- 6), height=(hei_ght <- 4))
chart_Series(x=beta_s[, "VTI"],
  name=paste("rolling betas", format(for_mula)))
# Perform daily rolling beta regressions in parallel
library(roll)
beta_s <- roll_lm(x=rutils::etf_env$re_turns[, "VTI"],
            y=rutils::etf_env$re_turns[, "XLP"],
            width=252)$coefficients

# Compare speed of rollapply() versus roll_lm()
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
# Plot scatterplot
plot(etf_annrets ~ etf_betas, xlab="betas",
      ylab="ann. rets", xlim=c(-0.25, 1.6))
points(x=1, y=etf_annrets["VTI"], col="red",
 lwd=3, pch=21)
abline(a=0, b=etf_annrets["VTI"])
label_names <- rownames(etf_reg_stats)[1:13]
# Add labels
text(x=1, y=etf_annrets["VTI"], labels="VTI",
     pos=2)
text(x=etf_betas[label_names],
     y=etf_annrets[label_names],
     labels=label_names, pos=2, cex=0.8)

library(PerformanceAnalytics)
etf_betas <- sapply(
  re_turns[, colnames(re_turns)!="VXX"],
  CAPM.beta, Rb=re_turns[, "VTI"])
etf_annrets <- sapply(
  re_turns[, colnames(re_turns)!="VXX"],
  Return.annualized)
# Plot scatterplot
plot(etf_annrets ~ etf_betas, xlab="betas",
      ylab="ann. rets", xlim=c(-0.25, 1.6))
points(x=1, y=etf_annrets["VTI"], col="red",
 lwd=3, pch=21)
abline(a=0, b=etf_annrets["VTI"])
label_names <- rownames(etf_reg_stats)[1:13]
# Add labels
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
# Copy capm_stats into etf_env and save to .RData file
assign("capm_stats", capm_stats, envir=etf_env)
save(etf_env, file="etf_data.RData")

capm_stats[, c("Information Ratio", "Annualized Alpha")]

# load S&P500 constituent stock prices
load("C:/Develop/lecture_slides/data/sp500_prices.RData")
date_s <- index(price_s)
# Calculate simple returns (not percentage)
re_turns <- rutils::diff_it(price_s)
# Center (de-mean) and scale the returns
re_turns <- t(t(re_turns) - colMeans(re_turns))
re_turns <- t(t(re_turns) / sqrt(colSums(re_turns^2)/(NROW(re_turns)-1)))
re_turns <- xts(re_turns, date_s)
# Perform principal component analysis PCA
pc_a <- prcomp(re_turns, scale=TRUE)
# Find number of components with variance greater than 2
n_comp <- which(pc_a$sdev^2 < 2)[1]
# Plot standard deviations of principal components
barplot(pc_a$sdev[1:n_comp],
  names.arg=colnames(pc_a$rotation[, 1:n_comp]),
  las=3, xlab="", ylab="",
  main="Volatilities of S&P500 Principal Components")

# Calculate principal component loadings (weights)
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

# Calculate principal component time series
pca_rets <- xts(re_turns %*% pc_a$rotation[, 1:n_comps],
          order.by=date_s)
round(cov(pca_rets), 3)
pca_ts <- xts:::cumsum.xts(pca_rets)
# Plot principal component time series in multiple panels
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
# Invert principal component time series
inv_rotation <- solve(pc_a$rotation)
all.equal(inv_rotation, t(pc_a$rotation))
sol_ved <- pca_rets %*% inv_rotation[1:n_comps, ]
sol_ved <- xts::xts(sol_ved, date_s)
sol_ved <- xts:::cumsum.xts(sol_ved)
cum_returns <- xts:::cumsum.xts(re_turns)
# Plot the solved returns
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
# Perform ADF unit-root tests on original series and residuals
sapply(sym_bols, function(sym_bol) {
  c(series=tseries::adf.test(cum_returns[, sym_bol])$p.value,
    resid=tseries::adf.test(cum_returns[, sym_bol] - sol_ved[, sym_bol])$p.value)
})  # end sapply
# Plot the residuals
for (sym_bol in sym_bols) {
  plot.zoo(cum_returns[, sym_bol] - sol_ved[, sym_bol],
    plot.type="single", col="blue", xlab="", ylab="")
  legend(x="topleft", bty="n",
   legend=paste0(sym_bol, " residuals"),
   title=NULL, inset=0.05, cex=1.0, lwd=6,
   lty=1, col="blue")
}  # end for
# Perform ADF unit-root test on principal component time series
pca_rets <- xts(re_turns %*% pc_a$rotation,
          order.by=date_s)
pca_ts <- xts:::cumsum.xts(pca_rets)
adf_pvalues <- sapply(1:NCOL(pca_ts), function(or_der)
  tseries::adf.test(pca_ts[, or_der])$p.value)
# AdF unit-root test on stationary time series
tseries::adf.test(rnorm(1e5))

library(quantmod)
#Perform pair-wise correlation analysis
# Calculate correlation matrix
cor_mat <- cor(re_turns)
colnames(cor_mat) <- colnames(re_turns)
rownames(cor_mat) <- colnames(re_turns)
# Reorder correlation matrix based on clusters
# Calculate permutation vector
library(corrplot)
or_der <- corrMatOrder(cor_mat,
        order="hclust",
        hclust.method="complete")
# Apply permutation vector
cor_mat <- cor_mat[or_der, or_der]
# Plot the correlation matrix
col_ors <- colorRampPalette(c("red", "white", "blue"))
corrplot(cor_mat,
    tl.col="black", tl.cex=0.8,
    method="square", col=col_ors(8),
    cl.offset=0.75, cl.cex=0.7,
    cl.align.text="l", cl.ratio=0.25)
# draw rectangles on the correlation matrix plot
corrRect.hclust(cor_mat, k=NROW(cor_mat) %/% 2,
          method="complete", col="red")

# Convert correlation matrix into distance object
dis_tance <- as.dist(1-cor_mat)
# Perform hierarchical clustering analysis
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
# Convert PC matrix to xts and rescale to decimals
pca_rets <- xts(pc_a$x/100,
    order.by=index(re_turns))

library(PerformanceAnalytics)  # load package "PerformanceAnalytics"
chart.CumReturns(
  pca_rets[, 1:3], lwd=2, ylab="",
  legend.loc="topright", main="")
# Add title
title(main="ETF cumulative returns", line=-1)

library(PerformanceAnalytics)
# Calculate PC correlation matrix
cor_mat <- cor(pca_rets)
colnames(cor_mat) <- colnames(pca_rets)
rownames(cor_mat) <- colnames(pca_rets)
cor_mat[1:3, 1:3]
table.CAPM(Ra=pca_rets[, 1:3],
    Rb=re_turns[, "VTI"], scale=252)

par(oma=c(1,0,1,0), mgp=c(2,1,0), mar=c(2,1,2,1), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # Set plot panels
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
par(mfrow=c(3,1))  # Set plot panels
# Get list of principal component vectors
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
# Get documentation for package "factorAnalytics"
packageDescription("factorAnalytics")  # Get short description
help(package="factorAnalytics")  # load help page

options(width=50)
library(factorAnalytics)  # load package "factorAnalytics"
# list all objects in "factorAnalytics"
ls("package:factorAnalytics")

# list all datasets in "factorAnalytics"
# data(package="factorAnalytics")

# Remove factorAnalytics from search path
detach("package:factorAnalytics")

library(factorAnalytics)
# Fit a three-factor model using PCA
factor_pca <- fitSfm(rutils::etf_env$re_turns, k=3)
head(factor_pca$loadings, 3)  # Factor loadings
# Factor realizations (time series)
head(factor_pca$factors)
# Residuals from regression
factor_pca$residuals[1:3, 1:3]

library(factorAnalytics)
factor_pca$alpha  # Estimated alphas
factor_pca$r2  # R-squared regression
# Covariance matrix estimated by factor model
factor_pca$Omega[1:3, 4:6]

library(factorAnalytics)
# load(file="C:/Develop/lecture_slides/data/portf_optim.RData")
plot(factor_pca, which.plot.group=3,
     n.max=30, loop=FALSE)
# ?plot.sfm

library(PortfolioAnalytics)
# Plot factor cumulative returns
chart.CumReturns(factor_pca$factors,
    lwd=2, ylab="", legend.loc="topleft",
    main="")

# Plot time series of factor returns
# Plot(factor_pca, which.plot.group=2,
#   loop=FALSE)

# Asset correlations "hclust" hierarchical clustering order
plot(factor_pca, which.plot.group=7,
     loop=FALSE, order="hclust",
     method="ellipse")

library(PortfolioAnalytics)
# Plot residual cumulative returns
chart.CumReturns(
  factor_pca$residuals[, c("IEF",
            "DBC", "XLF")],
  lwd=2, ylab="", legend.loc="topleft",
  main="")

library(PortfolioAnalytics)
# Plot residual histogram with normal curve
plot(factor_pca, asset.name="VTI",
     which.plot.single=8,
     plot.single=TRUE, loop=FALSE,
     xlim=c(-0.007, 0.007))

library(PortfolioAnalytics)
# Residual Q-Q plot
plot(factor_pca, asset.name="VTI",
     which.plot.single=9,
     plot.single=TRUE, loop=FALSE)

# SACF and PACF of residuals
plot(factor_pca, asset.name="VTI",
     which.plot.single=5,
     plot.single=TRUE, loop=FALSE)
