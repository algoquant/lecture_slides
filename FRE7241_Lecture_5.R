library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
thm <- knit_theme$get("acid")
knit_theme$set(thm)
library(quantmod)
load(file="C:/Develop/data/etf_data.Rdata")
# specify regression formula
reg_formula <- XLP ~ VTI
# perform regression
reg_model <- lm(reg_formula, data=etf_rets)
# plot scatterplot of returns
plot(reg_formula, data=etf_rets)
title(main="Regression XLP ~ VTI", line=-1)
# add regression line
abline(reg_model, lwd=2, col="red")


reg_model_sum <- summary(reg_model)
coef(reg_model_sum)
# Durbin-Watson test autocorrelation residuals
library(lmtest)
dwtest(reg_model)
library(lmtest)  # load lmtest
# perform regressions and collect statistics
etf_reg_stats <- sapply(colnames(etf_rets)[-1], 
                  function(etf_name) {
# specify regression formula
  reg_formula <- as.formula(
    paste(etf_name, "~ VTI"))
# perform regression
  reg_model <- lm(reg_formula, data=etf_rets)
# get regression summary
  reg_model_sum <- summary(reg_model)
# collect regression statistics
  etf_reg_stats <- with(reg_model_sum, 
    c(coefficients[1, 1], coefficients[1, 4], 
coefficients[2, 1], coefficients[2, 4]))
  etf_reg_stats <- c(etf_reg_stats, 
         dwtest(reg_model)$p.value)
  etf_reg_stats
})  # end sapply
rownames(etf_reg_stats) <- c("alpha", "p-alpha", 
                "beta", "p-beta", "p-dw")
etf_reg_stats <- t(etf_reg_stats)
# sort by p-alpha
etf_reg_stats <- etf_reg_stats[
  order(etf_reg_stats[, "p-alpha"]), ]
etf_reg_stats[, 1:3]
library(PerformanceAnalytics)
CAPM.beta(Ra=etf_rets[, "XLP"], Rb=etf_rets[, "VTI"])
CAPM.beta.bull(Ra=etf_rets[, "XLP"], 
  Rb=etf_rets[, "VTI"])
CAPM.beta.bear(Ra=etf_rets[, "XLP"], 
  Rb=etf_rets[, "VTI"])

CAPM.alpha(Ra=etf_rets[, "XLP"], Rb=etf_rets[, "VTI"])

InformationRatio(Ra=etf_rets[, "XLP"], 
     Rb=etf_rets[, "VTI"])
library(PerformanceAnalytics)
etf_betas <- sapply(etf_rets, CAPM.beta,
      Rb=etf_rets[, "VTI"])
etf_annrets <- sapply(etf_rets,
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
table.CAPM(Ra=etf_rets[, c("XLP", "XLF")], 
     Rb=etf_rets[, "VTI"], scale=252)
library(PerformanceAnalytics)
etf_perf_stats <- table.CAPM(Ra=etf_rets[, -1],
        Rb=etf_rets[, "VTI"], scale=252)
colnames(etf_perf_stats) <-
  sapply(colnames(etf_perf_stats),
  function (str) {strsplit(str, split=" ")[[1]][1]})
etf_perf_stats <- as.matrix(etf_perf_stats)
etf_perf_stats <- t(etf_perf_stats)
etf_perf_stats <- etf_perf_stats[
  order(etf_perf_stats[, "Annualized Alpha"],
  decreasing=TRUE), ]
load(file="C:/Develop/data/etf_data.Rdata")
etf_perf_stats[, c("Information Ratio", "Annualized Alpha")]
# load ETF returns
load(file="C:/Develop/data/etf_data.Rdata")
library(quantmod)
#Perform pair-wise correlation analysis
# Calculate correlation matrix
corr_matrix <- cor(etf_rets)
colnames(corr_matrix) <- colnames(etf_rets)
rownames(corr_matrix) <- colnames(etf_rets)
# Reorder the correlation matrix based on clusters
# Calculate permutation vector
library(corrplot)
corr_order <- corrMatOrder(corr_matrix,
        order="hclust",
        hclust.method="complete")
# Apply permutation vector
corr_matrix_ordered <-
  corr_matrix[corr_order, corr_order]
# Plot the correlation matrix
col3 <- colorRampPalette(c("red", "white", "blue"))
corrplot(corr_matrix_ordered,
    tl.col="black", tl.cex=0.8,
    method="square", col=col3(8),
    cl.offset=0.75, cl.cex=0.7,
    cl.align.text="l", cl.ratio=0.25)
# Draw rectangles on the correlation matrix plot
corrRect.hclust(corr_matrix_ordered,
k=13, method="complete", col="red")
# Perform hierarchical clustering analysis
data_dist <- as.dist(1-corr_matrix_ordered)
data_cluster <- hclust(data_dist)
plot(data_cluster,
     main="Dissimilarity = 1-Correlation",
     xlab="", ylab="")
par(oma=c(1, 0, 1, 0), mgp=c(2, 1, 0), mar=c(2, 1, 2, 1), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # set plot panels
# load ETF returns
load(file="C:/Develop/data/etf_data.Rdata")
#Perform principal component analysis PCA
etf_pca <- prcomp(etf_rets, center=TRUE, scale=TRUE)
barplot(etf_pca$sdev[1:10], 
  names.arg=colnames(etf_pca$rotation)[1:10], 
  las=3, ylab="STDEV", xlab="PCVec", 
  main="PCA Explain VAR")
# Show first three principal component loadings
head(etf_pca$rotation[,1:3], 3)
# Permute second principal component loadings by size
pca_vec2 <- as.matrix(
  etf_pca$rotation[order(etf_pca$rotation[, 2], 
  decreasing=TRUE), 2])
colnames(pca_vec2) <- "pca2"
head(pca_vec2, 3)
# The option las=3 rotates the names.arg labels
barplot(as.vector(pca_vec2), 
  names.arg=rownames(pca_vec2), 
  las=3, ylab="Loadings", 
  xlab="Symbol", main="Loadings pca2")
par(oma=c(1, 0, 1, 0), mgp=c(2, 1, 0), mar=c(2, 1, 2, 1), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(3,1))  # set plot panels
# get list of principal component vectors
pca_vecs <- lapply(1:3, function(in_dex) {
  pca_vec <- as.matrix(
    etf_pca$rotation[
    order(etf_pca$rotation[, in_dex],
    decreasing=TRUE), in_dex])
  colnames(pca_vec) <- paste0("pca", in_dex)
  pca_vec
})  # end sapply
names(pca_vecs) <- c("pca1", "pca2", "pca3")
# The option las=3 rotates the names.arg labels
for (in_dex in 1:3) {
  barplot(as.vector(pca_vecs[[in_dex]]),
  names.arg=rownames(pca_vecs[[in_dex]]),
  las=3, ylab="", xlab="",
  main=paste("Loadings",
    colnames(pca_vecs[[in_dex]])))
}  # end for
library(PerformanceAnalytics)  # load package "PerformanceAnalytics"
# PC returns from rotation and scaled etf_rets
etf_rets_scaled <- apply(etf_rets, 2, scale)
pca_rets <- etf_rets_scaled %*% etf_pca$rotation
# "x" matrix contains time series of PC returns
dim(etf_pca$x)
class(etf_pca$x)
head(etf_pca$x[, 1:3], 3)
# convert PC matrix to xts and rescale to decimals
pca_rets <- xts(etf_pca$x/100, 
    order.by=index(etf_rets))
library(PerformanceAnalytics)  # load package "PerformanceAnalytics"
chart.CumReturns(
  pca_rets[, 1:3], lwd=2, ylab="",
  legend.loc="topright", main="")
# add title
title(main="ETF cumulative returns", line=-1)
library(PerformanceAnalytics)
# Calculate PC correlation matrix
corr_matrix <- cor(pca_rets)
colnames(corr_matrix) <- colnames(pca_rets)
rownames(corr_matrix) <- colnames(pca_rets)
corr_matrix[1:3, 1:3]
table.CAPM(Ra=pca_rets[, 1:3], 
    Rb=etf_rets[, "VTI"], scale=252)
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
# load ETF returns
load(file="C:/Develop/data/etf_data.Rdata")
# fit a three-factor model using PCA
factor_pca <- fitSfm(etf_rets, k=3)
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
load(file="C:/Develop/data/portf_optim.RData")
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
# target vector of normal variables
target_vector <- rnorm(1000, mean=4, sd=2)
# objective function is log-likelihood
object_ive <- function(parm, target) {
  sum(2*log(parm[2]) + 
    ((target - parm[1])/parm[2])^2)
}  # end object_ive
# vectorize objective function
vec_objective <- Vectorize(
  FUN=function(mean, sd, target)
    object_ive(c(mean, sd), target),
  vectorize.args=c("mean", "sd")
)  # end Vectorize
# objective function on parameter grid
par_mean <- seq(1, 6, length=50)
par_sd <- seq(0.5, 3.0, length=50)
objective_grid <- outer(par_mean, par_sd, 
    vec_objective, target=target_vector)
rownames(objective_grid) <- round(par_mean, 2)
colnames(objective_grid) <- round(par_sd, 2)
objective_min <- which(
  objective_grid==min(objective_grid), 
  arr.ind=TRUE)
objective_min
objective_grid[objective_min]
objective_grid[(objective_min[, 1] + -1:1), 
         (objective_min[, 2] + -1:1)]
# perspective plot of objective function
persp(par_mean, par_sd, -objective_grid,
theta = 45, phi = 30,
shade = 0.5,
col = rainbow(50),
border = "green",
main = "objective function")
# initial parameters
par_init <- c(mean=0, sd=1)
# perform optimization quasi-Newton method
optim_run <- optim(par=par_init, 
       fn=object_ive, 
       target=target_vector,
       method="L-BFGS-B",
       upper=c(10, 10),
       lower=c(-10, 0.1))
# optimal parameters
optim_run$par
# plot histogram
histo_gram <- hist(target_vector, plot=FALSE)
plot(histo_gram, freq=FALSE,
     main="histogram of target vector")
curve(expr=dnorm(x, mean=optim_run$par["mean"],
           sd=optim_run$par["sd"]),
add=TRUE, type="l", lwd=2, col="red")
legend("topright", inset=0.0, cex=0.8, title=NULL,
 leg="optimal parameters",
 lwd=2, bg="white", col="red")
# target vector is mixture of normal distributions
target_vector <- c(rnorm(100, sd=1.0), 
             rnorm(100, mean=4, sd=1.0))
# objective function is log-likelihood
object_ive <- function(parm, target) {
  likelihood <- parm[1]/parm[3] * 
  dnorm((target-parm[2])/parm[3]) +
  (1-parm[1])/parm[5]*dnorm((target-parm[4])/parm[5])
  if(any(likelihood <= 0)) Inf else
    -sum(log(likelihood))
}  # end object_ive
# vectorize objective function
vec_objective <- Vectorize(
  FUN=function(mean, sd, w, m1, s1, target)
    object_ive(c(w, m1, s1, mean, sd), target),
  vectorize.args=c("mean", "sd")
)  # end Vectorize
# objective function on parameter grid
par_mean <- seq(3, 5, length=50)
par_sd <- seq(0.5, 1.5, length=50)
objective_grid <- outer(par_mean, par_sd, 
    vec_objective, target=target_vector,
    w=0.5, m1=2.0, s1=2.0)
rownames(objective_grid) <- round(par_mean, 2)
colnames(objective_grid) <- round(par_sd, 2)
objective_min <- which(objective_grid==
  min(objective_grid), arr.ind=TRUE)
objective_min
objective_grid[objective_min]
objective_grid[(objective_min[, 1] + -1:1), 
         (objective_min[, 2] + -1:1)]
# perspective plot of objective function
persp(par_mean, par_sd, -objective_grid,
theta = 45, phi = 30,
shade = 0.5,
col = rainbow(50),
border = "green",
main = "objective function")
# initial parameters
par_init <- c(weight=0.5, m1=0, s1=1, m2=2, s2=1)
# perform optimization
optim_run <- optim(par=par_init, 
      fn=object_ive, 
      target=target_vector,
      method="L-BFGS-B",
      upper=c(1,10,10,10,10),
      lower=c(0,-10,0.2,-10,0.2))
optim_run$par
# plot histogram
histo_gram <- hist(target_vector, plot=FALSE)
plot(histo_gram, freq=FALSE,
     main="histogram of target vector")
fit_func <- function(x, parm) {
  parm["weight"] * dnorm(x, mean=parm["m1"], sd=parm["s1"]) +
    (1-parm["weight"]) * dnorm(x, mean=parm["m2"], sd=parm["s2"])
}  # end fit_func
curve(expr=fit_func(x, parm=optim_run$par), add=TRUE,
type="l", lwd=2, col="red")
legend("topright", inset=0.0, cex=0.8, title=NULL,
 leg="optimal parameters",
 lwd=2, bg="white", col="red")
