library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
thm <- knit_theme$get("acid")
knit_theme$set(thm)
rm(list=ls())
options(digits=3)
setwd("C:/Develop/data")
library(xtable)
# ETF symbols for asset allocation
sym_bols <- c("VTI", "VEU", "IEF", "VNQ", 
  "DBC", "XLY", "XLP", "XLE", "XLF", "XLV", 
  "XLI", "XLB", "XLK", "XLU", "IWB", "IWD", 
  "IWF", "IWM", "IWN", "IWO", "IWP", "IWR", 
  "IWS", "IWV", "IUSV", "IUSG")
# read etf database into data frame
etf_list <- read.csv(file='etf_list.csv', 
               stringsAsFactors=FALSE)
rownames(etf_list) <- etf_list$Symbol
# subset etf_list only those ETF's in sym_bols
etf_list <- etf_list[sym_bols, ]
# shorten names
etf_names <- sapply(etf_list$Name, 
              function(name) {
  name_split <- strsplit(name, split=" ")[[1]]
  name_split <- 
    name_split[c(-1, -length(name_split))]
  name_match <- match("Select", name_split)
  if (!is.na(name_match))
    name_split <- name_split[-name_match]
  paste(name_split, collapse=" ")
})  # end sapply
etf_list$Name <- etf_names
etf_list["IEF", "Name"] <- "Treasury Bond Fund"
etf_list["XLY", "Name"] <- "Consumer Discr. Sector Fund"
etf_list[c(1, 2)]
print(xtable(etf_list), comment=FALSE, size="tiny")
library(quantmod)
library(ggplot2)
sym_bols <- sym_bols[c(1, 2)]
env_data <- new.env()
ls()
getSymbols(sym_bols, env=env_data)
ls(env_data)
class(env_data$VTI)
colnames(env_data$VTI)
head(env_data$VTI, 3)
library(quantmod)
library(ggplot2)
etf_series <- do.call(merge,
            as.list(env_data)[sym_bols])
etf_series_ad <- do.call(merge,
            eapply(env_data, Ad)[sym_bols])
col_names <- strsplit(
  colnames(etf_series_ad), split="[.]")
col_names <- as.data.frame(col_names,
              stringsAsFactors=FALSE)[1, ]
colnames(etf_series_ad) <- unlist(col_names)
tail(etf_series_ad[, 1:2], 3)
write.zoo(etf_series,
     file='etf_series.csv', sep=",")
save(etf_series, etf_series_ad,
     file='etf_data.RData')
etf_gg <- autoplot(etf_series_ad[, 1],
             main=etf_list$Name[1]) +
  xlab("") + ylab("") +
  theme(
    legend.position=c(0.1, 0.5),
    plot.title=element_text(vjust=-2.0),
    plot.background=element_blank()
  )  # end theme
# render ggplot
etf_gg
library(quantmod)
load(file="C:/Develop/data/etf_data.RData")
# scrub NA values
etf_series_ad <- 
  etf_series_ad[complete.cases(etf_series_ad)]
colnames(etf_series_ad)

# calculate returns from adjusted prices
library(quantmod)
etf_rets <- lapply(etf_series_ad, 
             function(x_ts) {
  daily_return <- dailyReturn(x_ts)
  colnames(daily_return) <- names(x_ts)
  daily_return
})  # end lapply

# "etf_rets" is a list of xts
class(etf_rets[[1]])

# flatten list of xts into a single xts
etf_rets <- do.call(merge, etf_rets)
class(etf_rets)
dim(etf_rets)
head(etf_rets[, 1:3])
library(PerformanceAnalytics)  # load package "PerformanceAnalytics"
# get documentation for package "PerformanceAnalytics"
packageDescription("PerformanceAnalytics")  # get short description
help(package="PerformanceAnalytics")  # load help page
data(package="PerformanceAnalytics")  # list all datasets in "PerformanceAnalytics"
ls("package:PerformanceAnalytics")  # list all objects in "PerformanceAnalytics"
detach("package:PerformanceAnalytics")  # remove PerformanceAnalytics from search path
library(PerformanceAnalytics)  # load package "PerformanceAnalytics"
perf_data <- 
  unclass(data(
    package="PerformanceAnalytics"))$results[, -(1:2)]
apply(perf_data, 1, paste, collapse=" - ")
data(managers)  # load "managers" data set
class(managers)
dim(managers)
head(managers, 3)
# load package "PerformanceAnalytics"
library(PerformanceAnalytics)
data(managers)  # load "managers" data set
ham_1 <- managers[, c("HAM1", "EDHEC LS EQ",
                "SP500 TR")]

chart.CumReturns(ham_1, lwd=2, ylab="",
  legend.loc="topleft", main="")
# add title
title(main="Managers cumulative returns",
line=-1)
library(PerformanceAnalytics)  # load package "PerformanceAnalytics"
data(managers)  # load "managers" data set
charts.PerformanceSummary(ham_1,
  main="", lwd=2, ylog=TRUE)
library(PerformanceAnalytics)  # load package "PerformanceAnalytics"
chart.CumReturns(
  etf_rets[, c("XLF", "XLP", "IEF")], lwd=2,
  ylab="", legend.loc="topleft", main="")
# add title
title(main="ETF cumulative returns", line=-1)
options(width=200)
library(PerformanceAnalytics)
chart.Drawdown(etf_rets[, "VTI"], ylab="", 
         main="VTI drawdowns")













table.Drawdowns(etf_rets[, "VTI"])
library(PerformanceAnalytics)
chart.Histogram(etf_rets[, 1], main="",
  xlim=c(-0.06, 0.06),
  methods = c("add.density", "add.normal"))
# add title
title(main=paste(colnames(etf_rets[, 1]),
           "density"), line=-1)
library(PerformanceAnalytics)
chart.Boxplot(etf_rets[,
  c(rownames(head(ret_stats, 3)),
    rownames(tail(ret_stats, 3)))])
library(PerformanceAnalytics)
tail(table.Stats(etf_rets[, 
  c("VTI", "IEF", "DBC", "IUSG")]), 3)
ret_stats <- table.Stats(etf_rets)
class(ret_stats)
# Transpose the data frame
ret_stats <- as.data.frame(t(ret_stats))
# plot scatterplot
plot(Kurtosis ~ Skewness, data=ret_stats,
     main="Kurtosis vs Skewness")
# add labels
text(x=ret_stats$Skewness, y=ret_stats$Kurtosis,
    labels=colnames(etf_rets),
    pos=1, cex=0.8)
# add skew_kurt column
ret_stats$skew_kurt <- 
  ret_stats$Skewness/ret_stats$Kurtosis
# sort on skew_kurt
ret_stats <- ret_stats[
  order(ret_stats$skew_kurt, 
  decreasing=TRUE), ]
# add names column
ret_stats$Name <- 
  etf_list[rownames(ret_stats), ]$Name
ret_stats[, c("Name", "Skewness", "Kurtosis")]
library(PerformanceAnalytics)
chart.RiskReturnScatter(etf_rets, Rf=0.01/12)
library(PerformanceAnalytics)
vti_ief <- etf_rets[, c("VTI", "IEF")]
SharpeRatio(vti_ief)

SortinoRatio(vti_ief)

CalmarRatio(vti_ief)
tail(table.Stats(vti_ief), 4)
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
save(file="C:/Develop/data/etf_data.RData")
etf_perf_stats[, c("Information Ratio", "Annualized Alpha")]
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
par(oma=c(1,0,1,0), mgp=c(2,1,0), mar=c(2,1,2,1), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # set plot panels
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
par(oma=c(1,0,1,0), mgp=c(2,1,0), mar=c(2,1,2,1), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
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
library(PortfolioAnalytics)  # load package "PortfolioAnalytics"
# get documentation for package "PortfolioAnalytics"
packageDescription("PortfolioAnalytics")  # get short description

help(package="PortfolioAnalytics")  # load help page

data(package="PortfolioAnalytics")  # list all datasets in "PortfolioAnalytics"

ls("package:PortfolioAnalytics")  # list all objects in "PortfolioAnalytics"

detach("package:PortfolioAnalytics")  # remove PortfolioAnalytics from search path
library(PortfolioAnalytics)
# load ETF returns
load(file="C:/Develop/data/etf_data.RData")
portf_names <- c("VTI", "IEF", "DBC", "XLF", 
  "VNQ", "XLP", "XLV", "XLU", "XLB", "XLE")
# initial portfolio to equal weights
portf_init <- rep(1/length(portf_names), 
            length(portf_names))
# named vector
names(portf_init) <- portf_names
# create portfolio object
portf_init <- portfolio.spec(
  assets=portf_init)
library(PortfolioAnalytics)
# add constraints
portf_maxSR <- add.constraint(
  portfolio=portf_init,  # initial portfolio
  type="weight_sum",  # constraint sum weights
  min_sum=0.9, max_sum=1.1)
# add constraints
portf_maxSR <- add.constraint(
  portfolio=portf_maxSR, 
  type="long_only")  # box constraint min=0, max=1
# add objectives
portf_maxSR <- add.objective(
  portfolio=portf_maxSR, 
  type="return",  # maximize mean return
  name="mean")
# add objectives
portf_maxSR <- add.objective(
  portfolio=portf_maxSR, 
  type="risk",  # minimize StdDev
  name="StdDev")
load(file="C:/Develop/data/portf_optim.RData")
library(PortfolioAnalytics)
# perform optimization of weights
maxSR_DEOpt <- optimize.portfolio(
  R=etf_rets[, portf_names],  # specify returns
  portfolio=portf_maxSR,  # specify portfolio
  optimize_method="DEoptim", # use DEoptim
  maxSR=TRUE,  # maximize Sharpe
  trace=TRUE, traceDE=0)
# plot optimization
chart.RiskReward(maxSR_DEOpt,
  risk.col="StdDev",
  return.col="mean")
library(PortfolioAnalytics)
load(file="C:/Develop/data/portf_optim.RData")
maxSR_DEOpt$weights
maxSR_DEOpt$objective_measures$mean[1]
maxSR_DEOpt$objective_measures$StdDev[[1]]
library(PortfolioAnalytics)
# plot optimization
chart.RiskReward(maxSR_DEOpt,
  risk.col="StdDev",
  return.col="mean")

# plot risk/ret points in portfolio scatterplot
risk_ret_points <- function(rets=etf_rets,
  risk=c("sd", "ETL"), sym_bols=c("VTI", "IEF")) {
  risk <- match.arg(risk)  # match to arg list
  if (risk=="ETL") {
    stopifnot(
"package:PerformanceAnalytics" %in% search() ||
require("PerformanceAnalytics", quietly=TRUE))
  }  # end if
  risk <- match.fun(risk)  # match to function
  risk_ret <- t(sapply(rets[, sym_bols],
     function(x_ts)
 c(ret=mean(x_ts), risk=abs(risk(x_ts)))))
  points(x=risk_ret[, "risk"], y=risk_ret[, "ret"],
   col="red", lwd=3, pch=21)
  text(x=risk_ret[, "risk"], y=risk_ret[, "ret"],
 labels=rownames(risk_ret), col="red",
 lwd=2, pos=4)
}  # end risk_ret_points

risk_ret_points()
library(PortfolioAnalytics)
plot_portf <- function(portfolio,
      rets_data=etf_rets) {
  portf_weights <- portfolio$weights
  portf_names <- names(portf_weights)
  # calculate xts of portfolio
  portf_max <- xts(
    rets_data[, portf_names] %*% portf_weights,
    order.by=index(rets_data))
  colnames(portf_max) <-
    deparse(substitute(portfolio))
  graph_params <- par(oma=c(1, 0, 1, 0),
    mgp=c(2, 1, 0), mar=c(2, 1, 2, 1),
    cex.lab=0.8, cex.axis=1.0,
    cex.main=0.8, cex.sub=0.5)
  layout(matrix(c(1,2), 2),
    widths=c(1,1), heights=c(1,3))
  barplot(portf_weights, names.arg=portf_names,
    las=3, ylab="", xlab="Symbol", main="")
  title(main=paste("Loadings",
          colnames(portf_max)), line=-1)
  chart.CumReturns(
    cbind(portf_max, rets_data[, c("IEF", "VTI")]),
    lwd=2, ylab="", legend.loc="topleft", main="")
  title(main=paste0(colnames(portf_max),
              ", IEF, VTI"), line=-1)
  par(graph_params)  # restore original parameters
  invisible(portf_max)
}  # end plot_portf
maxSR_DEOpt_xts <- plot_portf(portfolio=maxSR_DEOpt)
library(PortfolioAnalytics)
# add leverage constraint abs(weight_sum)
portf_maxSRN <- add.constraint(
  portfolio=portf_init, type="leverage",
  min_sum=0.9, max_sum=1.1)
# add box constraint long/short
portf_maxSRN <- add.constraint(
  portfolio=portf_maxSRN,
  type="box", min=-0.2, max=0.2)

# add objectives
portf_maxSRN <- add.objective(
  portfolio=portf_maxSRN,
  type="return",  # maximize mean return
  name="mean")
# add objectives
portf_maxSRN <- add.objective(
  portfolio=portf_maxSRN,
  type="risk",  # minimize StdDev
  name="StdDev")
load(file="C:/Develop/data/portf_optim.RData")
library(PortfolioAnalytics)
# perform optimization of weights
maxSRN_DEOpt <- optimize.portfolio(
  R=etf_rets[, portf_names],  # specify returns
  portfolio=portf_maxSRN,  # specify portfolio
  optimize_method="DEoptim", # use DEoptim
  maxSR=TRUE,  # maximize Sharpe
  trace=TRUE, traceDE=0)
# plot optimization
chart.RiskReward(maxSRN_DEOpt,
  risk.col="StdDev",
  return.col="mean",
  xlim=c(
    maxSR_DEOpt$objective_measures$StdDev[[1]]-0.001,
    0.016))
  points(x=maxSR_DEOpt$objective_measures$StdDev[[1]],
   y=maxSR_DEOpt$objective_measures$mean[1],
   col="green", lwd=3, pch=21)
  text(x=maxSR_DEOpt$objective_measures$StdDev[[1]],
   y=maxSR_DEOpt$objective_measures$mean[1],
 labels="maxSR", col="green",
 lwd=2, pos=4)
# plot risk/ret points in portfolio scatterplot
risk_ret_points()
library(PortfolioAnalytics)
load(file="C:/Develop/data/portf_optim.RData")
maxSRN_DEOpt$weights
maxSRN_DEOpt$objective_measures$mean[1]
maxSRN_DEOpt$objective_measures$StdDev[[1]]
library(PortfolioAnalytics)
maxSRN_DEOpt_xts <-
  plot_portf(portfolio=maxSRN_DEOpt)
library(PerformanceAnalytics)
load(file="C:/Develop/data/portf_optim.RData")
chart.CumReturns(
  cbind(maxSR_DEOpt_xts, maxSRN_DEOpt_xts),
  lwd=2, ylab="",
  legend.loc="topleft", main="")
library(PerformanceAnalytics)
load(file="C:/Develop/data/portf_optim.RData")
rbind(maxSR_DEOpt$weights, maxSRN_DEOpt$weights)
c(maxSR_DEOpt$objective_measures$mean,
maxSRN_DEOpt$objective_measures$mean)
c(maxSR_DEOpt$objective_measures$StdDev[[1]],
maxSRN_DEOpt$objective_measures$StdDev[[1]])
library(PortfolioAnalytics)
# add constraints
portf_maxSTARR <- add.constraint(
  portfolio=portf_init,  # initial portfolio
  type="weight_sum",  # constraint sum weights
  min_sum=0.9, max_sum=1.1)
# add constraints
portf_maxSTARR <- add.constraint(
  portfolio=portf_maxSTARR,
  type="long_only")  # box constraint min=0, max=1
# add objectives
portf_maxSTARR <- add.objective(
  portfolio=portf_maxSTARR,
  type="return",  # maximize mean return
  name="mean")
# add objectives
portf_maxSTARR <- add.objective(
  portfolio=portf_maxSTARR,
  type="risk",  # minimize StdDev
  name="ES")
load(file="C:/Develop/data/portf_optim.RData")
library(PortfolioAnalytics)
# perform optimization of weights
maxSTARR_DEOpt <- optimize.portfolio(
  R=etf_rets[, portf_names],  # specify returns
  portfolio=portf_maxSTARR,  # specify portfolio
  optimize_method="DEoptim", # use DEoptim
  maxSTARR=TRUE,  # maximize STARR
  trace=TRUE, traceDE=0)

# plot optimization
chart.RiskReward(maxSTARR_DEOpt,
  risk.col="ES",
  return.col="mean")
# plot risk/ret points in portfolio scatterplot
risk_ret_points(risk="ETL")
library(PortfolioAnalytics)
load(file="C:/Develop/data/portf_optim.RData")
maxSTARR_DEOpt$weights
maxSTARR_DEOpt$objective_measures$mean[1]
maxSTARR_DEOpt$objective_measures$ES[[1]]
library(PortfolioAnalytics)
maxSTARR_DEOpt_xts <-
  plot_portf(portfolio=maxSTARR_DEOpt)
library(PerformanceAnalytics)
load(file="C:/Develop/data/portf_optim.RData")
chart.CumReturns(
  cbind(maxSR_DEOpt_xts, maxSTARR_DEOpt_xts),
  lwd=2, ylab="",
  legend.loc="topleft", main="")
library(PerformanceAnalytics)
load(file="C:/Develop/data/portf_optim.RData")
rbind(maxSR_DEOpt$weights, maxSTARR_DEOpt$weights)
c(maxSR_DEOpt$objective_measures$mean,
maxSTARR_DEOpt$objective_measures$mean)
c(maxSR_DEOpt$objective_measures$StdDev[[1]],
maxSTARR_DEOpt$objective_measures$ES[[1]])
library(PortfolioAnalytics)
# plot the efficient frontier
chart.EfficientFrontier(maxSR_DEOpt,
          match.col="StdDev",
          n.portfolios=15, type="l")
points(x=maxSRN_DEOpt$objective_measures$StdDev[[1]],
   y=maxSRN_DEOpt$objective_measures$mean[1],
   col="green", lwd=3, pch=21)
text(x=maxSRN_DEOpt$objective_measures$StdDev[[1]],
   y=maxSRN_DEOpt$objective_measures$mean[1],
 labels="maxSRN", col="green",
 lwd=2, pos=4)
library(PortfolioAnalytics)
# add constraints
portf_minES <- add.constraint(
  portfolio=portf_init,  # initial portfolio
  type="weight_sum",  # constraint sum weights
  min_sum=0.9, max_sum=1.1)
# add constraints
portf_minES <- add.constraint(
  portfolio=portf_minES,
  type="long_only")  # box constraint min=0, max=1
# add objectives
portf_minES <- add.objective(
  portfolio=portf_minES,
  type="risk",  # minimize ES
  name="ES")
load(file="C:/Develop/data/portf_optim.RData")
library(PortfolioAnalytics)
# perform optimization of weights
minES_ROI <- optimize.portfolio(
  R=etf_rets[, portf_names],  # specify returns
  portfolio=portf_minES,  # specify portfolio
  optimize_method="ROI", # use ROI
  trace=TRUE, traceDE=0)

# plot optimization
chart.RiskReward(maxSTARR_DEOpt,
  risk.col="ES",
  return.col="mean")
  points(x=minES_ROI$objective_measures$ES[[1]],
   y=mean(minES_ROI_xts),
   col="green", lwd=3, pch=21)
  text(x=minES_ROI$objective_measures$ES[[1]],
   y=mean(minES_ROI_xts),
 labels="minES", col="green",
 lwd=2, pos=4)
# plot risk/ret points in portfolio scatterplot
risk_ret_points(risk="ETL")
library(PortfolioAnalytics)
load(file="C:/Develop/data/portf_optim.RData")
minES_ROI$weights
minES_ROI$objective_measures$ES[[1]]
library(PortfolioAnalytics)
minES_ROI_xts <-
  plot_portf(portfolio=minES_ROI)
library(PerformanceAnalytics)
load(file="C:/Develop/data/portf_optim.RData")
chart.CumReturns(
  cbind(maxSR_DEOpt_xts, minES_ROI_xts),
  lwd=2, ylab="",
  legend.loc="topleft", main="")
library(PerformanceAnalytics)
load(file="C:/Develop/data/portf_optim.RData")
rbind(maxSR_DEOpt$weights, minES_ROI$weights)
c(maxSR_DEOpt$objective_measures$mean,
minES_ROI$objective_measures$mean)
c(maxSR_DEOpt$objective_measures$StdDev[[1]],
minES_ROI$objective_measures$ES[[1]])
load(file="C:/Develop/data/portf_optim.RData")
library(PortfolioAnalytics)
# perform optimization of weights
maxSR_DEOpt <- optimize.portfolio(
  R=etf_rets["/2011", portf_names],  # specify returns
  portfolio=portf_maxSR,  # specify portfolio
  optimize_method="DEoptim", # use DEoptim
  maxSR=TRUE,  # maximize Sharpe
  trace=TRUE, traceDE=0)
weights_1h <- maxSR_DEOpt$weights

# plot optimization
maxSR_DEOpt_xts <-
  plot_portf(portfolio=maxSR_DEOpt)
load(file="C:/Develop/data/portf_optim.RData")
library(PortfolioAnalytics)
# perform optimization of weights
maxSR_DEOpt <- optimize.portfolio(
  R=etf_rets["2011/", portf_names],  # specify returns
  portfolio=portf_maxSR,  # specify portfolio
  optimize_method="DEoptim", # use DEoptim
  maxSR=TRUE,  # maximize Sharpe
  trace=TRUE, traceDE=0)
weights_2h <- maxSR_DEOpt$weights

# plot optimization
maxSR_DEOpt_xts <-
  plot_portf(portfolio=maxSR_DEOpt)
options(width=50)
par(oma=c(1,0,1,0), mgp=c(2,1,0), mar=c(2,1,2,1), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # set plot panels
weights_1h
weights_2h
weights_1h - weights_2h
barplot(weights_1h, 
  names.arg=names(weights_1h), 
  las=3, ylab="", xlab="", 
  main="Portfolio Weights First Half")
barplot(weights_2h, 
  names.arg=names(weights_2h), 
  las=3, ylab="", xlab="", 
  main="Portfolio Weights Second Half")
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
load(file="C:/Develop/data/etf_data.RData")
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
library(PerformanceAnalytics)
load(file="C:/Develop/data/etf_data.RData")
# rebalancing period
re_balance <- "weeks"
# look-back period in number of re_balance
win_dow <- 40

# create index of rebalancing period end points
end_points <- endpoints(etf_rets, 
                on=re_balance)
end_points[1] <- 1

# create index of rebalancing periods
periods <- lapply(win_dow:(length(end_points)-1),
    function(point)
list(back=end_points[point-win_dow+1]:(end_points[point]-1),
  fwd=end_points[point]:end_points[point+1])
    )  # end lapply
# calculate risk&ret stats for some symbols, over a range of dates
risk_ret_stats <- function(x_ts=etf_rets,  # daily returns
                 sym_bols=colnames(x_ts),  # names
                 range=index(x_ts),  # date range
                 ret="mean",  # return stat
                 risk="mad") {  # risk stat
  ret <- match.fun(ret)  # match to function
  risk <- match.fun(risk)  # match to function
  stats <- sapply(x_ts[range, sym_bols], 
  function(ts)
    c(ret=ret(ts), risk=risk(ts))
  )  # end sapply
  t(stats)
}  # end risk_ret_stats

# example
head(risk_ret_stats(range=
        periods[[1]]$back))
# calculate stats over overlaping period date windows
period_stats <- lapply(periods,
   function(point)
     cbind(risk_ret_stats(range=point$back),
      fut_ret=sapply(etf_rets[point$fwd, ], sum))
)  # end lapply
head(period_stats[[1]])
# calculate pnl for a given period
pnl_period <- function(period_stat, de_mean=FALSE) {
  weights <- period_stat[, "ret"]/
    period_stat[, "risk"]
  weights <- weights - de_mean*mean(weights)
  weights <- weights/sum(abs(weights))
c(sum(period_stat[, "fut_ret"]*weights), weights)
}  # end pnl_period

# calculate pnls over all windows
pnl_xts <- t(sapply(period_stats, pnl_period))
pnl_xts <- xts(pnl_xts,
     order.by=index(etf_rets)
 [end_points[win_dow:(length(end_points)-1)]]
 )  # end xts
colnames(pnl_xts)[1] <- "pnl"

# calculate transaction costs
bid_offer <- 0.001  # 10 bps for liquid ETFs
co_sts <- bid_offer*abs(diff(pnl_xts[, -1]))
co_sts[1, ] <- 0
co_sts <- rowSums(co_sts)
pnl_xts[, "pnl"] <- pnl_xts[, "pnl"] - co_sts
# plot cumulative pnl of strategy
plot(cumsum(pnl_xts[, "pnl"]),
  main=colnames(pnl_xts[, "pnl"]))
# plot portfolio weights
plot.zoo(pnl_xts[, portf_names], main="")
# calculate xts of net beta
betas <- c(1, etf_reg_stats[, 3])
names(betas)[1] <- colnames(pnl_xts[, 2])
# weights times betas
betas <- pnl_xts[, -1] * betas
betas <- xts(rowSums(betas),
    order.by=index(pnl_xts))
colnames(betas) <- "betas"
plot.zoo(cbind(betas,
    cumsum(etf_rets[, 1])[index(betas)]),
    main="betas & VTI", xlab="")
# create trading function
tot_pnl <- function(win_dow) {
  periods <- lapply(win_dow:(length(end_points)-1),
    function(point)
list(back=
end_points[point-win_dow+1]:(end_points[point]-1),
   fwd=end_points[point]:end_points[point+1])
  )  # end sapply
  period_stats <- lapply(periods,
 function(point)
   cbind(risk_ret_stats(range=point$back),
     fut_ret=sapply(etf_rets[point$fwd, ], sum))
  )  # end lapply
  pnl_xts <- t(sapply(period_stats, pnl_period))
  co_sts <- bid_offer*abs(diff(pnl_xts[, -1]))
  co_sts <- rowSums(co_sts)
  co_sts <- c(0, co_sts)
  pnl_xts[, 1] <- pnl_xts[, 1] - co_sts
  sum(pnl_xts[, 1])
}  # end tot_pnl
strat_profile <- sapply(4*(5:15), tot_pnl)
plot(cbind(4*(5:15), strat_profile), t="l")
