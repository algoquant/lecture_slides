library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
options(digits=3)
thm <- knit_theme$get("acid")
knit_theme$set(thm)
# load package HighFreq
library(HighFreq)
head(SPY_TAQ)
# load package HighFreq
library(HighFreq)
head(SPY)
# install package HighFreq from github
devtools::install_github(repo="algoquant/HighFreq")
# load package HighFreq
library(HighFreq)
# get documentation for package HighFreq
# get short description
packageDescription("HighFreq")
# load help page
help(package="HighFreq")
# list all datasets in "HighFreq"
data(package="HighFreq")
# list all objects in "HighFreq"
ls("package:HighFreq")
# remove HighFreq from search path
detach("package:HighFreq")
# load package HighFreq
library(HighFreq)
# you can see SPY when listing objects in HighFreq
ls("package:HighFreq")
# you can see SPY when listing datasets in HighFreq
data(package="HighFreq")
# but the SPY dataset isn't listed in the workspace
ls()
# HighFreq datasets are lazy loaded and available when needed
head(SPY)
# load all the datasets in package HighFreq
data(hf_data)
# HighFreq datasets are now loaded and in the workspace
head(SPY)
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
  env_etf$re_turns[, c("XLF", "XLP", "IEF")], lwd=2,
  ylab="", legend.loc="topleft", main="")
# add title
title(main="ETF cumulative returns", line=-1)
load(file="C:/Develop/data/etf_data.RData")
options(width=200)
library(PerformanceAnalytics)
chart.Drawdown(env_etf$re_turns[, "VTI"], ylab="",
         main="VTI drawdowns")
load(file="C:/Develop/data/etf_data.RData")
options(width=200)
library(PerformanceAnalytics)
table.Drawdowns(env_etf$re_turns[, "VTI"])
library(PerformanceAnalytics)
chart.Histogram(env_etf$re_turns[, 1], main="",
  xlim=c(-0.06, 0.06),
  methods = c("add.density", "add.normal"))
# add title
title(main=paste(colnames(env_etf$re_turns[, 1]),
           "density"), line=-1)
library(PerformanceAnalytics)
chart.Boxplot(env_etf$re_turns[,
  c("VTI", "IEF", "IVW", "VYM", "IWB", "DBC", "VXX")])
library(PerformanceAnalytics)
tail(table.Stats(env_etf$re_turns[,
  c("VTI", "IEF", "DBC", "VXX")]), 4)
risk_return <- table.Stats(env_etf$re_turns)
class(risk_return)
# Transpose the data frame
risk_return <- as.data.frame(t(risk_return))
# plot scatterplot
plot(Kurtosis ~ Skewness, data=risk_return,
     main="Kurtosis vs Skewness")
# add labels
text(x=risk_return$Skewness, y=risk_return$Kurtosis,
    labels=rownames(risk_return),
    pos=1, cex=0.8)
load(file="C:/Develop/data/etf_data.RData")
# add skew_kurt column
risk_return$skew_kurt <-
  risk_return$Skewness/risk_return$Kurtosis
# sort on skew_kurt
risk_return <- risk_return[
  order(risk_return$skew_kurt,
  decreasing=TRUE), ]
# add names column
risk_return$Name <-
  etf_list[rownames(risk_return), ]$Name
risk_return[, c("Name", "Skewness", "Kurtosis")]
library(PerformanceAnalytics)
chart.RiskReturnScatter(
  env_etf$re_turns[, colnames(env_etf$re_turns)!="VXX"],
  Rf=0.01/12)
library(PerformanceAnalytics)
vti_ief <- env_etf$re_turns[, c("VTI", "IEF")]
SharpeRatio(vti_ief)

SortinoRatio(vti_ief)

CalmarRatio(vti_ief)
tail(table.Stats(vti_ief), 4)
# formula of linear model with zero intercept
lin_formula <- z ~ x + y - 1
lin_formula

# collapsing a character vector into a text string
paste0("x", 1:5)
paste(paste0("x", 1:5), collapse="+")

# creating formula from text string
lin_formula <- as.formula(  # coerce text strings to formula
        paste("z ~ ",
          paste(paste0("x", 1:5), collapse="+")
          )  # end paste
      )  # end as.formula
class(lin_formula)
lin_formula
# modify the formula using "update"
update(lin_formula, log(.) ~ . + beta)
set.seed(1121)  # initialize random number generator
# define explanatory variable
explana_tory <- rnorm(100, mean=2)
noise <- rnorm(100)
# response equals linear form plus error terms
res_ponse <- -3 + explana_tory + noise
# specify regression formula
reg_formula <- res_ponse ~ explana_tory
reg_model <- lm(reg_formula)  # perform regression
class(reg_model)  # regressions have class lm
attributes(reg_model)
eval(reg_model$call$formula)  # regression formula
reg_model$coefficients  # regression coefficients
coef(reg_model)
par(oma=c(1, 2, 1, 0), mgp=c(2, 1, 0), mar=c(5, 1, 1, 1), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
x11(width=6, height=6)
plot(reg_formula)  # plot scatterplot using formula
title(main="Simple Regression", line=-1)
# add regression line
abline(reg_model, lwd=2, col="red")
# plot fitted (predicted) response values
points(x=explana_tory, y=reg_model$fitted.values,
       pch=16, col="blue")
reg_model_sum <- summary(reg_model)  # copy regression summary
reg_model_sum  # print the summary to console
attributes(reg_model_sum)$names  # get summary elements
reg_model_sum$coefficients
reg_model_sum$r.squared
reg_model_sum$adj.r.squared
reg_model_sum$fstatistic
# standard error of beta
reg_model_sum$
  coefficients["explana_tory", "Std. Error"]
sd(reg_model_sum$residuals)/sd(explana_tory)/
  sqrt(unname(reg_model_sum$fstatistic[3]))
anova(reg_model)
set.seed(1121)  # initialize random number generator
# high noise compared to coefficient
res_ponse <- 3 + 2*explana_tory + rnorm(30, sd=8)
reg_model <- lm(reg_formula)  # perform regression
# estimate of regression coefficient is not
# statistically significant
summary(reg_model)
par(oma=c(1, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=1.0, cex.axis=1.0, cex.main=1.0, cex.sub=1.0)
reg_stats <- function(std_dev) {  # noisy regression
  set.seed(1121)  # initialize number generator
# create explanatory and response variables
  explana_tory <- seq(from=0.1, to=3.0, by=0.1)
  res_ponse <- 3 + 0.2*explana_tory +
    rnorm(30, sd=std_dev)
# specify regression formula
  reg_formula <- res_ponse ~ explana_tory
# perform regression and get summary
  reg_model_sum <- summary(lm(reg_formula))
# extract regression statistics
  with(reg_model_sum, c(pval=coefficients[2, 4],
   adj_rsquared=adj.r.squared,
   fstat=fstatistic[1]))
}  # end reg_stats
# apply reg_stats() to vector of std dev values
vec_sd <- seq(from=0.1, to=0.5, by=0.1)
names(vec_sd) <- paste0("sd=", vec_sd)
mat_stats <- t(sapply(vec_sd, reg_stats))
# plot in loop
par(mfrow=c(NCOL(mat_stats), 1))
for (in_dex in 1:NCOL(mat_stats)) {
  plot(mat_stats[, in_dex], type="l",
 xaxt="n", xlab="", ylab="", main="")
  title(main=colnames(mat_stats)[in_dex], line=-1.0)
  axis(1, at=1:(NROW(mat_stats)),
 labels=rownames(mat_stats))
}  # end for
reg_stats <- function(da_ta) {  # get regression
# perform regression and get summary
  col_names <- colnames(da_ta)
  reg_formula <-
    paste(col_names[2], col_names[1], sep="~")
  reg_model_sum <- summary(lm(reg_formula,
                        data=da_ta))
# extract regression statistics
  with(reg_model_sum, c(pval=coefficients[2, 4],
   adj_rsquared=adj.r.squared,
   fstat=fstatistic[1]))
}  # end reg_stats
# apply reg_stats() to vector of std dev values
vec_sd <- seq(from=0.1, to=0.5, by=0.1)
names(vec_sd) <- paste0("sd=", vec_sd)
mat_stats <-
  t(sapply(vec_sd, function (std_dev) {
    set.seed(1121)  # initialize number generator
# create explanatory and response variables
    explana_tory <- seq(from=0.1, to=3.0, by=0.1)
    res_ponse <- 3 + 0.2*explana_tory +
rnorm(30, sd=std_dev)
    reg_stats(data.frame(explana_tory, res_ponse))
    }))
# plot in loop
par(mfrow=c(NCOL(mat_stats), 1))
for (in_dex in 1:NCOL(mat_stats)) {
  plot(mat_stats[, in_dex], type="l",
 xaxt="n", xlab="", ylab="", main="")
  title(main=colnames(mat_stats)[in_dex], line=-1.0)
  axis(1, at=1:(NROW(mat_stats)),
 labels=rownames(mat_stats))
}  # end for
# set plot paramaters - margins and font scale
par(oma=c(1,0,1,0), mgp=c(2,1,0), mar=c(2,1,2,1), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2, 2))  # plot 2x2 panels
plot(reg_model)  # plot diagnostic scatterplots
plot(reg_model, which=2)  # plot just Q-Q
library(lmtest)  # load lmtest
# perform Durbin-Watson test
dwtest(reg_model)
foo <- env_etf$re_turns[, c("VTI", "VEU")]
end_points <- endpoints(foo, on="months")
head(foo)
tail(foo)
class(foo)
dim(foo)
reg_model <- lm(paste(names(foo), collapse=" ~ "), data=foo)
reg_model_sum <- summary(reg_model)
reg_model_sum
dwtest(reg_model)

# filter over non-overlapping periods
bar <- names(foo)
foo <- merge(period.sum(foo[, 1], INDEX=end_points), period.sum(foo[, 2], INDEX=end_points))
foo <- foo[complete.cases(foo), ]
names(foo) <- bar

# filter over overlapping periods
foo <- rollsum(foo, k=11)


set.seed(1121)
library(lmtest)
# spurious regression in unit root time series
explana_tory <- cumsum(rnorm(100))  # unit root time series
res_ponse <- cumsum(rnorm(100))
reg_formula <- res_ponse ~ explana_tory
reg_model <- lm(reg_formula)  # perform regression
# summary indicates statistically significant regression
reg_model_sum <- summary(reg_model)
reg_model_sum$coefficients
reg_model_sum$r.squared
# Durbin-Watson test shows residuals are autocorrelated
dw_test <- dwtest(reg_model)
c(dw_test$statistic[[1]], dw_test$p.value)
par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # set plot panels
plot(reg_formula, xlab="", ylab="")  # plot scatterplot using formula
title(main="Spurious Regression", line=-1)
# add regression line
abline(reg_model, lwd=2, col="red")
plot(reg_model, which=2, ask=FALSE)  # plot just Q-Q
library(lmtest)  # load lmtest
design_matrix <- data.frame(  # design matrix
  explana_tory=1:30, omit_var=sin(0.2*1:30))
# response depends on both explanatory variables
res_ponse <- with(design_matrix,
  0.2*explana_tory + omit_var + 0.2*rnorm(30))
# mis-specified regression only one explanatory
reg_model <- lm(res_ponse ~ explana_tory,
        data=design_matrix)
reg_model_sum <- summary(reg_model)
reg_model_sum$coefficients
reg_model_sum$r.squared
# Durbin-Watson test shows residuals are autocorrelated
dwtest(reg_model)$p.value
par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # set plot panels
plot(reg_formula, data=design_matrix)
abline(reg_model, lwd=2, col="red")
title(main="OVB Regression", line=-1)
plot(reg_model, which=2, ask=FALSE)  # plot just Q-Q
set.seed(1121)
library(lmtest)
# spurious regression in unit root time series
explana_tory <- cumsum(rnorm(100))  # unit root time series
res_ponse <- cumsum(rnorm(100))
reg_formula <- res_ponse ~ explana_tory
reg_model <- lm(reg_formula)  # perform regression
# summary indicates statistically significant regression
reg_model_sum <- summary(reg_model)
reg_model_sum$coefficients
reg_model_sum$r.squared
# Durbin-Watson test shows residuals are autocorrelated
dw_test <- dwtest(reg_model)
c(dw_test$statistic[[1]], dw_test$p.value)
par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # set plot panels
plot(reg_formula, xlab="", ylab="")  # plot scatterplot using formula
title(main="Spurious Regression", line=-1)
# add regression line
abline(reg_model, lwd=2, col="red")
plot(reg_model, which=2, ask=FALSE)  # plot just Q-Q
explana_tory <- seq(from=0.1, to=3.0, by=0.1)  # explanatory variable
res_ponse <- 3 + 2*explana_tory + rnorm(30)
reg_formula <- res_ponse ~ explana_tory
reg_model <- lm(reg_formula)  # perform regression
new_data <- data.frame(explana_tory=0.1*31:40)
predict_lm <- predict(object=reg_model,
              newdata=new_data, level=0.95,
              interval="confidence")
predict_lm <- as.data.frame(predict_lm)
head(predict_lm, 2)
plot(reg_formula, xlim=c(1.0, 4.0),
     ylim=range(res_ponse, predict_lm),
     main="Regression predictions")
abline(reg_model, col="red")
with(predict_lm, {
  points(x=new_data$explana_tory, y=fit, pch=16, col="blue")
  lines(x=new_data$explana_tory, y=lwr, lwd=2, col="red")
  lines(x=new_data$explana_tory, y=upr, lwd=2, col="red")
})  # end with
library(quantmod)
load(file="C:/Develop/data/etf_data.RData")
re_turns <- na.omit(env_etf$re_turns)
# specify regression formula
reg_formula <- XLP ~ VTI
# perform regression
reg_model <- lm(reg_formula, data=re_turns)
# plot scatterplot of returns
plot(reg_formula, data=re_turns)
title(main="Regression XLP ~ VTI", line=-1)
# add regression line
abline(reg_model, lwd=2, col="red")
reg_model_sum <- summary(reg_model)
coef(reg_model_sum)
# Durbin-Watson test autocorrelation residuals
library(lmtest)
dwtest(reg_model)
library(quantmod)  # load quantmod
library(lmtest)  # load lmtest
# perform regressions and collect statistics
etf_reg_stats <- sapply(colnames(re_turns)[-1],
                  function(etf_name) {
# specify regression formula
  reg_formula <- as.formula(
    paste(etf_name, "~ VTI"))
# perform regression
  reg_model <- lm(reg_formula, data=re_turns)
# get regression summary
  reg_model_sum <- summary(reg_model)
# collect regression statistics
  etf_reg_stats <- with(reg_model_sum,
    c(alpha=coefficients[1, 1],
p_alpha=coefficients[1, 4],
beta=coefficients[2, 1],
p_beta=coefficients[2, 4]))
  etf_reg_stats <- c(etf_reg_stats,
         p_dw=dwtest(reg_model)$p.value)
  etf_reg_stats
})  # end sapply
etf_reg_stats <- t(etf_reg_stats)
# sort by p_alpha
etf_reg_stats <- etf_reg_stats[
  order(etf_reg_stats[, "p_alpha"]), ]
etf_reg_stats[, 1:3]
library(HighFreq)
# specify regression formula
reg_formula <- XLP ~ VTI
# perform rolling beta regressions every month
beta_s <- rollapply(env_etf$re_turns, width=252,
  FUN=function(design_matrix)
  coef(lm(reg_formula, data=design_matrix))[2],
  by=22, by.column=FALSE, align="right")
beta_s <- na.omit(beta_s)
# plot beta_s in x11() window
x11(width=(wid_th <- 6), height=(hei_ght <- 4))
chart_Series(x=beta_s,
  name=paste("rolling betas", format(reg_formula)))
# perform daily rolling beta regressions in parallel
library(roll)
beta_s <- roll_lm(x=env_etf$re_turns[, "VTI"],
            y=env_etf$re_turns[, "XLP"],
            width=252)$coefficients
# compare speed of rollapply() versus roll_lm()
library(microbenchmark)
da_ta <- env_etf$re_turns["2012", c("VTI", "XLP")]
summary(microbenchmark(
  rollapply=rollapply(da_ta, width=22,
FUN=function(design_matrix)
coef(lm(reg_formula, data=design_matrix))[2],
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
capm_stats <- table.CAPM(Ra=re_turns[, colnames(re_turns)!="VTI"],
        Rb=re_turns[, "VTI"], scale=252)
colnames(capm_stats) <-
  sapply(colnames(capm_stats),
  function (str) {strsplit(str, split=" ")[[1]][1]})
capm_stats <- as.matrix(capm_stats)
capm_stats <- t(capm_stats)
capm_stats <- capm_stats[
  order(capm_stats[, "Annualized Alpha"],
  decreasing=TRUE), ]
# copy capm_stats into env_etf and save to .RData file
assign("capm_stats", capm_stats, envir=env_etf)
save(env_etf, file='etf_data.RData')
# load(file="C:/Develop/data/etf_data.RData")
capm_stats[, c("Information Ratio", "Annualized Alpha")]
library(quantmod)
#Perform pair-wise correlation analysis
# Calculate correlation matrix
corr_matrix <- cor(re_turns)
colnames(corr_matrix) <- colnames(re_turns)
rownames(corr_matrix) <- colnames(re_turns)
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
# convert correlation matrix into distance object
data_dist <- as.dist(1-corr_matrix_ordered)
# Perform hierarchical clustering analysis
data_cluster <- hclust(data_dist)
plot(data_cluster, ann=FALSE, xlab="", ylab="")
title("Dissimilarity = 1-Correlation",
line=-0.5)
par(oma=c(1,0,1,0), mgp=c(2,1,0), mar=c(2,1,2,1), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # set plot panels
# load ETF returns
# load(file="C:/Develop/data/etf_data.RData")
re_turns <- na.omit(env_etf$re_turns)
#Perform principal component analysis PCA
etf_pca <- prcomp(re_turns, center=TRUE, scale=TRUE)
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
# PC returns from rotation and scaled re_turns
re_turns_scaled <- apply(re_turns, 2, scale)
pca_rets <- re_turns_scaled %*% etf_pca$rotation
# "x" matrix contains time series of PC returns
dim(etf_pca$x)
class(etf_pca$x)
head(etf_pca$x[, 1:3], 3)
# convert PC matrix to xts and rescale to decimals
pca_rets <- xts(etf_pca$x/100,
    order.by=index(re_turns))
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
    Rb=re_turns[, "VTI"], scale=252)
