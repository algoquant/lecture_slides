

library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
thm <- knit_theme$get("acid")
knit_theme$set(thm)



library(quantmod)
load(file="C:/Develop/data/etf_series_large.RData")
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



## library(PerformanceAnalytics)  # load package "PerformanceAnalytics"
## # get documentation for package "PerformanceAnalytics"
## packageDescription("PerformanceAnalytics")  # get short description
## help(package="PerformanceAnalytics")  # load help page
## data(package="PerformanceAnalytics")  # list all datasets in "PerformanceAnalytics"
## ls("package:PerformanceAnalytics")  # list all objects in "PerformanceAnalytics"
## detach("package:PerformanceAnalytics")  # remove PerformanceAnalytics from search path



library(PerformanceAnalytics)  # load package "PerformanceAnalytics"
perf_data <- 
  unclass(data(
    package="PerformanceAnalytics"))$results[, -(1:2)]
apply(perf_data, 1, paste, collapse=" - ")
data(managers)  # load "managers" data set
class(managers)
dim(managers)
head(managers, 3)



## # load package "PerformanceAnalytics"
## library(PerformanceAnalytics)
## data(managers)  # load "managers" data set
## ham_1 <- managers[, c("HAM1", "EDHEC LS EQ",
##                 "SP500 TR")]
## 
## chart.CumReturns(ham_1, lwd=2, ylab="",
##   legend.loc="topleft", main="")
## # add title
## title(main="Managers cumulative returns",
## line=-1)



## library(PerformanceAnalytics)  # load package "PerformanceAnalytics"
## data(managers)  # load "managers" data set
## charts.PerformanceSummary(ham_1,
##   main="", lwd=2, ylog=TRUE)



## library(PerformanceAnalytics)  # load package "PerformanceAnalytics"
## chart.CumReturns(
##   etf_rets[, c("XLF", "XLP", "IEF")], lwd=2,
##   ylab="", legend.loc="topleft", main="")
## # add title
## title(main="ETF cumulative returns", line=-1)



options(width=200)
library(PerformanceAnalytics)
chart.Drawdown(etf_rets[, "VTI"], ylab="", 
         main="VTI drawdowns")













table.Drawdowns(etf_rets[, "VTI"])



## library(PerformanceAnalytics)
## chart.Histogram(etf_rets[, 1], main="",
##   xlim=c(-0.06, 0.06),
##   methods = c("add.density", "add.normal"))
## # add title
## title(main=paste(colnames(etf_rets[, 1]),
##            "density"), line=-1)



## library(PerformanceAnalytics)
## chart.Boxplot(etf_rets[,
##   c(rownames(head(ret_stats, 3)),
##     rownames(tail(ret_stats, 3)))])



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



## library(PerformanceAnalytics)
## chart.RiskReturnScatter(etf_rets, Rf=0.01/12)



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
  names(etf_reg_stats) <- c("alpha", "p-alpha", 
             "beta", "p-beta", "p-dw")
  etf_reg_stats
})  # end sapply
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



## library(PerformanceAnalytics)
## etf_perf_stats <- table.CAPM(Ra=etf_rets[, -1],
##         Rb=etf_rets[, "VTI"], scale=252)
## colnames(etf_perf_stats) <-
##   sapply(colnames(etf_perf_stats),
##   function (str) {strsplit(str, split=" ")[[1]][1]})
## etf_perf_stats <- as.matrix(etf_perf_stats)
## etf_perf_stats <- t(etf_perf_stats)
## etf_perf_stats <- etf_perf_stats[
##   order(etf_perf_stats[, "Annualized Alpha"],
##   decreasing=TRUE), ]
## save(file="C:/Develop/data/etf_series_large.RData")



etf_perf_stats[, c("Information Ratio", "Annualized Alpha")]



library(quantmod)
### Perform pair-wise correlation analysis
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
### Perform principal component analysis PCA
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
dim(etf_pca$x)
head(etf_pca$x[, 1:3], 3)
class(etf_pca$x)

pca_rets <- xts(etf_pca$x/100, 
    order.by=index(etf_rets))
chart.CumReturns(
  pca_rets[, 1:3], lwd=2, geometric=FALSE,
  ylab="", legend.loc="bottomright", main="")
# add title
title(main="ETF cumulative returns", line=-1)


