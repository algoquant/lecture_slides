

library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
thm <- knit_theme$get("acid")
knit_theme$set(thm)



rm(list=ls())
setwd("C:/Develop/data")
library(xtable)
# ETF symbols for asset allocation
sym_bols <- c("VTI", "VEU", "IEF", "VNQ", 
        "DBC", "XLY", "XLP", "XLE", 
        "XLF", "XLV", "XLI", "XLB", 
        "XLK", "XLU", "IWB", "IWD", 
        "IWF", "IWM", "IWN", "IWO", 
        "IWP", "IWR", "IWS", "IWV", 
        "IUSV", "IUSG")
# read etf database into data frame
etf_list <- read.csv(file='etf_list.csv', 
               stringsAsFactors=FALSE)
# subset etf_list only those ETF's in sym_bols
etf_list <- 
  etf_list[etf_list$Symbol %in% sym_bols, ]
# shorten names
etf_names <- sapply(etf_list$Name, 
              function(name) {
  name_split <- strsplit(name, split=" ")[[1]]
  name_split <- 
    name_split[c(-1, -length(name_split))]
  paste(name_split, collapse=" ")
})  # end sapply
etf_list$Name <- etf_list$Name
rownames(etf_list) <- NULL
etf_list <- etf_list[match(sym_bols, etf_list$Symbol), ]



## etf_list[c(1, 2)]


print(xtable(etf_list), comment=FALSE, size="tiny")



library(quantmod)
library(ggplot2)
sym_bols <- sym_bols[c(1, 2)]
data_env <- new.env()
ls()
getSymbols(sym_bols, env=data_env)
ls(data_env)
class(data_env$VTI)
head(data_env$VTI, 3)



library(quantmod)
library(ggplot2)
etf_series <- do.call(merge, 
            as.list(data_env)[sym_bols])
etf_series_ad <- do.call(merge, 
            eapply(data_env, Ad)[sym_bols])
col_names <- strsplit(
  colnames(etf_series_ad), split="[.]")
col_names <- as.data.frame(col_names, 
              stringsAsFactors=FALSE)[1, ]
colnames(etf_series_ad) <- unlist(col_names)
tail(etf_series_ad[, 1:2], 3)
write.zoo(etf_series, 
     file='etf_series.csv', sep=",")
save(etf_series, etf_series_ad, 
     file='etf_series.Rdata')
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
load(file="C:/Develop/data/etf_series_large.RData")
etf_series_ad <- etf_series_ad[complete.cases(etf_series_ad)]
etf_rets <- lapply(etf_series_ad, function(x_ts) {
  daily_return <- dailyReturn(x_ts)
  colnames(daily_return) <- names(x_ts)
  daily_return
})  # end lapply
etf_rets <- do.call(merge, etf_rets)
class(etf_rets)



library(quantmod)
library(PerformanceAnalytics)
CalmarRatio(etf_rets[, "VTI"])
SharpeRatio(etf_rets[, "VTI"])
SortinoRatio(etf_rets[, "VTI"])
table.CAPM(Ra=etf_rets[, "XLE"], 
     Rb=etf_rets[, "VTI"], scale=252)



library(quantmod)
library(PerformanceAnalytics)
table.Drawdowns(etf_rets[, "VTI"])
chart.Drawdown(etf_rets[, "VTI"])



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
pca2_load <- as.matrix(
  etf_pca$rotation[order(etf_pca$rotation[,2], 
  decreasing=TRUE),2])
colnames(pca2_load) <- "PCA2"
head(pca2_load, 3)
# The option las=3 rotates the names.arg labels
barplot(as.vector(pca2_load), 
  names.arg=rownames(pca2_load), 
  las=3, ylab="Loadings", 
  xlab="Symbol", main="Loadings PCA2")


