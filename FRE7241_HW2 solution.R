#################################
### HW #2 (lecture #4) Solution
#################################
# due Dec. 9, 2014
# Max score 40pts

# The below solutions are an example,
# Slightly different solutions are also possible.

# 1. Perform PCA as follows:
etf_pca <- prcomp(etf_rets, center=TRUE, scale=TRUE)

# etf_rets is an xts time series of ETF returns,
# the principal component vectors are contained in the following matrix:
etf_pca$rotation
# the time series of principal component returns are contained in the following matrix:
etf_pca$x


# 2. (30pts) calculate the time series of principal component returns from etf_pca$rotation and etf_rets,
#    hint: you need to first scale etf_rets using function "scale",
#    compare your calculated PCA returns to etf_pca$x, to make sure they are exactly the same,
etf_rets_scaled <- apply(etf_rets, 2, scale)
pca_rets <- etf_rets_scaled %*% etf_pca$rotation
head(cbind(etf_pca$x[, 2], pca_rets[, 2]))


# 3. (5pts) convert the PCA returns matrix to a xts time series,
#    and rescale (divide) them by 100, so they are decimals, not percentages,
library(xts)
pca_rets <- xts(pca_rets/100, order.by=index(etf_rets))


# 4. (5pts) calculate CAPM Summary Statistics for the first three PCA returns,
library(PerformanceAnalytics)
table.CAPM(Ra=pca_rets[, 1:3], Rb=etf_rets[, "VTI"], scale=252)


