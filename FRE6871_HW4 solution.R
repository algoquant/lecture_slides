#################################
### HW #4 Solution
#################################
# Max score 35pts

# The below solutions are examples,
# Slightly different solutions are also possible.

# comment:
# In general, the solution requires two loops: one over columns of EuStockMarkets,
# and another loop over moments.
# The two loops can be combinations of "for" and "apply", so many slightly different solutions are possible.

# Homework assignment:
# Create an R script for calculating the first four moments of all four time series in the EuStockMarkets data (DAX, SMI, CAC, FTSE),
# Your script should produce a 4x4 matrix containing all 16 moments, with row and column names,
# You can choose to use 'for' loops and/or apply() functions,
# Your script should use iteration, instead of manually repeating the same calculation for each index,


# setup:
# Calculate percentage returns of EuStockMarkets.
ts_rets <- diff(log(EuStockMarkets))
# Load package "moments".
library("moments")
# create 4x1 matrix of moment orders - for using "apply"
moment_orders <- as.matrix(1:4)  # 4x1 matrix of moment orders



#############
# first solution: perform two sapply loops
#############
# 35pts

eu_moments <- sapply(colnames(ts_rets), 
                     FUN=function(col_name) {
                        sapply(1:4, FUN=moment, x=ts_rets[, col_name])
                  # or instead you could use apply:
                  #  apply(X=moment_orders, MARGIN=1, FUN=moment, x=ts_rets[, col_name])
                     }  # end anonymous function
)  # end sapply

# add rownames
rownames(eu_moments) <- paste0("moment", 1:4)



#############
# second solution: perform one "for" loop, and one sapply loop
#############

# first allocate the matrix eu_moments 
eu_moments <- matrix(0.0*(1:16), ncol=ncol(ts_rets))

# then perform sapply loop nested in "for" loop
for (col_num in 1:ncol(ts_rets)) {
    eu_moments[, col_num] <- sapply(1:4, FUN=moment, x=ts_rets[, col_num])
# or instead you could use apply:
#  eu_moments[, col_num] <- apply(X=moment_orders, MARGIN=1, FUN=moment, x=ts_rets[, col_num])
}  # end for

# add colnames
colnames(eu_moments) <- colnames(ts_rets)
# add rownames
rownames(eu_moments) <- paste0("moment", 1:4)

