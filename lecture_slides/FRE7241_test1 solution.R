#################################
### FRE7241 Test #1 Solutions Sep 22, 2015
#################################
# Max score 55pts

# The below solutions are examples,
# Slightly different solutions are also possible.


##############
# Summary: calculate the first four moments of all four time series 
# in the EuStockMarkets data (DAX, SMI, CAC, FTSE),

# Your script should produce a 4x4 matrix containing all 16 moments, 
# with row and column names,
# You can choose to use for() loops and/or apply() functions,
# Your script should use iteration, instead of manually repeating 
# the same calculation for each index,
# the output matrix should have proper row and column names.

# comment:
# In general, the solution requires two loops: 
# one over columns of EuStockMarkets, and another loop over moments.
# The two loops can be combinations of for() and apply() loops, 
# so that several slightly different solutions are possible.
# you can use functions for(), apply(), and sapply(), 

# 30pts
# Calculate percentage returns of EuStockMarkets.

ts_rets <- diff(log(EuStockMarkets))

# Load package "moments".
library("moments")

# first solution: perform two sapply loops

eu_moments <- sapply(colnames(ts_rets), 
                     FUN=function(col_name) {
                       sapply(1:4, FUN=moment, x=ts_rets[, col_name])
                     }  # end anonymous function
)  # end sapply


# second solution: perform sapply() loop nested in for() loop, 

# first allocate the matrix eu_moments 
eu_moments <- matrix(numeric(16), ncol=ncol(ts_rets))

for (col_num in seq(ncol(ts_rets))) {
  eu_moments[, col_num] <- sapply(1:4, FUN=moment, x=ts_rets[, col_num])
}  # end for


# add column names equal to the column names of EuStockMarkets data, 
# add row names equal to "moment1", "moment2", etc.
# you can use functions colnames(), rownames(), and paste(), 

colnames(eu_moments) <- colnames(EuStockMarkets)
rownames(eu_moments) <- paste0("moment", 1:4)



##############
# Convert integers representing dates to "POSIXct" date-time objects,

# Load the package "Ecdat", which contains a data frame called "Yen",
# the column Yen$date contains integers representing dates, in the 
# format "yyyymmdd", 
# from the column Yen$date create a vector of "POSIXct" dates, and 
# call it "in_dex", and set the "in_dex" timezone to "UTC", 
# you must perform this using two different methods, 

library("Ecdat")  # load Ecdat
head(Yen)  # explore the data

# 1. (10pts)
# first method: you can use functions as.character() and as.POSIXct() 
# (with a "format" argument), 
# but you cannot use any function from package lubridate, 

in_dex <- as.POSIXct(as.character(Yen$date), format="%Y%m%d", tz="UTC")

# 2. (10pts)
# second method: you must use function ymd() from package lubridate,

library(lubridate)
in_dex <- ymd(Yen$date, tz="UTC")

# 3. (5pts)
# Create an xts object from the column Yen$s and "in_dex", 
# and call it "xts_yen",

library(xts)
xts_yen <- xts(Yen$s, order.by=in_dex)

# plot "xts_yen", using generic function plot(),
plot(xts_yen)


