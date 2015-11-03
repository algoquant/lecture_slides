#################################
### FRE7241 Test #1 Sep 22, 2015
#################################
# Max score 55pts

# Please write in this file the R code needed to perform the tasks below, 
# rename the file to your_name_test1.R
# and upload the file to NYU Classes


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

### write your code here

# Load package "moments".
library("moments")

### write your code here

# add column names equal to the column names of EuStockMarkets data, 
# add row names equal to "moment1", "moment2", etc.
# you can use functions colnames(), rownames(), and paste(), 

### write your code here



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

### write your code here

# 2. (10pts)
# second method: you must use function ymd() from package lubridate,

library(lubridate)

### write your code here

# 3. (5pts)
# Create an xts object from the column Yen$s and "in_dex", 
# and call it "xts_yen",

### write your code here

# plot "xts_yen", using generic function plot(),

### write your code here


