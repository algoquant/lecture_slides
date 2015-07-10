#################################
### FRE7241 Test #3 July 7, 2015
#################################
# Max score 90pts

# Please write in this file the R code needed to perform the tasks below, 
# rename the file to your_name_test3.R
# and upload it to NYU Classes,

# summary: calculate optimized portfolio weights in each year, 
# and apply them to out-of-sample data in the following year, 
# 
# 1. (5pts) Load time series data from file 
# "etf_data.Rdata" (download it from NYU Classes),
# the xts series "etf_rets" contains stock returns data,
# use function load(),
load(file="C:/Develop/data/etf_data.Rdata")

# create a vector of annual end points from the index of "etf_rets",
# and call it "end_points", set the first "end_points" equal to 1,
# use xts function endpoints(),
library(xts)

### write your code here

# create a vector of symbols for the optimized portfolio,
sym_bols <- c("VTI", "VNQ", "DBC")

# create an initial vector of portfolio weights for the "sym_bols", 
# all equal to 1,

### write your code here


# 2. (10pts) create an objective function equal to 
# minus the Sharpe ratio, and call it object_ive(), 
# the objective function should accept two arguments: 
# "weights" and "portf_rets",

### write your code here

# apply object_ive() to an equal weight portfolio,

### write your code here


# 3. (25pts) create a function called optim_portf(), 
# that accepts three arguments:
#   "portf_rets": an "xts" containing returns data, 
#   "start_point": the start index of "portf_rets", 
#   "end_point": the end index of "portf_rets", 
# optim_portf() should:
# - extract (subset) returns from "portf_rets" between "start_point" 
#    and "end_point",
# - extract the symbol names from the column names of "portf_rets",
# - create a named vector of initial portfolio weights equal to 1,
#    the portfolio weight names should be the appropriate symbol names,
# - optimize the weights to minimize the object_ive(), using the 
#    subset returns,
# - return the vector of optimal portfolio weights,
# 
# use function optim() with the dots "..." argument, 
# use functions colnames(), rep(), and object_ive(), 


### write your code here

# apply optim_portf() to the "sym_bols" portfolio, 
# with start_point=1 and end_point=207,

### write your code here



# 4. (20pts) find the optimal portfolio weights in each year  
# by performing an sapply() loop over "end_points",
# call the matrix returned by sapply() "ann_weights",
# you can use functions sapply(), optim_portf(), 
# and an anonymous function,

### write your code here


# 5. (10pts) assign column names to "ann_weights", corresponding 
# to the year of the data,
# you can't assign the names by typing them one at a time,
# you must extract the names from the index of "etf_rets" using "end_points",
# you can use functions format(), index(), and lubridate function year(), 
library(lubridate)

### write your code here

# transpose "ann_weights",

### write your code here


# 6. (20pts) perform an sapply() loop over "end_points",
# in each year calculate the optimized portfolio returns,
# using portfolio weights from the previous year,
# you can use function sapply(), and an anonymous function,

### write your code here

# sapply() produces a list of vectors,
# flatten the list into a single vector, 

### write your code here

# plot the vector, 

### write your code here

