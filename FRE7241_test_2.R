#################################
### FRE7241 Test #2 05/05/15
#################################
# Max score 50pts

# Please write in this file the R code needed to perform the tasks below, 
# rename the file to your_name_test2.R
# and send the file to Luping Liu (ll2525@nyu.edu)


# 1. (10pts) Load time series data and calculate returns,
# the file "etf_series.Rdata" contains time series data,
# create a new environment called "data_env",
# load data from the file "etf_series.Rdata" into "data_env",
# use function load(), with the "envir" argument,

### write your code here


# perform an eapply loop to extract the adjusted prices for all 
# the variables in "data_env", and call it "etf_series_ad",

### write your code here


# "etf_series_ad" should be an "xts" containing adjusted prices,
# with colnames in the format "name.Adjusted",
# rename the colnames and drop ".Adjusted" from the colnames,
# use functions sapply() and strsplit(),

### write your code here


# scrub (remove) rows with NA values from "etf_series_ad",
# use function complete.cases(),

### write your code here


# calculate an "xts" containing returns of "etf_series_ad", and call it "etf_rets",
# use functions lapply(), dailyReturn(), do.call(), and merge(),

# load package "quantmod",
library(quantmod)

### write your code here

# flatten list of "xts" into a single "xts",

### write your code here



# 2. (20pts) Create a function called "get_hyp_stats()" that returns hypothesis test stats,
# function "get_hyp_stats()" should accept a single "xts" argument called "re_turns", 
# The function get_hyp_stats() should perform the following steps:
#    perform Jarque-Bera test of normality on "re_turns",
#    perform Shapiro-Wilk test of normality on "re_turns",
#    return a named vector containing the Jarque-Bera and the Shapiro-Wilk statistics (not p.values!),
# use functions jarque.bera.test() and shapiro.test(), 
# be careful because shapiro.test() doesn't accept arguments of class "xts",

# load package "tseries"
library(tseries)

### write your code here

# apply get_hyp_stats() as follows, to verify it works properly:

get_hyp_stats(etf_rets[, 1])



# 3. (10pts) Apply function get_hyp_stats() to all the columns of "etf_rets", 
# and call the result "hyp_stats",
# the first column of "hyp_stats" should contain Jarque-Bera statistics, 
# while the second Shapiro-Wilk,
# the rownames of "hyp_stats" should contain the names of "etf_rets" columns, 
# use functions sapply() and t(), 

### write your code here



# 4. (10pts) Create a scatterplot of "hyp_stats", 
# and add labels containing the rownames of "hyp_stats",
# use functions plot() and text(),

### write your code here


# sort "hyp_stats" on column "jarque_bera" in ascending (increasing) order,
# use function order(),

### write your code here


# save "hyp_stats" to comma-delimited CSV file,
# use function write.csv(),

### write your code here
