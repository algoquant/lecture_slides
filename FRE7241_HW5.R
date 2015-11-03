#################################
### FRE7241 Homework #5 due Nov 3, 2015
#################################
# Max score 90 pts

# Please write in this file the R code needed to perform the tasks below, 
# rename it to your_name_hw5.R
# and upload the file to NYU Classes

##############
# Summary: perform a rolling portfolio optimization over 
# annual periods, calculate optimized portfolio weights 
# in each year, and apply them to out-of-sample data in 
# the following year. 

# Download the file "etf_data.Rdata" from NYU Classes, 
# and load() it.  "etf_data.Rdata" contains an xts series 
# called "etf_rets", containing ETF returns. 

load(file="C:/Develop/data/etf_data.Rdata")

# 1. (10pts) create a vector of annual end points from the index 
# of "etf_rets", and call it "end_points". 
# Use "xts" function endpoints(),

library(xts)

### write your code here

# the above code should produce the following data:
# > end_points
# [1] 0  207  460  712  964 1216 1466 1718 1970 2053

# The "end_points" values are the integer indices 
# corresponding to dates that form non-overlapping annual 
# intervals. 
# Create a list of elements containing the indices belonging 
# to the non-overlapping annual intervals, and call the list 
# "period_s". 
# Each element of "period_s" represents an interval 
# corresponding to a year, and contains the indices of dates 
# belonging to that interval. 
# For example, the first element should contain the 
# integers: 1:207 belonging to the year 2007, the second 
# element: 208:460 belonging to the year 2008, etc. 
# Assign names to the elements of "period_s" corresponding 
# the year. 
# hint: perform an lapply() loop over "end_points". 
# You can use functions lapply(), names(), format(), index(), 
# and an anonymous function,

### write your code here

# the above code should produce the following data:
# > tail(period_s$"2008")
# [1] 455 456 457 458 459 460
# > head(period_s$"2009")
# [1] 461 462 463 464 465 466
# notice that there's no overlap between second and third periods, 

# create a vector of symbols for the optimized portfolio,

sym_bols <- c("VTI", "VNQ", "DBC")

# create a named vector of initial portfolio weights for the 
# "sym_bols", all equal to 1, and call it "portf_weights", 
# You can use functions rep() and names(), 

### write your code here


# 2. (10pts) create an objective function equal to minus 
# the Sharpe ratio, and call it object_ive(). 
# The objective function should accept two arguments: 
#  "weights": the portfolio weights, 
#  "re_turns": an xts series containing returns data, 
# hint: you can adapt code from the slide "Portfolio 
# Objective Function". 
# You can use functions sum() and sd(), 

### write your code here

# apply object_ive() to an equal weight portfolio, and 
# "etf_rets" subset to "sym_bols", 

### write your code here


# 3. (20pts) create a function called optim_portf(), 
# that accepts a single argument:
#  "re_turns": an xts series containing returns data, 
# optim_portf() should:
# - create a named vector of initial portfolio weights 
#  all equal to 1, with the names extracted from the 
#  column names of "re_turns",
# - optimize the weights to minimize the object_ive(), 
# - rescale the weights, so their sum of squares is "1", 
# - return the vector of optimal portfolio weights, 
# hint: you can adapt code from the slide 
# "Multi-dimensional Portfolio Optimization". 
# You can use the function optim() with the dots "..." 
# argument, and with the "upper" and "lower" parameters 
# equal to 10 and -10, respectively. 
# You can also use the functions rep(), colnames(), 
# structure(), and object_ive(), 

### write your code here

# apply optim_portf() to "etf_rets" subset to "sym_bols" 
# and the period "2009" from "period_s", 

### write your code here


# 4. (20pts) Perform an sapply() loop over "period_s",
# and calculate the optimal portfolio weights in each 
# period. Call the matrix returned by sapply() "weight_s". 
# You can use functions sapply(), optim_portf(), 
# and an anonymous function,

### write your code here


# 5. (30pts) Perform an lapply() loop over the length of 
# "period_s", starting in the second period (year). 
# In each period calculate the portfolio returns, 
# using out-of-sample portfolio weights from the previous 
# period.  In each period coerce the portfolio returns to 
# an xts series and return it. 
# hint: you can use the names of periods "names(period_s)" 
# to extract the "weight_s" from the previous period. 
# You can use functions lapply(), xts(), index(), 
# and an anonymous function,

### write your code here

# The lapply() loop produces a list of xts series. 
# Flatten "optim_rets" into a single xts series. 
# You can use functions do.call() and rbind(), 

### write your code here

# "optim_rets" should look like this: 
# > dim(optim_rets)
# [1] 1846    1
# > head(optim_rets)
#                    [,1]
# 2008-01-02  0.007910575
# 2008-01-03  0.017271646
# 2008-01-04 -0.004024706
# 2008-01-07 -0.013655207
# 2008-01-08  0.007658724
# 2008-01-09 -0.005758695

# plot the cumulative sum of "optim_rets" using 
# chart_Series(). 
# You must use functions cumsum() and chart_Series(), 

library(quantmod)

### write your code here

