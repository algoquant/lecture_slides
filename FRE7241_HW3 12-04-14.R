#################################
### HW #3 (lecture #5)
#################################
# due Dec. 16, 2014
# Max score 50pts

# Please write in this file the R code needed to perform the tasks below, 
# rename the file to your_name_hw3.R
# and send the file to the TA Dong Huang (dh1716@nyu.edu)


# 1. load ETF returns from "etf_data.Rdata" file,
load(file="C:/Develop/data/etf_data.Rdata")

#    create a list of symbols for creating an optimized portfolio,
sym_bols <- c("IEF", "VTI", "VNQ", "XLP")

#    create a named vector of portfolio weights all equal to "1",



# 2. (10pts) create an objective function proportional to the Sharpe ratio ("mean" divided by "sd"),
#    the objective function should be a function of the vector of portfolio weights,
#    the objective function should be at a minimum when the Sharpe ratio is at a maximum,
#    because the optimization function "optim" searches for the minimum,



# 3. (10pts) vectorize the objective function with respect to the weights for "VNQ" and "XLP",



# 4. (10pts) create vectors of weights for "VNQ" and "XLP", with 50 values from -1, to 1,
#    calculate the objective function on the 2-d parameter grid of "VNQ" and "XLP" weights,
#    set the weights of "IEF" and "VTI" equal to 0.5 and 0.1,



# 5. (5pts) create a perspective plot of the objective function,



# 6. (10pts) perform optimization using function "optim", to find the weight vector 
#    with the maximum Sharpe ratio,
#    set the "upper" and "lower" weights for "IEF" to 1.1, and 0.9,
#    calculate the Sharpe ratio for the optimal weights,



# 7. (5pts) calculate the xts of returns of the portfolio with optimal weights,
#    assign colnames to this xts,
#    plot the optimal portfolio weights using "barplot",
#    plot the cumulative returns of the optimal portfolio, 
#    together with "IEF" and "VTI", using "chart.CumReturns",
