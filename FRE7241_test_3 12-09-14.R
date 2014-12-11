#################################
### Test #3 12/09/14
#################################
# Max score 30pts

# Please write in this file the R code needed to perform the tasks below, 
# rename the file to your_name_test3.R
# and send the file to the TA Dong Huang (dh1716@nyu.edu)


# 1. (10pts) create a portfolio object with equal weights,
load(file="C:/Develop/data/etf_analysis.RData")
load(file="C:/Develop/data/portf_optim.RData")
portf_names <- c("VTI", "IEF", "DBC", "XLF", 
                 "VNQ", "XLP", "XLV", "XLU", "XLB", "XLE")

# add leverage constraints, with min_sum=0.9, max_sum=1.1,
# add box constraint long/short, with min=-0.5, max=0.5,
# add objectives "mean" for "return" and "StdDev" for "risk",


# 2. (20pts) create a vector of annual date windows by year, 
#    from 2007 to 2014, using "as.character",
#    find optimal weights in each year by performing an "sapply" loop,
#    save the weights into a csv file using "write.zoo",

