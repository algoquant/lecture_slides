#################################
### Test #3 Solutions 12/09/14
#################################
# Max score 45pts

# The below solutions are an example,
# Slightly different solutions are also possible.
# You must use the requested functions.


# 1. (10pts) create a portfolio object with equal weights,
load(file="C:/Develop/data/etf_analysis.RData")
portf_names <- c("VTI", "IEF", "DBC", "XLF", 
                 "VNQ", "XLP", "XLV", "XLU", "XLB", "XLE")
# create portfolio object with equal weights
portf_init <- rep(1/length(portf_names), 
                  length(portf_names))
names(portf_init) <- portf_names
portf_init <- portfolio.spec(
  assets=portf_init)

# add leverage constraints, with min_sum=0.9, max_sum=1.1,
# add box constraint long/short, with min=-0.5, max=0.5,
# add objectives "mean" for "return" and "StdDev" for "risk",
portf_maxSRN <- add.constraint(
  portfolio=portf_init, type="leverage",
  min_sum=0.9, max_sum=1.1)

# add box constraint long/short
portf_maxSRN <- add.constraint(
  portfolio=portf_maxSRN, 
  type="box", min=-0.5, max=0.5)

# add objectives
portf_maxSRN <- add.objective(
  portfolio=portf_maxSRN, 
  type="return",  # maximize mean return
  name="mean")
# add objectives
portf_maxSRN <- add.objective(
  portfolio=portf_maxSRN, 
  type="risk",  # minimize StdDev
  name="StdDev")


# 2. (20pts) create a vector of annual date windows by year, 
#    from 2007 to 2014, using "as.character",
year_win_dows <- as.character(2007:2014)

#    find optimal weights in each year by performing an "sapply" loop,
#    save the weights into a csv file using "write.zoo",
wei_ghts <- sapply(year_win_dows, 
                   function(ye_ar) {
  maxSR_DEOpt <- optimize.portfolio(
    R=etf_rets[ye_ar, portf_names],  # specify returns
    portfolio=portf_maxSR,  # specify portfolio
    optimize_method="DEoptim", # use DEoptim
    maxSR=TRUE,  # maximize Sharpe
    trace=TRUE, traceDE=0)
  maxSR_DEOpt$weights
}
  )


write.zoo(etf_series, 
          file='etf_series.csv', sep=",")
