#################################
### FRE7241 Test #3 Solutions July 7, 2015
#################################
# Max score 90pts

# The below solutions are examples,
# Slightly different solutions are also possible.


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
end_points <- endpoints(etf_rets, on='years')
end_points[1] <- 1

# create a vector of symbols for the optimized portfolio,
sym_bols <- c("VTI", "VNQ", "DBC")

# create an initial vector of portfolio weights for the "sym_bols", 
# all equal to 1,
portf_weights <- rep(1, length(sym_bols))
names(portf_weights) <- sym_bols


# 2. (10pts) create an objective function equal to 
# minus the Sharpe ratio, and call it object_ive(), 
# the objective function should accept two arguments: 
# "weights" and "portf_rets",
object_ive <- function(weights, portf_rets) {
  portf_ts <- portf_rets %*% weights
  -mean(portf_ts)/sd(portf_ts)
}  # end object_ive

# apply object_ive() to an equal weight portfolio,
object_ive(weights=portf_weights, portf_rets=etf_rets[, sym_bols])


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

optim_portf <- function(portf_rets, start_point, end_point) {
  portf_rets <- portf_rets[(start_point:end_point), ]
  sym_bols <- colnames(portf_rets)
# create initial vector of portfolio weights equal to 1,
  portf_weights <- rep(1, ncol(portf_rets))
  names(portf_weights) <- sym_bols
# optimization to find weights with maximum Sharpe ratio
  optim_run <- optim(par=portf_weights, 
                     fn=object_ive, 
                     method="L-BFGS-B",
                     upper=c(1.1, 10, 10),
                     lower=c(0.9, -10, -10),
                     portf_rets=portf_rets)
# return optimal weights
  optim_run$par
}  # end optim_portf

# apply optim_portf() to the "sym_bols" portfolio, 
# with start_point=1 and end_point=207,
optim_portf(portf_rets=etf_rets[, sym_bols], start_point=1, end_point=207)



# 4. (20pts) find the optimal portfolio weights in each year  
# by performing an sapply() loop over "end_points",
# call the matrix returned by sapply() "ann_weights",
# you can use functions sapply(), optim_portf(), 
# and an anonymous function,
ann_weights <- sapply(2:length(end_points), 
                   function(in_dex) {
                     optim_portf(
                       portf_rets=etf_rets[, sym_bols], 
                       start_point=end_points[in_dex-1], 
                       end_point=end_points[in_dex])
                   }  # end anon function
)  # end sapply


# 5. (10pts) assign column names to "ann_weights", corresponding 
# to the year of the data,
# you can't assign the names by typing them one at a time,
# you must extract the names from the index of "etf_rets" using "end_points",
# you can use functions format(), index(), and lubridate function year(), 
colnames(ann_weights) <- format(index(etf_rets[end_points[-1]]), "%Y")
# or
library(lubridate)
colnames(ann_weights) <- year(index(etf_rets[end_points[-1]]))

# transpose "ann_weights",
ann_weights <- t(ann_weights)


# 6. (20pts) perform an sapply() loop over "end_points",
# in each year calculate the optimized portfolio returns,
# using portfolio weights from the previous year,
# you can use function sapply(), and an anonymous function,
optim_rets <- sapply(2:(length(end_points)-1),
                   function(in_dex) {
                     portf_rets <- 
                       etf_rets[(end_points[in_dex]:end_points[in_dex+1]), sym_bols]
                     portf_rets %*% ann_weights[in_dex-1, ]
                   }  # end anon function
)  # end sapply

# sapply() produces a list of vectors,
# flatten the list into a single vector, 
optim_rets <- do.call(rbind, optim_rets)

# plot the vector, 
plot(cumsum(optim_rets), t="l")

