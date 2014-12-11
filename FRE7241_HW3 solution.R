#################################
### HW #3 (lecture #5) Solution
#################################
# due Dec. 16, 2014
# Max score 50pts

# The below solutions are an example,
# Slightly different solutions are also possible.

# 1. load ETF returns from "etf_analysis.RData" file,
load(file="C:/Develop/data/etf_analysis.RData")

#    create a list of symbols for creating an optimized portfolio,
sym_bols <- c("IEF", "VTI", "VNQ", "XLP")

#    create a named vector of portfolio weights all equal to "1",
portf_weights <- rep(1, length(sym_bols))
names(portf_weights) <- sym_bols



# 2. (10pts) create an objective function proportional to the Sharpe ratio ("mean" divided by "sd"),
#    the objective function should be a function of the vector of portfolio weights,
#    the objective function should be at a minimum when the Sharpe ratio is at a maximum,
#    because the optimization function "optim" searches for the minimum,
object_ive <- function(weights) {
  portf_ts <- etf_rets[, sym_bols] %*% weights
  -mean(portf_ts)/sd(portf_ts)
}  # end object_ive
object_ive(portf_weights)



# 3. (10pts) vectorize the objective function with respect to the weights for "VNQ" and "XLP",
vec_objective <- Vectorize(
  FUN=function(w_VNQ, w_XLP, w_IEF_VTI)
    object_ive(c(w_IEF_VTI, w_VNQ, w_XLP)),
  vectorize.args=c("w_VNQ", "w_XLP")
)  # end Vectorize



# 4. (10pts) create vectors of weights for "VNQ" and "XLP", with 50 values from -1, to 1,
w_VNQ <- seq(-1, 1, length=50)
w_XLP <- seq(-1, 1, length=50)

#    calculate the objective function on the 2-d parameter grid of "VNQ" and "XLP" weights,
#    set the weights of "IEF" and "VTI" equal to 0.5 and 0.1,
objective_grid <- outer(w_VNQ, w_XLP, 
                vec_objective, w_IEF_VTI=c(0.5, 0.1))
rownames(objective_grid) <- round(w_VNQ, 2)
colnames(objective_grid) <- round(w_XLP, 2)



# 5. (5pts) create a perspective plot of the objective function,
persp(w_VNQ, w_XLP, -objective_grid,
      theta = 45, phi = 30,
      shade = 0.5,
      col = rainbow(50),
      border = "green",
      main = "objective function")



# 6. (10pts) perform optimization using function "optim", to find the weight vector 
#    with the maximum Sharpe ratio,
#    set the "upper" and "lower" weights for "IEF" to 1.1, and 0.9,
optim_run <- optim(par=portf_weights, 
                   fn=object_ive, 
                   method="L-BFGS-B",
                   upper=c(1.1, 10, 10, 10),
                   lower=c(0.9, -10, -10, -10))

#    calculate the Sharpe ratio for the optimal weights,
-object_ive(optim_run$par)



# 7. (5pts) calculate the xts of returns of the portfolio with optimal weights,
optim_rets <- xts(etf_rets[, sym_bols] %*% optim_run$par, order.by=index(etf_rets))
#    assign colnames to this xts,
colnames(optim_rets) <- "optim_rets"

#    plot the optimal portfolio weights using "barplot",
barplot(optim_run$par, names.arg=names(optim_run$par), 
        las=3, ylab="", xlab="Symbol", main="")
#    plot the cumulative returns of the optimal portfolio, 
#    together with "IEF" and "VTI", using "chart.CumReturns",
chart.CumReturns(
  cbind(optim_rets, etf_rets[, c("IEF", "VTI")]), 
  lwd=2, ylab="", legend.loc="topleft", main="")

