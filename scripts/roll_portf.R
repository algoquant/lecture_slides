# Functions for simulating rolling portfolio optimization strategies

# library(HighFreq)  # load package HighFreq

# Simulate a rolling portfolio optimization strategy
# using the regularized inverse of the covariance matrix.

roll_portf_r <- function(excess, # Portfolio excess returns
                         returns, # Portfolio returns
                         startpoints, 
                         endpoints, 
                         alpha, 
                         max_eigen) {
  
  strat_rets <- lapply(2:NROW(endpoints), function(i) {
    # Subset the excess returns
    excess <- excess[startpoints[i-1]:endpoints[i-1], ]
    covmat <- cov(excess)
    # Perform eigen decomposition and calculate eigenvectors and eigenvalues
    eigend <- eigen(covmat)
    eigen_vec <- eigend$vectors
    # Calculate regularized inverse
    inverse <- eigen_vec[, 1:max_eigen] %*% (t(eigen_vec[, 1:max_eigen]) / eigend$values[1:max_eigen])
    # weights are proportional to the mean of excess
    excess_mean <- colMeans(excess)
    # Shrink excess_mean vector to the mean of excess_mean
    excess_mean <- ((1-alpha)*excess_mean + alpha*mean(excess_mean))
    # Apply regularized inverse to mean of excess
    weights <- drop(inverse %*% excess_mean)
    weights <- weights/sqrt(sum(weights^2))
    # Subset the returns to out-of-sample returns
    returns <- returns[(endpoints[i-1]+1):endpoints[i], ]
    # Calculate the out-of-sample portfolio returns
    xts(returns %*% weights, index(returns))
  }  # end anonymous function
  )  # end lapply
  
  # Flatten the list of xts into a single xts series
  strat_rets <- rutils::do_call(rbind, strat_rets)
  colnames(strat_rets) <- "strat_rets"
  # Add warmup period so that index(strat_rets) equals index(returns)
  indeks <- index(returns)[index(returns) < start(strat_rets)]
  strat_rets <- rbind(xts(numeric(NROW(indeks)), indeks), strat_rets)
  # Cumulate strat_rets
  cumsum(strat_rets)
}  # end roll_portf_r

