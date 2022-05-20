### Functions for back-testing strategies

# library(HighFreq)  # load package HighFreq

# Simulate a rolling portfolio optimization strategy
# using the regularized inverse of the covariance matrix.

# Function for back-testing rolling portfolio optimization strategy in R
roll_portf <- function(excess, # Excess returns
                       returns, # Stock returns
                       endp, # End points
                       look_back=12, # Look-back interval
                       eigen_max=3, # Eigen shrinkage intensity
                       alpha=0.0, # Return shrinkage intensity
                       bid_offer=0.0, # Bid-offer spread
                       ...) {
  npts <- NROW(endp)
  startp <- c(rep_len(0, look_back), endp[1:(npts-look_back)])
  pnls <- lapply(2:npts, function(ep) {
    # Calculate regularized inverse of covariance matrix
    insample <- excess[startp[ep-1]:endp[ep-1], ]
    eigend <- eigen(cov(insample))
    eigenvec <- eigend$vectors
    eigenval <- eigend$values
    invmat <- eigenvec[, 1:eigen_max] %*%
      (t(eigenvec[, 1:eigen_max]) / eigenval[1:eigen_max])
    # Shrink the in-sample returns to their mean
    insample <- (1-alpha)*insample + alpha*rowMeans(insample)
    # Calculate the maximum Sharpe ratio portfolio weights
    weightv <- invmat %*% colMeans(insample)
    weightv <- drop(weightv/sqrt(sum(weightv^2)))
    # Calculate the out-of-sample portfolio returns
    outsample <- returns[(endp[ep-1]+1):endp[ep], ]
    xts::xts(outsample %*% weightv, zoo::index(outsample))
  })  # end lapply
  pnls <- do.call(rbind, pnls)
  # Add warmup period to pnls
  # rbind(indeks[paste0("/", start(pnls)-1)], pnls)
  pnls
}  # end roll_portf



# Function backtestmom() performs a back-test of a momentum strategy over the end-points
backtestmom <- function(returns, 
                               objfunc=function(returns) (sum(returns)/sd(returns)), 
                               look_back=12, re_balance="months", bid_offer=0.001,
                               endp=rutils::calc_endpoints(returns, interval=re_balance), 
                               with_weights=FALSE, ...) {
  stopifnot("package:rutils" %in% search() || require("rutils", quietly=TRUE))
  # Define look-back and look-forward intervals
  nrows <- NROW(endp)
  startp <- c(rep_len(1, look_back-1), endp[1:(nrows-look_back+1)])
  # Calculate look-back intervals
  look_backs <- cbind(startp, endp)
  # Calculate look-forward intervals
  look_fwds <- cbind(endp + 1, rutils::lagit(endp, -1))
  look_fwds[nrows, 1] <- endp[nrows]
  # Calculate past performance over look-back intervals
  past <- t(apply(look_backs, 1, function(ep) sapply(returns[ep[1]:ep[2]], objfunc)))
  past[is.na(past)] <- 0
  # Calculate future performance
  future <- t(apply(look_fwds, 1, function(ep) sapply(returns[ep[1]:ep[2]], sum)))
  future[is.na(future)] <- 0
  # Scale weights so sum of squares is equal to 1
  weights <- past
  weights <- weights/sqrt(rowSums(weights^2))
  weights[is.na(weights)] <- 0  # Set NA values to zero
  # Calculate momentum profits and losses
  pnls <- rowSums(weights*future)
  # Calculate transaction costs
  costs <- 0.5*bid_offer*cumprod(1 + pnls)*rowSums(abs(rutils::diffit(weights)))
  pnls <- (pnls - costs)
  if (with_weights)
    rutils::lagit(cbind(pnls, weights))
  else
    rutils::lagit(pnls)
}  # end backtestmom


# Function momentum_daily() performs a back-test of a daily momentum strategy
# If tre_nd=(-1) then it backtests a mean reverting strategy
momentum_daily <- function(returns, look_back=252, bid_offer=0.001, tre_nd=1, ...) {
  stopifnot("package:quantmod" %in% search() || require("quantmod", quietly=TRUE))
  # Calculate rolling variance
  variance <- roll::roll_var(returns, width=look_back)
  variance <- zoo::na.locf(variance, na.rm=FALSE)
  variance[is.na(variance)] <- 0
  # Calculate rolling Sharpe
  past <- roll::roll_mean(returns, width=look_back)
  weights <- past/sqrt(variance)
  weights[variance == 0] <- 0
  weights[1:look_back, ] <- 1
  weights <- weights/sqrt(rowSums(weights^2))
  weights[is.na(weights)] <- 0
  weights <- rutils::lagit(weights)
  # Calculate momentum profits and losses
  pnls <- tre_nd*rowMeans(weights*returns)
  # Calculate transaction costs
  costs <- 0.5*bid_offer*rowSums(abs(rutils::diffit(weights)))
  cumprod(1 + pnls - costs)
}  # end momentum_daily
