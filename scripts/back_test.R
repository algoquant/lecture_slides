# Functions for back-testing momentum strategies

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
