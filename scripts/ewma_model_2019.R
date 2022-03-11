# Functions for simulating EWMA strategies

# library(HighFreq)  # load package HighFreq

# Simulate single EWMA model using historical ohlc data,
# and return percentage returns.
simu_ewma <- function(ohlc, lambdav=0.01, wid_th=251, bid_offer=0.001, tre_nd=1) {
  # Calculate EWMA prices
  weights <- exp(-lambdav*1:wid_th)
  weights <- weights/sum(weights)
  closep <- quantmod::Cl(ohlc)
  ew_ma <- stats::filter(as.numeric(closep), filter=weights, sides=1)
  ew_ma[1:(wid_th-1)] <- ew_ma[wid_th]
  # Determine dates right after EWMA has crossed prices
  indica_tor <- tre_nd*xts::xts(sign(as.numeric(closep) - ew_ma), order.by=index(ohlc))
  indicator_lag <- rutils::lagit(indica_tor)
  trade_dates <- (rutils::diffit(indica_tor) != 0)
  trade_dates <- which(trade_dates) + 1
  trade_dates <- trade_dates[trade_dates<NROW(ohlc)]
  # Calculate positions, either: -1, 0, or 1
  posi_tion <- rep(NA_integer_, NROW(ohlc))
  posi_tion[1] <- 0
  posi_tion[trade_dates] <- indicator_lag[trade_dates]
  posi_tion <- na.locf(posi_tion)
  posi_tion <- xts(posi_tion, order.by=index(ohlc))
  openp <- quantmod::Op(ohlc)
  close_lag <- rutils::lagit(closep)
  pos_lagged <- rutils::lagit(posi_tion)
  # Calculate transaction costs
  costs <- 0.0*posi_tion
  costs[trade_dates] <- 0.5*bid_offer*abs(pos_lagged[trade_dates] - posi_tion[trade_dates])*openp[trade_dates]
  # Calculate daily profits and losses
  returns <- pos_lagged*(closep - close_lag)
  returns[trade_dates] <- pos_lagged[trade_dates] * (openp[trade_dates] - close_lag[trade_dates]) + posi_tion[trade_dates] * (closep[trade_dates] - openp[trade_dates]) - costs
  # Calculate strategy returns
  output <- cbind(posi_tion, returns)
  colnames(output) <- c("positions", "returns")
  output
}  # end simu_ewma



# simulate two EWMA model using historical ohlc data
simu_ewma2 <- function(ohlc, lambda1=0.25, lambda2=0.05, wid_th=51) {
  # Calculate EWMA prices
  weights1 <- exp(-lambda1*1:wid_th)
  weights1 <- weights1/sum(weights1)
  weights2 <- exp(-lambda2*1:wid_th)
  weights2 <- weights2/sum(weights2)
  # Calculate open and close prices
  openp <- Op(ohlc)
  closep <- Cl(ohlc)
  # Adjust close price to start at zero
  openp <- openp - as.numeric(closep[1, ])
  closep <- closep - as.numeric(closep[1, ])
  prices_lag <- rutils::lagit(closep)
  # Filter the prices using weights
  ewma1 <- filter(closep, filter=weights1, sides=1)
  ewma1[1:(wid_th-1)] <- ewma1[wid_th]
  ewma2 <- filter(closep, filter=weights2, sides=1)
  ewma2[1:(wid_th-1)] <- ewma2[wid_th]
  # Determine dates right after EWMAs have crossed
  indic <- xts(sign(ewma1 - ewma2), order.by=index(ohlc))
  trade_dates <- (rutils::diffit(indic) != 0)
  trade_dates <- which(trade_dates) + 1
  trade_dates <- trade_dates[trade_dates<NROW(ohlc)]
  # Calculate positions, either: -1, 0, or 1
  position_s <- rep(NA_integer_, NROW(closep))
  position_s[1] <- 0
  position_s[trade_dates] <- rutils::lagit(indic)[trade_dates]
  position_s <- xts(na.locf(position_s), order.by=index(ohlc))
  position_lagged <- rutils::lagit(position_s)
  # Calculate daily profits and losses
  returns <- position_lagged*(closep - prices_lag)
  returns[trade_dates] <- 
    position_lagged[trade_dates] * 
    (openp[trade_dates] - prices_lag[trade_dates]) +
    position_s[trade_dates] * 
    (closep[trade_dates] - openp[trade_dates])
  output <- cbind(ewma1, ewma2, position_s, returns)
  colnames(output) <- c("ewma1", "ewma2", "positions", "returns")
  output
}  # end simu_ewma2


# Define aggregation function
agg_regate <- function(ohlc, lambdavs, ...) {
  sapply(lambdavs, function(lambdav) {
    # simulate EWMA strategy and calculate Sharpe ratio
    returns <- simu_ewma(ohlc=ohlc, lambdav=lambdav, ...)[, "returns"]
    sqrt(260)*sum(returns)/sd(returns)/NROW(returns)
  })  # end sapply
}  # end agg_regate

# Define functional for performing aggregations
roll_agg <- function(xtes, look_backs, FUN, ...) {
  # perform lapply() loop over look_backs
  agg_s <- lapply(look_backs, 
                  function(look_back) {
                    FUN(xtes[look_back], ...)
                  })  # end lapply
  # rbind list into single xts or matrix
  agg_s <- rutils::do_call_rbind(agg_s)
  if (!is.xts(agg_s))
    agg_s <- xts(agg_s, order.by=
                   index(xtes[unlist(lapply(look_backs, last))]))
  agg_s
}  # end roll_agg

