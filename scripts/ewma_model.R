# Functions for simulating EWMA strategies

library(rutils)  # load package rutils

# Simulate single EWMA model using historical ohlc data,
# and return percentage returns.
sim_ewma <- function(ohlc, lambda=0.01, look_back=333, bid_offer=0.001, 
                     trend=1, lagg=1) {
  close <- log(quantmod::Cl(ohlc))
  returns <- rutils::diffit(close)
 .n_rows <- NROW(ohlc)
  # Calculate EWMA prices
  weights <- exp(-lambda*(1:look_back))
  weights <- weights/sum(weights)
  ewma <- HighFreq::roll_wsum(close, weights=weights)
  # Calculate the indicator
  indic <- trend*sign(close - ewma)
  if (lagg > 1) {
    indic <- roll::roll_sum(indic, width=lagg, min_obs=1)
    indic[1:lagg] <- 0
  }  # end if
  # Calculate positions, either: -1, 0, or 1
  pos <- rep(NA_integer_,.n_rows)
  pos[1] <- 0
  pos <- ifelse(indic == lagg, 1, pos)
  pos <- ifelse(indic == (-lagg), -1, pos)
  pos <- zoo::na.locf(pos, na.rm=FALSE)
  pos <- xts::xts(pos, order.by=index(close))
  # Lag the positions to trade on next day
  pos <- rutils::lagit(pos, lagg=1)
  # Calculate PnLs of strategy
  pnls <- returns*pos
  costs <- 0.5*bid_offer*abs(rutils::diffit(pos))*close
  pnls <- (pnls - costs)
  # Calculate strategy returns
  pnls <- cbind(pos, pnls)
  colnames(pnls) <- c("positions", "pnls")
  pnls
}  # end sim_ewma


# Simulate Dual EWMA model using historical ohlc data
sim_ewma2 <- function(ohlc, lambda1=0.1, lambda2=0.01, look_back=333, 
                      bid_offer=0.001, trend=1, lagg=1) {
  if (lambda1 >= lambda2) return(NA)
  closep <- quantmod::Cl(ohlc)
  retsp <- rutils::diffit(closep)
  nrows <- NROW(ohlc)
  # Calculate EWMA prices
  ewma1 <- HighFreq::run_mean(closep, lambda=lambda1, weights=0)
  ewma2 <- HighFreq::run_mean(closep, lambda=lambda2, weights=0)
  # Calculate positions, either: -1, 0, or 1
  indic <- sign(ewma1 - ewma2)
  if (lagg > 1) {
    indic <- roll::roll_sum(indic, width=lagg, min_obs=1)
    indic[1:lagg] <- 0
  }  # end if
  posit <- rep(NA_integer_, nrows)
  posit[1] <- 0
  posit <- ifelse(indic == lagg, 1, posit)
  posit <- ifelse(indic == (-lagg), -1, posit)
  posit <- zoo::na.locf(posit, na.rm=FALSE)
  posit <- xts::xts(posit, order.by=zoo::index(closep))
  # Lag the positions to trade on next day
  posit <- rutils::lagit(posit, lagg=1)
  # Calculate PnLs of strategy
  pnls <- retsp*posit
  costs <- 0.5*bid_offer*abs(rutils::diffit(posit))
  pnls <- (pnls - costs)
  # Calculate strategy returns
  pnls <- cbind(posit, pnls)
  colnames(pnls) <- c("positions", "pnls")
  pnls
}  # end sim_ewma2

