# Functions for back-testing momentum strategies

# Function backtest_momentum() performs a back-test of a momentum strategy over the end-points
backtest_momentum <- function(re_turns, 
                               perform_ance=function(re_turns) (sum(re_turns)/sd(re_turns)), 
                               look_back=12, re_balance="months", bid_offer=0.001,
                               end_p=rutils::calc_endpoints(re_turns, inter_val=re_balance), 
                               with_weights=FALSE, ...) {
  stopifnot("package:rutils" %in% search() || require("rutils", quietly=TRUE))
  # Define look-back and look-forward intervals
  n_rows <- NROW(end_p)
  start_p <- c(rep_len(1, look_back-1), end_p[1:(n_rows-look_back+1)])
  # Calculate look-back intervals
  look_backs <- cbind(start_p, end_p)
  # Calculate look-forward intervals
  look_fwds <- cbind(end_p + 1, rutils::lag_it(end_p, -1))
  look_fwds[n_rows, 1] <- end_p[n_rows]
  # Calculate past performance over look-back intervals
  pas_t <- t(apply(look_backs, 1, function(ep) sapply(re_turns[ep[1]:ep[2]], perform_ance)))
  pas_t[is.na(pas_t)] <- 0
  # Calculate future performance
  fu_ture <- t(apply(look_fwds, 1, function(ep) sapply(re_turns[ep[1]:ep[2]], sum)))
  fu_ture[is.na(fu_ture)] <- 0
  # Scale weight_s so sum of squares is equal to 1
  weight_s <- pas_t
  weight_s <- weight_s/sqrt(rowSums(weight_s^2))
  weight_s[is.na(weight_s)] <- 0  # Set NA values to zero
  # Calculate momentum profits and losses
  pnl_s <- rowSums(weight_s*fu_ture)
  # Calculate transaction costs
  cost_s <- 0.5*bid_offer*cumprod(1 + pnl_s)*rowSums(abs(rutils::diff_it(weight_s)))
  pnl_s <- (pnl_s - cost_s)
  if (with_weights)
    rutils::lag_it(cbind(pnl_s, weight_s))
  else
    rutils::lag_it(pnl_s)
}  # end backtest_momentum


# Function momentum_daily() performs a back-test of a daily momentum strategy
# If tre_nd=(-1) then it backtests a mean reverting strategy
momentum_daily <- function(re_turns, look_back=252, bid_offer=0.001, tre_nd=1, ...) {
  stopifnot("package:quantmod" %in% search() || require("quantmod", quietly=TRUE))
  # Calculate rolling variance
  vari_ance <- roll::roll_var(re_turns, width=look_back)
  vari_ance <- zoo::na.locf(vari_ance, na.rm=FALSE)
  vari_ance[is.na(vari_ance)] <- 0
  # Calculate rolling Sharpe
  pas_t <- roll::roll_mean(re_turns, width=look_back)
  weight_s <- pas_t/sqrt(vari_ance)
  weight_s[vari_ance == 0] <- 0
  weight_s[1:look_back, ] <- 1
  weight_s <- weight_s/sqrt(rowSums(weight_s^2))
  weight_s[is.na(weight_s)] <- 0
  weight_s <- rutils::lag_it(weight_s)
  # Calculate momentum profits and losses
  pnl_s <- tre_nd*rowMeans(weight_s*re_turns)
  # Calculate transaction costs
  cost_s <- 0.5*bid_offer*rowSums(abs(rutils::diff_it(weight_s)))
  cumprod(1 + pnl_s - cost_s)
}  # end momentum_daily
