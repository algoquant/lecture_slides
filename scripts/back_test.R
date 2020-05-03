# Functions for back-testing momentum strategies

# Function back_test_momentum() performs a back-test of a momentum strategy over the end-points
back_test_momentum <- function(re_turns, 
                               perform_ance=function(re_turns) (sum(re_turns)/sd(re_turns)), 
                               look_back=12, re_balance="months", bid_offer=0.001,
                               end_points=rutils::calc_endpoints(re_turns, inter_val=re_balance), 
                               with_weights=FALSE, ...) {
  stopifnot("package:rutils" %in% search() || require("rutils", quietly=TRUE))
  # Define look-back and look-forward intervals
  n_rows <- NROW(end_points)
  start_points <- c(rep_len(1, look_back-1), end_points[1:(n_rows-look_back+1)])
  # Calculate look-back intervals
  look_backs <- cbind(start_points, end_points)
  # Calculate look-forward intervals
  look_fwds <- cbind(end_points + 1, rutils::lag_it(end_points, -1))
  look_fwds[n_rows, 1] <- end_points[n_rows]
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
}  # end back_test_momentum


# Function backtest_rolling() performs a back-test over rolling points
backtest_rolling <- function(re_turns, price_s, wid_th=252, bid_offer=0.001, tre_nd=1, ...) {
  stopifnot("package:quantmod" %in% search() || require("quantmod", quietly=TRUE))
  # Define look-back and look-forward intervals
  n_cols <- NCOL(re_turns)
  vari_ance <- roll::roll_var(re_turns, width=wid_th)
  vari_ance <- zoo::na.locf(vari_ance)
  vari_ance[is.na(vari_ance)] <- 0
  # calculate rolling Sharpe of S&P500 portfolio
  returns_width <- rutils::diff_it(log(price_s), lagg=wid_th)
  weight_s <- tre_nd*returns_width/sqrt(wid_th*vari_ance)
  weight_s[vari_ance==0] <- 0
  weight_s[1:wid_th, ] <- 1
  weight_s[is.na(weight_s)] <- 0
  weight_s <- weight_s/sqrt(rowSums(weight_s^2))
  weight_s[is.na(weight_s)] <- 0
  weight_s <- rutils::lag_it(weight_s)
  sum(is.na(weight_s))
  # calculate portfolio profits and losses
  pnl_s <- weight_s*re_turns
  # Calculate transaction costs
  bid_offer <- 0.001
  cost_s <- 0.5*bid_offer*abs(rutils::diff_it(weight_s))
  pnl_s <- (pnl_s - cost_s)
  pnl_s <- exp(cumsum(pnl_s))
  pnl_s <- rowMeans(pnl_s)
  pnl_s
}  # end backtest_rolling
