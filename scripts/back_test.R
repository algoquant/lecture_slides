# Functions for back-testing momentum strategies

# Function back_test_ep() performs a back-test over end-points
back_test_ep <- function(re_turns, price_s, agg_fun=sum, 
    look_back=12, re_balance="months", bid_offer=0.001,
    end_points=rutils::calc_endpoints(re_turns, inter_val=re_balance), 
    with_weights=FALSE, ...) {
  stopifnot("package:quantmod" %in% search() || require("quantmod", quietly=TRUE))
  # Define look-back and look-forward intervals
  n_col <- NCOL(re_turns)
  len_gth <- NROW(end_points)
  start_points <- c(rep_len(1, look_back-1), end_points[1:(len_gth-look_back+1)])
  fwd_points <- end_points[c(2:len_gth, len_gth)]
  # Perform loop over end-points and calculate aggregations
  agg_s <- sapply(1:(len_gth-1), function(it_er) {
    c(back_aggs=sapply(re_turns[start_points[it_er]:end_points[it_er]], agg_fun, ...),  # end sapply
    fwd_rets=sapply(re_turns[(end_points[it_er]+1):fwd_points[it_er]], sum))  # end sapply
  })  # end sapply
  agg_s <- t(agg_s)
  # Select look-back and look-forward aggregations
  back_aggs <- agg_s[, 1:n_col]
  fwd_rets <- agg_s[, n_col+1:n_col]
  # Calculate portfolio weights equal to number of shares
  end_prices <- price_s[end_points[-len_gth]]
  weight_s <- back_aggs/rowSums(abs(back_aggs))/end_prices
  weight_s[is.na(weight_s)] <- 0
  colnames(weight_s) <- colnames(re_turns)
  # Calculate profits and losses
  pnl_s <- rowSums(weight_s*fwd_rets)
  pnl_s <- xts(pnl_s, index(end_prices))
  colnames(pnl_s) <- "pnls"
  # Calculate transaction costs
  cost_s <- 0.5*bid_offer*end_prices*abs(rutils::diff_it(weight_s))
  cost_s <- rowSums(cost_s)
  pnl_s <- (pnl_s - cost_s)
  pnl_s <- cumsum(pnl_s)
  if (with_weights)
    cbind(pnl_s, weight_s)
  else
    pnl_s
}  # end back_test_ep


# Function back_test_rolling() performs a back-test over rolling points
back_test_rolling <- function(re_turns, price_s, 
                              wid_th=252, bid_offer=0.001, tre_nd=1, ...) {
  stopifnot("package:quantmod" %in% search() || require("quantmod", quietly=TRUE))
  # Define look-back and look-forward intervals
  n_col <- NCOL(re_turns)
  vari_ance <- roll::roll_var(re_turns, width=wid_th)
  vari_ance <- zoo::na.locf(vari_ance)
  vari_ance[is.na(vari_ance)] <- 0
  # calculate rolling Sharpe of S&P500 portfolio
  returns_width <- rutils::diff_it(price_s, lagg=wid_th)
  weight_s <- tre_nd*returns_width/sqrt(wid_th*vari_ance)
  weight_s[vari_ance==0] <- 0
  weight_s[1:wid_th, ] <- 1
  weight_s[is.na(weight_s)] <- 0
  weight_s <- weight_s/rowSums(abs(weight_s))/price_s
  weight_s[is.na(weight_s)] <- 0
  weight_s <- rutils::lag_it(weight_s)
  # calculate portfolio profits and losses
  pnl_s <- rowSums(weight_s*re_turns)
  # Calculate transaction costs
  bid_offer <- 0.001
  cost_s <- 0.5*bid_offer*price_s*abs(rutils::diff_it(weight_s))
  cost_s <- rowSums(cost_s)
  pnl_s <- (pnl_s - cost_s)
  pnl_s <- cumsum(pnl_s)
  pnl_s
}  # end back_test_rolling

