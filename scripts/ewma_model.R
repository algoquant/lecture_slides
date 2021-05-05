# Functions for simulating EWMA strategies

library(rutils)  # load package rutils

# Simulate single EWMA model using historical oh_lc data,
# and return percentage returns.
simu_ewma <- function(oh_lc, lamb_da=0.01, wid_th=351, bid_offer=0.001, tre_nd=1) {
  n_rows <- NROW(oh_lc)
  # Calculate EWMA prices
  weight_s <- exp(-lamb_da*1:wid_th)
  weight_s <- weight_s/sum(weight_s)
  clos_e <- quantmod::Cl(oh_lc)
  ew_ma <- .Call(stats:::C_cfilter, clos_e, filter=weight_s, sides=1, circular=FALSE)
  ew_ma[1:(wid_th-1)] <- ew_ma[wid_th]
  # Determine trade dates right after EWMA has crossed prices
  in_dic <- tre_nd*sign(clos_e - ew_ma)
  trade_dates <- (rutils::diff_it(in_dic) != 0)
  trade_dates <- which(trade_dates) + 1
  trade_dates <- trade_dates[trade_dates < n_rows]
  # Calculate positions, either: -1, 0, or 1
  position_s <- rep(NA_integer_, n_rows)
  position_s[1] <- 0
  position_s[trade_dates] <- in_dic[trade_dates-1]
  position_s <- zoo::na.locf(position_s, na.rm=FALSE)
  op_en <- quantmod::Op(oh_lc)
  close_lag <- rutils::lag_it(clos_e)
  pos_lagged <- rutils::lag_it(position_s)
  # Calculate daily profits and losses
  pnl_s <- rutils::diff_it(clos_e)*position_s
  pnl_s[trade_dates] <- pos_lagged[trade_dates]*
    (op_en[trade_dates] - close_lag[trade_dates])
  pnl_s[trade_dates] <- pnl_s[trade_dates] +
    position_s[trade_dates]*
    (clos_e[trade_dates] - op_en[trade_dates])
  # Calculate transaction costs
  cost_s <- 0.5*bid_offer*abs(pos_lagged - position_s)*clos_e
  pnl_s <- (pnl_s - cost_s)
  # Calculate strategy returns
  pnl_s <- cbind(position_s, pnl_s)
  colnames(pnl_s) <- c("positions", "pnls")
  pnl_s
}  # end simu_ewma


# Needs update to reflect changes in simu_ewma()
# Simulate two EWMA model using historical oh_lc data
simu_ewma2 <- function(oh_lc, lambda_1=0.25, lambda_2=0.05, wid_th=51) {
  # calculate EWMA prices
  weights_1 <- exp(-lambda_1*1:wid_th)
  weights_1 <- weights_1/sum(weights_1)
  weights_2 <- exp(-lambda_2*1:wid_th)
  weights_2 <- weights_2/sum(weights_2)
  # calculate open and close prices
  op_en <- Op(oh_lc)
  clos_e <- Cl(oh_lc)
  # adjust close price to start at zero
  op_en <- op_en - as.numeric(clos_e[1, ])
  clos_e <- clos_e - as.numeric(clos_e[1, ])
  prices_lag <- rutils::lag_it(clos_e)
  # filter the prices using weights
  ewma_1 <- filter(clos_e, filter=weights_1, sides=1)
  ewma_1[1:(wid_th-1)] <- ewma_1[wid_th]
  ewma_2 <- filter(clos_e, filter=weights_2, sides=1)
  ewma_2[1:(wid_th-1)] <- ewma_2[wid_th]
  # determine dates right after EWMAs have crossed
  in_dic <- xts(sign(ewma_1 - ewma_2), order.by=index(oh_lc))
  trade_dates <- (rutils::diff_it(in_dic) != 0)
  trade_dates <- which(trade_dates) + 1
  trade_dates <- trade_dates[trade_dates<NROW(oh_lc)]
  # calculate positions, either: -1, 0, or 1
  position_s <- rep(NA_integer_, NROW(clos_e))
  position_s[1] <- 0
  position_s[trade_dates] <- rutils::lag_it(in_dic)[trade_dates]
  position_s <- xts(na.locf(position_s), order.by=index(oh_lc))
  position_lagged <- rutils::lag_it(position_s)
  # calculate daily profits and losses
  re_turns <- position_lagged*(clos_e - prices_lag)
  re_turns[trade_dates] <- 
    position_lagged[trade_dates] * 
    (op_en[trade_dates] - prices_lag[trade_dates]) +
    position_s[trade_dates] * 
    (clos_e[trade_dates] - op_en[trade_dates])
  out_put <- cbind(ewma_1, ewma_2, position_s, re_turns)
  colnames(out_put) <- c("ewma_1", "ewma_2", "positions", "returns")
  out_put
}  # end simu_ewma2


# define aggregation function
agg_regate <- function(oh_lc, lamb_das, ...) {
  sapply(lamb_das, function(lamb_da) {
    # simulate EWMA strategy and calculate Sharpe ratio
    re_turns <- simu_ewma(oh_lc=oh_lc, lamb_da=lamb_da, ...)[, "returns"]
    sqrt(260)*sum(re_turns)/sd(re_turns)/NROW(re_turns)
  })  # end sapply
}  # end agg_regate

# define functional for performing aggregations
roll_agg <- function(x_ts, look_backs, FUN, ...) {
  # perform lapply() loop over look_backs
  agg_s <- lapply(look_backs, 
                  function(look_back) {
                    FUN(x_ts[look_back], ...)
                  })  # end lapply
  # rbind list into single xts or matrix
  agg_s <- rutils::do_call_rbind(agg_s)
  if (!is.xts(agg_s))
    agg_s <- xts(agg_s, order.by=
                   index(x_ts[unlist(lapply(look_backs, last))]))
  agg_s
}  # end roll_agg

