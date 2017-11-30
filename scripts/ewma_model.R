# Functions for simulating EWMA strategies

# library(HighFreq)  # load package HighFreq

# simulate single EWMA model using historical oh_lc data
simu_ewma <- function(oh_lc, lamb_da=0.05, wid_th=51) {
  # calculate EWMA prices
  weight_s <- exp(-lamb_da*1:wid_th)
  weight_s <- weight_s/sum(weight_s)
  cl_ose <- quantmod::Cl(oh_lc)
  ew_ma <- filter(as.numeric(cl_ose), filter=weight_s, sides=1)
  ew_ma[1:(wid_th-1)] <- ew_ma[wid_th]
  # determine dates right after EWMA has crossed prices
  in_dic <- xts::xts(sign(as.numeric(cl_ose) - ew_ma), order.by=index(oh_lc))
  trade_dates <- (rutils::diff_xts(in_dic) != 0)
  trade_dates <- which(trade_dates) + 1
  trade_dates <- trade_dates[trade_dates<NROW(oh_lc)]
  # calculate positions, either: -1, 0, or 1
  po_sitions <- rep(NA_integer_, NROW(cl_ose))
  po_sitions[1] <- 0
  po_sitions[trade_dates] <- rutils::lag_xts(in_dic)[trade_dates]
  po_sitions <- xts::xts(na.locf(po_sitions), order.by=index(oh_lc))
  op_en <- quantmod::Op(oh_lc)
  prices_lag <- rutils::lag_xts(cl_ose)
  position_lagged <- rutils::lag_xts(po_sitions)
  # calculate daily profits and losses
  re_turns <- position_lagged*(cl_ose - prices_lag)
  re_turns[trade_dates] <- 
    position_lagged[trade_dates] * 
    (op_en[trade_dates] - prices_lag[trade_dates]) +
    po_sitions[trade_dates] * 
    (cl_ose[trade_dates] - op_en[trade_dates])
  out_put <- cbind(po_sitions, re_turns)
  colnames(out_put) <- c("po_sitions", "re_turns")
  out_put
}  # end simu_ewma


# simulate two EWMA model using historical oh_lc data
simu_ewma2 <- function(oh_lc, lambda_1=0.25, lambda_2=0.05, wid_th=51) {
  # calculate EWMA prices
  weights_1 <- exp(-lambda_1*1:wid_th)
  weights_1 <- weights_1/sum(weights_1)
  weights_2 <- exp(-lambda_2*1:wid_th)
  weights_2 <- weights_2/sum(weights_2)
  # calculate open and close prices
  op_en <- Op(oh_lc)
  cl_ose <- Cl(oh_lc)
  # adjust close price to start at zero
  op_en <- op_en - as.numeric(cl_ose[1, ])
  cl_ose <- cl_ose - as.numeric(cl_ose[1, ])
  prices_lag <- rutils::lag_xts(cl_ose)
  # filter the prices using weights
  ewma_1 <- filter(cl_ose, filter=weights_1, sides=1)
  ewma_1[1:(wid_th-1)] <- ewma_1[wid_th]
  ewma_2 <- filter(cl_ose, filter=weights_2, sides=1)
  ewma_2[1:(wid_th-1)] <- ewma_2[wid_th]
  # determine dates right after EWMAs have crossed
  in_dic <- xts(sign(ewma_1 - ewma_2), order.by=index(oh_lc))
  trade_dates <- (rutils::diff_xts(in_dic) != 0)
  trade_dates <- which(trade_dates) + 1
  trade_dates <- trade_dates[trade_dates<NROW(oh_lc)]
  # calculate positions, either: -1, 0, or 1
  po_sitions <- rep(NA_integer_, NROW(cl_ose))
  po_sitions[1] <- 0
  po_sitions[trade_dates] <- rutils::lag_xts(in_dic)[trade_dates]
  po_sitions <- xts(na.locf(po_sitions), order.by=index(oh_lc))
  position_lagged <- rutils::lag_xts(po_sitions)
  # calculate daily profits and losses
  re_turns <- position_lagged*(cl_ose - prices_lag)
  re_turns[trade_dates] <- 
    position_lagged[trade_dates] * 
    (op_en[trade_dates] - prices_lag[trade_dates]) +
    po_sitions[trade_dates] * 
    (cl_ose[trade_dates] - op_en[trade_dates])
  out_put <- cbind(ewma_1, ewma_2, po_sitions, re_turns)
  colnames(out_put) <- c("ewma_1", "ewma_2", "po_sitions", "re_turns")
  out_put
}  # end simu_ewma2


# define aggregation function
agg_regate <- function(oh_lc, lamb_das, ...) {
  sapply(lamb_das, function(lamb_da) {
    # simulate EWMA strategy and calculate Sharpe ratio
    re_turns <- simu_ewma(oh_lc=oh_lc, lamb_da=lamb_da, ...)[, "re_turns"]
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

