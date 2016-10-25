library(knitr)
opts_chunk$set(prompt=TRUE, eval=FALSE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
options(digits=3)
thm <- knit_theme$get("acid")
knit_theme$set(thm)
# library(HighFreq)  # load package HighFreq
# define time interval for end points
re_balance <- "weeks"
# look-back window is multiple of re_balance
win_dow <- 41
# create index of rebalancing period end points
end_points <- xts::endpoints(env_etf$re_turns,
                       on=re_balance)
# start_points are multi-period lag of end_points
len_gth <- length(end_points)
start_points <-  end_points[
  c(rep_len(1, win_dow-1),
    1:(len_gth-win_dow+1))]
# create list of look-back intervals
inter_vals <- lapply(2:len_gth,
    function(in_dex) {
start_points[in_dex]:end_points[in_dex]
  })  # end lapply
# library(HighFreq)  # load package HighFreq
# create list of symbols for model
sym_bols <- c("VTI", "IEF", "DBC")

# calculate risk&ret stats for some symbols, over a range of dates
# perform lapply() loop over inter_vals
risk_stats <- lapply(inter_vals,
  function(inter_val) {
    x_ts <-
env_etf$re_turns[inter_val, sym_bols]
    t(sapply(x_ts,
function(col_umn)
  c(return=mean(col_umn), risk=mad(col_umn))
))  # end sapply
    })  # end lapply
# rbind list into single xts or matrix
# risk_stats <- rutils::do_call_rbind(risk_stats)
# head(risk_stats)
# calculate non-overlapping returns in interval
re_turns <-sapply(2:len_gth,
    function(in_dex) {
    sapply(env_etf$re_turns[
(end_points[in_dex-1]+1):end_points[in_dex],
sym_bols], sum)
  })  # end sapply
re_turns <- t(re_turns)
# calculate list of portfolio weights
# perform lapply() loop over risk_stats
weight_s <- sapply(risk_stats,
    function(risk_stat) {
weight_s <- risk_stat[, 1]/risk_stat[, 2]
weight_s <- weight_s - mean(weight_s)
weight_s <- weight_s/sum(abs(weight_s))
    })  # end sapply
weight_s <- t(weight_s)
weights_xts <- xts(weight_s,
  order.by=index(env_etf$re_turns[end_points]))
# plot weights
x11()
zoo::plot.zoo(weights_xts, xlab=NULL)
# calculate pnls over all windows
pn_l <- rowSums(weight_s[-NROW(weight_s), ] *
            re_turns[-1, ])
# calculate transaction costs
bid_offer <- 0.001  # 10 bps for liquid ETFs
cost_s <- bid_offer*abs(diff(weight_s))
cost_s <- rowSums(cost_s)
pn_l <- pn_l - cost_s
pn_l <- xts(cumsum(pn_l),
  order.by=index(env_etf$re_turns[end_points[-(1:2)]]))
colnames(pn_l)[1] <- "pnl"
# plot strategy pnl
x11()
chart_Series(x=pn_l, name="Strategy PnL")
# calculate betas
beta_s <- c(1, env_etf$capm_stats[
  match(sym_bols[-1],
  rownames(env_etf$capm_stats)),
  "Beta"])
names(beta_s)[1] <- sym_bols[1]
# weights times betas
beta_s <- weight_s %*% beta_s
beta_s <- xts(beta_s,
  order.by=index(
    env_etf$re_turns[end_points[-1]]))
colnames(beta_s) <- "portf_beta"
x11()
plot.zoo(cbind(beta_s,
  env_etf$VTI[, 4])[index(beta_s)],
  main="betas & VTI", xlab="")
# create trading function
run_strat <- function(win_dow) {
  start_points <-  end_points[
    c(rep_len(1, win_dow-1),
1:(len_gth-win_dow+1))]
  inter_vals <- lapply(2:len_gth,
                 function(in_dex) {
                   start_points[in_dex]:end_points[in_dex]
                 })  # end lapply
  risk_stats <- lapply(inter_vals,
                 function(inter_val) {
                   x_ts <- env_etf$re_turns[inter_val, sym_bols]
                   t(sapply(x_ts,
                            function(col_umn)
                              c(return=mean(col_umn), risk=mad(col_umn))
                   ))  # end sapply
                 })  # end lapply
  weight_s <- sapply(risk_stats,
               function(risk_stat) {
                 weight_s <- risk_stat[, 1]/risk_stat[, 2]
                 weight_s <- weight_s - mean(weight_s)
                 weight_s <- weight_s/sum(abs(weight_s))
               })  # end sapply
  weight_s <- t(weight_s)
  pn_l <- rowSums(weight_s[-NROW(weight_s), ] * re_turns[-1, ])
  cost_s <- bid_offer*abs(diff(weight_s))
  cost_s <- rowSums(cost_s)
  pn_l <- pn_l - cost_s
  sum(pn_l)
}  # end run_strat
window_s <- 8*(1:7)
strat_profile <- sapply(window_s, run_strat)
plot(cbind(window_s, strat_profile), t="l",
     main="Strategy PnL as function of window size",
     xlab="window", ylab="pnl")
