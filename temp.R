rm(list=ls())
options(max.print=80)
options(xts_check_TZ=FALSE)

set.seed(1121)
sapply(-1:1, rnorm, n=3)

####

# daily data
ye_ars <- format(
  index(sk_ew[endpoints(sk_ew, on="years"), ]), 
  format="%Y")
sapply(ye_ars, function(ye_ar) sum(vol_at[ye_ar]))
sapply(ye_ars, function(ye_ar) sum(sk_ew[ye_ar]))

foo <- format(index(daily_skew[which.max(daily_skew)]), "%Y-%m-%d")

foo <- which.max(daily_skew)
foo <- which.min(daily_skew)
foo <- format(index(daily_skew[(foo-1):(foo+1), ]), "%Y-%m-%d")

chart_Series(get(sym_bol)[foo], name=paste(sym_bol, "skew"))


# daily returns
daily_rets <- Cl(get(sym_bol)[index(daily_skew), ])
daily_rets <- diff(log(daily_rets))
daily_rets[1, ] <- daily_rets[2, ]
colnames(daily_rets) <- paste(sym_bol, "rets", sep=".")
head(daily_rets)
tail(daily_rets)

date_s <- "2008-09/2009-05"
# daily_rets and sk_ew
bar <- cbind(coredata(daily_rets), coredata(daily_skew))
# daily_rets and lagged sk_ew
bar <- cbind(coredata(daily_rets), c(0, coredata(daily_skew)[-length(daily_skew)]))

head(bar)
dim(bar)
apply(bar, 2, mad)
ma_d <- mad(bar[, 2])
blah <- (abs(bar[, 2]-mean(bar[, 2])) > 5*ma_d)
length(blah)
sum(blah)
bar <- bar[!blah, ]


### returns

# lag_rets equals returns lagged by -1
re_turns <- calc_rets(xts_data=get(sym_bol))
lag_rets <- re_turns[, 1]
lag_rets <- c(lag_rets[-1, ], lag_rets[length(lag_rets)])
tail(lag_rets)

sk_ew <- skew_ohlc(log_ohlc=log_ohlc)
colnames(sk_ew) <- 
  paste(sym_bol, "skew", sep=".")
lag_skew <- lag(sk_ew)


win_dow <- 2*60*6.5 + 101

# calc mad_var
mad_var <- runmad(coredata(vari_ance), k=win_dow)
# lag mad_var
mad_var <- c(rep(0, (win_dow-1)/2), mad_var[-((length(mad_var)-(win_dow-1)/2+1):(length(mad_var)))])
length(mad_var)
head(mad_var)
tail(mad_var)
# calc mad_skew
mad_skew <- runmad(coredata(sk_ew), k=win_dow)
# lag mad_skew
mad_skew <- c(rep(0, (win_dow-1)/2), mad_skew[-((length(mad_skew)-(win_dow-1)/2+1):(length(mad_skew)))])
plot(mad_skew[(length(mad_skew)-100*win_dow):length(mad_skew)], t="l", xlab="", ylab="", main="mad_skew")
# calc mad_volu
quan_tiles <- c("0.5"=0.5, "0.75"=0.75, "0.85"=0.85, "0.95"=0.95)
mad_volu <- runquantile(coredata(Vo(get(sym_bol))), probs=quan_tiles, k=win_dow)
mad_volu <- mad_volu[, 1, ]
# lag mad_volu
mad_volu <- rbind(
  matrix(numeric(ncol(mad_volu)*(win_dow-1)/2), ncol=ncol(mad_volu)), 
  mad_volu[-((nrow(mad_volu)-(win_dow-1)/2+1):(nrow(mad_volu))), ])
colnames(mad_volu) <- names(quan_tiles)
mad_volu <- xts(mad_volu, order.by=index(get(sym_bol)))
# plot(mad_volu[(nrow(mad_volu)-100*win_dow):nrow(mad_volu[,]), 4], t="l", xlab="", ylab="", main="mad_volu")
chart_Series(mad_volu[(nrow(mad_volu)-100*win_dow):nrow(mad_volu[,]), 4], name=paste(sym_bol, "mad_volu"))
# plot volume spikes above 85% quantile
date_s <- (nrow(mad_volu)-4*win_dow):nrow(mad_volu[,])
chart_Series(mad_volu[date_s, 3], name=paste(sym_bol, "mad_volu"))
chart_Series(Vo(get(sym_bol)[date_s]) - mad_volu[date_s, 4], name=paste(sym_bol, "volume spikes"))
chart_Series(Cl(get(sym_bol)[date_s]), name=paste(sym_bol, "prices"))


# signal threshold trading level
pos_skew <- coredata(ifelse(sk_ew > 5*mad_skew, 1, 0))
colnames(pos_skew) <- paste(sym_bol, "p_skew", sep=".")
neg_skew <- coredata(ifelse(sk_ew < -5*mad_skew, -1, 0))
colnames(neg_skew) <- paste(sym_bol, "n_skew", sep=".")
c(pos_skew=sum(pos_skew)/length(pos_skew), neg_skew=-sum(neg_skew)/length(neg_skew))
plot(pos_skew)

spike_skew <- coredata(Vo(get(sym_bol)) - mad_volu[, 4] > 0, sign(sk_ew), 0)
colnames(spike_skew) <- paste(sym_bol, "spike_skew", sep=".")

run_var <- runSum(vari_ance, n=win_dow)
run_var[1:(win_dow-1)] <- 0
colnames(run_var) <- colnames(vari_ance)
head(run_var)

chart_Series(run_var[date_s], 
             name=paste(sym_bol, "volatility"))

run_skew <- runSum(sk_ew, n=win_dow)
run_skew[1:(win_dow-1)] <- 0
colnames(run_skew) <- colnames(sk_ew)
head(run_skew)

chart_Series(run_skew[date_s], 
             name=paste(sym_bol, "skew"))

win_short <- 70
win_long <- 225
vwap_short <- v_wap(x_ts=get(sym_bol), win_dow=win_short)
vwap_long <- v_wap(x_ts=get(sym_bol), win_dow=win_long)
head(vwap_short)
head(vwap_long)
vwap_diff <- vwap_short - vwap_long
colnames(vwap_diff) <- paste(sym_bol, "vwap", sep=".")
vwap_diff <- na.locf(vwap_diff)


### data: lagged returns plus explanatory variables

# for lm reg
# bar <- cbind(lag_rets, coredata(re_turns[, 1]), pos_skew, neg_skew)
# bar <- cbind(lag_rets, coredata(vwap_diff), pos_skew, neg_skew)
bar <- cbind(re_turns[, 1], lag_skew)
bar <- cbind(lag_rets, sign(coredata(vwap_diff)), pos_skew, neg_skew)
# bar <- cbind(sign(lag_rets), sign(coredata(vwap_diff)), pos_skew, neg_skew)
# for logistic reg
bar <- cbind((sign(coredata(lag_rets))+1)/2, sign(coredata(vwap_diff)), pos_skew, neg_skew)
# for lda qda
bar <- cbind(sign(lag_rets), coredata(vwap_diff), pos_skew, neg_skew)
# colnames(bar) <- c("SPY.lagrets", "SPY.rets", "SPY.poskew", "SPY.negskew")
class(bar)
tail(bar)


### lm

# lm formula with zero intercept
for_mula <- as.formula(paste(colnames(bar)[1], paste(paste(colnames(bar)[-1], collapse=" + "), "- 1"), sep="~"))
for_mula <- as.formula(paste(colnames(bar)[1], paste(colnames(bar)[2], "- 1"), sep="~"))

l_m <- lm(for_mula, data=as.data.frame(bar))
# perform regressions over different calendar periods
l_m <- lm(for_mula, data=as.data.frame(bar["2011-01-01/"]))
l_m <- lm(for_mula, data=as.data.frame(bar["/2011-01-01"]))
lm_summ <- summary(l_m)
l_m <- lm(for_mula, data=as.data.frame(bar["2013-02-04/2013-03-05"]))
lm_summ <- summary(l_m)
lm_predict <- predict(l_m, newdata=as.data.frame(bar["2013-03-06"]))
foo <- data.frame(sign(lm_predict), coredata(bar["2013-03-06", 1]))
colnames(foo) <- c("lm_pred", "realized")
table(foo)
cumu_pnl <- cumsum(sign(lm_predict)*re_turns["2013-03-06", 1])
last(cumu_pnl)
chart_Series(cumu_pnl, name=paste(sym_bol, "optim_rets"))

# loop over thresholds and return regression t-values
foo <- sapply(structure(2:10, paste0("thresh", names=2:10)), function(thresh_old) {
  pos_skew <- coredata(ifelse(sk_ew > thresh_old*mad_skew, 1, 0))
  colnames(pos_skew) <- paste(sym_bol, "p_skew", sep=".")
  neg_skew <- coredata(ifelse(sk_ew < -thresh_old*mad_skew, -1, 0))
  colnames(neg_skew) <- paste(sym_bol, "n_skew", sep=".")
  bar <- cbind(sign(lag_rets), sign(coredata(vwap_diff)), pos_skew, neg_skew)
  l_m <- lm(for_mula, data=as.data.frame(bar))
  lm_summ <- summary(l_m)
  lm_summ$coefficients[, "t value"]
}, USE.NAMES=TRUE)  # end sapply


# loop over periods
date_s <- "2013-06-01/"
date_s <- "2008-06-01/2009-06-01"
end_points <- endpoints(get(sym_bol)[date_s], on="days")
end_points <- format(index((get(sym_bol)[date_s])[end_points[-1], ]), "%Y-%m-%d")
win_dow <- 10

position_s <- 
  lapply(win_dow:length(end_points),
         function(end_point) {
           date_s <- paste0(end_points[end_point-win_dow+1], "/", end_points[end_point-1])
           l_m <- lm(for_mula, data=as.data.frame(bar[date_s]))
           da_ta <- bar[end_points[end_point]]
           xts(x=predict(l_m, newdata=as.data.frame(da_ta)), order.by=index(da_ta))
         }  # end anon function
  )  # end lapply
position_s <- do.call(rbind, position_s)
chart_Series(position_s, name=paste(sym_bol, "optim_rets"))

cumu_pnl <- cumsum(sign(position_s)*re_turns[index(position_s), 1])
last(cumu_pnl)
chart_Series(cumu_pnl, name=paste(sym_bol, "optim_rets"))


### logistic reg
library(MASS)
library(ISLR)
library(glmnet)
g_lm <- glm(for_mula, data=as.data.frame(bar), family=binomial)
summary(g_lm)


### lda
l_da <- lda(for_mula, data=as.data.frame(bar))
summary(l_da)
l_da <- lda(for_mula, data=as.data.frame(bar["2013-02-04/2013-03-05"]))
lda_predict <- predict(l_da, newdata=as.data.frame(bar["2013-03-06"]))
foo <- data.frame(lda_predict$class, coredata(bar["2013-03-06", 1]))
colnames(foo) <- c("lda_pred", "realized")
table(foo)


### qda
q_da <- qda(for_mula, data=as.data.frame(bar))
summary(q_da)
date_s <- "2013-02-04/2013-02-06"
q_da <- qda(for_mula, data=as.data.frame(bar["2013-02-04/2013-03-05"]))
date_s <- "2013-02-07"
qda_predict <- predict(q_da, newdata=as.data.frame(bar["2013-03-06"]))
str(qda_predict)
head(qda_predict$class)
tail(qda_predict$class)
length(qda_predict$class)
sum(qda_predict$class!=1)
sum(bar["2013-02-07", 1]!=1)
foo <- data.frame(qda_predict$class, coredata(bar["2013-03-06", 1]))
colnames(foo) <- c("qda_pred", "realized")
table(foo)

# scatterplot of sk_ew and daily_rets
plot(for_mula, data=bar, xlab="skew", ylab="rets")
abline(l_m, col="blue")

cor.test(formula=as.formula(paste("~", paste(colnames(bar), collapse=" + "))), data=as.data.frame(bar))


date_s <- "2013-06-01/"
bar <- cbind(
  coredata(re_turns[date_s, 1]), 
  c(0, coredata(run_skew[date_s])[-nrow(run_skew[date_s])]))


# running strategy

# multiply matrix columns
foo <- t(t(coredata(bar[, -1]))*coef(l_m)[-1])
dim(foo)
tail(foo)
apply(foo, MARGIN=2, sum)

# thresh_old <- 2*mad(run_skew)  # signal threshold trading level
# position_s <- NA*numeric(nrow(sk_ew))
position_s <- ifelse((pos_skew!=0) | (neg_skew!=0), 1, sign(coredata(vwap_diff)))
position_s <- ifelse((pos_skew!=0) | (neg_skew!=0), 1, -coredata(re_turns[, 1]))
position_s <- ifelse((pos_skew!=0) | (neg_skew!=0), 1, sign(coredata(vwap_diff)))
position_s <- pos_skew + neg_skew + sign(coredata(vwap_diff))
position_s <- -sign(sk_ew) + sign(coredata(vwap_diff))
position_s <- coredata(bar[, -1]) %*% coef(l_m)
sum(is.na(position_s))
length(position_s)
head(position_s)
plot(position_s[(length(position_s)-100*win_dow):length(position_s)], t="l", xlab="", ylab="", main="position_s")
plot(position_s, t="l", ylim=c(0, 0.001))

position_s <- ifelse(run_skew>thresh_old, -1, position_s)
position_s <- ifelse(run_skew<(-thresh_old), 1, position_s)
position_s <- ifelse((run_skew*lag(run_skew))<0, 0, position_s)
# lag the position_s
lag_positions <- c(0, position_s[-length(position_s)])
lag_positions <- na.locf(lag_positions)
lag_positions <- merge(run_skew, lag_positions)
colnames(lag_positions)[2] <- 
  paste0(sym_bol, ".Position")
# cumulative PnL
cumu_pnl <- cumsum(lag_positions*re_turns[, 1])
last(cumu_pnl)
# cumu_pnl <- cumsum(lag_positions[, 2]*re_turns[, 1])
plot.zoo(cumu_pnl)
chart_Series(cumu_pnl, name=paste(sym_bol, "pnl"))

foo <- run_sum(abs(sign(sk_ew)-sign(lag_skew)), win_dow=1000)
chart_Series(
  foo[endpoints(foo, on="days"), ], 
  name=paste(sym_bol, "contrarian skew strategy frequency of trades"))
# calculate transaction costs
bid_offer <- 0.001  # 10 bps for liquid ETFs
cost_s <- bid_offer*abs(position_s-lag_positions)
pnl_xts[, "pnl"] <- pnl_xts[, "pnl"] - co_sts


### optimize vwap

run_vwap <- function(win_short=10, win_long=100, price_s, re_turns) {
  vwap_short <- coredata(v_wap(x_ts=price_s, win_dow=win_short))
  vwap_long <- coredata(v_wap(x_ts=price_s, win_dow=win_long))
# lag the position_s
  position_s <- sign(vwap_short - vwap_long)
  position_s <- c(0, position_s[-length(position_s)])
  sum(position_s*re_turns)
}  # end run_vwap

run_vwap(price_s=get(sym_bol), re_turns=re_turns[, 1])


short_windows <- seq(from=30, to=100, by=10)
names(short_windows) <- paste0("sh", short_windows)
long_windows <- seq(from=200, to=400, by=25)
names(long_windows) <- paste0("lo", long_windows)

mat_rix <- sapply(short_windows,
                  function(win_short, ...)
                    sapply(long_windows,
                           run_vwap,
                           win_short=win_short, ...),
                  price_s=get(sym_bol), re_turns=re_turns[, 1])

# load rgl
library(rgl)
persp3d(z=mat_rix, col="green", x=short_windows, y=long_windows)


####


# seconds index
in_dex <- as.POSIXct("2015-01-01 00:00:00") + 0:1000
in_dex <- seq(from=as.POSIXct("2015-01-01 00:00:00"), 
              to=as.POSIXct("2015-01-03 00:00:00"), by="sec")
head(in_dex)
tail(in_dex)
length(in_dex)

# simulate lognormal prices
foo <- xts(exp(cumsum(rnorm(length(in_dex)))/100), order.by=in_dex)
dim(foo)

# aggregate minutes OHLC bars
bar <- to.period(x=foo, period="minutes", name="synth")
tail(bar)
# OHLC candlechart
chart_Series(x=bar["2015-01-01 01:00:00/2015-01-01 05:00:00"], 
             name="OHLC candlechart")

# running volatility
vol_at <- run_moment_ohlc(ohlc=bar, n=1000, vo_lu=FALSE)
head(vol_at)
tail(vol_at)
# running skew
sk_ew <- run_moment_ohlc(ohlc=bar, mom_fun="skew_ohlc", n=1000, vo_lu=FALSE)
sk_ew <- sk_ew/(vol_at)^(1.5)
sk_ew[1, ] <- 0
sk_ew <- na.locf(sk_ew)
chart_Series(x=vol_at, name="volatility")
chart_Series(x=sk_ew, name="skew")


####

mat_rix <- matrix(1:6, ncol=2)

foo <- etf_rets[, sym_bols]
head(foo)
nrow(etf_rets)

foo <- xts(matrix(rnorm(3*nrow(etf_rets)), ncol=3), order.by=index(etf_rets))

colnames(foo) <- colnames(etf_rets[, sym_bols])
head(foo)

ann_weights <- sapply(2:length(end_points), 
                      function(in_dex) {
                        optim_portf(
                          portf_rets=foo, 
                          start_point=end_points[in_dex-1], 
                          end_point=end_points[in_dex])
                      }  # end anon function
)  # end sapply


colnames(ann_weights) <- format(index(foo[end_points[-1]]), "%Y")

ann_weights <- t(ann_weights)


bar <- lapply(3:length(end_points),
              function(in_dex) {
                foo[end_points[in_dex-1]:end_points[in_dex], ] %*% 
                  c(1, ann_weights[in_dex-2, ])
              }  # end anon function
)  # end lapply

bar <- do.call(rbind, bar)

plot(cumsum(bar), t="l")


###

class(list_capm)
list_capm[[1]]
class(list_capm[[1]])
dim(list_capm)


etf_series_ad <- do.call(merge, eapply(env_data, Ad))

# "etf_series_ad" should be an xts series containing adjusted prices,
# with colnames in the format "name.Adjusted",
# rename the colnames and drop ".Adjusted" from the colnames,
# use functions sapply() and strsplit(),
colnames(etf_series_ad) <- sapply(colnames(etf_series_ad), 
                                  function(col_name) 
                                    strsplit(col_name, split="[.]")[[1]])[1, ]


# scrub (remove) rows with NA values from "etf_series_ad",
# use function complete.cases(),
etf_series_ad <- etf_series_ad[complete.cases(etf_series_ad)]


# calculate an xts series containing returns of "etf_series_ad", and call it "etf_rets",
# use functions lapply(), dailyReturn(), do.call(), and merge(),
etf_rets <- lapply(etf_series_ad, 
                   function(x_ts) {
                     daily_return <- dailyReturn(x_ts)
                     colnames(daily_return) <- names(x_ts)
                     daily_return
                   })  # end lapply

# flatten list of xts series into a single xts series,
etf_rets <- do.call(merge, etf_rets)

# rearrange columns according to ETF symbols for asset allocation
sym_bols <- c("VTI", "VEU", "IEF", "VNQ", 
              "DBC", "XLY", "XLP", "XLE", "XLF", "XLV", 
              "XLI", "XLB", "XLK", "XLU", "IWB", "IWD", 
              "IWF", "IWM", "IWN", "IWO", "IWP", "IWR", 
              "IWS", "IWV", "IUSV", "IUSG")
etf_rets <- etf_rets[, sym_bols]


# Extract the numeric year from each element of the date index of "etf_rets"
# calculate a numeric vector of years from the date index of "etf_rets", 
# and call it "ye_ars",
# you can use either functions format() and as.numeric(), 
# or function year() from package lubridate,
ye_ars <- as.numeric(format(index(etf_rets), "%Y"))
ye_ars <- year(index(etf_rets))


# Calculate a matrix containing the annualized alpha for each ETF in each year, 
# and call it "ann_alphas"
# the matrix "ann_alphas" should have rows corresponding to ETF names, 
# and columns corresponding to years,
# assign row and column names from colnames of "etf_rets",
# use functions sapply(), unique(), and either CAPM.alpha() 
# or table.CAPM() from package PerformanceAnalytics,
# the function unique() calculates a vector of unique elements of an object,
# and can be used to extract unique years from "ye_ars",
# annualize the alphas by multiplying them by the average number 
# of business days in each year (250),

# first method, using CAPM.alpha(),
ann_alphas <- 250*sapply(unique(ye_ars), function(ye_ar) {
  in_dex <- (ye_ars==ye_ar)
  CAPM.alpha(Ra=etf_rets[in_dex, -1], 
             Rb=etf_rets[in_dex, "VTI"])
})


# second method, using table.CAPM(),
ann_alphas <- sapply(unique(ye_ars), function(ye_ar) {
  in_dex <- (ye_ars==ye_ar)
  etf_perf_stats <- table.CAPM(Ra=etf_rets[in_dex, -1], 
                               Rb=etf_rets[in_dex, "VTI"], 
                               scale=250)
  as.numeric(etf_perf_stats["Annualized Alpha", ])
})


# assign row and column names,
rownames(ann_alphas) <- colnames(etf_rets)[-1]
colnames(ann_alphas) <- unique(ye_ars)


###

list_names <- as.list(paste0("list", 1:4))
names(list_names) <- list_names

list_vec <- lapply(
  list_names, 
  function(name) structure(rnorm(10), names=paste0("el", 1:10))
  )  # end lapply

list_vec[[2]]

mat_rix <- do.call(cbind, list_vec)


###


# count NAs values
sapply(airquality, function(col_umn) sum(is.na(col_umn)))

# randomly replace values with NAs
student_scores[, -(1:2)] <- sapply(student_scores[, -(1:2)], function(col_umn) {
  is_na <- sample(1:20, size=sample(1:4, size=1))
  col_umn[is_na] <- NA
  col_umn
})  # end sapply



###

# sprintf scripts
# A wrapper for the C function sprintf, that returns a character vector containing a formatted combination of text and variable values.
# sprintf {base}	R Documentation
# Use C-style String Formatting Commands

sprintf(fmt="%f", foo[1])

## use a literal % :

sprintf("%.0f%% said yes (out of a sample of size %.0f)", 66.666, 3)

## various formats of pi :
## re-use one argument three times, show difference between %x and %X
xx <- sprintf("%1$d %1$x %1$X", 0:15)
xx <- matrix(xx, dimnames = list(rep("", 16), "%d%x%X"))
noquote(format(xx, justify = "right"))

## More sophisticated:

sprintf("min 10-char string '%10s'",
        c("a", "ABC", "and an even longer one"))

## Platform-dependent bad example from qdapTools 1.0.0:
## may pad with spaces or zeroes.
sprintf("%09s", month.name)

n <- 1:18
sprintf(paste0("e with %2d digits = %.", n, "g"), n, exp(1))

## Using arguments out of order
sprintf("second %2$1.0f, first %1$5.2f, third %3$1.0f", pi, 2, 3)

## Using asterisk for width or precision
sprintf("precision %.*f, width '%*.3f'", 3, pi, 8, pi)

## Asterisk and argument re-use, 'e' example reiterated:
sprintf("e with %1$2d digits = %2$.*1$g", n, exp(1))

## re-cycle arguments
sprintf("%s %d", "test", 1:3)

## binary output showing rounding/representation errors
x <- seq(0, 1.0, 0.1); y <- c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1)
cbind(x, sprintf("%a", x), sprintf("%a", y))



###


trade_prices <- NA*numeric(length=nrow(env_data$VTI))
trade_prices[1] <- Op(env_data$VTI[1, ])
trade_prices[trade_dates] <- Op(env_data$VTI[trade_dates, ])
trade_prices <- na.locf(trade_prices)
head(trade_prices)
tail(trade_prices)

pnl_unreal <- pos_ition*(Ad(env_data$VTI) - trade_prices)
head(pnl_unreal)
tail(pnl_unreal)
plot.zoo(pnl_unreal)

pnl_real <- numeric(length=nrow(env_data$VTI))
lag_trade_dates <- c(1, trade_dates[-length(trade_dates)])
# or
lag_trade_dates <- c(1, trade_dates)[seq_along(trade_dates)]
pnl_real[trade_dates] <- pos_ition[lag_trade_dates]*
  (trade_prices[trade_dates] - trade_prices[lag_trade_dates])
pnl_real <- cumsum(pnl_real)
# pnl_real <- na.locf(pnl_real)
plot.zoo(pnl_real)
pn_l <- xts(pnl_real + pnl_unreal, order.by=index((env_data$VTI)))
plot.zoo(pn_l)



###

lag_trade_dates <- c(1, trade_dates[-n_periods])

trade_prices <- NA*numeric(length=n_periods)
trade_prices[1] <- Op(env_data$VTI[1, ])
trade_prices[trade_dates] <- open_prices[trade_dates]
trade_prices <- na.locf(trade_prices)

pnl_unreal <- pos_ition*(price_s - trade_prices)

pnl_real <- numeric(length=n_periods)
pnl_real[trade_dates] <- pos_ition[lag_trade_dates]*
  (trade_prices[trade_dates] - trade_prices[lag_trade_dates])
pnl_real <- cumsum(pnl_real)

pn_l <- xts(pnl_real + pnl_unreal, order.by=index((env_data$VTI)))


###

############## hw
# 1. (35pts) Create a function called lag_it() that applies a lag to vectors 
# and "zoo" time series objects,
# lag_it() should accept two arguments:
# the first argument called "se_ries" can be a vector or "zoo" time series object,
# if "se_ries" is a vector, then lag_it() should return a lagged vector, 
# of the same length as the input,
# if "se_ries" is a "zoo", then lag_it() should return a lagged "zoo", 
# with the same number of rows as the input,
# the second argument called "lag" is an integer specifying the number of lags,
# if "lag" is positive, then lag_it() should replace the present value with 
# "lag" number of values from the past, 
# if "lag" is negative, then lag_it() should replace the present value with 
# "lag" number of values from the future, 
# for a vector, past values have a smaller index, and future values have a larger index,
# lag_it() should add NA values in place of values that are missing, 
# lag_it() should return NULL if "se_ries" is neither a vector nor a 
# "zoo" time series,
# for example, lag_it() should produce the following output:
#  lag_it(c(1:5), lag=2)
#  [1] NA NA  1  2  3
#  lag_it(c(1:5), lag=-2)
#  [1]  3  4  5 NA NA
# 
# some observations about the default method lag():
# the default method lag() can accept a vector and returns 
# a "ts" time series object,
# 
# some observations about lag.zoo():
# The method lag.zoo() returns a lagged version of a "zoo" time series, 
# by shifting its time index by "k" observations,
# If "k" is positive, then lag.zoo() shifts values from the future to the present, 
# and if "k" is negative then it shifts them from the past, 
# This is the opposite of what is usually considered as a positive "lag",
# A positive lag should replace the present value with values from the past 
# (negative lags should replace with values from the future), 
# lag.zoo() omits any NA values the lag may have produced, 
# returning a shorter time series than the original,
# 
# hint: you can use functions is.vector(), is.zoo(), cbind(), merge(), 
# lag.zoo(), c(), and rep(), 



###

end_points <- endpoints(etf_rets[, "VTI"], on="months")
foo <- merge(etf_rets[, "VTI"], env_data$VTI[, "VTI.Volume"])
foo <- foo[complete.cases(foo), ]
names(foo) <- c("rets", "volu")
foo <- merge(period.apply(foo[, 1], INDEX=end_points, FUN=sd), period.sum(foo[, 2], INDEX=end_points))
names(foo) <- c("rets", "volu")
plot.zoo(foo)
plot(as.formula(paste(names(foo), collapse=" ~ ")), data=foo)
reg_model <- lm(paste(names(foo), collapse=" ~ "), data=foo)
reg_model_sum <- summary(reg_model)
reg_model_sum
dwtest(reg_model)


###

# measure of dispersion
dis_persion <- function(da_ta, 
                        meth_od=c("mean", "mean_narm", "median")) {
  # validate "meth_od" argument
  meth_od <- match.arg(meth_od)
  switch(meth_od,
         mean=mean(da_ta),
         mean_narm=mean(da_ta, na.rm=TRUE),
         median=median(da_ta))
}  # end dis_persion

# sd
# range
# Interquartile range
# Median absolute deviation (MAD)



###

# rollSFM (rolling single-factor model) function to TTR. 
# rolling regression over time index

reg <- rollSFM(demo.xts, .index(demo.xts), 24)
rma <- reg$alpha + reg$beta*.index(demo.xts)
chart_Series(demo.xts, TA="add_TA(rma,on=1)")

###


# add vertical line:
http://stackoverflow.com/questions/15384458/add-vertical-lines-to-quantmodchart-series
l <- xts(!as.logical(s[,1]),index(s))
l[100] <- TRUE
chart_Series(s, TA="add_TA(l,col='grey', on=1)")


ch_ob$Env$actions[[4]]
attributes(ch_ob$Env$actions[[3]])
