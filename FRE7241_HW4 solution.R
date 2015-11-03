#################################
### FRE7241 Homework #4 Solution due Oct 27, 2015
#################################
# Max score 100pts

# The below solutions are examples,
# Slightly different solutions are also possible.


############## Part I
# Summary: Simulate a trading strategy based on two VWAPs. 

# download the file "etf_data.Rdata" from NYU Classes, and load() it. 
# "etf_data.Rdata" contains an environment called "env_data", 
# with OHLC time series data for ETFs, including "VTI". 

library(quantmod)
load(file="C:/Develop/data/etf_data.Rdata")

# 1. (5pts) Define two integer windows (lookback periods) called 
# "win_short=10" and "win_long=100". 
# Calculate two vectors of VWAPs called "vwap_short" and "vwap_long", 
# for the "VTI" OHLC data. 
# You must use function v_wap() from the previous homework, 

win_short <- 10
win_long <- 100
vwap_short <- v_wap(x_ts=env_data$VTI, win_dow=win_short)
vwap_long <- v_wap(x_ts=env_data$VTI, win_dow=win_long)

# Calculate a numeric vector called "indi_cator", that is 
# equal to 1 when "vwap_short > vwap_long" and equal to -1
# when "vwap_short < vwap_long", 
# The sign of "indi_cator" will determine the strategy's risk 
# positions, either long risk or short risk. 
# You can use function sign(), 

indi_cator <- sign(vwap_short - vwap_long)


############## Part II
# 2. (10pts) Calculate a boolean vector that is TRUE only on dates 
# right after the VWAPs have crossed, and call it "cross_es". 
# For example, if yesterday "vwap_short < vwap_long" and today 
# "vwap_short > vwap_long" (or vice versa), then today "cross_es" 
# should be TRUE, and otherwise it should be FALSE. 
# hint: the diff() of "indi_cator" is not zero right after the 
# VWAPs have crossed, and otherwise it's zero. 
# you can use the functions sign(), diff(), and is.na(), 
# and the logical operator "!=", 
# set any NAs to FALSE, 

cross_es <- (diff(indi_cator) != 0)
cross_es[is.na(cross_es)] <- FALSE

# The strategy should perform trades after "cross_es" becomes TRUE, 
# but with a one period lag, to reflect that in practice it's 
# impossible to trade immediately. 
# Calculate a vector of integer indices corresponding to trade 
# dates, and call it "trade_dates". 
# hint: first calculate the indices corresponding to the periods when 
# "cross_es" is TRUE, and add "1" to them, to reflect the one period lag. 
# you can use function which(), 

trade_dates <- which(cross_es)+1

# The strategy invests in a fixed number of shares called "num_shares".  

num_shares <- 100

# The strategy either owns "num_shares" number of shares (long position), 
# or sells the same number of shares short (short position).  
# Thus the strategy consists of consecutive time intervals of long risk 
# and short risk positions, depending on the sign of "indi_cator". 
# When "indi_cator" becomes positive then the strategy buys shares and 
# flips to a long risk position, and vice versa. 


############## Part III
# 3. (20pts) The strategy should be simulated over a number of periods 
# of time called "n_periods", which should be equal to the number of 
# rows in the OHLC time series data, 
# you can use function nrow(), 

n_periods <- nrow(env_data$VTI)

# Calculate a numeric vector called "pos_ition", that is equal to the 
# number of shares owned by the strategy at any given period of time, 
# either positive (long risk position) or negative (short risk position). 
# The strategy should start with a position of zero. 
# The strategy position should be reset on "trade_dates", depending on 
# the sign of "indi_cator". 
# The strategy position should remain unchamged between the "trade_dates". 
# you can use functions numeric() and na.locf(), 

pos_ition <- NA*numeric(length=n_periods)
pos_ition[1] <- 0
pos_ition[trade_dates] <- num_shares*indi_cator[trade_dates]
pos_ition <- na.locf(pos_ition)

# Lag the vector "pos_ition" by one period and call it "lag_position". 
# The first value of "lag_position" should be zero. 
# you can use function c() combined with subsetting "[]", 

lag_position <- c(0, pos_ition[-n_periods])

# you can inspect the vectors by merging and subsetting them:

foo <- merge(Ad(env_data$VTI), vwap_short, vwap_long, cross_es, pos_ition)
colnames(foo) <- c("price", "vwap_short", "vwap_long", "cross_es", "pos_ition")
foo[(which(cross_es)[2]-3):(which(cross_es)[2]+3), ]

# you should get this:
#               price vwap_short vwap_long cross_es pos_ition
# 2007-02-27 59.09827   60.62776  60.13491        0       100
# 2007-02-28 59.56461   60.42235  60.10604        0       100
# 2007-03-01 59.39503   60.17368  60.06567        0       100
# 2007-03-02 58.54714   59.98786  60.03648        1       100
# 2007-03-05 57.86882   59.65299  59.95280        0      -100
# 2007-03-06 58.84390   59.47699  59.92486        0      -100
# 2007-03-07 58.79727   59.32994  59.90830        0      -100

# Calculate a vector of adjusted close prices from the OHLC data, 
# and call it "price_s". 
# Calculate a vector of lagged "price_s", and call it "lag_prices". 
# Calculate a vector of open prices from the OHLC data, 
# and call it "open_prices". 
# you can use functions Ad(), Op(), and c(), 

price_s <- Ad(env_data$VTI)
lag_prices <- c(0, price_s[-n_periods])
open_prices <- Op(env_data$VTI)


############## Part IV
# 4. (20pts) Calculate a vector of periodic (daily) profits and 
# losses and call it "pn_l". 
# The periodic (day over day) profit or loss (pnl) for a period 
# without any trade, is equal to the position in the previous 
# period, times the difference between this period's closing 
# price minus the previous period's closing price. 
# The periodic pnl for a period with a trade, is equal to the 
# sum of two terms.  
# The first term is equal to the position in the previous 
# period, times the difference between this period's opening 
# price minus the previous period's closing price. 
# The first term represents the realized pnl after trading 
# out of the previous position. 
# The second term is equal to the current (new) position times 
# the difference between this period's closing minus opening 
# prices. 
# The second term represents the unrealized pnl of the new 
# position on the day of the trade. 
# you can use the vectors "price_s", "lag_prices", "open_prices", 
# "pos_ition", "lag_position" and "trade_dates", 

pn_l <- lag_position*(price_s - lag_prices)
pn_l[trade_dates] <- 
  lag_position[trade_dates]*(open_prices[trade_dates] - lag_prices[trade_dates]) + 
  pos_ition[trade_dates]*(price_s[trade_dates] - open_prices[trade_dates])

# you can inspect the vectors by merging and subsetting them:

foo <- merge(Ad(env_data$VTI), lag_prices, open_prices, pos_ition, lag_position, pn_l)
colnames(foo) <- c("price", "lag_prices", "open_prices", "pos_ition", "lag_position", "pn_l")
foo[(which(cross_es)[2]-3):(which(cross_es)[2]+3), ]

# you should get this:
#               price lag_prices open_prices pos_ition lag_position       pn_l
# 2007-02-27 59.09827   61.37487    60.68807       100          100 -227.66000
# 2007-02-28 59.56461   59.09827    59.36535       100          100   46.63400
# 2007-03-01 59.39503   59.56461    58.48778       100          100  -16.95800
# 2007-03-02 58.54714   59.39503    59.16186       100          100  -84.78900
# 2007-03-05 57.86882   58.54714    57.99600      -100          100  -42.39516
# 2007-03-06 58.84390   57.86882    58.37756      -100         -100  -97.50800
# 2007-03-07 58.79727   58.84390    58.92021      -100         -100    4.66300


# Calculate the Sharpe ratio of the strategy returns "pn_l", 
# given by the sum() of "pn_l" divided by the sd() of "pn_l". 

sum(pn_l)/sd(pn_l)

# Create an xts series from "pos_ition" and call it "xts_position". 
# you can use functions xts() and index(), 

xts_position <- xts(pos_ition, order.by=index((env_data$VTI)))

# Create an xts series from the cumulative sum of "pn_l" 
# and call it "xts_pnl". 
# you can use functions xts(), cumsum(), and index(), 

xts_pnl <- xts(cumsum(pn_l), order.by=index((env_data$VTI)))


############## Part V
# 5. (20pts) Plot the time series of prices and the strategy pnl 
# in two panels.
# open plot graphics device using function x11(), 
# set plot parameters using function par() with argument "mfrow", 

x11()
par(mfrow=c(2, 1))

# Plot in the top panel the adjusted close prices of the OHLC data, 
# add "vwap_long" to the plot, 
# add background shading of areas corresponding to long positions 
# in "lightgreen" and short positions in "lightgrey". 
# hint: call chart_Series() once, and then call add_TA() three times, 
# wrap the first three calls in invisible() to prevent plotting, 
# except for the last add_TA() call. 
# you must use functions Ad(), chart_Series(), add_TA() 
# (with "on" parameter), and invisible(), 
# you can use the xts series "xts_position" for shading, 
# You can adapt code from the "time_series_univariate" pdf and R files. 
# Be sure to download the most recent version.

invisible(chart_Series(x=Ad(env_data$VTI), name="VTI prices"))
invisible(add_TA(vwap_long, on=1, col='red'))
invisible(add_TA(xts_position > 0, on=-1, 
                 col="lightgreen", border="lightgreen"))
add_TA(xts_position < 0, on=-1, 
       col="lightgrey", border="lightgrey")

# Plot in the bottom panel "xts_pnl", 
# add background shading of areas as before. 
# you must use functions chart_Series(), add_TA() 
# (with "on" parameter), and invisible(), 

invisible(chart_Series(x=xts_pnl, name="VTI strategy"))
invisible(add_TA(xts_position > 0, on=-1, 
                 col="lightgreen", border="lightgreen"))
add_TA(xts_position < 0, on=-1, 
       col="lightgrey", border="lightgrey")


############## Part VI
# 6. (10pts) Create a function called run_vwap() which performs 
# a simulation of a trading strategy based on two VWAPs (as above), 
# and returns the Sharpe ratio of the strategy returns, 
# run_vwap() should accept three arguments: 
#  "win_short" and "win_long" - two integer lookback windows, 
#  "da_ta" - OHLC time series data, 
# hint: combine all the code from the previous parts. 

run_vwap <- function(win_short=10, win_long=100, da_ta) {
  vwap_short <- v_wap(x_ts=da_ta, win_dow=win_short)
  vwap_long <- v_wap(x_ts=da_ta, win_dow=win_long)
  indi_cator <- sign(vwap_short - vwap_long)
  cross_es <- (diff(indi_cator) != 0)
  cross_es[is.na(cross_es)] <- FALSE
  trade_dates <- which(cross_es)+1
  num_shares <- 100
  n_periods <- nrow(da_ta)
  pos_ition <- NA*numeric(length=n_periods)
  pos_ition[1] <- 0
  pos_ition[trade_dates] <- num_shares*indi_cator[trade_dates]
  pos_ition <- na.locf(pos_ition)
  lag_position <- c(0, pos_ition[-n_periods])
  price_s <- Ad(da_ta)
  lag_prices <- c(0, price_s[-n_periods])
  open_prices <- Op(da_ta)
  pn_l <- lag_position*(price_s - lag_prices)
  pn_l[trade_dates] <- 
    lag_position[trade_dates]*(open_prices[trade_dates] - lag_prices[trade_dates]) + 
    pos_ition[trade_dates]*(price_s[trade_dates] - open_prices[trade_dates])
  sum(pn_l)/sd(pn_l)
}  # end run_vwap

# call run_vwap() as follows, to verify it works correctly,

run_vwap(win_short=40, win_long=350, da_ta=env_data$VTI)

############## Part VII
# 7. (10pts) 
# Create a named vector of integer values for "win_short" from=30, to=100, by=10
# called "short_windows", with the following values:
# sh30  sh40  sh50  sh60  sh70  sh80  sh90 sh100 
#   30    40    50    60    70    80    90   100
# Create a named vector of integer values for "win_long" from=200, to=400, by=25
# called "long_windows", with the following values:
# lo200 lo225 lo250 lo275 lo300 lo325 lo350 lo375 lo400 
#   200   225   250   275   300   325   350   375   400 
# you can use functions seq(), paste0(), names(), and structure(), 

short_windows <- seq(from=30, to=100, by=10)
names(short_windows) <- paste0("sh", short_windows)
# or
short_windows <- structure(short_windows, names=paste0("sh", short_windows))

long_windows <- seq(from=200, to=400, by=25)
names(long_windows) <- paste0("lo", long_windows)
# or
long_windows <- structure(long_windows, names=paste0("lo", long_windows))

# perform two nested sapply() loops calling the function run_vwap(), 
# first over "short_windows", second over "long_windows". 
# To get full credit you must pass the arguments "da_ta=env_data$VTI" 
# into run_vwap() through the dots argument of the sapply() functions, 
# you can use an anonymous function,
# the output should ba a named matrix called "mat_rix" as follows 
# (transpose will also get full credit):
#           sh30      sh40      sh50      sh60     sh70     sh80     sh90    sh100
# lo200 84.06524  91.24971  93.77047  92.01054 87.79107 76.23158 57.91753 74.07076
# lo225 72.06119  97.77155  92.31084  88.33317 76.08169 75.71970 41.36928 38.56718
# lo250 86.73914  88.73794  91.26042  95.83383 82.29601 67.69120 61.48578 43.94475
# lo275 84.42591  95.18654  92.79157  81.29927 86.75097 55.87239 55.67067 60.19943
# lo300 90.90760  94.00829 103.63414 108.20157 84.86396 64.79328 49.14296 64.55424
# lo325 92.71081 105.49712  99.21752 108.44899 72.44130 66.29816 65.10376 73.93077
# lo350 95.65559 113.38591 112.83352  87.72392 85.38218 70.43943 66.78933 79.28444
# lo375 89.53747 121.43885 105.97034  94.08813 88.17621 71.61272 71.60992 64.63382
# lo400 91.10256 115.04823  95.78384  99.42304 89.05842 78.38954 67.25607 82.20783

mat_rix <- sapply(short_windows, 
                  function(win_short, da_ta) 
                    sapply(long_windows, 
                           run_vwap, 
                           win_short=win_short, 
                           da_ta=da_ta), 
                  da_ta=env_data$VTI)


############## Part VIII
# 8. (5pts) Draw an interactive 3d surface plot of "mat_rix". 
# you can use function persp3d(), 
# You can adapt code from the "plotting" pdf and R files. 
# Be sure to download the most recent version.

library(rgl)  # load rgl
persp3d(z=mat_rix, col="green", x=short_windows, y=long_windows)


