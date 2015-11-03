#################################
### FRE7241 HW #2 Solution due Sep 29, 2015
#################################
# Max score 75pts

# The below solutions are examples,
# Slightly different solutions are also possible.


###############
# Summary: Calculate moving averages and crossing points with prices.

# 1. (10pts) 
# Download from Yahoo the "AdjClose" prices and "Volume" for 
# MSFT stock, starting from Jun/01/2007, and call it "zoo_msft",
# use tseries function get.hist.quote(),

library(tseries)  # load package tseries
library(zoo)  # load package zoo

# load MSFT data
zoo_msft <- suppressWarnings(
  get.hist.quote(
    instrument="MSFT", 
    start=as.Date("2007-06-01"), 
    end=Sys.Date(), 
    quote=c("AdjClose","Volume"),
    origin="1970-01-01")
)  # end suppressWarnings

# calculate the 50-day moving average of the "AdjClose" prices,
# merge the moving average with "zoo_msft" by adding it as the last column,
# rename the last column to "50DMA",
# you must use function rollmean(), with the proper "align" argument, 
# so that the averages are calculated using values from the past,
# remove rows with NAs using function na.omit(), 

roll_mean <- rollmean(x=zoo_msft[, "AdjClose"], k=50, align="right")
zoo_msft <- na.omit(merge(zoo_msft, roll_mean))
colnames(zoo_msft)[3] <- "50DMA"


# 2. (15pts) 
# plot "zoo_msft" columns "AdjClose" and "50DMA" in the same panel, 
# starting from "2015-01-01", in the colors "black" and "red", 
# you must use method plot.zoo() with the proper argument "plot.type",
# add a legend and position it so that it doesn't obscure the plot too much,

start_date <- as.Date("2015-01-01")
plot(zoo_msft[(index(zoo_msft)>start_date), 
              c("AdjClose", "50DMA")], 
     main="MSFT Prices and 50DMA", 
     xlab="", ylab="", plot.type="single", 
     col=c("black", "red"))
# add legend
legend("bottomright", inset=0.05, cex=0.5, 
       title="MSFT Prices and 50DMA", 
       leg=c("MSFT", "50DMA"), lwd=2, bg="white", 
       col=c("black", "red"))

# calculate the vector of dates right after the "AdjClose" crosses the "50DMA", 
# and call it "cross_es". 
# First calculate a boolean vector that is TRUE for dates right 
# after a cross has just occurred, and FALSE otherwise. 
# Next apply this boolean vector to extract dates when 
# the "AdjClose" crosses the "50DMA". 
# you can use the logical operator "!=", 
# and functions sign(), diff(), and index(), 

cross_es <- (diff(sign(zoo_msft[, "AdjClose"] - zoo_msft[, "50DMA"]))!=0)
cross_es <- index(zoo_msft[cross_es, ])

# add grey vertical ablines to the plot above, at the dates of "cross_es",
# you must use function abline(), 

abline(v=cross_es[cross_es>start_date], col="grey")


# 3. (20pts)
# Calculate the 50-day rolling maximum and minimum of the "AdjClose" prices,
# you must use function rollmax() with the proper "align" argument, so that 
# the aggregations are calculated using values from the past,
# calculate the difference between the rolling maximum and minimum, 
# and call it "ba_nd",

roll_max <- rollmax(x=zoo_msft[, "AdjClose"], k=50, align="right")
roll_min <- -rollmax(x=-zoo_msft[, "AdjClose"], k=50, align="right")
ba_nd <- (roll_max-roll_min)

# calculate the rolling upper (lower) band as the 50-day moving average
# plus (minus) one half of "ba_nd",
# merge the rolling upper and lower bands with "zoo_msft" by adding 
# them as the last columns,
# rename the last columns to "Up_band" and "Low_band",
# remove rows with NAs using function na.omit(), 

upper_band <- zoo_msft[, "50DMA"] + ba_nd/2
lower_band <- zoo_msft[, "50DMA"] - ba_nd/2
zoo_msft <- merge(zoo_msft, upper_band, lower_band)
colnames(zoo_msft)[4:5] <- c("Up_band", "Low_band")
zoo_msft <- na.omit(zoo_msft)

# plot "zoo_msft" columns "AdjClose", "Up_band", and "Low_band" in the 
# same panel, starting from "2015-01-01",
# use method plot.zoo() with the proper argument "plot.type",
# add legend so that it doesn't obscure the plot,

plot(zoo_msft[(index(zoo_msft)>start_date), 
              c("AdjClose", "Up_band", "Low_band")], 
     main="MSFT Prices with Bollinger Bands", 
     xlab="", ylab="", plot.type="single", 
     col=c("black", "red", "blue"))
# add legend
legend("top", inset=0.05, cex=0.5, 
       title="MSFT Prices with Bollinger Bands", 
       leg=c("MSFT", "Up_band", "Low_band"), 
       lwd=2, bg="white", 
       col=c("black", "red", "blue"))

# calculate the vector of dates right after the "AdjClose"
# crosses any of the two bands, and call it "cross_es". 
# First calculate a boolean vector that is TRUE for dates right 
# after a cross has just occurred, and FALSE otherwise. 
# Next apply this boolean vector to extract dates when 
# the "AdjClose" crosses either "Up_band" or "Low_band". 
# you can use the logical operator "!=", 

up_cross <- zoo_msft[, "AdjClose"] - zoo_msft[, "Up_band"]
low_cross <- zoo_msft[, "AdjClose"] - zoo_msft[, "Low_band"]
cross_es <- (((diff(sign(up_cross))!=0)) | ((diff(sign(low_cross))!=0)))
cross_es <- index(zoo_msft[cross_es, ])

# add grey vertical ablines to the plot above, at the dates of "cross_es",
# you must use function abline(), 

abline(v=cross_es[cross_es>start_date], col="grey")



###############
# Summary: Calculate the maximum drawdown of a time series.

# 1. (30pts) 
# download the file "zoo_data.Rdata" from NYU Classes, and load() it, 
# the file "zoo_data.Rdata" includes a zoo series called "zoo_stx", 
# containing MSFT stock OHLC data. 
# extract the "AdjClose" prices from "zoo_stx" into a variable 
# called "msft_prices".

load(file="C:/Develop/data/zoo_data.RData")
msft_prices <- zoo_stx[, "AdjClose"]

# plot "msft_prices", using generic function plot(),

plot(msft_prices)

# The cumulative maximum of a price series is the maximum price up to 
# that point in time. 
# Plot the Cumulative maximum of "msft_prices" using function cummax(),

plot(cummax(msft_prices))

# A drawdown is a drop in price from its previous maximum.
# Calculate the zoo time series of drawdowns of "msft_prices", 
# as the difference between the cumulative maximum of "msft_prices" 
# minus "msft_prices", and call it "draw_down", 

draw_down <- cummax(msft_prices) - msft_prices

# plot "draw_down",

plot(draw_down)

# Find the date when "draw_down" reaches its maximum, and call it "date_trough", 
# and find the maximum value of "draw_down" on that date, and call it "max_drawdown", 
# you can use functions index() and which.max(),

in_dex <- index(msft_prices)
date_trough <- in_dex[which.max(draw_down)]
max_drawdown <- draw_down[date_trough]

# Subset "draw_down" to dates before "date_trough", and call it "pre_drawdown", 

pre_drawdown <- draw_down[in_dex<date_trough]

# Subset "draw_down" to dates after "date_trough", and call it "post_drawdown", 

post_drawdown <- draw_down[in_dex>date_trough]

# When the current price exceeds the previous maximum, then "draw_down" is zero, 
# a drawdown starts when "draw_down" is first zero and then increases above zero.
# Find the latest date when "pre_drawdown" was still zero before "date_trough", 
# and call it "date_from",
# you can use functions index() and max(),

date_from <- max((index(pre_drawdown))[pre_drawdown==0])

# A drawdown ends when "draw_down" drops back to zero after "date_trough".
# Find the first date when "post_drawdown" drops to zero after "date_trough", 
# and call it "date_to",
# you can use functions index() and min(),

date_to <- min((index(post_drawdown))[post_drawdown==0])

# Combine the three dates into a named vector: 
# from=date_from, trough=date_trough, to=date_to,
# and call it "drawdown_dates",

drawdown_dates <- c(from=date_from, trough=date_trough, to=date_to)

# 2. (5pts) plot "msft_prices", using generic function plot(),

plot(msft_prices, main="Drawdown dates")

# add vertical green, red, and blue lines for the three dates: 
# "date_from", "date_trough", "date_to",

abline(v=drawdown_dates, col=c("green", "red", "blue"))


