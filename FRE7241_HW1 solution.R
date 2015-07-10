#################################
### FRE7241 HW #1 Solution due June 15, 2015
#################################
# Max score 40pts

# The below solutions are examples,
# Slightly different solutions are also possible.


###############
# 1. (20pts) Download from Yahoo the "AdjClose" prices and "Volume" for 
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
# use function rollmean(), with the proper "align" argument, 
# so that the averages are calculated using values from the past,

roll_mean <- rollmean(x=zoo_msft[, "AdjClose"], k=50, align="right")
zoo_msft <- na.omit(merge(zoo_msft, roll_mean))
colnames(zoo_msft)[3] <- "50DMA"


# plot "zoo_msft" columns "AdjClose" and "50DMA" in the same panel, 
# starting from "2015-01-01",
# use method plot.zoo() with the proper argument "plot.type",
# add legend so that it doesn't obscure the plot,

plot(zoo_msft[(index(zoo_msft)>as.Date("2015-01-01")), 
              c("AdjClose", "50DMA")], 
     main="MSFT Prices and 50DMA", 
     xlab="", ylab="", plot.type="single", 
     col=c("black", "red"))
# add legend
legend("top", inset=0.05, cex=0.5, 
       title="MSFT Prices and 50DMA", 
       leg=c("MSFT", "50DMA"), lwd=2, bg="white", 
       col=c("black", "red"))


# calculate the vector of dates when zoo_msft[, "AdjClose"] 
# crosses "50DMA", and call it "cross_es", 
# "cross_es" should be TRUE for dates when a cross had just occurred, 
# and FALSE otherwise,

cross_es <- !(diff(sign(zoo_msft[, "AdjClose"] - zoo_msft[, "50DMA"]))==0)
cross_es <- index(zoo_msft[cross_es, ])


# add vertical ablines to the plot above, at the dates of "cross_es",

abline(v=cross_es[cross_es>as.Date("2015-01-01")], col="blue", lty="dashed")



###############
# 3. (20pts) Calculate the 50-day rolling maximum and minimum of 
# the "AdjClose" prices,
# use function rollmax() with the proper "align" argument, so that 
# the aggregations are calculated using values from the past,
# calculate the difference between the rolling maximum and minimum, 
# and call it "ba_nd",

roll_max <- rollmax(x=zoo_msft[, "AdjClose"], k=50, align="right")
roll_min <- -rollmax(x=-zoo_msft[, "AdjClose"], k=50, align="right")
ba_nd <- (roll_max-roll_min)


# calculate the rolling upper (lower) band as the moving average
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

plot(zoo_msft[(index(zoo_msft)>as.Date("2015-01-01")), 
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


# calculate the vector of dates when zoo_msft[, "AdjClose"] 
# crosses any of the bands, and call it "cross_es", 
# "cross_es" should be TRUE for dates when a cross had just occurred, 
# and FALSE otherwise,


up_cross <- zoo_msft[, "AdjClose"] - zoo_msft[, "Up_band"]
low_cross <- zoo_msft[, "AdjClose"] - zoo_msft[, "Low_band"]
cross_es <- ((!(diff(sign(up_cross))==0)) | (!(diff(sign(low_cross))==0)))
cross_es <- index(zoo_msft[cross_es, ])

# add vertical ablines to the plot above, at the dates of "cross_es",

abline(v=cross_es[cross_es>as.Date("2015-01-01")], col="green", lty="dashed")

