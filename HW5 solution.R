#################################
### HW #5 Solution
#################################
# Max score 30pts

# The below solutions are examples,
# Slightly different solutions are also possible.

# comment:
# Most of this homework required just copying and pasting from the lecture notes,

# Homework assignment:

# 1. (5pts) Download the zoo Quickref manual from CRAN, and read how to query the Oanda database,
# Download from Yahoo the EOD CLOSE quotes for MSFT stock, starting from Sep/01/2013,
library(tseries)  # load package tseries
library(zoo)  # load package zoo
# load MSFT data
suppressWarnings(
  zoo_msft <- get.hist.quote(instrument="MSFT", 
                             start=as.POSIXct("2013-09-01"), 
                             end=Sys.Date(), 
                             origin="1970-01-01")
)  # end suppressWarnings
# extract only EOD CLOSE quotes
zoo_msft <- zoo_msft[, "Close"]

# Download from Oanda the EOD CLOSE quotes for eur currency, starting from Sep/01/2013,
suppressWarnings(  # load EUR/USD data
  zoo_eurusd <- get.hist.quote(
    instrument="EUR/USD", provider="oanda",
    start=as.POSIXct("2013-09-01"), 
    end=Sys.Date(), 
    origin="1970-01-01")
)  # end suppressWarnings



# 2. (5pts) Create smooth version of each time series using rollmean over an 11 period window,
zoo_msft_smooth <- rollmean(x=zoo_msft, k=11)
zoo_eurusd_smooth <- rollmean(x=zoo_eurusd, k=11)



# 3. (5pts) Replace NA values using na.locf, first going forward, and then backward in time,
zoo_msft_smooth <- na.locf(zoo_msft_smooth)
zoo_msft_smooth <- na.locf(zoo_msft_smooth, fromLast=TRUE)
zoo_eurusd_smooth <- na.locf(zoo_eurusd_smooth)
zoo_eurusd_smooth <- na.locf(zoo_eurusd_smooth, fromLast=TRUE)



# 4. (5pts) Plot each time series combined with its smoothed version,
# first plot MSFT
plot(zoo_msft, type="l", lwd=2, xlab="", ylab="")
# add smoothed MSFT
lines(zoo_msft_smooth, col="red", lwd=2)
# add legend
legend("bottomright", title="MSFT smoothed", legend=c("MSFT", "MSFT-smooth"), 
       inset=0.05, cex=0.8, lwd=2, lty=c(1, 1), col=c("black", "red"))

# this is a version with better X-axis date labels (not required for full credit)
# first plot without X-axis
plot(zoo_msft, type="l", lwd=2, xlab="", ylab="", xaxt="n")
# create X-axis date labels
axis_dates <- seq(from=as.Date("2013-09-01"), to=Sys.Date(), by="quarter")
# add X-axis
axis(side=1, at=axis_dates, labels=format(axis_dates, "%b-%y"))
# add smoothed MSFT
lines(zoo_msft_smooth, col="red", lwd=2)
# add legend
legend("bottomright", title="MSFT smoothed", legend=c("MSFT", "MSFT-smooth"), 
       inset=0.05, cex=0.8, lwd=2, lty=c(1, 1), col=c("black", "red"))



# 5. (5pts) Bind the two original time series together using merge,
# Remove observations containing NA values,
zoo_msfteur <- merge(zoo_eurusd, zoo_msft)
colnames(zoo_msfteur) <- c("EURUSD", "MSFT")
zoo_msfteur <- zoo_msfteur[complete.cases(zoo_msfteur), ]



# 6. (5pts) Plot the MSFT stock and eur currency time series together on a plot with two y axes,
# plot first ts
plot(zoo_msfteur[, 1], xlab=NA, ylab=NA)
# set range of "y" coordinates for second axis
par(usr=c(par("usr")[1:2], range(zoo_msfteur[,2])))
lines(zoo_msfteur[, 2], col="red")  # second plot
axis(side=4, col="red")  # second "y" axis on right
# print axis labels
par(las=1)  # set text printing to "horizontal"
mtext(colnames(zoo_msfteur)[1], side=2, padj=-6, line=-4)
mtext(colnames(zoo_msfteur)[2], col="red", side=4, padj=-2, line=-3)
title(main="EUR and MSFT")  # add title
# add legend without box
legend("bottom", legend=colnames(zoo_msfteur), bg="white", 
       lty=c(1, 1), lwd=c(2, 2), col=c("black", "red"), bty="n")

