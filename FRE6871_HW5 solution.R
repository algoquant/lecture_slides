#################################
### HW #5 Solution
#################################
# Max score 50pts

# The below solutions are examples,
# Slightly different solutions are also possible.


# 1. (5pts) download from Yahoo the EOD CLOSE quotes for MSFT stock, starting from Sep/01/2013,
library(tseries)
zoo_msft <- suppressWarnings(  # load MSFT data
  get.hist.quote(instrument="MSFT", 
                 quote = "Close",
                 start=as.Date("2013-09-01"), 
                 end=Sys.Date(), 
                 origin="1970-01-01")
)  # end suppressWarnings


# 2. (5pts) smooth the time series using rollmean() over a 7-day window,
mean_msft <- rollmean(zoo_msft, k=7)


# 3. (5pts) bind the original time series together with its smoothed version using merge(),
#    replace NA values using na.locf(), first going forward, and then backward in time,
library(zoo)
zoo_msft <- merge(zoo_msft, mean_msft)
zoo_msft <- na.locf(zoo_msft)
zoo_msft <- na.locf(zoo_msft, fromLast=TRUE)


# 4. (5pts) plot the two time series together on the same plot,
plot(zoo_msft[, 1], xlab=NA, ylab=NA)
par(usr=c(par("usr")[1:2], range(zoo_msft[,2])))
lines(zoo_msft[, 2], col="red")  # second plot
axis(side=4, col="red")  # second "y" axis on right
mtext(colnames(zoo_msft)[1], side=2, padj=-6, line=-4)
mtext(colnames(zoo_msft)[2], col="red", side=4, padj=-2, line=-3)
title(main="MSFT and mean MSFT")  # add title
legend("bottomright", legend=colnames(zoo_msft), bg="white", 
       lty=c(1, 1), lwd=c(2, 2), col=c("black", "red"), bty="n")


# 5. (15pts) find what is the class of the index of the time series,
class(index(zoo_msft))

#    create a vector of weekly dates of class Date corresponding 
#    to every Monday, starting with: as.Date("2013-09-02"), until the 
#    most recent Monday (use lubridate function weeks()),
mon_days <- as.Date("2013-09-02") + weeks(1)*(0:70)
mon_days <- mon_days[(mon_days <= as.Date("2014-11-24"))]


# 5. (15pts) find the MSFT CLOSE quotes for every Monday, by subsetting the original time series,
zoo_msft <- zoo_msft[mon_days, 1]

#    calculate the weekly returns, and find the weeks with the highest and lowest returns,
msft_ret <- diff(zoo_msft)
mon_days[which.max(msft_ret)]
mon_days[which.min(msft_ret)]

