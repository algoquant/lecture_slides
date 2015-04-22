#################################
### HW #3 Solution
#################################
# Max score 25pts

# The below solutions are examples,
# Slightly different solutions are also possible.


# 1. (5pts) plot two Normal probability distributions on the same plot,
# one with (mean=0, sd=1), and another with (mean=0, sd=2),
# set the x scale to (-6, 6),
# plot a Normal probability distribution
curve(expr=dnorm, type="l", xlim=c(-6, 6), 
      xlab="", ylab="", lwd=2, col="blue")
# add shifted Normal probability distribution
curve(expr=dnorm(x, sd=2), add=TRUE, 
      type="l", lwd=2, col="red")
# add title
title(main="Normal probability distribution functions", 
      line=0.1)
# add legend
legend(x="topright", legend=c("Standard", "Normal"),
       title="legend", inset=0.05, cex=0.8, bg="white",
       lwd=2, lty=c(1, 1), col=c("blue", "red"))


# 2. (5pts) create a vector of decimal dates as follows:
#   date_time <- 2014 + (1:5)/12
# convert them to POSIXct dates, using a lubridate function,
library(lubridate)
date_time <- 2014 + (1:5)/12
date_decimal(date_time)
# or
date_decimal(date_time, tz="America/New_York")


# 3. (15pts) create a vector of 5 weekly POSIXct dates corresponding to 
#   Monday 09:30AM, starting with:
date_time <- as.POSIXct("2014-01-27 09:30:00", tz="America/New_York")

#   use lubridate function weeks(),
mon_days <- date_time + weeks(1)*(0:4)

# this is just an extra comment:
# three different ways of getting day of week - all OK
wday(mon_days, TRUE)
as.POSIXlt(mon_days)$wday
weekdays(mon_days)


