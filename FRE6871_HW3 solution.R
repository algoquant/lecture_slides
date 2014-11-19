#################################
### HW #3 Solution
#################################
# Max score 35pts

# The below solutions are examples,
# Slightly different solutions are also possible.

# plot two Normal probability distributions on the same plot,
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


# create a vector of 5 weekly POSIXct dates corresponding to Monday 09:30AM, starting with:
#   date_time <- as.POSIXct("2014-01-27 09:30:00", tz="America/New_York")
# use lubridate function weeks(),
date_time <- as.POSIXct("2014-01-27 09:30:00", tz="America/New_York")
mon_days <- date_time + weeks(1)*(0:4)
# three different ways of getting day of week - all OK
wday(mon_days, TRUE)
as.POSIXlt(mon_days)$wday
weekdays(mon_days)


# create a vector of decimal dates as follows:
#   date_time <- 2014 + (1:5)/12
# convert them to POSIXct dates, using a lubridate function,
date_time <- 2014 + (1:5)/12
date_decimal(date_time)
# or
date_decimal(date_time, tz="America/New_York")



# The "while" clause is the critical part of this homework, so all the credit comes from that.
# You will get full credit only if you correctly modified the "while" clause.


# 1. Create an $R$ script for simulating 1000 random prices using rnorm(),
set.seed(1121)  # for reproducibility
## set up simulation parameters
simu_max <- 1000  # max simulation trials
barrier_first <- -10  # barrier level #1
barrier_second <- 10  # barrier level #2

## initialize simulation variables
# initialize vector of prices to zero
simu_prices <- 0.0*(1:simu_max)
# first simulated price
simu_prices[1] <- rnorm(1)
# starting value of simulation index
simu_index <- 2
# Boolean variable keeps track if barrier_first was crossed yet
first_crossed <- FALSE
# Boolean variable keeps track if barrier_first was crossed yet
both_crossed <- FALSE


# 2. (35pts) Use a while loop to stop the simulation if the prices first cross barrier_first=-10, 
#    and then cross barrier_second=10,

## perform the simulation in while loop
while ((simu_index <= simu_max) && !both_crossed) {
# the "while" clause is the critical part of this homework
# the "while" clause is TRUE only if simulation hasn't reached end yet
# and if price haven't crossed both barriers yet
# first, check if first barrier was crossed
  first_crossed <- first_crossed || (simu_prices[simu_index-1] < barrier_first)
# second, check if both barriers were crossed
  both_crossed <- both_crossed || 
                  (first_crossed && 
                     (simu_prices[simu_index - 1] > barrier_second))
# simulate next price
  simu_prices[simu_index] <- simu_prices[simu_index - 1] + rnorm(1)
# advance simu_index
  simu_index <- simu_index + 1
}  # end while

## fill remaining zero prices
if (simu_index <= simu_max) {
  simu_prices[simu_index:simu_max] <- simu_prices[simu_index - 1]
}


# create daily time series starting 2011
ts_prices <- ts(data=simu_prices, frequency=365, start=c(2011, 1))


# 3. plot the prices and the two barrier levels,

plot(ts_prices, type="l", col="black", lty="solid", xlab="", ylab="")
abline(h=barrier_first, lwd=2, col="blue")  # add horizontal line
abline(h=barrier_second, lwd=2, col="red")  # add horizontal line
title(main="Random Prices", line=0)  # add title


