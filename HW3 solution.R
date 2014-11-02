#################################
### HW #3 Solution
#################################
# Max score 35pts

# The below solutions are examples,
# Slightly different solutions are also possible.

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


