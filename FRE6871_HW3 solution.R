#################################
### FRE6871 Homework #3 Solution
#################################
# Max score 50pts

# The below solutions are examples,
# Slightly different solutions are also possible.


##################################
# 1. (20pts) Create a function called my_sqrt() that calculates 
# the square root of its single argument,
# my_sqrt() should check if the input is both numeric and positive,
# if the input is numeric and positive, 
#  then my_sqrt() should return the square root,
# if the input is numeric and negative, 
#  then my_sqrt() should issue a warning using warning(), 
#  and return the square root of the absolute value,
# if the input is not numeric, 
#  then my_sqrt() should halt execution using stop(), 
#  and not return anything,
# use "if" and "else" statements,
# you can use functions sqrt(), is.numeric(), warning(), stop(), 
# and the operator "&&",

my_sqrt <- function(arg_var) {
  if (is.numeric(arg_var) && arg_var>=0) {
    sqrt(arg_var)
  } else if (is.numeric(arg_var)) {
    warning("negative input - taking square root of absolute!\n")
    sqrt(abs(arg_var))
  } else {
    stop(paste0("argument \"", arg_var, "\" isn't numeric"))
  }  # end if
}  # end my_sqrt

# call my_sqrt() as follows, to make sure it works properly:
my_sqrt(4)
my_sqrt(-4)
my_sqrt("a")



##################################
# 2. (30pts) Simulate random events using a "while" loop.
# "while" loops are often used in simulations, 
# when the number of required loops is unknown in advance,
# 
# Perform a simulation of randomly flipping a coin,
# record the number of heads, and stop when the number 
# of heads (stored in "he_ads") reaches "heads_max=10", 
# record the number of simulation loops "num_loops",
# use functions while() and runif(),
# 
# the coin may be biased, that is, the probability of 
# obtaining heads or tails may not be equal to 0.5,
# let "coin_bias" be the probability of obtaining heads,
# perform the simulation for a biased coin, with "coin_bias=0.4"

set.seed(1121)  # for reproducibility
heads_max <- 10  # max number of heads
coin_bias <- 0.4  # probability of obtaining heads
he_ads <- 0  # initialize heads counter
num_loops <- 0  # initialize loop counter
while (he_ads < heads_max) {
# flip coin and record it if it's heads
  he_ads <- he_ads + (runif(1) < coin_bias)
# or for unbiased coins:
#  he_ads <- he_ads + sample(0:1, 1)
# advance loop counter
  num_loops <- num_loops + 1
}  # end while


# Create a function called coin_flip(), that calculates the 
# number of coin flips needed to obtain a certain number of heads, 
# equal to "heads_max", 
# coin_flip() should perform a simulation of randomly flipping 
# a biased coin, using a "while" loop,
# coin_flip() should take two arguments:
# "heads_max" for the number of heads that need to be tossed
# before the simulation ends, and 
# "coin_bias" the probability of obtaining heads (with default
# value equal to 0.5),
# coin_flip() should return the number of simulation loops that 
# were performed before "heads_max" heads were tossed,
# coin_flip() should call set.seed(1121), for reproducibility,

coin_flip <- function(heads_max, coin_bias=0.5) {
  set.seed(1121)  # for reproducibility
  he_ads <- 0  # initialize heads counter
  num_loops <- 0  # initialize loop counter
  while (he_ads < heads_max) {
# flip coin and record it if it's heads
    he_ads <- he_ads + (runif(1) < coin_bias)
# advance loop counter
    num_loops <- num_loops + 1
  }  # end while
  num_loops
}  # end coin_flip


# call coin_flip() as follows, to make sure it works properly:
coin_flip(heads_max=10, coin_bias=0.5)
coin_flip(heads_max=100, coin_bias=0.5)
coin_flip(heads_max=10, coin_bias=0.1)
coin_flip(heads_max=10, coin_bias=0.9)


# Calculate the number of coin flips needed to obtain 
# a certain number of heads, using different "coin_bias" 
# parameters, from 0.2 to 0.8, in 0.1 increments, 
# with "heads_max=10",
# 
# perform an sapply() loop over a vector of "coin_bias" parameters,
# and pass "heads_max=10" to coin_flip() using the "..." argument,
# plot the vector returned by sapply(), and give proper names to 
# the axis labels,
coin_biases <- seq(from=0.2, to=0.8, by=0.1)
num_flips <- sapply(coin_biases, coin_flip, heads_max=10)
plot(x=coin_biases, y=num_flips, t="l", 
     xlab="coin bias", ylab="number of coin flips")

