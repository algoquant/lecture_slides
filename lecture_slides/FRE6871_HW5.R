#################################
### FRE6871 Homework #5 due Nov 2, 2015
#################################
# Max score 60pts

# Please write in this file the R code needed to perform the tasks below, 
# rename it to your_name_hw5.R
# and upload the file to NYU Classes

### write your code here

##############
# Summary: Estimate the probability of crossing a price barrier 
# by performing multiple simulations of prices.  
# Estimate the probabilities for several different price barrier 
# levels. 
# Start by defining the simulation parameters: 

# number of simulations
simu_times <- 500
# number of steps in each simulation
simu_length <- 1000
# barrier level
barrier_level <- 20

# Each simulation should consist of a number of simulation steps 
# equal to "simu_length".  At each step of the simulation, a random 
# number should be generated representing the price return (price 
# difference).  The prices should be equal to the cumulative returns.  
# If in a given simulation the prices cross a barrier called 
# "barrier_level", then a boolean vector called "did_cross" 
# should be set to TRUE, and otherwise it should be FALSE. 
# You will need to perform the simulations multiple times, equal 
# to "simu_times", each time recording the value of "did_cross". 

# 1. (20pts) Perform an sapply() loop multiple times, up to 
# "simu_times". Inside the loop perform a simulation of prices 
# crossing a barrier.  Adapt the code from the slide titled 
# "Simulating Barrier Options Using Vectorized Functions".
# The sapply() loop should return a boolean vector called 
# "did_cross" that should be set to TRUE if prices 
# crossed "barrier_level", and otherwise it should be FALSE. 
# "did_cross" should be a vector of length "simu_times". 

# hint: you can use an anonymous function that accepts an 
# integer argument (the loop count) and returns a boolean 
# value. 
# You can compare the simulated price vector to "barrier_level", 
# to determine if at any point the prices reached above the 
# "barrier_level".  If they did, then they must have crossed 
# "barrier_level" at some point. 
# The comparison of the prices with the "barrier_level" 
# produces a boolean vector, whose sum is zero only if prices 
# never crossed "barrier_level", and is greater than zero if
# they did. 
# You can use functions sapply(), sum(), cumsum(), and rnorm(), 

# reset random number generator
set.seed(1121)

### write your code here

# Calculate the probability of crossing the "barrier_level" 
# as the sum of "did_cross" divided by "simu_times". 

### write your code here


# 2. (20pts) Perform the same simulation as in p.1 but without 
# using an apply() loop, only using vectorized functions. 
# Start by creating a matrix of random numbers with dimensions 
# "simu_times" columns by "simu_length" rows, using rnorm(), 
# and call it "price_s". 
# Apply function colCumsums() from package "matrixStats" to 
# "price_s", to calculate the cumulative sums of its columns. 
# You can use functions matrix(), colCumsums(), and rnorm(), 

# load package matrixStats
library(matrixStats)
# reset random number generator
set.seed(1121)

### write your code here

# The columns of "price_s" represent vectors of simulated prices. 
# Following the methodology of p.1, compare the simulated prices 
# to "barrier_level", and produce a boolean matrix. 
# Sum up the columns of the boolean matrix to determine the 
# simulations for which the prices crossed the "barrier_level". 
# and call this boolean vector "did_cross". 
# "did_cross" should be a vector of length "simu_times". 
# You can use function colSums() from package "matrixStats", 

### write your code here

# Calculate the probability of crossing the "barrier_level" 
# as the sum of "did_cross" divided by "simu_times". 

### write your code here


# 3. (20pts) Estimate the probabilities for a vector of 
# different price barrier levels. 
# Create a named numeric vector called "barrier_levels" 
# with values from=5, to=60, by=5. 
# You can use functions seq(), structure(), and names(), 

### write your code here

# Perform an sapply() loop over "barrier_levels". 
# Inside the loop calculate the probabilities of crossing 
# the "barrier_level", and call the resulting vector 
# "cross_probs". 
# hint: you don't need to recalculate "price_s", and can 
# use the "price_s" from p.2, 
# To receive full credit you shouldn't recalculate "price_s"
# for different values of "barrier_levels". 
# You can use functions sapply(), sum(), colSums(), and 
# an anonymous function.

### write your code here

# Create a scatterplot of "cross_probs" versus "barrier_levels". 
# You can use functions plot() and title(). 

### write your code here


