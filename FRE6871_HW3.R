#################################
### FRE6871 Homework #3 due May 2, 2016
#################################
# Max score 120pts

# Please write in this file the R code needed to perform the tasks below, 
# rename it to your_name_hw3.R
# and upload the file to NYU Classes


############## Part I
# Summary: Estimate the probability of crossing a price 
# barrier by performing multiple simulations of prices.  
# Estimate the probabilities for several different price 
# barrier levels. 
# 
# Start by defining the simulation parameters: 
# number of simulations
simu_number <- 500
# number of steps in each simulation
simu_length <- 1000
# barrier level
lev_el <- 20

# 1. (20pts) 
# Summary: You'll need to perform a number simulations of 
# prices crossing a barrier, equal to simu_number.
# Each simulation should consist of a number of simulation 
# steps equal to simu_length.  

# Perform an sapply() loop, starting from one to 
# simu_number. Inside each loop perform a simulation of 
# prices crossing a barrier.  
# hint: you can adapt the code from the slide titled 
# "Simulating Barrier Options Using Vectorized Functions".
# The sapply() loop should return a boolean vector called 
# did_cross, that should be equal to TRUE if prices did 
# cross the lev_el, and otherwise it should be FALSE. 
# did_cross should be a vector of length simu_number. 

# hint: you can use an anonymous function that accepts an 
# integer argument (the loop count) and returns a boolean 
# value. 

# You can compare the simulated price vector to lev_el, 
# to determine if at any point the prices reached above the 
# lev_el.  If they did, then they must have crossed lev_el 
# at some point. 
# The comparison of prices with lev_el produces a boolean 
# vector, whose sum is zero only if prices never crossed 
# lev_el, and is greater than zero if they did. 
# You can use functions sapply(), sum(), cumsum(), and 
# rnorm(), 

# reset random number generator
set.seed(1121)

### write your code here

# Calculate the probability of crossing the lev_el 
# as the sum of did_cross divided by simu_number. 
# The probability should be equal to 0.516. 

### write your code here


# 2. (20pts) Perform the same simulation as in p.1 but 
# without using an sapply() loop, only using vectorized 
# functions. 
# Start by creating a matrix of random numbers with 
# dimensions equal to simu_number columns by simu_length 
# rows, and call it price_s. 
# Apply function colCumsums() from package matrixStats to 
# price_s, to calculate the cumulative sums of its columns. 
# You can use functions matrix(), colCumsums(), and rnorm(). 

# load package matrixStats
library(matrixStats)
# reset random number generator
set.seed(1121)

### write your code here

# The columns of price_s represent vectors of simulated prices. 
# Following the methodology of p.1, compare the simulated 
# prices to lev_el, and produce a boolean matrix. 
# Sum up the columns of the boolean matrix to determine the 
# simulations for which the prices crossed the lev_el. 
# and call this boolean vector did_cross. 
# did_cross should be a vector of length simu_number. 
# You can use function colSums() from package matrixStats, 

### write your code here

# Calculate the probability of crossing the lev_el 
# as the sum of did_cross divided by simu_number. 
# The probability should be equal to 0.516. 

### write your code here


# 3. (20pts) Estimate the probabilities for a vector of 
# different price barrier levels. 
# Create a named numeric vector called lev_els with 
# values from=5, to=60, by=5. 
# You can use functions seq(), structure(), and names(), 

### write your code here

# You should get the following result:
# lev_els
# level=5 level=10 level=15 level=20 level=25
#       5       10       15       20       25

# Perform an sapply() loop over lev_els. 
# In each loop calculate the probabilities of crossing 
# the lev_el, and call the resulting vector cross_probs. 
# To receive full credit you shouldn't recalculate 
# price_s for each loop (lev_el), but instead use the 
# price_s already calculated in p.2. 
# You can use functions sapply(), sum(), colSums(), and 
# an anonymous function.

### write your code here

# Create a scatterplot of cross_probs versus lev_els. 
# You can use functions plot() and title(). 

### write your code here



############## Part II
# Summary: Read a data frame containing student homework and test 
# scores, and assign letter grades. 

# Download the file "student_scores.csv" from NYU Classes. 
# The file contains a data frame with student names, track, 
# and scores. 
# Read the file into a variable called student_scores using 
# read.csv(). 

### write your code here

# The data frame student_scores contains 8 columns: student names, 
# track, and six columns of numerical scores for homeworks and tests. 
# But some of the numeric columns contain NAs and characters, which 
# forces their coercion into factors.

# 1. (10pts) Perform an sapply() loop over the columns 3 to 8, 
# containing numerical scores and coerce them to numeric. 
# You can use functions sapply() and as.numeric(), 

### write your code here

# Extract the "class" of the columns of student_scores using 
# functions class() and sapply(). 

### write your code here

# Now the columns containing numerical scores should all be class 
# "numeric", with some NAs in them, representing scores that are 
# not available. 


# 2. (20pts) Calculate a vector called "num_nas" containing the 
# number of NA scores for each student, and cbind() it to 
# student_scores as the last (9th) column,
# You can use functions apply(), cbind(), sum(), is.na(), 
# and an anonymous function, 

### write your code here

# Sort student_scores in descending order by column "num_nas", 
# use function order()

### write your code here


# 3. (10pts) Calculate a vector called avg_score containing 
# the average score of each student, and bind it to 
# student_scores as the last (10th) column,
# Remember to omit NA values. 
# You can use functions apply(), cbind(), and mean(). 
# You cannot use an anonymous function. 

### write your code here


# 4. (20pts) Assign letter grades to each student, based on 
# their avg_score column. 
# First calculate a histogram of avg_score values, using the 
# function hist(), with the Freedman-Diaconis rule for 
# determining the breakpoints. 
# The function hist() invisibly returns a list that includes 
# a vector of breakpoints called "breaks". 
# Assign the return value of function hist() to a list 
# called student_hist

### write your code here

# Calculate a vector of letter grades corresponding to the 
# avg_score values, using the "breaks" field in student_hist, 
# and call it letter_grades. 
# You must use function findInterval(), 

### write your code here

# letter_grades is an integer vector from 1 to 5, 
# with 1 corresponding to the highest avg_score 
# category, and 5 corresponding to the lowest category. 
# Convert letter_grades to a vector of strings 
# representing letter grades. 
# Use the following vector of strings called grade_s: 

### write your code here

# Convert the letter_grades to a vector of strings, 
# by assigning 1 to letter grade "A", 2 to letter 
# grade "B", etc.

### write your code here

# cbind() letter_grades to the data frame student_scores as 
# the last column. 

### write your code here

