#################################
### FRE6871 Homework #1 Solution due April 18, 2016
#################################
# Max score 70pts

# The below solutions are examples,
# Slightly different solutions are also possible.


############## Part I
# 1. (10pts) 
# Summary: Write code for performing operations in the R workspace, 
# change options settings and display them.

# remove all objects in the workspace:
rm(list=ls())

# set the max number of rows printed to console equal to 80:
# you can use function options(),
options(max.print=80)

# show the max number of rows printed to console:
# you can use function options(),
options("max.print")

# set the number of digits printed for numeric values equal to 3:
# you can use function options(),
options(digits=3)

# show the number of digits printed to console for numeric values:
# you can use function options(),
options("digits")

# display today's date and time in the format: 
# "Today is April 05, 2016 at 12:38:36"
paste("Today is", format(Sys.time(), "%B %d, %Y at %H:%M:%S"))

# Create objects called var1, var2, var3, 
# and assign the values rnorm(1) to them: 
var1 <- rnorm(1)
var2 <- rnorm(1)
var3 <- rnorm(1)

# list all objects with names starting with "v". 
# hint: you can use function glob2rx() and function 
# ls() with the "pattern" argument,
ls(pattern=glob2rx("v*"))

# save all objects with names ending with "1" to a file 
# called "vobjects.RData" in your cwd. 
# hint: you can use function save() with the "list" argument,
save(list=ls(pattern=glob2rx("*1")), file="my_data.RData")

# remove all objects with names starting with "v": 
rm(list=ls(pattern=glob2rx("v*")))



############## Part II
# Summary: create a function called which_true(), which calculates 
# the indices of the TRUE elements of a boolean vector. 
# which_true() should produce the same result as function which(), 
# when applied to boolean vectors. 
# Implement which_true() using two different methods. 

# 1. (20pts) First method: you must perform a for() loop. 
# hint: you can first create an empty integer vector, 
# and then perform a for() loop to populate it with the 
# index values. 
# you can use functions integer(), seq_along(), c(). 

which_true <- function(vec_tor){
  in_dex <- integer()
  for (i in seq_along(vec_tor)) {
    if (vec_tor[i])
      in_dex <- c(in_dex, i)
  }  # end for
  in_dex
}  # end which_true

# or a more complicated method:
which_true <- function(vec_tor){
  in_dex <- integer()
  j <- 1
  for (i in seq_along(vec_tor)) {
    if (vec_tor[i]) {
      in_dex[j] <- i
      j <- j + 1
    }  # end if
  }  # end for
  in_dex
}  # end which_true


# 2. (20pts) Second method: you cannot perform any type 
# of loop, only vectorized functions, 
# hint: you can use functions length() or seq_along(), 
# and then apply vector subsetting,

which_true <- function(vec_tor) (seq_along(vec_tor))[vec_tor]
# or
which_true <- function(vec_tor) (1:length(vec_tor))[vec_tor]

# apply the function which_true() to a boolean vector, and 
# compare the result with using function which(), to verify 
# that it works correctly:

set.seed(1121)
vec_tor <- sample(1:20, replace=TRUE)
which_true(vec_tor==18)
which(vec_tor==18)



############## Part III
# Summary: Create a function called kur_tosis(), for calculating 
# the kurtosis of a time series of returns (a vector of data). 

# 1. (10pts) The function kur_tosis() should accept a single numeric 
# argument called da_ta. 
# The function kur_tosis() should verify that da_ta is numeric, and 
# if it's not, then it should produce a warning and return NULL. 
# If da_ta is numeric, then kur_tosis() should calculate the kurtosis 
# of da_ta and return it. 
# The argument da_ta should be assigned a default value equal to a 
# vector of 1000 random numbers taken from the standard normal 
# distribution. 
# You can use functions is.numeric(), warning(), paste(), return(), 
# length(), mean(), sd(), and sum(). 

kur_tosis <- function(da_ta=rnorm(1000)) {
  if (!is.numeric(da_ta)) {
    warning(paste("argument", da_ta, "isn't numeric"))
    return(NULL)
  }  # end if
  len_data <- length(da_ta)
  mean_data <- mean(da_ta)
  sd_data <- sd(da_ta)
  len_data*(len_data+1)*sum(((da_ta-mean_data)/sd_data)^4)/((len_data-1)^3)
}  # end kur_tosis


# 2. (10pts) Use the function kur_tosis() to calculate the kurtosis  
# of DAX returns from the dataset EuStockMarkets. 
# Next, calculate the kurtosis of a vector of normal random numbers, 
# of the same length as the DAX returns. 
# Next, calculate the kurtosis of a vector of random numbers taken 
# from the t-distribution with four degrees of freedom, of the same 
# length as the DAX returns. 
# You can use the functions rt(), rnorm(), and length():

ts_rets <- 100*diff(log(EuStockMarkets[, 1]))

# call kur_tosis() as follows, to verify that it works correctly: 
kur_tosis(da_ta="hello")
kur_tosis()

# calculate kurtosis of DAX returns:
kur_tosis(da_ta=ts_rets)

# calculate kurtosis of normal returns:
kur_tosis(da_ta=rnorm(n=length(ts_rets)))

# calculate kurtosis of t-distribution returns:
kur_tosis(da_ta=rt(n=length(ts_rets), df=4))


