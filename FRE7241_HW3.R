#################################
### FRE7241 HW #3 due July 3, 2015
#################################
# Max score 45pts

# Please write in this file the R code needed to perform the tasks below, 
# rename it to your_name_hw3.R
# and send this file to Jaimin Doshi (jbd316@nyu.edu)



##################################
# extract price & volume columns using eapply() from all the objects contained in "env_data"
# use assign() to create new objects (returns) in env_data

# 1. (5pts) Load time series data from file "etf_series.Rdata",
# create a new environment called "env_data", for storing the "xts" 
# containing stock prices,
# load data from the file "etf_series.Rdata" into "env_data",
# use functions new.env() and load(), with the "envir" argument,

### write your code here



# 2. (20pts) create a function called get_returns_volume(), 
# that accepts an "xts" argument ("x_ts") and an environment 
# argument ("envir"), 
# get_returns_volume() should:
# - extract adjusted prices and volume data from the "xts",
# - calculate returns from adjusted prices,
# - extract the symbol name from the columns of "xts" ("symbol_name"),
# - merge returns with volume data into a single "xts" ("return_volume"),
# - rename the colnames of "return_volume" to "symbol_name.Return" 
#   and "symbol_name.Volume", (replace "symbol_name" with the symbol name),
# - assign (copy) "return_volume" to a object named "symbol_name_rets" 
#   in the "envir" environment, 
# get_returns_volume() should produce the side effect of creating 
# an "xts" object in the "envir" environment containing returns and 
# volume data "from the input "x_ts",
# get_returns_volume() should return invisible the "symbol_name",
# you can use functions Ad(), Vo(), strsplit(), colnames(), 
# paste() (or paste0), assign(), invisible(), and dailyReturn(),

### write your code here



# 2. (20pts) create a new environment called "env_returns", 
# for storing "xts" containing stock return and volume data,
# use function new.env(),

### write your code here


# apply function get_returns_volume() to a single "xts", to verify 
# it works correctly:

### write your code here



# perform an eapply() loop to apply get_returns_volume() to all  
# the objects in "env_data", and copy to "env_returns",
# use functions get_returns_volume() and eapply(),

### write your code here


# remove all files from "env_returns",

### write your code here


# perform a for() loop to apply get_returns_volume() to all  
# the objects in "env_data", and copy to "env_returns",
# use functions get_returns_volume() and for(),

### write your code here


# save all the objects in the environment "env_returns" 
# to a binary file called "etf_rets_volume.Rdata", 
# use function save(), with the "list" and "envir" arguments,
# make sure to save the objects in the environment, 
# not the environment itself,

### write your code here

