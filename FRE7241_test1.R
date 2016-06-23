#################################
### FRE7241 Test #1 April 26, 2016
#################################
# Max score 90pts

# Please write in this file the R code needed to perform the tasks below, 
# rename the file to your_name_test1.R
# and upload the file to NYU Classes

############## Part I
# 1. (10pts) Perform an sapply() loop to calculate a named 
# vector containing the number of NA values in the columns 
# of etf_rets. 
# You can use functions sum(), is.na(), and sapply(). 

library(quantmod)
load(file="C:/Develop/data/etf_data.RData")

### write your code here

# You should get the following output:
# VTI VEU IEF VNQ DBC VXX XLY XLP XLE XLF XLV XLI XLB XLK XLU VYM IVW IWB IWD IWF 
#   0  45   0   0   0 524   0   0   0   0   0   0   0   0   0   0   0   0   0   0 


############## Part II
# Summary: Create a data frame of value ETFs. 
# Add an investment style field to the data frame of ETFs. 

# 1. (20pts) Download the file "etf_list.csv" from NYU Classes, 
# and read it into a data frame called etf_list using read.csv(). 
# etf_list is a database of ETFs. 
# Use the argument "stringsAsFactors=FALSE" to avoid creating 
# factors. 

### write your code here

# Extract from etf_list a data frame of ETFs whose "Name" 
# field contains the keyword "Value", and call it value_etfs. 
# For example, if the "Name" field is equal to: 
#  "Vanguard Small-Cap Value ETF" 
# then that ETF should be selected into value_etfs. 
# You can use functions grep() and glob2rx(). 
# Look into the file data_structures.pdf to find 
# examples of using functions grep() and glob2rx(). 

### write your code here

# Save value_etfs to a comma-delimited CSV file called 
# "value_etfs.csv".
# Use function write.csv(),

### write your code here


# 2. (20pts) Add a field (column) to etf_list called "style", 
# and set it equal to NA. 

### write your code here

# Set the "style" field equal to "Value" for all the ETFs in 
# the etf_list, that are contained in the value_etfs data frame. 
# You can use data frame subsetting and the %in% operator. 

### write your code here



############## Part III
# Summary: Subset daily stock prices to dates at the 
# end of the week, and calculate percentage returns 
# over past four week intervals.

# Download the file etf_data.RData from NYU Classes, 
# and load() it. 

library(quantmod)
load(file="C:/Develop/data/etf_data.RData")

# etf_data.RData contains an environment called env_data, 
# with OHLC time series data for ETFs, including "VTI". 

# 1. (20pts) Extract the adjusted close prices from 
# "VTI" into a variable called price_s.
# You can use function Ad() from package quantmod. 

### write your code here

# Subset price_s to select prices from the dates at the 
# end of each week. (copy over price_s)
# You can use function endpoints() with the "on" argument, 

### write your code here

# Convert the tail of the index of price_s into days 
# of the week, to verify that the dates of price_s are 
# indeed "Friday". (except for some holidays)
# You can use functions weekdays(), index(), and tail(). 

### write your code here

# Calculate the percentage returns from price_s, over 
# trailing four week intervals, and call it re_turns. 
# You can use functions log() and diff() with the 
# "lag" argument, 

### write your code here


# 2. (20pts) Extract the highest and lowest returns, 
# and their associated dates. 
# You can use functions index(), which(), or 
# which.max() and which.min(). 

### write your code here

