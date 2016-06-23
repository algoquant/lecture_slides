#################################
### FRE7241 Test #1 Solutions April 26, 2016
#################################
# Max score 90pts

# The below solutions are examples,
# Slightly different solutions are also possible.

############## Part I
# 1. (10pts) Perform an sapply() loop to calculate a named 
# vector containing the number of NA values in the columns 
# of etf_rets. 
# You can use functions sum(), is.na(), and sapply(). 

library(quantmod)
load(file="C:/Develop/data/etf_data.RData")
sapply(etf_rets, function(x_ts) sum(is.na(x_ts)))

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

etf_list <- read.csv(file='etf_list.csv', stringsAsFactors=FALSE)

# Extract from etf_list a data frame of ETFs whose "Name" 
# field contains the keyword "Value", and call it value_etfs. 
# For example, if the "Name" field is equal to: 
#  "Vanguard Small-Cap Value ETF" 
# then that ETF should be selected into value_etfs. 
# You can use functions grep() and glob2rx(). 
# Look into the file data_structures.pdf to find 
# examples of using functions grep() and glob2rx(). 

value_etfs <- etf_list[grep(glob2rx("*Value*"), etf_list$Name), ]

# Save value_etfs to a comma-delimited CSV file called 
# "value_etfs.csv".
# Use function write.csv(),

write.csv(value_etfs, file="value_etfs.csv")


# 2. (20pts) Add a field (column) to etf_list called "style", 
# and set it equal to NA. 

etf_list$style <- NA

# Set the "style" field equal to "Value" for all the ETFs in 
# the etf_list, that are contained in the value_etfs data frame. 
# You can use data frame subsetting and the %in% operator. 

etf_list[etf_list$Symbol %in% value_etfs$Symbol, ]$style <- "Value"



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

price_s <- Ad(env_data$VTI)

# Subset price_s to select prices from the dates at the 
# end of each week. (copy over price_s)
# You can use function endpoints() with the "on" argument, 

price_s <- price_s[endpoints(price_s, on="weeks"), ]

# Convert the tail of the index of price_s into days 
# of the week, to verify that the dates of price_s are 
# indeed "Friday". (except for some holidays)
# You can use functions weekdays(), index(), and tail(). 

weekdays(index(tail(price_s)))

# Calculate the percentage returns from price_s, over 
# trailing four week intervals, and call it re_turns. 
# You can use functions log() and diff() with the 
# "lag" argument, 

re_turns <- diff(log(price_s), lag=4)


# 2. (20pts) Extract the highest and lowest returns, 
# and their associated dates. 
# You can use functions index(), which(), or 
# which.max() and which.min(). 

re_turns[which.max(re_turns)]
index(re_turns[which.max(re_turns)])
re_turns[which.min(re_turns)]
index(re_turns[which.min(re_turns)])

