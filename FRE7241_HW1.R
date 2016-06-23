#################################
### FRE7241 Homework #1 due April 19, 2016
#################################
# Max score 100pts

# Please write in this file the R code needed to perform the tasks below, 
# rename it to your_name_hw1.R
# and upload the file to NYU Classes


############## Part I
# Summary: Create a function which returns a list of attributes of 
# a time series object. Coerce time series objects into class xts, 
# and list their attributes. 

# 1. (20pts) Create a function called attri_butes() which returns a list 
# of attributes of a time series object. 
# The function attri_butes() should accept a single input called time_series, 
# and verify that it's a time series object of either class ts, zoo, or xts, 
# and if not, then it should produce and error and stop. 
# hint: you can use functions stopifnot(), is.ts(), is.zoo(), and is.xts(), 
# and the "||" operator. 
# 
# The function attri_butes() should return a named list of data 
# with the following information about the input time_series: 
# - dimensions, 
# - number of rows, 
# - number of rows with missing values (NA), 
# - number of columns, 
# - column names, 
# - the time_series class, 
# - the time index class of time_series, 
# - the first and last rows of time_series, 
# hint: you can use functions list(), dim(), if(), is.null(), 
# nrow(), length(), sum(), complete.cases(), ncol(), 
# colnames(), class(), index(), head(), and tail().

### write your code here



# 2. (10pts) Coerce the EuStockMarkets time series into class xts, 
# and call it xts_eustocks. 
# The time index of xts_eustocks should be of class POSIXct, and 
# the timezone should be equal to "America/New_York". 
# hint: you can use functions coredata(), xts(), index(), 
# and date_decimal(). 

### write your code here

# plot all 4 columns of xts_eustocks in a single panel:

### write your code here

# call attri_butes() as follows, to verify that it works correctly: 

attri_butes(EuStockMarkets)
attri_butes(EuStockMarkets[, 1])
attri_butes(xts_eustocks)
attri_butes(xts_eustocks[, 1])



############## Part II
# Summary: The package "Ecdat" contains a data frame called "Garch". 
# The column Garch$date contains dates as numeric values in the 
# format "yymmdd". 
# Coerce the numeric values into date-time objects. 

# 1. (10pts) Create a vector of dates of class Date from Garch$date, 
# and call it "in_dex". 
# You will need to add the century value "19" to the year. 
# You can use functions paste() and as.Date(), with the proper 
# "format" argument. 

library(Ecdat)  # load econometric data sets

### write your code here


# Use three different methods to create a vector of POSIXct dates 
# from Garch$date, and call it "in_dex". 
# 
# 2. (10pts) First method: create strings in the format "19yy-mm-dd", 
# and then coerce them into POSIXct.  
# hint: Extract substrings corresponding to the year, month, and day 
# using substr(), and then combine them using paste(). 
# Apply function as.POSIXct() and set the timezone to "America/New_York". 

### write your code here


# 3. (10pts) Second method: create strings in the format "19yymmdd", 
# and then coerce them into POSIXct. 
# hint: Apply function as.POSIXct() with the proper "format" argument,
# and set the timezone to "America/New_York". 

### write your code here


# 4. (10pts) Third method: use function ymd() from package lubridate,
# and set the timezone to "America/New_York", 

### write your code here


# 5. (20pts) Create an xts object called xts_series from the 
# columns Garch$dm and Garch$cd, and the vector in_dex. 
# Use functions c() and xts(). 

### write your code here

# Change the column names of xts_series to "DMark" and "CAD". 
# Use functions c() and colnames(). 

### write your code here

# Calculate the rolling mean of the "DMark" column of xts_series, 
# over a window of 11 points in the past, and call the result dm_mean,
# Use the function runMean() from package TTR. 

library(TTR)

### write your code here

# Add dm_mean as the third column of xts_series, 
# using the generic function cbind(). 

### write your code here

# Change the third column name of xts_series to "dm_mean". 
# Use function colnames(). 

### write your code here

# Calculate the number of NA values in xts_series. 
# Use functions is.na() and sum(). 

### write your code here

# Remove any NAs in xts_series using na.omit(). 

### write your code here

# Subset xts_series to the dates from "1982-01-01" to "1986-01-01". 

### write your code here

# 6. (10pts) Plot the "DMark" and "dm_mean" columns of xts_series, 
# for the years from 1984 to 1985, using the generic function plot(). 
# Add a legend using the function legend(). 

### write your code here

# Save xts_series to a comma-delimited csv file called "dmcad.csv". 
# Use function write.zoo(). 

### write your code here

