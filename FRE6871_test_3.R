#################################
### FRE6871 Test #3 05/11/15
#################################
# Max score 60pts

# Please write in this file the R code needed to perform the tasks below, 
# rename the file to your_name_test3.R
# and send the file to Harjinder Singh (harjinder.singh@nyu.edu)



##################################
# 1. (20pts) Load the package "Ecdat", which contains a data frame called "Yen",
# the column Yen$date contains integers representing dates, in the format "yyyymmdd",
# from the column Yen$date create a vector of "Date" dates, and call it "in_dex", 
# from the vector "in_dex" create a vector of strings corresponding to the year and month, 
# and call it "mon_ths", 
# use functions as.character(), as.Date(), and format(), 

library("Ecdat")  # load Ecdat

### write your code here


# create a data frame called "jp_yen", 
# make the first column equal to "in_dex", the second column equal to "mon_ths", 
# and the third column equal to Yen$s,
# name the columns "date", "month", and "jpy",
# use function data.frame(), 

### write your code here



##################################
# 2. (20pts) Calculate a matrix of OHLC prices for all the months, 
# and call it "jpy_ohlc", 
# the matrix "jpy_ohlc" should have rows corresponding to months, 
# and columns corresponding to OHLC prices with names: "Open", "High", "Low", "Close",
# use the split-apply-combine procedure,
# you can perform the split-apply-combine procedure in several different ways,
# you can use functions lapply(), tapply(), split(), with(), do.call(), rbind(), drop(),

# first method, only using split() and lapply(),

### write your code here



##################################
# 3. (20pts) Create a vector of end-of-month "Date" dates from the "date" column of "jp_yen", 
# and call it "eom_dates", 
# "eom_dates" should be equal to the last day of each month in the "date" column of "jp_yen", 
# use the split-apply-combine procedure,
# you can perform the split-apply-combine procedure in several different ways,
# you can use functions lapply(), tapply(), split(), with(), do.call(), rbind(), drop(),

### write your code here


# create a data frame called "jpy_ohlc", from "eom_dates" and the matrix "jpy_ohlc",
# the first column should be equal to "eom_dates", 
# while the remaining columns should be the OHLC prices,

### write your code here

