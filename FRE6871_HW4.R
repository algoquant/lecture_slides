#################################
### FRE6871 HW #4 due 05-11-15
#################################
# Max score 50pts

# Please write in this file the R code needed to perform the tasks below, 
# rename it to your_name_hw4.R
# and send this file to Harjinder Singh (harjinder.singh@nyu.edu)


##################################
# 1. (10pts) Read file containing date, time, and numeric data,
# the file "time_series.txt" (uploaded to NYU Classes) contains 
# time series data in space-delimited format,
# the first two columns contain the date and time, 
# while the third contains numeric data,
# the file also contains a header with two strings,
# 
# read the data using function read.table() or scan() (read.table is easier),
# and call the resulting data frame "data_frame",
# if you're using function read.table(), then use the proper argument "skip",
# to skip over the header when reading the data,
# also use the proper argument "stringsAsFactors" to avoid creating factors,


### write your code here



# 2. (20pts) Formatting dates and times,
# the first two columns of "data_frame" contain strings representing the date and time, 
# combine the first two columns of "data_frame" into a vector of strings, 
# and call them "date_time",
# use function paste(),

### write your code here


# convert the vector of strings into "POSIXct" in the UTC time zone, 
# using function as.POSIXct(),
# use the proper argument "format" that is appropriate for the strings,
# read help under ?as.POSIXct to learn more about "format",

### write your code here



# 3. (20pts) Formatting the data frame and its header,
# combine "date_time" with the third column of "data_frame" into a data frame, 
# and call it "data_frame",
# use function data.frame(),

### write your code here


# read the first line of the file "time_series.txt" which contains the header, 
# and call it "col_names",
# use function readLines(),

### write your code here


# "col_names" contains a single string which is the concatenation 
# of the header strings,
# extract the header strings from "col_names" 
# using function strsplit(), with the proper argument "split",

### write your code here


# assign the header strings to the column names of "data_frame"
# using function colnames(), 

### write your code here
