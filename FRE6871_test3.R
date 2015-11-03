#################################
### FRE6871 Test #3 Oct 13, 2015
#################################
# Max score 65pts

# Please write in this file the R code needed to perform the tasks below, 
# rename the file to your_name_test3.R
# and upload it to NYU Classes,


############## Part I
# 1. (20pts) calculate a vector with the number of NAs 
# in each column of "airquality", 
# you can use functions sapply(), is.na(), sum(), 
# and an anonymous function,

### write your code here

# replace the NAs in column "Solar.R" with zeros, 
# you can use function is.na(),

### write your code here


############## Part II
# 1. (5pts) Calculate the vector of "class" attributes of the 
# columns of the data frame "Cars93", and call it "class_cars",
# you can use functions class() and sapply(), 

library(MASS)

### write your code here

# calculate the vector of unique "class" attributes (elements) 
# in "class_cars",
# you can use function unique(), 

### write your code here

# 2. (20pts) calculate a list containing the column names in each 
# unique "class", that is, calculate the names of the columns 
# of class "factor", the names of the columns of class "numeric", etc.
# use functions lapply(), colnames(), and an anonymous function,

### write your code here

# 3. (20pts) calculate the "means" of columns that are 
# either of class "integer" or "numeric",
# you can use functions sapply(), mean(), and the "%in%" operator,
# some elements of Cars93 are NA, so you will need to pass the 
# parameter "na.rm=TRUE" to mean(),

### write your code here

