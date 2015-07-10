#################################
### FRE6871 Test #2 June 22, 2015
#################################
# Max score 50pts

# Please write in this file the R code needed to perform the tasks below, 
# rename the file to your_name_test2.R
# and send the file to Jaimin Doshi (jbd316@nyu.edu)


##################################
# 1. (15pts) create a function called mult_dots(), 
# which takes '...' as its first argument, 
# and "fac_tor" as its second argument, as follows: mult_dots(..., fac_tor),
# The function mult_dots() should sum up the '...' argument, 
# then multiply the sum by "fac_tor", and return the result,

### write your code here

# apply the function mult_dots() so that it adds the numbers "1, 2, 3", 
# and then multiplies the sum by "2",

### write your code here



##################################
# 2. (5pts) Calculate the vector of "class" attributes of the 
# columns of the data frame "Cars93", and call it "class_cars",
# use functions class() and sapply(), 
library(MASS)

### write your code here

# calculate the vector of unique "class" attributes in "class_cars",
# use function unique(), 

### write your code here


##################################
# 3. (10pts) calculate the number of columns with each 
# unique "class" attribute in "class_cars", 
# that is, calculate how many columns are of class "factor", 
# how many are of class "numeric", etc.
# use functions sapply(), sum(), and an anonymous function,

### write your code here


##################################
# 4. (10pts) extract the vectors of column names in each 
# unique "class", that is, extract the names of columns 
# of class "factor", the names of columns of class "numeric", etc.
# use functions sapply(), colnames(), and an anonymous function,

### write your code here


##################################
# 5. (10pts) calculate the "means" of columns that are 
# either of class "integer" or "numeric",
# you can use functions sapply(), mean(), and the "%in%" operator,
# some elements of Cars93 are NA, so you will need to pass the 
# parameter "na.rm=TRUE" to mean(),

### write your code here



