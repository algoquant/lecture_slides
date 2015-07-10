#################################
### FRE6871 Test #2 Solutions June 22, 2015
#################################
# Max score 50pts

# The below solutions are examples,
# Slightly different solutions are also possible.


# Skip this, since its was part of homework already,
##################################
# 1. (15pts) create a function called mult_dots(), 


##################################
# 2. (5pts) Calculate the vector of "class" attributes of the 
# columns of the data frame "Cars93", and call it "class_cars",
# use functions class() and sapply(), 
library(MASS)
class_cars <- sapply(Cars93, class)

# calculate the vector of unique "class" attributes in "class_cars",
# use function unique(), 
unique(class_cars)


##################################
# 3. (10pts) calculate the number of columns with each 
# unique "class" attribute in "class_cars", 
# that is, calculate how many columns are of class "factor", 
# how many are of class "numeric", etc.
# use functions sapply(), sum(), and an anonymous function,
sapply(unique(class_cars), function(unique_class) {
  sum(class_cars==unique_class)
})  # end sapply

# comment: the above is equivalent to using table() below, 
# but the test requires using functions sapply(), sum(), 
# and an anonymous function,
table(class_cars)


##################################
# 4. (10pts) extract the vectors of column names in each 
# unique "class", that is, extract the names of columns 
# of class "factor", the names of columns of class "numeric", etc.
# use functions sapply(), colnames(), and an anonymous function,
sapply(unique(class_cars), function(unique_class) {
  colnames(Cars93)[class_cars==unique_class]
})  # end sapply


##################################
# 5. (10pts) calculate the "means" of columns that are 
# either of class "integer" or "numeric",
# you can use functions sapply(), mean(), and the "%in%" operator,
# some elements of Cars93 are NA, so you will need to pass the 
# parameter "na.rm=TRUE" to mean(),
integer_or_numeric <- class_cars %in% c("numeric", "integer")
sapply(Cars93[, integer_or_numeric], mean, na.rm=TRUE)



