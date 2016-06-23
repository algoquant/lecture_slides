#################################
### FRE6871 Test #3 Solutions Oct 13, 2015
#################################
# Max score 65pts

# The below solutions are examples,
# Slightly different solutions are also possible.

############## Part I
# 1. (20pts) calculate a vector with the number of NAs 
# in each column of "airquality", 
# you can use functions sapply(), is.na(), sum(), 
# and an anonymous function,

sapply(airquality, function(col_umn) sum(is.na(col_umn)))

# replace the NAs in column "Solar.R" with zeros, 
# you can use function is.na(),

airquality[is.na(airquality$Solar.R), "Solar.R"] <- 0


############## Part II
# 1. (5pts) Calculate the vector of "class" attributes of the 
# columns of the data frame "Cars93", and call it "class_cars",
# you can use functions class() and sapply(), 

library(MASS)
class_cars <- sapply(Cars93, class)

# calculate the vector of unique "class" attributes (elements) 
# in "class_cars",
# you can use function unique(), 

unique(class_cars)

# 2. (20pts) calculate a list containing the column names in each 
# unique "class", that is, calculate the names of the columns 
# of class "factor", the names of the columns of class "numeric", etc.
# you can use functions lapply(), sapply(), colnames(), 
# and an anonymous function,

lapply(unique(class_cars), function(unique_class) {
  colnames(Cars93)[class_cars==unique_class]
})  # end lapply
# or to preserve list element names:
sapply(unique(class_cars), function(unique_class) {
  colnames(Cars93)[class_cars==unique_class]
}, simplify=FALSE, USE.NAMES=TRUE)  # end sapply

# 3. (20pts) calculate the "means" of columns that are 
# either of class "integer" or "numeric",
# you can use functions sapply(), mean(), and the "%in%" operator,
# some elements of Cars93 are NA, so you will need to pass the 
# parameter "na.rm=TRUE" to mean(),

integer_or_numeric <- class_cars %in% c("numeric", "integer")
sapply(Cars93[, integer_or_numeric], mean, na.rm=TRUE)

