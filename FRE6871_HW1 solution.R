#################################
### FRE6871 HW #1 Solution
#################################
# Max score 50pts

# The below solutions are examples,
# Slightly different solutions are also possible.


##################################
# 1. (15pts) Create the character string "y = x1 + x2 + x3" from 
# characters "x", "y", "=", "+", and the vector 1:3, using the function 
# paste() with a collapse string,

paste("y", "=", paste(paste0("x", 1:3), collapse=" + "))



##################################
# 2. (25pts) create a numeric vector of length 10 containing random normal variates, using rnorm(),

vec_tor <- rnorm(10)


######
# assign the names: "el1, ..., el10", to the vector elements, 
# using the function paste(), with the proper "sep" argument, 
# (you can't use function c())

names(vec_tor) <- paste("el", 1:10, sep='')


######
# change the vector names to: "num1, ..., num10",  
# using the function gsub(), (you can't use functions c() or paste())

names(vec_tor) <- gsub("el", "num", names(vec_tor))


######
# extract the element named "num4",

vec_tor["num4"]


######
# change the vector names to: "my.num1, ..., my.num10", 
# using the function paste(), with the proper "sep" argument,

names(vec_tor) <- paste("my", names(vec_tor), sep='.')


######
# change the first vector element name back to: "num1", 
# using the function strsplit(), with the proper "split" argument,
# hint: strsplit() returns a list, subset it using [[1]],

names(vec_tor)[1] <- strsplit(names(vec_tor)[1], split='[.]')[[1]][2]
# or:
names(vec_tor)[1] <- strsplit(names(vec_tor)[1], split='.', fixed=TRUE)[[1]][2]



##################################
# 3. (10pts) create a numeric vector of length 30 containing random normal variates, 
# using rnorm(),

vec_tor <- rnorm(30)


######
# get the indices of the elements that are greater than or equal to -0.5 and less than 1.0,

which((vec_tor >= -0.5) & (vec_tor < 1.0))


######
# extract the elements that are greater than or equal to -0.5 and less than 1.0,

vec_tor[(vec_tor >= -0.5) & (vec_tor < 1.0)]


######
# calculate the mean and standard deviation of the vector elements,

mean(vec_tor)
sd(vec_tor)


######
# combine this vector with the vector "31:33" inot a single vector,

c(vec_tor, 31:33)


