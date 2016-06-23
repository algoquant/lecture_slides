#################################
### FRE6871 HW #1 Solution due Sep 28, 2015
#################################
# Max score 60pts

# The below solutions are examples,
# Slightly different solutions are also possible.


##################################
# 1. (15pts) Create a vector of permutations of integers from 1 to 90, 
# and call it "vec_tor", 
# extract every third element of "vec_tor", starting with the first one, 
# into a single column matrix called "mat_rix",
# hint: you can use functions sample(), seq() and function matrix(), 
# with the proper "byrow" argument,

vec_tor <- sample(1:90)
mat_rix <- matrix(vec_tor[seq(from=1, to=length(vec_tor), by=3)], ncol=1)
# or:
mat_rix <- matrix(vec_tor, ncol=3, byrow=TRUE)
mat_rix <- mat_rix[, 1, drop=FALSE]


# 2. (15pts) extract a vector of odd index elements of "vec_tor" (first, third, etc), 
# and a vector of even index elements (second, fourth, etc.), 
# calculate the scalar ("inner") product of the two vectors, 
# the value should be a vector with a single element, not a matrix,
# hint: you can use the "%*%" operator, and the functions seq(), drop(), 
# or function matrix(), with the proper "byrow" argument,

odd_elements <- vec_tor[seq(from=1, to=length(vec_tor), by=2)]
even_elements <- vec_tor[seq(from=2, to=length(vec_tor), by=2)]
drop(odd_elements %*% even_elements)
# or:
mat_rix <- matrix(vec_tor, ncol=2, byrow=TRUE)
drop(mat_rix[, 1] %*% mat_rix[, 2])



##################################
# 3. (15pts) create a function called get_index(), that 
# returns the indices of the TRUE elements of a boolean vector,
# get_index() should be equivalent to the function which(), 
# when applied to boolean vectors,
# hint: you can use functions length(), seq_along(), 
# and then apply vector subsetting,

get_index <- function(vec_tor) (seq_along(vec_tor))[vec_tor]
# or
get_index <- function(vec_tor) (1:length(vec_tor))[vec_tor]

# apply the function get_index() to a boolean vector, and 
# compare the result with using function which(),

vec_tor <- sample(1:9)
get_index(vec_tor==5)
which(vec_tor == 5)



##################################
# 4. (15pts) create a numeric vector of length 10 containing random normal 
# variates, using rnorm(),
vec_tor <- rnorm(10)

# assign the names: "el1, ..., el10", to the vector elements, 
# you can use function paste(), with the proper "sep" argument, 
# you can't use function c()

names(vec_tor) <- paste("el", 1:10, sep='')

# change the vector names to: "num1, ..., num10", using the function gsub(), 
# (you can't use functions c() or paste())

names(vec_tor) <- gsub("el", "num", names(vec_tor))

# extract the element named "num4",

vec_tor["num4"]

# change the vector names to: "my.num1, ..., my.num10", 
# using the function paste(), with the proper "sep" argument,

names(vec_tor) <- paste("my", names(vec_tor), sep='.')

# change the first vector element name back to: "num1", 
# using the function strsplit(), with the proper "split" argument,
# hint: strsplit() returns a list, subset it using [[1]],

names(vec_tor)[1] <- strsplit(names(vec_tor)[1], split='[.]')[[1]][2]
# or:
names(vec_tor)[1] <- strsplit(names(vec_tor)[1], split='.', fixed=TRUE)[[1]][2]

# calculate the indices of the elements that are greater than 
# or equal to -0.5 and less than 1.0,

which((vec_tor >= -0.5) & (vec_tor < 1.0))

# extract the elements that are greater than or equal to -0.5 and less than 1.0,
vec_tor[(vec_tor >= -0.5) & (vec_tor < 1.0)]

# calculate the mean and standard deviation of the vector elements,

mean(vec_tor)
sd(vec_tor)

# combine this vector with the vector "31:33",
c(vec_tor, 31:33)


