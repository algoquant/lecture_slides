#################################
### FRE6871 HW #2 due June 22, 2015
#################################
# Max score 50pts

# Please write in this file the R code needed to perform the tasks below, 
# rename it to your_name_hw2.R
# and send this file to Jaimin Doshi (jbd316@nyu.edu)


##################################
# 1. (15pts) Create a function called match_matrix(), similar to match(), 
# but which accepts matrix arguments, as well as vectors,
# match_matrix() should return the row and column indices of the first 
# element of its second argument, that matches its first argument,
# hint: you can use function which(), with the argument "arr.ind=TRUE",

### write your code here

# call the function match_matrix() as follows, to make sure it works properly:
mat_rix <- matrix(1:6, ncol=3)
match_matrix(5, mat_rix)



##################################
# 2. (20pts) Create a function called re_move(), which takes two arguments:
# re_move() removes the first argument from the second argument, and returns the result,
# the first argument can be a single numeric or string vlaue, 
# the second argument can be a vector, or other object with mupltiple elements,
# if the first argument isn't among the elements of the second argument, 
# then re_move() just returns the second argument unchanged,
# you can use the functions match(), which(), 
# or the operator "%in%", but you don't have to use all of them,

### write your code here


# call the function re_move() as follows, to make sure it works properly:
re_move(3, 1:5)
re_move(6, 1:5)
re_move("bye", c("hello", "bye", "there"))
re_move("bye", c("hello", "there"))



##################################
# 3. (15pts) create a function called mult_dots(), which takes two arguments:
# the first argument is the '...' argument, 
# and the second argument is called "fac_tor", 
# the function mult_dots() arguments are as follows: mult_dots(..., fac_tor),
# mult_dots() should sum up the values in the '...' argument, 
# then multiply the sum by "fac_tor", and return the result,

### write your code here


# apply the function mult_dots() so that it adds the numbers "1, 2, 3", 
# and then multiplies the sum by "2",

### write your code here




