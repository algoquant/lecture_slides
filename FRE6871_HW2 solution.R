#################################
### HW #2 Solution
#################################
# Max score 25pts

# The below solutions are examples,
# Slightly different solutions are also possible.

# 1. (5pts) using the function which() perform the following,
#     create a data frame that is a subset of mtcars, 
#     and that contains only cars with 6 cylinders,
mtcars_6cyl <- mtcars[mtcars$cyl==6, ]
which(mtcars_6cyl$hp==max(mtcars_6cyl$hp))


# 2. (5pts) find the name of the car with the highest horsepower among 6 cylinder cars,
best_car <- mtcars_6cyl[which(mtcars_6cyl$hp==max(mtcars_6cyl$hp)), ]
rownames(best_car)

#     calculate the horsepower and the weight of that car,
best_car$hp
best_car$wt


# 3. (15pts) create a function called "mult_dots", which takes a '...' argument, 
#     and a single numeric argument called "fac_tor", as follows: function (..., fac_tor),
#     The function "mult_dots" should sum up the '...' argument, 
#     multiply the sum by "fac_tor", and return the result,
mult_dots <- function (..., fac_tor) {
  fac_tor*sum(...)
}  #  end mult_dots

#     apply the function "mult_dots" so that it adds "1, 2, 3", 
#     and then multiplies the sum by "2",
mult_dots(1, 2, 3, fac_tor=2)


