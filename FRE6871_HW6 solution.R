#################################
### HW #6 Solution
#################################
# Max score 35pts

# The below solutions are examples,
# Slightly different solutions are also possible.

# 1. (15pts) Create a function called "read_numeric" that reads numbers input by the user, and returns them in a vector,
#    "read_numeric" should ask the user to input a number, and should read the input using the function "readline",
#    "read_numeric" should read numbers from the console in a "while" loop,
#    "read_numeric" should validate the inputs, and produce errors and Warnings,
#    if the user input is numeric, then "read_numeric" should append the input to the numeric output vector,
#    if the input is not numeric, then "read_numeric" should produce a Warning "input is not numeric!",
#    if the input is empty, then "read_numeric" should terminate, and return the numeric output vector,

read_numeric <- function() {
  out_put <- numeric(0)
  nu_meric <- readline("Enter a number: ")
  while(nchar(nu_meric) > 0) {
    nu_meric <- as.numeric(nu_meric)
    if (!is.na(nu_meric)) {
      out_put <- c(out_put, as.numeric(nu_meric))
    } else {
      warning("input is not numeric!")
    }  # end if
    nu_meric <- readline("Enter a number: ")
  }  # end while
  out_put
}  # end read_numeric
read_numeric()


