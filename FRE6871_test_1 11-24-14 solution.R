#################################
### Test #1 11/24/14 solutions
#################################
# Max score 60pts

# The below solutions are an example,
# Slightly different solutions are also possible.


# 1. (30pts) "my_date" is a numeric date that represents "1997-05-18",
#     convert my_date to a POSIXct date, with time zone equal to "America/New_York",
#     you can use functions from package lubridate, 
#     or other functions such as "paste" and "substr",
#     you will need to add the century "19" to the year,
#     you will not receive any credit for creating a date "by hand" as follows: as.POSIXct("1997-05-18"),
my_date <- 970518
library(lubridate)
ymd(paste0(19, my_date), tz="America/New_York")
# or
as.POSIXct(
  paste(paste0(19, substr(my_date, 1, 2)), 
        substr(my_date, 3, 4), 
        substr(my_date, 5, 6), sep="-"), 
  tz="America/New_York"
)  # end as.POSIXct



# 2. (30pts) create a function called "my_sqrt" that calculates the square root of its single argument,
#    "my_sqrt" should check if the input is numeric and positive,
#    if the input is numeric and positive, then "my_sqrt" should return the square root,
#    if the input is numeric and negative, then "my_sqrt" should broadcast a warning using "cat", 
#    and return the square root of the absolute value,
#    if the input is not numeric, then "my_sqrt" should broadcast a different warning using "cat", 
#    and return NaN,
my_sqrt <- function(arg_var) {
  if (is.numeric(arg_var) && arg_var>=0) {
    sqrt(arg_var)
  } else if (is.numeric(arg_var)) {
    cat("negative input!\t")
    sqrt(abs(arg_var))
  } else {
    cat("not numeric input!\t")
    NaN
  }
}  # my_sqrt
my_sqrt(4)
my_sqrt(-4)
my_sqrt("a")


