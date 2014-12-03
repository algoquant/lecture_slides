#################################
### Test #1 11/24/14
#################################
# Max score 60pts

# Please write in this file the R code needed to perform the tasks below, 
# rename it to your_name_test1.R
# and send this file to Xirui He (xh521@nyu.edu)


# 1. (30pts) "my_date" is a numeric date that represents "1997-05-18",
#     convert my_date to a POSIXct date, with time zone equal to "America/New_York",
#     you can use functions from package lubridate, 
#     or other functions such as "paste" and "substr",
#     you will need to add the century "19" to the year,
#     you will not receive any credit for creating a date "by hand" as follows: as.POSIXct("1997-05-18"),
my_date <- 970518



# 2. (30pts) create a function called "my_sqrt" that calculates the square root of its single argument,
#    "my_sqrt" should check if the input is numeric and positive,
#    if the input is numeric and positive, then "my_sqrt" should return the square root,
#    if the input is numeric and negative, then "my_sqrt" should broadcast a warning using "cat", 
#    and return the square root of the absolute value,
#    if the input is not numeric, then "my_sqrt" should broadcast a different warning using "cat", 
#    and return NaN,
#    call "my_sqrt" as follows, to verify that it performs as required:
my_sqrt(4)
my_sqrt(-4)
my_sqrt("a")


