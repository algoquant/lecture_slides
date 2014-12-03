#################################
### Test #2 12/01/14
#################################
# Max score 60pts

# Please write in this file the R code needed to perform the tasks below, 
# rename the file to your_name_test2.R
# and send the file to Xirui He (xh521@nyu.edu)


# 1. (5pts) download and read the comma-delimited CSV file called "drawdowns.csv",
#    you can use functions "read.csv", "readLines", "scan", or any other functions you choose,
#    you may also want to use the option "stringsAsFactors",
#    the first column of "drawdowns.csv" are rownames,
#    call the resulting data frame as "draw_downs",
#    hint: open "drawdowns.csv" in Notepad or some other text editor,


# 2. (15pts) replace colnames of "draw_downs" with the strings in the first row of "draw_downs",
#    if necessary, convert factors into character strings, 
#    colnames should be strings, starting with "From", "Trough", ...
#    hint: you may need to use "sapply",


# 3. (25pts) convert the first three columns of "draw_downs" to "POSIXct" dates, 
#    with "tz" set to "America/New_York",
#    you can use function "as.POSIXct" with "format" options,
#    you can also use package "lubridate", and functions "mdy", "dmy_hm", etc.,
#    the first column of "draw_downs" are dates formatted as "month-day-year",
#    the second column are dates as "year-month-day",
#    the third column are dates as "day-month-year",


# 4. (15pts) convert the remaining columns to numeric, 
#    hint: you may need to use "sapply",

