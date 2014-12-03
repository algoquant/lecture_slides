#################################
### Test #2 12/01/14 solutions
#################################
# Max score 60pts

# The below solutions are an example,
# Slightly different solutions are also possible.


# 1. (5pts) download and read the comma-delimited CSV file called "drawdowns.csv",
#    you can use functions "read.csv", "readLines", "scan", or any other functions you choose,
#    you may also want to use the option "stringsAsFactors",
#    the first column of "drawdowns.csv" are rownames,
#    call the resulting data frame as "draw_downs",
#    hint: open "drawdowns.csv" in Notepad or some other text editor,
draw_downs <- read.csv(file="C:/Develop/R/FRE6871/drawdowns.csv")
# or better:
draw_downs <- read.csv(file="C:/Develop//R/FRE6871/drawdowns.csv", stringsAsFactors=FALSE)


# 2. (15pts) replace colnames of "draw_downs" with the strings in the first row of "draw_downs",
#    if necessary, convert factors into character strings, 
#    colnames should be strings, starting with "From", "Trough", ...
#    hint: you may need to use "sapply",
colnames(draw_downs) <- sapply(draw_downs[1, ], as.character)
# or if "stringsAsFactors=FALSE" was used then:
colnames(draw_downs) <- draw_downs[1, ]
#    remove the first row,
draw_downs <- draw_downs[-1, ]
colnames(draw_downs)


# 3. (25pts) convert the first three columns of "draw_downs" to "POSIXct" dates, 
#    with "tz" set to "America/New_York",
#    you can use function "as.POSIXct" with "format" options,
#    you can also use package "lubridate", and functions "mdy", "dmy_hm", etc.,
#    the first column of "draw_downs" are dates formatted as "month-day-year",
#    the second column are dates as "year-month-day",
#    the third column are dates as "day-month-year",
library(lubridate)
draw_downs[, 1] <- mdy(draw_downs[, 1], tz="America/New_York")
draw_downs[, 2] <- ymd(draw_downs[, 2], tz="America/New_York")
draw_downs[, 3] <- dmy_hm(draw_downs[, 3], tz="America/New_York")
# or:
draw_downs[, 1] <- as.POSIXct(draw_downs[, 1], format="%m/%d/%Y", tz="America/New_York")
draw_downs[, 2] <- as.POSIXct(draw_downs[, 2], format="%y-%m-%d", tz="America/New_York")
draw_downs[, 3] <- as.POSIXct(draw_downs[, 3], format="%d/%m/%Y", tz="America/New_York")


# 4. (15pts) convert the remaining columns to numeric, 
#    hint: you may need to use "sapply",
draw_downs[, 4:7] <- sapply(draw_downs[, 4:7], as.character)
draw_downs[, 4:7] <- sapply(draw_downs[, 4:7], as.numeric)
# or if "stringsAsFactors=FALSE" was used then:
draw_downs[, 4:7] <- sapply(draw_downs[, 4:7], as.numeric)

