#################################
### Test #2 10/20/14
#################################

# 1. load packages Ecdat and zoo, to access Garch data,
library(Ecdat)  # load package Ecdat
library(zoo)  # load package Ecdat


# 2. the column Garch$date contains dates as strings in the format "yymmdd",
head(Garch)  # explore the data


# 3. from column Garch$date create a vector of strings in the format "yyyy-mm-dd", using nested calls to paste() and substring(),
date_index <- paste(paste0(19, substring(Garch$date, 1, 2)), 
                    substring(Garch$date, 3, 4), 
                    substring(Garch$date, 5, 6), 
                    sep="-")


# 4. create a date-time index called "date_index", by parsing the vector of strings into a vector of POSIXct dates using as.POSIXct(),
date_index <- as.POSIXct(date_index)


# 5. create a zoo object called "zoo_series" from Garch columns "dm" and "cd" and from "date_index", using zoo(),


# 6. change the column names of zoo_series to "DMark" and "CAD"


# 7. plot zoo_series with two "y" axes, and add a legend containing the column names of zoo_series,
# plot the first column
# specify range of second axis
# plot second axis
# plot second column
# add legend


# 8. calculate the running mean of the "DMark" column of zoo_series, using the function rollmean(), 
# aggregate values from the past over a window of 11 points, and call the result "zoo_mean",


# 9. merge the "DMark" column of zoo_series with zoo_mean, using the function merge(), and copy it back to zoo_series,


# 10. change the column names of zoo_series to "DMark" and "mean_DMark"


# 11. calculate the number of rows with bad data in zoo_series, and then scrub the bad data using the function na.locf(),


# 12. plot both columns of zoo_series, with the second series in red, and add a legend
# plot the first column
# plot second column
# add legend


# 13. save zoo_series to a comma-delimited text file called "Garch.csv", using write.zoo()
