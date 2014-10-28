#################################
### Test #2 Solutions 10/20/14
#################################

# The below solutions are an example,
# Slightly different solutions are also possible.


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
zoo_series <- zoo(Garch[, c("dm", "cd")], order.by=date_index)


# 6. change the column names of zoo_series to "DMark" and "CAD"
colnames(zoo_series) <- c("DMark", "CAD")


# 7. plot zoo_series with two "y" axes, and add a legend containing the column names of zoo_series,
# plot the first column
plot(zoo_series[, 1], xlab=NA, ylab=NA)
# specify range of second axis
par(usr=c(par("usr")[1:2], range(zoo_series[, 2])))
# plot second axis
axis(side=4, col="red")
# plot second column
lines(zoo_series[, 2], col="red")
# add legend
legend("bottomleft", legend=colnames(zoo_series), bg="white", lty=c(1, 1), lwd=c(2, 2), col=c("black", "red"), bty="n")


# 8. calculate the running mean of the "DMark" column of zoo_series, using the function rollmean(), 
# aggregate values from the past over a window of 11 points, and call the result "zoo_mean",
zoo_mean <- rollmean(x=zoo_series[, "DMark"], k=11, align="right")


# 9. merge the "DMark" column of zoo_series with zoo_mean, using the function merge(), and copy it back to zoo_series,
zoo_series <- merge(zoo_series[, "DMark"], zoo_mean)


# 10. change the column names of zoo_series to "DMark" and "mean_DMark"
colnames(zoo_series) <- c("DMark", "mean_DMark")


# 11. calculate the number of rows with bad data in zoo_series, and then scrub the bad data using the function na.locf(),
sum(!complete.cases(zoo_series))
zoo_series <- na.locf(zoo_series, fromLast=TRUE)


# 12. plot both columns of zoo_series, with the second series in red, and add a legend
# plot the first column
plot(zoo_series[, 1], xlab="", ylab="")
# plot second column
lines(zoo_series[, 2], lwd=2, col="red")
# add legend
legend("bottomright", legend=colnames(zoo_series), bg="white", lty=c(1, 1), lwd=c(2, 2), col=c("black", "red"), bty="n")


# 13. save zoo_series to a comma-delimited text file called "Garch.csv", using write.zoo()
write.zoo(zoo_series, file='Garch.csv', sep=",")
