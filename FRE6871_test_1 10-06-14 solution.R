#################################
### Test #1 10/06/14 solutions
#################################
# Max score 55pts

# The below solutions are an example,
# Slightly different solutions are also possible.

# 1. load package Ecdat, to be able to access Garch data, 
library(Ecdat)  # load package Ecdat

# 2. the field Garch$date contains dates as strings in the format "yymmdd",
head(Garch)  # explore the data

# 3. (20pts) from field Garch$date create a vector of strings in the format "yyyy-mm-dd", using nested calls to paste() and substring(),
zoo_index <- paste(paste(19, substring(Garch$date, 1, 2), sep=""), 
                   substring(Garch$date, 3, 4), 
                   substring(Garch$date, 5, 6), 
                   sep="-")

# 4. (5pts) create a date-time index called "zoo_index", by coercing the vector of strings into a vector of POSIXct dates using as.POSIXct(),
zoo_index <- as.POSIXct(zoo_index)

# 5. (5pts) create a zoo object called "zoo_series" from field Garch$dm and from "zoo_index", using zoo(),
zoo_series <- zoo(Garch$dm, order.by=zoo_index)

# 6. (5pts) count the number of NA values in "zoo_series" using is.na(),
sum(is.na(zoo_series))

# 7. (5pts) remove any NAs in "zoo_series" using na.omit(),
zoo_series <- na.omit(zoo_series)

# 8. (5pts) save "zoo_series" to a text file called "zoo_series.txt", using write.zoo(),
write.zoo(zoo_series, file="zoo_series.txt")

# 9. (5pts) subset the "zoo_series" object to the dates from "1982-01-01" to "1986-01-01", using either logical operators ">", "<", "&", "|" or window(),
zoo_subindex <- (zoo_index > as.POSIXct("1982-01-01")) & (zoo_index < as.POSIXct("1986-01-01"))
zoo_subseries <- zoo_series[zoo_subindex]
# or
zoo_subseries <- window(zoo_series, start=as.POSIXct("1982-01-01"), end=as.POSIXct("1984-01-01"))

# 10. (5pts) plot "zoo_series" and add a title containing the word "rate" and the name of field Garch$dm, using paste() and colnames(),
plot(zoo_subseries, main=paste("rate", colnames(Garch)[3]))

