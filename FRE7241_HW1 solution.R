#################################
### FRE7241 Homework #1 Solution due April 19, 2016
#################################
# Max score 100pts

# The below solutions are examples,
# Slightly different solutions are also possible.


############## Part I
# Summary: Create a function which returns a list of attributes of 
# a time series object. Coerce time series objects into class xts, 
# and list their attributes. 

# 1. (20pts) Create a function called attri_butes() which returns a list 
# of attributes of a time series object. 
# The function attri_butes() should accept a single input called time_series, 
# and verify that it's a time series object of either class ts, zoo, or xts, 
# and if not, then it should produce and error and stop. 
# hint: you can use functions stopifnot(), is.ts(), is.zoo(), and is.xts(), 
# and the "||" operator. 
# 
# The function attri_butes() should return a named list of data 
# with the following information about the input time_series: 
# - dimensions, 
# - number of rows, 
# - number of rows with missing values (NA), 
# - number of columns, 
# - column names, 
# - the time_series class, 
# - the time index class of time_series, 
# - the first and last rows of time_series, 
# hint: you can use functions list(), dim(), if(), is.null(), 
# nrow(), length(), sum(), complete.cases(), ncol(), 
# colnames(), class(), index(), head(), and tail().

attri_butes <- function(time_series) {
  # check if argument is a time series object
  stopifnot(is.ts(time_series) || is.zoo(time_series) || is.xts(time_series))
  # create list and return it
  list(
    dim=dim(time_series), 
    nrows=if(is.null(nrow(time_series))) length(time_series) else nrow(time_series), 
    nrows_nas=sum(!complete.cases(time_series)), 
    ncols=if(is.null(ncol(time_series))) 1 else ncol(time_series), 
    col_names=if(is.null(colnames(time_series))) "none" else colnames(time_series), 
    ts_class=class(time_series), 
    ts_index_class=class(index(time_series)), 
    first_row=head(time_series, 1), 
    last_row=tail(time_series, 1)
  )  # end list
}  # end attri_butes


# 2. (10pts) Coerce the EuStockMarkets time series into class xts, 
# and call it xts_eustocks. 
# The time index of xts_eustocks should be of class POSIXct, and 
# the timezone should be equal to "America/New_York". 
# hint: you can use functions coredata(), xts(), index(), 
# and date_decimal(). 

library(lubridate)  # load lubridate
xts_eustocks <- xts(coredata(EuStockMarkets), 
                    order.by=date_decimal(index(EuStockMarkets), 
                                          tz="America/New_York"))

# plot all 4 columns of xts_eustocks in a single panel:

plot(xts_eustocks)


# call attri_butes() as follows, to verify that it works correctly: 

attri_butes(EuStockMarkets)
attri_butes(EuStockMarkets[, 1])
attri_butes(xts_eustocks)
attri_butes(xts_eustocks[, 1])



############## Part II
# Summary: The package "Ecdat" contains a data frame called "Garch". 
# The column Garch$date contains dates as numeric values in the 
# format "yymmdd". 
# Coerce the numeric values into date-time objects. 

# 1. (10pts) Create a vector of dates of class Date from Garch$date, 
# and call it "in_dex". 
# You will need to add the century value "19" to the year. 
# You can use functions paste() and as.Date(), with the proper 
# "format" argument. 

library(Ecdat)  # load econometric data sets
head(Garch)  # explore the data
in_dex <- as.Date(paste0(19, Garch$date), format="%Y%m%d")


# Use three different methods to create a vector of POSIXct dates 
# from Garch$date, and call it "in_dex". 
# 
# 2. (10pts) First method: create strings in the format "19yy-mm-dd", 
# and then coerce them into POSIXct.  
# hint: Extract substrings corresponding to the year, month, and day 
# using substr(), and then combine them using paste(). 
# Apply function as.POSIXct() and set the timezone to "America/New_York". 

in_dex <- as.POSIXct(
  paste(paste0(19, substr(Garch$date, 1, 2)), 
        substr(Garch$date, 3, 4), 
        substr(Garch$date, 5, 6), sep="-"), 
  tz="America/New_York")


# 3. (10pts) Second method: create strings in the format "19yymmdd", 
# and then coerce them into POSIXct. 
# hint: Apply function as.POSIXct() with the proper "format" argument,
# and set the timezone to "America/New_York". 

in_dex <- as.POSIXct(paste0(19, Garch$date), format="%Y%m%d", 
                     tz="America/New_York")


# 4. (10pts) Third method: use function ymd() from package lubridate,
# and set the timezone to "America/New_York", 

in_dex <- ymd(Garch$date, tz="America/New_York")


# 5. (20pts) Create an xts object called xts_series from the 
# columns Garch$dm and Garch$cd, and the vector in_dex. 
# Use functions c() and xts(). 

xts_series <- xts(Garch[, c("dm", "cd")], order.by=in_dex)

# Change the column names of xts_series to "DMark" and "CAD". 
# Use functions c() and colnames(). 

colnames(xts_series) <- c("DMark", "CAD")

# Calculate the rolling mean of the "DMark" column of xts_series, 
# over a window of 11 points in the past, and call the result dm_mean,
# Use the function runMean() from package TTR. 

library(TTR)
dm_mean <- runMean(x=xts_series[, "DMark"], n=11)

# Add dm_mean as the third column of xts_series, 
# using the generic function cbind(). 

xts_series <- cbind(xts_series, dm_mean)

# Change the third column name of xts_series to "dm_mean". 
# Use function colnames(). 

colnames(xts_series)[3] <- "dm_mean"

# Calculate the number of NA values in xts_series. 
# Use functions is.na() and sum(). 

sum(is.na(xts_series))

# Remove any NAs in xts_series using na.omit(). 

xts_series <- na.omit(xts_series)

# Subset xts_series to the dates from "1982-01-01" to "1986-01-01". 

xts_series <- xts_series["1982-01-01/1986-01-01"]

# 6. (10pts) Plot the "DMark" and "dm_mean" columns of xts_series, 
# for the years from 1984 to 1985, using the generic function plot(). 
# Add a legend using the function legend(). 

plot(xts_series["1984/1985", c("DMark", "dm_mean")])
legend(x="topleft", legend=c("DMark", "dm_mean"),
       inset=0.2, cex=0.7, bg="white",
       lwd=2, lty=c(1, 1), col=c("black", "red"))

# Save xts_series to a comma-delimited csv file called "dmcad.csv". 
# Use function write.zoo(). 

write.zoo(xts_series, file="dmcad.csv")

