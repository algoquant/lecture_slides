#################################
### HW #5 Solution
#################################
# Max score xxpts

# The below solutions are examples,
# Slightly different solutions are also possible.


# download from Yahoo the \texttt{EOD CLOSE} quotes for \texttt{MSFT} stock, starting from Sep/01/2013,
# smooth the time series using \texttt{rollmean()} over a 7-day window,
# bind the original time series together with its smoothed version using \texttt{merge()},
# replace \texttt{NA} values using \texttt{na.locf()}, first going forward, and then backward in time,
# plot the two time series together on the same plot,
# find what is the \texttt{class} of the \texttt{index} of the time series,
# create a vector of weekly dates of class \texttt{Date} corresponding to every Monday, starting with: \texttt{as.Date("2013-09-02")}, until the most recent Monday (use \texttt{lubridate} function \texttt{weeks()}),
# find the \texttt{MSFT} \texttt{CLOSE} quotes for every Monday, by subsetting the original time series,
# calculate the weekly returns, and find the weeks with the highest and lowest returns,


# 1. (15pts) Create a function called "re_move", which takes two arguments:
#    the first argument can be a numeric or string, the second argument must be a vector,
#    "re_move" removes the first argument from the second argument, and returns the result,
#    if the first argument isn't among the elements of the second argument, 
#    then "re_move" just returns the second argument unchanged,
#    you can use the functions "match", "which", or the operator "%in%", but you don't have to,
re_move <- function(zap_it, vec_tor) {
  name_match <- match(zap_it, vec_tor)
  if (is.na(name_match))
    vec_tor
  else
    vec_tor[-name_match]
}  # end re_move

# 1. (cont.) call the function "re_move" as follows, to make sure it works properly:
re_move(3, 1:5)
re_move(6, 1:5)
re_move("bye", c("hello", "there"))



# 2. (5pts) Extract (subset) the DAX index from the EuStockMarkets dataset, 
#    and coerce it into a single "zoo" time series object called "dax_series",
#    the index of "dax_series" will have dates in "year-fraction" format, 
#    convert the "dax_series" index dates into "POSIXct" objects using 
#    functions from the package "lubridate",
dax_series <- as.zoo(EuStockMarkets[, 1])
library(lubridate)
index(dax_series) <- date_decimal(index(dax_series))


# 3. (5pts) create a "zoo" time series called "zoo_series", with exactly the same index as "dax_series",
#    the values of "zoo_series" should be random prices generated using a "cumsum" of "rnorm",
zoo_series <- zoo(cumsum(rnorm(length(dax_series))), order.by=index(dax_series))


# 4. (5pts) merge "zoo_series" with "dax_series" into a single "zoo" time series called "zoo_series", with two columns,
#    rename the columns of "zoo_series" to "random prices" and "DAX",
#    plot "zoo_series" in two panels,
zoo_series <- merge(zoo_series, dax_series)
colnames(zoo_series) <- c("random prices", "DAX")
plot(zoo_series, main="Random prices and DAX")

# 5. (5pts) save "zoo_series" to a comma-delimited CSV file called "zoo_series.csv", using write.zoo(),
#    save "zoo_series" to a binary file called "zoo_series.Rdata", using save(),
write.zoo(zoo_series, file='zoo_series.csv', sep=",")
save(zoo_series, file='zoo_series.Rdata')


