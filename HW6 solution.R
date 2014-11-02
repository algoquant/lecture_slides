#################################
### HW #6 Solution
#################################
# Max score 25pts

# The below solutions are examples,
# Slightly different solutions are also possible.

# comment:
# Half of the credit for the first part (max 15pts) is from properly calculating 
# the length (nrow) of the list object, because nrow() returns NULL for one-dimensional objects.
# Most of the rest required just copying and pasting from the lecture notes.


# Homework assignment:

# 1. (15pts) Create a function that summarizes time series objects called str_ts(),
# The function input is a time series object,
# The function should return a named list object with the following information: length (nrow), dimensions, number of rows with bad data, colnames, the object's class, data type, and the first and last rows of data,
# The function should validate its argument, and throw an error if it's not a time series object,

str_ts <- function(ts_series=NULL) {
# check if argument is a time series object
  stopifnot(is.ts(ts_series) || is.zoo(ts_series))
# create list and return it
  list(
    length=ifelse(is.null(nrow(ts_series)), length(ts_series), nrow(ts_series)),
    dim=dim(ts_series),
    bad_data=sum(!complete.cases(ts_series)),
    col_names=colnames(ts_series),
    ts_class=class(ts_series),
    ts_type=typeof(ts_series),
    first_row=head(ts_series, 1),
    last_row=tail(ts_series, 1)
  )  # end list
}  # end str_ts



# 2. (10pts) Create a synthetic zoo time series of prices with two named columns, based on random returns equal to "rnorm",
# Introduce a few NA values into the time series, and call str_ts() on this time series,
library(zoo)  # load package zoo
ts_var <- zoo(matrix(rnorm(20), ncol=2), order.by=(Sys.Date() - 1:10))
colnames(ts_var) <- paste0("col", 1:2)
ts_var[3, 1] <- NA
ts_var[6, 2] <- NA
str_ts(ts_var)

