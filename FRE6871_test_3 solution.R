#################################
### FRE6871 Test #3 Solutions 05/11/15
#################################
# Max score 60pts

# The below solutions are examples,
# Slightly different solutions are also possible.


##################################
# 1. (20pts) Load the package "Ecdat", which contains a data frame called "Yen",
# the column Yen$date contains integers representing dates, in the format "yyyymmdd",
# from the column Yen$date create a vector of "Date" dates, and call it "in_dex", 
# from the vector "in_dex" create a vector of strings corresponding to the year and month, 
# and call it "mon_ths", 
# use functions as.character(), as.Date(), and format(), 

library("Ecdat")  # load Ecdat

in_dex <- as.Date(as.character(Yen$date), format="%Y%m%d")
mon_ths <- format(in_dex, "%Y-%m")


# create a data frame called "jp_yen", 
# make the first column equal to "in_dex", the second column equal to "mon_ths", 
# and the third column equal to Yen$s,
# name the columns "date", "month", and "jpy",
# use function data.frame(), 

jp_yen <- data.frame(date=in_dex, month=mon_ths, jpy=Yen$s, 
                     stringsAsFactors=FALSE)
head(jp_yen)
sapply(jp_yen, class)
class(jp_yen$in_dex)



##################################
# 2. (20pts) Calculate a matrix of OHLC prices for all the months, 
# and call it "jpy_ohlc", 
# the matrix "jpy_ohlc" should have rows corresponding to months, 
# and columns corresponding to OHLC prices with names: "Open", "High", "Low", "Close",
# use the split-apply-combine procedure,
# you can perform the split-apply-combine procedure in several different ways,
# you can use functions sapply(),lapply(), tapply(), split(), with(), do.call(), rbind(), drop(),

# first method, only using sapply() and t(),

jpy_ohlc <- sapply(split(jp_yen, jp_yen$month), 
                   function(month) 
                     with(month, 
                          c(Open=jpy[1], 
                            High=max(jpy), 
                            Low=min(jpy), 
                            Close=jpy[length(jpy)]))  # end with
)  # end lapply

# transpose
jpy_ohlc <- t(jpy_ohlc)


# second method, only using split() and lapply(),

jpy_ohlc <- lapply(split(jp_yen, jp_yen$month), 
  function(month) 
    with(month, 
      c(Open=jpy[1], 
        High=max(jpy), 
        Low=min(jpy), 
        Close=jpy[length(jpy)]))  # end with
  )  # end lapply

# flatten to matrix
jpy_ohlc <- do.call(rbind, jpy_ohlc)

class(jpy_ohlc)
head(jpy_ohlc)
length(jpy_ohlc)
dim(jpy_ohlc)


# third method, using tapply(),

jpy_ohlc <- tapply(jp_yen$jpy, jp_yen$month, 
                   function(jpy) 
                     c(Open=jpy[1], 
                       High=max(jpy), 
                       Low=min(jpy), 
                       Close=jpy[length(jpy)])
                   )  # end tapply

# flatten to matrix
jpy_ohlc <- do.call(rbind, jpy_ohlc)


# fourth method, using tapply() and with(),

jpy_ohlc <- with(jp_yen, 
                 tapply(jpy, month, 
                        function(jpy) 
                          c(Open=jpy[1], 
                            High=max(jpy), 
                            Low=min(jpy), 
                            Close=jpy[length(jpy)])
                        )  # end tapply
                 )  # end with

# flatten to matrix
jpy_ohlc <- do.call(rbind, jpy_ohlc)



##################################
# 3. (20pts) Create a vector of end-of-month "Date" dates from the "date" column of "jp_yen", 
# and call it "eom_dates", 
# "eom_dates" should be equal to the last day of each month in the "date" column of "jp_yen", 
# use the split-apply-combine procedure,
# you can perform the split-apply-combine procedure in several different ways,
# you can use functions lapply(), tapply(), split(), with(), do.call(), rbind(), drop(),

eom_dates <- with(jp_yen, tapply(date, month, max))
# remove dimensions of length one
eom_dates <- drop(eom_dates)
# coerce to "Date"
eom_dates <- as.Date(eom_dates)
head(eom_dates)


# create a data frame called "jpy_ohlc", from "eom_dates" and the matrix "jpy_ohlc",
# the first column should be equal to "eom_dates", 
# while the remaining columns should be the OHLC prices,

jpy_ohlc <- data.frame(eom_dates, jpy_ohlc, 
                     stringsAsFactors=FALSE)

