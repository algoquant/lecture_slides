#################################
### FRE7241 Test #2 Solutions 05/05/15
#################################
# Max score 50pts

# The below solutions are examples,
# Slightly different solutions are also possible.


# 1. (10pts) Load time series data and calculate returns,
# the file "etf_series.Rdata" contains time series data,
# create a new environment called "data_env",
# load data from the file "etf_series.Rdata" into "data_env",
# use function load(), with the "envir" argument,

data_env <- new.env()
load(file="C:/Develop/data/etf_series.Rdata", envir=data_env)


# perform an eapply loop to extract the adjusted prices for all 
# the variables in "data_env", and call it "etf_series_ad",

etf_series_ad <- do.call(merge, eapply(data_env, Ad))


# "etf_series_ad" should be an "xts" containing adjusted prices,
# with colnames in the format "name.Adjusted",
# rename the colnames and drop ".Adjusted" from the colnames,
# use functions sapply() and strsplit(),

colnames(etf_series_ad) <- sapply(colnames(etf_series_ad), 
    function(col_name) strsplit(col_name, split="[.]")[[1]])[1, ]


# scrub (remove) rows with NA values from "etf_series_ad",
# use function complete.cases(),

etf_series_ad <- etf_series_ad[complete.cases(etf_series_ad)]


# calculate an "xts" containing returns of "etf_series_ad", and call it "etf_rets",
# use functions lapply(), dailyReturn(), do.call(), and merge(),

# load package "quantmod",
library(quantmod)

etf_rets <- lapply(etf_series_ad, 
                   function(x_ts) {
                     daily_return <- dailyReturn(x_ts)
                     colnames(daily_return) <- names(x_ts)
                     daily_return
                   })  # end lapply

# flatten list of "xts" into a single "xts",

etf_rets <- do.call(merge, etf_rets)



# 2. (20pts) Create a function called "get_hyp_stats()" that returns hypothesis test stats,
# function "get_hyp_stats()" should accept a single "xts" argument called "re_turns", 
# The function get_hyp_stats() should perform the following steps:
#    perform Jarque-Bera test of normality on "re_turns",
#    perform Shapiro-Wilk test of normality on "re_turns",
#    return a named vector containing the Jarque-Bera and the Shapiro-Wilk statistics (not p.values!),
# use functions jarque.bera.test() and shapiro.test(), 
# be careful because shapiro.test() doesn't accept arguments of class "xts",

# load package "tseries"
library(tseries)

get_hyp_stats <- function(re_turns) {
# load package "tseries"
  stopifnot("package:tseries" %in% search() || require("tseries", quietly=TRUE))
  c(
    jarque_bera=unname(jarque.bera.test(re_turns)$statistic),
    shapiro=unname(shapiro.test(coredata(re_turns))$statistic))
}  # end get_hyp_stats

# apply get_hyp_stats() as follows, to verify it works properly:

get_hyp_stats(etf_rets[, 1])



# 3. (10pts) Apply function get_hyp_stats() to all the columns of "etf_rets", 
# and call the result "hyp_stats",
# the first column of "hyp_stats" should contain Jarque-Bera statistics, 
# while the second Shapiro-Wilk,
# the rownames of "hyp_stats" should contain the names of "etf_rets" columns, 
# use functions sapply() and t(), 

hyp_stats <- sapply(etf_rets, get_hyp_stats)
hyp_stats <- t(hyp_stats)



# 4. (10pts) Create a scatterplot of "hyp_stats", 
# and add labels containing the rownames of "hyp_stats",
# use functions plot() and text(),

plot(hyp_stats)
text(x=hyp_stats[, "jarque_bera"], 
     y=hyp_stats[, "shapiro"],
     labels=rownames(hyp_stats),
     pos=1, cex=0.8)


# sort "hyp_stats" on column "jarque_bera" in ascending (increasing) order,
# use function order(),

hyp_stats <- hyp_stats[order(hyp_stats[, "jarque_bera"], decreasing=FALSE), ]


# save "hyp_stats" to comma-delimited CSV file,
# use function write.csv(),

write.csv(hyp_stats, file='hyp_stats.csv')

