#################################
### FRE7241 Test #3 Solutions 05/12/15
#################################
# Max score 50pts

# The below solutions are examples,
# Slightly different solutions are also possible.


##################################
# 1. (20pts) Load time series data and calculate returns,
# the file "etf_series.Rdata" contains time series of ETF prices,
# create a new environment called "data_env",
# load data from the file "etf_series.Rdata" into "data_env",
# use function load(), with the "envir" argument,

data_env <- new.env()
load(file="C:/Develop/data/etf_series.Rdata", envir=data_env)


# perform an eapply loop to extract the adjusted prices for all 
# the variables in "data_env", and call it "etf_series_ad",

# load packages "quantmod", "lubridate", and "PerformanceAnalytics",
library(quantmod)
library(lubridate)
library(PerformanceAnalytics)

etf_series_ad <- do.call(merge, eapply(data_env, Ad))

# "etf_series_ad" should be an "xts" containing adjusted prices,
# with colnames in the format "name.Adjusted",
# rename the colnames and drop ".Adjusted" from the colnames,
# use functions sapply() and strsplit(),

colnames(etf_series_ad) <- sapply(colnames(etf_series_ad), 
                  function(col_name) 
                    strsplit(col_name, split="[.]")[[1]])[1, ]


# scrub (remove) rows with NA values from "etf_series_ad",
# use function complete.cases(),

etf_series_ad <- etf_series_ad[complete.cases(etf_series_ad)]


# calculate an "xts" containing returns of "etf_series_ad", and call it "etf_rets",
# use functions lapply(), dailyReturn(), do.call(), and merge(),

etf_rets <- lapply(etf_series_ad, 
                   function(x_ts) {
                     daily_return <- dailyReturn(x_ts)
                     colnames(daily_return) <- names(x_ts)
                     daily_return
                   })  # end lapply

# flatten list of "xts" into a single "xts",

etf_rets <- do.call(merge, etf_rets)

# rearrange columns according to ETF symbols for asset allocation
sym_bols <- c("VTI", "VEU", "IEF", "VNQ", 
              "DBC", "XLY", "XLP", "XLE", "XLF", "XLV", 
              "XLI", "XLB", "XLK", "XLU", "IWB", "IWD", 
              "IWF", "IWM", "IWN", "IWO", "IWP", "IWR", 
              "IWS", "IWV", "IUSV", "IUSG")
etf_rets <- etf_rets[, sym_bols]


# Extract the numeric year from each element of the date index of "etf_rets"
# calculate a numeric vector of years from the date index of "etf_rets", 
# and call it "ye_ars",
# you can use either functions format() and as.numeric(), 
# or function year() from package lubridate,

ye_ars <- as.numeric(format(index(etf_rets), "%Y"))
ye_ars <- year(index(etf_rets))


# Calculate a matrix containing the annualized alpha for each ETF in each year, 
# and call it "ann_alphas"
# the matrix "ann_alphas" should have rows corresponding to ETF names, 
# and columns corresponding to years,
# assign row and column names from colnames of "etf_rets",
# use functions sapply(), unique(), and either CAPM.alpha() 
# or table.CAPM() from package PerformanceAnalytics,
# the function unique() calculates a vector of unique elements of an object,
# and can be used to extract unique years from "ye_ars",
# annualize the alphas by multiplying them by the average number 
# of business days in each year (250),

# first method, using CAPM.alpha(),

ann_alphas <- 250*sapply(unique(ye_ars), function(ye_ar) {
  in_dex <- (ye_ars==ye_ar)
  CAPM.alpha(Ra=etf_rets[in_dex, -1], 
             Rb=etf_rets[in_dex, "VTI"])
})


# second method, using table.CAPM(),

ann_alphas <- sapply(unique(ye_ars), function(ye_ar) {
  in_dex <- (ye_ars==ye_ar)
  etf_perf_stats <- table.CAPM(Ra=etf_rets[in_dex, -1], 
                               Rb=etf_rets[in_dex, "VTI"], 
                               scale=250)
  as.numeric(etf_perf_stats["Annualized Alpha", ])
})


# assign row and column names,

rownames(ann_alphas) <- colnames(etf_rets)[-1]
colnames(ann_alphas) <- unique(ye_ars)



##################################
# 2. (10pts) 
# Create a scatterplot of "ann_alphas" values for "2008" and "2009", 
# add labels containing the rownames of "ann_alphas",
# use functions plot() and text(),

year1 <- "2008"
year2 <- "2009"
plot(x=ann_alphas[, year1], y=ann_alphas[, year2], 
     xlab=year1, ylab=year2)
text(x=ann_alphas[, year1], y=ann_alphas[, year2], 
     labels=rownames(ann_alphas), 
     pos=1, cex=0.8)



##################################
# 3. (20pts) 
# Calculate a matrix containing columns with the symbols of ETFs, 
# sorted from the highest to lowest alpha in each year, 
# and call it "top_etf"
# use functions apply() and order(), 

top_etf <- apply(ann_alphas, 2, function(ann_alpha, ...) {
  rownames(ann_alphas)[order(ann_alpha, ...)]
}, decreasing=TRUE)

# or simply:

top_etf <- apply(ann_alphas, 2, function(ann_alpha) {
  rownames(ann_alphas)[order(ann_alpha, decreasing=TRUE)]
})

