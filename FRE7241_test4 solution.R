#################################
### FRE7241 Test #4 Solutions Oct 20, 2015
#################################
# Max score 60pts

# The below solutions are examples,
# Slightly different solutions are also possible.

##############
# Summary: calculate a matrix of names of the best 
# performing ETFs in each year. 

# Download the file "etf_data.Rdata" from NYU Classes, 
# and load() it. 
# "etf_data.Rdata" contains an xts series called "etf_rets", 
# with ETF returns,

library(xts)
library(quantmod)
library(PerformanceAnalytics)

load(file="C:/Develop/data/etf_data.Rdata")

# create a vector of symbol names called "sym_bols", 

sym_bols <- c("VTI", "VNQ", "DBC", "XLP", "XLK")

# 1. (25pts) Create a vector of yearly end points 
# for "etf_rets", called "end_points", 
# you must use function endpoints() from package xts, 

end_points <- endpoints(etf_rets, on="years")

# Perform an lapply() loop over the "end_points", and call 
# the result "list_capm". 
# Inside the loop subset "etf_rets" to the "sym_bols" and 
# end points, and calculate a data frame of statistics 
# using table.CAPM(). 
# Simplify the columnn names and return the data frame. 
# "list_capm" should be a list of data frames like this: 
# list_capm[[1]]
#                         VNQ     DBC    XLP    XLK
# Alpha               -0.0013  0.0012 0.0004 0.0005
# Beta                 1.3835  0.1306 0.5371 0.8745
# Beta+                1.5589 -0.1205 0.4994 0.8482
# Beta-                1.2093  0.2158 0.5446 0.8607
# R-squared            0.6283  0.0178 0.6136 0.7505
# Annualized Alpha    -0.2776  0.3686 0.1137 0.1450
# Correlation          0.7927  0.1333 0.7833 0.8663
# Correlation p-value  0.0000  0.0555 0.0000 0.0000
# Tracking Error       0.1847  0.2129 0.1028 0.0849
# Active Premium      -0.2940  0.2964 0.0863 0.1424
# Information Ratio   -1.5915  1.3926 0.8401 1.6777
# Treynor Ratio       -0.1626  2.7993 0.2894 0.2418

# You can use functions lapply(), sapply(), table.CAPM(), 
# colnames(), strsplit(), and an anonymous function, 

list_capm <- 
  lapply(2:length(end_points), 
         function(in_dex) {
           x_ts <- 
             etf_rets[end_points[(in_dex-1)]:end_points[in_dex], sym_bols]
           cap_m <- table.CAPM(Ra=x_ts[, -1], 
                               Rb=x_ts[, 1], scale=252)
           colnames(cap_m) <- 
             sapply(colnames(cap_m), 
                    function (str) {strsplit(str, split=" ")[[1]][1]})
           cap_m
         })  # end lapply

# Assign names to the list using the years of the "end_points". 
# You can use functions names(), format(), and index(), 

names(list_capm) <- format(index(etf_rets[end_points, ]), "%Y")

# 2. (15pts) Perform an sapply() loop over "list_capm", 
# and call the result "names_capm". 
# Inside the loop subset the data frames to extract the 
# row with "Annualized Alpha", sort it in descending order,
# and return the names of the data frame (not the values). 
# You can use functions sapply(), sort(), names(), 
# and an anonymous function, 

names_capm <- sapply(list_capm, 
                     function(cap_m) {
                       cap_m <- cap_m["Annualized Alpha", ]
                       cap_m <- sort(cap_m, decreasing=TRUE)
                       names(cap_m)
                     })  # end sapply

# "names_capm" should be a matrix of ETF names like this: 
#      2007  2008  2009  2010  2011  2012  2013  2014  2015 
# [1,] "DBC" "VNQ" "XLK" "VNQ" "XLP" "VNQ" "XLP" "VNQ" "XLK"
# [2,] "XLK" "XLP" "DBC" "XLP" "VNQ" "XLP" "XLK" "XLP" "XLP"
# [3,] "XLP" "XLK" "XLP" "DBC" "XLK" "XLK" "DBC" "XLK" "DBC"
# [4,] "VNQ" "DBC" "VNQ" "XLK" "DBC" "DBC" "VNQ" "DBC" "VNQ"

# 3. (10pts) Perform an sapply() loop over "list_capm", 
# and call the result "alphas_capm". 
# Inside the loop subset the data frames to extract the 
# row with "Annualized Alpha", sort it in descending order,
# and return the data frame. 
# You can use functions sapply(), sort(), 
# and an anonymous function, 

alphas_capm <- sapply(list_capm, 
                      function(cap_m) {
                        cap_m <- cap_m["Annualized Alpha", ]
                        cap_m <- sort(cap_m, decreasing=TRUE)
                        cap_m
                      })  # end sapply

# "alphas_capm" should be a matrix of alphas like this: 
#      2007  2008  2009  2010  2011  2012  2013  2014  2015 
# DBC 0.3686  0.4663  0.2132 0.0571  0.1294  0.0517  0.0106  0.2169  0.0386 
# XLK 0.145   0.0487  0.0363 0.0317  0.0842  0.0162  -0.0134 0.0745  -0.0485
# XLP 0.1137  -0.0886 0.0085 0.0033  0.016   -0.0098 -0.1722 0.052   -0.0659
# VNQ -0.2776 -0.1694 -0.092 -0.0419 -0.0041 -0.06   -0.2216 -0.2912 -0.1036

# 4. (20pts) Create a scatterplot of alphas for "2008" and "2009", 
# and add labels with ETF names,
# use functions structure(), plot() and text(),

year1 <- "2008"
year2 <- "2009"
alphas_2008 <- structure(alphas_capm[, year1], names=names_capm[, year1])[sym_bols[-1]]
alphas_2009 <- structure(alphas_capm[, year2], names=names_capm[, year2])[sym_bols[-1]]

plot(x=alphas_2008, y=alphas_2009, xlab=year1, ylab=year2)
text(x=alphas_2008, y=alphas_2009, labels=sym_bols[-1], pos=4, cex=0.8)

