#################################
### FRE7241 HW #5 due May 25, 2015
#################################
# Max score 40 pts

# Please write in this file the R code needed to perform the tasks below, 
# rename it to your_name_hw5.R
# and send this file to Luping Liu (ll2525@nyu.edu)


##################################
# 1. (10pts) Load time series data and calculate period statistics 
# over end points, 
# download the file "etf_rets.Rdata" from NYU Classes,
# "etf_rets.Rdata" contains an xts of daily ETF returns called "etf_rets",
# use function load(),
# load package xts,

load(file="C:/Develop/data/etf_rets.Rdata")
library(xts)


# create an index of monthly end points for "etf_rets", called "end_points",
# and set the first "end_points" equal to 1,
# use function endpoints() from package xts,

### write your code here


# calculate a numeric vector of returns of etf_rets[, "VTI"], 
#   over monthly non-overlapping periods based on "end_points", 
#   and call it "period_rets",
# calculate a vector of Median Absolute Deviations (MAD) of etf_rets[, "VTI"], 
#   over monthly non-overlapping periods, and call it "period_vol",
# "period_vol" is a vector of volatility estimates over time,
# use function period.apply() from package xts,
# and functions mad(), sum(), and as.numeric(),

### write your code here


# create a vector of rnorm() of length equal to etf_rets[, "VTI"], 
# and calculate a vector of MAD from this vector, over monthly 
# non-overlapping periods, and call it "rand_vol",
# use functions period.apply(), mad(), rnorm(), and length(),

### write your code here



##################################
# 2. (20pts) Perform autocorrelation tests,
# 
# perform the Durbin-Watson autocorrelation test on "period_rets", 
# "period_vol", and "rand_vol",
# the Durbin-Watson test can be performed on a vector "y" using the syntax: 
#   dwtest(y ~ 1) 
# extract the p-value and compare it to the 2.27% confidence level,
# for which vector can the null hypothesis of zero autocorrelations 
# be rejected at 2.27% confidence level?
# use function dwtest(), from package lmtest,

# load lmtest
library(lmtest)

### write your code here


# perform the Ljung-Box autocorrelation test on "period_rets", 
# "period_vol", and "rand_vol",
# extract the p-value and compare it to the 2.27% confidence level,
# for which vector can the null hypothesis of zero autocorrelations 
# be rejected at 2.27% confidence level?
# use function Box.test(), with lag=10 and type="Ljung",

### write your code here


# apply the functions acf_plus() (from FRE7241 lecture #2) and pacf() 
# to "period_rets", "period_vol", and "rand_vol",
# based on visual inspection, which of them appear to be autocorrelated?

### write your code here



##################################
# 3. (10pts) fit ARIMA models,
# 
# fit ARIMA models to "period_rets", "period_vol", and "rand_vol",
# extract the ARIMA coefficients, and their standard errors,
# the standard errors can be calculated as the square roots 
# of the diagonal of the field "var.coef" of the ARIMA object,
# see:
http://r.789695.n4.nabble.com/ARIMA-standard-error-td820763.html
# use functions arima(), sqrt(), and diag(),

### write your code here

