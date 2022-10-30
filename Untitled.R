# Calculate the 10 best performing stocks in-sample
perfstat <- sort(drop(coredata(pricesn[cutoff, ])), decreasing=TRUE)
symbolv <- names(head(perfstat, 10))
# Calculate the in-sample portfolio
pricis <- pricesn[insample, symbolv]
# Normalize the prices so that they are 1 at cutoff+1
pricesn <- lapply(prices, function(x) x/as.numeric(x[cutoff+1]))
pricesn <- rutils::do_call(cbind, pricesn)
# Calculate the out-of-sample portfolio
pricos <- pricesn[outsample, symbolv]
# Scale the prices to preserve the in-sample wealth
pricos <- sum(pricis[cutoff, ])*pricos/sum(pricos[1, ])

# Combine indeks with out-of-sample stock portfolio returns
wealthv <- rbind(pricis, pricos)
wealthv <- xts::xts(rowMeans(wealthv), datev)
wealthv <- cbind(indeks, wealthv)
colnames(wealthv)[2] <- "Portfolio"
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(rutils::diffit(wealthv[outsample, ]), 
                 function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot out-of-sample stock portfolio returns
dygraphs::dygraph(log(wealthv[endp]), main="Out-of-sample Log Prices of Stock Portfolio") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyEvent(datev[cutoff], label="in-sample", strokePattern="solid", color="green") %>%
  dyLegend(width=500)


############## test
# Summary: Rank the stocks according to their alpha.

## Run all the setup code below.

# Load S&P500 stock prices use your own directory
library(rutils)
load("/Users/jerzy/Develop/lecture_slides/data/sp500.RData")

## End of setup code.


# 1. (20pts)
# Extract the VTI returns from rutils::etfenv.
# You can use the function na.omit().

retvti <- na.omit(rutils::etfenv$returns$VTI)

# You should get the following output:
head(retvti)
#                     VTI
# 2001-06-01  0.006944472
# 2001-06-04  0.004315932
# 2001-06-05  0.014536383
# 2001-06-06 -0.008525201
# 2001-06-07  0.005123837
# 2001-06-08 -0.008554372


# Extract the closing S&P500 stock prices and
# calculate their log returns.
# Subset the S&P500 stock prices so that their 
# start date is the same as retvti.
# 
# You can use the functions eapply(), quantmod::Cl(), 
# rutils::do_call(), zoo::na.locf(), rutils::get_name(), 
# index(), and colnames().



# Extract the closing prices
pricev <- eapply(sp500env, quantmod::Cl)
# Flatten the prices into a single xts series
pricev <- rutils::do_call(cbind, pricev)
# Carry forward and backward non-NA prices
pricev <- zoo::na.locf(pricev, na.rm=FALSE)
pricev <- zoo::na.locf(pricev, fromLast=TRUE)
# Drop ".Close" from column names
colnames(pricev) <- rutils::get_name(colnames(pricev))
# Select prices after the year 2000
pricev <- pricev[index(retvti), ]

# You should get the following output:
head(pricev[, 1:5])
#                  EL      SEE     VRSK      PPG      DXC
# 2001-06-01 15.74512 15.31367 26.59934 16.46893 51.46832
# 2001-06-04 15.81309 15.28024 26.59934 16.56213 51.46832
# 2001-06-05 15.94147 15.28396 26.59934 16.68238 51.46832
# 2001-06-06 15.86973 15.13910 26.59934 16.59219 51.46832
# 2001-06-07 16.15669 15.22824 26.59934 16.64630 51.46832
# 2001-06-08 16.04719 15.09453 26.59934 16.45991 51.46832

# Calculate the log returns of pricev.
# You can use the functions rutils::diffit() and log().

retsp <- rutils::diffit(log(pricev))

# You should get the following output:
head(retsp[, 1:5])
#                      EL           SEE VRSK          PPG DXC
# 2001-06-01  0.000000000  0.0000000000    0  0.000000000   0
# 2001-06-04  0.004307257 -0.0021852625    0  0.005643048   0
# 2001-06-05  0.008085656  0.0002430429    0  0.007234613   0
# 2001-06-06 -0.004510393 -0.0095227188    0 -0.005421047   0
# 2001-06-07  0.017920781  0.0058708584    0  0.003256153   0
# 2001-06-08 -0.006800354 -0.0088192634    0 -0.011260562   0



# 2. (20pts)
# Calculate the stock betas and alphas.  Use VTI returns 
# as a proxy for the market.
# You can use the functions sapply(), cov(), var(), mean(),
# c(), and t().
# You cannot use the function lm().

varvti <- drop(var(retvti))
meanvti <- mean(retvti)

regd <- sapply(retsp, function(rets) {
  betav <- cov(rets, retvti)/varvti
  alphav <- mean(rets) - betav*meanvti
  c(alpha=alphav, beta=betav)
})  # end sapply
regd <- t(regd)

# You should get the following outputs:
class(regd)
# [1] "matrix" "array" 
dim(regd)
# [1] 726   2
head(regd)
#              alpha      beta
# EL    4.444712e-05 0.8582245
# SEE   3.563555e-06 0.9886331
# VRSK  1.957477e-04 0.3756122
# PPG  -1.850810e-05 1.0590364
# DXC   1.679548e-04 0.4642649
# RTX  -5.157425e-06 1.0164509


# Sort regd according to alpha in descending order.
# You can use the function order().

regd <- regd[order(regd[, "alpha"], decreasing=TRUE), ]

# You should get the following outputs:
head(regd)
#              alpha          beta
# DISCA 0.0003136284 -0.0003986137
# KG    0.0003135034  0.0000000000
# CHKAQ 0.0003135034  0.0000000000
# PCP   0.0003133615  0.0004526781
# MON   0.0003133124  0.0006095027
# BRCM  0.0003127016  0.0025576078
tail(regd)
#             alpha     beta
# X   -0.0002160532 1.689157
# ATI -0.0002278351 1.726739
# GNW -0.0002316193 1.738810
# MS  -0.0002415598 1.770517
# CLF -0.0002435795 1.776960
# LNC -0.0002802909 1.894060


#######
# Scatterplot of alpha


#######
# Low beta/High alpha strategies

# In-sample alpha and beta
retvtis <- retvti["/2010"]
varvti <- var(retvtis)
meanvti <- mean(retvtis)

regd <- sapply(retsp["/2010"], function(rets) {
  betav <- cov(rets, retvtis)/varvti
  alphav <- mean(rets) - betav*meanvti
  c(alpha=alphav, beta=betav)
})  # end sapply
regd <- t(regd)

regd <- regd[!(regd[, "beta"] == 0), ]
# Sort by beta
regd <- regd[order(regd[, "beta"]), ]
# Sort by alpha
regd <- regd[order(regd[, "alpha"], decreasing=TRUE), ]
head(regd)


foo <- pricev["2011/", rownames(regd[1:(NROW(regd) %/% 2), ])]
foo <- lapply(foo, function(x) x/as.numeric(x[1]))
foo <- rutils::do_call(cbind, foo)
# head(foo[, 1:5])
# foo <- xts::xts(rowMeans(foo), zoo::index(foo))
foo <- rutils::diffit(rowMeans(foo))
foo <- foo*sd(retvti["2011/", ])/sd(foo)
# dygraphs::dygraph(foo)

# Calculate compounded wealth from returns
wealthv <- cbind(retvti["2011/", ], foo)
colnames(wealthv) <- c("VTI", "Low beta")
sqrt(252)*sapply(wealthv, function (x) mean(x)/sd(x))

# Plot compounded wealth
endp <- rutils::calc_endpoints(wealthv, interval="months")
dygraphs::dygraph(cumsum(wealthv)[endp], main="Low Beta Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)



#######


# Create a vector of strings called symbolv from the 
# column names of rutils::etfenv$returns, excluding 
# the symbols "VXX", SVXY", and "MTUM".
# You must use R code, not simply typing the symbol strings.
# You can use the functions colnames(), match(), c(), 
# the "==" operator, and the "!" operator.

symbolv <- colnames(rutils::etfenv$returns)
symbolv <- symbolv[-match(c("VXX", "SVXY", "MTUM"), symbolv)]
# Or
symbolv <- symbolv[!((symbolv == "VXX") | (symbolv == "SVXY") | (symbolv == "MTUM"))]

# You should get the following output:
symbolv
#  [1] "VTI" "VEU" "IEF" "VNQ" "DBC" "XLY" "XLP" "XLE" "XLF" "XLV"
# [11] "XLI" "XLB" "XLK" "XLU" "VYM" "IVW" "IWB" "IWD" "IWF"

# Extract the symbolv columns from rutils::etfenv$returns, 
# and call it returns.
# Remove any rows with NA values from returns. 
# You can use the function na.omit().

returns <- na.omit(rutils::etfenv$returns[, symbolv])


# You should get the following output:
colnames(returns)
#  [1] "VTI" "VEU" "IEF" "VNQ" "DBC" "XLY" "XLP" "XLE" "XLF" "XLV"
# [11] "XLI" "XLB" "XLK" "XLU" "VYM" "IVW" "IWB" "IWD" "IWF"
# 
dim(returns)
# [1] 2870   19

# Create a function called calc_betas() which performs 
# a regression and returns a vector of betas.
# calc_betas() should accept the following arguments:
#   tseries - time series of asset returns,
#   mar_ket - time series of market returns,
#   calc_bull_bear - Boolean if TRUE then calculate the 
#     bull-market and bear-market betas, else only the 
#     single beta.  Default is FALSE.
#   threshold - threshold level for market returns.
#     For bull-market only select returns above threshold.
#     For bear-market only select returns below -threshold.
#     Default is threshold=0.01.
# 
# You can use the functions lm(), summary(), and c().

calc_betas <- function(tseries, mar_ket, calc_bull_bear=FALSE, threshold=0.01) {
  # Calculate beta
  betav <- summary(lm(tseries ~ mar_ket))$coefficients[2, 1]
  if (calc_bull_bear) {
    # Calculate bull beta
    series_sub <- tseries[mar_ket > threshold]
    market_sub <- mar_ket[mar_ket > threshold]
    bull_beta <- summary(lm(series_sub ~ market_sub))$coefficients[2, 1]
    # Calculate bear beta
    series_sub <- tseries[mar_ket<(-threshold)]
    market_sub <- mar_ket[mar_ket<(-threshold)]
    bear_beta <- summary(lm(series_sub ~ market_sub))$coefficients[2, 1]
    c(beta=betav, bull_beta=bull_beta, bear_beta=bear_beta)
  } else
    betav  # Return single beta
}  # end calc_betas

# Call calc_betas() to verify that it works correctly.
# 
# You should get the following output:
calc_betas(tseries=returns$XLB, mar_ket=returns$VTI)
# [1] 1.069499
# 
calc_betas(tseries=returns$XLB, mar_ket=returns$VTI, calc_bull_bear=TRUE)
#      beta bull_beta bear_beta 
# 1.0694993 0.7825841 1.0732377


# 2. (20pts)
# Perform an sapply loop over the columns 
# of returns, excluding the first one "VTI", and 
# apply calc_betas() to them.
# Also pass into sapply() the arguments: 
# mar_ket=returns$VTI, calc_bull_bear=TRUE, threshold=0.005
# through the dots arguments of sapply().
# Call the output matrix etf_betas.
# You can also use the function t().

etf_betas <- sapply(returns[, -1], calc_betas, 
                    mar_ket=returns$VTI, calc_bull_bear=TRUE, threshold=0.005)

etf_betas <- t(etf_betas)

# You should get the following output:
round(etf_betas, 3)
#       beta bull_beta bear_beta
# VEU  1.098     1.144     1.143
# IEF -0.150    -0.099    -0.151
# VNQ  1.324     1.576     1.488
# DBC  0.446     0.354     0.556
# XLY  1.000     0.948     1.001
# XLP  0.562     0.560     0.539
# XLE  1.202     1.249     1.337
# XLF  1.460     1.603     1.579
# XLV  0.727     0.736     0.640
# XLI  1.013     0.931     0.977
# XLB  1.069     0.868     1.120
# XLK  0.944     0.950     0.870
# XLU  0.632     0.774     0.719
# VYM  0.888     0.868     0.907
# IVW  0.929     0.885     0.968
# IWB  0.978     0.969     0.997
# IWD  1.036     1.067     1.069
# IWF  0.936     0.909     0.964

# Calculate the names of the ETFs whose bear_beta
# is greater than its bull_beta.
# You can use the function names().

is_bear <- etf_betas[, "bear_beta"] > etf_betas[, "bull_beta"]
names(is_bear[is_bear])

# You should get the following output:
# [1] "DBC" "XLY" "XLE" "XLI" "XLB" "VYM" "IVW" "IWB" "IWD" "IWF"


# 3. (20pts)
# Perform an sapply loop over a vector of years, 
# and in each year calculate a vector of single ETF betas.
# Call the output matrix etf_betas.
# You can use the functions sapply() and t().

years <- c("2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018")

etf_betas <- sapply(years, function(ye_ar) {
  sapply(returns[ye_ar, -1], calc_betas, mar_ket=returns$VTI[ye_ar])
})  # end sapply
etf_betas <- t(etf_betas)

# You should get the following matrix:
round(head(etf_betas, 4), 3)
#        VEU    IEF   VNQ   DBC   XLY   XLP   XLE   XLF   XLV   XLI   XLB   XLK
# 2007 1.121 -0.208 1.377 0.136 1.003 0.537 1.274 1.374 0.655 0.941 1.295 0.877
# 2008 1.100 -0.117 1.498 0.339 0.971 0.546 1.266 1.499 0.690 0.889 0.913 0.929
# 2009 1.146 -0.105 2.028 0.630 1.084 0.490 1.165 2.094 0.531 1.129 1.115 0.874
# 2010 1.205 -0.202 1.269 0.749 1.058 0.566 1.173 1.266 0.692 1.146 1.217 0.951
#        XLU   VYM   IVW   IWB   IWD   IWF
# 2007 0.787 0.865 0.901 0.985 1.049 0.926
# 2008 0.790 0.878 0.900 0.974 1.046 0.911
# 2009 0.534 1.053 0.875 0.989 1.125 0.864
# 2010 0.675 0.802 1.015 0.977 1.054 0.977


# Calculate a vector of the names of the ETFs 
# with the highest beta in every year.
# You can use the functions apply(), names(x), 
# and which.max().

apply(etf_betas, MARGIN=1, FUN=function(x) {
  names(x)[which.max(x)]
})  # end apply

# You should get the following vector:
# 
#  2007  2008  2009  2010  2011  2012  2013  2014  2015  2016  2017  2018
# "VNQ" "XLF" "XLF" "VNQ" "XLF" "XLE" "XLF" "XLI" "XLE" "XLE" "XLF" "XLK"


