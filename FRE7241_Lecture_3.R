library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
thm <- knit_theme$get("acid")
knit_theme$set(thm)
set.seed(1121)  # initialize the random number generator
library(xts)  # load package xts
# create xts time series
in_dex <- Sys.Date() + 0:3
xts_series <- xts(rnorm(length(in_dex)), 
         order.by=in_dex)
names(xts_series) <- "random"
xts_series
tail(xts_series, 3)  # get last few elements
first(xts_series)  # get first element
last(xts_series)  # get last element
class(xts_series)  # class 'xts'
attributes(xts_series)
load(file="C:/Develop/data/zoo_data.RData")
library(xts)  # load package xts
# as.xts() creates xts from zoo
xts_stx <- as.xts(zoo_stx_adj)
dim(xts_stx)
head(xts_stx[, 1:4], 4)
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# plot using plot.xts method
plot(xts_stx[, "AdjClose"], xlab="", ylab="", main="")
title(main="MSFT Prices")  # add title
library(ggplot2)
load(file="C:/Develop/data/etf_data.Rdata")
# create ggplot object
etf_gg <- autoplot(etf_series_ad[, 1],
             main=names(etf_series_ad[, 1])) +
  xlab("") + ylab("") +
  theme(
    legend.position=c(0.1, 0.5),
    plot.title=element_text(vjust=-2.0),
    plot.background=element_blank()
  )  # end theme
# render ggplot object
etf_gg
library(xts)  # load package xts
# subset xts using a date range string
xts_sub <- xts_stx["2014-10-15/2015-01-10", 1:4]
first(xts_sub)
last(xts_sub)
# subset Nov 2014 using a date string
xts_sub <- xts_stx["2014-11", 1:4]
first(xts_sub)
last(xts_sub)
# subset all data after Nov 2014
xts_sub <- xts_stx["2014-11/", 1:4]
first(xts_sub)
last(xts_sub)
# comma after date range not necessary
identical(xts_stx["2014-11", ], xts_stx["2014-11"])
library(xts)  # load package xts
# vector of 1-minute times (ticks)
min_ticks <- seq.POSIXt(
  from=as.POSIXct("2015-04-14", tz="America/New_York"), 
  to=as.POSIXct("2015-04-16"), 
  by="min")
# xts of 1-minute times (ticks)
xts_series <- xts(rnorm(length(min_ticks)), 
               order.by=min_ticks)
# subset recurring time interval using "T notation",
xts_series <- xts_series["T09:30:00/T16:00:00"]
first(xts_series["2015-04-15"])  # first element of day
last(xts_series["2015-04-15"])  # last element of day
library(xts)  # load package xts
str(xts_stx)  # display structure of xts
# subsetting zoo to single column drops dim attribute
dim(zoo_stx_adj)
dim(zoo_stx_adj[, 1])
# zoo with single column are vectors not matrices
c(is.matrix(zoo_stx_adj), is.matrix(zoo_stx_adj[, 1]))
# xts always have a dim attribute
rbind(base=dim(xts_stx), subs=dim(xts_stx[, 1]))
c(is.matrix(xts_stx), is.matrix(xts_stx[, 1]))
library(xts)  # load package xts
# lag of zoo shortens it by one row
rbind(base=dim(zoo_stx_adj), lag=dim(lag(zoo_stx_adj)))
# lag of xts doesn't shorten it
rbind(base=dim(xts_stx), lag=dim(lag(xts_stx)))
# lag of zoo is in opposite direction from xts
head(lag(zoo_stx_adj), 4)
head(lag(xts_stx), 4)
library(xts)  # load package xts
# extract indices of the last observations in each month
end_points <- endpoints(xts_stx, on='months')
head(end_points)
# extract the last observations in each month
head(xts_stx[end_points, ])
library(xts)  # load package xts
# apply "mean" over end_points
period_mean <- period.apply(xts_stx[, "AdjClose"], 
               INDEX=end_points, 
               FUN=mean)
head(period_mean)
period_sum <- period.sum(xts_stx[, "AdjClose"], 
               INDEX=end_points)
head(period_sum)
library(xts)  # load package xts
# apply "mean" over monthly periods
period_mean <- apply.monthly(xts_stx[, "AdjClose"], 
               FUN=mean)
head(period_mean)
library(xts)  # load package xts
# lower the periodicity to months
xts_monthly <- to.period(x=xts_stx[, "AdjClose"], 
                   period="months", name="MSFT")
# convert colnames to standard OHLC format
colnames(xts_monthly)
colnames(xts_monthly) <- sapply(
  strsplit(colnames(xts_monthly), split=".", fixed=TRUE), 
  function(na_me) na_me[-1]
  )  # end sapply
head(xts_monthly, 3)
# lower the periodicity to years
xts_yearly <- to.period(x=xts_monthly, 
                   period="years", name="MSFT")
colnames(xts_yearly) <- sapply(
  strsplit(colnames(xts_yearly), split=".", fixed=TRUE), 
  function(na_me) na_me[-1]
  )  # end sapply
head(xts_yearly)
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
load(file="C:/Develop/data/zoo_data.RData")
library(xts)  # load package xts
# as.xts() creates xts from zoo
xts_stx <- as.xts(zoo_stx_adj)
# subset xts using a date
xts_sub <- xts_stx["2014-11", 1:4]

# plot OHLC using plot.xts method
plot(xts_sub, type="candles", main="")
title(main="MSFT Prices")  # add title
load(file="C:/Develop/data/zoo_data.RData")
ts_stx <- as.ts(zoo_stx)
class(ts_stx)
tail(ts_stx[, 1:4])
library(xts)
xts_stx <- as.xts(zoo_stx)
class(xts_stx)
tail(xts_stx[, 1:4])
rm(list=ls())
options(digits=3)
setwd("C:/Develop/data")
library(xtable)
# ETF symbols for asset allocation
sym_bols <- c("VTI", "VEU", "IEF", "VNQ", 
  "DBC", "XLY", "XLP", "XLE", "XLF", "XLV", 
  "XLI", "XLB", "XLK", "XLU", "IWB", "IWD", 
  "IWF", "IWM", "IWN", "IWO", "IWP", "IWR", 
  "IWS", "IWV", "IUSV", "IUSG")
# read etf database into data frame
etf_list <- read.csv(file='etf_list.csv', 
               stringsAsFactors=FALSE)
rownames(etf_list) <- etf_list$Symbol
# subset etf_list only those ETF's in sym_bols
etf_list <- etf_list[sym_bols, ]
# shorten names
etf_names <- sapply(etf_list$Name, 
              function(name) {
  name_split <- strsplit(name, split=" ")[[1]]
  name_split <- 
    name_split[c(-1, -length(name_split))]
  name_match <- match("Select", name_split)
  if (!is.na(name_match))
    name_split <- name_split[-name_match]
  paste(name_split, collapse=" ")
})  # end sapply
etf_list$Name <- etf_names
etf_list["IEF", "Name"] <- "Treasury Bond Fund"
etf_list["XLY", "Name"] <- "Consumer Discr. Sector Fund"
etf_list[c(1, 2)]
print(xtable(etf_list), comment=FALSE, size="tiny")
library(quantmod)  # load package "quantmod"
# get documentation for package "quantmod"
packageDescription("quantmod")  # get short description
help(package="quantmod")  # load help page
data(package="quantmod")  # list all datasets in "quantmod"
ls("package:quantmod")  # list all objects in "quantmod"
detach("package:quantmod")  # remove quantmod from search path
load(file="C:/Develop/data/etf_data.Rdata")
library(quantmod)  # load package "quantmod"
env_data <- new.env()  # new environment for data
# download data for sym_bols into env_data
getSymbols(sym_bols, env=env_data, adjust=TRUE,
    from="2007-01-03", to="2015-05-01")
load(file="C:/Develop/data/etf_data.Rdata")
library(quantmod)  # load package "quantmod"
ls(env_data)  # list files in env_data
# get class of object in env_data
class(get(x=sym_bols[1], envir=env_data))
# another way
class(env_data$VTI)
colnames(env_data$VTI)
head(env_data$VTI, 3)
load(file="C:/Develop/data/etf_data.Rdata")
library(quantmod)  # load package "quantmod"
# adjust single OHLC object using its name
env_data$VTI <- adjustOHLC(env_data$VTI,
                     use.Adjusted=TRUE)

# adjust OHLC object using string as name
assign(sym_bols[1], adjustOHLC(
    get(x=sym_bols[1], envir=env_data),
    use.Adjusted=TRUE),
  envir=env_data)

# adjust objects in environment using vector of strings
for (sym_bol in sym_bols) {
  assign(sym_bol,
   adjustOHLC(get(sym_bol, envir=env_data),
              use.Adjusted=TRUE),
   envir=env_data)
}
load(file="C:/Develop/data/etf_data.Rdata")
library(quantmod)  # load package "quantmod"
# extract and merge data for vector of strings
etf_series <- do.call(merge,
            as.list(env_data)[sym_bols])

# extract adjusted prices and merge into xts
etf_series_ad <- do.call(merge,
            eapply(env_data, Ad)[sym_bols])

# drop ".Adjusted" from colnames
colnames(etf_series_ad) <-
  sapply(colnames(etf_series_ad),
    function(col_name)
strsplit(col_name, split="[.]")[[1]])[1, ]
tail(etf_series_ad[, 1:2], 3)

# save xts to csv file
write.zoo(etf_series,
     file='etf_series.csv', sep=",")

# save data to .Rdata file
save(env_data, etf_series, etf_series_ad,
     file='etf_data.Rdata')
library(quantmod)
load(file="C:/Develop/data/etf_data.Rdata")
# scrub NA values
etf_series_ad <- 
  etf_series_ad[complete.cases(etf_series_ad)]
colnames(etf_series_ad)

# calculate returns from adjusted prices
library(quantmod)
etf_rets <- lapply(etf_series_ad, 
             function(x_ts) {
  daily_return <- dailyReturn(x_ts)
  colnames(daily_return) <- names(x_ts)
  daily_return
})  # end lapply

# "etf_rets" is a list of xts
class(etf_rets[[1]])

# flatten list of xts into a single xts
etf_rets <- do.call(merge, etf_rets)
class(etf_rets)
dim(etf_rets)
head(etf_rets[, 1:3])
library(quantmod)
load(file="C:/Develop/data/etf_data.Rdata")
# is it an OHLC time series?
is.OHLC(env_data$VTI)
# plot OHLC candlechart with volume
chartSeries(env_data$VTI,
      name="VTI",
      theme=chartTheme("white"))
# redraw plot only for Nov-2014
reChart(type="candlesticks", subset="2014-11")
# plot OHLC bar chart with volume
chartSeries(env_data$VTI["2014-11"],
      type="bars",
      name="VTI",
      theme=chartTheme("white"))
library(quantmod)
load(file="C:/Develop/data/etf_data.Rdata")
# candlechart with Bollinger Bands
chartSeries(env_data$VTI["2014"],
      TA="addBBands();addBBands(draw='percent');addVo()",
      name="VTI with Bollinger Bands",
      theme=chartTheme("white"))
# candlechart with two Moving Averages
chartSeries(env_data$VTI["2014"],
      TA="addVo();addEMA(10);addEMA(30)",
      name="VTI with Moving Averages",
      theme=chartTheme("white"))
# candlechart with Commodity Channel Index
chartSeries(env_data$VTI["2014"],
      TA="addVo();addBBands();addCCI()",
      name="VTI with Technical Indicators",
      theme=chartTheme("white"))
library(quantmod)
load(file="C:/Develop/data/etf_data.Rdata")
# download U.S. unemployment rate data
unemp_rate <- getSymbols("UNRATE",
            auto.assign=FALSE,
            src="FRED")
# plot U.S. unemployment rate data
chartSeries(unemp_rate["1990/"],
      name="U.S. unemployment rate",
      theme=chartTheme("white"))
# download 10-Year Treasury constant maturity rate
trs_10yr <- getSymbols("DGS10",
            auto.assign=FALSE,
            src="FRED")
# formula of linear model with zero intercept
lin_formula <- z ~ x + y - 1
lin_formula

# collapsing a character vector into a text string
paste0("x", 1:5)
paste(paste0("x", 1:5), collapse="+")

# creating formula from text string
lin_formula <- as.formula(  # coerce text strings to formula
        paste("y ~ ", 
          paste(paste0("x", 1:5), collapse="+")
          )  # end paste
      )  # end as.formula
class(lin_formula)
lin_formula
# modify the formula using "update"
update(lin_formula, log(.) ~ . + beta)
set.seed(1121)  # initialize random number generator
explana_tory <- seq(from=0.1, to=3.0, by=0.1)  # explanatory variable
# dependent (response) variable equals linear form plus noise
res_ponse <- 3 + 2*explana_tory + rnorm(30)
# specify regression formula
reg_formula <- res_ponse ~ explana_tory
reg_model <- lm(reg_formula)  # perform regression
class(reg_model)  # regressions have class lm
attributes(reg_model)
eval(reg_model$call$formula)  # the regression formula
reg_model$coefficients  # the regression formula coefficients
coef(reg_model)
par(oma=c(1, 2, 1, 0), mgp=c(2, 1, 0), mar=c(5, 1, 1, 1), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
plot(reg_formula)  # plot scatterplot using formula
title(main="Simple Regression", line=-1)
# add regression line
abline(reg_model, lwd=2, col="red")
# plot fitted (predicted) response values
points(x=explana_tory, y=reg_model$fitted.values,
       pch=16, col="blue")
reg_model_sum <- summary(reg_model)  # copy regression summary
reg_model_sum  # print the summary to console
attributes(reg_model_sum)$names  # get summary elements
reg_model_sum$coefficients
reg_model_sum$r.squared
reg_model_sum$adj.r.squared
reg_model_sum$fstatistic
anova(reg_model)
set.seed(1121)  # initialize random number generator
# small coefficient between response and explanatory variables
res_ponse <- 3 + 0.2*explana_tory + rnorm(30)
reg_model <- lm(reg_formula)  # perform regression
# the estimate of the coefficient is not statistically significant
summary(reg_model)
# set plot paramaters - margins and font scale
par(oma=c(1, 0, 1, 0), mgp=c(2, 1, 0), mar=c(2, 1, 2, 1), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2, 2))  # plot 2x2 panels
plot(reg_model)  # plot diagnostic scatterplots
plot(reg_model, which=2)  # plot just Q-Q
res_ponse <- 3 + 2*explana_tory + rnorm(30)
reg_formula <- res_ponse ~ explana_tory
reg_model <- lm(reg_formula)  # perform regression
library(lmtest)  # load lmtest

# perform Durbin-Watson test
dwtest(reg_model)
par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # set plot panels
library(lmtest)  # load lmtest
design_matrix <- data.frame(  # design matrix
  explana_tory=1:30, omit_var=sin(0.2*1:30))
# response depends on both explanatory variables
res_ponse <- with(design_matrix, 
  0.2*explana_tory + omit_var + 0.2*rnorm(30))
# mis-specified regression only one explanatory
reg_model <- lm(res_ponse ~ explana_tory, 
        data=design_matrix)
reg_model_sum <- summary(reg_model)
reg_model_sum$coefficients
reg_model_sum$r.squared
# Durbin-Watson test shows residuals are autocorrelated
dwtest(reg_model)$p.value
plot(reg_formula, data=design_matrix)
abline(reg_model, lwd=2, col="red")
title(main="OVB Regression", line=-1)
plot(reg_model, which=2, ask=FALSE)  # plot just Q-Q
par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # set plot panels
set.seed(1121)
library(lmtest)
# spurious regression in unit root time series
explana_tory <- cumsum(rnorm(100))  # unit root time series
res_ponse <- cumsum(rnorm(100))
reg_formula <- res_ponse ~ explana_tory
reg_model <- lm(reg_formula)  # perform regression
# summary indicates statistically significant regression
reg_model_sum <- summary(reg_model)
reg_model_sum$coefficients
reg_model_sum$r.squared
# Durbin-Watson test shows residuals are autocorrelated
dw_test <- dwtest(reg_model)
c(dw_test$statistic[[1]], dw_test$p.value)
plot(reg_formula, xlab="", ylab="")  # plot scatterplot using formula
title(main="Spurious Regression", line=-1)
# add regression line
abline(reg_model, lwd=2, col="red")
plot(reg_model, which=2, ask=FALSE)  # plot just Q-Q
explana_tory <- seq(from=0.1, to=3.0, by=0.1)  # explanatory variable
res_ponse <- 3 + 2*explana_tory + rnorm(30)
reg_formula <- res_ponse ~ explana_tory
reg_model <- lm(reg_formula)  # perform regression
new_data <- data.frame(explana_tory=0.1*31:40)
predict_lm <- predict(object=reg_model, 
              newdata=new_data, level=0.95, 
              interval="confidence")
predict_lm <- as.data.frame(predict_lm)
head(predict_lm, 2)
plot(reg_formula, xlim=c(1.0, 4.0), 
     ylim=range(res_ponse, predict_lm),
     main="Regression predictions")
abline(reg_model, col="red")
with(predict_lm, {
  points(x=new_data$explana_tory, y=fit, pch=16, col="blue")
  lines(x=new_data$explana_tory, y=lwr, lwd=2, col="red")
  lines(x=new_data$explana_tory, y=upr, lwd=2, col="red")
})  # end with
