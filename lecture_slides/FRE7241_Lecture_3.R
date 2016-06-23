library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
options(digits=3)
thm <- knit_theme$get("acid")
knit_theme$set(thm)
rm(list=ls())
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
# extract and merge all data, subset by symbols
etf_series <- do.call(merge,
            as.list(env_data)[sym_bols])

# extract and merge adjusted prices, subset by symbols
etf_series_ad <- do.call(merge,
            lapply(as.list(env_data)[sym_bols], Ad))

# extract and merge adjusted prices, subset by symbols
etf_series_ad <- do.call(merge,
            eapply(env_data, Ad)[sym_bols])

# drop ".Adjusted" from colnames
colnames(etf_series_ad) <-
  sapply(colnames(etf_series_ad),
    function(col_name)
strsplit(col_name, split="[.]")[[1]])[1, ]
tail(etf_series_ad[, 1:2], 3)

# which objects in global environment are class xts?
unlist(eapply(globalenv(), is.xts))

# save xts to csv file
write.zoo(etf_series,
     file='etf_series.csv', sep=",")
# save data to .Rdata file
save(env_data, etf_series, etf_series_ad,
     file='etf_data.Rdata')
library(quantmod)
load(file="C:/Develop/data/etf_data.Rdata")
# remove rows with NA values
etf_series_ad <- 
  etf_series_ad[complete.cases(etf_series_ad)]
colnames(etf_series_ad)

# calculate returns from adjusted prices
etf_rets <- lapply(etf_series_ad, 
             function(x_ts) {
# dailyReturn returns single xts with bad colname
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
library(PerformanceAnalytics)  # load package "PerformanceAnalytics"
# get documentation for package "PerformanceAnalytics"
packageDescription("PerformanceAnalytics")  # get short description
help(package="PerformanceAnalytics")  # load help page
data(package="PerformanceAnalytics")  # list all datasets in "PerformanceAnalytics"
ls("package:PerformanceAnalytics")  # list all objects in "PerformanceAnalytics"
detach("package:PerformanceAnalytics")  # remove PerformanceAnalytics from search path
library(PerformanceAnalytics)  # load package "PerformanceAnalytics"
perf_data <- 
  unclass(data(
    package="PerformanceAnalytics"))$results[, -(1:2)]
apply(perf_data, 1, paste, collapse=" - ")
data(managers)  # load "managers" data set
class(managers)
dim(managers)
head(managers, 3)
# load package "PerformanceAnalytics"
library(PerformanceAnalytics)
data(managers)  # load "managers" data set
ham_1 <- managers[, c("HAM1", "EDHEC LS EQ",
                "SP500 TR")]

chart.CumReturns(ham_1, lwd=2, ylab="",
  legend.loc="topleft", main="")
# add title
title(main="Managers cumulative returns",
line=-1)
library(PerformanceAnalytics)  # load package "PerformanceAnalytics"
data(managers)  # load "managers" data set
charts.PerformanceSummary(ham_1,
  main="", lwd=2, ylog=TRUE)
library(PerformanceAnalytics)  # load package "PerformanceAnalytics"
chart.CumReturns(
  etf_rets[, c("XLF", "XLP", "IEF")], lwd=2,
  ylab="", legend.loc="topleft", main="")
# add title
title(main="ETF cumulative returns", line=-1)
load(file="C:/Develop/data/etf_data.Rdata")
options(width=200)
library(PerformanceAnalytics)
chart.Drawdown(etf_rets[, "VTI"], ylab="",
         main="VTI drawdowns")
load(file="C:/Develop/data/etf_data.Rdata")
options(width=200)
library(PerformanceAnalytics)
table.Drawdowns(etf_rets[, "VTI"])
library(PerformanceAnalytics)
chart.Histogram(etf_rets[, 1], main="",
  xlim=c(-0.06, 0.06),
  methods = c("add.density", "add.normal"))
# add title
title(main=paste(colnames(etf_rets[, 1]),
           "density"), line=-1)
library(PerformanceAnalytics)
chart.Boxplot(etf_rets[,
  c(rownames(head(ret_stats, 3)),
    rownames(tail(ret_stats, 3)))])
library(PerformanceAnalytics)
tail(table.Stats(etf_rets[, 
  c("VTI", "IEF", "DBC", "IUSG")]), 3)
ret_stats <- table.Stats(etf_rets)
class(ret_stats)
# Transpose the data frame
ret_stats <- as.data.frame(t(ret_stats))
# plot scatterplot
plot(Kurtosis ~ Skewness, data=ret_stats,
     main="Kurtosis vs Skewness")
# add labels
text(x=ret_stats$Skewness, y=ret_stats$Kurtosis,
    labels=colnames(etf_rets),
    pos=1, cex=0.8)
load(file="C:/Develop/data/etf_data.Rdata")
# add skew_kurt column
ret_stats$skew_kurt <- 
  ret_stats$Skewness/ret_stats$Kurtosis
# sort on skew_kurt
ret_stats <- ret_stats[
  order(ret_stats$skew_kurt, 
  decreasing=TRUE), ]
# add names column
ret_stats$Name <- 
  etf_list[rownames(ret_stats), ]$Name
ret_stats[, c("Name", "Skewness", "Kurtosis")]
library(PerformanceAnalytics)
chart.RiskReturnScatter(etf_rets, Rf=0.01/12)
library(PerformanceAnalytics)
vti_ief <- etf_rets[, c("VTI", "IEF")]
SharpeRatio(vti_ief)

SortinoRatio(vti_ief)

CalmarRatio(vti_ief)
tail(table.Stats(vti_ief), 4)
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
par(oma=c(1, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=1.0, cex.axis=1.0, cex.main=1.0, cex.sub=1.0)
reg_stats <- function(std_dev) {  # noisy regression
  set.seed(1121)  # initialize number generator
# create explanatory and response variables
  explana_tory <- seq(from=0.1, to=3.0, by=0.1)
  res_ponse <- 3 + 0.2*explana_tory +
    rnorm(30, sd=std_dev)
# specify regression formula
  reg_formula <- res_ponse ~ explana_tory
# perform regression and get summary
  reg_model_sum <- summary(lm(reg_formula))
# extract regression statistics
  c(pval=reg_model_sum$coefficients[2, 4],
    adj.r.squared=reg_model_sum$adj.r.squared,
    fstat=reg_model_sum$fstatistic[1])
}  # end reg_stats
# apply reg_stats() to vector of std dev values
vec_sd <- seq(from=0.1, to=0.5, by=0.1)
names(vec_sd) <- paste0("sd=", vec_sd)
mat_reg_stats <- t(sapply(vec_sd, reg_stats))
# plot in loop
par(mfrow=c(ncol(mat_reg_stats), 1))
for (in_dex in 1:ncol(mat_reg_stats)) {
  plot(mat_reg_stats[, in_dex], type="l",
 xaxt="n", xlab="", ylab="", main="")
  title(main=colnames(mat_reg_stats)[in_dex],
  line=-1.0)
  axis(1, at=1:(nrow(mat_reg_stats)),
 labels=rownames(mat_reg_stats))
}  # end for
reg_stats <- function(da_ta) {  # get regression
# perform regression and get summary
  col_names <- colnames(da_ta)
  reg_formula <-
    paste(col_names[2], col_names[1], sep="~")
  reg_model_sum <- summary(lm(reg_formula,
                        data=da_ta))
# extract regression statistics
  c(pval=reg_model_sum$coefficients[2, 4],
    adj.r.squared=reg_model_sum$adj.r.squared,
    fstat=reg_model_sum$fstatistic[1])
}  # end reg_stats
# apply reg_stats() to vector of std dev values
vec_sd <- seq(from=0.1, to=0.5, by=0.1)
names(vec_sd) <- paste0("sd=", vec_sd)
mat_reg_stats <-
  t(sapply(vec_sd, function (std_dev) {
    set.seed(1121)  # initialize number generator
# create explanatory and response variables
    explana_tory <- seq(from=0.1, to=3.0, by=0.1)
    res_ponse <- 3 + 0.2*explana_tory +
rnorm(30, sd=std_dev)
    reg_stats(data.frame(explana_tory, res_ponse))
    }))
# plot in loop
par(mfrow=c(ncol(mat_reg_stats), 1))
for (in_dex in 1:ncol(mat_reg_stats)) {
  plot(mat_reg_stats[, in_dex], type="l",
 xaxt="n", xlab="", ylab="", main="")
  title(main=colnames(mat_reg_stats)[in_dex],
  line=-1.0)
  axis(1, at=1:(nrow(mat_reg_stats)),
 labels=rownames(mat_reg_stats))
}  # end for
# set plot paramaters - margins and font scale
par(oma=c(1,0,1,0), mgp=c(2,1,0), mar=c(2,1,2,1), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
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
