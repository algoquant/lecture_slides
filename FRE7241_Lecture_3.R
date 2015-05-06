library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
thm <- knit_theme$get("acid")
knit_theme$set(thm)
## library(zoo)  # load zoo
## library(ggplot2)  # load ggplot2
## library(gridExtra)  # load gridExtra
## # extract DAX time series
## dax_ts <- EuStockMarkets[, 1]
## # filter past values only (sides=1)
## dax_filt <- filter(dax_ts,
##              filter=rep(1/5,5), sides=1)
## # coerce to zoo and merge the time series
## dax_filt <- merge(as.zoo(dax_ts),
##             as.zoo(dax_filt))
## colnames(dax_filt) <- c("DAX", "DAX filtered")
## dax_data <- window(dax_filt,
##              start=1997, end=1998)
## autoplot(  # plot ggplot2
##     dax_data, main="Filtered DAX",
##     facets=NULL) +  # end autoplot
## xlab("") + ylab("") +
## theme(  # modify plot theme
##     legend.position=c(0.1, 0.5),
##     plot.title=element_text(vjust=-2.0),
##     plot.margin=unit(c(-0.5, 0.0, -0.5, 0.0), "cm"),
##     plot.background=element_blank(),
##     axis.text.y=element_blank()
##     )  # end theme
## # end ggplot2
## par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
## dax_rets <- na.omit(diff(log(dax_filt)))
## par(mfrow=c(2,1))  # set plot panels
## 
## acf_plus(coredata(dax_rets[, 1]), lag=10,
##    xlab="")
## title(main="DAX", line=-1)
## 
## acf_plus(coredata(dax_rets[, 2]), lag=10,
##    xlab="")
## title(main="DAX filtered", line=-1)
## par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
## par(mfrow=c(2,1))  # set plot panels
## # autocorrelation from "stats"
## acf_plus(dax_rets[, 2], lag=10, xlab=NA, ylab=NA)
## title(main="DAX filtered autocorrelations", line=-1)
## # partial autocorrelation
## pacf(dax_rets[, 2], lag=10, xlab=NA, ylab=NA)
## title(main="DAX filtered partial autocorrelations",
##       line=-1)
## # ARIMA processes
## library(ggplot2)  # load ggplot2
## library(gridExtra)  # load gridExtra
## daily_index <- Sys.Date() + 0:728  # two year daily series
## set.seed(1121)  # reset random numbers
## ar_zoo <- zoo(  # AR time series of returns
##   x=arima.sim(n=729, model=list(ar=0.2)),
##   order.by=daily_index)  # ar_zoo
## ar_zoo <- cbind(ar_zoo, cumsum(ar_zoo))
## colnames(ar_zoo) <- c("AR returns", "AR prices")
## # plot AR returns
## autoplot(object=ar_zoo,
##  facets="Series ~ .",
##  main="Autoregressive process (phi=0.2)") +
##   facet_grid("Series ~ .", scales="free_y") +
##   xlab("") + ylab("") +
## theme(
##   legend.position=c(0.1, 0.5),
## #  plot.title=element_text(vjust=-1.0),
##   plot.background=element_blank(),
##   axis.text.y=element_blank())
## ar_coeff <- c(-0.8, 0.01, 0.8)  # AR coefficients
## ar_zoo <- sapply(  # create three AR time series
##   ar_coeff, function(phi) {
##     set.seed(1121)  # reset random numbers
##     arima.sim(n=729, model=list(ar=phi))
##   } )
## ar_zoo <- zoo(x=ar_zoo, order.by=daily_index)
## ar_zoo <- cumsum(ar_zoo)  # returns to prices
## colnames(ar_zoo) <- paste("autocorr", ar_coeff)
## autoplot(ar_zoo, main="AR prices",
##    facets=Series ~ .) +
##     facet_grid(Series ~ ., scales="free_y") +
## xlab("") +
## theme(
##   legend.position=c(0.1, 0.5),
##   plot.title=element_text(vjust=-2.0),
##   plot.margin=unit(c(-0.5, 0.0, -0.5, 0.0), "cm"),
##   plot.background=element_blank(),
##   axis.text.y=element_blank())
## par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
## par(mfrow=c(2,1))  # set plot panels
## # ACF of AR(1) process
## acf_plus(na.omit(diff(ar_zoo[, 3])), lag=10,
##  xlab="", ylab="", main="ACF of AR(1) process")
## 
## # PACF of AR(1) process
## pacf(na.omit(diff(ar_zoo[, 3])), lag=10,
##      xlab="", ylab="", main="PACF of AR(1) process")
## library(zoo)  # load zoo
## library(ggplot2)  # load ggplot2
## set.seed(1121)  # initialize random number generator
## rand_walk <- cumsum(zoo(matrix(rnorm(3*100), ncol=3),
##             order.by=(Sys.Date()+0:99)))
## colnames(rand_walk) <-
##   paste("rand_walk", 1:3, sep="_")
## plot(rand_walk, main="Random walks",
##      xlab="", ylab="", plot.type="single",
##      col=c("black", "red", "blue"))
## # add legend
## legend(x="topleft",
##  legend=colnames(rand_walk),
##  col=c("black", "red", "blue"), lty=1)
## par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
## par(mfrow=c(2,1))  # set plot panels
## ar3_zoo <- zoo(  # AR(3) time series of returns
##   x=arima.sim(n=365,
##     model=list(ar=c(0.1, 0.5, 0.1))),
##   order.by=daily_index)  # ar_zoo
## # ACF of AR(3) process
## acf_plus(ar3_zoo, lag=10,
##  xlab="", ylab="", main="ACF of AR(3) process")
## 
## # PACF of AR(3) process
## pacf(ar3_zoo, lag=10,
##      xlab="", ylab="", main="PACF of AR(3) process")
ar3_zoo <- arima.sim(n=1000, 
      model=list(ar=c(0.1, 0.3, 0.1)))
arima(ar3_zoo, order = c(5,0,0))  # fit AR(5) model
library(forecast)  # load forecast
auto.arima(ar3_zoo)  # fit ARIMA model
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
indep_var <- 0.1*1:30  # independent (explanatory) variable
# dependent (response) variable equals linear form plus noise
depend_var <- 3 + 2*indep_var + rnorm(30)
# specify regression formula
reg_formula <- depend_var ~ indep_var
reg_model <- lm(reg_formula)  # perform regression
class(reg_model)  # regressions have class lm
attributes(reg_model)
eval(reg_model$call$formula)  # the regression formula
reg_model$coefficients  # the regression formula coefficients
coef(reg_model)
## par(oma=c(1, 2, 1, 0), mgp=c(2, 1, 0), mar=c(5, 1, 1, 1), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
## plot(reg_formula)  # plot scatterplot using formula
## title(main="Simple Regression", line=-1)
## # add regression line
## abline(reg_model, lwd=2, col="red")
## # plot fitted (predicted) response values
## points(x=indep_var, y=reg_model$fitted.values,
##        pch=16, col="blue")
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
depend_var <- 3 + 0.2*indep_var + rnorm(30)
reg_model <- lm(reg_formula)  # perform regression
# the estimate of the coefficient is not statistically significant
summary(reg_model)
## # set plot paramaters - margins and font scale
## par(oma=c(1, 0, 1, 0), mgp=c(2, 1, 0), mar=c(2, 1, 2, 1), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
## par(mfrow=c(2, 2))  # plot 2x2 panels
## plot(reg_model)  # plot diagnostic scatterplots
## plot(reg_model, which=2)  # plot just Q-Q
depend_var <- 3 + 2*indep_var + rnorm(30)
reg_formula <- depend_var ~ indep_var
reg_model <- lm(reg_formula)  # perform regression
library(lmtest)  # load lmtest

# perform Durbin-Watson test
dwtest(reg_model)
par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # set plot panels
library(lmtest)  # load lmtest
design_matrix <- data.frame(  # design matrix
  indep_var=1:30, omit_var=sin(0.2*1:30))
# response depends on both explanatory variables
depend_var <- with(design_matrix, 
  0.2*indep_var + omit_var + 0.2*rnorm(30))
# mis-specified regression only one explanatory
reg_model <- lm(depend_var ~ indep_var, 
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
indep_var <- cumsum(rnorm(100))  # unit root time series
depend_var <- cumsum(rnorm(100))
reg_formula <- depend_var ~ indep_var
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
indep_var <- 0.1*1:30  # explanatory variable
depend_var <- 3 + 2*indep_var + rnorm(30)
reg_formula <- depend_var ~ indep_var
reg_model <- lm(reg_formula)  # perform regression
new_data <- data.frame(indep_var=0.1*31:40)
predict_lm <- predict(object=reg_model, 
              newdata=new_data, level=0.95, 
              interval="confidence")
predict_lm <- as.data.frame(predict_lm)
head(predict_lm, 2)
plot(reg_formula, xlim=c(1.0, 4.0), 
     ylim=range(depend_var, predict_lm),
     main="Regression predictions")
abline(reg_model, col="red")
with(predict_lm, {
  points(x=new_data$indep_var, y=fit, pch=16, col="blue")
  lines(x=new_data$indep_var, y=lwr, lwd=2, col="red")
  lines(x=new_data$indep_var, y=upr, lwd=2, col="red")
})  # end with
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
## etf_list[c(1, 2)]
print(xtable(etf_list), comment=FALSE, size="tiny")
## library(quantmod)
## library(ggplot2)
## sym_bols <- sym_bols[c(1, 2)]
## data_env <- new.env()
## ls()
## getSymbols(sym_bols, env=data_env)
## ls(data_env)
## class(data_env$VTI)
## colnames(data_env$VTI)
## head(data_env$VTI, 3)
## library(quantmod)
## library(ggplot2)
## etf_series <- do.call(merge,
##             as.list(data_env)[sym_bols])
## etf_series_ad <- do.call(merge,
##             eapply(data_env, Ad)[sym_bols])
## col_names <- strsplit(
##   colnames(etf_series_ad), split="[.]")
## col_names <- as.data.frame(col_names,
##               stringsAsFactors=FALSE)[1, ]
## colnames(etf_series_ad) <- unlist(col_names)
## tail(etf_series_ad[, 1:2], 3)
## write.zoo(etf_series,
##      file='etf_series.csv', sep=",")
## save(etf_series, etf_series_ad,
##      file='etf_data.Rdata')
## etf_gg <- autoplot(etf_series_ad[, 1],
##              main=etf_list$Name[1]) +
##   xlab("") + ylab("") +
##   theme(
##     legend.position=c(0.1, 0.5),
##     plot.title=element_text(vjust=-2.0),
##     plot.background=element_blank()
##   )  # end theme
## # render ggplot
## etf_gg
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
## library(PerformanceAnalytics)  # load package "PerformanceAnalytics"
## # get documentation for package "PerformanceAnalytics"
## packageDescription("PerformanceAnalytics")  # get short description
## help(package="PerformanceAnalytics")  # load help page
## data(package="PerformanceAnalytics")  # list all datasets in "PerformanceAnalytics"
## ls("package:PerformanceAnalytics")  # list all objects in "PerformanceAnalytics"
## detach("package:PerformanceAnalytics")  # remove PerformanceAnalytics from search path
library(PerformanceAnalytics)  # load package "PerformanceAnalytics"
perf_data <- 
  unclass(data(
    package="PerformanceAnalytics"))$results[, -(1:2)]
apply(perf_data, 1, paste, collapse=" - ")
data(managers)  # load "managers" data set
class(managers)
dim(managers)
head(managers, 3)
## # load package "PerformanceAnalytics"
## library(PerformanceAnalytics)
## data(managers)  # load "managers" data set
## ham_1 <- managers[, c("HAM1", "EDHEC LS EQ",
##                 "SP500 TR")]
## 
## chart.CumReturns(ham_1, lwd=2, ylab="",
##   legend.loc="topleft", main="")
## # add title
## title(main="Managers cumulative returns",
## line=-1)
## library(PerformanceAnalytics)  # load package "PerformanceAnalytics"
## data(managers)  # load "managers" data set
## charts.PerformanceSummary(ham_1,
##   main="", lwd=2, ylog=TRUE)
## library(PerformanceAnalytics)  # load package "PerformanceAnalytics"
## chart.CumReturns(
##   etf_rets[, c("XLF", "XLP", "IEF")], lwd=2,
##   ylab="", legend.loc="topleft", main="")
## # add title
## title(main="ETF cumulative returns", line=-1)
options(width=200)
library(PerformanceAnalytics)
chart.Drawdown(etf_rets[, "VTI"], ylab="", 
         main="VTI drawdowns")













table.Drawdowns(etf_rets[, "VTI"])
## library(PerformanceAnalytics)
## chart.Histogram(etf_rets[, 1], main="",
##   xlim=c(-0.06, 0.06),
##   methods = c("add.density", "add.normal"))
## # add title
## title(main=paste(colnames(etf_rets[, 1]),
##            "density"), line=-1)
## library(PerformanceAnalytics)
## chart.Boxplot(etf_rets[,
##   c(rownames(head(ret_stats, 3)),
##     rownames(tail(ret_stats, 3)))])
library(PerformanceAnalytics)
tail(table.Stats(etf_rets[, 
  c("VTI", "IEF", "DBC", "IUSG")]), 3)
ret_stats <- table.Stats(etf_rets)
class(ret_stats)
# Transpose the data frame
ret_stats <- as.data.frame(t(ret_stats))
## # plot scatterplot
## plot(Kurtosis ~ Skewness, data=ret_stats,
##      main="Kurtosis vs Skewness")
## # add labels
## text(x=ret_stats$Skewness, y=ret_stats$Kurtosis,
##     labels=colnames(etf_rets),
##     pos=1, cex=0.8)
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
## library(PerformanceAnalytics)
## chart.RiskReturnScatter(etf_rets, Rf=0.01/12)
library(PerformanceAnalytics)
vti_ief <- etf_rets[, c("VTI", "IEF")]
SharpeRatio(vti_ief)

SortinoRatio(vti_ief)

CalmarRatio(vti_ief)
tail(table.Stats(vti_ief), 4)
