library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
options(digits=3)
thm <- knit_theme$get("acid")
knit_theme$set(thm)
### Perform two-tailed test that sample is 
### from Standard Normal Distribution (mean=0, SD=1)
# generate vector of samples and store in data frame
test_frame <- data.frame(samples=rnorm(1000))

# significance level, two-tailed test, critical value=2*SD
signif_level <- 2*(1-pnorm(2))
signif_level
# get p-values for all the samples
test_frame$p_values <- sapply(test_frame$samples, pnorm)
test_frame$p_values <- 2*(0.5-abs(test_frame$p_values-0.5))
# compare p_values to significance level
test_frame$result <- test_frame$p_values > signif_level
sum(!test_frame$result)  # number of null rejections
# show null rejections
head(test_frame[!test_frame$result, ])
## rm(list=ls())
## par(oma=c(1, 1, 1, 1), mgp=c(2, 0.5, 0), mar=c(5, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
## library(ggplot2)  # load ggplot2
## 
## qplot(  # simple ggplot2
##     main="Standard Normal Distribution",
##     c(-4, 4),
##     stat="function",
##     fun=dnorm,
##     geom="line",
##     xlab=NULL, ylab=NULL
##     ) +  # end qplot
## 
## theme(  # modify plot theme
##     plot.title=element_text(vjust=-1.0),
##     plot.background=element_blank()
##     ) +  # end theme
## 
## geom_vline(  # add vertical line
##   aes(xintercept=c(-2.0, 2.0)),
##   colour="red",
##   linetype="dashed"
##   )  # end geom_vline
## rm(list=ls())
## par(oma=c(1, 1, 1, 1), mgp=c(2, 0.5, 0), mar=c(5, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
## ### create ggplot2 with shaded area
## x_var <- -400:400/100
## norm_frame <- data.frame(x_var=x_var,
##                  d.norm=dnorm(x_var))
## norm_frame$shade <- ifelse(
##             abs(norm_frame$x_var) >= 2,
##             norm_frame$d.norm, NA)
## ggplot(  # main function
##   data=norm_frame,
##   mapping=aes(x=x_var, y=d.norm)
##   ) +  # end ggplot
## # plot line
##   geom_line() +
## # plot shaded area
##   geom_ribbon(aes(ymin=0, ymax=shade), fill="red") +
## # no axis labels
##   xlab("") + ylab("") +
## # add title
##   ggtitle("Standard Normal Distribution") +
## # modify plot theme
##   theme(
##   plot.title=element_text(vjust=-1.0),
##   plot.background=element_blank()
##   )  # end theme
# calculate DAX percentage returns
dax_rets <- diff(log(EuStockMarkets[, 1]))

# Shapiro-Wilk test for normal distribution
shapiro.test(rnorm(length(dax_rets)))

# Shapiro-Wilk test for DAX returns
shapiro.test(dax_rets)

# Shapiro-Wilk test for uniform distribution
shapiro.test(runif(length(dax_rets)))
dax_rets <- diff(log(EuStockMarkets[, 1]))
library(tseries)  # load package tseries

# Jarque-Bera test for normal distribution
jarque.bera.test(rnorm(length(dax_rets)))

# Jarque-Bera test for DAX returns
jarque.bera.test(dax_rets)

# Jarque-Bera test for uniform distribution
jarque.bera.test(runif(length(dax_rets)))
## par(mar=c(5,0,1,2), oma=c(1,2,1,0), mgp=c(2,1,0), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
## library(zoo)  # load package zoo
## # autocorrelation from "stats"
## acf(coredata(dax_rets), lag=10, main="")
## title(main="acf of DAX returns", line=-1)
library(zoo)  # load package zoo
dax_acf <- acf(coredata(dax_rets), plot=FALSE)
summary(dax_acf)  # get the structure of the "acf" object
# print(dax_acf)  # print acf data
dim(dax_acf$acf)
dim(dax_acf$lag)
head(dax_acf$acf)
## acf_plus <- function (ts_data, plot=TRUE,
##                 xlab="Lag", ylab="",
##                 main="", ...) {
##   acf_data <- acf(x=ts_data, plot=FALSE, ...)
## # remove first element of acf data
##   acf_data$acf <-  array(data=acf_data$acf[-1],
##     dim=c((dim(acf_data$acf)[1]-1), 1, 1))
##   acf_data$lag <-  array(data=acf_data$lag[-1],
##     dim=c((dim(acf_data$lag)[1]-1), 1, 1))
##   if(plot) {
##     ci <- qnorm((1+0.95)/2)*sqrt(1/length(ts_data))
##     ylim <- c(min(-ci, range(acf_data$acf[-1])),
##         max(ci, range(acf_data$acf[-1])))
##     plot(acf_data, xlab=xlab, ylab=ylab,
##    ylim=ylim, main=main, ci=0)
##     abline(h=c(-ci, ci), col="blue", lty=2)
##   }
##   invisible(acf_data)  # return invisibly
## }  # end acf_plus
## par(mar=c(5,0,1,2), oma=c(1,2,1,0), mgp=c(2,1,0), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
## library(zoo)  # load package zoo
## # improved autocorrelation function
## acf_plus(coredata(dax_rets), lag=10, main="")
## title(main="acf of DAX returns", line=-1)
## par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
## par(mfrow=c(2,1))  # set plot panels
## # autocorrelation of squared DAX returns
## acf_plus(coredata(dax_rets)^2,
##    lag=10, main="")
## title(main="acf of squared DAX returns",
## line=-1)
## # autocorrelation of squared random returns
## acf_plus(rnorm(length(dax_rets))^2,
##    lag=10, main="")
## title(main="acf of squared random returns",
## line=-1)
## library(Ecdat)  # load Ecdat
## colnames(Macrodat)  # United States Macroeconomic Time Series
## macro_zoo <- as.zoo(  # coerce to "zoo"
##     Macrodat[, c("lhur", "fygm3")])
## colnames(macro_zoo) <- c("unemprate", "3mTbill")
## # ggplot2 in multiple panes
## autoplot(  # generic ggplot2 for "zoo"
##   object=macro_zoo, main="US Macro",
##   facets=Series ~ .) + # end autoplot
##   xlab("") +
## theme(  # modify plot theme
##   legend.position=c(0.1, 0.5),
##   plot.title=element_text(vjust=-2.0),
##   plot.margin=unit(c(-0.5, 0.0, -0.5, 0.0), "cm"),
##   plot.background=element_blank(),
##   axis.text.y=element_blank()
## )  # end theme
## par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
## par(mfrow=c(2,1))  # set plot panels
## macro_diff <- na.omit(diff(macro_zoo))
## 
## acf_plus(coredata(macro_diff[, "unemprate"]),
##    lag=10)
## title(main="quarterly unemployment rate",
## line=-1)
## 
## acf_plus(coredata(macro_diff[, "3mTbill"]),
##    lag=10)
## title(main="3 month T-bill EOQ", line=-1)
library(Ecdat)  # load Ecdat
macro_zoo <- as.zoo(Macrodat[, c("lhur", "fygm3")])
colnames(macro_zoo) <- c("unemprate", "3mTbill")
macro_diff <- na.omit(diff(macro_zoo))
# Ljung-Box test for DAX data
# 'lag' is the number of autocorrelation coefficients
Box.test(dax_rets, lag=10, type="Ljung")

# changes in 3 month T-bill rate are autocorrelated
Box.test(macro_diff[, "3mTbill"], 
   lag=10, type="Ljung")

# changes in unemployment rate are autocorrelated
Box.test(macro_diff[, "unemprate"], 
   lag=10, type="Ljung")
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
## in_dex <- Sys.Date() + 0:728  # two year daily series
## set.seed(1121)  # reset random numbers
## zoo_arima <- zoo(  # AR time series of returns
##   x=arima.sim(n=729, model=list(ar=0.2)),
##   order.by=in_dex)  # zoo_arima
## zoo_arima <- cbind(zoo_arima, cumsum(zoo_arima))
## colnames(zoo_arima) <- c("AR returns", "AR prices")
## autoplot(object=zoo_arima, # ggplot AR process
##  facets="Series ~ .",
##  main="Autoregressive process (phi=0.2)") +
##   facet_grid("Series ~ .", scales="free_y") +
##   xlab("") + ylab("") +
## theme(
##   legend.position=c(0.1, 0.5),
##   plot.background=element_blank(),
##   axis.text.y=element_blank())
## ar_coeff <- c(-0.8, 0.01, 0.8)  # AR coefficients
## zoo_arima <- sapply(  # create three AR time series
##   ar_coeff, function(phi) {
##     set.seed(1121)  # reset random numbers
##     arima.sim(n=729, model=list(ar=phi))
##   } )
## zoo_arima <- zoo(x=zoo_arima, order.by=in_dex)
## # convert returns to prices
## zoo_arima <- cumsum(zoo_arima)
## colnames(zoo_arima) <-
##   paste("autocorr", ar_coeff)
## autoplot(zoo_arima, main="AR prices",
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
## # simulate AR(1) process
## ari_ma <- arima.sim(n=729, model=list(ar=0.8))
## # ACF of AR(1) process
## acf_plus(ari_ma, lag=10, xlab="", ylab="",
##    main="ACF of AR(1) process")
## # PACF of AR(1) process
## pacf(ari_ma, lag=10, xlab="", ylab="",
##      main="PACF of AR(1) process")
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
library(tseries)  # load tseries
# simulate AR(1) process
set.seed(1121)  # initialize random number generator
ari_ma <- arima.sim(n=729, model=list(ar=0.8))
adf.test(ari_ma)
set.seed(1121)  # initialize random number generator
ari_ma <- arima.sim(n=10000, model=list(ar=0.8))
adf.test(ari_ma)
set.seed(1121)  # initialize random number generator
rand_walk <- cumsum(rnorm(729))
adf.test(rand_walk)
set.seed(1121)  # initialize random number generator
rand_walk <- cumsum(rnorm(10000))
adf.test(rand_walk)
## par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
## par(mfrow=c(2,1))  # set plot panels
## ar3_zoo <- zoo(  # AR(3) time series of returns
##   x=arima.sim(n=365,
##     model=list(ar=c(0.1, 0.5, 0.1))),
##   order.by=in_dex)  # zoo_arima
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
        paste("z ~ ", 
          paste(paste0("x", 1:5), collapse="+")
          )  # end paste
      )  # end as.formula
class(lin_formula)
lin_formula
# modify the formula using "update"
update(lin_formula, log(.) ~ . + beta)
set.seed(1121)  # initialize random number generator
# define explanatory variable
explana_tory <- seq(from=0.1, to=3.0, by=0.1)
# response equals linear form plus error terms
res_ponse <- 3 + 2*explana_tory + rnorm(30)
# specify regression formula
reg_formula <- res_ponse ~ explana_tory
reg_model <- lm(reg_formula)  # perform regression
class(reg_model)  # regressions have class lm
attributes(reg_model)
eval(reg_model$call$formula)  # regression formula
reg_model$coefficients  # regression coefficients
coef(reg_model)
## par(oma=c(1, 2, 1, 0), mgp=c(2, 1, 0), mar=c(5, 1, 1, 1), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
## plot(reg_formula)  # plot scatterplot using formula
## title(main="Simple Regression", line=-1)
## # add regression line
## abline(reg_model, lwd=2, col="red")
## # plot fitted (predicted) response values
## points(x=explana_tory, y=reg_model$fitted.values,
##        pch=16, col="blue")
reg_model_sum <- summary(reg_model)  # copy regression summary
reg_model_sum  # print the summary to console
attributes(reg_model_sum)$names  # get summary elements
reg_model_sum$coefficients
reg_model_sum$r.squared
reg_model_sum$adj.r.squared
reg_model_sum$fstatistic
# standard error of beta
reg_model_sum$
  coefficients["explana_tory", "Std. Error"]
sd(reg_model_sum$residuals)/sd(explana_tory)/
  sqrt(unname(reg_model_sum$fstatistic[3]))
anova(reg_model)
set.seed(1121)  # initialize random number generator
# high noise compared to coefficient
res_ponse <- 3 + 2*explana_tory + rnorm(30, sd=8)
reg_model <- lm(reg_formula)  # perform regression
# estimate of regression coefficient is not
# statistically significant
summary(reg_model)
## par(oma=c(1, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=1.0, cex.axis=1.0, cex.main=1.0, cex.sub=1.0)
## reg_stats <- function(std_dev) {  # noisy regression
##   set.seed(1121)  # initialize number generator
## # create explanatory and response variables
##   explana_tory <- seq(from=0.1, to=3.0, by=0.1)
##   res_ponse <- 3 + 0.2*explana_tory +
##     rnorm(30, sd=std_dev)
## # specify regression formula
##   reg_formula <- res_ponse ~ explana_tory
## # perform regression and get summary
##   reg_model_sum <- summary(lm(reg_formula))
## # extract regression statistics
##   with(reg_model_sum, c(pval=coefficients[2, 4],
##    adj_rsquared=adj.r.squared,
##    fstat=fstatistic[1]))
## }  # end reg_stats
## # apply reg_stats() to vector of std dev values
## vec_sd <- seq(from=0.1, to=0.5, by=0.1)
## names(vec_sd) <- paste0("sd=", vec_sd)
## mat_stats <- t(sapply(vec_sd, reg_stats))
## # plot in loop
## par(mfrow=c(ncol(mat_stats), 1))
## for (in_dex in 1:ncol(mat_stats)) {
##   plot(mat_stats[, in_dex], type="l",
##  xaxt="n", xlab="", ylab="", main="")
##   title(main=colnames(mat_stats)[in_dex], line=-1.0)
##   axis(1, at=1:(nrow(mat_stats)),
##  labels=rownames(mat_stats))
## }  # end for
## reg_stats <- function(da_ta) {  # get regression
## # perform regression and get summary
##   col_names <- colnames(da_ta)
##   reg_formula <-
##     paste(col_names[2], col_names[1], sep="~")
##   reg_model_sum <- summary(lm(reg_formula,
##                         data=da_ta))
## # extract regression statistics
##   with(reg_model_sum, c(pval=coefficients[2, 4],
##    adj_rsquared=adj.r.squared,
##    fstat=fstatistic[1]))
## }  # end reg_stats
## # apply reg_stats() to vector of std dev values
## vec_sd <- seq(from=0.1, to=0.5, by=0.1)
## names(vec_sd) <- paste0("sd=", vec_sd)
## mat_stats <-
##   t(sapply(vec_sd, function (std_dev) {
##     set.seed(1121)  # initialize number generator
## # create explanatory and response variables
##     explana_tory <- seq(from=0.1, to=3.0, by=0.1)
##     res_ponse <- 3 + 0.2*explana_tory +
## rnorm(30, sd=std_dev)
##     reg_stats(data.frame(explana_tory, res_ponse))
##     }))
## # plot in loop
## par(mfrow=c(ncol(mat_stats), 1))
## for (in_dex in 1:ncol(mat_stats)) {
##   plot(mat_stats[, in_dex], type="l",
##  xaxt="n", xlab="", ylab="", main="")
##   title(main=colnames(mat_stats)[in_dex], line=-1.0)
##   axis(1, at=1:(nrow(mat_stats)),
##  labels=rownames(mat_stats))
## }  # end for
## # set plot paramaters - margins and font scale
## par(oma=c(1,0,1,0), mgp=c(2,1,0), mar=c(2,1,2,1), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
## par(mfrow=c(2, 2))  # plot 2x2 panels
## plot(reg_model)  # plot diagnostic scatterplots
## plot(reg_model, which=2)  # plot just Q-Q
library(lmtest)  # load lmtest
# perform Durbin-Watson test
dwtest(reg_model)
## foo <- etf_rets[, c("VTI", "VEU")]
## end_points <- endpoints(foo, on="months")
## head(foo)
## tail(foo)
## class(foo)
## dim(foo)
## reg_model <- lm(paste(names(foo), collapse=" ~ "), data=foo)
## reg_model_sum <- summary(reg_model)
## reg_model_sum
## dwtest(reg_model)
## 
## # filter over non-overlapping periods
## bar <- names(foo)
## foo <- merge(period.sum(foo[, 1], INDEX=end_points), period.sum(foo[, 2], INDEX=end_points))
## foo <- foo[complete.cases(foo), ]
## names(foo) <- bar
## 
## # filter over overlapping periods
## foo <- rollsum(foo, k=11)
## 
## 
## set.seed(1121)
## library(lmtest)
## # spurious regression in unit root time series
## explana_tory <- cumsum(rnorm(100))  # unit root time series
## res_ponse <- cumsum(rnorm(100))
## reg_formula <- res_ponse ~ explana_tory
## reg_model <- lm(reg_formula)  # perform regression
## # summary indicates statistically significant regression
## reg_model_sum <- summary(reg_model)
## reg_model_sum$coefficients
## reg_model_sum$r.squared
## # Durbin-Watson test shows residuals are autocorrelated
## dw_test <- dwtest(reg_model)
## c(dw_test$statistic[[1]], dw_test$p.value)
## par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
## par(mfrow=c(2,1))  # set plot panels
## plot(reg_formula, xlab="", ylab="")  # plot scatterplot using formula
## title(main="Spurious Regression", line=-1)
## # add regression line
## abline(reg_model, lwd=2, col="red")
## plot(reg_model, which=2, ask=FALSE)  # plot just Q-Q
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
## par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
## par(mfrow=c(2,1))  # set plot panels
## plot(reg_formula, data=design_matrix)
## abline(reg_model, lwd=2, col="red")
## title(main="OVB Regression", line=-1)
## plot(reg_model, which=2, ask=FALSE)  # plot just Q-Q
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
## par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
## par(mfrow=c(2,1))  # set plot panels
## plot(reg_formula, xlab="", ylab="")  # plot scatterplot using formula
## title(main="Spurious Regression", line=-1)
## # add regression line
## abline(reg_model, lwd=2, col="red")
## plot(reg_model, which=2, ask=FALSE)  # plot just Q-Q
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
