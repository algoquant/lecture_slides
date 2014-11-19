

library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
thm <- knit_theme$get("acid")
knit_theme$set(thm)



set.seed(1121)  # initialize the random number generator
runif(3)  # three random numbers from the uniform distribution
runif(3)  # produce another three numbers
set.seed(1121)  # re-initialize the random number generator
runif(3)  # produce another three numbers

# produce random number from standard normal distribution
rnorm(1)
# produce five random numbers from standard normal distribution
rnorm(5)
# produce five random numbers from the normal distribution
rnorm(n=5, mean=1, sd=2)  # match arguments by name
# calculate cumulative standard normal distribution
c(pnorm(-2), pnorm(2))
# calculate inverse cumulative standard normal distribution
c(qnorm(0.75), qnorm(0.25))



rm(list=ls())
set.seed(1121)  # initialize the random number generator
# sample from Standard Normal Distribution
rand_sample <- rnorm(1000)

mean(rand_sample)  # sample mean

median(rand_sample)  # sample median

sd(rand_sample)  # sample standard deviation



rm(list=ls())
ts_rets <- diff(log(EuStockMarkets[, 1]))  # DAX returns
len_rets <- length(ts_rets)  # number of observations
mean_rets <- mean(ts_rets)  # calculate mean
sd_rets <- sd(ts_rets)  # calculate standard deviation
# calculate skew
len_rets*(sum(((ts_rets - mean_rets)/sd_rets)^3))/
  ((len_rets-1)*(len_rets-2))
# calculate kurtosis
len_rets*(len_rets+1)*(sum(((ts_rets - mean_rets)/sd_rets)^4))/
  ((len_rets-1)^3)
ts_rets <- rnorm(len_rets, sd=2)  # random normal returns
mean_rets <- mean(ts_rets); sd_rets <- sd(ts_rets)
# calculate skew
len_rets*(sum(((ts_rets - mean_rets)/sd_rets)^3))/
  ((len_rets-1)*(len_rets-2))
# calculate kurtosis
len_rets*(len_rets+1)*(sum(((ts_rets - mean_rets)/sd_rets)^4))/
  ((len_rets-1)^3)



ts_rets <- diff(log(EuStockMarkets[, 1]))  # DAX returns
# define function CalcSkew to calculate the skew
CalcSkew <- function(ts.data=rnorm(1000)) {  # default is normal
# Calculates the skew of a time series of returns.
  len_data <- length(ts.data)  # number of observations
  mean_rets <- mean(ts.data)
  sd_rets <- sd(ts.data)
# the last statement is what is returned
  len_data*sum(((ts.data - mean_rets)/sd_rets)^3)/((len_data-1)*(len_data-2))
}  # end CalcSkew
# calculate skewness of DAX returns
CalcSkew(ts.data=ts_rets)  # match arguments by name
CalcSkew(ts_rets)  # match arguments by position
CalcSkew()  # use default value of arguments
CalcSkew  # show the function code



rm(list=ls())
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
v.xval <- seq(-4, 4, length=100)
v.sigma <- c(0.5, 1, 1.5, 2)  # sigma values
# create plot colors
colors <- c("red", "black", "blue", "green")
# create legend labels
v.labels <- paste("sigma", v.sigma, sep='=')
# plot an empty chart
plot(v.xval, dnorm(v.xval, sd=v.sigma[1]), 
     type="n", xlab="", ylab="", 
     main="Normal Distributions")
# add lines to plot
for (in_dex in 1:4) {
  lines(v.xval, dnorm(v.xval, sd=v.sigma[in_dex]), 
lwd=2, col=colors[in_dex])
}
# add legend
legend("topright", inset=0.05, title="Sigmas", 
       v.labels, cex=0.8, lwd=2, lty=c(1, 1, 1, 1), 
       col=colors)



rm(list=ls())
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
v.xval <- seq(0, 20, length=100)
v.df <- c(2, 5, 8, 11)  # df values
# create plot colors
colors <- c("red", "black", "blue", "green")
# create legend labels
v.labels <- paste("df", v.df, sep='=')
# plot an empty chart
plot(v.xval, dchisq(v.xval, df=v.df[1]), 
     type="n", xlab="", ylab="", 
     main="Chi-squared Distributions")
# add lines to plot
for (in_dex in 1:4) {
  lines(v.xval, dchisq(v.xval, df=v.df[in_dex]), 
lwd=2, col=colors[in_dex])
}
# add legend
legend("topright", inset=0.05, 
       title="Degrees of freedom", v.labels, 
       cex=0.8, lwd=2, lty=c(1, 1, 1, 1), 
       col=colors)



rm(list=ls())
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
v.xval <- seq(-5, 5, length=100)
v.df <- c(3, 6, 9)  # df values
# create plot colors
colors <- c("black", "red", "blue", "green")
# create legend labels
v.labels <- c('normal', paste("df", v.df, sep='='))
# plot chart of normal distribution
plot(v.xval, dnorm(v.xval), type="l", 
     lwd=2, xlab="", ylab="", 
     main="t-distributions")
# add lines to plot
for (in_dex in 1:3) {
  lines(v.xval, dt(v.xval, df=v.df[in_dex]), 
lwd=2, col=colors[in_dex+1])
}
# add legend
legend("topright", inset=0.05, 
       title="Degrees\n of freedom", v.labels, 
       cex=0.8, lwd=2, lty=c(1, 1, 1, 1), 
       col=colors)



rm(list=ls())
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
poisson_events <- 0:11  # Poisson events
poisson_freq <- dpois(poisson_events, lambda=4)
names(poisson_freq) <- as.character(poisson_events)
poisson_freq
poisson_func <- function(x, lambda)  # Poisson function
            {exp(-lambda)*lambda^x/factorial(x)}
curve(expr=poisson_func(x, lambda=4), xlim=c(0, 11), main="Poisson distribution",
      xlab="No. of events", ylab="Frequency of events", lwd=2, col="red")
legend(x="topright", legend="Poisson density", title="", 
       inset=0.05, cex=0.8, bg="white", lwd=4, lty=1, col="red")



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



rm(list=ls())
par(oma=c(1, 1, 1, 1), mgp=c(2, 0.5, 0), mar=c(5, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
library(ggplot2)  # load ggplot2

qplot(  # simple ggplot2
    main="Standard Normal Distribution", 
    c(-4, 4), 
    stat="function", 
    fun=dnorm, 
    geom="line", 
    xlab=NULL, ylab=NULL
    ) +  # end qplot

theme(  # modify plot theme
    plot.title=element_text(vjust=-1.0), 
    plot.background=element_blank()
    ) +  # end theme

geom_vline(  # add vertical line
  aes(xintercept=c(-2.0, 2.0)), 
  colour="red", 
  linetype="dashed"
  )  # end geom_vline



rm(list=ls())
par(oma=c(1, 1, 1, 1), mgp=c(2, 0.5, 0), mar=c(5, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
### create ggplot2 with shaded area
x_var <- -400:400/100
norm_frame <- data.frame(x_var=x_var, 
                 d.norm=dnorm(x_var))
norm_frame$shade <- ifelse(
            abs(norm_frame$x_var) >= 2, 
            norm_frame$d.norm, NA)
ggplot(  # main function
  data=norm_frame, 
  mapping=aes(x=x_var, y=d.norm)
  ) +  # end ggplot
# plot line
  geom_line() + 
# plot shaded area
  geom_ribbon(aes(ymin=0, ymax=shade), fill="red") + 
# no axis labels
  xlab("") + ylab("") + 
# add title
  ggtitle("Standard Normal Distribution") +
# modify plot theme
  theme(
  plot.title=element_text(vjust=-1.0), 
  plot.background=element_blank()
  )  # end theme



# calculate DAX percentage returns
rets_dax <- diff(log(EuStockMarkets[, 1]))

# Shapiro-Wilk test for normal distribution
shapiro.test(rnorm(length(rets_dax)))

# Shapiro-Wilk test for DAX returns
shapiro.test(rets_dax)

# Shapiro-Wilk test for uniform distribution
shapiro.test(runif(length(rets_dax)))



rets_dax <- diff(log(EuStockMarkets[, 1]))
library(tseries)  # load package tseries

# Jarque-Bera test for normal distribution
jarque.bera.test(rnorm(length(rets_dax)))

# Jarque-Bera test for DAX returns
jarque.bera.test(rets_dax)

# Jarque-Bera test for uniform distribution
jarque.bera.test(runif(length(rets_dax)))



par(oma=c(15, 1, 1, 1), mgp=c(2, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# calculate DAX percentage returns
rets_dax <- diff(log(EuStockMarkets[, 1]))
library(forecast)  # load forecast
par(mfrow=c(2,1))  # set plot panels
# autocorrelation from "stats"
acf(rets_dax, lag=5, xlab=NA)
# autocorrelation from "forecast"
Acf(rets_dax, lag=5, xlab=NA)



rm(list=ls())
library(zoo)  # load zoo
library(ggplot2)  # load ggplot2
library(gridExtra)  # load gridExtra
# coerce DAX time series to zoo
zoo.dax <- as.zoo(EuStockMarkets)[, 1]
index(zoo.dax) <-  # index to class 'Dates'
  as.Date(365*(index(zoo.dax)-1970))
# filter past values only (sides=1)
dax.filt <- filter(zoo.dax, 
             filter=rep(1/5,5), sides=1)
dax.filt <- zoo(coredata(dax.filt), 
          order.by=index(zoo.dax))
dax.filt <- merge(zoo.dax, dax.filt)
dax.filt <- na.omit(dax.filt)
colnames(dax.filt) <- c("DAX", "DAX filtered")
dax.data <- window(dax.filt, 
             start=as.Date("1997-01-01"), 
             end=as.Date("1998-01-01"))
autoplot(  # plot ggplot2
    dax.data, main="Filtered DAX", facets=NULL
) +  # end autoplot
xlab("") + ylab("") +
theme(  # modify plot theme
    legend.position=c(0.1, 0.5), 
    plot.title=element_text(vjust=-2.0), 
    plot.margin=unit(c(-0.5, 0.0, -0.5, 0.0), "cm"), 
    plot.background=element_blank(),
    axis.text.y=element_blank()
    )  # end theme
# end ggplot2



par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
dax.diff <- na.omit(diff(log(dax.filt)))
par(mfrow=c(2,1))  # set plot panels
Acf(dax.diff[, 1], lag=20, xlab=NA, ylab=NA)
title(main="DAX", line=-1)
Acf(dax.diff[, 2], lag=20, xlab=NA, ylab=NA)
title(main="DAX filtered", line=-1)



par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # set plot panels
# autocorrelation from "stats"
Acf(dax.diff[, 2], lag=20, xlab=NA, ylab=NA)
title(main="DAX filtered autocorrelations", line=-1)
# autocorrelation from "forecast"
Pacf(dax.diff[, 2], lag=20, xlab=NA, ylab=NA)
title(main="DAX filtered partial autocorrelations", 
      line=-1)


