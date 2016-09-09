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
# ?options  # get info on global options
getOption("warn")  # global option for "warn"
options("warn")  # global option for "warn"
getOption("error")  # global option for "error"
sqrt_safe <- function(in_put) {
# returns its argument
  if (in_put<0) {
    warning("sqrt_safe: in_put is negative")
    NULL  # return NULL for negative argument
  } else {
    sqrt(in_put)
  }  # end if
}  # end sqrt_safe
sqrt_safe(5)
sqrt_safe(-1)
options(warn=-1)
sqrt_safe(-1)
options(warn=0)
sqrt_safe()
options(warn=1)
sqrt_safe()
options(warn=3)
sqrt_safe()
# function vali_date validates its arguments
vali_date <- function(in_put=NULL) {
# check if argument is valid and return double
  if (is.null(in_put)) {
    return("vali_date: in_put is missing")
  } else if (is.numeric(in_put)) {
    2*in_put
  } else cat("vali_date: in_put not numeric")
}  # end vali_date
vali_date(3)
vali_date("a")
vali_date()
# vali_date validates arguments using missing()
vali_date <- function(in_put) {
# check if argument is valid and return double
  if (missing(in_put)) {
    return("vali_date: in_put is missing")
  } else if (is.numeric(in_put)) {
    2*in_put
  } else cat("vali_date: in_put is not numeric")
}  # end vali_date
vali_date(3)
vali_date("a")
vali_date()
# vali_date() validates its arguments and assertions
vali_date <- function(in_put) {
# check if argument is valid and return double
  if (missing(in_put)) {
    stop("vali_date: in_put is missing")
  } else if (!is.numeric(in_put)) {
    cat("in_put=", in_put)
    stop("vali_date: in_put is not numeric")
  } else 2*in_put
}  # end vali_date
vali_date(3)
vali_date("a")
vali_date()
## # print the call stack
## traceback()
vali_date <- function(in_put) {
# check argument using long form '&&' operator
  stopifnot(!missing(in_put) &&
      is.numeric(in_put))
  2*in_put
}  # end vali_date
vali_date(3)
vali_date()
vali_date("a")
vali_date <- function(in_put) {
# check argument using logical '&' operator
  stopifnot(!missing(in_put) & is.numeric(in_put))
  2*in_put
}  # end vali_date
vali_date()
vali_date("a")
# sum_two() returns the sum of its two arguments
sum_two <- function(in_put1, in_put2) {  # even more robust
# check if at least one argument is not missing
  stopifnot(!missing(in_put1) &&
      !missing(in_put2))
# check if arguments are valid and return sum
  if (is.numeric(in_put1) &&
      is.numeric(in_put2)) {
    in_put1 + in_put2  # both valid
  } else if (is.numeric(in_put1)) {
    cat("in_put2 is not numeric\n")
    in_put1  # in_put1 is valid
  } else if (is.numeric(in_put2)) {
    cat("in_put1 is not numeric\n")
    in_put2  # in_put2 is valid
  } else {
    stop("none of the arguments are numeric")
  }
}  # end sum_two
sum_two(1, 2)
sum_two(5, 'a')
sum_two('a', 5)
sum_two('a', 'b')
sum_two()
## # flag "vali_date" for debugging
## debug(vali_date)
## # calling "vali_date" starts debugger
## vali_date(3)
## # unflag "vali_date" for debugging
## undebug(vali_date)
vali_date <- function(in_put) {
  browser()  # pause and invoke browser
# check argument using long form '&&' operator
  stopifnot(!missing(in_put) &&
      is.numeric(in_put))
  2*in_put
}  # end vali_date
vali_date()  # invokes debugger
options("error")  # show default NULL "error" option
options(error=recover)  # set "error" option to "recover"
options(error=NULL)  # set back to default "error" option
str(tryCatch)  # get arguments of tryCatch()
tryCatch(  # without error handler
  {  # evaluate expressions
    num_var <- 101  # assign
    stop('my error')  # produce error
  },
  finally=print(paste("num_var=", num_var))
)  # end tryCatch

tryCatch(  # with error handler
  {  # evaluate expressions
    num_var <- 101  # assign
    stop('my error')  # produce error
  },
  error=function(error_cond)  # handler captures error condition
    print(paste("error handler: ", error_cond)),
  finally=print(paste("num_var=", num_var))
)  # end tryCatch
rm(list=ls())
# apply loop without tryCatch
apply(as.matrix(1:5), 1, function(num_var) {  # anonymous function
    stopifnot(num_var != 3)  # check for error
    cat("(cat) num_var =", num_var, "\n")  # broadcast
    paste("(return) num_var =", num_var)  # return value
  }  # end anonymous function
)  # end apply
# apply loop with tryCatch
apply(as.matrix(1:5), 1, function(num_var) {  # anonymous function
    tryCatch(  # with error handler
{  # body
  stopifnot(num_var != 3)  # check for error
  cat("(cat) num_var =", num_var, "\t")  # broadcast
  paste("(return) num_var =", num_var)  # return value
},
error=function(error_cond)  # handler captures error condition
  paste("handler: ", error_cond),
finally=print(paste("(finally) num_var =", num_var))
    )  # end tryCatch
  }  # end anonymous function
)  # end apply
