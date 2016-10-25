library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
options(digits=3)
thm <- knit_theme$get("acid")
knit_theme$set(thm)
setwd("C:/Develop/data")
cat("Enter\ttab")  # cat() interprets backslash escape sequences
print("Enter\ttab")

my_text <- print("hello")
my_text  # print() returns its argument

# create string
my_text <- "Title: My Text\nSome numbers: 1,2,3,...\nRprofile files contain code executed at R startup,\n"

cat(my_text, file="mytext.txt")  # write to text file

cat("Title: My Text",  # write several lines to text file
    "Some numbers: 1,2,3,...",
    "Rprofile files contain code executed at R startup,", 
    file="mytext.txt", sep="\n")

save(my_text, file="mytext.RData")  # write to binary file
print(pi)
print(pi, digits=10)
getOption("digits")
foo <- 12
bar <- "months"
sprintf("There are %i %s in the year", foo, bar)
setwd("C:/Develop/data")
# read text from file
scan(file="mytext.txt", what=character(), sep="\n")

# read lines from file
readLines(con="mytext.txt")

# read text from console
in_put <- readline("Enter a number: ")
class(in_put)
# coerce to numeric
in_put <- as.numeric(in_put)

# read text from file and display in editor:
# file.show("mytext.txt")
# file.show("mytext.txt", pager="")
setwd("C:/Develop/data")
data_frame <- data.frame(type=c("rose", "daisy", "tulip"), color=c("red", "white", "yellow"), price=c(1.5, 0.5, 1.0), row.names=c("flower1", "flower2", "flower3"))  # end data.frame
mat_rix <- matrix(sample(1:12), ncol=3, dimnames=list(NULL, c("col1", "col2", "col3")))
rownames(mat_rix) <- paste("row", 1:NROW(mat_rix), sep="")
# write data frame to text file, and then read it back
write.table(data_frame, file="florist.txt")
data_read <- read.table(file="florist.txt")
data_read  # a data frame

# write matrix to text file, and then read it back
write.table(mat_rix, file="matrix.txt")
mat_read <- read.table(file="matrix.txt")
mat_read  # write.table() coerced matrix to data frame
class(mat_read)
# coerce from data frame back to matrix
mat_read <- as.matrix(mat_read)
class(mat_read)
setwd("C:/Develop/data")
data_frame <- data.frame(small=c(3, 5), medium=c(9, 11), large=c(15, 13))
data_frame <- read.table("mydata.txt", header=TRUE)
data_frame <- read.table("clipboard", header=TRUE)

write.table(x=data_frame, file="clipboard", sep="\t")

# wrapper function for copying data frame from clipboard into R
# by default, data is tab delimited, with a header
read_clip <- function(file="clipboard", sep="\t",
              header=TRUE, ...) {
  read.table(file=file, sep=sep, header=header, ...)
}  # end read_clip

data_frame <- read_clip()

# wrapper function for copying data frame from R into clipboard
# by default, data is tab delimited, with a header
write_clip <- function(data, row.names=FALSE,
               col.names=TRUE, ...) {
  write.table(x=data, file="clipboard", sep="\t",
      row.names=row.names, col.names=col.names, ...)
}  # end write_clip

write_clip(data=data_frame)

# launch spreadsheet-style data editor
data_frame <- edit(data_frame)
setwd("C:/Develop/data")
# write data frame to CSV file, and then read it back
write.csv(data_frame, file="florist.csv")
data_read <- read.csv(file="florist.csv", 
                 stringsAsFactors=FALSE)
data_read  # the row names are read in as extra column
# restore row names
rownames(data_read) <- data_read[, 1]
data_read <- data_read[, -1]  # remove extra column
data_read
# read row names from first column
data_read <- read.csv(file="florist.csv", row.names=1)
data_read
setwd("C:/Develop/data")
# write data frame to CSV file, without row names
write.csv(data_frame, row.names=FALSE, file="florist.csv")
data_read <- read.csv(file="florist.csv")
data_read  # a data frame without row names
setwd("C:/Develop/data")
# write matrix to csv file, and then read it back
write.csv(mat_rix, file="matrix.csv")
mat_read <- read.csv(file="matrix.csv", row.names=1)
mat_read  # read.csv() reads matrix as data frame
class(mat_read)
mat_read <- as.matrix(mat_read)  # coerce to matrix
identical(mat_rix, mat_read)
write.csv(mat_rix, row.names=FALSE, 
    file="matrix_ex_rows.csv")
mat_read <- read.csv(file="matrix_ex_rows.csv")
mat_read <- as.matrix(mat_read)
mat_read  # a matrix without row names
setwd("C:/Develop/data")
library(MASS)  # load package "MASS"
# write to CSV file by row - it's very SLOW!!!
write.matrix(mat_rix, file="matrix.csv", sep=",")
system.time(  # scan reads faster - skip first line with colnames
  mat_read <- scan(file="matrix.csv", sep=",", 
            skip=1, what=numeric()))
col_names <- readLines(con="matrix.csv", n=1)  # read colnames
col_names  # this is a string!
col_names <- strsplit(col_names, s=",")[[1]]  # convert to char vector
mat_read  # mat_read is a vector, not matrix!
# coerce by row to matrix
mat_read <- matrix(mat_read, ncol=length(col_names), 
            byrow=TRUE)
colnames(mat_read) <- col_names  # restore colnames
mat_read
setwd("C:/Develop/data")
# read data from a csv file, including row names
mat_rix <- read.csv(file="matrix_bad.csv", row.names=1,
               stringsAsFactors=FALSE)
mat_rix
class(mat_rix)
# columns with bad data are character or factor
sapply(mat_rix, class)
row_names <- row.names(mat_rix)  # copy row names
# sapply loop over columns and coerce to numeric
mat_rix <- sapply(mat_rix, as.numeric)
row.names(mat_rix) <- row_names  # restore row names
# replace NAs with zero
mat_rix[is.na(mat_rix)] <- 0
# matrix without NAs
mat_rix
setwd("C:/Develop/data")
rm(list=ls())
set.seed(1121)  # reset random number generator
library(zoo)  # load package zoo
# create zoo with Date index
in_dex <- seq(from=as.Date("2013-06-15"), 
            by="day", length.out=100)
zoo_series <- zoo(cumsum(rnorm(length(in_dex))), 
            order.by=in_dex)
tail(zoo_series, 3)
# write zoo to text file, and then read it back
write.zoo(zoo_series, file="zoo_series.txt")
zoo_series <- read.zoo("zoo_series.txt")  # read it back
tail(zoo_series, 3)
setwd("C:/Develop/data")
rm(list=ls())
set.seed(1121)  # reset random number generator
library(zoo)  # load package zoo
# create zoo with POSIXct date-time index
in_dex <- seq(from=as.POSIXct("2013-06-15"), 
            by="hour", length.out=1000)
zoo_series <- zoo(cumsum(rnorm(length(in_dex))), 
            order.by=in_dex)
tail(zoo_series, 3)
# write zoo to text file, and then read it back
write.zoo(zoo_series, file="zoo_series.txt")
zoo_series <- read.zoo("zoo_series.txt")  # read it back
# time field was read as a separate column
tail(zoo_series, 3)
# read and specify that second column is time field
zoo_series <- read.zoo(file="zoo_series.txt", 
                 index.column=list(1,2), 
                 tz="America/New_York")
tail(zoo_series, 3)
setwd("C:/Develop/data")
library(zoo)  # load package zoo
# write zoo to CSV file, and then read it back
write.zoo(zoo_series, file="zoo_series.csv", sep=",")
zoo_series <- read.zoo(file="zoo_series.csv", 
            header=TRUE, sep=",", FUN=as.POSIXct,
            tz="America/New_York")
tail(zoo_series, 3)
# read zoo from CSV file, with custom date-time format
zoo_frame <- read.table(file="zoo_series2.csv", sep=",")
tail(zoo_frame, 3)  # date-time format mm/dd/yyyy hh:mm
zoo_series <- read.zoo(file="zoo_series2.csv", 
            header=TRUE, sep=",", FUN=as.POSIXct, 
            tz="America/New_York",
            format="%m/%d/%Y %H:%M")
tail(zoo_series, 3)
rm(list=ls())  # remove all objects
var1 <- 1; var2 <- 2
ls()  # list all objects
ls()[1]  # list first object
args(save)  # list arguments of save function
# save "var1" to a binary file using string argument
save("var1", file="my_data.RData")
# save "var1" to a binary file using object name
save(var1, file="my_data.RData")
# save multiple objects
save(var1, var2, file="my_data.RData")
# save first object in list by passing to "..." argument
# ls()[1] is not evaluated
save(ls()[1], file="my_data.RData")
# save first object in list by passing to "list" argument
save(list=ls()[1], file="my_data.RData")
# save whole list by passing it to the "list" argument
save(list=ls(), file="my_data.RData")
rm(list=ls())  # remove all objects
# load objects from file
load_ed <- load(file="my_data.RData")
load_ed  # vector of loaded objects
ls()  # list objects
# assign new values to objects in  global environment
sapply(load_ed, function(sym_bol) {
  assign(sym_bol, runif(1), envir=globalenv())
})  # end sapply
ls()  # list objects
# assign new values to objects using for loop
for (sym_bol in load_ed) {
  assign(sym_bol, runif(1))
}  # end for
ls()  # list objects
# save vector of objects
save(list=load_ed, file="my_data.RData")
# remove only loaded objects
rm(list=load_ed)
# remove the object "load_ed"
rm(load_ed)
setwd("C:/Develop/data")
{
sink("sinkdata.txt")# redirect text output to file

cat("Redirect text output from R\n")
print(runif(10))
cat("\nEnd data\nbye\n")

sink()  # turn redirect off

pdf("Rgraph.pdf", width=7, height=4)  # redirect graphics to pdf file

cat("Redirect data from R into pdf file\n")
my_var <- seq(-2*pi, 2*pi, len=100)
plot(x=my_var, y=sin(my_var), main="Sine wave", 
   xlab="", ylab="", type="l", lwd=2, col="red")
cat("\nEnd data\nbye\n")

dev.off()  # turn pdf output off

png("Rgraph.png")  # redirect output to png file

cat("Redirect graphics from R into png file\n")
plot(x=my_var, y=sin(my_var), main="Sine wave", 
 xlab="", ylab="", type="l", lwd=2, col="red")
cat("\nEnd data\nbye\n")

dev.off()  # turn png output off
}
#Perform two-tailed test that sample is
#from Standard Normal Distribution (mean=0, SD=1)
# generate vector of samples and store in data frame
test_frame <- data.frame(samples=rnorm(1000))

# significance level, two-tailed test, critical value=2*SD
signif_level <- 2*(1-pnorm(2))
signif_level
# get p-values for all the samples
test_frame$p_values <-
  sapply(test_frame$samples, pnorm)
test_frame$p_values <-
  2*(0.5-abs(test_frame$p_values-0.5))
# compare p_values to significance level
test_frame$result <-
  test_frame$p_values > signif_level
# number of null rejections
sum(!test_frame$result)
# show null rejections
head(test_frame[!test_frame$result, ])
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
explana_tory <- rnorm(100, mean=2)
noise <- rnorm(100)
# response equals linear form plus error terms
res_ponse <- -3 + explana_tory + noise
# specify regression formula
reg_formula <- res_ponse ~ explana_tory
reg_model <- lm(reg_formula)  # perform regression
class(reg_model)  # regressions have class lm
attributes(reg_model)
eval(reg_model$call$formula)  # regression formula
reg_model$coefficients  # regression coefficients
coef(reg_model)
par(oma=c(1, 2, 1, 0), mgp=c(2, 1, 0), mar=c(5, 1, 1, 1), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
x11(width=6, height=6)
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
  with(reg_model_sum, c(pval=coefficients[2, 4],
   adj_rsquared=adj.r.squared,
   fstat=fstatistic[1]))
}  # end reg_stats
# apply reg_stats() to vector of std dev values
vec_sd <- seq(from=0.1, to=0.5, by=0.1)
names(vec_sd) <- paste0("sd=", vec_sd)
mat_stats <- t(sapply(vec_sd, reg_stats))
# plot in loop
par(mfrow=c(NCOL(mat_stats), 1))
for (in_dex in 1:NCOL(mat_stats)) {
  plot(mat_stats[, in_dex], type="l",
 xaxt="n", xlab="", ylab="", main="")
  title(main=colnames(mat_stats)[in_dex], line=-1.0)
  axis(1, at=1:(NROW(mat_stats)),
 labels=rownames(mat_stats))
}  # end for
reg_stats <- function(da_ta) {  # get regression
# perform regression and get summary
  col_names <- colnames(da_ta)
  reg_formula <-
    paste(col_names[2], col_names[1], sep="~")
  reg_model_sum <- summary(lm(reg_formula,
                        data=da_ta))
# extract regression statistics
  with(reg_model_sum, c(pval=coefficients[2, 4],
   adj_rsquared=adj.r.squared,
   fstat=fstatistic[1]))
}  # end reg_stats
# apply reg_stats() to vector of std dev values
vec_sd <- seq(from=0.1, to=0.5, by=0.1)
names(vec_sd) <- paste0("sd=", vec_sd)
mat_stats <-
  t(sapply(vec_sd, function (std_dev) {
    set.seed(1121)  # initialize number generator
# create explanatory and response variables
    explana_tory <- seq(from=0.1, to=3.0, by=0.1)
    res_ponse <- 3 + 0.2*explana_tory +
rnorm(30, sd=std_dev)
    reg_stats(data.frame(explana_tory, res_ponse))
    }))
# plot in loop
par(mfrow=c(NCOL(mat_stats), 1))
for (in_dex in 1:NCOL(mat_stats)) {
  plot(mat_stats[, in_dex], type="l",
 xaxt="n", xlab="", ylab="", main="")
  title(main=colnames(mat_stats)[in_dex], line=-1.0)
  axis(1, at=1:(NROW(mat_stats)),
 labels=rownames(mat_stats))
}  # end for
# set plot paramaters - margins and font scale
par(oma=c(1,0,1,0), mgp=c(2,1,0), mar=c(2,1,2,1), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2, 2))  # plot 2x2 panels
plot(reg_model)  # plot diagnostic scatterplots
plot(reg_model, which=2)  # plot just Q-Q
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
par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # set plot panels
plot(reg_formula, data=design_matrix)
abline(reg_model, lwd=2, col="red")
title(main="OVB Regression", line=-1)
plot(reg_model, which=2, ask=FALSE)  # plot just Q-Q
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
par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # set plot panels
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
