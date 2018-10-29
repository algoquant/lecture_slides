par(oma=c(1, 1, 1, 1), mar=c(2, 1, 1, 1), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
lamb_da <- c(0.5, 1, 1.5)
col_ors <- c("red", "black", "blue")
# plot three curves in loop
for (in_dex in 1:3) {
  curve(expr=plogis(x, scale=lamb_da[in_dex]),
xlim=c(-4, 4), type="l",
xlab="", ylab="", lwd=2,
col=col_ors[in_dex], add=(in_dex>1))
}  # end for
# add title
title(main="Logistic function", line=0.5)
# add legend
legend("topleft", title="Scale parameters",
       paste("lambda", lamb_da, sep="="),
       inset=0.05, cex=0.8, lwd=2,
       lty=c(1, 1, 1), col=col_ors)
set.seed(1121)
# simulate overlapping scores data
scores_1 <- runif(100, max=0.6)
scores_2 <- runif(100, min=0.4)
# perform Wilcoxon test for mean
wilcox.test(scores_1, scores_2)
# combine scores and add categorical variable
score_s <- c(scores_1, scores_2)
res_ponse <- c(logical(100), !logical(100))
# perform logit regression
g_lm <- glm(res_ponse ~ score_s, family=binomial(logit))
class(g_lm)
summary(g_lm)
x11(width=7, height=5)
par(mar=c(3, 3, 2, 2), mgp=c(2, 1, 0), oma=c(0, 0, 0, 0))
or_der <- order(score_s)
plot(x=score_s[or_der], y=g_lm$fitted.values[or_der],
     type="l", lwd=4, col="orange",
     main="Category Densities and Logistic Function",
     xlab="score", ylab="density")
den_sity <- density(score_s[res_ponse])
den_sity$y <- den_sity$y/max(den_sity$y)
lines(den_sity, col="red")
polygon(c(min(den_sity$x), den_sity$x, max(den_sity$x)), c(min(den_sity$y), den_sity$y, min(den_sity$y)), col=rgb(1, 0, 0, 0.2), border=NA)
den_sity <- density(score_s[!res_ponse])
den_sity$y <- den_sity$y/max(den_sity$y)
lines(den_sity, col="blue")
polygon(c(min(den_sity$x), den_sity$x, max(den_sity$x)), c(min(den_sity$y), den_sity$y, min(den_sity$y)), col=rgb(0, 0, 1, 0.2), border=NA)
# add legend
legend(x="top", cex=1.0, bty="n", lty=c(1, NA, NA),
 lwd=c(6, NA, NA), pch=c(NA, 15, 15),
 legend=c("logistic fit", "TRUE", "FALSE"),
 col=c("orange", "red", "blue"),
 text.col=c("black", "red", "blue"))
library(ISLR)  # load package ISLR
# get documentation for package tseries
packageDescription("ISLR")  # get short description

help(package="ISLR")  # load help page

library(ISLR)  # load package ISLR

data(package="ISLR")  # list all datasets in ISLR

ls("package:ISLR")  # list all objects in ISLR

detach("package:ISLR")  # remove ISLR from search path
library(ISLR)  # load package ISLR
# load credit default data
attach(Default)
summary(Default)
sapply(Default, class)
dim(Default); head(Default)
x_lim <- range(balance)
y_lim <- range(income)
# plot data points for non-defaulters
default_ed <- (default=="Yes")
plot(income ~ balance,
     main="Default Dataset from Package ISLR",
     xlim=x_lim, ylim=y_lim,
     data=Default[!default_ed, ],
     pch=4, col="blue")
# plot data points for defaulters
points(income ~ balance,
 data=Default[default_ed, ],
 pch=4, lwd=2, col="red")
# add legend
legend(x="topright", bty="n",
 legend=c("non-defaulters", "defaulters"),
 col=c("blue", "red"), lty=1, lwd=6, pch=4)
default_ed <- (default=="Yes")
# Wilcoxon test for balance predictor
wilcox.test(balance[default_ed], balance[!default_ed])
# Wilcoxon test for income predictor
wilcox.test(income[default_ed], income[!default_ed])
library(ISLR)  # load package ISLR
attach(Default)  # load credit default data
par(mfrow=c(1,2))  # set plot panels
# balance boxplot
boxplot(formula=balance ~ default,
  col="lightgrey",
  main="balance", xlab="default")
# income boxplot
boxplot(formula=income ~ default,
  col="lightgrey",
  main="income", xlab="default")
# fit logistic regression model
g_lm <- glm(default ~ balance,
        family=binomial(logit))
class(g_lm)
summary(g_lm)
plot(x=balance, y=default_ed,
     main="Logistic regression of credit defaults", col="orange",
     xlab="credit balance", ylab="defaults")
or_der <- order(balance)
lines(x=balance[or_der], y=g_lm$fitted.values[or_der],
col="blue", lwd=2)
legend(x="topleft", inset=0.1,
 legend=c("defaults", "logit fitted values"),
 col=c("orange", "blue"), lty=c(NA, 1), pch=c(1, NA), lwd=c(3, 3))
library(ISLR)  # load package ISLR
attach(Default)  # load credit default data
# calculate cumulative defaults
default_ed <- (default=="Yes")
to_tal <- sum(default_ed)
default_s <- sapply(balance, function(ba_lance) {
    sum(default_ed[balance <= ba_lance])
})  # end sapply
# perform logit regression
g_lm <- glm(
  cbind(default_s, to_tal-default_s) ~
    balance,
  family=binomial(logit))
summary(g_lm)
plot(x=balance, y=default_s/to_tal, col="orange", lwd=1,
     main="Cumulative defaults versus balance",
     xlab="credit balance", ylab="cumulative defaults")
or_der <- order(balance)
lines(x=balance[or_der], y=g_lm$fitted.values[or_der],
col="blue", lwd=2)
legend(x="topleft", inset=0.1,
 legend=c("cumulative defaults", "fitted values"),
 col=c("orange", "blue"), lty=c(NA, 1), pch=c(1, NA), lwd=c(3, 3))
library(ISLR)  # load package ISLR
attach(Default)  # load credit default data
# fit multifactor logistic regression model
col_names <- colnames(Default)
for_mula <- as.formula(paste(col_names[1],
  paste(col_names[-1], collapse="+"), sep=" ~ "))
g_lm <- glm(for_mula, data=Default,
        family=binomial(logit))
summary(g_lm)
library(ISLR)  # load package ISLR
attach(Default)  # load credit default data
default_ed <- (default=="Yes")
stu_dent <- (student=="Yes")
# calculate cumulative defaults
default_s <- sapply(balance,
  function(ba_lance) {
    c(stu_dent=sum(default_ed[stu_dent & (balance <= ba_lance)]),
non_student=sum(default_ed[(!stu_dent) & (balance <= ba_lance)]))
})  # end sapply
to_tal <- c(sum(default_ed[stu_dent]), sum(default_ed[!stu_dent]))
default_s <- t(default_s / to_tal)
library(ISLR)  # load package ISLR
attach(Default)  # load credit default data
# plot cumulative defaults
par(mfrow=c(1,2))  # set plot panels
or_der <- order(balance)
plot(x=balance[or_der], y=default_s[or_der, 1],
     col="red", t="l", lwd=2,
     main="Cumulative defaults of\n students and non-students",
     xlab="credit balance", ylab="")
lines(x=balance[or_der], y=default_s[or_der, 2],
col="blue", lwd=2)
legend(x="topleft", bty="n",
 legend=c("students", "non-students"),
 col=c("red", "blue"), text.col=c("red", "blue"),
 lwd=3)
# balance boxplot for student factor
boxplot(formula=balance ~ student,
  col="lightgrey",
  main="balance", xlab="student")
# fit full logistic regression model
for_mula <- as.formula(paste(col_names[1],
  paste(col_names[-1], collapse="+"), sep=" ~ "))
g_lm <- glm(for_mula, data=Default, family=binomial(logit))
fore_casts <- predict(g_lm, type="response")
fore_casts[1:6]
all.equal(g_lm$fitted.values, fore_casts)
# discrimination threshold
thresh_old <- 0.05
# calculate confusion matrix
table(default_ed, (fore_casts>thresh_old))
sum(default_ed)
sum(Default$default=="Yes")
# fit logistic regression over training data
set.seed(1121)  # reset random number generator
sam_ple <- sample(x=1:NROW(Default), size=NROW(Default)/2)
train_data <- Default[sam_ple, ]
g_lm <- glm(for_mula, data=train_data, family=binomial(link="logit"))
# forecast over test data
test_data <- Default[-sam_ple, ]
fore_casts <- predict(g_lm, newdata=test_data, type="response")
# calculate confusion matrix
table(test_data$default=="No",
(fore_casts<thresh_old))
# FALSE positive (type I error)
sum(test_data$default=="No" & (fore_casts>thresh_old))
# FALSE negative (type II error)
sum(test_data$default=="Yes" & (fore_casts<thresh_old))
detach(Default)
library(ISLR)  # load package ISLR
attach(Default)  # load credit default data
col_names <- colnames(Default)
for_mula <- as.formula(paste(col_names[1], paste(col_names[-1], collapse="+"), sep=" ~ "))
set.seed(1121)  # reset random number generator
sam_ple <- sample(x=1:NROW(Default), size=NROW(Default)/2)
train_data <- Default[sam_ple, ]
g_lm <- glm(for_mula, data=train_data, family=binomial(link="logit"))
test_data <- Default[-sam_ple, ]
fore_casts <- predict(g_lm, newdata=test_data, type="response")
thresh_old <- 0.05
# calculate confusion matrix
confu_sion <- table(test_data$default=="No",
              (fore_casts<thresh_old))
dimnames(confu_sion) <- list(hypothesis=rownames(confu_sion),
  forecast=colnames(confu_sion))
confu_sion
confu_sion <- confu_sion / rowSums(confu_sion)
c(typeI=confu_sion[2, 1], typeII=confu_sion[1, 2])
# below is an unsuccessful attempt to draw confusion matrix using xtable
confusion_matrix <- matrix(c("| true positive \\\\ (sensitivity)", "| false negative \\\\ (type II error)", "| false positive \\\\ (type I error)", "| true negative \\\\ (specificity)"), nc=2)
dimnames(confusion_matrix) <- list(forecast=c("FALSE", "TRUE"),
                             hypothesis=c("FALSE", "TRUE"))
print(xtable::xtable(confusion_matrix,
caption="Confusion Matrix"),
caption.placement="top",
comment=FALSE, size="scriptsize",
include.rownames=TRUE,
include.colnames=TRUE)
# end unsuccessful attempt to draw confusion table using xtable
# confusion matrix as function of thresh_old
con_fuse <- function(res_ponse, fore_casts, thresh_old) {
    confu_sion <- table(res_ponse, (fore_casts<thresh_old))
    confu_sion <- confu_sion / rowSums(confu_sion)
    c(typeI=confu_sion[2, 1], typeII=confu_sion[1, 2])
  }  # end con_fuse
con_fuse(test_data$default=="No", fore_casts, thresh_old=thresh_old)
# define vector of discrimination thresholds
threshold_s <- seq(0.01, 0.95, by=0.01)^2
# calculate error rates
error_rates <- sapply(threshold_s, con_fuse,
  res_ponse=(test_data$default=="No"),
  fore_casts=fore_casts)  # end sapply
error_rates <- t(error_rates)
# calculate area under ROC curve (AUC)
(error_rates[, "typeII"] - 1) %*%
  rutils::diff_it(error_rates[, "typeI"])
# or
type_two <- (error_rates[, "typeII"] +
  rutils::lag_it(error_rates[, "typeII"]))/2
(type_two - 1) %*% rutils::diff_it(error_rates[, "typeI"])
# plot ROC Curve for Defaults
plot(x=error_rates[, "typeI"],
     y=1-error_rates[, "typeII"],
     xlab="FALSE positive rate",
     ylab="TRUE positive rate",
     main="ROC Curve for Defaults",
     type="l", lwd=3, col="blue")
abline(a=0.0, b=1.0, lwd=3, col="orange")
# symbols for constant maturity Treasury rates
sym_bols <- c("DGS1", "DGS2", "DGS5", "DGS10", "DGS20", "DGS30")
library(quantmod)  # load package quantmod
env_rates <- new.env()  # new environment for data
# download data for sym_bols into env_rates
getSymbols(sym_bols, env=env_rates, src="FRED")
ls(env_rates)  # list files in env_rates
# get class of object in env_rates
class(get(x=sym_bols[1], envir=env_rates))
# another way
class(env_rates$DGS10)
colnames(env_rates$DGS10)
save(env_rates, file="C:/Develop/R/lecture_slides/data/rates_data.RData")
x11(width=6, height=4)
par(mar=c(2, 2, 0, 0), oma=c(0, 0, 0, 0))
head(env_rates$DGS10, 3)
# get class of all objects in env_rates
eapply(env_rates, class)
# get class of all objects in R workspace
lapply(ls(), function(ob_ject) class(get(ob_ject)))
# plot 10-year constant maturity Treasury rate
chart_Series(env_rates$DGS10["1990/"],
  name="10-year constant maturity Treasury rate")
par(mar=c(3, 3, 2, 0), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
# load constant maturity Treasury rates
load(file="C:/Develop/R/lecture_slides/data/rates_data.RData")
# get end-of-year dates since 2006
date_s <- xts::endpoints(env_rates$DGS1["2006/"], on="years")
date_s <- zoo::index(env_rates$DGS1["2006/"])[date_s]
# create time series of end-of-year rates
rate_s <- eapply(env_rates, function(ra_te) ra_te[date_s])
rate_s <- rutils::do_call(cbind, rate_s)
# rename columns and rows, sort columns, and transpose into matrix
colnames(rate_s) <- substr(colnames(rate_s), start=4, stop=11)
rate_s <- rate_s[, order(as.numeric(colnames(rate_s)))]
colnames(rate_s) <- paste0(colnames(rate_s), "yr")
rate_s <- t(rate_s)
colnames(rate_s) <- substr(colnames(rate_s), start=1, stop=4)
# plot matrix using plot.zoo()
col_ors <- colorRampPalette(c("red", "blue"))(NCOL(rate_s))
plot.zoo(rate_s, main="Yield curve since 2006", lwd=3, xaxt="n",
   plot.type="single", xlab="maturity", ylab="yield", col=col_ors)
# add x-axis
axis(1, seq_along(rownames(rate_s)), rownames(rate_s))
# add legend
legend("bottomright", legend=colnames(rate_s),
 col=col_ors, lty=1, lwd=4, inset=0.05, cex=0.8)
# alternative plot using matplot()
matplot(rate_s, main="Yield curve since 2006", xaxt="n", lwd=3, lty=1,
  type="l", xlab="maturity", ylab="yield", col=col_ors)
# add x-axis
axis(1, seq_along(rownames(rate_s)), rownames(rate_s))
# add legend
legend("bottomright", legend=colnames(rate_s),
 col=col_ors, lty=1, lwd=4, inset=0.05, cex=0.8)
x11(width=6, height=4)
par(mar=c(0, 0, 0, 0), oma=c(0, 0, 0, 0), mgp=c(0, 0, 0))
# load constant maturity Treasury rates
load(file="C:/Develop/R/lecture_slides/data/rates_data.RData")
# symbols for constant maturity Treasury rates
sym_bols <- c("DGS1", "DGS2", "DGS5", "DGS10", "DGS20")
# calculate daily rates changes
rate_s <- xts:::na.locf.xts(rutils::do_call(cbind,
    as.list(env_rates)[sym_bols]))
rate_s <- xts:::na.locf.xts(rate_s)
rate_s <- xts:::na.locf.xts(rate_s, fromLast=TRUE)
re_turns <- rutils::diff_it(rate_s)
date_s <- index(re_turns)
# de-mean (center) and scale the returns
re_turns <- t(t(re_turns) - colMeans(re_turns))
re_turns <- t(t(re_turns) / sqrt(colSums(re_turns^2)/(NROW(re_turns)-1)))
re_turns <- xts(re_turns, date_s)
# correlation matrix of Treasury rates
cor_mat <- cor(re_turns)
# reorder correlation matrix based on clusters
library(corrplot)
or_der <- corrMatOrder(cor_mat, order="hclust",
  hclust.method="complete")
cor_mat <- cor_mat[or_der, or_der]
# plot the correlation matrix
col_ors <- colorRampPalette(c("red", "white", "blue"))
corrplot(cor_mat, title=NA, tl.col="black",
  tl.cex=0.8, mar=c(0,0,0,0), method="square",
  col=col_ors(8), cl.offset=0.75, cl.cex=0.7,
  cl.align.text="l", cl.ratio=0.25)
title("Correlation of Treasury rates", line=-1)
# draw rectangles on the correlation matrix plot
corrRect.hclust(cor_mat, k=NROW(cor_mat) %/% 2,
  method="complete", col="red")
# create initial vector of portfolio weights
n_weights <- NROW(sym_bols)
weight_s <- rep(1/sqrt(n_weights), n_weights)
names(weight_s) <- sym_bols
# objective function equal to minus portfolio variance
object_ive <- function(weight_s, re_turns) {
  portf_rets <- re_turns %*% weight_s
  -sum(portf_rets*portf_rets) +
    1e7*(1 - sum(weight_s*weight_s))^2
}  # end object_ive
# objective for equal weight portfolio
object_ive(weight_s, re_turns)
# compare speed of vector multiplication methods
summary(microbenchmark(
  trans_pose=t(re_turns) %*% re_turns,
  s_um=sum(re_turns*re_turns),
  times=10))[, c(1, 4, 5)]
# find weights with maximum variance
optim_run <- optim(par=weight_s,
  fn=object_ive,
  re_turns=re_turns,
  method="L-BFGS-B",
  upper=rep(1.0, n_weights),
  lower=rep(-1.0, n_weights))
# optimal weights and maximum variance
weight_s <- optim_run$par
-object_ive(weight_s, re_turns)
# plot first principal component loadings
barplot(weight_s, names.arg=names(weight_s),
  xlab="", ylab="",
  main="first principal component loadings")
# pc1 weights and returns
weights_1 <- weight_s
pc_1 <- re_turns %*% weights_1
# redefine objective function
object_ive <- function(weight_s, re_turns) {
  portf_rets <- re_turns %*% weight_s
  -sum(portf_rets*portf_rets) +
    1e7*(1 - sum(weight_s*weight_s))^2 +
    1e7*sum(pc_1*portf_rets)^2
}  # end object_ive
# find second principal component weights
optim_run <- optim(par=weight_s,
             fn=object_ive,
             re_turns=re_turns,
             method="L-BFGS-B",
             upper=rep(1.0, n_weights),
             lower=rep(-1.0, n_weights))
# pc2 weights and returns
weights_2 <- optim_run$par
pc_2 <- re_turns %*% weights_2
sum(pc_1*pc_2)
# plot second principal component loadings
barplot(weights_2, names.arg=names(weights_2),
  xlab="", ylab="",
  main="second principal component loadings")
# covariance matrix and variance vector of returns
cov_mat <- cov(re_turns)
vari_ance <- diag(cov_mat)
cor_mat <- cor(re_turns)
# calculate eigenvectors and eigenvalues
ei_gen <- eigen(cov_mat)
ei_gen$vectors
weights_1
weights_2
ei_gen$values[1]
var(pc_1)
(cov_mat %*% weights_1) / weights_1
ei_gen$values[2]
var(pc_2)
(cov_mat %*% weights_2) / weights_2
sum(vari_ance)
sum(ei_gen$values)
barplot(ei_gen$values, # plot eigenvalues
  names.arg=paste0("PC", 1:n_weights),
  las=3, xlab="", ylab="", main="Principal Component Variances")
# perform principal component analysis PCA
pc_a <- prcomp(re_turns, scale=TRUE)
# plot standard deviations
barplot(pc_a$sdev,
  names.arg=colnames(pc_a$rotation),
  las=3, xlab="", ylab="",
  main="Scree Plot: Volatilities of Principal Components
  of Treasury rates")
x11(width=6, height=7)
# principal component loadings (weights)
pc_a$rotation
# plot loading barplots in multiple panels
par(mfrow=c(3,2))
par(mar=c(2, 2, 2, 1), oma=c(0, 0, 0, 0))
for (or_der in 1:NCOL(pc_a$rotation)) {
  barplot(pc_a$rotation[, or_der],
  las=3, xlab="", ylab="", main="")
  title(paste0("PC", or_der), line=-2.0,
  col.main="red")
}  # end for
# principal component time series
pca_ts <- xts(re_turns %*% pc_a$rotation,
          order.by=index(re_turns))
pca_ts <- cumsum(pca_ts)
# plot principal component time series in multiple panels
par(mfrow=c(3,2))
par(mar=c(2, 2, 0, 1), oma=c(0, 0, 0, 0))
ra_nge <- range(pca_ts)
for (or_der in 1:NCOL(pca_ts)) {
  plot.zoo(pca_ts[, or_der],
     ylim=ra_nge,
     xlab="", ylab="")
  title(paste0("PC", or_der), line=-2.0)
}  # end for
# create a list with two elements
list_var <- list(c('a', 'b'), 1:4)
list_var
c(typeof(list_var), mode(list_var), class(list_var))
# lists are also vectors
c(is.vector(list_var), is.list(list_var))
NROW(list_var)
# create named list
list_var <- list(first=c('a', 'b'), second=1:4)
list_var
names(list_var)
unlist(list_var)
list_var[2]  # extract second element as sublist
list_var[[2]]  # extract second element
list_var[[2]][3]  # extract third element of second element
list_var[[c(2, 3)]]  # third element of second element
list_var$second  # extract second element
list_var$s  # extract second element - partial name matching
list_var$second[3]  # third element of second element
list_var <- list()  # empty list
list_var$a <- 1
list_var[2] <- 2
list_var
names(list_var)
as.list(c(1,2,3))
list(c(1,2,3))
data_frame <- data.frame(  # create a data frame
                type=c('rose', 'daisy', 'tulip'),
                color=c('red', 'white', 'yellow'),
                price=c(1.5, 0.5, 1.0)
              )  # end data.frame
data_frame
dim(data_frame)  # get dimension attribute
colnames(data_frame)  # get the colnames attribute
rownames(data_frame)  # get the rownames attribute
class(data_frame)  # get object class
typeof(data_frame)  # data frames are lists
is.data.frame(data_frame)

class(data_frame$type)  # get column class
class(data_frame$price)  # get column class
data_frame[, 3]  # extract third column as vector
data_frame[[3]]  # extract third column as vector
data_frame[3]  # extract third column as data frame
data_frame[, 3, drop=FALSE]  # extract third column as data frame
data_frame[[3]][2]  # second element from third column
data_frame$price[2]  # second element from 'price' column
is.data.frame(data_frame[[3]]); is.vector(data_frame[[3]])
data_frame[2, ]  # extract second row
data_frame[2, ][3]  # third element from second column
data_frame[2, 3]  # third element from second column
unlist(data_frame[2, ])  # coerce to vector
is.data.frame(data_frame[2, ]); is.vector(data_frame[2, ])
data_frame <- data.frame(  # create a data frame
                type=c('rose', 'daisy', 'tulip'),
                color=c('red', 'white', 'yellow'),
                price=c(1.5, 0.5, 1.0),
                row.names=c('flower1', 'flower2', 'flower3'),
                stringsAsFactors=FALSE
              )  # end data.frame
data_frame
class(data_frame$type)  # get column class
class(data_frame$price)  # get column class
# set option to not coerce character vectors to factors
options(stringsAsFactors=FALSE)
options("stringsAsFactors")
default.stringsAsFactors()
str(data_frame)  # display the object structure
dim(cars)  # the cars data frame has 50 rows
head(cars, n=5)  # get first five rows
tail(cars, n=5)  # get last five rows
# create a named vector
stu_dents <- sample(round(runif(5, min=1, max=10), digits=2))
names(stu_dents) <- c("Angie", "Chris", "Suzie", "Matt", "Liz")
# sort the vector into ascending order
sort(stu_dents)
# calculate index to sort into ascending order
order(stu_dents)
# sort the vector into ascending order
stu_dents[order(stu_dents)]
# calculate the sorted (ordered) vector
sort_ed <- stu_dents[order(stu_dents)]
# calculate index to sort into unsorted (original) order
order(order(stu_dents))
sort_ed[order(order(stu_dents))]
stu_dents
# create a data frame of stu_dents and their ranks
ra_nks <- c("first", "second", "third", "fourth", "fifth")
data.frame(students=stu_dents, rank=ra_nks[order(order(stu_dents))])
# permute data_frame of flowers on price column
order(data_frame$price)
# sort data_frame on price
data_frame[order(data_frame$price), ]
# sort data_frame on color
data_frame[order(data_frame$color), ]
order(c(2, 1:4))  # there's a tie
order(c(2, 1:4), 1:5)  # there's a tie
# read the Examples for sort()
as.matrix(data_frame)
vec_tor <- sample(9)
matrix(vec_tor, ncol=3)
as.matrix(vec_tor, ncol=3)
mat_rix <- matrix(5:10, nrow=2, ncol=3)  # create a matrix
rownames(mat_rix) <- c("row1", "row2")  # rownames attribute
colnames(mat_rix) <- c("col1", "col2", "col3")  # colnames attribute
library(microbenchmark)
# call method instead of generic function
as.data.frame.matrix(mat_rix)
# a few methods for generic function as.data.frame()
sample(methods(as.data.frame), size=4)
# function method is faster than generic function
summary(microbenchmark(
  as_data_frame_matrix=
    as.data.frame.matrix(mat_rix),
  as_data_frame=as.data.frame(mat_rix),
  data_frame=data.frame(mat_rix),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
library(microbenchmark)
# lapply is faster than coercion function
summary(microbenchmark(
  as_list=
    as.list(as.data.frame.matrix(mat_rix)),
  l_apply=
    lapply(seq_along(mat_rix[1, ]),
     function(in_dex) mat_rix[, in_dex]),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# ?iris  # get information on iris
dim(iris)
head(iris, 2)
colnames(iris)
unique(iris$Species)  # list of unique elements of iris
class(unique(iris$Species))
# find which columns of iris are numeric
sapply(iris, is.numeric)
# calculate means of iris columns
sapply(iris, mean)  # returns NA for Species
# ?mtcars  # mtcars data from 1974 Motor Trend magazine
# mpg   Miles/(US) gallon
# qsec   1/4 mile time
# hp	 Gross horsepower
# wt	 Weight (lb/1000)
# cyl   Number of cylinders
dim(mtcars)
head(mtcars, 2)
colnames(mtcars)
head(rownames(mtcars), 3)
unique(mtcars$cyl)  # extract list of car cylinders
sapply(mtcars, mean)  # calculate means of mtcars columns
library(MASS)
# ?Cars93  # get information on Cars93
dim(Cars93)
head(colnames(Cars93))
# head(Cars93, 2)
unique(Cars93$Type)  # extract list of car types
# sapply(Cars93, mean)  # calculate means of Cars93 columns
# plot histogram of Highway MPG using the Freedman-Diaconis rule
hist(Cars93$MPG.highway, col="lightblue1",
     main="Distance per Gallon 1993", xlab="Highway MPG", breaks="FD")
rm(list=ls())
as.numeric(c(1:3, "a"))  # NA from coercion
0/0  # NaN from ambiguous math
1/0  # Inf from divide by zero
is.na(c(NA, NaN, 0/0, 1/0))  # test for NA
is.nan(c(NA, NaN, 0/0, 1/0))  # test for NaN
NA*1:4  # create vector of Nas
# create vector with some NA values
da_ta <- c(1, 2, NA, 4, NA, 5)
da_ta
mean(da_ta)  # returns NA, when NAs are input
mean(da_ta, na.rm=TRUE)  # remove NAs from input data
da_ta[!is.na(da_ta)]  # delete the NA values
sum(!is.na(da_ta))  # count non-NA values
rm(list=ls())
# airquality data has some NAs
head(airquality)
dim(airquality)
# number of NAs
sum(!complete.cases(airquality))
# display rows containing NAs
head(airquality[!complete.cases(airquality), ])
rm(list=ls())
# remove rows containing NAs
good_air <- airquality[complete.cases(airquality), ]
dim(good_air)
head(good_air)  # NAs removed
library(zoo)  # load package zoo
# Replace NAs
good_air <- zoo::na.locf(airquality)
dim(good_air)
head(good_air)  # NAs replaced
# Create vector containing NA values
vec_tor <- sample(22)
vec_tor[sample(NROW(vec_tor), 4)] <- NA
# Replace NA values with the most recent non-NA values
zoo::na.locf(vec_tor)
# Replace NAs in xts time series
se_ries <- rutils::etf_env$price_s[, 1]
head(se_ries)
sum(is.na(se_ries))
library(quantmod)
series_zoo <- as.xts(zoo::na.locf(se_ries, fromLast=TRUE))
series_xts <- xts:::na.locf.xts(se_ries, fromLast=TRUE)
all.equal(series_zoo, series_xts, check.attributes=FALSE)
library(microbenchmark)
summary(microbenchmark(
  zoo=as.xts(zoo::na.locf(se_ries, fromLast=TRUE)),
  xts=xts:::na.locf.xts(se_ries, fromLast=TRUE),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# NULL values have no mode or type
c(mode(NULL), mode(NA))
c(typeof(NULL), typeof(NA))
c(length(NULL), length(NA))
# check for NULL values
is.null(NULL)
# NULL values are ignored when combined into a vector
c(1, 2, NULL, 4, 5)
# But NA value isn't ignored
c(1, 2, NA, 4, 5)
# vectors can be initialized to NULL
vec_tor <- NULL
is.null(vec_tor)
# grow the vector in a loop - very bad code!!!
for (in_dex in 1:5)
  vec_tor <- c(vec_tor, in_dex)
# initialize empty vector
vec_tor <- numeric()
# grow the vector in a loop - very bad code!!!
for (in_dex in 1:5)
  vec_tor <- c(vec_tor, in_dex)
# allocate vector
vec_tor <- numeric(5)
# assign to vector in a loop - good code
for (in_dex in 1:5)
  vec_tor[in_dex] <- runif(1)
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
setwd("C:/Develop/R/lecture_slides/data")
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
setwd("C:/Develop/R/lecture_slides/data")
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
# write data frame to CSV file, and then read it back
write.csv(data_frame, file="florist.csv")
data_read <- read.csv(file="florist.csv",
                 stringsAsFactors=FALSE)
data_read  # the row names are read in as extra column
# restore row names
rownames(data_read) <- data_read[, 1]
data_read <- data_read[, -1]  # remove extra column
data_read
# read data frame, with row names from first column
data_read <- read.csv(file="florist.csv", row.names=1)
data_read
# write data frame to CSV file, without row names
write.csv(data_frame, row.names=FALSE, file="florist.csv")
data_read <- read.csv(file="florist.csv")
data_read  # a data frame without row names
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
setwd("C:/Develop/R/lecture_slides/data")
library(MASS)  # load package "MASS"
# write to CSV file by row - it's very SLOW!!!
MASS::write.matrix(mat_rix,
  file="matrix.csv", sep=",")
# read using scan() and skip first line with colnames
mat_read <- scan(file="matrix.csv",
  sep=",", skip=1, what=numeric())
# read colnames
col_names <- readLines(con="matrix.csv", n=1)
col_names  # this is a string!
# convert to char vector
col_names <- strsplit(col_names,
  s=",")[[1]]
mat_read  # mat_read is a vector, not matrix!
# coerce by row to matrix
mat_read <- matrix(mat_read,
  ncol=NROW(col_names), byrow=TRUE)
# restore colnames
colnames(mat_read) <- col_names
mat_read
# scan() is a little faster than read.csv()
library(microbenchmark)
summary(microbenchmark(
  read_csv=read.csv("matrix.csv"),
  scan=scan(file="matrix.csv", sep=",",
    skip=1, what=numeric()),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# read data from a csv file, including row names
mat_rix <- read.csv(file="matrix_bad.csv",
  row.names=1, stringsAsFactors=FALSE)
mat_rix
class(mat_rix)
# columns with bad data are character or factor
sapply(mat_rix, class)
# copy row names
row_names <- row.names(mat_rix)
# sapply loop over columns and coerce to numeric
mat_rix <- sapply(mat_rix, as.numeric)
# restore row names
row.names(mat_rix) <- row_names
# replace NAs with zero
mat_rix[is.na(mat_rix)] <- 0
# matrix without NAs
mat_rix
setwd("C:/Develop/R/lecture_slides/data")
rm(list=ls())
set.seed(1121)  # reset random number generator
library(zoo)  # load package zoo
# create zoo with Date index
in_dex <- seq(from=as.Date("2013-06-15"),
            by="day", length.out=100)
zoo_series <- zoo(cumsum(rnorm(NROW(in_dex))),
            order.by=in_dex)
tail(zoo_series, 3)
# write zoo series to text file, and then read it back
write.zoo(zoo_series, file="zoo_series.txt")
zoo_series_read <- read.zoo("zoo_series.txt")  # read it back
tail(zoo_series, 3)
all.equal(zoo_series_read, zoo_series)
# Perform the same using write.table() and read.table()
# first coerce zoo_series into data frame
data_frame <- as.data.frame(zoo_series)
data_frame <- cbind(in_dex, data_frame)
# write zoo_series to text file using write.table
write.table(data_frame, file="zoo_series.txt",
      row.names=FALSE, col.names=FALSE)
# read data frame from file
zoo_series_read <- read.table(file="zoo_series.txt",
                        stringsAsFactors=FALSE)
sapply(zoo_series_read, class)  # a data frame
# coerce data frame into zoo_series
zoo_series_read <- zoo::zoo(
  drop(as.matrix(zoo_series_read[, -1])),
  order.by=as.Date(zoo_series_read[, 1]))
all.equal(zoo_series_read, zoo_series)
set.seed(1121)  # reset random number generator
library(zoo)  # load package zoo
# create zoo with POSIXct date-time index
in_dex <- seq(from=as.POSIXct("2013-06-15"),
            by="hour", length.out=1000)
zoo_series <- zoo(cumsum(rnorm(NROW(in_dex))),
            order.by=in_dex)
# write zoo series to text file, and then read it back
write.zoo(zoo_series, file="zoo_series.txt")
zoo_series_read <- read.zoo("zoo_series.txt")  # read it back
# time field was read as a separate column
tail(zoo_series_read, 3)
# read and specify that second column is time field
zoo_series_read <- read.zoo(file="zoo_series.txt",
                 index.column=list(1,2),
                 tz="America/New_York")
all.equal(zoo_series_read, zoo_series)
# Perform the same using write.table() and read.table()
# first coerce zoo_series into data frame
data_frame <- as.data.frame(zoo_series)
data_frame <- cbind(in_dex, data_frame)
# write zoo_series to text file using write.table
write.table(data_frame, file="zoo_series.txt",
      row.names=FALSE, col.names=FALSE)
# read data frame from file
zoo_series_read <- read.table(file="zoo_series.txt",
                        stringsAsFactors=FALSE)
sapply(zoo_series_read, class)  # a data frame
# paste first two date columns together and coerce into as.POSIXct
date_col <- 1:2
in_dex <- do.call(paste, zoo_series_read[, date_col])
in_dex <- as.POSIXct(in_dex, tz="America/New_York")
# coerce data frame into zoo_series
zoo_series_read <- zoo::zoo(
  drop(as.matrix(zoo_series_read[, -date_col])),
  order.by=in_dex)
all.equal(zoo_series_read, zoo_series)
library(zoo)  # load package zoo
# write zoo series to CSV file, and then read it back
write.zoo(zoo_series, file="zoo_series.csv",
    sep=",", col.names=TRUE)
zoo_series <- read.zoo(file="zoo_series.csv",
            header=TRUE, sep=",",
            drop=FALSE,
            FUN=as.POSIXct, tz="America/New_York")
tail(zoo_series, 3)
# read zoo from CSV file, with custom date-time format
zoo_frame <- read.table(file="zoo_series2.csv",
                  sep=",")
tail(zoo_frame, 3)  # date-time format mm/dd/yyyy hh:mm
zoo_series <- read.zoo(file="zoo_series2.csv",
            header=TRUE, sep=",",
            drop=FALSE,
            FUN=as.POSIXct,
            tz="America/New_York",
            format="%m/%d/%Y %H:%M")
tail(zoo_series, 3)
# read zoo from CSV file, with numeric date-time
zoo_series <- read.csv(file="es_ohlc.csv",
  sep=",")
zoo_series <- zoo::zoo(zoo_series[, -1],
  order.by=as.POSIXct.numeric(zoo_series[, 1],
    tz="America/New_York",
    origin="1970-01-01"))
head(zoo_series, 11)
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

png("r_plot.png")  # redirect graphics output to png file

cat("Redirect graphics from R into png file\n")
plot(x=my_var, y=sin(my_var), main="Sine wave",
 xlab="", ylab="", type="l", lwd=2, col="red")
cat("\nEnd data\nbye\n")

dev.off()  # turn png output off
