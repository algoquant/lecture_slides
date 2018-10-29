# R startup chunk
# ```{r setup, include=FALSE}
library(shiny)
library(quantmod)
inter_val <- 31
cl_ose <- quantmod::Cl(rutils::etf_env$VTI)
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue")
# ```
#end R startup chunk
inputPanel(
  sliderInput("lamb_da", label="lambda:",
    min=0.01, max=0.2, value=0.1, step=0.01)
)  # end inputPanel
renderPlot({
  # calculate EWMA prices
  lamb_da <- input$lamb_da
  weight_s <- exp(-lamb_da*1:inter_val)
  weight_s <- weight_s/sum(weight_s)
  ew_ma <- filter(cl_ose, filter=weight_s, sides=1)
  ew_ma[1:(inter_val-1)] <- ew_ma[inter_val]
  ew_ma <- xts(cbind(cl_ose, ew_ma), order.by=index(cl_ose))
  colnames(ew_ma) <- c("VTI", "VTI EWMA")
  # plot EWMA prices
  ch_ob <- chart_Series(ew_ma, theme=plot_theme, name="EWMA prices")
  plot(ch_ob)
  legend("top", legend=colnames(ew_ma),
   inset=0.1, bg="white", lty=1, lwd=2,
   col=plot_theme$col$line.col, bty="n")
})  # end renderPlot
set.seed(1121)  # initialize random number generator
# define design matrix
n_rows <- 100
n_cols <- 5
de_sign <- sapply(1:n_cols, function(col_umn) {
  sin(pi*col_umn*((1:n_rows)-(n_rows+1)/2)/n_rows)
})  # end sapply
# add column names
colnames(de_sign) <- paste0("col", 1:n_cols)
# plot design matrix
# matplot(de_sign, type="l", lty="solid", lwd=3)
# define the design weights
weight_s <- runif(n_cols, min=(-10), max=10)
# response equals linear form plus random noise
noise <- rnorm(n_rows, sd=0.1)
res_ponse <- (-1 + de_sign %*% weight_s + noise)
# add intercept column to design matrix
de_sign <- cbind(rep(1, NROW(de_sign)), de_sign)
# add column name
colnames(de_sign)[1] <- "intercept"
# calculate generalized inverse of the design matrix
design_inv <- MASS::ginv(de_sign)
# calculate the regression coefficients
beta_s <- design_inv %*% res_ponse
# perform multivariate regression without intercept term
mod_el <- lm(res_ponse ~ de_sign - 1)
all.equal(drop(beta_s), coef(mod_el), check.attributes=FALSE)
# calculate the influence matrix
influ_ence <- de_sign %*% design_inv
# calculate fitted values using influence matrix
fit_ted <- drop(influ_ence %*% res_ponse)
all.equal(fit_ted, mod_el$fitted.values, check.attributes=FALSE)
# calculate fitted values from regression coefficients
fit_ted <- drop(de_sign %*% beta_s)
all.equal(fit_ted, mod_el$fitted.values, check.attributes=FALSE)
# calculate the residuals
resid_uals <- drop(res_ponse - fit_ted)
all.equal(resid_uals, mod_el$residuals, check.attributes=FALSE)
# define transformation matrix
n_cols <- NCOL(de_sign)
trans_mat <- matrix(runif(n_cols^2, min=(-1), max=1), 
            ncol=n_cols)
# calculate linear combinations of design columns
design_trans <- de_sign %*% trans_mat
# calculate the influence matrix
influence_trans <- design_trans %*% MASS::ginv(design_trans)
# compare the influence matrices
all.equal(influ_ence, influence_trans)
# de-mean the design matrix columns
design_trans <- cbind(de_sign[, 1], t(t(de_sign[, -1])-colMeans(de_sign[, -1])))
round(apply(design_trans, 2, mean), 3)
# calculate the influence matrix
influence_trans <- design_trans %*% MASS::ginv(design_trans)
# compare the influence matrices
all.equal(influ_ence, influence_trans)
# regression model summary
model_sum <- summary(mod_el)
# degrees of freedom of residuals
deg_free <- (n_rows - NCOL(de_sign))
all.equal(deg_free, model_sum$df[2])
# variance of residuals
resid_var <- sum(resid_uals^2)/deg_free
# design matrix squared
design_2 <- crossprod(de_sign)
# design_2 <- t(de_sign) %*% de_sign
# calculate covariance matrix of betas
beta_covar <- resid_var*MASS::ginv(design_2)
# round(beta_covar, 3)
beta_sd <- sqrt(diag(beta_covar))
all.equal(beta_sd, model_sum$coeff[, 2], check.attributes=FALSE)
# calculate t-values of betas
beta_tvals <- drop(beta_s)/beta_sd
all.equal(beta_tvals, model_sum$coeff[, 3], check.attributes=FALSE)
# calculate two-sided p-values of betas
beta_pvals <- 2*pt(-abs(beta_tvals), df=deg_free)
all.equal(beta_pvals, model_sum$coeff[, 4], check.attributes=FALSE)
# calculate the influence matrix
influ_ence <- de_sign %*% design_inv
all.equal(influ_ence, influ_ence %*% influ_ence)
# calculate covariance matrix of fitted values
fit_covar <- resid_var*influ_ence
# calculate standard deviations of the fitted values
fit_sd <- sqrt(diag(fit_covar))
# plot the standard deviations
plot(fit_sd, type="l", lwd=3, col="blue", ylab="",
     main="Standard Deviations of Fitted Values")
x11(width=5, height=4)  # open x11 for plotting
# set plot parameters to reduce whitespace around plot
par(mar=c(5, 5, 2, 1), oma=c(0, 0, 0, 0))
# univariate regression with linear predictor
de_sign <- cbind(rep(1, n_rows), 1:n_rows/n_rows)
# calculate generalized inverse of the design matrix
design_inv <- MASS::ginv(de_sign)
# calculate the influence matrix
influ_ence <- de_sign %*% design_inv
# plot the leverage vector
plot(x=de_sign[,2], y=diag(influ_ence),
     type="l", lwd=3, col="blue",
     xlab="predictor", ylab="leverage",
     main="Leverage as Function of Predictor")
# define the design weights
weight_s <- c(-1, 1)
# response without random noise equals weighted sum over columns of de_sign
res_ponse <- de_sign %*% weight_s
# perform loop over different realizations of random noise
fit_ted <- lapply(1:50, function(it) {
  # add random noise to response
  res_ponse <- res_ponse + rnorm(n_rows, sd=1.0)
  # calculate fitted values using influence matrix
  influ_ence %*% res_ponse
})  # end lapply
fit_ted <- rutils::do_call(cbind, fit_ted)
x11(width=5, height=4)  # open x11 for plotting
# set plot parameters to reduce whitespace around plot
par(mar=c(5, 5, 2, 1), oma=c(0, 0, 0, 0))
# plot fitted values
matplot(x=de_sign[,2], y=fit_ted,
type="l", lty="solid", lwd=1, col="blue",
xlab="predictor", ylab="fitted",
main="Fitted Values for Different Realizations
of Random Noise")
lines(x=de_sign[,2], y=res_ponse, col="red", lwd=4)
legend(x="topleft", # add legend
       legend=c("response without noise", "fitted values"),
       title=NULL, inset=0.05, cex=0.8, lwd=6,
       lty=1, col=c("red", "blue"))
# univariate regression with linear predictor
de_sign <- cbind(rep(1, n_rows), 1:n_rows/n_rows)
res_ponse <- de_sign %*% weight_s + rnorm(n_rows, sd=0.3)
design_inv <- MASS::ginv(de_sign)
influ_ence <- de_sign %*% design_inv
beta_s <- design_inv %*% res_ponse
fit_ted <- drop(de_sign %*% beta_s)
resid_uals <- drop(res_ponse - fit_ted)
deg_free <- (n_rows - NCOL(de_sign))
r_ss <- sqrt(sum(resid_uals^2)/deg_free)
# Inverse of design matrix squared
design_2 <- MASS::ginv(crossprod(de_sign))
# Define new predictors
new_predictors <- (max(de_sign[, 2]) + 10*(1:5)/n_rows)
# Calculate the predicted values and standard errors
new_design <- cbind(rep(1, NROW(new_predictors)), new_predictors)
predic_tions <- cbind(
  predicted=drop(new_design %*% beta_s),
  stddev=diag(r_ss*sqrt(new_design %*% design_2 %*% t(new_design))))
# OR: Perform loop over new_predictors
predic_tions <- sapply(new_predictors, function(predic_tor) {
  predic_tor <- cbind(1, predic_tor)
  # calculate predicted values
  predic_ted <- predic_tor %*% beta_s
  # calculate standard deviation
  predict_sd <- r_ss*sqrt(predic_tor %*% design_2 %*% t(predic_tor))
  c(predicted=predic_ted, stddev=predict_sd)
})  # end sapply
predic_tions <- t(predic_tions)
# prepare plot data
x_data <- c(de_sign[,2], new_predictors)
x_lim <- range(x_data)
y_data <- c(fit_ted, predic_tions[, 1])
# calculate t-quantile
t_quant <- qt(pnorm(2), df=deg_free)
predict_low <- predic_tions[, 1]-t_quant*predic_tions[, 2]
predict_high <- predic_tions[, 1]+t_quant*predic_tions[, 2]
y_lim <- range(c(res_ponse, y_data, predict_low, predict_high))
# plot the regression predictions
plot(x=x_data, y=y_data,
     xlim=x_lim, ylim=y_lim,
     type="l", lwd=3, col="blue",
     xlab="predictor", ylab="fitted or predicted",
     main="Predictions from Linear Regression")
points(x=de_sign[,2], y=res_ponse, col="blue")
points(x=new_predictors, y=predic_tions[, 1], pch=16, col="blue")
lines(x=new_predictors, y=predict_high, lwd=3, col="red")
lines(x=new_predictors, y=predict_low, lwd=3, col="green")
legend(x="topleft", # add legend
       legend=c("predictions", "+2SD", "-2SD"),
       title=NULL, inset=0.05, cex=0.8, lwd=6,
       lty=1, col=c("blue", "red", "green"))
# perform regression
predic_tor <- de_sign[, 2]
mod_el <- lm(res_ponse ~ predic_tor)
# perform prediction from regression
new_data <- data.frame(predic_tor=new_predictors)
predict_lm <- predict(object=mod_el,
  newdata=new_data, level=1-2*(1-pnorm(2)),
  interval="confidence")
predict_lm <- as.data.frame(predict_lm)
all.equal(predict_lm$fit, predic_tions[, 1])
all.equal(predict_lm$lwr, predict_low)
all.equal(predict_lm$upr, predict_high)
plot(res_ponse ~ predic_tor,
     xlim=range(predic_tor, new_data),
     ylim=range(res_ponse, predict_lm),
     xlab="predictor", ylab="fitted or predicted",
     main="Predictions from lm() Regression")
abline(mod_el, col="blue", lwd=3)
with(predict_lm, {
  points(x=new_data$predic_tor, y=fit, pch=16, col="blue")
  lines(x=new_data$predic_tor, y=lwr, lwd=3, col="green")
  lines(x=new_data$predic_tor, y=upr, lwd=3, col="red")
})  # end with
legend(x="topleft", # add legend
       legend=c("predictions", "+2SD", "-2SD"),
       title=NULL, inset=0.05, cex=0.8, lwd=6,
       lty=1, col=c("blue", "red", "green"))
# perform PCA
pc_a <- prcomp(design_zm, 
       center=TRUE, scale=TRUE)
design_pca <- pc_a$x
round(cov(design_pca), 2)
round(apply(design_pca, 2, mean), 3)
round(apply(design_pca, 2, sd), 2)
# calculate the influence matrix
influ_ence <- design_zm %*% MASS::ginv(design_zm)
influence_pca <- design_pca %*% MASS::ginv(design_pca)
all.equal(influ_ence, influence_pca)
# calculate the fitted values
fit_ted <- drop(de_sign %*% beta_s)
all.equal(fit_ted, mod_el$fitted.values, check.attributes=FALSE)
# calculate the residuals
resid_uals <- drop(res_ponse - fit_ted)
all.equal(resid_uals, mod_el$residuals, check.attributes=FALSE)
# residuals are orthogonal to fitted values
all.equal(sum(resid_uals*fit_ted), target=0)
# TSS = ESS + RSS
t_ss <- (n_rows-1)*var(drop(res_ponse))
e_ss <- (n_rows-1)*var(fit_ted)
r_ss <- (n_rows-1)*var(resid_uals)
all.equal(t_ss, e_ss + r_ss)
# regression summary
model_sum <- summary(mod_el)
# regression R-squared
r_squared <- e_ss/t_ss
all.equal(r_squared, model_sum$r.squared)
# correlation between response and fitted values
cor_fitted <- drop(cor(res_ponse, fit_ted))
# squared correlation between response and fitted values
all.equal(cor_fitted^2, r_squared)
x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
deg_free <- c(3, 5, 9)  # df values
col_ors <- c("black", "red", "blue", "green")
lab_els <- paste0("df1=", deg_free, ", df2=3")
for (in_dex in 1:NROW(deg_free)) {  # plot four curves
curve(expr=df(x, df1=deg_free[in_dex], df2=3),
      type="l", xlim=c(0, 4),
      xlab="", ylab="", lwd=2,
      col=col_ors[in_dex],
      add=as.logical(in_dex-1))
}  # end for
# add title
title(main="F-Distributions", line=0.5)
# add legend
legend("topright", inset=0.05, title="degrees of freedom",
       lab_els, cex=0.8, lwd=2, lty=1,
       col=col_ors)
# F-statistic from lm()
model_sum$fstatistic
# degrees of freedom of residuals
deg_free <- (n_rows - n_cols - 1)
# F-statistic from RSS
f_stat <- e_ss*deg_free/r_ss/n_cols
all.equal(f_stat, model_sum$fstatistic[1], check.attributes=FALSE)
# p-value of F-statistic
1-pf(q=f_stat, df1=n_rows-n_cols-1, df2=n_cols)
library(lmtest)  # load lmtest
de_sign <- data.frame(  # design matrix
  de_sign=1:30, omit_var=sin(0.2*1:30))
# response depends on both predictors
res_ponse <- with(de_sign,
  0.2*de_sign + omit_var + 0.2*rnorm(30))
# mis-specified regression only one predictor
mod_el <- lm(res_ponse ~ de_sign,
        data=de_sign)
model_sum <- summary(mod_el)
model_sum$coeff
model_sum$r.squared
# Durbin-Watson test shows residuals are autocorrelated
dwtest(mod_el)$p.value
par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # set plot panels
plot(for_mula, data=de_sign)
abline(mod_el, lwd=2, col="red")
title(main="OVB Regression", line=-1)
plot(mod_el, which=2, ask=FALSE)  # plot just Q-Q
set.seed(1121)
library(lmtest)
# spurious regression in unit root time series
de_sign <- cumsum(rnorm(100))  # unit root time series
res_ponse <- cumsum(rnorm(100))
for_mula <- res_ponse ~ de_sign
mod_el <- lm(for_mula)  # perform regression
# summary indicates statistically significant regression
model_sum <- summary(mod_el)
model_sum$coeff
model_sum$r.squared
# Durbin-Watson test shows residuals are autocorrelated
dw_test <- dwtest(mod_el)
c(dw_test$statistic[[1]], dw_test$p.value)
par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # set plot panels
plot(for_mula, xlab="", ylab="")  # plot scatterplot using formula
title(main="Spurious Regression", line=-1)
# add regression line
abline(mod_el, lwd=2, col="red")
plot(mod_el, which=2, ask=FALSE)  # plot just Q-Q
library(HighFreq)
# specify formula and perform regression
for_mula <- XLP ~ VTI
mod_el <- lm(for_mula, 
          data=rutils::etf_env$re_turns)
# get regression coefficients
coef(summary(mod_el))
# Durbin-Watson test of autocorrelation of residuals
lmtest::dwtest(mod_el)
# plot scatterplot of returns with aspect ratio 1
plot(for_mula, data=rutils::etf_env$re_turns,
     xlim=c(-0.1, 0.1), ylim=c(-0.1, 0.1),
     asp=1, main="Regression XLP ~ VTI")
# add regression line and perpendicular line
abline(mod_el, lwd=2, col="red")
abline(a=0, b=-1/coef(summary(mod_el))[2, 1],
 lwd=2, col="blue")
library(HighFreq)  # load HighFreq
re_turns <- na.omit(rutils::etf_env$re_turns)
# perform regressions and collect statistics
etf_reg_stats <- sapply(colnames(re_turns)[-1],
                  function(etf_name) {
# specify regression formula
  for_mula <- as.formula(
    paste(etf_name, "~ VTI"))
# perform regression
  mod_el <- lm(for_mula, data=re_turns)
# get regression summary
  model_sum <- summary(mod_el)
# collect regression statistics
  etf_reg_stats <- with(model_sum,
    c(alpha=coefficients[1, 1],
p_alpha=coefficients[1, 4],
beta=coefficients[2, 1],
p_beta=coefficients[2, 4]))
  etf_reg_stats <- c(etf_reg_stats,
         p_dw=lmtest::dwtest(mod_el)$p.value)
  etf_reg_stats
})  # end sapply
etf_reg_stats <- t(etf_reg_stats)
# sort by p_alpha
etf_reg_stats <- etf_reg_stats[
  order(etf_reg_stats[, "p_alpha"]), ]
etf_reg_stats[, 1:3]
library(HighFreq)
# specify regression formula
for_mula <- XLP ~ VTI
# perform rolling beta regressions every month
beta_s <- rollapply(rutils::etf_env$re_turns, width=252,
  FUN=function(de_sign)
  coef(lm(for_mula, data=de_sign))[2],
  by=22, by.column=FALSE, align="right")
beta_s <- na.omit(beta_s)
# plot beta_s in x11() window
x11(width=(wid_th <- 6), height=(hei_ght <- 4))
chart_Series(x=beta_s[, "VTI"],
  name=paste("rolling betas", format(for_mula)))
# perform daily rolling beta regressions in parallel
library(roll)
beta_s <- roll_lm(x=rutils::etf_env$re_turns[, "VTI"],
            y=rutils::etf_env$re_turns[, "XLP"],
            width=252)$coefficients
# compare speed of rollapply() versus roll_lm()
library(microbenchmark)
da_ta <- rutils::etf_env$re_turns["2012", c("VTI", "XLP")]
summary(microbenchmark(
  rollapply=rollapply(da_ta, width=22,
FUN=function(de_sign)
coef(lm(for_mula, data=de_sign))[2],
  by.column=FALSE, align="right"),
  roll_lm=roll_lm(x=da_ta[, "VTI"],
            y=da_ta[, "XLP"],
            width=22)$coefficients,
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
library(PerformanceAnalytics)
CAPM.beta(Ra=re_turns[, "XLP"],
    Rb=re_turns[, "VTI"])
CAPM.beta.bull(Ra=re_turns[, "XLP"],
  Rb=re_turns[, "VTI"])
CAPM.beta.bear(Ra=re_turns[, "XLP"],
  Rb=re_turns[, "VTI"])
CAPM.alpha(Ra=re_turns[, "XLP"],
     Rb=re_turns[, "VTI"])
library(PerformanceAnalytics)
etf_betas <- sapply(
  re_turns[, colnames(re_turns)!="VXX"],
  CAPM.beta, Rb=re_turns[, "VTI"])
etf_annrets <- sapply(
  re_turns[, colnames(re_turns)!="VXX"],
  Return.annualized)
# plot scatterplot
plot(etf_annrets ~ etf_betas, xlab="betas",
      ylab="ann. rets", xlim=c(-0.25, 1.6))
points(x=1, y=etf_annrets["VTI"], col="red",
 lwd=3, pch=21)
abline(a=0, b=etf_annrets["VTI"])
label_names <- rownames(etf_reg_stats)[1:13]
# add labels
text(x=1, y=etf_annrets["VTI"], labels="VTI",
     pos=2)
text(x=etf_betas[label_names],
     y=etf_annrets[label_names],
     labels=label_names, pos=2, cex=0.8)
library(PerformanceAnalytics)
TreynorRatio(Ra=re_turns[, "XLP"],
     Rb=re_turns[, "VTI"])

InformationRatio(Ra=re_turns[, "XLP"],
     Rb=re_turns[, "VTI"])
library(PerformanceAnalytics)
table.CAPM(Ra=re_turns[, c("XLP", "XLF")],
     Rb=re_turns[, "VTI"], scale=252)
library(PerformanceAnalytics)
capm_stats <- PerformanceAnalytics::table.CAPM(Ra=re_turns[, colnames(re_turns)!="VTI"],
        Rb=re_turns[, "VTI"], scale=252)
colnames(capm_stats) <-
  sapply(colnames(capm_stats),
  function (str) {strsplit(str, split=" ")[[1]][1]})
capm_stats <- as.matrix(capm_stats)
capm_stats <- t(capm_stats)
capm_stats <- capm_stats[
  order(capm_stats[, "Annualized Alpha"],
  decreasing=TRUE), ]
# copy capm_stats into etf_env and save to .RData file
assign("capm_stats", capm_stats, envir=etf_env)
save(etf_env, file='etf_data.RData')
capm_stats[, c("Information Ratio", "Annualized Alpha")]
options(width=50, dev='pdf')
str(optimize)
# objective function with multiple minima
object_ive <- function(in_put, param1=0.01) {
  sin(0.25*pi*in_put) + param1*(in_put-1)^2
}  # end object_ive
unlist(optimize(f=object_ive, interval=c(-4, 2)))
unlist(optimize(f=object_ive, interval=c(0, 8)))
options(width=60, dev='pdf')
par(oma=c(1, 1, 1, 1), mgp=c(2, 1, 0), mar=c(5, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# plot the objective function
curve(expr=object_ive, type="l", xlim=c(-8, 9),
xlab="", ylab="", lwd=2)
# add title
title(main="Objective Function", line=-1)
library(rgl)  # load rgl
# define function of two variables
sur_face <- function(x, y) y*sin(x)
# draw 3d surface plot of function
persp3d(x=sur_face, xlim=c(-5, 5), ylim=c(-5, 5),
  col="green", axes=FALSE)
# draw 3d surface plot of matrix
x_lim <- seq(from=-5, to=5, by=0.1)
y_lim <- seq(from=-5, to=5, by=0.1)
persp3d(z=outer(x_lim, y_lim, FUN=sur_face),
  xlab="x", ylab="y", zlab="sur_face",
  col="green")
# save current view to png file
rgl.snapshot("surface_plot.png")
# define function of two variables and two parameters
sur_face <- function(x, y, par_1=1, par_2=1)
  sin(par_1*x)*sin(par_2*y)
# draw 3d surface plot of function
persp3d(x=sur_face, xlim=c(-5, 5), ylim=c(-5, 5),
  col="green", axes=FALSE,
  par_1=1, par_2=2)
# Rastrigin function with vector argument for optimization
rastri_gin <- function(vec_tor, pa_ram=25){
  sum(vec_tor^2 - pa_ram*cos(vec_tor))
}  # end rastri_gin
vec_tor <- c(pi/6, pi/6)
rastri_gin(vec_tor=vec_tor)
# draw 3d surface plot of Rastrigin function
rgl::persp3d(
  x=Vectorize(function(x, y) rastri_gin(vec_tor=c(x, y))),
  xlim=c(-10, 10), ylim=c(-10, 10),
  col="green", axes=FALSE, zlab="", main="rastri_gin")
# optimize with respect to vector argument
op_tim <- optim(par=vec_tor, fn=rastri_gin,
        method="L-BFGS-B",
        upper=c(4*pi, 4*pi),
        lower=c(pi/2, pi/2),
        pa_ram=1)
# optimal parameters and value
op_tim$par
op_tim$value
rastri_gin(op_tim$par, pa_ram=1)
# sample of normal variables
r_norm <- rnorm(1000, mean=4, sd=2)
# objective function is log-likelihood
object_ive <- function(pa_r, r_norm) {
  sum(2*log(pa_r[2]) +
    ((r_norm - pa_r[1])/pa_r[2])^2)
}  # end object_ive
# vectorize objective function
vec_objective <- Vectorize(
  FUN=function(mean, sd, r_norm)
    object_ive(c(mean, sd), r_norm),
  vectorize.args=c("mean", "sd")
)  # end Vectorize
# objective function on parameter grid
par_mean <- seq(1, 6, length=50)
par_sd <- seq(0.5, 3.0, length=50)
objective_grid <- outer(par_mean, par_sd,
vec_objective, r_norm=r_norm)
objective_min <- which(  # grid search
  objective_grid==min(objective_grid),
  arr.ind=TRUE)
objective_min
par_mean[objective_min[1]]  # mean
par_sd[objective_min[2]]  # sd
objective_grid[objective_min]
objective_grid[(objective_min[, 1] + -1:1),
       (objective_min[, 2] + -1:1)]
par(cex.lab=2.0, cex.axis=2.0, cex.main=2.0, cex.sub=2.0)
# perspective plot of log-likelihood function
persp(z=-objective_grid,
theta=45, phi=30, shade=0.5,
border="green", zlab="objective",
main="objective function")
# interactive perspective plot of log-likelihood function
library(rgl)  # load package rgl
par3d(cex=2.0)  # scale text by factor of 2
persp3d(z=-objective_grid, zlab="objective",
  col="green", main="objective function")
# initial parameters
par_init <- c(mean=0, sd=1)
# perform optimization using optim()
optim_fit <- optim(par=par_init,
  fn=object_ive, # log-likelihood function
  r_norm=r_norm,
  method="L-BFGS-B", # quasi-Newton method
  upper=c(10, 10), # upper constraint
  lower=c(-10, 0.1)) # lower constraint
# optimal parameters
optim_fit$par
# perform optimization using MASS::fitdistr()
optim_fit <- MASS::fitdistr(r_norm, densfun="normal")
optim_fit$estimate
optim_fit$sd
# plot histogram
histo_gram <- hist(r_norm, plot=FALSE)
plot(histo_gram, freq=FALSE,
     main="histogram of sample")
curve(expr=dnorm(x, mean=optim_fit$par["mean"],
           sd=optim_fit$par["sd"]),
add=TRUE, type="l", lwd=2, col="red")
legend("topright", inset=0.0, cex=0.8, title=NULL,
 leg="optimal parameters",
 lwd=2, bg="white", col="red")
# sample from mixture of normal distributions
r_norm <- c(rnorm(100, sd=1.0),
      rnorm(100, mean=4, sd=1.0))
# objective function is log-likelihood
object_ive <- function(pa_r, r_norm) {
  likelihood <- pa_r[1]/pa_r[3] *
  dnorm((r_norm-pa_r[2])/pa_r[3]) +
  (1-pa_r[1])/pa_r[5]*dnorm((r_norm-pa_r[4])/pa_r[5])
  if (any(likelihood <= 0)) Inf else
    -sum(log(likelihood))
}  # end object_ive
# vectorize objective function
vec_objective <- Vectorize(
  FUN=function(mean, sd, w, m1, s1, r_norm)
    object_ive(c(w, m1, s1, mean, sd), r_norm),
  vectorize.args=c("mean", "sd")
)  # end Vectorize
# objective function on parameter grid
par_mean <- seq(3, 5, length=50)
par_sd <- seq(0.5, 1.5, length=50)
objective_grid <- outer(par_mean, par_sd,
    vec_objective, r_norm=r_norm,
    w=0.5, m1=2.0, s1=2.0)
rownames(objective_grid) <- round(par_mean, 2)
colnames(objective_grid) <- round(par_sd, 2)
objective_min <- which(objective_grid==
  min(objective_grid), arr.ind=TRUE)
objective_min
objective_grid[objective_min]
objective_grid[(objective_min[, 1] + -1:1),
         (objective_min[, 2] + -1:1)]
# perspective plot of objective function
persp(par_mean, par_sd, -objective_grid,
theta=45, phi=30,
shade=0.5,
col=rainbow(50),
border="green",
main="objective function")
# initial parameters
par_init <- c(weight=0.5, m1=0, s1=1, m2=2, s2=1)
# perform optimization
optim_fit <- optim(par=par_init,
      fn=object_ive,
      r_norm=r_norm,
      method="L-BFGS-B",
      upper=c(1,10,10,10,10),
      lower=c(0,-10,0.2,-10,0.2))
optim_fit$par
# plot histogram
histo_gram <- hist(r_norm, plot=FALSE)
plot(histo_gram, freq=FALSE,
     main="histogram of sample")
fit_func <- function(x, pa_r) {
  pa_r["weight"] *
    dnorm(x, mean=pa_r["m1"], sd=pa_r["s1"]) +
  (1-pa_r["weight"]) *
    dnorm(x, mean=pa_r["m2"], sd=pa_r["s2"])
}  # end fit_func
curve(expr=fit_func(x, pa_r=optim_fit$par), add=TRUE,
type="l", lwd=2, col="red")
legend("topright", inset=0.0, cex=0.8, title=NULL,
 leg="optimal parameters",
 lwd=2, bg="white", col="red")
# Rastrigin function with vector argument for optimization
rastri_gin <- function(vec_tor, pa_ram=25){
  sum(vec_tor^2 - pa_ram*cos(vec_tor))
}  # end rastri_gin
vec_tor <- c(pi/6, pi/6)
rastri_gin(vec_tor=vec_tor)
library(DEoptim)
optimize rastri_gin using DEoptim
op_tim <-  DEoptim(rastri_gin,
  upper=c(6, 6), lower=c(-6, -6),
  DEoptim.control(trace=FALSE, itermax=50))
# optimal parameters and value
op_tim$optim$bestmem
rastri_gin(op_tim$optim$bestmem)
summary(op_tim)
plot(op_tim)
library(HighFreq)
# Select ETF symbols
sym_bols <- c("IEF", "DBC", "XLU", "XLF", "XLP", "XLI")
# calculate ETF prices and simple returns (not percentage)
price_s <- rutils::etf_env$price_s[, sym_bols]
price_s <- xts:::na.locf.xts(price_s)
price_s <- xts:::na.locf.xts(price_s, fromLast=TRUE)
date_s <- index(price_s)
re_turns <- rutils::diff_it(price_s)
# de-mean (center) and scale the returns
re_turns <- t(t(re_turns) - colMeans(re_turns))
re_turns <- t(t(re_turns) / sqrt(colSums(re_turns^2)/(NROW(re_turns)-1)))
re_turns <- xts(re_turns, date_s)
# alternative de-mean (center) and scale the returns
# re_turns <- scale(re_turns, center=TRUE, scale=TRUE)
# re_turns <- xts(re_turns, date_s)
# or
# re_turns <- lapply(re_turns, function(x) {x - sum(x)/NROW(re_turns)})
# re_turns <- rutils::do_call(cbind, re_turns)
# re_turns <- apply(re_turns, 2, scale)
# covariance matrix and variance vector of returns
cov_mat <- cov(re_turns)
vari_ance <- diag(cov_mat)
cor_mat <- cor(re_turns)
# cov_mat <- crossprod(re_turns) / (NROW(re_turns)-1)
# cor_mat <- cov_mat / sqrt(vari_ance)
# cor_mat <- t(t(cor_mat) / sqrt(vari_ance))
# reorder correlation matrix based on clusters
library(corrplot)
or_der <- corrMatOrder(cor_mat,
        order="hclust",
        hclust.method="complete")
cor_mat <- cor_mat[or_der, or_der]
# plot the correlation matrix
col_ors <- colorRampPalette(c("red", "white", "blue"))
corrplot(cor_mat, title="ETF Correlation Matrix",
    tl.col="black", tl.cex=0.8, mar=c(0,0,1,0),
    method="square", col=col_ors(8),
    cl.offset=0.75, cl.cex=0.7,
    cl.align.text="l", cl.ratio=0.25)
# draw rectangles on the correlation matrix plot
corrRect.hclust(cor_mat, k=NROW(cor_mat) %/% 2,
          method="complete", col="red")
# plot the correlation matrix
col_ors <- colorRampPalette(c("red", "white", "blue"))
corrplot(cor_mat, title="Correlation Matrix",
    tl.col="black", tl.cex=0.8, mar = c(0,0,1,0),
    method="square", col=col_ors(NCOL(cor_mat)),
    cl.offset=0.75, cl.cex=0.7,
    cl.align.text="l", cl.ratio=0.25)
# draw rectangles on the correlation matrix plot
corrRect.hclust(cor_mat, k=NCOL(cor_mat) %/% 2,
    method="complete", col="red")
# create initial vector of portfolio weights
n_weights <- NROW(sym_bols)
weight_s <- rep(1/sqrt(n_weights), n_weights)
names(weight_s) <- sym_bols
# objective function equal to minus portfolio variance
object_ive <- function(weight_s, re_turns) {
  portf_rets <- re_turns %*% weight_s
  -sum(portf_rets^2) +
    1e7*(1 - sum(weight_s^2))^2
}  # end object_ive
# objective for equal weight portfolio
object_ive(weight_s, re_turns)
# compare speed of vector multiplication methods
summary(microbenchmark(
  trans_pose=(t(re_turns[, 1]) %*% re_turns[, 1]),
  s_um=sum(re_turns[, 1]^2),
  times=10))[, c(1, 4, 5)]
# find weights with maximum variance
optim_run <- optim(par=weight_s,
  fn=object_ive,
  re_turns=re_turns,
  method="L-BFGS-B",
  upper=rep(10.0, n_weights),
  lower=rep(-10.0, n_weights))
# optimal weights and maximum variance
weight_s <- optim_run$par
-object_ive(weight_s, re_turns)
# plot first principal component weights
barplot(weight_s, names.arg=names(weight_s),
  xlab="", ylab="",
  main="First Principal Component Weights")
# pc1 weights and returns
weights_1 <- weight_s
pc_1 <- re_turns %*% weights_1
# redefine objective function
object_ive <- function(weight_s, re_turns) {
  portf_rets <- re_turns %*% weight_s
  -sum(portf_rets^2) +
    1e7*(1 - sum(weight_s^2))^2 +
    1e7*(sum(weights_1*weight_s))^2
}  # end object_ive
# find second PC weights using parallel DEoptim
optim_run <- DEoptim::DEoptim(fn=object_ive,
  upper=rep(10, NCOL(re_turns)),
  lower=rep(-10, NCOL(re_turns)),
  re_turns=re_turns, control=list(parVar="weights_1",
    trace=FALSE, itermax=1000, parallelType=1))
# pc2 weights and returns
weights_2 <- optim_run$optim$bestmem
names(weights_2) <- colnames(re_turns)
sum(weights_2^2)
sum(weights_1*weights_2)
# plot second principal component loadings
barplot(weights_2, names.arg=names(weights_2),
  xlab="", ylab="",
  main="Second Principal Component Loadings")
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
# redefine objective function to minimize variance
object_ive <- function(weight_s, re_turns) {
  portf_rets <- re_turns %*% weight_s
  sum(portf_rets^2) +
    1e7*(1 - sum(weight_s^2))^2
}  # end object_ive
# find highest order PC weights using parallel DEoptim
optim_run <- DEoptim::DEoptim(fn=object_ive,
  upper=rep(10, NCOL(re_turns)),
  lower=rep(-10, NCOL(re_turns)),
  re_turns=re_turns, control=list(trace=FALSE,
    itermax=1000, parallelType=1))
# pc6 weights and returns
weights_6 <- optim_run$optim$bestmem
names(weights_6) <- colnames(re_turns)
sum(weights_6^2)
sum(weights_1*weights_6)
# calculate objective function
object_ive(weights_6, re_turns)
object_ive(ei_gen$vectors[, 6], re_turns)
# plot highest order principal component loadings
barplot(weights_6, names.arg=names(weights_2),
  xlab="", ylab="",
  main="Highest Order Principal Component Loadings")
# perform principal component analysis PCA
pc_a <- prcomp(re_turns, scale=TRUE)
# plot standard deviations of principal components
barplot(pc_a$sdev,
  names.arg=colnames(pc_a$rotation),
  las=3, xlab="", ylab="",
  main="Scree Plot: Volatilities of Principal Components
  of Stock Returns")
# principal component loadings (weights)
pc_a$rotation
# Plot barplots with PCA weights in multiple panels
par(mfrow=c(n_weights/2, 2))
par(mar=c(2, 2, 2, 1), oma=c(0, 0, 0, 0))
for (or_der in 1:n_weights) {
  barplot(pc_a$rotation[, or_der],
  las=3, xlab="", ylab="", main="")
  title(paste0("PC", or_der), line=-2.0,
  col.main="red")
}  # end for
# principal component time series
pca_rets <- xts(re_turns %*% pc_a$rotation,
          order.by=date_s)
round(cov(pca_rets), 3)
all.equal(coredata(pca_rets), pc_a$x, check.attributes=FALSE)
pca_ts <- xts:::cumsum.xts(pca_rets)
# plot principal component time series in multiple panels
par(mfrow=c(n_weights/2, 2))
par(mar=c(2, 2, 0, 1), oma=c(0, 0, 0, 0))
ra_nge <- range(pca_ts)
for (or_der in 1:n_weights) {
  plot.zoo(pca_ts[, or_der],
     ylim=ra_nge,
     xlab="", ylab="")
  title(paste0("PC", or_der), line=-2.0)
}  # end for
par(mfrow=c(n_weights/2, 2))
par(mar=c(2, 2, 0, 1), oma=c(0, 0, 0, 0))
# invert all the principal component time series
pca_rets <- re_turns %*% pc_a$rotation
sol_ved <- pca_rets %*% solve(pc_a$rotation)
all.equal(re_turns, sol_ved)
# invert first 3 principal component time series
sol_ved <- pca_rets[, 1:3] %*% solve(pc_a$rotation)[1:3, ]
sol_ved <- xts::xts(sol_ved, date_s)
sol_ved <- xts:::cumsum.xts(sol_ved)
cum_returns <- xts:::cumsum.xts(re_turns)
# plot the solved returns
for (sym_bol in sym_bols) {
  plot.zoo(
    cbind(cum_returns[, sym_bol], sol_ved[, sym_bol]),
    plot.type="single", col=c("black", "blue"), xlab="", ylab="")
  legend(x="topleft", bty="n",
   legend=paste0(sym_bol, c("", " solved")),
   title=NULL, inset=0.0, cex=1.0, lwd=6,
   lty=1, col=c("black", "blue"))
}  # end for
# eigen decomposition of covariance matrix
re_turns <- rutils::diff_it(price_s)
cov_mat <- cov(re_turns)
ei_gen <- eigen(cov_mat)
# perform PCA without scaling
pc_a <- prcomp(re_turns, scale=FALSE)
# compare outputs
all.equal(ei_gen$values, pc_a$sdev^2)
all.equal(abs(ei_gen$vectors), abs(pc_a$rotation),
    check.attributes=FALSE)
# eigen decomposition of correlation matrix
cor_mat <- cor(re_turns)
ei_gen <- eigen(cor_mat)
# perform PCA with scaling
pc_a <- prcomp(re_turns, scale=TRUE)
# compare outputs
all.equal(ei_gen$values, pc_a$sdev^2)
all.equal(abs(ei_gen$vectors), abs(pc_a$rotation),
    check.attributes=FALSE)
# load S&P500 constituent stock prices
load("C:/Develop/R/lecture_slides/data/sp500_prices.RData")
date_s <- index(price_s)
# calculate simple returns (not percentage)
re_turns <- rutils::diff_it(price_s)
# de-mean (center) and scale the returns
re_turns <- t(t(re_turns) - colMeans(re_turns))
re_turns <- t(t(re_turns) / sqrt(colSums(re_turns^2)/(NROW(re_turns)-1)))
re_turns <- xts(re_turns, date_s)
# perform principal component analysis PCA
pc_a <- prcomp(re_turns, scale=TRUE)
# find number of components with variance greater than 2
n_comp <- which(pc_a$sdev^2 < 2)[1]
# plot standard deviations of principal components
barplot(pc_a$sdev[1:n_comp],
  names.arg=colnames(pc_a$rotation[, 1:n_comp]),
  las=3, xlab="", ylab="",
  main="Volatilities of S&P500 Principal Components")
# Principal component loadings (weights)
# Plot barplots with PCA weights in multiple panels
n_comps <- 6
par(mfrow=c(n_comps/2, 2))
par(mar=c(4, 2, 2, 1), oma=c(0, 0, 0, 0))
# First principal component weights
weight_s <- sort(pc_a$rotation[, 1], decreasing=TRUE)
barplot(weight_s[1:6],
  las=3, xlab="", ylab="", main="")
title(paste0("PC", 1), line=-2.0,
col.main="red")
for (or_der in 2:n_comps) {
  weight_s <- sort(pc_a$rotation[, or_der], decreasing=TRUE)
  barplot(weight_s[c(1:3, 498:500)],
  las=3, xlab="", ylab="", main="")
  title(paste0("PC", or_der), line=-2.0,
  col.main="red")
}  # end for
# principal component time series
pca_rets <- xts(re_turns %*% pc_a$rotation[, 1:n_comps],
          order.by=date_s)
round(cov(pca_rets), 3)
pca_ts <- xts:::cumsum.xts(pca_rets)
# plot principal component time series in multiple panels
par(mfrow=c(n_comps/2, 2))
par(mar=c(2, 2, 0, 1), oma=c(0, 0, 0, 0))
ra_nge <- range(pca_ts)
for (or_der in 1:n_comps) {
  plot.zoo(pca_ts[, or_der],
     ylim=ra_nge,
     xlab="", ylab="")
  title(paste0("PC", or_der), line=-2.0)
}  # end for
par(mfrow=c(n_comps/2, 2))
par(mar=c(2, 2, 0, 1), oma=c(0, 0, 0, 0))
# invert principal component time series
inv_rotation <- solve(pc_a$rotation)
all.equal(inv_rotation, t(pc_a$rotation))
sol_ved <- pca_rets %*% inv_rotation[1:n_comps, ]
sol_ved <- xts::xts(sol_ved, date_s)
sol_ved <- xts:::cumsum.xts(sol_ved)
cum_returns <- xts:::cumsum.xts(re_turns)
# plot the solved returns
sym_bols <- c("MSFT", "XOM", "JPM", "AAPL", "BRK_B", "JNJ")
for (sym_bol in sym_bols) {
  plot.zoo(
    cbind(cum_returns[, sym_bol], sol_ved[, sym_bol]),
    plot.type="single", col=c("black", "blue"), xlab="", ylab="")
  legend(x="topleft", bty="n",
   legend=paste0(sym_bol, c("", " solved")),
   title=NULL, inset=0.05, cex=1.0, lwd=6,
   lty=1, col=c("black", "blue"))
}  # end for
par(mfrow=c(n_comps/2, 2))
par(mar=c(2, 2, 0, 1), oma=c(0, 0, 0, 0))
# perform ADF unit-root tests on original series and residuals
sapply(sym_bols, function(sym_bol) {
  c(series=tseries::adf.test(cum_returns[, sym_bol])$p.value,
    resid=tseries::adf.test(cum_returns[, sym_bol] - sol_ved[, sym_bol])$p.value)
})  # end sapply
# plot the residuals
for (sym_bol in sym_bols) {
  plot.zoo(cum_returns[, sym_bol] - sol_ved[, sym_bol],
    plot.type="single", col="blue", xlab="", ylab="")
  legend(x="topleft", bty="n",
   legend=paste0(sym_bol, " residuals"),
   title=NULL, inset=0.05, cex=1.0, lwd=6,
   lty=1, col="blue")
}  # end for
# perform ADF unit-root test on principal component time series
pca_rets <- xts(re_turns %*% pc_a$rotation,
          order.by=date_s)
pca_ts <- xts:::cumsum.xts(pca_rets)
adf_pvalues <- sapply(1:NCOL(pca_ts), function(or_der)
  tseries::adf.test(pca_ts[, or_der])$p.value)
# ADF unit-root test on stationary time series
tseries::adf.test(rnorm(1e5))
library(quantmod)
#perform pair-wise correlation analysis
# calculate correlation matrix
cor_mat <- cor(re_turns)
colnames(cor_mat) <- colnames(re_turns)
rownames(cor_mat) <- colnames(re_turns)
# reorder correlation matrix based on clusters
# calculate permutation vector
library(corrplot)
or_der <- corrMatOrder(cor_mat,
        order="hclust",
        hclust.method="complete")
# apply permutation vector
cor_mat <- cor_mat[or_der, or_der]
# plot the correlation matrix
col_ors <- colorRampPalette(c("red", "white", "blue"))
corrplot(cor_mat,
    tl.col="black", tl.cex=0.8,
    method="square", col=col_ors(8),
    cl.offset=0.75, cl.cex=0.7,
    cl.align.text="l", cl.ratio=0.25)
# draw rectangles on the correlation matrix plot
corrRect.hclust(cor_mat, k=NROW(cor_mat) %/% 2,
          method="complete", col="red")
# convert correlation matrix into distance object
dis_tance <- as.dist(1-cor_mat)
# perform hierarchical clustering analysis
clus_ter <- hclust(dis_tance)
plot(clus_ter, ann=FALSE, xlab="", ylab="")
title("Dendrogram representing hierarchical clustering
\nwith dissimilarity = 1-correlation", line=-0.5)
library(quantmod)
library(Rglpk)
# vector of symbol names
sym_bols <- c("VTI", "IEF", "DBC")
n_weights <- NROW(sym_bols)
# Calculate mean returns
re_turns <- rutils::etf_env$re_turns[, sym_bols]
re_turns <- xts:::na.locf.xts(re_turns)
re_turns <- na.omit(re_turns)
mean_rets <- colMeans(re_turns)
# Specify weight constraints
constraint_s <- matrix(c(rep(1, n_weights),
                 1, 1, 0),
                 nc=n_weights, byrow=TRUE)
direction_s <- c("==", "<=")
rh_s <- c(1, 0)
# Specify weight bounds (-1, 1) (default is c(0, Inf))
bound_s <-
  list(lower=list(ind=1:n_weights, val=rep(-1, n_weights)),
 upper=list(ind=1:n_weights, val=rep(1, n_weights)))
# perform optimization
op_tim <- Rglpk::Rglpk_solve_LP(
  obj=mean_rets,
  mat=constraint_s,
  dir=direction_s,
  rhs=rh_s,
  bounds=bound_s,
  max=TRUE)
unlist(op_tim[1:2])
# Calculate covariance matrix of returns and its inverse
cov_mat <- cov(re_turns)
cov_inv <- solve(a=cov_mat)
u_nit <- rep(1, NCOL(cov_mat))
# minimum variance weights with constraint
# weight_s <- solve(a=cov_mat, b=u_nit)
weight_s <- cov_inv %*% u_nit
weight_s <- weight_s / drop(t(u_nit) %*% weight_s)
# minimum variance
t(weight_s) %*% cov_mat %*% weight_s
1/(t(u_nit) %*% cov_inv %*% u_nit)
# Calculate vector of mean returns
mean_rets <- colMeans(re_turns)
# Specify the target return
tar_get <- 1.5*mean(re_turns)
# products of inverse with mean returns and unit vector
f_mat <- matrix(c(
  t(u_nit) %*% cov_inv %*% u_nit,
  t(u_nit) %*% cov_inv %*% mean_rets,
  t(mean_rets) %*% cov_inv %*% u_nit,
  t(mean_rets) %*% cov_inv %*% mean_rets), nc=2)
# Solve for the Lagrange multipliers
multipli_ers <-
  solve(a=f_mat, b=c(2, 2*tar_get))
# Calculate weights
weight_s <- drop(0.5*cov_inv %*%
  cbind(u_nit, mean_rets) %*% multipli_ers)
# Calculate constraints
all.equal(1, sum(weight_s))
all.equal(tar_get, sum(mean_rets*weight_s))
# Calculate portfolio return and standard deviation
portf_rets <- drop(re_turns %*% weight_s)
c(return=mean(portf_rets), sd=sd(portf_rets))
all.equal(mean(portf_rets), tar_get)
# Calculate portfolio variance
uu <- c(1, tar_get)
f_inv <- solve(f_mat)
all.equal(var(portf_rets), drop(t(uu) %*% f_inv %*% uu))
# Calculate vertex of variance parabola
weight_s <- drop(cov_inv %*% u_nit /
  drop(t(u_nit) %*% cov_inv %*% u_nit))
portf_rets <- drop(re_turns %*% weight_s)
v_rets <-
  drop(t(u_nit) %*% cov_inv %*% mean_rets /
  t(u_nit) %*% cov_inv %*% u_nit)
all.equal(mean(portf_rets), v_rets)
var_min <-
  drop(1/t(u_nit) %*% cov_inv %*% u_nit)
all.equal(var(portf_rets), var_min)
# Calculate efficient frontier
target_s <- v_rets*(1+seq(from=-1, to=1, by=0.1))
eff_front <- sapply(target_s, function(tar_get) {
  uu <- c(1, tar_get)
  sqrt(drop(t(uu) %*% f_inv %*% uu))
})  # end sapply
# Plot efficient frontier
x11(width=6, height=5)
plot(x=eff_front, y=target_s, t="l", col="blue", lwd=2,
     main="Efficient Frontier and Minimum Variance Portfolio",
     xlab="standard deviation", ylab="return")
points(x=sqrt(var_min), y=v_rets, col="green", lwd=6)
text(x=sqrt(var_min), y=v_rets, labels="minimum \nvariance",
     pos=4, cex=0.8)
# Calculate portfolio standard deviation
std_dev <- sqrt(drop(t(uu) %*% f_inv %*% uu))
# Calculate the slope of the tangent line
slop_e <- (std_dev*det(f_mat))/(f_mat[1, 1]*tar_get-f_mat[1, 2])
# Calculate the risk-free rate as intercept of the tangent line
risk_free <- tar_get - slop_e*std_dev
# Calculate the risk-free rate from target return
risk_free <- (tar_get*f_mat[1, 2]-f_mat[2, 2]) /
  (tar_get*f_mat[1, 1]-f_mat[1, 2])
# Plot efficient frontier
plot(x=eff_front, y=target_s, t="l", col="blue", lwd=2,
     xlim=c(0.0, max(eff_front)),
     main="Efficient Frontier and Tangency Portfolio",
     xlab="standard deviation", ylab="return")
# Plot minimum variance
points(x=sqrt(var_min), y=v_rets, col="green", lwd=6)
text(x=sqrt(var_min), y=v_rets, labels="minimum \nvariance",
     pos=4, cex=0.8)
# Plot tangent point
points(x=std_dev, y=tar_get, col="red", lwd=6)
text(x=std_dev, y=tar_get, labels="tangency\nportfolio", pos=2, cex=0.8)
# Plot risk-free point
points(x=0, y=risk_free, col="red", lwd=6)
text(x=0, y=risk_free, labels="risk-free", pos=4, cex=0.8)
# Plot tangent line
abline(a=risk_free, b=slop_e, lwd=2, col="green")
# Calculate excess re_turns
risk_free <- 0.03/252
ex_cess <- re_turns - risk_free
# Calculate covariance and inverse matrix
cov_mat <- cov(re_turns)
u_nit <- rep(1, NCOL(cov_mat))
cov_inv <- solve(a=cov_mat)
# Calculate mean excess returns
ex_cess <- sapply(ex_cess, mean)
# weights of maximum Sharpe portfolio
# weight_s <- solve(a=cov_mat, b=re_turns)
weight_s <- cov_inv %*% ex_cess
weight_s <- weight_s/drop(t(u_nit) %*% weight_s)
# Sharpe ratios
sqrt(252)*sum(weight_s * ex_cess) /
  sqrt(drop(weight_s %*% cov_mat %*% weight_s))
sapply(re_turns - risk_free,
  function(x) sqrt(252)*mean(x)/sd(x))
weights_maxsharpe <- weight_s
library(quantmod)
# Calculate minimum variance weights
weight_s <- cov_inv %*% u_nit
weights_minvar <-
  weight_s / drop(t(u_nit) %*% weight_s)
# Calculate optimal portfolio returns
optim_rets <- xts(
  x=cbind(exp(cumsum(re_turns %*% weights_maxsharpe)),
    exp(cumsum(re_turns %*% weights_minvar))),
  order.by=index(re_turns))
colnames(optim_rets) <- c("maxsharpe", "minvar")
# Plot optimal portfolio returns, with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "green")
x11(width=6, height=5)
chart_Series(optim_rets, theme=plot_theme,
       name="Maximum Sharpe and \nMinimum Variance portfolios")
legend("top", legend=colnames(optim_rets), cex=0.8,
 inset=0.1, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")
x11(wid_th <- 6, hei_ght <- 6)
# Calculate minimum variance weights
weight_s <- cov_inv %*% u_nit
weight_s <- weight_s / drop(t(u_nit) %*% weight_s)
# minimum standard deviation and return
std_dev <- sqrt(252*drop(weight_s %*% cov_mat %*% weight_s))
min_ret <- 252*sum(weight_s * mean_rets)
# Calculate maximum Sharpe portfolios
risk_free <- (min_ret * seq(-10, 10, by=0.1)^3)/252
eff_front <- sapply(risk_free, function(risk_free) {
  weight_s <- cov_inv %*% (mean_rets - risk_free)
  weight_s <- weight_s/drop(t(u_nit) %*% weight_s)
  # portfolio return and standard deviation
  c(return=252*sum(weight_s * mean_rets),
    stddev=sqrt(252*drop(weight_s %*% cov_mat %*% weight_s)))
})  # end sapply
eff_front <- cbind(252*risk_free, t(eff_front))
colnames(eff_front)[1] <- "risk-free"
eff_front <- eff_front[is.finite(eff_front[, "stddev"]), ]
eff_front <- eff_front[order(eff_front[, "return"]), ]
# Plot maximum Sharpe portfolios
plot(x=eff_front[, "stddev"],
     y=eff_front[, "return"], t="l",
     xlim=c(0.0*std_dev, 3.0*std_dev),
     ylim=c(0.0*min_ret, 2.0*min_ret),
     main="Efficient Frontier and Capital Market Line",
     xlab="standard deviation", ylab="return")
points(x=eff_front[, "stddev"], y=eff_front[, "return"],
 col="red", lwd=3)
# Plot minimum variance portfolio
points(x=std_dev, y=min_ret, col="green", lwd=6)
text(std_dev, min_ret, labels="minimum \nvariance",
     pos=4, cex=0.8)
# Draw Capital Market Line
sor_ted <- sort(eff_front[, 1])
risk_free <-
  sor_ted[findInterval(x=0.5*min_ret, vec=sor_ted)]
points(x=0, y=risk_free, col="blue", lwd=6)
text(x=0, y=risk_free, labels="risk-free",
     pos=4, cex=0.8)
in_dex <- match(risk_free, eff_front[, 1])
points(x=eff_front[in_dex, "stddev"],
 y=eff_front[in_dex, "return"],
 col="blue", lwd=6)
text(x=eff_front[in_dex, "stddev"],
     y=eff_front[in_dex, "return"],
     labels="market portfolio",
     pos=2, cex=0.8)
sharp_e <- (eff_front[in_dex, "return"]-risk_free)/
  eff_front[in_dex, "stddev"]
abline(a=risk_free, b=sharp_e, col="blue", lwd=2)
text(x=0.7*eff_front[in_dex, "stddev"],
     y=0.7*eff_front[in_dex, "return"]+0.01,
     labels="Capital Market Line", pos=2, cex=0.8,
     srt=45*atan(sharp_e*hei_ght/wid_th)/(0.25*pi))
# Calculate random portfolios
n_portf <- 1000
ret_sd <- sapply(1:n_portf, function(in_dex) {
  weight_s <- runif(n_weights-1, min=-0.25, max=1.0)
  weight_s <- c(weight_s, 1-sum(weight_s))
  # portfolio return and standard deviation
  c(return=252*sum(weight_s * mean_rets),
    stddev=sqrt(252*drop(weight_s %*% cov_mat %*% weight_s)))
})  # end sapply
# Plot scatterplot of random portfolios
x11(wid_th <- 6, hei_ght <- 6)
plot(x=ret_sd["stddev", ], y=ret_sd["return", ],
     main="Efficient Frontier and Random Portfolios",
     xlim=c(0.5*std_dev, 0.8*max(ret_sd["stddev", ])),
     xlab="standard deviation", ylab="return")
# Plot maximum Sharpe portfolios
lines(x=eff_front[, "stddev"],
     y=eff_front[, "return"], lwd=2)
points(x=eff_front[, "stddev"], y=eff_front[, "return"],
 col="red", lwd=3)
# Plot minimum variance portfolio
points(x=std_dev, y=min_ret, col="green", lwd=6)
text(std_dev, min_ret, labels="minimum\nvariance",
     pos=2, cex=0.8)
# Plot market portfolio
points(x=eff_front[in_dex, "stddev"],
 y=eff_front[in_dex, "return"], col="green", lwd=6)
text(x=eff_front[in_dex, "stddev"],
     y=eff_front[in_dex, "return"],
     labels="market\nportfolio",
     pos=2, cex=0.8)
# Plot individual assets
points(x=sqrt(252*diag(cov_mat)),
 y=252*mean_rets, col="blue", lwd=6)
text(x=sqrt(252*diag(cov_mat)), y=252*mean_rets,
     labels=names(mean_rets),
     col="blue", pos=1, cex=0.8)
risk_free <- 0.03
re_turns <- c(asset1=0.05, asset2=0.06)
std_devs <- c(asset1=0.4, asset2=0.5)
cor_rel <- 0.6
cov_mat <- matrix(c(1, cor_rel, cor_rel, 1), nc=2)
cov_mat <- t(t(std_devs*cov_mat)*std_devs)
weight_s <- seq(from=-1, to=2, length.out=31)
weight_s <- cbind(weight_s, 1-weight_s)
portf_rets <- weight_s %*% re_turns
portf_sd <-
  sqrt(rowSums(weight_s * (weight_s %*% cov_mat)))
sharpe_ratios <- (portf_rets-risk_free)/portf_sd
in_dex <- which.max(sharpe_ratios)
max_Sharpe <- max(sharpe_ratios)
# Plot efficient frontier
x11(wid_th <- 6, hei_ght <- 5)
par(mar=c(3,3,2,1)+0.1, oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
plot(portf_sd, portf_rets, t="l",
 main=paste0("Efficient frontier and CML for two assets\ncorrelation = ", 100*cor_rel, "%"),
 xlab="standard deviation", ylab="return",
 lwd=2, col="orange",
 xlim=c(0, max(portf_sd)),
 ylim=c(0.02, max(portf_rets)))
# Add Market Portfolio (maximum Sharpe ratio portfolio)
points(portf_sd[in_dex], portf_rets[in_dex],
 col="blue", lwd=3)
text(x=portf_sd[in_dex], y=portf_rets[in_dex],
     labels=paste(c("market portfolio\n",
 structure(c(weight_s[in_dex], 1-weight_s[in_dex]),
         names=names(re_turns))), collapse=" "),
     pos=2, cex=0.8)
# Plot individual assets
points(std_devs, re_turns, col="green", lwd=3)
text(std_devs, re_turns, labels=names(re_turns), pos=4, cex=0.8)
# Add point at risk-free rate and draw Capital Market Line
points(x=0, y=risk_free, col="blue", lwd=3)
text(0, risk_free, labels="risk-free\nrate", pos=4, cex=0.8)
abline(a=risk_free, b=max_Sharpe, lwd=2, col="blue")
range_s <- par("usr")
text(portf_sd[in_dex]/2, (portf_rets[in_dex]+risk_free)/2,
     labels="Capital Market Line", cex=0.8, , pos=3,
     srt=45*atan(max_Sharpe*(range_s[2]-range_s[1])/
             (range_s[4]-range_s[3])*
             hei_ght/wid_th)/(0.25*pi))
# Plot portfolios in x11() window
x11(wid_th <- 6, hei_ght <- 5)
par(oma=c(0, 0, 0, 0), mar=c(3,3,2,1)+0.1, mgp=c(2, 1, 0), cex.lab=1.0, cex.axis=1.0, cex.main=1.0, cex.sub=1.0)
# vector of symbol names
sym_bols <- c("VTI", "IEF")
# matrix of portfolio weights
weight_s <- seq(from=-1, to=2, length.out=31)
weight_s <- cbind(weight_s, 1-weight_s)
# Calculate portfolio returns and volatilities
re_turns <- rutils::etf_env$re_turns[, sym_bols]
ret_sd <- re_turns %*% t(weight_s)
ret_sd <- cbind(252*colMeans(ret_sd),
  sqrt(252)*matrixStats::colSds(ret_sd))
colnames(ret_sd) <- c("returns", "stddev")
risk_free <- 0.06
ret_sd <- cbind(ret_sd,
  (ret_sd[, "returns"]-risk_free)/ret_sd[, "stddev"])
colnames(ret_sd)[3] <- "Sharpe"
in_dex <- which.max(ret_sd[, "Sharpe"])
max_Sharpe <- ret_sd[in_dex, "Sharpe"]
plot(x=ret_sd[, "stddev"], y=ret_sd[, "returns"],
     main="Stock and Bond portfolios", t="l",
     xlim=c(0, 0.7*max(ret_sd[, "stddev"])), ylim=c(0, max(ret_sd[, "returns"])),
     xlab="standard deviation", ylab="return")
# Add blue point for market portfolio
points(x=ret_sd[in_dex, "stddev"], y=ret_sd[in_dex, "returns"], col="blue", lwd=6)
text(x=ret_sd[in_dex, "stddev"], y=ret_sd[in_dex, "returns"],
     labels=paste(c("market portfolio\n", structure(c(weight_s[in_dex, 1], weight_s[in_dex, 2]), names=sym_bols)), collapse=" "),
     pos=3, cex=0.8)
# Plot individual assets
mean_rets <- 252*sapply(re_turns, mean)
std_devs <- sqrt(252)*sapply(re_turns, sd)
points(std_devs, mean_rets, col="green", lwd=6)
text(std_devs, mean_rets, labels=names(re_turns), pos=2, cex=0.8)
# Add point at risk-free rate and draw Capital Market Line
points(x=0, y=risk_free, col="blue", lwd=6)
text(0, risk_free, labels="risk-free", pos=4, cex=0.8)
abline(a=risk_free, b=max_Sharpe, col="blue", lwd=2)
range_s <- par("usr")
text(max(ret_sd[, "stddev"])/3, 0.75*max(ret_sd[, "returns"]),
     labels="Capital Market Line", cex=0.8, , pos=3,
     srt=45*atan(max_Sharpe*(range_s[2]-range_s[1])/
             (range_s[4]-range_s[3])*
             hei_ght/wid_th)/(0.25*pi))
# Plot portfolios in x11() window
x11(wid_th <- 6, hei_ght <- 5)
# Calculate cumulative returns of VTI and IEF
optim_rets <- lapply(re_turns,
  function(re_turns) exp(cumsum(re_turns)))
optim_rets <- rutils::do_call(cbind, optim_rets)
# Calculate market portfolio returns
optim_rets <- cbind(
  exp(cumsum(re_turns %*%
    c(weight_s[in_dex], 1-weight_s[in_dex]))),
  optim_rets)
colnames(optim_rets)[1] <- "market"
# Plot market portfolio with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue", "green")
chart_Series(optim_rets, theme=plot_theme,
       name="Market portfolio for stocks and bonds")
legend("top", legend=colnames(optim_rets),
 cex=0.8, inset=0.1, bg="white", lty=1,
 lwd=6, col=plot_theme$col$line.col, bty="n")
x11(width=6, height=4)
par(mar=c(3, 2, 1, 0), oma=c(0, 0, 0, 0))
# VTI percentage returns
re_turns <- rutils::diff_it(log(Ad(rutils::etf_env$VTI)))
conf_level <- 0.1
va_r <- quantile(re_turns, conf_level)
c_var <- mean(re_turns[re_turns < va_r])
# or
sort_ed <- sort(as.numeric(re_turns))
in_dex <- round(conf_level*NROW(re_turns))
va_r <- sort_ed[in_dex]
c_var <- mean(sort_ed[1:in_dex])
# Plot histogram of VTI returns
histo_gram <- hist(re_turns, col="lightgrey",
  xlab="returns", breaks=100, xlim=c(-0.05, 0.01),
  ylab="frequency", freq=FALSE,
  main="VTI returns histogram")
dens_ity <- density(re_turns, adjust=1.5)
lines(dens_ity, lwd=3, col="blue")
# Add line for VaR
abline(v=va_r, col="red", lwd=3)
text(x=va_r, y=20, labels="VaR",
     lwd=2, srt=90, pos=2)
# Add shading for CVaR
var_max <- -0.06
rang_e <- (dens_ity$x < va_r) & (dens_ity$x > var_max)
polygon(
  c(var_max, dens_ity$x[rang_e], va_r),
  c(0, dens_ity$y[rang_e], 0),
  col=rgb(1, 0, 0,0.5), border=NA)
text(x=va_r, y=3, labels="CVaR", lwd=2, pos=2)
library(HighFreq)
library(Rglpk)
# vector of symbol names and returns
sym_bols <- c("VTI", "IEF", "DBC")
n_weights <- NROW(sym_bols)
re_turns <- rutils::etf_env$re_turns[((NROW(re_turns)-6):NROW(re_turns)), sym_bols]
mean_rets <- colMeans(re_turns)
conf_level <- 0.05
r_min <- 0 ; w_min <- 0 ; w_max <- 1
weight_sum <- 1
n_cols <- NCOL(re_turns) # number of assets
n_rows <- NROW(re_turns) # number of rows
# Creat objective vector
obj_vector <- c(numeric(n_cols), rep(-1/(conf_level*n_rows), n_rows), -1)
# Specify weight constraints
constraint_s <- rbind(
  cbind(rbind(1, mean_rets),
  matrix(data=0, nrow=2, ncol=(n_rows+1))),
  cbind(coredata(re_turns), diag(n_rows), 1))
rh_s <- c(weight_sum, r_min, rep(0, n_rows))
direction_s <- c("==", ">=", rep(">=", n_rows))
# Specify weight bounds
bound_s <- list(
  lower=list(ind=1:n_cols, val=rep(w_min, n_cols)),
  upper=list(ind=1:n_cols, val=rep(w_max, n_cols)))
# perform optimization
op_tim <- Rglpk_solve_LP(obj=obj_vector, mat=constraint_s, dir=direction_s, rhs=rh_s, types=rep("C", NROW(obj_vector)), max=T, bounds=bound_s)
op_tim$solution
constraint_s %*% op_tim$solution
obj_vector %*% op_tim$solution
as.numeric(op_tim$solution[1:n_cols])
# Calculate daily percentage re_turns
sym_bols <- c("VTI", "IEF", "DBC")
re_turns <- rutils::etf_env$re_turns[, sym_bols]
# Create initial vector of portfolio weights
weight_s <- rep(1, NROW(sym_bols))
names(weight_s) <- sym_bols
# objective equal to minus Sharpe ratio
object_ive <- function(weight_s, re_turns) {
  portf_rets <- re_turns %*% weight_s
  if (sd(portf_rets) == 0)
    return(0)
  else
    return(-mean(portf_rets)/sd(portf_rets))
}  # end object_ive
# objective for equal weight portfolio
object_ive(weight_s, re_turns=re_turns)
op_tim <- unlist(optimize(
  f=function(weight)
    object_ive(c(1, 1, weight), re_turns=re_turns),
  interval=c(-4, 1)))
# vectorize objective function with respect to third weight
vec_object <- function(weights) sapply(weights,
  function(weight) object_ive(c(1, 1, weight),
    re_turns=re_turns))
# or
vec_object <- Vectorize(FUN=function(weight)
    object_ive(c(1, 1, weight), re_turns=re_turns),
  vectorize.args="weight")  # end Vectorize
vec_object(1)
vec_object(1:3)
x11(width=6, height=5)
par(oma=c(1, 1, 1, 1), mgp=c(2, 1, 0), mar=c(3, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# Plot objective function with respect to third weight
curve(expr=vec_object,
      type="l", xlim=c(-4.0, 1.0),
      xlab=paste("weight of", names(weight_s[3])),
      ylab="", lwd=2)
title(main="Objective Function", line=-1)  # Add title
points(x=op_tim[1], y=op_tim[2], col="green", lwd=6)
text(x=op_tim[1], y=op_tim[2],
     labels="minimum objective", pos=4, cex=0.8)

#below is simplified code for plotting objective function
# Create vector of DBC weights
weight_s <- seq(from=-4, to=1, by=0.1)
obj_val <- sapply(weight_s,
  function(weight) object_ive(c(1, 1, weight)))
plot(x=weight_s, y=obj_val, t="l",
      xlab="weight of DBC", ylab="", lwd=2)
title(main="Objective Function", line=-1)  # Add title
points(x=op_tim[1], y=op_tim[2], col="green", lwd=6)
text(x=op_tim[1], y=op_tim[2],
     labels="minimum objective", pos=4, cex=0.8)
# vectorize function with respect to all weights
vec_object <- Vectorize(
  FUN=function(w1, w2, w3)
    object_ive(c(w1, w2, w3)),
  vectorize.args=c("w2", "w3"))  # end Vectorize
# Calculate objective on 2-d (w2 x w3) parameter grid
w2 <- seq(-3, 7, length=50)
w3 <- seq(-5, 5, length=50)
grid_object <- outer(w2, w3, FUN=vec_object, w1=1)
rownames(grid_object) <- round(w2, 2)
colnames(grid_object) <- round(w3, 2)
# perspective plot of objective function
persp(w2, w3, -grid_object,
theta=45, phi=30, shade=0.5,
col=rainbow(50), border="green",
main="objective function")
# interactive perspective plot of objective function
library(rgl)
rgl::persp3d(z=-grid_object, zlab="objective",
  col="green", main="objective function")
rgl::persp3d(
  x=function(w2, w3)
    -vec_object(w1=1, w2, w3),
  xlim=c(-3, 7), ylim=c(-5, 5),
  col="green", axes=FALSE)
# optimization to find weights with maximum Sharpe ratio
op_tim <- optim(par=weight_s,
             fn=object_ive,
             method="L-BFGS-B",
             upper=c(1.1, 10, 10),
             lower=c(0.9, -10, -10))
# optimal parameters
op_tim$par
op_tim$par <- op_tim$par/sum(op_tim$par)
# optimal Sharpe ratio
-object_ive(op_tim$par)
x11(width=6, height=5)
par(oma=c(1, 1, 1, 0), mgp=c(2, 1, 0), mar=c(2, 1, 2, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# Plot in two vertical panels
layout(matrix(c(1,2), 2),
 widths=c(1,1), heights=c(1,3))
# barplot of optimal portfolio weights
barplot(op_tim$par, col=c("red", "green", "blue"),
  main="Optimized portfolio weights")
# Calculate cumulative returns of VTI, IEF, DBC
cum_rets <- lapply(re_turns,
  function(re_turns) exp(cumsum(re_turns)))
cum_rets <- rutils::do_call(cbind, cum_rets)
# Calculate optimal portfolio returns with VTI, IEF, DBC
optim_rets <- cbind(
  exp(cumsum(re_turns %*% op_tim$par)),
  cum_rets)
colnames(optim_rets)[1] <- "optim_rets"
# Plot optimal returns with VTI, IEF, DBC
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("black", "red", "green", "blue")
chart_Series(optim_rets, theme=plot_theme,
       name="Optimized portfolio performance")
legend("top", legend=colnames(optim_rets), cex=0.8,
 inset=0.1, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")
# or plot non-compounded (simple) cumulative returns
PerformanceAnalytics::chart.CumReturns(
  cbind(re_turns %*% op_tim$par, re_turns),
  lwd=2, ylab="", legend.loc="topleft", main="")
risk_free <- 0.03
re_turns <- c(asset1=0.05, asset2=0.06)
std_devs <- c(asset1=0.4, asset2=0.5)
cor_rel <- 0.6
cov_mat <- matrix(c(1, cor_rel, cor_rel, 1), nc=2)
cov_mat <- t(t(std_devs*cov_mat)*std_devs)
library(quadprog)
# minimum variance weights without constraints
op_tim <- solve.QP(Dmat=2*cov_mat,
            dvec=rep(0, 2),
            Amat=matrix(0, nr=2, nc=1),
            bvec=0)
# minimum variance weights sum equal to 1
op_tim <- solve.QP(Dmat=2*cov_mat,
            dvec=rep(0, 2),
            Amat=matrix(1, nr=2, nc=1),
            bvec=1)
# optimal value of objective function
t(op_tim$solution) %*% cov_mat %*% op_tim$solution
perform simple optimization for reference
# objective function for simple optimization
object_ive <- function(x) {
  x <- c(x, 1-x)
  t(x) %*% cov_mat %*% x
}  # end object_ive
unlist(optimize(f=object_ive, interval=c(-1, 2)))
# Calculate daily percentage re_turns
sym_bols <- c("VTI", "IEF", "DBC")
re_turns <- rutils::etf_env$re_turns[, sym_bols]
# Calculate the covariance matrix
cov_mat <- cov(re_turns)
# minimum variance weights, with sum equal to 1
op_tim <- quadprog::solve.QP(Dmat=2*cov_mat,
            dvec=numeric(3),
            Amat=matrix(1, nr=3, nc=1),
            bvec=1)
# minimum variance, maximum returns
op_tim <- quadprog::solve.QP(Dmat=2*cov_mat,
            dvec=apply(0.1*re_turns, 2, mean),
            Amat=matrix(1, nr=3, nc=1),
            bvec=1)
# minimum variance positive weights, sum equal to 1
a_mat <- cbind(matrix(1, nr=3, nc=1),
       diag(3), -diag(3))
b_vec <- c(1, rep(0, 3), rep(-1, 3))
op_tim <- quadprog::solve.QP(Dmat=2*cov_mat,
            dvec=numeric(3),
            Amat=a_mat,
            bvec=b_vec,
            meq=1)
# Calculate daily percentage re_turns
re_turns <- rutils::etf_env$re_turns[, sym_bols]
# objective equal to minus Sharpe ratio
object_ive <- function(weight_s, re_turns) {
  portf_rets <- re_turns %*% weight_s
  if (sd(portf_rets) == 0)
    return(0)
  else
    return(-mean(portf_rets)/sd(portf_rets))
}  # end object_ive
# perform optimization using DEoptim
op_tim <- DEoptim::DEoptim(fn=object_ive,
  upper=rep(10, NCOL(re_turns)),
  lower=rep(-10, NCOL(re_turns)),
  re_turns=re_turns,
  control=list(trace=FALSE, itermax=100, parallelType=1))
weight_s <- op_tim$optim$bestmem/sum(abs(op_tim$optim$bestmem))
names(weight_s) <- colnames(re_turns)
# objective with shrinkage penalty
object_ive <- function(weight_s, re_turns, lamb_da, al_pha) {
  portf_rets <- re_turns %*% weight_s
  if (sd(portf_rets) == 0)
    return(0)
  else {
    penal_ty <- lamb_da*((1-al_pha)*sum(weight_s^2) +
al_pha*sum(abs(weight_s)))
    return(-mean(portf_rets)/sd(portf_rets) + penal_ty)
  }
}  # end object_ive
# objective for equal weight portfolio
weight_s <- rep(1, NROW(sym_bols))
names(weight_s) <- sym_bols
lamb_da <- 0.5 ; al_pha <- 0.5
object_ive(weight_s, re_turns=re_turns,
  lamb_da=lamb_da, al_pha=al_pha)
# perform optimization using DEoptim
op_tim <- DEoptim::DEoptim(fn=object_ive,
  upper=rep(10, NCOL(re_turns)),
  lower=rep(-10, NCOL(re_turns)),
  re_turns=re_turns,
  lamb_da=lamb_da,
  al_pha=al_pha,
  control=list(trace=FALSE, itermax=100, parallelType=1))
weight_s <-
  op_tim$optim$bestmem/sum(abs(op_tim$optim$bestmem))
names(weight_s) <- colnames(re_turns)
