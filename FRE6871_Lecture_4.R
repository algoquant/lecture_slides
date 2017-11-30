library(knitr)
opts_chunk$set(prompt=TRUE, eval=FALSE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
options(digits=3)
thm <- knit_theme$get("acid")
knit_theme$set(thm)
# calculate random default probabilities
num_assets <- 100
default_probs <- runif(num_assets, max=0.05)
# calculate number of defaults
uni_form <- runif(num_assets)
sum(uni_form < default_probs)
# simulate average number of defaults
num_simu <- 1000
de_faults <- numeric(num_simu)
for (i in 1:num_simu) {  # perform loop
  uni_form <- runif(num_assets)
  de_faults[i] <- sum(uni_form < default_probs)
}  # end for
mean(de_faults)
# average defaults using vectorized functions
uni_form <- matrix(runif(num_simu*num_assets),
             ncol=num_simu)
sum(uni_form < default_probs)/num_simu
# plot Standard Normal distribution
x11(width=6, height=5)
curve(expr=dnorm(x),
type="l", xlim=c(-4, 4),
xlab="asset value", ylab="", lwd=2,
col="blue", main="Distribution of Asset Values")
abline(v=qnorm(0.025), col="red", lwd=2)
text(x=qnorm(0.025)-0.1, y=0.15,
 labels="default threshold",
 lwd=2, srt=90, pos=3)
# define correlation parameters
rh_o <- 0.05
rho_sqrt <- sqrt(rh_o)
rho_sqrtm <- sqrt(1-rh_o)
# calculate default thresholds
default_thresh <- qnorm(default_probs)
# calculate vector of systematic factors
system_atic <- rnorm(num_simu)
# allocate vector of defaults
de_faults <- numeric(num_simu)
# perform loop to calculate de_faults
for (i in 1:num_simu) {
  asset_values <-
    rho_sqrt*system_atic[i] +
    rho_sqrtm*rnorm(num_assets)
  de_faults[i] <-
    sum(asset_values < default_thresh)
}  # end for
# define default probability density function
vasi_cek <- function(x, def_thresh=-2, rh_o=0.08)
  sqrt((1-rh_o)/rh_o)*exp(-(sqrt(1-rh_o)*qnorm(x) - def_thresh)^2/(2*rh_o) + qnorm(x)^2/2)
vasi_cek(0.03, def_thresh=qnorm(0.025), rh_o=0.1)
# plot probability distribution of defaults
curve(expr=vasi_cek(x, def_thresh=qnorm(0.025), rh_o=0.02),
type="l", xlim=c(0, 0.1), lwd=3,
xlab="fraction of defaults", ylab="density",
col="green", main="Distribution of defaults")
# plot default distribution with higher correlation
curve(expr=vasi_cek(x, def_thresh=qnorm(0.025), rh_o=0.08),
type="l", xlim=c(0, 0.1), add=TRUE,
xlab="default fraction", ylab="", lwd=3,
col="blue", main="")
# add legend
legend(x="topright", legend=c("high correlation", "low correlation"),
 title=NULL, inset=0.05, cex=0.8, bg="white",
 lwd=6, lty=c(1, 1), col=c("blue", "green"))
# add unconditional default probability
abline(v=0.025, col="red", lwd=3)
text(x=0.023, y=8,
 labels="default probability",
 lwd=2, srt=90, pos=3)
# plot default distribution with low correlation
curve(expr=vasi_cek(x, def_thresh=qnorm(0.1), rh_o=0.01),
type="l", xlab="default fraction", ylab="",
lwd=2, col="green", main="Distribution of defaults")
# plot default distribution with high correlation
curve(expr=vasi_cek(x, def_thresh=qnorm(0.1), rh_o=0.99),
type="l", add=TRUE, lwd=2, n=10001,
xlab="fraction of defaults", ylab="density",
col="blue", main="")
# add legend
legend(x="topright", legend=c("high correlation", "low correlation"),
 title=NULL, inset=0.1, cex=0.8, bg="white",
 lwd=6, lty=c(1, 1), col=c("blue", "green"))
# add unconditional default probability
abline(v=0.1, col="red", lwd=2)
text(x=0.1, y=10,
 labels="default probability",
 lwd=2, pos=4)
# define Vasicek loss distribution density function
portf_loss <- function(x, def_thresh=-2, rh_o=0.08, l_gd=0.4)
  sqrt((1-rh_o)/rh_o)*exp(-(sqrt(1-rh_o)*qnorm(x/l_gd) - def_thresh)^2/(2*rh_o) + qnorm(x/l_gd)^2/2)/l_gd
integrate(portf_loss, low=0, up=0.3, def_thresh=-2, rh_o=0.08, l_gd=0.4)
# plot probability distribution of losses
curve(expr=portf_loss(x, def_thresh=qnorm(0.05), rh_o=0.08),
type="l", xlim=c(0, 0.06),
xlab="loss fraction", ylab="density", lwd=3,
col="orange", main="Distribution of Losses")
# add line for expected loss
abline(v=0.02, col="red", lwd=3)
text(x=0.02-0.001, y=10, labels="expected loss",
 lwd=2, srt=90, pos=3)
# add lines for unexpected loss
abline(v=0.04, col="blue", lwd=3)
arrows(x0=0.02, y0=35, x1=0.04, y1=35,
 code=3, lwd=3, cex=0.5)
text(x=0.03, y=36, labels="unexpected loss",
     lwd=2, pos=3)
# add lines for VaR
abline(v=0.055, col="red", lwd=3)
arrows(x0=0.0, y0=25, x1=0.055, y1=25,
 code=3, lwd=3, cex=0.5)
text(x=0.03, y=26, labels="VaR", lwd=2, pos=3)
text(x=0.055-0.001, y=10, labels="VaR",
 lwd=2, srt=90, pos=3)
# plot probability distribution of losses
curve(expr=portf_loss(x, def_thresh=qnorm(0.05), rh_o=0.08),
type="l", xlim=c(0, 0.06),
xlab="loss fraction", ylab="density", lwd=3,
col="orange", main="Conditional Value at Risk")
# add line for expected loss
abline(v=0.02, col="red", lwd=3)
text(x=0.02-0.001, y=10, labels="expected loss",
 lwd=2, srt=90, pos=3)
# add lines for VaR
abline(v=0.04, col="red", lwd=3)
text(x=0.04-0.001, y=10, labels="VaR",
 lwd=2, srt=90, pos=3)
# add shading for CVaR
v_ar <- 0.04; var_max <- 0.07
var_s <- seq(v_ar, var_max, length=100)
dens_ity <- sapply(var_s,
  portf_loss, def_thresh=qnorm(0.05), rh_o=0.08)
# draw shaded polygon
polygon(c(v_ar, var_s, var_max),
  c(-1, dens_ity, -1), col="red", border=NA)
text(x=0.045, y=0, labels="CVaR", lwd=2, pos=3)
# integrate portf_loss() over full range
integrate(portf_loss, low=0.0, up=0.3,
    def_thresh=qnorm(0.05), rh_o=0.08, l_gd=0.4)
# calculate expected losses using portf_loss()
integrate(function(x, ...) x*portf_loss(x, ...),
    low=0.0, up=0.3,
    def_thresh=qnorm(0.05), rh_o=0.08, l_gd=0.4)
# calculate confidence levels corresponding to VaR values
var_s <- seq(0.04, 0.06, 0.001)
conf_levels <- sapply(var_s, function(v_ar, ...) {
  integrate(portf_loss, low=v_ar, up=0.3, ...)
}, def_thresh=qnorm(0.05), rh_o=0.08, l_gd=0.4)  # end sapply
conf_levels <- cbind(as.numeric(t(conf_levels)[, 1]), var_s)
colnames(conf_levels) <- c("conf_levels", "VaRs")
# calculate 95% confidence level VaR value
conf_levels[
  match(TRUE, conf_levels[, "conf_levels"] < 0.05),
  "VaRs"]
plot(x=1-conf_levels[, "conf_levels"],
     y=conf_levels[, "VaRs"],
     xlab="conf_levels", ylab="VaRs",
     t="l", main="VaR values and confidence levels")
# calculate CVaR values
cvar_s <- sapply(var_s, function(v_ar, ...) {
  integrate(function(x, ...) x*portf_loss(x, ...),
      low=v_ar, up=0.3, ...)
}, def_thresh=qnorm(0.05), rh_o=0.08, l_gd=0.4)  # end sapply
conf_levels <- cbind(conf_levels, as.numeric(t(cvar_s)[, 1]))
colnames(conf_levels)[3] <- "CVaRs"
# divide CVaR by confidence level
conf_levels[, "CVaRs"] <- conf_levels[, "CVaRs"]/conf_levels[, "conf_levels"]
# calculate 95% confidence level CVaR value
conf_levels[
  match(TRUE, conf_levels[, "conf_levels"] < 0.05),
  "CVaRs"]
# plot CVaRs
plot(x=1-conf_levels[, "conf_levels"],
     y=conf_levels[, "CVaRs"],
     t="l", col="red", lwd=2,
     ylim=range(conf_levels[, c("VaRs", "CVaRs")]),
     xlab="conf_levels", ylab="CVaRs",
     main="CVaR values and confidence levels")
# add VaRs
lines(x=1-conf_levels[, "conf_levels"],
y=conf_levels[, "VaRs"], lwd=2)
# add legend
legend(x="topleft", legend=c("CVaRs", "VaRs"),
 title=NULL, inset=0.05, cex=0.8, bg="white",
 lwd=6, lty=c(1, 1), col=c("red", "black"))
# Define model parameters
num_assets <- 300
num_simu <- 1000
l_gd <- 0.4
# define correlation parameters
rh_o <- 0.08
rho_sqrt <- sqrt(rh_o)
rho_sqrtm <- sqrt(1-rh_o)
# calculate default probabilities and thresholds
set.seed(1121)
default_probs <- runif(num_assets, max=0.1)
default_thresh <- qnorm(default_probs)
# calculate vector of systematic factors
system_atic <- rnorm(num_simu)
# simulate losses under Vasicek model
asset_values <- matrix(rnorm(num_simu*num_assets), ncol=num_simu)
asset_values <- t(rho_sqrt*system_atic + t(rho_sqrtm*asset_values))
loss_es <-
  l_gd*colSums(asset_values < default_thresh)/num_assets
# calculate VaRs
conf_levels <- seq(0.93, 0.99, 0.01)
var_s <- quantile(loss_es, probs=conf_levels)
names(var_s) <- round(conf_levels, 3)
plot(x=conf_levels, y=var_s, t="l",
     main="Simulated VaR and confidence levels")
# calculate frequency table of loss_es
table_losses <- table(loss_es)/num_simu
# calculate CVaRs
cvar_s <- sapply(var_s, function(v_ar) {
  tai_l <- table_losses[v_ar < names(table_losses)]
  tai_l %*% as.numeric(names(tai_l)) / sum(tai_l)
})  # end sapply
cvar_s <- cbind(cvar_s, var_s)
# plot CVaRs
plot(x=rownames(cvar_s), y=cvar_s[, "cvar_s"],
     t="l", col="red", lwd=2,
     ylim=range(cvar_s),
     xlab="conf_levels", ylab="CVaRs",
     main="Simulated CVaR and confidence levels")
# add VaRs
lines(x=rownames(cvar_s), y=cvar_s[, "var_s"], lwd=2)
# add legend
legend(x="topleft", legend=c("CVaRs", "VaRs"),
 title=NULL, inset=0.05, cex=0.8, bg="white",
 lwd=6, lty=c(1, 1), col=c("red", "black"))
calc_var <- function(default_probs,
               l_gd=0.6,
               rh_o=0.08,
               num_simu=1000,
               conf_levels=seq(0.93, 0.99, 0.01)) {
  # Define model parameters
  num_assets <- NROW(default_probs)
  default_thresh <- qnorm(default_probs)
  # Define correlation parameters
  rho_sqrt <- sqrt(rh_o)
  rho_sqrtm <- sqrt(1-rh_o)
  # Simulate losses under Vasicek model
  system_atic <- rnorm(num_simu)
  asset_values <- matrix(rnorm(num_simu*num_assets), ncol=num_simu)
  asset_values <- t(rho_sqrt*system_atic + t(rho_sqrtm*asset_values))
  loss_es <- l_gd*colSums(asset_values < default_thresh)/num_assets
  # Calculate VaRs
  var_s <- quantile(loss_es, probs=conf_levels)
  names(var_s) <- round(conf_levels, 3)
  # Calculate CVaRs from the frequency table
  table_losses <- table(loss_es)/num_simu
  cvar_s <- sapply(var_s, function(v_ar) {
    tai_l <- table_losses[v_ar < names(table_losses)]
    tai_l %*% as.numeric(names(tai_l)) / sum(tai_l)
  })  # end sapply
  names(cvar_s) <- round(conf_levels, 3)
  c(var_s, cvar_s)
}  # end calc_var
# define number of bootstrap simulations
num_boot <- 500
num_assets <- NROW(default_probs)
# perform bootstrap of calc_var
set.seed(1121)
boot_strap <- sapply(rep(l_gd, num_boot),
  calc_var,
  default_probs=
    default_probs[sample.int(num_assets, replace=TRUE)],
  rh_o=rh_o, num_simu=num_simu,
  conf_levels=conf_levels)  # end sapply
boot_strap <- t(boot_strap)
# calculate vectors of standard errors of VaR and CVaR from boot_strap data
std_error_var <- apply(boot_strap[, 1:7], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
std_error_cvar <- apply(boot_strap[, 8:14], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
# scale the standard errors of VaRs and CVaRs
std_error_var[2, ] <- std_error_var[2, ]/std_error_var[1, ]
std_error_cvar[2, ] <- std_error_cvar[2, ]/std_error_cvar[1, ]
# plot the standard errors of VaRs and CVaRs
plot(x=colnames(std_error_cvar),
  y=std_error_cvar[2, ], t="l", col="red", lwd=2,
  ylim=range(c(std_error_var[2, ], std_error_cvar[2, ])),
  xlab="conf_levels", ylab="CVaRs",
  main="Scaled standard errors of CVaR and VaR")
lines(x=colnames(std_error_var), y=std_error_var[2, ], lwd=2)
legend(x="topleft", legend=c("CVaRs", "VaRs"),
 title=NULL, inset=0.05, cex=0.8, bg="white",
 lwd=6, lty=c(1, 1), col=c("red", "black"))
library(parallel)  # load package parallel
num_cores <- detectCores() - 1  # number of cores
clus_ter <- makeCluster(num_cores)  # initialize compute cluster
# perform bootstrap of calc_var for Windows
set.seed(1121)
boot_strap <- parLapply(clus_ter, rep(l_gd, num_boot),
  fun=calc_var,
  default_probs=default_probs[sample.int(num_assets, replace=TRUE)],
  rh_o=rh_o, num_simu=num_simu,
  conf_levels=conf_levels)  # end parLapply
# bootstrap under Mac-OSX or Linux
boot_strap <- mclapply(rep(l_gd, num_boot),
  fun=calc_var,
  default_probs=default_probs[sample.int(num_assets, replace=TRUE)],
  rh_o=rh_o, num_simu=num_simu,
  conf_levels=conf_levels)  # end mclapply
boot_strap <- do.call(rbind, boot_strap)
stopCluster(clus_ter)  # stop R processes over cluster
# calculate vectors of standard errors of VaR and CVaR from boot_strap data
std_error_var <- apply(boot_strap[, 1:7], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
std_error_cvar <- apply(boot_strap[, 8:14], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
# scale the standard errors of VaRs and CVaRs
std_error_var[2, ] <- std_error_var[2, ]/std_error_var[1, ]
std_error_cvar[2, ] <- std_error_cvar[2, ]/std_error_cvar[1, ]
# plot the standard errors of VaRs and CVaRs
plot(x=colnames(std_error_cvar),
  y=std_error_cvar[2, ], t="l", col="red", lwd=2,
  ylim=range(c(std_error_var[2, ], std_error_cvar[2, ])),
  xlab="conf_levels", ylab="CVaRs",
  main="Scaled standard errors of CVaR and VaR")
lines(x=colnames(std_error_var), y=std_error_var[2, ], lwd=2)
legend(x="topleft", legend=c("CVaRs", "VaRs"),
 title=NULL, inset=0.05, cex=0.8, bg="white",
 lwd=6, lty=c(1, 1), col=c("red", "black"))
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
# t-test for single sample
t.test(rnorm(100))
# t-test for two samples
t.test(rnorm(100),
       rnorm(100, mean=1))
# Wilcoxon test for normal distribution
wilcox.test(rnorm(100))
# Wilcoxon test for two normal distributions
wilcox.test(rnorm(100), rnorm(100, mean=0.1))
# Wilcoxon test for two normal distributions
wilcox.test(rnorm(100), rnorm(100, mean=1.0))
# Wilcoxon test for a uniform versus normal distribution
wilcox.test(runif(100), rnorm(100))
# KS test for normal distribution
ks.test(rnorm(100), pnorm)
# KS test for uniform distribution
ks.test(runif(100), pnorm)
# KS test for two similar normal distributions
ks.test(rnorm(100), rnorm(100, mean=0.1))
# KS test for two different normal distributions
ks.test(rnorm(100), rnorm(100, mean=1.0))
# calculate DAX percentage returns
dax_rets <- diff(log(EuStockMarkets[, 1]))

# Shapiro-Wilk test for normal distribution
shapiro.test(rnorm(NROW(dax_rets)))

# Shapiro-Wilk test for DAX returns
shapiro.test(dax_rets)

# Shapiro-Wilk test for uniform distribution
shapiro.test(runif(NROW(dax_rets)))
dax_rets <- diff(log(EuStockMarkets[, 1]))
library(tseries)  # load package tseries

# Jarque-Bera test for normal distribution
jarque.bera.test(rnorm(NROW(dax_rets)))

# Jarque-Bera test for DAX returns
jarque.bera.test(dax_rets)

# Jarque-Bera test for uniform distribution
jarque.bera.test(runif(NROW(dax_rets)))
# formula of linear model with zero intercept
lin_formula <- z ~ x + y - 1
lin_formula

# collapse vector of strings into single text string
paste0("x", 1:5)
paste(paste0("x", 1:5), collapse="+")

# create formula from text string
lin_formula <- as.formula(
  # coerce text strings to formula
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
len_gth <- 100
explana_tory <- rnorm(len_gth, mean=2)
noise <- rnorm(len_gth)
# response equals linear form plus error terms
res_ponse <- -3 + explana_tory + noise
# solve regression
be_ta <- (sum(res_ponse*explana_tory) - sum(res_ponse)*sum(explana_tory)/len_gth) / (sum(explana_tory^2) - sum(explana_tory)^2/len_gth)
al_pha <- mean(res_ponse) - be_ta * mean(explana_tory)
# specify regression formula
reg_formula <- res_ponse ~ explana_tory
reg_model <- lm(reg_formula)  # perform regression
class(reg_model)  # regressions have class lm
attributes(reg_model)
eval(reg_model$call$formula)  # regression formula
reg_model$coefficients  # regression coefficients
coef(reg_model)
# specify regression formula
reg_formula <- res_ponse ~ explana_tory
reg_model <- lm(reg_formula)  # perform regression
class(reg_model)  # regressions have class lm
attributes(reg_model)
eval(reg_model$call$formula)  # regression formula
reg_model$coefficients  # regression coefficients
coef(reg_model)
x11(width=6, height=5)  # open x11 for plotting
# set plot parameters to reduce whitespace around plot
par(mar=c(5, 5, 1, 1), oma=c(0, 0, 0, 0))
# plot scatterplot using formula
plot(reg_formula)
title(main="Simple Regression", line=-1)
# add regression line
abline(reg_model, lwd=2, col="red")
# plot fitted (predicted) response values
points(x=explana_tory, y=reg_model$fitted.values,
       pch=16, col="blue")
# sum of residuals = 0
sum(reg_model$residuals)
x11(width=6, height=5)  # open x11 for plotting
# set plot parameters to reduce whitespace around plot
par(mar=c(5, 5, 1, 1), oma=c(0, 0, 0, 0))
# extract residuals
resi_duals <- cbind(explana_tory, reg_model$residuals)
colnames(resi_duals) <- c("explanatory variable", "residuals")
# plot residuals
plot(resi_duals)
title(main="Residuals of the Linear Regression", line=-1)
abline(h=0, lwd=2, col="red")
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
res_ponse <- 3 + explana_tory + rnorm(30, sd=8)
reg_model <- lm(reg_formula)  # perform regression
# estimate of regression coefficient is not
# statistically significant
summary(reg_model)
par(oma=c(1, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=1.0, cex.axis=1.0, cex.main=1.0, cex.sub=1.0)
reg_stats <- function(std_dev) {  # noisy regression
  set.seed(1121)  # initialize number generator
# create explanatory and response variables
  explana_tory <- rnorm(100, mean=2)
  res_ponse <- 3 + 0.2*explana_tory +
    rnorm(NROW(explana_tory), sd=std_dev)
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
    explana_tory <- rnorm(100, mean=2)
    res_ponse <- 3 + 0.2*explana_tory +
rnorm(NROW(explana_tory), sd=std_dev)
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
de_sign <- data.frame(  # design matrix
  explana_tory=1:30, omit_var=sin(0.2*1:30))
# response depends on both explanatory variables
res_ponse <- with(de_sign,
  0.2*explana_tory + omit_var + 0.2*rnorm(30))
# mis-specified regression only one explanatory
reg_model <- lm(res_ponse ~ explana_tory,
        data=de_sign)
reg_model_sum <- summary(reg_model)
reg_model_sum$coefficients
reg_model_sum$r.squared
# Durbin-Watson test shows residuals are autocorrelated
dwtest(reg_model)$p.value
par(oma=c(15, 1, 1, 1), mgp=c(0, 0.5, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # set plot panels
plot(reg_formula, data=de_sign)
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
