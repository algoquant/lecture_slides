

###

16*sd(rutils::env_etf$re_turns[, "VTI"])
sqrt(250)
250/5

# Summary: Create a functional which aggregates 
# asset returns over look-back and look-forward 
# intervals.


# define functional

# 1. (20pts) Create a functional called roll_agg(), 

# should perform only a single 


#########

# 4. (20pts) Create a scatterplot of returns and forward returns 
# Create a scatterplot of alphas for "2008" and "2009",
# and add labels with ETF names,
# use columns of "alphas_capm" and functions plot() and text(),

dim(fwd_rets)
dim(cum_rets)

foo <- na.omit(merge(fwd_rets[, 5], cum_rets[, 5]))
colnames(foo) <- c("forward_returns", "past_returns")
foo <- as.data.frame(foo)
head(foo)
dim(foo)

x11()
# perform regression
reg_formula <- paste(colnames(foo), collapse=" ~ ")
reg_model <- lm(reg_formula, data=foo)
summary(reg_model)
# plot scatterplot using formula
plot(foo[, 2], foo[, 1], xlab="past returns", ylab="forward returns")
# plot(foo)
title(main="Simple Regression", line=-1)
# add regression line
abline(reg_model, lwd=2, col="red")


# select weight_s proportional to cum_rets
dim(cum_rets)
weight_s <- coredata(cum_rets[index(fwd_rets)])
weight_s <- weight_s/sqrt(rowSums(weight_s^2))

# bar <- matrixStats::rowMaxs(weight_s)
bar <- coredata(fwd_rets)
dim(bar)

# select best and worst models in each period
bes_t <- apply(weight_s, 1, which.max)
wors_t <- apply(weight_s, 1, function(x) {which.min(x)})
bes_t <- apply(weight_s, 1, which.max)
wors_t <- apply(weight_s, 1, which.min)

back_test <- rowSums(weight_s * bar)
x11()
plot(cumsum(back_test), t="l")

# back_test <- t(weight_s) %*% bar
back_test <- rowSums(weight_s * bar)
back_test <- xts(back_test, order.by=index(fwd_rets))
x11()
chart_Series(x=cumsum(back_test), name="Back-test of EWMA strategies")

plot(cumsum(back_test), t="l")
length(back_test)


#########

# define look-back intervals


# Create a functional for performing rolling 
# aggregations over overlapping intervals. 
# Apply the functional to roll the function simu_ewma() 
# over overlapping 12-month intervals in the past. 

# 1. (20pts) Create a functional called roll_agg(), 
# which should accept four arguments:
#  x_ts - an xts series containing one or more columns of data,
#  end_points - integer vector of end points, 
#  look_back - number of intervals in the lookback window,
#  FUN - name of of an aggregation function,
#  "..." - optional dots arguments to FUN. 

# The functional roll_agg() should perform an lapply() 
# loop over end_points, subset the x_ts series, and pass 
# it to FUN, together with the dots "..." argument. 
# roll_agg() should return an xts series, with each 
# row equal to the vector returned by FUN.
# hint: You can adapt code from the slide: 
# Performing Aggregations Over Overlapping Intervals.

roll_agg <- function(x_ts, end_points, look_back, FUN, ...) {
  len_gth <- length(end_points)
  # start_points are multi-period lag of end_points
  start_points <-  end_points[c(rep_len(1, look_back-1), 1:(len_gth-look_back+1))]
  # perform lapply() loop over length of end_points
  agg_s <- lapply(2:len_gth, 
                  function(in_dex) {
                    FUN(x_ts[start_points[in_dex]:end_points[in_dex]], ...)
                  })  # end lapply
  # rbind list into single xts or matrix
  agg_s <- rutils::do_call_rbind(agg_s)
  if (!is.xts(agg_s))
    agg_s <- xts(agg_s, order.by=index(x_ts[end_points]))
  agg_s
}  # end roll_agg


# 2. (20pts) Create an aggregation function called 
# agg_regate(), which calls the function simu_ewma() 
# and calculates the Sharpe ratios of the EWMA strategy, 
# for a given vector of lambdas.
# agg_regate() should accept three arguments:
#  oh_lc - an OHLC series containing four columns of data,
#  lamb_das - integer vector of lambda parameters, 
#  "..." - additional dots arguments to be passed to simu_ewma(). 
# hint: You can adapt code from the slide: 
# Simulating Multiple EWMA Strategies

agg_regate <- function(oh_lc, lamb_das, ...) {
  sapply(lamb_das, function(lamb_da) {
    # Simulate EWMA strategy and calculate Sharpe ratio
    re_turns <- simu_ewma(oh_lc=oh_lc, lamb_da=lamb_da, ...)[, "re_turns"]
    sqrt(260)*sum(re_turns)/sd(re_turns)/NROW(re_turns)
  })  # end sapply
}  # end agg_regate

# Source the function simu_ewma() from the file 
# ewma_model.R, using function source().
# Download the latest version from NYU Classes.

source("C:/Develop/R/scripts/ewma_model.R")

# Define oh_lc series, the EWMA win_dow, and lamb_das.

library(HighFreq)
oh_lc <- rutils::env_etf$VTI["/2011"]
win_dow <- 51
lamb_das <- seq(0.001, 0.01, 0.001)

# Call agg_regate() as follows:
agg_regate(oh_lc, lamb_das, win_dow=win_dow)

# You should get the following output:
#  [1] 0.1220623 0.1620571 0.1887122 0.2399056 0.2308350 0.1594881 0.1702486 0.1539695 0.1136539
# [10] 0.1180002


# 3. (20pts) Apply the functional roll_agg() to roll 
# the function simu_ewma() over overlapping 12-month 
# intervals in the past. 

# Define end points at the end of each month.
# Use function endpoints() from package xts.

end_points <- xts::endpoints(oh_lc, on="months")
len_gth <- length(end_points)

# Define number of monthly intervals per look-back interval:
look_back <- 12

# Note that there are two different windows in this simulation.
# The first window is the EWMA window, called win_dow and equal 
# to 51 by default.
# The second window is the look-back interval, called look_back.
# To avoid an error, the end_points should be greater than 
# the EWMA win_dow, except for the first end_points, which 
# should be equal to zero.
# Adjust the end_points so that they are greater than the 
# EWMA win_dow.

end_points[(end_points > 0) & (end_points <= win_dow)] <- win_dow+1

# Run roll_agg() as follows:

sharpe_ratios <- roll_agg(x_ts=oh_lc, 
                          end_points=end_points, 
                          look_back=look_back, 
                          FUN=agg_regate, 
                          lamb_das=lamb_das,
                          win_dow=win_dow)

# You should get the following output:
# > sharpe_ratios[1:6, 1:5]
#                  [,1]       [,2]       [,3]       [,4]       [,5]
# 2007-03-19 -1.7531927 -1.7531927 -1.7531927 -1.7531927 -1.7531927
# 2007-03-19 -1.7531927 -1.7531927 -1.7531927 -1.7531927 -1.7531927
# 2007-03-30 -2.2223479 -2.2223479 -2.2223479 -2.2223479 -2.2223479
# 2007-04-30 -0.9446608 -0.9446608 -0.9446608 -0.9446608 -0.9446608
# 2007-05-31  0.0550219  0.0550219  0.0550219  0.0550219  0.0550219
# 2007-06-29 -0.3290286 -0.3290286 -0.3290286 -0.3290286 -0.3290286


#########

bar <- "foo"
bar <- 10

bar <- "foo"
assign(bar, 10)



###

var_1 <- sum(pc_1*pc_1)
# make re_turns orthogonal to pc1
in_dex <- index(re_turns)
re_turns <- apply(re_turns, MARGIN=2, 
                  function(x) {x - sum(pc_1*x)*pc_1/var_1})
# apply(re_turns, MARGIN=2, function(x) sum(pc_1*x)) # verify orthogonality

###

x11()
foo <- seq(0, 2*pi, length.out=24)
plot(x=cos(foo), y=sin(foo), asp=1)
abline(a=0, b=-0.1, col="red")
abline(a=0, b=10, col="blue")


###

heatmap(sharpe_ratios, col=colorRampPalette(c("blue", "red"))(22))

summary(microbenchmark(
  tee=-t(portf_rets) %*% portf_rets,
  s_um=-sum(portf_rets*portf_rets),
  times=10))[, c(1, 4, 5)]


###

w_1 <- sqrt(0.5); w_2 <- w_1
foo <- matrix(c(w_1, w_2, -w_2, w_1), nc=2)
t(foo) %*% foo
# bar <- re_turns %*% t(solve(foo))
(t(bar) %*% bar) / NROW(bar)

cov_mat <- function(re_turns, an_gle=0) {
  w_1 <- cos(an_gle)
  w_2 <- sin(an_gle)
  mat_rix <- matrix(c(w_1, -w_2, w_2, w_1), nc=2)
  compo_nents <- re_turns %*% t(mat_rix)
  (t(compo_nents) %*% compo_nents) / NROW(compo_nents)
}  # end cov_mat

bar <- cov_mat(re_turns, an_gle=pi/4)
(t(re_turns) %*% re_turns) / NROW(re_turns)
(t(bar) %*% bar) / NROW(bar)

angle_s <- seq(0, pi/2, by=pi/24)
co_var <- sapply(angle_s, function(an_gle) 
  cov_mat(re_turns, an_gle=an_gle)[1, 1])
plot(x=angle_s, y=co_var, t="l")

op_tim <- optimize(
  f=function(an_gle) 
    -cov_mat(re_turns, an_gle=an_gle)[1, 1], 
  interval=range(angle_s))
an_gle <- op_tim$minimum
bar <- cov_mat(re_turns, an_gle=an_gle)
tan(an_gle)

w_1 <- cos(an_gle)
w_2 <- sin(an_gle)
mat_rix <- matrix(c(w_1, -w_2, w_2, w_1), nc=2)
compo_nents <- re_turns %*% t(mat_rix)
(t(compo_nents) %*% compo_nents) / NROW(compo_nents)

plot(x=compo_nents[, 1], y=compo_nents[, 2],
     xlim=c(-10, 10), ylim=c(-10, 10))

reg_model <- lm(reg_formula, data=re_turns)
# get regression coefficients
coef(summary(reg_model))

foo <- cbind(rnorm(1000, sd=0.2), rnorm(1000)) %*% t(mat_rix)
(t(foo) %*% foo) / NROW(foo)
plot(x=foo[, 1], y=foo[, 2])
summary(lm(foo[, 1] ~ foo[, 2]))

op_tim <- optimize(
  f=function(an_gle) 
    -cov_mat(foo, an_gle=an_gle)[1, 1], 
  interval=range(angle_s))
an_gle <- op_tim$minimum
tan(an_gle)

###

library(plotly)

df <- data.frame(Date = seq(as.Date("2016-01-01"), as.Date("2016-08-31"), by="days"),
                 Value = sample(100:200, size = 244, replace = T))

plot_ly(data = df, x = df$Date, y = df$Value, type = "scatter", mode="lines") %>%
  add_trace(x=~df$Date, y=~df$Value, name="20yr Treasury rate") %>% 
  layout(xaxis = list(range = c( as.numeric(max(df$Date)-30) *86400000,
                                 as.numeric(max(df$Date)) * 86400000   ),
                      rangeslider = list(type = "date")  ))

###



