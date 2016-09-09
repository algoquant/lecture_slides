library(knitr)
opts_chunk$set(prompt=TRUE, eval=FALSE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
options(digits=3)
thm <- knit_theme$get("acid")
knit_theme$set(thm)
## vec_tor1 <- rnorm(1000000)
## vec_tor2 <- rnorm(1000000)
## big_vector <- numeric(1000000)
## system.time(  # sum vectors using "for" loop
##   for(i in 1:length(vec_tor1)) {
##     big_vector[i] <- vec_tor1[i] + vec_tor2[i]
##   }  # end for
## )  # end system.time
## # sum vectors using vectorized "+"
## system.time(big_vector <- vec_tor1 + vec_tor2)
## # allocate memory for cumulative sum
## cum_sum <- numeric(length(big_vector))
## # cumulative sum using "for" loop
## cum_sum[1] <- big_vector[1]
## system.time(
##   for(i in 2:length(big_vector)) {
##     cum_sum[i] <- cum_sum[i-1] + big_vector[i]
##   }  # end for
## )  # end system.time
## # cumulative sum using "cumsum"
## system.time(cum_sum <- cumsum(big_vector))
## # calculate row sums two different ways
## summary(microbenchmark(
##   row_sums=rowSums(big_matrix),
##   ap_ply=apply(big_matrix, 1, sum),
##   times=10))[, c(1, 4, 5)]  # end microbenchmark summary
## library(microbenchmark)
## str(pmax)
## # calculate row maximums two different ways
## summary(microbenchmark(
##   p_max=
##     do.call(pmax.int,
## lapply(seq_along(big_matrix[1, ]),
##   function(in_dex) big_matrix[, in_dex])),
##   l_apply=unlist(
##     lapply(seq_along(big_matrix[, 1]),
##   function(in_dex) max(big_matrix[in_dex, ]))),
##   times=10))[, c(1, 4, 5)]
## library(matrixStats)  # load package matrixStats
## # calculate row min values three different ways
## summary(microbenchmark(
##   row_mins=rowMins(big_matrix),
##   p_min=
##     do.call(pmin.int,
##       lapply(seq_along(big_matrix[1, ]),
##              function(in_dex)
##                big_matrix[, in_dex])),
##   as_data_frame=
##     do.call(pmin.int,
##       as.data.frame.matrix(big_matrix)),
##   times=10))[, c(1, 4, 5)]  # end microbenchmark summary
## summary(microbenchmark(  # assign values to vector three different ways
## # fast vectorized assignment loop performed in C using brackets "[]"
##   brack_ets={vec_tor <- numeric(10)
##     vec_tor[] <- 2},
## # slow because loop is performed in R
##   for_loop={vec_tor <- numeric(10)
##     for (in_dex in seq_along(vec_tor))
##       vec_tor[in_dex] <- 2},
## # very slow because no memory is pre-allocated
## # "vec_tor" is "grown" with each new element
##   grow_vec={vec_tor <- numeric(0)
##     for (in_dex in 1:10)
## # add new element to "vec_tor" ("grow" it)
##       vec_tor[in_dex] <- 2},
##   times=10))[, c(1, 4, 5)]  # end microbenchmark summary
## summary(microbenchmark(  # assign values to vector two different ways
## # fast vectorized assignment loop performed in C using brackets "[]"
##   brack_ets={vec_tor <- numeric(10)
##     vec_tor[4:7] <- rnorm(4)},
## # slow because loop is performed in R
##   for_loop={vec_tor <- numeric(10)
##     for (in_dex in 4:7)
##       vec_tor[in_dex] <- rnorm(1)},
##   times=10))[, c(1, 4, 5)]  # end microbenchmark summary
## load(file="C:/Develop/data/etf_data.RData")
## # define function vectorized automatically
## my_fun <- function(in_put, pa_ram) {
##   pa_ram*in_put
## }  # end my_fun
## # "in_put" is vectorized
## my_fun(in_put=1:3, pa_ram=2)
## # "pa_ram" is vectorized
## my_fun(in_put=10, pa_ram=2:4)
## # define vectors of parameters of rnorm()
## std_devs <-
##   structure(1:3, names=paste0("sd=", 1:3))
## me_ans <-
##   structure(-1:1, names=paste0("mean=", -1:1))
## # "sd" argument of rnorm() isn't vectorized
## rnorm(1, sd=std_devs)
## # "mean" argument of rnorm() isn't vectorized
## rnorm(1, mean=me_ans)
## load(file="C:/Develop/data/etf_data.RData")
## # sapply produces desired vector output
## set.seed(1121)
## sapply(std_devs, function(std_dev) rnorm(n=2, sd=std_dev))
## set.seed(1121)
## sapply(std_devs, rnorm, n=2, mean=0)
## set.seed(1121)
## sapply(me_ans,
##  function(me_an) rnorm(n=2, mean=me_an))
## set.seed(1121)
## sapply(me_ans, rnorm, n=2)
## load(file="C:/Develop/data/etf_data.RData")
## # rnorm() vectorized with respect to "std_dev"
## vec_rnorm <- function(n, mean=0, sd=1) {
##   if (length(sd)==1)
##     rnorm(n=n, mean=mean, sd=sd)
##   else
##     sapply(sd, rnorm, n=n, mean=mean)
## }  # end vec_rnorm
## set.seed(1121)
## vec_rnorm(n=2, sd=std_devs)
## # rnorm() vectorized with respect to "mean" and "sd"
## vec_rnorm <- Vectorize(FUN=rnorm,
##         vectorize.args=c("mean", "sd")
## )  # end Vectorize
## set.seed(1121)
## vec_rnorm(n=2, sd=std_devs)
## set.seed(1121)
## vec_rnorm(n=2, mean=me_ans)
## load(file="C:/Develop/data/etf_data.RData")
## str(sum)
## # na.rm is bound by name
## mapply(sum, 6:9, c(5, NA, 3), 2:6, na.rm=TRUE)
## str(rnorm)
## # mapply vectorizes both arguments "mean" and "sd"
## mapply(rnorm, n=5, mean=me_ans, sd=std_devs)
## mapply(function(in_put, e_xp) in_put^e_xp,
##  1:5, seq(from=1, by=0.2, length.out=5))
## load(file="C:/Develop/data/etf_data.RData")
## # rnorm() vectorized with respect to "mean" and "sd"
## vec_rnorm <- function(n, mean=0, sd=1) {
##   if (length(mean)==1 && length(sd)==1)
##     rnorm(n=n, mean=mean, sd=sd)
##   else
##     mapply(rnorm, n=n, mean=mean, sd=sd)
## }  # end vec_rnorm
## # call vec_rnorm() on vector of "sd"
## vec_rnorm(n=2, sd=std_devs)
## # call vec_rnorm() on vector of "mean"
## vec_rnorm(n=2, mean=me_ans)
## # create two numeric vectors
## vec_tor1 <- sin(0.25*pi*1:10)
## vec_tor2 <- cos(0.25*pi*1:10)
## # create third vector using 'ifelse'
## vec_tor3 <- ifelse(vec_tor1 > vec_tor2,
##           vec_tor1, vec_tor2)
## # cbind all three together
## vec_tor4 <- cbind(vec_tor1, vec_tor2, vec_tor3)
## 
## # set plotting parameters
## par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0),
##     cex.lab=0.8, cex.axis=0.8, cex.main=0.8,
##     cex.sub=0.5)
## # plot matrix
## matplot(vec_tor4, type="l", lty="solid",
## col=c("green", "blue", "red"),
## lwd=c(2, 2, 2), xlab="", ylab="")
## # add legend
## legend(x="bottomright", legend=colnames(vec_tor4),
##        title="", inset=0.05, cex=0.8, lwd=2,
##        lty=c(1, 1, 1), col=c("green", "blue", "red"))
## options(width=50, dev='pdf')
## str(optimize)
## # objective function with multiple minima
## object_ive <- function(in_put, param1=0.01) {
##   sin(0.25*pi*in_put) + param1*(in_put-1)^2
## }  # end object_ive
## unlist(optimize(f=object_ive, interval=c(-4, 2)))
## unlist(optimize(f=object_ive, interval=c(0, 8)))
## options(width=60, dev='pdf')
## par(oma=c(1, 1, 1, 1), mgp=c(2, 1, 0), mar=c(5, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
## # plot objective function
## curve(expr=object_ive, type="l", xlim=c(-8, 9),
## xlab="", ylab="", lwd=2)
## title(main="Objective Function", line=-1)  # add title
## # sample of normal variables
## sam_ple <- rnorm(1000, mean=4, sd=2)
## # objective function is log-likelihood
## object_ive <- function(parm, sam_ple) {
##   sum(2*log(parm[2]) +
##     ((sam_ple - parm[1])/parm[2])^2)
## }  # end object_ive
## # vectorize objective function
## vec_objective <- Vectorize(
##   FUN=function(mean, sd, sam_ple)
##     object_ive(c(mean, sd), sam_ple),
##   vectorize.args=c("mean", "sd")
## )  # end Vectorize
## # objective function on parameter grid
## par_mean <- seq(1, 6, length=50)
## par_sd <- seq(0.5, 3.0, length=50)
## objective_grid <- outer(par_mean, par_sd,
## vec_objective, sam_ple=sam_ple)
## objective_min <- which(  # grid search
##   objective_grid==min(objective_grid),
##   arr.ind=TRUE)
## objective_min
## par_mean[objective_min[1]]  # mean
## par_sd[objective_min[2]]  # sd
## objective_grid[objective_min]
## objective_grid[(objective_min[, 1] + -1:1),
##        (objective_min[, 2] + -1:1)]
## par(cex.lab=2.0, cex.axis=2.0, cex.main=2.0, cex.sub=2.0)
## # perspective plot of log-likelihood function
## persp(z=-objective_grid,
## theta=45, phi=30, shade=0.5,
## border="green", zlab="objective",
## main="objective function")
## # interactive perspective plot of log-likelihood function
## library(rgl)  # load package rgl
## par3d(cex=2.0)  # scale text by factor of 2
## persp3d(z=-objective_grid, zlab="objective",
##   col="green", main="objective function")
## # initial parameters
## par_init <- c(mean=0, sd=1)
## # perform optimization quasi-Newton method
## optim_run <- optim(par=par_init,
##        fn=object_ive,
##        sam_ple=sam_ple,
##        method="L-BFGS-B",
##        upper=c(10, 10),
##        lower=c(-10, 0.1))
## # optimal parameters
## optim_run$par
## # plot histogram
## histo_gram <- hist(sam_ple, plot=FALSE)
## plot(histo_gram, freq=FALSE,
##      main="histogram of sample")
## curve(expr=dnorm(x, mean=optim_run$par["mean"],
##            sd=optim_run$par["sd"]),
## add=TRUE, type="l", lwd=2, col="red")
## legend("topright", inset=0.0, cex=0.8, title=NULL,
##  leg="optimal parameters",
##  lwd=2, bg="white", col="red")
## load(file="C:/Develop/data/etf_data.RData")
## # sample from mixture of normal distributions
## sam_ple <- c(rnorm(100, sd=1.0),
##              rnorm(100, mean=4, sd=1.0))
## # objective function is log-likelihood
## object_ive <- function(parm, sam_ple) {
##   likelihood <- parm[1]/parm[3] *
##   dnorm((sam_ple-parm[2])/parm[3]) +
##   (1-parm[1])/parm[5]*dnorm((sam_ple-parm[4])/parm[5])
##   if(any(likelihood <= 0)) Inf else
##     -sum(log(likelihood))
## }  # end object_ive
## # vectorize objective function
## vec_objective <- Vectorize(
##   FUN=function(mean, sd, w, m1, s1, sam_ple)
##     object_ive(c(w, m1, s1, mean, sd), sam_ple),
##   vectorize.args=c("mean", "sd")
## )  # end Vectorize
## # objective function on parameter grid
## par_mean <- seq(3, 5, length=50)
## par_sd <- seq(0.5, 1.5, length=50)
## objective_grid <- outer(par_mean, par_sd,
##     vec_objective, sam_ple=sam_ple,
##     w=0.5, m1=2.0, s1=2.0)
## rownames(objective_grid) <- round(par_mean, 2)
## colnames(objective_grid) <- round(par_sd, 2)
## objective_min <- which(objective_grid==
##   min(objective_grid), arr.ind=TRUE)
## objective_min
## objective_grid[objective_min]
## objective_grid[(objective_min[, 1] + -1:1),
##          (objective_min[, 2] + -1:1)]
## # perspective plot of objective function
## persp(par_mean, par_sd, -objective_grid,
## theta=45, phi=30,
## shade=0.5,
## col=rainbow(50),
## border="green",
## main="objective function")
## load(file="C:/Develop/data/etf_data.RData")
## # initial parameters
## par_init <- c(weight=0.5, m1=0, s1=1, m2=2, s2=1)
## # perform optimization
## optim_run <- optim(par=par_init,
##       fn=object_ive,
##       sam_ple=sam_ple,
##       method="L-BFGS-B",
##       upper=c(1,10,10,10,10),
##       lower=c(0,-10,0.2,-10,0.2))
## optim_run$par
## # plot histogram
## histo_gram <- hist(sam_ple, plot=FALSE)
## plot(histo_gram, freq=FALSE,
##      main="histogram of sample")
## fit_func <- function(x, parm) {
##   parm["weight"] * dnorm(x, mean=parm["m1"], sd=parm["s1"]) +
##     (1-parm["weight"]) * dnorm(x, mean=parm["m2"], sd=parm["s2"])
## }  # end fit_func
## curve(expr=fit_func(x, parm=optim_run$par), add=TRUE,
## type="l", lwd=2, col="red")
## legend("topright", inset=0.0, cex=0.8, title=NULL,
##  leg="optimal parameters",
##  lwd=2, bg="white", col="red")
## load(file="C:/Develop/data/etf_data.RData")
## # create list of symbols for optimized portfolio
## sym_bols <- c("VTI", "VNQ", "DBC")
## # create initial vector of portfolio weights
## portf_weights <- rep(1, length(sym_bols))
## names(portf_weights) <- sym_bols
## # objective equal to minus Sharpe ratio
## object_ive <- function(weights) {
##   portf_ts <- env_etf$re_turns[, sym_bols] %*% weights
##   -sum(portf_ts)/sd(portf_ts)
## }  # end object_ive
## # objective for equal weight portfolio
## object_ive(portf_weights)
## par(oma=c(1, 1, 1, 1), mgp=c(2, 1, 0), mar=c(5, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
## # vectorize objective function with respect to third weight
## vec_object <- Vectorize(
##   FUN=function(weight) object_ive(c(1, 1, weight)),
##   vectorize.args="weight"
## )  # end Vectorize
## # plot objective function with respect to third weight
## curve(expr=vec_object,
##       type="l", xlim=c(-4.0, 1.0),
##       xlab=paste("weight of", names(portf_weights[3])),
##       ylab="", lwd=2)
## title(main="Objective Function", line=-1)  # add title
## # vectorize function with respect to all weights
## vec_object <- Vectorize(
##   FUN=function(w1, w2, w3)
##     object_ive(c(w1, w2, w3)),
##   vectorize.args=c("w2", "w3"))  # end Vectorize
## # calculate objective on 2-d (w2 x w3) parameter grid
## w2 <- seq(-5, 5, length=50)
## w3 <- seq(-5, 5, length=50)
## grid_object <- outer(w2, w3, FUN=vec_object, w1=1)
## rownames(grid_object) <- round(w2, 2)
## colnames(grid_object) <- round(w3, 2)
## # perspective plot of objective function
## persp(w2, w3, -grid_object,
## theta=45, phi=30, shade=0.5,
## col=rainbow(50), border="green",
## main="objective function")
## # interactive perspective plot of objective function
## library(rgl)
## persp3d(z=-grid_object, zlab="objective",
##   col="green", main="objective function")
## persp3d(
##   x=function(w2, w3)
##     -vec_object(w1=1, w2, w3),
##   xlim=c(-5, 5), ylim=c(-5, 5),
##   col="green", axes=FALSE)
# optimization to find weights with maximum Sharpe ratio
optim_run <- optim(par=portf_weights,
             fn=object_ive,
             method="L-BFGS-B",
             upper=c(1.1, 10, 10),
             lower=c(0.9, -10, -10))
# optimal parameters
optim_run$par
# optimal Sharpe ratio
-object_ive(optim_run$par)
## par(oma=c(1, 0, 1, 0), mgp=c(2, 1, 0), mar=c(2, 1, 2, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
## library(PortfolioAnalytics)
## # returns of optimal portfolio
## optim_rets <- xts(env_etf$re_turns[, sym_bols] %*%
## optim_run$par, order.by=index(env_etf$re_turns))
## # assign colnames to this xts
## colnames(optim_rets) <- "optim_rets"
## # plot in two vertical panels
## layout(matrix(c(1,2), 2),
##  widths=c(1,1), heights=c(1,3))
## # barplot of optimal portfolio weights
## barplot(optim_run$par,
##   names.arg=names(optim_run$par),
##   las=3, ylab="", xlab="Symbol", main="")
## # plot optimal returns with "VTI", "VNQ" and "DBC"
## chart.CumReturns(
##   cbind(optim_rets, env_etf$re_turns[, names(portf_weights)]),
##   lwd=2, ylab="", legend.loc="topleft", main="")
