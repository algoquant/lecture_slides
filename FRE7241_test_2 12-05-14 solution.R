#################################
### Test #2 Solutions 12/05/14
#################################
# Max score 45pts

# The below solutions are an example,
# Slightly different solutions are also possible.
# You must use the requested functions.


# 1. (10pts) Create a log-likelihood objective function for a mixture of two normal distributions,
#    assume that the two distributions both have sd=1.0, but different means,
#    the target vector is a sample from the mixture of two normal distributions,
target_vector <- c(rnorm(300, mean=0.0, sd=1.0), 
                   rnorm(100, mean=4.0, sd=1.0))
#    the objective function should be a function of a vector of three parameters: 
#    weight, mean1, and mean2,
#    and also a function of the sample "target_vector", 
object_ive <- function(parm, target) {
  likelihood <- parm[1] * dnorm(target-parm[2]) +
    (1-parm[1])*dnorm(target-parm[3])
  if(any(likelihood <= 0)) Inf else
    -sum(log(likelihood))
}  # end object_ive


# 2. (10pts) create a vectorized objective function, by vectorizing the mean1 and mean2 parameters, 
vec_objective <- Vectorize(
  FUN=function(mean1, mean2, w, target)
    object_ive(c(w, mean1, mean2), target),
  vectorize.args=c("mean1", "mean2")
)  # end Vectorize


# 3. (10pts) create vectors of mean1 and mean2 parameters, with 50 values from -10, to 10,
mean1 <- seq(-10, 10, length=50)
mean2 <- seq(-10, 10, length=50)

#    calculate the objective function on a parameter grid made from the two vectors, 
#    set the weight=0.3,
objective_grid <- outer(mean1, mean2, 
                    vec_objective, 
                    target=target_vector, w=0.3)
rownames(objective_grid) <- round(mean1, 2)
colnames(objective_grid) <- round(mean2, 2)

#    create a perspective plot of the objective function,
persp(mean1, mean2, -objective_grid,
      theta = 45, phi = 30,
      shade = 0.5,
      col = rainbow(50),
      border = "green",
      main = "objective function")


# 4. (10pts) perform optimization using the function "optim", 
#    to find the optimal parameters: weight, mean1, and mean2,
#    set the "upper" and "lower" parameter limits to c(1,10,10) and c(0,-10,-10),
#    set the initial parameters to:
par_init <- c(weight=0.99, mean1=0, mean2=0)
# perform optimization
optim_run <- optim(par=par_init, 
                   fn=object_ive, 
                   target=target_vector,
                   method="L-BFGS-B",
                   upper=c(1,10,10),
                   lower=c(0,-10,-10))


# 5. (5pts) plot the histogram of "target_vector",
histo_gram <- hist(target_vector, plot=FALSE)
plot(histo_gram, freq=FALSE, 
     main="histogram of target vector")

#    plot the mixture distribution with the fitted parameters,
fit_func <- function(x, parm) {
  parm["weight"] * dnorm(x, mean=parm["mean1"]) + 
    (1-parm["weight"]) * dnorm(x, mean=parm["mean2"])
}  # end fit_func
curve(expr=fit_func(x, parm=optim_run$par), add=TRUE,
      type="l", lwd=2, col="red")
legend("topright", inset=0.0, cex=0.8, title=NULL, 
       leg="optimal parameters", 
       lwd=2, bg="white", col="red")


