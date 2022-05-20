lambdas <- c(0.5, 1, 1.5)
colors <- c("red", "blue", "green")
# Plot three curves in loop
for (it in 1:3) {
  curve(expr=plogis(x, scale=lambdas[it]),
xlim=c(-4, 4), type="l", xlab="", ylab="", lwd=4,
col=colors[it], add=(it>1))
}  # end for
# Add title
title(main="Logistic function", line=0.5)
# Add legend
legend("topleft", title="Scale parameters",
       paste("lambda", lambdas, sep="="),
       inset=0.05, cex=0.8, lwd=6, bty="n",
       lty=1, col=colors)

set.seed(1121)  # Reset random number generator
# Simulate overlapping scores data
sample1 <- runif(100, max=0.6)
sample2 <- runif(100, min=0.4)
# Perform Wilcoxon test for similar distributions
wilcox.test(sample1, sample2)
# Combine scores and add categorical variable
predictor <- c(sample1, sample2)
response <- c(logical(100), !logical(100))
# Perform logit regression
glmod <- glm(response ~ predictor, family=binomial(logit))
class(glmod)
summary(glmod)

x11(width=6, height=5)
par(mar=c(3, 3, 2, 2), mgp=c(2, 1, 0), oma=c(0, 0, 0, 0))
ordern <- order(predictor)
plot(x=predictor[ordern], y=glmod$fitted.values[ordern],
     main="Category Densities and Logistic Function",
     type="l", lwd=4, col="orange", xlab="predictor", ylab="density")
densityv <- density(predictor[response])
densityv$y <- densityv$y/max(densityv$y)
lines(densityv, col="red")
polygon(c(min(densityv$x), densityv$x, max(densityv$x)), c(min(densityv$y), densityv$y, min(densityv$y)), col=rgb(1, 0, 0, 0.2), border=NA)
densityv <- density(predictor[!response])
densityv$y <- densityv$y/max(densityv$y)
lines(densityv, col="blue")
polygon(c(min(densityv$x), densityv$x, max(densityv$x)), c(min(densityv$y), densityv$y, min(densityv$y)), col=rgb(0, 0, 1, 0.2), border=NA)
# Add legend
legend(x="top", cex=1.0, bty="n", lty=c(1, NA, NA),
 lwd=c(6, NA, NA), pch=c(NA, 15, 15),
 legend=c("logistic fit", "TRUE", "FALSE"),
 col=c("orange", "red", "blue"),
 text.col=c("black", "red", "blue"))

# Likelihood function of binomial distribution
likefun <- function(prob, b) {
  b*log(prob) + (1-b)*log(1-prob)
}  # end likefun
likefun(prob=0.25, b=1)
# Plot binomial likelihood function
curve(expr=likefun(x, b=1), xlim=c(0, 1), lwd=3,
      xlab="prob", ylab="likelihood", col="blue",
      main="Binomial Likelihood Function")
curve(expr=likefun(x, b=0), lwd=3, col="red", add=TRUE)
legend(x="top", bty="n", legend=c("b = 1", "b = 0"),
       title=NULL, inset=0.3, cex=1.0, lwd=6,
       lty=1, col=c("blue", "red"))

# Specify predictor matrix
predictor=cbind(intercept=rep(1, NROW(response)), predictor)
# Likelihood function of the logistic model
likefun <- function(coeff, response, predictor) {
  probs <- plogis(drop(predictor %*% coeff))
  -sum(response*log(probs) + (1-response)*log((1-probs)))
}  # end likefun
# Run likelihood function

# Rastrigin function with vector argument for optimization
rastrigin <- function(vectorv, param=25) {
  sum(vectorv^2 - param*cos(vectorv))
}  # end rastrigin
vectorv <- c(pi/6, pi/6)
rastrigin(vectorv=vectorv)
# Draw 3d surface plot of Rastrigin function
options(rgl.useNULL=TRUE); library(rgl)
rgl::persp3d(
  x=Vectorize(function(x, y) rastrigin(vectorv=c(x, y))),
  xlim=c(-10, 10), ylim=c(-10, 10),
  col="green", axes=FALSE, zlab="", main="rastrigin")
# Render the 3d surface plot of function
rgl::rglwidget(elementId="plot3drgl", width=400, height=400)
# Optimize with respect to vector argument
optiml <- optim(par=vectorv, fn=rastrigin,
        method="L-BFGS-B",
        upper=c(4*pi, 4*pi),
        lower=c(pi/2, pi/2),
        param=1)
# Optimal parameters and value
optiml$par
optiml$value
rastrigin(optiml$par, param=1)

# Initial parameters
initp <- c(1, 1)
# Find max likelihood parameters using steepest descent optimizer
optim_fit <- optim(par=initp,
           fn=likefun, # Log-likelihood function
           method="L-BFGS-B", # Quasi-Newton method
           response=response,
           predictor=predictor,
           upper=c(20, 20), # Upper constraint
           lower=c(-20, -20), # Lower constraint
           hessian=TRUE)
# Optimal logistic parameters
optim_fit$par
unname(glmod$coefficients)
# Standard errors of parameters
sqrt(diag(solve(optim_fit$hessian)))
model_sum <- summary(glmod)
model_sum$coefficients[, 2]

library(ISLR)  # Load package ISLR
# get documentation for package tseries
packageDescription("ISLR")  # get short description

help(package="ISLR")  # Load help page

library(ISLR)  # Load package ISLR

data(package="ISLR")  # list all datasets in ISLR

ls("package:ISLR")  # list all objects in ISLR

detach("package:ISLR")  # Remove ISLR from search path

# Coerce the student and default columns into Boolean
Default <- ISLR::Default
Default$default <- (Default$default == "Yes")
Default$student <- (Default$student == "Yes")
colnames(Default)[1:2] <- c("default", "student")
attach(Default)  # Attach Default to search path
# Explore credit default data
summary(Default)
sapply(Default, class)
dim(Default)
head(Default)

# Plot data points for non-defaulters
x11(width=6, height=5)
xlim <- range(balance); ylim <- range(income)
plot(income ~ balance,
     main="Default Dataset from Package ISLR",
     xlim=xlim, ylim=ylim, pch=4, col="blue",
     data=Default[!default, ])
# Plot data points for defaulters
points(income ~ balance, pch=4, lwd=2, col="red",
 data=Default[default, ])
# Add legend
legend(x="topright", legend=c("non-defaulters", "defaulters"),
 bty="n", col=c("blue", "red"), lty=1, lwd=6, pch=4)

# Wilcoxon test for balance predictor
wilcox.test(balance[default], balance[!default])
# Wilcoxon test for income predictor
wilcox.test(income[default], income[!default])

x11(width=6, height=5)
# Set 2 plot panels
par(mfrow=c(1,2))
# Balance boxplot
boxplot(formula=balance ~ default,
  col="lightgrey", main="balance", xlab="Default")
# Income boxplot
boxplot(formula=income ~ default,
  col="lightgrey", main="income", xlab="Default")

# Fit logistic regression model
glmod <- glm(default ~ balance, family=binomial(logit))
class(glmod)
summary(glmod)

x11(width=6, height=5)
par(mar=c(4, 4, 2, 2), oma=c(0, 0, 0, 0), mgp=c(2.5, 1, 0))
plot(x=balance, y=default,
     main="Logistic Regression of Credit Defaults",
     col="orange", xlab="credit balance", ylab="defaults")
ordern <- order(balance)
lines(x=balance[ordern], y=glmod$fitted.values[ordern], col="blue", lwd=3)
legend(x="topleft", inset=0.1, bty="n", lwd=6,
 legend=c("defaults", "logit fitted values"),
 col=c("orange", "blue"), lty=c(NA, 1), pch=c(1, NA))

# Calculate cumulative defaults
to_tal <- sum(default)
default_s <- sapply(balance, function(lim) {
    sum(default[balance <= lim])
})  # end sapply
# Perform logit regression
glmod <- glm(cbind(default_s, to_tal-default_s) ~ balance,
  family=binomial(logit))
summary(glmod)

plot(x=balance, y=default_s/to_tal, col="orange", lwd=1,
     main="Cumulative Defaults Versus Balance",
     xlab="credit balance", ylab="cumulative defaults")
ordern <- order(balance)
lines(x=balance[ordern], y=glmod$fitted.values[ordern],
col="blue", lwd=3)
legend(x="topleft", inset=0.1, bty="n",
 legend=c("cumulative defaults", "fitted values"),
 col=c("orange", "blue"), lty=c(NA, 1), pch=c(1, NA), lwd=6)

# Fit multifactor logistic regression model
colnamev <- colnames(Default)
formulav <- as.formula(paste(colnamev[1],
  paste(colnamev[-1], collapse="+"), sep=" ~ "))
formulav
glmod <- glm(formulav, data=Default, family=binomial(logit))
summary(glmod)

# Fit single-factor logistic model with student as predictor
glm_student <- glm(default ~ student, family=binomial(logit))
summary(glm_student)
# Multifactor coefficient is negative
glmod$coefficients
# Single-factor coefficient is positive
glm_student$coefficients

# Calculate cumulative defaults
cum_defaults <- sapply(balance, function(lim) {
c(student=sum(default[student & (balance <= lim)]),
  non_student=sum(default[!student & (balance <= lim)]))
})  # end sapply
total_defaults <- c(student=sum(student & default),
      student=sum(!student & default))
cum_defaults <- t(cum_defaults / total_defaults)
# Plot cumulative defaults
par(mfrow=c(1,2))  # Set plot panels
ordern <- order(balance)
plot(x=balance[ordern], y=cum_defaults[ordern, 1],
     col="red", t="l", lwd=2, xlab="credit balance", ylab="",
     main="Cumulative defaults of\n students and non-students")
lines(x=balance[ordern], y=cum_defaults[ordern, 2], col="blue", lwd=2)
legend(x="topleft", bty="n",
 legend=c("students", "non-students"),
 col=c("red", "blue"), text.col=c("red", "blue"), lwd=3)
# Balance boxplot for student factor
boxplot(formula=balance ~ !student,
  col="lightgrey", main="balance", xlab="Student")

# Perform in-sample forecast from logistic regression model
forecasts <- predict(glmod, type="response")
all.equal(glmod$fitted.values, forecasts)
# Define discrimination threshold value
threshold <- 0.7
# Calculate confusion matrix in-sample
table(actual=!default, forecast=(forecasts < threshold))
# Fit logistic regression over training data
set.seed(1121)  # Reset random number generator
nrows <- NROW(Default)
samplev <- sample.int(n=nrows, size=nrows/2)
traindata <- Default[samplev, ]
glmod <- glm(formulav, data=traindata, family=binomial(logit))
# Forecast over test data out-of-sample
testdata <- Default[-samplev, ]
forecasts <- predict(glmod, newdata=testdata, type="response")
# Calculate confusion matrix out-of-sample
table(actual=!testdata$default, 
forecast=(forecasts < threshold))

# Calculate confusion matrix out-of-sample
confmat <- table(actual=!testdata$default, 
forecast=(forecasts < threshold))
confmat
# Calculate FALSE positive (type I error)
sum(!testdata$default & (forecasts > threshold))
# Calculate FALSE negative (type II error)
sum(testdata$default & (forecasts < threshold))

# Calculate FALSE positive and FALSE negative rates
confmat <- confmat / rowSums(confmat)
c(typeI=confmat[2, 1], typeII=confmat[1, 2])
detach(Default)
# Below is an unsuccessful attempt to draw confusion matrix using xtable
confusion_matrix <- matrix(c("| true positive \\\\ (sensitivity)", "| false negative \\\\ (type II error)", "| false positive \\\\ (type I error)", "| true negative \\\\ (specificity)"), nc=2)
dimnames(confusion_matrix) <- list(forecast=c("FALSE", "TRUE"),
                             actual=c("FALSE", "TRUE"))
print(xtable::xtable(confusion_matrix,
caption="Confusion Matrix"),
caption.placement="top",
comment=FALSE, size="scriptsize",
include.rownames=TRUE,
include.colnames=TRUE)
# end unsuccessful attempt to draw confusion table using xtable

# Confusion matrix as function of threshold
confun <- function(actu_al, forecasts, threshold) {
    confmat <- table(actu_al, (forecasts < threshold))
    confmat <- confmat / rowSums(confmat)
    c(typeI=confmat[2, 1], typeII=confmat[1, 2])
  }  # end confun
confun(!testdata$default, forecasts, threshold=threshold)
# Define vector of discrimination thresholds
threshv <- seq(0.05, 0.95, by=0.05)^2
# Calculate error rates
error_rates <- sapply(threshv, confun,
  actu_al=!testdata$default, forecasts=forecasts)  # end sapply
error_rates <- t(error_rates)
rownames(error_rates) <- threshv
error_rates <- rbind(c(1, 0), error_rates)
error_rates <- rbind(error_rates, c(0, 1))
# Calculate area under ROC curve (AUC)
true_pos <- (1 - error_rates[, "typeII"])
true_pos <- (true_pos + rutils::lagit(true_pos))/2
false_pos <- rutils::diffit(error_rates[, "typeI"])
abs(sum(true_pos*false_pos))

# Plot ROC Curve for Defaults
x11(width=5, height=5)
plot(x=error_rates[, "typeI"], y=1-error_rates[, "typeII"],
     xlab="FALSE positive rate", ylab="TRUE positive rate",
     main="ROC Curve for Defaults", type="l", lwd=3, col="blue")
abline(a=0.0, b=1.0, lwd=3, col="orange")

# Install package data.table
install.packages("data.table")
# Load package data.table
library(data.table)
# Get documentation for package data.table
# Get short description
packageDescription("data.table")
# Load help page
help(package="data.table")
# List all datasets in "data.table"
data(package="data.table")
# List all objects in "data.table"
ls("package:data.table")
# Remove data.table from search path
detach("package:data.table")

# Create a data table
library(data.table)
data_table <- data.table::data.table(
  col1=sample(7), col2=sample(7), col3=sample(7))
# Print data_table
class(data_table); data_table
# Column referenced without quotes
data_table[, col2]
# Row referenced without a following comma
data_table[2]
# Print option "datatable.print.nrows"
getOption("datatable.print.nrows")
options(datatable.print.nrows=10)
getOption("datatable.print.nrows")
# Number of rows in data_table
NROW(data_table)
# Or
data_table[, NROW(col1)]
# Or
data_table[, .N]
# microbenchmark speed of data.table syntax
library(microbenchmark)
summary(microbenchmark(
  dt=data_table[, .N],
  pure_r=NROW(data_table),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# Read a data table from CSV file
dir_name <- "/Users/jerzy/Develop/lecture_slides/data/"
file_name <- file.path(dir_name, "weather_delays14.csv")
data_table <- data.table::fread(file_name)
class(data_table); dim(data_table)
data_table
# fread() reads the same data as read.csv()
all.equal(read.csv(file_name),
    setDF(data.table::fread(file_name)))
# fread() is much faster than read.csv()
library(microbenchmark)
summary(microbenchmark(
  pure_r=read.csv(file_name),
  fread=setDF(data.table::fread(file_name)),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Write data table to file in different ways
data.table::fwrite(data_table, file="data_table.csv")
write.csv(data_table, file="data_table2.csv")
cat(unlist(data_table), file="data_table3.csv")
# microbenchmark speed of data.table::fwrite()
summary(microbenchmark(
  fwrite=data.table::fwrite(data_table, file="data_table.csv"),
  write_csv=write.csv(data_table, file="data_table2.csv"),
  cat=cat(unlist(data_table), file="data_table3.csv"),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# Select first five rows of data_table
data_table[1:5]
# Select rows with JFK flights
jfk_flights <- data_table[origin=="JFK"]
# Select rows JFK flights in June
jfk_flights <- data_table[origin=="JFK" & month==6]
# Select rows without JFK flights
jfk_flights <- data_table[!(origin=="JFK")]
# Select flights with carrier_delay
data_table[carrier_delay > 0]
# Select column of data_table and return a vector
head(data_table[, origin])
# Select column of data_table and return a data_table, not vector
head(data_table[, list(origin)])
head(data_table[, .(origin)])
# Select two columns of data_table
data_table[, list(origin, month)]
data_table[, .(origin, month)]
columnv <- c("origin", "month")
data_table[, ..columnv]
data_table[, month, origin]
# Select two columns and rename them
data_table[, .(orig=origin, mon=month)]
# Select all columns except origin
head(data_table[, !"origin"])
head(data_table[, -"origin"])

# Select flights with positive carrier_delay
data_table[carrier_delay > 0]
# Number of flights with carrier_delay
data_table[, sum(carrier_delay > 0)]
# Or standard R commands
sum(data_table[, carrier_delay > 0])
# microbenchmark speed of data.table syntax
summary(microbenchmark(
  dt=data_table[, sum(carrier_delay > 0)],
  pure_r=sum(data_table[, carrier_delay > 0]),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Average carrier_delay
data_table[, mean(carrier_delay)]
# Average carrier_delay and aircraft_delay
data_table[, .(carrier=mean(carrier_delay),
         aircraft=mean(aircraft_delay))]
# Average aircraft_delay from JFK
data_table[origin=="JFK", mean(aircraft_delay)]
# Number of flights from JFK
data_table[origin=="JFK", NROW(aircraft_delay)]
# Or
data_table[origin=="JFK", .N]

# Number of flights from each airport
data_table[, .N, by=origin]
# Same, but add names to output
data_table[, .(flights=.N), by=.(airport=origin)]
# Number of AA flights from each airport
data_table[carrier=="AA", .(flights=.N),
     by=.(airport=origin)]
# Number of flights from each airport and airline
data_table[, .(flights=.N),
     by=.(airport=origin, airline=carrier)]
# Average aircraft_delay
data_table[, mean(aircraft_delay)]
# Average aircraft_delay from JFK
data_table[origin=="JFK", mean(aircraft_delay)]
# Average aircraft_delay from each airport
data_table[, .(delay=mean(aircraft_delay)),
     by=.(airport=origin)]
# Average and max delays from each airport and month
data_table[, .(mean_delay=mean(aircraft_delay), max_delay=max(aircraft_delay)),
     by=.(airport=origin, month=month)]
# Average and max delays from each airport and month
data_table[, .(mean_delay=mean(aircraft_delay), max_delay=max(aircraft_delay)),
     keyby=.(airport=origin, month=month)]

# Sort ascending by origin, then descending by dest
order_table <- data_table[order(origin, -dest)]
order_table
# Doesn't work outside data_table
order(origin, -dest)
# Sort data_table by reference
setorder(data_table, origin, -dest)
all.equal(data_table, order_table)
# setorder() is much faster than order()
summary(microbenchmark(
  order=data_table[order(origin, -dest)],
  setorder=setorder(data_table, origin, -dest),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Average aircraft_delay by month
order_table[, .(mean_delay=mean(aircraft_delay)),
      by=.(month=month)]
# Chained brackets to sort output by month
order_table[, .(mean_delay=mean(aircraft_delay)),
      by=.(month=month)][order(month)]

# Select weather_delay and aircraft_delay in two different ways
data_table[1:7, .SD,
     .SDcols=c("weather_delay", "aircraft_delay")]
data_table[1:7, .(weather_delay, aircraft_delay)]
# Calculate mean of weather_delay and aircraft_delay
data_table[, sapply(.SD, mean),
     .SDcols=c("weather_delay", "aircraft_delay")]
sapply(data_table[, .SD,
     .SDcols=c("weather_delay", "aircraft_delay")], mean)
# Return origin and dest, then all other columns
data_table[1:7, .SD, by=.(origin, dest)]
# Return origin and dest, then weather_delay and aircraft_delay columns
data_table[1:7, .SD, by=.(origin, dest),
     .SDcols=c("weather_delay", "aircraft_delay")]
# Return first two rows from each month
data_table[, head(.SD, 2), by=.(month)]
data_table[, head(.SD, 2), by=.(month),
     .SDcols=c("weather_delay", "aircraft_delay")]
# Calculate mean of weather_delay and aircraft_delay, grouped by origin
data_table[, lapply(.SD, mean),
     by=.(origin),
     .SDcols=c("weather_delay", "aircraft_delay")]
# Or simply
data_table[, .(weather_delay=mean(weather_delay),
         aircraft_delay=mean(aircraft_delay)),
     by=.(origin)]

# Add tot_delay column
data_table[, tot_delay := (carrier_delay + aircraft_delay)]
head(data_table, 4)
# Delete tot_delay column
data_table[, tot_delay := NULL]
# Add max_delay column grouped by origin and dest
data_table[, max_delay := max(aircraft_delay),
     by=.(origin, dest)]
data_table[, max_delay := NULL]
# Add date and tot_delay columns
data_table[, c("date", "tot_delay") :=
       list(paste(month, day, year, sep="/"),
            (carrier_delay + aircraft_delay))]
# Modify select rows of tot_delay column
data_table[month == 12, tot_delay := carrier_delay]
data_table[, c("date", "tot_delay") := NULL]
# Add several columns
data_table[, c("max_carrier", "max_aircraft") :=
       lapply(.SD, max),
     by=.(origin, dest),
     .SDcols=c("carrier_delay", "aircraft_delay")]
data_table[, c("max_carrier", "max_aircraft") := NULL]
# Modifying by reference is much faster than standard R
summary(microbenchmark(
  dt=data_table[, tot_delay := (carrier_delay + aircraft_delay)],
  pure_r=(data_table[, "tot_delay"] <- data_table[, "carrier_delay"] + data_table[, "aircraft_delay"]),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# Add a key based on the "origin" column
setkey(data_table, origin)
haskey(data_table)
key(data_table)
# Select rows with LGA using the key
data_table["LGA"]
all.equal(data_table["LGA"],
    data_table[origin == "LGA"])
# Select rows with LGA and JFK using the key
data_table[c("LGA", "JFK")]
# Add a key based on the "origin" and "dest" columns
setkey(data_table, origin, dest)
key(data_table)
# Select rows with origin from JFK and MIA
data_table[c("JFK", "MIA")]
# Select rows with origin from JFK and dest to MIA
data_table[.("JFK", "MIA")]
all.equal(data_table[.("JFK", "MIA")],
    data_table[origin == "JFK" & dest == "MIA"])
# Selecting rows using a key is much faster than standard R
summary(microbenchmark(
  with_key=data_table[.("JFK", "MIA")],
  standard_r=data_table[origin == "JFK" & dest == "MIA"],
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# Create data frame and coerce it to data table
data_table <- data.frame(
  col1=sample(7), col2=sample(7), col3=sample(7))
class(data_table); data_table
data.table::setDT(data_table)
class(data_table); data_table
# Coerce data_table into data frame
data.table::setDF(data_table)
class(data_table); data_table
# Or
data_table <- data.table:::as.data.frame.data.table(data_table)
# SetDF() is much faster than as.data.frame()
summary(microbenchmark(
  as.data.frame=data.table:::as.data.frame.data.table(data_table),
  setDF=data.table::setDF(data_table),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# Coerce xts to a data frame
prices <- rutils::etfenv$VTI
class(prices); head(prices)
prices <- as.data.frame(prices)
class(prices); head(prices)
# Coerce data frame to a data table
data.table::setDT(prices, keep.rownames=TRUE)
class(prices); head(prices)
# Dates are coerced to strings
sapply(prices, class)
# Coerce xts directly to a data table
data_table <- as.data.table(rutils::etfenv$VTI,
  keep.rownames=TRUE)
class(data_table); head(data_table)
# Dates are not coerced to strings
sapply(data_table, class)
all.equal(prices, data_table, check.attributes=FALSE)

# Install package fst
install.packages("fst")
# Load package fst
library(fst)
# Get documentation for package fst
# Get short description
packageDescription("fst")
# Load help page
help(package="fst")
# List all datasets in "fst"
data(package="fst")
# List all objects in "fst"
ls("package:fst")
# Remove fst from search path
detach("package:fst")

# Read a data frame from CSV file
dir_name <- "/Users/jerzy/Develop/lecture_slides/data/"
file_name <- file.path(dir_name, "weather_delays14.csv")
data.table::setDF(data_frame)
class(data_frame); dim(data_frame)
# Write data frame to .fst file in different ways
fst::write_fst(data_frame, path="data_frame.fst")
write.csv(data_frame, file="data_frame2.csv")
# microbenchmark speed of fst::write_fst()
library(microbenchmark)
summary(microbenchmark(
  fst=fst::write_fst(data_frame, path="data_frame.csv"),
  write_csv=write.csv(data_frame, file="data_frame2.csv"),
  cat=cat(unlist(data_frame), file="data_frame3.csv"),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# fst::read_fst() reads the same data as read.csv()
all.equal(read.csv(file_name),
    fst::read_fst("data_frame.fst"))
# fst::read_fst() is 10 times faster than read.csv()
summary(microbenchmark(
  fst=fst::read_fst("data_frame.fst"),
  read_csv=read.csv(file_name),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# Coerce TAQ xts to a data frame
library(HighFreq)
t_aq <- HighFreq::SPY_TAQ
t_aq <- as.data.frame(t_aq)
class(t_aq)
# Coerce data frame to a data table
data.table::setDT(t_aq, keep.rownames=TRUE)
class(t_aq); head(t_aq)
# Get memory size of data table
format(object.size(t_aq), units="MB")
# Save data table to .fst file
fst::write_fst(t_aq, path="/Users/jerzy/Develop/data/taq.fst")
# Create reference to .fst file similar to a data frame
fs_t <- fst::fst("/Users/jerzy/Develop/data/taq.fst")
class(fs_t)
# Memory size of reference to .fst is very small
format(object.size(fs_t), units="MB")
# Get sizes of all objects in workspace
sort(sapply(mget(ls()), object.size))
# Reference to .fst can be treated similar to a data table
dim(t_aq); dim(fs_t)
fst:::print.fst_table(fs_t)
# Subset reference to .fst just like a data table
fs_t[1e4:(1e4+5), ]

library(HighFreq)
# Read TAQ trade data from csv file
taq <- data.table::fread(file="/Users/jerzy/Develop/data/xlk_tick_trades_2020_03_16.csv")
# Inspect the TAQ data
taq
class(taq)
colnames(taq)
sapply(taq, class)
symbol <- taq$SYM_ROOT[1]
# Create date-time index
dates <- paste(taq$DATE, taq$TIME_M)
# Coerce date-time index to POSIXlt
dates <- strptime(dates, "%Y%m%d %H:%M:%OS")
class(dates)
# Display more significant digits
# options("digits")
options(digits=20, digits.secs=10)
last(dates)
unclass(last(dates))
as.numeric(last(dates))
# Coerce date-time index to POSIXct
dates <- as.POSIXct(dates)
class(dates)
last(dates)
unclass(last(dates))
as.numeric(last(dates))
# Calculate the number of ticks per second
nsecs <- as.numeric(last(dates)) - as.numeric(first(dates))
NROW(taq)/(6.5*3600)
# Select TAQ data columns
taq <- taq[, .(price=PRICE, volume=SIZE)]
# Add date-time index
taq <- cbind(index=dates, taq)

# Coerce trade ticks to xts series
xtes <- xts::xts(taq[, .(price, volume)], taq$index)
colnames(xtes) <- paste(symbol, c("Close", "Volume"), sep=".")
save(xtes, file="/Users/jerzy/Develop/data/xlk_tick_trades2020_0316.RData")
# save(xtes, file="/Users/jerzy/Develop/data/xlk_tick_trades2020_0316.RData")
# Plot dygraph
dygraphs::dygraph(xtes$XLK.Close,
  main="XLK Trade Ticks for 2020-03-16")
# Plot in x11 window
x11(width=6, height=5)
quantmod::chart_Series(x=xtes$XLK.Close,
  name="XLK Trade Ticks for 2020-03-16")

# Calculate centered Hampel filter to remove price jumps
look_back <- 111
half_back <- look_back %/% 2
medianv <- roll::roll_median(taq$price, width=look_back)
# medianv <- TTR::runMedian(taq$price, n=look_back)
medianv <- rutils::lagit(medianv, lagg=-half_back, pad_zeros=FALSE)
madv <- HighFreq::roll_var(matrix(taq$price), look_back=look_back, method="nonparametric")
# madv <- TTR::runMAD(taq$price, n=look_back)
madv <- rutils::lagit(madv, lagg=-half_back, pad_zeros=FALSE)
# Calculate Z-scores
zscores <- (taq$price - medianv)/madv
zscores[is.na(zscores)] <- 0
zscores[!is.finite(zscores)] <- 0
sum(is.na(zscores))
sum(!is.finite(zscores))
range(zscores); mad(zscores)
hist(zscores, breaks=2000, xlim=c(-5*mad(zscores), 5*mad(zscores)))

# Define discrimination threshold value
threshold <- 6*mad(zscores)
# Remove price jumps with large z-scores
bad_ticks <- (abs(zscores) > threshold)
good_ticks <- taq[!bad_ticks]
# Calculate number of price jumps
sum(bad_ticks)/NROW(zscores)
# Coerce trade prices to xts
xtes <- xts::xts(good_ticks[, .(price, volume)], good_ticks$index)
colnames(xtes) <- c("XLK.Close", "XLK.Volume")
# Plot dygraph of the clean lots
dygraphs::dygraph(xtes$XLK.Close,
  main="XLK Trade Ticks for 2020-03-16 (Hampel filtered)")
# Plot the large lots
x11(width=6, height=5)
quantmod::chart_Series(x=xtes$XLK.Close,
  name="XLK Trade Ticks for 2020-03-16 (Hampel filtered)")

# Define discrimination threshold value
threshold <- 6*mad(zscores)
# Calculate number of prices classified as bad data
is_bad <- (abs(zscores) > threshold)
sum(is_bad)
# Add 200 random price jumps into prices
set.seed(1121)
nbad <- 200
closep <- quantmod::Cl(xtes)
nrows <- NROW(xtes)
isjump <- logical(NROW(closep))
isjump[sample(x=NROW(isjump), size=nbad)] <- TRUE
closep[isjump] <- closep[isjump]*
  sample(c(0.95, 1.05), size=nbad, replace=TRUE)
# Plot prices and medians
dygraphs::dygraph(cbind(closep, medianv), main="VTI median") %>%
  dyOptions(colors=c("black", "red"))
# Calculate time series of z-scores
medianv <- roll::roll_median(closep, width=look_back)
medianv <- rutils::lagit(medianv, lagg=-half_back, pad_zeros=FALSE)
# medianv <- TTR::runMedian(closep, n=look_back)
madv <- HighFreq::roll_var(closep, look_back=look_back, method="nonparametric")
madv <- rutils::lagit(madv, lagg=-half_back, pad_zeros=FALSE)
# madv <- TTR::runMAD(closep, n=look_back)
zscores <- (closep - medianv)/madv
zscores[1:look_back, ] <- 0
zscores[is.na(zscores)] <- 0
zscores[!is.finite(zscores)] <- 0
# Calculate number of prices classified as bad data
is_bad <- (abs(zscores) > threshold)
sum(is_bad)

# Calculate confusion matrix
table(actual=!isjump, forecast=!is_bad)
sum(is_bad)
# FALSE positive (type I error)
sum(!isjump & is_bad)
# FALSE negative (type II error)
sum(isjump & !is_bad)

# Confusion matrix as function of threshold
confun <- function(actu_al, zscores, threshold) {
    confmat <- table(!actu_al, !(abs(zscores) > threshold))
    confmat <- confmat / rowSums(confmat)
    c(typeI=confmat[2, 1], typeII=confmat[1, 2])
  }  # end confun
confun(isjump, zscores, threshold=threshold)
# Define vector of discrimination thresholds
threshv <- seq(from=0.2, to=5.0, by=0.2)
# Calculate error rates
error_rates <- sapply(threshv, confun,
  actu_al=isjump, zscores=zscores)  # end sapply
error_rates <- t(error_rates)
rownames(error_rates) <- threshv
error_rates <- rbind(c(1, 0), error_rates)
error_rates <- rbind(error_rates, c(0, 1))
# Calculate area under ROC curve (AUC)
true_pos <- (1 - error_rates[, "typeII"])
true_pos <- (true_pos + rutils::lagit(true_pos))/2
false_pos <- rutils::diffit(error_rates[, "typeI"])
abs(sum(true_pos*false_pos))

# Plot ROC curve for Hampel classifier
x11(width=6, height=5)
plot(x=error_rates[, "typeI"], y=1-error_rates[, "typeII"],
     xlab="FALSE positive rate", ylab="TRUE positive rate",
     xlim=c(0, 1), ylim=c(0, 1),
     main="ROC Curve for Hampel Classifier",
     type="l", lwd=3, col="blue")
abline(a=0.0, b=1.0, lwd=3, col="orange")

# Calculate centered Hampel filter over 3 data points
medianv <- roll::roll_median(taq$price, width=3)
medianv[1:2] <- taq$price[1:2]
medianv <- rutils::lagit(medianv, lagg=-1, pad_zeros=FALSE)
madv <- HighFreq::roll_var(matrix(taq$price), look_back=look_back, method="nonparametric")
madv <- rutils::lagit(madv, lagg=-1, pad_zeros=FALSE)
# Calculate Z-scores
zscores <- ifelse(madv > 0, (taq$price - medianv)/madv, 0)
range(zscores); mad(zscores)
madv <- mad(zscores[abs(zscores)>0])
hist(zscores, breaks=2000, xlim=c(-5*madv, 5*madv))

# Define discrimination threshold value
threshold <- 6*madv
bad_ticks <- (abs(zscores) > threshold)
good_ticks <- taq[!bad_ticks]
# Calculate number of price jumps
sum(bad_ticks)/NROW(zscores)
# Coerce trade prices to xts
xtes <- xts::xts(good_ticks[, .(price, volume)], good_ticks$index)
colnames(xtes) <- c("XLK.Close", "XLK.Volume")
# Plot dygraph of the clean lots
dygraphs::dygraph(xtes$XLK.Close,
  main="XLK Trade Ticks for 2020-03-16 (Hampel filtered)")
# Plot the large lots
x11(width=6, height=5)
quantmod::chart_Series(x=xtes$XLK.Close,
  name="XLK Trade Ticks for 2020-03-16 (Hampel filtered)")
