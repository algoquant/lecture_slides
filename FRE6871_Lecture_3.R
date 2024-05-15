# Create a plotting expression
expv <- quote({
  degf <- 2:20
  rangev <- (1:NROW(degf))
  indeks <- 4
  # Plot a curve
  curve(expr=dchisq(x, df=degf[indeks]),
xlim=c(0, 30), ylim=c(0, 0.2),
xlab="", ylab="", lwd=3, col="red")
  # Add grey lines to plot
  for (it in rangev[-indeks]) {
    curve(expr=dchisq(x, df=degf[it]),
  xlim=c(0, 30), ylim=c(0, 0.2),
  xlab="", ylab="", lwd=2, col="grey80", add=TRUE)
  }  # end for
  # Add title
  title(main="Chi-squared Distributions", line=-1.5, cex.main=1.5)
  # Add legend
  text(x=20, y=0.15, labels=paste0("Degrees of freedom=",
      degf[indeks]), pos=1, cex=1.3)
})  # end quote
# View the plotting expression
expv
# Create plot by evaluating the plotting expression
x11(width=6, height=4)
eval(expv)
library(animation)
# Create an expression for creating multiple plots
expv <- quote({
  degf <- 2:20
  rangev <- (1:NROW(degf))
  # Set image refesh interval
  animation::ani.options(interval=0.5)
  # Create multiple plots with curves
  for (indeks in rangev) {
    curve(expr=dchisq(x, df=degf[indeks]),
  xlim=c(0, 30), ylim=c(0, 0.2),
  xlab="", ylab="", lwd=3, col="red")
    # Add grey lines to plot
    for (it in rangev[-indeks]) {
      curve(expr=dchisq(x, df=degf[it]),
    xlim=c(0, 30), ylim=c(0, 0.2),
    xlab="", ylab="", lwd=2, col="grey80", add=TRUE)
    }  # end for
    # Add title
    title(main="Chi-squared Distributions", line=-1.5, cex.main=1.5)
    # Add legend
    text(x=20, y=0.15, labels=paste0("Degrees of freedom=",
      degf[indeks]), pos=1, cex=1.3)
  }  # end for
})  # end quote
# Create plot by evaluating the plotting expression
x11(width=6, height=4)
eval(expv)
# Create gif with animated plot
animation::saveGIF(expr=eval(expv),
  movie.name="chi_squared.gif",
  img.name="chi_squared")
# Create html with animated plot
animation::saveHTML(expr=eval(expv),
  img.name="chi_squared",
  htmlfile="chi_squared.html",
  description="Chi-squared Distributions")  # end saveHTML
NA
App setup code that runs only once at startup.
ndata <- 1e4
stdev <- 1.0
Define the user interface
uiface <- shiny::fluidPage(
  # Create numeric input for the number of data points.
  numericInput("ndata", "Number of data points:", value=ndata),
  # Create slider input for the standard deviation parameter.
  sliderInput("stdev", label="Standard deviation:",
        min=0.1, max=3.0, value=stdev, step=0.1),
  # Render plot in a panel.
  plotOutput("plotobj", height=300, width=500)
)  # end user interface
Define the server function
servfun <- function(input, output) {
  output$plotobj <- shiny::renderPlot({
    # Simulate the data
    datav <- rnorm(input$ndata, sd=input$stdev)
    # Plot the data
    par(mar=c(2, 4, 4, 0), oma=c(0, 0, 0, 0))
    hist(datav, xlim=c(-4, 4), main="Histogram of Random Data")
  })  # end renderPlot
}  # end servfun
# Return a Shiny app object
shiny::shinyApp(ui=uiface, server=servfun)
Create elements of the user interface
uiface <- shiny::fluidPage(
  titlePanel("VWAP Moving Average"),
  # Create single row of widgets with two slider inputs
  fluidRow(
    # Input stock symbol
    column(width=3, selectInput("symbol", label="Symbol",
                          choices=symbolv, selected=symbol)),
    # Input look-back interval
    column(width=3, sliderInput("lookb", label="Lookback interval",
                          min=1, max=150, value=11, step=1))
  ),  # end fluidRow
  # Create output plot panel
  mainPanel(dygraphs::dygraphOutput("dyplot"), width=12)
)  # end fluidPage interface
Define the server function
servfun <- shiny::shinyServer(function(input, output) {
  # Get the close and volume data in a reactive environment
  closep <- shiny::reactive({
    # Get the data
    ohlc <- get(input$symbol, data_env)
    closep <- log(quantmod::Cl(ohlc))
    volum <- quantmod::Vo(ohlc)
    # Return the data
    cbind(closep, volum)
  })  # end reactive code
  # Calculate the VWAP indicator in a reactive environment
  vwapv <- shiny::reactive({
    # Get model parameters from input argument
    lookb <- input$lookb
    # Calculate the VWAP indicator
    closep <- closep()[, 1]
    volum <- closep()[, 2]
    vwapv <- HighFreq::roll_sum(tseries=closep*volum, lookb=lookb)
    volumroll <- HighFreq::roll_sum(tseries=volum, lookb=lookb)
    vwapv <- vwapv/volumroll
    vwapv[is.na(vwapv)] <- 0
    # Return the plot data
    datav <- cbind(closep, vwapv)
    colnames(datav) <- c(input$symbol, "VWAP")
    datav
  })  # end reactive code
  # Return the dygraph plot to output argument
  output$dyplot <- dygraphs::renderDygraph({
    colnamev <- colnames(vwapv())
    dygraphs::dygraph(vwapv(), main=paste(colnamev[1], "VWAP")) %>%
dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col="blue") %>%
dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=2, col="red")
  })  # end output plot
})  # end server code
Return a Shiny app object
shiny::shinyApp(ui=uiface, server=servfun)
Define the server function
servfun <- shiny::shinyServer(function(input, output) {
  # Create an empty list of reactive values.
  value_s <- reactiveValues()
  # Get input parameters from the user interface.
  nrows <- reactive({
    # Add nrows to list of reactive values.
    value_s*nrows <- input$nrows
    input$nrows
  })  # end reactive code
  # Broadcast a message to the console when the button is pressed.
  observeEvent(eventExpr=input$button, handlerExpr={
    cat("Input button pressed\n")
  })  # end observeEvent
  # Send the data when the button is pressed.
  datav <- eventReactive(eventExpr=input$button, valueExpr={
    # eventReactive() executes on input$button, but not on nrows() or input$nrows.
    cat("Sending", nrows(), "rows of data\n")
    datav <- head(mtcars, input$nrows)
    value_s$mpg <- mean(datav$mpg)
    datav
  })  # end eventReactive
  #   datav
  # Draw table of the data when the button is pressed.
  observeEvent(eventExpr=input$button, handlerExpr={
    datav <- datav()
    cat("Received", value_s*nrows, "rows of data\n")
    cat("Average mpg = ", value_s$mpg, "\n")
    cat("Drawing table\n")
    output$tablev <- renderTable(datav)
  })  # end observeEvent
})  # end server code
Return a Shiny app object
shiny::shinyApp(ui=uiface, server=servfun)
# Create a random real symmetric matrix
matv <- matrix(runif(25), nc=5)
matv <- matv + t(matv)
# Calculate the eigenvalues and eigenvectors
eigend <- eigen(matv)
eigenvec <- eigend$vectors
dim(eigenvec)
# Plot eigenvalues
barplot(eigend$values, xlab="", ylab="", las=3,
  names.arg=paste0("ev", 1:NROW(eigend$values)),
  main="Eigenvalues of a real symmetric matrix")
# Eigenvectors form an orthonormal basis
round(t(eigenvec) %*% eigenvec, digits=4)
# Diagonalize matrix using eigenvector matrix
round(t(eigenvec) %*% (matv %*% eigenvec), digits=4)
eigend$values
# Eigen decomposition of matrix by rotating the diagonal matrix
matrixe <- eigenvec %*% (eigend$values * t(eigenvec))
# Create diagonal matrix of eigenvalues
# diagmat <- diag(eigend$values)
# matrixe <- eigenvec %*% (diagmat %*% t(eigenvec))
all.equal(matv, matrixe)
# Create a random positive semi-definite matrix
matv <- matrix(runif(25), nc=5)
matv <- t(matv) %*% matv
# Calculate the eigenvalues and eigenvectors
eigend <- eigen(matv)
eigend$values
# Plot eigenvalues
barplot(eigend$values, las=3, xlab="", ylab="",
  names.arg=paste0("ev", 1:NROW(eigend$values)),
  main="Eigenvalues of positive semi-definite matrix")
# Perform singular value decomposition
matv <- matrix(rnorm(50), nc=5)
svdec <- svd(matv)
# Recompose matv from SVD mat_rices
all.equal(matv, svdec$u %*% (svdec$d*t(svdec$v)))
# Columns of U and V are orthonormal
round(t(svdec$u) %*% svdec$u, 4)
round(t(svdec$v) %*% svdec$v, 4)
# Dimensions of left and right matrices
nrows <- 6 ; ncols <- 4
# Calculate the left matrix
leftmat <- matrix(runif(nrows^2), nc=nrows)
eigend <- eigen(crossprod(leftmat))
leftmat <- eigend$vectors[, 1:ncols]
# Calculate the right matrix and singular values
rightmat <- matrix(runif(ncols^2), nc=ncols)
eigend <- eigen(crossprod(rightmat))
rightmat <- eigend$vectors
singval <- sort(runif(ncols, min=1, max=5), decreasing=TRUE)
# Compose rectangular matrix
matv <- leftmat %*% (singval * t(rightmat))
# Perform singular value decomposition
svdec <- svd(matv)
# Recompose matv from SVD
all.equal(matv, svdec$u %*% (svdec$d*t(svdec$v)))
# Compare SVD with matv components
all.equal(abs(svdec$u), abs(leftmat))
all.equal(abs(svdec$v), abs(rightmat))
all.equal(svdec$d, singval)
# Eigen decomposition of matv squared
retsq <- matv %*% t(matv)
eigend <- eigen(retsq)
all.equal(eigend$values[1:ncols], singval^2)
all.equal(abs(eigend$vectors[, 1:ncols]), abs(leftmat))
# Eigen decomposition of matv squared
retsq <- t(matv) %*% matv
eigend <- eigen(retsq)
all.equal(eigend$values, singval^2)
all.equal(abs(eigend$vectors), abs(rightmat))
# Create a random positive semi-definite matrix
matv <- matrix(runif(25), nc=5)
matv <- t(matv) %*% matv
# Calculate the inverse of matv
invmat <- solve(a=matv)
# Multiply inverse with matrix
round(invmat %*% matv, 4)
round(matv %*% invmat, 4)
# Calculate the eigenvalues and eigenvectors
eigend <- eigen(matv)
eigenvec <- eigend$vectors
# Calculate the inverse from eigen decomposition
inveigen <- eigenvec %*% (t(eigenvec) / eigend$values)
all.equal(invmat, inveigen)
# Decompose diagonal matrix with inverse of eigenvalues
# diagmat <- diag(1/eigend$values)
# inveigen <- eigenvec %*% (diagmat %*% t(eigenvec))
# Random rectangular matrix: nrows > ncols
nrows <- 6 ; ncols <- 4
matv <- matrix(runif(nrows*ncols), nc=ncols)
# Calculate the generalized inverse of matv
invmat <- MASS::ginv(matv)
round(invmat %*% matv, 4)
all.equal(matv, matv %*% invmat %*% matv)
# Random rectangular matrix: nrows < ncols
nrows <- 4 ; ncols <- 6
matv <- matrix(runif(nrows*ncols), nc=ncols)
# Calculate the generalized inverse of matv
invmat <- MASS::ginv(matv)
all.equal(matv, matv %*% invmat %*% matv)
round(matv %*% invmat, 4)
round(invmat %*% matv, 4)
# Perform singular value decomposition
svdec <- svd(matv)
# Calculate the generalized inverse from SVD
invsvd <- svdec$v %*% (t(svdec$u) / svdec$d)
all.equal(invsvd, invmat)
# Calculate the Moore-Penrose pseudo-inverse
invmp <- MASS::ginv(t(matv) %*% matv) %*% t(matv)
all.equal(invmp, invmat)
# Create a random singular matrix
# More columns than rows: ncols > nrows
nrows <- 4 ; ncols <- 6
matv <- matrix(runif(nrows*ncols), nc=ncols)
matv <- t(matv) %*% matv
# Perform singular value decomposition
svdec <- svd(matv)
# Incorrect inverse from SVD because of zero singular values
invsvd <- svdec$v %*% (t(svdec$u) / svdec$d)
# Inverse property doesn't hold
all.equal(matv, matv %*% invsvd %*% matv)
# Set tolerance for determining zero singular values
precv <- sqrt(.Machine$double.eps)
# Check for zero singular values
round(svdec$d, 12)
notzero <- (svdec$d > (precv*svdec$d[1]))
# Calculate the regularized inverse from SVD
invsvd <- svdec$v[, notzero] %*%
  (t(svdec$u[, notzero]) / svdec$d[notzero])
# Verify inverse property of matv
all.equal(matv, matv %*% invsvd %*% matv)
# Calculate the regularized inverse using MASS::ginv()
invmat <- MASS::ginv(matv)
all.equal(invsvd, invmat)
# Calculate the Moore-Penrose pseudo-inverse
invmp <- MASS::ginv(t(matv) %*% matv) %*% t(matv)
all.equal(invmp, invmat)
# Diagonalize the unit matrix
unitmat <- matv %*% invmat
round(unitmat, 4)
round(matv %*% invmat, 4)
round(t(svdec$u) %*% unitmat %*% svdec$v, 4)
# Define a square matrix
matv <- matrix(c(1, 2, -1, 2), nc=2)
vecv <- c(2, 1)
# Calculate the inverse of matv
invmat <- solve(a=matv)
invmat %*% matv
# Calculate the solution using inverse of matv
solutionv <- invmat %*% vecv
matv %*% solutionv
# Calculate the solution of linear system
solutionv <- solve(a=matv, b=vecv)
matv %*% solutionv
# Create a random matrix
matv <- matrix(rnorm(100), nc=10)
# Calculate the matrix inverse using solve()
invmatr <- solve(a=matv)
round(invmatr %*% matv, 4)
# Compile the C++ file using Rcpp
Rcpp::sourceCpp(file="/Users/jerzy/Develop/Rcpp/test_fun.cpp")
# Calculate the matrix inverse using C++
invmat <- calc_invmat(matv)
all.equal(invmat, invmatr)
# Compare the speed of RcppArmadillo with R code
library(microbenchmark)
summary(microbenchmark(
  ginv=MASS::ginv(matv),
  solve=solve(matv),
  cpp=calc_invmat(matv),
  times=10))[, c(1, 4, 5)]
# Create large random positive semi-definite matrix
matv <- matrix(runif(1e4), nc=100)
matv <- t(matv) %*% matv
# Calculate the eigen decomposition
eigend <- eigen(matv)
eigenval <- eigend$values
eigenvec <- eigend$vectors
# Set tolerance for determining zero singular values
precv <- sqrt(.Machine$double.eps)
# If needed convert to positive definite matrix
notzero <- (eigenval > (precv*eigenval[1]))
if (sum(!notzero) > 0) {
  eigenval[!notzero] <- 2*precv
  matv <- eigenvec %*% (eigenval * t(eigenvec))
}  # end if
# Calculate the Cholesky matv
cholmat <- chol(matv)
cholmat[1:5, 1:5]
all.equal(matv, t(cholmat) %*% cholmat)
# Calculate the inverse from Cholesky
invchol <- chol2inv(cholmat)
all.equal(solve(matv), invchol)
# Compare speed of Cholesky inversion
library(microbenchmark)
summary(microbenchmark(
  solve=solve(matv),
  cholmat=chol2inv(chol(matv)),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Calculate the random covariance matrix
covmat <- matrix(runif(25), nc=5)
covmat <- t(covmat) %*% covmat
# Calculate the Cholesky matrix
cholmat <- chol(covmat)
cholmat
# Simulate random uncorrelated returns
nassets <- 5
nrows <- 10000
retp <- matrix(rnorm(nassets*nrows), nc=nassets)
# Calculate the correlated returns by applying Cholesky
retscorr <- retp %*% cholmat
# Calculate the covariance matrix
covmat2 <- crossprod(retscorr) /(nrows-1)
covmat2 <-cov(retscorr)
all.equal(covmat, covmat2)
# Simulate random stock returns
nassets <- 10
nrows <- 100
# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
retp <- matrix(rnorm(nassets*nrows), nc=nassets)
# Calculate the centered (de-meaned) returns matrix
retp <- t(t(retp) - colMeans(retp))
# Or
retp <- apply(retp, MARGIN=2, function(x) (x-mean(x)))
# Calculate the covariance matrix
covmat <- crossprod(retp) /(nrows-1)
# Calculate the eigenvalues and eigenvectors
eigend <- eigen(covmat)
eigend$values
barplot(eigend$values, # Plot eigenvalues
  xlab="", ylab="", las=3,
  names.arg=paste0("ev", 1:NROW(eigend$values)),
  main="Eigenvalues of Covariance Matrix")
# Calculate the eigenvalues and eigenvectors
# as function of number of returns
ndata <- ((nassets/2):(2*nassets))
eigenval <- sapply(ndata, function(x) {
  retp <- retp[1:x, ]
  retp <- apply(retp, MARGIN=2, function(y) (y - mean(y)))
  covmat <- crossprod(retp) / (x-1)
  min(eigen(covmat)$values)
})  # end sapply
plot(y=eigenval, x=ndata, t="l", xlab="", ylab="", lwd=3, col="blue",
  main="Smallest eigenvalue of covariance matrix
  as function of number of returns")
# Create rectangular matrix with collinear columns
matv <- matrix(rnorm(10*8), nc=10)
# Calculate the covariance matrix
covmat <- cov(matv)
# Calculate the inverse of covmat - error
invmat <- solve(covmat)
# Calculate the regularized inverse of covmat
invmat <- MASS::ginv(covmat)
# Verify inverse property of matv
all.equal(covmat, covmat %*% invmat %*% covmat)
# Perform eigen decomposition
eigend <- eigen(covmat)
eigenvec <- eigend$vectors
eigenval <- eigend$values
# Set tolerance for determining zero singular values
precv <- sqrt(.Machine$double.eps)
# Calculate the regularized inverse matrix
notzero <- (eigenval > (precv * eigenval[1]))
invreg <- eigenvec[, notzero] %*%
  (t(eigenvec[, notzero]) / eigenval[notzero])
# Verify that invmat is same as invreg
all.equal(invmat, invreg)
# Calculate the regularized inverse matrix using cutoff
dimax <- 3
invmat <- eigenvec[, 1:dimax] %*%
  (t(eigenvec[, 1:dimax]) / eigend$values[1:dimax])
# Verify that invmat is same as invreg
all.equal(invmat, invreg)
# Create a random covariance matrix
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
matv <- matrix(rnorm(5e2), nc=5)
covmat <- cov(matv)
cormat <- cor(matv)
stdev <- sqrt(diag(covmat))
# Calculate the target matrix
cormean <- mean(cormat[upper.tri(cormat)])
targetmat <- matrix(cormean, nr=NROW(covmat), nc=NCOL(covmat))
diag(targetmat) <- 1
targetmat <- t(t(targetmat * stdev) * stdev)
# Calculate the shrinkage covariance matrix
alphac <- 0.5
covshrink <- (1-alphac)*covmat + alphac*targetmat
# Calculate the inverse matrix
invmat <- solve(covshrink)
# Create a random matrix
matv <- matrix(rnorm(100), nc=10)
# Calculate the inverse of matv
invmat <- solve(a=matv)
# Multiply inverse with matrix
round(invmat %*% matv, 4)
# Calculate the initial inverse
invmatr <- invmat + matrix(rnorm(100, sd=0.1), nc=10)
# Calculate the approximate recursive inverse of matv
invmatr <- (2*invmatr - invmatr %*% matv %*% invmatr)
# Calculate the sum of the off-diagonal elements
sum((invmatr %*% matv)[upper.tri(matv)])
# Calculate the recursive inverse of matv in a loop
invmatr <- invmat + matrix(rnorm(100, sd=0.1), nc=10)
iterv <- sapply(1:5, function(x) {
# Calculate the recursive inverse of matv
  invmatr <<- (2*invmatr - invmatr %*% matv %*% invmatr)
# Calculate the sum of the off-diagonal elements
  sum((invmatr %*% matv)[upper.tri(matv)])
})  # end sapply
# Plot the iterations
plot(x=1:5, y=iterv, t="l", xlab="iterations", ylab="error",
     main="Iterations of Recursive Matrix Inverse")
# Symbols for constant maturity Treasury rates
symbolv <- c("DGS1", "DGS2", "DGS5", "DGS10", "DGS20", "DGS30")
# Create new environment for time series
ratesenv <- new.env()
# Download time series for symbolv into ratesenv
quantmod::getSymbols(symbolv, env=ratesenv, src="FRED")
# List files in ratesenv
ls(ratesenv)
# Get class of all objects in ratesenv
sapply(ratesenv, class)
# Get class of all objects in R workspace
sapply(ls(), function(name) class(get(name)))
# Save the time series environment into a binary .RData file
save(ratesenv, file="/Users/jerzy/Develop/lecture_slides/data/rates_data.RData")
# Get class of time series object DGS10
class(get(x="DGS10", envir=ratesenv))
# Another way
class(ratesenv$DGS10)
# Get first 6 rows of time series
head(ratesenv$DGS10)
# Plot dygraphs of 10-year Treasury rate
dygraphs::dygraph(ratesenv$DGS10, main="10-year Treasury Rate") %>%
  dyOptions(colors="blue", strokeWidth=2)
# Plot 10-year constant maturity Treasury rate
x11(width=6, height=5)
par(mar=c(2, 2, 0, 0), oma=c(0, 0, 0, 0))
chart_Series(ratesenv$DGS10["1990/"], name="10-year Treasury Rate")
# Load constant maturity Treasury rates
load(file="/Users/jerzy/Develop/lecture_slides/data/rates_data.RData")
# Get most recent yield curve
ycnow <- eapply(ratesenv, xts::last)
class(ycnow)
ycnow <- do.call(cbind, ycnow)
# Check if 2020-03-25 is not a holiday
date2020 <- as.Date("2020-03-25")
weekdays(date2020)
# Get yield curve from 2020-03-25
yc2020 <- eapply(ratesenv, function(x) x[date2020])
yc2020 <- do.call(cbind, yc2020)
# Combine the yield curves
ycurves <- c(yc2020, ycnow)
# Rename columns and rows, sort columns, and transpose into matrix
colnames(ycurves) <- substr(colnames(ycurves), start=4, stop=11)
ycurves <- ycurves[, order(as.numeric(colnames(ycurves)))]
colnames(ycurves) <- paste0(colnames(ycurves), "yr")
ycurves <- t(ycurves)
colnames(ycurves) <- substr(colnames(ycurves), start=1, stop=4)
x11(width=6, height=5)
par(mar=c(3, 3, 2, 0), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
# Plot using matplot()
colorv <- c("blue", "red")
matplot(ycurves, main="Yield Curves in 2020 and 2023", xaxt="n", lwd=3, lty=1,
  type="l", xlab="maturity", ylab="yield", col=colorv)
# Add x-axis
axis(1, seq_along(rownames(ycurves)), rownames(ycurves))
# Add legend
legend("topleft", legend=colnames(ycurves), y.intersp=0.3,
 bty="n", col=colorv, lty=1, lwd=6, inset=0.05, cex=1.0)
x11(width=6, height=5)
par(mar=c(3, 3, 2, 0), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
# Load constant maturity Treasury rates
load(file="/Users/jerzy/Develop/lecture_slides/data/rates_data.RData")
# Get end-of-year dates since 2006
datev <- xts::endpoints(ratesenv$DGS1["2006/"], on="years")
datev <- zoo::index(ratesenv$DGS1["2006/"][datev])
# Create time series of end-of-year rates
ycurves <- eapply(ratesenv, function(ratev) ratev[datev])
ycurves <- rutils::do_call(cbind, ycurves)
# Rename columns and rows, sort columns, and transpose into matrix
colnames(ycurves) <- substr(colnames(ycurves), start=4, stop=11)
ycurves <- ycurves[, order(as.numeric(colnames(ycurves)))]
colnames(ycurves) <- paste0(colnames(ycurves), "yr")
ycurves <- t(ycurves)
colnames(ycurves) <- substr(colnames(ycurves), start=1, stop=4)
# Plot matrix using plot.zoo()
colorv <- colorRampPalette(c("red", "blue"))(NCOL(ycurves))
plot.zoo(ycurves, main="Yield Curve Since 2006", lwd=3, xaxt="n",
   plot.type="single", xlab="maturity", ylab="yield", col=colorv)
# Add x-axis
axis(1, seq_along(rownames(ycurves)), rownames(ycurves))
# Add legend
legend("topleft", legend=colnames(ycurves), y.intersp=0.1,
 bty="n", col=colorv, lty=1, lwd=4, inset=0.05, cex=0.8)
# Alternative plot using matplot()
matplot(ycurves, main="Yield curve since 2006", xaxt="n", lwd=3, lty=1,
  type="l", xlab="maturity", ylab="yield", col=colorv)
# Add x-axis
axis(1, seq_along(rownames(ycurves)), rownames(ycurves))
# Add legend
legend("topleft", legend=colnames(ycurves), y.intersp=0.1,
 bty="n", col=colorv, lty=1, lwd=4, inset=0.05, cex=0.8)
# Extract rates from ratesenv
symbolv <- c("DGS1", "DGS2", "DGS5", "DGS10", "DGS20")
ratem <- mget(symbolv, envir=ratesenv)
ratem <- rutils::do_call(cbind, ratem)
ratem <- zoo::na.locf(ratem, na.rm=FALSE)
ratem <- zoo::na.locf(ratem, fromLast=TRUE)
# Calculate daily percentage rates changes
retp <- rutils::diffit(log(ratem))
# Center (de-mean) the returns
retp <- lapply(retp, function(x) {x - mean(x)})
retp <- rutils::do_call(cbind, retp)
sapply(retp, mean)
# Covariance and Correlation matrices of Treasury rates
covmat <- cov(retp)
cormat <- cor(retp)
# Reorder correlation matrix based on clusters
library(corrplot)
ordern <- corrMatOrder(cormat, order="hclust",
  hclust.method="complete")
cormat <- cormat[ordern, ordern]
# Plot the correlation matrix
x11(width=6, height=6)
colorv <- colorRampPalette(c("red", "white", "blue"))
corrplot(cormat, title=NA, tl.col="black",
    method="square", col=colorv(NCOL(cormat)), tl.cex=0.8,
    cl.offset=0.75, cl.cex=0.7, cl.align.text="l", cl.ratio=0.25)
title("Correlation of Treasury Rates", line=1)
# Draw rectangles on the correlation matrix plot
corrRect.hclust(cormat, k=NROW(cormat) %/% 2,
  method="complete", col="red")
# Create initial vector of portfolio weights
nweights <- NROW(symbolv)
weightv <- rep(1/sqrt(nweights), nweights)
names(weightv) <- symbolv
# Objective function equal to minus portfolio variance
objfun <- function(weightv, retp) {
  retp <- retp %*% weightv
  -1e7*var(retp) + 1e7*(1 - sum(weightv*weightv))^2
}  # end objfun
# Objective function for equal weight portfolio
objfun(weightv, retp)
# Compare speed of vector multiplication methods
library(microbenchmark)
summary(microbenchmark(
  transp=t(retp) %*% retp,
  sumv=sum(retp*retp),
  times=10))[, c(1, 4, 5)]
# Find weights with maximum variance
optiml <- optim(par=weightv,
  fn=objfun,
  retp=retp,
  method="L-BFGS-B",
  upper=rep(5.0, nweights),
  lower=rep(-5.0, nweights))
# Optimal weights and maximum variance
weights1 <- optiml$par
objfun(weights1, retp)
# Plot first principal component loadings
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
barplot(weights1, names.arg=names(weights1),
  xlab="", ylab="", main="First Principal Component Loadings")
# pc1 weights and returns
pc1 <- drop(retp %*% weights1)
# Redefine objective function
objfun <- function(weightv, retp) {
  retp <- retp %*% weightv
  -1e7*var(retp) + 1e7*(1 - sum(weightv^2))^2 +
    1e7*sum(weights1*weightv)^2
}  # end objfun
# Find second principal component weights
optiml <- optim(par=weightv,
             fn=objfun,
             retp=retp,
             method="L-BFGS-B",
             upper=rep(5.0, nweights),
             lower=rep(-5.0, nweights))
# pc2 weights and returns
weights2 <- optiml$par
pc2 <- drop(retp %*% weights2)
sum(pc1*pc2)
# Plot second principal component loadings
barplot(weights2, names.arg=names(weights2),
  xlab="", ylab="", main="Second Principal Component Loadings")
eigend <- eigen(covmat)
eigend$vectors
# Compare with optimization
all.equal(sum(diag(covmat)), sum(eigend$values))
all.equal(abs(eigend$vectors[, 1]), abs(weights1), check.attributes=FALSE)
all.equal(abs(eigend$vectors[, 2]), abs(weights2), check.attributes=FALSE)
all.equal(eigend$values[1], var(pc1), check.attributes=FALSE)
all.equal(eigend$values[2], var(pc2), check.attributes=FALSE)
# Eigenvalue equations are satisfied approximately
(covmat %*% weights1) / weights1 / var(pc1)
(covmat %*% weights2) / weights2 / var(pc2)
# Plot eigenvalues
barplot(eigend$values, names.arg=paste0("PC", 1:nweights),
  las=3, xlab="", ylab="", main="Principal Component Variances")
# Eigen decomposition of correlation matrix
eigend <- eigen(cormat)
# Perform PCA with scaling
pcad <- prcomp(retp, scale=TRUE)
# Compare outputs
all.equal(eigend$values, pcad$sdev^2)
all.equal(abs(eigend$vectors), abs(pcad$rotation),
    check.attributes=FALSE)
# Eigen decomposition of covariance matrix
eigend <- eigen(covmat)
# Perform PCA without scaling
pcad <- prcomp(retp, scale=FALSE)
# Compare outputs
all.equal(eigend$values, pcad$sdev^2)
all.equal(abs(eigend$vectors), abs(pcad$rotation),
    check.attributes=FALSE)
# Perform principal component analysis PCA
pcad <- prcomp(retp, scale=TRUE)
# Plot standard deviations
barplot(pcad$sdev, names.arg=colnames(pcad$rotation),
  las=3, xlab="", ylab="",
  main="Scree Plot: Volatilities of Principal Components
  of Treasury rates")
x11(width=6, height=7)
# Calculate principal component loadings (weights)
pcad$rotation
# Plot loading barplots in multiple panels
par(mfrow=c(3,2))
par(mar=c(3.5, 2, 2, 1), oma=c(0, 0, 0, 0))
for (ordern in 1:NCOL(pcad$rotation)) {
  barplot(pcad$rotation[, ordern], las=3, xlab="", ylab="", main="")
  title(paste0("PC", ordern), line=-2.0, col.main="red")
}  # end for
# Standardize (center and scale) the returns
retp <- lapply(retp, function(x) {(x - mean(x))/sd(x)})
retp <- rutils::do_call(cbind, retp)
sapply(retp, mean)
sapply(retp, sd)
# Calculate principal component time series
retpcac <- retp %*% pcad$rotation
all.equal(pcad$x, retpcac, check.attributes=FALSE)
# Calculate products of principal component time series
round(t(retpcac) %*% retpcac, 2)
# Coerce to xts time series
retpcac <- xts(retpcac, order.by=zoo::index(retp))
retpcac <- cumsum(retpcac)
# Plot principal component time series in multiple panels
par(mfrow=c(3,2))
par(mar=c(2, 2, 0, 1), oma=c(0, 0, 0, 0))
rangev <- range(retpcac)
for (ordern in 1:NCOL(retpcac)) {
  plot.zoo(retpcac[, ordern], ylim=rangev, xlab="", ylab="")
  title(paste0("PC", ordern), line=-1, col.main="red")
}  # end for
# Invert all the principal component time series
retpca <- retp %*% pcad$rotation
solved <- retpca %*% solve(pcad$rotation)
all.equal(coredata(retp), solved)
# Invert first 3 principal component time series
solved <- retpca[, 1:3] %*% solve(pcad$rotation)[1:3, ]
solved <- xts::xts(solved, zoo::index(retp))
solved <- cumsum(solved)
retc <- cumsum(retp)
# Plot the solved returns
par(mfrow=c(3,2))
par(mar=c(2, 2, 0, 1), oma=c(0, 0, 0, 0))
for (symbol in symbolv) {
  plot.zoo(cbind(retc[, symbol], solved[, symbol]),
    plot.type="single", col=c("black", "blue"), xlab="", ylab="")
  legend(x="topleft", bty="n", y.intersp=0.1,
   legend=paste0(symboln, c("", " solved")),
   title=NULL, inset=0.0, cex=1.0, lwd=6,
   lty=1, col=c("black", "blue"))
}  # end for
library(quantmod)  # Load quantmod
library(RQuantLib)  # Load RQuantLib
# Specify curve parameters
curvep <- list(tradeDate=as.Date("2018-01-17"),
         settleDate=as.Date("2018-01-19"),
         dt=0.25,
         interpWhat="discount",
         interpHow="loglinear")
# Specify market data: prices of FI instruments
pricev <- list(d3m=0.0363,
         fut1=96.2875,
         fut2=96.7875,
         fut3=96.9875,
         fut4=96.6875,
         s5y=0.0443,
         s10y=0.05165,
         s15y=0.055175)
# Specify dates for calculating the zero rates
datev <- seq(0, 10, 0.25)
# Specify the evaluation (as of) date
setEvaluationDate(as.Date("2018-01-17"))
# Calculate the zero rates
ratev <- DiscountCurve(params=curvep, tsQuotes=pricev, times=datev)
# Plot the zero rates
x11()
plot(x=ratev$zerorates, t="l", main="zerorates")
