
library(rgl)
knit_hooks$set(rgl=hook_rgl)
knit_hooks$set(webgl=hook_webgl)






library(quantmod)
car_s <- mtcars[sample(NROW(mtcars), 10), ]
# Plot scatter plot horsepower vs miles per gallon
plot(car_s[, "hp"], car_s[, "mpg"],
     xlab="horsepower", ylab="miles per gallon",
     main="miles per gallon vs horsepower")
# Add a solid red point (pch=16) for the last car
points(x=car_s[NROW(car_s), "hp"],
 y=car_s[NROW(car_s), "mpg"],
 col="red", pch=16)
# Add labels with the car names
text(x=car_s[, "hp"], y=car_s[, "mpg"],
     labels=rownames(car_s[, ]),
     pos=1, cex=0.8)
# Labels using wordcloud, to prevent overlaps
library(wordcloud)
textplot(x=car_s[, "hp"], y=car_s[, "mpg"],
   words=rownames(car_s))

# Plot the tree Height
plot(trees[, "Height"],
     type="l",
     lwd=2,
     col="blue",
     main="Tree heights and volumes",
     xlab="tree number", ylab="",
     ylim=c(min(trees[, c("Height", "Volume")]),
      max(trees[, c("Height", "Volume")])))
# Plot the tree Volume
lines(trees[, "Volume"], lwd=2, col="green")
# Add legend
legend(x="left", legend=c("Height", "Volume"),
 inset=0.1, cex=1.0, bg="white", bty="n",
 lwd=2, lty=1, col=c("blue", "green"))

xvar <- seq(-2*pi, 2*pi, len=100)  # x values

# open Windows graphics device
x11(width=11, height=7, title="simple plot")

# Plot a sine function using basic line plot
plot(x=xvar, y=sin(xvar), xlab="x-val",
     ylab="y-val", type="l", lwd=2, col="red")
# Add a cosine function
lines(x=xvar, y=cos(xvar), lwd=2, col="blue")
# Add title
title(main="sine and cosine functions", line=0.1)
# Add legend
legend(x="topright", legend=c("sine", "cosine"),
 title="legend", inset=0.1, cex=1.0, bg="white",
 lwd=2, lty=1, bty="n", col=c("red", "blue"))
graphics.off()  # Close all graphics devices

par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# Plot a Normal probability distribution
curve(expr=dnorm, xlim=c(-3, 3),
xlab="", ylab="", lwd=2, col="blue")
# Add shifted Normal probability distribution
curve(expr=dnorm(x, mean=1), add=TRUE,lwd=2, col="red")

# Add title
title(main="Normal probability distribution functions",
line=0.1)
# Add legend
legend(x="topright", legend=c("Normal", "shifted"),
 title="legend", inset=0.05, cex=0.8, bg="white",
 lwd=2, lty=1, bty="n", col=c("blue", "red"))

par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0))
library(zoo)  # Load zoo
load(file="/Users/jerzy/Develop/lecture_slides/data/zoo_data.RData")
zoo_series <- window(zoo_stx[, "AdjClose"],
   start=as.Date("2013-01-01"),
   end=as.Date("2013-12-31"))
# extract time index and monthly dates
dates <- zoo::index(zoo_series)
# Coerce index to monthly dates
month_ly <- as.yearmon(dates)
# tick locations at beginning of month
tick_s <- dates[match(unique(month_ly), month_ly)]
# tick_s <- as.Date(tapply(X=dates, INDEX=month_ly, FUN=min))
# first plot zoo without "x" axis
plot(zoo_series, xaxt="n", xlab=NA, ylab=NA, main="MSFT stock prices")
# Add "x" axis with monthly ticks
axis(side=1, at=tick_s, labels=format(tick_s, "%b-%y"), tcl=-0.7)
# Add vertical lines
abline(v=tick_s, col="grey", lwd=0.5)
# Plot zoo using base plotting functions
plot(as.vector(zoo_series), xaxt="n",
 xlab=NA, ylab=NA, t="l", main="MSFT stock prices")
a_t <- match(tick_s, dates)
# a_t <- seq_along(dates)[dates %in% tick_s]
# Add "x" axis with monthly ticks
axis(side=1, at=a_t, labels=format(tick_s, "%b-%y"), tcl=-0.7)
abline(v=a_t, col="grey", lwd=0.5)

library(zoo)  # Load zoo
load(file="/Users/jerzy/Develop/lecture_slides/data/zoo_data.RData")
# extract time index and monthly dates
dates <- zoo::index(zoo_stx)
# Coerce index to monthly dates
month_ly <- as.yearmon(dates)
# benchmark two methods of calculating tick locations
library(microbenchmark)
summary(microbenchmark(
m_atch=dates[match(unique(month_ly), month_ly)],
t_apply=as.Date(tapply(X=dates,
    INDEX=month_ly, FUN=min)),
times=10)
  )[, c(1, 4, 5)]

load(file="/Users/jerzy/Develop/lecture_slides/data/zoo_data.RData")
# Set plot margines
par(mar=c(3, 3, 3, 3), oma=c(0, 0, 0, 0))
par(las=1)  # Set text printing to horizontal
Plot with two y-axes - plot first time series
zoo::plot.zoo(zoo_stxeur[, 1], lwd=2, xlab=NA, ylab=NA)
par(new=TRUE)  # Allow new plot on same chart
# Plot second time series without y-axis
zoo::plot.zoo(zoo_stxeur[, 2], xlab=NA, ylab=NA,
     lwd=2, yaxt="n", col="red")
# Plot second y-axis on right
axis(side=4, col="red")
# Add axis labels
colnamev <- colnames(zoo_stxeur)
mtext(colnamev[1], side=2, adj=-0.5)
mtext(colnamev[2], side=4, adj=1.5, col="red")
# Add title and legend
title(main=paste0(colnamev, collapse=" and "),
line=0.5)
legend("top", legend=colnamev,
  bg="white", lty=1, lwd=6,
  col=c("black", "red"), bty="n")

Slightly different method using par("usr")
par(las=1)  # Set text printing to horizontal
zoo::plot.zoo(zoo_stxeur[, 1], xlab=NA, ylab=NA, lwd=2)
# Set range of "y" coordinates for second axis
par(usr=c(par("usr")[1:2], range(zoo_stxeur[,2])))
lines(zoo_stxeur[, 2], col="red", lwd=2)  # Second plot
axis(side=4, col="red")  # Second y-axis on right
# Add axis labels
mtext(colnamev[1], side=2, adj=-0.5)
mtext(colnamev[2], side=4, adj=1.5, col="red")
# Add title and legend
title(main=paste0(colnamev, collapse=" and "),
line=0.5)
legend("top", legend=colnamev,
  bg="white", lty=1, lwd=6,
  col=c("black", "red"), bty="n")

graph_params <- par()  # get existing parameters
par("mar")  # get plot margins
par(mar=c(2, 1, 2, 1))  # Set plot margins
par(oma=c(1, 1, 1, 1))  # Set outer margins
par(mgp=c(2, 1, 0))  # Set title and label margins
par(cex.lab=0.8,  # Set font scales
    cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(las=1)  # Set axis labels to horizontal
par(ask=TRUE)  # Pause, ask before plotting
par(mfrow=c(2, 2))  # Plot on 2x2 grid by rows
for (i in 1:4) {  # Plot 4 panels
  barplot(sample(1:6), main=paste("panel", i),
    col=rainbow(6), border=NA, axes=FALSE)
  box()
}  # end for
par(ask=FALSE)  # Restore automatic plotting
par(new=TRUE)  # Allow new plot on same chart
par(graph_params)  # Restore original parameters

xvar <- seq(-5, 7, length=100)
yvar <- dnorm(xvar, mean=1.0, sd=2.0)
plot(xvar, yvar, type="l", lty="solid", xlab="", ylab="")
title(main="Normal Density Function", line=0.5)
startd <- 3; fin_ish <- 5  # Set lower and upper bounds
# Set polygon base
are_a <- ((xvar >= startd) & (xvar <= fin_ish))
polygon(c(startd, xvar[are_a], fin_ish),  # Draw polygon
  c(-1, yvar[are_a], -1), col="red")

par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
sigmavs <- c(0.5, 1, 1.5, 2)  # Sigma values
# Create plot colors
colors <- c("red", "black", "blue", "green")
# Create legend labels
labelv <- paste("sigma", sigmavs, sep="=")
for (indeks in 1:4) {  # Plot four curves
  curve(expr=dnorm(x, sd=sigmavs[indeks]),
  xlim=c(-4, 4), xlab="", ylab="", lwd=2,
  col=colors[indeks], add=as.logical(indeks-1))
}  # end for
# Add title
title(main="Normal Distributions", line=0.5)
# Add legend
legend("topright", inset=0.05, title="Sigmas",
 labelv, cex=0.8, lwd=2, lty=1, bty="n", col=colors)

rm(list=ls())
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
xvar <- seq(-4, 4, length=100)
sigmavs <- c(0.5, 1, 1.5, 2)  # Sigma values
# Create plot colors
colors <- c("red", "black", "blue", "green")
# Create legend labels
labelv <- paste("sigma", sigmavs, sep="=")
# Plot the first chart
plot(xvar, dnorm(xvar, sd=sigmavs[1]),
     type="n", xlab="", ylab="", main="Normal Distributions")
# Add lines to plot
for (indeks in 1:4) {
  lines(xvar, dnorm(xvar, sd=sigmavs[indeks]),
  lwd=2, col=colors[indeks])
}  # end for
# Add legend
legend("topright", inset=0.05, title="Sigmas",
 labelv, cex=0.8, lwd=2, lty=1, bty="n", col=colors)

# Standard deviations of log-normal distribution
sigmavs <- c(0.5, 1, 1.5)
# Create plot colors
colors <- c("black", "red", "blue")
# Plot all curves
for (indeks in 1:NROW(sigmavs)) {
  curve(expr=dlnorm(x, sdlog=sigmavs[indeks]),
  type="l", xlim=c(0, 3), lwd=2,
  xlab="", ylab="", col=colors[indeks],
  add=as.logical(indeks-1))
}  # end for

# Add title and legend
title(main="Log-normal Distributions", line=0.5)
legend("topright", inset=0.05, title="Sigmas",
 paste("sigma", sigmavs, sep="="),
 cex=0.8, lwd=2, lty=rep(1, NROW(sigmavs)), col=colors)

x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
# Degrees of freedom
degf <- c(2, 5, 8, 11)
# Plot four curves in loop
colors <- c("red", "black", "blue", "green")
for (indeks in 1:4) {
  curve(expr=dchisq(x, df=degf[indeks]),
  xlim=c(0, 20), ylim=c(0, 0.3),
  xlab="", ylab="", col=colors[indeks],
  lwd=2, add=as.logical(indeks-1))
}  # end for

# Add title
title(main="Chi-squared Distributions", line=0.5)
# Add legend
labelv <- paste("df", degf, sep="=")
legend("topright", inset=0.05, bty="n",
       title="Degrees of freedom", labelv,
       cex=0.8, lwd=6, lty=1,
       col=colors)

x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
degf <- c(2, 5, 8, 11)  # df values
# Create plot colors
colors <- c("red", "black", "blue", "green")
# Create legend labels
labelv <- paste("df", degf, sep="=")
# Plot an empty chart
xvar <- seq(0, 20, length=100)
plot(xvar, dchisq(xvar, df=degf[1]),
     type="n", xlab="", ylab="", ylim=c(0, 0.3))
# Add lines to plot
for (indeks in 1:4) {
  lines(xvar, dchisq(xvar, df=degf[indeks]),
lwd=2, col=colors[indeks])
}  # end for

# Add title
title(main="Chi-squared Distributions", line=0.5)
# Add legend
legend("topright", inset=0.05,
       title="Degrees of freedom", labelv,
       cex=0.8, lwd=6, lty=1, bty="n", col=colors)

x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
# Plot three curves in loop
degf <- c(3, 5, 9)  # Degrees of freedom
colors <- c("black", "red", "blue", "green")
for (indeks in 1:NROW(degf)) {
curve(expr=df(x, df1=degf[indeks], df2=3),
xlim=c(0, 4), xlab="", ylab="", lwd=2,
col=colors[indeks], add=as.logical(indeks-1))
}  # end for

# Add title
title(main="F-Distributions", line=0.5)
# Add legend
labelv <- paste("df", degf, sep="=")
legend("topright", inset=0.05, title="degrees of freedom",
       labelv, cex=0.8, lwd=2, lty=1, col=colors)

x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
degf <- c(3, 6, 9)  # df values
colors <- c("black", "red", "blue", "green")
labelv <- c("normal", paste("df", degf, sep="="))
# Plot a Normal probability distribution
curve(expr=dnorm, xlim=c(-4, 4), xlab="", ylab="", lwd=2)
for (indeks in 1:3) {  # Plot three t-distributions
  curve(expr=dt(x, df=degf[indeks]),
lwd=2, col=colors[indeks+1], add=TRUE)
}  # end for

# Add title
title(main="t-distributions", line=0.5)
# Add legend
legend("topright", inset=0.05, bty="n",
       title="Degrees\n of freedom", labelv,
       cex=0.8, lwd=6, lty=1, col=colors)

x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
xvar <- seq(-4, 4, length=100)
degf <- c(3, 6, 9)  # df values
colors <- c("black", "red", "blue", "green")
labelv <- c("normal", paste("df", degf, sep="="))
# Plot chart of normal distribution
plot(xvar, dnorm(xvar), type="l", lwd=2, xlab="", ylab="")
for (indeks in 1:3) {  # Add lines for t-distributions
  lines(xvar, dt(xvar, df=degf[indeks]),
lwd=2, col=colors[indeks+1])
}  # end for

# Add title
title(main="t-distributions", line=0.5)
# Add legend
legend("topright", inset=0.05, bty="n",
       title="Degrees\n of freedom", labelv,
       cex=0.8, lwd=6, lty=1, col=colors)

x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
# Plot the Normal and Cauchy probability distributions
curve(expr=dnorm, xlim=c(-4, 4), xlab="", ylab="", lwd=2)
curve(expr=dcauchy, lwd=3, col="blue", add=TRUE)
# Add title
title(main="Cauchy and Normal Distributions", line=0.5)
# Add legend
legend("topright", inset=0.05, bty="n",
       title=NULL,leg=c("Normal", "Cauchy"),
       cex=0.8, lwd=6, lty=1, col=c("black", "blue"))

x11(width=6, height=5)  # Plot in window
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
# Define Pareto function
pare_to <- function(x, alpha) alpha*x^(-alpha-1)
colors <- c("red", "blue", "green")
alpha_s <- c(1.0, 2.0, 3.0)
for (indeks in 1:3) {  # Plot three curves
  curve(expr=pare_to(x, alpha_s[indeks]),
  xlim=c(1, 2), ylim=c(0.0, 3.5), xlab="", ylab="",
  lwd=3, col=colors[indeks], add=as.logical(indeks-1))
}  # end for
# Add title and legend
title(main="Pareto Distributions", line=0.5)
labelv <- paste("alpha", 1:3, sep=" = ")
legend("topright", inset=0.2, bty="n",
 title=NULL, labelv, cex=0.8, lwd=6, lty=1,
 col=colors)

rm(list=ls())
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
poisson_events <- 0:11  # Poisson events
poisson_freq <- dpois(poisson_events, lambda=4)
names(poisson_freq) <- as.character(poisson_events)
# Poisson function
poissonfun <- function(x, lambda)
              {exp(-lambda)*lambda^x/factorial(x)}
curve(expr=poissonfun(x, lambda=4), xlim=c(0, 11), main="Poisson distribution",
xlab="No. of events", ylab="Frequency of events", lwd=2, col="red")
legend(x="topright", legend="Poisson density", title="", bty="n",
 inset=0.05, cex=0.8, bg="white", lwd=4, lty=1, col="red")

# generate Poisson variables
pois_counts <- rpois(1000, lambda=4)
head(pois_counts)
# Calculate contingency table
pois_table <- table(pois_counts)
pois_table

# Create barplot of table data
barplot(pois_table, col="lightgrey",
  xlab="counts", ylab="number of observations",
  main="barplot of Poisson count data")

# Create histogram of Poisson variables
histp <- hist(pois_counts, col="lightgrey", xlab="count",
     ylab="frequency", freq=FALSE, main="Poisson histogram")
lines(density(pois_counts, adjust=1.5), lwd=2, col="blue")
# Poisson probability distribution function
poissonfun <- function(x, lambda)
  {exp(-lambda)*lambda^x/factorial(x)}
curve(expr=poissonfun(x, lambda=4), xlim=c(0, 11), add=TRUE, lwd=2, col="red")
# Add legend
legend("topright", inset=0.05, title="Poisson histogram",
 c("histogram density", "probability"), cex=0.8, lwd=2,
 lty=1, bty="n", col=c("blue", "red"))
# total area under histogram
diff(histp$breaks) %*% histp$density

# boxplot of Poisson count data
boxplot(x=pois_counts, ylab="counts",
  main="Poisson box plot")
# boxplot method for formula
boxplot(formula=mpg ~ cyl, data=mtcars,
  main="Mileage by number of cylinders",
  xlab="Cylinders", ylab="Miles per gallon")

# Create a plotting expression
ex_pr <- quote({
  par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
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
ex_pr
# Create plot by evaluating the plotting expression
x11(width=6, height=4)
eval(ex_pr)

library(animation)
# Create an expression for creating multiple plots
ex_pr <- quote({
  par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
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
eval(ex_pr)
# Create gif with animated plot
animation::saveGIF(expr=eval(ex_pr),
  movie.name="chi_squared.gif",
  img.name="chi_squared")
# Create html with animated plot
animation::saveHTML(expr=eval(ex_pr),
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
    column(width=3, sliderInput("look_back", label="Lookback interval",
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
    volumes <- quantmod::Vo(ohlc)
    # Return the data
    cbind(closep, volumes)
  })  # end reactive code

  # Calculate the VWAP indicator in a reactive environment
  vwapv <- shiny::reactive({
    # Get model parameters from input argument
    look_back <- input$look_back
    # Calculate the VWAP indicator
    closep <- closep()[, 1]
    volumes <- closep()[, 2]
    vwapv <- HighFreq::roll_sum(se_ries=closep*volumes, look_back=look_back)
    volume_rolling <- HighFreq::roll_sum(se_ries=volumes, look_back=look_back)
    vwapv <- vwapv/volume_rolling
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

# R startup chunk
# ```{r setup, include=FALSE}
library(shiny)
library(quantmod)
interval <- 31
closep  <- quantmod::Cl(rutils::etfenv$VTI)
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue")
# ```
#end R startup chunk
inputPanel(
  sliderInput("lambda", label="lambda:",
    min=0.01, max=0.2, value=0.1, step=0.01)
)  # end inputPanel

renderPlot({
  # Calculate EWMA prices
  lambda <- input$lambda
  weights <- exp(-lambda*1:interval)
  weights <- weights/sum(weights)
  ewmap <- .Call(stats:::C_cfilter, closep, filter=weights, sides=1, circular=FALSE)
  ewmap[1:(interval-1)] <- ewmap[interval]
  ewmap <- xts(cbind(closep , ewmap), order.by=zoo::index(closep ))
  colnames(ewmap) <- c("VTI", "VTI EWMA")
  # Plot EWMA prices
  chobj <- chart_Series(ewmap, theme=plot_theme, name="EWMA prices")
  plot(chobj)
  legend("top", legend=colnames(ewmap),
   inset=0.1, bg="white", lty=1, lwd=2,
   col=plot_theme$col$line.col, bty="n")
})  # end renderPlot

library(zoo)  # Load zoo
library(ggplot2)  # Load ggplot2
library(scales)  # Load scales
my_ggplot <- ggplot(  # Specify data and aesthetics
  data=mtcars, mapping=aes(x=hp, y=mpg)) +
  geom_point() +  # Plot points
  ggtitle("basic scatter plot") +  # Add title
  theme(  # Customize plot object
  plot.title=element_text(vjust=-2.0),
  plot.background=element_blank()
  )  # end theme
my_ggplot  # Render the plot

# install.packages("directlabels", repo="http://r-forge.r-project.org")
library(ggplot2)  # Load ggplot2
library(scales)  # Load scales
library(gridExtra)  # Load gridExtra
library(directlabels)  # Load directlabels
my_ggplot <- ggplot(  # Data and aesthetics
  data=mtcars, mapping=aes(x=hp, y=mpg)) +
  geom_point() +  # Plot points
  theme(  # Customize plot object
  legend.position="none",
  plot.title=element_text(vjust=-2.0),
  plot.margin=unit(c(-0.0,0.0,-0.5,0.0),"cm"),
  plot.background=element_blank()
  ) +
  scale_colour_discrete(guide="none")  # no label guide
car_names <- rownames(mtcars)
gg_labels <- geom_text(aes(  # ggplot2 labels
  label=car_names, color=car_names, size=5))
d_labels <- geom_dl(mapping=aes(  # Directlabels
  label=car_names, color=car_names),
  method=list("last.bumpup", cex=0.7,
        hjust=1))
# Render plots in single column
grid.arrange(my_ggplot +
  ggtitle("ggplot2 labels") + gg_labels,
  my_ggplot + ggtitle("directlabels") +
    d_labels, ncol=1)  # end grid.arrange

my_ggplot <- ggplot(data=iris,
      mapping=aes(Petal.Length, Sepal.Length)) +
  geom_point(aes(shape=Species, color=Species)) +
  geom_dl(aes(label=Species, color=Species),
    method="smart.grid") +
  scale_shape_manual(values=c(setosa=1,
    virginica=6, versicolor=3), guide="none") +
  scale_colour_discrete(guide="none")  # no label guide
my_ggplot  # Render the plot

library(ggplot2)  # Load ggplot2
library(scales)  # Load scales
library(gridExtra)  # Load gridExtra
# Coerce mts object into zoo
zoo_series <- as.zoo(EuStockMarkets)
# Create ggplot2 theme object
auto_theme <- theme(
  legend.position="none",
  plot.title=element_text(vjust=-2.0),
  plot.margin=unit(c(-0.0,0.0,-0.5,0.0),"cm"),
#  axis.text.y=element_blank(),
  plot.background=element_blank()
  )  # end theme
# ggplot2 object for plotting in single panel
ggp_zoo_single <- autoplot(zoo_series,
            main="Eu Stox single panel",
            facets=NULL) + xlab("") +
            auto_theme
# ggplot2 object for plotting in multiple panels
ggp_zoo_multiple <- autoplot(zoo_series,
            main="Eu Stox multiple panels",
            facets="Series ~ .") + xlab("") +
            facet_grid("Series ~ .",
            scales="free_y") + auto_theme
# Render plots in single column
grid.arrange(ggp_zoo_single +
         theme(legend.position=c(0.1, 0.5)),
       ggp_zoo_multiple, ncol=1)

library(zoo)  # Load zoo
library(ggplot2)  # Load ggplot2
library(gridExtra)
#
auto_theme <- theme(legend.position=c(0.1, 0.5),
  plot.title=element_text(vjust=-2.0),
  plot.margin=unit(c(-0.5, 0.0, -0.5, 0.0), "cm"),
  plot.background=element_blank(),
  axis.text.y=element_blank()
  )

# Plot ggplot2 in single pane
ggp.zoo1 <- autoplot(zoo_series, main="Eu Stox",
   facets=NULL) + xlab("") +
  theme(legend.position=c(0.1, 0.5),
  plot.title=element_text(vjust=-2.0),
  plot.margin=unit(c(-0.5, 0.0, -0.5, 0.0), "cm"),
  plot.background=element_blank(),
  axis.text.y=element_blank()
  )
# Plot ggplot2 in multiple panes
ggp.zoo2 <- autoplot(zoo_series, main="Eu Stox",
   facets=Series ~ .) + xlab("") +
  theme(legend.position=c(0.1, 0.5),
  plot.title=element_text(vjust=-2.0),
  plot.margin=unit(c(-0.5, 0.0, -0.5, 0.0), "cm"),
  plot.background=element_blank(),
  axis.text.y=element_blank()
  )
# Create plot ggplot2 in multiple panes
grid.arrange(ggp.zoo1, ggp.zoo2, ncol=1)

# Define function of two variables
sur_face <- function(x, y) sin(sqrt(x^2+y^2))
# Calculate function over matrix grid
xlim <- seq(from=-10, to=10, by=0.2)
ylim <- seq(from=-10, to=10, by=0.2)
# Draw 3d surface plot of function
persp(z=outer(xlim, ylim, FUN=sur_face),
theta=45, phi=30, zlab="sine",
shade=0.1, col="green",
main="radial sine function")

# Set rgl options
options(rgl.useNULL=TRUE)
# Load package rgl
library(rgl)
# Create 3d scatter plot of function
with(iris, rgl::plot3d(Sepal.Length, Sepal.Width, Petal.Length,
            type="s", col=as.numeric(Species)))
# Render the 3d scatter plot of function
rgl::rglwidget(elementId="plot3drgl", width=1000, height=1000)

library(rgl)  # Load rgl
# Define function of two variables
sur_face <- function(x, y) y*sin(x)
# Create 3d surface plot of function
rgl::persp3d(x=sur_face, xlim=c(-5, 5), ylim=c(-5, 5),
  col="green", axes=FALSE)
# Render the 3d surface plot of function
rgl::rglwidget(elementId="surfacergl", width=1000, height=1000)
# Draw 3d surface plot of matrix
xlim <- seq(from=-5, to=5, by=0.1)
ylim <- seq(from=-5, to=5, by=0.1)
rgl::persp3d(z=outer(xlim, ylim, FUN=sur_face),
  xlab="x", ylab="y", zlab="sur_face", col="green")
# Save current view to png file
rgl::rgl.snapshot("surface_plot.png")
# Define function of two variables and two parameters
sur_face <- function(x, y, lambda1=1, lambda2=1)
  sin(lambda1*x)*sin(lambda2*y)
# Draw 3d surface plot of function
rgl::persp3d(x=sur_face, xlim=c(-5, 5), ylim=c(-5, 5),
  col="green", axes=FALSE, lambda1=1, lambda2=2)
