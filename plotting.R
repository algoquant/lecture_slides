library(knitr)
library(rgl)
knit_hooks$set(rgl=hook_rgl)
knit_hooks$set(webgl=hook_webgl)
opts_chunk$set(prompt=TRUE, eval=FALSE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size="scriptsize", fig.width=4, fig.height=4)
options(width=60, dev="pdf")
options(digits=3)
thm <- knit_theme$get("acid")
knit_theme$set(thm)

library(quantmod)
car_s <- mtcars[sample(NROW(mtcars), 10), ]
# Plot scatterplot horsepower vs miles per gallon
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

x_var <- seq(-2*pi, 2*pi, len=100)  # x values

# open Windows graphics device
x11(width=11, height=7, title="simple plot")

# Plot a sine function using basic line plot
plot(x=x_var, y=sin(x_var), xlab="x-val",
     ylab="y-val", type="l", lwd=2, col="red")
# Add a cosine function
lines(x=x_var, y=cos(x_var), lwd=2, col="blue")
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
curve(expr=dnorm(x, mean=1), add=TRUE,
lwd=2, col="red")

# Add title
title(main="Normal probability distribution functions",
line=0.1)
# Add legend
legend(x="topright", legend=c("Normal", "shifted"),
 title="legend", inset=0.05, cex=0.8, bg="white",
 lwd=2, lty=1, bty="n", col=c("blue", "red"))

par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0))
library(zoo)  # Load zoo
load(file="C:/Develop/lecture_slides/data/zoo_data.RData")
zoo_series <- window(zoo_stx[, "AdjClose"],
   start=as.Date("2013-01-01"),
   end=as.Date("2013-12-31"))
# extract time index and monthly dates
in_dex <- index(zoo_series)
# Coerce index to monthly dates
month_ly <- as.yearmon(in_dex)
# tick locations at beginning of month
tick_s <- in_dex[match(unique(month_ly), month_ly)]
# tick_s <- as.Date(tapply(X=in_dex, INDEX=month_ly, FUN=min))
# first plot zoo without "x" axis
plot(zoo_series, xaxt="n", xlab=NA, ylab=NA, main="MSFT stock prices")
# Add "x" axis with monthly ticks
axis(side=1, at=tick_s,
 labels=format(tick_s, "%b-%y"), tcl=-0.7)
# Add vertical lines
abline(v=tick_s, col="grey", lwd=0.5)
# Plot zoo using base plotting functions
plot(as.vector(zoo_series), xaxt="n",
 xlab=NA, ylab=NA, t="l", main="MSFT stock prices")
a_t <- match(tick_s, in_dex)
# a_t <- seq_along(in_dex)[in_dex %in% tick_s]
# Add "x" axis with monthly ticks
axis(side=1, at=a_t,
 labels=format(tick_s, "%b-%y"), tcl=-0.7)
abline(v=a_t, col="grey", lwd=0.5)

library(zoo)  # Load zoo
load(file="C:/Develop/lecture_slides/data/zoo_data.RData")
# extract time index and monthly dates
in_dex <- index(zoo_stx)
# Coerce index to monthly dates
month_ly <- as.yearmon(in_dex)
# benchmark two methods of calculating tick locations
library(microbenchmark)
summary(microbenchmark(
m_atch=
  in_dex[match(unique(month_ly), month_ly)],
t_apply=
  as.Date(tapply(X=in_dex,
                 INDEX=month_ly, FUN=min)),
times=10)
  )[, c(1, 4, 5)]

load(file="C:/Develop/lecture_slides/data/zoo_data.RData")
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
col_names <- colnames(zoo_stxeur)
mtext(col_names[1], side=2, adj=-0.5)
mtext(col_names[2], side=4, adj=1.5, col="red")
# Add title and legend
title(main=paste0(col_names, collapse=" and "),
line=0.5)
legend("top", legend=col_names,
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
mtext(col_names[1], side=2, adj=-0.5)
mtext(col_names[2], side=4, adj=1.5, col="red")
# Add title and legend
title(main=paste0(col_names, collapse=" and "),
line=0.5)
legend("top", legend=col_names,
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

x_var <- seq(-5, 7, length=100)
y_var <- dnorm(x_var, mean=1.0, sd=2.0)
plot(x_var, y_var, type="l", lty="solid",
     xlab="", ylab="")
title(main="Normal Density Function", line=0.5)
star_t <- 3; fin_ish <- 5  # Set lower and upper bounds
# Set polygon base
are_a <- ((x_var >= star_t) & (x_var <= fin_ish))
polygon(c(star_t, x_var[are_a], fin_ish),  # Draw polygon
  c(-1, y_var[are_a], -1), col="red")

par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
sig_mas <- c(0.5, 1, 1.5, 2)  # Sigma values
# Create plot colors
col_ors <- c("red", "black", "blue", "green")
# Create legend labels
lab_els <- paste("sigma", sig_mas, sep="=")
for (in_dex in 1:4) {  # Plot four curves
curve(expr=dnorm(x, sd=sig_mas[in_dex]),
xlim=c(-4, 4),
xlab="", ylab="", lwd=2,
col=col_ors[in_dex],
add=as.logical(in_dex-1))
}  # end for
# Add title
title(main="Normal Distributions", line=0.5)
# Add legend
legend("topright", inset=0.05, title="Sigmas",
 lab_els, cex=0.8, lwd=2, lty=1, bty="n",
 col=col_ors)

rm(list=ls())
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
x_var <- seq(-4, 4, length=100)
sig_mas <- c(0.5, 1, 1.5, 2)  # Sigma values
# Create plot colors
col_ors <- c("red", "black", "blue", "green")
# Create legend labels
lab_els <- paste("sigma", sig_mas, sep="=")
# Plot the first chart
plot(x_var, dnorm(x_var, sd=sig_mas[1]),
     type="n", xlab="", ylab="",
     main="Normal Distributions")
# Add lines to plot
for (in_dex in 1:4) {
  lines(x_var, dnorm(x_var, sd=sig_mas[in_dex]),
  lwd=2, col=col_ors[in_dex])
}  # end for
# Add legend
legend("topright", inset=0.05, title="Sigmas",
 lab_els, cex=0.8, lwd=2, lty=1, bty="n",
 col=col_ors)

# Standard deviations of log-normal distribution
sig_mas <- c(0.5, 1, 1.5)
# Create plot colors
col_ors <- c("black", "red", "blue")
# Plot all curves
for (in_dex in 1:NROW(sig_mas)) {
  curve(expr=dlnorm(x, sdlog=sig_mas[in_dex]),
  type="l", xlim=c(0, 3), lwd=2,
  xlab="", ylab="", col=col_ors[in_dex],
  add=as.logical(in_dex-1))
}  # end for

# Add title and legend
title(main="Log-normal Distributions", line=0.5)
legend("topright", inset=0.05, title="Sigmas",
 paste("sigma", sig_mas, sep="="),
 cex=0.8, lwd=2, lty=rep(1, NROW(sig_mas)),
 col=col_ors)

x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
# Degrees of freedom
deg_free <- c(2, 5, 8, 11)
# Plot four curves in loop
col_ors <- c("red", "black", "blue", "green")
for (in_dex in 1:4) {
curve(expr=dchisq(x, df=deg_free[in_dex]),
xlim=c(0, 20), ylim=c(0, 0.3),
xlab="", ylab="", col=col_ors[in_dex],
lwd=2, add=as.logical(in_dex-1))
}  # end for

# Add title
title(main="Chi-squared Distributions", line=0.5)
# Add legend
lab_els <- paste("df", deg_free, sep="=")
legend("topright", inset=0.05, bty="n",
       title="Degrees of freedom", lab_els,
       cex=0.8, lwd=6, lty=1,
       col=col_ors)

x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
deg_free <- c(2, 5, 8, 11)  # df values
# Create plot colors
col_ors <- c("red", "black", "blue", "green")
# Create legend labels
lab_els <- paste("df", deg_free, sep="=")
# Plot an empty chart
x_var <- seq(0, 20, length=100)
plot(x_var, dchisq(x_var, df=deg_free[1]),
     type="n", xlab="", ylab="", ylim=c(0, 0.3))
# Add lines to plot
for (in_dex in 1:4) {
  lines(x_var, dchisq(x_var, df=deg_free[in_dex]),
lwd=2, col=col_ors[in_dex])
}  # end for

# Add title
title(main="Chi-squared Distributions", line=0.5)
# Add legend
legend("topright", inset=0.05,
       title="Degrees of freedom", lab_els,
       cex=0.8, lwd=6, lty=1, bty="n", col=col_ors)

x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
# Plot three curves in loop
deg_free <- c(3, 5, 9)  # Degrees of freedom
col_ors <- c("black", "red", "blue", "green")
for (in_dex in 1:NROW(deg_free)) {
curve(expr=df(x, df1=deg_free[in_dex], df2=3),
xlim=c(0, 4), xlab="", ylab="", lwd=2,
col=col_ors[in_dex], add=as.logical(in_dex-1))
}  # end for

# Add title
title(main="F-Distributions", line=0.5)
# Add legend
lab_els <- paste("df", deg_free, sep="=")
legend("topright", inset=0.05, title="degrees of freedom",
       lab_els, cex=0.8, lwd=2, lty=1,
       col=col_ors)

x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
deg_free <- c(3, 6, 9)  # df values
col_ors <- c("black", "red", "blue", "green")
lab_els <- c("normal", paste("df", deg_free, sep="="))
# Plot a Normal probability distribution
curve(expr=dnorm, xlim=c(-4, 4),
      xlab="", ylab="", lwd=2)
for (in_dex in 1:3) {  # Plot three t-distributions
curve(expr=dt(x, df=deg_free[in_dex]),
      lwd=2, col=col_ors[in_dex+1], add=TRUE)
}  # end for

# Add title
title(main="t-distributions", line=0.5)
# Add legend
legend("topright", inset=0.05, bty="n",
       title="Degrees\n of freedom", lab_els,
       cex=0.8, lwd=6, lty=1, col=col_ors)

x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
x_var <- seq(-4, 4, length=100)
deg_free <- c(3, 6, 9)  # df values
col_ors <- c("black", "red", "blue", "green")
lab_els <- c("normal", paste("df", deg_free, sep="="))
# Plot chart of normal distribution
plot(x_var, dnorm(x_var), type="l",
     lwd=2, xlab="", ylab="")
for (in_dex in 1:3) {  # Add lines for t-distributions
  lines(x_var, dt(x_var, df=deg_free[in_dex]),
lwd=2, col=col_ors[in_dex+1])
}  # end for

# Add title
title(main="t-distributions", line=0.5)
# Add legend
legend("topright", inset=0.05, bty="n",
       title="Degrees\n of freedom", lab_els,
       cex=0.8, lwd=6, lty=1, col=col_ors)

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
pare_to <- function(x, al_pha)
  al_pha*x^(-al_pha-1)
col_ors <- c("red", "blue", "green")
alpha_s <- c(1.0, 2.0, 3.0)
for (in_dex in 1:3) {  # Plot three curves
  curve(expr=pare_to(x, alpha_s[in_dex]),
  xlim=c(1, 2), ylim=c(0.0, 3.5),
  xlab="", ylab="", lwd=3, col=col_ors[in_dex],
  add=as.logical(in_dex-1))
}  # end for
# Add title and legend
title(main="Pareto Distributions", line=0.5)
lab_els <- paste("alpha", 1:3, sep=" = ")
legend("topright", inset=0.2, bty="n",
 title=NULL, lab_els, cex=0.8, lwd=6, lty=1,
 col=col_ors)

rm(list=ls())
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
poisson_events <- 0:11  # Poisson events
poisson_freq <- dpois(poisson_events, lambda=4)
names(poisson_freq) <- as.character(poisson_events)
# Poisson function
poisson_func <- function(x, lambda)
              {exp(-lambda)*lambda^x/factorial(x)}
curve(expr=poisson_func(x, lambda=4), xlim=c(0, 11), main="Poisson distribution",
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
histo_gram <- hist(pois_counts, col="lightgrey", xlab="count",
     ylab="frequency", freq=FALSE, main="Poisson histogram")
lines(density(pois_counts, adjust=1.5), lwd=2, col="blue")
# Poisson probability distribution function
poisson_func <- function(x, lambda)
  {exp(-lambda)*lambda^x/factorial(x)}
curve(expr=poisson_func(x, lambda=4), xlim=c(0, 11), add=TRUE, lwd=2, col="red")
# Add legend
legend("topright", inset=0.05, title="Poisson histogram",
 c("histogram density", "probability"), cex=0.8, lwd=2,
 lty=1, bty="n", col=c("blue", "red"))
# total area under histogram
diff(histo_gram$breaks) %*% histo_gram$density

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
  deg_free <- 2:20
  rang_e <- (1:NROW(deg_free))
  in_dex <- 4
  # Plot a curve
  curve(expr=dchisq(x, df=deg_free[in_dex]),
xlim=c(0, 30), ylim=c(0, 0.2),
xlab="", ylab="", lwd=3, col="red")
  # Add grey lines to plot
  for (it in rang_e[-in_dex]) {
    curve(expr=dchisq(x, df=deg_free[it]),
  xlim=c(0, 30), ylim=c(0, 0.2),
  xlab="", ylab="", lwd=2, col="grey80", add=TRUE)
  }  # end for
  # Add title
  title(main="Chi-squared Distributions", line=-1.5, cex.main=1.5)
  # Add legend
  text(x=20, y=0.15, labels=paste0("Degrees of freedom=",
      deg_free[in_dex]), pos=1, cex=1.3)
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
  deg_free <- 2:20
  rang_e <- (1:NROW(deg_free))
  # Set image refesh interval
  animation::ani.options(interval=0.5)
  # Create multiple plots with curves
  for (in_dex in rang_e) {
    curve(expr=dchisq(x, df=deg_free[in_dex]),
  xlim=c(0, 30), ylim=c(0, 0.2),
  xlab="", ylab="", lwd=3, col="red")
    # Add grey lines to plot
    for (it in rang_e[-in_dex]) {
      curve(expr=dchisq(x, df=deg_free[it]),
    xlim=c(0, 30), ylim=c(0, 0.2),
    xlab="", ylab="", lwd=2, col="grey80", add=TRUE)
    }  # end for
    # Add title
    title(main="Chi-squared Distributions", line=-1.5, cex.main=1.5)
    # Add legend
    text(x=20, y=0.15, labels=paste0("Degrees of freedom=",
      deg_free[in_dex]), pos=1, cex=1.3)
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
  # Calculate EWMA prices
  lamb_da <- input$lamb_da
  weight_s <- exp(-lamb_da*1:inter_val)
  weight_s <- weight_s/sum(weight_s)
  ew_ma <- filter(cl_ose, filter=weight_s, sides=1)
  ew_ma[1:(inter_val-1)] <- ew_ma[inter_val]
  ew_ma <- xts(cbind(cl_ose, ew_ma), order.by=index(cl_ose))
  colnames(ew_ma) <- c("VTI", "VTI EWMA")
  # Plot EWMA prices
  ch_ob <- chart_Series(ew_ma, theme=plot_theme, name="EWMA prices")
  plot(ch_ob)
  legend("top", legend=colnames(ew_ma),
   inset=0.1, bg="white", lty=1, lwd=2,
   col=plot_theme$col$line.col, bty="n")
})  # end renderPlot

library(zoo)  # Load zoo
library(ggplot2)  # Load ggplot2
library(scales)  # Load scales
my_ggplot <- ggplot(  # Specify data and aesthetics
  data=mtcars, mapping=aes(x=hp, y=mpg)) +
  geom_point() +  # Plot points
  ggtitle("basic scatterplot") +  # Add title
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
x_lim <- seq(from=-10, to=10, by=0.2)
y_lim <- seq(from=-10, to=10, by=0.2)
# Draw 3d surface plot of function
persp(z=outer(x_lim, y_lim, FUN=sur_face),
theta=45, phi=30, zlab="sine",
shade=0.1, col="green",
main="radial sine function")

library(rgl)  # Load rgl
with(iris,
     plot3d(Sepal.Length, Sepal.Width, Petal.Length,
      type="s", col=as.numeric(Species)))

library(rgl)  # Load rgl
# Define function of two variables
sur_face <- function(x, y) y*sin(x)
# Draw 3d surface plot of function
persp3d(x=sur_face, xlim=c(-5, 5), ylim=c(-5, 5),
  col="green", axes=FALSE)
# Draw 3d surface plot of matrix
x_lim <- seq(from=-5, to=5, by=0.1)
y_lim <- seq(from=-5, to=5, by=0.1)
persp3d(z=outer(x_lim, y_lim, FUN=sur_face),
  xlab="x", ylab="y", zlab="sur_face",
  col="green")
# Save current view to png file
rgl.snapshot("surface_plot.png")
# Define function of two variables and two parameters
sur_face <- function(x, y, lambda_1=1, lambda_2=1)
  sin(lambda_1*x)*sin(lambda_2*y)
# Draw 3d surface plot of function
persp3d(x=sur_face, xlim=c(-5, 5), ylim=c(-5, 5),
  col="green", axes=FALSE,
  lambda_1=1, lambda_2=2)
