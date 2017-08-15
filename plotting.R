library(knitr)
library(rgl)
knit_hooks$set(rgl=hook_rgl)
knit_hooks$set(webgl=hook_webgl)
opts_chunk$set(prompt=TRUE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size="scriptsize", fig.width=4, fig.height=4)
options(width=60, dev="pdf")
options(digits=3)
thm <- knit_theme$get("acid")
knit_theme$set(thm)
library(quantmod)
some_cars <- mtcars[sample(NROW(mtcars), 10), ]
# plot scatterplot horsepower vs miles per gallon
plot(some_cars[, "hp"], some_cars[, "mpg"],
     xlab="horsepower", ylab="miles per gallon",
     main="miles per gallon vs horsepower")
# add a solid red point (pch=16) for the last car
points(x=some_cars[NROW(some_cars), "hp"],
 y=some_cars[NROW(some_cars), "mpg"],
 col="red", pch=16)
# add labels with the car names
text(x=some_cars[, "hp"], y=some_cars[, "mpg"],
     labels=rownames(some_cars[, ]),
     pos=1, cex=0.8)
# or add labels using wordcloud, to prevent overlaps
library(wordcloud)
textplot(x=some_cars[, "hp"], y=some_cars[, "mpg"],
   words=rownames(some_cars))
# plot the tree Height
plot(trees[, "Height"],
     type="l",
     lwd=2,
     col="blue",
     main="Tree heights and volumes",
     xlab="tree number", ylab="",
     ylim=c(min(trees[, c("Height", "Volume")]),
      max(trees[, c("Height", "Volume")])))
# plot the tree Volume
lines(trees[, "Volume"], lwd=2, col="green")
# add legend
legend(x="left", legend=c("Height", "Volume"),
 inset=0.1, cex=1.0, bg="white",
 lwd=2, lty=c(1, 1), col=c("blue", "green"))
x_var <- seq(-2*pi, 2*pi, len=100)  # x values

# open Windows graphics device
x11(width=11, height=7, title="simple plot")

# plot a sine function using basic line plot
plot(x=x_var, y=sin(x_var), xlab="x-val",
     ylab="y-val", type="l", lwd=2, col="red")
# add a cosine function
lines(x=x_var, y=cos(x_var), lwd=2, col="blue")
# add title
title(main="sine and cosine functions", line=0.1)
# add legend
legend(x="topright", legend=c("sine", "cosine"),
 title="legend", inset=0.1, cex=1.0, bg="white",
 lwd=2, lty=c(1, 1), col=c("red", "blue"))
graphics.off()  # close all graphics devices
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# plot a Normal probability distribution
curve(expr=dnorm, type="l", xlim=c(-3, 3),
xlab="", ylab="", lwd=2, col="blue")
# add shifted Normal probability distribution
curve(expr=dnorm(x, mean=1), add=TRUE,
type="l", lwd=2, col="red")

# add title
title(main="Normal probability distribution functions",
line=0.1)
# add legend
legend(x="topright", legend=c("Normal", "shifted"),
 title="legend", inset=0.05, cex=0.8, bg="white",
 lwd=2, lty=c(1, 1), col=c("blue", "red"))
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
library(zoo)  # load zoo
load(file="C:/Develop/data/zoo_data.RData")
zoo_series <- window(zoo_stx[, "AdjClose"],
   start=as.Date("2013-01-01"),
   end=as.Date("2013-12-31"))
# extract time index and monthly dates
in_dex <- index(zoo_series)
# coerce index to monthly dates
month_ly <- as.yearmon(in_dex)
# tick locations at beginning of month
tick_s <-
  in_dex[match(unique(month_ly), month_ly)]
# or
tick_s <-
  as.Date(tapply(X=in_dex, INDEX=month_ly, FUN=min))
# first plot zoo without "x" axis
plot(zoo_series, xaxt="n", xlab=NA, ylab=NA)
# add "x" axis with monthly ticks
axis(side=1, at=tick_s,
 labels=format(tick_s, "%b-%y"), tcl=-0.7)
# add vertical lines
abline(v=tick_s, col="grey", lwd=0.5)
# plot zoo using base plotting functions
plot(as.vector(zoo_series), xaxt="n",
 xlab=NA, ylab=NA, t="l")
a_t <- seq_along(in_dex)[in_dex %in% tick_s]
# add "x" axis with monthly ticks
axis(side=1, at=a_t,
 labels=format(tick_s, "%b-%y"), tcl=-0.7)
abline(v=a_t, col="grey", lwd=0.5)
library(zoo)  # load zoo
load(file="C:/Develop/data/zoo_data.RData")
# extract time index and monthly dates
in_dex <- index(zoo_stx)
# coerce index to monthly dates
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
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
load(file="C:/Develop/data/zoo_data.RData")
#plot with two "y" axes
par(las=1)  # set text printing to "horizontal"
# plot first ts
plot(zoo_stxeur[, 1], xlab=NA, ylab=NA)
# set range of "y" coordinates for second axis
par(usr=c(par("usr")[1:2], range(zoo_stxeur[,2])))
lines(zoo_stxeur[, 2], col="red")  # second plot
axis(side=4, col="red")  # second "y" axis on right
# print axis labels
mtext(colnames(zoo_stxeur)[1], side=2, padj=-6, line=-4)
mtext(colnames(zoo_stxeur)[2], col="red", side=4, padj=-2, line=-3)
title(main="EUR and MSFT")  # add title
# add legend without box
legend("bottomright", legend=colnames(zoo_stxeur), bg="white",
 lty=c(1, 1), lwd=c(2, 2), col=c("black", "red"), bty="n")

##########

# slightly different method using par(new=TRUE)
# par(las=1)  # set text printing to "horizontal"
# plot(zoo_stxeur[, 1], xlab=NA, ylab=NA)
# par(new=TRUE)  # allow new plot on same chart
# plot(zoo_stxeur[, 2], xlab=NA, ylab=NA, yaxt="n", col="red")
# axis(side=4, col="red")  # second "y" axis on right
# mtext(colnames(zoo_stxeur)[1], side=2, padj=-6, line=-4)
# mtext(colnames(zoo_stxeur)[2], col="red", side=4, padj=-2, line=-3)
# title(main="EUR and MSFT", line=-1)  # add title
# legend("bottomright", legend=colnames(zoo_stxeur),
#        lty=c(1, 1), lwd=c(2, 2), col=c("black", "red"), bty="n")
graph_params <- par()  # get existing parameters
par("mar")  # get plot margins
par(mar=c(2, 1, 2, 1))  # set plot margins
par(oma=c(1, 1, 1, 1))  # set outer margins
par(mgp=c(2, 1, 0))  # set title and label margins
par(cex.lab=0.8,  # set font scales
    cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(las=1)  # set axis labels to horizontal
par(ask=TRUE)  # pause, ask before plotting
par(mfrow=c(2, 2))  # plot on 2x2 grid by rows
for (i in 1:4) {  # plot 4 panels
  barplot(sample(1:6), main=paste("panel", i),
    col=rainbow(6), border=NA, axes=FALSE)
  box()
}  # end for
par(ask=FALSE)  # restore automatic plotting
par(new=TRUE)  # allow new plot on same chart
par(graph_params)  # restore original parameters
x_var <- seq(-5, 7, length=100)
y_var <- dnorm(x_var, mean=1.0, sd=2.0)
plot(x_var, y_var, type="l", lty="solid",
     xlab="", ylab="")
title(main="Normal Density Function", line=0.5)
star_t <- 3; fin_ish <- 5  # set lower and upper bounds
# set polygon base
are_a <- ((x_var >= star_t) & (x_var <= fin_ish))
polygon(c(star_t, x_var[are_a], fin_ish),  # draw polygon
  c(-1, y_var[are_a], -1), col="red")
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
sig_mas <- c(0.5, 1, 1.5, 2)  # sigma values
# create plot colors
col_ors <- c("red", "black", "blue", "green")
# create legend labels
lab_els <- paste("sigma", sig_mas, sep="=")
for (in_dex in 1:4) {  # plot four curves
curve(expr=dnorm(x, sd=sig_mas[in_dex]),
type="l", xlim=c(-4, 4),
xlab="", ylab="", lwd=2,
col=col_ors[in_dex],
add=as.logical(in_dex-1))
}  # end for
# add title
title(main="Normal Distributions", line=0.5)
# add legend
legend("topright", inset=0.05, title="Sigmas",
 lab_els, cex=0.8, lwd=2, lty=c(1, 1, 1, 1),
 col=col_ors)
rm(list=ls())
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
x_var <- seq(-4, 4, length=100)
sig_mas <- c(0.5, 1, 1.5, 2)  # sigma values
# create plot colors
col_ors <- c("red", "black", "blue", "green")
# create legend labels
lab_els <- paste("sigma", sig_mas, sep="=")
# plot an empty chart
plot(x_var, dnorm(x_var, sd=sig_mas[1]),
     type="n", xlab="", ylab="",
     main="Normal Distributions")
# add lines to plot
for (in_dex in 1:4) {
  lines(x_var, dnorm(x_var, sd=sig_mas[in_dex]),
  lwd=2, col=col_ors[in_dex])
}  # end for
# add legend
legend("topright", inset=0.05, title="Sigmas",
 lab_els, cex=0.8, lwd=2, lty=c(1, 1, 1, 1),
 col=col_ors)
x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
d_free <- c(2, 5, 8, 11)  # df values
# create plot colors
col_ors <- c("red", "black", "blue", "green")
# create legend labels
lab_els <- paste("df", d_free, sep="=")
for (in_dex in 1:4) {  # plot four curves
curve(expr=dchisq(x, df=d_free[in_dex]),
      type="l", xlim=c(0, 20), ylim=c(0, 0.3),
      xlab="", ylab="", lwd=2,
      col=col_ors[in_dex],
      add=as.logical(in_dex-1))
}  # end for
# add title
title(main="Chi-squared Distributions", line=0.5)
# add legend
legend("topright", inset=0.05,
       title="Degrees of freedom", lab_els,
       cex=0.8, lwd=6, lty=c(1, 1, 1, 1),
       col=col_ors)
x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
d_free <- c(2, 5, 8, 11)  # df values
# create plot colors
col_ors <- c("red", "black", "blue", "green")
# create legend labels
lab_els <- paste("df", d_free, sep="=")
# plot an empty chart
x_var <- seq(0, 20, length=100)
plot(x_var, dchisq(x_var, df=d_free[1]),
     type="n", xlab="", ylab="", ylim=c(0, 0.3))
# add lines to plot
for (in_dex in 1:4) {
  lines(x_var, dchisq(x_var, df=d_free[in_dex]),
lwd=2, col=col_ors[in_dex])
}  # end for
# add title
title(main="Chi-squared Distributions", line=0.5)
# add legend
legend("topright", inset=0.05,
       title="Degrees of freedom", lab_els,
       cex=0.8, lwd=6, lty=c(1, 1, 1, 1),
       col=col_ors)
x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
d_free <- c(3, 6, 9)  # df values
# create plot colors
col_ors <- c("black", "red", "blue", "green")
# create legend labels
lab_els <- c("normal", paste("df", d_free, sep="="))
# plot a Normal probability distribution
curve(expr=dnorm, type="l", xlim=c(-4, 4),
      xlab="", ylab="", lwd=2)
for (in_dex in 1:3) {  # plot three curves
curve(expr=dt(x, df=d_free[in_dex]),
      type="l", xlab="", ylab="", lwd=2,
      col=col_ors[in_dex+1], add=TRUE)
}  # end for
# add title
title(main="t-distributions", line=0.5)
# add legend
legend("topright", inset=0.05,
       title="Degrees\n of freedom", lab_els,
       cex=0.8, lwd=6, lty=c(1, 1, 1, 1),
       col=col_ors)
x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
x_var <- seq(-4, 4, length=100)
d_free <- c(3, 6, 9)  # df values
# create plot colors
col_ors <- c("black", "red", "blue", "green")
# create legend labels
lab_els <- c("normal", paste("df", d_free, sep="="))
# plot chart of normal distribution
plot(x_var, dnorm(x_var), type="l",
     lwd=2, xlab="", ylab="")
# add lines to plot
for (in_dex in 1:3) {
  lines(x_var, dt(x_var, df=d_free[in_dex]),
lwd=2, col=col_ors[in_dex+1])
}  # end for
# add title
title(main="t-distributions", line=0.5)
# add legend
legend("topright", inset=0.05,
       title="Degrees\n of freedom", lab_els,
       cex=0.8, lwd=6, lty=c(1, 1, 1, 1),
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
legend(x="topright", legend="Poisson density", title="",
 inset=0.05, cex=0.8, bg="white", lwd=4, lty=1, col="red")
# generate Poisson variables
pois_counts <- rpois(1000, lambda=4)
head(pois_counts)
# calculate contingency table
pois_table <- table(pois_counts)
pois_table
# create barplot of table data
barplot(pois_table, col="lightgrey",
  xlab="counts", ylab="number of observations",
  main="barplot of Poisson count data")
# create histogram of Poisson variables
histo_gram <- hist(pois_counts, col="lightgrey", xlab="count",
     ylab="frequency", freq=FALSE, main="Poisson histogram")
lines(density(pois_counts, adjust=1.5), type="l", lwd=2, col="blue")
# Poisson probability distribution function
poisson_func <- function(x, lambda)
  {exp(-lambda)*lambda^x/factorial(x)}
curve(expr=poisson_func(x, lambda=4), xlim=c(0, 11), add=TRUE, lwd=2, col="red")
# add legend
legend("topright", inset=0.05, title="Poisson histogram",
 c("histogram density", "probability"), cex=0.8, lwd=2,
 lty=c(1, 1), col=c("blue", "red"))
# total area under histogram
diff(histo_gram$breaks) %*% histo_gram$density
# boxplot of Poisson count data
boxplot(x=pois_counts, ylab="counts",
  main="Poisson box plot")
# boxplot method for formula
boxplot(formula=mpg ~ cyl, data=mtcars,
  main="Mileage by number of cylinders",
  xlab="Cylinders", ylab="Miles per gallon")
inputPanel(
  sliderInput("lamb_da", label="lambda:",
        min=0.01, max=0.2, value=0.1, step=0.01)
)  # end inputPanel
renderPlot({
  lamb_da <- input$lamb_da
  # calculate EWMA prices
  weight_s <- exp(-lamb_da*1:win_dow)
  weight_s <- weight_s/sum(weight_s)
  ew_ma <- filter(cl_ose, filter=weight_s, sides=1)
  ew_ma[1:(win_dow-1)] <- ew_ma[win_dow]
  ew_ma <- xts(cbind(cl_ose, ew_ma), order.by=index(oh_lc))
  colnames(ew_ma) <- c("VTI", "VTI EWMA")
  # plot EWMA prices
  ch_ob <- chart_Series(ew_ma, theme=plot_theme, name="EWMA prices")
  plot(ch_ob)
  legend("top", legend=colnames(ew_ma),
   inset=0.1, bg="white", lty=c(1, 1), lwd=c(2, 2),
   col=plot_theme$col$line.col, bty="n")
})  # end renderPlot
library(zoo)  # load zoo
library(ggplot2)  # load ggplot2
library(scales)  # load scales
my_ggplot <- ggplot(  # specify data and aesthetics
  data=mtcars, mapping=aes(x=hp, y=mpg)) +
  geom_point() +  # plot points
  ggtitle("basic scatterplot") +  # add title
  theme(  # customize plot object
  plot.title=element_text(vjust=-2.0),
  plot.background=element_blank()
  )  # end theme
my_ggplot  # render the plot
# install.packages("directlabels", repo="http://r-forge.r-project.org")
library(ggplot2)  # load ggplot2
library(scales)  # load scales
library(gridExtra)  # load gridExtra
library(directlabels)  # load directlabels
my_ggplot <- ggplot(  # data and aesthetics
  data=mtcars, mapping=aes(x=hp, y=mpg)) +
  geom_point() +  # plot points
  theme(  # customize plot object
  legend.position="none",
  plot.title=element_text(vjust=-2.0),
  plot.margin=unit(c(-0.0,0.0,-0.5,0.0),"cm"),
  plot.background=element_blank()
  ) +
  scale_colour_discrete(guide="none")  # no label guide
car_names <- rownames(mtcars)
gg_labels <- geom_text(aes(  # ggplot2 labels
  label=car_names, color=car_names, size=5))
d_labels <- geom_dl(mapping=aes(  # directlabels
  label=car_names, color=car_names),
  method=list("last.bumpup", cex=0.7,
        hjust=1))
# render plots in single column
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
my_ggplot  # render the plot
library(ggplot2)  # load ggplot2
library(scales)  # load scales
library(gridExtra)  # load gridExtra
# coerce mts object into zoo
zoo_series <- as.zoo(EuStockMarkets)
# create ggplot2 theme object
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
# render plots in single column
grid.arrange(ggp_zoo_single +
         theme(legend.position=c(0.1, 0.5)),
       ggp_zoo_multiple, ncol=1)
library(zoo)  # load zoo
library(ggplot2)  # load ggplot2
library(gridExtra)
#
auto_theme <- theme(legend.position=c(0.1, 0.5),
  plot.title=element_text(vjust=-2.0),
  plot.margin=unit(c(-0.5, 0.0, -0.5, 0.0), "cm"),
  plot.background=element_blank(),
  axis.text.y=element_blank()
  )

# plot ggplot2 in single pane
ggp.zoo1 <- autoplot(zoo_series, main="Eu Stox",
   facets=NULL) + xlab("") +
  theme(legend.position=c(0.1, 0.5),
  plot.title=element_text(vjust=-2.0),
  plot.margin=unit(c(-0.5, 0.0, -0.5, 0.0), "cm"),
  plot.background=element_blank(),
  axis.text.y=element_blank()
  )
# plot ggplot2 in multiple panes
ggp.zoo2 <- autoplot(zoo_series, main="Eu Stox",
   facets=Series ~ .) + xlab("") +
  theme(legend.position=c(0.1, 0.5),
  plot.title=element_text(vjust=-2.0),
  plot.margin=unit(c(-0.5, 0.0, -0.5, 0.0), "cm"),
  plot.background=element_blank(),
  axis.text.y=element_blank()
  )
# create plot ggplot2 in multiple panes
grid.arrange(ggp.zoo1, ggp.zoo2, ncol=1)
# define function of two variables
sur_face <- function(x, y) sin(sqrt(x^2+y^2))
# calculate function over matrix grid
x_lim <- seq(from=-10, to=10, by=0.2)
y_lim <- seq(from=-10, to=10, by=0.2)
# draw 3d surface plot of function
persp(z=outer(x_lim, y_lim, FUN=sur_face),
theta=45, phi=30, zlab="sine",
shade=0.1, col="green",
main="radial sine function")
library(rgl)  # load rgl
with(iris,
     plot3d(Sepal.Length, Sepal.Width, Petal.Length,
      type="s", col=as.numeric(Species)))
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
sur_face <- function(x, y, lambda_1=1, lambda_2=1)
  sin(lambda_1*x)*sin(lambda_2*y)
# draw 3d surface plot of function
persp3d(x=sur_face, xlim=c(-5, 5), ylim=c(-5, 5),
  col="green", axes=FALSE,
  lambda_1=1, lambda_2=2)
