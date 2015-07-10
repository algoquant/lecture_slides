library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
thm <- knit_theme$get("acid")
knit_theme$set(thm)
x_var <- seq(-2*pi, 2*pi, len=100)  # x values

# open Windows graphics device
x11(width=11, height=7, title="simple plot")

# plot a sine function using basic line plot
plot(x=x_var, y=sin(x_var), xlab="x-val",
     ylab="y-val", type='l', lwd=2, col="red")
# add a cosine function
lines(x=x_var, y=cos(x_var), lwd=2, col="blue")
# add title
title(main="sine and cosine functions", line=0.1)
# add legend
legend(x="topright", legend=c("sine", "cosine"),
       title="legend", inset=0.1, cex=1.0, bg="white",
       lwd=2, lty=c(1, 1), col=c("red", "blue"))
graphics.off()  # close all graphics devices
par(mar=c(5, 3, 1, 1), oma=c(1, 1, 1, 1), mgp=c(2, 0.5, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
attach(mtcars)  # add mtcars to search path
# plot scatterplot horsepower vs miles per gallon
plot(hp, mpg,
     main="miles per gallon vs horsepower")

# add labels using wordcloud, to prevent overlaps
library(wordcloud)
textplot(x=hp, y=mpg, words=rownames(mtcars))

# don't forget to detach!!!
detach(mtcars)
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
par(mar=c(7, 4, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
pois_vector <- rpois(100, 4)  # Poisson numbers
# calculate contingency table
pois_table <- table(pois_vector)
pois_table <- pois_table/sum(pois_table)
pois_table

library(MASS)  # create histogram of Poisson variables
truehist(pois_vector, nbins="FD", col="blue", xlab="No. of events",
 ylab="Frequency of events", main="Poisson histogram")
x_var <- 0:max(pois_vector)  # add Poisson density
lines(x=x_var, y=dpois(x_var, lambda=4), lwd=4, col="red")
# add legend
legend(x="topright", legend="Poisson density", title="", inset=0.05,
       cex=0.8, bg="white", lwd=4, lty=1, col="red")
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
}
par(ask=FALSE)  # restore automatic plotting
par(new=TRUE)  # allow new plot on same chart
par(graph_params)  # restore original parameters
rm(list=ls())
Sys.Date()  # get today's date
date_time <- as.Date("2013-06-15")  # "%Y-%m-%d" or "%Y/%m/%d"
date_time
class(date_time)
as.Date("06-15-2013", "%m-%d-%Y")  # specify format
date_time + 20  # add 20 days
unclass(date_time)  # get internal integer representation
some.date <- as.Date("11/22/2013", "%m/%d/%Y")
some.date
# difference between dates
difftime(some.date, date_time, units="weeks")
weekdays(date_time)  # get day of the week
rm(list=ls())
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
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
pal_let <- c("red", "black", "blue", "green")
# create legend labels
lab_els <- paste("sigma", sig_mas, sep='=')
for (in_dex in 1:4) {  # plot four curves
curve(expr=dnorm(x, sd=sig_mas[in_dex]),
      type="l", xlim=c(-4, 4),
      xlab="", ylab="", lwd=2,
      col=pal_let[in_dex],
      add=as.logical(in_dex-1))
}  # end for
# add title
title(main="Normal Distributions", line=0.5)
# add legend
legend("topright", inset=0.05, title="Sigmas",
       lab_els, cex=0.8, lwd=2, lty=c(1, 1, 1, 1),
       col=pal_let)
rm(list=ls())
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
x_var <- seq(-4, 4, length=100)
sig_mas <- c(0.5, 1, 1.5, 2)  # sigma values
# create plot colors
pal_let <- c("red", "black", "blue", "green")
# create legend labels
lab_els <- paste("sigma", sig_mas, sep='=')
# plot an empty chart
plot(x_var, dnorm(x_var, sd=sig_mas[1]),
     type="n", xlab="", ylab="",
     main="Normal Distributions")
# add lines to plot
for (in_dex in 1:4) {
  lines(x_var, dnorm(x_var, sd=sig_mas[in_dex]),
lwd=2, col=pal_let[in_dex])
}
# add legend
legend("topright", inset=0.05, title="Sigmas",
       lab_els, cex=0.8, lwd=2, lty=c(1, 1, 1, 1),
       col=pal_let)
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
d_free <- c(2, 5, 8, 11)  # df values
# create plot colors
pal_let <- c("red", "black", "blue", "green")
# create legend labels
lab_els <- paste("df", d_free, sep='=')
for (in_dex in 1:4) {  # plot four curves
curve(expr=dchisq(x, df=d_free[in_dex]),
      type="l", xlim=c(0, 20), ylim=c(0, 0.3),
      xlab="", ylab="", lwd=2,
      col=pal_let[in_dex],
      add=as.logical(in_dex-1))
}  # end for
# add title
title(main="Chi-squared Distributions", line=0.5)
# add legend
legend("topright", inset=0.05,
       title="Degrees of freedom", lab_els,
       cex=0.8, lwd=2, lty=c(1, 1, 1, 1),
       col=pal_let)
rm(list=ls())
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
x_var <- seq(0, 20, length=100)
d_free <- c(2, 5, 8, 11)  # df values
# create plot colors
pal_let <- c("red", "black", "blue", "green")
# create legend labels
lab_els <- paste("df", d_free, sep='=')
# plot an empty chart
plot(x_var, dchisq(x_var, df=d_free[1]),
     type="n", xlab="", ylab="", ylim=c(0, 0.3),
     main="Chi-squared Distributions")
# add lines to plot
for (in_dex in 1:4) {
  lines(x_var, dchisq(x_var, df=d_free[in_dex]),
lwd=2, col=pal_let[in_dex])
}
# add legend
legend("topright", inset=0.05,
       title="Degrees of freedom", lab_els,
       cex=0.8, lwd=2, lty=c(1, 1, 1, 1),
       col=pal_let)
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
d_free <- c(3, 6, 9)  # df values
# create plot colors
pal_let <- c("black", "red", "blue", "green")
# create legend labels
lab_els <- c('normal', paste("df", d_free, sep='='))
# plot a Normal probability distribution
curve(expr=dnorm, type="l", xlim=c(-4, 4),
      xlab="", ylab="", lwd=2)
for (in_dex in 1:3) {  # plot three curves
curve(expr=dt(x, df=d_free[in_dex]),
      type="l", xlab="", ylab="", lwd=2,
      col=pal_let[in_dex+1], add=TRUE)
}  # end for
# add title
title(main="t-distributions", line=0.5)
# add legend
legend("topright", inset=0.05,
       title="Degrees\n of freedom", lab_els,
       cex=0.8, lwd=2, lty=c(1, 1, 1, 1),
       col=pal_let)
rm(list=ls())
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
x_var <- seq(-4, 4, length=100)
d_free <- c(3, 6, 9)  # df values
# create plot colors
pal_let <- c("black", "red", "blue", "green")
# create legend labels
lab_els <- c('normal', paste("df", d_free, sep='='))
# plot chart of normal distribution
plot(x_var, dnorm(x_var), type="l",
     lwd=2, xlab="", ylab="",
     main="t-distributions")
# add lines to plot
for (in_dex in 1:3) {
  lines(x_var, dt(x_var, df=d_free[in_dex]),
lwd=2, col=pal_let[in_dex+1])
}
# add legend
legend("topright", inset=0.05,
       title="Degrees\n of freedom", lab_els,
       cex=0.8, lwd=2, lty=c(1, 1, 1, 1),
       col=pal_let)
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
  method=list('last.bumpup', cex=0.7,
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
library(zoo)  # load package zoo
# show the generic function "merge"
merge
# show the "merge" method dispatched to "zoo" objects
merge.zoo
library(zoo)  # load package zoo
# get all methods for generic function "merge"
methods(generic.function="merge")
# get generic function methods applied to "zoo" objects
methods(class="zoo")
# define a generic function
gen_sum <- function (a, b, ...) {
  UseMethod("gen_sum")
}  # end gen_sum

# define method for "numeric" class
gen_sum.numeric <- function (a, b, ...) {
  sum(a, b)
}  # end gen_sum.character

# define method for "character" class
gen_sum.character <- function (a, b, ...) {
  paste(a, "plus", b)
}  # end gen_sum.character

# apply gen_sum to "numeric" objects
gen_sum(1, 2)
# apply gen_sum to "character" objects
gen_sum("a", "b")
# 'cbind' is an internal generic function
cbind
# define "+" method for "character" class
"+.character" <- function (a, b, ...) {
  paste(a, "plus", b)
}  # end +.character
methods("+")  # view methods for "+" operator
# define variables with "character" class
char1 <- "a"
char2 <- "b"
class(char1)
char1 + char2  # add two "character" objects - doesn't work
attributes(char1)  # doesn't have explicit "character" class - only implicit
char1 <- structure("a", class="character")
char2 <- structure("b", class="character")
attributes(char1)  # now has explicit "character" class
# add two "character" objects
char1 + char2
# define object of class "string"
obj_string <- "how are you today?"
class(obj_string) <- "string"
obj_string
# overload "print" method for string objects
print.string <- function (str_ing) {
  print(
    paste(strsplit(str_ing, split=" ")[[1]], 
  collapse=" + "))
}  # end print.string
# methods("print")  # view new methods for "print" function
print(obj_string)
obj_string
# overwrite "+" operator
"+" = function(a, b) {
  if(is.character(a) && is.character(b)) {
    paste(a, "plus", b)
  } else {
    .Primitive("+") (a, b)
  }
}
methods("+")  # view methods for "+" operator
# add two "numeric" objects
1 + 2
# add two "character" objects
"a" + "b"
# overwrite "+" operator with a generic function
"+" <- function (a, b, ...) {
  UseMethod("+")
}  # end gen_sum
# define method for "numeric" class
"+.numeric" <- function (a, b, ...) {
  sum(a, b)
}  # end gen_sum.character
# define method for "character" class
"+.character" <- function (a, b, ...) {
  paste(a, "plus", b)
}  # end gen_sum.character
methods("+")  # view methods for "+" operator
# add two "numeric" objects
1 + 2
# add two "character" objects
"a" + "b"
cbind.ts  # can't view non-visible method
stats::cbind.ts  # can't view non-visible method
stats:::cbind.ts  # display non-visible method
getAnywhere(cbind.ts)  # display non-visible method
rm(list=ls())
new_zoo <- zoo(rnorm(10), order.by=(Sys.Date() + 0:9))
# coerce "zoo" object to new class "zoo_xtra"
class(new_zoo) <- "zoo_xtra"
class(new_zoo)
methods(generic.function="length")
length  # primitive function
# define "length" method for class "zoo_xtra"
length.zoo_xtra <- function(in_ts) {
  cat("length of zoo_xtra object:\n")
# unclass object, then calculate length
  length(unclass(in_ts))
}  # end length.zoo_xtra
length(new_zoo)  # apply "length" method to "zoo_xtra" object
methods(generic.function="length")
# define "last" method for class "zoo_xtra"
last.zoo_xtra <- function(in_ts) {
  in_ts[length(in_ts)]
}  # end last.zoo_xtra
last(new_zoo)  # doesn't work
last.zoo_xtra(new_zoo)  # works
# define a generic function
last <- function (a, b, ...) {
  UseMethod("last")
}  # end last
last(new_zoo)  # now works
# define generic "string" class converter
as.string <- function (str_ing, ...) 
  UseMethod("as.string")
# default "string" class converter
as.string.default <- function (str_ing, ...)
  structure(str_ing, class="string", ...)
# numeric "string" class converter
as.string.numeric <- function (str_ing, ...)
  structure(as.character(str_ing), class="string", ...)
# "string" class checker
is.string <- function (str_ing)
  inherits(x=str_ing, what="string")
# define "string" object
obj_string <- as.string("how are you today?")
obj_string
is.string(obj_string)
is.string("hello")
as.string(123)
is.string(as.string(123))
rm(list=ls())
library(xts)
new_xts <- xts(rnorm(10), order.by=(Sys.Date() + 0:9))
class(new_xts)  # class attribute is a vector
# "last" is a generic function from package "xts"
last
methods(generic.function="last")
last(new_xts)  # apply "last" method from "xts" class
# derive object "xts_xtra" from "xts" object
class(new_xts) <- c("xts_xtra", class(new_xts))
class(new_xts)  # class attribute is a vector
# "xts_xtra" object inherits "last" method from "xts" class
last(new_xts)
# define new "last" method for class "xts_xtra"
last.xts_xtra <- function(in_ts) {
  cat("last element of xts_xtra object:\n")
  drop(in_ts[length(in_ts), ])
}  # end last.xts_xtra
last(new_xts)  # apply "last" from "xts_xtra" class
# define "last" method for class "xts_xtra"
last.xts_xtra <- function(in_ts) {
  cat("last element of xts_xtra object:\n")
  drop(NextMethod())
}  # end last.xts_xtra
last(new_xts)  # apply "last" from "xts_xtra" class
