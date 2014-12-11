

library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
thm <- knit_theme$get("acid")
knit_theme$set(thm)



sessionInfo()  # get R version and other session info



Sys.getenv()[5:7]  # list some environment variables

Sys.getenv("Home")  # get R user HOME directory

Sys.setenv(Home='C:/Develop')  # set HOME directory

Sys.getenv("Home")  # get user HOME directory

Sys.getenv("R_home")  # get R_HOME directory

R.home()  # get R_HOME directory

R.home("etc")  # get "etc" sub-directory of R_HOME



## # ?options  # long list of global options
## 
## # interpret strings as characters, not factors
## options("stringsAsFactors")
## options(stringsAsFactors=FALSE)
## 
## # number of digits printed for numeric values
## options(digits=3)
## 
## # number of items printed to console
## options(max.print=80)
## 
## # warning levels options
## # negative - warnings are ignored
## options(warn=-1)
## # zero - warnings are stored and printed after top-level function has completed
## options(warn=0)
## # one - warnings are printed as they occur
## options(warn=1)
## # two or larger - warnings are turned into errors
## options(warn=2)



# R startup (site) directory
paste(R.home(), 'etc', sep='/')

file.path(R.home(), 'etc')  # better way

# perform tilde-expansions and convert to readable format
normalizePath(file.path(R.home(), 'etc'), winslash="/")

normalizePath(R.home("etc"), winslash="/")



normalizePath('~', winslash="/")  # Windows user HOME directory

Sys.getenv("Home")  # R user HOME directory

setwd("C:/Develop/R")
getwd()  # current working directory

# R startup (site) directory
normalizePath(file.path(R.home(), 'etc'), winslash="/")

# R executable directory
normalizePath(file.path(R.home(), 'bin/x64'), winslash="/")

# R documentation directory
normalizePath(file.path(R.home(), 'doc/manual'), winslash="/")



setwd("C:/Develop/data")
sample(dir(), 5)  # get 5 file names - dir() lists all files
dir(pattern="csv")  # list files containing "csv"
list.files(R.home())  # all files in R_HOME directory
list.files(R.home("etc"))  # all files in "etc" sub-directory of R_HOME directory
list.dirs()  # directories in cwd
list.dirs(R.home("etc"))  # directories in "etc" sub-directory
Sys.glob("*.csv")
Sys.glob(R.home("etc"))



getwd()  # get cwd



setwd("C:/Develop/R")
# help(Startup)  # description of R session startup mechanism

# files in R startup directory directory
dir(normalizePath(file.path(R.home(), 'etc'), winslash="/"))

# *.R* files in cwd directory
getwd()
dir(getwd(), all.files=TRUE, pattern="\\.R")
dir(getwd(), all.files=TRUE, pattern=glob2rx("*.R*"))



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
poisson_freq
poisson_func <- function(x, lambda)  # Poisson function
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


