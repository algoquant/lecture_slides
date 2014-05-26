rm(list=ls())  # remove all
par(new=TRUE)  # allow new plot on same chart
par(las=1)  # set text printing to "horizontal"


#################################
### Regression
#################################

# Draw a scatter plot
plot(x,y, xlab="x axis", ylab="y axis", main="my plot", ylim=c(0,20), xlim=c(0,20), pch=15, col="blue")
# fit a reg line to the points
myline.fit <- lm(y ~ x)
# get information about the fit
summary(myline.fit)
# draw the fit line on the plot
abline(myline.fit)


# coerce mts object into zoo
zoo.eustx <- as.zoo(EuStockMarkets)
# coerce index into class 'Dates'
index(zoo.eustx) <- as.Date(365*(index(zoo.eustx)-1970))


#################################
### plotting using ggplot2
#################################

# ggplot2 in multiple panes
autoplot(zoo.eustx, facets=Series ~ .)

library(zoo)  # load zoo
library(ggplot2)  # load ggplot2
library(gridExtra)

# ggplot2 in single pane
ggp.zoo1 <- autoplot(zoo.eustx, main="Eu Stox", 
                     facets=NULL) + xlab("") + 
  theme(legend.position=c(0.1, 0.5), 
        plot.title=element_text(vjust=-2.0), 
        plot.background=element_blank(),
        plot.background=element_rect(colour='purple', fill='pink', size=3, linetype='dashed')
  )

# ggplot2 in multiple panes
ggp.zoo2 <- autoplot(zoo.eustx, main="Eu Stox", 
                     facets=Series ~ .) + xlab("") + 
  theme(legend.position=c(0.1, 0.5), 
        plot.title=element_text(vjust=-2.0), 
        plot.background=element_blank(),
        plot.background=element_rect(colour='red', fill='blue', size=3, linetype='dashed')
  )


# ggplot2 with shaded area
x.reg <- -400:400/100
df.dnorm <- data.frame(x.reg=x.reg, d.norm=dnorm(x.reg))
df.dnorm$shade <- ifelse(df.dnorm$x.reg >= 2, df.dnorm$d.norm, NA)
ggp.my <- ggplot(
              data=df.dnorm, 
              mapping=aes(x=x.reg, y=d.norm)
          ) + 
          xlab("") + ylab("") + 
  ggtitle("Standard Normal Distribution") +
  theme(
        plot.title=element_text(vjust=-1.0), 
        plot.background=element_blank()
  )

ggp.my + geom_line() + geom_ribbon(aes(ymin=0, ymax=shade), fill="red")


# attempt to shade in ggplot2
geom_ribbon(  # add vertical line
  aes(ymin=0, ymax=dnorm),
  data=,
  fill="red"
  )  # end geom_vline


# attempt to plot two series, but the y-scale is the same for both
blah <- cbind(zoo.ar, cumsum(zoo.ar))
colnames(blah) <- c("AR returns", "AR prices")
autoplot(  # plot AR returns
  object=blah, main="Autoregressive model (phi=0.2)", 
  facets=Series ~ .) + xlab("") + ylab("") + 
  theme(legend.position=c(0.1, 0.5), 
        plot.title=element_text(vjust=-2.0), 
        plot.margin=unit(c(-0.5, 0.0, -0.5, 0.0), "cm"), 
        plot.background=element_blank(),
        axis.text.y=element_blank())

ggp.zoo1 <- autoplot(  # plot AR returns
  object=zoo.ar, main="AR returns", 
  facets=NULL) + xlab("") + ylab("") + 
  theme(legend.position=c(0.1, 0.5), 
        plot.title=element_text(vjust=-2.0), 
        plot.margin=unit(c(-0.5, 0.0, -0.5, 0.0), "cm"), 
        plot.background=element_blank(),
        axis.text.y=element_blank())
ggp.zoo2 <- autoplot(  # plot AR prices
  object=cumsum(zoo.ar), main="AR prices", 
  facets=NULL) + xlab("") + ylab("") + 
  theme(legend.position=c(0.1, 0.5), 
        plot.title=element_text(vjust=-2.0), 
        plot.margin=unit(c(-0.5, 0.0, -0.5, 0.0), "cm"), 
        plot.background=element_blank(),
        axis.text.y=element_blank())
# plot ggplot2 in multiple panes
grid.arrange(ggp.zoo1, ggp.zoo2, ncol=1)


#################################
### functions
#################################


n.globvar <- 11  # define a global variable
ls(environment())  # get all variables in environment
MyFunc <- function() {  # function for exploring environments
  n.locvar <- 1  # define a local variable
  cat('objects in parent environment:\t', 
      ls(parent.env(environment())), '\n')
  cat('objects in function environment:\t', 
      ls(environment()), '\n')
  cat('n.locvar:\t', n.locvar, 'n.globvar:\t', n.globvar, '\n')
  cat('this is the parent environment:')
  parent.env(environment())  # return parent environment
}  # end MyFunc
MyFunc()
environment(MyFunc)


FuncPower <- function(n.exp) {  # wrapper function
  # a power function factory
  function(n.arg) {  # anonymous closure
    cat(ls(environment()), '\n')
    var <- n.arg^n.exp
    cat(ls(environment()), '\n')
    var
  }
}  # end FuncPower

f.square <- FuncPower(2)

f.square(2)

funcTestFunc <- function(inputFunc, ...) {
  inputFunc(...)
}



#################################
### misc stuff for deletion
#################################

df.test <- data.frame(samples=rand.samples)
signif.level <- pnorm(2)
signif.level  # critical value=2 SD

df.test$p.values <- sapply(df.test$samples, 
                           function(sample) {
                             pnorm(sample) < critical.val
                           }
)  # end sapply


