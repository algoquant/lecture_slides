

library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
thm <- knit_theme$get("acid")
knit_theme$set(thm)



# ?iris  # get information on iris
dim(iris)
head(iris, 2)
colnames(iris)
unique(iris$Species)  # extract list of unique elements of iris
class(unique(iris$Species))
# find which columns of iris are numeric
sapply(iris, is.numeric)
# calculate means of iris columns
sapply(iris, mean)  # returns NA for Species



sapply(6:10, sqrt)  # sapply on vector
sapply(list(6, 7, 8, 9, 10), sqrt)  # sapply on list

# calculate means of iris data frame columns
sapply(iris, mean)  # returns NA for Species

# create a matrix
mat_rix <- matrix(sample(100), ncol=4)
# calculate column means using apply
apply(mat_rix, 2, mean)

# calculate column means using sapply, with anonymous function
sapply(1:ncol(mat_rix), 
       function(col_index) {  # anonymous function
 mean(mat_rix[, col_index])
  }  # end anonymous function
)  # end sapply



sapply(iris[, -5], mean)  # vector of means of numeric columns
lapply(iris[, -5], mean)  # calculate means of numeric columns
# calculate means of numeric columns using anonymous function
unlist(lapply(iris, 
      function(col_umn) {
        if (is.numeric(col_umn)) mean(col_umn)
      }  # end anonymous function
      )  # end sapply
       )  # end unlist
unlist(sapply(iris, function(col_umn) {if (is.numeric(col_umn)) mean(col_umn)}))



# ?mtcars  # get information on mtcars - data from 1974 Motor Trend magazine
# mpg   Miles/(US) gallon
# qsec   1/4 mile time
# hp   Gross horsepower
# wt	 Weight (lb/1000)
# cyl   Number of cylinders
dim(mtcars)
head(mtcars, 2)
colnames(mtcars)
head(rownames(mtcars), 3)
unique(mtcars$cyl)  # extract list of car cylinders
sapply(mtcars, mean)  # calculate means of mtcars columns



unique(iris$Species)  # Species takes on three distinct values
# split into separate data frames by hand
set_osa <- iris[iris$Species=="setosa", ]
versi_color <- iris[iris$Species=="versicolor", ]
virgin_ica <- iris[iris$Species=="virginica", ]
dim(set_osa)
head(set_osa, 2)
# split iris into list based on Species
split_iris <- split(iris, iris$Species)
str(split_iris, max.level=1)
names(split_iris)
dim(split_iris$setosa)
head(split_iris$setosa, 2)



unique(mtcars$cyl)  # cyl has three unique values
# split the mtcars data frame based on number of cylinders
split_cars <- split(mtcars, mtcars$cyl)
str(split_cars, max.level=1)
names(split_cars)
# get mean mpg for each cylinder group
unlist(lapply(split_cars, function(x) mean(x$mpg)))
# Which is identical to the tapply function
tapply(mtcars$mpg, mtcars$cyl, mean)
# using "with" environment
with(mtcars, tapply(mpg, cyl, mean))
# can also use the functions by() and aggregate()
with(mtcars, by(mpg, cyl, mean))
aggregate(formula=(mpg ~ cyl), data=mtcars, FUN=mean)



# linear model with zero intercept
my_form <- z ~ x + y -1
my_form

# formula from text string
my_form <- as.formula(  # coerce text strings to formula
        paste("y ~ ", 
          paste(paste0("x", 1:5), collapse="+")
          )  # end paste
      )  # end as.formula
class(my_form)
my_form



# rm(list=ls())
set.seed(1121)  # initialize random number generator
v.xvar <- 0.1*1:30  # independent variable
v.yvar <- 3 + 2*v.xvar + rnorm(30)  # dependent variable plus noise
my_form <- v.yvar ~ v.xvar  # specify model
my_reg <- lm(my_form)  # perform regression
summary(my_reg)  # regression summary



set.seed(1121)  # initialize random number generator
# set plot paramaters - margins and font scale
par(oma=c(1, 1, 1, 1))  # set outer margins
par(mgp=c(2, 0.5, 0))  # axis title and labels
par(mar=c(5, 3, 1, 1), cex.lab=0.8, 
    cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
indep_var <- 0.1*1:30
depend_var <- 3 + 2*indep_var + rnorm(30)
# perform regression
simple_reg <- lm(depend_var ~ indep_var)

# plot scatterplot
plot(depend_var ~ indep_var)
title(main="Simple Regression", line=-1)
# add reg line
abline(simple_reg, col="red")



set.seed(1121)  # initialize random number generator
# set plot paramaters - margins and font scale
par(oma=c(1, 1, 1, 1))  # set outer margins
par(mgp=c(2, 0.5, 0))  # axis title and labels
par(mar=c(5, 3, 1, 1), cex.lab=0.8, 
    cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
attach(mtcars)  # add mtcars to search path
# specify model of horsepower vs miles per gallon
mtcars_model <- hp ~ mpg
# plot scatterplot horsepower vs miles per gallon
plot(mtcars_model, xlab="", ylab="", 
     main="horsepower vs miles per gallon")

# perform regression
mtcars_reg <- lm(mtcars_model)
# summary(mtcars_reg)  # regression summary
abline(mtcars_reg, col="red")  # add reg line

# add scatterplot using wordcloud labels 
# to prevent label overlaps
library(wordcloud)
textplot(x=mtcars$mpg, y=mtcars$hp, words=rownames(mtcars))

# don't forget to detach!!!
detach(mtcars)


