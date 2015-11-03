Generic R Presentation
========================================================
title: "Generic R Presentation"
author: Jerzy Pawlowski
date: 10/04/2015
css: FRE_lectures.css
width: 1900
height: 1000



<div class="midcenter" style="margin-left:-300px; margin-top:300px;">
<img src="engineering_long_color.png"></img>
</div>


Slide With Images
========================================================
Hello World!  
Creating R Presentations isn't difficult at all!  
![easy](image/easy_button.png)

***

R is fun!  
![smiling_face](image/smile_small.png)



Slide With Math Formulas
========================================================

A sum:  
$$
\bar{x}=\frac{1}{n}{\sum_{i=1}^{n}x_{i}}
$$

***

An Identity of Ramanujan:  
\[ \frac{1}{\Bigl(\sqrt{\phi \sqrt{5}}-\phi\Bigr) e^{\frac25 \pi}} = 1+\frac{e^{-2\pi}} {1+\frac{e^{-4\pi}} {1+\frac{e^{-6\pi}} {1+\frac{e^{-8\pi}} {1+\ldots} } } } \]


Slide With Text Formatting
========================================================

### Header 3  
#### Header 4  
##### Header 5  
###### Header 6  

make *italics* with single asterisks.  
make **bold** with double asterisks.  
> make a block quote by starting the line with a greater-than sign.  

you can separate paragraphs with empty lines in between them.  

use two trailing spaces to add a line break  


***

RStudio offers tutorials about writing presentations in RMarkdown: Introduction to [RMarkdown](http://rmarkdown.rstudio.com/)

You can insert `inline R code chunks` by enclosing them in single backticks (a backtick is different from a single quote).  

You can insert 
```
block R code chunk
```
by enclosing them in triple backticks

You can also insert 
```
    block R code chunks
    more code...
    still more code...
```
by indenting with 4 leading spaces in each line.


Slide With Bullet Points
========================================================

For more details on authoring R presentations click the
**Help** button on the toolbar.

bulletted list with block quote:  
>- Bullet 1
>- Bullet 2
>- Bullet 3

another unordered list (need two spaces before sub-items):  
* item 1
  + sub-item 1
  + sub-item 2
* item 2
  + sub-item 1
  + sub-item 2

vertical line:
***

ordered list and sub-lists:  

1. item 1
  + sub-item 1
  + sub-item 2
2. item 2
  + sub-item 1
  + sub-item 2




Slide With Code
========================================================

First column contains simple R code that returns the summary of the cars data frame:  

```r
> summary(cars)
```

***

Second column contains the output of the code in the first column:  

```
     speed           dist    
 Min.   : 4.0   Min.   :  2  
 1st Qu.:12.0   1st Qu.: 26  
 Median :15.0   Median : 36  
 Mean   :15.4   Mean   : 43  
 3rd Qu.:19.0   3rd Qu.: 56  
 Max.   :25.0   Max.   :120  
```



Slide With Plot
========================================================

First column with R code:  

```r
> plot(cars)
```

***

Second column with plot:  
![plot of chunk plot_cars](generic_rpresentation-figure/plot_cars-1.png) 



Slide with Embedded External Shiny Application
========================================================
Below is an embedded external shiny application, using function ~~shinyAppDir()~~ which executes R code contained in files ui.R and server.R:

Here's another way of formatting Courier font:
<span style="font-family:Courier; color:blue; font-size:40px;">shinyAppDir()</span>

Here's a third way of formatting Courier font: `shinyAppDir()`

Here's a fourth way of formatting Courier font: <p><pre>Courier text</pre></p>
<p style="font-family:courier">This is another paragraph with courier text.</p>

***


```r
> library(shiny)
> shinyAppDir(
+   appDir="C:/Develop/R/shiny/normal_dist",
+   options=list(width="100%", height=400)
+   )  # end shinyAppDir
```



Slide with Interactive 3d Surface Plot
========================================================

First column with R code:  

```r
> library(rgl)  # load rgl
> knit_hooks$set(webgl=hook_webgl)
> # define function of two variables
> foo <- function(x, y) y*sin(x)
> # draw 3d surface plot of function
> persp3d(x=foo, xlim=c(-5, 5), ylim=c(-5, 5), col="green", axes=FALSE)
```

***

Second column with plot:  


