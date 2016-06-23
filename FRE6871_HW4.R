#################################
### FRE6871 Homework #4 due May 9, 2016
#################################
# Max score 70pts

# Please write in this file the R code needed to perform the tasks below, 
# rename it to your_name_hw4.R
# and upload the file to NYU Classes


############## Part I
# Summary: Perform aggregations over data frame columns, 
# given intervals defined by breakpoints. 

# 1. (10pts) calculate a vector containing the number 
# of NAs in each column of the airquality data frame. 
# You can use functions sapply(), is.na(), sum(), 
# and an anonymous function. 

### write your code here

# You should get the following result:
#   Ozone Solar.R    Wind    Temp   Month     Day 
#     37       7       0       0       0       0


# Create a data frame called good_air, by subsetting 
# the "Temp" and "Solar.R" columns of the airquality data 
# frame, and then removing any rows containing NAs. 
# You can use function complete.cases(). 

### write your code here

# You should get a data frame with the following dimensions:
# dim(good_air)
# [1] 146   6


# 2. (10pts) Calculate a vector of breakpoints based on 
# the "Temp" column of good_air. 
# You can use function hist() with argument "plot=FALSE". 
# The function hist() returns a structure (list) with an 
# element called "breaks", which are the breakpoints. 

### write your code here

# You should get the following vector of breakpoints:
# [1]  55  60  65  70  75  80  85  90  95 100


# 3. (10pts) Calculate a vector of categorical data 
# from the "Temp" column of good_air, using the 
# breakpoints from p.2.  Add the categorical vector 
# as a column to good_air called "categ".
# The categorical column shows to which category 
# of "Temp" each row of good_air belongs. 
# You can use the function findInterval(). 

### write your code here

# You should get the following output:
# head(good_air)
#   Temp Solar.R categ
# 1   67     190     3
# 2   72     118     4
# 3   74     149     4
# 4   62     313     2
# 7   65     299     3
# 8   59      99     1


# 4. (20pts) Perform a split-apply-combine procedure on 
# the "Solar.R" column of good_air, using the categorical 
# column of good_air from p.3.  
# You can use the function aggregate() with argument 
# "formula". 
# Pass the function range() to aggregate(), so that it's 
# applied to the "Solar.R" column of good_air. 
# Assign the data frame returned by function aggregate() 
# to a variable called "ran_ge". 

### write your code here

# Add the breakpoints from p.2. as a column to ran_ge 
# called "temp".

### write your code here

# The end result of the split-apply-combine procedure 
# should be a data frame as follows:
# ran_ge
#   categ Solar.R.1 Solar.R.2 temp
# 1     1         8       266   55
# 2     2        19       334   60
# 3     3        13       322   65
# 4     4         7       320   70
# 5     5        27       322   75
# 6     6        24       332   80
# 7     7        82       323   85
# 8     8       167       291   90
# 9     9       203       237   95

# Plot the difference between the two "Solar.R" columns 
# of ran_ge versus "temp", to display how the variability 
# of "Solar.R" depends on "temp". 
# You can use the functions with() and plot(). 

### write your code here



############## Part II
# Summary: Create a functional called do_call(), which is 
# a generalization of do_call_rbind() from the lecture slides.  

# 1. (20pts) do_call() should apply a function to 
# a list of objects, and return a single object.
# do_call() should accept three arguments:
# - func_tion - a function that returns a single object 
#   from a list of input objects (for example paste(), 
#   rbind(), etc). 
# - li_st - a list or vector of objects.
# - "..." - dots optional arguments to func_tion. 
# The dots "..." argument should be passed to func_tion. 
# You can use functions length(), while(), lapply(), 
# and return(). 

### write your code here

# call do_call() as below, to verify that it works correctly,

do_call(paste, c("a", "b", "c"), sep="/")
# should produce:
# [1] "a/b/c"

do_call(rbind, list(1:4, 8:11))
# should produce:
#       [,1] [,2] [,3] [,4]
# [1,]    1    2    3    4
# [2,]    8    9   10   11



