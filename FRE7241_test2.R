#################################
### FRE7241 Test #2 May 3, 2016
#################################
# Max score 70pts

# Please write in this file the R code needed to perform the tasks below, 
# rename the file to your_name_test2.R
# and upload the file to NYU Classes

##############
# Summary: List the class and dimension attributes 
# of objects in an environment. 

# Load the file etf_data_new.RData, which contains the 
# environment env_etf, containing ETF time series 
# and other ETF data. 

load(file="C:/Develop/data/etf_data_new.RData")

# 1. (10pts) List the names of all the objects in the 
# environment env_etf, and save the names in a vector 
# of strings called sym_bols. 
# You can use function ls(). 

### write your code here

# Create a list with the class attributes of all the 
# objects in the environment env_etf, and call it 
# class_es. 
# You can use functions eapply() and class(). 

### write your code here


# 2. (30pts) List the names of all the objects of 
# class xts in the environment env_etf, and save the 
# names in a vector of strings called sym_bols. 
# hint: first calculate a named boolean vector that 
# is TRUE for objects of class xts, then extract the 
# names of those objects. 
# You can use functions eapply(), is.xts(), unlist(), 
# and names(). 
# Or you can use sapply() instead of eapply(), and 
# an anonymous function, and the "%in%" operator. 

### write your code here


# 3. (30pts) Create a matrix called dimen_sions, 
# containing the dimensions of all the xts objects 
# in env_etf. 
# You can use the functions eapply(), dim(), rbind(), 
# and do.call(). 
# Or you can use lapply() and an anonymous function, 
# instead of eapply(). 
# hint: eapply() returns a list, and you must flatten 
# the list into a matrix using rbind() and do.call(). 

### write your code here

# You should get the following output:
#   dimen_sions
#           [,1] [,2]
# DBC       2338    6
# VNQ       2338    6
# price_s   2338   20
# XLB       2338    6
# VTI       2338    6
# etc.

