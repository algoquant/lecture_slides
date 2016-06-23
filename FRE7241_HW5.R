#################################
### FRE7241 Homework #5 due May 18, 2016
#################################
# Max score 160pts

# Please write in this file the R code needed to perform the tasks below, 
# rename it to your_name_hw5.R
# and upload the file to NYU Classes

############## Part I
# Summary: Estimate the standard errors of regression 
# coefficients using bootstrap simulations. 

# 1. (10pts) Specify a regression as follows:

set.seed(1121)  # reset random number generator
# define explanatory and response variables
explana_tory <- seq(from=0.1, to=3.0, by=0.1)
res_ponse <- 3 + 2*explana_tory + rnorm(length(explana_tory))
# specify regression formula and perform regression
reg_formula <- res_ponse ~ explana_tory
reg_model <- lm(reg_formula)

# Extract the regression coefficient standard errors 
# from the regression model summary, and call them 
# std_errors.
# You can use the function summary().

### write your code here


# 2. (20pts) Perform an sapply() loop 10000 times. 
# In each loop bootstrap (sample) the variables 
# explana_tory and res_ponse, perform the regression, 
# and return the regression coefficients.  
# Call the matrix output by sapply() boot_strap. 
# hint: download the latest version of the 
# statistics.pdf file from NYU Classes and follow 
# the bootstrap example there. 
# You can use the functions sample.int(), lm(), 
# coef(), and sapply().

### write your code here

# You should get the following output:
# > boot_strap[, 1:3]
#                 [,1]     [,2]     [,3]
# (Intercept)  3.339112 3.458160 3.108136
# explana_tory 1.809326 1.737513 1.864102

# Calculate the standard errors from bootstrap, 
# and call them boot_errors.
# You can use the functions sd() and apply(). 
# Calculate the differences between boot_errors 
# and std_errors. 

### write your code here

# You should get the following output:
# > boot_errors
# (Intercept) explana_tory 
#   0.2655507    0.1326947


# 3. (10pts) Create a density plot of the bootstrapped 
# coefficients boot_strap["explana_tory", ], 
# with plot title "Bootstrapped regression slopes". 
# and with x-axis label "regression slopes". 
# Add a vertical line in red at the x-axis point: 
#   mean(boot_strap["explana_tory", ]). 
# You can use the functions x11(), plot(), density(), 
# and abline(). 

x11()

### write your code here



############## Part II
# Summary: Perform aggregations on a data frame using 
# the split-apply-combine procedure. 
# Download the latest version of the file expressions.pdf 
# from NYU Classes, and follow the examples on slide #32. 

# 1. (10pts) Download the file "CRSPpanel.txt" from NYU 
# Classes. 
# The file "CRSPpanel.txt" contains a data frame with 
# a single day of panel data. The panel data contains 
# fundamental financial data for 265 S&P500 stocks. 
# Read the file into a data frame called panel_data using 
# read.table(), with the "header" and "sep" arguments. 

### write your code here

# You should get the following output for panel_data:
# > dim(panel_data)
# [1] 265  48
# 
# > panel_data[1:3, 1:4]
#       DATE PERMNO    CUSIP     COMPANY.NAME
# 1 20031231  26403 25468710   DISNEY WALT CO
# 2 20031231  89525 20030N10 COMCAST CORP NEW
# 3 20031231  66181 43707610   HOME DEPOT INC

# Coerce the Industry column of panel_data from a factor 
# into a vector called indus_tries. 
# Coerce the Sector column of panel_data from a factor 
# into a vector called sec_tors. 
# You can use function as.vector(). 

### write your code here

# You should get the following output:
# > head(unique(indus_tries))
# [1] "Media"                         "Retailing"                     "Hotels Restaurants & Leisure"
# [4] "Automobiles & Components"      "Consumer Durables & Apparel"   "Household & Personal Products"
# 
# > head(unique(sec_tors))
# [1] "Consumer Discretionary" "Consumer Staples"   "Industrials"   "Energy"
# [5] "Utilities"              "Materials"


# 2. (20pts) Each Industry belongs to a single Sector. 
# Calculate a named vector of Industries with the Sectors 
# to which they belong, and call it industry_sectors. 
# The names of industry_sectors should be the Industries, 
# and the strings should be the Sectors to which they 
# belong.
# You must perform the calculation in two different ways. 
# In the first method you must use function tapply(), 
# and you can also use functions drop(), as.matrix(), 
# unique(), and an anonymous function. 
# hint: you can use the vectors sec_tors and indus_tries.

### write your code here

# tapply() returns an array which you must coerce into 
# a named vector of strings (not factors!). 
# You can coerce industry_sectors into a named vector 
# using function sapply(), and an anonymous function. 
# hint: you can perform an sapply() over the array.
# You can instead use functions drop() and as.matrix(). 

### write your code here


# You should get the following output, with names of Industries 
# without quotes, and names of Sectors with quotes: 
# 
# > industry_sectors
# Automobiles & Components                    Capital Goods 
# "Consumer Discretionary"                    "Industrials" 
# Commercial & Professional Services          Consumer Durables & Apparel 
# "Industrials"                              "Consumer Discretionary" 


# In the second method you must use function sapply(). 
# You cannot use tapply(). 
# You can also use function match(), and an anonymous 
# function. 
# hint: you can use the vectors sec_tors and indus_tries.
# hint: you can either perform an sapply() loop over the 
# levels of panel_data$Industry, or over unique elements 
# of indus_tries. 

### write your code here


# Verify that both methods produce the same Industry 
# to Sector mappings, even though the vectors may be 
# permuted with respect to each other. 
# You must use function identical(). 
# You can also use function names(). 

### write your code here



# 3. (20pts) Each Sector has one or more Industries that 
# belong to it. 
# Calculate a named list (or array) of Sectors with all 
# the Industries belonging to them, and call it 
# sector_industries. 
# You can either use functions tapply() or sapply(). 
# You can also use functions as.vector(), unique(), 
# and an anonymous function. 
# hint: you can use the vectors sec_tors and indus_tries.

### write your code here


# You should get the following output, with names of Sectors 
# as list element names, and names of Industries with quotes: 
# 
# > sector_industries
# $`Consumer Discretionary`
# [1] "Media"                        "Retailing"                  "Hotels Restaurants & Leisure"
# [4] "Automobiles & Components"     "Consumer Durables & Apparel" 
# 
# $`Consumer Staples`
# [1] "Household & Personal Products" "Food, Beverage & Tobacco"    "Food & Drug Retailing"        


# 4. (20pts) Calculate a named list with the stock tickers 
# of companies in each industry, and call it industry_tickers. 
# You must perform the calculation in two different ways. 
# In the first method you must use function tapply(), and 
# you can also use functions as.vector(), and an anonymous 
# function. 
# hint: you can use the vector indus_tries.

### write your code here

# tapply() returns an array which you must coerce into 
# a named list of vectors of strings (not factors!). 
# You can coerce industry_tickers into a named list using 
# function lapply(), and an anonymous function. 
# hint: you can perform an lapply() loop over the array.
# You can instead use functions drop() and as.matrix(). 

### write your code here

# You should get the following output, with names 
# of Industries as the list element names:
# 
# > industry_tickers
# $`Automobiles & Components`
# [1] "JCI" "BWA"
# 
# $`Capital Goods`
# [1] "BA"   "MMM"  "HON"  "EMR"  "DHR"  "CMI"  "ETN"  "LMT"  "PCP"  "ITW"  "GD"   "RTN"  "NOC"  "GWW"  "PH"
# [16] "ROK"  "DOV"  "IR"   "FLR"  "TYC"  "FAST" "PNR"  "ROP"  "COL"  "PLL"  "LLL"  "MAS"  "JEC"


# In the second method you must use function sapply(). 
# You cannot use tapply(). 
# You can also use the functions levels(), unique(), 
# as.vector(), and an anonymous function. 
# hint: you can use the vector indus_tries.
# hint: you can either perform an sapply() loop over the 
# levels of panel_data$Industry, or over unique elements 
# of indus_tries. 

### write your code here


# 5. (20pts) Calculate a named vector with the number of 
# companies in each Industry. 
# You can use functions sapply() and length(). 
# hint: you can use the vector industry_tickers.

### write your code here

# You should get the following output, with names 
# of Industries as the element names:
# 
# Automobiles & Components                Capital Goods
#             2                                 28
# Commercial & Professional Services      Consumer Durables & Apparel
#             8                                 12


# Calculate a named list with the indices of companies in 
# each industry, and call it industry_indices. 
# The index of a company is its row number in panel_data.
# You can use functions sapply() and match(). 
# hint: you can use the vector industry_tickers.

### write your code here

# You should get the following output, with names 
# of Industries as the element names: 
# 
# > industry_indices
# $`Automobiles & Components`
# [1]  9 21
# 
# $`Capital Goods`
# [1] 139 141 142 144 145 146 147 148 150 151 153 155 156 157 158 160 161 162 163 164 165 166 167 168 170 177 178
# [28] 179



# 6. (10pts) Calculate a named vector (not an array!) with 
# the average "NET.INCOME" of all the companies in each Sector. 
# You can use functions tapply(), sapply(), with(), mean(), 
# unique(), levels(), and an anonymous function. 
# hint: you can use the vector sec_tors. 

### write your code here

# You should get the following output: 
# Consumer Discretionary    Consumer Staples    Energy    Financials
#   477.3523                  1033.5947       1542.6779     666.9847
# Health Care   Industrials     Information Technology    Materials
# 898.7432        490.6571            861.5079            216.4407
# Telecommunication Services      Utilities
#         1762.2357               229.8667


# 7. (20pts) Calculate a data frame (not an array!) of 
# companies that have the highest ROE in each Industry, 
# and call it max_roes. 
# The data frame row names should be the Industry names, 
# the first column should be the tickers of companies with 
# the highest ROE, and the second column should be their 
# ROE. 
# You must perform the calculation in two different ways. 
# In the first method you must use function tapply(), and 
# you can also use functions as.vector(), match(), max() 
# and with(). 

# hint: You can first perform a tapply() loop over the 
# Industry column, to get the max ROEs, and then use 
# match() to get the tickers. 

### write your code here


# In the second method you must use function sapply(). 
# You cannot use tapply(). 
# You can also use the functions which.max(), max(), 
# with(), list(), as.vector(), data.frame(), unlist(), 
# and an anonymous function. 
# hint: You can use the vector indus_tries.
# hint: You can first perform an sapply() loop over 
# industry_indices, and then pass the output to 
# data.frame(). 

### write your code here

# You should get the following output: 
# > max_roes
#                                           ticker      roe
# Automobiles & Components                    JCI 0.16025626
# Capital Goods                               COL 0.30972389
# Commercial & Professional Services          PBI 0.45809675
# Consumer Durables & Apparel                 COH 0.34344821
# Diversified Financials                      LUK 0.04547642

