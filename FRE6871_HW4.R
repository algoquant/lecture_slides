#################################
### FRE6871 Homework #4 due July 6, 2015
#################################
# Max score 45pts

# Please write in this file the R code needed to perform the tasks below, 
# and submit it through NYU Classes,

##################################
# 1. (15pts) download the file "student_scores.csv" from NYU Classes,
# the file contains a data frame with student names, scores, and track,
# read the file into a variable called "student_scores" using read.csv(),
# use read.csv(), 
# and call the data frame "student_scores",

### write your code here



# Assign letter grades to each student, based on their "avg_score" column, 
# use the following table:
# "A" if "avg_score" >= 50.0
# "A-" if "avg_score" >= 47.5
# "B+" if "avg_score" >= 45.0
# "B" if "avg_score" >= 42.5
# "B-" if "avg_score" >= 40.0
# "C+" if "avg_score" >= 37.5
# "C" if "avg_score" >= 35.0

# Create a named numeric vector of breakpoints for "avg_score", 
# called "brea_ks" as follows: 
brea_ks <- seq(from=35, to=50.0, by=2.5)
names(brea_ks) <- c("C", "C+", "B-", "B", "B+", "A-", "A")


# Create a factor variable containing letter grades, called "letter_grades", 
# There are at least two ways of doing this, but you only need to do it one way,
# 
# In the first approach you can use the names of "brea_ks", and either 
# a for() loop, and/or if() and else(), and/or logical operators "<", ">", etc., 

# first create vector "letter_grades" containing empty strings:

### write your code here

# next populate "letter_grades" with letter grades using a for() loop:

### write your code here


# In the second approach you can use function findInterval() 
# and the names of "brea_ks",

### write your code here



# cbind "letter_grades" to the data frame "student_scores",

### write your code here




##################################
# 2. (15pts) Sort "student_scores" by "avg_score" column, 
# first in descending order, then in ascending order,
# use function order()

### write your code here



# Calculate the average scores for students for each "finance_track" category,
# use the split-apply-combine procedure, and use functions with() and tapply(),

### write your code here



##################################
# 3. (15pts) Plot a histogram of the number of students in each 
# "letter_grade" category, you can use the lecture slide titled 
# "Cars93 Data Frame",
# 
# There are at least two ways of doing this, but you need to do 
# it only one way.
# 
# In the first approach you can use column "student_scores$avg_score" 
# and function hist(), and the vector "brea_ks",

### write your code here


# In the second approach you can use column "student_scores$letter_grade", 
# and functions table() and barplot(), 

### write your code here


# extract the "class" of the columns of "student_scores" using 
# functions class() and sapply(),
# make sure that none of the columns are factors, 
# except for "finance_track" and "letter_grades",

### write your code here


# save "student_scores" to a comma-delimited CSV file,
# use function write.csv(),

### write your code here


