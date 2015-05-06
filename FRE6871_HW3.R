#################################
### HW #3 FRE6871 R in Finance
#################################
# Max score 60pts

# Please write in this file the R code needed to perform the tasks below, 
# rename it to your_name_hw3.R
# and send this file to Harjinder Singh (harjinder.singh@nyu.edu)


### write your code here




##################################
# 1. (30pts) Create a data frame containing the homework and test scores for 20 students, 
# call the data frame "student_scores",
# 
# The data frame "student_scores" should have a column called "name" (character), 
# equal to the vector "student_names":

student_names <- c("Chloe", "Olivia", "Madison", "Ethan", "James", "Amelia", "Anthony", 
                   "Joseph", "Evelyn", "Matthew", "Michael", "Liam", "Allison", "Mason", 
                   "Emma", "Jayden", "Emily", "William", "Ella", "Elizabeth")


# Create a factor variable called "tra_ck" of length(student_names),
# and assign to it random values sampled from the vector of strings:
# "Corporate", "Computational", "InfoTech", "Risk"
# Use functions sample() and as.factor(), 

### write your code here



######
# Create the data frame "student_scores" from vectors "student_names" and "tra_ck",
# Use function data.frame() with "stringsAsFactors=FALSE" to avoid coercing "character" to "factor",

### write your code here



######
# Create a numeric matrix with six columns, called "sco_res", 
# and assign to it random integer scores between 30 and 60,
# Use functions sample() and matrix(), 

### write your code here



######
# calculate a vector containing average scores for each student, and bind it to "sco_res",
# Use functions cbind() and rowMeans(), 

### write your code here

# Assign the following column names to "sco_res":
# "HW1_score", "HW2_score", "HW3_score", "HW4_score", "test1_score", "test2_score", "avg_score",

### write your code here



######
# Assign letter grades to each student, based on their "avg_score", 
# using the following table:
# "A" if "avg_score" >= 50.0
# "A-" if "avg_score" >= 47.5
# "B+" if "avg_score" >= 45.0
# "B" if "avg_score" >= 42.5
# "B-" if "avg_score" >= 40.0
# "C+" if "avg_score" >= 37.5
# "C" if "avg_score" >= 35.0

# Create a numeric vector of breakpoints for "avg_score", called "brea_ks" as follows: 

brea_ks <- seq(from=35, to=52.5, length.out=8)
names(brea_ks) <- c("C-", "C", "C+", "B-", "B", "B+", "A-", "A")


######
# Create a factor variable  containing letter grades, called "letter_grades", 
# 
# There are at least two ways of doing this, but you only need to do it one way.
# In the first approach you can use the names of "brea_ks", and either 
# for() loop, and/or if() and else(), and/or logical operators "<", ">", etc., 

# first create vector "letter_grades" containing empty strings:

### write your code here

# next populate "letter_grades" with letter grades using for() loop:

### write your code here


# In the second approach you can use function findInterval() and the names of "brea_ks",

### write your code here


######
# cbind "sco_res" and "letter_grades" to the data frame "student_scores",

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
# 3. (15pts) Plot a histogram of the number of students in each "letter_grade" category,
# you can use the lecture slide titled "Cars93 Data Frame",
# 
# There are at least two ways of doing this, but you only need to do it one way.
# In the first approach you can use column "student_scores$avg_score" and function hist(), 
# and the vector "brea_ks",

### write your code here

# In the second approach you can use column "student_scores$letter_grade", 
# and functions table() and barplot(), 

### write your code here


# extract the "class" of the columns of "student_scores" using functions class() and sapply(),
# make sure that none of the columns are factors, except for "finance_track" and "letter_grades",

### write your code here


