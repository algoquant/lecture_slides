#################################
### FRE6871 Homework #4 due Oct 26, 2015
#################################
# Max score 75pts

# Please write in this file the R code needed to perform the tasks below, 
# rename it to your_name_hw4.R
# and upload the file to NYU Classes


############## Part I
# Download the file "student_scores.csv" from NYU Classes. 
# The file contains a data frame with student names, track, and scores. 
# Read the file into a variable called "student_scores" using read.csv(),

### write your code here

# The data frame "student_scores" contains 8 columns: student names, 
# track, and six columns of numerical scores for homeworks and tests. 
# But some of the numeric columns contain NAs and characters, which 
# forces their coercion into factors.

# 1. (10pts) Perform an sapply() loop over the columns containing 
# numerical scores and coerce them to numeric. 
# You can use functions sapply() and as.numeric(), 

### write your code here

# Extract the "class" of the columns of "student_scores" using 
# functions class() and sapply(),
# make sure that none of the columns are factors, 
# except for "finance_track" and "letter_grades",

### write your code here

# Now the columns containing numerical scores should all be class 
# "numeric", with some NAs in them, representing scores that are 
# not available. 

# 2. (15pts) Calculate a vector called "num_nas" containing the 
# number of NA scores for each student, and bind it to 
# "student_scores" as the last (9th) column,
# You can use functions apply(), cbind(), sum(), is.na(), 
# and an anonymous function, 

### write your code here

# Sort "student_scores" in descending order by column "num_nas", 
# use function order()

### write your code here

# 3. (10pts) Calculate a vector called "avg_score" containing the 
# average score of each student, and bind it to "student_scores" as 
# the last (10th) column,
# Remember to omit NA values. 
# You can use functions apply(), cbind(), and mean(). 
# You cannot use an anonymous function. 

### write your code here

# 4. (20pts) Assign letter grades to each student, based on their 
# "avg_score" column. 
# First calculate a histogram of "avg_score" values, using the function 
# hist(), with the Freedman-Diaconis rule for determining the breakpoints. 

### write your code here

# The function hist() invisibly returns a list that includes a vector 
# of breakpoints called "breaks". 
# Calculate a vector of letter grades corresponding to the "avg_score" 
# values, using the "breaks" from function hist(), and call it 
# "letter_grades". 
# You must use function findInterval(), 

### write your code here

# "letter_grades" is an integer vector.
# Convert "letter_grades" to a vector of strings representing letter grades.
# Use the following vector of strings called "grade_s":

grade_s <- c("A", "A-", "B+", "B", "B-", "C+", "C")

# Be careful to consider that the highest "avg_score" should correspond 
# to the letter grade "A", which has index equal to 1 in "grade_s", 

### write your code here

# cbind "letter_grades" to the data frame "student_scores" as 
# the last column,

### write your code here

# 5. (20pts) Calculate the average scores for students in each 
# "finance_track" category, using the split-apply-combine procedure, 
# you can use functions with(), tapply(), and mean(),

### write your code here

# Find the names of the students with the highest average scores 
# in each "finance_track" category, using the split-apply-combine 
# procedure, 
# you can use functions with(), tapply(), max(), and match(),

### write your code here
