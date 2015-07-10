#################################
### FRE6871 Homework #4 Solution due July 6, 2015
#################################
# Max score 45pts

# The below solutions are examples,
# Slightly different solutions are also possible.


##################################
# 1. (15pts) download the file "student_scores.csv" from NYU Classes,
# the file contains a data frame with student names, scores, and track,
# read the file into a variable called "student_scores" using read.csv(),
# use read.csv(), 
# and call the data frame "student_scores",
student_scores <- read.csv(file='student_scores.csv')


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
letter_grades <- character(20)
# next populate "letter_grades" with letter grades using a for() loop:
for (brea_k in 1:length(brea_ks)) {
  in_dex <- student_scores[, "avg_score"] >= brea_ks[brea_k]
  letter_grades[in_dex] <- names(brea_ks[brea_k])
}  # end for
letter_grades <- as.factor(letter_grades)

# In the second approach you can use function findInterval() 
# and the names of "brea_ks",
letter_grades <- names(brea_ks[findInterval(x=student_scores[, "avg_score"], 
                                            vec=brea_ks)])
letter_grades <- as.factor(letter_grades)

###############
# if you used the following "brea_ks":
brea_ks <- seq(from=35, to=52.5, length.out=8)
names(brea_ks) <- c("C-", "C", "C+", "B-", "B", "B+", "A-", "A")

# then the solution is:
for (brea_k in 2:length(brea_ks)) {
  in_dex <- student_scores[, "avg_score"] >= brea_ks[brea_k-1]
  letter_grades[in_dex] <- names(brea_ks[brea_k])
}  # end for
letter_grades <- as.factor(letter_grades)

# or:

letter_grades <- names(brea_ks[findInterval(x=student_scores[, "avg_score"], 
                                            all.inside=TRUE, 
                                            vec=brea_ks)+1])
letter_grades <- as.factor(letter_grades)
###############


# cbind "letter_grades" to the data frame "student_scores",
student_scores <- cbind(student_scores, letter_grades)
head(student_scores)



##################################
# 2. (15pts) Sort "student_scores" by "avg_score" column, 
# first in descending order, then in ascending order,
# use function order()
student_scores <- student_scores[order(student_scores$avg_score), ]
head(student_scores)
student_scores <- student_scores[order(student_scores$avg_score, decreasing=TRUE), ]
head(student_scores)


# Calculate the average scores for students for each "finance_track" category,
# use the split-apply-combine procedure, and use functions with() and tapply(),
with(student_scores, 
     tapply(avg_score, finance_track, mean)  # end tapply
)  # end with


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
hist(student_scores$avg_score, breaks=brea_ks)

# In the second approach you can use column "student_scores$letter_grade", 
# and functions table() and barplot(), 
cont_table <- table(student_scores$letter_grade)
cont_table <- cont_table[order(names(cont_table), decreasing=TRUE)]
barplot(cont_table)

# extract the "class" of the columns of "student_scores" using 
# functions class() and sapply(),
# make sure that none of the columns are factors, 
# except for "finance_track" and "letter_grades",
sapply(student_scores, class)

# save "student_scores" to a comma-delimited CSV file,
# use function write.csv(),
write.csv(student_scores, row.names=FALSE, file='student_scores.csv')


