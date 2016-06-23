#################################
### FRE6871 Test #3 Solutions May 9, 2016
#################################
# Max score 90pts

# The below solutions are examples,
# Slightly different solutions are also possible.

############## Part I
# Summary: Perform tapply() and sapply() loops to aggregate 
# student scores. 

# Download the file student_scores.RData from NYU Classes,
# and load() it.
# student_scores.RData contains the data frame student_scores.

load(file="C:/Develop/data/student_scores.RData")


# 1. (30pts) Calculate the average scores for students 
# in each finance_track category (the average of the 
# "avg_score" column), using the split-apply-combine 
# procedure. 
# You must perform the calculation in two different ways. 
# In the first method you must use functions with(), 
# tapply(), and mean(). 

with(student_scores, 
     tapply(avg_score, finance_track, mean)
)  # end with

# You should get the following output:
# Computational     Corporate      InfoTech       Risk 
#     25.60833      28.53333      20.20000      29.66250

# In the second method you must use functions with(), 
# sapply(), levels(), and mean().  You cannot use tapply(). 
# hint: use argument "USE.NAMES=TRUE" in sapply(). 

with(student_scores, 
     sapply(levels(finance_track), function(x) {
       mean(avg_score[x==finance_track])
     }, USE.NAMES=TRUE))  # end with


# 2. (20pts) Calculate the highest avg_scores in each 
# finance_track category, and call it high_scores. 
# You can use functions with(), tapply(), and max(). 
# Or you can use functions with(), sapply(), 
# levels(), and max().

high_scores <- with(student_scores, 
                    tapply(avg_score, finance_track, max))

# or: 
high_scores <- with(student_scores, 
                    sapply(levels(finance_track), function(x) {
                      max(avg_score[x==finance_track])
                    }, USE.NAMES=TRUE))  # end with


# 3. (20pts) Find the names of the students with the 
# highest avg_score in each finance_track category. 
# You must perform the calculation in two different ways. 
# In the first method you must use high_scores and 
# the function match(). 
# hint: use the "name" column of student_scores. 

student_scores$name[match(high_scores, student_scores$avg_score)]

# You should get the following output:
# [1] William   Jayden    Elizabeth Amelia

# In the second method you must use functions with() 
# and sapply(). 

with(student_scores, 
     sapply(high_scores, function(x) {
       name[x==avg_score]
     })
)  # end with



############## Part II
# Summary: Multiply the columns of a matrix by the 
# elements of a vector. 

# Create a vector and a matrix as follows:

vec_tor <- c(2, 1, 3)
set.seed(1121)
mat_rix <- matrix(sample(1:12), ncol=3)

# 1. (20pts) Multiply mat_rix by vec_tor, so that the 
# first column of mat_rix is multiplied by the first 
# element of vec_tor, the second column by the second 
# element, etc. 
# Call this product mult_matrix. 
# mult_matrix should have the same dimensions as mat_rix. 
# You can use the function t(), and the "*" operator. 
# hint: use the function t() twice. 

mult_matrix <- t(t(mat_rix)*vec_tor)

# You should get the following result:
# > mult_matrix
#      [,1] [,2] [,3]
# [1,]   14    5   33
# [2,]    8   10    3
# [3,]   24    6   24
# [4,]    6    9    6

# Calculate the row sums of mult_matrix. 
# You can use the function rowSums(), 

rowSums(mult_matrix)

# You should get the following result:
# [1] 52 21 54 21

# Calculate the inner product of mat_rix and vec_tor. 
# You can use the "%*%" operator, 

mat_rix %*% vec_tor

# You should get the following result:
#      [,1]
# [1,]   52
# [2,]   21
# [3,]   54
# [4,]   21

