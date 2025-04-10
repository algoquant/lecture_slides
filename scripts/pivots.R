# Summary: Analyze a data frame with student availabilities 
# for a test, and find the most convenient times for the test.


## Run the setup code below

# Load data frame with the student availabilities
dframe <- read.csv(file="/Users/jerzy/Develop/lecture_slides/scripts/FRE6871_Test1.csv")
class(dframe)
colnames(dframe)
rownames(dframe)

# The Time column are the times when the students are available 
# for the test.
# The Time column values are also duplicated, due to an error.

# The values in the data frame are "OK" if the student is 
# available at the time given in the column Time.
# Or blank if the student is not available at the time given.

## End of setup code


# 1. (20pts)
# Coerce the columns of the data frame to Boolean, except 
# the first Time column.

dframe[, -1] <- (dframe[, -1] == "OK")

# You should get the following outputs:
class(dframe)
# [1] "data.frame"
sapply(dframe, is.logical)
#  Time          Qisi_Lu      Deniz_Kural      Yiran_Zhang     Rongzhi_Xiao        Tong_Zhan 
# FALSE             TRUE             TRUE             TRUE             TRUE             TRUE 
# Tracy_Yang            Ji_Wu   Titash_Ghoshal       Jinyang_Li      Wendi_Zhang     Dingtian_Zhu 
# TRUE             TRUE             TRUE             TRUE             TRUE             TRUE 
# Elaine_Qiu       Ajay_Dugar         Fan_Yang     Elaine_Qiu.1       Fan_Yang.1     Ajay_Dugar.1 
# TRUE             TRUE             TRUE             TRUE             TRUE             TRUE 
# Qisi_Lu.1    Deniz_Kural.1     Jinyang_Li.1          Ji_Wu.1 Titash_Ghoshal.1    Yiran_Zhang.1 
# TRUE             TRUE             TRUE             TRUE             TRUE             TRUE 
# Qisi_Lu.2   Rongzhi_Xiao.1    Wendi_Zhang.1 
# TRUE             TRUE             TRUE 
dframe[5:11, 1:5]
#               Time      Qisi_Lu Deniz_Kural Yiran_Zhang Rongzhi_Xiao
# 5   4:00 PM to 5:00 PM    TRUE       FALSE       FALSE        FALSE
# 6   5:00 PM to 6:00 PM    TRUE       FALSE       FALSE        FALSE
# 7  12:00 PM to 1:00 PM    TRUE        TRUE        TRUE        FALSE
# 8   1:00 PM to 2:00 PM    TRUE        TRUE        TRUE        FALSE
# 9   2:00 PM to 3:00 PM    TRUE        TRUE        TRUE        FALSE
# 10  3:00 PM to 4:00 PM    TRUE        TRUE        TRUE        FALSE
# 11  4:00 PM to 5:00 PM    TRUE        TRUE        TRUE        FALSE


# The data frame columns are the Time and the student names.
# The student names are duplicated, for example, "Elaine_Qiu" 
# and "Elaine_Qiu.1".
# The duplicate student names have suffixes, like ".1" 
# added to it.

# Remove the suffixes from the student names.
# Hint: Use the function strsplit() function to split 
# the column names.
# You can use the functions sapply(), colnames(), 
# and strsplit().

colnames(dframe) <- sapply(colnames(dframe), function(x) {
  strsplit(x, split="[.]")[[1]][1]
})  # end sapply

# You should get the following output:
unname(colnames(dframe))
#  [1] "Time"           "Qisi_Lu"        "Deniz_Kural"    "Yiran_Zhang"    "Rongzhi_Xiao"   "Tong_Zhan"     
#  [7] "Tracy_Yang"     "Ji_Wu"          "Titash_Ghoshal" "Jinyang_Li"     "Wendi_Zhang"    "Dingtian_Zhu"  
# [13] "Elaine_Qiu"     "Ajay_Dugar"     "Fan_Yang"       "Elaine_Qiu"     "Fan_Yang"       "Ajay_Dugar"    
# [19] "Qisi_Lu"        "Deniz_Kural"    "Jinyang_Li"     "Ji_Wu"          "Titash_Ghoshal" "Yiran_Zhang"
# [25] "Qisi_Lu"        "Rongzhi_Xiao"   "Wendi_Zhang"


# 2. (20pts)
# Combine the columns with the same names, and add 
# their TRUE values together.

# Calculate the unique column names.
colv <- colnames(dframe)
uniqn <- unique(colv)

datav <- sapply(uniqn[-1], function(x) {
  # cat(x, "\n")
  indeks <- which(colv == x)
  selv <- dframe[, indeks]
  if (!is.null(dim(selv)))
    # If selv has multiple columns then sum the rows and coerce to Boolean
    as.logical(rowSums(selv))
  else
    # If selv is a vector then just return it
    selv
}) # end sapply

datav <- as.data.frame(datav)
dframe <- cbind(dframe$Time, datav)
colnames(dframe)[1] <- "Time"
colv <- colnames(dframe)

# You should get the following outputs:
class(dframe)
# [1] "data.frame" 
dim(dframe)
# [1] 34 15


# Combine the rows with the same Time values, 
# and add their TRUE values together.

# Calculate the unique times.
timev <- dframe[, 1]
uniquetime <- unique(timev)

datav <- sapply(uniquetime, function(x) {
  # cat(x, "\n")
  indeks <- which(timev == x)
  selv <- dframe[indeks, -1]
  if (!is.null(dim(selv)))
    # If selv has multiple rows then sum the rows and coerce to Boolean
    as.logical(colSums(selv))
  else
    # If selv is a vector then just return it
    selv
}) # end sapply

datav <- t(datav)
dframe <- as.data.frame(datav)
colnames(dframe) <- colv[-1]

# You should get the following outputs:
class(dframe)
# [1] "data.frame" 
dim(dframe)
# [1] 14 14
dframe
#                      Qisi_Lu Deniz_Kural Yiran_Zhang Rongzhi_Xiao Tong_Zhan Tracy_Yang Ji_Wu Titash_Ghoshal
# 12:00 PM to 1:00 PM     TRUE        TRUE        TRUE        FALSE     FALSE      FALSE FALSE          FALSE
# 1:00 PM to 2:00 PM      TRUE        TRUE        TRUE         TRUE     FALSE      FALSE FALSE          FALSE
# 2:00 PM to 3:00 PM      TRUE        TRUE        TRUE         TRUE     FALSE      FALSE FALSE          FALSE
# 3:00 PM to 4:00 PM      TRUE        TRUE        TRUE         TRUE     FALSE      FALSE FALSE          FALSE
# 4:00 PM to 5:00 PM      TRUE        TRUE        TRUE         TRUE      TRUE       TRUE FALSE           TRUE
# 5:00 PM to 6:00 PM      TRUE       FALSE        TRUE         TRUE      TRUE       TRUE  TRUE           TRUE
# 6:00 PM to 7:00 PM     FALSE       FALSE       FALSE         TRUE     FALSE      FALSE FALSE          FALSE
# 7:00 PM to 8:00 PM     FALSE       FALSE       FALSE         TRUE     FALSE      FALSE FALSE          FALSE
# 8:00 PM to 9:00 PM     FALSE       FALSE       FALSE         TRUE     FALSE      FALSE FALSE          FALSE
# 9:00 PM to 10:00 PM    FALSE       FALSE       FALSE        FALSE     FALSE      FALSE FALSE           TRUE
# 10:00 PM to 11:00 PM   FALSE       FALSE       FALSE        FALSE     FALSE      FALSE  TRUE           TRUE
# 11:00 PM to 12:00 AM   FALSE       FALSE       FALSE        FALSE     FALSE      FALSE  TRUE           TRUE
# 10:00 AM to 11:00 AM   FALSE        TRUE        TRUE        FALSE     FALSE      FALSE FALSE          FALSE
# 11:00 AM to 12:00 PM   FALSE        TRUE        TRUE        FALSE     FALSE      FALSE FALSE          FALSE
#                         Jinyang_Li Wendi_Zhang Dingtian_Zhu Elaine_Qiu Ajay_Dugar Fan_Yang
# 12:00 PM to 1:00 PM        TRUE       FALSE        FALSE       TRUE       TRUE     TRUE
# 1:00 PM to 2:00 PM         TRUE       FALSE        FALSE       TRUE       TRUE     TRUE
# 2:00 PM to 3:00 PM         TRUE       FALSE         TRUE       TRUE       TRUE     TRUE
# 3:00 PM to 4:00 PM         TRUE       FALSE         TRUE       TRUE       TRUE     TRUE
# 4:00 PM to 5:00 PM         TRUE       FALSE         TRUE       TRUE       TRUE     TRUE
# 5:00 PM to 6:00 PM         TRUE        TRUE        FALSE       TRUE       TRUE     TRUE
# 6:00 PM to 7:00 PM        FALSE       FALSE        FALSE      FALSE      FALSE    FALSE
# 7:00 PM to 8:00 PM        FALSE       FALSE        FALSE      FALSE      FALSE    FALSE
# 8:00 PM to 9:00 PM        FALSE       FALSE        FALSE      FALSE      FALSE    FALSE
# 9:00 PM to 10:00 PM       FALSE       FALSE        FALSE      FALSE      FALSE    FALSE
# 10:00 PM to 11:00 PM      FALSE       FALSE        FALSE      FALSE      FALSE    FALSE
# 11:00 PM to 12:00 AM      FALSE       FALSE        FALSE      FALSE      FALSE    FALSE
# 10:00 AM to 11:00 AM       TRUE       FALSE        FALSE      FALSE      FALSE     TRUE
# 11:00 AM to 12:00 PM       TRUE       FALSE        FALSE      FALSE      FALSE     TRUE


# Note that dframe no longer has the Time column, and 
# instead the row names are the times when the students 
# are available.


# 3. (20pts)
# Calculate the number of TRUE values in each column 
# of dframe, which is the number of times each student 
# is available.

sapply(dframe, sum)
# You should get the following output:
# Qisi_Lu    Deniz_Kural    Yiran_Zhang   Rongzhi_Xiao      Tong_Zhan 
#   6              7              8              8              2 
# Tracy_Yang          Ji_Wu Titash_Ghoshal     Jinyang_Li    Wendi_Zhang 
#       2              3              5              8              1 
# Dingtian_Zhu     Elaine_Qiu     Ajay_Dugar       Fan_Yang 
#       3              6              6              8 


# Calculate the number of TRUE values in each row of 
# dframe, which is the number of students available 
# at each time.

numav <- apply(dframe, MARGIN=1, sum)

# You should get the following output:
numav
# 12:00 PM to 1:00 PM   1:00 PM to 2:00 PM   2:00 PM to 3:00 PM   3:00 PM to 4:00 PM 
#         7                    8                    9                    9 
# 4:00 PM to 5:00 PM   5:00 PM to 6:00 PM   6:00 PM to 7:00 PM   7:00 PM to 8:00 PM 
#         12                   12                    1                    1 
# 8:00 PM to 9:00 PM  9:00 PM to 10:00 PM 10:00 PM to 11:00 PM 11:00 PM to 12:00 AM 
#         1                    1                    2                    2 
# 10:00 AM to 11:00 AM 11:00 AM to 12:00 PM 
#     4                    4 


# Find the most convenient times, with the most students available.
# Hint: Use the which function to find the row numbers of the maximum 
# number of students available.
# There may be more than one row with the maximum number of students 
# available.

bestt <- which(numav == max(numav))

# You should get the following output:
bestt
# 4:00 PM to 5:00 PM 5:00 PM to 6:00 PM 
#         5                  6 

# Find the names of the students that are available at the most 
# convenient times.
# Which students are available at the most the convenient times?

names(dframe[bestt[1], ])
names(dframe[bestt[2], ])

# You should get the following outputs:
# [1] "Qisi_Lu"        "Deniz_Kural"    "Yiran_Zhang"    "Rongzhi_Xiao"   "Tong_Zhan"     
# [6] "Tracy_Yang"     "Ji_Wu"          "Titash_Ghoshal" "Jinyang_Li"     "Wendi_Zhang"   
# [11] "Dingtian_Zhu"   "Elaine_Qiu"     "Ajay_Dugar"     "Fan_Yang"      
# and
# [1] "Qisi_Lu"        "Deniz_Kural"    "Yiran_Zhang"    "Rongzhi_Xiao"   "Tong_Zhan"     
# [6] "Tracy_Yang"     "Ji_Wu"          "Titash_Ghoshal" "Jinyang_Li"     "Wendi_Zhang"   
# [11] "Dingtian_Zhu"   "Elaine_Qiu"     "Ajay_Dugar"     "Fan_Yang"  


