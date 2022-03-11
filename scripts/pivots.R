# Load data frame with student availability
datav <- read.csv(file="C:/Users/Jerzy/Downloads/FRE6871_Test1.csv")
class(datav)
colnames(datav)

apply(datav, MARGIN=2, class)
apply(datav, MARGIN=2, function(x) sum(x == "OK"))

row_names <- datav[, 1]
datav <- datav[, -1]

# Split the column names to remove suffix in duplicate names
colnamev <- sapply(colnames(datav), function(x) {
  strsplit(x, split="[.]")[[1]][1]
})  # end sapply
u_nique <- unique(colnamev)

# Combine the columns with same names and coerce into Boolean
datav <- sapply(u_nique, function(x) {
  # cat(x, "\n")
  indeks <- which(colnamev == x)
  se_lect <- datav[, indeks]
  if (!is.null(dim(se_lect)))
    as.logical(rowSums(se_lect == "OK"))
  else
    (se_lect == "OK")
})  # end sapply
rownames(datav) <- row_names

apply(datav, MARGIN=1, sum)
rowSums(datav)

# Which students are available at most convenient time
# whichv <- datav[which.max(rowSums(datav)), , drop=FALSE]
whichv <- rownames(datav)[which.max(rowSums(datav)), , drop=FALSE]
whichv <- datav[12, , drop=FALSE]
colnames(whichv)[whichv]



