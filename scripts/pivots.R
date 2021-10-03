# Load data frame with student availability
da_ta <- read.csv(file="C:/Users/Jerzy/Downloads/FRE6871_Test1.csv")
class(da_ta)
colnames(da_ta)

apply(da_ta, MARGIN=2, class)
apply(da_ta, MARGIN=2, function(x) sum(x == "OK"))

row_names <- da_ta[, 1]
da_ta <- da_ta[, -1]

# Split the column names to remove suffix in duplicate names
col_names <- sapply(colnames(da_ta), function(x) {
  strsplit(x, split="[.]")[[1]][1]
})  # end sapply
u_nique <- unique(col_names)

# Combine the columns with same names and coerce into Boolean
da_ta <- sapply(u_nique, function(x) {
  # cat(x, "\n")
  in_dex <- which(col_names == x)
  se_lect <- da_ta[, in_dex]
  if (!is.null(dim(se_lect)))
    as.logical(rowSums(se_lect == "OK"))
  else
    (se_lect == "OK")
})  # end sapply
rownames(da_ta) <- row_names

apply(da_ta, MARGIN=1, sum)
rowSums(da_ta)

# Which students are available at most convenient time
# whi_ch <- da_ta[which.max(rowSums(da_ta)), , drop=FALSE]
whi_ch <- rownames(da_ta)[which.max(rowSums(da_ta)), , drop=FALSE]
whi_ch <- da_ta[12, , drop=FALSE]
colnames(whi_ch)[whi_ch]



