library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
thm <- knit_theme$get("acid")
knit_theme$set(thm)
my_var <- 1  # create new object
assign(x="my_var", value=2)  # assign value to existing object
my_var
rm(my_var)  # remove my_var
assign(x="my_var", value=3)  # create new object from name
my_var
# create new object in new environment
new_env <- new.env()  # create new environment
assign("my_var", 3, envir=new_env)  # assign value to name
ls(new_env)  # list objects in "new_env"
new_env$my_var
rm(list=ls())  # delete all objects
sym_bol <- "my_var"  # define symbol containing string "my_var"
assign(sym_bol, 1)  # assign value to "my_var"
ls()
my_var
assign("sym_bol", "new_var")
assign(sym_bol, 1)  # assign value to "new_var"
ls()
sym_bol <- 10
assign(sym_bol, 1)  # can't assign to non-string
rm(list=ls())  # delete all objects
# create individual vectors from column names of EuStockMarkets
for (col_name in colnames(EuStockMarkets)) {
# assign column values to column names
  assign(col_name, EuStockMarkets[, col_name])
}  # end for
ls()
head(CAC)
# create new environment
test_env <- new.env()
# pass string as name to create new object
assign("my_var1", 2, envir=test_env)
# create new object using $ string referencing
test_env$my_var2 <- 1
# list objects in new environment
ls(test_env)
# reference an object by name
test_env$my_var1
# reference an object by string name using get
get("my_var1", envir=test_env)
# retrieve and assign value to object
assign("my_var1", 
 2*get("my_var1", envir=test_env), 
envir=test_env)
get("my_var1", envir=test_env)
# delete environment
rm(test_env)
rm(list=ls())  # remove all objects
var1 <- 1; var2 <- 2
ls()  # list all objects
ls()[1]  # list first object
args(save)  # list arguments of save function
# save "var1" to a binary file
save("var1", file="my_data.RData")
# save first list object "var1" by passing it to the "..." argument
save(ls()[1], file="my_data.RData")  # 'ls()[1]' not evaluated
# save first list object "var1" by passing it to the "list" argument
save(list=ls()[1], file="my_data.RData")
# save whole list by passing it to the "list" argument
save(list=ls(), file="my_data.RData")
rm(list=ls())  # remove all objects
# load objects from file
loaded <- load(file="my_data.RData")
loaded  # vector of loaded objects
ls()  # list objects
# assign new values to objects
sapply(loaded, function(sym_bol) {
  assign(sym_bol, runif(1))
})
# save vector of objects
save(list=loaded, file="my_data.RData")
