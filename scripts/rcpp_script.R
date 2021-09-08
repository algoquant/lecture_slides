### Rcpp scripts
library(Rcpp)

# verify that rtools are working properly:
devtools::find_rtools()
devtools::has_devel()

## very simple example using cppFunction()
# define Rcpp function
Rcpp::cppFunction("
  int times_two(int x) 
                  { return 2 * x;}
                  ")  # end cppFunction

# run Rcpp function
times_two(3)


## simple examples with code in .cpp file, using sourceCpp()

# source Rcpp function
Rcpp::sourceCpp(file="C:/Develop/R/scripts/rcpp_func.cpp")

times_two(3)

logabs2(seq(1, 15, by=2))

