stop_if_neg <- function(in_put) {
  if(!is.numeric(in_put) || in_put<0)
    stop("argument not numeric or negative")
}  # end stop_if_neg

# wrapper for sqrt()
my_sqrt <- function(in_put) {
  stop_if_neg(in_put)
  sqrt(in_put)
}  # end my_sqrt
my_sqrt(-2)

