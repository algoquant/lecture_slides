# Install package data.table
install.packages("data.table")
# Load package data.table
library(data.table)
# get documentation for package data.table
# get short description
packageDescription("data.table")
# Load help page
help(package="data.table")
# List all datasets in "data.table"
data(package="data.table")
# List all objects in "data.table"
ls("package:data.table")
# Remove data.table from search path
detach("package:data.table")

# Create a data table
library(data.table)
data_table <- data.table::data.table(
  col1=sample(7), col2=sample(7), col3=sample(7))
# Print data_table
class(data_table); data_table
# column referenced without quotes
data_table[, col2]
# row referenced without a following comma
data_table[2]
# Print option "datatable.print.nrows"
getOption("datatable.print.nrows")
options(datatable.print.nrows=10)
getOption("datatable.print.nrows")
# Number of rows in data_table
NROW(data_table)
# Or
data_table[, NROW(col1)]
# Or
data_table[, .N]
# microbenchmark speed of data.table syntax
library(microbenchmark)
summary(microbenchmark(
  dt=data_table[, .N],
  pure_r=NROW(data_table),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# Read a data table from CSV file
dir_name <- "C:/Develop/R/lecture_slides/data/"
file_name <- file.path(dir_name, "weather_delays14.csv")
data_table <- data.table::fread(file_name)
class(data_table); dim(data_table)
data_table
# fread() reads the same data as read.csv()
all.equal(read.csv(file_name, stringsAsFactors=FALSE),
    setDF(data.table::fread(file_name)))
# fread() is much faster than read.csv()
summary(microbenchmark(
  pure_r=read.csv(file_name),
  fread=setDF(data.table::fread(file_name)),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Write data table to file in different ways
data.table::fwrite(data_table, file="data_table.csv")
write.csv(data_table, file="data_table2.csv")
cat(unlist(data_table), file="data_table3.csv")
# microbenchmark speed of data.table::fwrite()
library(microbenchmark)
summary(microbenchmark(
  fwrite=data.table::fwrite(data_table, file="data_table.csv"),
  write_csv=write.csv(data_table, file="data_table2.csv"),
  cat=cat(unlist(data_table), file="data_table3.csv"),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# Select first five rows of data_table
data_table[1:5]
# Select rows with JFK flights
jfk_flights <- data_table[origin=="JFK"]
# Select rows JFK flights in June
jfk_flights <- data_table[origin=="JFK" & month==6]
# Select rows without JFK flights
jfk_flights <- data_table[!(origin=="JFK")]
# Select flights with carrier_delay
data_table[carrier_delay > 0]
# Select column of data_table and return a vector
head(data_table[, origin])
# Select column of data_table and return a data_table, not vector
head(data_table[, list(origin)])
head(data_table[, .(origin)])
# Select two columns of data_table
data_table[, list(origin, month)]
data_table[, .(origin, month)]
column_s <- c("origin", "month")
data_table[, ..column_s]
data_table[, month, origin]
# Select two columns and rename them
data_table[, .(orig=origin, mon=month)]
# Select all columns except origin
head(data_table[, !c("origin")])
head(data_table[, -c("origin")])

# Select flights with positive carrier_delay
data_table[carrier_delay > 0]
# Number of flights with carrier_delay
data_table[, sum(carrier_delay > 0)]
# Or standard R commands
sum(data_table[, carrier_delay > 0])
# microbenchmark speed of data.table syntax
summary(microbenchmark(
  dt=data_table[, sum(carrier_delay > 0)],
  pure_r=sum(data_table[, carrier_delay > 0]),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Average carrier_delay
data_table[, mean(carrier_delay)]
# Average carrier_delay and aircraft_delay
data_table[, .(carrier=mean(carrier_delay),
         aircraft=mean(aircraft_delay))]
# Average aircraft_delay from JFK
data_table[origin=="JFK", mean(aircraft_delay)]
# Number of flights from JFK
data_table[origin=="JFK", NROW(aircraft_delay)]
# Or
data_table[origin=="JFK", .N]

# Number of flights from each airport
data_table[, .N, by=origin]
# Same, but add names to output
data_table[, .(flights=.N), by=.(airport=origin)]
# Number of AA flights from each airport
data_table[carrier=="AA", .(flights=.N),
     by=.(airport=origin)]
# Number of flights from each airport and airline
data_table[, .(flights=.N),
     by=.(airport=origin, airline=carrier)]
# Average aircraft_delay
data_table[, mean(aircraft_delay)]
# Average aircraft_delay from JFK
data_table[origin=="JFK", mean(aircraft_delay)]
# Average aircraft_delay from each airport
data_table[, .(delay=mean(aircraft_delay)),
     by=.(airport=origin)]
# Average and max delays from each airport and month
data_table[, .(mean_delay=mean(aircraft_delay), max_delay=max(aircraft_delay)),
     by=.(airport=origin, month=month)]
# Average and max delays from each airport and month
data_table[, .(mean_delay=mean(aircraft_delay), max_delay=max(aircraft_delay)),
     keyby=.(airport=origin, month=month)]

# Sort ascending by origin, then descending by dest
order_table <- data_table[order(origin, -dest)]
order_table
# Doesn't work outside data_table
order(origin, -dest)
# Sort data_table by reference
setorder(data_table, origin, -dest)
all.equal(data_table, order_table)
# setorder() is much faster than order()
summary(microbenchmark(
  order=data_table[order(origin, -dest)],
  setorder=setorder(data_table, origin, -dest),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Average aircraft_delay by month
order_table[, .(mean_delay=mean(aircraft_delay)),
      by=.(month=month)]
# Chained brackets to sort output by month
order_table[, .(mean_delay=mean(aircraft_delay)),
      by=.(month=month)][order(month)]

# Select weather_delay and aircraft_delay in two different ways
data_table[1:7, .SD,
     .SDcols=c("weather_delay", "aircraft_delay")]
data_table[1:7, .(weather_delay, aircraft_delay)]
# Calculate mean of weather_delay and aircraft_delay
data_table[, sapply(.SD, mean),
     .SDcols=c("weather_delay", "aircraft_delay")]
sapply(data_table[, .SD,
     .SDcols=c("weather_delay", "aircraft_delay")], mean)
# Return origin and dest, then all other columns
data_table[1:7, .SD, by=.(origin, dest)]
# Return origin and dest, then weather_delay and aircraft_delay columns
data_table[1:7, .SD,
     by=.(origin, dest),
     .SDcols="weather_delay", "aircraft_delay"]
# Return first two rows from each month
data_table[, head(.SD, 2), by=.(month)]
data_table[, head(.SD, 2), by=.(month),
     .SDcols=c("weather_delay", "aircraft_delay")]
# Calculate mean of weather_delay and aircraft_delay, grouped by origin
data_table[, lapply(.SD, mean),
     by=.(origin),
     .SDcols=c("weather_delay", "aircraft_delay")]
# Or simply
data_table[, .(weather_delay=mean(weather_delay),
         aircraft_delay=mean(aircraft_delay)),
     by=.(origin)]

# Add tot_delay column
data_table[, tot_delay := (carrier_delay + aircraft_delay)]
head(data_table, 4)
# Delete tot_delay column
data_table[, tot_delay := NULL]
# Add max_delay column grouped by origin and dest
data_table[, max_delay := max(aircraft_delay),
     by=.(origin, dest)]
data_table[, max_delay := NULL]
# Add date and tot_delay columns
data_table[, c("date", "tot_delay") :=
       list(paste(month, day, year, sep="/"),
            (carrier_delay + aircraft_delay))]
# Modify select rows of tot_delay column
data_table[month == 12, tot_delay := carrier_delay]
data_table[, c("date", "tot_delay") := NULL]
# Add several columns
data_table[, c("max_carrier", "max_aircraft") :=
       lapply(.SD, max),
     by=.(origin, dest),
     .SDcols=c("carrier_delay", "aircraft_delay")]
data_table[, c("max_carrier", "max_aircraft") := NULL]
# Modifying by reference is much faster than standard R
summary(microbenchmark(
  dt=data_table[, tot_delay := (carrier_delay + aircraft_delay)],
  pure_r=(data_table[, "tot_delay"] <- data_table[, "carrier_delay"] + data_table[, "aircraft_delay"]),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# Add a key based on the "origin" column
setkey(data_table, origin)
haskey(data_table)
key(data_table)
# Select rows with LGA using the key
data_table["LGA"]
all.equal(data_table["LGA"],
    data_table[origin == "LGA"])
# Select rows with LGA and JFK using the key
data_table[c("LGA", "JFK")]
# Add a key based on the "origin" and "dest" columns
setkey(data_table, origin, dest)
key(data_table)
# Select rows with origin from JFK and MIA
data_table[c("JFK", "MIA")]
# Select rows with origin from JFK and dest to MIA
data_table[.("JFK", "MIA")]
all.equal(data_table[.("JFK", "MIA")],
    data_table[origin == "JFK" & dest == "MIA"])
# Selecting rows using a key is much faster than standard R
summary(microbenchmark(
  with_key=data_table[.("JFK", "MIA")],
  standard_r=data_table[origin == "JFK" & dest == "MIA"],
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# Create data frame and coerce it to data table
data_table <- data.frame(
  col1=sample(7), col2=sample(7), col3=sample(7))
class(data_table); data_table
data.table::setDT(data_table)
class(data_table); data_table
# Coerce data_table into data frame
data.table::setDF(data_table)
class(data_table); data_table
# Or
data_table <- data.table:::as.data.frame.data.table(data_table)
# SetDF() is much faster than as.data.frame()
summary(microbenchmark(
  as.data.frame=data.table:::as.data.frame.data.table(data_table),
  setDF=data.table::setDF(data_table),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# Coerce xts to a data frame
price_s <- rutils::etf_env$VTI
class(price_s); head(price_s)
price_s <- as.data.frame(price_s)
class(price_s); head(price_s)
# Coerce data frame to a data table
data.table::setDT(price_s, keep.rownames=TRUE)
class(price_s); head(price_s)
# Dates are coerced to strings
sapply(price_s, class)
# Coerce xts directly to a data table
data_table <- as.data.table(rutils::etf_env$VTI,
  keep.rownames=TRUE)
class(data_table); head(data_table)
# Dates are not coerced to strings
sapply(data_table, class)
all.equal(price_s, data_table, check.attributes=FALSE)

# Install package fst
install.packages("fst")
# Load package fst
library(fst)
# get documentation for package fst
# get short description
packageDescription("fst")
# Load help page
help(package="fst")
# List all datasets in "fst"
data(package="fst")
# List all objects in "fst"
ls("package:fst")
# Remove fst from search path
detach("package:fst")

# Read a data frame from CSV file
dir_name <- "C:/Develop/R/lecture_slides/data/"
file_name <- file.path(dir_name, "weather_delays14.csv")
data.table::setDF(data_frame)
class(data_frame); dim(data_frame)
# Write data frame to .fst file in different ways
fst::write_fst(data_frame, path="data_frame.fst")
write.csv(data_frame, file="data_frame2.csv")
# microbenchmark speed of fst::write_fst()
library(microbenchmark)
summary(microbenchmark(
  fst=fst::write_fst(data_frame, path="data_frame.csv"),
  write_csv=write.csv(data_frame, file="data_frame2.csv"),
  cat=cat(unlist(data_frame), file="data_frame3.csv"),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# fst::read_fst() reads the same data as read.csv()
all.equal(read.csv(file_name, stringsAsFactors=FALSE),
    fst::read_fst("data_frame.fst"))
# fst::read_fst() is 10 times faster than read.csv()
summary(microbenchmark(
  fst=fst::read_fst("data_frame.fst"),
  read_csv=read.csv(file_name),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# Coerce TAQ xts to a data frame
library(HighFreq)
t_aq <- HighFreq::SPY_TAQ
t_aq <- as.data.frame(t_aq)
class(t_aq)
# Coerce data frame to a data table
data.table::setDT(t_aq, keep.rownames=TRUE)
class(t_aq); head(t_aq)
# Get memory size of data table
format(object.size(t_aq), units="MB")
# Save data table to .fst file
fst::write_fst(t_aq, path="C:/Develop/data/taq.fst")
# Create reference to .fst file similar to a data frame
fs_t <- fst::fst("C:/Develop/data/taq.fst")
class(fs_t)
# Memory size of reference to .fst is very small
format(object.size(fs_t), units="MB")
# Get sizes of all objects in workspace
sort(sapply(mget(ls()), object.size))
# reference to .fst can be treated similar to a data table
dim(t_aq); dim(fs_t)
fst:::print.fst_table(fs_t)
# Subset reference to .fst just like a data table
fs_t[1e4:(1e4+5), ]

va_r <- 0.3/3
va_r  # Printed as "0.1"
va_r - 0.1  # va_r is not equal to "0.1"
va_r == 0.1  # va_r is not equal to "0.1"
print(va_r, digits=10)
print(va_r, digits=16)
# va_r is equal to "0.1" within machine precision
all.equal(va_r, 0.1)
va_r <- (3-2.9)
print(va_r, digits=20)
# Info machine precision of computer R is running on
# ?.Machine
# Machine precision
.Machine$double.eps

va_r <- sqrt(2)
va_r^2  # Printed as "2"
va_r^2 == 2  # va_r^2 is not equal to "2"
print(va_r^2, digits=20)
# va_r^2 is equal to "2" within machine precision
all.equal(va_r^2, 2)
# Numbers with precision 0.1
0.1*(1:10)
# Round to precision 0.1
round(3.675, 1)
# Round to precision 1.0
round(3.675)
# Round to nearest even number
c(round(2.5), round(3.5), round(4.5))
round(4:20/2)  # Round to nearest even number
trunc(3.675)  # Truncate

num_var <- 2
num_var==2
identical(num_var, 2)

identical(num_var, NULL)
# This doesn't work:
# num_var==NULL
is.null(num_var)

vec_tor <- c(2, 4, 6)
vec_tor==2
identical(vec_tor, 2)

# num_ber is equal to "1.0" within machine precision
num_ber <- 1.0 + 2*sqrt(.Machine$double.eps)
all.equal(num_ber, 1.0)

# Info machine precision of computer R is running on
# ?.Machine
# Machine precision
.Machine$double.eps

4.7 %/% 0.5  # Modulo division
4.7 %% 0.5  # Remainder of modulo division
# Reversing modulo division usually
# Returns the original number
(4.7 %% 0.5) + 0.5 * (4.7 %/% 0.5)
# Modulo division of non-integer numbers can
# Produce incorrect results
0.6 %/% 0.2  # Produces 2 instead of 3
6 %/% 2  # use integers to get correct result
# 0.2 stored as binary number
# Slightly larger than 0.2
print(0.2, digits=22)

# Get help for integrate()
?integrate
# Calculate slowly converging integral
func_tion <- function(x) {1/((x+1)*sqrt(x))}
integrate(func_tion, lower=0, upper=10)
integrate(func_tion, lower=0, upper=Inf)
# Integrate function with parameter lamb_da
func_tion <- function(x, lamb_da=1) {
  exp(-x*lamb_da)
}  # end func_tion
integrate(func_tion, lower=0, upper=Inf)
integrate(func_tion, lower=0, upper=Inf,
    lamb_da=2)
# Cumulative probability over normal distribution
pnorm(-2)
integrate(dnorm, low=2, up=Inf)
str(dnorm)
pnorm(-1)
integrate(dnorm, low=2, up=Inf, mean=1)
# Expected value over normal distribution
integrate(function(x) x*dnorm(x),
    low=2, up=Inf)

# Get size of an object
vec_tor <- runif(1e6)
object.size(vec_tor)
format(object.size(vec_tor), units="MB")
# Get sizes of objects in workspace
sort(sapply(ls(), function(ob_ject) {
  format(object.size(get(ob_ject)), units="KB")}))
# Get sizes of all objects in workspace
sort(sapply(mget(ls()), object.size))
sort(sapply(mget(ls()), function(ob_ject) {
  format(object.size(ob_ject), units="KB")}
))
# Get total size of all objects in workspace
format(object.size(x=mget(ls())), units="MB")
# Get sizes of objects in rutils::etf_env environment
sort(sapply(ls(rutils::etf_env), function(ob_ject) {
  object.size(get(ob_ject, rutils::etf_env))}))
sort(sapply(mget(ls(rutils::etf_env), rutils::etf_env),
      object.size))
library(gdata)  # Load package gdata
# Get size of data frame columns
gdata::ll(unit="bytes", mtcars)
# Get names, class, and size of objects in workspace
ob_jects <- gdata::ll(unit="bytes")
# Sort by memory size (descending)
ob_jects[order(ob_jects[, 2], decreasing=TRUE), ]
gdata::ll()[order(ll()$KB, decreasing=TRUE), ]
# Get sizes of objects in etf_env environment
gdata::ll(unit="bytes", etf_env)

library(SOAR)  # Load package SOAR
# Get sizes of objects in workspace
sort(sapply(mget(ls()), object.size))
Store(etf_list)  # Store in object cache
# Get sizes of objects in workspace
sort(sapply(mget(ls()), object.size))
search()  # Get search path for R objects
Ls()  # list object cache
find("etf_list")  # Find object on search path

# Get R memory
v_cells <- gc()["Vcells", "used"]
# Create vector with 1,000,000 cells
va_r_bar <- numeric(1000000)
# Get extra R memory
gc()["Vcells", "used"] - v_cells
# Get total size of all objects in workspace
print(object.size(x=mget(ls())), units="MB")

library(microbenchmark)
va_r <- runif(1e6)
system.time(va_r^0.5)
microbenchmark(sqrt(va_r), va_r^0.5, times=10)

# Calculate cumulative sum of a vector
vec_tor <- runif(1e5)
# Use compiled function
cum_sum <- cumsum(vec_tor)
# Use for loop
cum_sum2 <- vec_tor
for (i in 2:NROW(vec_tor))
  cum_sum2[i] <- (cum_sum2[i] + cum_sum2[i-1])
# Compare the two methods
all.equal(cum_sum, cum_sum2)
# Microbenchmark the two methods
library(microbenchmark)
summary(microbenchmark(
  cumsum=cumsum(vec_tor),
  loop=for (i in 2:NROW(vec_tor))
    vec_tor[i] <- (vec_tor[i] + vec_tor[i-1]),
  times=10))[, c(1, 4, 5)]

library(microbenchmark)
# sum() is a compiled primitive function
sum
# mean() is a generic function
mean
va_r <- runif(1e6)
# sum() is much faster than mean()
summary(
  microbenchmark(sum(va_r), mean(va_r), times=10)
  )[, c(1, 4, 5)]
# any() is a compiled primitive function
any
# any() is much faster than %in% wrapper for match()
summary(
  microbenchmark(any(va_r == 1), {1 %in% va_r}, times=10)
  )[, c(1, 4, 5)]

library(microbenchmark)
mat_rix <- matrix(1:9, ncol=3, # Create matrix
  dimnames=list(paste0("row", 1:3),
          paste0("col", 1:3)))
# Create specialized function
matrix_to_dframe <- function(mat_rix) {
  n_cols <- ncol(mat_rix)
  dframe <- vector("list", n_cols)  # empty vector
  for (in_dex in 1:n_cols)  # Populate vector
    dframe <- mat_rix[, in_dex]
  attr(dframe, "row.names") <-  # Add attributes
    .set_row_names(NROW(mat_rix))
  attr(dframe, "class") <- "data.frame"
  dframe  # Return data frame
}  # end matrix_to_dframe
# Compare speed of three methods
summary(microbenchmark(
  matrix_to_dframe(mat_rix),
  as.data.frame.matrix(mat_rix),
  as.data.frame(mat_rix),
  times=10))[, c(1, 4, 5)]

# Matrix with 5,000 rows
mat_rix <- matrix(rnorm(10000), ncol=2)
# Allocate memory for row sums
row_sums <- numeric(NROW(mat_rix))
summary(microbenchmark(
  row_sums=rowSums(mat_rix),  # end row_sums
  ap_ply=apply(mat_rix, 1, sum),  # end apply
  l_apply=lapply(1:NROW(mat_rix), function(in_dex)
    sum(mat_rix[in_dex, ])),  # end lapply
  v_apply=vapply(1:NROW(mat_rix), function(in_dex)
    sum(mat_rix[in_dex, ]),
    FUN.VALUE=c(sum=0)),  # end vapply
  s_apply=sapply(1:NROW(mat_rix), function(in_dex)
    sum(mat_rix[in_dex, ])),  # end sapply
  for_loop=for (i in 1:NROW(mat_rix)) {
    row_sums[i] <- sum(mat_rix[i,])
  },  # end for
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

big_vector <- rnorm(5000)
summary(microbenchmark(
# Allocate full memory for cumulative sum
  for_loop={cum_sum <- numeric(NROW(big_vector))
    cum_sum[1] <- big_vector[1]
    for (i in 2:NROW(big_vector)) {
      cum_sum[i] <- cum_sum[i-1] + big_vector[i]
    }},  # end for
# Allocate zero memory for cumulative sum
  grow_vec={cum_sum <- numeric(0)
    cum_sum[1] <- big_vector[1]
    for (i in 2:NROW(big_vector)) {
# Add new element to "cum_sum" ("grow" it)
      cum_sum[i] <- cum_sum[i-1] + big_vector[i]
    }},  # end for
# Allocate zero memory for cumulative sum
  com_bine={cum_sum <- numeric(0)
    cum_sum[1] <- big_vector[1]
    for (i in 2:NROW(big_vector)) {
# Add new element to "cum_sum" ("grow" it)
      cum_sum <- c(cum_sum, big_vector[i])
    }},  # end for
  times=10))[, c(1, 4, 5)]

# Disable JIT
jit_level <- compiler::enableJIT(0)
# Create inefficient function
my_mean <- function(x) {
  out_put <- 0; n_elem <- NROW(x)
  for(it in 1:n_elem)
    out_put <- out_put + x[it]/n_elem
  out_put
}  # end my_mean
# Byte-compile function and inspect it
mymean_comp <- compiler::cmpfun(my_mean)
mymean_comp
# Test function
vec_tor <- runif(1e3)
all.equal(mean(vec_tor), mymean_comp(vec_tor), my_mean(vec_tor))
# microbenchmark byte-compile function
summary(microbenchmark(
  mean(vec_tor),
  mymean_comp(vec_tor),
  my_mean(vec_tor),
  times=10))[, c(1, 4, 5)]
# Create another inefficient function
sapply2 <- function(x, FUN, ...) {
  out_put <- vector(length=NROW(x))
  for (it in seq_along(x))
    out_put[it] <- FUN(x[it], ...)
  out_put
}  # end sapply2
sapply2_comp <- compiler::cmpfun(sapply2)
all.equal(sqrt(vec_tor),
  sapply2(vec_tor, sqrt),
  sapply2_comp(vec_tor, sqrt))
summary(microbenchmark(
  sqrt(vec_tor),
  sapply2_comp(vec_tor, sqrt),
  sapply2(vec_tor, sqrt),
  times=10))[, c(1, 4, 5)]
# enable JIT
compiler::enableJIT(jit_level)

# Define functions for profiling
out_er <- function() {fa_st(); sl_ow()}
fa_st <- function() Sys.sleep(0.1)
sl_ow <- function() Sys.sleep(0.2)
# Turn on profiling
Rprof(filename="C:/Develop/data_def/profile.out")
# Run code for profiling
replicate(n=10, out_er())
# Turn off profiling
Rprof(NULL)
# Compile summary of profiling from file
summaryRprof("C:/Develop/data_def/profile.out")

# Profile plotting of regression
profvis::profvis({
  plot(price ~ carat, data=ggplot2::diamonds)
  mod_el <- lm(price ~ carat, data=ggplot2::diamonds)
  abline(mod_el, col="red")
})  # end profvis
# Four methods of calculating matrix column means
mat_rix <- matrix(rnorm(1e5), ncol=5e4)
profvis::profvis({
  mean_s <- apply(mat_rix, 2, mean)
  mean_s <- colMeans(mat_rix)
  mean_s <- lapply(mat_rix, mean)
  mean_s <- vapply(mat_rix, mean, numeric(1))
})  # end profvis
# Four methods of calculating data frame column means
data_frame <- as.data.frame(mat_rix)
profvis::profvis({
  mean_s <- apply(data_frame, 2, mean)
  mean_s <- colMeans(data_frame)
  mean_s <- lapply(data_frame, mean)
  mean_s <- vapply(data_frame, mean, numeric(1))
})  # end profvis
# Profile a shiny app
profvis::profvis(
  shiny::runExample(example="06_tabsets",
            display.mode="normal")
)  # end profvis

vec_tor1 <- rnorm(1000000)
vec_tor2 <- rnorm(1000000)
big_vector <- numeric(1000000)
# Sum two vectors in two different ways
summary(microbenchmark(
  # Sum vectors using "for" loop
  r_loop=(for (i in 1:NROW(vec_tor1)) {
    big_vector[i] <- vec_tor1[i] + vec_tor2[i]
  }),
  # Sum vectors using vectorized "+"
  vec_torized=(vec_tor1 + vec_tor2),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Allocate memory for cumulative sum
cum_sum <- numeric(NROW(big_vector))
cum_sum[1] <- big_vector[1]
# Calculate cumulative sum in two different ways
summary(microbenchmark(
# Cumulative sum using "for" loop
  r_loop=(for (i in 2:NROW(big_vector)) {
    cum_sum[i] <- cum_sum[i-1] + big_vector[i]
  }),
# Cumulative sum using "cumsum"
  vec_torized=cumsum(big_vector),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# Matrix with 5,000 rows
mat_rix <- matrix(rnorm(10000), ncol=2)
# Calculate row sums two different ways
all.equal(rowSums(mat_rix),
  apply(mat_rix, 1, sum))
summary(microbenchmark(
  row_sums=rowSums(mat_rix),
  ap_ply=apply(mat_rix, 1, sum),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

library(microbenchmark)
str(pmax)
# Calculate row maximums two different ways
summary(microbenchmark(
  p_max=
    do.call(pmax.int,
lapply(seq_along(mat_rix[1, ]),
  function(in_dex) mat_rix[, in_dex])),
  l_apply=unlist(
    lapply(seq_along(mat_rix[, 1]),
  function(in_dex) max(mat_rix[in_dex, ]))),
  times=10))[, c(1, 4, 5)]

install.packages("matrixStats")  # Install package matrixStats
library(matrixStats)  # Load package matrixStats
# Calculate row min values three different ways
summary(microbenchmark(
  row_mins=rowMins(mat_rix),
  p_min=
    do.call(pmin.int,
      lapply(seq_along(mat_rix[1, ]),
             function(in_dex)
               mat_rix[, in_dex])),
  as_data_frame=
    do.call(pmin.int,
      as.data.frame.matrix(mat_rix)),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

install.packages("Rfast")  # Install package Rfast
library(Rfast)  # Load package Rfast
# Benchmark speed of calculating ranks
va_r <- 1e3
all.equal(rank(va_r), Rfast::Rank(va_r))
summary(microbenchmark(
  r=rank(va_r),
  fast=Rank(va_r),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Benchmark speed of calculating column medians
va_r <- matrix(1e4, nc=10)
all.equal(matrixStats::colMedians(va_r), Rfast::colMedians(va_r))
summary(microbenchmark(
  r=matrixStats::colMedians(va_r),
  fast=Rfast::colMedians(va_r),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

summary(microbenchmark(  # Assign values to vector three different ways
# Fast vectorized assignment loop performed in C using brackets "[]"
  brack_ets={vec_tor <- numeric(10)
    vec_tor[] <- 2},
# Slow because loop is performed in R
  for_loop={vec_tor <- numeric(10)
    for (in_dex in seq_along(vec_tor))
      vec_tor[in_dex] <- 2},
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
summary(microbenchmark(  # Assign values to vector two different ways
# Fast vectorized assignment loop performed in C using brackets "[]"
  brack_ets={vec_tor <- numeric(10)
    vec_tor[4:7] <- rnorm(4)},
# Slow because loop is performed in R
  for_loop={vec_tor <- numeric(10)
    for (in_dex in 4:7)
      vec_tor[in_dex] <- rnorm(1)},
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# Define function vectorized automatically
my_fun <- function(in_put, pa_ram) {
  pa_ram*in_put
}  # end my_fun
# "in_put" is vectorized
my_fun(in_put=1:3, pa_ram=2)
# "pa_ram" is vectorized
my_fun(in_put=10, pa_ram=2:4)
# Define vectors of parameters of rnorm()
std_devs <-
  structure(1:3, names=paste0("sd=", 1:3))
me_ans <-
  structure(-1:1, names=paste0("mean=", -1:1))
# "sd" argument of rnorm() isn't vectorized
rnorm(1, sd=std_devs)
# "mean" argument of rnorm() isn't vectorized
rnorm(1, mean=me_ans)

# sapply produces desired vector output
set.seed(1121)
sapply(std_devs, function(std_dev) rnorm(n=2, sd=std_dev))
set.seed(1121)
sapply(std_devs, rnorm, n=2, mean=0)
set.seed(1121)
sapply(me_ans,
 function(me_an) rnorm(n=2, mean=me_an))
set.seed(1121)
sapply(me_ans, rnorm, n=2)

# rnorm() vectorized with respect to "std_dev"
vec_rnorm <- function(n, mean=0, sd=1) {
  if (NROW(sd)==1)
    rnorm(n=n, mean=mean, sd=sd)
  else
    sapply(sd, rnorm, n=n, mean=mean)
}  # end vec_rnorm
set.seed(1121)
vec_rnorm(n=2, sd=std_devs)
# rnorm() vectorized with respect to "mean" and "sd"
vec_rnorm <- Vectorize(FUN=rnorm,
        vectorize.args=c("mean", "sd")
)  # end Vectorize
set.seed(1121)
vec_rnorm(n=2, sd=std_devs)
set.seed(1121)
vec_rnorm(n=2, mean=me_ans)

str(sum)
# na.rm is bound by name
mapply(sum, 6:9, c(5, NA, 3), 2:6, na.rm=TRUE)
str(rnorm)
# mapply vectorizes both arguments "mean" and "sd"
mapply(rnorm, n=5, mean=me_ans, sd=std_devs)
mapply(function(in_put, e_xp) in_put^e_xp,
 1:5, seq(from=1, by=0.2, length.out=5))

# rnorm() vectorized with respect to "mean" and "sd"
vec_rnorm <- function(n, mean=0, sd=1) {
  if (NROW(mean)==1 && NROW(sd)==1)
    rnorm(n=n, mean=mean, sd=sd)
  else
    mapply(rnorm, n=n, mean=mean, sd=sd)
}  # end vec_rnorm
# Call vec_rnorm() on vector of "sd"
vec_rnorm(n=2, sd=std_devs)
# Call vec_rnorm() on vector of "mean"
vec_rnorm(n=2, mean=me_ans)

# Create two numeric vectors
vec_tor1 <- sin(0.25*pi*1:10)
vec_tor2 <- cos(0.25*pi*1:10)
# Create third vector using 'ifelse'
vec_tor3 <- ifelse(vec_tor1 > vec_tor2,
          vec_tor1, vec_tor2)
# cbind all three together
vec_tor4 <- cbind(vec_tor1, vec_tor2, vec_tor3)

# Set plotting parameters
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0),
    cex.lab=0.8, cex.axis=0.8, cex.main=0.8,
    cex.sub=0.5)
# Plot matrix
matplot(vec_tor4, type="l", lty="solid",
col=c("green", "blue", "red"),
lwd=2, xlab="", ylab="")
# Add legend
legend(x="bottomright", legend=colnames(vec_tor4),
       title="", inset=0.05, cex=0.8, lwd=2,
       lty=1, col=c("green", "blue", "red"))

library(zoo)  # load package zoo
# show the generic function "merge"
merge
# show the "merge" method dispatched to "zoo" objects
merge.zoo

library(zoo)  # load package zoo
# get all methods for generic function merge()
methods(generic.function="merge")
# get generic function methods applied to "zoo" objects
methods(class="zoo")

# define a generic function
gen_sum <- function(a, b, ...) {
  UseMethod("gen_sum")
}  # end gen_sum

# define method for "numeric" class
gen_sum.numeric <- function(a, b, ...) {
  sum(a, b)
}  # end gen_sum.character

# define method for "character" class
gen_sum.character <- function(a, b, ...) {
  paste(a, "plus", b)
}  # end gen_sum.character

# apply gen_sum to "numeric" objects
gen_sum(1, 2)
# apply gen_sum to "character" objects
gen_sum("a", "b")

# 'cbind' is an internal generic function
cbind

# define "+" method for "character" class
"+.character" <- function(a, b, ...) {
  paste(a, "plus", b)
}  # end +.character
methods("+")  # view methods for "+" operator
# define variables with "character" class
char1 <- "a"
char2 <- "b"
class(char1)
char1 + char2  # add two "character" objects - doesn't work
attributes(char1)  # doesn't have explicit "character" class - only implicit
char1 <- structure("a", class="character")
char2 <- structure("b", class="character")
attributes(char1)  # now has explicit "character" class
# add two "character" objects
char1 + char2

# define object of class "string"
obj_string <- "how are you today?"
class(obj_string) <- "string"
obj_string
# overload "print" method for string objects
print.string <- function(str_ing) {
  print(
    paste(strsplit(str_ing, split=" ")[[1]],
  collapse=" + "))
}  # end print.string
# methods("print")  # view new methods for "print" function
print(obj_string)
obj_string

# overwrite "+" operator
"+" = function(a, b) {
  if (is.character(a) && is.character(b)) {
    paste(a, "plus", b)
  } else {
    .Primitive("+") (a, b)
  }
}
methods("+")  # view methods for "+" operator
# add two "numeric" objects
1 + 2
# add two "character" objects
"a" + "b"

# overwrite "+" operator with a generic function
"+" <- function(a, b, ...) {
  UseMethod("+")
}  # end gen_sum
# define method for "numeric" class
"+.numeric" <- function(a, b, ...) {
  sum(a, b)
}  # end gen_sum.character
# define method for "character" class
"+.character" <- function(a, b, ...) {
  paste(a, "plus", b)
}  # end gen_sum.character
methods("+")  # view methods for "+" operator
# add two "numeric" objects
1 + 2
# add two "character" objects
"a" + "b"

cbind.ts  # can't view non-visible method
stats::cbind.ts  # can't view non-visible method
stats:::cbind.ts  # display non-visible method
getAnywhere(cbind.ts)  # display non-visible method

rm(list=ls())
new_zoo <- zoo(rnorm(10), order.by=(Sys.Date() + 0:9))
# coerce "zoo" object to new class "zoo_xtra"
class(new_zoo) <- "zoo_xtra"
class(new_zoo)
methods(generic.function="length")
length  # primitive function
# define "length" method for class "zoo_xtra"
length.zoo_xtra <- function(in_ts) {
  cat("length of zoo_xtra object:\n")
# unclass object, then calculate length
  NROW(unclass(in_ts))
}  # end length.zoo_xtra
NROW(new_zoo)  # apply "length" method to "zoo_xtra" object
methods(generic.function="length")

# define "last" method for class "zoo_xtra"
last.zoo_xtra <- function(in_ts) {
  in_ts[NROW(in_ts)]
}  # end last.zoo_xtra
last(new_zoo)  # doesn't work
last.zoo_xtra(new_zoo)  # works
# define a generic function
last <- function(a, b, ...) {
  UseMethod("last")
}  # end last
last(new_zoo)  # now works

# define generic "string" class converter
as.string <- function(str_ing, ...)
  UseMethod("as.string")
# default "string" class converter
as.string.default <- function(str_ing, ...)
  structure(str_ing, class="string", ...)
# numeric "string" class converter
as.string.numeric <- function(str_ing, ...)
  structure(as.character(str_ing), class="string", ...)
# "string" class checker
is.string <- function(str_ing)
  inherits(x=str_ing, what="string")
# define "string" object
obj_string <- as.string("how are you today?")
obj_string
is.string(obj_string)
is.string("hello")
as.string(123)
is.string(as.string(123))

rm(list=ls())
library(xts)
new_xts <- xts(rnorm(10), order.by=(Sys.Date() + 0:9))
class(new_xts)  # class attribute is a vector
# "last" is a generic function from package "xts"
last
methods(generic.function="last")
last(new_xts)  # apply "last" method from "xts" class
# derive object "xts_xtra" from "xts" object
class(new_xts) <- c("xts_xtra", class(new_xts))
class(new_xts)  # class attribute is a vector
# "xts_xtra" object inherits "last" method from "xts" class
last(new_xts)

# define new "last" method for class "xts_xtra"
last.xts_xtra <- function(in_ts) {
  cat("last element of xts_xtra object:\n")
  drop(in_ts[NROW(in_ts), ])
}  # end last.xts_xtra
last(new_xts)  # apply "last" from "xts_xtra" class
# define "last" method for class "xts_xtra"
last.xts_xtra <- function(in_ts) {
  cat("last element of xts_xtra object:\n")
  drop(NextMethod())
}  # end last.xts_xtra
last(new_xts)  # apply "last" from "xts_xtra" class

getOption("repos")  # get default package source
.libPaths()  # get package save directory

install.packages("AER")  # install "AER" from CRAN
# install "PerformanceAnalytics" from R-Forge
install.packages(
  pkgs="PerformanceAnalytics",  # name
  lib="C:/Users/Jerzy/Downloads",  # directory
  repos="http://R-Forge.R-project.org")  # source

# install devtools from CRAN
install.packages("devtools")
# load devtools
library(devtools)
# install package "babynames" from GitHub
install_github(repo="hadley/babynames")

# install package "PortfolioAnalytics" from source
install.packages("PortfolioAnalytics",
  type="source",
  repos="http://r-forge.r-project.org")
# download files for package "PortfolioAnalytics"
download.packages(pkgs = "PortfolioAnalytics",
  destdir = ".",  # download to cwd
  type = "source",
  repos="http://r-forge.r-project.org")
# install "PortfolioAnalytics" from local tar source
install.packages(
  "C:/Users/Jerzy/Downloads/PortfolioAnalytics_0.9.3598.tar.gz",
  repos=NULL, type="source")

getOption("defaultPackages")
# matrix of installed package information
pack_info <- installed.packages()
dim(pack_info)
# get all installed package names
sort(unname(pack_info[, "Package"]))
# get a few package names and their versions
pack_info[sample(x=1:100, 5), c("Package", "Version")]
# get info for package "xts"
t(pack_info["xts", ])

# list directories in "PortfolioAnalytics" sub-directory
gsub(
  "C:/Users/Jerzy/Documents/R/win-library/3.1",
  "~",
  list.dirs(
    file.path(
      .libPaths()[1],
      "PortfolioAnalytics")))

# load package, produce error if can't be loaded
library(MASS)
# load package, return TRUE if loaded successfully
require(MASS)
# load quietly
library(MASS, quietly=TRUE)
# load without any messages
suppressMessages(library(MASS))
# remove package from search path
detach(MASS)
# install package if it can't be loaded successfully
if (!require("xts")) install.packages("xts")

# calculate VTI volume-weighted average price
v_wap <- TTR::VWAP(
  price=quantmod::Ad(rutils::etf_env$VTI),
  volume=quantmod::Vo(rutils::etf_env$VTI), n=10)

library()  # list all packages installed on the system
search()  # list all loaded packages on search path

# get documentation for package "Ecdat"
packageDescription("Ecdat")  # get short description
help(package="Ecdat")  # load help page
library(Ecdat)  # load package "Ecdat"
data(package="Ecdat")  # list all datasets in "Ecdat"
ls("package:Ecdat")  # list all objects in "Ecdat"
browseVignettes("Ecdat")  # view package vignette
detach("package:Ecdat")  # remove Ecdat from search path

library(Ecdat)  # load econometric data sets
class(Garch)  # Garch is a data frame from "Ecdat"
dim(Garch)  # daily currency prices
head(Garch[, -2])  # col 'dm' is Deutsch Mark
detach("package:Ecdat")  # remove Ecdat from search path

rm(list=ls())
search()  # get search path for R objects
library(MASS)  # load package "MASS"
head(ls("package:MASS"))  # list some objects in "MASS"
detach("package:MASS")  # remove "MASS" from search path

loadedNamespaces()  # get names of loaded namespaces

search()  # get search path for R objects

# get session info,
# including packages not attached to the search path
sessionInfo()

plot.xts  # package xts isn't loaded and attached
head(xts::plot.xts, 3)
methods("cbind")  # get all methods for function "cbind"
stats::cbind.ts  # cbind isn't exported from package stats
stats:::cbind.ts  # view the non-visible function
getAnywhere("cbind.ts")
library(MASS)  # load package 'MASS'
select  # code of primitive function from package 'MASS'

getAnywhere("cbind.ts")

library(quantmod)
car_s <- mtcars[sample(NROW(mtcars), 10), ]
# Plot scatterplot horsepower vs miles per gallon
plot(car_s[, "hp"], car_s[, "mpg"],
     xlab="horsepower", ylab="miles per gallon",
     main="miles per gallon vs horsepower")
# Add a solid red point (pch=16) for the last car
points(x=car_s[NROW(car_s), "hp"],
 y=car_s[NROW(car_s), "mpg"],
 col="red", pch=16)
# Add labels with the car names
text(x=car_s[, "hp"], y=car_s[, "mpg"],
     labels=rownames(car_s[, ]),
     pos=1, cex=0.8)
# Labels using wordcloud, to prevent overlaps
library(wordcloud)
textplot(x=car_s[, "hp"], y=car_s[, "mpg"],
   words=rownames(car_s))

# Plot the tree Height
plot(trees[, "Height"],
     type="l",
     lwd=2,
     col="blue",
     main="Tree heights and volumes",
     xlab="tree number", ylab="",
     ylim=c(min(trees[, c("Height", "Volume")]),
      max(trees[, c("Height", "Volume")])))
# Plot the tree Volume
lines(trees[, "Volume"], lwd=2, col="green")
# Add legend
legend(x="left", legend=c("Height", "Volume"),
 inset=0.1, cex=1.0, bg="white", bty="n",
 lwd=2, lty=1, col=c("blue", "green"))

x_var <- seq(-2*pi, 2*pi, len=100)  # x values

# open Windows graphics device
x11(width=11, height=7, title="simple plot")

# Plot a sine function using basic line plot
plot(x=x_var, y=sin(x_var), xlab="x-val",
     ylab="y-val", type="l", lwd=2, col="red")
# Add a cosine function
lines(x=x_var, y=cos(x_var), lwd=2, col="blue")
# Add title
title(main="sine and cosine functions", line=0.1)
# Add legend
legend(x="topright", legend=c("sine", "cosine"),
 title="legend", inset=0.1, cex=1.0, bg="white",
 lwd=2, lty=1, bty="n", col=c("red", "blue"))
graphics.off()  # Close all graphics devices

par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# Plot a Normal probability distribution
curve(expr=dnorm, xlim=c(-3, 3),
xlab="", ylab="", lwd=2, col="blue")
# Add shifted Normal probability distribution
curve(expr=dnorm(x, mean=1), add=TRUE,
lwd=2, col="red")

# Add title
title(main="Normal probability distribution functions",
line=0.1)
# Add legend
legend(x="topright", legend=c("Normal", "shifted"),
 title="legend", inset=0.05, cex=0.8, bg="white",
 lwd=2, lty=1, bty="n", col=c("blue", "red"))

par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0))
library(zoo)  # Load zoo
load(file="C:/Develop/R/lecture_slides/data/zoo_data.RData")
zoo_series <- window(zoo_stx[, "AdjClose"],
   start=as.Date("2013-01-01"),
   end=as.Date("2013-12-31"))
# extract time index and monthly dates
in_dex <- index(zoo_series)
# Coerce index to monthly dates
month_ly <- as.yearmon(in_dex)
# tick locations at beginning of month
tick_s <- in_dex[match(unique(month_ly), month_ly)]
# tick_s <- as.Date(tapply(X=in_dex, INDEX=month_ly, FUN=min))
# first plot zoo without "x" axis
plot(zoo_series, xaxt="n", xlab=NA, ylab=NA, main="MSFT stock prices")
# Add "x" axis with monthly ticks
axis(side=1, at=tick_s,
 labels=format(tick_s, "%b-%y"), tcl=-0.7)
# Add vertical lines
abline(v=tick_s, col="grey", lwd=0.5)
# Plot zoo using base plotting functions
plot(as.vector(zoo_series), xaxt="n",
 xlab=NA, ylab=NA, t="l", main="MSFT stock prices")
a_t <- match(tick_s, in_dex)
# a_t <- seq_along(in_dex)[in_dex %in% tick_s]
# Add "x" axis with monthly ticks
axis(side=1, at=a_t,
 labels=format(tick_s, "%b-%y"), tcl=-0.7)
abline(v=a_t, col="grey", lwd=0.5)

library(zoo)  # Load zoo
load(file="C:/Develop/R/lecture_slides/data/zoo_data.RData")
# extract time index and monthly dates
in_dex <- index(zoo_stx)
# Coerce index to monthly dates
month_ly <- as.yearmon(in_dex)
# benchmark two methods of calculating tick locations
library(microbenchmark)
summary(microbenchmark(
m_atch=
  in_dex[match(unique(month_ly), month_ly)],
t_apply=
  as.Date(tapply(X=in_dex,
                 INDEX=month_ly, FUN=min)),
times=10)
  )[, c(1, 4, 5)]

load(file="C:/Develop/R/lecture_slides/data/zoo_data.RData")
# Set plot margines
par(mar=c(3, 3, 3, 3), oma=c(0, 0, 0, 0))
par(las=1)  # Set text printing to horizontal
Plot with two y-axes - plot first time series
zoo::plot.zoo(zoo_stxeur[, 1], lwd=2, xlab=NA, ylab=NA)
par(new=TRUE)  # Allow new plot on same chart
# Plot second time series without y-axis
zoo::plot.zoo(zoo_stxeur[, 2], xlab=NA, ylab=NA,
     lwd=2, yaxt="n", col="red")
# Plot second y-axis on right
axis(side=4, col="red")
# Add axis labels
col_names <- colnames(zoo_stxeur)
mtext(col_names[1], side=2, adj=-0.5)
mtext(col_names[2], side=4, adj=1.5, col="red")
# Add title and legend
title(main=paste0(col_names, collapse=" and "),
line=0.5)
legend("top", legend=col_names,
  bg="white", lty=1, lwd=6,
  col=c("black", "red"), bty="n")

Slightly different method using par("usr")
par(las=1)  # Set text printing to horizontal
zoo::plot.zoo(zoo_stxeur[, 1], xlab=NA, ylab=NA, lwd=2)
# Set range of "y" coordinates for second axis
par(usr=c(par("usr")[1:2], range(zoo_stxeur[,2])))
lines(zoo_stxeur[, 2], col="red", lwd=2)  # Second plot
axis(side=4, col="red")  # Second y-axis on right
# Add axis labels
mtext(col_names[1], side=2, adj=-0.5)
mtext(col_names[2], side=4, adj=1.5, col="red")
# Add title and legend
title(main=paste0(col_names, collapse=" and "),
line=0.5)
legend("top", legend=col_names,
  bg="white", lty=1, lwd=6,
  col=c("black", "red"), bty="n")

graph_params <- par()  # get existing parameters
par("mar")  # get plot margins
par(mar=c(2, 1, 2, 1))  # Set plot margins
par(oma=c(1, 1, 1, 1))  # Set outer margins
par(mgp=c(2, 1, 0))  # Set title and label margins
par(cex.lab=0.8,  # Set font scales
    cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(las=1)  # Set axis labels to horizontal
par(ask=TRUE)  # Pause, ask before plotting
par(mfrow=c(2, 2))  # Plot on 2x2 grid by rows
for (i in 1:4) {  # Plot 4 panels
  barplot(sample(1:6), main=paste("panel", i),
    col=rainbow(6), border=NA, axes=FALSE)
  box()
}  # end for
par(ask=FALSE)  # Restore automatic plotting
par(new=TRUE)  # Allow new plot on same chart
par(graph_params)  # Restore original parameters

x_var <- seq(-5, 7, length=100)
y_var <- dnorm(x_var, mean=1.0, sd=2.0)
plot(x_var, y_var, type="l", lty="solid",
     xlab="", ylab="")
title(main="Normal Density Function", line=0.5)
star_t <- 3; fin_ish <- 5  # Set lower and upper bounds
# Set polygon base
are_a <- ((x_var >= star_t) & (x_var <= fin_ish))
polygon(c(star_t, x_var[are_a], fin_ish),  # Draw polygon
  c(-1, y_var[are_a], -1), col="red")

par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
sig_mas <- c(0.5, 1, 1.5, 2)  # Sigma values
# Create plot colors
col_ors <- c("red", "black", "blue", "green")
# Create legend labels
lab_els <- paste("sigma", sig_mas, sep="=")
for (in_dex in 1:4) {  # Plot four curves
curve(expr=dnorm(x, sd=sig_mas[in_dex]),
xlim=c(-4, 4),
xlab="", ylab="", lwd=2,
col=col_ors[in_dex],
add=as.logical(in_dex-1))
}  # end for
# Add title
title(main="Normal Distributions", line=0.5)
# Add legend
legend("topright", inset=0.05, title="Sigmas",
 lab_els, cex=0.8, lwd=2, lty=1, bty="n",
 col=col_ors)

rm(list=ls())
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
x_var <- seq(-4, 4, length=100)
sig_mas <- c(0.5, 1, 1.5, 2)  # Sigma values
# Create plot colors
col_ors <- c("red", "black", "blue", "green")
# Create legend labels
lab_els <- paste("sigma", sig_mas, sep="=")
# Plot the first chart
plot(x_var, dnorm(x_var, sd=sig_mas[1]),
     type="n", xlab="", ylab="",
     main="Normal Distributions")
# Add lines to plot
for (in_dex in 1:4) {
  lines(x_var, dnorm(x_var, sd=sig_mas[in_dex]),
  lwd=2, col=col_ors[in_dex])
}  # end for
# Add legend
legend("topright", inset=0.05, title="Sigmas",
 lab_els, cex=0.8, lwd=2, lty=1, bty="n",
 col=col_ors)

x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
deg_free <- c(2, 5, 8, 11)  # df values
# Create plot colors
col_ors <- c("red", "black", "blue", "green")
# Create legend labels
lab_els <- paste("df", deg_free, sep="=")
for (in_dex in 1:4) {  # Plot four curves
curve(expr=dchisq(x, df=deg_free[in_dex]),
      xlim=c(0, 20), ylim=c(0, 0.3),
      xlab="", ylab="", lwd=2,
      col=col_ors[in_dex],
      add=as.logical(in_dex-1))
}  # end for

# Add title
title(main="Chi-squared Distributions", line=0.5)
# Add legend
legend("topright", inset=0.05,
       title="Degrees of freedom", lab_els,
       cex=0.8, lwd=6, lty=1, bty="n", col=col_ors)

x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
deg_free <- c(2, 5, 8, 11)  # df values
# Create plot colors
col_ors <- c("red", "black", "blue", "green")
# Create legend labels
lab_els <- paste("df", deg_free, sep="=")
# Plot an empty chart
x_var <- seq(0, 20, length=100)
plot(x_var, dchisq(x_var, df=deg_free[1]),
     type="n", xlab="", ylab="", ylim=c(0, 0.3))
# Add lines to plot
for (in_dex in 1:4) {
  lines(x_var, dchisq(x_var, df=deg_free[in_dex]),
lwd=2, col=col_ors[in_dex])
}  # end for

# Add title
title(main="Chi-squared Distributions", line=0.5)
# Add legend
legend("topright", inset=0.05,
       title="Degrees of freedom", lab_els,
       cex=0.8, lwd=6, lty=1, bty="n", col=col_ors)

x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
deg_free <- c(3, 6, 9)  # df values
col_ors <- c("black", "red", "blue", "green")
lab_els <- c("normal", paste("df", deg_free, sep="="))
# Plot a Normal probability distribution
curve(expr=dnorm, xlim=c(-4, 4),
      xlab="", ylab="", lwd=2)
for (in_dex in 1:3) {  # Plot three t-distributions
curve(expr=dt(x, df=deg_free[in_dex]),
      lwd=2, col=col_ors[in_dex+1], add=TRUE)
}  # end for

# Add title
title(main="t-distributions", line=0.5)
# Add legend
legend("topright", inset=0.05, bty="n",
       title="Degrees\n of freedom", lab_els,
       cex=0.8, lwd=6, lty=1, col=col_ors)

x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
x_var <- seq(-4, 4, length=100)
deg_free <- c(3, 6, 9)  # df values
col_ors <- c("black", "red", "blue", "green")
lab_els <- c("normal", paste("df", deg_free, sep="="))
# Plot chart of normal distribution
plot(x_var, dnorm(x_var), type="l",
     lwd=2, xlab="", ylab="")
for (in_dex in 1:3) {  # Add lines for t-distributions
  lines(x_var, dt(x_var, df=deg_free[in_dex]),
lwd=2, col=col_ors[in_dex+1])
}  # end for

# Add title
title(main="t-distributions", line=0.5)
# Add legend
legend("topright", inset=0.05, bty="n",
       title="Degrees\n of freedom", lab_els,
       cex=0.8, lwd=6, lty=1, col=col_ors)

x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
# Plot the Normal and Cauchy probability distributions
curve(expr=dnorm, xlim=c(-4, 4), xlab="", ylab="", lwd=2)
curve(expr=dcauchy, lwd=3, col="blue", add=TRUE)
# Add title
title(main="Cauchy and Normal Distributions", line=0.5)
# Add legend
legend("topright", inset=0.05, bty="n",
       title=NULL,leg=c("Normal", "Cauchy"),
       cex=0.8, lwd=6, lty=1, col=c("black", "blue"))

x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
deg_free <- c(3, 5, 9)  # df values
col_ors <- c("black", "red", "blue", "green")
lab_els <- paste0("df1=", deg_free, ", df2=3")
for (in_dex in 1:NROW(deg_free)) {  # Plot four curves
curve(expr=df(x, df1=deg_free[in_dex], df2=3),
      xlim=c(0, 4),
      xlab="", ylab="", lwd=2,
      col=col_ors[in_dex],
      add=as.logical(in_dex-1))
}  # end for

# Add title
title(main="F-Distributions", line=0.5)
# Add legend
legend("topright", inset=0.05, bty="n",
       title="degrees of freedom", lab_els,
       cex=0.8, lwd=2, lty=1, col=col_ors)

rm(list=ls())
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
poisson_events <- 0:11  # Poisson events
poisson_freq <- dpois(poisson_events, lambda=4)
names(poisson_freq) <- as.character(poisson_events)
# Poisson function
poisson_func <- function(x, lambda)
              {exp(-lambda)*lambda^x/factorial(x)}
curve(expr=poisson_func(x, lambda=4), xlim=c(0, 11), main="Poisson distribution",
xlab="No. of events", ylab="Frequency of events", lwd=2, col="red")
legend(x="topright", legend="Poisson density", title="", bty="n",
 inset=0.05, cex=0.8, bg="white", lwd=4, lty=1, col="red")

# generate Poisson variables
pois_counts <- rpois(1000, lambda=4)
head(pois_counts)
# Calculate contingency table
pois_table <- table(pois_counts)
pois_table

# Create barplot of table data
barplot(pois_table, col="lightgrey",
  xlab="counts", ylab="number of observations",
  main="barplot of Poisson count data")

# Create histogram of Poisson variables
histo_gram <- hist(pois_counts, col="lightgrey", xlab="count",
     ylab="frequency", freq=FALSE, main="Poisson histogram")
lines(density(pois_counts, adjust=1.5), lwd=2, col="blue")
# Poisson probability distribution function
poisson_func <- function(x, lambda)
  {exp(-lambda)*lambda^x/factorial(x)}
curve(expr=poisson_func(x, lambda=4), xlim=c(0, 11), add=TRUE, lwd=2, col="red")
# Add legend
legend("topright", inset=0.05, title="Poisson histogram",
 c("histogram density", "probability"), cex=0.8, lwd=2,
 lty=1, bty="n", col=c("blue", "red"))
# total area under histogram
diff(histo_gram$breaks) %*% histo_gram$density

# boxplot of Poisson count data
boxplot(x=pois_counts, ylab="counts",
  main="Poisson box plot")
# boxplot method for formula
boxplot(formula=mpg ~ cyl, data=mtcars,
  main="Mileage by number of cylinders",
  xlab="Cylinders", ylab="Miles per gallon")

library(zoo)  # Load zoo
library(ggplot2)  # Load ggplot2
library(scales)  # Load scales
my_ggplot <- ggplot(  # Specify data and aesthetics
  data=mtcars, mapping=aes(x=hp, y=mpg)) +
  geom_point() +  # Plot points
  ggtitle("basic scatterplot") +  # Add title
  theme(  # Customize plot object
  plot.title=element_text(vjust=-2.0),
  plot.background=element_blank()
  )  # end theme
my_ggplot  # Render the plot

# install.packages("directlabels", repo="http://r-forge.r-project.org")
library(ggplot2)  # Load ggplot2
library(scales)  # Load scales
library(gridExtra)  # Load gridExtra
library(directlabels)  # Load directlabels
my_ggplot <- ggplot(  # Data and aesthetics
  data=mtcars, mapping=aes(x=hp, y=mpg)) +
  geom_point() +  # Plot points
  theme(  # Customize plot object
  legend.position="none",
  plot.title=element_text(vjust=-2.0),
  plot.margin=unit(c(-0.0,0.0,-0.5,0.0),"cm"),
  plot.background=element_blank()
  ) +
  scale_colour_discrete(guide="none")  # no label guide
car_names <- rownames(mtcars)
gg_labels <- geom_text(aes(  # ggplot2 labels
  label=car_names, color=car_names, size=5))
d_labels <- geom_dl(mapping=aes(  # Directlabels
  label=car_names, color=car_names),
  method=list("last.bumpup", cex=0.7,
        hjust=1))
# Render plots in single column
grid.arrange(my_ggplot +
  ggtitle("ggplot2 labels") + gg_labels,
  my_ggplot + ggtitle("directlabels") +
    d_labels, ncol=1)  # end grid.arrange

my_ggplot <- ggplot(data=iris,
      mapping=aes(Petal.Length, Sepal.Length)) +
  geom_point(aes(shape=Species, color=Species)) +
  geom_dl(aes(label=Species, color=Species),
    method="smart.grid") +
  scale_shape_manual(values=c(setosa=1,
    virginica=6, versicolor=3), guide="none") +
  scale_colour_discrete(guide="none")  # no label guide
my_ggplot  # Render the plot

library(ggplot2)  # Load ggplot2
library(scales)  # Load scales
library(gridExtra)  # Load gridExtra
# Coerce mts object into zoo
zoo_series <- as.zoo(EuStockMarkets)
# Create ggplot2 theme object
auto_theme <- theme(
  legend.position="none",
  plot.title=element_text(vjust=-2.0),
  plot.margin=unit(c(-0.0,0.0,-0.5,0.0),"cm"),
#  axis.text.y=element_blank(),
  plot.background=element_blank()
  )  # end theme
# ggplot2 object for plotting in single panel
ggp_zoo_single <- autoplot(zoo_series,
            main="Eu Stox single panel",
            facets=NULL) + xlab("") +
            auto_theme
# ggplot2 object for plotting in multiple panels
ggp_zoo_multiple <- autoplot(zoo_series,
            main="Eu Stox multiple panels",
            facets="Series ~ .") + xlab("") +
            facet_grid("Series ~ .",
            scales="free_y") + auto_theme
# Render plots in single column
grid.arrange(ggp_zoo_single +
         theme(legend.position=c(0.1, 0.5)),
       ggp_zoo_multiple, ncol=1)

library(zoo)  # Load zoo
library(ggplot2)  # Load ggplot2
library(gridExtra)
#
auto_theme <- theme(legend.position=c(0.1, 0.5),
  plot.title=element_text(vjust=-2.0),
  plot.margin=unit(c(-0.5, 0.0, -0.5, 0.0), "cm"),
  plot.background=element_blank(),
  axis.text.y=element_blank()
  )

# Plot ggplot2 in single pane
ggp.zoo1 <- autoplot(zoo_series, main="Eu Stox",
   facets=NULL) + xlab("") +
  theme(legend.position=c(0.1, 0.5),
  plot.title=element_text(vjust=-2.0),
  plot.margin=unit(c(-0.5, 0.0, -0.5, 0.0), "cm"),
  plot.background=element_blank(),
  axis.text.y=element_blank()
  )
# Plot ggplot2 in multiple panes
ggp.zoo2 <- autoplot(zoo_series, main="Eu Stox",
   facets=Series ~ .) + xlab("") +
  theme(legend.position=c(0.1, 0.5),
  plot.title=element_text(vjust=-2.0),
  plot.margin=unit(c(-0.5, 0.0, -0.5, 0.0), "cm"),
  plot.background=element_blank(),
  axis.text.y=element_blank()
  )
# Create plot ggplot2 in multiple panes
grid.arrange(ggp.zoo1, ggp.zoo2, ncol=1)

# Define function of two variables
sur_face <- function(x, y) sin(sqrt(x^2+y^2))
# Calculate function over matrix grid
x_lim <- seq(from=-10, to=10, by=0.2)
y_lim <- seq(from=-10, to=10, by=0.2)
# Draw 3d surface plot of function
persp(z=outer(x_lim, y_lim, FUN=sur_face),
theta=45, phi=30, zlab="sine",
shade=0.1, col="green",
main="radial sine function")

library(rgl)  # Load rgl
with(iris,
     plot3d(Sepal.Length, Sepal.Width, Petal.Length,
      type="s", col=as.numeric(Species)))

library(rgl)  # Load rgl
# Define function of two variables
sur_face <- function(x, y) y*sin(x)
# Draw 3d surface plot of function
persp3d(x=sur_face, xlim=c(-5, 5), ylim=c(-5, 5),
  col="green", axes=FALSE)
# Draw 3d surface plot of matrix
x_lim <- seq(from=-5, to=5, by=0.1)
y_lim <- seq(from=-5, to=5, by=0.1)
persp3d(z=outer(x_lim, y_lim, FUN=sur_face),
  xlab="x", ylab="y", zlab="sur_face",
  col="green")
# Save current view to png file
rgl.snapshot("surface_plot.png")
# Define function of two variables and two parameters
sur_face <- function(x, y, lambda_1=1, lambda_2=1)
  sin(lambda_1*x)*sin(lambda_2*y)
# Draw 3d surface plot of function
persp3d(x=sur_face, xlim=c(-5, 5), ylim=c(-5, 5),
  col="green", axes=FALSE,
  lambda_1=1, lambda_2=2)
