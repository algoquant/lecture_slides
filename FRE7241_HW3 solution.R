#################################
### FRE7241 HW #3 Solution due July 3, 2015
#################################
# Max score 45pts

# The below solutions are examples,
# Slightly different solutions are also possible.


##################################
# extract the price & volume columns using eapply(), from all 
# the objects contained in "env_data"
# use assign() to create new objects (returns) in env_data
# 
# 1. (5pts) Load time series data from file "etf_series.Rdata" (NYU Classes),
# create a new environment called "env_data", for storing the "xts" 
# containing stock prices,
# load data from the file "etf_series.Rdata" into "env_data",
# use functions new.env() and load(), with the "envir" argument,
env_data <- new.env()
load(file="C:/Develop/data/etf_series.Rdata", envir=env_data)


# 2. (20pts) create a function called get_returns_volume(), 
# that accepts an "xts" argument ("x_ts") and an environment 
# argument ("envir"), 
# get_returns_volume() should:
# - extract adjusted prices and volume data from the "xts",
# - calculate returns from adjusted prices,
# - extract the symbol name from the columns of "xts" ("symbol_name"),
# - merge returns with volume data into a single "xts" ("return_volume"),
# - rename the colnames of "return_volume" to "symbol_name.Return" 
#   and "symbol_name.Volume", (replace "symbol_name" with the symbol name),
# - assign (copy) "return_volume" to a object named "symbol_name_rets" 
#   in the "envir" environment, 
# get_returns_volume() should produce the side effect of creating 
# an "xts" object in the "envir" environment containing returns and 
# volume data "from the input "x_ts",
# get_returns_volume() should return invisible the "symbol_name",
# you can use functions Ad(), Vo(), strsplit(), colnames(), 
# paste() (or paste0), assign(), invisible(), and dailyReturn(),
get_returns_volume <- function(x_ts, envir=env_data) {
  re_turn <- dailyReturn(Ad(x_ts))
  vol_ume <- Vo(x_ts)
  symbol_name <- strsplit(colnames(vol_ume), split="[.]")[[1]][1]
  return_volume <- merge(re_turn, vol_ume)
  colnames(return_volume) <- 
    c(paste0(symbol_name, ".Return"), paste0(symbol_name, ".Volume"))
  assign(paste0(symbol_name, "_rets"), return_volume, envir=envir)
  invisible(symbol_name)
}  # end get_returns_volume



# 2. (20pts) create a new environment called "env_returns", 
# for storing "xts" containing stock return and volume data,
# use function new.env(),
env_returns <- new.env()

# apply function get_returns_volume() to a single "xts", to verify 
# it works correctly:
get_returns_volume(env_data$VTI, envir=env_returns)


# perform an eapply() loop to apply get_returns_volume() to all  
# the objects in "env_data", and copy to "env_returns",
# use functions get_returns_volume() and eapply(),
eapply(env_data, get_returns_volume, envir=env_returns)

# remove all files from "env_returns",
rm(list=ls(env_returns), envir=env_returns)

# perform a for() loop to apply get_returns_volume() to all  
# the objects in "env_data", and copy to "env_returns",
# use functions get_returns_volume() and for(),
for (x_ts in ls(env_data)) {
  get_returns_volume(get(x=x_ts, envir=env_data), envir=env_returns)
}  # end for


# save all the objects in the environment "env_returns" 
# to a binary file called "etf_rets_volume.Rdata", 
# use function save(), with the "list" and "envir" arguments,
# make sure to save the objects in the environment, 
# not the environment itself,
save(list=ls(env_returns), envir=env_returns, file="etf_rets_volume.Rdata")

