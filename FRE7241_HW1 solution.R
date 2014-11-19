#################################
### HW #1 (lecture #2) Solution
#################################
# due Nov. 24, 2014
# Max score 60pts

# The below solutions are an example,
# Slightly different solutions are also possible.

# 1. create a data directory on your computer, and save all files to that directory,
#    remember the location of the data directory for future use,
#    load package tseries,
library(tseries)

# 2. (5pts) read the ETF database file called "etf_list.csv" into a data frame called "etf_list", using read.csv(),
etf_list <- read.csv(file='etf_list.csv')

# 3. (5pts) create a vector of symbols called "sym_bols",
sym_bols <- c("VTI", "VEU", "IEF", "VNQ", "DBC", "XLY", "XLP", "XLE", "XLF", "XLV", "XLI", "XLB", "XLK", "XLU", "IWB", "IWD", "IWF", "IWM", "IWN", "IWO", "IWP", "IWR", "IWS", "IWV", "IUSV", "IUSG")
#    subset etf_list to include only those ETF's in sym_bols, using the "%in%" operator,
etf_list <- etf_list[etf_list$Symbol %in% sym_bols, ]

# 4. (15pts) download 10yrs of price and volume data for the list of sym_bols, and call it "zoo_series",
#    for each symbol download the fields "AdjClose" and "Volume",
field_names <- c("AdjClose", "Volume")
#    use get.hist.quote() and an lapply() loop,
#    name the list returned by lapply as "zoo_series" (each list element is a zoo object),
zoo_series <- suppressWarnings(
  lapply(sym_bols, # loop for loading data
         get.hist.quote,
         quote=field_names,
         start=Sys.Date()-3650, 
         end=Sys.Date(), 
         origin="1970-01-01")
)  # end suppressWarnings


# 4. (5pts) assign a names() attribute to zoo_series, equal to the sym_bols vector (result should be a named list),
names(zoo_series) <- sym_bols

# 5. (5pts) flatten zoo_series into a single zoo object, using do.call() and merge(),
zoo_series <- do.call(merge, zoo_series)

# 6. (5pts) assign new names() to zoo_series, in the format "XLI.Close", "XLI.Volume", etc.
#    use sapply() and paste(),
names(zoo_series) <- as.vector(sapply(sym_bols, paste, c("Close", "Volume"), sep="."))

# 7. (5pts) save zoo_series to a comma-separated CSV file called "zoo_series.csv", using write.zoo(),
write.zoo(zoo_series, file='zoo_series.csv', sep=",")

# 8. (5pts) save zoo_series to a binary file called "zoo_series.Rdata", using save(),
save(zoo_series, file='zoo_series.Rdata')

# 9. (10pts) plot the time series zoo_series[, "VTI.Close"] using autoplot(),
etf_gg <- autoplot(zoo_series[, "VTI.Close"], 
                   main="Vanguard Total Stock Market ETF") + 
  xlab("") + ylab("") + 
  theme(
    legend.position=c(0.1, 0.5), 
    plot.title=element_text(vjust=-2.0), 
    plot.background=element_blank()
  )  # end theme
# render ggplot
etf_gg

