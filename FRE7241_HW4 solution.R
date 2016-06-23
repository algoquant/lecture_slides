#################################
### FRE7241 Homework #4 Solution due May 10, 2016
#################################
# Max score 120pts

# The below solutions are examples,
# Slightly different solutions are also possible.

############## Part I
# Summary: Study the relationship between the
# standard deviation of returns and trading volumes
# using regression analysis.
# Calculate the rolling standard deviation of VTI
# returns, and the aggregated trading volumes,
# over monthly end points.
# Create plots and perform a regression of the two.

# Download the file etf_data.RData from NYU Classes,
# and load() it.
# etf_data.RData contains an environment called env_data,
# with OHLC time series data for ETFs, including VTI.

library(quantmod)
load(file="C:/Develop/data/etf_data.RData")


# 1. (20pts) Calculate the VTI daily returns from
# the adjusted close prices.
# You can use functions Ad() and dailyReturn(),

re_turns <- dailyReturn(Ad(env_data$VTI))

# Calculate the monthly end points for re_turns.
# You must use function endpoints().

end_points <- endpoints(env_data$VTI, on="months")

# Calculate the standard deviation of returns over
# monthly end points, and call it vol_at.
# You can use functions period.apply() and sd().

vol_at <- period.apply(re_turns, INDEX=end_points, FUN=sd)

# Calculate the aggregated trading volumes over
# monthly end points, and call it vol_ume.
# You can use functions period.sum() and Vo().

vol_ume <- period.sum(Vo(env_data$VTI), INDEX=end_points)

# Merge vol_at and vol_ume together and call it
# volat_volume. Assign to volat_volume the column
# names "volat" and "volu".
# Remove rows of volat_volume containing NA values.
# You can use functions merge(), colnames(), and
# complete.cases().

volat_volume <- merge(vol_at, vol_ume)
colnames(volat_volume) <- c("volat", "volu")
volat_volume <- volat_volume[complete.cases(volat_volume)]


# 2. (10pts) Plot the columns of volat_volume in a single
# plot in two panels.
# You can use functions x11(), par(), and chart_Series().

x11()
par(mfrow=c(2, 1))
chart_Series(vol_at, name="VTI volatility")
chart_Series(vol_ume, name="VTI volume")

# Plot a scatterplot of the two columns of volat_volume.
# You can use function plot() with the "formula" and
# "data" arguments.
# You must create the formula from the column names of
# volat_volume.
# You can use functions x11(), colnames(), as.formula(),
# and paste().

x11()
plot(
  formula=as.formula(paste(colnames(volat_volume), collapse=" ~ ")),
  data=volat_volume)


# 3. (10pts) Perform a regression of the two columns of
# volat_volume,
# You must create a formula from the column names of
# volat_volume.
# Extract from summary() the regression statistics for
# the slope coefficient: t-value, p-value, adj.r.squared,
# and create a named vector with these statistics.
# You can use functions colnames(), as.formula(), lm(),
# and summary(),

reg_model <- lm(
  as.formula(paste(colnames(volat_volume), collapse=" ~ ")),
  data=volat_volume)

reg_model_sum <- summary(reg_model)

with(reg_model_sum,
     c(tval=coefficients[2, 3],
       pval=coefficients[2, 4],
       adj_rsquared=adj.r.squared))


# 4. (10pts) Perform the Durbin-Watson test for the
# autocorrelations of residuals.
# Plot the residuals, using function plot().
# Can the null hypothesis be rejected in this case?
# Use function dwtest(), from package lmtest.

x11()
plot(reg_model$residuals, t="l")

library(lmtest)  # load lmtest
dwtest(reg_model)


# 5. (10pts) Calculate the month-over-month differences
# of volat_volume, and call it volat_vol_diff.
# Use function diff() and na.omit().

volat_vol_diff <- na.omit(diff(volat_volume))

# Plot a scatterplot of volat_vol_diff, and repeat the
# whole regression analysis from p.3 and p.4 above for
# volat_vol_diff.

x11()
plot(
  formula=as.formula(paste(colnames(volat_volume), collapse=" ~ ")),
  data=volat_vol_diff)

reg_model <- lm(
  as.formula(paste(colnames(volat_volume), collapse=" ~ ")),
  data=volat_vol_diff)

reg_model_sum <- summary(reg_model)

with(reg_model_sum,
     c(tval=coefficients[2, 3],
       pval=coefficients[2, 4],
       adj_rsquared=adj.r.squared))

dwtest(reg_model)



############## Part II
# Summary: Calculate the volume-weighted average price,
# find its crossing points with prices, and plot it.

# Download the file etf_data.RData from NYU Classes,
# and load() it.
# etf_data.RData contains an environment called env_data,
# with OHLC time series data for ETFs, including VTI.

library(quantmod)
load(file="C:/Develop/data/etf_data.RData")


# 1. (10pts) Extract the adjusted close prices from
# VTI into a variable called price_s.
# You can use function Ad() from package quantmod.

price_s <- Ad(env_data$VTI)

# Rename the column name of price_s to "VTI", by 
# dropping ".Adjusted" from the colnames. 
# Use must function strsplit(). 

colnames(price_s) <- strsplit(colnames(price_s), split="[.]")[[1]][1]

# Calculate the 50-day moving average of price_s,
# and merge it with price_s by adding it as the
# last column.
# Rename the last column to "VWAP".
# You must use function VWAP() from package TTR.

price_s <- merge(price_s, 
                 VWAP(price=price_s, volume=Vo(env_data$VTI), n=50))

colnames(price_s)[2] <- "VWAP"

# At this point price_s should be like this:
# tail(price_s)
#               VTI     VWAP
# 2016-04-08 104.20  99.28782
# 2016-04-11 103.91  99.45781
# 2016-04-12 104.92  99.58764
# 2016-04-13 106.15  99.74678
# 2016-04-14 106.11  99.89194
# 2016-04-15 106.06 100.07063

# Calculate a boolean xts series that is TRUE for 
# dates when VTI prices are above their VWAP, and 
# FALSE otherwise, and call it ma_indic. 

ma_indic <- price_s[, 1] > price_s[, 2]


# 2. (10pts) Plot both columns of price_s in the 
# same panel, from "2015-05-01" to "2015-08-01", 
# with custom line colors "black" and "red".
# You must use functions chart_theme(), chart_Series(),
# and you can also use add_TA().

x11()
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("black", "red")
chart_Series(x=price_s["2015-05-01/2015-08-01"],
             name="VTI plus VWAP",
             theme=plot_theme)

# or
plot_theme <- chart_theme()
plot_theme$col$line.col <- "black"
chart_Series(x=price_s["2015-05-01/2015-08-01", 1],
             name="VTI plus VWAP",
             theme=plot_theme)
add_TA(price_s["2015-05-01/2015-08-01", 2], on=1, lwd=2, col="red")

# Add background shading, with "lightgreen" color 
# for dates when the prices are above their VWAP, 
# and "lightgrey" when they're below their VWAP.  
# Add a legend.
# You must use functions add_TA() and legend().

add_TA(ma_indic["2015-05-01/2015-08-01"], on=-1,
       col="lightgreen", border="lightgreen")
add_TA(!ma_indic["2015-05-01/2015-08-01"], on=-1,
       col="lightgrey", border="lightgrey")

# add legend
legend("top", inset=0.05, cex=0.8,title=NULL,
       leg=c("VTI", "VWAP"), lwd=2, bg="white",
       col=c("black", "red"))


# 3. (10pts) Calculate an xts series of dates when 
# the prices cross their VWAP, and call it ma_crosses.
# hint: You can use ma_indic to calculate ma_crosses.
# You can use the functions diff(), abs(), index(),
# and the logical operator ">".

ma_crosses <- (abs(diff(ma_indic)) > 0)
ma_crosses <- index(price_s[ma_crosses])

# You should get the following output:
# head(ma_crosses)
# [1] "2007-03-21" "2007-06-25" "2007-06-27" "2007-06-29" "2007-07-02" "2007-07-24"
# tail(ma_crosses)
# [1] "2015-12-28" "2015-12-29" "2015-12-31" "2016-02-22" "2016-02-23" "2016-02-25"


# 3. (20pts) Calculate the 50-day rolling maximum 
# and minimum of the prices (price_s[, 1]).
# You must use the function runMax() from package TTR. 

roll_max <- runMax(x=price_s[, 1], n=50)
roll_min <- -runMax(x=-price_s[, 1], n=50)

# Calculate the difference between the rolling 
# maximum and minimum and call it ba_nd. 

ba_nd <- roll_max - roll_min

# Calculate the rolling upper (lower) band as the 
# 50-day moving average ("VWAP") plus (minus) 
# one half of ba_nd. 
# Merge the rolling upper and lower bands with 
# price_s by adding them as the last columns. 
# Rename the last columns to "up_band" and "low_band". 

upper_band <- price_s[, "VWAP"] + ba_nd/2
lower_band <- price_s[, "VWAP"] - ba_nd/2
price_s <- merge(price_s, upper_band, lower_band)
colnames(price_s)[2:4] <- c("VWAP", "up_band", "low_band")

# At this point price_s should be like this:
# tail(price_s)
#               VTI      VWAP  up_band low_band
# 2016-04-08 104.20  99.28782 105.9633 92.61229
# 2016-04-11 103.91  99.45781 106.1333 92.78228
# 2016-04-12 104.92  99.58764 106.2632 92.91211
# 2016-04-13 106.15  99.74678 106.7573 92.73625
# 2016-04-14 106.11  99.89194 106.9025 92.88141
# 2016-04-15 106.06 100.07063 107.0812 93.06010

# Calculate two boolean xts series called up_indic 
# and low_indic. 
# up_indic is TRUE for dates when VTI prices are 
# above upper_band, while low_indic is TRUE for 
# dates when VTI prices are below lower_band. 
# They are FALSE otherwise. 

up_indic <- price_s[, 1] > upper_band
low_indic <- price_s[, 1] < lower_band


# 4. (10pts) Plot all four columns of price_s 
# in the same panel, from "2015-01-01" onward, 
# with custom line colors "black", "blue", 
# "red", and "green". 
# You must use functions chart_theme() and
# chart_Series(), and you can also use add_TA().

x11()
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("black", "blue", "red", "green")
chart_Series(x=price_s["2015/"],
             name="VTI plus VWAP",
             theme=plot_theme)

# Add background shading, with "lightgreen" color 
# for dates when up_indic is TRUE, and "coral" 
# when low_indic is TRUE. 
# Add a legend for "VTI" and "VWAP", in the 
# colors "black" and "blue". 
# You must use functions add_TA() and legend().

add_TA(up_indic["2015/"], on=-1,
       col="lightgreen", border="lightgreen")
add_TA(low_indic["2015/"], on=-1,
       col="coral", border="coral")

# add legend
legend("top", inset=0.05, cex=0.8,title=NULL,
       leg=c("VTI", "VWAP"), lwd=2, bg="white",
       col=c("black", "blue"))
