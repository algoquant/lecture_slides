

###
# scripts for "Buy-and-Hold Strategies"
# https://www.portfolioeffect.com/blog/2015/10/14/intraday-strategy-backtesting-in-r-part-1-buy-and-hold-strategies/


# Create empty portfolio
port_folio <- portfolio_create("2014-10-01 09:30:00", "2014-10-02 16:00:00")

# Add position AAPL and GOOG to portfolio
portfolio_addPosition(port_folio, "AAPL", 100)
portfolio_addPosition(port_folio, "GOOG", 200)

# position_price() returns matrix with two columns: time in miliseconds, and price
price_AAPL <- position_price(port_folio, "AAPL")
foo <- price_AAPL[seq(from=1, to=nrow(price_AAPL), by=1000)]
head(price_AAPL)
dim(price_AAPL)
class(price_AAPL)
foo <- price_AAPL[1, ]
foo[1]%/%1000
foo[1]%%1000
sprintf(fmt="%f", foo[1])
foo <- price_AAPL
foo[ , 1] <- foo[ , 1]/1000
foo <- xts(foo[ , 2], order.by=as.POSIXct(foo[ , 1], origin="1970-01-01"))
colnames(foo) <- "AAPL"
head(foo)
chart_Series(x=foo, name="AAPL prices")

# suppress exponential notation and increase display digits
options(scipen=100, digits=6)
head(foo)
as.POSIXct("2014-10-01 09:30:00", origin="1970-01-01")
as.POSIXct(foo[1 , 1], origin="1970-01-01")

plot(port_folio)

print(port_folio)

# Plot portfolio value changes over time 
util_plot2d(portfolio_value(port_folio), title="Portfolio value in USD")

# Compute portfolio and position expected return (daily)
util_plot2d(position_expectedReturn(port_folio, "AAPL"), title="Expected Return, daily", Legend="AAPL") + 
  util_line2d(position_expectedReturn(port_folio, "GOOG"), Legend="GOOG") + 
  util_line2d(portfolio_expectedReturn(port_folio), Legend="Portfolio")

# Compute portfolio and position variance (daily)
util_plot2d(position_variance(port_folio, "AAPL"), title="Variance, daily", Legend="AAPL") + 
  util_line2d(position_variance(port_folio, "GOOG"), Legend="GOOG") + 
  util_line2d(portfolio_variance(port_folio), Legend="Portfolio")

#  Compute portfolio and position Sharpe Ratio (daily)
util_plot2d(position_sharpeRatio(port_folio, "AAPL"), title="Sharpe Ratio, daily", Legend="AAPL") + 
  util_line2d(position_sharpeRatio(port_folio, "GOOG"), Legend="GOOG") + 
  util_line2d(portfolio_sharpeRatio(port_folio), Legend="Portfolio")


###
# scripts for "Rule-based Strategies"
# https://www.portfolioeffect.com/blog/2015/10/14/intraday-strategy-backtesting-in-r-part-2-rule-based-strategies/


# create function for moving average over a vector
mov_avg <- function(x, win_dow){
  out_put <- x
  n_row <- NROW(x)
  cum_sum <- cumsum(x)
  out_put[(win_dow+1):n_row] <- 
    (cum_sum[-(1:win_dow)]-cum_sum[-((n_row-win_dow+1):n_row)])/win_dow
  out_put[1:win_dow] <- cumsum(x[1:win_dow])/(1:win_dow)
  return(out_put-0.0000000001)
}  # end mov_avg


sym_bol <- "GOOG"
date_start <- "2014-10-13 09:30:00"
date_end <- "2014-10-14 16:00:00"

highFrequencyPortfolio <- portfolio_create(fromTime=date_start, toTime=date_end)
lowFrequencyPortfolio <- portfolio_create(fromTime=date_start, toTime=date_end)

# add position "GOOG" to portfolios
portfolio_addPosition(highFrequencyPortfolio, sym_bol, 1)
price <- position_price(highFrequencyPortfolio, sym_bol)
printTime <- price[, 1]

# create vector of portfolio weights depending on rules
highFrequencyStrategy <- array(0, dim=NROW(price))
highFrequencyStrategy[price[, "value"] > mov_avg(price[, "value"], 150)] <- 100
lowFrequencyStrategy <- array(0, dim=NROW(price))
lowFrequencyStrategy[price[, "value"] > mov_avg(price[, "value"], 800)] <- 100

# add portfolio weights to strategies
portfolio_addPosition(
  portfolio=highFrequencyPortfolio, 
  symbol=sym_bol, 
  quantity=highFrequencyStrategy, 
  time=printTime)
portfolio_addPosition(
  portfolio=lowFrequencyPortfolio, 
  symbol=sym_bol, 
  quantity=lowFrequencyStrategy, 
  time=printTime)

# print the strategy summaries
print(highFrequencyPortfolio)
print(lowFrequencyPortfolio)

# ggplot the strategies
plot1 <- util_ggplot(util_plot2d(position_quantity(highFrequencyPortfolio, sym_bol), title="High Frequency Portfolio Strategy", line_size=0.6))
plot2 <- util_ggplot(util_plot2d(position_quantity(lowFrequencyPortfolio, sym_bol), title="Low Frequency Portfolio Strategy", line_size=0.6))
util_multiplot(plot1, plot2, cols=1)


# ggplot Variance
util_plot2d(portfolio_variance(highFrequencyPortfolio), title="Variance, daily", Legend="HF Portfolio") + 
  util_line2d(portfolio_variance(lowFrequencyPortfolio), Legend="LF Portfolio")


# ggplot Value-at-Risk
util_plot2d(portfolio_VaR(highFrequencyPortfolio, 0.05), title="Value at Risk in %, daily (95% c.i.)", Legend="HF Portfolio")+
  util_line2d(portfolio_VaR(lowFrequencyPortfolio, 0.05), Legend="LF Portfolio")

# ggplot Sharpe Ratio
util_plot2d(portfolio_sharpeRatio(highFrequencyPortfolio), title="Sharpe Ratio, daily", Legend="HF Portfolio")+
  util_line2d(portfolio_sharpeRatio(lowFrequencyPortfolio), Legend="LF Portfolio")

