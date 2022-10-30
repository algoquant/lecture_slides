"""
Script for loading OHLC data from a CSV file and plotting a candlestick plot.

"""

# Import packages 
import pandas as pd
import numpy as np
import plotly.graph_objects as go


# Load OHLC data from csv file - the time index is formatted inside read_csv()

symbol = "SPY"
range = "day"
filename = "/Users/jerzy/Develop/data/" + symbol + "_" + range + ".csv"
ohlc = pd.read_csv(filename)
datev = ohlc.Date


# Calculate log stock prices
ohlc[["Open", "High", "Low", "Close"]] = np.log(ohlc[["Open", "High", "Low", "Close"]])


# Calculate moving average

lookback = 55
closep = ohlc.Close
pricema = closep.ewm(span=lookback, adjust=False).mean()


# Plotly simple candlestick with moving average

# Create empty graph object
plotfig = go.Figure()
# Add trace for candlesticks
plotfig = plotfig.add_trace(go.Candlestick(x=datev,
                open=ohlc.Open, high=ohlc.High, low=ohlc.Low, close=ohlc.Close, 
                name=symbol+" Log OHLC Prices", showlegend=False))
# Add trace for moving average
plotfig = plotfig.add_trace(go.Scatter(x=datev, y=pricema, 
                            name="Moving Average", line=dict(color="blue")))
# Customize plot
plotfig = plotfig.update_layout(title=symbol + " Log OHLC Prices", 
                                title_font_size=24, title_font_color="blue", 
                                yaxis_title="Price", font_color="black", font_size=18,
                                xaxis_rangeslider_visible=False)
# Customize legend
plotfig = plotfig.update_layout(legend=dict(x=0.2, y=0.9, traceorder="normal", 
  itemsizing="constant", font=dict(family="sans-serif", size=18, color="blue")))
# Render the plot
plotfig.show()


