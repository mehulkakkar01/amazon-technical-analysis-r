install.packages("quantmod") 
install.packages("TTR")

library(quantmod)
library(TTR)

start_date <- Sys.Date() - 1095

getSymbols("AMZN", src = "yahoo", from = start_date)

amzn <- na.omit(AMZN)

sma20 <- SMA(Cl(amzn), 20)
sma50 <- SMA(Cl(amzn), 50)

chartSeries(amzn,
            name = "AMAZON – Simple Moving Average", theme = chartTheme("black"),
            TA = "addSMA(20); addSMA(50)")

chartSeries( amzn,
             name = "Amazon – RSI", theme = chartTheme("white"),
             TA = "addRSI(n = 14)"
)

chartSeries(
  amzn,
  name = "Amazon – MACD with Histogram", theme = chartTheme("white"),
  TA = "addMACD(fast = 12, slow = 26, signal = 9, type = 'EMA', histogram = TRUE)"
)

chartSeries(
  amzn,
  name = "Amazon – Bollinger Bands", theme = chartTheme("white"),
  TA = "addBBands(n = 20, sd = 2)"
)


macd <- MACD(Cl(amzn))
rsi <- RSI(Cl(amzn))

signal <- ifelse(
  sma20 > sma50 & macd$macd > macd$signal & rsi < 70,
  "BUY",
  ifelse(
    sma20 < sma50 & macd$macd < macd$signal & rsi > 30,
    "SELL",
    "HOLD"
  )
)

returns <- dailyReturn(Cl(amzn))

chartSeries(
  amzn,
  name = "Amazon – Full Chart Series",
  theme = chartTheme("white"),
  TA = c(
    "addSMA(20, col='blue')",
    "addSMA(50, col='red')",
    "addBBands(n=20, sd=2)",
    "addRSI(n=14)",
    "addMACD(fast=12, slow=26, signal=9, histogram=TRUE)"
  )
)




