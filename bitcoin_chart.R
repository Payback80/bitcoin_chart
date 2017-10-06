library(Quandl)
library(TTR)
library(quantmod)
library(plyr)

#install.packages("FinancialInstrument")
library("FinancialInstrument")
#install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
#install.packages("TTR")
library("TTR")

Quandl.api_key("HY4ErdYhrrKwUmRNi41B")
# FEED BITCOIN CHARTS
###############################################
#feed data variable with Quandl API 
data = Quandl("BCHARTS/BITSTAMPUSD", type = "xts")
#eliminate NA values, this method is efficient only with few missing data, check your data first!
data = na.omit(data)
# create  different timeframes from all the data 
last_3day = tail(data, n=3)
last_week = tail(data, n=7)
last_15days = tail(data, n=15) 
last_month =tail(data, n=30)
last_six_months =tail(data, n = 180)
# use the summary function to quickly have an overview of your timeframes
summary(last_3day)
summary(last_week)
summary(last_15days)
summary(last_month)
#get the last quote 
last_quote =   tail(data$Close, n=1)
#example of candlechart
candleChart(last_six_months,multi.col=TRUE,theme='white') 
# example with MACD BBands SMA SAR
chartSeries(last_six_months, dn.col = "red", TA="addMACD();addBBands();addSMA(10);addSMA(5);addSAR()", theme = chartTheme('white'))
#calculate the spread 
spread = 0.002
value_of_spread = (last_quote * spread)
breakeven= last_quote + value_of_spread
#now you can easly calculate your take profit adding (buy) or subtracting (sell) from the second part of the formula 
zerofivepercent= breakeven + (breakeven *0.005)
onepercent = breakeven + (breakeven * 0.01)
twopercent = breakeven + (breakeven * 0.02)
threepercent = breakeven + (breakeven * 0.03)
fivepercent = breakeven + (breakeven * 0.05)
