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
data = Quandl("BCHARTS/BTCEUSD", type = "xts" )
data = Quandl("BCHARTS/BITSTAMPUSD", type = "xts")
data = na.omit(data)
############################################à
# FEED BTCE
###########################################
data = Quandl("BTCE/USDBTC", type = "xts")
data = na.omit(data)
data = rename(data, c("Last" = "Close"))
#########################################

last_3day = tail(data, n=3)
last_week = tail(data, n=7)
last_15days = tail(data, n=15) 
last_month =tail(data, n=30)
last_six_months =tail(data, n = 180)

summary(last_3day)
summary(last_week)
summary(last_15days)
summary(last_month)

candleChart(last_six_months,multi.col=TRUE,theme='white') 
chartSeries(last_six_months, dn.col = "red", TA="addMACD();addSMA(10)", theme=chartTheme('white'))
#aggiunta
chartSeries(last_six_months, dn.col = "red", TA="addMACD();addBBands();addSMA(10);addSMA(5);addSAR()", theme = chartTheme('white'))
#get the last quote 
last_quote =   tail(data$Close, n=1)
#calculate buy/sell limits 
spread = 0.002
value_of_spread = (last_quote * spread)
breakeven= last_quote + value_of_spread

zerofivepercent= breakeven + (breakeven *0.005)
onepercent = breakeven + (breakeven * 0.01)
twopercent = breakeven + (breakeven * 0.02)
threepercent = breakeven + (breakeven * 0.03)
fivepercent = breakeven + (breakeven * 0.05)




##################################################
###########FUNZIONA##################

data = Quandl("BTCE/USDBTC", type = "xts")
data = na.omit(data)
data = rename(data, c("Last" = "Close"))

#rename data to stock
Stock <- data



# add the indicators
Stock$BBands <- BBands(HLC(Stock))
Stock$MACD <- MACD(HLC(Stock)[,3])
Stock$stochOSC <- stoch(Stock[,c("High","Low","Close")])
Stock$position <- ifelse(Cl(Stock)>Stock$BBands & Stock$BBands >Stock$MACD & Stock$MACD > Stock$stochOSC , 1 , -1)

Gains <- lag(Stock$position) * dailyReturn(Stock)
charts.PerformanceSummary(cbind(dailyReturn(Stock),Gains))

################################
data = Quandl("BTCE/USDBTC", type = "xts")
data = na.omit(data)
data = rename(data, c("Last" = "Close"))
#rename data to stock
Stock <- data
# add the indicators
Stock$BBands <- BBands(HLC(Stock))
Stock$MACD <- MACD(HLC(Stock)[,3])
Stock$stochOSC <- stoch(Stock[,c("High","Low","Close")])
Stock$chaikin<-   chaikinAD(Stock[,c("High","Low","Close")], Stock[,"Volume"])
Stock$gmma <- GMMA(Stock[,"Close"])
Stock$ult.osc <- ultimateOscillator(Stock[,c("High","Low","Close")])
ohlc <- Stock[,c("High","Low","Close")]
Stock$vparkinson <- volatility(ohlc, calc="parkinson")

Stock$position <- ifelse(Cl(Stock) >Stock$chaikin  , 1 , -1)

Stock$position <- ifelse(Cl(Stock) >Stock$gmma  , 1 , -1)
Stock$position <- ifelse(Cl(Stock) >Stock$vparkinson , 1 , -1)

Gains <- lag(Stock$position) * dailyReturn(Stock)
charts.PerformanceSummary(cbind(dailyReturn(Stock),Gains))



############################
#######################
Stock <- get(getSymbols('CAT'))["2014::"]

# add the indicators
Stock$BBands <- BBands(HLC(Stock))
Stock$MACD <- MACD(HLC(Stock)[,3])
Stock$stochOSC <- stoch(Stock[,c("CAT.High","CAT.Low","CAT.Close")])
Stock$position <- ifelse(Cl(Stock)>Stock$BBands & Stock$BBands >Stock$MACD & Stock$MACD > Stock$stochOSC , 1 , -1)

Gains <- lag(Stock$position) * dailyReturn(Stock)
charts.PerformanceSummary(cbind(dailyReturn(Stock),Gains))



##############################################
##############################################
################################################
######################################################
#########################################################
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

data = Quandl("BTCE/USDBTC", type = "xts")
data = na.omit(data)
data = rename(data, c("Last" = "Close"))
#########################################

last_3day = tail(data, n=3)
last_week = tail(data, n=7)
last_15days = tail(data, n=15) 
last_month =tail(data, n=30)
last_six_months =tail(data, n = 180)

summary(last_3day)
summary(last_week)
summary(last_15days)
summary(last_month)

Stock <- data

Stock$gmma <- GMMA(Stock[,"Close"])
Stock$position <- ifelse(Cl(Stock) >Stock$gmma  , 1 , -1)

Gains <- lag(Stock$position) * dailyReturn(Stock)
charts.PerformanceSummary(cbind(dailyReturn(Stock),Gains))
tail(Stock$position)