# FE800

library("xlsx")
library(ggplot2)
library(moments)

setwd("/Users/JohnPak/Desktop/FE800/")

data.all <- read.csv("crypto-markets.csv")
colnames(data.all)
head(data.all)

# date.start <- "24.04.2018"
# date.end <- "01.01.2016"

# 0 to 844
# index.days <- as.Date(date.start,"%d.%m.%Y") - as.Date(date.end, "%d.%m.%Y")
# index.days

data.all$date <- as.Date(data.all$date, format = "%m/%d/%y")

# In date range
data.crypto <- data.all[(data.all$date >= "2016-01-01" & data.all$date <= "2018-04-24"),]

# Only symbol and market capitalization

MarketCapRank <- data.crypto[, c(2,11)]

# Get maximum market capitalization for each symbol

MaxMarketCapRank <- aggregate(market~symbol, MarketCapRank, max)

# Maximum to minimum

MaxMarketCap <- MaxMarketCapRank[with(MaxMarketCapRank, order(-market)), ]
                 
# Top 25 Market Cap               
head(MaxMarketCap, 25)

# Data for cryptos in Top 25 Market Cap

data.crypto.top25 <- data.crypto[(data.crypto$symbol == "BTC" | data.crypto$symbol == "ETH" | data.crypto$symbol == "XRP"
                            | data.crypto$symbol == "BCH" | data.crypto$symbol == "ADA" | data.crypto$symbol == "LTC"
                            | data.crypto$symbol == "XEM" | data.crypto$symbol == "XLM" | data.crypto$symbol == "MIOTA"
                            | data.crypto$symbol == "TRX" | data.crypto$symbol == "NEO" | data.crypto$symbol == "DASH"
                            | data.crypto$symbol == "EOS" | data.crypto$symbol == "BTG" | data.crypto$symbol == "XMR"
                            | data.crypto$symbol == "QTUM" | data.crypto$symbol == "ICX" | data.crypto$symbol == "NANO"
                            | data.crypto$symbol == "ETC" | data.crypto$symbol == "LSK" | data.crypto$symbol == "XVG"
                            | data.crypto$symbol == "VEN" | data.crypto$symbol == "SC" | data.crypto$symbol == "BCN"
                            | data.crypto$symbol == "BCC") & (data.crypto$date >= "2016-01-01" & data.crypto$date <= "2018-04-24"),]

# Subset the data by cryptocurrency

data.BTC <- data.crypto[data.crypto$symbol == "BTC",]
data.ETH <- data.crypto[data.crypto$symbol == "ETH",]
data.XRP <- data.crypto[data.crypto$symbol == "XRP",]
data.BCH <- data.crypto[data.crypto$symbol == "BCH",]
data.ADA <- data.crypto[data.crypto$symbol == "ADA",]
data.LTC <- data.crypto[data.crypto$symbol == "LTC",]
data.XEM <- data.crypto[data.crypto$symbol == "XEM",]
data.XLM <- data.crypto[data.crypto$symbol == "XLM",]
data.MIOTA <- data.crypto[data.crypto$symbol == "MIOTA",]
data.TRX <- data.crypto[data.crypto$symbol == "TRX",]
data.NEO <- data.crypto[data.crypto$symbol == "NEO",]
data.DASH <- data.crypto[data.crypto$symbol == "DASH",]
data.EOS <- data.crypto[data.crypto$symbol == "EOS",]
data.BTG <- data.crypto[data.crypto$symbol == "BTG",]
data.XMR <- data.crypto[data.crypto$symbol == "XMR",]
data.QTUM <- data.crypto[data.crypto$symbol == "QTUM",]
data.ICX <- data.crypto[data.crypto$symbol == "ICX",]
data.NANO <- data.crypto[data.crypto$symbol == "NANO",]
data.ETC <- data.crypto[data.crypto$symbol == "ETC",]
data.LSK <- data.crypto[data.crypto$symbol == "LSK",]
data.XVG <- data.crypto[data.crypto$symbol == "XVG",]
data.VEN <- data.crypto[data.crypto$symbol == "VEN",]
data.SC <- data.crypto[data.crypto$symbol == "SC",]
data.BCN <- data.crypto[data.crypto$symbol == "BCN",]
data.BCC <- data.crypto[data.crypto$symbol == "BCC",]

# ROI Calculation

roi.calculation <- function(roi.data){
  
  roi.column <- data.frame(matrix(NA, nrow = nrow(roi.data) - 1, ncol = 1))
  for (i in (1: nrow(roi.data))){
    
    roi.column[i,] <- (roi.data[i+1,] - roi.data[i,])/roi.data[i,]*100
    
  }
  
  return(roi.column)
  
}

# Volume Calculation

volume.calculation <- function(volume.data){
  
  volume.column <- data.frame(matrix(NA, nrow = nrow(volume.data) - 1, ncol = 1))
  for (i in (1: nrow(volume.data))){
    
    volume.column[i,] <- (volume.data[i+1,] - volume.data[i,])/volume.data[i,]*100
    
  }
  
  return(volume.column)
  
}

# only get close column 

price.BTC <- data.BTC[,9, drop = FALSE]
volume.BTC <- data.BTC[,10, drop = FALSE]
mean.BTC <- mean(price.BTC$close, na.rm = TRUE)
max.BTC <- max(price.BTC$close, na.rm = TRUE)
min.BTC <- min(price.BTC$close, na.rm = TRUE)
sd.BTC <- sd(price.BTC$close)
skew.BTC <- skewness(price.BTC$close, na.rm = TRUE)
kur.BTC <- kurtosis(price.BTC$close, na.rm = TRUE)

price.ETH <- data.ETH[,9, drop = FALSE]
volume.ETH <- data.ETH[,10, drop = FALSE]
mean.ETH <- mean(price.ETH$close, na.rm = TRUE)
max.ETH <- max(price.ETH$close, na.rm = TRUE)
min.ETH <- min(price.ETH$close, na.rm = TRUE)
sd.ETH <- sd(price.ETH$close)
skew.ETH <- skewness(price.ETH$close, na.rm = TRUE)
kur.ETH <- kurtosis(price.ETH$close, na.rm = TRUE)

price.XRP <- data.XRP[,9, drop = FALSE]
volume.XRP <- data.XRP[,10, drop = FALSE]
mean.XRP <- mean(price.XRP$close, na.rm = TRUE)
max.XRP <- max(price.XRP$close, na.rm = TRUE)
min.XRP <- min(price.XRP$close, na.rm = TRUE)
sd.XRP <- sd(price.XRP$close)
skew.XRP <- skewness(price.XRP$close, na.rm = TRUE)
kur.XRP <- kurtosis(price.XRP$close, na.rm = TRUE)

price.BCH <- data.BCH[,9, drop = FALSE]
volume.BCH <- data.BCH[,10, drop = FALSE]
mean.BCH <- mean(price.BCH$close, na.rm = TRUE)
max.BCH <- max(price.BCH$close, na.rm = TRUE)
min.BCH <- min(price.BCH$close, na.rm = TRUE)
sd.BCH <- sd(price.BCH$close)
skew.BCH <- skewness(price.BCH$close, na.rm = TRUE)
kur.BCH <- kurtosis(price.BCH$close, na.rm = TRUE)

price.ADA <- data.ADA[,9, drop = FALSE]
volume.ADA <- data.ADA[,10, drop = FALSE]
mean.ADA <- mean(price.ADA$close, na.rm = TRUE)
max.ADA <- max(price.ADA$close, na.rm = TRUE)
min.ADA <- min(price.ADA$close, na.rm = TRUE)
sd.ADA <- sd(price.ADA$close)
skew.ADA <- skewness(price.ADA$close, na.rm = TRUE)
kur.ADA <- kurtosis(price.ADA$close, na.rm = TRUE)

price.LTC <- data.LTC[,9, drop = FALSE]
volume.LTC <- data.LTC[,10, drop = FALSE]
mean.LTC <- mean(price.LTC$close, na.rm = TRUE)
max.LTC <- max(price.LTC$close, na.rm = TRUE)
min.LTC <- min(price.LTC$close, na.rm = TRUE)
sd.LTC <- sd(price.LTC$close)
skew.LTC <- skewness(price.LTC$close, na.rm = TRUE)
kur.LTC <- kurtosis(price.LTC$close, na.rm = TRUE)

price.XEM <- data.XEM[,9, drop = FALSE]
volume.XEM <- data.XEM[,10, drop = FALSE]
mean.XEM <- mean(price.XEM$close, na.rm = TRUE)
max.XEM <- max(price.XEM$close, na.rm = TRUE)
min.XEM <- min(price.XEM$close, na.rm = TRUE)
sd.XEM <- sd(price.XEM$close)
skew.XEM <- skewness(price.XEM$close, na.rm = TRUE)
kur.XEM <- kurtosis(price.XEM$close, na.rm = TRUE)

price.XLM <- data.XLM[,9, drop = FALSE]
volume.XLM <- data.XLM[,10, drop = FALSE]
mean.XLM <- mean(price.XLM$close, na.rm = TRUE)
max.XLM <- max(price.XLM$close, na.rm = TRUE)
min.XLM <- min(price.XLM$close, na.rm = TRUE)
sd.XLM <- sd(price.XLM$close)
skew.XLM <- skewness(price.XLM$close, na.rm = TRUE)
kur.XLM <- kurtosis(price.XLM$close, na.rm = TRUE)

price.MIOTA <- data.MIOTA[,9, drop = FALSE]
volume.MIOTA <- data.MIOTA[,10, drop = FALSE]
mean.MIOTA <- mean(price.MIOTA$close, na.rm = TRUE)
max.MIOTA <- max(price.MIOTA$close, na.rm = TRUE)
min.MIOTA <- min(price.MIOTA$close, na.rm = TRUE)
sd.MIOTA <- sd(price.MIOTA$close)
skew.MIOTA <- skewness(price.MIOTA$close, na.rm = TRUE)
kur.MIOTA <- kurtosis(price.MIOTA$close, na.rm = TRUE)

price.TRX <- data.TRX[,9, drop = FALSE]
volume.TRX <- data.TRX[,10, drop = FALSE]
mean.TRX <- mean(price.TRX$close, na.rm = TRUE)
max.TRX <- max(price.TRX$close, na.rm = TRUE)
min.TRX <- min(price.TRX$close, na.rm = TRUE)
sd.TRX <- sd(price.TRX$close)
skew.TRX <- skewness(price.TRX$close, na.rm = TRUE)
kur.TRX <- kurtosis(price.TRX$close, na.rm = TRUE)

price.NEO <- data.NEO[,9, drop = FALSE]
volume.NEO <- data.NEO[,10, drop = FALSE]
mean.NEO <- mean(price.NEO$close, na.rm = TRUE)
max.NEO <- max(price.NEO$close, na.rm = TRUE)
min.NEO <- min(price.NEO$close, na.rm = TRUE)
sd.NEO <- sd(price.NEO$close)
skew.NEO <- skewness(price.NEO$close, na.rm = TRUE)
kur.NEO <- kurtosis(price.NEO$close, na.rm = TRUE)

price.DASH <- data.DASH[,9, drop = FALSE]
volume.DASH <- data.DASH[,10, drop = FALSE]
mean.DASH <- mean(price.DASH$close, na.rm = TRUE)
max.DASH <- max(price.DASH$close, na.rm = TRUE)
min.DASH <- min(price.DASH$close, na.rm = TRUE)
sd.DASH <- sd(price.DASH$close)
skew.DASH <- skewness(price.DASH$close, na.rm = TRUE)
kur.DASH <- kurtosis(price.DASH$close, na.rm = TRUE)

price.EOS <- data.EOS[,9, drop = FALSE]
volume.EOS <- data.EOS[,10, drop = FALSE]
mean.EOS <- mean(price.EOS$close, na.rm = TRUE)
max.EOS <- max(price.EOS$close, na.rm = TRUE)
min.EOS <- min(price.EOS$close, na.rm = TRUE)
sd.EOS <- sd(price.EOS$close)
skew.EOS <- skewness(price.EOS$close, na.rm = TRUE)
kur.EOS <- kurtosis(price.EOS$close, na.rm = TRUE)

price.BTG <- data.BTG[,9, drop = FALSE]
volume.BTG <- data.BTG[,10, drop = FALSE]
mean.BTG <- mean(price.BTG$close, na.rm = TRUE)
max.BTG <- max(price.BTG$close, na.rm = TRUE)
min.BTG <- min(price.BTG$close, na.rm = TRUE)
sd.BTG <- sd(price.BTG$close)
skew.BTG <- skewness(price.BTG$close, na.rm = TRUE)
kur.BTG <- kurtosis(price.BTG$close, na.rm = TRUE)

price.XMR <- data.XMR[,9, drop = FALSE]
volume.XMR <- data.XMR[,10, drop = FALSE]
mean.XMR <- mean(price.XMR$close, na.rm = TRUE)
max.XMR <- max(price.XMR$close, na.rm = TRUE)
min.XMR <- min(price.XMR$close, na.rm = TRUE)
sd.XMR <- sd(price.XMR$close)
skew.XMR <- skewness(price.XMR$close, na.rm = TRUE)
kur.XMR <- kurtosis(price.XMR$close, na.rm = TRUE)

price.QTUM <- data.QTUM[,9, drop = FALSE]
volume.QTUM <- data.QTUM[,10, drop = FALSE]
mean.QTUM <- mean(price.QTUM$close, na.rm = TRUE)
max.QTUM <- max(price.QTUM$close, na.rm = TRUE)
min.QTUM <- min(price.QTUM$close, na.rm = TRUE)
sd.QTUM <- sd(price.QTUM$close)
skew.QTUM <- skewness(price.QTUM$close, na.rm = TRUE)
kur.QTUM <- kurtosis(price.QTUM$close, na.rm = TRUE)

price.ICX <- data.ICX[,9, drop = FALSE]
volume.ICX <- data.ICX[,10, drop = FALSE]
mean.ICX <- mean(price.ICX$close, na.rm = TRUE)
max.ICX <- max(price.ICX$close, na.rm = TRUE)
min.ICX <- min(price.ICX$close, na.rm = TRUE)
sd.ICX <- sd(price.ICX$close)
skew.ICX <- skewness(price.ICX$close, na.rm = TRUE)
kur.ICX <- kurtosis(price.ICX$close, na.rm = TRUE)

price.NANO <- data.NANO[,9, drop = FALSE]
volume.NANO <- data.NANO[,10, drop = FALSE]
mean.NANO <- mean(price.NANO$close, na.rm = TRUE)
max.NANO <- max(price.NANO$close, na.rm = TRUE)
min.NANO <- min(price.NANO$close, na.rm = TRUE)
sd.NANO <- sd(price.NANO$close)
skew.NANO <- skewness(price.NANO$close, na.rm = TRUE)
kur.NANO <- kurtosis(price.NANO$close, na.rm = TRUE)

price.ETC <- data.ETC[,9, drop = FALSE]
volume.ETC <- data.ETC[,10, drop = FALSE]
mean.ETC <- mean(price.ETC$close, na.rm = TRUE)
max.ETC <- max(price.ETC$close, na.rm = TRUE)
min.ETC <- min(price.ETC$close, na.rm = TRUE)
sd.ETC <- sd(price.ETC$close)
skew.ETC <- skewness(price.ETC$close, na.rm = TRUE)
kur.ETC <- kurtosis(price.ETC$close, na.rm = TRUE)

price.LSK <- data.LSK[,9, drop = FALSE]
volume.LSK <- data.LSK[,10, drop = FALSE]
mean.LSK <- mean(price.LSK$close, na.rm = TRUE)
max.LSK <- max(price.LSK$close, na.rm = TRUE)
min.LSK <- min(price.LSK$close, na.rm = TRUE)
sd.LSK <- sd(price.LSK$close)
skew.LSK <- skewness(price.LSK$close, na.rm = TRUE)
kur.LSK <- kurtosis(price.LSK$close, na.rm = TRUE)

price.XVG <- data.XVG[,9, drop = FALSE]
volume.XVG <- data.XVG[,10, drop = FALSE]
mean.XVG <- mean(price.XVG$close, na.rm = TRUE)
max.XVG <- max(price.XVG$close, na.rm = TRUE)
min.XVG <- min(price.XVG$close, na.rm = TRUE)
sd.XVG <- sd(price.XVG$close)
skew.XVG <- skewness(price.XVG$close, na.rm = TRUE)
kur.XVG <- kurtosis(price.XVG$close, na.rm = TRUE)

price.VEN <- data.VEN[,9, drop = FALSE]
volume.VEN <- data.VEN[,10, drop = FALSE]
mean.VEN <- mean(price.VEN$close, na.rm = TRUE)
max.VEN <- max(price.VEN$close, na.rm = TRUE)
min.VEN <- min(price.VEN$close, na.rm = TRUE)
sd.VEN <- sd(price.VEN$close)
skew.VEN <- skewness(price.VEN$close, na.rm = TRUE)
kur.VEN <- kurtosis(price.VEN$close, na.rm = TRUE)

price.SC <- data.SC[,9, drop = FALSE]
volume.SC <- data.SC[,10, drop = FALSE]
mean.SC <- mean(price.SC$close, na.rm = TRUE)
max.SC <- max(price.SC$close, na.rm = TRUE)
min.SC <- min(price.SC$close, na.rm = TRUE)
sd.SC <- sd(price.SC$close)
skew.SC <- skewness(price.SC$close, na.rm = TRUE)
kur.SC <- kurtosis(price.SC$close, na.rm = TRUE)

price.BCN <- data.BCN[,9, drop = FALSE]
volume.BCN <- data.BCN[,10, drop = FALSE]
mean.BCN <- mean(price.BCN$close, na.rm = TRUE)
max.BCN <- max(price.BCN$close, na.rm = TRUE)
min.BCN <- min(price.BCN$close, na.rm = TRUE)
sd.BCN <- sd(price.BCN$close)
skew.BCN <- skewness(price.BCN$close, na.rm = TRUE)
kur.BCN <- kurtosis(price.BCN$close, na.rm = TRUE)

price.BCC <- data.BCC[,9, drop = FALSE]
volume.BCC <- data.BCC[,10, drop = FALSE]
mean.BCC <- mean(price.BCC$close, na.rm = TRUE)
max.BCC <- max(price.BCC$close, na.rm = TRUE)
min.BCC <- min(price.BCC$close, na.rm = TRUE)
sd.BCC <- sd(price.BCC$close)
skew.BCC <- skewness(price.BCC$close, na.rm = TRUE)
kur.BCC <- kurtosis(price.BCC$close, na.rm = TRUE)

# combine data
df.all <- data.frame(matrix(ncol = 26, nrow = 7))
col.names <- c("BTC", "ETH", "XRP","BCH","ADA", "LTC", "XEM", "XLM",
               "MIOTA","TRX","NEO", "DASH","EOS", "BTG", "XMR", "QTUM", 
               "ICX", "NANO", "ETC", "LSK","XVG", "VEN","SC", "BCN","BCC")
row.names <- c("","mean", "max", "min", "sd", "skewness", "kurtosis")
colnames(df.all) <- col.names
rownames(df.all) <- row.names  
df.all

df.all[2,1] <- mean.BTC
df.all[3,1] <- max.BTC
df.all[4,1] <- min.BTC
df.all[5,1] <- sd.BTC
df.all[6,1] <- skew.BTC
df.all[7,1] <- kur.BTC

df.all[2,2] <- mean.ETH
df.all[3,2] <- max.ETH
df.all[4,2] <- min.ETH
df.all[5,2] <- sd.ETH
df.all[6,2] <- skew.ETH
df.all[7,2] <- kur.ETH

df.all[2,3] <- mean.XRP
df.all[3,3] <- max.XRP
df.all[4,3] <- min.XRP
df.all[5,3] <- sd.XRP
df.all[6,3] <- skew.XRP
df.all[7,3] <- kur.XRP

df.all[2,4] <- mean.BCH
df.all[3,4] <- max.BCH
df.all[4,4] <- min.BCH
df.all[5,4] <- sd.BCH
df.all[6,4] <- skew.BCH
df.all[7,4] <- kur.BCH

df.all[2,5] <- mean.ADA
df.all[3,5] <- max.ADA
df.all[4,5] <- min.ADA
df.all[5,5] <- sd.ADA
df.all[6,5] <- skew.ADA
df.all[7,5] <- kur.ADA

df.all[2,6] <- mean.LTC
df.all[3,6] <- max.LTC
df.all[4,6] <- min.LTC
df.all[5,6] <- sd.LTC
df.all[6,6] <- skew.LTC
df.all[7,6] <- kur.LTC

df.all[2,7] <- mean.XEM
df.all[3,7] <- max.XEM
df.all[4,7] <- min.XEM
df.all[5,7] <- sd.XEM
df.all[6,7] <- skew.XEM
df.all[7,7] <- kur.XEM

df.all[2,8] <- mean.XLM
df.all[3,8] <- max.XLM
df.all[4,8] <- min.XLM
df.all[5,8] <- sd.XLM
df.all[6,8] <- skew.XLM
df.all[7,8] <- kur.XLM

df.all[2,9] <- mean.MIOTA
df.all[3,9] <- max.MIOTA
df.all[4,9] <- min.MIOTA
df.all[5,9] <- sd.MIOTA
df.all[6,9] <- skew.MIOTA
df.all[7,9] <- kur.MIOTA

df.all[2,10] <- mean.TRX
df.all[3,10] <- max.TRX
df.all[4,10] <- min.TRX
df.all[5,10] <- sd.TRX
df.all[6,10] <- skew.TRX
df.all[7,10] <- kur.TRX

df.all[2,11] <- mean.NEO
df.all[3,11] <- max.NEO
df.all[4,11] <- min.NEO
df.all[5,11] <- sd.NEO
df.all[6,11] <- skew.NEO
df.all[7,11] <- kur.NEO

df.all[2,12] <- mean.DASH
df.all[3,12] <- max.DASH
df.all[4,12] <- min.DASH
df.all[5,12] <- sd.DASH
df.all[6,12] <- skew.DASH
df.all[7,12] <- kur.DASH

df.all[2,13] <- mean.EOS
df.all[3,13] <- max.EOS
df.all[4,13] <- min.EOS
df.all[5,13] <- sd.EOS
df.all[6,13] <- skew.EOS
df.all[7,13] <- kur.EOS

df.all[2,14] <- mean.BTG
df.all[3,14] <- max.BTG
df.all[4,14] <- min.BTG
df.all[5,14] <- sd.BTG
df.all[6,14] <- skew.BTG
df.all[7,14] <- kur.BTG

df.all[2,15] <- mean.XMR
df.all[3,15] <- max.XMR
df.all[4,15] <- min.XMR
df.all[5,15] <- sd.XMR
df.all[6,15] <- skew.XMR
df.all[7,15] <- kur.XMR

df.all[2,16] <- mean.QTUM
df.all[3,16] <- max.QTUM
df.all[4,16] <- min.QTUM
df.all[5,16] <- sd.QTUM
df.all[6,16] <- skew.QTUM
df.all[7,16] <- kur.QTUM

df.all[2,17] <- mean.ICX
df.all[3,17] <- max.ICX
df.all[4,17] <- min.ICX
df.all[5,17] <- sd.ICX
df.all[6,17] <- skew.ICX
df.all[7,17] <- kur.ICX

df.all[2,18] <- mean.NANO
df.all[3,18] <- max.NANO
df.all[4,18] <- min.NANO
df.all[5,18] <- sd.NANO
df.all[6,18] <- skew.NANO
df.all[7,18] <- kur.NANO

df.all[2,19] <- mean.ETC
df.all[3,19] <- max.ETC
df.all[4,19] <- min.ETC
df.all[5,19] <- sd.ETC
df.all[6,19] <- skew.ETC
df.all[7,19] <- kur.ETC

df.all[2,20] <- mean.LSK
df.all[3,20] <- max.LSK
df.all[4,20] <- min.LSK
df.all[5,20] <- sd.LSK
df.all[6,20] <- skew.LSK
df.all[7,20] <- kur.LSK

df.all[2,21] <- mean.XVG
df.all[3,21] <- max.XVG
df.all[4,21] <- min.XVG
df.all[5,21] <- sd.XVG
df.all[6,21] <- skew.XVG
df.all[7,21] <- kur.XVG

df.all[2,22] <- mean.VEN
df.all[3,22] <- max.VEN
df.all[4,22] <- min.VEN
df.all[5,22] <- sd.VEN
df.all[6,22] <- skew.VEN
df.all[7,22] <- kur.VEN

df.all[2,23] <- mean.SC
df.all[3,23] <- max.SC
df.all[4,23] <- min.SC
df.all[5,23] <- sd.SC
df.all[6,23] <- skew.SC
df.all[7,23] <- kur.SC

df.all[2,24] <- mean.BCN
df.all[3,24] <- max.BCN
df.all[4,24] <- min.BCN
df.all[5,24] <- sd.BCN
df.all[6,24] <- skew.BCN
df.all[7,24] <- kur.BCN

df.all[2,25] <- mean.BCC
df.all[3,25] <- max.BCC
df.all[4,25] <- min.BCC
df.all[5,25] <- sd.BCC
df.all[6,25] <- skew.BCC
df.all[7,25] <- kur.BCC

df.all <- df.all[-1, , drop = FALSE]
df.all <- df.all[, -ncol(df.all), drop = FALSE]
df.all

write.xlsx(df.all, "/Users/JohnPak/Desktop/FE800/Final/Price16-18.xlsx", sheetName = "Price", 
           col.names = TRUE, row.names = TRUE, append = TRUE)

# ROI and Volume Dataframe

roi.close.BTC <- roi.calculation(price.BTC)
names(roi.close.BTC)[1] <- "ROI_BTC"
mean.roi.BTC <- mean(roi.close.BTC$ROI_BTC, na.rm = TRUE)
max.roi.BTC <- max(roi.close.BTC$ROI_BTC, na.rm = TRUE)
min.roi.BTC <- min(roi.close.BTC$ROI_BTC, na.rm = TRUE)
volume.BTC <- volume.calculation(volume.BTC)
names(volume.BTC)[1] <- "volume_BTC"
mean.volume.BTC <- mean(volume.BTC$volume_BTC, na.rm = TRUE)
max.volume.BTC <- max(volume.BTC$volume_BTC, na.rm = TRUE)
min.volume.BTC <- min(volume.BTC$volume_BTC, na.rm = TRUE)

roi.close.ETH <- roi.calculation(price.ETH)
names(roi.close.ETH)[1] <- "ROI_ETH"
mean.roi.ETH <- mean(roi.close.ETH$ROI_ETH, na.rm = TRUE)
max.roi.ETH <- max(roi.close.ETH$ROI_ETH, na.rm = TRUE)
min.roi.ETH <- min(roi.close.ETH$ROI_ETH, na.rm = TRUE)
volume.ETH <- volume.calculation(volume.ETH)
names(volume.ETH)[1] <- "volume_ETH"
mean.volume.ETH <- mean(volume.ETH$volume_ETH, na.rm = TRUE)
max.volume.ETH <- max(volume.ETH$volume_ETH, na.rm = TRUE)
min.volume.ETH <- min(volume.ETH$volume_ETH, na.rm = TRUE)

roi.close.XRP <- roi.calculation(price.XRP)
names(roi.close.XRP)[1] <- "ROI_XRP"
mean.roi.XRP <- mean(roi.close.XRP$ROI_XRP, na.rm = TRUE)
max.roi.XRP <- max(roi.close.XRP$ROI_XRP, na.rm = TRUE)
min.roi.XRP <- min(roi.close.XRP$ROI_XRP, na.rm = TRUE)
volume.XRP <- volume.calculation(volume.XRP)
names(volume.XRP)[1] <- "volume_XRP"
mean.volume.XRP <- mean(volume.XRP$volume_XRP, na.rm = TRUE)
max.volume.XRP <- max(volume.XRP$volume_XRP, na.rm = TRUE)
min.volume.XRP <- min(volume.XRP$volume_XRP, na.rm = TRUE)

roi.close.BCH <- roi.calculation(price.BCH)
names(roi.close.BCH)[1] <- "ROI_BCH"
mean.roi.BCH <- mean(roi.close.BCH$ROI_BCH, na.rm = TRUE)
max.roi.BCH <- max(roi.close.BCH$ROI_BCH, na.rm = TRUE)
min.roi.BCH <- min(roi.close.BCH$ROI_BCH, na.rm = TRUE)
volume.BCH <- volume.calculation(volume.BCH)
names(volume.BCH)[1] <- "volume_BCH"
mean.volume.BCH <- mean(volume.BCH$volume_BCH, na.rm = TRUE)
max.volume.BCH <- max(volume.BCH$volume_BCH, na.rm = TRUE)
min.volume.BCH <- min(volume.BCH$volume_BCH, na.rm = TRUE)

roi.close.ADA <- roi.calculation(price.ADA)
names(roi.close.ADA)[1] <- "ROI_ADA"
mean.roi.ADA <- mean(roi.close.ADA$ROI_ADA, na.rm = TRUE)
max.roi.ADA <- max(roi.close.ADA$ROI_ADA, na.rm = TRUE)
min.roi.ADA <- min(roi.close.ADA$ROI_ADA, na.rm = TRUE)
volume.ADA <- volume.calculation(volume.ADA)
names(volume.ADA)[1] <- "volume_ADA"
mean.volume.ADA <- mean(volume.ADA$volume_ADA, na.rm = TRUE)
max.volume.ADA <- max(volume.ADA$volume_ADA, na.rm = TRUE)
min.volume.ADA <- min(volume.ADA$volume_ADA, na.rm = TRUE)

roi.close.LTC <- roi.calculation(price.LTC)
names(roi.close.LTC)[1] <- "ROI_LTC"
mean.roi.LTC <- mean(roi.close.LTC$ROI_LTC, na.rm = TRUE)
max.roi.LTC <- max(roi.close.LTC$ROI_LTC, na.rm = TRUE)
min.roi.LTC <- min(roi.close.LTC$ROI_LTC, na.rm = TRUE)
volume.LTC <- volume.calculation(volume.LTC)
names(volume.LTC)[1] <- "volume_LTC"
mean.volume.LTC <- mean(volume.LTC$volume_LTC, na.rm = TRUE)
max.volume.LTC <- max(volume.LTC$volume_LTC, na.rm = TRUE)
min.volume.LTC <- min(volume.LTC$volume_LTC, na.rm = TRUE)

roi.close.XEM <- roi.calculation(price.XEM)
names(roi.close.XEM)[1] <- "ROI_XEM"
mean.roi.XEM <- mean(roi.close.XEM$ROI_XEM, na.rm = TRUE)
max.roi.XEM <- max(roi.close.XEM$ROI_XEM, na.rm = TRUE)
min.roi.XEM <- min(roi.close.XEM$ROI_XEM, na.rm = TRUE)
volume.XEM <- volume.calculation(volume.XEM)
names(volume.XEM)[1] <- "volume_XEM"
mean.volume.XEM <- mean(volume.XEM$volume_XEM, na.rm = TRUE)
max.volume.XEM <- max(volume.XEM$volume_XEM, na.rm = TRUE)
min.volume.XEM <- min(volume.XEM$volume_XEM, na.rm = TRUE)

roi.close.XLM <- roi.calculation(price.XLM)
names(roi.close.XLM)[1] <- "ROI_XLM"
mean.roi.XLM <- mean(roi.close.XLM$ROI_XLM, na.rm = TRUE)
max.roi.XLM <- max(roi.close.XLM$ROI_XLM, na.rm = TRUE)
min.roi.XLM <- min(roi.close.XLM$ROI_XLM, na.rm = TRUE)
volume.XLM <- volume.calculation(volume.XLM)
names(volume.XLM)[1] <- "volume_XLM"
mean.volume.XLM <- mean(volume.XLM$volume_XLM, na.rm = TRUE)
max.volume.XLM <- max(volume.XLM$volume_XLM, na.rm = TRUE)
min.volume.XLM <- min(volume.XLM$volume_XLM, na.rm = TRUE)

roi.close.MIOTA <- roi.calculation(price.MIOTA)
names(roi.close.MIOTA)[1] <- "ROI_MIOTA"
mean.roi.MIOTA <- mean(roi.close.MIOTA$ROI_MIOTA, na.rm = TRUE)
max.roi.MIOTA <- max(roi.close.MIOTA$ROI_MIOTA, na.rm = TRUE)
min.roi.MIOTA <- min(roi.close.MIOTA$ROI_MIOTA, na.rm = TRUE)
volume.MIOTA <- volume.calculation(volume.MIOTA)
names(volume.MIOTA)[1] <- "volume_MIOTA"
mean.volume.MIOTA <- mean(volume.MIOTA$volume_MIOTA, na.rm = TRUE)
max.volume.MIOTA <- max(volume.MIOTA$volume_MIOTA, na.rm = TRUE)
min.volume.MIOTA <- min(volume.MIOTA$volume_MIOTA, na.rm = TRUE)

roi.close.TRX <- roi.calculation(price.TRX)
names(roi.close.TRX)[1] <- "ROI_TRX"
mean.roi.TRX <- mean(roi.close.TRX$ROI_TRX, na.rm = TRUE)
max.roi.TRX <- max(roi.close.TRX$ROI_TRX, na.rm = TRUE)
min.roi.TRX <- min(roi.close.TRX$ROI_TRX, na.rm = TRUE)
volume.TRX <- volume.calculation(volume.TRX)
names(volume.TRX)[1] <- "volume_TRX"
mean.volume.TRX <- mean(volume.TRX$volume_TRX, na.rm = TRUE)
max.volume.TRX <- max(volume.TRX$volume_TRX, na.rm = TRUE)
min.volume.TRX <- min(volume.TRX$volume_TRX, na.rm = TRUE)

roi.close.NEO <- roi.calculation(price.NEO)
names(roi.close.NEO)[1] <- "ROI_NEO"
mean.roi.NEO <- mean(roi.close.NEO$ROI_NEO, na.rm = TRUE)
max.roi.NEO <- max(roi.close.NEO$ROI_NEO, na.rm = TRUE)
min.roi.NEO <- min(roi.close.NEO$ROI_NEO, na.rm = TRUE)
volume.NEO <- volume.calculation(volume.NEO)
names(volume.NEO)[1] <- "volume_NEO"
mean.volume.NEO <- mean(volume.NEO$volume_NEO, na.rm = TRUE)
max.volume.NEO <- max(volume.NEO$volume_NEO, na.rm = TRUE)
min.volume.NEO <- min(volume.NEO$volume_NEO, na.rm = TRUE)

roi.close.DASH <- roi.calculation(price.DASH)
names(roi.close.DASH)[1] <- "ROI_DASH"
mean.roi.DASH <- mean(roi.close.DASH$ROI_DASH, na.rm = TRUE)
max.roi.DASH <- max(roi.close.DASH$ROI_DASH, na.rm = TRUE)
min.roi.DASH <- min(roi.close.DASH$ROI_DASH, na.rm = TRUE)
volume.DASH <- volume.calculation(volume.DASH)
names(volume.DASH)[1] <- "volume_DASH"
mean.volume.DASH <- mean(volume.DASH$volume_DASH, na.rm = TRUE)
max.volume.DASH <- max(volume.DASH$volume_DASH, na.rm = TRUE)
min.volume.DASH <- min(volume.DASH$volume_DASH, na.rm = TRUE)

roi.close.EOS <- roi.calculation(price.EOS)
names(roi.close.EOS)[1] <- "ROI_EOS"
mean.roi.EOS <- mean(roi.close.EOS$ROI_EOS, na.rm = TRUE)
max.roi.EOS <- max(roi.close.EOS$ROI_EOS, na.rm = TRUE)
min.roi.EOS <- min(roi.close.EOS$ROI_EOS, na.rm = TRUE)
volume.EOS <- volume.calculation(volume.EOS)
names(volume.EOS)[1] <- "volume_EOS"
mean.volume.EOS <- mean(volume.EOS$volume_EOS, na.rm = TRUE)
max.volume.EOS <- max(volume.EOS$volume_EOS, na.rm = TRUE)
min.volume.EOS <- min(volume.EOS$volume_EOS, na.rm = TRUE)

roi.close.BTG <- roi.calculation(price.BTG)
names(roi.close.BTG)[1] <- "ROI_BTG"
mean.roi.BTG <- mean(roi.close.BTG$ROI_BTG, na.rm = TRUE)
max.roi.BTG <- max(roi.close.BTG$ROI_BTG, na.rm = TRUE)
min.roi.BTG <- min(roi.close.BTG$ROI_BTG, na.rm = TRUE)
volume.BTG <- volume.calculation(volume.BTG)
names(volume.BTG)[1] <- "volume_BTG"
volume.BTG <- as.data.frame.matrix(volume.BTG)
volume.BTG <- volume.BTG[!is.infinite(rowSums(volume.BTG)),]
mean.volume.BTG <- mean(volume.BTG, na.rm = TRUE)
max.volume.BTG <- max(volume.BTG, na.rm = TRUE)
min.volume.BTG <- min(volume.BTG, na.rm = TRUE)

roi.close.XMR <- roi.calculation(price.XMR)
names(roi.close.XMR)[1] <- "ROI_XMR"
mean.roi.XMR <- mean(roi.close.XMR$ROI_XMR, na.rm = TRUE)
max.roi.XMR <- max(roi.close.XMR$ROI_XMR, na.rm = TRUE)
min.roi.XMR <- min(roi.close.XMR$ROI_XMR, na.rm = TRUE)
volume.XMR <- volume.calculation(volume.XMR)
names(volume.XMR)[1] <- "volume_XMR"
mean.volume.XMR <- mean(volume.XMR$volume_XMR, na.rm = TRUE)
max.volume.XMR <- max(volume.XMR$volume_XMR, na.rm = TRUE)
min.volume.XMR <- min(volume.XMR$volume_XMR, na.rm = TRUE)

roi.close.QTUM <- roi.calculation(price.QTUM)
names(roi.close.QTUM)[1] <- "ROI_QTUM"
mean.roi.QTUM <- mean(roi.close.QTUM$ROI_QTUM, na.rm = TRUE)
max.roi.QTUM <- max(roi.close.QTUM$ROI_QTUM, na.rm = TRUE)
min.roi.QTUM <- min(roi.close.QTUM$ROI_QTUM, na.rm = TRUE)
volume.QTUM <- volume.calculation(volume.QTUM)
names(volume.QTUM)[1] <- "volume_QTUM"
mean.volume.QTUM <- mean(volume.QTUM$volume_QTUM, na.rm = TRUE)
max.volume.QTUM <- max(volume.QTUM$volume_QTUM, na.rm = TRUE)
min.volume.QTUM <- min(volume.QTUM$volume_QTUM, na.rm = TRUE)

roi.close.ICX <- roi.calculation(price.ICX)
names(roi.close.ICX)[1] <- "ROI_ICX"
mean.roi.ICX <- mean(roi.close.ICX$ROI_ICX, na.rm = TRUE)
max.roi.ICX <- max(roi.close.ICX$ROI_ICX, na.rm = TRUE)
min.roi.ICX <- min(roi.close.ICX$ROI_ICX, na.rm = TRUE)
volume.ICX <- volume.calculation(volume.ICX)
names(volume.ICX)[1] <- "volume_ICX"
mean.volume.ICX <- mean(volume.ICX$volume_ICX, na.rm = TRUE)
max.volume.ICX <- max(volume.ICX$volume_ICX, na.rm = TRUE)
min.volume.ICX <- min(volume.ICX$volume_ICX, na.rm = TRUE)

roi.close.NANO <- roi.calculation(price.NANO)
names(roi.close.NANO)[1] <- "ROI_NANO"
mean.roi.NANO <- mean(roi.close.NANO$ROI_NANO, na.rm = TRUE)
max.roi.NANO <- max(roi.close.NANO$ROI_NANO, na.rm = TRUE)
min.roi.NANO <- min(roi.close.NANO$ROI_NANO, na.rm = TRUE)
volume.NANO <- volume.calculation(volume.NANO)
names(volume.NANO)[1] <- "volume_NANO"
mean.volume.NANO <- mean(volume.NANO$volume_NANO, na.rm = TRUE)
max.volume.NANO <- max(volume.NANO$volume_NANO, na.rm = TRUE)
min.volume.NANO <- min(volume.NANO$volume_NANO, na.rm = TRUE)

roi.close.ETC <- roi.calculation(price.ETC)
names(roi.close.ETC)[1] <- "ROI_ETC"
mean.roi.ETC <- mean(roi.close.ETC$ROI_ETC, na.rm = TRUE)
max.roi.ETC <- max(roi.close.ETC$ROI_ETC, na.rm = TRUE)
min.roi.ETC <- min(roi.close.ETC$ROI_ETC, na.rm = TRUE)
volume.ETC <- volume.calculation(volume.ETC)
names(volume.ETC)[1] <- "volume_ETC"
mean.volume.ETC <- mean(volume.ETC$volume_ETC, na.rm = TRUE)
max.volume.ETC <- max(volume.ETC$volume_ETC, na.rm = TRUE)
min.volume.ETC <- min(volume.ETC$volume_ETC, na.rm = TRUE)

roi.close.LSK <- roi.calculation(price.LSK)
names(roi.close.LSK)[1] <- "ROI_LSK"
mean.roi.LSK <- mean(roi.close.LSK$ROI_LSK, na.rm = TRUE)
max.roi.LSK <- max(roi.close.LSK$ROI_LSK, na.rm = TRUE)
min.roi.LSK <- min(roi.close.LSK$ROI_LSK, na.rm = TRUE)
volume.LSK <- volume.calculation(volume.LSK)
names(volume.LSK)[1] <- "volume_LSK"
mean.volume.LSK <- mean(volume.LSK$volume_LSK, na.rm = TRUE)
max.volume.LSK <- max(volume.LSK$volume_LSK, na.rm = TRUE)
min.volume.LSK <- min(volume.LSK$volume_LSK, na.rm = TRUE)

roi.close.XVG <- roi.calculation(price.XVG)
names(roi.close.XVG)[1] <- "ROI_XVG"
mean.roi.XVG <- mean(roi.close.XVG$ROI_XVG, na.rm = TRUE)
max.roi.XVG <- max(roi.close.XVG$ROI_XVG, na.rm = TRUE)
min.roi.XVG <- min(roi.close.XVG$ROI_XVG, na.rm = TRUE)
volume.XVG <- volume.calculation(volume.XVG)
names(volume.XVG)[1] <- "volume_XVG"
mean.volume.XVG <- mean(volume.XVG$volume_XVG, na.rm = TRUE)
max.volume.XVG <- max(volume.XVG$volume_XVG, na.rm = TRUE)
min.volume.XVG <- min(volume.XVG$volume_XVG, na.rm = TRUE)

roi.close.VEN <- roi.calculation(price.VEN)
names(roi.close.VEN)[1] <- "ROI_VEN"
mean.roi.VEN <- mean(roi.close.VEN$ROI_VEN, na.rm = TRUE)
max.roi.VEN <- max(roi.close.VEN$ROI_VEN, na.rm = TRUE)
min.roi.VEN <- min(roi.close.VEN$ROI_VEN, na.rm = TRUE)
volume.VEN <- volume.calculation(volume.VEN)
names(volume.VEN)[1] <- "volume_VEN"
volume.VEN <- as.data.frame.matrix(volume.VEN)
volume.VEN <- volume.VEN[!is.infinite(rowSums(volume.VEN)),]
mean.volume.VEN <- mean(volume.VEN, na.rm = TRUE)
max.volume.VEN <- max(volume.VEN, na.rm = TRUE)
min.volume.VEN <- min(volume.VEN, na.rm = TRUE)

roi.close.SC <- roi.calculation(price.SC)
names(roi.close.SC)[1] <- "ROI_SC"
mean.roi.SC <- mean(roi.close.SC$ROI_SC, na.rm = TRUE)
max.roi.SC <- max(roi.close.SC$ROI_SC, na.rm = TRUE)
min.roi.SC <- min(roi.close.SC$ROI_SC, na.rm = TRUE)
volume.SC <- volume.calculation(volume.SC)
names(volume.SC)[1] <- "volume_SC"
mean.volume.SC <- mean(volume.SC$volume_SC, na.rm = TRUE)
max.volume.SC <- max(volume.SC$volume_SC, na.rm = TRUE)
min.volume.SC <- min(volume.SC$volume_SC, na.rm = TRUE)

roi.close.BCN <- roi.calculation(price.BCN)
names(roi.close.BCN)[1] <- "ROI_BCN"
mean.roi.BCN <- mean(roi.close.BCN$ROI_BCN, na.rm = TRUE)
max.roi.BCN <- max(roi.close.BCN$ROI_BCN, na.rm = TRUE)
min.roi.BCN <- min(roi.close.BCN$ROI_BCN, na.rm = TRUE)
volume.BCN <- volume.calculation(volume.BCN)
names(volume.BCN)[1] <- "volume_BCN"
mean.volume.BCN <- mean(volume.BCN$volume_BCN, na.rm = TRUE)
max.volume.BCN <- max(volume.BCN$volume_BCN, na.rm = TRUE)
min.volume.BCN <- min(volume.BCN$volume_BCN, na.rm = TRUE)

roi.close.BCC <- roi.calculation(price.BCC)
names(roi.close.BCC)[1] <- "ROI_BCC"
mean.roi.BCC <- mean(roi.close.BCC$ROI_BCC, na.rm = TRUE)
max.roi.BCC <- max(roi.close.BCC$ROI_BCC, na.rm = TRUE)
min.roi.BCC <- min(roi.close.BCC$ROI_BCC, na.rm = TRUE)
volume.BCC <- volume.calculation(volume.BCC)
names(volume.BCC)[1] <- "volume_BCC"
mean.volume.BCC <- mean(volume.BCC$volume_BCC, na.rm = TRUE)
max.volume.BCC <- max(volume.BCC$volume_BCC, na.rm = TRUE)
min.volume.BCC <- min(volume.BCC$volume_BCC, na.rm = TRUE)

# combine ROI and Volume Data 
df.all.roi.volume <- data.frame(matrix(ncol = 26, nrow = 7))
col.names <- c("BTC", "ETH", "XRP","BCH","ADA", "LTC", "XEM", "XLM",
               "MIOTA","TRX","NEO", "DASH","EOS", "BTG", "XMR", "QTUM", 
               "ICX", "NANO", "ETC", "LSK","XVG", "VEN","SC", "BCN","BCC")
row.names <- c("","mean ROI", "max ROI", "min ROI",
               "mean Volume", "max Volume", "min Volume")
colnames(df.all.roi.volume) <- col.names
rownames(df.all.roi.volume) <- row.names  
df.all.roi.volume

df.all.roi.volume[2,1] <- mean.roi.BTC
df.all.roi.volume[3,1] <- max.roi.BTC
df.all.roi.volume[4,1] <- min.roi.BTC
df.all.roi.volume[5,1] <- mean.volume.BTC
df.all.roi.volume[6,1] <- max.volume.BTC
df.all.roi.volume[7,1] <- min.volume.BTC

df.all.roi.volume[2,2] <- mean.roi.ETH
df.all.roi.volume[3,2] <- max.roi.ETH
df.all.roi.volume[4,2] <- min.roi.ETH
df.all.roi.volume[5,2] <- mean.volume.ETH
df.all.roi.volume[6,2] <- max.volume.ETH
df.all.roi.volume[7,2] <- min.volume.ETH

df.all.roi.volume[2,3] <- mean.roi.XRP
df.all.roi.volume[3,3] <- max.roi.XRP
df.all.roi.volume[4,3] <- min.roi.XRP
df.all.roi.volume[5,3] <- mean.volume.XRP
df.all.roi.volume[6,3] <- max.volume.XRP
df.all.roi.volume[7,3] <- min.volume.XRP

df.all.roi.volume[2,4] <- mean.roi.BCH
df.all.roi.volume[3,4] <- max.roi.BCH
df.all.roi.volume[4,4] <- min.roi.BCH
df.all.roi.volume[5,4] <- mean.volume.BCH
df.all.roi.volume[6,4] <- max.volume.BCH
df.all.roi.volume[7,4] <- min.volume.BCH

df.all.roi.volume[2,5] <- mean.roi.ADA
df.all.roi.volume[3,5] <- max.roi.ADA
df.all.roi.volume[4,5] <- min.roi.ADA
df.all.roi.volume[5,5] <- mean.volume.ADA
df.all.roi.volume[6,5] <- max.volume.ADA
df.all.roi.volume[7,5] <- min.volume.ADA

df.all.roi.volume[2,6] <- mean.roi.LTC
df.all.roi.volume[3,6] <- max.roi.LTC
df.all.roi.volume[4,6] <- min.roi.LTC
df.all.roi.volume[5,6] <- mean.volume.LTC
df.all.roi.volume[6,6] <- max.volume.LTC
df.all.roi.volume[7,6] <- min.volume.LTC

df.all.roi.volume[2,7] <- mean.roi.XEM
df.all.roi.volume[3,7] <- max.roi.XEM
df.all.roi.volume[4,7] <- min.roi.XEM
df.all.roi.volume[5,7] <- mean.volume.XEM
df.all.roi.volume[6,7] <- max.volume.XEM
df.all.roi.volume[7,7] <- min.volume.XEM

df.all.roi.volume[2,8] <- mean.roi.XLM
df.all.roi.volume[3,8] <- max.roi.XLM
df.all.roi.volume[4,8] <- min.roi.XLM
df.all.roi.volume[5,8] <- mean.volume.XLM
df.all.roi.volume[6,8] <- max.volume.XLM
df.all.roi.volume[7,8] <- min.volume.XLM

df.all.roi.volume[2,9] <- mean.roi.MIOTA
df.all.roi.volume[3,9] <- max.roi.MIOTA
df.all.roi.volume[4,9] <- min.roi.MIOTA
df.all.roi.volume[5,9] <- mean.volume.MIOTA
df.all.roi.volume[6,9] <- max.volume.MIOTA
df.all.roi.volume[7,9] <- min.volume.MIOTA

df.all.roi.volume[2,10] <- mean.roi.TRX
df.all.roi.volume[3,10] <- max.roi.TRX
df.all.roi.volume[4,10] <- min.roi.TRX
df.all.roi.volume[5,10] <- mean.volume.TRX
df.all.roi.volume[6,10] <- max.volume.TRX
df.all.roi.volume[7,10] <- min.volume.TRX

df.all.roi.volume[2,11] <- mean.roi.NEO
df.all.roi.volume[3,11] <- max.roi.NEO
df.all.roi.volume[4,11] <- min.roi.NEO
df.all.roi.volume[5,11] <- mean.volume.NEO
df.all.roi.volume[6,11] <- max.volume.NEO
df.all.roi.volume[7,11] <- min.volume.NEO

df.all.roi.volume[2,12] <- mean.roi.DASH
df.all.roi.volume[3,12] <- max.roi.DASH
df.all.roi.volume[4,12] <- min.roi.DASH
df.all.roi.volume[5,12] <- mean.volume.DASH
df.all.roi.volume[6,12] <- max.volume.DASH
df.all.roi.volume[7,12] <- min.volume.DASH

df.all.roi.volume[2,13] <- mean.roi.EOS
df.all.roi.volume[3,13] <- max.roi.EOS
df.all.roi.volume[4,13] <- min.roi.EOS
df.all.roi.volume[5,13] <- mean.volume.EOS
df.all.roi.volume[6,13] <- max.volume.EOS
df.all.roi.volume[7,13] <- min.volume.EOS

df.all.roi.volume[2,14] <- mean.roi.BTG
df.all.roi.volume[3,14] <- max.roi.BTG
df.all.roi.volume[4,14] <- min.roi.BTG
df.all.roi.volume[5,14] <- mean.volume.BTG
df.all.roi.volume[6,14] <- max.volume.BTG
df.all.roi.volume[7,14] <- min.volume.BTG

df.all.roi.volume[2,15] <- mean.roi.XMR
df.all.roi.volume[3,15] <- max.roi.XMR
df.all.roi.volume[4,15] <- min.roi.XMR
df.all.roi.volume[5,15] <- mean.volume.XMR
df.all.roi.volume[6,15] <- max.volume.XMR
df.all.roi.volume[7,15] <- min.volume.XMR

df.all.roi.volume[2,16] <- mean.roi.QTUM
df.all.roi.volume[3,16] <- max.roi.QTUM
df.all.roi.volume[4,16] <- min.roi.QTUM
df.all.roi.volume[5,16] <- mean.volume.QTUM
df.all.roi.volume[6,16] <- max.volume.QTUM
df.all.roi.volume[7,16] <- min.volume.QTUM

df.all.roi.volume[2,17] <- mean.roi.ICX
df.all.roi.volume[3,17] <- max.roi.ICX
df.all.roi.volume[4,17] <- min.roi.ICX
df.all.roi.volume[5,17] <- mean.volume.ICX
df.all.roi.volume[6,17] <- max.volume.ICX
df.all.roi.volume[7,17] <- min.volume.ICX

df.all.roi.volume[2,18] <- mean.roi.NANO
df.all.roi.volume[3,18] <- max.roi.NANO
df.all.roi.volume[4,18] <- min.roi.NANO
df.all.roi.volume[5,18] <- mean.volume.NANO
df.all.roi.volume[6,18] <- max.volume.NANO
df.all.roi.volume[7,18] <- min.volume.NANO

df.all.roi.volume[2,19] <- mean.roi.ETC
df.all.roi.volume[3,19] <- max.roi.ETC
df.all.roi.volume[4,19] <- min.roi.ETC
df.all.roi.volume[5,19] <- mean.volume.ETC
df.all.roi.volume[6,19] <- max.volume.ETC
df.all.roi.volume[7,19] <- min.volume.ETC

df.all.roi.volume[2,20] <- mean.roi.LSK
df.all.roi.volume[3,20] <- max.roi.LSK
df.all.roi.volume[4,20] <- min.roi.LSK
df.all.roi.volume[5,20] <- mean.volume.LSK
df.all.roi.volume[6,20] <- max.volume.LSK
df.all.roi.volume[7,20] <- min.volume.LSK

df.all.roi.volume[2,21] <- mean.roi.XVG
df.all.roi.volume[3,21] <- max.roi.XVG
df.all.roi.volume[4,21] <- min.roi.XVG
df.all.roi.volume[5,21] <- mean.volume.XVG
df.all.roi.volume[6,21] <- max.volume.XVG
df.all.roi.volume[7,21] <- min.volume.XVG

df.all.roi.volume[2,22] <- mean.roi.VEN
df.all.roi.volume[3,22] <- max.roi.VEN
df.all.roi.volume[4,22] <- min.roi.VEN
df.all.roi.volume[5,22] <- mean.volume.VEN
df.all.roi.volume[6,22] <- max.volume.VEN
df.all.roi.volume[7,22] <- min.volume.VEN

df.all.roi.volume[2,23] <- mean.roi.SC
df.all.roi.volume[3,23] <- max.roi.SC
df.all.roi.volume[4,23] <- min.roi.SC
df.all.roi.volume[5,23] <- mean.volume.SC
df.all.roi.volume[6,23] <- max.volume.SC
df.all.roi.volume[7,23] <- min.volume.SC

df.all.roi.volume[2,24] <- mean.roi.BCN
df.all.roi.volume[3,24] <- max.roi.BCN
df.all.roi.volume[4,24] <- min.roi.BCN
df.all.roi.volume[5,24] <- mean.volume.BCN
df.all.roi.volume[6,24] <- max.volume.BCN
df.all.roi.volume[7,24] <- min.volume.BCN

df.all.roi.volume[2,25] <- mean.roi.BCC
df.all.roi.volume[3,25] <- max.roi.BCC
df.all.roi.volume[4,25] <- min.roi.BCC
df.all.roi.volume[5,25] <- mean.volume.BCC
df.all.roi.volume[6,25] <- max.volume.BCC
df.all.roi.volume[7,25] <- min.volume.BCC

df.all.roi.volume <- df.all.roi.volume[-1, , drop = FALSE]
df.all.roi.volume <- df.all.roi.volume[, -ncol(df.all.roi.volume), drop = FALSE]
df.all.roi.volume

write.xlsx(df.all.roi.volume, "/Users/JohnPak/Desktop/FE800/Final/ROIVolume16-18.xlsx", sheetName = "ROIVolume", 
           col.names = TRUE, row.names = TRUE, append = TRUE)


