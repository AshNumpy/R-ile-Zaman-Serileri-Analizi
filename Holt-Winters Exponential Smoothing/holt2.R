##Holt Winter2

library(fpp)
library(stats)
library(xts)
library(ggplot2)
library(forecast)
library(dplyr)
##https://rpubs.com/Linh-LTP/648937
# load data
df <- read.csv("./Toplamsal_Ayristirma/dataset.csv")
names(df) <- c("date", "electric")



# tarih-saat belirleme
df[,1] <- as.POSIXct(df[,1], format = "%Y-%m-%d %H:%M:%S")
df_xts <- as.xts(df[,-1], order.by = df[,1])
class(df_xts) ; head(df_xts)

#periyodu 18 olan zaman serisi
df_ts <- ts(df_xts[1:144], frequency = 18)
df_ts +1

winters1 <- ets(df_ts, model = "AAA")
winters2 <- ets(df_ts +0.0005, model = "MAM")
summary(winters2)
