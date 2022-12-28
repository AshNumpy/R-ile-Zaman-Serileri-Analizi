library(fpp)
library(stats)
library(xts)
library(ggplot2)
library(forecast)
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

#zaman serisi grafiği
plot.ts(df_ts)
df_ts[is.nan(df_ts)] <- 0
df_timeseriescomponents <- decompose(df_ts)

plot(df_timeseriescomponents)
View(df_ts)
#Mevsimsellikten arındırılmış hali
df_timeseriesseasonallyadjusted <- df_ts - df_timeseriescomponents$seasonal
plot(df_timeseriesseasonallyadjusted)
df_ts <- log(df_ts)
df_ts[which(!is.finite(df_ts))] <- 0
df_timeseriesforecasts <- HoltWinters(df_ts, beta=FALSE, gamma=FALSE)
##Holt winters değeri 4.321549e-17 geldiği için anlamsız olur.

df_timeseriesforecasts$SSE

plot(df_timeseriesforecasts)
df_timeseriesforecasts2 <- 
  forecast:::forecast.HoltWinters(df_timeseriesforecasts, h=48)
forecast:::plot.forecast(df_timeseriesforecasts2)
