# libs
library(fpp)
library(stats)
library(xts)
library(ggplot2)
library(forecast)

# load data
df <- read.csv("../Toplamsal_Ayristirma/dataset.csv")
names(df) <- c("date", "electric")


# tarih-saat belirleme
df[,1] <- as.POSIXct(df[,1], format = "%Y-%m-%d %H:%M:%S")
df_xts <- as.xts(df[,-1], order.by = df[,1])
class(df_xts) ; head(df_xts)

#periyodu 18 olan zaman serisi
df_ts <- ts(df_xts[1:144], frequency = 18)

#germe sayısı s = 18 olan merkezsel hareketli ortalama işlemi seriye uygulanır
MHO <- ma(df_ts, order = 18, centre = TRUE)

#orijinal seriyi mho ya bölerek serinin mevsimsel bileşenlerini bulmaya çalışalım. (hata terimini de içeren)

mevsimsel_bilesen <- df_ts/MHO

#hata terimini yok edebilmek amacıyla her periyottaki ortalama değer hesaplanır.
donemort <- t(matrix(data=mevsimsel_bilesen, nrow = 18, ncol=8))
donemort <- colMeans(donemort, na.rm = T)
donemort_son <- mean(donemort)

mean(donemort/donemort_son) # 1 e eşit olmalı

mevsimsel_endeks_serisi <- donemort/donemort_son
##https://stackoverflow.com/questions/18142117/how-to-replace-nan-value-with-zero-in-a-huge-data-frame
trent_serisi <- df_ts/mevsimsel_endeks_serisi
##nan ları 0 yapalım
trent_serisi[is.nan(trent_serisi)] <- 0

#doğrusal regresyon
trent_bileseni<- tslm(trent_serisi~trend)

#tahmin serisini bulalım
tahmin_serisi <- trent_bileseni$fitted.values * mevsimsel_endeks_serisi
#hata serisi
hata <- df_ts - tahmin_serisi

#HATA Serisinin Analizi
#Hata serisi akgürültü olmalıdır.
View(hata)
Acf(hata, lag.max = 42, lwd=3, ylim=c(-1,1))
Pacf(hata, lag.max = 42, lwd=3, ylim=c(-1,1))

Box.test(hata, lag = 10, type = "Ljung-Box")
##Hatalar akgürültü değildir. Demekki serimiz çarpımsal ayrıştırmaya uygun bir seri değildir.

