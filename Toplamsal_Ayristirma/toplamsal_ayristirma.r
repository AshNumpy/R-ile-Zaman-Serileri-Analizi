# install.packages("fpp")
# install.packages("stats")
rm(list = ls())

# libs
library(fpp)
library(stats)
library(xts)
library(ggplot2)
library(forecast)

# set directory
setwd("./Toplamsal_Ayristirma")

# load data
df <- read.csv("./dataset.csv")
names(df) <- c("date", "electric")
head(df);tail(df)

# tarih-saat belirleme
df[,1] <- as.POSIXct(df[,1], format = "%Y-%m-%d %H:%M:%S")
df_xts <- as.xts(df[,-1], order.by = df[,1])
class(df_xts) ; head(df_xts)

# grafikler
ts_plot <- ts.plot(df_xts, xlab="Zaman", ylab="Üretilen Enerji")

acf_plot <- Acf(df_xts,lag.max = 42,  ylim=c(-1,1), lwd=3)
pacf_plot <- Pacf(df_xts,lag.max = 42, ylim=c(-1,1), lwd=3)

# zaman serisi
df_ts <- ts(df_xts[1:144], frequency = 18)

# trent bileşeni
df_trent <- tslm(df_ts ~ trend)

# Periyoda sahip mevsimsel bilesen serisi
periyot <- df_ts - df_trent[['fitted.values']]
Acf(periyot,lag.max = 42,  ylim=c(-1,1), lwd=3) # periyot=18 belirledik

# Merkezsel Hareketli ortlama MHO
MHO <- ma(df_ts, order = 18, centre = TRUE)

# Mevsim bileşeni
mevsim <- df_ts-MHO

# Mevsimlerin ortalaması 
donemort <- t(matrix(data=mevsim, nrow = 18, ncol=8))
colMeans(donemort, na.rm = T)
sum(colMeans(donemort, na.rm = T))
mean(colMeans(donemort, na.rm = T))

# mevsim endeks değerleri
endeks <- colMeans(donemort, na.rm = T) - mean(colMeans(donemort, na.rm = T))

# seri boyunda endeks değerlerini yerine koyma
indeks <-  matrix(data = endeks, nrow = 144)

# hatadan arındırılmamış trent bileşeni
trenthata <- df_ts - indeks

# seriyi hatadan arındırmak için doğrusal regresyon modeli kullanalım
trent <- tslm(trenthata~trend)
# trent serisinin #fitted.values -> orjinal serinin trent bileşenidir.

# tahmin serisini bulalim: (mevsimsel endeks + saf trent serisi)
tahmin <- indeks + trent$fitted.values

#hata serisini bulalim:
hata <- df_ts - indeks - trent$fitted.values

### MODELIN GUVENILIRLIGI ###
#orijinal seri ile tahmin serisinin uyumu
plot(window(df_ts), xlab = "Zaman", ylab="Üretilen Elektrik",
      lty=1, col=4, lwd=2, ylim=c(19,320))
lines(window(tahmin), lty = 3, col = 2, lwd = 3)
legend("topleft", lwd = c(2,2), lty = c(1,3),
      cex = 0.6, col = c(4,2))

# hatalar akgurultu serisi mi?
Acf(hata,main="Hata", lag.max = 42,  ylim=c(-1,1), lwd=3)
Pacf(hata,main="Hata",lag.max = 42, ylim=c(-1,1), lwd=3)
# ACF grafiğine göre hatalar akgürültü serisi değil
# Ancak nihai kararı Box-Ljung testi verir:
# H0: hatalar serisi ile ak-gürültü serisi arasında fark yoktur.
# Hs: hatalar serisi ile ak-gürültü serisi arasında fark yoktur.
Box.test(hata, lag = 42, type = "Ljung-Box")
# test sonucuna göre H0 reddedilir. Yani hatalar akgürültü serisi olmadığı için model kullanılamaz.
