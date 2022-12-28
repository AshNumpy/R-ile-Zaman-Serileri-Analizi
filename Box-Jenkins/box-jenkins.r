# clear environment
rm(list=ls())

# libs
library(fpp)

# load data
path = './Datasets/dataset.csv'
df <- read.csv(path)
df <- df[1:144,]
df[,1] <- as.POSIXct(df[,1], format = "%Y-%m-%d %H:%M:%S")
rownames(df) <- df$X
df$X <- NULL
View(df)

# acf plot
Acf(df, lag.max = 42,  ylim=c(-1,1), lwd=3)
# trend: var
# mevsimsellik: var 
# periyot: s=18

# trendden arındırmak:
df <- ts(df, frequency = 18)
df_1 <- diff(df)
Acf(df_1, lag.max = 42,  ylim=c(-1,1), lwd=3)
# ilk 4 çubuğun hepsi CI dışına çıkması lazım ancak hepsi çıkmamış trent yok
# trent farkı sayısı: d=1

# mevsimsellikten arındırma konusunda şuna başvurulabilir.
# ACF grafiğinden periyodu 18 olan bir mevsimsellik bulduk 
# ancak PACF grafiğinde bu mevsimsellik önemli ise (ilgili gözlemler CI dışında)
# tamam mevsimsellik var ve periyodu 18 derken
# PACF de ilgili gözlemler CI dışında değil ise 
# mevsimellik farkı alınmasına gerek yok o kadar da önemli değil şeklinde yorum da yapılabilir.

# mevsimsellikten arındırmak:
season <- rep(1:18, length.out = 144)
df_2 <- df_1 - ave(df_1, season, FUN = mean) #veya
# diff(diff(zaman_serisi),periyor)
Acf(df_2, lag.max = 42,  ylim=c(-1,1), lwd=3)
Pacf(df_2, lag.max = 42,  ylim=c(-1,1), lwd=3)
# mesimsellik farkı sayısı: D=1
# PACF grafiği ACF grafiğinden daha hızlı azalıyor: MA(q) modeli
# ACF -> PACF 'den hızlıysa p=0 verilip q'nun kaç değer aldığına bakmak için 
#       ACF grafiğinin ilk 3 değerinin kaç tanesi CI dışında ona bakılır.
# PACF -> ACF'den hızlıysa q=0 verilip p'nin hangi değeri aldığını bulmak için
#       PACF grafiğinin ilk 3 değerinin kaç tanesi CI dışında ona bakılır.
# Hem PACF hem ACF ikisi de yavaş azalıyorsa
# p değeri için PACF 'ye 
# q deperi için ACF'ye bakılır. ilk 3 gözlemin kaçı CI dışında.

# PACF -> ACF 'den hızlı o zaman q=0 ve p için PACF bakarız.
# trendden ve mevsimsellikten arındırılmış serinin 
# PACF ilk 3 gözleminin hiçbiri CI dışında değil: p=0 : 

# (P,Q) için ilgili grafikte periyot kaç ise 1 öncesi ve 1 sonrasındaki gözlem
# aynı yönde ise 1'den başlayıp 3'e kadar dene değilse tam tersinden başla 1'e doğru dene
Acf(df, lag.max = 42,  ylim=c(-1,1), lwd=3) 
# aynı yönlü o zaman 1 den başlayıp 3'e doğru deneyeceğiz.
# Modeller -> ARIMA(p,d,q)(P,D,Q)

# Model-1: ARIMA(0,1,0)(1,1,0)
ARIMA1 <- Arima(df, order=c(0,1,0), seasonal=c(1,1,0), include.constant = TRUE)
coeftest(ARIMA1) # P<0.05 Model anlamlı
summary(ARIMA1) # BIC:1542.41 RMSE:102.2535
Acf(ARIMA1$residuals, lag.max = 42,  ylim=c(-1,1), lwd=3) # Bence akgürültü serisi
# Hataların dağılımı ile akgürültü serisi arasında fark yoktur
# Hataların dağılımı ile akgürültü serisi arasında fark vardır
Box.test(ARIMA1$residuals, lag = 42, type = "Ljung") # Box-Ljung testine göre de akgürültü serisi


# Model-2: ARIMA(0,1,0)(2,1,0)
ARIMA2 <- Arima(df, order=c(0,1,0), seasonal=c(2,1,0), include.constant = TRUE)
coeftest(ARIMA2) # P<0.05 Model anlamlı
summary(ARIMA2) # BIC:1542.19 RMSE:99.38836
Acf(ARIMA2$residuals, lag.max = 42,  ylim=c(-1,1), lwd=3) # Bence akgürültü serisi
# Hataların dağılımı ile akgürültü serisi arasında fark yoktur
# Hataların dağılımı ile akgürültü serisi arasında fark vardır
Box.test(ARIMA2$residuals, lag = 42, type = "Ljung") # Box-Ljung testine göre de akgürültü serisi


# Model-3: ARIMA(0,1,0)(3,1,0)
ARIMA3 <- Arima(df, order=c(0,1,0), seasonal=c(3,1,0), include.constant = TRUE)
coeftest(ARIMA3) # P>0.05 Model anlamlı değil
summary(ARIMA3)


# Model-4: ARIMA(0,1,0)(0,1,1)
ARIMA4 <- Arima(df, order=c(0,1,0), seasonal=c(0,1,1), include.constant = TRUE)
coeftest(ARIMA4) # P<0.05 Model anlamlı
summary(ARIMA4) # BIC:1538.64 RMSE:100.1105
Acf(ARIMA4$residuals, lag.max = 42,  ylim=c(-1,1), lwd=3) # Bence akgürültü serisi
# Hataların dağılımı ile akgürültü serisi arasında fark yoktur
# Hataların dağılımı ile akgürültü serisi arasında fark vardır
Box.test(ARIMA4$residuals, lag = 42, type = "Ljung") # Box-Ljung testine göre de akgürültü serisi


# Model-5: ARIMA(0,1,0)(0,1,2)
ARIMA5 <- Arima(df, order=c(0,1,0), seasonal=c(0,1,2), include.constant = TRUE)
coeftest(ARIMA5) # P>0.05 Model anlamlı değil
summary(ARIMA5)

##################################
### Seçilen Model ve Öngörüler ###
##################################

# Model-2: ARIMA(0,1,0)(2,1,0)
ARIMA2 <- Arima(df, order=c(0,1,0), seasonal=c(2,1,0), include.constant = TRUE)
coeftest(ARIMA2) # P<0.05 Model anlamlı
summary(ARIMA2) # BIC:1542.19 RMSE:99.38836

# 5 dönemlik tahminde bulunalım
ongoru <- forecast(ARIMA2, h=5)
ongoru[['mean']]