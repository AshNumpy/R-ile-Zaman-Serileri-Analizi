# clear environment
rm(list=ls())

# libs
library(fpp)
library(forecast)
library(haven)

raw_df <- read.csv("./Datasets/dataset.csv")
raw_df[,1] <- as.POSIXct(raw_df[,1], format = "%Y-%m-%d %H:%M:%S")
rownames(raw_df) <- raw_df$X
raw_df$X <- NULL
View(raw_df)

# önceden periyodumuzu 18 olarak belirlemiştik ACF grafiğinden
periyot_acf = 18

# t değişkeni veri setimizin başından sonuna kadar 1'er artan bir seri
t <- 1:1:nrow(raw_df)


#####################################
### Toplamsal Regresyon Modelleri ###
#####################################

# 1. Regresyon Modeli:
# Eğer 1. regresyon modelinin tüm değişkenleri (t, sin1, cos1) anlamlı
# çıkarsa, 2. regresyon modelini deneyeceğiz. Anlamsız çıkana kadar 
# deneyip anlamsız çıkan ilk regresyon modelinin bir öncekine dönüp onu 
# kullanacağız.

# sin, cos tanımlama
sin1 <- sin(2*3.1416*t/periyot_acf)
cos1 <- cos(2*3.1416*t/periyot_acf)

# ham veri seti ile birleştirme
df <- as.data.frame(cbind(raw_df, t, sin1, cos1))
names(df)<- c("y", "t", "sin1", "cos1")
attach(df)
View(df)

# 1. regresyon modelini kurma
regresyon.model1 <-lm (y~t+sin1+cos1)
summary(regresyon.model1)
# t, sin1 ve cos1 anlamlı 2. modeli denemeye geçiyoruz
######################################################

# sin, cos tanımlama
sin2 <- sin(2*3.1416*2*t/periyot_acf)
cos2 <- cos(2*3.1416*2*t/periyot_acf)

# ham veri ile birleştirme
df2 <- as.data.frame(cbind(raw_df, t, sin1, cos1, sin2, cos2))
names(df2) <- c("y", "t", "sin1", "cos1", "sin2", "cos2")
attach(df2)
View(df2)

# 2. Regreson Modeli
regresyon.model2 <- lm(y~t+sin1+cos1+sin2+cos2)
summary(regresyon.model2)
#tüm değişkenler anlamlı 3. regresyon modelini denemeye geçiyoruz.
#################################################################

# sin, cos tanımlama
sin3 <- sin(2*3.1416*3*t/periyot_acf)
cos3 <- cos(2*3.1416*3*t/periyot_acf)

# ham veri ile birleştirme
df3 <- as.data.frame(cbind(raw_df, t, sin1, cos1, sin2, cos2, sin3, cos3))
names(df3) <- c("y", "t", "sin1", "cos1", "sin2", "cos2", "sin3", "cos3")
attach(df3)
View(df3)

# 3. Regresyon Modeli
regresyon.model3 <- lm(y~t+sin1+cos1+sin2+cos2+sin3+cos3)
summary(regresyon.model3)
# eklediğimiz 3. sin ve cos değerleri anlamsız çıktı 
# bu nedenle 2. modele geri dönüyoruz.

# %95 güven düzeyinde seçilen model 2. Regresyon Modeli
summary(regresyon.model2)
dwtest(y~t+sin1+cos1+sin2+cos2)

# H0: kurulan regresyon modeli hatalarının arasında ilişki yoktur.
# H1: kurulan regresyon modeli hatalarının arasında ilişki vardır.
# p<0.05 için söylenebilir ki hatalar arasında ilişki vardır.

# 2. model icin tahmin serisi, hata serisi ve tahminin alt ve üst sınırlarına ait seriler
tahmin1 <- predict(regresyon.model2)
sinir1 <- predict(regresyon.model2, interval = 'confidence' ,level = .95)
hata1 <- resid(regresyon.model2)

plot( window(y),
      xlab="Zaman", ylab="Üretilen Enerji", type="l", lty=3, col=2, lwd=2)
lines(window(sinir1[,2]) ,type="l",lty=1,col=4,lwd=2)
lines(window(sinir1[,3]) ,type="l",lty=1,col=3,lwd=2)
legend("topleft",c(expression(paste(x18)),
                   expression(paste(Altsinir)),
                   expression(paste(Üstsinir))),
       lwd=c(2,2,2),lty=c(3,1,1), cex=0.7, col=c(2,4,3))

#Hatalar akgurultu mu?
Acf(hata1, lag.max = 42,  ylim=c(-1,1), lwd=3)
# hatalar akgürültü serisi olarak görünmüyor, çünkü tüm değerler CI dışında
# ancak nihai kararı Box-Ljung testi verecek

#Box-Ljung
Box.test(hata1, lag = 42, type = "Ljung")
# p<0.05 için %95 güvenle söylenebilir ki modelin hataları akgürültü
# serisi olmadığı için model kullanılabilir bir model değildir.


######################################
### Çarpımsal Regresyon Moddelleri ###
######################################
s1<-t*sin(2*3.1416*t/9)
c1<-t*cos(2*3.1416*t/9)

df4 <- as.data.frame(cbind(raw_df, t, s1, c1))

names(df4)<- c("y", "t", "s1", "c1")
attach(df4)

regresyon.model4 <- lm(y~t+s1+c1)
summary(regresyon.model4)
# çarpımsal regresyon modelinin sinus ve cosinus değerleri anlamlı değil
# ancak t değişkeni anlamlı bu şekilde model kurup bir test edelim

regresyon.model4 <- lm(y~t)
dwtest(y~t)
# H0: kurulan regresyon modeli hatalarının arasında ilişki yoktur.
# H1: kurulan regresyon modeli hatalarının arasında ilişki vardır.
# p<0.05 için söylenebilir ki hatalar arasında ilişki vardır.

# 4.. model icin tahmin serisi, hata serisi ve tahminin alt ve üst sınırlarına ait seriler
tahmin4 <- predict(regresyon.model4)
sinir4 <- predict(regresyon.model4, interval = 'confidence' ,level = .95)
hata4 <- resid(regresyon.model4)

#Hatalar akgurultu mu?
Acf(hata4, lag.max = 42,  ylim=c(-1,1), lwd=3)
# hatalar akgürültü serisi olarak görünmüyor, çünkü tüm değerler CI dışında
# ancak nihai kararı Box-Ljung testi verecek

#Box-Ljung
Box.test(hata4, lag = 42, type = "Ljung")
# p<0.05 için %95 güvenle söylenebilir ki modelin hataları akgürültü
# serisi olmadığı için model kullanılabilir bir model değildir.