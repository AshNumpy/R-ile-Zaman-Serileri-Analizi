library(tidyverse)
library(readxl)
library(zoo)
library(xts)
library(forecast )
enData <- read_xlsx('./ikitelli-gune-enerjisi-santrali-elektrik-uretim-miktarlar.xlsx')
View(enData)

data.xts <- xts(enData$`Üretim (kWh)`, as.POSIXct(enData$Tarih)-1)
Sum.xts <- period.apply(data.xts, INDEX = endpoints(data.xts, "hours", 1), FUN = sum)
Sum.xts <- Sum.xts[1:150,]

# hourly.apply(data.xts, sum)
View(Sum.xts)
typeof(enData$Tarih)

Acf(diff(diff(Sum.xts), 18), lag.max = 42, lwd =3)
nrow(Sum.xts)

ma18 <- ma(Sum.xts, order = 18, centre = T)

ts.plot(window(Sum.xts), xlab = "Tarih", ylab = "Üretilen Enerji", lty = 1, col="purple", lwd=2)
par(new=T)
lines(ma18, lty=10, col="orange", lwd=3)


legend("topright", c(expression(paste(Sum.xts)), expression(paste(MA(Sum.xts, 18)))))
ts.plot(Sum.xts)
