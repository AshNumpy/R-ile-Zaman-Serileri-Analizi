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

Acf(Sum.xts, lag.max = 42, lwd =3)
nrow(Sum.xts)


