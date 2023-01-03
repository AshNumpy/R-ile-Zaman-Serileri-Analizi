## R-ile-Zaman-Serileri-Analizi

### Veri Seti Hakkında
Bu projede veri seti olarak, İstanbul Büyükşehir Belediyesi’nin İkitelli’de kurulan güneş panellerinden elde edilen enerjinin ölçüldüğü zaman serisi kullanılmıştır. Bu veri seti 15 dakikalık ölçümler içeriyor olup, saatlik olacak şekilde R programında ön işleme yapılmıştır.  

### Analiz Süreci Hakkında
Analiz sürecinde ön işleme olarak zaman serisi analizi yapıldıktan sonra serinin mevsimselliğe ve trende sahip bir zaman serisi olduğuna karar verilmiştir. Buna ilişkin; toplamsal ayrıştırma, çarpımsal ayrıştırma, basit doğrusal regresyon, Holt-Winters Üstel düzleştirme yöntemi ve Box-Jenkins modelleri kullanılmış olup, hataları ak gürültü serisi, hata kareler ortalaması (HKO) ve Baessian Yanlılığı (BIC) değeri en düşük olan model ARIMA(0,1,0)(2,1,0) seçilmiş olup, öngörüler bu modellerin tahminleriyle oluşturulmuştur.

### Sonuç Raporu
Analiz sonucunda gelecek zamana ait enerji üretimini tahmin edebilen modeli elde ettik. Modelimizin BIC değerini 1542.19 RMSE değerini ise 99.38836 olarak elde ettik.
