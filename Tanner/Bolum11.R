file_path <- "C:/Users/taner/OneDrive/Masa\u00fcst\u00fc/projeler/ist analiz/arasinav.txt"

veri_kitle <- read.table(file_path, header = TRUE, sep = " ")  
set.seed(123)  # Rastgelelik sabitlemek için
satir_no <- sample(1:nrow(veri_kitle),size = 2000)
                  #vektor,size(boyut)

veri_orneklem <- veri_kitle[satir_no,]

# Örneklenen veri kümesini kaydet
write.table(veri_orneklem, "veri_orneklem.txt", sep = " ", row.names = TRUE, col.names = TRUE)

veri <- veri_orneklem

names(veri)    #  Verinin içindeki değişkenleri görmek için kullanılır. 
attach(veri)
summary(veri)


# Bölüm 11 in     2,6,8,9,14,22,23,24


#bölüm 11.2

#bağımlı değişken : Income
#bağımsız değişkenler : Age,Occupation(ama kategorik?),City_Tier(kategorik),

", the model is useful only if there is a fairly linear relationship between the
predictors and the response, but that requirement is much less restrictive than you might think."

Occupation <- as.factor(Occupation)
City_Tier <- as.factor(City_Tier)

plot(Age, Income, main = "Income vs Age", xlab = "Age", ylab = "Income")
#bir lineer ilişki yok Age ve Income arasında

plot(Occupation, Income, main = "Income vs Occupation", xlab = "Occupation", ylab = "Income")
plot(City_Tier, Income, main = "Income vs City_Tier", xlab = "City_Tier", ylab = "Income")

#yukarıdaki grafiklerden aralarında ilişki olup olamadığını tam anlamıyla göremedik

cor_age_income <- cor(Income,Age)
cor_Occupation_income <- cor(Income,as.numeric(Occupation))
cor_City_Tier_income <- cor(Income,as.numeric(City_Tier))

#korelasyon katsayılarına bakarsak da aralarında ilişki olmadığını söyleriz


# Multiple Linear Regression modeli
model <- lm(Income ~Age+Occupation+City_Tier, data = veri)

# y= 42023.96+30.29*Age-2667.28*OccupationRetired+....

# Modelin özeti
summary(model)

"Multiple R-squared: 0.00419: Bu değer, modelin bağımsız değişkenlerin gelirdeki değişimi ne kadar 
açıkladığını gösterir. Burada çok küçük bir değer olan 0.00419, 
bağımsız değişkenlerin gelirdeki değişimin çok küçük bir kısmını açıkladığını ve 
modelin çok zayıf olduğunu gösterir. Yani, yaş, meslek ve şehir seviyesi gelirdeki değişimi 
açıklamada pek etkili değil.

Adjusted R-squared: 0.001192: Bu değer, modeldeki bağımsız değişkenlerin gelir değişimini açıklama 
gücünü biraz daha doğru yansıtır çünkü modeldeki fazla bağımsız değişkenleri cezalandırır. 
Bu da modelin oldukça zayıf olduğunu ve değişkenlerin gelir üzerinde çok az etkisi olduğunu gösteriyor.

F-statistic: 1.398 ve p-value: 0.2118: Bu test, modelin genel olarak anlamlı olup olmadığını 
kontrol eder. p-değeri 0.2118 olduğu için ve yokluk hipotezimiz H0: Yaş, Meslek ve Şehir Seviyesi 
gelir üzerinde anlamlı bir etkiye sahip değildir. olduğundan p>0,05 olduğunda H0 kabul edilir yani
modelin anlamlı olmadığı söylenebilir
Bu, modelin bağımsız değişkenlerinin (yaş, meslek, şehir seviyesi) gelir üzerinde anlamlı bir etkisi
olmadığına işaret eder.

1. Yaş (Age) İçin H0:
H0: Yaşın gelir üzerinde anlamlı bir etkisi yoktur.
Bu, yaşın gelirle olan ilişkisini sıfır olarak kabul ederiz.

2. Meslek (Occupation) İçin H0:
H0: Meslek durumunun gelir üzerinde anlamlı bir etkisi yoktur.
Bu, mesleklerin (Emekli, Kendi işini yapan, Öğrenci) gelir üzerindeki etkilerini sıfır olarak kabul ederiz.

3. Şehir Seviyesi (City_Tier) İçin H0:
H0: Şehir Seviyesinin gelir üzerinde anlamlı bir etkisi yoktur.
Bu, şehir seviyesinin (Tier 2 ve Tier 3) gelir üzerinde sıfır etkisi olduğunu varsayarız.

Yaş için p-değeri 0.5998 (büyük, yani H0 reddedilmez).
Meslek için p-değerleri (Retired: 0.2244, Self_Employed: 0.1872, Student: 0.7536) 
yine büyük ve H0 reddedilmez.
Şehir Seviyesi için p-değerleri (Tier 2: 0.0598, Tier 3: 0.7619) büyük, 
ancak Tier 2 için p-değeri sınırda ve %10 anlamlılık seviyesinde kabul edilebilir.
"

z_scores <- scale(Income)  # Gelir için Z-skorunu hesapla 
outliers <- which(abs(z_scores) > 3)  # Z-skoru 3'ten büyük olanları bul

Q1 <- quantile(Income, 0.25)
Q3 <- quantile(Income, 0.75)
IQR_value <- Q3 - Q1

outliers_iqr <- which(veri$Income < (Q1 - 1.5 * IQR_value) | veri$Income > (Q3 + 1.5 * IQR_value))

veri$Income_log <- log(veri$Income + 1)  # Gelir değişkenine log dönüşümü uygula

plot(Age, veri$Income_log, main = "Income vs Age", xlab = "Age", ylab = "Income")
plot(Occupation, veri$Income_log, main = "Income vs Occupation", xlab = "Occupation", ylab = "Income")
plot(City_Tier, veri$Income_log, main = "Income vs City_Tier", xlab = "City_Tier", ylab = "Income")

cor2_age_income <- cor(veri$Income_log,Age)
cor2_Occupation_income <- cor(veri$Income_log,as.numeric(Occupation))
cor2_City_Tier_income <- cor(veri$Income_log,as.numeric(City_Tier))

# Multiple Linear Regression modeli
model <- lm(veri$Income_log ~Age+Occupation+City_Tier, data = veri)

# y= 42023.96+30.29*Age-2667.28*OccupationRetired+....

# Modelin özeti
summary(model)
