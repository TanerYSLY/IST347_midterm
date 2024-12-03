file_path <- "C:/Users/taner/OneDrive/Masa\u00fcst\u00fc/projeler/ist analiz/arasinav.txt"

veri_kitle <- read.table(file_path, header = TRUE, sep = " ")  
set.seed(21)  # Rastgelelik sabitlemek için
satir_no <- sample(1:nrow(veri_kitle),size = 2000)
#vektor,size(boyut)

veri_orneklem <- veri_kitle[satir_no,]

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
