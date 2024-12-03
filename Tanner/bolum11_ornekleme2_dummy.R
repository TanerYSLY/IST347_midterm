file_path <- "C:/Users/taner/OneDrive/Masa\u00fcst\u00fc/projeler/ist analiz/arasinav.txt"

veri_kitle <- read.table(file_path, header = TRUE, sep = " ")  

# Rastgele örnekleme
set.seed(21)  # Rastgelelik sabitlemek için
satir_no <- sample(1:nrow(veri_kitle), size = 2000)
veri_orneklem <- veri_kitle[satir_no,]
veri <- veri_orneklem

# Değişkenlerin türlerini kontrol etme
names(veri)  # Verinin içindeki değişkenleri görmek için kullanılır.
summary(veri)

# Kategorik değişkenleri faktöre dönüştürme
veri$Occupation <- as.factor(veri$Occupation)
veri$City_Tier <- as.factor(veri$City_Tier)

# Kategorik değişkenlere dummy değişkeni ekleme
veri <- cbind(veri, model.matrix(~ Occupation - 1, data = veri))  # Occupation için dummy değişkenler
veri <- cbind(veri, model.matrix(~ City_Tier - 1, data = veri))  # City_Tier için dummy değişkenler

# Veri çerçevesinde yalnızca gerekli sütunları tutma
veri <- veri[, !(names(veri) %in% c("Occupation", "City_Tier"))]

attach(veri)

plot(Age, Income, main = "Income vs Age", xlab = "Age", ylab = "Income")
#bir lineer ilişki yok Age ve Income arasında

plot(Occupation, Income, main = "Income vs Occupation", xlab = "Occupation", ylab = "Income")
plot(City_Tier, Income, main = "Income vs City_Tier", xlab = "City_Tier", ylab = "Income")


# Modeli kurma (Bağımlı değişken: Income, Bağımsız değişkenler: Age + dummy değişkenler)
model <- lm(Income ~ Age + OccupationSelf_Employed + OccupationRetired + OccupationStudent + 
              City_TierTier_2 + City_TierTier_3, data = veri)

# Modelin özeti
summary(model)
